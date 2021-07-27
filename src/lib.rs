//! A persistent, immutable, and singly-linked list in Rust.
//!
//! Persistency is a property of data structures when modifying them. Persistency
//! stipulates that old versions of the structures are always preserved when adding
//! or removing elements. This crate provides such a structure in the form of
//! a linked list.
//!
//! To be efficient as possible when maintaining persistency, the linked list uses
//! [reference counting][ref]. This permits sharing memory between lists whose
//! elements are the same. By default, the list uses the [`Rc`] type for this
//! purpose. However, one major downside of `Rc` is that lists cannot be sent
//! across threads. For this, you might want to enable the `multithreaded` feature,
//! which will signal the list to use the multithreaded sibling [`Arc`].
//!
//! One innate requirement of persistency is that structures must be immutable. That
//! is, they cannot permit mutation of their elements. The list type only provides
//! an API for immutable and owned access to its elements. You may circumvent this
//! with the [`RefCell`] or [`Mutex`] types, which grant interior mutability.
//!
//! The list type is inspired by [cons] lists from Lisp languages, which is also how
//! the list type is constructed with and represented.
//!
//! ## Serde
//!
//! The list is able to be serialized or deserialized by the [serde framework][serde].
//! Support, however, is disabled by default (to avoid needless dependencies). Enable
//! support with the `serde_impls` feature.
//!
//! ## Example
//!
//! ```rust
//! use fplist::{PersistentList, cons};
//!
//! let list = cons(1, cons(2, cons(3, PersistentList::new())));
//!
//! assert_eq!(list.first(), Some(&1));
//!
//! let list = list.rest();
//!
//! assert_eq!(list.first(), Some(&2));
//!
//! let list = list.rest();
//!
//! assert_eq!(list.first(), Some(&3));
//!
//! let list = list.rest();
//!
//! assert_eq!(list.first(), None);
//! ```
//!
//! [ref]: https://en.m.wikipedia.org/wiki/Reference_counting
//! [`Rc`]: https://doc.rust-lang.org/stable/std/rc/struct.Rc.html
//! [`Arc`]: https://doc.rust-lang.org/stable/std/sync/struct.Arc.html
//! [`RefCell`]: https://doc.rust-lang.org/stable/std/cell/struct.RefCell.html
//! [`Mutex`]: https://doc.rust-lang.org/stable/std/sync/struct.Mutex.html
//! [cons]: https://en.wikipedia.org/wiki/Cons
//! [serde]: https://serde.rs

#![deny(rust_2018_idioms)]

#[cfg(feature = "serde_impls")]
use serde::ser::{Serialize, Serializer, SerializeSeq};
#[cfg(feature = "serde_impls")]
use serde::de::{Deserialize, Deserializer, Visitor, SeqAccess};

use std::fmt::{self, Write};
use std::iter::FromIterator;
use std::mem;
use std::ops::Index;

#[cfg(not(feature = "multithreaded"))]
type Ref<T> = std::rc::Rc<Node<T>>;

#[cfg(feature = "multithreaded")]
type Ref<T> = std::sync::Arc<Node<T>>;

#[derive(Clone, Hash)]
struct Node<T> {
    next: Option<Ref<T>>,
    elem: T,
}

/// A persistent, immutable, singly-linked list.
///
/// Refer to the crate documentation for more information.
#[derive(Hash)]
pub struct PersistentList<T> {
    inner: Option<Ref<T>>,
    len: usize,
}

impl<T> Default for PersistentList<T> {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl<T: PartialEq> PartialEq for PersistentList<T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.len() == other.len() && self.iter().eq(other.iter())
    }

    #[inline]
    fn ne(&self, other: &Self) -> bool {
        self.len() != other.len() || !self.iter().eq(other.iter())
    }
}

impl<T: PartialEq> Eq for PersistentList<T> {}

impl<T> Clone for PersistentList<T> {
    /// Creates a shallow copy of the list.
    ///
    /// ## Time Complexity
    ///
    /// Time Complexity is O(1).
    #[inline]
    fn clone(&self) -> Self {
        PersistentList {
            inner: self.inner.clone(),
            len: self.len,
        }
    }
}

impl<T: fmt::Debug> fmt::Debug for PersistentList<T> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.iter()).finish()
    }
}

impl<T: fmt::Display> fmt::Display for PersistentList<T> {
    /// Formats the list to a parenthesis representation.
    ///
    /// # Example
    ///
    /// ```rust
    ///
    /// use fplist::{PersistentList, cons};
    ///
    /// let list = cons(1, cons(2, cons(3, PersistentList::new())));
    ///
    /// assert_eq!(list.to_string(), "(1 2 3)");
    /// ```
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_char('(')?;

        let mut iter = self.iter();

        if let Some(elem) = iter.next() {
            fmt::Display::fmt(&elem, f)?;

            for elem in iter {
                f.write_char(' ')?;

                fmt::Display::fmt(&elem, f)?;
            }
        }

        f.write_char(')')
    }
}

impl<T> PersistentList<T> {
    /// An instance of an empty list.
    pub const NULL: PersistentList<T> = PersistentList::new();

    /// Creates an empty list.
    ///
    /// Does not allocate.
    #[inline]
    pub const fn new() -> Self {
        PersistentList {
            inner: None,
            len: 0,
        }
    }

    /// Retrieves the current element.
    ///
    /// Returns `None` on an empty list.
    #[inline]
    pub fn first(&self) -> Option<&T> {
        self.inner.as_ref().map(|n| &n.elem)
    }

    /// Retrieves the list pointing to the next element.
    ///
    /// The length of the list is decreased by one, unless it is an empty list.
    #[inline]
    pub fn rest(&self) -> Self {
        PersistentList {
            inner: self.inner.as_ref().and_then(|n| n.next.clone()),
            len: self.len.checked_sub(1).unwrap_or(0),
        }
    }

    /// Returns an immutable iterator to all of the elements in the list.
    #[inline]
    pub fn iter(&self) -> ListIter<'_, T> {
        ListIter {
            node: self.inner.as_ref(),
            len: self.len(),
        }
    }

    /// Returns a reference to an element at index `idx`.
    ///
    /// Returns `None` if no element is present at the specified index.
    ///
    /// ## Time Complexity
    ///
    /// Time complexity is O(n).
    #[inline]
    pub fn get(&self, idx: usize) -> Option<&T> {
        self.iter().nth(idx)
    }

    /// Returns the amount of elements in the list.
    ///
    /// ## Time Complexity
    ///
    /// Time complexity is O(1).
    #[inline]
    pub const fn len(&self) -> usize {
        self.len
    }

    /// Returns a boolean indicating whether the list is empty.
    #[inline]
    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns an owned instance of the list from a `&mut` context.
    ///
    /// The original list is replaced with the empty list.
    #[inline]
    pub fn take(&mut self) -> Self {
        mem::take(self)
    }
}

impl<T: Clone> PersistentList<T> {
    /// Splits the list to its current element and the list pointing to the next
    /// element.
    ///
    /// This will attempt to take ownership of the element, unless there is more
    /// than one reference to the element. In that case, the element is cloned.
    ///
    /// Returns `None` on an empty list.
    pub fn pop(mut self) -> Option<(T, Self)> {
        self.inner.take().map(|node| {
            let node = Ref::try_unwrap(node).unwrap_or_else(|n| (*n).clone());

            let rest = PersistentList {
                inner: node.next,
                len: self.len() - 1,
            };

            (node.elem, rest)
        })
    }

    /// Retrieves an owned instance of the current element.
    ///
    /// If there is more than one reference to the element, it will be cloned.
    ///
    /// Returns `None` on an empty list.
    #[inline]
    pub fn pfirst(self) -> Option<T> {
        self.pop().map(|(elem, _)| elem)
    }

    /// Merges the elements of this list with another.
    ///
    /// The elements of this list are [`pop`]ped, while the elements of the other
    /// list are shared.
    ///
    /// [`pop`]: Self::pop
    pub fn append(self, r: PersistentList<T>) -> PersistentList<T> {
        match self.pop() {
            None => r,
            Some((elem, l)) => cons(elem, l.append(r)),
        }
    }
}

impl<T: PartialEq, U> PersistentList<(T, U)> {
    /// Searches for the value belonging to the provided key.
    ///
    /// This method assumes this list is an [`association list`].
    ///
    /// [`association list`]: https://en.m.wikipedia.org/wiki/Association_list
    pub fn assoq(&self, key: &T) -> Option<&U> {
        self.iter()
            .find_map(|t| if t.0 == *key { Some(&t.1) } else { None })
    }
}

impl<T: Clone> IntoIterator for PersistentList<T> {
    type Item = T;
    type IntoIter = OwnedListIter<T>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        OwnedListIter { inner: self }
    }
}

impl<'a, T> IntoIterator for &'a PersistentList<T> {
    type Item = &'a T;
    type IntoIter = ListIter<'a, T>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<T> FromIterator<T> for PersistentList<T> {
    #[inline]
    fn from_iter<I: IntoIterator<Item = T>>(it: I) -> Self {
        list(it)
    }
}

impl<T: Clone> Extend<T> for PersistentList<T> {
    #[inline]
    fn extend<It: IntoIterator<Item = T>>(&mut self, iter: It) {
        *self = self.clone().append(list(iter));
    }
}

impl<T> Index<usize> for PersistentList<T> {
    type Output = T;

    #[inline]
    fn index(&self, idx: usize) -> &Self::Output {
        self.get(idx).expect("out of bounds")
    }
}

impl<T> Drop for PersistentList<T> {
    fn drop(&mut self) {
        // Only deallocate memory that is no longer being shared.
        while let Some(node) = self.inner.take() {
            if let Ok(node) = Ref::try_unwrap(node) {
                self.inner = node.next;
            }
        }
    }
}

/// An immutable view to elements of a [`PersistentList`].
#[derive(Clone)]
pub struct ListIter<'a, T> {
    node: Option<&'a Ref<T>>,
    len: usize,
}

impl<'a, T> Iterator for ListIter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        self.node.map(|n| {
            self.len -= 1;
            self.node = n.next.as_ref();

            &n.elem
        })
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len, Some(self.len))
    }
}

impl<'a, T> ExactSizeIterator for ListIter<'a, T> {}

impl<'a, T> fmt::Debug for ListIter<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ListIter").field("len", &self.len).finish()
    }
}

/// An owned view to elements of a list.
///
/// If there is more than one reference to an element, it will be cloned.
#[derive(Clone)]
pub struct OwnedListIter<T> {
    inner: PersistentList<T>,
}

impl<T: Clone> Iterator for OwnedListIter<T> {
    type Item = T;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let (elem, rest) = self.inner.take().pop()?;

        self.inner = rest;

        Some(elem)
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.inner.len;

        (len, Some(len))
    }
}

impl<T: Clone> ExactSizeIterator for OwnedListIter<T> {}

impl<T> fmt::Debug for OwnedListIter<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("OwnedListIter")
            .field("len", &self.inner.len())
            .finish()
    }
}

#[cfg(feature = "serde_impls")]
impl<T: Serialize> Serialize for PersistentList<T> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut seq = serializer.serialize_seq(Some(self.len()))?;

        for elem in self {
            seq.serialize_element(elem)?;
        }

        seq.end()
    }
}

#[cfg(feature = "serde_impls")]
impl<'de, T: Deserialize<'de>> Deserialize<'de> for PersistentList<T> {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        use std::marker::PhantomData;

        struct ListVisitor<T>(PhantomData<T>);

        impl<'de, T: Deserialize<'de>> Visitor<'de> for ListVisitor<T> {
            type Value = PersistentList<T>;

            fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                formatter.write_str("a sequence")
            }

            fn visit_seq<S: SeqAccess<'de>>(self, mut seq: S) -> Result<Self::Value, S::Error> {
                // Build the list from the last to first elements.
                match seq.next_element()? {
                    None => Ok(PersistentList::new()),
                    Some(elem) => {
                        let list = self.visit_seq(seq)?;
                        Ok(cons(elem, list))
                    }
                }
            }
        }

        deserializer.deserialize_seq(ListVisitor(PhantomData))
    }
}

/// Creates a new list with one element.
///
/// Shorthand for `cons(elem, PersistentList::new())`.
///
/// ```rust
/// use fplist::one;
///
/// let x = one(42);
///
/// assert_eq!(x.first(), Some(&42));
/// assert_eq!(x.len(), 1);
/// ```
#[inline]
pub fn one<T>(elem: T) -> PersistentList<T> {
    cons(elem, PersistentList::new())
}

/// Constructs a new list by prepending one element to the another list.
///
/// The length of the overall list is increased.
///
/// ```rust
/// use fplist::{cons, PersistentList};
///
/// let list = cons(1, cons(2, cons(3, PersistentList::new())));
///
/// assert_eq!(list.first(), Some(&1));
///
/// let list = list.rest();
///
/// assert_eq!(list.first(), Some(&2));
///
/// let list = list.rest();
///
/// assert_eq!(list.first(), Some(&3));
///
/// let list = list.rest();
///
/// assert_eq!(list.first(), None);
/// ```
#[inline]
pub fn cons<T>(elem: T, mut next: PersistentList<T>) -> PersistentList<T> {
    PersistentList {
        inner: Some(Ref::new(Node {
            elem,
            next: next.inner.take(),
        })),
        len: 1 + next.len(),
    }
}

/// Creates a new list out of an iterable.
///
/// ```rust
/// use fplist::list;
///
/// let list = list(vec![1, 2, 3]);
///
/// assert_eq!(list.first(), Some(&1));
///
/// let list = list.rest();
///
/// assert_eq!(list.first(), Some(&2));
///
/// let list = list.rest();
///
/// assert_eq!(list.first(), Some(&3));
///
/// let list = list.rest();
///
/// assert_eq!(list.first(), None);
/// ```
pub fn list<T, It: IntoIterator<Item = T>>(elems: It) -> PersistentList<T> {
    let mut elems = elems.into_iter();
    match elems.next() {
        None => PersistentList::new(),
        Some(elem) => cons(elem, list(elems)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn make_list() {
        assert_eq!(one(42), cons(42, PersistentList::new()));
        assert_eq!(cons(1, cons(2, one(3))), list([1, 2, 3].iter().cloned()));
    }

    #[test]
    fn index() {
        let list = cons(1, cons(2, one(3)));

        assert_eq!(list[0], 1);
        assert_eq!(list[1], 2);
        assert_eq!(list[2], 3);
    }

    #[test]
    fn hash() {
        use std::collections::HashMap;

        let mut map = HashMap::new();

        map.insert(one(1), 42);

        assert_eq!(map.get(&one(1)), Some(&42));
    }
}
