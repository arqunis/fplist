//! An immutable singly-linked list whose elements persist through multiple "modification".
//!
//! Each "modification" returns a new list with the changes, but leaving the original unscathed.
//! Memory is shared across the new lists, and is only deallocated once the last list referencing the memory is dropped.
//!
//! This is accomplished with the built-in [`Rc`] type, but may be replaced with its multithreaded sibling, [`Arc`], if so desired
//! through the `multithreaded` feature.
//!
//! [`Rc`]: https://doc.rust-lang.org/stable/std/rc/struct.Rc.html
//! [`Arc`]: https://doc.rust-lang.org/nightly/std/sync/struct.Arc.html

use std::fmt::{self, Write};
use std::iter::FromIterator;
use std::mem;

#[cfg(not(feature = "multithreaded"))]
type Ref<T> = std::rc::Rc<Node<T>>;

#[cfg(feature = "multithreaded")]
type Ref<T> = std::sync::Arc<Node<T>>;

#[derive(Clone)]
struct Node<T> {
    next: Option<Ref<T>>,
    elem: T,
}

/// An immutable singly-linked list.
pub struct PersistentList<T> {
    inner: Option<Ref<T>>,
    len: usize,
}

impl<T> Default for PersistentList<T> {
    #[inline]
    fn default() -> Self {
        PersistentList {
            inner: None,
            len: 0,
        }
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

impl<T> Clone for PersistentList<T> {
    /// Make a new copy of the list.
    ///
    /// O(1)
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
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_list().entries(self.iter()).finish()
    }
}

impl<T: fmt::Display> fmt::Display for PersistentList<T> {
    /// Formats this list to a parenthesis representation.
    ///
    /// A list of 1->2->3->() would result in the repr.: `(1 2 3)`
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
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
    /// Create a new empty list.
    /// Does not allocate.
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    /// Retrieve the current element.
    ///
    /// Returns `None` in an empty list.
    #[inline]
    pub fn first(&self) -> Option<&T> {
        self.inner.as_ref().map(|n| &n.elem)
    }

    /// Retrieve the next list in line.
    ///
    /// Decreases the length by one unless it's the empty list.
    #[inline]
    pub fn rest(&self) -> Self {
        PersistentList {
            inner: self.inner.as_ref().and_then(|n| n.next.clone()),
            len: self.len.checked_sub(1).unwrap_or(0),
        }
    }

    /// Provide an iterator to all of the elements in the list.
    #[inline]
    pub fn iter(&self) -> ListIter<T> {
        ListIter {
            node: self.inner.as_ref(),
            len: self.len(),
        }
    }

    /// Retrieve how many elements there are in total.
    ///
    /// O(1)
    #[inline]
    pub fn len(&self) -> usize {
        self.len
    }

    /// Is the list empty?
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Return an owned instance of this list, leaving an empty one in its place.
    #[inline]
    pub fn take(&mut self) -> Self {
        mem::replace(self, PersistentList::new())
    }
}

impl<T: Clone> PersistentList<T> {
    /// Split the list to its current element and
    /// the next succeeding list.
    ///
    /// This will attempt to take ownership of the element,
    /// but if there are other existing references to it, its clone will be returned.
    #[inline]
    pub fn pop(mut self) -> Option<(T, Self)> {
        self.inner.take().map(|node| {
            // Steal the node if possible.
            // Else clone it in order to leave it alone, as apparently it has a big cavalry standing behind it.
            let node = Ref::try_unwrap(node).unwrap_or_else(|n| (*n).clone());

            let rest = PersistentList {
                inner: node.next,
                len: self.len() - 1,
            };

            (node.elem, rest)
        })
    }

    /// Similar to [`first`] but tries to take ownership of the element if possible, otherwise clones it.
    ///
    /// [`first`]: #method.first
    #[inline]
    pub fn pfirst(self) -> Option<T> {
        self.pop().map(|(elem, _)| elem)
    }

    /// Merge this list's elements with another, creating a new list with all the elements combined.
    ///
    /// Note: All of the elements from this list will have their ownership taken, or be cloned into the new list
    /// depending on the number of references to said elements.
    ///
    /// The mergee list's elements will NOT be cloned; their memory will be shared in the new list!
    #[inline]
    pub fn append(self, r: PersistentList<T>) -> PersistentList<T> {
        if self.is_empty() {
            r
        } else {
            let (elem, l) = self.pop().unwrap();
            cons(elem, l.append(r))
        }
    }
}

impl<T: PartialEq, U> PersistentList<(T, U)> {
    /// Find the value belonging to the provided key in this [`Association List`].
    ///
    /// [`association list`]: https://en.m.wikipedia.org/wiki/Association_list
    #[inline]
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

impl<T> Drop for PersistentList<T> {
    fn drop(&mut self) {
        // Only deallocate memory that's no longer being shared.
        while let Some(node) = self.inner.take() {
            if let Ok(node) = Ref::try_unwrap(node) {
                self.inner = node.next;
            }
        }
    }
}

/// A view to a list's elements, expressed as an iterator.
#[derive(Clone)]
pub struct ListIter<'a, T: 'a> {
    node: Option<&'a Ref<T>>,
    len: usize,
}

impl<'a, T> Iterator for ListIter<'a, T> {
    type Item = &'a T;

    #[inline]
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
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("ListIter").field("len", &self.len).finish()
    }
}

/// An owning view to a list.
///
/// Attempts to take ownership of the elements if possible,
/// otherwise clones them.
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
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("OwnedListIter")
            .field("len", &self.inner.len())
            .finish()
    }
}

/// Create a new list with one element.
#[inline]
pub fn one<T>(elem: T) -> PersistentList<T> {
    cons(elem, PersistentList::new())
}

/// Construct a new list by prepending one element to the next list.
///
/// Increases the length.
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

/// Create a new list out of an iterable.
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
}
