# fplist

A persistent, immutable, and singly-linked list in Rust.

Persistency is a property of data structures when modifying them. Persistency
stipulates that old versions of the structures are always preserved when adding
or removing elements. This crate provides such a structure in the form of
a linked list.

To be efficient as possible when maintaining persistency, the linked list uses
[reference counting][ref]. This permits sharing memory between lists whose
elements are the same. By default, the list uses the [`Rc`] type for this
purpose. However, one major downside of `Rc` is that lists cannot be sent
across threads. For this, you might want to enable the `multithreaded` feature,
which will signal the list to use the multithreaded sibling [`Arc`].

One innate requirement of persistency is that structures must be immutable. That
is, they cannot permit mutation of their elements. The list type only provides
an API for immutable and owned access to its elements. You may circumvent this
with the [`RefCell`] or [`Mutex`] types, which grant interior mutability.

The list type is inspired by [cons] lists from Lisp languages, which is also how
the list type is constructed with.

## Serde

The list is able to be serialized or deserialized by the [serde framework][serde].
Support, however, is disabled by default (to avoid needless dependencies). Enable
support with the `serde_impls` feature.

## Example

```rust
use fplist::{PersistentList, cons};

let list = cons(1, cons(2, cons(3, PersistentList::new())));

assert_eq!(list.first(), Some(&1));

let list = list.rest();

assert_eq!(list.first(), Some(&2));

let list = list.rest();

assert_eq!(list.first(), Some(&3));

let list = list.rest();

assert_eq!(list.first(), None);
```

## License

This project is under the jurisdiction of the [MIT License][license].

[ref]: https://en.m.wikipedia.org/wiki/Reference_counting
[`Rc`]: https://doc.rust-lang.org/stable/std/rc/struct.Rc.html
[`Arc`]: https://doc.rust-lang.org/stable/std/sync/struct.Arc.html
[`RefCell`]: https://doc.rust-lang.org/stable/std/cell/struct.RefCell.html
[`Mutex`]: https://doc.rust-lang.org/stable/std/sync/struct.Mutex.html
[cons]: https://en.wikipedia.org/wiki/Cons
[serde]: https://serde.rs
[license]: LICENSE.md
