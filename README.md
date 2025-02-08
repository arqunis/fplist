# fplist

<!-- cargo-sync-readme start -->

A persistent singly-linked list in Rust.

## Persistency

Persistency is a property of data structures when they are modified.

When an object in memory is modified, this typically involves discarding its current value and
replacing it with a new one. This is often the desired effect, as most applications are not
interested in the history of how much an object's value has changed, or what it was changed
into. Such objects are understood to be *mutable*.

The opposite of a mutable object is an *immutable* object, whose value, once the object is
initialised, cannot change and never does throughout the course of an application. The only
time it may "change" is when the object itself is modified by recreating it with a new
computation of the previous value. This gives rise to a property that mutable objects lack:
consistency.

Persistency exploits consistency to allow immutability while minimising memory usage by sharing
values of previous objects with new objects. In data structures, this means their elements are
shared between different instances of the data structures if they are the same.

This crate offers a persistent data structure in the form of a singly-linked list, using
[reference counting][ref] to share nodes between lists.

## Multithreading

Reference counting is implemented using the standard library [`Rc`] type, restricting the use
of the list to a single thread. If you wish to use it in multiple threads with an added
overhead, enable the `multithreaded` feature to switch to the atomic version of [`Rc`],
[`Arc`].

## Mutating nodes

The innate requirement of persistency is immutability. In the case of this crate, this means
you cannot change the list's length and the first node it points to, as well as a node's
pointer to the next node.

A node's value is also immutable, but only as a side-effect of Rust's XOR mutability rule. If
you wish to change the value, you may wrap it with the standard library [`RefCell`], [`Mutex`],
and/or [`RwLock`] types, which grant interior mutability. This crate only cares about sharing
list nodes and, therefore, heap allocations made for the nodes, not the values they store.

## Serde

This crate offers optional support for serialisation and deserialisation using the [serde
framework][serde]. Enable it with the `serde_impls` feature.

## Example

The linked list is inspired by [cons] lists from Lisp languages, which is you construct it.

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

[ref]: https://en.m.wikipedia.org/wiki/Reference_counting
[`Rc`]: https://doc.rust-lang.org/stable/std/rc/struct.Rc.html
[`Arc`]: https://doc.rust-lang.org/stable/std/sync/struct.Arc.html
[`RefCell`]: https://doc.rust-lang.org/stable/std/cell/struct.RefCell.html
[`Mutex`]: https://doc.rust-lang.org/stable/std/sync/struct.Mutex.html
[`RwLock`]: https://doc.rust-lang.org/stable/std/sync/struct.RwLock.html
[cons]: https://en.wikipedia.org/wiki/Cons
[serde]: https://serde.rs

<!-- cargo-sync-readme end -->

## License

The license of this project is the [MIT License][license]. If the license
document is missing, you may acquire a copy of the license at
https://opensource.org/license/MIT.

SPDX identifier: MIT

[license]: LICENSE.md
