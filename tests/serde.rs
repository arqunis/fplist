#![cfg(feature = "serde_impls")]

use fplist::{cons, PersistentList};
use serde_json::{to_string, from_str};

#[test]
fn serialization() {
    let list = cons(1, cons(2, cons(3, PersistentList::new())));

    let res = to_string(&list).unwrap();

    assert_eq!(res, "[1,2,3]");
}

#[test]
fn deserialization() {
    let res: PersistentList<i32> = from_str("[1,2,3]").unwrap();

    let expect = cons(1, cons(2, cons(3, PersistentList::new())));

    assert_eq!(res, expect);
}

