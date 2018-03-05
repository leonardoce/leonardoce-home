---
layout: post
title: Rust is awesome
tag: rust
---

Sometimes [Rust](https://rust-lang.org) makes me think of the expressiveness of
[Haskell](https://www.haskell.org/), with the terseness of scripting languages,
with the performance of system programming languages compiled to native code
with manual memory management.

```rust
extern crate postgres;

use postgres::{Connection, TlsMode};

fn main() {
    let conn = Connection::connect(
        "postgres://[redacted]",
        TlsMode::None,
    ).unwrap();

    let merged_tickets: Vec<i32> =
        conn.query("SELECT id FROM tickets WHERE id <> effectiveid", &[])
            .unwrap()
            .into_iter()
            .map(|row| row.get(0))
            .collect();
    println!("{:?}", merged_tickets);
}
``` 

I'm still a newbie but, in a few words, I think it's awesome.
