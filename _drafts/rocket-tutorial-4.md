---
layout: post
title: Rust & Rocket Tutorial, how to use SQLite
---

In this post we will introduce structs and we will use them to load and store
records from the database.

Let's suppose we want to create a simple blog: we will need at least a table
for authors and a table for posts.

We will start with a structure for authors:

```rust
pub struct Author {
    id: i32,
    first_name: Option<String>,
    last_name: Option<String>,
    email: String,
    username: String,
}
```

It seems easy, but it actually contains a lot of information. First of all,
let's met the `String` type. The `String` type can be used to store strings,
and this is obvious, but what isn't obvious is that that type actually
**owns** a string.

Owning a value means that when the variable (or field, in our case) goes out
of scope, the memory for the string will be freed. If you know how C++ works,
`String` is similar to `std::string`.

Now, let's met the `Option<T>` generic type. This type can have two values:
`None` and `Some T`, which means a value of type `T`. I.e. a value of type
`Option<String>` can contain `None` or `Some String`.

This is really similar to nullable types in SQL! In effect (in my humble
opinion) `Option<String>` is to `String` as `TEXT NULL` is to `TEXT`.

`i32` is simply a signed integer of size 32 bits.

Now I would like to declare a "class method" to load authors from a database
using a known primary key.

Let's read together the following code:

```rust
use rusqlite::Connection;

impl Author {
    pub fn from_id(id: i32, connection: &Connection) -> Option<Author> {
        connection
            .query_row(
                "SELECT \
                 id, first_name, last_name, email, username \
                 FROM users \
                 WHERE id=?1",
                &[&id],
                |row| Author {
                    id: row.get(0),
                    first_name: row.get(1),
                    last_name: row.get(2),
                    email: row.get(3),
                    username: row.get(4),
                },
            )
            .ok()
    }
}
```

We use the wonderful `rusqlite` crate to attach a SQLite database, which is a
really comfortable database for development purposes. Let's add to our
`Cargo.toml` file the following dependency:

```
[dependencies.rusqlite]
version = "0.11.0"
features = ["bundled"]
```

By using the `bundled` feature we specify we want to use a version of SQLite
directly bundled in the `rusqlite` create.

The `impl Author` scope introduce something like the methods of the `Author`
class.

```rust
pub fn from_id(id: i32, connection: &Connection) -> Option<Author> {...}
```

First of all, let's study the parameter of the `connection` argument. It's of
type `&Connection`. Previously we talked about `String` and we said that it
**owns** the value, that will be freed when the **lifetime** of the variable
ends.

The same would happen to `connection` if it was of type `Connection`, and we
don't want that. What we want is to not own the connection, but to create it
in a function (perhaps main?) and use it in our program.

The code creating the connection will own it, the other function will not.

This is the reason why we used `&Connection`. To use the same C++
parallelism, I'll say that `Connection` is to `std::string` what
`&Connection` is to `const std::string &`.

`&Connection` is called a **reference**. And you create references when you
want (with some rules we will introduce later) using the `&` operator.

Please note that I used the C++ `const std::string &` and not `std::string&`.
You can not modify the value if you have the reference `&Connection`.

Rust also has **mutable references**, and we will also talk about them.

Now we can start commenting the code inside the function:

```rust
connection
    .query_row(
        "SELECT \
            id, first_name, last_name, email, username \
            FROM users \
            WHERE id=?1",
        &[&id],
        |row| Author {
            id: row.get(0),
            first_name: row.get(1),
            last_name: row.get(2),
            email: row.get(3),
            username: row.get(4),
        },
    )
    .ok()
```  

This is the declaration of the `query_row` method:

```rust
fn query_row<T, F>(&self, sql: &str, params: &[&ToSql], f: F) -> Result<T>
where
    F: FnOnce(&Row) -> T, 
```

Wow! This is not an easy one. First of all, it has two generic parameters,
`T` and `F`. Usually, when we work with generic parameters, with `T` we mean
a type containing a value and with `F` we mean a function.

Let's talk now about the first parameter: `&self`. This is the equivalent of
`this` in C++ and it means the current object, passed as a non mutable
reference.

The second parameter is `sql: &str`, and `&str` is a non mutable reference to
a string.

The third parameter is `params: &[&ToSql]`

## TODO

`FnOnce` means a function, in our case of type `&Row`, returning a result of
type `T`.