---
layout: post
title: Rust & Racket Tutorial, the URL path
---

In the [previous tutorial]({% post_url 2018-02-27-rocket-tutorial-1 %}) we
created a working development environment and compiled the Rocket.Rs getting
started example.

In this article we will talk about HTTP requests and how they are handled by
Rocket.Rs.

The function we wrote in the previous article is called a **route**, and it's
registered in the main function. As you can see, the request path is used to
route the request to the correct handler:

```rust
#[get("/")]
fn index() -> &'static str {
    "Hello, world!"
}

[...]
rocket::ignite().mount("/", routes![index]).launch();
[...]
```

Just to conferm our understanding, let's try creating another route and testing
it:

```rust
#[get("/hello")]
fn say_hello() -> String {
    "Hello!".to_string()
}

[...]
rocket::ignite().mount("/", routes![index, say_hello]).launch();
[...]
```

When we run our program using `cargo run`, as we did in the previous article, we
obtain the following result:

```
$ cargo run
[...]
🛰  Mounting '/':
    => GET /
    => GET /hello
🚀  Rocket has launched from http://localhost:8000

$ curl http://localhost:8000/hello/
Hello!
```

It works! But we can also receive parameters from url segments, like in the
following example:

```rust
#[get("/hello/<name>")]
fn say_hello(name: String) -> String {
    format!("Hello, {}!", name)
}
```

```
$ curl http://localhost:8000/hello/leonardo/
Hello, leonardo!
```

It works! Let's try what happens adding another parameter, the age:

```rust
#[get("/hello/<name>/<age>")]
fn say_hello(name: String, age: i32) -> String {
    format!("Hello, {}! Your age is {}.", name, age)
}
```

```
$ curl http://localhost:8000/hello/leonardo/37
Hello, leonardo! Your age is 37.
```

Let's try what happens if we use a wrong age...

```
$ curl http://localhost:8000/hello/leonardo/thirtysevel
<!DOCTYPE html>
<html>
<head>
    <meta charset="utf-8">
    <title>404 Not Found</title>
</head>
<body align="center">
    <div align="center">
        <h1>404: Not Found</h1>
        <p>The requested resource could not be found.</p>
        <hr />
        <small>Rocket</small>
    </div>
</body>
</html>
```

The URLs are type safe, and a wrong age can't be passed to this route, simply
because it can't be applied.