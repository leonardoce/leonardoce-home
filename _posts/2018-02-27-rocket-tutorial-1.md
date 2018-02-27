---
layout: post
title: Rust & Racket Tutorial, Part 1
---

Starting from today, I'm writing a series of posts composing a basic tutorial
on how to use Rust to create a Web Application.

I just discovered the [Rocket](https://rocket.rs) web framework, and I would
like to share with you my experience with it.

Let's start with the prerequisites. As you can find in the [getting
started](https://rocket.rs/guide/getting-started/) page of the Racket web site,
you will need to install the nightly Rust toolchain. The best way to do that is
to use [RustUp](http://rustup.rs/). If your computer is running Linux, like
mine is, you can install rustup simply with the following command:

```bash
# Don't use root or sudo, your unprivileged account is just ok!
$ curl https://sh.rustup.rs -sSf | sh
[...]

Current installation options:

   default host triple: x86_64-unknown-linux-gnu
     default toolchain: stable
  modify PATH variable: yes

1) Proceed with installation (default)
2) Customize installation
3) Cancel installation
1 # <====== This is important

[...]
Rust is installed now. Great!
[...]
```

Now you must reload your environment variables to use `cargo` without
specifying the path. All it takes is to logout and login again, and verify that
everything is working correctly:

```
$ cargo
Rust's package manager
[...]
```

Rocket needs the lastest Rust toolchain, and we can just install that and set
it as the default one using rustup:

```bash
$ rustup install nightly
[...]
nightly-x86_64-unknown-linux-gnu installed
[...]

$ rustup default nightly
[...]
```

You also need to ensure that your computer has a C compiler working correctly.
Every distro has its own package to install the C compiler, i.e. if you are
using Ubuntu you can just use this command to install it:

```bash
$ sudo apt update
$ sudo apt install -y build-essential
[...]
```

Please consult the documentation of your distro of choice to get help.

We are now ready to create our first application with Racket, using the one
from the [getting started](https://rocket.rs/guide/getting-started/) page.

Let's create a folder for our application:

```bash
~$ cargo init --bin rocket_getting_started
     Created binary (application) project

~$ cd rocket_getting_started/

~/rocket_getting_started$ ls
Cargo.toml  src

~/rocket_getting_started$ ls -l src/
total 4
-rw-rw-r-- 1 ubuntu ubuntu 45 Feb 27 09:20 main.rs
```

`main.rs` the "starting point" of a Rust application. We can replace that one
using our favourite text editor with the following source code:

```rust
#![feature(plugin)]
#![plugin(rocket_codegen)]

extern crate rocket;

#[get("/")]
fn index() -> &'static str {
    "Hello, world!"
}

fn main() {
    rocket::ignite().mount("/", routes![index]).launch();
}
```

We must also specify that we require Rocket to compile this app, and we can to
that modifying the `Cargo.toml` file in the `dependencies` section:

```
[package]
name = "rocket_getting_started"
version = "0.1.0"
authors = ["leonardo"]

[dependencies]
rocket = "0.3.6"
rocket_codegen = "0.3.6"
```

Let's compile and run our first application!

```
$ cargo run
[...]
🔧  Configured for development.
    => address: localhost
    => port: 8000
    => log: normal
    => workers: 4
    => secret key: generated
    => limits: forms = 32KiB
    => tls: disabled
🛰  Mounting '/':
    => GET /
🚀  Rocket has launched from http://localhost:8000
```

Great! You can use your browser to test the getting started application. Now
your development environment is correctly set up.

In the next post we will work on the getting started application. Thanks!