---
layout: post
title: Rust & Rocket Tutorial, the tera template engine
---

In the [previous tutorial]({% post_url 2018-03-08-rocket-tutorial-2 %}) we
discussed how to route a request to the correct handler using the URL, and now
we will discuss how to create a response using a template.

[Rocket.rs](https://rocket.rs), now, have already integrated two
template engine: [tera](https://github.com/Keats/tera) and
[handlebars](https://github.com/sunng87/handlebars-rust).

In this tutorial we will use **tera**.

The first step is to include on our application a dependency to `racket_contrib`
with the following section in our `Cargo.toml`:

```ini
[dependencies.rocket_contrib]
version = "0.3.6"
features = ["tera_templates"]
```

Simply including `rocket_contrib = "0.3.6"` isn't enough, since we want to
include `tera_template` in the features list.

The template engine need to be initialized using a **fairing**, which is the
Rocket analogous to **middlewares**.

```rust
extern crate rocket_contrib;
use rocket_contrib::Template;

/// In the main function, we should attach the template fairing
rocket::ignite()
    .mount("/", routes![hello])
    .attach(Template::fairing()) // <===
    .launch();
```

We can now create our first template in `templates/base.html.tera`, containing
this simple HTML code:

{% raw %}
```html
<!doctype html>
<html>
    <head>
        <meta name="viewport" 
            content="width=device-width, initial-scale=1">
        <link rel="stylesheet" 
            href="https://www.w3schools.com/w3css/4/w3.css">    
    </head>
    <body>
        <div class="w3-bar w3-light-gray">
            <div class="w3-button">
                <b>Blog</b>
            </div>
        </div>

        <div class="w3-content">
            {% block content %}
            {% endblock content %}
        </div>
    </body>
</html>
```
{% endraw %}

**Tera** syntax is similar to the Django one. In the previous template we used
the **block** tag, which isolate a section in the document to be used in
inherited templates.

Let's use this template in our home page, putting the following content in a
file named `templates/index.html.tera`:

{% raw %}
```html
{% extends "base" %}

{% block content %}
<h1>This is our example blog application</h1>
<p>
    Let's explore what we can do with
    <a href="https://rocket.rs">Rocket</a> and
    <a href="https://www.rust-lang.org">Rust</a>
</p>
{% endblock content %} 
```
{% endraw %}

The previous template includes all the content of `base.html.tera`, using the
contents of the blog from the previous file. As you can see, this mechanism is
similar to extension and overriding in object-oriented languages.

Now we will render the template in our root page with the following handler:

```rust
#[get("/")]
fn root() -> Template {
    let context = HashMap::<String, String>::new();
    Template::render("index", context)
}
```

Every template must be creared with a context, and this is the reason why we are
creating an `HashMap` in the first row of the function. The second row renders
the template creating the HTTP response for the user.

The first argument of the `Template::render` function is the name of the
template. Rocket will search in the `templates` folder a template named
`index.html.tera`.

Tera will preload all the templates files at the application startup, so you
won't be able to see the effect of changes you do while the web server is
running.

To sum up, beside the HTML template files, our Rust code looks like this:

```rust
#![feature(plugin)]
#![plugin(rocket_codegen)]

extern crate rocket;
extern crate rocket_contrib;

use std::collections::HashMap;
use rocket_contrib::Template;

#[get("/")]
fn root() -> Template {
    let context = HashMap::<String, String>::new();
    Template::render("index", context)
}

fn main() {
    rocket::ignite()
        .mount("/", routes![root])
        .attach(Template::fairing())
        .launch();
}
```