# Carp

[![Join the chat at https://gitter.im/eriksvedang/Carp](https://badges.gitter.im/eriksvedang/Carp.svg)](https://gitter.im/eriksvedang/Carp?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

<img src="https://github.com/eriksvedang/Carp/blob/master/img/carp_logo_300_c.png" alt="Logo" align="right" />

<i>WARNING! This is a research project and a lot of information here might become outdated and misleading without any explanation. Don't use it for anything important just yet!</i>

Carp is a small programming language designed to work well for interactive and performance sensitive use cases like games, sound synthesis and visualizations.

The key features of Carp are the following:
* Automatic and deterministic memory management (no garbage collector or VM)
* Inferred static types for great speed and reliability
* Live reloading of code, REPL-driven development, a fun and helpful workflow
* Ownership tracking enables a functional programming style while still using mutation of cache friendly data structures under the hood
* No hidden performance penalties – allocation and copying is explicit
* Very good integration with exisiting C code


## Learn more

* [Installation](INSTALL.md) - how to build the Carp compiler
* [The Compiler Manual](MANUAL.md) - how to compile code and configure your projects
* [Carp Language Guide](LANGUAGE.md) - syntax and semantics of the language
* [Libraries](LIBRARIES.md) - the various libraries that come built-in to Carp
* [typograf.carp](/examples/typograf.carp) - a more complex example


## A Small OpenGL/GLFW Example

```clojure
(import gl)

(defn init []
  0f)

(defn tick [state]
  (+ state 0.15f))

(defn draw [state]
  (let [t @state
        steps 100
        step (/ 1f (itof steps))]
    (for (i 0 steps)
      (let [r (* step (itof i))
            r2 (+ r step)]
        (draw-line (* r (cosf (* t r)))
                   (* r (sinf (* t r)))
                   (* r2 (cosf (* t r2)))
                   (* r2 (sinf (* t r2))))))))

(defn spin []
  (glfw-app "Spin" init tick draw default-on-keys))
```

To build this example, save it to a file called 'example.carp' and load it with ```(load-lisp "example.carp")```, then execute ```(bake-exe spin)``` to build an executable, or ```(spin)``` to run the program directly from the REPL.


### Contributors

* Erik Svedäng [@e_svedang](https://twitter.com/e_svedang)
* Markus Gustavsson
* Fyodor Shchukin


## License

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

© Erik Svedäng 2015 - 2016

