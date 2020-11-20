# Carp

<img src="resources/logo/carp_logo_300_c.png" alt="Logo" align="right"/>

[![Linux CI](https://github.com/carp-lang/Carp/workflows/Linux%20CI/badge.svg)](https://github.com/carp-lang/Carp/actions?query=workflow%3A%22Linux+CI%22)
[![MacOS CI](https://github.com/carp-lang/Carp/workflows/MacOS%20CI/badge.svg)](https://github.com/carp-lang/Carp/actions?query=workflow%3A"MacOS+CI")
[![Windows CI](https://github.com/carp-lang/Carp/workflows/Windows%20CI/badge.svg)](https://github.com/carp-lang/Carp/actions?query=workflow%3A"Windows+CI")

<i>WARNING! This is a research project and a lot of information here might become outdated and misleading without any explanation. Don't use it for anything important just yet!</i>

<i>[Version 0.4 of the language is out!](https://github.com/carp-lang/Carp/releases/)</i>

## About

Carp is a programming language designed to work well for interactive and performance sensitive use cases like games, sound synthesis and visualizations.

The key features of Carp are the following:
* Automatic and deterministic memory management (no garbage collector or VM)
* Inferred static types for great speed and reliability
* Ownership tracking enables a functional programming style while still using mutation of cache-friendly data structures under the hood
* No hidden performance penalties – allocation and copying are explicit
* Straightforward integration with existing C code

## Learn more
* [The Compiler Manual](docs/Manual.md) - how to install and use the compiler
* [Carp Language Guide](docs/LanguageGuide.md) - syntax and semantics of the language
* [Core Docs](http://carp-lang.github.io/carp-docs/core/core_index.html) - documentation for our standard library

[![Join the chat at https://gitter.im/eriksvedang/Carp](https://badges.gitter.im/eriksvedang/Carp.svg)](https://gitter.im/eriksvedang/Carp?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

## A Very Small Example

```clojure
(load-and-use SDL)

(defn tick [state]
  (+ state 10))

(defn draw [app rend state]
  (bg rend &(rgb (/ @state 2) (/ @state 3) (/ @state 4))))

(defn main []
  (let [app (SDLApp.create "The Minimalistic Color Generator" 400 300)
        state 0]
    (SDLApp.run-with-callbacks &app SDLApp.quit-on-esc tick draw state)))
```

To build this example, save it to a file called 'example.carp' and load it with ```(load "example.carp")```, then execute ```(build)``` to build an executable, and ```(run)``` to start. The external dependencies are SDL2 and pkg-config. On macOS Catalina libiconv is also required.

For a slightly longer example, check out [Reptile](examples/reptile.carp) - a Snake clone in Carp.

### Language Designer & Lead Developer
[Erik Svedäng](http://www.eriksvedang.com) ([@e_svedang](https://twitter.com/e_svedang))

### Core Contributor
[Veit Heller](http://veitheller.de) ([@hellerve](https://github.com/hellerve))

### Contributors
* Markus Gustavsson
* Fyodor Shchukin
* Anes Lihovac
* Chris Hall
* Tom Smeding
* Dan Connolly
* Reini Urban
* Jonas Granquist
* Joel Kaasinen ([@opqdonut](https://github.com/opqdonut))
* Eric Shimizu Karbstein ([@GrayJack](https://github.com/GrayJack))
* Jorge Acereda ([@jacereda](https://github.com/jacereda))
* Scott Olsen ([@scolsen](https://github.com/scolsen))
* Tim Dévé ([@timdeve](https://github.com/TimDeve))

Are you missing from the contributors list? Please send a pull request!

## License

Copyright 2016 - 2020 Erik Svedäng

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

The regular expression implementation as found in src/carp_regex.h are
Copyright (C) 1994-2017 Lua.org, PUC-Rio under the terms of the MIT license.
Details can be found in the License file LUA_LICENSE.
