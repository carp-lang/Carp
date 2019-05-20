# Carp

[![Join the chat at https://gitter.im/eriksvedang/Carp](https://badges.gitter.im/eriksvedang/Carp.svg)](https://gitter.im/eriksvedang/Carp?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

<img src="https://github.com/carp-lang/Carp/blob/master/img/carp_logo_300_c.png" alt="Logo" align="right" />

<i>WARNING! This is a research project and a lot of information here might become outdated and misleading without any explanation. Don't use it for anything important just yet!</i>

<i>Update (March 5, 2019): Union types are merged into master. The final big feature before release is "lifetimes", after those are in the core language will be more or less finished.</i>

<i>A conference talk about Carp was recorded and the video can be seen [here](https://youtu.be/BQeG6fXMk28).</i>

## About

Carp is a small programming language designed to work well for interactive and performance sensitive use cases like games, sound synthesis and visualizations.

The key features of Carp are the following:
* Automatic and deterministic memory management (no garbage collector or VM)
* Inferred static types for great speed and reliability
* Ownership tracking enables a functional programming style while still using mutation of cache-friendly data structures under the hood
* No hidden performance penalties – allocation and copying are explicit
* Straightforward integration with existing C code

## Learn more

* [Installation](docs/Install.md) - how to build and configure the Carp compiler
* [The Compiler Manual](docs/Manual.md) - how to compile code and configure your projects
* [Carp Language Guide](docs/LanguageGuide.md) - syntax and semantics of the language
* [Libraries](docs/Libraries.md) - how to work with libraries and modules
* [Tooling](docs/Tooling.md) - supported editors
* [Game Example](examples/reptile.carp) - a Snake clone in Carp

The Carp REPL has built-in documentation, run ```(help)``` to access it!

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

To build this example, save it to a file called 'example.carp' and load it with ```(load "example.carp")```, then execute ```(build)``` to build an executable, and ```(run)``` to start.

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

Are you missing from the contributors list? Please send a pull request!

## License

Copyright 2016 - 2018 Erik Svedäng

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
