# Carp

[![Join the chat at https://gitter.im/eriksvedang/Carp](https://badges.gitter.im/eriksvedang/Carp.svg)](https://gitter.im/eriksvedang/Carp?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

<img src="https://github.com/carp-lang/Carp/blob/master/img/carp_logo_300_c.png" alt="Logo" align="right" />

<i>WARNING! This is a research project and a lot of information here might become outdated and misleading without any explanation. Don't use it for anything important!</i>

<i>Update (June 26, 2017): The total rewrite is now live; if you want to look at the old version, it's under the branch named "c" in this repository.</i>

## About

Carp is a small programming language designed to work well for interactive and performance sensitive use cases like games, sound synthesis and visualizations.

The key features of Carp are the following:
* Automatic and deterministic memory management (no garbage collector or VM)
* Inferred static types for great speed and reliability
* Ownership tracking enables a functional programming style while still using mutation of cache-friendly data structures under the hood
* No hidden performance penalties – allocation and copying are explicit
* Straightforward integration with existing C code

## Learn more

* [Installation](docs/Install.md) - how to build the Carp compiler
* [The Compiler Manual](docs/Manual.md) - how to compile code and configure your projects
* [Carp Language Guide](docs/LanguageGuide.md) - syntax and semantics of the language
* [Libraries](docs/Libraries.md) - the various libraries that come built into Carp
* [Tooling](docs/Tooling.md) - supported editors
* [Game Example](examples/game.carp) - an example of how to use SDL with Carp

The Carp REPL has built-in documentation, run ```(help)``` to access it!

## A Small Example

```clojure
(use IO)
(use Int)
(use String)

(def guessing true)
(def answer 0)

(defn init! []
  (do (println "Seeding random number generator...\n")
      (seed (System.time))

      (println "~ The Number Guessing Game ~")
      (println "(Enter q to quit.)\n\n")

      (set! &answer (random-between 1 100))
      (print "Please enter a number between 1 - 99: ")))

(defn exit! []
  (do (println "Good bye...")
      (set! &guessing false)))

(defn correct! []
  (do (println "Correct!")
      (exit!)))

(defn guess-again [low-or-high]
  (do (println &(string-join @"->Too " @low-or-high @"."))
      (print "\nPlease guess again: ")))

(defn main []
  (do
    (init!)

    (while guessing
      (let [user-input (get-line)
            guessed-num (from-string &user-input)]
        (if (= &user-input "q\n")
          (exit!)
          (cond (< guessed-num answer) (guess-again "low")
                (> guessed-num answer) (guess-again "high")
                (correct!)))))))
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

Are you missing from the contributors list? Please send a pull request!

## License

Copyright 2016 - 2017 Erik Svedäng

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
