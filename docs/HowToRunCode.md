# How to run code

This document is aimed at people just starting out with Carp, in particular if you want to try out the [examples](../examples).

## Prerequisites

Make sure that you have [installed the Carp compiler and its dependencies](Install.md) and that you can start it without any error messages. Here's how it should look:

```bash
$ carp
Welcome to Carp X.Y.Z
This is free software with ABSOLUTELY NO WARRANTY.
Evaluate (help) for more information.
鲤
```

The `鲤` character on the last line is the [REPL](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop) prompt, it means that Carp is waiting for you to enter a command.

## Running code from the REPL

You can load some code with:

```bash
鲤 (load "some_file.carp")
```

The path to the file should be relative to where you started `carp` (or in your [search-path](Libraries.md) path).
It is also possible to paste a block of code (even multiple top-level expressions) into the REPL.

To build and run, first do:

```bash
鲤 (build)
```

And then:

```
鲤 (run)
```

## Running code from the terminal

If you don't want to work in the REPL and use a more classic "compile & run" setup, do this:

```bash
$ carp some_file.carp -x
```

Any files you list as arguments to `carp` will be loaded (this works when starting the REPL too).
The `-x` flag means that you want to compile and run the code immedately, exiting afterwards.

If you just want to build the executable, use `-b` instead:

```bash
$ carp some_file.carp -b
```
