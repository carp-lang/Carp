# Contributing Guide

Thank you for considering contributing to Carp!

This document currently focuses on developer contributions but we welcome all kinds of contributions.

## Community
The best place to start is to join the Carp Gitter channel over at
[https://gitter.im/carp-lang/Carp](https://gitter.im/carp-lang/Carp).

## Understanding the Compiler
See [Hacking.md](hacking.md) for a gentle introduction to the Carp compiler internals.

## Committing to the repo
We follow [Conventional Commits](https://www.conventionalcommits.org), there is
a `commit-msg` hook to ensure the messages follow the convention, please make
sure you have run `./scripts/git-hooks/setup.sh` before you commit the first
time so that the hook is in place.

## AI and agents
If you used AI tools while working on a patch, please say how in the pull
request description: which tools, and what they did (for instance writing
code, writing tests, or drafting the description). A sentence or two is
enough. This tells reviewers where to look closely.

Issues tagged [`beginner`](https://github.com/carp-lang/Carp/labels/beginner)
are meant for people who want to learn Carp and its compiler. Please leave them
to those contributors, and don't use agents to open pull requests for them.

License
-------
Carp is currently released under the terms of the ASL 2.0 license.
