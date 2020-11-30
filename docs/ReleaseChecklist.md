# Release Checklist

Do all of these things (somewhat) in order:

# 1. Update Cabal project version

See the second line of the file [CarpHask.cabal](../CarpHask.cabal).

# 2. Update the "Welcome to Carp X.Y.Z" REPL message

See [Main.hs](../App/Main.hs).

# 3. Make a commit on master

```bash
$ git commit -m "Release X.Y.Z"
```

# 4. Tag the commit and push it

```bash
$ git tag vX.Y.Z
$ git push --tags
```

# 5. Update the blurb in README.md

See [README.md](../README.md)

# 6. Update the changelog

TODO: Create CHANGELOG.md
