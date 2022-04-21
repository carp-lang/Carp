# Release Checklist

Do all of these things (somewhat) in order:

## 1. Update Cabal project version

See the second line of the file [CarpHask.cabal](../CarpHask.cabal).

## 2. Update the "Welcome to Carp X.Y.Z" REPL message

See [Main.hs](../App/Main.hs).

## 3. Update the blurb in README.md

See [README.md](../README.md)

## 4. Update the changelog

See [CHANGELOG.md](../CHANGELOG.md)

## 5. Make a commit on master

```bash
$ git add .
$ git commit -m "build: Release X.Y.Z"
```

## 6. Tag the commit and push it

```bash
$ git tag vX.Y.Z
$ git push --tags
```
