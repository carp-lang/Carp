#!/usr/bin/env sh
set -e;

name=$1
noprompt=$2

if [ "$name" = "" ]; then
   echo "ERROR: Must pass a name of the release as the first argument to this script.";
   exit 1;
fi

fullPath="$PWD/releases/$name"

echo "Creating release '$name'"
echo "Full path = '$fullPath'"

if [ "$noprompt" = "--noprompt" ]; then
    echo "No prompt, will continue"
else
    echo "Continue? (y/n)"
    read answer
    if [ "$answer" != "y" ]; then
        echo "Bye!"
        exit 1;
    fi
fi

mkdir -p "$fullPath"

echo
echo "Building Haskell Stack project..."
stack build

./scripts/carp.sh ./docs/core/generate_core_docs.carp

mkdir "$fullPath/bin"
echo "Copying executable..."
cp "$(stack path --local-install-root)/bin/carp" $fullPath/bin/carp
echo "Copying core..."
cp -r "./core/" "$fullPath/core/"
echo "Copying docs..."
cp -r "./docs/" "$fullPath/docs/"
echo "Copying README.md..."
cp -r "./README.md" "$fullPath/README.md"
echo "Copying img..."
cp -r "./resources/" "$fullPath/resources/"
echo "Copying examples..."
cp -r "./examples/" "$fullPath/examples/"

echo
echo "Zipping..."
cd releases
zip -r "${name}.zip" "${name}"

echo
echo "Done. New release created successfully!"
