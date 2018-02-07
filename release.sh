#!/bin/bash
set -e;

name=$1

if [ "$name" == "" ]; then
   echo "ERROR: Must pass a name of the release as the first argument to this script.";
   exit 1;
fi

fullPath="$PWD/releases/$name"

echo "Creating release '$name'"
echo "Full path = '$fullPath'"
echo "Continue? (y/n)"

read answer

if [ "$answer" != "y" ]; then
    echo "Bye!"
    exit 1;
fi

mkdir "$fullPath"

echo
echo "Building Haskell project..."
stack build

carpExePath="$(which carp)"

if [ "$carpExePath" == "" ]; then
   echo "ERROR: Can't find the carp executable on your system.";
   exit 1;
fi

echo "Path of Carp executable = '$carpExePath'"

mkdir "$fullPath/bin"
echo "Copying executable..."
cp $carpExePath "$fullPath/bin/carp"
echo "Copying core..."
cp -r "./core/" "$fullPath/core/"
echo "Copying docs..."
cp -r "./docs/" "$fullPath/docs/"
echo "Copying README.md..."
cp -r "./README.md" "$fullPath/README.md"
echo "Copying img..."
cp -r "./img/" "$fullPath/img/"
echo "Copying examples..."
cp -r "./examples/" "$fullPath/examples/"

echo
echo "Zipping..."
cd releases
zip -r "${name}.zip" "${name}"

echo
echo "Done. New release created successfully!"
