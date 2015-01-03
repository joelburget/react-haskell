#!/bin/bash
#
# This script is taken from https://github.com/ekmett/lens.
#
# It seems to fail if you most recently built with haste because somehow "cabal
# haddock" chooses hastec to build the docs, rather than ghc. So... you have to
# have run "cabal install" more recently than "haste-inst install". And for
# some reason, "cabal haddock" doesn't like the first line of React.Imports, so
# comment it out...
set -e

if [ "$#" -ne 1 ]; then
  echo "Usage: scripts/hackage-docs.sh HACKAGE_USER"
  exit 1
fi

user=$1

cabal_file=$(find . -maxdepth 1 -name "*.cabal" -print -quit)
if [ ! -f "$cabal_file" ]; then
  echo "Run this script in the top-level package directory"
  exit 1
fi

pkg=$(awk -F ":[[:space:]]*" 'tolower($1)=="name"    { print $2 }' < "$cabal_file")
ver=$(awk -F ":[[:space:]]*" 'tolower($1)=="version" { print $2 }' < "$cabal_file")

if [ -z "$pkg" ]; then
  echo "Unable to determine package name"
  exit 1
fi

if [ -z "$ver" ]; then
  echo "Unable to determine package version"
  exit 1
fi

echo "Detected package: $pkg-$ver"

dir=$(mktemp -d build-docs.XXXXXX)
trap 'rm -r "$dir"' EXIT

cabal haddock --hoogle --hyperlink-source --html-location='/package/$pkg-$version/docs' --contents-location='/package/$pkg-$version'

cp -R dist/doc/html/$pkg/ $dir/$pkg-$ver-docs
cp -R dist/doc/html/$pkg/ tmp/$pkg-$ver-docs

tar cvz -C $dir --format=ustar -f $dir/$pkg-$ver-docs.tar.gz $pkg-$ver-docs

curl -X PUT \
     -H 'Content-Type: application/x-tar' \
     -H 'Content-Encoding: gzip' \
     -u "$user" \
     --data-binary "@$dir/$pkg-$ver-docs.tar.gz" \
     "https://hackage.haskell.org/package/$pkg-$ver/docs"
