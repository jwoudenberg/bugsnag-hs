#!/usr/bin/env bash

set -euxo pipefail

name=$(cat package.yaml | grep name: | awk '{print $2}')
version=$(cat package.yaml | grep version: | awk '{print $2}')
bundle="$name-$version.tar.gz"

hpack
cabal sdist -o - > "$bundle"
cabal upload --publish "$bundle"
cabal upload -d --publish
