#!/bin/bash -e

FILES=$(ls src)

xs=""
for f in $FILES; do
  xs+="src/$f "
done

echo "Generating documentation for ${xs}..."

# shellcheck disable=SC2086
stack exec -- haddock --html --ignore-all-exports --title=Progen-cli $xs --odir=docs

open docs/index.html
