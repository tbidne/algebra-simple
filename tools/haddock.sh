set -e

export LANG="C.UTF-8"

cabal haddock --haddock-hyperlink-source --haddock-quickjump

mkdir -p docs/

# shellcheck disable=SC2038
find docs/ -type f | xargs -I % sh -c "rm -r %"

cp -r dist-newstyle/build/x86_64-linux/ghc-*/algebra-simple-0.1/doc/html/algebra-simple/* docs/

# update png size in html
needle="src=\".\/diagrams\/hierarchy.png\""

needle_sz="$needle width=\"1080\" height=\"458\""

sed -i -e "s/$needle/$needle_sz/g" docs/Numeric-Algebra.html
