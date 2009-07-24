#!/bin/sh

checkToBeFound(){
  hasktags=$1
  file=`echo $2/*.*` # exclude tags file
  echo
  echo
  echo "running test on $file"
  echo -n "to be found count: "
  toBeFound=$(sed -n 's/-- to be found\s*//p' $file)
  echo "$toBeFound" | wc -l

  [ -f tags ] && rm tags
  $hasktags $file &> /dev/null
  for i in $toBeFound; do
    grep -l "^$i" tags 2>&1 > /dev/null || echo "!->> tag $i was not found"
  done
  cp tags $2/tags
}

# compile hasktags
cd ..
ghc --make hasktags.hs -o hasktags
cd -

for f in `find . -mindepth 1 -maxdepth 1 -type d`; do
  checkToBeFound ../hasktags $f
done
