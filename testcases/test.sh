#!/bin/sh

checkToBeFound(){
  hasktags=$1
  file=`echo $2/*`
  echo
  echo
  echo "running test on $file"
  echo -n "to be found count: "
  toBeFound=$(sed -n 's/-- to be found\s*//p' $file)
  echo "$toBeFound" | wc -l

  [ -f tags ] && rm tags
  $hasktags $file &> /dev/null
  for i in $toBeFound; do
    grep -l $i tags 2>&1 > /dev/null || echo "!->> tag $i was not found"
  done
}

# compile hasktags
cd ..
ghc --make hasktags.hs -o hasktags
cd -

for f in `find . -maxdepth 1 -type d `; do
  checkToBeFound ../hasktags $f
done
