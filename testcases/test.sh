#!/bin/sh

IFS='
'

notFound(){
    echo "!->> tag $1 was ${2}found"
    exitCode=1
}

testCase(){
  sourceFile=$1
  tagsFile=$2
  not=$3
  fileSearch="$tagsFile "
  [ "$tagsFile" = "tags" ] && fileSearch=""
  code=1
  [ -n "$not" ] && code=0
  echo -n "${fileSearch}${not}to be found count: "
  toBeFound=$(sed -n 's/\s*-- '${fileSearch}${not}'to be found //p' $sourceFile)
  echo "$toBeFound" | wc -l
  for i in $toBeFound; do
    grep -l "^$i" $tagsFile 2>&1 > /dev/null
    [ $? -eq $code ] && notFound $i $not
  done
}

checkToBeFound(){
  hasktags=$1
  sourceFile=`echo $2/*.*` # exclude tags file
  echo
  echo
  echo "running test on $file"
  [ -f tags ] && rm tags
  $hasktags -b $sourceFile 2> /dev/null

  for not in "" "not "; do
      for tagsFile in tags TAGS; do
          testCase $sourceFile $tagsFile $not
      done
  done

  cp tags TAGS $2/
}

# compile hasktags
cd ..
set -e
runhaskell Setup.hs configure $@
runhaskell Setup.hs build
set +e
cd -
exitCode=0

for f in `find . -mindepth 1 -maxdepth 1 -type d`; do
  checkToBeFound ../dist/build/hasktags/hasktags $f
done

exit $exitCode
