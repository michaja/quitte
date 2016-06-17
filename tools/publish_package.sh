#!/bin/sh

# location of the R repository
repo="/home/pehl/www/R/src/contrib/"

# get package base directory
if [[ "tools" = $( basename $PWD ) ]]
then
  DIR=$( dirname $PWD )
else
  DIR=$PWD
fi

# check if SVN repository is up to date
if [[ 0 -ne $( svn status -q $DIR | wc -l ) ]]
then
  echo "Error: SVN repository is not up to date"
  exit 1
fi

# modify DESCRIPTION file with current date and version number
date=$( date +"%Y-%m-%d" )
revision=$( svn info $DIR | sed -n 's/Last Changed Rev: \([0-9]\+\)$/\1/p' )

sed -i $DIR/DESCRIPTION \
    -e "s/\(Version: [0-9]\+\.[0-9]\+\.\)[0-9]*/\1$revision/" \
    -e "s/Date: [0-9-]\{10\}/Date: $date/"

# build package
R CMD build $DIR | tee .package.name

package=$( sed -n 's/^\* building .\(.*\)./\1/p' .package.name )
rm .package.name

# move package to R repository
mv $package $repo

# update PACKAGES file in R repository
Rscript -e "library(tools); write_PACKAGES(\"$repo\")"

# revert changes to SVN repository
svn revert -qR $DIR

