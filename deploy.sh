#!/bin/bash

set -o errexit -o nounset

if [ "$TRAVIS_BRANCH" != "master" ]
then
	echo "This commit was made against the $TRAVIS_BRANCH and not the master! No deploy!"
	exit 0
fi

# Short hash of HEAD
rev=$(git rev-parse --short HEAD)

cd target/latest/api

git init
git config user.name "Filippo Costa"
git config user.email "filippocosta.italy@gmail.com"

git remote add upstream "https://$GH_TOKEN@github.com/rust-lang/rust-by-example.git"
git fetch upstream
git reset upstream/gh-pages

touch .

git add -A .
git commit -m "rebuild pages at ${rev}"
git push -q upstream HEAD:gh-pages
