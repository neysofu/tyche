#!/bin/bash
set -o errexit -o nounset
cd target
cd scala-2.12
cd api
git init
git config user.name "Filippo Costa"
git config user.email "filippocosta.italy@gmail.com"
git remote add upstream "https://$GH_TOKEN@github.com/neysofu/tyche.git"
git fetch upstream
git reset upstream/gh-pages
touch .
git add -A .
git commit -m "Rebuild pages"
git push -q upstream HEAD:gh-pages
