#! /usr/bin/env bash

# Temporarily store uncommited changes
git stash

# Verify correct branch
git checkout develop

# Build new files
cabal run akrmn-github-io clean
cabal run akrmn-github-io build

# Get previous files
git fetch --all
git checkout -b site --track origin/site

# Overwrite existing files with new files
rsync -a --filter='P _site/'      \
         --filter='P _cache/'     \
         --filter='P .git/'       \
         --filter='P .gitignore'  \
         --filter='P .stack-work' \
         --filter='P CNAME'       \
         --delete-excluded        \
         _site/ .

# Commit
git add -A
git commit -m "Publish."

# Push
git push origin site:site

# Restoration
git checkout develop
git branch -D site
git stash pop
