#!/usr/bin/env bash
set -e

if [ "$(git branch --show-current)" != "develop" ]; then
    echo "error: must be on develop branch" >&2
    exit 1
fi

stack exec condenso -- rebuild

git checkout master
rsync -a --force \
    --filter='P _site/' \
    --filter='P _cache/' \
    --filter='P .git/' \
    --filter='P .gitignore' \
    --filter='P .stack-work' \
    --delete-excluded \
    _site/ .

git add -A
git commit -m "${1:-deploy}"
git checkout develop

