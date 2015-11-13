#!/usr/bin/env bash
out="$(git status --porcelain)"
if [[ ! -z "$out" ]]; then
  echo uncommitted changes: "$out"
  exit 99
fi
