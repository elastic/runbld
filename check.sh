#!/usr/bin/env bash
if [[ ! -z "$(git status --porcelain)" ]]; then
  echo uncommitted changes
  exit 99
fi
