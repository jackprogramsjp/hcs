#!/bin/bash
for file in src/*.hs; do
  "hindent" "$file"
done

for file in app/*.hs; do
  "hindent" "$file"
done

for file in test/*.hs; do
  "hindent" "$file"
done
