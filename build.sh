#!/bin/bash
set -euo pipefail
IFS=$'\n\t'


cd src
ghc -O App.hs

mv App.exe ../
