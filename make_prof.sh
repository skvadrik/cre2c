#!/bin/sh

make clean
make PROF="-auto-all -caf-all -prof -fforce-recomp"
