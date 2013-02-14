#!/bin/sh

make clean_all
make PROF="-auto-all -caf-all -prof -fforce-recomp"