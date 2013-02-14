#!/bin/sh

make clean
make PROF="-rtsopts -auto-all -caf-all -prof -fforce-recomp"
