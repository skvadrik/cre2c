#!/bin/sh

make clean_all
make PROF="-rtsopts -auto-all -caf-all -prof -fforce-recomp"