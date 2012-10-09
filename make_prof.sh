#!/bin/sh

make clean
make PROF="-rtsopts -auto-all -prof"
