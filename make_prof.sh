#!/bin/sh

make clean
make PROF="-auto-all -prof"
