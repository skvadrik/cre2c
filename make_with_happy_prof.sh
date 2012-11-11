#!/bin/sh

make clean_all
make PROF="-auto-all -prof"