#!/usr/bin/env python3
from os.path import realpath
from sys import argv,exit
if len(argv)<2: exit(1)
print(realpath(argv[1]))
