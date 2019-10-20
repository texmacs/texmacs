#!/bin/bash

# [submodule "misc/benchmark/gbench"]
# 	path = misc/benchmark/gbench
# 	url = https://github.com/google/benchmark
# 	branch = tags/v1.5.0
# [submodule "tests/gtest"]
# 	path = tests/gtest
# 	url = https://github.com/google/googletest
# 	branch = tags/v1.10.0

# change to TEXMACS_SOURCE_DIR
if [ -L ${BASH_SOURCE-$0} ]; then
  TEXMACS_SOURCE_DIR=$(dirname $(readlink "${BASH_SOURCE-$0}"))/../../
else
  TEXMACS_SOURCE_DIR=$(dirname "${BASH_SOURCE-$0}")/../../
fi

cd $TEXMACS_SOURCE_DIR

if [ ! -e misc/benchmark/gbench ];then
  git clone https://github.com/google/benchmark misc/benchmark/gbench --branch v1.5.0 --depth 1
fi

if [ ! -e tests/gtest ];then
  git clone https://github.com/google/googletest tests/gtest --branch release-1.10.0 --depth 1
fi
