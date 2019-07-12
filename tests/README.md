# Guide to Run Unit Tests with ctest

First, compile the whole project.
```
cd texmacs/
mkdir build/ && cd build/
cmake ..
make -j8
```

Then, run your unit tests:
```
ctest // run all
ctest -R analyze // run unit tests with name containing `analyze`
```

## Advanced Topic
You may also run the unit tests via the binaries under `${cmake_build_dir}/tests/`
``` bash
tests/converter_test
```

However, this specify unit test will fail. For `utf8_to_cork`, we need to set
the `TEXMACS_PATH` to find the dictionaries. You may specify it manually:
``` bash
TEXMACS_PATH=/path/to/somewhere tests/converter_test
```

Or just using ctest(we've set the necessary environment variables):
``` bash
ctest -R converter_test
```
