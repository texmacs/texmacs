# How to build TeXmacs/wasm

You need to have already build a version of Qt 6.9 suitable for your platform. 

Install emscripten  (see https://emscripten.org/docs/getting_started/downloads.html)

```shell
# Get the emsdk repo
git clone https://github.com/emscripten-core/emsdk.git

# Enter that directory
cd emsdk
```

then select a particular version compatible with Qt 6.9 and activate it

```shell
# Fetch the latest version of the emsdk (not needed the first time you clone)
git pull

# Download and install the 3.1.70 SDK tools.
./emsdk install 3.1.70

# Make the 3.1.70 SDK "active" for the current user. (writes .emscripten file)
./emsdk activate 3.1.70

# Activate PATH and other environment variables in the current terminal
source ./emsdk_env.sh
```

In the builder create the `wasm-emscripten` folder and copy there the files in this folder (i.e. `builder/scripts/os/wasm-emscripten`)
```shell
mkdir wasm-emscripten
cd wasm-emscripten
cp scripts/os/wasm-emscripten/* .
```

Set devel paths with
```shell
source set-devel-paths
```

Build the various libraries
```shell
make
```

Go in the texmacs source directory and overwrite CMakeLists.txt with a version tailored for wasm
```
ln -s packages/wasm/CMakeLists.txt.qt6 CMakeLists.txt
```
then
```
mkdir build-wasm
cd build-wasm
$WORKING_DIR/local/bin/qt-cmake ..
cmake --build .
```


