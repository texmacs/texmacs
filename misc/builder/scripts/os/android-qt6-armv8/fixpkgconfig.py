import os
import shutil
os.chdir("local/lib")
files=os.listdir(".")
[shutil.copy(f, f.replace("_armeabi-v7a","")) for f in files if "_armeabi-v7a" in f]
[shutil.copy(f, f.replace("_arm64-v8a","")) for f in files if "_arm64-v8a" in f]
[shutil.copy(f, f.replace("_x86_64","")) for f in files if "_x86_64" in f]