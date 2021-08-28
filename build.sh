#!/bin/bash
set -e
sbt compilerJVM/universal:packageBin
unzip -o "jvm/target/universal/kaitai-struct-compiler-0.10-SNAPSHOT.zip" "kaitai-struct-compiler-0.10-SNAPSHOT/*" -d "output"
