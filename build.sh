#!/bin/sh
lein uberjar
mkdir -p target/build
cp -r res target/build/res
cp target/uberjar/nine-clj*standalone.jar target/build/nine.jar
echo "java -jar nine.jar" > target/build/run.sh
echo "java -jar nine.jar" > target/build/run.bat
(cd target/build; zip -r ../build.zip .)
rm -r target/build
