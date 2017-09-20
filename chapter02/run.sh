#!/bin/sh
# rm -rf _build
[ -d "_build" ] && \
rm -rf _build;
echo "Dir: \"_build\" removed..." || \
echo "Dir: \"_build\" not available..."

# rm driver.native
[ -f "driver.native" ] && \
rm driver.native;
echo "Executable: \"driver.native\" removed..." || \
echo "Executable: \"driver.native\" not available..."

echo "Building..."
ocamlbuild -use-menhir -tag thread -use-ocamlfind -quiet -pkg core driver.native;

echo "Done...\n"
./driver.native testcases/queens.tig
