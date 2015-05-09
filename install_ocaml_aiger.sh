#!/bin/bash

echo "Installing Ocaml-aiger ..."

OCAML_AIGER="ocaml-aiger"
OCAML_AIGER_ARCHIVE="$OCAML_AIGER.zip"

if [ ! -e "$OCAML_AIGER_ARCHIVE" ];
then
  echo " Downloading Ocaml-aiger utilities ..."
  wget https://github.com/romainbrenguier/ocaml-aiger/archive/master.zip
fi

echo " Unpacking Ocaml-aiger ..."
rm -rf $OCAML_AIGER
mv master.zip $OCAML_AIGER_ARCHIVE
unzip $OCAML_AIGER_ARCHIVE
mv $OCAML_AIGER-master $OCAML_AIGER

echo " Compiling Ocaml-aiger ..."
(cd $OCAML_AIGER; make)
