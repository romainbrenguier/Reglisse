#!/bin/bash

echo "Installing Ocaml-CUDD..."

OCAML_CUDD="ocaml-cudd"
OCAML_CUDD_ARCHIVE="$OCAML_CUDD.zip"

if [ ! -e "$OCAML_CUDD_ARCHIVE" ];
then
  echo " Downloading Ocaml-CUDD..."
  wget https://github.com/romainbrenguier/ocaml-cudd/archive/master.zip
fi

echo " Unpacking Ocaml-CUDD..."
rm -rf $OCAML_CUDD
mv master.zip $OCAML_CUDD_ARCHIVE
unzip $OCAML_CUDD_ARCHIVE
mv $OCAML_CUDD-master $OCAML_CUDD-master

echo " Compiling Ocaml-CUDD..."
(cd $OCAML_CUDD-master; ./install_cudd.sh; make)
