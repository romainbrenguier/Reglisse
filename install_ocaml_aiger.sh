#!/bin/bash

echo "Installing Ocaml-aiger ..."

OCAML_AIGER="ocaml-aiger"
OCAML_AIGER_ARCHIVE="$OCAML_AIGER.tar.gz"

if [ ! -e "$OCAML_AIGER_ARCHIVE" ];
then
  echo " Downloading Ocaml-aiger utilities ..."
  wget http://www.ulb.ac.be/di/verif/rbrengui/ocaml-aiger/$OCAML_AIGER_ARCHIVE
fi

echo " Unpacking Ocaml-aiger ..."
rm -rf $OCAML_AIGER
mkdir $OCAML_AIGER
tar -xzf $OCAML_AIGER_ARCHIVE

echo " Compiling Ocaml-aiger ..."
(cd $OCAML_AIGER; make)
