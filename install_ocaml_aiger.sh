#!/bin/bash

echo "Installing Ocaml-aiger ..."

OCAML_AIGER="ocaml-aiger"
OCAML_AIGER_ARCHIVE="$OCAML_AIGER.tar.gz"

if [ ! -e "$OCAML_AIGER_ARCHIVE" ];
then
  echo " Downloading Ocaml-aiger utilities ..."
  #wget http://www.ulb.ac.be/di/verif/rbrengui/ocaml-aiger/$OCAML_AIGER_ARCHIVE
  wget https://github.com/romainbrenguier/ocaml-aiger/archive/master.zip
fi

echo " Unpacking Ocaml-aiger ..."
rm -rf $OCAML_AIGER
mkdir $OCAML_AIGER
#tar -xzf $OCAML_AIGER_ARCHIVE
mv master.zip $OCAML_AIGER.zip
unzip $OCAML_AIGER.zip

echo " Compiling Ocaml-aiger ..."
(cd $OCAML_AIGER; make)
