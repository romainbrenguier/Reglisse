#!/bin/bash

echo "Installing Ocaml-CUDD..."

OCAML_CUDD="ocaml-cudd"
OCAML_CUDD_ARCHIVE="$OCAML_CUDD.tar.gz"

if [ ! -e "$CUDD_ARCHIVE" ];
then
  echo " Downloading Ocaml-CUDD..."
  #wget http://www.ulb.ac.be/di/verif/rbrengui/ocaml-cudd/$OCAML_CUDD_ARCHIVE
  wget https://github.com/romainbrenguier/ocaml-cudd/archive/master.zip
fi

echo " Unpacking Ocaml-CUDD..."
rm -rf $OCAML_CUDD
mkdir $OCAML_CUDD
#tar -xzf $OCAML_CUDD_ARCHIVE
mv master.zip $OCAML_CUDD.zip
unzip $OCAML_CUDD.zip

echo " Compiling Ocaml-CUDD..."
(cd $OCAML_CUDD; ./install_cudd.sh; make)
