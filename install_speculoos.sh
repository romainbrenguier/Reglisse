#!/bin/bash

echo "Installing Speculoos..."

OCAML_SPECULOOS="Speculoos"
OCAML_SPECULOOS_ARCHIVE="$OCAML_SPECULOOS.zip"

if [ ! -e "$OCAML_SPECULOOS_ARCHIVE" ];
then
  echo " Downloading Speculoos..."
  wget https://github.com/romainbrenguier/Speculoos/archive/master.zip
fi

echo " Unpacking Speculoos..."
rm -rf $OCAML_SPECULOOS
mv master.zip $OCAML_SPECULOOS_ARCHIVE
unzip $OCAML_SPECULOOS_ARCHIVE
mv $OCAML_SPECULOOS-master $OCAML_SPECULOOS

echo " Compiling Speculoos..."
(cd $OCAML_SPECULOOS; make)
