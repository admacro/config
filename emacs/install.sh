#!/bin/bash

EMACS_D=.emacs.d
CWD=$(pwd)
SOURCE_D=$(cd $(dirname "$0") && pwd)

cd ~
if [ -d "$EMACS_D" ]; then
    BACKUP="${EMACS_D}_backup"
    echo "$EMACS_D exists. Rename to $BACKUP."
    mv "$EMACS_D" "$BACKUP"
fi

echo "Creating dir $EMACS_D"
mkdir ~/"$EMACS_D"

cd ~/"$EMACS_D"
echo "Create init.el in $EMACS_D"
echo "(load \"${SOURCE_D}/init.el\")" > "init.el"

