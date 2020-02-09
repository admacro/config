#!/bin/bash

EMACS_D=.emacs.d

cd ~
if [ -d "$EMACS_D" ]; then
    BACKUP="${EMACS_D}_backup"
    echo "$EMACS_D exists. Rename to $BACKUP."
    mv "$EMACS_D" "$BACKUP"
fi

echo "Creating dir $EMACS_D"
mkdir ~/"$EMACS_D"

echo "Copy init.el to $EMACS_D"
cp ~/prog/config/emacs/emacs-init.el "$EMACS_D"/init.el

