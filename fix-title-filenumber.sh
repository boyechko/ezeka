#!/usr/bin/bash
# -----------------------------------------------------------------------------
#      Author: Roderick Hoybach <hoybach-code@diachronic.net>
# Description: Edits all .txt file in place to fix file number on title line
#     Created: 2014-07-05T10:19
#    Comments: 
# -----------------------------------------------------------------------------

function fixp {
    read -p "Name: $1 vs Title: $2. Fix? " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        return 0;
    else
        return 1;
    fi
}

find ./ -maxdepth 1 -type f -name "*.txt" -print | while read file; do
    titleslug=$(head -1 $(basename "$file") | sed -re 's!^#.* ([0-9]{3}[A-Z0-9-]*)/.*!\1!');
    fileslug=$(basename -s .txt "$file");

    echo "File: $fileslug, Title: $titleslug"
    if [[ "$fileslug" != "$titleslug" ]]; then
        if fixp "$fileslug" "$titleslug"
        then
            # Preserve the modification time so it can be restored after
            changedate=$(stat -c '%y' $file);
            sed -i -r -e "1s= ([0-9]{3}[A-Z0-9-]*)/= ${fileslug}/=" $file
            touch -d "$changedate" $file
        fi
    fi
done
