#!/usr/bin/zsh
#
# Adjust the access and modification times for each *.txt file in the current
# directory to the time/date recorded in the first line of the file.

# find ./ -maxdepth 1 -type f -name "*.txt" -print | while read file; do
#     firstline=$(head -1 $file)
#     if [[ "$firstline" =~ ([0-9-]{10})T([0-9:]{5}) ]]; then
#         time="${BASH_REMATCH[1]} ${BASH_REMATCH[2]}"
#         echo "$file: $time";
#     elif [[ "$firstline" =~ ([0-9-]{10}) ]]; then
#         time="${BASH_REMATCH[1]}"
#         echo "$file: $time";
#     else
#         echo "$file: NO MATCH '$firstline'";
#         time=""
#     fi
# 
#     if [ "$time" != "" ]; then
#         touch -d "$time" $file;
#     fi
# done

# retouch(file, modified/created)
function retouch {
    internal="$2"
    stat=$(stat -c '%y' $file | awk -e '{print $1}')
    printf "%-12s | internal: %s, stat: %s " $file $internal $stat
    
    if [[ "$stat" =~ "2015-05-09" ]]; then
        touch -d $internal $file
        echo "Retouched"
    else
        # Do nothing
        echo ""
    fi
}

find ./ -maxdepth 1 -type f -name "*.txt" -print | while read file; do
    top=$(head -n 2 $file)
    if [[ "$top" =~ "modified: +([0-9-]{10})" ]]; then
        retouch $file $match[1]
    elif [[ "$top" =~ "created: +([0-9-]{10})" ]]; then
        retouch $file $match[1]
    else
        echo $top
    fi
done
