# batch-query-day parties hql-script outfolder [starting-date-str]

# Set starting date folder
startdir="00000000"
if [ "$#" = 4 ]; then
    startdir=$4
    echo "Batch querying $2 from $startdir onwards!"
fi

# Loop over daily folders
for path in `hdfs dfs -ls /user/flume/$1 | awk '{print $8}'`
do
    sdir=$(basename $path)
    if [[ "$sdir" > "$startdir" ]] || [[ "$sdir" == "$startdir" ]]; then
        #if [ ! -e "../../data/$1/$3/$sdir.txt" ]; then
            echo "Processing folder $sdir..."
            hive --hiveconf tbl=$1 --hiveconf day=$sdir -f $2 > ../../data/$1/$3/$sdir.txt
        #fi
    fi
done
