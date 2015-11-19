# Set starting date folder
if [ "$#" = 2 ]; then
    startdir=$2
    echo "Batch adding partitions from $startdir onwards!"
else
    # Find last already existing partition
    partStr=`hive -e 'SHOW PARTITIONS parties;'`
    partArr=( $partStr )
    numParts=${#partArr[@]}
    lastPart=${partArr[$numParts-1]}
    # Partitions are printed as "coldate=yyyymmdd"
    # Extract date part only:
    startdir=`echo $lastPart | tr -dc '[0-9]'` 
fi

# Drop starting partition
hive -e "ALTER TABLE parties DROP IF EXISTS PARTITION (coldate=$startdir);"

for path in `hdfs dfs -ls /user/flume/$1 | awk '{print $8}'`
do
    sdir=$(basename $path)
    if [[ "$sdir" > "$startdir" ]] || [[ "$sdir" == "$startdir" ]]; then
        echo "Addition partition $sdir..."
        hive --hiveconf tbl=$1 --hiveconf day=$sdir -f add-twitter-partition.hql
    fi
done
