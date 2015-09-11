# Set starting date folder
startdir="00000000"
if [ "$#" = 2 ]; then
    startdir=$2
    echo "Batch adding partitions from $startdir onwards!"
fi


for path in `hdfs dfs -ls /user/flume/$1 | awk '{print $8}'`
do
    sdir=$(basename $path)
    if [[ "$sdir" > "$startdir" ]] || [[ "$sdir" == "$startdir" ]]; then
        echo "Addition partition $sdir..."
        hive --hiveconf tbl=$1 --hiveconf day=$sdir -f add-twitter-partition.hql
    fi
done
