for path in `hdfs dfs -ls /user/flume/$1 | awk '{print $8}'`
do
    sdir=$(basename $path)
    hive --hiveconf tbl=$1 --hiveconf day=$sdir -f add-twitter-partition.hql
done
