startdir="00000000"
if [ "$#" = 1 ]; then
    startdir=$1
    echo "Export all: processing folders starting from $startdir onwards!"
fi

# Make sure all partitions are loaded
./batch-add-partitions.sh parties $startdir

# all per-day retweet and mention networks
./batch-query-day.sh parties retweet-edges.hql retweet-edges $startdir
./batch-query-day.sh parties mention-edges.hql mention-edges $startdir

# hashtags per user per day
./batch-query-day.sh parties hashtags-by-user.hql hashtags-by-user $startdir

# hashtag frequency per day
./batch-query-day.sh parties hashtag-freq.hql hashtag-freq $startdir

# tweet frequency per hour
./batch-query-day.sh parties hourly-count.hql hourly-count $startdir

# all-time retweet network
#hive --hiveconf tbl=parties -f all-retweet-edges.hql > ../../data/parties/retweet-edges/all-retweet-edges.txt 
#hive --hiveconf tbl=parties -f all-mention-edges.hql > ../../data/parties/mention-edges/all-mention-edges.txt 
