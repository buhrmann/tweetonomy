-- Usage: hive --hiveconf tbl='parties' --hiveconf day=20150804 -f hashtag_freq.hql 
SELECT lkey, COUNT(*) AS freq
FROM (
    SELECT lower(keyword) as lkey
    FROM ${hiveconf:tbl}
    LATERAL VIEW EXPLODE(entities.hashtags.text) hashtable AS keyword
    WHERE coldate = ${hiveconf:day}
    ) lkeytab
GROUP BY lkey
ORDER BY freq DESC
LIMIT 100;