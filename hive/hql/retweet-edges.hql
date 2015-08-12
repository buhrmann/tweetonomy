SELECT `user`.screen_name, retweeted_status.`user`.screen_name, COUNT(*) AS num_retweets
FROM ${hiveconf:tbl}
WHERE retweeted_status.retweet_count IS NOT NULL
AND coldate = ${hiveconf:day}
GROUP BY `user`.screen_name, retweeted_status.`user`.screen_name
ORDER BY num_retweets DESC;