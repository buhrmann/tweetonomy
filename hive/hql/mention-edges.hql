SELECT `user`.screen_name, mentioned_user, COUNT(*) AS num_mentions
FROM ${hiveconf:tbl}
LATERAL VIEW explode(entities.user_mentions.screen_name) mentions AS mentioned_user
WHERE coldate = ${hiveconf:day}
GROUP BY `user`.screen_name, mentioned_user
ORDER BY num_mentions DESC;
