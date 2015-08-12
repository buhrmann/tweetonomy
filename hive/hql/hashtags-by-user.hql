SELECT `user`.screen_name, collect_set(lower(hash)) as hashes
FROM ${hiveconf:tbl}
LATERAL VIEW EXPLODE(entities.hashtags.text) tagtable AS hash
WHERE coldate = ${hiveconf:day}
GROUP BY `user`.screen_name
ORDER BY size(hashes) DESC;