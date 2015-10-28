SELECT lang, COUNT(*) as cnt 
FROM ${hiveconf:tbl}
WHERE coldate=${hiveconf:day}
GROUP BY lang 
ORDER BY cnt DESC