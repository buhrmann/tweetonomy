-- Use inline view since the complex hour extraction can't be aliased in select and reused in group by...
-- SELECT hour_created, COUNT(*)
-- FROM (
--     SELECT hour(from_unixtime(unix_timestamp(created_at, "EEE MMM d HH:mm:ss Z yyyy"))) as hour_created
--     FROM ${hiveconf:tbl} 
--     WHERE coldate = ${hiveconf:day}
-- ) x
-- GROUP BY hour_created
-- ORDER BY hour_created

SELECT hour(from_unixtime(unix_timestamp(created_at, "EEE MMM d HH:mm:ss Z yyyy"))) as hour_created, COUNT(*)
    FROM ${hiveconf:tbl} 
    WHERE coldate = ${hiveconf:day}
GROUP BY hour(from_unixtime(unix_timestamp(created_at, "EEE MMM d HH:mm:ss Z yyyy")))