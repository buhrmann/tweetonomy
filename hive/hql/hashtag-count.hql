-- Needs inner view as lower cannot be applied directly to array before exploding
SELECT ltag, COUNT(*) as cnt 
FROM (
    SELECT lower(tag) as ltag 
    FROM ${hiveconf:tbl} 
    LATERAL VIEW EXPLODE(entities.hashtags.text) tagtable AS tag 
    WHERE coldate = ${hiveconf:day}
) ltagtab
GROUP BY ltag
ORDER BY cnt DESC;

-- Alternative, applying pre-grouping transform twice (slower it seems!)
-- SELECT lower(tag) as ltag, COUNT(*) as cnt
-- FROM ${hiveconf:tbl} 
--     LATERAL VIEW EXPLODE(entities.hashtags.text) tagtable AS tag 
-- WHERE coldate=${hiveconf:day}
-- GROUP BY lower(tag)
-- ORDER BY cnt DESC;