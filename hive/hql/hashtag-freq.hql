-- Usage: hive --hiveconf tbl='parties' --hiveconf day=20150804 -f hashtag_freq.hql 
-- Needs inner view as lower cannot be applied directly to array before exploding
SELECT ltag, COUNT(*) AS freq
FROM (
    SELECT TRANSLATE(LOWER(tag), "áéíóúüñ", "aeiouun") as ltag
    FROM ${hiveconf:tbl}
    LATERAL VIEW EXPLODE(entities.hashtags.text) hashtable AS tag
    WHERE coldate = ${hiveconf:day}
    ) lkeytab
GROUP BY ltag
ORDER BY freq DESC;

-- Alternative, applying pre-grouping transform twice (slower it seems!)
-- SELECT lower(tag) as ltag, COUNT(*) as cnt
-- FROM ${hiveconf:tbl} 
--     LATERAL VIEW EXPLODE(entities.hashtags.text) tagtable AS tag 
-- WHERE coldate=${hiveconf:day}
-- GROUP BY lower(tag)
-- ORDER BY cnt DESC;