set hive.exec.dynamic.partition=true;
set hive.exec.dynamic.partition.mode=nonstrict;

CREATE TABLE IF NOT EXISTS samples LIKE parties;

INSERT OVERWRITE TABLE samples PARTITION (coldate)
SELECT id, created_at, lang, retweeted_status, entities, `text`, `user`, 
    in_reply_to_screen_name, in_reply_to_user_id, coldate 
FROM parties 
LIMIT 10000

--INSERT OVERWRITE TABLE samples PARTITION(coldate)
--SELECT id, created_at, lang, retweeted_status, entities, `text`, `user`, 
--    in_reply_to_screen_name, in_reply_to_user_id, coldate 
--FROM parties 
--TABLESAMPLE (100 ROWS) t;

