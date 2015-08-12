-- Usage: hive --hiveconf tbl='parties' --hiveconf day=20150804 -f add-twitter-partition.hql
ALTER TABLE ${hiveconf:tbl} DROP IF EXISTS PARTITION (coldate=${hiveconf:day});
ALTER TABLE ${hiveconf:tbl} ADD PARTITION (coldate=${hiveconf:day}) 
    LOCATION '/user/flume/${hiveconf:tbl}/${hiveconf:day}';