--ADD JAR /usr/local/Cellar/hive-1.2.1/lib/cloudera-json-serde.jar;

DROP TABLE IF EXISTS ${hiveconf:tbl};
CREATE EXTERNAL TABLE ${hiveconf:tbl} (
  id BIGINT,
  created_at STRING,
  lang STRING,
  retweeted_status STRUCT<
    `user`:STRUCT<id:BIGINT, screen_name:STRING, name:STRING>, 
    retweet_count:INT>,
  entities STRUCT<
    urls:ARRAY<STRUCT<expanded_url:STRING>>,
    user_mentions:ARRAY<STRUCT<id:BIGINT, screen_name:STRING, name:STRING>>,
    hashtags:ARRAY<STRUCT<text:STRING>>>,
  text STRING,
  `user` STRUCT<id:BIGINT, screen_name:STRING, name:STRING>,
  in_reply_to_screen_name STRING,
  in_reply_to_user_id BIGINT
) 
PARTITIONED BY (coldate INT)
--ROW FORMAT SERDE 'org.apache.hive.hcatalog.data.JsonSerDe'
ROW FORMAT SERDE 'org.openx.data.jsonserde.JsonSerDe'
LOCATION '/user/flume/${hiveconf:tbl}';
