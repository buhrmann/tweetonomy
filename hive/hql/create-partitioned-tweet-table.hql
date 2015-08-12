ADD JAR /usr/local/Cellar/hive-1.2.1/lib/cloudera-json-serde.jar;

DROP TABLE IF EXISTS daily_tweets;
CREATE EXTERNAL TABLE daily_tweets (
  id BIGINT,
  created_at STRING,
  lang STRING,
  source STRING,
  favorited BOOLEAN,
  retweeted_status STRUCT<
    text:STRING,
    `user`:STRUCT<id:BIGINT, screen_name:STRING, name:STRING>, 
    retweet_count:INT>,
  entities STRUCT<
    urls:ARRAY<STRUCT<expanded_url:STRING>>,
    user_mentions:ARRAY<STRUCT<id:BIGINT, screen_name:STRING, name:STRING>>,
    hashtags:ARRAY<STRUCT<text:STRING>>>,
  text STRING,
  `user` STRUCT<
    id BIGINT,
    screen_name:STRING,
    name:STRING,
    friends_count:INT,
    followers_count:INT,
    statuses_count:INT,
    verified:BOOLEAN,
    utc_offset:INT,
    time_zone:STRING>,
  in_reply_to_screen_name STRING
  in_reply_to_user_id BIGINT
) 
PARTITIONED BY (coldate INT)
ROW FORMAT SERDE 'com.cloudera.hive.serde.JSONSerDe'
LOCATION '/user/flume/daily-greece-tweets';
