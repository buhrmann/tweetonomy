-- All scripts/data used by scorer should be included.
-- Based on slides "Large-scale predictive modelling..." by Alez Zolotovitski.
-- USAGE: hive --hiveconf model="glm.Rdata" --hiveconf rscript="hive-senti-score.R" -f sentiment-score.hql
ADD FILE /Users/thomasbuhrmann/Code/tweetonomy/data/parties/sentiment/scorers/${hiveconf:model};
ADD FILE /Users/thomasbuhrmann/Code/tweetonomy/R/${hiveconf:rscript};

CREATE TABLE IF NOT EXISTS sentiments (
  id BIGINT,
  positive TINYINT
);

INSERT OVERWRITE TABLE sentiments 
SELECT TRANSFORM(tweets.id, tweets.text) USING 'rscript ${hiveconf:rscript} ${hiveconf:model}' 
AS id, positive
FROM
(
    SELECT id, trim(regexp_replace(text, '\r\n+|\r+|\n+|\t+|\s+]', ' ')) AS text FROM samples
) tweets

