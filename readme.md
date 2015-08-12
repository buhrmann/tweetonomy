## Apache Twitter Source ##
Lives in org.apache.flume.source.twitter.TwitterSource. But: "Demo Flume source that connects via Streaming API to the 1% sample twitter firehose". Doesn't have option for keyword filtering. This should be easy to add. On the other hand, it serializes to avro already.

https://apache.googlesource.com/flume/+/0f4a66fb0f2946cd61dd8df31bd255fef7581cbc/flume-ng-sources/flume-twitter-source/src/main/java/org/apache/flume/source/twitter/TwitterSource.java

http://grepcode.com/file/repository.cloudera.com/content/repositories/releases/org.apache.flume.flume-ng-sources/flume-twitter-source/1.4.0-cdh4.5.0/org/apache/flume/source/twitter/TwitterSource.java

## Cloudera Twitter Source ##
Similar to above, but implements logging as statuses come in, and allows for keywords to be provided in agent.conf file. It writes out raw json though, rather than avro.
https://github.com/cloudera/cdh-twitter-example/blob/master/flume-sources/src/main/java/com/cloudera/flume/source/TwitterSource.java
Jar file is flume-sources-1.0.SNAPSHOT.jar

## Twitter API ##

App: tweetonomy

1. Add cloudera twitter source .jar file to flume/lib (shouldn't be necessary to add to classpath in flume-env.sh) via symlink
2. Create twitter.conf file in flume.conf
3. Run flume-ng agent -n TwitterAgent -c conf -f conf/twitter.conf -Dflume.root.logger=DEBUG,console

This gets tweets into hadoop already. For Hive support:

1. Add hive-serdes-1.0-SHANPSHOT.jar file to hive/lib (the existing hive-serde-x.x.x.jar doesn't support json)
2. Add serde jar in hive (ADD JAR <path to serde.jar>), which stores it in distributed cache
3. Create hive table schema (see my .hv file in hive/config/scripts). Make sure columns with names that are reserved keywords are in back quotes (e.g. `user`)
4. Create table: hive -f path/to/schema.hql
5. When starting a hive shell for querying, have to first ADD JAR, as in schema file!

Network

Edges for:
- retweets: retweeted_status.retweet_count > 0 (also check for existence of "RT #user" etc..)
- mentions: entities.user_mentions.id (also includes replies)
- in-reply-to-user-id/name? Already included in mentions...

## Todo ##
- In retweet network:
    + leaders (also see http://www.egr.msu.edu/waves/publications_files/2012_03_zubair.pdf): 
        + have in-degree much higher than out-degree (are being retweeted a lot) 
        + also smallest average shortest path length to the rest of the users in the social network?
        + like hubs in small world networks?
        + lower clustering coefficients than followers
    + followers have higher out-degree than in-degree (they retweet others a lot); 

- Identify communities by use of hash-tags or content
    + Produce list of important keywords/topics in overall corpus of tweets per day
    + From hive export table of users and their content that day, i.e. hash tags, identified topics etc.
    + Cluster users by common topics
    + Assign user a cluster label

- Collection of spanish parties:
    - http://www.t-cracia.info/#/clas/Pol%C3%ADticos
    - #27S
    - PSOE: 
        -  @PSOE(50982086), 
        -  @sanchezcastejon(68740712, candidato, secretario general), 
        -  @AHernandoVera(276940518, portavoz congreso)
    - PP: 
        - @PPopular(20509689), 
        - @marianorajoy(343447873, candidato), 
        - @Rafa_Hernando(380456440, portavoz congreso)
    - Cuidadanos: 
        - @CiudadanosCs(19028805), 
        - @albert_rivera(108994652, candidato), 
        - @InesArrimadas(552561770, portavoz)
    - Podemos: 
        - @ahorapodemos(2288138575), 
        - @Pablo_Iglesias_(158342368, candidato, secretario general), 
        - @ierrejon(482389606, secretario politico)
    - IU: 
        - @iunida(14824411), 
        - @agarzon(11904592, candidato/portavoz), 
        - @cayo_lara(296912924, coordinador federal)

