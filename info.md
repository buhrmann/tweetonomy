## Apache Twitter Source ##
Lives in org.apache.flume.source.twitter.TwitterSource. But: "Demo Flume source that connects via Streaming API to the 1% sample twitter firehose". Doesn't have option for keyword filtering. This should be easy to add. On the other hand, it serializes to avro already.

https://apache.googlesource.com/flume/+/0f4a66fb0f2946cd61dd8df31bd255fef7581cbc/flume-ng-sources/flume-twitter-source/src/main/java/org/apache/flume/source/twitter/TwitterSource.java

http://grepcode.com/file/repository.cloudera.com/content/repositories/releases/org.apache.flume.flume-ng-sources/flume-twitter-source/1.4.0-cdh4.5.0/org/apache/flume/source/twitter/TwitterSource.java

## Cloudera Twitter Source ##
Similar to above, but implements logging as statuses come in, and allows for keywords to be provided in agent.conf file. It writes out raw json though, rather than avro.
https://github.com/cloudera/cdh-twitter-example/blob/master/flume-sources/src/main/java/com/cloudera/flume/source/TwitterSource.java
Jar file is flume-sources-1.0.SNAPSHOT.jar

## Jars
Both the twitter-flume source, as well as json serde are now customized and build with maven from cloudera's source. Code is in cdh-... folder. For each jar there is a pom file. In this folder simply run "mvn package" to create the jar.

- hive-env.sh specifies a folder to look for extra jars
- hive-site.xml has property hive.aux.jars.path, which also specifies path to extra jars (one per jar file, not folder).

Update: lastest json serde is from rcongiu: 

- http://www.congiu.net/hive-json-serde/1.3.6-SNAPSHOT/hdp23/
- The source here: https://github.com/rcongiu/Hive-JSON-Serde. 

If in the future it also doesn't fit all purposes still others are here:

- https://github.com/sheetaldolas/Hive-JSON-Serde/tree/master
- https://github.com/apache/hive/blob/master/hcatalog/core/src/main/java/org/apache/hive/hcatalog/data/JsonSerDe.java (binary already in hive/hcatalog/...core.jar)

Also see http://ottomata.org/tech/too-many-hive-json-serdes/ and http://grokbase.com/t/cloudera/cdh-user/13529zytq4/hive-jsonserde-question.

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
6. Actually, have added jar extra path in some config file, so no need to add in each hql script. Check where!!! 

Network

Edges for:
- retweets: retweeted_status.retweet_count > 0 (also check for existence of "RT #user" etc..)
- mentions: entities.user_mentions.id (also includes replies)
- in-reply-to-user-id/name? Already included in mentions...

## Todo ##
- Tracking communities over time: 
    - detect communities for each day. compare over several days and extract common "subcommunities"
    - detect communities based on tweets for the whole period, then each day assign a node to
    the established communities


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

- Collection of spanish parties (twitter-conf changed 07.10.2015 to include smaller parties and general election hashtags!!! Rajoy announced elections on 01.10.2015):
    - http://www.t-cracia.info/#/clas/Pol%C3%ADticos
    - #27S
    - #20D, #20D2015, #Elecciones20D, (NOT #EleccionesGenerales2015. IN GUATEMALA!)
    - #Ciudadanos, #PSOE #PartidoPopular, #Podemos, #IzquierdaUnida, #UPyD (Added 01/12)
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
        - @Albert_Rivera(108994652, candidato), 
        - @InesArrimadas(552561770, portavoz)
    - Podemos: 
        - @ahorapodemos(2288138575), 
        - @Pablo_Iglesias_(158342368, candidato, secretario general), 
        - @ierrejon(482389606, secretario politico)
    - IU: 
        - @iunida(14824411), 
        - @agarzon(11904592, candidato/portavoz), 
        - @cayo_lara(296912924, coordinador federal)
    - Upyd:
        - @UPyD (146150851)
        - @Herzogoff, Andres Herzog (192618456, portavoz y candidato)
        - @rosadiezupyd (3018165370, portavoz grupo parlamentario, diputada)
    - ERC:
        - @Esquerra_ERC (84053338)
        - @junqueras, Oriol Junqueras (31416615, presidente)
        - @martarovira, Marta Rovira (14336062, secretaria general)
    - CiU (CDC & UDC, not separate again):
        - ciu: @ConvergenciaCAT (10242562, CDC) @unio_cat(436799542, UDC), 
        - @ArturMasCat, Artur Mas (2302092113, presidente CDC)
        - @DuranLleida, Josep Antoni Duran i Lleida (194070793, portavoz en congreso, UDC)

## Other Affiliations ##1
- independent left? 
    - BeatrizTalegon(Beatriz Talegon), 
    - AntonioMeastre(Antonio Meastre), 
    - MonederoJC(Juan Carlos Monedero), 
    - Coordinadora25S (organization), 
    - members of cli-as (alternativa socialista [e.g. @JLuisHReyes]), 
    - xtf-por-tenerife, 
    - @contrapoder
- IU: 
    - members of attac.es (e.g. alberto garzon, €edugaresp[Eduardo Garzon]...), 
    - democraciareal, 
    - members of the communist party (PCE, e.g. javier_parra)
- Vox (PP split-off): 
    - vox_baracaldo, 
    - vox_azu, 
    - vox_es, 
- Catalunya independence: 
    - CatalunyaNoPots, 
    - pilarcarracelas
    - ciu etc... 
- UPyD:
    - UPyD, Csilva2Carlos, cmgorriaran
- Group around issue of the closure (or ERE?) of the company Elcogas 
    - NOcierreELCOGAS
- Strikes and unions? 
    - repartidores_em, 
    - cgtemergencia [CGT], 
    - PacoLavadoG [CCOO]), 
    - also local podemos groups
- Prensa:
    - el_pais, PrensaCs, ElHuffPost, m_pais, publico_es, eldiarioes, SextaNocheTV, europapress, EPNacional, voz_populi, el_jueves
- And the rest? 
    - Strange mixed group, no obvious political affiliation or issues, perhaps around music? 
    - HAlterMusicNews etc.


