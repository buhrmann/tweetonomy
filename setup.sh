# Flume 
# Just add required .jar files to lib, other settings are in local /conf dir
rm $FLUME_HOME/lib/flume-sources-1.0-SNAPSHOT.jar
ln -s /Users/thomasbuhrmann/Code/tweetonomy/jars/flume-sources-1.0-SNAPSHOT.jar $FLUME_HOME/lib

# Hive
# Create symlinks to local configuration files
# The .env file specifies location of custom .jar files in this case
cp $HIVE_HOME/conf/hive-site.xml $HIVE_HOME/conf/hive-site.xml.BCK
cp $HIVE_HOME/conf/hive-env.sh $HIVE_HOME/conf/hive-env.sh.BCK
rm $HIVE_HOME/conf/hive-site.xml
rm $HIVE_HOME/conf/hive-env.sh
ln -s /Users/thomasbuhrmann/Code/tweetonomy/hive/hive-site.xml $HIVE_HOME/conf
ln -s /Users/thomasbuhrmann/Code/tweetonomy/hive/hive-env.sh $HIVE_HOME/conf