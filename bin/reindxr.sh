#!/bin/bash

DATA_DIR=/Users/lucastorri/tmp/data
INDEX_DIR=/Users/lucastorri/tmp/index

SCRIPT=`basename "$0"`
JAVA_HOME=/System/Library/Frameworks/JavaVM.framework/Versions/1.6.0/Home/
MAIN=co.torri.reindxr.Main
CLASSPATH=/Users/lucastorri/Projects/reindxr/target/reindxr-assembly-1.0.jar
LOG_FILE=/Users/lucastorri/tmp/logs/$SCRIPT.log
PID_FILE=/Users/lucastorri/tmp/pids/$SCRIPT.pid

case $1 in
    start)
        ps -p `cat $PID_FILE`
        if [ $? -ne 0 ]
        then
            exec 2>&1 $JAVA_HOME/bin/java -cp $CLASSPATH $MAIN $DATA_DIR $INDEX_DIR 1> $LOG_FILE &
            echo $! > $PID_FILE;
        fi
        ;;
    stop)
        kill `cat $PID_FILE` ;;
    *)
        echo "usage: $SCRIPT {start|stop}" ;;
esac

exit 0
