#!/bin/bash

# Variables that must be set:
  # JAVA_HOME
  # IRCLOG_DATA_DIR
  # IRCLOG_INDEX_DIR
  # IRCLOG_LOGS_DIR
  # IRCLOG_PIDS_DIR


SCRIPT=`basename "$0"`
SCRIPT_DIR=`dirname "$0"`
MAIN=co.torri.reindxr.Main
CLASSPATH=$SCRIPT_DIR/lib/reindxr.jar
LOG_FILE=$IRCLOG_LOGS_DIR/$SCRIPT.log
PID_FILE=$IRCLOG_PIDS_DIR/$SCRIPT.pid

case $1 in
    start)
        ps -p `cat $PID_FILE`
        if [ $? -ne 0 ]
        then
            exec 2>&1 $JAVA_HOME/bin/java -cp $CLASSPATH $MAIN "$IRCLOG_DATA_DIR" "$IRCLOG_INDEX_DIR" 1> $LOG_FILE &
            echo $! > $PID_FILE;
        fi
        ;;
    stop)
        kill `cat $PID_FILE` ;;
    *)
        echo "usage: $SCRIPT {start|stop}" ;;
esac

exit 0
