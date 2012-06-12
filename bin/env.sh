REINDXR_HOME=$HOME/tmp/reindxr

JAVA_HOME=${JAVA_HOME:=/System/Library/Frameworks/JavaVM.framework/Versions/1.6/Home/}
DATA_DIR=${DATA_DIR:+${REINDXR_HOME}/data}
INDEX_DIR=${DATA_DIR:+${REINDXR_HOME}/index}
LOGS_DIR=${DATA_DIR:+${REINDXR_HOME}/logs}
PIDS_DIR=${DATA_DIR:+${REINDXR_HOME}/pids}

mkdir -p $DATA_DIR
mkdir -p $INDEX_DIR
mkdir -p $LOGS_DIR
mkdir -p $PIDS_DIR