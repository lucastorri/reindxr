DEFAULT_REINDXR_HOME=$HOME/.reindxr

__find_java()
{
    java_bin=`which java`
    java_bin=${java_bin:=${JAVA_HOME}/bin/java}
    echo $java_bin
}


JAVA_BIN=`__find_java`
DATA_DIR=${DATA_DIR:=${DEFAULT_REINDXR_HOME}/data}
INDEX_DIR=${INDEX_DIR:=${DEFAULT_REINDXR_HOME}/index}
LOGS_DIR=${LOGS_DIR:=${DEFAULT_REINDXR_HOME}/logs}
PIDS_DIR=${PIDS_DIR:=${DEFAULT_REINDXR_HOME}/pids}

mkdir -p $DATA_DIR
mkdir -p $INDEX_DIR
mkdir -p $LOGS_DIR
mkdir -p $PIDS_DIR