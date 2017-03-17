ALS_FILE=$1
CMD=$2
XML_DIR=$3
OS=$4

if [ -z "$JAVA_HEAP_SIZE" ]
then
  echo Heap size not set
else
    SET_MAX_HEAP_SIZE="-Xmx$JAVA_HEAP_SIZE"
fi

if [ -z "$SOLVER" ]
then
    echo "Environment variable 'SOLVER' not set -- defaulting to 'sat4j'."
    SOLVER="sat4j"
else
    case $SOLVER in
	sat4j|cryptominisat|glucose|plingeling|lingeling|minisatprover|minisat);;
	*) exit 1;;
    esac
fi

export PATH=`pwd`/$OS:$PATH

java \
    $SET_MAX_HEAP_SIZE                                        \
    -Djava.library.path="`pwd`/$OS"                           \
    -Dout=$XML_DIR    `# output to XML_DIR/test_<NUMBER>.xml` \
    -Dsolver=$SOLVER  `# using given solver`                  \
    -Dcmd=$CMD        `# run nth command in file`             \
    edu/mit/csail/sdg/alloy4whole/RunAlloy                    \
    $ALS_FILE > $XML_DIR/alloy.out \
    || echo "No instance found" >> $XML_DIR/result
