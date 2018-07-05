#!/bin/bash
if [[ -z $2 ]] ; then
	echo "Use arguments: $0 INVOICE.csv HISTORY.txt"
	exit 1
fi

INVOICE=$1
HISTORY=$2

if which cygpath>/dev/null ; then
    INVOICE=$(cygpath -w ${INVOICE})
    HISTORY=$(cygpath -w ${HISTORY})
fi

# Command to generate the JAR file:
#   $ sbt package
#
#scala -Dfile.encoding=UTF-8 target/scala-2.12/nubank-merger_2.12-0.0.0.1.jar ${INVOICE} ${HISTORY}

sbt "run ${INVOICE} ${HISTORY}"
