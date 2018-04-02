#!/bin/bash
if [[ -z $2 ]] ; then
	echo "Use arguments: $0 INVOICE HISTORY"
	exit 1
fi

INVOICE=$(cygpath -w $1)
HISTORY=$(cygpath -w $2)

# Command to generate the JAR file:
#   $ sbt package
#
scala target/scala-2.12/nubank-merger_2.12-0.0.0.1.jar $INVOICE $HISTORY

#sbt "run $INVOICE $HISTORY"
