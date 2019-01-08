#!/bin/bash
if [[ -z $2 ]] ; then
	echo "Use arguments: $0 YEAR MONTH"
	echo "  Reads files:"
	echo "    data/YEAR/nubank-YEAR-MONTH.csv"
	echo "    data/YEAR/history-YEAR-MONTH.txt"
	echo "  YEAR must be 4-digit."
	echo "  MONTH must be 2-digit."
	exit 1
fi >&2

YEAR=$1
MONTH=$2

INVOICE=data/$YEAR/nubank-$YEAR-$MONTH.csv
HISTORY=data/$YEAR/history-$YEAR-$MONTH.txt

# Command to generate the JAR file:
#   $ sbt package
#
#scala -Dfile.encoding=UTF-8 target/scala-2.12/nubank-merger_2.12-0.0.0.1.jar ${INVOICE} ${HISTORY}

sbt "run ${INVOICE} ${HISTORY}"
