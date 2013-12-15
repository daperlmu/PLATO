#!/bin/bash

source_file_path=$1
source_file=`echo $source_file_path | rev | cut -d "/" -f1 | rev`
java_out="main_"${source_file:0:-4}".java"
clean_directive=$2

if ! [ -e "$source_file_path" ]
then
	echo "Ocaml source file '$source_file_path' does not exist!"
	exit 1
fi

compile_result=`./platoc $source_file_path 2>&1`

if ! [ $? -eq 0 ]
then
	echo "'$source_file_path' did NOT translate properly"
	echo "TRANSLATION ERROR: $compile_result"
	exit 1
fi

echo "'$source_file_path' TRANSLATED successfully"

if ! [ -e "$java_out" ]
then
	echo "'$java_out' does not exist! The java code was not generated"
	exit 1
fi

compile_result=`javac $java_out 2>&1`

if ! [ $? -eq 0 ]
then
	echo "'$java_out' did NOT compile"
	echo "COMPILE ERROR: $compile_result"
	if [ $clean_directive="clean" ]
	then
		rm -f $java_out
	fi
	exit 1
fi

echo "'$java_out' COMPILED successfully"
if [ $3 == "run" ]
then
	echo -e "\n"
	echo "RUNNING..."
	java ${java_out:0:-5}
fi
if [ $clean_directive == "clean" ]
then
	rm -f $java_out
	rm -f "main_"${source_file:0:-4}".class"
fi
exit 0

