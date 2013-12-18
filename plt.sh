#!/bin/bash

source_file_path=$1
source_file=`echo $source_file_path | rev | cut -d "/" -f1 | rev`
str_len=${#source_file}
java_out="Main_"${source_file:0:`echo $str_len - 4 | bc`}".java"
clean_directive=$2

if ! [ -e "$source_file_path" ]
then
	echo "Ocaml source file '$source_file_path' does not exist!"
	exit 1
fi

compile_result=`./platoc $source_file_path 2>&1`

if ! [ $? -eq 0 ]
then
	#echo "'$source_file_path' did NOT translate properly"
	echo "TRANSLATION ERROR: $compile_result"
	exit 20
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
	if [ $clean_directive == "clean" ]
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
	java_out_len=${#java_out}
	java ${java_out:0:`echo $java_out_len - 5 | bc`}
fi
if [ $clean_directive == "clean" ]
then
	rm -f $java_out
	rm -f "Main_"${source_file:0:`echo $str_len - 4 | bc`}".class"
fi
exit 0

