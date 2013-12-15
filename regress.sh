#!/bin/bash

tests_dir=$1
failure_count=0
pass_count=0
declare -a regressions
declare -a passes
for source_file in `ls -L $tests_dir/*.plt | xargs -n1 basename`
do
	echo "-------------TESTING '$source_file'-----------------"
	echo ''

	source_file_path=$tests_dir"/"$source_file
	str_len=${#source_file}
	java_out="Main_"${source_file:0:`echo $str_len - 4 | bc`}".java"
	./plt.sh $source_file_path dirty no_run
	if ! [ $? -eq 0 ]
	then
		regressions[$failure_count]=$source_file
		((failure_count++))
		echo "FAILURE $failure_count: '$source_file' failed compilation!"
		echo "'$source_file' FAILED regression test!"
		if [ -e $java_out ]
		then
			rm -f $java_out
		fi
		if [ -e "Main_"${source_file:0:`echo $str_len - 4 | bc`}".class" ]
		then
			rm -f "Main_"${source_file:0:`echo $str_len - 4 | bc`}".class"
		fi
		echo -e '\n-------------END-------------------------------'
		echo ''
		continue
	fi

	src_path_len=${#source_file_path}
	expected_out=${source_file_path:0:`echo $src_path_len - 4 | bc`}".result"
	if ! [ -e "$expected_out" ]
	then
		regressions[$failure_count]=$source_file
		((failure_count++))
		echo "FAILURE $failure_count: '$expected_out' does not exist! Could not find an expected results file to compare against!"
		echo "'$source_file' FAILED regression test!"
		rm -f $java_out
		rm -f "Main_"${source_file:0:`echo $str_len - 4 | bc`}".class"
		echo -e '\n-------------END-------------------------------'
		echo ''
		continue
	fi

	java_out_len=${#java_out}
	java ${java_out:0:`echo $java_out_len - 5 | bc`} > ${java_out:0:`echo $java_out_len - 5 | bc`}.actual.result

	diff_result=`diff ${java_out:0:`echo $java_out_len - 5 | bc`}.actual.result $expected_out`

	if ! [ $? -eq 0 ]
	then
		regressions[$failure_count]=$source_file
		((failure_count++))
		echo "FAILURE $failure_count: Expected output does NOT match actual output!"
		echo "'$source_file' FAILED regression test!"
		rm -f $java_out
		rm -f "Main_"${source_file:0:`echo $str_len - 4 | bc`}".class"
		echo -e '\n-------------END-------------------------------'
		echo ''
		continue
	fi

	echo "'$source_file' PASSED regression test!"
	rm -f $java_out
	rm -f "Main_"${source_file:0:`echo $str_len - 4 | bc`}".class"
	echo -e '\n-------------END-------------------------------'
	echo ''
	passes[$pass_count]=$source_file
	((pass_count++))
done

if [ $failure_count -eq 0 ]
then
	echo -e "\n"
	echo "-------------------------"
	echo "ALL PASSED"
	echo "0 FAILURES, $pass_count PASSES"
	echo "-------------------------"
	exit
fi

echo -e "\n"
echo "-------------------------"
echo "$failure_count FAILURES"
echo "-------------------------"

index=0
for failure in "${regressions[@]}"
do
	((index++))
	echo "FAILURE $index: $failure"
done

echo "-------------------------"
echo "$pass_count PASSES"
echo "-------------------------"

for pass in "${passes[@]}"
do
	echo "$pass"
done

