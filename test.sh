#!/bin/bash

function run_test() {
	entry=$1
	
	entry2=`basename $entry`
	echo "$entry2"
	
	./obj/main $entry
	
	gcc $entry -o /tmp/ctest/$entry2
	gcc output.c -o /tmp/ctest/output
	
	diff <(/tmp/ctest/$entry2) <(/tmp/ctest/output)

	if [[ $? == 0 ]] ;then
		/tmp/ctest/$entry2 > /dev/null
		ret1=$?
		
		/tmp/ctest/output > /dev/null
		ret2=$?
		
		if [[ $ret1 == $ret2 ]] ; then
			echo "Pass"
		else
			echo "Fail on return values:"
			echo "Expected: $ret1"
			echo "Actual: $ret2"
			exit 1
		fi
	else
		echo "Fail"
		echo ""
		echo "Expected: "
		echo "`/tmp/ctest/$entry2`"
		echo "Actual:"
		echo "`$B_PATH/out.bin`"
		exit 1
	fi
	
	echo ""
}

echo "Testing..."
echo ""

if [ ! -d /tmp/ctest ] ; then
	mkdir -p /tmp/ctest
fi

echo ""
echo "Running tests..."
echo "====================================="
for entry in "./test"/*.c
do
	run_test $entry
done

echo "Done"
