#!/bin/bash
jar_file="../microc-compiler.jar"
in_folder="./in"
ref_folder="./ref"
temp_file="temp"

success_cnt=0
fail_cnt=0

for f in $in_folder/*.microc; do
	file=$(basename $f)
	filename="${file%.*}"

	# unoptimized
	java -jar $jar_file $in_folder/$file > $temp_file.asm
	nasm -felf64 $temp_file.asm && ld --dynamic-linker /lib/x86_64-linux-gnu/ld-linux-x86-64.so.2 -lc -o $temp_file $temp_file.o && ./$temp_file > $temp_file.out
	diff $temp_file.out $ref_folder/$filename.ref 2>&1
	error=$?
	if [ $error -eq 0 ]
	then
		success_cnt=$((success_cnt+1))
	else
		fail_cnt=$((fail_cnt+1))
		echo "Unoptimized test failed for $filename"
	fi

	# optimized
	java -jar $jar_file $in_folder/$file -o > $temp_file.asm
	nasm -felf64 $temp_file.asm && ld --dynamic-linker /lib/x86_64-linux-gnu/ld-linux-x86-64.so.2 -lc -o $temp_file $temp_file.o && ./$temp_file > $temp_file.out
	diff $temp_file.out $ref_folder/$filename.ref 2>&1
	error=$?
	if [ $error -eq 0 ]
	then
		success_cnt=$((success_cnt+1))
	else
		fail_cnt=$((fail_cnt+1))
		echo "Optimized test failed for $filename"
	fi
done

rm -f $temp_file*
echo "Tests: $(($success_cnt+fail_cnt)), Passed:$success_cnt, Failed:$fail_cnt"

if [ $fail_cnt -eq 0 ]
then
	exit 0
else
	exit 1
fi
