#!/bin/sh

pretty() {
    echo "javap -l -p -c -s ${1}.class > ${1}.bytecode"
    javap -l -p -c -s ${1}.class > ${1}.bytecode
}

cd hello-world
echo "javac Main.java"
javac Main.java
pretty Main
pretty Invoke

cd ../ssa
echo "javac Main.java"
javac Main.java
pretty Main

build_wordcount() {
    CDH=../../../jvmc-libs/cdh-4.6

    echo
    echo "javac WordCount.java -cp"
    echo "    $CDH/hadoop-annotations.jar"
    echo "    $CDH/hadoop-common.jar"
    echo "    $CDH/hadoop-mapreduce-client-common.jar"
    echo "    $CDH/hadoop-mapreduce-client-core.jar"
    javac WordCount.java -cp \
$CDH/hadoop-annotations.jar:\
$CDH/hadoop-common.jar:\
$CDH/hadoop-mapreduce-client-common.jar:\
$CDH/hadoop-mapreduce-client-core.jar

    pretty WordCount
    pretty WordCount\$IntSumReducer
    pretty WordCount\$TokenizerMapper

    echo
    echo "creating WordCount.jar"
    jar cvfe WordCount.jar WordCount WordCount*.class
}

cd ../wordcount
build_wordcount

cd ../nongeneric-wordcount
build_wordcount
