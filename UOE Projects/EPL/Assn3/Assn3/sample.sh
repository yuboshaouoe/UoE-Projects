#!/bin/sh
rm -f RunRabbit.scala
scala -cp Assn3Solution.jar:lib/com.sksamuel.scrimage.scrimage-core-4.0.32.jar:lib/com.drewnoakes.metadata-extractor-2.18.0.jar Assignment3.RabbitStandalone.Assignment3Standalone -s -o $2 $1
scala -cp Assn3Solution.jar:lib/com.sksamuel.scrimage.scrimage-core-4.0.32.jar:lib/com.drewnoakes.metadata-extractor-2.18.0.jar < RunRabbit.scala 
