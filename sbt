#!/bin/bash

java $SBT_OPTS -Dfile.encoding=UTF-8 -Xss4M -Xmx1200M -XX:MaxPermSize=512M -XX:NewSize=128M -XX:NewRatio=3 -jar `dirname $0`/project/sbt-launch-0.11.0.jar "$@"
