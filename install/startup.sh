#!/usr/bin/env bash
kill $(ps aux | grep "/etc/jdk17/bin/java -jar vocabulary-server-core-0.0.1-SNAPSHOT.jar" | head -n1 | awk '{print $2}')
sleep 1
docker rm -f goobi-vocabulary-migration-test
docker run -d --name goobi-vocabulary-migration-test -p 3316:3306 empty:latest
sleep 3
cd ../module-core/target/
/etc/jdk17/bin/java -jar vocabulary-server-core-0.0.1-SNAPSHOT.jar &
sleep 10
../../install/default_setup.sh
