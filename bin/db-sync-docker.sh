#!/bin/bash
​
use_ssh=false
transfer_dump_file=true
db_name="secretescapes"
user="root"
password=""
host="-h 127.0.0.1"
concurrent=false
​
transfer_dump_file_via_http() {
	curl -o /tmp/secretescapes.tgz "http://dbsync:yD9Q04IxhI^qsF%d*Y@artifactory.secretescapes.com/artifactory/database/secretescapes.tgz"
}
​
transfer_dump_file_via_ssh() {
	echo "Connecting to app01 via SSH"
	scp app01:/data/junk/db-sync/minimal/secretescapes.tgz /tmp/secretescapes.tgz
}
​
transfer_dump_file() {
	if $use_ssh; then
		transfer_dump_file_via_ssh
	else
		transfer_dump_file_via_http
	fi
}
​
prepare()
{
​
	echo "Recreate SE database...."
	echo "drop database if exists $db_name;" | mysql -u $user $password $host
	echo "create database $db_name default character set utf8mb4 default collate utf8mb4_general_ci;" | mysql -u $user $password $host
​
	echo "Importing data....."
}
​
mysqlimport()
{
  script="$1"
​
  echo "Importing $1 ...."
​
  if $transfer_dump_file
  then
    time curl "http://dbsync:yD9Q04IxhI^qsF%d*Y@artifactory.secretescapes.com/artifactory/database/$script.xz" |tee "$script.xz" |xz -d |mysql -u $user $db_name $password $host
  else
    echo "Skipping $script.xz download"
    time xz -dc "$script.xz" |mysql -u $user $db_name $password $host
  fi
}
​
finalise()
{
	cat `dirname $0`/db-sync/config-override.sql | mysql -u $user $db_name $password $host
​
}
​
db_import() {
	echo "Importing data....."
​
	# Recreate SE database.
	echo "drop database if exists $db_name;" | mysql -u $user $password $host
	echo "create database $db_name default character set utf8mb4 default collate utf8mb4_general_ci;" | mysql -u $user $password $host
​
	tar xzOf /tmp/$db_name.tgz baseline.dmp | mysql -u $user $db_name $password $host
	tar xzOf /tmp/$db_name.tgz compact.dmp | mysql -u $user $db_name $password $host
	tar xzOf /tmp/$db_name.tgz offer_translation.sql | mysql -u $user $db_name $password $host
	tar xzOf /tmp/$db_name.tgz sale_translation.sql | mysql -u $user $db_name $password $host
	tar xzOf /tmp/$db_name.tgz allocation.sql | mysql -u $user $db_name $password $host
	tar xzOf /tmp/$db_name.tgz allocation_items.sql | mysql -u $user $db_name $password $host
	tar xzOf /tmp/$db_name.tgz allocation_item.sql | mysql -u $user $db_name $password $host
​
	cat `dirname $0`/db-sync/config-override.sql | mysql -u $user $db_name $password $host
}
​
print_help() {
	echo "-h: show help"
	echo "-w: database server"
	echo "-d: database name"
	echo "-u: username"
	echo "-p: user password"
	echo "-S: download dump file via SSH"
	echo "-s: skip dump file download; just load the database from a previously downloaded dump file"
	echo "-c: concurrent imports (can be faster on some machines)"
}
​
​
while getopts ":chSsd:u:p:w:" opt
do
	case "$opt" in
	h)
		print_help
		exit 0
		;;
	S)
		use_ssh=true
		;;
	s)
		transfer_dump_file=false
		;;
	d)
		db_name="$OPTARG"
		;;
	u)
		user="$OPTARG"
		;;
	p)
		password="-p$OPTARG"
		;;
	w)
		host="-h$OPTARG"
		;;
	c)
		concurrent=true
		;;
	esac
done
​
if $concurrent
then
  prepare
  mysqlimport baseline.dmp  2>&1 |tee baseline.log
​
  { mysqlimport compact.dmp >compact.log 2>&1; } &
  PID1=$!
​
  { mysqlimport offer_translation.sql >offer_translation.log 2>&1; } &
  PID2=$!
​
  { mysqlimport allocation.sql >allocation.log 2>&1; } &
  PID3=$!
​
  { mysqlimport allocation_items.sql >allocation_items.log 2>&1; } &
  PID4=$!
​
  { mysqlimport allocation_item.sql >allocation_item.log 2>&1; } &
  PID5=$!
​
  tail -f *log &
  TAILPID=$!
​
  echo "PID1=$PID1"
  echo "PID2=$PID2"
  echo "PID3=$PID3"
  echo "PID4=$PID4"
  echo "PID5=$PID5"
  echo "TAILPID=$TAILPID"
​
  while ps -fp $PID1 >/dev/null \
  || ps -fp $PID2 >/dev/null \
  || ps -fp $PID3 >/dev/null \
  || ps -fp $PID4 >/dev/null \
  || ps -fp $PID5 >/dev/null
  do
    sleep 0.1
  done
​
  kill $TAILPID
​
  for log in *log
  do
    echo "$log:"
    cat $log
    echo "---------------------------"
  done
​
  finalise
else
  if $transfer_dump_file
  then
    transfer_dump_file
  else
    echo "Skipping dump file download"
  fi
​
  db_import
fi
