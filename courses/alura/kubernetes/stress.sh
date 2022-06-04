#!/bin/bash

SLEEP_TIME=$1

function stress_test() {
    for i in {1..10000}; do
	curl -s 192.168.59.101:30000
	sleep $SLEEP_TIME
    done
}

stress_test & stress_test
