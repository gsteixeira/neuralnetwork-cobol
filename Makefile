SHELL := /bin/bash

default: neural.cob
	cobc -x --free -O3 -o neural neural.cob

run:
	cobc -xj --free -O3 -o neural neural.cob

clean:
	rm -f neural
