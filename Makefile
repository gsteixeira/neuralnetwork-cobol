SHELL := /bin/bash

executable := "neuralnet"
source := "neural.cob"

default: neural.cob
	cobc -x --free -O3 -o $(executable) $(source)

run:
	cobc -xj --free -O3 -o $(executable) $(source)

clean:
	rm -f $(executable)
