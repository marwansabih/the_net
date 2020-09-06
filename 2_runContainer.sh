#!/bin/bash

docker run --ulimit nofile=100000:100000 --rm -it thenet -- save
