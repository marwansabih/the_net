#!/bin/bash

docker run -v /mnt/net1:/save --ulimit nofile=100000:100000 --rm -it thenet -- /save
