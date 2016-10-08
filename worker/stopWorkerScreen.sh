#!/bin/bash

pkill screen
docker stop $(docker ps -aq)
docker rm $(docker ps -aq)
