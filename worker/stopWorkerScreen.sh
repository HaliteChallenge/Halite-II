#!/bin/bash

pkill screen
docker kill $(docker ps -aq)
docker rm $(docker ps -aq)
