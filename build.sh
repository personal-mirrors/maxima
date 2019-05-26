#!/bin/sh
docker build -t maxima-deb .
docker run cp maxima-deb:/*.deb . && docker stop maxima-deb  && docker container rm maxima-deb

