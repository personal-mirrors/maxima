#!/bin/sh
docker build -t maxima-deb .
docker run cp maxima-deb:/*.deb .
