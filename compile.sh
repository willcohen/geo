#!/usr/bin/env bash


$JAVA_HOME/bin/native-image \
    -jar target/geo-3.0.1-standalone.jar \
    -H:Name=geo.graal \
    -H:+ReportExceptionStackTraces \
    -H:+TraceClassInitialization \
    --initialize-at-build-time \
    --report-unsupported-elements-at-runtime \
    --verbose \
    --no-fallback \
    --no-server
