#!/bin/bash

export RELX_REPLACE_OS_VARS=true

for i in `seq 1 2`;
do
    NODE_NAME=node$i PORT=808$i _build/default/rel/tbook/bin/tbook foreground &
    sleep 1
done