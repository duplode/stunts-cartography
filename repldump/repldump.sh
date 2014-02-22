#!/bin/sh
RPL_PATH="$1"
cp "$RPL_PATH" STUNTS/tmp-dump.rpl
dosbox -conf repldump.conf
mv STUNTS/TMP-DUMP.BIN ${RPL_PATH%.*}.bin
