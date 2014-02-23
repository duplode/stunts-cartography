@echo off
if not -%1-==-- (
  copy /y %1 STUNTS\tmp-dump.rpl
  "C:\Program Files\DOSBox-0.74\DOSBox.exe" -conf repldump.conf
  move /y STUNTS\tmp-dump.bin .
  for %%f in (%1) do move /y tmp-dump.bin %%~nf.bin
)
