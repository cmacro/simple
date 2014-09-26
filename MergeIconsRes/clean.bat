@echo off
echo erasing...


del /S *.local *.dcu *.ddp *.dsk *.~* *.dsm *.identcache *.stat *humbs.db
del /S *.map
del ??history* /S /a

del *.cfg *.config 



:LEAVE
