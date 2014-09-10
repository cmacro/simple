@echo off
echo erasing...

rem ren ".\Release\Bin\workbench.cfg" "workbench.dat"

del /S *.local *.dcu *.ddp *.dsk *.~* *.dsm *.identcache *.stat *humbs.db
del /S *.map
del ??history* /S /a
rem del __history /S /a


rem del *.cfg *.config 

rem ren ".\Release\Bin\workbench.dat" "workbench.cfg"



:LEAVE
