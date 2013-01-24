@echo off
rem you may need to modify this
PATH="C:\Program Files\WinRAR";%PATH%

rem rar owns me
rem we use WinRAR.exe to create zip files, to share the joy
rem otherwiese, we'd simpy use rar.exe

echo Building release archive...
WinRAR.exe a -m5 sql2gdb_release.zip ^
  sql2gdb.exe ^
  *.css *.htm ^
  check_here.txt reserved.txt source.txt source_sample.txt ^
  scripthead.sql FreeUDFLib_setup.sql
  
echo Building source archive...
WinRAR.exe a -m5 sql2gdb_source.zip ^
  *.bmp *.pas *.dfm *.dpr *.dof *.bat ^
  UpdateInfoBuilder\*.txt UpdateInfoBuilder\*.pas UpdateInfoBuilder\*.dpr UpdateInfoBuilder\*.dfm ^
  *.css *.htm ^
  check_here.txt reserved.txt source.txt source_sample.txt ^
  scripthead.sql FreeUDFLib_setup.sql

echo Archives built.

echo To release, rename sql2gdb_release.zip to sql2gdb.zip
echo and sql2gdb_source.zip to source.zip

pause