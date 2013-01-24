/* To run the script, make sure that the isql.exe is on the
 *  command line. If you also have MS SQL 7 on your system,
 *  you might invoke the wrong isql! To make sure you call
 *  the correct isql, modify the path with the following
 *  command:
 *
 * path=c:\progra~1\borland\interbase\bin;%path%
 *
 * Use the correct directory for the InterBase folder! This
 *  sample is just the default.
 *
 * At last, to invoke the script, use the following syntax:
 *
 * isql -i script.sql
 *
 * TODO: Customize the following command before running
 *  this script:
 */
CREATE DATABASE 'C:\gdb.gdb'
  USER 'sysdba' PASSWORD 'masterkey';
