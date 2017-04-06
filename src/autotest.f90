!< FiNeR autotest.
program autotest
!< FiNeR autotest.
!<
!<### Usage
!<```bash
!< ./autotest
!<```
use finer, only :  file_ini_autotest

implicit none

print "(A)", 'FiNeR autotest'
call file_ini_autotest
endprogram autotest
