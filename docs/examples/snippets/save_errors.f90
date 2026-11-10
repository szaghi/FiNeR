program save_errors
!< Cookbook: check that a file was saved.
use finer
use penf, only : I4P
implicit none
type(file_ini)     :: ini
integer(I4P)       :: iostat
character(len=128) :: iomsg

call ini%add(section_name='run', option_name='steps', val=10_I4P)
call ini%save(filename='out/run.ini', iostat=iostat, iomsg=iomsg)
if (iostat /= 0) print '(A)', 'out/run.ini not saved (the directory does not exist)'
call ini%save(filename='run.ini', iostat=iostat)
if (iostat == 0) print '(A)', 'run.ini saved'
endprogram save_errors
