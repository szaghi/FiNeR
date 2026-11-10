program print_prefix
!< Cookbook: print a file into a log, with a prefix.
use finer
implicit none
type(file_ini) :: ini
integer        :: iostat

call ini%load(filename='rod.ini')
print '(A)', 'run started with:'
call ini%print(unit=6, pref='[log] ', iostat=iostat)
endprogram print_prefix
