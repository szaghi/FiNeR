program parse_file
!< Cookbook: parse a file, and stop on errors.
use finer
use penf, only : I4P
implicit none
type(file_ini) :: ini
integer(I4P)   :: error

call ini%load(filename='heat.ini', error=error)
if (error /= 0) stop 'cannot load heat.ini'
print '(A,I0,A)', 'heat.ini: ', ini%sections_number(), ' sections'
call ini%load(filename='missing.ini', error=error)
if (error /= 0) print '(A)', 'missing.ini: cannot load it'
endprogram parse_file
