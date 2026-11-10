program reload_file
!< Cookbook: remember the file name, save and load without passing it.
use finer
use penf, only : I4P
implicit none
type(file_ini) :: ini
integer(I4P)   :: steps, error

call ini%initialize(filename='counter.ini')
call ini%add(section_name='run', option_name='steps', val=10_I4P)
call ini%save()                                  ! into ini%filename
call ini%free()
ini%filename = 'counter.ini'
call ini%load(error=error)                       ! from ini%filename
call ini%get(section_name='run', option_name='steps', val=steps)
print '(A,A,A,I0,A,I0)', 'file ', ini%filename, ': steps = ', steps, ', error ', error
endprogram reload_file
