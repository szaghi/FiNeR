program custom_separator
!< Cookbook: read a file that uses another separator.
use finer
use penf, only : I4P
implicit none
type(file_ini) :: ini
integer(I4P)   :: port

call ini%load(filename='colon.cfg', separator=':')
call ini%get(section_name='server', option_name='port', val=port)
print '(A,I0)', 'port = ', port
call ini%print(unit=6)
endprogram custom_separator
