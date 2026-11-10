program parse_string
!< Cookbook: parse a string.
use finer
use penf, only : I4P
implicit none
type(file_ini) :: ini
integer(I4P)   :: cells

call ini%load(source='[grid]'//new_line('a')//'cells = 64')
call ini%get(section_name='grid', option_name='cells', val=cells)
print '(A,I0)', 'cells = ', cells
endprogram parse_string
!run parse_string parse_string
