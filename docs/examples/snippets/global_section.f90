program global_section
!< Cookbook: options outside any section.
use finer
use penf, only : I4P
implicit none
type(file_ini) :: ini
integer(I4P)   :: version

call ini%load(source='version = 2'//new_line('a')//'[grid]'//new_line('a')//'cells = 64')
call ini%get(section_name='', option_name='version', val=version)
print '(A,I0,A,A,A)', 'version = ', version, ', section 1 is "', ini%section(1), '"'
call ini%add(section_name='', option_name='author', val='the lab')
call ini%print(unit=6)
endprogram global_section
