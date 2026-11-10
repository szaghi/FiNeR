program read_complex
!< Cookbook: read complex numbers.
use finer
use penf, only : R8P
implicit none
type(file_ini)            :: ini
complex(R8P), allocatable :: modes(:)

call ini%load(filename='settings.ini')
allocate(modes(ini%count_values(section_name='lists', option_name='modes')))
call ini%get(section_name='lists', option_name='modes', val=modes)
print '(I0,A)', size(modes), ' modes:'
print '(2F6.2)', modes
endprogram read_complex
!run read_complex read_complex
