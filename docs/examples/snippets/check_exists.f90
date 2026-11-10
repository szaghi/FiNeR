program check_exists
!< Cookbook: check that a section or an option exists, and find its section.
use finer
implicit none
type(file_ini)    :: ini
character(len=32) :: where

call ini%load(filename='heat.ini')
print '(A,L1)', 'section [time]: ', ini%has_section(section_name='time')
print '(A,L1)', 'section [restart]: ', ini%has_section(section_name='restart')
print '(A,L1)', 'option steps: ', ini%has_option(option_name='steps')
if (ini%has_option(option_name='every', section_name=where)) print '(A)', 'every is in ['//trim(where)//']'
print '(A,L1)', 'steps in [time]: ', ini%index(section_name='time', option_name='steps') > 0
endprogram check_exists
