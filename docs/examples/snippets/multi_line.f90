program multi_line
!< Cookbook: values on several lines.
use finer
use penf, only : R8P
implicit none
type(file_ini)                :: ini
real(R8P), allocatable        :: table(:)
character(len=:), allocatable :: text

call ini%load(source='[data]'//new_line('a')// &
                     'table = 1.0 2.0'//new_line('a')// &
                     '        3.0 4.0'//new_line('a')// &
                     '; a comment line does not break the value'//new_line('a')// &
                     '        5.0'//new_line('a')// &
                     'note  = first line'//new_line('a')// &
                     '        second line')
allocate(table(ini%count_values(section_name='data', option_name='table')))
call ini%get(section_name='data', option_name='table', val=table)
print '(I0,A,*(F4.1))', size(table), ' values:', table
call ini%get_string(section_name='data', option_name='note', val=text)
print '(A)', 'note = "'//text//'"'
endprogram multi_line
