program read_string
!< Cookbook: read a string of any length.
use finer
implicit none
type(file_ini)                :: ini
character(len=:), allocatable :: title
character(len=8)              :: short

call ini%load(filename='heat.ini')
call ini%get_string(section_name='', option_name='title', val=title)
print '(A,I0,A)', 'title (', len(title), ' characters): '//title
call ini%get(section_name='', option_name='title', val=short)
print '(A)', 'into a character(len=8): "'//short//'"'
endprogram read_string
