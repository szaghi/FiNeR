program read_list_delimiter
!< Cookbook: read a list with another delimiter.
use finer
use penf, only : I4P
implicit none
type(file_ini)            :: ini
integer(I4P), allocatable :: nodes(:)
character(len=8)          :: labels(3)
integer                   :: i

call ini%load(filename='settings.ini')
allocate(nodes(ini%count_values(section_name='lists', option_name='nodes', delimiter=',')))
call ini%get(section_name='lists', option_name='nodes', val=nodes, delimiter=',')
print '(A,*(1X,I0))', 'nodes:', nodes
call ini%get(section_name='lists', option_name='labels', val=labels)
print '(A,*(1X,A))', 'labels:', (trim(labels(i)), i=1, size(labels))
endprogram read_list_delimiter
!run read_list_delimiter read_list_delimiter
