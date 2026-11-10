program list_sections
!< Cookbook: list the sections.
use finer
implicit none
type(file_ini)                :: ini
character(len=:), allocatable :: names(:)
integer                       :: s

call ini%load(filename='heat.ini')
call ini%get_sections_list(names)
do s=1, size(names)
  if (len_trim(names(s)) == 0) then
    print '(I0,A)', s, ': (the global section)'
  else
    print '(I0,A)', s, ': ['//trim(names(s))//']'
  endif
enddo
endprogram list_sections
