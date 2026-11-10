program subsections
!< Cookbook: subsections as dotted names.
use finer
implicit none
type(file_ini)                :: ini
character(len=:), allocatable :: names(:)
integer                       :: s

call ini%load(source='[solver]'//new_line('a')//'name = heat'//new_line('a')// &
                     '[solver.linear]'//new_line('a')//'tolerance = 1e-10'//new_line('a')// &
                     '[solver.linear.preconditioner]'//new_line('a')//'type = ilu')
call ini%get_sections_list(names)
do s=1, size(names)
  if (index(names(s), 'solver.linear') == 1) print '(A)', 'under solver.linear: '//trim(names(s))
enddo
endprogram subsections
!run subsections subsections
