program loop_options
!< Cookbook: loop over the options of a section, or of the whole file.
use finer
implicit none
type(file_ini)                :: ini
character(len=:), allocatable :: pair(:)
integer                       :: n

call ini%load(filename='heat.ini')
do while (ini%loop(section_name='time', option_pairs=pair))
  print '(A)', trim(pair(1))//' = '//trim(pair(2))
enddo
n = 0
do while (ini%loop(option_pairs=pair))
  n = n + 1
enddo
print '(A,I0,A)', 'the file has ', n, ' options'
endprogram loop_options
!run loop_options loop_options
