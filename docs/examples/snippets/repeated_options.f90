program repeated_options
!< Cookbook: repeated options, the first one wins.
use finer
use penf, only : I4P
implicit none
type(file_ini)                :: ini
integer(I4P)                  :: level
character(len=:), allocatable :: pair(:)

call ini%load(filename='settings.ini')
call ini%get(section_name='repeated', option_name='level', val=level)
print '(A,I0)', 'get: level = ', level
print '(A,I0,A,I0)', 'index: first ', ini%index(section_name='repeated', option_name='level'), &
                     ', last ', ini%index(section_name='repeated', option_name='level', back=.true.)
do while (ini%loop(section_name='repeated', option_pairs=pair))
  print '(A)', '  '//trim(pair(1))//' = '//trim(pair(2))
enddo
endprogram repeated_options
