program find_typos
!< Cookbook: after reading the settings, list the options nobody asked for (a typo, a removed key).
use finer
use penf, only : I4P, R8P
implicit none
type(file_ini)                :: ini
character(len=:), allocatable :: unused(:,:)
integer(I4P)                  :: error, cells, u
real(R8P)                     :: cfl

call ini%load(source='[grid]'//new_line('a')//'cells = 64'//new_line('a')// &
                     '[time]'//new_line('a')//'CLF = 0.5'//new_line('a')//'cfl = 0.8')
call ini%get(section_name='grid', option_name='cells', val=cells, error=error)
call ini%get(section_name='time', option_name='cfl', val=cfl, error=error)
call ini%unused_options(items=unused)
do u=1, size(unused, dim=1)
  print '(A)', '['//trim(unused(u, 1))//'] '//trim(unused(u, 2))//': never read'
enddo
print '(I0,A)', size(unused, dim=1), ' unused options'
endprogram find_typos
!run find_typos find_typos
