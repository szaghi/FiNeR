program read_logical
!< Cookbook: read a logical.
use finer
use penf, only : I4P
implicit none
type(file_ini)  :: ini
logical         :: flag
integer(I4P)    :: error
character(len=1) :: name
integer         :: i

call ini%load(filename='settings.ini')
do i=1, 7
  name = achar(iachar('a') + i - 1)
  flag = .false.
  call ini%get(section_name='flags', option_name=name, val=flag, error=error)
  if (error == 0) then
    print '(A,L1)', name//' = ', flag
  else
    print '(A,I0)', name//': not a logical, error ', error
  endif
enddo
endprogram read_logical
