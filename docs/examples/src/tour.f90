program ini
!< The tour: `ini`, a small command line tool built on FiNeR.
!<
!< ini FILE                          the sections and the number of their options
!< ini FILE SECTION OPTION           the value of an option
!< ini FILE SECTION OPTION VALUE     set the value, and save the file
use finer
use penf, only : I4P
implicit none
type(file_ini)                :: file
character(len=:), allocatable :: arg(:), value
character(len=:), allocatable :: pair(:)
integer(I4P)                  :: error, s, n, i
character(len=256)            :: buffer

allocate(character(len=256) :: arg(command_argument_count()))
do i=1, size(arg)
  call get_command_argument(i, buffer) ; arg(i) = buffer
enddo
main: block
if (size(arg) < 1) then
  print '(A)', 'usage: ini FILE [SECTION OPTION [VALUE]]'
  exit main
endif
call file%load(filename=trim(arg(1)), error=error)
if (error /= 0) then
  print '(A)', 'ini: cannot read '//trim(arg(1))
  exit main
endif
select case(size(arg))
case(1)
  do s=1, file%sections_number()
    n = 0
    do while (file%loop(section_name=file%section(s), option_pairs=pair))
      n = n + 1
    enddo
    print '(A,T16,I0,A)', '['//file%section(s)//']', n, ' options'
  enddo
case(3)
  call file%get_string(section_name=trim(arg(2)), option_name=trim(arg(3)), val=value, error=error)
  if (error == 0) then
    print '(A)', value
  else
    print '(A)', 'ini: no option '//trim(arg(3))//' in ['//trim(arg(2))//']'
  endif
case(4)
  call file%add(section_name=trim(arg(2)), option_name=trim(arg(3)), val=trim(arg(4)))
  call file%save(iostat=error)
  if (error == 0) print '(A)', trim(arg(2))//'.'//trim(arg(3))//' = '//trim(arg(4))//', saved'
endselect
endblock main
endprogram ini
!as ini
!run tour-cat cat case.ini
!run tour-list ini case.ini
!run tour-get ini case.ini time dt
!run tour-set ini case.ini time dt 5e-5
!run tour-check ini case.ini time dt
!run tour-missing ini case.ini time cfl
!cast tour tour-cat tour-list tour-get tour-set tour-check tour-missing
