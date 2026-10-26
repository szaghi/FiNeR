!< FiNeR test: CRLF (Windows) line endings.
program finer_test_crlf
!< Covers: a source with CR+LF line endings, loaded from a string and from a file, gives the same values as with LF
!<         endings: numbers, logicals and strings, the last option of a section and the last line of the file included.
!<         Whether a compiler's formatted read keeps the CR at the end of a record is processor dependent (gfortran
!<         drops it, nvfortran keeps it); a source string always keeps it.
use finer, only: file_ini
use penf,  only: I4P, R8P
implicit none

character(len=*), parameter   :: CRLF = achar(13)//achar(10)
character(len=*), parameter   :: FILENAME = 'finer_test_crlf.tmp.ini'
character(len=:), allocatable :: source
type(file_ini)                :: fini
integer                       :: error, passed, total, unit

passed = 0 ; total = 0
print '(A)', 'finer_test_crlf'

source = '[numbers]'//CRLF//'n = 3'//CRLF//'x = 2.5'//CRLF// &
         '[flags]'//CRLF//'on = .true.'//CRLF//'name = Neumann'//CRLF
call fini%load(source=source, error=error)
call check('source: no error', error == 0)
call check_values('source')

open(newunit=unit, file=FILENAME, access='stream', form='unformatted', status='replace')
write(unit) source
close(unit)
call fini%free
call fini%load(filename=FILENAME, error=error)
call check('file: no error', error == 0)
call check_values('file')
open(newunit=unit, file=FILENAME)
close(unit, status='delete')

call summary
contains

  subroutine check_values(label)
  !< Check every option of the CRLF source.
  character(*), intent(in)      :: label   !< Which load.
  integer(I4P)                  :: n       !< Integer option.
  real(R8P)                     :: x       !< Real option.
  logical                       :: on      !< Logical option.
  character(len=:), allocatable :: name    !< String option.

  n = -1 ; x = -1._R8P ; on = .false. ; name = repeat(' ', 32)
  call fini%get(section_name='numbers', option_name='n', val=n, error=error)
  call check(label//': integer, no error', error == 0)
  call check(label//': integer == 3', n == 3_I4P)
  call fini%get(section_name='numbers', option_name='x', val=x, error=error)
  call check(label//': real, no error', error == 0)
  call check(label//': real == 2.5', abs(x - 2.5_R8P) < 1e-12_R8P)
  call fini%get(section_name='flags', option_name='on', val=on, error=error)
  call check(label//': logical, no error', error == 0)
  call check(label//': logical is true', on)
  call fini%get(section_name='flags', option_name='name', val=name, error=error)
  call check(label//': string, no error', error == 0)
  call check(label//': string == "Neumann", no CR', trim(name) == 'Neumann')
  end subroutine check_values

  subroutine check(label, ok)
  character(*), intent(in) :: label
  logical,      intent(in) :: ok
  total = total + 1
  if (ok) passed = passed + 1
  if (ok) then
    write(*, '("  [PASS] ", A)') label
  else
    write(*, '("  [FAIL] ", A)') label
  end if
  end subroutine check

  subroutine summary
  write(*, '(/, "--- ", I0, "/", I0, " passed")') passed, total
  write(*, '(A, L1)') 'Are all tests passed? ', passed == total
  if (passed /= total) stop 1
  end subroutine summary

end program finer_test_crlf
