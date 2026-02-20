!< FiNeR test: get method.
program finer_test_get
!< Covers: string, integer, real, logical, and array get; count_values;
!<         error code for missing option.
use finer, only: file_ini
use penf,  only: I4P, R4P
implicit none

type(file_ini)                :: fini
character(len=:), allocatable :: source, val
integer(I4P)                  :: ival, error
real(R4P)                     :: rval
logical                       :: lval
real(R4P),        allocatable :: arr(:)
integer                       :: passed, total

passed = 0 ; total = 0
print '(A)', 'finer_test_get'

source = '[sec]'//new_line('A')//          &
         'str-opt  = hello'//new_line('A')// &
         'int-opt  = 42'//new_line('A')//    &
         'real-opt = 3.14'//new_line('A')//  &
         'bool-opt = T'//new_line('A')//     &
         'arr-opt  = 1.0 2.0 3.0'

call fini%load(source=source)

val = repeat(' ', 64)
call fini%get(section_name='sec', option_name='str-opt', val=val, error=error)
call check('string get: no error',         error == 0)
call check('string get: value',            trim(val) == 'hello')

call fini%get(section_name='sec', option_name='int-opt', val=ival, error=error)
call check('integer get: no error',        error == 0)
call check('integer get: value',           ival == 42_I4P)

call fini%get(section_name='sec', option_name='real-opt', val=rval, error=error)
call check('real get: no error',           error == 0)
call check('real get: value',              abs(rval - 3.14_R4P) < 1e-4_R4P)

call fini%get(section_name='sec', option_name='bool-opt', val=lval, error=error)
call check('logical get: no error',        error == 0)
call check('logical get: value',           lval)

allocate(arr(1:fini%count_values(section_name='sec', option_name='arr-opt')))
call fini%get(section_name='sec', option_name='arr-opt', val=arr, error=error)
call check('array: count_values == 3',     size(arr) == 3)
call check('array get: no error',          error == 0)
call check('array get: element 1 == 1.0', abs(arr(1) - 1._R4P) < 1e-6_R4P)
call check('array get: element 2 == 2.0', abs(arr(2) - 2._R4P) < 1e-6_R4P)
call check('array get: element 3 == 3.0', abs(arr(3) - 3._R4P) < 1e-6_R4P)

val = repeat(' ', 64)
call fini%get(section_name='sec', option_name='missing', val=val, error=error)
call check('get missing option: error /= 0', error /= 0)

call summary
contains

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

end program finer_test_get
