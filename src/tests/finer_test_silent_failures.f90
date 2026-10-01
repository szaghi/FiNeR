!< FiNeR test: failures that must be reported rather than silently ignored.
program finer_test_silent_failures
!< Covers: full-line comments (;, #, !) following an option, multi-line continuation over more than two lines,
!<         unsupported value type in get/add, array get into a too small array, count_values of missing option/section.
use finer, only: file_ini
use penf,  only: I4P, R8P
implicit none

type(file_ini)                :: fini
character(len=:), allocatable :: source, val
character(1), parameter       :: markers(3) = [';', '#', '!']
complex(R8P)                  :: zval, zarr(2)
real(R8P)                     :: small(2), exact(5)
integer(I4P)                  :: error
integer                       :: m, passed, total

passed = 0 ; total = 0
print '(A)', 'finer_test_silent_failures'

! full-line comments following an option must not be joined to the option value
do m=1, size(markers)
  call fini%free
  source = '[sec]'//new_line('A')//                             &
           'opt-1 = 1'//new_line('A')//                         &
           markers(m)//' full-line comment'//new_line('A')//    &
           'opt-2 = 2.'//new_line('A')//                        &
           markers(m)//' comment inside a continuation'//new_line('A')// &
           '        3.'//new_line('A')//                        &
           'opt-3 = last'
  call fini%load(source=source)
  val = repeat(' ', 64)
  call fini%get(section_name='sec', option_name='opt-1', val=val, error=error)
  call check('comment ('//markers(m)//') after option: value',       trim(val) == '1')
  val = repeat(' ', 64)
  call fini%get(section_name='sec', option_name='opt-2', val=val, error=error)
  call check('comment ('//markers(m)//') inside continuation: value', trim(val) == '2. 3.')
  val = repeat(' ', 64)
  call fini%get(section_name='sec', option_name='opt-3', val=val, error=error)
  call check('comment ('//markers(m)//') option after comments',      trim(val) == 'last')
enddo

! continuation over more than two lines
call fini%free
source = '[sec]'//new_line('A')//     &
         'arr = 1.'//new_line('A')//  &
         '      2.'//new_line('A')//  &
         '      3.'//new_line('A')//  &
         '      4.'//new_line('A')//  &
         '      5.'//new_line('A')//  &
         'other = 9'
call fini%load(source=source)
call check('long continuation: count == 5',    fini%count_values(section_name='sec', option_name='arr') == 5)
call fini%get(section_name='sec', option_name='arr', val=exact, error=error)
call check('long continuation: no error',      error == 0)
call check('long continuation: last == 5.',    abs(exact(5) - 5._R8P) < 1e-12_R8P)
call check('long continuation: next option',   fini%count_values(section_name='sec', option_name='other') == 1)

! array get into a too small array
small = -1._R8P
call fini%get(section_name='sec', option_name='arr', val=small, error=error)
call check('small array get: error /= 0',      error /= 0)
call check('small array get: val untouched',   all(small == -1._R8P))

! count_values of missing option/section
call check('count_values: missing option',     fini%count_values(section_name='sec', option_name='missing') == 0)
call check('count_values: missing section',    fini%count_values(section_name='missing', option_name='arr') == 0)

! unsupported value type
zval = (-1._R8P, -1._R8P)
call fini%get(section_name='sec', option_name='other', val=zval, error=error)
call check('unsupported get: error /= 0',      error /= 0)
call check('unsupported get: val untouched',   zval == (-1._R8P, -1._R8P))
zarr = (-1._R8P, -1._R8P)
call fini%get(section_name='sec', option_name='arr', val=zarr(1:1), error=error)
call check('unsupported array get: error',     error /= 0)

call fini%add(section_name='sec', option_name='zeta', val=zval, error=error)
call check('unsupported add: error /= 0',      error /= 0)
call check('unsupported add: no option added', .not. fini%has_option(option_name='zeta'))
call fini%add(section_name='sec', option_name='zetas', val=zarr, error=error)
call check('unsupported array add: error',     error /= 0)
call check('unsupported array add: no option', .not. fini%has_option(option_name='zetas'))
call fini%add(section_name='sec', option_name='other', val=zval, error=error)
val = repeat(' ', 64)
call fini%get(section_name='sec', option_name='other', val=val)
call check('unsupported update: error /= 0',   error /= 0)
call check('unsupported update: value kept',   trim(val) == '9')
call fini%add(section_name='sec', option_name='other', val=zarr, error=error)
val = repeat(' ', 64)
call fini%get(section_name='sec', option_name='other', val=val)
call check('unsupported array update: error',  error /= 0)
call check('unsupported array update: kept',   trim(val) == '9')

call fini%add(section_name='sec', option_name='other', val=10_I4P, error=error)
call check('supported update: no error',       error == 0)
call fini%add(section_name='sec', option_name='new-arr', val=[1_I4P, 2_I4P], error=error)
call check('supported array add: no error',    error == 0)
call check('supported array add: count == 2',  fini%count_values(section_name='sec', option_name='new-arr') == 2)

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

end program finer_test_silent_failures
