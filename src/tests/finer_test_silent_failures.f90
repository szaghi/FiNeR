!< FiNeR test: failures that must be reported rather than silently ignored.
program finer_test_silent_failures
!< Covers: full-line comments (;, #, !) following an option, multi-line continuation over more than two lines,
!<         unsupported value type in get/add, array get into a too small array, count_values of missing option/section,
!<         values that cannot be converted to the requested type.
use finer, only: file_ini
use penf,  only: I4P, I8P, R4P, R8P
implicit none

type :: unsupported
  !< A type not supported as option value.
  integer :: i = -1
endtype unsupported

type(file_ini)                :: fini
character(len=:), allocatable :: source, val
character(1), parameter       :: markers(3) = [';', '#', '!']
type(unsupported)             :: zval, zarr(2)
real(R8P)                     :: small(2), exact(5)
real(R8P)                     :: rval, three(3)
real(R4P)                     :: rval4
integer(I4P)                  :: ival
integer(I8P)                  :: ival8
logical                       :: lval, larr(2)
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
zval = unsupported(-1)
call fini%get(section_name='sec', option_name='other', val=zval, error=error)
call check('unsupported get: error /= 0',      error /= 0)
call check('unsupported get: val untouched',   zval%i == -1)
zarr = unsupported(-1)
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

! values that cannot be converted to the requested type
call fini%free
source = '[vals]'//new_line('A')//           &
         'word  = abc'//new_line('A')//      &
         'float = 1.5'//new_line('A')//      &
         'expo  = 1e3'//new_line('A')//      &
         'dexpo = 1.0d-4'//new_line('A')//   &
         'int   = 42'//new_line('A')//       &
         'big   = 10000000000'//new_line('A')// &
         'slash = /'//new_line('A')//        &
         'star  = 2*3'//new_line('A')//      &
         'yes   = yes'//new_line('A')//      &
         'true  = true'//new_line('A')//     &
         'dotf  = .false.'//new_line('A')//  &
         'mixed = 1. two 3.'//new_line('A')// &
         'bools = T maybe'
call fini%load(source=source)
ival = -7_I4P
call fini%get(section_name='vals', option_name='word', val=ival, error=error)
call check('integer from word: error /= 0',    error /= 0)
call check('integer from word: val untouched', ival == -7_I4P)
call fini%get(section_name='vals', option_name='float', val=ival, error=error)
call check('integer from real: error /= 0',    error /= 0)
call fini%get(section_name='vals', option_name='expo', val=ival, error=error)
call check('integer from 1e3: error /= 0',     error /= 0)
call fini%get(section_name='vals', option_name='big', val=ival, error=error)
call check('integer overflow: error /= 0',     error /= 0)
call fini%get(section_name='vals', option_name='slash', val=ival, error=error)
call check('integer from "/": error /= 0',     error /= 0)
call fini%get(section_name='vals', option_name='star', val=ival, error=error)
call check('integer from "2*3": error /= 0',   error /= 0)
call check('failed integer gets: val untouched', ival == -7_I4P)
call fini%get(section_name='vals', option_name='big', val=ival8, error=error)
call check('big integer into I8P: no error',   error == 0)
call check('big integer into I8P: value',      ival8 == 10000000000_I8P)
rval = -7._R8P
call fini%get(section_name='vals', option_name='word', val=rval, error=error)
call check('real from word: error /= 0',       error /= 0)
call check('real from word: val untouched',    rval == -7._R8P)
call fini%get(section_name='vals', option_name='int', val=rval, error=error)
call check('real from integer: no error',      error == 0)
call check('real from integer: value',         abs(rval - 42._R8P) < 1e-12_R8P)
call fini%get(section_name='vals', option_name='dexpo', val=rval, error=error)
call check('real from 1.0d-4: no error',       error == 0)
call check('real from 1.0d-4: value',          abs(rval - 1.0e-4_R8P) < 1e-16_R8P)
lval = .false.
call fini%get(section_name='vals', option_name='yes', val=lval, error=error)
call check('logical from yes: error /= 0',     error /= 0)
call check('logical from yes: val untouched',  .not. lval)
call fini%get(section_name='vals', option_name='true', val=lval, error=error)
call check('logical from true: no error',      error == 0)
call check('logical from true: value',         lval)
call fini%get(section_name='vals', option_name='dotf', val=lval, error=error)
call check('logical from .false.: no error',   error == 0)
call check('logical from .false.: value',      .not. lval)
three = -7._R8P
call fini%get(section_name='vals', option_name='mixed', val=three, error=error)
call check('array with bad value: error /= 0', error /= 0)
call check('array with bad value: untouched',  all(three == -7._R8P))
larr = .false.
call fini%get(section_name='vals', option_name='bools', val=larr, error=error)
call check('logical array bad value: error',   error /= 0)
call check('logical array bad value: untouched', .not. any(larr))

! values written by add must be read back
call fini%free
call fini%add(section_name='rt', option_name='r8', val=0.1_R8P)
call fini%add(section_name='rt', option_name='r4', val=-32.1_R4P)
call fini%add(section_name='rt', option_name='i4', val=-42_I4P)
call fini%add(section_name='rt', option_name='l', val=.true.)
call fini%add(section_name='rt', option_name='r8s', val=[1._R8P, 2.5_R8P, -3._R8P])
call fini%add(section_name='rt', option_name='ls', val=[.true., .true.])
call fini%get(section_name='rt', option_name='r8', val=rval, error=error)
call check('round trip R8P',                   error == 0 .and. rval == 0.1_R8P)
call fini%get(section_name='rt', option_name='r4', val=rval4, error=error)
call check('round trip R4P',                   error == 0 .and. rval4 == -32.1_R4P)
call fini%get(section_name='rt', option_name='i4', val=ival, error=error)
call check('round trip I4P',                   error == 0 .and. ival == -42_I4P)
lval = .false.
call fini%get(section_name='rt', option_name='l', val=lval, error=error)
call check('round trip logical',               error == 0 .and. lval)
call fini%get(section_name='rt', option_name='r8s', val=three, error=error)
call check('round trip R8P array',             error == 0 .and. all(three == [1._R8P, 2.5_R8P, -3._R8P]))
call fini%get(section_name='rt', option_name='ls', val=larr, error=error)
call check('round trip logical array',         error == 0 .and. all(larr))

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
