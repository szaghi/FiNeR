!< FiNeR test: get_string method and default values of get.
program finer_test_get_string_default
!< Covers: get_string into an unallocated/allocated deferred length string, get_string errors and default,
!<         get with default (scalar and array) for missing section, missing option and not convertible value,
!<         default of a different (compatible or not) type.
use finer, only: file_ini
use penf,  only: I4P, I8P, R4P, R8P
implicit none

type(file_ini)                :: fini
character(len=:), allocatable :: source, sval, unalloc
character(8)                  :: cval
real(R8P)                     :: rval, rarr(3)
real(R4P)                     :: rval4
integer(I4P)                  :: ival, iarr(2)
logical                       :: lval
integer(I4P)                  :: error
integer                       :: passed, total

passed = 0 ; total = 0
print '(A)', 'finer_test_get_string_default'

source = '[workspace]'//new_line('A')//                                  &
         'id    = test_1207  ; title of the analysis'//new_line('A')//   &
         'long  = a rather long value with spaces'//new_line('A')//      &
         'empty ='//new_line('A')//                                      &
         'num   = 42'//new_line('A')//                                   &
         'word  = abc'//new_line('A')//                                  &
         'arr   = 1. 2. 3.'//new_line('A')//                             &
         'bad   = 1. two 3.'
call fini%load(source=source)

! get_string
call fini%get_string(section_name='workspace', option_name='id', val=unalloc, error=error)
call check('get_string unallocated: no error',   error == 0)
call check('get_string unallocated: allocated',  allocated(unalloc))
if (allocated(unalloc)) then
  call check('get_string unallocated: value',    unalloc == 'test_1207')
  call check('get_string unallocated: length',   len(unalloc) == 9)
endif
sval = 'x'
call fini%get_string(section_name='workspace', option_name='long', val=sval, error=error)
call check('get_string reallocates: value',      sval == 'a rather long value with spaces')
call check('get_string reallocates: length',     len(sval) == 31)
call fini%get_string(section_name='workspace', option_name='id', val=sval)
call check('get_string shrinks: length',         len(sval) == 9)

sval = 'keep'
call fini%get_string(section_name='workspace', option_name='missing', val=sval, error=error)
call check('get_string missing option: error',   error /= 0)
call check('get_string missing option: kept',    sval == 'keep')
call fini%get_string(section_name='nowhere', option_name='id', val=sval, error=error)
call check('get_string missing section: error',  error /= 0)
call check('get_string missing section: kept',   sval == 'keep')
call fini%get_string(section_name='workspace', option_name='empty', val=sval, error=error)
call check('get_string empty value: error',      error /= 0)
call fini%get_string(section_name='workspace', option_name='missing', val=sval, error=error, default='fallback')
call check('get_string default: error /= 0',     error /= 0)
call check('get_string default: value',          sval == 'fallback')
call fini%get_string(section_name='workspace', option_name='id', val=sval, error=error, default='fallback')
call check('get_string default unused: value',   error == 0 .and. sval == 'test_1207')

! scalar get with default
rval = -7._R8P
call fini%get(section_name='workspace', option_name='num', val=rval, error=error, default=-1._R8P)
call check('default unused: no error',           error == 0)
call check('default unused: value',              abs(rval - 42._R8P) < 1e-12_R8P)
call fini%get(section_name='workspace', option_name='missing', val=rval, error=error, default=-1._R8P)
call check('default missing option: error',      error /= 0)
call check('default missing option: value',      rval == -1._R8P)
rval = -7._R8P
call fini%get(section_name='nowhere', option_name='num', val=rval, error=error, default=-2._R8P)
call check('default missing section: error',     error /= 0)
call check('default missing section: value',     rval == -2._R8P)
call fini%get(section_name='workspace', option_name='word', val=rval, error=error, default=-3._R8P)
call check('default bad value: error',           error /= 0)
call check('default bad value: value',           rval == -3._R8P)
call fini%get(section_name='workspace', option_name='missing', val=rval, default=-4._R8P)
call check('default without error argument',     rval == -4._R8P)

! default of a different type
call fini%get(section_name='workspace', option_name='missing', val=rval, default=-5)
call check('real val, integer default',          rval == -5._R8P)
call fini%get(section_name='workspace', option_name='missing', val=rval, default=0.5_R4P)
call check('R8P val, R4P default',               rval == 0.5_R8P)
call fini%get(section_name='workspace', option_name='missing', val=rval4, default=0.25_R8P)
call check('R4P val, R8P default',               rval4 == 0.25_R4P)
call fini%get(section_name='workspace', option_name='missing', val=ival, default=7_I8P)
call check('I4P val, I8P default',               ival == 7_I4P)
ival = -7_I4P
call fini%get(section_name='workspace', option_name='missing', val=ival, error=error, default=10000000000_I8P)
call check('I4P val, too big default: error',    error /= 0)
call check('I4P val, too big default: kept',     ival == -7_I4P)
call fini%get(section_name='workspace', option_name='missing', val=ival, error=error, default=1.5_R8P)
call check('integer val, real default: kept',    error /= 0 .and. ival == -7_I4P)
call fini%get(section_name='workspace', option_name='missing', val=ival, error=error, default='text')
call check('integer val, string default: kept',  error /= 0 .and. ival == -7_I4P)
lval = .false.
call fini%get(section_name='workspace', option_name='missing', val=lval, default=.true.)
call check('logical default',                    lval)
cval = 'keep'
call fini%get(section_name='workspace', option_name='missing', val=cval, default='dflt')
call check('character default',                  cval == 'dflt')

! array get with default
rarr = -7._R8P
call fini%get(section_name='workspace', option_name='arr', val=rarr, error=error, default=[0._R8P, 0._R8P, 0._R8P])
call check('array default unused',               error == 0 .and. all(rarr == [1._R8P, 2._R8P, 3._R8P]))
call fini%get(section_name='workspace', option_name='missing', val=rarr, error=error, default=[7._R8P, 8._R8P, 9._R8P])
call check('array default missing: error',       error /= 0)
call check('array default missing: value',       all(rarr == [7._R8P, 8._R8P, 9._R8P]))
call fini%get(section_name='workspace', option_name='bad', val=rarr, error=error, default=[4, 5, 6])
call check('array default bad value: value',     error /= 0 .and. all(rarr == [4._R8P, 5._R8P, 6._R8P]))
iarr = -7_I4P
call fini%get(section_name='workspace', option_name='arr', val=iarr, error=error, default=[1_I4P, 2_I4P])
call check('array default too small val: value', error /= 0 .and. all(iarr == [1_I4P, 2_I4P]))
rarr = -7._R8P
call fini%get(section_name='workspace', option_name='missing', val=rarr, error=error, default=[1._R8P, 2._R8P])
call check('array default wrong size: kept',     error /= 0 .and. all(rarr == -7._R8P))

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

end program finer_test_get_string_default
