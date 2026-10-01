!< FiNeR test: complex values.
program finer_test_complex
!< Covers: get of complex values in the Fortran notation (scalar and array), malformed complex values, add of complex
!<         values with exact read back, count_values of complex lists, default values for complex.
use finer, only: file_ini
use penf,  only: I4P, R4P, R8P
implicit none

type(file_ini)                :: fini
character(len=:), allocatable :: source, sval
complex(R8P)                  :: z, zarr(2), zback(2), three(3)
complex(R4P)                  :: z4
real(R8P)                     :: rval
integer(I4P)                  :: error
integer                       :: passed, total

passed = 0 ; total = 0
print '(A)', 'finer_test_complex'

source = '[cylinder]'//new_line('A')//                                   &
         'epsilon = (80., 1.0d-4) ; water permittivity'//new_line('A')//  &
         'tight   = (1.5,-2.5)'//new_line('A')//                         &
         'spaced  =   (  -1 ,  2e3 )  '//new_line('A')//                 &
         'list    = (1.0, 2.0) (3.0, 4.0)'//new_line('A')//              &
         'commas  = (1.0,2.0), (3.0,4.0), (5.0,6.0)'//new_line('A')//    &
         'tuple   = 80. 1.0d-4'//new_line('A')//                         &
         'noclose = (1.0, 2.0'//new_line('A')//                          &
         'three   = (1.0, 2.0, 3.0)'//new_line('A')//                    &
         'word    = (a, b)'//new_line('A')//                             &
         'empty   = (,)'//new_line('A')//                                &
         'real    = 3.5'
call fini%load(source=source)

! scalar get
call fini%get(section_name='cylinder', option_name='epsilon', val=z, error=error)
call check('scalar: no error',            error == 0)
call check('scalar: value',               z == (80._R8P, 1.0e-4_R8P))
call fini%get(section_name='cylinder', option_name='tight', val=z, error=error)
call check('no blanks: value',            error == 0 .and. z == (1.5_R8P, -2.5_R8P))
call fini%get(section_name='cylinder', option_name='spaced', val=z, error=error)
call check('blanks: value',               error == 0 .and. z == (-1._R8P, 2000._R8P))
call fini%get(section_name='cylinder', option_name='tight', val=z4, error=error)
call check('R4P: value',                  error == 0 .and. z4 == (1.5_R4P, -2.5_R4P))

! malformed values
z = (-7._R8P, -7._R8P)
call fini%get(section_name='cylinder', option_name='tuple', val=z, error=error)
call check('tuple without parentheses',   error /= 0)
call fini%get(section_name='cylinder', option_name='noclose', val=z, error=error)
call check('missing parenthesis',         error /= 0)
call fini%get(section_name='cylinder', option_name='three', val=z, error=error)
call check('three parts',                 error /= 0)
call fini%get(section_name='cylinder', option_name='word', val=z, error=error)
call check('not numeric parts',           error /= 0)
call fini%get(section_name='cylinder', option_name='empty', val=z, error=error)
call check('empty parts',                 error /= 0)
call fini%get(section_name='cylinder', option_name='real', val=z, error=error)
call check('real value',                  error /= 0)
call check('malformed: val untouched',    z == (-7._R8P, -7._R8P))
rval = -7._R8P
call fini%get(section_name='cylinder', option_name='epsilon', val=rval, error=error)
call check('complex into real: error',    error /= 0 .and. rval == -7._R8P)

! array get: the delimiter can be also inside the values
call check('count_values: list',          fini%count_values(section_name='cylinder', option_name='list') == 2)
call fini%get(section_name='cylinder', option_name='list', val=zarr, error=error)
call check('array: no error',             error == 0)
call check('array: values',               all(zarr == [(1._R8P, 2._R8P), (3._R8P, 4._R8P)]))
call check('count_values: commas',        fini%count_values(section_name='cylinder', option_name='commas', delimiter=',') == 3)
call fini%get(section_name='cylinder', option_name='commas', val=three, delimiter=',', error=error)
call check('array, comma delimiter',      error == 0 .and. all(three == [(1._R8P, 2._R8P), (3._R8P, 4._R8P), (5._R8P, 6._R8P)]))
zarr = (-7._R8P, -7._R8P)
call fini%get(section_name='cylinder', option_name='commas', val=zarr, error=error)
call check('array too small: error',      error /= 0 .and. all(zarr == (-7._R8P, -7._R8P)))
call fini%get(section_name='cylinder', option_name='real', val=zarr, error=error)
call check('array, no complex: error',    error /= 0 .and. all(zarr == (-7._R8P, -7._R8P)))
call check('count_values: real unchanged', fini%count_values(section_name='cylinder', option_name='tuple') == 2)

! add and exact read back
call fini%add(section_name='out', option_name='z', val=(80._R8P, 1.0e-4_R8P), error=error)
call check('add: no error',               error == 0)
call fini%get_string(section_name='out', option_name='z', val=sval)
call check('add: text',                   sval == '(80.0,0.0001)')
z = cmplx(1._R8P/3._R8P, -2._R8P/3._R8P, kind=R8P)
call fini%add(section_name='out', option_name='thirds', val=z)
zback(1) = (0._R8P, 0._R8P)
call fini%get(section_name='out', option_name='thirds', val=zback(1), error=error)
call check('add: exact read back',        error == 0 .and. zback(1) == z)
zarr = [cmplx(1._R8P/3._R8P, 2.5_R8P, kind=R8P), (-1.e-30_R8P, 7.e40_R8P)]
call fini%add(section_name='out', option_name='arr', val=zarr, error=error)
call check('add array: no error',         error == 0)
call check('add array: count_values',     fini%count_values(section_name='out', option_name='arr') == 2)
call fini%get(section_name='out', option_name='arr', val=zback, error=error)
call check('add array: exact read back',  error == 0 .and. all(zback == zarr))
call fini%add(section_name='out', option_name='z4', val=(0.1_R4P, -0.2_R4P))
call fini%get(section_name='out', option_name='z4', val=z4, error=error)
call check('add R4P: exact read back',    error == 0 .and. z4 == (0.1_R4P, -0.2_R4P))

! default values
call fini%get(section_name='out', option_name='missing', val=z, error=error, default=(1._R8P, 2._R8P))
call check('default complex',             error /= 0 .and. z == (1._R8P, 2._R8P))
call fini%get(section_name='out', option_name='missing', val=z, default=(3._R4P, 4._R4P))
call check('default complex R4P',         z == (3._R8P, 4._R8P))
call fini%get(section_name='out', option_name='missing', val=z, default=5._R8P)
call check('default real',                z == (5._R8P, 0._R8P))
call fini%get(section_name='out', option_name='missing', val=z, default=6)
call check('default integer',             z == (6._R8P, 0._R8P))
rval = -7._R8P
call fini%get(section_name='out', option_name='missing', val=rval, error=error, default=(1._R8P, 2._R8P))
call check('complex default, real val',   error /= 0 .and. rval == -7._R8P)

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

end program finer_test_complex
