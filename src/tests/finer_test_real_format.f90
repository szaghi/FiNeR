!< FiNeR test: string representation of real and integer values.
program finer_test_real_format
!< Covers: shortest (tidy) representation of real values set by add, exact read back of scalar and array real values
!<         (R16P included, that is a quadruple precision real only where available), integer values without plus sign.
use finer, only: file_ini
use penf,  only: I4P, I8P, R4P, R8P, R16P
implicit none

type(file_ini)                :: fini
character(len=:), allocatable :: sval
real(R8P)                     :: r8, a8(4), a8_back(4)
real(R4P)                     :: r4
real(R16P)                    :: r16, r16_back
integer(I8P)                  :: i8
integer(I4P)                  :: error
integer                       :: i, mismatches, passed, total

passed = 0 ; total = 0
print '(A)', 'finer_test_real_format'

! tidy representation
call check('0.1',               text(0.1_R8P) == '0.1')
call check('-32.1',             text(-32.1_R8P) == '-32.1')
call check('100',               text(100._R8P) == '100.0')
call check('zero',              text(0._R8P) == '0.0')
call check('1.5',               text(1.5_R8P) == '1.5')
call check('0.001',             text(0.001_R8P) == '0.001')
call check('123456.789',        text(123456.789_R8P) == '123456.789')
call check('1/3',               text(1._R8P/3._R8P) == '0.3333333333333333')
call check('big exponent',      text(1.e20_R8P) == '1.0E+20')
call check('small exponent',    text(2.5e-7_R8P) == '2.5E-7')
call check('R4P 0.1',           text4(0.1_R4P) == '0.1')
call check('R4P -32.1',         text4(-32.1_R4P) == '-32.1')
call check('R4P 1/3',           text4(1._R4P/3._R4P) == '0.33333334')

call fini%add(section_name='s', option_name='arr', val=[1._R8P, 2.5_R8P, -3._R8P])
call fini%get_string(section_name='s', option_name='arr', val=sval)
call check('array',             sval == '1.0 2.5 -3.0')

! exact read back of extreme values
call check('huge R8P',          back(huge(1._R8P)) == huge(1._R8P))
call check('tiny R8P',          back(tiny(1._R8P)) == tiny(1._R8P))
call check('epsilon R8P',       back(1._R8P + epsilon(1._R8P)) == 1._R8P + epsilon(1._R8P))
call check('huge R4P',          back4(huge(1._R4P)) == huge(1._R4P))
call check('tiny R4P',          back4(tiny(1._R4P)) == tiny(1._R4P))

! exact read back of random values spanning the whole range of exponents
mismatches = 0
do i=1, 20000
  call random_number(r8) ; r8 = (r8 - 0.5_R8P) * 10._R8P**(mod(i, 600) - 300)
  if (back(r8) /= r8) mismatches = mismatches + 1
enddo
call check('random R8P exact',  mismatches == 0)
mismatches = 0
do i=1, 20000
  call random_number(r4) ; r4 = (r4 - 0.5_R4P) * 10._R4P**(mod(i, 60) - 30)
  if (back4(r4) /= r4) mismatches = mismatches + 1
enddo
call check('random R4P exact',  mismatches == 0)
call random_number(a8) ; a8 = (a8 - 0.5_R8P) * [1.e-200_R8P, 1._R8P, 1.e10_R8P, 1.e200_R8P]
call fini%add(section_name='s', option_name='rand', val=a8)
call fini%get(section_name='s', option_name='rand', val=a8_back, error=error)
call check('random array exact', error == 0 .and. all(a8_back == a8))

! R16P values: quadruple precision where available, the same of R8P otherwise
r16 = 1._R16P / 3._R16P
call fini%add(section_name='q', option_name='third', val=r16, error=error)
call check('R16P add: no error', error == 0)
r16_back = 0._R16P
call fini%get(section_name='q', option_name='third', val=r16_back, error=error)
call check('R16P exact',        error == 0 .and. r16_back == r16)
r16 = huge(1._R16P)
call fini%add(section_name='q', option_name='huge', val=r16)
call fini%get(section_name='q', option_name='huge', val=r16_back, error=error)
call check('R16P huge exact',   error == 0 .and. r16_back == r16)

! integer values are written without the plus sign
call fini%add(section_name='i', option_name='p', val=42_I4P)
call fini%get_string(section_name='i', option_name='p', val=sval)
call check('positive integer',  sval == '42')
call fini%add(section_name='i', option_name='n', val=-42_I4P)
call fini%get_string(section_name='i', option_name='n', val=sval)
call check('negative integer',  sval == '-42')
call fini%add(section_name='i', option_name='z', val=0_I4P)
call fini%get_string(section_name='i', option_name='z', val=sval)
call check('zero integer',      sval == '0')
call fini%add(section_name='i', option_name='b', val=10000000000_I8P)
call fini%get_string(section_name='i', option_name='b', val=sval)
call check('I8P integer',       sval == '10000000000')
call fini%get(section_name='i', option_name='b', val=i8, error=error)
call check('I8P read back',     error == 0 .and. i8 == 10000000000_I8P)
call fini%add(section_name='i', option_name='a', val=[1_I4P, -2_I4P, 3_I4P])
call fini%get_string(section_name='i', option_name='a', val=sval)
call check('integer array',     sval == '1 -2 3')

call summary
contains

  function text(val)
  real(R8P), intent(in)         :: val
  character(len=:), allocatable :: text
  call fini%add(section_name='t', option_name='x', val=val)
  call fini%get_string(section_name='t', option_name='x', val=text)
  end function text

  function text4(val)
  real(R4P), intent(in)         :: val
  character(len=:), allocatable :: text4
  call fini%add(section_name='t', option_name='x', val=val)
  call fini%get_string(section_name='t', option_name='x', val=text4)
  end function text4

  function back(val)
  real(R8P), intent(in) :: val
  real(R8P)             :: back
  back = -huge(1._R8P)
  if (val == back) back = 0._R8P
  call fini%add(section_name='t', option_name='x', val=val)
  call fini%get(section_name='t', option_name='x', val=back)
  end function back

  function back4(val)
  real(R4P), intent(in) :: val
  real(R4P)             :: back4
  back4 = -huge(1._R4P)
  if (val == back4) back4 = 0._R4P
  call fini%add(section_name='t', option_name='x', val=val)
  call fini%get(section_name='t', option_name='x', val=back4)
  end function back4

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

end program finer_test_real_format
