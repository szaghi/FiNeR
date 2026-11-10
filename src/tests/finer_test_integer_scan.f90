!< FiNeR test: conversion of option values into integers, on any compiler.
program finer_test_integer_scan
!< Covers: an integer `get` accepts an optional sign and decimal digits within the range of the kind, and rejects
!<         anything else with `val` untouched: reals, exponents, out of range values (each kind, both ends). nvfortran's
!<         list-directed read accepts `3.5`, `1e3` and out of range values without error, so the conversion no longer
!<         relies on it. A list reads its first value, as before.
use finer, only: file_ini
use penf,  only: I1P, I2P, I4P, I8P
implicit none

type(file_ini) :: fini
integer(I8P)   :: i8
integer(I4P)   :: i4, error
integer(I1P)   :: i1
integer        :: passed, total

passed = 0 ; total = 0
print '(A)', 'finer_test_integer_scan'

call fini%load(source='[v]'//new_line('a')//                  &
                      'plus = +42'//new_line('a')//            &
                      'zero = -0'//new_line('a')//             &
                      'lead = 007'//new_line('a')//            &
                      'list = 5 6 7'//new_line('a')//          &
                      'real = 3.5'//new_line('a')//            &
                      'expo = 1e3'//new_line('a')//            &
                      'sign = -'//new_line('a')//              &
                      'mixed = 12a'//new_line('a')//           &
                      'i4max = 2147483647'//new_line('a')//    &
                      'i4min = -2147483648'//new_line('a')//   &
                      'i4over = 2147483648'//new_line('a')//   &
                      'i4under = -2147483649'//new_line('a')// &
                      'i8max = 9223372036854775807'//new_line('a')//  &
                      'i8min = -9223372036854775808'//new_line('a')// &
                      'i8over = 9223372036854775808'//new_line('a')// &
                      'i8under = -9223372036854775809'//new_line('a')// &
                      'i1max = 127'//new_line('a')//           &
                      'i1over = 128'//new_line('a')//          &
                      'i1min = -128')

call get4('plus',  42_I4P, .true.)
call get4('zero',  0_I4P,  .true.)
call get4('lead',  7_I4P,  .true.)
call get4('list',  5_I4P,  .true.)
call get4('real',  0_I4P,  .false.)
call get4('expo',  0_I4P,  .false.)
call get4('sign',  0_I4P,  .false.)
call get4('mixed', 0_I4P,  .false.)
call get4('i4max', huge(1_I4P), .true.)
call get4('i4min', -huge(1_I4P)-1_I4P, .true.)
call get4('i4over',  0_I4P, .false.)
call get4('i4under', 0_I4P, .false.)

i8 = -7_I8P ; call fini%get(section_name='v', option_name='i8max', val=i8, error=error)
call check('I8P max', error == 0 .and. i8 == huge(1_I8P))
i8 = -7_I8P ; call fini%get(section_name='v', option_name='i8min', val=i8, error=error)
call check('I8P min', error == 0 .and. i8 == -huge(1_I8P)-1_I8P)
i8 = -7_I8P ; call fini%get(section_name='v', option_name='i8over', val=i8, error=error)
call check('I8P overflow: error, untouched', error /= 0 .and. i8 == -7_I8P)
i8 = -7_I8P ; call fini%get(section_name='v', option_name='i8under', val=i8, error=error)
call check('I8P underflow: error, untouched', error /= 0 .and. i8 == -7_I8P)

i1 = -7_I1P ; call fini%get(section_name='v', option_name='i1max', val=i1, error=error)
call check('I1P max', error == 0 .and. i1 == 127_I1P)
i1 = -7_I1P ; call fini%get(section_name='v', option_name='i1min', val=i1, error=error)
call check('I1P min', error == 0 .and. i1 == -127_I1P - 1_I1P)
i1 = -7_I1P ; call fini%get(section_name='v', option_name='i1over', val=i1, error=error)
call check('I1P overflow: error, untouched', error /= 0 .and. i1 == -7_I1P)

call summary

contains
  subroutine get4(name, expected, ok)
  character(*), intent(in) :: name
  integer(I4P), intent(in) :: expected
  logical,      intent(in) :: ok
  i4 = -7_I4P
  call fini%get(section_name='v', option_name=name, val=i4, error=error)
  if (ok) then
    call check('I4P '//name//': converted', error == 0 .and. i4 == expected)
  else
    call check('I4P '//name//': error, untouched', error /= 0 .and. i4 == -7_I4P)
  endif
  endsubroutine get4

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
end program finer_test_integer_scan
