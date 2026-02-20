!< FiNeR test: load from file.
program finer_test_load
!< Covers: load from a physical INI file, section count, scalar string get,
!<         count_values, and array get.
use finer, only: file_ini
use penf,  only: R4P
implicit none

type(file_ini)                :: fini
character(len=:), allocatable :: val, slist(:)
real(R4P),        allocatable :: vel(:)
integer                       :: error, Nv, passed, total

passed = 0 ; total = 0
print '(A)', 'finer_test_load'

call fini%load(filename='src/tests/test-2.ini', error=error)
call check('load: no error',       error == 0)

call fini%get_sections_list(slist)
call check('load: 4 sections',     size(slist) == 4)

val = repeat(' ', 64)
call fini%get(section_name='non_dimensional_numbers', option_name='Re', val=val, error=error)
call check('scalar string value',  trim(val) == '1.0e6')

Nv = fini%count_values(section_name='free_conditions', option_name='velocity')
call check('velocity count == 3',  Nv == 3)
allocate(vel(1:Nv))
call fini%get(section_name='free_conditions', option_name='velocity', val=vel, error=error)
call check('velocity(1) == 1.0',   abs(vel(1) - 1._R4P) < 1e-5_R4P)
call check('velocity(2) == 2.0',   abs(vel(2) - 2._R4P) < 1e-5_R4P)
call check('velocity(3) == 3.0',   abs(vel(3) - 3._R4P) < 1e-5_R4P)

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

end program finer_test_load
