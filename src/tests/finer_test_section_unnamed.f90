!< FiNeR test: name of a section without a name.
program finer_test_section_unnamed
!< Covers: `section%name()` of a default-initialised section (name not allocated) is an empty string. Its result length
!<         was `len(self%sname)` of an unallocated component, which is undefined (issue #35).
use finer_section_t, only: section
implicit none

type(section) :: unnamed, named
integer       :: passed, total

passed = 0 ; total = 0
print '(A)', 'finer_test_section_unnamed'

call check('unnamed section: empty name', len(unnamed%name()) == 0)
named = section(section_name='abc')
call check('named section: its name', named%name() == 'abc' .and. len(named%name()) == 3)
call named%free
call check('freed section: empty name', len(named%name()) == 0)

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
end program finer_test_section_unnamed
