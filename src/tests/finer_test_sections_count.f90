!< FiNeR test: the number of sections stays consistent.
program finer_test_sections_count
!< Covers: `sections_number` and `get_sections_list` agree after load, add, del, assignment and a reload of a source
!<         without sections, which leaves no section (the count was kept by hand in a separate component, issue #35).
use finer, only: file_ini
implicit none

type(file_ini)                :: fini, copy
character(len=:), allocatable :: list(:)
integer                       :: passed, total

passed = 0 ; total = 0
print '(A)', 'finer_test_sections_count'

call fini%load(source='[a]'//new_line('a')//'x = 1'//new_line('a')//'[b]'//new_line('a')//'y = 2')
call check('load: 2 sections', fini%sections_number() == 2)
call check('load: list agrees', list_size(fini) == 2)

call fini%add(section_name='c')
call fini%add(section_name='')
call check('add named and global: 4 sections', fini%sections_number() == 4)
call check('add: list agrees', list_size(fini) == 4)

call fini%del(section_name='a')
call check('del: 3 sections', fini%sections_number() == 3)
call check('del: list agrees', list_size(fini) == 3)

copy = fini
call check('assignment: 3 sections', copy%sections_number() == 3)
call check('assignment: list agrees', list_size(copy) == 3)

call fini%load(source='; only a comment')
call check('reload without sections: none left', fini%sections_number() == 0)
call check('reload without sections: list agrees', list_size(fini) == 0)
call check('reload without sections: old section gone', .not.fini%has_section(section_name='b'))

call summary

contains
  function list_size(f) result(n)
  type(file_ini), intent(in) :: f
  integer                    :: n
  call f%get_sections_list(list)
  n = 0 ; if (allocated(list)) n = size(list)
  endfunction list_size

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
end program finer_test_sections_count
