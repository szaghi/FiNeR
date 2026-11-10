!< FiNeR test: section names with leading or trailing blanks.
program finer_test_section_name_blanks
!< Covers: `add` (scalar and array) and `free_options` of a section given with leading or trailing blanks address the
!<         same section as its trimmed name. `add_section` stores the trimmed name, so the other procedures must look the
!<         section up the same way (issue #35: `add` returned error=0 and added nothing).
use finer, only: file_ini
use penf,  only: I4P
implicit none

type(file_ini) :: fini
integer(I4P)   :: error, iv, ia(3)
integer        :: passed, total

passed = 0 ; total = 0
print '(A)', 'finer_test_section_name_blanks'

call fini%add(section_name=' sec ', option_name='k', val=7_I4P, error=error)
call check('add scalar to " sec ": no error', error == 0)
call check('add scalar to " sec ": option exists', fini%has_option(option_name='k'))
iv = -1
call fini%get(section_name='sec', option_name='k', val=iv, error=error)
call check('get from "sec": no error, value 7', error == 0 .and. iv == 7_I4P)
iv = -1
call fini%get(section_name=' sec ', option_name='k', val=iv, error=error)
call check('get from " sec ": no error, value 7', error == 0 .and. iv == 7_I4P)
call check('only one section', fini%sections_number() == 1)

call fini%add(section_name='sec  ', option_name='a', val=[1_I4P, 2_I4P, 3_I4P], error=error)
call check('add array to "sec  ": no error', error == 0)
ia = -1
call fini%get(section_name='sec', option_name='a', val=ia, error=error)
call check('get array from "sec": no error, values 1 2 3', error == 0 .and. all(ia == [1_I4P, 2_I4P, 3_I4P]))
call check('still one section', fini%sections_number() == 1)

call fini%add(section_name='  sec', option_name='k', val=8_I4P, error=error)
iv = -1
call fini%get(section_name='sec', option_name='k', val=iv, error=error)
call check('update through "  sec": value 8', error == 0 .and. iv == 8_I4P)

call fini%free_options(section_name=' sec ')
call check('free_options of " sec ": no option left', .not.fini%has_option(option_name='k') .and. &
                                                       .not.fini%has_option(option_name='a'))
call check('free_options of " sec ": section kept', fini%has_section(section_name='sec'))

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
end program finer_test_section_name_blanks
