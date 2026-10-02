!< FiNeR test: regression on real INI files.
program finer_test_regression_files
!< Covers: load of the regression files src/tests/test.ini (odd but legal contents) and src/tests/huge.ini (a real,
!<         large file), typed get of their values, the library autotest.
use finer, only: file_ini, file_ini_autotest
use penf,  only: I4P, R8P
implicit none

type(file_ini)                :: fini
character(len=:), allocatable :: sval, pair(:)
real(R8P)                     :: rval, rarr(3)
integer(I4P)                  :: ival, iarr(3), error
logical                       :: lval
integer                       :: n, passed, total

passed = 0 ; total = 0
print '(A)', 'finer_test_regression_files'

! test.ini
call fini%load(filename='src/tests/test.ini', error=error)
call check('test.ini: no error',               error == 0)
call check('test.ini: 8 sections',             fini%sections_number() == 8)
call fini%get_string(section_name='', option_name='default', val=sval, error=error)
call check('test.ini: global option',          error == 0 .and. sval == 'default')
call check('test.ini: commented section',      .not. fini%has_section(section_name='TestINI'))
call check('test.ini: header with comment',    fini%has_section(section_name='SETTINGS'))
call fini%get(section_name='SETTINGS', option_name='sections', val=ival, error=error)
call check('test.ini: inline comment',         error == 0 .and. ival == 8_I4P)
call check('test.ini: commented option',       .not. fini%has_option(option_name='garbage'))
call check('test.ini: section without options', fini%has_section(section_name='EmptySection') .and. &
                                               .not. fini%loop(section_name='EmptySection', option_pairs=pair))
call fini%get_string(section_name='TestINI2=ga;rbage', option_name='aridiculouslylongname', val=sval, error=error)
call check('test.ini: long value',             error == 0 .and. &
                                               sval == 'anevenmoreridiculouslylongvalueofabsolutelyawesometestingvalueforminiini')
call fini%get(section_name='vals', option_name='float', val=rval, error=error)
call check('test.ini: negative real',          error == 0 .and. rval == -1.545_R8P)
call fini%get(section_name='vals', option_name='float3', val=rval, error=error)
call check('test.ini: real without integer part', error == 0 .and. rval == 0.5_R8P)
call fini%get(section_name='vals', option_name='floatplus', val=rval, error=error)
call check('test.ini: real with plus',         error == 0 .and. rval == 2.3_R8P)
call fini%get(section_name='vals', option_name='intmin', val=ival, error=error)
call check('test.ini: smallest I4P',           error == 0 .and. ival == -huge(1_I4P) - 1_I4P)
call fini%get(section_name='vals', option_name='intmax', val=ival, error=error)
call check('test.ini: biggest I4P',            error == 0 .and. ival == huge(1_I4P))
ival = -7_I4P
call fini%get(section_name='vals', option_name='intover1', val=ival, error=error)
call check('test.ini: I4P overflow',           error /= 0 .and. ival == -7_I4P)
call fini%get(section_name='vals', option_name='intplus', val=ival, error=error)
call check('test.ini: integer with plus',      error == 0 .and. ival == 5_I4P)
lval = .false.
call fini%get(section_name='vals', option_name='bool', val=lval, error=error)
call check('test.ini: logical',                error == 0 .and. lval)
call fini%get(section_name='vals', option_name='bool3', val=lval, error=error)
call check('test.ini: not a logical',          error /= 0)
call check('test.ini: count with delimiter',   fini%count_values(section_name='multivals', option_name='multiint', &
                                                                 delimiter=',') == 3)
call fini%get(section_name='multivals', option_name='multiint', val=iarr, delimiter=',', error=error)
call check('test.ini: integer array',          error == 0 .and. all(iarr == [-5_I4P, 6_I4P, 845_I4P]))
call fini%get(section_name='multivals', option_name='multifloat', val=rarr, delimiter=',', error=error)
call check('test.ini: real array',             error == 0 .and. all(rarr == [5.0_R8P, -6.987_R8P, 84458.461_R8P]))
call fini%get_string(section_name='arrays', option_name='string11', val=sval, error=error)
call check('test.ini: string',                 error == 0 .and. sval == 'c_lmao')
n = 0
do while (fini%loop(section_name='iteration', option_pairs=pair))
  n = n + 1
enddo
call check('test.ini: loop over a section',    n == 4)

! huge.ini
call fini%free
call fini%load(filename='src/tests/huge.ini', error=error)
call check('huge.ini: no error',               error == 0)
call check('huge.ini: 1616 sections',          fini%sections_number() == 1616)
call check('huge.ini: first section',          fini%section(1) == 'General')
call check('huge.ini: last section',           fini%section(1616) == 'VariableNames')
call fini%get_string(section_name='General', option_name='UIName', val=sval, error=error)
call check('huge.ini: first option',           error == 0 .and. sval == 'Name:General')
call fini%get_string(section_name='VariableNames', option_name='13', val=sval, error=error)
call check('huge.ini: last option',            error == 0 .and. sval == 'Hospital')

! the library autotest must run without errors
call fini%free
call file_ini_autotest

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

end program finer_test_regression_files
