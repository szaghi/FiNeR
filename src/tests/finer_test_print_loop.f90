!< FiNeR test: print, save, loops, memory handling and assignment.
program finer_test_print_loop
!< Covers: print (with prefix and retained comments), save with retained comments, loop over all options and over the
!<         options of a section (repeated, interrupted, with sections without options), sections_number, section(index),
!<         initialize, free_options (all and of a section), assignment between files.
use finer, only: file_ini
use penf,  only: I4P
implicit none

character(*), parameter       :: filename = 'finer_test_print_loop.ini'
character(1), parameter       :: nl = new_line('A')
type(file_ini)                :: fini, other, empty
character(len=:), allocatable :: source, pair(:), names, sval
character(64)                 :: lines(10)
integer(I4P)                  :: ival, error
integer                       :: n, iostat, passed, total
logical                       :: again

passed = 0 ; total = 0
print '(A)', 'finer_test_print_loop'

source = 'g = 0'//nl//            &
         '[one]'//nl//            &
         'x = 1 ; comment of x'//nl// &
         'y = 2'//nl//            &
         '[empty]'//nl//          &
         '[two]'//nl//            &
         'z = 3'
call fini%load(source=source)

! print
call print_lines(retain_comments=.true., pref='|')
call check('print: iostat == 0',                 iostat == 0)
call check('print: 7 lines',                     n == 7)
call check('print: global option, no header',    trim(lines(1)) == '|  g = 0')
call check('print: section header',              trim(lines(2)) == '|[one]')
call check('print: option with comment',         trim(lines(3)) == '|  x = 1 ; comment of x')
call check('print: option',                      trim(lines(4)) == '|  y = 2')
call check('print: section without options',     trim(lines(5)) == '|[empty]')
call check('print: last option',                 trim(lines(7)) == '|  z = 3')
call print_lines(retain_comments=.false., pref='')
call check('print: comments dropped by default', trim(lines(3)) == '  x = 1')
call check('print: no prefix',                   trim(lines(2)) == '[one]')

! save with retained comments and reload
call fini%save(filename=filename, retain_comments=.true., iostat=iostat)
call check('save: iostat == 0',                  iostat == 0)
call other%load(filename=filename, error=error)
call check('saved file: no error',               error == 0)
call check('saved file: 4 sections',             other%sections_number() == 4)
call other%get(section_name='one', option_name='x', val=ival, error=error)
call check('saved file: value without comment',  error == 0 .and. ival == 1_I4P)
call other%free

! loop over all options
call loop_all(fini)
call check('loop all: 4 options',                n == 4)
call check('loop all: order',                    names == 'g x y z')
call loop_all(fini)
call check('loop all: repeated',                 n == 4 .and. names == 'g x y z')
call loop_all(empty)
call check('loop all: file without sections',    n == 0)

! loop over the options of a section
call loop_section(fini, 'one')
call check('loop section: 2 options',            n == 2 .and. names == 'x y')
call loop_section(fini, 'one')
call check('loop section: after a loop on all',  n == 2 .and. names == 'x y')
call loop_section(fini, 'empty')
call check('loop section: without options',      n == 0)
call loop_section(fini, 'missing')
call check('loop section: missing section',      n == 0)
call loop_section(fini, '')
call check('loop section: global section',       n == 1 .and. names == 'g')
call check('loop: first step on section one',    fini%loop(section_name='one', option_pairs=pair))
call loop_section(fini, 'two')
call check('loop section: other loop pending',   n == 1 .and. names == 'z')
again = fini%loop(section_name='one', option_pairs=pair)
if (again) again = trim(pair(1)) == 'y'
call check('loop: second step on section one',   again)
call check('loop: end of section one',           .not. fini%loop(section_name='one', option_pairs=pair))

! loops on different files do not interfere
call other%load(source='[a]'//nl//'p = 1'//nl//'q = 2'//nl//'r = 3')
call check('two files: first step on other',     other%loop(option_pairs=pair))
call loop_all(fini)
call check('two files: loop on the first',       n == 4 .and. names == 'g x y z')
again = other%loop(option_pairs=pair)
if (again) again = trim(pair(1)) == 'q'
call check('two files: second step on other',    again)
do while (other%loop(option_pairs=pair))
enddo

! sections number and names
call check('sections_number',                    fini%sections_number() == 4)
call check('sections_number: empty file',        empty%sections_number() == 0)
call check('section(1): global section',         len(fini%section(1)) == 0)
call check('section(2)',                         fini%section(2) == 'one')
call check('section(4)',                         fini%section(4) == 'two')

! free options
call fini%free_options(section_name='one')
call check('free_options(section): options',     .not. fini%has_option(option_name='x'))
call check('free_options(section): section',     fini%has_section(section_name='one'))
call check('free_options(section): others kept', fini%has_option(option_name='z'))
call fini%free_options
call check('free_options: all options',          .not. (fini%has_option(option_name='z') .or. fini%has_option(option_name='g')))
call check('free_options: sections kept',        fini%sections_number() == 4)

! initialize
call fini%initialize(filename='  '//filename//'  ')
call check('initialize: no sections',            fini%sections_number() == 0)
call check('initialize: filename',               fini%filename == filename)
call fini%load(error=error)
call check('initialize: load by filename',       error == 0 .and. fini%sections_number() == 4)
call fini%initialize
call check('initialize: filename removed',       .not. allocated(fini%filename))

! assignment
call fini%load(source='[p]'//nl//'k = 1'//nl//'[q]'//nl//'k = 2'//nl//'[r]'//nl//'k = 3')
call other%load(separator=':', source='[only]'//nl//'k : 9')
call other%get_string(section_name='only', option_name='k', val=sval)
call check('assignment: source loaded',          sval == '9')
fini = other
call check('assignment: sections replaced',      fini%sections_number() == 1 .and. .not. fini%has_section(section_name='p'))
call fini%get(section_name='only', option_name='k', val=ival, error=error)
call check('assignment: values copied',          error == 0 .and. ival == 9_I4P)
call fini%add(section_name='only', option_name='k', val=10_I4P)
call other%get(section_name='only', option_name='k', val=ival)
call check('assignment: independent copy',       ival == 9_I4P)
call fini%load(source='[s]'//nl//'m : 5')
call fini%get(section_name='s', option_name='m', val=ival, error=error)
call check('assignment: separator copied',       error == 0 .and. ival == 5_I4P)
fini = empty
call check('assignment: from an empty file',     fini%sections_number() == 0)

open(newunit=n, file=filename) ; close(unit=n, status='DELETE')

call summary
contains

  subroutine print_lines(retain_comments, pref)
  !< Print fini to a scratch unit and read back its lines.
  logical,      intent(in) :: retain_comments
  character(*), intent(in) :: pref
  integer                  :: unit, ios

  open(newunit=unit, status='scratch', action='readwrite')
  call fini%print(unit=unit, pref=pref, retain_comments=retain_comments, iostat=iostat)
  rewind(unit)
  lines = ''
  n = 0
  do
    read(unit, '(A)', iostat=ios) lines(min(n+1, size(lines)))
    if (ios /= 0) exit
    n = n + 1
  enddo
  close(unit)
  end subroutine print_lines

  subroutine loop_all(file)
  !< Loop over all the options of a file collecting their number and names.
  type(file_ini), intent(inout) :: file

  n = 0
  names = ''
  do while (file%loop(option_pairs=pair))
    n = n + 1
    names = trim(names//' '//trim(pair(1)))
    if (n > 100) exit
  enddo
  names = trim(adjustl(names))
  end subroutine loop_all

  subroutine loop_section(file, section_name)
  !< Loop over the options of a section collecting their number and names.
  type(file_ini), intent(inout) :: file
  character(*),   intent(in)    :: section_name

  n = 0
  names = ''
  do while (file%loop(section_name=section_name, option_pairs=pair))
    n = n + 1
    names = trim(names//' '//trim(pair(1)))
    if (n > 100) exit
  enddo
  names = trim(adjustl(names))
  end subroutine loop_section

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

end program finer_test_print_loop
