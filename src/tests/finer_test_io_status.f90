!< FiNeR test: I/O status of save and print.
program finer_test_io_status
!< Covers: `save` and `print` always define `iostat` (issue #35: it was undefined when there was nothing to write) and
!<         report the first failure: no file name, nothing to save, a file that cannot be opened, a unit that cannot be
!<         written, and a save that succeeds and loads back.
use finer, only: file_ini, err_source_missing
use penf,  only: I4P
implicit none

character(len=*), parameter :: FILENAME = 'finer_test_io_status.tmp.ini'
type(file_ini)              :: fini, back
integer(I4P)                :: iostat, error, iv
character(len=200)          :: iomsg
integer                     :: passed, total, unit
logical                     :: exists

passed = 0 ; total = 0
print '(A)', 'finer_test_io_status'

! empty file_ini: no file name
iostat = -999
call fini%save(iostat=iostat, iomsg=iomsg)
call check('save without file name: iostat = ERR_SOURCE_MISSING', iostat == err_source_missing)
call check('save without file name: iomsg set', len_trim(iomsg) > 0)

! empty file_ini: nothing to print or save
iostat = -999
call fini%print(unit=6, iostat=iostat)
call check('print without sections: iostat = 0', iostat == 0)
open(newunit=unit, file=FILENAME) ; close(unit, status='delete')
iostat = -999
call fini%save(filename=FILENAME, iostat=iostat)
inquire(file=FILENAME, exist=exists)
call check('save without sections: iostat = 0', iostat == 0)
call check('save without sections: no file written', .not.exists)

! a file that cannot be opened
call fini%add(section_name='s', option_name='k', val=1_I4P)
iostat = 0
call fini%save(filename='finer_no_such_dir/x.ini', iostat=iostat, iomsg=iomsg)
call check('save into a missing directory: iostat /= 0', iostat /= 0)

! a unit that cannot be written
open(newunit=unit, file=FILENAME, action='write', status='replace') ; close(unit)
open(newunit=unit, file=FILENAME, action='read', status='old')
iostat = 0
call fini%print(unit=unit, iostat=iostat, iomsg=iomsg)
close(unit)
call check('print to a read-only unit: iostat /= 0', iostat /= 0)

! a save that succeeds
iostat = -999
call fini%save(filename=FILENAME, iostat=iostat)
call check('save: iostat = 0', iostat == 0)
call back%load(filename=FILENAME, error=error)
iv = -1
call back%get(section_name='s', option_name='k', val=iv, error=error)
call check('save: loads back', error == 0 .and. iv == 1_I4P)
open(newunit=unit, file=FILENAME) ; close(unit, status='delete')

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
end program finer_test_io_status
