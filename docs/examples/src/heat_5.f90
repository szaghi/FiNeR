program heat
!< Tutorial, chapter 5: editing and saving.
use finer
use penf, only : I4P, R8P
implicit none
type(file_ini) :: case_file, restart
integer(I4P)   :: error, iostat
real(R8P)      :: dt

call case_file%load(filename='heat.ini')
!region update
call case_file%add(section_name='time', option_name='dt', val=5.0e-5_R8P)       ! an option that exists: updated
call case_file%add(section_name='time', option_name='steps', val=4000_I4P)
call case_file%add(section_name='time', option_name='start', val=0.2_R8P)       ! a new option: appended
!endregion update
!region sections
call case_file%add(section_name='restart', option_name='from', val='heat-0002000.vtk')
call case_file%add(section_name='restart', option_name='step', val=2000_I4P)
call case_file%del(section_name='time', option_name='adaptive')
call case_file%del(section_name='output')
!endregion sections
!region save
call case_file%save(filename='restart.ini', retain_comments=.true., iostat=iostat)
if (iostat /= 0) stop 'cannot save restart.ini'
!endregion save
!region check
call restart%load(filename='restart.ini', error=error)
call restart%get(section_name='time', option_name='dt', val=dt)
print '(A,ES10.3,A,L1)', 'dt read back: ', dt, ', exact: ', dt == 5.0e-5_R8P
!endregion check
endprogram heat
!as heat
!run heat_5 heat
!run heat_5_file cat restart.ini
