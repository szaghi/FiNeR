program heat
!< Tutorial, chapter 6: writing a file from scratch.
use finer
use penf, only : I4P, I8P, R4P, R8P
implicit none
type(file_ini) :: results
integer(I4P)   :: iostat

!region build
call results%add(section_name='', option_name='case', val='heat.ini')                   ! the global section
call results%add(section_name='summary')
call results%add(section_name='summary', option_name='steps', val=4000_I4P)
call results%add(section_name='summary', option_name='cells_updated', val=256000_I8P)
call results%add(section_name='summary', option_name='time', val=0.4_R8P)
call results%add(section_name='summary', option_name='residual', val=3.2e-9_R4P)
call results%add(section_name='summary', option_name='converged', val=.true.)
call results%add(section_name='probes', option_name='x', val=[0.1_R8P, 0.25_R8P, 0.4_R8P])
call results%add(section_name='probes', option_name='T', val=[331.25_R8P, 324.5_R8P, 310.125_R8P])
call results%add(section_name='probes', option_name='names', val=['p1', 'p2', 'p3'])
call results%add(section_name='spectrum', option_name='mode1', val=(1.5_R8P, -0.25_R8P))
!endregion build
!region print
call results%print(unit=6, pref='| ')
!endregion print
!region save
call results%save(filename='results.ini', iostat=iostat)
print '(A,I0)', 'saved results.ini, iostat ', iostat
!endregion save
endprogram heat
!as heat
!run heat_6 heat
!run heat_6_file cat results.ini
