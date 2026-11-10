program write_arrays
!< Cookbook: write arrays and complex numbers.
use finer
use penf, only : I4P, R8P
implicit none
type(file_ini) :: ini

call ini%add(section_name='arrays', option_name='ids', val=[1_I4P, 2_I4P, 3_I4P])
call ini%add(section_name='arrays', option_name='weights', val=[0.25_R8P, 0.5_R8P, 0.25_R8P])
call ini%add(section_name='arrays', option_name='mask', val=[.true., .false., .true.])
call ini%add(section_name='arrays', option_name='names', val=['inlet ', 'outlet'])
call ini%add(section_name='arrays', option_name='z', val=(1.0_R8P, -2.0_R8P))
call ini%add(section_name='arrays', option_name='zs', val=[(1.0_R8P, 0.5_R8P), (0.0_R8P, 1.0_R8P)])
call ini%print(unit=6)
endprogram write_arrays
