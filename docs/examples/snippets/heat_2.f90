program heat
!< Tutorial, chapter 2: reading the case file.
use finer
use penf, only : I4P, R8P
implicit none
type(file_ini)            :: case_file
integer(I4P)              :: error
integer(I4P)              :: cells, steps
real(R8P)                 :: length, dt, conductivity
logical                   :: adaptive
character(len=64)         :: title
real(R8P)                 :: origin(3)
real(R8P),    allocatable :: probes(:)

call case_file%load(filename='heat.ini', error=error)
if (error /= 0) then
  print '(A,I0)', 'cannot read heat.ini, error ', error
  stop 1
endif
call case_file%get(section_name='grid', option_name='cells', val=cells)
call case_file%get(section_name='grid', option_name='length', val=length)
call case_file%get(section_name='time', option_name='dt', val=dt)
call case_file%get(section_name='time', option_name='steps', val=steps)
call case_file%get(section_name='time', option_name='adaptive', val=adaptive)
call case_file%get(section_name='material', option_name='conductivity', val=conductivity)
call case_file%get(section_name='', option_name='title', val=title)
call case_file%get(section_name='grid', option_name='origin', val=origin)
allocate(probes(case_file%count_values(section_name='boundaries', option_name='probes')))
call case_file%get(section_name='boundaries', option_name='probes', val=probes)
print '(A)', trim(title)
print '(A,I0,A,F5.2,A)', 'grid: ', cells, ' cells over ', length, ' m'
print '(A,ES10.3,A,I0,A,L1)', 'time: dt = ', dt, ', ', steps, ' steps, adaptive = ', adaptive
print '(A,F6.1)', 'conductivity: ', conductivity
print '(A,3F5.1)', 'origin:', origin
print '(A,I0,A,3F6.2)', 'probes (', size(probes), '):', probes
endprogram heat
