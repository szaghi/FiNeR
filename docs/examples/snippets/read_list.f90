program read_list
!< Cookbook: read a list of numbers of unknown length.
use finer
use penf, only : R8P
implicit none
type(file_ini)         :: ini
real(R8P), allocatable :: probes(:)

call ini%load(filename='heat.ini')
allocate(probes(ini%count_values(section_name='boundaries', option_name='probes')))
call ini%get(section_name='boundaries', option_name='probes', val=probes)
print '(I0,A,*(F6.2))', size(probes), ' probes:', probes
endprogram read_list
