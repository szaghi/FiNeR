call case_file%add(section_name='restart', option_name='from', val='heat-0002000.vtk')
call case_file%add(section_name='restart', option_name='step', val=2000_I4P)
call case_file%del(section_name='time', option_name='adaptive')
call case_file%del(section_name='output')
