call case_file%get(section_name='grid', option_name='origin', val=origin)
allocate(probes(case_file%count_values(section_name='boundaries', option_name='probes')))
call case_file%get(section_name='boundaries', option_name='probes', val=probes)
