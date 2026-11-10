call case_file%add(section_name='time', option_name='dt', val=5.0e-5_R8P)       ! an option that exists: updated
call case_file%add(section_name='time', option_name='steps', val=4000_I4P)
call case_file%add(section_name='time', option_name='start', val=0.2_R8P)       ! a new option: appended
