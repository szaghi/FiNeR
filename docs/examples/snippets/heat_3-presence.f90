print '(A,L1)', 'has [restart]: ', case_file%has_section(section_name='restart')
where = repeat(' ', 32)
if (case_file%has_option(option_name='density', section_name=where)) print '(A)', 'density is in ['//trim(where)//']'
