call case_file%get_string(section_name='output', option_name='fields', val=fields)
print '(A)', 'fields = "'//fields//'"'
call case_file%get_string(section_name='output', option_name='compression', val=format, default='none')
print '(A)', 'compression = "'//format//'"'
