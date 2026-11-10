call case_file%get(section_name='material', option_name='name', val=cells, error=error)
if (error == err_option_vals) print '(A)', 'error is err_option_vals'
