call case_file%get_items(items)
print '(A,I0,A)', 'get_items: ', size(items, dim=1), ' pairs, the last is '// &
                  trim(items(size(items, dim=1), 1))//' = '//trim(items(size(items, dim=1), 2))
