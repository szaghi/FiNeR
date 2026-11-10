call legacy%load(filename='legacy.cfg', separator=':')
call legacy%get(section_name='mesh.refinement', option_name='levels', val=levels)
call legacy%get_string(section_name='', option_name='author', val=author)
print '(A,I0,A)', 'levels = ', levels, ', author = '//author
