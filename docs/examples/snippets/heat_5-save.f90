call case_file%save(filename='restart.ini', retain_comments=.true., iostat=iostat)
if (iostat /= 0) stop 'cannot save restart.ini'
