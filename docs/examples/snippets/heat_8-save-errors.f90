call empty%save(iostat=iostat, iomsg=iomsg)
print '(A,I0,A)', 'save without a file name: iostat ', iostat, ', "'//trim(iomsg)//'"'
call case_file%save(filename='no-such-dir/heat.ini', iostat=iostat)
print '(A,L1)', 'save into a missing directory: iostat /= 0 is ', iostat /= 0
