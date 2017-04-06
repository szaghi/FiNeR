!< FiNeR test: basic load.
program finer_test_load
!< FiNeR test: basic load.
!<
!<### Usage
!<```bash
!< ./finer_test_load
!<```
use finer, only :  file_ini
use flap, only : command_line_interface
use penf, only : I4P

implicit none
character(999)                :: file_name  !< Name of INI file.
type(file_ini)                :: fini       !< INI file handler.
character(len=:), allocatable :: items(:,:) !< Items pairs.
integer                       :: i          !< Counter.

call cli_parse
call fini%load(filename=file_name)
call fini%get_items(items)
do i=1,size(items,dim=1)
  print "(A)", trim(items(i,1))//' = '//trim(items(i,2))
enddo

contains
  subroutine cli_parse()
  !< Build and parse test cli.
  type(command_line_interface) :: cli   !< Test command line interface.
  integer(I4P)                 :: error !< Error trapping flag.

  call cli%init(progname='finer_test_load',                  &
                authors='S. Zaghi',                          &
                help='Usage: ',                              &
                examples=["finer_test_load --ini test.ini"], &
                epilog=new_line('a')//"all done")

  call cli%add(switch='--ini',          &
               switch_ab='-i',          &
               help='name of ini file', &
               required=.true.,         &
               act='store')

  call cli%parse(error=error) ; if (error/=0) stop

  call cli%get(switch='--ini', val=file_name)
  endsubroutine cli_parse
endprogram finer_test_load
