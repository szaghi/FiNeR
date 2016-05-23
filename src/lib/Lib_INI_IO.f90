!< Library for INI file parsing.
module Lib_INI_IO
!< Library for INI file parsing.
!-----------------------------------------------------------------------------------------------------------------------------------
use penf
use Lib_IO_Misc
use Lib_Strings
use, intrinsic :: ISO_FORTRAN_ENV, only : stdout => OUTPUT_UNIT, stderr => ERROR_UNIT
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
save
private
public :: ini_autotest
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
integer(I4P), parameter :: err_option_name       = 1     !< Error flag for trapping errors in option name.
integer(I4P), parameter :: err_option_vals       = 2     !< Error flag for trapping errors in option values.
integer(I4P), parameter :: err_option            = 3     !< Error flag for trapping errors in option.
integer(I4P), parameter :: err_section_name      = 4     !< Error flag for trapping errors in section name.
integer(I4P), parameter :: err_section_options   = 5     !< Error flag for trapping errors in section options.
integer(I4P), parameter :: err_section           = 6     !< Error flag for trapping errors in section.
integer(I4P), parameter :: err_source_missing    = 7     !< Error flag for trapping errors in file when source is missing.
character(1), parameter :: def_opt_sep           = '='   !< Default separator of option name/value.
character(*), parameter :: comments              = "!;#" !< Characters used for defining a comment line.
character(1), parameter :: inline_comment        = ';'   !< Inline comment delimiter.

type :: Type_Option
  !< Option data of sections.
  character(len=:), allocatable :: oname !< Option name.
  character(len=:), allocatable :: ovals !< Option values.
  character(len=:), allocatable :: ocomm !< Eventual option inline comment.
  contains
    procedure:: free         => free_option         !< Procedure for freeing dynamic memory.
    procedure:: parse        => parse_option        !< Procedure for parsing option data.
    procedure:: count_values => count_values_option !< Procedure for counting option value(s).
    generic::   set          => set_option, &       !< Procedure for setting option value (scalar).
                                set_a_option        !< Procedure for setting option value (array).
    generic::   get          => get_option, &       !< Procedure for getting option value (scalar).
                                get_a_option        !< Procedure for getting option value (array).
    procedure:: print        => print_option        !< Procedure for pretting printing data.
    procedure:: save         => save_option         !< Procedure for saving data.
    final::     finalize_option                     !< Procedure for freeing dynamic memory when finalizing.
    ! operators overloading
    generic:: assignment(=) => assign_option !< Procedure for option assignment overloading.
    ! private procedures
    procedure,              private:: parse_name    => parse_name_option    !< Procedure for parsing option name.
    procedure,              private:: parse_value   => parse_value_option   !< Procedure for parsing option values.
    procedure,              private:: parse_comment => parse_comment_option !< Procedure for parsing option inline comment.
    procedure,              private:: set_option                            !< Procedure for setting option value (scalar).
    procedure,              private:: set_a_option                          !< Procedure for setting option value (array).
    procedure,              private:: get_option                            !< Procedure for getting option value (scalar).
    procedure,              private:: get_a_option                          !< Procedure for getting option value (array).
    procedure, pass(self1), private:: assign_option                         !< Procedure for option assignment overloading.
endtype Type_Option

type:: Type_Section
  !< Derived type for handling sections of file.
  character(len=:),  allocatable:: sname      !< Section name.
  type(Type_Option), allocatable:: options(:) !< Section options.
  contains
    procedure:: free         => free_section                !< Procedure for freeing dynamic memory.
    procedure:: free_options => free_options_section        !< Procedure for freeing all options.
    procedure:: free_option  => free_option_section         !< Procedure for freeing a option.
    procedure:: parse        => parse_section               !< Procedure for parsing section data.
    procedure:: index        => index_option_section        !< Procedure for returning the index of an option.
    procedure:: count_values => count_values_option_section !< Procedure for counting option value(s).
    generic::   set          => set_option_section, &       !< Procedure for setting option value (scalar).
                                set_a_option_section        !< Procedure for setting option value (array).
    generic::   add          => add_option_section, &       !< Procedure for adding an option (scalar).
                                add_a_option_section        !< Procedure for adding an option (array).
    generic::   get          => get_option_section, &       !< Procedure for getting option value (scalar).
                                get_a_option_section        !< Procedure for getting option value (array).
    procedure:: loop         => loop_options_section        !< Procedure for looping over options.
    procedure:: print        => print_section               !< Procedure for pretting printing data.
    procedure:: save         => save_section                !< Procedure for saving data.
    final::     finalize_section                            !< Procedure for freeing dynamic memory in finalizing.
    ! operators overloading
    generic:: assignment(=) => assign_section !< Procedure for section assignment overloading.
    ! private procedures
    procedure,              private:: parse_name    => parse_section_name    !< Procedure for getting section name.
    procedure,              private:: parse_options => parse_options_section !< Procedure for getting section options.
    procedure,              private:: set_option_section                     !< Procedure for setting option value (scalar).
    procedure,              private:: set_a_option_section                   !< Procedure for setting option value (array).
    procedure,              private:: add_option_section                     !< Procedure for adding an option (scalar).
    procedure,              private:: add_a_option_section                   !< Procedure for adding an option (array).
    procedure,              private:: get_option_section                     !< Procedure for getting option value (scalar).
    procedure,              private:: get_a_option_section                   !< Procedure for getting option value (array).
    procedure, pass(self1), private:: assign_section                         !< Procedure for section assignment overloading.
endtype Type_Section

type, public:: Type_File_INI
  !< Derived type for handling INI files.
  !<
  !< @note The OOP encapsulation allows safe use of parallel paradigms.
  character(len=:),   allocatable::          filename              !< File name
  integer(I4P)::                             Ns = 0                !< Number of sections.
  character(1)::                             opt_sep = def_opt_sep !< Separator character of option name/value.
  type(Type_Section), allocatable, private:: sections(:)           !< Sections.
  contains
    procedure:: free                                                 !< Procedure for freeing dynamic memory destroyng file data.
    generic::   free_options => free_options_all,        &           !< Procedure for freeing all options.
                                free_options_of_section, &           !< Procedure for freeing all options of a section.
                                free_option_of_section_file_ini      !< Procedure for freeing an option of a section.
    procedure:: load                                                 !< Procedure for loading file data.
    procedure:: has_option   => has_option_file_ini                  !< Procedure for inquiring the presence of an option.
    procedure:: has_section  => has_section_file_ini                 !< Procedure for inquiring the presence of a section.
    procedure:: section      => section_file_ini                     !< Procedure for getting section name once provided an index.
    generic::   index        => index_section_file_ini, &            !< Procedure for returning the index of a section.
                                index_option_file_ini                !< Procedure for returning the index of an option.
    procedure:: count_values => count_values_option_section_file_ini !< Procedure for counting option value(s).
    generic::   add          => add_section_file_ini,       &        !< Procedure for adding a section.
                                add_option_section_file_ini,&        !< Procedure for adding an option to a section (scalar).
                                add_a_option_section_file_ini        !< Procedure for adding an option to a section (array).
    generic::   get          => get_option_section_file_ini, &       !< Procedure for getting option value (scalar).
                                get_a_option_section_file_ini        !< Procedure for getting option value (array).
    generic::   del          => free_option_of_section_file_ini, &   !< Procedure for removing (freeing) an option of a section.
                                free_section_file_ini                !< Procedure for removing (freeing) a section.
    procedure:: items        => items_file_ini                       !< Procedure for getting list of couples option name/value.
    generic::   loop         => loop_options_section_file_ini, &     !< Procedure for looping over options of a section.
                                loop_options_file_ini                !< Procedure for looping over all options.
    procedure:: print        => print_file_ini                       !< Procedure for pretting printing data.
    procedure:: save         => save_file_ini                        !< Procedure for saving data.
    final::     finalize                                             !< Procedure for freeing dynamic memory when finalizing.
    ! operators overloading
    generic:: assignment(=) => assign_file_ini !< Procedure for section assignment overloading.
    ! private procedures
    procedure,              private:: parse                               !< Procedure for parsing file data.
    procedure,              private:: free_options_all                    !< Procedure for freeing all options of all sections.
    procedure,              private:: free_options_of_section             !< Procedure for freeing all options of a section.
    procedure,              private:: free_option_of_section_file_ini     !< Procedure for freeing an option of a section.
    procedure,              private:: free_section_file_ini               !< Procedure for freeing a section.
    procedure,              private:: index_section_file_ini              !< Procedure for returning the index of a section.
    procedure,              private:: index_option_file_ini               !< Procedure for returning the index of an option.
    procedure,              private:: add_section_file_ini                !< Procedure for adding a section.
    procedure,              private:: add_option_section_file_ini         !< Procedure for adding an option to a section (scalar).
    procedure,              private:: add_a_option_section_file_ini       !< Procedure for adding an option to a section (scalar).
    procedure,              private:: get_option_section_file_ini         !< Procedure for getting option value (scalar).
    procedure,              private:: get_a_option_section_file_ini       !< Procedure for getting option value (array).
    procedure,              private:: loop_options_section_file_ini       !< Procedure for looping over options of a section.
    procedure,              private:: loop_options_file_ini               !< Procedure for looping over all options.
    procedure, pass(self1), private:: assign_file_ini                     !< Procedure for section assignment overloading.
endtype Type_File_INI
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  ! Type_Option procedures
  elemental subroutine free_option(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for freeing dynamic memory.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Option), intent(INOUT):: self !< Option data.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(self%oname)) deallocate(self%oname)
  if (allocated(self%ovals)) deallocate(self%ovals)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine free_option

  elemental subroutine finalize_option(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for freeing dynamic memory when finalizing.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  type(Type_Option), intent(INOUT):: self !< Option data.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%free
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine finalize_option

  elemental subroutine parse_name_option(self,sep,source,error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for parsing option name from a source string.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Option), intent(INOUT):: self   !< Option data.
  character(*),       intent(IN)::    sep    !< Separator of option name/value.
  character(*),       intent(IN)::    source !< String containing option data.
  integer(I4P),       intent(OUT)::   error  !< Error code.
  integer(I4P)::                      pos    !< Characters counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  error = err_option_name
  pos = index(source, sep)
  if (pos > 0) then
    self%oname = trim(adjustl(source(:pos-1)))
    error = 0
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine parse_name_option

  elemental subroutine parse_value_option(self,sep,source,error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for parsing option value from a source string.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Option), intent(INOUT):: self   !< Option data.
  character(*),       intent(IN)::    sep    !< Separator of option name/value.
  character(*),       intent(IN)::    source !< String containing option data.
  integer(I4P),       intent(OUT)::   error  !< Error code.
  integer(I4P)::                      pos    !< Characters counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  error = err_option_vals
  pos = index(source, sep)
  if (pos > 0) then
    if (pos<len(source)) self%ovals = trim(adjustl(source(pos+1:)))
    error = 0
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine parse_value_option

  elemental subroutine parse_comment_option(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for parsing eventaul option inline comment trimming it out from pure value string.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Option), intent(INOUT):: self   !< Option data.
  integer(I4P)::                      pos    !< Characters counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(self%ovals)) then
    pos = index(self%ovals,inline_comment)
    if (pos>0) then
      if (pos<len(self%ovals)) self%ocomm = trim(adjustl(self%ovals(pos+1:)))
      self%ovals = trim(adjustl(self%ovals(:pos-1)))
    endif
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine parse_comment_option

  elemental subroutine parse_option(self,sep,source,error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for parsing option data from a source string.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Option), intent(INOUT):: self   !< Option data.
  character(*),       intent(IN)::    sep    !< Separator of option name/value.
  character(*),       intent(IN)::    source !< String containing option data.
  integer(I4P),       intent(OUT)::   error  !< Error code.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  error = err_option
  if (scan(adjustl(source), comments) == 1) return
  call self%parse_name(sep=sep,source=source,error=error)
  call self%parse_value(sep=sep,source=source,error=error)
  call self%parse_comment
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine parse_option

  elemental function count_values_option(self,delimiter) result(Nv)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for getting the number of values of option data.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Option),     intent(IN):: self      !< Option data.
  character(*), optional, intent(IN):: delimiter !< Delimiter used for separating values.
  character(len=:), allocatable::      dlm       !< Dummy string for delimiter handling.
  integer(I4P)::                       Nv        !< Number of values.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(self%ovals)) then
    dlm = ' ' ; if (present(delimiter)) dlm = delimiter
    Nv = count(string=self%ovals,substring=dlm) + 1
  else
    Nv = 0
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction count_values_option

  subroutine set_option(self,val)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for setting option data value (scalar).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Option), intent(INOUT):: self !< Option data.
  class(*),           intent(IN)::    val  !< Value.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select type(val)
#ifdef r16p
  type is(real(R16P))
    self%ovals = trim(str(n=val))
#endif
  type is(real(R8P))
    self%ovals = trim(str(n=val))
  type is(real(R4P))
    self%ovals = trim(str(n=val))
  type is(integer(I8P))
    self%ovals = trim(str(n=val))
  type is(integer(I4P))
    self%ovals = trim(str(n=val))
  type is(integer(I2P))
    self%ovals = trim(str(n=val))
  type is(integer(I1P))
    self%ovals = trim(str(n=val))
  type is(logical)
    self%ovals = trim(str(n=val))
  type is(character(*))
    self%ovals=val
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine set_option

  subroutine set_a_option(self,delimiter,val)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for setting option data value (array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Option),     intent(INOUT):: self      !< Option data.
  character(*), optional, intent(IN)::    delimiter !< Delimiter used for separating values.
  class(*),               intent(IN)::    val(1:)   !< Value.
  character(len=:), allocatable::         dlm       !< Dummy string for delimiter handling.
  integer(I4P)::                          v         !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  dlm = ' ' ; if (present(delimiter)) dlm = delimiter
  self%ovals = ''
  select type(val)
#ifdef r16p
  type is(real(R16P))
    do v=1,size(val)
      self%ovals = self%ovals//dlm//trim(str(n=val(v)))
    enddo
    self%ovals = trim(adjustl(self%ovals))
#endif
  type is(real(R8P))
    do v=1,size(val)
      self%ovals = self%ovals//dlm//trim(str(n=val(v)))
    enddo
    self%ovals = trim(adjustl(self%ovals))
  type is(real(R4P))
    do v=1,size(val)
      self%ovals = self%ovals//dlm//trim(str(n=val(v)))
    enddo
    self%ovals = trim(adjustl(self%ovals))
  type is(integer(I8P))
    do v=1,size(val)
      self%ovals = self%ovals//dlm//trim(str(n=val(v)))
    enddo
    self%ovals = trim(adjustl(self%ovals))
  type is(integer(I4P))
    do v=1,size(val)
      self%ovals = self%ovals//dlm//trim(str(n=val(v)))
    enddo
    self%ovals = trim(adjustl(self%ovals))
  type is(integer(I2P))
    do v=1,size(val)
      self%ovals = self%ovals//dlm//trim(str(n=val(v)))
    enddo
    self%ovals = trim(adjustl(self%ovals))
  type is(integer(I1P))
    do v=1,size(val)
      self%ovals = self%ovals//dlm//trim(str(n=val(v)))
    enddo
    self%ovals = trim(adjustl(self%ovals))
  type is(logical)
    do v=1,size(val)
      self%ovals = self%ovals//dlm//trim(str(n=val(v)))
    enddo
    self%ovals = trim(adjustl(self%ovals))
  type is(character(*))
    do v=1,size(val)
      self%ovals = self%ovals//dlm//trim(val(v))
    enddo
    self%ovals = trim(adjustl(self%ovals))
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine set_a_option

  subroutine get_option(self,error,val)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for getting option data value (scalar).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Option),     intent(IN)::    self   !< Option data.
  integer(I4P), optional, intent(OUT)::   error  !< Error code.
  class(*),               intent(INOUT):: val    !< Value.
  integer(I4P)::                          errd   !< Error code.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  errd = err_option_vals
  if (allocated(self%ovals)) then
    errd = 0
    select type(val)
#ifdef r16p
    type is(real(R16P))
      val = cton(str=trim(adjustl(self%ovals)),knd=1._R16P)
#endif
    type is(real(R8P))
      val = cton(str=trim(adjustl(self%ovals)),knd=1._R8P)
    type is(real(R4P))
      val = cton(str=trim(adjustl(self%ovals)),knd=1._R4P)
    type is(integer(I8P))
      val = cton(str=trim(adjustl(self%ovals)),knd=1_I8P)
    type is(integer(I4P))
      val = cton(str=trim(adjustl(self%ovals)),knd=1_I4P)
    type is(integer(I2P))
      val = cton(str=trim(adjustl(self%ovals)),knd=1_I2P)
    type is(integer(I1P))
      val = cton(str=trim(adjustl(self%ovals)),knd=1_I1P)
    type is(logical)
      read(self%ovals,*)val
    type is(character(*))
      val=self%ovals
    endselect
  endif
  if (present(error)) error = errd
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine get_option

  subroutine get_a_option(self,delimiter,error,val)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for getting option data values (array).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Option),     intent(IN)::          self      !< Option data.
  character(*), optional, intent(IN)::          delimiter !< Delimiter used for separating values.
  integer(I4P), optional, intent(OUT)::         error     !< Error code.
  class(*),               intent(INOUT)::       val(1:)   !< Value.
  character(len=:), allocatable::               dlm       !< Dummy string for delimiter handling.
  integer(I4P)::                                Nv        !< Number of values.
  character(len=len(self%ovals)), allocatable:: valsV(:)  !< String array of values.
  integer(I4P)::                                errd      !< Error code.
  integer(I4P)::                                v         !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  errd = err_option_vals
  dlm = ' ' ; if (present(delimiter)) dlm = delimiter
  if (allocated(self%ovals)) then
    errd = 0
    call tokenize(strin=trim(adjustl(self%ovals)),delimiter=dlm,Nt=Nv,toks=valsV)
    select type(val)
#ifdef r16p
    type is(real(R16P))
      do v=1,Nv
        val(v) = cton(str=trim(adjustl(valsV(v))),knd=1._R16P)
      enddo
#endif
    type is(real(R8P))
      do v=1,Nv
        val(v) = cton(str=trim(adjustl(valsV(v))),knd=1._R8P)
      enddo
    type is(real(R4P))
      do v=1,Nv
        val(v) = cton(str=trim(adjustl(valsV(v))),knd=1._R4P)
      enddo
    type is(integer(I8P))
      do v=1,Nv
        val(v) = cton(str=trim(adjustl(valsV(v))),knd=1_I8P)
      enddo
    type is(integer(I4P))
      do v=1,Nv
        val(v) = cton(str=trim(adjustl(valsV(v))),knd=1_I4P)
      enddo
    type is(integer(I2P))
      do v=1,Nv
        val(v) = cton(str=trim(adjustl(valsV(v))),knd=1_I2P)
      enddo
    type is(integer(I1P))
      do v=1,Nv
        val(v) = cton(str=trim(adjustl(valsV(v))),knd=1_I1P)
      enddo
    type is(logical)
      do v=1,Nv
        read(valsV(v),*)val(v)
      enddo
    type is(character(*))
      do v=1,Nv
        val(v)=valsV(v)
      enddo
    endselect
  endif
  if (present(error)) error = errd
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine get_a_option

  subroutine print_option(self,pref,iostat,iomsg,unit,retain_comments)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for printing data with a pretty format.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Option),     intent(IN)::  self            !< Option data.
  character(*), optional, intent(IN)::  pref            !< Prefixing string.
  integer(I4P), optional, intent(OUT):: iostat          !< IO error.
  character(*), optional, intent(OUT):: iomsg           !< IO error message.
  integer(I4P),           intent(IN)::  unit            !< Logic unit.
  logical,                intent(IN)::  retain_comments !< Flag for retaining eventual comments.
  character(len=:), allocatable::       prefd           !< Prefixing string.
  integer(I4P)::                        iostatd         !< IO error.
  character(500)::                      iomsgd          !< Temporary variable for IO error message.
  character(len=:), allocatable::       comment         !< Eventual option comments.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(self%oname)) then
    prefd = '' ; if (present(pref)) prefd = pref
    comment = '' ; if (allocated(self%ocomm).and.retain_comments) comment = ' ; '//self%ocomm
    if (allocated(self%ovals)) then
      write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)prefd//self%oname//' = '//self%ovals//comment
    else
      write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)prefd//self%oname//' = '//comment
    endif
    if (present(iostat)) iostat = iostatd
    if (present(iomsg))  iomsg  = iomsgd
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine print_option

  subroutine save_option(self,iostat,iomsg,unit,retain_comments)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for saving data.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Option),     intent(IN)::  self            !< Option data.
  integer(I4P), optional, intent(OUT):: iostat          !< IO error.
  character(*), optional, intent(OUT):: iomsg           !< IO error message.
  integer(I4P),           intent(IN)::  unit            !< Logic unit.
  logical,                intent(IN)::  retain_comments !< Flag for retaining eventual comments.
  integer(I4P)::                        iostatd         !< IO error.
  character(500)::                      iomsgd          !< Temporary variable for IO error message.
  character(len=:), allocatable::       comment         !< Eventual option comments.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(self%oname)) then
    comment = '' ; if (allocated(self%ocomm).and.retain_comments) comment = ' ; '//self%ocomm
    if (allocated(self%ovals)) then
      write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)self%oname//' = '//self%ovals//comment
    else
      write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)self%oname//' = '//comment
    endif
    if (present(iostat)) iostat = iostatd
    if (present(iomsg))  iomsg  = iomsgd
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine save_option

  elemental subroutine assign_option(self1,self2)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for assignment between two selfs.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Option), intent(INOUT):: self1 !< Left hand side.
  type(Type_Option),  intent(IN)::    self2 !< Rigth hand side.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(self2%oname)) self1%oname = self2%oname
  if (allocated(self2%ovals)) self1%ovals = self2%ovals
  if (allocated(self2%ocomm)) self1%ocomm = self2%ocomm
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine assign_option

  ! Type_Section procedures
  elemental subroutine free_section(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for freeing dynamic memory.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Section), intent(INOUT):: self !< Section data.
  integer(I4P)::                       o    !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(self%sname)) deallocate(self%sname)
  if (allocated(self%options)) then
    do o=1,size(self%options)
      call self%options(o)%free
    enddo
    deallocate(self%options)
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine free_section

  elemental subroutine finalize_section(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for freeing dynamic memory when finalizing.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  type(Type_Section), intent(INOUT):: self !< Section data.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%free
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine finalize_section

  elemental subroutine free_options_section(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for freeing all options.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Section), intent(INOUT):: self !< Section data.
  integer(I4P)::                       o    !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(self%options)) then
    do o=1,size(self%options)
      call self%options(o)%free
    enddo
    deallocate(self%options)
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine free_options_section

  elemental subroutine free_option_section(self,option)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for freeing an option.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Section), intent(INOUT):: self       !< Section data.
  character(*),        intent(IN)::    option     !< Option name.
  type(Type_Option), allocatable::     options(:) !< Temporary options array.
  integer(I4P)::                       o          !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(self%options)) then
    o = self%index(option=option)
    if (o>0) then
      allocate(options(1:size(self%options)-1))
      if (o==1) then
        options = self%options(2:)
      elseif (o==size(self%options)) then
        options = self%options(:o-1)
      else
        options(:o-1) = self%options(:o-1)
        options(o:  ) = self%options(o+1:)
      endif
      call move_alloc(options,self%options)
    endif
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine free_option_section

  elemental subroutine parse_section_name(self,source,error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for getting section name from a source string.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Section), intent(INOUT):: self     !< Section data.
  character(*),        intent(IN)::    source   !< String containing section data.
  integer(I4P),        intent(OUT)::   error    !< Error code.
  integer(I4P)::                       pos(1:2) !< Characters counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  error = err_section_name
  pos(1) = index(source, "[")
  pos(2) = index(source, "]")
  if (all(pos > 0)) then
    self%sname = trim(adjustl(source(pos(1)+1:pos(2)-1)))
    error = 0
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine parse_section_name

  elemental subroutine parse_options_section(self,sep,source,error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for getting section options from a source string.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Section), intent(INOUT)::  self       !< Section data.
  character(*),        intent(IN)::     sep        !< Separator of option name/value.
  character(*),        intent(IN)::     source     !< String containing section data.
  integer(I4P),        intent(OUT)::    error      !< Error code.
  character(len(source))::              osource    !< String containing options data.
  character(len(source)), allocatable:: options(:) !< Options strings tokenized.
  character(len(source))::              dummy      !< Dummy string for parsing options.
  integer(I4P)::                        No,o,oo    !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  error = 0
  osource = trim(adjustl(source(index(source, "]")+1:)))
  call tokenize(strin=osource,delimiter=new_line('A'),toks=options)
  No = 0
  o = 0
  do while (o+1<=size(options))
    o = o + 1
    if (scan(adjustl(options(o)), comments) == 1) cycle
    if (index(options(o), sep)>0) then
      No = No + 1
      dummy = options(o)
      oo = o
      do while (oo+1<=size(options))
        oo = oo + 1
        if (index(options(oo), sep)>0) then
          ! new option... go back
          exit
        else
          ! continuation of current option
          dummy = trim(adjustl(dummy))//" "//trim(adjustl(options(oo)))
          options(oo) = comments ! forcing skip this in the following scan
        endif
      enddo
      options(o) = trim(adjustl(dummy))
    endif
  enddo
  if (No>0) then
    if (allocated(self%options)) deallocate(self%options) ; allocate(self%options(1:No))
    o = 0
    oo = 0
    do while (o+1<=size(options))
      o = o + 1
      if (scan(adjustl(options(o)), comments) == 1) cycle
      if (index(options(o), sep)>0) then
        oo = oo + 1
        call self%options(oo)%parse(sep=sep,source=options(o),error=error)
      endif
    enddo
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine parse_options_section

  elemental subroutine parse_section(self,sep,source,error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for getting section data from a source string.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Section), intent(INOUT):: self   !< Section data.
  character(*),        intent(IN)::    sep    !< Separator of option name/value.
  character(*),        intent(IN)::    source !< String containing section data.
  integer(I4P),        intent(OUT)::   error  !< Error code.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%parse_name(source=source,error=error)
  call self%parse_options(sep=sep,source=source,error=error)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine parse_section

  elemental function count_values_option_section(self,delimiter,option) result(Nv)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for getting the number of values of option into section data.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Section),    intent(IN):: self      !< Section data.
  character(*), optional, intent(IN):: delimiter !< Delimiter used for separating values.
  character(*),           intent(IN):: option    !< Option name.
  integer(I4P)::                       Nv        !< Number of values.
  character(len=:), allocatable::      dlm       !< Dummy string for delimiter handling.
  integer(I4P)::                       o         !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(self%options)) then
    dlm = ' ' ; if (present(delimiter)) dlm = delimiter
    do o=1,size(self%options)
      if (self%options(o)%oname == trim(adjustl(option))) then
        Nv = self%options(o)%count_values(delimiter=dlm)
        exit
      endif
    enddo
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction count_values_option_section

  elemental function index_option_section(self,back,option) result(ind)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for returning the index of the option matching the name passed.
  !<
  !< @note The matching index returned is the first found if *back* is not passed or if *back=.false.*. On the contrary the last
  !< found is returned if *back=.true.*.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Section), intent(IN):: self   !< Section data.
  logical, optional,   intent(IN):: back   !< If back appears with the value true, the last matching index is returned.
  character(*),        intent(IN):: option !< Option name.
  integer(I4P)::                    ind    !< Index of searched section.
  logical::                         backd  !< Dummy back flag.
  integer(I4P)::                    o      !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  ind = 0
  if (allocated(self%options)) then
    backd = .false. ; if (present(back)) backd = back
    if (backd) then
      do o=size(self%options),1,-1
        if (self%options(o)%oname == trim(adjustl(option))) then
          ind = o
          exit
        endif
      enddo
    else
      do o=1,size(self%options)
        if (self%options(o)%oname == trim(adjustl(option))) then
          ind = o
          exit
        endif
      enddo
    endif
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction index_option_section

  subroutine set_option_section(self,error,option,val)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for setting option value (scalar)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Section),    intent(INOUT):: self   !< Section data.
  integer(I4P), optional, intent(OUT)::   error  !< Error code.
  character(*),           intent(IN)::    option !< Option name.
  class(*),               intent(IN)::    val    !< Value.
  integer(I4P)::                          errd   !< Error code.
  integer(I4P)::                          o      !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  errd = err_section_options
  if (allocated(self%options)) then
    do o=1,size(self%options)
      if (self%options(o)%oname == trim(adjustl(option))) then
        call self%options(o)%set(val=val)
        exit
      endif
    enddo
  endif
  if (present(error)) error = errd
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine set_option_section

  subroutine set_a_option_section(self,delimiter,error,option,val)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for setting option value (array)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Section),    intent(INOUT):: self      !< Section data.
  character(*), optional, intent(IN)::    delimiter !< Delimiter used for separating values.
  integer(I4P), optional, intent(OUT)::   error     !< Error code.
  character(*),           intent(IN)::    option    !< Option name.
  class(*),               intent(IN)::    val(:)    !< Value.
  integer(I4P)::                          errd      !< Error code.
  character(len=:), allocatable::         dlm       !< Dummy string for delimiter handling.
  integer(I4P)::                          o         !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  dlm = ' ' ; if (present(delimiter)) dlm = delimiter
  errd = err_section_options
  if (allocated(self%options)) then
    do o=1,size(self%options)
      if (self%options(o)%oname == trim(adjustl(option))) then
        call self%options(o)%set(delimiter=dlm,val=val)
        exit
      endif
    enddo
  endif
  if (present(error)) error = errd
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine set_a_option_section

  subroutine add_option_section(self,error,option,val)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for adding an option (with scalar value).
  !<
  !< If the option already exists, its value is updated.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Section),    intent(INOUT):: self       !< Section data.
  integer(I4P), optional, intent(OUT)::   error      !< Error code.
  character(*),           intent(IN)::    option     !< Option name.
  class(*),               intent(IN)::    val        !< Option value.
  type(Type_Option), allocatable::        options(:) !< Temporary options array.
  integer(I4P)::                          errd       !< Error code.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  errd = err_section_options
  if (allocated(self%options)) then
    call self%set(error=errd,option=option,val=val)
    if (errd/=0) then ! the option does not exist
      allocate(options(1:size(self%options)+1))
      options(1:size(self%options)  ) = self%options
      options(  size(self%options)+1) = Type_Option(oname=option)
      call move_alloc(options,self%options)
      call self%set(error=errd,option=option,val=val)
    endif
  else
    allocate(self%options(1:1))
    self%options(1)%oname = option
    call self%set(error=errd,option=option,val=val)
  endif
  if (present(error)) error = errd
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine add_option_section

  subroutine add_a_option_section(self,delimiter,error,option,val)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for adding an option (with array value).
  !<
  !< If the option already exists, its value is updated.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Section),    intent(INOUT):: self       !< Section data.
  character(*), optional, intent(IN)::    delimiter  !< Delimiter used for separating values.
  integer(I4P), optional, intent(OUT)::   error      !< Error code.
  character(*),           intent(IN)::    option     !< Option name.
  class(*),               intent(IN)::    val(:)     !< Option value.
  type(Type_Option), allocatable::        options(:) !< Temporary options array.
  integer(I4P)::                          errd       !< Error code.
  character(len=:), allocatable::         dlm        !< Dummy string for delimiter handling.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  dlm = ' ' ; if (present(delimiter)) dlm = delimiter
  errd = err_section_options
  if (allocated(self%options)) then
    call self%set(delimiter=dlm,error=errd,option=option,val=val)
    if (errd/=0) then ! the option does not exist
      allocate(options(1:size(self%options)+1))
      options(1:size(self%options)  ) = self%options
      options(  size(self%options)+1) = Type_Option(oname=option)
      call move_alloc(options,self%options)
      call self%set(error=errd,option=option,val=val)
    endif
  else
    allocate(self%options(1:1))
    self%options(1)%oname = option
    call self%set(delimiter=dlm,error=errd,option=option,val=val)
  endif
  if (present(error)) error = errd
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine add_a_option_section

  subroutine get_option_section(self,error,option,val)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for getting option value (scalar)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Section),    intent(IN)::    self   !< Section data.
  integer(I4P), optional, intent(OUT)::   error  !< Error code.
  character(*),           intent(IN)::    option !< Option name.
  class(*),               intent(INOUT):: val    !< Value.
  integer(I4P)::                          errd   !< Error code.
  integer(I4P)::                          o      !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(self%options)) then
    do o=1,size(self%options)
      if (self%options(o)%oname == trim(adjustl(option))) then
        call self%options(o)%get(error=errd,val=val)
        if (present(error)) error = errd
        exit
      endif
    enddo
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine get_option_section

  subroutine get_a_option_section(self,delimiter,error,option,val)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for getting option value (array)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Section),    intent(IN)::    self      !< Section data.
  character(*), optional, intent(IN)::    delimiter !< Delimiter used for separating values.
  integer(I4P), optional, intent(OUT)::   error     !< Error code.
  character(*),           intent(IN)::    option    !< Option name.
  class(*),               intent(INOUT):: val(1:)   !< Value.
  character(len=:), allocatable::         dlm       !< Dummy string for delimiter handling.
  integer(I4P)::                          errd      !< Error code.
  integer(I4P)::                          o         !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  dlm = ' ' ; if (present(delimiter)) dlm = delimiter
  if (allocated(self%options)) then
    do o=1,size(self%options)
      if (self%options(o)%oname == trim(adjustl(option))) then
        call self%options(o)%get(delimiter=dlm,error=errd,val=val)
        if (present(error)) error = errd
        exit
      endif
    enddo
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine get_a_option_section

  function loop_options_section(self,option) result(again)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for performing a while loop returning option name/value defined into section.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Section),           intent(IN)::  self      !< Section data.
  character(len=:), allocatable, intent(OUT):: option(:) !< Couples option name/value [1:2].
  logical::                                    again     !< Flag continuing the loop.
  integer(I4P), save::                         o=0       !< Counter.
  integer(I4P)::                               Nc        !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  again = .false.
  if (allocated(self%options)) then
    if (o==0) then
      o = lbound(self%options,dim=1)
      Nc = max(len(self%options(o)%oname),len(self%options(o)%ovals))
      allocate(character(Nc):: option(1:2))
      option(1) = self%options(o)%oname
      option(2) = self%options(o)%ovals
      again = .true.
    elseif (o<ubound(self%options,dim=1)) then
      o = o + 1
      Nc = max(len(self%options(o)%oname),len(self%options(o)%ovals))
      allocate(character(Nc):: option(1:2))
      option(1) = self%options(o)%oname
      option(2) = self%options(o)%ovals
      again = .true.
    else
      o = 0
      again = .false.
    endif
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction loop_options_section

  subroutine print_section(self,pref,iostat,iomsg,unit,retain_comments)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for printing data with a pretty format.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Section),    intent(IN)::  self            !< Section data.
  character(*), optional, intent(IN)::  pref            !< Prefixing string.
  integer(I4P), optional, intent(OUT):: iostat          !< IO error.
  character(*), optional, intent(OUT):: iomsg           !< IO error message.
  integer(I4P),           intent(IN)::  unit            !< Logic unit.
  logical,                intent(IN)::  retain_comments !< Flag for retaining eventual comments.
  character(len=:), allocatable::       prefd           !< Prefixing string.
  integer(I4P)::                        iostatd         !< IO error.
  character(500)::                      iomsgd          !< Temporary variable for IO error message.
  integer(I4P)::                        o               !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  prefd = '' ; if (present(pref)) prefd = pref
  if (allocated(self%sname)) write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)prefd//'['//self%sname//']'
  if (allocated(self%options)) then
    do o=1,size(self%options)
      call self%options(o)%print(pref=prefd//'  ',iostat=iostatd,iomsg=iomsgd,unit=unit,retain_comments=retain_comments)
    enddo
  endif
  if (present(iostat)) iostat = iostatd
  if (present(iomsg))  iomsg  = iomsgd
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine print_section

  subroutine save_section(self,iostat,iomsg,unit,retain_comments)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for saving data.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Section),    intent(IN)::  self            !< Section data.
  integer(I4P), optional, intent(OUT):: iostat          !< IO error.
  character(*), optional, intent(OUT):: iomsg           !< IO error message.
  integer(I4P),           intent(IN)::  unit            !< Logic unit.
  logical,                intent(IN)::  retain_comments !< Flag for retaining eventual comments.
  integer(I4P)::                        iostatd         !< IO error.
  character(500)::                      iomsgd          !< Temporary variable for IO error message.
  integer(I4P)::                        o               !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(self%sname)) write(unit=unit,fmt='(A)',iostat=iostatd,iomsg=iomsgd)'['//self%sname//']'
  if (allocated(self%options)) then
    do o=1,size(self%options)
      call self%options(o)%save(iostat=iostatd,iomsg=iomsgd,unit=unit,retain_comments=retain_comments)
    enddo
  endif
  if (present(iostat)) iostat = iostatd
  if (present(iomsg))  iomsg  = iomsgd
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine save_section

  elemental subroutine assign_section(self1,self2)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for assignment between two selfs.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Section), intent(INOUT):: self1 !< Left hand side.
  type(Type_Section),  intent(IN)::    self2 !< Rigth hand side.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(self2%sname)) self1%sname = self2%sname
  if (allocated(self2%options)) then
    if (allocated(self1%options)) deallocate(self1%options) ; allocate(self1%options(1:size(self2%options)))
    self1%options = self2%options
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine assign_section

  ! Type_File_INI procedures
  elemental subroutine free(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for freeing dynamic memory.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_File_INI), intent(INOUT):: self !< Section data.
  integer(I4P)::                        s    !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(self%filename)) deallocate(self%filename)
  if (allocated(self%sections)) then
    do s=1,size(self%sections)
      call self%sections(s)%free
    enddo
    deallocate(self%sections)
  endif
  self%Ns = 0
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine free

  elemental subroutine finalize(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for freeing dynamic memory when finalizing.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  type(Type_File_INI), intent(INOUT):: self !< Section data.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%free
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine finalize

  elemental subroutine free_options_all(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for freeing all options of all sections.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_File_INI), intent(INOUT):: self !< File data.
  integer(I4P)::                        s    !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(self%sections)) then
    do s=1,size(self%sections)
      call self%sections(s)%free_options
    enddo
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine free_options_all

  elemental subroutine free_options_of_section(self,section)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for freeing all options of a section.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_File_INI), intent(INOUT):: self    !< File data.
  character(*),         intent(IN)::    section !< Section name.
  integer(I4P)::                        s       !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(self%sections)) then
    do s=1,size(self%sections)
      if (self%sections(s)%sname == section) then
        call self%sections(s)%free_options
        exit
      endif
    enddo
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine free_options_of_section

  elemental subroutine free_option_of_section_file_ini(self,section,option)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for freeing all options of a section.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_File_INI), intent(INOUT):: self    !< File data.
  character(*),         intent(IN)::    section !< Section name.
  character(*),         intent(IN)::    option  !< Option  name.
  integer(I4P)::                        s       !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  s = self%index(section=section)
  if (s>0) call self%sections(s)%free_option(option=option)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine free_option_of_section_file_ini

  elemental subroutine free_section_file_ini(self,section)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for freeing all options of a section.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_File_INI), intent(INOUT):: self        !< File data.
  character(*),         intent(IN)::    section     !< Section name.
  type(Type_Section), allocatable::     sections(:) !< Temporary sections array.
  integer(I4P)::                        s           !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  s = self%index(section=section)
  if (s>0) then
    allocate(sections(1:size(self%sections)-1))
    if (s==1) then
      sections = self%sections(2:)
    elseif (s==size(self%sections)) then
      sections = self%sections(:s-1)
    else
      sections(:s-1) = self%sections(:s-1)
      sections(s:  ) = self%sections(s+1:)
    endif
    call move_alloc(sections,self%sections)
    self%Ns = self%Ns - 1
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine free_section_file_ini

  subroutine load(self,separator,filename,source,error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for getting file data from a file or a source string.
  !<
  !<### Usage
  !<
  !<##### Loading from a file
  !<```bash
  !<type(Type_File_INI):: fini
  !<call fini%load(filename='path_to_my_file.ini')
  !<```
  !<
  !<##### Loading from a source string
  !<```bash
  !<type(Type_File_INI):: fini
  !<call fini%load(source='[section-1] option-1=one [section-2] option-2=due')
  !<```
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_File_INI),   intent(INOUT):: self      !< File data.
  character(1), optional, intent(IN)::    separator !< Separator of options name/value.
  character(*), optional, intent(IN)::    filename  !< File name.
  character(*), optional, intent(IN)::    source    !< File source.
  integer(I4P), optional, intent(OUT)::   error     !< Error code.
  integer(I4P)::                          errd      !< Error code.
  character(len=:), allocatable::         sourced   !< Dummy source string.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  errd = err_source_missing
  if (present(separator)) self%opt_sep = separator
  if (present(filename)) then
    self%filename = trim(adjustl(filename))
    call read_file_as_stream(filename=self%filename,fast_read=.true.,stream=sourced)
    call self%parse(source=sourced,error=errd)
  elseif (present(source)) then
    call self%parse(source=source,error=errd)
  elseif (allocated(self%filename)) then
    call read_file_as_stream(filename=self%filename,fast_read=.true.,stream=sourced)
    call self%parse(source=sourced,error=errd)
  endif
  if (present(error)) error = errd
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine load

  subroutine parse(self,source,error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for parsing file either from the self source data or from a source string.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_File_INI),   intent(INOUT)::   self    !< File data.
  character(*),           intent(IN)::      source  !< String source.
  integer(I4P), optional, intent(OUT)::     error   !< Error code.
  integer(I4P)::                            errd    !< Error code.
  character(len=len(source)), allocatable:: toks(:) !< Dummies tokens.
  character(len(source))::                  dummy   !< Dummy string for parsing sections.
  integer(I4P)::                            Ns,s,ss !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  errd = err_source_missing
  call tokenize(strin=source,delimiter=new_line('A'),toks=toks)
  Ns = 0
  s = 0
  do while (s+1<=size(toks))
    s = s + 1
    if (scan(adjustl(toks(s)), comments) == 1) cycle
    if (index(trim(adjustl(toks(s))), "[") == 1) then
      Ns = Ns + 1
      dummy = trim(adjustl(toks(s)))//new_line('A')
      ss = s
      do while (ss+1<=size(toks))
        ss = ss + 1
        if (index(trim(adjustl(toks(ss))), "[") == 1) then
          ! new section... go back
          exit
        else
          ! continuation of current section
          dummy = trim(adjustl(dummy))//new_line('A')//trim(adjustl(toks(ss)))
          toks(ss) = comments ! forcing skip this in the following scan
        endif
      enddo
      toks(s) = trim(adjustl(dummy))
    endif
  enddo
  if (Ns>0) then
    if (allocated(self%sections)) deallocate(self%sections) ; allocate(self%sections(1:Ns))
    s = 0
    ss = 0
    do while (s+1<=size(toks))
      s = s + 1
      if (scan(adjustl(toks(s)), comments) == 1) cycle
      if (index(trim(adjustl(toks(s))), "[") == 1) then
        ss = ss + 1
        call self%sections(ss)%parse(sep=self%opt_sep,source=toks(s),error=errd)
      endif
    enddo
  endif
  self%Ns = size(self%sections,dim=1)
  if (present(error)) error = errd
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine parse

  function has_option_file_ini(self, section, option) result(pres)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Inquire the presence of (at least one) option with the name passed.
  !<
  !< Optionall, the first matching section name is returned.
  !<
  !< @note All sections are searched and the first occurence is returned.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(Type_File_INI),   intent(in)    :: self    !< File data.
  character(*), optional, intent(inout) :: section !< Section name.
  character(*),           intent(in)    :: option  !< Option name.
  logical                               :: pres    !< Inquiring flag.
  integer(I4P)                          :: s       !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  pres = .false.
  if (allocated(self%sections)) then
    do s=1,size(self%sections)
      pres = (self%sections(s)%index(option=option)>0)
      if (pres) then
        if (present(section)) section = self%sections(s)%sname
        exit
      endif
    enddo
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction has_option_file_ini

  elemental function has_section_file_ini(self,section) result(pres)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for inquiring the presence of (at least one) section with the name passed.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_File_INI), intent(IN):: self    !< File data.
  character(*),         intent(IN):: section !< Section name.
  logical::                          pres    !< Inquiring flag.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  pres = (self%index(section=section)>0)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction has_section_file_ini

  elemental function index_section_file_ini(self,back,section) result(ind)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for returning the index of the section matching the name passed.
  !<
  !< @note The matching index returned is the first found if *back* is not passed or if *back=.false.*. On the contrary the last
  !< found is returned if *back=.true.*.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_File_INI), intent(IN):: self    !< File data.
  logical, optional,    intent(IN):: back    !< If back appears with the value true, the last matching index is returned.
  character(*),         intent(IN):: section !< Section name.
  integer(I4P)::                     ind     !< Index of searched section.
  logical::                          backd   !< Dummy back flag.
  integer(I4P)::                     s       !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  ind = 0
  if (allocated(self%sections)) then
    backd = .false. ; if (present(back)) backd = back
    if (backd) then
      do s=size(self%sections),1,-1
        if (self%sections(s)%sname == trim(adjustl(section))) then
          ind = s
          exit
        endif
      enddo
    else
      do s=1,size(self%sections)
        if (self%sections(s)%sname == trim(adjustl(section))) then
          ind = s
          exit
        endif
      enddo
    endif
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction index_section_file_ini

  elemental function index_option_file_ini(self,back,section,option) result(ind)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for returning the index of the option (inside a  section) matching the name(s) passed.
  !<
  !< @note The matching index returned is the first found if *back* is not passed or if *back=.false.*. On the contrary the last
  !< found is returned if *back=.true.*.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_File_INI), intent(IN):: self    !< File data.
  logical, optional,    intent(IN):: back    !< If back appears with the value true, the last matching index is returned.
  character(*),         intent(IN):: option  !< Option  name.
  character(*),         intent(IN):: section !< Section name.
  integer(I4P)::                     ind     !< Index of searched section.
  logical::                          backd   !< Dummy back flag.
  integer(I4P)::                     s       !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  ind = 0
  if (allocated(self%sections)) then
    backd = .false. ; if (present(back)) backd = back
    s = self%index(section=section,back=backd)
    if (s>0) then
      ind = self%sections(s)%index(option=option,back=backd)
    endif
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction index_option_file_ini

  pure function section_file_ini(self, section_index) result(sname)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for getting section name once an index (valid) is provided.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_File_INI), intent(IN):: self          !< File data.
  integer(I4P),         intent(IN):: section_index !< Section index.
  character(len=:), allocatable::    sname         !< Section name.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(self%sections)) then
    if ((section_index >= lbound(self%sections,dim=1)).and.(section_index <= ubound(self%sections,dim=1))) then
      if (allocated(self%sections(section_index)%sname)) sname = self%sections(section_index)%sname
    endif
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction section_file_ini

  elemental function count_values_option_section_file_ini(self,delimiter,section,option) result(Nv)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for getting the number of values of option into section data.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_File_INI),   intent(IN):: self      !< Section data.
  character(*), optional, intent(IN):: delimiter !< Delimiter used for separating values.
  character(*),           intent(IN):: section   !< Section name.
  character(*),           intent(IN):: option    !< Option name.
  integer(I4P)::                       Nv        !< Number of values.
  character(len=:), allocatable::      dlm       !< Dummy string for delimiter handling.
  integer(I4P)::                       s         !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(self%sections)) then
    dlm = ' ' ; if (present(delimiter)) dlm = delimiter
    do s=1,size(self%sections)
      if (self%sections(s)%sname == trim(adjustl(section))) then
        Nv = self%sections(s)%count_values(delimiter=dlm,option=option)
        exit
      endif
    enddo
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction count_values_option_section_file_ini

  subroutine add_section_file_ini(self,error,section)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for adding a section.
  !<
  !< If the section already exists, it is left unchanged.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_File_INI),   intent(INOUT):: self        !< File data.
  integer(I4P), optional, intent(OUT)::   error       !< Error code.
  character(*),           intent(IN)::    section     !< Section name.
  type(Type_Section), allocatable::       sections(:) !< Temporary sections array.
  integer(I4P)::                          errd        !< Error code.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  errd = err_section
  if (allocated(self%sections)) then
    if (self%index(section=section)==0) then
      ! section not present
      allocate(sections(1:size(self%sections)+1))
      sections(1:size(self%sections)) = self%sections
      sections(size(self%sections)+1) = Type_Section(sname=trim(adjustl(section)))
      call move_alloc(sections,self%sections)
      self%Ns = self%Ns + 1
    endif
  else
    allocate(self%sections(1:1))
    self%sections(1)%sname = section
    self%Ns = self%Ns + 1
  endif
  if (self%index(section=section)>0) errd = 0
  if (present(error)) error = errd
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine add_section_file_ini

  subroutine add_option_section_file_ini(self,error,section,option,val)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for adding an option (with scalar value).
  !<
  !< If the option already exists, its value is updated.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_File_INI),   intent(INOUT):: self       !< File data.
  integer(I4P), optional, intent(OUT)::   error      !< Error code.
  character(*),           intent(IN)::    section    !< Section name.
  character(*),           intent(IN)::    option     !< Option name.
  class(*),               intent(IN)::    val        !< Option value.
  integer(I4P)::                          errd       !< Error code.
  integer(I4P)::                          s          !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  errd = err_section_options
  call self%add(section=section,error=errd)
  if (errd==0) then
    do s=1,size(self%sections)
      if (self%sections(s)%sname == section) then
        call self%sections(s)%add(error=errd,option=option,val=val)
        exit
      endif
    enddo
  endif
  if (present(error)) error = errd
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine add_option_section_file_ini

  subroutine add_a_option_section_file_ini(self,error,section,option,val)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for adding an option (with array value).
  !<
  !< If the option already exists, its value is updated.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_File_INI),   intent(INOUT):: self       !< File data.
  integer(I4P), optional, intent(OUT)::   error      !< Error code.
  character(*),           intent(IN)::    section    !< Section name.
  character(*),           intent(IN)::    option     !< Option name.
  class(*),               intent(IN)::    val(1:)    !< Option value.
  integer(I4P)::                          errd       !< Error code.
  integer(I4P)::                          s          !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  errd = err_section_options
  call self%add(section=section,error=errd)
  if (errd==0) then
    do s=1,size(self%sections)
      if (self%sections(s)%sname == section) then
        call self%sections(s)%add(error=errd,option=option,val=val)
        exit
      endif
    enddo
  endif
  if (present(error)) error = errd
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine add_a_option_section_file_ini

  subroutine get_option_section_file_ini(self,error,section,option,val)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for getting option value (scalar)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_File_INI),   intent(IN)::    self    !< Section data.
  integer(I4P), optional, intent(OUT)::   error   !< Error code.
  character(*),           intent(IN)::    section !< Section name.
  character(*),           intent(IN)::    option  !< Option name.
  class(*),               intent(INOUT):: val     !< Value.
  integer(I4P)::                          errd    !< Error code.
  integer(I4P)::                          s       !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(self%sections)) then
    do s=1,size(self%sections)
      if (self%sections(s)%sname == trim(adjustl(section))) then
        call self%sections(s)%get(error=errd,option=option,val=val)
        if (present(error)) error = errd
        exit
      endif
    enddo
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine get_option_section_file_ini

  subroutine get_a_option_section_file_ini(self,delimiter,error,section,option,val)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for getting option value (array)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_File_INI),   intent(IN)::    self      !< Section data.
  character(*), optional, intent(IN)::    delimiter !< Delimiter used for separating values.
  integer(I4P), optional, intent(OUT)::   error     !< Error code.
  character(*),           intent(IN)::    section   !< Section name.
  character(*),           intent(IN)::    option    !< Option name.
  class(*),               intent(INOUT):: val(1:)   !< Value.
  character(len=:), allocatable::         dlm       !< Dummy string for delimiter handling.
  integer(I4P)::                          errd      !< Error code.
  integer(I4P)::                          s         !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  dlm = ' ' ; if (present(delimiter)) dlm = delimiter
  if (allocated(self%sections)) then
    do s=1,size(self%sections)
      if (self%sections(s)%sname == trim(adjustl(section))) then
        call self%sections(s)%get(delimiter=dlm,error=errd,option=option,val=val)
        if (present(error)) error = errd
        exit
      endif
    enddo
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine get_a_option_section_file_ini

  pure function items_file_ini(self) result(items)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for getting list of couples option name/value.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_File_INI), intent(IN):: self       !< File data.
  character(len=:), allocatable::    items(:,:) !< Items, list of couples option name/value for all options [1:No,1:2].
  integer(I4P)::                     mx_chars   !< Maximum number of chars into name/value within all options.
  integer(I4P)::                     o,s,No     !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  mx_chars = MinI4P
  if (allocated(self%sections)) then
    No = 0
    do s=1,size(self%sections)
      if (allocated(self%sections(s)%options)) then
        do o=1,size(self%sections(s)%options)
          No = No + 1
          mx_chars = max(mx_chars,len(self%sections(s)%options(o)%oname),len(self%sections(s)%options(o)%ovals))
        enddo
      endif
    enddo
    if ((mx_chars > 0).and.(No > 0)) then
      allocate(character(mx_chars):: items(1:No,1:2))
      No = 0
      do s=1,size(self%sections)
        if (allocated(self%sections(s)%options)) then
          do o=1,size(self%sections(s)%options)
            No = No + 1
            items(No,1) = self%sections(s)%options(o)%oname
            items(No,2) = self%sections(s)%options(o)%ovals
          enddo
        endif
      enddo
    endif
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction items_file_ini

  function loop_options_section_file_ini(self,section,option) result(again)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for performing a while loop returning option name/value defined into section.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_File_INI),          intent(IN)::  self      !< File data.
  character(*),                  intent(IN)::  section   !< Section name.
  character(len=:), allocatable, intent(OUT):: option(:) !< Couples option name/value [1:2].
  logical::                                    again     !< Flag continuing the loop.
  integer(I4P)::                               s         !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  again = .false.
  s = self%index(section=section)
  if (s>0) then
    again = self%sections(s)%loop(option=option)
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction loop_options_section_file_ini

  recursive function loop_options_file_ini(self,option) result(again)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for performing a while loop returning option name/value defined into all sections.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_File_INI),          intent(IN)::  self           !< File data.
  character(len=:), allocatable, intent(OUT):: option(:)      !< Couples option name/value [1:2].
  logical::                                    again          !< Flag continuing the loop.
  logical, save::                              againO=.false. !< Flag continuing the loop.
  integer(I4P), save::                         s=0            !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  again = .false.
  if (allocated(self%sections)) then
    if (s==0) then
      s = lbound(self%sections,dim=1)
      againO = self%loop(section=self%sections(s)%sname,option=option)
      again = .true.
    elseif (s<ubound(self%sections,dim=1)) then
      if (.not.againO) s = s + 1
      againO = self%loop(section=self%sections(s)%sname,option=option)
      if (.not.againO) then
        again = self%loop(option=option)
      else
        again = .true.
      endif
    else
      s = 0
      againO = .false.
      again = .false.
    endif
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction loop_options_file_ini

  subroutine print_file_ini(self,pref,retain_comments,iostat,iomsg,unit)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for printing data with a pretty format.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_File_INI),   intent(IN)::  self            !< File data.
  character(*), optional, intent(IN)::  pref            !< Prefixing string.
  logical,      optional, intent(IN)::  retain_comments !< Flag for retaining eventual comments.
  integer(I4P), optional, intent(OUT):: iostat          !< IO error.
  character(*), optional, intent(OUT):: iomsg           !< IO error message.
  integer(I4P),           intent(IN)::  unit            !< Logic unit.
  character(len=:), allocatable::       prefd           !< Prefixing string.
  logical::                             rt_comm         !< Flag for retaining eventual comments.
  integer(I4P)::                        iostatd         !< IO error.
  character(500)::                      iomsgd          !< Temporary variable for IO error message.
  integer(I4P)::                        s               !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  prefd = '' ; if (present(pref)) prefd = pref
  rt_comm = .false. ; if (present(retain_comments)) rt_comm = retain_comments
  if (allocated(self%sections)) then
    do s=1,size(self%sections)
      call self%sections(s)%print(pref=prefd,iostat=iostatd,iomsg=iomsgd,unit=unit,retain_comments=rt_comm)
    enddo
  endif
  if (present(iostat)) iostat = iostatd
  if (present(iomsg))  iomsg  = iomsgd
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine print_file_ini

  subroutine save_file_ini(self,retain_comments,iostat,iomsg,filename)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for saving data.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_File_INI),   intent(INOUT):: self            !< File data.
  logical,      optional, intent(IN)::    retain_comments !< Flag for retaining eventual comments.
  integer(I4P), optional, intent(OUT)::   iostat          !< IO error.
  character(*), optional, intent(OUT)::   iomsg           !< IO error message.
  character(*), optional, intent(IN)::    filename        !< File name.
  logical::                               rt_comm         !< Flag for retaining eventual comments.
  integer(I4P)::                          unit            !< Logic unit.
  integer(I4P)::                          iostatd         !< IO error.
  character(500)::                        iomsgd          !< Temporary variable for IO error message.
  integer(I4P)::                          s               !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  rt_comm = .false. ; if (present(retain_comments)) rt_comm = retain_comments
  if (present(filename)) self%filename = filename
  if (allocated(self%filename).and.allocated(self%sections)) then
    open(unit=Get_Unit(unit),file=self%filename,action='WRITE',iostat=iostatd,iomsg=iomsgd)
    do s=1,size(self%sections)
      call self%sections(s)%save(iostat=iostatd,iomsg=iomsgd,unit=unit,retain_comments=rt_comm)
    enddo
    close(unit=unit,iostat=iostatd,iomsg=iomsgd)
  endif
  if (present(iostat)) iostat = iostatd
  if (present(iomsg))  iomsg  = iomsgd
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine save_file_ini

  elemental subroutine assign_file_ini(self1,self2)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for assignment between two selfs.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_File_INI), intent(INOUT):: self1 !< Left hand side.
  type(Type_File_INI),  intent(IN)::    self2 !< Rigth hand side.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(self2%filename)) self1%filename = self2%filename
  if (allocated(self2%sections)) then
    if (allocated(self1%sections)) deallocate(self1%sections) ; allocate(self1%sections(1:size(self2%sections)))
    self1%sections = self2%sections
  endif
  self1%Ns = self2%Ns
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine assign_file_ini

  subroutine ini_autotest()
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for autotesting the library functionalities.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  type(Type_File_INI)::           fini       !< INI File.
  character(len=:), allocatable:: source     !< Testing string.
  character(len=:), allocatable:: string     !< String option.
  real(R4P), allocatable::        array(:)   !< Array option.
  integer(I4P)::                  error      !< Error code.
  character(len=:), allocatable:: items(:,:) !< List of all options name/value couples.
  character(len=:), allocatable:: item(:)    !< Option name/value couple.
  integer(I4P)::                  i,s        !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  source='[section-1]'//new_line('A')//   &
         'option-1 = one'//new_line('A')//&
         'option-2 = 2.'//new_line('A')// &
         '           3. ; this is an inline comment'//new_line('A')// &
         'option-3 = bar ; this is an inline comment'//new_line('A')//&
         '[section-2]'//new_line('A')//   &
         'option-1 = foo'
  print "(A)", ''
  print "(A)", "Testing parsing procedures"
  print "(A)", ''
  print "(A)", "Source to be parsed:"
  print "(A)", source
  call fini%load(source=source)
  print "(A)", ''
  print "(A)", "Result of parsing:"
  string = '   '
  call fini%get(section='section-1',option='option-1',val=string,error=error)
  if (error==0) print "(A,A)", '  option-1 of section-1 has values: ',string
  allocate(array(1:fini%count_values(section='section-1',option='option-2')))
  call fini%get(section='section-1',option='option-2',val=array,error=error)
  if (error==0) print "(A,3(F4.1,1X))", '  option-2 of section-1 has values: ',array
  call fini%get(section='section-1',option='option-3',val=string,error=error)
  if (error==0) print "(A,A)", '  option-3 of section-1 has values: ',string
  call fini%get(section='section-2',option='option-1',val=string,error=error)
  if (error==0) print "(A,A)", '  option-1 of section-2 has values: ',string
  print "(A)", ''
  print "(A)", "Parsed data will be saved as (having retained inline comments that are trimmed out by default):"
  call fini%print(pref='  ',unit=stdout,retain_comments=.true.)
  call fini%save(filename='foo.ini',retain_comments=.true.)
  call fini%free
  print "(A)", ''
  print "(A)", "Testing generating procedures"
  call fini%add(section='sec-foo')
  call fini%add(section='sec-foo',option='bar',val=-32.1_R8P)
  call fini%add(section='sec-foo',option='baz',val=' hello FiNeR! ')
  call fini%add(section='sec-foo',option='array',val=[1,2,3,4])
  call fini%add(section='sec-bar')
  call fini%add(section='sec-bar',option='bools',val=[.true.,.false.,.false.])
  call fini%add(section='sec-bartolomeo')
  call fini%add(section='sec-bartolomeo',option='help',val='I am Bartolomeo')
  print "(A)", "The autogenerated INI file will be saved as:"
  call fini%print(pref='  ',unit=stdout)
  print "(A)", ''
  print "(A)", "Testing removing option baz"
  call fini%del(section='sec-foo',option='baz')
  call fini%print(pref='  ',unit=stdout)
  print "(A)", ''
  print "(A)", "Testing removing section sec-bar"
  call fini%del(section='sec-bar')
  call fini%print(pref='  ',unit=stdout)
  print "(A)", ''
  print "(A)", "Testing introspective methods"
  print "(A,L1)", "Is there option bar? ", fini%has_option(option='bar')
  print "(A,L1)", "Is there option baz? ", fini%has_option(option='baz')
  print "(A,L1)", "Is there section sec-bar? ", fini%has_section(section='sec-bar')
  print "(A,L1)", "Is there section sec-foo? ", fini%has_section(section='sec-foo')
  print "(A)", ''
  print "(A)", "What are all options name/values couples? Can I have a list? Yes, you can:"
  items = fini%items()
  do i=1,size(items,dim=1)
    print "(A)", trim(items(i,1))//' = '//trim(items(i,2))
  enddo
  print "(A)", ''
  print "(A)", "Testing loop method over options of a section:"
  do s=1,fini%Ns
    print "(A)", fini%section(s)
    do while(fini%loop(section=fini%section(s),option=item))
      print "(A)", '  '//trim(item(1))//' = '//trim(item(2))
    enddo
  enddo
  print "(A)", ''
  print "(A)", "Testing loop method over all options:"
  do while(fini%loop(option=item))
    print "(A)", '  '//trim(item(1))//' = '//trim(item(2))
  enddo
  print "(A)", ''
  print "(A)", "Testing custom separator of option name/value:, use ':' instead of '='"
  source='[section-1]'//new_line('A')//   &
         'option-1 : one'//new_line('A')//&
         'option-2 : 2.'//new_line('A')// &
         '           3.'//new_line('A')// &
         'option-3 : bar'//new_line('A')//&
         '[section-2]'//new_line('A')//   &
         'option-1 : foo'
  print "(A)", ''
  print "(A)", "Source to be parsed:"
  print "(A)", source
  call fini%free
  call fini%load(separator=':',source=source)
  print "(A)", ''
  print "(A)", "Result of parsing:"
  call fini%print(pref='  ',unit=stdout)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine ini_autotest
endmodule Lib_INI_IO
