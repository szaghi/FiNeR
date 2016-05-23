!< Option class definition.
module finer_option_t
!-----------------------------------------------------------------------------------------------------------------------------------
!< Option class definition.
!-----------------------------------------------------------------------------------------------------------------------------------
use finer_backend
use penf
use stringifor
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: option
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type :: option
  !< Option data of sections.
  character(len=:), allocatable :: oname !< Option name.
  character(len=:), allocatable :: ovals !< Option values.
  character(len=:), allocatable :: ocomm !< Eventual option inline comment.
  contains
    ! public methods
    procedure :: free         => free_option         !< Free dynamic memory.
    procedure :: parse        => parse_option        !< parse option data.
    procedure :: count_values => count_values_option !< Counting option value(s).
    generic ::   set          => set_option, &       !< Set option value (scalar).
                                 set_a_option        !< Set option value (array).
    generic ::   get          => get_option, &       !< Get option value (scalar).
                                 get_a_option        !< Get option value (array).
    procedure :: print        => print_option        !< Pretty print data.
    procedure :: save         => save_option         !< Save data.
    ! operators overloading
    generic :: assignment(=) => assign_option !< Assignment overloading.
    ! private methods
    procedure, private            :: parse_name    => parse_name_option    !< Parse option name.
    procedure, private            :: parse_value   => parse_value_option   !< Parse option values.
    procedure, private            :: parse_comment => parse_comment_option !< Parse option inline comment.
    procedure, private            :: set_option                            !< Set option value (scalar).
    procedure, private            :: set_a_option                          !< Set option value (array).
    procedure, private            :: get_option                            !< Get option value (scalar).
    procedure, private            :: get_a_option                          !< Get option value (array).
    procedure, private, pass(lhs) :: assign_option                         !< Assignment overloading.
endtype option
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  elemental subroutine free_option(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Free dynamic memory.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(option), intent(inout) :: self !< Option data.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(self%oname)) deallocate(self%oname)
  if (allocated(self%ovals)) deallocate(self%ovals)
  if (allocated(self%ocomm)) deallocate(self%ocomm)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine free_option

  elemental subroutine parse_name_option(self, sep, source, error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Parse option name from a source string.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(option), intent(inout) :: self   !< Option data.
  character(*),  intent(in)    :: sep    !< Separator of option name/value.
  character(*),  intent(in)    :: source !< String containing option data.
  integer(I4P),  intent(out)   :: error  !< Error code.
  integer(I4P)                 :: pos    !< Characters counter.
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

  elemental subroutine parse_value_option(self, sep, source, error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Parse option value from a source string.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(option), intent(inout) :: self   !< Option data.
  character(*),  intent(in)    :: sep    !< Separator of option name/value.
  character(*),  intent(in)    :: source !< String containing option data.
  integer(I4P),  intent(out)   :: error  !< Error code.
  integer(I4P)                 :: pos    !< Characters counter.
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
  !< Parse eventaul option inline comment trimming it out from pure value string.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(option), intent(inout) :: self !< Option data.
  integer(I4P)                 :: pos  !< Characters counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(self%ovals)) then
    pos = index(self%ovals, inline_comment)
    if (pos>0) then
      if (pos<len(self%ovals)) self%ocomm = trim(adjustl(self%ovals(pos+1:)))
      self%ovals = trim(adjustl(self%ovals(:pos-1)))
    endif
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine parse_comment_option

  elemental subroutine parse_option(self, sep, source, error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Parse option data from a source string.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(option), intent(inout) :: self   !< Option data.
  character(*),  intent(in)    :: sep    !< Separator of option name/value.
  character(*),  intent(in)    :: source !< String containing option data.
  integer(I4P),  intent(out)   :: error  !< Error code.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  error = err_option
  if (scan(adjustl(source), comments) == 1) return
  call self%parse_name(sep=sep, source=source, error=error)
  call self%parse_value(sep=sep, source=source, error=error)
  call self%parse_comment
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine parse_option

  elemental function count_values_option(self, delimiter) result(Nv)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Get the number of values of option data.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(option),          intent(in) :: self      !< Option data.
  character(*), optional, intent(in) :: delimiter !< Delimiter used for separating values.
  character(len=:), allocatable      :: dlm       !< Dummy string for delimiter handling.
  integer(I4P)                       :: Nv        !< Number of values.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(self%ovals)) then
    dlm = ' ' ; if (present(delimiter)) dlm = delimiter
    Nv = count(string_=self%ovals, substring=dlm) + 1
  else
    Nv = 0
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction count_values_option

  subroutine set_option(self, val)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Set option data value (scalar).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(option), intent(inout) :: self !< Option data.
  class(*),      intent(in)    :: val  !< Value.
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

  subroutine set_a_option(self, val, delimiter)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Set option data value (array).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(option),          intent(inout) :: self      !< Option data.
  class(*),               intent(in)    :: val(1:)   !< Value.
  character(*), optional, intent(in)    :: delimiter !< Delimiter used for separating values.
  character(len=:), allocatable         :: dlm       !< Dummy string for delimiter handling.
  integer(I4P)                          :: v         !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  dlm = ' ' ; if (present(delimiter)) dlm = delimiter
  self%ovals = ''
  select type(val)
#ifdef r16p
  type is(real(R16P))
    do v=1, size(val, dim=1)
      self%ovals = self%ovals//dlm//trim(str(n=val(v)))
    enddo
    self%ovals = trim(adjustl(self%ovals))
#endif
  type is(real(R8P))
    do v=1, size(val, dim=1)
      self%ovals = self%ovals//dlm//trim(str(n=val(v)))
    enddo
    self%ovals = trim(adjustl(self%ovals))
  type is(real(R4P))
    do v=1, size(val, dim=1)
      self%ovals = self%ovals//dlm//trim(str(n=val(v)))
    enddo
    self%ovals = trim(adjustl(self%ovals))
  type is(integer(I8P))
    do v=1, size(val, dim=1)
      self%ovals = self%ovals//dlm//trim(str(n=val(v)))
    enddo
    self%ovals = trim(adjustl(self%ovals))
  type is(integer(I4P))
    do v=1, size(val, dim=1)
      self%ovals = self%ovals//dlm//trim(str(n=val(v)))
    enddo
    self%ovals = trim(adjustl(self%ovals))
  type is(integer(I2P))
    do v=1, size(val, dim=1)
      self%ovals = self%ovals//dlm//trim(str(n=val(v)))
    enddo
    self%ovals = trim(adjustl(self%ovals))
  type is(integer(I1P))
    do v=1, size(val, dim=1)
      self%ovals = self%ovals//dlm//trim(str(n=val(v)))
    enddo
    self%ovals = trim(adjustl(self%ovals))
  type is(logical)
    do v=1, size(val, dim=1)
      self%ovals = self%ovals//dlm//trim(str(n=val(v)))
    enddo
    self%ovals = trim(adjustl(self%ovals))
  type is(character(*))
    do v=1, size(val, dim=1)
      self%ovals = self%ovals//dlm//trim(val(v))
    enddo
    self%ovals = trim(adjustl(self%ovals))
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine set_a_option

  subroutine get_option(self, val, error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< for getting option data value (scalar).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(option),          intent(in)    :: self   !< Option data.
  class(*),               intent(inout) :: val    !< Value.
  integer(I4P), optional, intent(out)   :: error  !< Error code.
  integer(I4P)                          :: errd   !< Error code.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  errd = err_option_vals
  if (allocated(self%ovals)) then
    errd = 0
    select type(val)
#ifdef r16p
    type is(real(R16P))
      val = cton(str=trim(adjustl(self%ovals)), knd=1._R16P)
#endif
    type is(real(R8P))
      val = cton(str=trim(adjustl(self%ovals)), knd=1._R8P)
    type is(real(R4P))
      val = cton(str=trim(adjustl(self%ovals)), knd=1._R4P)
    type is(integer(I8P))
      val = cton(str=trim(adjustl(self%ovals)), knd=1_I8P)
    type is(integer(I4P))
      val = cton(str=trim(adjustl(self%ovals)), knd=1_I4P)
    type is(integer(I2P))
      val = cton(str=trim(adjustl(self%ovals)), knd=1_I2P)
    type is(integer(I1P))
      val = cton(str=trim(adjustl(self%ovals)), knd=1_I1P)
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

  subroutine get_a_option(self, val, delimiter, error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Get option data values (array).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(option),          intent(in)          :: self      !< Option data.
  class(*),               intent(inout)       :: val(1:)   !< Value.
  character(*), optional, intent(in)          :: delimiter !< Delimiter used for separating values.
  integer(I4P), optional, intent(out)         :: error     !< Error code.
  character(len=:), allocatable               :: dlm       !< Dummy string for delimiter handling.
  integer(I4P)                                :: Nv        !< Number of values.
  character(len=len(self%ovals)), allocatable :: valsV(:)  !< String array of values.
  type(string), allocatable                   :: tokens(:) !< Options strings tokenized.
  type(string)                                :: dummy_str !< Dummy string.
  integer(I4P)                                :: errd      !< Error code.
  integer(I4P)                                :: v         !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  errd = err_option_vals
  dlm = ' ' ; if (present(delimiter)) dlm = delimiter
  if (allocated(self%ovals)) then
    errd = 0
    ! to remove after StringiFor adoption
    dummy_str = self%ovals
    call dummy_str%split(tokens=tokens, sep=dlm)
    allocate(valsV(1:size(tokens, dim=1)))
    Nv = size(tokens, dim=1)
    do v =1, Nv
      valsV(v) = tokens(v)%chars()
    enddo
    ! call tokenize(strin=trim(adjustl(self%ovals)), delimiter=dlm, Nt=Nv, toks=valsV)
    ! to remove after StringiFor adoption
    select type(val)
#ifdef r16p
    type is(real(R16P))
      do v=1, Nv
        val(v) = cton(str=trim(adjustl(valsV(v))), knd=1._R16P)
      enddo
#endif
    type is(real(R8P))
      do v=1, Nv
        val(v) = cton(str=trim(adjustl(valsV(v))), knd=1._R8P)
      enddo
    type is(real(R4P))
      do v=1, Nv
        val(v) = cton(str=trim(adjustl(valsV(v))), knd=1._R4P)
      enddo
    type is(integer(I8P))
      do v=1, Nv
        val(v) = cton(str=trim(adjustl(valsV(v))), knd=1_I8P)
      enddo
    type is(integer(I4P))
      do v=1, Nv
        val(v) = cton(str=trim(adjustl(valsV(v))), knd=1_I4P)
      enddo
    type is(integer(I2P))
      do v=1, Nv
        val(v) = cton(str=trim(adjustl(valsV(v))), knd=1_I2P)
      enddo
    type is(integer(I1P))
      do v=1, Nv
        val(v) = cton(str=trim(adjustl(valsV(v))), knd=1_I1P)
      enddo
    type is(logical)
      do v=1, Nv
        read(valsV(v),*)val(v)
      enddo
    type is(character(*))
      do v=1, Nv
        val(v)=valsV(v)
      enddo
    endselect
  endif
  if (present(error)) error = errd
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine get_a_option

  subroutine print_option(self, unit, retain_comments, pref, iostat, iomsg)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Print data with a pretty format.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(option),          intent(in)  :: self            !< Option data.
  integer(I4P),           intent(in)  :: unit            !< Logic unit.
  logical,                intent(in)  :: retain_comments !< Flag for retaining eventual comments.
  character(*), optional, intent(in)  :: pref            !< Prefixing string.
  integer(I4P), optional, intent(out) :: iostat          !< IO error.
  character(*), optional, intent(out) :: iomsg           !< IO error message.
  character(len=:), allocatable       :: prefd           !< Prefixing string.
  integer(I4P)                        :: iostatd         !< IO error.
  character(500)                      :: iomsgd          !< Temporary variable for IO error message.
  character(len=:), allocatable       :: comment         !< Eventual option comments.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(self%oname)) then
    prefd = '' ; if (present(pref)) prefd = pref
    comment = '' ; if (allocated(self%ocomm).and.retain_comments) comment = ' ; '//self%ocomm
    if (allocated(self%ovals)) then
      write(unit=unit, fmt='(A)', iostat=iostatd, iomsg=iomsgd)prefd//self%oname//' = '//self%ovals//comment
    else
      write(unit=unit, fmt='(A)', iostat=iostatd, iomsg=iomsgd)prefd//self%oname//' = '//comment
    endif
    if (present(iostat)) iostat = iostatd
    if (present(iomsg))  iomsg  = iomsgd
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine print_option

  subroutine save_option(self, unit, retain_comments, iostat, iomsg)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Save data.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(option),          intent(in)  :: self            !< Option data.
  integer(I4P),           intent(in)  :: unit            !< Logic unit.
  logical,                intent(in)  :: retain_comments !< Flag for retaining eventual comments.
  integer(I4P), optional, intent(out) :: iostat          !< IO error.
  character(*), optional, intent(out) :: iomsg           !< IO error message.
  integer(I4P)                        :: iostatd         !< IO error.
  character(500)                      :: iomsgd          !< Temporary variable for IO error message.
  character(len=:), allocatable       :: comment         !< Eventual option comments.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(self%oname)) then
    comment = '' ; if (allocated(self%ocomm).and.retain_comments) comment = ' ; '//self%ocomm
    if (allocated(self%ovals)) then
      write(unit=unit, fmt='(A)', iostat=iostatd, iomsg=iomsgd)self%oname//' = '//self%ovals//comment
    else
      write(unit=unit, fmt='(A)', iostat=iostatd, iomsg=iomsgd)self%oname//' = '//comment
    endif
    if (present(iostat)) iostat = iostatd
    if (present(iomsg))  iomsg  = iomsgd
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine save_option

  elemental subroutine assign_option(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Assignment between two options.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(option), intent(inout) :: lhs !< Left hand side.
  type(option),  intent(in)    :: rhs !< Rigth hand side.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(rhs%oname)) lhs%oname = rhs%oname
  if (allocated(rhs%ovals)) lhs%ovals = rhs%ovals
  if (allocated(rhs%ocomm)) lhs%ocomm = rhs%ocomm
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine assign_option
endmodule finer_option_t
