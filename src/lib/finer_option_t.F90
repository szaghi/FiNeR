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
  private
  type(string) :: oname !< Option name.
  type(string) :: ovals !< Option values.
  type(string) :: ocomm !< Eventual option inline comment.
  contains
    ! public methods
    procedure, pass(self) :: count_values          !< Counting option value(s).
    procedure, pass(self) :: free                  !< Free dynamic memory.
    generic               :: get => get_option, &  !< Get option value (scalar).
                                    get_a_option   !< Get option value (array).
    procedure, pass(self) :: get_pairs             !< Return option name/values pairs.
    procedure, pass(self) :: name_len              !< Return option name length.
    procedure, pass(self) :: parse                 !< Parse option data.
    procedure, pass(self) :: print => print_option !< Pretty print data.
    procedure, pass(self) :: save  => save_option  !< Save data.
    generic               :: set => set_option, &  !< Set option value (scalar).
                                    set_a_option   !< Set option value (array).
    procedure, pass(self) :: values_len            !< Return option values length.
    ! operators overloading
    generic :: assignment(=) => assign_option      !< Assignment overloading.
    generic :: operator(==) => option_eq_string, &
                               option_eq_character !< Equal operator overloading.
    ! private methods
    procedure, private, pass(self) :: get_option    !< Get option value (scalar).
    procedure, private, pass(self) :: get_a_option  !< Get option value (array).
    procedure, private, pass(self) :: parse_comment !< Parse option inline comment.
    procedure, private, pass(self) :: parse_name    !< Parse option name.
    procedure, private, pass(self) :: parse_value   !< Parse option values.
    procedure, private, pass(self) :: set_option    !< Set option value (scalar).
    procedure, private, pass(self) :: set_a_option  !< Set option value (array).
    ! assignments
    procedure, private, pass(lhs) :: assign_option !< Assignment overloading.
    ! logical operators
    procedure, private, pass(lhs) :: option_eq_string    !< Equal to string logical operator.
    procedure, private, pass(lhs) :: option_eq_character !< Equal to character logical operator.
endtype option

interface option
  !< Overload `option` name with a function returning a new (itiliazed) option instance.
  module procedure new_option
endinterface option
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  ! public methods
  elemental function count_values(self, delimiter) result(Nv)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Get the number of values of option data.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(option),          intent(in) :: self      !< Option data.
  character(*), optional, intent(in) :: delimiter !< Delimiter used for separating values.
  character(len=:), allocatable      :: dlm       !< Dummy string for delimiter handling.
  integer(I4P)                       :: Nv        !< Number of values.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%ovals%is_allocated()) then
    dlm = ' ' ; if (present(delimiter)) dlm = delimiter
    Nv = self%ovals%count(dlm) + 1
  else
    Nv = 0
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction count_values

  elemental subroutine free(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Free dynamic memory.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(option), intent(inout) :: self !< Option data.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%oname%free
  call self%ovals%free
  call self%ocomm%free
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine free

  pure subroutine get_pairs(self, pairs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return option name/values pairs.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(option),                 intent(in)  :: self     !< Option data.
  character(len=:), allocatable, intent(out) :: pairs(:) !< Option name/values pairs.
  integer(I4P)                               :: Nc       !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  Nc = max(self%oname%len(), self%ovals%len())
  allocate(character(Nc):: pairs(1:2))
  pairs(1) = self%oname%chars()
  pairs(2) = self%ovals%chars()
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine get_pairs

  elemental function name_len(self) result(length)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return option name length.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(option), intent(in) :: self   !< Option data.
  integer                   :: length !< Option name length.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%oname%is_allocated()) length = self%oname%len()
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction name_len

  elemental function values_len(self) result(length)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return option values length.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(option), intent(in) :: self   !< Option data.
  integer                   :: length !< Option values length.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%ovals%is_allocated()) length = self%ovals%len()
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction values_len

  elemental subroutine parse(self, sep, source, error)
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
  endsubroutine parse

  ! private methods
  subroutine get_option(self, val, error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< for getting option data value (scalar).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(option),          intent(in)    :: self   !< Option data.
  class(*),               intent(inout) :: val    !< Value.
  integer(I4P), optional, intent(out)   :: error  !< Error code.
  integer(I4P)                          :: errd   !< Error code.
  character(len=:), allocatable         :: buffer !< Dummy buffer.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  errd = err_option_vals
  if (self%ovals%is_allocated()) then
    errd = 0
    select type(val)
#ifdef r16p
    type is(real(R16P))
      val = self%ovals%to_number(kind=1._R16P)
#endif
    type is(real(R8P))
      val = self%ovals%to_number(kind=1._R8P)
    type is(real(R4P))
      val = self%ovals%to_number(kind=1._R4P)
    type is(integer(I8P))
      val = self%ovals%to_number(kind=1_I8P)
    type is(integer(I4P))
      val = self%ovals%to_number(kind=1_I4P)
    type is(integer(I2P))
      val = self%ovals%to_number(kind=1_I2P)
    type is(integer(I1P))
      val = self%ovals%to_number(kind=1_I1P)
    type is(logical)
      buffer = self%ovals%chars()
      read(buffer, *)val
    type is(character(*))
      val = self%ovals%chars()
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
  type(string), allocatable                   :: valsV(:)  !< String array of values.
  integer(I4P)                                :: errd      !< Error code.
  character(len=:), allocatable               :: buffer    !< Dummy buffer.
  integer(I4P)                                :: v         !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  errd = err_option_vals
  dlm = ' ' ; if (present(delimiter)) dlm = delimiter
  if (self%ovals%is_allocated()) then
    errd = 0
    call self%ovals%split(tokens=valsV, sep=dlm)
    Nv = size(valsV, dim=1)
    select type(val)
#ifdef r16p
    type is(real(R16P))
      do v=1, Nv
        val(v) = valsV(v)%to_number(kind=1._R16P)
      enddo
#endif
    type is(real(R8P))
      do v=1, Nv
        val(v) = valsV(v)%to_number(kind=1._R8P)
      enddo
    type is(real(R4P))
      do v=1, Nv
        val(v) = valsV(v)%to_number(kind=1._R4P)
      enddo
    type is(integer(I8P))
      do v=1, Nv
        val(v) = valsV(v)%to_number(kind=1_I8P)
      enddo
    type is(integer(I4P))
      do v=1, Nv
        val(v) = valsV(v)%to_number(kind=1_I4P)
      enddo
    type is(integer(I2P))
      do v=1, Nv
        val(v) = valsV(v)%to_number(kind=1_I2P)
      enddo
    type is(integer(I1P))
      do v=1, Nv
        val(v) = valsV(v)%to_number(kind=1_I1P)
      enddo
    type is(logical)
      do v=1, Nv
        buffer = valsV(v)%chars()
        read(buffer, *)val(v)
      enddo
    type is(character(*))
      do v=1, Nv
        val(v) = valsV(v)%chars()
      enddo
    endselect
  endif
  if (present(error)) error = errd
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine get_a_option

  elemental subroutine parse_comment(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Parse eventaul option inline comment trimming it out from pure value string.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(option), intent(inout) :: self !< Option data.
  integer(I4P)                 :: pos  !< Characters counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%ovals%is_allocated()) then
    pos = self%ovals%index(inline_comment)
    if (pos>0) then
      if (pos<self%ovals%len()) self%ocomm = trim(adjustl(self%ovals%slice(pos+1, self%ovals%len())))
      self%ovals = trim(adjustl(self%ovals%slice(1, pos-1)))
    endif
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine parse_comment

  elemental subroutine parse_name(self, sep, source, error)
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
  endsubroutine parse_name

  elemental subroutine parse_value(self, sep, source, error)
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
  endsubroutine parse_value

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
  if (self%oname%is_allocated()) then
    prefd = '' ; if (present(pref)) prefd = pref
    comment = '' ; if (self%ocomm%is_allocated().and.retain_comments) comment = ' ; '//self%ocomm
    if (self%ovals%is_allocated()) then
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

  pure subroutine set_option(self, val)
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
    self%ovals = val
#endif
  type is(real(R8P))
    self%ovals = val
  type is(real(R4P))
    self%ovals = val
  type is(integer(I8P))
    self%ovals = val
  type is(integer(I4P))
    self%ovals = val
  type is(integer(I2P))
    self%ovals = val
  type is(integer(I1P))
    self%ovals = val
  type is(logical)
    self%ovals = trim(str(n=val))
  type is(character(*))
    self%ovals = val
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine set_option

  pure subroutine set_a_option(self, val, delimiter)
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
    self%ovals = self%ovals%strip()
#endif
  type is(real(R8P))
    do v=1, size(val, dim=1)
      self%ovals = self%ovals//dlm//trim(str(n=val(v)))
    enddo
    self%ovals = self%ovals%strip()
  type is(real(R4P))
    do v=1, size(val, dim=1)
      self%ovals = self%ovals//dlm//trim(str(n=val(v)))
    enddo
    self%ovals = self%ovals%strip()
  type is(integer(I8P))
    do v=1, size(val, dim=1)
      self%ovals = self%ovals//dlm//trim(str(n=val(v)))
    enddo
    self%ovals = self%ovals%strip()
  type is(integer(I4P))
    do v=1, size(val, dim=1)
      self%ovals = self%ovals//dlm//trim(str(n=val(v)))
    enddo
    self%ovals = self%ovals%strip()
  type is(integer(I2P))
    do v=1, size(val, dim=1)
      self%ovals = self%ovals//dlm//trim(str(n=val(v)))
    enddo
    self%ovals = self%ovals%strip()
  type is(integer(I1P))
    do v=1, size(val, dim=1)
      self%ovals = self%ovals//dlm//trim(str(n=val(v)))
    enddo
    self%ovals = self%ovals%strip()
  type is(logical)
    do v=1, size(val, dim=1)
      self%ovals = self%ovals//dlm//trim(str(n=val(v)))
    enddo
    self%ovals = self%ovals%strip()
  type is(character(*))
    do v=1, size(val, dim=1)
      self%ovals = self%ovals//dlm//trim(val(v))
    enddo
    self%ovals = self%ovals%strip()
  endselect
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine set_a_option

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
  if (self%oname%is_allocated()) then
    comment = '' ; if (self%ocomm%is_allocated().and.retain_comments) comment = ' ; '//self%ocomm
    if (self%ovals%is_allocated()) then
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

  ! assignments
  elemental subroutine assign_option(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Assignment between two options.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(option), intent(inout) :: lhs !< Left hand side.
  type(option),  intent(in)    :: rhs !< Rigth hand side.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (rhs%oname%is_allocated()) lhs%oname = rhs%oname
  if (rhs%ovals%is_allocated()) lhs%ovals = rhs%ovals
  if (rhs%ocomm%is_allocated()) lhs%ocomm = rhs%ocomm
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine assign_option

  ! logical operators
  elemental function option_eq_string(lhs, rhs) result(is_it)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Equal to string logical operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(option), intent(in) :: lhs   !< Left hand side.
  type(string),  intent(in) :: rhs   !< Right hand side.
  logical                   :: is_it !< Opreator test result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  is_it = lhs%oname == rhs
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction option_eq_string

  elemental function option_eq_character(lhs, rhs) result(is_it)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Equal to character logical operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(option),             intent(in) :: lhs   !< Left hand side.
  character(kind=CK, len=*), intent(in) :: rhs   !< Right hand side.
  logical                               :: is_it !< Opreator test result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  is_it = lhs%oname == rhs
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction option_eq_character

  ! non TBP methods
  elemental function new_option(option_name, option_values, option_comment)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return a new (initiliazed) option instance.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(*), intent(in), optional :: option_name    !< Option name.
  character(*), intent(in), optional :: option_values  !< Option values.
  character(*), intent(in), optional :: option_comment !< Option comment.
  type(option)                       :: new_option     !< New (initiliazed) option instance.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (present(option_name   )) new_option%oname = option_name
  if (present(option_values )) new_option%ovals = option_values
  if (present(option_comment)) new_option%ocomm = option_comment
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction new_option
endmodule finer_option_t
