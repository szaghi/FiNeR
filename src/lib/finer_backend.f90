!< FiNeR backends: globals definition.
module finer_backend
!-----------------------------------------------------------------------------------------------------------------------------------
!< FiNeR backends: globals definition.
!-----------------------------------------------------------------------------------------------------------------------------------
use penf
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
save
private
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
integer(I4P), parameter, public :: err_option_name     = 1     !< Error flag for trapping errors in option name.
integer(I4P), parameter, public :: err_option_vals     = 2     !< Error flag for trapping errors in option values.
integer(I4P), parameter, public :: err_option          = 3     !< Error flag for trapping errors in option.
integer(I4P), parameter, public :: err_section_name    = 4     !< Error flag for trapping errors in section name.
integer(I4P), parameter, public :: err_section_options = 5     !< Error flag for trapping errors in section options.
integer(I4P), parameter, public :: err_section         = 6     !< Error flag for trapping errors in section.
integer(I4P), parameter, public :: err_source_missing  = 7     !< Error flag for trapping errors in file when source is missing.
character(1), parameter, public :: def_opt_sep         = '='   !< Default separator of option name/value.
character(*), parameter, public :: comments            = "!;#" !< Characters used for defining a comment line.
character(1), parameter, public :: inline_comment      = ';'   !< Inline comment delimiter.
!-----------------------------------------------------------------------------------------------------------------------------------
endmodule finer_backend
