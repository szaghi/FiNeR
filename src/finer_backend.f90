!< FiNeR backends: globals definition.
module finer_backend
!< FiNeR backends: globals definition.
use penf

implicit none
save
private

integer(I4P), parameter, public :: ERR_OPTION_NAME     = 1     !< Error flag for trapping errors in option name.
integer(I4P), parameter, public :: ERR_OPTION_VALS     = 2     !< Error flag for trapping errors in option values.
integer(I4P), parameter, public :: ERR_OPTION          = 3     !< Error flag for trapping errors in option.
integer(I4P), parameter, public :: ERR_SECTION_NAME    = 4     !< Error flag for trapping errors in section name.
integer(I4P), parameter, public :: ERR_SECTION_OPTIONS = 5     !< Error flag for trapping errors in section options.
integer(I4P), parameter, public :: ERR_SECTION         = 6     !< Error flag for trapping errors in section.
integer(I4P), parameter, public :: ERR_SOURCE_MISSING  = 7     !< Error flag for trapping errors in file when source is missing.
character(1), parameter, public :: DEF_OPT_SEP         = '='   !< Default separator of option name/value.
character(*), parameter, public :: COMMENTS            = "!;#" !< Characters used for defining a comment line.
character(1), parameter, public :: INLINE_COMMENT      = ';'   !< Inline comment delimiter.
endmodule finer_backend
