!> \mainpage
!> \brief The module ftn_getopt supplies a method for handling command arguments 
!> in a manner similar to the getopt facility in C.
!> \author R. Bader
!> General Description
!> -------------------
!> Command line options are strings that can be interpreted as key-value pairs
!> representing values of type logical, integer, real, or character string, 
!> respectively. A logical option takes its value from its presence on the command 
!> line, all other types require an additional string argument.
!>
!> The following ways of specifying options on the command line are supported:
!>
!> - `--switch`            for a logical option (true if option appears)
!> - `--switch <value>`    for an otherwise typed option
!> - `--switch=<value>`    equivalent to the preceding
!>
!> As an optional feature, it is also possible to add short option processing:
!>
!> - `-s`                  for a logical option
!> - `-stu`                same as `-s -t -u`
!> - `-s <value>`          for an otherwise typed option
!>
!> Only the first appearance of an option on the command line will be processed.
!>
!> Inside a program unit, the sequence of processing is as follows:
!>
!> -# invoke `optinit()` to create an option (a scalar or array of type `opt_t`)
!> -# invoke `optarg()` to extract the option(s) from the command line
!> -# invoke `optval()` to obtain the result object
!>
!> An example of how this might look like is
!>    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>    USE ftn_getopt
!>    TYPE(opt_t) :: option
!>    INTEGER :: nopt
!>    
!>    option = optinit('nopt', 'integer')
!>    CALL optarg(option)
!>    CALL optval(option, nopt)
!>    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!> The last procedure call will supply the value of the command line option
!> `--nopt <integer>` to the variable `nopt`. If the option is not encountered, 
!> the variable's value will remain unchanged. If no integer value can be 
!> extracted, the program will terminate, although error handling can also be 
!> deferred to the programmer if the optional `stat` argument is added to the
!> procedure invocation.
!>
!> Addition interfacing is available for convenience:
!>
!> - list of currently unprocessed command line items via `optignored`,
!> - check whether an option has been successfully set by optarg via `optset()`,
!> - obtain sequence index of a set option in the command line via `optind()`,
!> - obtain the length of a string-valued option via `optlen()`.
!>
!> The facilities supplied here are not thread-safe. All procedure calls
!> should be executed from the initial master thread, or need to be mutexed
!> against other threads (e.g. appear inside a critical region).
!>
!> Acknowledgment: Some ideas for the implementation were taken from Arjen Markus'
!> command_args module in his FLIBS project http://flibs.sourceforge.net/, but
!> the code given here was written from scratch.
!>
MODULE ftn_getopt
  IMPLICIT none
  PRIVATE
  PUBLIC :: optinit, optval, optarg, optind, optset, optignored, optlen
!
! Error status values
!> private
  INTEGER, PARAMETER :: stat_success = 0 
!> private
  INTEGER, PARAMETER :: stat_alloc_fail = 1      ! internal allocation failed
!> private
  INTEGER, PARAMETER :: stat_type_unsupp = 2     ! unsupported type of option
!> private
  INTEGER, PARAMETER :: stat_type_mismatch = 3   ! type mismatch
!> private
  INTEGER, PARAMETER :: stat_arg_fail = 4        ! argument processing failed
!> private
  INTEGER, PARAMETER :: stat_option_undef = 5    ! option is not defined
!
!> Unprocessed options
!> \brief This variable is available after the first call to optarg.
  LOGICAL, ALLOCATABLE, PROTECTED :: optignored(:)
!
! internal processing of arguments
  CHARACTER(len=:), ALLOCATABLE :: arg
!
!> Class opt_t
!> \brief each option is stored inside an object of this type.
  TYPE, PUBLIC :: opt_t
     PRIVATE 
!> \private 
     CHARACTER(len=:), ALLOCATABLE :: name
!> \private 
     CHARACTER(len=1) :: short_name = ' '
!> \private 
     LOGICAL :: set = .FALSE.      ! .TRUE. if supplied on command line
!> \private 
     INTEGER :: optind = -1        ! position of option on command line
!> \private 
     CLASS(*), ALLOCATABLE :: value
  END TYPE opt_t
!
!> Generic interface optval(type(opt_t) option, \<type\> value, integer stat)
!> \brief extract value from type(opt_t) option object if set.
!> \param[in]      option. If the incoming option is not set, the 
!!        value argument will not be modified and the invocation is 
!!        considered to have failed.
!> \param[in,out]  value. must be a scalar of one of the types
!!        integer, real, logical, or character(len=*)
!> \param[out]     stat.     (optional). status is returned if present. 

  INTERFACE optval
     MODULE PROCEDURE optval_logical
     MODULE PROCEDURE optval_integer
     MODULE PROCEDURE optval_real
     MODULE PROCEDURE optval_string
  END INTERFACE
CONTAINS
!> Procedure optinit
!> \brief Initialize an opt_t object by specifying its name and type.
!!        By default, the long option style is supported, but a short 
!!        option name can be optionally added.
!!        A blank or empty string for the short option is equivalent to 
!!        omission.
!> \param[in] type: must be one of the values 'integer', 'logical',
!!        'real' or 'string'.
  IMPURE ELEMENTAL FUNCTION optinit(name, type, short_name, stat) &
       RESULT(option)
    CHARACTER(len=*), INTENT(in) :: name, type
    CHARACTER(len=1), INTENT(in), OPTIONAL :: short_name 
    INTEGER, INTENT(OUT), OPTIONAL :: stat
    TYPE(opt_t) :: option

    INTEGER :: stat_local

    stat_local = stat_success
    option%name = trim(name)
    IF ( present(short_name) ) THEN
       IF ( short_name /= '' .AND. short_name /= ' ' ) option%short_name = short_name
    END IF
    SELECT CASE ( trim(type) )
    CASE( 'logical' )
       ALLOCATE(logical :: option%value, STAT=stat_local)
       if (stat_local /= 0) stat_local = stat_alloc_fail
    CASE( 'integer' )
       ALLOCATE(integer :: option%value, STAT=stat_local)
       if (stat_local /= 0) stat_local = stat_alloc_fail
    CASE( 'real' )
       ALLOCATE(real :: option%value, STAT=stat_local)
       if (stat_local /= 0) stat_local = stat_alloc_fail
    CASE( 'string' )
       ALLOCATE(character(len=1) :: option%value, STAT=stat_local)
!      the string length might change later
       if (stat_local /= 0) stat_local = stat_alloc_fail
    CASE default
       stat_local = stat_type_unsupp
    END SELECT
   
    IF ( present(stat) ) THEN
       stat = stat_local
    ELSE
       IF (stat_local /= 0) STOP 'ftn_getopt::optinit failed.'
    END IF
  END FUNCTION optinit
!
!>Procedure optset 
!> \brief Determine whether option has been set.
!!        This is useful after an optarg call, because the program-specified
!!        type might be inconsistent with what was supplied on the command line.
  ELEMENTAL LOGICAL FUNCTION optset(option)
    TYPE(opt_t), INTENT(in) :: option
    optset = option%set
  END FUNCTION optset
!
!> Procedure optind
!> \brief Determine position of option on command line (-1 if not set).
  ELEMENTAL INTEGER FUNCTION optind(option)
    TYPE(opt_t), INTENT(in) :: option
    optind = option%optind
  END FUNCTION optind
!
!> Procedure optlen
!> \brief Determine the length of a string type option. Zero is returned if the option
!!        is not set or of a different type.
  INTEGER FUNCTION optlen(option)
    TYPE(opt_t), INTENT(in) :: option
    optlen = 0
    IF ( .NOT. ( ALLOCATED(option%value) .AND. option%set ) ) RETURN
    SELECT TYPE (v => option%value)
    TYPE IS ( CHARACTER(len=*) )
       optlen = len(v)
    END SELECT
  END FUNCTION optlen
!
!> Specific procedure of optval
  SUBROUTINE optval_logical(option, value, stat)
    TYPE(opt_t), INTENT(in) :: option
    LOGICAL, INTENT(inout) :: value
    INTEGER, INTENT(out), OPTIONAL :: stat

    IF ( present(stat) ) stat = stat_success
    IF (option%set) THEN
       SELECT TYPE (v => option%value)
       TYPE IS ( LOGICAL )
          value = v
       CLASS DEFAULT
          CALL process_error('ftn_getopt::optval','<logical> mismatch',stat_type_mismatch,stat)
       END SELECT
    ELSE
       value = .FALSE.
    END IF
  END SUBROUTINE optval_logical
!
!> Specific procedure of optval
  SUBROUTINE optval_integer(option, value, stat)
    TYPE(opt_t), INTENT(in) :: option
    INTEGER, INTENT(inout) :: value
    INTEGER, INTENT(out), OPTIONAL :: stat

    IF ( present(stat) ) stat = stat_success
    IF (option%set) THEN
       SELECT TYPE (v => option%value)
       TYPE IS ( INTEGER )
          value = v
       CLASS DEFAULT
          CALL process_error('ftn_getopt::optval','<integer> mismatch',stat_type_mismatch,stat)
       END SELECT
    ELSE
       CALL process_error('ftn_getopt::optval','option undefined',stat_option_undef,stat)
    END IF
  END SUBROUTINE optval_integer
!
!> Specific procedure of optval
  SUBROUTINE optval_real(option, value, stat)
    TYPE(opt_t), INTENT(in) :: option
    REAL, INTENT(inout) :: value
    INTEGER, INTENT(out), OPTIONAL :: stat

    IF ( present(stat) ) stat = stat_success
    IF (option%set) THEN
       SELECT TYPE (v => option%value)
       TYPE IS ( REAL )
          value = v
       CLASS DEFAULT
          CALL process_error('ftn_getopt::optval','<real> mismatch',stat_type_mismatch,stat)
       END SELECT
    ELSE
       CALL process_error('ftn_getopt::optval','option undefined',stat_option_undef,stat)
    END IF
  END SUBROUTINE optval_real
!
!
!> Specific procedure of optval
  SUBROUTINE optval_string(option, value, stat)
    TYPE(opt_t), INTENT(in) :: option
    CHARACTER(len=*), INTENT(inout) :: value
    INTEGER, INTENT(out), OPTIONAL :: stat

    IF ( present(stat) ) stat = stat_success
    IF (option%set) THEN
       SELECT TYPE (v => option%value)
       TYPE IS ( CHARACTER(len=*) )
          value = v
       CLASS DEFAULT
          CALL process_error('ftn_getopt::optval','<string> mismatch',stat_type_mismatch,stat)
       END SELECT
    ELSE
       CALL process_error('ftn_getopt::optval','option undefined',stat_option_undef,stat)
    END IF
  END SUBROUTINE optval_string
!
!> Procedure optarg
!> \brief process command line and store the result in supplied option 
!!        if it is encountered.
  IMPURE ELEMENTAL SUBROUTINE optarg(option, stat)
    TYPE(opt_t), INTENT(inout) :: option
    INTEGER, INTENT(out), OPTIONAL :: stat

    INTEGER :: i, k, l, mode, stat_local

    INTEGER, PARAMETER :: mode_option = 1, mode_value = 2

    stat_local = 0
!
! Set up optignored on first invocation 
    IF ( .NOT. allocated(optignored) ) THEN
       ALLOCATE (optignored(command_argument_count()), STAT=stat_local)
       IF ( stat_local /=0 ) THEN
          stat_local = stat_alloc_fail
          go to 1
       END IF
       optignored(:) = .TRUE.
    END IF
!
! Largest needed string for arguments
    IF ( .NOT. allocated(arg) ) THEN
       CALL get_command( LENGTH=l )
       ALLOCATE (CHARACTER(len=l) :: arg, STAT=stat_local)
       IF ( stat_local /=0 ) THEN
          stat_local = stat_alloc_fail
          go to 1
       END IF
    END IF
!
! Parse command line
    mode = mode_option
    cmdline : DO i = 1, size(optignored, 1)

!
!   revisit argument if it is a concatenated short option
       IF (option%short_name == ' ' .AND. ( .NOT. optignored(i) ) ) &
            CYCLE cmdline
       
       CALL get_command_argument( i, VALUE=arg, LENGTH=l, STATUS=stat_local ) 
       IF ( stat_local /=0 ) THEN 
          stat_local = stat_arg_fail
          go to 1
       END IF

       SELECT CASE (mode)
       CASE (mode_option)
          IF (arg(1:2) == '--') THEN            ! long option
             IF (arg(3:l) == option%name) THEN  ! next argument has the value
                                                ! unless option of type logical
                optignored(i) = .FALSE.
                SELECT TYPE ( v => option%value ) 
                TYPE IS (LOGICAL)           
                   CALL update(option, 'T', i, stat_local)
                   EXIT cmdline
                CLASS DEFAULT
                   mode = mode_value
                   IF ( i+1 > size(optignored, 1) ) THEN
                      stat_local = stat_arg_fail
                   END IF
                END SELECT
             ELSE                               ! same argument has the value separated by =
                k = index(arg(1:l),'=')         
                IF (k > 0 .AND. arg(3:k-1) == option%name) THEN  
                   CALL update(option, arg(k+1:l), i, stat_local)
                   optignored(i) = .FALSE.
                   EXIT cmdline
                END IF
             END IF
          ELSE IF (arg(1:1) == '-') THEN        ! short option
             IF ( option%short_name /= ' ' ) THEN   
                IF ( len(arg(2:l)) > 1 .AND. &  ! concatenated logicals
                     index(arg(2:l),option%short_name ) > 0  )THEN
                   optignored(i) = .FALSE.
                   CALL update(option, 'T', i, stat_local)
                   EXIT cmdline
                ELSE IF ( len(arg(2:l)) == 1 .AND. &    ! single-letter
                     arg(2:2) == option%short_name )  THEN
                                                ! next argument has the value
                                                ! unless option of type logical
                   optignored(i) = .FALSE.
                   SELECT TYPE ( v => option%value )  
                   TYPE IS (LOGICAL)           
                      CALL update(option, 'T', i, stat_local)
                      EXIT cmdline
                   CLASS DEFAULT
                      mode = mode_value
                      IF ( i+1 > size(optignored, 1) ) THEN
                         stat_local = stat_arg_fail
                      END IF
                   END SELECT
                END IF
              END IF
           END IF                                ! fall through if not recognized as option
        CASE (mode_value)                        ! arg(1:l) contains the value
           optignored(i) = .FALSE.
           CALL update(option, arg(1:l), i-1, stat_local)
           mode = mode_option
           EXIT cmdline
        END SELECT
     END DO cmdline
!
! Error handling 
1    CONTINUE
     CALL process_error('ftn_getopt::optarg','failed',stat_local,stat)
  END SUBROUTINE optarg
!
! private module procedures
  IMPURE ELEMENTAL SUBROUTINE update(option, value, index, stat)
    TYPE(opt_t), INTENT(inout) :: option
    CHARACTER(len=*), INTENT(in) :: value
    INTEGER, INTENT(in) :: index
    INTEGER, INTENT(inout) :: stat
    LOGICAL :: re_allocate
    INTEGER :: digit, i, imin, ios, sign

    ios = 0
    re_allocate = .FALSE.
    IF ( .NOT. allocated( option%value ) ) THEN
       ios = stat_arg_fail
       GO TO 1
    END IF
!
! list directed I/O transfers (used for non-integers) are a bit more liberal than 
! explicit format checking, but more work would be needed in the implementation.
    SELECT TYPE ( v => option%value ) 
    TYPE IS (LOGICAL)
       READ(value, fmt=*, iostat=ios) v
    TYPE IS (INTEGER) 
       sign = 1
       IF ( value(1:1) == '-' .OR. value(1:1) == '+' ) THEN
          imin = 2
          IF ( value(1:1) == '-' ) sign = -1
       ELSE
          imin = 1
       END IF
       v = 0
       stringwalk : DO i = imin, len_trim(value)
          digit = iachar( value(i:i) ) - 48
          IF (digit < 0 .OR. digit > 9) THEN
             ios = stat_type_mismatch
             EXIT stringwalk
          END IF
          v = v * 10 + digit
!  NOTE: overflow currently not dealt with
       END DO stringwalk
       v = sign * v
    TYPE IS (REAL)
       READ(value, fmt=*, iostat=ios) v
    TYPE IS (CHARACTER(len=*))
       re_allocate = .TRUE.
    END SELECT
!
!  Only for string-typed option
    IF (re_allocate) THEN
       DEALLOCATE (option%value)
       ALLOCATE (option%value, source=value)
    END IF
!
!  Outgoing status
1   CONTINUE
    IF (ios == 0) THEN
       option%set = .TRUE.
       option%optind = index
    ELSE
       stat = stat_type_mismatch
    END IF
  END SUBROUTINE update
  SUBROUTINE process_error(procedure, message, value, stat)
    CHARACTER(len=*), INTENT(in) :: procedure, message
    INTEGER, INTENT(in) :: value
    INTEGER, INTENT(inout), OPTIONAL :: stat
    IF ( present(stat) ) THEN
       stat = value
    ELSE
       IF (value /= 0) THEN
          WRITE( *, FMT='(A,'': '',A, ''(return value '',I0,'')'')' ) &
               procedure, message, value 
          STOP 
       END IF
    END IF
  END SUBROUTINE process_error
END MODULE ftn_getopt
