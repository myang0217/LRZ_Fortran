PROGRAM example_getopt
  USE ftn_getopt
  IMPLICIT none

  INTEGER, PARAMETER :: nopt = 3, strlen = 80
  TYPE(opt_t) opts(nopt)

  INTEGER :: len
  LOGICAL :: verbose
  CHARACTER(len=strlen) :: key

  len = 0; verbose = .FALSE.; key = ''


  opts = optinit( [ character(len=7) :: 'len', 'verbose', 'key' ] , &
       [ character(len=7) :: 'integer', 'logical', 'string' ] )

  CALL optarg(opts)

  CALL optval(opts(1), len)
  CALL optval(opts(2), verbose)
  CALL optval(opts(3), key)

  WRITE(*, FMT='(''optignored: '',10L5)') optignored

  WRITE(*, FMT='(''                       len      verbose     key  '')')
  WRITE(*, FMT='(''is option set? '',3L10)') optset(opts)
  WRITE(*, FMT='(''no. of argument'',3I10)') optind(opts)
  WRITE(*, FMT='(''value          '',I10,L10,9X,A)') len, verbose, key

END PROGRAM example_getopt
