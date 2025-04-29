module mmrev_decs
! MSGMMREV contains a routine with ENTRY statements pretending to be a module.
! Putting shared variables in this module because compiler errors were encountered
! during refactoring.

	logical (kind=4) :: RPS_PROGRAM_FILE_EXISTS=.FALSE.
	
end module mmrev_decs