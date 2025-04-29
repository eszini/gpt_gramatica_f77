module clgetter
implicit none
contains
subroutine getcl_unit(UnitParameter)
    character(len=*) :: UnitParameter
	call GETCL(UnitParameter)
end subroutine getcl_unit

subroutine getenv_unit(UnitParameter1, UnitParameter2)
    character(len=*) :: UnitParameter1, UnitParameter2
    call GETENV(UnitParameter1, UnitParameter2)
end subroutine getenv_unit
end module