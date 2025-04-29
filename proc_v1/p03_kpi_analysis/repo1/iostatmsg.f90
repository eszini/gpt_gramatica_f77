module iostatmsg
implicit none
contains
subroutine iostatmsg_unit(UP1, UP2)
    integer(kind=4) :: UP1
	character(len=*) :: UP2
	call IOSTAT_MSG (UP1, UP2)
end subroutine
end module