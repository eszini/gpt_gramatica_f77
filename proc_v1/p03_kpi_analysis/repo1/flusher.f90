module flusher
implicit none
contains
subroutine flush_unit(unitId)
    integer(kind=2) :: unitId
    call flush(unitId)
end subroutine flush_unit
end module