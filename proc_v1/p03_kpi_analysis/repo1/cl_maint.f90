module cl_maint
! Module created to avoid passing MAINT_INDEX as an argument
! and reduce stack usage.
implicit none
 integer (kind=2), allocatable :: MAINT_INDEX(:)
end module cl_maint