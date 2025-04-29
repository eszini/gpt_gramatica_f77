module expansion
use hesi_gendecs
implicit none
    integer (kind=2) :: expansion_unit_location(max_cl_units) = &
        max_cl_units*(0-1.0)
end module expansion