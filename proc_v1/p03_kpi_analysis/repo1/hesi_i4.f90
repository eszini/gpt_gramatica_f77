module hesi_i4
use hesi_gendecs
implicit none
    
    integer hesi_unit_id_num_i4(max_el_units)
contains
    function get_hesi_unit_id_num_i4(nunits)
        integer (kind=2) :: nunits
        integer (kind=2) :: get_hesi_unit_id_num_i4
        get_hesi_unit_id_num_i4 = hesi_unit_id_num_i4(nunits)
    end function get_hesi_unit_id_num_i4
end module hesi_i4