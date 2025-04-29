module p_fuel_annl
! Module created to hold fuel delivery (1..2..3) variables from
! annlcommon
use hesi_gendecs
implicit none
    type t_ns_p_fuel_annl
    real (kind=4) :: p_fuel_delivery_1(max_cl_units)
    real (kind=4) :: p_fuel_delivery_2(max_cl_units)
    real (kind=4) :: p_fuel_delivery_3(max_cl_units)
    end type t_ns_p_fuel_annl
    type(t_ns_p_fuel_annl) :: ns_p_fuel_annl
contains

function get_p_fuel_delivery(r_nunits)  !r4
    real :: get_p_fuel_delivery
    integer (kind=2) :: r_nunits
       get_p_fuel_delivery = ns_p_fuel_annl%p_fuel_delivery_1(r_nunits)
    end function

    real function  get_p_fuel_delivery_2(r_nunits)   !r4
        integer (kind=2) :: r_nunits
      get_p_fuel_delivery_2 = ns_p_fuel_annl%p_fuel_delivery_2(r_nunits)
    end function

    real function  get_p_fuel_delivery_3(r_nunits)  !r4
        integer (kind=2) :: r_nunits
      get_p_fuel_delivery_3 = ns_p_fuel_annl%p_fuel_delivery_3(r_nunits)
    end function
end module p_fuel_annl