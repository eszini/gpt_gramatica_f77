module cl_units_read_decs
use hesi_gendecs
implicit none
      real, save :: S_FUEL_DELIVERY(MAX_CL_UNITS), &
            S_FUEL_DELIVERY_2(MAX_CL_UNITS), &
            S_FUEL_DELIVERY_3(MAX_CL_UNITS)
contains
      function GET_S_FUEL_DELIVERY(unit_idx)  !R4
         integer (kind=2) :: GET_S_FUEL_DELIVERY, unit_idx

         GET_S_FUEL_DELIVERY = S_FUEL_DELIVERY(unit_idx)
      end function GET_S_FUEL_DELIVERY
      function GET_S_FUEL_DELIVERY_2(unit_idx)   !R4
         integer (kind=2) :: GET_S_FUEL_DELIVERY_2, unit_idx
         GET_S_FUEL_DELIVERY_2 = S_FUEL_DELIVERY_2(unit_idx)
      end function GET_S_FUEL_DELIVERY_2

      function GET_S_FUEL_DELIVERY_3(unit_idx)  !R4
         integer (kind=2) :: GET_S_FUEL_DELIVERY_3, unit_idx
         GET_S_FUEL_DELIVERY_3 = S_FUEL_DELIVERY_3(unit_idx)
      end function GET_S_FUEL_DELIVERY_3

end module cl_units_read_decs
