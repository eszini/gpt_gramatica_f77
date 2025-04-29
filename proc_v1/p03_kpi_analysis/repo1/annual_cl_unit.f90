module annual_cl_unit
use hesi_gendecs
use prod_arrays_dimensions
implicit none
real (kind=4) :: annual_cl_unit_capacity(max_cl_units)
    real (kind=4) :: annual_cl_unit_emissions( &
        number_of_emission_types,max_cl_units)
    real (kind=4) :: annual_cl_unit_energy(max_cl_units)
    real (kind=4) :: annual_cl_unit_fixed_cost(max_cl_units)
    real (kind=4) :: annual_cl_unit_fuel_cost(max_cl_units)
    real (kind=4) :: annual_cl_unit_var_cost(max_cl_units)
    real (kind=8) :: annual_cl_unit_mmbtus(max_cl_units) ! yes, real*8
end module annual_cl_unit