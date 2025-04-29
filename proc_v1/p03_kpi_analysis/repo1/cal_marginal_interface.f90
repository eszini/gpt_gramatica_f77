module cal_marginal_interface
interface 
    function CAL_MAR_MONTHLY_ENERGY_routine( ENERGY,BLKNO,UNIT, &
      LEFT_HEAT,RIGHT_HEAT, REMAINING_ENERGY)
      use PROD_ARRAYS_DIMENSIONS
        real(kind=8) :: cal_mar_monthly_energy_routine
        real :: energy(2,MAX_CL_UNITS)
        integer(KIND=2) :: blkno(max_dispatch_blocks), unit(*)
        real :: left_heat(max_cl_units), right_heat(max_cl_units)
        real :: remaining_energy
    end function CAL_MAR_MONTHLY_ENERGY_routine
end interface 
end module cal_marginal_interface