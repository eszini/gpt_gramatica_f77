module co2_data
implicit none
real (kind=4) :: co2_emissions_for_system 
contains
    function get_co2_emiss_for_system()
    real :: get_co2_emiss_for_system
        get_co2_emiss_for_system = co2_emissions_for_system
    end function get_co2_emiss_for_system
end module co2_data