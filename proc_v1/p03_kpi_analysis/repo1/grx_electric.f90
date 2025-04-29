module grx_electric
! Module was created to avoid a circular reference issue. Name
! may not adequately reflect resources provided, and resources
! in other modules may belong in this module.
use co2_cap_n_trade
implicit none

contains
     real function grx_non_elec_sector_co2_emss(year)
      integer (kind=2) :: year
         if(year <= yr2_data .and. year >= yr1_data .and. &
          co2_param_file_exists) then
            grx_non_elec_sector_co2_emss = &
            10.**6*(residential_emissions(year) &
            + commercial_emission(year) &
            + industrial_emissions(year) &
            + transportation_emissions(year))
         else
            grx_non_elec_sector_co2_emss = 0.
         endif

      end function grx_non_elec_sector_co2_emss
end module grx_electric