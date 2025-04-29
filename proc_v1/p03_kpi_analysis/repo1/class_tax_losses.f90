module class_tax_losses
! From finaccom
use prod_arrays_dimensions

implicit none


    type t_ns_class_tax_losses
        real :: &
        bokitcfwk(max_financial_simulation_years+17), & 
        fdbkloss(max_financial_simulation_years+17), & 
        fboklcfwk(max_financial_simulation_years+17), & 
        feditc(max_financial_simulation_years+17), & 
        stbokloss(max_financial_simulation_years+17), & 
        sboklcfwk(max_financial_simulation_years+17), & 
        fdbkitc(max_financial_simulation_years+17), & 
        amtloss(max_financial_simulation_years+17), & 
        amtlcfwk(max_financial_simulation_years+17), & 
        amtfitcwk(max_financial_simulation_years+17)
   end type t_ns_class_tax_losses
   
   
   ! SAVE required by Lahey, and should be implicit per the Fortran standard.
   type(t_ns_class_tax_losses), save :: ns_class_tax_losses ! Namespace
   
end module class_tax_losses