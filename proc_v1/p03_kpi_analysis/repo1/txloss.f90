module txloss
    use prod_arrays_dimensions
    implicit none
    type t_ns_txloss
      real :: BOKITCFWK(MAX_FINANCIAL_SIMULATION_YEARS+17), &
        FDBKLOSS(MAX_FINANCIAL_SIMULATION_YEARS+17), &
        FBOKLCFWK(MAX_FINANCIAL_SIMULATION_YEARS+17), &
        FEDITC(MAX_FINANCIAL_SIMULATION_YEARS+17), &
        STBOKLOSS(MAX_FINANCIAL_SIMULATION_YEARS+17), &
        SBOKLCFWK(MAX_FINANCIAL_SIMULATION_YEARS+17), &
        FDBKITC(MAX_FINANCIAL_SIMULATION_YEARS+17), &
        AMTLOSS(MAX_FINANCIAL_SIMULATION_YEARS+17), &
        AMTLCFWK(MAX_FINANCIAL_SIMULATION_YEARS+17), &
        AMTFITCWK(MAX_FINANCIAL_SIMULATION_YEARS+17)   
    end type t_ns_txloss
    
    ! SAVE required by Lahey, and should be implicit per the Fortran standard.
    type(t_ns_txloss), save :: ns_txloss ! txloss namespace
        
end module txloss