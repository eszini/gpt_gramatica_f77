module ouc_parameters
    type t_ns_ouc_parameters
        real (kind=4) :: INTEREST_ON_RENEW_AND_REPLACE
        real (kind=4) :: INT_ON_LIABILITY_REDUCTION
        real (kind=4) :: INT_ON_SELF_INSURANCE
        real (kind=4) :: INT_ON_SR_PRINC_SINK_FUND
        real (kind=4) :: INT_ON_JR_PRINC_SINK_FUND
        real (kind=4) :: INT_ON_SR_INT_SINK_FUND
        real (kind=4) :: INT_ON_JR_INT_SINK_FUND
        real (kind=4) :: INT_ON_OP_CONSTRUCT_FUND
        real (kind=4) :: INT_ON_DEBT_SERV_RESERVE
        real (kind=4) :: INT_ON_PLACEHOLDER_A
        real (kind=4) :: INT_ON_PLACEHOLDER_B
        real (kind=4) :: INT_ON_BASE_EL_STAB_FUND
        real (kind=4) :: INT_ON_WATER_STAB_FUND
        real (kind=4) :: INT_ON_FUEL_STAB_FUND
        real (kind=4) :: INT_ON_CUST_RETENTION_FUND
        real (kind=4) :: LTD_FIXED_INTEREST_RATE
        real (kind=4) :: LTD_VARIABLE_INTEREST_RATE
        real (kind=4) :: GNP_DEFLATOR
        real (kind=4) :: INT_ON_MINI_BD_PRINC_SINK
        real (kind=4) :: INT_ON_MINI_BD_INT_SINK
        real (kind=4) :: OUC_RENEW_REPLACE_PERCENT
        real (kind=4) :: UNAPPLIED_CIAC
    end type t_ns_ouc_parameters
    
    type (t_ns_ouc_parameters) :: ns_ouc_parameters ! Namespace
end module ouc_parameters
