module cl_screen_data_mod
implicit none
    type t_ns_screen_data
    real (kind=4), allocatable :: P_SO2(:), P_NOX(:)
    CHARACTER (len=1), allocatable :: LDTYPE(:)
      INTEGER (kind=8) :: THERMAL_PARENT_ID
      REAL (KIND=4) :: DECOM_BASE_YR_COST_ord, & 
                       DECOMMISSIONING_COST_ESCALATION, & 
                       ANNUAL_ENERGY_PLANNING_FACTOR, & 
                       NAME_PLATE_CAPACITY_cls, & 
                       FuelRatio(5), & 
                       BlendableFuelsPtr(4), & ! 167 & 
                       BettermentProjectID, & 
                       DECOM_CONTINUING_COST, &
                       DECOM_CONTINUING_COST_ESCALATION, & 
                       EnrgPatternPointer, &
                       EmissRedRate(5), & 
                       EmissMaxRate(5), & 
                       MaxEnergyLimit(3), &
					   FIRST_YEAR_DECOM_AVAIL
		CHARACTER (LEN=20) :: TECH_TYPE
        character (len=9) :: emission_data_units
        character (len=5) :: linked_betterment_option
    end type t_ns_screen_data
    
    type(t_ns_screen_data)  :: ns_screen_data
    
    
    
end module cl_screen_data_mod
