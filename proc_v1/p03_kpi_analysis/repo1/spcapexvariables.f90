      MODULE SPCapExVariables
! SPCapEx Additions Nov. 2005
         USE PROD_ARRAYS_DIMENSIONS
         REAL (KIND=4), SAVE :: FIRST_YEAR_DECOM_AVAIL_capex(MAX_CL_UNITS)
         REAL (KIND=4), SAVE :: DECOMMISSIONING_BASE_YR_COST_capex(MAX_CL_UNITS),&
                                DECOMMISSIONING_COST_ESCALATION(MAX_CL_UNITS),&
                                ANNUAL_ENERGY_PLANNING_FACTOR_capex(MAX_CL_UNITS),&
                                NAME_PLATE_CAPACITY_capex(MAX_CL_UNITS),&
                                FuelRatio_capex(5,MAX_CL_UNITS),&
                                BlendableFuelsPtr(4,MAX_CL_UNITS),&
                                FuelTransportationCost(4,MAX_CL_UNITS),&
                                BlendedEnthalpyUp(MAX_CL_UNITS),&
                                BlendedEnthalpyLo(MAX_CL_UNITS),&
                                BlendedSO2Up(MAX_CL_UNITS),&
                                BettermentProjectID(MAX_CL_UNITS),&
                                DECOM_CONTINUING_COST(MAX_CL_UNITS),&
                                DECOM_CONT_COST_ESCALATION(MAX_CL_UNITS),&
                                EnrgPatternPointer_capex(MAX_CL_UNITS),&
                                EmissRedRate(5,MAX_CL_UNITS),&
                                EmissMaxRate(5,MAX_CL_UNITS),&
                                MaxEnergyLimit(3,MAX_CL_UNITS)
         CHARACTER (LEN=10), SAVE :: FUEL_BLENDING_IS(MAX_CL_UNITS)
         CHARACTER (LEN=20), SAVE :: SP_NEWGEN_UNIT_STATUS(MAX_CL_UNITS)
         CHARACTER (LEN=20), SAVE :: TECH_TYPE(MAX_CL_UNITS)
         CHARACTER (LEN=1),  SAVE :: AGGREGATE_THIS_UNIT(MAX_CL_UNITS),&
                                     EMISSION_DATA_UNITS(MAX_CL_UNITS),&
                                     LINKED_BETTERMENT_OPTION(MAX_CL_UNITS)
         CHARACTER (LEN=48), SAVE :: SP_UNIT_NAME(MAX_CL_UNITS)=" "
     CHARACTER (LEN=20) :: MARKETSYM_UNIT_NAME(MAX_CL_UNITS)=" "
! SPCapEx Additions April 13, 2005.
! 062006. SPCAPEX ADDITION LINKED_BETTERMENT_OPTION
      END MODULE SPCapExVariables