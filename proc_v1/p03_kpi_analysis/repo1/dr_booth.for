!
      module dr_booth_modules
      use nunits_shared
      use energy_trace
      implicit none

!     ENTRY RETURN_P_SALES_REVENUE_ENERGY
      REAL ::         ANNUAL_CNTR_SALES_REVENUE=0. 
      REAL ::         ANNUAL_CNTR_SALES_ENERGY=0. 


      INTEGER (kind=2) ::   LOCAL_MAX_CL_UNITS

!     ENTRY RETURN_UNIT_EMISSIONS
      REAL ::  SOX_BY_FUEL_TYPE(:,:)
      REAL ::  NOX_BY_FUEL_TYPE(:,:)
      REAL ::  CO2_BY_FUEL_TYPE(:,:)
      REAL ::  OTH2_BY_FUEL_TYPE(:,:)
      REAL ::  OTH3_BY_FUEL_TYPE(:,:)
      ALLOCATABLE :: SOX_BY_FUEL_TYPE,
     +               NOX_BY_FUEL_TYPE,
     +               CO2_BY_FUEL_TYPE,
     +               OTH2_BY_FUEL_TYPE,
     +               OTH3_BY_FUEL_TYPE

!     ENTRY RETURN_HEAT_BY_FUEL
      REAL        :: UNIT_HEAT_BY_FUEL(:,:)
      ALLOCATABLE :: UNIT_HEAT_BY_FUEL

!     ENTRY SAVE_EMISSIONS_BY_BLOCK
      REAL        :: BLOCK1_HEAT=0. ,BLOCK2_HEAT=0. 
      REAL        ::  SOX_BY_BLOCK(:,:)
      REAL        ::  NOX_BY_BLOCK(:,:)
      REAL        ::  CO2_BY_BLOCK(:,:)
      REAL        ::  OTH2_BY_BLOCK(:,:)
      REAL        ::  OTH3_BY_BLOCK(:,:)
      ALLOCATABLE :: SOX_BY_BLOCK,
     +               NOX_BY_BLOCK,
     +               CO2_BY_BLOCK,
     +               OTH2_BY_BLOCK,
     +               OTH3_BY_BLOCK
      REAL ::  SOX=  -99.
      REAL ::  CO2=  -99.
      REAL ::  OTH3= -99.
      REAL ::  OTH2= -99.
      REAL ::  NOX1= -99.
      REAL ::  NOX2= -99.


      LOGICAL (kind=1) ::  FISCAL_RESET_PEAK_MONTH
      LOGICAL (kind=1) ::  FISCAL_RESET_DATA

      contains


!     DR_BOOTH.FOR
!     Created: 10/29/02 2:44:52 PM


!             ONE WAY BOOTH-BALERIAUX METHOD FOR DSS
!                   COPYRIGHT (C) 1985, 1991, 1992
!               M.S. GERBER & ASSOCIATES, INC.
!                   ALL RIGHTS RESERVED

      SUBROUTINE DR_BOOTH(YR,ISEAS,FUELCOST,TENRG,TMMBTUS,VARCOST,
     +                 HOURS_INCREMENT,LPROB2,LODDUR,PEAK,DEMAND,
     +                 DX,MAINTENANCE_RATE,HEAT_RATE_FACTOR,
     +                 LAST_POINT,
     +                 SEASONAL_SYSTEM_CAP,
     +                 AVE_RUNNING_RATE,RR_OBS,
     +                 PEAK_MONTH,REMAIN_ANN_ENRG,
     +                 MAXIMUM_ANNUAL_CAPACITY,
     +                 PERIOD_COUNTER,
     +                 MMBTU_FUEL_BALANCE,
     +                 FUEL_INVENTORY_ACTIVE,
     +                 DISP_BTU_COST,USE_SECONDARY_FUEL,
     +                 TRANSACTION_BUY_SPREAD,TRANSACTION_SELL_SPREAD,
     +                 CNTR_WEIGHTED_CAPACITY,
     +                 SERVICE_TRANSACTIONS_ACTIVE,
     +                 WOLF_CREEK_ENRG_COST,
     +                 BLENDED_BTU_COST,
     +                 FUEL_INVENTORY_ID,
     +                 POOLING_VARIABLE_COST_SWITCH,
     +                 ANNUAL_BTUS_GOCN12,
     +                 ANNUAL_BTUS_FOR_BTU_TAX_GOCN12,
     +                 BTU_TAX_ACTIVE,
     +                 TIE_GROUP_LIMIT)

        use aitvafin
        use aitvaprod
        use annl_nuc
        use annual_cl_unit
        use annual_contracts
        use cal_marginal_interface
        use cap_prod_costs
        use capacity_arrays
        use contracts_data
        use eco
        use enrg_prod_costsr8
        use envircom
        use foshydi2com
        use globecom
        use group_prod
        use interg82
        use kepcocom
        use kootenay
        use logging
        use miscmod
        use mod_base_year
        use mwunih
        use poolcom
        use pp_objt_interfaces
        use prod2com
        use prod_arrays_dimensions
        use prodcom
        use production
        use rps_index_translate_mod
        use service_decs
        use shared_vars_interg82
        use sizecom
        use spindriftlib
        use offline_trace
        use debugtrace
!
! SERVICE TRANSACTIONS ADDED 10/18/92
!
      integer :: file_trace_bth=0 !DR_BOOTH proper only
      logical :: yrt_result
      character (len=6) ::   DZ
      LOGICAL (kind=4) ::   SERVICE_TRANSACTIONS_ACTIVE
      LOGICAL (kind=1) ::   LOLP_REPORT_ACTIVE
      LOGICAL (kind=1) ::  LOLP_REPORT
      LOGICAL (kind=1) ::  FACTORS_FOUND
      LOGICAL (kind=1) ::  GET_BLOCK_CAP_FACTORS
      LOGICAL (kind=1) ::  CPL_ACTIVE
      LOGICAL (kind=1) ::  CAL_MARGINAL_COST
      LOGICAL (kind=1) ::  UPDATE_OUTAGE_DATA
      LOGICAL (kind=1) ::  COMPUTED_MARGINAL_COSTS

      LOGICAL (kind=1) ::  HAVE_HOURLY_MC_AFTER_EL
      LOGICAL (kind=1) ::  CAL_HOURLY_MC_AFTER_EL
      LOGICAL (kind=1) ::  APPLY_TRANS_REV_TO_WHOLESALE
      LOGICAL (kind=1) ::  YES_RUN_MULTIAREA_TRANSACT
      LOGICAL (kind=1) ::  RUN_TRANSACT_THIS_MONTH
      LOGICAL (kind=1) ::  TRANSACT_ACTIVE_THIS_MONTH
      LOGICAL (kind=1) ::  GET_TRANSACT_C_STATUS
      LOGICAL (kind=1) ::  TRANSACT_C_ACTIVE
      LOGICAL (kind=1) ::  DAILY_THERMAL_REPORT_ACTIVE
      LOGICAL (kind=1) ::  YES_DAILY_THERMAL_RPT_ACTIVE
      LOGICAL (kind=1) ::  TEMP_L1
      LOGICAL (kind=1) ::  GET_THERMAL_RPS_DATA
      LOGICAL (kind=1) ::  PUT_THERMAL_RPS_ENRG_CAP
      REAL ::  HEAT_CONVERSION
      REAL ::  TONS_CONVERSION
      REAL ::  COST_CONVERSION
      REAL ::  MONTHS_ON_LINE
      REAL ::  POOLING_VARIABLE_COST_SWITCH
      REAL :: GET_COST_CONVERSION
      REAL :: GET_HEAT_CONVERSION,GET_TONS_CONVERSION
      REAL ::  REMAINING_ENERGY
      REAL ::  ENERGY(2,MAX_CL_UNITS)
      REAL ::  LEFT_SAVE(MAX_DISPATCH_BLOCKS)
      REAL ::  RIGHT_SAVE(MAX_DISPATCH_BLOCKS)
      REAL ::  RR_OBS(LOAD_CURVE_POINTS)
      REAL ::  SEGMENT_COST(MAX_DISPATCH_BLOCKS)
      REAL ::  SRP_SEGMENT_COST(MAX_DISPATCH_BLOCKS)
      REAL ::  AVE_RUNNING_RATE(LOAD_CURVE_POINTS)
      REAL ::  MONTHLY_ECONOMY_BOUGHT
      REAL ::  MONTHLY_ECONOMY_COST
      REAL ::  MONTHLY_ECONOMY_SOLD
      REAL ::  MONTHLY_ECONOMY_REVENUE
      REAL ::  WABASH_TRANS_BUY_ENERGY
      REAL ::  WABASH_TRANS_BUY_RATE
      REAL ::  WABASH_TRANS_SELL_ENERGY
      REAL ::  WABASH_TRANS_SELL_RATE
      REAL ::  CNTR_WEIGHTED_CAPACITY(MAX_CONTRACTS)
      REAL ::  REMAINING_ENERGY_FOR_BLOCK(MAX_DISPATCH_BLOCKS)
      REAL ::  LAST_B_FOR_BLOCK(MAX_DISPATCH_BLOCKS)
      REAL ::  REVENUE_GENERATING_CAPACITY(MAX_CL_UNITS)
      REAL ::  NUC_UNIT_FUEL_ADDER_COST
      

! FACET ALGORITHM VARIABLES
      LOGICAL (kind=1) ::   FACET_ACTIVE,FACET_IS_ACTIVE
      INTEGER (kind=2) ::   INT2_ZERO=0 ,PM,ST_TG,ST,R_MONTH,R_ST_TG
      INTEGER (kind=4) ::   START_DATE,END_DATE
      REAL (kind=4) ::  LOWER_BOUND_CAP_FACTOR(MAX_DISPATCH_BLOCKS)
      REAL (kind=4) ::  UPPER_BOUND_CAP_FACTOR(MAX_DISPATCH_BLOCKS)
      REAL (kind=4) ::  FACET_MAINTENANCE_RATE(MAX_CL_UNITS)
      REAL (kind=4) ::  AHR
      REAL (kind=4) ::  RPS_CONTRIB
      REAL (kind=4) ::  TEMP_R4
      REAL (kind=4) ::  R_RESOURCE_RPS_VARS(16)
      REAL :: PRIM_HEAT,SEC_HEAT,EMIS_HEAT
      REAL :: FUEL_MIX_PRIM(MAX_CL_UNITS)
      INTEGER (kind=2) ::   FUEL_INVENTORY_ID(0:*)
      INTEGER (kind=2) ::  ISEAS
      INTEGER (kind=2) ::  CURRENT
      INTEGER (kind=2) ::  UNITNO
      INTEGER (kind=2) ::  HOURS_INCREMENT
      INTEGER (kind=2) ::  YR
      INTEGER (kind=2) ::  GGI
      INTEGER (kind=2) ::  NBLOK2
      INTEGER (kind=2) ::  I
      INTEGER (kind=2) ::  J
      INTEGER (kind=2) ::  UNIT2(MAX_DISPATCH_BLOCKS)
      INTEGER (kind=2) ::  BLKNO2(MAX_DISPATCH_BLOCKS)
      INTEGER (kind=2) ::  BLKNUM
      INTEGER (kind=2) ::  PRODUCTION_PERIODS
      INTEGER (kind=2) ::  GROUP
      INTEGER (kind=2) ::  LAST_POINT
      INTEGER (kind=2) ::  EM
      INTEGER (kind=2) ::  ISTART
      INTEGER (kind=2) ::  AVAILABLE_CONTRACT_NO
      INTEGER (kind=2) ::  D_RATE
      INTEGER (kind=2) ::  OLD_LAST_POINT
      INTEGER (kind=2) ::  PEAK_MONTH
      INTEGER (kind=2) ::  CONTRACT
      INTEGER (kind=2) ::  BCM_NO
      INTEGER (kind=2) ::  PERIOD_COUNTER
      INTEGER (kind=2) ::  BEFORE_UPDATE_MONTH
      INTEGER (kind=2) ::  SUR_SALE_NO
      INTEGER (kind=2) ::  S_I
      INTEGER (kind=2) ::  SURPLUS_SALE(MAX_CONTRACTS)
      INTEGER (kind=2) ::  DISPATCH_BLOCK
      INTEGER (kind=2) ::  FUEL_ID
      INTEGER (kind=2) ::  R_I
      INTEGER (kind=2) ::  R_BLOCK
      INTEGER (kind=2) ::  GET_TRANS_UNIT_TO_BLOCK
      INTEGER (kind=2) ::  TEMP_I2
      INTEGER (kind=2) ::  R_UNIT
      INTEGER (kind=2) ::  R_BLOCK_NO
      PARAMETER(D_RATE=2)

      INTEGER (kind=2) ::   PROD_METHOD
      INTEGER (kind=2) ::   PROCMETH,ARRAY_POINTR,HOURLY_LOAD_OUT
      REAL ::  INHEAT2(MAX_DISPATCH_BLOCKS)
      REAL ::  BLK_MW
      REAL ::  MWBLK2(MAX_DISPATCH_BLOCKS)
      REAL ::  UNITCAP(MAX_CL_UNITS)
      REAL ::  UNIT_CAP_AFTER_LOSSES(MAX_CL_UNITS)
      REAL ::  CL_UNIT_MONTHLY_FIXED_COST(MAX_CL_UNITS)
      REAL ::  SEASON_CAPACITY(0:MAX_REPORTING_GROUPS)
      REAL ::  R1_OPTION_MWH
      REAL ::  R2_OPTION_MWH
      INTEGER (kind=2) ::   hours_month_ord
      INTEGER (kind=2) ::   MAX_MONTHLY_GROUPS
      INTEGER (kind=2) ::  MAX_MONTHLY_GROUP_VARIABLES
      INTEGER (kind=2) ::  MGI
      INTEGER (kind=2) ::  Capacity
      INTEGER (kind=2) ::  fixed_om
      INTEGER (kind=2) ::  Generation
      INTEGER (kind=2) ::  MMBTU
      INTEGER (kind=2) ::  Fuel
      integer (kind=2) ::  variable_om
      INTEGER (kind=2) ::  sulfur_o2
      INTEGER (kind=2) ::  Nitrix OX
      INTEGER (kind=2) ::  Other 1
      INTEGER (kind=2) ::  Other 2
      INTEGER (kind=2) ::  Other 3
      INTEGER (kind=2) ::  Wholesale Rev
      INTEGER (kind=2) ::  Wholesale Sell
      INTEGER (kind=2) ::  Wholesale Exp
      INTEGER (kind=2) ::  Wholesale Buy
      INTEGER (kind=2) ::  Equivalent Capacity

      PARAMETER(
     +         Capacity =     1,
     +         fixed_om =     2,
     +         Generation =   3,
     +         MMBTU =        4,
     +         Fuel =         5,
     +         variable_om =  6,
     +         sulfur_o2 =    7,
     +         Nitrix OX =    8,
     +         Other 1 =      9,
     +         Other 2 =      10,
     +         Other 3 =      11,
     +         Wholesale Rev= 12,
     +         Wholesale Sell=13,
     +         Wholesale Exp= 14,
     +         Wholesale Buy= 15,
     +         Equivalent Capacity = 16)
      REAL ::  CL_LOSSES(:)
      REAL ::  CT_LOSSES(:)
      REAL ::  BLK1_LOSSES
      REAL ::  MONTHLY_GROUP_REPORT(0:12,0:99,17)
      ALLOCATABLE :: CL_LOSSES,CT_LOSSES !,MONTHLY_GROUP_REPORT
      SAVE           MONTHLY_GROUP_REPORT

!
      REAL (kind=4) ::   SEASONAL_SYSTEM_CAP
      REAL (kind=4) ::   CL_P_CLASS_ASSIGNED_ENERGY(2)
      REAL (kind=4) ::   CL_P_CLASS_ASSIGNED_COST(2)
!
      REAL ::  LPROB2(CONVOLUTION_POINTS)
      REAL ::  CUMCAP
      REAL ::  CAPSYS
      REAL ::  CAPBLK
      REAL ::  CAPON
      REAL ::  SEAS_HOURS
      REAL ::  UNIT_ENRG
      REAL ::  LPROB(CONVOLUTION_POINTS,2)
      REAL ::  LODDUR(CONVOLUTION_POINTS)
      REAL ::  DX
      REAL ::  EA
      REAL ::  A
      REAL ::  B
      REAL ::  ENRG
      REAL ::  BASE
      REAL ::  PEAK
      REAL ::  LIMR
      REAL ::  LEFT(MAX_CL_UNITS)
      REAL ::  RIGHT(MAX_CL_UNITS)
      REAL ::  BLK1_HEAT(MAX_CL_UNITS)
      REAL ::  BLK2_HEAT(MAX_CL_UNITS)
      REAL ::  MAINTENANCE_RATE(MAX_CL_UNITS)
      REAL ::  TEMPEA(2,MAX_CL_UNITS)
      REAL ::  HEAT_RATE_FACTOR(MAX_CL_UNITS)
      REAL ::  EFFECTIVE_CAPACITY(2,MAX_CL_UNITS)
      REAL ::  P_NUCLEAR_FUEL_COST
      REAL ::  VAR_COST
      REAL ::  CL_DISPATCH_COST
      REAL ::  MAXIMUM_CAPACITY(MAX_CONTRACTS)
      REAL ::  MAXIMUM_ENERGY(MAX_CONTRACTS)
      REAL ::  TEMP_CNTR_VAR_COST
      REAL ::  TEMP_CNTR_FIXED_COST
      REAL ::  CUM_CONTRACT_CAP
      REAL ::  SEASONAL_CONTRACT_CAPACITY
      REAL ::  MINIMUM_ENERGY(MAX_CONTRACTS)
      REAL ::  MINIMUM_CAPACITY(MAX_CONTRACTS)
      REAL ::  MAX_RATCHET_CAPACITY(MAX_CONTRACTS)
      REAL ::  REMAIN_ANN_ENRG(MAX_CONTRACTS)
      REAL ::  MIN_RATCHET_CAPACITY(MAX_CONTRACTS)
      REAL ::  GET_VAR
      REAL ::  TOTAL_MIN_RATCHET_ENERGY
      REAL ::  CNTR_ENRG
      REAL ::  MAXIMUM_ANNUAL_CAPACITY(MAX_CONTRACTS)
      REAL ::  MIN_BC_FIXED_COST
      REAL ::  BC_BLK1_COST(3)
      REAL ::  TOTAL_MIN_RATCHET_CAPACITY
      REAL ::  CHECK_MAX_SURPLUS
      REAL ::  EXCESS_ENERGY
      REAL ::  FUEL_DERIVATIVE_PRICE
!
! TIE CONSTRAINTS 5/3/93 MSG COPYRIGHT M.S. GERBER & ASSOCIATES, INC
!
      REAL :: TIE_GROUP_LIMIT(3),CURRENT_TIE_LEVEL(3),TIE_CAPACITY
      INTEGER (kind=1) ::   TIE_GROUP
      LOGICAL (kind=1) ::  ALLOCATE_FUEL_RIGHT_TO_LEFT
      LOGICAL (kind=1) ::  RETURN_FUEL_ALLOCATION_SWITCH
      LOGICAL (kind=1) ::  UNIT_HAS_FUEL_DERIVATIVE
      LOGICAL (kind=1) ::  FUEL_DERIVATIVE_ACTIVE
      LOGICAL (kind=1) ::  FUEL_DERIVATIVES_FILE_STATUS
!
! BTU TAX STUFF
!
      INTEGER (kind=1) ::   L
      CHARACTER (len=6) ::   CAP_LIMIT_FUEL_TYPES
      PARAMETER(CAP_LIMIT_FUEL_TYPES='GOCN12')
      REAL (kind=8) ::  ANNUAL_BTUS_GOCN12(0:6)
      REAL (kind=8) ::  ANNUAL_BTUS_FOR_BTU_TAX_GOCN12(0:6)
      REAL (kind=8) ::    MONTH_BTUS_GOCN12(0:6)
      LOGICAL (kind=1) ::   BTU_TAX_ACTIVE
      LOGICAL (kind=1) ::  ECON_SWITCH
      LOGICAL (kind=1) ::  SALT_RIVER_PROJECT
      LOGICAL (kind=1) ::  ECON_SALES
      CHARACTER (len=1) ::   CONTRACT_REPORT
      INTEGER (kind=2) ::     X_CONTRACT,Y_CONTRACT,Z_CONTRACT
      REAL (kind=4) ::     TEMP_ENERGY
!
! RATCHET BASIS ADDED 6/16/92
!
      REAL :: RATCHET_CAPACITY_BASIS(MAX_CONTRACTS)
      REAL (kind=8) ::  DEMAND
      REAL (kind=8) ::  TENRG
      REAL (kind=8) ::  TMMBTUS
      REAL (kind=8) ::  FUELCOST
      REAL (kind=8) ::  VARCOST
      REAL (kind=8) ::  PENRG
      REAL (kind=8) ::  PMMBTUS
      REAL (kind=8) ::  PFUELCST
      REAL (kind=8) ::  PVARCOST
      REAL (kind=8) ::  P_PUR_POWER_COST
      REAL (kind=8) ::  PFIXCOST
      REAL (kind=8) ::  P_PUR_ENRG
      REAL (kind=8) ::  MMBTUS
      REAL (kind=8) ::  UTILITY_PORTION
      REAL (kind=8) ::  POOL_PORTION
      REAL (kind=8) ::  MMBTUS_USED
      REAL (kind=8) ::  TEMP_HEAT
      REAL (kind=8) ::  P_PUR_POWER_COST_VAR
      REAL (kind=8) ::  P_PUR_POWER_COST_FIXED
      REAL (kind=8) ::  PUR_ENRG_FROM_TRANSACT
      REAL (kind=8) ::  MONTHLY_UNIT_FUEL_DERIV_HEAT
      REAL :: RCOST
      REAL :: TEMP_SNGL,TEMP_SNGL2
      REAL (kind=8) ::   TEMP_DBLE
      LOGICAL (kind=1) ::   UNIT_OUTPUT_REPORT
      LOGICAL (kind=1) ::  ANNUAL_UNIT_OUTPUT_REPORT
      LOGICAL (kind=1) ::  SPREAD_SHEET
      LOGICAL (kind=1) ::  MONTHLY_UNIT_REPORT
      REAL (kind=4) ::    UNIT_FIXED_COST
      REAL :: UNIT_EMISS_CONTRIB(5)
!
! ENERGY ONLY CONTRACT VARIABLES
!
      REAL ::  ENRG_FROM_ENRG_ONLY_CONTRACTS
      REAL ::  REMAINING_ENRG_ONLY_ENRG
      REAL ::  MIN_ENRG_FROM_ENRG_ONLY_ENRG
      REAL ::  USED_ENRG_ONLY_ENERGY
      REAL ::  TOTAL_CONTRACT_ENRG
!
!     DECLARE PUCHASE CONTRACT VARIABLES
!
      REAL ::  CONTRACT_DISPATCH_COST
      REAL ::  CONTRACT_ENERGY(MAX_CONTRACTS)
      REAL ::  CONTRACT_CAPACITY(MAX_CONTRACTS)
      LOGICAL (kind=1) ::  CONTRACTS_ACTIVE
      LOGICAL (kind=1) ::  CL_UNITS_ACTIVE
      LOGICAL (kind=1) ::  AVAILABLE_CONTRACT(MAX_CONTRACTS)
      LOGICAL (kind=1) ::  CONTRACTS_IN_PERIOD
      LOGICAL (kind=1) ::  ACTIVE_3808
      LOGICAL (kind=1) ::  IS_3808_ACTIVE
      LOGICAL (kind=1) ::  CLASS_6_AND_WABASH_VALLEY

!
! USED IN ECONOMY INTERCHANGE
!
      REAL :: TRANSACTION_BUY_SPREAD,TRANSACTION_SELL_SPREAD
!
! KEPCO STUFF
!
      INTEGER (kind=1) ::   AREA
      REAL ::  CONTRACT_BILLING_CAPACITY
      REAL ::  WOLF_CREEK_ENRG_COST
      REAL ::  CURRENT_MAINTENANCE_EXPENSE
      REAL ::  P_SALES_REVENUE
      REAL ::  P_SALES_ENERGY
      REAL ::  R_CNTR_SALES_REVENUE
      REAL ::  R_CNTR_SALES_ENERGY
      real :: ANNUAL_CNTR_SALES_REVENUE=0.
	  real :: ANNUAL_CNTR_SALES_ENERGY=0.
      REAL ::  WC_PERIOD_MAINTENANCE_ENERGY
      REAL ::  WC_PERIOD_EMERGENCY_ENERGY
      REAL ::  CAPACITY_BILLED_AT_MIN
      REAL ::  CAPACITY_BILLED_AT_MAX
      INTEGER (kind=2) ::   WC_UNIT_NO
      LOGICAL (kind=1) ::   CONTROL_AREA_REPORT
      CHARACTER (len=1) ::   KEPCO_REPORTS,WABASH_POWER_COST_RPT
      CHARACTER (len=20) ::   MONTH_NAME
      REAL :: GIBSON_BLOCK1_ENERGY,GIBSON_BLOCK2_ENERGY
!
! FUEL INVENTORY VARIABLES 6/18/92
!
      REAL (kind=8) ::   MMBTU_FUEL_BALANCE(0:*)
      REAL (kind=8) ::  DEMAND_AFTER_DSM_AFTER_EL

      LOGICAL (kind=4) ::   FUEL_INVENTORY_ACTIVE
      REAL :: BLOCK_FUEL_COST(2,MAX_CL_UNITS)
      REAL :: DISP_BTU_COST(MAX_CL_UNITS)
      LOGICAL (kind=1) ::   USE_SECONDARY_FUEL(MAX_CL_UNITS)
      real (kind=8) :: ignoreit
      REAL :: AVAILABLE_SHADOW_CAPACITY(:)
      ALLOCATABLE :: AVAILABLE_SHADOW_CAPACITY
!
! FUEL BLENDING VARIABLES
!
      REAL :: BLENDED_BTU_COST(*)
      LOGICAL (kind=1) ::   FUEL_USAGE_REPORT,FUEL_USAGE_REPORT_ACTIVE
!
! MARGINAL COST DECLARATION
!
      LOGICAL (kind=1) ::   MARGINAL_COST_SWITCH,LAST_ELDC_REPORT
!
! MOVED CLASS ASSIGNED VARIABLES INTO COMMON BLOCK 6/5/91.
!
      EQUIVALENCE (LEFT(1),BLK1_HEAT(1)),(RIGHT(1),BLK2_HEAT(1))
      SAVE MAX_RATCHET_CAPACITY,MIN_RATCHET_CAPACITY,MIN_BC_FIXED_COST,
     +      BCM_NO,ACTIVE_3808,FUEL_USAGE_REPORT_ACTIVE

! DUKE CATAWBA
!
      LOGICAL (kind=1) ::   DUKE_IS_ACTIVE,INITIALIZE_DUKE_ROUTINES
      SAVE DUKE_IS_ACTIVE

      LOGICAL (kind=1) ::  VOID_LOGICAL
      LOGICAL (kind=1) ::  DUKE_CATAWBA_MCGUIRE_RESULTS
      LOGICAL (kind=1) ::  MON_MDS_CL_FIXED
      LOGICAL (kind=1) ::  MON_MDS_NUC_ADDER
      LOGICAL (kind=1) ::  CALCULATE_MONTHLY_TRANS_GROUP
      LOGICAL (kind=1) ::   LAHEY_LF95
      CHARACTER (len=11) ::   CLK
!
! END OF DATA DECLARATIONS
!
      if(file_trace_bth==0) then
        file_trace_bth=open_trace("dr_booth.trace", rq_bth)
        
      end if
      FUEL_DERIVATIVE_ACTIVE = FUEL_DERIVATIVES_FILE_STATUS()

      TRANSACT_ACTIVE_THIS_MONTH = RUN_TRANSACT_THIS_MONTH(ISEAS) .AND.
     +                                                YES_RUN_TRANSACT()
      CALL INIT_TRANS_C_MONTH_START_UPS()

      ALLOCATE_FUEL_RIGHT_TO_LEFT = RETURN_FUEL_ALLOCATION_SWITCH()
      MONTHLY_UNIT_REPORT = UNIT_OUTPUT_REPORT()
      FUEL_USAGE_REPORT_ACTIVE = FUEL_USAGE_REPORT() .AND.
     +                                                .NOT. TESTING_PLAN

      IF(PERIOD_COUNTER == 1) THEN

         MAX_MONTHLY_GROUPS = 99
         MAX_MONTHLY_GROUP_VARIABLES = 17

         MONTHLY_GROUP_REPORT = 0.
         RPS_THERMAL_DB = 0.
      ENDIF

      LOLP_REPORT_ACTIVE = LOLP_REPORT()
      FACET_ACTIVE = FACET_IS_ACTIVE()
      PROD_METHOD = PROCMETH()

      P_SALES_ENERGY = 0.
      P_SALES_REVENUE = 0.
      MONTHLY_ECONOMY_BOUGHT = 0.
      MONTHLY_ECONOMY_COST = 0.
      MONTHLY_ECONOMY_SOLD = 0.
      MONTHLY_ECONOMY_REVENUE = 0.
      LEFT_SAVE = 0.
      RIGHT_SAVE = 0.
      BLOCK_FUEL_COST = 0.0
      BLK1_HEAT = 0.0
      BLK2_HEAT = 0.0
!
! 6/9/93. GAT. TEMP VARIABLES TO TRACK BTUS BY FUEL BY BLOCK
!
      DO L = 0 , 6
         MONTH_BTUS_GOCN12(L) = 0.0
      ENDDO
      COST_CONVERSION = GET_COST_CONVERSION()
      HEAT_CONVERSION = GET_HEAT_CONVERSION()
      TONS_CONVERSION = GET_TONS_CONVERSION()
      BASE   = LODDUR(1)
      NBLOK2 = NBLOCK
      SEAS_HOURS = FLOAT(HOURS_INCREMENT)
      ISTART = 2
      DO I = 0, MAX_REPORTING_GROUPS
         SEASON_CAPACITY(I) = 0
      ENDDO
      ALLOCATE(AVAILABLE_SHADOW_CAPACITY(MAX(get_NUNITS(),1)))

      ENERGY = 0.
      

      DO I = 1, get_nunits()
         LEFT(I) = 0.
         RIGHT(I) = 0.
         UNITCAP(I) = 0.
         UNIT_CAP_AFTER_LOSSES(I) = 0.
         CL_UNIT_MONTHLY_FIXED_COST(I) = 0.
         IF(SHADOW_UNIT_NUMBER(I) /= 0 .AND.
     +             I > SHADOW_UNIT_NUMBER(I) .AND. SBTUCT(I) == 0.) THEN
            AVAILABLE_SHADOW_CAPACITY(I) = 0.
         ELSE
            AVAILABLE_SHADOW_CAPACITY(I) = 9999.
         ENDIF
         IF(PROD_METHOD == D_RATE .OR. FACET_ACTIVE .OR.
     +       (YES_RUN_TRANSACT() .AND. TRANSACT_ACTIVE_THIS_MONTH)) THEN
            TEMPEA(1,I) = 1.
            TEMPEA(2,I) = 1.
         ELSE
            TEMPEA(1,I) = EAVAIL(I)
            TEMPEA(2,I) = EAVAIL(I)
         ENDIF
         EFFECTIVE_CAPACITY(1,I) = 0.
         EFFECTIVE_CAPACITY(2,I) = 0.
         REVENUE_GENERATING_CAPACITY(I) = MW(2,I) * EAVAIL(I) *
     +                                          (1.-MAINTENANCE_RATE(I))
      ENDDO
      CL_P_CLASS_ASSIGNED_ENERGY(1) = 0.
      CL_P_CLASS_ASSIGNED_ENERGY(2) = 0.
      CL_P_CLASS_ASSIGNED_COST(1) = 0.
      CL_P_CLASS_ASSIGNED_COST(2) = 0.
      P_CLASS_ASS_ECON_SELL(1) = 0.
      P_CLASS_ASS_ECON_BUY (1) = 0.
      P_CLASS_ASS_ECON_COST(1) = 0.
      P_CLASS_ASS_ECON_REV(1) = 0.
      P_CLASS_ASS_ECON_SELL(2) = 0.
      P_CLASS_ASS_ECON_BUY (2) = 0.
      P_CLASS_ASS_ECON_COST(2) = 0.
      P_CLASS_ASS_ECON_REV(2) = 0.

! TIE VARIABLES

      CURRENT_TIE_LEVEL(1) = 0.
      CURRENT_TIE_LEVEL(2) = 0.
      CURRENT_TIE_LEVEL(3) = 0.

! ITEMS USED IN BOOTH
      IF( (PROD_METHOD /= D_RATE .OR. FACET_ACTIVE) .AND. .NOT.
     +       (YES_RUN_TRANSACT() .AND. TRANSACT_ACTIVE_THIS_MONTH)) THEN
         IF(MONTHLY_UNIT_REPORT .OR. FUEL_INVENTORY_ACTIVE) THEN
            BLKNO2(1:NBLOCK) = BLKNO(1:NBLOCK)
            MWBLK2(1:NBLOCK) = MWBLOK(1:NBLOCK)
            UNIT2(1:NBLOCK)  = UNIT(1:NBLOCK)
            INHEAT2(1:NBLOCK) = INHEAT(1:NBLOCK)
         ENDIF
         CAPON = 0.
         IF(get_NUNITS() > 0) then
            CALL COLAPSE(BASE,MAINTENANCE_RATE)
         end if
      ELSEIF(FUEL_INVENTORY_ACTIVE) THEN
         BLKNO2(1:NBLOCK) = BLKNO(1:NBLOCK)
         MWBLK2(1:NBLOCK) = MWBLOK(1:NBLOCK)
         UNIT2(1:NBLOCK)  = UNIT(1:NBLOCK)
         INHEAT2(1:NBLOCK) = INHEAT(1:NBLOCK)
      ENDIF ! BOOTH METHOD

      A = 0.
      B = 0.
      REMAINING_ENERGY = SNGL(DEMAND)/SEAS_HOURS
      CL_UNITS_ACTIVE = .FALSE.

!     IF THERE ARE ANY CONTRACTS LOADED, CHECK FOR ACTIVE CONTRACTS
!     AND INITIALIZE FOR PURCHASING

      CONTRACTS_ACTIVE = .FALSE.
      CONTRACTS_IN_PERIOD = .FALSE.
      CONTRACT_DISPATCH_COST = 999999.
      CUM_CONTRACT_CAP = 0.
      SEASONAL_CONTRACT_CAPACITY = 0.
      CUMCAP = 0.
      ENRG_FROM_ENRG_ONLY_CONTRACTS = 0.
      REMAINING_ENRG_ONLY_ENRG = 0.
      MIN_ENRG_FROM_ENRG_ONLY_ENRG = 0.
      TOTAL_CONTRACT_ENRG = 0.
      IF(NUMBER_OF_CONTRACTS > 0) THEN
         IF(get_WEST_KOOTENAY_POWER_active()) THEN
            CALL INITIALIZE_CONTRACTS_WKP(LPROB2,LODDUR,SEAS_HOURS,DX,
     +                             A,B,REMAINING_ENERGY,
     +                             ISEAS,
     +                             CONTRACTS_ACTIVE,
     +                             CONTRACT_DISPATCH_COST,
     +                             CONTRACT_ENERGY,CONTRACT_CAPACITY,
     +                            DATE1,DATE2,ISTART,LAST_POINT,
     +                             AVAILABLE_CONTRACT,
     +                             AVAILABLE_CONTRACT_NO,
     +                             MAXIMUM_ENERGY,MAXIMUM_CAPACITY,
     +                             CUMCAP,SEASONAL_CONTRACT_CAPACITY,
     +                             MINIMUM_ENERGY,MINIMUM_CAPACITY,
     +                             PEAK_MONTH,YR,
     +                             ENRG_FROM_ENRG_ONLY_CONTRACTS,
     +                             REMAINING_ENRG_ONLY_ENRG,
     +                             MIN_ENRG_FROM_ENRG_ONLY_ENRG,
     +                             MAX_RATCHET_CAPACITY,
     +                             REMAIN_ANN_ENRG,MIN_RATCHET_CAPACITY,
     +                             MAXIMUM_ANNUAL_CAPACITY,
     +                             TOTAL_MIN_RATCHET_ENERGY,CNTR_ENRG,
     +                             MIN_BC_FIXED_COST,BC_BLK1_COST,
     +                             BCM_NO,PERIOD_COUNTER,
     +                             TOTAL_CONTRACT_ENRG,
     +                             CONTRACTS_IN_PERIOD)
         ELSE
            CALL INITIALIZE_CONTRACTS(LPROB2,LODDUR,SEAS_HOURS,DX,
     +                             A,B,REMAINING_ENERGY,
     +                             ISEAS,
     +                             CONTRACTS_ACTIVE,
     +                             CONTRACT_DISPATCH_COST,
     +                             CONTRACT_ENERGY,CONTRACT_CAPACITY,
     +                            DATE1,DATE2,ISTART,LAST_POINT,
     +                             AVAILABLE_CONTRACT,
     +                             AVAILABLE_CONTRACT_NO,
     +                             MAXIMUM_ENERGY,MAXIMUM_CAPACITY,
     +                             CUMCAP,SEASONAL_CONTRACT_CAPACITY,
     +                             MINIMUM_ENERGY,MINIMUM_CAPACITY,
     +                             PEAK_MONTH,YR,
     +                             ENRG_FROM_ENRG_ONLY_CONTRACTS,
     +                             REMAINING_ENRG_ONLY_ENRG,
     +                             MIN_ENRG_FROM_ENRG_ONLY_ENRG,
     +                             MAX_RATCHET_CAPACITY,
     +                             REMAIN_ANN_ENRG,MIN_RATCHET_CAPACITY,
     +                             MAXIMUM_ANNUAL_CAPACITY,
     +                             TOTAL_MIN_RATCHET_ENERGY,CNTR_ENRG,
     +                             MIN_BC_FIXED_COST,BC_BLK1_COST,
     +                             BCM_NO,PERIOD_COUNTER,
     +                             TOTAL_CONTRACT_ENRG,
     +                             SUR_SALE_NO,
     +                             SURPLUS_SALE,RATCHET_CAPACITY_BASIS,
     +                             CONTRACTS_IN_PERIOD)
         ENDIF
      ENDIF

      CL_UNITS_ACTIVE = (REMAINING_ENERGY > 0.) .AND. (NBLOK2 > 0)

      IF(FACET_ACTIVE .AND. .NOT. TRANSACT_ACTIVE_THIS_MONTH) THEN


         FACTORS_FOUND =  GET_BLOCK_CAP_FACTORS(LOWER_BOUND_CAP_FACTOR,
     +                              UPPER_BOUND_CAP_FACTOR,NBLOK2,
     +                              FACET_MAINTENANCE_RATE,
     +                              FUEL_INVENTORY_ID,
     +                              MMBTU_FUEL_BALANCE,
     +                              MAINTENANCE_RATE)

              call write_trace_reals_vert(file_trace_bth, "2a. ENERGY", 
     + ENERGY(1,1:))
              call write_trace_reals_vert(file_trace_bth, "2b. ENERGY", 
     + ENERGY(2,1:))
        CALL FindOptCostByFacets( ! with ENERGY normalized to SEAS_HOURS
     +                            LAST_POINT,LPROB2,LODDUR,UNIT,BLKNO,
     +                  get_NUNITS(),NBLOK2,INT2_ZERO,INT2_ZERO,
     +                            LOWER_BOUND_CAP_FACTOR,
     +                            UPPER_BOUND_CAP_FACTOR,MWBLOK,
     +                            EAVAIL,FACET_MAINTENANCE_RATE,ENERGY)
              call write_trace_reals_vert(file_trace_bth, "3a. ENERGY", 
     + ENERGY(1,1:))
              call write_trace_reals_vert(file_trace_bth, "3b. ENERGY", 
     + ENERGY(2,1:))
         STORAGE_ENRG = STORAGE_ENRG + DYN_STORAGE_PUMP_ENRG
         ANN_DYN_STORAGE_PUMP_ENRG = ANN_DYN_STORAGE_PUMP_ENRG +
     +                                             DYN_STORAGE_PUMP_ENRG
         ANN_DYN_STORAGE_GEN_ENRG = ANN_DYN_STORAGE_GEN_ENRG +
     +                                              DYN_STORAGE_GEN_ENRG
         DO I = 1, NBLOK2
            UNITNO = UNIT(I)
            IF(FUEL_INVENTORY_ACTIVE .AND.
     +                         UNITNO > SHADOW_UNIT_NUMBER(UNITNO)) THEN
               UNITCAP(UNITNO) = UNITCAP(UNITNO) + MWBLOK(I)
            ENDIF
         ENDDO
      ENDIF

      PUR_ENRG_FROM_TRANSACT = 0.

      IF(MARGINAL_COST_SWITCH() .OR. (YES_RUN_TRANSACT() .AND.
     +                                 TRANSACT_ACTIVE_THIS_MONTH)) THEN
         IF( .NOT. FACET_ACTIVE)
     +               FACTORS_FOUND = GET_BLOCK_CAP_FACTORS(
     +                              LOWER_BOUND_CAP_FACTOR,
     +                              UPPER_BOUND_CAP_FACTOR,NBLOK2,
     +                              FACET_MAINTENANCE_RATE,
     +                              FUEL_INVENTORY_ID,
     +                              MMBTU_FUEL_BALANCE,
     +                              MAINTENANCE_RATE)

         IF(.NOT. LAHEY_LF95())
     +                 CALL MG_LOCATE_WRITE(11,9,'TRANS',ALL_VERSIONS,0)

         COMPUTED_MARGINAL_COSTS =
     +                     CAL_MARGINAL_COST(ISEAS,NBLOK2,MWBLOK,UNIT,
     +                                 FACET_MAINTENANCE_RATE,BLKNO,
     +                   get_NUNITS(),PERIOD_COUNTER,BASE,PEAK,
     +                                 UNIT_CAP_AFTER_LOSSES,UNITCAP)


         ! yrt_result=.true./.true.
         yrt_result=YES_RUN_TRANSACT()
         IF(yrt_result .AND. TRANSACT_ACTIVE_THIS_MONTH) THEN
            CALL REGIONAL_TRANSACTION_ANALYSIS(ISEAS,DEMAND)
              call write_trace_reals_vert(file_trace_bth, "4a. ENERGY", 
     + ENERGY(1,1:))
              call write_trace_reals_vert(file_trace_bth, "4b. ENERGY", 
     + ENERGY(2,1:))
        ignoreit = CAL_MAR_MONTHLY_ENERGY_routine(
     + ENERGY,BLKNO, UNIT,LEFT,RIGHT, REMAINING_ENERGY)
              call write_trace_reals_vert(file_trace_bth, "5a. ENERGY", 
     + ENERGY(1,1:))
              call write_trace_reals_vert(file_trace_bth, "5b. ENERGY", 
     + ENERGY(2,1:))

! 1/15/98. GAT. FOR TVA.

            IF(.NOT. YES_RUN_MULTIAREA_TRANSACT()) THEN


            ENDIF

            DO I = 1, NBLOK2
               UNITNO = UNIT(I)
               BLKNUM = MAX(1,BLKNO(I))
               EFFECTIVE_CAPACITY(BLKNUM,UNITNO) = MWBLOK(I) *
     +                                   (1. - MAINTENANCE_RATE(UNITNO))

! TOOK OUT CONDITIONAL ON 1/13/98. GAT.

               IF(.NOT. WABASH_VALLEY .AND.
     +               (SHADOW_UNIT_NUMBER(UNITNO) == 0 .OR.
     +                  .NOT. (FUEL_INVENTORY_ACTIVE .AND.
     +                       UNITNO > SHADOW_UNIT_NUMBER(UNITNO)))) THEN
                  UNITCAP(UNITNO) = UNITCAP(UNITNO) + MWBLOK(I)
               ENDIF
            ENDDO

         ENDIF ! YES RUN TRANSACT

      ENDIF ! MARGINAL_COST_SWITCH() .OR. YES_RUN_TRANSACT()

! LAST_POINT CALCULATIONS MOVED FROM PRO_COST AND CONTRACT INIT 3/4/92

      CAPSYS = SEASONAL_SYSTEM_CAP + SEASONAL_CONTRACT_CAPACITY
      OLD_LAST_POINT = LAST_POINT
      IF(CAPSYS > LODDUR(LAST_POINT)) THEN
         LAST_POINT =  MIN(CONVOLUTION_POINTS,
     +                             (1.1*CAPSYS - LODDUR(LAST_POINT))/DX)
      ELSE
         LAST_POINT =  LODDUR(LAST_POINT)/(10.*DX)
      ENDIF
      LAST_POINT = LAST_POINT + 1 + OLD_LAST_POINT
      LAST_POINT = MIN(CONVOLUTION_POINTS,LAST_POINT)
      DO I = OLD_LAST_POINT + 1, LAST_POINT
         LPROB2(I) = 0.
         LODDUR(I) = LODDUR(I-1) + DX
      ENDDO

      IF(PROD_METHOD /= D_RATE .OR. CONTRACTS_ACTIVE) THEN
         DO J = 1, LAST_POINT
            LPROB(J,1) = LPROB2(J)
            LPROB(J,2) = 0.
         ENDDO
         LPROB(1,2) = 1.
      ENDIF
      CURRENT = 1

      I = 0
      DOWHILE(.NOT. FACET_ACTIVE .AND. .NOT.
     +       (YES_RUN_TRANSACT() .AND. TRANSACT_ACTIVE_THIS_MONTH) .AND.
     +                     .NOT. LAHEY_LF95() .AND.
     +                          (CL_UNITS_ACTIVE .OR. CONTRACTS_ACTIVE))
         IF(CL_UNITS_ACTIVE) THEN
            I = MIN(I + 1,NBLOK2)
            UNITNO = UNIT(I)
            SEGMENT_COST(I) = DISPATCH_MULT(UNITNO) * (
     +                        INHEAT(I) * HEAT_RATE_FACTOR(UNITNO) *
     +                        FUEL_BTU_COST(UNITNO) *
     +                        COST_CONVERSION + 10. * VCPMWH(UNITNO) )
            SRP_SEGMENT_COST(I) = DISPATCH_MULT(UNITNO) * (
     +                        INHEAT(I) * HEAT_RATE_FACTOR(UNITNO) *
     +                        DISP_BTU_COST(UNITNO) *
     +                        COST_CONVERSION + 10. * VCPMWH(UNITNO) )
            IF(WABASH_VALLEY .AND. WABASH_IM_NIPSCO .AND.
     +          (UNITNO == GIBSON_BACKUP_POINTER .OR.
     +               UNITNO == GIBSON_POINTER .OR.
     +                   UNITNO == PSI_AREA_LAST_RESOURCE_POINTER)) THEN
               IF(UNITNO == GIBSON_BACKUP_POINTER) THEN
                  ARRAY_POINTR =
     +                CL_ALLOCATE_POINTR(CL_RESOURCE_ID(GIBSON_POINTER))
               ELSE
                  ARRAY_POINTR =
     +                        CL_ALLOCATE_POINTR(CL_RESOURCE_ID(UNITNO))
               ENDIF
               IF(WABASH_IM_NIPSCO) THEN
                  LEFT_SAVE(I) = 1.
                  RIGHT_SAVE(I) = 1.
                  IF(UNITNO /= GIBSON_BACKUP_POINTER) THEN
                     UNITCAP(UNITNO) = UNITCAP(UNITNO) + MWBLOK(I)
                     MWBLOK(I) = MWBLOK(I) *
     +                   (1.-AREA_CAPACITY_LOSSES(3,ISEAS,ARRAY_POINTR))
                     UNIT_CAP_AFTER_LOSSES(UNITNO) = MWBLOK(I) +
     +                                     UNIT_CAP_AFTER_LOSSES(UNITNO)
                     BLK_MW = MWBLOK(I)
                     IF(UNITNO == PSI_AREA_LAST_RESOURCE_POINTER) THEN
                        ENRG = BLK_MW
               call write_energy_trace("DR_BOOTH", "dr_booth:0003", 
     +             ENRG)
                     ELSE
                        ENRG = EAVAIL(UNITNO) *
     +                              (1.-MAINTENANCE_RATE(UNITNO))*BLK_MW
               call write_energy_trace("DR_BOOTH", "dr_booth:0004", 
     +             ENRG)
                     ENDIF
                     IF(BLKNO(I) <= 1) THEN
                        ENERGY(1,UNITNO) = ENRG
                        EFFECTIVE_CAPACITY(1,UNITNO) = ENRG
                     ELSE
                        ENERGY(2,UNITNO) = ENRG
                        EFFECTIVE_CAPACITY(2,UNITNO) = ENRG
                        LEFT(UNITNO) = 1.0
                        RIGHT(UNITNO) = 1.0
                     ENDIF
              call write_trace_reals_vert(file_trace_bth, "6a. ENERGY", 
     + ENERGY(1,1:))
              call write_trace_reals_vert(file_trace_bth, "6b. ENERGY", 
     + ENERGY(2,1:))
     
                     IF(UNITNO == GIBSON_POINTER .AND.
     +                                 GIBSON_BACKUP_POINTER /= 0) THEN

                        UNIT_CAP_AFTER_LOSSES(GIBSON_BACKUP_POINTER) =
     +                                     UNIT_CAP_AFTER_LOSSES(UNITNO)

                        IF(BLKNO(I) <= 1) THEN

                          CALL GET_GIBSON5_ENERGY(GIBSON_BLOCK1_ENERGY,
     +                                            GIBSON_BLOCK2_ENERGY,
     +                                                SEAS_HOURS,ISEAS,
     +                                                   UNITNO,BLK_MW,
     +                                                    ARRAY_POINTR)

                           ENRG = GIBSON_BLOCK1_ENERGY *
     +                        EAVAIL(GIBSON_BACKUP_POINTER) * (1.-
     +                          MAINTENANCE_RATE(GIBSON_BACKUP_POINTER))
               call write_energy_trace("DR_BOOTH", "dr_booth:0004", 
     +             ENRG)
     
                           ENERGY(1,UNITNO) = GIBSON_BLOCK1_ENERGY*
     +                        EAVAIL(GIBSON_BACKUP_POINTER) *
     +                           MAINTENANCE_RATE(GIBSON_BACKUP_POINTER)

               call write_energy_trace("DR_BOOTH", "dr_booth:0005", 
     +             energy(1,unitno))
                           EFFECTIVE_CAPACITY(1,UNITNO) =
     +                                                  ENERGY(1,UNITNO)

                           ENERGY(1,GIBSON_BACKUP_POINTER) = ENRG
               call write_energy_trace("DR_BOOTH", "dr_booth:0007", 
     +             ENRG) ! Set to rvalue
                           EFFECTIVE_CAPACITY(1,GIBSON_BACKUP_POINTER) =
     +                                                              ENRG
               call write_energy_trace("DR_BOOTH", "dr_booth:0007", 
     +             ENRG)
     
                        ELSE
                           ENRG = GIBSON_BLOCK2_ENERGY *
     +                        EAVAIL(GIBSON_BACKUP_POINTER) * (1.-
     +                          MAINTENANCE_RATE(GIBSON_BACKUP_POINTER))
               call write_energy_trace("DR_BOOTH", "dr_booth:0008", 
     +             ENRG)
                           ENERGY(2,UNITNO) = GIBSON_BLOCK2_ENERGY*
     +                        EAVAIL(GIBSON_BACKUP_POINTER) *
     +                           MAINTENANCE_RATE(GIBSON_BACKUP_POINTER)
                           EFFECTIVE_CAPACITY(2,UNITNO) =
     +                                                  ENERGY(2,UNITNO)

                           ENERGY(2,GIBSON_BACKUP_POINTER) = ENRG
                           EFFECTIVE_CAPACITY(2,GIBSON_BACKUP_POINTER) =
     +                                                              ENRG
                           LEFT(GIBSON_BACKUP_POINTER) = 1.0
                           RIGHT(GIBSON_BACKUP_POINTER) = 1.0
                        ENDIF ! BLKNO = 1
                     ENDIF ! GIBSON POINTER
                  ENDIF
                  IF(I == NBLOK2) CL_UNITS_ACTIVE = .FALSE.
                  CYCLE
               ENDIF
            ELSEIF(WABASH_VALLEY) THEN
               IF(UNITNO == GIBSON_BACKUP_POINTER) THEN
                  ARRAY_POINTR =
     +                CL_ALLOCATE_POINTR(CL_RESOURCE_ID(GIBSON_POINTER))
                  MWBLOK(I) = MWBLOK(I) *
     +                   (1.-AREA_CAPACITY_LOSSES(1,ISEAS,ARRAY_POINTR))
               ELSE
                  ARRAY_POINTR =
     +                        CL_ALLOCATE_POINTR(CL_RESOURCE_ID(UNITNO))
                  UNITCAP(UNITNO) = UNITCAP(UNITNO) + MWBLOK(I)
                  MWBLOK(I) = MWBLOK(I) *
     +                   (1.-AREA_CAPACITY_LOSSES(1,ISEAS,ARRAY_POINTR))
               ENDIF
               UNIT_CAP_AFTER_LOSSES(UNITNO) = MWBLOK(I) +
     +                                     UNIT_CAP_AFTER_LOSSES(UNITNO)
            ENDIF
         ENDIF

!        CONTRACTS SECTION

         IF(CONTRACTS_ACTIVE .AND.
     +                (.NOT. CL_UNITS_ACTIVE .OR. BLKNO(I) > 0)) THEN
            IF(CL_UNITS_ACTIVE) THEN
               IF(BLKNO(I) <= 1)  THEN
                  CL_DISPATCH_COST = SEGMENT_COST(I)+10.*DISPADJ(UNITNO)
               ELSE
                  CL_DISPATCH_COST=SEGMENT_COST(I)+10.*DISPADJ2(UNITNO)
               ENDIF
            ELSE
               CL_DISPATCH_COST = 10.**12
            ENDIF
            IF (.NOT. CL_UNITS_ACTIVE .OR.
     +             (CL_DISPATCH_COST > CONTRACT_DISPATCH_COST) ) THEN
               CUM_CONTRACT_CAP = 0.
               CALL CONTRACTS(CL_DISPATCH_COST,
     +                     LPROB(1,CURRENT),LODDUR,SEAS_HOURS,DX,
     +                     A,B,REMAINING_ENERGY,
     +                     ISTART,
     +                     CONTRACTS_ACTIVE,CONTRACT_DISPATCH_COST,
     +                     CONTRACT_ENERGY,CONTRACT_CAPACITY,
     +                     LAST_POINT,AVAILABLE_CONTRACT,
     +                     AVAILABLE_CONTRACT_NO,
     +                     MAXIMUM_ENERGY,MAXIMUM_CAPACITY,
     +                     CUM_CONTRACT_CAP,
     +                     REMAINING_ENRG_ONLY_ENRG,
     +                     TOTAL_CONTRACT_ENRG,ISEAS)
               CUMCAP = CUMCAP + CUM_CONTRACT_CAP
            ENDIF
         ENDIF

!        IF THE NEXT UNIT BLOCK IS THE LAST ENERGY SOURCE,
!        THEN ASSIGN REMAINING ENERGY TO THE FIRST CAPACITY
!        SEGMENT, AND STOP DISPATCHING THE SYSTEM.

         IF(CL_UNITS_ACTIVE) THEN
            IF(LDTYPE(UNITNO)=='L') THEN
               ENERGY(1,UNITNO) = MAX(0.,REMAINING_ENERGY)
               REMAINING_ENERGY = 0.
               CONTRACTS_ACTIVE = .FALSE.
               EXIT
            ENDIF

! 1/20/93 THE FOLLOWING CAUSES FIRST UNITS OR THE SHADOW UNIT TO BE
! SKIPPED IN THE DISPATCH ORDER

            IF(FUEL_INVENTORY_ACTIVE .AND.
     +                             SHADOW_UNIT_NUMBER(UNITNO) /= 0) THEN
               FUEL_ID = FUEL_INVENTORY_ID(FUEL_SUPPLY_ID(UNITNO))
               IF(UNITNO > SHADOW_UNIT_NUMBER(UNITNO)) THEN
                  FUEL_ID = FUEL_INVENTORY_ID(
     +                       FUEL_SUPPLY_ID(SHADOW_UNIT_NUMBER(UNITNO)))
                  IF(MMBTU_FUEL_BALANCE(FUEL_ID) > 0. .OR.
     +                     AVAILABLE_SHADOW_CAPACITY(UNITNO) == 0.) THEN
!
! SHADOW UNIT WITH FUEL AVAILABLE BY PASS THE UNIT
! 3/10/93. GAT. ADDED ZERO SECOND PRICE AS CONDITION FOR SKIPPING UNIT.
!
                     CL_UNITS_ACTIVE = .NOT. (I == NBLOK2)
                     CYCLE
                  ENDIF
                  MWBLOK(I) =  AVAILABLE_SHADOW_CAPACITY(UNITNO)
               ELSEIF(MMBTU_FUEL_BALANCE(FUEL_ID) <= 0.) THEN
                  CL_UNITS_ACTIVE = .NOT. (I == NBLOK2)
                  IF(SBTUCT(SHADOW_UNIT_NUMBER(UNITNO)) /= 0.)
     +             AVAILABLE_SHADOW_CAPACITY(SHADOW_UNIT_NUMBER(UNITNO))
     +                                                        =MWBLOK(I)
                  CYCLE
               ENDIF
            ENDIF
            ENRG = 0.0
               call write_energy_trace("DR_BOOTH", "dr_booth:0009", 
     +             ENRG)
            BLK_MW = MWBLOK(I)
            A = B
            IF(BLK_MW == 0.0) CYCLE
            IF(PROD_METHOD == D_RATE) THEN

! NEED TO LIMIT UNIT CAPACITY IF ON A TIE

               TIE_CAPACITY = EAVAIL(UNITNO) *
     +                              (1.-MAINTENANCE_RATE(UNITNO))*BLK_MW
               IF(TIE_CONSTRAINT_GROUP(UNITNO) /= 0) THEN
                  TIE_GROUP = TIE_CONSTRAINT_GROUP(UNITNO)
                  TIE_CAPACITY = MIN(TIE_GROUP_LIMIT(TIE_GROUP)-
     +                                     CURRENT_TIE_LEVEL(TIE_GROUP),
     +                                     TIE_CAPACITY)
                  CURRENT_TIE_LEVEL(TIE_GROUP) = MIN(TIE_CAPACITY +
     +                                     CURRENT_TIE_LEVEL(TIE_GROUP),
     +                                     TIE_GROUP_LIMIT(TIE_GROUP))

                  IF(TIE_CAPACITY < 0.0001) CYCLE
               ENDIF
               B = A + TIE_CAPACITY
               IF(A < PEAK) THEN
                  IF(B < BASE .OR.
     +                  (REALLY_KEPCO .AND. LDTYPE(UNITNO) == 'N')) THEN
                     LEFT_SAVE(I) = 1.
                     RIGHT_SAVE(I) = 1.
                     IF(REALLY_KEPCO .AND. LDTYPE(UNITNO) == 'N') THEN
                        B = A + BLK_MW
                        ENRG = MIN(BLK_MW,REMAINING_ENERGY)
               call write_energy_trace("DR_BOOTH", "dr_booth:0010", 
     +             ENRG)
                        REMAINING_ENERGY = REMAINING_ENERGY - ENRG
                        ENRG = EAVAIL(UNITNO) *
     +                           (1. - MAINTENANCE_RATE(UNITNO))* BLK_MW
               call write_energy_trace("DR_BOOTH", "dr_booth:0011", 
     +             ENRG)
                        WC_PERIOD_MAINTENANCE_ENERGY =
     +                              WC_PERIOD_MAINTENANCE_ENERGY +
     +                                 MAINTENANCE_RATE(UNITNO) * BLK_MW
                        WC_PERIOD_EMERGENCY_ENERGY =
     +                                      WC_PERIOD_EMERGENCY_ENERGY +
     +                           (1. - MAINTENANCE_RATE(UNITNO)) *
     +                                      (1.-EAVAIL(UNITNO)) * BLK_MW
                        WC_UNIT_NO = UNITNO
                     ELSE
                        ENRG = B - A
               call write_energy_trace("DR_BOOTH", "dr_booth:0012", 
     +             ENRG)
                        ENRG = MIN(ENRG,REMAINING_ENERGY)
               call write_energy_trace("DR_BOOTH", "dr_booth:0013", 
     +             ENRG)
                        IF(SHADOW_UNIT_NUMBER(UNITNO) > UNITNO) THEN
                           FUEL_ID =
     +                         FUEL_INVENTORY_ID(FUEL_SUPPLY_ID(UNITNO))
                           TEMP_HEAT = MMBTU_FUEL_BALANCE(FUEL_ID)
                           CALL SHADOW_FUEL_INVENTORY(ENRG,
     +                           MMBTU_FUEL_BALANCE(FUEL_ID),
     +                           1.0,
     +                           COEFF(1,UNITNO),
     +                           HEAT_RATE_FACTOR(UNITNO),
     +                           SEAS_HOURS,B,A,
     +                           LEFT_SAVE(I),RIGHT_SAVE(I),
     +                           AVAILABLE_SHADOW_CAPACITY(
     +                                   SHADOW_UNIT_NUMBER(UNITNO)),
     +                           MWBLOK(I),
     +                           LODDUR,LPROB2,
     +                           EMISS_BLENDING_RATE(UNITNO),
     +                           SBTUCT(UNITNO))
                           IF(FUEL_USAGE_REPORT_ACTIVE)
     +                        CALL SEASON_FUEL_INVENTORY_REPORT(
     +                                      UNITNM(UNITNO),
     +                                      FUEL_SUPPLY_ID(UNITNO),
     +                                      TEMP_HEAT,
     +                                      MMBTU_FUEL_BALANCE(FUEL_ID),
     +                                      BLKNO(I))
                        ENDIF
                        REMAINING_ENERGY = REMAINING_ENERGY - ENRG
                     ENDIF
                     IF(BLKNO(I) <= 1) THEN
                        ENERGY(1,UNITNO) = ENRG
                        EFFECTIVE_CAPACITY(1,UNITNO) = ENRG
                     ELSE
                        ENERGY(2,UNITNO) = ENRG
                        EFFECTIVE_CAPACITY(2,UNITNO) = ENRG
                        LEFT(UNITNO) = 1.0
                        RIGHT(UNITNO) = 1.0
                     ENDIF
                  ELSE
                     CALL CALENRG(LODDUR,LPROB2,A,B,ENRG,
     +                       PEAK,DX,LEFT(UNITNO),RIGHT(UNITNO),ISTART)
                     LEFT_SAVE(I) = LEFT(UNITNO)
                     RIGHT_SAVE(I) = RIGHT(UNITNO)
                     IF(SHADOW_UNIT_NUMBER(UNITNO) > UNITNO) THEN
                        FUEL_ID =
     +                      FUEL_INVENTORY_ID(FUEL_SUPPLY_ID(UNITNO))
                        TEMP_HEAT = MMBTU_FUEL_BALANCE(FUEL_ID)
                        CALL SHADOW_FUEL_INVENTORY(ENRG,
     +                           MMBTU_FUEL_BALANCE(FUEL_ID),
     +                           1.0,
     +                           COEFF(1,UNITNO),
     +                           HEAT_RATE_FACTOR(UNITNO),
     +                           SEAS_HOURS,B,A,
     +                           LEFT_SAVE(I),RIGHT_SAVE(I),
     +                           AVAILABLE_SHADOW_CAPACITY(
     +                                   SHADOW_UNIT_NUMBER(UNITNO)),
     +                           MWBLOK(I),
     +                           LODDUR,LPROB2,
     +                           EMISS_BLENDING_RATE(UNITNO),
     +                           SBTUCT(UNITNO))
                        IF(FUEL_USAGE_REPORT_ACTIVE)
     +                           CALL SEASON_FUEL_INVENTORY_REPORT(
     +                                      UNITNM(UNITNO),
     +                                      FUEL_SUPPLY_ID(UNITNO),
     +                                      TEMP_HEAT,
     +                                      MMBTU_FUEL_BALANCE(FUEL_ID),
     +                                      BLKNO(I))
                        RIGHT(UNITNO) = RIGHT_SAVE(I)
                     ENDIF
                     ENRG = MIN(ENRG,REMAINING_ENERGY)
               call write_energy_trace("DR_BOOTH", "dr_booth:0014", 
     +             ENRG)
                     REMAINING_ENERGY = REMAINING_ENERGY - ENRG
                     IF(BLKNO(I) <= 1) THEN
                        ENERGY(1,UNITNO) = ENRG
                        EFFECTIVE_CAPACITY(1,UNITNO) = B - A
                     ELSE
                        ENERGY(2,UNITNO) = ENRG
                        EFFECTIVE_CAPACITY(2,UNITNO) = B - A
                     ENDIF
                  ENDIF
               ENDIF
            ELSE
               BLKNUM = MAX(1,BLKNO(I))
               CAPBLK = BLK_MW * (1. - MAINTENANCE_RATE(UNITNO))
               IF(A > PEAK .AND. PROD_METHOD == 3) THEN
                  CAPBLK = CAPBLK * TEMPEA(BLKNUM,UNITNO)
                  TEMPEA(BLKNUM,UNITNO) = 1.0
                  EA = 1.0
               ELSE
                  EA = TEMPEA(BLKNUM,UNITNO)
               ENDIF

! NEED TO LIMIT UNIT CAPACITY IF ON A TIE

               IF(TIE_CONSTRAINT_GROUP(UNITNO) /= 0) THEN
                  TIE_GROUP = TIE_CONSTRAINT_GROUP(UNITNO)
                  TIE_CAPACITY = MIN(TIE_GROUP_LIMIT(TIE_GROUP)-
     +                                     CURRENT_TIE_LEVEL(TIE_GROUP),
     +                                     CAPBLK)
                  CURRENT_TIE_LEVEL(TIE_GROUP) = MIN(TIE_CAPACITY +
     +                                     CURRENT_TIE_LEVEL(TIE_GROUP),
     +                                     TIE_GROUP_LIMIT(TIE_GROUP))

                  IF(TIE_CAPACITY < 0.0001) CYCLE
                  CAPBLK = TIE_CAPACITY
               ENDIF
               CAPSYS = CAPSYS - BLK_MW + CAPBLK
               IF(CAPBLK > 0. .AND. REMAINING_ENERGY > 0.) THEN
                  B = A + CAPBLK
                  IF(A < CAPSYS) THEN
                     IF(B < BASE .OR.
     +                  (REALLY_KEPCO .AND. LDTYPE(UNITNO) == 'N')) THEN
                        LEFT_SAVE(I) = 1.
                        RIGHT_SAVE(I) = 1.
                        IF(REALLY_KEPCO .AND. LDTYPE(UNITNO)=='N') THEN
                           B = A + BLK_MW
                           ENRG = BLK_MW
               call write_energy_trace("DR_BOOTH", "dr_booth:0015", 
     +             ENRG)
                           REMAINING_ENERGY = MAX(0.,
     +                                            REMAINING_ENERGY-ENRG)
                           ENRG = (1.-MAINTENANCE_RATE(UNITNO))* BLK_MW
               call write_energy_trace("DR_BOOTH", "dr_booth:0016", 
     +             ENRG)
                           WC_PERIOD_MAINTENANCE_ENERGY =
     +                              WC_PERIOD_MAINTENANCE_ENERGY +
     +                                 MAINTENANCE_RATE(UNITNO) * BLK_MW
                           WC_PERIOD_EMERGENCY_ENERGY =
     +                                WC_PERIOD_EMERGENCY_ENERGY +
     +                                  (1.-MAINTENANCE_RATE(UNITNO)) *
     +                                                  (1.-EA) * BLK_MW
                           EA = 1.0  !STOPS CONVOLUTION FOR WOLF CREEK
                           WC_UNIT_NO = UNITNO
                        ELSE
                           ENRG = CAPBLK
               call write_energy_trace("DR_BOOTH", "dr_booth:0017", 
     +             ENRG)
                           IF(SHADOW_UNIT_NUMBER(UNITNO) > UNITNO) THEN
                              FUEL_ID =
     +                         FUEL_INVENTORY_ID(FUEL_SUPPLY_ID(UNITNO))
                              TEMP_HEAT = MMBTU_FUEL_BALANCE(FUEL_ID)
                              CALL SHADOW_FUEL_INVENTORY(ENRG,
     +                              MMBTU_FUEL_BALANCE(FUEL_ID),
     +                              EA,
     +                              COEFF(1,UNITNO),
     +                              HEAT_RATE_FACTOR(UNITNO),
     +                              SEAS_HOURS,B,A,
     +                              LEFT_SAVE(I),RIGHT_SAVE(I),
     +                              AVAILABLE_SHADOW_CAPACITY(
     +                                      SHADOW_UNIT_NUMBER(UNITNO)),
     +                              MWBLOK(I),
     +                              LODDUR,LPROB(1,CURRENT),
     +                              EMISS_BLENDING_RATE(UNITNO),
     +                              SBTUCT(UNITNO))
                              IF(FUEL_USAGE_REPORT_ACTIVE)
     +                              CALL SEASON_FUEL_INVENTORY_REPORT(
     +                                      UNITNM(UNITNO),
     +                                      FUEL_SUPPLY_ID(UNITNO),
     +                                      TEMP_HEAT,
     +                                      MMBTU_FUEL_BALANCE(FUEL_ID),
     +                                      BLKNO(I))
                              CAPBLK = B - A
                           ENDIF
                           REMAINING_ENERGY = MAX(0.,
     +                                         REMAINING_ENERGY-EA*ENRG)
                        ENDIF
                        IF(BLKNO(I) <= 1) THEN
                           ENERGY(1,UNITNO) = ENRG
                           EFFECTIVE_CAPACITY(1,UNITNO) = ENRG
                        ELSE
                           ENERGY(2,UNITNO) = ENRG
                           EFFECTIVE_CAPACITY(2,UNITNO) = ENRG
                           LEFT(UNITNO) = 1.0
                           RIGHT(UNITNO) = 1.0
                        ENDIF
                     ELSE
                        CALL CALENRG(LODDUR,LPROB(1,CURRENT),A,B,ENRG,
     +                     CAPSYS,DX,LEFT(UNITNO),RIGHT(UNITNO),ISTART)

! ATTEMPT TO TRANSACT STARTING AT THE POINT ON THE ELDC WHERE
! THE UNIT IS LOADED.

                        LEFT_SAVE(I) = LEFT(UNITNO)
                        RIGHT_SAVE(I) = RIGHT(UNITNO)
                        IF(SHADOW_UNIT_NUMBER(UNITNO) > UNITNO) THEN
                           FUEL_ID =
     +                         FUEL_INVENTORY_ID(FUEL_SUPPLY_ID(UNITNO))
                           TEMP_HEAT = MMBTU_FUEL_BALANCE(FUEL_ID)
                           CALL SHADOW_FUEL_INVENTORY(ENRG,
     +                              MMBTU_FUEL_BALANCE(FUEL_ID),
     +                              EA,
     +                              COEFF(1,UNITNO),
     +                              HEAT_RATE_FACTOR(UNITNO),
     +                              SEAS_HOURS,B,A,
     +                              LEFT_SAVE(I),RIGHT_SAVE(I),
     +                              AVAILABLE_SHADOW_CAPACITY(
     +                                      SHADOW_UNIT_NUMBER(UNITNO)),
     +                              MWBLOK(I),
     +                              LODDUR,LPROB(1,CURRENT),
     +                              EMISS_BLENDING_RATE(UNITNO),
     +                              SBTUCT(UNITNO))
                           IF(FUEL_USAGE_REPORT_ACTIVE)
     +                           CALL SEASON_FUEL_INVENTORY_REPORT(
     +                                      UNITNM(UNITNO),
     +                                      FUEL_SUPPLY_ID(UNITNO),
     +                                      TEMP_HEAT,
     +                                      MMBTU_FUEL_BALANCE(FUEL_ID),
     +                                      BLKNO(I))
                           CAPBLK = B - A
                           RIGHT(UNITNO) = RIGHT_SAVE(I)
                        ENDIF
                        ENRG = MIN(ENRG,REMAINING_ENERGY/EA)
               call write_energy_trace("DR_BOOTH", "dr_booth:0018", 
     +             ENRG)
                        REMAINING_ENERGY = MAX(0.,
     +                                         REMAINING_ENERGY-EA*ENRG)
                        IF(BLKNO(I) <= 1) THEN
                           ENERGY(1,UNITNO) = ENRG
                           EFFECTIVE_CAPACITY(1,UNITNO) = CAPBLK
                        ELSE
                           ENERGY(2,UNITNO) = ENRG
                           EFFECTIVE_CAPACITY(2,UNITNO) = CAPBLK
                        ENDIF
                     ENDIF
                  ENDIF
!
                  IF((REMAINING_ENERGY > 0.) .AND.
     +                       (I < NBLOK2 .OR. CONTRACTS_ACTIVE)) THEN
                     CAPON = CAPON + CAPBLK
                     IF(UNIT(I+1) /= UNITNO .OR.
     +                      (B > PEAK .AND. PROD_METHOD == 3)) THEN
                        IF(EA <= .999 .AND. EA >= .001) THEN
                           CUMCAP = CUMCAP + CAPON
                           LIMR = MIN(PEAK+CUMCAP,CAPSYS) + DX
                           CALL CONVOL(CAPON,EA,CURRENT,LIMR,
     +                                          LPROB,LODDUR,LAST_POINT)
                        ELSEIF(EA < .001) THEN
                           B = A
                        ENDIF
                        IF(LOLP_REPORT_ACTIVE .AND. ISEAS == PEAK_MONTH)
     +                     CALL LOLP_ANALYSIS(B,LPROB(1,CURRENT),LODDUR)
                        CAPON = 0.
                     ENDIF
                  ELSEIF(I == NBLOK2 .AND. LOLP_REPORT_ACTIVE .AND.
     +                                            PROD_METHOD == 3) THEN
                     CAPON = CAPON + CAPBLK
                     IF(EA <= .999 .AND. EA >= .001) THEN
                        CUMCAP = CUMCAP + CAPON
                        LIMR = MIN(PEAK+CUMCAP,CAPSYS) + DX
                        CALL CONVOL(CAPON,EA,CURRENT,LIMR,
     +                                          LPROB,LODDUR,LAST_POINT)
                     ELSEIF(EA < .001) THEN
                        B = A
                     ENDIF
                     IF(ISEAS == PEAK_MONTH)
     +                  CALL LOLP_ANALYSIS(B,LPROB(1,CURRENT),LODDUR)
                     CAPON = 0.
                  ENDIF
               ENDIF !END BOOTH REMAINING ENERGY /= 0
            ENDIF   ! END THE DRATE BOOTH IF
            IF(.NOT. WABASH_VALLEY) UNITCAP(UNITNO) = UNITCAP(UNITNO) +
     +                                                         MWBLOK(I)
            IF(I == NBLOK2) CL_UNITS_ACTIVE = .FALSE.
            REMAINING_ENERGY_FOR_BLOCK(I) = REMAINING_ENERGY
            LAST_B_FOR_BLOCK(I) = B
         ENDIF   ! END THE CL_UNITS_ACTIVE IF
      ENDDO ! DISPATCHING LOOP (CONTRACTS OR CL UNITS ACTIVE)
      IF(WABASH_VALLEY) THEN

!        WABASH 4 AREA ALLOCATION OF PSI SEASONAL ENERGY
!        1/17/95. GAT. MODIFIED AND MOVED INTO SUBROUTINE.

         CALL PSI_LAST_RESOURCE_CALC(ISEAS,SEAS_HOURS,ENERGY)

         IF(get_NUNITS() > 0) THEN
            ALLOCATE(CL_LOSSES(get_nunits()))
            CL_LOSSES = 0.
            CALL WABASH_LOSSES(YR,ISEAS,ENERGY,UNITCAP,CL_RESOURCE_ID,
     + get_NUNITS(),SEAS_HOURS,'CL',
     +                          LDTYPE,
     +                          ONLINE,OFLINE,
     +                          date1,DATE2,UNITNM,CL_LOSSES)
         ENDIF
         IF(NUMBER_OF_CONTRACTS > 0.) THEN
            ALLOCATE(CT_LOSSES(NUMBER_OF_CONTRACTS))
            CT_LOSSES = 0.
! MOVED FROM INSIDE TRANSACT. 3/14/98. GAT.
            IF((YES_RUN_TRANSACT() .AND.
     +  TRANSACT_ACTIVE_THIS_MONTH)) THEN
               CALL WABASH_TRANSACT_VARIABLES(ISEAS,
     +                                    WABASH_TRANS_BUY_ENERGY,
     +                                    WABASH_TRANS_BUY_RATE,
     +                                    WABASH_TRANS_SELL_ENERGY,
     +                                    WABASH_TRANS_SELL_RATE,
     +                                    HOURS_INCREMENT)
               REMAINING_ENERGY = REMAINING_ENERGY -
     +             (WABASH_TRANS_BUY_ENERGY - WABASH_TRANS_SELL_ENERGY)
               IF(WABASH_TRANS_BUY_ENERGY > 0. .OR.
     +                               WABASH_TRANS_SELL_ENERGY > 0.) THEN

                  DO I = 1, NUMBER_OF_CONTRACTS
                     IF(CNTRTYPE(I) == 'K') THEN ! TRANSACT BUY
                        CONTRACT_FIXED_COST(I) = 0.
                        CT_ANNUAL_FIXED_COST(I) = 0.
                        CT_MONTHLY_1ST_ENERGY_PRICE(I) =
     +                                             WABASH_TRANS_BUY_RATE
                        CT_ENERGY_COST_ADDER(I) = 0.

                        CONTRACT_ENERGY(I) = WABASH_TRANS_BUY_ENERGY

                        CT_SECOND_ENERGY_PRICE(I) = 0.

                     ELSEIF(CNTRTYPE(I) == 'L') THEN ! TRANSACT SELL
                        CONTRACT_FIXED_COST(I) = 0.
                        CT_ANNUAL_FIXED_COST(I) = 0.
                        CT_MONTHLY_1ST_ENERGY_PRICE(I) =
     +                                            WABASH_TRANS_SELL_RATE
                        CT_ENERGY_COST_ADDER(I) = 0.

                        CONTRACT_ENERGY(I) =  WABASH_TRANS_SELL_ENERGY
                        CT_SECOND_ENERGY_PRICE(I) = 0.

                     ENDIF
                  ENDDO ! CONTRACTS
               ENDIF ! IF THERE WAS TRANSACTION ENERGY
            ENDIF ! WABASH INSIDE TRANSACT

            CALL WABASH_CT_LOSSES(YR,ISEAS,CONTRACT_ENERGY,
     +                         CONTRACT_CAPACITY,
     +                         CT_RESOURCE_ID,
     +                         NUMBER_OF_CONTRACTS,
     +                         SEAS_HOURS,'CT',
     +                         CNTRTYPE,
     +                         CNTR_ON_LI,CNTR_OFF_LI,
     +                         date1,DATE2,CNTRNM,CT_LOSSES)
         ENDIF
      ENDIF ! WABASH VALLEY

! ALLOCATE REMAINING ENERGY ONLY CONTRACTS AND PRICE THAT ENERGY

      IF(ENRG_FROM_ENRG_ONLY_CONTRACTS > 0.) THEN
         IF(REALLY_KEPCO) THEN
            USED_ENRG_ONLY_ENERGY =
     +                          ENRG_FROM_ENRG_ONLY_CONTRACTS/SEAS_HOURS
         ELSE
            USED_ENRG_ONLY_ENERGY =
     +                         ENRG_FROM_ENRG_ONLY_CONTRACTS/SEAS_HOURS-
     +                                          REMAINING_ENRG_ONLY_ENRG
         ENDIF
         TOTAL_CONTRACT_ENRG = TOTAL_CONTRACT_ENRG+USED_ENRG_ONLY_ENERGY
         CONTRACT = 1
         DOWHILE ((USED_ENRG_ONLY_ENERGY > 0) .AND.
     +                              (CONTRACT <= NUMBER_OF_CONTRACTS))
            IF(AVAILABLE_CONTRACT(CONTRACT) .AND.
     +                     CNTR_CAPACITY_SWITCH(CONTRACT) == "E") THEN

               IF(IS_3808_ACTIVE()) THEN
                  CALL GET_X_Y_Z_CONTRACT(X_CONTRACT,
     +                                            Y_CONTRACT,Z_CONTRACT)
                  IF(X_CONTRACT > 0 .AND.
     +                      CONTRACT_CAPACITY(X_CONTRACT) > 0. .AND.
     +                        CONTRACT_ENERGY(X_CONTRACT) <
     +                           CONTRACT_CAPACITY(X_CONTRACT) .AND.
     +                        CONTRACT_VARIABLE_COST(X_CONTRACT) <
     +                           CONTRACT_VARIABLE_COST(CONTRACT) ) THEN
                     TEMP_ENERGY =
     +                  MAX(0.,MIN(USED_ENRG_ONLY_ENERGY,
     +                     MIN(CONTRACT_CAPACITY(X_CONTRACT),
     +                       MAXIMUM_ENERGY(X_CONTRACT)/SEAS_HOURS) -
     +                                     CONTRACT_ENERGY(X_CONTRACT)))
                     CONTRACT_ENERGY(X_CONTRACT) =
     +                         CONTRACT_ENERGY(X_CONTRACT) + TEMP_ENERGY
                     USED_ENRG_ONLY_ENERGY = USED_ENRG_ONLY_ENERGY -
     +                                                       TEMP_ENERGY
                  ENDIF ! X_CONTRACT
                  IF(Y_CONTRACT > 0 .AND.
     +                      CONTRACT_CAPACITY(Y_CONTRACT) > 0. .AND.
     +                        CONTRACT_ENERGY(Y_CONTRACT) <
     +                           CONTRACT_CAPACITY(Y_CONTRACT) .AND.
     +                        CONTRACT_VARIABLE_COST(Y_CONTRACT) <
     +                           CONTRACT_VARIABLE_COST(CONTRACT) ) THEN
                     TEMP_ENERGY =
     +                  MAX(0.,MIN(USED_ENRG_ONLY_ENERGY,
     +                     MIN(CONTRACT_CAPACITY(Y_CONTRACT),
     +                       MAXIMUM_ENERGY(Y_CONTRACT)/SEAS_HOURS) -
     +                                     CONTRACT_ENERGY(Y_CONTRACT)))
                     CONTRACT_ENERGY(Y_CONTRACT) =
     +                         CONTRACT_ENERGY(Y_CONTRACT) + TEMP_ENERGY
                     USED_ENRG_ONLY_ENERGY = USED_ENRG_ONLY_ENERGY -
     +                                                       TEMP_ENERGY
                  ENDIF ! Y_CONTRACT
                  IF(Z_CONTRACT > 0 .AND.
     +                      CONTRACT_CAPACITY(Z_CONTRACT) > 0. .AND.
     +                        CONTRACT_ENERGY(Z_CONTRACT) <
     +                           CONTRACT_CAPACITY(Z_CONTRACT) .AND.
     +                        CONTRACT_VARIABLE_COST(Z_CONTRACT) <
     +                           CONTRACT_VARIABLE_COST(CONTRACT) ) THEN
                     TEMP_ENERGY =
     +                  MAX(0.,MIN(USED_ENRG_ONLY_ENERGY,
     +                     MIN(CONTRACT_CAPACITY(Z_CONTRACT),
     +                       MAXIMUM_ENERGY(Z_CONTRACT)/SEAS_HOURS) -
     +                                     CONTRACT_ENERGY(Z_CONTRACT)))
                     CONTRACT_ENERGY(Z_CONTRACT) =
     +                         CONTRACT_ENERGY(Z_CONTRACT) + TEMP_ENERGY
                     USED_ENRG_ONLY_ENERGY = USED_ENRG_ONLY_ENERGY -
     +                                                       TEMP_ENERGY
                  ENDIF ! Z_CONTRACT
               ENDIF

               CONTRACT_ENERGY(CONTRACT) = MIN(USED_ENRG_ONLY_ENERGY,
     +                              MAXIMUM_ENERGY(CONTRACT)/SEAS_HOURS)
               USED_ENRG_ONLY_ENERGY = USED_ENRG_ONLY_ENERGY -
     +                                         CONTRACT_ENERGY(CONTRACT)
            ENDIF
            CONTRACT = CONTRACT + 1
         ENDDO
      ENDIF
!
      PENRG = 0.
      PMMBTUS = 0.
      PFUELCST = 0.
      P_PUR_POWER_COST = 0.
      P_PUR_POWER_COST_VAR = 0.
      P_PUR_POWER_COST_FIXED = 0.
      P_PUR_ENRG = PUR_ENRG_FROM_TRANSACT 
      PVARCOST = 0.
      PFIXCOST = 0.
      P_NUCLEAR_FUEL_COST = 0.
      IF(LOLP_REPORT_ACTIVE) THEN
         CALL CALCULATE_MONTHLY_LOLP(ISEAS,SEAS_HOURS,B,
     +                                          LPROB(1,CURRENT),LODDUR)
         CALL LOLP_ANALYSIS_REPORT(ISEAS,gc_end_point,
     +                    PEAK_MONTH,globecom_YEAR+get_BASE_YEAR())
      ENDIF
      IF(LAST_ELDC_REPORT()) THEN
         CALL WRITE_LAST_ELDC(LODDUR,LPROB,LAST_POINT,CURRENT,
     +           gc_end_point,globecom_YEAR+get_BASE_YEAR(),ISEAS,
     +                          REMAINING_ENERGY_FOR_BLOCK,NBLOK2,
     +                          LAST_B_FOR_BLOCK)
      ENDIF
      IF(ECON_SWITCH()) THEN
         IF(ECON_SALES()) THEN
            CALL SRP_NEW_ECONOMY_INTERCHANGE(ENERGY,
     +         NBLOK2,BLKNO,
     +         ANNUAL_ECONOMY_BOUGHT,ANNUAL_ECONOMY_SOLD,
     +         ANNUAL_ECONOMY_COST,ANNUAL_ECONOMY_REVENUE,
     +         UNIT,SEAS_HOURS,MAINTENANCE_RATE,
     +         UNITNM,PERIOD_COUNTER,
     +         TRANSACTION_BUY_SPREAD,TRANSACTION_SELL_SPREAD,
     +         MONTHLY_ECONOMY_BOUGHT,MONTHLY_ECONOMY_COST,
     +         MONTHLY_ECONOMY_SOLD,MONTHLY_ECONOMY_REVENUE,
     +         MWBLOK,EXCESS_ENERGY_SALES,TEMPEA,
     + get_NUNITS(),EAVAIL,
     +         SRP_SEGMENT_COST,ISEAS)
         ELSE

            CALL NEW_ECONOMY_INTERCHANGE(AVE_RUNNING_RATE,
     +         RR_OBS,NBLOK2,ENERGY,EAVAIL,
     +         BLKNO,ANNUAL_ECONOMY_BOUGHT,
     +         ANNUAL_ECONOMY_SOLD,
     +         ANNUAL_ECONOMY_COST,ANNUAL_ECONOMY_REVENUE,
     +         UNIT,SEAS_HOURS,LEFT_SAVE,RIGHT_SAVE,
     +         MAINTENANCE_RATE,YR,LEFT,RIGHT,IMPORT_CAP,
     +         EXPORT_CAP,PERIOD_COUNTER,
     +         TRANSACTION_BUY_SPREAD,TRANSACTION_SELL_SPREAD,
     +         MONTHLY_ECONOMY_BOUGHT,MONTHLY_ECONOMY_COST,
     +         MONTHLY_ECONOMY_SOLD,MONTHLY_ECONOMY_REVENUE,
     +         ECONOMY_TRANS_TYPE,
     +         ns_service_decs%CL_POOL_FRAC_OWN,MWBLOK,LODDUR,
     + get_NUNITS(),SRP_SEGMENT_COST,
     +         TEMPEA,GENGRP)
         ENDIF
      ENDIF

! Used to check the optimization of fuel switching going from right to
! left.  4/26/93 MSG

      IF(ALLOCATE_FUEL_RIGHT_TO_LEFT) THEN
         DISPATCH_BLOCK = NBLOK + 1
      ELSE
         DISPATCH_BLOCK = 0
      ENDIF

      TRANSACT_C_ACTIVE = GET_TRANSACT_C_STATUS()
      YES_DAILY_THERMAL_RPT_ACTIVE =
     +               DAILY_THERMAL_REPORT_ACTIVE(START_DATE,END_DATE)

      DO
         IF(ALLOCATE_FUEL_RIGHT_TO_LEFT) THEN
            DISPATCH_BLOCK = DISPATCH_BLOCK - 1
         ELSE
            DISPATCH_BLOCK = DISPATCH_BLOCK + 1
         ENDIF
         IF(DISPATCH_BLOCK == 0 .OR.  DISPATCH_BLOCK == NBLOK + 1) EXIT
         IF(BLKNO(DISPATCH_BLOCK) > 1) CYCLE
         I = UNIT(DISPATCH_BLOCK)
         GGI = GENGRP(I)
         MGI = GENGRP(I)
         IF(MGI < 1 .OR. MGI > MAX_MONTHLY_GROUPS) MGI = 0
         IF(GGI < 1 .OR. GGI > MAX_REPORTING_GROUPS) GGI = 0
         IF(PROD_METHOD == D_RATE .OR. FACET_ACTIVE) THEN
            UNIT_ENRG = ENERGY(1,I) + ENERGY(2,I)
         ELSE
            UNIT_ENRG = (ENERGY(1,I) * TEMPEA(1,I) +
     +                   ENERGY(2,I) * TEMPEA(2,I))
         ENDIF
         IF(WABASH_VALLEY) UNIT_ENRG = UNIT_ENRG + CL_LOSSES(I)
         UNIT_ENRG = UNIT_ENRG * SEAS_HOURS

         J = 2      
         IF(EXPENSE_COLLECTION(I) == 'A') J = 1
         IF(EXPENSE_COLLECTION(I) == 'N') J = 3

! ACCOUNT FOR FIXED COSTS

         IF (ONLINE(I) <= DATE2 .AND. OFLINE(I) >= DATE1) THEN

            SEASON_CAPACITY(GGI) = SEASON_CAPACITY(GGI)+UNITCAP(I)
            MONTHLY_GROUP_REPORT(ISEAS,MGI,Capacity) =
     +             MONTHLY_GROUP_REPORT(ISEAS,MGI,Capacity) + UNITCAP(I)
            MONTHS_ON_LINE = (FLOAT(MIN(OFLINE(I),DATE2)-
     +                               MAX(ONLINE(I),date1)) + 1.)/12.
            CL_AI_INVESTMENT(I) = CL_AI_INVESTMENT(I) +
     +             CL_AI_ENERGY_RATE(I) * UNIT_ENRG +
     +             1000. * MW(2,I) * CL_AI_CAPACITY_RATE(I) *
     +                                                    MONTHS_ON_LINE
            UNIT_FIXED_COST = 1000. * (FIXED_COST(I) * MW(2,I) +
     +                                1000. * ANNUAL_CL_FIXED_COST(I)) *
     +                                                    MONTHS_ON_LINE
            CL_UNIT_MONTHLY_FIXED_COST(I) = UNIT_FIXED_COST
            GROUP_FIXED_OM(GGI) = GROUP_FIXED_OM(GGI) + UNIT_FIXED_COST
            MONTHLY_GROUP_REPORT(ISEAS,MGI,fixed_om) =
     +                  MONTHLY_GROUP_REPORT(ISEAS,MGI,fixed_om) +
     +                                                   UNIT_FIXED_COST

            ANNUAL_CL_UNIT_CAPACITY(I) = ANNUAL_CL_UNIT_CAPACITY(I) +
     +                                     MW(2,I) * MONTHS_ON_LINE

            ANNUAL_CL_UNIT_FIXED_COST(I) = UNIT_FIXED_COST +
     +                                  ANNUAL_CL_UNIT_FIXED_COST(I)

! SEND COST INFORMATION FOR USE BY MONTHLY MIDAS

            VOID_LOGICAL = MON_MDS_CL_FIXED(ISEAS,I,
     +                                    UNIT_FIXED_COST,MW(2,I))

            IF(ns_service_decs%CL_POOL_FRAC_OWN(I) < 100.) THEN
           UTILITY_PORTION = ns_service_decs%CL_POOL_FRAC_OWN(I) / 100.

               POOL_PORTION = 1. - UTILITY_PORTION

               CLASS_ASSIGNED_FIXED_COST(2) =
     +                     CLASS_ASSIGNED_FIXED_COST(2) +
     +                     UNIT_FIXED_COST * POOL_PORTION
               UNIT_FIXED_COST = UNIT_FIXED_COST * UTILITY_PORTION
            ENDIF
            IF(EXPENSE_ASSIGNMENT(I) /= 'P') THEN
               PFIXCOST = PFIXCOST + UNIT_FIXED_COST
            ELSE
               P_PUR_POWER_COST_FIXED = P_PUR_POWER_COST_FIXED +
     +                                  UNIT_FIXED_COST
               PURCHASED_POWER_COSTS(J) = PURCHASED_POWER_COSTS(J) +
     +                                       UNIT_FIXED_COST
            ENDIF
         ENDIF

         ECO_SALES_REV_FROM(I) = ECO_SALES_REV_FROM(I) +
     +                                         MON_ECO_SALES_REV_FROM(I)
         ECO_SALES_ENRG_FROM(I) = ECO_SALES_ENRG_FROM(I) +
     +                                        MON_ECO_SALES_ENRG_FROM(I)
         ECO_PUCH_COST_FROM(I) = ECO_PUCH_COST_FROM(I) +
     +                                         MON_ECO_PUCH_COST_FROM(I)
         ECO_PUCH_ENRG_FROM(I) = ECO_PUCH_ENRG_FROM(I) +
     +                                         MON_ECO_PUCH_ENRG_FROM(I)
         IF(REVENUE_GENERATING_CAPACITY(I) > 0.01 .OR.
     +                                            LDTYPE(I) /= 'L') THEN
            MONTHLY_GROUP_REPORT(ISEAS,MGI,Wholesale Rev) =
     +               MONTHLY_GROUP_REPORT(ISEAS,MGI,Wholesale Rev) +
     +                                    MON_ECO_SALES_REV_FROM(I)
            MONTHLY_GROUP_REPORT(ISEAS,MGI,Wholesale Sell) =
     +               MONTHLY_GROUP_REPORT(ISEAS,MGI,Wholesale Sell) +
     +                                   MON_ECO_SALES_ENRG_FROM(I)
            MONTHLY_GROUP_REPORT(ISEAS,MGI,Wholesale Exp) =
     +               MONTHLY_GROUP_REPORT(ISEAS,MGI,Wholesale Exp) +
     +                                 MAX(0.,MON_ECO_PUCH_COST_FROM(I))
            MONTHLY_GROUP_REPORT(ISEAS,MGI,Wholesale Buy) =
     +               MONTHLY_GROUP_REPORT(ISEAS,MGI,Wholesale Buy) +
     +                                 MAX(0.,MON_ECO_PUCH_ENRG_FROM(I))
            MONTHLY_GROUP_REPORT(ISEAS,MGI,Equivalent Capacity) =
     +             MONTHLY_GROUP_REPORT(ISEAS,MGI,Equivalent Capacity) +
     +                                    REVENUE_GENERATING_CAPACITY(I)
         ENDIF

         IF(UNIT_ENRG > 0.) THEN


            IF(MW(1,I) == MW(2,I) .AND. ENERGY(2,I) > .0001) THEN
               WRITE(4,*) "NON PHYSICAL ENERGY IN TOP BLOCK"
               WRITE(4,*) "FOR THERMAL UNIT ",UNITNM(I)
               WRITE(4,*) "ENERGY = ",ENERGY(2,I)
               WRITE(4,*) "UNIT POSITION ",I
               WRITE(4,*) '*** line 1463 DR_BOOTH.FOR ***'

            ENDIF

! 01/30/02

            IF(TRANSACT_C_ACTIVE  .OR.
     +                            YES_DAILY_THERMAL_RPT_ACTIVE) THEN
               MMBTUS = LEFT(I) + RIGHT(I)
            ELSE
               CALL CALHEAT(ENERGY(1,I),LEFT(I),RIGHT(I),COEFF(1,I),
     +                   MW(1,I),MMBTUS,EFFECTIVE_CAPACITY(1,I),
     +                   TEMPEA(1,I),TEMPEA(2,I))
            ENDIF

            IF(WABASH_VALLEY) THEN
               IF(CL_LOSSES(I) /= 0.) THEN
                  AHR = MMBTUS/(ENERGY(1,I)+ENERGY(2,I))
                  IF(ENERGY(1,I) > 0.) THEN
                     BLK1_LOSSES = (ENERGY(1,I)*CL_LOSSES(I))/
     +                                       (ENERGY(1,I) + ENERGY(2,I))

                     BLK1_HEAT(I) = BLK1_HEAT(I) + AHR * BLK1_LOSSES
                     BLK2_HEAT(I) = BLK2_HEAT(I) + AHR *
     +                                      (CL_LOSSES(I) - BLK1_LOSSES)
                  ELSE

                     BLK2_HEAT(I) = BLK2_HEAT(I) + AHR * CL_LOSSES(I)
                  ENDIF

                  MMBTUS = MMBTUS + AHR*CL_LOSSES(I)
               ENDIF
            ENDIF
            TEMP_SNGL = (HEAT_RATE_FACTOR(I) * SEAS_HOURS)/
     +                         HEAT_CONVERSION
            MMBTUS = DBLE(TEMP_SNGL) * MMBTUS
            BLK1_HEAT(I) = BLK1_HEAT(I) * TEMP_SNGL
            BLK2_HEAT(I) = BLK2_HEAT(I) * TEMP_SNGL

! ADD CIPSCO FUEL USAGE 6/16/92 COPYRIGHT (C) M.S. GERBER & ASSOCIATES

            VAR_COST = VCPMWH(I)
            FUEL_MIX_PRIM(I) = ABS(FUELMX(I))
            IF(FUEL_DERIVATIVE_ACTIVE .AND.
     +                 UNIT_HAS_FUEL_DERIVATIVE(CL_RESOURCE_ID(I))) THEN

                  CALL EMISSIONS_BY_ALL_FUELS(I,
     +                                        BLK1_HEAT(I),BLK2_HEAT(I))

                  MMBTUS_USED =
     +                MONTHLY_UNIT_FUEL_DERIV_HEAT(
     +                                       CL_RESOURCE_ID(I),
     +                                       MMBTUS,
     +                                       FUEL_DERIVATIVE_PRICE,
     +                                       ISEAS)

                  RCOST = SNGL(MMBTUS_USED *
     +                         DBLE(FUEL_DERIVATIVE_PRICE) +
     +                         (MMBTUS - MMBTUS_USED) *
     +                                           DBLE(FUEL_BTU_COST(I)))


                     BLOCK_FUEL_COST(1,I) = BLK1_HEAT(I) *
     +                                             FUEL_DERIVATIVE_PRICE
                     TEMP_SNGL = MMBTUS_USED - DBLE(BLK1_HEAT(I))

                     TEMP_SNGL = SNGL(MMBTUS - MMBTUS_USED)
                     TEMP_DBLE = MMBTUS_USED - DBLE(BLK1_HEAT(I))
                     BLOCK_FUEL_COST(2,I) = SNGL(TEMP_DBLE) *
     +                                       FUEL_DERIVATIVE_PRICE +
     +                                             TEMP_SNGL *
     +                                                  FUEL_BTU_COST(I)

            ELSEIF(FUEL_INVENTORY_ACTIVE .AND.
     +                        FUEL_SUPPLY_ID(I) > 0 .AND.
     +                                  SHADOW_UNIT_NUMBER(I) == 0) THEN
               IF(USE_SECONDARY_FUEL(I)) THEN
                  RCOST = SNGL(MMBTUS * DBLE(SBTUCT(I)))
                  BLOCK_FUEL_COST(1,I) = BLK1_HEAT(I) * SBTUCT(I)
                  BLOCK_FUEL_COST(2,I) = BLK2_HEAT(I) * SBTUCT(I)
                  FUEL_MIX_PRIM(I) = 0.
                  CALL EMISSIONS_BY_SEC_FUEL(I,BLK1_HEAT(I),
     +                                                     BLK2_HEAT(I))

               ELSE
                  FUEL_ID = FUEL_INVENTORY_ID(FUEL_SUPPLY_ID(I))
                  CALL FUEL_INVENTORY(MMBTUS,MMBTUS_USED,
     +                                MMBTU_FUEL_BALANCE(FUEL_ID),
     +                                USE_SECONDARY_FUEL(I),
     +                                FUEL_MIX_PRIM(I),
     +                                EMISS_BLENDING_RATE(I))
                  IF(USE_SECONDARY_FUEL(I)) DISP_BTU_COST(I) = SBTUCT(I)
                  RCOST = SNGL(MMBTUS_USED * DBLE(BLENDED_BTU_COST(I)) +
     +                         (MMBTUS - MMBTUS_USED) * DBLE(SBTUCT(I)))
                  IF(MMBTUS_USED >= DBLE(BLK1_HEAT(I))) THEN

                     BLOCK_FUEL_COST(1,I) = BLK1_HEAT(I) *
     +                                               BLENDED_BTU_COST(I)
                     TEMP_SNGL = MMBTUS_USED - DBLE(BLK1_HEAT(I))
                     CALL EMISSIONS_BY_BLENDED_FUEL(I,BLK1_HEAT(I),
     +                                                        TEMP_SNGL)
                     TEMP_SNGL = SNGL(MMBTUS - MMBTUS_USED)
                     TEMP_DBLE = MMBTUS_USED - DBLE(BLK1_HEAT(I))
                     BLOCK_FUEL_COST(2,I) = SNGL(TEMP_DBLE) *
     +                                       BLENDED_BTU_COST(I) +
     +                                             TEMP_SNGL * SBTUCT(I)
                     CALL EMISSIONS_BY_SEC_FUEL(I,0.,TEMP_SNGL)
                  ELSE
                     BLOCK_FUEL_COST(1,I) = MMBTUS_USED *
     +                                             BLENDED_BTU_COST(I) +
     +                          (BLK1_HEAT(I) - MMBTUS_USED) * SBTUCT(I)
                     CALL EMISSIONS_BY_BLENDED_FUEL(I,SNGL(MMBTUS_USED),
     +                                                               0.)
                     TEMP_SNGL = SNGL(MMBTUS - DBLE(BLK1_HEAT(I)))
                     BLOCK_FUEL_COST(2,I) = TEMP_SNGL * SBTUCT(I)
                     TEMP_SNGL2 = SNGL(DBLE(BLK1_HEAT(I))-MMBTUS_USED)
                     CALL EMISSIONS_BY_SEC_FUEL(I,TEMP_SNGL2,TEMP_SNGL)
                  ENDIF
               ENDIF
            ELSE
!
! 6/10/93. GAT. ALTERED TO ALLOW SHADOW UNITS TO COST AT SECOND FUEL.
!
               IF(SHADOW_UNIT_NUMBER(I) > 0 .AND.
     +                           SHADOW_UNIT_NUMBER(I) < I ) THEN
                  RCOST = SNGL(MMBTUS * DBLE(SBTUCT(I)))
                  BLOCK_FUEL_COST(1,I) = BLK1_HEAT(I) * SBTUCT(I)
                  BLOCK_FUEL_COST(2,I) = BLK2_HEAT(I) * SBTUCT(I)
               ELSEIF(SHADOW_UNIT_NUMBER(I) > 0) THEN
                  RCOST = SNGL(MMBTUS * DBLE(BLENDED_BTU_COST(I)))
                  BLOCK_FUEL_COST(1,I) =
     +                        BLK1_HEAT(I) * BLENDED_BTU_COST(I)
                  BLOCK_FUEL_COST(2,I) =
     +                        BLK2_HEAT(I) * BLENDED_BTU_COST(I)
               ELSE
                  RCOST = SNGL(MMBTUS * DBLE(FUEL_BTU_COST(I)))
                  BLOCK_FUEL_COST(1,I) =
     +                        BLK1_HEAT(I) * FUEL_BTU_COST(I)
                  BLOCK_FUEL_COST(2,I) =
     +                        BLK2_HEAT(I) * FUEL_BTU_COST(I)
               ENDIF
               CALL EMISSIONS_BY_ALL_FUELS(I,BLK1_HEAT(I),BLK2_HEAT(I))
            ENDIF
            RCOST = RCOST + UNIT_ENRG * FUEL_ADDER_ADJUSTMENT(I)

            GROUP_ENERGY(GGI) = GROUP_ENERGY(GGI) + UNIT_ENRG
            GROUP_MMBTUS(GGI) = GROUP_MMBTUS(GGI) + MMBTUS
            GROUP_FUEL(GGI)   = GROUP_FUEL(GGI) + RCOST
            GROUP_VAROM(GGI)  = GROUP_VAROM(GGI) + VAR_COST * UNIT_ENRG

            MONTHLY_GROUP_REPORT(ISEAS,MGI,Generation) =
     +            MONTHLY_GROUP_REPORT(ISEAS,MGI,Generation) + UNIT_ENRG
            MONTHLY_GROUP_REPORT(ISEAS,MGI,MMBTU) =
     +              MONTHLY_GROUP_REPORT(ISEAS,MGI,MMBTU) + MMBTUS
            MONTHLY_GROUP_REPORT(ISEAS,MGI,Fuel) =
     +              MONTHLY_GROUP_REPORT(ISEAS,MGI,Fuel) + RCOST
            MONTHLY_GROUP_REPORT(ISEAS,MGI,variable_om) =
     +              MONTHLY_GROUP_REPORT(ISEAS,MGI,variable_om) +
     +                                              VAR_COST * UNIT_ENRG

! BTU BY FUEL TYPE AND BY BTUS TAXED--ADDED 3/12/93 MODIFIED 4/28/93

            CALL RETURN_HEAT_BY_FUEL(I,PRIM_HEAT,SEC_HEAT,EMIS_HEAT)

            CALL CALC_EMISSION_REPORT_GROUPS(ISEAS,I,PRIM_HEAT,SEC_HEAT,
     +                                              EMIS_HEAT,UNIT_ENRG)
            IF(EXPENSE_ASSIGNMENT(I) /= 'P') THEN
               CALL SUM_HEAT_BY_FUEL_TYPE(PRIM_HEAT,PRIM_FUEL_TYPE(I),
     +                                   SEC_HEAT,SEC_FUEL_TYPE(I),
     +                                   EMIS_HEAT,EMISS_FUEL_TYPE(I),
     +                                   ANNUAL_BTUS_GOCN12,
     +                                   MONTH_BTUS_GOCN12,
     +                                   ANNUAL_BTUS_FOR_BTU_TAX_GOCN12,
     +                                   BTU_TAX_ACTIVE)
            ENDIF
! 
            IF(FUEL_INVENTORY_ACTIVE .AND.
     +             FUEL_USAGE_REPORT_ACTIVE .AND.
     +                    FUEL_SUPPLY_ID(I) /= 0 .AND.
     +                         SHADOW_UNIT_NUMBER(I) < I) THEN
               FUEL_ID = FUEL_INVENTORY_ID(FUEL_SUPPLY_ID(I))
               TEMP_HEAT = PRIM_HEAT + MMBTU_FUEL_BALANCE(FUEL_ID)
               CALL SEASON_FUEL_INVENTORY_REPORT(UNITNM(I),
     +                                   FUEL_SUPPLY_ID(I),
     +                                   TEMP_HEAT,
     +                                   MMBTU_FUEL_BALANCE(FUEL_ID),3)
            ENDIF

            CALL RETURN_UNIT_EMISSIONS(I,UNIT_EMISS_CONTRIB(1),
     +                                 UNIT_EMISS_CONTRIB(2),
     +                                 UNIT_EMISS_CONTRIB(3),
     +                                 UNIT_EMISS_CONTRIB(4),
     +                                 UNIT_EMISS_CONTRIB(5))

            DO EM = 1, NUMBER_OF_EMISSION_TYPES
               GROUP_EMISSIONS(EM,GGI) = GROUP_EMISSIONS(EM,GGI) +
     +                                            UNIT_EMISS_CONTRIB(EM)
               ANNUAL_EMIS(EM) = ANNUAL_EMIS(EM)+UNIT_EMISS_CONTRIB(EM)
            ENDDO

            MONTHLY_GROUP_REPORT(ISEAS,MGI,sulfur_o2) =
     +              MONTHLY_GROUP_REPORT(ISEAS,MGI,sulfur_o2) +
     +                                             UNIT_EMISS_CONTRIB(1)
            MONTHLY_GROUP_REPORT(ISEAS,MGI,Nitrix OX) =
     +              MONTHLY_GROUP_REPORT(ISEAS,MGI,Nitrix OX) +
     +                                             UNIT_EMISS_CONTRIB(2)
            MONTHLY_GROUP_REPORT(ISEAS,MGI,Other 1) =
     +              MONTHLY_GROUP_REPORT(ISEAS,MGI,Other 1) +
     +                                             UNIT_EMISS_CONTRIB(3)
            MONTHLY_GROUP_REPORT(ISEAS,MGI,Other 2) =
     +              MONTHLY_GROUP_REPORT(ISEAS,MGI,Other 2) +
     +                                             UNIT_EMISS_CONTRIB(4)
            MONTHLY_GROUP_REPORT(ISEAS,MGI,Other 3) =
     +              MONTHLY_GROUP_REPORT(ISEAS,MGI,Other 3) +
     +                                             UNIT_EMISS_CONTRIB(5)

! 042609. RPS FOR THERMAL (AND HYDRO AS THERMAL)

            TEMP_L1 = GET_THERMAL_RPS_DATA(I,PM,ST_TG,ST,RPS_CONTRIB)
            TEMP_L1 =
     +         PUT_THERMAL_RPS_ENRG_CAP(I,UNIT_ENRG,UNITCAP(I),YR,ISEAS)

            IF(ST > 0 .AND. PM /= 13) THEN
               TEMP_R4 = RPS_CONTRIB * UNITCAP(I)
               RPS_THERMAL_DB(1,ST,PM,ISEAS) =
     +                      RPS_THERMAL_DB(1,ST,PM,ISEAS) +
     +                                 TEMP_R4
               RPS_THERMAL_DB(1,ST,PM,0) =
     +                      RPS_THERMAL_DB(1,ST,PM,0) +
     +                                 TEMP_R4
               TEMP_R4 = RPS_CONTRIB * UNIT_ENRG * 0.001
               RPS_THERMAL_DB(2,ST,PM,ISEAS) =
     +                   RPS_THERMAL_DB(2,ST,PM,ISEAS) + TEMP_R4
               RPS_THERMAL_DB(2,ST,PM,0) =
     +                            RPS_THERMAL_DB(2,ST,PM,0) + TEMP_R4
            ENDIF

            IF(ST_TG > 0 .AND. PM /= 13) THEN
               TEMP_R4 = RPS_CONTRIB * UNITCAP(I)
               RPS_THERMAL_DB(1,ST_TG,PM,ISEAS) =
     +                      RPS_THERMAL_DB(1,ST_TG,PM,ISEAS) +
     +                                 TEMP_R4
               RPS_THERMAL_DB(1,ST_TG,PM,0) =
     +                      RPS_THERMAL_DB(1,ST_TG,PM,0) +
     +                                 TEMP_R4
               TEMP_R4 = RPS_CONTRIB * UNIT_ENRG * 0.001
               RPS_THERMAL_DB(2,ST_TG,PM,ISEAS) =
     +                   RPS_THERMAL_DB(2,ST_TG,PM,ISEAS) + TEMP_R4
               RPS_THERMAL_DB(2,ST_TG,PM,0) =
     +                            RPS_THERMAL_DB(2,ST_TG,PM,0) + TEMP_R4
            ENDIF

! NOTE: WHEN ENERGY COMES OUT OF CALHEAT THE VALUES HAVE BEEN MULTIPILED
!       BY THE APPROPRIATE EA's

            UNIT_ENRG = ENERGY(1,I) + ENERGY(2,I)
            IF(WABASH_VALLEY) THEN
               UNIT_ENRG = UNIT_ENRG + CL_LOSSES(I)
            ENDIF

!           ADD NEW POOLING LOGIC FOR IP, 6/4/91.

            IF(ns_service_decs%CL_POOL_FRAC_OWN(I) < 100) THEN
               GROUP = 2
        UTILITY_PORTION = ns_service_decs%CL_POOL_FRAC_OWN(I) / 100.
               POOL_PORTION = 1. - UTILITY_PORTION
               CLASS_ASSIGNED_MMBTUS(GROUP) =
     +            CLASS_ASSIGNED_MMBTUS(GROUP) +
     +            MMBTUS * DBLE(POOL_PORTION)
               DO EM = 1, NUMBER_OF_EMISSION_TYPES
                  CLASS_ASSIGNED_EMISS(EM,GROUP)=UNIT_EMISS_CONTRIB(EM)*
     +                     POOL_PORTION + CLASS_ASSIGNED_EMISS(EM,GROUP)
               ENDDO
               CL_P_CLASS_ASSIGNED_ENERGY(GROUP) =
     +            CL_P_CLASS_ASSIGNED_ENERGY(GROUP) +
     +            UNIT_ENRG * SEAS_HOURS * POOL_PORTION
               CL_P_CLASS_ASSIGNED_COST(GROUP) =
     +            CL_P_CLASS_ASSIGNED_COST(GROUP) +
     +            (RCOST + POOLING_VARIABLE_COST_SWITCH *
     +                      UNIT_ENRG*VAR_COST* SEAS_HOURS)*POOL_PORTION
               IF(LDTYPE(I) /= 'N') THEN
                  IF(EXPENSE_ASSIGNMENT(I) /= 'P') THEN
                     CLASS_ASS_FOSSIL_COST(GROUP) =
     +                  CLASS_ASS_FOSSIL_COST(GROUP) +
     +                  RCOST * POOL_PORTION
                  ELSE
                     CLASS_ASS_PURCHASE_COST(GROUP) =
     +                  CLASS_ASS_PURCHASE_COST(GROUP) +
     +                  (RCOST + UNIT_ENRG*VAR_COST*SEAS_HOURS)
     +                  * POOL_PORTION
                  ENDIF
               ELSE
                  CLASS_ASS_NUCLEAR_COST(GROUP) =
     +               CLASS_ASS_NUCLEAR_COST(GROUP) +
     +               RCOST * POOL_PORTION
               ENDIF
               IF(EXPENSE_ASSIGNMENT(I) /= 'P')
     +            CLASS_ASSIGNED_VARIABLE_COST(GROUP) =
     +               CLASS_ASSIGNED_VARIABLE_COST(GROUP) +
     +               VAR_COST * UNIT_ENRG * SEAS_HOURS * POOL_PORTION
               RCOST = RCOST * UTILITY_PORTION
               UNIT_ENRG = UNIT_ENRG * UTILITY_PORTION
               MMBTUS = MMBTUS * DBLE(UTILITY_PORTION)
               GROUP = 1
            ENDIF
            IF(LDTYPE(I) == 'N') THEN
               TOTAL_NUCLEAR_FUEL_EXPENSE = RCOST +
     +                                     TOTAL_NUCLEAR_FUEL_EXPENSE
               NUC_UNIT_FUEL_ADDER_COST =
     +                 UNIT_ENRG * SEAS_HOURS * FUEL_ADDER_ADJUSTMENT(I)
               ANNUAL_NUC_UNIT_FUEL_ADDER_COST(I) =
     +                              ANNUAL_NUC_UNIT_FUEL_ADDER_COST(I) +
     +                                          NUC_UNIT_FUEL_ADDER_COST

! SEND COST INFORMATION FOR USE BY MONTHLY MIDAS

               VOID_LOGICAL = MON_MDS_NUC_ADDER(ISEAS,UNITNO,
     +                                         NUC_UNIT_FUEL_ADDER_COST)
               P_NUCLEAR_FUEL_COST = P_NUCLEAR_FUEL_COST + RCOST
               IF(EXPENSE_ASSIGNMENT(I) == 'O')
     +            OWNED_NUCLEAR_FUEL_EXPENSE(J) = RCOST +
     +                             OWNED_NUCLEAR_FUEL_EXPENSE(J)
               IF(EXPENSE_ASSIGNMENT(I) == 'L')
     +            LEASED_NUCLEAR_FUEL_EXPENSE(J) = RCOST +
     +                            LEASED_NUCLEAR_FUEL_EXPENSE(J)
            ELSE
               IF(EXPENSE_ASSIGNMENT(I) == 'P') THEN
                  P_PUR_ENRG = P_PUR_ENRG + UNIT_ENRG
                  RCOST = RCOST + VAR_COST * UNIT_ENRG * SEAS_HOURS
                  P_PUR_POWER_COST_VAR = P_PUR_POWER_COST_VAR + RCOST
                  PURCHASED_POWER_COSTS(J) = RCOST +
     +                                       PURCHASED_POWER_COSTS(J)
                ELSE
                   PFUELCST = PFUELCST + RCOST
                   FOSSIL_FUEL_COSTS(J) = FOSSIL_FUEL_COSTS(J) + RCOST
                ENDIF
            ENDIF
            IF(EXPENSE_ASSIGNMENT(I) /= 'P') THEN
               PVARCOST = PVARCOST +  VAR_COST * UNIT_ENRG
               IF(EXPENSE_COLLECTION(I) /= 'N')
     +             REG_VARIABLE_FOSSIL_COSTS = SEAS_HOURS *
     +               VAR_COST * UNIT_ENRG + REG_VARIABLE_FOSSIL_COSTS
            ENDIF
            IF(EXPENSE_ASSIGNMENT(I) /= 'P') THEN
               PENRG = PENRG + UNIT_ENRG
               PMMBTUS = PMMBTUS + MMBTUS
            ENDIF
         ENDIF
      ENDDO
      IF(KEPCO) THEN
         CONTROL_AREA_REPORT = INDEX('B,M',KEPCO_REPORTS())/=0
         PRINT_HEADER = .TRUE.
         EMERGENCY_ENERGY = 0.
         MAINTENANCE_ENERGY = 0.
         AREA_ENERGY_LOSSES = 0.
         DO AREA = 1, NUMBER_OF_AREAS
            AREA_ENERGY(AREA,ISEAS) = 0.
            AREA_CAPACITY(AREA,ISEAS) = 0.
         ENDDO
         IF(get_NUNITS() > 0) THEN
            IF(REALLY_KEPCO) THEN
               CALL KEPCO_ENRG_CAP(ISEAS,ENERGY,UNITCAP,
     +  get_NUNITS,UNITNM,SEAS_HOURS,'CL',
     +                             CONTROL_AREA_REPORT,LDTYPE,YR,
     +                             CL_RESOURCE_ANNUAL_AREA_ENRG,
     +                             CL_RESOURCE_ANNUAL_AREA_CAP,
     +                             CL_ANNUAL_EMERGENCY_ENERGY,
     +                             CL_ANNUAL_MAINTENANCE_ENERGY,
     +                             CL_ANNUAL_ENERGY_LOSSES,
     +                             ONLINE,OFLINE,
     +                             date1,DATE2,
     +                             EAVAIL,
     +                             WC_PERIOD_MAINTENANCE_ENERGY,
     +                             WC_PERIOD_EMERGENCY_ENERGY,
     +                             WC_UNIT_NO)
               WC_PERIOD_MAINTENANCE_ENERGY = 0.
               WC_PERIOD_EMERGENCY_ENERGY = 0.
            ELSE
               CALL REA_ENRG_CAP(ISEAS,ENERGY,UNITCAP,CL_RESOURCE_ID,
     + get_NUNITS(),UNITNM,SEAS_HOURS,'CL',
     +                          CONTROL_AREA_REPORT,LDTYPE,YR,
     +                          CL_RESOURCE_ANNUAL_AREA_ENRG,
     +                          CL_RESOURCE_ANNUAL_AREA_CAP,
     +                          CL_ANNUAL_EMERGENCY_ENERGY,
     +                          CL_ANNUAL_MAINTENANCE_ENERGY,
     +                          CL_ANNUAL_ENERGY_LOSSES,
     +                          ONLINE,OFLINE,
     +                          date1,DATE2,
     +                          EAVAIL,REMAINING_ENERGY)
            ENDIF
         ENDIF
         IF(NUMBER_OF_CONTRACTS > 0.) THEN
            IF(REALLY_KEPCO) THEN
               CALL KEPCO_ENRG_CAP(ISEAS,CONTRACT_ENERGY,
     +                             CONTRACT_CAPACITY,CT_RESOURCE_ID,
     +                             NUMBER_OF_CONTRACTS,
     +                             CNTRNM,SEAS_HOURS,'CT',
     +                             CONTROL_AREA_REPORT,CNTRTYPE,YR,
     +                             CT_RESOURCE_ANNUAL_AREA_ENRG,
     +                             CT_RESOURCE_ANNUAL_AREA_CAP,
     +                             CT_ANNUAL_EMERGENCY_ENERGY,
     +                             CT_ANNUAL_MAINTENANCE_ENERGY,
     +                             CT_ANNUAL_ENERGY_LOSSES,
     +                             CNTR_ON_LI,CNTR_OFF_LI,
     +                             date1,DATE2,
     +                             EAVAIL,
     +                             WC_PERIOD_MAINTENANCE_ENERGY,
     +                             WC_PERIOD_EMERGENCY_ENERGY,
     +                             WC_UNIT_NO)
            ELSE
               CALL REA_ENRG_CAP(ISEAS,CONTRACT_ENERGY,
     +                           CONTRACT_CAPACITY,
     +                           CT_RESOURCE_ID,
     +                           NUMBER_OF_CONTRACTS,
     +                           CNTRNM,SEAS_HOURS,'CT',
     +                           CONTROL_AREA_REPORT,CNTRTYPE,YR,
     +                           CT_RESOURCE_ANNUAL_AREA_ENRG,
     +                           CT_RESOURCE_ANNUAL_AREA_CAP,
     +                           CT_ANNUAL_EMERGENCY_ENERGY,
     +                           CT_ANNUAL_MAINTENANCE_ENERGY,
     +                           CT_ANNUAL_ENERGY_LOSSES,
     +                           CNTR_ON_LI,CNTR_OFF_LI,
     +                           date1,DATE2,
     +                           EAVAIL,REMAINING_ENERGY)
            ENDIF
         ENDIF
         IF(CONTROL_AREA_REPORT .AND. WABASH_VALLEY)
     +                                    CALL PRINT_CONTROL_AREA_TOTALS
         TOTAL_EMERGENCY_ENERGY = TOTAL_EMERGENCY_ENERGY +
     +                                                  EMERGENCY_ENERGY
         TOTAL_MAINTENANCE_ENERGY = TOTAL_MAINTENANCE_ENERGY +
     +                                                MAINTENANCE_ENERGY
         TOTAL_AREA_RESOURCE_LOSSES = TOTAL_AREA_RESOURCE_LOSSES +
     +                                                AREA_ENERGY_LOSSES
         IF(REALLY_KEPCO) THEN
            CALL KEPCO_LAST_RESOURCE(ISEAS,SEAS_HOURS,
     +                               CONTROL_AREA_REPORT,
     +                               CONTRACT_CAPACITY,CONTRACT_ENERGY,
     +                               NUMBER_OF_CONTRACTS,
     +                               CNTRTYPE,
     +                               MAXIMUM_CAPACITY,
     +                               MAXIMUM_ENERGY,
     +                               CNTR_CAPACITY_SWITCH,
     +                               MAX_RATCHET_PATTERN,
     +                               MINIMUM_CAPACITY,
     +                               date1,DATE2,
     +                               CNTR_ON_LI,CNTR_OFF_LI)
         ELSE
            CALL REA_LAST_RESOURCE(ISEAS,SEAS_HOURS,
     +                             CONTROL_AREA_REPORT,
     +                             CONTRACT_CAPACITY,CONTRACT_ENERGY,
     +                             CNTRTYPE,
     +                             MAXIMUM_CAPACITY,
     +                             MAXIMUM_ENERGY,
     +                             MINIMUM_CAPACITY,
     +                             DEMAND,PEAK)
         ENDIF
      ENDIF
      EXCESS_ENERGY =  TOTAL_CONTRACT_ENRG  - DEMAND/SEAS_HOURS -.00001

! WEST KOOTNEY NEW CUSTOMER 3808 CONTRACT LOGIC

      IF(IS_3808_ACTIVE()) THEN
         CALL CALC_3808_CAPACITY(CONTRACT_CAPACITY,PEAK_MONTH,ISEAS)

         IF(USED_ENRG_ONLY_ENERGY > 0.) THEN
            CALL GET_X_Y_Z_CONTRACT(X_CONTRACT,Y_CONTRACT,Z_CONTRACT)
            IF(X_CONTRACT > 0 .AND.
     +                      CONTRACT_CAPACITY(X_CONTRACT) > 0. .AND.
     +                        CONTRACT_ENERGY(X_CONTRACT) <
     +                              CONTRACT_CAPACITY(X_CONTRACT) ) THEN
               TEMP_ENERGY =
     +                  MAX(0.,MIN(USED_ENRG_ONLY_ENERGY,
     +                     MIN(CONTRACT_CAPACITY(X_CONTRACT),
     +                       MAXIMUM_ENERGY(X_CONTRACT)/SEAS_HOURS) -
     +                                     CONTRACT_ENERGY(X_CONTRACT)))
               CONTRACT_ENERGY(X_CONTRACT) =
     +                         CONTRACT_ENERGY(X_CONTRACT) + TEMP_ENERGY
               USED_ENRG_ONLY_ENERGY = USED_ENRG_ONLY_ENERGY -
     +                                                       TEMP_ENERGY
            ENDIF ! X_CONTRACT
            IF(Y_CONTRACT > 0 .AND.
     +                      CONTRACT_CAPACITY(Y_CONTRACT) > 0. .AND.
     +                        CONTRACT_ENERGY(Y_CONTRACT) <
     +                              CONTRACT_CAPACITY(Y_CONTRACT) ) THEN
               TEMP_ENERGY =
     +                  MAX(0.,MIN(USED_ENRG_ONLY_ENERGY,
     +                     MIN(CONTRACT_CAPACITY(Y_CONTRACT),
     +                       MAXIMUM_ENERGY(Y_CONTRACT)/SEAS_HOURS) -
     +                                     CONTRACT_ENERGY(Y_CONTRACT)))
               CONTRACT_ENERGY(Y_CONTRACT) =
     +                         CONTRACT_ENERGY(Y_CONTRACT) + TEMP_ENERGY
               USED_ENRG_ONLY_ENERGY = USED_ENRG_ONLY_ENERGY -
     +                                                       TEMP_ENERGY
            ENDIF ! Y_CONTRACT
            IF(Z_CONTRACT > 0 .AND.
     +                      CONTRACT_CAPACITY(Z_CONTRACT) > 0. .AND.
     +                        CONTRACT_ENERGY(Z_CONTRACT) <
     +                           CONTRACT_CAPACITY(Z_CONTRACT) .AND.
     +                        CONTRACT_VARIABLE_COST(Z_CONTRACT) <
     +                           CONTRACT_VARIABLE_COST(CONTRACT) ) THEN
               TEMP_ENERGY =
     +                  MAX(0.,MIN(USED_ENRG_ONLY_ENERGY,
     +                     MIN(CONTRACT_CAPACITY(Z_CONTRACT),
     +                       MAXIMUM_ENERGY(Z_CONTRACT)/SEAS_HOURS) -
     +                                     CONTRACT_ENERGY(Z_CONTRACT)))
               CONTRACT_ENERGY(Z_CONTRACT) =
     +                         CONTRACT_ENERGY(Z_CONTRACT) + TEMP_ENERGY
               USED_ENRG_ONLY_ENERGY = USED_ENRG_ONLY_ENERGY -
     +                                                       TEMP_ENERGY
            ENDIF ! Z_CONTRACT
         ENDIF ! USED_ENRG_ONLY_ENERGY > 0.
      ENDIF ! 3808_ACTIVE

      IF(NUMBER_OF_CONTRACTS > 0.) THEN
         DO I = 1 , NUMBER_OF_CONTRACTS
            IF(CNTR_ON_LI(I) <= DATE2 .AND.
     +                                 CNTR_OFF_LI(I) >= DATE1) THEN

! 5/13/92 SURPLUS SALE LOGIC: AFTER CONTRACT LOOP FOR P_PUR_ENRG
! 5/22/92 MOVED INTO CONTRACT LOOP

               IF(EXCESS_ENERGY > 0.0 .AND.
     +                          CNTR_ENERGY_SWITCH(I) == 'O' .AND.
     +                                         CNTRTYPE(I) /= 'X' ) THEN
                  CHECK_MAX_SURPLUS = CONTRACT_ENERGY(I)+EXCESS_ENERGY
                  CONTRACT_ENERGY(I) = -1*MIN(
     +               MAXIMUM_ENERGY(I)/SEAS_HOURS,
     +               CONTRACT_ENERGY(I) + EXCESS_ENERGY)
                  EXCESS_ENERGY = EXCESS_ENERGY + CONTRACT_ENERGY(I)
!
! TAKE CONTRACT ENERGY OUT HERE SO THAT THE BALANCE = 0.
!
                  P_PUR_ENRG = P_PUR_ENRG - CONTRACT_ENERGY(I)
               ENDIF
!
               IF(CNTR_ENERGY_SWITCH(I) == 'T') THEN
                  IF(CONTRACT_CAPACITY(I) > 0.0 .AND.
     +                  CONTRACT_ENERGY(I)/CONTRACT_CAPACITY(I) >
     +                                      .99999*.7 ) THEN
                     CONTRACT_VARIABLE_COST(I) =
     +                  (CT_MONTHLY_1ST_ENERGY_PRICE(I)*.7 +
     +                  CT_SECOND_ENERGY_PRICE(I)*
     +                  ( CONTRACT_ENERGY(I)/CONTRACT_CAPACITY(I) -
     +                  .7 ) ) * CONTRACT_CAPACITY(I) /
     +                  CONTRACT_ENERGY(I)
                  ENDIF
               ENDIF ! PALO ALTO 2 TIER

               IF(.NOT.(CNTRTYPE(I) == 'F'.OR.
     +                  CNTRTYPE(I) == 'G'.OR.
     +                  CNTRTYPE(I) == 'H'.OR.
     +                  CNTRTYPE(I) == 'J')) THEN
                  IF(MAXIMUM_CAPACITY(I) > 0. .AND.
     +                     CONTRACT_CAPACITY(I)/MAXIMUM_CAPACITY(I)
     +                                                     > 1.001) THEN
                     WRITE(4,*) "In Endpoint",gc_end_point,
     +                       ", Year",globecom_YEAR+get_BASE_YEAR()
                     WRITE(4,*) ", Season ",MONTH_NAME(ISEAS),
     +                                           ", Contract ",CNTRNM(I)
                     WRITE(4,*) "the contract capacity exceeded the "//
     +                       "maximum capacity."
                     WRITE(4,*) " "
                  ENDIF
                  IF(MAXIMUM_ENERGY(I) > 0. .AND.
     +                CONTRACT_ENERGY(I)/MAXIMUM_ENERGY(I) > 1.001) THEN
                     WRITE(4,*) "In Endpoint",gc_end_point,
     +                         ", Year",globecom_YEAR+get_BASE_YEAR()
                     WRITE(4,*) ", Season ",MONTH_NAME(ISEAS),
     +                                           ", Contract ",CNTRNM(I)
                     WRITE(4,*) "the contract energy exceeded the "//
     +                       "maximum energy."
                     WRITE(4,*) " "
                  ENDIF
                  IF(CONTRACT_CAPACITY(I) > 0. .AND.
     +              CONTRACT_ENERGY(I)/(SEAS_HOURS*CONTRACT_CAPACITY(I))
     +                                                     > 1.001) THEN
                     WRITE(4,*) "In Endpoint",gc_end_point,
     +                            ", Year",globecom_YEAR+get_BASE_YEAR()
                     WRITE(4,*) ", Season ",MONTH_NAME(ISEAS),
     +                                           ", Contract ",CNTRNM(I)
                     WRITE(4,*) "the contract capacity factor "//
     +                       "exceeded 100%."
                     WRITE(4,*) " "
                  ENDIF
               ENDIF

               GGI = CNTR_GROUP(I)
               CNTR_ENRG = CONTRACT_ENERGY(I) * SEAS_HOURS
               REMAIN_ANN_ENRG(I) =
     +                           MAX(0.,REMAIN_ANN_ENRG(I) - CNTR_ENRG)
               J = 2      !IF(CNTR_EXP_COLLECT(I) == 'B') J = 2
               IF(CNTR_EXP_COLLECT(I)(1:1) == 'A') J = 1
               IF(CNTR_EXP_COLLECT(I)(1:1) == 'N' .OR.
     +                      INDEX(CNTR_EXP_COLLECT(I),'BTL') /= 0) J = 3
!
! ADDED 10/5/92 FOR ANNUAL CAPACITY FACTOR CALCULATION
!
               CNTR_WEIGHTED_CAPACITY(I) = CNTR_WEIGHTED_CAPACITY(I) +
     +                          MAXIMUM_CAPACITY(I)*SEAS_HOURS
               TEMP_CNTR_VAR_COST =
     +               CONTRACT_VARIABLE_COST(I)*CONTRACT_ENERGY(I)
               CONTRACT_BILLING_CAPACITY = MIN(MINIMUM_CAPACITY(I),
     +                                           CONTRACT_CAPACITY(I))
               MONTHS_ON_LINE = (FLOAT(MIN(CNTR_OFF_LI(I),DATE2)-
     +                         MAX(CNTR_ON_LI(I),date1)) + 1.)/12.
               TEMP_CNTR_FIXED_COST = MIN(MINIMUM_CAPACITY(I),
     +            CONTRACT_CAPACITY(I))*MIN_CONTRACT_FIXED_COST(I) +
     +            1000.*MONTHS_ON_LINE*CT_ANNUAL_FIXED_COST(I)
               IF(CNTR_CAPACITY_SWITCH(I) == 'V' ) THEN
                  CONTRACT_BILLING_CAPACITY = CONTRACT_CAPACITY(I)
                  TEMP_CNTR_FIXED_COST = TEMP_CNTR_FIXED_COST +
     +               (CONTRACT_CAPACITY(I) - MIN(MINIMUM_CAPACITY(I),
     +               CONTRACT_CAPACITY(I)))*CONTRACT_FIXED_COST(I)
               ELSE
                  IF(REALLY_KEPCO) THEN
                     IF(MAX_RATCHET_PATTERN(I) /= 0) THEN
                        CONTRACT_BILLING_CAPACITY =
     +                                 MAX(RATCHET_CAPACITY_BASIS(I),
     +                                       MIN_RATCHET_CAPACITY(I),
     +                                           CONTRACT_CAPACITY(I))
                        CAPACITY_BILLED_AT_MIN =MIN(MINIMUM_CAPACITY(I),
     +                                        CONTRACT_BILLING_CAPACITY)
                        TEMP_CNTR_FIXED_COST = CAPACITY_BILLED_AT_MIN *
     +                                        MIN_CONTRACT_FIXED_COST(I)
                        CAPACITY_BILLED_AT_MAX=MAX(0.,
     +                                        CONTRACT_BILLING_CAPACITY-
     +                                           CAPACITY_BILLED_AT_MIN)
                        TEMP_CNTR_FIXED_COST = TEMP_CNTR_FIXED_COST +
     +                                        CAPACITY_BILLED_AT_MAX *
     +                                            CONTRACT_FIXED_COST(I)
                        MAX_RATCHET_CAPACITY(I) =
     +                                      MAX(CONTRACT_CAPACITY(I),
     +                                          MAX_RATCHET_CAPACITY(I))
                     ELSE
                        CONTRACT_BILLING_CAPACITY =
     +                                         MAX(MAXIMUM_CAPACITY(I),
     +                                             CONTRACT_CAPACITY(I))
                        TEMP_CNTR_FIXED_COST = TEMP_CNTR_FIXED_COST +
     +                                  (MAX(MAXIMUM_CAPACITY(I),
     +                                           CONTRACT_CAPACITY(I)) -
     +                                   MIN(MINIMUM_CAPACITY(I),
     +                                          CONTRACT_CAPACITY(I))) *
     +                                            CONTRACT_FIXED_COST(I)
                     ENDIF
                  ELSE
                     IF(KEPCO .AND. MAX_RATCHET_PATTERN(I) /= 0) THEN
                        CONTRACT_BILLING_CAPACITY =
     +                                 MAX(RATCHET_CAPACITY_BASIS(I),
     +                                       MIN_RATCHET_CAPACITY(I),
     +                                           CONTRACT_CAPACITY(I))
                        TEMP_CNTR_FIXED_COST=CONTRACT_BILLING_CAPACITY *
     +                                            CONTRACT_FIXED_COST(I)
                        MAX_RATCHET_CAPACITY(I) =
     +                                      MAX(CONTRACT_CAPACITY(I),
     +                                          MAX_RATCHET_CAPACITY(I))
                     ELSEIF(KEPCO) THEN
                        CONTRACT_BILLING_CAPACITY =
     +                                      MAX(MAXIMUM_CAPACITY(I),
     +                                             CONTRACT_CAPACITY(I))
                        TEMP_CNTR_FIXED_COST = TEMP_CNTR_FIXED_COST +
     +                      (CONTRACT_BILLING_CAPACITY -
     +                        MIN(MINIMUM_CAPACITY(I),
     +                                     CONTRACT_CAPACITY(I))) *
     +                                            CONTRACT_FIXED_COST(I)
                     ELSE
                        CONTRACT_BILLING_CAPACITY = MAXIMUM_CAPACITY(I)
                        TEMP_CNTR_FIXED_COST = TEMP_CNTR_FIXED_COST +
     +                   (MAXIMUM_CAPACITY(I) - MIN(MINIMUM_CAPACITY(I),
     +                     CONTRACT_CAPACITY(I)))*CONTRACT_FIXED_COST(I)
                     ENDIF
                  ENDIF
               ENDIF
! CONTRACT GROUPS
               IF(ISEAS == PEAK_MONTH) THEN
                  IF(KEPCO) THEN
                     SEASON_CAPACITY(GGI) = SEASON_CAPACITY(GGI) +
     +                                         CONTRACT_BILLING_CAPACITY
                  ELSE
                     SEASON_CAPACITY(GGI) = SEASON_CAPACITY(GGI) +
     +                                             CONTRACT_CAPACITY(I)
                  ENDIF
               ENDIF
               GROUP_FIXED_OM(GGI) = GROUP_FIXED_OM(GGI) +
     +                                        1000*TEMP_CNTR_FIXED_COST
               GROUP_ENERGY(GGI) = GROUP_ENERGY(GGI) + CNTR_ENRG
               GROUP_EMISSIONS(1,GGI) = GROUP_EMISSIONS(1,GGI) +
     +                             CNTR_SO2(I)*CNTR_ENRG/TONS_CONVERSION
               CT_SO2_ANNUAL = CT_SO2_ANNUAL +
     +                             CNTR_SO2(I)*CNTR_ENRG/TONS_CONVERSION
               GROUP_VAROM(GGI) = GROUP_VAROM(GGI) +
     +                                     TEMP_CNTR_VAR_COST*SEAS_HOURS
! CONTRACT POOLING
               IF(CNTR_POOL(I) < 100) THEN
                  GROUP = 2
                  UTILITY_PORTION = CNTR_POOL(I) / 100.
                  POOL_PORTION = 1. - UTILITY_PORTION
                  CL_P_CLASS_ASSIGNED_ENERGY(GROUP) =
     +               CL_P_CLASS_ASSIGNED_ENERGY(GROUP) +
     +               CNTR_ENRG * POOL_PORTION
                  P_CLASS_ASSIGNED_COST(GROUP) =
     +               P_CLASS_ASSIGNED_COST(GROUP) +
     +               TEMP_CNTR_VAR_COST * POOL_PORTION * SEAS_HOURS

! ALTERED 9/22/92

                  IF(CNTR_EXP_ASSIGN(I) /= 'P') THEN
                     CLASS_ASSIGNED_VARIABLE_COST(GROUP) =
     +                  CLASS_ASSIGNED_VARIABLE_COST(GROUP) +
     +                  TEMP_CNTR_VAR_COST * POOL_PORTION * SEAS_HOURS
                     CLASS_ASSIGNED_FIXED_COST(GROUP) =
     +                  CLASS_ASSIGNED_FIXED_COST(GROUP) +
     +                  TEMP_CNTR_FIXED_COST * POOL_PORTION * 1000.
                  ELSE
                     CLASS_ASS_PURCHASE_COST(GROUP) =
     +                  CLASS_ASS_PURCHASE_COST(GROUP) +
     +                  TEMP_CNTR_VAR_COST * POOL_PORTION * SEAS_HOURS +
     +                  TEMP_CNTR_FIXED_COST * POOL_PORTION * 1000.
                  ENDIF
                  CLASS_ASSIGNED_EMISS(1,GROUP)=
     +               CLASS_ASSIGNED_EMISS(1,GROUP) +
     +               CNTR_SO2(I)*CNTR_ENRG*POOL_PORTION/TONS_CONVERSION
                  CNTR_ENRG = CNTR_ENRG * UTILITY_PORTION

                  TEMP_CNTR_VAR_COST =
     +                       TEMP_CNTR_VAR_COST * UTILITY_PORTION
                  TEMP_CNTR_FIXED_COST =
     +               TEMP_CNTR_FIXED_COST * UTILITY_PORTION
               ENDIF


! ADDED 12/8/92 FOR KEPCO DEFERRED WC MAINTENACNE EXPENSE
!  THESE CASH EXPENSES ARE NOT INCLUDED IN THE INCOME STATEMENT

               CURRENT_MAINTENANCE_EXPENSE = 0.
               IF(REALLY_KEPCO .AND. INDEX(CNTRTYPE(I),'M') /= 0) THEN
                  IF(SEAS_HOURS*CONTRACT_ENERGY(I) > .1) THEN
                     CURRENT_MAINTENANCE_EXPENSE=(WOLF_CREEK_ENRG_COST *
     +                         SEAS_HOURS * CONTRACT_ENERGY(I))/1000000.
                     CALL KEPCO_DEFERRED_MAINTENANCE(ISEAS,
     +                      (TEMP_CNTR_VAR_COST * SEAS_HOURS +
     +                         1000.*TEMP_CNTR_FIXED_COST)/1000000.,
     +                                      CURRENT_MAINTENANCE_EXPENSE)
                  ENDIF
               ELSEIF(KEPCO .AND. INDEX('SX',CNTRTYPE(I)) /= 0) THEN
                  P_SALES_REVENUE = P_SALES_REVENUE +
     +                                   TEMP_CNTR_VAR_COST * SEAS_HOURS
                  P_SALES_ENERGY = P_SALES_ENERGY + CONTRACT_ENERGY(I)
                  CNTR_ENRG = 0. !TO ELIMINATE FROM P_PUR_ENRG
               ELSEIF(CNTR_EXP_ASSIGN(I) == 'P') THEN
                  TEMP_CNTR_VAR_COST = TEMP_CNTR_VAR_COST * SEAS_HOURS
                  P_PUR_POWER_COST_VAR = P_PUR_POWER_COST_VAR +
     +                                                TEMP_CNTR_VAR_COST
                  P_PUR_POWER_COST_FIXED = P_PUR_POWER_COST_FIXED +
     +                                        1000.*TEMP_CNTR_FIXED_COST
                  PURCHASED_POWER_COSTS(J) = PURCHASED_POWER_COSTS(J) +
     +               TEMP_CNTR_VAR_COST + 1000.*TEMP_CNTR_FIXED_COST
               ELSEIF(CNTR_EXP_ASSIGN(I) == 'F') THEN
                  PVARCOST = PVARCOST + TEMP_CNTR_VAR_COST
                  PFIXCOST = PFIXCOST + TEMP_CNTR_FIXED_COST*1000.
               ENDIF

               IF(.NOT. KEPCO .OR. INDEX('SX',CNTRTYPE(I)) == 0) THEN
                  P_PUR_ENRG = P_PUR_ENRG + CNTR_ENRG/SEAS_HOURS
               ENDIF
            ENDIF
            IF(MAX_RATCHET_PATTERN(I) /= 0 .AND.
     +                                     PEAK_MONTH == ISEAS)
     +             MAX_RATCHET_CAPACITY(I) = CONTRACT_CAPACITY(I)
         ENDDO !END CONTRACTS COUNTER

!        5/13/92 ADDED SURPLUS SALE LOGIC

       IF(.NOT. get_WEST_KOOTENAY_POWER_active() 
     +  .AND. .NOT. REALLY_KEPCO) THEN
         IF(DEMAND - SEAS_HOURS*(P_PUR_ENRG) < -0.00001
     +                                 .AND. SUR_SALE_NO > 0) THEN
            EXCESS_ENERGY =  (P_PUR_ENRG) - DEMAND/SEAS_HOURS
            I = 0
            DOWHILE(EXCESS_ENERGY > 0. .AND. I < SUR_SALE_NO)
               I = I + 1
               S_I = SURPLUS_SALE(I)
               CHECK_MAX_SURPLUS = CONTRACT_ENERGY(S_I) + EXCESS_ENERGY
               CONTRACT_ENERGY(S_I) = -1*MIN(
     +                MAXIMUM_ENERGY(S_I)/SEAS_HOURS,CHECK_MAX_SURPLUS)
               TEMP_CNTR_VAR_COST = SEAS_HOURS*
     +               CONTRACT_VARIABLE_COST(S_I)*CONTRACT_ENERGY(S_I)
               P_PUR_POWER_COST_VAR = P_PUR_POWER_COST_VAR +
     +                                                TEMP_CNTR_VAR_COST
               PURCHASED_POWER_COSTS(1) = PURCHASED_POWER_COSTS(1) +
     +                                               TEMP_CNTR_VAR_COST
                P_PUR_ENRG = P_PUR_ENRG + CONTRACT_ENERGY(I)
               EXCESS_ENERGY = EXCESS_ENERGY + CONTRACT_ENERGY(S_I)
            ENDDO
         ENDIF !END SURPLUS SALE
        ENDIF
      ENDIF !END CONTRACTS

      DO I = 0, MAX_REPORTING_GROUPS
         ANN_CL_CAPACITY(I) = MAX(ANN_CL_CAPACITY(I),SEASON_CAPACITY(I))
      ENDDO
      CL_P_CLASS_ASSIGNED_ENERGY(1) = PENRG * SEAS_HOURS
      P_CLASS_ASSIGNED_ENERGY(1) = P_CLASS_ASSIGNED_ENERGY(1) +
     +       CL_P_CLASS_ASSIGNED_ENERGY(1) + P_PUR_ENRG * SEAS_HOURS
      P_CLASS_ASSIGNED_ENERGY(2) = P_CLASS_ASSIGNED_ENERGY(2) +
     +                                 CL_P_CLASS_ASSIGNED_ENERGY(2)
      CLASS_ASSIGNED_ENERGY(1) = CLASS_ASSIGNED_ENERGY(1) +
     +                                 CL_P_CLASS_ASSIGNED_ENERGY(1)
      CLASS_ASSIGNED_ENERGY(2) = CLASS_ASSIGNED_ENERGY(2) +
     +                                 P_CLASS_ASSIGNED_ENERGY(2)
      PENRG    = DBLE(CL_P_CLASS_ASSIGNED_ENERGY(1)) +
     +                          DBLE(CL_P_CLASS_ASSIGNED_ENERGY(2))
      TENRG    = TENRG + PENRG
      TMMBTUS  = TMMBTUS + PMMBTUS
      FUELCOST = FUELCOST + PFUELCST
      VARCOST  = VARCOST + PVARCOST * DBLE(SEAS_HOURS)
      PURCHASE_ENERGY = PURCHASE_ENERGY + P_PUR_ENRG * SEAS_HOURS
      P_PUR_POWER_COST = P_PUR_POWER_COST_VAR + P_PUR_POWER_COST_FIXED
      PURCHASE_COSTS = PURCHASE_COSTS + P_PUR_POWER_COST
      FIXED_OM_COSTS = FIXED_OM_COSTS + PFIXCOST
      ANNUAL_CNTR_SALES_REVENUE = ANNUAL_CNTR_SALES_REVENUE +
     +                                                   P_SALES_REVENUE
      ANNUAL_CNTR_SALES_ENERGY = ANNUAL_CNTR_SALES_ENERGY +
     +                                       P_SALES_ENERGY * SEAS_HOURS
      CL_P_CLASS_ASSIGNED_COST(1) = PFUELCST +
     +                    POOLING_VARIABLE_COST_SWITCH *
     +                                           PVARCOST * SEAS_HOURS +
     +                    P_PUR_POWER_COST_VAR +
     +                    P_NUCLEAR_FUEL_COST

! REMOVED 6/10/92 COST(1) EXCLUDES COST(2)

      P_CLASS_ASSIGNED_COST(1) =
     +      CL_P_CLASS_ASSIGNED_COST(1) + P_CLASS_ASSIGNED_COST(1)
      P_CLASS_ASSIGNED_COST(2) =
     +      CL_P_CLASS_ASSIGNED_COST(2) + P_CLASS_ASSIGNED_COST(2)

      IF(CPL_ACTIVE()) THEN

         CALL CPL_PA_DISPATCH(LPROB2,LODDUR,HOURS_INCREMENT,
     +                              LAST_POINT,MAINTENANCE_RATE,DEMAND,
     +                              TEMPEA,PEAK,ISEAS,
     +                              BLOCK_FUEL_COST,ENERGY,
     +                              EFFECTIVE_CAPACITY,
     +                              PFUELCST,
     +                              P_NUCLEAR_FUEL_COST,
     +                              P_PUR_POWER_COST_VAR)

      ENDIF

      VOID_LOGICAL = CALCULATE_MONTHLY_TRANS_GROUP(ISEAS,YR)

      IF(MONTHLY_UNIT_REPORT .OR. CONTRACT_REPORT() /= 'F' .OR.
     +      ANNUAL_UNIT_OUTPUT_REPORT() .OR. SPREAD_SHEET() .OR.
     +      .TRUE. .OR.             ! ADD FOR ASSET CLASS WORK 3/31/95
     +           (KEPCO .AND. INDEX('M,B,A',KEPCO_REPORTS()) /= 0)) THEN
         IF(MONTHLY_UNIT_REPORT .AND. PROD_METHOD /= 2 .AND.
     +       .NOT.
     +       (YES_RUN_TRANSACT() .AND. TRANSACT_ACTIVE_THIS_MONTH)) THEN
            BLKNO(1:NBLOCK) = BLKNO2(1:NBLOCK)
            MWBLOK(1:NBLOCK) = MWBLK2(1:NBLOCK)
            UNIT(1:NBLOCK)  = UNIT2(1:NBLOCK)
            INHEAT(1:NBLOCK) = INHEAT2(1:NBLOCK)
         ENDIF
         
         if(yr==0) then
             call end_program("dr_booth:0002 - YR argument must " // 
     +          "be nonzero.")
         end if
         CALL PERIOD_CL_REPORT(ISEAS,YR,PENRG,
     +         PMMBTUS,PFUELCST,PVARCOST,P_PUR_ENRG,P_PUR_POWER_COST,
     +         PFIXCOST,ENERGY,SEAS_HOURS,BLK1_HEAT,
     +         BLK2_HEAT,
     +         MAINTENANCE_RATE,
     +         DEMAND,P_NUCLEAR_FUEL_COST,
     +         BASE,PEAK,
     +         CONTRACT_ENERGY,CONTRACT_CAPACITY,MAXIMUM_CAPACITY,
     +         MINIMUM_CAPACITY,PEAK_MONTH,KEPCO,
     +         BLOCK_FUEL_COST,CONTRACTS_IN_PERIOD,
     +         MONTHLY_ECONOMY_BOUGHT,MONTHLY_ECONOMY_COST,
     +         MONTHLY_ECONOMY_SOLD,MONTHLY_ECONOMY_REVENUE,
     +         FUEL_MIX_PRIM,
     +         EFFECTIVE_CAPACITY,MONTH_BTUS_GOCN12,MMBTU_FUEL_BALANCE,
     +         WABASH_VALLEY,REALLY_KEPCO,
     +         P_SALES_ENERGY,P_SALES_REVENUE,MAXIMUM_ENERGY,
     +         RATCHET_CAPACITY_BASIS,MIN_RATCHET_CAPACITY,
     +         REVENUE_GENERATING_CAPACITY,
     +         CL_UNIT_MONTHLY_FIXED_COST)
      ENDIF



      CALL WRITE_MONTHLY_GROUP_REPORT(ISEAS,YR,
     +                                 MAX_MONTHLY_GROUPS,
     +                                 MAX_MONTHLY_GROUP_VARIABLES,
     +                                 MONTHLY_GROUP_REPORT,PEAK_MONTH)

      IF(KEPCO .AND.
     +         INDEX('B,A',KEPCO_REPORTS()) /= 0 .AND.
     +  PERIOD_COUNTER == get_PRODUCTION_PERIODS_in()) THEN
         PRINT_HEADER = .TRUE.
         IF(get_NUNITS() > 0) THEN
            CALL ANNUAL_AREA_REPORT('CL',get_NUNITS(),
     +                              UNITNM,
     +                              ANNUAL_CL_UNIT_ENERGY,
     +                              MW,
     +                              LDTYPE,
     +                              CL_RESOURCE_ANNUAL_AREA_ENRG,
     +                              CL_RESOURCE_ANNUAL_AREA_CAP,
     +                              CL_ANNUAL_EMERGENCY_ENERGY,
     +                              CL_ANNUAL_MAINTENANCE_ENERGY,
     +                              CL_ANNUAL_ENERGY_LOSSES,
     +                              ONLINE,OFLINE,
     +                              (get_BASE_YEAR() + YR - 1900) * 100)
         ENDIF
         IF(NUMBER_OF_CONTRACTS > 0.) THEN
            CALL ANNUAL_AREA_REPORT('CT',NUMBER_OF_CONTRACTS,
     +                              CNTRNM,
     +                              ANNUAL_CONTRACT_ENERGY,
     +                              ANNUAL_CONTRACT_CAPACITY,
     +                              CNTRTYPE,
     +                              CT_RESOURCE_ANNUAL_AREA_ENRG,
     +                              CT_RESOURCE_ANNUAL_AREA_CAP,
     +                              CT_ANNUAL_EMERGENCY_ENERGY,
     +                              CT_ANNUAL_MAINTENANCE_ENERGY,
     +                              CT_ANNUAL_ENERGY_LOSSES,
     +                              CNTR_ON_LI,CNTR_OFF_LI,
     +                              (get_BASE_YEAR() + YR - 1900) * 100)
         ENDIF
      ENDIF
!
      IF(WABASH_VALLEY .AND.
     +                 INDEX('M,B,A',WABASH_POWER_COST_RPT()) /= 0) THEN
         IF(get_NUNITS() > 0) THEN
            CALL WABASH_CL_POWER_COST_REPORT(ISEAS,YR,ENERGY,
     +                                       SEAS_HOURS,
     +                                       BLOCK_FUEL_COST,
     +                                       CL_LOSSES,
     +                                       UNITCAP,
     +                                       UNIT_CAP_AFTER_LOSSES,
     +                                       CL_UNIT_MONTHLY_FIXED_COST,
     +                                       VCPMWH,
     +                                       FUEL_ADDER_ADJUSTMENT,
     +                                       UNITNM)
         ENDIF
         IF(NUMBER_OF_CONTRACTS > 0) THEN
            CALL WABASH_CT_POWER_COST_REPORT(ISEAS,YR,
     +                                       CONTRACT_ENERGY,
     +                                       SEAS_HOURS,
     +                                       CONTRACT_CAPACITY,
     +                                       MONTHLY_ECONOMY_BOUGHT,
     +                                       MONTHLY_ECONOMY_COST,
     +                                       MONTHLY_ECONOMY_SOLD,
     +                                       MONTHLY_ECONOMY_REVENUE,
     +                                       CT_LOSSES)
         ENDIF

         CALL WABASH_MD_POWER_COST_REPORT(ISEAS,YR,
     +                                  SEAS_HOURS)
      ENDIF

! SERVICE TRANSACTION ADDED 10/18/92 MSG

      IF(SERVICE_TRANSACTIONS_ACTIVE) THEN
         IF(get_NUNITS() > 0) then

! I think I did this.
!        online(1)=0
!        ofline(1)=0
        
       CALL CL_SERVICE_TRANS_CALCULATIONS(get_nunits(),
     + ENERGY,
     + ONLINE, OFLINE, date1, date2, ISEAS, 
     + REVENUE_GENERATING_CAPACITY, 
     + MW)
     
         endif
     
         IF(NUMBER_OF_CONTRACTS > 0) then
                CALL CT_SERVICE_TRANS_CALCULATIONS(NUMBER_OF_CONTRACTS,
     +                                              CONTRACT_ENERGY,
     +                                              CONTRACT_CAPACITY,
     +                                        date1,DATE2,ISEAS)
         endif
     
      ENDIF
      IF(ALLOCATED(AVAILABLE_SHADOW_CAPACITY))
     +   DEALLOCATE(AVAILABLE_SHADOW_CAPACITY)
      IF(ALLOCATED(CT_LOSSES)) DEALLOCATE(CT_LOSSES)
      IF(ALLOCATED(CL_LOSSES)) DEALLOCATE(CL_LOSSES)
      RETURN

      ENTRY GET_THERMAL_RPS_SUM(R_MONTH,R_ST_TG,R_RESOURCE_RPS_VARS)

         DO PM = 2, 8 ! 7
            I = RPS_INDEX_TRANSLATE(PM)
            R_RESOURCE_RPS_VARS(PM) = R_RESOURCE_RPS_VARS(PM) +
     +                               RPS_THERMAL_DB(2,R_ST_TG,I,R_MONTH)
            R_RESOURCE_RPS_VARS(PM+8) = R_RESOURCE_RPS_VARS(PM+8) +
     +                               RPS_THERMAL_DB(1,R_ST_TG,I,R_MONTH)
! TOTAL GWH MW
            R_RESOURCE_RPS_VARS(1) = R_RESOURCE_RPS_VARS(1) +
     +                               RPS_THERMAL_DB(2,R_ST_TG,I,R_MONTH)
            R_RESOURCE_RPS_VARS(9) = R_RESOURCE_RPS_VARS(9) +
     +                               RPS_THERMAL_DB(1,R_ST_TG,I,R_MONTH)
         END DO
      RETURN


      ENTRY GET_MWBLOK_FOR_UNIT(R_I,R1_OPTION_MWH,R2_OPTION_MWH)

         TEMP_I2 = GET_TRANS_UNIT_TO_BLOCK(R_I,INT2(1))
         IF(TEMP_I2 > 0) THEN
            R1_OPTION_MWH = MWBLOK(TEMP_I2)
         ELSE
            R1_OPTION_MWH = 0.0
         ENDIF
         TEMP_I2 = GET_TRANS_UNIT_TO_BLOCK(R_I,INT2(2))
         IF(TEMP_I2 > 0) THEN
            R2_OPTION_MWH = MWBLOK(TEMP_I2)
         ELSE
            R2_OPTION_MWH = 0.0
         ENDIF

      RETURN



      ENTRY PUT_TRANS_C_ENERGY(R_I,R1_OPTION_MWH,
     +                         R2_OPTION_MWH,hours_month_ord)

         if(hours_month_ord .eq. 0 .or. hours_month_ord>744) then
            call end_program("dr_booth:0001 - hours_month_ord= " //
     + trim(itos(int(hours_month_ord))))
         end if
         ENERGY(1,R_I) = R1_OPTION_MWH/FLOAT(hours_month_ord)
         ENERGY(2,R_I) = R2_OPTION_MWH/FLOAT(hours_month_ord)

      RETURN

      ENTRY PUT_TRANS_C_HEAT(R_I,R1_OPTION_MWH,R2_OPTION_MWH)

! ! PERCENT OF TIME AT MARGIN

         LEFT(R_I) = R1_OPTION_MWH
         RIGHT(R_I) = R2_OPTION_MWH

      RETURN



      ENTRY RETURN_P_SALES_REVENUE_ENERGY(R_CNTR_SALES_REVENUE,
     +                                    R_CNTR_SALES_ENERGY)



         R_CNTR_SALES_REVENUE = ANNUAL_CNTR_SALES_REVENUE
         R_CNTR_SALES_ENERGY = ANNUAL_CNTR_SALES_ENERGY
         ANNUAL_CNTR_SALES_ENERGY = 0.
         ANNUAL_CNTR_SALES_REVENUE = 0.
      RETURN


      ENTRY GET_UNIT_FROM_BLOCK(R_UNIT,R_BLOCK_NO)


         R_UNIT = UNIT(R_BLOCK_NO)
      RETURN

      END SUBROUTINE DR_BOOTH

!   USED FOR FUEL SWITCHING IF THE PRIM FUEL IS THE DISPATCHING FUEL
!                   COPYRIGHT (C) 1993
!               M.S. GERBER & ASSOCIATES, INC.
!                   ALL RIGHTS RESERVED

      SUBROUTINE SHADOW_FUEL_INVENTORY(ENERGY,MMBTU_FUEL_BALANCE,
     +                                 EA,coeff_loc,HEAT_RATE_FACTOR,
     +                                 SEAS_HOURS,B,A,
     +                                 LEFT,RIGHT,
     +                                 AVAILABLE_SHADOW_CAPACITY,
     +                                 MWBLOK,LODDUR,LPROB,
     +                                 BLENDING_RATE,
     +                                 SEC_BTU_COST)
      use kootenay
      use prodcom
      implicit none
      REAL ::  EA
      REAL ::  COEFF_loc
      REAL ::  HEAT_RATE_FACTOR
      REAL ::  SEAS_HOURS
      REAL ::  GET_HEAT_CONVERSION
      REAL ::  ENERGY
      REAL ::  B
      REAL ::  A
      REAL ::  LEFT
      REAL ::  RIGHT
      REAL ::  AVAILABLE_SHADOW_CAPACITY
      REAL :: ADJUSTED_ENERGY,B_ADJUSTED,MWBLOK,SEC_BTU_COST
      REAL :: LODDUR(*),LPROB(*),BLENDING_RATE
      REAL (kind=8) ::   DBLE_TEMP,HEAT,MMBTU_FUEL_BALANCE

      DBLE_TEMP = DBLE((EA * COEFF_loc * HEAT_RATE_FACTOR * SEAS_HOURS)/


     +                         GET_HEAT_CONVERSION())
      HEAT = DBLE_TEMP * DBLE(ENERGY * (1. - BLENDING_RATE))
      IF(HEAT <= MMBTU_FUEL_BALANCE) THEN
         MMBTU_FUEL_BALANCE = MMBTU_FUEL_BALANCE - HEAT
         AVAILABLE_SHADOW_CAPACITY = 0.
      ELSE
         HEAT = MMBTU_FUEL_BALANCE/(DBLE_TEMP*DBLE(1. - BLENDING_RATE))
         ADJUSTED_ENERGY = SNGL(HEAT)
         IF(A + ADJUSTED_ENERGY <= LODDUR(1)) THEN
            B_ADJUSTED = A + ADJUSTED_ENERGY
            RIGHT = 1.
         ELSEIF(ABS(LEFT - RIGHT) < .0001) THEN
            B_ADJUSTED = A + ADJUSTED_ENERGY/LEFT
         ELSE

! FIND THE INCREMENT THAT HAS THE ENERGY CROSS-OVER

            CALL FIND_ENRGY_CROSSOVER(A,LEFT,ADJUSTED_ENERGY,LODDUR,
     +                                LPROB,B_ADJUSTED,RIGHT)
         ENDIF
         AVAILABLE_SHADOW_CAPACITY = (B - B_ADJUSTED)*MWBLOK/(B-A)
         MWBLOK = MWBLOK - AVAILABLE_SHADOW_CAPACITY
         MMBTU_FUEL_BALANCE = 0.
         ENERGY = ADJUSTED_ENERGY
              
     
         B = B_ADJUSTED
         IF(SEC_BTU_COST == 0.) AVAILABLE_SHADOW_CAPACITY = 0.
      ENDIF
      RETURN

      END SUBROUTINE SHADOW_FUEL_INVENTORY

!       FINDS THE COST CROSSING POINT FOR A CONTRACT AND A CL UNIT
!                   COPYRIGHT (C) 1992
!               M.S. GERBER & ASSOCIATES, INC.
!                   ALL RIGHTS RESERVED

      SUBROUTINE FIND_ENRGY_CROSSOVER(A,LEFT,TARGET_ENERGY,
     +                                 LODDUR,LPROB,B_ADJUSTED,RIGHT)

      use shared_vars_interg82
      use interg82
      use prodcom

      LOGICAL (kind=1) ::   ENERGY_EXCEEDED_AT_FIRST_PASS


      INTEGER (kind=2) ::   ISTART,IXSTOP,I
      REAL :: A,LEFT,TARGET_ENERGY,LODDUR(*),LPROB(*),B_ADJUSTED,RIGHT
      REAL :: INTPL8,X1,Y1,X3,Y2,X2,ENERGY,INCREMENTAL_ENERGY,BASE

      INTPL8(X1,Y1,X3,Y2,X2) = Y1 + (Y2-Y1) * (X3-X1)/(X2-X1)

      CALL RETURN_ISTART_ISTOP(ISTART,IXSTOP)
      BASE = LODDUR(1)
      ENERGY = MAX(BASE-A,0.0)
      INCREMENTAL_ENERGY = 0.
      ENERGY_EXCEEDED_AT_FIRST_PASS = .FALSE.
      IF(A > BASE) THEN
         INCREMENTAL_ENERGY = (LODDUR(ISTART)-A)*(LPROB(ISTART)+LEFT)/2.
         ENERGY = ENERGY + INCREMENTAL_ENERGY
         IF(ENERGY > TARGET_ENERGY) ENERGY_EXCEEDED_AT_FIRST_PASS =
     +                                                            .TRUE.
      ENDIF
      I = ISTART
      DOWHILE (ENERGY < TARGET_ENERGY)
         INCREMENTAL_ENERGY =(LPROB(I) + LPROB(I+1)) *
     +                                      (LODDUR(I+1) - LODDUR(I))/2.
         ENERGY = ENERGY + INCREMENTAL_ENERGY
         I = I + 1
      ENDDO
      ENERGY = ENERGY - INCREMENTAL_ENERGY
      I = MAX(ISTART,I - 1)
      IF(ENERGY_EXCEEDED_AT_FIRST_PASS) THEN
         CALL FIND_MAXIMUM_ENERGY(A,LODDUR(I),
     +                         LEFT,LPROB(I),ENERGY,
     +                         TARGET_ENERGY,B_ADJUSTED,RIGHT)
      ELSE
         CALL FIND_MAXIMUM_ENERGY(LODDUR(I),LODDUR(I+1),
     +                         LPROB(I),LPROB(I+1),ENERGY,
     +                         TARGET_ENERGY,B_ADJUSTED,RIGHT)
      ENDIF
      RETURN

      END SUBROUTINE FIND_ENRGY_CROSSOVER

!             OBJECT BASED FORTRAN FOR UNIT EMISSIONS AND HEAT DATA
!                   COPYRIGHT (C) 1993
!               M.S. GERBER & ASSOCIATES, INC.
!                   ALL RIGHTS RESERVED


      RECURSIVE SUBROUTINE DO_EMISSIONS_BY_FUEL_TYPE

      SAVE
      CHARACTER (len=6) ::   CAP_LIMIT_FUEL_TYPES
      PARAMETER(CAP_LIMIT_FUEL_TYPES='GOCN12')

      INTEGER (kind=2) ::   UNIT_NO
      INTEGER (kind=2) ::   I,R_CL_UNITS,J,BLK_NO,L
      CHARACTER (len=1) ::   PRIM_FUEL_TYPE
      CHARACTER (len=1) ::   SEC_FUEL_TYPE,EMISS_FUEL_TYPE
      LOGICAL (kind=1) ::   BTU_TAX_ACTIVE
      REAL (kind=8) ::  ANNUAL_BTUS_GOCN12(0:6)
      REAL (kind=8) ::  ANNUAL_BTUS_FOR_BTU_TAX_GOCN12(0:6)
      REAL (kind=8) ::   MONTH_BTUS_GOCN12(0:6)
      REAL :: SEC_HEAT
      REAL ::  BLK1_HEAT
      REAL ::  BLK2_HEAT
      REAL ::  BLENDING_RATE
      REAL ::  BLEND_HEAT
      REAL ::  PRIM_HEAT
      REAL :: FUEL_MIX_RATE
      REAL :: R_SOX,R_NOX,R_CO2,R_OTH2,R_OTH3
      REAL :: R_PRIM_HEAT,R_SEC_HEAT,R_EMIS_HEAT

      END SUBROUTINE DO_EMISSIONS_BY_FUEL_TYPE


      SUBROUTINE EMISSIONS_BY_BLENDED_FUEL(UNIT_NO,BLK1_HEAT,BLK2_HEAT)

      implicit none
      integer (kind=2) , intent(in) :: UNIT_NO
      real, intent(in) :: BLK1_HEAT
      real, intent(in) :: BLK2_HEAT

      integer (kind=2) :: I
      real :: BLENDING_RATE, BLEND_HEAT, PRIM_HEAT


         I = UNIT_NO
         CALL RETURN_BLENDING_MIX(I,BLENDING_RATE)
         IF(BLENDING_RATE > 0.) THEN
            BLEND_HEAT = BLENDING_RATE * (BLK1_HEAT + BLK2_HEAT)
            BLOCK1_HEAT = BLENDING_RATE * BLK1_HEAT
            BLOCK2_HEAT = BLENDING_RATE * BLK2_HEAT
            CALL RETURN_BLEND_EMISSION_RATES(I,SOX,NOX1,NOX2,CO2,
     +                                                        OTH2,OTH3)
            SOX_BY_FUEL_TYPE(3,I) = BLEND_HEAT * SOX
            NOX_BY_FUEL_TYPE(3,I) = BLENDING_RATE *
     +                             (BLK1_HEAT * NOX1 + BLK2_HEAT * NOX2)
            CO2_BY_FUEL_TYPE(3,I) = BLEND_HEAT * CO2
            OTH2_BY_FUEL_TYPE(3,I) = BLEND_HEAT * OTH2
            OTH3_BY_FUEL_TYPE(3,I) = BLEND_HEAT * OTH3
            UNIT_HEAT_BY_FUEL(3,I) = UNIT_HEAT_BY_FUEL(3,I) + BLEND_HEAT
            CALL SAVE_EMISSIONS_BY_BLOCK(UNIT_NO)

! DO PRIM FUEL PORTION

            PRIM_HEAT = BLK1_HEAT + BLK2_HEAT - BLEND_HEAT
            BLOCK1_HEAT = BLK1_HEAT - BLOCK1_HEAT
            BLOCK2_HEAT = BLK2_HEAT - BLOCK2_HEAT
         ELSE
            BLOCK1_HEAT = BLK1_HEAT
            BLOCK2_HEAT = BLK2_HEAT
            PRIM_HEAT = BLK1_HEAT + BLK2_HEAT
            BLEND_HEAT = 0.
         ENDIF
         CALL RETURN_PRIM_EMISSION_RATES(I,SOX,NOX1,NOX2,CO2,OTH2,OTH3)
         CALL SAVE_EMISSIONS_BY_BLOCK(UNIT_NO)
         SOX_BY_FUEL_TYPE(1,I) = PRIM_HEAT * SOX
         NOX_BY_FUEL_TYPE(1,I) = (1.-BLENDING_RATE) *
     +                             (BLK1_HEAT * NOX1 + BLK2_HEAT * NOX2)
         CO2_BY_FUEL_TYPE(1,I) = PRIM_HEAT * CO2
         OTH2_BY_FUEL_TYPE(1,I) = PRIM_HEAT * OTH2
         OTH3_BY_FUEL_TYPE(1,I) = PRIM_HEAT * OTH3
         UNIT_HEAT_BY_FUEL(1,I) = UNIT_HEAT_BY_FUEL(1,I) + PRIM_HEAT
      RETURN

      END SUBROUTINE EMISSIONS_BY_BLENDED_FUEL


      SUBROUTINE EMISSIONS_BY_SEC_FUEL(UNIT_NO,BLK1_HEAT,BLK2_HEAT)

      implicit none
      integer (kind=2) , intent(in) :: UNIT_NO
      real, intent(in) :: BLK1_HEAT
      real, intent(in) :: BLK2_HEAT

      integer (kind=2) :: I
      real :: SEC_HEAT


         I = UNIT_NO
         SEC_HEAT = BLK1_HEAT + BLK2_HEAT
         IF(SEC_HEAT /= 0.) THEN
            CALL RETURN_SEC_EMISSION_RATES(I,SOX,NOX1,NOX2,
     +                                     CO2,OTH2,OTH3)
            SOX_BY_FUEL_TYPE(2,I) = SEC_HEAT * SOX
            NOX_BY_FUEL_TYPE(2,I) = BLK1_HEAT*NOX1 + BLK2_HEAT*NOX2
            CO2_BY_FUEL_TYPE(2,I) = SEC_HEAT * CO2
            OTH2_BY_FUEL_TYPE(2,I) = SEC_HEAT * OTH2
            OTH3_BY_FUEL_TYPE(2,I) = SEC_HEAT * OTH3
            UNIT_HEAT_BY_FUEL(2,I) = UNIT_HEAT_BY_FUEL(2,I) + SEC_HEAT
            BLOCK1_HEAT = BLK1_HEAT
            BLOCK2_HEAT = BLK2_HEAT
            CALL SAVE_EMISSIONS_BY_BLOCK(UNIT_NO)
         ENDIF
      RETURN

      END SUBROUTINE EMISSIONS_BY_SEC_FUEL


      SUBROUTINE EMISSIONS_BY_ALL_FUELS(UNIT_NO,BLK1_HEAT,BLK2_HEAT)

      implicit none
      integer (kind=2) , intent(in) :: UNIT_NO
      real, intent(inout) :: BLK1_HEAT
      real, intent(inout) :: BLK2_HEAT

      integer (kind=2) :: I
      real :: FUEL_MIX_RATE


         I = UNIT_NO
         CALL RETURN_FUEL_MIX(I,FUEL_MIX_RATE)
         IF(FUEL_MIX_RATE < 1.) THEN
            CALL EMISSIONS_BY_SEC_FUEL(I,(1.-FUEL_MIX_RATE)*BLK1_HEAT,
     +                                   (1.-FUEL_MIX_RATE)*BLK2_HEAT)
         ENDIF
         CALL EMISSIONS_BY_BLENDED_FUEL(I,FUEL_MIX_RATE*BLK1_HEAT,
     +                                    FUEL_MIX_RATE*BLK2_HEAT)


      END SUBROUTINE EMISSIONS_BY_ALL_FUELS


      SUBROUTINE CREATE_EMISSIONS_BY_FUEL_TYPE(R_CL_UNITS)

      implicit none
      integer (kind=2) , intent(in) :: R_CL_UNITS


         LOCAL_MAX_CL_UNITS = MAX(1,R_CL_UNITS)
         IF(ALLOCATED(SOX_BY_BLOCK)) THEN
            DEALLOCATE(SOX_BY_BLOCK)
            DEALLOCATE(NOX_BY_BLOCK)
            DEALLOCATE(CO2_BY_BLOCK)
            DEALLOCATE(OTH2_BY_BLOCK)
            DEALLOCATE(OTH3_BY_BLOCK)
            DEALLOCATE(UNIT_HEAT_BY_FUEL)
         ENDIF
         IF(ALLOCATED(SOX_BY_FUEL_TYPE)) DEALLOCATE(SOX_BY_FUEL_TYPE)
         IF(ALLOCATED(NOX_BY_FUEL_TYPE)) DEALLOCATE(NOX_BY_FUEL_TYPE)
         IF(ALLOCATED(CO2_BY_FUEL_TYPE)) DEALLOCATE(CO2_BY_FUEL_TYPE,
     +                                              OTH2_BY_FUEL_TYPE,
     +                                              OTH3_BY_FUEL_TYPE)
         ALLOCATE(SOX_BY_FUEL_TYPE(3,LOCAL_MAX_CL_UNITS),
     +               NOX_BY_FUEL_TYPE(3,LOCAL_MAX_CL_UNITS),
     +               CO2_BY_FUEL_TYPE(3,LOCAL_MAX_CL_UNITS),
     +               OTH2_BY_FUEL_TYPE(3,LOCAL_MAX_CL_UNITS),
     +               OTH3_BY_FUEL_TYPE(3,LOCAL_MAX_CL_UNITS),
     +               SOX_BY_BLOCK(2,LOCAL_MAX_CL_UNITS),
     +               NOX_BY_BLOCK(2,LOCAL_MAX_CL_UNITS),
     +               CO2_BY_BLOCK(2,LOCAL_MAX_CL_UNITS),
     +               OTH2_BY_BLOCK(2,LOCAL_MAX_CL_UNITS),
     +               OTH3_BY_BLOCK(2,LOCAL_MAX_CL_UNITS),
     +               UNIT_HEAT_BY_FUEL(3,LOCAL_MAX_CL_UNITS))
      RETURN

      END SUBROUTINE CREATE_EMISSIONS_BY_FUEL_TYPE


      SUBROUTINE DEALLOCATE_EMISSIONS_BY_FUEL()

      implicit none


         IF(ALLOCATED(SOX_BY_BLOCK)) THEN
            DEALLOCATE(SOX_BY_BLOCK)
            DEALLOCATE(NOX_BY_BLOCK)
            DEALLOCATE(CO2_BY_BLOCK)
            DEALLOCATE(OTH2_BY_BLOCK)
            DEALLOCATE(OTH3_BY_BLOCK)
            DEALLOCATE(UNIT_HEAT_BY_FUEL)
         ENDIF
         IF(ALLOCATED(SOX_BY_FUEL_TYPE)) DEALLOCATE(SOX_BY_FUEL_TYPE)
         IF(ALLOCATED(NOX_BY_FUEL_TYPE)) DEALLOCATE(NOX_BY_FUEL_TYPE)
         IF(ALLOCATED(CO2_BY_FUEL_TYPE)) DEALLOCATE(CO2_BY_FUEL_TYPE,
     +                                              OTH2_BY_FUEL_TYPE,
     +                                              OTH3_BY_FUEL_TYPE)
      RETURN

      END SUBROUTINE DEALLOCATE_EMISSIONS_BY_FUEL


      SUBROUTINE CLEAR_EMISSIONS_BY_FUEL_TYPE

      implicit none
      integer (kind=2)             :: I,J


         SOX_BY_FUEL_TYPE = 0.
         NOX_BY_FUEL_TYPE = 0.
         CO2_BY_FUEL_TYPE = 0.
         OTH2_BY_FUEL_TYPE = 0.
         OTH3_BY_FUEL_TYPE = 0.
         UNIT_HEAT_BY_FUEL = 0.
         SOX_BY_BLOCK = 0.
         NOX_BY_BLOCK = 0.
         CO2_BY_BLOCK = 0.
         OTH2_BY_BLOCK = 0.
         OTH3_BY_BLOCK = 0.
      ! TODO: Remove code in IF block
      IF(.FALSE.) THEN
         DO I = 1, LOCAL_MAX_CL_UNITS
            SOX_BY_FUEL_TYPE(3,I) = 0.
            NOX_BY_FUEL_TYPE(3,I) = 0.
            CO2_BY_FUEL_TYPE(3,I) = 0.
            OTH2_BY_FUEL_TYPE(3,I) = 0.
            OTH3_BY_FUEL_TYPE(3,I) = 0.
            UNIT_HEAT_BY_FUEL(3,I) = 0.
            DO J = 1, 2
               SOX_BY_FUEL_TYPE(J,I) = 0.
               NOX_BY_FUEL_TYPE(J,I) = 0.
               CO2_BY_FUEL_TYPE(J,I) = 0.
               OTH2_BY_FUEL_TYPE(J,I) = 0.
               OTH3_BY_FUEL_TYPE(J,I) = 0.
               UNIT_HEAT_BY_FUEL(J,I) = 0.

               SOX_BY_BLOCK(J,I) = 0.
               NOX_BY_BLOCK(J,I) = 0.
               CO2_BY_BLOCK(J,I) = 0.
               OTH2_BY_BLOCK(J,I) = 0.
               OTH3_BY_BLOCK(J,I) = 0.
            ENDDO
         ENDDO
      ENDIF
      RETURN

      END SUBROUTINE CLEAR_EMISSIONS_BY_FUEL_TYPE


      SUBROUTINE RETURN_UNIT_EMISSIONS(UNIT_NO,R_SOX,R_NOX,R_CO2,
     +                            R_OTH2,R_OTH3)

      implicit none
      integer (kind=2) , intent(in) :: UNIT_NO
      real, intent(inout) :: R_SOX,R_NOX,R_CO2,R_OTH2,R_OTH3
      integer (kind=2)             :: I,J


         I = UNIT_NO
         R_SOX = 0.
         R_NOX = 0.
         R_CO2 = 0.
         R_OTH2 = 0.
         R_OTH3 = 0.
         DO J = 1, 3
            R_SOX = R_SOX + SOX_BY_FUEL_TYPE(J,I)
            R_NOX = R_NOX + NOX_BY_FUEL_TYPE(J,I)
            R_CO2 = R_CO2 + CO2_BY_FUEL_TYPE(J,I)
            R_OTH2 = R_OTH2 + OTH2_BY_FUEL_TYPE(J,I)
            R_OTH3 = R_OTH3 + OTH3_BY_FUEL_TYPE(J,I)
         ENDDO
         R_SOX = R_SOX/2000.
         R_NOX = R_NOX/2000.
         R_CO2 = R_CO2/2000.
         R_OTH2 = R_OTH2/2000.
         R_OTH3 = R_OTH3/2000.
      RETURN

      END SUBROUTINE RETURN_UNIT_EMISSIONS


      SUBROUTINE RETURN_HEAT_BY_FUEL(UNIT_NO,R_PRIM_HEAT,
     +                                           R_SEC_HEAT,R_EMIS_HEAT)

      implicit none
      integer (kind=2) , intent(in) :: UNIT_NO
      real, intent(inout) :: r_prim_heat
      real, intent(inout) :: r_sec_heat
      real, intent(inout) :: r_emis_heat



         R_PRIM_HEAT = UNIT_HEAT_BY_FUEL(1,UNIT_NO)
         R_SEC_HEAT = UNIT_HEAT_BY_FUEL(2,UNIT_NO)
         R_EMIS_HEAT = UNIT_HEAT_BY_FUEL(3,UNIT_NO)


      END SUBROUTINE RETURN_HEAT_BY_FUEL


      SUBROUTINE RETURN_EMISSIONS_BY_BLOCK(UNIT_NO,BLK_NO,
     +         R_SOX,R_NOX,R_CO2,
     +             R_OTH2,R_OTH3)

      implicit none
      integer (kind=2) , intent(in) :: UNIT_NO,BLK_NO
      real, intent(inout) :: R_SOX,R_NOX,R_CO2,R_OTH2,R_OTH3
      integer (kind=2)             :: I,J


         I = UNIT_NO
         J = BLK_NO
         R_SOX = SOX_BY_BLOCK(J,I)
         R_NOX = NOX_BY_BLOCK(J,I)
         R_CO2 = CO2_BY_BLOCK(J,I)
         R_OTH2 = OTH2_BY_BLOCK(J,I)
         R_OTH3 = OTH3_BY_BLOCK(J,I)
      RETURN

      END SUBROUTINE RETURN_EMISSIONS_BY_BLOCK


      SUBROUTINE SAVE_EMISSIONS_BY_BLOCK(UNIT_NO)

      implicit none
      integer (kind=2), intent(in) :: UNIT_NO
      integer (kind=2)             :: I


         I = UNIT_NO
         IF(BLOCK1_HEAT /= 0.) THEN
            SOX_BY_BLOCK(1,I) = SOX_BY_BLOCK(1,I) + BLOCK1_HEAT * SOX
            NOX_BY_BLOCK(1,I) = NOX_BY_BLOCK(1,I) + BLOCK1_HEAT * NOX1
            CO2_BY_BLOCK(1,I) = CO2_BY_BLOCK(1,I) + BLOCK1_HEAT * CO2
            OTH2_BY_BLOCK(1,I) = OTH2_BY_BLOCK(1,I) + BLOCK1_HEAT*OTH2
            OTH3_BY_BLOCK(1,I) = OTH3_BY_BLOCK(1,I) + BLOCK1_HEAT*OTH3
         ENDIF
         IF(BLOCK2_HEAT /= 0.) THEN
            SOX_BY_BLOCK(2,I) = SOX_BY_BLOCK(2,I) + BLOCK2_HEAT * SOX
            NOX_BY_BLOCK(2,I) = NOX_BY_BLOCK(2,I) + BLOCK2_HEAT * NOX2
            CO2_BY_BLOCK(2,I) = CO2_BY_BLOCK(2,I) + BLOCK2_HEAT * CO2
            OTH2_BY_BLOCK(2,I) = OTH2_BY_BLOCK(2,I) + BLOCK2_HEAT*OTH2
            OTH3_BY_BLOCK(2,I) = OTH3_BY_BLOCK(2,I) + BLOCK2_HEAT*OTH3
         ENDIF
      RETURN

      END SUBROUTINE SAVE_EMISSIONS_BY_BLOCK


      SUBROUTINE SUM_HEAT_BY_FUEL_TYPE(R_PRIM_HEAT,PRIM_FUEL_TYPE,

     +                            R_SEC_HEAT,SEC_FUEL_TYPE,
     +                            R_EMIS_HEAT,EMISS_FUEL_TYPE,
     +                            ANNUAL_BTUS_GOCN12,
     +                            MONTH_BTUS_GOCN12,
     +                            ANNUAL_BTUS_FOR_BTU_TAX_GOCN12,
     +                            BTU_TAX_ACTIVE)


         IMPLICIT NONE

         real, intent(in) :: r_prim_heat
         character (len=1), intent(in) :: prim_fuel_type
         real, intent(in) :: r_sec_heat
         character (len=1), intent(in) :: sec_fuel_type
         real, intent(in) :: r_emis_heat
         character (len=1), intent(in) :: emiss_fuel_type
         real (kind=8) , intent(inout) :: annual_btus_gocn12(0:6)
         real (kind=8) , intent(inout) :: month_btus_gocn12(0:6)
         real (kind=8) , intent(inout) ::
     +                       annual_btus_for_btu_tax_gocn12(0:6)
         logical (kind=1) , intent(in) :: btu_tax_active

         CHARACTER (len=6) :: CAP_LIMIT_FUEL_TYPES
         PARAMETER(CAP_LIMIT_FUEL_TYPES='GOCN12')
         integer (kind=1) :: L


         L = INDEX(CAP_LIMIT_FUEL_TYPES,PRIM_FUEL_TYPE)
         ANNUAL_BTUS_GOCN12(L) = ANNUAL_BTUS_GOCN12(L) + R_PRIM_HEAT
         MONTH_BTUS_GOCN12(L) = MONTH_BTUS_GOCN12(L) + R_PRIM_HEAT
         IF(BTU_TAX_ACTIVE) ANNUAL_BTUS_FOR_BTU_TAX_GOCN12(L) =
     +                           ANNUAL_BTUS_FOR_BTU_TAX_GOCN12(L) +
     +                                                   R_PRIM_HEAT
         L = INDEX(CAP_LIMIT_FUEL_TYPES,SEC_FUEL_TYPE)
         ANNUAL_BTUS_GOCN12(L) = ANNUAL_BTUS_GOCN12(L) + R_SEC_HEAT
         MONTH_BTUS_GOCN12(L) = MONTH_BTUS_GOCN12(L) + R_SEC_HEAT
         IF(BTU_TAX_ACTIVE) ANNUAL_BTUS_FOR_BTU_TAX_GOCN12(L) =
     +                           ANNUAL_BTUS_FOR_BTU_TAX_GOCN12(L) +
     +                                                    R_SEC_HEAT
         L = INDEX(CAP_LIMIT_FUEL_TYPES,EMISS_FUEL_TYPE)
         ANNUAL_BTUS_GOCN12(L) = ANNUAL_BTUS_GOCN12(L) + R_EMIS_HEAT
         MONTH_BTUS_GOCN12(L) = MONTH_BTUS_GOCN12(L) + R_EMIS_HEAT
         IF(BTU_TAX_ACTIVE) ANNUAL_BTUS_FOR_BTU_TAX_GOCN12(L) =
     +                              ANNUAL_BTUS_FOR_BTU_TAX_GOCN12(L) +
     +                                                      R_EMIS_HEAT
      RETURN

      END SUBROUTINE SUM_HEAT_BY_FUEL_TYPE


      SUBROUTINE GET_GIBSON5_ENERGY(GIBSON_BLOCK1_ENERGY,
     +                              GIBSON_BLOCK2_ENERGY,SEAS_HOURS,
     +                              ISEAS,UNITNO,BLK_MW,ARRAY_POINTR)

      use SpinDriftLib
      use prod_arrays_dimensions
      use kepcocom
      use lamcom
      use sizecom
      use prodcom
      use forecast

      LOGICAL (kind=1) ::   FOUND_THIS_MONTH_FORECAST

      INTEGER (kind=2) ::   GIB
      INTEGER (kind=2) ::   ISEAS,UNITNO,ARRAY_POINTR,PSI_CLASS_NUMBER
      REAL ::  PSI_LDC
      REAL ::  PSI_ENERGY_DEMAND
      REAL ::  PSI_PEAK_DEMAND
      REAL ::  PSI_BASE_DEMAND
      REAL ::  GIB5_CAP
      REAL ::  GIB5_ENERGY
      REAL ::  GIBSON_BLOCK1_ENERGY
      REAL ::  GIBSON_BLOCK2_ENERGY
      REAL ::  GIBSON_TOTAL_ENERGY
      REAL ::  BLK_MW
      REAL ::  SEAS_HOURS
      REAL ::  FORECAST_DSM_ENERGY(MAX_LOAD_CLASSES)
      REAL ::  THIS_MONTH_ENERGY
      REAL ::  THIS_MONTH_PEAK
!
! ADDED 9/1/94. GAT.
!
         CALL GET_CLASS_DSM_ENERGY_ALLOC(ISEAS,FORECAST_DSM_ENERGY)

         PSI_CLASS_NUMBER = 3
         FOUND_THIS_MONTH_FORECAST = GET_THIS_MONTH_CLASS_FORECAST(
     +                                    ISEAS,
     +                                    PSI_CLASS_NUMBER,
     +                                    THIS_MONTH_ENERGY,
     +                                    THIS_MONTH_PEAK)

         PSI_ENERGY_DEMAND =(THIS_MONTH_ENERGY +
     +                       FORECAST_DSM_ENERGY(PSI_CLASS_NUMBER))/
     +                                                        SEAS_HOURS
         PSI_PEAK_DEMAND = THIS_MONTH_PEAK ! ASSUMES MONTHLY PRODUCTION
         PSI_BASE_DEMAND = MAX(0.,
     +                           2.*PSI_ENERGY_DEMAND - PSI_PEAK_DEMAND)
         DO GIB = 1 , 2
            IF(GIB == 1) THEN ! CALCULATE TOTAL GIBSON ENERGY
               GIB5_CAP = MW(2,UNITNO) *
     +                   (1.-AREA_CAPACITY_LOSSES(3,ISEAS,ARRAY_POINTR))
            ELSE
               GIB5_CAP = BLK_MW
            ENDIF
            IF(GIB5_CAP <= PSI_BASE_DEMAND) THEN
               GIB5_ENERGY = GIB5_CAP
            ELSE
               IF(GIB5_CAP > PSI_PEAK_DEMAND) GIB5_CAP = PSI_PEAK_DEMAND
               PSI_LDC = (PSI_PEAK_DEMAND - GIB5_CAP)/
     +                               (PSI_PEAK_DEMAND - PSI_BASE_DEMAND)
               GIB5_ENERGY = PSI_BASE_DEMAND + (1. + PSI_LDC) *
     +                                   (GIB5_CAP - PSI_BASE_DEMAND)/2.
            ENDIF
            IF(GIB == 1) THEN
               GIBSON_TOTAL_ENERGY = GIB5_ENERGY
            ELSE
               GIBSON_BLOCK1_ENERGY = GIB5_ENERGY
            ENDIF
         ENDDO
         GIBSON_BLOCK2_ENERGY = GIBSON_TOTAL_ENERGY-GIBSON_BLOCK1_ENERGY
      RETURN

      END SUBROUTINE GET_GIBSON5_ENERGY


      SUBROUTINE WRITE_LAST_ELDC(LODDUR,LPROB,LAST_POINT,CURRENT,
     +                                gc_end_point,YEAR,ISEAS,
     +                                REMAINING_ENERGY_FOR_BLOCK,NBLOK2,
     +                                LAST_B_FOR_BLOCK)


      LOGICAL (kind=1) ::  WRITE_LAST_ELDC_NOT_OPEN=.TRUE. 
      INTEGER (kind=2) ::  LAST_POINT
      INTEGER (kind=2) ::  CURRENT
      INTEGER (kind=2) ::  gc_end_point
      INTEGER (kind=2) ::  YEAR
      INTEGER (kind=2) ::  ISEAS
      INTEGER (kind=2) ::  LAST_ELDC_NO=0 
      INTEGER (kind=2) ::  LAST_ELDC_HEADER
      INTEGER (kind=2) ::  I
      INTEGER (kind=2) ::  NBLOK2
      REAL (kind=4) ::  LODDUR(1000)
      REAL (kind=4) ::  LPROB(1000,2)
      REAL (kind=4) ::  REMAINING_ENERGY_FOR_BLOCK(*)
      REAL (kind=4) ::  LAST_B_FOR_BLOCK(*)

! END DATA DECLARATIONS
!
         IF(WRITE_LAST_ELDC_NOT_OPEN) THEN
            LAST_ELDC_NO = LAST_ELDC_HEADER()
            WRITE_LAST_ELDC_NOT_OPEN = .FALSE.
         ENDIF
         DO I = 1, LAST_POINT
       WRITE(LAST_ELDC_NO,1000) gc_end_point,',',YEAR,',',ISEAS,',',
     +                           I,',',LODDUR(I),',',LPROB(I,CURRENT)
         ENDDO
      RETURN
 1000 FORMAT(I4,A,I4,A,I2,A,I4,A,F8.1,A,F8.6)
 2000 FORMAT(I4,A,I4,A,I2,A,I4,A,F8.1,A,F8.1)

      END SUBROUTINE WRITE_LAST_ELDC


!
      SUBROUTINE PSI_LAST_RESOURCE_CALC(ISEAS,SEAS_HOURS,ENERGY)
!

!
      use SpinDriftLib
      use prod_arrays_dimensions
      use kepcocom
      USE SIZECOM

      use forecast
      implicit none

      LOGICAL (kind=1) ::   FOUND_THIS_MONTH_FORECAST
      INTEGER (kind=2) ::   PSI_CLASS_NUMBER,ISEAS
      REAL (kind=4) ::     ENERGY(2,MAX_CL_UNITS),SEAS_HOURS
      REAL (kind=4) ::     ENERGY_FROM_GIBSON
      REAL (kind=4) ::     FORECAST_DSM_ENERGY(MAX_LOAD_CLASSES)
      REAL (kind=4) ::     THIS_MONTH_ENERGY,THIS_MONTH_PEAK

! END DATA DECLARATIONS


         IF(WABASH_IM_NIPSCO .AND. GIBSON_POINTER /= 0 .AND.
     +                         PSI_AREA_LAST_RESOURCE_POINTER /= 0) THEN
            ENERGY_FROM_GIBSON = ENERGY(1,GIBSON_POINTER) +
     +                                          ENERGY(2,GIBSON_POINTER)
            IF(GIBSON_BACKUP_POINTER /= 0) THEN
               ENERGY_FROM_GIBSON = ENERGY_FROM_GIBSON +
     +                                 ENERGY(1,GIBSON_BACKUP_POINTER) +
     +                                   ENERGY(2,GIBSON_BACKUP_POINTER)
            ENDIF

            CALL GET_CLASS_DSM_ENERGY_ALLOC(ISEAS,FORECAST_DSM_ENERGY)

            PSI_CLASS_NUMBER = 3
            FOUND_THIS_MONTH_FORECAST = GET_THIS_MONTH_CLASS_FORECAST(
     +                                                ISEAS,
     +                                                PSI_CLASS_NUMBER,
     +                                                THIS_MONTH_ENERGY,
     +                                                THIS_MONTH_PEAK)

            ENERGY(2,PSI_AREA_LAST_RESOURCE_POINTER) =
     +            MAX(0.,(THIS_MONTH_ENERGY +
     +               FORECAST_DSM_ENERGY(PSI_CLASS_NUMBER))/SEAS_HOURS -
     +                                               ENERGY_FROM_GIBSON)
            ENERGY(1,PSI_AREA_LAST_RESOURCE_POINTER) = 0.
         ENDIF

      RETURN
      END SUBROUTINE PSI_LAST_RESOURCE_CALC

      SUBROUTINE WRITE_MONTHLY_GROUP_REPORT(R_ISEAS,R_YEAR,
     +                                 R_MAX_MONTHLY_GROUPS,
     +                                 R_MAX_MONTHLY_GROUP_VARIABLES,
     +                                 R_MONTHLY_grp_rpt,
     +                                 R_PEAK_MONTH)


      USE IREC_ENDPOINT_CONTROL
      use grx_planning_routines
      use SpinDriftLib
      use prod_arrays_dimensions
      USE SIZECOM
      use globecom
      use endpoint
      use month_extra
      use enrglimit


      INTEGER (kind=2) ::  R_ISEAS
      INTEGER (kind=2) ::  R_YEAR
      integer (kind=2) ::  loc_year
      INTEGER (kind=2) ::  R_MAX_MONTHLY_GROUPS
      INTEGER (kind=2) ::  R_MAX_MONTHLY_GROUP_VARIABLES
      INTEGER (kind=2) ::  VARIABLE_NUMBER
      INTEGER (kind=2) ::  MONTH_GROUP_UNIT
      INTEGER (kind=2) ::  MONTH_GROUP_HEADER
      INTEGER (kind=2) ::  MGI
      INTEGER (kind=2) ::  I
      INTEGER (kind=2) ::  ANNUAL=0 
      INTEGER (kind=2) ::  R_PEAK_MONTH
      INTEGER (kind=2) ::  F_PEAK_MONTH

      INTEGER (kind=2) ::  FISCAL_YEAR
      INTEGER :: MONTH_GROUP_REC
      SAVE MONTH_GROUP_REC,MONTH_GROUP_UNIT
      REAL (kind=4) ::  F_MONTHLY_GROUP_REPORT(0:12,0:99,17)
      REAL (kind=4) ::  R_MONTHLY_grp_rpt(0:12,0:99,17)
      REAL (kind=4) ::  EV_DATA_SOURCE=2. 
      REAL (kind=4) ::  ENVIRONMENTAL_RATE(5)
      REAL (kind=4) ::  TEMP_MMBTUS
      LOGICAL (kind=1) ::  MONTHLY_GROUP_NOT_OPEN=.TRUE. 
      LOGICAL (kind=1) ::  MONTHLY_GROUP_REPORT
      LOGICAL (kind=1) ::  GET_MONTHLY_CT_GROUP_REPORT
      LOGICAL (kind=1) ::  VOID_L
      logical (kind=1) :: FISCAL_RESET_PEAK_MONTH
      logical (kind=1) :: FISCAL_RESET_DATA
      CHARACTER (len=2) ::   LOAD_FILE_CHAR_EXT
      CHARACTER (len=9) ::   CL_MONTH_NAME(0:13)
      CHARACTER (len=20) ::   GROUP_NAME


      SAVE F_MONTHLY_GROUP_REPORT

! END DATA DECLARATIONS

         IF(.NOT. MONTHLY_GROUP_REPORT()) then
             RETURN
         end if

         YES_FISCAL_REPORTING =
     +                        IS_FISCAL_YEAR_ACTIVE(FISCAL_SEASON_RESET,
     +                                                      FISCAL_ONLY)
         IF(FISCAL_SEASON_RESET == 1) THEN
            FISCAL_SEASON = LAST_SEASON
         ELSE
            FISCAL_SEASON = FISCAL_SEASON_RESET - 1
         ENDIF

         loc_year = FLOAT(R_YEAR+get_BASE_YEAR())

         IF(FISCAL_ONLY) THEN
            IF(FISCAL_SEASON_RESET > 1 .AND.
     +                              R_ISEAS >= FISCAL_SEASON_RESET) THEN
               loc_year = FLOAT(R_YEAR+get_BASE_YEAR()+1)
            ENDIF
         ENDIF

! SHOULD BE RESET FOR THE FIRST MONTH OF THE SIMULATION AND IN EVERY
! FISCAL FIRST MONTH THEREAFTER.
         IF(FISCAL_RESET_DATA) THEN
            F_MONTHLY_GROUP_REPORT = 0.
            FISCAL_RESET_DATA = .FALSE.
         ENDIF

         IF(MONTHLY_GROUP_NOT_OPEN) THEN

            MONTHLY_GROUP_NOT_OPEN = .FALSE.
            ! 09/09/05. ADDED SIX VARIABLES FOR BURESH
            VARIABLE_NUMBER = R_MAX_MONTHLY_GROUP_VARIABLES + 6 
            MONTH_GROUP_UNIT = MONTH_GROUP_HEADER(VARIABLE_NUMBER,
     +                                            MONTH_GROUP_REC)

            CL_MONTH_NAME(0) = 'Annual   '
            CL_MONTH_NAME(1) = 'January  '
            CL_MONTH_NAME(2) = 'February '
            CL_MONTH_NAME(3) = 'March    '
            CL_MONTH_NAME(4) = 'April    '
            CL_MONTH_NAME(5) = 'May      '
            CL_MONTH_NAME(6) = 'June     '
            CL_MONTH_NAME(7) = 'July     '
            CL_MONTH_NAME(8) = 'August   '
            CL_MONTH_NAME(9) = 'September'
            CL_MONTH_NAME(10) = 'October  '
            CL_MONTH_NAME(11) = 'November '
            CL_MONTH_NAME(12) = 'December '
            CL_MONTH_NAME(13) = 'Fiscal   '
         ENDIF
         DO MGI = 0, R_MAX_MONTHLY_GROUPS

! 091306. MAJOR REWRITE. CREATE FIXED ARRAY SIZE FILES IN
!         CALLING ROUTINES FOR R_MONTHLY_grp_rpt.

            CALL GET_MONTHLY_EL_GROUP_REPORT(
     +                                  R_ISEAS,MGI,
     +                                  R_MONTHLY_grp_rpt)

            VOID_L = GET_MONTHLY_CT_GROUP_REPORT(
     +                                  R_ISEAS,MGI,
     +                                  R_MONTHLY_grp_rpt)
!
            R_MONTHLY_grp_rpt(ANNUAL,MGI,2) =
     +            R_MONTHLY_grp_rpt(ANNUAL,MGI,2) +
     +                      R_MONTHLY_grp_rpt(R_ISEAS,MGI,2)
            R_MONTHLY_grp_rpt(ANNUAL,MGI,3) =
     +            R_MONTHLY_grp_rpt(ANNUAL,MGI,3) +
     +                      R_MONTHLY_grp_rpt(R_ISEAS,MGI,3)
            R_MONTHLY_grp_rpt(ANNUAL,MGI,4) =
     +            R_MONTHLY_grp_rpt(ANNUAL,MGI,4) +
     +                      R_MONTHLY_grp_rpt(R_ISEAS,MGI,4)
            R_MONTHLY_grp_rpt(ANNUAL,MGI,5) =
     +            R_MONTHLY_grp_rpt(ANNUAL,MGI,5) +
     +                      R_MONTHLY_grp_rpt(R_ISEAS,MGI,5)
            R_MONTHLY_grp_rpt(ANNUAL,MGI,6) =
     +            R_MONTHLY_grp_rpt(ANNUAL,MGI,6) +
     +                      R_MONTHLY_grp_rpt(R_ISEAS,MGI,6)
            R_MONTHLY_grp_rpt(ANNUAL,MGI,7) =
     +            R_MONTHLY_grp_rpt(ANNUAL,MGI,7) +
     +                      R_MONTHLY_grp_rpt(R_ISEAS,MGI,7)
            R_MONTHLY_grp_rpt(ANNUAL,MGI,8) =
     +            R_MONTHLY_grp_rpt(ANNUAL,MGI,8) +
     +                      R_MONTHLY_grp_rpt(R_ISEAS,MGI,8)
            R_MONTHLY_grp_rpt(ANNUAL,MGI,9) =
     +            R_MONTHLY_grp_rpt(ANNUAL,MGI,9) +
     +                      R_MONTHLY_grp_rpt(R_ISEAS,MGI,9)
            R_MONTHLY_grp_rpt(ANNUAL,MGI,10) =
     +            R_MONTHLY_grp_rpt(ANNUAL,MGI,10) +
     +                      R_MONTHLY_grp_rpt(R_ISEAS,MGI,10)
            R_MONTHLY_grp_rpt(ANNUAL,MGI,11) =
     +            R_MONTHLY_grp_rpt(ANNUAL,MGI,11) +
     +                      R_MONTHLY_grp_rpt(R_ISEAS,MGI,11)
            R_MONTHLY_grp_rpt(ANNUAL,MGI,12) =
     +            R_MONTHLY_grp_rpt(ANNUAL,MGI,12) +
     +                      R_MONTHLY_grp_rpt(R_ISEAS,MGI,12)
            R_MONTHLY_grp_rpt(ANNUAL,MGI,13) =
     +            R_MONTHLY_grp_rpt(ANNUAL,MGI,13) +
     +                      R_MONTHLY_grp_rpt(R_ISEAS,MGI,13)
            R_MONTHLY_grp_rpt(ANNUAL,MGI,14) =
     +            R_MONTHLY_grp_rpt(ANNUAL,MGI,14) +
     +                      R_MONTHLY_grp_rpt(R_ISEAS,MGI,14)
            R_MONTHLY_grp_rpt(ANNUAL,MGI,15) =
     +            R_MONTHLY_grp_rpt(ANNUAL,MGI,15) +
     +                      R_MONTHLY_grp_rpt(R_ISEAS,MGI,15)

! 083106. ACCUMULATE FOR FISCAL YEAR

            F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,2) =
     +            F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,2) +
     +                 R_MONTHLY_grp_rpt(R_ISEAS,MGI,2)/1000000.
            F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,3) =
     +            F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,3) +
     +                R_MONTHLY_grp_rpt(R_ISEAS,MGI,3)/1000.
            F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,4) =
     +            F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,4) +
     +                      R_MONTHLY_grp_rpt(R_ISEAS,MGI,4)
            F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,5) =
     +            F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,5) +
     +                      R_MONTHLY_grp_rpt(R_ISEAS,MGI,5)
            F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,6) =
     +            F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,6) +
     +                      R_MONTHLY_grp_rpt(R_ISEAS,MGI,6)
            F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,7) =
     +            F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,7) +
     +                      R_MONTHLY_grp_rpt(R_ISEAS,MGI,7)
            F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,8) =
     +            F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,8) +
     +                      R_MONTHLY_grp_rpt(R_ISEAS,MGI,8)
            F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,9) =
     +            F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,9) +
     +                      R_MONTHLY_grp_rpt(R_ISEAS,MGI,9)
            F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,10) =
     +            F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,10) +
     +                      R_MONTHLY_grp_rpt(R_ISEAS,MGI,10)
            F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,11) =
     +            F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,11) +
     +                      R_MONTHLY_grp_rpt(R_ISEAS,MGI,11)
            F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,12) =
     +            F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,12) +
     +                      R_MONTHLY_grp_rpt(R_ISEAS,MGI,12)
            F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,13) =
     +            F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,13) +
     +                      R_MONTHLY_grp_rpt(R_ISEAS,MGI,13)
            F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,14) =
     +            F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,14) +
     +                      R_MONTHLY_grp_rpt(R_ISEAS,MGI,14)
            F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,15) =
     +            F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,15) +
     +                      R_MONTHLY_grp_rpt(R_ISEAS,MGI,15)

            IF(R_ISEAS == R_PEAK_MONTH .OR.
     +                                     FISCAL_RESET_PEAK_MONTH) THEN
               F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,1) =
     +                             R_MONTHLY_grp_rpt(R_ISEAS,MGI,1)
               F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,16) =
     +                            R_MONTHLY_grp_rpt(R_ISEAS,MGI,16)
            ENDIF

! 092206. MOVED FOR FISCAL REPORTING.

            R_MONTHLY_grp_rpt(R_ISEAS,MGI,2) =
     +       R_MONTHLY_grp_rpt(R_ISEAS,MGI,2)/1000000. ! fixed_om
            R_MONTHLY_grp_rpt(R_ISEAS,MGI,3) =
     +            R_MONTHLY_grp_rpt(R_ISEAS,MGI,3)/1000. ! ENERGY

            IF(R_MONTHLY_grp_rpt(R_ISEAS,MGI,3) > 0.001) THEN
               R_MONTHLY_grp_rpt(R_ISEAS,MGI,4) =
     +             R_MONTHLY_grp_rpt(R_ISEAS,MGI,4)/ ! AVE HR
     +                       R_MONTHLY_grp_rpt(R_ISEAS,MGI,3)
            ELSE
               R_MONTHLY_grp_rpt(R_ISEAS,MGI,4) = 0.
            ENDIF

            R_MONTHLY_grp_rpt(R_ISEAS,MGI,5) =
     +            R_MONTHLY_grp_rpt(R_ISEAS,MGI,5)/1000000. ! FUEL
            R_MONTHLY_grp_rpt(R_ISEAS,MGI,6) =
     +      R_MONTHLY_grp_rpt(R_ISEAS,MGI,6)/1000000. ! VARIABLE O&M

            R_MONTHLY_grp_rpt(R_ISEAS,MGI,12) =
     +            R_MONTHLY_grp_rpt(R_ISEAS,MGI,12)/1000000.
            R_MONTHLY_grp_rpt(R_ISEAS,MGI,13) =
     +            R_MONTHLY_grp_rpt(R_ISEAS,MGI,13)/1000.
            R_MONTHLY_grp_rpt(R_ISEAS,MGI,14) =
     +            R_MONTHLY_grp_rpt(R_ISEAS,MGI,14)/1000000.
            R_MONTHLY_grp_rpt(R_ISEAS,MGI,15) =
     +            R_MONTHLY_grp_rpt(R_ISEAS,MGI,15)/1000.
! 
            R_MONTHLY_grp_rpt(R_ISEAS,MGI,17) =
     +            R_MONTHLY_grp_rpt(R_ISEAS,MGI,12) -
     +            R_MONTHLY_grp_rpt(R_ISEAS,MGI,5) -
     +            R_MONTHLY_grp_rpt(R_ISEAS,MGI,6) -
     +            R_MONTHLY_grp_rpt(R_ISEAS,MGI,2)
            R_MONTHLY_grp_rpt(ANNUAL,MGI,17) =
     +            R_MONTHLY_grp_rpt(ANNUAL,MGI,17) +
     +                      R_MONTHLY_grp_rpt(R_ISEAS,MGI,17)

           F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,17) =
     +            F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,17) +
     +                      R_MONTHLY_grp_rpt(R_ISEAS,MGI,17)

            GROUP_NAME = 'GROUP  '//LOAD_FILE_CHAR_EXT(MGI)

            ENVIRONMENTAL_RATE = 0.
            TEMP_MMBTUS = R_MONTHLY_grp_rpt(R_ISEAS,MGI,3)*
     +                           R_MONTHLY_grp_rpt(R_ISEAS,MGI,4)
            DO I = 1, 5
               IF(TEMP_MMBTUS < .001) CYCLE
               ENVIRONMENTAL_RATE(I) =
     +               R_MONTHLY_grp_rpt(R_ISEAS,MGI,I+6)*2000. /
     +                                                       TEMP_MMBTUS
            END DO

            WRITE(MONTH_GROUP_UNIT,REC=MONTH_GROUP_REC)
     +         PRT_ENDPOINT(),
     +         FLOAT(loc_year),
     +         CL_MONTH_NAME(R_ISEAS),
     +         GROUP_NAME,
     +         (R_MONTHLY_grp_rpt(R_ISEAS,MGI,I),I = 1,
     +                                   R_MAX_MONTHLY_GROUP_VARIABLES),
     +         (ENVIRONMENTAL_RATE(I),I = 1, 5),
     +         EV_DATA_SOURCE
            MONTH_GROUP_REC = MONTH_GROUP_REC + 1
         ENDDO ! GROUPS

         IF(FISCAL_RESET_PEAK_MONTH) THEN
            FISCAL_RESET_PEAK_MONTH = .FALSE.
         ENDIF

         IF(R_ISEAS == 12 .AND. .NOT. FISCAL_ONLY) THEN
            DO MGI = 0, R_MAX_MONTHLY_GROUPS

               R_MONTHLY_grp_rpt(ANNUAL,MGI,1) =
     + ! CAPACITY: NOTE USE PEAK MONTH
     +            R_MONTHLY_grp_rpt(R_PEAK_MONTH,MGI,1) 

               R_MONTHLY_grp_rpt(ANNUAL,MGI,16) =
     + ! ! EQVIVALENT CAPACITY: NOTE USE PEAK MONTH
     +            R_MONTHLY_grp_rpt(R_PEAK_MONTH,MGI,16) 

               R_MONTHLY_grp_rpt(ANNUAL,MGI,2) =
     +          R_MONTHLY_grp_rpt(ANNUAL,MGI,2)/1000000. ! fixed_om
               R_MONTHLY_grp_rpt(ANNUAL,MGI,3) =
     +            R_MONTHLY_grp_rpt(ANNUAL,MGI,3)/1000. ! ENERGY
               IF(R_MONTHLY_grp_rpt(ANNUAL,MGI,3) > 0.001) THEN
                  R_MONTHLY_grp_rpt(ANNUAL,MGI,4) =
     +               R_MONTHLY_grp_rpt(ANNUAL,MGI,4)/ ! AVE HR
     +                       R_MONTHLY_grp_rpt(ANNUAL,MGI,3)
               ELSE
                  R_MONTHLY_grp_rpt(ANNUAL,MGI,4) = 0.
               ENDIF
               R_MONTHLY_grp_rpt(ANNUAL,MGI,5) =
     +            R_MONTHLY_grp_rpt(ANNUAL,MGI,5)/1000000. ! FUEL
               R_MONTHLY_grp_rpt(ANNUAL,MGI,6) =
     +      R_MONTHLY_grp_rpt(ANNUAL,MGI,6)/1000000. ! variable_om

               R_MONTHLY_grp_rpt(ANNUAL,MGI,12) =
     +            R_MONTHLY_grp_rpt(ANNUAL,MGI,12)/1000000.
               R_MONTHLY_grp_rpt(ANNUAL,MGI,13) =
     +            R_MONTHLY_grp_rpt(ANNUAL,MGI,13)/1000.
               R_MONTHLY_grp_rpt(ANNUAL,MGI,14) =
     +            R_MONTHLY_grp_rpt(ANNUAL,MGI,14)/1000000.
               R_MONTHLY_grp_rpt(ANNUAL,MGI,15) =
     +            R_MONTHLY_grp_rpt(ANNUAL,MGI,15)/1000.

               GROUP_NAME = 'GROUP  '//LOAD_FILE_CHAR_EXT(MGI)

               ENVIRONMENTAL_RATE = 0.
               TEMP_MMBTUS = R_MONTHLY_grp_rpt(ANNUAL,MGI,3)*
     +                             R_MONTHLY_grp_rpt(ANNUAL,MGI,4)
               DO I = 1, 5
                  IF(TEMP_MMBTUS < .001) CYCLE
                  ENVIRONMENTAL_RATE(I) =
     +               R_MONTHLY_grp_rpt(ANNUAL,MGI,I+6)*2000. /
     +                                                       TEMP_MMBTUS
               END DO

               WRITE(MONTH_GROUP_UNIT,REC=MONTH_GROUP_REC)
     +            PRT_ENDPOINT(),
     +            FLOAT(loc_year),
     +            CL_MONTH_NAME(ANNUAL),
     +            GROUP_NAME,
     +            (R_MONTHLY_grp_rpt(ANNUAL,MGI,I),I = 1,
     +                                   R_MAX_MONTHLY_GROUP_VARIABLES),
     +            (ENVIRONMENTAL_RATE(I),I = 1, 5),
     +            EV_DATA_SOURCE
               MONTH_GROUP_REC = MONTH_GROUP_REC + 1
            ENDDO
         ENDIF ! R_ISEAS == 12

! 083106. FOR SRP.

         IF(YES_FISCAL_REPORTING .AND. R_ISEAS == FISCAL_SEASON) THEN

            DO MGI = 0, R_MAX_MONTHLY_GROUPS

! 092206. TOOK UNITS CONVERSIOS OUT.

               F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,2) =

     +               F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,2) 

               IF(F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,3) > 0.001) THEN
                  F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,4) =
     +               F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,4)/ 
     +                       F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,3)
               ELSE
                  F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,4) = 0.
               ENDIF

               F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,3) =
     +            F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,3) 

               F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,5) =
     +            F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,5)/1000000. 
               F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,6) =

     +            F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,6)/1000000. 

               F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,12) =
     +            F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,12)/1000000.
               F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,13) =
     +            F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,13)/1000.
               F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,14) =
     +            F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,14)/1000000.
               F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,15) =
     +            F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,15)/1000.

               GROUP_NAME = 'GROUP  '//LOAD_FILE_CHAR_EXT(MGI)

               ENVIRONMENTAL_RATE = 0.
               TEMP_MMBTUS = F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,3)*
     +                             F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,4)
               DO I = 1, 5
                  IF(TEMP_MMBTUS < .001) CYCLE
                  ENVIRONMENTAL_RATE(I) =
     +               F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,I+6)*2000. /
     +                                                       TEMP_MMBTUS
               END DO

               WRITE(MONTH_GROUP_UNIT,REC=MONTH_GROUP_REC)
     +            PRT_ENDPOINT(),
     +            FLOAT(loc_year),
     +            CL_MONTH_NAME(13),
     +            GROUP_NAME,
     +            (F_MONTHLY_GROUP_REPORT(ANNUAL,MGI,I),I = 1,
     +                                   R_MAX_MONTHLY_GROUP_VARIABLES),
     +            (ENVIRONMENTAL_RATE(I),I = 1, 5),
     +            EV_DATA_SOURCE
               MONTH_GROUP_REC = MONTH_GROUP_REC + 1
            ENDDO ! GROUPS

            F_MONTHLY_GROUP_REPORT = 0.
         ENDIF ! ISEAS == FISCAL_SEASON
      

      END SUBROUTINE WRITE_MONTHLY_GROUP_REPORT


      SUBROUTINE RESET_FISCAL_MONTHLY_GROUP()

      IMPLICIT NONE
         FISCAL_RESET_DATA = .TRUE.
         FISCAL_RESET_PEAK_MONTH = .TRUE.
      RETURN
      END SUBROUTINE RESET_FISCAL_MONTHLY_GROUP


      FUNCTION IS_A_VALID_RPS_PM(R_PM)

         LOGICAL (kind=1) ::   IS_A_VALID_RPS_PM
         INTEGER (kind=2) ::   R_PM


      IS_A_VALID_RPS_PM = .FALSE.
      IF(R_PM == 2 .OR. R_PM == 3 .OR. R_PM == 5 .OR. R_PM == 9 .OR.
     +             R_PM == 11 .OR. R_PM == 12 .OR.
     +                            R_PM == 16) IS_A_VALID_RPS_PM = .TRUE.
      RETURN
      END FUNCTION IS_A_VALID_RPS_PM


      end module dr_booth_modules
