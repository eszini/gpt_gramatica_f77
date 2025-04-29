!     Last change: msg 3/17/2022 9:55:49 AM


!             THE PRODUCTION & INCREMENTAL COST ANALYSIS MODEL (PICAm)
!               COPYRIGHT (C) 1981, 1982, 1983, 1984, 1985, 1991, 1992
!                        M.S. GERBER & ASSOCIATES, INC.
!                              ALL RIGHTS RESERVED




      RECURSIVE FUNCTION PROCOST(CoalLPActive)

      INCLUDE 'SpinLib.MON'
      ! The original procost function doesn't define
      ! the variable globecom_year.
      ! It also doesn't INCLUDE anything that exports it, and no
      ! use statements available export it either. Assuming the only 
      ! shared globecom_YEAR variable in Globecom is the one that 
      ! was used. _(File didn't import globecom.mon originally)_. 
      ! WORKAROUND: In Legacy, renamed globecom.mon:globecom_year to 
      ! globecom_year and PROCOST was unable to see it.  That's
      ! a confirmation that Globecom_Year IS USED in PROCOST.
      use aitvafin
      use aitvaprod
      use annl_nuc
      use cap_prod_costs
      use cap_trans
      use cls_load
      use contracts_data
      use conversion_routines
      use dr_booth_modules
      use dsex_obj_interfaces
      use elrptcom
      use enrg_prod_costs
      use enrg_prod_costsi4
      use enrg_prod_costsr8
      use enrglimit
      use envircom
      use forecast
      use foshydi2com
      use globecom
      use group_prod
      use grx_planning_routines
      use irec_endpoint_control
      use month_extra
      use mwunih
      use poolcom
      use procsave_common
      use prod2com
      use prodcom
      use servcom
      use service_decs
      use sizecom
      use procost_data
      use annual_cl_unit
      use annual_contracts
      use monthly_icap
      use grxmodules
      use grx_captrade
      use kootenay
      use program_state
      use debugtrace
      use energy_trace
      
      SAVE
      integer (kind=2) :: procmeth_result
      integer (kind=2) :: imm_nunits
      CHARACTER (len=5) ::  GET_SCENAME

      LOGICAL (KIND=4) :: TEMP_L4,COAL_MODEL_RAN_LAST_ITERATION
      LOGICAL (KIND=4) :: STOP_HERE

      REAL (KIND=4) :: SYS_CO2_EMISSIONS
      integer (kind=2) ::  ji
      LOGICAL (kind=1) ::  CALUCLATE_NUC_FUEL_PRICES
      LOGICAL (kind=1) ::  TRANS_HYDRO
      LOGICAL (kind=1) ::  YES_RUN_MULTIAREA_TRANSACT
      LOGICAL (kind=1) ::  INIT_TRANS_LOAD_AFTER_EL
      LOGICAL (kind=1) ::  TF_FILE_EXISTS=.FALSE. 
      LOGICAL (kind=1) ::  YES_USE_TRANSACT_LOADS
      LOGICAL (kind=1) ::  YES_STRICT_MARKET_PRICE
      LOGICAL (kind=1) ::  YES_USE_TF_FILE_FOR_MULTIAREA
      LOGICAL (kind=1) ::  YES_CENTRAL_DISPATCH_TRANSACT
      LOGICAL (kind=1) ::  APPLY_NOX_SEASON
      LOGICAL (kind=1) ::  SEASON_IS_NOX_SEASON
      LOGICAL (kind=1) ::  CALCULATE_NOX
      LOGICAL (kind=1) ::  NOX_CONTROL_FOR_UNIT
      LOGICAL (kind=1) ::  SOX_CONTROL_FOR_UNIT
      LOGICAL (kind=1) ::  CO2_CONTROL_FOR_UNIT
      LOGICAL (kind=1) ::  HG_CONTROL_FOR_UNIT
      LOGICAL (kind=1) ::  OTHER3_CONTROL_FOR_UNIT
      LOGICAL (kind=1) ::  GET_NOX_CONTROL_FOR_UNIT
      LOGICAL (kind=1) ::  GET_SOX_CONTROL_FOR_UNIT
      LOGICAL (kind=1) ::  GET_CO2_CONTROL_FOR_UNIT
      LOGICAL (kind=1) ::  CO2_FIRST_CONTROL_MONTH
      LOGICAL (kind=1) ::  CO2_FIRST_CONTROL_YEAR
      LOGICAL (kind=1) ::  CO2_YEAR_BEFORE_CONTROL
      LOGICAL (kind=1) ::  GET_HG_CONTROL_FOR_UNIT
      LOGICAL (kind=1) ::  GET_OTHER3_CONTROL_FOR_UNIT
      LOGICAL (kind=1) ::  UNIT_IS_IN_NOX_SEASON_GROUP
      LOGICAL (kind=1) ::  TRANS_NOX_SEASON_ACTIVE=.FALSE. 
      LOGICAL (kind=1) ::  NOX_ACTIVE_IN_TG(:)
      LOGICAL (kind=1) ::  NOX_YEAR_ACTIVE_IN_TG(:)
      LOGICAL (kind=1) ::  FUEL_POINTERS_USED
      LOGICAL (kind=1) ::  WEATHER_ANALYST_ACTIVE=.FALSE. 
      LOGICAL (kind=1) ::  GAS_WEATHER_ANALYST_ACTIVE=.FALSE. 
      LOGICAL (kind=1) ::  MANAGE_WEATHER_DEMAND_FORECASTS
      LOGICAL (kind=1) ::  MANAGE_WEEKLY_HYDRO_FORECASTS
      LOGICAL (kind=1) ::  MANAGE_GAS_WEATHER_DEMAND_FORECASTS
      LOGICAL (kind=1) ::  WEEKLY_HYDRO_ACTIVE=.FALSE. 
      LOGICAL (kind=1) ::  YES_REGIONAL_OUTAGES_ACTIVE=.FALSE. 
      LOGICAL (kind=1) ::  REGIONAL_PARAMS_ACTIVE
      LOGICAL (kind=1) ::  REGIONAL_OUTAGES_ACTIVE
      LOGICAL (kind=1) ::  NOX_ACTIVE_FOR_UNIT
      LOGICAL (kind=1) ::  YES_UNIT_COMMITMENT_LOGIC
      LOGICAL (kind=1) ::  UNIT_COMMITMENT_LOGIC
      LOGICAL (kind=1) ::  TRANSACT_ANALYST_ONLY
      LOGICAL (kind=1) ::  INIT_FILE_IS_ACTIVE
      LOGICAL (kind=1) ::  IS_A_MARKET_RESOURCE
      LOGICAL (kind=1) ::  REGIONAL_HYDRO_PARAM_ACTIVE
      LOGICAL (kind=1) ::  END_MONTH_FUEL_DERIVATIVES
      LOGICAL (kind=1) ::  IS_REGIONAL_HYDRO_PARAM_ACTIVE=.FALSE. 
      LOGICAL (kind=1) ::  TEMP_L
      LOGICAL (kind=1) ::  RESURRECT_RETROFIT_UNIT
      LOGICAL (kind=1) ::  ANNUAL_CO2_RETIREMENTS_PROCESS
      LOGICAL (kind=1) ::  ANNUAL_CO2_RETROFIT_PROCESS
      LOGICAL (kind=1) ::  ANNUAL_RETIRE_RETRO_PROCESS
      LOGICAL (kind=1) ::  RPS_PROGRAMS_ANNUAL_CALC
      LOGICAL (kind=1) ::  WRITE_HOURLY_STORAGE_REPORT
      LOGICAL (kind=1) ::  UPDATE_STATUS_REPORT
      LOGICAL (kind=1) ::  READ_TRANS_CONSTRAINT_DATA
      LOGICAL (kind=1) ::  WRITE_MONTHLY_GAS_DEMAND_CLASS_SUMMARY
      LOGICAL (kind=1) ::  READ_TRANS_PATH_DATA
      LOGICAL (kind=1) ::  ANNUAL_GAS_SUPPLY_CURVES
      LOGICAL (kind=1) ::  annual_lp_basin_gas
      LOGICAL (kind=1) ::  GAS_MODEL_ONLY
      LOGICAL (kind=1) ::  YES_GAS_MODEL_ONLY
      LOGICAL (kind=1) ::  INIT_CLA_UNIT_UP_YESTERDAY
      LOGICAL (kind=1) ::  RPS_PROGRAMS_ANNUAL_INIT
      LOGICAL (kind=1) ::  ANNUAL_LP_GAS_PRICING
      LOGICAL (kind=1) ::  MONTHLY_GAS_SUPPLY_CURVES
      LOGICAL (kind=1) ::  NEW_SUPPLY_CURVE_REPORT
      LOGICAL (kind=1) ::  GAS_MODEL_ACTIVE
      LOGICAL (kind=1) ::  HH_ONLY
      LOGICAL (kind=1) ::  RUN_HH_ONLY
      LOGICAL (kind=1) ::  GAS_DATA_AVAILABLE
      LOGICAL (kind=1) ::  YES_GAS_MODEL_DATA_AVAILABLE
      LOGICAL (kind=1) ::  RUN_GAS_MODEL
      LOGICAL (kind=1) ::  gas_lp_active
      LOGICAL (kind=1) ::  LP_GAS_MODEL
      LOGICAL (kind=1) ::  COMMIT_ON_TOTAL_COST_ENV=.FALSE. 
      LOGICAL (kind=1) ::  YES_COMMIT_ON_TOTAL_COST_ENV
      LOGICAL (kind=1) ::  RE_ALLOCATE_NUNITS_ARRAYS
      LOGICAL (kind=1) ::  RUN_COAL_MODEL
      CHARACTER (len=1) ::  NOX_SEASON_REGION(:)
      CHARACTER (len=1) ::  GET_TRANS_GROUP_NOX_SEASON
      INTEGER (kind=2) ::  MAX_TRANS_GROUP_nr=0 
      INTEGER (kind=2) ::  GET_MAX_TRANS_GROUP_NUMBER
      INTEGER (kind=2) ::  TG
      INTEGER (kind=2) ::  get_TRANSACTION_GROUP
      INTEGER (kind=2) ::  HYDRO_TRANS_GROUP
      INTEGER (kind=2) ::  HH_VECTOR
      INTEGER (kind=2) ::  GET_HH_VECTOR
      INTEGER (kind=2) ::  CURRENT_Yr
      INTEGER (kind=2) ::  NOX_YEAR(:)
      INTEGER (kind=2) ::  END_NOX_YEAR(:)
      INTEGER (kind=2) ::  GET_TRANS_GROUP_NOX_YEAR
      INTEGER (kind=2) ::  GET_TRANS_GROUP_END_NOX_YEAR
      INTEGER (kind=2) ::  HYDRO_LOAD_NO
      INTEGER (kind=2) ::  GET_TRANS_LOAD_AGGREGATION
      INTEGER (kind=2) ::  NUMBER_OF_HYDRO_LOAD_GROUPS
      INTEGER (kind=2) ::  GET_NUMBER_OF_HYDRO_GROUPS
      INTEGER (kind=2) ::  FIRST_HYDRO_LOAD_NO(:)
      INTEGER (kind=2) ::  HYDRO_AG_IPNTX(:)
      INTEGER (kind=2) ::  PLOT_PTS
      INTEGER (kind=2) ::  GSP
      INTEGER (kind=2) ::  GET_UNIT_GAS_REGION_INDEX
      INTEGER (kind=2) ::  MAX_GAS_REGION_NO=500 
      INTEGER (kind=2) ::  NUM_GAS_NODES
      INTEGER (kind=2) ::  NUM_GAS_LINKS


      ALLOCATABLE :: NOX_SEASON_REGION,NOX_ACTIVE_IN_TG,
     +               NOX_YEAR_ACTIVE_IN_TG,
     +               NOX_YEAR,
     +               END_NOX_YEAR,
     +               FIRST_HYDRO_LOAD_NO,
     +               HYDRO_AG_IPNTX


      INTEGER*4 PROCOST 
      LOGICAL*1 :: READ_PRODUCTION_PARAMETERS ! External
      logical (kind=1), intent(inout) :: CoalLPActive
      REAL GET_SYSTEM_CAPACITY,
     +     GET_ANNUAL_PEAK,
     +     GET_MONTHLY_VAR_SUM,LOCAL_CLASS_RESERVE_MARGIN,
     +     GET_MONTHLY_FUEL_INDEX,
     +     P_DELIVERY_COST,
     +     S_DELIVERY_COST,
     +     GET_DISPADJ_SAVE,
     +     GET_DISPADJ2_SAVE,
     +     CO2_RETRO_HEAT_MULT,GET_CO2_RETRO_HEAT_MULT,
     +     CO2_RETRO_CAP_MULT,GET_CO2_RETRO_CAP_MULT
      CHARACTER*1 PALO_ALTO


      CHARACTER*11 CLK ! INTRINSIC TIME FUNCTION.

      PARAMETER(PALO_ALTO='P')
      CHARACTER (len=5) ::  AREA_ALLOCATORS_FILE
      INTEGER (kind=1) ::  MO
      REAL ::  TOTAL_AREA_CAP_ALLOCATORS,TOTAL_AREA_LOSSES,LOSSES
      CHARACTER (len=6) ::  CAP_LIMIT_FUEL_TYPES,ENRG_LIMIT_FUEL_TYPES*3
      CHARACTER (len=20) ::  TEMP_UNIT_NAME
      CHARACTER (len=2) ::  CHR_ISEAS
      PARAMETER(CAP_LIMIT_FUEL_TYPES='GOCN12',
     +          ENRG_LIMIT_FUEL_TYPES='H12')

      CHARACTER (len=1) ::  UTILITY_TYPE,COUNTRY
      LOGICAL (kind=1) ::  OFFSET_MAINTENANCE_VECTORS
      LOGICAL (kind=1) ::  DO_IT
      LOGICAL (kind=1) ::  MONTHLY_INVENTORY_ACTIVE
      LOGICAL (kind=1) ::  ECON_SWITCH
      LOGICAL (kind=1) ::  BEFORE_LOAD_CURVE
      LOGICAL (kind=1) ::  CLASS_EXISTS_loc(MAX_LOAD_CLASSES)
      LOGICAL (kind=1) ::  CLASS_IN_AREA_1(MAX_LOAD_CLASSES)
      LOGICAL (kind=1) ::  PRICE_PLANNING_FIRST_PASS
      LOGICAL (kind=1) ::  LOLP_REPORT
      LOGICAL (kind=1) ::  HAVE_HOURLY_MC_AFTER_EL
      LOGICAL (kind=1) ::  CAL_HOURLY_MC_AFTER_EL
      INTEGER (kind=2) ::  NUM_UNITS_BEFORE_ADDITIONS
      INTEGER (kind=2) ::  RETURN_CL_UNITS_B4_ADDITIONS
      INTEGER (kind=2) ::  PERIOD
      INTEGER (kind=2) ::  POINTER
      INTEGER (kind=2) ::  PSMO
      INTEGER (kind=2) ::  PEMO
      INTEGER (kind=2) ::  PROCMETH

      INTEGER (kind=2) ::  I, i_for_argument
      INTEGER (kind=2) ::  M
      INTEGER (kind=2) ::  ISEAS
      INTEGER (kind=2) ::  SET_CURRENT_IPNT
      INTEGER (kind=2) ::  IPNTX
      INTEGER (kind=2) ::   HOURS_INCREMENT
      INTEGER (kind=2) ::  EM
      INTEGER (kind=2) ::  PRODUCTION_PERIODS
      INTEGER (kind=2) ::  J
      INTEGER (kind=2) ::  BASE_DATE
      INTEGER (kind=2) ::  PEAK_MONTH
      INTEGER (kind=2) ::  HISTORICAL_PRODUCTION_DATE
      INTEGER (kind=2) ::   GET_YR
      INTEGER (kind=2) ::  RUN_YEARS
      INTEGER (kind=2) ::  PRODUCTION_COSTING_PERIODS
      INTEGER (kind=2) ::  R_ISEAS
      INTEGER (kind=2) ::  R_PSMO
      INTEGER (kind=2) ::  R_PEMO
      INTEGER (kind=2) ::  TRANS
      INTEGER (kind=2) ::  MAX_TRANS_LOAD_GROUPS
      INTEGER (kind=2) ::  NUMBER_OF_ACTIVE_TRANS_GROUPS
      INTEGER (kind=2) ::  GET_NUMBER_OF_ACTIVE_GROUPS
      INTEGER (kind=2) ::  GET_THERMAL_ONLINE
      INTEGER (kind=2) ::  SAVE_Yr
      INTEGER (kind=2) ::  LAST_NUNITS=0 
      INTEGER (kind=2) ::  GET_PROCOST_LAST_NUNITS
      INTEGER (kind=2) ::  FT
      INTEGER (kind=2) ::  GET_PRIMARY_MOVER
!
      REAL ::  ESCALATED_MONTHLY_VALUE
      REAL ::  ESCALATE_THERMAL_VOM
      REAL ::  ESCALATE_THERMAL_FOM
      REAL ::  LEASOR_MARK_UP
      REAL ::  TRANS_LOAD_PROB_2_HOURS
      REAL ::  HYDRO_LOAD_PROB_2_HOURS
      REAL ::  SEASONAL_SYSTEM_CAP
      REAL ::  RTEMP
      REAL ::  RTEMP012(0:12)
      REAL ::  GET_CL_POOL_FRAC_OWN
      REAL ::  RTEMP_2
      REAL ::  RTEMP_3
      REAL ::  RTEMP_4
      REAL ::  RTEMP_5
      REAL ::  RTEMP_6
      REAL ::  RTEMP_7
      REAL ::  RTEMP_8
      REAL ::  RTEMP_9
      REAL ::  RTEMP_10
      REAL ::  RETRO_VOM
      REAL ::  RETRO_FOM
      REAL ::  ANNUAL_PEAK=0. 
      REAL ::  PEAK
      REAL ::  DX
      REAL ::  TRANS_PEAK
      REAL ::  BASE_TO_PEAK_RATIO
      REAL ::  PEAK_AFTER_DSM
      REAL ::  PEAK_AFTER_EL
      REAL ::  BASE_AFTER_DSM
      REAL ::  LPROB(CONVOLUTION_POINTS)
      REAL ::  LODDUR(CONVOLUTION_POINTS)
      REAL ::  HEAT_RATE_FACTOR(MAX_CL_UNITS)
      REAL ::  GET_HEAT_RATE_FACTOR
      REAL ::  MAINTENANCE_RATE(MAX_CL_UNITS)
      REAL ::  GET_VAR
      REAL ::  HYDRO_MONTHLY_ENRG(MAX_EL_UNITS)
      REAL ::  HYDROCAPMO(MAX_EL_UNITS)
      REAL ::  EL_SO2(MAX_EL_UNITS)
      REAL ::  ENERGY_EXCHANGE_ADJUSTMENT
      REAL ::  EL_SO2_PERIOD(12)
      REAL ::  RR_OBS(LOAD_CURVE_POINTS)
      REAL ::  AVE_RUNNING_RATE(LOAD_CURVE_POINTS)
      REAL ::  DIVIDE_BY
      REAL ::  DISP_BTU_COST(MAX_CL_UNITS)
      REAL ::  MN_RATE
      REAL ::  REMAIN_ANN_ENRG(MAX_CONTRACTS)
      REAL ::  MAXIMUM_ANNUAL_CAPACITY(MAX_CONTRACTS)
      REAL ::  CT_TARGET_ANN_ENRG(MAX_CONTRACTS)
      REAL ::  CNTR_WEIGHTED_CAPACITY(MAX_CONTRACTS)
      REAL ::  NUCLEAR_AVAIL_MULT
      REAL ::  GET_SCENARIO_NUCLEAR_AVAIL
      REAL ::  COAL_AVAIL_MULT
      REAL ::  GET_SCENARIO_COAL_AVAIL
      REAL ::  GET_SCENARIO_GAS_AVAIL
      REAL ::  GAS_AVAIL_MULT
      REAL ::  GET_SCENARIO_OIL_AVAIL
      REAL ::  GET_SCENARIO_OTHER_AVAIL
      REAL ::  OIL_AVAIL_MULT
      REAL ::  OTHER_AVAIL_MULT
      REAL ::  HYDRO_WATER_YEAR_MULT
      REAL ::  GET_SCENARIO_HYDRO_WATER_YEAR
      REAL ::  GET_PARAM_HYDRO
      REAL ::  FUEL_SCEN_MULT(5)
      REAL ::   GET_SCENARIO_GAS_PRICES
      REAL ::  GET_SCENARIO_OIL_PRICES
      REAL ::  GET_SCENARIO_COAL_PRICES
      REAL ::  GET_SCENARIO_URANIUM_PRICES
      REAL ::  GET_MONTHLY_REGIONAL_OUTAGE
      REAL ::  NOX_CONTROL_MULT
      REAL ::  SOX_CONTROL_MULT
      REAL ::  CO2_CONTROL_MULT
      REAL ::  HG_CONTROL_MULT
      REAL ::  OTHER3_CONTROL_MULT
      REAL ::  GET_VCPMWH_IN
      REAL ::  GET_FIXED_COST_IN
      REAL ::  GET_NOX_VOM
      REAL ::  GET_NOX_FOM
      REAL ::  GET_SOX_VOM
      REAL ::  GET_SOX_FOM
      REAL ::  GET_CO2_VOM
      REAL ::  GET_CO2_FOM
      REAL ::  GET_HG_VOM
      REAL ::  GET_HG_FOM
      REAL ::  GET_OTHER3_VOM
      REAL ::  GET_OTHER3_FOM
      REAL ::  GET_MAINTENANCE_RATE
      REAL ::  TEMP_R4
      REAL ::  CL_CAPACITY_PLANNING_ADJ
      REAL ::  GET_MONTHLY_GAS_PRICE_BY_STATE
      REAL (kind=4) ::  UNSCHEDULED_MAINT_HOURS
      REAL (kind=4) ::  HH_ADJ
      REAL (kind=4) ::  MONTHLY_HH_BASIS
      REAL (kind=4) ::  UPDATE_HH_PRICE
      REAL (kind=4) ::  LOWER_48_BASIN_ADD
!
      REAL ::  DSM_PEAK_RESERVE_ALLOCATION(3,MAX_LOAD_CLASSES)
      REAL ::  LPROB_100(LOAD_CURVE_POINTS)
      REAL ::  LODDUR_100(LOAD_CURVE_POINTS)
      REAL ::  ANNUAL_BASE
      LOGICAL (kind=1) ::  DISPATCH_REPORT
      LOGICAL (kind=1) ::  ENRG_LIMIT_REPORT
      LOGICAL (kind=1) ::  LOAD_PROB_PLOT
      LOGICAL (kind=1) ::  UNIT_OUTPUT_REPORT
      LOGICAL (kind=1) ::  ANNUAL_UNIT_OUTPUT_REPORT
      LOGICAL (kind=1) ::  SPREAD_SHEET
      LOGICAL (kind=1) ::  POOLING_TRANSACTIONS
      LOGICAL (kind=1) ::  ESC_DISP_ADDER
      LOGICAL (kind=1) ::  ESCALATE_DISP_ADDER
      LOGICAL (kind=1) ::  CALL_DISPATCH_REPORT
      LOGICAL (kind=1) ::  ANNUAL_OR_SPREAD_SHEET_REPORT
      LOGICAL (kind=1) ::  USE_SECONDARY_FUEL(MAX_CL_UNITS)
      LOGICAL (kind=1) ::  WEST_KOOTENAY_POWER
      LOGICAL (kind=1) ::  WKP_ACTIVE=.false.
      LOGICAL (kind=1) ::  USE_EXTERNAL_PRODUCTION_COST
      REAL (kind=8) ::  DEMAND
      REAL (kind=8) ::  TOTAL_SALES_ENERGY
      REAL (kind=8) ::  TOTAL_SALES_REVENUE
      REAL (kind=8) ::  SALES_ENERGY
      REAL (kind=8) ::  CAP_LIMITED_DEMAND
      REAL (kind=8) ::  SALES_ENERGY_NOT_IN_FORECAST
      REAL (kind=8) ::  ENRG_LIMITED_PURCHASE_ENERGY
      REAL (kind=8) ::  DEMAND_AFTER_DSM_BEFORE_EL
      REAL (kind=8) ::  DEMAND_AFTER_DSM_AFTER_EL
      REAL (kind=8) ::  GET_DEMAND_AFTER_EL
      REAL (kind=8) ::  TRANS_EL_GENERATION
      REAL (kind=8) ::  TRANS_DEMAND
      REAL (kind=8) ::  GET_TOTAL_SALES_ENERGY
      REAL (kind=8) ::  GET_TOTAL_SALES_REVENUE
      REAL (kind=8) ::  GET_SALES_ENRGY_NOT_IN_FORECAST
      REAL (kind=8) ::  GET_ENRG_LIMITED_PURCHASE_ENRGY
      REAL ::  GET_ENERGY_EXCHANGE_ADJUSTMENT,TRANS_EL_CAPACITY
!
! COMMON /PROCSAVE/
!
      REAL (kind=8) ::  GET_FUEL_COST,GET_VAR_COST

      INTEGER (kind=2) ::  PERIOD_COUNTER,CALCULATION_ORDER(12)
!
!
! ADDED 5/16/92 FOR POOLING COST TRANSACITONS
!
      INTEGER (kind=1) ::  L
      LOGICAL (kind=4) ::  AREA_CONTROL_FILE_EXISTS
      LOGICAL (kind=4) ::  TEST_PRODP_FILE_EXISTS
      LOGICAL (kind=1) ::  BTU_TAX_ALREADY_ACTIVE
      LOGICAL (kind=1) ::  BTU_TAX_ACTIVE_THIS_MONTH(12)
      LOGICAL (kind=1) ::  BTU_TAX_ACTIVE_DISP_RPT
      LOGICAL (kind=1) ::  BTU_TAX_ACTIVE
      LOGICAL (kind=1) ::  DISPAT_BTU_TAX_ACTIVE
!
! ECONOMY INTERCHANGE
!
      LOGICAL (kind=1) ::  INCLUDE_IN_DISPATCH_ORDER
      INTEGER (kind=1) ::  BTU_TAX_START_MONTH
      CHARACTER (len=1) ::  POOL_PRICING_SWITCH
      REAL ::  TRANSACTION_BUY_SPREAD,TRANSACTION_SELL_SPREAD
      REAL ::  POOL_NATIVE_MULT
      REAL ::  POOL_OTHER_MULT
      REAL ::  UNSERVED_ENERGY_PRICE
      REAL ::  CAP_LIMT_BTU_TAX_RATES_GOCN12(0:6)
      REAL ::  ENRG_LIMT_ENRGY_TAX_RATES_H12(0:3)
      REAL ::  BTU_TAX_IN_ADJUSTMENT_CLAUSE
      REAL ::  POOLING_VARIABLE_COST_SWITCH
      REAL ::  POOLING_FUEL_COST_SWITCH
      REAL ::  POOLING_FIXED_COST_SWITCH
      REAL ::  TIE_GROUP_LIMIT(3)
!
      REAL ::  DISP_BTU_TAX_ADDER(MAX_CL_UNITS)
      REAL (kind=8) ::  ANNUAL_BTUS_GOCN12(0:6)
      REAL (kind=8) ::  ANNUAL_BTUS_FOR_BTU_TAX_GOCN12(0:6)
      REAL (kind=8) ::  ANNUAL_EL_MWH_H12(0:3)
      REAL (kind=8) ::  ANNUAL_EL_MWH_FOR_BTU_TAX_H12(0:3)
!
! KEPCO STUFF 5/20/92
!
      CHARACTER (len=1) ::  KEPCO_REPORTS,WABASH_POWER_COST_RPT
      REAL ::  CT_PATTERN_SUM(MAX_CONTRACTS)
      REAL ::  WOLF_CREEK_ENRG_COST
      REAL ::  CNTR_SALES_REVENUE,CNTR_SALES_ENERGY
      REAL ::  WEIGHTED_HOURS,PERIOD_RATE,UNIT_CAPACITY,MW_CAPACITY
!
      INTEGER (kind=2) ::    R_UNITS,RECALCULATE_FUEL_COSTS

      LOGICAL (KIND=1) :: NUC_FUEL_PRICE_SOURCE_IS_NFILE
!
! 7/20/94. GAT. SRP.
      REAL ::  TEMP_HYDRO_ENERGY,TEMP_HYDRO_CAP
!
! WABASH VALLEY STUFF January, 1994
!
      REAL ::  RESOURCE_CAP(MAX_LOAD_CLASSES)
      REAL ::  CAP_ALLOCATOR
      REAL ::  CONTROL_AREA_1_NEEDS
      REAL ::  AVAILABLE_MWS
      INTEGER (kind=2) ::  ARRAY_POINTR
      INTEGER (kind=2) ::  AREA,LAST_CLASS_IN_AREA1,I2_ZERO=0 
!
! FUEL INVENTORY VARIABLES
!
      REAL (kind=8) ::  MMBTU_FUEL_BALANCE(0:MAXIMUM_FUEL_TYPES)
      LOGICAL (kind=4) ::  FUEL_INVENTORY_ACTIVE
      INTEGER (kind=2) ::  FUEL_INVENTORY_ID(0:1024)
      INTEGER (kind=2) ::  FUEL_ID ! 041908. CHANGE.
!
!
! EMISSION FUEL BLENDING 12/30/92 MSG
!
      LOGICAL (kind=1) ::  PHASE_I_UNIT_OR_NOT_USA(MAX_CL_UNITS)
      INTEGER (kind=4) ::  ERROR
      INTEGER (kind=4) ::  DEALLOCATE_FUEL_INVENTORY_ID
      INTEGER (kind=4) ::  VALUES_TO_ZERO
      REAL ::  BLEND_SO2
      REAL ::  BLEND_NOX_BK1
      REAL ::  BLEND_NOX_BK2
      REAL ::  BLEND_CO2
      REAL ::  BLEND_OTH2
      REAL ::  BLEND_OTH3
      REAL ::  ANNUAL_BLENDED_BTU_COST(:)
      REAL ::  BLENDED_BTU_COST(:)
      REAL ::  ANNUAL_FUEL_BTU_COST(:)
      REAL ::  B_FUEL
      REAL ::  P_FUEL
      REAL ::  ANNUAL_DISP_BTU_COST(:)
!
! SERVICE TRANSACTIONS VARIABLES
!
      LOGICAL (kind=4) ::  SERVICE_TRANSACTIONS_ACTIVE
      LOGICAL (kind=1) ::  MONTHLY_SERVICE_REPORT_ACTIVE=.FALSE. 
      LOGICAL (kind=1) ::  CAP_PLANNING_METHOD_ANNDECOMP
      CHARACTER (len=2) ::  CAPACITY_PLANNING_METHOD,GREEN_MRX_METHOD
!
! FUEL PRICE VARIABLES
!
      LOGICAL (kind=1) ::  FUEL_PRICE_DATA_AVAILABLE
!
      LOGICAL (kind=1) ::    DUKE
      LOGICAL (kind=1) ::  VOID_LOGICAL
      LOGICAL (kind=1) ::  UPDATE_CATAWBA_CONTRACT_DATA
      LOGICAL (kind=1) ::  WRITE_MARGINAL_UNIT_REPORT
      LOGICAL (kind=1) ::  TEST_MONTHLY_MUST_RUN
      LOGICAL (kind=1) ::  CPL_ACTIVE
      LOGICAL (kind=1) ::  CALC_CONTRACT_CLASS_INFO
      LOGICAL (kind=1) ::  READ_ENERGY_PRODUCTS_DATA
      LOGICAL (kind=1) ::  READ_FUEL_DERIVATIVES_DATA
      LOGICAL (kind=1) ::  MONTHLY_ACTIVE_ENERGY_PRODUCTS
      LOGICAL (kind=1) ::  BEGIN_MONTH_FUEL_DERIVATIVES
      LOGICAL (kind=1) ::  MONTHLY_TRANSACTION_LOADS
      LOGICAL (kind=1) ::  CALC_CL_CAP_MARKETS
      LOGICAL (kind=1) ::  MONTHLY_USER_DATA_RESET
      LOGICAL (kind=1) ::  INIT_CAP_MARKET_REVENUE
      LOGICAL (kind=1) ::  MONTHLY_GAS_DEMAND
      LOGICAL (kind=1) ::  MONTHLY_GAS_PRICING
      LOGICAL (kind=1) ::  monthly_lp_gas_pricing
      LOGICAL (kind=1) ::  INIT_MON_MDS_CL_UNITS
      LOGICAL (kind=1) ::  TRANS_OUTAGE_DATA_ACTIVE
      LOGICAL (kind=1) ::  READ_TRANS_OUTAGE_DATA
      LOGICAL (kind=1) ::  THIS_MONTH_TRANS_OUTAGES
      LOGICAL (kind=1) ::  MONTHLY_ACTIVE_TRANS_OUTAGES
      LOGICAL (kind=1) ::  PROCESS_ANNUAL_GAS_STORAGE
      LOGICAL (kind=1) ::  ANNUAL_FUEL_DERIVATIVES
      LOGICAL (kind=1) ::  ANNUAL_ACTIVE_TRANS_OUTAGES
      LOGICAL (kind=1) ::  GasModulePipeExpansion

      INTEGER (kind=2) ::  FUEL_MIX_POINTERS
      INTEGER (kind=2) ::  UPDATE_PRIMARY_FUEL_MIX_RATIO
      INTEGER (kind=2) ::  MON_MDS_CL_EXP_2_AC

      REAL (kind=4) ::  VOIDR4
      REAL (kind=4) ::  DUKE_TOTAL_CAPACITY_CHARGES
      REAL (kind=4) ::  BUYERS_SECTION_82_ENRG_RATE
      REAL (kind=4) ::  CATAWBA_ENRG,CATAWBA_FUEL_COST,CATAWBA_OM_COST
      REAL (kind=4) ::  MCGUIRE_ENRG,MCGUIRE_FUEL_COST,MCGUIRE_OM_COST
!
! 6/22/94 ESCALATION MODIFICATIONS
!
      INTEGER (kind=2) ::  VOID_INT2
      INTEGER (kind=2) ::  ESCALATE_ALL_VALUES
      INTEGER (kind=2) ::  CL_EXPENSES_2_ASSET_CLASSES
      INTEGER (kind=2) ::  SET_UP_CL_CLASS_ARRAYS
      INTEGER (kind=2) ::  ZERO_CL_CLASS_ARRAYS
      INTEGER (kind=2) ::  ESCALATE_DISP_ADDER_VALUES
      INTEGER (kind=2) ::  ESCALATE_FUEL_ADDER_VALUES
      INTEGER (kind=2) ::  RETROFIT_ACTIVE_ID
      INTEGER (kind=2) ::  TEMP_I2
      INTEGER (kind=2) ::  ESCALATE_ONE_VALUE
!
      LOGICAL (kind=1) ::  DETAILED_MAINTENANCE_ACTIVE
      LOGICAL (kind=1) ::  CALC_ANNUAL_CAP_AND_MAINT
      LOGICAL (kind=1) ::  GET_ANNUAL_CAPACITY
      LOGICAL (kind=1) ::  UPDATE_PERIOD_CAPACITY
      LOGICAL (kind=1) ::  GET_PERIOD_CAPACITY
      LOGICAL (kind=1) ::  GET_PERIOD_MAINTENANCE
      LOGICAL (kind=1) ::  UPDATE_PERIOD_MAINTENANCE
      LOGICAL (kind=1) ::  DETAILED_MAINTENANCE_IS_ACTIVE
!
! FUEL SUPPLY INVENTORY POINTER
!
      INTEGER (kind=2) ::  GET_THIS_YEARS_FUEL_SUPPLY_ID
      INTEGER (kind=2) ::  LAST_YEARS_NUNITS
      INTEGER (kind=2) ::  THIS_YEARS_FUEL_SUPPLY_ID(MAX_CL_UNITS)
      INTEGER (kind=2) ::  R_YR
      INTEGER (kind=2) ::  R_UNIT
      INTEGER (kind=2) ::  FUEL_SUPPLY_ID_4_THIS_YEAR
!
      ALLOCATABLE :: BLENDED_BTU_COST,ANNUAL_BLENDED_BTU_COST,
     +               ANNUAL_FUEL_BTU_COST,ANNUAL_DISP_BTU_COST
      EQUIVALENCE (LPROB(1),LPROB_100(1)),(LODDUR(1),LODDUR_100(1))
!
      LOGICAL (kind=1) ::  FOUND_CLASS_FORECAST
      
      LOGICAL (kind=1) ::  CLASS_6_AND_WABASH_VALLEY
      REAL (kind=4) ::  CLASS_PEAK_AFTER_LOSSES(2,MAX_LOAD_CLASSES)
      REAL (kind=4) ::  CLASS_ENERGY_AFTER_LOSSES(2,MAX_LOAD_CLASSES)
      LOGICAL (kind=1) ::  NUC_GEN_UNIT_FOUND
      LOGICAL (kind=1) ::  SEASON_EMISSIONS_RATES=.FALSE. 
! REGIONAL ANALYSIS TRANSACTIONS VARIABLES
      INTEGER (kind=2) ::    R_BLOCK
      integer (kind=2) :: ghip
      REAL (kind=4) ::  GET_TOTAL_INCREMENTAL_COST
      REAL (kind=4) ::  INC_CAP
      REAL (kind=4) ::  PRIM_TOTAL_EMISSION_COST
      REAL (kind=4) ::  TEMP_DISPATCH_MULT
      REAL (kind=4) ::  DISP_ADJUSTER
      REAL (kind=4) ::  GET_INCREMENTAL_FUEL_COST
      REAL (kind=4) ::  A_HR
      REAL (kind=4) ::  B_HR
      REAL (kind=4) ::  SECOND_BLOCK_HEAT
      REAL (kind=4) ::  GET_TOTAL_INC_COST_PARAMS
      REAL (kind=4) ::  R_BLENDED_BTU_COST
      REAL (kind=4) ::  R_1ST_FUEL
      REAL (kind=4) ::  R_1ST_NON_FUEL
      REAL (kind=4) ::  R_2ND_FUEL
      REAL (kind=4) ::  R_2ND_NON_FUEL
!
      LOGICAL (kind=1) ::  PA_CONTRACT_ACITVE
      LOGICAL (kind=1) ::  DEALLO_MONTH_MARG_COST_ARRAYS
      LOGICAL (kind=1) ::  DEALLO_CAL_HOURLY_MC_AFTER_EL
      LOGICAL (kind=1) ::  DEALLOCATE_MONTHLY_TRANS_LOADS
      LOGICAL (kind=1) ::  DEALLOCATE_MONTHLY_GAS_DEMAND
      LOGICAL (kind=1) ::  DEALLO_ANNUAL_MARG_COSTS_ARRAYS
      CHARACTER (len=20) ::  MONTH_NAME
      LOGICAL (kind=1) ::  LAHEY_LF95,DEBUG_ON,SET_DEBUG_ON
      integer :: file_trace_procost=0

      
      if(file_trace_procost==0) then
        file_trace_procost=open_trace("procost_trace.trace", 
     + rq_pc_pt)
      end if
      
      call write_trace_message(file_trace_procost, "Entered procost")
      call write_trace_int2(file_trace_procost, "YEAR", globecom_year)



      IF(globecom_YEAR == 1) THEN
         LAST_NUNITS = -1
         LAST_YEARS_NUNITS = -1
      ENDIF
      IF(globecom_YEAR == 1 .OR. GRX_ITERATIONS == 0) THEN
         RETRO_POINTER = 0
      ENDIF
      RE_ALLOCATE_NUNITS_ARRAYS = get_nunits() > LAST_NUNITS
      IF(DEBUG_ON()) CALL write_scroll_line_RW('PROCOST',2)
      CALL UPDATE_PRODUCTION_SWITCHES
      WKP_ACTIVE = get_WEST_KOOTENAY_POWER_active()
      !wkp_active=asdf/.false.
      CALL INITIALIZE_CAWCD_ENERGY
      if(globecom_year+get_BASE_YEAR() >= 2015) then
         temp_l4 = temp_l4
      endif

      IF(get_nunits() > 0) THEN

         THIS_YEARS_FUEL_SUPPLY_ID = 0
         imm_nunits=get_nunits()
         VOID_LOGICAL = INIT_MON_MDS_CL_UNITS(imm_nunits)

      ENDIF



      SAVE_Yr = globecom_YEAR
      CURRENT_Yr = globecom_YEAR + get_BASE_YEAR()
      call write_trace_bool1(file_trace_procost, "CPL_ACTIVE", 
     + CPL_ACTIVE())
      IF(CPL_ACTIVE()) THEN
         CALL READ_CPL_DATA(globecom_YEAR)
      ENDIF
      gas_lp_active = LP_GAS_MODEL()
      ! gas_lp_active=asdf/.false.
      ANNUAL_PEAK = 0.
      PRODUCTION_COSTING_PERIODS = get_PRODUCTION_PERIODS_in()
      ESCALATE_DISP_ADDER = ESC_DISP_ADDER()
      ANNUAL_OR_SPREAD_SHEET_REPORT = ANNUAL_UNIT_OUTPUT_REPORT() .OR.
     +                                                    SPREAD_SHEET()
      MONTHLY_SERVICE_REPORT_ACTIVE = SERVICE_REPORT() /= 'F'
      CALL FUEL_PRICE_FILES_ACTIVE(FUEL_PRICE_DATA_AVAILABLE)
      NUM_UNITS_BEFORE_ADDITIONS = RETURN_CL_UNITS_B4_ADDITIONS()
! 062307.
      TEMP_L = ANNUAL_GAS_SUPPLY_CURVES(globecom_YEAR)
!
! 061209. TOOK OUT TO REFLECT DYNAMIC UPDATING OF BASIN EXTRACTION.

      TEMP_L = ANNUAL_LP_GAS_PRICING(NUM_GAS_NODES,NUM_GAS_LINKS, 
     + globecom_YEAR)
      YES_GAS_MODEL_ONLY = GAS_MODEL_ONLY()

! 012019

         VOID_LOGICAL = RPS_PROGRAMS_ANNUAL_INIT()

! Hourly Product Files. 072119.
      CALL READ_HOURLY_PRODUCT_FILES(globecom_YEAR)

      IF(globecom_YEAR == 1) THEN
         GAS_DATA_AVAILABLE = .FALSE.
         IF(ALLOCATED(ENERGY)) THEN
            DEALLOCATE(ENERGY,
     +                 TENRG,
     +                 TMMBTUS,
     +                 FUELCOST,
     +                 VARCOST)

         ENDIF
         ALLOCATE(ENERGY(MAX_SIMULATION_YEARS),
     +            TENRG(MAX_SIMULATION_YEARS),
     +            TMMBTUS(MAX_SIMULATION_YEARS),
     +            FUELCOST(MAX_SIMULATION_YEARS),
     +            VARCOST(MAX_SIMULATION_YEARS))
! 092608. ONCE PER ENDPOINT
         VOID_LOGICAL = INIT_CLA_UNIT_UP_YESTERDAY()
         FUELCOST = 0. D0
         VARCOST = 0. D0
         TENRG = 0. D0
         TMMBTUS = 0. D0
         ENERGY = 0. D0
! 07/12/04. FROM TRANSOBJ.FOR


         WEATHER_ANALYST_ACTIVE = MANAGE_WEATHER_DEMAND_FORECASTS()
! 072006.
         GAS_WEATHER_ANALYST_ACTIVE =
     +                             MANAGE_GAS_WEATHER_DEMAND_FORECASTS()
!
         TRANS_OUTAGE_DATA_ACTIVE = READ_TRANS_OUTAGE_DATA()
         YES_REGIONAL_OUTAGES_ACTIVE = REGIONAL_PARAMS_ACTIVE()

         IS_REGIONAL_HYDRO_PARAM_ACTIVE = REGIONAL_HYDRO_PARAM_ACTIVE()

         CALL SET_DISPATCH_AVERAGE_INC

         BTU_TAX_ALREADY_ACTIVE = .FALSE.
         MONTHLY_INVENTORY_ACTIVE = .FALSE. ! 6/15/93. GAT.
         CAP_PLANNING_METHOD_ANNDECOMP=CAPACITY_PLANNING_METHOD()=='AN'
         CALL INITIALIZE_EMIS_REPORTING
         VOID_INT2 = SET_UP_CL_CLASS_ARRAYS()
         CALL SET_UP_EL_CLASS_ARRAYS
         CALL DOES_CATAWBA2_FILE_EXIST(CATAWBA2_FILE_EXISTS)

         VOID_LOGICAL = READ_FUEL_DERIVATIVES_DATA()
         CALL DOES_TF_FILE_EXIST(TF_FILE_EXISTS)


         MAX_TRANS_GROUP_Nr = GET_MAX_TRANS_GROUP_NUMBER()
         IF(MAX_TRANS_GROUP_Nr > 0) THEN
            IF(ALLOCATED(NOX_SEASON_REGION))
     +                    DEALLOCATE(  NOX_SEASON_REGION,
     +                                 NOX_ACTIVE_IN_TG,
     +                                 NOX_YEAR_ACTIVE_IN_TG,
     +                                 NOX_YEAR,
     +                                 END_NOX_YEAR)
            ALLOCATE(NOX_SEASON_REGION(MAX_TRANS_GROUP_Nr),
     +                    NOX_ACTIVE_IN_TG(MAX_TRANS_GROUP_Nr),
     +                    NOX_YEAR_ACTIVE_IN_TG(MAX_TRANS_GROUP_Nr),
     +                    NOX_YEAR(MAX_TRANS_GROUP_Nr),
     +                    END_NOX_YEAR(MAX_TRANS_GROUP_Nr))
     
       call write_trace_int2(file_trace_procost, 
     + "MAX_TRANS_GROUP_NUMBER",MAX_TRANS_GROUP_Nr)
        
            DO I = 1, MAX_TRANS_GROUP_Nr
               NOX_SEASON_REGION(I) = GET_TRANS_GROUP_NOX_SEASON(I) 
               ! TRIGGERED OFF OF TG #, NOT INDEX
               
               NOX_YEAR(I) = GET_TRANS_GROUP_NOX_YEAR(I)
               END_NOX_YEAR(I) = GET_TRANS_GROUP_END_NOX_YEAR(I)
            ENDDO
            TRANS_NOX_SEASON_ACTIVE = .TRUE.
         ELSE
            TRANS_NOX_SEASON_ACTIVE = .FALSE.
         ENDIF
      ENDIF
! 010713. MOVE FROM OUTSIDE OF globecom_year=1.
      COMMIT_ON_TOTAL_COST_ENV = YES_COMMIT_ON_TOTAL_COST_ENV()

      GAS_MODEL_ACTIVE = RUN_GAS_MODEL()
      HH_ONLY = RUN_HH_ONLY()
      HH_VECTOR = GET_HH_VECTOR()

      IF(GAS_MODEL_ACTIVE) THEN
         VOID_LOGICAL = PROCESS_ANNUAL_GAS_STORAGE(globecom_YEAR)
      ENDIF


      VOID_LOGICAL = ANNUAL_FUEL_DERIVATIVES()
      
      ! globecom_year NOT PASSED 070704
      VOID_LOGICAL = ANNUAL_ACTIVE_TRANS_OUTAGES() 
      FUEL_MIX_POINTERS = UPDATE_PRIMARY_FUEL_MIX_RATIO(globecom_YEAR)
!
! SET_EMISSIONS_RATES ALLOCATES ARRAY SIZES BASED ON get_nunits()
! WHEN NEXT DECISION PLANNING IS BEING USED get_nunits() CAN CHANGE EACH
! year.  THIS REQUIRES THAT THE ARRAYS BE RESET.  THE 10/14/94 SOLUTION
!IS TO RE-RUN THE SET UP ROUTINE.  A BETTER SOLUTION IS TO INCREMENTALLY
! ADD TO THE ARRAYS.
!


! 8/5/99. GAT. MOVED FROM BELOW

      IF(globecom_YEAR == 1) THEN
         CALL INIT_EMIS_POINTR_RECORD
         CALL SET_EMISSIONS_RATES(get_nunits())
      ELSEIF(LAST_YEARS_NUNITS /= get_nunits()) THEN
         CALL SET_EMISSIONS_RATES(get_nunits())
      ENDIF


      IF(TRANS_NOX_SEASON_ACTIVE) THEN
         DO I = 1, MAX_TRANS_GROUP_Nr
         

            IF( (NOX_SEASON_REGION(I) == 'T' .AND.
     +                           CURRENT_Yr >= NOX_YEAR(I).AND.
     +                           CURRENT_Yr <= END_NOX_YEAR(I))
     +                            .OR. NOX_SEASON_REGION(I) == 'M') THEN
               NOX_ACTIVE_IN_TG(I) = .TRUE.
               NOX_YEAR_ACTIVE_IN_TG(I) = .TRUE.
            ELSE
               NOX_ACTIVE_IN_TG(I) = .FALSE.

               IF(NOX_SEASON_REGION(I) == 'T' .AND.
     +                                 CURRENT_Yr < NOX_YEAR(I) ) THEN
                  IF(CURRENT_Yr > END_NOX_YEAR(I)) THEN
                     NOX_YEAR_ACTIVE_IN_TG(I) = .TRUE.
                  ELSE
                     NOX_YEAR_ACTIVE_IN_TG(I) = .FALSE.
                  ENDIF
               ELSE
                  NOX_YEAR_ACTIVE_IN_TG(I) = .TRUE.
               ENDIF
            ENDIF
         ENDDO
      ENDIF
      CALL ZERO_EL_CLASS_ARRAYS
! TEMPORARY. GAT.
      CALL CREATE_EMISSIONS_BY_FUEL_TYPE(get_nunits())
!
      VOID_INT2 = ZERO_CL_CLASS_ARRAYS()
      BTU_TAX_ACTIVE_THIS_MONTH = .FALSE.

      BTU_TAX_ACTIVE_DISP_RPT = .FALSE.
      POOL_PRICING_SWITCH = 'P'
      POOL_NATIVE_MULT = 0.
      POOL_OTHER_MULT = 0.
      UNSERVED_ENERGY_PRICE = 0.
      TRANSACTION_BUY_SPREAD = 0. ! SET TO .25 FOR MARYLAND PSC
      TRANSACTION_SELL_SPREAD = 0.! SET TO 0. FOR MARYLAND PSC
      POOLING_VARIABLE_COST_SWITCH = 1.
      POOLING_FUEL_COST_SWITCH = 1.
      POOLING_FIXED_COST_SWITCH = 1.
      BTU_TAX_IN_ADJUSTMENT_CLAUSE = 0.
      DO I = 0, 6
         CAP_LIMT_BTU_TAX_RATES_GOCN12(I) = 0.
         ANNUAL_BTUS_GOCN12(I) = 0.D0
         ANNUAL_BTUS_FOR_BTU_TAX_GOCN12(I) = 0.D0
      ENDDO
      DO I = 0, 3
         ENRG_LIMT_ENRGY_TAX_RATES_H12(I) = 0.
         ANNUAL_EL_MWH_H12(I) = 0.D0
         ANNUAL_EL_MWH_FOR_BTU_TAX_H12(I) = 0.D0
         IF(I > 0) TIE_GROUP_LIMIT(I) = 999999.
      ENDDO
      INCLUDE_IN_DISPATCH_ORDER = .FALSE.

      IF(globecom_YEAR == 1) THEN

         FUEL_INVENTORY_ID = 0

         CALL READ_SERVICE_TRANS(MAX_CL_UNITS,
     +                                     SERVICE_TRANSACTIONS_ACTIVE)
         CALL INIT_TRANSACTION_MANAGER() ! 11/6/01.

! FUEL INVENTORY DATA

         CALL BUILD_FUEL_INVENTORIES(MMBTU_FUEL_BALANCE,
     +                               FUEL_INVENTORY_ID,
     +                               MONTHLY_INVENTORY_ACTIVE,
     +                               FUEL_INVENTORY_ACTIVE)
      ENDIF

      IF(FUEL_PRICE_DATA_AVAILABLE) THEN
         IF(globecom_YEAR == 1 .OR. get_nunits() > LAST_NUNITS .or.
     +                                      ITERATE_GRX_PLANNING()) THEN
            if(globecom_year==5) then
                globecom_year=globecom_year ! Debugstop
            end if


            imm_nunits=get_nunits()
            CALL SET_UP_FUEL_COST_INFO(imm_nunits,
     +                                 SFESCR,
     +                                 EMISS_FUEL_ESCAL)
         ENDIF
      ENDIF
      AREA_CONTROL_FILE_EXISTS = INDEX(AREA_ALLOCATORS_FILE(),'NONE')==0

      WOLF_CREEK_ENRG_COST = 0.
      KEPCO = AREA_CONTROL_FILE_EXISTS
      KEPCO_WC_DEF_MAINT_ENRG_COST = 0.
      KEPCO_WC_DEF_MAINT_ENRG_AMORT = 0.
      FIRST_MAINTENANCE_PERIOD = 13

      IF(.NOT. TESTING_PLAN .AND. WABASH_VALLEY .AND.
     +                 INDEX('M,B,A',WABASH_POWER_COST_RPT()) /= 0) THEN

         CALL IS_CLASS_6_AND_WV(CLASS_6_AND_WABASH_VALLEY)
         IF(CLASS_6_AND_WABASH_VALLEY) THEN
            NUMBER_OF_AREAS = 6
         ELSE
            NUMBER_OF_AREAS = 5 ! MAYBE RESET TO 6 FOR HOOSIER.
         ENDIF
!
         CALL WABASH_POWER_COST_REPORT(get_nunits(),NUMBER_OF_CONTRACTS,
     +                                                      HYDRO_UNITS)
      ENDIF
      IF(KEPCO) THEN
         EMERGENCY_ENERGY_CONTRACT = 0
         MAINTENANCE_ENERGY_CONTRACT = 0
         TOTAL_EMERGENCY_ENERGY = 0.
         TOTAL_MAINTENANCE_ENERGY = 0.
         TOTAL_AREA_RESOURCE_LOSSES = 0.
         CALL READ_AREA_ALLOCATORS
         CL_ANNUAL_EMERGENCY_ENERGY = 0.
         CT_ANNUAL_EMERGENCY_ENERGY = 0.
         EL_ANNUAL_EMERGENCY_ENERGY = 0.
         CL_ANNUAL_MAINTENANCE_ENERGY = 0.
         CT_ANNUAL_MAINTENANCE_ENERGY = 0.
         EL_ANNUAL_MAINTENANCE_ENERGY = 0.
         CL_ANNUAL_ENERGY_LOSSES = 0.
         CT_ANNUAL_ENERGY_LOSSES = 0.
         EL_ANNUAL_ENERGY_LOSSES = 0.
         ANNUAL_AREA_ENERGY = 0.
         ANNUAL_COINCIDENT_PEAK = 0.
         ANNUAL_NONCOINCIDENT_PEAK = 0.
         ANNUAL_AREA_SURPLUS_ENERGY = 0.
         KEPCO_ANNUAL_AREA_SALES = 0.
         KEPCO_ANNUAL_DUMP_ENERGY = 0.
         CL_RESOURCE_ANNUAL_AREA_ENRG = 0.
         CT_RESOURCE_ANNUAL_AREA_ENRG = 0.
         EL_RESOURCE_ANNUAL_AREA_ENRG = 0.
         CL_RESOURCE_ANNUAL_AREA_CAP = 0.
         CT_RESOURCE_ANNUAL_AREA_CAP = 0.
         EL_RESOURCE_ANNUAL_AREA_CAP = 0.

         CALL INIT_WC_DEF_MAINTENANCE(globecom_YEAR)
      ENDIF
      IF(TEST_PRODP_FILE_EXISTS()) THEN
         VOID_LOGICAL = READ_PRODUCTION_PARAMETERS(SAVE_Yr,
     +                              POOL_PRICING_SWITCH,
     +                              POOL_NATIVE_MULT,POOL_OTHER_MULT,
     +                              TRANSACTION_BUY_SPREAD,
     +                              TRANSACTION_SELL_SPREAD,
     +                              CAP_LIMT_BTU_TAX_RATES_GOCN12,
     +                              UNSERVED_ENERGY_PRICE,
     +                              POOLING_VARIABLE_COST_SWITCH,
     +                              INCLUDE_IN_DISPATCH_ORDER,
     +                              POOLING_FUEL_COST_SWITCH,
     +                              POOLING_FIXED_COST_SWITCH,
     +                              ENRG_LIMT_ENRGY_TAX_RATES_H12,
     +                              BTU_TAX_START_MONTH,
     +                              BTU_TAX_IN_ADJUSTMENT_CLAUSE,
     +                              TIE_GROUP_LIMIT)


         IF(BTU_TAX_START_MONTH > 0 .AND.
     +                                .NOT. BTU_TAX_ALREADY_ACTIVE) THEN
            DO MO = BTU_TAX_START_MONTH, 12
               BTU_TAX_ACTIVE_THIS_MONTH(MO) = .TRUE.
            ENDDO
            BTU_TAX_ALREADY_ACTIVE = .TRUE.
            BTU_TAX_ACTIVE_DISP_RPT = .TRUE.
         ELSE IF(BTU_TAX_START_MONTH < 0 .AND.
     +          (BTU_TAX_ALREADY_ACTIVE .OR. globecom_YEAR == 1)) THEN
            DO MO = 1, ABS(BTU_TAX_START_MONTH)
               BTU_TAX_ACTIVE_THIS_MONTH(MO) = .TRUE.
            ENDDO
            BTU_TAX_ALREADY_ACTIVE = .FALSE.
            BTU_TAX_ACTIVE_DISP_RPT = .TRUE.
         ELSE IF(BTU_TAX_ALREADY_ACTIVE) THEN
            DO MO = 1, 12
               BTU_TAX_ACTIVE_THIS_MONTH(MO) = .TRUE.
            ENDDO
            BTU_TAX_ACTIVE_DISP_RPT = .TRUE.
         ENDIF
      ENDIF

      ELCPTS = CONVOLUTION_POINTS
      MINLOD = 0.1
      BASE_DATE = (CURRENT_Yr - 1900) * 100

      REG_VARIABLE_FOSSIL_COSTS = 0.
      ENRG_LIMITED_ENRG = 0. D0
      ENRG_LIMITED_CAPACITY = 0
      ENRG_LIMITED_FIXED_COST(1:4) = 0. D0

      STORAGE_ENRG = 0. D0
      SYSCAP = 0
      TOTAL_SALES_ENERGY = 0. D0
      TOTAL_SALES_REVENUE = 0. D0
      SALES_ENERGY_NOT_IN_FORECAST = 0. D0
      ENRG_LIMITED_PURCHASE_ENERGY = 0. D0
      FIXED_OM_COSTS = 0. D0
      TOTAL_NUCLEAR_FUEL_EXPENSE = 0. D0
      PURCHASE_ENERGY = 0. D0
      PURCHASE_COSTS = 0. D0
      ENERGY_EXCHANGE_ADJUSTMENT = 0.
      CAP_LIMITED_DEMAND = 0. D0
      ANNUAL_ECONOMY_BOUGHT = 0.
      ANNUAL_ECONOMY_SOLD = 0.
      ANNUAL_ECONOMY_COST = 0.
      ANNUAL_ECONOMY_REVENUE = 0.
      ANNUAL_BASE = 9999999.
      ANN_DYN_STORAGE_GEN_ENRG = 0.
      ANN_DYN_STORAGE_PUMP_ENRG = 0.

!
! INITIALIZE ANNUAL SERVICE COSTS VARIABLES
!
      IF(MONTHLY_SERVICE_REPORT_ACTIVE) then 
      	CALL INIT_TRANS_ANNUAL_COSTS
      end if
!



      DO I = 1 , 2
         CLASS_ASSIGNED_ENERGY(I) = 0.
         CLASS_ASS_FOSSIL_COST(I) = 0.
         CLASS_ASS_PURCHASE_COST(I) = 0.
         CLASS_ASS_NUCLEAR_COST(I) = 0.
         CLASS_ASSIGNED_VARIABLE_COST(I) = 0.
         CLASS_ASSIGNED_FIXED_COST(I) = 0.
         CLASS_ASSIGNED_MMBTUS(I) = 0.
         DO EM = 1, NUMBER_OF_EMISSION_TYPES
            CLASS_ASSIGNED_EMISS(EM,I) = 0.
         ENDDO
      ENDDO
      DO I = 1, 4
         ENRG_LIMITED_PURCHASE_EXPENSE(I) = 0.
         ENRG_LIMITED_SALES_REVENUE(I) = 0.
         ENRG_LIMITED_VAR_COST(I) = 0.
         OWNED_NUCLEAR_FUEL_EXPENSE(I) = 0.
         LEASED_NUCLEAR_FUEL_EXPENSE(I) = 0.
         FOSSIL_FUEL_COSTS(I) = 0.
         PURCHASED_POWER_COSTS(I) = 0.
      ENDDO

      ANNUAL_EMIS = 0.
      CALL INIT_EMISSION_REPORT_GROUPS(get_nunits(),BASE_DATE,
     +  ONLINE,OFLINE)
      DO I = 0, MAX_REPORTING_GROUPS
         GROUP_ENERGY(I) = 0. D0
         GROUP_CAPACITY(I) = 0.
         ANN_EL_CAPACITY(I) = 0.
         ANN_CL_CAPACITY(I) = 0.
         GROUP_MMBTUS(I) = 0. D0
         GROUP_FUEL(I) = 0. D0
         GROUP_VAROM(I) = 0. D0
         GROUP_FIXED_OM(I) = 0. D0
         GROUP_SALES(I) = 0. D0
         GROUP_SALES_REVENUE(I) = 0. D0
         DO EM = 1, NUMBER_OF_EMISSION_TYPES
            GROUP_EMISSIONS(EM,I) = 0.
         ENDDO
      ENDDO
      NBLOK = 0

! NEED TO LOAD THE FUEL BALANCES


      IF(FUEL_INVENTORY_ACTIVE) THEN
         PSMO = 1
         PEMO = 1
         IF(PRODUCTION_COSTING_PERIODS == 1) THEN
            PEMO = 12

         ENDIF
         CALL GET_FUEL_INVENTORIES(MMBTU_FUEL_BALANCE,
     + globecom_YEAR,INT2(1),
     + PSMO,PEMO)
      ENDIF
      IF(get_nunits() > LAST_NUNITS) THEN

         IF(ALLOCATED(BLENDED_BTU_COST)) THEN
            DEALLOCATE(BLENDED_BTU_COST,STAT=ERROR)
         ENDIF
         ALLOCATE(BLENDED_BTU_COST(MAX(get_nunits(),1)))
         IF(ALLOCATED(ANNUAL_BLENDED_BTU_COST)) THEN
            DEALLOCATE(ANNUAL_BLENDED_BTU_COST,STAT=ERROR)
            DEALLOCATE(ANNUAL_FUEL_BTU_COST,STAT=ERROR)
            DEALLOCATE(ANNUAL_DISP_BTU_COST,STAT=ERROR)
         ENDIF

         ALLOCATE(ANNUAL_BLENDED_BTU_COST(MAX(get_nunits(),1)))
         ALLOCATE(ANNUAL_FUEL_BTU_COST(MAX(get_nunits(),1)))
         ALLOCATE(ANNUAL_DISP_BTU_COST(MAX(get_nunits(),1)))
      ENDIF
      BLENDED_BTU_COST = 0.
      ANNUAL_BLENDED_BTU_COST = 0.
      ANNUAL_FUEL_BTU_COST = 0.
      ANNUAL_DISP_BTU_COST = 0.

      GIBSON_POINTER = 0
      GIBSON_BACKUP_POINTER = 0
      PSI_AREA_LAST_RESOURCE_POINTER = 0
      MARKET_PURCHASE_POINTER = 0

!
! SET ALL PRICES
!
      VOID_INT2 = ESCALATE_ALL_VALUES(PBTUCT,PFESCR,get_nunits(),UNITNM)
      VOID_INT2 = ESCALATE_ALL_VALUES(SBTUCT,SFESCR,get_nunits(),UNITNM)
!


      VOID_INT2 = ESCALATE_ALL_VALUES(EMISS_FUEL_COST,
     +    EMISS_FUEL_ESCAL,get_nunits(),UNITNM)
!

      VOID_INT2 = ESCALATE_ALL_VALUES(CL_AI_ENERGY_RATE,
     +  CL_AI_ENERGY_ESCALATOR,get_nunits(),UNITNM)
      VOID_INT2 = ESCALATE_ALL_VALUES(CL_AI_CAPACITY_RATE,
     +  CL_AI_CAPACITY_ESCALATOR,get_nunits(),UNITNM)
!
! ADDED FOR STEININGER. 5/19/97. GAT.


      VOID_INT2 = ESCALATE_ALL_VALUES(ANNUAL_CL_FIXED_COST,
     +                ANNUAL_CL_FIXED_COST_ESC,get_nunits(),UNITNM)
      VOID_INT2 = ESCALATE_FUEL_ADDER_VALUES(SAVE_Yr,
     +                     FUELADJ,PFESCR,get_nunits(),UNITNM)


      IF(ESCALATE_DISP_ADDER) THEN
         VOID_INT2 = ESCALATE_DISP_ADDER_VALUES(SAVE_Yr,
     +                          DISPADJ,DISPADJ2,
     +                       PFESCR,get_nunits(),UNITNM)
      ENDIF
      
      call write_trace_int2(file_trace_procost, 
     + "PRODUCTION_COSTING_PERIODS", PRODUCTION_COSTING_PERIODS)
     
      IF(PRODUCTION_COSTING_PERIODS == 1) THEN
         DETAILED_MAINTENANCE_ACTIVE = DETAILED_MAINTENANCE_IS_ACTIVE()
         
         call write_trace_bool(file_trace_procost, 
     + "DETAILED_MAINTENANCE_ACTIVE changed to", 
     + logical(DETAILED_MAINTENANCE_IS_ACTIVE()))
     
      ELSE
      

      call write_trace_message(file_trace_procost, 
     + "calling calc_annual_cap_and_maint")
      call write_trace_int2(file_trace_procost, 
     + "YEAR", globecom_year)


     
         DETAILED_MAINTENANCE_ACTIVE = 
     +      CALC_ANNUAL_CAP_AND_MAINT(globecom_YEAR)
         IF(DETAILED_MAINTENANCE_ACTIVE)
     +            GET_ANNUAL_CAPACITY =
     +           UPDATE_PERIOD_CAPACITY(get_nunits(),INT2(1))

      ENDIF

      ECO_SALES_REV_FROM = 0.
      ECO_SALES_ENRG_FROM = 0.
      ECO_PUCH_COST_FROM = 0.
      ECO_PUCH_ENRG_FROM = 0.
      DATE1 = BASE_DATE + 1
      DATE2 = BASE_DATE + 12

      IF(globecom_YEAR == 1 .AND. GRX_ITERATIONS == 0) then

        COAL_MODEL_RAN_LAST_ITERATION = .FALSE.
      end if
      IF(COAL_MODEL_RAN_LAST_ITERATION) THEN
         CALL SET_SCRUBBERS_FROM_COAL_LP(CURRENT_Yr)
      ENDIF
      DO I = 1, get_nunits()
!
! ADDED 8/9/99. GAT. FOR CPL.
!
         RTEMP = GET_CL_POOL_FRAC_OWN(I)
         IF(RTEMP >= 0.) THEN
            CL_POOL_FRAC_OWN(I) = RTEMP
         ELSE
            CL_POOL_FRAC_OWN(I) = GET_VAR(RTEMP,globecom_YEAR,UNITNM(I))
         ENDIF
!
         USE_SECONDARY_FUEL(I) = .FALSE.

! EMISSIONS BLENDING ADDED 12/30/92


         IF(EMISS_BLENDING_RATE(I) > 0.) THEN
            ANNUAL_BLENDED_BTU_COST(I) =
     +               (1.-EMISS_BLENDING_RATE(I))*PBTUCT(I)+
     +                       EMISS_BLENDING_RATE(I) * EMISS_FUEL_COST(I)
         ELSE
            ANNUAL_BLENDED_BTU_COST(I) = PBTUCT(I)
         ENDIF
         IF(ABS(FUELMX(I)) /= 1.) THEN
            ANNUAL_FUEL_BTU_COST(I) = ABS(FUELMX(I)) *
     +               ANNUAL_BLENDED_BTU_COST(I) +
     +                                (1. - ABS(FUELMX(I))) * SBTUCT(I)
         ELSE
            ANNUAL_FUEL_BTU_COST(I) = ANNUAL_BLENDED_BTU_COST(I)
         ENDIF
         DISP_BTU_TAX_ADDER(I) = 0.


         FUEL_SUPPLY_ID_4_THIS_YEAR = FUEL_SUPPLY_ID(I)
         IF(FUEL_SUPPLY_ID_4_THIS_YEAR < 0) THEN
            FUEL_SUPPLY_ID_4_THIS_YEAR =
     +         GET_VAR(FLOAT(
     +      FUEL_SUPPLY_ID_4_THIS_YEAR),globecom_YEAR,'FUEL SUPPLY ID')
         ENDIF
         THIS_YEARS_FUEL_SUPPLY_ID(I) = FUEL_SUPPLY_ID_4_THIS_YEAR

         DO_IT = .FALSE.
         IF(FUEL_INVENTORY_ACTIVE) THEN
            IF(THIS_YEARS_FUEL_SUPPLY_ID(I) /= 0) THEN
               IF(FUEL_INVENTORY_ID(
     +                          THIS_YEARS_FUEL_SUPPLY_ID(I)) /= 0) THEN
                  DO_IT = MMBTU_FUEL_BALANCE(
     +               FUEL_INVENTORY_ID(
     +                              THIS_YEARS_FUEL_SUPPLY_ID(I))) == 0.
               ELSE
                  WRITE(4,*) trim(UNITNM(I)),
     +                      ' references fuel inventory number',
     +                       THIS_YEARS_FUEL_SUPPLY_ID(I)
                  WRITE(4,*) 'That fuel ID number was not '//
     +                       'found in the fuel inventory file.'
                  THIS_YEARS_FUEL_SUPPLY_ID(I) = 0
               ENDIF
            ENDIF
         ENDIF
         IF(FUELMX(I) < 0. .OR. DO_IT) THEN
            ANNUAL_DISP_BTU_COST(I) = SBTUCT(I)
            IF(INCLUDE_IN_DISPATCH_ORDER) THEN
               L = INDEX(CAP_LIMIT_FUEL_TYPES,SEC_FUEL_TYPE(I))
               DISP_BTU_TAX_ADDER(I)=CAP_LIMT_BTU_TAX_RATES_GOCN12(L)
            ENDIF
         ELSE
            ANNUAL_DISP_BTU_COST(I) = ANNUAL_FUEL_BTU_COST(I)
            IF(INCLUDE_IN_DISPATCH_ORDER) THEN
               L = INDEX(CAP_LIMIT_FUEL_TYPES,PRIM_FUEL_TYPE(I))
               IF(ABS(FUELMX(I)) /= 1.) THEN
                  DISP_BTU_TAX_ADDER(I) = ABS(FUELMX(I))*
     +                                  CAP_LIMT_BTU_TAX_RATES_GOCN12(L)
                  L = INDEX(CAP_LIMIT_FUEL_TYPES,SEC_FUEL_TYPE(I))
                  DISP_BTU_TAX_ADDER(I) =  DISP_BTU_TAX_ADDER(I) +
     +                              (1. - ABS(FUELMX(I))) *
     +                                  CAP_LIMT_BTU_TAX_RATES_GOCN12(L)
               ELSE
                  DISP_BTU_TAX_ADDER(I) =
     +                                  CAP_LIMT_BTU_TAX_RATES_GOCN12(L)
               ENDIF
            ENDIF
         ENDIF


! EMISSION PARAMETERS

         IF(I == 1) then
            CALL CREATE_EMISS_BY_UNIT_BY_FUEL(get_nunits())
         end if
         PHASE_I_UNIT_OR_NOT_USA(I) = PHASE_I_UNIT(I) .OR.
     +                      CURRENT_Yr  > 1999 .OR. COUNTRY() /= 'U'

! SET TO TRUE 6/11/98. GAT.

         SEASON_EMISSIONS_RATES = .TRUE.
!
         ! Big IF block that depends on SEASON_EMISSIONS_RATE==FALSE
         ! removed because it can never be called.

!        CAPACITY LIMITED POINTER LOGIC

         IF(.NOT. DETAILED_MAINTENANCE_ACTIVE .OR.
     +                             PRODUCTION_COSTING_PERIODS == 1) THEN

            DO M = 2, 1 , -1
               IF(INPUT_MW(M,I) < 0.) THEN
            MW(M,I) = GET_VAR(INPUT_MW(M,I),globecom_YEAR,UNITNM(I))
            call write_mw_pc_trace("pro_cost:0004", MW(M,I))
            
            IF(MW(M,I) < 0.) THEN
               WRITE(4,*) "Negative value for capacity"
               WRITE(4,*) "for unit ",UNITNM(I)
               WRITE(4,*) " "
            ENDIF
               ELSE
                  MW(M,I) = INPUT_MW(M,I)
                  call write_mw_pc_trace("pro_cost:0005", MW(M,I))
               ENDIF
               IF(MW(M,I) < 1. .AND. M == 1) THEN
                  MW(1,I) = MW(2,I) * MW(1,I)
                  call write_mw_pc_trace("pro_cost:0006", MW(1,I))
               ELSE
                  ! Merge note: Replaced invalid accessor.
                  MW(M,I) = MW(M,I) * foshyd%CAP_FRAC_OWN(I)/100.
                  call write_mw_pc_trace("pro_cost:0007", MW(M,I))

               ENDIF
            ENDDO
         ENDIF
         CALL HR_CALC(I)

         ANNUAL_CL_UNIT_CAPACITY(I) = 0.
         ANNUAL_CL_UNIT_ENERGY(I) = 0.
         ANNUAL_CL_UNIT_FUEL_COST(I) = 0.
         ANNUAL_CL_UNIT_VAR_COST(I) = 0.
         ANNUAL_CL_UNIT_MMBTUS(I) = 0. D0
         ANNUAL_CL_UNIT_FIXED_COST(I) = 0.
         ANNUAL_NUC_UNIT_FUEL_ADDER_COST(I) = 0.
         CL_AI_INVESTMENT(I) = 0.
         DO EM = 1, 5
            ANNUAL_CL_UNIT_EMISSIONS(EM,I) = 0.
         ENDDO
      ENDDO

      VOID_INT2 = ESCALATE_ALL_VALUES(VAROM,VOMESR,HYDRO_UNITS,
     +                                                     EL_UNIT_NAME)
      VOID_INT2 = ESCALATE_ALL_VALUES(HYDRO_FIXED_COST,
     +              HYDRO_FIXED_COST_ESCALATOR,HYDRO_UNITS,EL_UNIT_NAME)
      VOID_INT2 = ESCALATE_ALL_VALUES(HYDRO_ANNUAL_FIXED_COST,
     +             HYDRO_ANNUAL_FIXED_COST_ESC,HYDRO_UNITS,EL_UNIT_NAME)
      VOID_INT2 = ESCALATE_ALL_VALUES(EL_AI_ENERGY_RATE,
     +                  EL_AI_ENERGY_ESCALATOR,HYDRO_UNITS,EL_UNIT_NAME)
      VOID_INT2 = ESCALATE_ALL_VALUES(EL_AI_CAPACITY_RATE,
     +                EL_AI_CAPACITY_ESCALATOR,HYDRO_UNITS,EL_UNIT_NAME)
      EL_AI_INVESTMENT = 0.
      EL_SO2_ANNUAL = 0.
      IF(CURRENT_Yr > 1999) THEN
         DO I = 1, HYDRO_UNITS
            IF(EL_SO2_RATE(I) < 0.) THEN
               EL_SO2(I) = GET_VAR(EL_SO2_RATE(I),globecom_YEAR,
     +                        'EL SO2 rates for unit '//EL_UNIT_NAME(I))
            ELSE
               EL_SO2(I) = EL_SO2_RATE(I)
            ENDIF
         ENDDO
      ELSE
         EL_SO2 = 0.
      ENDIF

      CT_SO2_ANNUAL = 0.

      DO I = 1 , NUMBER_OF_CONTRACTS
         ANNUAL_CONTRACT_CAPACITY(I) = 0.0
         ANNUAL_CONTRACT_ENERGY(I) = 0.0
         ANNUAL_CONTRACT_VARIABLE_COST(I) = 0.0
         ANNUAL_CONTRACT_FIXED_COST(I) = 0.0
         ANNUAL_CONTRACT_SO2(I) = 0.0
         CNTR_WEIGHTED_CAPACITY(I) = 0.0
         CT_PATTERN_SUM(I) = 0.
         IF(CT_ENERGY_PATTERN_VECTOR(I) /= 0) THEN
            CT_PATTERN_SUM(I) =
     +                  GET_MONTHLY_VAR_SUM(CT_ENERGY_PATTERN_VECTOR(I))
         ENDIF
      ENDDO
      DO PERIOD_COUNTER = 1, 12
         CALCULATION_ORDER(PERIOD_COUNTER) = PERIOD_COUNTER
      ENDDO

! INITIALIZE SERVICE COSTS VARIABLES

C INITIALIZE SERVICE COSTS VARIABLES


      IF(SERVICE_TRANSACTIONS_ACTIVE) THEN
         CALL INITIALIZE_SERVICE_VARS(globecom_YEAR)
      ELSE
         TRANSMISSION_CHARGES = 0.
         WHEELING_CHARGES = 0.
         STAND_BY_TRANS_CHARGES= 0.
         DISPATCHING_CHARGES = 0.
         OTHER_SERVICE_CHARGES = 0.
         SERVICE_REVENUES = 0.
         SERVICE_EXPENSES = 0.
         SERVICE_ADJ_CLAUSE_EXPENSES = 0.
         SERVICE_BASE_RATE_EXPENSES = 0.
      ENDIF

      IF(USE_EXTERNAL_PRODUCTION_COST()) RETURN

      UNIT_COMMITMENT_LOGIC = YES_UNIT_COMMITMENT_LOGIC()



      IF(CPL_ACTIVE()) THEN
         CALL INIT_CPL_EMC_ANN_CAP
         CALL CPL_CALC_RESERVE_PERCENT
      ENDIF

      DO PERIOD_COUNTER = 1, PRODUCTION_COSTING_PERIODS
         CALL CLEAR_EMISSIONS_BY_FUEL_TYPE
         ISEAS = CALCULATION_ORDER(PERIOD_COUNTER)

         CALL STORE_SERVICE_MONTH(ISEAS)
         MON_ECO_SALES_REV_FROM = 0.
         MON_ECO_SALES_ENRG_FROM = 0.
         MON_ECO_PUCH_COST_FROM = 0.
         MON_ECO_PUCH_ENRG_FROM = 0.
         MONTHLY_ICAP_REVENUES = 0.

         IF(TRANS_OUTAGE_DATA_ACTIVE .OR. UNIT_COMMITMENT_LOGIC) THEN
            THIS_MONTH_TRANS_OUTAGES =
     +       MONTHLY_ACTIVE_TRANS_OUTAGES(ISEAS,get_nunits())
         ELSE
            THIS_MONTH_TRANS_OUTAGES = .FALSE.
         ENDIF

         TEMP_L = UPDATE_STATUS_REPORT(ISEAS)


         NUCLEAR_AVAIL_MULT = GET_SCENARIO_NUCLEAR_AVAIL(globecom_year,
     + ISEAS)
         COAL_AVAIL_MULT = GET_SCENARIO_COAL_AVAIL(globecom_year,ISEAS)
         GAS_AVAIL_MULT = GET_SCENARIO_GAS_AVAIL(globecom_year,ISEAS)
         OIL_AVAIL_MULT = GET_SCENARIO_OIL_AVAIL(globecom_year,ISEAS)
       OTHER_AVAIL_MULT = GET_SCENARIO_OTHER_AVAIL(globecom_year,ISEAS)
         HYDRO_WATER_YEAR_MULT =
     +               GET_SCENARIO_HYDRO_WATER_YEAR(globecom_year,ISEAS)

! 5/1/99. GAT. FOR SCENARIO MAKER.

       FUEL_SCEN_MULT(1) =  
     + GET_SCENARIO_COAL_PRICES(globecom_year,ISEAS)
       FUEL_SCEN_MULT(2) =  GET_SCENARIO_GAS_PRICES(globecom_year,ISEAS)
       FUEL_SCEN_MULT(3) =  GET_SCENARIO_OIL_PRICES(globecom_year,ISEAS)
       FUEL_SCEN_MULT(4) =  
     + GET_SCENARIO_URANIUM_PRICES(globecom_year,ISEAS)
       FUEL_SCEN_MULT(5) = 1.


         CALL UPDATE_SEASON_FOR_REPORTS(ISEAS)
         EL_SO2_PERIOD(ISEAS) = 0.
         IF(PRODUCTION_COSTING_PERIODS == 1) THEN
            PSMO = 1
            PEMO = 12
         ELSE
            PSMO = ISEAS
            PEMO = ISEAS

         ENDIF
         BTU_TAX_ACTIVE = .FALSE.
         HOURS_INCREMENT = 0

! 10/29/02. MOVED UP
! 080406. THIS CALL ALLOCATES AND DEALLOCATES EVERY MONTH. COULD
!  BE SET AT BEGINNING OF EVERY globecom_year TO CUT DOWN ON DEALLOCATES

       CALL  CALC_TOTAL_DELIVERY_COSTS(get_nunits(),ISEAS,globecom_year,
     +    UNITNM)

         VOID_LOGICAL = MONTHLY_USER_DATA_RESET()
         VOID_LOGICAL = ! MOVED 2/1/99. GAT.
     +              MONTHLY_ACTIVE_ENERGY_PRODUCTS(ISEAS,CURRENT_Yr)
         VOID_LOGICAL = ! MOVED 2/1/99. GAT.
     +              BEGIN_MONTH_FUEL_DERIVATIVES(ISEAS,CURRENT_Yr)
       
       ghip=get_HOURS_IN_PERIOD(iseas)

     

         VOID_LOGICAL =  MONTHLY_TRANSACTION_LOADS(globecom_year,ISEAS,
     + ghip)
     
         VOID_LOGICAL = INIT_CAP_MARKET_REVENUE()



         VOID_LOGICAL =  CALC_CL_CAP_MARKETS(globecom_YEAR,ISEAS)
!
! 031507.

         TEMP_R4 = INIT_MONTHLY_LDC_2_HOURS(get_HOURS_IN_PERIOD(ISEAS))

! 072706.
!
         VOID_LOGICAL =
     +      MONTHLY_GAS_DEMAND(globecom_YEAR,ISEAS, 
     + get_HOURS_IN_PERIOD(ISEAS))

! 080806.
! 062107.
! 051109. PUT BACK IN FOR COST ESCALATION OF SUPPLY CURVES
!         AND RESERVE APPRECIATION.

         VOID_LOGICAL =
     +      MONTHLY_GAS_SUPPLY_CURVES(ISEAS,globecom_year)


         DO I = PSMO, PEMO
            HOURS_INCREMENT = HOURS_INCREMENT + get_HOURS_IN_PERIOD(I)
            BTU_TAX_ACTIVE = BTU_TAX_ACTIVE .OR.
     +                                      BTU_TAX_ACTIVE_THIS_MONTH(I)
         ENDDO

! CHECK FOR UPDATING SERVICE COSTS

         IF(SERVICE_TRANSACTIONS_ACTIVE) THEN
            CALL SERVICE_UPDATE_COSTS(ISEAS,globecom_YEAR)
            IF(MONTHLY_SERVICE_REPORT_ACTIVE) then
                CALL INIT_TRANS_SEASON_TYPE
            end if


         ENDIF
!
! INITIALIZE FUEL ANNUAL FUEL INVENTORIES ADDED 6/18/92
! MODIFIED 12/31/92, 1/2-3/93 FOR MONTHLY INVENTORIES
! 6/15/93. GAT. MOVED ABOVE UNITS LOOP TO TRAP FOR USE_SEC_FUEL
!


         IF(FUEL_INVENTORY_ACTIVE .AND. PERIOD_COUNTER > 1) THEN
            CALL GET_FUEL_INVENTORIES(MMBTU_FUEL_BALANCE,globecom_YEAR,
     +                                PERIOD_COUNTER,PSMO,PEMO)
         ENDIF


         IF(DETAILED_MAINTENANCE_ACTIVE .AND.
     +                            PRODUCTION_COSTING_PERIODS == 12) THEN
        GET_PERIOD_MAINTENANCE = UPDATE_PERIOD_MAINTENANCE(get_nunits(),
     +                                            PEMO,MAINTENANCE_RATE)
       GET_PERIOD_CAPACITY = UPDATE_PERIOD_CAPACITY(get_nunits(),PEMO)
         ENDIF

!
! ADDED FOR JONES. 11/19/97. GAT.


         IF(ISEAS > 4 .AND. ISEAS < 10 ) THEN
            SEASON_IS_NOX_SEASON = .TRUE.
         ELSE
            SEASON_IS_NOX_SEASON = .FALSE.
         ENDIF

         DO I = 1, get_nunits()
            ! Mergissue_pc 
            ! SEASON_EMISSIONS_RATES=.true./.true.
            IF(SEASON_EMISSIONS_RATES) THEN
               PHASE_I_UNIT_OR_NOT_USA(I) = PHASE_I_UNIT(I) .OR.
     +                      CURRENT_Yr  > 1999 .OR. COUNTRY() /= 'U'
               i_for_argument=i
               TG = get_TRANSACTION_GROUP(i_for_argument)
               IF(TRANS_NOX_SEASON_ACTIVE) THEN
                  IF(TG > 0 .AND. TG <= MAX_TRANS_GROUP_Nr) THEN
                     IF(NOX_ACTIVE_IN_TG(TG)) THEN
                        CALCULATE_NOX = SEASON_IS_NOX_SEASON
                     ELSEIF(.NOT. NOX_YEAR_ACTIVE_IN_TG(TG)) THEN
                        CALCULATE_NOX = .FALSE.
                     ELSE
                        CALCULATE_NOX = .TRUE.
                     ENDIF
                  ELSE
                     CALCULATE_NOX = .TRUE.
                     WRITE(4,*) "TRANSACTION GROUP ",TG
                     WRITE(4,*) "IS OUTSIDE OF RANGE"
                  ENDIF
               ELSE
                  CALCULATE_NOX = .TRUE.
               ENDIF

!
! 11/12/01. THIS IS CHANGED ONLY IF TRUE.
! OTHERWISE REVERTS BACK TO PREVIOUS LOGIC.
! 03/04/04. END_NOX_YEAR RULES OVER INFORMATION

               IF(CURRENT_Yr <= END_NOX_YEAR(TG)) THEN
               i_for_argument=I
                  VOID_LOGICAL = NOX_ACTIVE_FOR_UNIT(i_for_argument,
     +                                          ISEAS+BASE_DATE,
     +                                            SEASON_IS_NOX_SEASON,
     +                                            CALCULATE_NOX)
               ENDIF
               i_for_argument=i
               NOX_CONTROL_FOR_UNIT =  GET_NOX_CONTROL_FOR_UNIT(
     +                                       i_for_argument,
     +                                       ISEAS+BASE_DATE,
     +                                       NOX_CONTROL_MULT)
!
               i_for_argument=i
               SOX_CONTROL_FOR_UNIT =  GET_SOX_CONTROL_FOR_UNIT(
     +                                       i_for_argument,
     +                                       ISEAS+BASE_DATE,
     +                                       SOX_CONTROL_MULT)

                i_for_argument=i
               CO2_CONTROL_FOR_UNIT =  GET_CO2_CONTROL_FOR_UNIT(
     +                                       i_for_argument,
     +                                       ISEAS+BASE_DATE,
     +                                       CO2_CONTROL_MULT,
     +                                       CO2_FIRST_CONTROL_MONTH,
     +                                       CO2_FIRST_CONTROL_YEAR,
     +                                       CO2_YEAR_BEFORE_CONTROL)

! 062809
               ji = i
               IF(RETROFIT_ACTIVE_ID(jI) > 0) THEN
                  TEMP_I2 = RETROFIT_ACTIVE_ID(jI)
                  CALL RETROFIT_CO2_IMPACT(TEMP_I2,TEMP_R4)
                  CO2_CONTROL_MULT = MIN(1.,1.0 - TEMP_R4/100.0)

               ENDIF
               i_for_argument=I
               HG_CONTROL_FOR_UNIT =  GET_HG_CONTROL_FOR_UNIT(
     +                                       i_for_argument,
     +                                       ISEAS+BASE_DATE,
     +                                       HG_CONTROL_MULT)

               i_for_argument=i
               OTHER3_CONTROL_FOR_UNIT =  GET_OTHER3_CONTROL_FOR_UNIT(
     +                                       i_for_argument,
     +                                       ISEAS+BASE_DATE,
     +                                       OTHER3_CONTROL_MULT)

!
        CALL GET_SEASON_EMISSIONS_RATES(ISEAS,globecom_YEAR,UNITNM(I),
     +                     PHASE_I_UNIT_OR_NOT_USA(I),
     +                     SO2(I),P_SO2(I),NOX(I),P_NOX(I),
     +                     PRIM_NOX_BK2(I),P_NOX_BK2(I),
     +                     PARTICULATES(I),P_PARTICULATES(I),
     +                     EMIS_OTH2(I),P_EMIS_OTH2(I),
     +                     EMIS_OTH3(I),P_EMIS_OTH3(I),
     +                     I,'P',CALCULATE_NOX,
     +                     NOX_CONTROL_MULT,
     +                     SOX_CONTROL_MULT,
     +                     CO2_CONTROL_MULT,
     +                     HG_CONTROL_MULT,
     +                     OTHER3_CONTROL_MULT)
               CALL STORE_FUEL_MIX(I,FUELMX(I))
      CALL GET_SEASON_EMISSIONS_RATES(ISEAS,globecom_YEAR,UNITNM(I),
     +          PHASE_I_UNIT_OR_NOT_USA(I),
     +          SEC_SO2(I),S_SO2(I),
     +          SEC_NOX_BK1(I),S_NOX_BK1(I),
     +          SEC_NOX_BK2(I),S_NOX_BK2(I),
     +          SEC_CO2(I),S_CO2(I),
     +          SEC_OTH2(I),S_OTHER2(I),
     +          SEC_OTH3(I),S_OTHER3(I),
     +          I,'S',CALCULATE_NOX,
     +          NOX_CONTROL_MULT,
     +          SOX_CONTROL_MULT,
     +          CO2_CONTROL_MULT,
     +          HG_CONTROL_MULT,
     +          OTHER3_CONTROL_MULT)
               IF(EMISS_BLENDING_RATE(I) > 0.)THEN
       CALL GET_SEASON_EMISSIONS_RATES(ISEAS,globecom_YEAR,UNITNM(I),
     +                   PHASE_I_UNIT_OR_NOT_USA(I),
     +                   BLEND_SO2,EMISS_FUEL_SO2(I),
     +                   BLEND_NOX_BK1,EMISS_FUEL_NOX_BK1(I),
     +                   BLEND_NOX_BK2,EMISS_FUEL_NOX_BK2(I),
     +                   BLEND_CO2,EMISS_FUEL_CO2(I),
     +                   BLEND_OTH2,EMISS_FUEL_OTHER2(I),
     +                   BLEND_OTH3,EMISS_FUEL_OTHER3(I),
     +                   I,'B',CALCULATE_NOX,
     +                   NOX_CONTROL_MULT,
     +                   SOX_CONTROL_MULT,
     +                   CO2_CONTROL_MULT,
     +                   HG_CONTROL_MULT,
     +                   OTHER3_CONTROL_MULT)
                  B_FUEL = EMISS_BLENDING_RATE(I)
                  CALL STORE_BLENDING_MIX(I,B_FUEL)
                  P_FUEL = 1. -  B_FUEL
                  SO2(I) = P_FUEL * SO2(I) + B_FUEL * BLEND_SO2
                  NOX(I) = P_FUEL * NOX(I) + B_FUEL * BLEND_NOX_BK1
                  PRIM_NOX_BK2(I) =
     +                       P_FUEL*PRIM_NOX_BK2(I)+B_FUEL*BLEND_NOX_BK2
                  PARTICULATES(I) =
     +                         P_FUEL*PARTICULATES(I) + B_FUEL*BLEND_CO2
                  EMIS_OTH2(I) =
     +                       P_FUEL * EMIS_OTH2(I) + B_FUEL * BLEND_OTH2
                  EMIS_OTH3(I) =
     +                       P_FUEL * EMIS_OTH3(I) + B_FUEL * BLEND_OTH3
               ENDIF

! 030207. FOR ODEC.

               IF(.NOT. ESCALATE_DISP_ADDER) THEN
                  RTEMP = GET_DISPADJ_SAVE(I)
                  IF(RTEMP < 0.) THEN
                     RTEMP = GET_VAR(RTEMP,globecom_YEAR,UNITNM(I))
                     IF(RTEMP < 0.) THEN
                        DISPADJ(I) = GET_VAR(RTEMP,ISEAS,UNITNM(I))
                     ELSE
                        DISPADJ(I) = RTEMP
                     ENDIF
                  ELSE
                     DISPADJ(I) = RTEMP
                  ENDIF
                  RTEMP = GET_DISPADJ2_SAVE(I)
                  IF(RTEMP < 0.) THEN
                     RTEMP = GET_VAR(RTEMP,globecom_YEAR,UNITNM(I))
                     IF(RTEMP < 0.) THEN
                        DISPADJ2(I) = GET_VAR(RTEMP,ISEAS,UNITNM(I))
                     ELSE
                        DISPADJ2(I) = RTEMP
                     ENDIF
                  ELSE
                     DISPADJ2(I) = RTEMP
                  ENDIF
               ENDIF

! FOR SCOTT JONES. 12/10/97. GAT.

               IF(EFOR(I) < 0.) THEN
                  RTEMP = GET_VAR(EFOR(I),globecom_YEAR,UNITNM(I))
                  IF(RTEMP < 0.) THEN
                     EAVAIL(I) =
     +                  (100. - GET_VAR(RTEMP,ISEAS,UNITNM(I)))/100.
                  ELSE
                     EAVAIL(I) = (100. - RTEMP)/100.
                  ENDIF
               ELSE
                  EAVAIL(I) = (100. - EFOR(I))/100.
               ENDIF
               IF(HR_FACTOR(I) < 0.) THEN
                  HEAT_RATE_FACTOR(I) =
     +  GET_VAR(HR_FACTOR(I),globecom_YEAR,UNITNM(I))
                  IF(HEAT_RATE_FACTOR(I) < 0.)
     +                HEAT_RATE_FACTOR(I) =
     +           GET_VAR(HEAT_RATE_FACTOR(I),ISEAS,UNITNM(I))
               ELSE
                  HEAT_RATE_FACTOR(I) = HR_FACTOR(I)
               ENDIF

            ENDIF
!
! ADDED FOR JONES. 11/6/97. GAT.

! 8/29/02. ALTERED FOR NOX DISPATCH FOR RDI.

            TEMP_UNIT_NAME = UNITNM(I)
            RTEMP = 0.0
            RTEMP_2 = 0.0
            IF(NOX_CONTROL_FOR_UNIT .AND. CALCULATE_NOX) THEN

                i_for_argument=i
               RTEMP = GET_NOX_VOM(i_for_argument)

!
               IF(RTEMP < -0.1) THEN
                  RTEMP=ABS(RTEMP)
                  RTEMP =
     +  ESCALATED_MONTHLY_VALUE(RTEMP,
     +               INT2(RTEMP),
     +               globecom_YEAR,ISEAS,INT2(1))
               ELSE
                  TEMP_I2 = ESCALATE_ONE_VALUE(RTEMP,TEMP_I2,
     +                         OMESCR(I),
     +                         TEMP_UNIT_NAME,globecom_YEAR)
               ENDIF
!

               i_for_argument=i
               RTEMP_2 = GET_NOX_FOM(i_for_argument)

!
               IF(RTEMP_2 < -0.1) THEN
                  RTEMP_2 = ABS(RTEMP_2)
                  RTEMP_2 =
     +  ESCALATED_MONTHLY_VALUE(RTEMP_2,
     +     INT2(RTEMP_2),
     +     globecom_YEAR,ISEAS,INT2(1))
               ELSE
                  TEMP_I2 = ESCALATE_ONE_VALUE(RTEMP_2,TEMP_I2,
     +                         FIXED_COST_ESCALATOR(I),
     +                         TEMP_UNIT_NAME,globecom_YEAR)
               ENDIF
            ENDIF
            RTEMP_3 = 0.0
            RTEMP_4 = 0.0
            IF(SOX_CONTROL_FOR_UNIT .AND.
     +                                  PHASE_I_UNIT_OR_NOT_USA(I)) THEN

               i_for_argument=i
               RTEMP_3 = GET_SOX_VOM(i_for_argument)

!
               IF(RTEMP_3 < -0.1) THEN
                  RTEMP_3=ABS(RTEMP_3)
                  RTEMP_3 =
     +               ESCALATED_MONTHLY_VALUE(RTEMP_3,
     +                                           INT2(RTEMP_3),
     +               globecom_YEAR,ISEAS,INT2(1))
               ELSE
                  TEMP_I2 = ESCALATE_ONE_VALUE(RTEMP_3,TEMP_I2,
     +                         OMESCR(I),
     +                         TEMP_UNIT_NAME,globecom_YEAR)
               ENDIF
!

               RTEMP_4 = GET_SOX_FOM(i_for_argument)

!
               IF(RTEMP_4 < -0.1) THEN
                  RTEMP_4 = ABS(RTEMP_4)
                  RTEMP_4 =
     +               ESCALATED_MONTHLY_VALUE(RTEMP_4,
     +                                           INT2(RTEMP_4),
     +                     globecom_YEAR,ISEAS,INT2(1))
               ELSE
                  TEMP_I2 = ESCALATE_ONE_VALUE(RTEMP_4,TEMP_I2,
     +                         FIXED_COST_ESCALATOR(I),
     +                         TEMP_UNIT_NAME,globecom_YEAR)
               ENDIF
            ENDIF

! 101309. MODIFIED THIS SECTION FOR FALL REFERENCE 2009.
!
            RTEMP_5 = 0.0
            RTEMP_6 = 0.0
            RETRO_VOM = 0.0
            RETRO_FOM = 0.0
            CO2_RETRO_HEAT_MULT = 1.0
            CO2_RETRO_CAP_MULT = 1.0

            ! Hard Wired
            IF(CO2_CONTROL_FOR_UNIT .OR. RETROFIT_ACTIVE_ID(I) > 0) THEN


! 071209.
               IF(RETROFIT_ACTIVE_ID(I) > 0) THEN
                  TEMP_I2 = RETROFIT_ACTIVE_ID(I)
                  CALL RETROFIT_VOM_ADDER(TEMP_I2,RETRO_VOM,
     + globecom_YEAR,ISEAS)
        CALL RETROFIT_FOM_ADDER(TEMP_I2,RETRO_FOM,globecom_YEAR,ISEAS)
                  RTEMP_6 = 0.
                  RTEMP_5 = 0.
               ELSE
                  RTEMP_6 = GET_CO2_FOM(I)
                  RTEMP_5 = GET_CO2_VOM(I)
               ENDIF
               IF(CAPACITY_PLANNING_METHOD() == 'MX'  .AND.
     +                                  GREEN_MRX_METHOD() == 'GX') THEN
                  IF(HardWiredRetrofitProject(I)) THEN
                     IF(HWRetrofitProjectThisYear(I) .AND.
     +                                         GRX_ITERATIONS == 0) THEN

!                      Add writing this report to ghe GRX output

                     ENDIF
                  ENDIF
               ENDIF
               CO2_RETRO_HEAT_MULT = GET_CO2_RETRO_HEAT_MULT(I)
               CO2_RETRO_CAP_MULT = GET_CO2_RETRO_CAP_MULT(I)

!

               IF(RTEMP_5 < -0.1) THEN
                  RTEMP_5=ABS(RTEMP_5)
                  RTEMP_5 =
     + ESCALATED_MONTHLY_VALUE(RTEMP_5,
     +                          INT2(RTEMP_5),
     +                          globecom_YEAR,ISEAS,INT2(1))
               ELSE
                  TEMP_I2 = ESCALATE_ONE_VALUE(RTEMP_5,TEMP_I2,
     +                         OMESCR(I),
     +                         TEMP_UNIT_NAME,globecom_YEAR)
               ENDIF

!
!
               IF(RTEMP_6 < -0.1) THEN
                  RTEMP_6 = ABS(RTEMP_6)
                  RTEMP_6 =
     +  ESCALATED_MONTHLY_VALUE(RTEMP_6,
     +                           INT2(RTEMP_6),
     +                           globecom_YEAR,ISEAS,INT2(1))
               ELSE
                  TEMP_I2 = ESCALATE_ONE_VALUE(RTEMP_6,TEMP_I2,
     +                         FIXED_COST_ESCALATOR(I),
     +                         TEMP_UNIT_NAME,globecom_YEAR)
               ENDIF
!
!
               HEAT_RATE_FACTOR(I) = HEAT_RATE_FACTOR(I) *
     +                                               CO2_RETRO_HEAT_MULT
! THESE ARE THE MW'S USED FOR OPERATIONS FOR THE REST OF THE year.
             IF(GRX_ITERATIONS == 0 .and. .false.) THEN
               IF(CO2_FIRST_CONTROL_YEAR) THEN !  .AND. .NOT.

               ENDIF
               IF(CO2_FIRST_CONTROL_MONTH) THEN
                  IF(INPUT_MW(1,I) < -.01) THEN
! NEED TO 'POKE' A NEW CAPACITY
                  ELSE
                     INPUT_MW(1,I) = INPUT_MW(1,I) * CO2_RETRO_CAP_MULT
                     call write_mw_pc_trace("pro_cost:0006", MW(M,I))
                  ENDIF
                  IF(INPUT_MW(2,I) < -.01) THEN
! NEED TO 'POKE' A NEW CAPACITY
                  ELSE
                     INPUT_MW(2,I) = INPUT_MW(2,I) * CO2_RETRO_CAP_MULT
                  ENDIF
               ENDIF
             ENDIF ! GRX_ITERATIONS== 0
!
            ENDIF
!
!
            RTEMP_7 = 0.0
            RTEMP_8 = 0.0

            IF(HG_CONTROL_FOR_UNIT) THEN

               RTEMP_7 = GET_HG_VOM(I)
!
               IF(RTEMP_7 < -0.1) THEN
                  RTEMP_7=ABS(RTEMP_7)
                  RTEMP_7 =
     + ESCALATED_MONTHLY_VALUE(RTEMP_7,
     +                           INT2(RTEMP_7),
     +                           globecom_YEAR,ISEAS,INT2(1))
               ELSE
                  TEMP_I2 = ESCALATE_ONE_VALUE(RTEMP_7,TEMP_I2,
     +                         OMESCR(I),
     +                         TEMP_UNIT_NAME,globecom_YEAR)
               ENDIF
               
               RTEMP_8 = GET_HG_FOM(I)
!
               IF(RTEMP_8 < -0.1) THEN
                  RTEMP_8 = ABS(RTEMP_8)
                  RTEMP_8 =
     +  ESCALATED_MONTHLY_VALUE(RTEMP_8,
     +                              INT2(RTEMP_8),
     +                              globecom_YEAR,ISEAS,INT2(1))
               ELSE
                  TEMP_I2 = ESCALATE_ONE_VALUE(RTEMP_8,TEMP_I2,
     +                         FIXED_COST_ESCALATOR(I),
     +                         TEMP_UNIT_NAME,globecom_YEAR)
               ENDIF
            ENDIF

            RTEMP_9 = 0.0
            RTEMP_10 = 0.0

            IF(OTHER3_CONTROL_FOR_UNIT) THEN
               
               RTEMP_9 = GET_OTHER3_VOM(I)
!
               IF(RTEMP_9 < -0.1) THEN
                  RTEMP_9=ABS(RTEMP_9)
                  RTEMP_9 =
     +  ESCALATED_MONTHLY_VALUE(RTEMP_9,
     +                              INT2(RTEMP_9),
     +                              globecom_YEAR,ISEAS,INT2(1))
               ELSE
                  TEMP_I2 = ESCALATE_ONE_VALUE(RTEMP_9,TEMP_I2,
     +                         OMESCR(I),
     +                         TEMP_UNIT_NAME,globecom_YEAR)
               ENDIF
               
               RTEMP_10 = GET_OTHER3_FOM(I)
!
               IF(RTEMP_10 < -0.1) THEN
                  RTEMP_10 = ABS(RTEMP_10)
                  RTEMP_10 =
     +  ESCALATED_MONTHLY_VALUE(RTEMP_10,
     +                              INT2(RTEMP_10),
     +                              globecom_YEAR,ISEAS,INT2(1))
               ELSE
                  TEMP_I2 = ESCALATE_ONE_VALUE(RTEMP_10,TEMP_I2,
     +                         FIXED_COST_ESCALATOR(I),
     +                         TEMP_UNIT_NAME,globecom_YEAR)
               ENDIF
            ENDIF

       i_for_argument=i
       VCPMWH(I) = ESCALATE_THERMAL_VOM(globecom_YEAR,ISEAS,
     + i_for_argument) + 
     + RETRO_VOM + RTEMP + RTEMP_3 + RTEMP_5 + RTEMP_7 + RTEMP_9
            i_for_argument=i
            FIXED_COST(I) =
     +     ESCALATE_THERMAL_FOM(globecom_YEAR,ISEAS,I_for_argument) + 
     + RETRO_FOM +
     +  RTEMP_2 + RTEMP_4 + RTEMP_6 + RTEMP_8 + RTEMP_10

            IF(.NOT. DETAILED_MAINTENANCE_ACTIVE .OR.
     +                             PRODUCTION_COSTING_PERIODS == 1) THEN
               IF(DETAILED_MAINTENANCE_ACTIVE) THEN
                  MAINTENANCE_RATE(I) = UNSCHEDULED_MAINT_HOURS(I)
               ELSE
                  MAINTENANCE_RATE(I) = 0.
               ENDIF
               
               DO J = PSMO, PEMO

!                 TEST WHETHER THE UNIT IS ONLINE DURING THE year.


                  DATE1 = BASE_DATE + J
                  IF(ONLINE(I) <= DATE1 .AND. OFLINE(I) >= DATE1) THEN
                     IF(MNRATE(I,J) < 0.) THEN
                        IF(OFFSET_MAINTENANCE_VECTORS() .AND.
     +                              I > NUM_UNITS_BEFORE_ADDITIONS) THEN
                           GET_YR =
     + globecom_YEAR-(ONLINE(I)/100+1900 - get_BASE_YEAR())+1
                        ELSE
                           GET_YR = globecom_YEAR
                        ENDIF
                        MN_RATE = GET_VAR(MNRATE(I,J),GET_YR,UNITNM(I))
                        MAINTENANCE_RATE(I) = MAINTENANCE_RATE(I) +
     +                     get_HOURS_IN_PERIOD(J) * MIN(100.,MN_RATE)

                     ELSE
                        MAINTENANCE_RATE(I) = MAINTENANCE_RATE(I) +
     +                    get_HOURS_IN_PERIOD(J) * MIN(100.,MNRATE(I,J))
                     ENDIF
                  ELSE
                     MAINTENANCE_RATE(I) = MAINTENANCE_RATE(I) +
     +                                     get_HOURS_IN_PERIOD(J) * 100.
                  ENDIF
               ENDDO
               MAINTENANCE_RATE(I) = MAINTENANCE_RATE(I) /
     +                                            (100.*HOURS_INCREMENT)
            ENDIF
! 6/15/93. GAT.
            FUEL_ID = FUEL_INVENTORY_ID(THIS_YEARS_FUEL_SUPPLY_ID(I))
            IF(MONTHLY_INVENTORY_ACTIVE .AND.
     +                  (USE_SECONDARY_FUEL(I) .OR.
     +                        I < SHADOW_UNIT_NUMBER(I)) .AND.

     +                   FUEL_ID > 0 .AND.
     +                   MMBTU_FUEL_BALANCE(FUEL_ID) > 0.0 .AND.

     +               J /= 1) THEN
               USE_SECONDARY_FUEL(I) = .FALSE.
               IF(FUELMX(I) < 0.) THEN
                  ANNUAL_DISP_BTU_COST(I) = SBTUCT(I)
               ELSE
                  ANNUAL_DISP_BTU_COST(I) = ANNUAL_FUEL_BTU_COST(I)
               ENDIF
            ENDIF

! SET NUC PLANT COSTS BASED ON NUCLEAR FUEL FILE. 7/19/96

            i_for_argument=i

            IF(LDTYPE(I) == 'N' .AND.
     +              NUC_FUEL_PRICE_SOURCE_IS_NFILE(i_for_argument)) THEN
               CALL RETURN_NUC_FUEL_PRICES(I,PSMO,SAVE_Yr,
     +                                LEASOR_MARK_UP,NUC_GEN_UNIT_FOUND)
               IF(NUC_GEN_UNIT_FOUND) THEN
                  FUEL_ADDER_ADJUSTMENT(I) = FUELADJ(I) + LEASOR_MARK_UP
                  ANNUAL_BLENDED_BTU_COST(I) = PBTUCT(I)
                  ANNUAL_FUEL_BTU_COST(I) = ANNUAL_BLENDED_BTU_COST(I)
                  ANNUAL_DISP_BTU_COST(I) = ANNUAL_FUEL_BTU_COST(I)
               ELSE
                  FUEL_ADDER_ADJUSTMENT(I) = FUELADJ(I)
               ENDIF
            ELSE
               FUEL_ADDER_ADJUSTMENT(I) = FUELADJ(I)
            ENDIF

! 5/1/99. GAT.
! MONTHLY FUEL PRICES HAVE BEEN CALCULATED AT THIS POINT

            i_for_argument=i
            FT = GET_PRIMARY_MOVER(i_for_argument)
!
! OUTAGES ADDED 5/22/99. GAT.

            IF(FT == 1) THEN
               EAVAIL(I) = MIN(EAVAIL(I) * COAL_AVAIL_MULT,1.0)
            ELSEIF(FT == 2) THEN
               EAVAIL(I) = MIN(EAVAIL(I) * GAS_AVAIL_MULT,1.0)
            ELSEIF(FT ==3) THEN
               EAVAIL(I) = MIN(EAVAIL(I) * OIL_AVAIL_MULT,1.0)
            ELSEIF(FT == 4) THEN
               EAVAIL(I) = MIN(EAVAIL(I) * NUCLEAR_AVAIL_MULT,1.0)
            ELSEIF(FT == 5) THEN

               TEMP_HYDRO_ENERGY = 1.0
               IF(IS_REGIONAL_HYDRO_PARAM_ACTIVE) THEN

        TEMP_HYDRO_ENERGY = GET_PARAM_HYDRO(globecom_YEAR,ISEAS,TG)
               ENDIF

               EAVAIL(I) = EAVAIL(I) * HYDRO_WATER_YEAR_MULT *
     +                                                 TEMP_HYDRO_ENERGY
            ELSEIF(FT == 6) THEN
               EAVAIL(I) = MIN(EAVAIL(I) * OTHER_AVAIL_MULT,1.0)
            ENDIF

! REGIONAL OUTAGES ADDED 10/18/01.
             i_for_argument=i
            IF(YES_REGIONAL_OUTAGES_ACTIVE) THEN
               TG = get_TRANSACTION_GROUP(i_for_argument)
               EAVAIL(I) = MIN(1.0,
     +                 EAVAIL(I) *
     +     GET_MONTHLY_REGIONAL_OUTAGE(globecom_YEAR,ISEAS,TG,FT))
            ENDIF
!
            IF(FT > 4 .OR. FT < 1) FT = 5
!

! ADDED 7/25/99. GAT.
            i_for_argument=i
            RTEMP = GET_MONTHLY_FUEL_INDEX(i_for_argument,ISEAS)
!
            CALL GET_TOTAL_DELIVERY_COST(P_DELIVERY_COST,I)
            CALL GET_TOTAL_S_DELIVERY_COST(S_DELIVERY_COST,I)

! 6/26/00. MOVED CALCULATION OF FUEL PRICES INSIDE UNITS LOOP.
!          THIS PERMITS SCENARIO MAKER TO BE ACTIVE FOR THE FUEL
!          PRICE FILE.
            IF(FUEL_PRICE_DATA_AVAILABLE) THEN

               CALL GET_FUEL_PRICES_BY_UNIT(I,PSMO,PEMO,SAVE_Yr,
     +                            PBTUCT,SBTUCT,FUEL_BTU_COST,FUELMX,
     +                            DISP_BTU_COST,MMBTU_FUEL_BALANCE,
     +                            FUEL_INVENTORY_ID,
     +                            FUEL_INVENTORY_ACTIVE,
     +                            EMISS_FUEL_COST,EMISS_FUEL_ESCAL,
     +                            EMISS_BLENDING_RATE,BLENDED_BTU_COST,
     +                            THIS_YEARS_FUEL_SUPPLY_ID,UNITNM,
     +                            FUEL_POINTERS_USED)

               IF(FUEL_POINTERS_USED) THEN

                  FUEL_BTU_COST(I) =
     +               FUEL_BTU_COST(I) *
     +                                        FUEL_SCEN_MULT(FT) * RTEMP
                  DISP_BTU_COST(I) =
     +               DISP_BTU_COST(I) *
     +                                        FUEL_SCEN_MULT(FT) * RTEMP
                  BLENDED_BTU_COST(I) =
     +               BLENDED_BTU_COST(I) *
     +                                        FUEL_SCEN_MULT(FT) * RTEMP
               ELSE

                  FUEL_BTU_COST(I) =
     +               (ANNUAL_FUEL_BTU_COST(I) + P_DELIVERY_COST ) *
     +                                        FUEL_SCEN_MULT(FT) * RTEMP
                  DISP_BTU_COST(I) =
     +               (ANNUAL_DISP_BTU_COST(I) + P_DELIVERY_COST ) *
     +                                        FUEL_SCEN_MULT(FT) * RTEMP
                  BLENDED_BTU_COST(I) =
     +               (ANNUAL_BLENDED_BTU_COST(I) + P_DELIVERY_COST) *
     +                                        FUEL_SCEN_MULT(FT) * RTEMP
               ENDIF
            ELSE

               FUEL_BTU_COST(I) =
     +               (ANNUAL_FUEL_BTU_COST(I) + P_DELIVERY_COST ) *
     +                                        FUEL_SCEN_MULT(FT) * RTEMP
               DISP_BTU_COST(I) =
     +               (ANNUAL_DISP_BTU_COST(I) + P_DELIVERY_COST ) *
     +                                        FUEL_SCEN_MULT(FT) * RTEMP
               BLENDED_BTU_COST(I) =
     +               (ANNUAL_BLENDED_BTU_COST(I) + P_DELIVERY_COST) *
     +                                        FUEL_SCEN_MULT(FT) * RTEMP
            ENDIF
!
! 121406. PROVIDE LINKAGE FROM GAS MODEL TO POWER MODEL.
!
            IF(FT == 2 .AND. GAS_MODEL_ACTIVE .AND.
     +                                  .NOT. HH_ONLY .AND.
     +                                          GAS_DATA_AVAILABLE) THEN


! INCLUDE DELIVERY ADDERS?
! INCLUDE MONTHLY FUEL INDEX?
! INCLUDE STOCHASTIC MULTIPIERS?

! CONVERT FROM $/MCF TO $/MMBTU

               GSP = MIN(MAX(0,GET_UNIT_GAS_REGION_INDEX(I)),
     +                                                MAX_GAS_REGION_NO)

               TEMP_R4 = GET_MONTHLY_GAS_PRICE_BY_STATE(GSP,ISEAS)

! 081308. FIXED BAD ASSIGNMENT. DEFAULT TO INITIAL PRICE.
!
               IF(TEMP_R4 < -1.0) THEN
!
                  WRITE(4,*) "GAS PRICE NOT FOUND FOR UNIT",UNITNM(I)
               ELSE
                  FUEL_BTU_COST(I) = TEMP_R4
                  DISP_BTU_COST(I) = TEMP_R4
                  BLENDED_BTU_COST(I) = TEMP_R4
               ENDIF
            ENDIF


! 05/09/03. IF TRUE THIS ALTERS BLKNO = 0 (MUST RUN CONDITION)
! ON A MONTHLY BASIS

            i_for_argument=i
            VOID_LOGICAL = TEST_MONTHLY_MUST_RUN(i_for_argument,ISEAS)

! REPORT COST VALUES
           ! todo: Remove code in IF block #2
           if(.false.) then
            IF(globecom_YEAR ==1 .AND. GRX_ITERATIONS == 0 .AND.
     +                                      I ==1 .and. iseas == 1) THEN
               WRITE(16742,"(1X,A)")
     +      "globecom_year,Iters,UnitName,PrimFuel,DispCost,SecFuel,"//
     +                   "FuelAdder,FixedCost,VarCost,EmissFuel"
            ENDIF
            IF((I < 4 .OR. (I > GRX_NUNITS .AND. i < GRX_NUNITS+3)).AND.
     +                                                       ISEAS == 1)
     +         WRITE(16742,"(1X,I4,',',I2,',',20(A,','))") CURRENT_Yr,
     +                  GRX_ITERATIONS,
     +                  trim(CONVERT_2_STR(UNITNM(I)//'-'//
     +                                               CONVERT_2_STR(I))),
     +                  CONVERT_2_STR(PBTUCT(I)),
     +                  CONVERT_2_STR(DISP_BTU_COST(I)),
     +                  CONVERT_2_STR(SBTUCT(I)),
     +                  CONVERT_2_STR(FUEL_ADDER_ADJUSTMENT(I)),
     +                  CONVERT_2_STR(FIXED_COST(I)),
     +                  CONVERT_2_STR(VCPMWH(I)),
     +                  CONVERT_2_STR(EMISS_FUEL_COST(I))


            endif

         ENDDO ! get_nunits()


      IF(COAL_MODEL_RAN_LAST_ITERATION) THEN
         CALL COAL_PRICES_TRANSFERRED_MIDAS(ISEAS,CURRENT_Yr,
     +                                         gc_end_point,
     +                                         PBTUCT,
     +                                         FUEL_BTU_COST,
     +                                         DISP_BTU_COST,
     +                                         BLENDED_BTU_COST,
     +                                         SO2)
      ENDIF

         IF(FUEL_PRICE_DATA_AVAILABLE .AND. get_nunits() <= 0) THEN
               WRITE(4,*) "Fuel Price file active when there are no"
               WRITE(4,*) "generating units.  Fuel prices are bypassed."
         ENDIF

         IF(KEPCO) THEN
            DATE1 = BASE_DATE + PSMO
            DATE2 = BASE_DATE + PEMO
            DO I = 1, get_nunits()
               IF(ONLINE(I) > DATE2 .OR. OFLINE(I) < DATE1) CYCLE
               IF(LDTYPE(I) == 'N' .AND. REALLY_KEPCO) THEN
                  WOLF_CREEK_ENRG_COST = (PBTUCT(I) *
     +                                       HEAT_RATE_FACTOR(I) *
     +                                       COEFF(1,I))/1000.+VCPMWH(I)
               ENDIF
               IF(WABASH_VALLEY) THEN
                  CALL UPC(UNITNM(I),TEMP_UNIT_NAME)
                  IF(INDEX(TEMP_UNIT_NAME,'GIB') /= 0) THEN
                     IF(INDEX(TEMP_UNIT_NAME,'BACK') /= 0 .OR.
     +                         INDEX(TEMP_UNIT_NAME,'REPLACE') /=0) THEN
                        GIBSON_BACKUP_POINTER = I
                     ELSE
                        GIBSON_POINTER = I
                     ENDIF
                  ELSEIF(INDEX(TEMP_UNIT_NAME,'MARK') /= 0 .AND.
     +                           INDEX(TEMP_UNIT_NAME,'PURC') /= 0) THEN
                     MARKET_PURCHASE_POINTER = I
                  ELSEIF(INDEX(TEMP_UNIT_NAME,'PSI') /= 0 .AND.
     +                      (INDEX(TEMP_UNIT_NAME,'SEA') /= 0 .OR.
     +                          INDEX(TEMP_UNIT_NAME,'SUPP') /= 0)) THEN
                     PSI_AREA_LAST_RESOURCE_POINTER = I
                  ELSE
                     IF(WABASH_IM_NIPSCO) THEN

! IF PSI IS A STAND ALONE AREA AND THIS UNIT IS ALLOCATED TO THAT AREA


                        POINTER = CL_ALLOCATE_POINTR(CL_RESOURCE_ID(I))
                        IF(AREA_CAP_ALLOCATORS(3,ISEAS,POINTER) /= 0.)
     +                                PSI_AREA_LAST_RESOURCE_POINTER = I
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO
         ENDIF ! KEPCO OR WABASH

         RESOURCE_CAP = 0.
         DO I = 1, get_nunits()
            IF(WABASH_VALLEY .AND. I == GIBSON_BACKUP_POINTER .AND.
     +                                         GIBSON_POINTER /= 0) THEN
               MW(1,I) = MW(1,GIBSON_POINTER)
               call write_mw_pc_trace("pro_cost:0008", mw(1,I))
               MW(2,I) = MW(2,GIBSON_POINTER)
               call write_mw_pc_trace("pro_cost:0009", mw(2,I))
               MAINTENANCE_RATE(I) = 1.-MAINTENANCE_RATE(GIBSON_POINTER)
               CYCLE
            ENDIF

     

            IF(.NOT. DETAILED_MAINTENANCE_ACTIVE .OR.
     +                             PRODUCTION_COSTING_PERIODS == 1) THEN

               IF(DETAILED_MAINTENANCE_ACTIVE) THEN
                  MAINTENANCE_RATE(I) = UNSCHEDULED_MAINT_HOURS(I)
               ELSE
                  MAINTENANCE_RATE(I) = 0.
               ENDIF

               IF(MONTHLY_CAPACITY_POINTER(I) /= 0) THEN
                  MW(1,I) = 0.
                  call write_mw_pc_trace("pro_cost:0010", mw(1,I))
                  MW(2,I) = 0.
                  call write_mw_pc_trace("pro_cost:0011", mw(2,I))
               ENDIF
               WEIGHTED_HOURS = 0.
               DO J = PSMO, PEMO

                  IF(MONTHLY_CAPACITY_POINTER(I) /= 0) THEN
                     IF(CL_CAP_AREA_LINKED(I)) THEN
               PERIOD_RATE =GET_CLASS_PEAK_NET_DSM(J,UNITNM(I),
     +                                 ABS(MONTHLY_CAPACITY_POINTER(I)))
                     ELSE
                        PERIOD_RATE = GET_VAR(FLOAT(
     +                    ABS(MONTHLY_CAPACITY_POINTER(I))),J,UNITNM(I))
                     ENDIF
                     DO M = 2 , 1, -1
                        IF(INPUT_MW(M,I) < 0.) THEN
         MW_CAPACITY = GET_VAR(INPUT_MW(M,I),globecom_YEAR,
     +                                      UNITNM(I))
                           IF(MW_CAPACITY < 0.) THEN
                              MW_CAPACITY = GET_VAR(MW_CAPACITY,
     +                                                      J,UNITNM(I))
                           ENDIF
                        ELSE
                           MW_CAPACITY = INPUT_MW(M,I)
                        ENDIF
                        IF(PERIOD_RATE > 2.) THEN
                           IF(MW_CAPACITY > 0.) THEN
                              PERIOD_RATE = PERIOD_RATE/MW_CAPACITY
                           ELSE
                              MW_CAPACITY = PERIOD_RATE
                              PERIOD_RATE = 1.
                           ENDIF
                        ENDIF
                        IF(MW_CAPACITY < 1. .AND. M == 1) THEN
                           RTEMP = MW_CAPACITY * UNIT_CAPACITY
                        ELSE
                   RTEMP = MW_CAPACITY * foshyd%CAP_FRAC_OWN(I)/100. *
     +                                PERIOD_RATE*get_HOURS_IN_PERIOD(J)
                        ENDIF
                        IF(M == 2) UNIT_CAPACITY = RTEMP
                        MW(M,I) = MW(M,I) + RTEMP
                    call write_mw_pc_trace("pro_cost:0012", mw(M,I))
                     ENDDO
                  ELSE
                     UNIT_CAPACITY = MW(2,I) * get_HOURS_IN_PERIOD(J)
                  ENDIF
                  WEIGHTED_HOURS = WEIGHTED_HOURS + UNIT_CAPACITY

                  DATE1 = BASE_DATE + J
                  IF(ONLINE(I) <= DATE1 .AND. OFLINE(I) >= DATE1) THEN

!                    NEED TO WEIGHT MAINTENANCE BY HOURS AND CAPACITY
                     IF(MNRATE(I,J) < 0.) THEN
                        IF(OFFSET_MAINTENANCE_VECTORS() .AND.
     +                              I > NUM_UNITS_BEFORE_ADDITIONS) THEN
                   GET_YR=globecom_YEAR-(ONLINE(I)/100+1900-
     + get_BASE_YEAR())+1
                 ELSE
                    GET_YR = globecom_YEAR
                 ENDIF
                 MN_RATE = GET_VAR(MNRATE(I,J),GET_YR,UNITNM(I))
                 MAINTENANCE_RATE(I) = MAINTENANCE_RATE(I) +
     +                                MIN(100.,MN_RATE) * UNIT_CAPACITY
                     ELSE
                        MAINTENANCE_RATE(I) = MAINTENANCE_RATE(I) +
     +                             MIN(100.,MNRATE(I,J)) * UNIT_CAPACITY
                     ENDIF
                  ELSE
                     MAINTENANCE_RATE(I) = MAINTENANCE_RATE(I) +
     +                                              100. * UNIT_CAPACITY
                  ENDIF
               ENDDO
               IF(WEIGHTED_HOURS > 0.0) THEN
                  MAINTENANCE_RATE(I) = MAINTENANCE_RATE(I) /
     +                                       (100.*WEIGHTED_HOURS)
               ENDIF
               IF(MONTHLY_CAPACITY_POINTER(I) /= 0) THEN
                  MW(1,I) = MW(1,I)/HOURS_INCREMENT
                  call write_mw_pc_trace("pro_cost:0012", MW(1,I))
                  MW(2,I) = MW(2,I)/HOURS_INCREMENT
                  call write_mw_pc_trace("pro_cost:0013", MW(2,I))
               ENDIF
            ENDIF

            IF(WABASH_VALLEY) THEN
               DATE1 = BASE_DATE + PSMO
               DATE2 = BASE_DATE + PEMO
               IF(ONLINE(I) <= DATE1 .AND. OFLINE(I) >= DATE2  .AND.
     +               MAINTENANCE_RATE(I) < 1. .AND. EAVAIL(I) > 0.) THEN
                  ARRAY_POINTR = CL_ALLOCATE_POINTR(CL_RESOURCE_ID(I))
                  UNIT_CAPACITY = MW(2,I)
                  TOTAL_AREA_CAP_ALLOCATORS = 0.
                  TOTAL_AREA_LOSSES = 0.
                  DO AREA = 1, NUMBER_OF_AREAS

! ALLOCATE CAPACITY FIRST

                     CAP_ALLOCATOR =
     +                      AREA_CAP_ALLOCATORS(AREA,ISEAS,ARRAY_POINTR)
                     IF(CAP_ALLOCATOR < 0.) CAP_ALLOCATOR =
     +               GET_VAR(CAP_ALLOCATOR,globecom_YEAR,UNITNM(I))
                     IF(CAP_ALLOCATOR <= 1. .AND.
     +                          .NOT. CAPACITY_ALLOCATION_ABSOLUTE) THEN
                        CAP_ALLOCATOR = CAP_ALLOCATOR * UNIT_CAPACITY
                     ENDIF
                     TOTAL_AREA_CAP_ALLOCATORS = CAP_ALLOCATOR +
     +                                         TOTAL_AREA_CAP_ALLOCATORS
                     TOTAL_AREA_LOSSES = TOTAL_AREA_LOSSES +
     +                                    CAP_ALLOCATOR/
     +                (1.-AREA_CAPACITY_LOSSES(AREA,ISEAS,ARRAY_POINTR))
                     IF(.NOT. CL_CAP_AREA_LINKED(I)) THEN
                        RESOURCE_CAP(AREA) = RESOURCE_CAP(AREA) +
     +                                                   CAP_ALLOCATOR *
     +                (1.-AREA_CAPACITY_LOSSES(AREA,ISEAS,ARRAY_POINTR))
                     ENDIF
                  ENDDO
                  IF(CL_CAP_AREA_LINKED(I)) THEN
                     IF(TOTAL_AREA_CAP_ALLOCATORS /= 0.) THEN
                        UNIT_CAPACITY = MW(2,I) * TOTAL_AREA_LOSSES/
     +                                    TOTAL_AREA_CAP_ALLOCATORS
                        MW(1,I) = MW(1,I)*UNIT_CAPACITY/MW(2,I)
          call write_mw_pc_trace("pro_cost:0013", MW(1,I))
     
                        MW(2,I) = UNIT_CAPACITY
                 call write_mw_pc_trace("pro_cost:0014", MW(2,I))
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDDO ! end thermal units

         DO I = 1, HYDRO_UNITS
            HYDRO_MONTHLY_ENRG(I) = 0. D0
            HYDROCAPMO(I) = 0. D0
            DO J = PSMO, PEMO
               IF(IN_CAPMO(J,I) .LT. 0) THEN
                  TEMP_HYDRO_CAP = get_HOURS_IN_PERIOD(J) *
     +                     GET_VAR(IN_CAPMO(J,I),globecom_YEAR,
     +                            'from the Energy Limited File')
               ELSE
                TEMP_HYDRO_CAP = get_HOURS_IN_PERIOD(J) * IN_CAPMO(J,I)
               ENDIF
               HYDROCAPMO(I) = HYDROCAPMO(I) + TEMP_HYDRO_CAP

               IF(ENRGMO(J,I) .LT. 0.) THEN
               TEMP_HYDRO_ENERGY =  GET_VAR(ENRGMO(J,I),globecom_YEAR,
     +                                   'from the Energy Limited File')
               ELSE
                  TEMP_HYDRO_ENERGY = ENRGMO(J,I)
               ENDIF

               TEMP_HYDRO_ENERGY = TEMP_HYDRO_ENERGY *
     +                                             HYDRO_WATER_YEAR_MULT

               IF(IS_REGIONAL_HYDRO_PARAM_ACTIVE) THEN
                  TG = HYDRO_TRANS_GROUP(I)
                  TEMP_HYDRO_ENERGY = TEMP_HYDRO_ENERGY *
     +                       GET_PARAM_HYDRO(globecom_YEAR,ISEAS,TG)
               ENDIF

               IF(TEMP_HYDRO_ENERGY < 1.0) THEN
                  IF(TEMP_HYDRO_ENERGY < 0.) TEMP_HYDRO_ENERGY = 0.
                  HYDRO_MONTHLY_ENRG(I) = HYDRO_MONTHLY_ENRG(I) +
     +                                  TEMP_HYDRO_ENERGY*TEMP_HYDRO_CAP
               ELSE
                  HYDRO_MONTHLY_ENRG(I) = HYDRO_MONTHLY_ENRG(I) +
     +                                                 TEMP_HYDRO_ENERGY
               ENDIF


            ENDDO
            HYDROCAPMO(I) = HYDROCAPMO(I)/HOURS_INCREMENT
         ENDDO

! CONTRACTS
         DATE1 = BASE_DATE + PSMO
         DATE2 = BASE_DATE + PEMO
         DO I = 1, NUMBER_OF_CONTRACTS
! TEST THE MONTHLY ESCALATION FILE
            CT_FIRST_ENERGY_PRICE(I) = ESCALATED_MONTHLY_VALUE(
     +                        CT_FIRST_ENERGY_PRICE(I),CNTR_ENER_ESC(I),
     +                        globecom_YEAR,ISEAS,UPDATE_MONTH(I))
            CT_SECOND_ENERGY_PRICE(I) = ESCALATED_MONTHLY_VALUE(
     +                                        CT_SECOND_ENERGY_PRICE(I),
     +                                    CT_SECOND_ENERGY_ESCALATOR(I),
     +                        globecom_YEAR,ISEAS,UPDATE_MONTH(I))
            CT_ENERGY_COST_ADDER(I) =  ESCALATED_MONTHLY_VALUE(
     +                                          CT_ENERGY_COST_ADDER(I),
     +                               CT_ENERGY_COST_ADDER_ESC_VECTOR(I),
     +                         globecom_YEAR,ISEAS,UPDATE_MONTH(I))
            MIN_CONTRACT_FIXED_COST(I) = ESCALATED_MONTHLY_VALUE(
     +                                       MIN_CONTRACT_FIXED_COST(I),
     +                                              CNTR_MIN_CAP_ESC(I),
     +                        globecom_YEAR,ISEAS,UPDATE_MONTH(I))
            CONTRACT_FIXED_COST(I) = ESCALATED_MONTHLY_VALUE(
     +                                           CONTRACT_FIXED_COST(I),
     +                                                  CNTR_CAP_ESC(I),
     +                         globecom_YEAR,ISEAS,UPDATE_MONTH(I))
            CT_ANNUAL_FIXED_COST(I) = ESCALATED_MONTHLY_VALUE(
     +                                          CT_ANNUAL_FIXED_COST(I),
     +                                      CT_ANNUAL_FIXED_COST_ESC(I),
     +                          globecom_YEAR,ISEAS,UPDATE_MONTH(I))
            CT_MONTHLY_1ST_ENERGY_PRICE(I) = CT_FIRST_ENERGY_PRICE(I)
            ANNUAL_CONTRACT_FIXED_COST(I)=ANNUAL_CONTRACT_FIXED_COST(I)+
     +                                       CT_ANNUAL_FIXED_COST(I)/12.
            IF(CNTR_ENERGY_SWITCH(I)== 'T') THEN
               CONTRACT_VARIABLE_COST(I)= CT_MONTHLY_1ST_ENERGY_PRICE(I)
            ELSEIF(CT_ENERGY_COST_WEIGHTING_FACTOR(I) /= 0.) THEN
               RTEMP = CT_ENERGY_COST_WEIGHTING_FACTOR(I)
               CONTRACT_VARIABLE_COST(I)=RTEMP*
     +                  CT_MONTHLY_1ST_ENERGY_PRICE(I)+
     +                              (1.-RTEMP)*CT_SECOND_ENERGY_PRICE(I)
            ELSE
               CONTRACT_VARIABLE_COST(I) =
     +            CT_MONTHLY_1ST_ENERGY_PRICE(I) +
     +                                         CT_SECOND_ENERGY_PRICE(I)
            ENDIF
            IF(WKP_ACTIVE .OR. REALLY_KEPCO) THEN
               IF(ISEAS == UPDATE_MONTH(I) .OR.
     +              (globecom_YEAR == 1 .AND. PERIOD_COUNTER == 1)) THEN
                  IF(UPDATE_MONTH(I) == 1 .OR.
     +               (globecom_YEAR == 1 .AND. PERIOD_COUNTER == 1) .OR.
     +               (ISEAS == UPDATE_MONTH(I)) .AND.
     +                   globecom_YEAR == MAX_SIMULATION_YEARS) THEN
                     J = globecom_YEAR
                  ELSE
                     J = globecom_YEAR+1
                  ENDIF
                  IF(MAX_ANNUAL_ENERGY(I) < 0.) THEN
                     REMAIN_ANN_ENRG(I) = GET_VAR(
     +                                 MAX_ANNUAL_ENERGY(I),J,CNTRNM(I))
                  ELSE
                     REMAIN_ANN_ENRG(I) = MAX_ANNUAL_ENERGY(I)
                  ENDIF
                  IF(MAX_ANNUAL_CAPACITY(I) < 0.) THEN
                     MAXIMUM_ANNUAL_CAPACITY(I) = GET_VAR(
     +                               MAX_ANNUAL_CAPACITY(I),J,CNTRNM(I))
                  ELSE
                     MAXIMUM_ANNUAL_CAPACITY(I) = MAX_ANNUAL_CAPACITY(I)
                  ENDIF
                  IF(REALLY_KEPCO)
     +                        CT_TARGET_ANN_ENRG(I) = REMAIN_ANN_ENRG(I)
               ENDIF
            ELSE
               IF(PERIOD_COUNTER == 1) THEN
                  IF(MAX_ANNUAL_ENERGY(I) < 0.) THEN
                     REMAIN_ANN_ENRG(I) = GET_VAR(
     +                MAX_ANNUAL_ENERGY(I),globecom_YEAR,CNTRNM(I))
                  ELSE
                     REMAIN_ANN_ENRG(I) = MAX_ANNUAL_ENERGY(I)
                  ENDIF
                  CT_TARGET_ANN_ENRG(I) = REMAIN_ANN_ENRG(I)
                  IF(MAX_ANNUAL_CAPACITY(I) < 0.) THEN
                     MAXIMUM_ANNUAL_CAPACITY(I) = GET_VAR(
     +                   MAX_ANNUAL_CAPACITY(I),globecom_YEAR,CNTRNM(I))
                  ELSE
                     MAXIMUM_ANNUAL_CAPACITY(I) = MAX_ANNUAL_CAPACITY(I)
                  ENDIF
               ENDIF
            ENDIF
            IF(CT_ENERGY_PATTERN_VECTOR(I) /= 0) THEN
               RTEMP = FLOAT(CT_ENERGY_PATTERN_VECTOR(I))
               MAX_ENERGY(I) = GET_VAR(RTEMP,ISEAS,CNTRNM(I))
               IF(CT_PATTERN_SUM(I) /= 0.) THEN
                  MAX_ENERGY(I) =  CT_TARGET_ANN_ENRG(I) *
     +                                (MAX_ENERGY(I)/CT_PATTERN_SUM(I))
               ENDIF
               MIN_ENERGY(I) = 0.
            ENDIF

! WABASH AREA ALLOCATION

            IF(WABASH_VALLEY .AND.
     +                      INDEX('123456EMSRKL',CNTRTYPE(I)) == 0) THEN
               IF(CNTR_ON_LI(I) <= DATE1 .AND.
     +                                     CNTR_OFF_LI(I) >= DATE2) THEN
                  ARRAY_POINTR=CT_ALLOCATE_POINTR(CT_RESOURCE_ID(I))
                  UNIT_CAPACITY = MAXIMUM_ANNUAL_CAPACITY(I)
                  DO AREA = 1, NUMBER_OF_AREAS

! ALLOCATE CAPACITY FIRST

                     CAP_ALLOCATOR =
     +                      AREA_CAP_ALLOCATORS(AREA,ISEAS,ARRAY_POINTR)
                     IF(CAP_ALLOCATOR < 0.) CAP_ALLOCATOR =
     +              GET_VAR(CAP_ALLOCATOR,globecom_YEAR,UNITNM(I))
                     IF(CAP_ALLOCATOR <= 1. .AND.
     +                          .NOT. CAPACITY_ALLOCATION_ABSOLUTE) THEN
                        CAP_ALLOCATOR = CAP_ALLOCATOR * UNIT_CAPACITY
                     ENDIF
                     RESOURCE_CAP(AREA) = RESOURCE_CAP(AREA) +
     +                                                   CAP_ALLOCATOR *
     +                (1.-AREA_CAPACITY_LOSSES(AREA,ISEAS,ARRAY_POINTR))
                  ENDDO
               ENDIF
            ENDIF
         ENDDO

! WABASH DYNAMIC RESOURCE ALLOCATION

         IF(WABASH_VALLEY) THEN

            LAST_CLASS_IN_AREA1 = 2
            IF(WABASH_IM_NIPSCO_PSI) LAST_CLASS_IN_AREA1 = 3
       CONTROL_AREA_1_NEEDS = GET_AREA1_PEAK_NET_DSM(ISEAS,INT2(1), 
     + CONTROL_AREA_1_NEEDS)
            CALL GET_CLASS_MW_RESERVE_ALLOC(ISEAS,
     +                                      DSM_PEAK_RESERVE_ALLOCATION)
            RTEMP = 0.

            CALL GET_CLASS_IN_AREA(CLASS_IN_AREA_1,INT(1,2))

            FOUND_CLASS_FORECAST = GET_PEAK_ENERGY_AFTER_LOSSES(ISEAS,
     +                                          CLASS_PEAK_AFTER_LOSSES,
     +                                        CLASS_ENERGY_AFTER_LOSSES)
            DO AREA = 1, MAX_LOAD_CLASSES
               IF(.NOT. CLASS_IN_AREA_1(AREA)) CYCLE

! 7/31/98. GAT. PER STEININGER.

               IF(CLASS_RESERVE_MARGIN(AREA) >= 0.) THEN
                  LOCAL_CLASS_RESERVE_MARGIN =
     +                                   CLASS_RESERVE_MARGIN(AREA)/100.
               ELSE
                  LOCAL_CLASS_RESERVE_MARGIN =
     +               GET_VAR(CLASS_RESERVE_MARGIN(AREA),ISEAS,
     +                              'from the Class Reserves WVPA')/100.
               ENDIF
!
               RTEMP = RTEMP + MAX(0.,
! PLUS RESERVE MARGIN NEEDS
     +              LOCAL_CLASS_RESERVE_MARGIN *
     +                 CLASS_COIN_FACTOR(AREA) *
     +                     MAX(CLASS_PEAK_AFTER_LOSSES(1,AREA),
     +                         CLASS_PEAK_AFTER_LOSSES(2,AREA)) +
! MINUS RESERVES PROVIDED BY DSM NOTE THE DSM VALUES ARE NEGATIVE
     +              DSM_PEAK_RESERVE_ALLOCATION(2,AREA) +
     +              DSM_PEAK_RESERVE_ALLOCATION(3,AREA))
            ENDDO
            CONTROL_AREA_1_NEEDS = CONTROL_AREA_1_NEEDS + RTEMP
            DO AREA = 2, MAX_LOAD_CLASSES
               IF(.NOT. CLASS_IN_AREA_1(AREA)) CYCLE
               RESOURCE_CAP(1) = RESOURCE_CAP(1) + RESOURCE_CAP(AREA)
            ENDDO
            CALL GET_CLASS_EXISTS(CLASS_EXISTS_loc)
            DO AREA = MAX_LOAD_CLASSES, 1, -1
               IF(.NOT. CLASS_EXISTS_loc(AREA)) CYCLE
               DO I = 1, get_nunits()
                  IF(ABS(MONTHLY_CAPACITY_POINTER(I)) /= AREA .OR.
     +                                .NOT. CL_CAP_AREA_LINKED(I)) CYCLE
                  IF(ONLINE(I) > DATE2 .OR. OFLINE(I) < DATE1) CYCLE
                  ARRAY_POINTR = CL_ALLOCATE_POINTR(CL_RESOURCE_ID(I))
                  LOSSES = (1. -
     +                    AREA_CAPACITY_LOSSES(AREA,ISEAS,ARRAY_POINTR))
                  IF(MW(2,I) /= 0.) THEN
                     RTEMP = MW(1,I)/MW(2,I)
                  ELSE
                     RTEMP = 0.
                  ENDIF
                  IF(AREA == 1) THEN
                     MW(2,I) = CONTROL_AREA_1_NEEDS
                     call write_mw_pc_trace("pro_cost:0015", MW(2,I))
                  ELSE
                     MW(2,I) = MW(2,I) * LOSSES
                     call write_mw_pc_trace("pro_cost:0016", MW(2,I))
                  ENDIF
                  MW(2,I) = MAX(0.,MW(2,I) - RESOURCE_CAP(AREA))
                  call write_mw_pc_trace("pro_cost:0017", MW(2,I))
                  IF(INPUT_MW(2,I) < 0.) THEN
                     AVAILABLE_MWS =
     +               GET_VAR(INPUT_MW(2,I),globecom_YEAR,UNITNM(I))
                  ELSE
                     AVAILABLE_MWS = INPUT_MW(2,I)
                  ENDIF
                  AVAILABLE_MWS = AVAILABLE_MWS * LOSSES *
     +                                     foshyd%CAP_FRAC_OWN(I)/100.
                  IF(I /= PSI_AREA_LAST_RESOURCE_POINTER)
     +                       MW(2,I) = MIN(MW(2,I),AVAILABLE_MWS)
                  MW(1,I) = RTEMP * MW(2,I)
                  call write_mw_pc_trace("pro_cost:0018", MW(1,I))
                  IF(AREA > 1) RESOURCE_CAP(AREA) = RESOURCE_CAP(AREA) +
     +                                                     MW(2,I)
                  IF(CLASS_IN_AREA_1(AREA)) THEN
                     RESOURCE_CAP(1) = RESOURCE_CAP(1) + MW(2,I)
                  ENDIF
                  MW(2,I) = MW(2,I)/LOSSES
                  call write_mw_pc_trace("pro_cost:0019", MW(2,I))
                  MW(1,I) = MW(1,I)/LOSSES
                  call write_mw_pc_trace("pro_cost:0020", MW(1,I))

               ENDDO
            ENDDO
         ENDIF ! WVPA

! ---- CAPEX
       call write_trace_bool1(file_trace_procost, "1. SP_CAPEX_ACTIVE", 
     +      SP_CAPEX_ACTIVE())
       call write_trace_int2(file_trace_procost, "ISEAS", ISEAS)
       call write_trace_bool(file_trace_procost, 
     + "NEW_SUPPLY_CURVE_REPORT", NEW_SUPPLY_CURVE_REPORT()=="T")
    

         IF(SP_CAPEX_ACTIVE() ) THEN
         call write_trace_message(file_trace_procost, "PATH: " //
     +      "1. call sp_capex.")
     
            CALL SP_CAPEX_OUTPUT(ISEAS,
     +                           HEAT_RATE_FACTOR,
     +                           MAINTENANCE_RATE,
     +                           DISP_BTU_COST,
     +                           USE_SECONDARY_FUEL,
     +                           BLENDED_BTU_COST)
            call write_trace_message(file_trace_procost, "1. CYCLE")
            CYCLE
         ELSEIF(NEW_SUPPLY_CURVE_REPORT()) THEN
            call write_trace_message(file_trace_procost, "PATH: " //
     +       "NEW_SUPPLY_CURVE_REPORT is TRUE.")
         ! pro_cost:0003
         call write_trace_message(file_trace_procost, "PATH: " //
     +      "2. call sp_capex.")
     
            CALL SP_CAPEX_OUTPUT(ISEAS,
     +                           HEAT_RATE_FACTOR,
     +                           MAINTENANCE_RATE,
     +                           DISP_BTU_COST,
     +                           USE_SECONDARY_FUEL,
     +                           BLENDED_BTU_COST)
         ENDIF
! ---- CAPEX

         ! DISPATCH_REPORT MEANS ANNAUL REPORT
       IF(.NOT. TESTING_PLAN .AND. DISPATCH_REPORT() .AND. 
     + PERIOD_COUNTER == 1) THEN
            DATE1 = BASE_DATE + 1
            DATE2 = BASE_DATE + 12
            
            ! TODO:  Rename `dispatch_report` variables to `annual`?
            ! THIS MEANS TO PRODUCE AN ANNUAL REPORT
            CALL_DISPATCH_REPORT = .TRUE. 
            DISPAT_BTU_TAX_ACTIVE = BTU_TAX_ACTIVE_DISP_RPT .AND.
     +                                         INCLUDE_IN_DISPATCH_ORDER
            CALL DISPAT(SEASONAL_SYSTEM_CAP,HEAT_RATE_FACTOR,
     +                  MAINTENANCE_RATE,CALL_DISPATCH_REPORT,
     +                  DISP_BTU_COST,DISP_BTU_TAX_ADDER,
     +                  DISPAT_BTU_TAX_ACTIVE,
     +                  FUEL_INVENTORY_ACTIVE,
     +                  USE_SECONDARY_FUEL,ISEAS)
         ENDIF
         CALL_DISPATCH_REPORT = .FALSE.
         DATE1 = BASE_DATE + PSMO
         DATE2 = BASE_DATE + PEMO
         CALL DISPAT(SEASONAL_SYSTEM_CAP,HEAT_RATE_FACTOR,
     +               MAINTENANCE_RATE,CALL_DISPATCH_REPORT,
     +               DISP_BTU_COST,DISP_BTU_TAX_ADDER,
     +               BTU_TAX_ACTIVE .AND. INCLUDE_IN_DISPATCH_ORDER,
     +               FUEL_INVENTORY_ACTIVE,
     +               USE_SECONDARY_FUEL,ISEAS)
         SYSCAP = MAX(SYSCAP,NINT(SEASONAL_SYSTEM_CAP))

         IF(CAP_PLANNING_METHOD_ANNDECOMP) THEN
            CALL LOCATEW(7,0,32)
            WRITE(CHR_ISEAS,"(I2)") ISEAS
            CALL PRINTW(7,CHR_ISEAS)
         ELSE 
            IF(LAHEY_LF95()) THEN
               IF(get_nunits() > 0) THEN
                  IF(HYDRO_UNITS > 0) THEN
                     WRITE(SCREEN_MESSAGES,"(A,I3,A,I4,A,I5,A,I4,A)")
     +                    'EndPt:',INT(PRT_ENDPOINT()),' ',CURRENT_Yr,
     +                             ', '//trim(MONTH_NAME(ISEAS))//' ',
     +                             get_nunits(),'-thermal units. ',
     +                             HYDRO_UNITS,'-hydro units.'
                  ELSE
                     WRITE(SCREEN_MESSAGES,"(A,I3,A,I4,A,I5,A)")
     +                    'EndPt:',INT(PRT_ENDPOINT()),' ',CURRENT_Yr,
     +                             ', '//trim(MONTH_NAME(ISEAS))//' ',
     +                             get_nunits(),'-thermal units. '
                  ENDIF
                  CALL MG_LOCATE_WRITE(20,70,trim(SCREEN_MESSAGES),
     +                                                   ALL_VERSIONS,0)
               ELSEIF(HYDRO_UNITS > 0) THEN
                  WRITE(SCREEN_MESSAGES,"(A,I3,A,I4,A,I4,A)")
     +                    'EndPt:',INT(PRT_ENDPOINT()),' ',CURRENT_Yr,
     +                             ', '//trim(MONTH_NAME(ISEAS))//' ',
     +                             HYDRO_UNITS,'-hydro units.'
                  CALL MG_LOCATE_WRITE(20,70,trim(SCREEN_MESSAGES),
     +                                                   ALL_VERSIONS,0)
               ENDIF
            ELSE

               IF(.NOT. INIT_FILE_IS_ACTIVE()) THEN
                  WRITE(SCREEN_MESSAGES,"(I4)") get_nunits()
                  CALL MG_LOCATE_WRITE(15,70,trim(SCREEN_MESSAGES),
     +                                                   ALL_VERSIONS,0)
                  WRITE(SCREEN_MESSAGES,"(I4)") HYDRO_UNITS
                  CALL MG_LOCATE_WRITE(17,70,trim(SCREEN_MESSAGES),
     +                                                   ALL_VERSIONS,0)
                  WRITE(SCREEN_MESSAGES,"(I4)")
     +                                     GET_NUMBER_OF_ACTIVE_GROUPS()
                  CALL MG_LOCATE_WRITE(18,70,trim(SCREEN_MESSAGES),
     +                                                   ALL_VERSIONS,0)
               ELSE
                  WRITE(SCREEN_MESSAGES,"(I4)") get_nunits()+HYDRO_UNITS
                  CALL MG_LOCATE_WRITE(20,70,trim(SCREEN_MESSAGES),
     +                                                   ALL_VERSIONS,0)
               ENDIF
               WRITE(SCREEN_MESSAGES,"(I2)") ISEAS
               CALL MG_LOCATE_WRITE(11,30,trim(SCREEN_MESSAGES),
     +                                                   ALL_VERSIONS,0)
            ENDIF


         ENDIF
         DO J = 1 , 2
            P_CLASS_ASSIGNED_ENERGY(J) = 0.
            P_CLASS_ASSIGNED_COST(J) = 0.
            P_CLASS_ASSIGNED_FIXED_COST(J) = 0.
         ENDDO
         BEFORE_LOAD_CURVE = .TRUE.
         DYN_STORAGE_PUMP_ENRG = 0. D0
         DYN_STORAGE_GEN_ENRG = 0. D0

         TRANS_HYDRO = TF_FILE_EXISTS .AND.
     +                                 YES_USE_TRANSACT_LOADS()
         
         IF(.NOT. TRANS_HYDRO .AND.
     +               (YES_RUN_MULTIAREA_TRANSACT() .OR.
     +                                  YES_STRICT_MARKET_PRICE())) THEN
!
            NUMBER_OF_ACTIVE_TRANS_GROUPS =
     +                                     GET_NUMBER_OF_ACTIVE_GROUPS()
        if(NUMBER_OF_ACTIVE_TRANS_GROUPS==0) then
            stop "pro_cost:0001 - NUMBER_OF_ACTIVE_TRANS_GROUPS is zero"
        end if
            CALL INIT_TRANS_ROR_CAPACITY(NUMBER_OF_ACTIVE_TRANS_GROUPS)
!
            CALL GET_MAX_TRANS_LOAD_GROUPS(MAX_TRANS_LOAD_GROUPS)
            VOID_LOGICAL =
     +                   INIT_TRANS_LOAD_AFTER_EL(MAX_TRANS_LOAD_GROUPS,
     +                                                  HOURS_INCREMENT)
         ENDIF

         IF(TRANS_HYDRO .OR. TRANSACT_ANALYST_ONLY()) THEN

            NUMBER_OF_ACTIVE_TRANS_GROUPS =
     +                                     GET_NUMBER_OF_ACTIVE_GROUPS()
!
            NUMBER_OF_HYDRO_LOAD_GROUPS =
     +                                    GET_NUMBER_OF_HYDRO_GROUPS()
            IF(ALLOCATED(FIRST_HYDRO_LOAD_NO))
     +                                   DEALLOCATE(FIRST_HYDRO_LOAD_NO,
     +                                                   HYDRO_AG_IPNTX)
            ALLOCATE(FIRST_HYDRO_LOAD_NO(0:NUMBER_OF_HYDRO_LOAD_GROUPS),
     +                    HYDRO_AG_IPNTX(0:NUMBER_OF_HYDRO_LOAD_GROUPS))
            FIRST_HYDRO_LOAD_NO = 0
            HYDRO_AG_IPNTX = 79
            if(NUMBER_OF_ACTIVE_TRANS_GROUPS==0) then
              stop "pro_cost:0002 - number active trans groups is zero."
            end if
            CALL INIT_TRANS_ROR_CAPACITY(NUMBER_OF_ACTIVE_TRANS_GROUPS)

            CALL GET_MAX_TRANS_LOAD_GROUPS(MAX_TRANS_LOAD_GROUPS)
            VOID_LOGICAL =
     +                   INIT_TRANS_LOAD_AFTER_EL(MAX_TRANS_LOAD_GROUPS,
     +                                                  HOURS_INCREMENT)
            DO TRANS = 1, MAX_TRANS_LOAD_GROUPS

! HYDRO LOAD AGGREGATION CASES

               HYDRO_LOAD_NO = GET_TRANS_LOAD_AGGREGATION(TRANS)

! GET TRANSACTION LDC'S


               IF(HYDRO_LOAD_NO <= 0) THEN
                  LPROB_100 = 0.
                  LODDUR_100 = 0.
                  CALL GET_TRANS_LOAD_PROB(TRANS_DEMAND,DX,
     +                                     TRANS_PEAK,LODDUR(1),
     +                                     LPROB_100,LODDUR_100,IPNTX,
     +                                     TRANS)
               ELSE
                  IF(FIRST_HYDRO_LOAD_NO(HYDRO_LOAD_NO) == 0) THEN
                     LPROB_100 = 0.
                     LODDUR_100 = 0.
                     FIRST_HYDRO_LOAD_NO(HYDRO_LOAD_NO) = 1
                  ELSE
                     IPNTX = HYDRO_AG_IPNTX(HYDRO_LOAD_NO)
                  ENDIF
                  ! TO SET TRANS FOR ENRGLIMT
                  CALL PUT_TARGET_TRANS_GROUP(TRANS) 
                  CALL GET_HYDRO_LOAD_PROB(TRANS_DEMAND,DX,
     +                                     TRANS_PEAK,LODDUR(1),
     +                                     LPROB_100,LODDUR_100,IPNTX,
     +                                     HYDRO_LOAD_NO)
               ENDIF

               TRANS_EL_GENERATION = TRANS_DEMAND
               TRANS_EL_CAPACITY = TRANS_PEAK

! OPERATE ON HYDRO FOR EACH LDC

         IF(globecom_YEAR == 17 .AND. ISEAS == 9) THEN
            ISEAS = ISEAS
         ENDIF

         IF(DEBUG_ON()) CALL write_scroll_line_RW('ENRGLIMT',2)
               IF(HYDRO_UNITS > 0 .AND. PROCMETH() /= 4) THEN
                  CALL ENRGLIMT(
     +               LPROB,LODDUR,DX,TRANS_PEAK,HYDRO_MONTHLY_ENRG,
     +               HYDROCAPMO,
     +               EL_SO2_PERIOD(ISEAS),EL_SO2,
     +               TRANS_DEMAND,SALES_ENERGY,
     +               SALES_ENERGY_NOT_IN_FORECAST,
     +               ENRG_LIMITED_PURCHASE_ENERGY,
     +               globecom_YEAR,ISEAS,IPNTX,HOURS_INCREMENT,
     +               PERIOD_COUNTER,
     +               BTU_TAX_ACTIVE,
     +               ANNUAL_EL_MWH_H12,
     +               ANNUAL_EL_MWH_FOR_BTU_TAX_H12)

               ENDIF
               TRANS_EL_GENERATION = TRANS_EL_GENERATION -
     +                                                      TRANS_DEMAND
               TRANS_EL_CAPACITY = TRANS_EL_CAPACITY - TRANS_PEAK
               DEMAND = DEMAND + TRANS_DEMAND
               PEAK = PEAK + TRANS_PEAK
! CREATE AND SAVE AFTER HYDRO HOURLY LOADS FROM AFTER HYDRO LDC'S
               IF(HYDRO_LOAD_NO <= 0) THEN
                  RTEMP =
     +                TRANS_LOAD_PROB_2_HOURS(LPROB,LODDUR,
     +                                        TRANS_DEMAND,
     +                                        TRANS_EL_GENERATION,
     +                                        TRANS_EL_CAPACITY,
     +                                        TRANS_PEAK,
     +                                        ISEAS,
     +                                        HOURS_INCREMENT,
     +                                        TRANS,
     +                                        IPNTX)
               ELSE
                  HYDRO_AG_IPNTX(HYDRO_LOAD_NO) = IPNTX
                  CALL PUT_HYDRO_LOAD_PROB(TRANS_DEMAND,DX,
     +                                     TRANS_PEAK,LODDUR(1),
     +                                     LPROB_100,LODDUR_100,IPNTX,
     +                                     HYDRO_LOAD_NO)

! FANCY ROUTINE

! DETERMINE CHANGE IN HOURLY LOADS DUE TO HYDRO AND APPLY IT TO
! THE TRANSACTION GROUP. ALLOW FOR NEGATIVE VALUES.

                  RTEMP =
     +                HYDRO_LOAD_PROB_2_HOURS(LPROB,LODDUR,
     +                                        TRANS_DEMAND,
     +                                        TRANS_EL_GENERATION,
     +                                        TRANS_EL_CAPACITY,
     +                                        TRANS_PEAK,
     +                                        ISEAS,
     +                                        HOURS_INCREMENT,
     +                                        TRANS,
     +                                        IPNTX,
     +                                        HYDRO_LOAD_NO)
               ENDIF
            ENDDO
            IF(HYDRO_UNITS > 0 .AND. PROCMETH() /= 4) THEN
               IF(SERVICE_TRANSACTIONS_ACTIVE)
     +            CALL EL_SERVICE_TRANS_CALCULATIONS(HYDRO_UNITS,
     +                                               HYDRO_MONTHLY_ENRG,
     +                                               HYDROCAPMO,
     +                                               DATE1,DATE2,ISEAS)
               ENRG_LIMITED_CAPACITY = MAX(ENRG_LIMITED_CAPACITY,
     +                              SEASONAL_ENRG_LIMITED_CAPACITY)
               TOTAL_SALES_ENERGY = TOTAL_SALES_ENERGY + SALES_ENERGY
            ENDIF
         ELSE

! MOVED ITEMS


            READ(8800,REC=ISEAS) DEMAND,DX,PEAK,LODDUR(1),LPROB_100,
     +                           LODDUR_100,IPNTX,AVE_RUNNING_RATE
            IPNTX = SET_CURRENT_IPNT(IPNTX)
            IF(ECON_SWITCH()) THEN
               DO I = 1, LOAD_CURVE_POINTS
                  RR_OBS(I) = LPROB_100(I)
               ENDDO
            ENDIF
            IF(LOAD_PROB_PLOT() .AND. .NOT. TESTING_PLAN) THEN
               PLOT_PTS = IPNTX+1
               CALL LPPLOT(LODDUR,LPROB,PLOT_PTS,ISEAS,
     +                       gc_end_point,CURRENT_Yr,BEFORE_LOAD_CURVE)
            ENDIF
            BASE_AFTER_DSM = LODDUR(1)
            PEAK_AFTER_DSM = PEAK
            BASE_TO_PEAK_RATIO = BASE_AFTER_DSM/PEAK_AFTER_DSM
            DEMAND_AFTER_DSM_BEFORE_EL = DEMAND
! END MOVED ITEMS
            IF(HYDRO_UNITS > 0 .AND. PROCMETH() /= 4) THEN
               IF(DEBUG_ON()) CALL write_scroll_line_RW('ENRGLIMT 2',2)
               CALL ENRGLIMT(
! REAL*4
     +             LPROB,LODDUR,DX,PEAK,HYDRO_MONTHLY_ENRG,HYDROCAPMO,
     +             EL_SO2_PERIOD(ISEAS),EL_SO2,
! REAL*8
     +             DEMAND,SALES_ENERGY,SALES_ENERGY_NOT_IN_FORECAST,
     +             ENRG_LIMITED_PURCHASE_ENERGY,
! INTEGER*2
     +             globecom_YEAR,ISEAS,IPNTX,HOURS_INCREMENT,
     +             PERIOD_COUNTER,
! LOGICAL*1
     +             BTU_TAX_ACTIVE,
! REAL*8
     +             ANNUAL_EL_MWH_H12,
     +             ANNUAL_EL_MWH_FOR_BTU_TAX_H12)
               IF(SERVICE_TRANSACTIONS_ACTIVE)
     +            CALL EL_SERVICE_TRANS_CALCULATIONS(HYDRO_UNITS,
     +                                               HYDRO_MONTHLY_ENRG,
     +                                               HYDROCAPMO,
     +                                               DATE1,DATE2,ISEAS)
               ENRG_LIMITED_CAPACITY = MAX(ENRG_LIMITED_CAPACITY,
     +                              SEASONAL_ENRG_LIMITED_CAPACITY)
               TOTAL_SALES_ENERGY = TOTAL_SALES_ENERGY + SALES_ENERGY
            ENDIF
         ENDIF

         DEMAND_AFTER_DSM_AFTER_EL = DEMAND
         PEAK_AFTER_EL = PEAK
         EL_SO2_ANNUAL = EL_SO2_ANNUAL + EL_SO2_PERIOD(ISEAS)

         IF(IPNTX > 0 .AND. IPNTX < 1000) THEN
            LPROB(IPNTX+1) = 0.
            LODDUR(IPNTX+1) = LODDUR(IPNTX) + DX
         ENDIF

         BEFORE_LOAD_CURVE = .FALSE.
         IF(HYDRO_UNITS > 0 .AND. PROCMETH() /= 4 .AND.
     +                   LOAD_PROB_PLOT() .AND. .NOT. TESTING_PLAN) THEN
            PLOT_PTS = IPNTX+1
            CALL LPPLOT(LODDUR,LPROB,PLOT_PTS,ISEAS,
     +      gc_end_point, CURRENT_Yr, BEFORE_LOAD_CURVE)
         ENDIF

         IF(gc_end_point >= 20 .AND. globecom_YEAR+get_BASE_YEAR() 
     + >= 2052) THEN
            VOID_INT2 = 0
         ENDIF

         procmeth_result=PROCMETH()
         IF(procmeth_result <= 3 .AND. .NOT. YES_GAS_MODEL_ONLY) THEN
            IF(DEBUG_ON()) then
                CALL write_scroll_line_RW('BOOTH',2)
            end if
       
            CALL DR_BOOTH(globecom_YEAR,ISEAS,FUELCOST(globecom_YEAR),
     + TENRG(globecom_YEAR),
     +   TMMBTUS(globecom_YEAR),
     +   VARCOST(globecom_YEAR),HOURS_INCREMENT,LPROB,LODDUR,PEAK,
     +   DEMAND,DX,
     +   MAINTENANCE_RATE,HEAT_RATE_FACTOR,
     +   IPNTX,
     +   SEASONAL_SYSTEM_CAP,
     +   AVE_RUNNING_RATE,RR_OBS,
     +   PEAK_MONTH(globecom_YEAR),REMAIN_ANN_ENRG,
     +   MAXIMUM_ANNUAL_CAPACITY,
     +   PERIOD_COUNTER,
     +   MMBTU_FUEL_BALANCE,
     +   FUEL_INVENTORY_ACTIVE,
     +   DISP_BTU_COST,USE_SECONDARY_FUEL,
     +   TRANSACTION_BUY_SPREAD,TRANSACTION_SELL_SPREAD,
     +   CNTR_WEIGHTED_CAPACITY,
     +   SERVICE_TRANSACTIONS_ACTIVE,
     +   WOLF_CREEK_ENRG_COST,
     +   BLENDED_BTU_COST,
     +   FUEL_INVENTORY_ID,
     +   POOLING_VARIABLE_COST_SWITCH,
     +   ANNUAL_BTUS_GOCN12,
     +   ANNUAL_BTUS_FOR_BTU_TAX_GOCN12,
     +   BTU_TAX_ACTIVE,
     +   TIE_GROUP_LIMIT)
            CALL DISPLAY_TIME
            CALL FLUSH(4)
            IF(DEBUG_ON()) CALL write_scroll_line_RW('EXIT BOOTH',2)
         ENDIF
         IF(gc_end_point >= 20 .AND. 
     + globecom_YEAR+get_BASE_YEAR() >= 2052) THEN
            VOID_INT2 = 0
         ENDIF


         IF( BASE_DATE + ISEAS >= HISTORICAL_PRODUCTION_DATE() .AND.
     +                                       INIT_FILE_IS_ACTIVE()) THEN
            VOID_INT2 = MON_MDS_CL_EXP_2_AC(ISEAS)
            CALL CALC_EL_MON_MDS_ASSET_CLASS(ISEAS)
         ENDIF


! 2/21/97. GAT. MOVED CAL_HOURLY_MC_AFTER_EL INSIDE DR_BOOTH.

         IF(WABASH_VALLEY) THEN
            ENERGY(globecom_YEAR) = ENERGY(globecom_YEAR) + 
     + DEMAND_AFTER_DSM_BEFORE_EL +
     +               DEMAND - DEMAND_AFTER_DSM_AFTER_EL
            PEAK_AFTER_DSM = PEAK_AFTER_DSM + PEAK - PEAK_AFTER_EL
            BASE_AFTER_DSM = PEAK_AFTER_DSM * BASE_TO_PEAK_RATIO
         ELSE
            ENERGY(globecom_YEAR) = ENERGY(globecom_YEAR) + 
     + DEMAND_AFTER_DSM_BEFORE_EL
         ENDIF
!
         ANNUAL_PEAK = MAX(ANNUAL_PEAK,PEAK_AFTER_DSM)
         ANNUAL_BASE = MIN(ANNUAL_BASE,BASE_AFTER_DSM)
         CAP_LIMITED_DEMAND = CAP_LIMITED_DEMAND + DEMAND
         IF(POOLING_TRANSACTIONS()) then
           CALL POOLING(globecom_YEAR,PSMO,PEMO,
     + ENERGY_EXCHANGE_ADJUSTMENT,
     + DEMAND_AFTER_DSM_BEFORE_EL,
     + POOL_PRICING_SWITCH,
     + POOL_NATIVE_MULT,
     + POOL_OTHER_MULT)

         endif

! 051609.

         CALL WRITE_STATE_RPS_REPORT(globecom_YEAR,ISEAS)
!
! MOVED FROM INSIDE ENRGLIMT FOR DYNAMIC STORAGE RECORD

         CALL ANNUAL_EL_REPORT

         IF(GAS_MODEL_ACTIVE) THEN
            TEMP_L = WRITE_MONTHLY_GAS_DEMAND_CLASS_SUMMARY(
     + globecom_YEAR,ISEAS)
      WRITE(SCREEN_MESSAGES,"(A,I3,A,I4,A,I5,A,I4,A)")
     +              'EndPt:',INT(PRT_ENDPOINT()),' ',CURRENT_Yr,
     +              ', '//trim(MONTH_NAME(ISEAS))//' ',
     +              NUM_GAS_NODES,'-gas nodes. ',
     +              NUM_GAS_LINKS,'-gas links.'
            CALL MG_LOCATE_WRITE(20,70,trim(SCREEN_MESSAGES),
     +                                                   ALL_VERSIONS,0)
         ENDIF
! 121006. MOVED BELOW WRITE_...
!         SO THAT THE GAS POWER DEMAND
!         CAN BE USED IN PRICING
! 121406. REDEFINED AS A FUNCTION

         if(gas_lp_active) then
            HH_ADJ = MONTHLY_HH_BASIS(globecom_YEAR,ISEAS)
            IF(HH_ONLY) THEN
      CALL PUT_A_FUEL_PRICE(globecom_YEAR,ISEAS,HH_VECTOR,HH_ADJ)
            ENDIF
            LOWER_48_BASIN_ADD = UPDATE_HH_PRICE(globecom_YEAR,ISEAS)
            GAS_DATA_AVAILABLE =
     + monthly_lp_gas_pricing(globecom_YEAR,ISEAS,
     + get_HOURS_IN_PERIOD(ISEAS),
     +                            HH_ADJ,LOWER_48_BASIN_ADD)

         ELSE ! 040109.
            GAS_DATA_AVAILABLE = .FALSE.
         endif
! 092310.
         VOID_LOGICAL = WRITE_MARGINAL_UNIT_REPORT()

         IF(SERVICE_TRANSACTIONS_ACTIVE) THEN
            CALL SERVICE_TRANS_CALCULATIONS(ISEAS,
     + globecom_YEAR,DATE1,DATE2)
            IF(.NOT. TESTING_PLAN .AND. MONTHLY_SERVICE_REPORT_ACTIVE)
     +                               CALL WRITE_TRANS_SEASON_TYPE(ISEAS)
         ENDIF
         ! INSIDE CAL_MARGINAL_COST
         VOID_LOGICAL = DEALLO_MONTH_MARG_COST_ARRAYS() 
         VOID_LOGICAL = END_MONTH_FUEL_DERIVATIVES(ISEAS,CURRENT_Yr)
      ENDDO !PRODUCTION_COSTING_PERIODS

!     9/5/95. GAT. MOVED FROM DR_BOOTH FOR WVPA.

! moved 4/11/10 to put convergenc check on reports written below.
      SYS_CO2_EMISSIONS = CALCULATE_CO2_EMISS_FOR_SYSTEM()
      TEMP_L4 = CHECK_FOR_GRX_ABATEMENT_CONVERG(0_2)
! ---- CAPEX
      IF(.NOT. SP_CAPEX_ACTIVE()) THEN
! ---- CAPEX
      IF(WABASH_VALLEY .AND.
     +                   INDEX('B,A',WABASH_POWER_COST_RPT()) /= 0) THEN
         CALL WABASH_ANNUAL_POWER_COSTS(globecom_YEAR,UNITNM)
      ENDIF

      IF(SERVICE_TRANSACTIONS_ACTIVE) THEN
         IF(.NOT. TESTING_PLAN .AND. MONTHLY_SERVICE_REPORT_ACTIVE)
     +                                      CALL WRITE_TRANS_ANNUAL_TYPE
      ENDIF
!
      if(gas_lp_active) then
       VOID_LOGICAL = GasModulePipeExpansion(gc_end_point,globecom_YEAR,
     +                                         GRX_ITERATIONS)
      Endif
! COAL LP ACTIVE
!
      IF(RUN_COAL_MODEL() .AND. CoalLPActive) THEN

         CALL COAL_DEMAND_EXPANSION_UNITS(globecom_YEAR,get_nunits(),
     +                                    ANNUAL_CL_UNIT_MMBTUS,
     +                                    ANNUAL_CL_UNIT_CAPACITY,
     +                                    ANNUAL_CL_UNIT_ENERGY)
         CALL COAL_DEMAND_TRANSFERRED_MIDAS(globecom_YEAR,
     +                                          GRX_ITERATIONS,
     +                                          ANNUAL_CL_UNIT_MMBTUS,
     +                                          ANNUAL_CL_UNIT_CAPACITY,
     +                                          ANNUAL_CL_UNIT_ENERGY)
         IF(RUN_COAL_MODEL() .AND. CoalLPActive) THEN
            COAL_MODEL_RAN_LAST_ITERATION = GRX_CONVERGED .OR.
     +                          GRX_ITERATIONS == GRX_MAXIMUM_ITERATIONS
            IF(.NOT. COAL_MODEL_RAN_LAST_ITERATION) THEN
                ! MUST RUN B4 LP because LP generates new scrubber list
                CALL RESET_SCRUBBERS_FROM_COAL_LP()  
            ENDIF
            CALL write_scroll_line_RW('Running Coal LP',4)
            CALL GregsCnwRoutine(get_BASE_YEAR(),globecom_YEAR,.TRUE.)

          CALL COAL_MODEL_REPORTS(CURRENT_Yr,globecom_YEAR,gc_end_point,
     +                              COAL_MODEL_RAN_LAST_ITERATION)
            COAL_MODEL_RAN_LAST_ITERATION = .TRUE.
         ENDIF

      ENDIF
!
! ADDED 10/3/92
      DO I = 0, MAX_REPORTING_GROUPS
         GROUP_CAPACITY(I) = ANN_EL_CAPACITY(I) + ANN_CL_CAPACITY(I)
      ENDDO

! PUT EL_SO2_ANNUAL TO TONS OR METRIC TONS

      IF(COUNTRY() == 'C') THEN
         DIVIDE_BY = 1000.
      ELSE
         DIVIDE_BY = 2000.
      ENDIF
      EL_SO2_ANNUAL = EL_SO2_ANNUAL/DIVIDE_BY
      DO ISEAS = 1, PRODUCTION_COSTING_PERIODS
         EL_SO2_PERIOD(ISEAS) = EL_SO2_PERIOD(ISEAS)/DIVIDE_BY
      ENDDO
      TOTAL_ENRG_LIMITED_VAR_COST = ENRG_LIMITED_VAR_COST(1) +
     +                              ENRG_LIMITED_VAR_COST(2) +
     +                              ENRG_LIMITED_VAR_COST(3) +
     +                              ENRG_LIMITED_VAR_COST(4)
      TOTAL_ENRG_LIMITED_FIXED_COST = ENRG_LIMITED_FIXED_COST(1) +
     +                                ENRG_LIMITED_FIXED_COST(2) +
     +                                ENRG_LIMITED_FIXED_COST(3) +
     +                                ENRG_LIMITED_FIXED_COST(4)
      IF(PROCMETH() /= 4) THEN
         IF(SERVICE_TRANSACTIONS_ACTIVE) THEN
            DATE1 = BASE_DATE + 1
            DATE2 = BASE_DATE + 12
            CALL ANNUAL_SERVICE_TRANSACTIONS(globecom_YEAR,ANNUAL_PEAK,
     +                                                      DATE1,DATE2)
            IF(.NOT. TESTING_PLAN .AND. REALLY_KEPCO .AND.
     +             INDEX('B,A',KEPCO_REPORTS()) /= 0)
     +                     CALL SERVICE_COST_ANNUAL_REPORT(REALLY_KEPCO)
         ENDIF
         CALL RETURN_P_SALES_REVENUE_ENERGY(CNTR_SALES_REVENUE,
     +                                      CNTR_SALES_ENERGY)
         TOTAL_SALES_REVENUE = ENRG_LIMITED_SALES_REVENUE(1) +
     +                         ENRG_LIMITED_SALES_REVENUE(2) +
     +                         ENRG_LIMITED_SALES_REVENUE(3) +
     +                         CNTR_SALES_REVENUE
         TOTAL_SALES_ENERGY = TOTAL_SALES_ENERGY + CNTR_SALES_ENERGY
         SALES_ENERGY_NOT_IN_FORECAST = SALES_ENERGY_NOT_IN_FORECAST +
     +                                                 CNTR_SALES_ENERGY
         IF( .NOT. TESTING_PLAN .AND. ((UNIT_OUTPUT_REPORT() .AND.
     +        PRODUCTION_COSTING_PERIODS /= 1) .OR.
     +                             ANNUAL_OR_SPREAD_SHEET_REPORT)) THEN
            IF(GRX_CONVERGED)CALL ANNUAL_REPORT(globecom_YEAR,
     +                             CAP_LIMITED_DEMAND,
     +                             ENERGY_EXCHANGE_ADJUSTMENT,
     +                             ANNUAL_BASE,ANNUAL_PEAK,
     +                             NUMBER_OF_CONTRACTS,CNTRNM,
     +                             CNTR_ON_LI,CNTR_OFF_LI,KEPCO,
     +                             CNTR_WEIGHTED_CAPACITY,
     +                             ANNUAL_BTUS_GOCN12,
     +                             WABASH_VALLEY,REALLY_KEPCO,
     +                             CNTR_SALES_ENERGY,CNTR_SALES_REVENUE,
     +                             SAVE_Yr)
            IF(SERVICE_TRANSACTIONS_ACTIVE .AND. .NOT. KEPCO) THEN
               IF(GRX_CONVERGED) CALL SERVICE_COST_ANNUAL_REPORT(KEPCO)
            ENDIF
         ENDIF
         IF(.NOT. TESTING_PLAN .AND. get_ENRG_LIMIT_REPORT())
     +             CALL ENRG_LIMITED_REPORT(EL_SO2_ANNUAL,EL_SO2_PERIOD)
      ENDIF

! LOLP REPORT

      IF(.NOT. TESTING_PLAN .AND. LOLP_REPORT() .AND. GRX_CONVERGED)
     +                CALL LOLP_MONTHLY_REPORT(gc_end_point,CURRENT_Yr)

! MOVED FROM CLREPORT.

      IF(GRX_CONVERGED) CALL MARKETSYM_REPORT(get_nunits())

! FEDERAL MMBTU TAX

      MMBTU_FEDERAL_TAX = 0.
      MMBTU_FEDERAL_TAX_IN_ADJ_CLAUSE = 0.
      IF(BTU_TAX_ACTIVE_DISP_RPT) THEN
         DO I = 1, 6
            MMBTU_FEDERAL_TAX = MMBTU_FEDERAL_TAX +
     +                          ANNUAL_BTUS_FOR_BTU_TAX_GOCN12(I) *
     +                           CAP_LIMT_BTU_TAX_RATES_GOCN12(I)/10.**6
         ENDDO
         DO I = 1, 3
            MMBTU_FEDERAL_TAX = MMBTU_FEDERAL_TAX +
     +                          ANNUAL_EL_MWH_FOR_BTU_TAX_H12(I) *
     +                          ENRG_LIMT_ENRGY_TAX_RATES_H12(I)/10.**6
         ENDDO
         MMBTU_FEDERAL_TAX_IN_ADJ_CLAUSE = MMBTU_FEDERAL_TAX *
     +                                 BTU_TAX_IN_ADJUSTMENT_CLAUSE/100.
      ENDIF

      IF(DUKE()) THEN
         IF(CATAWBA2_FILE_EXISTS) THEN
            IF(globecom_YEAR == 1) then
                VOID_LOGICAL = UPDATE_CATAWBA_CONTRACT_DATA()
            end if
            VOIDR4 = DUKE_TOTAL_CAPACITY_CHARGES(globecom_YEAR)
         ENDIF
      ENDIF
      IF(CPL_ACTIVE()) THEN
         PA_CONTRACT_ACITVE = .FALSE.
!
         CALL GET_PA_SWITCH(PA_CONTRACT_ACITVE)
         CALL GET_CPL_EMC_CAP_REVS
!
         IF(PA_CONTRACT_ACITVE) THEN
            CALL CPL_PA_ANN_ENERGY
            CALL UPDATE_CPL_CAP_CALCS
         ENDIF
         CALL CPL_PA_ANN_CAPACITY
      ENDIF


      LAST_NUNITS = get_nunits()
      PROCOST = SYSCAP
      globecom_YEAR = SAVE_Yr


      IF(INIT_FILE_IS_ACTIVE()) THEN
         ! This call must preceed the EL call
         VOID_INT2 = CL_EXPENSES_2_ASSET_CLASSES() 
         CALL CALC_EL_ANN_ASSET_CLASS
         CALL CALC_ST_ANN_ASSET_CLASS
         VOID_LOGICAL = CALC_CONTRACT_CLASS_INFO(CNTR_WEIGHTED_CAPACITY,
     +                                           REALLY_KEPCO)

         IF(GRX_CONVERGED) CALL WRITE_ASSET_PROD_REPORT
      ENDIF

      if(globecom_year+get_BASE_YEAR() == 2015) then
         temp_l4 = temp_l4
      endif
      TEMP_L = ANNUAL_CO2_RETIREMENTS_PROCESS(globecom_year)
      TEMP_L =  ANNUAL_CO2_RETROFIT_PROCESS(globecom_year)
      TEMP_L = ANNUAL_RETIRE_RETRO_PROCESS(globecom_year)
      TEMP_L = RPS_PROGRAMS_ANNUAL_CALC(globecom_year)
      TEMP_L = WRITE_HOURLY_STORAGE_REPORT()
      CALL ZERO_ITER_PRICE()
      IF(GRX_CONVERGED) then 
        CALL WRITE_STATE_RPS_REPORT(globecom_year,I2_ZERO)
      end if


      ! INSIDE CAL_MARGINAL_COST
      VOID_LOGICAL = DEALLO_MONTH_MARG_COST_ARRAYS() 

! 090706. TEST FOR AVOIDING REALLOCATION

      ! INSIDE CAL_MARGINAL_COST
      VOID_LOGICAL = DEALLO_ANNUAL_MARG_COSTS_ARRAYS() 

      VOID_LOGICAL = DEALLO_CAL_HOURLY_MC_AFTER_EL()
      VOID_LOGICAL = DEALLOCATE_MONTHLY_TRANS_LOADS()
      VOID_LOGICAL = DEALLOCATE_MONTHLY_GAS_DEMAND()
!
! ---- CAPEX
      ENDIF ! SP_CAPEX

      CALL DEALLOCATE_DISPAT_ARRAYS()

      RETURN
      ENTRY GET_PROCOST_LAST_NUNITS()
         IF(CAPACITY_PLANNING_METHOD() == 'MX'  .AND.
     +                                  GREEN_MRX_METHOD() == 'GX') THEN
            GET_PROCOST_LAST_NUNITS = GRX_NUNITS
         ELSE
            GET_PROCOST_LAST_NUNITS = LAST_NUNITS
         ENDIF
      RETURN

      ENTRY YES_GAS_MODEL_DATA_AVAILABLE()

         YES_GAS_MODEL_DATA_AVAILABLE = GAS_DATA_AVAILABLE
      RETURN

      ENTRY GET_HEAT_RATE_FACTOR(R_UNITS)

         GET_HEAT_RATE_FACTOR = HEAT_RATE_FACTOR(R_UNITS)
      RETURN

      ENTRY RECALCULATE_FUEL_COSTS(R_UNITS)

! EMISSIONS BLENDING ADDED 12/30/92
! MOVED FROM FUEL UNITS 9/27/94

         DO I = 1, R_UNITS
            IF(EMISS_BLENDING_RATE(I) > 0.) THEN
               BLENDED_BTU_COST(I) =
     +                           (1.-EMISS_BLENDING_RATE(I))*PBTUCT(I) +
     +                       EMISS_BLENDING_RATE(I) * EMISS_FUEL_COST(I)
            ELSE
               BLENDED_BTU_COST(I) = PBTUCT(I)
            ENDIF
            IF(ABS(FUELMX(I)) .NE. 1.) THEN
               FUEL_BTU_COST(I) = ABS(FUELMX(I)) *
     +             BLENDED_BTU_COST(I)+(1. - ABS(FUELMX(I))) * SBTUCT(I)
            ELSE
               FUEL_BTU_COST(I) = BLENDED_BTU_COST(I)
            ENDIF
            FUEL_ID = FUEL_SUPPLY_ID(I)
            IF(FUELMX(I) < 0. .OR.
     +              (FUEL_INVENTORY_ACTIVE .AND. FUEL_ID /= 0 .AND.
     +              MMBTU_FUEL_BALANCE(FUEL_INVENTORY_ID(FUEL_ID))
     +                                                      <=0.D0))THEN
               DISP_BTU_COST(I) = SBTUCT(I)
            ELSE
               DISP_BTU_COST(I) = FUEL_BTU_COST(I)
            ENDIF
         ENDDO
         RECALCULATE_FUEL_COSTS = R_UNITS
      RETURN
      ENTRY GET_THIS_YEARS_FUEL_SUPPLY_ID(R_UNIT,R_YR)

         FUEL_SUPPLY_ID_4_THIS_YEAR = FUEL_SUPPLY_ID(R_UNIT)
         IF(FUEL_SUPPLY_ID_4_THIS_YEAR < 0) THEN
            FUEL_SUPPLY_ID_4_THIS_YEAR =
     +         GET_VAR(FLOAT(
     +                FUEL_SUPPLY_ID_4_THIS_YEAR),R_YR,'FUEL SUPPLY ID')
         ENDIF
         GET_THIS_YEARS_FUEL_SUPPLY_ID = FUEL_SUPPLY_ID_4_THIS_YEAR
      RETURN

      ENTRY GET_ANNUAL_PEAK

         GET_ANNUAL_PEAK = ANNUAL_PEAK
      RETURN

      ENTRY GET_SYSTEM_CAPACITY

         GET_SYSTEM_CAPACITY = FLOAT(SYSCAP)
      RETURN

      ENTRY GET_TOTAL_SALES_ENERGY

         GET_TOTAL_SALES_ENERGY = TOTAL_SALES_ENERGY
      RETURN

      ENTRY GET_TOTAL_SALES_REVENUE

         GET_TOTAL_SALES_REVENUE = TOTAL_SALES_REVENUE
      RETURN

      ENTRY GET_SALES_ENRGY_NOT_IN_FORECAST

         GET_SALES_ENRGY_NOT_IN_FORECAST = SALES_ENERGY_NOT_IN_FORECAST
      RETURN

      ENTRY GET_ENRG_LIMITED_PURCHASE_ENRGY

         GET_ENRG_LIMITED_PURCHASE_ENRGY = ENRG_LIMITED_PURCHASE_ENERGY
      RETURN

      ENTRY GET_ENERGY_EXCHANGE_ADJUSTMENT

         GET_ENERGY_EXCHANGE_ADJUSTMENT = ENERGY_EXCHANGE_ADJUSTMENT
      RETURN

      ENTRY GET_MAINTENANCE_RATE(R_UNIT)

         GET_MAINTENANCE_RATE = MAINTENANCE_RATE(R_UNIT)
      RETURN

      ENTRY GET_FUEL_COST

         IF(globecom_YEAR > MAX_SIMULATION_YEARS) THEN
            GET_FUEL_COST = FUELCOST(MAX_SIMULATION_YEARS)
         ELSE
            GET_FUEL_COST = FUELCOST(globecom_YEAR)
         ENDIF
      RETURN

      ENTRY GET_VAR_COST

         IF(globecom_YEAR > MAX_SIMULATION_YEARS) THEN
            GET_VAR_COST = VARCOST(MAX_SIMULATION_YEARS)
         ELSE
            GET_VAR_COST = VARCOST(globecom_YEAR)
         ENDIF
      RETURN

      ENTRY GET_INCREMENTAL_FUEL_COST(R_UNIT,R_BLOCK)

         IF(R_BLOCK < 2) THEN
            GET_INCREMENTAL_FUEL_COST = 
     +                    BLENDED_BTU_COST(R_UNIT)*COEFF(1,R_UNIT)/1000.
         ELSEIF(MW(2,R_UNIT) - MW(1,R_UNIT) > 0.) THEN
            INC_CAP = MW(2,R_UNIT) - MW(1,R_UNIT)
            A_HR = (COEFF(3,R_UNIT) - COEFF(2,R_UNIT))/(2.*INC_CAP)
            B_HR = COEFF(3,R_UNIT) - 2.* A_HR * INC_CAP
            SECOND_BLOCK_HEAT = A_HR*INC_CAP + B_HR

            GET_INCREMENTAL_FUEL_COST =
     +                  BLENDED_BTU_COST(R_UNIT)*SECOND_BLOCK_HEAT/1000.
         ENDIF
      RETURN

      ENTRY GET_TOTAL_INCREMENTAL_COST(R_UNIT,R_BLOCK)

         GET_TOTAL_INCREMENTAL_COST = VCPMWH(R_UNIT) +
     +                                     FUEL_ADDER_ADJUSTMENT(R_UNIT)

         IF(R_UNIT < 1 .OR. R_UNIT > get_nunits()) THEN
            WRITE(4,*) "BAD UNIT INDEX IN GET_TOTAL_INCREMENTAL_COST"
            WRITE(4,*) "UNIT INDEX = ",R_UNIT
            RETURN
         ENDIF

! 06/27/05. FOR SRP

         IF(R_BLOCK == 2) THEN
            DISP_ADJUSTER = DISPADJ2(R_UNIT)
         ELSE
            DISP_ADJUSTER = DISPADJ(R_UNIT)
         ENDIF
         GET_TOTAL_INCREMENTAL_COST = GET_TOTAL_INCREMENTAL_COST +
     +                                                     DISP_ADJUSTER

         IF(COMMIT_ON_TOTAL_COST_ENV) THEN
            CALL GET_PRIM_TOTAL_EMISSION_COST(R_UNIT,
     +                                        PRIM_TOTAL_EMISSION_COST)
            GET_TOTAL_INCREMENTAL_COST = GET_TOTAL_INCREMENTAL_COST +
     +                                          PRIM_TOTAL_EMISSION_COST
         ENDIF

         TEMP_DISPATCH_MULT = DISPATCH_MULT(R_UNIT)
         IF(TEMP_DISPATCH_MULT < 0.) THEN
            get_var_called_from="pro_cost:3502"
            TEMP_DISPATCH_MULT =
     +            GET_VAR(TEMP_DISPATCH_MULT,globecom_YEAR,
     + UNITNM(R_UNIT))
         ENDIF

         IF(R_BLOCK < 2) THEN

            GET_TOTAL_INCREMENTAL_COST = GET_TOTAL_INCREMENTAL_COST +
     +             HEAT_RATE_FACTOR(R_UNIT) *
     +                    BLENDED_BTU_COST(R_UNIT)*COEFF(1,R_UNIT)/1000.
         ELSEIF(MW(2,R_UNIT) - MW(1,R_UNIT) > 0.) THEN
            INC_CAP = MW(2,R_UNIT) - MW(1,R_UNIT)
            A_HR = (COEFF(3,R_UNIT) - COEFF(2,R_UNIT))/(2.*INC_CAP)
            B_HR = COEFF(3,R_UNIT) - 2.* A_HR * INC_CAP
            SECOND_BLOCK_HEAT = A_HR*INC_CAP + B_HR

            GET_TOTAL_INCREMENTAL_COST =
     +               GET_TOTAL_INCREMENTAL_COST +
     +             HEAT_RATE_FACTOR(R_UNIT) *
     +                  BLENDED_BTU_COST(R_UNIT)*SECOND_BLOCK_HEAT/1000.
         ENDIF
         GET_TOTAL_INCREMENTAL_COST = TEMP_DISPATCH_MULT *
     +                                        GET_TOTAL_INCREMENTAL_COST
      RETURN

      ENTRY GET_TOTAL_INC_COST_PARAMS(R_UNIT,
     +                                R_BLENDED_BTU_COST,
     +                                R_1ST_FUEL,
     +                                R_1ST_NON_FUEL,
     +                                R_2ND_FUEL,
     +                                R_2ND_NON_FUEL)


         IF(R_UNIT < 1 .OR. R_UNIT > get_nunits()) THEN
            WRITE(4,*) "BAD INDEX IN GET_TOTAL_INC_COST_PARAMS"
            WRITE(4,*) "UNIT INDEX = ",R_UNIT
            RETURN
         ENDIF
         R_1ST_NON_FUEL = VCPMWH(R_UNIT) +
     +                                     FUEL_ADDER_ADJUSTMENT(R_UNIT)
         R_2ND_NON_FUEL = VCPMWH(R_UNIT) +
     +                                     FUEL_ADDER_ADJUSTMENT(R_UNIT)
         R_1ST_NON_FUEL = R_1ST_NON_FUEL + DISPADJ(R_UNIT)
         R_2ND_NON_FUEL = R_2ND_NON_FUEL + DISPADJ2(R_UNIT)

         IF(COMMIT_ON_TOTAL_COST_ENV) THEN
            CALL GET_PRIM_TOTAL_EMISSION_COST(R_UNIT,
     +                                        PRIM_TOTAL_EMISSION_COST)
            R_1ST_NON_FUEL = R_1ST_NON_FUEL + PRIM_TOTAL_EMISSION_COST
            R_2ND_NON_FUEL = R_2ND_NON_FUEL + PRIM_TOTAL_EMISSION_COST

         ENDIF

         R_1ST_FUEL = HEAT_RATE_FACTOR(R_UNIT) * COEFF(1,R_UNIT)/1000.
         IF(MW(2,R_UNIT) - MW(1,R_UNIT) < 0.00001) THEN
            R_2ND_FUEL = R_1ST_FUEL
         ELSE
            INC_CAP = MW(2,R_UNIT) - MW(1,R_UNIT)
            A_HR = (COEFF(3,R_UNIT) - COEFF(2,R_UNIT))/(2.*INC_CAP)
            B_HR = COEFF(3,R_UNIT) - 2.* A_HR * INC_CAP
            SECOND_BLOCK_HEAT = A_HR*INC_CAP + B_HR
            R_2ND_FUEL =
     +             HEAT_RATE_FACTOR(R_UNIT) * SECOND_BLOCK_HEAT/1000.
         ENDIF

         TEMP_DISPATCH_MULT = DISPATCH_MULT(R_UNIT)
         IF(TEMP_DISPATCH_MULT < 0.) THEN
            TEMP_DISPATCH_MULT =
     + GET_VAR(TEMP_DISPATCH_MULT,globecom_YEAR,UNITNM(R_UNIT))
         ENDIF

         R_1ST_NON_FUEL = R_1ST_NON_FUEL * TEMP_DISPATCH_MULT
         R_2ND_NON_FUEL = R_2ND_NON_FUEL * TEMP_DISPATCH_MULT
         R_1ST_FUEL = R_1ST_FUEL * TEMP_DISPATCH_MULT
         R_2ND_FUEL = R_2ND_FUEL * TEMP_DISPATCH_MULT

         R_BLENDED_BTU_COST = BLENDED_BTU_COST(R_UNIT)
         GET_TOTAL_INC_COST_PARAMS = R_BLENDED_BTU_COST

      RETURN

      ENTRY GET_DEMAND_AFTER_EL(R_ISEAS,R_PSMO,R_PEMO)

         R_ISEAS = ISEAS
         R_PSMO = PSMO
         R_PEMO = PEMO
         GET_DEMAND_AFTER_EL = DEMAND_AFTER_DSM_AFTER_EL
      RETURN

      ENTRY GET_THERMAL_ONLINE(R_UNIT)

         GET_THERMAL_ONLINE = ONLINE(R_UNIT)
      RETURN

      END function procost

!
!             SUBROUTINE TO FIND HEAT RATES FOR UNIT BLOCKS
!                   COPYRIGHT (C) 1990
!                        M.S. GERBER & ASSOCIATES, INC.
!                              ALL RIGHTS RESERVED
!

!
      SUBROUTINE HR_CALC(NUS)
      use enrg_helper
      use foshydi2com
      use mwunih
      use prod2com
      use prod_arrays_dimensions
      use prodcom
      use sizecom
      use spindriftlib
      use debugtrace
      implicit none
      SAVE
      INTEGER (kind=2) ::  LSTBLOK,NUS
!
! ADDED 12/29/92 FOR FUEL LIMITS M.S.G.
!
      LOGICAL (kind=1) ::  DISPATCH_ON_AVERAGE=.FALSE. 
      LOGICAL (kind=1) ::  ZERO_CAPACITY_ACTIVE_UNIT
      LOGICAL (kind=1) ::  DISPATCH_ON_TOTAL=.FALSE. 
      CHARACTER (len=1) ::  FIRST_BLOCK_DISP_SWITCH
      REAL (kind=4) ::  A_HR
      REAL (kind=4) ::  B_HR,FIRST_BLOCK_HEAT,SECOND_BLOCK_HEAT,INC_CAP
      integer, save :: file_trace_hrc=0
!
!
      if (file_trace_hrc==0) then
        file_trace_hrc=open_trace("hr_calc.trace", rq_hr_calc)
        call write_trace_message(file_trace_hrc, 
     + "Tracing hr_calc subroutine.")
      end if
      
      LSTBLOK = 0
! ZERO CAPACITY UNIT STUFF ADDED 10/25/96. GAT.

      IF(ONLINE(NUS) <= DATE2 .AND. OFLINE(NUS) >= DATE1 .AND.
     +                                     MW(2,NUS) == 0.) THEN
         ZERO_CAPACITY_ACTIVE_UNIT = .TRUE.
      ELSE
         ZERO_CAPACITY_ACTIVE_UNIT = .FALSE.
      ENDIF
!
      call write_trace_message(file_trace_hrc, 
     + "Checking MW, LDTYPE and ZERO_CAPACITY_ACTIVE_UNIT to " //
     + "determine whether to increment NBLOK")
      call write_trace_string(file_trace_hrc, "LDTYPE", LDTYPE(NUS))
      call write_trace_bool1(file_trace_hrc, 
     + "1. ZERO_CAPACITY_ACTIVE_UNIT", ZERO_CAPACITY_ACTIVE_UNIT)
      call write_trace_real(file_trace_hrc, "1. MW", MW(1,NUS))
      call write_trace_int2(file_trace_hrc, "NUS", NUS)
      
      IF (MW(1,NUS) > 0. .OR. LDTYPE(NUS) == 'L' .OR.
     +                                   ZERO_CAPACITY_ACTIVE_UNIT) THEN
         NBLOK = NBLOK + 1
         call write_trace_int2(file_trace_hrc, "1. NBLOK++", NBLOK)

         UNIT(NBLOK) = NUS
         BLKNO(NBLOK) = 1
         LSTBLOK = 1
         IF(LDTYPE(NUS) == 'M' .OR. LDTYPE(NUS) == 'N') BLKNO(NBLOK) = 0
         IF(DISPATCH_ON_AVERAGE) THEN
            INHEAT(NBLOK) = COEFF(1,NUS)
         ELSEIF(DISPATCH_ON_TOTAL) THEN
            IF(MW(2,NUS) > MW(1,NUS)) THEN
               INC_CAP = MW(2,NUS) - MW(1,NUS)
               FIRST_BLOCK_HEAT = MW(1,NUS) * COEFF(1,NUS)
               A_HR = (COEFF(3,NUS) - COEFF(2,NUS))/(2.*INC_CAP)
               B_HR = COEFF(3,NUS) - 2.* A_HR * INC_CAP
               SECOND_BLOCK_HEAT = A_HR*INC_CAP**2. + B_HR*INC_CAP
               INHEAT(NBLOK) = (FIRST_BLOCK_HEAT + SECOND_BLOCK_HEAT) /
     +                                                 (MW(2,NUS))
            ELSE
               INHEAT(NBLOK) = COEFF(1,NUS)
            ENDIF
         ELSE
            INHEAT(NBLOK) = COEFF(2,NUS)
         ENDIF
         FUEL_LIMITED(NBLOK) = ABS(FUEL_SUPPLY_ID(NUS))
      ENDIF
      call write_trace_message(file_trace_hrc, "2. Checking " //
     + "MW and ZERO_CAPACITY_ACTIVE_UNIT to see whether " // 
     + "to increment NBLOK")
       call write_trace_real(file_trace_hrc, "2. MW", MW(2,NUS))
       call write_trace_bool1(file_trace_hrc, 
     + "2. ZERO_CAPACITY_ACTIVE_UNIT", ZERO_CAPACITY_ACTIVE_UNIT)
     
      IF(MW(2,NUS) > MW(1,NUS) .OR. ZERO_CAPACITY_ACTIVE_UNIT) THEN
         NBLOK = NBLOK + 1
         LSTBLOK = LSTBLOK + 1

         BLKNO(NBLOK) = LSTBLOK
         UNIT(NBLOK) = NUS
         INHEAT(NBLOK) = COEFF(3,NUS)
         FUEL_LIMITED(NBLOK) = ABS(FUEL_SUPPLY_ID(NUS))
      ENDIF
      RETURN
      ENTRY SET_DISPATCH_AVERAGE_INC
         DISPATCH_ON_AVERAGE = FIRST_BLOCK_DISP_SWITCH() == 'A'
         DISPATCH_ON_TOTAL = FIRST_BLOCK_DISP_SWITCH() == 'F'
      RETURN
      END subroutine hr_calc

!     COPYRIGHT (C) 1992 M.S. GERBER & ASSOCIATES, INC. ALL RIGHT 
!     RESERVED
      SUBROUTINE CREATE_EMISS_BY_UNIT_BY_FUEL(nunits_loc)
      use program_state
      use eco
      use annual_cl_unit
      SAVE

      LOGICAL (kind=1) ::  PHASE_I,CALCULATE_NOX
      INTEGER (kind=2) ::  YR,UNIT_NO,NUNITS_LOC,I,J,L,R_ISEAS
      INTEGER (kind=2) ::  FUEL
      REAL ::  R_SOX,R_NOX1,R_NOX2,R_CO2,R_OTH2,R_OTH3
      CHARACTER (len=*) :: UNIT_NAME,FUEL_TYPE*1
      REAL ::  SO2
      REAL ::  I_SO2
      REAL ::  NOX_BLK1
      REAL ::  I_NOX_BLK1
      REAL ::  NOX_BLK2
      REAL ::  I_NOX_BLK2
      REAL ::  CO2
      REAL ::  I_CO2
      REAL ::  EMIS_OTH2
      REAL ::  I_EMIS_OTH2
      REAL ::  EMIS_OTH3
      REAL ::  I_EMIS_OTH3
      REAL ::  GET_VAR
      REAL ::  MIX_RATE
      REAL ::  STORE_MIX_RATE
      REAL ::  NOX_CONTROL_MULT
      REAL ::  SOX_CONTROL_MULT
      REAL ::  CO2_CONTROL_MULT
      REAL ::  HG_CONTROL_MULT
      REAL ::  OTHER3_CONTROL_MULT
!
      REAL ::  FUEL_MIX_UNIT(MAX_CL_UNITS),BLEND_MIX_UNIT(MAX_CL_UNITS)


         EMISSION_RATES = 0.
         FUEL_MIX_UNIT = 1.
         BLEND_MIX_UNIT = 0.
      RETURN

      ENTRY GET_SEASON_EMISSIONS_RATES(R_ISEAS,YR,UNIT_NAME,PHASE_I,
     +                                 SO2,I_SO2,NOX_BLK1,I_NOX_BLK1,
     +                                 NOX_BLK2,I_NOX_BLK2,
     +                                 CO2,I_CO2,
     +                                 EMIS_OTH2,I_EMIS_OTH2,
     +                                 EMIS_OTH3,I_EMIS_OTH3,
     +                                 UNIT_NO,FUEL_TYPE,
     +                                 CALCULATE_NOX,
     +                                 NOX_CONTROL_MULT,
     +                                 SOX_CONTROL_MULT,
     +                                 CO2_CONTROL_MULT,
     +                                 HG_CONTROL_MULT,
     +                                 OTHER3_CONTROL_MULT)

      IF(PHASE_I) THEN
         IF(I_SO2 < 0.) THEN
            get_var_called_from="pro_cost:3740"
            SO2 = GET_VAR(I_SO2,YR,UNIT_NAME)
            IF(SO2 < 0.) THEN ! 11/19/97. GAT. MONTHLY EMISSIONS
               get_var_called_from="pro_cost:3742"
               SO2 = GET_VAR(SO2,R_ISEAS,FUEL_TYPE)
            ENDIF
         ELSE
            SO2 = I_SO2
         ENDIF
! 093004
            SO2 = SO2 * SOX_CONTROL_MULT
      ELSE
         SO2 = 0.
      ENDIF
      IF(.NOT. CALCULATE_NOX) THEN
         NOX_BLK1 = 0.
         NOX_BLK2 = 0.
      ELSE
         IF(I_NOX_BLK1 < 0.) THEN
            NOX_BLK1 = GET_VAR(I_NOX_BLK1,YR,UNIT_NAME)
            IF(NOX_BLK1 < 0.) THEN ! 11/19/97. GAT. MONTHLY EMISSIONS
               NOX_BLK1 = GET_VAR(NOX_BLK1,R_ISEAS,FUEL_TYPE)
            ENDIF
         ELSE
            NOX_BLK1 = I_NOX_BLK1
         ENDIF
         IF(I_NOX_BLK2 < 0.) THEN
            NOX_BLK2 = GET_VAR(I_NOX_BLK2,YR,UNIT_NAME)
            IF(NOX_BLK2 < 0.) THEN ! 11/19/97. GAT. MONTHLY EMISSIONS
               NOX_BLK2 = GET_VAR(NOX_BLK2,R_ISEAS,FUEL_TYPE)
            ENDIF
         ELSE
            NOX_BLK2 = I_NOX_BLK2
         ENDIF

         NOX_BLK1 = NOX_BLK1 * NOX_CONTROL_MULT
         NOX_BLK2 = NOX_BLK2 * NOX_CONTROL_MULT
!
      ENDIF
      IF(I_CO2 < 0.) THEN
         CO2 = GET_VAR(I_CO2,YR,UNIT_NAME)
         IF(CO2 < 0.) THEN ! 11/19/97. GAT. MONTHLY EMISSIONS
            CO2 = GET_VAR(CO2,R_ISEAS,FUEL_TYPE)
         ENDIF
      ELSE
         CO2 = I_CO2
      ENDIF
      CO2 = CO2 * CO2_CONTROL_MULT
      IF(I_EMIS_OTH2 < 0.) THEN
         EMIS_OTH2 = GET_VAR(I_EMIS_OTH2,YR,UNIT_NAME)
         IF(EMIS_OTH2 < 0.) THEN ! 11/19/97. GAT. MONTHLY EMISSIONS
            EMIS_OTH2 = GET_VAR(EMIS_OTH2,R_ISEAS,FUEL_TYPE)
         ENDIF
      ELSE
         EMIS_OTH2 = I_EMIS_OTH2
      ENDIF
      EMIS_OTH2 = EMIS_OTH2 * HG_CONTROL_MULT
      IF(I_EMIS_OTH3 < 0.) THEN
         EMIS_OTH3 = GET_VAR(I_EMIS_OTH3,YR,UNIT_NAME)
         IF(EMIS_OTH3 < 0.) THEN ! 11/19/97. GAT. MONTHLY EMISSIONS
            EMIS_OTH3 = GET_VAR(EMIS_OTH3,R_ISEAS,FUEL_TYPE)
         ENDIF
      ELSE
         EMIS_OTH3 = I_EMIS_OTH3
      ENDIF
      EMIS_OTH3 = EMIS_OTH3 * OTHER3_CONTROL_MULT
      FUEL = INDEX('PSB',FUEL_TYPE)
      EMISSION_RATES(1,FUEL,UNIT_NO) = SO2
      EMISSION_RATES(2,FUEL,UNIT_NO) = NOX_BLK1
      EMISSION_RATES(3,FUEL,UNIT_NO) = NOX_BLK2
      EMISSION_RATES(4,FUEL,UNIT_NO) = CO2
      EMISSION_RATES(5,FUEL,UNIT_NO) = EMIS_OTH2
      EMISSION_RATES(6,FUEL,UNIT_NO) = EMIS_OTH3

      RETURN


      ENTRY RETURN_PRIM_EMISSION_RATES(UNIT_NO,R_SOX,R_NOX1,R_NOX2,
     +                                 R_CO2,R_OTH2,R_OTH3)
         R_SOX = EMISSION_RATES(1,1,UNIT_NO)
         R_NOX1 = EMISSION_RATES(2,1,UNIT_NO)
         R_NOX2 = EMISSION_RATES(3,1,UNIT_NO)
         R_CO2 = EMISSION_RATES(4,1,UNIT_NO)
         R_OTH2 = EMISSION_RATES(5,1,UNIT_NO)
         R_OTH3 = EMISSION_RATES(6,1,UNIT_NO)
      RETURN
      ENTRY RETURN_SEC_EMISSION_RATES(UNIT_NO,R_SOX,R_NOX1,R_NOX2,
     +                                R_CO2,R_OTH2,R_OTH3)
         R_SOX = EMISSION_RATES(1,2,UNIT_NO)
         R_NOX1 = EMISSION_RATES(2,2,UNIT_NO)
         R_NOX2 = EMISSION_RATES(3,2,UNIT_NO)
         R_CO2 = EMISSION_RATES(4,2,UNIT_NO)
         R_OTH2 = EMISSION_RATES(5,2,UNIT_NO)
         R_OTH3 = EMISSION_RATES(6,2,UNIT_NO)
      RETURN
      ENTRY RETURN_BLEND_EMISSION_RATES(UNIT_NO,R_SOX,R_NOX1,R_NOX2,
     +                                 R_CO2,R_OTH2,R_OTH3)
         R_SOX = EMISSION_RATES(1,3,UNIT_NO)
         R_NOX1 = EMISSION_RATES(2,3,UNIT_NO)
         R_NOX2 = EMISSION_RATES(3,3,UNIT_NO)
         R_CO2 = EMISSION_RATES(4,3,UNIT_NO)
         R_OTH2 = EMISSION_RATES(5,3,UNIT_NO)
         R_OTH3 = EMISSION_RATES(6,3,UNIT_NO)
      RETURN
      ENTRY STORE_FUEL_MIX(UNIT_NO,STORE_MIX_RATE)

         FUEL_MIX_UNIT(UNIT_NO) = ABS(STORE_MIX_RATE)

      RETURN
      ENTRY RETURN_FUEL_MIX(UNIT_NO,MIX_RATE)
         MIX_RATE = FUEL_MIX_UNIT(UNIT_NO)

      RETURN
      ENTRY STORE_BLENDING_MIX(UNIT_NO,MIX_RATE)
         BLEND_MIX_UNIT(UNIT_NO) = MIX_RATE
      RETURN
      ENTRY RETURN_BLENDING_MIX(UNIT_NO,MIX_RATE)
         MIX_RATE = BLEND_MIX_UNIT(UNIT_NO)
      RETURN

      END subroutine CREATE_EMISS_BY_UNIT_BY_FUEL

      SUBROUTINE UPDATE_PRODUCTION_SWITCHES

         use enrg_helper
         use service_decs
         implicit none
         CALL UPDATE_MONTHLY_EL_RPT_ACTIVE

         CALL UPDATE_SERVICE_REPORT_SWITCH

      RETURN
      END subroutine UPDATE_PRODUCTION_SWITCHES

      SUBROUTINE UPDATE_SEASON_FOR_REPORTS(ISEAS)
         use enrglimit
         implicit none
         INTEGER (kind=2) ::  ISEAS
         LOGICAL (kind=1) ::  VOID_L,UPDATE_SEASON_FOR_CT_RPT

         CALL UPDATE_SEASON_FOR_FUEL_RPT(ISEAS)

         CALL UPDATE_SEASON_FOR_EL_RPT(ISEAS)
 
! 083106. FOR SRP.

         VOID_L = UPDATE_SEASON_FOR_CT_RPT(ISEAS)

      RETURN

      END subroutine UPDATE_SEASON_FOR_REPORTS
