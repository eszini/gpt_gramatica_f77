! from clreport
      SUBROUTINE PERIOD_CL_REPORT(ISEAS,YR_loc,PENRG,
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
     +         EFFECTIVE_CAPACITY,MONTH_BTUS_GOCN12,
     +         MMBTU_FUEL_BALANCE,
     +         WABASH_VALLEY,REALLY_KEPCO,
     +         P_SALES_ENERGY,P_SALES_REVENUE,MAXIMUM_ENERGY,
     +         RATCHET_CAPACITY_BASIS,MIN_RATCHET_CAPACITY,
     +         REVENUE_GENERATING_CAPACITY,
     +         CL_UNIT_MONTHLY_FIXED_COST)

      use debugtrace
      use energy_trace
      use arrayallocationinterface
      use spcapexvariables
      use trans_group_variables
      use irec_endpoint_control
      use grx_planning_routines
      use icap_results
      use abb_capmarketrptdata
      use sizecom
      use prodcom
      use envircom
      use p_fuel_annl
      use annual_cl_unit
      use annual_contracts
      use capacity_arrays
      use cl_data
      use clreport_decs
      use clreport_routines
      use contracts_data
      use dr_booth_modules
      use dsex_data
      use eco
      use enrg_prod_costsr8
      use enrglimit
      use foshydi2com
      use prod2com
      use group_prod
      use hesi
      use icap_summary_report
      use logging
      use month_extra
      use monthly_icap
      use mwunih
      use p_fuel_annl
      use prod_arrays_dimensions
      use hesi_gendecs
      use production
      use rptreccontrol
      use spindriftlib
      use tf_objt_interfaces
      use pclr_shared
      use offline_trace
      use program_state


      implicit none
      integer (kind=2) :: imm_i
      integer, save :: file_trace_pclr=0
      integer (kind=2) :: yr_loc
      integer (kind=2) :: get_TRANSACTION_GROUP ! External
      logical (kind=1) :: YES_RUN_TRANSACT ! External
      logical (kind=1) :: GET_CL_BASECASE_MARKET_ID
      real :: tvalue1, tvalue2, tvalue3
! MARKETSYM TRANSFER VARIABLE
      TYPE MARKETSYM_TRANSFER_STRUCTURE
            CHARACTER (LEN=31) :: UnitName
            CHARACTER (LEN=20) :: TransZoneName
            INTEGER (KIND=2) :: UnitsBuilt
      END TYPE MARKETSYM_TRANSFER_STRUCTURE


      TYPE (MARKETSYM_TRANSFER_STRUCTURE) ::
     +  MarketSymAdditions(1000)
      INTEGER (KIND=2) ::  UnitsAdded
      INTEGER (KIND=2) ::  LastNunits
      INTEGER (KIND=2) ::  MSUnitsAdded
      INTEGER (KIND=2) ::  GET_PROCOST_LAST_NUNITS ! External
      LOGICAL (KIND=1), SAVE :: MarketSymFileOpen=.FALSE.
      INTEGER (KIND=4) :: MARKETSYM_REC
      LOGICAL (kind=1) ::  KEPCO
      LOGICAL (kind=1) ::  CONTRACTS_IN_PERIOD
      LOGICAL (kind=1) ::  WABASH_VALLEY
      LOGICAL (kind=1) ::  REALLY_KEPCO
      LOGICAL (kind=1) ::  GET_MONTHLY_TRANS_VARIABLES ! External
      LOGICAL (kind=1) ::  VOID_LOGICAL
      LOGICAL (kind=1) ::  MON_MDS_CL_VAR
      LOGICAL (kind=1) ::  FUEL_PRICE_DATA_AVAILABLE=.FALSE.
      LOGICAL (kind=1) ::  RUN_TRANSACT=.FALSE.
      LOGICAL (kind=1) ::  YES_RUN_MULTIAREA_TRANSACT
      LOGICAL (kind=1) ::  RUN_MULTIAREA_TRANSACT
      LOGICAL (kind=1) ::  YES_REPORT_PRODUCT
      LOGICAL (kind=1) ::  TEMP_L1

      LOGICAL (kind=1) ::  CALC_ANN_ICAP_VALUES
      LOGICAL (kind=1) ::  GET_TG_MONTH_SUM_B4_HYDRO
      LOGICAL (kind=1) ::  YES_DETAILED_TRANSFER_PRICING
      LOGICAL (kind=1) ::  R_DETAILED_TRANSFER_PRICING
      LOGICAL (kind=1) ::  TRANSFER_PRICING_VECTORS
      LOGICAL (kind=1) ::  DERIVATIVES_REPORT=.FALSE.
      LOGICAL (kind=1) ::  MONTHLY_DERIVATIVES_ACTIVE
      LOGICAL (kind=1) ::  SUPPRESS_DERIVATIVES_ACTIVE
      LOGICAL (kind=1) ::  SUPPRESS_DERIVATIVES=.FALSE.
      LOGICAL (kind=1) ::  NEW_UNITS_REPORT=.FALSE.
      LOGICAL (kind=1) ::  NEW_UNITS_REPORT_ACTIVE
      LOGICAL (kind=1) ::  CLA_SPECIAL_ID_NAME
      LOGICAL (kind=1) ::  GET_PORT_MARKET_AREA_ABBREV
      LOGICAL (kind=1) ::  GET_BASECASE_MARKET_AREA_ID
      LOGICAL (kind=1) ::  GET_CUBIC_HEAT_CURVE
      LOGICAL (kind=1) ::  GET_PW_UNIT_TEXT_FIELDS
      LOGICAL (kind=1) ::  MONTHLY_PW_REPORT
      LOGICAL (kind=1) ::  MONTHLY_NEW_PW_REPORT
      LOGICAL (kind=1) ::  YES_POWERWORLD_REPORT
      LOGICAL (kind=1) ::  YES_POWERWORLD_NEW_REPORT
      LOGICAL (kind=1) ::  YES_REFERENCE_CASE_REPORT
      LOGICAL (kind=1) ::  REFERENCE_CASE_REPORT
      LOGICAL (kind=1) ::  CapEx_Running
      LOGICAL (kind=1) ::  PW_REPORT_NOT_OPEN=.TRUE.
      LOGICAL (kind=1) ::  TRANSACT_PROD_REPORT
      LOGICAL (kind=1) ::  TRANS_NOT_ACTIVE
      LOGICAL (kind=1) ::  GET_TRANS_RPS_SUM
      LOGICAL (kind=1) ::  GET_GRX_UNIT_INFO
      LOGICAL (kind=1) ::  GET_DERIV_PRIM_MOVER_INDEX
      LOGICAL (kind=1) ::  GET_DERIV_ZONE_ID
      LOGICAL (kind=4) ::  FILE_OPENED
      CHARACTER (len=1) ::  QUOTE='"'
      CHARACTER (len=5) ::  GET_SCENAME
      CHARACTER (len=6) ::  MARKET_ID
      CHARACTER (len=6) ::  EIA_PLANT_CODE
      CHARACTER (len=6) ::  RDI_MARKET_AREA_NAME
      CHARACTER (len=6) ::  EV_TRANS_AREA_NAME
      CHARACTER (len=64) ::  FILE_NAME
      CHARACTER (len=256) ::  XML_FILE_DIRECTORY
      CHARACTER (len=1024) ::  PW_REC
      REAL ::  P_SALES_REVENUE,P_SALES_ENERGY
      REAL ::  ENERGY(2,MAX_CL_UNITS)
      REAL ::  BASE
      REAL ::  PEAK
      REAL ::  MONTHLY_ECONOMY_BOUGHT
      REAL ::  MONTHLY_ECONOMY_COST
      REAL ::  MONTHLY_ECONOMY_SOLD
      REAL ::  MONTHLY_ECONOMY_REVENUE
      REAL ::  MONTHLY_KEPCO_SALES
      REAL ::  MONTHLY_KEPCO_SALES_REV
      REAL ::  CAPACITY_BILLED_AT_MIN
      REAL ::  CAPACITY_BILLED_AT_MAX
      REAL ::  CONTRACT_BILLING_CAPACITY
      REAL ::  REPORTING_CAPACITY
      REAL ::  CL_UNIT_MONTHLY_FIXED_COST(*)
      REAL ::  GET_DUKE_RESERVE_CAPACITY
      REAL ::  GET_POWERDAT_PLANT_ID
      REAL ::  GET_Holding_Company_ID
      REAL ::  HESI_ID_NUM_4_UNIT
      REAL ::  HESI_SECOND_UNIT_ID_NUM
      REAL ::  POWERDAT_PLANT_ID
      REAL ::  P_FUEL_DELIVERY
      REAL ::  PBTUCT_SAVE
      REAL ::  GET_PBTUCT_SAVE
      REAL ::  A_CO
      REAL ::  B_CO
      REAL ::  C_CO
      REAL ::  D_CO
      REAL ::  FIXED_COST_PER_UNIT
      REAL ::  AVE_FUEL_MULT
      REAL ::  LOCAL_MW1
      REAL ::  LOCAL_MW2
      REAL ::  MONTHLY_MOR
      REAL ::  MONTHLY_FOR
      REAL ::  EV_DATA_SOURCE=2.
      REAL ::  EV_PLANNING_AREA=0.
      REAL ::  CAP_MARKET_REVENUE=0.
      REAL ::  CAPITAL_COST_OF_BUILD=0.
      REAL ::  MONTHLY_CAPITAL_COST_OF_BUILD
      REAL ::  UNIT_EBITDA=0.
      REAL ::  MONTHLY_EBITDA
      REAL ::  MONTHLY_CHARGING_ENERGY
      REAL ::  GET_CAP_MARKET_REVENUE
      REAL ::  TRANS_ON_LINE
      REAL ::  TRANS_OFF_LINE
      REAL ::  TRANS_UNIT_ID
      REAL ::  TRANS_ASSET_CLASS
      REAL ::  PRIMARY_HEAT
      REAL ::  PRIMARY_FUEL_COST
      REAL ::  SECONDARY_HEAT
      REAL ::  SECONDARY_FUEL_COST
      REAL ::  TEMP_R
      REAL ::  GET_TRANS_ITER_CAP_PERCENT
      CHARACTER (len=1) ::  COUNTRY
      LOGICAL (kind=1) ::  CANADA
      CHARACTER (len=20) ::  UNIT_NAME
      CHARACTER (len=22) ::  CL_SEGMENT_NAME
      CHARACTER (len=25) ::  CL_TRANS_NAME
      CHARACTER (len=20) ::  SPECIAL_ID_NAME
      CHARACTER (len=22) ::  TRANS_NAME
      CHARACTER (len=20) ::  PA_NAME
      CHARACTER (len=25) ::  CM_NAME

      CHARACTER (len=2) ::  CAPACITY_PLANNING_METHOD,LOAD_FILE_CHAR_EXT
      CHARACTER (len=6) ::  STATE_PROVINCE ! CHANGED 060206.
      CHARACTER (len=20) ::  TEMP_NAME
      LOGICAL (kind=1) ::  UNIT_OUTPUT_REPORT,MONTHLY_BLOCK_REPORT
      LOGICAL (kind=1) :: MONTHLY_SUMMARY_REPORT_ACTIVE
      LOGICAL (kind=1) :: MONTHLY_SUMMARY_REPORT
      INTEGER (kind=2) ::  ISEAS
      INTEGER (kind=2) ::  UNITNO
      INTEGER (kind=2) ::  I
      INTEGER (kind=2) ::  BLKNUM
      INTEGER (kind=2) ::  EM
      INTEGER (kind=2) ::  PEAK_MONTH
      INTEGER (kind=2) ::  FIRST_MONTH
      INTEGER (kind=2) ::  LAST_MONTH
      INTEGER (kind=2) ::  TG
      INTEGER (kind=2) ::  GET_PA_PEAK_MONTH
      INTEGER (kind=2) ::  LPM
      INTEGER (kind=2) ::  GET_CM_PEAK_MONTH
      INTEGER (kind=2) ::  MONTHS_ACTIVE(MAX_CL_UNITS)
      INTEGER (kind=2) ::  FISCAL_MONTHS_ACTIVE(MAX_CL_UNITS)
      INTEGER (kind=2) ::  GET_DUKE_RESERVE_CAP_NO
      INTEGER (kind=2) ::  DUKE_RESERVE_CAP_NO
      INTEGER (kind=2) ::  CLASS
      INTEGER (kind=2) ::  GET_ASSET_CLASS_NUM
      INTEGER (kind=2) ::  TRANS
      INTEGER (kind=2) ::  MON_CL_TRANS_UNIT_VAR_NUM=63
      INTEGER (kind=2) ::  MON_DV_TRANS_UNIT_VAR_NUM=63
      INTEGER (kind=2) ::  MON_NU_TRANS_UNIT_VAR_NUM=63
      INTEGER (kind=2) ::  SUM_ANNUAL
      INTEGER (kind=2) ::  MK
      INTEGER (kind=2) ::  MARKET_AREA_LOOKUP
      INTEGER (kind=2) ::  GET_STATE_PROVINCE_NAMES
      INTEGER (kind=2) ::  SP
      INTEGER (kind=2) ::  R_SP
      INTEGER (kind=2) ::  GET_UNIT_STATE_PROVINCE_INDEX
      INTEGER (kind=2) ::  GET_UNIT_GAS_REGION_INDEX
      INTEGER (kind=2) ::  R_ISEAS
      INTEGER (kind=2) ::  R_AC_REV_VECTOR
      INTEGER (kind=2) ::  R_AC_REV_ALLOC_VECTOR
      INTEGER (kind=2) ::  R_AC_COST_VECTOR
      INTEGER (kind=2) ::  R_AC_COST_ALLOC_VECTOR
      INTEGER (kind=2) ::  GET_PURCHASE_ASSET_CLASS_ID
      INTEGER (kind=2) ::  MONTH_UNIT_STARTS(MAX_CL_UNITS)
      INTEGER (kind=2) ::  ANNUAL_UNIT_STARTS(MAX_CL_UNITS)
      INTEGER (kind=2) ::  FISCAL_UNIT_STARTS(MAX_CL_UNITS)
      INTEGER (kind=2) ::  R_UNIT
      INTEGER (kind=2) ::  R_STARTS
      INTEGER (kind=2) ::  MONTHLY_STARTS
      INTEGER (kind=2) ::  Fscl_Ssn_loc
      INTEGER (kind=2) ::  FSCL_SEASON_RESET
      INTEGER (kind=2) ::  BEGIN_DATE
      INTEGER (kind=2) ::  MAX_ASSET_CLASSES=1
      INTEGER (kind=2) ::  R_CLASS
      INTEGER (kind=2) ::  PM
      INTEGER (kind=2) ::  GET_PRIMARY_MOVER_INDEX
      INTEGER (kind=2) ::  LM
      INTEGER (kind=2) ::  GET_PA_VALUE_FROM_TG ! External
      INTEGER (kind=2) ::  GET_CM_INDEX_FROM_TG ! External
      INTEGER (kind=2) ::  PA_INDEX
      INTEGER (kind=2) ::  CM_INDEX
      INTEGER (kind=2) ::  R_MONTH
      INTEGER (kind=2) ::  INSERVICE_STATUS
      INTEGER (kind=2) ::  LAST_RESOURCE_UPDATE_NO
      INTEGER (kind=2) ::  TEMP_I2
      INTEGER (kind=2) ::  GET_REGIONAL_CM_NAME
      INTEGER (kind=2) ::  ST_TG
      INTEGER (kind=2) ::  STATE_ID_LOOKUP
      INTEGER (kind=2) ::  RPS_STATE_VAR_NUM=12
      INTEGER (kind=2) ::  NUM_RESOURCE_RPS_VARS=14
      INTEGER (kind=2) ::  I2_VARIABLE_NUMBER
      INTEGER (kind=2) ::  GET_THERMAL_STATE_INDEX ! External

! new declarations
      REAL (KIND=4), ALLOCATABLE :: SaveMW2(:,:)
      REAL (KIND=4) :: VariableOMRate
!
      REAL (kind=4) ::  RTG
      REAL (kind=4) ::  RMK
      REAL (kind=4) ::  RFT
      REAL (kind=4) ::  RSP
      REAL (kind=4) ::  RPM
      REAL (kind=4) ::  CHARGING_ENERGY
      REAL (kind=4) ::  R_START_COSTS
      REAL (kind=4) ::  R_CAP
      REAL (kind=4) ::  R_ENERGY
      REAL (kind=4) ::  MONTH_UNIT_START_COSTS(MAX_CL_UNITS)
      REAL (kind=4) ::  ANNUAL_UNIT_START_COSTS(MAX_CL_UNITS)
      REAL (kind=4) ::  ANNUAL_CAPITAL_COST_OF_BUILD(MAX_CL_UNITS)
      REAL (kind=4) ::  ANNUAL_EBITDA(MAX_CL_UNITS)
      REAL (kind=4) ::  FISCAL_CAPITAL_COST_OF_BUILD(MAX_CL_UNITS)
      REAL (kind=4) ::  FISCAL_EBITDA(MAX_CL_UNITS)
      REAL (kind=4) ::  FISCAL_UNIT_START_COSTS(MAX_CL_UNITS)
      REAL (kind=4) ::  ANNUAL_UNIT_CAPACITY_REVENUE(MAX_CL_UNITS)
      REAL (kind=4) ::  MONTHLY_CAPACITY_REVENUE
      REAL (kind=4) ::  MONTHLY_START_COSTS
      REAL (kind=4) ::  GET_UNIT_START_UP_COSTS
      REAL (kind=4) ::  UNIT_START_UP_COSTS
      REAL (kind=4) ::  GET_TRANS_BASE_AFTER_EL
      REAL (kind=4) ::  GET_TRANS_PEAK_AFTER_EL
      REAL (kind=4) ::  GET_MONTHLY_TL_MWH
      REAL (kind=4) ::  TG_MWH
      REAL (kind=4) ::  TG_PEAK
      REAL (kind=4) ::  TG_BASE
      REAL (kind=4) ::  STATE_RPS_VARS(13)
      REAL (kind=4) ::  RESOURCE_RPS_VARS(17)
!

      REAL ::  SEAS_HOURS
      REAL ::  P_NUCLEAR_FUEL_COST
      REAL ::  CAPBLK
      REAL ::  ENRG
      REAL ::  BLK1_HEAT(MAX_CL_UNITS)
      REAL ::  BLK2_HEAT(MAX_CL_UNITS)
      REAL ::  MAINTENANCE_RATE(MAX_CL_UNITS)
      REAL ::  AVERAGE_HEATRATE
      REAL ::  AVERAGE_FUEL_COST_PER_MWH
      REAL ::  DAYS_IN_MONTH
      REAL (kind=8) ::  DEMAND
      REAL (kind=8) ::  PENRG
      REAL (kind=8) ::  PMMBTUS
      REAL (kind=8) ::  PFUELCST
      REAL (kind=8) ::  PVARCOST
      REAL (kind=8) ::  P_PUR_POWER_COST
      REAL (kind=8) ::  PFIXCOST
      REAL (kind=8) ::  P_PUR_ENRG
      REAL (kind=8) ::  FUEL_COST
      REAL (kind=8) ::  HEAT
      REAL (kind=8) ::  CNTR_ENRG
      REAL (kind=8) ::  MMBTU_FUEL_BALANCE(*)
      REAL ::  HEAT_CONVERSION,TONS_CONVERSION
      REAL ::  GET_HEAT_CONVERSION,GET_TONS_CONVERSION
      REAL ::  CONTRACT_ENERGY(MAX_CONTRACTS)
      REAL ::  CONTRACT_CAPACITY(MAX_CONTRACTS)
      REAL ::  MAXIMUM_CAPACITY(MAX_CONTRACTS)
      REAL ::  CNTR_EMISSIONS
      REAL ::  CNTR_MIN_FIXED_COST
      REAL ::  CNTR_MAX_FIXED_COST
      REAL ::  TOTAL_CONTRACT_COST
      REAL ::  MINIMUM_CAPACITY(MAX_CONTRACTS)
      REAL ::  MAXIMUM_ENERGY(MAX_CONTRACTS)
      REAL ::  R_SOX,R_NOX,R_CO2,R_OTH2,R_OTH3
      REAL ::  EFFECTIVE_CAPACITY(2,*)
      REAL (kind=4) ::  MONTHLY_TOTAL_EMISSIONS(5)
      REAL (kind=4) ::  UNIT_EMISSIONS(5)
      REAL (kind=4) ::  TEMP_SNGL
      REAL (kind=4) ::  EMISSION_COST_PER_TON(5)
      REAL (kind=4) ::  EMISSIONS_COST_BY_SP(:,:)
      REAL (kind=4) ::  ANNUAL_EMISSIONS_COST_BY_SP(:,:)
      REAL (kind=4) ::  MONTHLY_TOTAL_EMISSIONS_COST(5)
      REAL (kind=4) ::  ANNUAL_LOCAL_EMIS_COST(5)
      REAL (kind=4) ::  TOTAL_EMISSION_COSTS_BY_TYPE(5)
      REAL (kind=4) ::  MONTHLY_UNIT_EMISSIONS_COST(:,:)
      REAL (kind=4) ::  R_MONTHLY_UNIT_EMISSIONS_COST
      REAL (kind=4) ::  UNIT_EMISSIONS_COST(5)
      REAL (kind=4) ::  EMIS_DISPATCH_1
      REAL (kind=4) ::  EMIS_DISPATCH_2
      REAL (kind=4) ::  EMIS_DISPATCH_3
      REAL (kind=4) ::  EMIS_DISPATCH_4
      REAL (kind=4) ::  EMIS_DISPATCH_5
      REAL (kind=4) ::  TRANS_DISPATCH_EMIS_ADDER
      REAL (kind=4) ::  TRANS_CAP
      REAL (kind=4) ::  TRANS_VAR_EXP
      REAL (kind=4) ::  TRANS_VAR_MWH
      REAL (kind=4) ::  TRANS_FIX_EXP
      REAL (kind=4) ::  TRANS_REV
      REAL (kind=4) ::  TRANS_REV_MWH
      REAL (kind=4) ::  TRANS_HOURS
      REAL (kind=4) ::  PRODUCT_HOURS
      REAL (kind=4) ::  TRANS_STRIKES
      REAL (kind=4) ::  WHOLESALE_PRODUCTION_COST
      REAL (kind=4) ::  RETAIL_SALES
      REAL (kind=4) ::  MONTHLY_RETAIL_SALES=0.
      REAL (kind=4) ::  MONTHLY_WHOLE_COSTS=0.
      REAL (kind=4) ::  MONTHLY_WHOLESALE_FUEL_COSTS=0.
      REAL (kind=4) ::  MONTHLY_WHOLESALE_VOM_COSTS=0.
      REAL (kind=4) ::  MONTHLY_DERIV_WHOLESALE_COST=0.
      REAL (kind=4) ::  R_WHOLESALE_MARKET_COST_AC(0:12)
      REAL (kind=4) ::  R_WHOLESALE_MARKET_BUY_MWH_AC(0:12)
      REAL (kind=4) ::  R_MONTHLY_WHOLESALE_COST_AC(0:12)
      REAL (kind=4) ::  R_MONTHLY_WHOLE_FUEL_COST_AC(0:12)
      REAL (kind=4) ::  R_MONTHLY_WHOLE_VOM_COST_AC(0:12)
      REAL (kind=4) ::  R_WHOLESALE_MARKET_REV_AC(0:12)
      REAL (kind=4) ::  R_WHOLESALE_MARKET_SELL_MWH_AC(0:12)
      REAL (kind=4) ::  R_WHOLESALE_DERIV_COST_AC(0:12)
      REAL (kind=4) ::  R_WHOLESALE_MARKET_COST
      REAL (kind=4) ::  R_WHOLESALE_MARKET_BUY_MWH
      REAL (kind=4) ::  R_MONTHLY_WHOLESALE_COST
      REAL (kind=4) ::  R_MONTHLY_WHOLESALE_FUEL_COST
      REAL (kind=4) ::  R_MONTHLY_WHOLESALE_VOM_COST
      REAL (kind=4) ::  R_WHOLESALE_MARKET_REV
      REAL (kind=4) ::  R_WHOLESALE_MARKET_SELL_MWH
      REAL (kind=4) ::  WHOLESALE_MARKET_COST(0:12)
      REAL (kind=4) ::  WHOLESALE_MARKET_BUY_MWH(0:12)
      REAL (kind=4) ::  WHOLESALE_DERIV_COST(0:12)
      REAL (kind=4) ::  MONTHLY_WHOLESALE_COST(0:12,3)
      REAL (kind=4) ::   WHOLESALE_MARKET_REV(0:12)
      REAL (kind=4) ::  WHOLESALE_MARKET_SELL_MWH(0:12)
      REAL (kind=4) ::  ANNUAL_WHOLESALE_COST=0.
      REAL (kind=4) ::  ANNUAL_ECO_SALES=0.
      REAL (kind=4) ::  ANNUAL_ECO_SALES_ENRG=0.
      REAL (kind=4) ::  ANNUAL_ECO_PUCH=0.
      REAL (kind=4) ::  ANNUAL_ECO_PUCH_ENRG=0.
      REAL (kind=4) ::  UNECONOMIC_RATE
!
      REAL (kind=8) ::  MONTHLY_HEAT
      CHARACTER (len=20) ::  MONTH_NAME
      REAL (kind=8) ::  MONTH_BTUS_GOCN12(0:6)
      REAL (kind=4) ::  MONTHLY_TRANS_CAPACITY
      REAL (kind=4) ::  MONTHLY_TRANS_EFFECTIVE_CAP
      REAL (kind=4) ::  MONTHLY_TRANS_AVAIL_CAP
      REAL (kind=4) ::  MONTHLY_TRANS_ENERGY
      REAL (kind=4) ::  MONTHLY_TRANS_VAR_COST
      REAL (kind=4) ::  MONTHLY_UNIT_EMISSIONS(5)
      REAL (kind=4) ::  MONTHLY_TRANS_FIXED_COST
      REAL (kind=4) ::  MONTHLY_ECO_SALES=0.
      REAL (kind=4) ::  MONTHLY_ECO_SALES_ENRG=0.
      REAL (kind=4) ::  MONTHLY_DERIV_ECO_SALES=0.
      REAL (kind=4) ::  MONTHLY_DERIV_ECO_SALES_ENRG=0.
      REAL (kind=4) ::  MONTHLY_ECO_PUCH=0.
      REAL (kind=4) ::  MONTHLY_ECO_PUCH_ENRG=0.
      REAL (kind=4) ::  TOTAL_UNIT_COST
      REAL (kind=4) ::  AVE_REV_FROM_SALES
      REAL (kind=4) ::  REVENUE_GENERATING_CAPACITY(MAX_CL_UNITS)
      REAL (kind=4) ::  TRANS_EQUIV_AVAIL
      REAL (kind=4) ::  TRANS_CAP_FACTOR
      REAL (kind=4) ::  ANNUAL_REV_GEN_CAPACITY(MAX_CL_UNITS)
      REAL (kind=4) ::  FISCAL_REV_GEN_CAPACITY(MAX_CL_UNITS)
      REAL (kind=4) ::  ANNUAL_UNIT_EMISSIONS_COST(MAX_CL_UNITS,5)
      REAL (kind=4) ::  ANNUAL_EFFECTIVE_CAP(MAX_CL_UNITS)
      REAL (kind=4) ::  FISCAL_EFFECTIVE_CAP(MAX_CL_UNITS)
      REAL (kind=4) ::  ANNUAL_CAP(MAX_CL_UNITS)
      REAL (kind=4) ::  AllHoursMonth(MAX_CL_UNITS)
      REAL (kind=4) ::  CapHours(MAX_CL_UNITS)
      REAL (kind=4) ::  EffectiveCapHours(MAX_CL_UNITS)
      REAL (kind=4) ::  AvailHoursMonth(MAX_CL_UNITS)
      REAL (kind=4) ::  TransAllHoursMonth(:)
      REAL (kind=4) ::  TransCapHours(:)
      REAL (kind=4) ::  TransEffectiveCapHours(:)
      REAL (kind=4) ::  TransAvailHoursMonth(:)

      REAL (kind=4) ::  FISCAL_CAP(MAX_CL_UNITS)
      REAL (kind=4) ::  FISCAL_CL_UNIT_ENERGY(MAX_CL_UNITS)
      REAL (kind=4) ::  FISCAL_CL_UNIT_FUEL_COST(MAX_CL_UNITS)
      REAL (kind=4) ::  FISCAL_CL_UNIT_VAR_COST(MAX_CL_UNITS)
      REAL (kind=4) ::  FISCAL_CL_UNIT_FIXED_COST(MAX_CL_UNITS)
      REAL (kind=4) ::  ANNUAL_P_FUEL_CONSUMPTION(MAX_CL_UNITS)
      REAL (kind=4) ::  ANNUAL_S_FUEL_CONSUMPTION(MAX_CL_UNITS)
      REAL (kind=4) ::  ANNUAL_E_FUEL_CONSUMPTION(MAX_CL_UNITS)
      REAL (kind=4) ::  ANNUAL_WHOLESALE_PROD_COST(MAX_CL_UNITS)
      REAL (kind=4) ::  ANNUAL_NOX_SEASON_EMISSIONS(MAX_CL_UNITS)
      REAL (kind=4) ::  ANNUAL_NOX_SEASON_THERMAL(MAX_CL_UNITS)
      REAL (kind=4) ::  FISCAL_P_FUEL_CONSUMPTION(MAX_CL_UNITS)
      REAL (kind=4) ::  FISCAL_S_FUEL_CONSUMPTION(MAX_CL_UNITS)
      REAL (kind=4) ::  FISCAL_E_FUEL_CONSUMPTION(MAX_CL_UNITS)
      REAL (kind=4) ::  FISCAL_WHOLESALE_PROD_COST(MAX_CL_UNITS)
      REAL (kind=4) ::  FISCAL_NOX_SEASON_EMISSIONS(MAX_CL_UNITS)
      REAL (kind=4) ::  FISCAL_NOX_SEASON_THERMAL(MAX_CL_UNITS)
      REAL (kind=4) ::  FISCAL_CL_UNIT_EMISSIONS(5,MAX_CL_UNITS)
      REAL (kind=4) ::  FISCAL_UNIT_EMISSIONS_COST(MAX_CL_UNITS,5)
      REAL (kind=4) ::  FISCAL_ECO_SALES_REV_FROM(MAX_CL_UNITS)
      REAL (kind=4) ::  FISCAL_ECO_SALES_ENRG_FROM(MAX_CL_UNITS)
      REAL (kind=4) ::  FISCAL_ECO_PUCH_COST_FROM(MAX_CL_UNITS)
      REAL (kind=4) ::  FISCAL_ECO_PUCH_ENRG_FROM(MAX_CL_UNITS)
      REAL (kind=4) ::  MONTHLY_P_HEAT
      REAL (kind=4) ::  MONTHLY_S_HEAT
      REAL (kind=4) ::  MONTHLY_E_HEAT
      REAL (kind=8) ::  MONTHLY_TRANS_HEAT
      REAL (kind=8) ::  MONTHLY_TRANS_FUEL_COST
      REAL (kind=8) ::  FISCAL_CL_UNIT_MMBTUS(MAX_CL_UNITS)


! NDIM DETAILED REPORTS. 12/2/93.
      LOGICAL (kind=1) ::  MON_CL_REPORT_NOT_OPEN=.TRUE.
      LOGICAL (kind=1) ::  MON_CT_REPORT_NOT_OPEN=.TRUE.
      LOGICAL (kind=1) ::  TRANS_REPORT_NOT_OPEN=.TRUE.
      LOGICAL (kind=1) ::  MON_SUMMARY_REPORT_NOT_OPEN=.TRUE.
      LOGICAL (kind=1) ::  MONTHLY_CONTRACT_REPORT_ACTIVE
      LOGICAL (kind=1) ::  ANNUAL_CONTRACT_REPORT_ACTIVE
      LOGICAL (kind=1) ::  TRANS_GROUP_REPORTING_ACTIVE(:)
      LOGICAL (kind=1) ::  REPORT_MARKET_AREA
      LOGICAL (kind=1) ::  GET_REPORT_CL_CAPACITY
      LOGICAL (kind=1) ::  REPORT_THIS_CL_UNIT
      ALLOCATABLE :: TRANS_GROUP_REPORTING_ACTIVE
!
      SAVE           TRANS_GROUP_REPORTING_ACTIVE,
     +               ANNUAL_UNIT_EMISSIONS_COST,
     +               FISCAL_UNIT_EMISSIONS_COST,
     +               ANNUAL_UNIT_STARTS,
     +               FISCAL_UNIT_STARTS,
     +               ANNUAL_CAPITAL_COST_OF_BUILD,
     +               ANNUAL_EBITDA,
     +               FISCAL_CAPITAL_COST_OF_BUILD,
     +               FISCAL_EBITDA,
     +               ANNUAL_UNIT_START_COSTS,
     +               ANNUAL_UNIT_CAPACITY_REVENUE,
     +               FISCAL_UNIT_START_COSTS

      CHARACTER (len=1) ::  CONTRACT_REPORT
      INTEGER (kind=2) ::  MON_CL_UNIT_NO
      INTEGER (kind=2) ::  MON_CL_UNIT_HEADER
      INTEGER (kind=2) ::  MON_CL_TRANS_UNIT_NO
      INTEGER (kind=2) ::  MON_CL_TRANS_UNIT_HEADER
      INTEGER (kind=2) ::  MON_DV_TRANS_UNIT_NO
      INTEGER (kind=2) ::  MON_DV_TRANS_UNIT_HEADER
      INTEGER (kind=2) ::  MON_NU_TRANS_UNIT_NO
      INTEGER (kind=2) ::  MON_NU_TRANS_UNIT_HEADER
      INTEGER (kind=2) ::  MON_CL_SUM_NO
      INTEGER (kind=2) ::  MON_CL_SUMMARY_HEADER
      INTEGER (kind=2) ::  MON_CT_UNIT_NO
      INTEGER (kind=2) ::  MON_CT_UNIT_HEADER

      INTEGER (kind=2) ::  PRODUCTION_PERIODS
      INTEGER (kind=2) ::  MAX_TRANS_GROUP_NUMBER=0
      INTEGER (kind=2) ::  GET_MAX_TRANS_GROUP_NUMBER

      INTEGER (kind=2) ::  TP
      INTEGER (kind=2) ::  TRANS_FUEL_VARIABLES
      INTEGER (kind=2) ::  TRANS_PROD_VARIABLES
      INTEGER ::  CL_TRANS_UNIT_REC_LEN
      INTEGER ::  DV_TRANS_UNIT_REC_LEN
      INTEGER ::  NU_TRANS_UNIT_REC_LEN
      INTEGER ::  MON_PW_TRANS_UNIT_REC=0
      REAL ::  N_A=-999999.
      REAL ::  TOTAL_PRODUCTION_COSTS
      REAL ::  AVERAGE_PRODUCTION_COSTS
      REAL ::  DIVIDE_BY_ENRG
      REAL ::  AVERAGE_TOTAL_COST
      REAL ::  MONTHLY_REPORTING_CAPACITY
      REAL ::  MONTHLY_CONTRACT_CAPACITY
      REAL ::  MONTHLY_MINIMUM_CAPACITY
      REAL ::  MONTHLY_ENRG
      REAL ::  MONTHLY_CONTRACT_VARIABLE_COST
      REAL ::  MONTHLY_CONTRACT_AVE_VAR_COST
      REAL ::  MONTHLY_CNTR_MAX_FIXED_COST
      REAL ::  MONTHLY_CNTR_MIN_FIXED_COST
      REAL ::  MONTHLY_TOTAL_FIXED_COST
      REAL ::  MONTHLY_TOTAL_CONTRACT_COST
      REAL ::  MONTHLY_AVERAGE_TOTAL_COST
      REAL ::  MONTHLY_CNTR_EMISSIONS
      REAL ::  ANNUAL_REPORTING_CAPACITY
      REAL ::  ANNUAL_CNTR_CAPACITY
      REAL ::  ANNUAL_MINIMUM_CAPACITY
      REAL ::  ANNUAL_ENRG
      REAL ::  ANNUAL_CNTR_VARIABLE_COST
      REAL ::  ANNUAL_CONTRACT_AVE_VAR_COST
      REAL ::  ANNUAL_CNTR_MAX_FIXED_COST
      REAL ::  ANNUAL_CNTR_MIN_FIXED_COST
      REAL ::  ANNUAL_TOTAL_FIXED_COST
      REAL ::  ANNUAL_TOTAL_CONTRACT_COST
      REAL ::  ANNUAL_AVERAGE_TOTAL_COST
      REAL ::  ANNUAL_CNTR_EMISSIONS
      REAL ::  TRANS_TOTAL_GROSS_MARGIN
      REAL ::  R_TRANS_GROSS_MARGIN
      REAL ::  REAL_FUEL_COST
      REAL ::  VOM_COST
      REAL ::  UNIT_GROSS_MARGIN
      REAL ::  SAVE_ANNUAL_GROSS_MARGIN
      REAL ::  INIT_ANNUAL_GROSS_MARGIN
      REAL ::  TEMP_R4
      REAL ::  CO2_COST
      REAL ::  CAP_COST


!
      CHARACTER (len=9) ::  CL_MONTH_NAME(14)
     +                         /'January  ','February ',
     +                          'March    ','April    ',
     +                          'May      ','June     ',
     +                          'July     ','August   ',
     +                          'September','October  ',
     +                          'November ','December ',
     +                          'Annual   ','Fiscal Yr'/
      CHARACTER (len=3) ::  THREE_CHAR_MONTH_NAMES
      CHARACTER (len=3) ::  PA_CHAR_NUM
      CHARACTER (len=3) ::  CM_CHAR_NUM
      SAVE     MON_CL_UNIT_NO,
     +         MON_CL_TRANS_UNIT_NO,
     +         MON_DV_TRANS_UNIT_NO,
     +         MON_NU_TRANS_UNIT_NO,
     +         MON_CL_SUM_NO,
     +         CL_MONTH_NAME,
     +         MON_CT_UNIT_NO,
     +         ANNUAL_REPORTING_CAPACITY,
     +         ANNUAL_CNTR_CAPACITY,
     +         ANNUAL_MINIMUM_CAPACITY,
     +         ANNUAL_ENRG,
     +         ANNUAL_CNTR_VARIABLE_COST,
     +         ANNUAL_CONTRACT_AVE_VAR_COST,
     +         ANNUAL_CNTR_MAX_FIXED_COST,
     +         ANNUAL_CNTR_MIN_FIXED_COST,
     +         ANNUAL_TOTAL_FIXED_COST,
     +         ANNUAL_TOTAL_CONTRACT_COST,
     +         ANNUAL_AVERAGE_TOTAL_COST,
     +         ANNUAL_CNTR_EMISSIONS,
     +         ANNUAL_REV_GEN_CAPACITY,
     +         FISCAL_REV_GEN_CAPACITY,
     +         ANNUAL_EFFECTIVE_CAP,
     +         FISCAL_EFFECTIVE_CAP,
     +         MONTHS_ACTIVE,
     +         FISCAL_MONTHS_ACTIVE,
     +         ANNUAL_CAP,
     +         AllHoursMonth,
     +         CapHours,
     +         EffectiveCapHours,
     +         AvailHoursMonth,
     +         FISCAL_CAP,
     +         FISCAL_CL_UNIT_ENERGY,
     +         FISCAL_CL_UNIT_MMBTUS,
     +         FISCAL_CL_UNIT_VAR_COST,
     +         FISCAL_CL_UNIT_FIXED_COST,
     +         FISCAL_CL_UNIT_FUEL_COST,
     +         ANNUAL_P_FUEL_CONSUMPTION,
     +         ANNUAL_S_FUEL_CONSUMPTION,
     +         ANNUAL_E_FUEL_CONSUMPTION,
     +         ANNUAL_WHOLESALE_PROD_COST,
     +         ANNUAL_NOX_SEASON_EMISSIONS,
     +         ANNUAL_NOX_SEASON_THERMAL,
     +         FISCAL_P_FUEL_CONSUMPTION,
     +         FISCAL_S_FUEL_CONSUMPTION,
     +         FISCAL_E_FUEL_CONSUMPTION,
     +         FISCAL_WHOLESALE_PROD_COST,
     +         FISCAL_NOX_SEASON_EMISSIONS,
     +         FISCAL_NOX_SEASON_THERMAL
      INTEGER ::  MON_CT_UNIT_REC
      INTEGER ::  MON_CL_SUM_REC
      INTEGER ::  MON_CL_UNIT_REC
      INTEGER ::  MON_CL_TRANS_UNIT_REC
      INTEGER ::  MON_DV_TRANS_UNIT_REC
      INTEGER ::  MON_NU_TRANS_UNIT_REC
      INTEGER ::  TRANS_FUEL_REC
      INTEGER ::  TRANS_STATE_REC
      INTEGER ::  TRAN_PROD_FUEL_REC
      SAVE MON_CT_UNIT_REC,MON_CL_SUM_REC,MON_CL_UNIT_REC,
     +     MON_CL_TRANS_UNIT_REC,
     +     MON_DV_TRANS_UNIT_REC,
     +     MON_NU_TRANS_UNIT_REC,
     +     TRANS_FUEL_REC,
     +     TRAN_PROD_FUEL_REC


      REAL ::  RATCHET_CAPACITY_BASIS(NUMBER_OF_CONTRACTS)
      REAL ::  MIN_RATCHET_CAPACITY(NUMBER_OF_CONTRACTS)
!

!
      REAL ::  FUEL_MIX_PRIM(MAX_CL_UNITS)
!

!
!
      CHARACTER (len=35) ::  MULTI_AREA_NAME(:), 
     +     GET_GROUP_NAME ! External
      real (kind=4) :: temp_year
      integer (kind=2), parameter :: MAX_LOAD_TYPES=4, MFT1=16
      ! Externals TODO: Convert to routines.
      integer (kind=2) :: RETURN_CL_UNITS_B4_ADDITIONS,
     + TRANSACT_FUEL_RPT_HEADER, TRANSACT_STATE_RPT_HEADER,
     + TRANSACT_PROD_RPT_HEADER, TRAN_PROD_FUEL_RPT_HEADER,
     + GET_MAX_STATE_PROVINCE_NO, GET_NUM_SCENARIO_TRANSACTIONS,
     + GET_NUM_ANNUAL_TRANSACTIONS, GET_NUM_FISCAL_TRANSACTIONS,
     + GET_TRANS_STATE_INDEX, GET_TRANS_GROUP_INDEX, GET_PRIMARY_MOVER,
     + GET_CM_FROM_TG, GET_TRANS_GROUP_POSITION, R_MK
      INTEGER (kind=2), save ::  J
      INTEGER (kind=2), save ::  K


      INTEGER (kind=2) ::  TFT
      INTEGER (kind=2) ::  TRANS_FUEL_NO=0

      INTEGER (kind=2) ::  TRANS_STATE_NO=0

      INTEGER (kind=2) ::  MAX_STATE_PROVINCE_NO=0

      INTEGER (kind=2) ::  MAX_GROUP_VAR=17
      INTEGER (kind=2) ::  MAX_GAS_REGION_NO=500
      INTEGER (kind=2) ::  GSP

      INTEGER (kind=2) ::  TRAN_PROD_FUEL_NO=0
      INTEGER (kind=2) ::  FT

      INTEGER (kind=2) ::  SD
      INTEGER (kind=2) ::  TG_POSITION

      INTEGER (kind=2) ::  CL_UNITS_B4_ADDITIONS

      INTEGER (kind=2) ::  NUM_MONTHLY_TRANSACTIONS
      INTEGER (kind=2) ::  GET_NUM_MONTHLY_TRANSACTIONS

      INTEGER (kind=2) ::  NUM_TRANSACTIONS=0

! 061207. max_prod_types_clr: ADDED STORAGE AS 18
! 112220. ADDED DG POSITION 19.
! 112920. MFT1=14 FOR DG (?).

      INTEGER (kind=4) ::  VALUES_2_ZERO
      ! ACCUMULATE BY TRANSACTION GROUP AND FUEL TYPE
      real, allocatable ::
     +   ANNUAL_FUEL_COST_BY_TG_BY_FUEL(:,:) 
      REAL ::  P_FUEL_CONSUMPTION
      REAL ::  S_FUEL_CONSUMPTION
      REAL ::  E_FUEL_CONSUMPTION
      REAL ::  SINGLE_FUEL_COST
      REAL ::  RETURN_PROD_BY_MK_BY_MWH(3)
      REAL ::  R_NG_BTUS_BY_GSP_BY_FUEL(3)
      REAL ::  BLOCK_FUEL_COST(2,MAX_CL_UNITS)
      REAL ::  FUEL_COST_BY_SP_BY_FUEL(:,:)
     + ! ACCUMULATE BY TRANSACTION GROUP AND FUEL TYPE
      REAL ::  ANNUAL_FUEL_COST_BY_SP_BY_FUEL(:,:)
      REAL ::   GROUP_VAR_BY_SP(:,:)
     + ! ACCUMULATE BY TRANSACTION GROUP AND FUEL TYPE
      REAL ::  ANNUAL_GROUP_VAR_BY_SP(:,:)
      REAL ::   FUEL_COST_BY_TG_BY_FUEL(:,:)
      REAL ::  PROD_BY_MK_BY_MWH(:,:)
     + ! ACCUMULATE BY TRANSACTION GROUP AND PROD TYPE
      REAL ::  ANNUAL_PROD_BY_MK_BY_MWH(:,:)
      REAL ::   FUEL_BTUS_BY_TG_BY_FUEL(:,:)
      REAL ::  ANNUAL_FUEL_BTUS_BY_TG_BY_FUEL(:,:)
      REAL ::   FUEL_BTUS_BY_SP_BY_FUEL(:,:)
     + ! ACCUMULATE BY TRANSACTION GROUP AND FUEL TYPE
      REAL ::  ANNUAL_FUEL_BTUS_BY_SP_BY_FUEL(:,:)
      REAL ::   FUEL_BTUS_BY_GSP_BY_FUEL(:,:)
      REAL ::  ANNUAL_FUEL_BTUS_BY_GSP_BY_FUEL(:,:)
      REAL ::  FUEL_COST_PER_MMBTU(:)
      REAL ::  PRIM_MOVER_CONVERSION(max_fuel_types_clr)


     +                /0.04545455,0.970874,0.1724138,
     +                 0.000025,1.0,1.0/,
     +     FUEL_PRICE_CONVERSION,
     +     CAP_MARKET_RATE,CAP_MARKET_FACTOR
      REAL ::  PRIM_HEAT,SEC_HEAT,EMIS_HEAT
      ALLOCATABLE ::
     +     MULTI_AREA_NAME,
     +     EMISSIONS_COST_BY_SP,
     +     ANNUAL_EMISSIONS_COST_BY_SP,
     +     FUEL_COST_PER_MMBTU,
     +     FUEL_COST_BY_SP_BY_FUEL,
     +     ANNUAL_FUEL_COST_BY_SP_BY_FUEL,
     +     GROUP_VAR_BY_SP,
     +     ANNUAL_GROUP_VAR_BY_SP,
     +     FUEL_COST_BY_TG_BY_FUEL,
     +     PROD_BY_MK_BY_MWH,
     + ! ACCUMULATE BY TRANSACTION GROUP AND PROD TYPE
     +     ANNUAL_PROD_BY_MK_BY_MWH,
     +     FUEL_BTUS_BY_TG_BY_FUEL,
     +     ANNUAL_FUEL_BTUS_BY_TG_BY_FUEL,
     +     FUEL_BTUS_BY_SP_BY_FUEL,
     +     ANNUAL_FUEL_BTUS_BY_SP_BY_FUEL,
     +     FUEL_BTUS_BY_GSP_BY_FUEL,
     +     ANNUAL_FUEL_BTUS_BY_GSP_BY_FUEL,
     +     MONTHLY_UNIT_EMISSIONS_COST,
     +     TransAllHoursMonth,
     +     TransCapHours,
     +     TransEffectiveCapHours,
     +     TransAvailHoursMonth
      SAVE MULTI_AREA_NAME,
     +     EMISSIONS_COST_BY_SP,
     +     ANNUAL_EMISSIONS_COST_BY_SP,
     +     FUEL_COST_PER_MMBTU,
     +     FUEL_COST_BY_SP_BY_FUEL,
     +     ANNUAL_FUEL_COST_BY_SP_BY_FUEL,
     +     GROUP_VAR_BY_SP,
     +     ANNUAL_GROUP_VAR_BY_SP,
     +     FUEL_COST_BY_TG_BY_FUEL,
     +     ANNUAL_FUEL_COST_BY_TG_BY_FUEL,
     +     PROD_BY_MK_BY_MWH,
     + ! ACCUMULATE BY TRANSACTION GROUP AND PROD TYPE
     +     ANNUAL_PROD_BY_MK_BY_MWH,
     +     FUEL_BTUS_BY_TG_BY_FUEL,
     +     ANNUAL_FUEL_BTUS_BY_TG_BY_FUEL,
     +     FUEL_BTUS_BY_SP_BY_FUEL,
     +     ANNUAL_FUEL_BTUS_BY_SP_BY_FUEL,
     +     FUEL_BTUS_BY_GSP_BY_FUEL,
     +     ANNUAL_FUEL_BTUS_BY_GSP_BY_FUEL

      LOGICAL (kind=1) ::  CAP_MARKET_ACTIVE,CAP_MARKET_SALE
      LOGICAL (kind=1) ::  MONTHLY_TRANS_REPORT
      LOGICAL (kind=1) ::  ANNUAL_TRANS_REPORT
      LOGICAL (kind=1) ::  TRANSACT_UNIT_MONTHLY_REPORT
      LOGICAL (kind=1) ::  TRANSACT_UNIT_ANNUAL_REPORT
      LOGICAL (kind=1) ::  TRANSACT_FUEL_REPORT_ACTIVE
      LOGICAL (kind=1) ::  TRANSACT_FUEL_REPORT
      LOGICAL (kind=1) ::  TRANS_FUEL_REPORT_NOT_OPEN=.TRUE.
      LOGICAL (kind=1) ::  TRANS_PROD_REPORT_NOT_OPEN=.TRUE.
      LOGICAL (kind=1) ::  DERIVATIVES_REPORT_NOT_OPEN=.TRUE.
      LOGICAL (kind=1) ::  NEW_UNITS_REPORT_NOT_OPEN=.TRUE.

      REAL (kind=4) ::  AVERAGE_NOX_RATE
      REAL (kind=4) ::  AVERAGE_SOX_RATE
      REAL (kind=4) ::  AVERAGE_CO2_RATE
      REAL (kind=4) ::  AVERAGE_HG_RATE
      REAL (kind=4) ::  AVERAGE_FUEL_COST
      REAL (kind=4) ::  PW_AVE_FUEL_COST
      REAL (kind=4) ::  DISP_COST_1
      REAL (kind=4) ::  DISP_COST_2
      REAL (kind=4) ::  AVERAGE_VAR_OM
      REAL (kind=4) ::  TOTAL_NOX_HEAT
      REAL (kind=4) ::  MONTHLY_NOX_SEASON_THERM
!
      CHARACTER (len=5) ::
     +    CL_UNIQUE_RPT_STR !  THAT GETS A UNIQUE ID FOR THE UNIT
      CHARACTER (len=10) ::  SAC_ONLINE_DATE,SAC_DATE
      CHARACTER (len=16) ::  SAC_UNIT_NAME
      CHARACTER (len=16) ::  GET_RUN_DATE_TIME,SAC_STATION_GROUP


!
! DETAILED REPORTING FOR ICAP.
!
      
      LOGICAL (kind=1) ::  MX_ICAP_REPORT_NOT_OPEN=.TRUE.


      INTEGER (kind=2) ::  MX_ICAP_REPORT_VARIABLES
      INTEGER (kind=2) ::  MX_ICAP_ANNUAL_ALT_NO=0
      INTEGER (kind=2) ::  MX_ICAP_ANNUAL_HEADER
      INTEGER (kind=2) ::  TEMP_POINTER
      INTEGER (kind=2) ::  GET_POINTER_FOR_NEW_CL_UNIT
      INTEGER (kind=2) ::  UPPER_TRANS_GROUP=1



      INTEGER (kind=2) ::  GET_NUMBER_OF_PLANNING_GROUPS
      INTEGER (kind=2) ::  GET_NUMBER_OF_CAPACITY_MARKETS
      INTEGER (kind=2) ::  FIRST_MONTHLY_TRANSACT=0
      INTEGER (kind=2) ::  FIRST_LAST_MONTH_OF_TRANSACT
      INTEGER (kind=2) ::  R_LAST_MONTH
      INTEGER (kind=2) ::  GET_NEWGEN_INDEX
      INTEGER (kind=2) ::  MIN_UPTIME
      INTEGER (kind=2) ::  MIN_DOWNTIME
      INTEGER (kind=2) ::  ICAP_MRX_USE_MARGINAL
         REAL (kind=4) ::
     +                  ICAP_TOTAL_CAPACITY(:),
     +                  ICAP_TOTAL_MWH(:),
     +                  ICAP_TOTAL_COST(:),
     +                  ICAP_TOTAL_REVENUE(:),
     +                  ICAP_LEVEL_CAPITAL_COST(:),
     +                  ICAP_NET_MARGIN(:),
     +                  ICAP_NET_MARGIN_PER_MWH(:),
     +                  ICAP_NET_MARGIN_PER_KW(:),
     +                  ICAP_CONE_VALUE,
     +                  ICAP_MRX_EAS(:),
     +                  ICAP_MRX_EAS_PER_KW,
     +                  ICAP_VAR_COST(:),
     +                  ICAP_FUEL_COST(:),
     +                  ICAP_START_COST(:),
     +                  ICAP_VOM_COST(:),
     +                  ICAP_EMISSION_COST(:),
     +                  ICAP_INSTL_CAPACITY_VALUE,
     +                  ICAP_EAS_REVENUE_OFFSET,
     +                  ICAP_ADJ_CAPACITY_VALUE,
     +                  ICAP_MAX_MRX_OR_CONE,
     +                  ICAP_RESERVE_MARGIN,
     +                  ICAP_DEMAND_POINT,
     +                  ICAP_VALUE_POINT,
     +                  ICAP_MRX_CONE,
     +                  ANNUAL_ICAP_CONE_VALUE(:),
     +                  ANNUAL_ICAP_MAX_MRX_OR_CONE(:),
     +                  ANNUAL_ICAP_RESERVE_MARGIN(:),
     +                  ANNUAL_ICAP_INSTL_CAPACITY_VALUE(:),
     +                  ANNUAL_ICAP_DEMAND_POINT(:),
     +                  ANNUAL_ICAP_VALUE_POINT(:),
     +                  ANNUAL_ICAP_MRX_CONE(:),
     +                  ANNUAL_ICAP_MRX_EAS(:),
     +                  ANNUAL_ICAP_FUEL_COST(:),
     +                  ANNUAL_ICAP_START_COST(:),
     +                  ANNUAL_ICAP_VOM_COST(:),
     +                  ANNUAL_ICAP_EMISSION_COST(:),
     +                  ANNUAL_ICAP_VAR_COST(:),
     +                  ANNUAL_ICAP_EAS_REVENUE_OFFSET(:),
     +                  ANNUAL_ICAP_ADJ_CAPACITY_VALUE(:),
     +                  ANNUAL_ICAP_TOTAL_CAPACITY(:),
     +                  ANNUAL_ICAP_PEAK_MONTH_CAP,
     +                  MONTHLY_ICAP_TOTAL_CAPACITY(:,:),
     +                  MONTHLY_ICAP_VALUE(:),
     +                  ANNUAL_ICAP_TOTAL_MWH(:),
     +                  ANNUAL_ICAP_TOTAL_COST(:),
     +                  ANNUAL_ICAP_TOTAL_REVENUE(:),
     +                  ANNUAL_ICAP_LEVEL_CAPITAL_COST(:),
     +                  ANNUAL_ICAP_NET_MARGIN(:),
     +                  ANNUAL_ICAP_NET_MARGIN_PER_MWH(:),
     +                  ANNUAL_ICAP_NET_MARGIN_PER_KW(:),
     +                  RETURN_SCREEN_CAP_COST,
     +                  NEWGEN_UNIT_STATUS,
     +                  ONLINE_DATE,
     +                  OFLINE_DATE,
     +                  SAC_ONLINE_MO,
     +                  GET_MIN_UP_TIME, !TMS 30/23/05 ADDED FOR EXELON
     +                 GET_MIN_DOWN_TIME,!TMS 30/23/05 ADDED FOR EXELON
     +                  MRX_CAPACITY,
     +                  MRX_ENERGY,
     +                  MRX_TOTAL_REVENUE,
     +                  MRX_TOTAL_VARIABLE,
     +                  MRX_FUEL,
     +                  MRX_VOM,
     +                  MRX_EMISSIONS,
     +                  MRX_TOTAL_COST,
     +                  MRX_ANN_CAP_COST,
     +                  MRX_NET_MARGIN_MM,
     +                  MRX_NET_MARGIN_PER_MWH,
     +                  MRX_NET_MARGIN_PER_KWMO,
     +                  MRX_CONE_PER_KWMO,
     +                  MRX_EAS_REV_PER_KWMO,
     +                  MRX_ICAP_VALUE_POINT
!
      ALLOCATABLE ::
     +                  ICAP_TOTAL_CAPACITY,
     +                  ICAP_TOTAL_MWH,
     +                  ICAP_TOTAL_COST,
     +                  ICAP_TOTAL_REVENUE,
     +                  ICAP_LEVEL_CAPITAL_COST,
     +                  ICAP_NET_MARGIN,
     +                  ICAP_NET_MARGIN_PER_MWH,
     +                  ICAP_NET_MARGIN_PER_KW,
     +                  ICAP_MRX_EAS,
     +                  ICAP_VAR_COST,
     +                  ICAP_FUEL_COST,
     +                  ICAP_START_COST,
     +                  ICAP_VOM_COST,
     +                  ICAP_EMISSION_COST,
     +                  ANNUAL_ICAP_CONE_VALUE,
     +                  ANNUAL_ICAP_MAX_MRX_OR_CONE,
     +                  ANNUAL_ICAP_RESERVE_MARGIN,
     +                  ANNUAL_ICAP_INSTL_CAPACITY_VALUE,
     +                  ANNUAL_ICAP_DEMAND_POINT,
     +                  ANNUAL_ICAP_VALUE_POINT,
     +                  ANNUAL_ICAP_MRX_CONE,
     +                  ANNUAL_ICAP_MRX_EAS,
     +                  ANNUAL_ICAP_VAR_COST,
     +                  ANNUAL_ICAP_FUEL_COST,
     +                  ANNUAL_ICAP_START_COST,
     +                  ANNUAL_ICAP_VOM_COST,
     +                  ANNUAL_ICAP_EMISSION_COST,
     +                  ANNUAL_ICAP_EAS_REVENUE_OFFSET,
     +                  ANNUAL_ICAP_ADJ_CAPACITY_VALUE,
     +                  ANNUAL_ICAP_TOTAL_CAPACITY,
     +                  MONTHLY_ICAP_TOTAL_CAPACITY,
     +                  MONTHLY_ICAP_VALUE,
     +                  ANNUAL_ICAP_TOTAL_MWH,
     +                  ANNUAL_ICAP_TOTAL_COST,
     +                  ANNUAL_ICAP_TOTAL_REVENUE,
     +                  ANNUAL_ICAP_LEVEL_CAPITAL_COST,
     +                  ANNUAL_ICAP_NET_MARGIN,
     +                  ANNUAL_ICAP_NET_MARGIN_PER_MWH,
     +                  ANNUAL_ICAP_NET_MARGIN_PER_KW
      INTEGER ::  MX_ICAP_ANNUAL_ALT_REC

      integer (kind=2) :: gpm_result


      
      integer (kind=2) :: INT2_zero=0
      SAVE MX_ICAP_ANNUAL_ALT_REC,
     +                  ICAP_TOTAL_CAPACITY,
     +                  ICAP_TOTAL_MWH,
     +                  ICAP_TOTAL_COST,
     +                  ICAP_TOTAL_REVENUE,
     +                  ICAP_LEVEL_CAPITAL_COST,
     +                  ICAP_NET_MARGIN,
     +                  ICAP_NET_MARGIN_PER_MWH,
     +                  ICAP_NET_MARGIN_PER_KW,
     +                  ICAP_MRX_EAS,
     +                  ICAP_VAR_COST,
     +                  ICAP_FUEL_COST,
     +                  ICAP_START_COST,
     +                  ICAP_VOM_COST,
     +                  ICAP_EMISSION_COST,
     +                  ANNUAL_ICAP_CONE_VALUE,
     +                  ANNUAL_ICAP_MAX_MRX_OR_CONE,
     +                  ANNUAL_ICAP_RESERVE_MARGIN,
     +                  ANNUAL_ICAP_INSTL_CAPACITY_VALUE,
     +                  ANNUAL_ICAP_DEMAND_POINT,
     +                  ANNUAL_ICAP_VALUE_POINT,
     +                  ANNUAL_ICAP_MRX_CONE,
     +                  ANNUAL_ICAP_MRX_EAS,
     +                  ANNUAL_ICAP_FUEL_COST,
     +                  ANNUAL_ICAP_START_COST,
     +                  ANNUAL_ICAP_VOM_COST,
     +                  ANNUAL_ICAP_EMISSION_COST,
     +                  ANNUAL_ICAP_VAR_COST,
     +                  ANNUAL_ICAP_EAS_REVENUE_OFFSET,
     +                  ANNUAL_ICAP_ADJ_CAPACITY_VALUE,
     +                  ANNUAL_ICAP_TOTAL_CAPACITY,
     +                  MONTHLY_ICAP_TOTAL_CAPACITY,
     +                  MONTHLY_ICAP_VALUE,
     +                  ANNUAL_ICAP_TOTAL_MWH,
     +                  ANNUAL_ICAP_TOTAL_COST,
     +                  ANNUAL_ICAP_TOTAL_REVENUE,
     +                  ANNUAL_ICAP_LEVEL_CAPITAL_COST,
     +                  ANNUAL_ICAP_NET_MARGIN,
     +                  ANNUAL_ICAP_NET_MARGIN_PER_MWH,
     +                  ANNUAL_ICAP_NET_MARGIN_PER_KW,
     +                  TransAllHoursMonth,
     +                  TransCapHours,
     +                  TransEffectiveCapHours,
     +                  TransAvailHoursMonth
      REAL (kind=4) ::  TOTAL_EMISSION_COSTS,PER_UNIT_EMISSION_COST
! 031005

      REAL (kind=4) ::  GET_WH_MONTH_ENERGY
      REAL (kind=4) ::  TEMP_TL_MWH
      REAL (kind=4) ::  TEMP_TL_HYDRO_MW
      REAL (kind=4) ::  GET_SYSTEM_PROD_BY_TG_BY_MWH
      REAL (kind=4) ::  GET_MONTHLY_TL_HYDRO_MW
      REAL (kind=4) ::  GET_WH_MONTH_CAPACITY
      real (kind=4) ::  fLocal_year

      real(kind=4) :: GET_MONTHLY_TL_HYDRO_MWH ! External
      real(kind=4) :: DebugValue
      
      ! This variable specifies the number of products written to 
      ! the report, and prevents writing new products when 
      ! run in compatibility mode. Expanding the number of 
      ! products 
      integer (kind=2) :: max_prod_types_for_report
      
      hesi_second_unit_id_num=0
      
       ! Introduce report compatibility mode.  This allows original
       ! report to be written for compatibity with June '22 executable.
       
       if(report_compatibility_mode) then
        max_prod_types_for_report=old_max_prod_types_clr
      else
        max_prod_types_for_report=max_prod_types_clr
       end if

      if(YR_loc==0) then
        call end_program("clreport:0008 - " // 
     +      "YR_loc argument cannot be zero.")
      end if
      
      if(file_trace_pclr==0) then
        file_trace_pclr=open_trace("period_cl_report.trace", rq_pclr)
        call write_trace_message(file_trace_pclr, 
     +       "Tracing period_cl_report subroutine.")
      end if
      call write_trace_int2(file_trace_pclr, "MAX_LOAD_TYPES", 
     +   MAX_LOAD_TYPES)
      MONTHLY_PW_REPORT = YES_POWERWORLD_REPORT()
      MONTHLY_NEW_PW_REPORT = YES_POWERWORLD_NEW_REPORT()
      REFERENCE_CASE_REPORT = YES_REFERENCE_CASE_REPORT()
      CapEx_Running = YES_POWERWORLD_REPORT()
      CL_UNITS_B4_ADDITIONS = RETURN_CL_UNITS_B4_ADDITIONS()
      call write_trace_int2(file_trace_pclr, "Units B4 Addn", 
     +   CL_UNITS_B4_ADDITIONS)
      call write_trace_bool1(file_trace_pclr, "MONTHLY_PW_REPORT", 
     +   MONTHLY_PW_REPORT)
      call write_trace_bool1(file_trace_pclr, "MONTHLY_NEW_PW_REPORT", 
     +   MONTHLY_NEW_PW_REPORT)
        
      IF(MONTHLY_PW_REPORT .OR. MONTHLY_NEW_PW_REPORT) THEN
         IF(MONTHLY_NEW_PW_REPORT) THEN
            FILE_NAME = TRIM(XML_FILE_DIRECTORY())//
     +                              'PWB'//TRIM(clData%Scename)//'.XML'
        call write_trace_string(file_trace_pclr, "filename", file_name)
         ELSE
            clData%Scename=check_scename()
            FILE_NAME = 'PWB'//TRIM(clData%Scename)//'.CSV'
            call write_trace_string(file_trace_pclr, "filename", 
     +          FILE_NAME)
            
         ENDIF
         IF(PW_REPORT_NOT_OPEN) THEN
            call write_trace_message(file_trace_pclr, "Opening report")
            OPEN(4444,FILE=FILE_NAME,ACCESS='DIRECT',
     +                     FORM='FORMATTED',RECL=1024,STATUS='REPLACE')
            PW_REPORT_NOT_OPEN = .FALSE.

            IF(MONTHLY_NEW_PW_REPORT) THEN

               WRITE(4444,4445,REC=1)
     +               '<XMLIntegration>'//GET_RUN_DATE_TIME()
               AVE_FUEL_MULT = 100.
               LAST_RESOURCE_UPDATE_NO = CL_UNITS_B4_ADDITIONS
            ELSE
               AVE_FUEL_MULT = 1.
               LAST_RESOURCE_UPDATE_NO = 30000
            ENDIF
            WRITE(4444,4445,REC=2)
     +               CRLF//'<NewStation COLUMNS='//
     +               '"ENDPOINT,YEAR,MONTH,UNIT,'//
     +               'EV_TRANS_AREA_NAME,'//
     +               'TRANS_GROUP_ID,TRANS_GROUP_NAME,MIN_CAPACITY,'//
     +               'TOTAL_CAPACITY,'//
     +               'FIXED COST,AVERAGE_FUEL_COST,'//
     +               'FUEL_TYPE,AVERAGE_VAR_COST,IOA,IOB,IOC,IOD,'//
     +               'INSERVICE_STATUS,ONLINE_DATE,'//
     +               'NOX_RATE,SOX_RATE,NO_COST,'//
     +               'SOX_COST,START_UP_COST,MOR,FOR,DATE,'//
     +            'STATIONGROUP,'//
     +            'MINUP,MINDOWN,'//
     +               'CO2_COST,CO2_RATE,HG_COST,HG_RATE">'
            MON_PW_TRANS_UNIT_REC = rptrec(4444_2,SAVE_REC=3,
     +                     REC_LENGHT=1024_2,FILE_NAME=FILE_NAME)
4445        FORMAT(4A)

         ENDIF
      ENDIF
!
      P_FUEL_CONSUMPTION = 0.
      S_FUEL_CONSUMPTION = 0.
      E_FUEL_CONSUMPTION = 0.
      CNTR_ENRG = 0.
      TOTAL_CONTRACT_COST = 0.
      CANADA = COUNTRY() == 'C'
      TONS_CONVERSION = GET_TONS_CONVERSION()
      HEAT_CONVERSION = GET_HEAT_CONVERSION()


      MONTHLY_BLOCK_REPORT = .FALSE.


      MONTHLY_SUMMARY_REPORT_ACTIVE = .FALSE.


      MONTHLY_CONTRACT_REPORT_ACTIVE = .FALSE.
!
      ANNUAL_CONTRACT_REPORT_ACTIVE = (CONTRACT_REPORT() == 'A' .OR.
     +                               MONTHLY_CONTRACT_REPORT_ACTIVE)
!
      MONTHLY_TRANS_REPORT = TRANSACT_UNIT_MONTHLY_REPORT()
      ANNUAL_TRANS_REPORT = TRANSACT_UNIT_ANNUAL_REPORT()
      TRANSACT_FUEL_REPORT_ACTIVE = TRANSACT_FUEL_REPORT()
      TRANSACT_PROD_REPORT_ACTIVE = TRANSACT_PROD_REPORT()
!
      DUKE_RESERVE_CAP_NO = GET_DUKE_RESERVE_CAP_NO()
!
      LAST_SEASON = get_PRODUCTION_PERIODS_in()
      call write_trace_int2(file_trace_pclr, "last_season", 
     + LAST_SEASON)
      BEGIN_DATE = 100.*(get_BASE_YEAR()+1-1900)
      call write_trace_int2(file_trace_pclr, "begin date", begin_date)
! 11/26/02. GAT. AS REQUESTED FROM THE 2002 USERS GROUP MEETING.
      DERIVATIVES_REPORT = MONTHLY_DERIVATIVES_ACTIVE()
      SUPPRESS_DERIVATIVES = SUPPRESS_DERIVATIVES_ACTIVE()
      NEW_UNITS_REPORT = NEW_UNITS_REPORT_ACTIVE()
!
      IF(MON_CT_REPORT_NOT_OPEN .AND.
     +     (ANNUAL_CONTRACT_REPORT_ACTIVE .OR.
     +                       MONTHLY_CONTRACT_REPORT_ACTIVE) .AND.
     +               .NOT. TESTING_PLAN .AND. CONTRACTS_IN_PERIOD) THEN
         MON_CT_REPORT_NOT_OPEN = .FALSE.
         MON_CT_UNIT_NO = MON_CT_UNIT_HEADER(MON_CT_UNIT_REC)
         IF(MON_SUMMARY_REPORT_NOT_OPEN) THEN
            MON_CL_SUM_NO = MON_CL_SUMMARY_HEADER(MON_CL_SUM_REC)
            MON_SUMMARY_REPORT_NOT_OPEN = .FALSE.

         ENDIF
      ENDIF
      IF((MONTHLY_BLOCK_REPORT.OR. MONTHLY_SUMMARY_REPORT_ACTIVE) .AND.
     +                        .NOT. TESTING_PLAN .AND. NBLOCK > 0) THEN
         IF(MON_CL_REPORT_NOT_OPEN .AND. MONTHLY_BLOCK_REPORT) THEN
            MON_CL_UNIT_NO = MON_CL_UNIT_HEADER(MON_CL_UNIT_REC)
            MON_CL_REPORT_NOT_OPEN = .FALSE.
         ENDIF
         IF(MON_SUMMARY_REPORT_NOT_OPEN) THEN
            MON_CL_SUM_NO = MON_CL_SUMMARY_HEADER(MON_CL_SUM_REC)
            MON_SUMMARY_REPORT_NOT_OPEN = .FALSE.

         ENDIF
      ENDIF
      IF( (MONTHLY_TRANS_REPORT .OR. ANNUAL_TRANS_REPORT .OR.
     +          NEW_UNITS_REPORT .OR. DERIVATIVES_REPORT) .AND.
     +                        .NOT. TESTING_PLAN .AND. NBLOCK > 0) THEN
         IF(TRANS_REPORT_NOT_OPEN .AND.
     +            (MONTHLY_TRANS_REPORT .OR. ANNUAL_TRANS_REPORT)) THEN
!
! VAR(37) = COST TO PRODUCE WHOLESALE POWER
! VAR(38) = POWER GENERATED TO SERVE OWN LOADS
! VAR(39) = REVENUE FROM NATIVE LOADS: PROVIDES A BETTER
! VIEW OF GROSS MARGIN WITH 'W' SWITCH
! VAR(44) = SUM OF EMISSION COSTS
!

            MON_CL_TRANS_UNIT_NO = MON_CL_TRANS_UNIT_HEADER(
     +                 MON_CL_TRANS_UNIT_REC,MON_CL_TRANS_UNIT_VAR_NUM,
     +                  CL_TRANS_UNIT_REC_LEN)
!
            TRANS_REPORT_NOT_OPEN = .FALSE.

!
         ENDIF
!
         MAX_TRANS_GROUP_NUMBER = GET_MAX_TRANS_GROUP_NUMBER()
        call write_trace_int2(file_trace_pclr, "MAX_TRANS_GROUP_NUMBER", 
     + MAX_TRANS_GROUP_NUMBER)
!
         IF(ALLOCATED(TRANS_GROUP_REPORTING_ACTIVE))
     +                         DEALLOCATE(TRANS_GROUP_REPORTING_ACTIVE)
         ALLOCATE(
     +         TRANS_GROUP_REPORTING_ACTIVE(
     +                                  MAX(1,MAX_TRANS_GROUP_NUMBER)))
         DO I = 1, MAX_TRANS_GROUP_NUMBER
            TRANS_GROUP_REPORTING_ACTIVE(I) =
     +                                        GET_REPORT_CL_CAPACITY(I)
         ENDDO
!
      ENDIF
!
!
! 11/26/02. DERIVATIVES REPORT FOR SRP AND OTHERS.
!
      IF( DERIVATIVES_REPORT .AND.
     +                 DERIVATIVES_REPORT_NOT_OPEN .AND.
     +                        .NOT. TESTING_PLAN .AND. NBLOCK > 0) THEN
            DERIVATIVES_REPORT_NOT_OPEN = .FALSE.
            MON_DV_TRANS_UNIT_VAR_NUM = MON_CL_TRANS_UNIT_VAR_NUM
            MON_DV_TRANS_UNIT_NO = MON_DV_TRANS_UNIT_HEADER(
     +                 MON_DV_TRANS_UNIT_REC,MON_DV_TRANS_UNIT_VAR_NUM,
     +                  DV_TRANS_UNIT_REC_LEN)
      ENDIF
      IF(NEW_UNITS_REPORT .AND.
     +                 NEW_UNITS_REPORT_NOT_OPEN .AND.
     +                        .NOT. TESTING_PLAN .AND. NBLOCK > 0) THEN
            NEW_UNITS_REPORT_NOT_OPEN = .FALSE.
            MON_NU_TRANS_UNIT_VAR_NUM = MON_CL_TRANS_UNIT_VAR_NUM
            MON_NU_TRANS_UNIT_NO = MON_NU_TRANS_UNIT_HEADER(
     +                 MON_NU_TRANS_UNIT_REC,MON_NU_TRANS_UNIT_VAR_NUM,
     +                  NU_TRANS_UNIT_REC_LEN)
      ENDIF

      IF( TRANSACT_FUEL_REPORT_ACTIVE .AND.
     +                 TRANS_FUEL_REPORT_NOT_OPEN .AND.
     +                        .NOT. TESTING_PLAN .AND. NBLOCK > 0) THEN
         TRANS_FUEL_VARIABLES = 3*max_fuel_types_clr+1
        TRANS_FUEL_NO = TRANSACT_FUEL_RPT_HEADER(TRANS_FUEL_VARIABLES,
     +                                                 TRANS_FUEL_REC)
         I2_VARIABLE_NUMBER = TRANS_FUEL_VARIABLES +
     +                              3*NUMBER_OF_EMISSION_TYPES +
     +                              MAX_GROUP_VAR +
     +                              RPS_STATE_VAR_NUM+
     +                              NUM_RESOURCE_RPS_VARS
         TRANS_STATE_NO = TRANSACT_STATE_RPT_HEADER(I2_VARIABLE_NUMBER,
     +                                                 TRANS_STATE_REC)
         TRANS_FUEL_REPORT_NOT_OPEN = .FALSE.
      ENDIF
!
      IF( TRANSACT_PROD_REPORT_ACTIVE .AND.
     +                 TRANS_PROD_REPORT_NOT_OPEN .AND.
     +                        .NOT. TESTING_PLAN .AND. NBLOCK > 0) THEN
         TRANS_PROD_VARIABLES = 2*max_prod_types_clr +
     + MAX_LOAD_TYPES + 1
         TRANS_PROD_NO = TRANSACT_PROD_RPT_HEADER(TRANS_PROD_VARIABLES,
     +                                                  TRANS_PROD_REC)
         TRANS_PROD_VARIABLES = 2*MFT1
         TRAN_PROD_FUEL_NO =
     +                  TRAN_PROD_FUEL_RPT_HEADER(TRANS_PROD_VARIABLES,
     +                                              TRAN_PROD_FUEL_REC)
         TRANS_PROD_REPORT_NOT_OPEN = .FALSE.
      ENDIF
!
      YES_FISCAL_REPORTING = IS_FISCAL_YEAR_ACTIVE(FSCL_SEASON_RESET,
     +                                                     FISCAL_ONLY)


      IF(FSCL_SEASON_RESET == 1) THEN
         Fscl_Ssn_loc = LAST_SEASON
      ELSE
         Fscl_Ssn_loc = FSCL_SEASON_RESET - 1
      ENDIF
!
      fLocal_year = FLOAT(globecom_year+get_BASE_YEAR())
      TEMP_YEAR = FLOAT(globecom_year+get_BASE_YEAR())
!
      IF(FISCAL_ONLY) THEN
         IF(FSCL_SEASON_RESET > 1 .AND.
     +                               ISEAS >= FSCL_SEASON_RESET) THEN
            fLocal_year = FLOAT(globecom_year+get_BASE_YEAR()+1)
         ENDIF
      ENDIF
      IF((YES_FISCAL_REPORTING .AND. ISEAS == FSCL_SEASON_RESET) .OR.
     +              (.NOT. YES_FISCAL_REPORTING .AND. ISEAS == 1)) THEN
            FISCAL_REV_GEN_CAPACITY = 0.
            FISCAL_EFFECTIVE_CAP = 0.
            FISCAL_CAP = 0.
            FISCAL_CL_UNIT_ENERGY = 0.
!
               FISCAL_CL_UNIT_MMBTUS = 0.D0
!
            FISCAL_CL_UNIT_VAR_COST = 0.
            FISCAL_CL_UNIT_FIXED_COST = 0.
            FISCAL_CL_UNIT_FUEL_COST = 0.
            FISCAL_P_FUEL_CONSUMPTION = 0.
            FISCAL_S_FUEL_CONSUMPTION = 0.
            FISCAL_E_FUEL_CONSUMPTION = 0.
            FISCAL_WHOLESALE_PROD_COST = 0.
            FISCAL_NOX_SEASON_EMISSIONS = 0.
            FISCAL_NOX_SEASON_THERMAL = 0.
            FISCAL_UNIT_STARTS = 0.
            FISCAL_UNIT_START_COSTS = 0.
            FISCAL_CAPITAL_COST_OF_BUILD = 0.
            FISCAL_EBITDA = 0.
            FISCAL_MONTHS_ACTIVE = 0.
            FISCAL_CL_UNIT_EMISSIONS = 0.
            FISCAL_UNIT_EMISSIONS_COST = 0.
            FISCAL_ECO_SALES_REV_FROM = 0.
            FISCAL_ECO_SALES_ENRG_FROM = 0.
            FISCAL_ECO_PUCH_COST_FROM = 0.
            FISCAL_ECO_PUCH_ENRG_FROM = 0.
      ENDIF
!
! ALWAYS ACCUMULATE THE FUEL INFO.
!

         IF(ISEAS == 1) THEN
            UPPER_TRANS_GROUP = GET_NUMBER_OF_ACTIVE_GROUPS()
!
            MAX_STATE_PROVINCE_NO = GET_MAX_STATE_PROVINCE_NO()
!

            CG = MAX(GET_NUMBER_OF_CAPACITY_MARKETS(),1)
!
            NUM_TRANSACTIONS = GET_NUM_SCENARIO_TRANSACTIONS()
!
            ANNUAL_UNIT_EMISSIONS_COST = 0.
            ANNUAL_UNIT_STARTS = 0.
            ANNUAL_CAPITAL_COST_OF_BUILD = 0.
            ANNUAL_EBITDA = 0.
            ANNUAL_UNIT_START_COSTS = 0.
            ANNUAL_UNIT_CAPACITY_REVENUE = 0.
!

            RUN_TRANSACT = YES_RUN_TRANSACT()
            if(file_trace_pclr/=BAD_TRACE_HANDLE) then
           write(file_trace_pclr,*) 
     +      "UPPER_TRANS_GROUP ", UPPER_TRANS_GROUP, 
     +      "MAX_STATE_PROVINCE_NO ", MAX_STATE_PROVINCE_NO, 
     +  "max_fuel_types ", max_fuel_types_clr, 
     +  "MAX_STATE_PROVINCE_NO ", MAX_STATE_PROVINCE_NO,
     +  "MAX_GROUP_VAR ", MAX_GROUP_VAR,
     +  "MFT1 ", MFT1,
     +  "MAX_GAS_REGION_NO ", MAX_GAS_REGION_NO,
     +  "max_prod_types ", max_prod_types_clr,
     +  "MAX_LOAD_TYPES ", MAX_LOAD_TYPES
            end if
!
            IF(ALLOCATED(ANNUAL_FUEL_COST_BY_TG_BY_FUEL)) THEN
                DEALLOCATE(   ANNUAL_FUEL_COST_BY_TG_BY_FUEL,
     +                        FUEL_COST_BY_TG_BY_FUEL)
                DEALLOCATE(   ANNUAL_FUEL_COST_BY_SP_BY_FUEL,
     +                        FUEL_COST_BY_SP_BY_FUEL)
                DEALLOCATE(   MULTI_AREA_NAME)
                DEALLOCATE(   ANNUAL_FUEL_BTUS_BY_TG_BY_FUEL)
                DEALLOCATE(   FUEL_BTUS_BY_TG_BY_FUEL)
                DEALLOCATE(   MW_BY_TG_BY_FUEL,
     +                        ANNUAL_MW_BY_TG_BY_FUEL,
     +                        MWH_BY_TG_BY_FUEL,
     +                        ANNUAL_MWH_BY_TG_BY_FUEL)
                DEALLOCATE(   ANNUAL_FUEL_BTUS_BY_SP_BY_FUEL)
                DEALLOCATE(   FUEL_BTUS_BY_SP_BY_FUEL)
                DEALLOCATE(   FUEL_BTUS_BY_GSP_BY_FUEL,
     +                        ANNUAL_FUEL_BTUS_BY_GSP_BY_FUEL)
                DEALLOCATE(   PROD_BY_TG_BY_MW)
                DEALLOCATE(   ANNUAL_PROD_BY_TG_BY_MW)
                DEALLOCATE(   PROD_BY_TG_BY_MWH)
                DEALLOCATE(   ANNUAL_PROD_BY_TG_BY_MWH)
                DEALLOCATE(   LOAD_BY_TG_BY_MWH)
                DEALLOCATE(   ANNUAL_LOAD_BY_TG_BY_MWH)
                DEALLOCATE(   PROD_BY_MK_BY_MWH)
                DEALLOCATE(   ANNUAL_PROD_BY_MK_BY_MWH)
                DEALLOCATE(   FUEL_COST_PER_MMBTU)
                DEALLOCATE(   EMISSIONS_COST_BY_SP,
     +                        ANNUAL_EMISSIONS_COST_BY_SP)
                DEALLOCATE(   GROUP_VAR_BY_SP)
                DEALLOCATE(   ANNUAL_GROUP_VAR_BY_SP)
            ENDIF
            
            ALLOCATE(
     +         ANNUAL_FUEL_COST_BY_TG_BY_FUEL(
     +               MAX(1,UPPER_TRANS_GROUP),max_fuel_types_clr),
     +         FUEL_COST_BY_TG_BY_FUEL(
     +               MAX(1,UPPER_TRANS_GROUP),max_fuel_types_clr),
     +         ANNUAL_FUEL_COST_BY_SP_BY_FUEL(
     +                    0:MAX_STATE_PROVINCE_NO,max_fuel_types_clr),
     +         FUEL_COST_BY_SP_BY_FUEL(
     +                    0:MAX_STATE_PROVINCE_NO,max_fuel_types_clr),
     +         GROUP_VAR_BY_SP(
     +                    0:MAX_STATE_PROVINCE_NO,MAX_GROUP_VAR),
     +         ANNUAL_GROUP_VAR_BY_SP(
     +                    0:MAX_STATE_PROVINCE_NO,MAX_GROUP_VAR),
     +         EMISSIONS_COST_BY_SP(
     +                    0:MAX_STATE_PROVINCE_NO,5),
     +         ANNUAL_EMISSIONS_COST_BY_SP(
     +                    0:MAX_STATE_PROVINCE_NO,5),
     +         FUEL_COST_PER_MMBTU(max_fuel_types_clr),
     +         ANNUAL_FUEL_BTUS_BY_TG_BY_FUEL(
     +               MAX(1,UPPER_TRANS_GROUP),max_fuel_types_clr),
     +         FUEL_BTUS_BY_TG_BY_FUEL(
     +               MAX(1,UPPER_TRANS_GROUP),max_fuel_types_clr),
     +         MW_BY_TG_BY_FUEL(
     +               MAX(1,UPPER_TRANS_GROUP),MFT1),
     +         ANNUAL_MW_BY_TG_BY_FUEL(
     +               MAX(1,UPPER_TRANS_GROUP),MFT1),
     +         MWH_BY_TG_BY_FUEL(
     +               MAX(1,UPPER_TRANS_GROUP),MFT1),
     +         ANNUAL_MWH_BY_TG_BY_FUEL(
     +               MAX(1,UPPER_TRANS_GROUP),MFT1),
     +         ANNUAL_FUEL_BTUS_BY_SP_BY_FUEL(
     +                    0:MAX_STATE_PROVINCE_NO,max_fuel_types_clr),
     +         FUEL_BTUS_BY_SP_BY_FUEL(
     +                    0:MAX_STATE_PROVINCE_NO,max_fuel_types_clr),
     +         ANNUAL_FUEL_BTUS_BY_GSP_BY_FUEL(
     +                    0:MAX_GAS_REGION_NO,3),
     +         FUEL_BTUS_BY_GSP_BY_FUEL(
     +                    0:MAX_GAS_REGION_NO,3),
     +         PROD_BY_TG_BY_MW(
     +               MAX(1,UPPER_TRANS_GROUP),max_prod_types_clr),
     +         ANNUAL_PROD_BY_TG_BY_MW(
     +               MAX(1,UPPER_TRANS_GROUP),max_prod_types_clr),
     +         PROD_BY_TG_BY_MWH(
     +               MAX(1,UPPER_TRANS_GROUP),max_prod_types_clr),
     +         ANNUAL_PROD_BY_TG_BY_MWH(
     +               MAX(1,UPPER_TRANS_GROUP),max_prod_types_clr),
     +         LOAD_BY_TG_BY_MWH(
     +               MAX(1,UPPER_TRANS_GROUP),MAX_LOAD_TYPES),
     +         ANNUAL_LOAD_BY_TG_BY_MWH(
     +               MAX(1,UPPER_TRANS_GROUP),MAX_LOAD_TYPES),
     +         PROD_BY_MK_BY_MWH(0:600,3),
     +         ANNUAL_PROD_BY_MK_BY_MWH(0:600,3),
     +         MULTI_AREA_NAME(MAX(1,UPPER_TRANS_GROUP)))
            ANNUAL_FUEL_COST_BY_TG_BY_FUEL = 0.
            ANNUAL_FUEL_COST_BY_SP_BY_FUEL = 0.
            ANNUAL_GROUP_VAR_BY_SP = 0.
            ANNUAL_MW_BY_TG_BY_FUEL = 0.
            ANNUAL_MWH_BY_TG_BY_FUEL = 0.
            ANNUAL_FUEL_BTUS_BY_TG_BY_FUEL = 0.
            ANNUAL_FUEL_BTUS_BY_SP_BY_FUEL = 0.
            ANNUAL_FUEL_BTUS_BY_GSP_BY_FUEL = 0.
            ANNUAL_PROD_BY_TG_BY_MW = 0.
            ANNUAL_PROD_BY_TG_BY_MWH = 0.
            ANNUAL_LOAD_BY_TG_BY_MWH = 0.
            ANNUAL_LOAD_BY_TG_BY_MWH(:,1) = 9999999.
            ANNUAL_PROD_BY_MK_BY_MWH = 0.
            IF(UPPER_TRANS_GROUP > 0) THEN
               DO I = 1, UPPER_TRANS_GROUP
                  MULTI_AREA_NAME(I) = GET_GROUP_NAME(I)
               ENDDO
            ELSE
               MULTI_AREA_NAME(1) = 'System              '
            ENDIF
         ENDIF
!
         CALL FUEL_PRICE_FILES_ACTIVE(FUEL_PRICE_DATA_AVAILABLE)
!
         FUEL_COST_BY_TG_BY_FUEL = 0.
         FUEL_COST_BY_SP_BY_FUEL = 0.
         GROUP_VAR_BY_SP = 0.
         FUEL_BTUS_BY_TG_BY_FUEL = 0.
         FUEL_BTUS_BY_SP_BY_FUEL = 0.
         FUEL_BTUS_BY_GSP_BY_FUEL = 0.
         MW_BY_TG_BY_FUEL = 0.
         MWH_BY_TG_BY_FUEL = 0.
         PROD_BY_TG_BY_MW = 0.
         PROD_BY_TG_BY_MWH = 0.
         LOAD_BY_TG_BY_MWH = 0.
         PROD_BY_MK_BY_MWH = 0.
!
!
!
! 040905. NUNITS MAY BE TOO SHORT IF WE INNCLUDE DERIVATIVES
!
      IF(ALLOCATED(MONTHLY_UNIT_EMISSIONS_COST))
     +                          DEALLOCATE(MONTHLY_UNIT_EMISSIONS_COST)
      ALLOCATE(MONTHLY_UNIT_EMISSIONS_COST(5,get_NUNITS()))
      MONTHLY_UNIT_EMISSIONS_COST = 0.
!
         MONTHLY_TOTAL_EMISSIONS = 0.
         MONTHLY_TOTAL_EMISSIONS_COST = 0.
      MONTHLY_HEAT = 0. D0
!
      DAYS_IN_MONTH = MAX(SEAS_HOURS/24.,.001)

!
! CL UNITS BY BLOCK
!
      DO I = 1, NBLOCK
! First assignment to UNITNO. Only case where it's assigned from the
! UNIT array. In this case it's iterating to NBLOCK; other cases it's
! iterating to NUNITS.

         call write_trace_int2(file_trace_pclr, "I", I)
         
         UNITNO = UNIT(I)
         call write_trace_int2(file_trace_pclr, "UNITNO", UNITNO)
         
         BLKNUM = MIN(2,MAX(BLKNO(I),1))
         call write_trace_int2(file_trace_pclr, "MAX_CL_UNITS", 
     + MAX_CL_UNITS)
        call write_trace_real(file_trace_pclr, "SEAS_HOURS", SEAS_HOURS)
         call write_trace_int2(file_trace_pclr, "BLKNUM", BLKNUM)
         call write_trace_real4(file_trace_pclr, "ENRG (was)", ENRG)
         call write_trace_real4(file_trace_pclr, "ENERGY(B,U)", 
     + ENERGY(BLKNUM,UNITNO))
         ENRG = SEAS_HOURS * ENERGY(BLKNUM,UNITNO)
         call write_trace_real4(file_trace_pclr, "ENRG (is)", ENRG)
         


         CAPBLK = EFFECTIVE_CAPACITY(BLKNUM,UNITNO)
         IF(BLKNUM == 1) THEN
            HEAT = BLK1_HEAT(UNITNO)
            FUEL_COST = BLOCK_FUEL_COST(1,UNITNO)
         ELSE
            HEAT = BLK2_HEAT(UNITNO)
            FUEL_COST = BLOCK_FUEL_COST(2,UNITNO)
         ENDIF
         VOM_COST = 0.0
!
         ANNUAL_CL_UNIT_FUEL_COST(UNITNO) = FUEL_COST +
     +                            ANNUAL_CL_UNIT_FUEL_COST(UNITNO)
         ANNUAL_CL_UNIT_MMBTUS(UNITNO) = HEAT +
     +                                  ANNUAL_CL_UNIT_MMBTUS(UNITNO)
         FISCAL_CL_UNIT_FUEL_COST(UNITNO) = FUEL_COST +
     +                            FISCAL_CL_UNIT_FUEL_COST(UNITNO)
         FISCAL_CL_UNIT_MMBTUS(UNITNO) = HEAT +
     +                                  FISCAL_CL_UNIT_MMBTUS(UNITNO)
         MONTHLY_HEAT = MONTHLY_HEAT + HEAT
         IF(ENRG > .45) THEN
           FUEL_COST = FUEL_COST + ENRG * FUEL_ADDER_ADJUSTMENT(UNITNO)
!
            VOM_COST = VCPMWH(UNITNO) * ENRG
        ANNUAL_CL_UNIT_ENERGY(UNITNO) = ENRG +
     +                                  ANNUAL_CL_UNIT_ENERGY(UNITNO)
        ANNUAL_CL_UNIT_VAR_COST(UNITNO) = VOM_COST +
     +                                ANNUAL_CL_UNIT_VAR_COST(UNITNO)
        FISCAL_CL_UNIT_ENERGY(UNITNO) = ENRG +
     +                                  FISCAL_CL_UNIT_ENERGY(UNITNO)
        FISCAL_CL_UNIT_VAR_COST(UNITNO) = VOM_COST +
     +                                FISCAL_CL_UNIT_VAR_COST(UNITNO)
         ENDIF
!
! EMISSIONS
!
         CALL RETURN_EMISSIONS_BY_BLOCK(UNITNO,BLKNUM,R_SOX,R_NOX,
     +                                  R_CO2,R_OTH2,R_OTH3)
         UNIT_EMISSIONS(1) = R_SOX/TONS_CONVERSION
         UNIT_EMISSIONS(2) = R_NOX/TONS_CONVERSION
         UNIT_EMISSIONS(3) = R_CO2/TONS_CONVERSION
         UNIT_EMISSIONS(4) = R_OTH2/TONS_CONVERSION
         UNIT_EMISSIONS(5) = R_OTH3/TONS_CONVERSION
!
! SEND COST INFORMATION FOR USE BY MONTHLY MIDAS
!
         REAL_FUEL_COST = SNGL(FUEL_COST)
        if(unitno> 4000000000) then
            call end_program("clreport:0007 - r_unitno " //
     +       "value is invalid.")
          end if
         VOID_LOGICAL = MON_MDS_CL_VAR(ISEAS,YR_loc,UNITNO,
     +                                 REAL_FUEL_COST,
     +                                 ENRG,HEAT,UNIT_EMISSIONS,
     +                                 VOM_COST)
!
! TODO: EXTRACT_METHOD record_emission_types (failed)
         DO EM = 1, NUMBER_OF_EMISSION_TYPES
            MONTHLY_TOTAL_EMISSIONS(EM) = UNIT_EMISSIONS(EM) +
     +                                    MONTHLY_TOTAL_EMISSIONS(EM)
            ANNUAL_CL_UNIT_EMISSIONS(EM,UNITNO)=UNIT_EMISSIONS(EM) +
     +                           ANNUAL_CL_UNIT_EMISSIONS(EM,UNITNO)
            FISCAL_CL_UNIT_EMISSIONS(EM,UNITNO)=UNIT_EMISSIONS(EM) +
     +                           FISCAL_CL_UNIT_EMISSIONS(EM,UNITNO)
         ENDDO

! TODO: EXTRACT_METHOD record_emission_types (end)

         IF(MONTHLY_BLOCK_REPORT .AND.
     +                        .NOT. TESTING_PLAN .AND. NBLOCK > 0) THEN
            IF(.NOT. CANADA .AND. PHASE_I_UNIT(UNITNO) ) THEN

               UNIT_NAME = trim(UNITNM(UNITNO))//'*'
            ELSE
               UNIT_NAME = UNITNM(UNITNO)
            endif
            IF(BLKNO(I) < 2) THEN
               CL_SEGMENT_NAME = trim(UNIT_NAME)//'01'
            ELSE
               CL_SEGMENT_NAME = trim(UNIT_NAME)//'02'
            endif
            IF(ENRG > 0.45) THEN
               AVERAGE_HEATRATE = HEAT_CONVERSION*HEAT/ENRG
               AVERAGE_PRODUCTION_COSTS =
     +               SNGL((FUEL_COST))/ENRG+VCPMWH(UNITNO)
              IF(.NOT. CANADA) AVERAGE_HEATRATE = AVERAGE_HEATRATE + .5
            ELSE

               AVERAGE_HEATRATE = N_A
               AVERAGE_PRODUCTION_COSTS = N_A
            endif
            MON_CL_UNIT_REC = rptrec(MON_CL_UNIT_NO)
            WRITE(MON_CL_UNIT_NO,REC=MON_CL_UNIT_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(globecom_year+get_BASE_YEAR()),
     +               CL_MONTH_NAME(ISEAS),
     +               CL_SEGMENT_NAME,
     +               FLOAT(I),
     +               MWBLOK(I),
     +               CAPBLK,
     +               ENRG/1000.,
     +               SNGL(HEAT),
     +               AVERAGE_HEATRATE,
     +               SNGL(FUEL_COST)/1000000.,
     +               VCPMWH(UNITNO)*ENRG/1000000.,
     +               AVERAGE_PRODUCTION_COSTS,
     +               (UNIT_EMISSIONS(EM),
     +                     EM=1,NUMBER_OF_EMISSION_TYPES)
            MON_CL_UNIT_REC = MON_CL_UNIT_REC + 1
         endif
      ENDDO
!
! CL UNITS BY UNIT
!
! 3/21/02. ICAP REPORT.

      MX_ICAP_SUMMARY_REPORT = CAPACITY_PLANNING_METHOD() == 'MX'
     +                                               .AND. RUN_TRANSACT

!
! NEED TO PULL REVENUES FROM TRANSACT INTO THIS REPORT
!
!
      IF((MONTHLY_TRANS_REPORT .OR. ANNUAL_TRANS_REPORT
     +                         .OR. MX_ICAP_SUMMARY_REPORT
     +                         .OR. TRANSACT_FUEL_REPORT_ACTIVE) .AND.
     +                                         .NOT. TESTING_PLAN) THEN

!
         IF(ISEAS == 1) THEN
            ANNUAL_REV_GEN_CAPACITY = 0.
            ANNUAL_CAP = 0.
            AllHoursMonth = 0.
            CapHours = 0.
            EffectiveCapHours = 0.
            AvailHoursMonth = 0.
            ANNUAL_P_FUEL_CONSUMPTION = 0.
            ANNUAL_S_FUEL_CONSUMPTION = 0.
            ANNUAL_E_FUEL_CONSUMPTION = 0.
            ANNUAL_EFFECTIVE_CAP = 0.
            MONTHS_ACTIVE = 0.
            ANNUAL_WHOLESALE_PROD_COST = 0.
            ANNUAL_NOX_SEASON_EMISSIONS = 0.
            ANNUAL_NOX_SEASON_THERMAL = 0.
            TOTAL_NOX_HEAT = 0.
!
            WHOLESALE_MARKET_COST = 0.
            WHOLESALE_MARKET_BUY_MWH = 0.
            MONTHLY_WHOLESALE_COST = 0.
            WHOLESALE_MARKET_REV = 0.
            WHOLESALE_DERIV_COST = 0.
            WHOLESALE_MARKET_SELL_MWH = 0.
         endif
!
         MONTHLY_STARTS = 0
         MONTHLY_START_COSTS = 0.
         MONTHLY_CAPITAL_COST_OF_BUILD = 0.
         MONTHLY_EBITDA = 0.
         MONTHLY_CHARGING_ENERGY = 0.
         MONTHLY_CAPACITY_REVENUE = 0.
!
         MONTHLY_TRANS_CAPACITY = 0.0
         MONTHLY_TRANS_EFFECTIVE_CAP = 0.0
         MONTHLY_TRANS_AVAIL_CAP = 0.0
         MONTHLY_TRANS_ENERGY = 0.0
         MONTHLY_TRANS_HEAT = 0.0 D0
         MONTHLY_P_HEAT = 0.0 ! D0
         MONTHLY_S_HEAT = 0.0 ! D0
         MONTHLY_E_HEAT = 0.0 ! D0
         MONTHLY_TRANS_FUEL_COST = 0.0 D0
         MONTHLY_TRANS_VAR_COST = 0.0
!
         MONTHLY_UNIT_EMISSIONS(:) = 0.0
!
!
         MONTHLY_TRANS_FIXED_COST = 0.
!
         MONTHLY_ECO_SALES = 0.
         MONTHLY_ECO_SALES_ENRG = 0.
         MONTHLY_DERIV_ECO_SALES = 0.
         MONTHLY_DERIV_ECO_SALES_ENRG = 0.
         MONTHLY_ECO_PUCH = 0.
         MONTHLY_ECO_PUCH_ENRG = 0.
!
         MONTHLY_WHOLE_COSTS = 0.
         MONTHLY_WHOLESALE_FUEL_COSTS = 0.
         MONTHLY_WHOLESALE_VOM_COSTS = 0.
         MONTHLY_DERIV_WHOLESALE_COST = 0.
!
         IF(ALLOCATED(ICAP_TOTAL_MWH))
     +         DEALLOCATE(ICAP_TOTAL_MWH)
         IF(ALLOCATED(ICAP_TOTAL_REVENUE))
     +         DEALLOCATE(ICAP_TOTAL_REVENUE,
     +                  ICAP_LEVEL_CAPITAL_COST,
     +                  ICAP_NET_MARGIN,
     +                  ICAP_NET_MARGIN_PER_MWH,
     +                  ICAP_NET_MARGIN_PER_KW,
     +                  ICAP_MRX_EAS,
     +                  ICAP_VAR_COST,
     +                  ICAP_FUEL_COST,
     +                  ICAP_START_COST,
     +                  ICAP_VOM_COST,
     +                  ICAP_EMISSION_COST)
         IF (ALLOCATED(ICAP_TOTAL_CAPACITY) )
     +         DEALLOCATE(ICAP_TOTAL_CAPACITY)
         IF(ALLOCATED(ICAP_TOTAL_COST))
     +         DEALLOCATE(ICAP_TOTAL_COST)
         ALLOCATE(ICAP_TOTAL_CAPACITY(CG),
     +            ICAP_TOTAL_MWH(CG),
     +            ICAP_TOTAL_COST(CG),
     +            ICAP_TOTAL_REVENUE(CG),
     +            ICAP_LEVEL_CAPITAL_COST(CG),
     +            ICAP_NET_MARGIN(CG),
     +            ICAP_NET_MARGIN_PER_MWH(CG),
     +            ICAP_NET_MARGIN_PER_KW(CG),
     +            ICAP_MRX_EAS(CG),
     +            ICAP_VAR_COST(CG),
     +            ICAP_FUEL_COST(CG),
     +            ICAP_START_COST(CG),
     +            ICAP_VOM_COST(CG),
     +            ICAP_EMISSION_COST(CG))
!
!
            ICAP_TOTAL_CAPACITY = 0.
            ICAP_TOTAL_MWH = 0.
            ICAP_TOTAL_REVENUE = 0.
            ICAP_TOTAL_COST = 0.
            ICAP_LEVEL_CAPITAL_COST = 0.
            ICAP_NET_MARGIN = 0.
            ICAP_MRX_EAS = 0.
            ICAP_VAR_COST = 0.
            ICAP_FUEL_COST = 0.
            ICAP_START_COST = 0.
            ICAP_VOM_COST = 0.
            ICAP_EMISSION_COST = 0.
            EMISSIONS_COST_BY_SP = 0.
!
!
         IF(ISEAS == 1) THEN
            FIRST_MONTHLY_TRANSACT =
     +                       FIRST_LAST_MONTH_OF_TRANSACT(R_LAST_MONTH)
         endif
         IF(ISEAS == FIRST_MONTHLY_TRANSACT) THEN
!
            IF (ALLOCATED(ANNUAL_ICAP_TOTAL_CAPACITY) )
     +         DEALLOCATE(TG_EffectiveCapacity,
     +                    TG_CapacitySupplyObligation,
     +                  ANNUAL_ICAP_CONE_VALUE,
     +                  ANNUAL_ICAP_MAX_MRX_OR_CONE,
     +                  ANNUAL_ICAP_RESERVE_MARGIN,
     +                  ANNUAL_ICAP_INSTL_CAPACITY_VALUE,
     +                  ANNUAL_ICAP_DEMAND_POINT,
     +                  ANNUAL_ICAP_VALUE_POINT,
     +                  ANNUAL_ICAP_MRX_CONE,
     +                  ANNUAL_ICAP_MRX_EAS,
     +                  ANNUAL_ICAP_VAR_COST,
     +                  ANNUAL_ICAP_FUEL_COST,
     +                  ANNUAL_ICAP_START_COST,
     +                  ANNUAL_ICAP_VOM_COST,
     +                  ANNUAL_ICAP_EMISSION_COST,
     +                  ANNUAL_ICAP_EAS_REVENUE_OFFSET,
     +                  ANNUAL_ICAP_ADJ_CAPACITY_VALUE,
     +                  ANNUAL_ICAP_TOTAL_CAPACITY,
     +                  MONTHLY_ICAP_TOTAL_CAPACITY,
     +                  MONTHLY_ICAP_VALUE,
     +                  ANNUAL_ICAP_TOTAL_MWH,
     +                  ANNUAL_ICAP_TOTAL_COST,
     +                  ANNUAL_ICAP_TOTAL_REVENUE,
     +                  ANNUAL_ICAP_LEVEL_CAPITAL_COST,
     +                  ANNUAL_ICAP_NET_MARGIN,
     +                  ANNUAL_ICAP_NET_MARGIN_PER_MWH,
     +                  ANNUAL_ICAP_NET_MARGIN_PER_KW,
     +                  MRX_RPS_DV_CURVE_GROSS_MARGIN,
     +                  MRX_RPS_DV_CURVE_REVENUE,
     +                  MRX_RPS_DV_CURVE_COST,
     +                  MRX_RPS_DV_CURVE_UMWH,
     +                  TransAllHoursMonth,
     +                  TransCapHours,
     +                  TransEffectiveCapHours,
     +                  TransAvailHoursMonth)
            ALLOCATE(
     +               ANNUAL_ICAP_CONE_VALUE(CG),
     +               ANNUAL_ICAP_MAX_MRX_OR_CONE(CG),
     +               ANNUAL_ICAP_RESERVE_MARGIN(CG),
     +               ANNUAL_ICAP_INSTL_CAPACITY_VALUE(CG),
     +               ANNUAL_ICAP_DEMAND_POINT(CG),
     +               ANNUAL_ICAP_MRX_CONE(CG),
     +               ANNUAL_ICAP_MRX_EAS(CG),
     +               ANNUAL_ICAP_VAR_COST(CG),
     +               ANNUAL_ICAP_FUEL_COST(CG),
     +               ANNUAL_ICAP_START_COST(CG),
     +               ANNUAL_ICAP_VOM_COST(CG),
     +               ANNUAL_ICAP_EMISSION_COST(CG),
     +               ANNUAL_ICAP_EAS_REVENUE_OFFSET(CG),
     +               ANNUAL_ICAP_ADJ_CAPACITY_VALUE(CG),
     +               ANNUAL_ICAP_VALUE_POINT(CG),
     +               ANNUAL_ICAP_TOTAL_CAPACITY(CG),
     +               MONTHLY_ICAP_TOTAL_CAPACITY(CG,0:12),
     +               MONTHLY_ICAP_VALUE(CG),
     +               ANNUAL_ICAP_TOTAL_MWH(CG),
     +               ANNUAL_ICAP_TOTAL_COST(CG),
     +               ANNUAL_ICAP_TOTAL_REVENUE(CG),
     +               ANNUAL_ICAP_LEVEL_CAPITAL_COST(CG),
     +               ANNUAL_ICAP_NET_MARGIN(CG),
     +               ANNUAL_ICAP_NET_MARGIN_PER_MWH(CG),
     +               ANNUAL_ICAP_NET_MARGIN_PER_KW(CG),
     +               MRX_RPS_DV_CURVE_GROSS_MARGIN(NUM_TRANSACTIONS),
     +               MRX_RPS_DV_CURVE_REVENUE(NUM_TRANSACTIONS),
     +               MRX_RPS_DV_CURVE_COST(NUM_TRANSACTIONS),
     +               MRX_RPS_DV_CURVE_UMWH(NUM_TRANSACTIONS),
     +               TransAllHoursMonth(NUM_TRANSACTIONS),
     +               TransCapHours(NUM_TRANSACTIONS),
     +               TransEffectiveCapHours(NUM_TRANSACTIONS),
     +               TransAvailHoursMonth(NUM_TRANSACTIONS),
     +               TG_CapacitySupplyObligation(
     +                                MAX(1,UPPER_TRANS_GROUP),12),
     +               TG_EffectiveCapacity(
     +                                MAX(1,UPPER_TRANS_GROUP),12))
!
!
            MRX_RPS_CL_CURVE_GROSS_MARGIN = 0.
            MRX_RPS_CL_CURVE_REVENUE = 0.
            MRX_RPS_CL_CURVE_COST = 0.
            MRX_RPS_CL_CURVE_UMWH = 0.
            MRX_RPS_DV_CURVE_GROSS_MARGIN = 0.
            MRX_RPS_DV_CURVE_REVENUE = 0.
            MRX_RPS_DV_CURVE_COST = 0.
            MRX_RPS_DV_CURVE_UMWH = 0.
!
            TransAllHoursMonth = 0.
            TransCapHours = 0.
            TransEffectiveCapHours = 0.
            TransAvailHoursMonth = 0.
            TG_CapacitySupplyObligation = 0.0
            TG_EffectiveCapacity = 0.0
            MONTHLY_ICAP_TOTAL_CAPACITY = 0.
            ANNUAL_ICAP_TOTAL_CAPACITY = 0.
            ANNUAL_ICAP_TOTAL_MWH = 0.
            ANNUAL_ICAP_TOTAL_REVENUE = 0.
            ANNUAL_ICAP_TOTAL_COST = 0.
            ANNUAL_ICAP_LEVEL_CAPITAL_COST = 0.
            ANNUAL_ICAP_NET_MARGIN = 0.
            ANNUAL_ICAP_CONE_VALUE = 0.
            ANNUAL_ICAP_MAX_MRX_OR_CONE = 0.
            ANNUAL_ICAP_RESERVE_MARGIN = 0.
            ANNUAL_ICAP_INSTL_CAPACITY_VALUE = 0.0
            ANNUAL_ICAP_DEMAND_POINT = 0.
            ANNUAL_ICAP_VALUE_POINT = 0.
            ANNUAL_ICAP_MRX_CONE = 0.0
            ANNUAL_ICAP_MRX_EAS = 0.0
            ANNUAL_ICAP_VAR_COST = 0.0
            ANNUAL_ICAP_FUEL_COST = 0.0
            ANNUAL_ICAP_START_COST = 0.0
            ANNUAL_ICAP_VOM_COST = 0.0
            ANNUAL_ICAP_EMISSION_COST = 0.0
            ANNUAL_ICAP_EAS_REVENUE_OFFSET = 0.0
            ANNUAL_ICAP_ADJ_CAPACITY_VALUE = 0.0
            ANNUAL_EMISSIONS_COST_BY_SP = 0.0
!
            TEMP_L1 = CALC_ANN_ICAP_VALUES(YR_loc)
!
!
         endif ! FIRST SEASON
         RUN_MULTIAREA_TRANSACT = YES_RUN_MULTIAREA_TRANSACT()
         MONTHLY_ICAP_VALUE = 0.0
!
         CALL AllocateArray(SaveMW2,2_2,get_NUNITS())
         DO I = 1, get_NUNITS()
            SaveMW2(:,I)= MW(:,I)
            IF(ONLINE(I) > DATE2 .OR. OFLINE(I) < DATE1) THEN
               MW(:,I) = 0.
            endif
         ENDDO
!
! 102409.
!

! TODOne: EXTRACT_METHOD prepare_icap_summary_report (done)
       
       
      CALL prepare_icap_summary_report(YR_loc, iseas, 
     + ICAP_TOTAL_CAPACITY,
     +ICAP_TOTAL_MWH,ICAP_TOTAL_REVENUE,ICAP_LEVEL_CAPITAL_COST,
     +ICAP_FUEL_COST,ICAP_START_COST,ICAP_VOM_COST,ICAP_EMISSION_COST,
     +ANNUAL_ICAP_TOTAL_CAPACITY,MONTHLY_ICAP_TOTAL_CAPACITY,
     +ANNUAL_ICAP_TOTAL_MWH,ANNUAL_ICAP_TOTAL_REVENUE,
     +ANNUAL_ICAP_LEVEL_CAPITAL_COST,ICAP_VAR_COST,ICAP_MRX_EAS,
     +ICAP_TOTAL_COST,ICAP_NET_MARGIN,ICAP_NET_MARGIN_PER_KW,
     +ICAP_NET_MARGIN_PER_MWH,ANNUAL_ICAP_CONE_VALUE,
     +ANNUAL_ICAP_RESERVE_MARGIN,ANNUAL_ICAP_INSTL_CAPACITY_VALUE,
     +ANNUAL_ICAP_DEMAND_POINT,ANNUAL_ICAP_VALUE_POINT,
     +ANNUAL_ICAP_MRX_CONE,ANNUAL_ICAP_MRX_EAS,ANNUAL_ICAP_VAR_COST,
     +ANNUAL_ICAP_FUEL_COST,ANNUAL_ICAP_START_COST,ANNUAL_ICAP_VOM_COST,
     +ANNUAL_ICAP_EMISSION_COST,ANNUAL_ICAP_EAS_REVENUE_OFFSET,
     +MONTHLY_ICAP_VALUE,ANNUAL_ICAP_TOTAL_COST,ANNUAL_ICAP_NET_MARGIN,
     +ANNUAL_ICAP_NET_MARGIN_PER_KW,ANNUAL_ICAP_NET_MARGIN_PER_MWH,
     +ANNUAL_ICAP_MAX_MRX_OR_CONE,ANNUAL_ICAP_ADJ_CAPACITY_VALUE,
     +CL_UNIT_MONTHLY_FIXED_COST,ENERGY,BLOCK_FUEL_COST,TONS_CONVERSION,
     +MONTH_UNIT_START_COSTS,CL_MONTH_NAME,SEAS_HOURS)
! TODOne: EXTRACT_METHOD prepare_icap_summary_report (end)


!

!
         DO I = 1, get_NUNITS()
            UNITNO = I
            call write_trace_int2(file_trace_pclr, "1. UNIT", UNITNO)
            cur_called_from="pcr:1"
            CL_TRANS_NAME = UNITNM(I)//CL_UNIQUE_RPT_STR(I)
            ENRG = SEAS_HOURS * (ENERGY(1,UNITNO) + ENERGY(2,UNITNO))
      call write_trace_real(file_trace_pclr, "3. ENRG", ENRG)
!
! 5/6/00. ADDED FOR ECITIES.
!
            IF(DUKE_RESERVE_CAP_NO == UNITNO) THEN
!
               CAPBLK = GET_DUKE_RESERVE_CAPACITY()
!
            ELSE
               CAPBLK = EFFECTIVE_CAPACITY(1,UNITNO) +
     +                                     EFFECTIVE_CAPACITY(2,UNITNO)
            ENDIF
!
            ANNUAL_CAP(I) = ANNUAL_CAP(I) + MW(2,I)
            ANNUAL_REV_GEN_CAPACITY(I) = ANNUAL_REV_GEN_CAPACITY(I) +
     +                                   REVENUE_GENERATING_CAPACITY(I)
            ANNUAL_EFFECTIVE_CAP(I) = ANNUAL_EFFECTIVE_CAP(I) + CAPBLK
            MONTHS_ACTIVE(I) = MONTHS_ACTIVE(I) + 1
!
! 11/22/04. FISCAL OFF-LINE PROBLEM WITH SRP.
!
            IF (ONLINE(I) .LE. DATE2 .AND. OFLINE(I) .GE. DATE1) THEN
               FISCAL_CAP(I) = FISCAL_CAP(I) + MW(2,I)
              FISCAL_REV_GEN_CAPACITY(I) = FISCAL_REV_GEN_CAPACITY(I) +
     +                                   REVENUE_GENERATING_CAPACITY(I)
               FISCAL_EFFECTIVE_CAP(I) =
     +                                 FISCAL_EFFECTIVE_CAP(I) + CAPBLK
               FISCAL_CL_UNIT_FIXED_COST(I) =
     +                     FISCAL_CL_UNIT_FIXED_COST(I) +
     +                                    CL_UNIT_MONTHLY_FIXED_COST(I)
               FISCAL_MONTHS_ACTIVE(I) = FISCAL_MONTHS_ACTIVE(I) + 1
            ENDIF
! 8/28/02
            CLASS = GET_ASSET_CLASS_NUM(UNITNO)
            P_FUEL_DELIVERY = ABS(GET_P_FUEL_DELIVERY(UNITNO))
            PBTUCT_SAVE = ABS(GET_PBTUCT_SAVE(UNITNO))
!
!
            IF(ENRG > 0.45) THEN
               HEAT = BLK1_HEAT(UNITNO) + BLK2_HEAT(UNITNO)
               FUEL_COST = BLOCK_FUEL_COST(1,UNITNO) +
     +                                        BLOCK_FUEL_COST(2,UNITNO)
               FUEL_COST = FUEL_COST +

     +                             ENRG * FUEL_ADDER_ADJUSTMENT(UNITNO)

! EMISSIONS
!
               CALL RETURN_EMISSIONS_BY_BLOCK(UNITNO,INT2(1),R_SOX,
     +                                       R_NOX,R_CO2,R_OTH2,R_OTH3)
               UNIT_EMISSIONS(1) = R_SOX/TONS_CONVERSION
               UNIT_EMISSIONS(2) = R_NOX/TONS_CONVERSION
               UNIT_EMISSIONS(3) = R_CO2/TONS_CONVERSION
               UNIT_EMISSIONS(4) = R_OTH2/TONS_CONVERSION
               UNIT_EMISSIONS(5) = R_OTH3/TONS_CONVERSION
!
               CALL RETURN_EMISSIONS_BY_BLOCK(UNITNO,INT2(2),R_SOX,
     +                                       R_NOX,R_CO2,R_OTH2,R_OTH3)
               UNIT_EMISSIONS(1) = UNIT_EMISSIONS(1) +
     +                                            R_SOX/TONS_CONVERSION
               UNIT_EMISSIONS(2) = UNIT_EMISSIONS(2) +
     +                                            R_NOX/TONS_CONVERSION
               UNIT_EMISSIONS(3) = UNIT_EMISSIONS(3) +
     +                                            R_CO2/TONS_CONVERSION
               UNIT_EMISSIONS(4) = UNIT_EMISSIONS(4) +
     +                                           R_OTH2/TONS_CONVERSION
               UNIT_EMISSIONS(5) = UNIT_EMISSIONS(5) +
     +                                           R_OTH3/TONS_CONVERSION
!
!
        EMIS_DISPATCH_1 = 0.002*
     +              TRANS_DISPATCH_EMIS_ADDER(1,ISEAS,YR_loc,UNITNO)
        EMIS_DISPATCH_2 = 0.002*
     +              TRANS_DISPATCH_EMIS_ADDER(2,ISEAS,YR_loc,UNITNO)
        EMIS_DISPATCH_3 = 0.002*
     +              TRANS_DISPATCH_EMIS_ADDER(3,ISEAS,YR_loc,UNITNO)
        EMIS_DISPATCH_4 = 0.002*
     +              TRANS_DISPATCH_EMIS_ADDER(4,ISEAS,YR_loc,UNITNO)
        EMIS_DISPATCH_5 = 0.002*
     +              TRANS_DISPATCH_EMIS_ADDER(5,ISEAS,YR_loc,UNITNO)
!       
        UNIT_EMISSIONS_COST(1) =
     +                       UNIT_EMISSIONS(1) * EMIS_DISPATCH_1
        UNIT_EMISSIONS_COST(2) =
     +                       UNIT_EMISSIONS(2) * EMIS_DISPATCH_2
        UNIT_EMISSIONS_COST(3) =
     +                       UNIT_EMISSIONS(3) * EMIS_DISPATCH_3
        UNIT_EMISSIONS_COST(4) =
     +                       UNIT_EMISSIONS(4) * EMIS_DISPATCH_4
        UNIT_EMISSIONS_COST(5) =
     +                       UNIT_EMISSIONS(5) * EMIS_DISPATCH_5
!
            ELSE
               HEAT = 0.D0
               UNIT_EMISSIONS(1) = 0.
               UNIT_EMISSIONS(2) = 0.
               UNIT_EMISSIONS(3) = 0.
               UNIT_EMISSIONS(4) = 0.
               UNIT_EMISSIONS(5) = 0.
!
               UNIT_EMISSIONS_COST(1) = 0.
               UNIT_EMISSIONS_COST(2) = 0.
               UNIT_EMISSIONS_COST(3) = 0.
               UNIT_EMISSIONS_COST(4) = 0.
               UNIT_EMISSIONS_COST(5) = 0.
!
            ENDIF
! 
! 091307. CAPACITY MARKETS. ! 092507. UPDATED
! 103109. MOVED TG, TG_POSITION OUT OF LOOP

            
            ! check this pass variable
            imm_i=I
            TG = MAX(1,get_TRANSACTION_GROUP(imm_I))

            TG_POSITION = MAX(1,GET_TRANS_GROUP_POSITION(TG))
            TG_EffectiveCapacity(TG_POSITION,ISEAS) =
     +                          TG_EffectiveCapacity(TG_POSITION,ISEAS)
     +                           + CAPBLK
! 030311. FOR ODEC.
            IF(CAP_MARKET_ACTIVE(I) .AND.
     +            ONLINE(I) .LE. DATE2 .AND. OFLINE(I) .GE. DATE1) THEN

               TP = GET_CM_FROM_TG(TG_POSITION)

               IF(RUN_MULTIAREA_TRANSACT) THEN ! 102409. FOR BURESH.
! THE INDEX IS A GUESS.
                  CAP_MARKET_REVENUE =
     +                             MONTHLY_ICAP_VALUE(TP)*MW(2,I)*0.001
               ELSE
                  CAP_MARKET_REVENUE = GET_CAP_MARKET_REVENUE(I)
               ENDIF
            ELSE
               CAP_MARKET_REVENUE = 0.0
            ENDIF
!
! NEW VARIABLES FOR TRACKING INPUTS TO OUTPUTS. MSG 3/29/00
!
!
            IF(HEAT > 0.D0) THEN
              AVERAGE_NOX_RATE = TONS_CONVERSION*UNIT_EMISSIONS(2)/HEAT
              AVERAGE_SOX_RATE = TONS_CONVERSION*UNIT_EMISSIONS(1)/HEAT
              AVERAGE_CO2_RATE = TONS_CONVERSION*UNIT_EMISSIONS(3)/HEAT
               AVERAGE_HG_RATE = TONS_CONVERSION*UNIT_EMISSIONS(4)/HEAT
               AVERAGE_FUEL_COST = FUEL_COST/HEAT
            ELSE
               AVERAGE_NOX_RATE = 0.
               AVERAGE_SOX_RATE = 0.
               AVERAGE_CO2_RATE = 0.
               AVERAGE_HG_RATE = 0.
               AVERAGE_FUEL_COST = 0.
            endif

! TODO: EXTRACT_METHOD collect_nox_data (done)
      CALL nox_data_collector(I,UNIT_EMISSIONS,
     +      ANNUAL_NOX_SEASON_EMISSIONS,ANNUAL_NOX_SEASON_THERMAL,HEAT,
     +      FISCAL_NOX_SEASON_EMISSIONS,FISCAL_NOX_SEASON_THERMAL,
     +      TOTAL_NOX_HEAT)


! TODO: EXTRACT_METHOD collect_nox_data (end)
!
            IF(.NOT. TESTING_PLAN .AND. NBLOCK > 0) THEN
               IF(.NOT. CANADA .AND. PHASE_I_UNIT(UNITNO) ) THEN

                  UNIT_NAME = trim(UNITNM(UNITNO))//'*'
               ELSE
                  UNIT_NAME = UNITNM(UNITNO)
               ENDIF
               IF(ENRG > 0.45) THEN
                  AVERAGE_HEATRATE = HEAT_CONVERSION*HEAT/ENRG
                  AVERAGE_FUEL_COST_PER_MWH = FUEL_COST / ENRG

                  AVERAGE_PRODUCTION_COSTS =
     +               SNGL((FUEL_COST))/ENRG+VCPMWH(UNITNO)
                  IF(.NOT. CANADA) AVERAGE_HEATRATE =
     +                                            AVERAGE_HEATRATE + .5
               ELSE
                  FUEL_COST = 0.0 D0
                  HEAT = 0.0 D0
                  AVERAGE_HEATRATE = 0.0
                  AVERAGE_FUEL_COST_PER_MWH = 0.0
                  AVERAGE_PRODUCTION_COSTS = 0.0
               ENDIF
               IF(MW(2,I) > 0.01 .AND. SEAS_HOURS > 0) THEN
                  TRANS_EQUIV_AVAIL =  100. *
     +                           REVENUE_GENERATING_CAPACITY(I)/MW(2,I)
                  TRANS_CAP_FACTOR = ENRG * 100. /
     +                                           (MW(2,I) * SEAS_HOURS)
               ELSE
                  TRANS_EQUIV_AVAIL = 0.0
                  TRANS_CAP_FACTOR = 0.0
               ENDIF
! 050519.
              IF (ONLINE(I) .LE. DATE2 .AND. OFLINE(I) .GE. DATE1) THEN
                  AllHoursMonth(I) = AllHoursMonth(I) + SEAS_HOURS
                  CapHours(I) = CapHours(I) + SEAS_HOURS * MW(2,I)
                  EffectiveCapHours(I) = EffectiveCapHours(I) +
     +                              SEAS_HOURS * CAPBLK
                  AvailHoursMonth(I) = AvailHoursMonth(I) +
     +                              SEAS_HOURS * TRANS_EQUIV_AVAIL *.01
               ENDIF
!
               TOTAL_UNIT_COST =
     +               (SNGL(FUEL_COST) + VCPMWH(UNITNO)*ENRG +
     +                     CL_UNIT_MONTHLY_FIXED_COST(UNITNO))/1000000.
!
               IF(MON_ECO_SALES_ENRG_FROM(UNITNO) > 0.1) THEN
                 AVE_REV_FROM_SALES =  MON_ECO_SALES_REV_FROM(UNITNO) /
     +                                  MON_ECO_SALES_ENRG_FROM(UNITNO)
               ELSE
                  AVE_REV_FROM_SALES =  0.
               ENDIF
               MONTHLY_ECO_SALES = MONTHLY_ECO_SALES +
     +                                   MON_ECO_SALES_REV_FROM(UNITNO)
               MONTHLY_ECO_SALES_ENRG = MONTHLY_ECO_SALES_ENRG +
     +                                  MON_ECO_SALES_ENRG_FROM(UNITNO)
               MONTHLY_ECO_PUCH = MONTHLY_ECO_PUCH +
     +                                   MON_ECO_PUCH_COST_FROM(UNITNO)
               MONTHLY_ECO_PUCH_ENRG = MONTHLY_ECO_PUCH_ENRG +
     +                                   MON_ECO_PUCH_ENRG_FROM(UNITNO)
!
              MONTHLY_TRANS_CAPACITY = MONTHLY_TRANS_CAPACITY + MW(2,I)
               MONTHLY_TRANS_EFFECTIVE_CAP =
     +                             MONTHLY_TRANS_EFFECTIVE_CAP + CAPBLK
               MONTHLY_TRANS_AVAIL_CAP = MONTHLY_TRANS_AVAIL_CAP +
     +                                   REVENUE_GENERATING_CAPACITY(I)
               MONTHLY_TRANS_ENERGY = MONTHLY_TRANS_ENERGY + ENRG
               MONTHLY_TRANS_HEAT = MONTHLY_TRANS_HEAT + HEAT
               MONTHLY_TRANS_FUEL_COST = MONTHLY_TRANS_FUEL_COST +
     +                                                        FUEL_COST
               MONTHLY_TRANS_VAR_COST = MONTHLY_TRANS_VAR_COST +
     +                                              VCPMWH(UNITNO)*ENRG
!
               MONTHLY_UNIT_EMISSIONS(1) = MONTHLY_UNIT_EMISSIONS(1) +
     +                                                UNIT_EMISSIONS(1)
               MONTHLY_UNIT_EMISSIONS(2) = MONTHLY_UNIT_EMISSIONS(2) +
     +                                                UNIT_EMISSIONS(2)
               MONTHLY_UNIT_EMISSIONS(3) = MONTHLY_UNIT_EMISSIONS(3) +
     +                                                UNIT_EMISSIONS(3)
               MONTHLY_UNIT_EMISSIONS(4) = MONTHLY_UNIT_EMISSIONS(4) +
     +                                                UNIT_EMISSIONS(4)
               MONTHLY_UNIT_EMISSIONS(5) = MONTHLY_UNIT_EMISSIONS(5) +
     +                                                UNIT_EMISSIONS(5)
!
               MONTHLY_UNIT_EMISSIONS_COST(1,UNITNO) =
     +               MONTHLY_UNIT_EMISSIONS_COST(1,UNITNO) +
     +                                           UNIT_EMISSIONS_COST(1)
               MONTHLY_UNIT_EMISSIONS_COST(2,UNITNO) =
     +               MONTHLY_UNIT_EMISSIONS_COST(2,UNITNO) +
     +                                           UNIT_EMISSIONS_COST(2)
               MONTHLY_UNIT_EMISSIONS_COST(3,UNITNO) =
     +               MONTHLY_UNIT_EMISSIONS_COST(3,UNITNO) +
     +                                           UNIT_EMISSIONS_COST(3)
               MONTHLY_UNIT_EMISSIONS_COST(4,UNITNO) =
     +               MONTHLY_UNIT_EMISSIONS_COST(4,UNITNO) +
     +                                           UNIT_EMISSIONS_COST(4)
               MONTHLY_UNIT_EMISSIONS_COST(5,UNITNO) =
     +               MONTHLY_UNIT_EMISSIONS_COST(5,UNITNO) +
     +                                           UNIT_EMISSIONS_COST(5)
!
               ANNUAL_UNIT_EMISSIONS_COST(I,1) =
     +               ANNUAL_UNIT_EMISSIONS_COST(I,1) +
     +                                           UNIT_EMISSIONS_COST(1)
               ANNUAL_UNIT_EMISSIONS_COST(I,2) =
     +               ANNUAL_UNIT_EMISSIONS_COST(I,2) +
     +                                           UNIT_EMISSIONS_COST(2)
               ANNUAL_UNIT_EMISSIONS_COST(I,3) =
     +               ANNUAL_UNIT_EMISSIONS_COST(I,3) +
     +                                           UNIT_EMISSIONS_COST(3)
               ANNUAL_UNIT_EMISSIONS_COST(I,4) =
     +               ANNUAL_UNIT_EMISSIONS_COST(I,4) +
     +                                           UNIT_EMISSIONS_COST(4)
               ANNUAL_UNIT_EMISSIONS_COST(I,5) =
     +               ANNUAL_UNIT_EMISSIONS_COST(I,5) +
     +                                           UNIT_EMISSIONS_COST(5)
!
               MONTHLY_TRANS_FIXED_COST = MONTHLY_TRANS_FIXED_COST +
     +                                    CL_UNIT_MONTHLY_FIXED_COST(I)
!

         imm_i=I
         FT = MAX(int(1,2),
     + MIN(int(GET_PRIMARY_MOVER(imm_I),2),
     + int(max_fuel_types_clr,2)))
        imm_i=i
               SP = MIN(MAX(int(0,2),
     + int(GET_UNIT_STATE_PROVINCE_INDEX(imm_I)),2),
     +  int(MAX_STATE_PROVINCE_NO),2)
       imm_i=i
               GSP = MIN(MAX(int(0,2),
     + int(GET_UNIT_GAS_REGION_INDEX(imm_I)),2),
     + int(MAX_GAS_REGION_NO,2))

         imm_i=i
               TEMP_L1 = GET_CL_BASECASE_MARKET_ID(imm_I,MARKET_ID)

               MK = MARKET_AREA_LOOKUP(MARKET_ID(1:5))
               IF(MK < 0 .OR. MK > 600) THEN
                 MK =MAX(0,MIN(600,MARKET_AREA_LOOKUP(MARKET_ID(1:5))))
               ENDIF
               GET_PRIMARY_MOVER_INDEX_called_from="pclr:0001"
               imm_i=i
               PM = GET_PRIMARY_MOVER_INDEX(imm_I)
               RTG = TG
               RMK = MK
               RFT = FT
               imm_i=i
               RSP = FLOAT(GET_THERMAL_STATE_INDEX(imm_I))
               CHARGING_ENERGY = 0.0
!
               RPM = PM

               imm_i=i
               HESI_ID_NUM_4_UNIT = GET_HESI_UNIT_ID_NUM(imm_i,
     +                  HESI_SECOND_UNIT_ID_NUM)
               POWERDAT_PLANT_ID = GET_POWERDAT_PLANT_ID(imm_I)
               NEWGEN_UNIT_STATUS = FLOAT(GET_NEWGEN_INDEX(imm_I))

!
               ONLINE_DATE = 190000.+FLOAT(ONLINE(imm_I))
               OFLINE_DATE = 190000.+FLOAT(OFLINE(imm_I))
               call write_offline_trace_r4("period_cl_report_pclr:0007", 
     + ofline_date)
!
               SINGLE_FUEL_COST = SNGL(FUEL_COST)/1000000.
!
! 072006. TEMPORARY FOR BURESH.
!
               IF(ABS(FUELMX(I)) < 0.99999 .AND.
     +                      SEC_FUEL_TYPE(I) /= PRIM_FUEL_TYPE(I)) THEN
                  IF( SEC_FUEL_TYPE(I) == 'N' .OR.
     +                           SEC_FUEL_TYPE(I) == 'G') THEN
                     SD = 2
                  ELSEIF( SEC_FUEL_TYPE(I) == 'F' .OR.
     +                           SEC_FUEL_TYPE(I) == 'J' .OR.
     +                                 SEC_FUEL_TYPE(I) == 'K' .OR.
     +                                    SEC_FUEL_TYPE(I) == 'D') THEN
                     SD = 3
                  ELSE
                     SD = FT
                  ENDIF
                  PRIMARY_HEAT = ABS(FUELMX(I)) * SNGL(HEAT)
                  SECONDARY_HEAT = SNGL(HEAT) - PRIMARY_HEAT
                  IF(ABS(FUELMX(I)) > 0.001) THEN
                     PRIMARY_FUEL_COST = MAX(0.,SINGLE_FUEL_COST -
     +                                SECONDARY_HEAT*SBTUCT(I)*.000001)
                  ELSE
                     PRIMARY_FUEL_COST = 0.0
                  ENDIF
                  SECONDARY_FUEL_COST =
     +                     MAX(0.,SINGLE_FUEL_COST - PRIMARY_FUEL_COST)
! PRIMARY
                  FUEL_COST_BY_TG_BY_FUEL(TG_POSITION,FT) =
     +                 FUEL_COST_BY_TG_BY_FUEL(TG_POSITION,FT) +
     +                                                PRIMARY_FUEL_COST
                  ANNUAL_FUEL_COST_BY_TG_BY_FUEL(TG_POSITION,FT) =
     +                 ANNUAL_FUEL_COST_BY_TG_BY_FUEL(TG_POSITION,FT) +
     +                                                PRIMARY_FUEL_COST
                  FUEL_BTUS_BY_TG_BY_FUEL(TG_POSITION,FT) =
     +                 FUEL_BTUS_BY_TG_BY_FUEL(TG_POSITION,FT) +
     +                                                     PRIMARY_HEAT
                  ANNUAL_FUEL_BTUS_BY_TG_BY_FUEL(TG_POSITION,FT) =
     +                 ANNUAL_FUEL_BTUS_BY_TG_BY_FUEL(TG_POSITION,FT) +
     +                                                     PRIMARY_HEAT
! SECONDARY
                  FUEL_COST_BY_TG_BY_FUEL(TG_POSITION,SD) =
     +                 FUEL_COST_BY_TG_BY_FUEL(TG_POSITION,SD) +
     +                                              SECONDARY_FUEL_COST
                  ANNUAL_FUEL_COST_BY_TG_BY_FUEL(TG_POSITION,SD) =
     +                 ANNUAL_FUEL_COST_BY_TG_BY_FUEL(TG_POSITION,SD) +
     +                                              SECONDARY_FUEL_COST
                  FUEL_BTUS_BY_TG_BY_FUEL(TG_POSITION,SD) =
     +                 FUEL_BTUS_BY_TG_BY_FUEL(TG_POSITION,SD) +
     +                                                   SECONDARY_HEAT
                  ANNUAL_FUEL_BTUS_BY_TG_BY_FUEL(TG_POSITION,SD) =
     +                 ANNUAL_FUEL_BTUS_BY_TG_BY_FUEL(TG_POSITION,SD) +
     +                                                   SECONDARY_HEAT

               ELSE
!
                  FUEL_COST_BY_TG_BY_FUEL(TG_POSITION,FT) =
     +                 FUEL_COST_BY_TG_BY_FUEL(TG_POSITION,FT) +
     +                                                 SINGLE_FUEL_COST
                  ANNUAL_FUEL_COST_BY_TG_BY_FUEL(TG_POSITION,FT) =
     +                 ANNUAL_FUEL_COST_BY_TG_BY_FUEL(TG_POSITION,FT) +
     +                                                 SINGLE_FUEL_COST
                  FUEL_BTUS_BY_TG_BY_FUEL(TG_POSITION,FT) =
     +                 FUEL_BTUS_BY_TG_BY_FUEL(TG_POSITION,FT) +
     +                                                       SNGL(HEAT)
                  ANNUAL_FUEL_BTUS_BY_TG_BY_FUEL(TG_POSITION,FT) =
     +                 ANNUAL_FUEL_BTUS_BY_TG_BY_FUEL(TG_POSITION,FT) +
     +                                                       SNGL(HEAT)
               ENDIF


               PROD_BY_MK_BY_MWH(MK,1) = PROD_BY_MK_BY_MWH(MK,1) + ENRG
               PROD_BY_MK_BY_MWH(MK,2) = PROD_BY_MK_BY_MWH(MK,2) +
     +                                                          MW(2,I)
               PROD_BY_MK_BY_MWH(MK,3) = PROD_BY_MK_BY_MWH(MK,3) + 1
               ANNUAL_PROD_BY_MK_BY_MWH(MK,1) =
     +                            ANNUAL_PROD_BY_MK_BY_MWH(MK,1) + ENRG
               ANNUAL_PROD_BY_MK_BY_MWH(MK,2) =
     +                         ANNUAL_PROD_BY_MK_BY_MWH(MK,2) + MW(2,I)
               ANNUAL_PROD_BY_MK_BY_MWH(MK,3) =

     +                                ANNUAL_PROD_BY_MK_BY_MWH(MK,3) + 1


               IF(TRANSACT_PROD_REPORT_ACTIVE) THEN
!
               IF( ABS(HESI_ID_NUM_4_UNIT+999999.0) > 2) THEN ! PER KJ


       call check_capacity_array_bounds("mw_by_tg_by_fuel",
     + mw_by_tg_by_fuel,int(tg_position), int(tft))
       call check_capacity_array_bounds("mwh_by_tg_by_fuel",
     + mwh_by_tg_by_fuel,int(tg_position), int(tft))
       call check_capacity_array_bounds("annual_mwh_by_tg_by_fuel",
     + annual_mwh_by_tg_by_fuel,int(tg_position), int(tft))
       call check_capacity_array_bounds("annual_mwh_by_tg_by_fuel",
     + annual_mwh_by_tg_by_fuel,int(tg_position), int(tft))
                     IF(FT == 6) THEN
                        IF(PM == 2) THEN
                           TFT = 7
                        ELSEIF(PM == 3) THEN
                           TFT = 8
                        ELSEIF(PM == 7) THEN
                           TFT = 9
                        ELSEIF(PM == 9) THEN
                           TFT = 10
                        ELSEIF(PM == 11) THEN
                           TFT = 11
                        ELSEIF(PM == 12) THEN
                           TFT = 12
                        ELSEIF(PM == 14) THEN
                           TFT = 14
                        ELSEIF(PM == 15) THEN
                           TFT = 15
                        ELSE
                           TFT = 13
                        ENDIF
!
                        MW_BY_TG_BY_FUEL(TG_POSITION,TFT) =
     +                      MW_BY_TG_BY_FUEL(TG_POSITION,TFT) + MW(2,I)
                        MWH_BY_TG_BY_FUEL(TG_POSITION,TFT) =
     +                        MWH_BY_TG_BY_FUEL(TG_POSITION,TFT) + ENRG
                        ANNUAL_MWH_BY_TG_BY_FUEL(TG_POSITION,TFT) =
     +                      ANNUAL_MWH_BY_TG_BY_FUEL(TG_POSITION,TFT) +
     +                                                            ENRG
                        IF(ISEAS == 12) THEN
                           ANNUAL_MW_BY_TG_BY_FUEL(TG_POSITION,TFT) =
     +                       ANNUAL_MW_BY_TG_BY_FUEL(TG_POSITION,TFT) +
     +                                                          MW(2,I)
                        ENDIF
                     ENDIF
!
                     MW_BY_TG_BY_FUEL(TG_POSITION,FT) =
     +                  MW_BY_TG_BY_FUEL(TG_POSITION,FT) + MW(2,I)
                     MWH_BY_TG_BY_FUEL(TG_POSITION,FT) =
     +                  MWH_BY_TG_BY_FUEL(TG_POSITION,FT) + ENRG
                     ANNUAL_MWH_BY_TG_BY_FUEL(TG_POSITION,FT) =
     +                  ANNUAL_MWH_BY_TG_BY_FUEL(TG_POSITION,FT) + ENRG
!
                     MW_BY_TG_BY_FUEL(TG_POSITION,MFT1) =
     +                   MW_BY_TG_BY_FUEL(TG_POSITION,MFT1) + MW(2,I)
                     MWH_BY_TG_BY_FUEL(TG_POSITION,MFT1) =
     +                  MWH_BY_TG_BY_FUEL(TG_POSITION,MFT1) + ENRG
                     ANNUAL_MWH_BY_TG_BY_FUEL(TG_POSITION,MFT1) =
     +                  ANNUAL_MWH_BY_TG_BY_FUEL(TG_POSITION,MFT1) +
     +                                                            ENRG
!PER KJ.
                     IF(ISEAS == 12) THEN
                        ANNUAL_MW_BY_TG_BY_FUEL(TG_POSITION,FT) =
     +                    ANNUAL_MW_BY_TG_BY_FUEL(TG_POSITION,FT) +
     +                                                          MW(2,I)
                        ANNUAL_MW_BY_TG_BY_FUEL(TG_POSITION,MFT1) =
     +                      ANNUAL_MW_BY_TG_BY_FUEL(TG_POSITION,MFT1) +
     +                                                          MW(2,I)
                     ENDIF
                  ENDIF
!
       call check_capacity_array_bounds("prod_by_tg_by_mwh",
     + prod_by_tg_by_mwh,int(tg_position), int(PM))
       call check_capacity_array_bounds("prod_by_tg_by_mw",
     + prod_by_tg_by_mw,int(tg_position), int(PM))
                  PROD_BY_TG_BY_MWH(TG_POSITION,PM) =
     +                  PROD_BY_TG_BY_MWH(TG_POSITION,PM) + ENRG

     
                  ANNUAL_PROD_BY_TG_BY_MWH(TG_POSITION,PM) =
     +                  ANNUAL_PROD_BY_TG_BY_MWH(TG_POSITION,PM) + ENRG
       call write_trace_real4(file_trace_pclr, "Ann. Prod/TG/MWH", 
     + ANNUAL_PROD_BY_TG_BY_MWH(TG_position, PM))

                  PROD_BY_TG_BY_MW(TG_POSITION,PM) =
     +                  PROD_BY_TG_BY_MW(TG_POSITION,PM) +  MW(2,I)
       call write_trace_real4(file_trace_pclr, "Prod/TG/MW", 
     + PROD_BY_TG_BY_MW(TG_POSITION, PM))
       call write_trace_int2(file_trace_pclr, "PM", PM)
      call write_trace_int2(file_trace_pclr, "TG_POSITION", TG_POSITION)

                  IF(PM == 10) THEN ! STEAM

                     LM = MIN(PM + FT + 4,max_prod_types_clr)
                     PROD_BY_TG_BY_MWH(TG_POSITION,LM) =
     +                  PROD_BY_TG_BY_MWH(TG_POSITION,LM) +
     + ENRG
                    if(PROD_BY_TG_BY_MWH(tg_position,lm)>0.0) then
                        LM=LM!debugstop
                    endif
                     ANNUAL_PROD_BY_TG_BY_MWH(TG_POSITION,LM) =
     +                  ANNUAL_PROD_BY_TG_BY_MWH(TG_POSITION,LM) +
     + ENRG
                     if(LM>15 .and. lm<18) then
                        lm=lm!debugstop
                     endif
                     PROD_BY_TG_BY_MW(TG_POSITION,LM) =
     +                  PROD_BY_TG_BY_MW(TG_POSITION,LM) + MW(2,I)
       call write_trace_real4(file_trace_pclr, "prod/tg/mw", 
     + prod_by_tg_by_mw(TG_position, LM))
       call write_trace_int2(file_trace_pclr, "LM", LM)
                  ENDIF
!
       call write_trace_int2(file_trace_pclr, "TG_POSITION", 
     +     tg_position)
                  PROD_BY_TG_BY_MWH(TG_POSITION,14) =
     +                  PROD_BY_TG_BY_MWH(TG_POSITION,14) + ENRG
      call write_trace_real4(file_trace_pclr, "prod/tg/mwh->14" ,
     + prod_by_tg_by_mwh(TG_POSITION, 14))
     
                  ANNUAL_PROD_BY_TG_BY_MWH(TG_POSITION,14) =
     +                  ANNUAL_PROD_BY_TG_BY_MWH(TG_POSITION,14) + ENRG
                  PROD_BY_TG_BY_MW(TG_POSITION,14) =
     +                  PROD_BY_TG_BY_MW(TG_POSITION,14) + MW(2,I)

!
               ENDIF

!
               IF(FUEL_PRICE_DATA_AVAILABLE) THEN
                  CALL
     +              RETURN_HEAT_BY_FUEL(I,PRIM_HEAT,SEC_HEAT,EMIS_HEAT)
        IF(PRIM_HEAT > 0.) THEN
           CALL GET_P_FUEL_PRICE_CONVERSION(
     +                          I,FUEL_PRICE_CONVERSION,globecom_year)
           P_FUEL_CONSUMPTION =
     +                      PRIM_HEAT * FUEL_PRICE_CONVERSION
        ELSE
           P_FUEL_CONSUMPTION = 0.
        ENDIF
         IF(SEC_HEAT > 0.) THEN
            CALL GET_S_FUEL_PRICE_CONVERSION(
     +                           I,FUEL_PRICE_CONVERSION,globecom_year)
            S_FUEL_CONSUMPTION =
     +                        SEC_HEAT * FUEL_PRICE_CONVERSION
         ELSE
            S_FUEL_CONSUMPTION = 0.
         ENDIF
           IF(EMIS_HEAT > 0.) THEN
              CALL GET_E_FUEL_PRICE_CONVERSION(
     +     I,FUEL_PRICE_CONVERSION,globecom_year)
              E_FUEL_CONSUMPTION =
     +             EMIS_HEAT * FUEL_PRICE_CONVERSION
           ELSE
              E_FUEL_CONSUMPTION = 0.
           ENDIF
               ELSE
                  P_FUEL_CONSUMPTION =
     +                             SNGL(HEAT)*PRIM_MOVER_CONVERSION(FT)
               ENDIF
!
               MONTHLY_P_HEAT = MONTHLY_P_HEAT + P_FUEL_CONSUMPTION
               MONTHLY_S_HEAT = MONTHLY_S_HEAT + S_FUEL_CONSUMPTION
               MONTHLY_E_HEAT = MONTHLY_E_HEAT + E_FUEL_CONSUMPTION
!
              ANNUAL_UNIT_START_COSTS(I) = ANNUAL_UNIT_START_COSTS(I) +
     +                                        MONTH_UNIT_START_COSTS(I)
               MONTHLY_START_COSTS = MONTHLY_START_COSTS +
     +                                        MONTH_UNIT_START_COSTS(I)
!
               MONTHLY_CAPACITY_REVENUE = MONTHLY_CAPACITY_REVENUE +
     +                                               CAP_MARKET_REVENUE
               ANNUAL_UNIT_CAPACITY_REVENUE(I) =
     +                        ANNUAL_UNIT_CAPACITY_REVENUE(I) +
     +                                               CAP_MARKET_REVENUE
!
               ANNUAL_UNIT_STARTS(I) = ANNUAL_UNIT_STARTS(I) +
     +                                             MONTH_UNIT_STARTS(I)
               MONTHLY_STARTS = MONTHLY_STARTS + MONTH_UNIT_STARTS(I)
!
               ANNUAL_P_FUEL_CONSUMPTION(I) =
     +                           ANNUAL_P_FUEL_CONSUMPTION(I) +
     +                                               P_FUEL_CONSUMPTION
               ANNUAL_S_FUEL_CONSUMPTION(I) =
     +                           ANNUAL_S_FUEL_CONSUMPTION(I) +
     +                                               S_FUEL_CONSUMPTION
               ANNUAL_E_FUEL_CONSUMPTION(I) =
     +                           ANNUAL_E_FUEL_CONSUMPTION(I) +
     +                                               E_FUEL_CONSUMPTION
               WHOLESALE_PRODUCTION_COST =
     +                  AVERAGE_PRODUCTION_COSTS *
     +                         MON_ECO_SALES_ENRG_FROM(UNITNO)/1000000.
               ANNUAL_WHOLESALE_PROD_COST(I) =
     +                     ANNUAL_WHOLESALE_PROD_COST(I) +
     +                                        WHOLESALE_PRODUCTION_COST
               RETAIL_SALES = MAX(0.,
     +                  (ENRG - MON_ECO_SALES_ENRG_FROM(UNITNO))/1000.)
               MONTHLY_WHOLE_COSTS = MONTHLY_WHOLE_COSTS +
     +                                        WHOLESALE_PRODUCTION_COST
               MONTHLY_WHOLESALE_FUEL_COSTS =
     +                           MONTHLY_WHOLESALE_FUEL_COSTS +
     +                              AVERAGE_FUEL_COST_PER_MWH *
     +                         MON_ECO_SALES_ENRG_FROM(UNITNO)/1000000.
               MONTHLY_WHOLESALE_VOM_COSTS =
     +                           MONTHLY_WHOLESALE_VOM_COSTS +
     +                              VCPMWH(UNITNO) *

     +                          MON_ECO_SALES_ENRG_FROM(UNITNO)/1000000.
! TODO: EXTRACT_METHOD collect_fiscal_data (Done)
          CALL collect_fiscal_data(I,UNITNO,FISCAL_ECO_SALES_REV_FROM,
     +         FISCAL_ECO_SALES_ENRG_FROM,FISCAL_ECO_PUCH_COST_FROM,
     +         FISCAL_ECO_PUCH_ENRG_FROM,FISCAL_P_FUEL_CONSUMPTION,
     +         FISCAL_S_FUEL_CONSUMPTION,FISCAL_E_FUEL_CONSUMPTION,
     +         P_FUEL_CONSUMPTION,FISCAL_UNIT_STARTS,
     +         FISCAL_UNIT_START_COSTS,MONTH_UNIT_STARTS,
     +         MONTH_UNIT_START_COSTS,FISCAL_WHOLESALE_PROD_COST,
     +         WHOLESALE_PRODUCTION_COST,FISCAL_UNIT_EMISSIONS_COST,
     +         UNIT_EMISSIONS_COST)

! TODO: EXTRACT_METHOD collect_fiscal_data (end)

! TODO: EXTRACT_METHOD collect_emissions_costs (done)
      CALL collect_emissions_costs(EMISSIONS_COST_BY_SP,
     +                             UNIT_EMISSIONS_COST,
     +                             SP)

! TODO: EXTRACT_METHOD collect_emissions_costs (end)

! TODO: EXTRACT_METHOD collect_group_data (Done)

      CALL collect_group_data(I,SP,CAPBLK,ENRG,HEAT,UNITNO,FUEL_COST,
     +                         GROUP_VAR_BY_SP,ANNUAL_GROUP_VAR_BY_SP,
     +                         CL_UNIT_MONTHLY_FIXED_COST,
     +                         UNIT_EMISSIONS,TOTAL_UNIT_COST)
! TODO: EXTRACT_METHOD collect_group_data (end)


! TODO: EXTRACT_METHOD collect_fuel_data

               FUEL_COST_BY_SP_BY_FUEL(SP,FT) =
     +                 FUEL_COST_BY_SP_BY_FUEL(SP,FT) +
     +                                                 SINGLE_FUEL_COST
               ANNUAL_FUEL_COST_BY_SP_BY_FUEL(SP,FT) =
     +                  ANNUAL_FUEL_COST_BY_SP_BY_FUEL(SP,FT) +
     +                                                 SINGLE_FUEL_COST
               FUEL_BTUS_BY_SP_BY_FUEL(SP,FT) =
     +                 FUEL_BTUS_BY_SP_BY_FUEL(SP,FT) +
     +                                                       SNGL(HEAT)
               ANNUAL_FUEL_BTUS_BY_SP_BY_FUEL(SP,FT) =
     +                  ANNUAL_FUEL_BTUS_BY_SP_BY_FUEL(SP,FT) +
     +                                                       SNGL(HEAT)
               IF(FT == 2) THEN ! FOR THE GAS MODEL.
                  FUEL_BTUS_BY_GSP_BY_FUEL(GSP,1) =
     +                 FUEL_BTUS_BY_GSP_BY_FUEL(GSP,1) + SNGL(HEAT)
                  ANNUAL_FUEL_BTUS_BY_GSP_BY_FUEL(GSP,1) =
     +                  ANNUAL_FUEL_BTUS_BY_GSP_BY_FUEL(GSP,1) +
     +                                                       SNGL(HEAT)
! THE HEAT PEAK DAY HEAT SHOULD BE A FUNCTION OF THE SIZE OF THE
! MONTHLY CF
                  FUEL_BTUS_BY_GSP_BY_FUEL(GSP,2) =
     +                  FUEL_BTUS_BY_GSP_BY_FUEL(GSP,2) +
     +                                        SNGL(HEAT)/DAYS_IN_MONTH
                  ANNUAL_FUEL_BTUS_BY_GSP_BY_FUEL(GSP,2) =
     +                  ANNUAL_FUEL_BTUS_BY_GSP_BY_FUEL(GSP,2) +
     +                                        SNGL(HEAT)/DAYS_IN_MONTH
                  FUEL_BTUS_BY_GSP_BY_FUEL(GSP,3) =
     +                 FUEL_BTUS_BY_GSP_BY_FUEL(GSP,3) + 1
                  ANNUAL_FUEL_BTUS_BY_GSP_BY_FUEL(GSP,3) =
     +                  ANNUAL_FUEL_BTUS_BY_GSP_BY_FUEL(GSP,3) + 1
               ENDIF
!
! 120410. FOR RETIREMENTS LOGIC
!
               TEMP_POINTER = GET_POINTER_FOR_NEW_CL_UNIT(UNITNO)
               IF(TEMP_POINTER > 0) THEN
                  CAPITAL_COST_OF_BUILD =
     +    RETURN_SCREEN_CAP_COST(TEMP_POINTER,globecom_year)/12.
               ELSE
                  CAPITAL_COST_OF_BUILD = 0.0
               ENDIF
               TOTAL_EMISSION_COSTS = SUM(UNIT_EMISSIONS_COST)
               UNIT_EBITDA = CAP_MARKET_REVENUE +
     +                      MON_ECO_SALES_REV_FROM(UNITNO)/1000000. -
     +                      (TOTAL_UNIT_COST + TOTAL_EMISSION_COSTS +
     +                                        CAPITAL_COST_OF_BUILD)
               ANNUAL_EBITDA(UNITNO) = ANNUAL_EBITDA(UNITNO) +
     +                                                      UNIT_EBITDA
!
!

               IF(TG <= MAX_TRANS_GROUP_NUMBER .AND.
     +                        TRANS_GROUP_REPORTING_ACTIVE(TG) .AND.
     +                             REPORT_MARKET_AREA(MARKET_ID) .AND.
     +                                REPORT_THIS_CL_UNIT(UNITNO)) THEN

!
                  IF(TRANS_EQUIV_AVAIL > 0.) THEN
                     UNECONOMIC_RATE = 100.*(1. -
     +                            (TRANS_CAP_FACTOR/TRANS_EQUIV_AVAIL))
                  ELSE
                     UNECONOMIC_RATE = 0.
                  ENDIF
!

                  MONTHLY_CAPITAL_COST_OF_BUILD =
     +                  MONTHLY_CAPITAL_COST_OF_BUILD +
     +                                           CAPITAL_COST_OF_BUILD
                  ANNUAL_CAPITAL_COST_OF_BUILD(UNITNO) =
     +                       ANNUAL_CAPITAL_COST_OF_BUILD(UNITNO) +
     +                           CAPITAL_COST_OF_BUILD
                  FISCAL_CAPITAL_COST_OF_BUILD(UNITNO) =
     +                  FISCAL_CAPITAL_COST_OF_BUILD(UNITNO) +
     +                           CAPITAL_COST_OF_BUILD
!

                  MONTHLY_EBITDA = MONTHLY_EBITDA + UNIT_EBITDA
                  MONTHLY_CHARGING_ENERGY = MONTHLY_CHARGING_ENERGY +
     +                                                  CHARGING_ENERGY

                  FISCAL_EBITDA(UNITNO) = FISCAL_EBITDA(UNITNO) +
     +                                                      UNIT_EBITDA

! Ignore fractional component when determining variable O&M rate.
                 IF(FUEL_COST > 0.) THEN
                    VariableOMRate =  VCPMWH(UNITNO)
                 ELSE
                    VariableOMRate = 0.
                 ENDIF

                 IF(MONTHLY_TRANS_REPORT) THEN


                   MON_CL_TRANS_UNIT_REC = RPTREC(MON_CL_TRANS_UNIT_NO)

                   call write_unique_msgmtrcl_log_entry(1) ! debugmod

! This is the monthly transact report (monthly thermal units).
!msgmtrcl monthly thermal units.  The third write is also
! a monthly_trans_report, so document differences.
! todo: msgmtrcl to trace

                   WRITE(MON_CL_TRANS_UNIT_NO,REC=MON_CL_TRANS_UNIT_REC)
     +               PRT_ENDPOINT(),
     +               fLocal_year,
     +          CL_MONTH_NAME(ISEAS),! Can be season, or an index value
     +               CL_TRANS_NAME,
     +               FLOAT(I), ! 0 - Unit
     +               MW(2,I), ! 1 - Unit Capacity (MW)
     +               CAPBLK, ! 2 - Effective Capacity (MW)
     +               TRANS_EQUIV_AVAIL, ! 3 - Equivalent Availability
     +               TRANS_CAP_FACTOR, ! 4 - Capacity Factor
     +               ENRG/1000., ! 5 - Unit Generation (GWh)
     +               SNGL(HEAT), ! 6 - Thermal Input (MMBTUs)
     +               AVERAGE_HEATRATE, ! 7 Average Heatrate
     +               SNGL(FUEL_COST)/1000000., ! 8 - Fuel Cost ($/MMW)
     +               VariableOMRate*ENRG/1000000., 
     +               AVERAGE_PRODUCTION_COSTS,

                     ! 10: Avg. Generating Cost ($/MWh)
                     ! 11: SO2 Emissions (Tons)
                     ! 12: NOx Emssions (Tons)
                     ! 13: CO2 Emissions (Tons)
                     ! 14: Hg Emissions (Tons)
                     ! 15: Other Emissions (Tons)
     +               (UNIT_EMISSIONS(EM),
     +                     EM=1,NUMBER_OF_EMISSION_TYPES), !11-15
     +               ! 16 - Fixed Cost ($/MMW)
     +               CL_UNIT_MONTHLY_FIXED_COST(UNITNO)/1000000.,
     +               TOTAL_UNIT_COST, ! 17 Total Production Cost ($/MMW)
     + ! 18 - Wholesale market revenue ($/MWh)
     +               MON_ECO_SALES_REV_FROM(UNITNO)/1000000., 
     + ! 19 - Wholesale Market Sold ($/MWh)
     +               MON_ECO_SALES_ENRG_FROM(UNITNO)/1000.,
     +           AVE_REV_FROM_SALES, !20 - Avg. Market Revenue ($/MWh)
     +               MON_ECO_SALES_REV_FROM(UNITNO)/1000000. -
     +                 TOTAL_UNIT_COST, ! 21 - Gross Margin ($/MMW)
     + ! 22 - Wholesale Market Cost ($/MMW)
     +               MON_ECO_PUCH_COST_FROM(UNITNO)/1000000.,
     + ! 23 - Whilesale Market Bought ($/MMW)
     +               MON_ECO_PUCH_ENRG_FROM(UNITNO)/1000.,
     +             P_FUEL_CONSUMPTION, ! 24 - Primary Fuel Consumption
     +            S_FUEL_CONSUMPTION, ! 25 - Secondary Fuel Consumption
     +             E_FUEL_CONSUMPTION, ! 26 - Tertiary Fuel Consumption
     +               AVERAGE_NOX_RATE, ! 27 - NOX avg. emiss. rate
     +               AVERAGE_SOX_RATE, ! 28 - SOX avg. emiss. rate
     +               AVERAGE_FUEL_COST, ! 29 - Av. fuel cost
     + VariableOMRate, ! VCPMWH(UNITNO), ! 30 - Variable O&M cost, avg.
!
!                     ! 31 - SO2 emissions cost
!                     ! 32 - NOx emissions cost
!                     ! 33 - CO2 emissions cost
!                     ! 34 - Hg emissions cost
!                     ! 35 - Other emissions cost
     +               (UNIT_EMISSIONS_COST(EM),
     +                     EM=1,NUMBER_OF_EMISSION_TYPES), !31-35
     +               WHOLESALE_PRODUCTION_COST, ! 36 - ($/MMW)
     +               RETAIL_SALES, ! 37 - Retail Market Sold (GWh)
     +               RTG, ! 38 - Transaction Group
     +               RMK, ! 39 - Zone
     +               RFT, ! 40 - Fuel Type
     +               HESI_ID_NUM_4_UNIT, ! 41 - Unit ID
     +               POWERDAT_PLANT_ID, ! 42 - Plant ID
     +               FLOAT(MONTH_UNIT_STARTS(UNITNO)), ! 43 - Start-ups
     +        TOTAL_EMISSION_COSTS, ! 44 - Total emission costs ($/MMW)
     +  MONTH_UNIT_START_COSTS(UNITNO)/1000000., ! 45 - Start-up costs
     +               FLOAT(CLASS), ! 46 - Asset class
     +               PBTUCT_SAVE, ! 47 - Fuel hub index
     +               P_FUEL_DELIVERY, ! 48 - Fuel Basis index
     +               UNECONOMIC_RATE, ! 49 - Uneconomic rate (%)
     +            NEWGEN_UNIT_STATUS, ! 50 - New entrants unit status
     +               ONLINE_DATE, ! 51 - online date
     +               OFLINE_DATE, ! 52 - offline date
     +               RPM, ! 53 - Prime Mover
     +               HESI_SECOND_UNIT_ID_NUM, ! 54 - Second unit id
     +               EV_DATA_SOURCE, ! 55 - EV Data Source
     +               EV_PLANNING_AREA, ! 56 - EV Planning Area
     +              CAP_MARKET_REVENUE, ! 57 - Capacity Revenue ($/MMW)
     +  CAPITAL_COST_OF_BUILD, ! 58 - Capital Cost (MRX units) ($/MMW)
     +               UNIT_EBITDA,   ! 59 - EBITDA ($/MMW)
     +               RSP, ! 60 - State/Province
     +               CHARGING_ENERGY/1000. ! 61 - Charging energy (GWh)

       
       call write_trace_message(file_trace_pclr, "1. msgmctrl write.")
       
       ! Bypassing internal check in debugtrace to be able to write
       ! the trace entry in one call. This is done by checking the
       ! value of BAD_TRACE_HANDLE locally. (copy/pasted above write
       ! and compressed arguments to reduce codebase intrusion).
       if(file_trace_pclr/=BAD_TRACE_HANDLE) then
           WRITE(file_trace_pclr,*)
     + "PRT_ENDPOINT", PRT_ENDPOINT(), "LOCAL_YEAR", fLocal_year, 
     + "CL_MONTH_NAME(ISEAS)", CL_MONTH_NAME(ISEAS), "CL_TRANS_NAME",
     + CL_TRANS_NAME, "I", FLOAT(I), "MW(2,I)", MW(2,I), "CAPBLK", 
     + CAPBLK, "TRANS_EQUIV_AVAIL", TRANS_EQUIV_AVAIL, 
     + "TRANS_CAP_FACTOR", TRANS_CAP_FACTOR, "ENRG/1000", 
     + ENRG/1000., "HEAT", SNGL(HEAT), "AVERAGE_HEATRATE", 
     + AVERAGE_HEATRATE, "FUEL_COST/1000000.",
     + SNGL(FUEL_COST)/1000000.,"VariableOMRate*ENRG/1000000.", 
     + VariableOMRate*ENRG/1000000., "AVERAGE_PRODUCTION_COSTS", 
     + AVERAGE_PRODUCTION_COSTS, "UNIT_EMISSIONS(EM)", 
     + (UNIT_EMISSIONS(EM),EM=1,NUMBER_OF_EMISSION_TYPES), 
     + "CL_UNIT_MONTHLY_FIXED_COST(UNITNO)/1000000.", 
     + CL_UNIT_MONTHLY_FIXED_COST(UNITNO)/1000000., "TOTAL_UNIT_COST", 
     + TOTAL_UNIT_COST, "MON_ECO_SALES_REV_FROM(UNITNO)/1000000.",
     + MON_ECO_SALES_REV_FROM(UNITNO)/1000000., 
     + "MON_ECO_SALES_ENRG_FROM(UNITNO)/1000.",
     + MON_ECO_SALES_ENRG_FROM(UNITNO)/1000.,"AVE_REV_FROM_SALES", 
     + AVE_REV_FROM_SALES, "MON_ECO_SALES_REV_FROM(UNITNO).eq",
     + MON_ECO_SALES_REV_FROM(UNITNO)/1000000. - TOTAL_UNIT_COST, 
     + "MON_ECO_PUCH_COST_FROM(UNITNO)/1000000.",
     + MON_ECO_PUCH_COST_FROM(UNITNO)/1000000.,
     + "MON_ECO_PUCH_ENRG_FROM(UNITNO)/1000.",
     + MON_ECO_PUCH_ENRG_FROM(UNITNO)/1000.,"P_FUEL_CONSUMPTION", 
     + P_FUEL_CONSUMPTION, "S_FUEL_CONSUMPTION", 
     + S_FUEL_CONSUMPTION, "E_FUEL_CONSUMPTION", E_FUEL_CONSUMPTION, 
     + "AVERAGE_NOX_RATE", AVERAGE_NOX_RATE, "AVERAGE_SOX_RATE", 
     + AVERAGE_SOX_RATE, "AVERAGE_FUEL_COST", AVERAGE_FUEL_COST,
     + "VariableOMRate", VariableOMRate, "(UNIT_EMISSIONS_COST(EM)", 
     + (UNIT_EMISSIONS_COST(EM),EM=1,NUMBER_OF_EMISSION_TYPES), 
     + "WHOLESALE_PRODUCTION_COST", WHOLESALE_PRODUCTION_COST, 
     + "RETAIL_SALES", RETAIL_SALES, "RTG", RTG, "RMK", RMK, 
     + "RFT", RFT, "HESI_ID_NUM_4_UNIT", HESI_ID_NUM_4_UNIT,
     + "POWERDAT_PLANT_ID", POWERDAT_PLANT_ID, "MONTH_UNIT_STARTS", 
     + FLOAT(MONTH_UNIT_STARTS(UNITNO)), "TOTAL_EMISSION_COSTS", 
     + TOTAL_EMISSION_COSTS, "MONTH_UNIT_START_COSTS",
     + MONTH_UNIT_START_COSTS(UNITNO)/1000000., "CLASS",
     + FLOAT(CLASS), "PBTUCT_SAVE", PBTUCT_SAVE, "P_FUEL_DELIVERY", 
     + P_FUEL_DELIVERY, "UNECONOMIC_RATE", UNECONOMIC_RATE, 
     + "NEWGEN_UNIT_STATUS", NEWGEN_UNIT_STATUS, "ONLINE_DATE", 
     + ONLINE_DATE, "OFLINE_DATE", OFLINE_DATE, "RPM", RPM, 
     + "HESI_SECOND_UNIT_ID_NUM", HESI_SECOND_UNIT_ID_NUM, 
     + "EV_DATA_SOURCE", EV_DATA_SOURCE, "EV_PLANNING_AREA", 
     + EV_PLANNING_AREA, "CAP_MARKET_REVENUE", CAP_MARKET_REVENUE,
     + "CAPITAL_COST_OF_BUILD", CAPITAL_COST_OF_BUILD, "UNIT_EBITDA", 
     + UNIT_EBITDA, "RSP", RSP, "CHARGING_ENERGY", CHARGING_ENERGY/1000.
       end if
      
       ! End of trace write
       
                  MON_CL_TRANS_UNIT_REC = MON_CL_TRANS_UNIT_REC + 1
                 endif ! THERMAL UNITS
! TODO: EXTRACT_METHOD handle_monthly_pw_report
                 IF( MONTHLY_PW_REPORT .OR.
     +                     ( MONTHLY_NEW_PW_REPORT .AND.
     +                          UNITNO > CL_UNITS_B4_ADDITIONS ) ) THEN
!
                     UNIT_START_UP_COSTS =
     +     GET_UNIT_START_UP_COSTS(globecom_year,UNITNO,ISEAS)
!
                     TEMP_L1 =
     +                GET_PW_UNIT_TEXT_FIELDS(
     +                     UNITNO,
     +                     EIA_PLANT_CODE)
                     TEMP_L1 = GET_PORT_MARKET_AREA_ABBREV(
     +                                        RDI_MARKET_AREA_NAME,RMK)
                     TEMP_L1 = GET_BASECASE_MARKET_AREA_ID(
     +                                  EV_TRANS_AREA_NAME,TG_POSITION)
                     TEMP_L1 = GET_CUBIC_HEAT_CURVE(UNITNO,
     +                                             A_CO,B_CO,C_CO,D_CO)
!
!         !TMS 03/23/05 ADDED FOR EXELON
          MIN_UPTIME = INT(GET_MIN_UP_TIME(UNITNO))
                     MIN_DOWNTIME = INT(GET_MIN_DOWN_TIME(UNITNO))
!
                     IF(MONTHLY_NEW_PW_REPORT .AND.
     +                             UNITNO > CL_UNITS_B4_ADDITIONS) THEN

!                       ! TMS 3/01/05 INCREASED SIZE TO 8:16
                        SAC_UNIT_NAME =
     +                 TRIM(CL_TRANS_NAME(8:16))//'.'//
     +                           TRIM(CL_TRANS_NAME(1:2))//
     +                              TRIM(CL_TRANS_NAME(4:7))
                  SAC_STATION_GROUP =
     +                        'NEW.'//
     +                        TRIM(CL_TRANS_NAME(9:10))
                     ELSE
                        SAC_UNIT_NAME = CL_TRANS_NAME
                        SAC_STATION_GROUP = 'EXISTING.UNIT'
                     endif
!
! 07/28/03. ADDED FOR RICHARDSON.
!
                     LOCAL_MW1 = MW(1,I)
                     LOCAL_MW2 = MW(2,I)
                     CALL CheckO3P(A_CO,B_CO,C_CO,D_CO,
     +                                                 LOCAL_MW1,
     +                                                 LOCAL_MW2)
!
! TEST FOR MONOTONIC NON-DECREASING.
!
                     IF(MW(2,I) > 0.) THEN
                        FIXED_COST_PER_UNIT =
     +                     .001*CL_UNIT_MONTHLY_FIXED_COST(UNITNO)/
     +                                                          MW(2,I)
                     ELSE
                        FIXED_COST_PER_UNIT = 0.
                     ENDIF
!
                     IF(MW(2,UNITNO) > 0.) THEN
                        MONTHLY_MOR = MAX(0.,MIN(1.,
     +                                       (1.-CAPBLK/MW(2,UNITNO))))
                     ELSE
                        MONTHLY_MOR = 0.
                     ENDIF
!
                     IF(MONTHLY_MOR  < 0.999) THEN
                        MONTHLY_FOR = MAX(0.,MIN(1.,
     +                            (1.-(.01*TRANS_EQUIV_AVAIL)/
     +                                            (1.-MONTHLY_MOR)) ) )
                     ELSE
                        MONTHLY_FOR = 0.
                     ENDIF

                     IF(HEAT <= 10.D0) THEN
                        CALL GET_DISP_COST_FOR_A_UNIT(

     +                                   UNITNO,DISP_COST_1,DISP_COST_2)
                        PW_AVE_FUEL_COST = MAX(DISP_COST_1,DISP_COST_2)*
     +                                                .1 * AVE_FUEL_MULT


                     ELSE
                        PW_AVE_FUEL_COST =
     +                                  AVERAGE_FUEL_COST*AVE_FUEL_MULT
                     ENDIF
!
                     SAC_ONLINE_MO = ONLINE_DATE
     +                   - AINT(ONLINE_DATE/100.)*100
!
                     IF(ISEAS == 1 .AND. UNITNO >
     +                                   LAST_RESOURCE_UPDATE_NO) THEN
                      INSERVICE_STATUS = 0
                      SAC_ONLINE_DATE = '01/01/2004'
                      PW_REC = ' '

                      WRITE(PW_REC,4321)
     +                PRT_ENDPOINT(),                      ',', ! F4.0
     +              fLocal_year,                            ',', ! F4.0
     +                QUOTE,CL_MONTH_NAME(ISEAS),QUOTE,      ',', ! A
     +                SAC_UNIT_NAME,                         ',', ! A
     +              MULTI_AREA_NAME(TG_POSITION),          ',', ! F4.0
     +               RTG,                                   ',', ! F4.0
     + ! A 01/31/05. FLIPPED POSITION
     +               EV_TRANS_AREA_NAME,                    ',',
     +                 MW(1,I),                          ',', ! F8.1
     +                  MW(2,I),                      ',', ! F8.1
     +                  FIXED_COST_PER_UNIT,                   ',',
     +               PW_AVE_FUEL_COST               ,       ',', ! F7.3
     +              RFT,                                   ',', ! F3.0
     +              VCPMWH(UNITNO),                        ',', ! F7.3
     +             A_CO,                                  ',', ! F12.4
     +              B_CO,                                  ',', ! F10.4
     +             C_CO,                                  ',', ! F14.10
     +             D_CO,                                  ',', ! F14.12
     +                INSERVICE_STATUS,                      ',', ! I4
     +                 SAC_ONLINE_DATE,                       ',', ! A
     +               AVERAGE_NOX_RATE,                      ',', ! F6.3
     +               AVERAGE_SOX_RATE,                      ',', ! F6.3
     +              EMIS_DISPATCH_2*1000000.,              ',', ! F6.0
     +              EMIS_DISPATCH_1*1000000.,              ',', ! F6.0
     +              UNIT_START_UP_COSTS,                   ',', ! F10.2
     +               MONTHLY_MOR,                           ',', ! F6.2
     +               MONTHLY_FOR,                           ',', ! F6.2
     +                  SAC_ONLINE_DATE,                       ',', ! A
     +              TRIM(SAC_STATION_GROUP),               ',',
     +                  MIN_UPTIME,                            ',',
     +                  MIN_DOWNTIME,                          ',',
     +                  EMIS_DISPATCH_3*1000000.,              ',',
     +                  AVERAGE_CO2_RATE,                      ',',
     +                  EMIS_DISPATCH_4*1000000.,              ',',
     +                  AVERAGE_HG_RATE,                       ','
                      MON_PW_TRANS_UNIT_REC = rptrec(4444_2)
                      WRITE(4444,4445,REC=MON_PW_TRANS_UNIT_REC)
     +                                                     TRIM(PW_REC)
                      LAST_RESOURCE_UPDATE_NO =
     +                                      LAST_RESOURCE_UPDATE_NO + 1
                     ENDIF
!
                     INSERVICE_STATUS = 1
                     ! TMS 03/15/05 MODIFIED TO ALLOW USER SELECTABLE
                     ! ON-LINE MO
                     WRITE(SAC_ONLINE_DATE,4322) INT(SAC_ONLINE_MO),
     +                      '/01/',globecom_year+get_BASE_YEAR()
                  WRITE(SAC_DATE,4323) ISEAS,'/01/',
     +                        globecom_year+get_BASE_YEAR()
 4322                FORMAT(I2,A,I4)
 4323                FORMAT(I2,A,I4)
                     PW_REC = ' '
                     WRITE(PW_REC,4321)
     +                  PRT_ENDPOINT(),                      ',',
     +                  fLocal_year,                            ',',
     +                  QUOTE,CL_MONTH_NAME(ISEAS),QUOTE,      ',',
     +                  SAC_UNIT_NAME,                         ',',

     +                  MULTI_AREA_NAME(TG_POSITION),          ',',

     +                  RTG,                                   ',',
     +                  EV_TRANS_AREA_NAME,                    ',',

     +                  MW(1,I),                               ',',
     +                  MW(2,I),                               ',',
     +                  FIXED_COST_PER_UNIT*MW(2,I)*1000.,     ',',
     +                  PW_AVE_FUEL_COST,                      ',',
     +                  RFT,                                   ',',
     +                  VCPMWH(UNITNO),                        ',',
     +                  A_CO,                                  ',',
     +                  B_CO,                                  ',',
     +                  C_CO,                                  ',',
     +                  D_CO,                                  ',',
     +                  INSERVICE_STATUS,                      ',',
     +                  SAC_ONLINE_DATE,                       ',',
     +                  AVERAGE_NOX_RATE,                      ',',
     +                  AVERAGE_SOX_RATE,                      ',',
     +                  EMIS_DISPATCH_2*1000000.,              ',',
     +                  EMIS_DISPATCH_1*1000000.,              ',',
     +                  UNIT_START_UP_COSTS,                   ',',
     +                  MONTHLY_MOR,                           ',',
     +                  MONTHLY_FOR,                           ',',
     +                  SAC_DATE,                              ',',
     +                  TRIM(SAC_STATION_GROUP), ',',
     +                  MIN_UPTIME,  ',', ! I4
     +                  MIN_DOWNTIME,',', ! I4
     +              EMIS_DISPATCH_3*1000000.,              ',', ! F6.0
     +               AVERAGE_CO2_RATE,                      ',', ! F6.3
     +               EMIS_DISPATCH_4*1000000.,              ',', ! F6.0
     +               AVERAGE_HG_RATE,                       ',' ! F6.3

                     MON_PW_TRANS_UNIT_REC = rptrec(4444_2)
                     WRITE(4444,4445,REC=MON_PW_TRANS_UNIT_REC)
     +                                                      TRIM(PW_REC)

 4321                FORMAT(
     +                   1X,F5.0,   A,
     +                   1X,F5.0,   A,
     +                   1X,A,A,A,A,
     +                   1X,A,A,
     +                   1X,A,A,
     +                   1X,F6.0,   A,  ! MULTI-AREA TRANSACTION GROUP
     +                   1X,A,A,
     +                   1X,F8.1,   A, ! MIN CAPACITY
     +                   1X,F8.1,   A, ! MAX CAPACITY
     +                   1X,F12.0,   A,! FIXED COST
     +                   1X,F9.3,   A, ! AVE FUEL COST
     +                   1X,F3.0,   A, ! FUEL TYPE INDEX
     +                   1X,F7.3,   A, ! VOM
     +                   1X,F12.4, A, ! A COEFFICIENT
     +                   1X,F10.4, A, ! B COEFFICIENT
     +                   1X,F14.10,   A, ! C COEFFICIENT
     +                   1X,F14.12,   A, ! D COEFFICIENT
     +                   1X,I4,       A, ! INSERVICE_STATUS
     +                   1X,A,        A, ! SAC_ONLINE_DATE
     +                   1X,F6.3,     A, ! NOX RATE
     +                   1X,F6.3,     A, ! SOX RATE
     +                   1X,F6.0,     A, ! NOX EMISSION COST
     +                   1X,F6.0,     A, ! SOX EMISSION COST
     +                   1X,F10.2,     A, ! UNIT START UP COSTS
     +                   1X,F6.2,     A, ! MONTHLY MOR
     +                   1X,F6.2,     A, ! MONTHLY FOR
     +                   1X,A,        A, ! SAC_DATE
     +                   1X,A,        A, ! SAC_STATION_GROUP
     +                   1X,I4,       A, ! MIN_UPTIME
     +                   1X,I4        A,  ! MIN_DOWNTIME
     +                   1X,F9.2,     A, ! CO2 EMISSION COST
     +                   1X,F6.1,     A, ! CO2 RATE
     +                   1X,F9.0,     A, ! HG EMISSION COST
     +                   1X,F11.9,     A) ! HG RATE


!
                     MON_PW_TRANS_UNIT_REC = MON_PW_TRANS_UNIT_REC + 1
!
                 ENDIF

! TODO: EXTRACT_METHOD handle_monthly_pw_report (end)

! TODO: Determine whether three writes to MON_NU_TRANS_UNIT_NO
! can be combined into a single function call.
                 IF(NEW_UNITS_REPORT .AND.
     +                                ONLINE(UNITNO) > BEGIN_DATE) THEN
               MON_NU_TRANS_UNIT_REC = rptrec(MON_NU_TRANS_UNIT_NO)
                  WRITE(MON_NU_TRANS_UNIT_NO,REC=MON_NU_TRANS_UNIT_REC)
     +               PRT_ENDPOINT(),
     +               fLocal_year,
     +               CL_MONTH_NAME(ISEAS),
     +               CL_TRANS_NAME,
     +               FLOAT(I),
     +               MW(2,I),
     +               CAPBLK,
     +               TRANS_EQUIV_AVAIL,
     +               TRANS_CAP_FACTOR,
     +               ENRG/1000.,
     +               SNGL(HEAT),
     +               AVERAGE_HEATRATE,
     +               SNGL(FUEL_COST)/1000000.,
     +               VCPMWH(UNITNO)*ENRG/1000000.,
     +               AVERAGE_PRODUCTION_COSTS,
     +               (UNIT_EMISSIONS(EM),
     +                     EM=1,NUMBER_OF_EMISSION_TYPES),
     +               CL_UNIT_MONTHLY_FIXED_COST(UNITNO)/1000000.,
     +               TOTAL_UNIT_COST,
     +               MON_ECO_SALES_REV_FROM(UNITNO)/1000000.,
     +               MON_ECO_SALES_ENRG_FROM(UNITNO)/1000.,
     +               AVE_REV_FROM_SALES,
     +               MON_ECO_SALES_REV_FROM(UNITNO)/1000000. -
     +                                                 TOTAL_UNIT_COST,
     +               MON_ECO_PUCH_COST_FROM(UNITNO)/1000000.,
     +               MON_ECO_PUCH_ENRG_FROM(UNITNO)/1000.,
     +               P_FUEL_CONSUMPTION,
     +               S_FUEL_CONSUMPTION,
     +               E_FUEL_CONSUMPTION,
     +               AVERAGE_NOX_RATE,
     +               AVERAGE_SOX_RATE,
     +               AVERAGE_FUEL_COST,
     +               VCPMWH(UNITNO),
     +               (UNIT_EMISSIONS_COST(EM),
     +                     EM=1,NUMBER_OF_EMISSION_TYPES),
     +               WHOLESALE_PRODUCTION_COST,
     +               RETAIL_SALES,
     +               RTG,RMK,RFT,
     +               HESI_ID_NUM_4_UNIT,
     +               POWERDAT_PLANT_ID,
     +               FLOAT(MONTH_UNIT_STARTS(UNITNO)),
     +               TOTAL_EMISSION_COSTS,
     +               MONTH_UNIT_START_COSTS(UNITNO)/1000000.,
     +               FLOAT(CLASS),
     +               PBTUCT_SAVE,
     +               P_FUEL_DELIVERY,
     +               UNECONOMIC_RATE,
     +               NEWGEN_UNIT_STATUS,
     +               ONLINE_DATE,
     +               OFLINE_DATE,
     +               RPM,
     +               HESI_SECOND_UNIT_ID_NUM,
     +               EV_DATA_SOURCE,
     +               EV_PLANNING_AREA,
     +               CAP_MARKET_REVENUE,
     +               CAPITAL_COST_OF_BUILD,
     +               UNIT_EBITDA,
     +               RSP,
     +               CHARGING_ENERGY/1000.
                  MON_NU_TRANS_UNIT_REC = MON_NU_TRANS_UNIT_REC + 1
                 ENDIF ! NEW UNITS
               ENDIF ! EXTERNAL UNIT TRAPS
            ENDIF

         ENDDO ! UNITS

         SUM_ANNUAL = 1
!
         I = get_NUNITS()
!

         DO TRANS = 1, NUM_TRANSACTIONS
!
            VOID_LOGICAL = GET_MONTHLY_TRANS_VARIABLES(TRANS,
     +                                               ENRG,
     +                                               TRANS_CAP,
     +                                               TRANS_VAR_EXP,
     +                                               TRANS_VAR_MWH,
     +                                               TRANS_FIX_EXP,
     +                                               TRANS_REV,
     +                                               TRANS_REV_MWH,
     +                                               TRANS_HOURS,
     +                                               PRODUCT_HOURS,
     +                                               TRANS_STRIKES,
     +                                               ISEAS,
     +                                               TRANS_NAME,
     +                                               SUM_ANNUAL,
     +                                              YES_REPORT_PRODUCT,
     +                                               TG,
     +                                               PM,
     +                                               FT,
     +                                               TRANS_ON_LINE,
     +                                               TRANS_OFF_LINE,
     +                                               TRANS_UNIT_ID,
     +                                               TRANS_ASSET_CLASS,
     +                                               TRANS_NOT_ACTIVE,
     +                                               TEMP_YEAR,
     +                                        CHARGING_ENERGY) ! 060308
! 121806
            IF(TRANS_NOT_ACTIVE) CYCLE
!
            I = I + 1
            CL_TRANS_NAME = TRANS_NAME

            TRANS_STRIKES = 0.0 ! 092107. PER BURESH.
!
            ANNUAL_CAP(I) = ANNUAL_CAP(I) + TRANS_CAP
            FISCAL_CAP(I) = FISCAL_CAP(I) + TRANS_CAP
            ANNUAL_REV_GEN_CAPACITY(I) = ANNUAL_REV_GEN_CAPACITY(I) +
     +                                    TRANS_CAP
            FISCAL_REV_GEN_CAPACITY(I) = FISCAL_REV_GEN_CAPACITY(I) +
     +                                    TRANS_CAP
            ANNUAL_EFFECTIVE_CAP(I) =
     +                              ANNUAL_EFFECTIVE_CAP(I) + TRANS_CAP
            FISCAL_EFFECTIVE_CAP(I) =
     +                              FISCAL_EFFECTIVE_CAP(I) + TRANS_CAP
            FISCAL_CL_UNIT_FIXED_COST(I) =
     +                     FISCAL_CL_UNIT_FIXED_COST(I) +
     +                                    CL_UNIT_MONTHLY_FIXED_COST(I)
            MONTHS_ACTIVE(I) = MONTHS_ACTIVE(I) + 1
            FISCAL_MONTHS_ACTIVE(I) = FISCAL_MONTHS_ACTIVE(I) + 1
!
      CAP_MARKET_REVENUE = 0. ! NO CAPACITY MARKET FOR DERIVATIVES YET.


!
               UNIT_EMISSIONS = 0.
               UNIT_EMISSIONS_COST = 0.
!
!
!
               TG_POSITION = MAX(1,GET_TRANS_GROUP_POSITION(TG))
!
               RTG = TG
               RFT = FT
               RSP = FLOAT(GET_TRANS_STATE_INDEX(TRANS))
               MONTHLY_CHARGING_ENERGY = MONTHLY_CHARGING_ENERGY +
     +                                                  CHARGING_ENERGY


               RPM = PM
! todo: promote_pm
               IF(PM == 16) THEN
                  PM = 18 ! BATTERY
               ELSEIF(PM == 17) THEN
                  PM = 19 ! DG
               ENDIF
!
               IF(TRANSACT_PROD_REPORT_ACTIVE .AND.
     +                                      FT > 0 .AND. FT /= 10) THEN
!
                  FT = MAX(1,MIN(FT,6))
!
                  PROD_BY_TG_BY_MWH(TG_POSITION,PM) =
     +                  PROD_BY_TG_BY_MWH(TG_POSITION,PM) + ENRG
                  ANNUAL_PROD_BY_TG_BY_MWH(TG_POSITION,PM) =
     +                  ANNUAL_PROD_BY_TG_BY_MWH(TG_POSITION,PM) + ENRG
                  PROD_BY_TG_BY_MW(TG_POSITION,PM) =
     +                  PROD_BY_TG_BY_MW(TG_POSITION,PM) + TRANS_CAP
                  IF(PM == 10) THEN ! STEAM

                     pm=pm !debugstop

                     LM = MIN(PM + FT + 4,max_prod_types_clr)
                     PROD_BY_TG_BY_MWH(TG_POSITION,LM) =
     +                  PROD_BY_TG_BY_MWH(TG_POSITION,LM) + ENRG
                     ANNUAL_PROD_BY_TG_BY_MWH(TG_POSITION,LM) =
     +                  ANNUAL_PROD_BY_TG_BY_MWH(TG_POSITION,LM) + ENRG
                     PROD_BY_TG_BY_MW(TG_POSITION,LM) =
     +                  PROD_BY_TG_BY_MW(TG_POSITION,LM) + TRANS_CAP
                  ENDIF
!
                  PROD_BY_TG_BY_MWH(TG_POSITION,14) =
     +                  PROD_BY_TG_BY_MWH(TG_POSITION,14) + ENRG
                  ANNUAL_PROD_BY_TG_BY_MWH(TG_POSITION,14) =
     +                  ANNUAL_PROD_BY_TG_BY_MWH(TG_POSITION,14) + ENRG
                  PROD_BY_TG_BY_MW(TG_POSITION,14) =
     +                     PROD_BY_TG_BY_MW(TG_POSITION,14) + TRANS_CAP
!
                  IF( ABS(TRANS_UNIT_ID+999999.0) > 2) THEN ! PER KJ
!
                     IF(FT == 6) THEN
                        IF(PM == 2) THEN
                           TFT = 7
                        ELSEIF(PM == 3) THEN
                           TFT = 8
                        ELSEIF(PM == 7) THEN
                           TFT = 9
                        ELSEIF(PM == 9) THEN
                           TFT = 10
                        ELSEIF(PM == 11) THEN
                           TFT = 11
                        ELSEIF(PM == 12) THEN
                           TFT = 12
                        ELSEIF(INT2(RPM) == 16) THEN
                           TFT = 14 ! BATTERY
                        ELSEIF(INT2(RPM) == 17) THEN
                           TFT = 15 ! DG
                        ELSE
                           TFT = 13
                        ENDIF
                        MW_BY_TG_BY_FUEL(TG_POSITION,TFT) =
     +                    MW_BY_TG_BY_FUEL(TG_POSITION,TFT) + TRANS_CAP
                        MWH_BY_TG_BY_FUEL(TG_POSITION,TFT) =
     +                        MWH_BY_TG_BY_FUEL(TG_POSITION,TFT) + ENRG
                        ANNUAL_MWH_BY_TG_BY_FUEL(TG_POSITION,TFT) =
     +                      ANNUAL_MWH_BY_TG_BY_FUEL(TG_POSITION,TFT) +
     +                                                             ENRG
                        IF(ISEAS == 12) THEN
                           ANNUAL_MW_BY_TG_BY_FUEL(TG_POSITION,TFT) =
     +                       ANNUAL_MW_BY_TG_BY_FUEL(TG_POSITION,TFT) +
     +                                                        TRANS_CAP
                        ENDIF
                     ENDIF
!
                     MW_BY_TG_BY_FUEL(TG_POSITION,FT) =
     +                  MW_BY_TG_BY_FUEL(TG_POSITION,FT) + TRANS_CAP
                     MWH_BY_TG_BY_FUEL(TG_POSITION,FT) =
     +                  MWH_BY_TG_BY_FUEL(TG_POSITION,FT) + ENRG
                     ANNUAL_MWH_BY_TG_BY_FUEL(TG_POSITION,FT) =
     +                  ANNUAL_MWH_BY_TG_BY_FUEL(TG_POSITION,FT) + ENRG
!
                     MW_BY_TG_BY_FUEL(TG_POSITION,MFT1) =
     +                  MW_BY_TG_BY_FUEL(TG_POSITION,MFT1) + TRANS_CAP
! PER KJ.
                     IF(ISEAS == 12) THEN
                        ANNUAL_MW_BY_TG_BY_FUEL(TG_POSITION,FT) =
     +                    ANNUAL_MW_BY_TG_BY_FUEL(TG_POSITION,FT) +
     +                                                     TRANS_CAP
                        ANNUAL_MW_BY_TG_BY_FUEL(TG_POSITION,MFT1) =
     +                    ANNUAL_MW_BY_TG_BY_FUEL(TG_POSITION,MFT1) +
     +                                                     TRANS_CAP
                     ENDIF
                  ENDIF
! TODO: EXTRACT_METHOD handle_complex_inputs (end)
! (don't know what to call this)
                  MWH_BY_TG_BY_FUEL(TG_POSITION,MFT1) =
     +                  MWH_BY_TG_BY_FUEL(TG_POSITION,MFT1) + ENRG
                  ANNUAL_MWH_BY_TG_BY_FUEL(TG_POSITION,MFT1) =
     +                ANNUAL_MWH_BY_TG_BY_FUEL(TG_POSITION,MFT1) + ENRG
!
               ENDIF

               AVERAGE_NOX_RATE = 0.
               AVERAGE_SOX_RATE = 0.
               AVERAGE_FUEL_COST = 0.

            IF(.NOT. TESTING_PLAN .AND. NBLOCK > 0) THEN

                  FUEL_COST = 0.0 D0
                  HEAT = 0.0 D0
                  AVERAGE_HEATRATE = 0.0
                  AVERAGE_PRODUCTION_COSTS = 0.0

               IF( ABS(TRANS_CAP) > 0.01 .AND. PRODUCT_HOURS > 0) THEN
                  TRANS_CAP_FACTOR = ENRG * 100. /
     +                                      (TRANS_CAP * PRODUCT_HOURS)
               ELSE
                  TRANS_CAP_FACTOR = 0.0
               ENDIF
!
               TOTAL_UNIT_COST = TRANS_VAR_EXP + TRANS_FIX_EXP
!
               MONTHLY_ECO_SALES = MONTHLY_ECO_SALES + TRANS_REV
               MONTHLY_DERIV_ECO_SALES = MONTHLY_DERIV_ECO_SALES +
     +                                                        TRANS_REV
!
               MONTHLY_ECO_SALES_ENRG = MONTHLY_ECO_SALES_ENRG + ENRG
               MONTHLY_DERIV_ECO_SALES_ENRG =
     +                             MONTHLY_DERIV_ECO_SALES_ENRG + ENRG

!
               MONTHLY_TRANS_CAPACITY =
     +                               MONTHLY_TRANS_CAPACITY + TRANS_CAP
               MONTHLY_TRANS_EFFECTIVE_CAP =
     +                          MONTHLY_TRANS_EFFECTIVE_CAP + TRANS_CAP
               MONTHLY_TRANS_AVAIL_CAP = MONTHLY_TRANS_AVAIL_CAP +
     +                                                       TRANS_CAP
               MONTHLY_TRANS_ENERGY = MONTHLY_TRANS_ENERGY + ENRG

               MONTHLY_TRANS_VAR_COST = MONTHLY_TRANS_VAR_COST +
     +                                                    TRANS_VAR_EXP

               MONTHLY_TRANS_FIXED_COST = MONTHLY_TRANS_FIXED_COST +
     +                                                    TRANS_FIX_EXP


              WHOLESALE_PRODUCTION_COST = TRANS_VAR_MWH * ENRG/1000000.
               RETAIL_SALES = MAX(0.,(ENRG - ENRG)/1000.)
               MONTHLY_WHOLE_COSTS = MONTHLY_WHOLE_COSTS +
     +                                        WHOLESALE_PRODUCTION_COST
               MONTHLY_DERIV_WHOLESALE_COST =
     +                                   MONTHLY_DERIV_WHOLESALE_COST +
     +                                        WHOLESALE_PRODUCTION_COST
!
               NEWGEN_UNIT_STATUS = 0.
               CAPITAL_COST_OF_BUILD = 0.
               TOTAL_EMISSION_COSTS = SUM(UNIT_EMISSIONS_COST)
               UNIT_EBITDA = (TRANS_REV-TOTAL_UNIT_COST-
     +                                   TOTAL_EMISSION_COSTS)/1000000.
               POWERDAT_PLANT_ID = GET_Holding_Company_ID(TRANS)
! 112820. NEW MARKET_ID

               TEMP_L1 = GET_DERIV_ZONE_ID(I,MARKET_ID)
               MK = MARKET_AREA_LOOKUP(MARKET_ID(1:5))
               IF(MK < 0 .OR. MK > 600) THEN
                 MK =MAX(0,MIN(600,MARKET_AREA_LOOKUP(MARKET_ID(1:5))))
               ENDIF
               RMK = FLOAT(MK)
               IF (TRANS_ON_LINE - 190000. .LE. DATE2 .AND.
     +                        TRANS_OFF_LINE - 190000. .GE. DATE1) THEN
                  TransAllHoursMonth(TRANS) =
     +                   TransAllHoursMonth(TRANS) + SEAS_HOURS
                  TransCapHours(TRANS) =
     +                   TransCapHours(TRANS) + SEAS_HOURS * TRANS_CAP
                  TransEffectiveCapHours(TRANS) =
     +                   TransEffectiveCapHours(TRANS) +
     +                                           SEAS_HOURS * TRANS_CAP
                  TransAvailHoursMonth(TRANS) =
     +                   TransAvailHoursMonth(TRANS) + PRODUCT_HOURS
               ENDIF
               TRANS_EQUIV_AVAIL = 100.* PRODUCT_HOURS / SEAS_HOURS

!
               IF( MONTHLY_TRANS_REPORT .AND.
     +                    YES_REPORT_PRODUCT .AND.
     +                                 .NOT. SUPPRESS_DERIVATIVES) THEN

               MON_CL_TRANS_UNIT_REC = rptrec(MON_CL_TRANS_UNIT_NO)
        ! This is the monthly transact report, derivatives are active.
        !_Product_ is being reported.
        ! msgmtrcl Monthly derivatives/renewables
                   call write_unique_msgmtrcl_log_entry(2)
               WRITE(MON_CL_TRANS_UNIT_NO,REC=MON_CL_TRANS_UNIT_REC)
     +               PRT_ENDPOINT(),
     +               fLocal_year,
     +               CL_MONTH_NAME(ISEAS),
     +               CL_TRANS_NAME,
     +               FLOAT(I),
     +               TRANS_CAP, ! Installed capacity
     +               TRANS_CAP, ! Equivalent capacity
     +               TRANS_EQUIV_AVAIL, ! TRANS_HOURS,
     +               TRANS_CAP_FACTOR,
     +               ENRG/1000.,
     +               0.,
     +               TRANS_STRIKES,
     +               0.,
     +          TRANS_VAR_EXP/1000000., ! 9 - Variable O&M Cost ($/MWH)
     +               TRANS_VAR_MWH,
     +               (UNIT_EMISSIONS(EM),
     +                     EM=1,NUMBER_OF_EMISSION_TYPES), ! 10-15
     +               TRANS_FIX_EXP/1000000., ! 16
     +               TOTAL_UNIT_COST/1000000.,
     +               TRANS_REV/1000000.,
     +               ENRG/1000.,
     +               TRANS_REV_MWH,
     +               (TRANS_REV-TOTAL_UNIT_COST)/1000000.,
     +               0.,
     +               0.,
     +               0.,
     +               0.,
     +               0.,
     +               0.,
     +               0.,
     +               0.,
     +               TRANS_VAR_MWH,
     +               (UNIT_EMISSIONS_COST(EM),
     +                EM=1,NUMBER_OF_EMISSION_TYPES),
     +               WHOLESALE_PRODUCTION_COST,
     +               RETAIL_SALES,
     +               RTG,RMK,RFT,
     +               TRANS_UNIT_ID, ! HESI UNIT ID NUM
     +               POWERDAT_PLANT_ID,  ! POWER PLANT ID NUM
     +               0.,
     +               TOTAL_EMISSION_COSTS,
     +               0.,
     +               TRANS_ASSET_CLASS, ! ASSET CLASS
     +               0.,
     +               0.,
     +               0., ! UNECONOMIC_RATE
     +               NEWGEN_UNIT_STATUS,
     +               TRANS_ON_LINE, ! ONLINE
     +               TRANS_OFF_LINE, ! OFLINE
     +               RPM,
     +               0., ! HESI_SECOND_UNIT_ID
     +               EV_DATA_SOURCE,
     +               EV_PLANNING_AREA,
     +               CAP_MARKET_REVENUE,
     +               CAPITAL_COST_OF_BUILD,
     +               UNIT_EBITDA,
     +               RSP,
     +               CHARGING_ENERGY/1000.
     
       ! Begin tracing
         call write_trace_message(file_trace_pclr, "2. msgmctrl write.")
         if(file_trace_pclr/=BAD_TRACE_HANDLE) then
               WRITE(file_trace_pclr,*)
     + "PRT_ENDPOINT", PRT_ENDPOINT(), "LOCAL_YEAR", fLocal_year,
     + "CL_MONTH_NAME", CL_MONTH_NAME(ISEAS), "CL_TRANS_NAME", 
     + CL_TRANS_NAME, "I", FLOAT(I), "TRANS_CAP", TRANS_CAP, 
     + "TRANS_CAP", TRANS_CAP, "TRANS_EQUIV_AVAIL", TRANS_EQUIV_AVAIL, 
     + "TRANS_CAP_FACTOR", TRANS_CAP_FACTOR, "ENRG", ENRG/1000.,
     + 0., "TRANS_STRIKES", TRANS_STRIKES, 0., "TRANS_VAR_EXP", 
     + TRANS_VAR_EXP/1000000., "TRANS_VAR_EXP", TRANS_VAR_MWH,
     + "UNIT_EMISSIONS", (UNIT_EMISSIONS(EM),
     + EM=1,NUMBER_OF_EMISSION_TYPES), "TRANS_FIX_EXP", 
     + TRANS_FIX_EXP/1000000., "TOTAL_UNIT_COST", 
     + TOTAL_UNIT_COST/1000000., "TRANS_REV", TRANS_REV/1000000.,
     + "ENRG", ENRG/1000., "TRANS_REV_MWH", TRANS_REV_MWH,
     + "TR-TUC", (TRANS_REV-TOTAL_UNIT_COST)/1000000., 0., 0., 0., 0.,
     + 0., 0., 0., 0.,"TRANS_VAR_MWH", TRANS_VAR_MWH, "UEC for EM", 
     + (UNIT_EMISSIONS_COST(EM), EM=1,NUMBER_OF_EMISSION_TYPES),
     + "WHOLESALE_PRODUCTION_COST", WHOLESALE_PRODUCTION_COST,
     + "RETAIL_SALES", RETAIL_SALES, "RTG", RTG, "RMK", RMK, "RFT", RFT,
     + "TRANS_UNIT_ID", TRANS_UNIT_ID, "POWERDAT_PLANT_ID", 
     + POWERDAT_PLANT_ID, 0., "TOTAL_EMISSION_COSTS", 
     + TOTAL_EMISSION_COSTS, 0., "TRANS_ASSET_CLASS", 
     + TRANS_ASSET_CLASS, 0., 0., 0., "NEWGEN_UNIT_STATUS", 
     + NEWGEN_UNIT_STATUS, "TRANS_ON_LINE", TRANS_ON_LINE,
     +"TRANS_OFF_LINE", TRANS_OFF_LINE, "RPM", RPM,0., "EV_DATA_SOURCE",
     +EV_DATA_SOURCE, "EV_PLANNING_AREA", EV_PLANNING_AREA,
     +"CAP_MARKET_REVENUE", CAP_MARKET_REVENUE, "CAPITAL_COST_OF_BUILD",
     + CAPITAL_COST_OF_BUILD, "UNIT_EBITDA", UNIT_EBITDA, "RSP", RSP,
     + "CHARGING_ENERGY", CHARGING_ENERGY/1000.
       end if
       ! End tracing
       
              MON_CL_TRANS_UNIT_REC = MON_CL_TRANS_UNIT_REC + 1
               ENDIF ! THERMAL UNITS REPORT
!
               IF(DERIVATIVES_REPORT) THEN
               MON_DV_TRANS_UNIT_REC = rptrec(MON_DV_TRANS_UNIT_NO)
                  WRITE(MON_DV_TRANS_UNIT_NO,REC=MON_DV_TRANS_UNIT_REC)
     +               PRT_ENDPOINT(),
     +               fLocal_year,
     +               CL_MONTH_NAME(ISEAS),
     +               CL_TRANS_NAME,
     +               FLOAT(I),
     +               TRANS_CAP,
     +               PRODUCT_HOURS,
     +               TRANS_HOURS,
     +               TRANS_CAP_FACTOR,
     +               ENRG/1000.,
     +               0.,
     +               TRANS_STRIKES,
     +               0.,
     +               TRANS_VAR_EXP/1000000.,
     +               TRANS_VAR_MWH,
     +               (UNIT_EMISSIONS(EM),
     +                     EM=1,NUMBER_OF_EMISSION_TYPES),
     +               TRANS_FIX_EXP/1000000.,
     +               TOTAL_UNIT_COST/1000000.,
     +               TRANS_REV/1000000.,
     +               ENRG/1000.,
     +               TRANS_REV_MWH,
     +               (TRANS_REV-TOTAL_UNIT_COST)/1000000.,
     +               0.,
     +               0.,
     +               0.,
     +               0.,
     +               0.,
     +               0.,
     +               0.,
     +               0.,
     +               TRANS_VAR_MWH,
     +               (UNIT_EMISSIONS_COST(EM),
     +                     EM=1,NUMBER_OF_EMISSION_TYPES),
     +               WHOLESALE_PRODUCTION_COST,
     +               RETAIL_SALES,
     +               RTG,RMK,RFT,
     +               TRANS_UNIT_ID, ! HESI UNIT ID NUM
     +               POWERDAT_PLANT_ID, ! POWER PLANT ID NUM
     +               0.,
     +               TOTAL_EMISSION_COSTS,
     +               0.,
     +               TRANS_ASSET_CLASS, ! ASSET CLASS
     +               0.,
     +               0.,
     +               0., ! UNECONOMIC_RATE
     +               NEWGEN_UNIT_STATUS,
     +               TRANS_ON_LINE, ! ONLINE
     +               TRANS_OFF_LINE, ! OFLINE
     +               RPM,
     +               0., ! HESI_SECOND_UNIT_ID
     +               EV_DATA_SOURCE,
     +               EV_PLANNING_AREA,
     +               CAP_MARKET_REVENUE,
     +               CAPITAL_COST_OF_BUILD,
     +               UNIT_EBITDA,
     +               RSP,
     +               CHARGING_ENERGY/1000.
                    MON_DV_TRANS_UNIT_REC = MON_DV_TRANS_UNIT_REC + 1
               ENDIF ! DERIVATIVES REPORT
            ENDIF
         ENDDO ! TRANSACTIONS
!
         CL_TRANS_NAME = 'Total Resources       '
         I = I + 1
!
         IF(MONTHLY_TRANS_ENERGY > 0.45) THEN
            AVERAGE_HEATRATE = HEAT_CONVERSION*MONTHLY_TRANS_HEAT/
     +                                             MONTHLY_TRANS_ENERGY
            AVERAGE_PRODUCTION_COSTS =
     +               (SNGL(MONTHLY_TRANS_FUEL_COST) +
     +                              MONTHLY_TRANS_VAR_COST) /
     +                                             MONTHLY_TRANS_ENERGY
            IF(.NOT. CANADA) AVERAGE_HEATRATE = AVERAGE_HEATRATE + .5
         ELSE
            MONTHLY_TRANS_FUEL_COST = 0.0 D0
            MONTHLY_TRANS_HEAT = 0.0 D0
            AVERAGE_HEATRATE = 0.0
            AVERAGE_PRODUCTION_COSTS = 0.0
         ENDIF
         IF(MONTHLY_TRANS_CAPACITY > 0.01 .AND. SEAS_HOURS > 0) THEN
            TRANS_EQUIV_AVAIL =  100. * MONTHLY_TRANS_AVAIL_CAP/
     +                                           MONTHLY_TRANS_CAPACITY
            TRANS_CAP_FACTOR = MONTHLY_TRANS_ENERGY * 100. /
     +                            (MONTHLY_TRANS_CAPACITY * SEAS_HOURS)
         ELSE
            TRANS_EQUIV_AVAIL = 0.0
            TRANS_CAP_FACTOR = 0.0
         ENDIF
! TODO: EXTRACT_METHOD apply_trans_energy (end)
         TOTAL_UNIT_COST =
     +               (SNGL(MONTHLY_TRANS_FUEL_COST) +
     +                         MONTHLY_TRANS_VAR_COST +
     +                               MONTHLY_TRANS_FIXED_COST)/1000000.
!
         IF(MONTHLY_ECO_SALES_ENRG > 0.1) THEN
            AVE_REV_FROM_SALES =  MONTHLY_ECO_SALES /
     +                                           MONTHLY_ECO_SALES_ENRG
         ELSE
            AVE_REV_FROM_SALES =  0.
         ENDIF

!
         IF(MONTHLY_TRANS_HEAT /= 0.) THEN
            AVERAGE_SOX_RATE=TONS_CONVERSION*MONTHLY_UNIT_EMISSIONS(1)/
     +                              MONTHLY_TRANS_HEAT
            AVERAGE_FUEL_COST = MONTHLY_TRANS_FUEL_COST/
     +                           MONTHLY_TRANS_HEAT
         ELSE
            AVERAGE_SOX_RATE = 0.
            AVERAGE_FUEL_COST = 0.
         ENDIF
         AVERAGE_VAR_OM = 0.
         IF(MONTHLY_TRANS_ENERGY /= 0.) AVERAGE_VAR_OM =
     +                                          MONTHLY_TRANS_VAR_COST/
     +                                            MONTHLY_TRANS_ENERGY
         AVERAGE_NOX_RATE = 0.
         IF(TOTAL_NOX_HEAT > 0.) THEN
            AVERAGE_NOX_RATE = TONS_CONVERSION *
     +                         MONTHLY_UNIT_EMISSIONS(2)/TOTAL_NOX_HEAT
         ENDIF
! TODO: EXTRACT_METHOD handle_for_monthly_trans_heat (end)
         MONTHLY_RETAIL_SALES = MAX(0.,
     +           (MONTHLY_TRANS_ENERGY - MONTHLY_ECO_SALES_ENRG)/1000.)
!
!
         WHOLESALE_MARKET_COST(ISEAS) = MONTHLY_ECO_PUCH/1000.
         WHOLESALE_MARKET_BUY_MWH(ISEAS) = MONTHLY_ECO_PUCH_ENRG ! MWH
        MONTHLY_WHOLESALE_COST(ISEAS,1) =   MONTHLY_WHOLE_COSTS * 1000.

         MONTHLY_WHOLESALE_COST(ISEAS,2) =
     +                          (MONTHLY_WHOLESALE_FUEL_COSTS)*1000.
        MONTHLY_WHOLESALE_COST(ISEAS,3) = MONTHLY_WHOLESALE_VOM_COSTS *
     +                                                            1000.

         WHOLESALE_MARKET_REV(ISEAS) =
     +                           (MONTHLY_ECO_SALES -
     +                                   MONTHLY_DERIV_ECO_SALES)/1000.
!
         WHOLESALE_DERIV_COST(ISEAS) = WHOLESALE_DERIV_COST(ISEAS) +
     +                                    MONTHLY_DERIV_ECO_SALES/1000.
!
         WHOLESALE_MARKET_SELL_MWH(ISEAS) =
     +                     MONTHLY_ECO_SALES_ENRG -
     +                                MONTHLY_DERIV_ECO_SALES_ENRG! MWH
!
         WHOLESALE_MARKET_COST(0) =
     +                     WHOLESALE_MARKET_COST(0) +
     +                                     WHOLESALE_MARKET_COST(ISEAS)
         WHOLESALE_MARKET_BUY_MWH(0) =
     +                     WHOLESALE_MARKET_BUY_MWH(0) +
     +                                  WHOLESALE_MARKET_BUY_MWH(ISEAS)
         MONTHLY_WHOLESALE_COST(0,1) =  MONTHLY_WHOLESALE_COST(0,1) +
     +                                  MONTHLY_WHOLESALE_COST(ISEAS,1)
         MONTHLY_WHOLESALE_COST(0,2) =  MONTHLY_WHOLESALE_COST(0,2) +
     +                                  MONTHLY_WHOLESALE_COST(ISEAS,2)
         MONTHLY_WHOLESALE_COST(0,3) =  MONTHLY_WHOLESALE_COST(0,3) +
     +                                  MONTHLY_WHOLESALE_COST(ISEAS,3)
         WHOLESALE_MARKET_REV(0) = WHOLESALE_MARKET_REV(0) +
     +                                 WHOLESALE_MARKET_REV(ISEAS)
!
         WHOLESALE_DERIV_COST(0) = WHOLESALE_DERIV_COST(0) +
     +                                      WHOLESALE_DERIV_COST(ISEAS)
!
         WHOLESALE_MARKET_SELL_MWH(0) =
     +                                 WHOLESALE_MARKET_SELL_MWH(0) +
     +                                 WHOLESALE_MARKET_SELL_MWH(ISEAS)
!
! TODO: EXTRACT_METHOD handle_market_data (end)
         NEWGEN_UNIT_STATUS = 0.
!
! TODO: EXTRACT_METHOD handle_monthly_trans_report
         IF(MONTHLY_TRANS_REPORT) THEN

            TOTAL_EMISSION_COSTS = SUM(MONTHLY_UNIT_EMISSIONS_COST)
            TOTAL_EMISSION_COSTS_BY_TYPE(:) =
     +                      SUM(MONTHLY_UNIT_EMISSIONS_COST(:,:),dim=2)

!
            IF(TRANS_EQUIV_AVAIL > 0.) THEN
               UNECONOMIC_RATE = 100.*(1. -
     +                            (TRANS_CAP_FACTOR/TRANS_EQUIV_AVAIL))
            ELSE
               UNECONOMIC_RATE = 0.
            ENDIF

            MON_CL_TRANS_UNIT_REC = RPTREC(MON_CL_TRANS_UNIT_NO)
            ! Monthly Transact report.
            !msgmtrcl

            call write_unique_msgmtrcl_log_entry(3)
            WRITE(MON_CL_TRANS_UNIT_NO,REC=MON_CL_TRANS_UNIT_REC)
     +               PRT_ENDPOINT(),
     +               fLocal_year,
     +               CL_MONTH_NAME(ISEAS),
     +               CL_TRANS_NAME,
     +               FLOAT(I),
     +               MONTHLY_TRANS_CAPACITY,
     +               MONTHLY_TRANS_EFFECTIVE_CAP,
     +               TRANS_EQUIV_AVAIL,
     +               TRANS_CAP_FACTOR,
     +               MONTHLY_TRANS_ENERGY/1000.,
     +               SNGL(MONTHLY_TRANS_HEAT),
     +               AVERAGE_HEATRATE,
     +               SNGL(MONTHLY_TRANS_FUEL_COST)/1000000.,
     + ! 9 - Variable O&M Cost ($/MMW)
     +               MONTHLY_TRANS_VAR_COST/1000000.,
     +               AVERAGE_PRODUCTION_COSTS,
     +               (MONTHLY_UNIT_EMISSIONS(EM),
     +                     EM=1,NUMBER_OF_EMISSION_TYPES),
     + ! 16 - Fixed Cost ($/MMW)
     +               MONTHLY_TRANS_FIXED_COST/1000000.,
     +               TOTAL_UNIT_COST,
     +               MONTHLY_ECO_SALES/1000000.,
     +               MONTHLY_ECO_SALES_ENRG/1000.,
     +               AVE_REV_FROM_SALES,
     +               MONTHLY_ECO_SALES/1000000. - TOTAL_UNIT_COST,
     +               MONTHLY_ECO_PUCH/1000000.,
     +               MONTHLY_ECO_PUCH_ENRG/1000.,
     +               MONTHLY_P_HEAT,
     +               MONTHLY_S_HEAT,
     +               MONTHLY_E_HEAT,
     +               AVERAGE_NOX_RATE,
     +               AVERAGE_SOX_RATE,
     +               AVERAGE_FUEL_COST,
     +               AVERAGE_VAR_OM,
     +               (TOTAL_EMISSION_COSTS_BY_TYPE(EM),
     +                                  EM=1,NUMBER_OF_EMISSION_TYPES),
     +               MONTHLY_WHOLE_COSTS,
     +               MONTHLY_RETAIL_SALES,
     +               999.,999.,999.,99999.,99999.,
     +               FLOAT(MONTHLY_STARTS),
     +               TOTAL_EMISSION_COSTS,
     +               MONTHLY_START_COSTS/1000000.,
     +               999., ! ASSET CLASS
     +               0.,
     +               0.,
     +               UNECONOMIC_RATE,
     +               NEWGEN_UNIT_STATUS,
     +               0.,
     +               0.,
     +               0.,
     +               0.,
     +               EV_DATA_SOURCE,
     +               EV_PLANNING_AREA,
     +               MONTHLY_CAPACITY_REVENUE,
     +               MONTHLY_CAPITAL_COST_OF_BUILD,
     +               MONTHLY_EBITDA,
     +               999.,
     +               MONTHLY_CHARGING_ENERGY/1000.
            MON_CL_TRANS_UNIT_REC = MON_CL_TRANS_UNIT_REC + 1
         endif
! TODO: EXTRACT_METHOD handle_monthly_trans_report (end)

! TODO: EXTRACT_METHOD handle_transact_fuel_report
         IF(TRANSACT_FUEL_REPORT_ACTIVE) THEN
            DO I = 1, UPPER_TRANS_GROUP
               FUEL_COST_PER_MMBTU = 0.
               DO J = 1, max_fuel_types_clr
                  IF(FUEL_BTUS_BY_TG_BY_FUEL(I,J) < .0001) CYCLE
                  FUEL_COST_PER_MMBTU(J) =
     +                  FUEL_COST_BY_TG_BY_FUEL(I,J)*1000000. /
     +                              FUEL_BTUS_BY_TG_BY_FUEL(I,J)
               ENDDO

               if(TRANS_FUEL_NO==0) then
                    call end_program("clreport:0002 - " //
     + "TRANS_FUEL_NO not set.")
               end if

               TRANS_FUEL_REC = rptrec(TRANS_FUEL_NO)
               WRITE(TRANS_FUEL_NO,REC=TRANS_FUEL_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(globecom_year+get_BASE_YEAR()),
     +               CL_MONTH_NAME(ISEAS),
     +               MULTI_AREA_NAME(I),
     +         (FUEL_COST_BY_TG_BY_FUEL(I,J),J=1,max_fuel_types_clr),
     +         (FUEL_BTUS_BY_TG_BY_FUEL(I,J),J=1,max_fuel_types_clr),
     +               (FUEL_COST_PER_MMBTU(J),J=1,max_fuel_types_clr),
     +               EV_DATA_SOURCE
               TRANS_FUEL_REC = TRANS_FUEL_REC + 1
            ENDDO
            DO I = 1, MAX_STATE_PROVINCE_NO
               FUEL_COST_PER_MMBTU = 0.
               RESOURCE_RPS_VARS = 0.0
               DO J = 1, max_fuel_types_clr
                  IF(FUEL_BTUS_BY_SP_BY_FUEL(I,J) < .0001) CYCLE
                  FUEL_COST_PER_MMBTU(J) =
     +                  FUEL_COST_BY_SP_BY_FUEL(I,J)*1000000. /
     +                              FUEL_BTUS_BY_SP_BY_FUEL(I,J)
               ENDDO
! 1=LB/MMBTU; 2=$MM; 3=$/TON
               DO J = 1, 5
                  IF(GROUP_VAR_BY_SP(I,4) > 0.1) THEN
                     UNIT_EMISSIONS(J) =
     +                  2000.* GROUP_VAR_BY_SP(I,7+J)/
     +                                             GROUP_VAR_BY_SP(I,4)
                  ELSE
                     UNIT_EMISSIONS(J) = 0.
                  ENDIF
                  IF(GROUP_VAR_BY_SP(I,7+J) > 0.1) THEN
                     EMISSION_COST_PER_TON(J) =
     +                     1000000. * EMISSIONS_COST_BY_SP(I,J) /
     +                                GROUP_VAR_BY_SP(I,7+J)
                  ELSE
                     EMISSION_COST_PER_TON(J) = 0.0
                  ENDIF
                  ANNUAL_EMISSIONS_COST_BY_SP(I,J) =
     +                     ANNUAL_EMISSIONS_COST_BY_SP(I,J) +
     +                                   EMISSIONS_COST_BY_SP(I,J)
               ENDDO
               IF(GROUP_VAR_BY_SP(I,3) > 0.1) THEN
                  GROUP_VAR_BY_SP(I,4) = GROUP_VAR_BY_SP(I,4)/
     +                                             GROUP_VAR_BY_SP(I,3)
               ELSE
                  GROUP_VAR_BY_SP(I,4) = 0.
               ENDIF
               TEMP_I2 = GET_STATE_PROVINCE_NAMES(STATE_PROVINCE,I)
               ST_TG = STATE_ID_LOOKUP(STATE_PROVINCE)
               CALL GET_STATE_RPS_DATA(ISEAS,ST_TG,STATE_RPS_VARS)

               TEMP_L1 = GET_TRANS_RPS_SUM(ISEAS,ST_TG,
     +                                               RESOURCE_RPS_VARS)
               CALL GET_HYDRO_RPS_SUM(ISEAS,ST_TG,
     +                                               RESOURCE_RPS_VARS)
               CALL GET_THERMAL_RPS_SUM(ISEAS,ST_TG,RESOURCE_RPS_VARS)
               TEMP_NAME = STATE_PROVINCE
               TRANS_STATE_REC = rptrec(TRANS_STATE_NO)
               WRITE(TRANS_STATE_NO,REC=TRANS_STATE_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(globecom_year+get_BASE_YEAR()),
     +               CL_MONTH_NAME(ISEAS),
     +               TEMP_NAME,
     +         (FUEL_COST_BY_SP_BY_FUEL(I,J),J=1,max_fuel_types_clr),
     +         (FUEL_BTUS_BY_SP_BY_FUEL(I,J),J=1,max_fuel_types_clr),
     +               (FUEL_COST_PER_MMBTU(J),J=1,max_fuel_types_clr),
     +               EV_DATA_SOURCE,
     +               (GROUP_VAR_BY_SP(I,J),J=1,MAX_GROUP_VAR),
     +               UNIT_EMISSIONS,
     +               (EMISSIONS_COST_BY_SP(I,J),J=1,5),
     +               EMISSION_COST_PER_TON,
     +               STATE_RPS_VARS,
     +               RESOURCE_RPS_VARS

               TRANS_STATE_REC = TRANS_STATE_REC + 1
            ENDDO
         ENDIF
!
         IF(TRANSACT_PROD_REPORT_ACTIVE) THEN
            DO I = 1, UPPER_TRANS_GROUP

               ANNUAL_PROD_BY_TG_BY_MW(I,18) =
     +                MAX(ANNUAL_PROD_BY_TG_BY_MW(I,18),
     +                                 PROD_BY_TG_BY_MW(I,18))
               ANNUAL_PROD_BY_TG_BY_MW(I,19) =
     +                MAX(ANNUAL_PROD_BY_TG_BY_MW(I,19),
     +                                 PROD_BY_TG_BY_MW(I,19))
               DO PM = 1, 13
                  ANNUAL_PROD_BY_TG_BY_MW(I,PM) =
     +                MAX(ANNUAL_PROD_BY_TG_BY_MW(I,PM),
     +                                 PROD_BY_TG_BY_MW(I,PM))
                  IF(PM == 10) THEN ! STEAM
                     FT=FT !debugstop

                     DO FT = 1, 3
                        LM = MIN(PM + FT + 4,max_prod_types_clr)
                        if(lm==max_prod_types_clr) THEN
                            lm=lm ! debugstop
                        endif
                        ANNUAL_PROD_BY_TG_BY_MW(I,LM) =
     +                      MAX(ANNUAL_PROD_BY_TG_BY_MW(I,LM),
     +                                 PROD_BY_TG_BY_MW(I,LM))
                     END DO
                  ENDIF
! TODO: EXTRACT_METHOD handle_transact_fuel_report (end)

               ENDDO
!
!
! 031005. HYDRO SECTION. ADDED FOR DOUG
!
               WH_MONTH_ENERGY = GET_WH_MONTH_ENERGY(ISEAS,I)
!
               CALL GET_ONE_TRANS_ROR_CAP(I,ROR_CAPACITY)
!
               T_I = 1
               T_J = 6
!
               SYSTEM_PROD_BY_TG_BY_MWH_ord =
     +                          GET_SYSTEM_PROD_BY_TG_BY_MWH(T_I,I,T_J)

               TEMP_TL_MWH = ROR_CAPACITY*SEAS_HOURS +
     +                (GET_MONTHLY_TL_HYDRO_MWH(I) + WH_MONTH_ENERGY) -
     +                                     SYSTEM_PROD_BY_TG_BY_MWH_ord

               TEMP_TL_HYDRO_MW = GET_MONTHLY_TL_HYDRO_MW(I) +
     +                          GET_WH_MONTH_CAPACITY(ISEAS,I) +
     +                                                ROR_CAPACITY
!
               PROD_BY_TG_BY_MWH(I,5) =
     +                        PROD_BY_TG_BY_MWH(I,5) +
     +                                                TEMP_TL_MWH
               ANNUAL_PROD_BY_TG_BY_MWH(I,5) =
     +                  ANNUAL_PROD_BY_TG_BY_MWH(I,5) +
     +                                                TEMP_TL_MWH
!
               PROD_BY_TG_BY_MWH(I,18) =
     +                        PROD_BY_TG_BY_MWH(I,18) +
     +                                  SYSTEM_PROD_BY_TG_BY_MWH_ord
               ANNUAL_PROD_BY_TG_BY_MWH(I,18) =
     +                  ANNUAL_PROD_BY_TG_BY_MWH(I,18) +
     +                                      SYSTEM_PROD_BY_TG_BY_MWH_ord
               PROD_BY_TG_BY_MW(I,18) =
     +                        PROD_BY_TG_BY_MW(I,18) +
     +                       SYSTEM_PROD_BY_TG_BY_MWH_ord/SEAS_HOURS
               ANNUAL_PROD_BY_TG_BY_MW(I,18) =
     +                  ANNUAL_PROD_BY_TG_BY_MW(I,18) +
     +                          SYSTEM_PROD_BY_TG_BY_MWH_ord/SEAS_HOURS
! TODO: EXTRACT_METHOD handle_fiscal_reporting_for_season
               PROD_BY_TG_BY_MW(I,5) =
     +                  PROD_BY_TG_BY_MW(I,5) + TEMP_TL_HYDRO_MW
               ANNUAL_PROD_BY_TG_BY_MW(I,5) = ! 080608
     +                MAX(ANNUAL_PROD_BY_TG_BY_MW(I,5),
     +                                           PROD_BY_TG_BY_MW(I,5))

               MW_BY_TG_BY_FUEL(I,5) =
     +                  MW_BY_TG_BY_FUEL(I,5) + TEMP_TL_HYDRO_MW
               ANNUAL_MW_BY_TG_BY_FUEL(I,5) =
     +               MAX(ANNUAL_MW_BY_TG_BY_FUEL(I,5),TEMP_TL_HYDRO_MW)
               MWH_BY_TG_BY_FUEL(I,5) =
     +                  MWH_BY_TG_BY_FUEL(I,5) + TEMP_TL_MWH
               ANNUAL_MWH_BY_TG_BY_FUEL(I,5) =
     +            ANNUAL_MWH_BY_TG_BY_FUEL(I,5) + TEMP_TL_MWH
!
               MW_BY_TG_BY_FUEL(I,MFT1) =
     +                  MW_BY_TG_BY_FUEL(I,MFT1) + TEMP_TL_HYDRO_MW
               IF(ISEAS == 12) THEN
                  ANNUAL_MW_BY_TG_BY_FUEL(I,MFT1) =
     +                   ANNUAL_MW_BY_TG_BY_FUEL(I,MFT1) +
     +                                  ANNUAL_MW_BY_TG_BY_FUEL(I,5)
               ENDIF
               MWH_BY_TG_BY_FUEL(I,MFT1) =
     +                  MWH_BY_TG_BY_FUEL(I,MFT1) + TEMP_TL_MWH
               ANNUAL_MWH_BY_TG_BY_FUEL(I,MFT1) =
     +            ANNUAL_MWH_BY_TG_BY_FUEL(I,MFT1) + TEMP_TL_MWH
!
!
               PROD_BY_TG_BY_MWH(I,14) =
     +                  PROD_BY_TG_BY_MWH(I,14) +
     +                       SYSTEM_PROD_BY_TG_BY_MWH_ord + TEMP_TL_MWH
               ANNUAL_PROD_BY_TG_BY_MWH(I,14) =
     +                  ANNUAL_PROD_BY_TG_BY_MWH(I,14) +
     +                        SYSTEM_PROD_BY_TG_BY_MWH_ord + TEMP_TL_MWH
               PROD_BY_TG_BY_MW(I,14) =
     +                  PROD_BY_TG_BY_MW(I,14) +
     +                                                 TEMP_TL_HYDRO_MW
               ANNUAL_PROD_BY_TG_BY_MW(I,14) =
     +                MAX(ANNUAL_PROD_BY_TG_BY_MW(I,14),
     +                                 PROD_BY_TG_BY_MW(I,14))
!

               TG = GET_TRANS_GROUP_INDEX(I)

               TEMP_L1 = GET_TG_MONTH_SUM_B4_HYDRO(TG,
     +                                             TG_MWH,
     +                                             TG_PEAK,
     +                                             TG_BASE)
               LOAD_BY_TG_BY_MWH(I,1) = MAX(0.,TG_BASE)
               LOAD_BY_TG_BY_MWH(I,3) = MAX(0.,TG_PEAK)
               LOAD_BY_TG_BY_MWH(I,4) = MAX(0.,TG_MWH)
               IF(SEAS_HOURS > 0.0) THEN
                  LOAD_BY_TG_BY_MWH(I,2) =
     +                                LOAD_BY_TG_BY_MWH(I,4)/SEAS_HOURS
               ENDIF
               ANNUAL_LOAD_BY_TG_BY_MWH(I,1) =
     +               MIN(ANNUAL_LOAD_BY_TG_BY_MWH(I,1),
     +                          LOAD_BY_TG_BY_MWH(I,1))
               ANNUAL_LOAD_BY_TG_BY_MWH(I,3) =
     +               MAX(ANNUAL_LOAD_BY_TG_BY_MWH(I,3),
     +                          LOAD_BY_TG_BY_MWH(I,3))
               ANNUAL_LOAD_BY_TG_BY_MWH(I,4) =
     +               ANNUAL_LOAD_BY_TG_BY_MWH(I,4) +
     +                      LOAD_BY_TG_BY_MWH(I,4)
!
               TG_CapacitySupplyObligation(I,ISEAS) =
     +                              + PROD_BY_TG_BY_MW(I,3)
     +                              + PROD_BY_TG_BY_MW(I,5)
     +                              + PROD_BY_TG_BY_MW(I,6)
     +                              + PROD_BY_TG_BY_MW(I,8)
     +                              + PROD_BY_TG_BY_MW(I,12)
     +                              + PROD_BY_TG_BY_MW(I,15)
     +                              + PROD_BY_TG_BY_MW(I,16)


               TG_ResourceActualPreference =
     +                            TG_EffectiveCapacity(I,ISEAS)
     +                           - TG_CapacitySupplyObligation(I,ISEAS)

               UZVars(ISEAS,I,1:18) = PROD_BY_TG_BY_MW(I,1:18)
               UZVars(ISEAS,I,19) = TG_EffectiveCapacity(I,ISEAS)
              UZVars(ISEAS,I,20) = TG_CapacitySupplyObligation(I,ISEAS)
               UZVars(ISEAS,I,21) = TG_ResourceActualPreference
               UZVars(ISEAS,I,22) = PROD_BY_TG_BY_MWH(I,14)
               CAPACITY_AREA_NAME(I) = MULTI_AREA_NAME(I)

               TRANS_PROD_REC = RPTREC(TRANS_PROD_NO)

       call check_capacity_array_bounds("prod_by_tg_by_mw",
     +   prod_by_tg_by_mw, int(I), int(max_prod_types_clr))
       call check_capacity_array_bounds("prod_by_tg_by_mwh",
     +   prod_by_tg_by_mwh, int(I), int(max_prod_types_clr))

       
               ! Transact Production report (msgmopro)
               WRITE(TRANS_PROD_NO,REC=TRANS_PROD_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(globecom_year+get_BASE_YEAR()),
     +               CL_MONTH_NAME(ISEAS),
     +               MULTI_AREA_NAME(I),
     +          (PROD_BY_TG_BY_MW(I,J),J=1,max_prod_types_for_report),
     +        (PROD_BY_TG_BY_MWH(I,J),J=1,max_prod_types_for_report), 
     +               2.0,
     +              (LOAD_BY_TG_BY_MWH(I,J),J=1,MAX_LOAD_TYPES), !37-40
     +               TG_EffectiveCapacity(I,ISEAS),   ! 41
     +               TG_CapacitySupplyObligation(I,ISEAS),  !42
     +               TG_ResourceActualPreference      ! 43

            
            ! start msgmopro trace 1
            call write_trace_message(file_trace_pclr, "1. msgmopro")
            call write_trace_int(file_trace_pclr, "TRANS_PROD_REC", 
     +          TRANS_PROD_REC)
     

        
            if(file_trace_pclr/=BAD_TRACE_HANDLE) then
                write(file_trace_pclr,*)
     + "PRT_ENDPOINT", PRT_ENDPOINT(),
     + "YEAR",        FLOAT(globecom_year+get_BASE_YEAR()),
     + "MONTH_NAME",   CL_MONTH_NAME(ISEAS),
     + "MULTI_AREA_NAME",     MULTI_AREA_NAME(I), "PROD_BY_TG_BY_MW",
     + (PROD_BY_TG_BY_MW(I,J),J=1,max_prod_types_for_report), 
     + "PROD_BY_TG_BY_MWH",
     + (PROD_BY_TG_BY_MWH(I,J),J=1,max_prod_types_for_report), !19-37
     + 2.0,
     + "LOAD_BY_TG_BY_MWH", (LOAD_BY_TG_BY_MWH(I,J),J=1,MAX_LOAD_TYPES),
     + "TG_EffectiveCapacity",  TG_EffectiveCapacity(I,ISEAS),  
     + "TG_CapacitySupplyObligation",
     + TG_CapacitySupplyObligation(I,ISEAS),  !42
     + "TG_ResourceActualPreference", TG_ResourceActualPreference
            end if
            
            ! End trace
            
       TRANS_PROD_REC = TRANS_PROD_REC + 1
       call check_capacity_array_bounds("mw_by_tg_by_fuel",
     +   mw_by_tg_by_fuel, int(I), int(MFT1))
       call check_capacity_array_bounds("mwh_by_tg_by_fuel",
     +   mwh_by_tg_by_fuel, int(I), int(MFT1))

               TRAN_PROD_FUEL_REC = RPTREC(TRAN_PROD_FUEL_NO)

               WRITE(TRAN_PROD_FUEL_NO,REC=TRAN_PROD_FUEL_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(globecom_year+get_BASE_YEAR()),
     +               CL_MONTH_NAME(ISEAS),
     +               MULTI_AREA_NAME(I),
     +               (MW_BY_TG_BY_FUEL(I,J),J=1,MFT1),
     +               (MWH_BY_TG_BY_FUEL(I,J),J=1,MFT1)
               TRAN_PROD_FUEL_REC = TRAN_PROD_FUEL_REC + 1
            ENDDO
         ENDIF
!
!
! FISCAL TRANS REPORT
!
!
         IF(YES_FISCAL_REPORTING .AND. ISEAS == Fscl_Ssn_loc) THEN
!
            MONTHLY_STARTS = 0
            MONTHLY_START_COSTS = 0.
            MONTHLY_CAPITAL_COST_OF_BUILD = 0.
            MONTHLY_EBITDA = 0.
            MONTHLY_CHARGING_ENERGY = 0.
            MONTHLY_CAPACITY_REVENUE = 0.
!
            MONTHLY_NOX_SEASON_THERM = 0.
            MONTHLY_TRANS_CAPACITY = 0.0
            MONTHLY_TRANS_EFFECTIVE_CAP = 0.0
            MONTHLY_TRANS_AVAIL_CAP = 0.0
            MONTHLY_TRANS_ENERGY = 0.0
!
            MONTHLY_P_HEAT = 0.0 ! D0
            MONTHLY_S_HEAT = 0.0 ! D0
            MONTHLY_E_HEAT = 0.0 ! D0
!
            MONTHLY_TRANS_HEAT = 0.0 D0
            MONTHLY_TRANS_FUEL_COST = 0.0 D0
            MONTHLY_TRANS_VAR_COST = 0.0
!
            MONTHLY_UNIT_EMISSIONS(:) = 0.0
!
!
!
            MONTHLY_TRANS_FIXED_COST = 0.
!
            ANNUAL_ECO_SALES = 0.
            ANNUAL_ECO_SALES_ENRG = 0.
            ANNUAL_ECO_PUCH = 0.
            ANNUAL_ECO_PUCH_ENRG = 0.
!
            ANNUAL_WHOLESALE_COST = 0.
!
!
! ONLY CALLED ONCE FOR RETIREMENT LOGIC.
!
!
!
            DO I = 1, get_NUNITS()
               UNITNO = I
               call write_trace_int2(file_trace_pclr, "2. UNIT", UNITNO)
               cur_called_from="pcr:2"
               CL_TRANS_NAME = UNITNM(I)//CL_UNIQUE_RPT_STR(I) ! '  '
               ENRG = FISCAL_CL_UNIT_ENERGY(UNITNO)
        call write_trace_real(file_trace_pclr, "4. ENRG", ENRG)
! 1/12/00.
               FISCAL_REV_GEN_CAPACITY(I) =
     +                           FISCAL_REV_GEN_CAPACITY(I) /
     +                                                      LAST_SEASON
               CAPBLK = FISCAL_EFFECTIVE_CAP(I) / LAST_SEASON
               IF(FISCAL_MONTHS_ACTIVE(I) == LAST_SEASON) THEN
                  FISCAL_CAP(I) = FISCAL_CAP(I) / LAST_SEASON
               ELSEIF(FISCAL_MONTHS_ACTIVE(I) > 0) THEN
                  FISCAL_CAP(I) = FISCAL_CAP(I) /FISCAL_MONTHS_ACTIVE(I)
               ENDIF
! 8/28/02
               CLASS = GET_ASSET_CLASS_NUM(UNITNO)
               P_FUEL_DELIVERY = ABS(GET_P_FUEL_DELIVERY(UNITNO))
               PBTUCT_SAVE = ABS(GET_PBTUCT_SAVE(UNITNO))
!
               IF(ENRG > 0.45) THEN
                  HEAT = FISCAL_CL_UNIT_MMBTUS(UNITNO)
                  FUEL_COST = FISCAL_CL_UNIT_FUEL_COST(UNITNO)
!
! EMISSIONS


    ! TODO: integration
    ! TODO: Suggest iterating or using constants, depending on which
    ! is appropriate. UNIT_EMISSIONS, UNIT_EMISSIONS_COST
                  UNIT_EMISSIONS(1) = FISCAL_CL_UNIT_EMISSIONS(1,UNITNO)
                  UNIT_EMISSIONS(2) = FISCAL_CL_UNIT_EMISSIONS(2,UNITNO)
                  UNIT_EMISSIONS(3) = FISCAL_CL_UNIT_EMISSIONS(3,UNITNO)
                  UNIT_EMISSIONS(4) = FISCAL_CL_UNIT_EMISSIONS(4,UNITNO)
                  UNIT_EMISSIONS(5) = FISCAL_CL_UNIT_EMISSIONS(5,UNITNO)


!
                  UNIT_EMISSIONS_COST(1) =
     +                             FISCAL_UNIT_EMISSIONS_COST(UNITNO,1)
                  UNIT_EMISSIONS_COST(2) =
     +                             FISCAL_UNIT_EMISSIONS_COST(UNITNO,2)
                  UNIT_EMISSIONS_COST(3) =
     +                             FISCAL_UNIT_EMISSIONS_COST(UNITNO,3)
                  UNIT_EMISSIONS_COST(4) =
     +                             FISCAL_UNIT_EMISSIONS_COST(UNITNO,4)
                  UNIT_EMISSIONS_COST(5) =
     +                             FISCAL_UNIT_EMISSIONS_COST(UNITNO,5)
!
               ELSE
                  HEAT = 0.D0
                  FUEL_COST = 0.
!
                  UNIT_EMISSIONS(1) = 0.
                  UNIT_EMISSIONS(2) = 0.
                  UNIT_EMISSIONS(3) = 0.
                  UNIT_EMISSIONS(4) = 0.
                  UNIT_EMISSIONS(5) = 0.
!
                  UNIT_EMISSIONS_COST(1) = 0.
                  UNIT_EMISSIONS_COST(2) = 0.
                  UNIT_EMISSIONS_COST(3) = 0.
                  UNIT_EMISSIONS_COST(4) = 0.
                  UNIT_EMISSIONS_COST(5) = 0.
!
               ENDIF
               IF(.NOT. TESTING_PLAN) THEN
                  IF(.NOT. CANADA .AND. PHASE_I_UNIT(UNITNO) ) THEN
                     UNIT_NAME = trim(UNITNM(UNITNO))//'*'
                  ELSE
                     UNIT_NAME = UNITNM(UNITNO)
                  ENDIF
                  IF(ENRG > 0.45) THEN
                     AVERAGE_HEATRATE = HEAT_CONVERSION*HEAT/ENRG
                     AVERAGE_PRODUCTION_COSTS =
     +                        (SNGL((FUEL_COST)) +
     +                             FISCAL_CL_UNIT_VAR_COST(UNITNO))/ENRG
                     IF(.NOT. CANADA) AVERAGE_HEATRATE =
     +                                             AVERAGE_HEATRATE + .5
                  ELSE
                     FUEL_COST = 0.0 D0
                     HEAT = 0.0 D0
                     AVERAGE_HEATRATE = 0.0
                     AVERAGE_PRODUCTION_COSTS = 0.0
                  ENDIF
                  IF(FISCAL_CAP(I) > 0.01) THEN
                     TRANS_EQUIV_AVAIL =  100. *
     +                        FISCAL_REV_GEN_CAPACITY(I) / FISCAL_CAP(I)
                     TRANS_CAP_FACTOR = ENRG * 100. /
     +                                           (FISCAL_CAP(I) * 8760.)
                  ELSE
                     TRANS_EQUIV_AVAIL = 0.0
                     TRANS_CAP_FACTOR = 0.0
                  ENDIF
!
                  TOTAL_UNIT_COST =
     +               (SNGL(FUEL_COST) +
     +                   FISCAL_CL_UNIT_VAR_COST(UNITNO) +
     +                       FISCAL_CL_UNIT_FIXED_COST(UNITNO))/1000000.
!
                  IF(FISCAL_ECO_SALES_ENRG_FROM(UNITNO) > 0.1) THEN
                     AVE_REV_FROM_SALES =
     +                        FISCAL_ECO_SALES_REV_FROM(UNITNO) /
     +                                FISCAL_ECO_SALES_ENRG_FROM(UNITNO)
                  ELSE
                     AVE_REV_FROM_SALES =  0.
                  ENDIF
!
                  MONTHLY_NOX_SEASON_THERM = MONTHLY_NOX_SEASON_THERM
     +                                    + FISCAL_NOX_SEASON_THERMAL(I)
                  ANNUAL_ECO_SALES = ANNUAL_ECO_SALES +
     +                                 FISCAL_ECO_SALES_REV_FROM(UNITNO)
                  ANNUAL_ECO_SALES_ENRG = ANNUAL_ECO_SALES_ENRG +
     +                                FISCAL_ECO_SALES_ENRG_FROM(UNITNO)
                  ANNUAL_ECO_PUCH = ANNUAL_ECO_PUCH +
     +                                 FISCAL_ECO_PUCH_COST_FROM(UNITNO)
                  ANNUAL_ECO_PUCH_ENRG = ANNUAL_ECO_PUCH_ENRG +
     +                                 FISCAL_ECO_PUCH_ENRG_FROM(UNITNO)
!
                  MONTHLY_TRANS_CAPACITY =
     +                            MONTHLY_TRANS_CAPACITY + FISCAL_CAP(I)
                  MONTHLY_TRANS_EFFECTIVE_CAP =
     +                              MONTHLY_TRANS_EFFECTIVE_CAP + CAPBLK
                  MONTHLY_TRANS_AVAIL_CAP = MONTHLY_TRANS_AVAIL_CAP +
     +                                        FISCAL_REV_GEN_CAPACITY(I)
                  MONTHLY_TRANS_ENERGY = MONTHLY_TRANS_ENERGY + ENRG
                  MONTHLY_TRANS_HEAT = MONTHLY_TRANS_HEAT + HEAT
                  MONTHLY_P_HEAT = MONTHLY_P_HEAT +
     +                                 FISCAL_P_FUEL_CONSUMPTION(UNITNO)
                  MONTHLY_S_HEAT = MONTHLY_S_HEAT +
     +                                 FISCAL_S_FUEL_CONSUMPTION(UNITNO)
                  MONTHLY_E_HEAT = MONTHLY_E_HEAT +
     +                                 FISCAL_E_FUEL_CONSUMPTION(UNITNO)
                  MONTHLY_TRANS_FUEL_COST = MONTHLY_TRANS_FUEL_COST +
     +                                                         FUEL_COST
! 6/13/01. To account for varying VCPMWH.
                  MONTHLY_TRANS_VAR_COST = MONTHLY_TRANS_VAR_COST +
     +                                   FISCAL_CL_UNIT_VAR_COST(UNITNO)

                  MONTHLY_UNIT_EMISSIONS(1) =
     +                           MONTHLY_UNIT_EMISSIONS(1) +
     +                                                 UNIT_EMISSIONS(1)
                  MONTHLY_UNIT_EMISSIONS(2) =
     +                           MONTHLY_UNIT_EMISSIONS(2) +
     +                                                 UNIT_EMISSIONS(2)
                  MONTHLY_UNIT_EMISSIONS(3) =
     +                           MONTHLY_UNIT_EMISSIONS(3) +
     +                                                 UNIT_EMISSIONS(3)
                  MONTHLY_UNIT_EMISSIONS(4) =
     +                           MONTHLY_UNIT_EMISSIONS(4) +
     +                                                 UNIT_EMISSIONS(4)
                  MONTHLY_UNIT_EMISSIONS(5) =
     +                           MONTHLY_UNIT_EMISSIONS(5) +
     +                                                 UNIT_EMISSIONS(5)
                  MONTHLY_TRANS_FIXED_COST = MONTHLY_TRANS_FIXED_COST +
     +                                 FISCAL_CL_UNIT_FIXED_COST(UNITNO)
!
                  MONTHLY_UNIT_EMISSIONS_COST(1,I) =
     +               MONTHLY_UNIT_EMISSIONS_COST(1,I) +
     +                                            UNIT_EMISSIONS_COST(1)
                  MONTHLY_UNIT_EMISSIONS_COST(2,I) =
     +               MONTHLY_UNIT_EMISSIONS_COST(2,I) +
     +                                            UNIT_EMISSIONS_COST(2)
                  MONTHLY_UNIT_EMISSIONS_COST(3,I) =
     +               MONTHLY_UNIT_EMISSIONS_COST(3,I) +
     +                                            UNIT_EMISSIONS_COST(3)
                  MONTHLY_UNIT_EMISSIONS_COST(4,I) =
     +               MONTHLY_UNIT_EMISSIONS_COST(4,I) +
     +                                            UNIT_EMISSIONS_COST(4)
                  MONTHLY_UNIT_EMISSIONS_COST(5,I) =
     +               MONTHLY_UNIT_EMISSIONS_COST(5,I) +
     +                                            UNIT_EMISSIONS_COST(5)
!
!
                  FT = MAX(1,MIN(GET_PRIMARY_MOVER(I),6))
                  SP = MIN(MAX(0,GET_UNIT_STATE_PROVINCE_INDEX(I)),
     +                                            MAX_STATE_PROVINCE_NO)
                  TG = get_TRANSACTION_GROUP(UNITNO)

                  TEMP_L1 = GET_CL_BASECASE_MARKET_ID(I,MARKET_ID)
                  MK = MARKET_AREA_LOOKUP(MARKET_ID(1:5))
                  GET_PRIMARY_MOVER_INDEX_called_from="pclr:0002"
                  PM = GET_PRIMARY_MOVER_INDEX(I)
                  RTG = TG
                  RMK = MK
                  RFT = FT
                  RSP = FLOAT(GET_THERMAL_STATE_INDEX(I))
                  CHARGING_ENERGY = 0.0

                  RPM = PM
!
                  HESI_ID_NUM_4_UNIT = GET_HESI_UNIT_ID_NUM(I,
     +                                    HESI_SECOND_UNIT_ID_NUM)
                  POWERDAT_PLANT_ID = GET_POWERDAT_PLANT_ID(I)
!
                  NEWGEN_UNIT_STATUS = FLOAT(GET_NEWGEN_INDEX(I))
!

                  IF(HEAT /= 0.) THEN
                     AVERAGE_SOX_RATE=TONS_CONVERSION*UNIT_EMISSIONS(1)/
     +                                             HEAT
                     AVERAGE_FUEL_COST = FUEL_COST/HEAT
                  ELSE
                     AVERAGE_SOX_RATE = 0.
                     AVERAGE_FUEL_COST = 0.
                  ENDIF
                  AVERAGE_VAR_OM = 0.
                  IF(ENRG /= 0.) AVERAGE_VAR_OM =
     +                              FISCAL_CL_UNIT_VAR_COST(UNITNO)/ENRG
                  AVERAGE_NOX_RATE = 0.
                  IF(FISCAL_NOX_SEASON_THERMAL(I) > 0.) THEN
                     AVERAGE_NOX_RATE=TONS_CONVERSION*UNIT_EMISSIONS(2)/
     +                                    FISCAL_NOX_SEASON_THERMAL(I)
                  ENDIF
!
                  UNIT_GROSS_MARGIN =
     +                  FISCAL_ECO_SALES_REV_FROM(UNITNO)/1000000. -
     +                                                   TOTAL_UNIT_COST

                  RETAIL_SALES = MAX(0.,
     +                (ENRG - FISCAL_ECO_SALES_ENRG_FROM(UNITNO))/1000.)
                  ANNUAL_WHOLESALE_COST = ANNUAL_WHOLESALE_COST +
     +                                     FISCAL_WHOLESALE_PROD_COST(I)

                  IF(TG <= MAX_TRANS_GROUP_NUMBER .AND.
     +                  TRANS_GROUP_REPORTING_ACTIVE(TG) .AND.
     +                        REPORT_MARKET_AREA(MARKET_ID) .AND.
     +                              REPORT_THIS_CL_UNIT(UNITNO) .AND.
     +                                 (MONTHLY_TRANS_REPORT .OR.
     +                                   ANNUAL_TRANS_REPORT)) THEN

                    TOTAL_EMISSION_COSTS = SUM(UNIT_EMISSIONS_COST)
!
                     IF(TRANS_EQUIV_AVAIL > 0.) THEN
                     UNECONOMIC_RATE = 100.*(1. -
     +                             (TRANS_CAP_FACTOR/TRANS_EQUIV_AVAIL))
                     ELSE
                        UNECONOMIC_RATE = 0.
                     ENDIF

                   MON_CL_TRANS_UNIT_REC = RPTREC(MON_CL_TRANS_UNIT_NO)
                   !msgmtrcl


                   call write_unique_msgmtrcl_log_entry(4)
                   WRITE(MON_CL_TRANS_UNIT_NO,REC=MON_CL_TRANS_UNIT_REC)
     +                  PRT_ENDPOINT(),
     +                  fLocal_year,
     +                  CL_MONTH_NAME(14),
     +                  CL_TRANS_NAME,
     +                  FLOAT(I),
     +                  FISCAL_CAP(I),
     +                  CAPBLK,
     +                  TRANS_EQUIV_AVAIL,
     +                  TRANS_CAP_FACTOR,
     +                  ENRG/1000.,
     +                  SNGL(HEAT),
     +                  AVERAGE_HEATRATE,
     +                  SNGL(FUEL_COST)/1000000.,
     +                  FISCAL_CL_UNIT_VAR_COST(UNITNO)/1000000., ! 9
     +                  AVERAGE_PRODUCTION_COSTS,
     +                  (UNIT_EMISSIONS(EM),
     +                     EM=1,NUMBER_OF_EMISSION_TYPES),
     +                  FISCAL_CL_UNIT_FIXED_COST(UNITNO)/1000000.,! 16
     +                  TOTAL_UNIT_COST,
     +                  FISCAL_ECO_SALES_REV_FROM(UNITNO)/1000000.,
     +                  FISCAL_ECO_SALES_ENRG_FROM(UNITNO)/1000.,
     +                  AVE_REV_FROM_SALES,
     +                  UNIT_GROSS_MARGIN,
     +                  FISCAL_ECO_PUCH_COST_FROM(UNITNO)/1000000.,
     +                  FISCAL_ECO_PUCH_ENRG_FROM(UNITNO)  /1000.,
     +                  FISCAL_P_FUEL_CONSUMPTION(UNITNO),
     +                  FISCAL_S_FUEL_CONSUMPTION(UNITNO),
     +                  FISCAL_E_FUEL_CONSUMPTION(UNITNO),
     +                  AVERAGE_NOX_RATE,
     +                  AVERAGE_SOX_RATE,
     +                  AVERAGE_FUEL_COST,
     +                  AVERAGE_VAR_OM,
     +                  (UNIT_EMISSIONS_COST(EM),
     +                     EM=1,NUMBER_OF_EMISSION_TYPES),
     +                  FISCAL_WHOLESALE_PROD_COST(I),
     +                  RETAIL_SALES,
     +                  RTG,RMK,RFT,
     +                  HESI_ID_NUM_4_UNIT,
     +                  POWERDAT_PLANT_ID,
     +                  FLOAT(FISCAL_UNIT_STARTS(UNITNO)),
     +                  TOTAL_EMISSION_COSTS,
     +                  FISCAL_UNIT_START_COSTS(UNITNO)/1000000.,
     +                  FLOAT(CLASS),
     +                  PBTUCT_SAVE,
     +                  P_FUEL_DELIVERY,
     +                  UNECONOMIC_RATE,
     +                  NEWGEN_UNIT_STATUS,
     +                  0.,
     +                  0.,
     +                  RPM,
     +                  HESI_SECOND_UNIT_ID_NUM,
     +                  EV_DATA_SOURCE,
     +                  EV_PLANNING_AREA,
     +                  CAP_MARKET_REVENUE,
     +                  FISCAL_CAPITAL_COST_OF_BUILD(UNITNO),
     +                  FISCAL_EBITDA(UNITNO),
     +                  RSP,
     +                  CHARGING_ENERGY/1000.
                     MON_CL_TRANS_UNIT_REC = MON_CL_TRANS_UNIT_REC + 1
                  ENDIF
               ENDIF
            ENDDO ! UNITS
!
! GET NUMBER OF TRANSACTIONS
!
!
!
!
            SUM_ANNUAL = -1
!
            DO TRANS = 1, NUM_TRANSACTIONS
               VOID_LOGICAL = GET_MONTHLY_TRANS_VARIABLES(TRANS,
     +                                               ENRG,
     +                                               TRANS_CAP,
     +                                               TRANS_VAR_EXP,
     +                                               TRANS_VAR_MWH,
     +                                               TRANS_FIX_EXP,
     +                                               TRANS_REV,
     +                                               TRANS_REV_MWH,
     +                                               TRANS_HOURS,
     +                                               PRODUCT_HOURS,
     +                                               TRANS_STRIKES,
     +                                               INT2_zero,
     +                                               TRANS_NAME,
     +                                               SUM_ANNUAL,
     +                                               YES_REPORT_PRODUCT,
     +                                               TG,
     +                                               PM,
     +                                               FT,
     +                                               TRANS_ON_LINE,
     +                                               TRANS_OFF_LINE,
     +                                               TRANS_UNIT_ID,
     +                                               TRANS_ASSET_CLASS,
     +                                               TRANS_NOT_ACTIVE,
     +                                               TEMP_YEAR,
     +                                       CHARGING_ENERGY) ! 060308
! 121806
               IF(TRANS_NOT_ACTIVE) CYCLE
               I = I + 1
!
               TRANS_STRIKES = 0.0 ! 092107. PER BURESH.
!
               TG_POSITION = MAX(1,GET_TRANS_GROUP_POSITION(TG))
               FT = MAX(1,MIN(FT,6))
!
               RTG = TG
               RFT = FT
               RSP = FLOAT(GET_TRANS_STATE_INDEX(TRANS))

               RPM = PM
!
               CL_TRANS_NAME = TRANS_NAME
               FISCAL_REV_GEN_CAPACITY(I) =
     +                           FISCAL_REV_GEN_CAPACITY(I) /
     +                                                       LAST_SEASON
               CAPBLK = FISCAL_EFFECTIVE_CAP(I) / LAST_SEASON
!
               IF(TRANS_HOURS > 0.) THEN
                  FISCAL_CAP(I) = ENRG / TRANS_HOURS
               ELSE
                  FISCAL_CAP(I) = TRANS_CAP / LAST_SEASON
               ENDIF
!
               HEAT = 0.D0
               FUEL_COST = 0.
!
               NEWGEN_UNIT_STATUS = 0.
!
               UNIT_EMISSIONS(1) = 0.
               UNIT_EMISSIONS(2) = 0.
               UNIT_EMISSIONS(3) = 0.
               UNIT_EMISSIONS(4) = 0.
               UNIT_EMISSIONS(5) = 0.
!
               UNIT_EMISSIONS_COST(1) = 0.
               UNIT_EMISSIONS_COST(2) = 0.
               UNIT_EMISSIONS_COST(3) = 0.
               UNIT_EMISSIONS_COST(4) = 0.
               UNIT_EMISSIONS_COST(5) = 0.
!
               IF(.NOT. TESTING_PLAN) THEN
                  IF(ABS(ENRG) > 0.45) THEN
                     AVERAGE_HEATRATE = 0.
                     AVERAGE_PRODUCTION_COSTS = TRANS_VAR_EXP/ENRG
                  ELSE
                     FUEL_COST = 0.0 D0
                     HEAT = 0.0 D0
                     AVERAGE_HEATRATE = 0.0
                     AVERAGE_PRODUCTION_COSTS = 0.0
                  ENDIF
                  IF(TRANS_CAP > 0.01 .AND. PRODUCT_HOURS > 0) THEN
                     TRANS_CAP_FACTOR = ENRG * 100. /
     +                                   (FISCAL_CAP(I) * PRODUCT_HOURS)
                  ELSE
                     TRANS_CAP_FACTOR = 0.0
                  ENDIF
!
                  TOTAL_UNIT_COST = TRANS_VAR_EXP + TRANS_FIX_EXP
!
                  IF(ABS(ENRG) > 0.1) THEN
                     AVE_REV_FROM_SALES =   TRANS_REV / ENRG
                  ELSE
                     AVE_REV_FROM_SALES =  0.
                  ENDIF
!
                  ANNUAL_ECO_SALES = ANNUAL_ECO_SALES +  TRANS_REV
                  ANNUAL_ECO_SALES_ENRG = ANNUAL_ECO_SALES_ENRG +
     +                                                              ENRG
!
                  MONTHLY_STARTS = MONTHLY_STARTS +
     +                                             FISCAL_UNIT_STARTS(I)
                  MONTHLY_START_COSTS = MONTHLY_START_COSTS +
     +                                        FISCAL_UNIT_START_COSTS(I)
                  MONTHLY_CAPITAL_COST_OF_BUILD =
     +                        MONTHLY_CAPITAL_COST_OF_BUILD +
     +                                   FISCAL_CAPITAL_COST_OF_BUILD(I)
                  MONTHLY_EBITDA = MONTHLY_EBITDA + FISCAL_EBITDA(I)
                  MONTHLY_CHARGING_ENERGY = MONTHLY_CHARGING_ENERGY +
     +                                                   CHARGING_ENERGY
!
                  MONTHLY_TRANS_CAPACITY =
     +                            MONTHLY_TRANS_CAPACITY + FISCAL_CAP(I)
                  MONTHLY_TRANS_EFFECTIVE_CAP =
     +                              MONTHLY_TRANS_EFFECTIVE_CAP + CAPBLK
                  MONTHLY_TRANS_AVAIL_CAP = MONTHLY_TRANS_AVAIL_CAP +
     +                                        FISCAL_REV_GEN_CAPACITY(I)
                  MONTHLY_TRANS_ENERGY = MONTHLY_TRANS_ENERGY + ENRG
                  MONTHLY_TRANS_VAR_COST = MONTHLY_TRANS_VAR_COST +
     +                                                     TRANS_VAR_EXP
!
                  MONTHLY_TRANS_FIXED_COST = MONTHLY_TRANS_FIXED_COST +
     +                                                     TRANS_FIX_EXP
!
                  AVERAGE_VAR_OM = 0.
                  IF(ENRG /= 0.) AVERAGE_VAR_OM = TRANS_VAR_EXP / ENRG
!
                  UNIT_GROSS_MARGIN =
     +                  (TRANS_REV - TOTAL_UNIT_COST)/1000000.
                  TOTAL_EMISSION_COSTS = SUM(UNIT_EMISSIONS_COST)
                  UNIT_EBITDA = (TRANS_REV-TOTAL_UNIT_COST-
     +                                    TOTAL_EMISSION_COSTS)/1000000.
!
                  WHOLESALE_PRODUCTION_COST =
     +                                     TRANS_VAR_MWH * ENRG/1000000.
                  RETAIL_SALES = MAX(0.,(ENRG - ENRG)/1000.)
                  ANNUAL_WHOLESALE_COST = ANNUAL_WHOLESALE_COST +
     +                                         WHOLESALE_PRODUCTION_COST
!
!
                  POWERDAT_PLANT_ID = GET_Holding_Company_ID(I)
!
                  TEMP_L1 = GET_DERIV_ZONE_ID(I,MARKET_ID)
                  MK = MARKET_AREA_LOOKUP(MARKET_ID(1:5))
                  IF(MK < 0 .OR. MK > 600) THEN
                     MK =MAX(0,MIN(600,
     +                              MARKET_AREA_LOOKUP(MARKET_ID(1:5))))
                  ENDIF
                  RMK = FLOAT(MK)
                   IF( (MONTHLY_TRANS_REPORT .OR.
     +                                   ANNUAL_TRANS_REPORT) .AND.
     +                                 .NOT. SUPPRESS_DERIVATIVES .AND.
     +                                          YES_REPORT_PRODUCT) THEN


                   MON_CL_TRANS_UNIT_REC = RPTREC(MON_CL_TRANS_UNIT_NO)
                   !msgmtrcl


                   call write_unique_msgmtrcl_log_entry(5)
                   WRITE(MON_CL_TRANS_UNIT_NO,REC=MON_CL_TRANS_UNIT_REC)
     +                  PRT_ENDPOINT(),
     +                  fLocal_year,
     +                  CL_MONTH_NAME(14),
     +                  CL_TRANS_NAME,
     +                  FLOAT(I),
     +                  FISCAL_CAP(I),
     +                  PRODUCT_HOURS,
     +                  TRANS_HOURS,
     +                  TRANS_CAP_FACTOR,
     +                  ENRG/1000.,
     +                  0.,
     +                  TRANS_STRIKES,
     +                  0.,
     +                  TRANS_VAR_EXP/1000000., ! 9
     +                  AVERAGE_PRODUCTION_COSTS,
     +                  (UNIT_EMISSIONS(EM),
     +                     EM=1,NUMBER_OF_EMISSION_TYPES),
     +                  TRANS_FIX_EXP/1000000., ! 16
     +                  TOTAL_UNIT_COST/1000000.,
     +                  TRANS_REV/1000000.,
     +                  ENRG/1000.,
     +                  AVE_REV_FROM_SALES,
     +                  UNIT_GROSS_MARGIN ,
     +                  0.,
     +                  0.,
     +                  0.,
     +                  0.,
     +                  0.,
     +                  0.,
     +                  0.,
     +                  0.,
     +                  AVERAGE_VAR_OM,
     +                  (UNIT_EMISSIONS_COST(EM),
     +                     EM=1,NUMBER_OF_EMISSION_TYPES),
     +                  WHOLESALE_PRODUCTION_COST,
     +                  RETAIL_SALES,
     +                  RTG,RMK,RFT,
     +                  TRANS_UNIT_ID, ! HESI UNIT ID NUM
     +                  POWERDAT_PLANT_ID, ! POWER PLANT ID NUM
     +                  0.,
     +                  TOTAL_EMISSION_COSTS,
     +                  0.,
     +                  TRANS_ASSET_CLASS, ! ASSET CLASS
     +                  0.,
     +                  0.,
     +                  0., ! UNECONOMIC_RATE
     +                  NEWGEN_UNIT_STATUS,
     +                  TRANS_ON_LINE, ! ONLINE
     +                  TRANS_OFF_LINE, ! OFLINE
     +                  RPM,
     +                  0., ! HESI_SECOND_UNIT_ID
     +                  EV_DATA_SOURCE,
     +                  EV_PLANNING_AREA,
     +                  CAP_MARKET_REVENUE,
     +                  CAPITAL_COST_OF_BUILD,
     +                  UNIT_EBITDA,
     +                  RSP,
     +                  CHARGING_ENERGY/1000.
                     MON_CL_TRANS_UNIT_REC = MON_CL_TRANS_UNIT_REC + 1
                  ENDIF ! THERMAL UNITS REPORT
!
                  IF(DERIVATIVES_REPORT) THEN
               MON_DV_TRANS_UNIT_REC = rptrec(MON_DV_TRANS_UNIT_NO)
                      WRITE(MON_DV_TRANS_UNIT_NO,
     +                                        REC=MON_DV_TRANS_UNIT_REC)
     +                  PRT_ENDPOINT(),
     +                  fLocal_year,
     +                  CL_MONTH_NAME(14),
     +                  CL_TRANS_NAME,
     +                  FLOAT(I),
     +                  FISCAL_CAP(I),
     +                  PRODUCT_HOURS,
     +                  TRANS_HOURS,
     +                  TRANS_CAP_FACTOR,
     +                  ENRG/1000.,
     +                  0.,
     +                  TRANS_STRIKES,
     +                  0.,
     +                  TRANS_VAR_EXP/1000000.,
     +                  AVERAGE_PRODUCTION_COSTS,
     +                  (UNIT_EMISSIONS(EM),
     +                     EM=1,NUMBER_OF_EMISSION_TYPES),
     +                  TRANS_FIX_EXP/1000000.,
     +                  TOTAL_UNIT_COST/1000000.,
     +                  TRANS_REV/1000000.,
     +                  ENRG/1000.,
     +                  AVE_REV_FROM_SALES,
     +                  UNIT_GROSS_MARGIN ,
     +                  0.,
     +                  0.,
     +                  0.,
     +                  0.,
     +                  0.,
     +                  0.,
     +                  0.,
     +                  0.,
     +                  AVERAGE_VAR_OM,
     +                  (UNIT_EMISSIONS_COST(EM),
     +                     EM=1,NUMBER_OF_EMISSION_TYPES),
     +                  WHOLESALE_PRODUCTION_COST,
     +                  RETAIL_SALES,
     +                  RTG,RMK,RFT,
     +                  TRANS_UNIT_ID, ! HESI UNIT ID NUM
     +                  POWERDAT_PLANT_ID,  ! POWER PLANT ID NUM
     +                  0.,
     +                  TOTAL_EMISSION_COSTS,
     +                  0.,
     +                  TRANS_ASSET_CLASS, ! ASSET CLASS
     +                  0.,
     +                  0.,
     +                  0., ! UNECONOMIC_RATE
     +                  NEWGEN_UNIT_STATUS,
     +                  TRANS_ON_LINE, ! ONLINE
     +                  TRANS_OFF_LINE, ! OFLINE
     +                  RPM,
     +                  0., ! HESI_SECOND_UNIT_ID
     +                  EV_DATA_SOURCE,
     +                  EV_PLANNING_AREA,
     +                  MONTHLY_CAPACITY_REVENUE,
     +                  CAPITAL_COST_OF_BUILD,
     +                  UNIT_EBITDA,
     +                  RSP,
     +                  CHARGING_ENERGY/1000.
                      MON_DV_TRANS_UNIT_REC = MON_DV_TRANS_UNIT_REC + 1
                  ENDIF ! DERIVATIVES REPORT
               ENDIF
            ENDDO ! DERIVATIVES
!
            CL_TRANS_NAME = 'Total Resources       '
            I = I + 1
!
            IF(MONTHLY_TRANS_ENERGY > 0.45) THEN
               AVERAGE_HEATRATE = HEAT_CONVERSION*MONTHLY_TRANS_HEAT/
     +                                              MONTHLY_TRANS_ENERGY
               AVERAGE_PRODUCTION_COSTS =
     +               (SNGL(MONTHLY_TRANS_FUEL_COST) +
     +                              MONTHLY_TRANS_VAR_COST) /
     +                                              MONTHLY_TRANS_ENERGY
               AVERAGE_VAR_OM = MONTHLY_TRANS_VAR_COST/
     +                            MONTHLY_TRANS_ENERGY
               IF(.NOT. CANADA) AVERAGE_HEATRATE = AVERAGE_HEATRATE + .5
            ELSE
               MONTHLY_TRANS_FUEL_COST = 0.0 D0
               MONTHLY_TRANS_HEAT = 0.0 D0
               AVERAGE_HEATRATE = 0.0
               AVERAGE_PRODUCTION_COSTS = 0.0
               AVERAGE_VAR_OM = 0.
            ENDIF
            IF(MONTHLY_TRANS_CAPACITY > 0.01 .AND. SEAS_HOURS > 0) THEN
               TRANS_EQUIV_AVAIL =  100. * MONTHLY_TRANS_AVAIL_CAP/
     +                                            MONTHLY_TRANS_CAPACITY
               TRANS_CAP_FACTOR = MONTHLY_TRANS_ENERGY * 100. /
     +                                  (MONTHLY_TRANS_CAPACITY * 8760.)
            ELSE
               TRANS_EQUIV_AVAIL = 0.0
               TRANS_CAP_FACTOR = 0.0
            ENDIF
!
            TOTAL_UNIT_COST =
     +               (SNGL(MONTHLY_TRANS_FUEL_COST) +
     +                       MONTHLY_TRANS_VAR_COST +
     +                                MONTHLY_TRANS_FIXED_COST)/1000000.
!
            IF(ANNUAL_ECO_SALES_ENRG > 0.1) THEN
               AVE_REV_FROM_SALES =  ANNUAL_ECO_SALES /
     +                                            ANNUAL_ECO_SALES_ENRG
            ELSE
               AVE_REV_FROM_SALES =  0.
            ENDIF
!
            NEWGEN_UNIT_STATUS = 0.
!
            AVERAGE_NOX_RATE = 0.
            AVERAGE_SOX_RATE = 0.
            AVERAGE_FUEL_COST = 0.
            IF(MONTHLY_NOX_SEASON_THERM /= 0.) THEN
               AVERAGE_NOX_RATE =
     +                        TONS_CONVERSION*MONTHLY_UNIT_EMISSIONS(2)/
     +                                 MONTHLY_NOX_SEASON_THERM
            ENDIF
!
            IF(MONTHLY_TRANS_HEAT /= 0.) THEN
               AVERAGE_SOX_RATE =
     +                        TONS_CONVERSION*MONTHLY_UNIT_EMISSIONS(1)/
     +                                  MONTHLY_TRANS_HEAT
               AVERAGE_FUEL_COST = MONTHLY_TRANS_FUEL_COST/
     +                               MONTHLY_TRANS_HEAT
            ENDIF
!
            TRANS_TOTAL_GROSS_MARGIN =
     +               ANNUAL_ECO_SALES/1000000. - TOTAL_UNIT_COST
!
            MONTHLY_RETAIL_SALES = MAX(0.,
     +            (MONTHLY_TRANS_ENERGY - ANNUAL_ECO_SALES_ENRG)/1000.)
!

            TOTAL_EMISSION_COSTS = SUM(MONTHLY_UNIT_EMISSIONS_COST)
            TOTAL_EMISSION_COSTS_BY_TYPE(:) =
     +                       SUM(MONTHLY_UNIT_EMISSIONS_COST(:,:),DIM=2)
            IF(TRANS_EQUIV_AVAIL > 0.) THEN
               UNECONOMIC_RATE = 100.*(1. -
     +                             (TRANS_CAP_FACTOR/TRANS_EQUIV_AVAIL))
            ELSE
               UNECONOMIC_RATE = 0.
            ENDIF
            IF(MONTHLY_TRANS_REPORT .OR. ANNUAL_TRANS_REPORT) THEN


                   MON_CL_TRANS_UNIT_REC = RPTREC(MON_CL_TRANS_UNIT_NO)
             !msgmtrcl - total for fiscal (includes deriv. and thermal)


                   call write_unique_msgmtrcl_log_entry(6)
                   WRITE(MON_CL_TRANS_UNIT_NO,REC=MON_CL_TRANS_UNIT_REC)
     +               PRT_ENDPOINT(),
     +               fLocal_year,
     +               CL_MONTH_NAME(14),
     +               CL_TRANS_NAME,
     +               FLOAT(I),
     +               MONTHLY_TRANS_CAPACITY,
     +               MONTHLY_TRANS_EFFECTIVE_CAP,
     +               TRANS_EQUIV_AVAIL,
     +               TRANS_CAP_FACTOR,
     +               MONTHLY_TRANS_ENERGY/1000.,
     +               SNGL(MONTHLY_TRANS_HEAT),
     +               AVERAGE_HEATRATE,
     +               SNGL(MONTHLY_TRANS_FUEL_COST)/1000000.,
     +               MONTHLY_TRANS_VAR_COST/1000000., ! 9
     +               AVERAGE_PRODUCTION_COSTS,
     +               (MONTHLY_UNIT_EMISSIONS(EM),
     +                     EM=1,NUMBER_OF_EMISSION_TYPES),
     +               MONTHLY_TRANS_FIXED_COST/1000000., ! 16
     +               TOTAL_UNIT_COST,
     +               ANNUAL_ECO_SALES/1000000.,
     +               ANNUAL_ECO_SALES_ENRG/1000.,
     +               AVE_REV_FROM_SALES,
     +               TRANS_TOTAL_GROSS_MARGIN,
     +               ANNUAL_ECO_PUCH/1000000.,
     +               ANNUAL_ECO_PUCH_ENRG/1000.,
     +               MONTHLY_P_HEAT,
     +               MONTHLY_S_HEAT,
     +               MONTHLY_E_HEAT,

     +               AVERAGE_NOX_RATE, ! 27
     +               AVERAGE_SOX_RATE,
     +               AVERAGE_FUEL_COST,
     +               AVERAGE_VAR_OM,
     +               (TOTAL_EMISSION_COSTS_BY_TYPE(EM),
     +                                   EM=1,NUMBER_OF_EMISSION_TYPES),
     +               ANNUAL_WHOLESALE_COST,
     +               MONTHLY_RETAIL_SALES,
     +               999.,999.,999.,99999.,99999.,
     +               FLOAT(MONTHLY_STARTS),
     +               TOTAL_EMISSION_COSTS/1000000.,
     +               MONTHLY_START_COSTS/1000000.,
     +               999., ! ASSET CLASS
     +               0.,
     +               0.,
     +               UNECONOMIC_RATE,
     +               NEWGEN_UNIT_STATUS,
     +               0.,
     +               0.,
     +               0.,
     +               0.,
     +               EV_DATA_SOURCE,
     +               EV_PLANNING_AREA,
     +               MONTHLY_CAPACITY_REVENUE,
     +               MONTHLY_CAPITAL_COST_OF_BUILD,
     +               MONTHLY_EBITDA,
     +               999.,
     +               MONTHLY_CHARGING_ENERGY/1000.
               MON_CL_TRANS_UNIT_REC = MON_CL_TRANS_UNIT_REC + 1
            ENDIF
!
         ENDIF ! ISEAS == LAST_SEASON
!
! ANNUAL TRANS REPORT
!
!
! TODO: EXTRACT_METHOD HANDLE_ANNUAL_TRANS_REPORT
         IF(ISEAS == LAST_SEASON) THEN
!
            IF(TRANSACT_FUEL_REPORT_ACTIVE) THEN
               DO I = 1, UPPER_TRANS_GROUP
                  FUEL_COST_PER_MMBTU = 0.
                  DO J = 1, max_fuel_types_clr
                     IF(ANNUAL_FUEL_BTUS_BY_TG_BY_FUEL(I,J) <
     +                                                      .0001) CYCLE
                     FUEL_COST_PER_MMBTU(J) =
     +                  ANNUAL_FUEL_COST_BY_TG_BY_FUEL(I,J) * 1000000. /
     +                               ANNUAL_FUEL_BTUS_BY_TG_BY_FUEL(I,J)
                  ENDDO
                  TRANS_FUEL_REC = rptrec(TRANS_FUEL_NO)
                  WRITE(TRANS_FUEL_NO,REC=TRANS_FUEL_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(globecom_year+get_BASE_YEAR()),
     +               CL_MONTH_NAME(13),
     +               MULTI_AREA_NAME(I),
     +               (ANNUAL_FUEL_COST_BY_TG_BY_FUEL(I,J),
     +                                      J=1,max_fuel_types_clr),
     +               (ANNUAL_FUEL_BTUS_BY_TG_BY_FUEL(I,J),
     +                                       J=1,max_fuel_types_clr),
     +               (FUEL_COST_PER_MMBTU(J),J=1,max_fuel_types_clr),
     +               EV_DATA_SOURCE
                  TRANS_FUEL_REC = TRANS_FUEL_REC + 1
               ENDDO
               DO I = 1, MAX_STATE_PROVINCE_NO
                  FUEL_COST_PER_MMBTU = 0.
                  RESOURCE_RPS_VARS = 0.0
                  DO J = 1, max_fuel_types_clr
                     IF(ANNUAL_FUEL_BTUS_BY_SP_BY_FUEL(I,J) < .0001)
     +                                                             CYCLE
                     FUEL_COST_PER_MMBTU(J) =
     +                     ANNUAL_FUEL_COST_BY_SP_BY_FUEL(I,J) *
     +                                                        1000000. /
     +                         ANNUAL_FUEL_BTUS_BY_SP_BY_FUEL(I,J)
                  ENDDO
                  DO J = 1, 5
                     IF(ANNUAL_GROUP_VAR_BY_SP(I,4) > 0.1) THEN
                        UNIT_EMISSIONS(J) =
     +                          2000.*ANNUAL_GROUP_VAR_BY_SP(I,7+J)/
     +                                       ANNUAL_GROUP_VAR_BY_SP(I,4)
                     ELSE
                        UNIT_EMISSIONS(J) = 0.
                     ENDIF
                     IF(GROUP_VAR_BY_SP(I,7+J) > 0.1) THEN
                        EMISSION_COST_PER_TON(J) = 1000000.0 *
     +                        ANNUAL_EMISSIONS_COST_BY_SP(I,J) /
     +                                     ANNUAL_GROUP_VAR_BY_SP(I,7+J)
                     ELSE
                        EMISSION_COST_PER_TON(J) = 0.0
                     ENDIF
                  ENDDO
!
                  IF(ANNUAL_GROUP_VAR_BY_SP(I,3) > 0.1) THEN
                     ANNUAL_GROUP_VAR_BY_SP(I,4) =
     +                        ANNUAL_GROUP_VAR_BY_SP(I,4)/
     +                                       ANNUAL_GROUP_VAR_BY_SP(I,3)
                  ELSE
                     GROUP_VAR_BY_SP(I,4) = 0.
                  ENDIF
                  ANNUAL_GROUP_VAR_BY_SP(I,1) =
     +                                   ANNUAL_GROUP_VAR_BY_SP(I,1)/12.
                  ANNUAL_GROUP_VAR_BY_SP(I,2) =
     +                                   ANNUAL_GROUP_VAR_BY_SP(I,2)/12.
!
                  TEMP_I2 = GET_STATE_PROVINCE_NAMES(STATE_PROVINCE,I)
                  TEMP_NAME = STATE_PROVINCE
                  ST_TG = STATE_ID_LOOKUP(STATE_PROVINCE)
             CALL GET_STATE_RPS_DATA(INT2_zero,ST_TG,STATE_RPS_VARS)
! RESOURCE_RPS_VARS ADDS TO PREVIOUS VALUE. NEEDS TO BE
! PROPERLY INITIALIZED.
                  TEMP_L1 = GET_TRANS_RPS_SUM(INT2_zero,ST_TG,
     +                                             RESOURCE_RPS_VARS)
                  CALL GET_HYDRO_RPS_SUM(INT2_zero,ST_TG,
     +                                             RESOURCE_RPS_VARS)
                  CALL GET_THERMAL_RPS_SUM(
     +                               INT2_zero,ST_TG,RESOURCE_RPS_VARS)
                  DO J = 8, 14
                     RESOURCE_RPS_VARS(J) = RESOURCE_RPS_VARS(J) / 12.0
                  ENDDO
                  TRANS_STATE_REC = rptrec(TRANS_STATE_NO)
                  WRITE(TRANS_STATE_NO,REC=TRANS_STATE_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(globecom_year+get_BASE_YEAR()),
     +               CL_MONTH_NAME(13),
     +               TEMP_NAME,
     +               (ANNUAL_FUEL_COST_BY_SP_BY_FUEL(I,J),
     +                                       J=1,max_fuel_types_clr),
     +               (ANNUAL_FUEL_BTUS_BY_SP_BY_FUEL(I,J),
     +                                       J=1,max_fuel_types_clr),
     +               (FUEL_COST_PER_MMBTU(J),J=1,max_fuel_types_clr),
     +               EV_DATA_SOURCE,
     +               (ANNUAL_GROUP_VAR_BY_SP(I,J),J=1,MAX_GROUP_VAR),
     +               UNIT_EMISSIONS,
     +               (ANNUAL_EMISSIONS_COST_BY_SP(I,J),J=1,5),
     +               EMISSION_COST_PER_TON,
     +               STATE_RPS_VARS,
     +               RESOURCE_RPS_VARS
                  TRANS_STATE_REC = TRANS_STATE_REC + 1
               ENDDO
            ENDIF
!
            IF(TRANSACT_PROD_REPORT_ACTIVE) THEN
               DO I = 1, UPPER_TRANS_GROUP
                  ANNUAL_LOAD_BY_TG_BY_MWH(I,2) =
     +                      ANNUAL_LOAD_BY_TG_BY_MWH(I,4)/8760.

                  TRANS_PROD_REC = RPTREC(TRANS_PROD_NO)

                  ! Transact Production report (msgmopro)
                  WRITE(TRANS_PROD_NO,REC=TRANS_PROD_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(globecom_year+get_BASE_YEAR()),
     +               CL_MONTH_NAME(13),
     +               MULTI_AREA_NAME(I),
     +               (ANNUAL_PROD_BY_TG_BY_MW(I,J),
     +                        J=1,max_prod_types_for_report),
     +               (ANNUAL_PROD_BY_TG_BY_MWH(I,J),
     +                        J=1,max_prod_types_for_report),
     +               2.0,
     +               (ANNUAL_LOAD_BY_TG_BY_MWH(I,J),
     +                        J=1,MAX_LOAD_TYPES)
     
        ! Start trace msgmopro 2
             call write_trace_message(file_trace_pclr, "2. msgmopro")
            call write_trace_int(file_trace_pclr, "TRAN_PROD_FUEL_REC",
     + TRAN_PROD_FUEL_REC)
        if(file_trace_pclr/=BAD_TRACE_HANDLE) then
           WRITE(file_trace_pclr,*)
     + "PRT_ENDPOINT", PRT_ENDPOINT(),
     +  "YEAR", FLOAT(globecom_year+get_BASE_YEAR()),
     +  "CL_MONTH_NAME",   CL_MONTH_NAME(13),
     + "MULTI_AREA_NAME",  MULTI_AREA_NAME(I),
     + "ANNUAL_PROD_BY_TG_BY_MW", 
     + (ANNUAL_PROD_BY_TG_BY_MW(I,J),J=1,max_prod_types_clr),
     +  "ANNUAL_PROD_BY_MK_BY_MWH", 
     + (ANNUAL_PROD_BY_TG_BY_MWH(I,J),J=1,max_prod_types_clr),
     +               2.0, "ANNUAL_LOAD_BY_TG_BY_MWH", 
     +               (ANNUAL_LOAD_BY_TG_BY_MWH(I,J),
     +                        J=1,MAX_LOAD_TYPES)
        end if
        ! end trace msgmopro 2
                  TRANS_PROD_REC = TRANS_PROD_REC + 1
        
                  TRAN_PROD_FUEL_REC = rptrec(TRAN_PROD_FUEL_NO)
                  WRITE(TRAN_PROD_FUEL_NO,REC=TRAN_PROD_FUEL_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(globecom_year+get_BASE_YEAR()),
     +               CL_MONTH_NAME(13),
     +               MULTI_AREA_NAME(I),
     +               (ANNUAL_MW_BY_TG_BY_FUEL(I,J),
     +                                            J=1,MFT1),
     +               (ANNUAL_MWH_BY_TG_BY_FUEL(I,J),
     +                                            J=1,MFT1)
       ! Start trace TRAN_PROD_FUEL_REC 1
       call write_trace_message(file_trace_pclr, "1 TRAN_PROD_FUEL")
       call write_trace_int(file_trace_pclr, "TRAN_PROD_FUEL_REC",
     +   TRAN_PROD_FUEL_REC)
        if(file_trace_pclr/=BAD_TRACE_HANDLE) then
            write(file_trace_pclr, *)
     + "PRT_ENDPOINT", PRT_ENDPOINT(),
     + "YEAR",FLOAT(globecom_year+get_BASE_YEAR()),
     + "CL_MONTH_NAME",               CL_MONTH_NAME(13),
     + "MULTI_AREA_NAME", MULTI_AREA_NAME(I), "ANNUAL_MW_BY_TG_BY_FUEL",
     +               (ANNUAL_MW_BY_TG_BY_FUEL(I,J),
     +                                            J=1,MFT1),
     + "ANNUAL_MWH_BY_TG_BY_FUEL",
     +               (ANNUAL_MWH_BY_TG_BY_FUEL(I,J),
     +                                            J=1,MFT1)
        end if
        
        
        ! End trace TRAN_PROD_FUEL_REC 1
          
                  TRAN_PROD_FUEL_REC = TRAN_PROD_FUEL_REC + 1
!
               ENDDO
            ENDIF
!
            MONTHLY_STARTS = 0
            MONTHLY_START_COSTS = 0.
            MONTHLY_CAPITAL_COST_OF_BUILD = 0.
            MONTHLY_EBITDA = 0.
            MONTHLY_CHARGING_ENERGY = 0.
            MONTHLY_CAPACITY_REVENUE = 0.
!
            MONTHLY_NOX_SEASON_THERM = 0.
            MONTHLY_TRANS_CAPACITY = 0.0
            MONTHLY_TRANS_EFFECTIVE_CAP = 0.0
            MONTHLY_TRANS_AVAIL_CAP = 0.0
            MONTHLY_TRANS_ENERGY = 0.0
!
            MONTHLY_P_HEAT = 0.0 ! D0
            MONTHLY_S_HEAT = 0.0 ! D0
            MONTHLY_E_HEAT = 0.0 ! D0
!
            MONTHLY_TRANS_HEAT = 0.0 D0
            MONTHLY_TRANS_FUEL_COST = 0.0 D0
            MONTHLY_TRANS_VAR_COST = 0.0
!
            MONTHLY_UNIT_EMISSIONS(:) = 0.0
!
!
            ANNUAL_LOCAL_EMIS_COST(:) = 0.0
!
            MONTHLY_TRANS_FIXED_COST = 0.
!
            ANNUAL_ECO_SALES = 0.
            ANNUAL_ECO_SALES_ENRG = 0.
            ANNUAL_ECO_PUCH = 0.
            ANNUAL_ECO_PUCH_ENRG = 0.
!
            ANNUAL_WHOLESALE_COST = 0.
!
!
! ONLY CALLED ONCE FOR RETIREMENT LOGIC.
!
            UNIT_GROSS_MARGIN = INIT_ANNUAL_GROSS_MARGIN()
!
            DO I = 1, get_NUNITS()
!

!              ! Fifth assignment of Unitno
               UNITNO = I
               call write_trace_int2(file_trace_pclr, "3. UNIT", UNITNO)
               cur_called_from="pcr:3"
               CL_TRANS_NAME = UNITNM(I)//CL_UNIQUE_RPT_STR(I) ! '  '
               ENRG = ANNUAL_CL_UNIT_ENERGY(UNITNO)
            call write_trace_real(file_trace_pclr, "6. ENRG", ENRG)
! 1/12/00.
               ANNUAL_REV_GEN_CAPACITY(I) =
     +                           ANNUAL_REV_GEN_CAPACITY(I) /
     +                                                       LAST_SEASON
               CAPBLK = ANNUAL_EFFECTIVE_CAP(I) / LAST_SEASON
               IF(MONTHS_ACTIVE(I) >= LAST_SEASON) THEN
                  ANNUAL_CAP(I) = ANNUAL_CAP(I) / LAST_SEASON
               ELSEIF(MONTHS_ACTIVE(I) > 0) THEN ! MODIFIED 9/23/02.
                  ANNUAL_CAP(I) = ANNUAL_CAP(I) /
     +                                           FLOAT(MONTHS_ACTIVE(I))
               ELSE
                  ANNUAL_CAP(I) = MW(2,I)
               ENDIF
! 8/28/02
               CLASS = GET_ASSET_CLASS_NUM(UNITNO)
               P_FUEL_DELIVERY = ABS(GET_P_FUEL_DELIVERY(UNITNO))
               PBTUCT_SAVE = ABS(GET_PBTUCT_SAVE(UNITNO))
!
               IF(ENRG > 0.45) THEN
                  HEAT = ANNUAL_CL_UNIT_MMBTUS(UNITNO)
                  FUEL_COST = ANNUAL_CL_UNIT_FUEL_COST(UNITNO)
!
! EMISSIONS
!
                  UNIT_EMISSIONS(1) = ANNUAL_CL_UNIT_EMISSIONS(1,UNITNO)
                  UNIT_EMISSIONS(2) = ANNUAL_CL_UNIT_EMISSIONS(2,UNITNO)
                  UNIT_EMISSIONS(3) = ANNUAL_CL_UNIT_EMISSIONS(3,UNITNO)
                  UNIT_EMISSIONS(4) = ANNUAL_CL_UNIT_EMISSIONS(4,UNITNO)
                  UNIT_EMISSIONS(5) = ANNUAL_CL_UNIT_EMISSIONS(5,UNITNO)
!
                  UNIT_EMISSIONS_COST(1) =
     +                              ANNUAL_UNIT_EMISSIONS_COST(UNITNO,1)
                  UNIT_EMISSIONS_COST(2) =
     +                              ANNUAL_UNIT_EMISSIONS_COST(UNITNO,2)
                  UNIT_EMISSIONS_COST(3) =
     +                              ANNUAL_UNIT_EMISSIONS_COST(UNITNO,3)
                  UNIT_EMISSIONS_COST(4) =
     +                              ANNUAL_UNIT_EMISSIONS_COST(UNITNO,4)
                  UNIT_EMISSIONS_COST(5) =
     +                              ANNUAL_UNIT_EMISSIONS_COST(UNITNO,5)
!
               ELSE
                  HEAT = 0.D0
                  FUEL_COST = 0.
!
                  UNIT_EMISSIONS(1) = 0.
                  UNIT_EMISSIONS(2) = 0.
                  UNIT_EMISSIONS(3) = 0.
                  UNIT_EMISSIONS(4) = 0.
                  UNIT_EMISSIONS(5) = 0.
!
                  UNIT_EMISSIONS_COST(1) = 0.
                  UNIT_EMISSIONS_COST(2) = 0.
                  UNIT_EMISSIONS_COST(3) = 0.
                  UNIT_EMISSIONS_COST(4) = 0.
                  UNIT_EMISSIONS_COST(5) = 0.
!
               ENDIF
               IF(.NOT. TESTING_PLAN) THEN
                  IF(.NOT. CANADA .AND. PHASE_I_UNIT(UNITNO) ) THEN
                     UNIT_NAME = trim(UNITNM(UNITNO))//'*'
                  ELSE
                     UNIT_NAME = UNITNM(UNITNO)
                  ENDIF
                  IF(ENRG > 0.45) THEN
                     AVERAGE_HEATRATE = HEAT_CONVERSION*HEAT/ENRG
                     AVERAGE_PRODUCTION_COSTS =
     +                        (SNGL((FUEL_COST)) +
     +                             ANNUAL_CL_UNIT_VAR_COST(UNITNO))/ENRG
                     IF(.NOT. CANADA) AVERAGE_HEATRATE =
     +                                             AVERAGE_HEATRATE + .5
                  ELSE
                     FUEL_COST = 0.0 D0
                     HEAT = 0.0 D0
                     AVERAGE_HEATRATE = 0.0
                     AVERAGE_PRODUCTION_COSTS = 0.0
                  ENDIF
                  IF(ANNUAL_CAP(I) > 0.01) THEN
                     TRANS_EQUIV_AVAIL =  100. *
     +                        ANNUAL_REV_GEN_CAPACITY(I) / ANNUAL_CAP(I)
                     TRANS_CAP_FACTOR = ENRG * 100. /
     +                                           (ANNUAL_CAP(I) * 8760.)
                  ELSE
                     TRANS_EQUIV_AVAIL = 0.0
                     TRANS_CAP_FACTOR = 0.0
                  ENDIF
!
                  TOTAL_UNIT_COST =
     +               (SNGL(FUEL_COST) +
     +                   ANNUAL_CL_UNIT_VAR_COST(UNITNO) +
     +                       ANNUAL_CL_UNIT_FIXED_COST(UNITNO))/1000000.
!
                  IF(ECO_SALES_ENRG_FROM(UNITNO) > 0.1) THEN
                     AVE_REV_FROM_SALES =  ECO_SALES_REV_FROM(UNITNO) /
     +                                       ECO_SALES_ENRG_FROM(UNITNO)
                  ELSE
                     AVE_REV_FROM_SALES =  0.
                  ENDIF
!
                  MONTHLY_NOX_SEASON_THERM = MONTHLY_NOX_SEASON_THERM
     +                                    + ANNUAL_NOX_SEASON_THERMAL(I)
                  ANNUAL_ECO_SALES = ANNUAL_ECO_SALES +
     +                                        ECO_SALES_REV_FROM(UNITNO)
                  ANNUAL_ECO_SALES_ENRG = ANNUAL_ECO_SALES_ENRG +
     +                                       ECO_SALES_ENRG_FROM(UNITNO)
                  ANNUAL_ECO_PUCH = ANNUAL_ECO_PUCH +
     +                                        ECO_PUCH_COST_FROM(UNITNO)
                  ANNUAL_ECO_PUCH_ENRG = ANNUAL_ECO_PUCH_ENRG +
     +                                        ECO_PUCH_ENRG_FROM(UNITNO)
!
                  MONTHLY_STARTS = MONTHLY_STARTS +
     +                                        ANNUAL_UNIT_STARTS(UNITNO)
                  MONTHLY_START_COSTS = MONTHLY_START_COSTS +
     +                                   ANNUAL_UNIT_START_COSTS(UNITNO)
                  MONTHLY_CAPITAL_COST_OF_BUILD =
     +                     MONTHLY_CAPITAL_COST_OF_BUILD +
     +                              ANNUAL_CAPITAL_COST_OF_BUILD(UNITNO)
                  MONTHLY_EBITDA = MONTHLY_EBITDA +
     +                                             ANNUAL_EBITDA(UNITNO)

                  MONTHLY_CAPACITY_REVENUE =
     +                        MONTHLY_CAPACITY_REVENUE +
     +                              ANNUAL_UNIT_CAPACITY_REVENUE(UNITNO)
!
                  MONTHLY_TRANS_CAPACITY =
     +                            MONTHLY_TRANS_CAPACITY + ANNUAL_CAP(I)
                  MONTHLY_TRANS_EFFECTIVE_CAP =
     +                              MONTHLY_TRANS_EFFECTIVE_CAP + CAPBLK
                  MONTHLY_TRANS_AVAIL_CAP = MONTHLY_TRANS_AVAIL_CAP +
     +                                        ANNUAL_REV_GEN_CAPACITY(I)
                  MONTHLY_TRANS_ENERGY = MONTHLY_TRANS_ENERGY + ENRG
                  MONTHLY_TRANS_HEAT = MONTHLY_TRANS_HEAT + HEAT
                  MONTHLY_P_HEAT = MONTHLY_P_HEAT +
     +                                 ANNUAL_P_FUEL_CONSUMPTION(UNITNO)
                  MONTHLY_S_HEAT = MONTHLY_S_HEAT +
     +                                 ANNUAL_S_FUEL_CONSUMPTION(UNITNO)
                  MONTHLY_E_HEAT = MONTHLY_E_HEAT +
     +                                 ANNUAL_E_FUEL_CONSUMPTION(UNITNO)
                  MONTHLY_TRANS_FUEL_COST = MONTHLY_TRANS_FUEL_COST +
     +                                                         FUEL_COST
! 6/13/01. To account for varying VCPMWH.
                  MONTHLY_TRANS_VAR_COST = MONTHLY_TRANS_VAR_COST +
     +                                   ANNUAL_CL_UNIT_VAR_COST(UNITNO)

                  MONTHLY_UNIT_EMISSIONS(1) =
     +                           MONTHLY_UNIT_EMISSIONS(1) +
     +                                                 UNIT_EMISSIONS(1)
                  MONTHLY_UNIT_EMISSIONS(2) =
     +                           MONTHLY_UNIT_EMISSIONS(2) +
     +                                                 UNIT_EMISSIONS(2)
                  MONTHLY_UNIT_EMISSIONS(3) =
     +                           MONTHLY_UNIT_EMISSIONS(3) +
     +                                                 UNIT_EMISSIONS(3)
                  MONTHLY_UNIT_EMISSIONS(4) =
     +                           MONTHLY_UNIT_EMISSIONS(4) +
     +                                                 UNIT_EMISSIONS(4)
                  MONTHLY_UNIT_EMISSIONS(5) =
     +                           MONTHLY_UNIT_EMISSIONS(5) +
     +                                                 UNIT_EMISSIONS(5)
                  MONTHLY_TRANS_FIXED_COST = MONTHLY_TRANS_FIXED_COST +
     +                                 ANNUAL_CL_UNIT_FIXED_COST(UNITNO)
!
                  ANNUAL_LOCAL_EMIS_COST(1) =
     +               ANNUAL_LOCAL_EMIS_COST(1) +
     +                                            UNIT_EMISSIONS_COST(1)
                  ANNUAL_LOCAL_EMIS_COST(2) =
     +               ANNUAL_LOCAL_EMIS_COST(2) +
     +                                            UNIT_EMISSIONS_COST(2)
                  ANNUAL_LOCAL_EMIS_COST(3) =
     +               ANNUAL_LOCAL_EMIS_COST(3) +
     +                                            UNIT_EMISSIONS_COST(3)
                  ANNUAL_LOCAL_EMIS_COST(4) =
     +               ANNUAL_LOCAL_EMIS_COST(4) +
     +                                            UNIT_EMISSIONS_COST(4)
                  ANNUAL_LOCAL_EMIS_COST(5) =
     +               ANNUAL_LOCAL_EMIS_COST(5) +
     +                                            UNIT_EMISSIONS_COST(5)
!
                  imm_i=i
                  gpm_result=GET_PRIMARY_MOVER(imm_I)
                  FT = MAX(1,MIN(gpm_result,6))
                  SP = MIN(MAX(0,GET_UNIT_STATE_PROVINCE_INDEX(I)),
     +                                            MAX_STATE_PROVINCE_NO)

                  TG = get_TRANSACTION_GROUP(UNITNO)
!
                  ONLINE_DATE = 190000.+FLOAT(ONLINE(I))
                  OFLINE_DATE = 190000.+FLOAT(OFLINE(I))
!
! 10/04/01
!
                  TEMP_L1 = GET_CL_BASECASE_MARKET_ID(I,MARKET_ID)
                  MK = MARKET_AREA_LOOKUP(MARKET_ID(1:5))
                  GET_PRIMARY_MOVER_INDEX_called_from="pclr:0003"
                  PM = GET_PRIMARY_MOVER_INDEX(I)
                  RTG = TG
                  RMK = MK
                  RFT = FT
                  RSP = FLOAT(GET_THERMAL_STATE_INDEX(I))
                  CHARGING_ENERGY = 0.0

                  RPM = PM
                  HESI_ID_NUM_4_UNIT = GET_HESI_UNIT_ID_NUM(I,
     +                                    HESI_SECOND_UNIT_ID_NUM)

                  POWERDAT_PLANT_ID = GET_POWERDAT_PLANT_ID(I)

                  NEWGEN_UNIT_STATUS = FLOAT(GET_NEWGEN_INDEX(I))

                  IF(HEAT /= 0.) THEN
                     AVERAGE_SOX_RATE=TONS_CONVERSION*UNIT_EMISSIONS(1)/
     +                                             HEAT
                     AVERAGE_FUEL_COST = FUEL_COST/HEAT
                  ELSE
                     AVERAGE_SOX_RATE = 0.
                     AVERAGE_FUEL_COST = 0.
                  ENDIF
                  AVERAGE_VAR_OM = 0.
                  IF(ENRG /= 0.) AVERAGE_VAR_OM =
     +                              ANNUAL_CL_UNIT_VAR_COST(UNITNO)/ENRG
                  AVERAGE_NOX_RATE = 0.
                  IF(ANNUAL_NOX_SEASON_THERMAL(I) > 0.) THEN
                     AVERAGE_NOX_RATE=TONS_CONVERSION*UNIT_EMISSIONS(2)/
     +                                    ANNUAL_NOX_SEASON_THERMAL(I)
                  ENDIF
!
                  UNIT_GROSS_MARGIN =
     +                  ECO_SALES_REV_FROM(UNITNO)/1000000. -
     +                                                   TOTAL_UNIT_COST
                  IF(RUN_TRANSACT) THEN
                     TEMP_R4 = ANNUAL_EBITDA(UNITNO) ! 102509.
                     CO2_COST = UNIT_EMISSIONS_COST(3)
                     CAP_COST = CAP_MARKET_REVENUE
                     TEMP_R4 =
     +                  SAVE_ANNUAL_GROSS_MARGIN(UNITNO,
     +                                           ANNUAL_CAP(I),
     +                                           TEMP_R4,
     +                                           UNIT_EMISSIONS(3),
     +                                           ENRG,
     +                                           CO2_COST,
     +                                           CAP_COST)
                     MRX_RPS_CL_CURVE_GROSS_MARGIN(UNITNO) =
     +                     1000.*(ANNUAL_EBITDA(UNITNO) +
     +                                               CAP_MARKET_REVENUE)
                     MRX_RPS_CL_CURVE_REVENUE(UNITNO) =
     +                     0.001*ECO_SALES_REV_FROM(UNITNO) +
     +                        1000.*ANNUAL_UNIT_CAPACITY_REVENUE(UNITNO)
                     MRX_RPS_CL_CURVE_COST(UNITNO) =
     +                    1000.*(TOTAL_UNIT_COST + TOTAL_EMISSION_COSTS)
                     MRX_RPS_CL_CURVE_UMWH(UNITNO) = ENRG


                  ENDIF

                  RETAIL_SALES = MAX(0.,
     +                       (ENRG - ECO_SALES_ENRG_FROM(UNITNO))/1000.)
                  ANNUAL_WHOLESALE_COST = ANNUAL_WHOLESALE_COST +
     +                                     ANNUAL_WHOLESALE_PROD_COST(I)

!
                  IF(TG <= MAX_TRANS_GROUP_NUMBER .AND.
     +                  TRANS_GROUP_REPORTING_ACTIVE(TG) .AND.
     +                      REPORT_MARKET_AREA(MARKET_ID) .AND.
     +                              REPORT_THIS_CL_UNIT(UNITNO) .AND.
     +                                .NOT. FISCAL_ONLY .AND.
     +                                 (MONTHLY_TRANS_REPORT .OR.
     +                                   ANNUAL_TRANS_REPORT .OR.
     +                                       NEW_UNITS_REPORT)) THEN

                    TOTAL_EMISSION_COSTS = SUM(UNIT_EMISSIONS_COST)
!
                     IF(TRANS_EQUIV_AVAIL > 0.) THEN
                        UNECONOMIC_RATE = 100.*(1. -
     +                             (TRANS_CAP_FACTOR/TRANS_EQUIV_AVAIL))
                     ELSE
                        UNECONOMIC_RATE = 0.
                     ENDIF
!
! 050519.
                     IF(AllHoursMonth(UNITNO) > 0.) THEN
                        ANNUAL_CAP(UNITNO) =
     +                        CapHours(UNITNO) / AllHoursMonth(UNITNO)
                        CAPBLK =
     +                        EffectiveCapHours(UNITNO) /
     +                                             AllHoursMonth(UNITNO)
                        TRANS_EQUIV_AVAIL =
     +                        100. * AvailHoursMonth(UNITNO) / 8760.
                        TRANS_CAP_FACTOR =
     +                        100. * ENRG / (8760. * ANNUAL_CAP(UNITNO))
                        UNECONOMIC_RATE =
     +                              TRANS_EQUIV_AVAIL - TRANS_CAP_FACTOR
                     ELSE
                        ANNUAL_CAP(I) = 0.
                        CAPBLK = 0.
                        TRANS_EQUIV_AVAIL = 0.
                        TRANS_CAP_FACTOR = 0.
                        UNECONOMIC_RATE = 0.
                     ENDIF

                     IF(MONTHLY_TRANS_REPORT .OR.
     +                                         ANNUAL_TRANS_REPORT) THEN

                   MON_CL_TRANS_UNIT_REC = RPTREC(MON_CL_TRANS_UNIT_NO)
                   !msgmtrcl

                   ! TODO: Put trace here.
                   call write_unique_msgmtrcl_log_entry(7)
                   WRITE(MON_CL_TRANS_UNIT_NO,REC=MON_CL_TRANS_UNIT_REC)
     +                  PRT_ENDPOINT(),
     +                  fLocal_year,
     +                  CL_MONTH_NAME(13),
     +                  CL_TRANS_NAME,
     +                  FLOAT(I),
     +                  ANNUAL_CAP(I),
     +                  CAPBLK,
     +                  TRANS_EQUIV_AVAIL,
     +                  TRANS_CAP_FACTOR,
     +                  ENRG/1000.,
     +                  SNGL(HEAT),
     +                  AVERAGE_HEATRATE,
     +                  SNGL(FUEL_COST)/1000000.,
     +                  ANNUAL_CL_UNIT_VAR_COST(UNITNO)/1000000., ! 9
     +                  AVERAGE_PRODUCTION_COSTS,
     +                  (UNIT_EMISSIONS(EM),
     +                     EM=1,NUMBER_OF_EMISSION_TYPES),
     +                  ANNUAL_CL_UNIT_FIXED_COST(UNITNO)/1000000., ! 16
     +                  TOTAL_UNIT_COST,
     +                  ECO_SALES_REV_FROM(UNITNO)/1000000.,
     +                  ECO_SALES_ENRG_FROM(UNITNO)/1000.,
     +                  AVE_REV_FROM_SALES,
     +                  UNIT_GROSS_MARGIN,
     +                  ECO_PUCH_COST_FROM(UNITNO)/1000000.,
     +                  ECO_PUCH_ENRG_FROM(UNITNO)  /1000.,
     +                  ANNUAL_P_FUEL_CONSUMPTION(UNITNO),
     +                  ANNUAL_S_FUEL_CONSUMPTION(UNITNO),
     +                  ANNUAL_E_FUEL_CONSUMPTION(UNITNO),
     +                  AVERAGE_NOX_RATE,
     +                  AVERAGE_SOX_RATE,
     +                  AVERAGE_FUEL_COST,
     +                  AVERAGE_VAR_OM,
     +                  (UNIT_EMISSIONS_COST(EM),
     +                     EM=1,NUMBER_OF_EMISSION_TYPES),
     +                  ANNUAL_WHOLESALE_PROD_COST(I),
     +                  RETAIL_SALES,
     +                  RTG,RMK,RFT,
     +                  HESI_ID_NUM_4_UNIT,
     +                  POWERDAT_PLANT_ID,
     +                  FLOAT(ANNUAL_UNIT_STARTS(UNITNO)),
     +                  TOTAL_EMISSION_COSTS,
     +                  ANNUAL_UNIT_START_COSTS(UNITNO)/1000000.,
     +                  FLOAT(CLASS),
     +                  PBTUCT_SAVE,
     +                  P_FUEL_DELIVERY,
     +                  UNECONOMIC_RATE,
     +                  NEWGEN_UNIT_STATUS,
     +                  ONLINE_DATE,
     +                  OFLINE_DATE,
     +                  RPM,
     +                  HESI_SECOND_UNIT_ID_NUM,
     +                  EV_DATA_SOURCE,
     +                  EV_PLANNING_AREA,
     +                  ANNUAL_UNIT_CAPACITY_REVENUE(UNITNO),
     +                  ANNUAL_CAPITAL_COST_OF_BUILD(UNITNO),
     +                  ANNUAL_EBITDA(UNITNO),
     +                  RSP,
     +                  CHARGING_ENERGY/1000.
      ! Trace start
       call write_trace_message(file_trace_pclr, "3. MSGMCTRL report")
       call write_trace_int(file_trace_pclr, "MON_CL_TRANS_UNIT_REC",
     + MON_CL_TRANS_UNIT_REC)
       if(file_trace_pclr/=BAD_TRACE_HANDLE) then
                         WRITE(file_trace_pclr,*)
     + "PRT_ENDPOINT", PRT_ENDPOINT(), "LOCAL_YEAR", fLocal_year,
     + "CL_MONTH_NAME", CL_MONTH_NAME(13), "CL_TRANS_NAME", 
     + CL_TRANS_NAME, "I", FLOAT(I), "ANNUAL_CAP", ANNUAL_CAP(I),
     + "CAPBLK", CAPBLK, "TRANS_EQUIV_AVAIL", TRANS_EQUIV_AVAIL,
     + "TRANS_CAP_FACTOR", TRANS_CAP_FACTOR, "ENRG", ENRG/1000.,
     + "HEAT", SNGL(HEAT), "AVERAGE_HEATRATE", AVERAGE_HEATRATE,
     + "FUEL_COST", SNGL(FUEL_COST)/1000000., "ANNUAL_CL_UNIT_VAR_COST",
     + ANNUAL_CL_UNIT_VAR_COST(UNITNO)/1000000., 
     + "AVERAGE_PRODUCTION_COSTS", AVERAGE_PRODUCTION_COSTS,
     + "UNIT_EMISSIONS", (UNIT_EMISSIONS(EM),
     +  EM=1,NUMBER_OF_EMISSION_TYPES), "ANNUAL_CL_UNIT_FIXED_COST", 
     + ANNUAL_CL_UNIT_FIXED_COST(UNITNO)/1000000., "TOTAL_UNIT_COST", 
     + TOTAL_UNIT_COST, "ECO_SALES_REV_FROM", 
     + ECO_SALES_REV_FROM(UNITNO)/1000000., "ECO_SALES_ENRG_FROM", 
     + ECO_SALES_ENRG_FROM(UNITNO)/1000., "AVE_REV_FROM_SALES", 
     + AVE_REV_FROM_SALES, "UNIT_GROSS_MARGIN", UNIT_GROSS_MARGIN,
     + "ECO_PUCH_COST_FROM", ECO_PUCH_COST_FROM(UNITNO)/1000000.,
     + "ECO_PUCH_ENRG_FROM", ECO_PUCH_ENRG_FROM(UNITNO)  /1000.,
     + "ANNUAL_P_FUEL_CONSUMPTION", ANNUAL_P_FUEL_CONSUMPTION(UNITNO),
     + "ANNUAL_S_FUEL_CONSUMPTION", ANNUAL_S_FUEL_CONSUMPTION(UNITNO),
     + "ANNUAL_E_FUEL_CONSUMPTION", ANNUAL_E_FUEL_CONSUMPTION(UNITNO),
     + "AVERAGE_NOX_RATE", AVERAGE_NOX_RATE, "AVERAGE_SOX_RATE", 
     + AVERAGE_SOX_RATE, "AVERAGE_FUEL_COST", AVERAGE_FUEL_COST,
     + "AVERAGE_VAR_OM", AVERAGE_VAR_OM, "UNIT_EMISSIONS_COST", 
     + (UNIT_EMISSIONS_COST(EM),EM=1,NUMBER_OF_EMISSION_TYPES),
     + "ANNUAL_WHOLESALE_PROD_COST", ANNUAL_WHOLESALE_PROD_COST(I),
     + "RETAIL_SALES", RETAIL_SALES, "RTG", RTG, "RMK", RMK, "RFT", RFT,
     + "HESI_ID_NUM_4_UNIT", HESI_ID_NUM_4_UNIT, "POWERDAT_PLANT_ID", 
     + POWERDAT_PLANT_ID, "ANNUAL_UNIT_STARTS", 
     + FLOAT(ANNUAL_UNIT_STARTS(UNITNO)), "TOTAL_EMISSION_COSTS", 
     + TOTAL_EMISSION_COSTS, "ANNUAL_UNIT_START_COSTS", 
     + ANNUAL_UNIT_START_COSTS(UNITNO)/1000000., "CLASS", FLOAT(CLASS),
     + "PBTUCT_SAVE", PBTUCT_SAVE, "P_FUEL_DELIVERY", 
     + P_FUEL_DELIVERY, "UNECONOMIC_RATE", UNECONOMIC_RATE,
     + "NEWGEN_UNIT_STATUS", NEWGEN_UNIT_STATUS, "ONLINE_DATE", 
     + ONLINE_DATE, "OFLINE_DATE", OFLINE_DATE, "RPM", RPM,
     + "HESI_SECOND_UNIT_ID_NUM", HESI_SECOND_UNIT_ID_NUM,
     + "EV_DATA_SOURCE", EV_DATA_SOURCE, "EV_PLANNING_AREA", 
     + EV_PLANNING_AREA, "ANNUAL_UNIT_CAPACITY_REVENUE", 
     + ANNUAL_UNIT_CAPACITY_REVENUE(UNITNO),
     + "ANNUAL_CAPITAL_COST_OF_BUILD", 
     + ANNUAL_CAPITAL_COST_OF_BUILD(UNITNO), "ANNUAL_EBITDA", 
     + ANNUAL_EBITDA(UNITNO), "RSP", RSP,
     + "CHARGING_ENERGY", CHARGING_ENERGY/1000.
     
      end if
      ! Trace end
            
                       MON_CL_TRANS_UNIT_REC = MON_CL_TRANS_UNIT_REC + 1
                    ENDIF ! WRITE THE ANNUAL THERMAL UNITS REPORT
                    IF(NEW_UNITS_REPORT .AND. ONLINE(UNITNO) >
     +                                                  BEGIN_DATE) THEN
               MON_NU_TRANS_UNIT_REC = rptrec(MON_NU_TRANS_UNIT_NO)

                       WRITE(MON_NU_TRANS_UNIT_NO,
     +                                        REC=MON_NU_TRANS_UNIT_REC)
     +                  PRT_ENDPOINT(),
     +                  fLocal_year,
     +                  CL_MONTH_NAME(13),
     +                  CL_TRANS_NAME,
     +                  FLOAT(I),
     +                  ANNUAL_CAP(I),
     +                  CAPBLK,
     +                  TRANS_EQUIV_AVAIL,
     +                  TRANS_CAP_FACTOR,
     +                  ENRG/1000.,
     +                  SNGL(HEAT),
     +                  AVERAGE_HEATRATE,
     +                  SNGL(FUEL_COST)/1000000.,
     +                  ANNUAL_CL_UNIT_VAR_COST(UNITNO)/1000000.,
     +                  AVERAGE_PRODUCTION_COSTS,
     +                  (UNIT_EMISSIONS(EM),
     +                     EM=1,NUMBER_OF_EMISSION_TYPES),
     +                  ANNUAL_CL_UNIT_FIXED_COST(UNITNO)/1000000.,
     +                  TOTAL_UNIT_COST,
     +                  ECO_SALES_REV_FROM(UNITNO)/1000000.,
     +                  ECO_SALES_ENRG_FROM(UNITNO)/1000.,
     +                  AVE_REV_FROM_SALES,
     +                  UNIT_GROSS_MARGIN,
     +                  ECO_PUCH_COST_FROM(UNITNO)/1000000.,
     +                  ECO_PUCH_ENRG_FROM(UNITNO)/1000.,
     +                  ANNUAL_P_FUEL_CONSUMPTION(UNITNO),
     +                  ANNUAL_S_FUEL_CONSUMPTION(UNITNO),
     +                  ANNUAL_E_FUEL_CONSUMPTION(UNITNO),
     +                  AVERAGE_NOX_RATE,
     +                  AVERAGE_SOX_RATE,
     +                  AVERAGE_FUEL_COST,
     +                  AVERAGE_VAR_OM,
     +                  (UNIT_EMISSIONS_COST(EM),
     +                                   EM=1,NUMBER_OF_EMISSION_TYPES),
     +                  ANNUAL_WHOLESALE_PROD_COST(I),
     +                  RETAIL_SALES,
     +                  RTG,RMK,RFT,
     +                  HESI_ID_NUM_4_UNIT,
     +                  POWERDAT_PLANT_ID,
     +                  FLOAT(ANNUAL_UNIT_STARTS(UNITNO)),
     +                  TOTAL_EMISSION_COSTS,
     +                  ANNUAL_UNIT_START_COSTS(UNITNO)/1000000.,
     +                  FLOAT(CLASS),
     +                  PBTUCT_SAVE,
     +                  P_FUEL_DELIVERY,
     +                  UNECONOMIC_RATE,
     +                  NEWGEN_UNIT_STATUS,
     +                  ONLINE_DATE,
     +                  OFLINE_DATE,
     +                  RPM,
     +                  HESI_SECOND_UNIT_ID_NUM,
     +                  EV_DATA_SOURCE,
     +                  EV_PLANNING_AREA,
     +                  ANNUAL_UNIT_CAPACITY_REVENUE(UNITNO),
     +                  ANNUAL_CAPITAL_COST_OF_BUILD(UNITNO),
     +                  ANNUAL_EBITDA(UNITNO),
     +                  RSP,
     +                  CHARGING_ENERGY/1000.
                     MON_NU_TRANS_UNIT_REC = MON_NU_TRANS_UNIT_REC + 1
                    ENDIF ! WRITE THE ANNUAL NEW UNITS REPORT
                  ENDIF ! WRITE ONE OF THE REPORTS
               ENDIF ! TESTING PLAN
            ENDDO ! UNITS
!
! GET NUMBER OF TRANSACTIONS
!
            SUM_ANNUAL = 0


            DO TRANS = 1, NUM_TRANSACTIONS
               VOID_LOGICAL = GET_MONTHLY_TRANS_VARIABLES(TRANS,
     +                                               ENRG,
     +                                               TRANS_CAP,
     +                                               TRANS_VAR_EXP,
     +                                               TRANS_VAR_MWH,
     +                                               TRANS_FIX_EXP,
     +                                               TRANS_REV,
     +                                               TRANS_REV_MWH,
     +                                               TRANS_HOURS,
     +                                               PRODUCT_HOURS,
     +                                               TRANS_STRIKES,
     +                                               INT2_zero,
     +                                               TRANS_NAME,
     +                                               SUM_ANNUAL,
     +                                               YES_REPORT_PRODUCT,
     +                                               TG,
     +                                               PM,
     +                                               FT,
     +                                               TRANS_ON_LINE,
     +                                               TRANS_OFF_LINE,
     +                                               TRANS_UNIT_ID,
     +                                               TRANS_ASSET_CLASS,
     +                                               TRANS_NOT_ACTIVE,
     +                                               TEMP_YEAR,
     +                                      CHARGING_ENERGY) ! 060308
! 121806
               IF(TRANS_NOT_ACTIVE) CYCLE
               I = I + 1
!
               CL_TRANS_NAME = TRANS_NAME
!
               TRANS_STRIKES = 0.0 ! 092107. PER BURESH.
!
               TG_POSITION = MAX(1,GET_TRANS_GROUP_POSITION(TG))
               FT = MAX(1,MIN(FT,6))

               RTG = TG
               RFT = FT
               RSP = FLOAT(GET_TRANS_STATE_INDEX(TRANS))
               MONTHLY_CHARGING_ENERGY = MONTHLY_CHARGING_ENERGY +
     +                                                   CHARGING_ENERGY

               RPM = PM

               ANNUAL_REV_GEN_CAPACITY(I) =
     +                           ANNUAL_REV_GEN_CAPACITY(I) /
     +                                                       LAST_SEASON
               CAPBLK = ANNUAL_EFFECTIVE_CAP(I) / LAST_SEASON

               IF(MONTHS_ACTIVE(I) > 0) THEN
                  TRANS_CAP = TRANS_CAP / FLOAT(MONTHS_ACTIVE(I))
               ELSE
                  TRANS_CAP = 0.0
               ENDIF

! EMISSIONS
                  HEAT = 0.D0
                  FUEL_COST = 0.

                  NEWGEN_UNIT_STATUS = 0.

                  UNIT_EMISSIONS(1) = 0.
                  UNIT_EMISSIONS(2) = 0.
                  UNIT_EMISSIONS(3) = 0.
                  UNIT_EMISSIONS(4) = 0.
                  UNIT_EMISSIONS(5) = 0.
!
                  UNIT_EMISSIONS_COST(1) = 0.
                  UNIT_EMISSIONS_COST(2) = 0.
                  UNIT_EMISSIONS_COST(3) = 0.
                  UNIT_EMISSIONS_COST(4) = 0.
                  UNIT_EMISSIONS_COST(5) = 0.

               IF(.NOT. TESTING_PLAN) THEN

                  IF(ABS(ENRG) > 0.45) THEN
                     AVERAGE_HEATRATE = 0.
! TODO: Determine why I put the following code in and took it back out:
!

                  ELSE
                     FUEL_COST = 0.0 D0
                     HEAT = 0.0 D0
                     AVERAGE_HEATRATE = 0.0
                     AVERAGE_PRODUCTION_COSTS = 0.0
                  ENDIF
                  IF(TRANS_CAP > 0.01 .AND. PRODUCT_HOURS > 0) THEN
                     TRANS_CAP_FACTOR = ENRG * 100. /
     +                                   (TRANS_CAP * PRODUCT_HOURS)
                  ELSE
                     TRANS_CAP_FACTOR = 0.0
                  ENDIF

                  TOTAL_UNIT_COST = TRANS_VAR_EXP + TRANS_FIX_EXP

                  IF(ABS(ENRG) > 0.1) THEN
                     AVE_REV_FROM_SALES =   ABS(TRANS_REV / ENRG)
                  ELSE
                     AVE_REV_FROM_SALES =  0.
                  ENDIF

                  ANNUAL_ECO_SALES = ANNUAL_ECO_SALES +  TRANS_REV
                  ANNUAL_ECO_SALES_ENRG = ANNUAL_ECO_SALES_ENRG +
     +                                                              ENRG

                  MONTHLY_TRANS_CAPACITY =
     +                            MONTHLY_TRANS_CAPACITY + TRANS_CAP
                  MONTHLY_TRANS_EFFECTIVE_CAP =
     +                              MONTHLY_TRANS_EFFECTIVE_CAP + CAPBLK
                  MONTHLY_TRANS_AVAIL_CAP = MONTHLY_TRANS_AVAIL_CAP +
     +                                        ANNUAL_REV_GEN_CAPACITY(I)
                  MONTHLY_TRANS_ENERGY = MONTHLY_TRANS_ENERGY + ENRG

                  MONTHLY_TRANS_VAR_COST = MONTHLY_TRANS_VAR_COST +
     +                                                     TRANS_VAR_EXP

                  MONTHLY_TRANS_FIXED_COST = MONTHLY_TRANS_FIXED_COST +
     +                                                     TRANS_FIX_EXP

                  AVERAGE_VAR_OM = 0.
                  IF(ENRG /= 0.) AVERAGE_VAR_OM = TRANS_VAR_EXP / ENRG

                  UNIT_GROSS_MARGIN =
     +                  (TRANS_REV - TOTAL_UNIT_COST)/1000000.
                  TOTAL_EMISSION_COSTS = SUM(UNIT_EMISSIONS_COST)
                  UNIT_EBITDA = (TRANS_REV-TOTAL_UNIT_COST-
     +                                    TOTAL_EMISSION_COSTS)/1000000.

                  WHOLESALE_PRODUCTION_COST =
     +                                     TRANS_VAR_MWH * ENRG/1000000.
                  RETAIL_SALES = MAX(0.,(ENRG - ENRG)/1000.)
                  ANNUAL_WHOLESALE_COST = ANNUAL_WHOLESALE_COST +
     +                                         WHOLESALE_PRODUCTION_COST

                  TEMP_R4 = GET_TRANS_ITER_CAP_PERCENT(TRANS)
                  MRX_RPS_DV_CURVE_REVENUE(TRANS) = TEMP_R4 *
     +                     (TRANS_REV*0.001 + CAP_MARKET_REVENUE*1000.0)
                  MRX_RPS_DV_CURVE_COST(TRANS) = TEMP_R4 *
     +                     (TOTAL_UNIT_COST*0.001 +
     +                                      TOTAL_EMISSION_COSTS*1000.0)
                  MRX_RPS_DV_CURVE_GROSS_MARGIN(TRANS) =
     +                     MRX_RPS_DV_CURVE_REVENUE(TRANS) -
     +                                 MRX_RPS_DV_CURVE_COST(TRANS)
                  MRX_RPS_DV_CURVE_UMWH(TRANS) = TEMP_R4 * ENRG
!
                  POWERDAT_PLANT_ID = GET_Holding_Company_ID(TRANS)

                  TEMP_L1 = GET_DERIV_ZONE_ID(I,MARKET_ID)
                  MK = MARKET_AREA_LOOKUP(MARKET_ID(1:5))
                  RMK = FLOAT(MK)
                  IF(MK < 0 .OR. MK > 600) THEN
                     MK =MAX(0,
     +                      MIN(600,MARKET_AREA_LOOKUP(MARKET_ID(1:5))))
                  ENDIF
! 050519.
                  IF(TransAllHoursMonth(TRANS) > 0.) THEN
                     TRANS_CAP =
     +                  TransCapHours(TRANS) / TransAllHoursMonth(TRANS)
                     CAPBLK =
     +                        TransEffectiveCapHours(TRANS) /
     +                                         TransAllHoursMonth(TRANS)
                     TRANS_EQUIV_AVAIL =
     +                        100. * TransAvailHoursMonth(TRANS) / 8760.
                     TRANS_CAP_FACTOR =
     +                        100. * ENRG / (8760. * TRANS_CAP)
                     UNECONOMIC_RATE =
     +                              TRANS_EQUIV_AVAIL - TRANS_CAP_FACTOR
                  ELSE
                     TRANS_CAP = 0.
                     CAPBLK = 0.
                     TRANS_EQUIV_AVAIL = 0.
                     TRANS_CAP_FACTOR = 0.
                     UNECONOMIC_RATE = 0.
                  ENDIF
                   IF( (MONTHLY_TRANS_REPORT .OR.
     +                                   ANNUAL_TRANS_REPORT) .AND.
     +                     .NOT. SUPPRESS_DERIVATIVES .AND.
     +                                .NOT. FISCAL_ONLY .AND.
     +                                          YES_REPORT_PRODUCT) THEN
!

                   MON_CL_TRANS_UNIT_REC = RPTREC(MON_CL_TRANS_UNIT_NO)
                   !msgmtrcl

                   call write_unique_msgmtrcl_log_entry(8)
                   WRITE(MON_CL_TRANS_UNIT_NO,REC=MON_CL_TRANS_UNIT_REC)
     +                  PRT_ENDPOINT(),
     +                  fLocal_year,
     +                  CL_MONTH_NAME(13),
     +                  CL_TRANS_NAME,
     +                  FLOAT(I),
     +                  TRANS_CAP,
     +                  CAPBLK,
     +                  TRANS_EQUIV_AVAIL,
     +                  TRANS_CAP_FACTOR,
     +                  ENRG/1000.,
     +                  0.,
     +                  TRANS_STRIKES,
     +                  0.,
     +                  TRANS_VAR_EXP/1000000., ! 9
     +                  AVERAGE_PRODUCTION_COSTS,
     +                  (UNIT_EMISSIONS(EM),
     +                     EM=1,NUMBER_OF_EMISSION_TYPES),
     +                  TRANS_FIX_EXP/1000000., ! 16 Fixed cost ($/MMW)
     +                  TOTAL_UNIT_COST/1000000.,
     +                  TRANS_REV/1000000.,
     +                  ENRG/1000.,
     +                  AVE_REV_FROM_SALES,
     +                  UNIT_GROSS_MARGIN ,
     +                  0.,
     +                  0.,
     +                  0.,
     +                  0.,
     +                  0.,
     +                  0.,
     +                  0.,
     +                  0.,
     +                  AVERAGE_VAR_OM,
     +                  (UNIT_EMISSIONS_COST(EM),
     +                     EM=1,NUMBER_OF_EMISSION_TYPES),
     +                  WHOLESALE_PRODUCTION_COST,
     +                  RETAIL_SALES,
     +                  RTG,RMK,RFT,
     +                  TRANS_UNIT_ID, ! HESI UNIT ID NUM
     +                  POWERDAT_PLANT_ID,  ! POWER PLANT ID NUM
     +                  0.,
     +                  TOTAL_EMISSION_COSTS,
     +                  0.,
     +                  TRANS_ASSET_CLASS, ! ASSET CLASS
     +                  0.,
     +                  0.,
     +                  0., ! UNECONOMIC_RATE
     +                  NEWGEN_UNIT_STATUS,
     +                  TRANS_ON_LINE, ! ONLINE
     +                  TRANS_OFF_LINE, ! OFLINE
     +                  RPM,
     +                  0., ! HESI_SECOND_UNIT_ID
     +                  EV_DATA_SOURCE,
     +                  EV_PLANNING_AREA,
     +                  CAP_MARKET_REVENUE,
     +                  CAPITAL_COST_OF_BUILD,
     +                  UNIT_EBITDA,
     +                  RSP,
     +                  CHARGING_ENERGY/1000.
                     MON_CL_TRANS_UNIT_REC = MON_CL_TRANS_UNIT_REC + 1
                  ENDIF ! THERMAL UNITS REPORT
!
                  IF(DERIVATIVES_REPORT) THEN
               MON_DV_TRANS_UNIT_REC = rptrec(MON_DV_TRANS_UNIT_NO)
                    WRITE(MON_DV_TRANS_UNIT_NO,
     +                                        REC=MON_DV_TRANS_UNIT_REC)
     +                  PRT_ENDPOINT(),
     +                  fLocal_year,
     +                  CL_MONTH_NAME(13),
     +                  CL_TRANS_NAME,
     +                  FLOAT(I),
     +                  TRANS_CAP,
     +                  PRODUCT_HOURS,
     +                  TRANS_HOURS,
     +                  TRANS_CAP_FACTOR,
     +                  ENRG/1000.,
     +                  0.,
     +                  TRANS_STRIKES,
     +                  0.,
     +                  TRANS_VAR_EXP/1000000.,
     +                  AVERAGE_PRODUCTION_COSTS,
     +                  (UNIT_EMISSIONS(EM),
     +                     EM=1,NUMBER_OF_EMISSION_TYPES),
     +                  TRANS_FIX_EXP/1000000.,
     +                  TOTAL_UNIT_COST/1000000.,
     +                  TRANS_REV/1000000.,
     +                  ENRG/1000.,
     +                  AVE_REV_FROM_SALES,
     +                  UNIT_GROSS_MARGIN ,
     +                  0.,
     +                  0.,
     +                  0.,
     +                  0.,
     +                  0.,
     +                  0.,
     +                  0.,
     +                  0.,
     +                  AVERAGE_VAR_OM,
     +                  (UNIT_EMISSIONS_COST(EM),
     +                     EM=1,NUMBER_OF_EMISSION_TYPES),
     +                  WHOLESALE_PRODUCTION_COST,
     +                  RETAIL_SALES,
     +                  RTG,RMK,RFT,
     +                  TRANS_UNIT_ID, ! HESI UNIT ID NUM
     +                  POWERDAT_PLANT_ID, ! POWER PLANT ID NUM
     +                  0.,
     +                  TOTAL_EMISSION_COSTS,
     +                  0.,
     +                  TRANS_ASSET_CLASS, ! ASSET CLASS
     +                  0.,
     +                  0.,
     +                  0., ! UNECONOMIC_RATE
     +                  NEWGEN_UNIT_STATUS,
     +                  TRANS_ON_LINE, ! ONLINE
     +                  TRANS_OFF_LINE, ! OFLINE
     +                  RPM,
     +                  0., ! HESI_SECOND_UNIT_ID
     +                  EV_DATA_SOURCE,
     +                  EV_PLANNING_AREA,
     +                  CAP_MARKET_REVENUE,
     +                  CAPITAL_COST_OF_BUILD,
     +                  UNIT_EBITDA,
     +                  RSP,
     +                  CHARGING_ENERGY/1000.
                     MON_DV_TRANS_UNIT_REC = MON_DV_TRANS_UNIT_REC + 1
                  ENDIF ! DERIVATIVES REPORT
               ENDIF
            ENDDO ! DERIVATIVES
!
            CL_TRANS_NAME = 'Total Resources       '
            I = I + 1
!
            IF(MONTHLY_TRANS_ENERGY > 0.45) THEN
               AVERAGE_HEATRATE = HEAT_CONVERSION*MONTHLY_TRANS_HEAT/
     +                                              MONTHLY_TRANS_ENERGY
               AVERAGE_PRODUCTION_COSTS =
     +               (SNGL(MONTHLY_TRANS_FUEL_COST) +
     +                              MONTHLY_TRANS_VAR_COST) /
     +                                              MONTHLY_TRANS_ENERGY
               AVERAGE_VAR_OM = MONTHLY_TRANS_VAR_COST/
     +                            MONTHLY_TRANS_ENERGY
               IF(.NOT. CANADA) AVERAGE_HEATRATE = AVERAGE_HEATRATE + .5
            ELSE
               MONTHLY_TRANS_FUEL_COST = 0.0 D0
               MONTHLY_TRANS_HEAT = 0.0 D0
               AVERAGE_HEATRATE = 0.0
               AVERAGE_PRODUCTION_COSTS = 0.0
               AVERAGE_VAR_OM = 0.
            ENDIF
            IF(MONTHLY_TRANS_CAPACITY > 0.01 .AND. SEAS_HOURS > 0) THEN
               TRANS_EQUIV_AVAIL =  100. * MONTHLY_TRANS_AVAIL_CAP/
     +                                            MONTHLY_TRANS_CAPACITY
               TRANS_CAP_FACTOR = MONTHLY_TRANS_ENERGY * 100. /
     +                                  (MONTHLY_TRANS_CAPACITY * 8760.)
            ELSE
               TRANS_EQUIV_AVAIL = 0.0
               TRANS_CAP_FACTOR = 0.0
            ENDIF
!
            TOTAL_UNIT_COST =
     +               (SNGL(MONTHLY_TRANS_FUEL_COST) +
     +                       MONTHLY_TRANS_VAR_COST +
     +                                MONTHLY_TRANS_FIXED_COST)/1000000.
!
            IF(ABS(ANNUAL_ECO_SALES_ENRG) > 0.1) THEN
               AVE_REV_FROM_SALES =  ABS(ANNUAL_ECO_SALES /
     +                                            ANNUAL_ECO_SALES_ENRG)
            ELSE
               AVE_REV_FROM_SALES =  0.
            ENDIF
!
            AVERAGE_NOX_RATE = 0.
            AVERAGE_SOX_RATE = 0.
            AVERAGE_FUEL_COST = 0.
            IF(MONTHLY_NOX_SEASON_THERM /= 0.) THEN
               AVERAGE_NOX_RATE =
     +                        TONS_CONVERSION*MONTHLY_UNIT_EMISSIONS(2)/
     +                                 MONTHLY_NOX_SEASON_THERM
            ENDIF
!
            IF(MONTHLY_TRANS_HEAT /= 0.) THEN
               AVERAGE_SOX_RATE =
     +                        TONS_CONVERSION*MONTHLY_UNIT_EMISSIONS(1)/
     +                                  MONTHLY_TRANS_HEAT
               AVERAGE_FUEL_COST = MONTHLY_TRANS_FUEL_COST/
     +                               MONTHLY_TRANS_HEAT
            ENDIF
!
            TRANS_TOTAL_GROSS_MARGIN =
     +               ANNUAL_ECO_SALES/1000000. - TOTAL_UNIT_COST

            MONTHLY_RETAIL_SALES = MAX(0.,
     +            (MONTHLY_TRANS_ENERGY - ANNUAL_ECO_SALES_ENRG)/1000.)
!
            IF( (MONTHLY_TRANS_REPORT .OR. ANNUAL_TRANS_REPORT) .AND.
     +                                          .NOT. FISCAL_ONLY ) THEN
               TOTAL_EMISSION_COSTS = 0
               DO EM = 1, NUMBER_OF_EMISSION_TYPES
                  TOTAL_EMISSION_COSTS = TOTAL_EMISSION_COSTS
     +                               + ANNUAL_LOCAL_EMIS_COST(EM)
               ENDDO
!
               IF(TRANS_EQUIV_AVAIL > 0.) THEN
                  UNECONOMIC_RATE = 100.*(1. -
     +                             (TRANS_CAP_FACTOR/TRANS_EQUIV_AVAIL))
               ELSE
                  UNECONOMIC_RATE = 0.
               ENDIF

                MON_CL_TRANS_UNIT_REC = RPTREC(MON_CL_TRANS_UNIT_NO)
                !msgmtrcl - total

                call write_unique_msgmtrcl_log_entry(9)
       ! TODO: Put trace here.
                WRITE(MON_CL_TRANS_UNIT_NO,REC=MON_CL_TRANS_UNIT_REC)
     +                  PRT_ENDPOINT(),
     +               fLocal_year,
     +               CL_MONTH_NAME(13),
     +               CL_TRANS_NAME,
     +               FLOAT(I),
     +               MONTHLY_TRANS_CAPACITY,
     +               MONTHLY_TRANS_EFFECTIVE_CAP,
     +               TRANS_EQUIV_AVAIL,
     +               TRANS_CAP_FACTOR,
     +               MONTHLY_TRANS_ENERGY/1000.,
     +               SNGL(MONTHLY_TRANS_HEAT),
     +               AVERAGE_HEATRATE,
     +               SNGL(MONTHLY_TRANS_FUEL_COST)/1000000.,
     +               MONTHLY_TRANS_VAR_COST/1000000., ! 9
     +               AVERAGE_PRODUCTION_COSTS,
     +               (MONTHLY_UNIT_EMISSIONS(EM),
     +                     EM=1,NUMBER_OF_EMISSION_TYPES),
     +               MONTHLY_TRANS_FIXED_COST/1000000., ! 16
     +               TOTAL_UNIT_COST,
     +               ANNUAL_ECO_SALES/1000000.,
     +               ANNUAL_ECO_SALES_ENRG/1000.,
     +               AVE_REV_FROM_SALES,
     +               TRANS_TOTAL_GROSS_MARGIN,
     +               ANNUAL_ECO_PUCH/1000000.,
     +               ANNUAL_ECO_PUCH_ENRG/1000.,
     +               MONTHLY_P_HEAT,
     +               MONTHLY_S_HEAT,
     +               MONTHLY_E_HEAT,

     +               AVERAGE_NOX_RATE, ! 27
     +               AVERAGE_SOX_RATE,
     +               AVERAGE_FUEL_COST,
     +               AVERAGE_VAR_OM,
     +               (ANNUAL_LOCAL_EMIS_COST(EM),
     +                     EM=1,NUMBER_OF_EMISSION_TYPES),
     +               ANNUAL_WHOLESALE_COST,
     +               MONTHLY_RETAIL_SALES,
     +               999.,999.,999.,99999.,99999.,
     +               FLOAT(MONTHLY_STARTS),
     +               TOTAL_EMISSION_COSTS,
     +               MONTHLY_START_COSTS/1000000.,
     +               999., ! ASSET CLASS
     +               0.,
     +               0.,
     +               UNECONOMIC_RATE,
     +               NEWGEN_UNIT_STATUS,
     +               0.,
     +               0.,
     +               0.,
     +               0.,
     +               EV_DATA_SOURCE,
     +               EV_PLANNING_AREA,
     +               MONTHLY_CAPACITY_REVENUE,
     +               MONTHLY_CAPITAL_COST_OF_BUILD,
     +               MONTHLY_EBITDA,
     +               999.,
     +               MONTHLY_CHARGING_ENERGY/1000.
               MON_CL_TRANS_UNIT_REC = MON_CL_TRANS_UNIT_REC + 1
            ENDIF
            IF( NEW_UNITS_REPORT .AND. ONLINE(UNITNO) > BEGIN_DATE) THEN
               TOTAL_EMISSION_COSTS = 0

               TOTAL_EMISSION_COSTS = SUM(MONTHLY_UNIT_EMISSIONS_COST)
               IF(TRANS_EQUIV_AVAIL > 0.) THEN
                  UNECONOMIC_RATE = 100.*(1. -
     +                             (TRANS_CAP_FACTOR/TRANS_EQUIV_AVAIL))
               ELSE
                  UNECONOMIC_RATE = 0.
               ENDIF

               MON_NU_TRANS_UNIT_REC = RPTREC(MON_NU_TRANS_UNIT_NO)
               WRITE(MON_NU_TRANS_UNIT_NO,REC=MON_NU_TRANS_UNIT_REC)
     +               PRT_ENDPOINT(),
     +               fLocal_year,
     +               CL_MONTH_NAME(13),
     +               CL_TRANS_NAME,
     +               FLOAT(I),
     +               MONTHLY_TRANS_CAPACITY,
     +               MONTHLY_TRANS_EFFECTIVE_CAP,
     +               TRANS_EQUIV_AVAIL,
     +               TRANS_CAP_FACTOR,
     +               MONTHLY_TRANS_ENERGY/1000.,
     +               SNGL(MONTHLY_TRANS_HEAT),
     +               AVERAGE_HEATRATE,
     +               SNGL(MONTHLY_TRANS_FUEL_COST)/1000000.,
     +               MONTHLY_TRANS_VAR_COST/1000000.,
     +               AVERAGE_PRODUCTION_COSTS,
     +               (MONTHLY_UNIT_EMISSIONS(EM),
     +                     EM=1,NUMBER_OF_EMISSION_TYPES),
     +               MONTHLY_TRANS_FIXED_COST/1000000.,
     +               TOTAL_UNIT_COST,
     +               ANNUAL_ECO_SALES/1000000.,
     +               ANNUAL_ECO_SALES_ENRG/1000.,
     +               AVE_REV_FROM_SALES,
     +               TRANS_TOTAL_GROSS_MARGIN,
     +               ANNUAL_ECO_PUCH/1000000.,
     +               ANNUAL_ECO_PUCH_ENRG/1000.,
     +               MONTHLY_P_HEAT,
     +               MONTHLY_S_HEAT,
     +               MONTHLY_E_HEAT,
     +               AVERAGE_NOX_RATE, ! 27
     +               AVERAGE_SOX_RATE,
     +               AVERAGE_FUEL_COST,
     +               AVERAGE_VAR_OM,
     +               (ANNUAL_LOCAL_EMIS_COST(EM),
     +                                   EM=1,NUMBER_OF_EMISSION_TYPES),
     +               ANNUAL_WHOLESALE_COST,
     +               MONTHLY_RETAIL_SALES,
     +               999.,999.,999.,99999.,99999.,
     +               FLOAT(MONTHLY_STARTS),
     +               TOTAL_EMISSION_COSTS,
     +               MONTHLY_START_COSTS/1000000.,
     +               999., ! ASSET CLASS
     +               0.,
     +               0.,
     +               UNECONOMIC_RATE,
     +               NEWGEN_UNIT_STATUS,
     +               0.,
     +               0.,
     +               0.,
     +               0.,
     +               EV_DATA_SOURCE,
     +               EV_PLANNING_AREA,
     +               MONTHLY_CAPACITY_REVENUE,
     +               MONTHLY_CAPITAL_COST_OF_BUILD,
     +               MONTHLY_EBITDA,
     +               999.,
     +               MONTHLY_CHARGING_ENERGY/1000.
               MON_NU_TRANS_UNIT_REC = MON_NU_TRANS_UNIT_REC + 1
            ENDIF
!
         ENDIF ! ISEAS == LAST_SEASON
         MW(1:2,1:get_NUNITS()) = SaveMW2(1:2,1:get_NUNITS())
      ENDIF ! MONTHLY_CL_UNIT_REPORT
      MONTHLY_KEPCO_SALES = 0.
      MONTHLY_KEPCO_SALES_REV = 0.
!
      MONTHLY_REPORTING_CAPACITY = 0.
      MONTHLY_CONTRACT_CAPACITY = 0.
      MONTHLY_MINIMUM_CAPACITY = 0.
      MONTHLY_ENRG = 0.
      MONTHLY_CONTRACT_VARIABLE_COST = 0.
      MONTHLY_CNTR_MAX_FIXED_COST = 0.
      MONTHLY_CNTR_MIN_FIXED_COST = 0.
      MONTHLY_TOTAL_FIXED_COST = 0.
      MONTHLY_TOTAL_CONTRACT_COST = 0.
      MONTHLY_CNTR_EMISSIONS = 0.
!
      IF(ISEAS == 1) THEN
         ANNUAL_REPORTING_CAPACITY = 0.
         ANNUAL_CNTR_CAPACITY = 0.
         ANNUAL_MINIMUM_CAPACITY = 0.
         ANNUAL_ENRG = 0.
         ANNUAL_CNTR_VARIABLE_COST = 0.
         ANNUAL_CNTR_MAX_FIXED_COST = 0.
         ANNUAL_CNTR_MIN_FIXED_COST = 0.
         ANNUAL_TOTAL_FIXED_COST = 0.
         ANNUAL_TOTAL_CONTRACT_COST = 0.
         ANNUAL_CNTR_EMISSIONS = 0.
      ENDIF
!
! TODO: EXTRACT_METHOD handle_contracts
      DO I = 1, NUMBER_OF_CONTRACTS
         IF(CNTR_ON_LI(I) <= DATE2 .AND. CNTR_OFF_LI(I) >= DATE1) THEN
            TOTAL_CONTRACT_COST = 0.0
            ENRG = SEAS_HOURS * CONTRACT_ENERGY(I)
       call write_trace_real(file_trace_pclr, "7. ENRG", ENRG)
            CNTR_ENRG = CNTR_ENRG + ENRG
            CNTR_EMISSIONS = 0.
            CNTR_MIN_FIXED_COST =MIN(MINIMUM_CAPACITY(I),
     +                               CONTRACT_CAPACITY(I)) *
     +                           MIN_CONTRACT_FIXED_COST(I)/1000.
            IF(CNTR_CAPACITY_SWITCH(I) == 'V') THEN
               CNTR_MAX_FIXED_COST = CONTRACT_FIXED_COST(I) *
     +                (CONTRACT_CAPACITY(I) - MIN(MINIMUM_CAPACITY(I),
     +                CONTRACT_CAPACITY(I)))/1000.
            ELSEIF(KEPCO .AND. .NOT. REALLY_KEPCO) THEN
               CNTR_MAX_FIXED_COST = CONTRACT_FIXED_COST(I) *
     +             (MAX(MAXIMUM_CAPACITY(I),CONTRACT_CAPACITY(I)) -
     +              MIN(MINIMUM_CAPACITY(I),CONTRACT_CAPACITY(I)))/1000.
            ELSEIF(REALLY_KEPCO) THEN
               IF(MAX_RATCHET_PATTERN(I) /= 0) THEN
                  CONTRACT_BILLING_CAPACITY =
     +                                    MAX(RATCHET_CAPACITY_BASIS(I),
     +                                          MIN_RATCHET_CAPACITY(I),
     +                                             CONTRACT_CAPACITY(I))
                  CAPACITY_BILLED_AT_MIN =  MIN(MINIMUM_CAPACITY(I),
     +                                        CONTRACT_BILLING_CAPACITY)
                  CNTR_MIN_FIXED_COST = CAPACITY_BILLED_AT_MIN *
     +                                  MIN_CONTRACT_FIXED_COST(I)/1000.
                  CAPACITY_BILLED_AT_MAX=MAX(0.,
     +                                       CONTRACT_BILLING_CAPACITY-
     +                                           CAPACITY_BILLED_AT_MIN)
                  CNTR_MAX_FIXED_COST = CAPACITY_BILLED_AT_MAX *
     +                                      CONTRACT_FIXED_COST(I)/1000.
               ELSE
                  CNTR_MAX_FIXED_COST = CONTRACT_FIXED_COST(I) *
     +                (MAX(MAXIMUM_CAPACITY(I),CONTRACT_CAPACITY(I)) -
     +              MIN(MINIMUM_CAPACITY(I),CONTRACT_CAPACITY(I)))/1000.
               ENDIF
            ELSE
               CNTR_MAX_FIXED_COST = CONTRACT_FIXED_COST(I) *
     +                (MAXIMUM_CAPACITY(I) - MIN(MINIMUM_CAPACITY(I),
     +                CONTRACT_CAPACITY(I)))/1000.
            ENDIF
            IF(KEPCO) THEN
               IF(INDEX('1,2,3,4,5,6',CNTRTYPE(I)) .NE. 0) THEN
                  ANNUAL_CONTRACT_CAPACITY(I)=MAX(CONTRACT_CAPACITY(I),
     +                                      ANNUAL_CONTRACT_CAPACITY(I))
               ELSEIF(INDEX('SX',CNTRTYPE(I)) /= 0) THEN
                  MONTHLY_KEPCO_SALES = MONTHLY_KEPCO_SALES + ABS(ENRG)
                  MONTHLY_KEPCO_SALES_REV = MONTHLY_KEPCO_SALES_REV +
     +                           CONTRACT_VARIABLE_COST(I)*ENRG/1000000.
               ELSE
                  ANNUAL_CONTRACT_CAPACITY(I) = MAX(MAXIMUM_CAPACITY(I),
     +                                      ANNUAL_CONTRACT_CAPACITY(I))
               ENDIF
            ELSE
               IF(ISEAS == PEAK_MONTH) ANNUAL_CONTRACT_CAPACITY(I) =
     +                                            CONTRACT_CAPACITY(I)
            ENDIF
            ANNUAL_CONTRACT_FIXED_COST(I) =
     +               ANNUAL_CONTRACT_FIXED_COST(I) +
     +               CNTR_MAX_FIXED_COST + CNTR_MIN_FIXED_COST
            TOTAL_CONTRACT_COST = TOTAL_CONTRACT_COST +
     +                    CNTR_MAX_FIXED_COST + CNTR_MIN_FIXED_COST +
     +                    CONTRACT_VARIABLE_COST(I)*ENRG/1000000.
            IF(ENRG > 0.5 .OR. ENRG < -.5) THEN
               ANNUAL_CONTRACT_ENERGY(I) = ENRG +
     +                                  ANNUAL_CONTRACT_ENERGY(I)
               ANNUAL_CONTRACT_VARIABLE_COST(I) =
     +                             ANNUAL_CONTRACT_VARIABLE_COST(I) +
     +                           CONTRACT_VARIABLE_COST(I)*ENRG/1000000.
!
! EMISSIONS
!
               CNTR_EMISSIONS = CNTR_SO2(I)*ENRG/2000.
               MONTHLY_TOTAL_EMISSIONS(1) = CNTR_EMISSIONS +
     +                                      MONTHLY_TOTAL_EMISSIONS(1)
               ANNUAL_CONTRACT_SO2(I) = ANNUAL_CONTRACT_SO2(I) +
     +                                                  CNTR_EMISSIONS
            ENDIF
             IF(MONTHLY_CONTRACT_REPORT_ACTIVE .AND.
     +          CONTRACTS_IN_PERIOD .AND. .NOT. TESTING_PLAN .AND.
     +              (ENRG > .0001 .OR.  ENRG < -.05 .OR.
     +                     CONTRACT_CAPACITY(I) > .01 .OR.
     +                                   TOTAL_CONTRACT_COST > 0.)) THEN
               UNIT_NAME = CNTRNM(I)
               REPORTING_CAPACITY = MAXIMUM_CAPACITY(I)
               IF(REALLY_KEPCO) THEN
                  IF(INDEX('SX',CNTRTYPE(I)) /= 0) THEN
                     UNIT_NAME = trim(CNTRNM(I))//'+'
                     REPORTING_CAPACITY = MAXIMUM_CAPACITY(I)
                  ELSEIF(MAX_RATCHET_PATTERN(I) /= 0) THEN
                     REPORTING_CAPACITY = CONTRACT_BILLING_CAPACITY
                  ENDIF
               ENDIF
               IF( ENRG .LT. .5 .AND. ENRG .GT. -.5) THEN
                  ENRG = 0.0
       call write_trace_real(file_trace_pclr, "7. ENRG", ENRG)
                  DIVIDE_BY_ENRG = 1E-15
                  AVERAGE_TOTAL_COST = 0.
               ELSE
                  DIVIDE_BY_ENRG = ENRG
                  AVERAGE_TOTAL_COST  =
     +               CONTRACT_VARIABLE_COST(I) +
     +               (CNTR_MIN_FIXED_COST+
     +               CNTR_MAX_FIXED_COST)*1000000./DIVIDE_BY_ENRG
               ENDIF
!
               MONTHLY_REPORTING_CAPACITY = MONTHLY_REPORTING_CAPACITY +
     +                                                REPORTING_CAPACITY
               MONTHLY_CONTRACT_CAPACITY = MONTHLY_CONTRACT_CAPACITY +
     +                                              CONTRACT_CAPACITY(I)
               MONTHLY_MINIMUM_CAPACITY = MONTHLY_MINIMUM_CAPACITY +
     +                                               MINIMUM_CAPACITY(I)
               MONTHLY_ENRG = MONTHLY_ENRG + ENRG/1000.
               MONTHLY_CONTRACT_VARIABLE_COST =
     +                 MONTHLY_CONTRACT_VARIABLE_COST +
     +                 CONTRACT_VARIABLE_COST(I)*ENRG/1000000.
               MONTHLY_CNTR_MAX_FIXED_COST =
     +                 MONTHLY_CNTR_MAX_FIXED_COST + CNTR_MAX_FIXED_COST
               MONTHLY_CNTR_MIN_FIXED_COST =
     +                 MONTHLY_CNTR_MIN_FIXED_COST + CNTR_MIN_FIXED_COST
               MONTHLY_TOTAL_FIXED_COST = MONTHLY_TOTAL_FIXED_COST +
     +                         CNTR_MAX_FIXED_COST + CNTR_MIN_FIXED_COST
               MONTHLY_TOTAL_CONTRACT_COST =
     +                 MONTHLY_TOTAL_CONTRACT_COST + TOTAL_CONTRACT_COST
               MONTHLY_CNTR_EMISSIONS = MONTHLY_CNTR_EMISSIONS +
     +                                                    CNTR_EMISSIONS
!
               MON_CT_UNIT_REC = rptrec(MON_CT_UNIT_NO)
               WRITE(MON_CT_UNIT_NO,REC=MON_CT_UNIT_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(globecom_year+get_BASE_YEAR()),
     +               CL_MONTH_NAME(ISEAS),
     +               UNIT_NAME,
     +               FLOAT(I),
     +               REPORTING_CAPACITY,
     +               CONTRACT_CAPACITY(I),MINIMUM_CAPACITY(I),
     +               ENRG/1000.,
     +               CONTRACT_VARIABLE_COST(I)*ENRG/1000000.,
     +               CONTRACT_VARIABLE_COST(I),CNTR_MAX_FIXED_COST,
     +               CNTR_MIN_FIXED_COST,
     +               CNTR_MAX_FIXED_COST+CNTR_MIN_FIXED_COST,
     +               TOTAL_CONTRACT_COST,
     +               AVERAGE_TOTAL_COST,
     +               CNTR_EMISSIONS
               MON_CT_UNIT_REC = MON_CT_UNIT_REC + 1
            ENDIF
         ENDIF
      ENDDO
      IF(MONTHLY_CONTRACT_REPORT_ACTIVE .AND.
     +          CONTRACTS_IN_PERIOD .AND. .NOT. TESTING_PLAN .AND.
     +              (MONTHLY_ENRG > .0001 .OR.
     +                     MONTHLY_CONTRACT_CAPACITY > .01 .OR.
     +                           MONTHLY_TOTAL_CONTRACT_COST > 0.)) THEN
!
         UNIT_NAME = 'Contract Total      '
         IF(MONTHLY_ENRG == 0.) THEN
            MONTHLY_CONTRACT_AVE_VAR_COST = 0.
            MONTHLY_AVERAGE_TOTAL_COST = 0.
         ELSE
            MONTHLY_CONTRACT_AVE_VAR_COST =
     +                 1000.*MONTHLY_CONTRACT_VARIABLE_COST/MONTHLY_ENRG
            MONTHLY_AVERAGE_TOTAL_COST =
     +                    1000.*MONTHLY_TOTAL_CONTRACT_COST/MONTHLY_ENRG
         ENDIF
! TODO: EXTRACT_METHOD handle_contracts (end)
! TODO: EXTRACT_METHOD handle_capacity_enrg_and_emissions
         ANNUAL_REPORTING_CAPACITY = ANNUAL_REPORTING_CAPACITY +
     +                                        MONTHLY_REPORTING_CAPACITY
         ANNUAL_CNTR_CAPACITY = ANNUAL_CNTR_CAPACITY +
     +                                         MONTHLY_CONTRACT_CAPACITY
         ANNUAL_MINIMUM_CAPACITY = ANNUAL_MINIMUM_CAPACITY +
     +                                          MONTHLY_MINIMUM_CAPACITY
         ANNUAL_ENRG = ANNUAL_ENRG + MONTHLY_ENRG
         ANNUAL_CNTR_VARIABLE_COST =
     +                 ANNUAL_CNTR_VARIABLE_COST +
     +                                    MONTHLY_CONTRACT_VARIABLE_COST
         ANNUAL_CNTR_MAX_FIXED_COST =
     +                 ANNUAL_CNTR_MAX_FIXED_COST +
     +                                       MONTHLY_CNTR_MAX_FIXED_COST
         ANNUAL_CNTR_MIN_FIXED_COST =
     +                 ANNUAL_CNTR_MIN_FIXED_COST +
     +                                       MONTHLY_CNTR_MIN_FIXED_COST
         ANNUAL_TOTAL_FIXED_COST = ANNUAL_TOTAL_FIXED_COST +
     +                 MONTHLY_CNTR_MAX_FIXED_COST +
     +                                       MONTHLY_CNTR_MIN_FIXED_COST
         ANNUAL_TOTAL_CONTRACT_COST =
     +                 ANNUAL_TOTAL_CONTRACT_COST +
     +                                       MONTHLY_TOTAL_CONTRACT_COST
         ANNUAL_CNTR_EMISSIONS = ANNUAL_CNTR_EMISSIONS +
     +                                            MONTHLY_CNTR_EMISSIONS
! TODO: EXTRACT_METHOD handle_capacity_enrg_and_emissions (end)
         MON_CT_UNIT_REC = rptrec(MON_CT_UNIT_NO)
         WRITE(MON_CT_UNIT_NO,REC=MON_CT_UNIT_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(globecom_year+get_BASE_YEAR()),
     +               CL_MONTH_NAME(ISEAS),
     +               UNIT_NAME,
     +               FLOAT(I),
     +               MONTHLY_REPORTING_CAPACITY,
     +               MONTHLY_CONTRACT_CAPACITY,
     +               MONTHLY_MINIMUM_CAPACITY,
     +               MONTHLY_ENRG,
     +               MONTHLY_CONTRACT_VARIABLE_COST,
     +               MONTHLY_CONTRACT_AVE_VAR_COST,
     +               MONTHLY_CNTR_MAX_FIXED_COST,
     +               MONTHLY_CNTR_MIN_FIXED_COST,
     +               MONTHLY_TOTAL_FIXED_COST,
     +               MONTHLY_TOTAL_CONTRACT_COST,
     +               MONTHLY_AVERAGE_TOTAL_COST,
     +               MONTHLY_CNTR_EMISSIONS
         MON_CT_UNIT_REC = MON_CT_UNIT_REC + 1
      ENDIF
      IF(ISEAS == LAST_SEASON .AND. ISEAS /= 1 .AND.
     +      CONTRACTS_IN_PERIOD .AND.
     +      ANNUAL_CONTRACT_REPORT_ACTIVE .AND.
     +                                       .NOT. TESTING_PLAN) THEN
         FIRST_MONTH = 100.*(get_BASE_YEAR()+YR_loc-1900) + 1
         LAST_MONTH = 100.*(get_BASE_YEAR()+YR_loc-1900) + 12
         DO I = 1 , NUMBER_OF_CONTRACTS
            IF(CNTR_ON_LI(I) > LAST_MONTH .OR.
     +                               CNTR_OFF_LI(I) < FIRST_MONTH) CYCLE
            IF( ANNUAL_CONTRACT_ENERGY(I) .LT. .5 .AND.
     +                       ANNUAL_CONTRACT_ENERGY(I) .GT. -.5) THEN
               ANNUAL_CONTRACT_ENERGY(I) = 0.0
               DIVIDE_BY_ENRG = 1E-15
               AVERAGE_TOTAL_COST = 0.
            ELSE
               DIVIDE_BY_ENRG = ANNUAL_CONTRACT_ENERGY(I)
               AVERAGE_TOTAL_COST =
     +                  (ANNUAL_CONTRACT_FIXED_COST(I) +
     +                  ANNUAL_CONTRACT_VARIABLE_COST(I))*1000000./
     +                  DIVIDE_BY_ENRG
            ENDIF
            UNIT_NAME = CNTRNM(I)
            MON_CT_UNIT_REC = rptrec(MON_CT_UNIT_NO)
            WRITE(MON_CT_UNIT_NO,REC=MON_CT_UNIT_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(globecom_year+get_BASE_YEAR()),
     +               CL_MONTH_NAME(LAST_SEASON+1),
     +               UNIT_NAME,
     +               FLOAT(I),
     +               N_A,
     +               ANNUAL_CONTRACT_CAPACITY(I),
     +               N_A,
     +                 ANNUAL_CONTRACT_ENERGY(I)/1000.,
     +                ANNUAL_CONTRACT_VARIABLE_COST(I),
     +                  ANNUAL_CONTRACT_VARIABLE_COST(I)*1000000./
     +                  DIVIDE_BY_ENRG,
     +                  N_A,
     +                  N_A,
     +               ANNUAL_CONTRACT_FIXED_COST(I),
     +                ANNUAL_CONTRACT_VARIABLE_COST(I) +
     +                                    ANNUAL_CONTRACT_FIXED_COST(I),
     +               AVERAGE_TOTAL_COST,
     +               ANNUAL_CONTRACT_SO2(I)
            MON_CT_UNIT_REC = MON_CT_UNIT_REC + 1
         ENDDO
!
         UNIT_NAME = 'Contract Total      '
         IF(ANNUAL_ENRG == 0.) THEN
            ANNUAL_CONTRACT_AVE_VAR_COST = 0.
            ANNUAL_AVERAGE_TOTAL_COST = 0.
         ELSE
            ANNUAL_CONTRACT_AVE_VAR_COST =
     +                   1000.*ANNUAL_CNTR_VARIABLE_COST/ANNUAL_ENRG
            ANNUAL_AVERAGE_TOTAL_COST =
     +                      1000.*ANNUAL_TOTAL_CONTRACT_COST/ANNUAL_ENRG
         ENDIF
!
         MON_CT_UNIT_REC = rptrec(MON_CT_UNIT_NO)
         WRITE(MON_CT_UNIT_NO,REC=MON_CT_UNIT_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(globecom_year+get_BASE_YEAR()),
     +               CL_MONTH_NAME(LAST_SEASON+1),
     +               UNIT_NAME,
     +               FLOAT(I),
     +               ANNUAL_REPORTING_CAPACITY,
     +               ANNUAL_CNTR_CAPACITY,
     +               ANNUAL_MINIMUM_CAPACITY,
     +               ANNUAL_ENRG,
     +               ANNUAL_CNTR_VARIABLE_COST,
     +               ANNUAL_CONTRACT_AVE_VAR_COST,
     +               ANNUAL_CNTR_MAX_FIXED_COST,
     +               ANNUAL_CNTR_MIN_FIXED_COST,
     +               ANNUAL_TOTAL_FIXED_COST,
     +               ANNUAL_TOTAL_CONTRACT_COST,
     +               ANNUAL_AVERAGE_TOTAL_COST,
     +               ANNUAL_CNTR_EMISSIONS
         MON_CT_UNIT_REC = MON_CT_UNIT_REC + 1
      ENDIF
! TODO: EXTRACT_METHOD handle_block_and_summary_report_items
      IF(((MONTHLY_BLOCK_REPORT .OR.
     +               MONTHLY_SUMMARY_REPORT_ACTIVE) .AND.
     +                    .NOT. TESTING_PLAN .AND. NBLOCK > 0) .OR.
     +    (CONTRACTS_IN_PERIOD .AND.
     +                    MONTHLY_CONTRACT_REPORT_ACTIVE .AND.
     +                                         .NOT. TESTING_PLAN)) THEN
         P_PUR_ENRG = P_PUR_ENRG * SEAS_HOURS
         PVARCOST = PVARCOST * SEAS_HOURS
         P_SALES_ENERGY = P_SALES_ENERGY * SEAS_HOURS
         TOTAL_PRODUCTION_COSTS = SNGL( (PFUELCST +
     +               P_NUCLEAR_FUEL_COST + PVARCOST +
     +               PFIXCOST + P_PUR_POWER_COST +
     +               MONTHLY_ECONOMY_COST -
     +               MONTHLY_ECONOMY_REVENUE-P_SALES_REVENUE))/1000000.
         IF(MONTHLY_HEAT > 0.0) THEN
            IF(PMMBTUS /= 0.D0) THEN
               TEMP_SNGL = 2000./SNGL(PMMBTUS)
            ELSE
               TEMP_SNGL = 1.
            ENDIF
            MON_CL_SUM_REC = rptrec(MON_CL_SUM_NO)
            WRITE(MON_CL_SUM_NO,REC=MON_CL_SUM_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(globecom_year+get_BASE_YEAR()),
     +               CL_MONTH_NAME(ISEAS),
     +               SNGL(DEMAND)/1000.,
     +               MONTHLY_ECONOMY_SOLD/1000.,
     +               SNGL((DEMAND + MONTHLY_ECONOMY_SOLD +
     +                        DYN_STORAGE_PUMP_ENRG -
     +                           DYN_STORAGE_GEN_ENRG -
     +                              PENRG - P_PUR_ENRG -
     +                                MONTHLY_ECONOMY_BOUGHT))/1000.,
     +               SNGL(PENRG)/1000.,
     +               SNGL(P_PUR_ENRG)/1000.,
     +               MONTHLY_ECONOMY_BOUGHT/1000.,
     +               SNGL(PMMBTUS),
     +               SNGL(PFUELCST)/1000000.,
     +               P_NUCLEAR_FUEL_COST/1000000.,
     +               SNGL(PVARCOST)/1000000.,
     +               SNGL(PFIXCOST)/1000000.,
     +               SNGL(P_PUR_POWER_COST)/1000000.,
     +               MONTHLY_ECONOMY_COST/1000000.,
     +               MONTHLY_ECONOMY_REVENUE/1000000.,
     +               TOTAL_PRODUCTION_COSTS,
     +               PEAK,BASE,
     +               (MONTHLY_TOTAL_EMISSIONS(I),
     +                      MONTHLY_TOTAL_EMISSIONS(I)*TEMP_SNGL,I=1,5),
     +               SNGL(DYN_STORAGE_PUMP_ENRG)/1000.,
     +               SNGL(DYN_STORAGE_GEN_ENRG)/1000.,
     +               SNGL(DEMAND + MONTHLY_ECONOMY_SOLD +
     +                        DYN_STORAGE_PUMP_ENRG)/1000.,
     +               SNGL(DYN_STORAGE_GEN_ENRG +
     +                              PENRG + P_PUR_ENRG +
     +                                MONTHLY_ECONOMY_BOUGHT)/1000.
            MON_CL_SUM_REC = MON_CL_SUM_REC + 1
         ELSE
            MON_CL_SUM_REC = rptrec(MON_CL_SUM_NO)
            WRITE(MON_CL_SUM_NO,REC=MON_CL_SUM_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(globecom_year+get_BASE_YEAR()),
     +               CL_MONTH_NAME(ISEAS),
     +               SNGL(DEMAND)/1000.,
     +               MONTHLY_ECONOMY_SOLD/1000.,
     +               SNGL((DEMAND + MONTHLY_ECONOMY_SOLD +
     +                        DYN_STORAGE_PUMP_ENRG -
     +                           DYN_STORAGE_GEN_ENRG -
     +                              PENRG - P_PUR_ENRG -
     +                                MONTHLY_ECONOMY_BOUGHT))/1000.,
     +               SNGL(PENRG)/1000.,
     +               SNGL(P_PUR_ENRG)/1000.,
     +               MONTHLY_ECONOMY_BOUGHT/1000.,
     +               SNGL(PMMBTUS),
     +               SNGL(PFUELCST)/1000000.,
     +               P_NUCLEAR_FUEL_COST/1000000.,
     +               SNGL(PVARCOST)/1000000.,
     +               SNGL(PFIXCOST)/1000000.,
     +               SNGL(P_PUR_POWER_COST)/1000000.,
     +               MONTHLY_ECONOMY_COST/1000000.,
     +               MONTHLY_ECONOMY_REVENUE/1000000.,
     +               TOTAL_PRODUCTION_COSTS,
     +               PEAK,BASE,
     +               (MONTHLY_TOTAL_EMISSIONS(I),N_A,I=1,5),
     +               SNGL(DYN_STORAGE_PUMP_ENRG)/1000.,
     +               SNGL(DYN_STORAGE_GEN_ENRG)/1000.,
     +               SNGL(DEMAND + MONTHLY_ECONOMY_SOLD +
     +                        DYN_STORAGE_PUMP_ENRG)/1000.,
     +               SNGL(DYN_STORAGE_GEN_ENRG +
     +                              PENRG + P_PUR_ENRG +
     +                                MONTHLY_ECONOMY_BOUGHT)/1000.
            MON_CL_SUM_REC = MON_CL_SUM_REC + 1
         ENDIF
      ENDIF
! TODO: EXTRACT_METHOD handle_block_and_summary_report_items (end)
 1000 FORMAT('&',F9.2,I10,I8,F9.2,F7.2,F8.2,2F9.1,3F8.1)
 1001 FORMAT('&',F9.2,I10,F8.3,F9.2,F7.2,F8.2,2F9.1,3F8.1)
 1002 FORMAT(1X,I3,1X,A20,3F7.1,F9.2,F7.2,F7.2,3F6.2,F8.2,F8.2,3X,F8.1)
 1003 FORMAT(/1X,A,F11.1)
 1005 FORMAT(1X,I3,1X,A20,3F7.1,23X,3F6.2,F8.2)
 1010 FORMAT(1X,A,F11.1)
 1011 FORMAT(1X,'mmBTUs                ',I11)
 1012 FORMAT(1X,'GigaJoules            ',I11)
 1020 FORMAT('&',2X,A,F11.2)
 1030 FORMAT('&',F11.1,'  (lbs/mmBTU)  ',F6.2)
 1031 FORMAT('&',F11.1,'  (kg/GJ)      ',F6.2)
 1100 FORMAT(4X,A,A,I4,A,I3,5X,A/)
      RETURN

      ENTRY GET_PROD_BY_MK_BY_MWH(R_MK,RETURN_PROD_BY_MK_BY_MWH)

         RETURN_PROD_BY_MK_BY_MWH(1) = PROD_BY_MK_BY_MWH(R_MK,1)
         RETURN_PROD_BY_MK_BY_MWH(2) = PROD_BY_MK_BY_MWH(R_MK,2)
         RETURN_PROD_BY_MK_BY_MWH(3) = PROD_BY_MK_BY_MWH(R_MK,3)
      RETURN

      ENTRY GET_NG_BY_REGION_BY_MMBTU(R_SP,R_NG_BTUS_BY_GSP_BY_FUEL)

!
! FOR NG MODEL.
! 1 = MONTH BTU
! 2 = PEAK DAY BTU
! 3 = GENERATING UNITS WITH BTU
!
         R_NG_BTUS_BY_GSP_BY_FUEL(1) = FUEL_BTUS_BY_GSP_BY_FUEL(R_SP,1)
         R_NG_BTUS_BY_GSP_BY_FUEL(2) = FUEL_BTUS_BY_GSP_BY_FUEL(R_SP,2)
         R_NG_BTUS_BY_GSP_BY_FUEL(3) = FUEL_BTUS_BY_GSP_BY_FUEL(R_SP,3)
      RETURN

      ENTRY GET_ANNUAL_NG_BY_REGION_BY_MMBTU(R_SP,
     +                                         R_NG_BTUS_BY_GSP_BY_FUEL)

!
! FOR NG MODEL.
! 1 = MONTH BTU
! 2 = PEAK DAY BTU
! 3 = GENERATING UNITS WITH BTU
!
         R_NG_BTUS_BY_GSP_BY_FUEL(1) =
     +                           ANNUAL_FUEL_BTUS_BY_GSP_BY_FUEL(R_SP,1)
         R_NG_BTUS_BY_GSP_BY_FUEL(2) =
     +                           ANNUAL_FUEL_BTUS_BY_GSP_BY_FUEL(R_SP,2)
         R_NG_BTUS_BY_GSP_BY_FUEL(3) =
     +                           ANNUAL_FUEL_BTUS_BY_GSP_BY_FUEL(R_SP,3)
      RETURN

      ENTRY GET_ANNUAL_PROD_BY_MK_BY_MWH(R_MK,RETURN_PROD_BY_MK_BY_MWH)

         RETURN_PROD_BY_MK_BY_MWH(1) = ANNUAL_PROD_BY_MK_BY_MWH(R_MK,1)
         RETURN_PROD_BY_MK_BY_MWH(2) = ANNUAL_PROD_BY_MK_BY_MWH(R_MK,2)
         RETURN_PROD_BY_MK_BY_MWH(3) = ANNUAL_PROD_BY_MK_BY_MWH(R_MK,3)
      RETURN

      ENTRY INIT_TRANS_C_MONTH_START_UPS()

!
         MONTH_UNIT_STARTS(1:MAX_CL_UNITS) = 0
         MONTH_UNIT_START_COSTS(1:MAX_CL_UNITS) = 0.
!
      RETURN

      ENTRY PUT_TRANS_C_START_UPS(
     +                        R_UNIT,
     +                        R_STARTS,
     +                        R_START_COSTS)

!
         MONTH_UNIT_STARTS(R_UNIT) = R_STARTS
         MONTH_UNIT_START_COSTS(R_UNIT) = R_START_COSTS
!
      RETURN

      ENTRY CL_REPORT_WHOLE_MARKET_COST( ! CALLED MONTHLY
     +                               R_MONTH,
     +                               R_MONTHLY_WHOLESALE_COST)

         R_MONTHLY_WHOLESALE_COST = 1000000.*
     +                     (MONTHLY_WHOLE_COSTS -
     +                               MONTHLY_DERIV_WHOLESALE_COST)
      RETURN

      ENTRY CL_EMIS_TO_WVPA_REPORT( ! CALLED MONTHLY BY UNIT
     +                             R_UNIT,R_MONTHLY_UNIT_EMISSIONS_COST)

         R_MONTHLY_UNIT_EMISSIONS_COST = 1000.*
     +                          (MONTHLY_UNIT_EMISSIONS_COST(1,R_UNIT) +
     +                           MONTHLY_UNIT_EMISSIONS_COST(2,R_UNIT) +
     +                           MONTHLY_UNIT_EMISSIONS_COST(3,R_UNIT) +
     +                           MONTHLY_UNIT_EMISSIONS_COST(4,R_UNIT) +
     +                           MONTHLY_UNIT_EMISSIONS_COST(5,R_UNIT) )
      RETURN

      ENTRY CL_REPORT_TO_WVPA_REPORT( ! CALLED MONTHLY
     +                               R_MONTH,
     +                               R_WHOLESALE_MARKET_COST,
     +                               R_WHOLESALE_MARKET_BUY_MWH,
     +                               R_MONTHLY_WHOLESALE_COST,
     +                               R_MONTHLY_WHOLESALE_FUEL_COST,
     +                               R_MONTHLY_WHOLESALE_VOM_COST,
     +                               R_WHOLESALE_MARKET_REV,
     +                               R_WHOLESALE_MARKET_SELL_MWH)

!
         R_WHOLESALE_MARKET_COST = MONTHLY_ECO_PUCH/1000.
         R_WHOLESALE_MARKET_BUY_MWH = MONTHLY_ECO_PUCH_ENRG ! MWH
!
         R_MONTHLY_WHOLESALE_COST = (MONTHLY_WHOLE_COSTS -
     +                               MONTHLY_DERIV_WHOLESALE_COST)*1000.
         R_MONTHLY_WHOLESALE_FUEL_COST =
     +                           MONTHLY_WHOLESALE_COST(R_MONTH,2)/1000.
         R_MONTHLY_WHOLESALE_VOM_COST =
     +                           MONTHLY_WHOLESALE_COST(R_MONTH,3)/1000.
         R_WHOLESALE_MARKET_REV = (MONTHLY_ECO_SALES -
     +                                    MONTHLY_DERIV_ECO_SALES)/1000.
         R_WHOLESALE_MARKET_SELL_MWH = MONTHLY_ECO_SALES_ENRG -
     +                                 MONTHLY_DERIV_ECO_SALES_ENRG! MWH
!
      RETURN

      ENTRY TRANSACT_WHOLESALE_REV_COST(
     + R_DETAILED_TRANSFER_PRICING, ! L1
     + R_AC_REV_VECTOR, ! I2
     + R_AC_REV_ALLOC_VECTOR, !I2
     + R_AC_COST_VECTOR, ! I2
     + R_AC_COST_ALLOC_VECTOR, !I2
     + R_WHOLESALE_MARKET_COST_AC, ! R(0:12) USE THIS FOR AMEREN
     + R_WHOLESALE_MARKET_BUY_MWH_AC,
     + R_MONTHLY_WHOLESALE_COST_AC,
     + R_WHOLESALE_MARKET_REV_AC, ! R(0:12) USE THIS FOR AMEREN
     + R_WHOLESALE_MARKET_SELL_MWH_AC,
     + R_MONTHLY_WHOLE_FUEL_COST_AC, ! R(0:12) USE THIS FOR AMEREN
     + R_MONTHLY_WHOLE_VOM_COST_AC)


!

!
         R_DETAILED_TRANSFER_PRICING =
     +            YES_DETAILED_TRANSFER_PRICING() .AND.
     +                RUN_TRANSACT .AND.
     +                   .NOT. YES_RUN_MULTIAREA_TRANSACT() .AND.
     +                                 TRANSFER_PRICING_VECTORS(
     +                                       R_AC_REV_VECTOR,
     +                                       R_AC_REV_ALLOC_VECTOR,
     +                                       R_AC_COST_VECTOR,
     +                                       R_AC_COST_ALLOC_VECTOR)
!
         DO I = 0, 12
!
            R_WHOLESALE_MARKET_COST_AC(I) = WHOLESALE_MARKET_COST(I)/
     +                                                             1000.
            R_WHOLESALE_MARKET_BUY_MWH_AC(I) =
     +                                 WHOLESALE_MARKET_BUY_MWH(I)
!
            R_MONTHLY_WHOLESALE_COST_AC(I) =
     +                                 MONTHLY_WHOLESALE_COST(I,1)/1000.
!
            R_WHOLESALE_MARKET_REV_AC(I) = WHOLESALE_MARKET_REV(I)/1000.
            R_WHOLESALE_MARKET_SELL_MWH_AC(I) =
     +                                WHOLESALE_MARKET_SELL_MWH(I)
            R_MONTHLY_WHOLE_FUEL_COST_AC(I) =
     +                                 MONTHLY_WHOLESALE_COST(I,2)/1000.
            R_MONTHLY_WHOLE_VOM_COST_AC(I) =
     +                                 MONTHLY_WHOLESALE_COST(I,3)/1000.

         ENDDO

      RETURN

      ENTRY WVPA_WHOLESALE_REV_COST_TRAMS(R_MONTHLY_WHOLESALE_COST_AC,
     +     R_MONTHLY_WHOLE_FUEL_COST_AC, ! R(0:12) USE THIS FOR AMEREN
     +     R_MONTHLY_WHOLE_VOM_COST_AC,
     +    R_WHOLESALE_MARKET_REV_AC) ! R(0:12) USE THIS FOR AMEREN

!
         DO I = 0, 12
!
            R_MONTHLY_WHOLESALE_COST_AC(I) =
     +                                 MONTHLY_WHOLESALE_COST(I,1)/1000.
            R_MONTHLY_WHOLE_FUEL_COST_AC(I) =
     +                                 MONTHLY_WHOLESALE_COST(I,2)/1000.
            R_MONTHLY_WHOLE_VOM_COST_AC(I) =
     +                                 MONTHLY_WHOLESALE_COST(I,3)/1000.
            R_WHOLESALE_MARKET_REV_AC(I) = WHOLESALE_MARKET_REV(I)/1000.
         ENDDO
!
      RETURN

      ENTRY GET_TRANSACT_VARIABLES(R_TRANS_GROSS_MARGIN)

         R_TRANS_GROSS_MARGIN = TRANS_TOTAL_GROSS_MARGIN
      RETURN
      END SUBROUTINE PERIOD_CL_REPORT
