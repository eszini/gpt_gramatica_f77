! Function taken from Cla_objt.for
FUNCTION CL_UNITS_READ()
      use unitnm_trace
      use program_state
      use cl_object_shared
      use nunits_shared
      use sf_objt_interfaces
      use cl_units_read_data
      use aitvaprod
      use annl_nuc
      use annual_cl_unit
      use cla_decs
      use cla_objt_arrays
      use conversion_routines
      use dsex_obj_interfaces
      use eco
      use end_routine
      use enrg_helper
      use expansion
      use filename_tracker
      use financial_switches_common
      use forecast
      use foshydi2com
      use fuelused_decs
      use grx_planning_routines
      use grxmodules
      use hesi
      use internal_operation_switches
      use irec_endpoint_control
      use kepcocom
      use lh_global_variables
      use logging
      use miscmod
      use mod_base_year
      use monthly_icap
      use mwunih
      use namescom
      use p_fuel_annl
      use pp_objt_interfaces
      use prim_mover_idx
      use prod2com
      use prod_arrays_dimensions
      use prodcom
      use readenergyproductsdata
      use rptreccontrol
      use sizecom
      use spindriftlib
      use trancom
      use trans_group_variables
      use wvpartrk_interfaces
      use up_objt_decs
      use tf_decs
      use tf_objt_interfaces
      use cl_maint
      use cl_units_read_data
      use cl_units_read_decs
      use nunits_shared
	  use spcapexvariables
	  use cl_screen_data_mod
	  use grxmodules, only: HWRetrofitProjectThisYear
	  use mthnmcom
      use nunits_shared
      use capacity_arrays
      use debugtrace
      use energy_trace
      use offline_trace
      use program_state
      use sic_trace
      implicit none
      integer :: file_trace_clm=0
	  integer :: file_trace_gcrk=0 ! get_cm_retirement_kw
      integer :: file_trace_cacm=0
      integer :: file_trace_clax=0
      logical (kind=1) :: in_atma
      real :: MW_INSIDE_FENCE_ord
      integer (kind=2) :: bcu ! For debugger
      integer (kind=2) :: dbg_offline_month, dbg_online_month, &
        dbg_online_year
      real :: dbg_vcpmwh_in
      integer (kind=2) :: dbgThisCapMktPtr
      
      integer (kind=2) :: GET_PA_FROM_TG !External
      logical (kind=1) ::REGION_OR_STATE_POWER_MAP, &
                CLA_UNIT_UP_YESTERDAY(MAX_CL_UNITS)

      
      real :: GET_DISPADJ_SAVE ! External
      real :: GET_PBTUCT_SAVE ! External
      INTEGER*2 PT_YR,YEAR_START,YEAR_END,UNIT_OFF_YEAR
      CHARACTER*20 R_NAME
      INTEGER*2 R_NBLOK2
        INTEGER*2 R_EMISS_TYPE
      REAL R_MONTHLY_OWNED_NF(0:12), &
           R_MONTHLY_LEASED_NF(0:12), &
           R_MONTHLY_DOE_DISPOSAL(0:12), &
           R_MONTHLY_DOE_DECOMMISSIONING(0:12)

      real (kind=4) :: TRANS_DISPATCH_EMIS_ADDER ! External
      integer (kind=2) :: MARKET_AREA_LOOKUP !External
      REAL CL_CAPACITY_PLANNING_adj  ! External

      integer (kind=2) :: WVPA_TRACKING_TYPE
      ! External
      real (kind=4) :: ESCALATED_MONTHLY_VALUE
        REAL*4 CAP_MARKET_RATE, &
                 PLAN_FACTOR, &
                 PRIMARY_FUEL_ID, &
                 CAPACITY_MARKET_COIN_ADJ_FCT(MAX_CL_UNITS)
      CHARACTER (LEN=2) :: GREEN_MRX_METHOD


      REAL (KIND=4) :: RETRO_CAP_CO2_MULT
      character(len=6) :: R_EIA_PLANT_CODE
      integer (kind=2) :: CL_UNITS_READ, I
       

               
      INTEGER IOS,IRAN32,IRAN32_OUT,CO2_ABATEMENT_REC
      real :: R_A_CO,R_B_CO,R_C_CO,R_D_CO
      integer (kind=2) :: r_id
      real :: R_GROSS_MARGIN
      real :: R_EMERGENCY_HEATRATE
      INTEGER(kind=8)  :: TEMP_I8,R_I8_ID
      integer (kind=2) :: r_unitno, r_year
      
              INTEGER*8   &
                  TRANS_BUS_ID(MAX_CL_UNITS), &
                  THERMAL_PARENT_ID
                  
        integer (kind=2) :: R_NUMBER_OF_RESOURCE_ID
        REAL*4 R_INTRA_BASE_REVENUES, &
             R_INTRA_ADJ_REVENUES, &
             R_INTRA_SALES_REVENUES, &
             R_INTRA_OTHER_REVENUES, &
             R_INTRA_COMPANY_NF_BURN, &
             R_WVPA_EMISSIONS_EXPENSE, &
             R_CAPACITY_SALES_TO_LEVEL_RM, &
             R_CAPACITY_PURCHASES_TO_LEVEL_RM, &
             RETIREMENTS_PROFIT_PER_KW, &
             R1_MONTHLY_TRANSFER_REV, &
             R1_MONTHLY_TRANSFER_COST, &
             R2_MONTHLY_TRANSFER_REV, &
             R2_MONTHLY_TRANSFER_COST, &
             UNDER_CONSTRUCTION_PERCENT, &
             ADVANCED_DEVELOPMENT_PERCENT, &
             EARLY_DEVELOPMENT_PERCENT, &
             PROPOSED_PERCENT, &
             INDEFIN_POSTPHONED_PERCENT
      LOGICAL*1 R_CLASS_EXISTS
      real (kind=4) :: R_BAY_SHORE_FUEL_EXPENSE
      REAL*4 MMBTU_ENERGY,MWH_ENERGY, &
             R_DOE_NUC_FUEL_FEE,DOE_NF_DISPOSAL_COST, &
             R_NUC_DECOMMISSIONING_COST,NUCLEAR_DECOMMISSIONING_COST, &
             R_DOE_R300_DISPOSAL_COST,DOE_R300_DISPOSAL_COST, &
             NUC_MAINTENANCE(12), &
             R_CL_ANN_CLASS_CAPACITY(4), &
             R_CL_ANN_CLASS_ENERGY(4), &
             R_CL_ANN_TOTAL_CLASS_CAPACITY, &
             R_CL_ANN_TOTAL_CLASS_ENERGY, &
             R_NUC_ENERGY, &
             R_STEAM_ENERGY, &
             R_PURCHASES_ENERGY, &
             R_ASSET_CLASS_900, &
             R_WHOLESALE_FUEL_EXPENSE, &
             R_WHOLESALE_VOM_EXPENSE, &
             R_ICAP_REVENUES
      LOGICAL*1 ECITY_OBJ_REPORT, &
                ECITY_OBJ_REPORT_NOT_OPEN/.TRUE./
      INTEGER*2 ECITY_VARIABLE_NUMBER, &
                ECITY_OBJ_UNIT, &
                ECITY_OBJ_HEADER
      INTEGER   ECITY_OBJ_REC
      CHARACTER*9 CL_MONTH_NAME(14) &
                               /'January  ','February ', &
                                'March    ','April    ', &
                                'May      ','June     ', &
                                'July     ','August   ', &
                                'September','October  ', &
                                'November ','December ', &
                                'Annual   ','Fiscal Yr'/
      REAL R_CLASS_CL_EMISSIONS(NUMBER_OF_EMISSION_TYPES), &
           R_MONTH_AC_WHOLESALE_PROD_COST(0:12), &
           R_MONTH_AC_ECITY_VAR_PROD_COST(0:12), &
           R_MONTH_AC_ECITY_NEW_FIX_COST(0:12), &
           R_WTB(0:12), &
           R_MONTH_AC_ECITY_SUPP(0:12), &
           R_MONTH_AC_ECITY_REVENUE(0:12), &
           R_MONTH_AC_ECITY_FEES(0:12), &
           R_ECITIES_WHOLESALE_PROD_COST, &
           R_ECITIES_VAR_PROD_COST, &
           R_ECITIES_NEW_FIXED_COST, &
           R_ECITIES_MARKET_ENERGY_SALES, &
           R_ECITIES_TRANSMISSION_FEES, &
           TOTAL_DERIV_REV, &
           TOTAL_DERIV_EXP, &
           WHOLESALE_PRODUCTION_COST, &
           TOTAL_VARIABLE_COST, &
           AVERAGE_PRODUCTION_COSTS
      INTEGER*2 RETURN_NUM_CAP_LIMITED_CLASSES, &
                R_MAX_CAP_LIMITED_CLASS_NUM, &
                RETURN_CAP_LIMITED_POINTER, &
                R_CLASS,CLASS
      logical (kind=1) :: ECITIES=.FALSE.,YES_ECITIES_UNITS_ACTIVE, &
                  ECITY_COMPANY, &
                  POWER_DERIV_REV_EXP_BY_CLASS
      LOGICAL*1 RETURN_ASSET_CLASS_LISTS, &
                OLD_ADJ_LOGIC/.TRUE./
                
      real (kind=4) :: R_EMIS(5)
      
            REAL R_NUC_FUEL_LEASED, &
           R_NUC_FUEL_OWNED, &
           R_NUC_FUEL_OWNED_BURN, &
           R_NUC_FUEL_LEASED_BURN, &
           R_NF_BURN_IN_RATEBASE, &
           R_PURCHASE_POWER_EXPENSE, &
           R_FUEL_COST, &
           R_VOM_COST, &
           R_VARIABLE_EXPENSE, &
           R_FIXED_EXPENSE, &
           R_EXPENSE_COLLECTED_ADJ_CLAUSE, &
           R_EXPENSE_COLLECTED_BASE_RATES, &
           R_NOT_COLLECTED_IN_RATES, &
           R_TOTAL_SALES_REVENUE, &
           R_SALES_REVENUE_NOT_IN_RATES, &
           R_BTL_SALES_REVENUE, &
           R_BTL_EXPENSES
      
            integer (kind=2) :: R_YR, &
        RETIRED_UNITS, &
                          CO2_MARKET_PTS,ABATE_RETROFIT_COUNTER, &
                          RETROFIT_PROJECT_YEAR,K

       REAL*4 R_INTRA_FOSSIL_FUEL_EXPENSES, &
             R_INTRA_LEASED_NUCLEAR_FUEL, &
             R_INTRA_OWNED_NUC_FUEL_EXPENSES, &
             R_INTRA_NUC_OWN_BURN, &
             R_INTRA_NUC_LEASE_BURN, &
             R_INTRA_PURCHASE_EXPENSES, &
             R_FIXED_COST, &
             ANNUAL_MARGIN_PER_KW,MINIMUM_MARGIN_PER_KW/0./, &
             CO2_K, &
             RETIREMENTS_CUM_CO2(MAX_CL_UNITS), &
             RETROFIT_CUM_CO2(MAX_CL_UNITS), &
             CM_RETIREMENT_KW(0:1000), &
             RETIREMENTS_UPLIFT_PER_TON(MAX_CL_UNITS), &
             RETROFIT_UPLIFT_PER_TON(MAX_CL_UNITS),  &
             ANNUAL_CAPITAL_COST, &
             MONTHLY_EAVAIL(12), &
             MONTHLY_NUCLEAR_AVAIL_MULT(12), &
             REGIONAL_MONTHLY_MULT(12), &
             MONTHLY_COAL_AVAIL_MULT(12), &
             MONTHLY_GAS_AVAIL_MULT(12), &
             MONTHLY_OIL_AVAIL_MULT(12), &
             MONTHLY_OTHER_AVAIL_MULT(12), &
             MONTHLY_HYDRO_WATER_YEAR_MULT(12), &
             TT_PERCENT, &
             INTER_BLOCKS(3,MAX_CL_UNITS), &
             PERCENT_INSIDE_FENCE, &
             MONTHLY_MAINTENANCE_PEAKS(12), &
             MAINTENANCE_PENALTY(12), &
             LOCAL_MW(2), &
             CO2_PRICE, &
             CUM_CO2_EMISSIONS, &
             RETROFIT_CO2, &
             CUM_MW, &
             CUM_MWH, &
             RETIREMENT_COST_PER_KWMO, &
             LOCAL_RPS_CONTRIB_PERCENT

      REAL (kind=8) :: R_HEAT
      integer (kind=2) :: R_FUEL_DELIVERY_1_ord, &
                          R_FUEL_DELIVERY_2_ord, &
                          R_FUEL_DELIVERY_3_ord

      character (len=1)  :: R_MARKET_RESOURCE_STR
      LOGICAL (KIND=1) :: ANNUAL_CALL_PUT_CAPACITY
      INTEGER (KIND=2) :: GET_MARKET_PRICE_YEAR
      CHARACTER*2 LOAD_FILE_CHAR_EXT
      CHARACTER*3 GET_HOURLY_PRICE_NAME
      CHARACTER*5 MARKET_PRICE_NAME
      
      CHARACTER*20 RETURN_UNITNM,MONTH_NAME,LOCAL_NAME
      CHARACTER*4 YEAR_CHR
      CHARACTER*5 NUNITS_CHR
      character (len=256) :: file_name=""
      CHARACTER*256 PRB_FILE_DIRECTORY,OUTPUT_DIRECTORY
      logical (kind=1) :: R_AREA_PRICE, &
                TEMP_L
      integer (kind=2) :: MAINTENANCE_HEADER
      logical (kind=1) :: PICK_FOR_SEED_OPTIONS
      logical (kind=1) :: YES_UNIT_COMMITMENT_LOGIC
      LOGICAL (kind=1) ::   MAINTENANCE_REPORT_NOT_OPEN/.TRUE./, &
                  MAINTENANCE_REPORT
      LOGICAL*1 ACCUMULATE_MAINTENANCE,MAINTENANCE_IS_ACCUMULATED
      INTEGER R_CAP_TYPE
      integer (kind=2) :: R_FT,R_TG, R_NG
      integer (kind=2) :: R_ST_P
      INTEGER*2 UNIT_NO,R_UNIT_NO
      integer (kind=2) :: R_ALLOCATION_VECTOR, &
                R_CL_RESOURCE_ID
        integer (kind=2) :: UNIT_ON_LINE_YEAR, &
                    OPERATION_LIFE, &
                    POINTER
        integer (kind=2) :: R_DAY_TYPE, &
                R_TRANS_GROUP, &
                R_DATA_BASE, &
                R_TRANSACTION_GROUP
      integer (kind=2) ::  R_ON_LINE_MONTH
      
      integer (kind=2) :: R_ASSET_CLASS
      logical (kind=1) :: PUT_RPS_ENRG_CAP
      character (len=6) :: R_MARKET_ID
      real (kind=4) :: R_CAP
      integer (kind=2) :: R_ISEAS
      integer (kind=2) :: RR_CO2_NUNIT, &
                R_CO2_RETIRE_OR_RETRO, &
                R_PM,R_ST_TG
      real :: R_CO2_UNIT_CO2_AFTER, &
           R_RPS_CONTRIB
      integer (kind=2) :: R_ST
      real (kind=4) :: R_ENRG
      integer (kind=2) :: R_CO2_NUNIT, R_CO2_TG
      
      real :: R_CO2_UNIT_MW, &
           R_CO2_UNIT_MWH, &
           R_CO2_UNIT_CO2, &
           R_CO2_UNIT_PRICE, &
           R_CO2_STRIKE_PRICE, &
           R_CO2_UNIT_MW_AFTER, &
           R_CO2_STRIKE_PRICE_AFTER
           
      real (kind=4) :: R_CO2_REDUCTION

      integer (kind=2) :: R_CO2_COUNTER, &
                R_RETIRE_OR_RETRO
                
      CHARACTER (LEN=2) :: CAPACITY_PLANNING_METHOD
      integer (kind=2) :: GET_TRANS_GROUP_POSITION, &
                NUMBER_OF_RPS_PROGRAMS, &
                CO2_ABATEMENT_RPT_HEADER
      logical (kind=1) :: CO2_ABATEMENT_REPORT_ACTIVE, &
                CO2_ABATEMENT_REPORT_NOT_OPEN/.TRUE./, &
                RETROFIT_LOGIC_ACTIVE, &
                ASCENDING, &
                R_CO2_END_LIST
                
      real (kind=4) :: R_CO2, &
            R_MWH, &
             R_CO2_COST, &
             R_CAP_COST
             
      integer(kind=2) :: R_CM
      real (kind=4) :: R_MW
      real :: START_UP_COST_ESCALAT_MTHLY_VAL, &
           RET_VAL_FROM_ESCALATION_VECTOR
      character (len=1) :: R_START_UP_TYPE
      logical (kind=1) :: strategic_rtrmts_logic=.FALSE., &
                  SCHEDULE_FO, &
                  DETAILED_MAINT, &
                  PRICE_ONLY_WHOLESALE_REV=.FALSE., &
                  APPLY_TRANS_REV_TO_WHOLESALE, &
                  DETAILED_TRANSFER_PRICING=.FALSE., &
                  R_CO2_FIRST_CONTROL_MONTH, &
                  R_CO2_FIRST_CONTROL_YEAR, &
                  R_CO2_YEAR_BEFORE_CONTROL, &
                  FIRST_ENERGY=.FALSE., &
                  NEW_BUILD_ADDITIONS_ACTIVE, &
                  KEEP_UNIT, &
                  CapEx_Running, &
                  WVPA
                  
      real :: R_NOX_CONTROL_PERCENT, &
           R_SOX_CONTROL_PERCENT, &
           R_CO2_CONTROL_PERCENT, &
           R_HG_CONTROL_PERCENT, &
           R_OTHER3_CONTROL_PERCENT
           
      logical (kind=1) :: R_CALCULATE_NOX
      integer (kind=2) :: R_I,R_J, R_MONTH, R_DATE
      logical (kind=1) :: R_LOGICAL ! Entry argument
      character (len=6) :: R_STATE_PROVINCE ! ENTRY argument
      real (kind=4) :: LATITUDE(MAX_CL_UNITS)=0, &
        LONGITUDE(MAX_CL_UNITS)=0
      real (kind=4) :: MARKET_FLOOR(MAX_CL_UNITS)=0, &
        MARKET_CEILING(MAX_CL_UNITS)=0
      CHARACTER*20 :: CAPACITY_MARKET_COST_ASSIGN(MAX_CL_UNITS)=" "
      integer (kind=2) :: r_nunits_loc ! For passing to ENTRY routines.
      INTEGER (KIND=2) :: STATE_ID_LOOKUP ! External
      integer (kind=2) :: CAP_MARKET_MONTH_NO_INDEX !External
      ! Externals
      integer (kind=2) :: GET_BELONGS_TO_GROUP, &
        GET_MAX_TRANS_GROUPS, GET_CM_FROM_TG, &
                GET_RPS_PROGRAM_POSITION, &
                GET_TRANS_RPS_PROG_NUMBER, &
                GET_PURCHASE_ASSET_CLASS_ID, &
                get_num_save_active_transact
      character (len=1) :: IGNORE_NON_UTILITY ! External
      character (len=1024) :: RECLN
      
      logical (kind=1) :: GET_STRATEGIC_RETIRMENTS_LOGIC, &
                  YES_FREQUENCY_DURATION, &
                  FREQUENCY_DURATION, &
                  UNIT_FREQUENCY_DURATION=.FALSE., &
                  YES_DETAILED_FOR, &
                  YES_DETAILED_MAINTENANCE, &
                  GET_MONTHLY_MAINTENANCE_PENALTY, &
                  YES_REGIONAL_OUTAGES_ACTIVE, &
                  REGIONAL_PARAMS_ACTIVE, &
                  YES_TURN_OFF_POLY,TURN_OFF_POLY, &
                  YES_NEW_BUILD_ADDITIONS_ACTIVE, &
                  AOD_THERMAL_UNIT_VALUE_LIST, &
                  YES_AOD_PRICE_THERM_VALUE_LIST, &
                  YES_AOD_COMP_THERM_VALUE_LIST, &
                  AOD_THERMAL_LIST_COMPARE, &
                  CO2_RETROFIT_LOGIC_ACTIVE, &
                  GET_NEW_BUILD_PERCENTS
                  
      integer (kind=2) :: GET_MAX_DAY_TYPES,GET_LAST_DAY_TYPE
      
      
      integer (kind=2) :: GET_NUMBER_OF_PLANNING_GROUPS, &
        GET_NUMBER_OF_CAPACITY_MARKETS
      logical (kind=1) :: YES_POWERWORLD_REPORT
      integer (kind=4) :: UPPER_TRANS_Gp
      
      real (kind=4) :: GET_NEW_UNIT_RETIRE_VALUE_BY_CM !Entry
      logical (kind=1) :: CALC_MRX_RPS_CURVES, &
        RETROFIT_PROJECT_ACTIVE ! Entry
      INTEGER (KIND=2) :: RETURN_MONTHLY_CL_EXPENSES ! Entry
      INTEGER*2 NUMBER_OF_CL_UNITS
      LOGICAL*1 NUC_FUEL_PRICES_FROM_FUEL_FILE(MAX_CL_UNITS), &
                NUC_FUEL_PRICE_SOURCE_IS_NFILE, &
                TF_FILE_EXISTS,TG_FILE_EXISTS
      INTEGER :: GET_HESI_UNIT_ID_NUM_I4 !Entry
      LOGICAL*1 CLA_SPECIAL_ID_NAME ! Entry

      INTEGER*4 UNIQUE_REPORT_VALUE_FOR_CL_UNIT
      real(kind=4) ::  R_FACET_MAINTENANCE_RATE(*)
      REAL (kind=8) :: R_MMBTU_FUEL_BALANCE(*)
      integer (kind=2) :: R_FUEL_INVENTORY_ID(0:*)
      real (kind=4) ::  R_PNL_CL_ANN_TOTAL_CLASS_ENERGY(*), &
        R_PNL_CL_ANN_CLASS_CAPACITY(*)

      real :: R_MAINTENANCE_RATE(*)
      real :: MAINTENANCE_RATE(*)
      integer (kind=2) :: R_ASSET_VECTOR(*)
      LOGICAL*1 GET_BLOCK_CAP_FACTORS !Entry
      INTEGER*2 R_ASSET_CLASS_NUM(*), &
           GET_ASSET_CLASS_NUM
      REAL*4   EL_BASE_REVENUE_RATE,EL_PEAK_REVENUE_RATE, &
               R_NUC_UNIT_FUEL_ADDER_COST
      REAL (kind=4) :: MONTH_VARS(0:12,1:*)
      real (kind=4) :: R_LOWER_BOUND_CAP_FACTOR(*), &
        R_UPPER_BOUND_CAP_FACTOR(*)
      REAL*4 R_PNL_FUEL_COST(*), &
             R_PNL_PURCHASE_POWER_EXPENSE(*), &
             R_PNL_VARIABLE_EXPENSE(*), &
             R_PNL_FIXED_EXPENSE(*), &
             R_PNL_TOTAL_SALES_REVENUE(*)
! END RESOURCE VARIABLE.

      REAL GET_CL_EMISSIONS,GET_CL_EMISS_FOR_CLASS
      INTEGER*2 RETURN_MONTHLY_CL_CASH_EXPENSES
      INTEGER*2 RETURN_MONTHLY_CL_REVENUES
      INTEGER*2 RETURN_MONTHLY_CL_CASH_REVENUES
      REAL (kind=4) ::   GET_POWERDAT_PLANT_ID, &
               R_HESI_SECOND_UNIT_ID_NUM
      INTEGER*2 RDI_SIZE_INDEX,RDI_FUEL_INDEX,GET_PRIMARY_MOVER
      INTEGER*2 CL_EXPENSES_2_ASSET_CLASSES, &
                MON_MDS_CL_EXP_2_AC, &
                RETURN_CL_ASSET_CLASS_EXPENSES, &
                RETURN_NUC_CL_ASSET_CLASS_EXPENSES, &
                RETURN_MONTHLY_NF_OP_EXPENSES, &
                SET_UP_CL_CLASS_ARRAYS, &
                ZERO_CL_CLASS_ARRAYS, &
                RETURN_AMEREN_CL_CLASS_EXPENSES

       INTEGER*2   UPDATE_PR_ESCALATIONS,ESCALATE_ONE_VALUE, &
        VOID_INT2, &
                  R_POINTER,ASSET_ALLOCATION_VECTOR, &
                  ESCALATE_ONE_VALUE_FOR_ADDS

      INTEGER*2 RETURN_CL_FUEL_POINTERS, &
        UPDATE_PRIMARY_FUEL_MIX_RATIO, &
                FIRST_YEAR_PRICE_MODE/19999/, &
                GET_FIRST_YEAR_PRICE_MODE

      REAL RETURN_ECITIES_OBJ_VARS
      INTEGER*2 NUMBER_OF_CAP_LIMITED_CLASSES, &
! MAX_CAP_LIMITED_CLASS_ID_NUM now in ns_cla_decs.
                RETURN_MAX_CL_CLASS_NUM
      INTEGER*2 GET_CL_ASSET_CLASS_INFO, &
                ASSET_CLS_NUM(MAX_CL_UNITS), &
                ASSET_CLS_VECTOR(MAX_CL_UNITS), &
                INTRA_COMPANY_CLSID(MAX_CL_UNITS)
       REAL*4 UNSCHEDULED_MAINT_HOURS
      REAL  PERIOD_RATE,FOR_MT, &
            MW_CAPACITY,RTEMP,GET_VAR, &
            GET_ANNUAL_CL_CAPACITY, &
            MONTHLY_DISPATCH_COST(2,12), &
            MONTHLY_CAPACITY(2,12), &
            LOCAL_COEFF(3)
      LOGICAL (kind=1) :: OFFSET_MAINTENANCE_VECTORS, &
        OFFSET_MAINTENANCE_ACTIVE, &
               CALC_ANNUAL_CAP_AND_MAINT, &
            SAVE_DETAILED_MAINTENANCE,DETAILED_MAINTENANCE_IS_ACTIVE, &
               DETAIL_MAINTENANCE_NOT_ACTIVE,UPDATE_PERIOD_CAPACITY, &
               UPDATE_PERIOD_MAINTENANCE,TRANS_GROUP_ACTIVE_SWITCH, &
               RUN_TRANSACT=.FALSE., &
               IN_ACTIVE_THERMAL_MARKET_AREA, &
               UNIT_ON_LINE_IN_YEAR, &
               GET_CUBIC_HEAT_CURVE, &
               GET_PW_UNIT_TEXT_FIELDS
      INTEGER*2 TEMP_I2, &
           GET_MARKET_RESOURCE_INDEX, &
           GET_MARKET_RESOURCE_COUNTER, &
           MARKET_COUNT/0/, &
           TWO_BLOCK(2)
      real :: GET_MAINTENANCE_RATE_FOR_MONTH

      
      real :: GET_FUEL_DELIVERY, &
           GET_SECOND_FUEL_DELIVERY, &
           GET_EMERGENCY_CAPACITY, &
           GET_DISPADJ2_SAVE
       real :: PERCENT_RETAIL
      real :: GET_ANNUAL_GROSS_MARGIN, &
           GET_START_UP_COSTS, &
           GET_RAMP_RATE, &
           GET_RAMP_DOWN_RATE, &
           GET_MIN_UP_TIME, &
           GET_MIN_DOWN_TIME, &
           GET_FOR_DURATION, &
           GET_FOR_FREQUENCY

      real :: SAVE_ANNUAL_GROSS_MARGIN, &
           GET_MIN_SPIN_CAP, &
           GET_MAX_SPIN_CAP

      real :: GET_MONTHLY_FUEL_INDEX, &
           GET_UNIT_START_UP_COSTS
      real :: ESCALATE_THERMAL_VOM, &
           ESCALATE_THERMAL_FOM
      real :: GET_FIXED_COST_IN
      real :: GET_VCPMWH_IN
      integer (kind=2) :: GET_MAX_TRANS_ID_USED
      integer (kind=2) :: SET_TRANS_FOR_DATA_BASE
      integer (kind=2) :: GET_TRANS_FOR_DATA_BASE, &
          GET_DATA_BASE_FOR_TRANS, &
          GET_DATA_BASE_FOR_UNIT, &
          GET_MAX_DATA_BASES, &
          DAY_TYPE_FOR_RESOURCE, &
          GET_STATE_PROVINCE_NAMES, &
          GET_MAX_STATE_PROVINCE_NO, &
          GET_UNIT_STATE_PROVINCE_INDEX

      REAL UNIT_CAPACITY, &
           CL_PLANNING_CAPACITY,FIRST_YEAR_CAPACITY, &
           R_CL_CAP_PLANNING_REMOVALS, &
           R_CL_CAPACITY_PLANNING_ADDITIONS, &
           OLD_CL_PLANNING_CAPACITY,NEW_CL_PLANNING_CAPACITY
     integer*2  :: GET_UNIT_GAS_REGION_INDEX
     REAL CL_PLANNING_LOAD_REDUCTION

      real (kind=4) :: GET_CO2_RETRO_CAP_MULT, &
               GET_CO2_RETRO_HEAT_MULT

      real (kind=4) ::  GET_CAP_MARKET_REVENUE, &
                     GET_CL_TG_CAP_MARKET_MW, &
                     GET_CL_TG_CAP_MARKET_REV
      logical (kind=1) :: CALC_CL_CAP_MARKETS
      LOGICAL*1      CAP_MARKET_ACTIVE,CAP_MARKET_SALE, &
                     INIT_CAP_MARKET_REVENUE, &
                     YES_GSP_IS_ST_TG ! External
      LOGICAL*1 VOID_LOGICAL,GET_PURCHASE_POWER_ASSIGN, &
                 ZONAL_LEVEL_MARKET, &
                 YES_ZONAL_LEVEL_MARKET, &
                 RESURRECT_RETROFIT_UNIT, &
                 ADJUST_CO2_RETRO_PLAN_CAP, &
                 ADJUST_GRX_CO2_RETRO_PLAN_CAP


      logical (kind=1) :: INIT_CLA_UNIT_UP_YESTERDAY, &
                GET_CLA_UNIT_UP_YESTERDAY, &
                PUT_CLA_UNIT_UP_YESTERDAY

      LOGICAL*1 GET_START_UP_LOGIC,GET_CL_BASECASE_MARKET_ID, &
                GET_SPIN_STATUS, &
                IS_A_MARKET_RESOURCE, &
                YES_ALLOW_DECOMMIT_BY_UNIT, &
                YES_REGION_POWER_MAP

      logical(kind=1) :: IS_STRICT_MARKET_RESOURCE, &
                PROCESS_MARKET_RESOURCES


      REAL :: GET_CL_TG_CAP, &
           GET_NEWGEN_CAP_BY_INDEX, &
           GET_CL_AFTER_PEAK, &
           GET_CL_TG_RETIRE, &
           GET_TOTAL_INCREMENTAL_COST

      REAL CAP_MW, &
       FIRST_YEAR_CL_CAPACITY, &
           R_CAP_MW



      LOGICAL*1 SUBTRACT_IT,REPORT_THIS_CL_UNIT

      LOGICAL*1 RETURN_CL_ASSET_CLASS_PROD, &
        RETURN_CL_CLASS_TOTAL_PROD, &
                RETURN_FE_PNL_EXPENSES

      integer (kind=2) :: RETROFIT_ACTIVE_ID
      LOGICAL (kind=1) :: PUT_THERMAL_RPS_ENRG_CAP

        logical (kind=1) :: GET_THERMAL_RPS_DATA, &
                GET_NEXT_MRX_RETIRE_RETRO, &
                YES_CO2_ABATEMENT_REPORT, &
                GET_CO2_RETIREMENTS_LOGIC
      INTEGER*2 ADD_NEW_CL_UNIT, &
                GET_THERMAL_STATE_INDEX

      logical (kind=1) :: RETROFIT_CO2_THERMAL_UNIT, &
                MRX_RETIRE_THIS_UNIT, &
                ANNUAL_CO2_RETIREMENTS_PROCESS, &
                ANNUAL_CO2_RETROFIT_PROCESS, &
                ANNUAL_RETIRE_RETRO_PROCESS

      LOGICAL*1 CL_RESET_PRICES,TRANS_ANNUAL_UNIT_RETIREMENT, &
                RETIRE_CO2_THERMAL_UNIT
      INTEGER*2 SET_AI_CL_REMAINING_LIFE,LOCAL_POINTER
      INTEGER*2 RETURN_SHADOW_UNIT_NUMBER,AI_REMAINING_LIFE


            integer (kind=2) :: RETURN_CL_UNITS_B4_ADDITIONS, &
                RETURN_UNIT_ADDITIONS, &
                RESET_CL_UNITS, &
                RETURN_CL_UNITS_AFTER_HARD_ADDS

      integer (kind=2) :: CL_PLANNING_ADDITIONS
      integer (kind=2) :: INCREMENT_AVAILABLE_CL_UNITS, &
                INCREMENT_HARDWIRED_CL_UNITS

      integer (kind=2) ::  GET_POINTER_FOR_NEW_CL_UNIT, &
                  GET_PRIMARY_MOVER_INDEX, &
                  GET_NUM_ANNUAL_DERIVATIVES

      integer (kind=2) :: GET_MAX_FO_PER_MONTH, &
                  GET_NEWGEN_INDEX, &
                  GET_THERMAL_TRACKER_INDEX, &
                  GET_THERMAL_RES_TRACKER_INDEX, &
                  GET_THERMAL_FUEL_TRACKER_INDEX, &
                  GET_THERMAL_MEM_TRACKER_INDEX, &
                  GET_EMISSION_MARKET_LINK, &
                  GET_START_UP_INDEX, &
                  GET_TOTAL_START_UP_UNITS, &
                  GET_CL_ENERGY_BY_TYPE, &
                  GET_RESOURCE_ID_TO_UNIT, &
                  GET_I8_ID_TO_UNIT

      real (kind=4) :: GET_SCENARIO_NUCLEAR_AVAIL, &
             GET_MONTHLY_REGIONAL_OUTAGE, &
             GET_SCENARIO_COAL_AVAIL, &
             GET_SCENARIO_GAS_AVAIL, &
             GET_SCENARIO_OIL_AVAIL, &
             GET_SCENARIO_OTHER_AVAIL, &
             GET_SCENARIO_HYDRO_WATER_YEAR, &
             GET_NOX_VOM, &
             GET_NOX_FOM, &
             GET_SOX_VOM, &
             GET_SOX_FOM, &
             GET_CO2_VOM, &
             GET_CO2_FOM, &
             GET_HG_VOM, &
             GET_HG_FOM, &
             GET_OTHER3_VOM, &
             GET_OTHER3_FOM, &
             GET_MARKET_FLOOR, &
             GET_MARKET_CEILING, &
             GET_TRANS_RPS_PERCENT, &
             GET_CO2_RETIREMENT_PRICE, &
             GET_CO2_RETIRE_RETRO_PRICE, &
             INIT_ANNUAL_GROSS_MARGIN

      LOGICAL*1 RETURN_CL_INTRA_EXPENSES, &
                REGIONAL_MAINT_SCHEDULING, &
                REGIONAL_PA_MAINT_SCHEDULING, &
                REGIONAL_TG_MAINT_SCHEDULING, &
                REGIONAL_PA_MAINTENANCE_LOGIC, &
                REGIONAL_TG_MAINTENANCE_LOGIC
      real (kind=4) :: GET_CM_RETIREMENT_KW, &
             GET_TRANS_GROUP_PEAK, &
             GET_PA_PEAK


      logical (kind=1) :: EXPENSE_ASSIGNMENT_IS_PURCHASE, &
              RETURN_CL_INTRA_CLASS_REVENUES

      logical (kind=1) :: TEST_MONTHLY_MUST_RUN, &
                  GET_NOX_CONTROL_FOR_UNIT, &
                  GET_SOX_CONTROL_FOR_UNIT, &
                  GET_CO2_CONTROL_FOR_UNIT, &
                  SET_SOX_CONTROL_COAL_LP, &
                  RESET_SOX_CONTROL_COAL_LP, &
                  YES_DETAILED_TRANSFER_PRICING, &
                  NOX_ACTIVE_FOR_UNIT, &
                  GET_HG_CONTROL_FOR_UNIT, &
                  GET_OTHER3_CONTROL_FOR_UNIT, &
                  R_SEASON_IS_NOX_SEASON

      logical (kind=1) :: INIT_MON_MDS_CL_UNITS, &
              MON_MDS_CL_VAR,MON_MDS_CL_FIXED,MON_MDS_NUC_ADDER
       integer (kind=2) :: STATE_2_GAS_REGION_LOOKUP ! External
       integer (kind=2) :: YR
       integer, save :: numcalls=0
       logical (kind=1) :: tgas
       integer, save :: timeshere=0
       integer (kind=2) :: del_ord
       integer :: lb_1, lb_2, lb_3, lb_4
       integer :: ub_1, ub_2, ub_3, ub_4
       integer (kind=2) :: year_argument
       
       integer (kind=2) :: pa_ord
       integer :: ub
       integer (kind=2) :: ofline_unit_number

      ! Gets here
      numcalls=numcalls+1
! END OF DATA DECLARATIONS
      ASSET_ALLOCATOR=0. ! Spurious lahey error about unset variable.
      UPPER_TRANS_Gp = int(GET_NUMBER_OF_ACTIVE_GROUPS(),2)
      CapEx_Running = YES_POWERWORLD_REPORT()
      UNIT_NAME_COUNT = 0
      INSTALLED_POINTER = 0

      BEGIN_DATE = 100.*(get_BASE_YEAR()+1-1900)

      MAX_PLANNING_AREAS = GET_NUMBER_OF_PLANNING_GROUPS()


      call write_trace_bool1(file_trace_cacm, &
        "REGIONAL_PA_MAINT_SCHEDULING=", &
        REGIONAL_PA_MAINT_SCHEDULING)

      REGIONAL_PA_MAINT_SCHEDULING = REGIONAL_PA_MAINTENANCE_LOGIC()
      call write_trace_bool1(file_trace_cacm, &
        "REGIONAL_PA_MAINT_SCHEDULING changed to ", &
        REGIONAL_PA_MAINT_SCHEDULING)
      call write_trace_bool1(file_trace_cacm, &
        "REGIONAL_TG_MAINT_SCHEDULING",  REGIONAL_TG_MAINT_SCHEDULING)
      
      REGIONAL_TG_MAINT_SCHEDULING = REGIONAL_TG_MAINTENANCE_LOGIC()
      
      call write_trace_bool1(file_trace_cacm, &
      "REGIONAL_TG_MAINT_SCHEDULING changed to ", &
        REGIONAL_TG_MAINT_SCHEDULING)

      IF(REGIONAL_PA_MAINT_SCHEDULING) THEN
         MAX_MAINT_PLANNING_AREAS = MAX_PLANNING_AREAS
         ! REGIONAL_TG_MAINT_SCHEDULING is probably wrong here.
      ELSEIF(REGIONAL_TG_MAINT_SCHEDULING) THEN
         MAX_MAINT_PLANNING_AREAS = UPPER_TRANS_Gp
      ELSE
         MAX_MAINT_PLANNING_AREAS = 1
      ENDIF

      MAX_TRANS_GROUPS = 256

      MAX_DAY_TYPES = MIN(GET_MAX_DAY_TYPES(),GET_LAST_DAY_TYPE())
      IF(ALLOCATED(TG_2_PLANNING_AREA)) DEALLOCATE( &
        TG_2_PLANNING_AREA)
      ALLOCATE(TG_2_PLANNING_AREA(0:MAX(1,UPPER_TRANS_Gp)))
      IF(ALLOCATED(DAY_TYPE_TRANS_GROUP_PAIR)) &
                DEALLOCATE(DAY_TYPE_TRANS_GROUP_PAIR, &
        DATA_BASE_POSITION)
      ALLOCATE(DAY_TYPE_TRANS_GROUP_PAIR(MAX_DAY_TYPES, &
                                                   0:MAX_TRANS_GROUPS))
      ALLOCATE(DATA_BASE_POSITION(2*MAX_DAY_TYPES,MAX_CL_UNITS))
      IF(ALLOCATED(Mtnc_DAYS)) DEALLOCATE(Mtnc_DAYS)
      ALLOCATE(Mtnc_DAYS(MAX_CL_UNITS))
      ! Gets here
      IF(ALLOCATED(CL_ANN_CAP)) DEALLOCATE(CL_ANN_CAP,CL_TG_CAP, &
                                           CL_TG_AFTER_PEAK, &
                                           CL_TG_RETIRE, &
                                           NEWGEN_CAP_BY_INDEX, &
                                           CL_TG_CAP_MARKET_MW, &
                                           CL_TG_CAP_MARKET_REV)
      ALLOCATE(CL_ANN_CAP(3,get_globecom_study_period(),2))
      ALLOCATE(CL_TG_CAP(0:max_technology_counters,0:MAX(1,UPPER_TRANS_Gp), &
       get_globecom_study_period(),2))
      ALLOCATE(NEWGEN_CAP_BY_INDEX(MAX_NEWGEN_INDEX, &
                                         0:MAX(1,UPPER_TRANS_Gp), &
                                                        get_globecom_study_period()))
      ALLOCATE(CL_TG_AFTER_PEAK(0:UPPER_TRANS_Gp,get_globecom_study_period()))
      ALLOCATE(CL_TG_CAP_MARKET_MW(0:UPPER_TRANS_Gp), &
               CL_TG_CAP_MARKET_REV(0:UPPER_TRANS_Gp))
      ALLOCATE(CL_TG_RETIRE(0:UPPER_TRANS_Gp,get_globecom_study_period()))
      IF(ALLOCATED(CL_ANNUAL_LOAD_REDUCTION)) &
               DEALLOCATE(CL_ANNUAL_LOAD_REDUCTION)
      ALLOCATE(CL_ANNUAL_LOAD_REDUCTION(get_globecom_study_period()))
      HIGHEST_ID = 1

      UNIT_FREQUENCY_DURATION = .FALSE.

      ZONAL_LEVEL_MARKET = YES_ZONAL_LEVEL_MARKET()

      UNIT_STATE_PROVINCE_INDEX = 0
      UNIT_GAS_REGION_INDEX = 0
      STATE_PROVINCE_INDEX = 0
      STATE_PROVINCE_ADDRESS = 0
      MAX_STATE_PROVINCE_NO = 0
      STATE_PROVINCE_NAMES(0) = "NA "

! 112307. HARD-WIRED TO FALSE FOR INTEGRATED PRICING



      YES_AOD_PRICE_THERM_VALUE_LIST = &
             AOD_THERMAL_UNIT_VALUE_LIST(YES_AOD_COMP_THERM_VALUE_LIST)

      TURN_OFF_POLY = YES_TURN_OFF_POLY()

      NEW_BUILD_ADDITIONS_ACTIVE = YES_NEW_BUILD_ADDITIONS_ACTIVE()
      ! Gets here
      IF(NEW_BUILD_ADDITIONS_ACTIVE) THEN
         TEMP_L = GET_NEW_BUILD_PERCENTS( &
                                    UNDER_CONSTRUCTION_PERCENT, &
                                    ADVANCED_DEVELOPMENT_PERCENT, &
                                    EARLY_DEVELOPMENT_PERCENT, &
                                    PROPOSED_PERCENT, &
                                    INDEFIN_POSTPHONED_PERCENT)
      ENDIF

      TG_2_PLANNING_AREA = 0
      DO TG = 1, UPPER_TRANS_Gp
         
         TG_2_PLANNING_AREA(TG) = GET_PA_FROM_TG(TG)

      ENDDO

      RETROFIT_ACTIVE = .FALSE.
      NEWGEN_INDEX = -1
      NEWGEN_COUNTER = 0

      START_UP_INDEX= 0

      RESOURCE_ID_TO_UNIT = 0

      NUMBER_OF_RESOURCE_ID = 0

      ANNUAL_GROSS_MARGIN = 0.
      ECON_RETIRE_MARGIN = 0.

      CALL RESET_NUCLEAR_UNIT_COUNTER()

      MONTHLY_MUST_RUN_vctr = 0
      MONTHLY_dcmt_VECOT = 0
      EMISSION_MARKET_LINK = 0
      CO2_RTRO_CAP_MULT = 0.0


      MAX_DAY_TYPES = MIN(GET_MAX_DAY_TYPES(),GET_LAST_DAY_TYPE())


      DAY_TYPE_TRANS_GROUP_PAIR = 0
      DATA_BASE_POSITION = 0.

!     TRACKING CENTRAL DISPATCHING FOR EVERY DAY TYPE ACROSS
!     TRANSACTION GROUPS

      MAX_TRANS_DATA_BASES = MAX_DAY_TYPES
      DO M = 1, MAX_DAY_TYPES
         DAY_TYPE_TRANS_GROUP_PAIR(M,0) = M
      ENDDO

      SAVE_DETAILED_MAINTENANCE = .FALSE.

      CALL DOES_RDI_FILE_EXIST(RDI_FILE_EXISTS)
      ! Gets here
      IF(RDI_FILE_EXISTS) THEN
         ! Didn't get here first time.
         CALL GET_NUM_OF_RDI_CL_RECORDS(RDI_RECORDS,RDI_CL_RECORDS)
         IF(RDI_CL_RECORDS > 0) THEN
            CALL OPEN_RDI_CL_FILES(RDI_IN_UNIT,RDI_OUT_UNIT)
            WRITE(RDI_OUT_UNIT,*) "9, "
         ENDIF
      ELSE
         RDI_RECORDS = 0
         RDI_CL_RECORDS = 0
      ENDIF

      SAVE_IGNORE_NON_UTILITY = IGNORE_NON_UTILITY()

      CALL RETURN_NUCLEAR_UNITS_ACTIVE(NUM_OF_NUCLEAR_UNITS)
      ! Gets here. NUM_OF_NUCLEAR_UNITS=0
      IF(NUM_OF_NUCLEAR_UNITS > 0) THEN
         CALL SETUP_GENERATING_INFORMATION(NUM_OF_NUCLEAR_UNITS)
      ENDIF


      CUBIC_HEAT_CURVE = 0.

      CL_ANN_CAP = 0.
      CL_TG_CAP = 0.
      NEWGEN_CAP_BY_INDEX = 0.
      CL_TG_AFTER_PEAK = 0.
      CL_TG_RETIRE = 0.
      RETROFIT_ACTIVE = .FALSE.
      PRIM_HEAT_CONTENT = 0.
      NOX_CNTRL_PERCENT = 0.
      SOX_CNTRL_PERCENT = 0.
      CO2_CONTROL_PERCENT = 0.
      HG_CNTRL_PERCENT = 0.
      OTHER3_CNTRL_PERCENT = 0
      CO2_CONTROL_DATE = 0
      HG_CTL_DATE = 0
      OTH3_CONTROL_DATE = 0.
      EMRGCY_CAPACITY = 0.
      EMRGENCY_HEATRATE = 0.
      CAP_MARKET_MONTH_NO = 0
      ! Johncheck
      ns_cla_decs%capacity_market_pointer_acl = 0
      RETROFIT_UNIT_INDEX = 0
      RETROFIT_COUNTER = 0
      RETIREMENT_COUNTER = 0

      POINTER_FOR_NEW_CL_UNIT = 0
      CL_ANNUAL_LOAD_REDUCTION = 0.
      CL_CAP_AREA_LINKED = .FALSE.
      SP_NEWGEN_UNIT_STATUS = " "
      CAP_LIMITED_CLASS_POINTER = 0
      NUMBER_OF_CAP_LIMITED_CLASSES = 0
      SAVE_DETAILED_MAINTENANCE = .FALSE.

      CALL DOES_TG_FILE_EXIST(TG_FILE_EXISTS)
      CALL DOES_TF_FILE_EXIST(TF_FILE_EXISTS)
      RUN_TRANSACT = TG_FILE_EXISTS .AND. UPPER_TRANS_Gp > 0

      RDI_REC = 0
      RDI_CL_REC = 0
      call set_nunits(int(1,2))
      
      
      UNIQUE_ID_NUM = 0
      CL_UNIT_UNIQUE_RPT_ID = 0
      TOTAL_START_UP_UNITS = 0
      PROCESSING_FIRST_DATA_FILE = .TRUE.
      PHASE_I_UNIT = .FALSE.
      ! Removed IF statement that will never execute because false
      ! is never true.

      
      DO FILE_ID = 0, MAX_CL_FILES-1

         IF(.NOT. SP_CAPEX_ACTIVE() .AND. FILE_ID == 23) then
            CYCLE
         endif
         CALL OPEN_CL_FILE(INT2(10),CL_RECORDS,FILE_ID)
         IF(CL_RECORDS == 0) then 
            CYCLE
         end if
         
         IREC = 0
               ! Filenames agree between reference and current
               inquire(unit=10, NAME=file_name)

         IREC=IREC 
         DOWHILE (IREC < CL_RECORDS .OR. RDI_CL_REC < RDI_CL_RECORDS)
            IF(IREC < CL_RECORDS) THEN
               IREC = IREC + 1

               UNIQUE_ID_NUM = UNIQUE_ID_NUM + 1

       ! Initializations added 11/21/2024
       del_ord=0
       SP_UNIT_NAME(get_nunits())=" "
       CL_LOAD_TYPE=" "
       EXPENSE_ASSIGNMENT_prod(get_nunits())=" "

       foshyd%CAP_FRAC_OWN(get_nunits())=0
       ON_LINE_MONTH=0
       ON_LINE_YEAR=0
       OFF_LINE_MONTH(get_nunits())=0
       OFF_LINE_YEAR(get_nunits())=0
       FUEL_MIX_PTR(get_nunits())=0
       PBTUCT(get_nunits())=0
       PFESCR(get_nunits())=0
       SBTUCT(get_nunits())=0
       SFESCR(get_nunits())=0
       FUELADJ(get_nunits())=0
       EFOR(get_nunits())=0
       mnrate(get_nunits(),1)=0


       VCPMWH_IN(get_nunits())=0
       OMESCR(get_nunits())=0
       FIXED_COST_IN(get_nunits())=0
       FIXED_COST_ESCALATOR(get_nunits())=0
       DISPADJ_prod(get_nunits())=0
       HR_FACTOR(get_nunits())=0
       input_mw(1,get_nunits())=0


       foshyd%CAP_PLANNING_FAC(get_nunits())=0
       coeff(1, get_nunits())=0

       CL_AI_CAPACITY_RATE(get_nunits())=0
       CL_AI_CAPACITY_ESCALATOR(get_nunits())=0
       CL_AI_ENERGY_RATE(get_nunits())=0
       CL_AI_ENERGY_ESCALATOR(get_nunits())=0
       AI_CL_REMAINING_LIFE(get_nunits())=0
       P_SO2(get_nunits())=0
       P_NOX(get_nunits())=0
       P_PARTICULATES(get_nunits())=0
       LOCAL_CL_POOL_FRAC_OWN(get_nunits())=0
       P_EMIS_OTH2(get_nunits())=0
       P_EMIS_OTH3(get_nunits())=0
       PHASE_I_STR=" "
       DISPADJ2(get_nunits())=0
       CL_RESOURCE_ID(get_nunits())=0
       FUEL_SUPPLY_ID(get_nunits())=0
       P_NOX_BK2(get_nunits())=0
       SEC_FUEL_EMISS_PTR(get_nunits())=0
       EMISS_FUEL_COST(get_nunits())=0
       EMISS_FUEL_ESCAL(get_nunits())=0
       EMISS_FUEL_EMISS_PTR(get_nunits())=0
       EMISS_BLENDING_RATE(get_nunits())=0
       PRIM_FUEL_EMISS_PTR(get_nunits())=0
       TEMP_PRIM_FUEL_TYPE_STR=" "
       SEC_FUEL_TYPE(get_nunits())=" "
       EMISS_FUEL_TYPE(get_nunits())=" "
       TIE_CONSTRAINT_GROUP(get_nunits())=0
       MONTHLY_CAPACITY_POINTER(get_nunits())=0
       ANNUAL_CL_FIXED_COST(get_nunits())=0
       ANNUAL_CL_FIXED_COST_ESC(get_nunits())=0
       Mtnc_DAYS(get_nunits())=0
       DISPATCH_MULT(get_nunits())=0
       EXCESS_ENERGY_SALES(get_nunits())=0
       AI_CL_TAX_LIFE(get_nunits())=0
       AI_CL_ADR_LIFE(get_nunits())=0
       ASSET_CLS_NUM(get_nunits())=0
       ASSET_CLS_VECTOR(get_nunits())=0
       INTRA_COMPANY_CLSID(get_nunits())=0
       INTRA_CO_transaction_loc(get_nunits())=" "
       SPL_unit_id_loc(get_nunits())=" "
       MIN_CAP_FACTOR(get_nunits())=0
       MAX_CAP_FACTOR(get_nunits())=0
       TRANSACTION_GROUP_ID(get_nunits())=0
       dTYPE_ID(get_nunits())=0
       unit_is_active="T"
       Bc_PLANT_ID(get_nunits())=" "
       bcuid=" "
       Bc_MARKET_AREA_ID(get_nunits())=" "
       BC_TRANS_AREA_ID=" "
       BASECASE_PLANT_NERC_SUBID=" "
       BC_PLANT_OWNER_ID=" "
       UTILITY_OWNED_loc="Y"
       MONTHLY_FUEL_Idx(get_nunits())=0
       STARTUP_COSTS(get_nunits())=0
       STARTup_LOGIC(get_nunits())=" "
       RAMP_RATE_array(get_nunits())=0
       MIN_DOWN_TIMEs(get_nunits())=0
       ns_cla_decs%REPORT_THIS_UNIT(get_nunits())=" "
       ns_p_fuel_annl%P_FUEL_DELIVERY_1(get_nunits())=0
       ns_p_fuel_annl%P_FUEL_DELIVERY_2(get_nunits())=0
       ns_p_fuel_annl%P_FUEL_DELIVERY_3(get_nunits())=0
       MINimum_UP_TIME(get_nunits())=0
       HESI_UNIT_ID_NUMs(get_nunits())=0
       FOR_FREQUENCies(get_nunits())=0
       FOR_DURAtn(get_nunits())=0
       WINTER_TOTAL_CAPACITY=0
       CONTRIBUTES_2_SPIN(get_nunits())="F"
       H2_UNIT_ID_NUM(get_nunits())=0
       POWERDAT_PLANT_ID(get_nunits())=0
       APPLY_Nx_SEASON_DATE(get_nunits())=" "
       NOX_SsN_DATE(get_nunits())=0
       RAMP_DOWN_RATEs(get_nunits())=0
       NOX_CNTRL_PERCENT(get_nunits())=0
       NOX_CTL_DATE(get_nunits())=0
       EMRGCY_CAPACITY(get_nunits())=0
        EMRGENCY_HEATRATE(get_nunits())=0
       MkT_RESOURCE(get_nunits())=" "
       PRIM_MOVER_STR(get_nunits())=" "
       cnty=" "
       st_pvnc=" "
       ns_cla_ox%NOX_VOM(get_nunits())=0
       ns_cla_ox%NOX_FOM(get_nunits())=0
       S_FUEL_DELIVERY(get_nunits())=0
       S_FUEL_DELIVERY_2(get_nunits())=0
       S_FUEL_DELIVERY_3(get_nunits())=0
       CONSTANT_POLY_ord=0
       FIRST_POLY_ord=0
       SECOND_POLY_ord=0
       THIRD_POLY_ord=0
       USE_PLY_HEAT_RATES=" "
       SP_NEWGEN_UNIT_STATUS(get_nunits())=" "
       MONTHLY_MUST_RUN_vctr(get_nunits())=0
       EMISSION_MARKET_LINK(get_nunits())=0
       wvpa_rt_tckr(get_nunits())=" "
       ALLOW_DcmT(get_nunits())=" "
       MIN_SPIN_Cpcty(get_nunits())=0
       MAX_SPN_CAP(get_nunits())=0
       wvpa_res_trkr(get_nunits())=" "
       WVPA_fl_TCKR_loc(get_nunits())=" "
       monte_or_frq(get_nunits())=" "
       WVPA_MM_TRACKER(get_nunits())=" "
       MONTHLY_dcmt_VECOT(get_nunits())=0
       TRANS_BUS_ID(get_nunits())=0
       SOX_CNTRL_PERCENT(get_nunits())=0
       SOX_CTL_DATE(get_nunits())=0
       ns_cla_ox%SOX_VOM(get_nunits())=0
       ns_cla_ox%SOX_FOM(get_nunits())=0
       LATITUDE(get_nunits())=0
       LONGITUDE(get_nunits())=0
       MW_INSIDE_FENCE_ord=0
       do m=1,3
        inter_blocks(m, get_nunits())=0
       end do


       FIRST_YEAR_DECOM_AVAIL_capex(get_nunits())=0
       DECOMMISSIONING_BASE_YR_COST_capex(get_nunits())=0
       DECOM_CONT_COST_ESCALATION(get_nunits())=0
       ANNUAL_ENERGY_PLANNING_FACTOR_capex(get_nunits())=0
       AGGREGATE_THIS_UNIT(get_nunits())=" "
       NAME_PLATE_CAPACITY_capex(get_nunits())=0
       FUEL_BLENDING_IS(get_nunits())=" "
       FuelRatio_capex(1,get_nunits())=0
       FuelRatio_capex(2,get_nunits())=0
       FuelRatio_capex(3,get_nunits())=0
       FuelRatio_capex(4,get_nunits())=0
       FuelRatio_capex(5,get_nunits())=0
       BlendableFuelsPtr(1,get_nunits())=0
       BlendableFuelsPtr(2,get_nunits())=0
       BlendableFuelsPtr(3,get_nunits())=0
       BlendableFuelsPtr(4,get_nunits())=0
       FuelTransportationCost(1,get_nunits())=0
       FuelTransportationCost(2,get_nunits())=0
       FuelTransportationCost(3,get_nunits())=0
       FuelTransportationCost(4,get_nunits())=0
       BlendedEnthalpyUp(get_nunits())=0
       BlendedEnthalpyLo(get_nunits())=0
       BlendedSO2Up(get_nunits())=0
       BettermentProjectID(get_nunits())=0
       DECOM_CONTINUING_COST(get_nunits())=0
	   !CAPEX
       DECOM_CONT_COST_ESCALATION(get_nunits())=0
       EnrgPatternPointer_capex(get_nunits())=0
       EMISSION_DATA_UNITS(get_nunits())=" "
       EmissRedRate(1,get_nunits())=0
       EmissRedRate(2,get_nunits())=0
       EmissRedRate(3,get_nunits())=0
       EmissRedRate(4,get_nunits())=0
       EmissRedRate(5,get_nunits())=0
       EmissMaxRate(1,get_nunits())=0
       EmissMaxRate(2,get_nunits())=0
       EmissMaxRate(3,get_nunits())=0
       EmissMaxRate(4,get_nunits())=0
       EmissMaxRate(5,get_nunits())=0
       MaxEnergyLimit(1,get_nunits())=0
       MaxEnergyLimit(2,get_nunits())=0
       MaxEnergyLimit(3,get_nunits())=0
       TECH_TYPE(get_nunits())=" "
       LINKED_BETTERMENT_OPTION(get_nunits())=" "
       CO2_CONTROL_PERCENT(get_nunits())=0
       HG_CNTRL_PERCENT(get_nunits())=0
       OTHER3_CNTRL_PERCENT(get_nunits())=0
       CO2_CONTROL_DATE(get_nunits())=0
       HG_CTL_DATE(get_nunits())=0
       OTH3_CONTROL_DATE(get_nunits())=0
       ns_cla_ox%CO2_VOM(get_nunits())=0
       ns_cla_ox%CO2_FOM(get_nunits())=0
       ns_cla_ox%HG_VOM(get_nunits())=0
       ns_cla_ox%HG_FOM(get_nunits())=0
       ns_cla_ox%OTHER3_VOM(get_nunits())=0
       ns_cla_ox%OTHER3_FOM(get_nunits())=0
       MARKET_FLOOR(get_nunits())=0
       MARKET_CEILING(get_nunits())=0
       STARTUP_COSTS_ESCALATION(get_nunits())=0
       CAP_MARKET_TYPE(get_nunits())=" "
       CAPACITY_MARKET_MTH(get_nunits())=" "
       ns_cla_decs%capacity_market_pointer_acl(get_nunits())=0
       CAPACITY_MARKET_COST_ASSIGN(get_nunits())=" "
       CAP_MARKET_EXP_COLLECT(get_nunits())=" "
       CAPACITY_MARKET_COIN_ADJ_FCT(get_nunits())=0
       prim_fuelcat(get_nunits())=" "
       scndary_fuel_cat(get_nunits())=" "
       EMSSNS_FUEL_CATEGORY(get_nunits())=" "
       RPS_CONTRIBUTION_PERCENT(get_nunits())=0
       RETIREMENT_CANDIDATE(get_nunits())=" "
       RETROFIT_CANDIDATE(get_nunits())=" "
       RETROFIT_PROJ_ID(get_nunits())=0
       CO2_BASIN_NAME_cla=" "
       CO2_PPLN_DISTANCE(get_nunits())=0
       STATE_PROVince_NAME=" "
       CO2_RTRO_HEAT_MULT(get_nunits())=0
       CO2_RTRO_CAP_MULT(get_nunits())=int(0,2)
       GUID=" "
       THERMAL_PARENT_ID=0 !  232
       THrm_AGGREGATED_UNIT=" "
       FIRST_RETIREMENT_YEAR(get_nunits())=0
       UNIT_TYPE=" "
       UNIT_TYPE_CATEGORY=" "
       RPS_PROGRAM_NUMBER(get_nunits())=0




       dbgThisCapMktPtr= &
        ns_cla_decs%capacity_market_pointer_acl(get_nunits())
        dbg_offline_month=off_line_month(get_nunits())
        dbg_online_month=ON_LINE_MONTH
        dbg_online_year=on_line_year
        dbg_vcpmwh_in=VCPMWH_IN(get_nunits())
        FILE_NAME=get_filename_from_unit(10)
              
        
        ! OLFOSIL.BIN and similar
               ! First array read - HAS BAD DATA AFTER THIS READ!
               ! * MW_INSIDE_FENCE_ord should be zero at first read.
               ! * Capacity_market_pointer_acl(get_nunits()) should be 6 
               ! at first read.
               ! Argument 107 is different between read and write
    ! H2_UNIT_ID_NUM
! Initializations added 11/21/2024 (end)

! UQ_CLA_OBJT (same keyword in legacy cla_objt.for)
               READ(10,REC=IREC,IOSTAT=IOS) del_ord, &
       SP_UNIT_NAME(get_nunits()), &
                     CL_LOAD_TYPE, &
       EXPENSE_ASSIGNMENT(get_nunits()), &
       EXPENSE_COLLECTION(get_nunits()), &
       GENGRP(get_nunits()), &
       foshyd%CAP_FRAC_OWN(get_nunits()), &
       ON_LINE_MONTH, &
       ON_LINE_YEAR,OFF_LINE_MONTH(get_nunits()), &
       OFF_LINE_YEAR(get_nunits()), &
       FUEL_MIX_PTR(get_nunits()), &
       PBTUCT(get_nunits()), &
       PFESCR(get_nunits()), &
       SBTUCT(get_nunits()), &
       SFESCR(get_nunits()), &
       FUELADJ(get_nunits()), &
       EFOR(get_nunits()), &
       (MNRATE(get_nunits(),M),M=1,12), &
       VCPMWH_IN(get_nunits()), &
       OMESCR(get_nunits()), &
       FIXED_COST_IN(get_nunits()), &
       FIXED_COST_ESCALATOR(get_nunits()), &
       DISPADJ(get_nunits()), &
       HR_FACTOR(get_nunits()), &
       (INPUT_MW(M,get_nunits()),M=1,2), &
       foshyd%CAP_PLANNING_FAC(get_nunits()), &
       (COEFF(M,get_nunits()),M=1,3), &
       CL_AI_CAPACITY_RATE(get_nunits()), &
       CL_AI_CAPACITY_ESCALATOR(get_nunits()), &
       CL_AI_ENERGY_RATE(get_nunits()), &
       CL_AI_ENERGY_ESCALATOR(get_nunits()), &
       AI_CL_REMAINING_LIFE(get_nunits()), &
       P_SO2(get_nunits()), &
       P_NOX(get_nunits()), &
       P_PARTICULATES(get_nunits()), &
       LOCAL_CL_POOL_FRAC_OWN(get_nunits()), &
       P_EMIS_OTH2(get_nunits()), &
       P_EMIS_OTH3(get_nunits()), &
       PHASE_I_STR, &
       DISPADJ2(get_nunits()), &
       CL_RESOURCE_ID(get_nunits()), &
       FUEL_SUPPLY_ID(get_nunits()), &
       P_NOX_BK2(get_nunits()), &
       SEC_FUEL_EMISS_PTR(get_nunits()), &
       EMISS_FUEL_COST(get_nunits()),        &
       EMISS_FUEL_ESCAL(get_nunits()), &
       EMISS_FUEL_EMISS_PTR(get_nunits()), &
       EMISS_BLENDING_RATE(get_nunits()), &
       PRIM_FUEL_EMISS_PTR(get_nunits()), &
       TEMP_PRIM_FUEL_TYPE_STR, &
       SEC_FUEL_TYPE(get_nunits()), &
       EMISS_FUEL_TYPE(get_nunits()), &
       TIE_CONSTRAINT_GROUP(get_nunits()), &
       MONTHLY_CAPACITY_POINTER(get_nunits()), &
       ANNUAL_CL_FIXED_COST(get_nunits()),   &
       ANNUAL_CL_FIXED_COST_ESC(get_nunits()), &
       Mtnc_DAYS(get_nunits()), &
       DISPATCH_MULT(get_nunits()), &
       EXCESS_ENERGY_SALES(get_nunits()), &
       AI_CL_TAX_LIFE(get_nunits()), &
       AI_CL_ADR_LIFE(get_nunits()), &
       ASSET_CLS_NUM(get_nunits()), &
       ASSET_CLS_VECTOR(get_nunits()), &
       INTRA_COMPANY_CLSID(get_nunits()), &
       INTRA_CO_transaction_loc(get_nunits()),  &
       SPL_unit_id_loc(get_nunits()), &
       MIN_CAP_FACTOR(get_nunits()), &
       MAX_CAP_FACTOR(get_nunits()), &
       TRANSACTION_GROUP_ID(get_nunits()), &
       dTYPE_ID(get_nunits()),    &
       unit_is_active, &
       Bc_PLANT_ID(get_nunits()), &
       bcuid, &
       Bc_MARKET_AREA_ID(get_nunits()), &
       BC_TRANS_AREA_ID, &
       BASECASE_PLANT_NERC_SUBID, &
       BC_PLANT_OWNER_ID, &
       UTILITY_OWNED_loc, &
       MONTHLY_FUEL_Idx(get_nunits()), &
       STARTUP_COSTS(get_nunits()), &
       STARTup_LOGIC(get_nunits()), &
       RAMP_RATE_array(get_nunits()), &
       MIN_DOWN_TIMEs(get_nunits()), &
       ns_cla_decs%REPORT_THIS_UNIT(get_nunits()), &
       ns_p_fuel_annl%P_FUEL_DELIVERY_1(get_nunits()), &
       ns_p_fuel_annl%P_FUEL_DELIVERY_2(get_nunits()), &
       ns_p_fuel_annl%P_FUEL_DELIVERY_3(get_nunits()), &
       MINimum_UP_TIME(get_nunits()), &
       HESI_UNIT_ID_NUMs(get_nunits()), &
       FOR_FREQUENCies(get_nunits()), &
       FOR_DURAtn(get_nunits()), &
       WINTER_TOTAL_CAPACITY, &
       CONTRIBUTES_2_SPIN(get_nunits()), &
       H2_UNIT_ID_NUM(get_nunits()), &
       POWERDAT_PLANT_ID(get_nunits()), &
       APPLY_Nx_SEASON_DATE(get_nunits()), &
       NOX_SsN_DATE(get_nunits()), &
       RAMP_DOWN_RATEs(get_nunits()), &
       NOX_CNTRL_PERCENT(get_nunits()), &
       NOX_CTL_DATE(get_nunits()), &
       EMRGCY_CAPACITY(get_nunits()), &
        EMRGENCY_HEATRATE(get_nunits()), &
       MkT_RESOURCE(get_nunits()), &
       PRIM_MOVER_STR(get_nunits()), &
       cnty, &
       st_pvnc, & ! Suspect (109th argument)
       ns_cla_ox%NOX_VOM(get_nunits()), &
       ns_cla_ox%NOX_FOM(get_nunits()), &
       S_FUEL_DELIVERY(get_nunits()), &
       S_FUEL_DELIVERY_2(get_nunits()), &
       S_FUEL_DELIVERY_3(get_nunits()), &
       CONSTANT_POLY_ord, &
       FIRST_POLY_ord, &
       SECOND_POLY_ord, &
       THIRD_POLY_ord, &
       USE_PLY_HEAT_RATES, &
       SP_NEWGEN_UNIT_STATUS(get_nunits()), &
       MONTHLY_MUST_RUN_vctr(get_nunits()), &
       EMISSION_MARKET_LINK(get_nunits()), &
       wvpa_rt_tckr(get_nunits()), &
       ALLOW_DcmT(get_nunits()), &
       MIN_SPIN_Cpcty(get_nunits()), &
       MAX_SPN_CAP(get_nunits()), &
       wvpa_res_trkr(get_nunits()), &
       WVPA_fl_TCKR_loc(get_nunits()), &
       monte_or_frq(get_nunits()), &
       WVPA_MM_TRACKER(get_nunits()), &
       MONTHLY_dcmt_VECOT(get_nunits()), &
       TRANS_BUS_ID(get_nunits()), &
       SOX_CNTRL_PERCENT(get_nunits()), &
       SOX_CTL_DATE(get_nunits()), &
       ns_cla_ox%SOX_VOM(get_nunits()), &
       ns_cla_ox%SOX_FOM(get_nunits()), &
       LATITUDE(get_nunits()), &
       LONGITUDE(get_nunits()), &
       MW_INSIDE_FENCE_ord, &
       (INTER_BLOCKS(M,get_nunits()),M=1,3), &
       FIRST_YEAR_DECOM_AVAIL_capex(get_nunits()), &
       DECOMMISSIONING_BASE_YR_COST_capex(get_nunits()), &
       DECOM_CONT_COST_ESCALATION(get_nunits()), &
       ANNUAL_ENERGY_PLANNING_FACTOR_capex(get_nunits()), &
       AGGREGATE_THIS_UNIT(get_nunits()), &
       NAME_PLATE_CAPACITY_capex(get_nunits()), &
       FUEL_BLENDING_IS(get_nunits()), &
       FuelRatio_capex(1,get_nunits()), &
       FuelRatio_capex(2,get_nunits()), &
       FuelRatio_capex(3,get_nunits()), &
       FuelRatio_capex(4,get_nunits()), &
       FuelRatio_capex(5,get_nunits()), &
       BlendableFuelsPtr(1,get_nunits()), &
       BlendableFuelsPtr(2,get_nunits()), &
       BlendableFuelsPtr(3,get_nunits()), &
       BlendableFuelsPtr(4,get_nunits()), &
       FuelTransportationCost(1,get_nunits()), &
       FuelTransportationCost(2,get_nunits()), &
       FuelTransportationCost(3,get_nunits()), &
       FuelTransportationCost(4,get_nunits()), &
       BlendedEnthalpyUp(get_nunits()), &
       BlendedEnthalpyLo(get_nunits()), &
       BlendedSO2Up(get_nunits()), &
       BettermentProjectID(get_nunits()), &
       DECOM_CONTINUING_COST(get_nunits()), &
       DECOM_CONT_COST_ESCALATION(get_nunits()), &
       EnrgPatternPointer_capex(get_nunits()), &
       EMISSION_DATA_UNITS(get_nunits()), &
       EmissRedRate(1,get_nunits()), &
       EmissRedRate(2,get_nunits()), &
       EmissRedRate(3,get_nunits()), &
       EmissRedRate(4,get_nunits()), &
       EmissRedRate(5,get_nunits()), &
       EmissMaxRate(1,get_nunits()), &
       EmissMaxRate(2,get_nunits()), &
       EmissMaxRate(3,get_nunits()), &
       EmissMaxRate(4,get_nunits()), &
       EmissMaxRate(5,get_nunits()), &
       MaxEnergyLimit(1,get_nunits()), &
       MaxEnergyLimit(2,get_nunits()), &
       MaxEnergyLimit(3,get_nunits()), &
       TECH_TYPE(get_nunits()), &
       LINKED_BETTERMENT_OPTION(get_nunits()), &
       CO2_CONTROL_PERCENT(get_nunits()), &
       HG_CNTRL_PERCENT(get_nunits()), &
       OTHER3_CNTRL_PERCENT(get_nunits()), &
       CO2_CONTROL_DATE(get_nunits()), &
       HG_CTL_DATE(get_nunits()), &
       OTH3_CONTROL_DATE(get_nunits()), &
       ns_cla_ox%CO2_VOM(get_nunits()), &
       ns_cla_ox%CO2_FOM(get_nunits()), &
       ns_cla_ox%HG_VOM(get_nunits()), &
       ns_cla_ox%HG_FOM(get_nunits()), &
       ns_cla_ox%OTHER3_VOM(get_nunits()), &
       ns_cla_ox%OTHER3_FOM(get_nunits()), &
       MARKET_FLOOR(get_nunits()), &
       MARKET_CEILING(get_nunits()), &
       STARTUP_COSTS_ESCALATION(get_nunits()), &
       CAP_MARKET_TYPE(get_nunits()), &
       CAPACITY_MARKET_MTH(get_nunits()), &
       ns_cla_decs%capacity_market_pointer_acl(get_nunits()), &
       CAPACITY_MARKET_COST_ASSIGN(get_nunits()), &
       CAP_MARKET_EXP_COLLECT(get_nunits()), &
       CAPACITY_MARKET_COIN_ADJ_FCT(get_nunits()), &
       prim_fuelcat(get_nunits()), &
       scndary_fuel_cat(get_nunits()), &
       EMSSNS_FUEL_CATEGORY(get_nunits()), &
       RPS_CONTRIBUTION_PERCENT(get_nunits()), &
       RETIREMENT_CANDIDATE(get_nunits()), &
       RETROFIT_CANDIDATE(get_nunits()), &
       RETROFIT_PROJ_ID(get_nunits()), &
       CO2_BASIN_NAME_cla, &
       CO2_PPLN_DISTANCE(get_nunits()), &
       STATE_PROVince_NAME, &
       CO2_RTRO_HEAT_MULT(get_nunits()), &
       CO2_RTRO_CAP_MULT(get_nunits()), &
       GUID, &
       THERMAL_PARENT_ID, & !  232
       THrm_AGGREGATED_UNIT, &
       FIRST_RETIREMENT_YEAR(get_nunits()), &
       UNIT_TYPE, &
       UNIT_TYPE_CATEGORY, &
       RPS_PROGRAM_NUMBER(get_nunits())
       

       

       

        dbg_vcpmwh_in=VCPMWH_IN(get_nunits())
        dbg_offline_month=off_line_month(get_nunits())
        dbg_online_year=on_line_year
        dbgThisCapMktPtr= & !Debugstop debugjohn
            ns_cla_decs%capacity_market_pointer_acl(get_nunits())
            
       MW_INSIDE_FENCE_ord=MW_INSIDE_FENCE_ord ! Debugstop
       




!      ! After first array read
       name_of_file=" "



        inquire(unit=rdi_in_unit, file=name_of_file)

         elseIF(RDI_REC < RDI_RECORDS .AND. &
               PROCESSING_FIRST_DATA_FILE) THEN
               RDI_REC = RDI_REC + 1
               unit_is_active = 'T'

        inquire(unit=RDI_IN_UNIT, NAME=file_name)
        ! call write_log_entry("cla_objt:0017", &
       ! "Reading " // trim(just_the_filename(file_name)) // &
       ! " (unit " // trim(itos(int(RDI_IN_UNIT))) // " RDI_IN_UNIT")
        


               READ(RDI_IN_UNIT,REC=RDI_REC) &
                  del_ord, &
                  RDI_COMPANY_NAME, &
                  RDI_NERC_REGION, &
                  RDI_SUB_REGION, &
                  UNITNM(get_nunits()), &
                  RDI_PM_ABBREV, &
                  RDI_PRIME_FUEL, &
                  ON_LINE_YEAR, &
                  ON_LINE_MONTH, & !  INT
                  foshyd%CAP_FRAC_OWN(get_nunits()), &
                  RDI_DEMONSTRATED_CAPACITY_MW, &
                  RDI_INCREMENTAL_FUEL_COST_MWH, &
                  PBTUCT(get_nunits()), &
                  VCPMWH_IN(get_nunits()), &
                  FIXED_COST_IN(get_nunits()), &
                  RDI_AVERAGE_HEAT_RATE, &
                  RDI_MONTH_GENERATION, & !  12 VALUES
                  RDI_MONTH_CAPACITY ! 12 VALUES
               ! THIS PERMITS ONE BIN FILE
               IF(trim(RDI_PM_ABBREV) == 'HY') CYCLE

               RDI_CL_REC = RDI_CL_REC + 1

               CL_LOAD_TYPE = 'B '
               EXPENSE_ASSIGNMENT(get_nunits()) = 'F' !ossil Fuel
         EXPENSE_COLLECTION(get_nunits()) = 'A' !djustment Clause'
               GENGRP(get_nunits()) = 0
               OFF_LINE_MONTH(get_nunits()) = 12
               OFF_LINE_YEAR(get_nunits()) = 2050
               ON_LINE_MONTH = 1 ! HARD WIRED
         ! OWNERSHIP APPEARS TO BE ACCOUNTED FOR
         foshyd%CAP_FRAC_OWN(get_nunits()) = 100.0

               FUEL_MIX_PTR(get_nunits()) = 1.
               PFESCR(get_nunits()) = 0.
               SBTUCT(get_nunits()) = 0.
               SFESCR(get_nunits()) = 0
               FUELADJ(get_nunits()) = 0.

               IF(RDI_DEMONSTRATED_CAPACITY_MW < 100.) THEN
                  RDI_SIZE_INDEX = 1
                  IF(RDI_PRIME_FUEL(1:4) == 'COAL') THEN
                     RDI_FUEL_INDEX = 1
                  ELSEIF(RDI_PRIME_FUEL(1:3) == 'OIL') THEN
                     RDI_FUEL_INDEX = 2
                  ELSEIF(RDI_PRIME_FUEL(1:4) == 'URAN') THEN
                     RDI_FUEL_INDEX = 3
                  ELSEIF(RDI_PRIME_FUEL(1:3) == 'GAS') THEN
                     RDI_FUEL_INDEX = 4
                  ELSE ! LIGNITE
                     RDI_FUEL_INDEX = 5
                  ENDIF
               ELSEIF(RDI_DEMONSTRATED_CAPACITY_MW >= 100. .AND. &
                              RDI_DEMONSTRATED_CAPACITY_MW < 200.) THEN
                  RDI_SIZE_INDEX = 2
                  IF(RDI_PRIME_FUEL(1:4) == 'COAL') THEN
                     RDI_FUEL_INDEX = 1
                  ELSEIF(RDI_PRIME_FUEL(1:3) == 'OIL') THEN
                     RDI_FUEL_INDEX = 2
                  ELSEIF(RDI_PRIME_FUEL(1:4) == 'URAN') THEN
                     RDI_FUEL_INDEX = 3
                  ELSEIF(RDI_PRIME_FUEL(1:3) == 'GAS') THEN
                     RDI_FUEL_INDEX = 4
                  ELSE ! LIGNITE
                     RDI_FUEL_INDEX = 5
                  ENDIF
               ELSEIF(RDI_DEMONSTRATED_CAPACITY_MW >= 200. .AND. &
                              RDI_DEMONSTRATED_CAPACITY_MW < 300.) THEN
                  RDI_SIZE_INDEX = 3
                  IF(RDI_PRIME_FUEL(1:4) == 'COAL') THEN
                     RDI_FUEL_INDEX = 1
                  ELSEIF(RDI_PRIME_FUEL(1:3) == 'OIL') THEN
                     RDI_FUEL_INDEX = 2
                  ELSEIF(RDI_PRIME_FUEL(1:4) == 'URAN') THEN
                     RDI_FUEL_INDEX = 3
                  ELSEIF(RDI_PRIME_FUEL(1:3) == 'GAS') THEN
                     RDI_FUEL_INDEX = 4
                  ELSE ! LIGNITE
                     RDI_FUEL_INDEX = 5
                  ENDIF
               ELSEIF(RDI_DEMONSTRATED_CAPACITY_MW >= 300. .AND. &
                              RDI_DEMONSTRATED_CAPACITY_MW < 400.) THEN
                  RDI_SIZE_INDEX = 4
                  IF(RDI_PRIME_FUEL(1:4) == 'COAL') THEN
                     RDI_FUEL_INDEX = 1
                  ELSEIF(RDI_PRIME_FUEL(1:3) == 'OIL') THEN
                     RDI_FUEL_INDEX = 2
                  ELSEIF(RDI_PRIME_FUEL(1:4) == 'URAN') THEN
                     RDI_FUEL_INDEX = 3
                  ELSEIF(RDI_PRIME_FUEL(1:3) == 'GAS') THEN
                     RDI_FUEL_INDEX = 4
                  ELSE ! LIGNITE
                     RDI_FUEL_INDEX = 5
                  ENDIF
               ELSEIF(RDI_DEMONSTRATED_CAPACITY_MW >= 400. .AND. &
                              RDI_DEMONSTRATED_CAPACITY_MW < 600.) THEN
                  RDI_SIZE_INDEX = 5
                  IF(RDI_PRIME_FUEL(1:4) == 'COAL') THEN
                     RDI_FUEL_INDEX = 1
                  ELSEIF(RDI_PRIME_FUEL(1:3) == 'OIL') THEN
                     RDI_FUEL_INDEX = 2
                  ELSEIF(RDI_PRIME_FUEL(1:4) == 'URAN') THEN
                     RDI_FUEL_INDEX = 3
                  ELSEIF(RDI_PRIME_FUEL(1:3) == 'GAS') THEN
                     RDI_FUEL_INDEX = 4
                  ELSE ! LIGNITE
                     RDI_FUEL_INDEX = 5
                  ENDIF
               ELSEIF(RDI_DEMONSTRATED_CAPACITY_MW >= 600. .AND. &
                             RDI_DEMONSTRATED_CAPACITY_MW < 800.) THEN
                  RDI_SIZE_INDEX = 6
                  IF(RDI_PRIME_FUEL(1:4) == 'COAL') THEN
                     RDI_FUEL_INDEX = 1
                  ELSEIF(RDI_PRIME_FUEL(1:3) == 'OIL') THEN
                     RDI_FUEL_INDEX = 2
                  ELSEIF(RDI_PRIME_FUEL(1:4) == 'URAN') THEN
                     RDI_FUEL_INDEX = 3
                  ELSEIF(RDI_PRIME_FUEL(1:3) == 'GAS') THEN
                     RDI_FUEL_INDEX = 4
                  ELSE ! LIGNITE
                     RDI_FUEL_INDEX = 5
                  ENDIF
               ELSEIF(RDI_DEMONSTRATED_CAPACITY_MW >= 800. .AND. &
                            RDI_DEMONSTRATED_CAPACITY_MW < 1000.) THEN
                  RDI_SIZE_INDEX = 7
                  IF(RDI_PRIME_FUEL(1:4) == 'COAL') THEN
                     RDI_FUEL_INDEX = 1
                  ELSEIF(RDI_PRIME_FUEL(1:3) == 'OIL') THEN
                     RDI_FUEL_INDEX = 2
                  ELSEIF(RDI_PRIME_FUEL(1:4) == 'URAN') THEN
                     RDI_FUEL_INDEX = 3
                  ELSEIF(RDI_PRIME_FUEL(1:3) == 'GAS') THEN
                     RDI_FUEL_INDEX = 4
                  ELSE ! LIGNITE
                     RDI_FUEL_INDEX = 5
                  ENDIF
               ELSE ! > 1000 MW
                  RDI_SIZE_INDEX = 8
                  IF(RDI_PRIME_FUEL(1:4) == 'COAL') THEN
                     RDI_FUEL_INDEX = 1
                  ELSEIF(RDI_PRIME_FUEL(1:3) == 'OIL') THEN
                     RDI_FUEL_INDEX = 2
                  ELSEIF(RDI_PRIME_FUEL(1:4) == 'URAN') THEN
                     RDI_FUEL_INDEX = 3
                  ELSEIF(RDI_PRIME_FUEL(1:3) == 'GAS') THEN
                     RDI_FUEL_INDEX = 4
                  ELSE ! LIGNITE
                     RDI_FUEL_INDEX = 5
                  ENDIF
               ENDIF

      EFOR(get_nunits())= &
       GADS_FOR(RDI_SIZE_INDEX,RDI_FUEL_INDEX)*100.
               Mtnc_DAYS(get_nunits()) = &
                               GADS_MOR(RDI_SIZE_INDEX,RDI_FUEL_INDEX)

               MNRATE(get_nunits(),1) = 0.
               MNRATE(get_nunits(),2) = 0.
               MNRATE(get_nunits(),3) = 0.
               MNRATE(get_nunits(),4) = 0.
               MNRATE(get_nunits(),5) = 0.
               MNRATE(get_nunits(),6) = 0.
               MNRATE(get_nunits(),7) = 0.
               MNRATE(get_nunits(),8) = 0.
               MNRATE(get_nunits(),9) = 0.
               MNRATE(get_nunits(),10) = 0.
               MNRATE(get_nunits(),11) = 0.
               MNRATE(get_nunits(),12) = 0.
               OMESCR(get_nunits()) = 0.
               FIXED_COST_ESCALATOR(get_nunits()) = 0.
               DISPADJ(get_nunits()) = 0.
               HR_FACTOR(get_nunits()) = 1.0
      INPUT_MW(1,get_nunits()) = RDI_DEMONSTRATED_CAPACITY_MW
      INPUT_MW(2,get_nunits())=RDI_DEMONSTRATED_CAPACITY_MW
              foshyd%CAP_PLANNING_FAC(get_nunits()) = 1.0
              COEFF(1,get_nunits()) = RDI_AVERAGE_HEAT_RATE
              COEFF(2,get_nunits()) = RDI_AVERAGE_HEAT_RATE
              COEFF(3,get_nunits()) = RDI_AVERAGE_HEAT_RATE
               CL_AI_CAPACITY_RATE(get_nunits()) = 0.
              CL_AI_CAPACITY_ESCALATOR(get_nunits()) = 0
               CL_AI_ENERGY_RATE(get_nunits()) = 0.
               CL_AI_ENERGY_ESCALATOR(get_nunits()) = 0
               AI_CL_REMAINING_LIFE(get_nunits()) = 1
               P_SO2(get_nunits()) = 0.
               P_NOX(get_nunits()) = 0.
               P_PARTICULATES(get_nunits()) = 0.
               LOCAL_CL_POOL_FRAC_OWN(get_nunits()) = 100.
               P_EMIS_OTH2(get_nunits()) = 0.
               P_EMIS_OTH3(get_nunits()) = 0.
               PHASE_I_UNIT(get_nunits()) = .FALSE.
               DISPADJ2(get_nunits()) = 0.
               CL_RESOURCE_ID(get_nunits()) = 0
               FUEL_SUPPLY_ID(get_nunits()) = 0
               P_NOX_BK2(get_nunits()) = 0.
               SEC_FUEL_EMISS_PTR(get_nunits()) = 0
               EMISS_FUEL_COST(get_nunits()) =  0.
               EMISS_FUEL_ESCAL(get_nunits()) =  0
               EMISS_FUEL_EMISS_PTR(get_nunits()) = 0
               EMISS_BLENDING_RATE(get_nunits()) = 0.
               PRIM_FUEL_EMISS_PTR(get_nunits()) = 0
               IF(trim(RDI_PRIME_FUEL) == 'OIL-L') THEN
                  PRIM_ft_STR = 'Oil   '
                  SEC_FUEL_TYPE(get_nunits()) = 'O' !il   '
               ELSEIF(trim(RDI_PRIME_FUEL) == 'GAS') THEN
                  PRIM_ft_STR = 'Gas   '
                  SEC_FUEL_TYPE(get_nunits()) = 'G' !as   '
               ELSE
                  PRIM_ft_STR = 'Coal  '
                  SEC_FUEL_TYPE(get_nunits()) = 'C' !oal  '
               ENDIF
               EMISS_FUEL_TYPE(get_nunits()) = 'C' !oal  '
               TIE_CONSTRAINT_GROUP(get_nunits()) = 0.
               MONTHLY_CAPACITY_POINTER(get_nunits()) = 0.
               ANNUAL_CL_FIXED_COST(get_nunits()) = 0.
               ANNUAL_CL_FIXED_COST_ESC(get_nunits()) = 0.

               DISPATCH_MULT(get_nunits()) = 1.0
               EXCESS_ENERGY_SALES(get_nunits()) = 0.
               AI_CL_TAX_LIFE(get_nunits()) = 1
               AI_CL_ADR_LIFE(get_nunits()) = 1
               ASSET_CLS_NUM(get_nunits()) = 0
               ASSET_CLS_VECTOR(get_nunits()) = 0
               INTRA_COMPANY_CLSID(get_nunits()) = 0
              INTRA_CO_transaction_loc(get_nunits()) = 'N'
               RDI_DESC = trim(RDI_COMPANY_NAME)//':POWERDAT'
               SPL_unit_id_loc(get_nunits()) = 'NONE'
               MIN_CAP_FACTOR(get_nunits()) = 0.
               MAX_CAP_FACTOR(get_nunits()) = 1.
               TRANSACTION_GROUP_ID(get_nunits()) = 1
               dTYPE_ID(get_nunits()) = 0
! THIS CREATES A MIDAS DATA READABLE CL FILE
               WRITE(RDI_OUT_UNIT,1000) &
                  "1,", &
                  trim(UNITNM(get_nunits())), &
                  trim(CL_LOAD_TYPE), &
                  EXPENSE_ASSIGNMENT(get_nunits()), &
        EXPENSE_COLLECTION(get_nunits()), &
                  GENGRP(get_nunits()), &
       foshyd%CAP_FRAC_OWN(get_nunits()),ON_LINE_MONTH, &
                 ON_LINE_YEAR,OFF_LINE_MONTH(get_nunits()), &
       OFF_LINE_YEAR(get_nunits()), &
       FUEL_MIX_PTR(get_nunits()), &
       PBTUCT(get_nunits()), &
       PFESCR(get_nunits()), &
       SBTUCT(get_nunits()), &
                  SFESCR(get_nunits()), &
       FUELADJ(get_nunits()), &
       EFOR(get_nunits()), &
                  (MNRATE(get_nunits(),M),M=1,12), &
       VCPMWH_IN(get_nunits()), &
       OMESCR(get_nunits()), &
                  FIXED_COST_IN(get_nunits()), &
                  FIXED_COST_ESCALATOR(get_nunits()), &
                  DISPADJ(get_nunits()), &
                  HR_FACTOR(get_nunits()), &
                  (INPUT_MW(M,get_nunits()),M=1,2), &
                  foshyd%CAP_PLANNING_FAC(get_nunits()), &
                  (COEFF(M,get_nunits()),M=1,3), &
                  trim(RDI_DESC), &
                  P_SO2(get_nunits()), &
                  P_NOX(get_nunits()), &
                  P_PARTICULATES(get_nunits()), &
                  LOCAL_CL_POOL_FRAC_OWN(get_nunits()), &
                  P_EMIS_OTH2(get_nunits()), &
                  P_EMIS_OTH3(get_nunits()), &
                  PHASE_I_UNIT(get_nunits()), &
                  DISPADJ2(get_nunits()), &
                  CL_RESOURCE_ID(get_nunits()), &
                  FUEL_SUPPLY_ID(get_nunits()), &
                  P_NOX_BK2(get_nunits()), &
                  SEC_FUEL_EMISS_PTR(get_nunits()), &
                  EMISS_FUEL_COST(get_nunits()), &
                  EMISS_FUEL_ESCAL(get_nunits()), &
                  EMISS_FUEL_EMISS_PTR(get_nunits()), &
                  EMISS_BLENDING_RATE(get_nunits()), &
                  PRIM_FUEL_EMISS_PTR(get_nunits()), &
                  trim(PRIM_ft_STR), &
                  trim(SEC_FUEL_TYPE(get_nunits())), &
                  trim(EMISS_FUEL_TYPE(get_nunits())), &
                  TIE_CONSTRAINT_GROUP(get_nunits()), &
                  MONTHLY_CAPACITY_POINTER(get_nunits()), &
                  CL_AI_CAPACITY_RATE(get_nunits()), &
                  CL_AI_CAPACITY_ESCALATOR(get_nunits()), &
                  CL_AI_ENERGY_RATE(get_nunits()), &
                  CL_AI_ENERGY_ESCALATOR(get_nunits()), &
                  AI_CL_REMAINING_LIFE(get_nunits()), &
                  ANNUAL_CL_FIXED_COST(get_nunits()), &
                  ANNUAL_CL_FIXED_COST_ESC(get_nunits()), &
                  Mtnc_DAYS(get_nunits()), &
                  DISPATCH_MULT(get_nunits()), &
                  EXCESS_ENERGY_SALES(get_nunits()), &
                  AI_CL_TAX_LIFE(get_nunits()), &
                  AI_CL_ADR_LIFE(get_nunits()), &
                  ASSET_CLS_NUM(get_nunits()), &
                  ASSET_CLS_VECTOR(get_nunits()), &
                  INTRA_COMPANY_CLSID(get_nunits()), &
                  INTRA_CO_transaction_loc(get_nunits()), &
                  trim(SPL_unit_id_loc(get_nunits())), &
                  MIN_CAP_FACTOR(get_nunits()), &
                  MAX_CAP_FACTOR(get_nunits()), &
                  TRANSACTION_GROUP_ID(get_nunits()), &
                  dTYPE_ID(get_nunits())

 1000             FORMAT(  A,4('"',A,'"',','), &
                     I3,',',F9.2,',',4(I5,','), &
                     2(F9.2,','),I5,',',F9.2,',',I5,',', &
                     15(F9.2,','),I5,',', &
                     F9.2,',',I5,',',8(F9.2,','),'"',A,'"',',', &
                     6(F9.2,','), &
                     L1,',',F9.2,',',2(I5,','),F9.2,',',I5,',', &
                     F9.2,',',2(I5,','), &
                     F9.2,',',I5,',',3(A,','), &
                     F9.2,',',I5,',', & !CAP DISTN POINTER
                     F9.2,',',I5,',',F9.2,',',I5,',',I5,',', &
                     F9.2,',',I5,',', & !  FIXED COSTS
                     5(F9.2,','), &
                     3(I5,','),2(A,','),2(F9.2,','),2(I5,','))

            ELSE
               WRITE(4,*) "Untrapped error when processing"
               WRITE(4,*) "Capacity Limited NUNITS"
               WRITE(4,*) '*** CLA_OBJT.FOR ***'
               er_message='See WARNING MESSAGES-cla_objt-1'
               call end_program(er_message)
            ENDIF
            IF(IOS /= 0) then
                EXIT
            end if
            if(INDEX(SP_UNIT_NAME(get_nunits()),'Stryker') /= 0) then
               write(4,*) "stop here"
            endif

! CHECK FOR DUPLICATE NAME

! ^^^^        TODO:  CHECK FOR DUPLICATE NAME shouldn't be a
            ! comment. It should be a call. This code
            ! has nothing to do with the problem being solved.
            ! It should be a routine. It should have been a
            ! routine in 1977 too. Fix.
            UNIT_NAME = ADJUSTL(SP_UNIT_NAME(get_nunits()))

            DO ID = 1, get_nunits()-1
               IF(LEN_TRIM(SP_UNIT_NAME(ID)) == &
                                         LEN_TRIM(UNIT_NAME) .AND. &
                       INDEX(SP_UNIT_NAME(ID),UNIT_NAME) /= 0) THEN
                  WRITE(TEMP_UNIT_NAME,'(A,I2.2)') &
                           TRIM(UNIT_NAME),UNIT_NAME_COUNT(ID)
                  UNIT_NAME_COUNT(ID) = UNIT_NAME_COUNT(ID) + 1
                  WRITE(4,*) 'The unit name '// &
                            TRIM(UNIT_NAME)//' has been used before.'
                  WRITE(4,*) 'The name has been changed to '// &
                                                 TRIM(TEMP_UNIT_NAME)
                SP_UNIT_NAME(get_nunits()) = TEMP_UNIT_NAME
                  EXIT
               ENDIF
            ENDDO
! END UNIQUE NAME FIX
! End TODO: Check for duplicate name

            UNITNM(get_nunits()) = &
       SP_UNIT_NAME(get_nunits())
       call write_unitnm_trace("clun:0001", get_nunits(), &
        unitnm(get_nunits()))
       NG_UNIT_STATUS = &
       SP_NEWGEN_UNIT_STATUS(get_nunits())
            IF(INDEX(NG_UNIT_STATUS,"SPCapEx") /= 0) THEN
               ON_LINE_YEAR = 2000
               OFF_LINE_YEAR(get_nunits()) = 2200
               MW_INSIDE_FENCE_ord = 0.
            ENDIF

            IF(ZONAL_LEVEL_MARKET) THEN
          MARKET_ID = Bc_MARKET_AREA_ID(get_nunits())
               TRANSACTION_GROUP_ID(get_nunits()) = &
                                         MARKET_AREA_LOOKUP(MARKET_ID)
            ENDIF

            TRANSACTION_GROUP_ID(get_nunits()) = &
       GET_BELONGS_TO_GROUP( &
       TRANSACTION_GROUP_ID(get_nunits()))

            IF(INPUT_MW(2,get_nunits()) > 1. &
       .AND. MW_INSIDE_FENCE_ord > .1) THEN
               PERCENT_INSIDE_FENCE = MIN(1., &
       MW_INSIDE_FENCE_ord/INPUT_MW(2,get_nunits()))
            ELSE
               PERCENT_INSIDE_FENCE = 0.
            ENDIF

            IF(YES_AOD_PRICE_THERM_VALUE_LIST) THEN
               IF(.NOT. AOD_THERMAL_LIST_COMPARE( &
       HESI_UNIT_ID_NUMs(get_nunits()))) CYCLE
            ELSEIF(YES_AOD_COMP_THERM_VALUE_LIST) THEN
               IF(.NOT. &
                  AOD_THERMAL_LIST_COMPARE( &
                     HESI_UNIT_ID_NUMs(get_nunits()))) &
               THEN
                  ns_cla_decs%REPORT_THIS_UNIT(get_nunits()) = 'F'
               ENDIF
            ENDIF

            TRANS_ID = TRANSACTION_GROUP_ID(get_nunits())
            in_atma=IN_ACTIVE_THERMAL_MARKET_AREA( &
       Bc_MARKET_AREA_ID(get_nunits()))
       timeshere=timeshere+1
       ! Gets here
       tgas=logical(TRANS_GROUP_ACTIVE_SWITCH(TRANS_ID),1)
       if(timeshere==1580) then
        timeshere=timeshere ! Debugstop
       end if
            ! Percent inside fence should be zero here, first pass.
            IF(del_ord > 7 .OR. &
                   (RUN_TRANSACT .AND. &
                     (.NOT. tgas .OR. &
                      .NOT. in_atma)) .OR. &
                                  PERCENT_INSIDE_FENCE > .995) then
                 CYCLE ! believed to be exit point - john
            end if 
            CAP_MARKET_MONTH_NO(get_nunits()) = &
       CAP_MARKET_MONTH_NO_INDEX( &
       CAPACITY_MARKET_MTH(get_nunits()))

            IF(PERCENT_INSIDE_FENCE > .005) THEN
       foshyd%CAP_FRAC_OWN(get_nunits()) = &
       foshyd%CAP_FRAC_OWN(get_nunits()) * &
                                            (1.-PERCENT_INSIDE_FENCE)
            ENDIF

            IF(monte_or_frq(get_nunits()) == 'F') &
                                    UNIT_FREQUENCY_DURATION = .TRUE.

       IF(EMISSION_MARKET_LINK(get_nunits()) == -9999) THEN
         EMISSION_MARKET_LINK(get_nunits()) = &
       ASSET_CLS_NUM(get_nunits())
            ENDIF

            DO M = 2, 1 , -1
               IF(INPUT_MW(M,get_nunits()) < 0.) THEN
                  get_var_called_from="cla_objt:554"
         call write_trace_message(file_trace_cacm, "<<< " // &
            trim(get_var_called_from))
                  
                  LOCAL_MW(M) = &
                  GET_VAR(INPUT_MW(M,get_nunits()),INT2(1), &
                                      UNITNM(get_nunits()))
                  IF(LOCAL_MW(M) < 0.) THEN
                     WRITE(4,*) "Negative value for capacity"
             WRITE(4,*) "for unit ",UNITNM(get_nunits())
                     WRITE(4,*) " "
                  ENDIF
               ELSE
                  LOCAL_MW(M) = INPUT_MW(M,get_nunits())
               ENDIF
               IF(LOCAL_MW(M) < 1. .AND. M == 1) THEN
                  LOCAL_MW(1) = LOCAL_MW(2) * LOCAL_MW(1)
               ELSE
                  LOCAL_MW(M) = LOCAL_MW(M) * &
                        foshyd%CAP_FRAC_OWN(get_nunits())/100.
               ENDIF
            
               
            ENDDO



            IF(LOCAL_MW(1) > 0. .AND. LOCAL_MW(2) > 0.) THEN
               IF(USE_PLY_HEAT_RATES == 'T' .AND. &
                                           .NOT. TURN_OFF_POLY) THEN

! TOM TO ASSOCIATE VARIABLES HERE (AND IN NEW_CL_UNITS_READ).


                  COEFF(1,get_nunits()) = 1000. * &
                     (CONSTANT_POLY_ord + &
                      FIRST_POLY_ord * LOCAL_MW(1) + &
                      SECOND_POLY_ord * &
                                 LOCAL_MW(1)*LOCAL_MW(1) + &
                      THIRD_POLY_ord * &
                           LOCAL_MW(1)*LOCAL_MW(1)* &
                                                    LOCAL_MW(1))/ &
                                                      LOCAL_MW(1)
                  COEFF(2,get_nunits()) = 1000. * &
                      (FIRST_POLY_ord + &
                      2. * SECOND_POLY_ord * LOCAL_MW(1) + &
                      3. * THIRD_POLY_ord * &
                                  LOCAL_MW(1)*LOCAL_MW(1))
                  COEFF(3,get_nunits()) = 1000. * &
                      (FIRST_POLY_ord + &
                      2. * SECOND_POLY_ord * LOCAL_MW(2) + &
                      3. * THIRD_POLY_ord * &
                                  LOCAL_MW(2)*LOCAL_MW(2))

               ENDIF


! 1/17/03.
! A+BX+CX^2
! Doesn't get here.
               IF(LOCAL_MW(2) > LOCAL_MW(1)) THEN

                  CUBIC_HEAT_CURVE(1,get_nunits()) = &
       (COEFF(2,get_nunits())- &
       COEFF(3,get_nunits()))/ &
                           (-2.*(LOCAL_MW(2)-LOCAL_MW(1)))
                  CUBIC_HEAT_CURVE(2,get_nunits()) = &
                        COEFF(2,get_nunits()) - &
        (COEFF(2,get_nunits()) - &
       COEFF(3,get_nunits())) * &
                              LOCAL_MW(1)/ &
                           (-1.*(LOCAL_MW(2)-LOCAL_MW(1)))
                  CUBIC_HEAT_CURVE(3,get_nunits()) = &
         ((COEFF(1,get_nunits())- &
       CUBIC_HEAT_CURVE(2,get_nunits()))* &
                                     LOCAL_MW(1)) - &
                         CUBIC_HEAT_CURVE(1,get_nunits())* &
                                   LOCAL_MW(1)*LOCAL_MW(1)
               ELSE
               ENDIF
            ELSE ! NEED TO CHECK THIS WITH TOM.
               CUBIC_HEAT_CURVE(2,get_nunits()) = &
       COEFF(1,get_nunits())
            ENDIF

            CUBIC_HEAT_CURVE(0,get_nunits()) = CONSTANT_POLY_ord
            CUBIC_HEAT_CURVE(2,get_nunits()) = SECOND_POLY_ord
            CUBIC_HEAT_CURVE(3,get_nunits()) = THIRD_POLY_ord
            IF(CONSTANT_POLY_ord <= 0. .AND. FIRST_POLY_ord <= 0..AND. &
                             SECOND_POLY_ord <= 0.) THEN
               IF(COEFF(1,get_nunits()) > 0.) THEN
                  CUBIC_HEAT_CURVE(1,get_nunits()) = &
       COEFF(1,get_nunits())*.001
               ELSE
                  CUBIC_HEAT_CURVE(1,get_nunits()) = 10.
               ENDIF
            ELSE
              CUBIC_HEAT_CURVE(1,get_nunits()) = FIRST_POLY_ord
            ENDIF


! MOVE EXISTING UNIT RETIREMENT DATES IN THE EXTENSION PERIOD TO
! OUTSIDE THE PERIOD
! 08/25/03. MOVED UP FOR BURESH AND A.G. EDWARDS.

           ! Doesn't get here
           IF(OFF_LINE_YEAR(get_nunits()) > gc_last_study_year) &
               OFF_LINE_YEAR(get_nunits()) = 2200

            ONLINE(get_nunits()) = &
       100*(ON_LINE_YEAR - 1900) + ON_LINE_MONTH
       
            OFLINE(get_nunits()) = &
       100*(OFF_LINE_YEAR(get_nunits()) - 1900) + &
                            OFF_LINE_MONTH(get_nunits())
       ofline_unit_number=get_nunits()
       call write_offline_trace("cl_units_read_ext:0012", &
        ofline(ofline_unit_number))
        
            FIRST_RETIREMENT_YEAR(get_nunits()) = &
         100*(FIRST_RETIREMENT_YEAR(get_nunits()) - 1900)

! 05/13/03. NEW RESOURCE ADDITIONS LOGIC
! 05/16/03. MOVED INSIDE nunits LOOP TO AVOID ADDITIONS AND REMOVALS.
! 08/25/03. MOVED ABOVE nunits ACTIVE LOOP FOR BURESH AND A.G. EDWARDS.
            ! Doesn't get here

            IF(NG_UNIT_STATUS(1:2) == '01' ) THEN
               NEWGEN_INDEX(get_nunits()) = 1
            ELSEIF(NG_UNIT_STATUS(1:2) == '02' ) THEN
               NEWGEN_INDEX(get_nunits()) = 2
            ELSEIF(NG_UNIT_STATUS(1:2) == '03' ) THEN
               NEWGEN_INDEX(get_nunits()) = 3
            ELSEIF(NG_UNIT_STATUS(1:2) == '04' ) THEN
               NEWGEN_INDEX(get_nunits()) = 4
            ELSEIF(NG_UNIT_STATUS(1:2) == '05') THEN
               NEWGEN_INDEX(get_nunits()) = 5
            ELSEIF(NG_UNIT_STATUS(1:2) == '06' ) THEN
               NEWGEN_INDEX(get_nunits()) = 6
            ELSEIF(NG_UNIT_STATUS(1:2) == '07' ) THEN
               NEWGEN_INDEX(get_nunits()) = 7
            ELSEIF(NG_UNIT_STATUS(1:2) == '08' ) THEN
               NEWGEN_INDEX(get_nunits()) = 8
            ELSEIF(NG_UNIT_STATUS(1:2) == '09' ) THEN
               NEWGEN_INDEX(get_nunits()) = 9
            ELSEIF(NG_UNIT_STATUS(1:2) == '10' ) THEN
               NEWGEN_INDEX(get_nunits()) = 10
            ELSEIF(NG_UNIT_STATUS(1:2) == '11' ) THEN
               NEWGEN_INDEX(get_nunits()) = 11
            ELSEIF(NG_UNIT_STATUS(1:2) == '12' ) THEN
               NEWGEN_INDEX(get_nunits()) = 12
            ELSEIF(NG_UNIT_STATUS(1:2) == '13' ) THEN
               NEWGEN_INDEX(get_nunits()) = 13
            ELSE ! DEFAULT INDEX = 1
               NEWGEN_INDEX(get_nunits()) = 1
            ENDIF
            NEWGEN_COUNTER(NEWGEN_INDEX(get_nunits())) = &
            NEWGEN_COUNTER(NEWGEN_INDEX(get_nunits())) + 1

            IF(NEW_BUILD_ADDITIONS_ACTIVE .AND. &
       ONLINE(get_nunits()) > BEGIN_DATE) THEN
! (0.0,1.0) ASSUME UNIFORM DISTRIBUTION
               IRAN32_OUT = iran32()
               RANDOM_NUMBER = FLOAT(IRAN32_OUT)/FLOAT(2147483647)
               KEEP_UNIT = .TRUE.
               IF(    NEWGEN_INDEX(get_nunits()) == 2) THEN
                  IF(UNDER_CONSTRUCTION_PERCENT < RANDOM_NUMBER) THEN
                     KEEP_UNIT = .FALSE.
                  ENDIF
               ELSEIF(NEWGEN_INDEX(get_nunits()) == 3) THEN
                  IF(ADVANCED_DEVELOPMENT_PERCENT < RANDOM_NUMBER) THEN
                     KEEP_UNIT = .FALSE.
                  ENDIF
               ELSEIF(NEWGEN_INDEX(get_nunits()) == 4) THEN
                  IF(EARLY_DEVELOPMENT_PERCENT < RANDOM_NUMBER) THEN
                     KEEP_UNIT = .FALSE.
                  ENDIF
               ELSEIF(NEWGEN_INDEX(get_nunits()) == 5) THEN
                  IF(PROPOSED_PERCENT < RANDOM_NUMBER) THEN
                     KEEP_UNIT = .FALSE.
                  ENDIF
               ELSEIF(NEWGEN_INDEX(get_nunits()) == 7) THEN
                  IF(INDEFIN_POSTPHONED_PERCENT < RANDOM_NUMBER) THEN
                     KEEP_UNIT = .FALSE.
                  ENDIF
               ELSE
                  IF(unit_is_active == 'F') CYCLE
               ENDIF
               IF(.NOT. KEEP_UNIT) THEN
                  CYCLE
               ELSE
                  KEEP_UNIT = .TRUE.
               ENDIF
            ELSE

               IF(unit_is_active == 'F') CYCLE

            ENDIF

            CO2_CONTROL_DATE(get_nunits()) = &
       CO2_CONTROL_DATE(get_nunits()) + 10000
            IF(HardWiredRetrofitProject(get_nunits())) THEN
               RETIREMENT_CANDIDATE(get_nunits()) = 'F'
               FIRST_RETIREMENT_YEAR(get_nunits()) = 9999
               RETROFIT_CANDIDATE(get_nunits()) = 'F'
            ENDIF
           IF(RETROFIT_CANDIDATE(get_nunits()) /= 'F') THEN
               RETROFIT_COUNTER = RETROFIT_COUNTER + 1
               RETROFIT_UNIT_INDEX(RETROFIT_COUNTER) = &
       get_nunits()
            ENDIF
         IF(RETIREMENT_CANDIDATE(get_nunits()) /= 'F') THEN
               RETIREMENT_COUNTER = RETIREMENT_COUNTER + 1
            ENDIF

! 02/03/06.

            TEMP_STATE = st_pvnc(1:2)
            TEMP_I2 = STATE_ID_LOOKUP(TEMP_STATE)
            thermal_State_Index(get_nunits()) = TEMP_I2
            TEMP_I2 = STATE_ID_LOOKUP(st_pvnc)
            State_TG_Index(get_nunits()) = TEMP_I2
            IF( TEMP_I2 > 0 .AND. &
                               STATE_PROVINCE_INDEX(TEMP_I2) == 0) THEN
               MAX_STATE_PROVINCE_NO = MAX_STATE_PROVINCE_NO + 1
               STATE_PROVINCE_ADDRESS(MAX_STATE_PROVINCE_NO) = TEMP_I2
               STATE_PROVINCE_INDEX(TEMP_I2) = MAX_STATE_PROVINCE_NO
               STATE_PROVINCE_NAMES(MAX_STATE_PROVINCE_NO) = &
                                                         st_pvnc
            ENDIF
            UNIT_STATE_PROVINCE_INDEX(get_nunits()) = &
                                          STATE_PROVINCE_INDEX(TEMP_I2)



            TEMP_I2 = STATE_2_GAS_REGION_LOOKUP(st_pvnc)
            IF(YES_GSP_IS_ST_TG(TEMP_I2)) THEN
               TEMP_STATE = st_pvnc
            ELSE
               TEMP_STATE = st_pvnc(1:2)
            ENDIF
            UNIT_GAS_REGION_INDEX(get_nunits()) = &
                               STATE_2_GAS_REGION_LOOKUP(TEMP_STATE)

! 12/05/01
        ! Doesn't get here.
        IF(trim(SPL_unit_id_loc(get_nunits())) /= ' ' .AND. &
       trim(SPL_unit_id_loc(get_nunits())) /= 'None') THEN
       IF(trim(SPL_unit_id_loc(get_nunits())) == &
         'BayShore') THEN
                  FIRST_ENERGY = .TRUE.
               ENDIF
            ENDIF

            NOX_SsN_DATE(get_nunits()) =  &
       NOX_SsN_DATE(get_nunits()) + 10000

            NOX_CNTRL_PERCENT(get_nunits()) = &
                   MIN(100.,MAX(0.,1.- &
       NOX_CNTRL_PERCENT(get_nunits())/100.))
            NOX_CTL_DATE(get_nunits()) = &
       NOX_CTL_DATE(get_nunits()) + 10000

            SOX_CNTRL_PERCENT(get_nunits()) = &
                   MIN(100.,MAX(0.,1. &
       -SOX_CNTRL_PERCENT(get_nunits())/100.))
            SOX_CTL_DATE(get_nunits()) = &
       SOX_CTL_DATE(get_nunits()) + 10000

            CO2_CONTROL_PERCENT(get_nunits()) = &
                   MIN(100.,MAX(0.,1.-CO2_CONTROL_PERCENT( &
       get_nunits())/100.))
            HG_CNTRL_PERCENT(get_nunits()) = &
                   MIN(100.,MAX(0.,1.-HG_CNTRL_PERCENT( &
       get_nunits())/100.))
            HG_CTL_DATE(get_nunits()) = &
       HG_CTL_DATE(get_nunits()) + 10000
            OTHER3_CNTRL_PERCENT(get_nunits()) = &
                   MIN(100., &
      MAX(0.,1.-OTHER3_CNTRL_PERCENT(get_nunits())/100.))
            OTH3_CONTROL_DATE(get_nunits()) = &
               OTH3_CONTROL_DATE(get_nunits()) + 10000


            IF((SAVE_IGNORE_NON_UTILITY  == 'T' .AND. &
                UTILITY_OWNED_loc == 'N') .OR. &
               (SAVE_IGNORE_NON_UTILITY  == 'K' .AND. &
                UTILITY_OWNED_loc == 'N' .AND. &
                ON_LINE_YEAR-get_BASE_YEAR() < 1)  )  &
            then
                CYCLE
            end if

! THIS ESTABLISHES A UNIQUE UNIT ID VALUE FOR EACH RUN

      !Doesn't get here
      CL_UNIT_UNIQUE_RPT_ID(get_nunits()) = UNIQUE_ID_NUM

            CALL UPC(TEMP_PRIM_FUEL_TYPE_STR,PRIM_ft_STR)
            PRIMARY_MOVER(get_nunits()) = &
                           FUEL_TYPE_2_PRIM_MOVER(PRIM_ft_STR)
      ! doesn't get here
      CALL SET_ASSET_CLASSES(ASSET_CLS_NUM(get_nunits()), &
                                   NUMBER_OF_CAP_LIMITED_CLASSES, &
                         ns_cla_decs%MAX_CAP_LIMITED_CLASS_ID_NUM, &
                                   CAP_LIMITED_CLASS_POINTER)

            IF(TRANS_ID > 0 .AND. ON_LINE_YEAR <= gc_last_study_year) THEN
               DAY_ID = dTYPE_ID(get_nunits())
               MAX_TRANS_ID_USED = MAX(MAX_TRANS_ID_USED, TRANS_ID)
               IF(DAY_ID > 0 .AND. DAY_ID <= MAX_DAY_TYPES ) THEN
                 IF(DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,TRANS_ID)==0) THEN
                     MAX_TRANS_DATA_BASES = MAX_TRANS_DATA_BASES + 1
                     DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,TRANS_ID) = &
                                                   MAX_TRANS_DATA_BASES
                  ENDIF

                  DATA_BASE_POSITION(1,get_nunits()) = &
                             DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,TRANS_ID)
                  DATA_BASE_POSITION(2,get_nunits()) = &
                              DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,0)
               ELSEIF(DAY_ID < 0) THEN
                  DAY_TYPE_COUNTER = 1
                  DO
            DAY_ID=GET_VAR(dTYPE_ID(get_nunits()), &
                     DAY_TYPE_COUNTER,"FIND DAY TYPE")

                   IF(DAY_ID<= 0 .or. DAY_ID>MAX_DAY_TYPES) then
                    EXIT
                   endif
                   

       IF(int(DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,TRANS_ID)) &
        .eq. 0) THEN
                        MAX_TRANS_DATA_BASES = MAX_TRANS_DATA_BASES + 1
                        DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,TRANS_ID) = &
                                                   MAX_TRANS_DATA_BASES
                     ENDIF
      !
       DATA_BASE_POSITION(2*DAY_TYPE_COUNTER-1, &
         get_nunits()) = &
                            DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,TRANS_ID)
       DATA_BASE_POSITION( &
       2*DAY_TYPE_COUNTER,get_nunits())= &
                                    DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,0)
                     DAY_TYPE_COUNTER = DAY_TYPE_COUNTER + 1
                  ENDDO
               ELSEIF(DAY_ID == 0) THEN
                  DO M = 1, MAX_DAY_TYPES
                   IF(DAY_TYPE_TRANS_GROUP_PAIR(M,TRANS_ID) == 0) THEN
                        MAX_TRANS_DATA_BASES = MAX_TRANS_DATA_BASES + 1
                        DAY_TYPE_TRANS_GROUP_PAIR(M,TRANS_ID) = &
                                                   MAX_TRANS_DATA_BASES
                     ENDIF
                   DATA_BASE_POSITION(2*M-1,get_nunits()) = &
                                  DAY_TYPE_TRANS_GROUP_PAIR(M,TRANS_ID)
                     DATA_BASE_POSITION(2*M,get_nunits()) = &
                                         DAY_TYPE_TRANS_GROUP_PAIR(M,0)
                  ENDDO
               ELSE
                  WRITE(4,*) "DAY TYPE DESIGNATION",DAY_ID
                  WRITE(4,*) "OUTSIDE THE RANGE OF POSSIBLE OUTCOMES"
                WRITE(4,*)"MAXIMUM DAY TYPES DEFINED = ",MAX_DAY_TYPES
                  WRITE(4,*) '*** line 2056 CLA_OBJT.FOR ***'
                  er_message='See WARNING MESSAGES-cla_objt-7'
                  call end_program(er_message)
               ENDIF
            ENDIF

            IF(Mtnc_DAYS(get_nunits()) /= 0.) &
                                     SAVE_DETAILED_MAINTENANCE = .TRUE.
            DISP_ADDER_ESCR(get_nunits()) = &
       PFESCR(get_nunits())

           LOCAL_RESOURCE_ID = CL_RESOURCE_ID(get_nunits())
            IF(NUMBER_OF_RESOURCE_ID(LOCAL_RESOURCE_ID) == 0) THEN
               RESOURCE_ID_TO_UNIT(LOCAL_RESOURCE_ID) = &
       get_nunits() ! FIRST OCCURRENCE
            ENDIF
            NUMBER_OF_RESOURCE_ID(LOCAL_RESOURCE_ID) = &
                           NUMBER_OF_RESOURCE_ID(LOCAL_RESOURCE_ID) + 1

            IF(INPUT_MW(1,get_nunits()) == 0.) &
                                 INPUT_MW(1,get_nunits()) &
       = INPUT_MW(2,get_nunits())
            ECONOMY_TRANS_TYPE(get_nunits()) = 'T'
            LDTYPE(get_nunits()) = CL_LOAD_TYPE(1:1)
            PRIM_FUEL_TYPE(get_nunits()) = &
       PRIM_ft_STR(1:1)
            NUC_FUEL_PRICES_FROM_FUEL_FILE(get_nunits()) = &
                         INDEX(PRIM_ft_STR,'Nuc-NF') /= 0 .OR. &
                                INDEX(PRIM_ft_STR,'NUC-NF') /= 0
            IF(INDEX('123456',CL_LOAD_TYPE(2:2)) /= 0) THEN
               CL_CAP_AREA_LINKED(get_nunits()) = .TRUE.
               MONTHLY_CAPACITY_POINTER(get_nunits()) = &
                                     -INDEX('123456',CL_LOAD_TYPE(2:2))
               ECONOMY_TRANS_TYPE(get_nunits()) = 'N'
            ELSE
               CL_CAP_AREA_LINKED(get_nunits()) = .FALSE.
               IF(CL_LOAD_TYPE(2:2)=='S') then
                ECONOMY_TRANS_TYPE(get_nunits())='S'
               endif
               IF(CL_LOAD_TYPE(2:2)=='B') then
                ECONOMY_TRANS_TYPE(get_nunits())='B'
               endif

               IF(CL_LOAD_TYPE(2:2)=='N') then
                ECONOMY_TRANS_TYPE(get_nunits())='N'
               endif
            ENDIF
            ON_LINE_YEAR = MIN(MAX(ON_LINE_YEAR,1901),2200)
            OFF_LINE_YEAR(get_nunits()) = &
       MIN(OFF_LINE_YEAR(get_nunits()),2200)
! Doesn't get here
            IF(EFORS0) EFOR(get_nunits()) = 0.
! READ PLANNED MAINTENANCE RATES
            IF(SORS0) THEN
               Mtnc_DAYS(get_nunits()) = 0
               DO M = 1, 12
                  MNRATE(get_nunits(),M) = 0.
               ENDDO
            ENDIF

! CHECK IF LAST ENERGY SOURCE
! Doesn't get here
            IF(LDTYPE(get_nunits()) == 'L') THEN
               INPUT_MW(1,get_nunits()) = 0.
               INPUT_MW(2,get_nunits()) = 0.
               foshyd%CAP_PLANNING_FAC(get_nunits()) = 0.
               EFOR(get_nunits()) = 0.
               FUEL_SUPPLY_ID(get_nunits()) = 0
               DO M = 1 , 12
                  IF(MNRATE(get_nunits(),M) /= 100.) THEN
                    MNRATE(get_nunits(),M) = 0.
                  endif
               ENDDO
            ENDIF

! 9/15/95. GAT. FOR DUKE POWER.

            IF(LDTYPE(get_nunits()) == 'N'.AND. &
       EXPENSE_ASSIGNMENT(get_nunits())/='O' &
       .AND. EXPENSE_ASSIGNMENT(get_nunits()) /= 'L') THEN
               EXPENSE_ASSIGNMENT(get_nunits()) = 'O'
       WRITE(4,*) "Generating unit",UNITNM(get_nunits()), &
                         "was assigned"// &
                        " as nuclear without either an owned or leased"
               WRITE(4,*) "expense designation. This unit was "// &
                                    "reassigned as owned nuclear fuel."
               WRITE(4,*) " "
            ENDIF
            IF(LDTYPE(get_nunits()) == 'N') THEN
               DO M = 1, 12
                NUC_MAINTENANCE(M) = MNRATE(get_nunits(),M)
               ENDDO
               CALL CAL_GENERATING_INFORMATION( &
       CL_RESOURCE_ID(get_nunits()), &
                                foshyd%CAP_FRAC_OWN(get_nunits()), &
                                 INPUT_MW(2,get_nunits()), &
                     MONTHLY_CAPACITY_POINTER(get_nunits()), &
                                    EFOR(get_nunits()), &
                                    NUC_MAINTENANCE, &
                                    ONLINE(get_nunits()), &
                                    OFLINE(get_nunits()), &
                                    UNITNM(get_nunits()), &
                                    PRIM_ft_STR)
      IF(NUC_FUEL_PRICES_FROM_FUEL_FILE(get_nunits())) THEN
                  PBTUCT(get_nunits()) = 0.
                  SBTUCT(get_nunits()) = 0.
                  EMISS_FUEL_COST(get_nunits()) = 0.
      ENDIF
    ENDIF

! NEED TO SAVE PRICE INFORMATION
! Doesn't get here.
            PBTUCT_SAVE(get_nunits()) = &
       PBTUCT(get_nunits())
            SBTUCT_SAVE(get_nunits()) = &
       SBTUCT(get_nunits())
            FUELADJ_SAVE(get_nunits()) = &
       FUELADJ(get_nunits())
            VCPMWH_IN_SAVE(get_nunits()) = &
       VCPMWH_IN(get_nunits())
            FIXED_COST_SAVE(get_nunits()) = &
       FIXED_COST_IN(get_nunits())
            ANNUAL_CL_FIXED_COST_SAVE(get_nunits()) = &
                 ANNUAL_CL_FIXED_COST(get_nunits())
            DISPADJ_SAVE(get_nunits()) = &
       DISPADJ(get_nunits())
            DISPADJ2_SAVE(get_nunits()) = &
       DISPADJ2(get_nunits())
            DISPATCH_MULT_SAVE(get_nunits()) = &
       DISPATCH_MULT(get_nunits())
            EXCESS_ENERGY_SALES_SAVE(get_nunits()) = &
                       EXCESS_ENERGY_SALES(get_nunits())
            CL_AI_CAPACITY_RATE_SAVE(get_nunits()) = &
                        CL_AI_CAPACITY_RATE(get_nunits())
            CL_AI_ENERGY_RATE_SAVE(get_nunits()) = &
       CL_AI_ENERGY_RATE(get_nunits())
            CL_AI_REMAINING_LIFE_SAVE(get_nunits()) = &
                         AI_CL_REMAINING_LIFE(get_nunits())
            DISPADJ2_SAVE(get_nunits()) = &
       DISPADJ2(get_nunits())
            EMISS_FUEL_COST_SAVE(get_nunits()) = &
       EMISS_FUEL_COST(get_nunits())
       ! Doesn't get here
       IF(ON_LINE_YEAR <= OFF_LINE_YEAR(get_nunits()) .AND. &
               ON_LINE_YEAR-get_BASE_YEAR() <= get_globecom_study_period() .AND. &
        OFF_LINE_YEAR(get_nunits()) > get_BASE_YEAR()) THEN
               VOID_REAL = &
                      CL_CAPACITY_PLANNING_adj(ON_LINE_YEAR, &
                               get_nunits(),1,.FALSE.)

               SHADOW_UNIT_NUMBER(get_nunits()) = 0
               IF(FUEL_SUPPLY_ID(get_nunits())>0 .AND. &
                FUEL_MIX_PTR(get_nunits()) > 0. .AND. &
                 STARTup_LOGIC(get_nunits()) == 'F') THEN
                  I = get_nunits()
                  call set_nunits(get_nunits())
                  
                  !nunits counted here, but this code never gets run.
                  call set_nunits(int(I+1,2))
                  SHADOW_UNIT_NUMBER(int(get_nunits(),2)) = I
                  SHADOW_UNIT_NUMBER(I) = get_nunits()
                  UNITNM(get_nunits()) = '+'//UNITNM(I)
       call write_unitnm_trace("clun:0002", get_nunits(), &
        unitnm(get_nunits()))
                  
       CL_UNIT_UNIQUE_RPT_ID(get_nunits()) = UNIQUE_ID_NUM
                  FUEL_MIX_PTR(get_nunits()) = 0
                  LDTYPE(get_nunits()) = 'S'
                  EXPENSE_ASSIGNMENT(get_nunits()) = &
       EXPENSE_ASSIGNMENT(I)
                  EXPENSE_COLLECTION(get_nunits()) = &
       EXPENSE_COLLECTION(I)
                  ASSET_CLS_NUM(get_nunits()) = &
       ASSET_CLS_NUM(I)
       ASSET_CLS_VECTOR(get_nunits()) = &
       ASSET_CLS_VECTOR(I)
               INTRA_COMPANY_CLSID(get_nunits()) = &
                                           INTRA_COMPANY_CLSID(I)
                 INTRA_CO_transaction_loc(get_nunits()) = &
                                           INTRA_CO_transaction_loc(I)
       SPL_unit_id_loc(get_nunits()) = SPL_unit_id_loc(I)
                  MIN_CAP_FACTOR(get_nunits()) = &
                                             MIN_CAP_FACTOR(I)
                  MAX_CAP_FACTOR(get_nunits()) = &
                                             MAX_CAP_FACTOR(I)
                  TRANSACTION_GROUP_ID(get_nunits()) = &
       TRANSACTION_GROUP_ID(I)
                  ns_cla_decs%REPORT_THIS_UNIT(get_nunits()) = &
       ns_cla_decs%REPORT_THIS_UNIT(I)
                 dTYPE_ID(get_nunits()) = dTYPE_ID(I)
      ECONOMY_TRANS_TYPE(get_nunits()) = &
       ECONOMY_TRANS_TYPE(I)
                  CL_CAP_AREA_LINKED(get_nunits()) = &
       CL_CAP_AREA_LINKED(I)
                  GENGRP(get_nunits()) = GENGRP(I)
              foshyd%CAP_FRAC_OWN(get_nunits()) = foshyd%CAP_FRAC_OWN(I)
                  ONLINE(get_nunits()) = ONLINE(I)

                  OFLINE(get_nunits()) = OFLINE(I)
                  
       call write_offline_trace("cl_units_read_ext:0013",&
        ofline(get_nunits()))
                  
                  SBTUCT(get_nunits()) = SBTUCT(I)
                  SBTUCT(I) = 0.
                  SFESCR(get_nunits()) = SFESCR(I)
                  SFESCR(I) = 0
                  FUELADJ(get_nunits()) = FUELADJ(I)
                  EFOR(get_nunits()) = EFOR(I)
                  Mtnc_DAYS(get_nunits()) = Mtnc_DAYS(I)
                  DO M = 1, 12
                     MNRATE(get_nunits(),M) = MNRATE(I,M)
                  ENDDO
                  VCPMWH_IN(get_nunits()) = VCPMWH_IN(I)
                  OMESCR(get_nunits()) = OMESCR(I)
                  FIXED_COST_IN(get_nunits()) = 0.
                  ANNUAL_CL_FIXED_COST(get_nunits()) = 0.
                  FIXED_COST_ESCALATOR(get_nunits()) = 0.
                 ANNUAL_CL_FIXED_COST_ESC(get_nunits()) = 0.
                  DISPADJ(get_nunits()) = DISPADJ(I)
             DISPATCH_MULT(get_nunits()) = DISPATCH_MULT(I)
                  EXCESS_ENERGY_SALES(get_nunits()) = &
       EXCESS_ENERGY_SALES(I)
                  HR_FACTOR(get_nunits()) = HR_FACTOR(I)
                  INPUT_MW(1,get_nunits()) = INPUT_MW(1,I)
                  INPUT_MW(2,get_nunits()) = INPUT_MW(2,I)
                  COEFF(1,get_nunits()) = COEFF(1,I)
                  COEFF(2,get_nunits()) = COEFF(2,I)
                  COEFF(3,get_nunits()) = COEFF(3,I)
                  CL_AI_CAPACITY_RATE(get_nunits()) = &
       CL_AI_CAPACITY_RATE(I)
                  CL_AI_CAPACITY_ESCALATOR(get_nunits()) = &
                                            CL_AI_CAPACITY_ESCALATOR(I)
                  CL_AI_ENERGY_RATE(get_nunits()) = &
       CL_AI_ENERGY_RATE(I)
                  CL_AI_ENERGY_ESCALATOR(get_nunits()) = &
                                              CL_AI_ENERGY_ESCALATOR(I)
                  AI_CL_REMAINING_LIFE(get_nunits()) = &
       AI_CL_REMAINING_LIFE(I)
           AI_CL_TAX_LIFE(get_nunits()) = AI_CL_TAX_LIFE(I)
            AI_CL_ADR_LIFE(get_nunits()) = AI_CL_ADR_LIFE(I)
                  SEC_FUEL_EMISS_PTR(get_nunits()) = &
       SEC_FUEL_EMISS_PTR(I)
                  SEC_FUEL_EMISS_PTR(I) = 0
           CL_RESOURCE_ID(get_nunits()) = CL_RESOURCE_ID(I)
                  DISPADJ2(get_nunits()) = DISPADJ2(I)
             PHASE_I_UNIT(get_nunits()) = PHASE_I_UNIT(I)
                  LOCAL_CL_POOL_FRAC_OWN(get_nunits()) = &
                                              LOCAL_CL_POOL_FRAC_OWN(I)

! SINCE ONLY THE SECONDARY FUEL IS USED
! ZERO PRIMARLY AND EMISSIONS FUEL
! DATA

          FUEL_SUPPLY_ID(get_nunits()) = FUEL_SUPPLY_ID(I)
                  foshyd%CAP_PLANNING_FAC(get_nunits()) = 0.
                  PBTUCT(get_nunits()) = PBTUCT(I)
                  PFESCR(get_nunits()) = PFESCR(I)
         DISP_ADDER_ESCR(get_nunits()) = DISP_ADDER_ESCR(I)
                  P_SO2(get_nunits()) = P_SO2(I)
                  P_NOX(get_nunits()) = P_NOX(I)
           P_PARTICULATES(get_nunits()) = P_PARTICULATES(I)
               P_EMIS_OTH2(get_nunits()) = P_EMIS_OTH2(I)
                 P_EMIS_OTH3(get_nunits()) = P_EMIS_OTH3(I)
                  P_NOX_BK2(get_nunits()) = P_NOX_BK2(I)
      EMISS_FUEL_COST(get_nunits()) = EMISS_FUEL_COST(I)
      EMISS_FUEL_ESCAL(get_nunits()) = EMISS_FUEL_ESCAL(I)
                  EMISS_FUEL_EMISS_PTR(get_nunits()) = 0.
      EMISS_BLENDING_RATE(get_nunits()) = &
       EMISS_BLENDING_RATE(I)
                  PRIM_FUEL_EMISS_PTR(get_nunits()) = &
       PRIM_FUEL_EMISS_PTR(I)
! 6/10/93. GAT. NO ALLOCATION OF FUEL TYPES BETWEEN SHADOWS
                  PRIM_FUEL_TYPE(get_nunits()) = " "
       NUC_FUEL_PRICES_FROM_FUEL_FILE(get_nunits()) = &
       .FALSE.
           SEC_FUEL_TYPE(get_nunits()) = SEC_FUEL_TYPE(I)
                  SEC_FUEL_TYPE(I) = " "
                  EMISS_FUEL_TYPE(get_nunits()) = " "
                  TIE_CONSTRAINT_GROUP(get_nunits()) = &
       TIE_CONSTRAINT_GROUP(I)
                  PBTUCT_SAVE(get_nunits()) = PBTUCT(I)

                  SBTUCT_SAVE(get_nunits()) = &
       SBTUCT(get_nunits())
                  FUELADJ_SAVE(get_nunits()) = FUELADJ(I)
                VCPMWH_IN_SAVE(get_nunits()) = VCPMWH_IN(I)
           FIXED_COST_SAVE(get_nunits()) = FIXED_COST_IN(I)
                 ANNUAL_CL_FIXED_COST_SAVE(get_nunits()) = &
                                                ANNUAL_CL_FIXED_COST(I)
       DISPATCH_MULT_SAVE(get_nunits()) = DISPATCH_MULT(I)
                 EXCESS_ENERGY_SALES_SAVE(get_nunits()) = &
                                                 EXCESS_ENERGY_SALES(I)
                  MONTHLY_CAPACITY_POINTER(get_nunits()) = &
                                            MONTHLY_CAPACITY_POINTER(I)
                  DISPADJ_SAVE(get_nunits()) = DISPADJ(I)
                  DISPADJ2_SAVE(get_nunits()) = DISPADJ2(I)
                  CL_AI_CAPACITY_RATE_SAVE(get_nunits()) = &
                                                 CL_AI_CAPACITY_RATE(I)
                  CL_AI_ENERGY_RATE_SAVE(get_nunits()) = &
       CL_AI_ENERGY_RATE(I)
                 CL_AI_REMAINING_LIFE_SAVE(get_nunits()) = &
                                                AI_CL_REMAINING_LIFE(I)
                  DISPADJ2_SAVE(get_nunits()) = DISPADJ2(I)
       EMISS_FUEL_COST_SAVE(get_nunits()) = &
        EMISS_FUEL_COST(I)

             TRANS_ID = TRANSACTION_GROUP_ID(get_nunits())

                  IF(TRANS_ID > 0 .AND. &
                                  ON_LINE_YEAR <= gc_last_study_year) THEN
                     DAY_ID = dTYPE_ID(get_nunits())
                    MAX_TRANS_ID_USED = MAX(MAX_TRANS_ID_USED,TRANS_ID)
                    IF(DAY_ID > 0 .AND. DAY_ID <= MAX_DAY_TYPES ) THEN
                        IF(DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,TRANS_ID) &
                                                             == 0) THEN
                          MAX_TRANS_DATA_BASES = MAX_TRANS_DATA_BASES+1
                         DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,TRANS_ID) = &
                                                  MAX_TRANS_DATA_BASES
                        ENDIF

                       DATA_BASE_POSITION(1,get_nunits()) = &
                             DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,TRANS_ID)
                       DATA_BASE_POSITION(2,get_nunits()) = &
                                    DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,0)
                     ELSEIF(DAY_ID < 0) THEN
                        DAY_TYPE_COUNTER = 1
                        DO
                           DAY_ID = &
            INT(GET_VAR(FLOAT(dTYPE_ID(get_nunits())), &
                                    DAY_TYPE_COUNTER,"FIND DAY TYPE"))
                           IF(DAY_ID <= 0 .OR. &
                                          DAY_ID > MAX_DAY_TYPES) EXIT
                        IF(DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,TRANS_ID) &
                                                            == 0) THEN
                              MAX_TRANS_DATA_BASES = &
                                               MAX_TRANS_DATA_BASES + 1
                              DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID, &
                                       TRANS_ID) = MAX_TRANS_DATA_BASES
                           ENDIF
      !
                           DATA_BASE_POSITION( &
                     2*DAY_TYPE_COUNTER-1,get_nunits()) = &
                             DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,TRANS_ID)
                           DATA_BASE_POSITION(2*DAY_TYPE_COUNTER, &
          get_nunits())=DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,0)
                           DAY_TYPE_COUNTER = DAY_TYPE_COUNTER + 1
                        ENDDO
                     ELSEIF(DAY_ID == 0) THEN
                        DO M = 1, MAX_DAY_TYPES
                           IF(DAY_TYPE_TRANS_GROUP_PAIR(M,TRANS_ID) &
                                                             == 0) THEN
                              MAX_TRANS_DATA_BASES = &
                                              MAX_TRANS_DATA_BASES + 1
                              DAY_TYPE_TRANS_GROUP_PAIR(M,TRANS_ID) = &
                                                  MAX_TRANS_DATA_BASES
                           ENDIF
                   DATA_BASE_POSITION(2*M-1,get_nunits()) = &
                                  DAY_TYPE_TRANS_GROUP_PAIR(M,TRANS_ID)
                     DATA_BASE_POSITION(2*M,get_nunits()) = &
                                      DAY_TYPE_TRANS_GROUP_PAIR(M,0)
                        ENDDO
                     ELSE
                        WRITE(4,*) "DAY TYPE DESIGNATION",DAY_ID
                        WRITE(4,*) &
                              "OUTSIDE THE RANGE OF POSSIBLE OUTCOMES"
                        WRITE(4,*) &
                          "MAXIMUM DAY TYPES DEFINED = ",MAX_DAY_TYPES
                        WRITE(4,*) '*** line 2391 CLA_OBJT.FOR ***'
                        er_message='See WARNING MESSAGES-cla_objt-6'
                        call end_program(er_message)
                     ENDIF
                  ENDIF

               ENDIF ! SHADOW UNIT

               IF(STARTup_LOGIC(get_nunits()) /= 'F') THEN
                  TOTAL_START_UP_UNITS = TOTAL_START_UP_UNITS + 1
       START_UP_INDEX(get_nunits()) = TOTAL_START_UP_UNITS
               ENDIF

               call set_nunits(int(get_nunits() + 1,2))

               
            ENDIF ! CONTRIBUTES TO CAPACITY PLANNING
            IF(get_nunits() > MAX_CL_UNITS) THEN

               WRITE(SCREEN_MESSAGES,'(A,I5,A)') &
                                           'More than ',MAX_CL_UNITS, &
                                              ' capacity-limited'
               CALL MG_LOCATE_WRITE(20,0,trim(SCREEN_MESSAGES), &
                                                        ALL_VERSIONS,1)
               CALL MG_LOCATE_WRITE(21,0, &
                            'UNITS are in the capacity-limited', &
                                                        ALL_VERSIONS,1)
               CALL MG_LOCATE_WRITE(22,0, &
                                   'file after expansion planning.', &
                                                        ALL_VERSIONS,1)
               er_message='stop requested from Cla_objt SIID24'
               call end_program(er_message)
            ENDIF
         ENDDO ! PROCESSING WITHIN A FILE
         CLOSE(10,IOSTAT=IOS)
         IF(RDI_CL_RECORDS > 0 .AND. PROCESSING_FIRST_DATA_FILE) THEN
            CALL CLOSE_BOTH_RDI_FILES
         ENDIF
      ENDDO ! CL FILES LOOP

      call set_nunits(int(get_nunits() - 1,2))

      
      BASE_CL_UNITS = get_nunits()
      AVAILABLE_CL_UNITS = get_nunits()
      BASE_PLUS_HARDWIRED_CL_UNITS = get_nunits()
      CL_UNITS_READ = get_nunits()
! TO GENERATE MONTHLY CAPACITIES
      IF(get_nunits() > 0) THEN
         SAVE_DETAILED_MAINTENANCE = .TRUE.
      else
        call end_program("cl_units_read:0001 - cl_units_read is " // &
            "returning zero.")
      ENDIF
      RETIRE_RETRO_COUNTER = RETROFIT_COUNTER + RETIREMENT_COUNTER
      IF(ALLOCATED(RETIRE_RETRO_CO2_PRICE)) &
            DEALLOCATE(RETIRE_RETRO_CO2_PRICE, &
                       RETIRE_RETRO_UPLIFT_PER_TON, &
                       RETIRE_RETRO_CUM_CO2, &
                       NEW_UNIT_RETIRE_VALUE_BY_CM, &
                       RETIRE_RETRO_INDEX, &
                       RETIRE_OR_RETRO, &
                       RETIRE_RETRO_POSITION, &
                       RETIRE_RETRO_ABATE_INDEX)
      ALLOCATE(RETIRE_RETRO_CO2_PRICE(RETIRE_RETRO_COUNTER), &
               RETIRE_RETRO_UPLIFT_PER_TON(RETIRE_RETRO_COUNTER), &
               RETIRE_RETRO_CUM_CO2(0:RETIRE_RETRO_COUNTER), &
               NEW_UNIT_RETIRE_VALUE_BY_CM(UPPER_TRANS_Gp), &
               RETIRE_RETRO_INDEX(RETIRE_RETRO_COUNTER), &
               RETIRE_OR_RETRO(RETIRE_RETRO_COUNTER), &
               RETIRE_RETRO_POSITION(RETIRE_RETRO_COUNTER), &
               RETIRE_RETRO_ABATE_INDEX(RETIRE_RETRO_COUNTER))


      RETURN


      ENTRY INIT_CLA_UNIT_UP_YESTERDAY()


! CALLED FROM PRO_COST, ONCE PER ENDPOINT

         CLA_UNIT_UP_YESTERDAY = .TRUE. ! MAX_CL_UNITS

         INIT_CLA_UNIT_UP_YESTERDAY = .TRUE.

      RETURN

      ENTRY GET_CLA_UNIT_UP_YESTERDAY(r_nunits_loc)


! CALLED FROM TF_OBJT2, ONCE PER MONTH/TG/UNIT

         GET_CLA_UNIT_UP_YESTERDAY = CLA_UNIT_UP_YESTERDAY(r_nunits_loc)

      RETURN

      ENTRY PUT_CLA_UNIT_UP_YESTERDAY(r_nunits_loc,R_LOGICAL)


! CALLED FROM TF_OBJT2, ONCE PER MONTH/TG/UNIT

         PUT_CLA_UNIT_UP_YESTERDAY = R_LOGICAL
         CLA_UNIT_UP_YESTERDAY(r_nunits_loc) = R_LOGICAL

      RETURN

      ENTRY GET_STATE_PROVINCE_NAMES(R_STATE_PROVINCE,R_I)

         R_STATE_PROVINCE = STATE_PROVINCE_NAMES(R_I)
         GET_STATE_PROVINCE_NAMES = 1
      RETURN

      ENTRY GET_UNIT_STATE_PROVINCE_INDEX(r_nunits_loc)

         GET_UNIT_STATE_PROVINCE_INDEX = &
                                 UNIT_STATE_PROVINCE_INDEX(r_nunits_loc)
      RETURN

      ENTRY GET_UNIT_GAS_REGION_INDEX(r_nunits_loc)

         GET_UNIT_GAS_REGION_INDEX = UNIT_GAS_REGION_INDEX(r_nunits_loc)
      RETURN

      ENTRY GET_MAX_STATE_PROVINCE_NO

         GET_MAX_STATE_PROVINCE_NO = MAX_STATE_PROVINCE_NO
      RETURN

      ENTRY GET_MIN_SPIN_CAP(r_nunits_loc)

         GET_MIN_SPIN_CAP = MIN_SPIN_Cpcty(r_nunits_loc)
      RETURN

      ENTRY GET_MAX_SPIN_CAP(r_nunits_loc)

         GET_MAX_SPIN_CAP = MAX_SPN_CAP(r_nunits_loc)
      RETURN

      ENTRY YES_ALLOW_DECOMMIT_BY_UNIT(r_nunits_loc,R_MONTH)

         IF(ALLOW_DcmT(r_nunits_loc) == 'T') THEN
            YES_ALLOW_DECOMMIT_BY_UNIT = .TRUE.
         ELSEIF(ALLOW_DcmT(r_nunits_loc) == 'M') THEN
            TEMP_R4 = GET_VAR( &
                   FLOAT(MONTHLY_dcmt_VECOT(r_nunits_loc)),R_MONTH, &
                                                     UNITNM(r_nunits_loc))
            IF(TEMP_R4 > 0.) THEN
               YES_ALLOW_DECOMMIT_BY_UNIT = .TRUE.
            ELSE
               YES_ALLOW_DECOMMIT_BY_UNIT = .FALSE.
            ENDIF
         ELSE
            YES_ALLOW_DECOMMIT_BY_UNIT = .FALSE.
         ENDIF
      RETURN

      ENTRY EXPENSE_ASSIGNMENT_IS_PURCHASE(r_nunits_loc)

         EXPENSE_ASSIGNMENT_IS_PURCHASE = &
                                    EXPENSE_ASSIGNMENT(r_nunits_loc) == 'P'
      RETURN

      ENTRY GET_THERMAL_TRACKER_INDEX(r_nunits_loc)


         GET_THERMAL_TRACKER_INDEX = &
                        WVPA_TRACKING_TYPE(wvpa_rt_tckr(r_nunits_loc))

      RETURN

      ENTRY GET_THERMAL_RES_TRACKER_INDEX(r_nunits_loc)


         GET_THERMAL_RES_TRACKER_INDEX = &
                WVPA_RESOURCE_TRACKING_TYPE(wvpa_res_trkr(r_nunits_loc))

      RETURN

      ENTRY GET_THERMAL_FUEL_TRACKER_INDEX(r_nunits_loc)


         GET_THERMAL_FUEL_TRACKER_INDEX = &
                   WVPA_FUEL_TRACKING_TYPE(WVPA_fl_TCKR_loc(r_nunits_loc))

      RETURN

      ENTRY GET_THERMAL_MEM_TRACKER_INDEX(r_nunits_loc)


         GET_THERMAL_MEM_TRACKER_INDEX = &
                    WVPA_MEM_TRACKING_TYPE(WVPA_MM_TRACKER(r_nunits_loc))

      RETURN

      ENTRY GET_EMISSION_MARKET_LINK(r_nunits_loc)

         GET_EMISSION_MARKET_LINK = EMISSION_MARKET_LINK(r_nunits_loc)
      RETURN

      ENTRY TEST_MONTHLY_MUST_RUN(r_nunits_loc,R_MONTH)
         call write_trace_message(file_trace_cacm, "TEST_MONTHLY_MUST_RUN()")
         call write_trace_int2(file_trace_cacm, "NUNITS", r_nunits_loc)
         

         TEST_MONTHLY_MUST_RUN = .FALSE.
         IF(MONTHLY_MUST_RUN_vctr(r_nunits_loc) /= 0) THEN
! FIND THE BLKNO
            DO I = 1, NBLOK
               IF(UNIT(I) /= r_nunits_loc) CYCLE
               IF(BLKNO(I) > 1) CYCLE ! THIS SHOULD HAPPEN.
               get_var_called_from="cl_units_read_ext:2822"
                call write_trace_message(file_trace_cacm, "<<< " // &
                    trim(get_var_called_from))
               TEMP_R4 = GET_VAR( &
                    FLOAT(MONTHLY_MUST_RUN_vctr(r_nunits_loc)),R_MONTH, &
                                                      UNITNM(r_nunits_loc))
               IF(TEMP_R4 > 0.) THEN
                  BLKNO(I) = 0
                  TEST_MONTHLY_MUST_RUN = .TRUE.
               ELSE
                  BLKNO(I) = 1
               ENDIF
               EXIT

            ENDDO
         ELSE

         ENDIF
      RETURN

      ENTRY GET_CUBIC_HEAT_CURVE(r_nunits_loc,R_A_CO,R_B_CO,R_C_CO, &
        R_D_CO)

         GET_CUBIC_HEAT_CURVE = .TRUE.
         R_A_CO = CUBIC_HEAT_CURVE(0,r_nunits_loc)
         R_B_CO = CUBIC_HEAT_CURVE(1,r_nunits_loc)
         R_C_CO = CUBIC_HEAT_CURVE(2,r_nunits_loc)
         R_D_CO = CUBIC_HEAT_CURVE(3,r_nunits_loc)
      RETURN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ENTRY GET_PW_UNIT_TEXT_FIELDS(r_nunits_loc, &
                                    R_EIA_PLANT_CODE)

 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         GET_PW_UNIT_TEXT_FIELDS = .TRUE.
         R_EIA_PLANT_CODE = Bc_PLANT_ID(r_nunits_loc)

       RETURN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ENTRY GET_EMERGENCY_CAPACITY(r_nunits_loc,R_EMERGENCY_HEATRATE)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         GET_EMERGENCY_CAPACITY = EMRGCY_CAPACITY(r_nunits_loc)
         R_EMERGENCY_HEATRATE = EMRGENCY_HEATRATE(r_nunits_loc)
      RETURN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ENTRY NOX_ACTIVE_FOR_UNIT(r_nunits_loc,R_DATE, &
                                R_SEASON_IS_NOX_SEASON, &
                                R_CALCULATE_NOX)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         NOX_ACTIVE_FOR_UNIT = .TRUE.
         IF(APPLY_Nx_SEASON_DATE(r_nunits_loc) == 'T') THEN
            IF(R_DATE >= NOX_SsN_DATE(r_nunits_loc) .AND. &
                                           R_SEASON_IS_NOX_SEASON) THEN
               R_CALCULATE_NOX = .TRUE.
            ELSE
               R_CALCULATE_NOX = .FALSE.
            ENDIF
         ENDIF
      RETURN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ENTRY GET_NOX_CONTROL_FOR_UNIT( &
                                r_nunits_loc, &
                                R_DATE, &
                                R_NOX_CONTROL_PERCENT)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         GET_NOX_CONTROL_FOR_UNIT = .FALSE.
         R_NOX_CONTROL_PERCENT = 1.0
         IF(R_DATE >= NOX_CTL_DATE(r_nunits_loc)) THEN
            GET_NOX_CONTROL_FOR_UNIT = .TRUE.
            R_NOX_CONTROL_PERCENT = NOX_CNTRL_PERCENT(r_nunits_loc)
         ENDIF
      RETURN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ENTRY SET_SOX_CONTROL_COAL_LP(r_nunits_loc, &
                                    R_DATE, &
                                    R_SOX_CONTROL_PERCENT)
        SET_SOX_CONTROL_COAL_LP = .TRUE.
        SOX_CONTROL_DATE_TEMP(r_nunits_loc) =  SOX_CTL_DATE(r_nunits_loc)
        SOX_CONTROL_PERCENT_TEMP(r_nunits_loc) = &
                                          SOX_CNTRL_PERCENT(r_nunits_loc)
        IF(R_DATE < SOX_CTL_DATE(r_nunits_loc)) THEN
           SOX_CTL_DATE(r_nunits_loc) =  R_DATE
           SOX_CNTRL_PERCENT(r_nunits_loc) = R_SOX_CONTROL_PERCENT
        ENDIF
      RETURN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ENTRY RESET_SOX_CONTROL_COAL_LP(r_nunits_loc)
        RESET_SOX_CONTROL_COAL_LP = .FALSE.
        SOX_CTL_DATE(r_nunits_loc) =  SOX_CONTROL_DATE_TEMP(r_nunits_loc)
        SOX_CNTRL_PERCENT(r_nunits_loc) = &
                                     SOX_CONTROL_PERCENT_TEMP(r_nunits_loc)
      RETURN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ENTRY GET_SOX_CONTROL_FOR_UNIT( &
                                r_nunits_loc, &
                                R_DATE, &
                                R_SOX_CONTROL_PERCENT)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         GET_SOX_CONTROL_FOR_UNIT = .FALSE.
         R_SOX_CONTROL_PERCENT = 1.0
         IF(R_DATE >= SOX_CTL_DATE(r_nunits_loc)) THEN
            GET_SOX_CONTROL_FOR_UNIT = .TRUE.
            R_SOX_CONTROL_PERCENT = SOX_CNTRL_PERCENT(r_nunits_loc)
         ENDIF
      RETURN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ENTRY GET_CO2_CONTROL_FOR_UNIT( &
                                r_nunits_loc, &
                                R_DATE, &
                                R_CO2_CONTROL_PERCENT, &
                                R_CO2_FIRST_CONTROL_MONTH, &
                                R_CO2_FIRST_CONTROL_YEAR, &
                                R_CO2_YEAR_BEFORE_CONTROL)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         GET_CO2_CONTROL_FOR_UNIT = .FALSE.
         R_CO2_FIRST_CONTROL_MONTH = .FALSE.
         R_CO2_YEAR_BEFORE_CONTROL = .FALSE.
         R_CO2_CONTROL_PERCENT = 1.0
         R_CO2_FIRST_CONTROL_YEAR = .FALSE.
         IF(R_DATE == CO2_CONTROL_DATE(r_nunits_loc) - 100) THEN
            R_CO2_YEAR_BEFORE_CONTROL = .TRUE.
         ENDIF
         IF(R_DATE >= CO2_CONTROL_DATE(r_nunits_loc)) THEN
            GET_CO2_CONTROL_FOR_UNIT = .TRUE.
            R_CO2_CONTROL_PERCENT = CO2_CONTROL_PERCENT(r_nunits_loc)
            IF(R_DATE == CO2_CONTROL_DATE(r_nunits_loc)) THEN
               R_CO2_FIRST_CONTROL_MONTH = .TRUE.
            ENDIF
            IF(R_DATE/100 == CO2_CONTROL_DATE(r_nunits_loc)/100) THEN
               R_CO2_FIRST_CONTROL_YEAR = .TRUE.
            ENDIF
         ENDIF
      RETURN

! 102909. TO GET MRX TO ADD CAPACITY CORRECTLY

      ENTRY ADJUST_CO2_RETRO_PLAN_CAP(R_YEAR)


         ADJUST_CO2_RETRO_PLAN_CAP = .TRUE.
         CURRENT_YEAR = get_BASE_YEAR() + R_YEAR
         BASE_DATE = (CURRENT_YEAR - 1900) * 100

         DO I = 1, get_nunits()

            IF(BASE_DATE/100 >= CO2_CONTROL_DATE(I)/100) THEN
               RETRO_CAP_CO2_MULT = CO2_RTRO_CAP_MULT(I)
! RETIRE CURRENT CAPACITY
               TEMP_R4 = CL_CAPACITY_PLANNING_adj( &
                                         CURRENT_YEAR,I,2,.TRUE.)

               IF(INPUT_MW(2,I) < -.01) THEN
! NEED TO 'POKE' A NEW CAPACITY
               ELSE
                  INPUT_MW(2,I) = INPUT_MW(2,I) * RETRO_CAP_CO2_MULT
               ENDIF
! ADD NEW CAPACITY
               TEMP_L = RESURRECT_RETROFIT_UNIT(CURRENT_YEAR,I)
               TEMP_R4 = CL_CAPACITY_PLANNING_adj( &
                                        CURRENT_YEAR,I,2,.FALSE.)
               IF(RETRO_CAP_CO2_MULT > .01) THEN
                  IF(INPUT_MW(2,I) < -.01) THEN
                  ELSE
                     INPUT_MW(2,I) = INPUT_MW(2,I)/RETRO_CAP_CO2_MULT
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
      RETURN

! 102909. TO GET GRX TO ADD CAPACITY CORRECTLY

      ENTRY ADJUST_GRX_CO2_RETRO_PLAN_CAP(R_YEAR,R_UNITNO)


         ADJUST_GRX_CO2_RETRO_PLAN_CAP = .TRUE.
         CURRENT_YEAR = get_BASE_YEAR() + R_YEAR

         RETRO_CAP_CO2_MULT = CO2_RTRO_CAP_MULT(I)
         IF(GRX_RETROFITED_UNIT(int(R_UNITNO,2), &
            real(RETRO_CAP_CO2_MULT,4))) THEN
! RETIRE CURRENT CAPACITY
            TEMP_R4 = CL_CAPACITY_PLANNING_adj( &
                                        CURRENT_YEAR,R_UNITNO,2,.TRUE.)

            IF(INPUT_MW(2,R_UNITNO) < -.01) THEN
! NEED TO 'POKE' A NEW CAPACITY
            ELSE
               INPUT_MW(2,R_UNITNO) = INPUT_MW(2,R_UNITNO) &
                                                   * RETRO_CAP_CO2_MULT
            ENDIF
! ADD NEW CAPACITY
            TEMP_L = RESURRECT_RETROFIT_UNIT(CURRENT_YEAR,R_UNITNO)
            TEMP_R4 = CL_CAPACITY_PLANNING_adj( &
                                       CURRENT_YEAR,R_UNITNO,2,.FALSE.)
            IF(RETRO_CAP_CO2_MULT > .01) THEN
               IF(INPUT_MW(2,R_UNITNO) < -.01) THEN
               ELSE
                  INPUT_MW(2,R_UNITNO) = INPUT_MW(2,R_UNITNO)/ &
                                                     RETRO_CAP_CO2_MULT
               ENDIF
            ENDIF
         ENDIF
      RETURN

      ENTRY GET_CO2_RETRO_HEAT_MULT(r_nunits_loc)

         GET_CO2_RETRO_HEAT_MULT = CO2_RTRO_HEAT_MULT(r_nunits_loc)
      RETURN

      ENTRY GET_CO2_RETRO_CAP_MULT(r_nunits_loc)

         GET_CO2_RETRO_CAP_MULT = CO2_RTRO_CAP_MULT(r_nunits_loc)
      RETURN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ENTRY GET_HG_CONTROL_FOR_UNIT( &
                                r_nunits_loc, &
                                R_DATE, &
                                R_HG_CONTROL_PERCENT)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         GET_HG_CONTROL_FOR_UNIT = .FALSE.
         R_HG_CONTROL_PERCENT = 1.0
         IF(R_DATE >= HG_CTL_DATE(r_nunits_loc)) THEN
            GET_HG_CONTROL_FOR_UNIT = .TRUE.
            R_HG_CONTROL_PERCENT = HG_CNTRL_PERCENT(r_nunits_loc)
         ENDIF
      RETURN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ENTRY GET_OTHER3_CONTROL_FOR_UNIT( &
                                r_nunits_loc, &
                                R_DATE, &
                                R_OTHER3_CONTROL_PERCENT)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         GET_OTHER3_CONTROL_FOR_UNIT = .FALSE.
         R_OTHER3_CONTROL_PERCENT = 1.0
         IF(R_DATE >= OTH3_CONTROL_DATE(r_nunits_loc)) THEN
            GET_OTHER3_CONTROL_FOR_UNIT = .TRUE.
            R_OTHER3_CONTROL_PERCENT = OTHER3_CNTRL_PERCENT(r_nunits_loc)
         ENDIF
      RETURN
 &
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      ENTRY GET_POWERDAT_PLANT_ID(r_nunits_loc)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         GET_POWERDAT_PLANT_ID = FLOAT(POWERDAT_PLANT_ID(r_nunits_loc))
      RETURN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 10/1/00 MOVED FROM MARGNOBJ.FOR

      ENTRY GET_RESOURCE_ID_TO_UNIT(R_ID,R_NUMBER_OF_RESOURCE_ID) ! I2


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         GET_RESOURCE_ID_TO_UNIT = RESOURCE_ID_TO_UNIT(R_ID)
         R_NUMBER_OF_RESOURCE_ID = NUMBER_OF_RESOURCE_ID(R_ID)
         IF(GET_RESOURCE_ID_TO_UNIT > 0) THEN
            TEMP_R4 = TEMP_R4
         ENDIF
      RETURN
! 063007.

      ENTRY GET_I8_ID_TO_UNIT(R_I8_ID) ! I8

! 021910 CHANGED TO GET CORRECT RETURN
         GET_I8_ID_TO_UNIT = 0
         DO I = 1, get_nunits()
            IF(HESI_UNIT_ID_NUMs(I) /= R_I8_ID) CYCLE
            GET_I8_ID_TO_UNIT = I
            EXIT
         END DO
      RETURN

      ENTRY UNIQUE_REPORT_VALUE_FOR_CL_UNIT(r_nunits_loc)  ! I4

         UNIQUE_REPORT_VALUE_FOR_CL_UNIT = &
                                       CL_UNIT_UNIQUE_RPT_ID(r_nunits_loc)
      RETURN

      ENTRY REPORT_THIS_CL_UNIT(r_nunits_loc)  ! L1

         REPORT_THIS_CL_UNIT =  &
            ns_cla_decs%REPORT_THIS_UNIT(r_nunits_loc) == 'T'
      RETURN

      ENTRY GET_VCPMWH_IN(r_nunits_loc)  !R4

         GET_VCPMWH_IN = VCPMWH_IN(r_nunits_loc)
      RETURN

      ENTRY ESCALATE_THERMAL_VOM(R_YEAR,R_MONTH,r_nunits_loc)  !R4

         IF(GRX_ITERATIONS == 0 .OR. r_nunits_loc > GRX_NUNITS) THEN
            VCPMWH_IN(r_nunits_loc) = &
                         ESCALATED_MONTHLY_VALUE(VCPMWH_IN(r_nunits_loc), &
                               OMESCR(r_nunits_loc),R_YEAR,R_MONTH,INT2(1))
         ENDIF
         ESCALATE_THERMAL_VOM = VCPMWH_IN(r_nunits_loc)
      RETURN

      ENTRY ESCALATE_THERMAL_FOM(R_YEAR,R_MONTH,r_nunits_loc)  !R4

         IF(GRX_ITERATIONS == 0 .OR. r_nunits_loc > GRX_NUNITS) THEN
            FIXED_COST_IN(r_nunits_loc) = &
                     ESCALATED_MONTHLY_VALUE(FIXED_COST_IN(r_nunits_loc), &
                               FIXED_COST_ESCALATOR(r_nunits_loc), &
                                                R_YEAR,R_MONTH,INT2(1))
         ENDIF
         ESCALATE_THERMAL_FOM = FIXED_COST_IN(r_nunits_loc)
      RETURN

      ENTRY GET_FIXED_COST_IN(r_nunits_loc)  !R4

         GET_FIXED_COST_IN = FIXED_COST_IN(r_nunits_loc)
      RETURN

      ENTRY GET_NOX_VOM(r_nunits_loc)  !R4

         GET_NOX_VOM = ns_cla_ox%NOX_VOM(r_nunits_loc)
      RETURN

      ENTRY GET_NOX_FOM(r_nunits_loc)  !R4

         GET_NOX_FOM = ns_cla_ox%NOX_FOM(r_nunits_loc)
      RETURN

      ENTRY GET_SOX_VOM(r_nunits_loc)  !R4

         GET_SOX_VOM = ns_cla_ox%SOX_VOM(r_nunits_loc)
      RETURN

      ENTRY GET_SOX_FOM(r_nunits_loc)  !R4

         GET_SOX_FOM = ns_cla_ox%SOX_FOM(r_nunits_loc)
      RETURN

      ENTRY GET_CO2_VOM(r_nunits_loc)  !R4

         GET_CO2_VOM = ns_cla_ox%CO2_VOM(r_nunits_loc)
      RETURN

      ENTRY GET_CO2_FOM(r_nunits_loc)  !R4

         GET_CO2_FOM = ns_cla_ox%CO2_FOM(r_nunits_loc)
      RETURN

      ENTRY GET_HG_VOM(r_nunits_loc)  !R4

         GET_HG_VOM = ns_cla_ox%HG_VOM(r_nunits_loc)
      RETURN

      ENTRY GET_HG_FOM(r_nunits_loc)  !R4

         GET_HG_FOM = ns_cla_ox%HG_FOM(r_nunits_loc)
      RETURN

      ENTRY GET_OTHER3_VOM(r_nunits_loc)  !R4

         GET_OTHER3_VOM = ns_cla_ox%OTHER3_VOM(r_nunits_loc)
      RETURN

      ENTRY GET_OTHER3_FOM(r_nunits_loc)  !R4

         GET_OTHER3_FOM = ns_cla_ox%OTHER3_FOM(r_nunits_loc)
      RETURN

      ENTRY GET_MARKET_FLOOR(r_nunits_loc)

         GET_MARKET_FLOOR = MARKET_FLOOR(r_nunits_loc)
      RETURN

      ENTRY GET_MARKET_CEILING(r_nunits_loc)

         GET_MARKET_CEILING = MARKET_CEILING(r_nunits_loc)
      RETURN

      ENTRY GET_PBTUCT_SAVE(r_nunits_loc)  !R4

         GET_PBTUCT_SAVE = PBTUCT_SAVE(r_nunits_loc)
      RETURN

      ENTRY GET_DISPADJ_SAVE(r_nunits_loc)  !R4

         GET_DISPADJ_SAVE = DISPADJ_SAVE(r_nunits_loc)
      RETURN

      ENTRY GET_DISPADJ2_SAVE(r_nunits_loc)  !R4

         GET_DISPADJ2_SAVE = DISPADJ2_SAVE(r_nunits_loc)
      RETURN

       ENTRY GET_START_UP_LOGIC(r_nunits_loc,R_START_UP_TYPE) !L1

          R_START_UP_TYPE = STARTup_LOGIC(r_nunits_loc)
          GET_START_UP_LOGIC = .TRUE.
       RETURN

       ENTRY GET_SPIN_STATUS(r_nunits_loc)

          GET_SPIN_STATUS = CONTRIBUTES_2_SPIN(r_nunits_loc) /= 'F'
       RETURN

      ENTRY GET_TOTAL_START_UP_UNITS()

         GET_TOTAL_START_UP_UNITS = TOTAL_START_UP_UNITS
      RETURN

      ENTRY GET_NEWGEN_INDEX(r_nunits_loc) ! INTEGER*2

         GET_NEWGEN_INDEX = NEWGEN_INDEX(r_nunits_loc)
      RETURN

      ENTRY GET_START_UP_INDEX(r_nunits_loc)

         GET_START_UP_INDEX = START_UP_INDEX(r_nunits_loc)
      RETURN

      ENTRY GET_ANNUAL_GROSS_MARGIN(r_nunits_loc)

         GET_ANNUAL_GROSS_MARGIN = ANNUAL_GROSS_MARGIN(r_nunits_loc,3)
      RETURN

      ENTRY GET_UNIT_START_UP_COSTS(R_YEAR,r_nunits_loc,R_MONTH)

         IF(STARTUP_COSTS(r_nunits_loc) < 0.) THEN
            GET_UNIT_START_UP_COSTS = &
                       START_UP_COST_ESCALAT_MTHLY_VAL( &
                                 INT2(ABS(STARTUP_COSTS(r_nunits_loc))), &
                                    R_YEAR)
         ELSE

            IF(STARTUP_COSTS_ESCALATION(r_nunits_loc) < 0.) THEN
               GET_UNIT_START_UP_COSTS = 1. ! STARTUP_COSTS(r_nunits_loc)
               VECTOR_IS_VALUE = .FALSE.
               DO YR = R_YEAR ,1,-1
                  TEMP_I2 = INT2(STARTUP_COSTS_ESCALATION(r_nunits_loc))
                  ESCAL_RATE = RET_VAL_FROM_ESCALATION_VECTOR( &
                                   YR,TEMP_I2,VECTOR_IS_VALUE,R_MONTH)
                  IF(VECTOR_IS_VALUE) THEN

                     GET_UNIT_START_UP_COSTS = &
                        GET_UNIT_START_UP_COSTS * ESCAL_RATE

                     EXIT
                  ELSE
                  GET_UNIT_START_UP_COSTS = GET_UNIT_START_UP_COSTS * &
                                               (1.0 + ESCAL_RATE/100.)
                  ENDIF
               ENDDO ! YR
               IF(.NOT. VECTOR_IS_VALUE) THEN
                  GET_UNIT_START_UP_COSTS = GET_UNIT_START_UP_COSTS * &
                                               STARTUP_COSTS(r_nunits_loc)
               ENDIF
            ELSEIF(STARTUP_COSTS_ESCALATION(r_nunits_loc) > 0.) THEN
               ESCAL_RATE =  STARTUP_COSTS_ESCALATION(r_nunits_loc)/100.
               GET_UNIT_START_UP_COSTS = STARTUP_COSTS(r_nunits_loc)
               DO YR = 1, R_YEAR
                  GET_UNIT_START_UP_COSTS = GET_UNIT_START_UP_COSTS * &
                                                     (1.0 + ESCAL_RATE)
               ENDDO
            ELSE
               GET_UNIT_START_UP_COSTS = STARTUP_COSTS(r_nunits_loc)
            ENDIF
         ENDIF
      RETURN

      ENTRY GET_START_UP_COSTS(r_nunits_loc)

         GET_START_UP_COSTS = STARTUP_COSTS(r_nunits_loc)
      RETURN

      ENTRY GET_RAMP_RATE(r_nunits_loc)

         GET_RAMP_RATE = RAMP_RATE_array(r_nunits_loc)
      RETURN

      ENTRY GET_RAMP_DOWN_RATE(r_nunits_loc)

         GET_RAMP_DOWN_RATE = RAMP_DOWN_RATEs(r_nunits_loc)
      RETURN

      ENTRY GET_MIN_UP_TIME(r_nunits_loc)

         GET_MIN_UP_TIME = MINimum_UP_TIME(r_nunits_loc)
      RETURN

      ENTRY GET_FOR_DURATION(r_nunits_loc)

         GET_FOR_DURATION = FOR_DURAtn(r_nunits_loc)
      RETURN

      ENTRY GET_FOR_FREQUENCY(r_nunits_loc)

         GET_FOR_FREQUENCY = FOR_FREQUENCies(r_nunits_loc)
      RETURN

      ENTRY GET_MIN_DOWN_TIME(r_nunits_loc)

         GET_MIN_DOWN_TIME = MIN_DOWN_TIMEs(r_nunits_loc)
      RETURN

      ENTRY UNIT_ON_LINE_IN_YEAR(r_nunits_loc,R_YEAR)

         BASE_DATE = (get_BASE_YEAR() + R_YEAR - 1900) * 100
         DATE1 = BASE_DATE + 1
         DATE2 = BASE_DATE + 12
      IF(ONLINE(r_nunits_loc) > DATE2 .OR. &
        OFLINE(r_nunits_loc) < DATE1) THEN
            UNIT_ON_LINE_IN_YEAR = .FALSE.
         ELSE
            UNIT_ON_LINE_IN_YEAR = .TRUE.
         ENDIF
      RETURN

      ENTRY INIT_ANNUAL_GROSS_MARGIN()


         strategic_rtrmts_logic = &
               GET_STRATEGIC_RETIRMENTS_LOGIC(MINIMUM_MARGIN_PER_KW)

         INIT_ANNUAL_GROSS_MARGIN = MINIMUM_MARGIN_PER_KW
         RETIREMENTS_CO2 = 0.0
         RETIREMENTS_MW = 0.0
         RETIREMENTS_MWH = 0.0
         RETIREMENTS_UPLIFT_PER_TON = 0.0
         RETIREMENTS_CUM_CO2 = 0.0
         RETIREMENTS_CO2_PRICE = 999.0 ! COST OF UNMET CO2

         CM_RETIREMENT_KW = 999999.0
         CM_RETIREMENT_UNIT = 0
         NEW_UNIT_RETIRE_VALUE_BY_CM = 0.0

       IF(GRX_ITERATIONS == 0) ECON_CO2_RETIRE_PRICE = 999999.0

      RETURN

      ENTRY GET_CM_RETIREMENT_KW(R_CM,r_nunits_loc)
      
         if(file_trace_gcrk==0) then
            file_trace_gcrk=open_trace("get_cm_retirement_kw.trace", &
            rq_gcrk)
         end if
       
       
       call write_trace_message(file_trace_gcrk, "get_cm_retirement_kw")
       call write_trace_int2(file_trace_gcrk, "R_CM", R_CM)
       call write_trace_int2(file_trace_gcrk, "R_NUNITS", r_nunits_loc)

      
         IF(R_CM > 0 .AND. R_CM < 1001) THEN
            IF(CM_RETIREMENT_KW(R_CM) > 999998.0) THEN
               GET_CM_RETIREMENT_KW = 0.0
               r_nunits_loc = 0
            ELSE
               GET_CM_RETIREMENT_KW = CM_RETIREMENT_KW(R_CM)
               r_nunits_loc = CM_RETIREMENT_UNIT(R_CM)
               ANNUAL_MARGIN_PER_KW = &
                     1000.0 * (ECON_RETIRE_MARGIN(r_nunits_loc,0))/ &
                                                (MW(2,r_nunits_loc)*4.0)
               WRITE(4,*) &
                'At GET_CM_RETIREMENT_KW   ', &
                'RET T-3',ECON_RETIRE_MARGIN(r_nunits_loc,1), &
                'RET T-2',ECON_RETIRE_MARGIN(r_nunits_loc,2), &
                'RET T-1',ECON_RETIRE_MARGIN(r_nunits_loc,3), &
                'RET T  ',ECON_RETIRE_MARGIN(r_nunits_loc,4), &
                'MW     ',MW(2,r_nunits_loc), &
                'RET SUM',ECON_RETIRE_MARGIN(r_nunits_loc,0), &
                'r_nunits_loc',r_nunits_loc, &
                'R_CM',R_CM, &
                'IN-CM_RETIRE_KW',CM_RETIREMENT_KW(R_CM), &
                'ANNUAL_MARGIN_PER_KW',ANNUAL_MARGIN_PER_KW, &
                'ReCalCM_RETIRE_KW',MIN(CM_RETIREMENT_KW(R_CM), &
                                               -ANNUAL_MARGIN_PER_KW)
            ENDIF
         ENDIF
      RETURN

      ENTRY SAVE_ANNUAL_GROSS_MARGIN(  r_nunits_loc, &
                                       R_MW, &
                                       R_GROSS_MARGIN, &
                                       R_CO2,R_MWH, &
                                       R_CO2_COST, &
                                       R_CAP_COST)


         LAST_ITER_GROSS_MARGIN(r_nunits_loc) = R_GROSS_MARGIN
         IF(GRX_ITERATIONS == 0) then
            IF(USED_4_YR_EBITDA_TOTAL .OR. USE_EACH_YR_EBITDA_NEG) THEN
               ECON_RETIRE_MARGIN(r_nunits_loc,1) = &
                                        ECON_RETIRE_MARGIN(r_nunits_loc,2)
               ECON_RETIRE_MARGIN(r_nunits_loc,2) = &
                                         ECON_RETIRE_MARGIN(r_nunits_loc,3)
               ECON_RETIRE_MARGIN(r_nunits_loc,3) = &
                                         ECON_RETIRE_MARGIN(r_nunits_loc,4)
               ECON_RETIRE_MARGIN(r_nunits_loc,4) = R_GROSS_MARGIN
               ITER_0_ECON_RETIRE_MARGIN(r_nunits_loc) = &
                                     ECON_RETIRE_MARGIN(r_nunits_loc,1) + &
                                     ECON_RETIRE_MARGIN(r_nunits_loc,2) + &
                                     ECON_RETIRE_MARGIN(r_nunits_loc,3) + &
                                       ECON_RETIRE_MARGIN(r_nunits_loc,4)
            ELSE
               ITER_0_ECON_RETIRE_MARGIN(r_nunits_loc) = R_GROSS_MARGIN
               ECON_RETIRE_MARGIN(r_nunits_loc,4) = &
                                       LAST_ITER_GROSS_MARGIN(r_nunits_loc)
            ENDIF
         ELSE
            ECON_RETIRE_MARGIN(r_nunits_loc,4) = &
                                      LAST_ITER_GROSS_MARGIN(r_nunits_loc)
         ENDIF
         ECON_RETIRE_MARGIN(r_nunits_loc,0) = &
                                     ECON_RETIRE_MARGIN(r_nunits_loc,1) + &
                                     ECON_RETIRE_MARGIN(r_nunits_loc,2) + &
                                     ECON_RETIRE_MARGIN(r_nunits_loc,3) + &
                                       ECON_RETIRE_MARGIN(r_nunits_loc,4)
         IF(R_MWH < .001) THEN
            ! RESET TO 0 ITER
            RETIREMENT_MEASURE = ITER_0_ECON_RETIRE_MARGIN(r_nunits_loc)
        ELSEIF(USED_4_YR_EBITDA_TOTAL .OR. USE_EACH_YR_EBITDA_NEG) THEN
            RETIREMENT_MEASURE = ECON_RETIRE_MARGIN(r_nunits_loc,1) + &
                                 ECON_RETIRE_MARGIN(r_nunits_loc,2) + &
                                 ECON_RETIRE_MARGIN(r_nunits_loc,3) + &
                                 R_GROSS_MARGIN
         ELSE
            RETIREMENT_MEASURE = R_GROSS_MARGIN
         ENDIF
         RETIRE_THIS_UNIT(r_nunits_loc) = RETIREMENT_MEASURE < 0.
         IF(USE_EACH_YR_EBITDA_NEG) THEN
            RETIRE_THIS_UNIT(r_nunits_loc) = &
                            ECON_RETIRE_MARGIN(r_nunits_loc,1) < 0. &
                           .AND. ECON_RETIRE_MARGIN(r_nunits_loc,2)  < 0. &
                            .AND. ECON_RETIRE_MARGIN(r_nunits_loc,3) < 0. &
                            .AND. R_GROSS_MARGIN < 0.
         ENDIF


! retirement unit price is set here. retros are in
         ECON_CO2_RETIRE_PRICE(r_nunits_loc) = RETIREMENT_MEASURE
         CO2_K = 0.0000005 * 7000.0 * 130.0
         if(ABS(R_CO2 - CO2_K * R_MWH) < 0.01) then
            ECON_CO2_RETRO_PRICE(r_nunits_loc) = RETIREMENT_MEASURE
         else
! the retro fit comparison should be based
! on the change in theunit CO2 output
            ECON_CO2_RETRO_PRICE(r_nunits_loc) = &
                                    RETIREMENT_MEASURE* 1000000.0 / &
                                                (R_CO2 - CO2_K * R_MWH)
         endif

         RETIREMENTS_CO2(r_nunits_loc) = R_CO2
       RETIREMENTS_CO2_COST(r_nunits_loc) = RETIREMENT_MEASURE ! R_CO2_COST
         RETIREMENTS_CAP_COST(r_nunits_loc) = R_CAP_COST
         RETIREMENTS_MW(r_nunits_loc) = R_MW
         RETIREMENTS_MWH(r_nunits_loc) = R_MWH

         ANNUAL_GROSS_MARGIN(r_nunits_loc,3) = R_GROSS_MARGIN
         IF(GRX_ITERATIONS == 0) then
            ANNUAL_GROSS_MARGIN(r_nunits_loc,1) = &
                                        ANNUAL_GROSS_MARGIN(r_nunits_loc,2)
            ANNUAL_GROSS_MARGIN(r_nunits_loc,2) = &
                                        ANNUAL_GROSS_MARGIN(r_nunits_loc,3)
            ANNUAL_GROSS_MARGIN(r_nunits_loc,0) = &
                                   ANNUAL_GROSS_MARGIN(r_nunits_loc,1) + &
                                    ANNUAL_GROSS_MARGIN(r_nunits_loc,2) + &
                                      ANNUAL_GROSS_MARGIN(r_nunits_loc,3)
         ENDIF
         IF(R_MWH < .001) THEN
            UNIT_ANNUAL_GROSS_MARGIN = ANNUAL_GROSS_MARGIN(r_nunits_loc,0)
         ELSE
            UNIT_ANNUAL_GROSS_MARGIN = R_GROSS_MARGIN + &
                                    ANNUAL_GROSS_MARGIN(r_nunits_loc,1) + &
                                       ANNUAL_GROSS_MARGIN(r_nunits_loc,2)
         ENDIF
         IF(strategic_rtrmts_logic .AND. R_MW > 0.) THEN
            ANNUAL_MARGIN_PER_KW = UNIT_ANNUAL_GROSS_MARGIN/(.036*R_MW)
            CURRENT_YEAR = globecom_year+get_BASE_YEAR()
            IF(ANNUAL_MARGIN_PER_KW < MINIMUM_MARGIN_PER_KW) THEN

               TEMP_R4 = CL_CAPACITY_PLANNING_adj( &
                                        CURRENT_YEAR,r_nunits_loc,2,.TRUE.)
               TEMP_I2 = CL_CAPACITY_TEMP_RETIRE_UNIT(CURRENT_YEAR, &
                                                      r_nunits_loc)
            WRITE(9,1007) "Retirement",UNITNM(r_nunits_loc),CURRENT_YEAR, &
                                          ANNUAL_MARGIN_PER_KW
            ENDIF
         ENDIF
         TG = GET_TRANS_GROUP_POSITION( &
                                        TRANSACTION_GROUP_ID(r_nunits_loc))
         CM = GET_CM_FROM_TG(TG)

         IF(MW(2,r_nunits_loc) > 0.001 .AND. R_MWH > 0.001 .AND. &
                  ECON_RETIRE_MARGIN(r_nunits_loc,4) < 0.0 .AND. &
                  ECON_RETIRE_MARGIN(r_nunits_loc,3) < 0.0 .AND. &
                  ECON_RETIRE_MARGIN(r_nunits_loc,2) < 0.0 .AND. &
                  ECON_RETIRE_MARGIN(r_nunits_loc,1) < 0.0 .AND. &
                  ns_cla_decs%REPORT_THIS_UNIT(r_nunits_loc) == 'T' .AND. &
                  PRIM_MOVER_STR(r_nunits_loc) /= 'FC    ' .AND. &
                  PRIM_MOVER_STR(r_nunits_loc) /= 'GE    ' .AND. &
                  PRIM_MOVER_STR(r_nunits_loc) /= 'BI    ' .AND. &
                  PRIM_MOVER_STR(r_nunits_loc) /= 'LF    ' .AND. &
                  PRIM_MOVER_STR(r_nunits_loc) /= 'ZZ    ') THEN
            ANNUAL_MARGIN_PER_KW = &
                     1000.0 * (ECON_RETIRE_MARGIN(r_nunits_loc,0))/ &
                                                (MW(2,r_nunits_loc)*4.0)
            TEMP_REAL = CM_RETIREMENT_KW(CM)
            IF(ANNUAL_MARGIN_PER_KW < MINIMUM_MARGIN_PER_KW) THEN

               IF(-ANNUAL_MARGIN_PER_KW < CM_RETIREMENT_KW(CM)) THEN
                  CM_RETIREMENT_KW(CM) = MIN(CM_RETIREMENT_KW(CM), &
                                           -ANNUAL_MARGIN_PER_KW)
                  CM_RETIREMENT_UNIT(CM) = r_nunits_loc
               ENDIF
            ENDIF
               WRITE(4,*) &
                'At SAVE_ANNUAL_GROSS_MARGIN   ', &
                'RET T-3',ECON_RETIRE_MARGIN(r_nunits_loc,1), &
                'RET T-2',ECON_RETIRE_MARGIN(r_nunits_loc,2), &
                'RET T-1',ECON_RETIRE_MARGIN(r_nunits_loc,3), &
                'RET T  ',ECON_RETIRE_MARGIN(r_nunits_loc,4), &
                'MW     ',MW(2,r_nunits_loc), &
                'RET SUM',ECON_RETIRE_MARGIN(r_nunits_loc,0), &
                'NUNITS',r_nunits_loc, &
                'R_CM',CM, &
                'OUT-CM_RETIRE_KW',CM_RETIREMENT_KW(CM), &
                'ANNUAL_MARGIN_PER_KW',ANNUAL_MARGIN_PER_KW, &
                'IN-CM_RETIRE_KW',TEMP_REAL
         ENDIF
         ! TODO: Remove code in IF block
         IF( 1 == 2 .AND. r_nunits_loc > BASE_CL_UNITS .AND. &
                        R_MW > 0.0 .AND. &
                              R_GROSS_MARGIN < 0.0 .AND. &
                                    CM > 0 .AND. &
                                          CM <= UPPER_TRANS_Gp) THEN
            NEW_UNIT_RETIRE_VALUE_BY_CM(CM) = &
                     MAX(NEW_UNIT_RETIRE_VALUE_BY_CM(CM), &
                                           -1000.0*R_GROSS_MARGIN/R_MW)
         ENDIF



         SAVE_ANNUAL_GROSS_MARGIN = R_GROSS_MARGIN
      RETURN
 1007 FORMAT(5X,A,5X,A,5X,I4,5X,F7.2)

      ENTRY CALC_MRX_RPS_CURVES(R_YEAR)

         CALC_MRX_RPS_CURVES = .FALSE.


         PG = NUMBER_OF_RPS_PROGRAMS()
         NUM_TRANS = get_num_save_active_transact()
         IF(ALLOCATED(MRX_RPS_CL_CURVE_MWH)) &
                DEALLOCATE(MRX_RPS_CL_CURVE_MWH, &
                            MRX_RPS_CL_CURVE_CUM_MWH, &
                            MRX_RPS_CL_CURVE_MARGIN, &
                            MRX_RPS_CL_CURVE_CUM_MARGIN, &
                            MRX_RPS_CL_CURVE_INDEX, &
                            MRX_RPS_CL_UNIT_NO, &
                            MRX_RPS_CL_UNIT_INDEX, &
                            MRX_RPS_DV_CURVE_MWH, &
                            MRX_RPS_DV_CURVE_CUM_MWH, &
                            MRX_RPS_DV_CURVE_MARGIN, &
                            MRX_RPS_DV_CURVE_CUM_MARGIN, &
                            MRX_RPS_DV_CURVE_INDEX, &
                            MRX_RPS_DV_UNIT_NO, &
                            MRX_RPS_DV_UNIT_INDEX)
         ALLOCATE(MRX_RPS_CL_CURVE_MWH(get_nunits(),PG), &
                 MRX_RPS_CL_CURVE_CUM_MWH(get_nunits(),PG), &
                  MRX_RPS_CL_CURVE_MARGIN(get_nunits(),PG), &
              MRX_RPS_CL_CURVE_CUM_MARGIN(get_nunits(),PG), &
                  MRX_RPS_CL_CURVE_INDEX(get_nunits(),PG), &
                  MRX_RPS_CL_UNIT_NO(PG), &
                  MRX_RPS_CL_UNIT_INDEX(get_nunits(),PG), &
                  MRX_RPS_DV_CURVE_MWH(NUM_TRANS,PG), &
                  MRX_RPS_DV_CURVE_CUM_MWH(NUM_TRANS,PG), &
                  MRX_RPS_DV_CURVE_MARGIN(NUM_TRANS,PG), &
                  MRX_RPS_DV_CURVE_CUM_MARGIN(NUM_TRANS,PG), &
                  MRX_RPS_DV_CURVE_INDEX(NUM_TRANS,PG), &
                  MRX_RPS_DV_UNIT_NO(PG), &
                  MRX_RPS_DV_UNIT_INDEX(NUM_TRANS,PG))
         CALC_MRX_RPS_CURVES = .TRUE.

         MRX_RPS_CL_UNIT_NO = 0
         MRX_RPS_CL_UNIT_INDEX = 0
         MRX_RPS_CL_CURVE_INDEX = 0
         MRX_RPS_CL_CURVE_MWH = 0.0
         MRX_RPS_CL_CURVE_CUM_MWH = 0.0
         MRX_RPS_CL_CURVE_MARGIN = 0.0
         MRX_RPS_CL_CURVE_CUM_MARGIN = 0.0

         DO U = 1, GRX_NUNITS ! 071220. CHANGED FROM UNITS.
! DOUBLE INDEX
            pa_ord = RPS_PROGRAM_NUMBER(U)

            IF(pa_ord == 0) then
                CYCLE
            end if


            RPS_COUNT = 1
            DO
               PX = GET_RPS_PROGRAM_POSITION(pa_ord,RPS_COUNT)
               IF(PX == 0) EXIT
               RPS_COUNT = RPS_COUNT + 1

               LOCAL_RPS_CONTRIB_PERCENT = RPS_CONTRIBUTION_PERCENT(U)
               IF(LOCAL_RPS_CONTRIB_PERCENT < -0.01) THEN
                  LOCAL_RPS_CONTRIB_PERCENT = 0.01 * &
                  ESCALATED_MONTHLY_VALUE(LOCAL_RPS_CONTRIB_PERCENT, &
                          ABS(INT2(RPS_CONTRIBUTION_PERCENT(U))), &
                                                R_YEAR,INT2(1),INT2(1))
               ELSE
                  LOCAL_RPS_CONTRIB_PERCENT = &
                                       0.01 * LOCAL_RPS_CONTRIB_PERCENT
               ENDIF
               TEMP_ENRG = MRX_RPS_CL_CURVE_UMWH(U) * &
                                              LOCAL_RPS_CONTRIB_PERCENT

               IF(TEMP_ENRG < 0.0001) CYCLE
               MRX_RPS_CL_UNIT_NO(PX) = MRX_RPS_CL_UNIT_NO(PX) + 1
               TEMP_I2 = MRX_RPS_CL_UNIT_NO(PX)
           IF(TEMP_I2 < 1 .OR. TEMP_I2 > get_nunits()) THEN
                  TEMP_I2 = TEMP_I2
               ENDIF
               MRX_RPS_CL_UNIT_INDEX(TEMP_I2,PX) = U
               MRX_RPS_CL_CURVE_INDEX(TEMP_I2,PX) = &
                                                MRX_RPS_CL_UNIT_NO(PX)
               MRX_RPS_CL_CURVE_MWH(TEMP_I2,PX) = TEMP_ENRG
               MRX_RPS_CL_CURVE_MARGIN(TEMP_I2,PX) = &
                       1000.0 * (MRX_RPS_CL_CURVE_REVENUE(U) - &
                                    MRX_RPS_CL_CURVE_COST(U))/TEMP_ENRG
            ENDDO
         ENDDO
         DO PA_ord = 1, PG
            
            IF(MRX_RPS_CL_UNIT_NO(pa_ord) > 1) THEN
        call write_sic_trace("clrx:0001",MRX_RPS_CL_UNIT_NO(PA_ord))
               CALL SortIncrPos(MRX_RPS_CL_UNIT_NO(PA_ord), &
                                MRX_RPS_CL_CURVE_INDEX(1,pa_ord), &
                                MRX_RPS_CL_CURVE_MARGIN(1,pa_ord))
            ENDIF
            DO I = MRX_RPS_CL_UNIT_NO(pa_ord), 1, -1
               J = MRX_RPS_CL_CURVE_INDEX(I,pa_ord)
               U = MRX_RPS_CL_UNIT_INDEX(J,pa_ord)
               IF(I == MRX_RPS_CL_UNIT_NO(pa_ord)) THEN
                  MRX_RPS_CL_CURVE_CUM_MWH(I,pa_ord) = &
                                             MRX_RPS_CL_CURVE_MWH(J,pa_ord)
               ELSE
                  MRX_RPS_CL_CURVE_CUM_MWH(I,pa_ord) = &
                        MRX_RPS_CL_CURVE_CUM_MWH(I+1,pa_ord) + &
                                           MRX_RPS_CL_CURVE_MWH(J,pa_ord)
               ENDIF
               MRX_RPS_CL_CURVE_CUM_MARGIN(I,pa_ord) = &
                                          MRX_RPS_CL_CURVE_MARGIN(J,pa_ord)
            ENDDO
         ENDDO

         MRX_RPS_DV_UNIT_NO = 0
         MRX_RPS_DV_UNIT_INDEX = 0
         MRX_RPS_DV_CURVE_INDEX = 0
         MRX_RPS_DV_CURVE_MWH = 0.0
         MRX_RPS_DV_CURVE_CUM_MWH = 0.0
         MRX_RPS_DV_CURVE_MARGIN = 0.0
         MRX_RPS_DV_CURVE_CUM_MARGIN = 0.0
         DO U = 1, NUM_TRANS
! DOUBLE INDEX
            pa_ord=GET_TRANS_RPS_PROG_NUMBER(U)
            
            
            IF(pa_ord == 0) then
                CYCLE
            end if

            RPS_COUNT = 1
            DO
               PX = GET_RPS_PROGRAM_POSITION(pa_ord,RPS_COUNT)
               IF(PX == 0) EXIT
               RPS_COUNT = RPS_COUNT + 1

               LOCAL_RPS_CONTRIB_PERCENT = &
                                        GET_TRANS_RPS_PERCENT(U,R_YEAR)
               TEMP_ENRG = MRX_RPS_DV_CURVE_UMWH(U) * &
                                              LOCAL_RPS_CONTRIB_PERCENT
               IF(TEMP_ENRG < 0.0001) CYCLE
               MRX_RPS_DV_UNIT_NO(PX) = MRX_RPS_DV_UNIT_NO(PX) + 1
               TEMP_I2 = MRX_RPS_DV_UNIT_NO(PX)
               MRX_RPS_DV_UNIT_INDEX(TEMP_I2,PX) = U
               MRX_RPS_DV_CURVE_INDEX(TEMP_I2,PX) = &
                                                 MRX_RPS_DV_UNIT_NO(PX)
               MRX_RPS_DV_CURVE_MWH(TEMP_I2,PX) = TEMP_ENRG
               MRX_RPS_DV_CURVE_MARGIN(TEMP_I2,PX) = &
                       1000.0 * (MRX_RPS_DV_CURVE_REVENUE(U) - &
                                    MRX_RPS_DV_CURVE_COST(U))/TEMP_ENRG
            ENDDO
         ENDDO
         DO pa_ord = 1, PG
            IF(MRX_RPS_DV_UNIT_NO(pa_ord) > 1) THEN
        call write_sic_trace("clrx:0002",MRX_RPS_DV_UNIT_NO(pa_ord))
               CALL SortIncrPos(MRX_RPS_DV_UNIT_NO(pa_ord), &
                                MRX_RPS_DV_CURVE_INDEX(1,pa_ord), &
                                MRX_RPS_DV_CURVE_MARGIN(1,pa_ord))
            ENDIF
            DO I = MRX_RPS_DV_UNIT_NO(pa_ord), 1, -1
               J = MRX_RPS_DV_CURVE_INDEX(I,pa_ord)
               U = MRX_RPS_DV_UNIT_INDEX(J,pa_ord)
               IF(I == MRX_RPS_DV_UNIT_NO(pa_ord)) THEN
                  MRX_RPS_DV_CURVE_CUM_MWH(I,pa_ord) = &
                                             MRX_RPS_DV_CURVE_MWH(J,pa_ord)
               ELSE
                  MRX_RPS_DV_CURVE_CUM_MWH(I,pa_ord) = &
                        MRX_RPS_DV_CURVE_CUM_MWH(I+1,pa_ord) + &
                                             MRX_RPS_DV_CURVE_MWH(J,pa_ord)
               ENDIF
               MRX_RPS_DV_CURVE_CUM_MARGIN(I,pa_ord) = &
                                          MRX_RPS_DV_CURVE_MARGIN(J,pa_ord)
            ENDDO
         ENDDO
      RETURN

      ENTRY GET_NEW_UNIT_RETIRE_VALUE_BY_CM(R_CM)

         IF(ALLOCATED(NEW_UNIT_RETIRE_VALUE_BY_CM) .AND. CM > 0 .AND. &
                                         CM <= UPPER_TRANS_Gp) THEN
            GET_NEW_UNIT_RETIRE_VALUE_BY_CM = &
                                     NEW_UNIT_RETIRE_VALUE_BY_CM(R_CM)
         ELSE
            GET_NEW_UNIT_RETIRE_VALUE_BY_CM = 0.0
         ENDIF
      RETURN

      ENTRY ANNUAL_CO2_RETROFIT_PROCESS(R_YEAR)

         TEMP_L = RETROFIT_LOGIC_ACTIVE(TEMP_R4,TEMP_I2)
         ANN_RETROFIT_COUNTER = 0

         IF(.NOT. TEMP_L) THEN
            ANNUAL_CO2_RETROFIT_PROCESS = .FALSE.
            RETURN
         ENDIF
         IF(.NOT. GRX_CONVERGED) THEN
            !RESET THE get_nunits() RETIRED IN THE LOOP
            CALL  GRX_RETROFIT_OPTONS(RESTORE)
         ENDIF
         CO2_RETROFIT_ABATEMENT_INDEX = 0
         CUM_CO2_EMISSIONS = 0.0
         RETROFIT_UNIT_IS_ACTIVE = .FALSE.
         DO I = 1, RETROFIT_COUNTER
            U = RETROFIT_UNIT_INDEX(I)
            IF(RETROFIT_CANDIDATE(U) /= 'T') CYCLE
            IF(RETIREMENTS_CO2(U) < 0.01 .OR. &
                        RETIREMENTS_MWH(U) < 0.01 .OR. &
                               RETIREMENTS_MW(U) < 0.0001) CYCLE
            IF(.NOT. GRX_CONVERGED) THEN
               CALL GRX_CAPACITY_TEMP_RETRO_UNIT(U)
            ENDIF

            ANN_RETROFIT_COUNTER = ANN_RETROFIT_COUNTER + 1
            RETROFIT_UNIT_IS_ACTIVE(U) = .TRUE.
            CALL ANNUAL_RETROFIT_PROJECT(RETROFIT_PROJ_ID(U), &
                                         R_YEAR, &
                                         RETIREMENTS_CO2(U), &
                                         RETIREMENTS_MW(U), &
                                         RETIREMENTS_MWH(U), &
                                         ANNUAL_GROSS_MARGIN(U,3), &
                                         RETROFIT_UPLIFT_PER_TON(U), &
                                         RETROFIT_CO2)
            RETROFIT_CO2_PRICE(U) = RETROFIT_UPLIFT_PER_TON(U)
            CUM_CO2_EMISSIONS = CUM_CO2_EMISSIONS + RETROFIT_CO2
         END DO
! SORT FROM WORST TO BEST RETIREMENT

         ASCENDING = .FALSE. ! DECENDING
         CALL INDEXED_SORT_ASCENDING(RETROFIT_UPLIFT_PER_TON, &
                                     RETROFIT_UNIT_INDEX, &
                                     RETROFIT_COUNTER, &
                                     ASCENDING)
         NEXT_MRX_RETROFIT = 1
         DO COUNTER = 1, RETROFIT_COUNTER
             TEMP_I2 = RETROFIT_UNIT_INDEX(RETROFIT_COUNTER-COUNTER+1)
             CO2_RETROFIT_ABATEMENT_INDEX(COUNTER) = TEMP_I2
             IF(COUNTER > 1) THEN
               RETROFIT_CUM_CO2(TEMP_I2) = &
                   RETROFIT_CUM_CO2( &
                          CO2_RETROFIT_ABATEMENT_INDEX(COUNTER-1)) + &
                                               RETIREMENTS_CO2(TEMP_I2)
             ELSE
               RETROFIT_CUM_CO2(TEMP_I2) = RETIREMENTS_CO2(TEMP_I2)
             ENDIF
             RETROFIT_CO2_PRICE(TEMP_I2) = &
                                     RETROFIT_UPLIFT_PER_TON(TEMP_I2)
         END DO

         ANNUAL_CO2_RETROFIT_PROCESS = .TRUE.
      RETURN

      ENTRY ANNUAL_CO2_RETIREMENTS_PROCESS(R_YEAR)

         RETIREMENTS_INDEX = 0
         CO2_ABATEMENT_INDEX = 0
         ANN_ECON_RETIRE_COUNTER = 0
         CUM_CO2_EMISSIONS = 0.0

         CLASS = 0
         MONTH = 7
         INT4_EMIS_TYPE = 3
         ANNUAL_CO2_RETIREMENTS_PROCESS = .FALSE.
         IF(.NOT. GET_CO2_RETIREMENTS_LOGIC()) RETURN  !MSG AUG 09

         IF(.NOT. GRX_CONVERGED) &
                               RETIRED_UNITS = RESET_CL_RETIRED_UNITS()
         CURRENT_YEAR = 100*(get_BASE_YEAR() + R_YEAR - 1900)

         DO U = 1, get_nunits()

            IF(FIRST_RETIREMENT_YEAR(U) > CURRENT_YEAR) CYCLE
            IF(RETIREMENT_CANDIDATE(U) /= 'T' .OR. &
                      RETIREMENTS_CO2(U) < 0.01 .OR. &
                           OFLINE(U) < CURRENT_YEAR .OR. &
                                     RETIREMENTS_MW(U) < 0.0001) CYCLE
            ANN_ECON_RETIRE_COUNTER = ANN_ECON_RETIRE_COUNTER + 1
            RETIREMENTS_INDEX(ANN_ECON_RETIRE_COUNTER) = U


            IF(.NOT. GRX_CONVERGED) THEN
               CALL &
               GRX_CAPACITY_RETIRE_OPTIONS(U,ANNUAL_GROSS_MARGIN(U,3))
            ENDIF
            ANNUAL_CAPITAL_COST = 0.17 * 1050.0 * 1000.0 * &
                                                     RETIREMENTS_MW(U)

! 031710. NEW MEASURE OF PROFITABILITY
            RETIREMENTS_UPLIFT_PER_TON(U) = ECON_CO2_RETIRE_PRICE(U)

            CUM_CO2_EMISSIONS = CUM_CO2_EMISSIONS + &
                                                    RETIREMENTS_CO2(U)
         END DO
! SORT FROM WORST TO BEST RETIREMENT
         ASCENDING = .FALSE. ! DECENDING
         CALL INDEXED_SORT_ASCENDING(RETIREMENTS_UPLIFT_PER_TON, &
                                     RETIREMENTS_INDEX, &
                                     ANN_ECON_RETIRE_COUNTER, &
                                     ASCENDING)
         NEXT_MRX_RETIREMENT = 1
         DO COUNTER = 1, ANN_ECON_RETIRE_COUNTER
             TEMP_I2 = &
                 RETIREMENTS_INDEX(ANN_ECON_RETIRE_COUNTER-COUNTER+1)
             CO2_ABATEMENT_INDEX(COUNTER) = TEMP_I2
             IF(COUNTER > 1) THEN
               RETIREMENTS_CUM_CO2(TEMP_I2) = &
               RETIREMENTS_CUM_CO2(CO2_ABATEMENT_INDEX(COUNTER-1)) + &
                                              RETIREMENTS_CO2(TEMP_I2)
             ELSE
               RETIREMENTS_CUM_CO2(TEMP_I2) = RETIREMENTS_CO2(TEMP_I2)
             ENDIF
             RETIREMENTS_CO2_PRICE(TEMP_I2) = &
                                   RETIREMENTS_UPLIFT_PER_TON(TEMP_I2)
         END DO
         ANNUAL_CO2_RETIREMENTS_PROCESS = .TRUE.
      RETURN

      ENTRY ANNUAL_RETIRE_RETRO_PROCESS(R_YEAR)

         ANNUAL_RETIRE_RETRO_PROCESS = .FALSE.  !MSG AUG 09
         RETIRE_RETRO_COUNTER = 0               !MSG AUG 09
         IF(CO2_RETROFIT_LOGIC_ACTIVE()) THEN
           RETIRE_RETRO_COUNTER = RETIRE_RETRO_COUNTER+RETROFIT_COUNTER
            ABATE_RETROFIT_COUNTER = RETROFIT_COUNTER
         ELSE
            ABATE_RETROFIT_COUNTER = 0
         ENDIF
         IF(GET_CO2_RETIREMENTS_LOGIC()) &
                   RETIRE_RETRO_COUNTER = RETIRE_RETRO_COUNTER &
                                         + ANN_ECON_RETIRE_COUNTER
          CO2_MARKET_PTS = 0

         IF(RETIRE_RETRO_COUNTER <= 0) RETURN
         ANNUAL_RETIRE_RETRO_PROCESS = .TRUE.

         YES_CO2_ABATEMENT_REPORT = CO2_ABATEMENT_REPORT_ACTIVE()
         IF(  YES_CO2_ABATEMENT_REPORT .AND. &
                       CO2_ABATEMENT_REPORT_NOT_OPEN) THEN
            CO2_ABATEMENT_VAR_NUM = 12
            CO2_ABATEMENT_NO = CO2_ABATEMENT_RPT_HEADER( &
                                              CO2_ABATEMENT_VAR_NUM, &
                                                CO2_ABATEMENT_REC)
            CO2_ABATEMENT_REPORT_NOT_OPEN = .FALSE.
         ENDIF
         CURRENT_YEAR = R_YEAR+get_BASE_YEAR()

         RETIRE_RETRO_INDEX = 0
         RETIRE_OR_RETRO = 0
         RETIRE_RETRO_POSITION = 0
         RETIRE_RETRO_ABATE_INDEX = 0
! COMBINE RETIRE AND RETRO AND SORT INTO ONE MASTER LIST
         DO I = 1, ANN_ECON_RETIRE_COUNTER
            RETIRE_RETRO_POSITION(I) = I
            RETIRE_RETRO_INDEX(I) = RETIREMENTS_INDEX(I) ! = U
            RETIRE_OR_RETRO(I) = 1 ! RETIRE = 1
! CHANGING INDEX DEFINITION
            RETIRE_RETRO_UPLIFT_PER_TON(I) = &
                            RETIREMENTS_CO2_PRICE(RETIREMENTS_INDEX(I))
         END DO
         I = ANN_ECON_RETIRE_COUNTER
         DO J = 1, ABATE_RETROFIT_COUNTER
            U = RETROFIT_UNIT_INDEX(J)
            IF(.NOT. RETROFIT_UNIT_IS_ACTIVE(U)) CYCLE
            IF(RETROFIT_CANDIDATE(U) /= 'T') CYCLE

            IF(ALLOCATED(GRX_BOP_RETROFIT_ACTIVE)) THEN
               IF(GRX_BOP_RETROFIT_ACTIVE(U)) CYCLE
            ENDIF
            IF(HardWiredRetrofitProject(U) .AND. &
                        .NOT. HWRetrofitProjectThisYear(U))CYCLE
            IF(OFLINE(U) < 100*(get_BASE_YEAR() + R_YEAR - 1900)) CYCLE
            I = I + 1
            RETIRE_RETRO_POSITION(I) = I
            RETIRE_RETRO_INDEX(I) = RETROFIT_UNIT_INDEX(J)
            IF(HWRetrofitProjectThisYear(U)) THEN
               RETIRE_OR_RETRO(I) = 4 ! RETRO = IS Hard Wired
            ELSE
               RETIRE_OR_RETRO(I) = 2 ! RETRO = 2
            ENDIF
            RETIRE_RETRO_UPLIFT_PER_TON(I) = RETROFIT_CO2_PRICE(U)
         END DO
       ! TODO: Remove code in IF block
       IF(.FALSE. ) THEN
         DO J = 1, CO2_MARKET_PTS
            I = I +1
            RETIRE_RETRO_POSITION(I) = I
            RETIRE_RETRO_INDEX(I) = J
            RETIRE_OR_RETRO(I) = 3 ! MARKET SOURCE = 2
            RETIRE_RETRO_UPLIFT_PER_TON(I) = 0.
         ENDDO
       ENDIF ! REMOVE MARKET SOURCE WHICH ISN'T ACTIVE
       RETIRE_RETRO_COUNTER = I
! SORT FROM WORST TO BEST RETIREMENT
         ASCENDING = .FALSE. ! DECENDING
         CALL INDEXED_SORT_ASCENDING(RETIRE_RETRO_UPLIFT_PER_TON, &
                                     RETIRE_RETRO_POSITION, &
                                     RETIRE_RETRO_COUNTER, &
                                     ASCENDING)
         CUM_MW = 0.0
         CUM_MWH = 0.0
         RETIRE_RETRO_COUNT_AFTER_SORT = 0
         RETIRE_RETRO_USED = 0
         LAST_COUNTER = 0
         RETIRE_RETRO_CUM_CO2(0) =  0.
         DO COUNTER = 1, RETIRE_RETRO_COUNTER
            TEMP_I2 = &
                  RETIRE_RETRO_POSITION(RETIRE_RETRO_COUNTER-COUNTER+1)
            U = RETIRE_RETRO_INDEX(TEMP_I2)

! 031910. THIS FILTERS-OUT MORE EXPENSIVE OPTIONS.

               RETIRE_RETRO_USED(U) = RETIRE_RETRO_USED(U) + 1

            RETIRE_RETRO_COUNT_AFTER_SORT = &
                                      RETIRE_RETRO_COUNT_AFTER_SORT + 1
            RETIRE_RETRO_ABATE_INDEX(COUNTER) = U
            IF(RETIRE_OR_RETRO(TEMP_I2) == 3) THEN ! MARKET POSITION
               RPT_UNIT_NAME = "CO2 Market Purchase-"// &
                  CONVERT_2_STR(RETIRE_RETRO_UPLIFT_PER_TON(TEMP_I2))
               RPT_RETIREMENTS_CO2 = 0. ! GRX_CO2_MARKET_TONS(U)
               RPT_RETIREMENTS_MW = 0.
               RPT_RETIREMENTS_MWH = 0.
            ELSE
               IF(RETIRE_OR_RETRO(TEMP_I2) == 2) THEN
                 RPT_UNIT_NAME = TRIM(SP_UNIT_NAME(U))//'-Retro'
               ELSE
                 RPT_UNIT_NAME = SP_UNIT_NAME(U)
               ENDIF
               RPT_RETIREMENTS_CO2 = RETIREMENTS_CO2(U)
               CUM_MW = CUM_MW + RETIREMENTS_MW(U)
               CUM_MWH = CUM_MWH + RETIREMENTS_MWH(U)
               RPT_RETIREMENTS_MW = RETIREMENTS_MW(U)
               RPT_RETIREMENTS_MWH = RETIREMENTS_MWH(U)
            ENDIF
            RETIRE_RETRO_CUM_CO2(COUNTER) = &
                 RETIRE_RETRO_CUM_CO2(COUNTER-1) + RPT_RETIREMENTS_CO2
            RETIRE_RETRO_CO2_PRICE(COUNTER) = &
                                   RETIRE_RETRO_UPLIFT_PER_TON(TEMP_I2)
           UNIT_Decision_CO2_Price(U) = RETIRE_RETRO_CO2_PRICE(COUNTER)

            IF(YES_CO2_ABATEMENT_REPORT) THEN
               IF(trim(CAPACITY_PLANNING_METHOD()) == 'MX'  .AND. &
                  trim(GREEN_MRX_METHOD()) == 'GX') THEN
                  WRITE(CO2_ABATEMENT_NO, &
                        REC=CO2_ABATEMENT_REC) &
                              PRT_ENDPOINT(), &
                              FLOAT(CURRENT_YEAR), &
                              RPT_UNIT_NAME, &
                              FLOAT(GRX_ITERATIONS), &
                              FLOAT(COUNTER), &
                              FLOAT(RETIRE_OR_RETRO(TEMP_I2)), &
                              RETIRE_RETRO_CUM_CO2(COUNTER)/1000000., &
                              RETIRE_RETRO_CO2_PRICE(COUNTER), &
                              CUM_MW, &
                              CUM_MWH, &
                              RPT_RETIREMENTS_CO2/1000000., &
                              RPT_RETIREMENTS_MW, &
                              RPT_RETIREMENTS_MWH, &
                              CURRENT_CO2_DISPATCH_COST, &
                              RETIREMENT_CO2_COST_PER_TON(U)
               ELSE
                  WRITE(CO2_ABATEMENT_NO, &
                        REC=CO2_ABATEMENT_REC) &
                              PRT_ENDPOINT(), &
                              FLOAT(CURRENT_YEAR), &
                              RPT_UNIT_NAME, &
                              FLOAT(COUNTER), &
                              FLOAT(RETIRE_OR_RETRO(TEMP_I2)), &
                              RETIRE_RETRO_CUM_CO2(COUNTER)/1000000., &
                              RETIRE_RETRO_CO2_PRICE(COUNTER), &
                              CUM_MW, &
                              CUM_MWH, &
                              RPT_RETIREMENTS_CO2/1000000., &
                              RPT_RETIREMENTS_MW, &
                              RPT_RETIREMENTS_MWH, &
                              CURRENT_CO2_DISPATCH_COST, &
                              RETIREMENT_CO2_COST_PER_TON(U)
               ENDIF
               CO2_ABATEMENT_REC = CO2_ABATEMENT_REC + 1
            ENDIF
         END DO
         IF(YES_CO2_ABATEMENT_REPORT) CALL FLUSH(CO2_ABATEMENT_NO)
         J = J
      RETURN

      ENTRY GET_CO2_RETIREMENT_PRICE(R_CO2_REDUCTION)

! SEARCH ALGORITHM WOULD BE BETTER.
       IF(R_CO2_REDUCTION < 0.01) THEN
          GET_CO2_RETIREMENT_PRICE = 0.0
       ELSE
         IF(R_CO2_REDUCTION < &
                      RETIREMENTS_CUM_CO2(CO2_ABATEMENT_INDEX(1))) THEN
            COUNTER = 1
         ELSEIF(R_CO2_REDUCTION > &
             RETIREMENTS_CUM_CO2(CO2_ABATEMENT_INDEX( &
                                        ANN_ECON_RETIRE_COUNTER))) THEN
            COUNTER = ANN_ECON_RETIRE_COUNTER
         ELSE

            DO COUNTER = 1, ANN_ECON_RETIRE_COUNTER
               IF(R_CO2_REDUCTION > &
                    RETIREMENTS_CUM_CO2( &
                                   CO2_ABATEMENT_INDEX(COUNTER))) CYCLE
               EXIT

            ENDDO
         ENDIF
         GET_CO2_RETIREMENT_PRICE = RETIREMENTS_CO2_PRICE( &
                                         CO2_ABATEMENT_INDEX(COUNTER))
        ENDIF
      RETURN

      ENTRY GET_CO2_RETIRE_RETRO_PRICE(R_CO2_REDUCTION)

! SEARCH ALGORITHM WOULD BE BETTER.
         IF(R_CO2_REDUCTION < RETIRE_RETRO_CUM_CO2(1)) THEN
            COUNTER = 1
         ELSEIF(R_CO2_REDUCTION > &
             RETIRE_RETRO_CUM_CO2(RETIRE_RETRO_COUNT_AFTER_SORT) ) THEN
            COUNTER = RETIRE_RETRO_COUNT_AFTER_SORT
         ELSE

            DO COUNTER = 1, RETIRE_RETRO_COUNT_AFTER_SORT
               IF(R_CO2_REDUCTION > &
                    RETIRE_RETRO_CUM_CO2(COUNTER) ) CYCLE
               EXIT

            ENDDO
         ENDIF
         GET_CO2_RETIREMENT_PRICE = RETIRE_RETRO_CO2_PRICE(COUNTER)
      RETURN

      ENTRY GET_NEXT_MRX_RETIRE_RETRO(    R_CO2_COUNTER, &
                                          R_RETIRE_OR_RETRO, &
                                          R_CO2_NUNIT, &
                                          R_CO2_TG, &
                                          R_CO2_UNIT_MW, &
                                          R_CO2_UNIT_MWH, &
                                          R_CO2_UNIT_CO2, &
                                          R_CO2_UNIT_PRICE, &
                                          R_CO2_STRIKE_PRICE, &
                                          R_CO2_UNIT_MW_AFTER, &
                                          R_CO2_STRIKE_PRICE_AFTER, &
                                          R_CO2_UNIT_CO2_AFTER, &
                                          R_CO2_END_LIST)

! FOR RETRO NEED:
!  1. CAPACITY BEFORE
!  2. CAPACITY AFTER
!  3. STRIKE BEFORE
!  4. STRIKE AFTER

         GET_NEXT_MRX_RETIRE_RETRO = .FALSE.
         IF(R_CO2_COUNTER > RETIRE_RETRO_COUNT_AFTER_SORT) RETURN
         COUNTER = R_CO2_COUNTER! ASSUMES THE CORRECT COUNTER IS PASSED
         TEMP_I2 = &
                  RETIRE_RETRO_POSITION(RETIRE_RETRO_COUNTER-COUNTER+1)
         R_RETIRE_OR_RETRO = RETIRE_OR_RETRO(TEMP_I2)
         IF(R_RETIRE_OR_RETRO == 3) THEN ! CO2 MARKET
         ELSE
            R_CO2_NUNIT = RETIRE_RETRO_ABATE_INDEX(COUNTER)
            R_CO2_TG = GET_TRANS_GROUP_POSITION( &
                                     TRANSACTION_GROUP_ID(R_CO2_NUNIT))
            R_CO2_UNIT_CO2 = RETIREMENTS_CO2(R_CO2_NUNIT)
            R_CO2_UNIT_MW = RETIREMENTS_MW(R_CO2_NUNIT)
            R_CO2_UNIT_MWH = RETIREMENTS_MWH(R_CO2_NUNIT)
            R_CO2_UNIT_PRICE = RETIRE_RETRO_CO2_PRICE(COUNTER)
            TEMP_I2 = 2
         R_CO2_STRIKE_PRICE = GET_TOTAL_INCREMENTAL_COST(R_CO2_NUNIT, &
                                                           TEMP_I2)
            TEMP_I2 = 1
            R_CO2_STRIKE_PRICE = &
                     MAX(R_CO2_STRIKE_PRICE, &
                            GET_TOTAL_INCREMENTAL_COST(R_CO2_NUNIT, &
                                                            TEMP_I2))

            IF(R_RETIRE_OR_RETRO == 2) THEN ! .AND. 1 == 2) THEN
               CALL RETURN_RETROFIT_PROJECT_IMPACTS( &
                              RETROFIT_PROJ_ID(R_CO2_NUNIT), &
                                            Min_Cap_Change, &
                                            Max_Cap_Change, &
                                            Ave_Heat_Rate_Mult, &
                                            CO2_Cntl_Percent)
               R_CO2_UNIT_MW_AFTER = R_CO2_UNIT_MW * Max_Cap_Change
! THIS NEEDS TO BE REDEFINED
               R_CO2_STRIKE_PRICE_AFTER = R_CO2_STRIKE_PRICE * &
                                                    Ave_Heat_Rate_Mult
               R_CO2_UNIT_CO2_AFTER = R_CO2_UNIT_CO2 * &
                                        (1.0 - CO2_Cntl_Percent*0.01)
            ELSE
               R_CO2_UNIT_MW_AFTER = 0.0
               R_CO2_STRIKE_PRICE_AFTER = 0.0
               R_CO2_UNIT_CO2_AFTER = 0.0
            ENDIF
         ENDIF ! CO2 MARKET

         R_CO2_COUNTER = R_CO2_COUNTER + 1
         IF(R_CO2_COUNTER <= RETIRE_RETRO_COUNT_AFTER_SORT) THEN
            R_CO2_END_LIST = .FALSE.
         ELSE
            R_CO2_END_LIST = .TRUE.
         ENDIF
         GET_NEXT_MRX_RETIRE_RETRO = .TRUE.
      RETURN
 
 1009 FORMAT(1X,I4,5X,I4,4X,I4,2X,A,2X,A,1X,F9.1,F12.0,F12.2)

      ENTRY RETIRE_CO2_THERMAL_UNIT(      R_CO2_NUNIT, &
                                          R_CO2_UNIT_MW, &
                                          R_CO2_UNIT_CO2, &
                                          R_CO2_UNIT_PRICE)


         CURRENT_YEAR = globecom_year+get_BASE_YEAR()
         TEMP_I2 = &
                CL_CAPACITY_TEMP_RETIRE_UNIT(CURRENT_YEAR,R_CO2_NUNIT)

         IF(.NOT. GRX_RETIRE_THIS_YEAR) THEN
            TEMP_R4 = CL_CAPACITY_PLANNING_adj( &
                                    CURRENT_YEAR,R_CO2_NUNIT,2,.TRUE.)
         ENDIF
         WRITE(9,1009) INT4(PRT_ENDPOINT()), &
                       CURRENT_YEAR, &
                       CURRENT_YEAR, &
                       "Retirement", &
                       UNITNM(R_CO2_NUNIT), &
                       R_CO2_UNIT_MW, &
                       R_CO2_UNIT_CO2, &
                       R_CO2_UNIT_PRICE

      RETURN

      ENTRY RETROFIT_ACTIVE_ID(RR_CO2_NUNIT)

        IF(RETROFIT_ACTIVE(RR_CO2_NUNIT)) THEN
            RETROFIT_ACTIVE_ID = RETROFIT_PROJ_ID(RR_CO2_NUNIT)
        ELSE
            RETROFIT_ACTIVE_ID = 0
        ENDIF
      RETURN

      ENTRY RETROFIT_PROJECT_ACTIVE(RR_CO2_NUNIT)

        IF(RETROFIT_ACTIVE(RR_CO2_NUNIT)) THEN
            RETROFIT_ACTIVE_ID = RETROFIT_PROJ_ID(RR_CO2_NUNIT)
            CALL RETRO_PROJECT_AVAILABLE(RETROFIT_ACTIVE_ID,globecom_year, &
                                         TEMP_L1)
            RETROFIT_PROJECT_ACTIVE = TEMP_L1
        ELSE
            RETROFIT_ACTIVE_ID = 0
            RETROFIT_PROJECT_ACTIVE= .FALSE.
        ENDIF
      RETURN

      ENTRY RETROFIT_CO2_THERMAL_UNIT(R_CO2_NUNIT, &
                                          R_CO2_UNIT_MW, &
                                          R_CO2_UNIT_CO2, &
                                          R_CO2_UNIT_PRICE, &
                                          R_CO2_RETIRE_OR_RETRO)

         RETROFIT_ACTIVE(R_CO2_NUNIT) = .TRUE.
         RETROFIT_CO2_THERMAL_UNIT = .TRUE.
         CALL GRX_RETROFIT_UNIT_LIST(R_CO2_NUNIT, &
                                     R_CO2_RETIRE_OR_RETRO, &
                                     R_CO2_UNIT_PRICE)
         CURRENT_YEAR = globecom_year+get_BASE_YEAR()
         WRITE(9,1009)  INT4(PRT_ENDPOINT()), &
                        CURRENT_YEAR, &
                        CURRENT_YEAR, &
                        "Retrofit", &
                        UNITNM(R_CO2_NUNIT), &
                        R_CO2_UNIT_MW, &
                        R_CO2_UNIT_CO2, &
                        R_CO2_UNIT_PRICE

      RETURN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ENTRY TRANS_ANNUAL_UNIT_RETIREMENT() ! (r_nunits_loc)


! POSSIBLE RETIREMENT CRITERIA:

!     LOSES $ X MILLIONS GROSS MARGIN PLUS ANNUALIZED CAPITAL COSTS.
!     SAME AS ABOVE, BUT OVER Y YEARS.

!        TEST CRITERIA
!        IF CRITERIA MET:
!           CHANGE OFF LINE YEAR
!           CALL CL_PLANNING_REMOVALS

         TRANS_ANNUAL_UNIT_RETIREMENT = .FALSE.
      RETURN

      ENTRY GET_THERMAL_STATE_INDEX(r_nunits_loc)

         GET_THERMAL_STATE_INDEX = thermal_State_Index(r_nunits_loc)
      RETURN
      ! RPS=Renewable Portfolio Standards
      ENTRY GET_THERMAL_RPS_DATA(r_nunits_loc,R_PM,R_ST_TG,R_ST, &
                                                        R_RPS_CONTRIB)
         get_primary_mover_index_called_from="cl_units_read:0002"
         R_PM=PRIMARY_MOVER_INDEX_DB(PRIM_MOVER_STR(r_nunits_loc))

         R_ST_TG = State_TG_Index(r_nunits_loc)
         R_ST = thermal_State_Index(r_nunits_loc)
         R_RPS_CONTRIB = RPS_CONTRIBUTION_PERCENT(r_nunits_loc)* 0.01
         GET_THERMAL_RPS_DATA = .TRUE.
      RETURN

      ENTRY PUT_THERMAL_RPS_ENRG_CAP(r_nunits_loc,R_ENRG,R_CAP,R_YEAR, &
                                                           R_ISEAS)
          

         TEMP_RPS_NO = RPS_PROGRAM_NUMBER(r_nunits_loc)
         
         
         get_primary_mover_index_called_from="cl_units_read:0003"
         TEMP_PM = PRIMARY_MOVER_INDEX_DB(PRIM_MOVER_STR(r_nunits_loc))
         LOCAL_RPS_CONTRIB_PERCENT = RPS_CONTRIBUTION_PERCENT(r_nunits_loc)
         IF(LOCAL_RPS_CONTRIB_PERCENT < -0.01) THEN
            LOCAL_RPS_CONTRIB_PERCENT = 0.01 * &
                   ESCALATED_MONTHLY_VALUE(LOCAL_RPS_CONTRIB_PERCENT, &
                       ABS(INT2(RPS_CONTRIBUTION_PERCENT(r_nunits_loc))), &
                                               R_YEAR,INT2(1),INT2(1))
         ELSE
        LOCAL_RPS_CONTRIB_PERCENT = 0.01 * LOCAL_RPS_CONTRIB_PERCENT
         ENDIF
         TEMP_ENRG = R_ENRG * LOCAL_RPS_CONTRIB_PERCENT
         TEMP_CAP = R_CAP * LOCAL_RPS_CONTRIB_PERCENT

         TEMP_L1=PUT_RPS_ENRG_CAP(TEMP_RPS_NO,TEMP_PM, &
                                    TEMP_ENRG,TEMP_CAP)
         PUT_THERMAL_RPS_ENRG_CAP = .TRUE.
      RETURN

      ENTRY GET_CL_BASECASE_MARKET_ID(r_nunits_loc,R_MARKET_ID)  ! L1

         R_MARKET_ID = Bc_MARKET_AREA_ID(r_nunits_loc)
         GET_CL_BASECASE_MARKET_ID = .TRUE.
      RETURN

      ENTRY GET_MONTHLY_FUEL_INDEX(r_nunits_loc,R_ISEAS)

         IF(MONTHLY_FUEL_Idx(r_nunits_loc) == 0) THEN
            GET_MONTHLY_FUEL_INDEX = 1.
         ELSE
            GET_MONTHLY_FUEL_INDEX = &
                  GET_VAR(FLOAT(ABS(MONTHLY_FUEL_Idx(r_nunits_loc))), &
                                             R_ISEAS,UNITNM(r_nunits_loc))
         ENDIF
      RETURN

      ENTRY GET_PRIMARY_MOVER(r_nunits_loc)

         GET_PRIMARY_MOVER = PRIMARY_MOVER(r_nunits_loc)
      RETURN

      ENTRY SET_TRANS_FOR_DATA_BASE()

         IF(ALLOCATED(TRANS_GROUP_FOR_DATA_BASE)) &
                                 DEALLOCATE(TRANS_GROUP_FOR_DATA_BASE)
         ALLOCATE(TRANS_GROUP_FOR_DATA_BASE(MAX_TRANS_DATA_BASES))
         TRANS_GROUP_FOR_DATA_BASE = 0
         DO M = 1, MAX_DAY_TYPES
            DO I = 1, MAX_TRANS_GROUPS
               RTEMP = DAY_TYPE_TRANS_GROUP_PAIR(M,I)
               IF( RTEMP > 0) THEN
                  TRANS_GROUP_FOR_DATA_BASE(RTEMP) = I
               ENDIF
            ENDDO
         ENDDO
         SET_TRANS_FOR_DATA_BASE = MAX_TRANS_DATA_BASES
      RETURN

      ENTRY GET_ASSET_CLASS_NUM(r_nunits_loc)

         GET_ASSET_CLASS_NUM = ASSET_CLS_NUM(r_nunits_loc)
      RETURN

      ENTRY GET_TRANS_FOR_DATA_BASE(R_DATA_BASE)

         GET_TRANS_FOR_DATA_BASE = &
                                TRANS_GROUP_FOR_DATA_BASE(R_DATA_BASE)
      RETURN

      ENTRY GET_DATA_BASE_FOR_TRANS(R_TRANS_GROUP,R_DAY_TYPE)

         GET_DATA_BASE_FOR_TRANS = &
                    DAY_TYPE_TRANS_GROUP_PAIR(R_DAY_TYPE,R_TRANS_GROUP)
      RETURN

      ENTRY GET_DATA_BASE_FOR_UNIT(r_nunits_loc,R_DAY_TYPE)

         IF(R_DAY_TYPE <= 2*MAX_DAY_TYPES) THEN
            GET_DATA_BASE_FOR_UNIT = &
                                DATA_BASE_POSITION(R_DAY_TYPE,r_nunits_loc)
         ELSE
            GET_DATA_BASE_FOR_UNIT = 0
         ENDIF
      RETURN

      ENTRY GET_MAX_TRANS_ID_USED()

         GET_MAX_TRANS_ID_USED =  MAX_TRANS_ID_USED
      RETURN

      ENTRY GET_MAX_DATA_BASES()

         GET_MAX_DATA_BASES = MAX_TRANS_DATA_BASES
      RETURN


      ENTRY GET_TRANSACTION_GROUP(r_nunits_loc) &
                       RESULT(R_TRANSACTION_GROUP)

         R_TRANSACTION_GROUP = TRANSACTION_GROUP_ID(r_nunits_loc)
      RETURN

      ENTRY DAY_TYPE_FOR_RESOURCE(r_nunits_loc)

         DAY_TYPE_FOR_RESOURCE = dTYPE_ID(r_nunits_loc)
      RETURN

      ENTRY NUC_FUEL_PRICE_SOURCE_IS_NFILE(r_nunits_loc)

         NUC_FUEL_PRICE_SOURCE_IS_NFILE = &
                               NUC_FUEL_PRICES_FROM_FUEL_FILE(r_nunits_loc)
      RETURN

      ENTRY RETURN_SHADOW_UNIT_NUMBER(r_nunits_loc)

         RETURN_SHADOW_UNIT_NUMBER = SHADOW_UNIT_NUMBER(r_nunits_loc)
      RETURN

      ENTRY NUMBER_OF_CL_UNITS

         NUMBER_OF_CL_UNITS = get_nunits()
      RETURN

      ENTRY RESET_CL_UNITS

         DO I = 1, INSTALLED_POINTER
            UNIT_NO = TEMP_RETIRED_UNIT_NO(I)
            OFLINE(UNIT_NO) = TEMP_RETIRED_OFF_LINE(I)
       call write_offline_trace("cl_units_read_ext:0014",&
        ofline(unit_no))
            RETIREMENT_CANDIDATE(UNIT_NO) = TEMP_RETIREMENT_SWITCH(I)
            FIRST_RETIREMENT_YEAR(UNIT_NO) = &
                                          TEMP_FIRST_RETIREMENT_YEAR(I)
         ENDDO
         call set_nunits(int(AVAILABLE_CL_UNITS,2))
         RESET_CL_UNITS = get_nunits()
         INSTALLED_POINTER = 0
      RETURN

      ENTRY RETURN_CL_UNITS_B4_ADDITIONS
         bcu=BASE_CL_UNITS ! debugger can't see base_cl_units
         
         if(BASE_CL_UNITS==0) then
            call end_program("cl_units_read_ext:0001 - " // &
            " BASE_CL_UNITS has not been set.")
         end if
         RETURN_CL_UNITS_B4_ADDITIONS = BASE_CL_UNITS
      RETURN

      ENTRY RETURN_UNIT_ADDITIONS

         RETURN_UNIT_ADDITIONS = max(0,get_nunits() - BASE_CL_UNITS)
      RETURN
! ********************************************************************
      ENTRY INCREMENT_HARDWIRED_CL_UNITS

         BASE_PLUS_HARDWIRED_CL_UNITS = get_nunits()
         INCREMENT_HARDWIRED_CL_UNITS = BASE_PLUS_HARDWIRED_CL_UNITS
      RETURN
! ********************************************************************
      ENTRY INCREMENT_AVAILABLE_CL_UNITS

         AVAILABLE_CL_UNITS = get_nunits()
         INCREMENT_AVAILABLE_CL_UNITS = AVAILABLE_CL_UNITS
      RETURN
! ********************************************************************
      ENTRY ADD_NEW_CL_UNIT(UNIT_ON_LINE_YEAR, &
                            OPERATION_LIFE, &
                            POINTER, &
                            R_ON_LINE_MONTH, &
                            R_ASSET_CLASS, &
                            R_ALLOCATION_VECTOR, &
                      R_CL_RESOURCE_ID, & !  PER ANDERSON. 3/13/98.GAT
                            R_POINTER) ! FOR ICAP REPORT. 4/5/02.


            ADD_NEW_CL_UNIT = 0


            call set_nunits(int(get_nunits() + 1,2))

            IF(gc_end_point == 2 .AND. globecom_year == 23 .AND. &
                    get_nunits() == 12008) THEN
               
            ENDIF
            IF(get_nunits() > MAX_CL_UNITS) THEN

               WRITE(SCREEN_MESSAGES,'(A,I5,A)') &
                          'More than ',MAX_CL_UNITS,' capacity-limited'
               CALL MG_LOCATE_WRITE(20,0,trim(SCREEN_MESSAGES), &
                                                        ALL_VERSIONS,1)
               CALL MG_LOCATE_WRITE(21,0, &
                            'get_nunits() are in the capacity-limited', &
                                                        ALL_VERSIONS,1)
               WRITE(SCREEN_MESSAGES,'(A,I5)') &
                 'file after expansion planning for endpoint',gc_end_point
               CALL MG_LOCATE_WRITE(22,0,trim(SCREEN_MESSAGES), &
                                                        ALL_VERSIONS,1)
               er_message='stop requested from Cla_objt SIID25'
               call end_program(er_message)
            ENDIF
            INQUIRE(UNIT=13,OPENED=NEW_CL_UNIT_OPENED)
            IF(.NOT. NEW_CL_UNIT_OPENED) THEN
               WRITE(4,*) "READ ERROR IN EXPANSION OPERATIONS FILE"
               WRITE(4,*) '*** line 2789 CLA_OBJT.FOR ***'
               er_message='See WARNING MESSAGES-cla_objt-5'
               call end_program(er_message)
            ENDIF
            file_name=get_filename_from_unit(13)
            ! call write_log_entry("cla_objt:0102", "Reading from " // &
                ! trim(file_name) // " (unit 13).")

           READ(13,REC=POINTER) del_ord,UNITNM(get_nunits()), &
                           CL_LOAD_TYPE, &
                           TEMP_EXPENSE_ASSIGNMENT, &
                           TEMP_EXPENSE_COLLECTION, &
                  GENGRP(get_nunits()),foshyd%CAP_FRAC_OWN(get_nunits()), &
                  ON_LINE_MONTH_TEMP,OFF_LINE_MONTH(get_nunits()), &
                           FUEL_MIX_PTR(get_nunits()), &
       PBTUCT(get_nunits()),PFESCR(get_nunits()), &
       SBTUCT(get_nunits()), &
                           SFESCR(get_nunits()), &
                        FUELADJ(get_nunits()),EFOR(get_nunits()), &
                           (MNRATE(get_nunits(),M),M=1,12), &
                  VCPMWH_IN(get_nunits()),OMESCR(get_nunits()), &
                           FIXED_COST_IN(get_nunits()), &
                           FIXED_COST_ESCALATOR(get_nunits()), &
                   DISPADJ(get_nunits()),HR_FACTOR(get_nunits()), &
                           (INPUT_MW(M,get_nunits()),M=1,2), &
                           foshyd%CAP_PLANNING_FAC(get_nunits()), &
                           (COEFF(M,get_nunits()),M=1,3), &
                           CL_AI_CAPACITY_RATE(get_nunits()), &
                           CL_AI_CAPACITY_ESCALATOR(get_nunits()), &
                           CL_AI_ENERGY_RATE(get_nunits()), &
                           CL_AI_ENERGY_ESCALATOR(get_nunits()), &
                          P_SO2(get_nunits()),P_NOX(get_nunits()), &
                           P_PARTICULATES(get_nunits()), &
                           LOCAL_CL_POOL_FRAC_OWN(get_nunits()), &
                           P_EMIS_OTH2(get_nunits()), &
                           P_EMIS_OTH3(get_nunits()), &
                           DISPADJ2(get_nunits()), &
                           CL_RESOURCE_ID(get_nunits()), &
                           FUEL_SUPPLY_ID(get_nunits()), &
                           P_NOX_BK2(get_nunits()), &
                           SEC_FUEL_EMISS_PTR(get_nunits()), &
                           EMISS_FUEL_COST(get_nunits()), &
                           EMISS_FUEL_ESCAL(get_nunits()), &
                           EMISS_FUEL_EMISS_PTR(get_nunits()), &
                           EMISS_BLENDING_RATE(get_nunits()), &
                           PRIM_FUEL_EMISS_PTR(get_nunits()), &
                           TEMP_PRIM_FUEL_TYPE_STR, &
                           TEMP_SEC_FUEL_TYPE, &
                           TEMP_EMISS_FUEL_TYPE, &
                           TIE_CONSTRAINT_GROUP(get_nunits()), &
                           MONTHLY_CAPACITY_POINTER(get_nunits()), &
                           ANNUAL_CL_FIXED_COST(get_nunits()), &
                           ANNUAL_CL_FIXED_COST_ESC(get_nunits()), &
                           Mtnc_DAYS(get_nunits()), &
                           DISPATCH_MULT(get_nunits()), &
                           EXCESS_ENERGY_SALES(get_nunits()), &
                           AI_CL_REMAINING_LIFE(get_nunits()), &
                           AI_CL_TAX_LIFE(get_nunits()), &
                           AI_CL_ADR_LIFE(get_nunits()), &
                           ASSET_CLS_NUM(get_nunits()), &
                           ASSET_CLS_VECTOR(get_nunits()), &
                           INTRA_COMPANY_CLSID(get_nunits()), &
                           TEMP_INTRA_COMPANY_TRANSACTION, &
                           SPL_unit_id_loc(get_nunits()), &
                           MIN_CAP_FACTOR(get_nunits()), &
                           MAX_CAP_FACTOR(get_nunits()), &
                           TRANSACTION_GROUP_ID(get_nunits()), &
                           dTYPE_ID(get_nunits()), &
                           TEMP_UNIT_ACTIVE, &
                           Bc_PLANT_ID(get_nunits()), &
                           bcuid, &
                           Bc_MARKET_AREA_ID(get_nunits()), &
                           BC_TRANS_AREA_ID, &
                           BASECASE_PLANT_NERC_SUBID, &
                           BC_PLANT_OWNER_ID, &
                           TEMP_UTILITY_OWNED, & !  UTILITY_OWNED_loc,
                           MONTHLY_FUEL_Idx(get_nunits()), &
                           STARTUP_COSTS(get_nunits()), &
                           TEMP_START_UP_LOGIC, &
                           RAMP_RATE_array(get_nunits()), &
                           MIN_DOWN_TIMEs(get_nunits()), &
                           TEMP_REPORT_THIS_UNIT, &
                        ns_p_fuel_annl%P_FUEL_DELIVERY_1(get_nunits()), &
                        ns_p_fuel_annl%P_FUEL_DELIVERY_2(get_nunits()), &
                        ns_p_fuel_annl%P_FUEL_DELIVERY_3(get_nunits()), &
                           MINimum_UP_TIME(get_nunits()), &
                         HESI_UNIT_ID_NUMs(get_nunits()), & !  PLACEHOLDER
                           FOR_FREQUENCies(get_nunits()), &
                           FOR_DURAtn(get_nunits()), &
                           WINTER_TOTAL_CAPACITY, &
                           TEMP_CONTRIBUTES_TO_SPIN, &
                           H2_UNIT_ID_NUM(get_nunits()), &
                           POWERDAT_PLANT_ID(get_nunits()), &
                           TEMP_APPLY_NOX_SEASON_DATE, &
                           NOX_SsN_DATE(get_nunits()), &
                           RAMP_DOWN_RATEs(get_nunits()), &
                           NOX_CNTRL_PERCENT(get_nunits()), &
                           NOX_CTL_DATE(get_nunits()), &
                           EMRGCY_CAPACITY(get_nunits()), &
                           EMRGENCY_HEATRATE(get_nunits()), &
                           TEMP_MARKET_RESOURCE, &
                           PRIM_MOVER_STR(get_nunits()), &
                           cnty, &
                           st_pvnc, &
                           ns_cla_ox%NOX_VOM(get_nunits()), &
                           ns_cla_ox%NOX_FOM(get_nunits()), &
                           S_FUEL_DELIVERY(get_nunits()), &
                           S_FUEL_DELIVERY_2(get_nunits()), &
                           S_FUEL_DELIVERY_3(get_nunits()), &
                           CONSTANT_POLY_ord, &
                           FIRST_POLY_ord, &
                           SECOND_POLY_ord, &
                           THIRD_POLY_ord, &
                           TEMP_USE_POLY_HEAT_RATES, &
                           NG_UNIT_STATUS, &
                           MONTHLY_MUST_RUN_vctr(get_nunits()), &
                           EMISSION_MARKET_LINK(get_nunits()), &
                           TEMP_WVPA_RATE_TRACKER, &
                           TEMP_ALLOW_DECOMMIT, &
                           MIN_SPIN_Cpcty(get_nunits()), &
                           MAX_SPN_CAP(get_nunits()), &
                           TEMP_WVPA_RES_TRACKER, &
                           TEMP_WVPA_FUEL_TRACKER, &
                           TEMP_MONTE_OR_FREQ, &
                           TEMP_WVPA_MEM_TRACKER, &
                           MONTHLY_dcmt_VECOT(get_nunits()), &
                           TRANS_BUS_ID(get_nunits()), &
                           SOX_CNTRL_PERCENT(get_nunits()), &
                           SOX_CTL_DATE(get_nunits()), &
                           ns_cla_ox%SOX_VOM(get_nunits()), &
                           ns_cla_ox%SOX_FOM(get_nunits()), &
                           LATITUDE(get_nunits()), &
                           LONGITUDE(get_nunits()), &
                           MW_INSIDE_FENCE_ord, &
                          (INTER_BLOCKS(M,get_nunits()),M=1,3), &
                     FIRST_YEAR_DECOM_AVAIL_capex(get_nunits()),  & !  155
                         DECOMMISSIONING_BASE_YR_COST_capex(get_nunits()), &
                     DECOM_CONT_COST_ESCALATION(get_nunits()), &
                   ANNUAL_ENERGY_PLANNING_FACTOR_capex(get_nunits()),   & !  158
                           TEMP_AGGREGATE_THIS_UNIT, &
                           NAME_PLATE_CAPACITY_capex(get_nunits()), &
!   NO, FIXED BLEND, MODEL DETERMINED IF ONLY ONE FUEL ELSE USER OPTION
                           FUEL_BLENDING_IS(get_nunits()),   &
                           FuelRatio_capex(1,get_nunits()), &
                           FuelRatio_capex(2,get_nunits()), &
                           FuelRatio_capex(3,get_nunits()),         & !  164
                           FuelRatio_capex(4,get_nunits()), &
                           FuelRatio_capex(5,get_nunits()),         & !  166
                           BlendableFuelsPtr(1,get_nunits()), & !  167
                           BlendableFuelsPtr(2,get_nunits()), & !  168
                           BlendableFuelsPtr(3,get_nunits()), &
                           BlendableFuelsPtr(4,get_nunits()), &
                           FuelTransportationCost(1,get_nunits()), &
                        FuelTransportationCost(2,get_nunits()),  & !  172
                           FuelTransportationCost(3,get_nunits()), &
                           FuelTransportationCost(4,get_nunits()), &
                           BlendedEnthalpyUp(get_nunits()), &
                           BlendedEnthalpyLo(get_nunits()), &
                    BlendedSO2Up(get_nunits()),                  & !  177
                           BettermentProjectID(get_nunits()), &
                           DECOM_CONTINUING_COST(get_nunits()), &
                    DECOM_CONT_COST_ESCALATION(get_nunits()), &
                           EnrgPatternPointer_capex(get_nunits()), & !  181
                           TEMP_EMISSION_DATA_UNITS, &
                           EmissRedRate(1,get_nunits()), &
                           EmissRedRate(2,get_nunits()), &
                           EmissRedRate(3,get_nunits()), &
                       EmissRedRate(4,get_nunits()),              & !  186
                           EmissRedRate(5,get_nunits()), &
                           EmissMaxRate(1,get_nunits()), &
                           EmissMaxRate(2,get_nunits()), &
                           EmissMaxRate(3,get_nunits()), &
                           EmissMaxRate(4,get_nunits()), &
                           EmissMaxRate(5,get_nunits()), &
                           MaxEnergyLimit(1,get_nunits()), &
                           MaxEnergyLimit(2,get_nunits()), &
                           MaxEnergyLimit(3,get_nunits()), &
                           TECH_TYPE(get_nunits()), &
                           TEMP_LINKED_BETTERMENT_OPTION, &
                           CO2_CONTROL_PERCENT(get_nunits()), &
                           HG_CNTRL_PERCENT(get_nunits()), &
                           OTHER3_CNTRL_PERCENT(get_nunits()), &
                           CO2_CONTROL_DATE(get_nunits()), &
                           HG_CTL_DATE(get_nunits()), &
                           OTH3_CONTROL_DATE(get_nunits()), &
                           ns_cla_ox%CO2_VOM(get_nunits()), &
                           ns_cla_ox%CO2_FOM(get_nunits()), &
                           ns_cla_ox%HG_VOM(get_nunits()), &
                           ns_cla_ox%HG_FOM(get_nunits()), &
                           ns_cla_ox%OTHER3_VOM(get_nunits()), &
                           ns_cla_ox%OTHER3_FOM(get_nunits()), &
                           MARKET_FLOOR(get_nunits()), &
                           MARKET_CEILING(get_nunits()), &
                       STARTUP_COSTS_ESCALATION(get_nunits()),  & !  212
                           CAP_MARKET_TYPE(get_nunits()), &
                           CAPACITY_MARKET_MTH(get_nunits()), &
              ns_cla_decs%capacity_market_pointer_acl(get_nunits()), &
                CAPACITY_MARKET_COST_ASSIGN(get_nunits()), &
                          CAP_MARKET_EXP_COLLECT(get_nunits()), &
                        CAPACITY_MARKET_COIN_ADJ_FCT(get_nunits()), &
                           TEMP_PRIMARY_FUEL_CATEGORY, &
                           TEMP_SECONDARY_FUEL_CATEGORY, &
                           TEMP_EMISSIONS_FUEL_CATEGORY, &
                           RPS_CONTRIBUTION_PERCENT(get_nunits()), &
                           TEMP_RETIREMENT_CANDIDATE, &
                           TEMP_RETROFIT_CANDIDATE, &
                           RETROFIT_PROJ_ID(get_nunits()), &
                           CO2_BASIN_NAME_cla, &
                           CO2_PPLN_DISTANCE(get_nunits()), &
                           STATE_PROVince_NAME, & !  228
                           CO2_RTRO_HEAT_MULT(get_nunits()), &
                           CO2_RTRO_CAP_MULT(get_nunits()), &
                           GUID, &
                           THERMAL_PARENT_ID, & !  232
                           THrm_AGGREGATED_UNIT, &
                           FIRST_RETIREMENT_YEAR(get_nunits()), &
                           UNIT_TYPE, &
                           UNIT_TYPE_CATEGORY, &
                           RPS_PROGRAM_NUMBER(get_nunits())

       ! second array read
       EXPANSION_UNIT_LOCATION=EXPANSION_UNIT_LOCATION !debugstop

            IF(INDEX(TEMP_PRIMARY_FUEL_CATEGORY,"Coal")/=0)THEN
               EXPANSION_UNIT_LOCATION(get_nunits()) = POINTER
            ENDIF

! 10/03/02. FOR REGIONAL CONSOLIDATION. NOTE REASSIGNMENT.

            TRANSACTION_GROUP_ID(get_nunits()) = &
        GET_BELONGS_TO_GROUP(TRANSACTION_GROUP_ID(get_nunits()))


            TRANS_ID = TRANSACTION_GROUP_ID(get_nunits())
            IF(del_ord > 7 .OR. TEMP_UNIT_ACTIVE(1:1) == 'F') THEN

               call set_nunits(int(get_nunits() - 1,2))


               RETURN
            ENDIF
            FIRST_RETIREMENT_YEAR(get_nunits()) = &
                   100*(FIRST_RETIREMENT_YEAR(get_nunits()) - 1900)
            CAP_MARKET_MONTH_NO(get_nunits()) = &
                CAP_MARKET_MONTH_NO_INDEX(CAPACITY_MARKET_MTH &
       (get_nunits()))
! CONVERT TO PERCENT

            IF(INPUT_MW(2,get_nunits()) > 1. .AND. &
       MW_INSIDE_FENCE_ord > .1) THEN
               PERCENT_INSIDE_FENCE = MIN(1., &
                         MW_INSIDE_FENCE_ord/INPUT_MW(2,get_nunits()))
            ELSE
               PERCENT_INSIDE_FENCE = 0.
            ENDIF

            IF(RUN_TRANSACT .AND. &
                     (.NOT.  TRANS_GROUP_ACTIVE_SWITCH(TRANS_ID) .OR. &
                         .NOT. IN_ACTIVE_THERMAL_MARKET_AREA( &
                        Bc_MARKET_AREA_ID(get_nunits()))) .OR. &
                                          PERCENT_INSIDE_FENCE > .995 &
                                                                ) THEN
       WRITE(4,*) UNITNM(get_nunits())," was not added to expansion"
               WRITE(4,*) "because it did not have a valid Transaction"
         WRITE(4,*) "Group ID = ",TRANSACTION_GROUP_ID(get_nunits())

               call set_nunits(int(get_nunits() - 1,2))


               RETURN
            ENDIF

            IF(PERCENT_INSIDE_FENCE > .005) THEN
   foshyd%CAP_FRAC_OWN(get_nunits()) = foshyd%CAP_FRAC_OWN(get_nunits()) * &
                                              (1.-PERCENT_INSIDE_FENCE)
            ENDIF

            IF(EMISSION_MARKET_LINK(get_nunits()) == -9999) THEN
               EMISSION_MARKET_LINK(get_nunits()) = R_ASSET_CLASS
            ENDIF

            POINTER_FOR_NEW_CL_UNIT(get_nunits()) = R_POINTER

! ADDED SO THAT SP COMPATIBLE CL FILE OUTPUT

       EXPENSE_ASSIGNMENT(get_nunits()) = &
       TEMP_EXPENSE_ASSIGNMENT(1:1)
       EXPENSE_COLLECTION(get_nunits()) = &
       TEMP_EXPENSE_COLLECTION(1:1)
            SEC_FUEL_TYPE(get_nunits()) = TEMP_SEC_FUEL_TYPE(1:1)
          EMISS_FUEL_TYPE(get_nunits()) = TEMP_EMISS_FUEL_TYPE(1:1)
            INTRA_CO_transaction_loc(get_nunits()) = &
                                   TEMP_INTRA_COMPANY_TRANSACTION(1:1)
            STARTup_LOGIC(get_nunits()) = TEMP_START_UP_LOGIC(1:1)
         ns_cla_decs%REPORT_THIS_UNIT(get_nunits()) = &
            TEMP_REPORT_THIS_UNIT(1:1)
         CONTRIBUTES_2_SPIN(get_nunits()) = &
       TEMP_CONTRIBUTES_TO_SPIN(1:1)
            APPLY_Nx_SEASON_DATE(get_nunits()) = &
                                       TEMP_APPLY_NOX_SEASON_DATE(1:1)
           MkT_RESOURCE(get_nunits()) = TEMP_MARKET_RESOURCE(1:1)
            USE_PLY_HEAT_RATES = TEMP_USE_POLY_HEAT_RATES(1:1)
       wvpa_rt_tckr(get_nunits()) = TEMP_WVPA_RATE_TRACKER(1:1)
            ALLOW_DcmT(get_nunits()) = TEMP_ALLOW_DECOMMIT(1:1)
         wvpa_res_trkr(get_nunits()) = TEMP_WVPA_RES_TRACKER(1:1)
       WVPA_fl_TCKR_loc(get_nunits()) = TEMP_WVPA_FUEL_TRACKER(1:1)
            monte_or_frq(get_nunits()) = TEMP_MONTE_OR_FREQ(1:1)
          WVPA_MM_TRACKER(get_nunits()) = TEMP_WVPA_MEM_TRACKER(1:1)
            AGGREGATE_THIS_UNIT(get_nunits()) = &
       TEMP_AGGREGATE_THIS_UNIT(1:1)
            EMISSION_DATA_UNITS(get_nunits()) = &
       TEMP_EMISSION_DATA_UNITS(1:1)
            LINKED_BETTERMENT_OPTION(get_nunits()) = &
                                    TEMP_LINKED_BETTERMENT_OPTION(1:1)
            prim_fuelcat(get_nunits()) = &
                                      TEMP_PRIMARY_FUEL_CATEGORY(1:1)
            scndary_fuel_cat(get_nunits()) = &
                                     TEMP_SECONDARY_FUEL_CATEGORY(1:1)
            EMSSNS_FUEL_CATEGORY(get_nunits()) = &
                                      TEMP_EMISSIONS_FUEL_CATEGORY(1:1)
            RETIREMENT_CANDIDATE(get_nunits()) = &
                                         TEMP_RETIREMENT_CANDIDATE(1:1)

            RETROFIT_CANDIDATE(get_nunits()) = &
                                          TEMP_RETROFIT_CANDIDATE(1:1)

            R_ON_LINE_MONTH = ON_LINE_MONTH_TEMP
            ON_LINE_MONTH = ABS(ON_LINE_MONTH_TEMP)
            WRITE(YEAR_CHR,'(I4)') UNIT_ON_LINE_YEAR
            IF(get_nunits() < 10) THEN
               WRITE(NUNITS_CHR,'(I1)') get_nunits()
            ELSEIF(get_nunits() < 100) THEN
               WRITE(NUNITS_CHR,'(I2)') get_nunits()
            ELSEIF(get_nunits() < 1000) THEN
               WRITE(NUNITS_CHR,'(I3)') get_nunits()
            ELSEIF(get_nunits() < 1000) THEN
               WRITE(NUNITS_CHR,'(I4)') get_nunits()
            ELSE
               WRITE(NUNITS_CHR,'(I5)') get_nunits()
            ENDIF

            MARKETSYM_UNIT = MIN(14000,MAX_CL_UNITS,get_nunits())
          MARKETSYM_UNIT_NAME(MARKETSYM_UNIT) = UNITNM(get_nunits())

        UNITNM(get_nunits()) = YEAR_CHR(3:)//'/'//trim(NUNITS_CHR)// &
                                     ' '//trim(UNITNM(get_nunits()))
       call write_unitnm_trace("clun:0003", get_nunits(), &
        unitnm(get_nunits()))
        

            TG = GET_TRANS_GROUP_POSITION( &
       TRANSACTION_GROUP_ID(get_nunits()))

            IF(((CAPACITY_PLANNING_METHOD() == 'MX' .AND. &
                               .NOT. GREEN_MRX_METHOD() == 'GX') .OR. &
               (CAPACITY_PLANNING_METHOD() == 'MX' .AND. &
                GREEN_MRX_METHOD() == 'GX' .AND. GRX_ITERATIONS > 0)) &
               .AND.   (SAVE_SCENARIO_MRX_PLANS == 'A' .OR. &
                     (SAVE_SCENARIO_MRX_PLANS == 'G' .AND. &
                             SAVE_MRX_EXPANSION_PLAN(TG)=='T'))) THEN
               IF(gc_end_point > END_POINT_CHECK .AND. .FALSE. .AND. &
                                      .NOT. MRX_TEXT_FILE_OPEN) THEN
                  CPO_REC = rptrec(9979_2)
                  WRITE(9979,'(A1)',REC=CPO_REC) ENDFILE_MARK
                  CLOSE(9979,IOSTAT=IOS)
                  END_POINT_CHECK = gc_end_point
                  MRX_TEXT_FILE_OPEN = .TRUE.
               ENDIF
               INQUIRE(UNIT=9979,OPENED=MRX_TEXT_FILE_OPEN)
               IF(.NOT. MRX_TEXT_FILE_OPEN) THEN
                  SEQU_NO = MRX_SEQUENCE_NUMBER + gc_end_point - 1
                  file_name=get_mrx_full_filename( &
                        mrx_expansion_plan_file_code,sequ_no)

                  OPEN(9979,FILE=FILE_NAME,ACCESS='DIRECT', &
                         FORM='FORMATTED',RECL=2048, STATUS='REPLACE')
            WRITE(9979,"(A,I4,',')",REC=1)"9,    ,",globecom_year+get_BASE_YEAR()
                  CPO_REC = 2
                  YEAR_CHECK = globecom_year
                  IREC_SAVED = rptrec(9979_2,SAVE_REC=CPO_REC, &
                                 REC_LENGHT=2048_2,FILE_NAME=FILE_NAME)
                  END_POINT_CHECK = gc_end_point
                  MRX_TEXT_FILE_OPEN = .FALSE.
                  RESET_REC = 2
               ELSEIF(rptrec(9979_2,CURRENT_REC=.TRUE.)==2) THEN
            WRITE(9979,"(A,I4,',')",REC=1)"9,    ,",globecom_year+get_BASE_YEAR()
                  YEAR_CHECK = globecom_year
               ELSEIF(globecom_year > YEAR_CHECK .OR. &
                     rptrec(9979_2,CURRENT_REC=.TRUE.)==RESET_REC) THEN
                  RESET_REC = rptrec(9979_2)
                  WRITE(9979,"(A,I4,',')",REC=RESET_REC) &
                                    CRLF//"7,    ,",globecom_year+get_BASE_YEAR()

                  YEAR_CHECK = globecom_year
               ENDIF
             RECLN = '1,'//TRIM(CONVERT_2_STR(UNITNM(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(CL_LOAD_TYPE))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(TEMP_EXPENSE_ASSIGNMENT))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(TEMP_EXPENSE_COLLECTION))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(GENGRP(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(foshyd%CAP_FRAC_OWN(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(ON_LINE_MONTH_TEMP))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(UNIT_ON_LINE_YEAR))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(OFF_LINE_MONTH(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(UNIT_ON_LINE_YEAR &
                                               + OPERATION_LIFE)) ! 10
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(FUEL_MIX_PTR(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(PBTUCT(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(PFESCR(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(SBTUCT(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(SFESCR(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(FUELADJ(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(EFOR(get_nunits())))
               DO M = 1, 12
                  RECLN = TRIM(RECLN)//','// &
                       TRIM(CONVERT_2_STR(MNRATE(get_nunits(),M)))
               ENDDO



               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(VCPMWH_IN(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
              TRIM(CONVERT_2_STR(OMESCR(get_nunits())))               ! 31
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(FIXED_COST_IN(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
        TRIM(CONVERT_2_STR(FIXED_COST_ESCALATOR(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(DISPADJ(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(HR_FACTOR(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(INPUT_MW(1,get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(INPUT_MW(2,get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                TRIM(CONVERT_2_STR(foshyd%CAP_PLANNING_FAC(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(COEFF(1,get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(COEFF(2,get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(COEFF(3,get_nunits())))
               RECLN = TRIM(RECLN)//','//','// &
                  TRIM(CONVERT_2_STR(P_SO2(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(P_NOX(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(P_PARTICULATES(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
           TRIM(CONVERT_2_STR(LOCAL_CL_POOL_FRAC_OWN(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(P_EMIS_OTH2(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(P_EMIS_OTH3(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  "False"  !  PHASE_I_STR))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(DISPADJ2(get_nunits())))  ! 50
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(CL_RESOURCE_ID(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(FUEL_SUPPLY_ID(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(P_NOX_BK2(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
               TRIM(CONVERT_2_STR(SEC_FUEL_EMISS_PTR(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(EMISS_FUEL_COST(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                 TRIM(CONVERT_2_STR(EMISS_FUEL_ESCAL(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
             TRIM(CONVERT_2_STR(EMISS_FUEL_EMISS_PTR(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
              TRIM(CONVERT_2_STR(EMISS_BLENDING_RATE(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
               TRIM(CONVERT_2_STR(PRIM_FUEL_EMISS_PTR(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(TEMP_PRIM_FUEL_TYPE_STR)           ! 60
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(TEMP_SEC_FUEL_TYPE))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(TEMP_EMISS_FUEL_TYPE))
               RECLN = TRIM(RECLN)//','// &
             TRIM(CONVERT_2_STR(TIE_CONSTRAINT_GROUP(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
         TRIM(CONVERT_2_STR(MONTHLY_CAPACITY_POINTER(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
          TRIM(CONVERT_2_STR(CL_AI_CAPACITY_RATE(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
        TRIM(CONVERT_2_STR(CL_AI_CAPACITY_ESCALATOR(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
           TRIM(CONVERT_2_STR(CL_AI_ENERGY_RATE(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
         TRIM(CONVERT_2_STR(CL_AI_ENERGY_ESCALATOR(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
        TRIM(CONVERT_2_STR(AI_CL_REMAINING_LIFE(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
        TRIM(CONVERT_2_STR(ANNUAL_CL_FIXED_COST(get_nunits()))) ! 70
               RECLN = TRIM(RECLN)//','// &
         TRIM(CONVERT_2_STR(ANNUAL_CL_FIXED_COST_ESC(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(Mtnc_DAYS(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(DISPATCH_MULT(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
             TRIM(CONVERT_2_STR(EXCESS_ENERGY_SALES(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(AI_CL_TAX_LIFE(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(AI_CL_ADR_LIFE(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(ASSET_CLS_NUM(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
              TRIM(CONVERT_2_STR(ASSET_CLS_VECTOR(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
          TRIM(CONVERT_2_STR(INTRA_COMPANY_CLSID(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
               TRIM(CONVERT_2_STR(TEMP_INTRA_COMPANY_TRANSACTION)) ! 80
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(SPL_unit_id_loc(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
         TRIM(CONVERT_2_STR(MIN_CAP_FACTOR(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
         TRIM(CONVERT_2_STR(MAX_CAP_FACTOR(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
          TRIM(CONVERT_2_STR(TRANSACTION_GROUP_ID(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(dTYPE_ID(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(TEMP_UNIT_ACTIVE))
               RECLN = TRIM(RECLN)//','// &
                TRIM(CONVERT_2_STR(Bc_PLANT_ID(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(bcuid))
               RECLN = TRIM(RECLN)//','// &
          TRIM(CONVERT_2_STR(Bc_MARKET_AREA_ID(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(BC_TRANS_AREA_ID))  ! 90
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(BASECASE_PLANT_NERC_SUBID))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(BC_PLANT_OWNER_ID))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(TEMP_UTILITY_OWNED))
               RECLN = TRIM(RECLN)//','// &
               TRIM(CONVERT_2_STR(MONTHLY_FUEL_Idx(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(STARTUP_COSTS(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(TEMP_START_UP_LOGIC))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(RAMP_RATE_array(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(MIN_DOWN_TIMEs(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(TEMP_REPORT_THIS_UNIT))
               RECLN = TRIM(RECLN)//','// &
  TRIM(CONVERT_2_STR(ns_p_fuel_annl%P_FUEL_DELIVERY_1(get_nunits())))! 100
               RECLN = TRIM(RECLN)//','// &
       TRIM(CONVERT_2_STR(ns_p_fuel_annl%P_FUEL_DELIVERY_2(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
       TRIM(CONVERT_2_STR(ns_p_fuel_annl%P_FUEL_DELIVERY_3(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(MINimum_UP_TIME(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
         TRIM(CONVERT_2_STR(HESI_UNIT_ID_NUMs(get_nunits()))) ! PLACEHOLDER
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(FOR_FREQUENCies(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(FOR_DURAtn(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(WINTER_TOTAL_CAPACITY))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(TEMP_CONTRIBUTES_TO_SPIN))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(H2_UNIT_ID_NUM(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
            TRIM(CONVERT_2_STR(POWERDAT_PLANT_ID(get_nunits())))  ! 110
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(TEMP_APPLY_NOX_SEASON_DATE))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(NOX_SsN_DATE(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(RAMP_DOWN_RATEs(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
              TRIM(CONVERT_2_STR(NOX_CNTRL_PERCENT(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                 TRIM(CONVERT_2_STR(NOX_CTL_DATE(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
               TRIM(CONVERT_2_STR(EMRGCY_CAPACITY(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
               TRIM(CONVERT_2_STR(EMRGENCY_HEATRATE(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(TEMP_MARKET_RESOURCE))
               RECLN = TRIM(RECLN)//','// &
               TRIM(CONVERT_2_STR(PRIM_MOVER_STR(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(cnty))                     ! 120
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(st_pvnc))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(ns_cla_ox%NOX_VOM(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(ns_cla_ox%NOX_FOM(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
            TRIM(CONVERT_2_STR(S_FUEL_DELIVERY(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
          TRIM(CONVERT_2_STR(S_FUEL_DELIVERY_2(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
          TRIM(CONVERT_2_STR(S_FUEL_DELIVERY_3(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(CONSTANT_POLY_ord))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(FIRST_POLY_ord))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(SECOND_POLY_ord))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(THIRD_POLY_ord)) ! 130
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(TEMP_USE_POLY_HEAT_RATES))
               RECLN = TRIM(RECLN)//','// &
                  "07-Forecasted"  ! NG_UNIT_STATUS,
               RECLN = TRIM(RECLN)//','// &
         TRIM(CONVERT_2_STR(MONTHLY_MUST_RUN_vctr(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
             TRIM(CONVERT_2_STR(EMISSION_MARKET_LINK(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(TEMP_WVPA_RATE_TRACKER))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(TEMP_ALLOW_DECOMMIT))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(MIN_SPIN_Cpcty(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(MAX_SPN_CAP(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(TEMP_WVPA_RES_TRACKER))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(TEMP_WVPA_FUEL_TRACKER))   ! 140
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(TEMP_MONTE_OR_FREQ))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(TEMP_WVPA_MEM_TRACKER))
               RECLN = TRIM(RECLN)//','// &
          TRIM(CONVERT_2_STR(MONTHLY_dcmt_VECOT(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(TRANS_BUS_ID(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
              TRIM(CONVERT_2_STR(SOX_CNTRL_PERCENT(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                 TRIM(CONVERT_2_STR(SOX_CTL_DATE(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(ns_cla_ox%SOX_VOM(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(ns_cla_ox%SOX_FOM(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(LATITUDE(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
               TRIM(CONVERT_2_STR(LONGITUDE(get_nunits())))        ! 150
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(MW_INSIDE_FENCE_ord))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(INTER_BLOCKS(1,get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(INTER_BLOCKS(2,get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(INTER_BLOCKS(3,get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR( &
                       FIRST_YEAR_DECOM_AVAIL_capex(get_nunits())))  ! 155
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR( &
                      DECOMMISSIONING_BASE_YR_COST_capex(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR( &
           DECOM_CONT_COST_ESCALATION(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR( &
             ANNUAL_ENERGY_PLANNING_FACTOR_capex(get_nunits())))   ! 158
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(TEMP_AGGREGATE_THIS_UNIT)) ! 159
               RECLN = TRIM(RECLN)//','// &
           TRIM(CONVERT_2_STR(NAME_PLATE_CAPACITY_capex(get_nunits())))    ! 160
               RECLN = TRIM(RECLN)//','// &
                 TRIM(CONVERT_2_STR(FUEL_BLENDING_IS(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(FuelRatio_capex(1,get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(FuelRatio_capex(2,get_nunits())))
               RECLN = TRIM(RECLN)//','// &
              TRIM(CONVERT_2_STR(FuelRatio_capex(3,get_nunits())))         ! 164
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(FuelRatio_capex(4,get_nunits())))
               RECLN = TRIM(RECLN)//','// &
             TRIM(CONVERT_2_STR(FuelRatio_capex(5,get_nunits())))         ! 166
               RECLN = TRIM(RECLN)//','// &
        TRIM(CONVERT_2_STR(BlendableFuelsPtr(1,get_nunits()))) ! 167
               RECLN = TRIM(RECLN)//','// &
           TRIM(CONVERT_2_STR(BlendableFuelsPtr(2,get_nunits()))) ! 168
               RECLN = TRIM(RECLN)//','// &
           TRIM(CONVERT_2_STR(BlendableFuelsPtr(3,get_nunits())))
               RECLN = TRIM(RECLN)//','// &
            TRIM(CONVERT_2_STR(BlendableFuelsPtr(4,get_nunits())))  ! 170
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR( &
                         FuelTransportationCost(1,get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR( &
                           FuelTransportationCost(2,get_nunits())))  ! 172
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR( &
                           FuelTransportationCost(3,get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR( &
                          FuelTransportationCost(4,get_nunits())))
               RECLN = TRIM(RECLN)//','// &
           TRIM(CONVERT_2_STR(BlendedEnthalpyUp(get_nunits())))      ! 175
               RECLN = TRIM(RECLN)//','// &
               TRIM(CONVERT_2_STR(BlendedEnthalpyLo(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
              TRIM(CONVERT_2_STR(BlendedSO2Up(get_nunits())))      ! 177
               RECLN = TRIM(RECLN)//','// &
               TRIM(CONVERT_2_STR(BettermentProjectID(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
           TRIM(CONVERT_2_STR(DECOM_CONTINUING_COST(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR( &
                  DECOM_CONT_COST_ESCALATION(get_nunits())))   ! 180
               RECLN = TRIM(RECLN)//','// &
             TRIM(CONVERT_2_STR(EnrgPatternPointer_capex(get_nunits()))) ! 181
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(TEMP_EMISSION_DATA_UNITS))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(EmissRedRate(1,get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(EmissRedRate(2,get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(EmissRedRate(3,get_nunits())))
               RECLN = TRIM(RECLN)//','// &
              TRIM(CONVERT_2_STR(EmissRedRate(4,get_nunits())))      ! 186
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(EmissRedRate(5,get_nunits()))) ! 187
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(EmissMaxRate(1,get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(EmissMaxRate(2,get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(EmissMaxRate(3,get_nunits()))) ! 190
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(EmissMaxRate(4,get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(EmissMaxRate(5,get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                TRIM(CONVERT_2_STR(MaxEnergyLimit(1,get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                TRIM(CONVERT_2_STR(MaxEnergyLimit(2,get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                TRIM(CONVERT_2_STR(MaxEnergyLimit(3,get_nunits())))  ! 195
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(TECH_TYPE(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(TEMP_LINKED_BETTERMENT_OPTION))
               RECLN = TRIM(RECLN)//','// &
              TRIM(CONVERT_2_STR(CO2_CONTROL_PERCENT(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
               TRIM(CONVERT_2_STR(HG_CNTRL_PERCENT(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
          TRIM(CONVERT_2_STR(OTHER3_CNTRL_PERCENT(get_nunits()))) ! 200
               RECLN = TRIM(RECLN)//','// &
                 TRIM(CONVERT_2_STR(CO2_CONTROL_DATE(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(HG_CTL_DATE(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
              TRIM(CONVERT_2_STR(OTH3_CONTROL_DATE(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(ns_cla_ox%CO2_VOM(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                TRIM(CONVERT_2_STR(ns_cla_ox%CO2_FOM(get_nunits())))! 205
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(ns_cla_ox%HG_VOM(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(ns_cla_ox%HG_FOM(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(ns_cla_ox%OTHER3_VOM(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(ns_cla_ox%OTHER3_FOM(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(MARKET_FLOOR(get_nunits())))  ! 210
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR(MARKET_CEILING(get_nunits())))  ! 211
               RECLN = TRIM(RECLN)//','// &
        TRIM(CONVERT_2_STR(STARTUP_COSTS_ESCALATION(get_nunits())))  ! 212
               RECLN = TRIM(RECLN)//','// &
            TRIM(CONVERT_2_STR(CAP_MARKET_TYPE(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
            TRIM(CONVERT_2_STR(CAPACITY_MARKET_MTH(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
      TRIM(CONVERT_2_STR( &
       ns_cla_decs%capacity_market_pointer_acl(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
      TRIM(CONVERT_2_STR(CAP_MARKET_COST_ASSIGN(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR( &
                        CAP_MARKET_EXP_COLLECT(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR( &
                    CAPACITY_MARKET_COIN_ADJ_FCT(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR( &
                                 TEMP_PRIMARY_FUEL_CATEGORY))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR( &
                                 TEMP_SECONDARY_FUEL_CATEGORY))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR( &
                                 TEMP_EMISSIONS_FUEL_CATEGORY))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR( &
                            RPS_CONTRIBUTION_PERCENT(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR( &
                                 TEMP_RETIREMENT_CANDIDATE))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR( &
                                 TEMP_RETROFIT_CANDIDATE))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR( &
                           RETROFIT_PROJ_ID(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR( &
                           CO2_BASIN_NAME_cla))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR( &
                           CO2_PPLN_DISTANCE(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR( &
                           STATE_PROVince_NAME))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR( &
                           CO2_RTRO_HEAT_MULT(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR( &
                           CO2_RTRO_CAP_MULT(get_nunits())))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR( &
                           GUID))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR( &
                           THERMAL_PARENT_ID))
               RECLN = TRIM(RECLN)//','// &
                  TRIM(CONVERT_2_STR( &
                           THrm_AGGREGATED_UNIT))
               RECLN = CRLF//TRIM(RECLN)//',,,,,,,,,,,,,,,,,,,,,'
               CPO_REC = rptrec(9979_2)
               WRITE(9979,'(A)',REC=CPO_REC) TRIM(RECLN)
            ENDIF

            IF(LOCAL_MW(1) > 0. .AND. LOCAL_MW(2) > 0.) THEN
               IF(USE_PLY_HEAT_RATES == 'T' .AND. &
                                            .NOT. TURN_OFF_POLY) THEN

                  COEFF(1,get_nunits()) = 1000. * &
                     (CONSTANT_POLY_ord + &
                      FIRST_POLY_ord * LOCAL_MW(1) + &
                      SECOND_POLY_ord * &
                                 LOCAL_MW(1)*LOCAL_MW(1) + &
                      THIRD_POLY_ord * &
                           LOCAL_MW(1)*LOCAL_MW(1)* &
                                                    LOCAL_MW(1))/ &
                                                      LOCAL_MW(1)
                  COEFF(2,get_nunits()) = 1000. * &
                      (FIRST_POLY_ord + &
                      2. * SECOND_POLY_ord * LOCAL_MW(1) + &
                      3. * THIRD_POLY_ord * &
                                  LOCAL_MW(1)*LOCAL_MW(1))
                  COEFF(3,get_nunits()) = 1000. * &
                      (FIRST_POLY_ord + &
                      2. * SECOND_POLY_ord * LOCAL_MW(2) + &
                      3. * THIRD_POLY_ord * &
                                  LOCAL_MW(2)*LOCAL_MW(2))

               ENDIF


! 1/17/03.
! A+BX+CX^2

               IF(LOCAL_MW(2) > LOCAL_MW(1)) THEN

                  CUBIC_HEAT_CURVE(1,get_nunits()) = &
                (COEFF(2,get_nunits())-COEFF(3,get_nunits()))/ &
                           (-2.*(LOCAL_MW(2)-LOCAL_MW(1)))
                  CUBIC_HEAT_CURVE(2,get_nunits()) = &
                        COEFF(2,get_nunits()) - &
             (COEFF(2,get_nunits()) - COEFF(3,get_nunits())) * &
                              LOCAL_MW(1)/ &
                           (-1.*(LOCAL_MW(2)-LOCAL_MW(1)))
                  CUBIC_HEAT_CURVE(3,get_nunits()) = &
       ((COEFF(1,get_nunits())-CUBIC_HEAT_CURVE(2,get_nunits()))* &
                                     LOCAL_MW(1)) - &
                              CUBIC_HEAT_CURVE(1,get_nunits())* &
                                   LOCAL_MW(1)*LOCAL_MW(1)
               ELSE
               ENDIF
            ELSE ! NEED TO CHECK THIS WITH TOM.
         CUBIC_HEAT_CURVE(2,get_nunits()) = COEFF(1,get_nunits())
            ENDIF

            CUBIC_HEAT_CURVE(0,get_nunits()) = CONSTANT_POLY_ord
            CUBIC_HEAT_CURVE(2,get_nunits()) = SECOND_POLY_ord
            CUBIC_HEAT_CURVE(3,get_nunits()) = THIRD_POLY_ord
           IF(CONSTANT_POLY_ord <= 0. .AND. FIRST_POLY_ord <= 0. .AND. &
                                             SECOND_POLY_ord <= 0.) THEN
               IF(COEFF(1,get_nunits()) > 0.) THEN
                   CUBIC_HEAT_CURVE(1,get_nunits()) = &
       COEFF(1,get_nunits())*.001
               ELSE
                  CUBIC_HEAT_CURVE(1,get_nunits()) = 10.
               ENDIF
            ELSE
               CUBIC_HEAT_CURVE(1,get_nunits()) = FIRST_POLY_ord
            ENDIF

            TEMP_STATE = st_pvnc(1:2)
            TEMP_I2 = STATE_ID_LOOKUP(TEMP_STATE)
            thermal_State_Index(get_nunits()) = TEMP_I2
            TEMP_I2 = STATE_ID_LOOKUP(st_pvnc)
            State_TG_Index(get_nunits()) = TEMP_I2
            IF( TEMP_I2 > 0 .AND. &
                           STATE_PROVINCE_INDEX(TEMP_I2) == 0) THEN
               MAX_STATE_PROVINCE_NO = MAX_STATE_PROVINCE_NO + 1
               STATE_PROVINCE_ADDRESS(MAX_STATE_PROVINCE_NO) = TEMP_I2
               STATE_PROVINCE_INDEX(TEMP_I2) = MAX_STATE_PROVINCE_NO
               STATE_PROVINCE_NAMES(MAX_STATE_PROVINCE_NO) = &
                                                        st_pvnc
            ENDIF
            UNIT_STATE_PROVINCE_INDEX(get_nunits()) = &
                                         STATE_PROVINCE_INDEX(TEMP_I2)

            TEMP_I2 = STATE_2_GAS_REGION_LOOKUP(st_pvnc)
            IF(YES_GSP_IS_ST_TG(TEMP_I2)) THEN
               TEMP_STATE = st_pvnc
            ELSE
               TEMP_STATE = st_pvnc(1:2)
            ENDIF
            UNIT_GAS_REGION_INDEX(get_nunits()) = &
                                 STATE_2_GAS_REGION_LOOKUP(TEMP_STATE)

            ON_LINE_YEAR = UNIT_ON_LINE_YEAR
            OFF_LINE_YEAR(get_nunits()) = UNIT_ON_LINE_YEAR + &
       OPERATION_LIFE

      ONLINE(get_nunits()) = 100*(ON_LINE_YEAR-1900) + ON_LINE_MONTH
            OFLINE(get_nunits()) = &
       100*(OFF_LINE_YEAR(get_nunits())-1900) + &
                                       OFF_LINE_MONTH(get_nunits())
       call write_offline_trace("cl_units_read_ext:0015",&
        ofline(get_nunits()))

            IF(Mtnc_DAYS(get_nunits()) /= 0.) &
                                     SAVE_DETAILED_MAINTENANCE = .TRUE.
            ASSET_CLS_NUM(get_nunits()) = R_ASSET_CLASS
            ASSET_CLS_VECTOR(get_nunits()) = R_ALLOCATION_VECTOR

          CL_RESOURCE_ID(get_nunits()) = R_CL_RESOURCE_ID ! 3/13/98. GAT.

            NOX_SsN_DATE(get_nunits()) = &
       NOX_SsN_DATE(get_nunits()) + 10000

            NOX_CNTRL_PERCENT(get_nunits()) = &
         MIN(100.,MAX(0.,1.-NOX_CNTRL_PERCENT(get_nunits())/100.))
            NOX_CTL_DATE(get_nunits()) = &
       NOX_CTL_DATE(get_nunits()) + 10000

            SOX_CNTRL_PERCENT(get_nunits()) = &
         MIN(100.,MAX(0.,1.-SOX_CNTRL_PERCENT(get_nunits())/100.))
            SOX_CTL_DATE(get_nunits()) = &
       SOX_CTL_DATE(get_nunits()) + 10000
            CO2_CONTROL_PERCENT(get_nunits()) = &
         MIN(100.,MAX(0.,1.-CO2_CONTROL_PERCENT(get_nunits())/100.))
            CO2_CONTROL_DATE(get_nunits()) = &
       CO2_CONTROL_DATE(get_nunits()) + 10000
            HG_CNTRL_PERCENT(get_nunits()) = &
       MIN(100.,MAX(0.,1.-HG_CNTRL_PERCENT(get_nunits())/100.))
            HG_CTL_DATE(get_nunits()) = &
       HG_CTL_DATE(get_nunits()) + 10000
            OTHER3_CNTRL_PERCENT(get_nunits()) = &
                   MIN(100., &
               MAX(0.,1.-OTHER3_CNTRL_PERCENT(get_nunits())/100.))
            OTH3_CONTROL_DATE(get_nunits()) = &
                       OTH3_CONTROL_DATE(get_nunits()) + 10000

            LOCAL_RESOURCE_ID = CL_RESOURCE_ID(get_nunits())
            IF(NUMBER_OF_RESOURCE_ID(LOCAL_RESOURCE_ID) == 0) THEN
               ! FIRST OCCURRENCE
               RESOURCE_ID_TO_UNIT(LOCAL_RESOURCE_ID) = get_nunits()
            ENDIF
            NUMBER_OF_RESOURCE_ID(LOCAL_RESOURCE_ID) = &
                           NUMBER_OF_RESOURCE_ID(LOCAL_RESOURCE_ID) + 1

            IF(STARTup_LOGIC(get_nunits()) /= 'F') THEN
               TOTAL_START_UP_UNITS = TOTAL_START_UP_UNITS + 1
               START_UP_INDEX(get_nunits()) = TOTAL_START_UP_UNITS
            ENDIF

            CALL SET_ASSET_CLASSES(ASSET_CLS_NUM(get_nunits()), &
                                   NUMBER_OF_CAP_LIMITED_CLASSES, &
                       ns_cla_decs%MAX_CAP_LIMITED_CLASS_ID_NUM, &
                                   CAP_LIMITED_CLASS_POINTER)

            CALL UPC(TEMP_PRIM_FUEL_TYPE_STR,PRIM_ft_STR)
            PRIMARY_MOVER(get_nunits()) = &
                             FUEL_TYPE_2_PRIM_MOVER(PRIM_ft_STR)

            TRANS_ID = TRANSACTION_GROUP_ID(get_nunits())
            IF(TRANS_ID > 0 .AND. ON_LINE_YEAR <= gc_last_study_year) THEN
               DAY_ID = dTYPE_ID(get_nunits())
               MAX_TRANS_ID_USED = MAX(MAX_TRANS_ID_USED, TRANS_ID)
               IF(DAY_ID > 0 .AND. DAY_ID <= MAX_DAY_TYPES ) THEN
        IF(DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,TRANS_ID) == 0) THEN
                     MAX_TRANS_DATA_BASES = MAX_TRANS_DATA_BASES + 1
                     DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,TRANS_ID) = &
                                                  MAX_TRANS_DATA_BASES
                  ENDIF

                  DATA_BASE_POSITION(1,get_nunits()) = &
                            DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,TRANS_ID)
                  DATA_BASE_POSITION(2,get_nunits()) = &
                              DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,0)
               ELSEIF(DAY_ID < 0) THEN
                  DAY_TYPE_COUNTER = 1
                  DO
              DAY_ID = INT(GET_VAR(FLOAT(dTYPE_ID(get_nunits())), &
                                     DAY_TYPE_COUNTER,"FIND DAY TYPE"))
                     IF(DAY_ID <= 0 .OR. DAY_ID > MAX_DAY_TYPES) EXIT
                  IF(DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,TRANS_ID) == 0) &
                                                                  THEN
                        MAX_TRANS_DATA_BASES = MAX_TRANS_DATA_BASES + 1
                        DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,TRANS_ID) = &
                                                 MAX_TRANS_DATA_BASES
                     ENDIF
      !
            DATA_BASE_POSITION(2*DAY_TYPE_COUNTER-1,get_nunits()) = &
                           DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,TRANS_ID)
               DATA_BASE_POSITION(2*DAY_TYPE_COUNTER,get_nunits()) = &
                                    DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,0)
                     DAY_TYPE_COUNTER = DAY_TYPE_COUNTER + 1
                  ENDDO
               ELSEIF(DAY_ID == 0) THEN
                  DO M = 1, MAX_DAY_TYPES
                    IF(DAY_TYPE_TRANS_GROUP_PAIR(M,TRANS_ID) == 0) THEN
                        MAX_TRANS_DATA_BASES = MAX_TRANS_DATA_BASES + 1
                        DAY_TYPE_TRANS_GROUP_PAIR(M,TRANS_ID) = &
                                                  MAX_TRANS_DATA_BASES
                     ENDIF
                     DATA_BASE_POSITION(2*M-1,get_nunits()) = &
                                 DAY_TYPE_TRANS_GROUP_PAIR(M,TRANS_ID)
                     DATA_BASE_POSITION(2*M,get_nunits()) = &
                                        DAY_TYPE_TRANS_GROUP_PAIR(M,0)
                  ENDDO
               ELSE
                  WRITE(4,*) "DAY TYPE DESIGNATION",DAY_ID
                  WRITE(4,*) "OUTSIDE THE RANGE OF POSSIBLE OUTCOMES"
                  WRITE(4,*) "MAXIMUM DAY TYPES DEFINED = ", &
                                                          MAX_DAY_TYPES
                  WRITE(4,*) '*** line 3020 CLA_OBJT.FOR ***'
                  er_message='See WARNING MESSAGES-cla_objt-4'
                  call end_program(er_message)
               ENDIF
            ENDIF

            DISP_ADDER_ESCR(get_nunits()) = PFESCR(get_nunits())
            PRIM_FUEL_TYPE(get_nunits()) = PRIM_ft_STR(1:1)
            NUC_FUEL_PRICES_FROM_FUEL_FILE(get_nunits()) = &
                        INDEX(PRIM_ft_STR,'Nuc-NF') /= 0 .OR. &
                                INDEX(PRIM_ft_STR,'NUC-NF') /= 0
            LDTYPE(get_nunits()) = CL_LOAD_TYPE(1:1)
            ECONOMY_TRANS_TYPE(get_nunits()) = 'T'
            IF(CL_LOAD_TYPE(2:2) == 'S') then
                ECONOMY_TRANS_TYPE(get_nunits())='S'
            endif
            IF(CL_LOAD_TYPE(2:2) == 'B') then
                ECONOMY_TRANS_TYPE(get_nunits())='B'
            endif

            IF(CL_LOAD_TYPE(2:2) == 'N') then
                ECONOMY_TRANS_TYPE(get_nunits())='N'
            endif

            IF(EFORS0) EFOR(get_nunits()) = 0.
            IF(SORS0) THEN
               Mtnc_DAYS(get_nunits()) = 0
               DO M = 1, 12
                  MNRATE(get_nunits(),M) = 0.
               ENDDO
            ENDIF
            PBTUCT_SAVE(get_nunits()) = PBTUCT(get_nunits())
            SBTUCT_SAVE(get_nunits()) = SBTUCT(get_nunits())
            FUELADJ_SAVE(get_nunits()) = FUELADJ(get_nunits())
           VCPMWH_IN_SAVE(get_nunits()) = VCPMWH_IN(get_nunits())
      FIXED_COST_SAVE(get_nunits()) = FIXED_COST_IN(get_nunits())
            ANNUAL_CL_FIXED_COST_SAVE(get_nunits()) = &
                                 ANNUAL_CL_FIXED_COST(get_nunits())
            DISPATCH_MULT_SAVE(get_nunits()) = &
       DISPATCH_MULT(get_nunits())
            EXCESS_ENERGY_SALES_SAVE(get_nunits()) = &
                                   EXCESS_ENERGY_SALES(get_nunits())
            DISPADJ_SAVE(get_nunits()) = DISPADJ(get_nunits())
            DISPADJ2_SAVE(get_nunits()) = DISPADJ2(get_nunits())
            CL_AI_CAPACITY_RATE_SAVE(get_nunits())= &
       CL_AI_CAPACITY_RATE(get_nunits())
            CL_AI_ENERGY_RATE_SAVE(get_nunits()) = &
       CL_AI_ENERGY_RATE(get_nunits())
            CL_AI_REMAINING_LIFE_SAVE(get_nunits()) = &
                                AI_CL_REMAINING_LIFE(get_nunits())
            DISPADJ2_SAVE(get_nunits()) = DISPADJ2(get_nunits())
            EMISS_FUEL_COST_SAVE(get_nunits()) = &
       EMISS_FUEL_COST(get_nunits())

            ADD_NEW_CL_UNIT = get_nunits()


            SHADOW_UNIT_NUMBER(get_nunits()) = 0
            IF(FUEL_SUPPLY_ID(get_nunits()) > 0 .AND. &
                          FUEL_MIX_PTR(get_nunits()) > 0. .AND. &
                        STARTup_LOGIC(get_nunits()) == 'F') THEN
               I = get_nunits()
               call set_nunits(int(I + 1,2))

               SHADOW_UNIT_NUMBER(get_nunits()) = I
               SHADOW_UNIT_NUMBER(I) = get_nunits()
               UNITNM(get_nunits()) = '+'//UNITNM(I)
       call write_unitnm_trace("clun:0004", get_nunits(), &
        unitnm(get_nunits()))
               FUEL_MIX_PTR(get_nunits()) = 0.
               LDTYPE(get_nunits()) = 'S'
               EXPENSE_ASSIGNMENT(get_nunits()) = &
       EXPENSE_ASSIGNMENT(I)
            EXPENSE_COLLECTION(get_nunits()) = EXPENSE_COLLECTION(I)
               ASSET_CLS_NUM(get_nunits()) = ASSET_CLS_NUM(I)
            ASSET_CLS_VECTOR(get_nunits()) = ASSET_CLS_VECTOR(I)
      INTRA_COMPANY_CLSID(get_nunits())=INTRA_COMPANY_CLSID(I)
               INTRA_CO_transaction_loc(get_nunits()) = &
                                           INTRA_CO_transaction_loc(I)
               SPL_unit_id_loc(get_nunits()) = SPL_unit_id_loc(I)
               MIN_CAP_FACTOR(get_nunits()) = &
                                             MIN_CAP_FACTOR(I)
               MAX_CAP_FACTOR(get_nunits()) = &
                                             MAX_CAP_FACTOR(I)
       TRANSACTION_GROUP_ID(get_nunits()) = TRANSACTION_GROUP_ID(I)
               ns_cla_decs%REPORT_THIS_UNIT(get_nunits()) = &
               ns_cla_decs%REPORT_THIS_UNIT(I)
            ECONOMY_TRANS_TYPE(get_nunits()) = ECONOMY_TRANS_TYPE(I)
            CL_CAP_AREA_LINKED(get_nunits()) = CL_CAP_AREA_LINKED(I)
               GENGRP(get_nunits()) = GENGRP(I)
               foshyd%CAP_FRAC_OWN(get_nunits()) = foshyd%CAP_FRAC_OWN(I)
               ONLINE(get_nunits()) = ONLINE(I)
               OFLINE(get_nunits()) = OFLINE(I)
       call write_offline_trace("cl_units_read_ext:0016", &
        ofline(get_nunits()))
               OFF_LINE_YEAR(get_nunits()) = OFF_LINE_YEAR(I)
               SBTUCT(get_nunits()) = SBTUCT(I)
               SBTUCT(I) = 0.
               SFESCR(get_nunits()) = SFESCR(I)
               SFESCR(I) = 0
               FUELADJ(get_nunits()) = FUELADJ(I)
               EFOR(get_nunits()) = EFOR(I)
               Mtnc_DAYS(get_nunits()) = Mtnc_DAYS(I)
               DO M = 1, 12
                  MNRATE(get_nunits(),M) = MNRATE(I,M)
               ENDDO
               VCPMWH_IN(get_nunits()) = VCPMWH_IN(I)
               OMESCR(get_nunits()) = OMESCR(I)
               FIXED_COST_IN(get_nunits()) = 0.
               FIXED_COST_SAVE(get_nunits()) = 0.
               ANNUAL_CL_FIXED_COST(get_nunits()) = 0.
               ANNUAL_CL_FIXED_COST_SAVE(get_nunits()) = 0.
               FIXED_COST_ESCALATOR(get_nunits()) = 0.
               ANNUAL_CL_FIXED_COST_ESC(get_nunits()) = 0.
               DISPADJ(get_nunits()) = DISPADJ(I)
               DISPATCH_MULT(get_nunits()) = DISPATCH_MULT(I)
          EXCESS_ENERGY_SALES(get_nunits()) = EXCESS_ENERGY_SALES(I)
               HR_FACTOR(get_nunits()) = HR_FACTOR(I)
               INPUT_MW(1,get_nunits()) = INPUT_MW(1,I)
               INPUT_MW(2,get_nunits()) = INPUT_MW(2,I)
               COEFF(1,get_nunits()) = COEFF(1,I)
               COEFF(2,get_nunits()) = COEFF(2,I)
               COEFF(3,get_nunits()) = COEFF(3,I)
         CL_AI_CAPACITY_RATE(get_nunits()) = CL_AI_CAPACITY_RATE(I)
               CL_AI_CAPACITY_ESCALATOR(get_nunits()) = &
                                          CL_AI_CAPACITY_ESCALATOR(I)
               CL_AI_ENERGY_RATE(get_nunits()) = CL_AI_ENERGY_RATE(I)
       CL_AI_ENERGY_ESCALATOR(get_nunits())=CL_AI_ENERGY_ESCALATOR(I)
        AI_CL_REMAINING_LIFE(get_nunits()) = AI_CL_REMAINING_LIFE(I)
               AI_CL_TAX_LIFE(get_nunits()) = AI_CL_TAX_LIFE(I)
               AI_CL_ADR_LIFE(get_nunits()) = AI_CL_ADR_LIFE(I)
        SEC_FUEL_EMISS_PTR(get_nunits()) = SEC_FUEL_EMISS_PTR(I)
               SEC_FUEL_EMISS_PTR(I) = 0
               CL_RESOURCE_ID(get_nunits()) = CL_RESOURCE_ID(I)
               DISPADJ2(get_nunits()) = DISPADJ2(I)
               PHASE_I_UNIT(get_nunits()) = PHASE_I_UNIT(I)
               LOCAL_CL_POOL_FRAC_OWN(get_nunits()) = &
                                            LOCAL_CL_POOL_FRAC_OWN(I)

! SINCE ONLY THE SECONDARY FUEL IS USED ZERO PRIMARLY
! AND EMISSIONS FUEL DATA

               FUEL_SUPPLY_ID(get_nunits()) = FUEL_SUPPLY_ID(I)
               foshyd%CAP_PLANNING_FAC(get_nunits()) = 0.
               PBTUCT(get_nunits()) = PBTUCT(I)
               PFESCR(get_nunits()) = PFESCR(I)
               DISP_ADDER_ESCR(get_nunits()) = DISP_ADDER_ESCR(I)
               P_SO2(get_nunits()) = P_SO2(I)
               P_NOX(get_nunits()) = P_NOX(I)
               P_PARTICULATES(get_nunits()) = P_PARTICULATES(I)
               P_EMIS_OTH2(get_nunits()) = P_EMIS_OTH2(I)
               P_EMIS_OTH3(get_nunits()) = P_EMIS_OTH3(I)
               P_NOX_BK2(get_nunits()) = P_NOX_BK2(I)
               EMISS_FUEL_COST(get_nunits()) = EMISS_FUEL_COST(I)
               EMISS_FUEL_ESCAL(get_nunits()) = EMISS_FUEL_ESCAL(I)
               EMISS_FUEL_EMISS_PTR(get_nunits()) = 0.
           EMISS_BLENDING_RATE(get_nunits()) = EMISS_BLENDING_RATE(I)
           PRIM_FUEL_EMISS_PTR(get_nunits()) = PRIM_FUEL_EMISS_PTR(I)
! 6/10/93. GAT. NO ALLOCATION OF FUEL TYPES BETWEEN SHADOWS
               PRIM_FUEL_TYPE(get_nunits()) = " "
              NUC_FUEL_PRICES_FROM_FUEL_FILE(get_nunits()) = .FALSE.
               SEC_FUEL_TYPE(get_nunits()) = SEC_FUEL_TYPE(I)
               SEC_FUEL_TYPE(I) = " "
               EMISS_FUEL_TYPE(get_nunits()) = " "
             TIE_CONSTRAINT_GROUP(get_nunits()) = &
       TIE_CONSTRAINT_GROUP(I)
               PBTUCT_SAVE(get_nunits()) = PBTUCT(I)

               SBTUCT_SAVE(get_nunits()) = SBTUCT(get_nunits())
               FUELADJ_SAVE(get_nunits()) = FUELADJ(I)
               VCPMWH_IN_SAVE(get_nunits()) = VCPMWH_IN(I)

               DISPATCH_MULT_SAVE(get_nunits()) = DISPATCH_MULT(I)
      EXCESS_ENERGY_SALES_SAVE(get_nunits()) = EXCESS_ENERGY_SALES(I)
               MONTHLY_CAPACITY_POINTER(get_nunits()) = &
                                            MONTHLY_CAPACITY_POINTER(I)
               DISPADJ_SAVE(get_nunits()) = DISPADJ(I)
               DISPADJ2_SAVE(get_nunits()) = DISPADJ2(I)
      CL_AI_CAPACITY_RATE_SAVE(get_nunits()) = CL_AI_CAPACITY_RATE(I)
          CL_AI_ENERGY_RATE_SAVE(get_nunits()) = CL_AI_ENERGY_RATE(I)
      CL_AI_REMAINING_LIFE_SAVE(get_nunits())=AI_CL_REMAINING_LIFE(I)
               DISPADJ2_SAVE(get_nunits()) = DISPADJ2(I)
              EMISS_FUEL_COST_SAVE(get_nunits()) = EMISS_FUEL_COST(I)

               TRANS_ID = TRANSACTION_GROUP_ID(get_nunits())

               IF(TRANS_ID > 0 .AND. &
                                  ON_LINE_YEAR <= gc_last_study_year) THEN
                  DAY_ID = dTYPE_ID(get_nunits())
                  MAX_TRANS_ID_USED = MAX(MAX_TRANS_ID_USED,TRANS_ID)
                  IF(DAY_ID > 0 .AND. DAY_ID <= MAX_DAY_TYPES ) THEN
                     IF(DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,TRANS_ID) &
                                                             == 0) THEN
                        MAX_TRANS_DATA_BASES = MAX_TRANS_DATA_BASES + 1
                        DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,TRANS_ID) = &
                                                   MAX_TRANS_DATA_BASES
                     ENDIF

                     DATA_BASE_POSITION(1,get_nunits()) = &
                             DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,TRANS_ID)
                     DATA_BASE_POSITION(2,get_nunits()) = &
                                    DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,0)
                  ELSEIF(DAY_ID < 0) THEN
                     DAY_TYPE_COUNTER = 1
                     DO
              DAY_ID = INT(GET_VAR(FLOAT(dTYPE_ID(get_nunits())), &
                                     DAY_TYPE_COUNTER,"FIND DAY TYPE"))
                      IF(DAY_ID <= 0 .OR. DAY_ID > MAX_DAY_TYPES) EXIT
                        IF(DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,TRANS_ID) &
                                                            == 0) THEN
                           MAX_TRANS_DATA_BASES = &
                                               MAX_TRANS_DATA_BASES + 1
                         DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,TRANS_ID) = &
                                                   MAX_TRANS_DATA_BASES
                        ENDIF
      !
                        DATA_BASE_POSITION( &
                            2*DAY_TYPE_COUNTER-1,get_nunits()) = &
                             DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,TRANS_ID)
              DATA_BASE_POSITION(2*DAY_TYPE_COUNTER,get_nunits()) = &
                                    DAY_TYPE_TRANS_GROUP_PAIR(DAY_ID,0)
                        DAY_TYPE_COUNTER = DAY_TYPE_COUNTER + 1
                     ENDDO
                  ELSEIF(DAY_ID == 0) THEN
                     DO M = 1, MAX_DAY_TYPES
                        IF(DAY_TYPE_TRANS_GROUP_PAIR(M,TRANS_ID) &
                                                            == 0) THEN
                           MAX_TRANS_DATA_BASES = &
                                               MAX_TRANS_DATA_BASES + 1
                           DAY_TYPE_TRANS_GROUP_PAIR(M,TRANS_ID) = &
                                                   MAX_TRANS_DATA_BASES
                        ENDIF
                        DATA_BASE_POSITION(2*M-1,get_nunits()) = &
                                 DAY_TYPE_TRANS_GROUP_PAIR(M,TRANS_ID)
                        DATA_BASE_POSITION(2*M,get_nunits()) = &
                                        DAY_TYPE_TRANS_GROUP_PAIR(M,0)
                     ENDDO
                  ELSE
                     WRITE(4,*) "DAY TYPE DESIGNATION",DAY_ID
                    WRITE(4,*) "OUTSIDE THE RANGE OF POSSIBLE OUTCOMES"
                     WRITE(4,*) &
                           "MAXIMUM DAY TYPES DEFINED = ",MAX_DAY_TYPES
                     WRITE(4,*) '*** line 3278 CLA_OBJT.FOR ***'
                     er_message='See WARNING MESSAGES-cla_objt-3'
                     call end_program(er_message)
                  ENDIF
               ENDIF
            ENDIF ! SHADOW get_nunits()

! 051310. TO GENERATE MONTHLY CAPACITIES
         IF(get_nunits() > 0) THEN
            SAVE_DETAILED_MAINTENANCE = .TRUE.
         ENDIF
      RETURN
! ***************************************************************
      ENTRY INIT_CAP_MARKET_REVENUE()
! ***************************************************************

! 091307. CAPACITY MARKET

         MON_CAP_SALE_MW_FROM = 0.0
         MON_CAP_SALE_REV_FROM = 0.0
         MON_CAP_PUCH_MW_FROM = 0.0
         MON_CAP_PUCH_COST_FROM = 0.0

         CL_TG_CAP_MARKET_MW = 0.0
         CL_TG_CAP_MARKET_REV = 0.0

         INIT_CAP_MARKET_REVENUE = .TRUE.
      RETURN
! ***************************************************************
      ENTRY CALC_CL_CAP_MARKETS(R_YEAR,R_MONTH)
! ***************************************************************
         IF(.NOT. ALLOCATED(ANNUAL_CL_CAPACITY)) THEN
            WRITE(4,*) 'CANNOT CALCULATE CAPACITY MARKETS'
            WRITE(4,*) 'THE MONTHLY CAPACITY IS NOT ALLOCATED'
            IF(.NOT. SAVE_DETAILED_MAINTENANCE) THEN
               WRITE(4,*) 'NO DETAILED MAINTENANCE'
            ENDIF
         ENDIF

         DATE_CHECK = 100*(R_YEAR+get_BASE_YEAR()-1900) + R_MONTH
         ! date_check=12301/12301
         ! r_month=1/1
         ! r_year=1/1
         ! capacity_market_pointer(i)=71/71
         DO I = 1, get_nunits()
            IF(ns_cla_decs%capacity_market_pointer_acl(I) <= 0 .OR. &
                               CAP_MARKET_TYPE(I) == 'NONE') then
                CYCLE
            end if
            CAP_MARKET_SALE =  (CAP_MARKET_COST_ASSIGN(I) == &
                                                 'Capacity Sales      ')

            PRIMARY_FUEL_ID = &
               -1.*(10000. + ns_cla_decs%capacity_market_pointer_acl(I))

            

            CALL GET_A_CAP_MARKET_PRICE( &
              PRIMARY_FUEL_ID, &
              CAP_MARKET_RATE, &
              R_MONTH, &
              int(R_YEAR,2))


! IF AN ANNUAL CAPACITY CHARGE IN A GIVEN MONTH

            IF(CAP_MARKET_MONTH_NO(I) /= 13) THEN
               TEMP_I2 = CAP_MARKET_MONTH_NO(I)
            ELSE

               TEMP_I2 = R_MONTH
            ENDIF
            CAP_MARKET_RATE = CAP_MARKET_RATE ! CONVERSION TO $mm
            IF(CAP_MARKET_TYPE(I) == 'ICAP')THEN
               PLAN_FACTOR = 1.0
            ELSE
               IF(foshyd%CAP_PLANNING_FAC(I) < 0.) THEN
                  LOCAL_YEAR = R_YEAR - get_BASE_YEAR()
                  PLAN_FACTOR = &
                     GET_VAR(foshyd%CAP_PLANNING_FAC(I), &
                     LOCAL_YEAR, UNITNM(I))
               ELSE
                  PLAN_FACTOR = foshyd%CAP_PLANNING_FAC(I)
               ENDIF

            ENDIF

            PLAN_FACTOR = PLAN_FACTOR * &
                                CAPACITY_MARKET_COIN_ADJ_FCT(I)

            IF(ONLINE(I) > DATE_CHECK .OR. OFLINE(I) < DATE_CHECK) THEN
               TEMP_R4 = 0.
            ELSE
               TEMP_R4 = ANNUAL_CL_CAPACITY(2,I,TEMP_I2)
            ENDIF

            IF(CAP_MARKET_SALE)THEN            ! L*1
               MON_CAP_SALE_MW_FROM(I) = TEMP_R4 * PLAN_FACTOR
               MON_CAP_SALE_REV_FROM(I) = 1000.* &
                       MON_CAP_SALE_MW_FROM(I) * CAP_MARKET_RATE
            ELSE
               MON_CAP_PUCH_MW_FROM(I) = TEMP_R4 * PLAN_FACTOR
               MON_CAP_PUCH_COST_FROM(I) = 1000.* &
                       MON_CAP_PUCH_MW_FROM(I) * CAP_MARKET_RATE
            ENDIF
! TRANSACT C REPORTING
            TG = TRANSACTION_GROUP_ID(I)
            TG = GET_TRANS_GROUP_POSITION(TG)
            CL_TG_CAP_MARKET_MW(TG) = CL_TG_CAP_MARKET_MW(TG) + &
                                   MON_CAP_PUCH_MW_FROM(I) + &
                                   MON_CAP_SALE_MW_FROM(I)
            CL_TG_CAP_MARKET_REV(TG) = CL_TG_CAP_MARKET_REV(TG) + &
                  (MON_CAP_SALE_REV_FROM(I) + &
                                      MON_CAP_PUCH_COST_FROM(I))
         ENDDO
         CALC_CL_CAP_MARKETS = .TRUE.
      RETURN
! ***************************************************************
      ENTRY GET_CAP_MARKET_REVENUE(R_UNIT_NO)
! ***************************************************************
         GET_CAP_MARKET_REVENUE = .000001 * &
                  (MON_CAP_SALE_REV_FROM(R_UNIT_NO) + &
                                    MON_CAP_PUCH_COST_FROM(R_UNIT_NO))
      RETURN
! ***************************************************************
      ENTRY GET_CL_TG_CAP_MARKET_MW(R_TG)
! ***************************************************************
         GET_CL_TG_CAP_MARKET_MW =  CL_TG_CAP_MARKET_MW(R_TG)
      RETURN
! ***************************************************************
      ENTRY GET_CL_TG_CAP_MARKET_REV(R_TG)
! ***************************************************************
         GET_CL_TG_CAP_MARKET_REV =  CL_TG_CAP_MARKET_REV(R_TG)
      RETURN
! ***************************************************************
      ENTRY CAP_MARKET_ACTIVE(R_UNIT_NO)
! ***************************************************************
       IF(ns_cla_decs%capacity_market_pointer_acl(R_UNIT_NO) > 0 .AND. &
                       CAP_MARKET_TYPE(R_UNIT_NO) /= 'NONE') THEN
            CAP_MARKET_ACTIVE = .TRUE.
         ELSE
            CAP_MARKET_ACTIVE = .FALSE.
         ENDIF
      RETURN
! ***************************************************************
      ENTRY GET_PRIMARY_MOVER_INDEX(R_UNIT_NO)
         
         GET_PRIMARY_MOVER_INDEX = &
                   PRIMARY_MOVER_INDEX_DB(PRIM_MOVER_STR(R_UNIT_NO))


         IF(GET_PRIMARY_MOVER_INDEX == 10 .AND. &
                                   PRIMARY_MOVER(R_UNIT_NO) > 3 ) THEN
            GET_PRIMARY_MOVER_INDEX = 7
         ENDIF
      RETURN
! ***************************************************************
      ENTRY GET_POINTER_FOR_NEW_CL_UNIT(R_UNIT_NO)
! ***************************************************************
         GET_POINTER_FOR_NEW_CL_UNIT = &
                                    POINTER_FOR_NEW_CL_UNIT(R_UNIT_NO)
      RETURN

      ENTRY GET_CL_AFTER_PEAK(R_TG,R_YEAR)


         GET_CL_AFTER_PEAK = CL_TG_AFTER_PEAK(R_TG, &
                                             MIN(R_YEAR,get_globecom_study_period()))

      RETURN

      ENTRY GET_CL_TG_RETIRE(R_TG,R_YEAR)


         GET_CL_TG_RETIRE = CL_TG_RETIRE(R_TG, &
                                             MIN(R_YEAR,get_globecom_study_period()))
      RETURN

      ENTRY GET_CL_TG_CAP(R_FT,R_TG,R_YEAR,R_ST_P)

         
         year_argument=min(r_year, get_globecom_study_period())

         
         lb_1=lbound(cl_tg_cap, 1)
         ub_1=ubound(cl_tg_cap,1)
         lb_2=lbound(cl_tg_cap, 2)
         ub_2=ubound(cl_tg_cap,2)
         lb_3=lbound(cl_tg_cap,3)
         ub_3=ubound(cl_tg_cap,3)
         lb_4=lbound(cl_tg_cap,4)
         ub_4=ubound(cl_tg_cap,4)

         if(lb_1>r_ft .or. ub_1<r_ft &
         .or. lb_2>r_tg .or. ub_2<r_tg  &
         .or. lb_3>year_argument .or. ub_3<year_argument &
         .or. lb_4>r_st_p .or. ub_4<r_st_p) then
            call end_program("cl_units_read_ext:-0008 - attempting " // &
            "out-of-bounds access on cl_tg_cap array.")
         end if
         
         GET_CL_TG_CAP = CL_TG_CAP(R_FT,R_TG, &
                                      MIN(R_YEAR,get_globecom_study_period()),R_ST_P)
      RETURN

      ENTRY GET_NEWGEN_CAP_BY_INDEX(R_NG,R_TG,R_YEAR)


         GET_NEWGEN_CAP_BY_INDEX = NEWGEN_CAP_BY_INDEX(R_NG,R_TG, &
                                             MIN(R_YEAR,get_globecom_study_period()))
      RETURN
! ***************************************************************
      ENTRY FIRST_YEAR_CL_CAPACITY(R_YEAR,R_UNIT_NO)
! ***************************************************************
         UNIT_NO = R_UNIT_NO
         UNIT_CAPACITY = INPUT_MW(2,UNIT_NO)
         YEAR_START = R_YEAR - get_BASE_YEAR()
         YEAR_END = OFF_LINE_YEAR(UNIT_NO)

         CAP_MW = UNIT_CAPACITY
         IF(CAP_MW < 0.) THEN
          CAP_MW = GET_CL_MW_FROM_POINTR(CAP_MW,YEAR_START)
! ADDED PER JONES. 3/13/98. GAT.
            IF(CAP_MW < 0.) &
          CAP_MW = GET_CL_MW_FROM_POINTR(real(CAP_MW),&
            int(PEAK_MONTH(YEAR_START),2))

            CAP_MW = MAX(0.,CAP_MW)
         ELSE

! IF NO POINTER HAS BEEN SPECIFIED FOR THE PEAK MONTH

            CAP_MW = MAX(0.,CAP_MW)
         ENDIF

         IF(MONTHLY_CAPACITY_POINTER(UNIT_NO) /= 0 .AND. &
                               .NOT. CL_CAP_AREA_LINKED(UNIT_NO)) THEN
            R_POINTR = FLOAT(ABS(MONTHLY_CAPACITY_POINTER(UNIT_NO)))
            MONTH_MULT = GET_CL_MW_FROM_POINTR(real(R_POINTR), &
                                      int(PEAK_MONTH(YEAR_START),2))
            IF(MONTH_MULT > 2. .AND. CAP_MW /= 0.) THEN
               CAP_MW = MONTH_MULT
            ELSE
               CAP_MW = MONTH_MULT * CAP_MW
            ENDIF
         ELSEIF(MONTHLY_CAPACITY_POINTER(UNIT_NO) < 0 .AND. &
                                    CL_CAP_AREA_LINKED(UNIT_NO)) THEN
            LOCAL_NAME = UNITNM(UNIT_NO)
            LOCAL_POINTER = ABS(MONTHLY_CAPACITY_POINTER(UNIT_NO))
            CAP_MW = MIN(GET_CLASS_PEAK_NET_DSM_FOR_CAP( &
                         YEAR_START,LOCAL_NAME,LOCAL_POINTER),CAP_MW)
         ENDIF

! EFFECTIVE MW'S OF PLANNING CAPACITY

         IF(foshyd%CAP_PLANNING_FAC(UNIT_NO) < 0.) THEN
            PLAN_FACTOR = &
                        GET_VAR(foshyd%CAP_PLANNING_FAC(UNIT_NO), &
                                              globecom_year, &
                                              UNITNM(UNIT_NO))
         ELSE
            PLAN_FACTOR = foshyd%CAP_PLANNING_FAC(UNIT_NO)
         ENDIF
         CAP_MW = CAP_MW * foshyd%CAP_FRAC_OWN(UNIT_NO) *  &
            PLAN_FACTOR * 0.01

         FIRST_YEAR_CL_CAPACITY = CAP_MW
      RETURN
! ***************************************************************
      ENTRY CL_PLANNING_CAPACITY(R_CAP_TYPE,R_YEAR)
! ***************************************************************
         IF(ALLOCATED(CL_ANN_CAP)) THEN
            IF(R_YEAR > get_globecom_study_period()) THEN
               CL_PLANNING_CAPACITY = &
                  CL_ANN_CAP(R_CAP_TYPE,get_globecom_study_period(),1) + &
                  CL_ANN_CAP(R_CAP_TYPE,get_globecom_study_period(),2)
            ELSE
            CL_PLANNING_CAPACITY = CL_ANN_CAP(R_CAP_TYPE,R_YEAR,1) + &
                                      CL_ANN_CAP(R_CAP_TYPE,R_YEAR,2)
            ENDIF
         ELSE
            CL_PLANNING_CAPACITY = 0.
         ENDIF
      RETURN
! ***************************************************************
      ENTRY NEW_CL_PLANNING_CAPACITY(R_CAP_TYPE,R_YEAR)
! ***************************************************************
         IF(ALLOCATED(CL_ANN_CAP)) THEN
            IF(R_YEAR > get_globecom_study_period()) THEN
               NEW_CL_PLANNING_CAPACITY = &
                  CL_ANN_CAP(R_CAP_TYPE,get_globecom_study_period(),2)
            ELSE
               NEW_CL_PLANNING_CAPACITY = &
                  CL_ANN_CAP(R_CAP_TYPE,R_YEAR,2)
            ENDIF
         ELSE
            NEW_CL_PLANNING_CAPACITY = 0.
         ENDIF
      RETURN
! ***************************************************************
      ENTRY CL_PLANNING_LOAD_REDUCTION(R_YEAR)
! ***************************************************************
         IF(ALLOCATED(CL_ANNUAL_LOAD_REDUCTION)) THEN
            IF(R_YEAR > get_globecom_study_period()) THEN
               CL_PLANNING_LOAD_REDUCTION = &
                     CL_ANNUAL_LOAD_REDUCTION(get_globecom_study_period())
            ELSE
               CL_PLANNING_LOAD_REDUCTION = &
                     CL_ANNUAL_LOAD_REDUCTION(R_YEAR)
            ENDIF
         ELSE
            CL_PLANNING_LOAD_REDUCTION = 0.
         ENDIF
      RETURN
! ***************************************************************
      ENTRY OLD_CL_PLANNING_CAPACITY(R_CAP_TYPE,R_YEAR)
! ***************************************************************
         if(r_year==5) then
            r_year=r_year ! Debugstop
         end if
         IF(ALLOCATED(CL_ANN_CAP)) THEN
            IF(R_YEAR > get_globecom_study_period()) THEN
               OLD_CL_PLANNING_CAPACITY = &
                  CL_ANN_CAP(R_CAP_TYPE,get_globecom_study_period(),1)
            ELSE
               OLD_CL_PLANNING_CAPACITY = &
                  CL_ANN_CAP(R_CAP_TYPE,R_YEAR,1)
            ENDIF
         ELSE
            OLD_CL_PLANNING_CAPACITY = 0.
         ENDIF
         
            
      RETURN
! ***************************************************************
      ENTRY SET_AI_CL_REMAINING_LIFE(AI_REMAINING_LIFE)
! ***************************************************************
         AI_CL_REMAINING_LIFE(get_nunits()) = AI_REMAINING_LIFE
         CL_AI_REMAINING_LIFE_SAVE(get_nunits()) = AI_REMAINING_LIFE
         SET_AI_CL_REMAINING_LIFE = &
       AI_CL_REMAINING_LIFE(get_nunits())
      RETURN
! ***************************************************************
      ENTRY CL_RESET_PRICES

         DO I = 1, get_nunits()
            PBTUCT(I) = PBTUCT_SAVE(I)
            SBTUCT(I) = SBTUCT_SAVE(I)
            FUELADJ(I) = FUELADJ_SAVE(I)
            VCPMWH_IN(I) = VCPMWH_IN_SAVE(I)
            FIXED_COST_IN(I) = FIXED_COST_SAVE(I)
            ANNUAL_CL_FIXED_COST(I) = ANNUAL_CL_FIXED_COST_SAVE(I)
            DISPATCH_MULT(I) = DISPATCH_MULT_SAVE(I)
            DISPADJ(I) = DISPADJ_SAVE(I)
            DISPADJ2(I) = DISPADJ2_SAVE(I)
            CL_AI_CAPACITY_RATE(I) = CL_AI_CAPACITY_RATE_SAVE(I)
            CL_AI_ENERGY_RATE(I) = CL_AI_ENERGY_RATE_SAVE(I)
            AI_CL_REMAINING_LIFE(I) = CL_AI_REMAINING_LIFE_SAVE(I)
            DISPADJ2(I) = DISPADJ2_SAVE(I)
            EMISS_FUEL_COST(I) = EMISS_FUEL_COST_SAVE(I)
         ENDDO
         CL_RESET_PRICES = .TRUE.
      RETURN

      ENTRY UPDATE_PRIMARY_FUEL_MIX_RATIO(R_YEAR)

         UPDATE_PRIMARY_FUEL_MIX_RATIO = 0
         DO I = 1, get_nunits()
            IF(FUEL_MIX_PTR(I) <= -2.) THEN
               IF(R_YEAR > AVAIL_DATA_YEARS) THEN
                FUELMX(I) = GET_VAR(FUEL_MIX_PTR(I),AVAIL_DATA_YEARS, &
                                                            UNITNM(I))
               ELSE
                  FUELMX(I) = GET_VAR(FUEL_MIX_PTR(I),R_YEAR,UNITNM(I))
               ENDIF
               UPDATE_PRIMARY_FUEL_MIX_RATIO = 1 + &
                                         UPDATE_PRIMARY_FUEL_MIX_RATIO
            ELSE
               FUELMX(I) = FUEL_MIX_PTR(I)
            ENDIF
         ENDDO
      RETURN

      ENTRY CL_PLANNING_ADDITIONS

         CL_PLANNING_ADDITIONS = AVAILABLE_CL_UNITS - &
                                          BASE_PLUS_HARDWIRED_CL_UNITS
      RETURN

      ENTRY RETURN_CL_UNITS_AFTER_HARD_ADDS

         RETURN_CL_UNITS_AFTER_HARD_ADDS = BASE_PLUS_HARDWIRED_CL_UNITS
      RETURN

      ENTRY DETAILED_MAINTENANCE_IS_ACTIVE

         DETAILED_MAINTENANCE_IS_ACTIVE = SAVE_DETAILED_MAINTENANCE
      RETURN

      ENTRY DETAIL_MAINTENANCE_NOT_ACTIVE

         SAVE_DETAILED_MAINTENANCE = .FALSE.
         DETAIL_MAINTENANCE_NOT_ACTIVE = SAVE_DETAILED_MAINTENANCE
      RETURN


      ENTRY CALC_ANNUAL_CAP_AND_MAINT(R_YEAR)
        if(file_trace_clm==0) then
            file_trace_clm=open_trace("cl_maint.trace", rq_clm)
        end if
         if(file_trace_cacm==0) then
         
       file_trace_cacm=open_trace("calc_annual_cap_maint.trace", rq_cacm)
       call write_trace_message(file_trace_cacm, &
        "Tracing calc_annual_cap_and_maint")
        
         endif
         call write_trace_int2(file_trace_cacm, "R_YEAR ", R_YEAR)

         MAINTENANCE_IS_ACCUMULATED = ACCUMULATE_MAINTENANCE()


         MAINTENANCE_IS_ACCUMULATED = .FALSE.

         IF(.NOT. (MAINTENANCE_REPORT() .OR. &
                     SAVE_DETAILED_MAINTENANCE .OR. &
                           YES_UNIT_COMMITMENT_LOGIC() .OR. &
                                 MAINTENANCE_IS_ACCUMULATED) .OR. &
                                             get_nunits()==0) THEN
            CALC_ANNUAL_CAP_AND_MAINT = .FALSE.
            
          call write_trace_message(file_trace_cacm, "1. Exiting routine.")
          
            RETURN
         ENDIF
         IF(ALLOCATED(ANNUAL_CL_CAPACITY)) &
               DEALLOCATE(ANNUAL_CL_CAPACITY,ANNUAL_CL_MAINTENANCE, &
                           ANNUAL_CL_MAINT_MW,MAINT_DIVISIBLE)
         ALLOCATE(ANNUAL_CL_CAPACITY(2,get_nunits(),12))
      ALLOCATE(ANNUAL_CL_MAINTENANCE(get_nunits(),13))
      ALLOCATE(ANNUAL_CL_MAINT_MW(get_nunits()))
      ALLOCATE(MAINT_DIVISIBLE(get_nunits()))

         IF(ALLOCATED(MAINT_INDEX)) then 
             DEALLOCATE(MAINT_INDEX, &
             MAINT_INDEX_MW, &
             MAINT_INDEX_DAYS, &
             MAINT_INDEX_MONTHLY_MW)
         endif
         
         ALLOCATE(MAINT_INDEX(get_nunits()))
         ALLOCATE(MAINT_INDEX_MW(get_nunits()))
         ALLOCATE(MAINT_INDEX_DAYS(get_nunits()))
         ALLOCATE(MAINT_INDEX_MONTHLY_MW(get_nunits(),13))


         IF(ALLOCATED(MAINTENANCE_DAYS)) DEALLOCATE(MAINTENANCE_DAYS)
         ALLOCATE(MAINTENANCE_DAYS(get_nunits()))
         MAINTENANCE_DAYS = 0.
         ANNUAL_CL_CAPACITY = 0.
         ANNUAL_CL_MAINTENANCE = 0.
         ANNUAL_CL_MAINT_MW = 0.
         MAINT_DIVISIBLE = FALSE_BYTE
         OFFSET_MAINTENANCE_ACTIVE = OFFSET_MAINTENANCE_VECTORS()
         BASE_DATE = (get_BASE_YEAR() + R_YEAR - 1900) * 100
         BASE_YEAR_DATE = (get_BASE_YEAR() - 1900) * 100
! MkT_RESOURCE 7/5/02. FOR TVA
         IF(ALLOCATED(MARKET_RESOURCE_INDEX)) &
                                  DEALLOCATE(MARKET_RESOURCE_INDEX, &
                                               MARKET_RESOURCE_COUNTER)
         ALLOCATE(MARKET_RESOURCE_INDEX(get_nunits()))
         ALLOCATE(MARKET_RESOURCE_COUNTER(get_nunits()))

         MARKET_RESOURCE_COUNTER = 0
         MARKET_COUNT = 0

         LAST_ACTIVE_UNIT = get_nunits()

         
         call write_trace_int2(file_trace_cacm, "1. LAST_ACTIVE_UNIT", &
            LAST_ACTIVE_UNIT)
       call write_trace_real4s(file_trace_cacm, "MAINTENANCE_PENALTY=", &
        MAINTENANCE_PENALTY)

         TEMP_L = GET_MONTHLY_MAINTENANCE_PENALTY(MAINTENANCE_PENALTY)
         call write_trace_bool1(file_trace_cacm, "TEMP_L=",TEMP_L)
         
         YES_REGIONAL_OUTAGES_ACTIVE = REGIONAL_PARAMS_ACTIVE()
         
        call write_trace_bool1(file_trace_cacm, &
            "YES_REGIONAL_OUTAGES_ACTIVE=", YES_REGIONAL_OUTAGES_ACTIVE)

         FREQUENCY_DURATION = YES_FREQUENCY_DURATION()
         
         call write_trace_bool1(file_trace_cacm, "FREQUENCY_DURATION=", &
            FREQUENCY_DURATION)
            
        call write_trace_bool1(file_trace_cacm, "UNIT_FREQUENCY_DURATION=", &
            UNIT_FREQUENCY_DURATION)
            
            
         IF(FREQUENCY_DURATION .OR. UNIT_FREQUENCY_DURATION) THEN
            MAX_FO_PER_MONTH = 8

         ELSE
            MAX_FO_PER_MONTH = 1
         ENDIF
         
         call write_trace_int2(file_trace_cacm, "MAX_FO_PER_MONTH=", &
            MAX_FO_PER_MONTH)
            
         call write_trace_int2(file_trace_cacm, "NUNITS=", get_nunits())

         CALL INIT_MAINT_SCHEDULE(MAX_FO_PER_MONTH,get_nunits())
         
         
       call write_trace_int2(file_trace_cacm, "MAX_MAINT_PLANNING_AREAS=", &
            MAX_MAINT_PLANNING_AREAS)
            
         if(MAX_MAINT_PLANNING_AREAS==1) then
            call end_program("cl_units_read_ext:0010 - " // &
                "MAX_MAINT_PLANNING_AREAS=" // &
                trim(itos(int(MAX_MAINT_PLANNING_AREAS))))
         end if
            
         DO pa_ord = 1, MAX_MAINT_PLANNING_AREAS
         
            call write_trace_int2(file_trace_cacm, "1. PA=", pa_ord)
            
            MAINT_INDEX_MW = 0.
            MAINT_INDEX_DAYS = 0.
            MAINT_INDEX_MONTHLY_MW = 0.

            MAINT_INDEX = 0
            K = 0

       call write_trace_bool1(file_trace_cacm, &
        "REGIONAL_PA_MAINT_SCHEDULING=",  REGIONAL_PA_MAINT_SCHEDULING)
                
            if(REGIONAL_PA_MAINT_SCHEDULING) then
                call write_trace_message(file_trace_cacm, &
                   "Entering REGIONAL_PA_MAINT_SCHEDULING loop")
            else
                call write_trace_message(file_trace_cacm, &
                   "NOT entering REGIONAL_PA_MAINT_SCHEDULING loop")
            end if
            
       call write_trace_bool1(file_trace_cacm, &
       "REGIONAL_TG_MAINT_SCHEDULING=", REGIONAL_TG_MAINT_SCHEDULING)
            
            IF(REGIONAL_PA_MAINT_SCHEDULING) THEN

               DO I = 1, 12

                  MONTHLY_MAINTENANCE_PEAKS(I) = &
                             GET_PA_PEAK(pa_ord,I) * MAINTENANCE_PENALTY(I)
             call write_trace_int2(file_trace_cacm, "I", I)
      call write_trace_real4s(file_trace_cacm, &
      "MONTHLY_MAINTENANCE_PEAKS changed to", &
       MONTHLY_MAINTENANCE_PEAKS)

               ENDDO
               



            ELSEIF(REGIONAL_TG_MAINT_SCHEDULING) THEN
               DO I = 1, 12
                  MONTHLY_MAINTENANCE_PEAKS(I) = &
                    GET_TRANS_GROUP_PEAK(pa_ord,I) * MAINTENANCE_PENALTY(I)
             call write_trace_int2(file_trace_cacm, "2. I", I)
      call write_trace_real4s(file_trace_cacm, &
      "MONTHLY_MAINTENANCE_PEAKS changed to", &
       MONTHLY_MAINTENANCE_PEAKS)
               ENDDO
               
               
      call write_trace_int2(file_trace_cacm, "2. pa=", pa_ord)
      call write_trace_int2(file_trace_cacm, "I=", I)
      
       call write_trace_real4s(file_trace_cacm, &
      "MAINTENANCE_PENALTY changed", MAINTENANCE_PENALTY)
      
      


            ELSE
               MONTHLY_MAINTENANCE_PEAKS(1) = FUT_PEAK(3,1) * &
                                                 MAINTENANCE_PENALTY(1)
               MONTHLY_MAINTENANCE_PEAKS(2) = FUT_PEAK(3,2) * &
                                                 MAINTENANCE_PENALTY(2)
               MONTHLY_MAINTENANCE_PEAKS(3) = FUT_PEAK(3,3) * &
                                                 MAINTENANCE_PENALTY(3)
               MONTHLY_MAINTENANCE_PEAKS(4) = FUT_PEAK(3,4) * &
                                                 MAINTENANCE_PENALTY(4)
               MONTHLY_MAINTENANCE_PEAKS(5) = FUT_PEAK(3,5) * &
                                                 MAINTENANCE_PENALTY(5)
               MONTHLY_MAINTENANCE_PEAKS(6) = FUT_PEAK(3,6) * &
                                                 MAINTENANCE_PENALTY(6)
               MONTHLY_MAINTENANCE_PEAKS(7) = FUT_PEAK(3,7) * &
                                                 MAINTENANCE_PENALTY(7)
               MONTHLY_MAINTENANCE_PEAKS(8) = FUT_PEAK(3,8) * &
                                                 MAINTENANCE_PENALTY(8)
               MONTHLY_MAINTENANCE_PEAKS(9) = FUT_PEAK(3,9) * &
                                                 MAINTENANCE_PENALTY(9)
               MONTHLY_MAINTENANCE_PEAKS(10) = FUT_PEAK(3,10) * &
                                                MAINTENANCE_PENALTY(10)
               MONTHLY_MAINTENANCE_PEAKS(11) = FUT_PEAK(3,11) * &
                                                MAINTENANCE_PENALTY(11)
               MONTHLY_MAINTENANCE_PEAKS(12) = FUT_PEAK(3,12) * &
                                                MAINTENANCE_PENALTY(12)
                                                
                                                
               call write_trace_real4s(file_trace_cacm,  &
        "MONTHLY_MAINTENANCE_PEAKS=", MONTHLY_MAINTENANCE_PEAKS(1:12))
        
        
            ENDIF
            
            call write_trace_int2(file_trace_cacm, "NUNITS=", get_nunits())
            
            DO I = 1, get_nunits()

! 2/13/96 CORRECTION FOR SHADOW get_nunits()

!           IF(LDTYPE(I) == 'L') CYCLE
               IF(LDTYPE(I) == 'S') THEN
               
        call write_trace_string(file_trace_cacm, "LDTYPE(I)=", LDTYPE(I))
        call write_trace_message(file_trace_cacm, "...will cycle.")
        
                  SHADOW_UNITS_ACTIVE = .TRUE.
                  CYCLE
               ENDIF
           call write_trace_int2(file_trace_cacm, "I after cycle", I)

      call write_trace_bool1(file_trace_cacm, &
      "REGIONAL_PA_MAINT_SCHEDULING", REGIONAL_PA_MAINT_SCHEDULING)
                
               IF(REGIONAL_PA_MAINT_SCHEDULING) THEN
                  TG = TRANSACTION_GROUP_ID(I)
                  TG = GET_TRANS_GROUP_POSITION(TG)
                  IF(TG_2_PLANNING_AREA(TG) /= pa_ord) CYCLE
               ELSEIF(REGIONAL_TG_MAINT_SCHEDULING) THEN
                  TG = TRANSACTION_GROUP_ID(I)
                  TG = GET_TRANS_GROUP_POSITION(TG)
                  IF(TG /= pa_ord) CYCLE
               ENDIF
               
       call write_trace_int2(file_trace_cacm, "TG=", TG)

               DATE1 = BASE_DATE + 1
               DATE2 = BASE_DATE + 12
               
            call write_trace_int2(file_trace_cacm, "I after tg set", I)
               
            call write_trace_int2(file_trace_cacm,"DATE1=", DATE1)
            call write_trace_int2(file_trace_cacm, "DATE2=", DATE2)
               
            call write_trace_int2(file_trace_cacm,"ONLINE(I)=", ONLINE(I))
            call write_trace_int2(file_trace_cacm,"OFLINE(I)=", OFLINE(I))
               
               IF(ONLINE(I) > DATE2.OR. OFLINE(I) < DATE1) then
               
       call write_trace_message(file_trace_cacm, &
       "cycling after DATES check.")
       
                CYCLE
               end if
               
          call write_trace_real4(file_trace_cacm,"MAINTENANCE_DAYS(I)=", &
                Mtnc_DAYS(I))
                
                
               IF(Mtnc_DAYS(I) /= 0.) THEN
                  IF(Mtnc_DAYS(I) < 0.) THEN
                   MAINTENANCE_DAYS(I) = GET_VAR(Mtnc_DAYS(I),globecom_year, &
                                                             UNITNM(I))
                  ELSE
                     MAINTENANCE_DAYS(I) = Mtnc_DAYS(I)
                     
       call write_trace_real4(file_trace_cacm, &
       "MAINTENANCE_DAYS(I) set to", MAINTENANCE_DAYS(I))
             
                  ENDIF
               ENDIF


      call write_trace_string(file_trace_cacm,"MARKET_RESOURCE(I)=", &
        MkT_RESOURCE(I))
                    
               IF(MkT_RESOURCE(I) == 'T' .OR. &
                          MkT_RESOURCE(I) == 'G' .OR. &
                                        MkT_RESOURCE(I) == 'D') THEN
                  MARKET_COUNT = MARKET_COUNT + 1
                  MARKET_RESOURCE_COUNTER(I) = MARKET_COUNT
                  MARKET_RESOURCE_INDEX(MARKET_COUNT) = I
               ENDIF
           
           call write_trace_int2(file_trace_cacm, "MARKET_COUNT=", &
            MARKET_COUNT)
            call write_trace_int2(file_trace_cacm, &
                "MARKET_RESOURCE_INDEX(MARKET_COUNT)=", &
                MARKET_RESOURCE_INDEX(MARKET_COUNT))

               
          call write_trace_real4(file_trace_cacm,"MAINTENANCE_DAYS(I)=", &
                MAINTENANCE_DAYS(I))
                
               IF(MAINTENANCE_DAYS(I) > 52.14) THEN
                  WRITE(4,*) "AUTOMATIC MAINTENANCE FOR CL UNIT ", &
                                                              UNITNM(I)
                  WRITE(4,*) "IN YEAR ",globecom_year+get_BASE_YEAR(), &
                                                    " EXCEEDS 52 WEEKS"
                  WRITE(4,*) "MAINTENANCE RESET TO 52 WEEKS"
                  WRITE(4,*) " "
                  MAINTENANCE_DAYS(I) = 52.14
               ENDIF
               
               if(file_trace_cacm/=BAD_TRACE_HANDLE) then
       write(file_trace_cacm,*) "MAINTENANCE_DAYS(I)=", MAINTENANCE_DAYS(I)
               end if
               
                
               IF(MAINTENANCE_DAYS(I) < 0.) THEN
                  WRITE(4,*) "AUTOMAT & !  MAINTENANCE FOR CL UNIT ", &
                                                              UNITNM(I)
            WRITE(4,*) "IN YEAR ",globecom_year+get_BASE_YEAR()," IS LESS THAN 0"
                  WRITE(4,*) "MAINTENANCE RESET TO 0 WEEKS"
                  WRITE(4,*) " "
                  MAINTENANCE_DAYS(I) = 0.
               ENDIF
               
       call write_trace_real4(file_trace_cacm, "2. MAINTENANCE_DAYS(I)=", &
                MAINTENANCE_DAYS(I))
                
               DO J = 1 , 12 ! CALCULATE CAPACITIES FOR ALL MONTHS
               
            call write_trace_int2(file_trace_cacm, "J=", J)
            call write_trace_int2(file_trace_cacm, "I=", I)
            
            call write_trace_int2(file_trace_cacm, &
            "MONTHLY_CAPACITY_POINTER(I)=",MONTHLY_CAPACITY_POINTER(I))
                    
                  IF(MONTHLY_CAPACITY_POINTER(I) /= 0) THEN
                  
           call write_trace_bool1(file_trace_cacm,"CL_CAP_AREA_LINKED(I)", &
                        CL_CAP_AREA_LINKED(I))
                        
                        
                     IF(CL_CAP_AREA_LINKED(I)) THEN
                        PERIOD_RATE = &
                                 GET_CLASS_PEAK_NET_DSM(J,UNITNM(I), &
                                      ABS(MONTHLY_CAPACITY_POINTER(I)))
                     ELSE
                        get_var_called_from="cl_units_read_ext:6410"
         call write_trace_message(file_trace_cacm, "<<< " // &
            trim(get_var_called_from))
            
                        PERIOD_RATE = GET_VAR(FLOAT( &
                         ABS(MONTHLY_CAPACITY_POINTER(I))),J,UNITNM(I))
                         
       call write_trace_real4(file_trace_cacm, "PERIOD_RATE after getvar", &
       PERIOD_RATE)
       
                     ENDIF
       call write_trace_real4(file_trace_cacm, "PERIOD_RATE=", PERIOD_RATE)
                     DO M = 2 , 1, -1

          call write_trace_int2(file_trace_cacm, "YEAR=", globecom_year)
          call write_trace_real4(file_trace_cacm, "INPUT_MW(M,I)=", &
        INPUT_MW(M,I))
        
                        IF(INPUT_MW(M,I) < 0.) THEN
                           MW_CAPACITY = GET_VAR(INPUT_MW(M,I),globecom_year, &
                                                             UNITNM(I))
                           IF(MW_CAPACITY < 0.) THEN
                              MW_CAPACITY = GET_VAR(MW_CAPACITY,J, &
                                                             UNITNM(I))
                           ENDIF
                        ELSE
                           MW_CAPACITY = INPUT_MW(M,I)
                        ENDIF
                        
        call write_trace_real4(file_trace_cacm,"MW_CAPACITY=", MW_CAPACITY)
         
      call write_trace_real4(file_trace_cacm, "PERIOD_RATE", PERIOD_RATE)
      call write_trace_real4(file_trace_cacm, "CAP_FRAC_OWN(I)=", &
        foshyd%CAP_FRAC_OWN(I))
        
                        IF(PERIOD_RATE > 2.) THEN
                           IF(MW_CAPACITY > 0.) THEN
                              PERIOD_RATE = PERIOD_RATE/MW_CAPACITY
                           ELSE
                              MW_CAPACITY = PERIOD_RATE
                              PERIOD_RATE = 1.
                           ENDIF
                        ENDIF
                        IF(MW_CAPACITY < 1. .AND. M == 1) THEN
                           ANNUAL_CL_CAPACITY(M,I,J) = &
                                          MW_CAPACITY * UNIT_CAPACITY
                        ELSE
                           ANNUAL_CL_CAPACITY(M,I,J) =  MW_CAPACITY * &
                            foshyd%CAP_FRAC_OWN(I)/100.* PERIOD_RATE
                        ENDIF
         call write_trace_int2(file_trace_cacm, "M", M)
                        IF(M == 2) then
         call write_trace_message(file_trace_cacm, "M is 2")
                            UNIT_CAPACITY=ANNUAL_CL_CAPACITY(M,I,J)
                            
       call write_trace_real4(file_trace_cacm, "UNIT_CAPACITY became", &
        unit_capacity)
        
                        end if
                     ENDDO
                  ELSE
                     DO M = 2, 1 , -1
                        IF(INPUT_MW(M,I) < 0.) THEN
                        
                        get_var_called_from="cl_units_read_ext:6446"
       if(file_trace_cacm/=BAD_TRACE_HANDLE) then
              write(file_trace_cacm,*) "<<< " // trim(get_var_called_from)
       end if
       
                           ANNUAL_CL_CAPACITY(M,I,J) = &
                   GET_VAR(INPUT_MW(M,I),globecom_year,UNITNM(I))
                   
      if(file_trace_cacm/=BAD_TRACE_HANDLE) then
         write(file_trace_cacm,*) "1. ANNUAL_CL_CAPACITY(M,I,J) set to ", &
                ANNUAL_CL_CAPACITY(M,I,J), "M=", M, "I=", I, "J=", J
       end if
                
                           IF(ANNUAL_CL_CAPACITY(M,I,J) < 0.) THEN
                           

                get_var_called_from="cl_units_read_ext:6449"
        call write_trace_message(file_trace_cacm, "<<< " // &
            trim(get_var_called_from))

            
                              ANNUAL_CL_CAPACITY(M,I,J) = &
                                 GET_VAR(ANNUAL_CL_CAPACITY(M,I,J),J, &
                                                             UNITNM(I))
        
        if(file_trace_cacm/=BAD_TRACE_HANDLE) then                                             
        write(file_trace_cacm,*) "2. ANNUAL_CL_CAPACITY(M,I,J) set to ", &
                 ANNUAL_CL_CAPACITY(M,I,J), "M=", M, "I=", I, "J=", J
        end if
        

                           ENDIF
                        ELSE
                           ANNUAL_CL_CAPACITY(M,I,J) = INPUT_MW(M,I)
                           
       if(file_trace_cacm/=BAD_TRACE_HANDLE) then
        write(file_trace_cacm,*) "3. ANNUAL_CL_CAPACITY(M,I,J) set to ", &
                ANNUAL_CL_CAPACITY(M,I,J), "M=", M, "I=", I, "J=", J
       end if
       
                        ENDIF
                        IF(ANNUAL_CL_CAPACITY(M,I,J) < 1. .AND. &
                                                             M==1) THEN
                           ANNUAL_CL_CAPACITY(1,I,J) = &
                             ANNUAL_CL_CAPACITY(2,I,J) * &
                                              ANNUAL_CL_CAPACITY(1,I,J)
                                              
       if(file_trace_cacm/=BAD_TRACE_HANDLE) then
        write(file_trace_cacm,*) "4. ANNUAL_CL_CAPACITY(M,I,J) set to ", &
                ANNUAL_CL_CAPACITY(M,I,J), "M=", M, "I=", I, "J=", J
       end if
       
                        ELSE
                           ANNUAL_CL_CAPACITY(M,I,J) = &
                                    ANNUAL_CL_CAPACITY(M,I,J)* &
                                             foshyd%CAP_FRAC_OWN(I)/100.
                                             
       if(file_trace_cacm/=BAD_TRACE_HANDLE) then
        write(file_trace_cacm,*) "4. ANNUAL_CL_CAPACITY(M,I,J) set to ", &
                ANNUAL_CL_CAPACITY(M,I,J), "M=", M, "I=", I, "J=", J
       end if
       
                        ENDIF
                     ENDDO
                     UNIT_CAPACITY = ANNUAL_CL_CAPACITY(2,I,J)
                  ENDIF
                  DATE1 = BASE_DATE + J
                  
           if(file_trace_cacm/=BAD_TRACE_HANDLE) then
           write(file_trace_cacm,*) "DATE1 set to ", DATE1, &
            "RETROFIT_ACTIVE(I)=", RETROFIT_ACTIVE(I)
          end if
           
                  IF(RETROFIT_ACTIVE(I)) THEN
                     TEMP_I2 = RETROFIT_PROJ_ID(I)
                     CALL RETROFIT_MINCAP_IMPACT( &
                                              TEMP_I2, &
                                              TEMP_R4)
                     ANNUAL_CL_CAPACITY(1,I,J) = &
                                    ANNUAL_CL_CAPACITY(1,I,J) * TEMP_R4
                                    
                                    
        if(file_trace_cacm/=BAD_TRACE_HANDLE) then
        write(file_trace_cacm,*) "6. ANNUAL_CL_CAPACITY(M,I,J) set to ", &
                ANNUAL_CL_CAPACITY(M,I,J), "M=", M, "I=", I, "J=", J
                
        end if

                     CALL RETROFIT_MAXCAP_IMPACT( &
                                              TEMP_I2, &
                                              TEMP_R4)
                     ANNUAL_CL_CAPACITY(2,I,J) = &
                                    ANNUAL_CL_CAPACITY(2,I,J) * TEMP_R4
                                    
       if(file_trace_cacm/=BAD_TRACE_HANDLE) then
           write(file_trace_cacm,*) "ANNUAL_CL_CAPACITY(2,I,J) set to", &
            ANNUAL_CL_CAPACITY(2,I,J)
       end if
       
        
                     UNIT_CAPACITY = ANNUAL_CL_CAPACITY(2,I,J)
                     
       call write_trace_real4(file_trace_cacm, "UNIT_CAPACITY set to ", &
        UNIT_CAPACITY)
        
        

                  ELSEIF(DATE1 >= CO2_CONTROL_DATE(I) .AND. &
                             CO2_CONTROL_DATE(I) > BASE_YEAR_DATE) THEN
                     TEMP_R4 = CO2_RTRO_CAP_MULT(I)
                     ANNUAL_CL_CAPACITY(1,I,J) = &
                                    ANNUAL_CL_CAPACITY(1,I,J) * TEMP_R4
                                    
                                    
       call write_trace_real4(file_trace_cacm, &
        "ANNUAL_CL_CAPACITY(1,I,J) set to", ANNUAL_CL_CAPACITY(1,I,J))
       
                     ANNUAL_CL_CAPACITY(2,I,J) = &
                                    ANNUAL_CL_CAPACITY(2,I,J) * TEMP_R4
                                    
       call write_trace_real4(file_trace_cacm, &
       "ANNUAL_CL_CAPACITY(2,I,J) set to ",ANNUAL_CL_CAPACITY(2,I,J))
        
                     UNIT_CAPACITY = ANNUAL_CL_CAPACITY(2,I,J)
                     
       call write_trace_real4(file_trace_cacm,"2. UNIT_CAPACITY set to", &
        UNIT_CAPACITY)
        
                  ENDIF
                IF(ONLINE(I) <= DATE1 .AND. OFLINE(I) >= DATE1 .AND. &
                       (UNIT_CAPACITY > 0. .OR. LDTYPE(I) == 'L')) THEN
                     IF(MNRATE(I,J) < 0.) THEN
                        IF(OFFSET_MAINTENANCE_ACTIVE .AND. &
                                                I > BASE_CL_UNITS) THEN
      
                           GET_YR = &
                          globecom_year-(ONLINE(I)/100+1900 - get_BASE_YEAR())+1
                     
       call write_trace_real4(file_trace_cacm,"2. UNIT_CAPACITY set to", &
        UNIT_CAPACITY)
 
                        ELSE
                           GET_YR = globecom_year
                           
       call write_trace_int2(file_trace_cacm, "2. GET_YR set to", GET_YR)
       
                        ENDIF
                        
                        get_var_called_from="cl_units_read_ext:6503"
       call write_trace_message(file_trace_cacm, "<<< " // &
        trim(get_var_called_from))
        
                        MN_RATE = GET_VAR(MNRATE(I,J),GET_YR,UNITNM(I))
                        
       call write_trace_real4(file_trace_cacm, "MN_RATE set to", MN_RATE)
       
                        TEMP_CL_MAINTENANCE = MIN(100.,MN_RATE)/100.
                        
       call write_trace_real4(file_trace_cacm, &
        "1. TEMP_CL_MAINTENANCE set to ", TEMP_CL_MAINTENANCE)
                
                     ELSE
                       TEMP_CL_MAINTENANCE = MIN(100.,MNRATE(I,J))/100.
       call write_trace_real4(file_trace_cacm, &
       "2. TEMP_CL_MAINTENANCE set to ", TEMP_CL_MAINTENANCE)
       
                     ENDIF
                  ELSE
                     TEMP_CL_MAINTENANCE = 1.0
                     MAINTENANCE_DAYS(I) = &
                           MIN(52.14,MAINTENANCE_DAYS(I) + &
                            TEMP_CL_MAINTENANCE * DAYS_PER_MONTH(J)/7.)
                  ENDIF
                  
       call write_trace_bool1(file_trace_cacm, &
       "MAINTENANCE_IS_ACCUMULATED= ", MAINTENANCE_IS_ACCUMULATED)
                
                  IF(MAINTENANCE_IS_ACCUMULATED .AND. &
                   ONLINE(I) <= DATE1 .AND. OFLINE(I) >= DATE1 .AND. &
                     .NOT. (LDTYPE(I)=='N' .OR. &
                                      EXPENSE_ASSIGNMENT(I)=='P')) THEN
                     MAINTENANCE_DAYS(I) = MAINTENANCE_DAYS(I) + &
                             TEMP_CL_MAINTENANCE * DAYS_PER_MONTH(J)/7.
                  ELSE
                     ANNUAL_CL_MAINTENANCE(I,J) = TEMP_CL_MAINTENANCE
                  ENDIF
                  ANNUAL_CL_MAINTENANCE(I,13) = &
                     ANNUAL_CL_MAINTENANCE(I,13) + &
                         ANNUAL_CL_MAINTENANCE(I,J) * DAYS_PER_MONTH(J)
                  ANNUAL_CL_MAINT_MW(I) = MAX(ANNUAL_CL_MAINT_MW(I), &
                                             ANNUAL_CL_CAPACITY(2,I,J))

               ENDDO ! SEASONS
               
               call write_trace_message(file_trace_cacm,"ENDDO ! SEASONS")

               K = K + 1
               MAINT_INDEX(K) = I
               call write_trace_int2(file_trace_cacm, "K", K)
               call write_trace_int2(file_trace_cacm, "I [mi(k)]", I)
               MAINT_INDEX_DAYS(K) = MAINTENANCE_DAYS(I)
               MAINT_INDEX_MW(K) = ANNUAL_CL_MAINT_MW(I)
               MAINT_INDEX_MONTHLY_MW(K,1) = ANNUAL_CL_MAINTENANCE(I,1)
               MAINT_INDEX_MONTHLY_MW(K,2) = ANNUAL_CL_MAINTENANCE(I,2)
               MAINT_INDEX_MONTHLY_MW(K,3) = ANNUAL_CL_MAINTENANCE(I,3)
               MAINT_INDEX_MONTHLY_MW(K,4) = ANNUAL_CL_MAINTENANCE(I,4)
               MAINT_INDEX_MONTHLY_MW(K,5) = ANNUAL_CL_MAINTENANCE(I,5)
               MAINT_INDEX_MONTHLY_MW(K,6) = ANNUAL_CL_MAINTENANCE(I,6)
               MAINT_INDEX_MONTHLY_MW(K,7) = ANNUAL_CL_MAINTENANCE(I,7)
               MAINT_INDEX_MONTHLY_MW(K,8) = ANNUAL_CL_MAINTENANCE(I,8)
               MAINT_INDEX_MONTHLY_MW(K,9) = ANNUAL_CL_MAINTENANCE(I,9)
               MAINT_INDEX_MONTHLY_MW(K,10) = &
                                            ANNUAL_CL_MAINTENANCE(I,10)
               MAINT_INDEX_MONTHLY_MW(K,11) = &
                                            ANNUAL_CL_MAINTENANCE(I,11)
               MAINT_INDEX_MONTHLY_MW(K,12) = &
                                            ANNUAL_CL_MAINTENANCE(I,12)
               MAINT_INDEX_MONTHLY_MW(K,13) = &
                                            ANNUAL_CL_MAINTENANCE(I,13)

            ENDDO ! get_nunits()

!  SIMPLIFICATIONS FOR MAINTENANCE SCHEDULER:

!   (1) WABASH_VALLEY GIBSON CAPACITY IS TAKEN OUT
!   (2) DAILY SCHEDULING ONLY
!   (3) NO SPLITTING THE MAINTENANCE
!   (4) THE ALGORITHM IS LOOKING FOR SCHEDULED WEEKS, NOT DAYS
!   (5) MAINT SCHEDULED BEYOND DECEMBER 31 IT MOVES TO JAN 1 SAME YEAR
            !  TEST. 8/15/02. GAT.
            ! TODO: MAINTENANCE_IS_ACCUMULATED is never true! fix or remove.
            IF(SAVE_DETAILED_MAINTENANCE .OR. &
              YES_UNIT_COMMITMENT_LOGIC() .OR.   &
              MAINTENANCE_IS_ACCUMULATED) THEN


               LAST_ACTIVE_UNIT = K

               SCHEDULE_FO = .TRUE.


               DETAILED_MAINT = YES_DETAILED_MAINTENANCE()


               CALL SW_MAINT_DERATING(SCHEDULE_FO)

               SCHEDULE_FO = YES_DETAILED_FOR()

               VOID_LOGICAL = PICK_FOR_SEED_OPTIONS(FOR_SEED_OPTIONS)
    !
               IF(FOR_SEED_OPTIONS == 'Y') THEN
                  YEAR_OR_SCEN = R_YEAR
               ELSE
                  YEAR_OR_SCEN = gc_end_point
               ENDIF

               IF(LAST_ACTIVE_UNIT == 0) CYCLE

               CALL MAINT_SCHEDULER( &
                                 get_nunits(), &
                                 LAST_ACTIVE_UNIT, &
                                 MAINT_DIVISIBLE, &
                                 'D', &
                                 MAINT_INDEX_MW, &
                                 MAINT_INDEX_DAYS, &
                                 MAINT_INDEX_MONTHLY_MW, &
                                 MAINT_INDEX, &
                                 SCHEDULE_FO, &
                                 DETAILED_MAINT, &
                                 MAX_FO_PER_MONTH, &
                         MONTHLY_MAINTENANCE_PEAKS) ! ADDED 9/27/02.

! TODO: -- explore what this comment says.
! MAINT_INDEX above is needed even if active items in arrays
! are compressed; get_nunits() above needed only for properly dimensioning
! MAINT_INDEX_MONTHLY_MW; if subscript order on MAINT_INDEX_MONTHLY_MW
! were reversed, we could avoid passing get_nunits(), making 
! MAINT_SCHEDULER simpler and more elegant.



! 11/17/98. GAT. FOR SCENARIO MAKER.

               DO J = 1, 12
                  MONTHLY_NUCLEAR_AVAIL_MULT(J) = &
                                   GET_SCENARIO_NUCLEAR_AVAIL(R_YEAR,J)
                  MONTHLY_COAL_AVAIL_MULT(J) = &
                                      GET_SCENARIO_COAL_AVAIL(R_YEAR,J)
                  MONTHLY_GAS_AVAIL_MULT(J) = &
                                       GET_SCENARIO_GAS_AVAIL(R_YEAR,J)
                  MONTHLY_OIL_AVAIL_MULT(J) = &
                                       GET_SCENARIO_OIL_AVAIL(R_YEAR,J)
                  MONTHLY_OTHER_AVAIL_MULT(J) = &
                                     GET_SCENARIO_OTHER_AVAIL(R_YEAR,J)
                  MONTHLY_HYDRO_WATER_YEAR_MULT(J) = &
                               GET_SCENARIO_HYDRO_WATER_YEAR(R_YEAR,J)
               ENDDO

               
               REGIONAL_MONTHLY_MULT = 1.

               DO K = 1, LAST_ACTIVE_UNIT
                  I = MAINT_INDEX(K)
                  ANNUAL_CL_MAINTENANCE(I,1) = &
                                            MAINT_INDEX_MONTHLY_MW(K,1)
                  ANNUAL_CL_MAINTENANCE(I,2) = &
                                            MAINT_INDEX_MONTHLY_MW(K,2)
                  ANNUAL_CL_MAINTENANCE(I,3) = &
                                            MAINT_INDEX_MONTHLY_MW(K,3)
                  ANNUAL_CL_MAINTENANCE(I,4) = &
                                            MAINT_INDEX_MONTHLY_MW(K,4)
                  ANNUAL_CL_MAINTENANCE(I,5) = &
                                            MAINT_INDEX_MONTHLY_MW(K,5)
                  ANNUAL_CL_MAINTENANCE(I,6) = &
                                            MAINT_INDEX_MONTHLY_MW(K,6)
                  ANNUAL_CL_MAINTENANCE(I,7) = &
                                            MAINT_INDEX_MONTHLY_MW(K,7)
                  ANNUAL_CL_MAINTENANCE(I,8) = &
                                            MAINT_INDEX_MONTHLY_MW(K,8)
                  ANNUAL_CL_MAINTENANCE(I,9) = &
                                            MAINT_INDEX_MONTHLY_MW(K,9)
                  ANNUAL_CL_MAINTENANCE(I,10) = &
                                           MAINT_INDEX_MONTHLY_MW(K,10)
                  ANNUAL_CL_MAINTENANCE(I,11) = &
                                           MAINT_INDEX_MONTHLY_MW(K,11)
                  ANNUAL_CL_MAINTENANCE(I,12) = &
                                           MAINT_INDEX_MONTHLY_MW(K,12)
                  ANNUAL_CL_MAINTENANCE(I,13) = &
                                           MAINT_INDEX_MONTHLY_MW(K,13)


                  FT = PRIMARY_MOVER(I)

                  IF(YES_REGIONAL_OUTAGES_ACTIVE) THEN

                    TG = TRANSACTION_GROUP_ID(I)
                     DO J = 1, 12
                        REGIONAL_MONTHLY_MULT(J) = &
                                  GET_MONTHLY_REGIONAL_OUTAGE(R_YEAR, &
                                                               J,TG,FT)
                     ENDDO
                  ENDIF


                  IF(EFOR(I) < 0.) THEN
                     TEMP_R4 = GET_VAR(EFOR(I),R_YEAR,UNITNM(I))
                     DO J = 1, 12
                        IF(TEMP_R4 < 0.) THEN
                           MONTHLY_EAVAIL(J) = &
                             (100. - GET_VAR(TEMP_R4,J,UNITNM(I)))/100.
                        ELSE
                           MONTHLY_EAVAIL(J) = (100. - TEMP_R4)/100.
                        ENDIF

                        IF(FT == 1) THEN
                           MONTHLY_EAVAIL(J) = &
                              MIN(MONTHLY_EAVAIL(J) * &
                                 REGIONAL_MONTHLY_MULT(J) * &
                                        MONTHLY_COAL_AVAIL_MULT(J),1.0)
                        ELSEIF(FT ==2) THEN
                           MONTHLY_EAVAIL(J) = &
                              MIN(MONTHLY_EAVAIL(J) * &
                                 REGIONAL_MONTHLY_MULT(J) * &
                                         MONTHLY_GAS_AVAIL_MULT(J),1.0)
                        ELSEIF(FT == 3) THEN
                           MONTHLY_EAVAIL(J) = &
                              MIN(MONTHLY_EAVAIL(J) * &
                                 REGIONAL_MONTHLY_MULT(J) * &
                                         MONTHLY_OIL_AVAIL_MULT(J),1.0)
                        ELSEIF(FT == 4) THEN
                           MONTHLY_EAVAIL(J) = &
                              MIN(MONTHLY_EAVAIL(J) * &
                                 REGIONAL_MONTHLY_MULT(J) * &
                                     MONTHLY_NUCLEAR_AVAIL_MULT(J),1.0)
                        ELSEIF(FT == 5) THEN
                           MONTHLY_EAVAIL(J) = &
                               MONTHLY_EAVAIL(J) * &
                                 REGIONAL_MONTHLY_MULT(J) * &
                                       MONTHLY_HYDRO_WATER_YEAR_MULT(J)
                        ELSEIF(FT == 6) THEN
                           MONTHLY_EAVAIL(J) = &
                              MIN(MONTHLY_EAVAIL(J) * &
                                 REGIONAL_MONTHLY_MULT(J) * &
                                       MONTHLY_OTHER_AVAIL_MULT(J),1.0)
                        ENDIF
                     ENDDO
                  ELSE
                     DO J = 1, 12

                        MONTHLY_EAVAIL(J) = (100. - EFOR(I))/100.

                        IF(FT == 1) THEN
                           MONTHLY_EAVAIL(J) = &
                              MIN(MONTHLY_EAVAIL(J) * &
                                 REGIONAL_MONTHLY_MULT(J) * &
                                        MONTHLY_COAL_AVAIL_MULT(J),1.0)
                        ELSEIF(FT ==2) THEN
                           MONTHLY_EAVAIL(J) = &
                              MIN(MONTHLY_EAVAIL(J) * &
                                 REGIONAL_MONTHLY_MULT(J) * &
                                         MONTHLY_GAS_AVAIL_MULT(J),1.0)
                        ELSEIF(FT == 3) THEN
                           MONTHLY_EAVAIL(J) = &
                              MIN(MONTHLY_EAVAIL(J) * &
                                 REGIONAL_MONTHLY_MULT(J) * &
                                         MONTHLY_OIL_AVAIL_MULT(J),1.0)
                        ELSEIF(FT == 4) THEN
                           MONTHLY_EAVAIL(J) = &
                              MIN(MONTHLY_EAVAIL(J) * &
                                 REGIONAL_MONTHLY_MULT(J) * &
                                     MONTHLY_NUCLEAR_AVAIL_MULT(J),1.0)
                        ELSEIF(FT == 5) THEN
                           MONTHLY_EAVAIL(J) = &
                               MONTHLY_EAVAIL(J) * &
                                 REGIONAL_MONTHLY_MULT(J) * &
                                       MONTHLY_HYDRO_WATER_YEAR_MULT(J)
                        ELSEIF(FT == 6) THEN
                           MONTHLY_EAVAIL(J) = &
                              MIN(MONTHLY_EAVAIL(J) * &
                                 REGIONAL_MONTHLY_MULT(J) * &
                                       MONTHLY_OTHER_AVAIL_MULT(J),1.0)
                        ENDIF
                     ENDDO

                  ENDIF


!                  CALL GET_FO_HRS1(I,MONTHLY_EAVAIL(INT2(1)),

! 11/10/03. DETAILED CONDITIONS.

! 02/10/04. TOOK OUT DETAILED_MAIN CONDITION

!                  IF(DETAILED_MAINT .AND.
! 070806. ADDED EXTRA CONDITION

                  IF( (FREQUENCY_DURATION .AND. &
                                    monte_or_frq(I) == 'G').OR. &
                      (SCHEDULE_FO .AND.monte_or_frq(I) == 'F') ) THEN
                     CALL GET_FO_HRS2( &
                               I, &
                               K, &
                               FOR_FREQUENCies(I), &
                               FOR_DURAtn(I), &
                               MONTHLY_EAVAIL(INT2(1)))

       ! DO I REALLY NEED TO DO DETAILED REGARDLESS OF DETAILED SWITCH?
                  ELSEIF(SCHEDULE_FO) THEN

                     CALL GET_FO_HRS1( &
                               I, &
                               K, &
                               MONTHLY_EAVAIL, &
                               YEAR_OR_SCEN)! 11/24/03. K INDEX
                  ENDIF

               ENDDO ! K COUNTER FOR ACTIVE get_nunits()
               IF(SHADOW_UNITS_ACTIVE) THEN
                  DO I = 1, get_nunits()
                     IF(LDTYPE(I) == 'S') THEN
                        MY_SHADOW = SHADOW_UNIT_NUMBER(I)
                        ANNUAL_CL_MAINT_MW(I) = &
                                          ANNUAL_CL_MAINT_MW(MY_SHADOW)
               DO MO = 1, 12  ! 7/31/96. GAT. ADDED ANNUAL_CL_CAPACITY.

                           ANNUAL_CL_CAPACITY(1,I,MO) = &
                                     ANNUAL_CL_CAPACITY(1,MY_SHADOW,MO)
                           ANNUAL_CL_CAPACITY(2,I,MO) = &
                                     ANNUAL_CL_CAPACITY(2,MY_SHADOW,MO)

                           ANNUAL_CL_MAINTENANCE(I,MO) = &
                                    ANNUAL_CL_MAINTENANCE(MY_SHADOW,MO)
                        ENDDO
                        ANNUAL_CL_MAINTENANCE(I,13) = &
                                    ANNUAL_CL_MAINTENANCE(MY_SHADOW,13)
                     ENDIF
                  ENDDO
               ENDIF ! SHADOW get_nunits()
            ENDIF ! ENDIF FOR MAINT_SCHEDULER
            
            call write_trace_bool1(file_trace_cacm,"MAINTENANCE_REPORT", &
                MAINTENANCE_REPORT())
                
            IF(MAINTENANCE_REPORT() .AND. .NOT. TESTING_PLAN) THEN
            
        call write_trace_int2s(file_trace_cacm, & 
         "Calling WRITE_YEARS_CMD_FILE for", MAINT_INDEX)
                
               CALL WRITE_YEARS_CMD_FILE(MAINT_INDEX)
            ENDIF
         ENDDO ! PA PLANNING AREA LOOP
         
         ! TODO: Remove code in IF block
         IF(1 == 2) THEN

            IF(MAINTENANCE_REPORT_NOT_OPEN) THEN
               MAINTENANCE_REPORT_NOT_OPEN = .FALSE.
               CL_MAINT_NO=MAINTENANCE_HEADER(MW_MAINT_NO, &
                                              CL_MAINT_REC, &
                                              MW_MAINT_REC)
            ENDIF
            GROUP_MW_MAINTENANCE = 0.
            TOTAL_MONTHLY_MAINTENANCE = 0.
            DO I = 1, get_nunits()

! CALCULATE MONTHLY MAINTENANCE CAPACITY BY REPORTING GROUP

               IF(ONLINE(I) <= DATE1 .AND. OFLINE(I) >= DATE1) THEN
                  GGI = GENGRP(I)
                  IF(GGI < 1 .OR. GGI > MAX_REPORTING_GROUPS) GGI = 0
                  DO MO = 1, 12
                  MAINTENANCE_CAPACITY = ANNUAL_CL_MAINTENANCE(I,MO)* &
                                             ANNUAL_CL_CAPACITY(2,I,MO)
                  GROUP_MW_MAINTENANCE(MO,GGI)=MAINTENANCE_CAPACITY + &
                                           GROUP_MW_MAINTENANCE(MO,GGI)
                  TOTAL_MONTHLY_MAINTENANCE(MO)=MAINTENANCE_CAPACITY+ &
                                          TOTAL_MONTHLY_MAINTENANCE(MO)
                  GROUP_MW_MAINTENANCE(0,GGI)=MAINTENANCE_CAPACITY * &
                                                 DAYS_PER_MONTH(MO) + &
                                            GROUP_MW_MAINTENANCE(0,GGI)
                  TOTAL_MONTHLY_MAINTENANCE(0)=MAINTENANCE_CAPACITY * &
                                                 DAYS_PER_MONTH(MO) + &
                                           TOTAL_MONTHLY_MAINTENANCE(0)
                  ENDDO
               ENDIF

               IF(EFOR(I) < 0.) THEN
             FOR_MT = (100. - GET_VAR(EFOR(I),globecom_year,UNITNM(I)))/100.
               ELSE
                  FOR_MT = (100. - EFOR(I))/100.
               ENDIF
               
               if(file_trace_clm/=BAD_TRACE_HANDLE) then
                    write(file_trace_clm,*) &
                    "REC ", CL_MAINT_REC, &
                    "PRT_ENDPOINT ", PRT_ENDPOINT(), &
                 "YEAR ", real(real(globecom_year)+get_BASE_YEAR()), &
                 "UNITNM ", UNITNM(I), &
           "ANNUAL_CL_MAINT ", (ANNUAL_CL_MAINTENANCE(I,J),J=1,12), &
           "D13 ", DAYS_PER_MONTH(13), &
           "DJ ", DAYS_PER_MONTH(J), &
           "ACM13 ", ANNUAL_CL_MAINTENANCE(I,13), &
           "FOR_MT ", FOR_MT

               end if
               
               WRITE(CL_MAINT_NO,REC=CL_MAINT_REC) &
                    PRT_ENDPOINT(), &
                    real(real(globecom_year)+get_BASE_YEAR()), &
                    UNITNM(I), &
                    (ANNUAL_CL_MAINTENANCE(I,J)*100.,J=1,12), &
                 100.*ANNUAL_CL_MAINTENANCE(I,13)/DAYS_PER_MONTH(13), &
                    (DAYS_PER_MONTH(J) * &
                                ANNUAL_CL_MAINTENANCE(I,J),J=1,12), &
                    ANNUAL_CL_MAINTENANCE(I,13), &
                    (100.*(1. - ANNUAL_CL_MAINTENANCE(I,J))* &
                                                      FOR_MT,J=1,12), &
                  (1.-ANNUAL_CL_MAINTENANCE(I,13)/DAYS_PER_MONTH(13)) &
                                                          *100.* FOR_MT
               CL_MAINT_REC = CL_MAINT_REC + 1
            ENDDO
            DO GGI = 0,MAX_REPORTING_GROUPS
             GROUP_MW_MAINTENANCE(0,GGI)=GROUP_MW_MAINTENANCE(0,GGI)/ &
                                                     DAYS_PER_MONTH(13)
               WRITE(MW_MAINT_NO,REC=MW_MAINT_REC) &
                    PRT_ENDPOINT(), &
                    FLOAT(globecom_year+get_BASE_YEAR()), &
                    GROUP_NAME(GGI), &
                    (GROUP_MW_MAINTENANCE(MO,GGI),MO=0,12)
               MW_MAINT_REC = MW_MAINT_REC + 1
            ENDDO
          TOTAL_MONTHLY_MAINTENANCE(0)= TOTAL_MONTHLY_MAINTENANCE(0)/ &
                                                     DAYS_PER_MONTH(13)
            WRITE(MW_MAINT_NO,REC=MW_MAINT_REC) &
                   PRT_ENDPOINT(), &
                    FLOAT(globecom_year+get_BASE_YEAR()), &
                    'Totals   ', &
                    (TOTAL_MONTHLY_MAINTENANCE(MO),MO=0,12)
            MW_MAINT_REC = MW_MAINT_REC + 1
         ENDIF

         CALC_ANNUAL_CAP_AND_MAINT = .TRUE.
      call write_trace_message(file_trace_cacm,"Leaving routine.")
      RETURN


      ENTRY GET_MAX_FO_PER_MONTH

         GET_MAX_FO_PER_MONTH = MAX_FO_PER_MONTH
      RETURN
! CALLED ANNUALLY FROM TRANSOBJ.FOR

      ENTRY PROCESS_MARKET_RESOURCES(R_TG,R_AREA_PRICE)


! 09/23/03. CHANGED TO CYCLE OVER MULTIPLE TRANSACTION GROUPS (R_TG).
! ONLY ACTIVE IN TRANSACT C.

         PROCESS_MARKET_RESOURCES = .FALSE.
         DO TG = 1, R_TG
            IF(YES_RUN_TRANSACT() .AND. (MARKET_COUNT > 0 .OR. &
                             GET_NUM_ANNUAL_DERIVATIVES(TG) > 0)) THEN

               IF(R_AREA_PRICE) THEN
                  FILE_NAME = trim(PRB_FILE_DIRECTORY())//"PRB"// &
                             trim(GET_HOURLY_PRICE_NAME(TG))// &
                              LOAD_FILE_CHAR_EXT(gc_end_point)//".P"// &
                        LOAD_FILE_CHAR_EXT(GET_MARKET_PRICE_YEAR(globecom_year))

! 10/02/03. ADDED FOR FE. GO GET THE FIRST gc_end_point AGAIN.

                  INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
                  IF(.NOT. FILE_EXISTS .AND. gc_end_point > 1) THEN
                     FILE_NAME = trim(PRB_FILE_DIRECTORY())//"PRB"// &
                             trim(GET_HOURLY_PRICE_NAME(TG))// &
                              LOAD_FILE_CHAR_EXT(INT2(1))//".P"// &
                        LOAD_FILE_CHAR_EXT(GET_MARKET_PRICE_YEAR(globecom_year))
                  ENDIF

               ELSE
                  CALL GET_NEW_MARKET_PRICE_NAME(MARKET_PRICE_NAME)
                  FILE_NAME = trim(PRB_FILE_DIRECTORY())//"PRB"// &
                                    trim(MARKET_PRICE_NAME)//".P"// &
                        LOAD_FILE_CHAR_EXT(GET_MARKET_PRICE_YEAR(globecom_year))
               ENDIF

               CALL READ_USER_MARKET_ANNUAL(globecom_year, &
                                    globecom_year+get_BASE_YEAR(), &
                                    TG, & !  TRANS_GROUP
                                    FILE_NAME, & !  PRICE FILE NAME
                                    gc_end_point, &
                                    R_AREA_PRICE) ! ENDPOINT


              TEMP_L = ANNUAL_CALL_PUT_CAPACITY(globecom_year+get_BASE_YEAR(),TG)


               IF(MARKET_COUNT > 0) THEN
                  CALL PickHrStrInitArrays(MARKET_COUNT, &
                                     INT2(2), &
                                     INT2(8800))
                  CALL SortPricesForYear(globecom_year+get_BASE_YEAR())
                  DO I = 1, MARKET_COUNT
                     J = MARKET_RESOURCE_INDEX(I)

                     LOCAL_COEFF(1) = COEFF(1,J)
                     LOCAL_COEFF(2) = COEFF(2,J)
                     LOCAL_COEFF(3) = COEFF(3,J)

                     CALL GET_MONTHLY_DISP_COST_BY_UNIT( &
                                                  J, &
                                                  LOCAL_COEFF, &
                                                 MONTHLY_DISPATCH_COST)

                     DO MO = 1, 12
                        MONTHLY_CAPACITY(1,MO) = &
                                             ANNUAL_CL_CAPACITY(1,J,MO)
                        MONTHLY_CAPACITY(2,MO) = &
                           MAX(0.,ANNUAL_CL_CAPACITY(2,J,MO) - &
                                            ANNUAL_CL_CAPACITY(1,J,MO))
                        IF(ANNUAL_CL_MAINTENANCE(J,MO) == 1.) THEN
                           MONTHLY_INTEGER(MO) = 0
                        ELSE
                           IF(MONTHLY_CAPACITY_POINTER(J) < 0) THEN
!                              MONTHLY_INTEGER(MO) =
                              TEMP_R4 = GET_VAR(FLOAT( &
                                        MONTHLY_CAPACITY_POINTER(J)), &
                                                          MO,UNITNM(J))
                              IF(TEMP_R4 < .001) THEN
                                 MONTHLY_INTEGER(MO) = 0
                              ELSE
                                 MONTHLY_INTEGER(MO) = 1
                              ENDIF
                           ELSE
                              MONTHLY_INTEGER(MO) = 1
                           ENDIF
                        ENDIF
                     ENDDO

! 10/29/03. TESTING.

                     STARTS_ONLY = .TRUE.

                     CALL PickStrikesForUnit( &
                        I, &
                        2_2, &
                        MONTHLY_CAPACITY, &
                        MONTHLY_DISPATCH_COST, &
                        INT2(MIN_CAP_FACTOR(J)), &
                        INT2(MAX_CAP_FACTOR(J)), &
                        MONTHLY_INTEGER, &
                        STARTS_ONLY)
                  ENDDO
               ENDIF
               PROCESS_MARKET_RESOURCES = .TRUE.
            ENDIF
         ENDDO
       RETURN

      ENTRY IS_A_MARKET_RESOURCE(R_UNIT_NO)

       IS_A_MARKET_RESOURCE = MkT_RESOURCE(R_UNIT_NO) == 'T' .OR. &
                                      MkT_RESOURCE(R_UNIT_NO) == 'D'
      RETURN

      ENTRY IS_STRICT_MARKET_RESOURCE(R_UNIT_NO,R_MARKET_RESOURCE_STR)


         R_MARKET_RESOURCE_STR = MkT_RESOURCE(R_UNIT_NO)
         IF(MkT_RESOURCE(R_UNIT_NO) == 'M') THEN
            IS_STRICT_MARKET_RESOURCE = .TRUE.
         ELSE
            IS_STRICT_MARKET_RESOURCE = .FALSE.
         ENDIF
      RETURN

      ENTRY GET_MARKET_RESOURCE_INDEX(R_UNIT_NO)

         GET_MARKET_RESOURCE_INDEX = MARKET_RESOURCE_INDEX(R_UNIT_NO)
      RETURN

      ENTRY GET_MARKET_RESOURCE_COUNTER(R_UNIT_NO)

         IF(ALLOCATED(MARKET_RESOURCE_COUNTER) .AND. &
                                     R_UNIT_NO <= get_nunits()) THEN
            GET_MARKET_RESOURCE_COUNTER = &
                                     MARKET_RESOURCE_COUNTER(R_UNIT_NO)
         ELSE
            GET_MARKET_RESOURCE_COUNTER = 0
         ENDIF
      RETURN

      ENTRY GET_FUEL_DELIVERY(  R_I, &
                                R_FUEL_DELIVERY_1_ord, &
                                R_FUEL_DELIVERY_2_ord, &
                                R_FUEL_DELIVERY_3_ord)

         R_FUEL_DELIVERY_1_ord = ns_p_fuel_annl%P_FUEL_DELIVERY_1(R_I)
         R_FUEL_DELIVERY_2_ord = ns_p_fuel_annl%P_FUEL_DELIVERY_2(R_I)
         R_FUEL_DELIVERY_3_ord = ns_p_fuel_annl%P_FUEL_DELIVERY_3(R_I)
         GET_FUEL_DELIVERY = PBTUCT(R_I)
      RETURN

      ENTRY GET_SECOND_FUEL_DELIVERY( &
                                R_I, &
                                R_FUEL_DELIVERY_1_ord, &
                                R_FUEL_DELIVERY_2_ord, &
                                R_FUEL_DELIVERY_3_ord)

         R_FUEL_DELIVERY_1_ord = S_FUEL_DELIVERY(R_I)
         R_FUEL_DELIVERY_2_ord = S_FUEL_DELIVERY_2(R_I)
         R_FUEL_DELIVERY_3_ord = S_FUEL_DELIVERY_3(R_I)
         GET_SECOND_FUEL_DELIVERY = SBTUCT(R_I)
      RETURN

      ENTRY UNSCHEDULED_MAINT_HOURS(R_UNIT_NO)

         IF(Mtnc_DAYS(R_UNIT_NO) /= 0.) THEN
            IF(Mtnc_DAYS(R_UNIT_NO) < 0.) THEN
             UNSCHEDULED_MAINT_HOURS = GET_VAR(Mtnc_DAYS(R_UNIT_NO), &
                                               globecom_year,UNITNM(R_UNIT_NO))
            ELSE
               UNSCHEDULED_MAINT_HOURS = Mtnc_DAYS(R_UNIT_NO)
            ENDIF
         ENDIF
         UNSCHEDULED_MAINT_HOURS = UNSCHEDULED_MAINT_HOURS*7.*24.
      RETURN

      ENTRY UPDATE_PERIOD_CAPACITY(R_I,R_J)

         UPDATE_PERIOD_CAPACITY = .FALSE.
         IF(.NOT. ALLOCATED(ANNUAL_CL_CAPACITY)) RETURN
         I = R_I
         J = R_J
         IF(I/=get_nunits()) THEN
            UPDATE_PERIOD_CAPACITY = .FALSE.
            RETURN
         ENDIF
         call write_trace_reals2(file_trace_cacm, "MW_pc", MW)
         DO J = 1, get_nunits()
            DO I = 1, 2
              
              MW(I,J) = ANNUAL_CL_CAPACITY(I,J,R_J) !debugstop
              
              call write_trace_int2(file_trace_cacm, "J", J)
              call write_trace_int2(file_trace_cacm, "I", I)
              
              call write_trace_real(file_trace_cacm, "MW(I,J)", MW(I,J))
              
            ENDDO

         ENDDO

                

         UPDATE_PERIOD_CAPACITY = .TRUE.
      RETURN

      ENTRY GET_ANNUAL_CL_CAPACITY(R_I,R_J,R_MONTH)

         IF(ALLOCATED(ANNUAL_CL_CAPACITY) .AND. &
       R_J <= get_nunits()) THEN
           GET_ANNUAL_CL_CAPACITY = ANNUAL_CL_CAPACITY(R_I,R_J,R_MONTH)
         ELSE
            GET_ANNUAL_CL_CAPACITY = 0.
         ENDIF
      RETURN

      ENTRY GET_MAINTENANCE_RATE_FOR_MONTH(R_UNIT_NO,R_MONTH)

         IF(ALLOCATED(ANNUAL_CL_MAINTENANCE)) THEN
            GET_MAINTENANCE_RATE_FOR_MONTH = &
                            ANNUAL_CL_MAINTENANCE(R_UNIT_NO,R_MONTH)
         ELSE
            GET_MAINTENANCE_RATE_FOR_MONTH = 0.
         ENDIF
      RETURN

      ENTRY UPDATE_PERIOD_MAINTENANCE(R_I,R_J,MAINTENANCE_RATE)

         I = R_I
         J = R_J
         IF(I/=get_nunits()) THEN
            UPDATE_PERIOD_MAINTENANCE = .FALSE.
            RETURN
         ENDIF

         DO I = 1, get_nunits()
            MAINTENANCE_RATE(I) = ANNUAL_CL_MAINTENANCE(I,J)
         ENDDO
         UPDATE_PERIOD_MAINTENANCE = .TRUE.
      RETURN

      ENTRY UPDATE_PR_ESCALATIONS(R_YEAR,R_POINTER)



      VOID_INT2 = &
        ESCALATE_ONE_VALUE_FOR_ADDS(PBTUCT(R_POINTER),R_POINTER, &
                           PFESCR(R_POINTER),UNITNM(R_POINTER),R_YEAR)
      VOID_INT2 = &
        ESCALATE_ONE_VALUE_FOR_ADDS(SBTUCT(R_POINTER),R_POINTER, &
                            SFESCR(R_POINTER),UNITNM(R_POINTER),R_YEAR)
      VOID_INT2 = &
         ESCALATE_ONE_VALUE_FOR_ADDS(EMISS_FUEL_COST(R_POINTER), &
                           R_POINTER,EMISS_FUEL_ESCAL(R_POINTER), &
                                              UNITNM(R_POINTER),R_YEAR)
      VOID_INT2 = &
       ESCALATE_ONE_VALUE_FOR_ADDS(VCPMWH_IN(R_POINTER), &
        R_POINTER, &
                            OMESCR(R_POINTER),UNITNM(R_POINTER),R_YEAR)
      VOID_INT2 = &
       ESCALATE_ONE_VALUE_FOR_ADDS(CL_AI_ENERGY_RATE(R_POINTER), &
                     R_POINTER,CL_AI_ENERGY_ESCALATOR(R_POINTER), &
                                             UNITNM(R_POINTER),R_YEAR)
      VOID_INT2 = &
        ESCALATE_ONE_VALUE_FOR_ADDS(CL_AI_CAPACITY_RATE( &
        R_POINTER), &
                  R_POINTER,CL_AI_CAPACITY_ESCALATOR(R_POINTER), &
                                             UNITNM(R_POINTER),R_YEAR)
      VOID_INT2 = &
       ESCALATE_ONE_VALUE_FOR_ADDS(FIXED_COST_IN(R_POINTER), &
                          R_POINTER,FIXED_COST_ESCALATOR(R_POINTER), &
                              UNITNM(R_POINTER),R_YEAR)
      VOID_INT2 = &
       ESCALATE_ONE_VALUE_FOR_ADDS(ANNUAL_CL_FIXED_COST( &
        R_POINTER), &
                  R_POINTER,ANNUAL_CL_FIXED_COST_ESC(R_POINTER), &
                                             UNITNM(R_POINTER),R_YEAR)

      UPDATE_PR_ESCALATIONS = R_POINTER

      RETURN


      ENTRY INIT_MON_MDS_CL_UNITS(r_nunits_loc)


            IF(ALLOCATED(MON_MDS_CL_UNIT_FUEL_COST)) then
                deallocate(MON_MDS_CL_UNIT_FUEL_COST)
            end if

            IF(ALLOCATED(MON_MDS_CL_UNIT_EMISSIONS)) then
                deallocate(MON_MDS_CL_UNIT_EMISSIONS)
            end if

            IF(ALLOCATED(MON_MDS_CL_UNIT_EMISSIONS_COST)) then
                deallocate(MON_MDS_CL_UNIT_EMISSIONS_COST)
            end if

            IF(ALLOCATED(MON_MDS_CL_UNIT_CAPACITY)) then
                deallocate(MON_MDS_CL_UNIT_CAPACITY)
            end if

            IF(ALLOCATED(MON_MDS_CL_UNIT_ENERGY)) then
                deallocate(MON_MDS_CL_UNIT_ENERGY)
            end if

            IF(ALLOCATED(MON_MDS_CL_UNIT_VAR_COST)) then
                deallocate(MON_MDS_CL_UNIT_VAR_COST)
            end if

            IF(ALLOCATED(MON_MDS_CL_UNIT_FIXED_COST)) then
                deallocate(MON_MDS_CL_UNIT_FIXED_COST)
            end if

            IF(ALLOCATED(MON_MDS_NUC_FUEL_ADDER_COST)) then
                deallocate(MON_MDS_NUC_FUEL_ADDER_COST)
            end if

            IF(ALLOCATED(MON_MDS_ECO_SALES_REV_FROM)) then
                deallocate(MON_MDS_ECO_SALES_REV_FROM)
            end if

            IF(ALLOCATED(MON_MDS_ECO_SALES_ENRG_FROM)) then
                deallocate(MON_MDS_ECO_SALES_ENRG_FROM)
            end if

            IF(ALLOCATED(MON_MDS_ECO_PUCH_COST_FROM)) then
                deallocate(MON_MDS_ECO_PUCH_COST_FROM)
            end if

            IF(ALLOCATED(MON_MDS_ECO_PUCH_ENRG_FROM)) then
                deallocate(MON_MDS_ECO_PUCH_ENRG_FROM)
            end if

            IF(ALLOCATED(MON_MDS_ICAP_REVENUES)) then
                deallocate(MON_MDS_ICAP_REVENUES)
            end if

            IF(ALLOCATED(MON_MDS_CL_UNIT_MMBTUS)) then
                deallocate(MON_MDS_CL_UNIT_MMBTUS)
            end if

            ALLOCATE(MON_MDS_CL_UNIT_EMISSIONS( &
                              NUMBER_OF_EMISSION_TYPES,r_nunits_loc,0:12))
            ALLOCATE(MON_MDS_CL_UNIT_EMISSIONS_COST( &
                              NUMBER_OF_EMISSION_TYPES,r_nunits_loc,0:12))
            ALLOCATE(MON_MDS_CL_UNIT_CAPACITY(r_nunits_loc,0:12))
            ALLOCATE(MON_MDS_CL_UNIT_ENERGY(r_nunits_loc,0:12))
            ALLOCATE(MON_MDS_CL_UNIT_FUEL_COST(r_nunits_loc,0:12))
            ALLOCATE(MON_MDS_CL_UNIT_VAR_COST(r_nunits_loc,0:12))
            ALLOCATE(MON_MDS_CL_UNIT_FIXED_COST(r_nunits_loc,0:12))
            ALLOCATE(MON_MDS_NUC_FUEL_ADDER_COST(r_nunits_loc,0:12))
            ALLOCATE(MON_MDS_ECO_SALES_REV_FROM(r_nunits_loc,0:12))
            ALLOCATE(MON_MDS_ECO_SALES_ENRG_FROM(r_nunits_loc,0:12))
            ALLOCATE(MON_MDS_ECO_PUCH_COST_FROM(r_nunits_loc,0:12))
            ALLOCATE(MON_MDS_ECO_PUCH_ENRG_FROM(r_nunits_loc,0:12))
            ALLOCATE(MON_MDS_ICAP_REVENUES(r_nunits_loc,0:12))
            ALLOCATE(MON_MDS_CL_UNIT_MMBTUS(r_nunits_loc,0:12))

         MON_MDS_CL_UNIT_CAPACITY = 0.
         MON_MDS_CL_UNIT_ENERGY = 0.
         MON_MDS_CL_UNIT_FUEL_COST = 0.
         MON_MDS_CL_UNIT_VAR_COST = 0.
         MON_MDS_CL_UNIT_FIXED_COST = 0.
         MON_MDS_NUC_FUEL_ADDER_COST = 0.
         MON_MDS_ECO_SALES_REV_FROM = 0.
         MON_MDS_ECO_SALES_ENRG_FROM = 0.
         MON_MDS_ECO_PUCH_COST_FROM = 0.
         MON_MDS_ECO_PUCH_ENRG_FROM = 0.
         MON_MDS_ICAP_REVENUES = 0.

         MON_MDS_CL_UNIT_MMBTUS = 0.0D0

         MON_MDS_CL_UNIT_EMISSIONS = 0.
         MON_MDS_CL_UNIT_EMISSIONS_COST = 0.

         INIT_MON_MDS_CL_UNITS = .TRUE.

         PRICE_ONLY_WHOLESALE_REV = APPLY_TRANS_REV_TO_WHOLESALE()

         DETAILED_TRANSFER_PRICING = YES_DETAILED_TRANSFER_PRICING()

      RETURN

      ENTRY MON_MDS_CL_VAR(R_ISEAS,R_YR,R_UNITNO,R_FUEL_COST, &
                           R_ENRG,R_HEAT,R_EMIS,R_VOM_COST)

! MONTH
          if(r_unitno> 4000000000) then
            call end_program("cl_units_read_ext:0007 - r_unitno " // &
            "value is invalid.")
          end if
          MON_MDS_CL_UNIT_ENERGY(R_UNITNO,R_ISEAS) = R_ENRG + &
                           MON_MDS_CL_UNIT_ENERGY(R_UNITNO,R_ISEAS)
          MON_MDS_CL_UNIT_FUEL_COST(R_UNITNO,R_ISEAS) = &
                      MON_MDS_CL_UNIT_FUEL_COST(R_UNITNO,R_ISEAS) + &
                                                        R_FUEL_COST
          MON_MDS_CL_UNIT_VAR_COST(R_UNITNO,R_ISEAS) = &
                   MON_MDS_CL_UNIT_VAR_COST(R_UNITNO,R_ISEAS) + &
                                      R_VOM_COST
          MON_MDS_CL_UNIT_MMBTUS(R_UNITNO,R_ISEAS) = &
                         MON_MDS_CL_UNIT_MMBTUS(R_UNITNO,R_ISEAS) + &
                                                             R_HEAT
          
          !Access violation here
          MON_MDS_CL_UNIT_EMISSIONS(:,R_UNITNO,R_ISEAS) = &
                      MON_MDS_CL_UNIT_EMISSIONS(:,R_UNITNO,R_ISEAS) &
                      + R_EMIS(:)

         DO I4 =  1, 5
        MON_MDS_CL_UNIT_EMISSIONS_COST(I4,R_UNITNO,R_ISEAS) = &
            MON_MDS_CL_UNIT_EMISSIONS_COST(I4,R_UNITNO,R_ISEAS) &
                 + R_EMIS(I4) * 0.002 * &
           TRANS_DISPATCH_EMIS_ADDER(I4,R_ISEAS,R_YR,R_UNITNO)
          MON_MDS_CL_UNIT_EMISSIONS_COST(I4,R_UNITNO,0) = &
            MON_MDS_CL_UNIT_EMISSIONS_COST(I4,R_UNITNO,0) &
             + R_EMIS(I4) * 0.002 * &
            TRANS_DISPATCH_EMIS_ADDER(I4,R_ISEAS,R_YR,R_UNITNO)
         ENDDO
! ANNUAL
               MON_MDS_CL_UNIT_ENERGY(R_UNITNO,0) = R_ENRG &
                                   + MON_MDS_CL_UNIT_ENERGY(R_UNITNO,0)
               MON_MDS_CL_UNIT_FUEL_COST(R_UNITNO,0) = &
                                MON_MDS_CL_UNIT_FUEL_COST(R_UNITNO,0) &
                                   + R_FUEL_COST
               MON_MDS_CL_UNIT_VAR_COST(R_UNITNO,0) = &
                                 MON_MDS_CL_UNIT_VAR_COST(R_UNITNO,0) &
                                    + R_VOM_COST
               MON_MDS_CL_UNIT_MMBTUS(R_UNITNO,0) = &
                                   MON_MDS_CL_UNIT_MMBTUS(R_UNITNO,0) &
                                      + R_HEAT
               MON_MDS_CL_UNIT_EMISSIONS(:,R_UNITNO,0) = &
                         MON_MDS_CL_UNIT_EMISSIONS(:,R_UNITNO,0) &
                         + R_EMIS(:)

            MON_MDS_CL_VAR = .TRUE.

      RETURN


      ENTRY MON_MDS_CL_FIXED(R_ISEAS,R_UNITNO,R_FIXED_COST,R_CAP)

! MONTH
        MON_MDS_CL_UNIT_CAPACITY(R_UNITNO,R_ISEAS) = R_CAP + &
                             MON_MDS_CL_UNIT_CAPACITY(R_UNITNO,R_ISEAS)
        MON_MDS_CL_UNIT_FIXED_COST(R_UNITNO,R_ISEAS) = &
                      MON_MDS_CL_UNIT_FIXED_COST(R_UNITNO,R_ISEAS) + &
                                                           R_FIXED_COST

!     SALES
         MON_MDS_ECO_SALES_REV_FROM(R_UNITNO,R_ISEAS) = &
                       MON_MDS_ECO_SALES_REV_FROM(R_UNITNO,R_ISEAS) + &
                                       MON_ECO_SALES_REV_FROM(R_UNITNO)
         MON_MDS_ECO_SALES_ENRG_FROM(R_UNITNO,R_ISEAS) = &
                      MON_MDS_ECO_SALES_ENRG_FROM(R_UNITNO,R_ISEAS) + &
                                      MON_ECO_SALES_ENRG_FROM(R_UNITNO)
!     PURCHASES

         MON_MDS_ECO_PUCH_COST_FROM(R_UNITNO,R_ISEAS) = &
                       MON_MDS_ECO_PUCH_COST_FROM(R_UNITNO,R_ISEAS) + &
                                       MON_ECO_PUCH_COST_FROM(R_UNITNO)
         MON_MDS_ECO_PUCH_ENRG_FROM(R_UNITNO,R_ISEAS) = &
                       MON_MDS_ECO_PUCH_ENRG_FROM(R_UNITNO,R_ISEAS) + &
                                       MON_ECO_PUCH_ENRG_FROM(R_UNITNO)

! ANNUAL
        MON_MDS_CL_UNIT_CAPACITY(R_UNITNO,0) = R_CAP + &
                              MON_MDS_CL_UNIT_CAPACITY(R_UNITNO,0)
        MON_MDS_CL_UNIT_FIXED_COST(R_UNITNO,0) = &
                        MON_MDS_CL_UNIT_FIXED_COST(R_UNITNO,0) + &
                                                           R_FIXED_COST

!     SALES
         MON_MDS_ECO_SALES_REV_FROM(R_UNITNO,0) = &
                          MON_MDS_ECO_SALES_REV_FROM(R_UNITNO,0) + &
                                       MON_ECO_SALES_REV_FROM(R_UNITNO)
         MON_MDS_ECO_SALES_ENRG_FROM(R_UNITNO,0) = &
                         MON_MDS_ECO_SALES_ENRG_FROM(R_UNITNO,0) + &
                                      MON_ECO_SALES_ENRG_FROM(R_UNITNO)
!     PURCHASES

         MON_MDS_ECO_PUCH_COST_FROM(R_UNITNO,0) = &
                          MON_MDS_ECO_PUCH_COST_FROM(R_UNITNO,0) + &
                                       MON_ECO_PUCH_COST_FROM(R_UNITNO)
         MON_MDS_ECO_PUCH_ENRG_FROM(R_UNITNO,0) = &
                          MON_MDS_ECO_PUCH_ENRG_FROM(R_UNITNO,0) + &
                                       MON_ECO_PUCH_ENRG_FROM(R_UNITNO)

! icap_revenues
         MON_MDS_ICAP_REVENUES(R_UNITNO,R_ISEAS) = &
                      MON_MDS_ICAP_REVENUES(R_UNITNO,R_ISEAS) + &
                                        MONTHLY_ICAP_REVENUES(R_UNITNO)
         MON_MDS_ICAP_REVENUES(R_UNITNO,0) = &
                      MON_MDS_ICAP_REVENUES(R_UNITNO,0) + &
                                        MONTHLY_ICAP_REVENUES(R_UNITNO)

         MON_MDS_CL_FIXED = .TRUE.
      RETURN

      ENTRY MON_MDS_NUC_ADDER(R_ISEAS,R_UNITNO, &
                              R_NUC_UNIT_FUEL_ADDER_COST)

! MONTHLY
         MON_MDS_NUC_FUEL_ADDER_COST(R_UNITNO,R_ISEAS) = &
                                             R_NUC_UNIT_FUEL_ADDER_COST
! ANNUAL
         MON_MDS_NUC_FUEL_ADDER_COST(R_UNITNO,0) = &
                        MON_MDS_NUC_FUEL_ADDER_COST(R_UNITNO,0) + &
                          MON_MDS_NUC_FUEL_ADDER_COST(R_UNITNO,R_ISEAS)
         MON_MDS_NUC_ADDER = .TRUE.
      RETURN

      ENTRY RETURN_MAX_CL_CLASS_NUM

         RETURN_MAX_CL_CLASS_NUM = &
            ns_cla_decs%MAX_CAP_LIMITED_CLASS_ID_NUM
      RETURN

      ENTRY GET_CL_ASSET_CLASS_INFO(R_ASSET_CLASS_NUM,R_ASSET_VECTOR)

         DO I = 1, get_nunits()
            R_ASSET_CLASS_NUM(I) = ASSET_CLS_NUM(I)
            R_ASSET_VECTOR(I) = ASSET_CLS_VECTOR(I)
         ENDDO
         GET_CL_ASSET_CLASS_INFO = NUMBER_OF_CAP_LIMITED_CLASSES
      RETURN

      ENTRY SET_UP_CL_CLASS_ARRAYS


         IF(ALLOCATED(ns_cla_decs%ASSET_CLASS_ptr)) &
                            DEALLOCATE(ns_cla_decs%ASSET_CLASS_ptr)
         CALL RETURN_INITIALIZATION_CLASSES( &
                                       NUMBER_OF_CAP_LIMITED_CLASSES, &
            ns_cla_decs%MAX_CAP_LIMITED_CLASS_ID_NUM)
         IF(ns_cla_decs%MAX_CAP_LIMITED_CLASS_ID_NUM > 0) THEN
    ALLOCATE(ns_cla_decs%ASSET_CLASS_ptr( &
        ns_cla_decs%MAX_CAP_LIMITED_CLASS_ID_NUM) &
        )
         CALL RETURN_INITIALIZATION_POINTER(ns_cla_decs%ASSET_CLASS_ptr)
         ENDIF
         IF(ALLOCATED(CL_ANN_CLASS_FUEL_COST)) &
      DEALLOCATE(CL_ANN_CLASS_FUEL_COST, &
                 CL_ANN_MULT_FUEL_COST, &
                 FE_ANN_MULT_FUEL_COST, &
                 NF_FUEL_LEASED_BY_CLASS, &
                 NUC_FUEL_LEASED_BURN_BY_CLASS, &
                 NUC_FUEL_OWNED_BURN_BY_CLASS, &
                 NUCLEAR_MWH_BY_CLASS, &
                 NUCLEAR_MMBTU_BY_CLASS, &
                 NF_FUEL_OWNED_BY_CLASS, &
                 CL_ANN_CLASS_VAR_COST, &
                 CL_ANN_CLASS_FIXED_COST, &
                 CL_ANN_CLASS_REVENUE, &
                 CL_ANN_CLASS_CAPACITY, &
                 CL_ANN_CLASS_PURCHASES, &
                 CL_ANN_CLASS_ENERGY, &
                 CL_ANN_CLASS_EMISSIONS, &
                 CL_ANN_MULT_VAR_COST, &
                 CL_ANN_MULT_FIXED_COST, &
                 CL_ANN_MULT_REVENUE, &
                 CL_ANN_MULT_CAPACITY, &
                 CL_ANN_MULT_PURCHASES, &
                 CL_ANN_MULT_ENERGY, &
                 INTRA_COMPANY_REVENUES, &
                 INTRA_COMPANY_NF_BURN, &
       ns_cla_decs%MON_MDS_CL_CLASS_FUEL_COST, &
                 MON_MDS_CL_MULT_FUEL_COST, &
                 MON_MDS_CL_MULT_VAR_COST, &
                 MON_MDS_CL_MULT_FIXED_COST, &
                 MON_MDS_CL_MULT_CAPACITY, &
                 MON_MDS_CL_MULT_ENERGY, &
                 MON_MDS_CL_MULT_PURCHASES, &
                 MON_MDS_CL_CLASS_VAR_COST, &
                 MON_MDS_CL_CLASS_FIXED_COST, &
            ns_cla_decs%MON_MDS_CL_CLASS_REVENUE, &
               ns_cla_decs%MON_MDS_CL_CAP_REVENUE, &
                 MON_MDS_CL_CLASS_CAPACITY, &
                 ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES, &
                 ns_cla_decs%MON_MDS_CL_CAP_PURCHASES, &
                 MON_MDS_CL_CLASS_ENERGY, &
                 MON_MDS_CL_CLASS_EMISSIONS, &
                 MON_MDS_CL_CLASS_EMISSIONS_COST, &
                 MON_MDS_INTRA_COMPANY_REVENUES, &
                 MON_MDS_INTRA_COMPANY_NF_BURN, &
                ns_cla_decs%MON_MDS_NF_FUEL_LEASED_BC, &
                 ns_cla_decs%MON_MDS_NF_FUEL_OWNED_BC, &
                 MON_MDS_NUC_FUEL_LEASE_BURN_BC, &
                 MON_MDS_NUC_FUEL_OWNED_BURN_BC, &
                 MON_MDS_NUCLEAR_MWH_BY_CLASS, &
                 MON_MDS_NUCLEAR_MMBTU_BY_CLASS, &
                 ns_cla_decs%MON_MDS_ICAP_REV_BY_CLASS, &
                 MONTHLY_AC_WHOLESALE_PROD_COST, &
                 MONTHLY_AC_ECITY_VAR_PROD_COST, &
                                     MONTHLY_AC_ECITY_NEW_FIX_COST)
         ALLOCATE( &
             CL_ANN_CLASS_FUEL_COST(0:NUMBER_OF_CAP_LIMITED_CLASSES,4))
      ALLOCATE(CL_ANN_MULT_FUEL_COST(0:NUMBER_OF_CAP_LIMITED_CLASSES, &
                                                  NUM_FUEL_CATEGORIES))
      ALLOCATE(FE_ANN_MULT_FUEL_COST(0:NUMBER_OF_CAP_LIMITED_CLASSES, &
                                                  NUM_FUEL_CATEGORIES))
         ALLOCATE(NF_FUEL_LEASED_BY_CLASS( &
                                    0:NUMBER_OF_CAP_LIMITED_CLASSES,4))
         ALLOCATE(NF_FUEL_OWNED_BY_CLASS( &
                                    0:NUMBER_OF_CAP_LIMITED_CLASSES,4))
         ALLOCATE(CL_ANN_CLASS_VAR_COST( &
                                    0:NUMBER_OF_CAP_LIMITED_CLASSES,4))
         ALLOCATE(CL_ANN_CLASS_FIXED_COST( &
                                    0:NUMBER_OF_CAP_LIMITED_CLASSES,4))
         ALLOCATE(CL_ANN_CLASS_CAPACITY( &
                                    0:NUMBER_OF_CAP_LIMITED_CLASSES,4))
         ALLOCATE(CL_ANN_CLASS_ENERGY( &
                                    0:NUMBER_OF_CAP_LIMITED_CLASSES,4))
         ALLOCATE(CL_ANN_CLASS_REVENUE( &
                                    0:NUMBER_OF_CAP_LIMITED_CLASSES,5))
         ALLOCATE(CL_ANN_CLASS_PURCHASES( &
                                    0:NUMBER_OF_CAP_LIMITED_CLASSES,5))
         ALLOCATE(CL_ANN_CLASS_EMISSIONS(NUMBER_OF_EMISSION_TYPES, &
                                      0:NUMBER_OF_CAP_LIMITED_CLASSES))
       ALLOCATE(CL_ANN_MULT_VAR_COST(0:NUMBER_OF_CAP_LIMITED_CLASSES, &
                                                  NUM_FUEL_CATEGORIES))
         ALLOCATE( &
              CL_ANN_MULT_FIXED_COST(0:NUMBER_OF_CAP_LIMITED_CLASSES, &
                                                  NUM_FUEL_CATEGORIES))
       ALLOCATE(CL_ANN_MULT_REVENUE(0:NUMBER_OF_CAP_LIMITED_CLASSES, &
                                                  NUM_FUEL_CATEGORIES))
       ALLOCATE(CL_ANN_MULT_CAPACITY(0:NUMBER_OF_CAP_LIMITED_CLASSES, &
                                                  NUM_FUEL_CATEGORIES))
         ALLOCATE(CL_ANN_MULT_ENERGY(0:NUMBER_OF_CAP_LIMITED_CLASSES, &
                                                  NUM_FUEL_CATEGORIES))
         ALLOCATE(CL_ANN_MULT_PURCHASES(0: &
        NUMBER_OF_CAP_LIMITED_CLASSES, &
                                                  NUM_FUEL_CATEGORIES))
         ALLOCATE(NUC_FUEL_LEASED_BURN_BY_CLASS( &
                                      0:NUMBER_OF_CAP_LIMITED_CLASSES))
         ALLOCATE(NUC_FUEL_OWNED_BURN_BY_CLASS( &
                                      0:NUMBER_OF_CAP_LIMITED_CLASSES))
        ALLOCATE(NUCLEAR_MWH_BY_CLASS(0:NUMBER_OF_CAP_LIMITED_CLASSES))
         ALLOCATE(NUCLEAR_MMBTU_BY_CLASS( &
                                      0:NUMBER_OF_CAP_LIMITED_CLASSES))
         ALLOCATE(INTRA_COMPANY_REVENUES(0:1024,0:3))
         ALLOCATE(INTRA_COMPANY_NF_BURN(0:1024))
         ALLOCATE(ns_cla_decs%MON_MDS_CL_CLASS_FUEL_COST &
                              (0:NUMBER_OF_CAP_LIMITED_CLASSES,4,0:12))
         ALLOCATE(MON_MDS_CL_MULT_FUEL_COST &
                               (0:NUMBER_OF_CAP_LIMITED_CLASSES, &
                                             NUM_FUEL_CATEGORIES,0:12))
         ALLOCATE(MON_MDS_CL_MULT_VAR_COST &
                               (0:NUMBER_OF_CAP_LIMITED_CLASSES, &
                                             NUM_FUEL_CATEGORIES,0:12))
         ALLOCATE(MON_MDS_CL_MULT_FIXED_COST &
                               (0:NUMBER_OF_CAP_LIMITED_CLASSES, &
                                             NUM_FUEL_CATEGORIES,0:12))
         ALLOCATE(MON_MDS_CL_MULT_CAPACITY &
                               (0:NUMBER_OF_CAP_LIMITED_CLASSES, &
                                             NUM_FUEL_CATEGORIES,0:12))
         ALLOCATE(MON_MDS_CL_MULT_ENERGY &
                               (0:NUMBER_OF_CAP_LIMITED_CLASSES, &
                                             NUM_FUEL_CATEGORIES,0:12))
         ALLOCATE(MON_MDS_CL_MULT_PURCHASES &
                               (0:NUMBER_OF_CAP_LIMITED_CLASSES, &
                                             NUM_FUEL_CATEGORIES,0:12))
         ALLOCATE(MON_MDS_CL_CLASS_VAR_COST &
                              (0:NUMBER_OF_CAP_LIMITED_CLASSES,4,0:12))
         ALLOCATE(MON_MDS_CL_CLASS_FIXED_COST &
                              (0:NUMBER_OF_CAP_LIMITED_CLASSES,4,0:12))
         ALLOCATE(ns_cla_decs%MON_MDS_CL_CLASS_REVENUE &
                              (0:NUMBER_OF_CAP_LIMITED_CLASSES,5,0:12))
         ALLOCATE(ns_cla_decs%MON_MDS_CL_CAP_REVENUE &
                              (0:NUMBER_OF_CAP_LIMITED_CLASSES,5,0:12))
         ALLOCATE(MON_MDS_CL_CLASS_CAPACITY &
                              (0:NUMBER_OF_CAP_LIMITED_CLASSES,4,0:12))
         ALLOCATE(ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES &
                              (0:NUMBER_OF_CAP_LIMITED_CLASSES,5,0:12))
         ALLOCATE(ns_cla_decs%MON_MDS_CL_CAP_PURCHASES &
                              (0:NUMBER_OF_CAP_LIMITED_CLASSES,5,0:12))
         ALLOCATE(MON_MDS_CL_CLASS_ENERGY &
                              (0:NUMBER_OF_CAP_LIMITED_CLASSES,4,0:12))
       ALLOCATE(MON_MDS_CL_CLASS_EMISSIONS(NUMBER_OF_EMISSION_TYPES, &
                                 0:NUMBER_OF_CAP_LIMITED_CLASSES,0:12))
         ALLOCATE(MON_MDS_CL_CLASS_EMISSIONS_COST( &
            NUMBER_OF_EMISSION_TYPES,0:NUMBER_OF_CAP_LIMITED_CLASSES, &
                                                                 0:12))

         ALLOCATE(MON_MDS_INTRA_COMPANY_REVENUES(0:1024,0:3,0:12))
         ALLOCATE(MON_MDS_INTRA_COMPANY_NF_BURN(0:1024,0:12))
         ALLOCATE(ns_cla_decs%MON_MDS_NF_FUEL_LEASED_BC &
                              (0:NUMBER_OF_CAP_LIMITED_CLASSES,4,0:12))
         ALLOCATE(ns_cla_decs%MON_MDS_NF_FUEL_OWNED_BC &
                              (0:NUMBER_OF_CAP_LIMITED_CLASSES,4,0:12))
         ALLOCATE(MON_MDS_NUC_FUEL_LEASE_BURN_BC &
                                (0:NUMBER_OF_CAP_LIMITED_CLASSES,0:12))
         ALLOCATE(MON_MDS_NUC_FUEL_OWNED_BURN_BC &
                                (0:NUMBER_OF_CAP_LIMITED_CLASSES,0:12))
         ALLOCATE(MON_MDS_NUCLEAR_MWH_BY_CLASS &
                                (0:NUMBER_OF_CAP_LIMITED_CLASSES,0:12))
         ALLOCATE(MON_MDS_NUCLEAR_MMBTU_BY_CLASS &
                                (0:NUMBER_OF_CAP_LIMITED_CLASSES,0:12))
         ALLOCATE(ns_cla_decs%MON_MDS_ICAP_REV_BY_CLASS &
                                (0:NUMBER_OF_CAP_LIMITED_CLASSES,0:12))
         ALLOCATE(MONTHLY_AC_WHOLESALE_PROD_COST &
                                (0:NUMBER_OF_CAP_LIMITED_CLASSES,0:12))
         ALLOCATE(MONTHLY_AC_ECITY_VAR_PROD_COST &
                                (0:NUMBER_OF_CAP_LIMITED_CLASSES,0:12))
         ALLOCATE(MONTHLY_AC_ECITY_NEW_FIX_COST &
                                (0:NUMBER_OF_CAP_LIMITED_CLASSES,0:12))

         SET_UP_CL_CLASS_ARRAYS = &
            ns_cla_decs%MAX_CAP_LIMITED_CLASS_ID_NUM

         IF(UPPER_TRANS_Gp > 0) THEN
            IF(ALLOCATED(PURCHASE_ASSET_CLASS_ID)) &
                                  DEALLOCATE(PURCHASE_ASSET_CLASS_ID, &
                                                PURCHASE_POWER_ASSIGN)
            ALLOCATE(PURCHASE_ASSET_CLASS_ID(0:UPPER_TRANS_Gp))
            ALLOCATE(PURCHASE_POWER_ASSIGN(0:UPPER_TRANS_Gp))
            DO I = 1, UPPER_TRANS_Gp

! NEED TO CHECK WHETHER CLASS EXISTS

               PURCHASE_ASSET_CLASS_ID(I) = &
                           GET_PURCHASE_ASSET_CLASS_ID(I)
               VOID_LOGICAL = GET_PURCHASE_POWER_ASSIGN(I, &
                                 PURCHASE_POWER_ASSIGN(I))
            ENDDO
         ENDIF

      RETURN

      ENTRY ZERO_CL_CLASS_ARRAYS


! MONTHLY INITIALIZATION. NECESSARY TO ACCUMULATE ANNUAL VALUES.


         ns_cla_decs%MON_MDS_NF_FUEL_LEASED_BC = 0.
         ns_cla_decs%MON_MDS_NF_FUEL_OWNED_BC = 0.
         MON_MDS_INTRA_COMPANY_REVENUES = 0.
         MON_MDS_INTRA_COMPANY_NF_BURN = 0.
         MON_MDS_NUC_FUEL_LEASE_BURN_BC = 0.
         MON_MDS_NUC_FUEL_OWNED_BURN_BC = 0.
         MON_MDS_NUCLEAR_MWH_BY_CLASS = 0.
         MON_MDS_NUCLEAR_MMBTU_BY_CLASS = 0.
         ns_cla_decs%MON_MDS_ICAP_REV_BY_CLASS = 0.
         MONTHLY_AC_WHOLESALE_PROD_COST = 0.
         MONTHLY_AC_ECITY_VAR_PROD_COST = 0.
         MONTHLY_AC_ECITY_NEW_FIX_COST = 0.
         MON_MDS_CL_CLASS_VAR_COST = 0.
         ns_cla_decs%MON_MDS_CL_CLASS_FUEL_COST = 0.
         MON_MDS_CL_CLASS_FIXED_COST = 0.
         MON_MDS_CL_CLASS_CAPACITY = 0.
         MON_MDS_CL_CLASS_ENERGY = 0.
         ns_cla_decs%MON_MDS_CL_CLASS_REVENUE = 0.
         ns_cla_decs%MON_MDS_CL_CAP_REVENUE = 0.
         ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES = 0.
         ns_cla_decs%MON_MDS_CL_CAP_PURCHASES = 0.
         MON_MDS_CL_CLASS_EMISSIONS = 0.
         MON_MDS_CL_CLASS_EMISSIONS_COST = 0.

         MON_MDS_CL_MULT_FUEL_COST = 0.
         MON_MDS_CL_MULT_VAR_COST = 0.
         MON_MDS_CL_MULT_FIXED_COST = 0.
         MON_MDS_CL_MULT_CAPACITY = 0.
         MON_MDS_CL_MULT_ENERGY = 0.
         MON_MDS_CL_MULT_PURCHASES = 0.

         FIRST_YEAR_PRICE_MODE = GET_FIRST_YEAR_PRICE_MODE()


         ZERO_CL_CLASS_ARRAYS = NUMBER_OF_CAP_LIMITED_CLASSES

      RETURN

      ENTRY MON_MDS_CL_EXP_2_AC(R_ISEAS)


         ALLOCATE(ASSET_CLASS_LIST(AVAIL_DATA_YEARS))
         ALLOCATE(ASSET_ALLOCATION_LIST(AVAIL_DATA_YEARS))
         CURRENT_YEAR = globecom_year+get_BASE_YEAR()
         CURRENT_YEAR_COMPARISON = (CURRENT_YEAR-1900)*100

         ECITIES = ECITY_COMPANY() ! YES_ECITIES_UNITS_ACTIVE()

         MAX_INTRA_CLASS = 0
         INTRA_FOSSIL_FUEL_EXPENSES = 0.
         INTRA_LEASED_NUC_FUEL_EXPENSES = 0.
         INTRA_OWNED_NUC_FUEL_EXPENSES = 0.
         INTRA_NUC_OWN_BURN = 0.
         INTRA_NUC_LEASE_BURN = 0.
         INTRA_PURCHASE_EXPENSES = 0.
         BASE_MARKET_ENRG_SALES = 0.
         BASE_MARKET_REVENUES = 0.
         PEAK_MARKET_ENRG_SALES = 0.
         PEAK_MARKET_REVENUES = 0.
         MON_MDS_CL_EXP_2_AC = UNUM

         DO UNUM = 1, get_nunits()

            IF(ONLINE(UNUM) - CURRENT_YEAR_COMPARISON > 12  .OR. &
                    CURRENT_YEAR_COMPARISON - OFLINE(UNUM) > 12) CYCLE

            CLASS_POINTER = 1

            IF(MON_MDS_CL_UNIT_ENERGY(UNUM,R_ISEAS) > .45) THEN
               PERCENT_RETAIL = 1. - &
                     MON_MDS_ECO_SALES_ENRG_FROM(UNUM,R_ISEAS)/ &
                                  MON_MDS_CL_UNIT_ENERGY(UNUM,R_ISEAS)
            ELSE
               PERCENT_RETAIL = 1. ! 100 %
            ENDIF

            IF(PRICE_ONLY_WHOLESALE_REV) THEN
               OLD_ADJ_LOGIC = .FALSE. ! THIS WILL INVOKE THE IPL FAC
            ELSE
               OLD_ADJ_LOGIC = .TRUE.
            ENDIF

            IF(INTRA_COMPANY_CLSID(UNUM) >= 0 .AND. &
                           INTRA_CO_transaction_loc(UNUM) == 'Y') THEN
               ns_cla_decs%ASSET_CLS_LOC = INTRA_COMPANY_CLSID(UNUM)
               CALL CHECK_IF_CLASS_DEFINED(ns_cla_decs%ASSET_CLS_LOC)
               ns_cla_decs%ASSET_CLS_LOC = ns_cla_decs%ASSET_CLS_LOC + 1
               MAX_INTRA_CLASS = &
                MAX(MAX_INTRA_CLASS,ns_cla_decs%ASSET_CLS_LOC)

               SELECT CASE (EXPENSE_ASSIGNMENT(UNUM))
               CASE('F')  ! FUEL REVENUES
                  COLLECTION = MAX(1, &
                                INDEX('BA',(EXPENSE_COLLECTION(UNUM))))
      IF(COLLECTION /=1 .OR. PERCENT_RETAIL > .9999 .OR. &
                                        OLD_ADJ_LOGIC) THEN
         MON_MDS_INTRA_COMPANY_REVENUES( &
                       ns_cla_decs%ASSET_CLS_LOC,COLLECTION,R_ISEAS) = &
            MON_MDS_INTRA_COMPANY_REVENUES( &
                       ns_cla_decs%ASSET_CLS_LOC,COLLECTION,R_ISEAS) + &
                  MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS) &
                               *CL_POOL_FRAC_OWN(UNUM)/100.
      ELSE
         MON_MDS_INTRA_COMPANY_REVENUES( &
                       ns_cla_decs%ASSET_CLS_LOC,COLLECTION,R_ISEAS) = &
            MON_MDS_INTRA_COMPANY_REVENUES( &
                       ns_cla_decs%ASSET_CLS_LOC,COLLECTION,R_ISEAS) + &
                  MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS) &
                *PERCENT_RETAIL*CL_POOL_FRAC_OWN(UNUM)/100.
         MON_MDS_INTRA_COMPANY_REVENUES( &
                           ns_cla_decs%ASSET_CLS_LOC,2,R_ISEAS) = &
            MON_MDS_INTRA_COMPANY_REVENUES( &
                           ns_cla_decs%ASSET_CLS_LOC,2,R_ISEAS) + &
                  MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS) &
                       *(1.-PERCENT_RETAIL)*CL_POOL_FRAC_OWN(UNUM)/100.
                  ENDIF
               CASE('L','O')  ! FUEL REVENUES
                  COLLECTION = MAX(1, &
                                INDEX('BA',(EXPENSE_COLLECTION(UNUM))))
                  IF(COLLECTION /=1 .OR. PERCENT_RETAIL > .9999 .OR. &
                                                    OLD_ADJ_LOGIC) THEN
                     MON_MDS_INTRA_COMPANY_REVENUES( &
               ns_cla_decs%ASSET_CLS_LOC,COLLECTION,R_ISEAS) = &
                        MON_MDS_INTRA_COMPANY_REVENUES( &
                       ns_cla_decs%ASSET_CLS_LOC,COLLECTION,R_ISEAS) + &
                              MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS) &
                                           *CL_POOL_FRAC_OWN(UNUM)/100.
                  ELSE
                     MON_MDS_INTRA_COMPANY_REVENUES( &
                      ns_cla_decs%ASSET_CLS_LOC,COLLECTION,R_ISEAS) = &
                        MON_MDS_INTRA_COMPANY_REVENUES( &
                     ns_cla_decs%ASSET_CLS_LOC,COLLECTION,R_ISEAS) + &
                              MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS) &
                            *PERCENT_RETAIL*CL_POOL_FRAC_OWN(UNUM)/100.
                     MON_MDS_INTRA_COMPANY_REVENUES( &
                                ns_cla_decs%ASSET_CLS_LOC,2,R_ISEAS) = &
                        MON_MDS_INTRA_COMPANY_REVENUES( &
                             ns_cla_decs%ASSET_CLS_LOC,2,R_ISEAS) + &
                              MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS) &
                       *(1.-PERCENT_RETAIL)*CL_POOL_FRAC_OWN(UNUM)/100.
                  ENDIF
    MON_MDS_INTRA_COMPANY_NF_BURN(ns_cla_decs%ASSET_CLS_LOC,R_ISEAS) = &
    MON_MDS_INTRA_COMPANY_NF_BURN(ns_cla_decs%ASSET_CLS_LOC,R_ISEAS) + &
                           (MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS) - &
                           MON_MDS_NUC_FUEL_ADDER_COST(UNUM,R_ISEAS)) &
                                           *CL_POOL_FRAC_OWN(UNUM)/100.
               CASE('P')  ! PURCHASE REVENUES
                  MON_MDS_INTRA_COMPANY_REVENUES( &
                               ns_cla_decs%ASSET_CLS_LOC,3,R_ISEAS) = &
                         MON_MDS_INTRA_COMPANY_REVENUES( &
                              ns_cla_decs%ASSET_CLS_LOC,3,R_ISEAS) + &
                           (MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS) + &
                             MON_MDS_CL_UNIT_VAR_COST(UNUM,R_ISEAS) + &
                            MON_MDS_CL_UNIT_FIXED_COST(UNUM,R_ISEAS)) &
                                           *CL_POOL_FRAC_OWN(UNUM)/100.
               CASE DEFAULT
               END SELECT

               SELECT CASE (EXPENSE_ASSIGNMENT(UNUM))
               CASE('F')  ! FUEL EXPENSES
                  INTRA_FOSSIL_FUEL_EXPENSES = &
                         INTRA_FOSSIL_FUEL_EXPENSES + &
                              MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS) &
                                           *CL_POOL_FRAC_OWN(UNUM)/100.
               CASE('L')  ! NUCLEAR FUEL EXPENSES
                  INTRA_LEASED_NUC_FUEL_EXPENSES = &
                            INTRA_LEASED_NUC_FUEL_EXPENSES + &
                             MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS) &
                                           *CL_POOL_FRAC_OWN(UNUM)/100.
                  INTRA_NUC_LEASE_BURN = INTRA_NUC_LEASE_BURN + &
                           (MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS) - &
                           MON_MDS_NUC_FUEL_ADDER_COST(UNUM,R_ISEAS)) &
                                           *CL_POOL_FRAC_OWN(UNUM)/100.
               CASE('O')  ! NUCLEAR FUEL EXPENSES
                  INTRA_OWNED_NUC_FUEL_EXPENSES = &
                          INTRA_OWNED_NUC_FUEL_EXPENSES + &
                              MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS) &
                                          *CL_POOL_FRAC_OWN(UNUM)/100.
                  INTRA_NUC_OWN_BURN = INTRA_NUC_OWN_BURN + &
                          (MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS) - &
                           MON_MDS_NUC_FUEL_ADDER_COST(UNUM,R_ISEAS)) &
                                          *CL_POOL_FRAC_OWN(UNUM)/100.
               CASE('P')  ! PURCHASE EXPENSES
                  INTRA_PURCHASE_EXPENSES = INTRA_PURCHASE_EXPENSES + &
                          (MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS) + &
                             MON_MDS_CL_UNIT_VAR_COST(UNUM,R_ISEAS) + &
                           MON_MDS_CL_UNIT_FIXED_COST(UNUM,R_ISEAS)) &
                                          *CL_POOL_FRAC_OWN(UNUM)/100.
               CASE DEFAULT
               END SELECT
            ENDIF

            COLLECTION = INDEX('ABNX',(EXPENSE_COLLECTION(UNUM)))
            IF(COLLECTION == 0) COLLECTION = 3
            ns_cla_decs%ASSET_CLS_LOC = ASSET_CLS_NUM(UNUM)
            ASSET_ALLOCATION_VECTOR = ASSET_CLS_VECTOR(UNUM)

     VOID_LOGICAL=RETURN_ASSET_CLASS_LISTS(ns_cla_decs%ASSET_CLS_LOC, &
                                                ASSET_CLASS_LIST, &
                                             ASSET_ALLOCATION_VECTOR, &
                                                ASSET_ALLOCATION_LIST)

            FT = PRIMARY_MOVER(UNUM)

            if(FT==0)  FT = 11
            TT = 10


            DO
            ns_cla_decs%ASSET_CLS_LOC = ASSET_CLASS_LIST(CLASS_POINTER)
               CALL CHECK_IF_CLASS_DEFINED(ns_cla_decs%ASSET_CLS_LOC)

! IDENTIFYING BASE AND PEAK EL REVENUE CLASSES

               BASE_EL_REVENUE_CASE = (ns_cla_decs%ASSET_CLS_LOC == 10)
               PEAK_EL_REVENUE_CASE = (ns_cla_decs%ASSET_CLS_LOC == 19)

               ns_cla_decs%ASSET_CLS_LOC = ns_cla_decs%ASSET_CLS_LOC + 1
               IF(ns_cla_decs%ASSET_CLS_LOC <= 0 .OR. &
                    ns_cla_decs%ASSET_CLS_LOC > &
                ns_cla_decs%MAX_CAP_LIMITED_CLASS_ID_NUM) then
                        CYCLE
               endif

              ns_cla_decs%ASSET_CLS_LOC = &
              ns_cla_decs%ASSET_CLASS_ptr(ns_cla_decs%ASSET_CLS_LOC)
               IF(ASSET_ALLOCATION_LIST(CLASS_POINTER) < 0.) THEN
                  ALLOCATION_VECTOR = &
                             ABS(ASSET_ALLOCATION_LIST(CLASS_POINTER))
                  CALL GET_ASSET_VAR(ALLOCATION_VECTOR, &
                                          DUMMY_TYPE,ALLOCATION_VALUE)
                  ASSET_ALLOCATOR = CL_POOL_FRAC_OWN(UNUM)/100. * &
                     ALLOCATION_VALUE(MIN(globecom_year,AVAIL_DATA_YEARS))/100.
               ELSE
                  ASSET_ALLOCATOR = &
                       ASSET_ALLOCATION_LIST(CLASS_POINTER)/100. * &
                             CL_POOL_FRAC_OWN(UNUM)/100.
               ENDIF

               DO I = 1, NUMBER_OF_EMISSION_TYPES
                 MON_MDS_CL_CLASS_EMISSIONS(I, &
                 ns_cla_decs%ASSET_CLS_LOC,R_ISEAS) = &
                   MON_MDS_CL_CLASS_EMISSIONS(I, &
                   ns_cla_decs%ASSET_CLS_LOC,R_ISEAS) &
                       + ASSET_ALLOCATOR * &
                             MON_MDS_CL_UNIT_EMISSIONS(I,UNUM,R_ISEAS)
               ENDDO
            MON_MDS_CL_CLASS_EMISSIONS_COST(:, &
            ns_cla_decs%ASSET_CLS_LOC,R_ISEAS) = &
              MON_MDS_CL_CLASS_EMISSIONS_COST(:, &
              ns_cla_decs%ASSET_CLS_LOC,R_ISEAS) &
                  + ASSET_ALLOCATOR * &
                        MON_MDS_CL_UNIT_EMISSIONS_COST(:,UNUM,R_ISEAS)

               IF(EXPENSE_ASSIGNMENT(UNUM) == 'L') THEN
                  IF(COLLECTION /=1 .OR. PERCENT_RETAIL > .9999 .OR. &
                                                   OLD_ADJ_LOGIC) THEN
                     ns_cla_decs%MON_MDS_NF_FUEL_LEASED_BC( &
                      ns_cla_decs%ASSET_CLS_LOC,COLLECTION,R_ISEAS) = &
                       ns_cla_decs%MON_MDS_NF_FUEL_LEASED_BC( &
                   ns_cla_decs%ASSET_CLS_LOC,COLLECTION,R_ISEAS) + &
                        ASSET_ALLOCATOR * &
                              MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS)
                  ELSE
                     ns_cla_decs%MON_MDS_NF_FUEL_LEASED_BC( &
                       ns_cla_decs%ASSET_CLS_LOC,COLLECTION,R_ISEAS) = &
                       ns_cla_decs%MON_MDS_NF_FUEL_LEASED_BC( &
                    ns_cla_decs%ASSET_CLS_LOC,COLLECTION,R_ISEAS) + &
                        ASSET_ALLOCATOR * PERCENT_RETAIL * &
                               MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS)
                     ns_cla_decs%MON_MDS_NF_FUEL_LEASED_BC( &
                           ns_cla_decs%ASSET_CLS_LOC,2,R_ISEAS) = &
                       ns_cla_decs%MON_MDS_NF_FUEL_LEASED_BC( &
                             ns_cla_decs%ASSET_CLS_LOC,2,R_ISEAS) + &
                        ASSET_ALLOCATOR * (1.-PERCENT_RETAIL) * &
                               MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS)
                  ENDIF
   MON_MDS_NUC_FUEL_LEASE_BURN_BC(ns_cla_decs%ASSET_CLS_LOC,R_ISEAS) = &
                      MON_MDS_NUC_FUEL_LEASE_BURN_BC( &
                                  ns_cla_decs%ASSET_CLS_LOC,R_ISEAS) + &
                      ASSET_ALLOCATOR * &
                          (MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS) - &
                            MON_MDS_NUC_FUEL_ADDER_COST(UNUM,R_ISEAS))
     MON_MDS_NUCLEAR_MWH_BY_CLASS(ns_cla_decs%ASSET_CLS_LOC,R_ISEAS) = &
                           MON_MDS_NUCLEAR_MWH_BY_CLASS( &
                                  ns_cla_decs%ASSET_CLS_LOC,R_ISEAS) + &
                           ASSET_ALLOCATOR * &
                                 MON_MDS_CL_UNIT_ENERGY(UNUM,R_ISEAS)
   MON_MDS_NUCLEAR_MMBTU_BY_CLASS(ns_cla_decs%ASSET_CLS_LOC,R_ISEAS) = &
                        MON_MDS_NUCLEAR_MMBTU_BY_CLASS( &
                                  ns_cla_decs%ASSET_CLS_LOC,R_ISEAS) + &
                           ASSET_ALLOCATOR * &
                           MON_MDS_CL_UNIT_MMBTUS(UNUM,R_ISEAS)
               ELSEIF(EXPENSE_ASSIGNMENT(UNUM) == 'O') THEN
                  IF(COLLECTION /=1 .OR. PERCENT_RETAIL > .9999 .OR. &
                                                    OLD_ADJ_LOGIC) THEN
                     ns_cla_decs%MON_MDS_NF_FUEL_OWNED_BC( &
                       ns_cla_decs%ASSET_CLS_LOC,COLLECTION,R_ISEAS) = &
                        ns_cla_decs%MON_MDS_NF_FUEL_OWNED_BC( &
                      ns_cla_decs%ASSET_CLS_LOC,COLLECTION,R_ISEAS) + &
                        ASSET_ALLOCATOR * &
                               MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS)
                  ELSE
                     ns_cla_decs%MON_MDS_NF_FUEL_OWNED_BC( &
                     ns_cla_decs%ASSET_CLS_LOC,COLLECTION,R_ISEAS) = &
                        ns_cla_decs%MON_MDS_NF_FUEL_OWNED_BC( &
                       ns_cla_decs%ASSET_CLS_LOC,COLLECTION,R_ISEAS) + &
                        ASSET_ALLOCATOR * PERCENT_RETAIL * &
                                MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS)
                     ns_cla_decs%MON_MDS_NF_FUEL_OWNED_BC( &
                           ns_cla_decs%ASSET_CLS_LOC,2,R_ISEAS) = &
                        ns_cla_decs%MON_MDS_NF_FUEL_OWNED_BC( &
                             ns_cla_decs%ASSET_CLS_LOC,2,R_ISEAS) + &
                        ASSET_ALLOCATOR * (1.-PERCENT_RETAIL) * &
                               MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS)
                  ENDIF
   MON_MDS_NUC_FUEL_OWNED_BURN_BC(ns_cla_decs%ASSET_CLS_LOC,R_ISEAS) = &
                      MON_MDS_NUC_FUEL_OWNED_BURN_BC( &
                                ns_cla_decs%ASSET_CLS_LOC,R_ISEAS) + &
                      ASSET_ALLOCATOR * &
                          (MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS) - &
                           MON_MDS_NUC_FUEL_ADDER_COST(UNUM,R_ISEAS))
     MON_MDS_NUCLEAR_MWH_BY_CLASS(ns_cla_decs%ASSET_CLS_LOC,R_ISEAS) = &
    MON_MDS_NUCLEAR_MWH_BY_CLASS(ns_cla_decs%ASSET_CLS_LOC,R_ISEAS) + &
                          ASSET_ALLOCATOR * &
                           MON_MDS_CL_UNIT_ENERGY(UNUM,R_ISEAS)
   MON_MDS_NUCLEAR_MMBTU_BY_CLASS(ns_cla_decs%ASSET_CLS_LOC,R_ISEAS) = &
                        MON_MDS_NUCLEAR_MMBTU_BY_CLASS( &
                                  ns_cla_decs%ASSET_CLS_LOC,R_ISEAS) + &
                           ASSET_ALLOCATOR * &
                           MON_MDS_CL_UNIT_MMBTUS(UNUM,R_ISEAS)
               ELSEIF(EXPENSE_ASSIGNMENT(UNUM) == 'P') THEN
                  IF(COLLECTION /=1 .OR. PERCENT_RETAIL > .9999 .OR. &
                                                   OLD_ADJ_LOGIC) THEN
                     ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES( &
                       ns_cla_decs%ASSET_CLS_LOC,COLLECTION,R_ISEAS) = &
                        ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES( &
                     ns_cla_decs%ASSET_CLS_LOC,COLLECTION,R_ISEAS) + &
                              ASSET_ALLOCATOR * &
                           (MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS) + &
                             MON_MDS_CL_UNIT_VAR_COST(UNUM,R_ISEAS) + &
                             MON_MDS_CL_UNIT_FIXED_COST(UNUM,R_ISEAS))
                  ELSE
                    ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES( &
                     ns_cla_decs%ASSET_CLS_LOC,COLLECTION,R_ISEAS) = &
                        ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES( &
                    ns_cla_decs%ASSET_CLS_LOC,COLLECTION,R_ISEAS) + &
                              ASSET_ALLOCATOR * PERCENT_RETAIL * &
                           (MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS) + &
                             MON_MDS_CL_UNIT_VAR_COST(UNUM,R_ISEAS) + &
                              MON_MDS_CL_UNIT_FIXED_COST(UNUM,R_ISEAS))
                     ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES( &
                        ns_cla_decs%ASSET_CLS_LOC,2,R_ISEAS) = &
                        ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES( &
                           ns_cla_decs%ASSET_CLS_LOC,2,R_ISEAS) + &
                              ASSET_ALLOCATOR * (1.-PERCENT_RETAIL) * &
                           (MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS) + &
                             MON_MDS_CL_UNIT_VAR_COST(UNUM,R_ISEAS) + &
                             MON_MDS_CL_UNIT_FIXED_COST(UNUM,R_ISEAS))
                  ENDIF
      MON_MDS_CL_CLASS_CAPACITY(ns_cla_decs%ASSET_CLS_LOC,2,R_ISEAS) = &
      MON_MDS_CL_CLASS_CAPACITY(ns_cla_decs%ASSET_CLS_LOC,2,R_ISEAS) + &
                         ASSET_ALLOCATOR * &
                           MON_MDS_CL_UNIT_CAPACITY(UNUM,R_ISEAS)
        MON_MDS_CL_CLASS_ENERGY(ns_cla_decs%ASSET_CLS_LOC,2,R_ISEAS) = &
        MON_MDS_CL_CLASS_ENERGY(ns_cla_decs%ASSET_CLS_LOC,2,R_ISEAS) + &
                           ASSET_ALLOCATOR * &
                           MON_MDS_CL_UNIT_ENERGY(UNUM,R_ISEAS)

     MON_MDS_CL_MULT_PURCHASES(ns_cla_decs%ASSET_CLS_LOC,FT,R_ISEAS) = &
     MON_MDS_CL_MULT_PURCHASES(ns_cla_decs%ASSET_CLS_LOC,FT,R_ISEAS) + &
                              ASSET_ALLOCATOR * &
                           (MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS) + &
                             MON_MDS_CL_UNIT_VAR_COST(UNUM,R_ISEAS) + &
                              MON_MDS_CL_UNIT_FIXED_COST(UNUM,R_ISEAS))

               ELSE
                  IF(COLLECTION /=1 .OR. PERCENT_RETAIL > .9999 .OR. &
                                                   OLD_ADJ_LOGIC) THEN
                     ns_cla_decs%MON_MDS_CL_CLASS_FUEL_COST( &
                    ns_cla_decs%ASSET_CLS_LOC,COLLECTION,R_ISEAS) = &
                        ns_cla_decs%MON_MDS_CL_CLASS_FUEL_COST( &
                       ns_cla_decs%ASSET_CLS_LOC,COLLECTION,R_ISEAS) + &
                        ASSET_ALLOCATOR * &
                               MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS)
                  ELSE
! FOR IPL, KCPL AND OTHERS
                     ns_cla_decs%MON_MDS_CL_CLASS_FUEL_COST( &
                       ns_cla_decs%ASSET_CLS_LOC,COLLECTION,R_ISEAS) = &
                        ns_cla_decs%MON_MDS_CL_CLASS_FUEL_COST( &
                       ns_cla_decs%ASSET_CLS_LOC,COLLECTION,R_ISEAS) + &
                        ASSET_ALLOCATOR * PERCENT_RETAIL * &
                                MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS)

! ASSIGN THE BALANCE OF THE ADJ TO UNCOLLECTED PER KATHY ANDERSON.

                     ns_cla_decs%MON_MDS_CL_CLASS_FUEL_COST( &
                           ns_cla_decs%ASSET_CLS_LOC,2,R_ISEAS) = &
                        ns_cla_decs%MON_MDS_CL_CLASS_FUEL_COST( &
                           ns_cla_decs%ASSET_CLS_LOC,2,R_ISEAS) + &
                        ASSET_ALLOCATOR * (1.-PERCENT_RETAIL) * &
                               MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS)
                  ENDIF

                  MON_MDS_CL_MULT_FUEL_COST( &
                          ns_cla_decs%ASSET_CLS_LOC,FT,R_ISEAS) = &
                        MON_MDS_CL_MULT_FUEL_COST( &
                         ns_cla_decs%ASSET_CLS_LOC,FT,R_ISEAS) + &
                        ASSET_ALLOCATOR * &
                               MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS)
               ENDIF
               IF(EXPENSE_ASSIGNMENT(UNUM) /= 'P') THEN
                  MON_MDS_CL_CLASS_VAR_COST( &
                     ns_cla_decs%ASSET_CLS_LOC,COLLECTION,R_ISEAS) = &
                      MON_MDS_CL_CLASS_VAR_COST( &
                    ns_cla_decs%ASSET_CLS_LOC,COLLECTION,R_ISEAS) + &
                         ASSET_ALLOCATOR * &
                                MON_MDS_CL_UNIT_VAR_COST(UNUM,R_ISEAS)
                  MON_MDS_CL_CLASS_FIXED_COST( &
                     ns_cla_decs%ASSET_CLS_LOC,COLLECTION,R_ISEAS)= &
                       MON_MDS_CL_CLASS_FIXED_COST( &
                    ns_cla_decs%ASSET_CLS_LOC,COLLECTION,R_ISEAS) + &
                           ASSET_ALLOCATOR * &
                              MON_MDS_CL_UNIT_FIXED_COST(UNUM,R_ISEAS)
      MON_MDS_CL_CLASS_CAPACITY(ns_cla_decs%ASSET_CLS_LOC,1,R_ISEAS) = &
      MON_MDS_CL_CLASS_CAPACITY(ns_cla_decs%ASSET_CLS_LOC,1,R_ISEAS) + &
                   ASSET_ALLOCATOR * &
                   MON_MDS_CL_UNIT_CAPACITY(UNUM,R_ISEAS)
      MON_MDS_CL_CLASS_ENERGY(ns_cla_decs%ASSET_CLS_LOC,1,R_ISEAS) = &
        MON_MDS_CL_CLASS_ENERGY(ns_cla_decs%ASSET_CLS_LOC,1,R_ISEAS) + &
               ASSET_ALLOCATOR * &
               MON_MDS_CL_UNIT_ENERGY(UNUM,R_ISEAS)
      ns_cla_decs%MON_MDS_ICAP_REV_BY_CLASS( &
        ns_cla_decs%ASSET_CLS_LOC,R_ISEAS) = &
       ns_cla_decs%MON_MDS_ICAP_REV_BY_CLASS( &
        ns_cla_decs%ASSET_CLS_LOC,R_ISEAS) + &
               ASSET_ALLOCATOR * &
               MON_MDS_ICAP_REVENUES(UNUM,R_ISEAS)

      MON_MDS_CL_MULT_VAR_COST(ns_cla_decs%ASSET_CLS_LOC,FT,R_ISEAS) = &
      MON_MDS_CL_MULT_VAR_COST(ns_cla_decs%ASSET_CLS_LOC,FT,R_ISEAS) + &
                ASSET_ALLOCATOR * &
                    MON_MDS_CL_UNIT_VAR_COST(UNUM,R_ISEAS)
      MON_MDS_CL_MULT_FIXED_COST( &
                            ns_cla_decs%ASSET_CLS_LOC,FT,R_ISEAS)= &
                       MON_MDS_CL_MULT_FIXED_COST( &
            ns_cla_decs%ASSET_CLS_LOC,FT,R_ISEAS) + &
                           ASSET_ALLOCATOR * &
                              MON_MDS_CL_UNIT_FIXED_COST(UNUM,R_ISEAS)
      MON_MDS_CL_MULT_CAPACITY(ns_cla_decs%ASSET_CLS_LOC,FT,R_ISEAS) = &
      MON_MDS_CL_MULT_CAPACITY(ns_cla_decs%ASSET_CLS_LOC,FT,R_ISEAS) + &
                               ASSET_ALLOCATOR * &
                               MON_MDS_CL_UNIT_CAPACITY(UNUM,R_ISEAS)
      MON_MDS_CL_MULT_ENERGY(ns_cla_decs%ASSET_CLS_LOC,FT,R_ISEAS) = &
      MON_MDS_CL_MULT_ENERGY(ns_cla_decs%ASSET_CLS_LOC,FT,R_ISEAS) + &
                           ASSET_ALLOCATOR * &
                           MON_MDS_CL_UNIT_ENERGY(UNUM,R_ISEAS)
               ENDIF
               IF(MON_MDS_ECO_SALES_ENRG_FROM(UNUM,R_ISEAS) /= 0.) THEN
                  ns_cla_decs%MON_MDS_CL_CLASS_REVENUE( &
                       ns_cla_decs%ASSET_CLS_LOC,COLLECTION,R_ISEAS) = &
                      ns_cla_decs%MON_MDS_CL_CLASS_REVENUE( &
                       ns_cla_decs%ASSET_CLS_LOC,COLLECTION,R_ISEAS) + &
                              ASSET_ALLOCATOR * &
                               MON_MDS_ECO_SALES_REV_FROM(UNUM,R_ISEAS)
       MON_MDS_CL_CLASS_ENERGY(ns_cla_decs%ASSET_CLS_LOC,3,R_ISEAS) = &
        MON_MDS_CL_CLASS_ENERGY(ns_cla_decs%ASSET_CLS_LOC,3,R_ISEAS) + &
                             ASSET_ALLOCATOR * &
                            MON_MDS_ECO_SALES_ENRG_FROM(UNUM,R_ISEAS)
                  IF(MON_MDS_CL_UNIT_ENERGY(UNUM,R_ISEAS) > 0.) THEN

                     TT_PERCENT = &
                           MON_MDS_ECO_SALES_ENRG_FROM(UNUM,R_ISEAS)/ &
                                   MON_MDS_CL_UNIT_ENERGY(UNUM,R_ISEAS)

     MON_MDS_CL_MULT_FUEL_COST(ns_cla_decs%ASSET_CLS_LOC,TT,R_ISEAS) = &
                        MON_MDS_CL_MULT_FUEL_COST( &
                            ns_cla_decs%ASSET_CLS_LOC,TT,R_ISEAS) + &
                        ASSET_ALLOCATOR * &
                            MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS) * &
                                                             TT_PERCENT
      MON_MDS_CL_MULT_VAR_COST(ns_cla_decs%ASSET_CLS_LOC,TT,R_ISEAS) = &
                        MON_MDS_CL_MULT_VAR_COST( &
                              ns_cla_decs%ASSET_CLS_LOC,TT,R_ISEAS) + &
                       ASSET_ALLOCATOR * &
                             MON_MDS_CL_UNIT_VAR_COST(UNUM,R_ISEAS) * &
                                                             TT_PERCENT
      MON_MDS_CL_MULT_CAPACITY(ns_cla_decs%ASSET_CLS_LOC,TT,R_ISEAS) = &
    MON_MDS_CL_MULT_CAPACITY(ns_cla_decs%ASSET_CLS_LOC,TT,R_ISEAS) + &
                               ASSET_ALLOCATOR * &
                             MON_MDS_CL_UNIT_CAPACITY(UNUM,R_ISEAS) * &
                                                             TT_PERCENT
       MON_MDS_CL_MULT_ENERGY(ns_cla_decs%ASSET_CLS_LOC,TT,R_ISEAS) = &
       MON_MDS_CL_MULT_ENERGY(ns_cla_decs%ASSET_CLS_LOC,TT,R_ISEAS) + &
                           ASSET_ALLOCATOR * &
                           MON_MDS_CL_UNIT_ENERGY(UNUM,R_ISEAS) * &
                                                            TT_PERCENT
                  ENDIF
                  IF(BASE_EL_REVENUE_CASE) THEN
                   BASE_MARKET_ENRG_SALES = BASE_MARKET_ENRG_SALES + &
                             ASSET_ALLOCATOR * &
                             MON_MDS_ECO_SALES_ENRG_FROM(UNUM,R_ISEAS)
                     BASE_MARKET_REVENUES = BASE_MARKET_REVENUES + &
                              ASSET_ALLOCATOR * &
                              MON_MDS_ECO_SALES_REV_FROM(UNUM,R_ISEAS)
                  ELSEIF(PEAK_EL_REVENUE_CASE) THEN
                   PEAK_MARKET_ENRG_SALES = PEAK_MARKET_ENRG_SALES + &
                             ASSET_ALLOCATOR * &
                             MON_MDS_ECO_SALES_ENRG_FROM(UNUM,R_ISEAS)
                     PEAK_MARKET_REVENUES = PEAK_MARKET_REVENUES + &
                              ASSET_ALLOCATOR * &
                             MON_MDS_ECO_SALES_REV_FROM(UNUM,R_ISEAS)
                  ENDIF
               ENDIF ! MON_MDS_ECO_SALES_ENRG_FROM /= 0.

! 091407. FOR CAPACITY_MARKETS

               IF(ABS(MON_CAP_SALE_MW_FROM(UNUM)) >  0.1) THEN
                  IF(CAP_MARKET_COST_ASSIGN(UNUM) == &
                                       'Capacity Sales      ') THEN
                     LOCAL_COLLECTION = &
                       INDEX('ABNX',( &
                              CAP_MARKET_EXP_COLLECT(UNUM)(1:1)))
                     ns_cla_decs%MON_MDS_CL_CAP_REVENUE( &
              ns_cla_decs%ASSET_CLS_LOC,LOCAL_COLLECTION,R_ISEAS) = &
                        ns_cla_decs%MON_MDS_CL_CAP_REVENUE( &
         ns_cla_decs%ASSET_CLS_LOC,LOCAL_COLLECTION,R_ISEAS) + &
                              ASSET_ALLOCATOR * &
                                    MON_CAP_SALE_REV_FROM(UNUM)
                  ENDIF
               ENDIF
               IF(ABS(MON_CAP_PUCH_MW_FROM(UNUM)) >  0.1) THEN
                  LOCAL_COLLECTION = &
                       INDEX('ABNX',( &
                               CAP_MARKET_EXP_COLLECT(UNUM)(1:1)))
                  IF(CAP_MARKET_COST_ASSIGN(UNUM) == &
                                       'Capacity Purchases  ') THEN
                     ns_cla_decs%MON_MDS_CL_CAP_PURCHASES( &
             ns_cla_decs%ASSET_CLS_LOC,LOCAL_COLLECTION,R_ISEAS) = &
                        ns_cla_decs%MON_MDS_CL_CAP_PURCHASES( &
                ns_cla_decs%ASSET_CLS_LOC,LOCAL_COLLECTION,R_ISEAS) - &
                              ASSET_ALLOCATOR * &
                                   MON_CAP_PUCH_COST_FROM(UNUM)
                  ELSEIF(CAP_MARKET_COST_ASSIGN(UNUM) == &
                                       'Purchased Power     ') THEN
                     ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES( &
                ns_cla_decs%ASSET_CLS_LOC,LOCAL_COLLECTION,R_ISEAS) = &
                        ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES( &
                 ns_cla_decs%ASSET_CLS_LOC,LOCAL_COLLECTION,R_ISEAS) - &
                              ASSET_ALLOCATOR * &
                                   MON_CAP_PUCH_COST_FROM(UNUM)
! ELSE NOT COLLECTED
                  ENDIF
               ENDIF

               IF(MON_ECO_PUCH_ENRG_FROM(UNUM) /= 0.) THEN

                  IF(UPPER_TRANS_Gp > 0) THEN
              TG = TRANSACTION_GROUP_ID(UNUM) ! NOTE DOUBLE ASSIGNMENT
          TG = GET_TRANS_GROUP_POSITION(TG)
          IF(PURCHASE_POWER_ASSIGN(TG) /= 'U') THEN
             TG_ASSET_CLASS = PURCHASE_ASSET_CLASS_ID(TG)

             CALL CHECK_IF_CLASS_DEFINED(TG_ASSET_CLASS)
             TG_ASSET_CLASS = TG_ASSET_CLASS + 1
             IF(TG_ASSET_CLASS > 0 .AND. TG_ASSET_CLASS <= &
                         ns_cla_decs%MAX_CAP_LIMITED_CLASS_ID_NUM) THEN
                TG_ASSET_CLASS = &
                       ns_cla_decs%ASSET_CLASS_ptr(TG_ASSET_CLASS)
                ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES(TG_ASSET_CLASS, &
                                    COLLECTION,R_ISEAS) = &
                ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES(TG_ASSET_CLASS, &
                                    COLLECTION,R_ISEAS) + &
                   MON_MDS_ECO_PUCH_COST_FROM(UNUM,R_ISEAS)

                MON_ECO_PUCH_COST_FROM(UNUM) = 0.
             ENDIF
                     ENDIF
                  ENDIF


                  IF(COLLECTION /=1 .OR. PERCENT_RETAIL > .9999 .OR. &
                                                    OLD_ADJ_LOGIC) THEN
                     ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES( &
                    ns_cla_decs%ASSET_CLS_LOC,COLLECTION,R_ISEAS) = &
                           ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES( &
                     ns_cla_decs%ASSET_CLS_LOC,COLLECTION,R_ISEAS) + &
                        ASSET_ALLOCATOR * MON_ECO_PUCH_COST_FROM(UNUM)
                  ELSE
                     ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES( &
                           ns_cla_decs%ASSET_CLS_LOC,2,R_ISEAS) = &
                           ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES( &
                            ns_cla_decs%ASSET_CLS_LOC,2,R_ISEAS) + &
                        ASSET_ALLOCATOR * MON_ECO_PUCH_COST_FROM(UNUM)
                  ENDIF
     MON_MDS_CL_CLASS_ENERGY(ns_cla_decs%ASSET_CLS_LOC,4,R_ISEAS) = &
        MON_MDS_CL_CLASS_ENERGY(ns_cla_decs%ASSET_CLS_LOC,4,R_ISEAS) + &
                         ASSET_ALLOCATOR * MON_ECO_PUCH_ENRG_FROM(UNUM)

                  IF(MON_ECO_PUCH_ENRG_FROM(UNUM) + &
                      MON_MDS_CL_UNIT_ENERGY(UNUM,R_ISEAS) > 0.) THEN

                        TT_PERCENT =  MON_ECO_PUCH_ENRG_FROM(UNUM)/ &
                                      (MON_ECO_PUCH_ENRG_FROM(UNUM) + &
                                          ANNUAL_CL_UNIT_ENERGY(UNUM))

                        MON_MDS_CL_MULT_CAPACITY( &
                              ns_cla_decs%ASSET_CLS_LOC,TT+1,R_ISEAS)= &
                                  MON_MDS_CL_MULT_CAPACITY( &
                             ns_cla_decs%ASSET_CLS_LOC,TT+1,R_ISEAS) &
                                    + ASSET_ALLOCATOR * &
                            MON_MDS_CL_UNIT_CAPACITY(UNUM,R_ISEAS) * &
                                                             TT_PERCENT
                        MON_MDS_CL_MULT_ENERGY( &
                             ns_cla_decs%ASSET_CLS_LOC,TT+1,R_ISEAS) = &
                                MON_MDS_CL_MULT_ENERGY( &
                            ns_cla_decs%ASSET_CLS_LOC,TT+1,R_ISEAS) &
                                       + ASSET_ALLOCATOR * &
                                          MON_ECO_PUCH_ENRG_FROM(UNUM)


                        MON_MDS_CL_MULT_PURCHASES( &
                           ns_cla_decs%ASSET_CLS_LOC,TT+1,R_ISEAS) = &
                              MON_MDS_CL_MULT_PURCHASES( &
                             ns_cla_decs%ASSET_CLS_LOC,TT+1,R_ISEAS) + &
                        ASSET_ALLOCATOR * MON_ECO_PUCH_COST_FROM(UNUM)

                  ENDIF

               ENDIF

               IF(ECITIES) THEN

                  TOTAL_VARIABLE_COST = &
                     ASSET_ALLOCATOR * &
                           (MON_MDS_CL_UNIT_VAR_COST(UNUM,R_ISEAS) + &
                     MON_MDS_CL_UNIT_FUEL_COST(UNUM,R_ISEAS))/1000000.

                  IF(ASSET_ALLOCATOR * &
                      MON_MDS_CL_UNIT_ENERGY(UNUM,R_ISEAS) > .1) THEN
                     AVERAGE_PRODUCTION_COSTS = TOTAL_VARIABLE_COST/ &
                                 (ASSET_ALLOCATOR * &
                                 MON_MDS_CL_UNIT_ENERGY(UNUM,R_ISEAS))
                  ELSE
                     AVERAGE_PRODUCTION_COSTS = 0.
                  ENDIF
                  WHOLESALE_PRODUCTION_COST = &
                        AVERAGE_PRODUCTION_COSTS * &
                                         MON_ECO_SALES_ENRG_FROM(UNUM)
                  MONTHLY_AC_WHOLESALE_PROD_COST( &
                                  ns_cla_decs%ASSET_CLS_LOC,R_ISEAS) = &
                        MONTHLY_AC_WHOLESALE_PROD_COST( &
                                  ns_cla_decs%ASSET_CLS_LOC,R_ISEAS) + &
                                             WHOLESALE_PRODUCTION_COST
         MONTHLY_AC_WHOLESALE_PROD_COST(ns_cla_decs%ASSET_CLS_LOC,0) = &
                        MONTHLY_AC_WHOLESALE_PROD_COST( &
                                        ns_cla_decs%ASSET_CLS_LOC,0) + &
                                            WHOLESALE_PRODUCTION_COST
                  IF( (LDTYPE(UNUM) /= 'NUC' .AND. &
                       SPL_unit_id_loc(UNUM) == 'NONE') .OR. &
                                EXPENSE_ASSIGNMENT(UNUM) == 'P') THEN
                     MONTHLY_AC_ECITY_VAR_PROD_COST( &
                                  ns_cla_decs%ASSET_CLS_LOC,R_ISEAS) = &
                        MONTHLY_AC_ECITY_VAR_PROD_COST( &
                                  ns_cla_decs%ASSET_CLS_LOC,R_ISEAS) + &
                                                   TOTAL_VARIABLE_COST
                     MONTHLY_AC_ECITY_VAR_PROD_COST( &
                                       ns_cla_decs%ASSET_CLS_LOC,0) = &
                        MONTHLY_AC_ECITY_VAR_PROD_COST( &
                                      ns_cla_decs%ASSET_CLS_LOC,0) + &
                                                   TOTAL_VARIABLE_COST
                  ENDIF
                  IF(MON_ECO_PUCH_ENRG_FROM(UNUM) /= 0.) THEN

                     MONTHLY_AC_ECITY_VAR_PROD_COST( &
                                  ns_cla_decs%ASSET_CLS_LOC,R_ISEAS) = &
                        MONTHLY_AC_ECITY_VAR_PROD_COST( &
                                ns_cla_decs%ASSET_CLS_LOC,R_ISEAS) + &
                     ASSET_ALLOCATOR * MON_ECO_PUCH_COST_FROM(UNUM)/ &
                                                              1000000.
                     MONTHLY_AC_ECITY_VAR_PROD_COST( &
                                  ns_cla_decs%ASSET_CLS_LOC,0) = &
                        MONTHLY_AC_ECITY_VAR_PROD_COST( &
                                        ns_cla_decs%ASSET_CLS_LOC,0) + &
                     ASSET_ALLOCATOR * MON_ECO_PUCH_COST_FROM(UNUM)/ &
                                                              1000000.
                  ENDIF
                  IF(ONLINE(UNUM) > FIRST_YEAR_PRICE_MODE) THEN
                     MONTHLY_AC_ECITY_NEW_FIX_COST( &
                                ns_cla_decs%ASSET_CLS_LOC,R_ISEAS) = &
                        MONTHLY_AC_ECITY_NEW_FIX_COST( &
                                 ns_cla_decs%ASSET_CLS_LOC,R_ISEAS) + &
                           ASSET_ALLOCATOR * &
                           MON_MDS_CL_UNIT_FIXED_COST(UNUM,R_ISEAS)/ &
                                                             1000000.
                     MONTHLY_AC_ECITY_NEW_FIX_COST( &
                                  ns_cla_decs%ASSET_CLS_LOC,0) = &
                        MONTHLY_AC_ECITY_NEW_FIX_COST( &
                                    ns_cla_decs%ASSET_CLS_LOC,0) + &
                         ASSET_ALLOCATOR * &
                           MON_MDS_CL_UNIT_FIXED_COST(UNUM,R_ISEAS)/ &
                                                              1000000.
                  ENDIF
               ENDIF


               CLASS_POINTER = CLASS_POINTER + 1
               IF(CLASS_POINTER > AVAIL_DATA_YEARS) EXIT
               IF(ASSET_CLASS_LIST(CLASS_POINTER) == 0. .OR. &
                        ASSET_CLASS_LIST(CLASS_POINTER) == -99.) then
                  EXIT
               endif

            ENDDO ! ASSET CLASSES
         ENDDO ! CL get_nunits()

! 09/02/01. AMEREN TRANSFER PRICING CAPABILITY. TWO PARTY FOR NOW.
!           NEED A SWITCH TO DEFINE.

         IF(DETAILED_TRANSFER_PRICING) THEN
            CALL GET_MONTHLY_TRANSFER_REV_COST( &
                                          R_ISEAS, &
                                          R1_MONTHLY_TRANSFER_REV, &
                                          R1_MONTHLY_TRANSFER_COST, &
                                          R2_MONTHLY_TRANSFER_REV, &
                                          R2_MONTHLY_TRANSFER_COST)
            TG = 1 ! ASSUME UE FOR NOW.
            TG_ASSET_CLASS = PURCHASE_ASSET_CLASS_ID(TG)
            CALL CHECK_IF_CLASS_DEFINED(TG_ASSET_CLASS)
            COLLECTION = 5 ! ASSUME
  TG_ASSET_CLASS = TG_ASSET_CLASS + 1
  IF(TG_ASSET_CLASS > 0 .AND. TG_ASSET_CLASS <= &
                       ns_cla_decs%MAX_CAP_LIMITED_CLASS_ID_NUM) THEN
     TG_ASSET_CLASS = ns_cla_decs%ASSET_CLASS_ptr(TG_ASSET_CLASS)
     ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES( &
                       TG_ASSET_CLASS,COLLECTION,R_ISEAS) = &
                ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES(TG_ASSET_CLASS, &
                                      COLLECTION,R_ISEAS) + &
           R1_MONTHLY_TRANSFER_COST ! FROM TRANSACT MODULE
     ns_cla_decs%MON_MDS_CL_CLASS_REVENUE( &
                    ns_cla_decs%ASSET_CLS_LOC,COLLECTION,R_ISEAS) = &
              ns_cla_decs%MON_MDS_CL_CLASS_REVENUE( &
                    ns_cla_decs%ASSET_CLS_LOC,COLLECTION,R_ISEAS) + &
         R1_MONTHLY_TRANSFER_REV ! FROM TRANSACT MODULE
  ENDIF
  TG = 2 ! ASSUME UE FOR NOW.
  TG_ASSET_CLASS = PURCHASE_ASSET_CLASS_ID(TG)
  CALL CHECK_IF_CLASS_DEFINED(TG_ASSET_CLASS)
  COLLECTION = 5 ! ASSUME
  TG_ASSET_CLASS = TG_ASSET_CLASS + 1
  IF(TG_ASSET_CLASS > 0 .AND. TG_ASSET_CLASS <= &
                         ns_cla_decs%MAX_CAP_LIMITED_CLASS_ID_NUM) THEN
     TG_ASSET_CLASS = ns_cla_decs%ASSET_CLASS_ptr(TG_ASSET_CLASS)
     ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES( &
                    TG_ASSET_CLASS,COLLECTION,R_ISEAS) = &
             ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES(TG_ASSET_CLASS, &
                                   COLLECTION,R_ISEAS) + &
             R2_MONTHLY_TRANSFER_COST ! FROM TRANSACT MODULE
     ns_cla_decs%MON_MDS_CL_CLASS_REVENUE( &
                   ns_cla_decs%ASSET_CLS_LOC,COLLECTION,R_ISEAS) = &
              ns_cla_decs%MON_MDS_CL_CLASS_REVENUE( &
                ns_cla_decs%ASSET_CLS_LOC,COLLECTION,R_ISEAS) + &
                    R2_MONTHLY_TRANSFER_REV ! FROM TRANSACT MODULE
            ENDIF
         ENDIF


! 08/07/01. CHANGE VARIABLE LIST TO WORK WITH MONTHLY MIDAS.
!        GET COST OF SUPPLYING CAPACITY FOR A "SHORT" SYSTEM.

         RUN_TRANSACT = YES_RUN_TRANSACT() ! 10/28/03.

         IF(UPPER_TRANS_Gp > 0 .AND. RUN_TRANSACT) THEN

            DO TG = 1, UPPER_TRANS_Gp
               TEMP_I2 = 0
               CALL GET_ANNUAL_UNSERVED_COST( &
                                          TG, &
                                          TRANS_ANNUAL_UNSERVED_COST, &
                                          TEMP_R4, & !  UNSERVED MWH'S
                                       R_ISEAS) ! ANNUAL CALL 6/25/01.

                  IF(PURCHASE_POWER_ASSIGN(TG) == 'A') THEN
                     COLLECTION = 1
                  ELSEIF(PURCHASE_POWER_ASSIGN(TG) == 'B') THEN
                     COLLECTION = 2
                  ELSEIF(PURCHASE_POWER_ASSIGN(TG) == 'C') THEN
                     COLLECTION = 4
                  ELSE
                     COLLECTION = 3
                  ENDIF
                  TG_ASSET_CLASS = PURCHASE_ASSET_CLASS_ID(TG)

       CALL CHECK_IF_CLASS_DEFINED(TG_ASSET_CLASS)
       TG_ASSET_CLASS = TG_ASSET_CLASS + 1
       IF(TG_ASSET_CLASS > 0 .AND. TG_ASSET_CLASS <= &
                        ns_cla_decs%MAX_CAP_LIMITED_CLASS_ID_NUM) THEN
          TG_ASSET_CLASS = &
                        ns_cla_decs%ASSET_CLASS_ptr(TG_ASSET_CLASS)
          ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES( &
                      TG_ASSET_CLASS,COLLECTION,R_ISEAS) = &
                ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES(TG_ASSET_CLASS, &
                                     COLLECTION,R_ISEAS) + &
  TRANS_ANNUAL_UNSERVED_COST ! FROM TRANSACT MODULE
          IF(WABASH_VALLEY .AND. TG == 1) THEN
             CALL CL_REPORT_WHOLE_MARKET_COST( &
                                     R_ISEAS, &
                      WHOLESALE_MARKET_COST)

         ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES( &
            TG_ASSET_CLASS,COLLECTION,R_ISEAS) = &
      ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES(TG_ASSET_CLASS, &
                             COLLECTION,R_ISEAS) + &
            WHOLESALE_MARKET_COST ! FROM TRANSACT MODULE
         COLLECTION = 2
         ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES( &
            TG_ASSET_CLASS,COLLECTION,R_ISEAS) = &
          ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES(TG_ASSET_CLASS, &
                             COLLECTION,R_ISEAS) - &
          WHOLESALE_MARKET_COST ! FROM TRANSACT MODULE
      ENDIF
                  ENDIF

            ENDDO
         ENDIF
         DEALLOCATE(ASSET_CLASS_LIST,ASSET_ALLOCATION_LIST)

! MARKET REVENUE PROXY FOR UNION EL get_nunits() 8/7/97

         EL_BASE_RATE = 0.
         EL_PEAK_RATE = 0.
         IF(BASE_MARKET_ENRG_SALES > 50 .AND. &
                                 BASE_MARKET_REVENUES > 100) THEN
            EL_BASE_RATE = BASE_MARKET_REVENUES/BASE_MARKET_ENRG_SALES
         ENDIF
         IF(PEAK_MARKET_ENRG_SALES > 25 .AND. &
                                      PEAK_MARKET_REVENUES > 50) THEN
            EL_PEAK_RATE = PEAK_MARKET_REVENUES/PEAK_MARKET_ENRG_SALES
         ENDIF

         DO COLLECTION = 0, 3
            DO CLASS = 1, MAX_INTRA_CLASS
               MON_MDS_INTRA_COMPANY_REVENUES(0,COLLECTION,R_ISEAS) = &
                        MON_MDS_INTRA_COMPANY_REVENUES( &
                                        0,COLLECTION,R_ISEAS) + &
                              MON_MDS_INTRA_COMPANY_REVENUES( &
                                           CLASS,COLLECTION,R_ISEAS)
            ENDDO
         ENDDO
         DO CLASS = 1, MAX_INTRA_CLASS
            MON_MDS_INTRA_COMPANY_NF_BURN(0,R_ISEAS) = &
                     MON_MDS_INTRA_COMPANY_NF_BURN(0,R_ISEAS) + &
                       MON_MDS_INTRA_COMPANY_NF_BURN(CLASS,R_ISEAS)
         ENDDO

         DO CLASS = 1, NUMBER_OF_CAP_LIMITED_CLASSES
! get_nunits()
            MON_MDS_NUC_FUEL_LEASE_BURN_BC(CLASS,R_ISEAS) = &
               MON_MDS_NUC_FUEL_LEASE_BURN_BC(CLASS,R_ISEAS)/1000000.
            MON_MDS_NUC_FUEL_OWNED_BURN_BC(CLASS,R_ISEAS) = &
               MON_MDS_NUC_FUEL_OWNED_BURN_BC(CLASS,R_ISEAS)/1000000.
! TOTAL ACROSS CLASSES
            MON_MDS_NUC_FUEL_LEASE_BURN_BC(0,R_ISEAS) = &
                     MON_MDS_NUC_FUEL_LEASE_BURN_BC(0,R_ISEAS) + &
           MON_MDS_NUC_FUEL_LEASE_BURN_BC(CLASS,R_ISEAS)
  MON_MDS_NUC_FUEL_OWNED_BURN_BC(0,R_ISEAS) = &
          MON_MDS_NUC_FUEL_OWNED_BURN_BC(0,R_ISEAS) + &
         MON_MDS_NUC_FUEL_OWNED_BURN_BC(CLASS,R_ISEAS)
  MON_MDS_NUCLEAR_MWH_BY_CLASS(0,R_ISEAS) = &
     MON_MDS_NUCLEAR_MWH_BY_CLASS(0,R_ISEAS) + &
           MON_MDS_NUCLEAR_MWH_BY_CLASS(CLASS,R_ISEAS)
  MON_MDS_NUCLEAR_MMBTU_BY_CLASS(0,R_ISEAS) = &
        MON_MDS_NUCLEAR_MMBTU_BY_CLASS(0,R_ISEAS) + &
          MON_MDS_NUCLEAR_MMBTU_BY_CLASS(CLASS,R_ISEAS)
  ns_cla_decs%MON_MDS_ICAP_REV_BY_CLASS(0,R_ISEAS) = &
           ns_cla_decs%MON_MDS_ICAP_REV_BY_CLASS(0,R_ISEAS)/1000000.
! TOTAL ACROSS MONTHS
            MON_MDS_NUC_FUEL_LEASE_BURN_BC(CLASS,0) = &
                     MON_MDS_NUC_FUEL_LEASE_BURN_BC(CLASS,0) + &
                   MON_MDS_NUC_FUEL_LEASE_BURN_BC(CLASS,R_ISEAS)
            MON_MDS_NUC_FUEL_OWNED_BURN_BC(CLASS,0) = &
                    MON_MDS_NUC_FUEL_OWNED_BURN_BC(CLASS,0) + &
                      MON_MDS_NUC_FUEL_OWNED_BURN_BC(CLASS,R_ISEAS)
 MON_MDS_NUCLEAR_MWH_BY_CLASS(CLASS,0) = &
    MON_MDS_NUCLEAR_MWH_BY_CLASS(CLASS,0) + &
                MON_MDS_NUCLEAR_MWH_BY_CLASS(CLASS,R_ISEAS)
 MON_MDS_NUCLEAR_MMBTU_BY_CLASS(CLASS,0) = &
       MON_MDS_NUCLEAR_MMBTU_BY_CLASS(CLASS,0) + &
              MON_MDS_NUCLEAR_MMBTU_BY_CLASS(CLASS,R_ISEAS)
 ns_cla_decs%MON_MDS_ICAP_REV_BY_CLASS(CLASS,R_ISEAS) = &
             ns_cla_decs%MON_MDS_ICAP_REV_BY_CLASS(CLASS,R_ISEAS) + &
                  ns_cla_decs%MON_MDS_ICAP_REV_BY_CLASS(CLASS,R_ISEAS)
! TOTAL ACROSS CLASSES AND MONTHS
            MON_MDS_NUC_FUEL_LEASE_BURN_BC(0,0) = &
                     MON_MDS_NUC_FUEL_LEASE_BURN_BC(0,0) + &
                         MON_MDS_NUC_FUEL_LEASE_BURN_BC(CLASS,R_ISEAS)
            MON_MDS_NUC_FUEL_OWNED_BURN_BC(0,0) = &
                    MON_MDS_NUC_FUEL_OWNED_BURN_BC(0,0) + &
                         MON_MDS_NUC_FUEL_OWNED_BURN_BC(CLASS,R_ISEAS)
            MON_MDS_NUCLEAR_MWH_BY_CLASS(0,0) = &
               MON_MDS_NUCLEAR_MWH_BY_CLASS(0,0) + &
                           MON_MDS_NUCLEAR_MWH_BY_CLASS(CLASS,R_ISEAS)
            MON_MDS_NUCLEAR_MMBTU_BY_CLASS(0,0) = &
                  MON_MDS_NUCLEAR_MMBTU_BY_CLASS(0,0) + &
                         MON_MDS_NUCLEAR_MMBTU_BY_CLASS(CLASS,R_ISEAS)
            ns_cla_decs%MON_MDS_ICAP_REV_BY_CLASS(0,0) = &
                        ns_cla_decs%MON_MDS_ICAP_REV_BY_CLASS(0,0) + &
                  ns_cla_decs%MON_MDS_ICAP_REV_BY_CLASS(CLASS,R_ISEAS)


            DO EMISS_TYPE = 1, NUMBER_OF_EMISSION_TYPES
! TOTAL ACROSS CLASSES
               MON_MDS_CL_CLASS_EMISSIONS(EMISS_TYPE,0,R_ISEAS) = &
                   MON_MDS_CL_CLASS_EMISSIONS(EMISS_TYPE,0,R_ISEAS) + &
                  MON_MDS_CL_CLASS_EMISSIONS(EMISS_TYPE,CLASS,R_ISEAS)
! TOTAL ACROSS MONTHS
               MON_MDS_CL_CLASS_EMISSIONS(EMISS_TYPE,CLASS,0) = &
                   MON_MDS_CL_CLASS_EMISSIONS(EMISS_TYPE,CLASS,0) + &
                  MON_MDS_CL_CLASS_EMISSIONS(EMISS_TYPE,CLASS,R_ISEAS)
! TOTAL ACROSS CLASSES AND MONTHS
               MON_MDS_CL_CLASS_EMISSIONS(EMISS_TYPE,0,0) = &
                   MON_MDS_CL_CLASS_EMISSIONS(EMISS_TYPE,0,0) + &
                   MON_MDS_CL_CLASS_EMISSIONS(EMISS_TYPE,CLASS,R_ISEAS)
            ENDDO

            DO I = 1, 4
! get_nunits()
       MON_MDS_CL_CLASS_VAR_COST(CLASS,I,R_ISEAS) = &
           MON_MDS_CL_CLASS_VAR_COST(CLASS,I,R_ISEAS)/1000000.
       ns_cla_decs%MON_MDS_CL_CLASS_FUEL_COST(CLASS,I,R_ISEAS)  = &
       ns_cla_decs%MON_MDS_CL_CLASS_FUEL_COST(CLASS,I,R_ISEAS)/1000000.
     ns_cla_decs%MON_MDS_NF_FUEL_LEASED_BC(CLASS,I,R_ISEAS) = &
 ns_cla_decs%MON_MDS_NF_FUEL_LEASED_BC(CLASS,I,R_ISEAS)/1000000.
       ns_cla_decs%MON_MDS_NF_FUEL_OWNED_BC(CLASS,I,R_ISEAS) = &
          ns_cla_decs%MON_MDS_NF_FUEL_OWNED_BC(CLASS,I,R_ISEAS)/1000000.
       MON_MDS_CL_CLASS_FIXED_COST(CLASS,I,R_ISEAS) = &
          MON_MDS_CL_CLASS_FIXED_COST(CLASS,I,R_ISEAS)/1000000.
       ns_cla_decs%MON_MDS_CL_CLASS_REVENUE(CLASS,I,R_ISEAS) = &
          ns_cla_decs%MON_MDS_CL_CLASS_REVENUE(CLASS,I,R_ISEAS)/1000000.
       ns_cla_decs%MON_MDS_CL_CAP_REVENUE(CLASS,I,R_ISEAS) = &
            ns_cla_decs%MON_MDS_CL_CAP_REVENUE(CLASS,I,R_ISEAS)/1000000.
       MON_MDS_CL_CLASS_CAPACITY(CLASS,I,R_ISEAS) = &
            MON_MDS_CL_CLASS_CAPACITY(CLASS,I,R_ISEAS)/1000000.
       MON_MDS_CL_CLASS_ENERGY(CLASS,I,R_ISEAS) = &
              MON_MDS_CL_CLASS_ENERGY(CLASS,I,R_ISEAS)/1000000.
       ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES(CLASS,I,R_ISEAS) = &
       ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES(CLASS,I,R_ISEAS)/1000000.
       ns_cla_decs%MON_MDS_CL_CAP_PURCHASES(CLASS,I,R_ISEAS) = &
          ns_cla_decs%MON_MDS_CL_CAP_PURCHASES(CLASS,I,R_ISEAS)/1000000.
! TOTAL ACROSS CLASSES
               MON_MDS_CL_CLASS_VAR_COST(0,I,R_ISEAS) = &
                     MON_MDS_CL_CLASS_VAR_COST(0,I,R_ISEAS) + &
                             MON_MDS_CL_CLASS_VAR_COST(CLASS,I,R_ISEAS)
               ns_cla_decs%MON_MDS_CL_CLASS_FUEL_COST(0,I,R_ISEAS) = &
                ns_cla_decs%MON_MDS_CL_CLASS_FUEL_COST(0,I,R_ISEAS) + &
              ns_cla_decs%MON_MDS_CL_CLASS_FUEL_COST(CLASS,I,R_ISEAS)
               ns_cla_decs%MON_MDS_NF_FUEL_LEASED_BC(0,I,R_ISEAS) = &
                  ns_cla_decs%MON_MDS_NF_FUEL_LEASED_BC(0,I,R_ISEAS) + &
                  ns_cla_decs%MON_MDS_NF_FUEL_LEASED_BC(CLASS,I,R_ISEAS)
               ns_cla_decs%MON_MDS_NF_FUEL_OWNED_BC(0,I,R_ISEAS) = &
                   ns_cla_decs%MON_MDS_NF_FUEL_OWNED_BC(0,I,R_ISEAS) + &
                  ns_cla_decs%MON_MDS_NF_FUEL_OWNED_BC(CLASS,I,R_ISEAS)
               MON_MDS_CL_CLASS_FIXED_COST(0,I,R_ISEAS) = &
                     MON_MDS_CL_CLASS_FIXED_COST(0,I,R_ISEAS) + &
                           MON_MDS_CL_CLASS_FIXED_COST(CLASS,I,R_ISEAS)
               ns_cla_decs%MON_MDS_CL_CLASS_REVENUE(0,I,R_ISEAS) = &
              ns_cla_decs%MON_MDS_CL_CLASS_REVENUE(0,I,R_ISEAS) + &
              ns_cla_decs%MON_MDS_CL_CLASS_REVENUE(CLASS,I,R_ISEAS)
               ns_cla_decs%MON_MDS_CL_CAP_REVENUE(0,I,R_ISEAS) = &
                     ns_cla_decs%MON_MDS_CL_CAP_REVENUE(0,I,R_ISEAS) + &
                 ns_cla_decs%MON_MDS_CL_CAP_REVENUE(CLASS,I,R_ISEAS)
               MON_MDS_CL_CLASS_CAPACITY(0,I,R_ISEAS) = &
                        MON_MDS_CL_CLASS_CAPACITY(0,I,R_ISEAS) + &
                             MON_MDS_CL_CLASS_CAPACITY(CLASS,I,R_ISEAS)
               MON_MDS_CL_CLASS_ENERGY(0,I,R_ISEAS) = &
                        MON_MDS_CL_CLASS_ENERGY(0,I,R_ISEAS) + &
                               MON_MDS_CL_CLASS_ENERGY(CLASS,I,R_ISEAS)
               ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES(0,I,R_ISEAS) = &
                ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES(0,I,R_ISEAS) + &
               ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES(CLASS,I,R_ISEAS)
               ns_cla_decs%MON_MDS_CL_CAP_PURCHASES(0,I,R_ISEAS) = &
                  ns_cla_decs%MON_MDS_CL_CAP_PURCHASES(0,I,R_ISEAS) + &
                 ns_cla_decs%MON_MDS_CL_CAP_PURCHASES(CLASS,I,R_ISEAS)
! TOTAL ACROSS MONTHS
               MON_MDS_CL_CLASS_VAR_COST(CLASS,I,0) = &
                     MON_MDS_CL_CLASS_VAR_COST(CLASS,I,0) + &
                             MON_MDS_CL_CLASS_VAR_COST(CLASS,I,R_ISEAS)
               ns_cla_decs%MON_MDS_CL_CLASS_FUEL_COST(CLASS,I,0) = &
              ns_cla_decs%MON_MDS_CL_CLASS_FUEL_COST(CLASS,I,0) + &
              ns_cla_decs%MON_MDS_CL_CLASS_FUEL_COST(CLASS,I,R_ISEAS)
               ns_cla_decs%MON_MDS_NF_FUEL_LEASED_BC(CLASS,I,0) = &
                   ns_cla_decs%MON_MDS_NF_FUEL_LEASED_BC(CLASS,I,0) + &
                ns_cla_decs%MON_MDS_NF_FUEL_LEASED_BC(CLASS,I,R_ISEAS)
               ns_cla_decs%MON_MDS_NF_FUEL_OWNED_BC(CLASS,I,0) = &
               ns_cla_decs%MON_MDS_NF_FUEL_OWNED_BC(CLASS,I,0) + &
            ns_cla_decs%MON_MDS_NF_FUEL_OWNED_BC(CLASS,I,R_ISEAS)
               MON_MDS_CL_CLASS_FIXED_COST(CLASS,I,0) = &
                     MON_MDS_CL_CLASS_FIXED_COST(CLASS,I,0) + &
                           MON_MDS_CL_CLASS_FIXED_COST(CLASS,I,R_ISEAS)
               ns_cla_decs%MON_MDS_CL_CLASS_REVENUE(CLASS,I,0) = &
                     ns_cla_decs%MON_MDS_CL_CLASS_REVENUE(CLASS,I,0) + &
              ns_cla_decs%MON_MDS_CL_CLASS_REVENUE(CLASS,I,R_ISEAS)
               ns_cla_decs%MON_MDS_CL_CAP_REVENUE(CLASS,I,0) = &
                     ns_cla_decs%MON_MDS_CL_CAP_REVENUE(CLASS,I,0) + &
                ns_cla_decs%MON_MDS_CL_CAP_REVENUE(CLASS,I,R_ISEAS)
               MON_MDS_CL_CLASS_CAPACITY(CLASS,I,0) = &
                        MON_MDS_CL_CLASS_CAPACITY(CLASS,I,0) + &
                             MON_MDS_CL_CLASS_CAPACITY(CLASS,I,R_ISEAS)
               MON_MDS_CL_CLASS_ENERGY(CLASS,I,0) = &
                        MON_MDS_CL_CLASS_ENERGY(CLASS,I,0) + &
                               MON_MDS_CL_CLASS_ENERGY(CLASS,I,R_ISEAS)
               ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES(CLASS,I,0) = &
             ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES(CLASS,I,0) + &
             ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES(CLASS,I,R_ISEAS)
               ns_cla_decs%MON_MDS_CL_CAP_PURCHASES(CLASS,I,0) = &
                    ns_cla_decs%MON_MDS_CL_CAP_PURCHASES(CLASS,I,0) + &
                  ns_cla_decs%MON_MDS_CL_CAP_PURCHASES(CLASS,I,R_ISEAS)
! TOTAL ACROSS CLASSES AND MONTHS
  MON_MDS_CL_CLASS_VAR_COST(0,I,0) = &
        MON_MDS_CL_CLASS_VAR_COST(0,I,0) + &
                MON_MDS_CL_CLASS_VAR_COST(CLASS,I,R_ISEAS)
  ns_cla_decs%MON_MDS_CL_CLASS_FUEL_COST(0,I,0) = &
            ns_cla_decs%MON_MDS_CL_CLASS_FUEL_COST(0,I,0) + &
               ns_cla_decs%MON_MDS_CL_CLASS_FUEL_COST(CLASS,I,R_ISEAS)
  ns_cla_decs%MON_MDS_NF_FUEL_LEASED_BC(0,I,0) = &
            ns_cla_decs%MON_MDS_NF_FUEL_LEASED_BC(0,I,0) + &
                ns_cla_decs%MON_MDS_NF_FUEL_LEASED_BC(CLASS,I,R_ISEAS)
  ns_cla_decs%MON_MDS_NF_FUEL_OWNED_BC(0,I,0) = &
           ns_cla_decs%MON_MDS_NF_FUEL_OWNED_BC(0,I,0) + &
                 ns_cla_decs%MON_MDS_NF_FUEL_OWNED_BC(CLASS,I,R_ISEAS)
  MON_MDS_CL_CLASS_FIXED_COST(0,I,0) = &
        MON_MDS_CL_CLASS_FIXED_COST(0,I,0) + &
              MON_MDS_CL_CLASS_FIXED_COST(CLASS,I,R_ISEAS)
  ns_cla_decs%MON_MDS_CL_CLASS_REVENUE(0,I,0) = &
        ns_cla_decs%MON_MDS_CL_CLASS_REVENUE(0,I,0) + &
                 ns_cla_decs%MON_MDS_CL_CLASS_REVENUE(CLASS,I,R_ISEAS)
  ns_cla_decs%MON_MDS_CL_CAP_REVENUE(0,I,0) = &
        ns_cla_decs%MON_MDS_CL_CAP_REVENUE(0,I,0) + &
                   ns_cla_decs%MON_MDS_CL_CAP_REVENUE(CLASS,I,R_ISEAS)
               MON_MDS_CL_CLASS_CAPACITY(0,I,0) = &
                        MON_MDS_CL_CLASS_CAPACITY(0,I,0) + &
                             MON_MDS_CL_CLASS_CAPACITY(CLASS,I,R_ISEAS)
               MON_MDS_CL_CLASS_ENERGY(0,I,0) = &
                        MON_MDS_CL_CLASS_ENERGY(0,I,0) + &
                               MON_MDS_CL_CLASS_ENERGY(CLASS,I,R_ISEAS)
               ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES(0,I,0) = &
         ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES(0,I,0) + &
            ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES(CLASS,I,R_ISEAS)
               ns_cla_decs%MON_MDS_CL_CAP_PURCHASES(0,I,0) = &
                        ns_cla_decs%MON_MDS_CL_CAP_PURCHASES(0,I,0) + &
                ns_cla_decs%MON_MDS_CL_CAP_PURCHASES(CLASS,I,R_ISEAS)
            ENDDO
            DO I = 1, NUM_FUEL_CATEGORIES
! get_nunits()
               MON_MDS_CL_MULT_FUEL_COST(CLASS,I,R_ISEAS)  = &
                    MON_MDS_CL_MULT_FUEL_COST(CLASS,I,R_ISEAS)/1000000.

               MON_MDS_CL_MULT_VAR_COST(CLASS,I,R_ISEAS) = &
                     MON_MDS_CL_MULT_VAR_COST(CLASS,I,R_ISEAS)/1000000.
               MON_MDS_CL_MULT_FIXED_COST(CLASS,I,R_ISEAS) = &
                   MON_MDS_CL_MULT_FIXED_COST(CLASS,I,R_ISEAS)/1000000.
               MON_MDS_CL_MULT_CAPACITY(CLASS,I,R_ISEAS) = &
                    MON_MDS_CL_MULT_CAPACITY(CLASS,I,R_ISEAS)/1000000.
               MON_MDS_CL_MULT_ENERGY(CLASS,I,R_ISEAS) = &
                    MON_MDS_CL_MULT_ENERGY(CLASS,I,R_ISEAS)/1000000.
               MON_MDS_CL_MULT_PURCHASES(CLASS,I,R_ISEAS) = &
                    MON_MDS_CL_MULT_PURCHASES(CLASS,I,R_ISEAS)/1000000.

! CLASSES

               MON_MDS_CL_MULT_FUEL_COST(0,I,R_ISEAS) = &
                     MON_MDS_CL_MULT_FUEL_COST(0,I,R_ISEAS) + &
                             MON_MDS_CL_MULT_FUEL_COST(CLASS,I,R_ISEAS)
               MON_MDS_CL_MULT_VAR_COST(0,I,R_ISEAS) = &
                     MON_MDS_CL_MULT_VAR_COST(0,I,R_ISEAS) + &
                              MON_MDS_CL_MULT_VAR_COST(CLASS,I,R_ISEAS)
               MON_MDS_CL_MULT_FIXED_COST(0,I,R_ISEAS) = &
                     MON_MDS_CL_MULT_FIXED_COST(0,I,R_ISEAS) + &
                            MON_MDS_CL_MULT_FIXED_COST(CLASS,I,R_ISEAS)
               MON_MDS_CL_MULT_CAPACITY(0,I,R_ISEAS) = &
                     MON_MDS_CL_MULT_CAPACITY(0,I,R_ISEAS) + &
                              MON_MDS_CL_MULT_CAPACITY(CLASS,I,R_ISEAS)
               MON_MDS_CL_MULT_ENERGY(0,I,R_ISEAS) = &
                     MON_MDS_CL_MULT_ENERGY(0,I,R_ISEAS) + &
                                MON_MDS_CL_MULT_ENERGY(CLASS,I,R_ISEAS)
               MON_MDS_CL_MULT_PURCHASES(0,I,R_ISEAS) = &
                     MON_MDS_CL_MULT_PURCHASES(0,I,R_ISEAS) + &
                             MON_MDS_CL_MULT_PURCHASES(CLASS,I,R_ISEAS)

               MON_MDS_CL_MULT_FUEL_COST(CLASS,I,0) = &
                     MON_MDS_CL_MULT_FUEL_COST(CLASS,I,0) + &
                             MON_MDS_CL_MULT_FUEL_COST(CLASS,I,R_ISEAS)
               MON_MDS_CL_MULT_VAR_COST(CLASS,I,0) = &
                     MON_MDS_CL_MULT_VAR_COST(CLASS,I,0) + &
                              MON_MDS_CL_MULT_VAR_COST(CLASS,I,R_ISEAS)
               MON_MDS_CL_MULT_FIXED_COST(CLASS,I,0) = &
                     MON_MDS_CL_MULT_FIXED_COST(CLASS,I,0) + &
                            MON_MDS_CL_MULT_FIXED_COST(CLASS,I,R_ISEAS)
               MON_MDS_CL_MULT_CAPACITY(CLASS,I,0) = &
                     MON_MDS_CL_MULT_CAPACITY(CLASS,I,0) + &
                             MON_MDS_CL_MULT_CAPACITY(CLASS,I,R_ISEAS)
               MON_MDS_CL_MULT_ENERGY(CLASS,I,0) = &
                     MON_MDS_CL_MULT_ENERGY(CLASS,I,0) + &
                             MON_MDS_CL_MULT_ENERGY(CLASS,I,R_ISEAS)
               MON_MDS_CL_MULT_PURCHASES(CLASS,I,0) = &
                     MON_MDS_CL_MULT_PURCHASES(CLASS,I,0) + &
                             MON_MDS_CL_MULT_PURCHASES(CLASS,I,R_ISEAS)

! CLASSES AND MONTHS

               MON_MDS_CL_MULT_FUEL_COST(0,I,0) = &
                     MON_MDS_CL_MULT_FUEL_COST(0,I,0) + &
                             MON_MDS_CL_MULT_FUEL_COST(CLASS,I,R_ISEAS)
               MON_MDS_CL_MULT_VAR_COST(0,I,0) = &
                     MON_MDS_CL_MULT_VAR_COST(0,I,0) + &
                              MON_MDS_CL_MULT_VAR_COST(CLASS,I,R_ISEAS)
               MON_MDS_CL_MULT_FIXED_COST(0,I,0) = &
                     MON_MDS_CL_MULT_FIXED_COST(0,I,0) + &
                            MON_MDS_CL_MULT_FIXED_COST(CLASS,I,R_ISEAS)
               MON_MDS_CL_MULT_CAPACITY(0,I,0) = &
                     MON_MDS_CL_MULT_CAPACITY(0,I,0) + &
                             MON_MDS_CL_MULT_CAPACITY(CLASS,I,R_ISEAS)
               MON_MDS_CL_MULT_ENERGY(0,I,0) = &
                     MON_MDS_CL_MULT_ENERGY(0,I,0) + &
                             MON_MDS_CL_MULT_ENERGY(CLASS,I,R_ISEAS)
               MON_MDS_CL_MULT_PURCHASES(0,I,0) = &
                     MON_MDS_CL_MULT_PURCHASES(0,I,0) + &
                             MON_MDS_CL_MULT_PURCHASES(CLASS,I,R_ISEAS)
            ENDDO
         ENDDO
      RETURN

      ENTRY RETURN_ECITIES_OBJ_VARS(R_CLASS, &
                  R_ECITIES_WHOLESALE_PROD_COST,  & !  684  C-PROD
                  R_ECITIES_VAR_PROD_COST,        & !  685  C-SUPP
                  R_ECITIES_NEW_FIXED_COST,       & !  686  C-CAP
                  R_ECITIES_MARKET_ENERGY_SALES,  & !  687  R
                  R_ECITIES_TRANSMISSION_FEES)     ! 688  C-FEES

         IF(R_CLASS <= ns_cla_decs%MAX_CAP_LIMITED_CLASS_ID_NUM &
            .AND. ECITIES) THEN
            IF(R_CLASS ==0) THEN
               CLASS = 0
            ELSE
               CLASS = ns_cla_decs%ASSET_CLASS_ptr(R_CLASS)
            ENDIF
            IF(CLASS > 0 .OR. R_CLASS == 0) THEN
               THIS_YEAR = globecom_year + get_BASE_YEAR()
               DO MO = 0, 12

                  TEMP_L = &
                        POWER_DERIV_REV_EXP_BY_CLASS( R_CLASS, &
                                                   MO, &
                                                   TOTAL_DERIV_REV, &
                                                   TOTAL_DERIV_EXP)

      R_MONTH_AC_WHOLESALE_PROD_COST(MO) = &
             MONTHLY_AC_WHOLESALE_PROD_COST(CLASS,MO)
      R_MONTH_AC_ECITY_VAR_PROD_COST(MO) = &
             MONTHLY_AC_ECITY_VAR_PROD_COST(CLASS,MO)
      R_MONTH_AC_ECITY_NEW_FIX_COST(MO) = &
              MONTHLY_AC_ECITY_NEW_FIX_COST(CLASS,MO)
      R_MONTH_AC_ECITY_SUPP(MO) = &
            MONTHLY_AC_ECITY_VAR_PROD_COST(CLASS,MO)
      R_MONTH_AC_ECITY_REVENUE(MO) = &
            ns_cla_decs%MON_MDS_CL_CLASS_REVENUE(CLASS,1,MO) &
            + ns_cla_decs%MON_MDS_CL_CLASS_REVENUE(CLASS,2,MO) &
            + ns_cla_decs%MON_MDS_CL_CLASS_REVENUE(CLASS,3,MO) &
            + ns_cla_decs%MON_MDS_CL_CLASS_REVENUE(CLASS,4,MO) &
            + TOTAL_DERIV_REV
      R_MONTH_AC_ECITY_FEES(MO) = TOTAL_DERIV_EXP

!  CHECK get_nunits(), KATHY TO PASS EXAMPLE

                  R_WTB(MO) =  R_MONTH_AC_ECITY_REVENUE(MO)  & !  DONE.
                       - R_MONTH_AC_WHOLESALE_PROD_COST(MO)  & !  DONE.
                               - R_MONTH_AC_ECITY_SUPP(MO)  & !  DONE.
                               - R_MONTH_AC_ECITY_FEES(MO)  & !  DONE.
                            - R_MONTH_AC_ECITY_NEW_FIX_COST(MO) ! DONE.

                  ECITY_OBJ_REPORT = .TRUE. ! HARD-WIRED FOR NOW.
                  IF(ECITY_OBJ_REPORT) THEN

                     IF(ECITY_OBJ_REPORT_NOT_OPEN) THEN
                        ECITY_OBJ_REPORT_NOT_OPEN = .FALSE.
                        ECITY_VARIABLE_NUMBER = 8
                        ECITY_OBJ_UNIT = &
                           ECITY_OBJ_HEADER(ECITY_VARIABLE_NUMBER, &
                                                         ECITY_OBJ_REC)
                     ENDIF

                     IF(MO > 0) THEN
                        LOCAL_MONTH = MO
                     ELSE
                        LOCAL_MONTH = 13
                     ENDIF

                     WRITE(ECITY_OBJ_UNIT,REC=ECITY_OBJ_REC) &
                              PRT_ENDPOINT(), &
                              FLOAT(THIS_YEAR), &
                              FLOAT(R_CLASS-1), &
                              CL_MONTH_NAME(LOCAL_MONTH), &
                              R_WTB(MO), &
                              R_MONTH_AC_ECITY_REVENUE(MO), &
                              R_MONTH_AC_WHOLESALE_PROD_COST(MO), &
                              R_MONTH_AC_ECITY_SUPP(MO), &
                              R_MONTH_AC_ECITY_FEES(MO), &
                              R_MONTH_AC_ECITY_NEW_FIX_COST(MO), &
                              TOTAL_DERIV_REV, &
                              TOTAL_DERIV_EXP
                     ECITY_OBJ_REC = ECITY_OBJ_REC + 1

                  ENDIF
               ENDDO

            ELSE ! ASSET CLASSES NOT ACTIVE

               DO MO = 0, 12
                  R_WTB(MO) = 0.
                  R_MONTH_AC_ECITY_REVENUE(MO) = 0.  ! DONE.
                  R_MONTH_AC_WHOLESALE_PROD_COST(MO) = 0.  ! DONE.
                  R_MONTH_AC_ECITY_SUPP(MO) = 0.  ! DONE.
                  R_MONTH_AC_ECITY_FEES(MO) = 0.  ! DONE.
                  R_MONTH_AC_ECITY_NEW_FIX_COST(MO) = 0. ! DONE.
               ENDDO

            ENDIF
            RETURN_ECITIES_OBJ_VARS = R_WTB(0)
            R_ECITIES_WHOLESALE_PROD_COST = &
              R_MONTH_AC_WHOLESALE_PROD_COST(0)  ! 684  C-PROD
R_ECITIES_VAR_PROD_COST = R_MONTH_AC_ECITY_SUPP(0)        ! 685  C-SUPP
            ! 686  C-CAP
            R_ECITIES_NEW_FIXED_COST = R_MONTH_AC_ECITY_NEW_FIX_COST(0)
  R_ECITIES_MARKET_ENERGY_SALES = R_MONTH_AC_ECITY_REVENUE(0)  ! 687  R
            R_ECITIES_TRANSMISSION_FEES = &
                R_MONTH_AC_ECITY_FEES(0)     ! 688  C-FEES
         ELSE
            RETURN_ECITIES_OBJ_VARS = 0.
            R_ECITIES_WHOLESALE_PROD_COST = 0.
            R_ECITIES_VAR_PROD_COST = 0.
            R_ECITIES_NEW_FIXED_COST = 0.
            R_ECITIES_MARKET_ENERGY_SALES = 0.
            R_ECITIES_TRANSMISSION_FEES = 0.
         ENDIF
      RETURN

      ENTRY CL_EXPENSES_2_ASSET_CLASSES()


         ALLOCATE(ASSET_CLASS_LIST(AVAIL_DATA_YEARS))
         ALLOCATE(ASSET_ALLOCATION_LIST(AVAIL_DATA_YEARS))
         CURRENT_YEAR = globecom_year+get_BASE_YEAR()
         CURRENT_YEAR_COMPARISON = (CURRENT_YEAR-1900)*100
         CL_ANN_CLASS_VAR_COST = 0.
         CL_ANN_CLASS_FUEL_COST = 0.
         NF_FUEL_LEASED_BY_CLASS = 0.
         NF_FUEL_OWNED_BY_CLASS = 0.
         CL_ANN_CLASS_FIXED_COST = 0.
         CL_ANN_CLASS_CAPACITY = 0.
         CL_ANN_CLASS_ENERGY = 0.
! SLOT 5 RESERVED FOR AMEREN 12/11/01
         CL_ANN_CLASS_PURCHASES = 0.
         CL_ANN_CLASS_REVENUE = 0.
         CL_ANN_CLASS_EMISSIONS = 0.
         INTRA_COMPANY_REVENUES = 0.
         INTRA_COMPANY_NF_BURN = 0.
         NUC_FUEL_LEASED_BURN_BY_CLASS = 0.
         NUC_FUEL_OWNED_BURN_BY_CLASS = 0.
         NUCLEAR_MWH_BY_CLASS = 0.
         NUCLEAR_MMBTU_BY_CLASS = 0.

         MAX_INTRA_CLASS = 0
         INTRA_FOSSIL_FUEL_EXPENSES = 0.
         INTRA_LEASED_NUC_FUEL_EXPENSES = 0.
         INTRA_OWNED_NUC_FUEL_EXPENSES = 0.
         INTRA_NUC_OWN_BURN = 0.
         INTRA_NUC_LEASE_BURN = 0.
         INTRA_PURCHASE_EXPENSES = 0.
         BASE_MARKET_ENRG_SALES = 0.
         BASE_MARKET_REVENUES = 0.
         PEAK_MARKET_ENRG_SALES = 0.
         PEAK_MARKET_REVENUES = 0.
         CL_EXPENSES_2_ASSET_CLASSES = UNUM

         CL_ANN_MULT_FUEL_COST = 0.
         FE_ANN_MULT_FUEL_COST = 0.
         CL_ANN_MULT_VAR_COST = 0.
         CL_ANN_MULT_FIXED_COST = 0.
         CL_ANN_MULT_REVENUE = 0.
         CL_ANN_MULT_CAPACITY = 0.
         CL_ANN_MULT_ENERGY = 0.
         CL_ANN_MULT_PURCHASES = 0.

         DO UNUM = 1, get_nunits()

            IF(ONLINE(UNUM) - CURRENT_YEAR_COMPARISON > 12  .OR. &
                     CURRENT_YEAR_COMPARISON - OFLINE(UNUM) > 12) CYCLE


            IF(INTRA_COMPANY_CLSID(UNUM) >= 0 .AND. &
                           INTRA_CO_transaction_loc(UNUM) == 'Y') THEN
               ns_cla_decs%ASSET_CLS_LOC = INTRA_COMPANY_CLSID(UNUM)
               CALL CHECK_IF_CLASS_DEFINED(ns_cla_decs%ASSET_CLS_LOC)
               ns_cla_decs%ASSET_CLS_LOC = ns_cla_decs%ASSET_CLS_LOC + 1
        MAX_INTRA_CLASS = MAX(MAX_INTRA_CLASS,ns_cla_decs%ASSET_CLS_LOC)

               SELECT CASE (EXPENSE_ASSIGNMENT(UNUM))
               CASE('F')  ! FUEL REVENUES
                  COLLECTION = MAX(1, &
                                INDEX('BA',(EXPENSE_COLLECTION(UNUM))))
        INTRA_COMPANY_REVENUES(ns_cla_decs%ASSET_CLS_LOC,COLLECTION) = &
        INTRA_COMPANY_REVENUES(ns_cla_decs%ASSET_CLS_LOC,COLLECTION) + &
                                       ANNUAL_CL_UNIT_FUEL_COST(UNUM) &
                                           *CL_POOL_FRAC_OWN(UNUM)/100.
               CASE('L','O')  ! FUEL REVENUES
                  COLLECTION = MAX(1, &
                                INDEX('BA',(EXPENSE_COLLECTION(UNUM))))
     INTRA_COMPANY_REVENUES(ns_cla_decs%ASSET_CLS_LOC,COLLECTION) = &
     INTRA_COMPANY_REVENUES(ns_cla_decs%ASSET_CLS_LOC,COLLECTION) + &
                                       ANNUAL_CL_UNIT_FUEL_COST(UNUM) &
                                           *CL_POOL_FRAC_OWN(UNUM)/100.
                  INTRA_COMPANY_NF_BURN(ns_cla_decs%ASSET_CLS_LOC) = &
                 INTRA_COMPANY_NF_BURN(ns_cla_decs%ASSET_CLS_LOC) + &
                                 (ANNUAL_CL_UNIT_FUEL_COST(UNUM)- &
                               ANNUAL_NUC_UNIT_FUEL_ADDER_COST(UNUM)) &
                                           *CL_POOL_FRAC_OWN(UNUM)/100.
               CASE('P')  ! PURCHASE REVENUES
                 INTRA_COMPANY_REVENUES(ns_cla_decs%ASSET_CLS_LOC,3) = &
                INTRA_COMPANY_REVENUES(ns_cla_decs%ASSET_CLS_LOC,3) + &
                                    (ANNUAL_CL_UNIT_FUEL_COST(UNUM)+ &
                                     ANNUAL_CL_UNIT_VAR_COST(UNUM) + &
                                     ANNUAL_CL_UNIT_FIXED_COST(UNUM)) &
                                           *CL_POOL_FRAC_OWN(UNUM)/100.
               CASE DEFAULT
               END SELECT

               SELECT CASE (EXPENSE_ASSIGNMENT(UNUM))
               CASE('F')  ! FUEL EXPENSES
                  INTRA_FOSSIL_FUEL_EXPENSES = &
                                         INTRA_FOSSIL_FUEL_EXPENSES + &
                                       ANNUAL_CL_UNIT_FUEL_COST(UNUM) &
                                           *CL_POOL_FRAC_OWN(UNUM)/100.
               CASE('L')  ! NUCLEAR FUEL EXPENSES
                  INTRA_LEASED_NUC_FUEL_EXPENSES = &
                                     INTRA_LEASED_NUC_FUEL_EXPENSES + &
                                       ANNUAL_CL_UNIT_FUEL_COST(UNUM) &
                                           *CL_POOL_FRAC_OWN(UNUM)/100.
                  INTRA_NUC_LEASE_BURN = INTRA_NUC_LEASE_BURN + &
                                 (ANNUAL_CL_UNIT_FUEL_COST(UNUM)- &
                               ANNUAL_NUC_UNIT_FUEL_ADDER_COST(UNUM)) &
                                           *CL_POOL_FRAC_OWN(UNUM)/100.
               CASE('O')  ! NUCLEAR FUEL EXPENSES
                  INTRA_OWNED_NUC_FUEL_EXPENSES = &
                                      INTRA_OWNED_NUC_FUEL_EXPENSES + &
                                       ANNUAL_CL_UNIT_FUEL_COST(UNUM) &
                                           *CL_POOL_FRAC_OWN(UNUM)/100.
                  INTRA_NUC_OWN_BURN = INTRA_NUC_OWN_BURN + &
                                 (ANNUAL_CL_UNIT_FUEL_COST(UNUM)- &
                               ANNUAL_NUC_UNIT_FUEL_ADDER_COST(UNUM)) &
                                           *CL_POOL_FRAC_OWN(UNUM)/100.
               CASE('P')  ! PURCHASE EXPENSES
                  INTRA_PURCHASE_EXPENSES = INTRA_PURCHASE_EXPENSES + &
                                     (ANNUAL_CL_UNIT_FUEL_COST(UNUM)+ &
                                      ANNUAL_CL_UNIT_VAR_COST(UNUM) + &
                                     ANNUAL_CL_UNIT_FIXED_COST(UNUM)) &
                                           *CL_POOL_FRAC_OWN(UNUM)/100.
               CASE DEFAULT
               END SELECT
            ENDIF

            COLLECTION = INDEX('ABNX',(EXPENSE_COLLECTION(UNUM)))
            IF(COLLECTION == 0) COLLECTION = 3

            ns_cla_decs%ASSET_CLS_LOC = ASSET_CLS_NUM(UNUM)
            ASSET_ALLOCATION_VECTOR = ASSET_CLS_VECTOR(UNUM)


     VOID_LOGICAL=RETURN_ASSET_CLASS_LISTS(ns_cla_decs%ASSET_CLS_LOC, &
                                                ASSET_CLASS_LIST, &
                                             ASSET_ALLOCATION_VECTOR, &
                                                ASSET_ALLOCATION_LIST)

            FT = PRIMARY_MOVER(UNUM)

            if(ft == 0) ft = 11
            TT = 10

            CLASS_POINTER = 1
            DO
        ns_cla_decs%ASSET_CLS_LOC = ASSET_CLASS_LIST(CLASS_POINTER)
               CALL CHECK_IF_CLASS_DEFINED(ns_cla_decs%ASSET_CLS_LOC)

! IDENTIFYING BASE AND PEAK EL REVENUE CLASSED

               BASE_EL_REVENUE_CASE = ns_cla_decs%ASSET_CLS_LOC == 10
               PEAK_EL_REVENUE_CASE = ns_cla_decs%ASSET_CLS_LOC == 19

               ns_cla_decs%ASSET_CLS_LOC = ns_cla_decs%ASSET_CLS_LOC + 1
               IF(ns_cla_decs%ASSET_CLS_LOC <= 0 .OR. &
                    ns_cla_decs%ASSET_CLS_LOC > &
                    ns_cla_decs%MAX_CAP_LIMITED_CLASS_ID_NUM) then
                        CYCLE
               endif
              ns_cla_decs%ASSET_CLS_LOC = &
                ns_cla_decs%ASSET_CLASS_ptr(ns_cla_decs%ASSET_CLS_LOC)
               IF(ASSET_ALLOCATION_LIST(CLASS_POINTER) < 0.) THEN
                  ALLOCATION_VECTOR = &
                              ABS(ASSET_ALLOCATION_LIST(CLASS_POINTER))
                  CALL GET_ASSET_VAR(ALLOCATION_VECTOR, &
                                           DUMMY_TYPE,ALLOCATION_VALUE)
                  ASSET_ALLOCATOR = CL_POOL_FRAC_OWN(UNUM)/100. * &
                      ALLOCATION_VALUE(MIN(globecom_year,AVAIL_DATA_YEARS))/100.
               ELSE
                  ASSET_ALLOCATOR = &
                        ASSET_ALLOCATION_LIST(CLASS_POINTER)/100. * &
                             CL_POOL_FRAC_OWN(UNUM)/100.
               ENDIF

               DO I = 1, NUMBER_OF_EMISSION_TYPES
             CL_ANN_CLASS_EMISSIONS(I,ns_cla_decs%ASSET_CLS_LOC) = &
                CL_ANN_CLASS_EMISSIONS(I,ns_cla_decs%ASSET_CLS_LOC) + &
                                    ASSET_ALLOCATOR * &
                                       ANNUAL_CL_UNIT_EMISSIONS(I,UNUM)
               ENDDO
               IF(EXPENSE_ASSIGNMENT(UNUM) == 'L') THEN
       NF_FUEL_LEASED_BY_CLASS(ns_cla_decs%ASSET_CLS_LOC,COLLECTION) = &
       NF_FUEL_LEASED_BY_CLASS(ns_cla_decs%ASSET_CLS_LOC,COLLECTION) + &
                       ASSET_ALLOCATOR * ANNUAL_CL_UNIT_FUEL_COST(UNUM)
         NUC_FUEL_LEASED_BURN_BY_CLASS(ns_cla_decs%ASSET_CLS_LOC) = &
            NUC_FUEL_LEASED_BURN_BY_CLASS(ns_cla_decs%ASSET_CLS_LOC) + &
                   ASSET_ALLOCATOR * (ANNUAL_CL_UNIT_FUEL_COST(UNUM)- &
                                 ANNUAL_NUC_UNIT_FUEL_ADDER_COST(UNUM))
                  NUCLEAR_MWH_BY_CLASS(ns_cla_decs%ASSET_CLS_LOC) = &
                NUCLEAR_MWH_BY_CLASS(ns_cla_decs%ASSET_CLS_LOC) + &
                          ASSET_ALLOCATOR * ANNUAL_CL_UNIT_ENERGY(UNUM)
                  NUCLEAR_MMBTU_BY_CLASS(ns_cla_decs%ASSET_CLS_LOC) = &
                NUCLEAR_MMBTU_BY_CLASS(ns_cla_decs%ASSET_CLS_LOC) + &
                          ASSET_ALLOCATOR * ANNUAL_CL_UNIT_MMBTUS(UNUM)
               ELSEIF(EXPENSE_ASSIGNMENT(UNUM) == 'O') THEN
       NF_FUEL_OWNED_BY_CLASS(ns_cla_decs%ASSET_CLS_LOC,COLLECTION) = &
        NF_FUEL_OWNED_BY_CLASS(ns_cla_decs%ASSET_CLS_LOC,COLLECTION) + &
                        ASSET_ALLOCATOR * &
                                         ANNUAL_CL_UNIT_FUEL_COST(UNUM)
          NUC_FUEL_OWNED_BURN_BY_CLASS(ns_cla_decs%ASSET_CLS_LOC) = &
             NUC_FUEL_OWNED_BURN_BY_CLASS(ns_cla_decs%ASSET_CLS_LOC) + &
                      ASSET_ALLOCATOR * ( &
                                      ANNUAL_CL_UNIT_FUEL_COST(UNUM)- &
                                 ANNUAL_NUC_UNIT_FUEL_ADDER_COST(UNUM))
                  NUCLEAR_MWH_BY_CLASS(ns_cla_decs%ASSET_CLS_LOC) = &
                  NUCLEAR_MWH_BY_CLASS(ns_cla_decs%ASSET_CLS_LOC) + &
                          ASSET_ALLOCATOR * ANNUAL_CL_UNIT_ENERGY(UNUM)
                  NUCLEAR_MMBTU_BY_CLASS(ns_cla_decs%ASSET_CLS_LOC) = &
                  NUCLEAR_MMBTU_BY_CLASS(ns_cla_decs%ASSET_CLS_LOC) + &
                          ASSET_ALLOCATOR * ANNUAL_CL_UNIT_MMBTUS(UNUM)
  ELSEIF(EXPENSE_ASSIGNMENT(UNUM) == 'P') THEN
     CL_ANN_CLASS_PURCHASES(ns_cla_decs%ASSET_CLS_LOC,COLLECTION) = &
       CL_ANN_CLASS_PURCHASES(ns_cla_decs%ASSET_CLS_LOC,COLLECTION) + &
                 ASSET_ALLOCATOR * &
                      (ANNUAL_CL_UNIT_FUEL_COST(UNUM) + &
                         ANNUAL_CL_UNIT_VAR_COST(UNUM) + &
                          ANNUAL_CL_UNIT_FIXED_COST(UNUM))
     CL_ANN_CLASS_CAPACITY(ns_cla_decs%ASSET_CLS_LOC,2) = &
                 CL_ANN_CLASS_CAPACITY(ns_cla_decs%ASSET_CLS_LOC,2) + &
           ASSET_ALLOCATOR * ANNUAL_CL_UNIT_CAPACITY(UNUM)
     CL_ANN_CLASS_ENERGY(ns_cla_decs%ASSET_CLS_LOC,2) = &
                   CL_ANN_CLASS_ENERGY(ns_cla_decs%ASSET_CLS_LOC,2) + &
             ASSET_ALLOCATOR * ANNUAL_CL_UNIT_ENERGY(UNUM)

  ELSE
     CL_ANN_CLASS_FUEL_COST(ns_cla_decs%ASSET_CLS_LOC,COLLECTION) = &
       CL_ANN_CLASS_FUEL_COST(ns_cla_decs%ASSET_CLS_LOC,COLLECTION) + &
          ASSET_ALLOCATOR * ANNUAL_CL_UNIT_FUEL_COST(UNUM)
     CL_ANN_MULT_FUEL_COST(ns_cla_decs%ASSET_CLS_LOC,FT) = &
           CL_ANN_MULT_FUEL_COST(ns_cla_decs%ASSET_CLS_LOC,FT) + &
          ASSET_ALLOCATOR * ANNUAL_CL_UNIT_FUEL_COST(UNUM)
     IF(FIRST_ENERGY) THEN
        IF(SPL_unit_id_loc(UNUM) == 'BayShore') THEN
    FE_ANN_MULT_FUEL_COST(ns_cla_decs%ASSET_CLS_LOC,3) = &
       FE_ANN_MULT_FUEL_COST(ns_cla_decs%ASSET_CLS_LOC,3) + &
       ASSET_ALLOCATOR * &
                     ANNUAL_CL_UNIT_FUEL_COST(UNUM)

         ELSEIF(PRIMARY_MOVER(UNUM) == 3 .OR. &
                             PRIMARY_MOVER(UNUM) == 5) THEN
            FE_ANN_MULT_FUEL_COST(ns_cla_decs%ASSET_CLS_LOC,6) = &
               FE_ANN_MULT_FUEL_COST(ns_cla_decs%ASSET_CLS_LOC,6) + &
               ASSET_ALLOCATOR * &
                             ANNUAL_CL_UNIT_FUEL_COST(UNUM)
         ELSE
            FE_ANN_MULT_FUEL_COST(ns_cla_decs%ASSET_CLS_LOC,FT) = &
               FE_ANN_MULT_FUEL_COST(ns_cla_decs%ASSET_CLS_LOC,FT) + &
               ASSET_ALLOCATOR * &
                             ANNUAL_CL_UNIT_FUEL_COST(UNUM)
         ENDIF
      ENDIF
   ENDIF
   IF(EXPENSE_ASSIGNMENT(UNUM) /= 'P') THEN
      CL_ANN_CLASS_VAR_COST(ns_cla_decs%ASSET_CLS_LOC,COLLECTION) = &
         CL_ANN_CLASS_VAR_COST(ns_cla_decs%ASSET_CLS_LOC,COLLECTION) + &
            ASSET_ALLOCATOR * ANNUAL_CL_UNIT_VAR_COST(UNUM)
      CL_ANN_CLASS_FIXED_COST(ns_cla_decs%ASSET_CLS_LOC,COLLECTION)= &
       CL_ANN_CLASS_FIXED_COST(ns_cla_decs%ASSET_CLS_LOC,COLLECTION) + &
               ASSET_ALLOCATOR * &
                            ANNUAL_CL_UNIT_FIXED_COST(UNUM)
      CL_ANN_CLASS_CAPACITY(ns_cla_decs%ASSET_CLS_LOC,1) = &
                  CL_ANN_CLASS_CAPACITY(ns_cla_decs%ASSET_CLS_LOC,1) + &
            ASSET_ALLOCATOR * ANNUAL_CL_UNIT_CAPACITY(UNUM)
      CL_ANN_CLASS_ENERGY(ns_cla_decs%ASSET_CLS_LOC,1) = &
                    CL_ANN_CLASS_ENERGY(ns_cla_decs%ASSET_CLS_LOC,1) + &
              ASSET_ALLOCATOR * ANNUAL_CL_UNIT_ENERGY(UNUM)

      CL_ANN_MULT_VAR_COST(ns_cla_decs%ASSET_CLS_LOC,FT) = &
             CL_ANN_MULT_VAR_COST(ns_cla_decs%ASSET_CLS_LOC,FT) + &
            ASSET_ALLOCATOR * ANNUAL_CL_UNIT_VAR_COST(UNUM)
      CL_ANN_MULT_FIXED_COST(ns_cla_decs%ASSET_CLS_LOC,FT)= &
           CL_ANN_MULT_FIXED_COST(ns_cla_decs%ASSET_CLS_LOC,FT) + &
               ASSET_ALLOCATOR * &
                            ANNUAL_CL_UNIT_FIXED_COST(UNUM)
      CL_ANN_MULT_CAPACITY(ns_cla_decs%ASSET_CLS_LOC,FT) = &
                  CL_ANN_MULT_CAPACITY(ns_cla_decs%ASSET_CLS_LOC,FT) + &
            ASSET_ALLOCATOR * ANNUAL_CL_UNIT_CAPACITY(UNUM)
      CL_ANN_MULT_ENERGY(ns_cla_decs%ASSET_CLS_LOC,FT) = &
                    CL_ANN_MULT_ENERGY(ns_cla_decs%ASSET_CLS_LOC,FT) + &
              ASSET_ALLOCATOR * ANNUAL_CL_UNIT_ENERGY(UNUM)
   ENDIF
   IF(ECO_SALES_ENRG_FROM(UNUM) /= 0.) THEN
      CL_ANN_CLASS_REVENUE(ns_cla_decs%ASSET_CLS_LOC,COLLECTION) = &
          CL_ANN_CLASS_REVENUE(ns_cla_decs%ASSET_CLS_LOC,COLLECTION) + &
                 ASSET_ALLOCATOR * ECO_SALES_REV_FROM(UNUM)
      CL_ANN_CLASS_ENERGY(ns_cla_decs%ASSET_CLS_LOC,3) = &
                    CL_ANN_CLASS_ENERGY(ns_cla_decs%ASSET_CLS_LOC,3) + &
                ASSET_ALLOCATOR * ECO_SALES_ENRG_FROM(UNUM)

      IF(ANNUAL_CL_UNIT_ENERGY(UNUM) > 0.) THEN

         TT_PERCENT =  ECO_SALES_ENRG_FROM(UNUM)/ &
                                ANNUAL_CL_UNIT_ENERGY(UNUM)

         CL_ANN_MULT_FUEL_COST(ns_cla_decs%ASSET_CLS_LOC,TT) = &
            CL_ANN_MULT_FUEL_COST(ns_cla_decs%ASSET_CLS_LOC,TT) + &
            ASSET_ALLOCATOR * &
                ANNUAL_CL_UNIT_FUEL_COST(UNUM) * TT_PERCENT
         CL_ANN_MULT_VAR_COST(ns_cla_decs%ASSET_CLS_LOC,TT) = &
             CL_ANN_MULT_VAR_COST(ns_cla_decs%ASSET_CLS_LOC,TT) + &
        ASSET_ALLOCATOR * ANNUAL_CL_UNIT_VAR_COST(UNUM) * &
                                                 TT_PERCENT
         CL_ANN_MULT_CAPACITY(ns_cla_decs%ASSET_CLS_LOC,TT) = &
                  CL_ANN_MULT_CAPACITY(ns_cla_decs%ASSET_CLS_LOC,TT) + &
        ASSET_ALLOCATOR * ANNUAL_CL_UNIT_CAPACITY(UNUM) * &
                                                 TT_PERCENT
         CL_ANN_MULT_ENERGY(ns_cla_decs%ASSET_CLS_LOC,TT) = &
                    CL_ANN_MULT_ENERGY(ns_cla_decs%ASSET_CLS_LOC,TT) + &
          ASSET_ALLOCATOR * ANNUAL_CL_UNIT_ENERGY(UNUM) * &
                                                 TT_PERCENT
      ENDIF

      IF(BASE_EL_REVENUE_CASE) THEN
        BASE_MARKET_ENRG_SALES = BASE_MARKET_ENRG_SALES + &
                ASSET_ALLOCATOR * ECO_SALES_ENRG_FROM(UNUM)
         BASE_MARKET_REVENUES = BASE_MARKET_REVENUES + &
                 ASSET_ALLOCATOR * ECO_SALES_REV_FROM(UNUM)
      ELSEIF(PEAK_EL_REVENUE_CASE) THEN
        PEAK_MARKET_ENRG_SALES = PEAK_MARKET_ENRG_SALES + &
                ASSET_ALLOCATOR * ECO_SALES_ENRG_FROM(UNUM)
         PEAK_MARKET_REVENUES = PEAK_MARKET_REVENUES + &
                 ASSET_ALLOCATOR * ECO_SALES_REV_FROM(UNUM)
      ENDIF
   ENDIF
   IF(ECO_PUCH_ENRG_FROM(UNUM) /= 0.) THEN
! THIS WILL WORK ONLY FOR ASSET ANALYST.
                  IF(UPPER_TRANS_Gp > 0) THEN
               TG = TRANSACTION_GROUP_ID(UNUM) ! NOTE DOUBLE ASSIGNMENT
                     TG = GET_TRANS_GROUP_POSITION(TG)
                     IF(PURCHASE_POWER_ASSIGN(TG) /= 'U') THEN
                        TG_ASSET_CLASS = PURCHASE_ASSET_CLASS_ID(TG)
!                        TG_ASSET_CLASS = 15
                        CALL CHECK_IF_CLASS_DEFINED(TG_ASSET_CLASS)
                        TG_ASSET_CLASS = TG_ASSET_CLASS + 1
                        IF(TG_ASSET_CLASS > 0 .AND. TG_ASSET_CLASS <= &
                         ns_cla_decs%MAX_CAP_LIMITED_CLASS_ID_NUM) THEN
                           TG_ASSET_CLASS = &
                        ns_cla_decs%ASSET_CLASS_ptr(TG_ASSET_CLASS)
                           CL_ANN_CLASS_PURCHASES(TG_ASSET_CLASS, &
                                                       COLLECTION) = &
                              CL_ANN_CLASS_PURCHASES(TG_ASSET_CLASS, &
                                                       COLLECTION) + &
                                               ECO_PUCH_COST_FROM(UNUM)
                           ECO_PUCH_COST_FROM(UNUM) = 0.
                        ENDIF
                     ENDIF
                  ENDIF


 CL_ANN_CLASS_PURCHASES(ns_cla_decs%ASSET_CLS_LOC,COLLECTION) = &
   CL_ANN_CLASS_PURCHASES(ns_cla_decs%ASSET_CLS_LOC,COLLECTION) + &
            ASSET_ALLOCATOR * ECO_PUCH_COST_FROM(UNUM)
 CL_ANN_CLASS_ENERGY(ns_cla_decs%ASSET_CLS_LOC,4) = &
               CL_ANN_CLASS_ENERGY(ns_cla_decs%ASSET_CLS_LOC,4) + &
            ASSET_ALLOCATOR * ECO_PUCH_ENRG_FROM(UNUM)

               ENDIF

               CLASS_POINTER = CLASS_POINTER + 1
               IF(CLASS_POINTER > AVAIL_DATA_YEARS) EXIT
               IF(ASSET_CLASS_LIST(CLASS_POINTER) == 0. .OR. &
                          ASSET_CLASS_LIST(CLASS_POINTER) == -99.) EXIT
            ENDDO ! ASSET CLASSES
         ENDDO ! CL get_nunits()

! 09/02/01. AMEREN TRANSFER PRICING CAPABILITY. TWO PARTY FOR NOW.
!           NEED A SWITCH TO DEFINE.

         IF(DETAILED_TRANSFER_PRICING) THEN
            TEMP_I2 = 0
            CALL GET_MONTHLY_TRANSFER_REV_COST( &
                                          TEMP_I2, &
                                          R1_MONTHLY_TRANSFER_REV, &
                                          R1_MONTHLY_TRANSFER_COST, &
                                          R2_MONTHLY_TRANSFER_REV, &
                                          R2_MONTHLY_TRANSFER_COST)
            TG = 1 ! ASSUME UE FOR NOW.
            TG_ASSET_CLASS = PURCHASE_ASSET_CLASS_ID(TG)
            CALL CHECK_IF_CLASS_DEFINED(TG_ASSET_CLASS)
            COLLECTION = 5 ! ASSUME
            TG_ASSET_CLASS = TG_ASSET_CLASS + 1
            IF(TG_ASSET_CLASS > 0 .AND. TG_ASSET_CLASS <= &
                     ns_cla_decs%MAX_CAP_LIMITED_CLASS_ID_NUM) THEN
            TG_ASSET_CLASS = ns_cla_decs%ASSET_CLASS_ptr(TG_ASSET_CLASS)
               CL_ANN_CLASS_PURCHASES(TG_ASSET_CLASS,COLLECTION) = &
                  CL_ANN_CLASS_PURCHASES(TG_ASSET_CLASS,COLLECTION) + &
                  R1_MONTHLY_TRANSFER_COST ! FROM TRANSACT MODULE
               CL_ANN_CLASS_REVENUE(TG_ASSET_CLASS,COLLECTION) = &
                    CL_ANN_CLASS_REVENUE(TG_ASSET_CLASS,COLLECTION) + &
                        R1_MONTHLY_TRANSFER_REV ! FROM TRANSACT MODULE
            ENDIF
            TG = 2 ! ASSUME UE FOR NOW.
            TG_ASSET_CLASS = PURCHASE_ASSET_CLASS_ID(TG)
            CALL CHECK_IF_CLASS_DEFINED(TG_ASSET_CLASS)
            COLLECTION = 5 ! ASSUME
            TG_ASSET_CLASS = TG_ASSET_CLASS + 1
            IF(TG_ASSET_CLASS > 0 .AND. TG_ASSET_CLASS <= &
                      ns_cla_decs%MAX_CAP_LIMITED_CLASS_ID_NUM) THEN
            TG_ASSET_CLASS = ns_cla_decs%ASSET_CLASS_ptr(TG_ASSET_CLASS)
               CL_ANN_CLASS_PURCHASES(TG_ASSET_CLASS,COLLECTION) = &
                 CL_ANN_CLASS_PURCHASES(TG_ASSET_CLASS,COLLECTION) + &
                       R2_MONTHLY_TRANSFER_COST ! FROM TRANSACT MODULE
               CL_ANN_CLASS_REVENUE(TG_ASSET_CLASS,COLLECTION) = &
                   CL_ANN_CLASS_REVENUE(TG_ASSET_CLASS,COLLECTION) + &
                       R2_MONTHLY_TRANSFER_REV ! FROM TRANSACT MODULE
            ENDIF
         ENDIF

! GET COST OF SUPPLYING CAPACITY FOR A "SHORT" SYSTEM.

         RUN_TRANSACT = YES_RUN_TRANSACT() ! 10/28/03.

         IF(UPPER_TRANS_Gp > 0 .AND. RUN_TRANSACT) THEN

            DO TG = 1, UPPER_TRANS_Gp

               TEMP_I2 = 0
               CALL GET_ANNUAL_UNSERVED_COST( &
                                          TG, &
                                          TRANS_ANNUAL_UNSERVED_COST, &
                                          TEMP_R4, & !  UNSERVED MWH'S
                                    TEMP_I2) ! ANNUAL CALL 6/25/01.

                  IF(PURCHASE_POWER_ASSIGN(TG) == 'A') THEN
                     COLLECTION = 1
                  ELSEIF(PURCHASE_POWER_ASSIGN(TG) == 'B') THEN
                     COLLECTION = 2
                  ELSEIF(PURCHASE_POWER_ASSIGN(TG) == 'C') THEN
                     COLLECTION = 4
                  ELSE
                     COLLECTION = 3
                  ENDIF
                  TG_ASSET_CLASS = PURCHASE_ASSET_CLASS_ID(TG)

                  CALL CHECK_IF_CLASS_DEFINED(TG_ASSET_CLASS)
                  TG_ASSET_CLASS = TG_ASSET_CLASS + 1
                  IF(TG_ASSET_CLASS > 0 .AND. TG_ASSET_CLASS <= &
                      ns_cla_decs%MAX_CAP_LIMITED_CLASS_ID_NUM) THEN
                     TG_ASSET_CLASS = &
                            ns_cla_decs%ASSET_CLASS_ptr(TG_ASSET_CLASS)
             CL_ANN_CLASS_PURCHASES(TG_ASSET_CLASS,COLLECTION) = &
                            CL_ANN_CLASS_PURCHASES(TG_ASSET_CLASS, &
                                                     COLLECTION) + &
                TRANS_ANNUAL_UNSERVED_COST ! FROM TRANSACT MODULE
                  ENDIF

            ENDDO
         ENDIF

         DEALLOCATE(ASSET_CLASS_LIST,ASSET_ALLOCATION_LIST)

! MARKET REVENUE PROXY FOR UNION EL get_nunits() 8/7/97

         EL_BASE_RATE = 0.
         EL_PEAK_RATE = 0.
         IF(BASE_MARKET_ENRG_SALES > 50 .AND. &
                                       BASE_MARKET_REVENUES > 100) THEN
            EL_BASE_RATE = BASE_MARKET_REVENUES/BASE_MARKET_ENRG_SALES
         ENDIF
         IF(PEAK_MARKET_ENRG_SALES > 25 .AND. &
                                        PEAK_MARKET_REVENUES > 50) THEN
            EL_PEAK_RATE = PEAK_MARKET_REVENUES/PEAK_MARKET_ENRG_SALES
         ENDIF

         DO COLLECTION = 0, 3
            DO CLASS = 1, MAX_INTRA_CLASS
               INTRA_COMPANY_REVENUES(0,COLLECTION) = &
                               INTRA_COMPANY_REVENUES(0,COLLECTION) + &
                               INTRA_COMPANY_REVENUES(CLASS,COLLECTION)
            ENDDO
         ENDDO
         DO CLASS = 1, MAX_INTRA_CLASS
            INTRA_COMPANY_NF_BURN(0) = INTRA_COMPANY_NF_BURN(0) + &
                                           INTRA_COMPANY_NF_BURN(CLASS)
         ENDDO

         DO CLASS = 1, NUMBER_OF_CAP_LIMITED_CLASSES
            NUC_FUEL_LEASED_BURN_BY_CLASS(CLASS) = &
                          NUC_FUEL_LEASED_BURN_BY_CLASS(CLASS)/1000000.
            NUC_FUEL_LEASED_BURN_BY_CLASS(0) = &
                                   NUC_FUEL_LEASED_BURN_BY_CLASS(0) + &
                                   NUC_FUEL_LEASED_BURN_BY_CLASS(CLASS)
            NUC_FUEL_OWNED_BURN_BY_CLASS(CLASS) = &
                           NUC_FUEL_OWNED_BURN_BY_CLASS(CLASS)/1000000.
            NUC_FUEL_OWNED_BURN_BY_CLASS(0) = &
                                    NUC_FUEL_OWNED_BURN_BY_CLASS(0) + &
                                    NUC_FUEL_OWNED_BURN_BY_CLASS(CLASS)
            NUCLEAR_MWH_BY_CLASS(0) = NUCLEAR_MWH_BY_CLASS(0) + &
                                            NUCLEAR_MWH_BY_CLASS(CLASS)
            NUCLEAR_MMBTU_BY_CLASS(0) = NUCLEAR_MMBTU_BY_CLASS(0) + &
                                          NUCLEAR_MMBTU_BY_CLASS(CLASS)
            DO EMISS_TYPE = 1, NUMBER_OF_EMISSION_TYPES
               CL_ANN_CLASS_EMISSIONS(EMISS_TYPE,0) = &
                               CL_ANN_CLASS_EMISSIONS(EMISS_TYPE,0) + &
                               CL_ANN_CLASS_EMISSIONS(EMISS_TYPE,CLASS)
            ENDDO
            DO I = 1, 4
               CL_ANN_CLASS_VAR_COST(CLASS,I) = &
                                CL_ANN_CLASS_VAR_COST(CLASS,I)/1000000.
               CL_ANN_CLASS_FUEL_COST(CLASS,I)  = &
                               CL_ANN_CLASS_FUEL_COST(CLASS,I)/1000000.
               NF_FUEL_LEASED_BY_CLASS(CLASS,I) = &
                              NF_FUEL_LEASED_BY_CLASS(CLASS,I)/1000000.
               NF_FUEL_OWNED_BY_CLASS(CLASS,I) = &
                               NF_FUEL_OWNED_BY_CLASS(CLASS,I)/1000000.
               CL_ANN_CLASS_FIXED_COST(CLASS,I) = &
                              CL_ANN_CLASS_FIXED_COST(CLASS,I)/1000000.
               CL_ANN_CLASS_REVENUE(CLASS,I) = &
                                 CL_ANN_CLASS_REVENUE(CLASS,I)/1000000.
               CL_ANN_CLASS_CAPACITY(CLASS,I) = &
                                CL_ANN_CLASS_CAPACITY(CLASS,I)/1000000.
               CL_ANN_CLASS_ENERGY(CLASS,I) = &
                                  CL_ANN_CLASS_ENERGY(CLASS,I)/1000000.
               CL_ANN_CLASS_PURCHASES(CLASS,I) = &
                               CL_ANN_CLASS_PURCHASES(CLASS,I)/1000000.

           CL_ANN_CLASS_VAR_COST(0,I) = CL_ANN_CLASS_VAR_COST(0,I) + &
                                         CL_ANN_CLASS_VAR_COST(CLASS,I)
           CL_ANN_CLASS_FUEL_COST(0,I)=CL_ANN_CLASS_FUEL_COST(0,I) + &
                                        CL_ANN_CLASS_FUEL_COST(CLASS,I)
               NF_FUEL_LEASED_BY_CLASS(0,I) = &
                                       NF_FUEL_LEASED_BY_CLASS(0,I) + &
                                       NF_FUEL_LEASED_BY_CLASS(CLASS,I)
            NF_FUEL_OWNED_BY_CLASS(0,I)=NF_FUEL_OWNED_BY_CLASS(0,I) + &
                                        NF_FUEL_OWNED_BY_CLASS(CLASS,I)
               CL_ANN_CLASS_FIXED_COST(0,I) = &
                                       CL_ANN_CLASS_FIXED_COST(0,I) + &
                                       CL_ANN_CLASS_FIXED_COST(CLASS,I)
              CL_ANN_CLASS_REVENUE(0,I) = CL_ANN_CLASS_REVENUE(0,I) + &
                                          CL_ANN_CLASS_REVENUE(CLASS,I)
           CL_ANN_CLASS_CAPACITY(0,I) = CL_ANN_CLASS_CAPACITY(0,I) + &
                                         CL_ANN_CLASS_CAPACITY(CLASS,I)
               CL_ANN_CLASS_ENERGY(0,I) = CL_ANN_CLASS_ENERGY(0,I) + &
                                           CL_ANN_CLASS_ENERGY(CLASS,I)
           CL_ANN_CLASS_PURCHASES(0,I)=CL_ANN_CLASS_PURCHASES(0,I) + &
                                        CL_ANN_CLASS_PURCHASES(CLASS,I)
            ENDDO
            DO I = 1, NUM_FUEL_CATEGORIES
               CL_ANN_MULT_FUEL_COST(CLASS,I)  = &
                                CL_ANN_MULT_FUEL_COST(CLASS,I)/1000000.
               IF(FIRST_ENERGY) THEN
                  FE_ANN_MULT_FUEL_COST(CLASS,I)  = &
                                FE_ANN_MULT_FUEL_COST(CLASS,I)/1000000.
                  FE_ANN_MULT_FUEL_COST(0,I) = &
                                 FE_ANN_MULT_FUEL_COST(0,I) + &
                                         FE_ANN_MULT_FUEL_COST(CLASS,I)
               ENDIF
               CL_ANN_MULT_VAR_COST(CLASS,I) = &
                                CL_ANN_MULT_VAR_COST(CLASS,I)/1000000.
               CL_ANN_MULT_FIXED_COST(CLASS,I) = &
                               CL_ANN_MULT_FIXED_COST(CLASS,I)/1000000.
               CL_ANN_MULT_REVENUE(CLASS,I) = &
                                  CL_ANN_MULT_REVENUE(CLASS,I)/1000000.
               CL_ANN_MULT_CAPACITY(CLASS,I) = &
                                 CL_ANN_MULT_CAPACITY(CLASS,I)/1000000.
               CL_ANN_MULT_ENERGY(CLASS,I) = &
                                   CL_ANN_MULT_ENERGY(CLASS,I)/1000000.
               CL_ANN_MULT_PURCHASES(CLASS,I) = &
                                CL_ANN_MULT_PURCHASES(CLASS,I)/1000000.

              CL_ANN_MULT_FUEL_COST(0,I)=CL_ANN_MULT_FUEL_COST(0,I) + &
                                         CL_ANN_MULT_FUEL_COST(CLASS,I)
              CL_ANN_MULT_VAR_COST(0,I) = CL_ANN_MULT_VAR_COST(0,I) + &
                                         CL_ANN_MULT_VAR_COST(CLASS,I)
               CL_ANN_MULT_FIXED_COST(0,I) = &
                                       CL_ANN_MULT_FIXED_COST(0,I) + &
                                        CL_ANN_MULT_FIXED_COST(CLASS,I)
               CL_ANN_MULT_REVENUE(0,I) = CL_ANN_MULT_REVENUE(0,I) + &
                                           CL_ANN_MULT_REVENUE(CLASS,I)
              CL_ANN_MULT_CAPACITY(0,I) = CL_ANN_MULT_CAPACITY(0,I) + &
                                          CL_ANN_MULT_CAPACITY(CLASS,I)
               CL_ANN_MULT_ENERGY(0,I) = CL_ANN_MULT_ENERGY(0,I) + &
                                            CL_ANN_MULT_ENERGY(CLASS,I)
            CL_ANN_MULT_PURCHASES(0,I) = CL_ANN_MULT_PURCHASES(0,I) + &
                                         CL_ANN_MULT_PURCHASES(CLASS,I)
            ENDDO
         ENDDO
      RETURN

      ENTRY GET_CL_ENERGY_BY_TYPE(  R_ISEAS, &
                                    R_NUC_ENERGY, &
                                    R_STEAM_ENERGY, &
                                    R_PURCHASES_ENERGY, &
                                    R_ASSET_CLASS_900)

         R_NUC_ENERGY = 0.
         R_STEAM_ENERGY = 0.
         R_PURCHASES_ENERGY = 0.

         ns_cla_decs%ASSET_CLS_LOC = 0 ! SYSTEM CLASS

         R_NUC_ENERGY = R_NUC_ENERGY + &
   MON_MDS_NUCLEAR_MWH_BY_CLASS(ns_cla_decs%ASSET_CLS_LOC,R_ISEAS)/1000.
         R_PURCHASES_ENERGY = R_PURCHASES_ENERGY + &
    MON_MDS_CL_CLASS_ENERGY(ns_cla_decs%ASSET_CLS_LOC,2,R_ISEAS) * 1000.


         R_ASSET_CLASS_900 = 0.
         DO I = 900, ns_cla_decs%MAX_CAP_LIMITED_CLASS_ID_NUM

            ns_cla_decs%ASSET_CLS_LOC = ns_cla_decs%ASSET_CLASS_ptr(I)
            IF(ns_cla_decs%ASSET_CLS_LOC == 0) CYCLE
            R_ASSET_CLASS_900 = R_ASSET_CLASS_900 + &
    MON_MDS_CL_CLASS_ENERGY(ns_cla_decs%ASSET_CLS_LOC,1,R_ISEAS) * 1000.
         ENDDO
         ns_cla_decs%ASSET_CLS_LOC = 0 ! SYSTEM CLASS
         R_STEAM_ENERGY = R_STEAM_ENERGY + &
            MON_MDS_CL_CLASS_ENERGY( &
            ns_cla_decs%ASSET_CLS_LOC,1,R_ISEAS) * 1000. - &
                                   R_NUC_ENERGY - R_ASSET_CLASS_900

         GET_CL_ENERGY_BY_TYPE = 1
      RETURN

      ENTRY EL_BASE_REVENUE_RATE

         EL_BASE_REVENUE_RATE = EL_BASE_RATE
      RETURN

      ENTRY EL_PEAK_REVENUE_RATE

         EL_PEAK_REVENUE_RATE = EL_PEAK_RATE
      RETURN

      ENTRY RETURN_CL_ASSET_CLASS_PROD(R_CLASS, &
                                       R_CL_ANN_CLASS_CAPACITY, &
                                       R_CL_ANN_CLASS_ENERGY)

         RETURN_CL_ASSET_CLASS_PROD = .FALSE.
         DO I = 1, 4
            R_CL_ANN_CLASS_CAPACITY(I) = 0.
            R_CL_ANN_CLASS_ENERGY(I) = 0.
         ENDDO

! NOTE: 1 == PRODUCTION FROM RESOURCE TO MEET NATIVE LOAD
!       2 == DEFINED PURCHASE TO MEET NATIVE LOAD
!       3 == ECONOMY PURCHASE TO MEET NATIVE LOAD
!       4 == ECONOMY SALES WHICH INCREASE LOAD

 IF(R_CLASS <= ns_cla_decs%MAX_CAP_LIMITED_CLASS_ID_NUM) THEN
    IF(R_CLASS ==0) THEN
       ns_cla_decs%ASSET_CLS_LOC = 0
    ELSE
       ns_cla_decs%ASSET_CLS_LOC = ns_cla_decs%ASSET_CLASS_ptr(R_CLASS)
    ENDIF
    IF(ns_cla_decs%ASSET_CLS_LOC > 0 .OR. R_CLASS == 0) THEN
       RETURN_CL_ASSET_CLASS_PROD = .TRUE.
       R_CL_ANN_CLASS_CAPACITY(1) = &
             CL_ANN_CLASS_CAPACITY(ns_cla_decs%ASSET_CLS_LOC,1)
       R_CL_ANN_CLASS_CAPACITY(2) = &
                     CL_ANN_CLASS_CAPACITY(ns_cla_decs%ASSET_CLS_LOC,2)
       R_CL_ANN_CLASS_CAPACITY(3) = &
                  CL_ANN_CLASS_CAPACITY(ns_cla_decs%ASSET_CLS_LOC,3)
       R_CL_ANN_CLASS_CAPACITY(4) = &
                   CL_ANN_CLASS_CAPACITY(ns_cla_decs%ASSET_CLS_LOC,4)

       R_CL_ANN_CLASS_ENERGY(1) = &
                   CL_ANN_CLASS_ENERGY(ns_cla_decs%ASSET_CLS_LOC,1)
       R_CL_ANN_CLASS_ENERGY(2) = &
                   CL_ANN_CLASS_ENERGY(ns_cla_decs%ASSET_CLS_LOC,2)
       R_CL_ANN_CLASS_ENERGY(3) = &
                   CL_ANN_CLASS_ENERGY(ns_cla_decs%ASSET_CLS_LOC,3)
       R_CL_ANN_CLASS_ENERGY(4) = &
                    CL_ANN_CLASS_ENERGY(ns_cla_decs%ASSET_CLS_LOC,4)
    ENDIF
         ENDIF

      RETURN

      ENTRY RETURN_CL_CLASS_TOTAL_PROD(R_CLASS, &
                                       R_CL_ANN_TOTAL_CLASS_CAPACITY, &
                                          R_CL_ANN_TOTAL_CLASS_ENERGY)

         RETURN_CL_CLASS_TOTAL_PROD = .FALSE.
         R_CL_ANN_TOTAL_CLASS_CAPACITY = 0.
         R_CL_ANN_TOTAL_CLASS_ENERGY = 0.

! NOTE: 1 == PRODUCTION FROM RESOURCE TO MEET NATIVE LOAD
!       2 == DEFINED PURCHASE TO MEET NATIVE LOAD
!       3 == ECONOMY PURCHASE TO MEET NATIVE LOAD
!       4 == ECONOMY SALES WHICH INCREASE LOAD

         IF(R_CLASS <= ns_cla_decs%MAX_CAP_LIMITED_CLASS_ID_NUM) THEN
            IF(R_CLASS ==0) THEN
               ns_cla_decs%ASSET_CLS_LOC = 0
            ELSE
        ns_cla_decs%ASSET_CLS_LOC = ns_cla_decs%ASSET_CLASS_ptr(R_CLASS)
            ENDIF
 iF(ns_cla_decs%ASSET_CLS_LOC > 0 .OR. R_CLASS == 0) THEN
   RETURN_CL_CLASS_TOTAL_PROD = .TRUE.
   R_CL_ANN_TOTAL_CLASS_CAPACITY = &
                  CL_ANN_CLASS_CAPACITY(ns_cla_decs%ASSET_CLS_LOC,1) + &
                  CL_ANN_CLASS_CAPACITY(ns_cla_decs%ASSET_CLS_LOC,2) + &
                      CL_ANN_CLASS_CAPACITY(ns_cla_decs%ASSET_CLS_LOC,3)

   R_CL_ANN_TOTAL_CLASS_ENERGY = &
                  CL_ANN_CLASS_ENERGY(ns_cla_decs%ASSET_CLS_LOC,1) + &
                 CL_ANN_CLASS_ENERGY(ns_cla_decs%ASSET_CLS_LOC,2) + &
                        CL_ANN_CLASS_ENERGY(ns_cla_decs%ASSET_CLS_LOC,3)

 eNDIF
         ENDIF
      RETURN

      ENTRY RETURN_FE_PNL_EXPENSES(R_CLASS, &
                                   R_PNL_FUEL_COST, &
                                   R_PNL_PURCHASE_POWER_EXPENSE, &
                                   R_PNL_VARIABLE_EXPENSE, &
                                   R_PNL_FIXED_EXPENSE, &
                                   R_PNL_TOTAL_SALES_REVENUE, &
                                   R_PNL_CL_ANN_TOTAL_CLASS_ENERGY, &
                                   R_PNL_CL_ANN_CLASS_CAPACITY, &
                                   R_BAY_SHORE_FUEL_EXPENSE)

         RETURN_FE_PNL_EXPENSES = .FALSE.
         IF(R_CLASS <= ns_cla_decs%MAX_CAP_LIMITED_CLASS_ID_NUM) THEN
            IF(R_CLASS ==0) THEN
               ns_cla_decs%ASSET_CLS_LOC = 0
            ELSE
       ns_cla_decs%ASSET_CLS_LOC = ns_cla_decs%ASSET_CLASS_ptr(R_CLASS)
            ENDIF
 IF(ns_cla_decs%ASSET_CLS_LOC > 0 .OR. R_CLASS == 0) THEN
    RETURN_FE_PNL_EXPENSES = .TRUE.

!   NEED TO GET NUCLEAR FUEL AND EMISSIONS CREDITS ELSEWHERE

    R_BAY_SHORE_FUEL_EXPENSE = &
                     FE_ANN_MULT_FUEL_COST(ns_cla_decs%ASSET_CLS_LOC,3)

    DO FT = 1, 6

       R_PNL_FUEL_COST(FT) = &
                    CL_ANN_MULT_FUEL_COST(ns_cla_decs%ASSET_CLS_LOC,FT)

       R_PNL_VARIABLE_EXPENSE(FT) = &
                     CL_ANN_MULT_VAR_COST(ns_cla_decs%ASSET_CLS_LOC,FT)

       R_PNL_FIXED_EXPENSE(FT) = &
                    CL_ANN_MULT_FIXED_COST(ns_cla_decs%ASSET_CLS_LOC,FT)

       R_PNL_TOTAL_SALES_REVENUE(FT) = &
                       CL_ANN_MULT_REVENUE(ns_cla_decs%ASSET_CLS_LOC,FT)
    ENDDO

    DO FT = 1, NUM_FUEL_CATEGORIES
       R_PNL_CL_ANN_TOTAL_CLASS_ENERGY(FT) = &
                  MON_MDS_CL_MULT_ENERGY(ns_cla_decs%ASSET_CLS_LOC,FT,0)

       R_PNL_CL_ANN_CLASS_CAPACITY(FT) = &
     MON_MDS_CL_MULT_CAPACITY(ns_cla_decs%ASSET_CLS_LOC,FT,0) ! 02/04/04

       R_PNL_PURCHASE_POWER_EXPENSE(FT) = &
               MON_MDS_CL_MULT_PURCHASES(ns_cla_decs%ASSET_CLS_LOC,FT,0)
       IF(R_PNL_PURCHASE_POWER_EXPENSE(FT) > 0.1) THEN
          TEMP_R4 = TEMP_R4
       ENDIF

               ENDDO

            ENDIF
         ENDIF

      RETURN

      ENTRY RETURN_AMEREN_CL_CLASS_EXPENSES(R_CLASS, &
                 R_TOTAL_SALES_REVENUE, &
                 R_WHOLESALE_FUEL_EXPENSE, &
                 R_WHOLESALE_VOM_EXPENSE)

         R_TOTAL_SALES_REVENUE = 0.
         R_WHOLESALE_FUEL_EXPENSE = 0.
         R_WHOLESALE_VOM_EXPENSE = 0.
         IF(R_CLASS <= ns_cla_decs%MAX_CAP_LIMITED_CLASS_ID_NUM) THEN
            IF(R_CLASS ==0) THEN
               ns_cla_decs%ASSET_CLS_LOC = 0
            ELSE
               ns_cla_decs%ASSET_CLS_LOC = &
                ns_cla_decs%ASSET_CLASS_ptr(R_CLASS)
            ENDIF
            IF(ns_cla_decs%ASSET_CLS_LOC > 0 .OR. R_CLASS == 0) THEN
               R_TOTAL_SALES_REVENUE = &
          CL_ANN_CLASS_REVENUE(ns_cla_decs%ASSET_CLS_LOC,1) &
        + CL_ANN_CLASS_REVENUE(ns_cla_decs%ASSET_CLS_LOC,2) &
          + CL_ANN_CLASS_REVENUE(ns_cla_decs%ASSET_CLS_LOC,3)

               TT = 10
               R_WHOLESALE_FUEL_EXPENSE = &
                    CL_ANN_MULT_FUEL_COST(ns_cla_decs%ASSET_CLS_LOC,TT)

               R_WHOLESALE_VOM_EXPENSE = &
                     CL_ANN_MULT_VAR_COST(ns_cla_decs%ASSET_CLS_LOC,TT)
            ENDIF
         ENDIF
         RETURN_AMEREN_CL_CLASS_EXPENSES = 1
      RETURN

      ENTRY RETURN_CL_ASSET_CLASS_EXPENSES(R_CLASS,R_CLASS_EXISTS, &
                                         R_FUEL_COST, &
                                         R_PURCHASE_POWER_EXPENSE, &
                                         R_VARIABLE_EXPENSE, &
                                         R_FIXED_EXPENSE, &
                                     R_EXPENSE_COLLECTED_ADJ_CLAUSE, &
                                     R_EXPENSE_COLLECTED_BASE_RATES, &
                                         R_NOT_COLLECTED_IN_RATES, &
                                         R_BTL_SALES_REVENUE, &
                                         R_TOTAL_SALES_REVENUE, &
                                         R_BTL_EXPENSES, &
                                         R_WHOLESALE_FUEL_EXPENSE, &
                                         R_WHOLESALE_VOM_EXPENSE, &
                                         R_ICAP_REVENUES, &
                                         R_WVPA_EMISSIONS_EXPENSE, &
                              R_CAPACITY_SALES_TO_LEVEL_RM,    & !  726
                             R_CAPACITY_PURCHASES_TO_LEVEL_RM)  ! 725

         if(file_trace_clax==0) then
            file_trace_clax=open_trace( &
                "return_cl_asset_class_expenses.trace", rq_clax)
         end if
         
         R_CLASS_EXISTS = .FALSE.
         IF(R_CLASS <= ns_cla_decs%MAX_CAP_LIMITED_CLASS_ID_NUM) THEN
            IF(R_CLASS ==0) THEN
               ns_cla_decs%ASSET_CLS_LOC = 0
            ELSE
        ns_cla_decs%ASSET_CLS_LOC = ns_cla_decs%ASSET_CLASS_ptr(R_CLASS)
            ENDIF
            IF(ns_cla_decs%ASSET_CLS_LOC > 0 .OR. R_CLASS == 0) THEN
               R_CLASS_EXISTS = .TRUE.
               MO = 0

               DO I = 1, NUMBER_OF_EMISSION_TYPES
      MON_MDS_CL_CLASS_EMISSIONS_COST(I,ns_cla_decs%ASSET_CLS_LOC,0) = &
                       SUM(MON_MDS_CL_CLASS_EMISSIONS_COST(I, &
                ns_cla_decs%ASSET_CLS_LOC,1:))
      ENDDO
      IF(WVPA()) R_WVPA_EMISSIONS_EXPENSE = &
     SUM(MON_MDS_CL_CLASS_EMISSIONS_COST(:,ns_cla_decs%ASSET_CLS_LOC,0))

      R_ICAP_REVENUES = R_ICAP_REVENUES &
   + ns_cla_decs%MON_MDS_ICAP_REV_BY_CLASS(ns_cla_decs%ASSET_CLS_LOC,0)

      R_PURCHASE_POWER_EXPENSE = R_PURCHASE_POWER_EXPENSE &
          + ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES( &
            ns_cla_decs%ASSET_CLS_LOC,1,MO) &
          + ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES( &
            ns_cla_decs%ASSET_CLS_LOC,2,MO) &
            + ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES( &
                ns_cla_decs%ASSET_CLS_LOC,3,MO)
      R_CAPACITY_PURCHASES_TO_LEVEL_RM = &
             ns_cla_decs%MON_MDS_CL_CAP_PURCHASES( &
                ns_cla_decs%ASSET_CLS_LOC,1,MO) &
           + ns_cla_decs%MON_MDS_CL_CAP_PURCHASES( &
           ns_cla_decs%ASSET_CLS_LOC,2,MO) &
              + ns_cla_decs%MON_MDS_CL_CAP_PURCHASES( &
              ns_cla_decs%ASSET_CLS_LOC,3,MO)
      call write_trace_real(file_trace_clax, "cap/pc/l/rm",  &
      R_CAPACITY_PURCHASES_TO_LEVEL_RM)

      R_FUEL_COST = R_FUEL_COST &
          + ns_cla_decs%MON_MDS_CL_CLASS_FUEL_COST(&
            ns_cla_decs%ASSET_CLS_LOC,1,MO) &
          + ns_cla_decs%MON_MDS_CL_CLASS_FUEL_COST( &
            ns_cla_decs%ASSET_CLS_LOC,2,MO) &
            + ns_cla_decs%MON_MDS_CL_CLASS_FUEL_COST( &
                ns_cla_decs%ASSET_CLS_LOC,3,MO)

      R_VARIABLE_EXPENSE = R_VARIABLE_EXPENSE + &
           MON_MDS_CL_CLASS_VAR_COST(ns_cla_decs%ASSET_CLS_LOC,1,MO) + &
           MON_MDS_CL_CLASS_VAR_COST(ns_cla_decs%ASSET_CLS_LOC,2,MO) + &
               MON_MDS_CL_CLASS_VAR_COST(ns_cla_decs%ASSET_CLS_LOC,3,MO)

      R_FIXED_EXPENSE = R_FIXED_EXPENSE &
        + MON_MDS_CL_CLASS_FIXED_COST(ns_cla_decs%ASSET_CLS_LOC,1,MO) &
        + MON_MDS_CL_CLASS_FIXED_COST(ns_cla_decs%ASSET_CLS_LOC,2,MO) &
          + MON_MDS_CL_CLASS_FIXED_COST(ns_cla_decs%ASSET_CLS_LOC,3,MO)

   R_EXPENSE_COLLECTED_ADJ_CLAUSE = &
              R_EXPENSE_COLLECTED_ADJ_CLAUSE &
          + ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES( &
            ns_cla_decs%ASSET_CLS_LOC,1,MO) &
            + ns_cla_decs%MON_MDS_CL_CAP_PURCHASES( &
                ns_cla_decs%ASSET_CLS_LOC,1,MO) &
           + ns_cla_decs%MON_MDS_NF_FUEL_LEASED_BC( &
            ns_cla_decs%ASSET_CLS_LOC,1,MO) &
            + ns_cla_decs%MON_MDS_NF_FUEL_OWNED_BC( &
                ns_cla_decs%ASSET_CLS_LOC,1,MO) &
            + ns_cla_decs%MON_MDS_CL_CLASS_FUEL_COST( &
                ns_cla_decs%ASSET_CLS_LOC,1,MO)

   R_EXPENSE_COLLECTED_BASE_RATES = &
             R_EXPENSE_COLLECTED_BASE_RATES &
          + ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES( &
            ns_cla_decs%ASSET_CLS_LOC,2,MO) &
            + ns_cla_decs%MON_MDS_CL_CAP_PURCHASES( &
                ns_cla_decs%ASSET_CLS_LOC,2,MO) &
           + MON_MDS_CL_CLASS_VAR_COST( &
            ns_cla_decs%ASSET_CLS_LOC,2,MO) &
         + MON_MDS_CL_CLASS_FIXED_COST( &
            ns_cla_decs%ASSET_CLS_LOC,2,MO) &
           + ns_cla_decs%MON_MDS_NF_FUEL_LEASED_BC( &
            ns_cla_decs%ASSET_CLS_LOC,2,MO) &
            + ns_cla_decs%MON_MDS_NF_FUEL_OWNED_BC( &
                ns_cla_decs%ASSET_CLS_LOC,2,MO) &
            + ns_cla_decs%MON_MDS_CL_CLASS_FUEL_COST( &
            ns_cla_decs%ASSET_CLS_LOC,2,MO)


    R_NOT_COLLECTED_IN_RATES = R_NOT_COLLECTED_IN_RATES &
          + ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES( &
          ns_cla_decs%ASSET_CLS_LOC,3,MO) &
            + ns_cla_decs%MON_MDS_CL_CAP_PURCHASES( &
            ns_cla_decs%ASSET_CLS_LOC,3,MO) &
           + MON_MDS_CL_CLASS_VAR_COST(ns_cla_decs%ASSET_CLS_LOC,3,MO) &
         + MON_MDS_CL_CLASS_FIXED_COST(ns_cla_decs%ASSET_CLS_LOC,3,MO) &
           + ns_cla_decs%MON_MDS_NF_FUEL_LEASED_BC( &
            ns_cla_decs%ASSET_CLS_LOC,3,MO) &
         + ns_cla_decs%MON_MDS_NF_FUEL_OWNED_BC( &
            ns_cla_decs%ASSET_CLS_LOC,3,MO) &
            + ns_cla_decs%MON_MDS_CL_CLASS_FUEL_COST( &
                ns_cla_decs%ASSET_CLS_LOC,3,MO)

    R_TOTAL_SALES_REVENUE = R_TOTAL_SALES_REVENUE &
           + ns_cla_decs%MON_MDS_CL_CLASS_REVENUE( &
            ns_cla_decs%ASSET_CLS_LOC,1,MO) &
            + ns_cla_decs%MON_MDS_CL_CLASS_REVENUE( &
                ns_cla_decs%ASSET_CLS_LOC,2,MO) &
              + ns_cla_decs%MON_MDS_CL_CLASS_REVENUE( &
                ns_cla_decs%ASSET_CLS_LOC,3,MO)
    R_CAPACITY_SALES_TO_LEVEL_RM = &
              ns_cla_decs%MON_MDS_CL_CAP_REVENUE( &
                ns_cla_decs%ASSET_CLS_LOC,1,MO) &
              + ns_cla_decs%MON_MDS_CL_CAP_REVENUE( &
                ns_cla_decs%ASSET_CLS_LOC,2,MO) &
                + ns_cla_decs%MON_MDS_CL_CAP_REVENUE( &
                ns_cla_decs%ASSET_CLS_LOC,3,MO)

    R_SALES_REVENUE_NOT_IN_RATES = &
                 R_SALES_REVENUE_NOT_IN_RATES &
            + ns_cla_decs%MON_MDS_CL_CLASS_REVENUE( &
                ns_cla_decs%ASSET_CLS_LOC,3,MO) &
                + ns_cla_decs%MON_MDS_CL_CAP_REVENUE( &
                ns_cla_decs%ASSET_CLS_LOC,3,MO)

               R_BTL_SALES_REVENUE = R_BTL_SALES_REVENUE &
           + ns_cla_decs%MON_MDS_CL_CLASS_REVENUE( &
           ns_cla_decs%ASSET_CLS_LOC,4,MO) &
               + ns_cla_decs%MON_MDS_CL_CAP_REVENUE( &
               ns_cla_decs%ASSET_CLS_LOC,4,MO)

               R_BTL_EXPENSES = R_BTL_EXPENSES &
         + ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES( &
            ns_cla_decs%ASSET_CLS_LOC,4,MO) &
           + ns_cla_decs%MON_MDS_CL_CAP_PURCHASES( &
            ns_cla_decs%ASSET_CLS_LOC,4,MO) &
                       + MON_MDS_CL_CLASS_VAR_COST( &
                       ns_cla_decs%ASSET_CLS_LOC,4,MO) &
                     + MON_MDS_CL_CLASS_FIXED_COST( &
                     ns_cla_decs%ASSET_CLS_LOC,4,MO) &
  + ns_cla_decs%MON_MDS_NF_FUEL_LEASED_BC( &
    ns_cla_decs%ASSET_CLS_LOC,4,MO) &
         + ns_cla_decs%MON_MDS_NF_FUEL_OWNED_BC( &
         ns_cla_decs%ASSET_CLS_LOC,4,MO) &
            + ns_cla_decs%MON_MDS_CL_CLASS_FUEL_COST( &
            ns_cla_decs%ASSET_CLS_LOC,4,MO)

               TT = 10
               R_WHOLESALE_FUEL_EXPENSE = &
                          MON_MDS_CL_MULT_FUEL_COST( &
                          ns_cla_decs%ASSET_CLS_LOC,TT,MO)

               R_WHOLESALE_VOM_EXPENSE = &
                           MON_MDS_CL_MULT_VAR_COST( &
                           ns_cla_decs%ASSET_CLS_LOC,TT,MO)

           ENDIF
         ENDIF
         RETURN_CL_ASSET_CLASS_EXPENSES = 1
      RETURN

     ENTRY RETURN_NUC_CL_ASSET_CLASS_EXPENSES(R_CLASS,R_CLASS_EXISTS, &
                                         R_NUC_FUEL_OWNED, &
                                         R_NUC_FUEL_LEASED, &
                                         R_NF_BURN_IN_RATEBASE, &
                                         R_NUC_FUEL_OWNED_BURN, &
                                         R_NUC_FUEL_LEASED_BURN, &
                                         R_DOE_NUC_FUEL_FEE, &
                                         R_NUC_DECOMMISSIONING_COST, &
                                         R_DOE_R300_DISPOSAL_COST)

         R_CLASS_EXISTS = .FALSE.
         R_NUC_FUEL_OWNED_BURN = 0.
         R_NUC_FUEL_LEASED_BURN = 0.
         R_DOE_R300_DISPOSAL_COST = 0.
         IF(R_CLASS <= ns_cla_decs%MAX_CAP_LIMITED_CLASS_ID_NUM) THEN
            IF(R_CLASS ==0) THEN
               ns_cla_decs%ASSET_CLS_LOC = 0
            ELSE
        ns_cla_decs%ASSET_CLS_LOC = ns_cla_decs%ASSET_CLASS_ptr(R_CLASS)
            ENDIF
            IF(ns_cla_decs%ASSET_CLS_LOC > 0 .OR. R_CLASS == 0) THEN
               R_CLASS_EXISTS = .TRUE.
               MO = 0

               R_NUC_FUEL_OWNED_BURN = &
  ns_cla_decs%MON_MDS_NF_FUEL_OWNED_BC(ns_cla_decs%ASSET_CLS_LOC,1,MO) &
+ ns_cla_decs%MON_MDS_NF_FUEL_OWNED_BC(ns_cla_decs%ASSET_CLS_LOC,2,MO) &
  + ns_cla_decs%MON_MDS_NF_FUEL_OWNED_BC(ns_cla_decs%ASSET_CLS_LOC,3,MO)

    R_NUC_FUEL_LEASED_BURN = &
 ns_cla_decs%MON_MDS_NF_FUEL_LEASED_BC(ns_cla_decs%ASSET_CLS_LOC,1,MO) &
           + ns_cla_decs%MON_MDS_NF_FUEL_LEASED_BC( &
            ns_cla_decs%ASSET_CLS_LOC,2,MO) &
             + ns_cla_decs%MON_MDS_NF_FUEL_LEASED_BC( &
                ns_cla_decs%ASSET_CLS_LOC,3,MO)

    MMBTU_ENERGY = &
       MON_MDS_NUCLEAR_MMBTU_BY_CLASS(ns_cla_decs%ASSET_CLS_LOC,MO)
               MWH_ENERGY = &
    MON_MDS_NUCLEAR_MWH_BY_CLASS(ns_cla_decs%ASSET_CLS_LOC,MO)
               R_DOE_NUC_FUEL_FEE = R_DOE_NUC_FUEL_FEE + &
                          DOE_NF_DISPOSAL_COST(MMBTU_ENERGY,MWH_ENERGY)
               R_DOE_R300_DISPOSAL_COST = &
                        DOE_R300_DISPOSAL_COST(MMBTU_ENERGY,MWH_ENERGY)
           R_NUC_DECOMMISSIONING_COST = R_NUC_DECOMMISSIONING_COST + &
                  NUCLEAR_DECOMMISSIONING_COST(MMBTU_ENERGY,MWH_ENERGY)

     R_NF_BURN_IN_RATEBASE = R_NF_BURN_IN_RATEBASE &
       + ns_cla_decs%MON_MDS_NF_FUEL_OWNED_BC( &
        ns_cla_decs%ASSET_CLS_LOC,1,MO) &
       + ns_cla_decs%MON_MDS_NF_FUEL_OWNED_BC( &
        ns_cla_decs%ASSET_CLS_LOC,2,MO) &
         + ns_cla_decs%MON_MDS_NF_FUEL_OWNED_BC( &
            ns_cla_decs%ASSET_CLS_LOC,3,MO)

               R_NUC_FUEL_LEASED = R_NUC_FUEL_LEASED + &
        ns_cla_decs%MON_MDS_NF_FUEL_LEASED_BC( &
      ns_cla_decs%ASSET_CLS_LOC,1,MO) &
       + ns_cla_decs%MON_MDS_NF_FUEL_LEASED_BC( &
       ns_cla_decs%ASSET_CLS_LOC,2,MO) &
         + ns_cla_decs%MON_MDS_NF_FUEL_LEASED_BC( &
         ns_cla_decs%ASSET_CLS_LOC,3,MO)

             R_NUC_FUEL_OWNED = R_NUC_FUEL_OWNED &
            + ns_cla_decs%MON_MDS_NF_FUEL_OWNED_BC( &
            ns_cla_decs%ASSET_CLS_LOC,1,MO) &
            + ns_cla_decs%MON_MDS_NF_FUEL_OWNED_BC( &
            ns_cla_decs%ASSET_CLS_LOC,2,MO) &
              + ns_cla_decs%MON_MDS_NF_FUEL_OWNED_BC( &
              ns_cla_decs%ASSET_CLS_LOC,3,MO)

           ENDIF
         ENDIF
         RETURN_NUC_CL_ASSET_CLASS_EXPENSES = 1
      RETURN

      ENTRY RETURN_MONTHLY_CL_EXPENSES(R_CLASS,MONTH_VARS)


         RETURN_MONTHLY_CL_EXPENSES = 1
         IF(.NOT. ALLOCATED(ns_cla_decs%MON_MDS_NF_FUEL_OWNED_BC)) then
            RETURN
         endif

         IF(R_CLASS <= ns_cla_decs%MAX_CAP_LIMITED_CLASS_ID_NUM) THEN
            IF(R_CLASS ==0) THEN
               ns_cla_decs%ASSET_CLS_LOC = 0
            ELSE
       ns_cla_decs%ASSET_CLS_LOC = ns_cla_decs%ASSET_CLASS_ptr(R_CLASS)
            ENDIF
            IF(ns_cla_decs%ASSET_CLS_LOC > 0 .OR. R_CLASS == 0) THEN

               DO MO = 0, 12
                  IF(WVPA()) THEN
                     MONTH_VARS(MO,Emission Credits) = &
                            MONTH_VARS(MO,Emission Credits) &
                            + SUM(MON_MDS_CL_CLASS_EMISSIONS_COST(:, &
                                          ns_cla_decs%ASSET_CLS_LOC,MO))
       ENDIF
       MONTH_VARS(MO,Owned Nuclear Fuel) = &
                 MONTH_VARS(MO,Owned Nuclear Fuel) &
+ ns_cla_decs%MON_MDS_NF_FUEL_OWNED_BC(ns_cla_decs%ASSET_CLS_LOC,1,MO) &
+ ns_cla_decs%MON_MDS_NF_FUEL_OWNED_BC(ns_cla_decs%ASSET_CLS_LOC,2,MO) &
  + ns_cla_decs%MON_MDS_NF_FUEL_OWNED_BC(ns_cla_decs%ASSET_CLS_LOC,3,MO)
       MONTH_VARS(MO,Leased Nuclear Fuel) = &
                MONTH_VARS(MO,Leased Nuclear Fuel) &
           + ns_cla_decs%MON_MDS_NF_FUEL_LEASED_BC( &
           ns_cla_decs%ASSET_CLS_LOC,1,MO) &
        + ns_cla_decs%MON_MDS_NF_FUEL_LEASED_BC(&
        ns_cla_decs%ASSET_CLS_LOC,2,MO) &
            + ns_cla_decs%MON_MDS_NF_FUEL_LEASED_BC( &
            ns_cla_decs%ASSET_CLS_LOC,3,MO)
                  MMBTU_ENERGY = &
                       MON_MDS_NUCLEAR_MMBTU_BY_CLASS( &
                       ns_cla_decs%ASSET_CLS_LOC,MO)
                  MWH_ENERGY = &
                        MON_MDS_NUCLEAR_MWH_BY_CLASS( &
                        ns_cla_decs%ASSET_CLS_LOC,MO)

                  MONTH_VARS(MO,DOE Disposal) = &
                         MONTH_VARS(MO,DOE Disposal) + &
                      DOE_NF_DISPOSAL_COST(MMBTU_ENERGY,MWH_ENERGY) + &
                        DOE_R300_DISPOSAL_COST(MMBTU_ENERGY,MWH_ENERGY)
      MONTH_VARS(MO,DOE Decommissioning) = &
       MONTH_VARS(MO,DOE Decommissioning) + &
      NUCLEAR_DECOMMISSIONING_COST(MMBTU_ENERGY,MWH_ENERGY)

      MONTH_VARS(MO,Purchased Power) = &
              MONTH_VARS(MO,Purchased Power) &
         + ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES( &
            ns_cla_decs%ASSET_CLS_LOC,1,MO) &
         + ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES( &
            ns_cla_decs%ASSET_CLS_LOC,2,MO) &
            + ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES( &
                ns_cla_decs%ASSET_CLS_LOC,3,MO)
      MONTH_VARS(MO, &
       mty_xpns_rsv_mgn_cap_pchs) = &
            + ns_cla_decs%MON_MDS_CL_CAP_PURCHASES( &
                ns_cla_decs%ASSET_CLS_LOC,1,MO) &
            + ns_cla_decs%MON_MDS_CL_CAP_PURCHASES( &
                ns_cla_decs%ASSET_CLS_LOC,2,MO) &
              + ns_cla_decs%MON_MDS_CL_CAP_PURCHASES( &
                ns_cla_decs%ASSET_CLS_LOC,3,MO)

      MONTH_VARS(MO,Fossil Fuel) = &
              MONTH_VARS(MO,Fossil Fuel) &
          + ns_cla_decs%MON_MDS_CL_CLASS_FUEL_COST( &
            ns_cla_decs%ASSET_CLS_LOC,1,MO) &
          + ns_cla_decs%MON_MDS_CL_CLASS_FUEL_COST( &
            ns_cla_decs%ASSET_CLS_LOC,2,MO) &
           + ns_cla_decs%MON_MDS_CL_CLASS_FUEL_COST( &
            ns_cla_decs%ASSET_CLS_LOC,3,MO)

  MONTH_VARS(MO,Variable OandM) = &
           MONTH_VARS(MO,Variable OandM) + &
        MON_MDS_CL_CLASS_VAR_COST(ns_cla_decs%ASSET_CLS_LOC,1,MO) + &
        MON_MDS_CL_CLASS_VAR_COST(ns_cla_decs%ASSET_CLS_LOC,2,MO) + &
        MON_MDS_CL_CLASS_VAR_COST(ns_cla_decs%ASSET_CLS_LOC,3,MO)

  MONTH_VARS(MO,Fixed OandM) = &
          MONTH_VARS(MO,Fixed OandM) + &
        MON_MDS_CL_CLASS_FIXED_COST(ns_cla_decs%ASSET_CLS_LOC,1,MO) + &
        MON_MDS_CL_CLASS_FIXED_COST(ns_cla_decs%ASSET_CLS_LOC,2,MO) + &
        MON_MDS_CL_CLASS_FIXED_COST(ns_cla_decs%ASSET_CLS_LOC,3,MO)

     MONTH_VARS(MO,BTL Expenses) = &
            MONTH_VARS(MO,BTL Expenses) &
         + ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES( &
            ns_cla_decs%ASSET_CLS_LOC,4,MO) &
           + ns_cla_decs%MON_MDS_CL_CAP_PURCHASES( &
            ns_cla_decs%ASSET_CLS_LOC,4,MO) &
          + MON_MDS_CL_CLASS_VAR_COST(ns_cla_decs%ASSET_CLS_LOC,4,MO) &
                     + MON_MDS_CL_CLASS_FIXED_COST( &
                        ns_cla_decs%ASSET_CLS_LOC,4,MO) &
        + ns_cla_decs%MON_MDS_NF_FUEL_LEASED_BC( &
            ns_cla_decs%ASSET_CLS_LOC,4,MO) &
            + ns_cla_decs%MON_MDS_NF_FUEL_OWNED_BC( &
                ns_cla_decs%ASSET_CLS_LOC,4,MO) &
            + ns_cla_decs%MON_MDS_CL_CLASS_FUEL_COST( &
                ns_cla_decs%ASSET_CLS_LOC,4,MO)

               ENDDO
            ENDIF
         ENDIF
      RETURN

      ENTRY RETURN_MONTHLY_NF_OP_EXPENSES(R_CLASS, &
                                       R_MONTHLY_OWNED_NF)


         RETURN_MONTHLY_NF_OP_EXPENSES = 1
         R_MONTHLY_OWNED_NF(:) = 0.
         IF(R_CLASS <= ns_cla_decs%MAX_CAP_LIMITED_CLASS_ID_NUM .AND. &
              ALLOCATED(ns_cla_decs%MON_MDS_NF_FUEL_OWNED_BC)) THEN
            IF(R_CLASS ==0) THEN
               ns_cla_decs%ASSET_CLS_LOC = 0
            ELSE
      ns_cla_decs%ASSET_CLS_LOC = ns_cla_decs%ASSET_CLASS_ptr(R_CLASS)
            ENDIF
            IF(ns_cla_decs%ASSET_CLS_LOC > 0 .OR. R_CLASS == 0) THEN

  R_MONTHLY_OWNED_NF(:) = &
   ns_cla_decs%MON_MDS_NF_FUEL_OWNED_BC(ns_cla_decs%ASSET_CLS_LOC,1,:) &
 + ns_cla_decs%MON_MDS_NF_FUEL_OWNED_BC(ns_cla_decs%ASSET_CLS_LOC,2,:) &
   + ns_cla_decs%MON_MDS_NF_FUEL_OWNED_BC(ns_cla_decs%ASSET_CLS_LOC,3,:)

            ENDIF
         ENDIF
      RETURN

      ENTRY RETURN_MONTHLY_CL_CASH_EXPENSES(R_CLASS,MONTH_VARS)


         RETURN_MONTHLY_CL_CASH_EXPENSES = 1
        IF(.NOT. ALLOCATED(ns_cla_decs%MON_MDS_NF_FUEL_LEASED_BC)) then
            RETURN
        endif
         IF(R_CLASS <= ns_cla_decs%MAX_CAP_LIMITED_CLASS_ID_NUM) THEN
            IF(R_CLASS ==0) THEN
               ns_cla_decs%ASSET_CLS_LOC = 0
            ELSE
      ns_cla_decs%ASSET_CLS_LOC = ns_cla_decs%ASSET_CLASS_ptr(R_CLASS)
            ENDIF
            IF(ns_cla_decs%ASSET_CLS_LOC > 0 .OR. R_CLASS == 0) THEN

               DO MO = 0, 12
                  IF(WVPA()) THEN
                     MONTH_VARS(MO,Cash Emission Credits) = &
                            MONTH_VARS(MO,Cash Emission Credits) &
                            + SUM(MON_MDS_CL_CLASS_EMISSIONS_COST(:, &
                                          ns_cla_decs%ASSET_CLS_LOC,MO))
                  ENDIF

      MONTH_VARS(MO,cash_leased_nuclear_fuel) = &
              MONTH_VARS(MO,cash_leased_nuclear_fuel) + &
           ns_cla_decs%MON_MDS_NF_FUEL_LEASED_BC( &
           ns_cla_decs%ASSET_CLS_LOC,1,MO) + &
           ns_cla_decs%MON_MDS_NF_FUEL_LEASED_BC( &
            ns_cla_decs%ASSET_CLS_LOC,2,MO) + &
              ns_cla_decs%MON_MDS_NF_FUEL_LEASED_BC( &
                ns_cla_decs%ASSET_CLS_LOC,3,MO)

      MMBTU_ENERGY = &
      MON_MDS_NUCLEAR_MMBTU_BY_CLASS(ns_cla_decs%ASSET_CLS_LOC,MO)
                  MWH_ENERGY = &
              MON_MDS_NUCLEAR_MWH_BY_CLASS(ns_cla_decs%ASSET_CLS_LOC,MO)
                  MONTH_VARS(MO,Cash DOE Disposal) = &
                         MONTH_VARS(MO,Cash DOE Disposal) + &
                      DOE_NF_DISPOSAL_COST(MMBTU_ENERGY,MWH_ENERGY) + &
            DOE_R300_DISPOSAL_COST(MMBTU_ENERGY,MWH_ENERGY)
      MONTH_VARS(MO,Cash DOE Decommissioning) = &
       MONTH_VARS(MO,Cash DOE Decommissioning) + &
      NUCLEAR_DECOMMISSIONING_COST(MMBTU_ENERGY,MWH_ENERGY)

      MONTH_VARS(MO,cash_purchased_power) = &
              MONTH_VARS(MO,cash_purchased_power) + &
          ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES( &
            ns_cla_decs%ASSET_CLS_LOC,1,MO) + &
          ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES( &
            ns_cla_decs%ASSET_CLS_LOC,2,MO) + &
              ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES( &
                ns_cla_decs%ASSET_CLS_LOC,3,MO)
      MONTH_VARS(MO, &
          csh_expns_rsv_mgn_cap_purch) = &
          MONTH_VARS(MO, &
            csh_expns_rsv_mgn_cap_purch) &
        +    ns_cla_decs%MON_MDS_CL_CAP_PURCHASES( &
            ns_cla_decs%ASSET_CLS_LOC,1,MO) &
        +    ns_cla_decs%MON_MDS_CL_CAP_PURCHASES( &
            ns_cla_decs%ASSET_CLS_LOC,2,MO) &
         +    ns_cla_decs%MON_MDS_CL_CAP_PURCHASES( &
            ns_cla_decs%ASSET_CLS_LOC,3,MO)

     MONTH_VARS(MO,cash_fossil_fuel) = &
             MONTH_VARS(MO,cash_fossil_fuel) + &
         ns_cla_decs%MON_MDS_CL_CLASS_FUEL_COST( &
            ns_cla_decs%ASSET_CLS_LOC,1,MO) + &
         ns_cla_decs%MON_MDS_CL_CLASS_FUEL_COST( &
            ns_cla_decs%ASSET_CLS_LOC,2,MO) + &
             ns_cla_decs%MON_MDS_CL_CLASS_FUEL_COST( &
                ns_cla_decs%ASSET_CLS_LOC,3,MO)

     MONTH_VARS(MO,cash_variable_oandm) = &
              MONTH_VARS(MO,cash_variable_oandm) + &
          MON_MDS_CL_CLASS_VAR_COST(ns_cla_decs%ASSET_CLS_LOC,1,MO) + &
          MON_MDS_CL_CLASS_VAR_COST(ns_cla_decs%ASSET_CLS_LOC,2,MO) + &
              MON_MDS_CL_CLASS_VAR_COST(ns_cla_decs%ASSET_CLS_LOC,3,MO)

     MONTH_VARS(MO,cash_fixed_oandm) = &
            MONTH_VARS(MO,cash_fixed_oandm) + &
        MON_MDS_CL_CLASS_FIXED_COST(ns_cla_decs%ASSET_CLS_LOC,1,MO) + &
         MON_MDS_CL_CLASS_FIXED_COST(ns_cla_decs%ASSET_CLS_LOC,2,MO) + &
          MON_MDS_CL_CLASS_FIXED_COST(ns_cla_decs%ASSET_CLS_LOC,3,MO)
      MONTH_VARS(MO,Cash BTL Expenses) = &
             MONTH_VARS(MO,Cash BTL Expenses) + &
          ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES( &
            ns_cla_decs%ASSET_CLS_LOC,4,MO) + &
            ns_cla_decs%MON_MDS_CL_CAP_PURCHASES( &
            ns_cla_decs%ASSET_CLS_LOC,4,MO) + &
           MON_MDS_CL_CLASS_VAR_COST(ns_cla_decs%ASSET_CLS_LOC,4,MO) + &
         MON_MDS_CL_CLASS_FIXED_COST(ns_cla_decs%ASSET_CLS_LOC,4,MO) + &
           ns_cla_decs%MON_MDS_NF_FUEL_LEASED_BC( &
            ns_cla_decs%ASSET_CLS_LOC,4,MO) + &
            ns_cla_decs%MON_MDS_NF_FUEL_OWNED_BC( &
                ns_cla_decs%ASSET_CLS_LOC,4,MO) + &
             ns_cla_decs%MON_MDS_CL_CLASS_FUEL_COST( &
                ns_cla_decs%ASSET_CLS_LOC,4,MO)
               ENDDO
            ENDIF
         ENDIF
      RETURN

      ENTRY RETURN_MONTHLY_CL_REVENUES(R_CLASS,MONTH_VARS)


         RETURN_MONTHLY_CL_REVENUES = 1
        IF(.NOT. ALLOCATED(ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES)) then
            RETURN
         end if
         IF(R_CLASS <= ns_cla_decs%MAX_CAP_LIMITED_CLASS_ID_NUM) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN

               DO MO = 0, 12
                  MONTH_VARS(MO,adjustment_clause) = &
              MONTH_VARS(MO,adjustment_clause) &
           + ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES(ASSET_CLASS,1,MO) &
              + ns_cla_decs%MON_MDS_CL_CAP_PURCHASES(ASSET_CLASS,1,MO) &
             + ns_cla_decs%MON_MDS_NF_FUEL_LEASED_BC(ASSET_CLASS,1,MO) &
             + ns_cla_decs%MON_MDS_NF_FUEL_OWNED_BC(ASSET_CLASS,1,MO) &
              + ns_cla_decs%MON_MDS_CL_CLASS_FUEL_COST(ASSET_CLASS,1,MO)
                  MONTH_VARS(MO,SecondarySales) = &
             MONTH_VARS(MO,SecondarySales) &
             + ns_cla_decs%MON_MDS_CL_CLASS_REVENUE(ASSET_CLASS,1,MO) &
             + ns_cla_decs%MON_MDS_CL_CLASS_REVENUE(ASSET_CLASS,2,MO) &
             + ns_cla_decs%MON_MDS_CL_CLASS_REVENUE(ASSET_CLASS,3,MO)
                MONTH_VARS(MO,Capacity Sales) = &
                         MONTH_VARS(MO,Capacity Sales) &
                + ns_cla_decs%MON_MDS_CL_CAP_REVENUE(ASSET_CLASS,1,MO) &
                + ns_cla_decs%MON_MDS_CL_CAP_REVENUE(ASSET_CLASS,2,MO) &
                + ns_cla_decs%MON_MDS_CL_CAP_REVENUE(ASSET_CLASS,3,MO)
               MONTH_VARS(MO,btl_monthly_other_income) = &
                         MONTH_VARS(MO,btl_monthly_other_income) &
              + ns_cla_decs%MON_MDS_CL_CLASS_REVENUE(ASSET_CLASS,4,MO) &
              + ns_cla_decs%MON_MDS_CL_CAP_REVENUE(ASSET_CLASS,4,MO)
                MONTH_VARS(MO,ICAP_Revs_mth) = &
                           MONTH_VARS(MO,ICAP_Revs_mth) &
                 + ns_cla_decs%MON_MDS_ICAP_REV_BY_CLASS(ASSET_CLASS,MO)
               ENDDO
            ENDIF
         ENDIF
      RETURN

      ENTRY RETURN_MONTHLY_CL_CASH_REVENUES(R_CLASS,MONTH_VARS)


         RETURN_MONTHLY_CL_CASH_REVENUES = 1
      IF(.NOT. ALLOCATED(ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES)) then
        RETURN
      endif
         IF(R_CLASS <= ns_cla_decs%MAX_CAP_LIMITED_CLASS_ID_NUM) THEN
            IF(R_CLASS ==0) THEN
               ns_cla_decs%ASSET_CLS_LOC = 0
            ELSE
   ns_cla_decs%ASSET_CLS_LOC = ns_cla_decs%ASSET_CLASS_ptr(R_CLASS)
            ENDIF
            IF(ns_cla_decs%ASSET_CLS_LOC > 0 .OR. R_CLASS == 0) THEN

               DO MO = 0, 12
    MONTH_VARS(MO,cash_adjustment_clause) = &
            MONTH_VARS(MO,cash_adjustment_clause) + &
        ns_cla_decs%MON_MDS_CL_CLASS_PURCHASES( &
            ns_cla_decs%ASSET_CLS_LOC,1,MO) + &
          ns_cla_decs%MON_MDS_CL_CAP_PURCHASES( &
            ns_cla_decs%ASSET_CLS_LOC,1,MO) + &
         ns_cla_decs%MON_MDS_NF_FUEL_LEASED_BC( &
            ns_cla_decs%ASSET_CLS_LOC,1,MO) + &
          ns_cla_decs%MON_MDS_NF_FUEL_OWNED_BC( &
          ns_cla_decs%ASSET_CLS_LOC,1,MO) + &
            ns_cla_decs%MON_MDS_CL_CLASS_FUEL_COST( &
            ns_cla_decs%ASSET_CLS_LOC,1,MO)
    MONTH_VARS(MO,Cash Secondary Sales) = &
               MONTH_VARS(MO,Cash Secondary Sales) + &
           ns_cla_decs%MON_MDS_CL_CLASS_REVENUE( &
            ns_cla_decs%ASSET_CLS_LOC,1,MO) + &
           ns_cla_decs%MON_MDS_CL_CLASS_REVENUE( &
           ns_cla_decs%ASSET_CLS_LOC,2,MO) + &
               ns_cla_decs%MON_MDS_CL_CLASS_REVENUE( &
               ns_cla_decs%ASSET_CLS_LOC,3,MO)
     MONTH_VARS(MO, &
            csh_income_rsv_mgn_cap_sales) = &
           MONTH_VARS(MO, &
               csh_income_rsv_mgn_cap_sales) &
           + ns_cla_decs%MON_MDS_CL_CAP_REVENUE( &
           ns_cla_decs%ASSET_CLS_LOC,1,MO) &
           + ns_cla_decs%MON_MDS_CL_CAP_REVENUE( &
           ns_cla_decs%ASSET_CLS_LOC,2,MO) &
           + ns_cla_decs%MON_MDS_CL_CAP_REVENUE( &
           ns_cla_decs%ASSET_CLS_LOC,3,MO)
     MONTH_VARS(MO,Cash BTL Revenues) = &
              MONTH_VARS(MO,Cash BTL Revenues) + &
           ns_cla_decs%MON_MDS_CL_CLASS_REVENUE( &
           ns_cla_decs%ASSET_CLS_LOC,4,MO) + &
              ns_cla_decs%MON_MDS_CL_CAP_REVENUE( &
              ns_cla_decs%ASSET_CLS_LOC,4,MO)
     MONTH_VARS(MO,CashICAPRevenues) = &
          MONTH_VARS(MO,CashICAPRevenues) &
           + ns_cla_decs%MON_MDS_ICAP_REV_BY_CLASS( &
           ns_cla_decs%ASSET_CLS_LOC,MO)
               ENDDO
            ENDIF
         ENDIF
      RETURN

      ENTRY GET_CL_EMISS_FOR_CLASS(R_EMISS_TYPE,R_CLASS)

         GET_CL_EMISS_FOR_CLASS = 0
         IF(R_CLASS <= ns_cla_decs%MAX_CAP_LIMITED_CLASS_ID_NUM) THEN
            IF(R_CLASS ==0) THEN
               ns_cla_decs%ASSET_CLS_LOC = 0
            ELSE
       ns_cla_decs%ASSET_CLS_LOC = ns_cla_decs%ASSET_CLASS_ptr(R_CLASS)
            ENDIF
            IF(ns_cla_decs%ASSET_CLS_LOC > 0 .OR. R_CLASS == 0) THEN
               GET_CL_EMISS_FOR_CLASS = &
        CL_ANN_CLASS_EMISSIONS(R_EMISS_TYPE,ns_cla_decs%ASSET_CLS_LOC)
            ENDIF
         ENDIF
      RETURN

      ENTRY GET_CL_EMISSIONS(R_CLASS,R_CLASS_CL_EMISSIONS)

         R_CLASS_CL_EMISSIONS(1:NUMBER_OF_EMISSION_TYPES) = 0.
         IF(R_CLASS <= ns_cla_decs%MAX_CAP_LIMITED_CLASS_ID_NUM) THEN
            IF(R_CLASS ==0) THEN
               ns_cla_decs%ASSET_CLS_LOC = 0
            ELSE
   ns_cla_decs%ASSET_CLS_LOC = ns_cla_decs%ASSET_CLASS_ptr(R_CLASS)
            ENDIF
            IF(ns_cla_decs%ASSET_CLS_LOC > 0 .OR. R_CLASS == 0) THEN
               R_CLASS_CL_EMISSIONS(1:NUMBER_OF_EMISSION_TYPES) = &
                  CL_ANN_CLASS_EMISSIONS(1:NUMBER_OF_EMISSION_TYPES, &
                                           ns_cla_decs%ASSET_CLS_LOC)
            ENDIF
         ENDIF
         GET_CL_EMISSIONS = R_CLASS_CL_EMISSIONS(1)
      RETURN

      ENTRY RETURN_CL_INTRA_CLASS_REVENUES(R_CLASS, &
                                           R_INTRA_BASE_REVENUES, &
                                           R_INTRA_ADJ_REVENUES, &
                                           R_INTRA_SALES_REVENUES, &
                                           R_INTRA_OTHER_REVENUES, &
                                           R_INTRA_COMPANY_NF_BURN)


         R_INTRA_COMPANY_NF_BURN = 0.
         IF(R_CLASS <= MAX_INTRA_CLASS) THEN
            RETURN_CL_INTRA_CLASS_REVENUES = .TRUE.
            R_INTRA_BASE_REVENUES = R_INTRA_BASE_REVENUES + &
                             INTRA_COMPANY_REVENUES(R_CLASS,1)/1000000.
            R_INTRA_ADJ_REVENUES = R_INTRA_ADJ_REVENUES + &
                             INTRA_COMPANY_REVENUES(R_CLASS,2)/1000000.
            R_INTRA_SALES_REVENUES = R_INTRA_SALES_REVENUES + &
                             INTRA_COMPANY_REVENUES(R_CLASS,3)/1000000.
            R_INTRA_OTHER_REVENUES = R_INTRA_OTHER_REVENUES + &
                             INTRA_COMPANY_REVENUES(R_CLASS,0)/1000000.
            R_INTRA_COMPANY_NF_BURN = &
                                INTRA_COMPANY_NF_BURN(R_CLASS)/1000000.

         ELSE
            RETURN_CL_INTRA_CLASS_REVENUES = .FALSE.
         ENDIF
      RETURN

      ENTRY RETURN_CL_INTRA_EXPENSES(R_INTRA_FOSSIL_FUEL_EXPENSES, &
                                     R_INTRA_LEASED_NUCLEAR_FUEL, &
                                     R_INTRA_OWNED_NUC_FUEL_EXPENSES, &
                                     R_INTRA_PURCHASE_EXPENSES, &
                                     R_INTRA_NUC_OWN_BURN, &
                                     R_INTRA_NUC_LEASE_BURN)


         RETURN_CL_INTRA_EXPENSES = .TRUE.
        R_INTRA_FOSSIL_FUEL_EXPENSES = R_INTRA_FOSSIL_FUEL_EXPENSES + &
                                    INTRA_FOSSIL_FUEL_EXPENSES/1000000.
         R_INTRA_LEASED_NUCLEAR_FUEL = R_INTRA_LEASED_NUCLEAR_FUEL + &
                                INTRA_LEASED_NUC_FUEL_EXPENSES/1000000.
         R_INTRA_OWNED_NUC_FUEL_EXPENSES = &
                                  R_INTRA_OWNED_NUC_FUEL_EXPENSES + &
                                 INTRA_OWNED_NUC_FUEL_EXPENSES/1000000.
         R_INTRA_NUC_OWN_BURN = R_INTRA_NUC_OWN_BURN + &
                                            INTRA_NUC_OWN_BURN/1000000.
         R_INTRA_NUC_OWN_BURN = R_INTRA_NUC_OWN_BURN + &
                                            INTRA_NUC_OWN_BURN/1000000.
         R_INTRA_NUC_LEASE_BURN = R_INTRA_NUC_LEASE_BURN + &
                                          INTRA_NUC_LEASE_BURN/1000000.
         R_INTRA_PURCHASE_EXPENSES = R_INTRA_PURCHASE_EXPENSES &
                                     + INTRA_PURCHASE_EXPENSES/1000000.
      RETURN
      ENTRY GET_BLOCK_CAP_FACTORS(R_LOWER_BOUND_CAP_FACTOR, &
                                  R_UPPER_BOUND_CAP_FACTOR,R_NBLOK2, &
                                  R_FACET_MAINTENANCE_RATE, &
                                  R_FUEL_INVENTORY_ID, &
                                  R_MMBTU_FUEL_BALANCE, &
                                  R_MAINTENANCE_RATE)
         DO I = 1, R_NBLOK2

            UNITNO = UNIT(I)
            R_LOWER_BOUND_CAP_FACTOR(I) = &
                                        MIN_CAP_FACTOR(UNITNO)
            R_UPPER_BOUND_CAP_FACTOR(I) = &
                                        MAX_CAP_FACTOR(UNITNO)

         ENDDO
         DO UNITNO = 1, get_nunits()
            IF(SHADOW_UNIT_NUMBER(UNITNO) /= 0) THEN
               FUEL_ID = R_FUEL_INVENTORY_ID(FUEL_SUPPLY_ID(UNITNO))
               IF(UNITNO > SHADOW_UNIT_NUMBER(UNITNO) .AND. &
                               R_MMBTU_FUEL_BALANCE(FUEL_ID) > 0.) THEN
                  R_FACET_MAINTENANCE_RATE(UNITNO) = 1.0
               ELSEIF(UNITNO < SHADOW_UNIT_NUMBER(UNITNO) .AND. &
                           R_MMBTU_FUEL_BALANCE(FUEL_ID) <= 0. D0) THEN
                  R_FACET_MAINTENANCE_RATE(UNITNO) = 1.0
               ELSE
                  R_FACET_MAINTENANCE_RATE(UNITNO) = &
                                             R_MAINTENANCE_RATE(UNITNO)
               ENDIF
            ELSE
               R_FACET_MAINTENANCE_RATE(UNITNO) = &
                                             R_MAINTENANCE_RATE(UNITNO)
            ENDIF
         ENDDO
         GET_BLOCK_CAP_FACTORS = .TRUE.
      RETURN

      ENTRY CLA_SPECIAL_ID_NAME(r_nunits_loc,R_NAME)

         R_NAME = SPL_unit_id_loc(r_nunits_loc)
         CLA_SPECIAL_ID_NAME = .TRUE.
      RETURN

END function cl_units_read

function CLA_RETURN_UNITNM(idx)
use prodcom
use debugtrace

implicit none
 character(len=20) :: cla_return_unitnm
 character (len=20) UnitNmVal
 integer (kind=2) :: idx
 integer :: file_trace_cru=0
 
    if(file_trace_cru==0) then
        file_trace_cru=open_trace("cla_return_unitnm.trace", rq_cru)
    end if
    call write_trace_int2(file_trace_cru, "idx", idx)
    call write_trace_string(file_trace_cru, "result", unitnm(idx))
    
 UnitNmVal=UnitNM(idx)
 ! Johndebug - it's believed that UNITNM has not
 ! been filled with useful data.
 CLA_RETURN_UNITNM = UNITNM(idx)
end function CLA_RETURN_UNITNM

module fuel_pointers
implicit none
contains

            function RETURN_CL_FUEL_POINTERS(R_P_BTU_COST_POINTR, &
                R_S_BTU_COST_POINTR,  &
                 R_E_BTU_COST_POINTR, upper_bound)
              use cl_object_shared
              use program_state
              use string
              use cl_units_read_data
              use prod2com
              use nunits_shared
              implicit none
              integer, intent(in) :: upper_bound
              integer (kind=2) :: RETURN_CL_FUEL_POINTERS
              INTEGER (kind=2), intent(inout) :: R_P_BTU_COST_POINTR(:)
              INTEGER (kind=2), intent(inout) :: R_S_BTU_COST_POINTR(:)
              INTEGER (kind=2), intent(inout) :: R_E_BTU_COST_POINTR(:)
              integer :: I
              if(bcp_arraysize<get_nunits()) then
                ! bcp_arraysize=----/980
                ! get_nunits()=1027/1027
                ! upper_bound=1027/asdf
                bcp_arraysize=bcp_arraysize ! Debugstop
              end if
              
              ! ubound returns negative or zero, so collecting
              ! data at allocation and putting in program_state module.
              if(BCP_arraysize<get_nunits()) then
                 ! nunits=1027/1027
                 call end_program("cl_units_read_ext:0009 - " // &
                 "Will attempt to access element " // &
                 trim(itos(int(get_nunits()))) // &
                 " of the r_p_btu_cost_pointr " // &
                 "array, which has " // itos(BCP_arraysize) // &
                 " elements.")
              end if

             
             DO I = 1, get_nunits()
                
                R_P_BTU_COST_POINTR(I) = PBTUCT_SAVE(I)
                R_S_BTU_COST_POINTR(I) = SBTUCT_SAVE(I)
                R_E_BTU_COST_POINTR(I) = EMISS_FUEL_COST_SAVE(I)
             ENDDO
             RETURN_CL_FUEL_POINTERS = get_nunits()
      
    end function RETURN_CL_FUEL_POINTERS
end module fuel_pointers