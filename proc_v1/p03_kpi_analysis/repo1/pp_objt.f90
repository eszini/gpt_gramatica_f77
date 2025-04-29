
!     PP_OBJT.FOR
!     Copyright(c) M.S. Gerber & Associates 2000
!
!     Created: 1/16/2003 10:54:43 AM
!     Author : MARK S GERBER
!     Last change: msg 7/4/2019 3:27:54 PM


      SUBROUTINE PP_OBJECT
      use filemod
      use end_routine, only: end_program, er_message
      use run_period_data
      use filename_tracker
      use mod_base_year
      use spindriftlib
      use prod_arrays_dimensions
      use sizecom
      INTEGER(kind=2) :: DELETE,YEAR,INUNIT,IREC,LRECL=622
      INTEGER :: IOS
      INTEGER(kind=2) :: UNIT_NUM=10
      INTEGER(kind=2) :: R_UNIT_NUM,I
      CHARACTER(len=5) :: BASE_FILE_NAME,OVERLAY_FAMILY_NAME, &
                  PROD_PARM_FILE,THERMAL_RETROFIT_FILE
      CHARACTER(len=3) :: TRANSFER_TO_ASSET_ANALYST ! 151
      CHARACTER(len=50) :: COMMENT
      CHARACTER(len=256) :: FILE_NAME
      CHARACTER(len=256) :: BASE_FILE_DIRECTORY,OUTPUT_DIRECTORY
      CHARACTER(len=256) :: DATA_DRIVE
      LOGICAL(kind=4) :: FILE_EXISTS=.FALSE.,R_PRODP_FILE_EXISTS, &
                RETRO_FILE_EXISTS=.FALSE.
!  DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER(len=1024) :: RECLN
!  PRODUCTION PARAMETER FILE VARIABLES
      LOGICAL(kind=1) :: LOGICAL_VAL,INCLUDE_IN_DISPATCH_ORDER,CIPSCO
      LOGICAL(kind=1) :: TRANSACT_ACTIVE_IN_OVERLAY, &
                TRANSACT_ACTIVE_IN_BASEFILE, &
                R_TRANSACT_ACTIVE_IN_ENDPOINT
      SAVE TRANSACT_ACTIVE_IN_OVERLAY, &
           TRANSACT_ACTIVE_IN_BASEFILE
      INTEGER(kind=2) :: YEARS_IN_STUDY=1,ENDYR
      INTEGER(kind=1) :: BTU_TAX_START_MONTH
      REAL :: PARM_VALUES_5(5),PARM_VALUES_10(10), &
           COMPANY_COST_ADJUSTMENT,POOL_COST_ADJUSTMENT, &
           UNSERVED_ENERGY_COST,TRANSACTION_BUY_SPREAD, &
           TRANSACTION_SELL_SPREAD,BTU_TAX_RATE, &
           POOLING_VARIABLE_COST_SWITCH, &
           POOLING_FUEL_COST_SWITCH, &
           POOLING_FIXED_COST_SWITCH, &
           OIL_MMBTU_TAX_RATE, &
           COAL_MMBTU_TAX_RATE, &
           NUCLEAR_MMBTU_TAX_RATE, &
           CAP_OTHER_1_MMBTU_TAX_RATE, &
           CAP_OTHER_2_MMBTU_TAX_RATE, &
           HYDRO_TAX_RATE, &
           HYDRO_OTHER_1_TAX_RATE, &
           HDYRO_OTHER_2_TAX_RATE, &
           BTU_TAX_IN_ADJUSTMENT_CLAUSE, &
           TIE_GROUP_LIMIT(3), &
           MARKET_PRICE_SCALAR, &
           TRANSACT_UNSERVED_COST, &
           TRANSACT_ABUNDANCE_COST, &
           GLOBAL_CAPACITY_VALUE(3), &
           GLOBAL_CAPACITY_PERCENT(3), &
           NEW_GLOBAL_CAPACITY_VALUE(7), &
           NEW_GLOBAL_CAPACITY_PERCENT(7), &
           RETIREMENTS_PROFIT_PER_KW
      real :: ADDITIONS_PROFIT_PER_MWH, &
           MAXIMUM_ANNUAL_MRX_CAP, &
           MAXIMUM_TRANSACTION_SIZE, &
           TECHNOLOGY_SCARCITY_STEP, &
           TECHNOLOGY_SCARCITY_POWER, &
           UNDER_CONSTRUCTION_PERCENT, &
           ADVANCED_DEVELOPMENT_PERCENT, &
           EARLY_DEVELOPMENT_PERCENT, &
           PROPOSED_PERCENT, &
           INDEFIN_POSTPHONED_PERCENT, &
           FUEL_AND_PURCHASE_COST_CAP, &
           SYS_EMERGENCY_MW_FLOOR, &
           SYS_EMERGENCY_COST_CAP, &
           DMD_COEFF_A, &
           DMD_COEFF_B, &
           GAS_MODEL_INTERCEPT, &
           GAS_MODEL_WTI_COEFF, &
           GAS_MODEL_DEMAND_COEFF, &
           GAS_MODEL_EPUC_COEFF, &
           GAS_MODEL_STORAGE_UTIL_COEFF, &
           MINIMUM_SIZE_OF_RETROFIT, &
           GAS_PRICE_LAG_COEFF, &
           GAS_PCA_LAGGED_PCA, &
           GAS_PCA_INTERCEPT
      REAL :: LEVELIZING_RESERVE_MARGIN, &
           RM_PURCHASE_FIXED_COST, &
           RM_PURCHASE_VARIABLE_COST, &
           MAX_SELL_RESERVE_MARGIN, &
           RM_SALE_FIXED_AMOUNT, &
           RM_SALE_VARIABLE_AMOUNT
      CHARACTER(len=21) :: FILE_TYPE='Production Parameters'
      CHARACTER(len=1) :: TRANSACTION_PRICING_METHOD,CHAR_VAL, &
                  ACCUM_MAINTENANCE,IGNORE_NON_UTILITY, &
                  REMOVE_NIGHTIME_SCARCITY,strategic_rtrmts_logic, &
                  BIDDING_LOGIC, &
                  USE_MINIMUM_RM, &
                  DETAILED_MAINTENANCE, &
                  DETAILED_FOR, &
                  DETAILED_TRANSFER_PRICING, &
                  PRICES_ARE_CAPPED, &
                  GLOBAL_SCARCITY_BUY_N_SELL, &
                  DYNAMIC_FUEL_PRICING, &
                  TECHNOLOGY_SCARCITY_BASIS, &
                  RESOURCE_TO_LOAD_ALLOC, &
                  MULTI_MARKET_ACTIVE, &
                  DISALLOW_REDUNDANT_TRANS, &
                  ALLOW_MARKET_ARBITRAGE, &
                  NET_TG_BUYS_SELLS, &
                  ICAP_MARKET_SOURCE, &
                  STRICTLY_POSITIVE_MARGIN, &
                  TRANS_ZONAL_NODAL, &
                  ACU_POWER_ID_INDEX, &
                  LEGACY_PRIMARY_MOVER, &
                  CO2_RETIREMENTS_LOGIC, &
                  DYNAMIC_GAS_EPUC, &
                  RETROFIT_LOGIC_ACTIVE, &
                  USE_AVERAGE_DAILY_DEMAND, &
                  USE_PRICE_MINIMUMS
      CHARACTER(len=2) :: PRODP_OL='BC'
!
!  FORMER RUN SPECS PRODUCTION VARIABLES
!
      INTEGER(kind=2) ::  HOURLY_LOAD_OUT,HOURLY_LOAD_IN, &
                 TRANS_GROUP_LOAD(25),MONTHLY_MAINTENANCE_PENALTY, &
                 MAX_TRANS_ITERATIONS,TECH_SCAR_MONTH_VECTOR, &
                 TRANS_AC_REV, &
                 TRANS_AC_REV_ALLOC, &
                 TRANS_AC_COST, &
                 TRANS_AC_COST_ALLOC, &
                 R_TRANS_AC_REV, &
                 R_TRANS_AC_REV_ALLOC, &
                 R_TRANS_AC_COST, &
                 R_TRANS_AC_COST_ALLOC, &
                 GAS_MODEL_WTI_ESC, &
                 GAS_MODEL_DEMAND_ESC, &
                 GAS_MODEL_EPUC_ESC, &
                 GAS_MODEL_STORAGE_UTIL_ESC, &
                 GAS_MODEL_RESIDUAL_ESC, &
                 GAS_MODEL_INFLATION_ESC, &
                 MINIMUM_YEAR_OF_RETROFIT, &
                 GAS_PCA_LAGGED_PRICE_VECTOR, & !  209
                 GAS_PCA_MONTH_VECTOR, &
                 GAS_MONTH_HH_BENCH, &
                 HH_VECTOR
      CHARACTER(len=1) :: ECON_SWITCH, & !  5/26/95. GAT. FOR ST JOE.
                  LMP_ACTIVE, &
                  GRX_RPS_MODULE_ACTIVE
      CHARACTER(len=6) :: RESOURCE_TIMING_SWITCH
      LOGICAL(kind=1) :: OFFSET_MAINTENANCE_VECTORS, &
                SET_OFFSET_MAINTENANCE_VALUE,VOID_LOGICAL
      CHARACTER(len=1) :: FIRST_BLOCK_DISP_SWITCH, &
                  CAPACITY_PLANNING_METHOD,PRODUCTION_COST_METHOD, &
                  START_PRODUCTION_COST_METHOD, &
                  ALLOCATE_FUEL_RIGHT_TO_LEFT, &
                  MARGINAL_COST_SWITCH,ADD_2_INCOME_STATEMENT, &
                  PRODUCTION_PERIODS_CHR, &
                  START_PRODUCTION_PERIODS, &
                  USE_EXT_LOAD_SOURCE_ACTIVE, &
                  RUN_TRANSACT, &
                  APPLY_TRANS_REV, &
                  TRANSACTION_MODE, &
                  TRANS_TIME_FRAME, &
                  CALANDER_CORRECT_OUTPUT, &
                  SECOND_FUEL_EMIS_DISPATCH, &
                  TRANS_REPRESENTATION, &
                  MONTHLY_TRANSACT_SWITCH(12), &
                  UNIT_COMMITMENT_LOGIC, &
                  ADVANCED_PRICE_MODELING, &
                  PRICE_LAST_MW, &
                  FOR_SEED_OPTIONS, &
                  LONG_PATH, &
                  REGIONAL_MAINTENANCE, &
                  DEPTH_OF_MARKET, &
                  USE_5X16, &
                  TURN_OFF_POLY, &
                  NEW_BUILD_ADDITIONS_ACTIVE, &
                  DECOMMIT_THERMAL_RESOURCES, &
                  REGIONAL_RESERVE_MARGINS, &
                  PLAN_TO_MWH_OR_KWYR, &
                  EMERGENCY_MEETS_SPIN, &
                  INCLUDE_ICAP_REVENUE, &
                  REMEMBER_LAST_HOUR, &
                  COMMIT_ON_TOTAL_COST, &
                  USE_EMIS_IN_MRX, &
                  FAST_DISPATCH, &
                  RUN_GAS_MODEL, &
                  RUN_COAL_MODEL, &
                  REGION_OR_STATE_POWER_MAP, &
                  LP_GAS_MODEL
      INTEGER(kind=2) :: ASSET_CLASS_ID,ASSET_CLASS_VECTOR
      REAL :: DOE_NF_DISPOSAL_RATES(2), &
           NUC_DECOMMISSIONING_RATES(2), &
           DOE_R300_DISPOSAL_FEE(2)
      LOGICAL(kind=1) :: LAHEY_LF95
      CHARACTER(len=30) :: SCREEN_OUTPUT
!

!
!    ROUTINE TO CONVERT PRODUCTION PARAMETER FILE DATA
!
!                              COPYRIGHT (C) 1992
!                         M.S. GERBER & ASSOCIATES, INC.
!                              ALL RIGHTS RESERVED
!

!
!  CONVERT THE PRODUCTION-PARAMETERS FILE
      ENTRY PP_MAKEBIN
      DATA_DRIVE = OUTPUT_DIRECTORY()
      BASE_FILE_NAME = THERMAL_RETROFIT_FILE()
      FILE_NAME = trim(BASE_FILE_DIRECTORY())// &
                                   "RTB"//trim(BASE_FILE_NAME)//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=RETRO_FILE_EXISTS)

      BASE_FILE_NAME = PROD_PARM_FILE()
      FILE_NAME = get_ppb_filename(base_file_name)
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      YEARS_IN_STUDY = get_endyr() - get_base_year()
      TRANSACT_ACTIVE_IN_BASEFILE = .FALSE.
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_LOCATE_WRITE(16,30,BASE_FILE_NAME,ALL_VERSIONS,0)
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         ENDIF
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCPRODP.BIN",ACCESS="DIRECT", &
                                            STATUS="REPLACE",RECL=LRECL)
         IREC = 1
         TRANSACTION_BUY_SPREAD = 0.
         TRANSACTION_SELL_SPREAD = 0.
         BTU_TAX_RATE = 0.
         BTU_TAX_IN_ADJUSTMENT_CLAUSE = 0.
         INCLUDE_IN_DISPATCH_ORDER = .FALSE.
         UNSERVED_ENERGY_COST = 0.
         POOLING_VARIABLE_COST_SWITCH = 1.
         IF(CIPSCO()) POOLING_VARIABLE_COST_SWITCH = 0.
         POOLING_FUEL_COST_SWITCH = 0.
         POOLING_FIXED_COST_SWITCH = 0.
         OIL_MMBTU_TAX_RATE = 0.
         COAL_MMBTU_TAX_RATE = 0.
         NUCLEAR_MMBTU_TAX_RATE = 0.
         CAP_OTHER_1_MMBTU_TAX_RATE = 0.
         CAP_OTHER_2_MMBTU_TAX_RATE = 0.
         HYDRO_TAX_RATE = 0.
         HYDRO_OTHER_1_TAX_RATE = 0.
         HDYRO_OTHER_2_TAX_RATE = 0.
         BTU_TAX_START_MONTH = 0
         BTU_TAX_IN_ADJUSTMENT_CLAUSE = 0.
         TIE_GROUP_LIMIT(1) = 999999.
         TIE_GROUP_LIMIT(2) = 999999.
         TIE_GROUP_LIMIT(3) = 999999.
         LEVELIZING_RESERVE_MARGIN = 0.
         RM_PURCHASE_FIXED_COST = 0.
         RM_PURCHASE_VARIABLE_COST = 0.
         MAX_SELL_RESERVE_MARGIN = 0.
         RM_SALE_FIXED_AMOUNT = 0.
         RM_SALE_VARIABLE_AMOUNT = 0.
!
!  RUN SPECS VARIABLES
!
         PRODUCTION_COST_METHOD = START_PRODUCTION_COST_METHOD()
         PRODUCTION_PERIODS_CHR = START_PRODUCTION_PERIODS()
         RESOURCE_TIMING_SWITCH = "ADJUST"
         FIRST_BLOCK_DISP_SWITCH = 'I'
         ECON_SWITCH = 'F'
         OFFSET_MAINTENANCE_VECTORS = .FALSE.
         CAPACITY_PLANNING_METHOD = 'C'
         HOURLY_LOAD_OUT = 0
         HOURLY_LOAD_IN = 0
         ALLOCATE_FUEL_RIGHT_TO_LEFT = 'L'
         ADD_2_INCOME_STATEMENT = 'F'
         ASSET_CLASS_ID = 1
         ASSET_CLASS_VECTOR = 0
         DOE_NF_DISPOSAL_RATES(1) = 0.
         DOE_NF_DISPOSAL_RATES(2) = 0.
         NUC_DECOMMISSIONING_RATES(1) = 0.
         NUC_DECOMMISSIONING_RATES(2) = 0.
         DOE_R300_DISPOSAL_FEE(1) = 0.
         DOE_R300_DISPOSAL_FEE(2) = 0.
         USE_EXT_LOAD_SOURCE_ACTIVE = 'N'
         RUN_TRANSACT = 'F'
         APPLY_TRANS_REV = 'W'
         TRANSACTION_MODE = 'E'
         TRANS_TIME_FRAME = 'M'
         SECOND_FUEL_EMIS_DISPATCH = 'F'
         CALANDER_CORRECT_OUTPUT = 'T'
         MARKET_PRICE_SCALAR = 1.
         TRANSACT_UNSERVED_COST = 999999.
         TRANSACT_ABUNDANCE_COST = -99.
         TRANS_REPRESENTATION = 'P'
         IGNORE_NON_UTILITY = 'F'
         REMOVE_NIGHTIME_SCARCITY = 'F'
         MONTHLY_MAINTENANCE_PENALTY = 0
         MAX_TRANS_ITERATIONS = 100
         TECH_SCAR_MONTH_VECTOR = 0
!
         MONTHLY_TRANSACT_SWITCH(1) = 'T'
         MONTHLY_TRANSACT_SWITCH(2) = 'T'
         MONTHLY_TRANSACT_SWITCH(3) = 'T'
         MONTHLY_TRANSACT_SWITCH(4) = 'T'
         MONTHLY_TRANSACT_SWITCH(5) = 'T'
         MONTHLY_TRANSACT_SWITCH(6) = 'T'
         MONTHLY_TRANSACT_SWITCH(7) = 'T'
         MONTHLY_TRANSACT_SWITCH(8) = 'T'
         MONTHLY_TRANSACT_SWITCH(9) = 'T'
         MONTHLY_TRANSACT_SWITCH(10) = 'T'
         MONTHLY_TRANSACT_SWITCH(11) = 'T'
         MONTHLY_TRANSACT_SWITCH(12) = 'T'
         GLOBAL_CAPACITY_VALUE(1) = 0.
         GLOBAL_CAPACITY_VALUE(2) = 0.
         GLOBAL_CAPACITY_VALUE(3) = 0.
         GLOBAL_CAPACITY_PERCENT(1) = 50.
         GLOBAL_CAPACITY_PERCENT(2) = 80.
         GLOBAL_CAPACITY_PERCENT(3) = 100.
         NEW_GLOBAL_CAPACITY_VALUE(1) = 0.
         NEW_GLOBAL_CAPACITY_VALUE(2) = 0.
         NEW_GLOBAL_CAPACITY_VALUE(3) = 0.
         NEW_GLOBAL_CAPACITY_VALUE(4) = 0.
         NEW_GLOBAL_CAPACITY_VALUE(5) = 0.
         NEW_GLOBAL_CAPACITY_VALUE(6) = 0.
         NEW_GLOBAL_CAPACITY_VALUE(7) = 0.
         NEW_GLOBAL_CAPACITY_PERCENT(1) = 0.
         NEW_GLOBAL_CAPACITY_PERCENT(2) = 0.
         NEW_GLOBAL_CAPACITY_PERCENT(3) = 0.
         NEW_GLOBAL_CAPACITY_PERCENT(4) = 0.
         NEW_GLOBAL_CAPACITY_PERCENT(5) = 0.
         NEW_GLOBAL_CAPACITY_PERCENT(6) = 0.
         NEW_GLOBAL_CAPACITY_PERCENT(7) = 0.
!
         strategic_rtrmts_logic = 'F'
         BIDDING_LOGIC = 'F'
         USE_MINIMUM_RM = 'F'
         DETAILED_MAINTENANCE = 'F'
         DETAILED_FOR = 'F'
         DETAILED_TRANSFER_PRICING = 'F'
         PRICES_ARE_CAPPED = 'F'
         GLOBAL_SCARCITY_BUY_N_SELL = 'F'
         DYNAMIC_FUEL_PRICING = 'F'
         RETIREMENTS_PROFIT_PER_KW = 0.
         ADDITIONS_PROFIT_PER_MWH = 0.
         MAXIMUM_ANNUAL_MRX_CAP = 999999.
!
         TRANS_AC_REV = 0
         TRANS_AC_REV_ALLOC = 0
         TRANS_AC_COST = 0
         TRANS_AC_COST_ALLOC = 0
!
         UNIT_COMMITMENT_LOGIC = 'F'
         ADVANCED_PRICE_MODELING = 'F'
         PRICE_LAST_MW = 'F'
         FOR_SEED_OPTIONS = 'Y'
         LONG_PATH = 'F'
         REGIONAL_MAINTENANCE = 'F'
         DEPTH_OF_MARKET = 'F'
         USE_5X16 = 'T'
         TURN_OFF_POLY = 'F'
         MAXIMUM_TRANSACTION_SIZE = 999999.
         TRANSFER_TO_ASSET_ANALYST = 'Jan' ! 151
!
         TECHNOLOGY_SCARCITY_BASIS = 'A'
         TECHNOLOGY_SCARCITY_STEP = 0.0
         TECHNOLOGY_SCARCITY_POWER = 2.0
!
         NEW_BUILD_ADDITIONS_ACTIVE = 'F'
         UNDER_CONSTRUCTION_PERCENT = 100.
         ADVANCED_DEVELOPMENT_PERCENT = 100.
         EARLY_DEVELOPMENT_PERCENT = 100.
         PROPOSED_PERCENT = 100.
         INDEFIN_POSTPHONED_PERCENT = 100.
!
         DECOMMIT_THERMAL_RESOURCES = 'T'
         REGIONAL_RESERVE_MARGINS = 'F'
         PLAN_TO_MWH_OR_KWYR = 'M'
         EMERGENCY_MEETS_SPIN = 'T'
         INCLUDE_ICAP_REVENUE = 'F'
         REMEMBER_LAST_HOUR = 'T'
         COMMIT_ON_TOTAL_COST = 'F'
         USE_EMIS_IN_MRX = 'F'
         FAST_DISPATCH = 'T'
         RUN_GAS_MODEL = 'F'
         RUN_COAL_MODEL = 'F'
         REGION_OR_STATE_POWER_MAP = 'R'
         LP_GAS_MODEL = 'F'
         GAS_MODEL_WTI_ESC = 0
         GAS_MODEL_DEMAND_ESC = 0
         GAS_MODEL_EPUC_ESC = 0
         GAS_MODEL_STORAGE_UTIL_ESC = 0
         GAS_MODEL_RESIDUAL_ESC = 0
         GAS_MODEL_INFLATION_ESC = 0
         GAS_MODEL_WTI_COEFF = 0.
         GAS_MODEL_INTERCEPT = 0.
         GAS_MODEL_DEMAND_COEFF = 0.
         GAS_MODEL_EPUC_COEFF = 0.
         GAS_MODEL_STORAGE_UTIL_COEFF = 0.
!
         TRANS_GROUP_LOAD = 0
         RESOURCE_TO_LOAD_ALLOC = 'M'
         FUEL_AND_PURCHASE_COST_CAP = 999999.
         SYS_EMERGENCY_MW_FLOOR = 0.
         SYS_EMERGENCY_COST_CAP = 999999.
         MULTI_MARKET_ACTIVE = 'F'
         DISALLOW_REDUNDANT_TRANS = 'F'
         ALLOW_MARKET_ARBITRAGE = 'F'
         DMD_COEFF_A = -9999.
         DMD_COEFF_B = 0.0
         NET_TG_BUYS_SELLS = 'F'
         ICAP_MARKET_SOURCE = 'I'
         STRICTLY_POSITIVE_MARGIN = 'F'
         TRANS_ZONAL_NODAL = 'T'
         ACU_POWER_ID_INDEX = 'F'
         LEGACY_PRIMARY_MOVER = 'F'
         CO2_RETIREMENTS_LOGIC = 'F'
         DYNAMIC_GAS_EPUC = 'F'
         RETROFIT_LOGIC_ACTIVE = 'F'
         MINIMUM_SIZE_OF_RETROFIT = 999999.0
         MINIMUM_YEAR_OF_RETROFIT = 2100
         USE_AVERAGE_DAILY_DEMAND = 'F'
         GAS_PRICE_LAG_COEFF = 0.0
         GAS_PCA_LAGGED_PRICE_VECTOR = 0
         GAS_PCA_LAGGED_PCA = 0.0
         GAS_PCA_MONTH_VECTOR = 0
         GAS_MONTH_HH_BENCH = 0
         HH_VECTOR = 1
         USE_PRICE_MINIMUMS = 'F'
         GAS_PCA_INTERCEPT = 0.0
         LMP_ACTIVE = 'N'
         GRX_RPS_MODULE_ACTIVE = 'F'
         READ(10,"(A)") RECLN
         DO
            READ(10,'(A)',IOSTAT=IOS) RECLN
            IF(IOS /= 0) EXIT
            RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
                                   ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
                                   ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
                                   ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
                                   
            TRANSACTION_PRICING_METHOD="X" ! lahey spurious warning
            POOL_COST_ADJUSTMENT=0. ! lahey spurious warning
            COMPANY_COST_ADJUSTMENT=0. ! lahey spurious warning
            READ(RECLN,*,ERR=200) DELETE,YEAR, &
               TRANSACTION_PRICING_METHOD, COMPANY_COST_ADJUSTMENT, &
               POOL_COST_ADJUSTMENT, TRANSACTION_BUY_SPREAD, &
               TRANSACTION_SELL_SPREAD, BTU_TAX_RATE, &
               INCLUDE_IN_DISPATCH_ORDER, UNSERVED_ENERGY_COST, &
               POOLING_VARIABLE_COST_SWITCH, POOLING_FUEL_COST_SWITCH, &
               POOLING_FIXED_COST_SWITCH, OIL_MMBTU_TAX_RATE, &
               COAL_MMBTU_TAX_RATE, NUCLEAR_MMBTU_TAX_RATE, &
       CAP_OTHER_1_MMBTU_TAX_RATE,  CAP_OTHER_2_MMBTU_TAX_RATE, &
             HYDRO_TAX_RATE, HYDRO_OTHER_1_TAX_RATE, &
             HDYRO_OTHER_2_TAX_RATE, BTU_TAX_START_MONTH, &
             BTU_TAX_IN_ADJUSTMENT_CLAUSE, TIE_GROUP_LIMIT, &
             LEVELIZING_RESERVE_MARGIN,  RM_PURCHASE_FIXED_COST, &
             RM_PURCHASE_VARIABLE_COST,   MAX_SELL_RESERVE_MARGIN, &
             RM_SALE_FIXED_AMOUNT,   RM_SALE_VARIABLE_AMOUNT, &
             FIRST_BLOCK_DISP_SWITCH,    ECON_SWITCH, &
             CAPACITY_PLANNING_METHOD, &
             PRODUCTION_COST_METHOD,  OFFSET_MAINTENANCE_VECTORS, &
             HOURLY_LOAD_OUT,   HOURLY_LOAD_IN, &
             ALLOCATE_FUEL_RIGHT_TO_LEFT,  RESOURCE_TIMING_SWITCH, &
             MARGINAL_COST_SWITCH,  COMMENT, &
             ADD_2_INCOME_STATEMENT,   ASSET_CLASS_ID, &
             ASSET_CLASS_VECTOR,  PRODUCTION_PERIODS_CHR, &
             DOE_NF_DISPOSAL_RATES, NUC_DECOMMISSIONING_RATES, &
             ACCUM_MAINTENANCE,  USE_EXT_LOAD_SOURCE_ACTIVE, &
             DOE_R300_DISPOSAL_FEE,  RUN_TRANSACT, &
             CALANDER_CORRECT_OUTPUT,  (TRANS_GROUP_LOAD(I),I=1,6), & !  SIX VALUES
             SECOND_FUEL_EMIS_DISPATCH, (TRANS_GROUP_LOAD(I),I=7,25), & !  19 VALUES
             MARKET_PRICE_SCALAR, APPLY_TRANS_REV,  TRANSACTION_MODE, &
             TRANS_TIME_FRAME, TRANS_REPRESENTATION, IGNORE_NON_UTILITY, &
             MONTHLY_MAINTENANCE_PENALTY, MONTHLY_TRANSACT_SWITCH, & !  12 VALUES
             TRANSACT_UNSERVED_COST, GLOBAL_CAPACITY_VALUE, & !  3 VALUES
             GLOBAL_CAPACITY_PERCENT, TRANSACT_ABUNDANCE_COST, & !  109
             REMOVE_NIGHTIME_SCARCITY, MAX_TRANS_ITERATIONS, & !  111
             NEW_GLOBAL_CAPACITY_VALUE, NEW_GLOBAL_CAPACITY_PERCENT, & !  7 VALUES
             strategic_rtrmts_logic, RETIREMENTS_PROFIT_PER_KW, &
             ADDITIONS_PROFIT_PER_MWH,  UNIT_COMMITMENT_LOGIC, &
             BIDDING_LOGIC,  MAXIMUM_ANNUAL_MRX_CAP, USE_MINIMUM_RM, &
             DETAILED_MAINTENANCE, DETAILED_FOR, DETAILED_TRANSFER_PRICING, &
             PRICES_ARE_CAPPED,  GLOBAL_SCARCITY_BUY_N_SELL,DYNAMIC_FUEL_PRICING, &
             TRANS_AC_REV, TRANS_AC_REV_ALLOC,  TRANS_AC_COST, &
             TRANS_AC_COST_ALLOC, ADVANCED_PRICE_MODELING, PRICE_LAST_MW, &
             FOR_SEED_OPTIONS, LONG_PATH, REGIONAL_MAINTENANCE, &
             DEPTH_OF_MARKET,  USE_5X16, MAXIMUM_TRANSACTION_SIZE, &
             TRANSFER_TO_ASSET_ANALYST, TURN_OFF_POLY, TECHNOLOGY_SCARCITY_BASIS, &
             TECHNOLOGY_SCARCITY_STEP, TECHNOLOGY_SCARCITY_POWER, &
             NEW_BUILD_ADDITIONS_ACTIVE,UNDER_CONSTRUCTION_PERCENT, &
             ADVANCED_DEVELOPMENT_PERCENT, EARLY_DEVELOPMENT_PERCENT, &
             PROPOSED_PERCENT, INDEFIN_POSTPHONED_PERCENT, &
             DECOMMIT_THERMAL_RESOURCES,  REGIONAL_RESERVE_MARGINS, &
             PLAN_TO_MWH_OR_KWYR,EMERGENCY_MEETS_SPIN, TECH_SCAR_MONTH_VECTOR, &
             INCLUDE_ICAP_REVENUE, REMEMBER_LAST_HOUR,COMMIT_ON_TOTAL_COST, &
             RESOURCE_TO_LOAD_ALLOC, FUEL_AND_PURCHASE_COST_CAP, &
             SYS_EMERGENCY_MW_FLOOR, SYS_EMERGENCY_COST_CAP, &
             MULTI_MARKET_ACTIVE,   DISALLOW_REDUNDANT_TRANS, &
             ALLOW_MARKET_ARBITRAGE, DMD_COEFF_A, DMD_COEFF_B, NET_TG_BUYS_SELLS, &
             ICAP_MARKET_SOURCE, STRICTLY_POSITIVE_MARGIN, TRANS_ZONAL_NODAL,  &
             USE_EMIS_IN_MRX, FAST_DISPATCH, RUN_GAS_MODEL, &
             REGION_OR_STATE_POWER_MAP,  LP_GAS_MODEL, GAS_MODEL_WTI_ESC, &
             GAS_MODEL_DEMAND_ESC,GAS_MODEL_EPUC_ESC, GAS_MODEL_STORAGE_UTIL_ESC, &
             GAS_MODEL_INTERCEPT,GAS_MODEL_WTI_COEFF, GAS_MODEL_DEMAND_COEFF, &
             GAS_MODEL_EPUC_COEFF,GAS_MODEL_STORAGE_UTIL_COEFF, &
             ACU_POWER_ID_INDEX, LEGACY_PRIMARY_MOVER, GAS_MODEL_RESIDUAL_ESC, &
             GAS_MODEL_INFLATION_ESC, CO2_RETIREMENTS_LOGIC, &
             DYNAMIC_GAS_EPUC, RUN_COAL_MODEL, &
             RETROFIT_LOGIC_ACTIVE, MINIMUM_SIZE_OF_RETROFIT, &
             MINIMUM_YEAR_OF_RETROFIT,   USE_AVERAGE_DAILY_DEMAND, &
             GAS_PRICE_LAG_COEFF, &
             GAS_PCA_LAGGED_PRICE_VECTOR, & !  209
             GAS_PCA_LAGGED_PCA, &
             GAS_PCA_MONTH_VECTOR, &
             GAS_PCA_INTERCEPT, &
             GAS_MONTH_HH_BENCH, &
             USE_PRICE_MINIMUMS, &
             HH_VECTOR,                  & !  215
             LMP_ACTIVE, &
             GRX_RPS_MODULE_ACTIVE

!
!
            IF(.NOT. RETRO_FILE_EXISTS) RETROFIT_LOGIC_ACTIVE = 'F'
            IF(IREC <= YEARS_IN_STUDY) THEN
               TRANSACT_ACTIVE_IN_BASEFILE = RUN_TRANSACT /= 'F' .OR. &
                                             TRANSACT_ACTIVE_IN_BASEFILE
            ENDIF
            WRITE(11,REC=IREC) TRANSACTION_PRICING_METHOD, &
                               COMPANY_COST_ADJUSTMENT, &
                               POOL_COST_ADJUSTMENT, &
                               TRANSACTION_BUY_SPREAD, &
                               TRANSACTION_SELL_SPREAD, &
                               BTU_TAX_RATE, &
                               UNSERVED_ENERGY_COST, &
                               POOLING_VARIABLE_COST_SWITCH, &
                               INCLUDE_IN_DISPATCH_ORDER, &
                               POOLING_FUEL_COST_SWITCH, &
                               POOLING_FIXED_COST_SWITCH, &
                               OIL_MMBTU_TAX_RATE, &
                               COAL_MMBTU_TAX_RATE, &
                               NUCLEAR_MMBTU_TAX_RATE, &
                               CAP_OTHER_1_MMBTU_TAX_RATE, &
                               CAP_OTHER_2_MMBTU_TAX_RATE, &
                               HYDRO_TAX_RATE, &
                               HYDRO_OTHER_1_TAX_RATE, &
                               HDYRO_OTHER_2_TAX_RATE, &
                               BTU_TAX_START_MONTH, &
                               BTU_TAX_IN_ADJUSTMENT_CLAUSE, &
                               TIE_GROUP_LIMIT, &
                               LEVELIZING_RESERVE_MARGIN, &
                               RM_PURCHASE_FIXED_COST, &
                               RM_PURCHASE_VARIABLE_COST, &
                               MAX_SELL_RESERVE_MARGIN, &
                               RM_SALE_FIXED_AMOUNT, &
                               RM_SALE_VARIABLE_AMOUNT, &
! RUN SPECS VARIABLE
                               FIRST_BLOCK_DISP_SWITCH, &
                               ECON_SWITCH, &
                               CAPACITY_PLANNING_METHOD, &
                               PRODUCTION_COST_METHOD, &
                               OFFSET_MAINTENANCE_VECTORS, &
                               HOURLY_LOAD_OUT, &
                               HOURLY_LOAD_IN, &
                               ALLOCATE_FUEL_RIGHT_TO_LEFT, &
                               RESOURCE_TIMING_SWITCH, &
                               MARGINAL_COST_SWITCH, &
                               ADD_2_INCOME_STATEMENT, &
                               ASSET_CLASS_ID, &
                               ASSET_CLASS_VECTOR, &
                               PRODUCTION_PERIODS_CHR, &
                               DOE_NF_DISPOSAL_RATES, &
                               NUC_DECOMMISSIONING_RATES, &
                               ACCUM_MAINTENANCE, &
                               USE_EXT_LOAD_SOURCE_ACTIVE, &
                               DOE_R300_DISPOSAL_FEE, &
                               RUN_TRANSACT, &
                               CALANDER_CORRECT_OUTPUT, &
                               (TRANS_GROUP_LOAD(I),I=1,6), & !  SIX VALUES
                               SECOND_FUEL_EMIS_DISPATCH, &
                               (TRANS_GROUP_LOAD(I),I=7,25), & !  19 VALUES
                               MARKET_PRICE_SCALAR, &
                               APPLY_TRANS_REV, &
                               TRANSACTION_MODE, &
                               TRANS_TIME_FRAME, &
                               TRANS_REPRESENTATION, &
                               IGNORE_NON_UTILITY, &
                               MONTHLY_MAINTENANCE_PENALTY, &
                               MONTHLY_TRANSACT_SWITCH, & !  12 VALUES
                               TRANSACT_UNSERVED_COST, &
                               GLOBAL_CAPACITY_VALUE, & !  3 VALUES
                               GLOBAL_CAPACITY_PERCENT, & !  3 VALUES
                               TRANSACT_ABUNDANCE_COST, &
                               REMOVE_NIGHTIME_SCARCITY, &
                               MAX_TRANS_ITERATIONS, &
                               NEW_GLOBAL_CAPACITY_VALUE, & !  7 VALUES
                               NEW_GLOBAL_CAPACITY_PERCENT, & !  7 VALUES
                               strategic_rtrmts_logic, &
                               RETIREMENTS_PROFIT_PER_KW, &
                               ADDITIONS_PROFIT_PER_MWH, &
                               UNIT_COMMITMENT_LOGIC, &
                               BIDDING_LOGIC, &
                               MAXIMUM_ANNUAL_MRX_CAP, &
                               USE_MINIMUM_RM, &
                               DETAILED_MAINTENANCE, &
                               DETAILED_FOR, &
                               DETAILED_TRANSFER_PRICING, &
                               PRICES_ARE_CAPPED, &
                               GLOBAL_SCARCITY_BUY_N_SELL, &
                               DYNAMIC_FUEL_PRICING, &
                               TRANS_AC_REV, &
                               TRANS_AC_REV_ALLOC, &
                               TRANS_AC_COST, &
                               TRANS_AC_COST_ALLOC, &
                               ADVANCED_PRICE_MODELING, &
                               PRICE_LAST_MW, &
                               FOR_SEED_OPTIONS, &
                               LONG_PATH, &
                               REGIONAL_MAINTENANCE, &
                               DEPTH_OF_MARKET, &
                               USE_5X16, &
                               MAXIMUM_TRANSACTION_SIZE, &
                               TRANSFER_TO_ASSET_ANALYST, & !  151
                               TURN_OFF_POLY, &
                               TECHNOLOGY_SCARCITY_BASIS, &
                               TECHNOLOGY_SCARCITY_STEP, &
                               TECHNOLOGY_SCARCITY_POWER, &
                               NEW_BUILD_ADDITIONS_ACTIVE, &
                               UNDER_CONSTRUCTION_PERCENT, &
                               ADVANCED_DEVELOPMENT_PERCENT, &
                               EARLY_DEVELOPMENT_PERCENT, &
                               PROPOSED_PERCENT, &
                               INDEFIN_POSTPHONED_PERCENT, &
                               DECOMMIT_THERMAL_RESOURCES, &
                               REGIONAL_RESERVE_MARGINS, &
                               PLAN_TO_MWH_OR_KWYR, &
                               EMERGENCY_MEETS_SPIN, &
                               TECH_SCAR_MONTH_VECTOR, &
                               INCLUDE_ICAP_REVENUE, &
                               REMEMBER_LAST_HOUR, &
                               COMMIT_ON_TOTAL_COST, &
                               RESOURCE_TO_LOAD_ALLOC, &
                               FUEL_AND_PURCHASE_COST_CAP, &
                               SYS_EMERGENCY_MW_FLOOR, &
                               SYS_EMERGENCY_COST_CAP, &
                               MULTI_MARKET_ACTIVE, &
                               DISALLOW_REDUNDANT_TRANS, &
                               ALLOW_MARKET_ARBITRAGE, &
                               DMD_COEFF_A, &
                               DMD_COEFF_B, &
                               NET_TG_BUYS_SELLS, &
                               ICAP_MARKET_SOURCE, &
                               STRICTLY_POSITIVE_MARGIN, &
                               TRANS_ZONAL_NODAL, &
                               USE_EMIS_IN_MRX, &
                               FAST_DISPATCH, &
                               RUN_GAS_MODEL, &
                               REGION_OR_STATE_POWER_MAP, &
                               LP_GAS_MODEL, &
                               GAS_MODEL_WTI_ESC, &
                               GAS_MODEL_DEMAND_ESC, &
                               GAS_MODEL_EPUC_ESC, &
                               GAS_MODEL_STORAGE_UTIL_ESC, &
                               GAS_MODEL_INTERCEPT, &
                               GAS_MODEL_WTI_COEFF, &
                               GAS_MODEL_DEMAND_COEFF, &
                               GAS_MODEL_EPUC_COEFF, &
                               GAS_MODEL_STORAGE_UTIL_COEFF, &
                               ACU_POWER_ID_INDEX, &
                               LEGACY_PRIMARY_MOVER, &
                               GAS_MODEL_RESIDUAL_ESC, &
                               GAS_MODEL_INFLATION_ESC, &
                               CO2_RETIREMENTS_LOGIC, &
                               DYNAMIC_GAS_EPUC, &
                               RUN_COAL_MODEL, &
                               RETROFIT_LOGIC_ACTIVE, &
                               MINIMUM_SIZE_OF_RETROFIT, &
                               MINIMUM_YEAR_OF_RETROFIT, &
                               USE_AVERAGE_DAILY_DEMAND, &
                               GAS_PRICE_LAG_COEFF, &
                               GAS_PCA_LAGGED_PRICE_VECTOR, & !  209
                               GAS_PCA_LAGGED_PCA, &
                               GAS_PCA_MONTH_VECTOR, &
                               GAS_PCA_INTERCEPT, &
                               GAS_MONTH_HH_BENCH, &
                               USE_PRICE_MINIMUMS, &
                               HH_VECTOR, &
                               LMP_ACTIVE, &
                               GRX_RPS_MODULE_ACTIVE
            IF(IREC == 1) VOID_LOGICAL = &
                SET_OFFSET_MAINTENANCE_VALUE(OFFSET_MAINTENANCE_VECTORS)
            IREC = IREC + 1
         ENDDO
         CLOSE(10)
         CLOSE(11)
      ELSE IF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "pp_objt:1111")
      ENDIF
      RETURN
!  OVERLAY THE PRODUCTION-PARAMETERS FILE

      ENTRY PP_MAKEOVL(OVERLAY_FAMILY_NAME)


      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME = get_ppo_filename(data_drive, overlay_family_name)
      if(.not. does_file_exist(file_name)) then
        call end_program("pp_objt:0001 - File " // trim(file_name) // &
      " doesn't exist.")
      end if
      OPEN(10,FILE=FILE_NAME)
      READ(10,*) DELETE
      INUNIT = 12
      IF(PRODP_OL == 'BC') THEN
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCPRODP.BIN", &
                                             ACCESS="DIRECT",RECL=LRECL)
         INUNIT = 11
      ENDIF
      OPEN(12,FILE=trim(DATA_DRIVE)//"OLPRODP.BIN",ACCESS="DIRECT", &
                                            STATUS="UNKNOWN",RECL=LRECL)
      IREC = 0
      DO
         IREC = IREC + 1
         READ(INUNIT,REC=IREC,IOSTAT=IOS) CHAR_VAL,PARM_VALUES_5, &
                                        UNSERVED_ENERGY_COST, &
                                        POOLING_VARIABLE_COST_SWITCH, &
                                        LOGICAL_VAL, &
                                        PARM_VALUES_10, &
                                        BTU_TAX_START_MONTH, &
                                        BTU_TAX_IN_ADJUSTMENT_CLAUSE, &
                                        TIE_GROUP_LIMIT, &
                                        LEVELIZING_RESERVE_MARGIN, &
                                        RM_PURCHASE_FIXED_COST, &
                                        RM_PURCHASE_VARIABLE_COST, &
                                        MAX_SELL_RESERVE_MARGIN, &
                                        RM_SALE_FIXED_AMOUNT, &
                                        RM_SALE_VARIABLE_AMOUNT, &
! RUN SPECS VARIABLE & 
                                        FIRST_BLOCK_DISP_SWITCH, &
                                        ECON_SWITCH, &
                                        CAPACITY_PLANNING_METHOD, &
                                        PRODUCTION_COST_METHOD, &
                                        OFFSET_MAINTENANCE_VECTORS, &
                                        HOURLY_LOAD_OUT, &
                                        HOURLY_LOAD_IN, &
                                        ALLOCATE_FUEL_RIGHT_TO_LEFT, &
                                        RESOURCE_TIMING_SWITCH, &
                                        MARGINAL_COST_SWITCH, &
                                        ADD_2_INCOME_STATEMENT, &
                                        ASSET_CLASS_ID, &
                                        ASSET_CLASS_VECTOR, &
                                        PRODUCTION_PERIODS_CHR, &
                                        DOE_NF_DISPOSAL_RATES, &
                                        NUC_DECOMMISSIONING_RATES, &
                                        ACCUM_MAINTENANCE, &
                                        USE_EXT_LOAD_SOURCE_ACTIVE, &
                                        DOE_R300_DISPOSAL_FEE, &
                                        RUN_TRANSACT, &
                                        CALANDER_CORRECT_OUTPUT, &
                                        (TRANS_GROUP_LOAD(I),I=1,6), & !  SIX VALUES
                                        SECOND_FUEL_EMIS_DISPATCH, &
                                        (TRANS_GROUP_LOAD(I),I=7,25), & !  19 VALUES
                                        MARKET_PRICE_SCALAR, &
                                        APPLY_TRANS_REV, &
                                        TRANSACTION_MODE, &
                                        TRANS_TIME_FRAME, &
                                        TRANS_REPRESENTATION, &
                                        IGNORE_NON_UTILITY, &
                                        MONTHLY_MAINTENANCE_PENALTY, &
                                        MONTHLY_TRANSACT_SWITCH, & !  12 VALUES
                                        TRANSACT_UNSERVED_COST, &
                                        GLOBAL_CAPACITY_VALUE, & !  3 VALUES
                                        GLOBAL_CAPACITY_PERCENT, & !  3 VALUES
                                        TRANSACT_ABUNDANCE_COST, &
                                        REMOVE_NIGHTIME_SCARCITY, &
                                        MAX_TRANS_ITERATIONS, &
                                        NEW_GLOBAL_CAPACITY_VALUE, & !  7 VALUES
                                        NEW_GLOBAL_CAPACITY_PERCENT, & !  7 VALUES
                                        strategic_rtrmts_logic, &
                                        RETIREMENTS_PROFIT_PER_KW, &
                                        ADDITIONS_PROFIT_PER_MWH, &
                                        UNIT_COMMITMENT_LOGIC, &
                                        BIDDING_LOGIC, &
                                        MAXIMUM_ANNUAL_MRX_CAP, &
                                        USE_MINIMUM_RM, &
                                        DETAILED_MAINTENANCE, &
                                        DETAILED_FOR, &
                                        DETAILED_TRANSFER_PRICING, &
                                        PRICES_ARE_CAPPED, &
                                        GLOBAL_SCARCITY_BUY_N_SELL, &
                                        DYNAMIC_FUEL_PRICING, &
                                        TRANS_AC_REV, &
                                        TRANS_AC_REV_ALLOC, &
                                        TRANS_AC_COST, &
                                        TRANS_AC_COST_ALLOC, &
                                        ADVANCED_PRICE_MODELING, &
                                        PRICE_LAST_MW, &
                                        FOR_SEED_OPTIONS, &
                                        LONG_PATH, &
                                        REGIONAL_MAINTENANCE, &
                                        DEPTH_OF_MARKET, &
                                        USE_5X16, &
                                        MAXIMUM_TRANSACTION_SIZE, &
                                        TRANSFER_TO_ASSET_ANALYST, & !  151
                                        TURN_OFF_POLY, &
                                        TECHNOLOGY_SCARCITY_BASIS, &
                                        TECHNOLOGY_SCARCITY_STEP, &
                                        TECHNOLOGY_SCARCITY_POWER, &
                                        NEW_BUILD_ADDITIONS_ACTIVE, &
                                        UNDER_CONSTRUCTION_PERCENT, &
                                        ADVANCED_DEVELOPMENT_PERCENT, &
                                        EARLY_DEVELOPMENT_PERCENT, &
                                        PROPOSED_PERCENT, &
                                        INDEFIN_POSTPHONED_PERCENT, &
                                        DECOMMIT_THERMAL_RESOURCES, &
                                        REGIONAL_RESERVE_MARGINS, &
                                        PLAN_TO_MWH_OR_KWYR, &
                                        EMERGENCY_MEETS_SPIN, &
                                        TECH_SCAR_MONTH_VECTOR, &
                                        INCLUDE_ICAP_REVENUE, &
                                        REMEMBER_LAST_HOUR, &
                                        COMMIT_ON_TOTAL_COST, &
                                        RESOURCE_TO_LOAD_ALLOC, &
                                        FUEL_AND_PURCHASE_COST_CAP, &
                                        SYS_EMERGENCY_MW_FLOOR, &
                                        SYS_EMERGENCY_COST_CAP, &
                                        MULTI_MARKET_ACTIVE, &
                                        DISALLOW_REDUNDANT_TRANS, &
                                        ALLOW_MARKET_ARBITRAGE, &
                                        DMD_COEFF_A, &
                                        DMD_COEFF_B, &
                                        NET_TG_BUYS_SELLS, &
                                        ICAP_MARKET_SOURCE, &
                                        STRICTLY_POSITIVE_MARGIN, &
                                        TRANS_ZONAL_NODAL, &
                                        USE_EMIS_IN_MRX, &
                                        FAST_DISPATCH, &
                                        RUN_GAS_MODEL, &
                                        REGION_OR_STATE_POWER_MAP, &
                                        LP_GAS_MODEL, &
                                        GAS_MODEL_WTI_ESC, &
                                        GAS_MODEL_DEMAND_ESC, &
                                        GAS_MODEL_EPUC_ESC, &
                                        GAS_MODEL_STORAGE_UTIL_ESC, &
                                        GAS_MODEL_INTERCEPT, &
                                        GAS_MODEL_WTI_COEFF, &
                                        GAS_MODEL_DEMAND_COEFF, &
                                        GAS_MODEL_EPUC_COEFF, &
                                        GAS_MODEL_STORAGE_UTIL_COEFF, &
                                        ACU_POWER_ID_INDEX, &
                                        LEGACY_PRIMARY_MOVER, &
                                        GAS_MODEL_RESIDUAL_ESC, &
                                        GAS_MODEL_INFLATION_ESC, &
                                        CO2_RETIREMENTS_LOGIC, &
                                        DYNAMIC_GAS_EPUC, &
                                        RUN_COAL_MODEL, &
                                        RETROFIT_LOGIC_ACTIVE, &
                                        MINIMUM_SIZE_OF_RETROFIT, &
                                        MINIMUM_YEAR_OF_RETROFIT, &
                                        USE_AVERAGE_DAILY_DEMAND, &
                                        GAS_PRICE_LAG_COEFF, &
                                        GAS_PCA_LAGGED_PRICE_VECTOR, & !  209
                                        GAS_PCA_LAGGED_PCA, &
                                        GAS_PCA_MONTH_VECTOR, &
                                        GAS_PCA_INTERCEPT, &
                                        GAS_MONTH_HH_BENCH, &
                                        USE_PRICE_MINIMUMS, &
                                        HH_VECTOR, &
                                        LMP_ACTIVE, &
                                        GRX_RPS_MODULE_ACTIVE
         IF(IOS /= 0) EXIT
         READ(10,'(A)',IOSTAT=IOS) RECLN
         IF(IOS == 0) THEN
            RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
                                   ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
            READ(RECLN,*,ERR=300) DELETE,YEAR,CHAR_VAL,PARM_VALUES_5, &
                                  LOGICAL_VAL, &
                                  UNSERVED_ENERGY_COST, &
                                  POOLING_VARIABLE_COST_SWITCH, &
                                  PARM_VALUES_10, &
                                  BTU_TAX_START_MONTH, &
                                  BTU_TAX_IN_ADJUSTMENT_CLAUSE, &
                                  TIE_GROUP_LIMIT, &
                                  LEVELIZING_RESERVE_MARGIN, &
                                  RM_PURCHASE_FIXED_COST, &
                                  RM_PURCHASE_VARIABLE_COST, &
                                  MAX_SELL_RESERVE_MARGIN, &
                                  RM_SALE_FIXED_AMOUNT, &
                                  RM_SALE_VARIABLE_AMOUNT, &
! RUN SPECS VARIABLE 
                                  FIRST_BLOCK_DISP_SWITCH, &
                                  ECON_SWITCH, &
                                  CAPACITY_PLANNING_METHOD, &
                                  PRODUCTION_COST_METHOD, &
                                  OFFSET_MAINTENANCE_VECTORS, &
                                  HOURLY_LOAD_OUT, &
                                  HOURLY_LOAD_IN, &
                                  ALLOCATE_FUEL_RIGHT_TO_LEFT, &
                                  RESOURCE_TIMING_SWITCH, &
                                  MARGINAL_COST_SWITCH, &
                                  COMMENT, &
                                  ADD_2_INCOME_STATEMENT, &
                                  ASSET_CLASS_ID, &
                                  ASSET_CLASS_VECTOR, &
                                  PRODUCTION_PERIODS_CHR, &
                                  DOE_NF_DISPOSAL_RATES, &
                                  NUC_DECOMMISSIONING_RATES, &
                                  ACCUM_MAINTENANCE, &
                                  USE_EXT_LOAD_SOURCE_ACTIVE, &
                                  DOE_R300_DISPOSAL_FEE, &
                                  RUN_TRANSACT, &
                                  CALANDER_CORRECT_OUTPUT, &
                   (TRANS_GROUP_LOAD(I),I=1,6), & !  SIX VALUES
                                  SECOND_FUEL_EMIS_DISPATCH, &
                   (TRANS_GROUP_LOAD(I),I=7,25), & !  19 VALUES
                                  MARKET_PRICE_SCALAR, &
                                  APPLY_TRANS_REV, &
                                  TRANSACTION_MODE, &
                                  TRANS_TIME_FRAME, &
                                  TRANS_REPRESENTATION, &
                                  IGNORE_NON_UTILITY, &
                                  MONTHLY_MAINTENANCE_PENALTY, &
                                  MONTHLY_TRANSACT_SWITCH, & !  12 VALUES
                                  TRANSACT_UNSERVED_COST, &
                                  GLOBAL_CAPACITY_VALUE, & !  3 VALUES
                                  GLOBAL_CAPACITY_PERCENT, & !  3 VALUES
                                  TRANSACT_ABUNDANCE_COST, &
                                  REMOVE_NIGHTIME_SCARCITY, &
                                  MAX_TRANS_ITERATIONS, &
                                  NEW_GLOBAL_CAPACITY_VALUE, & !  7 VALUES
                                  NEW_GLOBAL_CAPACITY_PERCENT, & !  7 VALUES
                                  strategic_rtrmts_logic, &
                                  RETIREMENTS_PROFIT_PER_KW, &
                                  ADDITIONS_PROFIT_PER_MWH, &
                                  UNIT_COMMITMENT_LOGIC, &
                                  BIDDING_LOGIC, &
                                  MAXIMUM_ANNUAL_MRX_CAP, &
                                  USE_MINIMUM_RM, &
                                  DETAILED_MAINTENANCE, &
                                  DETAILED_FOR, &
                                  DETAILED_TRANSFER_PRICING, &
                                  PRICES_ARE_CAPPED, &
                                  GLOBAL_SCARCITY_BUY_N_SELL, &
                                  DYNAMIC_FUEL_PRICING, &
                                  TRANS_AC_REV, &
                                  TRANS_AC_REV_ALLOC, &
                                  TRANS_AC_COST, &
                                  TRANS_AC_COST_ALLOC, &
                                  ADVANCED_PRICE_MODELING, &
                                  PRICE_LAST_MW, &
                                  FOR_SEED_OPTIONS, &
                                  LONG_PATH, &
                                  REGIONAL_MAINTENANCE, &
                                  DEPTH_OF_MARKET, &
                                  USE_5X16, &
                                  MAXIMUM_TRANSACTION_SIZE, &
                                  TRANSFER_TO_ASSET_ANALYST, & !  151
                                  TURN_OFF_POLY, &
                                  TECHNOLOGY_SCARCITY_BASIS, &
                                  TECHNOLOGY_SCARCITY_STEP, &
                                  TECHNOLOGY_SCARCITY_POWER, &
                                  NEW_BUILD_ADDITIONS_ACTIVE, &
                                  UNDER_CONSTRUCTION_PERCENT, &
                                  ADVANCED_DEVELOPMENT_PERCENT, &
                                  EARLY_DEVELOPMENT_PERCENT, &
                                  PROPOSED_PERCENT, &
                                  INDEFIN_POSTPHONED_PERCENT, &
                                  DECOMMIT_THERMAL_RESOURCES, &
                                  REGIONAL_RESERVE_MARGINS, &
                                  PLAN_TO_MWH_OR_KWYR, &
                                  EMERGENCY_MEETS_SPIN, &
                                  TECH_SCAR_MONTH_VECTOR, &
                                  INCLUDE_ICAP_REVENUE, &
                                  REMEMBER_LAST_HOUR, &
                                  COMMIT_ON_TOTAL_COST, &
                                  RESOURCE_TO_LOAD_ALLOC, &
                                  FUEL_AND_PURCHASE_COST_CAP, &
                                  SYS_EMERGENCY_MW_FLOOR, &
                                  SYS_EMERGENCY_COST_CAP, &
                                  MULTI_MARKET_ACTIVE, &
                                  DISALLOW_REDUNDANT_TRANS, &
                                  ALLOW_MARKET_ARBITRAGE, &
                                  DMD_COEFF_A, &
                                  DMD_COEFF_B, &
                                  NET_TG_BUYS_SELLS, &
                                  ICAP_MARKET_SOURCE, &
                                  STRICTLY_POSITIVE_MARGIN, &
                                  TRANS_ZONAL_NODAL, &
                                  USE_EMIS_IN_MRX, &
                                  FAST_DISPATCH, &
                                  RUN_GAS_MODEL, &
                                  REGION_OR_STATE_POWER_MAP, &
                                  LP_GAS_MODEL, &
                                  GAS_MODEL_WTI_ESC, &
                                  GAS_MODEL_DEMAND_ESC, &
                                  GAS_MODEL_EPUC_ESC, &
                                  GAS_MODEL_STORAGE_UTIL_ESC, &
                                  GAS_MODEL_INTERCEPT, &
                                  GAS_MODEL_WTI_COEFF, &
                                  GAS_MODEL_DEMAND_COEFF, &
                                  GAS_MODEL_EPUC_COEFF, &
                                  GAS_MODEL_STORAGE_UTIL_COEFF, &
                                  ACU_POWER_ID_INDEX, &
                                  LEGACY_PRIMARY_MOVER, &
                                  GAS_MODEL_RESIDUAL_ESC, &
                                  GAS_MODEL_INFLATION_ESC, &
                                  CO2_RETIREMENTS_LOGIC, &
                                  DYNAMIC_GAS_EPUC, &
                                  RUN_COAL_MODEL, &
                                  RETROFIT_LOGIC_ACTIVE, &
                                  MINIMUM_SIZE_OF_RETROFIT, &
                                  MINIMUM_YEAR_OF_RETROFIT, &
                                  USE_AVERAGE_DAILY_DEMAND, &
                                  GAS_PRICE_LAG_COEFF, &
                                  GAS_PCA_LAGGED_PRICE_VECTOR, & !  209
                                  GAS_PCA_LAGGED_PCA, &
                                  GAS_PCA_MONTH_VECTOR, &
                                  GAS_PCA_INTERCEPT, &
                                  GAS_MONTH_HH_BENCH, &
                                  USE_PRICE_MINIMUMS, &
                                  HH_VECTOR, &
                                  LMP_ACTIVE, &
                                  GRX_RPS_MODULE_ACTIVE
         ENDIF
         IF(.NOT. RETRO_FILE_EXISTS) RETROFIT_LOGIC_ACTIVE = 'F'
         IF(IREC <= YEARS_IN_STUDY) THEN
            TRANSACT_ACTIVE_IN_OVERLAY = RUN_TRANSACT /= 'F' .OR. &
                                         TRANSACT_ACTIVE_IN_OVERLAY
         ENDIF
         WRITE(12,REC=IREC) CHAR_VAL,PARM_VALUES_5, &
                            UNSERVED_ENERGY_COST, &
                            POOLING_VARIABLE_COST_SWITCH, &
                            LOGICAL_VAL, &
                            PARM_VALUES_10, &
                            BTU_TAX_START_MONTH, &
                            BTU_TAX_IN_ADJUSTMENT_CLAUSE, &
                            TIE_GROUP_LIMIT, &
                            LEVELIZING_RESERVE_MARGIN, &
                            RM_PURCHASE_FIXED_COST, &
                            RM_PURCHASE_VARIABLE_COST, &
                            MAX_SELL_RESERVE_MARGIN, &
                            RM_SALE_FIXED_AMOUNT, &
                            RM_SALE_VARIABLE_AMOUNT, &
! RUN SPECS VARIABLE
                            FIRST_BLOCK_DISP_SWITCH, &
                            ECON_SWITCH, &
                            CAPACITY_PLANNING_METHOD, &
                            PRODUCTION_COST_METHOD, &
                            OFFSET_MAINTENANCE_VECTORS, &
                            HOURLY_LOAD_OUT, &
                            HOURLY_LOAD_IN, &
                            ALLOCATE_FUEL_RIGHT_TO_LEFT, &
                            RESOURCE_TIMING_SWITCH, &
                            MARGINAL_COST_SWITCH, &
                            ADD_2_INCOME_STATEMENT, &
                            ASSET_CLASS_ID, &
                            ASSET_CLASS_VECTOR, &
                            PRODUCTION_PERIODS_CHR, &
                            DOE_NF_DISPOSAL_RATES, &
                            NUC_DECOMMISSIONING_RATES, &
                            ACCUM_MAINTENANCE, &
                            USE_EXT_LOAD_SOURCE_ACTIVE, &
                            DOE_R300_DISPOSAL_FEE, &
                            RUN_TRANSACT, &
                            CALANDER_CORRECT_OUTPUT, &
                            (TRANS_GROUP_LOAD(I),I=1,6), & !  SIX VALUES
                            SECOND_FUEL_EMIS_DISPATCH, &
                            (TRANS_GROUP_LOAD(I),I=7,25), & !  19 VALUES
                            MARKET_PRICE_SCALAR, &
                            APPLY_TRANS_REV, &
                            TRANSACTION_MODE, &
                            TRANS_TIME_FRAME, &
                            TRANS_REPRESENTATION, &
                            IGNORE_NON_UTILITY, &
                            MONTHLY_MAINTENANCE_PENALTY, &
                            MONTHLY_TRANSACT_SWITCH, & !  12 VALUES
                            TRANSACT_UNSERVED_COST, &
                            GLOBAL_CAPACITY_VALUE, & !  3 VALUES
                            GLOBAL_CAPACITY_PERCENT, & !  3 VALUES
                            TRANSACT_ABUNDANCE_COST, &
                            REMOVE_NIGHTIME_SCARCITY, &
                            MAX_TRANS_ITERATIONS, &
                            NEW_GLOBAL_CAPACITY_VALUE, & !  7 VALUES
                            NEW_GLOBAL_CAPACITY_PERCENT, & !  7 VALUES
                            strategic_rtrmts_logic, &
                            RETIREMENTS_PROFIT_PER_KW, &
                            ADDITIONS_PROFIT_PER_MWH, &
                            UNIT_COMMITMENT_LOGIC, &
                            BIDDING_LOGIC, &
                            MAXIMUM_ANNUAL_MRX_CAP, &
                            USE_MINIMUM_RM, &
                            DETAILED_MAINTENANCE, &
                            DETAILED_FOR, &
                            DETAILED_TRANSFER_PRICING, &
                            PRICES_ARE_CAPPED, &
                            GLOBAL_SCARCITY_BUY_N_SELL, &
                            DYNAMIC_FUEL_PRICING, &
                            TRANS_AC_REV, &
                            TRANS_AC_REV_ALLOC, &
                            TRANS_AC_COST, &
                            TRANS_AC_COST_ALLOC, &
                            ADVANCED_PRICE_MODELING, &
                            PRICE_LAST_MW, &
                            FOR_SEED_OPTIONS, &
                            LONG_PATH, &
                            REGIONAL_MAINTENANCE, &
                            DEPTH_OF_MARKET, &
                            USE_5X16, &
                            MAXIMUM_TRANSACTION_SIZE, &
                            TRANSFER_TO_ASSET_ANALYST, & !  151
                            TURN_OFF_POLY, &
                            TECHNOLOGY_SCARCITY_BASIS, &
                            TECHNOLOGY_SCARCITY_STEP, &
                            TECHNOLOGY_SCARCITY_POWER, &
                            NEW_BUILD_ADDITIONS_ACTIVE, &
                            UNDER_CONSTRUCTION_PERCENT, &
                            ADVANCED_DEVELOPMENT_PERCENT, &
                            EARLY_DEVELOPMENT_PERCENT, &
                            PROPOSED_PERCENT, &
                            INDEFIN_POSTPHONED_PERCENT, &
                            DECOMMIT_THERMAL_RESOURCES, &
                            REGIONAL_RESERVE_MARGINS, &
                            PLAN_TO_MWH_OR_KWYR, &
                            EMERGENCY_MEETS_SPIN, &
                            TECH_SCAR_MONTH_VECTOR, &
                            INCLUDE_ICAP_REVENUE, &
                            REMEMBER_LAST_HOUR, &
                            COMMIT_ON_TOTAL_COST, &
                            RESOURCE_TO_LOAD_ALLOC, &
                            FUEL_AND_PURCHASE_COST_CAP, &
                            SYS_EMERGENCY_MW_FLOOR, &
                            SYS_EMERGENCY_COST_CAP, &
                            MULTI_MARKET_ACTIVE, &
                            DISALLOW_REDUNDANT_TRANS, &
                            ALLOW_MARKET_ARBITRAGE, &
                            DMD_COEFF_A, &
                            DMD_COEFF_B, &
                            NET_TG_BUYS_SELLS, &
                            ICAP_MARKET_SOURCE, &
                            STRICTLY_POSITIVE_MARGIN, &
                            TRANS_ZONAL_NODAL, &
                            USE_EMIS_IN_MRX, &
                            FAST_DISPATCH, &
                            RUN_GAS_MODEL, &
                            REGION_OR_STATE_POWER_MAP, &
                            LP_GAS_MODEL, &
                            GAS_MODEL_WTI_ESC, &
                            GAS_MODEL_DEMAND_ESC, &
                            GAS_MODEL_EPUC_ESC, &
                            GAS_MODEL_STORAGE_UTIL_ESC, &
                            GAS_MODEL_INTERCEPT, &
                            GAS_MODEL_WTI_COEFF, &
                            GAS_MODEL_DEMAND_COEFF, &
                            GAS_MODEL_EPUC_COEFF, &
                            GAS_MODEL_STORAGE_UTIL_COEFF, &
                            ACU_POWER_ID_INDEX, &
                            LEGACY_PRIMARY_MOVER, &
                            GAS_MODEL_RESIDUAL_ESC, &
                            GAS_MODEL_INFLATION_ESC, &
                            CO2_RETIREMENTS_LOGIC, &
                            DYNAMIC_GAS_EPUC, &
                            RUN_COAL_MODEL, &
                            RETROFIT_LOGIC_ACTIVE, &
                            MINIMUM_SIZE_OF_RETROFIT, &
                            MINIMUM_YEAR_OF_RETROFIT, &
                            USE_AVERAGE_DAILY_DEMAND, &
                            GAS_PRICE_LAG_COEFF, &
                            GAS_PCA_LAGGED_PRICE_VECTOR, & !  209
                            GAS_PCA_LAGGED_PCA, &
                            GAS_PCA_MONTH_VECTOR, &
                            GAS_PCA_INTERCEPT, &
                            GAS_MONTH_HH_BENCH, &
                            USE_PRICE_MINIMUMS, &
                            HH_VECTOR, &
                            LMP_ACTIVE, &
                            GRX_RPS_MODULE_ACTIVE
         IF(IREC == 1) VOID_LOGICAL = &
                SET_OFFSET_MAINTENANCE_VALUE(OFFSET_MAINTENANCE_VECTORS)
      ENDDO
      CLOSE(10)
      CLOSE(12)
      IF(PRODP_OL == 'BC') CLOSE(11)
      PRODP_OL = 'OL'
      RETURN


  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from PP_OBJT SIID255'
      call end_program(er_message)

  300 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      CALL MG_LOCATE_WRITE(21,0, &
                 'Error reading Production Parameter record. Look for'// &
                                          ' a "," in a character name.', &
                           ALL_VERSIONS,1)
      er_message='stop requested from PP_OBJT SIID256'
      call end_program(er_message)
!

      ENTRY RESET_PRODP_OL

         PRODP_OL = 'BC'
         TRANSACT_ACTIVE_IN_OVERLAY = .FALSE.
      RETURN
!

      ENTRY OPEN_PROD_PARAMETERS_FILE(R_UNIT_NUM,R_PRODP_FILE_EXISTS)

         UNIT_NUM = R_UNIT_NUM
         R_PRODP_FILE_EXISTS = FILE_EXISTS
         IF(FILE_EXISTS) OPEN(UNIT_NUM, &
                 FILE=trim(OUTPUT_DIRECTORY())//PRODP_OL//"PRODP.BIN", &
                            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      RETURN
!

      ENTRY CLOSE_PROD_PARAMETERS_FILE

         IF(FILE_EXISTS) CLOSE(UNIT_NUM)
      RETURN

      ENTRY IS_TRANSACT_ACTIVE_IN_ENDPOINT( &
                                          R_TRANSACT_ACTIVE_IN_ENDPOINT)


         IF(PRODP_OL == 'OL') THEN
            R_TRANSACT_ACTIVE_IN_ENDPOINT = TRANSACT_ACTIVE_IN_OVERLAY
         ELSE
            R_TRANSACT_ACTIVE_IN_ENDPOINT = TRANSACT_ACTIVE_IN_BASEFILE
         ENDIF
      RETURN
!
 1010 FORMAT('&',A)
      END

      FUNCTION TRANSACT_ACTIVE_IN_ENDPOINT()
      use pp_objt_interfaces
      implicit none
      LOGICAL(kind=1) :: TRANSACT_ACTIVE_IN_ENDPOINT, &
                R_TRANSACT_ACTIVE_IN_ENDPOINT
         CALL IS_TRANSACT_ACTIVE_IN_ENDPOINT( &
             R_TRANSACT_ACTIVE_IN_ENDPOINT)
         TRANSACT_ACTIVE_IN_ENDPOINT = R_TRANSACT_ACTIVE_IN_ENDPOINT
      RETURN
      END

      FUNCTION PRODUCTION_PARAMETERS_OBJECT(R_UNIT)
      use spindriftlib
      use prod_arrays_dimensions
      use sizecom
      use globecom
      use production
      use mod_base_year
      use pp_objt_interfaces
      implicit none
      INTEGER (KIND=2) :: Retro_Project_First_Year_Avail, &
                          Retro_Project_Last_Year_Avail
      LOGICAL(kind=4) :: PRODP_FILE_EXISTS,PRODUCTION_PARAMETERS_OBJECT, &
                TEST_PRODP_FILE_EXISTS
      INTEGER(kind=2) :: R_UNIT,PP_UNIT,IREC,R_YEAR,I,LOCAL_YEAR
      INTEGER :: IOS
      LOGICAL(kind=1) :: READ_PRODUCTION_PARAMETERS
      LOGICAL(kind=1) :: UPDATE_PRODUCTION_PARAMETERS, &
                SET_OFFSET_MAINTENANCE_VALUE, &
                R_OFFSET_MAINTENANCE_VECTORS
      SAVE PRODP_FILE_EXISTS
      CHARACTER(len=64) :: ERR_MESSAGE
      CHARACTER(len=3) :: TRANSFER_TO_ASSET_ANALYST ! 151
!
      INTEGER(kind=1) :: BTU_TAX_START_MONTH,R_BTU_TAX_START_MONTH
      CHARACTER(len=1) :: POOL_PRICING_SWITCH,R_POOL_PRICING_SWITCH, &
                  strategic_rtrmts_logic, &
                  UNIT_COMMITMENT_LOGIC, &
                  ADVANCED_PRICE_MODELING, &
                  PRICE_LAST_MW, &
                  FOR_SEED_OPTIONS, &
                  LONG_PATH, &
                  REGIONAL_MAINTENANCE, &
                  DEPTH_OF_MARKET, &
                  USE_5X16, &
                  TURN_OFF_POLY, &
                  R_FOR_SEED_OPTIONS, &
                  BIDDING_LOGIC, &
                  USE_MINIMUM_RM, &
                  R_USE_MINIMUM_RM, &
                  DETAILED_MAINTENANCE, &
                  DETAILED_FOR, &
                  DETAILED_TRANSFER_PRICING, &
                  PRICES_ARE_CAPPED, &
                  GLOBAL_SCARCITY_BUY_N_SELL, &
                  DYNAMIC_FUEL_PRICING, &
                  NEW_BUILD_ADDITIONS_ACTIVE, &
                  DECOMMIT_THERMAL_RESOURCES, &
                  REGIONAL_RESERVE_MARGINS, &
                  PLAN_TO_MWH_OR_KWYR, &
                  EMERGENCY_MEETS_SPIN, &
                  INCLUDE_ICAP_REVENUE, &
                  REMEMBER_LAST_HOUR, &
                  COMMIT_ON_TOTAL_COST, &
                  RESOURCE_TO_LOAD_ALLOC, &
                  TECHNOLOGY_SCARCITY_BASIS, &
                  MULTI_MARKET_ACTIVE, &
                  DISALLOW_REDUNDANT_TRANS_STR, &
                  ALLOW_MARKET_ARBITRAGE_STR, &
                  NET_TG_BUYS_SELLS_STR, &
                  ICAP_MARKET_SOURCE_STR, &
                  STRICTLY_POSITIVE_MARGIN_STR, &
                  TRANS_ZONAL_NODAL_STR, &
                  USE_EMIS_IN_MRX_STR, &
                  FAST_DISPATCH_STR, &
                  RUN_GAS_MODEL_STR, &
                  REGION_OR_STATE_POWER_MAP_STR, &
                  LP_GAS_MODEL_STR, &
                  ACU_POWER_ID_INDEX_STR, &
                  LEGACY_PRIMARY_MOVER_STR, &
                  CO2_RETIREMENTS_LOGIC_STR, &
                  DYNAMIC_GAS_EPUC_STR, &
                  RUN_COAL_MODEL_STR, &
                  RETROFIT_LOGIC_ACTIVE_STR, &
                  USE_AVERAGE_DAILY_DEMAND_STR
      LOGICAL(kind=1) :: INCLUDE_IN_DISPATCH_ORDER, &
                         R_INCLUDE_IN_DISPATCH_ORDER
      LOGICAL(kind=1) :: FACET_IS_PRODUCTION_METHOD,FACET_IS_ACTIVE, &
                PICK_FOR_SEED_OPTIONS, &
                GET_STRATEGIC_RETIRMENTS_LOGIC, &
                GET_TIE_GROUP_LIMIT, &
                YES_UNIT_COMMITMENT_LOGIC, &
                YES_ADVANCED_PRICE_MODELING, &
                YES_PRICE_LAST_MW, &
                YES_LAMBDA_PRICE, &
                YES_PRICE_LAST_MW_AS_UNSERVED, &
                YES_BIDDING_LOGIC, &
                MODIFIED_BIDDING_LOGIC, &
                LONG_PATH_LOGIC, &
                REGIONAL_PA_MAINTENANCE_LOGIC
      logical (kind=1) :: REGIONAL_TG_MAINTENANCE_LOGIC, &
                DEPTH_OF_MARKET_LOGIC, &
                USE_5X16_LOGIC_CONSTRAINTS, &
                YES_TURN_OFF_POLY, &
                YES_DETAILED_MAINTENANCE, &
                YES_DETAILED_FOR, &
                YES_FREQUENCY_DURATION, &
                YES_DETAILED_TRANSFER_PRICING, &
                YES_PRICES_ARE_CAPPED, &
                YES_GLOBAL_SCARCITY_BUY_N_SELL, &
                YES_DYNAMIC_FUEL_PRICING, &
                YES_USE_MINIMUM_RM, &
                YES_TECH_PERCENT_TRANS_CAP, &
                YES_NEW_BUILD_ADDITIONS_ACTIVE, &
                GET_NEW_BUILD_PERCENTS, &
                YES_MULTI_MARKET_ACTIVE, &
                DISALLOW_REDUNDANT_TRANS, &
                ALLOW_MARKET_ARBITRAGE, &
                STRICTLY_POSITIVE_MARGIN, &
                YES_ZONAL_LEVEL_MARKET, &
                USE_EMIS_IN_MRX, &
                FAST_DISPATCH, &
                RUN_GAS_MODEL, &
                REGION_OR_STATE_POWER_MAP, &
                LP_GAS_MODEL, &
                GET_ACU_POWER_ID_INDEX, &
                GET_LEGACY_PRIMARY_MOVER, &
                GET_CO2_RETIREMENTS_LOGIC, &
                GET_DMD_COEFF, &
                GAS_MODEL_ONLY, &
                COAL_MODEL_ONLY, &
                DYNAMIC_GAS_EPUC, &
                RUN_COAL_MODEL, &
                RETROFIT_LOGIC_ACTIVE,SET_RETROFIT_LOGIC_OFF, &
                CO2_RETROFIT_LOGIC_ACTIVE, &
                USE_AVERAGE_DAILY_DEMAND, &
                RUN_HH_ONLY
      SAVE FACET_IS_PRODUCTION_METHOD
      REAL :: TRANSACTION_BUY_SPREAD,R_TRANSACTION_BUY_SPREAD, &
            TRANSACTION_SELL_SPREAD,R_TRANSACTION_SELL_SPREAD
      REAL :: POOL_NATIVE_MULT,R_POOL_NATIVE_MULT, &
            POOL_OTHER_MULT,R_POOL_OTHER_MULT, &
            UNSERVED_ENERGY_PRICE,R_UNSERVED_ENERGY_PRICE, &
            CAP_LIMT_BTU_TAX_RATES_GOCN12(0:6), &
                                   R_CAP_LIMT_BTU_TAX_RATES_GOCN12(0:6), &
            ENRG_LIMT_ENRGY_TAX_RATES_H12(0:3), &
                                   R_ENRG_LIMT_ENRGY_TAX_RATES_H12(0:3), &
            BTU_TAX_IN_ADJUSTMENT_CLAUSE,R_BTU_TAX_IN_ADJUSTMENT_CLAUSE, &
            POOLING_VARIABLE_COST_SWITCH,R_POOLING_VARIABLE_COST_SWITCH, &
            POOLING_FUEL_COST_SWITCH,R_POOLING_FUEL_COST_SWITCH, &
            POOLING_FIXED_COST_SWITCH,R_POOLING_FIXED_COST_SWITCH, &
            TIE_GROUP_LIMIT(3),R_TIE_GROUP_LIMIT(3), &
            GET_MARKET_PRICE_SCALAR,MARKET_PRICE_SCALAR, &
            TRANSACT_UNSERVED_COST=0., &
            GET_GLOBAL_SCARCITY, &
            GLOBAL_CAPACITY_VALUE(3), &
            GLOBAL_CAPACITY_PERCENT(3), &
            NEW_GLOBAL_CAPACITY_VALUE(7), &
            NEW_GLOBAL_CAPACITY_PERCENT(7), &
            R_GLOBAL_CAPACITY_VALUE_1, &
            R_GLOBAL_CAPACITY_VALUE_2, &
            R_GLOBAL_CAPACITY_VALUE_3, &
            R_GLOBAL_CAPACITY_VALUE_4, &
            R_GLOBAL_CAPACITY_VALUE_5, &
            R_GLOBAL_CAPACITY_VALUE_6, &
            R_GLOBAL_CAPACITY_VALUE_7, &
            R_GLOBAL_CAPACITY_VALUE_8, &
            R_GLOBAL_CAPACITY_VALUE_9, &
            R_GLOBAL_CAPACITY_VALUE_10, &
            R_GLOBAL_CAPACITY_PERCENT_1
            
      real :: R_GLOBAL_CAPACITY_PERCENT_2, &
            R_GLOBAL_CAPACITY_PERCENT_3, &
            R_GLOBAL_CAPACITY_PERCENT_4, &
            R_GLOBAL_CAPACITY_PERCENT_5, &
            R_GLOBAL_CAPACITY_PERCENT_6, &
            R_GLOBAL_CAPACITY_PERCENT_7, &
            R_GLOBAL_CAPACITY_PERCENT_8, &
            R_GLOBAL_CAPACITY_PERCENT_9, &
            R_GLOBAL_CAPACITY_PERCENT_10, &
            GET_TRANSACT_UNSERVED_COST, &
            RETIREMENTS_PROFIT_PER_KW, &
            R_RETIREMENTS_PROFIT_PER_KW, &
            ADDITIONS_PROFIT_PER_MWH, &
            GET_ADDITIONS_PROFIT_PER_MWH, &
            MAXIMUM_ANNUAL_MRX_CAP, &
            GET_MAXIMUM_ANNUAL_MRX_CAP, &
            MAXIMUM_TRANSACTION_SIZE, &
            GET_MAXIMUM_TRANSACTION_SIZE, &
            TECHNOLOGY_SCARCITY_STEP, &
            TECHNOLOGY_SCARCITY_POWER, &
            UNDER_CONSTRUCTION_PERCENT, &
            ADVANCED_DEVELOPMENT_PERCENT, &
            EARLY_DEVELOPMENT_PERCENT, &
            PROPOSED_PERCENT, &
            INDEFIN_POSTPHONED_PERCENT, &
            GET_TECH_PERCENT_INPUT, &
            GET_TECH_POWER, &
            DMD_COEFF_A, &
            DMD_COEFF_B, &
            TEMP_R1, &
            TEMP_R2, &
            GAS_MODEL_INTERCEPT, &
            GAS_MODEL_WTI_COEFF, &
            GAS_MODEL_DEMAND_COEFF, &
            GAS_MODEL_EPUC_COEFF, &
            GAS_MODEL_STORAGE_UTIL_COEFF, &
            GAS_PRICE_LAG_COEFF, &
            GAS_PCA_LAGGED_PCA, &
            GAS_PCA_INTERCEPT, &
            TEMP_COEFF(8)
!
      SAVE  POOL_PRICING_SWITCH,TRANSACTION_BUY_SPREAD, &
            TRANSACTION_SELL_SPREAD,POOL_NATIVE_MULT,POOL_OTHER_MULT, &
            UNSERVED_ENERGY_PRICE,CAP_LIMT_BTU_TAX_RATES_GOCN12, &
            ENRG_LIMT_ENRGY_TAX_RATES_H12,BTU_TAX_IN_ADJUSTMENT_CLAUSE, &
            POOLING_VARIABLE_COST_SWITCH,POOLING_FUEL_COST_SWITCH, &
            POOLING_FIXED_COST_SWITCH,TIE_GROUP_LIMIT, &
            INCLUDE_IN_DISPATCH_ORDER,BTU_TAX_START_MONTH, &
            GLOBAL_CAPACITY_VALUE, &
            GLOBAL_CAPACITY_PERCENT, &
            NEW_GLOBAL_CAPACITY_VALUE, &
            NEW_GLOBAL_CAPACITY_PERCENT, &
            strategic_rtrmts_logic, &
            RETIREMENTS_PROFIT_PER_KW, &
            ADDITIONS_PROFIT_PER_MWH, &
            UNIT_COMMITMENT_LOGIC, &
            ADVANCED_PRICE_MODELING, &
            PRICE_LAST_MW, &
            FOR_SEED_OPTIONS
      save LONG_PATH, &
            REGIONAL_MAINTENANCE, &
            DEPTH_OF_MARKET, &
            USE_5X16, &
            TURN_OFF_POLY, &
            TECHNOLOGY_SCARCITY_BASIS, &
            TECHNOLOGY_SCARCITY_STEP, &
            TECHNOLOGY_SCARCITY_POWER, &
            NEW_BUILD_ADDITIONS_ACTIVE, &
            UNDER_CONSTRUCTION_PERCENT, &
            ADVANCED_DEVELOPMENT_PERCENT, &
            EARLY_DEVELOPMENT_PERCENT, &
            PROPOSED_PERCENT, &
            INDEFIN_POSTPHONED_PERCENT, &
            DECOMMIT_THERMAL_RESOURCES, &
            REGIONAL_RESERVE_MARGINS, &
            PLAN_TO_MWH_OR_KWYR, &
            EMERGENCY_MEETS_SPIN, &
            INCLUDE_ICAP_REVENUE, &
            REMEMBER_LAST_HOUR, &
            COMMIT_ON_TOTAL_COST, &
            MAXIMUM_TRANSACTION_SIZE, &
            BIDDING_LOGIC, &
            MAXIMUM_ANNUAL_MRX_CAP, &
            USE_MINIMUM_RM, &
            DETAILED_MAINTENANCE, &
            DETAILED_FOR, &
            DETAILED_TRANSFER_PRICING, &
            PRICES_ARE_CAPPED, &
            GLOBAL_SCARCITY_BUY_N_SELL, &
            DYNAMIC_FUEL_PRICING, &
            RUN_COAL_MODEL_STR, &
            RETROFIT_LOGIC_ACTIVE_STR, &
            USE_AVERAGE_DAILY_DEMAND_STR

!
      REAL :: LEVELIZING_RESERVE_MARGIN, &
           RM_PURCHASE_FIXED_COST, &
           RM_PURCHASE_VARIABLE_COST, &
           MAX_SELL_RESERVE_MARGIN, &
           RM_SALE_FIXED_AMOUNT, &
           RM_SALE_VARIABLE_AMOUNT, &
           FUEL_AND_PURCHASE_COST_CAP, &
           SYS_EMERGENCY_MW_FLOOR, &
           SYS_EMERGENCY_COST_CAP, &
           R_FUEL_AND_PURCHASE_COST_CAP, &
           R_SYS_EMERGENCY_MW_FLOOR, &
           R_SYS_EMERGENCY_COST_CAP
      REAL :: R_LEVELIZING_RESERVE_MARGIN, &
           R_RM_PURCHASE_FIXED_COST, &
           R_RM_PURCHASE_VARIABLE_COST, &
           R_MAX_SELL_RESERVE_MARGIN, &
           R_RM_SALE_FIXED_AMOUNT, &
           R_RM_SALE_VARIABLE_AMOUNT, &
           RETURN_RESERVE_MARGIN_COSTS
!
      REAL :: P_TRANSACTION_BUY_SPREAD=1.,L_TRANSACTION_BUY_SPREAD=0., &
           P_TRANSACTION_SELL_SPREAD=1.,L_TRANSACTION_SELL_SPREAD=0., &
           P_UNSERVED_ENERGY_PRICE=1.,L_UNSERVED_ENERGY_PRICE=0., &
           P_BTU_TAX_IN_ADJUSTMENT_CLAUSE=1., &
                               L_BTU_TAX_IN_ADJUSTMENT_CLAUSE=0., &
           P_RM_PURCHASE_FIXED_COST=1.,L_RM_PURCHASE_FIXED_COST=0., &
           P_RM_PURCHASE_VARIABLE_COST=1., &
                                        L_RM_PURCHASE_VARIABLE_COST=0., &
           P_RM_SALE_FIXED_AMOUNT=1.,L_RM_SALE_FIXED_AMOUNT=0., &
           P_RM_SALE_VARIABLE_AMOUNT=1.,L_RM_SALE_VARIABLE_AMOUNT=0.
!
      SAVE PP_UNIT
      SAVE LEVELIZING_RESERVE_MARGIN, &
           RM_PURCHASE_FIXED_COST, &
           RM_PURCHASE_VARIABLE_COST, &
           MAX_SELL_RESERVE_MARGIN, &
           RM_SALE_FIXED_AMOUNT, &
           RM_SALE_VARIABLE_AMOUNT
      CHARACTER(len=1) :: ADD_2_INCOME_STATEMENT, &
                  R_ADD_2_INCOME_STATEMENT
      INTEGER(kind=2) :: ASSET_CLASS_ID,ASSET_CLASS_VECTOR, &
                R_ASSET_CLASS_ID,GAS_MODEL_WTI_ESC, &
                GAS_MODEL_DEMAND_ESC, &
                GAS_MODEL_EPUC_ESC, &
                GAS_MODEL_STORAGE_UTIL_ESC, &
                GAS_MODEL_RESIDUAL_ESC, &
                GAS_MODEL_INFLATION_ESC, &
                TEMP_ESC(9), &
                GAS_PCA_MONTH_VECTOR, &
                GAS_PCA_LAGGED_PRICE_VECTOR, &
                GAS_MONTH_HH_BENCH, &
                HH_VECTOR, &
                GET_HH_VECTOR
!     +          TEMP_ESC(4)
      SAVE ADD_2_INCOME_STATEMENT,ASSET_CLASS_ID,ASSET_CLASS_VECTOR
!
!  RUN SPECS PRODUCTION VARIABLES
!
      CHARACTER(len=80) :: PRODUCTION_METHOD_TITLE
      INTEGER(kind=2) :: SET_PRODUCTION_METHOD, &
                TRANSACTION_MODE,TRANSACTION_MODE_NUMBER=0, &
                MONTHLY_MAINTENANCE_PENALTY, &
                MONTHLY_MAINTENANCE_PENALTY_NUM=0, &
                MAX_TRANS_ITERATIONS, &
                TECH_SCAR_MONTH_VECTOR=0, &
                GET_TECH_SCAR_MONTH_VECTOR, &
                MAX_TRANS_ITERATIONS_NUM=100
      CHARACTER(len=1) :: PRODUCTION_COST_METHOD_CHR, &
                  R_PRODUCTION_COST_METHOD_CHR, &
                  PRODUCTION_PERIODS_CHR, &
                  R_PRODUCTION_PERIODS_CHR, &
                  ACCUM_MAINTENANCE_CHR, &
                  RUN_TRANSACT_CHR, &
                  APPLY_TRANS_REV_CHR, &
                  TRANSACTION_MODE_CHR, &
                  SECOND_FUEL_EMIS_DISPATCH_CHR, &
                  CALANDER_CORRECT_CHR, &
                  USE_EXT_LOAD_SOURCE_ACTIVE, &
                  TRANS_TIME_FRAME,TRANS_TIME_FRAME_CHR='M', &
                  TRANS_REPRESENTATION,TRANS_REPRESENTATION_CHR='P', &
                  IGNORE_NON_UTILITY,IGNORE_NON_UTILITY_CHR='F', &
                  MONTHLY_TRANSACT_SWITCH(12), &
                  REMOVE_NIGHTIME_SCARCITY, &
                  REMOVE_NIGHTIME_SCARCITY_CHR='F', &
                  USE_PRICE_MINIMUMS_CHR='F'

      LOGICAL(kind=1) :: ACCUM_MAINTENANCE,ACCUMULATE_MAINTENANCE, &
                USE_EXT_LOAD_SOURCE,RUN_TRANSACT, &
                APPLY_TRANS_REV_TO_WHOLESALE, &
                APPLY_RETAIL_AS_WHOLESALE, &
                IGNORE_NATIVE_PURCHASE_USE_W, &
                SECOND_FUEL_EMIS_DISPATCH, &
                CALANDER_CORRECT, &
                YES_CALANDER_CORRECT, &
                ONLY_BUY_TRANSACT, &
                ONLY_SELL_TRANSACT, &
                ONLY_DISPATCH_TRANSACT, &
                CENTRAL_DISPATCH_TRANSACT, &
                STRICT_MARKET_PRICE, &
                MULTI_AREA_PRICE, &
                REGIONAL_AREA_PRICE, &
                USE_TRANSACT_LOADS, &
                YES_RUN_TRANSACT, &
                YES_SECOND_FUEL_EMIS_DISPATCH, &
                YES_ONLY_BUY_TRANSACT, &
                YES_ONLY_SELL_TRANSACT, &
                YES_ONLY_DISPATCH_TRANSACT, &
                YES_USE_TRANSACT_LOADS, &
                YES_CENTRAL_DISPATCH_TRANSACT, &
                YES_STRICT_MARKET_PRICE, &
                YES_MULTI_AREA_PRICE, &
                YES_REGIONAL_AREA_PRICE, &
                YES_RUN_MULTIAREA_TRANSACT,RUN_MULTIAREA_TRANSACT, &
                RUN_TRANSACT_THIS_MONTH, &
                YES_DECOMMIT_THERMAL_RESOURCES, &
                YES_REGIONAL_RESERVE_MARGINS, &
                YES_PLAN_TO_MWH_OR_KWYR, &
                YES_EMERGENCY_MEETS_SPIN, &
                YES_INCLUDE_ICAP_REVENUE, &
                YES_REMEMBER_LAST_HOUR, &
                YES_COMMIT_ON_TOTAL_COST, &
                YES_COMMIT_ON_TOTAL_COST_ENV, &
                YES_RESOURCE_TO_LOAD_ALLOC, &
                GET_HH_PARAMETERS, &
                USE_PRICE_MINIMUMS
      SAVE ACCUM_MAINTENANCE,USE_EXT_LOAD_SOURCE_ACTIVE
      INTEGER(kind=2) :: PRODUCTION_COST_METHOD
      REAL(kind=4) :: NUCLEAR_DECOMMISSIONING_COST,DOE_NF_DISPOSAL_COST, &
             DOE_R300_DISPOSAL_COST
      REAL(kind=4) :: R_MMBTU,R_MWH
      REAL :: DOE_NF_DISPOSAL_RATES(2) = 0., &
           NUC_DECOMMISSIONING_RATES(2) = 0., &
           DOE_R300_DISPOSAL_FEE(2) = 0., &
           TRANSACT_ABUNDANCE_COST, &
           GET_TRANSACT_ABUNDANCE_COST, &
           R_TEMP_R4
      REAL (KIND=4), SAVE :: ESC_DOE_NF_DISPOSAL_RATES(2), &
                             ESC_NUC_DECOMMISSIONING_RATES(2), &
                             ESC_DOE_R300_DISPOSAL_FEE(2)
      REAL (KIND=4), SAVE :: P_DOE_NF_DISPOSAL_RATES(2), &
                             P_NUC_DECOMMISSIONING_RATES(2), &
                             P_DOE_R300_DISPOSAL_FEE(2)
      REAL (KIND=4), SAVE :: P_TRANSACT_UNSERVED_COST, &
                             ESC_TRANSACT_UNSERVED_COST, &
                             P_TRANSACT_ABUNDANCE_COST, &
                             ESC_TRANSACT_ABUNDANCE_COST, &
                             MINIMUM_SIZE_OF_RETROFIT=0
      SAVE PRODUCTION_COST_METHOD,PRODUCTION_PERIODS_CHR
      CHARACTER(len=1) :: PLANNING_TRIGGER, &
                          CAPACITY_PLANNING_TRIGGER_VALUE=' '
      CHARACTER(len=1) :: FIRST_BLOCK_DISP_SWITCH, &
                                     FIRST_BLOCK_DISP_SWITCH_ACTIVE=' '
      LOGICAL(kind=1) :: MARGINAL_COST_SWITCH, &
                         RETURN_FUEL_ALLOCATION_SWITCH
      CHARACTER(len=1) :: MARGINAL_COST_SWITCH_ACTIVE=' '
      CHARACTER(len=1) :: ECON_SWITCH_ACTIVE='F' ! 5/26/95. GAT. FOR ST JOE.
      LOGICAL(kind=1) :: ECON_SWITCH,ECON_SALES,ECON_PRICING,ECON_MARGIN
      LOGICAL(kind=1) :: OFFSET_MAINTENANCE_VECTORS, &
                OFFSET_MAINTENANCE_SWITCH, &
                OFFSET_MAINTENANCE_ACTIVE, &
                TRANSFER_PRICING_VECTORS
      SAVE OFFSET_MAINTENANCE_ACTIVE
      LOGICAL(kind=1) :: MODIFY_RESOURCE_TIMING, &
                         RESOURCE_TIMING_SWITCH=.TRUE.
      CHARACTER(len=6) :: RESOURCE_TIMING_SWITCH_STR
      INTEGER(kind=2) :: PROCMETH
!
      CHARACTER(len=1) :: ALLOCATE_FUEL_RIGHT_TO_LEFT
      INTEGER(kind=2) :: HOURLY_LOAD_OUT,HOURLY_LOAD_IN, &
                TRANS_GROUP_LOAD,TRANS_GROUP_LOAD_IN_FILE(25),R_GROUP, &
                HOURLY_LOAD_OUT_FILE,HOURLY_LOAD_IN_FILE, &
                MAX_TRANS_LOADS,GET_MAX_TRANS_LOADS, &
                FIRST_LAST_MONTH_OF_TRANSACT,FIRST_MONTHLY_TRANSACT=0, &
                MO,R_LAST_MONTH,LAST_MONTHLY_TRANSACT=0,R_MONTH, &
                TRANS_AC_REV, &
                TRANS_AC_REV_ALLOC, &
                TRANS_AC_COST, &
                TRANS_AC_COST_ALLOC, &
                R_TRANS_AC_REV, &
                R_TRANS_AC_REV_ALLOC, &
                R_TRANS_AC_COST, &
                R_TRANS_AC_COST_ALLOC, &
                FIRST_YEAR_PRICE_MODE=19999, &
                GET_FIRST_YEAR_PRICE_MODE, &
                MINIMUM_YEAR_OF_RETROFIT=0, &
                R_TEMP_I2
      SAVE  ALLOCATE_FUEL_RIGHT_TO_LEFT, &
            HOURLY_LOAD_OUT_FILE,HOURLY_LOAD_IN_FILE, &
            RUN_TRANSACT, &
            APPLY_TRANS_REV_CHR, &
            SECOND_FUEL_EMIS_DISPATCH, &
            ONLY_BUY_TRANSACT, &
            ONLY_SELL_TRANSACT, &
            ONLY_DISPATCH_TRANSACT, &
            USE_TRANSACT_LOADS, &
            CENTRAL_DISPATCH_TRANSACT, &
            STRICT_MARKET_PRICE, &
            MULTI_AREA_PRICE, &
            REGIONAL_AREA_PRICE, &
            CALANDER_CORRECT, &
            TRANS_GROUP_LOAD_IN_FILE, &
            RUN_MULTIAREA_TRANSACT, &
            MAX_TRANS_LOADS, &
            MARKET_PRICE_SCALAR, &
            TRANSACT_ABUNDANCE_COST
      CHARACTER(len=1) :: VOID_CHR,SAVE_CHR_PP_VALUES
      LOGICAL(kind=1) :: TRANSACT_ANALYST_ONLY
!
!  ASSET ANALYST SWITCHES 1/16/03 MSG
!
      INTEGER(kind=2) :: BEGIN_ASSET_ANALYST_TRANSFER=25000, &
                START_TRANSFER_MONTH=13, &
                GET_MONTH_NUMBER, &
                TRANSACT_ANLST_RSLTS_AVAIL_STG

      INTEGER(kind=2) :: HISTORICAL_PRODUCTION_DATE
      INTEGER (KIND=2) :: BASE_DATE
      LOGICAL (KIND=1) :: USE_TRANSACT_ANALYST_RESULTS(12)
      LOGICAL (KIND=1) :: TRANSFER_TRANSACT_ANL_RESULTS, &
                          GRX_RPS_MODULE_IS_ACTIVE, &
                          RPS_ONLY_SWITCH
      CHARACTER (LEN=1) :: LMP_ACTIVE,GRX_RPS_MODULE_ACTIVE
      LOGICAL (KIND=4) :: LMP_IS_ACTIVE

!
!
!
! END DATA DECLARATIONS
!
         PP_UNIT = R_UNIT
         CALL OPEN_PROD_PARAMETERS_FILE(PP_UNIT,PRODP_FILE_EXISTS)
         PRODUCTION_PARAMETERS_OBJECT = PRODP_FILE_EXISTS
      RETURN

      ENTRY TEST_PRODP_FILE_EXISTS()

         TEST_PRODP_FILE_EXISTS = PRODP_FILE_EXISTS
      RETURN

      ENTRY SET_OFFSET_MAINTENANCE_VALUE(R_OFFSET_MAINTENANCE_VECTORS)

         OFFSET_MAINTENANCE_ACTIVE = R_OFFSET_MAINTENANCE_VECTORS
         SET_OFFSET_MAINTENANCE_VALUE = OFFSET_MAINTENANCE_ACTIVE
      RETURN

      ENTRY READ_PRODUCTION_PARAMETERS(R_YEAR, &
                                       R_POOL_PRICING_SWITCH, &
                                       R_POOL_NATIVE_MULT, &
                                       R_POOL_OTHER_MULT, &
                                       R_TRANSACTION_BUY_SPREAD, &
                                       R_TRANSACTION_SELL_SPREAD, &
                                       R_CAP_LIMT_BTU_TAX_RATES_GOCN12, &
                                       R_UNSERVED_ENERGY_PRICE, &
                                       R_POOLING_VARIABLE_COST_SWITCH, &
                                       R_INCLUDE_IN_DISPATCH_ORDER, &
                                       R_POOLING_FUEL_COST_SWITCH, &
                                       R_POOLING_FIXED_COST_SWITCH, &
                                       R_ENRG_LIMT_ENRGY_TAX_RATES_H12, &
                                       R_BTU_TAX_START_MONTH, &
                                       R_BTU_TAX_IN_ADJUSTMENT_CLAUSE, &
                                       R_TIE_GROUP_LIMIT)

         R_POOL_PRICING_SWITCH = POOL_PRICING_SWITCH
         R_POOL_NATIVE_MULT = POOL_NATIVE_MULT
         R_POOL_OTHER_MULT = POOL_OTHER_MULT
         R_TRANSACTION_BUY_SPREAD = TRANSACTION_BUY_SPREAD
         R_TRANSACTION_SELL_SPREAD = TRANSACTION_SELL_SPREAD
         R_CAP_LIMT_BTU_TAX_RATES_GOCN12(1) = &
                                        CAP_LIMT_BTU_TAX_RATES_GOCN12(1)
         R_CAP_LIMT_BTU_TAX_RATES_GOCN12(2) = &
                                        CAP_LIMT_BTU_TAX_RATES_GOCN12(2)
         R_CAP_LIMT_BTU_TAX_RATES_GOCN12(3) = &
                                        CAP_LIMT_BTU_TAX_RATES_GOCN12(3)
         R_CAP_LIMT_BTU_TAX_RATES_GOCN12(4) = &
                                        CAP_LIMT_BTU_TAX_RATES_GOCN12(4)
         R_CAP_LIMT_BTU_TAX_RATES_GOCN12(5) = &
                                        CAP_LIMT_BTU_TAX_RATES_GOCN12(5)
         R_CAP_LIMT_BTU_TAX_RATES_GOCN12(6) = &
                                        CAP_LIMT_BTU_TAX_RATES_GOCN12(6)
         R_UNSERVED_ENERGY_PRICE = UNSERVED_ENERGY_PRICE
         R_POOLING_VARIABLE_COST_SWITCH = POOLING_VARIABLE_COST_SWITCH
         R_INCLUDE_IN_DISPATCH_ORDER = INCLUDE_IN_DISPATCH_ORDER
         R_POOLING_FUEL_COST_SWITCH = POOLING_FUEL_COST_SWITCH
         R_POOLING_FIXED_COST_SWITCH = POOLING_FIXED_COST_SWITCH
         R_ENRG_LIMT_ENRGY_TAX_RATES_H12(1) = &
                                        ENRG_LIMT_ENRGY_TAX_RATES_H12(1)
         R_ENRG_LIMT_ENRGY_TAX_RATES_H12(2) = &
                                        ENRG_LIMT_ENRGY_TAX_RATES_H12(2)
         R_ENRG_LIMT_ENRGY_TAX_RATES_H12(3) = &
                                        ENRG_LIMT_ENRGY_TAX_RATES_H12(3)
         R_BTU_TAX_START_MONTH = BTU_TAX_START_MONTH
         R_BTU_TAX_IN_ADJUSTMENT_CLAUSE = BTU_TAX_IN_ADJUSTMENT_CLAUSE
         R_TIE_GROUP_LIMIT(1) = TIE_GROUP_LIMIT(1)
         R_TIE_GROUP_LIMIT(2) = TIE_GROUP_LIMIT(2)
         R_TIE_GROUP_LIMIT(3) = TIE_GROUP_LIMIT(3)
         READ_PRODUCTION_PARAMETERS = .TRUE.
      RETURN

      ENTRY UPDATE_PRODUCTION_PARAMETERS(R_YEAR)

         IF(.NOT. PRODP_FILE_EXISTS) RETURN
         UPDATE_PRODUCTION_PARAMETERS = .FALSE.
         IREC = MIN(R_YEAR,AVAIL_DATA_YEARS)
         READ(PP_UNIT,REC=IREC,IOSTAT=IOS) POOL_PRICING_SWITCH, &
                               POOL_NATIVE_MULT,POOL_OTHER_MULT, &
                               TRANSACTION_BUY_SPREAD, &
                               TRANSACTION_SELL_SPREAD, &
                               CAP_LIMT_BTU_TAX_RATES_GOCN12(1), &
                               UNSERVED_ENERGY_PRICE, &
                               POOLING_VARIABLE_COST_SWITCH, &
                               INCLUDE_IN_DISPATCH_ORDER, &
                               POOLING_FUEL_COST_SWITCH, &
                               POOLING_FIXED_COST_SWITCH, &
                               (CAP_LIMT_BTU_TAX_RATES_GOCN12(I),I=2,6), &
                               (ENRG_LIMT_ENRGY_TAX_RATES_H12(I),I=1,3), &
                               BTU_TAX_START_MONTH, &
                               BTU_TAX_IN_ADJUSTMENT_CLAUSE, &
                               TIE_GROUP_LIMIT, &
                               LEVELIZING_RESERVE_MARGIN, &
                               RM_PURCHASE_FIXED_COST, &
                               RM_PURCHASE_VARIABLE_COST, &
                               MAX_SELL_RESERVE_MARGIN, &
                               RM_SALE_FIXED_AMOUNT, &
                               RM_SALE_VARIABLE_AMOUNT, & 
!
!  RUN SPECS PRODUCTION HERE
!
                               FIRST_BLOCK_DISP_SWITCH_ACTIVE, &
                               ECON_SWITCH_ACTIVE, &
                               CAPACITY_PLANNING_TRIGGER_VALUE, &
                               PRODUCTION_COST_METHOD_CHR, &
                               OFFSET_MAINTENANCE_SWITCH, &
                               HOURLY_LOAD_OUT_FILE, &
                               HOURLY_LOAD_IN_FILE, &
                               ALLOCATE_FUEL_RIGHT_TO_LEFT, &
                               RESOURCE_TIMING_SWITCH_STR, &
                               MARGINAL_COST_SWITCH_ACTIVE, &
!
!  12/22/95 ADDED TO PUT RESERVE EXPENSES ON THE INCOME STATEMENT
!
                               ADD_2_INCOME_STATEMENT, &
                               ASSET_CLASS_ID, &
                               ASSET_CLASS_VECTOR, &
                               PRODUCTION_PERIODS_CHR, &
                               DOE_NF_DISPOSAL_RATES, &
                               NUC_DECOMMISSIONING_RATES, &
                               ACCUM_MAINTENANCE_CHR, &
                               USE_EXT_LOAD_SOURCE_ACTIVE, &
                               DOE_R300_DISPOSAL_FEE, &
                               RUN_TRANSACT_CHR, &
                               CALANDER_CORRECT_CHR, &
                               (TRANS_GROUP_LOAD_IN_FILE(I),I=1,6), & !  SIX VALUES
                               SECOND_FUEL_EMIS_DISPATCH_CHR, &
                               (TRANS_GROUP_LOAD_IN_FILE(I),I=7,25), & !  19 VALUES
                               MARKET_PRICE_SCALAR, &
                               APPLY_TRANS_REV_CHR, &
                               TRANSACTION_MODE_CHR, &
                               TRANS_TIME_FRAME_CHR, &
                               TRANS_REPRESENTATION_CHR, &
                               IGNORE_NON_UTILITY_CHR, &
                               MONTHLY_MAINTENANCE_PENALTY_NUM, &
                               MONTHLY_TRANSACT_SWITCH, &
                               TRANSACT_UNSERVED_COST, &
                               GLOBAL_CAPACITY_VALUE, &
                               GLOBAL_CAPACITY_PERCENT, &
                               TRANSACT_ABUNDANCE_COST, &
                               REMOVE_NIGHTIME_SCARCITY_CHR, &
                               MAX_TRANS_ITERATIONS_NUM, &
                               NEW_GLOBAL_CAPACITY_VALUE, &
                               NEW_GLOBAL_CAPACITY_PERCENT, &
                               strategic_rtrmts_logic, &
                               RETIREMENTS_PROFIT_PER_KW, &
                               ADDITIONS_PROFIT_PER_MWH, &
                               UNIT_COMMITMENT_LOGIC, &
                               BIDDING_LOGIC, &
                               MAXIMUM_ANNUAL_MRX_CAP, &
                               USE_MINIMUM_RM, &
                               DETAILED_MAINTENANCE, &
                               DETAILED_FOR, &
                               DETAILED_TRANSFER_PRICING, &
                               PRICES_ARE_CAPPED, &
                               GLOBAL_SCARCITY_BUY_N_SELL, &
                               DYNAMIC_FUEL_PRICING, &
                               TRANS_AC_REV, &
                               TRANS_AC_REV_ALLOC, &
                               TRANS_AC_COST, &
                               TRANS_AC_COST_ALLOC, &
                               ADVANCED_PRICE_MODELING, &
                               PRICE_LAST_MW, &
                               FOR_SEED_OPTIONS, &
                               LONG_PATH, &
                               REGIONAL_MAINTENANCE, &
                               DEPTH_OF_MARKET, &
                               USE_5X16, &
                               MAXIMUM_TRANSACTION_SIZE, & !  150
                               TRANSFER_TO_ASSET_ANALYST, &
                               TURN_OFF_POLY, &
                               TECHNOLOGY_SCARCITY_BASIS, &
                               TECHNOLOGY_SCARCITY_STEP, &
                               TECHNOLOGY_SCARCITY_POWER, &
                               NEW_BUILD_ADDITIONS_ACTIVE, &
                               UNDER_CONSTRUCTION_PERCENT, &
                               ADVANCED_DEVELOPMENT_PERCENT, &
                               EARLY_DEVELOPMENT_PERCENT, &
                               PROPOSED_PERCENT, &
                               INDEFIN_POSTPHONED_PERCENT, &
                               DECOMMIT_THERMAL_RESOURCES, &
                               REGIONAL_RESERVE_MARGINS, &
                               PLAN_TO_MWH_OR_KWYR, &
                               EMERGENCY_MEETS_SPIN, &
                               TECH_SCAR_MONTH_VECTOR, &
                               INCLUDE_ICAP_REVENUE, &
                               REMEMBER_LAST_HOUR, &
                               COMMIT_ON_TOTAL_COST, &
                               RESOURCE_TO_LOAD_ALLOC, &
                               FUEL_AND_PURCHASE_COST_CAP, &
                               SYS_EMERGENCY_MW_FLOOR, &
                               SYS_EMERGENCY_COST_CAP, &
                               MULTI_MARKET_ACTIVE, &
                               DISALLOW_REDUNDANT_TRANS_STR, &
                               ALLOW_MARKET_ARBITRAGE_STR, &
                               DMD_COEFF_A, &
                               DMD_COEFF_B, &
                               NET_TG_BUYS_SELLS_STR, &
                               ICAP_MARKET_SOURCE_STR, &
                               STRICTLY_POSITIVE_MARGIN_STR, &
                               TRANS_ZONAL_NODAL_STR, &
                               USE_EMIS_IN_MRX_STR, &
                               FAST_DISPATCH_STR, &
                               RUN_GAS_MODEL_STR, &
                               REGION_OR_STATE_POWER_MAP_STR, &
                               LP_GAS_MODEL_STR, &
                               GAS_MODEL_WTI_ESC, &
                               GAS_MODEL_DEMAND_ESC, &
                               GAS_MODEL_EPUC_ESC, &
                               GAS_MODEL_STORAGE_UTIL_ESC, &
                               GAS_MODEL_INTERCEPT, &
                               GAS_MODEL_WTI_COEFF, &
                               GAS_MODEL_DEMAND_COEFF, &
                               GAS_MODEL_EPUC_COEFF, &
                               GAS_MODEL_STORAGE_UTIL_COEFF, &
                               ACU_POWER_ID_INDEX_STR, &
                               LEGACY_PRIMARY_MOVER_STR, &
                               GAS_MODEL_RESIDUAL_ESC, &
                               GAS_MODEL_INFLATION_ESC, &
                               CO2_RETIREMENTS_LOGIC_STR, &
                               DYNAMIC_GAS_EPUC_STR, &
                               RUN_COAL_MODEL_STR, &
                               RETROFIT_LOGIC_ACTIVE_STR, &
                               MINIMUM_SIZE_OF_RETROFIT, &
                               MINIMUM_YEAR_OF_RETROFIT, &
                               USE_AVERAGE_DAILY_DEMAND_STR, &
                               GAS_PRICE_LAG_COEFF, &
                               GAS_PCA_LAGGED_PRICE_VECTOR, & !  209
                               GAS_PCA_LAGGED_PCA, &
                               GAS_PCA_MONTH_VECTOR, &
                               GAS_PCA_INTERCEPT, &
                               GAS_MONTH_HH_BENCH, &
                               USE_PRICE_MINIMUMS_CHR,       & !  214
                               HH_VECTOR, &
                               LMP_ACTIVE, &
                               GRX_RPS_MODULE_ACTIVE

!
         IF(RETROFIT_LOGIC_ACTIVE_STR == 'T') THEN
            CALL GET_RETRO_AVAIL_PERIOD(Retro_Project_First_Year_Avail, &
                                        Retro_Project_Last_Year_Avail)
            IF(Retro_Project_First_Year_Avail <= get_BASE_YEAR() + IREC .AND. &
                 get_BASE_YEAR() + IREC <= Retro_Project_Last_Year_Avail) THEN
            ELSE
               RETROFIT_LOGIC_ACTIVE_STR = 'F'
            ENDIF
         ENDIF
!
!  ASSET ANALYST TRANSFER 1/16/03 MSG
!
         IF(INDEX(TRANSFER_TO_ASSET_ANALYST,'No ') /= 0) THEN
            BEGIN_ASSET_ANALYST_TRANSFER = 25000
            USE_TRANSACT_ANALYST_RESULTS = .FALSE.
            START_TRANSFER_MONTH = 13
         ELSE
            START_TRANSFER_MONTH = &
                             GET_MONTH_NUMBER(TRANSFER_TO_ASSET_ANALYST)
            BEGIN_ASSET_ANALYST_TRANSFER = 100*(get_BASE_YEAR()+R_YEAR-1900) &
                                           + START_TRANSFER_MONTH
            BASE_DATE = (get_BASE_YEAR() + R_YEAR - 1900) * 100
            USE_TRANSACT_ANALYST_RESULTS = .TRUE.
            DO MO = 1, 12
               IF(BASE_DATE + MO >= BEGIN_ASSET_ANALYST_TRANSFER) EXIT
               USE_TRANSACT_ANALYST_RESULTS(MO) = .FALSE.
            ENDDO
         ENDIF
!
         IF(R_YEAR == 1) THEN
            FIRST_YEAR_PRICE_MODE = 19999
         ENDIF
!
         FIRST_MONTHLY_TRANSACT = 13
         LAST_MONTHLY_TRANSACT = 0
         DO MO = 1, 12
            IF(MONTHLY_TRANSACT_SWITCH(MO) == 'T')THEN
               IF(FIRST_MONTHLY_TRANSACT == 13 ) THEN
                  FIRST_MONTHLY_TRANSACT = MO
               ENDIF
               LAST_MONTHLY_TRANSACT = MO
            ENDIF
         ENDDO
!
         LOCAL_YEAR = R_YEAR ! 8/14/95. GAT. R_YEAR CALLED RESET TO SAVE_YEAR
!  RUN SPECS STUFF
         RESOURCE_TIMING_SWITCH = &
                             INDEX(RESOURCE_TIMING_SWITCH_STR,'AD') /= 0
         ACCUM_MAINTENANCE = ACCUM_MAINTENANCE_CHR == 'T'
!
         IF(TRANSACTION_MODE_CHR == 'C') THEN
            TRANSACTION_MODE_NUMBER = 2
         ELSEIF(TRANSACTION_MODE_CHR == 'S') THEN
            TRANSACTION_MODE_NUMBER = 3
         ELSEIF(TRANSACTION_MODE_CHR == 'T') THEN
            TRANSACTION_MODE_NUMBER = 4
         ELSEIF(TRANSACTION_MODE_CHR == 'A') THEN
            TRANSACTION_MODE_NUMBER = 5
         ELSE
            TRANSACTION_MODE_NUMBER = 1
         ENDIF
         RUN_TRANSACT = RUN_TRANSACT_CHR /= 'F'
         ONLY_BUY_TRANSACT = RUN_TRANSACT_CHR == 'B'
         ONLY_SELL_TRANSACT = RUN_TRANSACT_CHR == 'S'
         ONLY_DISPATCH_TRANSACT = RUN_TRANSACT_CHR == 'D' .OR. &
                                  RUN_TRANSACT_CHR == 'C'
         USE_TRANSACT_LOADS = RUN_TRANSACT_CHR == 'M' .OR. &
                              RUN_TRANSACT_CHR == 'B' .OR. &
                              RUN_TRANSACT_CHR == 'S' .OR. &
                              RUN_TRANSACT_CHR == 'C' .OR. &
                              RUN_TRANSACT_CHR == 'E' .OR. &
                              RUN_TRANSACT_CHR == 'P' .OR. &
                              RUN_TRANSACT_CHR == 'A' .OR. &
                              RUN_TRANSACT_CHR == 'R'
         CENTRAL_DISPATCH_TRANSACT = RUN_TRANSACT_CHR == 'C'
         STRICT_MARKET_PRICE = RUN_TRANSACT_CHR == 'P'
! 090606.
         MULTI_AREA_PRICE = RUN_TRANSACT_CHR == 'A' !  .OR.
!     +                                   RUN_TRANSACT_CHR == 'R'
         REGIONAL_AREA_PRICE =  RUN_TRANSACT_CHR == 'R'
!
         RUN_MULTIAREA_TRANSACT = RUN_TRANSACT_CHR == 'M'
!
         IF(STRICT_MARKET_PRICE .AND. &
                                    FIRST_YEAR_PRICE_MODE == 19999) THEN
            FIRST_YEAR_PRICE_MODE =  100.*(get_BASE_YEAR()+R_YEAR-1900)
         ENDIF
!
         MAX_TRANS_LOADS = 0
         DO I = 1, 25
            IF(TRANS_GROUP_LOAD_IN_FILE(I) == 0) CYCLE
            MAX_TRANS_LOADS = I
         ENDDO
!
         SECOND_FUEL_EMIS_DISPATCH = &
                                    SECOND_FUEL_EMIS_DISPATCH_CHR /= 'F'
!
         CALANDER_CORRECT = CALANDER_CORRECT_CHR /= 'F'
         IF(R_YEAR == 1) THEN
            OFFSET_MAINTENANCE_ACTIVE = OFFSET_MAINTENANCE_SWITCH
         ENDIF
         FACET_IS_PRODUCTION_METHOD = &
                              INDEX(PRODUCTION_COST_METHOD_CHR,'F') /= 0
         IF(FACET_IS_PRODUCTION_METHOD) THEN
            PRODUCTION_COST_METHOD = 2
         ELSE
            PRODUCTION_COST_METHOD = &
                             INDEX('1234BDC',PRODUCTION_COST_METHOD_CHR)
            IF(PRODUCTION_COST_METHOD > 4) &
                     PRODUCTION_COST_METHOD = PRODUCTION_COST_METHOD - 4
         ENDIF
         call set_production_periods_in(int(12,2))
         IF(PRODUCTION_PERIODS_CHR == 'A') then
            call set_production_periods_in(int(1,2))
         end if

         IF(LOCAL_YEAR == AVAIL_DATA_YEARS-1) THEN
            L_TRANSACTION_BUY_SPREAD = TRANSACTION_BUY_SPREAD
            L_TRANSACTION_SELL_SPREAD = TRANSACTION_SELL_SPREAD
            L_UNSERVED_ENERGY_PRICE =   UNSERVED_ENERGY_PRICE
            L_BTU_TAX_IN_ADJUSTMENT_CLAUSE = &
                                            BTU_TAX_IN_ADJUSTMENT_CLAUSE
            L_RM_PURCHASE_FIXED_COST =  RM_PURCHASE_FIXED_COST
            L_RM_PURCHASE_VARIABLE_COST = RM_PURCHASE_VARIABLE_COST
            L_RM_SALE_FIXED_AMOUNT = RM_SALE_FIXED_AMOUNT
            L_RM_SALE_VARIABLE_AMOUNT = RM_SALE_VARIABLE_AMOUNT
            ESC_DOE_NF_DISPOSAL_RATES = DOE_NF_DISPOSAL_RATES
            ESC_NUC_DECOMMISSIONING_RATES = NUC_DECOMMISSIONING_RATES
            ESC_DOE_R300_DISPOSAL_FEE = DOE_R300_DISPOSAL_FEE
            ESC_TRANSACT_UNSERVED_COST = TRANSACT_UNSERVED_COST
            ESC_TRANSACT_ABUNDANCE_COST = TRANSACT_ABUNDANCE_COST

         ELSEIF(LOCAL_YEAR == AVAIL_DATA_YEARS) THEN
!
            CALL IMPLIED_ESC(L_TRANSACTION_BUY_SPREAD, &
                             TRANSACTION_BUY_SPREAD)
            CALL IMPLIED_ESC(L_TRANSACTION_SELL_SPREAD, &
                             TRANSACTION_SELL_SPREAD)
            CALL IMPLIED_ESC(L_UNSERVED_ENERGY_PRICE, &
                             UNSERVED_ENERGY_PRICE)
            CALL IMPLIED_ESC(L_BTU_TAX_IN_ADJUSTMENT_CLAUSE, &
                             BTU_TAX_IN_ADJUSTMENT_CLAUSE)
            CALL IMPLIED_ESC(L_RM_PURCHASE_FIXED_COST, &
                             RM_PURCHASE_FIXED_COST)
            CALL IMPLIED_ESC(L_RM_PURCHASE_VARIABLE_COST, &
                             RM_PURCHASE_VARIABLE_COST)
            CALL IMPLIED_ESC(L_RM_SALE_FIXED_AMOUNT, &
                             RM_SALE_FIXED_AMOUNT)
            CALL IMPLIED_ESC(L_RM_SALE_VARIABLE_AMOUNT, &
                             RM_SALE_VARIABLE_AMOUNT)
            CALL IMPLIED_ESC(ESC_DOE_NF_DISPOSAL_RATES(1), &
                                               DOE_NF_DISPOSAL_RATES(1))
            CALL IMPLIED_ESC(ESC_DOE_NF_DISPOSAL_RATES(2), &
                                               DOE_NF_DISPOSAL_RATES(2))
            CALL IMPLIED_ESC(ESC_NUC_DECOMMISSIONING_RATES(1), &
                                           NUC_DECOMMISSIONING_RATES(1))
            CALL IMPLIED_ESC(ESC_NUC_DECOMMISSIONING_RATES(2), &
                                           NUC_DECOMMISSIONING_RATES(2))
            CALL IMPLIED_ESC(ESC_DOE_R300_DISPOSAL_FEE(1), &
                                               DOE_R300_DISPOSAL_FEE(1))
            CALL IMPLIED_ESC(ESC_DOE_R300_DISPOSAL_FEE(2), &
                                               DOE_R300_DISPOSAL_FEE(2))
            CALL IMPLIED_ESC(ESC_TRANSACT_UNSERVED_COST, &
                                                 TRANSACT_UNSERVED_COST)
            CALL IMPLIED_ESC(ESC_TRANSACT_ABUNDANCE_COST, &
                                                TRANSACT_ABUNDANCE_COST)
!
            P_TRANSACTION_BUY_SPREAD = 1.
            P_TRANSACTION_SELL_SPREAD = 1.
            P_UNSERVED_ENERGY_PRICE = 1.
            P_BTU_TAX_IN_ADJUSTMENT_CLAUSE = 1.
            P_RM_PURCHASE_FIXED_COST = 1.
            P_RM_PURCHASE_VARIABLE_COST = 1.
            P_RM_SALE_FIXED_AMOUNT = 1.
            P_RM_SALE_VARIABLE_AMOUNT = 1.
            P_DOE_NF_DISPOSAL_RATES = 1.
            P_NUC_DECOMMISSIONING_RATES = 1.
            P_DOE_R300_DISPOSAL_FEE = 1.
            P_TRANSACT_UNSERVED_COST = 1.
            P_TRANSACT_ABUNDANCE_COST = 1.
!
         ELSEIF(LOCAL_YEAR > AVAIL_DATA_YEARS) THEN
!
            P_TRANSACT_UNSERVED_COST =  P_TRANSACT_UNSERVED_COST * &
                                              ESC_TRANSACT_UNSERVED_COST
            P_TRANSACT_ABUNDANCE_COST =  P_TRANSACT_ABUNDANCE_COST * &
                                             ESC_TRANSACT_ABUNDANCE_COST
            P_DOE_NF_DISPOSAL_RATES(1) = P_DOE_NF_DISPOSAL_RATES(1) * &
                                         ESC_DOE_NF_DISPOSAL_RATES(1)
            P_DOE_NF_DISPOSAL_RATES(2) = P_DOE_NF_DISPOSAL_RATES(2) * &
                                         ESC_DOE_NF_DISPOSAL_RATES(2)
            P_NUC_DECOMMISSIONING_RATES(1) = &
                                        P_NUC_DECOMMISSIONING_RATES(1) * &
                                        ESC_NUC_DECOMMISSIONING_RATES(1)
            P_NUC_DECOMMISSIONING_RATES(2) = &
                                        P_NUC_DECOMMISSIONING_RATES(2) * &
                                        ESC_NUC_DECOMMISSIONING_RATES(2)
            P_DOE_R300_DISPOSAL_FEE(1) = P_DOE_R300_DISPOSAL_FEE(1) * &
                                         ESC_DOE_R300_DISPOSAL_FEE(1)
            P_DOE_R300_DISPOSAL_FEE(2) = P_DOE_R300_DISPOSAL_FEE(2) * &
                                         ESC_DOE_R300_DISPOSAL_FEE(2)
!
            DOE_NF_DISPOSAL_RATES(1) = DOE_NF_DISPOSAL_RATES(1) * &
                                       P_DOE_NF_DISPOSAL_RATES(1)
            DOE_NF_DISPOSAL_RATES(2) = DOE_NF_DISPOSAL_RATES(2) * &
                                       P_DOE_NF_DISPOSAL_RATES(2)
            NUC_DECOMMISSIONING_RATES(1) = NUC_DECOMMISSIONING_RATES(1)* &
                                          P_NUC_DECOMMISSIONING_RATES(1)
            NUC_DECOMMISSIONING_RATES(2) = NUC_DECOMMISSIONING_RATES(2)* &
                                          P_NUC_DECOMMISSIONING_RATES(2)
            DOE_R300_DISPOSAL_FEE(1) = DOE_R300_DISPOSAL_FEE(1) * &
                                       P_DOE_R300_DISPOSAL_FEE(1)
            DOE_R300_DISPOSAL_FEE(2) = DOE_R300_DISPOSAL_FEE(2) * &
                                       P_DOE_R300_DISPOSAL_FEE(2)
            TRANSACT_UNSERVED_COST =  TRANSACT_UNSERVED_COST * &
                                               P_TRANSACT_UNSERVED_COST
            TRANSACT_ABUNDANCE_COST = TRANSACT_ABUNDANCE_COST * &
                                               P_TRANSACT_ABUNDANCE_COST
!
            P_TRANSACTION_BUY_SPREAD = &
               P_TRANSACTION_BUY_SPREAD * L_TRANSACTION_BUY_SPREAD
            P_TRANSACTION_SELL_SPREAD = &
               P_TRANSACTION_SELL_SPREAD * L_TRANSACTION_SELL_SPREAD
            P_UNSERVED_ENERGY_PRICE = &
               P_UNSERVED_ENERGY_PRICE * L_UNSERVED_ENERGY_PRICE
            P_BTU_TAX_IN_ADJUSTMENT_CLAUSE = &
               P_BTU_TAX_IN_ADJUSTMENT_CLAUSE * &
                                L_BTU_TAX_IN_ADJUSTMENT_CLAUSE
            P_RM_PURCHASE_FIXED_COST = &
               P_RM_PURCHASE_FIXED_COST * L_RM_PURCHASE_FIXED_COST
            P_RM_PURCHASE_VARIABLE_COST = &
               P_RM_PURCHASE_VARIABLE_COST * &
                                     L_RM_PURCHASE_VARIABLE_COST
            P_RM_SALE_FIXED_AMOUNT = &
               P_RM_SALE_FIXED_AMOUNT * L_RM_SALE_FIXED_AMOUNT
            P_RM_SALE_VARIABLE_AMOUNT = &
               P_RM_SALE_VARIABLE_AMOUNT * L_RM_SALE_VARIABLE_AMOUNT
!
            TRANSACTION_BUY_SPREAD = &
                         TRANSACTION_BUY_SPREAD*P_TRANSACTION_BUY_SPREAD
            TRANSACTION_SELL_SPREAD = &
                       TRANSACTION_SELL_SPREAD * TRANSACTION_SELL_SPREAD
            UNSERVED_ENERGY_PRICE = &
                           UNSERVED_ENERGY_PRICE*P_UNSERVED_ENERGY_PRICE
            BTU_TAX_IN_ADJUSTMENT_CLAUSE = &
                        BTU_TAX_IN_ADJUSTMENT_CLAUSE* &
                                          P_BTU_TAX_IN_ADJUSTMENT_CLAUSE
            RM_PURCHASE_FIXED_COST = &
                         RM_PURCHASE_FIXED_COST*P_RM_PURCHASE_FIXED_COST
            RM_PURCHASE_VARIABLE_COST = &
                 RM_PURCHASE_VARIABLE_COST * P_RM_PURCHASE_VARIABLE_COST
            RM_SALE_FIXED_AMOUNT = &
                             RM_SALE_FIXED_AMOUNT*P_RM_SALE_FIXED_AMOUNT
            RM_SALE_VARIABLE_AMOUNT = &
                       RM_SALE_VARIABLE_AMOUNT*P_RM_SALE_VARIABLE_AMOUNT
!
         ENDIF
         RM_PURCHASE_VARIABLE_COST = 1000.*RM_PURCHASE_VARIABLE_COST
         RM_SALE_VARIABLE_AMOUNT = 1000.*RM_SALE_VARIABLE_AMOUNT
         UPDATE_PRODUCTION_PARAMETERS =  IOS /= 0
!
!  CHARACTER VALUES ARE IN A SEPARATE ROUTINE FOR LF95
!
         VOID_CHR = SAVE_CHR_PP_VALUES(FACET_IS_PRODUCTION_METHOD, &
                                       TRANS_REPRESENTATION_CHR, &
                                       IGNORE_NON_UTILITY_CHR, &
                                       REMOVE_NIGHTIME_SCARCITY_CHR, &
                                       TRANS_TIME_FRAME_CHR, &
                                       CAPACITY_PLANNING_TRIGGER_VALUE, &
                                       FIRST_BLOCK_DISP_SWITCH_ACTIVE)
      RETURN

      ENTRY USE_PRICE_MINIMUMS

         USE_PRICE_MINIMUMS =    USE_PRICE_MINIMUMS_CHR == 'T'
      RETURN

      ENTRY GET_HH_PARAMETERS(TEMP_ESC,TEMP_COEFF,DYNAMIC_GAS_EPUC)

         TEMP_ESC(1) = GAS_MODEL_WTI_ESC
         TEMP_ESC(2) = GAS_MODEL_DEMAND_ESC
         TEMP_ESC(3) = GAS_MODEL_EPUC_ESC
         TEMP_ESC(4) = GAS_MODEL_STORAGE_UTIL_ESC
         TEMP_ESC(5) = GAS_MODEL_RESIDUAL_ESC
         TEMP_ESC(6) = GAS_MODEL_INFLATION_ESC
         TEMP_ESC(7) = GAS_PCA_LAGGED_PRICE_VECTOR
         TEMP_ESC(8) = GAS_PCA_MONTH_VECTOR
         TEMP_ESC(9) = GAS_MONTH_HH_BENCH
         TEMP_COEFF(1) = GAS_MODEL_INTERCEPT
         TEMP_COEFF(2) = GAS_MODEL_WTI_COEFF
         TEMP_COEFF(3) = GAS_MODEL_DEMAND_COEFF
         TEMP_COEFF(4) = GAS_MODEL_EPUC_COEFF
         TEMP_COEFF(5) = GAS_MODEL_STORAGE_UTIL_COEFF
         TEMP_COEFF(6) = GAS_PRICE_LAG_COEFF
         TEMP_COEFF(7) = GAS_PCA_LAGGED_PCA
         TEMP_COEFF(8) = GAS_PCA_INTERCEPT
         DYNAMIC_GAS_EPUC =   DYNAMIC_GAS_EPUC_STR == 'T'
         GET_HH_PARAMETERS = .TRUE.
      RETURN
!
!  PRODUCTION RUN SPECS ENTRY'S
!

      ENTRY GET_TIE_GROUP_LIMIT(R_TIE_GROUP_LIMIT)

         GET_TIE_GROUP_LIMIT = .TRUE.
         R_TIE_GROUP_LIMIT(1) = TIE_GROUP_LIMIT(1)
         R_TIE_GROUP_LIMIT(2) = TIE_GROUP_LIMIT(2)
         R_TIE_GROUP_LIMIT(3) = TIE_GROUP_LIMIT(3)
      RETURN

      ENTRY GET_FIRST_YEAR_PRICE_MODE

         GET_FIRST_YEAR_PRICE_MODE = FIRST_YEAR_PRICE_MODE
      RETURN

      ENTRY GET_STRATEGIC_RETIRMENTS_LOGIC(R_RETIREMENTS_PROFIT_PER_KW)

         R_RETIREMENTS_PROFIT_PER_KW = RETIREMENTS_PROFIT_PER_KW
         GET_STRATEGIC_RETIRMENTS_LOGIC = &
                                       strategic_rtrmts_logic == 'T'
      RETURN

      ENTRY GET_TRANSACT_ABUNDANCE_COST

         GET_TRANSACT_ABUNDANCE_COST = TRANSACT_ABUNDANCE_COST
      RETURN

      ENTRY GET_ADDITIONS_PROFIT_PER_MWH

         GET_ADDITIONS_PROFIT_PER_MWH = ADDITIONS_PROFIT_PER_MWH
      RETURN

      ENTRY HISTORICAL_PRODUCTION_DATE

         HISTORICAL_PRODUCTION_DATE = BEGIN_ASSET_ANALYST_TRANSFER
      RETURN

      ENTRY GET_MAXIMUM_ANNUAL_MRX_CAP

         GET_MAXIMUM_ANNUAL_MRX_CAP = MAXIMUM_ANNUAL_MRX_CAP
      RETURN

      ENTRY YES_UNIT_COMMITMENT_LOGIC

         YES_UNIT_COMMITMENT_LOGIC = UNIT_COMMITMENT_LOGIC == 'T'
      RETURN

      ENTRY YES_PRICE_LAST_MW

         YES_PRICE_LAST_MW = PRICE_LAST_MW == 'T'
      RETURN

      ENTRY YES_LAMBDA_PRICE

         YES_LAMBDA_PRICE =  PRICE_LAST_MW == 'L'
      RETURN

      ENTRY YES_PRICE_LAST_MW_AS_UNSERVED

         YES_PRICE_LAST_MW_AS_UNSERVED = PRICE_LAST_MW == 'U'
      RETURN

      ENTRY LONG_PATH_LOGIC

         LONG_PATH_LOGIC =   LONG_PATH == 'T'
      RETURN

      ENTRY REGIONAL_PA_MAINTENANCE_LOGIC

         REGIONAL_PA_MAINTENANCE_LOGIC =  REGIONAL_MAINTENANCE == 'P'
      RETURN

      ENTRY DEPTH_OF_MARKET_LOGIC

         DEPTH_OF_MARKET_LOGIC =  DEPTH_OF_MARKET == 'T'
      RETURN

      ENTRY GET_MAXIMUM_TRANSACTION_SIZE

         GET_MAXIMUM_TRANSACTION_SIZE = MAXIMUM_TRANSACTION_SIZE
      RETURN

      ENTRY USE_5X16_LOGIC_CONSTRAINTS

         USE_5X16_LOGIC_CONSTRAINTS =  USE_5X16 == 'T'
      RETURN

      ENTRY YES_NEW_BUILD_ADDITIONS_ACTIVE

         YES_NEW_BUILD_ADDITIONS_ACTIVE = &
                                       NEW_BUILD_ADDITIONS_ACTIVE == 'T'
      RETURN

      ENTRY GET_NEW_BUILD_PERCENTS( &
                                    R_GLOBAL_CAPACITY_VALUE_2, &
                                    R_GLOBAL_CAPACITY_VALUE_3, &
                                    R_GLOBAL_CAPACITY_VALUE_4, &
                                    R_GLOBAL_CAPACITY_VALUE_5, &
                                    R_GLOBAL_CAPACITY_VALUE_7)
         R_GLOBAL_CAPACITY_VALUE_2 = &
                               UNDER_CONSTRUCTION_PERCENT/100.
         R_GLOBAL_CAPACITY_VALUE_3 = &
                               ADVANCED_DEVELOPMENT_PERCENT/100.
         R_GLOBAL_CAPACITY_VALUE_4 = &
                               EARLY_DEVELOPMENT_PERCENT/100.
         R_GLOBAL_CAPACITY_VALUE_5 = &
                               PROPOSED_PERCENT/100.
         R_GLOBAL_CAPACITY_VALUE_7 = &
                               INDEFIN_POSTPHONED_PERCENT/100.
         GET_NEW_BUILD_PERCENTS = .TRUE.
      RETURN

      ENTRY YES_DECOMMIT_THERMAL_RESOURCES

         YES_DECOMMIT_THERMAL_RESOURCES = &
                                       DECOMMIT_THERMAL_RESOURCES == 'T'
      RETURN

      ENTRY YES_EMERGENCY_MEETS_SPIN

         YES_EMERGENCY_MEETS_SPIN = EMERGENCY_MEETS_SPIN == 'T'
      RETURN

      ENTRY YES_INCLUDE_ICAP_REVENUE

         YES_INCLUDE_ICAP_REVENUE = INCLUDE_ICAP_REVENUE == 'T'
      RETURN

      ENTRY YES_REMEMBER_LAST_HOUR

         YES_REMEMBER_LAST_HOUR = REMEMBER_LAST_HOUR == 'T'
      RETURN

      ENTRY YES_COMMIT_ON_TOTAL_COST_ENV

         YES_COMMIT_ON_TOTAL_COST_ENV =   COMMIT_ON_TOTAL_COST == 'E'
      RETURN

      ENTRY YES_COMMIT_ON_TOTAL_COST

         IF(COMMIT_ON_TOTAL_COST == 'T' .OR. &
                                    COMMIT_ON_TOTAL_COST == 'E') THEN
            YES_COMMIT_ON_TOTAL_COST = .TRUE.
         ELSE
            YES_COMMIT_ON_TOTAL_COST = .FALSE.
         ENDIF
      RETURN

      ENTRY YES_RESOURCE_TO_LOAD_ALLOC( &
                                  R_FUEL_AND_PURCHASE_COST_CAP, &
                                  R_SYS_EMERGENCY_MW_FLOOR, &
                                  R_SYS_EMERGENCY_COST_CAP)

         YES_RESOURCE_TO_LOAD_ALLOC =  RESOURCE_TO_LOAD_ALLOC == 'U'
         R_FUEL_AND_PURCHASE_COST_CAP = FUEL_AND_PURCHASE_COST_CAP
         R_SYS_EMERGENCY_MW_FLOOR = SYS_EMERGENCY_MW_FLOOR
         R_SYS_EMERGENCY_COST_CAP = SYS_EMERGENCY_COST_CAP
      RETURN

      ENTRY ALLOW_MARKET_ARBITRAGE

         ALLOW_MARKET_ARBITRAGE = ALLOW_MARKET_ARBITRAGE_STR == 'T'
      RETURN

      ENTRY YES_ZONAL_LEVEL_MARKET

         YES_ZONAL_LEVEL_MARKET =    TRANS_ZONAL_NODAL_STR == 'Z'
      RETURN

      ENTRY REGION_OR_STATE_POWER_MAP

         REGION_OR_STATE_POWER_MAP = &
                                    REGION_OR_STATE_POWER_MAP_STR == 'R'
      RETURN

      ENTRY GET_CO2_RETIREMENTS_LOGIC
          ! co2_retirements_logic_str="F"/asdf
         GET_CO2_RETIREMENTS_LOGIC =   CO2_RETIREMENTS_LOGIC_STR == 'T'
      RETURN

      ENTRY GET_LEGACY_PRIMARY_MOVER

         GET_LEGACY_PRIMARY_MOVER =   LEGACY_PRIMARY_MOVER_STR == 'T'
      RETURN

      ENTRY GET_ACU_POWER_ID_INDEX

         GET_ACU_POWER_ID_INDEX = ACU_POWER_ID_INDEX_STR == 'T'
      RETURN

      ENTRY GET_HH_VECTOR

         GET_HH_VECTOR = HH_VECTOR
      RETURN

      ENTRY LP_GAS_MODEL

         LP_GAS_MODEL = LP_GAS_MODEL_STR == 'T' .AND. &
                              (RUN_GAS_MODEL_STR == 'T' .OR. &
                                    RUN_GAS_MODEL_STR == 'H' .OR. &
                                               RUN_GAS_MODEL_STR == 'G')
      RETURN

      ENTRY RUN_GAS_MODEL

         RUN_GAS_MODEL =        RUN_GAS_MODEL_STR == 'T' .OR. &
                                       RUN_GAS_MODEL_STR == 'H' .OR. &
                                                RUN_GAS_MODEL_STR == 'G'
      RETURN

      ! STOPS RETRO IF RT FILE DOESN'T EXIST
      ENTRY SET_RETROFIT_LOGIC_OFF()  
         RETROFIT_LOGIC_ACTIVE_STR = 'F'
         SET_RETROFIT_LOGIC_OFF = .TRUE.
      RETURN

      ENTRY RETROFIT_LOGIC_ACTIVE(R_TEMP_R4,R_TEMP_I2)

         RETROFIT_LOGIC_ACTIVE = RETROFIT_LOGIC_ACTIVE_STR == 'T'
         R_TEMP_R4 = MINIMUM_SIZE_OF_RETROFIT
         R_TEMP_I2 = MINIMUM_YEAR_OF_RETROFIT
      RETURN

      ENTRY CO2_RETROFIT_LOGIC_ACTIVE()

         RETROFIT_LOGIC_ACTIVE = RETROFIT_LOGIC_ACTIVE_STR == 'T'
      RETURN

      ENTRY USE_AVERAGE_DAILY_DEMAND

         USE_AVERAGE_DAILY_DEMAND = &
                                     USE_AVERAGE_DAILY_DEMAND_STR == 'T'
      RETURN

      ENTRY RUN_COAL_MODEL

         RUN_COAL_MODEL = RUN_COAL_MODEL_STR == 'T' .OR. &
                                             RUN_COAL_MODEL_STR == 'C'
      RETURN

      ENTRY COAL_MODEL_ONLY
         COAL_MODEL_ONLY = .false.
         if (RUN_COAL_MODEL_STR == 'C') then
            COAL_MODEL_ONLY=.true.
         end if
      RETURN

      ENTRY GAS_MODEL_ONLY

         GAS_MODEL_ONLY =       RUN_GAS_MODEL_STR == 'G'
      RETURN

      ENTRY RUN_HH_ONLY

         RUN_HH_ONLY =          RUN_GAS_MODEL_STR == 'H'
      RETURN

      ENTRY FAST_DISPATCH

         FAST_DISPATCH =        FAST_DISPATCH_STR == 'T'
      RETURN

      ENTRY USE_EMIS_IN_MRX

         USE_EMIS_IN_MRX =    USE_EMIS_IN_MRX_STR == 'T'
      RETURN

      ENTRY STRICTLY_POSITIVE_MARGIN

         STRICTLY_POSITIVE_MARGIN =  STRICTLY_POSITIVE_MARGIN_STR == 'T'
      RETURN

      ENTRY GET_DMD_COEFF(TEMP_R1,TEMP_R2)

         GET_DMD_COEFF = .TRUE.
         TEMP_R1 = DMD_COEFF_A
         TEMP_R2 = DMD_COEFF_B*.000001
      RETURN

      ENTRY DISALLOW_REDUNDANT_TRANS

         DISALLOW_REDUNDANT_TRANS =  DISALLOW_REDUNDANT_TRANS_STR == 'T'
      RETURN

      ENTRY YES_MULTI_MARKET_ACTIVE

         YES_MULTI_MARKET_ACTIVE = MULTI_MARKET_ACTIVE == 'T'
      RETURN

      ENTRY YES_PLAN_TO_MWH_OR_KWYR

         YES_PLAN_TO_MWH_OR_KWYR =  PLAN_TO_MWH_OR_KWYR == 'M'
      RETURN

      ENTRY YES_REGIONAL_RESERVE_MARGINS

         YES_REGIONAL_RESERVE_MARGINS = REGIONAL_RESERVE_MARGINS == 'T'
      RETURN

      ENTRY YES_TECH_PERCENT_TRANS_CAP

         YES_TECH_PERCENT_TRANS_CAP =  TECHNOLOGY_SCARCITY_BASIS == 'A'
      RETURN

      ENTRY GET_TECH_PERCENT_INPUT

         GET_TECH_PERCENT_INPUT = TECHNOLOGY_SCARCITY_STEP/100.
      RETURN

      ENTRY GET_TECH_POWER

         GET_TECH_POWER = TECHNOLOGY_SCARCITY_POWER
      RETURN

      ENTRY YES_TURN_OFF_POLY

         YES_TURN_OFF_POLY =          TURN_OFF_POLY == 'T'
      RETURN

      ENTRY REGIONAL_TG_MAINTENANCE_LOGIC

         REGIONAL_TG_MAINTENANCE_LOGIC =  REGIONAL_MAINTENANCE == 'T'
      RETURN

      ENTRY PICK_FOR_SEED_OPTIONS(R_FOR_SEED_OPTIONS)

         R_FOR_SEED_OPTIONS = FOR_SEED_OPTIONS
         PICK_FOR_SEED_OPTIONS = .TRUE.
      RETURN

      ENTRY YES_ADVANCED_PRICE_MODELING

         YES_ADVANCED_PRICE_MODELING = ADVANCED_PRICE_MODELING == 'T'
      RETURN

      ENTRY YES_BIDDING_LOGIC

         YES_BIDDING_LOGIC =     BIDDING_LOGIC == 'D' .OR. &
                                                  BIDDING_LOGIC == 'M'
      RETURN

      ENTRY MODIFIED_BIDDING_LOGIC

         MODIFIED_BIDDING_LOGIC =   BIDDING_LOGIC == 'M'
      RETURN

      ENTRY YES_USE_MINIMUM_RM(R_USE_MINIMUM_RM)

         R_USE_MINIMUM_RM = USE_MINIMUM_RM
         YES_USE_MINIMUM_RM = .TRUE.


      ENTRY YES_DETAILED_MAINTENANCE()

         YES_DETAILED_MAINTENANCE = DETAILED_MAINTENANCE == 'T'
      RETURN

      ENTRY YES_DETAILED_FOR()

         YES_DETAILED_FOR = DETAILED_FOR == 'T' .OR. DETAILED_FOR == 'D'
      RETURN

      ENTRY YES_FREQUENCY_DURATION()

         YES_FREQUENCY_DURATION =  DETAILED_FOR == 'D'
      RETURN

      ENTRY YES_DETAILED_TRANSFER_PRICING()

         YES_DETAILED_TRANSFER_PRICING = &
                                        DETAILED_TRANSFER_PRICING == 'T'
      RETURN

      ENTRY YES_PRICES_ARE_CAPPED()

         YES_PRICES_ARE_CAPPED = PRICES_ARE_CAPPED == 'T'
      RETURN

      ENTRY YES_DYNAMIC_FUEL_PRICING()

         YES_DYNAMIC_FUEL_PRICING = DYNAMIC_FUEL_PRICING == 'T'
      RETURN

      ENTRY TRANSFER_PRICING_VECTORS( &
                                       R_TRANS_AC_REV, &
                                       R_TRANS_AC_REV_ALLOC, &
                                       R_TRANS_AC_COST, &
                                       R_TRANS_AC_COST_ALLOC)

!
         IF(TRANS_AC_REV /= 0 .AND. &
               TRANS_AC_REV_ALLOC /= 0 .AND. &
                            TRANS_AC_COST /= 0 .AND. &
                               TRANS_AC_COST_ALLOC /= 0) THEN
!
            R_TRANS_AC_REV = TRANS_AC_REV
            R_TRANS_AC_REV_ALLOC = TRANS_AC_REV_ALLOC
            R_TRANS_AC_COST = TRANS_AC_COST
            R_TRANS_AC_COST_ALLOC = TRANS_AC_COST_ALLOC
!
            TRANSFER_PRICING_VECTORS = .TRUE.
         ELSE
            TRANSFER_PRICING_VECTORS = .FALSE.
         ENDIF
!
      RETURN

      ENTRY YES_GLOBAL_SCARCITY_BUY_N_SELL()

         YES_GLOBAL_SCARCITY_BUY_N_SELL = &
                                       GLOBAL_SCARCITY_BUY_N_SELL == 'T'
      RETURN

      ENTRY GET_GLOBAL_SCARCITY( R_GLOBAL_CAPACITY_VALUE_1, &
                                 R_GLOBAL_CAPACITY_VALUE_2, &
                                 R_GLOBAL_CAPACITY_VALUE_3, &
                                 R_GLOBAL_CAPACITY_PERCENT_1, &
                                 R_GLOBAL_CAPACITY_PERCENT_2, &
                                 R_GLOBAL_CAPACITY_PERCENT_3, &
                                 R_GLOBAL_CAPACITY_VALUE_4, &
                                 R_GLOBAL_CAPACITY_VALUE_5, &
                                 R_GLOBAL_CAPACITY_VALUE_6, &
                                 R_GLOBAL_CAPACITY_VALUE_7, &
                                 R_GLOBAL_CAPACITY_VALUE_8, &
                                 R_GLOBAL_CAPACITY_VALUE_9, &
                                 R_GLOBAL_CAPACITY_VALUE_10, &
                                 R_GLOBAL_CAPACITY_PERCENT_4, &
                                 R_GLOBAL_CAPACITY_PERCENT_5, &
                                 R_GLOBAL_CAPACITY_PERCENT_6, &
                                 R_GLOBAL_CAPACITY_PERCENT_7, &
                                 R_GLOBAL_CAPACITY_PERCENT_8, &
                                 R_GLOBAL_CAPACITY_PERCENT_9, &
                                 R_GLOBAL_CAPACITY_PERCENT_10)
!

!
         R_GLOBAL_CAPACITY_VALUE_1 = GLOBAL_CAPACITY_VALUE(1)
         R_GLOBAL_CAPACITY_VALUE_2 = GLOBAL_CAPACITY_VALUE(2)
         R_GLOBAL_CAPACITY_VALUE_3 = GLOBAL_CAPACITY_VALUE(3)
         R_GLOBAL_CAPACITY_PERCENT_1 = GLOBAL_CAPACITY_PERCENT(1)
         R_GLOBAL_CAPACITY_PERCENT_2 = GLOBAL_CAPACITY_PERCENT(2)
         R_GLOBAL_CAPACITY_PERCENT_3 = GLOBAL_CAPACITY_PERCENT(3)
!
         R_GLOBAL_CAPACITY_VALUE_4 = NEW_GLOBAL_CAPACITY_VALUE(1)
         R_GLOBAL_CAPACITY_VALUE_5 = NEW_GLOBAL_CAPACITY_VALUE(2)
         R_GLOBAL_CAPACITY_VALUE_6 = NEW_GLOBAL_CAPACITY_VALUE(3)
         R_GLOBAL_CAPACITY_VALUE_7 = NEW_GLOBAL_CAPACITY_VALUE(4)
         R_GLOBAL_CAPACITY_VALUE_8 = NEW_GLOBAL_CAPACITY_VALUE(5)
         R_GLOBAL_CAPACITY_VALUE_9 = NEW_GLOBAL_CAPACITY_VALUE(6)
         R_GLOBAL_CAPACITY_VALUE_10 = NEW_GLOBAL_CAPACITY_VALUE(7)
!
         R_GLOBAL_CAPACITY_PERCENT_4 = NEW_GLOBAL_CAPACITY_PERCENT(1)
         R_GLOBAL_CAPACITY_PERCENT_5 = NEW_GLOBAL_CAPACITY_PERCENT(2)
         R_GLOBAL_CAPACITY_PERCENT_6 = NEW_GLOBAL_CAPACITY_PERCENT(3)
         R_GLOBAL_CAPACITY_PERCENT_7 = NEW_GLOBAL_CAPACITY_PERCENT(4)
         R_GLOBAL_CAPACITY_PERCENT_8 = NEW_GLOBAL_CAPACITY_PERCENT(5)
         R_GLOBAL_CAPACITY_PERCENT_9 = NEW_GLOBAL_CAPACITY_PERCENT(6)
         R_GLOBAL_CAPACITY_PERCENT_10 = NEW_GLOBAL_CAPACITY_PERCENT(7)
!
         GET_GLOBAL_SCARCITY = 0
      RETURN

      ENTRY RUN_TRANSACT_THIS_MONTH(R_MONTH)

          RUN_TRANSACT_THIS_MONTH = &
                                 MONTHLY_TRANSACT_SWITCH(R_MONTH) == 'T'
      RETURN

      ENTRY GET_TRANSACT_UNSERVED_COST

          GET_TRANSACT_UNSERVED_COST = TRANSACT_UNSERVED_COST
      RETURN

      ENTRY FIRST_LAST_MONTH_OF_TRANSACT(R_LAST_MONTH)

          R_LAST_MONTH = LAST_MONTHLY_TRANSACT
          FIRST_LAST_MONTH_OF_TRANSACT = FIRST_MONTHLY_TRANSACT
      RETURN

      ENTRY MONTHLY_MAINTENANCE_PENALTY

         MONTHLY_MAINTENANCE_PENALTY = MONTHLY_MAINTENANCE_PENALTY_NUM
      RETURN

      ENTRY GET_TECH_SCAR_MONTH_VECTOR

         GET_TECH_SCAR_MONTH_VECTOR = TECH_SCAR_MONTH_VECTOR
      RETURN

      ENTRY MAX_TRANS_ITERATIONS

         MAX_TRANS_ITERATIONS = MAX_TRANS_ITERATIONS_NUM
      RETURN

      ENTRY RETURN_FUEL_ALLOCATION_SWITCH

         RETURN_FUEL_ALLOCATION_SWITCH=ALLOCATE_FUEL_RIGHT_TO_LEFT=='H'
      RETURN

      ENTRY APPLY_TRANS_REV_TO_WHOLESALE

         APPLY_TRANS_REV_TO_WHOLESALE = &
                              APPLY_TRANS_REV_CHR == 'W' .OR. &
                                     APPLY_TRANS_REV_CHR == 'R' .OR. &
                                              APPLY_TRANS_REV_CHR == 'I'
      RETURN

      ENTRY APPLY_RETAIL_AS_WHOLESALE

         APPLY_RETAIL_AS_WHOLESALE = APPLY_TRANS_REV_CHR == 'R'
      RETURN

      ENTRY IGNORE_NATIVE_PURCHASE_USE_W

         IGNORE_NATIVE_PURCHASE_USE_W =  APPLY_TRANS_REV_CHR == 'I'
      RETURN

      ENTRY TRANSACTION_MODE

         TRANSACTION_MODE = TRANSACTION_MODE_NUMBER
      RETURN

      ENTRY HOURLY_LOAD_OUT

         HOURLY_LOAD_OUT = HOURLY_LOAD_OUT_FILE
      RETURN

      ENTRY HOURLY_LOAD_IN

         HOURLY_LOAD_IN = HOURLY_LOAD_IN_FILE
      RETURN

      ENTRY TRANS_GROUP_LOAD(R_GROUP)

         TRANS_GROUP_LOAD = TRANS_GROUP_LOAD_IN_FILE(R_GROUP)
      RETURN

      ENTRY GET_MARKET_PRICE_SCALAR

         GET_MARKET_PRICE_SCALAR = MARKET_PRICE_SCALAR
      RETURN

      ENTRY USE_EXT_LOAD_SOURCE

         USE_EXT_LOAD_SOURCE = &
                             INDEX('YT',USE_EXT_LOAD_SOURCE_ACTIVE) /= 0
      RETURN

      ENTRY PROCMETH

         PROCMETH = PRODUCTION_COST_METHOD
      RETURN

      ENTRY FACET_IS_ACTIVE

         FACET_IS_ACTIVE = FACET_IS_PRODUCTION_METHOD
      RETURN

      ENTRY SET_PRODUCTION_METHOD(R_PRODUCTION_COST_METHOD_CHR)

         PRODUCTION_COST_METHOD_CHR = R_PRODUCTION_COST_METHOD_CHR
         FACET_IS_PRODUCTION_METHOD = &
                              INDEX(PRODUCTION_COST_METHOD_CHR,'F') /= 0
         IF(FACET_IS_PRODUCTION_METHOD) THEN
            PRODUCTION_COST_METHOD = 2
         ELSE
            PRODUCTION_COST_METHOD = &
                             INDEX('1234BDC',PRODUCTION_COST_METHOD_CHR)
            IF(PRODUCTION_COST_METHOD > 4) &
                     PRODUCTION_COST_METHOD = PRODUCTION_COST_METHOD - 4
         ENDIF
         SET_PRODUCTION_METHOD = PRODUCTION_COST_METHOD
      RETURN


      ENTRY ECON_SWITCH

         ECON_SWITCH = (ECON_SWITCH_ACTIVE == 'T' .OR. &
                        ECON_SWITCH_ACTIVE == 'S' .OR. &
                        ECON_SWITCH_ACTIVE == 'O' .OR. &
                        ECON_SWITCH_ACTIVE == 'M')
      RETURN

      ENTRY ECON_PRICING

         ECON_PRICING = ECON_SWITCH_ACTIVE == 'T'
      RETURN

      ENTRY ECON_SALES

         ECON_SALES = (ECON_SWITCH_ACTIVE == 'S' .OR. &
                       ECON_SWITCH_ACTIVE == 'M')
      RETURN

      ENTRY ECON_MARGIN

         ECON_MARGIN = ECON_SWITCH_ACTIVE == 'M'
      RETURN

      ENTRY MARGINAL_COST_SWITCH

         MARGINAL_COST_SWITCH = MARGINAL_COST_SWITCH_ACTIVE == 'T'
      RETURN

      ENTRY OFFSET_MAINTENANCE_VECTORS

         OFFSET_MAINTENANCE_VECTORS = OFFSET_MAINTENANCE_ACTIVE
      RETURN

      ENTRY MODIFY_RESOURCE_TIMING

         MODIFY_RESOURCE_TIMING = RESOURCE_TIMING_SWITCH
      RETURN

      ENTRY DOE_NF_DISPOSAL_COST(R_MMBTU,R_MWH)

         DOE_NF_DISPOSAL_COST = (R_MMBTU * DOE_NF_DISPOSAL_RATES(1) + &
                              R_MWH * DOE_NF_DISPOSAL_RATES(2))/1000000.
      RETURN

      ENTRY NUCLEAR_DECOMMISSIONING_COST(R_MMBTU,R_MWH)

         NUCLEAR_DECOMMISSIONING_COST = &
                         (R_MMBTU * NUC_DECOMMISSIONING_RATES(1) + &
                          R_MWH * NUC_DECOMMISSIONING_RATES(2))/1000000.
      RETURN

      ENTRY DOE_R300_DISPOSAL_COST(R_MMBTU,R_MWH)

         DOE_R300_DISPOSAL_COST = (R_MMBTU * DOE_R300_DISPOSAL_FEE(1) + &
                              R_MWH * DOE_R300_DISPOSAL_FEE(2))/1000000.
      RETURN

      ENTRY RETURN_RESERVE_MARGIN_COSTS(R_RM_PURCHASE_FIXED_COST, &
                                        R_RM_PURCHASE_VARIABLE_COST, &
                                        R_MAX_SELL_RESERVE_MARGIN, &
                                        R_RM_SALE_FIXED_AMOUNT, &
                                        R_RM_SALE_VARIABLE_AMOUNT, &
                                        R_ASSET_CLASS_ID, &
                                        R_ADD_2_INCOME_STATEMENT)

         RETURN_RESERVE_MARGIN_COSTS = LEVELIZING_RESERVE_MARGIN/100.
!
         R_ADD_2_INCOME_STATEMENT = ADD_2_INCOME_STATEMENT
!         IF(ADD_2_INCOME_STATEMENT == 'T') THEN
            R_RM_PURCHASE_FIXED_COST = RM_PURCHASE_FIXED_COST
            R_RM_PURCHASE_VARIABLE_COST = RM_PURCHASE_VARIABLE_COST
            R_MAX_SELL_RESERVE_MARGIN = MAX_SELL_RESERVE_MARGIN/100.
            R_RM_SALE_FIXED_AMOUNT = RM_SALE_FIXED_AMOUNT
            R_RM_SALE_VARIABLE_AMOUNT = RM_SALE_VARIABLE_AMOUNT
            R_ASSET_CLASS_ID = ASSET_CLASS_ID
!         ELSE
!            R_RM_PURCHASE_FIXED_COST = 0.
!            R_RM_PURCHASE_VARIABLE_COST = 0.
!            R_MAX_SELL_RESERVE_MARGIN = 0.
!            R_RM_SALE_FIXED_AMOUNT = 0.
!            R_RM_SALE_VARIABLE_AMOUNT = 0.
!            R_ASSET_CLASS_ID = 0
!         ENDIF
!
      RETURN

      ENTRY RPS_ONLY_SWITCH

         RPS_ONLY_SWITCH = GRX_RPS_MODULE_ACTIVE == 'R'
      RETURN

      ENTRY GRX_RPS_MODULE_IS_ACTIVE

         GRX_RPS_MODULE_IS_ACTIVE = GRX_RPS_MODULE_ACTIVE == 'T'
      RETURN

      ENTRY LMP_IS_ACTIVE

         LMP_IS_ACTIVE = LMP_ACTIVE == 'A'
      RETURN

      ENTRY ACCUMULATE_MAINTENANCE

         ACCUMULATE_MAINTENANCE = ACCUM_MAINTENANCE
      RETURN

      ENTRY YES_RUN_TRANSACT

         YES_RUN_TRANSACT = RUN_TRANSACT
      RETURN

      ENTRY YES_SECOND_FUEL_EMIS_DISPATCH

         YES_SECOND_FUEL_EMIS_DISPATCH = SECOND_FUEL_EMIS_DISPATCH
      RETURN

      ENTRY YES_ONLY_BUY_TRANSACT

         YES_ONLY_BUY_TRANSACT = ONLY_BUY_TRANSACT
      RETURN

      ENTRY YES_ONLY_SELL_TRANSACT

         YES_ONLY_SELL_TRANSACT = ONLY_SELL_TRANSACT
      RETURN

      ENTRY YES_ONLY_DISPATCH_TRANSACT

         YES_ONLY_DISPATCH_TRANSACT = ONLY_DISPATCH_TRANSACT
      RETURN

      ENTRY YES_USE_TRANSACT_LOADS

         YES_USE_TRANSACT_LOADS = USE_TRANSACT_LOADS
      RETURN

      ENTRY YES_STRICT_MARKET_PRICE

         YES_STRICT_MARKET_PRICE = STRICT_MARKET_PRICE
      RETURN

      ENTRY YES_MULTI_AREA_PRICE

         YES_MULTI_AREA_PRICE = MULTI_AREA_PRICE
      RETURN

      ENTRY YES_REGIONAL_AREA_PRICE

         YES_REGIONAL_AREA_PRICE = REGIONAL_AREA_PRICE
      RETURN

      ENTRY YES_CENTRAL_DISPATCH_TRANSACT

         YES_CENTRAL_DISPATCH_TRANSACT = CENTRAL_DISPATCH_TRANSACT
      RETURN

      ENTRY YES_RUN_MULTIAREA_TRANSACT

         YES_RUN_MULTIAREA_TRANSACT = RUN_MULTIAREA_TRANSACT
      RETURN

      ENTRY YES_CALANDER_CORRECT

         YES_CALANDER_CORRECT = CALANDER_CORRECT .OR. &
                                                 TRANSACT_ANALYST_ONLY()
      RETURN

      ENTRY GET_MAX_TRANS_LOADS

         GET_MAX_TRANS_LOADS = MAX_TRANS_LOADS
      RETURN

      ENTRY TRANSFER_TRANSACT_ANL_RESULTS(R_MONTH)


         TRANSFER_TRANSACT_ANL_RESULTS = &
                                   USE_TRANSACT_ANALYST_RESULTS(R_MONTH)
      RETURN
      ! Was TRANSACT_ANALYST_RESULTS_AVAILABLE_STARTING 
      ENTRY TRANSACT_ANLST_RSLTS_AVAIL_STG()

         TRANSACT_ANLST_RSLTS_AVAIL_STG = &
                                                    START_TRANSFER_MONTH
      RETURN

      END

      FUNCTION SAVE_CHR_PP_VALUES(R_FACET_IS_PRODUCTION_METHOD, &
                                  R_TRANS_REPRESENTATION_CHR, &
                                  R_IGNORE_NON_UTILITY_CHR, &
                                  R_REMOVE_NIGHTIME_SCARCITY_CHR, &
                                  R_TRANS_TIME_FRAME_CHR, &
                                  R_CAP_PLANNING_TRIGGER_VALUE, &
                                  R_FIRST_BLK_DISP_SWITCH_ACTIVE)

!
      CHARACTER(len=1) :: SAVE_CHR_PP_VALUES
      LOGICAL(kind=1) :: FACET_IS_PRODUCTION_METHOD, &
                R_FACET_IS_PRODUCTION_METHOD
      CHARACTER(len=80) :: PRODUCTION_METHOD_TITLE
      CHARACTER(len=1) :: R_TRANS_REPRESENTATION_CHR, &
                  R_IGNORE_NON_UTILITY_CHR, &
                  R_REMOVE_NIGHTIME_SCARCITY_CHR, &
                  R_TRANS_TIME_FRAME_CHR, &
                  R_CAP_PLANNING_TRIGGER_VALUE, &
                  R_FIRST_BLK_DISP_SWITCH_ACTIVE
      CHARACTER(len=1) :: TRANS_REPRESENTATION_CHR, &
                  IGNORE_NON_UTILITY_CHR, &
                  REMOVE_NIGHTIME_SCARCITY_CHR, &
                  TRANS_TIME_FRAME_CHR, &
                  CAPACITY_PLANNING_TRIGGER_VALUE, &
                  FIRST_BLOCK_DISP_SWITCH_ACTIVE
      SAVE FACET_IS_PRODUCTION_METHOD, &
           TRANS_REPRESENTATION_CHR, &
           IGNORE_NON_UTILITY_CHR, &
           REMOVE_NIGHTIME_SCARCITY_CHR, &
           TRANS_TIME_FRAME_CHR, &
           CAPACITY_PLANNING_TRIGGER_VALUE, &
           FIRST_BLOCK_DISP_SWITCH_ACTIVE

      CHARACTER(len=1) :: TRANS_REPRESENTATION,IGNORE_NON_UTILITY, &
                  REMOVE_NIGHTIME_SCARCITY,TRANS_TIME_FRAME, &
                  PLANNING_TRIGGER,FIRST_BLOCK_DISP_SWITCH
      INTEGER(kind=2) :: PROCMETH
!
        FACET_IS_PRODUCTION_METHOD = R_FACET_IS_PRODUCTION_METHOD
        TRANS_REPRESENTATION_CHR = R_TRANS_REPRESENTATION_CHR
        IGNORE_NON_UTILITY_CHR = R_IGNORE_NON_UTILITY_CHR
        REMOVE_NIGHTIME_SCARCITY_CHR = R_REMOVE_NIGHTIME_SCARCITY_CHR
        TRANS_TIME_FRAME_CHR = R_TRANS_TIME_FRAME_CHR
        CAPACITY_PLANNING_TRIGGER_VALUE = R_CAP_PLANNING_TRIGGER_VALUE
        FIRST_BLOCK_DISP_SWITCH_ACTIVE = R_FIRST_BLK_DISP_SWITCH_ACTIVE
        SAVE_CHR_PP_VALUES = 'X'
      RETURN

      ENTRY TRANS_REPRESENTATION

         TRANS_REPRESENTATION = TRANS_REPRESENTATION_CHR
      RETURN

      ENTRY IGNORE_NON_UTILITY

         IGNORE_NON_UTILITY = IGNORE_NON_UTILITY_CHR
      RETURN

      ENTRY REMOVE_NIGHTIME_SCARCITY

         REMOVE_NIGHTIME_SCARCITY = REMOVE_NIGHTIME_SCARCITY_CHR
      RETURN

      ENTRY TRANS_TIME_FRAME

         TRANS_TIME_FRAME = TRANS_TIME_FRAME_CHR
      RETURN

      ENTRY PRODUCTION_METHOD_TITLE

         IF(FACET_IS_PRODUCTION_METHOD) THEN
            PRODUCTION_METHOD_TITLE = &
                             'Facet Optimization Production Cost Method'
         ELSEIF(PROCMETH() == 1) THEN
            PRODUCTION_METHOD_TITLE = &
                               'Modified Booth Production Cost Method'
         ELSEIF(PROCMETH() == 2) THEN
            PRODUCTION_METHOD_TITLE = 'Derating Production Cost Method'
         ELSEIF(PROCMETH() == 3) THEN
            PRODUCTION_METHOD_TITLE = &
                    'Modified Booth and Derating Production Cost Method'
         ENDIF
      RETURN

      ENTRY PLANNING_TRIGGER

         PLANNING_TRIGGER = CAPACITY_PLANNING_TRIGGER_VALUE
      RETURN

      ENTRY FIRST_BLOCK_DISP_SWITCH

         FIRST_BLOCK_DISP_SWITCH = FIRST_BLOCK_DISP_SWITCH_ACTIVE
      RETURN
      END
