
      RECURSIVE SUBROUTINE REGIONAL_TRANSACTION_ANALYSIS(ISEAS, &
                                                  R_TRANSACTION_DEMAND)

      use abb_capmarketrptdata
      use end_routine
      use endpoint
      use enrglimit
      use filemod
      use globecom
      use grx_planning_routines
      use irec_endpoint_control
      use kepcocom
      use miscmod
      use month_extra
      use msgoutpt_data
      use procsave_common
      use prod_arrays_dimensions
      use rptreccontrol
      use sizecom
      use spindriftlib
      use spindriftlib 
      use string
      use tf_decs
      use tf_objt_interfaces
      use transobj_hourly
      use program_state
      use debugtrace

      
      implicit none
      SAVE
      integer :: file_trace_aum=0 
      integer :: simulate_multi_party_count=0
      integer (kind=2), parameter :: trans_tie_unit=1927
      integer (kind=2), parameter :: trans_las_unit=1936
      logical (kind=1) :: yrt_result
      logical (kind=1) :: GET_CREATE_HOURLY_PRICE ! External
      INTEGER(kind=1) :: F7=7 ! 7H == z"f7"
      logical :: utf
      integer :: statvar
      integer  :: timeshere=0
      
      integer (kind=2), allocatable :: TG_USING_PRICE_DISTN(:)
      INTEGER(kind=2) :: MAX_TRANS_GROUPS,GET_MAX_TRANS_GROUPS, &
                 DAY_TYPE,GET_DAY_TYPE, &
                 LOCAL_MONTH=0, &
                 LOCAL_HOURS=0, &
                 LOCAL_DAY=0, &
                 LOCAL_DAY_WEEK=0, &
                 LOCAL_HR=0, &
                 R_HOUR, &
                 USER_MONTH=0, &
                 MONTH_COUNT, &
                 ANNUAL_USER_MONTH=13, &
                 USER_HOURS=0, &
                 USER_HR=0, &
                 USER_CUM_HOURS=0, &
                 MARKET_HR, &
                 LOCAL_CUM_HOURS=0, &
                 ANNUAL_STARTING_HOUR=0, &
                 USER_ANNUAL_STARTING_HOUR=0, &
                 ANNUAL_TOTAL_HOURS=0, &
                 MONTH_STARTING_HOUR=0
      INTEGER(kind=2) :: &
                 USER_MONTH_STARTING_HOUR=0, &
                 MONTH_TOTAL_HOURS=0, &
                 R_DAY,R_DAY_WEEK,loc_year=0, &
                 USER_YEAR=0, &
                 PUT_SEASON_FOR_CONSTRAINTS,I2_TEMP, &
                 DAILY_OPTION_ANNUAL_VALUES, &
                 FIRST_MONTHLY_TRANSACT=0, &
                 FIRST_REPORTING_GROUP=0, &
                 FIRST_TRANSACT_REPORTING_GROUP, &
                 FIRST_LAST_MONTH_OF_TRANSACT,R_LAST_MONTH, &
                 LAST_MONTHLY_TRANSACT,MARKET_TG=0, &
                 LOCAL_UNIT_NO, &
                 USER_I=1, &
                 C_M=0,L_M,E_M=1,TRANSACTION_MODE, &
                 HOUR_IS_5X8=4,HOUR_IS_WKX16=9, &
                 HOUR_IS_5X16=1, &
                 HOUR_IS_6X16=2, &
                 HOUR_IS_SAX16=7, &
                 HOUR_IS_SUX16=8
      INTEGER(kind=2) :: &
                 HOUR_IS_ON_PEAK=1, &
                 R_TRANS_GROUP, &
                 R_ASSET_CLASS, &
                 GET_ASSET_CLASS_2_TG, &
                 TOTAL_ZERO_MW_TRANS_PER_MONTH, &
                 D_OR_C, &
                 PEAK_HOUR, &
                 MIN_ITER_B4_LONG_PATH=1000, &
                 L_P, &
                 LOCAL_TRANS_GROUP, &
                 K, &
                 FISCAL_YEAR, &
                 GET_MAX_IN_PATHS,MAX_IN_PATHS, &
                 GET_PRB_SEQUENCE_NUMBER, &
                 STEP_4_ADDER, &
                 R_TRANS
       INTEGER(kind=4) :: TOTAL_TRANS_PER_MONTH,LOCAL_TIE_FLOW(24), &
                TIE_FLOW_DATA_MAX(12),TIE_FLOW_DATA_MIN(12), &
                TIE_FLOW_DATA_SUM(12)
!
! INTER-TIE INFORMATION
       REAL(kind=4) :: &
              MAX_USER_W_PRICE(:), &
              SYSTEM_STORAGE(:,:), &
              MIN_USER_W_PRICE(:), &
              MONTH_MIN_USER_W_PRICE(:,:), &
              MONTH_MAX_USER_PRICE(12)
              
       real (kind=4) :: &
              MONTH_MAX_USER_W_PRICE(:,:), &
                 TEMP_NATIVE_COST, &
              DELIVERED_TIE_COST, &
              PATH_WHEELING_CHARGE, &
              PATH_SPREAD_RATE, &
              TIE_SPREAD, &
              MONTHLY_INCREMENTAL_COST, &
              TOTAL_LOAD_BEFORE_HYDRO, &
              TOTAL_HYDRO_GENERATION, &
              TOTAL_HYDRO_CAPACITY, &
              TOTAL_HYDRO_ROR, &
              TOTAL_EFFECTIVE_CAPACITY, &
              TOTAL_NATIVE_COST, &
              TOTAL_SPINNING_MWH, &
              TOTAL_NON_COIN_PEAK, &
              TOTAL_NON_COIN_BASE, &
              AVE_SALE,PUT_MARKET_REV_AND_EXP,PUT_BUYERS_LOAD, &
              BUYERS_UNSERVED_ENERGY, &
              HOURLY_TIE_LIMIT, &
              TRANSACTION_TOLERANCE=.1, &
              MAX_MARKET_PRICE=0
              
       REAL(kind=4) :: &
              MAX_USER_PRICE=0, &
              DAILY_PRODUCTS_CAPACITY(:,:), &
              SCARCITY_MULT(:), &
              MIN_MARKET_PRICE=0, &
              MIN_USER_PRICE=0, &
              MONTH_MAX_MARKET_PRICE(12), &
              MONTH_MIN_MARKET_PRICE(12), &
              MONTH_MIN_USER_PRICE(12), &
              R_WABASH_TRANS_BUY_ENERGY,R_WABASH_TRANS_BUY_RATE, &
              R_WABASH_TRANS_SELL_ENERGY,R_WABASH_TRANS_SELL_RATE, &
              TEMP_TL_MWH, &
              TEMP_TL_HYDRO_MWH, &
              TEMP_TL_HYDRO_MW, &
              TEMP_TL_HYDRO_ROR, &
              TEMP_SPINNING_MWH
              
              
       real (kind=4) :: GET_LAST_TRANS_PRICE, &
              LAST_TRANS_PRICE, &
              PATH_CAPACITY, TEMP_PRICE
       


       REAL(kind=4) :: &
              MONTHLY_EFFECTIVE_CAPACITY, &
              SAVE_DAYS_PER_TRANS_PERIOD=0, &
              PERCENT_OF_COST_2_ALLOCATE, &
              PRICE_AFTER_WHEEL, &
              TRANSFER_LOAD_B4(0:2), &
              TRANSACT_UNSERVED_COST=0.,GET_TRANSACT_UNSERVED_COST, &
              TOTAL_UNSERVED,TOTAL_UNSERVED_COST, &
              TOTAL_ABOVE_RESOURCES, &
              TOTAL_COST_ABOVE_RESOURCES, &
              R_ANNUAL_UNSERVED_ENERGY, &
             TRANSACT_ABUNDANCE_COST=0, &
              HOURLY_DUMP_USED, &
              SCENARIO_ELECTRIC_MULT
              
       REAL(kind=4) :: &
              SELLER_ACTIVE, &
              BUYER_ACTIVE, &
              CONVERGENCE_PATH, &
              R_DAILY_PAYOFF,R_STRIKE_PRICE, &
              RESERVE_CAPACITY(3)=(/0.,0.,0./), &
              MONTHLY_CONTINGENT_CAP, &
              BEFORE_RETAINED, &
              SUPPLEMENTAL_CAPACITY(3)=0
       real (kind=4) :: RESERVE_CAPACITY_USED, &
              SUPPLEMENTAL_CAPACITY_USED, &
              MONTHLY_RESERVE_ENERGY(3)=(/0.,0.,0./), &
              MONTHLY_SUPPLEMENTAL_ENERGY(3)=(/0.,0.,0./), &
              RES_N_SUP_CAPACITY_USED, &
              WRITE_DAY_OF_WEEK(31)
       REAL(kind=4) :: &
              EQUIVALENT_RETAINED, &
              HOURLY_OPTION_CAPACITY, &
              HOURLY_FORWARD_CONTRACT_ENERGY, &
              HOURLY_DERIVATIVE_ENERGY, &
              DAILY_PRICE(24), &
              TEMP_DAILY_PRICE(24)

              
       real (kind=4) :: RETAINED_AVAILABLE(3), &
              HOURLY_COMMITMENT_GENERATION, &
              HOURLY_COMMITMENT_SPIN, &
              HOURLY_COMMITMENT_UNSERVED, &
              HOURLY_COMMITMENT_LOAD, &
              HOURLY_COMMITMENT_MC, &
              HOURLY_COMMITMENT_MC_AT_LOAD, &
              HOURLY_NATIVE_COST
              
       real (kind=4) :: &
              R_ANNUAL_UNSERVED_COST, &
              HOURLY_UNSERVED_ENERGY, &
              HOURLY_TRANSFER_MWH(:,:), &
              HOURLY_FORWARDS_4_MONTH(:,:), &
              SYSTEM_DERIVATIVES(:,:)

       real (kind=4) :: TEMP_TRANSFER_MWH
              
       ! Externals
       REAL(kind=4) :: &
              GET_DAILY_PEAK_SPIN, &
              GET_TRANSACT_ABUNDANCE_COST, &           
              GET_SCENARIO_ELECTRIC_PRICE, &
              GET_HOURLY_SCEN_ELECT_MULT, &
              GET_HRLY_TG_SCEN_ELECT_MULT, &
              GET_MONTHLY_CONTINGENT_CAP, &
              GET_CPL_RESERVE_CAPACITY, &
              GET_DUKE_BEFORE_RETAINED, &
              GET_DUKE_SUP_CAPACITY, &
              GET_CPL_SUP_CAPACITY, &
              GET_WEEKEND_SCARCITY_MULT, &
              GET_NIGHT_SCARCITY_MULT, &
              GET_DUKE_RETAINED_CAPACITY, &
              CALC_MONTHLY_DERIVATIVE_FIXED, &
              R_DAYS_PER_TRANS_PERIOD, &
              HOUR_TIE_LIMIT, &
              HOURLY_DERIVATIVE_LOAD, &
              ESCALATED_MONTHLY_VALUE, &
              R1_MONTHLY_TRANSFER_REV, &
              R1_MONTHLY_TRANSFER_COST, &
              R2_MONTHLY_TRANSFER_REV, &
              GET_GRX_TRANS_HOURLY_STORAGE, &
              HOURLY_INTERRUPTIBLE_CAPACITY, &              
              R2_MONTHLY_TRANSFER_COST


              
              
      real (kind=4) ::  &
           TRANSFER_MULT, &
              TEMP_TRANSFER_SELLER_MWH, &
              TEMP_TRANSFER_PRICE, &
              HOURLY_TRANSFER_PRICE(24), &
              HOURLY_TRANSFER_SYS_PRICE(24), &
              MONTHLY_TRANSFER_MWH(2,0:12), &
              MONTHLY_TRANSFER_REVENUE(2,0:12), &
              TEMP_R1,TEMP_R2


       REAL(kind=4) :: &
              AVAIL_DERIVATIVE_CAPACITY(0:800), &
              SYSTEM_AVAIL_STORAGE(0:800), &
              SYSTEM_AVAIL_DERIVATIVES(0:800), &
              EV_DATA_SOURCE=2., &
              HOURLY_GRX_STORAGE
              
        real (kind=8) :: r8TEMP, & ! previously named R4TEMP
              r8LoadB4, & ! previously named LOAD_B4
              r8_X_TEMP,r8_Y_TEMP
              
              


           
      REAL(kind=8) :: MAX_LOCAL_IMPORT, &
              MAX_LOCAL_EXPORT, &
              DENOM,DENOM_1,DENOM_2, &
              QUANT_AT_MAX_TRANS, &
              QUANT_AT_MIN_TRANS, &
              PREVIOUS_QUANT_AT_MIN_TRANS, &
              SELLER_PRICE_AT_MAX_TRANS, &
              SELLER_PRICE_AT_MIN_TRANS, &
              BUYER_PRICE_AT_MAX_TRANS, & 
              BUYER_PRICE_AT_MIN_TRANS, &
              SELLER_PRICE_AT_TEST_TRANS, &
              BUYER_PRICE_AT_TEST_TRANS, &
              LOAD_FOR_MC, &
              GET_MUST_RUN_CAPACITY, &
              HOURLY_FORWARD_SALE, &
              GET_HOUR_PATH_MW
      


! LOAD INFORMATION TRANSACTIONS
      real (kind=8), allocatable :: SUM_USER_W_PRICES(:)
      real (kind=8) :: R_PUR_ENRG_FROM_TRANSACT
      real (kind=8) :: MONTH_SUM_USER_W_PRICES(:,:), &
          SYSTEM_HOURLY_LOADS(:,:) !HOUR,SYSTEM

      


      REAL(kind=8) :: PERIOD_TRANSACTION_DEMAND=0.D0
      REAL(kind=8) :: &
              R_TRANSACTION_DEMAND, &
              SUM_MARKET_PRICES, &
              SUM_USER_PRICES, &
              MONTH_SUM_MARKET_PRICES(12), &
              MONTH_SUM_USER_PRICES(12)
              
      ALLOCATABLE :: &
              MAX_USER_W_PRICE, &
              MIN_USER_W_PRICE, &
              MONTH_SUM_USER_W_PRICES, &
              MONTH_MAX_USER_W_PRICE, &
              MONTH_MIN_USER_W_PRICE, &
              SCARCITY_MULT, &
              SYSTEM_STORAGE, & 
              SYSTEM_DERIVATIVES
! ALLOCABLE SECTION
      ALLOCATABLE :: SYSTEM_HOURLY_LOADS ! HOUR,SYSTEM
! SYSTEM INFORMATION
      
      
      LOGICAL(kind=1) :: VOID_LOGICAL, CX_DailyStorePattern, &
          DC_LOAD_FLOW=.FALSE., &
          PRICE_ONLY_WHOLESALE_REV=.TRUE., &
          FAILED_DIAGNOSTICS, &
          SELLERS_PRICE_DIDNT_CHANGE, &
          BUYERS_PRICE_DIDNT_CHANGE, &
          CREATE_HOURLY_PRICE
          
      
      
      logical (kind=1) :: INIT_HOURLY_TIE ! External

      ! External
      logical (kind=1) :: CX_DailyStorePat2, CX_DailyStorePat3
      logical (kind=1) :: ALLOCATE_PRICE_ARRAYS=.true.
      logical (kind=1) :: TEST_DAILY_OPTION=.FALSE.
          
          
      ! Externals           
      logical (kind=1) ::         HOURLY_DIAGNOSTICS, &
               INIT_HOUR_PATH_LIMIT,INIT_HOUR_CONSTRAINT_LIMIT, &
               REDUCE_PATH_CAPACITY, &
               RECORD_TRANSACTION_TO_TIE,UPDATE_MONTHLY_TIE_COST, &
               HOUR_PATH_ACTIVE, &
               PUT_AC_HOURLY_COST_AT_MARKET, &
               YES_RUN_TRANSACT, &
               YES_PUT_AC_HOURLY_COST, &
               GET_TRANS_SPINNING_CAPACITY, &
               GET_TG_PRICE_MULT, &
               GET_TRANS_PRICE_CAPS, &
               GET_TRANS_PRICE_MINIMUM, &
               GET_OFF_PEAK_SPINNING_CAPACITY, &
               GET_TRANS_RAMP_RATES, &
               GET_TRANS_MAX_IMPORT_EXPORT, &
               YES_DISALLOW_REDUNDANT_TRANS
       
      logical (kind=1) :: TEMP_L, &
          USE_MARKET_PRICING=.FALSE., &
          BURESH/.FALSE./
          
          logical (kind=1) :: IGNORE_NIGHTIME_CAPVAL/.FALSE./, &
               RECORD_EP_2_AC
      logical (kind=1) ::  DUTCH_AUCTION_LOGIC=.FALSE.
      logical (kind=1) :: DUTCH_AUCTION_LOGIC_2=.TRUE., &
               MODIFIED_DUTCH_AUCTION/.FALSE./, &
               NEW_LONG_PATH/.FALSE./, &
               HOURLY_DERIVATIVES_NOT_OPEN=.TRUE., &
               HOURLY_AFTER_HYDRO_NOT_OPEN=.TRUE.
      logical (kind=1) :: YES_MARGINAL_UNIT_REPORT=.FALSE.
      logical (kind=1) :: USE_PRICE_CAPS=.FALSE., &
        GLOBAL_SCARCITY_BUY_N_SELL/.FALSE./, &
        HOURLY_CAPACITY_NOT_OPEN/.TRUE./
      logical (kind=2) :: NEW_TRANS_LOGIC=.FALSE.
        
      
      

REAL(kind=4) :: PRODUCT_PRICE(:,:), &
                 PRODUCT_VOLATILITY(:,:), &
                 PRODUCT_QUANTITY(:,:), &
                 PRODUCT_HEATRATE(:,:), &
                 PRODUCT_MARGINAL_FUEL(:,:), &
                 PRODUCT_FUEL_PRICE(:,:), &
                 PRODUCT_HOURS(:), &
                & ! NUM_PRODUCTS X HOURS IN THE MONTH
                 PRODUCT_DAILY_RETURN(:,:,:), &
                 PRODUCT_MEAN_RETURN(:,:), &
                 PRODUCT_SCARCITY(:,:), &
                 PRODUCT_LAST_PRICE(:,:), &
                 SUM_SQUARED_DEVIATIONS(:,:), &
                 DEVIATIONS_FROM_MEAN
      
      LOGICAL(kind=1) :: &
               DISALLOW_REDUNDANT_TRANS, &
               R_CREATE_HOURLY_PRICE, &
               YES_UNIT_COMMITMENT_LOGIC, &
               DAILY_OPTION_FOR_MONTH, &
               DUKE_OR_CPL,ECITIES, &
               YES_ECITIES_UNITS_ACTIVE, &
               YES_MONTH_OUTAGE_EVENTS,OUTAGE_EVENTS_IN_MONTH, &
               MARKET_DURATION_REPORT,YES_MARKET_DURATION=.FALSE., &
               DAILY_CALL_PUT_CAPACITY,YES_DAILY_CALL_PUT_CAPACITY, &
               DETAILED_TRANSFER_PRICING=.FALSE., &
               YES_DETAILED_TRANSFER_PRICING, &
               HOURLY_PRICE_RECONCILE=.FALSE.
      LOGICAL(kind=1) :: &
               YES_ADVANCED_PRICE_MODELING, &
               PRICE_LAST_MW, &
               YES_PRICE_LAST_MW, &
               YES_PRICE_LAST_MW_AS_UNSERVED, &
               PRICE_LAST_MW_AS_UNSERVED, &
               WRITE_MONTHLY_CLASS_SUMMARY, &
               MIN_MAX_THERMAL, &
               DailyOperMoPumpedStorage, &
               MONTHLY_CALL_PUT_CAPACITY, &
               PROCESS_MARKET_RESOURCES, &
               ANN_HOURLY_SCEN_ELECT_MULT, &
               ANN_HRLY_TG_SCEN_ELECT_MULT, &
               ANN_HOURLY_SCEN_GAS_MULT, &
               ANN_HOURLY_SCEN_OIL_MULT, &
               SET_HOUR_LONG_PATH_WI_PATHS, &
               SET_HOUR_LONG_CAP_TWH
      LOGICAL(kind=1) :: &
               USE_5X16_LOGIC_CONSTRAINTS, &
               YES_USE_5X16_LOGIC_CONSTRAINTS, &
               MULTI_AREA_TRANC_ACTIVE, &
               MULTI_AREA_TRANC, &
               MULTI_MARKET_ACTIVE, &
               YES_MULTI_MARKET_ACTIVE, &
               LAMBDA_PRICE,YES_LAMBDA_PRICE, &
               YES_TWO_PRB_DIGITS, &
               STEP_4, &
               TRANSACT_C
!
      LOGICAL(kind=1) :: READ_TRANSACT_TIE_DATA, &
               INIT_ANN_ALLOC_BLOCKS_2_CUST, &
               WRITE_TRANC_MONTHLY_SUMMARY, &
               YES_MONTHLY_TRANC_REPORT, &
               TRANC_JDA_LOGIC, &
               TRANSACT_C_MONTHLY_INIT, &
               READ_TRANS_PATH_DATA, &
               UPDATE_TRANS_PATH_DATA, &
               READ_TRANS_CONSTRAINT_DATA, &
               TRANSACTION_COMPLETED, &
               SEARCH_SUCCESSFUL, &
               ANNUAL_MARKET_PRICING, &
               UPDATE_OUTAGE_DATA, &
               MODIFIED_BIDDING_LOGIC

      LOGICAL(kind=1) :: &
               YES_BIDDING_LOGIC, &
               APPLY_SECOND_BEST_TRAN, &
               APPLY_COMMITMENT_IN_MONTH, &
               SAVE_DEPTH_OF_MARKET_DATA, &
               SAVE_TRANS_DEPTH_DATA, &
               WRITE_DEPTH_OF_MARKET_REPORT, &
               INIT_DEPTH_PRICE, &
               WEEKEND_DAY, &
               NIGHT, &
               MARGINAL_UNIT_REPORT, &
               YES_HOOSIER,HOOSIER, &
               YES_PRICES_ARE_CAPPED, &
               YES_GLOBAL_SCARCITY_BUY_N_SELL, &
               HOURLY_CAPACITY_REPORT, &
               HOURLY_CAPACITY_COST_REPORT, &
               HOURLY_DERIVATIVES_REPORT, &
               HOURLY_AFTER_HYDRO_REPORT, &
               HOURLY_AFTER_TRANS_REPORT
      LOGICAL(kind=1) :: &
               HOURLY_CAPACITY_COST_NOT_OPEN=.TRUE., &
               YES_HOURLY_CAPACITY_REPORT, & 
               YES_HOURLY_CAPACITY_COST_REPORT, &
               YES_HOURLY_DERIVATIVES_REPORT, &
               YES_HOURLY_AFTER_HYDRO_REPORT, &
               YES_HOURLY_AFTER_TRANS_REPORT, &
               YES_USE_PRICE_MINIMUMS, &
               USE_PRICE_MINIMUMS, &
               HOURLY_TRANSACTION_REPORT, &
               YES_HOURLY_TRANSACTION_REPORT, &
               LONG_PATH_LOGIC, &
               HOURLY_PATHS_REPORTS, &
               DAILY_PATHS_REPORTS
      LOGICAL(kind=1) :: &
               WRITE_DAILY_THERMAL_UNIT, &
               LAST_HOUR_TRANSACTIONS, &
               YES_REMEMBER_LAST_HOUR, &
               SET_HOUR_LONG_PATH_PARAMS
      LOGICAL(kind=4) :: &
               PRICE_FILE_OPEN,PRICE_FILE_EXISTS
      REAL(kind=8) :: & ! REAL before 20030429
           GET_MARGINAL_COST_AT_MARKET, &
           GET_PRODUCTION_LEVEL_AT_MARKET
      REAL :: GET_AVERAGE_COST, &
           GET_CLOSEST_MARGINAL_COST, &
           WRITE_CONTRIBUTION_REPORT, &
           HOURLY_MARGINAL_HEATRATE, &
           HOURLY_MARGINAL_FUEL_PRICE, &
           HOURLY_MARGINAL_FT
      
!
      INTEGER(kind=2) :: NUM_PRODUCTS,TEMP_PRODUCT_HOURS
      PARAMETER ( NUM_PRODUCTS=15) !TMS 20041129 NUM_PRODUCTS increased
                                   ! by 2 to 15
      
      REAL(kind=4) :: &
                 SCARCITY_COST(:,:), &
                 ENERGY_COST(:,:), &
                 ANNUAL_PRODUCT_PRICE(:,:), &
                 ANNUAL_PRODUCT_SCARCITY(:,:), &
                 ANNUAL_PRODUCT_QUANTITY(:,:), &
                 ANNUAL_PRODUCT_HEATRATE(:,:), &
                 ANNUAL_PRODUCT_MARGINAL_FUEL(:,:), &
                 ANNUAL_PRODUCT_FUEL_PRICE(:,:), &
                 ANNUAL_PRODUCT_HOURS(:), &
                 R_ANNUAL_PRICES(8760)
      LOGICAL(kind=1) :: PRODUCT_FIRST_PERIOD(NUM_PRODUCTS)
      ALLOCATABLE :: &
                    PRODUCT_PRICE,PRODUCT_VOLATILITY, &
                    PRODUCT_QUANTITY,PRODUCT_HEATRATE, &
                    PRODUCT_MARGINAL_FUEL,PRODUCT_FUEL_PRICE, &
                    PRODUCT_HOURS, &
                    PRODUCT_DAILY_RETURN, &
                    PRODUCT_MEAN_RETURN, &
                    PRODUCT_SCARCITY, &
                    PRODUCT_LAST_PRICE, &
                    SUM_SQUARED_DEVIATIONS, &
                    SCARCITY_COST, &
                    ENERGY_COST, &
                    ANNUAL_PRODUCT_PRICE, &
                    ANNUAL_PRODUCT_SCARCITY, &
                    ANNUAL_PRODUCT_QUANTITY, &
                    ANNUAL_PRODUCT_HEATRATE, &
                    ANNUAL_PRODUCT_MARGINAL_FUEL, &
                    ANNUAL_PRODUCT_FUEL_PRICE, &
                    ANNUAL_PRODUCT_HOURS 
!
      CHARACTER(len=1) :: TRANSACTION_PERIOD,TRANS_TIME_FRAME, &
                 TRANS_REPRESENTATION,REMOVE_NIGHTIME_SCARCITY
!
      CHARACTER(len=20) :: LOCAL_PRODUCT_TYPE(NUM_PRODUCTS)= &
        (/'5x16                ', &
          '6x16                ', &
          '7x24                ', &
          '5x8                 ', &
          'Wrap                ', &
          '2x24                ', &
          'Sax16               ', &
          'Sux16               ', &
          'Wkx16               ', &
          'Western Wrap        ', &
          '6x8                 ', &
          '7x8                 ', &
          'Super Peak          ', &
          '7x16                ', & ! TMS added 20041129 for TVA
          'Wkx8                '/)   ! TMS added 20041129 for TVA
!
! LOCAL VARIABLES
      REAL LOAD_INCREMENT
      INTEGER(kind=2) :: I,J,L,M,VALID_FILE_MARK,ISEAS,HR, &
               TRANS_GROUPS_RECORDS,J_F,K_F,L_F,I_HR,J_HR
      
      INTEGER(kind=2) :: HOURS_IN_MONTH,CUM_HOURS,R_HOURS_IN_MONTH, &
                 TEMP_HOURS,HOURS_FOR_TRANSACTIONS, &
                 DAILY_HOURS=24,HOUR_IN_DAY,DATA_BASE, &
                 DATA_BASE_2, &
                 GET_DATA_BASE_FOR_TRANS,GET_MAX_TRANS_ID_USED, &
                 LOWER_TRANS_GROUP,UPPER_TRANS_GROUP=0, &
                 MAX_TRANS_LOADS, &
                 R_MAX_TRANS_LOADS, &
                 GET_MAX_TRANS_LOADS,TOTAL_BUYER_INDEX, &
                 TG,TGJ, &
                 GET_TRANS_GROUP_INDEX, &
                 MAX_TRANS_GROUP_NUMBER=0,GET_MAX_TRANS_GROUP_NUMBER, &
                 ANNUAL_HOURS=8800
      CHARACTER(len=256) :: LOAD_FILE_NAME,MARGINAL_COST_FILE_NAME
      CHARACTER(len=256) :: MARKET_RATES_DIR,MIDAS_OUTPUT_DIR, &
                    MIDAS_INPUT_DIR,TRANSACT_OUTPUT_DIR
      CHARACTER(len=5) :: LOAD_INPUT_NAME,MARGINAL_COST_INPUT_NAME, &
                 MARKET_PRICE_NAME
      CHARACTER(len=35) :: AREA_NAME_35
      CHARACTER(len=3) :: GET_HOURLY_PRICE_NAME,TEMP_STR
      LOGICAL(kind=1) :: ZERO_SELL,ZERO_BUY, &
                 YES_ONLY_SELL_TRANSACT, &
                 YES_ONLY_BUY_TRANSACT, &
                 YES_ONLY_DISPATCH_TRANSACT, &
                 TRANS_GROUP_LOAD_ACTIVE(:), &
                 HOURLY_TRANS_GROUP_LOAD_ACTIVE(:), &
                 HOURLY_TRANS_GROUP_GEN_ACTIVE(:), &
                 TRANS_GROUP_GEN_ACTIVE, &
                 APPLY_TRANS_REV_TO_WHOLESALE, &
                 TF_FILE_LOAD_GROUP_ACTIVE, &
                 APPLY_ENERGY_PRODUCT, &
                 NewStructure
      ALLOCATABLE :: TRANS_GROUP_LOAD_ACTIVE, &
                    HOURLY_TRANS_GROUP_LOAD_ACTIVE, &
                    HOURLY_TRANS_GROUP_GEN_ACTIVE
      INTEGER(kind=4) :: VALUES_2_ZERO
      REAL :: HOURLY_MARGINAL_COST(:,:),M_HOURLY_MC_B4_SALES(:,:), &
           HOURLY_MC_AFTER(:,:),HOURLY_LAST_PRICE(:,:), &
           HOURLY_IN_PRICE(:,:), &
           LAST_TRANS_MC(:)
      real, allocatable :: ALL_MONTH_PRICES(:,:)
      REAL :: HOURLY_LAMDA(:,:)
      ALLOCATABLE :: HOURLY_LAMDA
      INTEGER(kind=2) :: SELLER,BUYER,SELLER_DB,BUYER_DB
      ALLOCATABLE :: HOURLY_MARGINAL_COST, &
                    LAST_TRANS_MC, &
                    HOURLY_LAST_PRICE, &
                    DAILY_PRODUCTS_CAPACITY, &
                    HOURLY_IN_PRICE, &
                    M_HOURLY_MC_B4_SALES, &
                    HOURLY_MC_AFTER
      REAL :: SELLERS_COST,BUYERS_COST,TIE_POWER, &
          UP_100MW_BUY, &
          DOWN_100MW_BUY, &
          UP_100MW_SELL, &
          DOWN_100MW_SELL, &
          UP_25MW_BUY, &
          DOWN_25MW_BUY, &
          UP_25MW_SELL, &
          DOWN_25MW_SELL, &
          TEMP_CAP,LOCAL_MARKET_PRICE(24), &
          LOCAL_USER_PRICES, &
          LOCAL_MONTH_USER_PRICES(12), &
          R_LOCAL_MARKET_PRICE(24), &
          R_MONTHLY_ECONOMY_BOUGHT,R_MONTHLY_ECONOMY_COST, &
          R_MONTHLY_ECONOMY_SOLD,R_MONTHLY_ECONOMY_REVENUE, &
          R_ANNUAL_ECONOMY_BOUGHT,R_ANNUAL_ECONOMY_COST, &
          R_ANNUAL_ECONOMY_SOLD,R_ANNUAL_ECONOMY_REVENUE
      REAL :: &
          GET_CAPPED_PRICE, &
          HOURLY_GLOBAL_SCARCITY, &
          TRANSFER_AVE_COST_AFTER, &
          TRANSFER_AVE_COST_BEFORE, &
          TRANSFER_PROCOST_CHANGE
      REAL(kind=4) :: TEST_HOURLY_REVENUE(:,:)
      ALLOCATABLE ::  TEST_HOURLY_REVENUE
      CHARACTER(len=8) :: EEICODE
      INTEGER(kind=2) :: TIMZON,TEMPER,DELTMP,DAY_OF_WEEK(:,:),DAY, &
               CURRENT_HR,DA,DAYS_IN_MONTH,LDE_YEAR, &
               MARKET_DAY_OF_WEEK,CALENDAR_DAY_OF_WEEK, &
               LDE_DAY,LDE_MONTH,DAY_WEEK
      INTEGER(kind=2) :: GET_DAY_OF_WEEK_4,TEMP_I2,TEMP_I
!      
      INTEGER(kind=4) :: DAILY_LOADS_I4(24),TEMP_I4
      INTEGER(kind=2) :: DAILY_LOADS_I2(24)
      INTEGER(kind=4) :: RECORD_LENGHT,FILE_LENGHT
! INT*2 FROM INT*4. 11/26/99. GAT.
      INTEGER(kind=2) :: IREC,ALINE_LOAD_DATA, &
                 USER_IREC=0,DAILY_IREC=0
      LOGICAL(kind=1) :: READ_I4_LOADS,RUN_MULTIAREA, &
               YES_RUN_MULTIAREA_TRANSACT,YES_USE_TRANSACT_LOADS, &
               USE_TRANSACT_LOADS,USE_TF_FILE, &
               YES_USE_TF_FILE_FOR_PRICE, &
               USE_TF_FILE_FOR_PRICE, &
               YES_USE_TF_FILE_FOR_MULTIAREA, &
               USE_TF_FILE_FOR_MULTIAREA, &
               CENTRAL_DISPATCH_TRANSACT, &
               YES_CENTRAL_DISPATCH_TRANSACT, &
               YES_STRICT_MARKET_PRICE,STRICT_MARKET_PRICE, &
               YES_MULTI_AREA_PRICE, MULTI_AREA_PRICE, &
               YES_REGIONAL_AREA_PRICE, REGIONAL_AREA_PRICE, &
               YES_TRANSACT_HOURLY_REPORTS,TRANSACT_HOURLY_REPORTS, &
               YES_TRANSACT_MONTHLY_REPORTS,TRANSACT_MONTHLY_REPORTS, &
               YES_TRANSACT_DAILY_REPORTS,TRANSACT_DAILY_REPORTS, &
               YES_HOURLY_PRICE_REPORT,HOURLY_PRICE_REPORT, &
               YES_STRICTLY_POSITIVE_MARGIN,STRICTLY_POSITIVE_MARGIN
      INTEGER(kind=2) :: R_MONTH, &
               TWH,LAST_TWH, &
               TRANSACTIONS_WITHIN_HOUR(:), &
               BUY_FOR_TRANSACTION(:), &
               LAST_BUY_FOR_TRANSACTION(:), &
               LAST_SELL_FOR_TRANSACTION(:), &
               SELL_FOR_TRANSACTION(:), &
               CUM_REDUNDANT_TRANSACTION(:), &
               LONG_PATH_TRANSACTION(:), &
               LAST_BUYER(:), &
               LAST_SELLER(:), &
               PRICE_ANCHORED(:)
      CHARACTER(len=20) :: MONTH_NAME
      CHARACTER(len=35) :: GET_GROUP_NAME
      CHARACTER(len=35) :: MULTI_AREA_NAME(:)
      INTEGER(kind=2) :: TVA,CINERGY,START_HOUR,RPT_HR, &
                 END_HOUR, &
                 ALLOWED_TRANSACTION_PAIR(:,:), &
                 TRANSACTIONS_PER_HOUR(:,:), &
                 LONG_PATH_TRANSACTION_PAIR(:,:)
      PARAMETER (TVA=2,CINERGY=1)
!
      CHARACTER(len=3) :: TEMP_TWO_DIGIT_NAME
      CHARACTER(len=3) :: R_MULTI_AREA_NAME,TEMP_MULTI_AREA_NAME
      CHARACTER(len=5) :: SAVE_MARKET_PRICE_NAME(256), &
                         SAVE_TIE_FLOW_NAME(256)
      LOGICAL(kind=1) :: REPORT_AREA_ACTIVE(:)
!
      REAL :: TIE_FLOW(:,:), &
          MARGINAL_COST_DELTA(:,:), &
          MAX_TIE_FLOW, &
          MIN_TIE_FLOW, &
          HOURLY_LOAD_B4_SALES(:,:), &
          DAILY_MARKET_PRICE(:), &
          HOURLY_PRO_COST_AFTER_SALES(:,:), &
          HOURLY_CAPACITY(:,:), &
          HOURLY_EUE(:,:), &
          HOURLY_DERIVATIVES(:,:), &
          M_HOURLY_PRO_COST_AFTER_SALES(:,:), &
          HOURLY_PRO_COST_B4_SALES(:,:), &
          M_HOURLY_PRO_COST_B4_SALES(:,:), &
          HOURLY_INCREMENTAL_COST(:,:), &
          M_HOURLY_INCREMENTAL_COST(:,:), &
          HOURLY_SELL_MWH(:,:), &
          MONTHLY_TRANSACTION_MWH(:,:), &
          ANNUAL_TRANSACTION_MWH(:,:), &
          TOTAL_DELIVERED_COST(:,:), &
          SELLERS_LOCAL_CAPACITY(:,:)
      REAL :: &
          HOURLY_LOADS(:,:), &
          TEST_HOUR_TIE_LIMIT(:,:), &
          M_MONTHLY_PRO_COST_AFTER_SALES(:), &
          M_MONTHLY_PRO_COST_B4_SALES(:), &
          M_MONTHLY_LOAD_B4_SALES(:), &
          M_MONTHLY_LOAD_AFTER_SALES(:), &
          M_PURCHASE_ENERGY(:), &
          M_PURCHASE_COSTS(:), &
          MARKET_COST_ABOVE_RESOURCES(:), &
          M_SALES_ENERGY(:), &
          M_SALES_REVENUES(:), &
          M_NATIVE_COST(:), &
          M_ANNUAL_PURCHASE_ENERGY(:), &
          M_ANNUAL_PURCHASE_COSTS(:), &
          M_ANNUAL_SALES_ENERGY(:), &
          M_ANNUAL_SALES_REVENUES(:), &
          M_ANNUAL_NATIVE_COST(:), &
          M_ANNUAL_LOAD_B4_SALES(:), &
          M_ANNUAL_PRO_COST_B4_SALES(:)
      REAL :: &
          M_ANNUAL_LOAD_AFTER_SALES(:), &
          M_ANNUAL_PRO_COST_AFTER_SALES(:), &
          ANNUAL_TL_MWH(:), &
          ANNUAL_TL_PEAK(:), &
          ANNUAL_COIN_PEAK(:), &
          ANNUAL_TL_BASE(:), &
          ANNUAL_TL_HYDRO_MWH(:), &
          ANNUAL_TL_HYDRO_MW(:), &
          ANNUAL_TL_HYDRO_ROR(:), &
          ANNUAL_SPINNING_MWH(:), &
          ANNUAL_EFFECTIVE_CAPACITY(:), &
          ANNUAL_UNSERVED_ENERGY(:,:), &
          ANNUAL_ABOVE_RESOURCES(:,:), &
          ANNUAL_UNSERVED_ENERGY_COST(:), &
          ANNUAL_COST_ABOVE_RESOURCES(:,:), &
          FISCAL_PURCHASE_ENERGY(:), &
          FISCAL_PURCHASE_COSTS(:), &
          FISCAL_SALES_ENERGY(:), &
          FISCAL_SALES_REVENUES(:)
      REAL :: &
          FISCAL_NATIVE_COST(:), &
          FISCAL_LOAD_B4_SALES(:), &
          FISCAL_PRO_COST_B4_SALES(:), &
          FISCAL_LOAD_AFTER_SALES(:), &
          FISCAL_PRO_COST_AFTER_SALES(:), &
          FISCAL_TL_MWH(:), &
          FISCAL_TL_PEAK(:), &
          FISCAL_COIN_PEAK(:), &
          FISCAL_TL_BASE(:), &
          FISCAL_TL_HYDRO_MWH(:), &
          FISCAL_TL_HYDRO_MW(:), &
          FISCAL_TL_HYDRO_ROR(:), &
          FISCAL_SPINNING_MWH(:), &
          FISCAL_EFFECTIVE_CAPACITY(:), &
          FISCAL_UNSERVED_ENERGY(:), &
          FISCAL_ABOVE_RESOURCES(:), &
          FISCAL_UNSERVED_ENERGY_COST(:), &
          FISCAL_COST_ABOVE_RESOURCES(:), &
          FISCAL_LOAD_B4_SALES_V
      REAL :: &
          FISCAL_PRO_COST_B4_SALES_V, &
          FISCAL_LOAD_AFTER_SALES_V, &
          FISCAL_PRO_COST_AFTER_SALES_V, &
          MONTH_COIN_PEAK(:), &
          MONTH_NON_COIN_PEAK(:), &
          M_UNSERVED_ENERGY(:), &
          M_ABOVE_RESOURCES(:), &
          CAPPED_PRICE(:), &
          PRICE_MINIMUM(:), &
          M_UNSERVED_ENERGY_COST(:), &
          NET_MARGIN(:), &
          BEST_PRICE_TO_BUYER(:,:), &
          ANNUAL_TIE_FLOW, &
          ANNUAL_MARKET_PRICES(:), & ! IF USED, ITS 8760 VALUES
          USER_MARKET_PRICES(:,:), & ! 4/13/00. GAT.
          ZERO_MARKET_PRICES(:,:), &
          HOURLY_DUMP_CAPACITY(:), &
          HOURLY_DUMP_BEFORE(:), &
          TRANS_MUST_CAPACITY(:)
      real, allocatable ::  trans_ror_cap(:)
      REAL :: &
          TRANS_SPINNING_CAPACITY(:), &
          AREA_PRICE_MULT(:), &
          HOURLY_SPINNING_CAPACITY(:), &
          DAILY_PEAK(:), &
          M_SPINNING_MWH(:), &
          OFF_PEAK_SPINNING_CAPACITY(:), &
          TRANS_MAX_IMPORT(:), &
          TRANS_MAX_EXPORT(:), &
          TRANS_RAMP_UP(:), &
          TRANS_RAMP_DOWN(:), &
          MARGIN_FOR_TRANSACTION(:), &
          MAX_HOURLY_IMPORT(:),  &
          MAX_HOURLY_EXPORT(:), &
          MW_FOR_TRANSACTION(:)
      REAL(kind=8) :: & ! REAL before 20030429
          SELLERS_LOAD,BUYERS_LOAD, &
          MAX_HOURLY_RAMP_UP, &
          MAX_HOURLY_RAMP_DOWN, &
          LAST_HOUR_SELL(:), &
          HOURLY_TRANSACTION(:), &
          LAST_MW_FOR_TRANSACTION(:), &
          M_HOURLY_LOADS(:,:), &
          M_HOURLY_LOAD_B4_SALES(:,:)
      ALLOCATABLE :: TIE_FLOW, &
                    MARGINAL_COST_DELTA, &
                    HOURLY_LOAD_B4_SALES, &
                    HOURLY_TRANSFER_MWH, &
                    HOURLY_FORWARDS_4_MONTH, &
                    DAILY_MARKET_PRICE, &
                    HOURLY_PRO_COST_AFTER_SALES, &
                    HOURLY_CAPACITY, &
                    HOURLY_EUE, &
                    HOURLY_DERIVATIVES, &
                    M_HOURLY_PRO_COST_AFTER_SALES, &
                    HOURLY_LOADS, &
                    M_HOURLY_LOADS, &
                    TEST_HOUR_TIE_LIMIT, &
                    HOURLY_PRO_COST_B4_SALES, &
                    M_HOURLY_PRO_COST_B4_SALES, &
                    HOURLY_INCREMENTAL_COST, &
                    M_HOURLY_INCREMENTAL_COST, &
                    HOURLY_SELL_MWH, &
                    MONTHLY_TRANSACTION_MWH, &
                    ANNUAL_TRANSACTION_MWH, &
                    TOTAL_DELIVERED_COST, &
                    SELLERS_LOCAL_CAPACITY, &
                    TRANSACTIONS_WITHIN_HOUR, &
                    BUY_FOR_TRANSACTION, &
                    LAST_BUY_FOR_TRANSACTION, &
                    LAST_BUYER, &
                    LAST_SELLER, &
                    PRICE_ANCHORED, &
                    SELL_FOR_TRANSACTION, &
                    LAST_SELL_FOR_TRANSACTION, &
                    CUM_REDUNDANT_TRANSACTION, &
                    LONG_PATH_TRANSACTION, &
                    MARGIN_FOR_TRANSACTION, &
                    MW_FOR_TRANSACTION, &
                    LAST_MW_FOR_TRANSACTION, &
                    DAY_OF_WEEK, &
                    M_MONTHLY_PRO_COST_AFTER_SALES, &
                    M_MONTHLY_PRO_COST_B4_SALES, &
                    M_MONTHLY_LOAD_B4_SALES, &
                    M_MONTHLY_LOAD_AFTER_SALES, &
                    M_PURCHASE_ENERGY, &
                    M_PURCHASE_COSTS, &
                    MARKET_COST_ABOVE_RESOURCES, &
                    M_SALES_ENERGY, &
                    M_SALES_REVENUES, &
                    M_NATIVE_COST, &
                    M_SPINNING_MWH, &
                    M_ANNUAL_PURCHASE_ENERGY, &
                    M_ANNUAL_PURCHASE_COSTS, &
                    M_ANNUAL_SALES_ENERGY, &
                    M_ANNUAL_SALES_REVENUES, &
                    M_ANNUAL_NATIVE_COST, &
                    M_ANNUAL_LOAD_B4_SALES, &
                    M_ANNUAL_PRO_COST_B4_SALES, &
                    M_ANNUAL_LOAD_AFTER_SALES, &
                    M_ANNUAL_PRO_COST_AFTER_SALES, &
                    ANNUAL_TL_MWH, &
                    ANNUAL_TL_PEAK, &
                    ANNUAL_COIN_PEAK, &
                    MONTH_COIN_PEAK, &
                    MONTH_NON_COIN_PEAK, &
                    ANNUAL_TL_BASE, &
                    ANNUAL_TL_HYDRO_MWH, &
                    ANNUAL_TL_HYDRO_MW, &
                    ANNUAL_TL_HYDRO_ROR, &
                    ANNUAL_SPINNING_MWH, &
                    ANNUAL_EFFECTIVE_CAPACITY, &
                    FISCAL_PURCHASE_ENERGY, &
                    FISCAL_PURCHASE_COSTS, &
                    FISCAL_SALES_ENERGY, &
                    FISCAL_SALES_REVENUES, &
                    FISCAL_NATIVE_COST, &
                    FISCAL_LOAD_B4_SALES, &
                    FISCAL_PRO_COST_B4_SALES, &
                    FISCAL_LOAD_AFTER_SALES, &
                    FISCAL_PRO_COST_AFTER_SALES, &
                    FISCAL_TL_MWH, &
                    FISCAL_TL_PEAK, & 
                    FISCAL_COIN_PEAK, &
                    FISCAL_TL_BASE, &
                    FISCAL_TL_HYDRO_MWH, &
                    FISCAL_TL_HYDRO_MW, &
                    FISCAL_TL_HYDRO_ROR, &
                    FISCAL_SPINNING_MWH, &
                    FISCAL_EFFECTIVE_CAPACITY, &
                    FISCAL_UNSERVED_ENERGY, &
                    FISCAL_ABOVE_RESOURCES, &
                    FISCAL_UNSERVED_ENERGY_COST, &
                    FISCAL_COST_ABOVE_RESOURCES, &
                    ALLOWED_TRANSACTION_PAIR, &
                    TRANSACTIONS_PER_HOUR, &
                    LONG_PATH_TRANSACTION_PAIR, &
                    NET_MARGIN, &
                    BEST_PRICE_TO_BUYER, &
                    M_UNSERVED_ENERGY, &
                    M_ABOVE_RESOURCES, &
                    CAPPED_PRICE, &
                    PRICE_MINIMUM, &
                    ANNUAL_UNSERVED_ENERGY, &
                    ANNUAL_ABOVE_RESOURCES, &
                    M_UNSERVED_ENERGY_COST, &
                    ANNUAL_UNSERVED_ENERGY_COST, &
                    ANNUAL_COST_ABOVE_RESOURCES, &
                    M_HOURLY_LOAD_B4_SALES, &
                    MULTI_AREA_NAME, &
                    REPORT_AREA_ACTIVE, &
                    ANNUAL_MARKET_PRICES, &
                    USER_MARKET_PRICES, &
                    ZERO_MARKET_PRICES, &
                    HOURLY_DUMP_CAPACITY, &
                    HOURLY_DUMP_BEFORE, &
                    TRANS_MUST_CAPACITY, &
                    TRANS_SPINNING_CAPACITY, &
                    AREA_PRICE_MULT, &
                    HOURLY_SPINNING_CAPACITY, &
                    DAILY_PEAK, &
                    OFF_PEAK_SPINNING_CAPACITY, &
                    TRANS_RAMP_UP, &
                    HOURLY_TRANSACTION, &
                    MAX_HOURLY_IMPORT, &
                    MAX_HOURLY_EXPORT, &
                    TRANS_RAMP_DOWN, &
                    TRANS_MAX_IMPORT, &
                    TRANS_MAX_EXPORT, &
                    LAST_HOUR_SELL
!     
      REAL :: TIE_LIMIT,R_TIE_LIMIT=0, &
          REMAIN
      INTEGER(kind=2) :: MAX_TRANS_WITHIN_HOUR,MAX_TRANS_ITERATIONS
!      PARAMETER (MAX_TRANS_WITHIN_HOUR=100) ! 11/2/99.
      INTEGER(kind=2) :: NUM_ARGS,SPREADS_2_CHECK=0
      CHARACTER(len=256) :: OUTPUT_DIRECTORY, &
                    BASE_FILE_DIRECTORY, &
                    LDE_FILE_DIRECTORY, &
                    PRB_FILE_DIRECTORY
      INTEGER(kind=2) :: SPREADS,NUM_OF_SPREADS,R_SPREADS=0
      PARAMETER(NUM_OF_SPREADS=9)
      REAL :: SPREAD_PRICES(NUM_OF_SPREADS)= &
                (/0.,1.,2.,2.5,3.,4.,5.,6.,7./)
      REAL :: SPREAD_MARGIN, HOURLY_REVENUE,HOURLY_TRANSACTION_LOAD, &
           GET_TRANS_LOAD_AFTER_EL, &
           GET_MONTHLY_TL_MWH, &
           GET_MONTHLY_TL_HYDRO_MWH, &
           GET_WH_MONTH_ENERGY, &
           WH_MTH_ENERGY, &
           GET_WH_MONTH_CAPACITY, &
           GET_MONTHLY_TL_HYDRO_MW, &
           GET_TRANS_PEAK_AFTER_EL, &
           GET_TRANS_BASE_AFTER_EL, &
           TEMP_TL_PEAK, &
           TEMP_TL_BASE, &
           MAXIMUM_TRANSACTION_SIZE, &
           GET_MAXIMUM_TRANSACTION_SIZE
!
! MARKET PRICES
!
      REAL(kind=4) :: MONTHLY_RR(800)
      REAL(kind=4) :: MONTHLY_MARKET_PRICES(:),SYSTEM_OUTPUT(:)
      ALLOCATABLE :: MONTHLY_MARKET_PRICES,SYSTEM_OUTPUT
      CHARACTER(len=4) :: FILE_SUFFIX
      CHARACTER(len=256) :: FILE_NAME
      INTEGER(kind=2) :: R_YEAR
      INTEGER :: IOS
      REAL(kind=4) :: PURCHASE_ENERGY(12),PURCHASE_COSTS(12), &
            SALES_ENERGY(12),SALES_REVENUE(12),AVERAGE_COST, &
            MONTHLY_PRO_COST_AFTER_SALES(12), &
            MONTHLY_PRO_COST_B4_SALES(12), &
            ANNUAL_PRO_COST_AFTER_SALES, &
            ANNUAL_PRO_COST_B4_SALES, &
            MONTHLY_LOAD_B4_SALES(12), &
            MONTHLY_LOAD_AFTER_SALES(12), &
            ANNUAL_LOAD_B4_SALES, &
            ANNUAL_LOAD_AFTER_SALES, &
            AVE_B4, &
            AVE_AFTER, &
            BEST_MARGIN,SECOND_BEST_MARGIN,TEMP_MARGIN,MINIMUM_MARGIN, &
            TIE_COST_OF_BEST_MARGIN, &
            GET_MARKET_PRICE_SCALAR,MARKET_PRICE_SCALAR,GET_VAR
      REAL(kind=4) :: MARKET_PRICE, & ! GET_PRODUCTION_LEVEL, unused
            HOURLY_MARKET_PRICE, &
            TEMP_MARKET_PRICE, &
            PRICE_PLUS_SPREAD,TEMP_BUY_SPREAD,TEMP_SELL_SPREAD, &
            TEMP_BUY_WHEEL,TEMP_SELL_WHEEL
      REAL :: ANNUAL_PURCHASES, &
           ANNUAL_PURCHASE_COST, &
           ANNUAL_SALES, &
           ANNUAL_SALES_REVENUE, &
           FISCAL_PURCHASES, &
           FISCAL_PURCHASE_COST, &
           FISCAL_SALES, &
           FISCAL_SALES_REVENUE
      REAL(kind=4) :: PROB_2_SERVE_AFTER_SALES, &
            EXPECTED_SALE,PROB_2_SERVE_B4_SALES,GET_PROB_2_SERVE
!
      real(kind=4) :: BUY_SPREAD_MARGIN,SELL_SPREAD_MARGIN
      REAL(kind=4) :: GET_TRANS_GROUP_HOURLY_LOAD
      CHARACTER(len=5) :: BSYRLOAD
      LOGICAL(kind=4) :: FILE_EXISTS
!
! DETAILED REPORT OVERHEAD
!
      LOGICAL(kind=1) :: TRANS_HOURLY_REPORT_NOT_OPEN=.TRUE., &
                 TRANS_DAILY_REPORT_NOT_OPEN=.TRUE., &
                 TRANS_MONTHLY_REPORT_NOT_OPEN=.TRUE., &
                 TRANS_MONTHLY_EXCHANGE_NOT_OPEN=.TRUE., &
                 HOURLY_PRICE_REPORT_NOT_OPEN=.TRUE., &
                 LAST_PRICE_REPORT_NOT_OPEN=.TRUE., &
                 INIT_DAILY_TIE_REVENUES,WRITE_DAILY_TIE_REVENUES, &
                 HOUR_DIAG_REPORT_NOT_OPEN=.TRUE., &
                 RUN_HOUR_DIAG_REPORT=.FALSE., &
                 TRANS_HOURLY_DIAG, &
                 TRANS_TO_FROM_REPORT, &
                 YES_TRANS_TO_FROM_REPORT, &
                 TRANS_TRN_ACTIVE=.FALSE., &
                 MRX_ACTIVE=.FALSE.

      INTEGER(kind=2) :: PRODUCTION_PERIODS, &
                 TRANS_TIE_RPT_HEADER,TRANS_DEL_RPT_HEADER, &
                 TRANS_DEL_UNIT, &
                 TRANS_ANN_RPT_HEADER,TRANS_ANN_UNIT, &
                 DAILY_PRODUCTS_RPT_HEADER,DAILY_PRODUCTS_UNIT, &
                 TRANS_PRI_RPT_HEADER,TRANS_PRI_UNIT, &
                 TRANS_SCA_RPT_HEADER,trans_sca_unit_loc, &
                 TRANS_MCB_RPT_HEADER,TRANS_MCB_UNIT, &
                 TRANS_HMP_RPT_HEADER,TRANS_HMP_UNIT, &
                 TRANS_LDS_RPT_HEADER, &
                 TRANS_LAS_RPT_HEADER, &
                 TRANS_MRK_RPT_HEADER,TRANS_MRK_UNIT, &
                 TRANS_PB4_RPT_HEADER,TRANS_PB4_UNIT, &
                 TRANS_TRN_RPT_HEADER,TRANS_TRN_UNIT, &
                 TRANS_PAF_RPT_HEADER,TRANS_PAF_UNIT
      INTEGER (KIND=2), PARAMETER ::  TRANS_LDS_UNIT = 1935
      INTEGER(kind=2) :: &
                 TRANS_CAP_RPT_HEADER,TRANS_CAP_UNIT, &
                 TRANS_DER_RPT_HEADER,TRANS_DER_UNIT, &
                 TRANS_EUE_RPT_HEADER,TRANS_EUE_UNIT, &
                 TRANS_INC_RPT_HEADER,TRANS_INC_UNIT, &
                 TRANS_LMD_RPT_HEADER,TRANS_LMD_UNIT, &
                 TRANS_EXC_RPT_HEADER,TRANS_EXC_UNIT, &
                 DAY_WEEK_RPT_HEADER,DAY_WEEK_UNIT=0, &
                 HOUR_DIAG_RPT_HEADER,HOUR_DIAG_UNIT, &
                 CURRENT_YEAR=0, &
                 CURRENT_MONTH,&
                 CURRENT_TRANS_GROUP,TRANS_GROUP_LOAD, &
                 LOAD_UNIT, &
                 HOUR_DIAG_NUM, &
                 GET_DIAGNOSTIC_DAY_OF_MONTH, &
                 DIAGNOSTIC_DAY_OF_MONTH=1
      INTEGER :: TRANS_HMP_REC,TRANS_TIE_REC,TRANS_DEL_REC, &
             TRANS_PRI_REC,TRANS_SCA_REC,TRANS_MCB_REC, &
             TRANS_LMD_REC,TRANS_LDS_REC,TRANS_LAS_REC, &
             TRANS_PB4_REC,TRANS_TRN_REC,TRANS_PAF_REC, &
             TRANS_CAP_REC,TRANS_INC_REC,TRANS_MRK_REC, &
             TRANS_ANN_REC,DAY_WEEK_REC,DAILY_PRODUCTS_REC, &
             HOUR_DIAG_REC,TRANS_EXC_REC,TRANS_DER_REC, &
             TRANS_EUE_REC
!
      INTEGER(kind=4) :: SEARCHES_WITHIN_TRANSACTIONS, &
                        FAILED_SEARCH_SPEEDUP
!
      REAL(kind=8) :: & ! REAL(kind=4 before 20030429
              BUYER_AT_SELLERS_COST, &
              SELLER_AT_BUYERS_COST, &
              SELLERS_AVAILABLE_CAPACITY, &
              SELLER_SECOND_BEST_TRANS, &
              SELLER_SLOPE,SELLER_INTERCEPT, &
              BUYER_SLOPE,BUYER_INTERCEPT, &
              MAX_PRICE,MIN_PRICE, &
              MIN_TRANSACTION, &
              MAX_TRANSACTION, &
              TEMP_BUYER_TRANSACTION, &
              TEMP_SELLER_TRANSACTION, &
              TEMP_INTERP_TRANSACTION, &
              TRANS_GROUP_SELLER_CAP,TRANS_GROUP_BUYER_CAP, & ! ADDED 
                                               ! FOR DISPATCHABLE HYDRO.
              TRANS_GROUP_CAP, &
              I_AVAILABLE_CAPACITY, &
              J_AVAILABLE_CAPACITY
      REAL(kind=4) :: AVE_BUY,AVE_SELL, &
              TEMP_TRANSACTION, & ! AGT is considering making this 
                                  ! REAL(kind=8)
              TEMP_TRANSACTION_CHANGE, &
              MARKET_FLASH, &
              HOURLY_LOAD_FROM_AC_TG, &
              HOURLY_AC_LOAD, &
              UNSERVED, &
              ABOVE_RESOURCES, &
              HOURLY_ENERGY_ABOVE_RESOURCES
      CHARACTER(len=9) :: CL_MONTH_NAME(14)= &
                             (/'January  ','February ', &
                               'March    ','April    ', &
                               'May      ','June     ', &
                               'July     ','August   ', &
                               'September','October  ', &
                               'November ','December ', &
                               'Annual   ','Fiscal   '/)
      CHARACTER(len=2) :: LOAD_FILE_CHAR_EXT, &
                 CAPACITY_PLANNING_METHOD, &
                 SAVE_CAPACITY_PLANNING_METHOD='MI'
      CHARACTER(len=3) :: SCEN_FILE_CHAR_EXT
      REAL(kind=4) :: SellerTransPower
      INTEGER(kind=2) :: iTG ! use current value of global HOUR_IN_DAY
      INTEGER(kind=2) :: MAX_ACTIVE_GROUPS, &
               GET_MARKET_PRICE_YEAR
      LOGICAL(kind=1) :: LAHEY_LF95,LAHEY
      INTEGER (KIND = 2) :: DATA_YEAR
      integer (kind=2) :: this_unit
      integer (kind=2) :: throwaway

      logical :: beenhere=.false.
!
!
! END DATA DECLARATIONS
!
!
      if(.not. beenhere) then
        beenhere=.true. ! Debugstop
        call set_hourly_transaction_not_open(logical(.true.,1))
      end if
      TEST_DAILY_OPTION = YES_UNIT_COMMITMENT_LOGIC()

      C_M = TRANSACTION_MODE()
      IGNORE_NIGHTIME_CAPVAL = REMOVE_NIGHTIME_SCARCITY() == 'T'
      DUTCH_AUCTION_LOGIC = YES_BIDDING_LOGIC()
      MODIFIED_DUTCH_AUCTION = MODIFIED_BIDDING_LOGIC()

      
      BURESH = LONG_PATH_LOGIC()
!
      TIE_FLOW_DATA_MAX = 2 
      TIE_FLOW_DATA_MIN = 1 
      TIE_FLOW_DATA_SUM = 10
!
      TRANSACT_UNSERVED_COST = GET_TRANSACT_UNSERVED_COST()
      TRANSACT_ABUNDANCE_COST = MAX(0.1,GET_TRANSACT_ABUNDANCE_COST())
      FIRST_MONTHLY_TRANSACT = &
                             FIRST_LAST_MONTH_OF_TRANSACT(R_LAST_MONTH)

! 9/6/01
      USE_PRICE_CAPS = YES_PRICES_ARE_CAPPED()
! 9/7/01
      GLOBAL_SCARCITY_BUY_N_SELL = YES_GLOBAL_SCARCITY_BUY_N_SELL()
!
      FIRST_REPORTING_GROUP = FIRST_TRANSACT_REPORTING_GROUP()
!
      LAST_HOUR_TRANSACTIONS = YES_REMEMBER_LAST_HOUR()
!
! 5/8/01. SRP.
!

      YES_MARGINAL_UNIT_REPORT = MARGINAL_UNIT_REPORT()
! 10/08/01. FOR TVA/BURESH/OTHERS
      YES_HOURLY_CAPACITY_REPORT = HOURLY_CAPACITY_REPORT()
      HOURLY_TRANSACTION_REPORT = YES_HOURLY_TRANSACTION_REPORT()
      YES_HOURLY_CAPACITY_COST_REPORT = HOURLY_CAPACITY_COST_REPORT()
      YES_HOURLY_DERIVATIVES_REPORT = HOURLY_DERIVATIVES_REPORT()
      YES_HOURLY_AFTER_HYDRO_REPORT = &
                                       HOURLY_AFTER_HYDRO_REPORT()
      YES_HOURLY_AFTER_TRANS_REPORT = &
                                       HOURLY_AFTER_TRANS_REPORT()
!
! 120909. FOR BURESH AND ERCOT WEST. MULTI-MARKET ONLY.
!
      YES_USE_PRICE_MINIMUMS = USE_PRICE_MINIMUMS()
!
! 12/13/02. GAT. FOR PACIFICORP.

      YES_USE_5X16_LOGIC_CONSTRAINTS = USE_5X16_LOGIC_CONSTRAINTS()
      YES_DISALLOW_REDUNDANT_TRANS = DISALLOW_REDUNDANT_TRANS()

      IF(YES_USE_5X16_LOGIC_CONSTRAINTS) THEN
         HOUR_IS_ON_PEAK = 1
      ELSE
         HOUR_IS_ON_PEAK = 2
      ENDIF

! 10/22/03
!
      MULTI_AREA_TRANC = MULTI_AREA_TRANC_ACTIVE()
! 06/12/04.      
      MULTI_MARKET_ACTIVE = YES_MULTI_MARKET_ACTIVE()

!
! 10/6/00. FOR PACIFICORP.
!
      YES_MARKET_DURATION = MARKET_DURATION_REPORT()

      LAST_MONTHLY_TRANSACT = R_LAST_MONTH

      CALL DOES_TRPATH_FILE_EXIST(NEW_TRANS_LOGIC)

      DC_LOAD_FLOW = TRANS_REPRESENTATION() == 'D'
!
      SAVE_CAPACITY_PLANNING_METHOD = CAPACITY_PLANNING_METHOD()
      MRX_ACTIVE = SAVE_CAPACITY_PLANNING_METHOD == 'MX' .OR. &
                     SAVE_CAPACITY_PLANNING_METHOD == 'GX'
!      
      PRICE_ONLY_WHOLESALE_REV = APPLY_TRANS_REV_TO_WHOLESALE()
      DETAILED_TRANSFER_PRICING = YES_DETAILED_TRANSFER_PRICING()
      RUN_MULTIAREA = YES_RUN_MULTIAREA_TRANSACT()
      YES_TRANS_TO_FROM_REPORT = TRANS_TO_FROM_REPORT()
      CENTRAL_DISPATCH_TRANSACT = YES_CENTRAL_DISPATCH_TRANSACT()
      YES_STRICTLY_POSITIVE_MARGIN = STRICTLY_POSITIVE_MARGIN()
!
      HOURLY_PRICE_RECONCILE = YES_ADVANCED_PRICE_MODELING()
      PRICE_LAST_MW = YES_PRICE_LAST_MW()
      PRICE_LAST_MW_AS_UNSERVED = YES_PRICE_LAST_MW_AS_UNSERVED()
      LAMBDA_PRICE = YES_LAMBDA_PRICE()

!
! MOVED 11/4/99.
!
      ZERO_SELL = YES_ONLY_BUY_TRANSACT() .OR. &
                    YES_ONLY_DISPATCH_TRANSACT() .OR. &
                    CENTRAL_DISPATCH_TRANSACT
      ZERO_BUY = YES_ONLY_SELL_TRANSACT() .OR. &
                   YES_ONLY_DISPATCH_TRANSACT() .OR. &
                   CENTRAL_DISPATCH_TRANSACT
!      
      STRICT_MARKET_PRICE = YES_STRICT_MARKET_PRICE()
      MULTI_AREA_PRICE = YES_MULTI_AREA_PRICE()
      REGIONAL_AREA_PRICE = YES_REGIONAL_AREA_PRICE()
      USE_TF_FILE_FOR_MULTIAREA = YES_USE_TF_FILE_FOR_MULTIAREA()
      USE_TRANSACT_LOADS = YES_USE_TRANSACT_LOADS()
      USE_TF_FILE_FOR_PRICE = (USE_TF_FILE_FOR_MULTIAREA .AND. &
                              (STRICT_MARKET_PRICE .OR. &
                               MULTI_AREA_PRICE .OR. &
                               REGIONAL_AREA_PRICE .OR. &
                             CENTRAL_DISPATCH_TRANSACT)) .OR. &
                                ZERO_SELL .OR. &
                                ZERO_BUY
!
! ADDED FOR JANET. 8/12/99. GAT.
!
      MARKET_PRICE_SCALAR = GET_MARKET_PRICE_SCALAR()
      IF(MARKET_PRICE_SCALAR < 0.) THEN
         MARKET_PRICE_SCALAR = GET_VAR(MARKET_PRICE_SCALAR,ISEAS, &
                                                'MARKET PRICE SCALAR ')
      ENDIF
!      
!

      
      IF(RUN_MULTIAREA .AND. USE_TF_FILE_FOR_MULTIAREA) THEN
         USE_TF_FILE = .TRUE.
      ELSEIF(USE_TF_FILE_FOR_PRICE) THEN
         USE_TF_FILE = .TRUE.
      ELSE
         USE_TF_FILE = .FALSE. ! E.G. 'T','B','S' TRANSACT SWITCH
      ENDIF
! 8/14/02. TESTING FOR CASE WITH NO TF FILE.
      USE_TF_FILE = .TRUE.  ! this might be a problem

      IF(USE_TF_FILE_FOR_PRICE) THEN
         CALL GET_MAX_TRANS_LOAD_GROUPS(R_MAX_TRANS_LOADS)
         MAX_TRANS_LOADS = R_MAX_TRANS_LOADS
      ELSEIF(USE_TRANSACT_LOADS) THEN
         MAX_TRANS_LOADS = GET_MAX_TRANS_LOADS()
      ELSE
         MAX_TRANS_LOADS = 1
      ENDIF
      MAX_ACTIVE_GROUPS = GET_NUMBER_OF_ACTIVE_GROUPS()
!
! 08/19/03. 
!
      MAX_TRANS_GROUPS = MAX_ACTIVE_GROUPS

      YES_HOURLY_PRICE_REPORT = HOURLY_PRICE_REPORT()
      YES_TRANSACT_HOURLY_REPORTS = TRANSACT_HOURLY_REPORTS()
      YES_TRANSACT_DAILY_REPORTS = TRANSACT_DAILY_REPORTS()
      YES_TRANSACT_MONTHLY_REPORTS = TRANSACT_MONTHLY_REPORTS()


      IF(hrly_after_trans_not_open .AND. &
                       (YES_HOURLY_AFTER_TRANS_REPORT .OR. &
                               YES_TRANSACT_HOURLY_REPORTS) .AND. &
                                              .NOT. TESTING_PLAN ) THEN
         hrly_after_trans_not_open = .FALSE.
         throwaway = TRANS_LAS_RPT_HEADER(TRANS_LAS_REC)

         call check_unit(TRANS_LAS_UNIT, "trans_las_unit_transobj")
      ENDIF

      IF(HOURLY_AFTER_HYDRO_NOT_OPEN .AND. &
                       (YES_HOURLY_AFTER_HYDRO_REPORT .OR. &
                               YES_TRANSACT_HOURLY_REPORTS) .AND. & 
                                              .NOT. TESTING_PLAN ) THEN
         HOURLY_AFTER_HYDRO_NOT_OPEN = .FALSE.
         this_unit = TRANS_LDS_RPT_HEADER(TRANS_LDS_REC)
      ENDIF
!
      IF(HOURLY_DERIVATIVES_NOT_OPEN .AND. &
                       (YES_HOURLY_DERIVATIVES_REPORT .OR. &
                               YES_TRANSACT_HOURLY_REPORTS) .AND. & 
                                              .NOT. TESTING_PLAN ) THEN
         HOURLY_DERIVATIVES_NOT_OPEN = .FALSE.
         TRANS_DER_UNIT = TRANS_DER_RPT_HEADER(TRANS_DER_REC)
      ENDIF
!
      IF(HOURLY_CAPACITY_NOT_OPEN .AND. &
                       (YES_HOURLY_CAPACITY_REPORT .OR. &
                               YES_TRANSACT_HOURLY_REPORTS) .AND. &
                                              .NOT. TESTING_PLAN ) THEN
         HOURLY_CAPACITY_NOT_OPEN = .FALSE.
         TRANS_CAP_UNIT = TRANS_CAP_RPT_HEADER(TRANS_CAP_REC)
      ENDIF


      IF(get_HOURLY_TRANSACTION_NOT_OPEN() .AND. &
                       (HOURLY_TRANSACTION_REPORT .OR. &
                               YES_TRANSACT_HOURLY_REPORTS) .AND. & 
                                              .NOT. TESTING_PLAN ) THEN

         call set_HOURLY_TRANSACTION_NOT_OPEN (logical(.FALSE.,1))
         
         ! I believe trans_tie_unit is opened here.
         throwaway = TRANS_TIE_RPT_HEADER(TRANS_TIE_REC)
         call check_unit(trans_tie_unit, "transobj:0083")
         if(throwaway/=trans_tie_unit) then
            call end_program("transobj:-0082 - trans_tie_unit " // &
            "returned isn't the unit expected.")
         end if

         
        
      ENDIF
      if(.not. get_hourly_transaction_not_open()) then
        call set_hourly_transaction_not_open(get_hourly_transaction_not_open())! Debugstop
      end if

      IF(HOURLY_CAPACITY_COST_NOT_OPEN .AND. &
                       (YES_HOURLY_CAPACITY_COST_REPORT .OR. &
                               YES_TRANSACT_HOURLY_REPORTS) .AND. & 
                                              .NOT. TESTING_PLAN ) THEN

         HOURLY_CAPACITY_COST_NOT_OPEN = .FALSE.


         trans_sca_unit_loc = TRANS_SCA_RPT_HEADER(TRANS_SCA_REC)

      ENDIF

      IF(HOURLY_PRICE_REPORT_NOT_OPEN .AND. &
                       YES_HOURLY_PRICE_REPORT .AND. & 
                                              .NOT. TESTING_PLAN ) THEN
         HOURLY_PRICE_REPORT_NOT_OPEN = .FALSE.
         TRANS_HMP_UNIT = TRANS_HMP_RPT_HEADER(TRANS_HMP_REC)
         IF(LAST_PRICE_REPORT_NOT_OPEN) THEN
            LAST_PRICE_REPORT_NOT_OPEN = .FALSE.
            TRANS_MRK_UNIT = TRANS_MRK_RPT_HEADER(TRANS_MRK_REC)
         ENDIF
      ENDIF
      IF(.NOT. TRANS_TRN_ACTIVE .AND. &
                    RUN_MULTIAREA .AND. &
                                   YES_TRANS_TO_FROM_REPORT .AND. & 
                                              .NOT. TESTING_PLAN ) THEN
!
         TRANS_TRN_ACTIVE = .TRUE.
         TRANS_TRN_UNIT = TRANS_TRN_RPT_HEADER(TRANS_TRN_REC)
!         
      ENDIF

      ! #ENDLINE
      IF( TRANS_HOURLY_REPORT_NOT_OPEN .AND. &
                       YES_TRANSACT_HOURLY_REPORTS .AND. &
                                              .NOT. TESTING_PLAN ) THEN
         TRANS_HOURLY_REPORT_NOT_OPEN = .FALSE.

         TRANS_DEL_UNIT = TRANS_DEL_RPT_HEADER(TRANS_DEL_REC)
         TRANS_PRI_UNIT = TRANS_PRI_RPT_HEADER(TRANS_PRI_REC)

         TRANS_MCB_UNIT = TRANS_MCB_RPT_HEADER(TRANS_MCB_REC)
         IF(HOURLY_PRICE_REPORT_NOT_OPEN) THEN
            HOURLY_PRICE_REPORT_NOT_OPEN = .FALSE.
            TRANS_HMP_UNIT = TRANS_HMP_RPT_HEADER(TRANS_HMP_REC)
         ENDIF
         IF(LAST_PRICE_REPORT_NOT_OPEN) THEN
            LAST_PRICE_REPORT_NOT_OPEN = .FALSE.
            TRANS_MRK_UNIT = TRANS_MRK_RPT_HEADER(TRANS_MRK_REC)
         ENDIF
         TRANS_LMD_UNIT = TRANS_LMD_RPT_HEADER(TRANS_LMD_REC)

         TRANS_PB4_UNIT = TRANS_PB4_RPT_HEADER(TRANS_PB4_REC)

         TRANS_PAF_UNIT = TRANS_PAF_RPT_HEADER(TRANS_PAF_REC)

         TRANS_EUE_UNIT = TRANS_EUE_RPT_HEADER(TRANS_EUE_REC)
         TRANS_INC_UNIT = TRANS_INC_RPT_HEADER(TRANS_INC_REC)

      ENDIF
      IF(TRANS_DAILY_REPORT_NOT_OPEN .AND. &
                       YES_TRANSACT_DAILY_REPORTS .AND. &
                                              .NOT. TESTING_PLAN ) THEN
         TRANS_DAILY_REPORT_NOT_OPEN = .FALSE.
! 
! DAILY REPORTS ACTIVE FOR MARKET PRICE METHOD. GAT. 3/27/98.
         DAILY_PRODUCTS_UNIT = DAILY_PRODUCTS_RPT_HEADER( &
                                                    DAILY_PRODUCTS_REC)
!         
      ENDIF
      IF(TRANS_MONTHLY_REPORT_NOT_OPEN .AND. &
                       YES_TRANSACT_MONTHLY_REPORTS .AND. &
                                              .NOT. TESTING_PLAN ) THEN
         TRANS_MONTHLY_REPORT_NOT_OPEN = .FALSE.
         TRANS_ANN_UNIT = TRANS_ANN_RPT_HEADER(TRANS_ANN_REC)
         DAY_WEEK_UNIT = DAY_WEEK_RPT_HEADER(DAY_WEEK_REC)
      ENDIF

      RUN_HOUR_DIAG_REPORT = TRANS_HOURLY_DIAG() ! For Buresh 7/7/00
      DIAGNOSTIC_DAY_OF_MONTH = GET_DIAGNOSTIC_DAY_OF_MONTH()
      IF(HOUR_DIAG_REPORT_NOT_OPEN .AND. &
                       RUN_HOUR_DIAG_REPORT .AND. &
                                              .NOT. TESTING_PLAN ) THEN
         HOUR_DIAG_REPORT_NOT_OPEN = .FALSE.
         HOUR_DIAG_NUM = 29
         HOUR_DIAG_UNIT = HOUR_DIAG_RPT_HEADER(HOUR_DIAG_NUM, &
                                                         HOUR_DIAG_REC)
      ENDIF
      IF(RUN_MULTIAREA .AND. &
              TRANS_MONTHLY_EXCHANGE_NOT_OPEN .AND. & 
                             YES_TRANSACT_MONTHLY_REPORTS .AND. & 
                                              .NOT. TESTING_PLAN ) THEN
         TRANS_MONTHLY_EXCHANGE_NOT_OPEN = .FALSE.
! A TO/FROM REPORT IS NOT AVAILABLE FOR MARKET PRICE METHOD 
! (ONLY TRANSACT AGAINST MARKET)
         TRANS_EXC_UNIT = TRANS_EXC_RPT_HEADER(TRANS_EXC_REC)

      ENDIF
! MOVED FROM INSIDE READ LOADS. 4/20/98. GAT.
      MAX_TRANS_GROUP_NUMBER = GET_NUMBER_OF_ACTIVE_GROUPS()
!      
      ALLOCATE(MULTI_AREA_NAME(0:MAX_TRANS_GROUPS))
      ALLOCATE(TRANS_GROUP_LOAD_ACTIVE(MAX_TRANS_GROUP_NUMBER))
      ALLOCATE(REPORT_AREA_ACTIVE(MAX_TRANS_GROUPS))
!
      TRANS_GROUPS_RECORDS = 0         
      CALL GET_TRANS_GROUPS_RECORDS(TRANS_GROUPS_RECORDS)
!
      MULTI_AREA_NAME(0) = 'System'
      DO CURRENT_TRANS_GROUP = 1, MAX_TRANS_GROUPS
!
         IF(CURRENT_TRANS_GROUP <= TRANS_GROUPS_RECORDS) THEN
            MULTI_AREA_NAME(CURRENT_TRANS_GROUP) = &
                                    GET_GROUP_NAME(CURRENT_TRANS_GROUP)
            REPORT_AREA_ACTIVE(CURRENT_TRANS_GROUP) = &
                            GET_REPORT_TRANS_GROUP(CURRENT_TRANS_GROUP)
         ELSE            
            MULTI_AREA_NAME(CURRENT_TRANS_GROUP) = 'Group   '// &
                                LOAD_FILE_CHAR_EXT(CURRENT_TRANS_GROUP)
            REPORT_AREA_ACTIVE(CURRENT_TRANS_GROUP) = .TRUE.
         ENDIF
!     
      ENDDO
!
      CALL GET_CUM_HOURS_N_HOURS_IN_MONTH(ISEAS,HOURS_IN_MONTH, &
                                                             CUM_HOURS)
!
      I2_TEMP = PUT_SEASON_FOR_CONSTRAINTS(ISEAS)
!      
      IF(ISEAS == FIRST_MONTHLY_TRANSACT) THEN
!
! 6/21/02.
!
! TESTING ALAN'S ANNUAL THERMAL CALL STUFF.
!

!
!
         SAVE_MARKET_PRICE_NAME = 'NONE ' 
         SAVE_TIE_FLOW_NAME = 'NONE ' 
!
         ALLOCATE_PRICE_ARRAYS = .TRUE.
!
         DO J = 1, NUM_PRODUCTS
            PRODUCT_FIRST_PERIOD(J) = .TRUE.
         ENDDO
!         

!
! MOVED FOR FE: 09/23/03.
!

         I2_TEMP = DAILY_OPTION_ANNUAL_VALUES()
!         
         TEMP_L = READ_TRANSACT_TIE_DATA()

!
! 112204. 
!
         IF(.NOT. RUN_MULTIAREA) THEN
            TEMP_L = INIT_ANN_ALLOC_BLOCKS_2_CUST()
         ENDIF
!         
         if(YES_RUN_MULTIAREA_TRANSACT() .AND. &
                                        TRANS_REPRESENTATION() == 'D') &
              call SetupConstraints ! for LP solution to
                                    ! inter-nodal flows
         R_TIE_LIMIT = 999999.   
         NUM_ARGS = 8
         MARKET_RATES_DIR = OUTPUT_DIRECTORY()
         MIDAS_OUTPUT_DIR = OUTPUT_DIRECTORY()
         MIDAS_INPUT_DIR = OUTPUT_DIRECTORY()
!
         TRANSACT_OUTPUT_DIR = MARKET_RATES_DIR
!
         LOAD_INPUT_NAME = 'TEST'
         MARGINAL_COST_INPUT_NAME = 'TEST'
         SPREADS_2_CHECK = 1
!
         CURRENT_YEAR = get_base_year() + globecom_year
         DATA_YEAR = MIN(globecom_year,AVAIL_DATA_YEARS) 
!        
         ANNUAL_TIE_FLOW = 0.
         R_SPREADS = 1 ! TEMPORARY
      ENDIF

!
      IF(CURRENT_YEAR == 2019 .AND. ISEAS == 3) THEN
         TEMP_L = TEMP_L
      ENDIF
      IF(.NOT. MULTI_MARKET_ACTIVE) THEN
         TEMP_L = UPDATE_TRANS_PATH_DATA(ISEAS,DATA_YEAR)
      ENDIF
!
!
! ELECTRICITIES. 3/25/00.
!
      RETAINED_AVAILABLE(1) = GET_DUKE_RETAINED_CAPACITY()

      RESERVE_CAPACITY(1) = 0.
      RESERVE_CAPACITY(2) = 0.
      RESERVE_CAPACITY(3) = RESERVE_CAPACITY(1) + RESERVE_CAPACITY(2)
      MONTHLY_RESERVE_ENERGY(1) = 0.
      MONTHLY_RESERVE_ENERGY(2) = 0.
      MONTHLY_RESERVE_ENERGY(3) = 0.
      SUPPLEMENTAL_CAPACITY(1) = GET_DUKE_SUP_CAPACITY()
      SUPPLEMENTAL_CAPACITY(2) = GET_CPL_SUP_CAPACITY()
      SUPPLEMENTAL_CAPACITY(3) = SUPPLEMENTAL_CAPACITY(1) + &
                                   SUPPLEMENTAL_CAPACITY(2)
      MONTHLY_SUPPLEMENTAL_ENERGY(1) = 0.
      MONTHLY_SUPPLEMENTAL_ENERGY(2) = 0.
      MONTHLY_SUPPLEMENTAL_ENERGY(3) = 0.
!      
! NOT THE BEST TRAP      
!
      ECITIES = YES_ECITIES_UNITS_ACTIVE()

!
      HOOSIER = YES_HOOSIER()

      ALLOCATE(MONTHLY_MARKET_PRICES(HOURS_IN_MONTH))
      MONTHLY_MARKET_PRICES = 0.0
!
      IF(USE_TRANSACT_LOADS .OR. USE_TF_FILE) THEN
            if(hours_in_month>744) then
                call end_program ("transobj:0080 - hours_in_month " // &
      "value of " // trim(itos(int(hours_in_month))) // &
      " is invalid.")
            end if
         CALL READ_LOAD_DATA(HOURS_IN_MONTH,ISEAS) ! READS LDE FOR EACH 
                                                   ! TRANS LOAD GROUP.
         if(hours_in_month>744) then
          call end_program("transobj:0076 - hours_in_month should " // &
            "be <=744 and is " // trim(itos(int(hours_in_month))))
         end if
      ENDIF
!
      TRANSACTION_PERIOD = TRANS_TIME_FRAME()
!      
      IF(TRANSACTION_PERIOD == 'S') THEN
         HOURS_FOR_TRANSACTIONS = HOURS_IN_MONTH
         SAVE_DAYS_PER_TRANS_PERIOD = 4.
      ELSE
         IF(TRANSACTION_PERIOD == 'D') THEN
            HOURS_FOR_TRANSACTIONS = 24
         ELSEIF(TRANSACTION_PERIOD == 'W') THEN
            HOURS_FOR_TRANSACTIONS = 168
         ELSE
            HOURS_FOR_TRANSACTIONS = HOURS_IN_MONTH
         ENDIF
!      
         SAVE_DAYS_PER_TRANS_PERIOD = &
                  FLOAT(HOURS_IN_MONTH) / FLOAT(HOURS_FOR_TRANSACTIONS)
      ENDIF
!
      TRANSACT_C = .FALSE.


      call check_hours_in_month(hours_for_transactions, "transobj:0075")
      
      IF(RUN_MULTIAREA) THEN

         CALL SIMULATE_MULTI_PARTY(HOURS_FOR_TRANSACTIONS, &
                                           ISEAS,R_TIE_LIMIT,R_SPREADS)
      ELSE
!
! MOVED 10/22/98. GAT.
! READ MONTHLY MARKET PRICE FILE LDE<5 CHAR>.BXY WHERE XY = 01, 02, 03 
! FOR EACH YEAR.
!

            CALL READ_MARKET_DATA(ISEAS,HOURS_IN_MONTH) 
!
!        TESTING... TESTING... TESTING
!
            IF(ISEAS == FIRST_MONTHLY_TRANSACT) &
                             CALL READ_ANNUAL_MARKET_DATA(CURRENT_YEAR)
!         ENDIF
!
         TRANSACT_C = .TRUE.
!         .
         CALL MARKET_PRICE_TRANSACTIONS(HOURS_FOR_TRANSACTIONS,ISEAS, &
                                             R_TIE_LIMIT,R_SPREADS)
      ENDIF     
!
! 071108. FOR BURESH.
!
      r8TEMP = CALC_MONTHLY_DERIVATIVE_FIXED(ISEAS)
!
      DEALLOCATE(MULTI_AREA_NAME, &
                 TRANS_GROUP_LOAD_ACTIVE, &
                 REPORT_AREA_ACTIVE )
!
      IF(ISEAS == LAST_SEASON .AND. USE_TRANSACT_LOADS) then
        call close_unit(int(1722,2))
      end if
!
      R_TRANSACTION_DEMAND = PERIOD_TRANSACTION_DEMAND
!         
      if(YES_RUN_MULTIAREA_TRANSACT() .AND. &
                                    TRANS_REPRESENTATION() == 'D') THEN
         call DeallocLPSVariables ! for LPSolver
         call DeallocLPSInternals
      endif
! 3/19/02. GAT.
      TEMP_L = WRITE_MONTHLY_CLASS_SUMMARY(ISEAS)
!      
! ADDED 08/18/03.
!
      CALL RW_PROCESS_MESSAGES()
!

      RETURN ! BACK TO THE MIDAS GOLD SIMULATION SYSTEM
!      

      ENTRY GET_DAYS_PER_TRANS_PERIOD(R_DAYS_PER_TRANS_PERIOD)

         R_DAYS_PER_TRANS_PERIOD = SAVE_DAYS_PER_TRANS_PERIOD 
      RETURN


      ENTRY READ_MARKET_DATA(R_MONTH,R_HOURS_IN_MONTH)

!
! CASE 2 WITH BINARY MARKET FILES
!
!
         MONTHLY_MARKET_PRICES = .001
!
         CALL GET_NEW_MARKET_PRICE_NAME(MARKET_PRICE_NAME)
         IF(MARKET_PRICE_NAME == 'NONE') THEN
            CALL GET_MARKET_PRICE_NAME(MARKET_PRICE_NAME)
            IF(MARKET_PRICE_NAME == 'NONE') THEN
               WRITE(4,*) '*** line 1318 TRANSOBJ.FOR ***'
               WRITE(4,*) "NO MARKET PRICE INFORMATION FOUND"
               er_message='See WARNING MESSAGES -transobj.for-1'
               call end_program(er_message)
            ENDIF
            FILE_NAME = TRIM(LDE_FILE_DIRECTORY())//"LDE"// &
                       TRIM(MARKET_PRICE_NAME)//".B"// &
                         LOAD_FILE_CHAR_EXT(MIN(globecom_year,AVAIL_DATA_YEARS))
         ELSE
            FILE_NAME = TRIM(PRB_FILE_DIRECTORY())//"PRB"// &
                                      TRIM(MARKET_PRICE_NAME)//".P"// &
                   LOAD_FILE_CHAR_EXT(GET_MARKET_PRICE_YEAR(globecom_year))
         ENDIF

         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
         IF(FILE_EXISTS) THEN
            USE_MARKET_PRICING = .TRUE.
            OPEN(UNIT=2801,FILE=FILE_NAME,RECL=118, &
                                          ACCESS="DIRECT",STATUS="OLD")
            I = 1
            IREC = ALINE_LOAD_DATA(I,R_MONTH) 
            DAYS_IN_MONTH = HOURS_IN_MONTH/24
!            
            CURRENT_HR = 0
            DO DA = 1, DAYS_IN_MONTH
               READ(2801,REC=IREC) LDE_MONTH,LDE_DAY,LDE_YEAR, &
                                    EEICODE,DAY_WEEK, &
                                    TIMZON,TEMPER, &
                                    DELTMP, &
                                    (LOCAL_MARKET_PRICE(HR),HR=1,24)
               IREC = IREC + 1
! PUT EXTENSION PERIOD MULTIPLIER HERE
               IF(globecom_year > AVAIL_DATA_YEARS) THEN
                  CALL SGL_AREA_EXT_PERIOD_PRICE_ESC( &
                                               globecom_year,LOCAL_MARKET_PRICE)
               ENDIF
               DO HR = 1, 24
                  CURRENT_HR = CURRENT_HR + 1
                  MONTHLY_MARKET_PRICES(CURRENT_HR) = &
                                       MAX(.001,LOCAL_MARKET_PRICE(HR))
               ENDDO
            ENDDO
            call close_unit(int(2801,2))
!
         ELSE
            USE_MARKET_PRICING = .FALSE.
         ENDIF
!
!
!         
!
      RETURN

      ENTRY GET_DAILY_PRICE(R_DAY,R_LOCAL_MARKET_PRICE)

         START_HOUR = (R_DAY-1)*24 + 1
         END_HOUR = START_HOUR + 23
         I = 0
         DO HR = START_HOUR, END_HOUR
            I = I + 1
            R_LOCAL_MARKET_PRICE(I) = MONTHLY_MARKET_PRICES(HR)
         ENDDO
      RETURN

      ENTRY GET_DAILY_OPTION_PAYOFF(R_STRIKE_PRICE,R_DAY,R_DAILY_PAYOFF)

!
! DAILY PAYOFF SHOULD BE CALCULATED BEFORE HAND TO SAVE TIME, 
! UNLESS WE GET INTO RAMPING, WHERE INTER-HOUR MW CHANGES ARE IMPORTANT.
!
         R_DAILY_PAYOFF = 0.
         START_HOUR = (R_DAY-1)*24 + 1
         END_HOUR = START_HOUR + 23
         DO HR = START_HOUR, END_HOUR
            IF(R_STRIKE_PRICE < MONTHLY_MARKET_PRICES(HR)) THEN
               R_DAILY_PAYOFF = R_DAILY_PAYOFF + &
                                              MONTHLY_MARKET_PRICES(HR)
            ENDIF
         ENDDO
      RETURN

      ENTRY READ_ANNUAL_MARKET_DATA(R_YEAR)

!
         IF(ALLOCATED(ANNUAL_MARKET_PRICES)) &
                                       DEALLOCATE(ANNUAL_MARKET_PRICES)
         ALLOCATE(ANNUAL_MARKET_PRICES(ANNUAL_HOURS)) 
!
!
!
         CALL GET_NEW_MARKET_PRICE_NAME(MARKET_PRICE_NAME)
         
         IF(MARKET_PRICE_NAME == 'NONE') THEN
            CALL GET_MARKET_PRICE_NAME(MARKET_PRICE_NAME)
            IF(MARKET_PRICE_NAME == 'NONE') THEN
               WRITE(4,*) '*** line 1411 TRANSOBJ.FOR ***'
               WRITE(4,*) "NO MARKET PRICE INFORMATION FOUND"
               er_message='See WARNING MESSAGES -transobj.for-2'
               call end_program(er_message)
            ENDIF
            FILE_NAME = TRIM(LDE_FILE_DIRECTORY())//"LDE"// &
                       TRIM(MARKET_PRICE_NAME)//".B"// &
                         LOAD_FILE_CHAR_EXT(MIN(globecom_year,AVAIL_DATA_YEARS))
         ELSE
            FILE_NAME = TRIM(PRB_FILE_DIRECTORY())//"PRB"// &
                                     TRIM(MARKET_PRICE_NAME)//".P"// &
                       LOAD_FILE_CHAR_EXT(GET_MARKET_PRICE_YEAR(globecom_year))
         ENDIF
         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
         IF(FILE_EXISTS) THEN
            ANNUAL_MARKET_PRICING = .TRUE.
            OPEN(UNIT=2801,FILE=FILE_NAME,RECL=118, &
                                          ACCESS="DIRECT",STATUS="OLD")
            I = 1
            CURRENT_HR = 1
            SUM_MARKET_PRICES = 0.
            MAX_MARKET_PRICE = 0.
            MIN_MARKET_PRICE = 999999.
            ANNUAL_STARTING_HOUR = 1
            ANNUAL_TOTAL_HOURS = 8760
!
            DO LOCAL_MONTH = 1, 12
!
               MONTH_STARTING_HOUR = CURRENT_HR
               MONTH_TOTAL_HOURS = 0
               MONTH_SUM_MARKET_PRICES(LOCAL_MONTH) = 0.
               MONTH_MAX_MARKET_PRICE(LOCAL_MONTH) = 0.
               MONTH_MIN_MARKET_PRICE(LOCAL_MONTH) = 999999.
!
               CALL GET_CUM_HOURS_N_HOURS_IN_MONTH(LOCAL_MONTH, &
                                                  LOCAL_HOURS, &
                                                  LOCAL_CUM_HOURS)
               IREC = ALINE_LOAD_DATA(I,LOCAL_MONTH) 
               DAYS_IN_MONTH = LOCAL_HOURS/24
!            
               DO DA = 1, DAYS_IN_MONTH
                  READ(2801,REC=IREC,IOSTAT=IOS) &
                                      LDE_MONTH,LDE_DAY,LDE_YEAR, &
                                      EEICODE,DAY_WEEK, &
                                      TIMZON,TEMPER, &
                                      DELTMP, &
                                      (LOCAL_MARKET_PRICE(HR),HR=1,24)
                  IF(IOS /= 0) THEN
                     WRITE(4,*) "UNEXPECTED END OF FILE ENCOUNTERED"
                     WRITE(4,*) "IN FILE ",FILE_NAME," FOR ANNUAL"
                     WRITE(4,*) "MARKET PRICES"
                     er_message='Stop requested from transobj SIID304'
                     call end_program(er_message)
                  ENDIF
                  IREC = IREC + 1
!                  
! IGNORE LEAP YEARS FOR NOW.

                  IF(LDE_DAY == 29 .AND. LDE_MONTH == 2) CYCLE
! PUT EXTENSION PERIOD MULTIPLIER HERE
                  IF(globecom_year > AVAIL_DATA_YEARS) THEN
                     CALL SGL_AREA_EXT_PERIOD_PRICE_ESC( &
                                               globecom_year,LOCAL_MARKET_PRICE)
                  ENDIF
                  DO HR = 1, 24
                     TEMP_PRICE = LOCAL_MARKET_PRICE(HR) * &
                                                    MARKET_PRICE_SCALAR
                     MONTH_SUM_MARKET_PRICES(LOCAL_MONTH) = &
                             MONTH_SUM_MARKET_PRICES(LOCAL_MONTH) + &
                                                             TEMP_PRICE 
                     MONTH_MAX_MARKET_PRICE(LOCAL_MONTH) =&
                              MAX(MONTH_MAX_MARKET_PRICE(LOCAL_MONTH), &
                                                           TEMP_PRICE )
                     MONTH_MIN_MARKET_PRICE(LOCAL_MONTH) = &
                              MIN(MONTH_MIN_MARKET_PRICE(LOCAL_MONTH), &
                                                           TEMP_PRICE )
                     ANNUAL_MARKET_PRICES(CURRENT_HR) = TEMP_PRICE 
                     CURRENT_HR = CURRENT_HR + 1
                     MONTH_TOTAL_HOURS = MONTH_TOTAL_HOURS + 1
                  ENDDO ! HOURS
               ENDDO ! DAYS
!
               CALL MARKET_PRICE_PROB(MONTH_STARTING_HOUR, &
                   MONTH_TOTAL_HOURS, &
                   ANNUAL_MARKET_PRICES, & 
                   MONTH_SUM_MARKET_PRICES(LOCAL_MONTH), & ! REAL(kind=8
                   MONTH_MAX_MARKET_PRICE(LOCAL_MONTH), & 
                   MONTH_MIN_MARKET_PRICE(LOCAL_MONTH), &
                   LOCAL_MONTH)
!
               SUM_MARKET_PRICES = SUM_MARKET_PRICES + &
                                   MONTH_SUM_MARKET_PRICES(LOCAL_MONTH)
               MAX_MARKET_PRICE = &
                           MAX(MAX_MARKET_PRICE, &
                                   MONTH_MAX_MARKET_PRICE(LOCAL_MONTH))
               MIN_MARKET_PRICE = &
                           MIN(MIN_MARKET_PRICE, &
                                   MONTH_MIN_MARKET_PRICE(LOCAL_MONTH))
!
            ENDDO ! MONTHS
            call close_unit(int(2801,2))
!
            CALL MARKET_PRICE_PROB(ANNUAL_STARTING_HOUR, &
                                 ANNUAL_TOTAL_HOURS, &
                                 ANNUAL_MARKET_PRICES, & 
                                 SUM_MARKET_PRICES, & ! REAL(kind=8)
                                 MAX_MARKET_PRICE, &
                                 MIN_MARKET_PRICE, &
                                 LOCAL_MONTH)
!
         ELSE
            ANNUAL_MARKET_PRICING = .FALSE.
         ENDIF
!
!
         MIN_MAX_THERMAL = .FALSE.
!
         IF(MIN_MAX_THERMAL) &
                    CALL PUT_ANNUAL_MARKET_PRICES(ANNUAL_MARKET_PRICES)
!
!         
!
      RETURN

      ENTRY WRITE_DAILY_MARKET_DATA(R_MONTH,R_DAY,R_DAY_WEEK, &
                                   R_LOCAL_MARKET_PRICE)

!
         IF(R_MONTH == FIRST_MONTHLY_TRANSACT .AND. R_DAY == 1) THEN
            IF(ALLOCATED(ANNUAL_MARKET_PRICES)) &
                                       DEALLOCATE(ANNUAL_MARKET_PRICES)
            ALLOCATE(ANNUAL_MARKET_PRICES(ANNUAL_HOURS)) 
            ANNUAL_MARKET_PRICES = 0.
!
!
!
            MARKET_PRICE_NAME = 'ABCDE'
            FILE_NAME = trim(LDE_FILE_DIRECTORY())//"LDE"// &
                                      trim(MARKET_PRICE_NAME)//".B"// &
                                               LOAD_FILE_CHAR_EXT(globecom_year)
            INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
            IF(FILE_EXISTS) THEN
              CALL ERASE(FILE_NAME)
            ENDIF
            OPEN(UNIT=2811,FILE=FILE_NAME,FORM='UNFORMATTED',RECL=118, &
                                     ACCESS="DIRECT",STATUS="UNKNOWN")
            I = 1
            LOCAL_HR = 1
            SUM_MARKET_PRICES = 0.
            MAX_MARKET_PRICE = 0.
            MIN_MARKET_PRICE = 999999.
            ANNUAL_STARTING_HOUR = 1
            ANNUAL_TOTAL_HOURS = 8784
            IF(CURRENT_YEAR > 1999) THEN
               loc_year = CURRENT_YEAR - 2000
            ELSE
               loc_year = CURRENT_YEAR - 1900
            ENDIF
         ENDIF
!
!            DO LOCAL_MONTH = 1, 12
         IF(R_DAY == 1) THEN
            LOCAL_MONTH = R_MONTH
!
            MONTH_STARTING_HOUR = LOCAL_HR
            MONTH_TOTAL_HOURS = 0
            MONTH_SUM_MARKET_PRICES(LOCAL_MONTH) = 0.
            MONTH_MAX_MARKET_PRICE(LOCAL_MONTH) = 0.
            MONTH_MIN_MARKET_PRICE(LOCAL_MONTH) = 999999.
!
            CALL GET_CUM_HOURS_N_HOURS_IN_MONTH(LOCAL_MONTH, &
                                               LOCAL_HOURS, &
                                               LOCAL_CUM_HOURS)
            DAILY_IREC = ALINE_LOAD_DATA(I,LOCAL_MONTH) 
            DAYS_IN_MONTH = LOCAL_HOURS/24
         ENDIF
!            
!               DO DA = 1, DAYS_IN_MONTH
         WRITE(2811,REC=DAILY_IREC)  R_MONTH,R_DAY,loc_year, &
                              EEICODE,R_DAY_WEEK, &
                              TIMZON,TEMPER, &
                              DELTMP, &
                       (R_LOCAL_MARKET_PRICE(MARKET_HR),MARKET_HR=1,24)
         DAILY_IREC = DAILY_IREC + 1
!                  
! ALWAYS ADD LEAP YEAR INTO LDE FILE
!
         IF(R_DAY == 28 .AND. R_MONTH == 2) THEN
            WRITE(2811,REC=DAILY_IREC)R_MONTH,INT(29,2),loc_year, &
                               EEICODE,R_DAY_WEEK, &
                               TIMZON,TEMPER, &
                               DELTMP, &
                       (R_LOCAL_MARKET_PRICE(MARKET_HR),MARKET_HR=1,24)
            DAILY_IREC = DAILY_IREC + 1
         ENDIF
!        
         DO MARKET_HR = 1, 24
            TEMP_PRICE = R_LOCAL_MARKET_PRICE(MARKET_HR) * &
                                                    MARKET_PRICE_SCALAR
            MONTH_SUM_MARKET_PRICES(LOCAL_MONTH) = &
                          MONTH_SUM_MARKET_PRICES(LOCAL_MONTH) + &
                                                             TEMP_PRICE 
            MONTH_MAX_MARKET_PRICE(LOCAL_MONTH) = &
                              MAX(MONTH_MAX_MARKET_PRICE(LOCAL_MONTH), &
                                                          TEMP_PRICE )
            MONTH_MIN_MARKET_PRICE(LOCAL_MONTH) = &
                              MIN(MONTH_MIN_MARKET_PRICE(LOCAL_MONTH), &
                                                          TEMP_PRICE )
            ANNUAL_MARKET_PRICES(LOCAL_HR) = TEMP_PRICE 
            LOCAL_HR = LOCAL_HR + 1
            MONTH_TOTAL_HOURS = MONTH_TOTAL_HOURS + 1
         ENDDO ! HOURS
!                  
! ALWAYS ADD LEAP YEAR INTO LDE FILE
!
         IF(R_DAY == 28 .AND. R_MONTH == 2) THEN
           WRITE(2811,REC=DAILY_IREC)    R_MONTH,INT(29,2),loc_year, &
                                   EEICODE,R_DAY_WEEK, &
                                   TIMZON,TEMPER, &
                                   DELTMP, &
                       (R_LOCAL_MARKET_PRICE(MARKET_HR),MARKET_HR=1,24)
            DAILY_IREC = DAILY_IREC + 1
            DO MARKET_HR = 1, 24
               TEMP_PRICE = R_LOCAL_MARKET_PRICE(MARKET_HR) * &
                                                   MARKET_PRICE_SCALAR
             MONTH_SUM_MARKET_PRICES(LOCAL_MONTH) = &
                            MONTH_SUM_MARKET_PRICES(LOCAL_MONTH) + &
                                                            TEMP_PRICE 
             MONTH_MAX_MARKET_PRICE(LOCAL_MONTH) = &
                              MAX(MONTH_MAX_MARKET_PRICE(LOCAL_MONTH), &
                                                          TEMP_PRICE )
             MONTH_MIN_MARKET_PRICE(LOCAL_MONTH) = &
                              MIN(MONTH_MIN_MARKET_PRICE(LOCAL_MONTH), &
                                                          TEMP_PRICE )
               ANNUAL_MARKET_PRICES(LOCAL_HR) = TEMP_PRICE 
               LOCAL_HR = LOCAL_HR + 1
               MONTH_TOTAL_HOURS = MONTH_TOTAL_HOURS + 1
            ENDDO
         ENDIF
!               ENDDO ! DAYS
!
         IF(LOCAL_HR == MONTH_STARTING_HOUR + LOCAL_HOURS) THEN
            CALL MARKET_PRICE_PROB(MONTH_STARTING_HOUR, &
                  MONTH_TOTAL_HOURS, &
                  ANNUAL_MARKET_PRICES, & 
                  MONTH_SUM_MARKET_PRICES(LOCAL_MONTH), & ! REAL(kind=8)
                  MONTH_MAX_MARKET_PRICE(LOCAL_MONTH), & 
                  MONTH_MIN_MARKET_PRICE(LOCAL_MONTH), &
                  LOCAL_MONTH)
!
            SUM_MARKET_PRICES = SUM_MARKET_PRICES + &
                                   MONTH_SUM_MARKET_PRICES(LOCAL_MONTH)
            MAX_MARKET_PRICE = &
                           MAX(MAX_MARKET_PRICE, &
                                   MONTH_MAX_MARKET_PRICE(LOCAL_MONTH))
            MIN_MARKET_PRICE =&
                           MIN(MIN_MARKET_PRICE, &
                                   MONTH_MIN_MARKET_PRICE(LOCAL_MONTH))
         ENDIF
!
!            ENDDO ! MONTHS
         IF(R_MONTH == LAST_MONTHLY_TRANSACT .AND. R_DAY == 31) THEN
!         
            IF(LAHEY_LF95()) THEN
              WRITE(2811,REC=368) &
                 (MONTH_MAX_MARKET_PRICE(LOCAL_MONTH), &
                                     LOCAL_MONTH=1,12),MAX_MARKET_PRICE
              WRITE(2811,REC=369) &
                 (MONTH_MIN_MARKET_PRICE(LOCAL_MONTH), &
                                     LOCAL_MONTH=1,12),MIN_MARKET_PRICE
              WRITE(2811,REC=370) &
                 (SNGL(MONTH_SUM_MARKET_PRICES(LOCAL_MONTH)), &
                             LOCAL_MONTH=1,12),SNGL(SUM_MARKET_PRICES)
           ELSE
              WRITE(2811,REC=367) &
                 (MONTH_MAX_MARKET_PRICE(LOCAL_MONTH), &
                                     LOCAL_MONTH=1,12),MAX_MARKET_PRICE
              WRITE(2811,REC=368) &
                 (MONTH_MIN_MARKET_PRICE(LOCAL_MONTH), &
                                     LOCAL_MONTH=1,12),MIN_MARKET_PRICE
              WRITE(2811,REC=369) &
                 (SNGL(MONTH_SUM_MARKET_PRICES(LOCAL_MONTH)), &
                             LOCAL_MONTH=1,12),SNGL(SUM_MARKET_PRICES)
            ENDIF
!         
            call close_unit(int(2811,2))
!
            LOCAL_MONTH = 13 ! FOR ANNUAL VALUE
!
            CALL MARKET_PRICE_PROB(ANNUAL_STARTING_HOUR, &
                                  ANNUAL_TOTAL_HOURS, &
                                  ANNUAL_MARKET_PRICES, & 
                                  SUM_MARKET_PRICES, & ! REAL(kind=8
                                  MAX_MARKET_PRICE, &
                                  MIN_MARKET_PRICE, &
                                  LOCAL_MONTH)
         ENDIF

      RETURN

      ENTRY WRITE_USER_MARKET_DATA(R_MONTH,R_DAY,R_DAY_WEEK, &
                                   R_TRANS_GROUP, &
                                   R_MULTI_AREA_NAME, &
                                   R_CREATE_HOURLY_PRICE)

!
         LOCAL_UNIT_NO = 2813 + R_TRANS_GROUP
         IF(R_MONTH == FIRST_MONTHLY_TRANSACT .AND. R_DAY == 1) THEN
!            IF(R_TRANS_GROUP == 1) THEN
! 7/16/01.
!
            IF(ALLOCATE_PRICE_ARRAYS) THEN
!
               ALLOCATE_PRICE_ARRAYS = .FALSE.
!       
               IF(ALLOCATED(USER_MARKET_PRICES)) &
                                     DEALLOCATE(USER_MARKET_PRICES, &
                                          ZERO_MARKET_PRICES, &
                                          MAX_USER_W_PRICE, &
                                          MIN_USER_W_PRICE, &
                                          MONTH_MAX_USER_W_PRICE, &
                                          MONTH_MIN_USER_W_PRICE, &
                                          SUM_USER_W_PRICES, &
                                          MONTH_SUM_USER_W_PRICES)
               ALLOCATE( &
                   USER_MARKET_PRICES(ANNUAL_HOURS,UPPER_TRANS_GROUP))
               ALLOCATE( &
                   ZERO_MARKET_PRICES(ANNUAL_HOURS,UPPER_TRANS_GROUP))
               ALLOCATE(MAX_USER_W_PRICE(UPPER_TRANS_GROUP))
               ALLOCATE(MIN_USER_W_PRICE(UPPER_TRANS_GROUP))
               ALLOCATE(MONTH_MAX_USER_W_PRICE(12,UPPER_TRANS_GROUP))
               ALLOCATE(MONTH_MIN_USER_W_PRICE(12,UPPER_TRANS_GROUP))
               ALLOCATE(SUM_USER_W_PRICES(UPPER_TRANS_GROUP))
               ALLOCATE(MONTH_SUM_USER_W_PRICES(12,UPPER_TRANS_GROUP))
               TEMP_I4 = INT(ANNUAL_HOURS)*INT(UPPER_TRANS_GROUP)
               USER_MARKET_PRICES = 0.
               USER_MARKET_PRICES = 0.
!
               DO USER_MONTH = 1, 12
                 DO LOCAL_TRANS_GROUP = 1, UPPER_TRANS_GROUP
                    MONTH_SUM_USER_W_PRICES( &
                                     USER_MONTH,LOCAL_TRANS_GROUP) = 0.
                    MONTH_MAX_USER_W_PRICE( &
                                     USER_MONTH,LOCAL_TRANS_GROUP) = 0.
                    MONTH_MIN_USER_W_PRICE( &
                                     USER_MONTH,LOCAL_TRANS_GROUP) = & 
                                                                999999.
                  ENDDO
               ENDDO
!               
            ENDIF
!
! 082806. FOR BURESH.
!
            IF(R_CREATE_HOURLY_PRICE) THEN
!
! 092706. TEST WITH NEW TEMP_I AND NO YES_TWO_PRB_DIGITS.
! 092806. JUST CHANGED TEMP_I BACK.
! 092806. HARD-WIRED FROM LOAD_FILE_CHAR_EXT TO SCEN_FILE_CHAR_EXT 
!         (STILL NOT USING YES_TWO_PRB_DIGITS) 
! 092806. ADDED TEMP_MULTI_AREA_NAME
! 092906. SEEMS TO BE RELATED TO THE MARKET_PRICE_NAME WHEN 
!         IT IS THREE CHARACTERS.
! 093006. CONFIRMED THAT THE TWO DIGIT VERSION WORKS.
!
               TEMP_I = GET_PRB_SEQUENCE_NUMBER() + gc_end_point - 1
               TEMP_MULTI_AREA_NAME = R_MULTI_AREA_NAME


               IF(YES_TWO_PRB_DIGITS() .AND. TEMP_I < 99) THEN
                  MARKET_PRICE_NAME = TRIM(TEMP_MULTI_AREA_NAME)// &
                                             LOAD_FILE_CHAR_EXT(TEMP_I)
               ELSE

                  MARKET_PRICE_NAME = &
                        TRIM(R_MULTI_AREA_NAME(1:2))// &
                                             SCEN_FILE_CHAR_EXT(TEMP_I)
               ENDIF
!

!              CHECK FOR UNIQUE NAME FOR EACH GROUP?
!
               FILE_NAME = trim(PRB_FILE_DIRECTORY())//"PRB"// &
                                      trim(MARKET_PRICE_NAME)//".P"// &
                        LOAD_FILE_CHAR_EXT(GET_MARKET_PRICE_YEAR(globecom_year))

               DO TEMP_I = 1, 256
                  IF(MARKET_PRICE_NAME == &
                                   SAVE_MARKET_PRICE_NAME(TEMP_I)) THEN 
                     WRITE(4,*) 'REDUNDANT MARKET PRICE NAMES'
                     WRITE(4,*) 'MARKET NAME = ',MARKET_PRICE_NAME
                     WRITE(4,*) 'PLEASE RENAME IN THE TRANSACTION'
                     WRITE(4,*) 'GROUPS FILE'
                     er_message='Stop requested from transobj SIID305'
                     call end_program(er_message)
                  ELSEIF(SAVE_MARKET_PRICE_NAME(TEMP_I) == 'NONE ') THEN
                     SAVE_MARKET_PRICE_NAME(TEMP_I) = MARKET_PRICE_NAME
                     EXIT
                  ENDIF
               END DO
               INQUIRE(UNIT=LOCAL_UNIT_NO,OPENED=PRICE_FILE_OPEN)
               IF(PRICE_FILE_OPEN) then
                call close_unit(LOCAL_UNIT_NO)
               end if
!
! 110206. THE FILE DID NOT SEEM TO PROPERLY ERASE PER DOUG E-MAIL. 
!         SO REPLACE IF EXISTS
!
               INQUIRE(FILE=FILE_NAME,EXIST=PRICE_FILE_EXISTS)
               IF(PRICE_FILE_EXISTS) THEN 

                  OPEN(UNIT=LOCAL_UNIT_NO, &
                    FILE=FILE_NAME,FORM='UNFORMATTED',RECL=118, &
                                     ACCESS="DIRECT",STATUS="REPLACE")
               ELSE
                  OPEN(UNIT=LOCAL_UNIT_NO, &
                    FILE=FILE_NAME,FORM='UNFORMATTED',RECL=118, &
                                     ACCESS="DIRECT",STATUS="NEW")
               ENDIF
! 07/07/03.
               IF(LAHEY_LF95()) THEN
                  call check_unit(local_unit_no, "transobj:0003")
                  WRITE(LOCAL_UNIT_NO,REC=1) F7,INT(118,2)
               ENDIF
            ENDIF
            USER_HR = 1
            SUM_USER_W_PRICES(R_TRANS_GROUP) = 0.
            MAX_USER_W_PRICE(R_TRANS_GROUP) = 0.
            MIN_USER_W_PRICE(R_TRANS_GROUP) = 999999.
            USER_ANNUAL_STARTING_HOUR = 1
            ANNUAL_TOTAL_HOURS = 8784
            IF(CURRENT_YEAR > 1999) THEN
               USER_YEAR = CURRENT_YEAR - 2000
            ELSE
               USER_YEAR = CURRENT_YEAR - 1900
            ENDIF
!
         ENDIF
!
!            DO USER_MONTH = 1, 12
         IF(R_DAY == 1) THEN
            USER_MONTH = R_MONTH
            CALL GET_CUM_HOURS_N_HOURS_IN_MONTH(USER_MONTH, &
                                               USER_HOURS, &
                                               USER_CUM_HOURS)
            DAYS_IN_MONTH = USER_HOURS/24
!
            IF(R_TRANS_GROUP == 1) THEN
               USER_MONTH_STARTING_HOUR = USER_HR
               CALL INIT_USER_MARKET_PRICE_PROB(UPPER_TRANS_GROUP)
            ENDIF
!
            IF(USER_MONTH == 2) USER_HOURS = USER_HOURS + 24
!               
         ENDIF
!
! 11/1/99.
!         
         IF(R_CREATE_HOURLY_PRICE) THEN
            USER_IREC = ALINE_LOAD_DATA(USER_I,R_MONTH) + R_DAY - 1
            call check_unit(local_unit_no, "transobj:0004")
            WRITE(LOCAL_UNIT_NO,REC=USER_IREC) &
                              R_MONTH,R_DAY,USER_YEAR, &
                              EEICODE,R_DAY_WEEK, &
                              TIMZON,TEMPER, &
                              DELTMP, &
            (HOURLY_LAST_PRICE(R_TRANS_GROUP,MARKET_HR),MARKET_HR=1,24)
            USER_IREC = USER_IREC + 1
!                  
! ALWAYS ADD LEAP YEAR INTO LDE FILE
!
            IF(R_DAY == 28 .AND. R_MONTH == 2) THEN
               call check_unit(LOCAL_UNIT_NO, "transobj:0006")
               WRITE(LOCAL_UNIT_NO,REC=USER_IREC) &
                               R_MONTH,INT(29,2),USER_YEAR, &
                               EEICODE,R_DAY_WEEK, &
                               TIMZON,TEMPER, &
                               DELTMP, &
            (HOURLY_LAST_PRICE(R_TRANS_GROUP,MARKET_HR),MARKET_HR=1,24)
               USER_IREC = USER_IREC + 1
            ENDIF
         ENDIF
!
         USER_HR = (R_DAY-1) * 24 + USER_MONTH_STARTING_HOUR
!        
         DO MARKET_HR = 1, 24
          TEMP_PRICE = HOURLY_LAST_PRICE(R_TRANS_GROUP,MARKET_HR) * &
                                                   MARKET_PRICE_SCALAR
          MONTH_SUM_USER_W_PRICES(USER_MONTH,R_TRANS_GROUP) = &
                   MONTH_SUM_USER_W_PRICES(USER_MONTH,R_TRANS_GROUP) + &
                                                            TEMP_PRICE 
          MONTH_MAX_USER_W_PRICE(USER_MONTH,R_TRANS_GROUP) = &
                 MAX(MONTH_MAX_USER_W_PRICE(USER_MONTH,R_TRANS_GROUP), &
                                                          TEMP_PRICE )
          MONTH_MIN_USER_W_PRICE(USER_MONTH,R_TRANS_GROUP) = &
                 MIN(MONTH_MIN_USER_W_PRICE(USER_MONTH,R_TRANS_GROUP), &
                                                          TEMP_PRICE )
            USER_MARKET_PRICES(USER_HR,R_TRANS_GROUP) = TEMP_PRICE 
            USER_HR = USER_HR + 1

         ENDDO ! HOURS
!                  
! ALWAYS ADD LEAP YEAR INTO LDE FILE
!
         IF(R_DAY == 28 .AND. R_MONTH == 2) THEN
            IF(R_CREATE_HOURLY_PRICE) THEN
               call check_unit(local_unit_no, "transobj:0007")
               WRITE(LOCAL_UNIT_NO,REC=USER_IREC) &    
                                   R_MONTH,INT(29,2),USER_YEAR, &
                                   EEICODE,R_DAY_WEEK, &
                                   TIMZON,TEMPER, &
                                   DELTMP, &
            (HOURLY_LAST_PRICE(R_TRANS_GROUP,MARKET_HR),MARKET_HR=1,24)
               USER_IREC = USER_IREC + 1
            ENDIF
            DO MARKET_HR = 1, 24
             TEMP_PRICE = HOURLY_LAST_PRICE(R_TRANS_GROUP,MARKET_HR) * &
                                                   MARKET_PRICE_SCALAR
             MONTH_SUM_USER_W_PRICES(USER_MONTH,R_TRANS_GROUP) = &
                   MONTH_SUM_USER_W_PRICES(USER_MONTH,R_TRANS_GROUP) + &
                                                            TEMP_PRICE 
             MONTH_MAX_USER_W_PRICE(USER_MONTH,R_TRANS_GROUP) = &
                MAX(MONTH_MAX_USER_W_PRICE(USER_MONTH,R_TRANS_GROUP), &
                                                           TEMP_PRICE)
             MONTH_MIN_USER_W_PRICE(USER_MONTH,R_TRANS_GROUP) = &
                MIN(MONTH_MIN_USER_W_PRICE(USER_MONTH,R_TRANS_GROUP), &
                                                           TEMP_PRICE)
               USER_MARKET_PRICES(USER_HR,R_TRANS_GROUP) = TEMP_PRICE 
               USER_HR = USER_HR + 1

            ENDDO
         ENDIF


         IF(USER_HR == USER_MONTH_STARTING_HOUR + USER_HOURS) THEN
!
! TEMPORARY OUT TO CREATE THE MRX PRICE CURVE. 9/14/00. GAT.
!

            IF(MRX_ACTIVE .OR. YES_MARKET_DURATION) &
             CALL USER_MARKET_PRICE_PROB( &
                  R_TRANS_GROUP, &
                  USER_MONTH_STARTING_HOUR, &
                  USER_HOURS, &
                  USER_MARKET_PRICES( &
                                       USER_MONTH_STARTING_HOUR, &
                                                   R_TRANS_GROUP), & 
                MONTH_SUM_USER_W_PRICES(USER_MONTH,R_TRANS_GROUP), & 
                                                          ! REAL(kind=8)
                 MONTH_MAX_USER_W_PRICE(USER_MONTH,R_TRANS_GROUP), & 
                 MONTH_MIN_USER_W_PRICE(USER_MONTH,R_TRANS_GROUP), &
                  USER_MONTH, &
                  MULTI_AREA_NAME(R_TRANS_GROUP))
!
            SUM_USER_W_PRICES(R_TRANS_GROUP) = &
                   SUM_USER_W_PRICES(R_TRANS_GROUP) + & 
                     MONTH_SUM_USER_W_PRICES(USER_MONTH,R_TRANS_GROUP)
          MAX_USER_W_PRICE(R_TRANS_GROUP) = &
                  MAX(MAX_USER_W_PRICE(R_TRANS_GROUP), &
                     MONTH_MAX_USER_W_PRICE(USER_MONTH,R_TRANS_GROUP))
          MIN_USER_W_PRICE(R_TRANS_GROUP) = &
                  MIN(MIN_USER_W_PRICE(R_TRANS_GROUP), &
                     MONTH_MIN_USER_W_PRICE(USER_MONTH,R_TRANS_GROUP))
         ENDIF
!
!            ENDDO ! MONTHS
         IF(R_MONTH == LAST_MONTHLY_TRANSACT .AND. R_DAY == 31) THEN
!         
            IF(R_CREATE_HOURLY_PRICE) THEN
               IF(LAHEY_LF95()) THEN
                WRITE(LOCAL_UNIT_NO,REC=368) &
                   (MONTH_MAX_USER_W_PRICE(MONTH_COUNT,R_TRANS_GROUP), &
                                    MONTH_COUNT=1,12), &
                                       MAX_USER_W_PRICE(R_TRANS_GROUP)
                WRITE(LOCAL_UNIT_NO,REC=369) &
                   (MONTH_MIN_USER_W_PRICE(MONTH_COUNT,R_TRANS_GROUP), &
                                    MONTH_COUNT=1,12), &
                                       MIN_USER_W_PRICE(R_TRANS_GROUP)
                WRITE(LOCAL_UNIT_NO,REC=370) &
                   (SNGL(MONTH_SUM_USER_W_PRICES( &
                                          MONTH_COUNT,R_TRANS_GROUP)), &
                            MONTH_COUNT=1,12), &
                               SNGL(SUM_USER_W_PRICES(R_TRANS_GROUP))
             ELSE
                WRITE(LOCAL_UNIT_NO,REC=367) &
                   (MONTH_MAX_USER_W_PRICE(MONTH_COUNT,R_TRANS_GROUP), &
                                    MONTH_COUNT=1,12), &
                                       MAX_USER_W_PRICE(R_TRANS_GROUP)
                WRITE(LOCAL_UNIT_NO,REC=368) &
                   (MONTH_MIN_USER_W_PRICE(MONTH_COUNT,R_TRANS_GROUP), &
                                    MONTH_COUNT=1,12), &
                                       MIN_USER_W_PRICE(R_TRANS_GROUP)
                WRITE(LOCAL_UNIT_NO,REC=369) &
                   (SNGL(MONTH_SUM_USER_W_PRICES( &
                                          MONTH_COUNT,R_TRANS_GROUP)), &
                            MONTH_COUNT=1,12), &
                               SNGL(SUM_USER_W_PRICES(R_TRANS_GROUP))
               ENDIF
!         
               call close_unit(LOCAL_UNIT_NO)
            ENDIF
!
!            ANNUAL_USER_MONTH = 13 ! FOR ANNUAL VALUE
!
            IF(MRX_ACTIVE .OR. YES_MARKET_DURATION) THEN
               CALL USER_MARKET_PRICE_PROB( &
                      R_TRANS_GROUP, &
                      USER_ANNUAL_STARTING_HOUR, &
                      ANNUAL_TOTAL_HOURS, &
                      USER_MARKET_PRICES( &
                            USER_ANNUAL_STARTING_HOUR, &
                                        R_TRANS_GROUP), &
                      SUM_USER_W_PRICES(R_TRANS_GROUP), & ! REAL(kind=8)
                      MAX_USER_W_PRICE(R_TRANS_GROUP), &
                      MIN_USER_W_PRICE(R_TRANS_GROUP), &
                      ANNUAL_USER_MONTH, &
                      MULTI_AREA_NAME(R_TRANS_GROUP))
            ENDIF
         ENDIF
!
!         ELSE
!            ANNUAL_MARKET_PRICING = .FALSE.
!         ENDIF
!
!
!         
!
      RETURN

      ENTRY INIT_USER_MARKET_PRICES

         IF(ALLOCATED(ZERO_MARKET_PRICES)) THEN
            ZERO_MARKET_PRICES = 0.0
         ELSE
            I = I
         ENDIF
      RETURN

      ENTRY WRITE_USER_TIE_FLOW_DATA(R_MONTH,R_DAY,R_DAY_WEEK, &
                                   R_TRANS_GROUP, &
                                   R_MULTI_AREA_NAME, &
                                   R_CREATE_HOURLY_PRICE)

!
         LOCAL_UNIT_NO = 4455 + R_TRANS_GROUP
         IF(R_MONTH == FIRST_MONTHLY_TRANSACT .AND. R_DAY == 1) THEN
!               
            IF(R_CREATE_HOURLY_PRICE) THEN
!
! 092706. TEST WITH NEW TEMP_I AND NO YES_TWO_PRB_DIGITS.
! 092806. JUST CHANGED TEMP_I BACK.
! 092806. HARD-WIRED FROM LOAD_FILE_CHAR_EXT TO SCEN_FILE_CHAR_EXT 
!         (STILL NOT USING YES_TWO_PRB_DIGITS) 
! 092806. ADDED TEMP_MULTI_AREA_NAME
! 092906. SEEMS TO BE RELATED TO THE MARKET_PRICE_NAME WHEN 
!         IT IS THREE CHARACTERS.
! 093006. CONFIRMED THAT THE TWO DIGIT VERSION WORKS.
!
               TEMP_I = GET_PRB_SEQUENCE_NUMBER() + gc_end_point - 1
               TEMP_MULTI_AREA_NAME = R_MULTI_AREA_NAME
!               TEMP_I = gc_end_point
!
               IF(YES_TWO_PRB_DIGITS() .AND. TEMP_I < 99) THEN
                  MARKET_PRICE_NAME = TRIM(TEMP_MULTI_AREA_NAME)// &
                                             LOAD_FILE_CHAR_EXT(TEMP_I)
               ELSE

                  MARKET_PRICE_NAME = &
                        TRIM(R_MULTI_AREA_NAME(1:2))// &
                                             SCEN_FILE_CHAR_EXT(TEMP_I)
               ENDIF
!
!              CHECK FOR UNIQUE NAME FOR EACH GROUP?
!
               FILE_NAME = trim(LDE_FILE_DIRECTORY())//"LDE"// &
                                      trim(MARKET_PRICE_NAME)//".B"// &
                        LOAD_FILE_CHAR_EXT(GET_MARKET_PRICE_YEAR(globecom_year))
!
! 100206. 
!
               DO TEMP_I = 1, 256
                  IF(MARKET_PRICE_NAME == &
                                   SAVE_TIE_FLOW_NAME(TEMP_I)) THEN 
                     WRITE(4,*) 'REDUNDANT MARKET PRICE NAMES'
                     WRITE(4,*) 'MARKET NAME = ',MARKET_PRICE_NAME
                     WRITE(4,*) 'PLEASE RENAME IN THE TRANSACTION'
                     WRITE(4,*) 'GROUPS FILE'
                     er_message='Stop requested from transobj SIID306'
                     call end_program(er_message)
                  ELSEIF(SAVE_TIE_FLOW_NAME(TEMP_I) == 'NONE ') THEN
                     SAVE_TIE_FLOW_NAME(TEMP_I) = MARKET_PRICE_NAME
                     EXIT
                  ENDIF
               END DO
               INQUIRE(UNIT=LOCAL_UNIT_NO,OPENED=PRICE_FILE_OPEN)
               IF(PRICE_FILE_OPEN) then
                call close_unit(LOCAL_UNIT_NO)
               end if
!
! 110206. THE FILE DID NOT SEEM TO PROPERLY ERASE PER DOUG E-MAIL. 
!         SO REPLACE IF EXISTS
!
               INQUIRE(FILE=FILE_NAME,EXIST=PRICE_FILE_EXISTS)
               IF(PRICE_FILE_EXISTS) THEN ! CALL ERASE(FILE_NAME)
                  OPEN(UNIT=LOCAL_UNIT_NO, &
                    FILE=FILE_NAME,FORM='UNFORMATTED',RECL=118, &
                                     ACCESS="DIRECT",STATUS="REPLACE")
               ELSE
                  OPEN(UNIT=LOCAL_UNIT_NO, &
                    FILE=FILE_NAME,FORM='UNFORMATTED',RECL=118, &
                                     ACCESS="DIRECT",STATUS="NEW")
               ENDIF
! 07/07/03.
               IF(LAHEY_LF95()) THEN
                  WRITE(LOCAL_UNIT_NO,REC=1) F7,INT(118,2)
               ENDIF
            ENDIF
!
         ENDIF
! 11/1/99.
!         
         IF(R_CREATE_HOURLY_PRICE) THEN
            USER_IREC = ALINE_LOAD_DATA(USER_I,R_MONTH) + R_DAY - 1
!
            DO MARKET_HR = 1, 24 
               LOCAL_TIE_FLOW(MARKET_HR) = &
                                 INT(TIE_FLOW(R_TRANS_GROUP,MARKET_HR))
            END DO
! 
            WRITE(LOCAL_UNIT_NO,REC=USER_IREC) &  
                              R_MONTH,R_DAY,USER_YEAR, &
                              EEICODE,R_DAY_WEEK, &
                              TIMZON,TEMPER, &
                              DELTMP, &
                              LOCAL_TIE_FLOW
            USER_IREC = USER_IREC + 1
!                  
! ALWAYS ADD LEAP YEAR INTO LDE FILE
! REUSE FEB 28 
!
            IF(R_DAY == 28 .AND. R_MONTH == 2) THEN
               WRITE(LOCAL_UNIT_NO,REC=USER_IREC) &
                               R_MONTH,INT(29,2),USER_YEAR, &
                               EEICODE,R_DAY_WEEK, &
                               TIMZON,TEMPER, &
                               DELTMP, &
                               LOCAL_TIE_FLOW
               USER_IREC = USER_IREC + 1
            ENDIF
         ENDIF
!        
!                  
! ALWAYS ADD LEAP YEAR INTO LDE FILE
!
         IF(R_DAY == 28 .AND. R_MONTH == 2) THEN
            IF(R_CREATE_HOURLY_PRICE) THEN
               WRITE(LOCAL_UNIT_NO,REC=USER_IREC) & 
                                   R_MONTH,INT(29,2),USER_YEAR, &
                                   EEICODE,R_DAY_WEEK, &
                                   TIMZON,TEMPER, &
                                   DELTMP, &
            (HOURLY_LAST_PRICE(R_TRANS_GROUP,MARKET_HR),MARKET_HR=1,24)
               USER_IREC = USER_IREC + 1
            ENDIF
         ENDIF


         IF(R_MONTH == LAST_MONTHLY_TRANSACT .AND. R_DAY == 31) THEN
!         
            IF(R_CREATE_HOURLY_PRICE) THEN
               IF(LAHEY_LF95()) THEN
                  WRITE(LOCAL_UNIT_NO,REC=368) TIE_FLOW_DATA_MAX
                  WRITE(LOCAL_UNIT_NO,REC=369) TIE_FLOW_DATA_MIN
                  WRITE(LOCAL_UNIT_NO,REC=370) TIE_FLOW_DATA_SUM
               ELSE
                  WRITE(LOCAL_UNIT_NO,REC=367) TIE_FLOW_DATA_MAX
                  WRITE(LOCAL_UNIT_NO,REC=368) TIE_FLOW_DATA_MIN
                  WRITE(LOCAL_UNIT_NO,REC=369) TIE_FLOW_DATA_SUM
               ENDIF
!         
               call close_unit(LOCAL_UNIT_NO)
!               
            ENDIF
!
         ENDIF
!         
!
      RETURN

      ENTRY READ_USER_MARKET_DATA(R_HOUR, &
                                   R_MONTH, &
                                   R_DAY, &
                                   R_TRANS_GROUP, &
                                   R_MULTI_AREA_NAME)

!
         LOCAL_UNIT_NO = 2813 + R_TRANS_GROUP
!         
         IF(R_MONTH == FIRST_MONTHLY_TRANSACT .AND. R_DAY == 1) THEN
!
! 12/15/04.
!
            IF(ALLOCATE_PRICE_ARRAYS) THEN
!
               ALLOCATE_PRICE_ARRAYS = .FALSE.
!       
               IF(ALLOCATED(USER_MARKET_PRICES)) &
                                      DEALLOCATE(USER_MARKET_PRICES, &
                                           MAX_USER_W_PRICE, &
                                           MIN_USER_W_PRICE, &
                                           MONTH_MAX_USER_W_PRICE, &
                                           MONTH_MIN_USER_W_PRICE, &
                                           SUM_USER_W_PRICES, &
                                           MONTH_SUM_USER_W_PRICES)
               ALLOCATE( &
                    USER_MARKET_PRICES(ANNUAL_HOURS,UPPER_TRANS_GROUP))
               ALLOCATE(MAX_USER_W_PRICE(UPPER_TRANS_GROUP))
               ALLOCATE(MIN_USER_W_PRICE(UPPER_TRANS_GROUP))
               ALLOCATE(MONTH_MAX_USER_W_PRICE(12,UPPER_TRANS_GROUP))
               ALLOCATE(MONTH_MIN_USER_W_PRICE(12,UPPER_TRANS_GROUP))
               ALLOCATE(SUM_USER_W_PRICES(UPPER_TRANS_GROUP))
               ALLOCATE(MONTH_SUM_USER_W_PRICES(12,UPPER_TRANS_GROUP))
               USER_MARKET_PRICES = 0.
!
               DO USER_MONTH = 1, 12
                  DO LOCAL_TRANS_GROUP = 1, UPPER_TRANS_GROUP
                    MONTH_SUM_USER_W_PRICES( &
                                     USER_MONTH,LOCAL_TRANS_GROUP) = 0.
                    MONTH_MAX_USER_W_PRICE( &
                                     USER_MONTH,LOCAL_TRANS_GROUP) = 0.
                    MONTH_MIN_USER_W_PRICE( &
                                     USER_MONTH,LOCAL_TRANS_GROUP) = &
                                                                999999.
                  ENDDO
               ENDDO
!               
            ENDIF
! 082806. FOR BURESH.
! 090806. ADDED REGIONAL_AREA_PRICE
!
!
            IF(REGIONAL_AREA_PRICE) THEN
               CALL GET_NEW_MARKET_PRICE_NAME(MARKET_PRICE_NAME)
               IF(YES_TWO_PRB_DIGITS()) THEN
                  MARKET_PRICE_NAME = trim(R_MULTI_AREA_NAME)// &
                                                 MARKET_PRICE_NAME(3:4)
               ELSE
                  MARKET_PRICE_NAME = trim(R_MULTI_AREA_NAME(1:2))// &
                                                 MARKET_PRICE_NAME(3:5)
               ENDIF
            ELSE
               TEMP_I = GET_PRB_SEQUENCE_NUMBER() + gc_end_point - 1
               IF(YES_TWO_PRB_DIGITS() .AND. TEMP_I < 99) THEN
                  MARKET_PRICE_NAME = trim(R_MULTI_AREA_NAME)// &
                                             LOAD_FILE_CHAR_EXT(TEMP_I)
               ELSE
                  MARKET_PRICE_NAME = trim(R_MULTI_AREA_NAME(1:2))// &
                                             SCEN_FILE_CHAR_EXT(TEMP_I)
               ENDIF
            ENDIF
!               

!
! CHECK FOR UNIQUE NAME FOR EACH GROUP?
!
            FILE_NAME = trim(PRB_FILE_DIRECTORY())//"PRB"// &
                                      TRIM(MARKET_PRICE_NAME)//".P"// &
                        LOAD_FILE_CHAR_EXT(GET_MARKET_PRICE_YEAR(globecom_year))
            INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
            IF(.NOT. FILE_EXISTS) THEN 
!            
! FLAG THAT THERE ARE NO PRICES FOR THIS SERIES.
!
!
! 11/25/02. TEST IF THERE ARE FIRST SCENARIO PRICES
!
! 090806. FIRST ATTEMPT AT MARKET PRICES FAILED.
!
               WRITE(4,*) "---START ERROR 2642---"
               WRITE(4,*) "THERE IS NO MARKET PRICE FILE NAMED", &
                                                              FILE_NAME
               WRITE(4,*) "RUNNING MARKET PRICE MODE FOR AREA", &
                                                      R_MULTI_AREA_NAME
               WRITE(4,*) "CHECK FOR EXISTENCE OF THE PRB FILE"
               WRITE(4,*) "IN YOUR MARKET PRICE DIRECTORY."
               WRITE(4,*) "---END ERROR 2642---"
               er_message='See WARNING MESSAGES -transobj.for-3'
               call end_program(er_message)
               
               WRITE(4,*) "TRANSACT WILL ATTEMPT"// &
                                                "TO CONTINUE EXECUTION"
               MARKET_PRICE_NAME = trim(R_MULTI_AREA_NAME)// &
                                           LOAD_FILE_CHAR_EXT(INT(1,2))
               FILE_NAME = trim(PRB_FILE_DIRECTORY())//"PRB"// &
                                      trim(MARKET_PRICE_NAME)//".P"// &
                        LOAD_FILE_CHAR_EXT(GET_MARKET_PRICE_YEAR(globecom_year))
               INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
!               
               IF(.NOT. FILE_EXISTS) THEN 
                  WRITE(4,*) "THERE IS NO MARKET PRICE FILE NAMED", &
                                                              FILE_NAME
                  WRITE(4,*) "RUNNING MARKET PRICE MODE FOR AREA", &
                                                      R_MULTI_AREA_NAME
                  WRITE(4,*) "TRANSACT WILL ATTEMPT"// &
                                                "TO CONTINUE EXECUTION"
                  WRITE(4,*) "WITH NULL PRICES"
                  DO LOCAL_HR = R_HOUR, R_HOUR + 23
                     MARKET_HR = LOCAL_HR - R_HOUR + 1
                     HOURLY_IN_PRICE(R_TRANS_GROUP,MARKET_HR) = &
                                      MONTHLY_MARKET_PRICES(LOCAL_HR)
                  ENDDO
                  RETURN
               ELSEIF(R_DAY == 1) THEN
                  WRITE(4,*) "RUNNING IN PRICE MODE WITH INSUFFICIENT"
                  WRITE(4,*) "PRICING INFORMATION. REVERTING BACK TO "
                  WRITE(4,*) "FIRST ENDPOINT PRICES"
               ENDIF
            ENDIF

            OPEN(UNIT=LOCAL_UNIT_NO, &
                    FILE=FILE_NAME,FORM='UNFORMATTED',RECL=118, &
                                     ACCESS="DIRECT",STATUS="UNKNOWN")

            USER_HR = 1
!
! 12/15/04.
!            
            SUM_USER_W_PRICES(R_TRANS_GROUP) = 0.
            MAX_USER_W_PRICE(R_TRANS_GROUP) = 0.
            MIN_USER_W_PRICE(R_TRANS_GROUP) = 999999.
            SUM_USER_PRICES = 0.
!            
            MAX_USER_PRICE = 0.
            MIN_USER_PRICE = 999999.
            ANNUAL_STARTING_HOUR = 1
            ANNUAL_TOTAL_HOURS = 8784
            IF(CURRENT_YEAR > 1999) THEN
               USER_YEAR = CURRENT_YEAR - 2000
            ELSE
               USER_YEAR = CURRENT_YEAR - 1900
            ENDIF
! 12/16/04.
            IF(R_TRANS_GROUP == 1) THEN
               CALL INIT_USER_MARKET_PRICE_PROB(UPPER_TRANS_GROUP)
            ENDIF
!            
         ENDIF
!

         IF(R_DAY == 1) THEN
            USER_MONTH = R_MONTH
!
            MONTH_STARTING_HOUR = USER_HR

            MONTH_SUM_USER_PRICES(USER_MONTH) = 0.
            MONTH_MAX_USER_PRICE(USER_MONTH) = 0.
            MONTH_MIN_USER_PRICE(USER_MONTH) = 999999.
!
! 11/19/04. FOR MULTI-AREA MRX.
! 12/15/04. 
!
            IF(R_TRANS_GROUP == 1) THEN
               USER_MONTH_STARTING_HOUR = USER_HR

            ENDIF
!
            CALL GET_CUM_HOURS_N_HOURS_IN_MONTH(USER_MONTH, &
                                               USER_HOURS, &
                                               USER_CUM_HOURS)
            DAYS_IN_MONTH = USER_HOURS/24
         ENDIF
!
! 11/1/99.
!         
         USER_IREC = ALINE_LOAD_DATA(USER_I,R_MONTH) + R_DAY - 1
!            
         READ(LOCAL_UNIT_NO,REC=USER_IREC) &
                              LOCAL_MONTH,LOCAL_DAY,USER_YEAR, &
                              EEICODE,LOCAL_DAY_WEEK, &
                              TIMZON,TEMPER, &
                              DELTMP, &
                              TEMP_DAILY_PRICE ! 24 ELEMENT ARRAY
         USER_IREC = USER_IREC + 1
         IF(globecom_year > AVAIL_DATA_YEARS) THEN
            CALL MULTI_AREA_EXT_PERIOD_PRICE_ESC(globecom_year, &
                                        R_TRANS_GROUP,TEMP_DAILY_PRICE)
         ENDIF
         HOURLY_IN_PRICE(R_TRANS_GROUP,:) = TEMP_DAILY_PRICE(:)
!
! 12/15/04.
!
         USER_HR = (R_DAY-1) * 24 + USER_MONTH_STARTING_HOUR
!                  


         DO MARKET_HR = 1, 24
            TEMP_PRICE = HOURLY_IN_PRICE(R_TRANS_GROUP,MARKET_HR) * &
                                                    MARKET_PRICE_SCALAR
            MONTH_SUM_USER_PRICES(USER_MONTH) = &
                          MONTH_SUM_USER_PRICES(USER_MONTH) + & 
                                                             TEMP_PRICE
            MONTH_MAX_USER_PRICE(USER_MONTH) = &
                               MAX(MONTH_MAX_USER_PRICE(USER_MONTH), &
                                                           TEMP_PRICE)
            MONTH_MIN_USER_PRICE(USER_MONTH) = &
                               MIN(MONTH_MIN_USER_PRICE(USER_MONTH), &
                                                           TEMP_PRICE)
!
! 12/15/04.
!
            MONTH_SUM_USER_W_PRICES(USER_MONTH,R_TRANS_GROUP) = &
                   MONTH_SUM_USER_W_PRICES(USER_MONTH,R_TRANS_GROUP) + &
                                                            TEMP_PRICE 
          MONTH_MAX_USER_W_PRICE(USER_MONTH,R_TRANS_GROUP) = &
                 MAX(MONTH_MAX_USER_W_PRICE(USER_MONTH,R_TRANS_GROUP), &
                                                          TEMP_PRICE )
          MONTH_MIN_USER_W_PRICE(USER_MONTH,R_TRANS_GROUP) = &
                 MIN(MONTH_MIN_USER_W_PRICE(USER_MONTH,R_TRANS_GROUP), &
                                                          TEMP_PRICE )
            USER_MARKET_PRICES(USER_HR,R_TRANS_GROUP) = TEMP_PRICE 

            USER_HR = USER_HR + 1

         ENDDO ! HOURS
!                  
! ALWAYS ADD LEAP YEAR INTO LDE FILE
!
         IF(R_DAY == 28 .AND. R_MONTH == 2) THEN
            READ(LOCAL_UNIT_NO,REC=USER_IREC) &
                                  LOCAL_MONTH,LOCAL_DAY,USER_YEAR, &
                                  EEICODE,LOCAL_DAY_WEEK, &
                                  TIMZON,TEMPER, &
                                  DELTMP, &
                                  TEMP_DAILY_PRICE ! 24 ELEMENT ARRAY
            USER_IREC = USER_IREC + 1
            IF(globecom_year > AVAIL_DATA_YEARS) THEN
               CALL MULTI_AREA_EXT_PERIOD_PRICE_ESC(globecom_year, &
                                        R_TRANS_GROUP,TEMP_DAILY_PRICE)
            ENDIF
            DO MARKET_HR = 1, 24

               TEMP_PRICE = TEMP_DAILY_PRICE(MARKET_HR) * &
                                                    MARKET_PRICE_SCALAR
               MONTH_SUM_USER_PRICES(USER_MONTH) = &
                             MONTH_SUM_USER_PRICES(USER_MONTH) + &
                                                             TEMP_PRICE 
               MONTH_MAX_USER_PRICE(USER_MONTH) = &
                               MAX(MONTH_MAX_USER_PRICE(USER_MONTH), &
                                                           TEMP_PRICE)
               MONTH_MIN_USER_PRICE(USER_MONTH) = &
                               MIN(MONTH_MIN_USER_PRICE(USER_MONTH), &
                                                           TEMP_PRICE)
!
! 12/15/04.
!
               MONTH_SUM_USER_W_PRICES(USER_MONTH,R_TRANS_GROUP) = &
                   MONTH_SUM_USER_W_PRICES(USER_MONTH,R_TRANS_GROUP) + &
                                                            TEMP_PRICE 
             MONTH_MAX_USER_W_PRICE(USER_MONTH,R_TRANS_GROUP) = &
                 MAX(MONTH_MAX_USER_W_PRICE(USER_MONTH,R_TRANS_GROUP), &
                                                          TEMP_PRICE )
             MONTH_MIN_USER_W_PRICE(USER_MONTH,R_TRANS_GROUP) = &
                 MIN(MONTH_MIN_USER_W_PRICE(USER_MONTH,R_TRANS_GROUP), &
                                                          TEMP_PRICE )
               USER_MARKET_PRICES(USER_HR,R_TRANS_GROUP) = TEMP_PRICE 

               USER_HR = USER_HR + 1

            ENDDO
         ENDIF
!               ENDDO ! DAYS
!
         IF(USER_HR == USER_MONTH_STARTING_HOUR + USER_HOURS) THEN
            IF(MRX_ACTIVE .OR. YES_MARKET_DURATION) &
            CALL USER_MARKET_PRICE_PROB( &
                    R_TRANS_GROUP, &
                    USER_MONTH_STARTING_HOUR, &
                    USER_HOURS, &
                    USER_MARKET_PRICES( &
                                           USER_MONTH_STARTING_HOUR, &
                                                     R_TRANS_GROUP), & 
                  MONTH_SUM_USER_W_PRICES(USER_MONTH,R_TRANS_GROUP), & 
                                                         ! REAL(kind=8)
                  MONTH_MAX_USER_W_PRICE(USER_MONTH,R_TRANS_GROUP), & 
                  MONTH_MIN_USER_W_PRICE(USER_MONTH,R_TRANS_GROUP), &
                  USER_MONTH, &
                  MULTI_AREA_NAME(R_TRANS_GROUP))
!
           SUM_USER_PRICES = SUM_USER_PRICES +  &
                                   MONTH_SUM_USER_PRICES(USER_MONTH)
           MAX_USER_PRICE = &
                           MAX(MAX_USER_PRICE, &
                                   MONTH_MAX_USER_PRICE(USER_MONTH))
           MIN_USER_PRICE = &
                           MIN(MIN_USER_PRICE, &
                                   MONTH_MIN_USER_PRICE(USER_MONTH))
!
           SUM_USER_W_PRICES(R_TRANS_GROUP) = &
                    SUM_USER_W_PRICES(R_TRANS_GROUP) + &
                      MONTH_SUM_USER_W_PRICES(USER_MONTH,R_TRANS_GROUP)
           MAX_USER_W_PRICE(R_TRANS_GROUP) = &
                   MAX(MAX_USER_W_PRICE(R_TRANS_GROUP), &
                      MONTH_MAX_USER_W_PRICE(USER_MONTH,R_TRANS_GROUP))
           MIN_USER_W_PRICE(R_TRANS_GROUP) = &
                   MIN(MIN_USER_W_PRICE(R_TRANS_GROUP), &
                      MONTH_MIN_USER_W_PRICE(USER_MONTH,R_TRANS_GROUP))
!
         ENDIF
!

         IF(R_MONTH == LAST_MONTHLY_TRANSACT .AND. R_DAY == 31) THEN
!         
!

!         
            call close_unit(LOCAL_UNIT_NO)
!

! 12/16/04.
! 12/17/04.
!
            IF(MRX_ACTIVE .OR. YES_MARKET_DURATION) THEN

               CALL USER_MARKET_PRICE_PROB( &
                      R_TRANS_GROUP, &
                      ANNUAL_STARTING_HOUR, &
                      ANNUAL_TOTAL_HOURS, &
                      USER_MARKET_PRICES( &
                                  ANNUAL_STARTING_HOUR, &
                                        R_TRANS_GROUP), &
                      SUM_USER_W_PRICES(R_TRANS_GROUP), & ! REAL(kind=8)
                      MAX_USER_W_PRICE(R_TRANS_GROUP), &
                      MIN_USER_W_PRICE(R_TRANS_GROUP), &
                      ANNUAL_USER_MONTH, & ! SEEMS TO BE THE PROBLEM
                      MULTI_AREA_NAME(R_TRANS_GROUP))
            ENDIF
!     

         ENDIF
!

!
!
!         
!
      RETURN

      ENTRY ZERO_ITER_PRICE()


         IF(ALLOCATED(ZERO_MARKET_PRICES)) THEN

               ZERO_MARKET_PRICES = USER_MARKET_PRICES

         ENDIF
      RETURN

      ENTRY CX_TRANS_ANNUAL_USER_MARKET(R_TRANS,R_ANNUAL_PRICES)
         if(file_trace_aum==0) then
          file_trace_aum=open_trace("cx_trans_annual_user_market.trace", &
            rq_aum)
         end if
         call write_trace_int2(file_trace_aum, "R_TRANS arg", R_TRANS)
         call write_trace_real4s_vert(file_trace_aum, "annual prices", &
            R_ANNUAL_PRICES)
            
         TG = R_TRANS 
         call write_trace_int2(file_trace_aum, "TG became", TG)
         

!
! 092221. 
!
         IF(TRANSACT_C) THEN
            IF(ALLOCATED(ANNUAL_MARKET_PRICES) .AND. TG > 0) THEN
               M = 0
               DO I_HR = 1, 12
                  CALL GET_CUM_HOURS_N_HOURS_IN_MONTH(I_HR,K,L)
                  SCENARIO_ELECTRIC_MULT = &
                            GET_SCENARIO_ELECTRIC_PRICE(globecom_year,I_HR)
                  DO J_HR = 1, K 
                     M = M + 1
                     R_ANNUAL_PRICES(M) = ANNUAL_MARKET_PRICES(M)* &
                                              SCENARIO_ELECTRIC_MULT * &
                      GET_HOURLY_SCEN_ELECT_MULT(J_HR,I_HR) * &
                      GET_HRLY_TG_SCEN_ELECT_MULT(J_HR,I_HR,R_TRANS)
                  ENDDO 
               ENDDO
            ELSE
               R_ANNUAL_PRICES = 0.0
            ENDIF
         ELSE
            IF(ALLOCATED(USER_MARKET_PRICES) .AND. TG > 0) THEN 

               R_ANNUAL_PRICES(1:8760) = ZERO_MARKET_PRICES(1:8760,TG)
            ELSE
               R_ANNUAL_PRICES = 0.0
            ENDIF
         ENDIF
      RETURN
      
      entry alloc_2_for_marketprice_trans_r
            IF(ALLOCATED(ANNUAL_UNSERVED_ENERGY_COST)) &
                            DEALLOCATE(ANNUAL_UNSERVED_ENERGY_COST, &
                                       ANNUAL_COST_ABOVE_RESOURCES, &
                                           ANNUAL_UNSERVED_ENERGY, &
                                           ANNUAL_ABOVE_RESOURCES)
            ALLOCATE(M_ANNUAL_PURCHASE_ENERGY(UPPER_TRANS_GROUP))
            ALLOCATE(M_ANNUAL_PURCHASE_COSTS(UPPER_TRANS_GROUP))
            ALLOCATE(M_ANNUAL_SALES_ENERGY(UPPER_TRANS_GROUP))
            ALLOCATE(M_ANNUAL_SALES_REVENUES(UPPER_TRANS_GROUP))
            ALLOCATE(M_ANNUAL_NATIVE_COST(UPPER_TRANS_GROUP))
            ALLOCATE(M_ANNUAL_LOAD_B4_SALES(UPPER_TRANS_GROUP))
            ALLOCATE(M_ANNUAL_PRO_COST_B4_SALES(UPPER_TRANS_GROUP))
            ALLOCATE(M_ANNUAL_LOAD_AFTER_SALES(UPPER_TRANS_GROUP))
            ALLOCATE(M_ANNUAL_PRO_COST_AFTER_SALES(UPPER_TRANS_GROUP))
            ALLOCATE(ANNUAL_TL_MWH(UPPER_TRANS_GROUP))
            ALLOCATE(ANNUAL_TL_PEAK(UPPER_TRANS_GROUP))
            ALLOCATE(ANNUAL_COIN_PEAK(0:UPPER_TRANS_GROUP))
            ALLOCATE(ANNUAL_TL_BASE(UPPER_TRANS_GROUP))
            ALLOCATE(ANNUAL_TL_HYDRO_MWH(UPPER_TRANS_GROUP))
            ALLOCATE(ANNUAL_TL_HYDRO_MW(UPPER_TRANS_GROUP))
            ALLOCATE(ANNUAL_TL_HYDRO_ROR(UPPER_TRANS_GROUP))
            ALLOCATE(ANNUAL_SPINNING_MWH(UPPER_TRANS_GROUP))
            ALLOCATE(ANNUAL_EFFECTIVE_CAPACITY(UPPER_TRANS_GROUP))
            ALLOCATE(ANNUAL_UNSERVED_ENERGY(UPPER_TRANS_GROUP,0:12))
            ALLOCATE(ANNUAL_ABOVE_RESOURCES(UPPER_TRANS_GROUP,0:12))
            ALLOCATE(ANNUAL_UNSERVED_ENERGY_COST(UPPER_TRANS_GROUP))
            ALLOCATE(ANNUAL_COST_ABOVE_RESOURCES(UPPER_TRANS_GROUP, &
                                                                 0:12))
!
            ALLOCATE(ANNUAL_PRODUCT_PRICE(0:UPPER_TRANS_GROUP, &
                                                         NUM_PRODUCTS))
            ALLOCATE(ANNUAL_PRODUCT_QUANTITY(0:UPPER_TRANS_GROUP, &
                                                         NUM_PRODUCTS))
            ALLOCATE(ANNUAL_PRODUCT_HEATRATE(0:UPPER_TRANS_GROUP, &
                                                         NUM_PRODUCTS))
            ALLOCATE(ANNUAL_PRODUCT_MARGINAL_FUEL(0:UPPER_TRANS_GROUP, &
                                                         NUM_PRODUCTS))
            ALLOCATE(ANNUAL_PRODUCT_FUEL_PRICE(0:UPPER_TRANS_GROUP, &
                                                         NUM_PRODUCTS))
            ALLOCATE(ANNUAL_PRODUCT_HOURS(NUM_PRODUCTS))


      ENTRY READ_LOAD_DATA(R_HOURS_IN_MONTH,R_MONTH)

!
!
! THIS SHOULD REALLY BE MOVED INTO CAL_AFTER_EL_LOAD.
! ALSO, THERE SHOULD BE A SWITCH THAT ALLOWS THE FIRST TRANSACTION 
! GROUP TO BE THE NATIVE UTILITY THAT ALREADY WENT THROUGH ALL THE
! DSM AND EL ADJUSTMENTS.  FOR NOW, I WILL ASSUME THAT MARKET 
! TRANSACTIONS WILL ALWAYS USE THE OTHER STUFF AND MULTI-AREA MODULE
! WILL USE THIS STUFF.

         if(R_HOURS_IN_MONTH>744) then
            call end_program( "transobj:0005 - HOURS_IN_MONTH value" // &
            " of " // trim(itos(int(R_HOURS_IN_MONTH))) // &
            " is invalid. It should be <=744.")
         end if
         DAYS_IN_MONTH = R_HOURS_IN_MONTH/24
         IF(ALLOCATED(SYSTEM_HOURLY_LOADS)) &
                            DEALLOCATE(SYSTEM_HOURLY_LOADS,DAY_OF_WEEK)
         IF(.NOT. USE_TF_FILE) THEN
            ALLOCATE(SYSTEM_HOURLY_LOADS( &
                                  R_HOURS_IN_MONTH,0:MAX_TRANS_LOADS), &
                                    DAY_OF_WEEK(31,0:MAX_TRANS_LOADS))
!
            SYSTEM_HOURLY_LOADS = 0.d0
!         
         ENDIF
!
!
!
         IF(USE_TF_FILE) THEN
            DO I = 1, MAX_TRANS_GROUP_NUMBER
               
               TG = GET_TRANS_GROUP_INDEX(I)

               TRANS_GROUP_LOAD_ACTIVE(I) = &
                  TF_FILE_LOAD_GROUP_ACTIVE(TG)
            ENDDO
         ELSEIF(.NOT. USE_TRANSACT_LOADS) THEN
        !   TODO: FLEN is not compatible. Should use SIZE=FILE_LENGHT
            INQUIRE(UNIT=1722,RECL=RECORD_LENGHT,FLEN=FILE_LENGHT)
            READ_I4_LOADS = RECORD_LENGHT > 80
            IREC = ALINE_LOAD_DATA(I,R_MONTH) 
!
            PERIOD_TRANSACTION_DEMAND = 0.D0
!
            DO DA = 1, DAYS_IN_MONTH
               IF(READ_I4_LOADS) THEN
                  READ(1722,REC=IREC) LDE_MONTH,LDE_DAY,LDE_YEAR, &
                                    EEICODE,DAY_OF_WEEK(DA,1), &
                                    TIMZON,TEMPER, &
                                    DELTMP,DAILY_LOADS_I4
               ELSE
                  READ(1722,REC=IREC) LDE_MONTH,LDE_DAY,LDE_YEAR, &
                                    EEICODE,DAY_OF_WEEK(DA,1), &
                                    TIMZON,TEMPER, &
                                    DELTMP,DAILY_LOADS_I2
               ENDIF
               IREC = IREC + 1
               DO HR = 1, 24
                  CURRENT_HR = CURRENT_HR + 1
                  IF(READ_I4_LOADS) THEN
                     SYSTEM_HOURLY_LOADS(CURRENT_HR,I) = &
                                                     DAILY_LOADS_I4(HR)
                  ELSE
                     SYSTEM_HOURLY_LOADS(CURRENT_HR,I) = &
                                                     DAILY_LOADS_I2(HR)
                  ENDIF
                  SYSTEM_HOURLY_LOADS(CURRENT_HR,0) = &
                                   SYSTEM_HOURLY_LOADS(CURRENT_HR,0) + &
                                     SYSTEM_HOURLY_LOADS(CURRENT_HR,I)
                  PERIOD_TRANSACTION_DEMAND = &
                             PERIOD_TRANSACTION_DEMAND + &
                                      SYSTEM_HOURLY_LOADS(CURRENT_HR,I)
               ENDDO
            ENDDO
         ELSE
!
            PERIOD_TRANSACTION_DEMAND = 0.D0
!
            DO I = 1, MAX_TRANS_GROUPS
               TRANS_GROUP_LOAD_ACTIVE(I) = .FALSE. ! USE CINIT?
            ENDDO
!            
            DO I = 1, MAX_TRANS_LOADS
!
!
               CURRENT_TRANS_GROUP = TRANS_GROUP_LOAD(I)
               IF(CURRENT_TRANS_GROUP <= 0) CYCLE
!
               TRANS_GROUP_LOAD_ACTIVE(I) = .TRUE.
!

               FILE_NAME = trim(OUTPUT_DIRECTORY())//"LDE"// &
                          trim(BSYRLOAD())//".B"// &
                          LOAD_FILE_CHAR_EXT(CURRENT_TRANS_GROUP)
               LOAD_UNIT = 2100 + CURRENT_TRANS_GROUP
!
               INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
               IF(FILE_EXISTS) THEN
                  OPEN(UNIT=LOAD_UNIT,FILE=FILE_NAME,RECL=118, &
                                          ACCESS="DIRECT",STATUS="OLD")
               ELSE
                  WRITE(4,*) '*** line 2285 TRANSOBJ.FOR ***'
                  WRITE(4,*) "For transact loads group number ",I
                  WRITE(4,*) "analysis could not find data value ", &
                                                    CURRENT_TRANS_GROUP 
                  WRITE(4,*) "in the project directory."
                  WRITE(4,*) "See Production Parameters file in year", &
                                                           CURRENT_YEAR
                  er_message='See WARNING MESSAGES -transobj.for-4'
                  call end_program(er_message)
               ENDIF
!
               CURRENT_HR = 0
               IREC = ALINE_LOAD_DATA(I,R_MONTH) 
!
               DO DA = 1, DAYS_IN_MONTH
                  READ(LOAD_UNIT,REC=IREC) LDE_MONTH,LDE_DAY,LDE_YEAR, &
                                    EEICODE,DAY_OF_WEEK(DA,I), &
                                    TIMZON,TEMPER, &
                                    DELTMP,DAILY_LOADS_I4
                  IREC = IREC + 1
                  DO HR = 1, 24
                     CURRENT_HR = CURRENT_HR + 1
                     SYSTEM_HOURLY_LOADS(CURRENT_HR,I) = &
                          DAILY_LOADS_I4(HR)
                     SYSTEM_HOURLY_LOADS(CURRENT_HR,0) = &
                                   SYSTEM_HOURLY_LOADS(CURRENT_HR,0) + &
                                     SYSTEM_HOURLY_LOADS(CURRENT_HR,I)
                     PERIOD_TRANSACTION_DEMAND = &
                            PERIOD_TRANSACTION_DEMAND + & 
                                     SYSTEM_HOURLY_LOADS(CURRENT_HR,I)
                  ENDDO
               ENDDO
!
               call close_unit(int(10,2))
!
!              MAY WANT TO RESORT HIGHEST TO LOWEST IN THE FUTURE
!
            ENDDO ! TRANS LOAD GROUPS
         ENDIF ! JUST MARKET
      RETURN
     ENTRY MARKET_PRICE_TRANSACTIONS(R_HOURS_IN_MONTH,R_MONTH, &
                                     TIE_LIMIT,SPREADS) 

         IF(.NOT. LAHEY_LF95()) then
             CALL MG_LOCATE_WRITE(15,9,'Market Price Transaction', &
                                 ALL_VERSIONS,0)
         end if

         IF(CENTRAL_DISPATCH_TRANSACT) THEN
            LOWER_TRANS_GROUP = 1   
            UPPER_TRANS_GROUP = GET_NUMBER_OF_ACTIVE_GROUPS()
            UPPER_TRANS_GROUP = GET_MAX_TRANS_ID_USED()
         ELSEIF(STRICT_MARKET_PRICE .OR. MULTI_AREA_PRICE .OR. &
             REGIONAL_AREA_PRICE .OR. ZERO_BUY .OR. ZERO_SELL) THEN
            LOWER_TRANS_GROUP = 1
            UPPER_TRANS_GROUP = GET_NUMBER_OF_ACTIVE_GROUPS()
         ELSE
            LOWER_TRANS_GROUP = 1
            UPPER_TRANS_GROUP = 1
         ENDIF
         TOTAL_BUYER_INDEX = UPPER_TRANS_GROUP + 1

         IF(TEST_DAILY_OPTION) THEN
            TEMP_L = TRANSACT_C_MONTHLY_INIT(R_HOURS_IN_MONTH)
         ENDIF
!
! 6/22/98. GAT. FROM MULTI-PARTY
! 
         ALLOCATE(M_MONTHLY_PRO_COST_AFTER_SALES(UPPER_TRANS_GROUP))
         ALLOCATE(M_MONTHLY_PRO_COST_B4_SALES(UPPER_TRANS_GROUP))
         ALLOCATE(M_MONTHLY_LOAD_B4_SALES(UPPER_TRANS_GROUP))
         ALLOCATE(M_MONTHLY_LOAD_AFTER_SALES(UPPER_TRANS_GROUP))
         ALLOCATE(M_PURCHASE_ENERGY(UPPER_TRANS_GROUP))
         ALLOCATE(M_PURCHASE_COSTS(UPPER_TRANS_GROUP))
         ALLOCATE(MARKET_COST_ABOVE_RESOURCES(UPPER_TRANS_GROUP))
         ALLOCATE(M_SALES_ENERGY(UPPER_TRANS_GROUP))
         ALLOCATE(M_SALES_REVENUES(UPPER_TRANS_GROUP))
         ALLOCATE(M_NATIVE_COST(UPPER_TRANS_GROUP))
         ALLOCATE(M_SPINNING_MWH(UPPER_TRANS_GROUP))
         ALLOCATE(M_UNSERVED_ENERGY(UPPER_TRANS_GROUP))
         ALLOCATE(M_ABOVE_RESOURCES(UPPER_TRANS_GROUP))
         ALLOCATE(M_UNSERVED_ENERGY_COST(UPPER_TRANS_GROUP))
         ALLOCATE(trans_ror_cap(0:UPPER_TRANS_GROUP))
         ALLOCATE(HOURLY_DUMP_CAPACITY(UPPER_TRANS_GROUP))
         ALLOCATE(TRANS_MUST_CAPACITY(UPPER_TRANS_GROUP))
         ALLOCATE(TRANS_SPINNING_CAPACITY(UPPER_TRANS_GROUP))
         ALLOCATE(AREA_PRICE_MULT(UPPER_TRANS_GROUP))
         ALLOCATE(TG_USING_PRICE_DISTN(UPPER_TRANS_GROUP))
         ALLOCATE(HOURLY_SPINNING_CAPACITY(UPPER_TRANS_GROUP))
         ALLOCATE(DAILY_PEAK(UPPER_TRANS_GROUP))
         ALLOCATE(OFF_PEAK_SPINNING_CAPACITY(UPPER_TRANS_GROUP))
         ALLOCATE(TRANS_RAMP_UP(UPPER_TRANS_GROUP))
         ALLOCATE(TRANS_RAMP_DOWN(UPPER_TRANS_GROUP))
         ALLOCATE(TRANS_MAX_IMPORT(UPPER_TRANS_GROUP))
         ALLOCATE(TRANS_MAX_EXPORT(UPPER_TRANS_GROUP))
         ALLOCATE(LAST_HOUR_SELL(UPPER_TRANS_GROUP))
         ALLOCATE(MONTH_COIN_PEAK(0:UPPER_TRANS_GROUP))
         ALLOCATE(MONTH_NON_COIN_PEAK(0:UPPER_TRANS_GROUP))
         ALLOCATE(HOURLY_TRANSACTION(UPPER_TRANS_GROUP))
         ALLOCATE(MAX_HOURLY_IMPORT(UPPER_TRANS_GROUP))
         ALLOCATE(MAX_HOURLY_EXPORT(UPPER_TRANS_GROUP))
         ALLOCATE(SCARCITY_MULT(0:UPPER_TRANS_GROUP))
         ALLOCATE(SYSTEM_STORAGE(0:800,0:UPPER_TRANS_GROUP))
         ALLOCATE(SYSTEM_DERIVATIVES(0:800,UPPER_TRANS_GROUP))

         M_MONTHLY_PRO_COST_AFTER_SALES = 0.
         M_MONTHLY_PRO_COST_B4_SALES = 0.
         M_MONTHLY_LOAD_AFTER_SALES = 0.
         M_MONTHLY_LOAD_B4_SALES = 0.
         M_PURCHASE_ENERGY = 0.
         M_PURCHASE_COSTS = 0.
         M_SALES_ENERGY = 0.
         MARKET_COST_ABOVE_RESOURCES = 0.
         M_SALES_REVENUES = 0.
         M_NATIVE_COST = 0.
         M_SPINNING_MWH = 0.
         M_UNSERVED_ENERGY = 0.
         M_ABOVE_RESOURCES = 0.
         M_UNSERVED_ENERGY_COST = 0.
         trans_ror_cap = 0.
         TRANS_MUST_CAPACITY = 0.
         TRANS_SPINNING_CAPACITY = 0.
         AREA_PRICE_MULT = 0.
         HOURLY_SPINNING_CAPACITY = 0.
         OFF_PEAK_SPINNING_CAPACITY = 0.
         TRANS_RAMP_UP = 0.
         TRANS_RAMP_DOWN = 0.
         TRANS_MAX_IMPORT = 0.
         TRANS_MAX_EXPORT = 0.
         LAST_HOUR_SELL = 0.
         MONTH_COIN_PEAK = 0.
         MONTH_NON_COIN_PEAK = 0.
!
         SYSTEM_STORAGE = 0.
         SYSTEM_DERIVATIVES = 0.0
         SYSTEM_AVAIL_STORAGE = 0.
         SYSTEM_AVAIL_DERIVATIVES = 0.
!
         TG_USING_PRICE_DISTN = 0
!
         VOID_LOGICAL = GET_TRANS_SPINNING_CAPACITY(UPPER_TRANS_GROUP, &
                                R_MONTH,globecom_year,TRANS_SPINNING_CAPACITY)

         VOID_LOGICAL = GET_TG_PRICE_MULT( &
                             AREA_PRICE_MULT, &
                             TG_USING_PRICE_DISTN, &
                             UPPER_TRANS_GROUP, &
                             R_MONTH, &
                             globecom_year)

         VOID_LOGICAL = GET_OFF_PEAK_SPINNING_CAPACITY( &
                                UPPER_TRANS_GROUP, &
                                R_MONTH,globecom_year, &
                                OFF_PEAK_SPINNING_CAPACITY)
         VOID_LOGICAL = GET_TRANS_RAMP_RATES(UPPER_TRANS_GROUP, &
                                            R_MONTH,TRANS_RAMP_UP, &
                                            TRANS_RAMP_DOWN)
         VOID_LOGICAL = GET_TRANS_MAX_IMPORT_EXPORT( &
                                           UPPER_TRANS_GROUP, &
                                           R_MONTH, &
                                           TRANS_MAX_IMPORT, &
                                           TRANS_MAX_EXPORT)
         CALL GET_TRANS_ROR_CAPACITY(UPPER_TRANS_GROUP, &
                                                   trans_ror_cap)

         IF(R_MONTH == FIRST_MONTHLY_TRANSACT) THEN
! 10/23/03.        
!
            TEMP_L = ANN_HOURLY_SCEN_ELECT_MULT(globecom_year)
            TEMP_L = ANN_HRLY_TG_SCEN_ELECT_MULT( &
       globecom_year,UPPER_TRANS_GROUP,gc_end_point) ! R_TG HERE MEANS NUMBER OF TG

            TEMP_L = ANN_HOURLY_SCEN_GAS_MULT(globecom_year)
            TEMP_L = ANN_HOURLY_SCEN_OIL_MULT(globecom_year)
!
            TEMP_L = PROCESS_MARKET_RESOURCES(UPPER_TRANS_GROUP, &
                                                      MULTI_AREA_PRICE)
            IF(ALLOCATED(ANNUAL_UNSERVED_ENERGY_COST))then
               DEALLOCATE(ANNUAL_UNSERVED_ENERGY_COST)
               deallocate(ANNUAL_COST_ABOVE_RESOURCES)
               deallocate(ANNUAL_UNSERVED_ENERGY)
               deallocate(ANNUAL_ABOVE_RESOURCES)
            end if
            ALLOCATE(M_ANNUAL_PURCHASE_ENERGY(UPPER_TRANS_GROUP))
            ALLOCATE(M_ANNUAL_PURCHASE_COSTS(UPPER_TRANS_GROUP))
            ALLOCATE(M_ANNUAL_SALES_ENERGY(UPPER_TRANS_GROUP))
            ALLOCATE(M_ANNUAL_SALES_REVENUES(UPPER_TRANS_GROUP))
            ALLOCATE(M_ANNUAL_NATIVE_COST(UPPER_TRANS_GROUP))
            ALLOCATE(M_ANNUAL_LOAD_B4_SALES(UPPER_TRANS_GROUP))
            ALLOCATE(M_ANNUAL_PRO_COST_B4_SALES(UPPER_TRANS_GROUP))
            ALLOCATE(M_ANNUAL_LOAD_AFTER_SALES(UPPER_TRANS_GROUP))
            ALLOCATE(M_ANNUAL_PRO_COST_AFTER_SALES(UPPER_TRANS_GROUP))
            ALLOCATE(ANNUAL_TL_MWH(UPPER_TRANS_GROUP))
            ALLOCATE(ANNUAL_TL_PEAK(UPPER_TRANS_GROUP))
            ALLOCATE(ANNUAL_COIN_PEAK(0:UPPER_TRANS_GROUP))
            ALLOCATE(ANNUAL_TL_BASE(UPPER_TRANS_GROUP))
            ALLOCATE(ANNUAL_TL_HYDRO_MWH(UPPER_TRANS_GROUP))
            ALLOCATE(ANNUAL_TL_HYDRO_MW(UPPER_TRANS_GROUP))
            ALLOCATE(ANNUAL_TL_HYDRO_ROR(UPPER_TRANS_GROUP))
            ALLOCATE(ANNUAL_SPINNING_MWH(UPPER_TRANS_GROUP))
            ALLOCATE(ANNUAL_EFFECTIVE_CAPACITY(UPPER_TRANS_GROUP))
            ALLOCATE(ANNUAL_UNSERVED_ENERGY(UPPER_TRANS_GROUP,0:12))
            ALLOCATE(ANNUAL_ABOVE_RESOURCES(UPPER_TRANS_GROUP,0:12))
            ALLOCATE(ANNUAL_UNSERVED_ENERGY_COST(UPPER_TRANS_GROUP))
      ALLOCATE(ANNUAL_COST_ABOVE_RESOURCES(UPPER_TRANS_GROUP,0:12))
      ALLOCATE(ANNUAL_PRODUCT_PRICE(0:UPPER_TRANS_GROUP, NUM_PRODUCTS))
      ALLOCATE(ANNUAL_PRODUCT_QUANTITY(0:UPPER_TRANS_GROUP,NUM_PRODUCTS))
      ALLOCATE(ANNUAL_PRODUCT_HEATRATE(0:UPPER_TRANS_GROUP, NUM_PRODUCTS))
      ALLOCATE(ANNUAL_PRODUCT_MARGINAL_FUEL(0:UPPER_TRANS_GROUP,NUM_PRODUCTS))
      ALLOCATE(ANNUAL_PRODUCT_FUEL_PRICE(0:UPPER_TRANS_GROUP,NUM_PRODUCTS))
            ALLOCATE(ANNUAL_PRODUCT_HOURS(NUM_PRODUCTS))
!     

            ANNUAL_PRODUCT_PRICE = 0.
            ANNUAL_PRODUCT_QUANTITY = 0.
            ANNUAL_PRODUCT_HEATRATE = 0.
            ANNUAL_PRODUCT_MARGINAL_FUEL = 0.
            ANNUAL_PRODUCT_FUEL_PRICE = 0.
            ANNUAL_PRODUCT_HOURS = 0.
            M_ANNUAL_PURCHASE_ENERGY = 0.
            M_ANNUAL_PURCHASE_COSTS = 0.
            M_ANNUAL_SALES_ENERGY = 0.
            M_ANNUAL_SALES_REVENUES = 0.
            M_ANNUAL_NATIVE_COST = 0.
            M_ANNUAL_LOAD_B4_SALES = 0.
            M_ANNUAL_LOAD_AFTER_SALES = 0.
            M_ANNUAL_PRO_COST_B4_SALES = 0.
            M_ANNUAL_PRO_COST_AFTER_SALES = 0.
            ANNUAL_TL_MWH = 0.
            ANNUAL_TL_PEAK = 0.
            ANNUAL_COIN_PEAK = 0.
            ANNUAL_TL_BASE = 999999.
            ANNUAL_TL_HYDRO_MWH = 0.
            ANNUAL_TL_HYDRO_MW = 0.
            ANNUAL_TL_HYDRO_ROR = 0.
            ANNUAL_SPINNING_MWH = 0.
            ANNUAL_EFFECTIVE_CAPACITY = 0.
            ANNUAL_UNSERVED_ENERGY = 0.
            ANNUAL_ABOVE_RESOURCES = 0.
            ANNUAL_UNSERVED_ENERGY_COST = 0.
            ANNUAL_COST_ABOVE_RESOURCES = 0.
         ENDIF

! FISCAL REPORTING MOVE UP
!
         YES_FISCAL_REPORTING = &
            IS_FISCAL_YEAR_ACTIVE(FISCAL_SEASON_RESET, &
            FISCAL_ONLY)
      
         IF(FISCAL_SEASON_RESET == 1) THEN
            FISCAL_SEASON = LAST_MONTHLY_TRANSACT
         ELSE
            FISCAL_SEASON = FISCAL_SEASON_RESET - 1
         ENDIF

         FISCAL_YEAR = FLOAT(loc_YEAR+get_BASE_YEAR())
      
         IF(FISCAL_ONLY) THEN
            IF(FISCAL_SEASON_RESET > 1 .AND.  &
                 R_MONTH >= FISCAL_SEASON_RESET) THEN
               FISCAL_YEAR = FLOAT(loc_YEAR+get_BASE_YEAR()+1)
            ENDIF
         ENDIF
! FIRST PASS AND FISCAL_SEASON_RESET (FROM BELOW)        
            
         IF( .NOT. ALLOCATED(FISCAL_PURCHASE_ENERGY)) THEN
            ALLOCATE(FISCAL_PURCHASE_ENERGY(UPPER_TRANS_GROUP))
            ALLOCATE(FISCAL_PURCHASE_COSTS(UPPER_TRANS_GROUP))
            ALLOCATE(FISCAL_SALES_ENERGY(UPPER_TRANS_GROUP))
            ALLOCATE(FISCAL_SALES_REVENUES(UPPER_TRANS_GROUP))
            ALLOCATE(FISCAL_NATIVE_COST(UPPER_TRANS_GROUP))
            ALLOCATE(FISCAL_LOAD_B4_SALES(UPPER_TRANS_GROUP))
            ALLOCATE(FISCAL_PRO_COST_B4_SALES(UPPER_TRANS_GROUP))
            ALLOCATE(FISCAL_LOAD_AFTER_SALES(UPPER_TRANS_GROUP))
            ALLOCATE(FISCAL_PRO_COST_AFTER_SALES(UPPER_TRANS_GROUP))
            ALLOCATE(FISCAL_TL_MWH(UPPER_TRANS_GROUP))
            ALLOCATE(FISCAL_TL_PEAK(UPPER_TRANS_GROUP))
            ALLOCATE(FISCAL_COIN_PEAK(0:UPPER_TRANS_GROUP))
            ALLOCATE(FISCAL_TL_BASE(UPPER_TRANS_GROUP))
            ALLOCATE(FISCAL_TL_HYDRO_MWH(UPPER_TRANS_GROUP))
            ALLOCATE(FISCAL_TL_HYDRO_MW(UPPER_TRANS_GROUP))
            ALLOCATE(FISCAL_TL_HYDRO_ROR(UPPER_TRANS_GROUP))
            ALLOCATE(FISCAL_SPINNING_MWH(UPPER_TRANS_GROUP))
            ALLOCATE(FISCAL_EFFECTIVE_CAPACITY(UPPER_TRANS_GROUP))
            ALLOCATE(FISCAL_UNSERVED_ENERGY(UPPER_TRANS_GROUP))
            ALLOCATE(FISCAL_ABOVE_RESOURCES(UPPER_TRANS_GROUP))
            ALLOCATE(FISCAL_UNSERVED_ENERGY_COST(UPPER_TRANS_GROUP))
            ALLOCATE(FISCAL_COST_ABOVE_RESOURCES(UPPER_TRANS_GROUP))
            
            FISCAL_PURCHASE_ENERGY = 0.
            FISCAL_PURCHASE_COSTS = 0.
            FISCAL_SALES_ENERGY = 0.
            FISCAL_SALES_REVENUES = 0.
            FISCAL_NATIVE_COST = 0.
            FISCAL_LOAD_B4_SALES = 0.
            FISCAL_LOAD_AFTER_SALES = 0.
            FISCAL_PRO_COST_B4_SALES = 0.
            FISCAL_PRO_COST_AFTER_SALES = 0.
            FISCAL_TL_MWH = 0.
            FISCAL_TL_PEAK = 0.
            FISCAL_COIN_PEAK = 0.
            FISCAL_TL_BASE = 999999.
            FISCAL_TL_HYDRO_MWH = 0.
            FISCAL_TL_HYDRO_MW = 0.
            FISCAL_TL_HYDRO_ROR = 0.
            FISCAL_SPINNING_MWH = 0.
            FISCAL_EFFECTIVE_CAPACITY = 0.
            FISCAL_UNSERVED_ENERGY = 0.
            FISCAL_ABOVE_RESOURCES = 0.
            FISCAL_UNSERVED_ENERGY_COST = 0.
            FISCAL_COST_ABOVE_RESOURCES = 0.
!
            FISCAL_PURCHASES = 0.
            FISCAL_PURCHASE_COST = 0.
            FISCAL_SALES = 0.
            FISCAL_SALES_REVENUE = 0.
            FISCAL_PRO_COST_B4_SALES_V = 0.
            FISCAL_PRO_COST_AFTER_SALES_V = 0.
            FISCAL_LOAD_B4_SALES_V = 0.
            FISCAL_LOAD_AFTER_SALES_V = 0.  
         ENDIF

         ALLOCATE(HOURLY_MARGINAL_COST(0:UPPER_TRANS_GROUP,DAILY_HOURS))
         ALLOCATE(HOURLY_LAST_PRICE(0:UPPER_TRANS_GROUP,DAILY_HOURS))
         ALLOCATE(DAILY_PRODUCTS_CAPACITY( &
                                      DAILY_HOURS,0:UPPER_TRANS_GROUP))
         ALLOCATE(HOURLY_IN_PRICE(0:UPPER_TRANS_GROUP,DAILY_HOURS))
         ALLOCATE(HOURLY_MC_AFTER(0:UPPER_TRANS_GROUP,DAILY_HOURS))
         ALLOCATE(SYSTEM_OUTPUT(DAILY_HOURS))
         ALLOCATE(TIE_FLOW(0:UPPER_TRANS_GROUP,DAILY_HOURS))
         ALLOCATE(HOURLY_LOAD_B4_SALES(0:UPPER_TRANS_GROUP,DAILY_HOURS))
         ALLOCATE(HOURLY_TRANSFER_MWH(UPPER_TRANS_GROUP,DAILY_HOURS))
         ALLOCATE(HOURLY_FORWARDS_4_MONTH(UPPER_TRANS_GROUP, &
                                                     R_HOURS_IN_MONTH))
         ALLOCATE(DAILY_MARKET_PRICE(DAILY_HOURS))
         ALLOCATE(HOURLY_LOADS(0:UPPER_TRANS_GROUP,DAILY_HOURS))
         ALLOCATE(TEST_HOURLY_REVENUE(0:UPPER_TRANS_GROUP,DAILY_HOURS))
         ALLOCATE(MARGINAL_COST_DELTA(0:UPPER_TRANS_GROUP,DAILY_HOURS))
         ALLOCATE(HOURLY_PRO_COST_AFTER_SALES(0:UPPER_TRANS_GROUP, &
                                                          DAILY_HOURS))
         ALLOCATE(HOURLY_CAPACITY(0:UPPER_TRANS_GROUP,DAILY_HOURS))
         ALLOCATE(HOURLY_EUE(0:UPPER_TRANS_GROUP,DAILY_HOURS))
         ALLOCATE(HOURLY_DERIVATIVES(0:UPPER_TRANS_GROUP,DAILY_HOURS))
         ALLOCATE(HOURLY_PRO_COST_B4_SALES(0:UPPER_TRANS_GROUP, &
                                                          DAILY_HOURS))
         ALLOCATE(HOURLY_INCREMENTAL_COST(0:UPPER_TRANS_GROUP, &
                                                          DAILY_HOURS))
         ALLOCATE(HOURLY_LAMDA(0:UPPER_TRANS_GROUP,DAILY_HOURS))
         ALLOCATE(PRODUCT_PRICE(0:UPPER_TRANS_GROUP,NUM_PRODUCTS))
         ALLOCATE(PRODUCT_VOLATILITY(0:UPPER_TRANS_GROUP,NUM_PRODUCTS))
         ALLOCATE(PRODUCT_QUANTITY(0:UPPER_TRANS_GROUP,NUM_PRODUCTS))
         ALLOCATE(PRODUCT_HEATRATE(0:UPPER_TRANS_GROUP,NUM_PRODUCTS))
         ALLOCATE(PRODUCT_MARGINAL_FUEL(0:UPPER_TRANS_GROUP, &
                                                        NUM_PRODUCTS))
         ALLOCATE(PRODUCT_FUEL_PRICE(0:UPPER_TRANS_GROUP,NUM_PRODUCTS))
         ALLOCATE(PRODUCT_HOURS(NUM_PRODUCTS))
         ALLOCATE(PRODUCT_MEAN_RETURN(0:UPPER_TRANS_GROUP,NUM_PRODUCTS))
         ALLOCATE(SUM_SQUARED_DEVIATIONS( &
                                     0:UPPER_TRANS_GROUP,NUM_PRODUCTS))
         ALLOCATE(PRODUCT_DAILY_RETURN( &
                    0:UPPER_TRANS_GROUP,NUM_PRODUCTS,R_HOURS_IN_MONTH))
         ALLOCATE(SCARCITY_COST(0:UPPER_TRANS_GROUP,DAILY_HOURS))
         ALLOCATE(ENERGY_COST(0:UPPER_TRANS_GROUP,DAILY_HOURS))
!
         PRODUCT_PRICE = 0.
         PRODUCT_VOLATILITY = 0.
         PRODUCT_QUANTITY = 0.
         PRODUCT_HEATRATE = 0.
         PRODUCT_MARGINAL_FUEL = 0. 
         PRODUCT_FUEL_PRICE = 0.
         PRODUCT_HOURS = 0.
         PRODUCT_MEAN_RETURN = 0.
         SUM_SQUARED_DEVIATIONS = 0.
         SCARCITY_COST = 0.
         ENERGY_COST = 0.0
         HOURLY_LOAD_B4_SALES = 0.
         PRODUCT_DAILY_RETURN = 0.
         HOURLY_FORWARDS_4_MONTH = 0.
         WRITE_DAY_OF_WEEK = 0.
!
!
! 6/28/96. GAT. TEMPORARY HARD-CODE FOR LGE RUNS.
!
         SPREAD_MARGIN = SPREAD_PRICES(SPREADS) 
         BUY_SPREAD_MARGIN = 0.
         SELL_SPREAD_MARGIN = 0.0 ! PER SINCLAIR. 8/20/96.
         
         MAX_TIE_FLOW = 0.
         MIN_TIE_FLOW = TIE_LIMIT
         IF(R_MONTH == FIRST_MONTHLY_TRANSACT) THEN
            IF(ALLOCATED(PRODUCT_LAST_PRICE)) & ! ADD 6/18/98 MSG
                                         DEALLOCATE(PRODUCT_LAST_PRICE)
            ALLOCATE(PRODUCT_LAST_PRICE( &
                                     0:UPPER_TRANS_GROUP,NUM_PRODUCTS))
            PRODUCT_LAST_PRICE = 0.
            ANNUAL_PURCHASES = 0.
            ANNUAL_PURCHASE_COST = 0.
            ANNUAL_SALES = 0.
            ANNUAL_SALES_REVENUE = 0.
            ANNUAL_PRO_COST_B4_SALES = 0.
            ANNUAL_PRO_COST_AFTER_SALES = 0.
            ANNUAL_LOAD_B4_SALES = 0.
            ANNUAL_LOAD_AFTER_SALES = 0.  
!
            MONTHLY_TRANSFER_MWH = 0.
            MONTHLY_TRANSFER_REVENUE = 0.

    
         ENDIF
         


         PURCHASE_ENERGY(R_MONTH) = 0.
         PURCHASE_COSTS(R_MONTH) = 0.
         SALES_ENERGY(R_MONTH) = 0.
         SALES_REVENUE(R_MONTH) = 0.
         MONTHLY_LOAD_B4_SALES(R_MONTH) = 0.
         MONTHLY_LOAD_AFTER_SALES(R_MONTH) = 0.
         MONTHLY_PRO_COST_B4_SALES(R_MONTH) = 0.
         MONTHLY_PRO_COST_AFTER_SALES(R_MONTH) = 0.

         IF(.NOT. USE_MARKET_PRICING .AND. &
                    .NOT. REGIONAL_AREA_PRICE .AND. &
                                  .NOT. CENTRAL_DISPATCH_TRANSACT) THEN
            WRITE(4,*) '*** error in TRANSOBJ ***'
            WRITE(4,*) "Trying to run Transact with Market Pricing"
            WRITE(4,*) "when no Market Price file found in the"
            WRITE(4,*) "base directory."
            er_message='See WARNING MESSAGES -transobj.for-5'
            call end_program(er_message)
         ENDIF

         TOTAL_UNSERVED_COST = 0.
         TOTAL_ABOVE_RESOURCES = 0.
         TOTAL_UNSERVED = 0.
         TOTAL_COST_ABOVE_RESOURCES = 0.
         L_M = C_M
!         
         HOURLY_LAST_PRICE = 0.
         DAILY_MARKET_PRICE = 0.
!
! YEAR IS 1 BASED
!
         SCENARIO_ELECTRIC_MULT = &
                            GET_SCENARIO_ELECTRIC_PRICE(globecom_year,R_MONTH)
!
         YES_MONTH_OUTAGE_EVENTS = OUTAGE_EVENTS_IN_MONTH() 
!
! 11/15/00. GAT. HOURLY PRICES FOR ALL TRANSACTION GROUPS IS KNOWN
!           BEFORE THE MONTH BEGINS, REGARDLESS OF METHOD.
!
! ALL_MONTH_PRICES BECOMES THE ONE PRICE SOURCE
!
         ALLOCATE(ALL_MONTH_PRICES(R_HOURS_IN_MONTH, &
          0:UPPER_TRANS_GROUP))
         ALL_MONTH_PRICES = 0.
         TIE_FLOW = 0.0 
!
         HOUR_IN_DAY = 1
         DAY = 1
!
! 10/22/03
!

!
         DO HR = 1, R_HOURS_IN_MONTH
            DO I = LOWER_TRANS_GROUP, UPPER_TRANS_GROUP

               IF(CENTRAL_DISPATCH_TRANSACT) THEN
                  TG = GET_TRANS_GROUP_INDEX(I)
                  DATA_BASE = GET_DATA_BASE_FOR_TRANS(TG,DAY_TYPE)
! NOTE THAT THIS IGNORES FORWARD SALES
                  HOURLY_LOAD_B4_SALES(I,HOUR_IN_DAY) = &
                       MAX(0.,GET_TRANS_LOAD_AFTER_EL(HR,I) &
                                       -trans_ror_cap(I))
                                       
                  HOURLY_MARGINAL_COST(I,HOUR_IN_DAY) = &
                     GET_MARGINAL_COST_AT_MARKET( &
                      REAL(HOURLY_LOAD_B4_SALES(I,HOUR_IN_DAY),8), &
                      DATA_BASE,SCARCITY_COST(I,HOUR_IN_DAY),L_M, &
                      1.)  ! Greg note this is a missing argument 8/8/01
                     
!
                  ALL_MONTH_PRICES(HR,I) = &
                           HOURLY_MARGINAL_COST(I,HOUR_IN_DAY) * &
                                           MARKET_PRICE_SCALAR * &
                                        SCENARIO_ELECTRIC_MULT * &
                        GET_HOURLY_SCEN_ELECT_MULT(HR,R_MONTH) * &
                  GET_HRLY_TG_SCEN_ELECT_MULT(HR,R_MONTH,I)

               ELSEIF(MULTI_AREA_PRICE .OR. REGIONAL_AREA_PRICE) THEN
                  IF(HOUR_IN_DAY == 1) &
                  CALL READ_USER_MARKET_DATA (HR, &
                                            R_MONTH, &
                                            DAY, &
                                            I, &
                                            GET_HOURLY_PRICE_NAME(I))
! 11/25/02.
                  IF(TG_USING_PRICE_DISTN(I) == 1) THEN
                     ALL_MONTH_PRICES(HR,I) = &
                      HOURLY_IN_PRICE(I,HOUR_IN_DAY) * &
                                      MARKET_PRICE_SCALAR * &
                                        AREA_PRICE_MULT(I)* &
                  GET_HOURLY_SCEN_ELECT_MULT(HR,R_MONTH) * & ! 10/29/03.
             GET_HRLY_TG_SCEN_ELECT_MULT(HR,R_MONTH,I) ! 01/08/04.
                  ELSE
                     ALL_MONTH_PRICES(HR,I) = &
                                 HOURLY_IN_PRICE(I,HOUR_IN_DAY) * &
                                                 MARKET_PRICE_SCALAR * &
                                              SCENARIO_ELECTRIC_MULT * &
                              GET_HOURLY_SCEN_ELECT_MULT(HR,R_MONTH) * &
                        GET_HRLY_TG_SCEN_ELECT_MULT(HR,R_MONTH,I)
                  ENDIF
               ELSE
                  ALL_MONTH_PRICES(HR,I) = &
                                  MONTHLY_MARKET_PRICES(HR) * &
                                                 MARKET_PRICE_SCALAR * &
                                              SCENARIO_ELECTRIC_MULT * &
                              GET_HOURLY_SCEN_ELECT_MULT(HR,R_MONTH) * &
                        GET_HRLY_TG_SCEN_ELECT_MULT(HR,R_MONTH,I)
               ENDIF
!
               IF(.NOT. TEST_DAILY_OPTION) THEN

                  CALENDAR_DAY_OF_WEEK = GET_DAY_OF_WEEK_4(R_MONTH,DAY)
                  HOURLY_FORWARDS_4_MONTH(I,HR) = &
                        HOURLY_FORWARD_CONTRACT_ENERGY( &
                               HOUR_IN_DAY, &
                               CALENDAR_DAY_OF_WEEK,DAY,I, &
                               ALL_MONTH_PRICES(HR,I),R_MONTH) 
               ENDIF
!
!
            ENDDO ! TRANSACTION_GROUPS
!
            IF(HOUR_IN_DAY < 24) THEN
               HOUR_IN_DAY = HOUR_IN_DAY + 1
            ELSE
               HOUR_IN_DAY = 1
               DAY = DAY + 1
            ENDIF
!
         ENDDO
!
         HOURLY_TRANSFER_PRICE = 0.
         HOURLY_TRANSFER_SYS_PRICE = 0.
!
         HOUR_IN_DAY = 1
         DAY = 1
!
         DO HR = 1, R_HOURS_IN_MONTH
!
!
            IF(YES_MONTH_OUTAGE_EVENTS) THEN
               TEMP_L = UPDATE_OUTAGE_DATA(HR)
            ENDIF
!            
! MOVED UP. 7/22/98. GAT.
            REMAIN = MOD(FLOAT(HR),24.)
            IF(REMAIN < .001) THEN ! SUMMARIZE THE DAY
               DAY = HR/24
!
               WRITE_DAY_OF_WEEK(DAY) = CALENDAR_DAY_OF_WEEK
!
            ELSE
               DAY = HR/24 + 1
            ENDIF

            CALENDAR_DAY_OF_WEEK = GET_DAY_OF_WEEK_4(R_MONTH,DAY)
            DAY_TYPE = &
                   GET_DAY_TYPE(HOUR_IN_DAY, &
                                          CALENDAR_DAY_OF_WEEK,R_MONTH)
!
            IF(APPLY_ENERGY_PRODUCT(HOUR_IN_DAY, &
                                       CALENDAR_DAY_OF_WEEK, &
                             LOCAL_PRODUCT_TYPE(HOUR_IS_ON_PEAK))) THEN

               PEAK_HOUR = 1
            ELSE
               PEAK_HOUR = 2
            ENDIF
!
            IF(APPLY_ENERGY_PRODUCT(HOUR_IN_DAY, &
                                      CALENDAR_DAY_OF_WEEK, &
                              LOCAL_PRODUCT_TYPE(HOUR_IS_SAX16)) .OR. &
              APPLY_ENERGY_PRODUCT(HOUR_IN_DAY, &
                                      CALENDAR_DAY_OF_WEEK, &
                              LOCAL_PRODUCT_TYPE(HOUR_IS_SUX16))) THEN
               WEEKEND_DAY = .TRUE.
               NIGHT = .FALSE.
            ELSE
               WEEKEND_DAY = .FALSE.
               IF(PEAK_HOUR ==2) THEN
                  NIGHT = .TRUE.
               ELSE
                  NIGHT = .FALSE.
               ENDIF
            ENDIF
            SCARCITY_MULT = 1.
!
            IF(HOUR_IN_DAY == 1) THEN
               DAILY_PRODUCTS_CAPACITY = 0.
               DAILY_PEAK = 0.
               HOURLY_TRANSFER_MWH = 0.
            ENDIF
!
            VOID_LOGICAL = INIT_HOURLY_TIE()
! 1/8/03. ALTERED HOUR_PATH_LIMIT TO REFLECT MULTIPLIERS
            IF(.NOT. MULTI_MARKET_ACTIVE) THEN
               VOID_LOGICAL = &
                           INIT_HOUR_PATH_LIMIT(PEAK_HOUR,R_MONTH,globecom_year)
            ENDIF
! 10/23/02.  TESTING !!!

            VOID_LOGICAL =  INIT_HOUR_CONSTRAINT_LIMIT()
            HOURLY_TRANSACTION = 0.d0
            MAX_HOURLY_IMPORT = 0.
            MAX_HOURLY_EXPORT = 0.
            HOURLY_LOAD_B4_SALES(0,HOUR_IN_DAY) = 0.
!
!
! 11/19/01. MOVED ABOVE TRANSACTIONS.
!
! 8/30/01. AMEREN TRANSFER PRICING ROUTINE
!
! ASSUME THAT THE TWO GROUPS ARE THE FIRST TWO CONSECUTIVE GROUPS
!
!
            IF(DETAILED_TRANSFER_PRICING .AND. &
                                          .NOT. TEST_DAILY_OPTION) THEN
!
!
               DO I = 1, 2
                  TG = GET_TRANS_GROUP_INDEX(I)
                  DATA_BASE = GET_DATA_BASE_FOR_TRANS(TG,DAY_TYPE)
                  TRANSFER_LOAD_B4(I) = &
                       MAX(0.,GET_TRANS_LOAD_AFTER_EL(HR,I) + &
                              HOURLY_FORWARDS_4_MONTH(I,HR) - &
                                     trans_ror_cap(I))
                  HOURLY_MARGINAL_COST(I,HOUR_IN_DAY) = &
                       GET_MARGINAL_COST_AT_MARKET( &
                           REAL(TRANSFER_LOAD_B4(I),8), &
                           DATA_BASE,SCARCITY_COST(I,HOUR_IN_DAY),L_M, &
                           SCARCITY_MULT(I))
               ENDDO
!
               TRANSFER_LOAD_B4(0) = TRANSFER_LOAD_B4(1) + &
                                    TRANSFER_LOAD_B4(2)
!
               HOURLY_MARGINAL_COST(MARKET_TG,HOUR_IN_DAY) = &
                         GET_MARGINAL_COST_AT_MARKET( &
                            REAL(TRANSFER_LOAD_B4(0),8),INT(1,2), &
                            SCARCITY_COST(MARKET_TG,HOUR_IN_DAY),L_M, &
                            SCARCITY_MULT(MARKET_TG))
!
               TRANSFER_MULT = 1.0
!
! ALWAYS TRANSFER BETWEEN COMPANIES TO EQUATE MARGINS COST
! BLESSING 09/21/01.
!
               IF(ABS(HOURLY_MARGINAL_COST(2,HOUR_IN_DAY) - &
                        HOURLY_MARGINAL_COST(1,HOUR_IN_DAY)) > .1) THEN
                  IF(HOURLY_MARGINAL_COST(1,HOUR_IN_DAY) < &
                              HOURLY_MARGINAL_COST(2,HOUR_IN_DAY)) THEN
!
                     I = 1
                     J = 2

                  ELSE
!
                     I = 2
                     J = 1

                  ENDIF
!
! NO TRANSFER            
!
               ELSE
                  I = 0
                  J = 0
               ENDIF
!

                  HOURLY_MARGINAL_COST(MARKET_TG,HOUR_IN_DAY) = &
                    MAX(HOURLY_MARGINAL_COST(MARKET_TG,HOUR_IN_DAY), &
                                   HOURLY_MARGINAL_COST(I,HOUR_IN_DAY))

!
               HOURLY_TRANSFER_SYS_PRICE(HOUR_IN_DAY) = &
                            HOURLY_MARGINAL_COST(MARKET_TG,HOUR_IN_DAY)

!               
               IF (I > 0) THEN
!
                  TG = GET_TRANS_GROUP_INDEX(J)
                  DATA_BASE = GET_DATA_BASE_FOR_TRANS(TG,DAY_TYPE)
                  BUYERS_COST = GET_CLOSEST_MARGINAL_COST( &
                          HOURLY_MARGINAL_COST(MARKET_TG,HOUR_IN_DAY), &
                                                        DATA_BASE,L_M)
!
                  TG = GET_TRANS_GROUP_INDEX(I)
                  DATA_BASE = GET_DATA_BASE_FOR_TRANS(TG,DAY_TYPE)
!
                  PATH_CAPACITY = HOUR_TIE_LIMIT(I,J)
!
                  SELLERS_AVAILABLE_CAPACITY = MAX(0.d0, &
                      TRANS_GROUP_CAP(DATA_BASE) - TRANSFER_LOAD_B4(I))
!
                  TEMP_SELL_SPREAD = PATH_SPREAD_RATE(I,J, &
                                                PEAK_HOUR,R_MONTH,globecom_year)
!
                  TEMP_SELL_WHEEL = PATH_WHEELING_CHARGE(I,J, &
                                                PEAK_HOUR,R_MONTH,globecom_year)
!
!
! 11/16/01. WHICH SYSTEM IS ON THE MARGIN?
!
                  SELLERS_COST = GET_CLOSEST_MARGINAL_COST( &
                          HOURLY_MARGINAL_COST(MARKET_TG,HOUR_IN_DAY), &
                                                        DATA_BASE,L_M)
!

!
                  TEMP_TRANSFER_SELLER_MWH = 0.

                     IF( ABS(SELLERS_COST - &
                      HOURLY_MARGINAL_COST(MARKET_TG,HOUR_IN_DAY)) <= &
                      ABS(BUYERS_COST - &
                      HOURLY_MARGINAL_COST(MARKET_TG,HOUR_IN_DAY))) THEN
                        TG = GET_TRANS_GROUP_INDEX(J)
                        DATA_BASE_2 = &
                                   GET_DATA_BASE_FOR_TRANS(TG,DAY_TYPE)
                        TEMP_TRANSFER_SELLER_MWH = & 
                         MAX( 0.,sngl(REAL( &
                            TRANSFER_LOAD_B4(J),8) - &
                         GET_PRODUCTION_LEVEL_AT_MARKET( &
                          HOURLY_MARGINAL_COST(MARKET_TG,HOUR_IN_DAY), &
                                                    DATA_BASE_2,L_M)))
                     ELSE
                        TEMP_TRANSFER_SELLER_MWH = &
                         MAX( 0.,sngl( &
                            GET_PRODUCTION_LEVEL_AT_MARKET(MAX( 0., &
                          HOURLY_MARGINAL_COST(MARKET_TG,HOUR_IN_DAY)- &
                                    TEMP_SELL_WHEEL-TEMP_SELL_SPREAD), &
                                                      DATA_BASE,L_M) - &
                                                 TRANSFER_LOAD_B4(I)))
                     ENDIF

!
                  TEMP_TRANSFER_MWH = &
                       MIN(TEMP_TRANSFER_SELLER_MWH,PATH_CAPACITY, &
                                      sngl(SELLERS_AVAILABLE_CAPACITY))
!     
!
!                  IF(5 < 4) THEN
                  IF(TEMP_TRANSFER_MWH > 0.) THEN
!
                     SELLERS_COST = 0.
                     TEMP_R1 = TRANSFER_LOAD_B4(I) + &
                                  HOURLY_OPTION_CAPACITY - &
                                           RES_N_SUP_CAPACITY_USED
                     TEMP_R2 = TEMP_TRANSFER_MWH/99.
                     DO K = 1, 100 ! ARBITRARY RESOLUTION
!
                        SELLERS_COST = SELLERS_COST + & 
                        ! THIS RESETS THE POINT IN THE COST CURVE
                        GET_MARGINAL_COST_AT_MARKET( &
                        REAL(TEMP_R1), &
                        DATA_BASE,SCARCITY_COST(I,HOUR_IN_DAY),L_M, &
                        SCARCITY_MULT(I)) 
                        TEMP_R1 = TEMP_R1 + TEMP_R2
!                        
                     ENDDO
!
! 11/15/01. FOR BLESSING.
!
!
                     TEMP_TRANSFER_PRICE = SELLERS_COST/100.
                     IF(TEMP_TRANSFER_PRICE < 0.) THEN
                        WRITE(4,*) '*** line 2975 TRANSOBJ.FOR ***'
                        WRITE(4,*) "TRANSFER PRICING NOT WORKING"
                        er_message= &
                        'See WARNING MESSAGES -transobj.for-6'
                        call end_program(er_message)
                     ENDIF
!
!                     ENDIF
                        HOURLY_TRANSFER_PRICE(HOUR_IN_DAY) = &
                                                    TEMP_TRANSFER_PRICE
                        MONTHLY_TRANSFER_REVENUE(I,R_MONTH) = &
                                MONTHLY_TRANSFER_REVENUE(I,R_MONTH) + &
                                   TEMP_TRANSFER_MWH * &
                                     HOURLY_TRANSFER_PRICE(HOUR_IN_DAY)
!
! POSITIVE IS SELL, NEGATIVE IS BUY
!
                        HOURLY_TRANSFER_MWH(I,HOUR_IN_DAY) = &
                                    TEMP_TRANSFER_MWH*TRANSFER_MULT
                        HOURLY_TRANSFER_MWH(J,HOUR_IN_DAY) = &
                                      -TEMP_TRANSFER_MWH*TRANSFER_MULT
                        MONTHLY_TRANSFER_MWH(I,R_MONTH) = &
                                     MONTHLY_TRANSFER_MWH(I,R_MONTH) + &
                                     HOURLY_TRANSFER_MWH(I,HOUR_IN_DAY)
                        MONTHLY_TRANSFER_MWH(J,R_MONTH) = &
                                     MONTHLY_TRANSFER_MWH(J,R_MONTH) + &
                                     HOURLY_TRANSFER_MWH(J,HOUR_IN_DAY)
!     
                  ELSE
!                  
                     HOURLY_TRANSFER_PRICE(HOUR_IN_DAY) = 0.
!
                     HOURLY_TRANSFER_MWH(I,HOUR_IN_DAY) = 0.
                     HOURLY_TRANSFER_MWH(J,HOUR_IN_DAY) = 0.
!                     
                  ENDIF ! TRANSFER > 0.
!
               ELSE
                  HOURLY_TRANSFER_PRICE(HOUR_IN_DAY) = 0.
!
                  HOURLY_TRANSFER_MWH(1,HOUR_IN_DAY) = 0.
                  HOURLY_TRANSFER_MWH(2,HOUR_IN_DAY) = 0.
               ENDIF ! ATTEMPT TRANSFER 
!
            ENDIF ! DETAILED TRANSFER PRICING
!            
            DO I = LOWER_TRANS_GROUP, UPPER_TRANS_GROUP
!
! USING AN EXTERNAL MARKET PRICE
!
               IF(WEEKEND_DAY) THEN
                  SCARCITY_MULT(I) = GET_WEEKEND_SCARCITY_MULT(I)
                  HOURLY_SPINNING_CAPACITY(I) = &
                                          OFF_PEAK_SPINNING_CAPACITY(I)
               ELSEIF(NIGHT) THEN
                  SCARCITY_MULT(I) = GET_NIGHT_SCARCITY_MULT(I)
                  HOURLY_SPINNING_CAPACITY(I) = &
                                          OFF_PEAK_SPINNING_CAPACITY(I)
               ELSE
                  SCARCITY_MULT(I) = 1.0
                  HOURLY_SPINNING_CAPACITY(I) = &
                                             TRANS_SPINNING_CAPACITY(I)
               ENDIF
!
               IF(HOUR_IN_DAY == 1) THEN
                  DAILY_PRICE(1) = ALL_MONTH_PRICES(HR,I)
                  DAILY_PRICE(2) = ALL_MONTH_PRICES(HR+1,I)
                  DAILY_PRICE(3) = ALL_MONTH_PRICES(HR+2,I)
                  DAILY_PRICE(4) = ALL_MONTH_PRICES(HR+3,I)
                  DAILY_PRICE(5) = ALL_MONTH_PRICES(HR+4,I)
                  DAILY_PRICE(6) = ALL_MONTH_PRICES(HR+5,I)
                  DAILY_PRICE(7) = ALL_MONTH_PRICES(HR+6,I)
                  DAILY_PRICE(8) = ALL_MONTH_PRICES(HR+7,I)
                  DAILY_PRICE(9) = ALL_MONTH_PRICES(HR+8,I)
                  DAILY_PRICE(10) = ALL_MONTH_PRICES(HR+9,I)
                  DAILY_PRICE(11) = ALL_MONTH_PRICES(HR+10,I)
                  DAILY_PRICE(12) = ALL_MONTH_PRICES(HR+11,I)
                  DAILY_PRICE(13) = ALL_MONTH_PRICES(HR+12,I)
                  DAILY_PRICE(14) = ALL_MONTH_PRICES(HR+13,I)
                  DAILY_PRICE(15) = ALL_MONTH_PRICES(HR+14,I)
                  DAILY_PRICE(16) = ALL_MONTH_PRICES(HR+15,I)
                  DAILY_PRICE(17) = ALL_MONTH_PRICES(HR+16,I)
                  DAILY_PRICE(18) = ALL_MONTH_PRICES(HR+17,I)
                  DAILY_PRICE(19) = ALL_MONTH_PRICES(HR+18,I)
                  DAILY_PRICE(20) = ALL_MONTH_PRICES(HR+19,I)
                  DAILY_PRICE(21) = ALL_MONTH_PRICES(HR+20,I)
                  DAILY_PRICE(22) = ALL_MONTH_PRICES(HR+21,I)
                  DAILY_PRICE(23) = ALL_MONTH_PRICES(HR+22,I)
                  DAILY_PRICE(24) = ALL_MONTH_PRICES(HR+23,I)

               ENDIF

               TG = GET_TRANS_GROUP_INDEX(I)
               DATA_BASE = GET_DATA_BASE_FOR_TRANS(TG,DAY_TYPE)
               IF(.NOT. TRANS_GROUP_GEN_ACTIVE(DATA_BASE)) CYCLE 
                            ! DATA BASE NOT DEFINED IN THIS HOUR
!
!
! TESTING START-UP LOGIC.
!
               IF(TEST_DAILY_OPTION) THEN
!
! 092121. NEW STORAGE AND HYBRID LOGIC.
!
                  IF(R_MONTH == 1 .AND. HR == 1) THEN
                     VOID_LOGICAL = CX_DailyStorePat3()
                  ENDIF
!
! MAY WANT TO CALL THIS FOR THE WHOLE MONTH AND ADD TO THE 
! EVENT CALENDAR.
!
                  IF(HR == 1) THEN
                     DAYS_IN_MONTH = R_HOURS_IN_MONTH/24
                     APPLY_COMMITMENT_IN_MONTH = &
                          DAILY_OPTION_FOR_MONTH(DAYS_IN_MONTH, &
                                              CL_MONTH_NAME(R_MONTH), &
                                              DATA_BASE, &
                                              ALL_MONTH_PRICES(HR,I), &
                                              R_MONTH, &
                                              I, &
                                              trans_ror_cap(I), &
                                            TRANS_SPINNING_CAPACITY(I))
                  ENDIF
                  HOURLY_FORWARD_SALE = HOURLY_DERIVATIVE_LOAD(HR,I)
                  HOURLY_OPTION_CAPACITY = 0.
               ELSE
!
! 10/20/00. GAT. DAILY OPTION FROM THE ENERGY PRODUCTS FILE.
!
!               
! GET THE STATE OF THE SYSTEM BEFORE TRANSACTIONS 
! I.E. SERVING NATIVE LOAD

!
                  HOURLY_FORWARD_SALE = HOURLY_FORWARDS_4_MONTH(I,HR)
! 6/27/02. 
                  IF(HR == 1) THEN
                     TEMP_L = DailyOperMoPumpedStorage( &
                      I, &
                      ALL_MONTH_PRICES(HR,I), & ! (24,31) hourly detail 
                      ! Alan: I need a single dimension string with *
                      SYSTEM_STORAGE(0,I), & ! (24,31) hourly detail 
                      ! Alan: I need a single dimension 801 values
                      R_MONTH, &
                      DAYS_IN_MONTH, &
                      SYSTEM_AVAIL_STORAGE)
! 04/12/04.     
                     TEMP_L = MONTHLY_CALL_PUT_CAPACITY( &
                      I, &
                      ALL_MONTH_PRICES(HR,I), & ! (24,31) hourly detail 
                      ! Alan: I need a single dimension string with *
                      SYSTEM_DERIVATIVES(0,I), & ! (24,31) hourly detail
                      ! Alan: I need a single dimension 801 values
                      R_MONTH, &
                      DAYS_IN_MONTH, &
                      SYSTEM_AVAIL_DERIVATIVES)
                  ENDIF
!
                  HOURLY_FORWARD_SALE = &
                          HOURLY_FORWARD_SALE + &
                                   SYSTEM_STORAGE(HR,I) + &
                                                SYSTEM_DERIVATIVES(0,I)
!                  
                  IF(HOUR_IN_DAY == 1) THEN
                     YES_DAILY_CALL_PUT_CAPACITY = &
                      DAILY_CALL_PUT_CAPACITY( &
                           CALENDAR_DAY_OF_WEEK, &  
                           DAY,I, &
                           DAILY_PRICE, &
                           DAILY_PRODUCTS_CAPACITY(1,I), &
                           R_MONTH, &
                           AVAIL_DERIVATIVE_CAPACITY) ! NOT USED FOR NOW
                  ENDIF
                  HOURLY_OPTION_CAPACITY = &
                                -DAILY_PRODUCTS_CAPACITY(HOUR_IN_DAY,I)
               ENDIF
!
!
!

!
               IF(HOOSIER) THEN
                  HOURLY_OPTION_CAPACITY = 0.0
                  HOURLY_FORWARD_SALE = HOURLY_FORWARD_SALE + &
                                                 HOURLY_OPTION_CAPACITY
               ENDIF
               IF(USE_TF_FILE) THEN
! 4/25/99. GAT.

!
! NOTE REASSIGNMENT OF HOURLY_LOAD_B4_SALES AND READ ACROSS 24 HOURS
! 080101 
!
                  IF(HOUR_IN_DAY == 1) THEN
                     DO J = 1, 24
                        HOURLY_LOAD_B4_SALES(I,J) = &
                               GET_TRANS_LOAD_AFTER_EL(INT(HR+J-1,2),I)
                        DAILY_PEAK(I) = MAX(DAILY_PEAK(I), &
                                             HOURLY_LOAD_B4_SALES(I,J))
                     ENDDO
                  ENDIF
!
!
                  HOURLY_SPINNING_CAPACITY(I) = &
                       GET_DAILY_PEAK_SPIN(I, &
                                       R_MONTH, &
                                       globecom_year, &
                                       HOURLY_SPINNING_CAPACITY(I), &
                                       PEAK_HOUR, &
                                       DAILY_PEAK(I))
!
                  HOURLY_LOAD_B4_SALES(I,HOUR_IN_DAY) = &
                   MAX(0.d0,REAL(HOURLY_LOAD_B4_SALES(I,HOUR_IN_DAY) + &
                                  HOURLY_TRANSFER_MWH(I,HOUR_IN_DAY) + &
                               HOURLY_FORWARD_SALE - &
                                    trans_ror_cap(I),8))
                  IF(.NOT. APPLY_COMMITMENT_IN_MONTH) THEN
                     HOURLY_LOAD_B4_SALES(0,HOUR_IN_DAY) = &
                       HOURLY_LOAD_B4_SALES(0,HOUR_IN_DAY) + &
                             HOURLY_LOAD_B4_SALES(I,HOUR_IN_DAY) 
                  ENDIF
               ELSEIF(USE_TRANSACT_LOADS) THEN
                  HOURLY_LOAD_B4_SALES(I,HOUR_IN_DAY) = &
                                              SYSTEM_HOURLY_LOADS(HR,I)
               ELSE
                  HOURLY_LOAD_B4_SALES(I,HOUR_IN_DAY) = &
                                      GET_TRANS_GROUP_HOURLY_LOAD(HR,I)
               ENDIF
!

!
               HOURLY_MARKET_PRICE = ALL_MONTH_PRICES(HR,I)
!
!
               ! Hourly_last_price set here.
               HOURLY_LAST_PRICE(I,HOUR_IN_DAY)=HOURLY_MARKET_PRICE
!         
               IF(.NOT. MULTI_MARKET_ACTIVE) THEN

                  TEMP_BUY_SPREAD = PATH_SPREAD_RATE(MARKET_TG,I, &
                                                PEAK_HOUR,R_MONTH,globecom_year)
                  TEMP_SELL_SPREAD = PATH_SPREAD_RATE(I,MARKET_TG, &
                                                PEAK_HOUR,R_MONTH,globecom_year)
                  TEMP_BUY_WHEEL = &
                   MAX(PATH_WHEELING_CHARGE(MARKET_TG,I, &
                                             PEAK_HOUR,R_MONTH,globecom_year), &
                                                       TEMP_BUY_SPREAD)
                  TEMP_SELL_WHEEL = MAX( &
                                PATH_WHEELING_CHARGE(I,MARKET_TG, &
                                             PEAK_HOUR,R_MONTH,globecom_year), &
                                                      TEMP_SELL_SPREAD)
               ENDIF
!
! COMMITMENT IMPACT.
! 5/13/01. MOVED TEST UP AND HOURLY MARGINAL COST INSIDE  
! TESTING SPINNING WHEN GENERATION APPROACHES ZERO DUE TO MARKET.
!
               IF(APPLY_COMMITMENT_IN_MONTH) THEN
!
! MODIFIED 08/20/01 FOR EDE
! MODIFIED 10/15/01 FOR AMEREN.
!               
! 5/21/02. HOURLY_LOAD_B4_SALES IS AFTER DERIVATIVES AND ROR HYDRO.
!               
                  HOURLY_LOAD_B4_SALES(I,HOUR_IN_DAY) = &
                                      HOURLY_COMMITMENT_LOAD(HR,I) 

!
                  HOURLY_LOAD_B4_SALES(0,HOUR_IN_DAY) = &
                       HOURLY_LOAD_B4_SALES(0,HOUR_IN_DAY) + &
                             HOURLY_LOAD_B4_SALES(I,HOUR_IN_DAY) 
!
                  HOURLY_MARGINAL_COST(I,HOUR_IN_DAY) = &
                                 HOURLY_COMMITMENT_MC_AT_LOAD(HR,I) 
                  HOURLY_PRO_COST_B4_SALES(I,HOUR_IN_DAY) = &
                       HOURLY_MARGINAL_COST(I,HOUR_IN_DAY) * &
                                    HOURLY_LOAD_B4_SALES(I,HOUR_IN_DAY)
!                  
                  HOURLY_SPINNING_CAPACITY(I) = &
                                    HOURLY_COMMITMENT_SPIN(HR,I)
                  TIE_POWER = &
                     HOURLY_COMMITMENT_GENERATION(HR,I) - &
                                    HOURLY_LOAD_B4_SALES(I,HOUR_IN_DAY)

               ELSE
!
                  HOURLY_MARGINAL_COST(I,HOUR_IN_DAY) = &
                   GET_MARGINAL_COST_AT_MARKET( &
                          REAL(HOURLY_LOAD_B4_SALES(I,HOUR_IN_DAY),8), &
                          DATA_BASE,SCARCITY_COST(I,HOUR_IN_DAY),L_M, &
                          SCARCITY_MULT(I))
                  HOURLY_PRO_COST_B4_SALES(I,HOUR_IN_DAY) = &
                       GET_AVERAGE_COST(HR,DATA_BASE,"Before") * &
                                    HOURLY_LOAD_B4_SALES(I,HOUR_IN_DAY)
                  TIE_POWER = &
                   MAX(0.,sngl(GET_PRODUCTION_LEVEL_AT_MARKET( &
                            HOURLY_MARKET_PRICE,DATA_BASE,L_M)) - &
                                          HOURLY_SPINNING_CAPACITY(I)) &
                          - HOURLY_LOAD_B4_SALES(I,HOUR_IN_DAY)
! MOVED 5/21/02.
                  TIE_POWER = TIE_POWER - HOURLY_OPTION_CAPACITY
               ENDIF
!
! ESTIMATE IMPACT OF THE MARKET
!
               MARGINAL_COST_DELTA(I,HOUR_IN_DAY) = &
                       HOURLY_MARKET_PRICE - &
                                    HOURLY_MARGINAL_COST(I,HOUR_IN_DAY)
               IF(MARGINAL_COST_DELTA(I,HOUR_IN_DAY) < 0.) THEN
                                                           ! NET BUYER
                  IF(-MARGINAL_COST_DELTA(I,HOUR_IN_DAY) < &
                                                   TEMP_BUY_WHEEL) THEN
                     PRICE_AFTER_WHEEL = &
                                    HOURLY_MARGINAL_COST(I,HOUR_IN_DAY)
                  ELSE
                     PRICE_AFTER_WHEEL = &
                                   HOURLY_MARKET_PRICE + TEMP_BUY_WHEEL
                  ENDIF
               ELSE
                  IF(MARGINAL_COST_DELTA(I,HOUR_IN_DAY) < &
                                                  TEMP_SELL_WHEEL) THEN
                     PRICE_AFTER_WHEEL = &
                                    HOURLY_MARGINAL_COST(I,HOUR_IN_DAY)
                  ELSE
                     PRICE_AFTER_WHEEL = &
                                  HOURLY_MARKET_PRICE + TEMP_SELL_WHEEL
                  ENDIF
               ENDIF

!
               I_AVAILABLE_CAPACITY = &
                          TRANS_GROUP_CAP(DATA_BASE) - &
                                 HOURLY_LOAD_B4_SALES(I,HOUR_IN_DAY) - &
                                                 HOURLY_OPTION_CAPACITY
!
! 3/25/00. ECITY.
!
               RESERVE_CAPACITY_USED = 0.
               SUPPLEMENTAL_CAPACITY_USED = 0.
               IF(ECITIES .AND. .NOT. APPLY_COMMITMENT_IN_MONTH) THEN
                  D_OR_C = I ! NEED SWITCH TO TELL !
                  IF(D_OR_C > 0) THEN
!
                     IF(HR == 1) THEN
                        BEFORE_RETAINED = GET_DUKE_BEFORE_RETAINED()
                        MONTHLY_CONTINGENT_CAP = &
                                         -GET_MONTHLY_CONTINGENT_CAP(I)
                     ENDIF
!
                     HOURLY_AC_LOAD = &
                              HOURLY_LOAD_B4_SALES(I,HOUR_IN_DAY) + &
                                                    HOURLY_FORWARD_SALE

! CONTINGENT AND HOURLY FORWARD ARE NEGATIVE
!
                     EQUIVALENT_RETAINED =  RETAINED_AVAILABLE(1) + &
                                                   HOURLY_FORWARD_SALE
!

                     RESERVE_CAPACITY_USED = &
                            MAX(0., &
                               MIN(BEFORE_RETAINED + &
                                      MONTHLY_CONTINGENT_CAP - &
                                                  EQUIVALENT_RETAINED, &
                                   HOURLY_AC_LOAD - &
                                                EQUIVALENT_RETAINED))
!     
                     RESERVE_CAPACITY(D_OR_C) = &
                                   MAX(RESERVE_CAPACITY_USED, &
                                              RESERVE_CAPACITY(D_OR_C))
!
!
                     TIE_POWER = TIE_POWER + RESERVE_CAPACITY_USED
                     MONTHLY_RESERVE_ENERGY(D_OR_C) = &
                                MONTHLY_RESERVE_ENERGY(D_OR_C) + & 
                                               RESERVE_CAPACITY_USED
                     IF(RESERVE_CAPACITY_USED > 0.) THEN
                        RESERVE_CAPACITY_USED = RESERVE_CAPACITY_USED
                     ENDIF

                  ENDIF ! DUKE OR CPL OR JOINT
               ENDIF ! ECITIES
               RES_N_SUP_CAPACITY_USED = RESERVE_CAPACITY_USED + &
                                          SUPPLEMENTAL_CAPACITY_USED

               IF(HOURLY_LOAD_B4_SALES(I,HOUR_IN_DAY) > 0.) THEN
                  IF(.NOT. APPLY_COMMITMENT_IN_MONTH) THEN
                     TEMP_NATIVE_COST = &
                                    HOURLY_MARKET_PRICE * &
                                    HOURLY_LOAD_B4_SALES(I,HOUR_IN_DAY)
                     M_NATIVE_COST(I) = M_NATIVE_COST(I) + &
                                                    TEMP_NATIVE_COST
                     IF(PRICE_ONLY_WHOLESALE_REV) THEN
                        IF(TIE_POWER < 0.) THEN
                           PERCENT_OF_COST_2_ALLOCATE = -TIE_POWER / &
                                    HOURLY_LOAD_B4_SALES(I,HOUR_IN_DAY)
                        ELSE
                           PERCENT_OF_COST_2_ALLOCATE = 0.
                        ENDIF
                     ELSE     
                        PERCENT_OF_COST_2_ALLOCATE = 1.0
                     ENDIF
                     TEMP_NATIVE_COST = TEMP_NATIVE_COST * &
                                             PERCENT_OF_COST_2_ALLOCATE
                     YES_PUT_AC_HOURLY_COST = &
                       PUT_AC_HOURLY_COST_AT_MARKET(TG, &
                                                  HOURLY_MARKET_PRICE, &
                                                  TEMP_NATIVE_COST, &
                                                  HR,R_MONTH)
                  ELSE ! CALL TRANSACT C
                     M_NATIVE_COST(I) = M_NATIVE_COST(I) + &
                                               HOURLY_NATIVE_COST(HR,I)
                  ENDIF 
               ENDIF
!

!     

!
!
! ADDED FOR MACY. 3/30/99. GAT. 
!
               HOURLY_EUE(I,HOUR_IN_DAY) = 0.
               HOURLY_CAPACITY(I,HOUR_IN_DAY) = 0.
! MODIFIED 1/24/02.
               IF(TRANS_MAX_IMPORT(I) >= 0.) THEN
                  MAX_HOURLY_IMPORT(I) = TRANS_MAX_IMPORT(I)
               ELSE
                  MAX_HOURLY_IMPORT(I) = &
                       ESCALATED_MONTHLY_VALUE( &
                                   ABS(TRANS_MAX_IMPORT(I)), &
                                   INT(ABS(TRANS_MAX_IMPORT(I)),2), &
                                               globecom_year, &
                                               R_MONTH,INT(1,2))
               ENDIF

               IF(TRANS_MAX_EXPORT(I) >= 0.) THEN
                  MAX_HOURLY_EXPORT(I) = TRANS_MAX_EXPORT(I)
               ELSE
                  MAX_HOURLY_EXPORT(I) = &
                       ESCALATED_MONTHLY_VALUE( &
                                   ABS(TRANS_MAX_EXPORT(I)), &
                                   INT(ABS(TRANS_MAX_EXPORT(I)),2), &
                                               globecom_year, &
                                               R_MONTH,INT(1,2))
               ENDIF
!
! 7/24/01. GAT.
!
               IF(HR == 1) THEN
                     MAX_HOURLY_RAMP_UP = TIE_POWER
               ELSEIF(REAL(TIE_POWER,8) > LAST_HOUR_SELL(I) ) THEN
               ! RAMP UP

                  MAX_HOURLY_RAMP_UP = &
                          MIN(sngl(REAL(TRANS_RAMP_UP(I),8) + &
                                          LAST_HOUR_SELL(I)),TIE_POWER)
               ELSE
                  MAX_HOURLY_RAMP_UP = &
                          MAX(sngl(LAST_HOUR_SELL(I) - &
                                 REAL(TRANS_RAMP_DOWN(I),8)),TIE_POWER)

               ENDIF
!!! TEMPORARY !!! 09/11/01
            IF(APPLY_COMMITMENT_IN_MONTH) THEN
               IF(TIE_POWER > 0.) THEN
                  TEMP_MARKET_PRICE = &
                                  HOURLY_MARKET_PRICE - TEMP_SELL_WHEEL
               ELSE
                  TEMP_MARKET_PRICE = &
                                  HOURLY_MARKET_PRICE + TEMP_BUY_WHEEL
               ENDIF
               IF(TIE_POWER > 0.) THEN
                  SALES_ENERGY(R_MONTH) = SALES_ENERGY(R_MONTH) + &
                                                              TIE_POWER
                  SALES_REVENUE(R_MONTH) = SALES_REVENUE(R_MONTH) + &
                                        TEMP_MARKET_PRICE * TIE_POWER
! 6/22/98.
                  M_SALES_ENERGY(I) =  M_SALES_ENERGY(I) + TIE_POWER
                  M_SALES_REVENUES(I) = M_SALES_REVENUES(I) + &
                                        TEMP_MARKET_PRICE * TIE_POWER

               ELSEIF(TIE_POWER < 0.) THEN
                   PURCHASE_ENERGY(R_MONTH)=PURCHASE_ENERGY(R_MONTH) + &
                                                         ABS(TIE_POWER)
                   PURCHASE_COSTS(R_MONTH) = PURCHASE_COSTS(R_MONTH) + &
                                   TEMP_MARKET_PRICE * ABS(TIE_POWER)

! 6/22/98.
                     M_PURCHASE_ENERGY(I) = M_PURCHASE_ENERGY(I) + &
                                                         ABS(TIE_POWER)
                     M_PURCHASE_COSTS(I) = M_PURCHASE_COSTS(I) + &
                                   TEMP_MARKET_PRICE * ABS(TIE_POWER)

               ELSE
                  TIE_POWER = 0.
                  TIE_FLOW(I,HOUR_IN_DAY) = 0.
               ENDIF
!

               UNSERVED = HOURLY_COMMITMENT_UNSERVED(HR,I)
               IF(UNSERVED > 0.) THEN
                  M_UNSERVED_ENERGY(I) = & 
                          M_UNSERVED_ENERGY(I) +  UNSERVED
                  HOURLY_EUE(I,HOUR_IN_DAY) = UNSERVED
                  M_UNSERVED_ENERGY_COST(I) = & 
                          M_UNSERVED_ENERGY_COST(I) + &
                                   TRANSACT_UNSERVED_COST * UNSERVED
               ENDIF
               ABOVE_RESOURCES = HOURLY_ENERGY_ABOVE_RESOURCES(HR)
!
               IF(ABOVE_RESOURCES > 0.001) THEN
                  M_ABOVE_RESOURCES(I) = M_ABOVE_RESOURCES(I) + &
                                                        ABOVE_RESOURCES
                  IF(UNSERVED > 0.001) THEN
                     IF(ABOVE_RESOURCES/UNSERVED < .9999 .AND. &
                                  UNSERVED - ABOVE_RESOURCES > .1) THEN
                        WRITE(4,*) '*** line 3588 TRANSOBJ.FOR ***'
                        WRITE(4,*) "UNSERVED CALCULATIONS IN TRANSACT C"
!
! 060206. FOR BURESH.
!
                       er_message='See WARNING MESSAGES -transobj.for-7'
                       call end_program(er_message)
                     ENDIF
                  ENDIF
                  MARKET_COST_ABOVE_RESOURCES(I) = &
                             MARKET_COST_ABOVE_RESOURCES(I) + &
                                   TRANSACT_UNSERVED_COST * UNSERVED + &
                                       (ABOVE_RESOURCES - UNSERVED) * &
                                                    TEMP_MARKET_PRICE

               ENDIF
!
               TIE_FLOW(I,HOUR_IN_DAY) = TIE_POWER
            ELSE
!
               IF(TIE_POWER > 0.) THEN
                  IF(APPLY_COMMITMENT_IN_MONTH .OR. &
                       (.NOT. ZERO_SELL .AND. &
                            MARGINAL_COST_DELTA(I,HOUR_IN_DAY) - &
                                         TEMP_SELL_SPREAD > .001)) THEN
                     IF(NEW_TRANS_LOGIC) THEN
                        PATH_CAPACITY = HOUR_TIE_LIMIT(I,MARKET_TG)
                     ELSE
                        PATH_CAPACITY = HOURLY_TIE_LIMIT(I,MARKET_TG)
                     ENDIF
                     TIE_POWER = MIN(TIE_POWER,PATH_CAPACITY, &
                                          REAL(MAX_HOURLY_RAMP_UP), &
                                     MAX_HOURLY_EXPORT(I))
                     SALES_ENERGY(R_MONTH) = SALES_ENERGY(R_MONTH) + &
                                                              TIE_POWER
                     SALES_REVENUE(R_MONTH) = SALES_REVENUE(R_MONTH) + &
                                        HOURLY_MARKET_PRICE * TIE_POWER
! 6/22/98.
                     M_SALES_ENERGY(I) =  M_SALES_ENERGY(I) + TIE_POWER
                     M_SALES_REVENUES(I) = M_SALES_REVENUES(I) + &
                                        HOURLY_MARKET_PRICE * TIE_POWER
! END 6/22/98.
!
!
                     TIE_FLOW(I,HOUR_IN_DAY) = TIE_POWER
                  ELSE
                     TIE_POWER = 0.
                     TIE_FLOW(I,HOUR_IN_DAY) = 0.
                  ENDIF
!     

! ADDED FOR MACY. 3/30/99. GAT. 
!
               ELSEIF(TIE_POWER < 0.) THEN
                  IF(.NOT. ZERO_BUY .AND. &
                                      I_AVAILABLE_CAPACITY < 0.d0) THEN
                     IF(NEW_TRANS_LOGIC) THEN
                        PATH_CAPACITY = HOUR_TIE_LIMIT(MARKET_TG,I)
                     ELSE
                        PATH_CAPACITY = HOURLY_TIE_LIMIT(MARKET_TG,I)
                     ENDIF
                     TIE_POWER = MAX(TIE_POWER,-PATH_CAPACITY, &
                                             REAL(MAX_HOURLY_RAMP_UP), &
                                            -MAX_HOURLY_IMPORT(I)) 
                   PURCHASE_ENERGY(R_MONTH)=PURCHASE_ENERGY(R_MONTH) + &
                                                        ABS(TIE_POWER)
                   PURCHASE_COSTS(R_MONTH) = PURCHASE_COSTS(R_MONTH) + &
                                  HOURLY_MARKET_PRICE * ABS(TIE_POWER)
! 6/22/98.
                     M_PURCHASE_ENERGY(I) = M_PURCHASE_ENERGY(I) + &
                                                         ABS(TIE_POWER)
                     M_PURCHASE_COSTS(I) = M_PURCHASE_COSTS(I) + &
                                   HOURLY_MARKET_PRICE * ABS(TIE_POWER)
                     MARKET_COST_ABOVE_RESOURCES(I) = &
                             MARKET_COST_ABOVE_RESOURCES(I) + &
                                   ABS(I_AVAILABLE_CAPACITY) * &
                                                    HOURLY_MARKET_PRICE
! END 6/22/98.
                     TIE_FLOW(I,HOUR_IN_DAY) = TIE_POWER
!
! 7/29/99.
!
                     M_UNSERVED_ENERGY_COST(I) = &
                          M_UNSERVED_ENERGY_COST(I) + &
                                   HOURLY_MARKET_PRICE * &
                                              ABS(I_AVAILABLE_CAPACITY)
! 9/2/99.
                     M_UNSERVED_ENERGY(I) = &
                          M_UNSERVED_ENERGY(I) + &
                                              ABS(I_AVAILABLE_CAPACITY)
!
! 11/9/00. TIE_POWER MAY BE LOWER THAN AVAILABLE DUE TO ECONOMICS
!           OF THE GENERATING SYSTEM.
!
                     HOURLY_EUE(I,HOUR_IN_DAY) = &
                                              ABS(I_AVAILABLE_CAPACITY)

                  ELSEIF(.NOT. ZERO_BUY .AND. &
                  MARGINAL_COST_DELTA(I,HOUR_IN_DAY) + & !NOTE PLUS SIGN
                               TEMP_BUY_SPREAD < -.001) THEN
                     IF(NEW_TRANS_LOGIC) THEN
                        PATH_CAPACITY = HOUR_TIE_LIMIT(MARKET_TG,I)
                     ELSE
                        PATH_CAPACITY = HOURLY_TIE_LIMIT(MARKET_TG,I)
                     ENDIF
                     TIE_POWER = MAX(TIE_POWER,-PATH_CAPACITY, &
                                            REAL(MAX_HOURLY_RAMP_UP), &
                                            -MAX_HOURLY_IMPORT(I))
                   PURCHASE_ENERGY(R_MONTH)=PURCHASE_ENERGY(R_MONTH) + &
                                                        ABS(TIE_POWER)
                   PURCHASE_COSTS(R_MONTH) = PURCHASE_COSTS(R_MONTH) + &
                                  HOURLY_MARKET_PRICE * ABS(TIE_POWER)
! 6/22/98.
                     M_PURCHASE_ENERGY(I) = M_PURCHASE_ENERGY(I) + &
                                                         ABS(TIE_POWER)
                     M_PURCHASE_COSTS(I) = M_PURCHASE_COSTS(I) + &
                                   HOURLY_MARKET_PRICE * ABS(TIE_POWER)
! END 6/22/98.
                     TIE_FLOW(I,HOUR_IN_DAY) = TIE_POWER
                  ELSE
                     TIE_POWER = 0.
                     TIE_FLOW(I,HOUR_IN_DAY) = 0.
                  ENDIF
               ELSE
                  TIE_POWER = 0.
                  TIE_FLOW(I,HOUR_IN_DAY) = 0.

               ENDIF
            ENDIF ! COMMITMENT
!
!               
               HOURLY_LOADS(I,HOUR_IN_DAY) = &
                       HOURLY_LOAD_B4_SALES(I,HOUR_IN_DAY) + &
                                   TIE_FLOW(I,HOUR_IN_DAY)
! 5/13/01. TRAPPED FOR SELLER'S COST AND MARGINAL COST AFTER FOR
!           COMMITMENT LOGIC.
!
!
! RESET PRODUCTION FOR CONSTRAINTS. 4/25/99. GAT.
!
!
!
               IF(APPLY_COMMITMENT_IN_MONTH) THEN
!
                  SELLERS_COST = HOURLY_COMMITMENT_MC(HR,I)
                  HOURLY_MC_AFTER(I,HOUR_IN_DAY) = HOURLY_MARKET_PRICE
                  HOURLY_PRO_COST_AFTER_SALES(I,HOUR_IN_DAY) = &
                             SELLERS_COST * HOURLY_LOADS(I,HOUR_IN_DAY)
!               
               ELSE
!
               SELLERS_COST = & !THIS RESETS THE POINT IN THE COST CURVE
                     GET_MARGINAL_COST_AT_MARKET( &
                        REAL(HOURLY_LOADS(I,HOUR_IN_DAY) + &
                                 HOURLY_OPTION_CAPACITY - &
                                          RES_N_SUP_CAPACITY_USED,8), &
                        DATA_BASE,SCARCITY_COST(I,HOUR_IN_DAY),L_M, &
                        SCARCITY_MULT(I))
! ADDED FOR MARTON. 1/10/00.
                  HOURLY_MC_AFTER(I,HOUR_IN_DAY) = HOURLY_MARKET_PRICE
!
                  HOURLY_PRO_COST_AFTER_SALES(I,HOUR_IN_DAY) = &
                          GET_AVERAGE_COST(HR,DATA_BASE,"After ") * &
                                            HOURLY_LOADS(I,HOUR_IN_DAY)
               ENDIF
!               
               IF(ZERO_SELL .AND. TIE_POWER == 0.0) THEN
                  HOURLY_MC_AFTER(I,HOUR_IN_DAY) = SELLERS_COST
               ENDIF
!
               HOURLY_CAPACITY(I,HOUR_IN_DAY) = &
                                   TRANS_GROUP_CAP(DATA_BASE) + &
                                             RES_N_SUP_CAPACITY_USED - &
                                                HOURLY_OPTION_CAPACITY
!
! 11/9/00.
!     
               HOURLY_DERIVATIVES(I,HOUR_IN_DAY) = &
                           HOURLY_FORWARD_SALE + HOURLY_OPTION_CAPACITY
!     
               MONTHLY_LOAD_B4_SALES(R_MONTH) = &
                          MONTHLY_LOAD_B4_SALES(R_MONTH) + &
                                    HOURLY_LOAD_B4_SALES(I,HOUR_IN_DAY)
!
               MONTHLY_LOAD_AFTER_SALES(R_MONTH) = &
                          MONTHLY_LOAD_AFTER_SALES(R_MONTH) + &
                                            HOURLY_LOADS(I,HOUR_IN_DAY)
!
               MONTHLY_PRO_COST_B4_SALES(R_MONTH) = &
                       MONTHLY_PRO_COST_B4_SALES(R_MONTH) + &
                                HOURLY_PRO_COST_B4_SALES(I,HOUR_IN_DAY)
               MONTHLY_PRO_COST_AFTER_SALES(R_MONTH) = &
                       MONTHLY_PRO_COST_AFTER_SALES(R_MONTH) + &
                             HOURLY_PRO_COST_AFTER_SALES(I,HOUR_IN_DAY)
! 8/1/01.
               M_SPINNING_MWH(I) = M_SPINNING_MWH(I) + &
                                            HOURLY_SPINNING_CAPACITY(I)
!
!
! 6/22/98.
!
               M_MONTHLY_PRO_COST_AFTER_SALES(I) = &
                            M_MONTHLY_PRO_COST_AFTER_SALES(I) + &
                             HOURLY_PRO_COST_AFTER_SALES(I,HOUR_IN_DAY)
               M_MONTHLY_PRO_COST_B4_SALES(I) = &
                               M_MONTHLY_PRO_COST_B4_SALES(I) + &
                                HOURLY_PRO_COST_B4_SALES(I,HOUR_IN_DAY)
               M_MONTHLY_LOAD_B4_SALES(I) = &
                           M_MONTHLY_LOAD_B4_SALES(I) + &
                                    HOURLY_LOAD_B4_SALES(I,HOUR_IN_DAY)
               M_MONTHLY_LOAD_AFTER_SALES(I) = &
                                M_MONTHLY_LOAD_AFTER_SALES(I) + &
                                            HOURLY_LOADS(I,HOUR_IN_DAY)

!
! END OF 6/22/98.
!     
               IF(TIE_FLOW(I,HOUR_IN_DAY) > 0.) THEN
!
                  HOURLY_LAMDA(I,HOUR_IN_DAY) = &
                                    HOURLY_MARGINAL_COST(I,HOUR_IN_DAY)
!     
                  HOURLY_INCREMENTAL_COST(I,HOUR_IN_DAY) = & 
                        (HOURLY_PRO_COST_AFTER_SALES(I,HOUR_IN_DAY) - &
                   HOURLY_PRO_COST_B4_SALES(I,HOUR_IN_DAY))/ &
                                              TIE_FLOW(I,HOUR_IN_DAY)
                  IF(HOURLY_INCREMENTAL_COST(I,HOUR_IN_DAY) > &
                                              HOURLY_MARKET_PRICE) THEN
!     
                     R_MONTH = R_MONTH 
                     ! NATIVE LOOSING MONEY ON THE TRANSACTION
                  ENDIF
               ELSEIF(TIE_FLOW(I,HOUR_IN_DAY) < 0.) THEN
!
                  HOURLY_LAMDA(I,HOUR_IN_DAY) = &
                                         HOURLY_MC_AFTER(I,HOUR_IN_DAY)
!     
                  HOURLY_INCREMENTAL_COST(I,HOUR_IN_DAY) = &
                         (HOURLY_PRO_COST_AFTER_SALES(I,HOUR_IN_DAY) - &
                             HOURLY_PRO_COST_B4_SALES(I,HOUR_IN_DAY))/ &
                                                TIE_FLOW(I,HOUR_IN_DAY)
                  IF(HOURLY_INCREMENTAL_COST(I,HOUR_IN_DAY) < &
                                              HOURLY_MARKET_PRICE) THEN
!     
                     R_MONTH = R_MONTH 
                     ! MARKET LOOSING MONEY ON THE TRANSACTION
                  ENDIF
               ELSE
                  HOURLY_INCREMENTAL_COST(I,HOUR_IN_DAY) = 0.
                  HOURLY_LAMDA(I,HOUR_IN_DAY) = &
                                    HOURLY_MARGINAL_COST(I,HOUR_IN_DAY)
               ENDIF
!
! 7/24/01. GAT.
!
               LAST_HOUR_SELL(I) = TIE_FLOW(I,HOUR_IN_DAY)
!

                  IF(CENTRAL_DISPATCH_TRANSACT) THEN
                     HOURLY_MARKET_PRICE = &
                                         HOURLY_MC_AFTER(I,HOUR_IN_DAY)
                     DAILY_MARKET_PRICE(HOUR_IN_DAY) = &
                                         HOURLY_MC_AFTER(I,HOUR_IN_DAY)
                  ELSE
                     DAILY_MARKET_PRICE(HOUR_IN_DAY) = &
                                                    HOURLY_MARKET_PRICE
                  ENDIF
!               ENDIF
!
! UNION DEFINITION OF REVENUE: IE SPOT ENERGY SALES REVENUE
!
               HOURLY_REVENUE = &
                       DAILY_MARKET_PRICE(HOUR_IN_DAY) * &
                                 HOURLY_LOADS(I,HOUR_IN_DAY)
               IF(.NOT. APPLY_COMMITMENT_IN_MONTH) THEN
                  AVE_SALE = PUT_MARKET_REV_AND_EXP(HR, &
                                HOURLY_REVENUE, &
                                HOURLY_LOADS(I,HOUR_IN_DAY) - &
                                            RES_N_SUP_CAPACITY_USED, &
                                DATA_BASE,R_MONTH,CURRENT_YEAR)
               ENDIF
               MONTH_NON_COIN_PEAK(I) = MAX(MONTH_NON_COIN_PEAK(I), &
                                   HOURLY_LOAD_B4_SALES(I,HOUR_IN_DAY))
!

!               
            ENDDO ! TRANSACTION_GROUPS
!
            IF(HOURLY_LOAD_B4_SALES(0,HOUR_IN_DAY) > &
                                               MONTH_COIN_PEAK(0)) THEN
               DO I = 0, UPPER_TRANS_GROUP
                  MONTH_COIN_PEAK(I) = &
                                    HOURLY_LOAD_B4_SALES(I,HOUR_IN_DAY)
               ENDDO
            ENDIF
!
!            
            IF(YES_TRANSACT_DAILY_REPORTS) THEN
               DO J = 1, NUM_PRODUCTS
                  IF(APPLY_ENERGY_PRODUCT(HOUR_IN_DAY, &
                      CALENDAR_DAY_OF_WEEK,LOCAL_PRODUCT_TYPE(J))) THEN
                     PRODUCT_PRICE(0,J) = PRODUCT_PRICE(0,J) + & 
                                              MONTHLY_MARKET_PRICES(HR) 
                     PRODUCT_QUANTITY(0,J) = PRODUCT_QUANTITY(0,J) + & 
                            HOURLY_LOAD_B4_SALES(INT(0,2),HOUR_IN_DAY)
                     PRODUCT_HOURS(J) = PRODUCT_HOURS(J) + 1.
!
                     TEMP_PRODUCT_HOURS = PRODUCT_HOURS(J)
!
                     IF(PRODUCT_FIRST_PERIOD(J)) THEN
                        PRODUCT_LAST_PRICE(0,J) = &
                                              MONTHLY_MARKET_PRICES(HR)
                        PRODUCT_FIRST_PERIOD(J) = .FALSE.
                     ENDIF
!
                     PRODUCT_DAILY_RETURN(0,J,TEMP_PRODUCT_HOURS) = &
                          LOG(MAX(1.01, &
                                MAX(MONTHLY_MARKET_PRICES(HR),.001)/ &
                                  MAX(.0001,PRODUCT_LAST_PRICE(0,J))))
!
                     PRODUCT_MEAN_RETURN(0,J) = &
                          PRODUCT_MEAN_RETURN(0,J) + &
                           PRODUCT_DAILY_RETURN(0,J,TEMP_PRODUCT_HOURS)
!
                     PRODUCT_LAST_PRICE(0,J) = MONTHLY_MARKET_PRICES(HR)
!                     
                  ENDIF
               ENDDO
!               
               DO I = 1, UPPER_TRANS_GROUP
                  DO J = 1, NUM_PRODUCTS
                     IF(APPLY_ENERGY_PRODUCT(HOUR_IN_DAY, &
                                   CALENDAR_DAY_OF_WEEK, &
                                           LOCAL_PRODUCT_TYPE(J))) THEN
                        PRODUCT_PRICE(I,J) = PRODUCT_PRICE(I,J) + & 
                                                 ALL_MONTH_PRICES(HR,I)
                       PRODUCT_QUANTITY(I,J) = PRODUCT_QUANTITY(I,J) + &
                                    HOURLY_LOAD_B4_SALES(I,HOUR_IN_DAY)

!
                        TEMP_PRODUCT_HOURS = PRODUCT_HOURS(J)
!
                        IF(PRODUCT_FIRST_PERIOD(J)) THEN
                           PRODUCT_LAST_PRICE(I,J) = &
                                                 ALL_MONTH_PRICES(HR,I)
                           PRODUCT_FIRST_PERIOD(J) = .FALSE.
                        ENDIF
!
                        IF(ALL_MONTH_PRICES(HR,I) > 0.) THEN
!
                           IF(TEMP_PRODUCT_HOURS > 744) THEN
                              TEMP_PRODUCT_HOURS = TEMP_PRODUCT_HOURS
                           ENDIF
!                        
                           PRODUCT_DAILY_RETURN(I,J, &
                                                 TEMP_PRODUCT_HOURS) = &
                                 LOG(MAX(1.01, &
                                  MAX(.001,ALL_MONTH_PRICES(HR,I))/ &
                                  MAX(.0001,PRODUCT_LAST_PRICE(I,J))))
                        ELSE
                           PRODUCT_DAILY_RETURN(I,J, &
                                                TEMP_PRODUCT_HOURS) =0.
                        ENDIF
!
                        PRODUCT_MEAN_RETURN(I,J) = &
                          PRODUCT_MEAN_RETURN(I,J) + &
                           PRODUCT_DAILY_RETURN(I,J,TEMP_PRODUCT_HOURS)
!
                        PRODUCT_LAST_PRICE(I,J) = ALL_MONTH_PRICES(HR,I)
!                     
                     ENDIF
                  ENDDO
               ENDDO
            ENDIF
!
            REMAIN = MOD(FLOAT(HR),24.)
            IF(REMAIN < .001) THEN
               START_HOUR = HR - 23


               CALENDAR_DAY_OF_WEEK = GET_DAY_OF_WEEK_4(R_MONTH,DAY)
!     
!
               DO I = LOWER_TRANS_GROUP,UPPER_TRANS_GROUP
! 
                  TG = GET_TRANS_GROUP_INDEX(I)
                  DATA_BASE = GET_DATA_BASE_FOR_TRANS(TG,DAY_TYPE)
                  IF(.NOT. TRANS_GROUP_GEN_ACTIVE(DATA_BASE)) CYCLE 
                                   ! DATA BASE NOT DEFINED IN THIS HOUR
                  IF( REPORT_AREA_ACTIVE(I) .AND. &
                       (YES_TRANSACT_HOURLY_REPORTS .OR. & 
                                         YES_HOURLY_PRICE_REPORT)) THEN
                     TRANS_HMP_REC = RPTREC(TRANS_HMP_UNIT)
                     call check_unit(TRANS_HMP_UNIT, "transobj:0008")
                     ! First write - write hourly last prices
                     WRITE(TRANS_HMP_UNIT,REC= TRANS_HMP_REC) & 
                          PRT_ENDPOINT(), &
                          FLOAT(CURRENT_YEAR), &
                          CL_MONTH_NAME(R_MONTH), &
                          FLOAT(DAY), &
                          MULTI_AREA_NAME(I), &
                          (HOURLY_LAST_PRICE(I,RPT_HR), &
                                                 RPT_HR=1,DAILY_HOURS)

                     TRANS_HMP_REC = TRANS_HMP_REC + 1
!
                     IF(I==1 .AND. DETAILED_TRANSFER_PRICING .AND. &
                                          .NOT. TEST_DAILY_OPTION) THEN
                        AREA_NAME_35 = &
                        TRIM(MULTI_AREA_NAME(I)(1:22))//' Transfer MWH'
                        TRANS_HMP_REC = RPTREC(TRANS_HMP_UNIT)
                        ! Second write - write hourly transfer mwh
                        call check_unit(TRANS_HMP_UNIT, "transobj:0009")
                        WRITE(TRANS_HMP_UNIT,REC= TRANS_HMP_REC) &
                          PRT_ENDPOINT(), &
                          FLOAT(CURRENT_YEAR), &
                          CL_MONTH_NAME(R_MONTH), &
                          FLOAT(DAY), &
                          AREA_NAME_35, &
                          (HOURLY_TRANSFER_MWH(I,RPT_HR), &
                                                 RPT_HR=1,DAILY_HOURS)

                        TRANS_HMP_REC = TRANS_HMP_REC + 1
!
                        TRANS_HMP_REC = RPTREC(TRANS_HMP_UNIT)
                       AREA_NAME_35 = TRIM(MULTI_AREA_NAME(I)(1:20))// &
                                                      ' Transfer Price'
                        ! Third write - write hourly transfer price
                        call check_unit(TRANS_HMP_UNIT, "transobj:0011")
                        WRITE(TRANS_HMP_UNIT,REC= TRANS_HMP_REC) &
                          PRT_ENDPOINT(), &
                          FLOAT(CURRENT_YEAR), &
                          CL_MONTH_NAME(R_MONTH), &
                          FLOAT(DAY), &
                          AREA_NAME_35, & 
                          (HOURLY_TRANSFER_PRICE(RPT_HR), &
                                                 RPT_HR=1,DAILY_HOURS)

                        TRANS_HMP_REC = TRANS_HMP_REC + 1
!
                       AREA_NAME_35 = TRIM(MULTI_AREA_NAME(I)(1:22))// &
                                                        ' Central Disp'
                        TRANS_HMP_REC = RPTREC(TRANS_HMP_UNIT)
                        call check_unit(TRANS_HMP_UNIT, "transobj:0012")
                    ! fourth write - write hourly transfer system price
                        WRITE(TRANS_HMP_UNIT,REC= TRANS_HMP_REC) &
                          PRT_ENDPOINT(), &
                          FLOAT(CURRENT_YEAR), &
                          CL_MONTH_NAME(R_MONTH), &
                          FLOAT(DAY), &
                          AREA_NAME_35, & 
                             ! MULTI_AREA_NAME(I)(1:7)//' Central Disp',
                          (HOURLY_TRANSFER_SYS_PRICE(RPT_HR), &
                                                 RPT_HR=1,DAILY_HOURS)
                        TRANS_HMP_REC = TRANS_HMP_REC + 1
                     ENDIF
!
                  ENDIF
                  IF(YES_TRANSACT_HOURLY_REPORTS .OR. &
                                          YES_HOURLY_PRICE_REPORT) THEN
                     IF(I == LOWER_TRANS_GROUP) THEN
                        TRANS_MRK_REC = RPTREC(TRANS_MRK_UNIT)
                 call check_unit(int(TRANS_MRK_REC,2), "transobj:0013")
                        WRITE(TRANS_MRK_UNIT,REC= TRANS_MRK_REC) & 
                          PRT_ENDPOINT(), &
                          FLOAT(CURRENT_YEAR), &
                          CL_MONTH_NAME(R_MONTH), &
                          FLOAT(DAY), &
                          (DAILY_MARKET_PRICE(RPT_HR), &
                                                  RPT_HR=1,DAILY_HOURS)
                     TRANS_MRK_REC = TRANS_MRK_REC + 1
                     ENDIF
                  ENDIF
                  IF(YES_TRANSACT_HOURLY_REPORTS) THEN

                     TRANS_DEL_REC = RPTREC(TRANS_DEL_UNIT)
                     call check_unit(TRANS_DEL_UNIT, "transobj:0014")
                     WRITE(TRANS_DEL_UNIT,REC= TRANS_DEL_REC) &
                       PRT_ENDPOINT(), &
                       FLOAT(CURRENT_YEAR), &
                       CL_MONTH_NAME(R_MONTH), &
                       FLOAT(DAY), &
                       MULTI_AREA_NAME(I), &
                       (MARGINAL_COST_DELTA(I,RPT_HR), &
                                                  RPT_HR=1,DAILY_HOURS)
                     TRANS_DEL_REC = TRANS_DEL_REC + 1
                     TRANS_MCB_REC = RPTREC(TRANS_MCB_UNIT)
                     call check_unit(TRANS_MCB_UNIT, "tranobj:0016")
                     WRITE(TRANS_MCB_UNIT,REC= TRANS_MCB_REC) &
                       PRT_ENDPOINT(), &
                       FLOAT(CURRENT_YEAR), &
                       CL_MONTH_NAME(R_MONTH), &
                       FLOAT(DAY), &
                       MULTI_AREA_NAME(I), &
                       (HOURLY_MARGINAL_COST(I,RPT_HR), &
                                                  RPT_HR=1,DAILY_HOURS)
                     TRANS_MCB_REC = TRANS_MCB_REC + 1
                     TRANS_PRI_REC = RPTREC(TRANS_PRI_UNIT)
                     call check_unit(trans_pri_unit, "transobj:0017")
                     WRITE(TRANS_PRI_UNIT,REC= TRANS_PRI_REC) &
                       PRT_ENDPOINT(), &
                       FLOAT(CURRENT_YEAR), &
                       CL_MONTH_NAME(R_MONTH), &
                       FLOAT(DAY), &
                       MULTI_AREA_NAME(I), &
                       (HOURLY_MC_AFTER(I,RPT_HR), &
                                                  RPT_HR=1,DAILY_HOURS)
                     TRANS_PRI_REC = TRANS_PRI_REC + 1
                     TRANS_LMD_REC = RPTREC(TRANS_LMD_UNIT)
                     call check_unit(TRANS_LMD_UNIT, "transobj:0018")
                     WRITE(TRANS_LMD_UNIT,REC= TRANS_LMD_REC) &
                      PRT_ENDPOINT(), &
                      FLOAT(CURRENT_YEAR), &
                      CL_MONTH_NAME(R_MONTH), &
                      FLOAT(DAY), &
                      MULTI_AREA_NAME(I), &
                      (HOURLY_LAMDA(I,RPT_HR), &
                                                  RPT_HR=1,DAILY_HOURS)
                     TRANS_LMD_REC = TRANS_LMD_REC + 1
                     TRANS_PB4_REC = RPTREC(TRANS_PB4_UNIT)
                     call check_unit(TRANS_PB4_UNIT, "transobj:0020")
                     WRITE(TRANS_PB4_UNIT,REC= TRANS_PB4_REC) &
                       PRT_ENDPOINT(), &
                       FLOAT(CURRENT_YEAR), &
                       CL_MONTH_NAME(R_MONTH), &
                       FLOAT(DAY), &
                       MULTI_AREA_NAME(I), &
                       (HOURLY_PRO_COST_B4_SALES(I,RPT_HR), &
                                                  RPT_HR=1,DAILY_HOURS)
                     TRANS_PB4_REC = TRANS_PB4_REC + 1
                     TRANS_PAF_REC = RPTREC(TRANS_PAF_UNIT)
                     call check_unit(TRANS_PAF_UNIT, "transobj:0021")
                     WRITE(TRANS_PAF_UNIT,REC= TRANS_PAF_REC) &
                       PRT_ENDPOINT(), &
                       FLOAT(CURRENT_YEAR), &
                       CL_MONTH_NAME(R_MONTH), &
                       FLOAT(DAY), &
                       MULTI_AREA_NAME(I), &
                       (HOURLY_PRO_COST_AFTER_SALES(I,RPT_HR), &
                                                  RPT_HR=1,DAILY_HOURS)
                     TRANS_PAF_REC = TRANS_PAF_REC + 1
                     TRANS_EUE_REC = RPTREC(TRANS_EUE_UNIT)
                     call check_unit(TRANS_EUE_UNIT, "transobj:0022")
                     WRITE(TRANS_EUE_UNIT,REC= TRANS_EUE_REC) &
                       PRT_ENDPOINT(), &
                       FLOAT(CURRENT_YEAR), &
                       CL_MONTH_NAME(R_MONTH), &
                       FLOAT(DAY), &
                       MULTI_AREA_NAME(I), &
                       (HOURLY_EUE(I,RPT_HR),RPT_HR=1,DAILY_HOURS)
                     TRANS_EUE_REC = TRANS_EUE_REC + 1
                     TRANS_INC_REC = RPTREC(TRANS_INC_UNIT)
                     call check_unit(TRANS_INC_UNIT, "transobj:0022")
                     WRITE(TRANS_INC_UNIT,REC= TRANS_INC_REC) &
                       PRT_ENDPOINT(), &
                       FLOAT(CURRENT_YEAR), &
                       CL_MONTH_NAME(R_MONTH), &
                       FLOAT(DAY), &
                       MULTI_AREA_NAME(I), &
                       (HOURLY_INCREMENTAL_COST(I,RPT_HR), &
                                                  RPT_HR=1,DAILY_HOURS)
                     TRANS_INC_REC = TRANS_INC_REC + 1   
!     


                  ENDIF ! TRANSACT_HOURLY_REPORTS
                  IF(YES_HOURLY_AFTER_TRANS_REPORT .OR. &
                                      YES_TRANSACT_HOURLY_REPORTS) THEN
                     TRANS_LAS_REC = RPTREC(TRANS_LAS_UNIT)
                     call check_unit(int(trans_las_rec,2), "transobj:0099")
                     WRITE(TRANS_LAS_UNIT,REC= TRANS_LAS_REC) & 
                          PRT_ENDPOINT(), &
                          FLOAT(CURRENT_YEAR), &
                          CL_MONTH_NAME(R_MONTH), &
                          FLOAT(DAY), &
                          MULTI_AREA_NAME(I), &
                          (HOURLY_LOADS(I,RPT_HR),RPT_HR=1,DAILY_HOURS)
                     TRANS_LAS_REC = TRANS_LAS_REC + 1
                  
                  ENDIF
                  IF(YES_HOURLY_AFTER_HYDRO_REPORT .OR. &
                                      YES_TRANSACT_HOURLY_REPORTS) THEN
                        TRANS_LDS_REC = RPTREC(TRANS_LDS_UNIT)
                  call check_unit(int(TRANS_LDS_REC,2), "transobj:0023")
                        WRITE(TRANS_LDS_UNIT,REC= TRANS_LDS_REC) &
                          PRT_ENDPOINT(), &
                          FLOAT(CURRENT_YEAR), &
                          CL_MONTH_NAME(R_MONTH), &
                          FLOAT(DAY), &
                          MULTI_AREA_NAME(I), &
                          (HOURLY_LOAD_B4_SALES(I,RPT_HR), &
                                                  RPT_HR=1,DAILY_HOURS)
                        TRANS_LDS_REC = TRANS_LDS_REC + 1
                  ENDIF
                  IF(YES_HOURLY_DERIVATIVES_REPORT .OR. &
                                      YES_TRANSACT_HOURLY_REPORTS) THEN
                     TRANS_DER_REC = RPTREC(TRANS_DER_UNIT)
                     call check_unit(TRANS_DER_UNIT, "transobj:0024")
                     WRITE(TRANS_DER_UNIT,REC= TRANS_DER_REC) & 
                       PRT_ENDPOINT(), &
                       FLOAT(CURRENT_YEAR), &
                       CL_MONTH_NAME(R_MONTH), &
                       FLOAT(DAY), &
                       MULTI_AREA_NAME(I), &
                       (HOURLY_DERIVATIVES(I,RPT_HR), &
                                                  RPT_HR=1,DAILY_HOURS)
                     TRANS_DER_REC = TRANS_DER_REC + 1
                  ENDIF
                  IF(YES_HOURLY_CAPACITY_REPORT .OR. &
                                      YES_TRANSACT_HOURLY_REPORTS) THEN
                     TRANS_CAP_REC = RPTREC(TRANS_CAP_UNIT)
                     call check_unit(TRANS_CAP_UNIT, "transobj:0025")
                     WRITE(TRANS_CAP_UNIT,REC= TRANS_CAP_REC) &
                       PRT_ENDPOINT(), &
                       FLOAT(CURRENT_YEAR), &
                       CL_MONTH_NAME(R_MONTH), &
                       FLOAT(DAY), &
                       MULTI_AREA_NAME(I), &
                       (HOURLY_CAPACITY(I,RPT_HR), &
                                                  RPT_HR=1,DAILY_HOURS)
                     TRANS_CAP_REC = TRANS_CAP_REC + 1
                  ENDIF
! 10/22/03
                  IF(HOURLY_TRANSACTION_REPORT .OR. &
                                      YES_TRANSACT_HOURLY_REPORTS) THEN
                     TRANS_TIE_REC = RPTREC(TRANS_TIE_UNIT)
                     call check_unit(TRANS_TIE_UNIT, "transobj:0026")
                     WRITE(TRANS_TIE_UNIT,REC= TRANS_TIE_REC) &
                       PRT_ENDPOINT(), &
                       FLOAT(CURRENT_YEAR), &
                       CL_MONTH_NAME(R_MONTH), &
                       FLOAT(DAY), &
                       MULTI_AREA_NAME(I), &
                       (TIE_FLOW(I,RPT_HR),RPT_HR=1,DAILY_HOURS)
                     TRANS_TIE_REC = TRANS_TIE_REC + 1
                  ENDIF
!                  
                  IF(YES_HOURLY_CAPACITY_COST_REPORT .OR. &
                                      YES_TRANSACT_HOURLY_REPORTS) THEN
                     TRANS_SCA_REC = RPTREC(trans_sca_unit_loc)
                     call check_unit(trans_sca_unit_loc, "transobj:0027")
                     WRITE(trans_sca_unit_loc,REC= TRANS_SCA_REC) &
                       PRT_ENDPOINT(), &
                       FLOAT(CURRENT_YEAR), &
                       CL_MONTH_NAME(R_MONTH), &
                       FLOAT(DAY), &
                       MULTI_AREA_NAME(I), &
                       (SCARCITY_COST(I,RPT_HR), &
                                                  RPT_HR=1,DAILY_HOURS)
                     TRANS_SCA_REC = TRANS_SCA_REC + 1
                  ENDIF
               ENDDO ! TRANSACTION_GROUPS
               HOUR_IN_DAY = 1 ! RESET FOR THE NEXT DAY
!               
            ELSE
               HOUR_IN_DAY = HOUR_IN_DAY + 1
            ENDIF
         ENDDO ! MONTHLY HOURS
!
         IF(DETAILED_TRANSFER_PRICING .AND. TEST_DAILY_OPTION) THEN
            TEMP_L = TRANC_JDA_LOGIC(R_HOURS_IN_MONTH,R_MONTH, &
                                   CURRENT_YEAR,CL_MONTH_NAME(R_MONTH))
         ENDIF
!
         ANNUAL_PURCHASES = ANNUAL_PURCHASES + PURCHASE_ENERGY(R_MONTH)
         ANNUAL_PURCHASE_COST = ANNUAL_PURCHASE_COST + &
                                                PURCHASE_COSTS(R_MONTH)
         ANNUAL_SALES = ANNUAL_SALES + SALES_ENERGY(R_MONTH)
         ANNUAL_SALES_REVENUE = ANNUAL_SALES_REVENUE + &
                                                 SALES_REVENUE(R_MONTH)
!
         ANNUAL_LOAD_B4_SALES = ANNUAL_LOAD_B4_SALES + &
                                         MONTHLY_LOAD_B4_SALES(R_MONTH)
!
         ANNUAL_LOAD_AFTER_SALES = ANNUAL_LOAD_AFTER_SALES + &
                                      MONTHLY_LOAD_AFTER_SALES(R_MONTH)
!
         ANNUAL_PRO_COST_B4_SALES = ANNUAL_PRO_COST_B4_SALES + &
                                     MONTHLY_PRO_COST_B4_SALES(R_MONTH)
         ANNUAL_PRO_COST_AFTER_SALES = ANNUAL_PRO_COST_AFTER_SALES + &
                                  MONTHLY_PRO_COST_AFTER_SALES(R_MONTH)
!
         FISCAL_PURCHASES = FISCAL_PURCHASES + PURCHASE_ENERGY(R_MONTH)
         FISCAL_PURCHASE_COST = FISCAL_PURCHASE_COST + &
                                                PURCHASE_COSTS(R_MONTH)
         FISCAL_SALES = FISCAL_SALES + SALES_ENERGY(R_MONTH)
         FISCAL_SALES_REVENUE = FISCAL_SALES_REVENUE + &
                                                 SALES_REVENUE(R_MONTH)
!
         FISCAL_LOAD_B4_SALES_V = FISCAL_LOAD_B4_SALES_V + &
                                         MONTHLY_LOAD_B4_SALES(R_MONTH)
!
         FISCAL_LOAD_AFTER_SALES_V = FISCAL_LOAD_AFTER_SALES_V + &
                                      MONTHLY_LOAD_AFTER_SALES(R_MONTH)
!
         FISCAL_PRO_COST_B4_SALES_V = FISCAL_PRO_COST_B4_SALES_V + &
                                     MONTHLY_PRO_COST_B4_SALES(R_MONTH)
       FISCAL_PRO_COST_AFTER_SALES_V = FISCAL_PRO_COST_AFTER_SALES_V + &
                                  MONTHLY_PRO_COST_AFTER_SALES(R_MONTH)

! 6/22/98.
         TOTAL_LOAD_BEFORE_HYDRO = 0.
         TOTAL_HYDRO_GENERATION = 0.
         TOTAL_HYDRO_CAPACITY = 0.
         TOTAL_HYDRO_ROR = 0.
         TOTAL_EFFECTIVE_CAPACITY = 0.
         TOTAL_NATIVE_COST = 0.
         TOTAL_SPINNING_MWH = 0.
         TOTAL_NON_COIN_PEAK = 0.
         TOTAL_NON_COIN_BASE = 0.
!         
         DO I = LOWER_TRANS_GROUP,UPPER_TRANS_GROUP
            IF(YES_TRANSACT_MONTHLY_REPORTS) THEN
!
               TG = GET_TRANS_GROUP_INDEX(I)
               DATA_BASE = GET_DATA_BASE_FOR_TRANS(TG,DAY_TYPE)
!
               IF(M_PURCHASE_ENERGY(I) > 0.) THEN
                  AVE_BUY = M_PURCHASE_COSTS(I) / M_PURCHASE_ENERGY(I)
               ELSE
                  AVE_BUY = 0.
               ENDIF
               IF(M_SALES_ENERGY(I) > 0.) THEN
                  AVE_SELL = M_SALES_REVENUES(I) / M_SALES_ENERGY(I)
               ELSE
                  AVE_SELL = 0.
               ENDIF
               IF(M_MONTHLY_LOAD_B4_SALES(I) > 0.) THEN
                  AVE_B4 = M_MONTHLY_PRO_COST_B4_SALES(I) / &
                                             M_MONTHLY_LOAD_B4_SALES(I)
               ELSE
                  AVE_B4 = 0.
               ENDIF
               IF(M_MONTHLY_LOAD_AFTER_SALES(I) > 0.) THEN
                  AVE_AFTER = M_MONTHLY_PRO_COST_AFTER_SALES(I) / & 
                                          M_MONTHLY_LOAD_AFTER_SALES(I)
               ELSE
                  AVE_AFTER = 0.
               ENDIF
               IF( ABS(M_MONTHLY_LOAD_AFTER_SALES(I) - &
                                 M_MONTHLY_LOAD_B4_SALES(I)) > 0.) THEN
                  MONTHLY_INCREMENTAL_COST = &
                    (M_MONTHLY_PRO_COST_AFTER_SALES(I) - &
                                      M_MONTHLY_PRO_COST_B4_SALES(I))/ &
                    (M_MONTHLY_LOAD_AFTER_SALES(I) - &
                                            M_MONTHLY_LOAD_B4_SALES(I))
               ELSE
                  MONTHLY_INCREMENTAL_COST = 0.
               ENDIF
!
               MONTHLY_EFFECTIVE_CAPACITY = TRANS_GROUP_CAP(DATA_BASE) 

               IF(ECITIES) THEN
                  IF(I < 3) THEN
                     MONTHLY_EFFECTIVE_CAPACITY = &
                             MONTHLY_EFFECTIVE_CAPACITY + &
                                      RESERVE_CAPACITY(I) + &
                                            SUPPLEMENTAL_CAPACITY(I)
                  ELSE
                     MONTHLY_EFFECTIVE_CAPACITY = &
                             MONTHLY_EFFECTIVE_CAPACITY + &
                                      RESERVE_CAPACITY(1) + &
                                            SUPPLEMENTAL_CAPACITY(1) + &
                                      RESERVE_CAPACITY(2) + &
                                            SUPPLEMENTAL_CAPACITY(2)
                  ENDIF
               ENDIF

               IF(USE_TF_FILE) THEN
!
                  WH_MTH_ENERGY = GET_WH_MONTH_ENERGY(R_MONTH,I)
!

                  TEMP_TL_HYDRO_MWH = GET_MONTHLY_TL_HYDRO_MWH(I) + &
                                                        WH_MTH_ENERGY
                  TEMP_TL_HYDRO_MW = GET_MONTHLY_TL_HYDRO_MW(I) + &
                                                        WH_MTH_ENERGY
                  TEMP_TL_HYDRO_ROR = trans_ror_cap(I)

!

!               
!               
                  TEMP_TL_MWH = GET_MONTHLY_TL_MWH(I) + WH_MTH_ENERGY
!
                  TEMP_TL_HYDRO_MWH = GET_MONTHLY_TL_HYDRO_MWH(I) + &
                              trans_ror_cap(I)*R_HOURS_IN_MONTH + &
                                                        WH_MTH_ENERGY
                  TEMP_TL_PEAK = MAX(0., &
                    GET_TRANS_PEAK_AFTER_EL(I) - trans_ror_cap(I))
                  TEMP_TL_BASE = MAX(0., &
                    GET_TRANS_BASE_AFTER_EL(I) - trans_ror_cap(I))
!
               ELSE
                  TEMP_TL_MWH = 0.
                  TEMP_TL_HYDRO_MWH = 0.
                  TEMP_TL_HYDRO_MW = 0.
!               
                  TEMP_TL_PEAK = 0.
                  TEMP_TL_BASE = 0.
!               
               ENDIF
!
! MOVED INTO HOURLY BY GROUP
!
               TOTAL_SPINNING_MWH = TOTAL_SPINNING_MWH + &
                                                      M_SPINNING_MWH(I)
!

               TOTAL_UNSERVED = TOTAL_UNSERVED + M_UNSERVED_ENERGY(I)
               TOTAL_ABOVE_RESOURCES = TOTAL_ABOVE_RESOURCES + &
                                                   M_ABOVE_RESOURCES(I)
!               
               TOTAL_UNSERVED_COST = TOTAL_UNSERVED_COST + &
                                              M_UNSERVED_ENERGY_COST(I)
             TOTAL_COST_ABOVE_RESOURCES = TOTAL_COST_ABOVE_RESOURCES + &
                                         MARKET_COST_ABOVE_RESOURCES(I)
!               
               TBVars(R_MONTH,I,23)= M_PURCHASE_ENERGY(I)
               TBVars(R_MONTH,I,24)= M_SALES_ENERGY(I)
               TRANS_ANN_REC = RPTREC(TRANS_ANN_UNIT)
               call check_unit(TRANS_ANN_UNIT, "transobj:0027")
               ! msgtrmon.rpt (Monthly Summary)
               WRITE(TRANS_ANN_UNIT,REC= TRANS_ANN_REC) &
                    PRT_ENDPOINT(), &
                    FLOAT(FISCAL_YEAR), &
                    CL_MONTH_NAME(R_MONTH), &
                    MULTI_AREA_NAME(I), &
                    M_PURCHASE_ENERGY(I), &
                    M_PURCHASE_COSTS(I)/1000000., &
                    AVE_BUY, &
                    M_SALES_ENERGY(I), &
                    M_SALES_REVENUES(I)/1000000., &
                    AVE_SELL, &
                    M_MONTHLY_LOAD_B4_SALES(I), &
                    M_MONTHLY_PRO_COST_B4_SALES(I)/1000000., &
                    AVE_B4, &
                    M_MONTHLY_LOAD_AFTER_SALES(I), &
                    M_MONTHLY_PRO_COST_AFTER_SALES(I)/1000000., &
                    AVE_AFTER, &
                    MONTHLY_INCREMENTAL_COST, &
                    TEMP_TL_MWH, & ! LOAD BEFORE HYDRO
                    TEMP_TL_HYDRO_MWH, & ! HYDRO GENERATION
                    MONTHLY_EFFECTIVE_CAPACITY, & ! EFFECTIVE_CAPACITY
                    MONTH_NON_COIN_PEAK(I), & ! PEAK
                    TEMP_TL_BASE, & ! BASE
                    M_NATIVE_COST(I)/1000000., &
                    M_SPINNING_MWH(I), &
                    M_UNSERVED_ENERGY(I), &
                    M_UNSERVED_ENERGY_COST(I)/1000000., &
                    MARKET_COST_ABOVE_RESOURCES(I)/1000000., &
                    MONTH_COIN_PEAK(I), &
                    TEMP_TL_HYDRO_MW, &
                    TEMP_TL_HYDRO_ROR, &
                    M_ABOVE_RESOURCES(I)
               TRANS_ANN_REC = TRANS_ANN_REC + 1
            ENDIF
            M_ANNUAL_PURCHASE_ENERGY(I) = &
                                  M_ANNUAL_PURCHASE_ENERGY(I) + &
                                                   M_PURCHASE_ENERGY(I)
            M_ANNUAL_PURCHASE_COSTS(I) = M_ANNUAL_PURCHASE_COSTS(I) + &
                                                    M_PURCHASE_COSTS(I)
            M_ANNUAL_SALES_ENERGY(I) = M_ANNUAL_SALES_ENERGY(I) + &
                                                      M_SALES_ENERGY(I)
            M_ANNUAL_SALES_REVENUES(I) = M_ANNUAL_SALES_REVENUES(I) + &
                                                    M_SALES_REVENUES(I)
            M_ANNUAL_NATIVE_COST(I) = M_ANNUAL_NATIVE_COST(I) + &
                                                   M_NATIVE_COST(I)
            M_ANNUAL_LOAD_B4_SALES(I) = M_ANNUAL_LOAD_B4_SALES(I) + &
                                             M_MONTHLY_LOAD_B4_SALES(I)
            M_ANNUAL_LOAD_AFTER_SALES(I) = &
                             M_ANNUAL_LOAD_AFTER_SALES(I) + &
                                          M_MONTHLY_LOAD_AFTER_SALES(I)
            M_ANNUAL_PRO_COST_B4_SALES(I) = &
                             M_ANNUAL_PRO_COST_B4_SALES(I) + &
                                         M_MONTHLY_PRO_COST_B4_SALES(I)
            M_ANNUAL_PRO_COST_AFTER_SALES(I) = &
                                M_ANNUAL_PRO_COST_AFTER_SALES(I) + &
                                      M_MONTHLY_PRO_COST_AFTER_SALES(I)
            MONTH_NON_COIN_PEAK(0) = MONTH_NON_COIN_PEAK(0) + &
                                                 MONTH_NON_COIN_PEAK(I)
!     
            ANNUAL_TL_MWH(I) = ANNUAL_TL_MWH(I) + TEMP_TL_MWH
!            
            ANNUAL_TL_PEAK(I) = &
                          MAX(ANNUAL_TL_PEAK(I),MONTH_NON_COIN_PEAK(0))
            ANNUAL_TL_BASE(I) = MIN(ANNUAL_TL_BASE(I),TEMP_TL_BASE)
!            
            ANNUAL_TL_HYDRO_MWH(I) = ANNUAL_TL_HYDRO_MWH(I) + &
                                                      TEMP_TL_HYDRO_MWH
            ANNUAL_TL_HYDRO_MW(I) = ANNUAL_TL_HYDRO_MW(I) + &
                                                       TEMP_TL_HYDRO_MW
            ANNUAL_TL_HYDRO_ROR(I) = ANNUAL_TL_HYDRO_ROR(I) + & 
                                                      TEMP_TL_HYDRO_ROR
            ANNUAL_EFFECTIVE_CAPACITY(I) = &
                               ANNUAL_EFFECTIVE_CAPACITY(I) + &
                                             MONTHLY_EFFECTIVE_CAPACITY
            ANNUAL_SPINNING_MWH(I) = ANNUAL_SPINNING_MWH(I) + &
                                                      M_SPINNING_MWH(I)
            ANNUAL_UNSERVED_ENERGY(I,0) = &
                             ANNUAL_UNSERVED_ENERGY(I,0) + &
                                                   M_UNSERVED_ENERGY(I)
            ANNUAL_UNSERVED_ENERGY(I,R_MONTH) = &
                             ANNUAL_UNSERVED_ENERGY(I,R_MONTH) + &
                                                   M_UNSERVED_ENERGY(I)
            ANNUAL_ABOVE_RESOURCES(I,R_MONTH) = &
                             ANNUAL_ABOVE_RESOURCES(I,R_MONTH) + &
                                                   M_ABOVE_RESOURCES(I)
            ANNUAL_ABOVE_RESOURCES(I,0) = &
                             ANNUAL_ABOVE_RESOURCES(I,0) + &
                                                   M_ABOVE_RESOURCES(I)
            ANNUAL_UNSERVED_ENERGY_COST(I) = &
                    ANNUAL_UNSERVED_ENERGY_COST(I) + &
                                              M_UNSERVED_ENERGY_COST(I)
            ANNUAL_COST_ABOVE_RESOURCES(I,0) = &
                    ANNUAL_COST_ABOVE_RESOURCES(I,0) + &
                                         MARKET_COST_ABOVE_RESOURCES(I)
            ANNUAL_COST_ABOVE_RESOURCES(I,R_MONTH) = &
                    ANNUAL_COST_ABOVE_RESOURCES(I,R_MONTH) + &
                                         MARKET_COST_ABOVE_RESOURCES(I)
!
            FISCAL_PURCHASE_ENERGY(I) = &
                                  FISCAL_PURCHASE_ENERGY(I) + &
                                                   M_PURCHASE_ENERGY(I)
            FISCAL_PURCHASE_COSTS(I) = FISCAL_PURCHASE_COSTS(I) + &
                                                    M_PURCHASE_COSTS(I)
            FISCAL_SALES_ENERGY(I) = FISCAL_SALES_ENERGY(I) + &
                                                      M_SALES_ENERGY(I)
            FISCAL_SALES_REVENUES(I) = FISCAL_SALES_REVENUES(I) + &
                                                    M_SALES_REVENUES(I)
            FISCAL_NATIVE_COST(I) = FISCAL_NATIVE_COST(I) + &
                                                   M_NATIVE_COST(I)
            FISCAL_LOAD_B4_SALES(I) = FISCAL_LOAD_B4_SALES(I) + &
                                             M_MONTHLY_LOAD_B4_SALES(I)
            FISCAL_LOAD_AFTER_SALES(I) = &
                             FISCAL_LOAD_AFTER_SALES(I) + &
                                          M_MONTHLY_LOAD_AFTER_SALES(I)
            FISCAL_PRO_COST_B4_SALES(I) = &
                             FISCAL_PRO_COST_B4_SALES(I) + &
                                         M_MONTHLY_PRO_COST_B4_SALES(I)
            FISCAL_PRO_COST_AFTER_SALES(I) = &
                                FISCAL_PRO_COST_AFTER_SALES(I) + &
                                      M_MONTHLY_PRO_COST_AFTER_SALES(I)

!     
            FISCAL_TL_MWH(I) = FISCAL_TL_MWH(I) + TEMP_TL_MWH
!            
            FISCAL_TL_PEAK(I) = &
                          MAX(FISCAL_TL_PEAK(I),MONTH_NON_COIN_PEAK(0))
            FISCAL_TL_BASE(I) = MIN(FISCAL_TL_BASE(I),TEMP_TL_BASE)
!            
            FISCAL_TL_HYDRO_MWH(I) = FISCAL_TL_HYDRO_MWH(I) + &
                                                      TEMP_TL_HYDRO_MWH
            FISCAL_TL_HYDRO_MW(I) = FISCAL_TL_HYDRO_MW(I) + &
                                                       TEMP_TL_HYDRO_MW
            FISCAL_TL_HYDRO_ROR(I) = FISCAL_TL_HYDRO_ROR(I) + & 
                                                      TEMP_TL_HYDRO_ROR
            FISCAL_EFFECTIVE_CAPACITY(I) = &
                               FISCAL_EFFECTIVE_CAPACITY(I) + &
                                             MONTHLY_EFFECTIVE_CAPACITY
            FISCAL_SPINNING_MWH(I) = FISCAL_SPINNING_MWH(I) + &
                                                      M_SPINNING_MWH(I)
            FISCAL_UNSERVED_ENERGY(I) = &
                             FISCAL_UNSERVED_ENERGY(I) + &
                                                   M_UNSERVED_ENERGY(I)
            FISCAL_ABOVE_RESOURCES(I) = &
                             FISCAL_ABOVE_RESOURCES(I) + &
                                                   M_ABOVE_RESOURCES(I)
            FISCAL_UNSERVED_ENERGY_COST(I) = &
                    FISCAL_UNSERVED_ENERGY_COST(I) + &
                                              M_UNSERVED_ENERGY_COST(I)
            FISCAL_COST_ABOVE_RESOURCES(I) = &
                    FISCAL_COST_ABOVE_RESOURCES(I) + &
                                         MARKET_COST_ABOVE_RESOURCES(I)
!
            TOTAL_LOAD_BEFORE_HYDRO = TOTAL_LOAD_BEFORE_HYDRO  + &
                                                            TEMP_TL_MWH
            TOTAL_HYDRO_GENERATION = TOTAL_HYDRO_GENERATION + &
                                                      TEMP_TL_HYDRO_MWH
            TOTAL_HYDRO_CAPACITY = TOTAL_HYDRO_CAPACITY + &
                                                       TEMP_TL_HYDRO_MW
            TOTAL_HYDRO_ROR = TOTAL_HYDRO_ROR + TEMP_TL_HYDRO_ROR
            TOTAL_EFFECTIVE_CAPACITY = TOTAL_EFFECTIVE_CAPACITY + &
                                             MONTHLY_EFFECTIVE_CAPACITY
            TOTAL_NATIVE_COST = TOTAL_NATIVE_COST + &
                                                   M_NATIVE_COST(I)
            TOTAL_NON_COIN_PEAK = TOTAL_NON_COIN_PEAK + TEMP_TL_PEAK
            TOTAL_NON_COIN_BASE = TOTAL_NON_COIN_BASE + TEMP_TL_BASE
         ENDDO ! TRANS_GROUPS
         IF(MONTH_COIN_PEAK(0) > ANNUAL_COIN_PEAK(0)) THEN
            DO I = 0, UPPER_TRANS_GROUP
               ANNUAL_COIN_PEAK(I) = MONTH_COIN_PEAK(I)
            ENDDO
         ENDIF
         IF(MONTH_COIN_PEAK(0) > FISCAL_COIN_PEAK(0)) THEN
            DO I = 0, UPPER_TRANS_GROUP
               FISCAL_COIN_PEAK(I) = MONTH_COIN_PEAK(I)
            ENDDO
         ENDIF
!         
! 09/02/01. TRANSFER PRICING.
!
         MONTHLY_TRANSFER_MWH(1,0) = &
                                  MONTHLY_TRANSFER_MWH(1,0) + &
                                        MONTHLY_TRANSFER_MWH(1,R_MONTH)
         MONTHLY_TRANSFER_REVENUE(1,0) = &
                            MONTHLY_TRANSFER_REVENUE(1,0) + &
                                    MONTHLY_TRANSFER_REVENUE(1,R_MONTH)
!
         MONTHLY_TRANSFER_MWH(2,0) = &
                                  MONTHLY_TRANSFER_MWH(2,0) + &
                                        MONTHLY_TRANSFER_MWH(2,R_MONTH)
         MONTHLY_TRANSFER_REVENUE(2,0) = &
                            MONTHLY_TRANSFER_REVENUE(2,0) + &
                                    MONTHLY_TRANSFER_REVENUE(2,R_MONTH)
!
! ENDOF 6/22/98.
!
         IF(PURCHASE_ENERGY(R_MONTH) > 0.) THEN
            AVE_BUY = PURCHASE_COSTS(R_MONTH) / PURCHASE_ENERGY(R_MONTH)
         ELSE
            AVE_BUY = 0.
         ENDIF
         IF(SALES_ENERGY(R_MONTH) > 0.) THEN
            AVE_SELL = SALES_REVENUE(R_MONTH) / SALES_ENERGY(R_MONTH)
         ELSE
            AVE_SELL = 0.
         ENDIF
         IF(MONTHLY_LOAD_B4_SALES(R_MONTH) > 0.) THEN
            AVE_B4 = MONTHLY_PRO_COST_B4_SALES(R_MONTH) / &
                                         MONTHLY_LOAD_B4_SALES(R_MONTH)
         ELSE
            AVE_B4 = 0.
         ENDIF
         IF(MONTHLY_LOAD_AFTER_SALES(R_MONTH) > 0.) THEN
            AVE_AFTER = MONTHLY_PRO_COST_AFTER_SALES(R_MONTH) / &
                                      MONTHLY_LOAD_AFTER_SALES(R_MONTH)
         ELSE
            AVE_AFTER = 0.
         ENDIF
         IF( ABS(MONTHLY_LOAD_AFTER_SALES(R_MONTH) - &
                             MONTHLY_LOAD_B4_SALES(R_MONTH)) > 0.) THEN
            MONTHLY_INCREMENTAL_COST = &
              (MONTHLY_PRO_COST_AFTER_SALES(R_MONTH) - &
                                  MONTHLY_PRO_COST_B4_SALES(R_MONTH))/ &
                 (MONTHLY_LOAD_AFTER_SALES(R_MONTH) - &
                                        MONTHLY_LOAD_B4_SALES(R_MONTH))
         ELSE
            MONTHLY_INCREMENTAL_COST = 0.
         ENDIF

         TBVars(R_MONTH,0,23)= PURCHASE_ENERGY(I)
         TBVars(R_MONTH,0,24)= SALES_ENERGY(I)
         IF(YES_TRANSACT_MONTHLY_REPORTS .AND. .NOT. FISCAL_ONLY) THEN
            TRANS_ANN_REC = RPTREC(TRANS_ANN_UNIT)
            ! msgtrmon.rpt (Monthly Summary)
            call check_unit(TRANS_ANN_UNIT, "transobj:0028")
            WRITE(TRANS_ANN_UNIT,REC= TRANS_ANN_REC) &
                    PRT_ENDPOINT(), &
                    FLOAT(FISCAL_YEAR), &
                    CL_MONTH_NAME(R_MONTH), &
                    MULTI_AREA_NAME(0), &
                    PURCHASE_ENERGY(R_MONTH), &
                    PURCHASE_COSTS(R_MONTH)/1000000., &
                    AVE_BUY, &
                    SALES_ENERGY(R_MONTH), &
                    SALES_REVENUE(R_MONTH)/1000000., &
                    AVE_SELL, &
                    MONTHLY_LOAD_B4_SALES(R_MONTH), &
                    MONTHLY_PRO_COST_B4_SALES(R_MONTH)/1000000., &
                    AVE_B4, &
                    MONTHLY_LOAD_AFTER_SALES(R_MONTH), &
                    MONTHLY_PRO_COST_AFTER_SALES(R_MONTH)/1000000., &
                    AVE_AFTER, &
                    MONTHLY_INCREMENTAL_COST, &
                    TOTAL_LOAD_BEFORE_HYDRO, & ! LOAD BEFORE HYDRO
                    TOTAL_HYDRO_GENERATION, & ! HYDRO GENERATION
                    TOTAL_EFFECTIVE_CAPACITY, & ! EFFECTIVE_CAPACITY
                    MONTH_NON_COIN_PEAK(0), & ! PEAK
                    TOTAL_NON_COIN_BASE, & ! BASE
                    TOTAL_NATIVE_COST/1000000., &
                    TOTAL_SPINNING_MWH, &
                    TOTAL_UNSERVED, &
                    TOTAL_UNSERVED_COST/1000000., &
                    TOTAL_COST_ABOVE_RESOURCES/1000000., &
                    MONTH_COIN_PEAK(0), &
                    TOTAL_HYDRO_CAPACITY, &
                    TOTAL_HYDRO_ROR, &
                    TOTAL_ABOVE_RESOURCES
               TRANS_ANN_REC = TRANS_ANN_REC + 1
!     
         ENDIF
         IF(YES_TRANSACT_MONTHLY_REPORTS) THEN
!
!
               DAY_WEEK_REC = RPTREC(DAY_WEEK_UNIT)
               call check_unit(DAY_WEEK_UNIT, "transobj:0029")
               WRITE(DAY_WEEK_UNIT,REC=DAY_WEEK_REC) &
                    PRT_ENDPOINT(), &
                    FLOAT(CURRENT_YEAR), &
                    CL_MONTH_NAME(R_MONTH), &
                    WRITE_DAY_OF_WEEK
               DAY_WEEK_REC = DAY_WEEK_REC + 1
         ENDIF
         IF(YES_MONTHLY_TRANC_REPORT()) THEN
            TEMP_L = WRITE_TRANC_MONTHLY_SUMMARY( &
                                    R_MONTH, &
                                    CL_MONTH_NAME(R_MONTH), &
                                    R_HOURS_IN_MONTH)
         ENDIF
         IF(YES_TRANSACT_DAILY_REPORTS) THEN
!
!
            DO J = 1, NUM_PRODUCTS
!
!
               TEMP_PRODUCT_HOURS = PRODUCT_HOURS(J)
!               
               DO HR = 1, TEMP_PRODUCT_HOURS
!
!
                  IF(PRODUCT_HOURS(J) <= 1.) CYCLE
!
                  DEVIATIONS_FROM_MEAN = &
                          PRODUCT_DAILY_RETURN(0,J,HR) - &
                                               PRODUCT_MEAN_RETURN(0,J)
                  SUM_SQUARED_DEVIATIONS(0,J)  = &
                       SUM_SQUARED_DEVIATIONS(0,J) + & 
                              DEVIATIONS_FROM_MEAN*DEVIATIONS_FROM_MEAN
!                     
                  DO I = 1, UPPER_TRANS_GROUP
                     DEVIATIONS_FROM_MEAN = &
                          PRODUCT_DAILY_RETURN(I,J,HR) - &
                                               PRODUCT_MEAN_RETURN(I,J)
                     SUM_SQUARED_DEVIATIONS(I,J)  = &
                       SUM_SQUARED_DEVIATIONS(I,J) + & 
                              DEVIATIONS_FROM_MEAN*DEVIATIONS_FROM_MEAN
                  ENDDO
               ENDDO
            ENDDO
!     
            DO I = 0, UPPER_TRANS_GROUP 
!         
               DO J = 1, NUM_PRODUCTS
                  IF(I == 0) THEN
                     IF(PRODUCT_HOURS (J) > 1.) THEN
!
                        ANNUAL_PRODUCT_PRICE(0,J) = &
                                ANNUAL_PRODUCT_PRICE(0,J) + &
                                                     PRODUCT_PRICE(0,J)
                        ANNUAL_PRODUCT_QUANTITY(0,J) = &
                                ANNUAL_PRODUCT_QUANTITY(0,J) + & 
                                                  PRODUCT_QUANTITY(0,J)
                        ANNUAL_PRODUCT_HEATRATE(0,J) = &
                                ANNUAL_PRODUCT_HEATRATE(0,J) + &
                                                  PRODUCT_HEATRATE(0,J)
                        ANNUAL_PRODUCT_MARGINAL_FUEL(0,J) = &
                                ANNUAL_PRODUCT_MARGINAL_FUEL(0,J) + &
                                            PRODUCT_MARGINAL_FUEL(0,J)
                        ANNUAL_PRODUCT_FUEL_PRICE(0,J) = &
                                ANNUAL_PRODUCT_FUEL_PRICE(0,J) + & 
                                                PRODUCT_FUEL_PRICE(0,J)
                        ANNUAL_PRODUCT_HOURS(J) = &
                                ANNUAL_PRODUCT_HOURS(J) + &
                                                       PRODUCT_HOURS(J)
!
                        PRODUCT_PRICE(0,J) = &
                                  PRODUCT_PRICE(0,J) / PRODUCT_HOURS(J)
                        PRODUCT_VOLATILITY(0,J) = &  
                          ( (1./(PRODUCT_HOURS(J)-1.))* &
                                      SUM_SQUARED_DEVIATIONS(0,J) )**.5
                        PRODUCT_HEATRATE(0,J) = &
                             PRODUCT_HEATRATE(0,J) / & 
                                 (PRODUCT_HOURS(J) * UPPER_TRANS_GROUP)
                        PRODUCT_MARGINAL_FUEL(0,J) = &
                             PRODUCT_MARGINAL_FUEL(0,J) / &
                                (PRODUCT_MARGINAL_FUEL(0,J) * &
                                                     UPPER_TRANS_GROUP)
                        PRODUCT_FUEL_PRICE(0,J) = &
                             PRODUCT_FUEL_PRICE(0,J) / &
                                 (PRODUCT_HOURS(J) * UPPER_TRANS_GROUP)
                     ELSE
                        PRODUCT_VOLATILITY(0,J) = 0.
                     ENDIF
                     DAILY_PRODUCTS_REC = RPTREC(DAILY_PRODUCTS_UNIT)
                     call check_unit(DAILY_PRODUCTS_UNIT, "transobj:0031")
                     WRITE(DAILY_PRODUCTS_UNIT,REC=DAILY_PRODUCTS_REC) &
                       PRT_ENDPOINT(), &
                       FLOAT(CURRENT_YEAR), &
                       CL_MONTH_NAME(R_MONTH), &
                       MULTI_AREA_NAME(0), &
                       LOCAL_PRODUCT_TYPE(J), &
                       PRODUCT_HOURS(J), &
                       PRODUCT_PRICE(0,J), &
                       PRODUCT_VOLATILITY(0,J), &
                       PRODUCT_QUANTITY(0,J), &
                       PRODUCT_MEAN_RETURN(0,J), &
                       PRODUCT_HEATRATE(0,J), &
                       PRODUCT_FUEL_PRICE(0,J), &
                       EV_DATA_SOURCE, &
                       PRODUCT_MARGINAL_FUEL(0,J)*100.
                     DAILY_PRODUCTS_REC = DAILY_PRODUCTS_REC + 1
!
                  ELSE ! I > 0
!
                     ANNUAL_PRODUCT_PRICE(I,J) = &
                                ANNUAL_PRODUCT_PRICE(I,J) + &
                                                     PRODUCT_PRICE(I,J)
                     ANNUAL_PRODUCT_QUANTITY(I,J) = &
                                ANNUAL_PRODUCT_QUANTITY(I,J) + & 
                                                  PRODUCT_QUANTITY(I,J)
                     ANNUAL_PRODUCT_HEATRATE(I,J) = &
                                ANNUAL_PRODUCT_HEATRATE(I,J) + &
                                                  PRODUCT_HEATRATE(I,J)
                     ANNUAL_PRODUCT_MARGINAL_FUEL(I,J) = &
                                ANNUAL_PRODUCT_MARGINAL_FUEL(I,J) + &
                                             PRODUCT_MARGINAL_FUEL(I,J)
                     
                     ANNUAL_PRODUCT_FUEL_PRICE(I,J) = &
                                ANNUAL_PRODUCT_FUEL_PRICE(I,J) + &
                                                PRODUCT_FUEL_PRICE(I,J)

!
                     IF(PRODUCT_HOURS (J) > 1.) THEN
                        PRODUCT_PRICE(I,J) = &
                                  PRODUCT_PRICE(I,J) / PRODUCT_HOURS(J)
                        PRODUCT_VOLATILITY(I,J) = &
                          ( (1./(PRODUCT_HOURS(J)-1.))* &
                                      SUM_SQUARED_DEVIATIONS(I,J) )**.5
                        PRODUCT_HEATRATE(I,J) = &
                          PRODUCT_HEATRATE(I,J)/PRODUCT_HOURS(J)
                        PRODUCT_MARGINAL_FUEL(I,J) = &
                          PRODUCT_MARGINAL_FUEL(I,J)/PRODUCT_HOURS(J)
                        PRODUCT_FUEL_PRICE(I,J) = &
                          PRODUCT_FUEL_PRICE(I,J)/PRODUCT_HOURS(J)
                     ELSE
                        PRODUCT_VOLATILITY(I,J) = 0.
                        PRODUCT_HEATRATE(I,J) = 0.
                        PRODUCT_MARGINAL_FUEL(I,J) = 0.
                        PRODUCT_FUEL_PRICE(I,J) = 0.
                     ENDIF
                     DAILY_PRODUCTS_REC = RPTREC(DAILY_PRODUCTS_UNIT)
                     call check_unit(DAILY_PRODUCTS_UNIT, "transobj:0033")
                     WRITE(DAILY_PRODUCTS_UNIT,REC=DAILY_PRODUCTS_REC) &
                       PRT_ENDPOINT(), &
                       FLOAT(CURRENT_YEAR), &
                       CL_MONTH_NAME(R_MONTH), &
                       MULTI_AREA_NAME(I), &
                       LOCAL_PRODUCT_TYPE(J), &
                       PRODUCT_HOURS(J), &
                       PRODUCT_PRICE(I,J), &
                       PRODUCT_VOLATILITY(I,J), &
                       PRODUCT_QUANTITY(I,J), &
                       PRODUCT_MEAN_RETURN(I,J), &
                       PRODUCT_HEATRATE(I,J), &
                       PRODUCT_FUEL_PRICE(I,J), &
                       EV_DATA_SOURCE, &
                       PRODUCT_MARGINAL_FUEL(I,J)*100.
                     DAILY_PRODUCTS_REC = DAILY_PRODUCTS_REC + 1
                  ENDIF ! I > 0
               ENDDO ! PRODUCTS
            ENDDO ! TRANS GROUPS
         ENDIF ! YES DAILY PRODUCTS REPORT
!

         IF(R_MONTH == LAST_MONTHLY_TRANSACT ) THEN
            IF(YES_TRANSACT_MONTHLY_REPORTS .AND. &
                                                .NOT. FISCAL_ONLY) THEN
!
               TOTAL_LOAD_BEFORE_HYDRO = 0.
               TOTAL_HYDRO_GENERATION = 0.
               TOTAL_HYDRO_CAPACITY = 0.
               TOTAL_HYDRO_ROR = 0.
               TOTAL_EFFECTIVE_CAPACITY = 0.
               TOTAL_NATIVE_COST = 0.
               TOTAL_SPINNING_MWH = 0.
               TOTAL_NON_COIN_PEAK = 0.
               TOTAL_NON_COIN_BASE = 0.
!
               TOTAL_UNSERVED = 0.
               TOTAL_ABOVE_RESOURCES = 0.
               TOTAL_UNSERVED_COST = 0.
               TOTAL_COST_ABOVE_RESOURCES = 0.
!
               DO I = LOWER_TRANS_GROUP,UPPER_TRANS_GROUP
                  IF(M_ANNUAL_PURCHASE_ENERGY(I) > 0.) THEN
                     AVE_BUY = M_ANNUAL_PURCHASE_COSTS(I) / &
                                            M_ANNUAL_PURCHASE_ENERGY(I)
                  ELSE
                     AVE_BUY = 0.
                  ENDIF
                  IF(M_ANNUAL_SALES_ENERGY(I) > 0.) THEN
                     AVE_SELL = M_ANNUAL_SALES_REVENUES(I) / &
                                               M_ANNUAL_SALES_ENERGY(I)
                  ELSE
                     AVE_SELL = 0.
                  ENDIF
!            
                  IF(M_ANNUAL_LOAD_B4_SALES(I) > 0.) THEN
                     AVE_B4 = M_ANNUAL_PRO_COST_B4_SALES(I) / & 
                                              M_ANNUAL_LOAD_B4_SALES(I)
                  ELSE
                     AVE_B4 = 0.
                  ENDIF
!            
                  IF(M_ANNUAL_LOAD_AFTER_SALES(I) > 0.) THEN
                     AVE_AFTER = M_ANNUAL_PRO_COST_AFTER_SALES(I) / &
                                           M_ANNUAL_LOAD_AFTER_SALES(I)
                  ELSE
                     AVE_AFTER = 0.
                  ENDIF
                  IF( ABS(M_ANNUAL_LOAD_AFTER_SALES(I) - &
                                  M_ANNUAL_LOAD_B4_SALES(I)) > 0.) THEN
                     MONTHLY_INCREMENTAL_COST = &
                       (M_ANNUAL_PRO_COST_AFTER_SALES(I) - & 
                                       M_ANNUAL_PRO_COST_B4_SALES(I))/ &
                       (M_ANNUAL_LOAD_AFTER_SALES(I) - &
                                             M_ANNUAL_LOAD_B4_SALES(I))
                  ELSE
                     MONTHLY_INCREMENTAL_COST = 0.
                  ENDIF
!

                  TOTAL_UNSERVED = TOTAL_UNSERVED + &
                                            ANNUAL_UNSERVED_ENERGY(I,0)
                  TOTAL_ABOVE_RESOURCES = TOTAL_ABOVE_RESOURCES + &
                                            ANNUAL_ABOVE_RESOURCES(I,0)
                  TOTAL_UNSERVED_COST = TOTAL_UNSERVED_COST + &
                                       ANNUAL_UNSERVED_ENERGY_COST(I)
                  TOTAL_COST_ABOVE_RESOURCES = &
                       TOTAL_COST_ABOVE_RESOURCES + &
                                       ANNUAL_COST_ABOVE_RESOURCES(I,0)
!            
                  TBVars(13,I,23)= M_ANNUAL_PURCHASE_ENERGY(I)
                  TBVars(13,I,24)= M_ANNUAL_SALES_ENERGY(I)
                  TRANS_ANN_REC = RPTREC(TRANS_ANN_UNIT)
                  ! msgtrmon.rpt (Monthly Summary)
                  call check_unit(TRANS_ANN_UNIT, "transobj:0034")
                  WRITE(TRANS_ANN_UNIT,REC= TRANS_ANN_REC) &
                    PRT_ENDPOINT(), &
                    FLOAT(FISCAL_YEAR), &
                    CL_MONTH_NAME(13), &
                    MULTI_AREA_NAME(I), &
                    M_ANNUAL_PURCHASE_ENERGY(I), &
                    M_ANNUAL_PURCHASE_COSTS(I)/1000000., &
                    AVE_BUY, &
                    M_ANNUAL_SALES_ENERGY(I), &
                    M_ANNUAL_SALES_REVENUES(I)/1000000., &
                    AVE_SELL, &
                    M_ANNUAL_LOAD_B4_SALES(I), &
                    M_ANNUAL_PRO_COST_B4_SALES(I)/1000000., &
                    AVE_B4, &
                    M_ANNUAL_LOAD_AFTER_SALES(I), &
                    M_ANNUAL_PRO_COST_AFTER_SALES(I)/1000000., &
                    AVE_AFTER, &
                    MONTHLY_INCREMENTAL_COST, &
                    ANNUAL_TL_MWH(I), &
                    ANNUAL_TL_HYDRO_MWH(I), &
                    ANNUAL_EFFECTIVE_CAPACITY(I)/12., &
                    ANNUAL_TL_PEAK(I), &
                    ANNUAL_TL_BASE(I), &
                    M_ANNUAL_NATIVE_COST(I)/1000000., &
                    ANNUAL_SPINNING_MWH(I), &
                    ANNUAL_UNSERVED_ENERGY(I,0), &
                    ANNUAL_UNSERVED_ENERGY_COST(I)/1000000., &
                    ANNUAL_COST_ABOVE_RESOURCES(I,0)/1000000., &
                    ANNUAL_COIN_PEAK(I), &
                    ANNUAL_TL_HYDRO_MW(I)/12., &
                    ANNUAL_TL_HYDRO_ROR(I)/12., &
                    ANNUAL_ABOVE_RESOURCES(I,0)

                  TRANS_ANN_REC = TRANS_ANN_REC + 1
!
                  TOTAL_LOAD_BEFORE_HYDRO = TOTAL_LOAD_BEFORE_HYDRO + &
                                                       ANNUAL_TL_MWH(I)
                  TOTAL_HYDRO_GENERATION = TOTAL_HYDRO_GENERATION + &
                                                 ANNUAL_TL_HYDRO_MWH(I)
                  TOTAL_HYDRO_CAPACITY = TOTAL_HYDRO_CAPACITY + &
                                                  ANNUAL_TL_HYDRO_MW(I)
                  TOTAL_HYDRO_ROR = TOTAL_HYDRO_ROR + &
                                                 ANNUAL_TL_HYDRO_ROR(I)
                 TOTAL_EFFECTIVE_CAPACITY = TOTAL_EFFECTIVE_CAPACITY + &
                                           ANNUAL_EFFECTIVE_CAPACITY(I)
                  TOTAL_NATIVE_COST = TOTAL_NATIVE_COST + &
                                            M_ANNUAL_NATIVE_COST(I)
                  TOTAL_SPINNING_MWH = TOTAL_SPINNING_MWH + &
                                                 ANNUAL_SPINNING_MWH(I)
                  TOTAL_NON_COIN_PEAK = TOTAL_NON_COIN_PEAK + & 
                                                      ANNUAL_TL_PEAK(I)
                  TOTAL_NON_COIN_BASE = TOTAL_NON_COIN_BASE + &
                                                      ANNUAL_TL_BASE(I)
               ENDDO
               IF(ANNUAL_PURCHASES > 0.) THEN
                  AVE_BUY = ANNUAL_PURCHASE_COST / ANNUAL_PURCHASES
               ELSE
                  AVE_BUY = 0.
               ENDIF
               IF(ANNUAL_SALES > 0.) THEN
                  AVE_SELL = ANNUAL_SALES_REVENUE / ANNUAL_SALES
               ELSE
                  AVE_SELL = 0.
               ENDIF
!            
               IF(ANNUAL_LOAD_B4_SALES > 0.) THEN
                  AVE_B4 = ANNUAL_PRO_COST_B4_SALES / &
                                                   ANNUAL_LOAD_B4_SALES 
               ELSE
                  AVE_B4 = 0.
               ENDIF
!            
               IF(ANNUAL_LOAD_AFTER_SALES > 0.) THEN
                  AVE_AFTER = ANNUAL_PRO_COST_AFTER_SALES / &
                                                ANNUAL_LOAD_AFTER_SALES
               ELSE
                  AVE_AFTER = 0.
               ENDIF
               IF( ABS(ANNUAL_LOAD_AFTER_SALES - &
                                   ANNUAL_LOAD_B4_SALES) > 0.) THEN
                  MONTHLY_INCREMENTAL_COST = &
                    (ANNUAL_PRO_COST_AFTER_SALES - & 
                                            ANNUAL_PRO_COST_B4_SALES)/ &
                    (ANNUAL_LOAD_AFTER_SALES - ANNUAL_LOAD_B4_SALES)
               ELSE
                  MONTHLY_INCREMENTAL_COST = 0.
               ENDIF
!

!            
               TBVars(13,0,23)= ANNUAL_PURCHASES
               TBVars(13,0,24)= ANNUAL_SALES
               TRANS_ANN_REC = RPTREC(TRANS_ANN_UNIT)
               ! msgtrmon.rpt (Monthly Summary)
               call check_unit(TRANS_ANN_UNIT, "transobj:0035")
               WRITE(TRANS_ANN_UNIT,REC= TRANS_ANN_REC) &
                   PRT_ENDPOINT(), &
                   FLOAT(FISCAL_YEAR), &
                   CL_MONTH_NAME(13), &
                   MULTI_AREA_NAME(0), &
                   ANNUAL_PURCHASES, &
                   ANNUAL_PURCHASE_COST/1000000., &
                   AVE_BUY, &
                   ANNUAL_SALES, &
                   ANNUAL_SALES_REVENUE/1000000., &
                   AVE_SELL, &
                   ANNUAL_LOAD_B4_SALES, &
                   ANNUAL_PRO_COST_B4_SALES/1000000., &
                   AVE_B4, &
                   ANNUAL_LOAD_AFTER_SALES, &
                   ANNUAL_PRO_COST_AFTER_SALES/1000000., &
                   AVE_AFTER, &
                   MONTHLY_INCREMENTAL_COST, &
                   TOTAL_LOAD_BEFORE_HYDRO, & ! LOAD BEFORE HYDRO
                   TOTAL_HYDRO_GENERATION, & ! HYDRO GENERATION
                   TOTAL_EFFECTIVE_CAPACITY/12., & ! EFFECTIVE_CAPACITY
                   TOTAL_NON_COIN_PEAK, & ! PEAK
                   TOTAL_NON_COIN_BASE, & ! BASE
                   TOTAL_NATIVE_COST/1000000., &
                   TOTAL_SPINNING_MWH, &
                   TOTAL_UNSERVED, &
                   TOTAL_UNSERVED_COST/1000000., &
                   TOTAL_COST_ABOVE_RESOURCES/1000000., &
                   ANNUAL_COIN_PEAK(0), &
                   TOTAL_HYDRO_CAPACITY/12., &
                   TOTAL_HYDRO_ROR/12., &
                   TOTAL_ABOVE_RESOURCES
               TRANS_ANN_REC = TRANS_ANN_REC + 1
               IF(ANNUAL_PURCHASES > 0.) THEN
                  AVERAGE_COST = ANNUAL_PURCHASE_COST/ ANNUAL_PURCHASES
               ELSE
                  AVERAGE_COST = 0.   
               ENDIF
               IF(ANNUAL_SALES > 0.) THEN
                  AVERAGE_COST = ANNUAL_SALES_REVENUE/ANNUAL_SALES
               ELSE
                  AVERAGE_COST = 0.   
               ENDIF
            ENDIF ! MONTHLY REPORT ! MOVED UP 04/08/04.

            IF(YES_TRANSACT_DAILY_REPORTS) THEN
               DO I = 0, UPPER_TRANS_GROUP
                  DO J = 1, NUM_PRODUCTS
                     IF(I == 0) THEN
                        IF(ANNUAL_PRODUCT_HOURS (J) > 1.) THEN
                           ANNUAL_PRODUCT_PRICE(0,J) = &
                                  ANNUAL_PRODUCT_PRICE(0,J) / &
                                                ANNUAL_PRODUCT_HOURS(J)
                           ANNUAL_PRODUCT_HEATRATE(0,J) = &
                                  ANNUAL_PRODUCT_HEATRATE(0,J) / &
                                                ANNUAL_PRODUCT_HOURS(J)
                           ANNUAL_PRODUCT_MARGINAL_FUEL(0,J) = &
                                  ANNUAL_PRODUCT_MARGINAL_FUEL(0,J) / &
                                                ANNUAL_PRODUCT_HOURS(J)
                           ANNUAL_PRODUCT_FUEL_PRICE(0,J) = &
                                  ANNUAL_PRODUCT_FUEL_PRICE(0,J) / &
                                                ANNUAL_PRODUCT_HOURS(J)
                        ELSE
!                             PRODUCT_VOLATILITY(0,J) = 0.
                        ENDIF
                        DAILY_PRODUCTS_REC = RPTREC(DAILY_PRODUCTS_UNIT)
                   call check_unit(DAILY_PRODUCTS_UNIT, "transobj:0036")
                        WRITE(DAILY_PRODUCTS_UNIT, &
                                        REC=DAILY_PRODUCTS_REC) &
                             PRT_ENDPOINT(), &
                             FLOAT(CURRENT_YEAR), &
                             CL_MONTH_NAME(13), &
                             MULTI_AREA_NAME(0), &
                             LOCAL_PRODUCT_TYPE(J), &
                             ANNUAL_PRODUCT_HOURS(J), &
                             ANNUAL_PRODUCT_PRICE(0,J), &
                             PRODUCT_VOLATILITY(0,J), &
                             ANNUAL_PRODUCT_QUANTITY(0,J), &
                             PRODUCT_MEAN_RETURN(0,J), &
                             ANNUAL_PRODUCT_HEATRATE(0,J), &
                             ANNUAL_PRODUCT_FUEL_PRICE(0,J), &
                             EV_DATA_SOURCE, &
                             ANNUAL_PRODUCT_MARGINAL_FUEL(0,J)*100.
                        DAILY_PRODUCTS_REC = DAILY_PRODUCTS_REC + 1
                     ELSE ! I > 0
!         
                        IF(ANNUAL_PRODUCT_HOURS (J) > 1.) THEN
                           ANNUAL_PRODUCT_PRICE(I,J) = &
                                  ANNUAL_PRODUCT_PRICE(I,J) / &
                                                ANNUAL_PRODUCT_HOURS(J)
                           ANNUAL_PRODUCT_HEATRATE(I,J) = &
                                  ANNUAL_PRODUCT_HEATRATE(I,J) / &
                                                ANNUAL_PRODUCT_HOURS(J)
                           ANNUAL_PRODUCT_MARGINAL_FUEL(I,J) = &
                                  ANNUAL_PRODUCT_MARGINAL_FUEL(I,J) / &
                                                ANNUAL_PRODUCT_HOURS(J)
                           ANNUAL_PRODUCT_FUEL_PRICE(I,J) = &
                                  ANNUAL_PRODUCT_FUEL_PRICE(I,J) / &
                                                ANNUAL_PRODUCT_HOURS(J)
                        ELSE
!                             PRODUCT_VOLATILITY(I,J) = I.
                        ENDIF
                        DAILY_PRODUCTS_REC = RPTREC(DAILY_PRODUCTS_UNIT)
                  call check_unit(DAILY_PRODUCTS_UNIT, "transobj:0037")
                        WRITE(DAILY_PRODUCTS_UNIT, &
                                         REC=DAILY_PRODUCTS_REC) &
                             PRT_ENDPOINT(), &
                             FLOAT(CURRENT_YEAR), &
                             CL_MONTH_NAME(13), &
                             MULTI_AREA_NAME(I), &
                             LOCAL_PRODUCT_TYPE(J), &
                             ANNUAL_PRODUCT_HOURS(J), &
                             ANNUAL_PRODUCT_PRICE(I,J), &
                             PRODUCT_VOLATILITY(I,J), &
                             ANNUAL_PRODUCT_QUANTITY(I,J), &
                             PRODUCT_MEAN_RETURN(I,J), &
                             ANNUAL_PRODUCT_HEATRATE(I,J), &
                             ANNUAL_PRODUCT_FUEL_PRICE(I,J), &
                             EV_DATA_SOURCE, &
                             ANNUAL_PRODUCT_MARGINAL_FUEL(I,J)*100.
                        DAILY_PRODUCTS_REC = DAILY_PRODUCTS_REC + 1
                     ENDIF ! I = 0
                     ENDDO ! PRODUCTS
               ENDDO ! I = ?
            ENDIF ! WRITE DAILY REPORTS FOR ANNUAL
!
!            
            DEALLOCATE( &
                 M_ANNUAL_PURCHASE_ENERGY, &
                 M_ANNUAL_PURCHASE_COSTS, &
                 M_ANNUAL_SALES_ENERGY, &
                 M_ANNUAL_SALES_REVENUES, &
                 M_ANNUAL_NATIVE_COST, &
                 M_ANNUAL_LOAD_B4_SALES, &
                 M_ANNUAL_PRO_COST_B4_SALES, &
                 M_ANNUAL_LOAD_AFTER_SALES, &
                 M_ANNUAL_PRO_COST_AFTER_SALES, &
                 ANNUAL_TL_MWH, &
                 ANNUAL_TL_PEAK, &
                 ANNUAL_COIN_PEAK, &
                 ANNUAL_TL_BASE, &
                 ANNUAL_EFFECTIVE_CAPACITY, &
                 ANNUAL_TL_HYDRO_MWH, &
                 ANNUAL_TL_HYDRO_MW, &
                 ANNUAL_TL_HYDRO_ROR, &
                 ANNUAL_SPINNING_MWH, &
                 ANNUAL_PRODUCT_PRICE, &
                 ANNUAL_PRODUCT_QUANTITY, &
                 ANNUAL_PRODUCT_HEATRATE, &
                 ANNUAL_PRODUCT_MARGINAL_FUEL, &
                 ANNUAL_PRODUCT_FUEL_PRICE, &
                 ANNUAL_PRODUCT_HOURS)

!            
         ENDIF ! MONTH = 12
!
! FISCAL 08/23/02
!         
         IF(YES_FISCAL_REPORTING .AND. R_MONTH == FISCAL_SEASON) THEN

            IF(YES_TRANSACT_MONTHLY_REPORTS) THEN
!
               TOTAL_LOAD_BEFORE_HYDRO = 0.
               TOTAL_HYDRO_GENERATION = 0.
               TOTAL_HYDRO_CAPACITY = 0.
               TOTAL_HYDRO_ROR = 0.
               TOTAL_EFFECTIVE_CAPACITY = 0.
               TOTAL_NATIVE_COST = 0.
               TOTAL_SPINNING_MWH = 0.
               TOTAL_NON_COIN_PEAK = 0.
               TOTAL_NON_COIN_BASE = 0.
!
               TOTAL_UNSERVED = 0.
               TOTAL_ABOVE_RESOURCES = 0.
               TOTAL_UNSERVED_COST = 0.
               TOTAL_COST_ABOVE_RESOURCES = 0.
!
               DO I = LOWER_TRANS_GROUP,UPPER_TRANS_GROUP
                  IF(FISCAL_PURCHASE_ENERGY(I) > 0.) THEN
                     AVE_BUY = FISCAL_PURCHASE_COSTS(I) / &
                                            FISCAL_PURCHASE_ENERGY(I)
                  ELSE
                     AVE_BUY = 0.
                  ENDIF
                  IF(FISCAL_SALES_ENERGY(I) > 0.) THEN
                     AVE_SELL = FISCAL_SALES_REVENUES(I) / &
                                               FISCAL_SALES_ENERGY(I)
                  ELSE
                     AVE_SELL = 0.
                  ENDIF
!            
                  IF(FISCAL_LOAD_B4_SALES(I) > 0.) THEN
                     AVE_B4 = FISCAL_PRO_COST_B4_SALES(I) / &
                                              FISCAL_LOAD_B4_SALES(I)
                  ELSE
                     AVE_B4 = 0.
                  ENDIF
!            
                  IF(FISCAL_LOAD_AFTER_SALES(I) > 0.) THEN
                     AVE_AFTER = FISCAL_PRO_COST_AFTER_SALES(I) / &
                                           FISCAL_LOAD_AFTER_SALES(I)
                  ELSE
                     AVE_AFTER = 0.
                  ENDIF
                  IF( ABS(FISCAL_LOAD_AFTER_SALES(I) - &
                                  FISCAL_LOAD_B4_SALES(I)) > 0.) THEN
                     MONTHLY_INCREMENTAL_COST = &
                       (FISCAL_PRO_COST_AFTER_SALES(I) - &
                                        FISCAL_PRO_COST_B4_SALES(I))/ &
                       (FISCAL_LOAD_AFTER_SALES(I) - &
                                             FISCAL_LOAD_B4_SALES(I))
                  ELSE
                     MONTHLY_INCREMENTAL_COST = 0.
                  ENDIF
!

                  TOTAL_UNSERVED = TOTAL_UNSERVED + &
                                            FISCAL_UNSERVED_ENERGY(I)
                  TOTAL_ABOVE_RESOURCES = TOTAL_ABOVE_RESOURCES + &
                                            FISCAL_ABOVE_RESOURCES(I)
                  TOTAL_UNSERVED_COST = TOTAL_UNSERVED_COST + &
                                       FISCAL_UNSERVED_ENERGY_COST(I)
                  TOTAL_COST_ABOVE_RESOURCES = &
                       TOTAL_COST_ABOVE_RESOURCES + & 
                                       FISCAL_COST_ABOVE_RESOURCES(I)
!            
                  TBVars(13,I,23)= FISCAL_PURCHASE_ENERGY(I)
                  TBVars(13,I,24)= FISCAL_SALES_ENERGY(I)
                  TRANS_ANN_REC = RPTREC(TRANS_ANN_UNIT)
               ! msgtrmon.rpt (Monthly Summary)
                  call check_unit(TRANS_ANN_UNIT, "transobj:0039")
                  WRITE(TRANS_ANN_UNIT,REC= TRANS_ANN_REC) &
                    PRT_ENDPOINT(), &
                    FLOAT(FISCAL_YEAR), &
                    CL_MONTH_NAME(14), &
                    MULTI_AREA_NAME(I), &
                    FISCAL_PURCHASE_ENERGY(I), &
                    FISCAL_PURCHASE_COSTS(I)/1000000., &
                    AVE_BUY, &
                    FISCAL_SALES_ENERGY(I), &
                    FISCAL_SALES_REVENUES(I)/1000000., &
                    AVE_SELL, &
                    FISCAL_LOAD_B4_SALES(I), &
                    FISCAL_PRO_COST_B4_SALES(I)/1000000., &
                    AVE_B4, &
                    FISCAL_LOAD_AFTER_SALES(I), &
                    FISCAL_PRO_COST_AFTER_SALES(I)/1000000., &
                    AVE_AFTER, &
                    MONTHLY_INCREMENTAL_COST, &
                    FISCAL_TL_MWH(I), &
                    FISCAL_TL_HYDRO_MWH(I), &
                    FISCAL_EFFECTIVE_CAPACITY(I)/12., &
                    FISCAL_TL_PEAK(I), &
                    FISCAL_TL_BASE(I), &
                    FISCAL_NATIVE_COST(I)/1000000., &
                    FISCAL_SPINNING_MWH(I), &
                    FISCAL_UNSERVED_ENERGY(I), &
                    FISCAL_UNSERVED_ENERGY_COST(I)/1000000., &
                    FISCAL_COST_ABOVE_RESOURCES(I)/1000000., &
                    FISCAL_COIN_PEAK(I), &
                    FISCAL_TL_HYDRO_MW(I)/12., &
                    FISCAL_TL_HYDRO_ROR(I)/12., &
                    FISCAL_ABOVE_RESOURCES(I)

                  TRANS_ANN_REC = TRANS_ANN_REC + 1
!
                  TOTAL_LOAD_BEFORE_HYDRO = TOTAL_LOAD_BEFORE_HYDRO + &
                                                       FISCAL_TL_MWH(I)
                  TOTAL_HYDRO_GENERATION = TOTAL_HYDRO_GENERATION + &
                                                 FISCAL_TL_HYDRO_MWH(I)
                  TOTAL_HYDRO_CAPACITY = TOTAL_HYDRO_CAPACITY + &
                                                  FISCAL_TL_HYDRO_MW(I)
                  TOTAL_HYDRO_ROR = TOTAL_HYDRO_ROR + &
                                                 FISCAL_TL_HYDRO_ROR(I)
                 TOTAL_EFFECTIVE_CAPACITY = TOTAL_EFFECTIVE_CAPACITY + &
                                           FISCAL_EFFECTIVE_CAPACITY(I)
                  TOTAL_NATIVE_COST = TOTAL_NATIVE_COST + &
                                            FISCAL_NATIVE_COST(I)
                  TOTAL_SPINNING_MWH = TOTAL_SPINNING_MWH + &
                                                 FISCAL_SPINNING_MWH(I)
                  TOTAL_NON_COIN_PEAK = TOTAL_NON_COIN_PEAK + & 
                                                      FISCAL_TL_PEAK(I)
                  TOTAL_NON_COIN_BASE = TOTAL_NON_COIN_BASE + &
                                                      FISCAL_TL_BASE(I)
               ENDDO
               IF(FISCAL_PURCHASES > 0.) THEN
                  AVE_BUY = FISCAL_PURCHASE_COST / FISCAL_PURCHASES
               ELSE
                  AVE_BUY = 0.
               ENDIF
               IF(FISCAL_SALES > 0.) THEN
                  AVE_SELL = FISCAL_SALES_REVENUE / FISCAL_SALES
               ELSE
                  AVE_SELL = 0.
               ENDIF
!            
               IF(FISCAL_LOAD_B4_SALES_V > 0.) THEN
                  AVE_B4 = FISCAL_PRO_COST_B4_SALES_V / &
                                                 FISCAL_LOAD_B4_SALES_V
               ELSE
                  AVE_B4 = 0.
               ENDIF
!            
               IF(FISCAL_LOAD_AFTER_SALES_V > 0.) THEN
                  AVE_AFTER = FISCAL_PRO_COST_AFTER_SALES_V / &
                                              FISCAL_LOAD_AFTER_SALES_V
               ELSE
                  AVE_AFTER = 0.
               ENDIF
               IF( ABS(FISCAL_LOAD_AFTER_SALES_V - &
                                   FISCAL_LOAD_B4_SALES_V) > 0.) THEN
                  MONTHLY_INCREMENTAL_COST = &
                    (FISCAL_PRO_COST_AFTER_SALES_V - &
                                          FISCAL_PRO_COST_B4_SALES_V)/ &
                   (FISCAL_LOAD_AFTER_SALES_V - FISCAL_LOAD_B4_SALES_V)
               ELSE
                  MONTHLY_INCREMENTAL_COST = 0.
               ENDIF
!
!            
               TBVars(13,0,23)= FISCAL_PURCHASES
               TBVars(13,0,24)= FISCAL_SALES
               TRANS_ANN_REC = RPTREC(TRANS_ANN_UNIT)
               ! msgtrmon.rpt (Monthly Summary)
               call check_unit(TRANS_ANN_UNIT, "transobj:0040")
               WRITE(TRANS_ANN_UNIT,REC= TRANS_ANN_REC) &
                    PRT_ENDPOINT(), &
                    FLOAT(CURRENT_YEAR), &
                    CL_MONTH_NAME(14), &
                    MULTI_AREA_NAME(0), &
                    FISCAL_PURCHASES, &
                    FISCAL_PURCHASE_COST/1000000., &
                    AVE_BUY, &
                    FISCAL_SALES, &
                    FISCAL_SALES_REVENUE/1000000., &
                    AVE_SELL, &
                    FISCAL_LOAD_B4_SALES_V, &
                    FISCAL_PRO_COST_B4_SALES_V/1000000., &
                    AVE_B4, &
                    FISCAL_LOAD_AFTER_SALES_V, &
                    FISCAL_PRO_COST_AFTER_SALES_V/1000000., &
                    AVE_AFTER, &
                    MONTHLY_INCREMENTAL_COST, &
                    TOTAL_LOAD_BEFORE_HYDRO, & ! LOAD BEFORE HYDRO
                    TOTAL_HYDRO_GENERATION, & ! HYDRO GENERATION
                    TOTAL_EFFECTIVE_CAPACITY/12., & ! EFFECTIVE_CAPACITY
                    TOTAL_NON_COIN_PEAK, & ! PEAK
                    TOTAL_NON_COIN_BASE, & ! BASE
                    TOTAL_NATIVE_COST/1000000., &
                    TOTAL_SPINNING_MWH, &
                    TOTAL_UNSERVED, &
                    TOTAL_UNSERVED_COST/1000000., &
                    TOTAL_COST_ABOVE_RESOURCES/1000000., &
                    FISCAL_COIN_PEAK(0), &
                    TOTAL_HYDRO_CAPACITY/12., &
                    TOTAL_HYDRO_ROR/12., &
                    TOTAL_ABOVE_RESOURCES
               TRANS_ANN_REC = TRANS_ANN_REC + 1

!
            ENDIF ! MONTHLY REPORT
!            
            DEALLOCATE( &
                 FISCAL_PURCHASE_ENERGY, &
                 FISCAL_PURCHASE_COSTS, &
                 FISCAL_SALES_ENERGY, &
                 FISCAL_SALES_REVENUES, &
                 FISCAL_NATIVE_COST, &
                 FISCAL_LOAD_B4_SALES, & 
                 FISCAL_PRO_COST_B4_SALES, &
                 FISCAL_LOAD_AFTER_SALES, &
                 FISCAL_PRO_COST_AFTER_SALES, &
                 FISCAL_TL_MWH, &
                 FISCAL_TL_PEAK, &
                 FISCAL_COIN_PEAK, &
                 FISCAL_TL_BASE, &
                 FISCAL_EFFECTIVE_CAPACITY, &
                 FISCAL_TL_HYDRO_MWH, &
                 FISCAL_TL_HYDRO_MW, &
                 FISCAL_TL_HYDRO_ROR, &
                 FISCAL_SPINNING_MWH)

!            
            IF(ALLOCATED(FISCAL_UNSERVED_ENERGY_COST)) &
                               DEALLOCATE(FISCAL_UNSERVED_ENERGY_COST, &
                                          FISCAL_COST_ABOVE_RESOURCES, &
                                          FISCAL_UNSERVED_ENERGY, &
                                          FISCAL_ABOVE_RESOURCES)
         ENDIF ! MONTH = FISCAL MONTH
!         
         IREC = ALINE_LOAD_DATA(INT(1,2),R_MONTH)
!         
         DEALLOCATE(PRODUCT_PRICE)
         DEALLOCATE( &
           PRODUCT_VOLATILITY, &
           PRODUCT_QUANTITY, &
           PRODUCT_HEATRATE, &
           PRODUCT_MARGINAL_FUEL, &
           PRODUCT_FUEL_PRICE, &
           PRODUCT_HOURS, &
           PRODUCT_MEAN_RETURN, &
           SUM_SQUARED_DEVIATIONS, &
           SCARCITY_COST, &
           ENERGY_COST, &
           PRODUCT_DAILY_RETURN)
!
         DEALLOCATE(HOURLY_MARGINAL_COST, &
                   HOURLY_LAMDA, &
                   HOURLY_MC_AFTER, &
                   TIE_FLOW,HOURLY_LOADS, &
                   TEST_HOURLY_REVENUE, &
                   MARGINAL_COST_DELTA, &
                   HOURLY_LOAD_B4_SALES, &
                   HOURLY_TRANSFER_MWH, &
                   HOURLY_FORWARDS_4_MONTH, &
                   HOURLY_PRO_COST_AFTER_SALES, &
                   HOURLY_CAPACITY, &
                   HOURLY_EUE, &
                   HOURLY_DERIVATIVES, &
                   HOURLY_PRO_COST_B4_SALES, &
                   HOURLY_INCREMENTAL_COST, &
                   MONTHLY_MARKET_PRICES, &
                   MONTH_COIN_PEAK, &
                   MONTH_NON_COIN_PEAK, &
                   HOURLY_LAST_PRICE, &
                   DAILY_PRODUCTS_CAPACITY, &
                   HOURLY_IN_PRICE, &
                   ALL_MONTH_PRICES, &
                   SYSTEM_OUTPUT, &
                   SYSTEM_STORAGE, &
                   SYSTEM_DERIVATIVES)
! MISSING DELALOCTED ARRAYS 8/8/01 MSG
         DEALLOCATE( DAILY_MARKET_PRICE)
         DEALLOCATE(M_MONTHLY_PRO_COST_AFTER_SALES)
         DEALLOCATE(M_MONTHLY_PRO_COST_B4_SALES)
!
         DEALLOCATE( M_UNSERVED_ENERGY, &
                    M_UNSERVED_ENERGY_COST, &
                    M_ABOVE_RESOURCES)
!
         DEALLOCATE( M_PURCHASE_ENERGY, &
                    M_PURCHASE_COSTS, &
                    MARKET_COST_ABOVE_RESOURCES, &
                    M_SALES_ENERGY, &
                    M_SALES_REVENUES, &
                    M_NATIVE_COST, &
                    M_SPINNING_MWH, &
                    M_MONTHLY_LOAD_B4_SALES, &
                    M_MONTHLY_LOAD_AFTER_SALES, &
                    trans_ror_cap, &
                    HOURLY_DUMP_CAPACITY, &
                    TRANS_MUST_CAPACITY, &
                    TRANS_SPINNING_CAPACITY, &
                    AREA_PRICE_MULT, &
                    TG_USING_PRICE_DISTN, &
                    HOURLY_SPINNING_CAPACITY, &
                    DAILY_PEAK, &
                    OFF_PEAK_SPINNING_CAPACITY, &
                    TRANS_RAMP_UP, &
                    TRANS_RAMP_DOWN, &
                    TRANS_MAX_IMPORT, &
                    TRANS_MAX_EXPORT, &
                    LAST_HOUR_SELL, &
                    HOURLY_TRANSACTION, &
                    MAX_HOURLY_IMPORT, &
                    MAX_HOURLY_EXPORT, &
                    SCARCITY_MULT)
!
         CALL CLS(15,9,33)
!
      RETURN

!      ENTRY GET_DUKE_RESERVE_CAPACITY(R_MONTHLY_ECONOMY_BOUGHT)

!
!         R_MONTHLY_ECONOMY_BOUGHT = RESERVE_CAPACITY(1)
!         
!      RETURN

      ENTRY GET_ANN_COST_ABOVE_RESOURCES(R_MONTHLY_ECONOMY_BOUGHT, &
                                            R_MONTHLY_ECONOMY_COST, &
                                                 R_MONTH,R_ASSET_CLASS)

         IF(ALLOCATED(ANNUAL_COST_ABOVE_RESOURCES) .AND. &
                          YES_RUN_TRANSACT() .AND. &
                                         PRICE_ONLY_WHOLESALE_REV) THEN
!            
            LOCAL_TRANS_GROUP = GET_ASSET_CLASS_2_TG(R_ASSET_CLASS)
!            
            IF(LOCAL_TRANS_GROUP > 0 .AND. &
                           LOCAL_TRANS_GROUP <= UPPER_TRANS_GROUP) THEN
               R_MONTHLY_ECONOMY_COST = .000001 * &
                        ANNUAL_COST_ABOVE_RESOURCES(LOCAL_TRANS_GROUP, &
                                                       R_MONTH)
               R_MONTHLY_ECONOMY_BOUGHT = .000001 * &
                             ANNUAL_ABOVE_RESOURCES(LOCAL_TRANS_GROUP, &
                                                       R_MONTH)
            ELSE
               R_MONTHLY_ECONOMY_BOUGHT = 0.
               R_MONTHLY_ECONOMY_COST = 0.
!               WRITE(4,*) "UNTRAPPED ERROR IN ENERGY ABOVE RESOURCES"
            ENDIF
         ELSE
            R_MONTHLY_ECONOMY_BOUGHT = 0.
            R_MONTHLY_ECONOMY_COST = 0.
         ENDIF
      RETURN

      ENTRY GET_MONTHLY_TRANSFER_REV_COST(R_MONTH, &
                                         R1_MONTHLY_TRANSFER_REV, &
                                         R1_MONTHLY_TRANSFER_COST, &
                                         R2_MONTHLY_TRANSFER_REV, &
                                         R2_MONTHLY_TRANSFER_COST)

!
! CALLER HAS TO KNOW 1 & 2.
! R_MONTH = 0 => ANNUAL
!
         R1_MONTHLY_TRANSFER_REV = &
                       MONTHLY_TRANSFER_REVENUE(1,R_MONTH)
         R1_MONTHLY_TRANSFER_COST = &
                       MONTHLY_TRANSFER_REVENUE(2,R_MONTH)
         R2_MONTHLY_TRANSFER_REV = &
                       MONTHLY_TRANSFER_REVENUE(2,R_MONTH)
         R2_MONTHLY_TRANSFER_COST = &
                       MONTHLY_TRANSFER_REVENUE(1,R_MONTH)
!     
      RETURN

!      ENTRY GET_MONTHLY_DUKE_RESERVE_ENERGY(R_MONTHLY_ECONOMY_BOUGHT)

!         R_MONTHLY_ECONOMY_BOUGHT = MONTHLY_RESERVE_ENERGY(1)
!      RETURN

      ENTRY GET_MONTHLY_CPL_RESERVE_ENERGY(R_MONTHLY_ECONOMY_BOUGHT)

         R_MONTHLY_ECONOMY_BOUGHT = MONTHLY_RESERVE_ENERGY(2)
      RETURN

      ENTRY GET_MONTHLY_DUKE_SUP_ENERGY(R_MONTHLY_ECONOMY_BOUGHT)

         R_MONTHLY_ECONOMY_BOUGHT = MONTHLY_SUPPLEMENTAL_ENERGY(1)
      RETURN

      ENTRY GET_MONTHLY_CPL_SUP_ENERGY(R_MONTHLY_ECONOMY_BOUGHT)

         R_MONTHLY_ECONOMY_BOUGHT = MONTHLY_SUPPLEMENTAL_ENERGY(2)
      RETURN

      ENTRY GET_ECONOMY_FROM_TRANSACT(R_MONTH, &
                     R_MONTHLY_ECONOMY_BOUGHT,R_MONTHLY_ECONOMY_COST, &
                     R_MONTHLY_ECONOMY_SOLD,R_MONTHLY_ECONOMY_REVENUE, &
                     R_ANNUAL_ECONOMY_BOUGHT,R_ANNUAL_ECONOMY_COST, &
                     R_ANNUAL_ECONOMY_SOLD,R_ANNUAL_ECONOMY_REVENUE, &
                     R_PUR_ENRG_FROM_TRANSACT,R_HOURS_IN_MONTH)

         IF(APPLY_TRANS_REV_TO_WHOLESALE()) THEN ! 'W'
            R_PUR_ENRG_FROM_TRANSACT = R_PUR_ENRG_FROM_TRANSACT + &
                       PURCHASE_ENERGY(R_MONTH)/FLOAT(R_HOURS_IN_MONTH)
            R_MONTHLY_ECONOMY_COST = PURCHASE_COSTS(R_MONTH)
            R_MONTHLY_ECONOMY_SOLD = SALES_ENERGY(R_MONTH)
         ELSE ! 'T'
            R_MONTHLY_ECONOMY_BOUGHT = PURCHASE_ENERGY(R_MONTH)
!            R_MONTHLY_ECONOMY_COST = PURCHASE_COSTS(R_MONTH)
            R_MONTHLY_ECONOMY_SOLD = SALES_ENERGY(R_MONTH)
!            R_MONTHLY_ECONOMY_REVENUE = SALES_REVENUE(R_MONTH)
         ENDIF
         R_ANNUAL_ECONOMY_BOUGHT = R_ANNUAL_ECONOMY_BOUGHT + &
                                               R_MONTHLY_ECONOMY_BOUGHT
         R_ANNUAL_ECONOMY_COST = R_ANNUAL_ECONOMY_COST + &
                                               R_MONTHLY_ECONOMY_COST
         R_ANNUAL_ECONOMY_SOLD = R_ANNUAL_ECONOMY_SOLD + &
                                               R_MONTHLY_ECONOMY_SOLD 
!
      RETURN

      ENTRY WABASH_TRANSACT_VARIABLES(   R_MONTH, &
                                         R_WABASH_TRANS_BUY_ENERGY, &
                                         R_WABASH_TRANS_BUY_RATE, &
                                         R_WABASH_TRANS_SELL_ENERGY, &
                                         R_WABASH_TRANS_SELL_RATE, &
                                         R_HOURS_IN_MONTH)

         IF(PURCHASE_ENERGY(R_MONTH) > 0.) THEN
            R_WABASH_TRANS_BUY_ENERGY = &
                       PURCHASE_ENERGY(R_MONTH)/FLOAT(R_HOURS_IN_MONTH)
            R_WABASH_TRANS_BUY_RATE = PURCHASE_COSTS(R_MONTH) / &
                                               PURCHASE_ENERGY(R_MONTH)
         ELSE
            R_WABASH_TRANS_BUY_ENERGY = 0.
            R_WABASH_TRANS_BUY_RATE = 0.
         ENDIF
         IF(SALES_ENERGY(R_MONTH) > 0.) THEN
            R_WABASH_TRANS_SELL_ENERGY = &
                          SALES_ENERGY(R_MONTH)/FLOAT(R_HOURS_IN_MONTH)
            R_WABASH_TRANS_SELL_RATE = SALES_REVENUE(R_MONTH)/ &
                                                  SALES_ENERGY(R_MONTH)
         ELSE
            R_WABASH_TRANS_SELL_ENERGY = 0. 
            R_WABASH_TRANS_SELL_RATE = 0.
         ENDIF
      RETURN

      entry GetTransPowerSold(iTG,SellerTransPower) 
      ! positive for the seller


      SellerTransPower=M_HOURLY_LOAD_B4_SALES(iTG,HOUR_IN_DAY) &
                     -M_HOURLY_LOADS        (iTG,HOUR_IN_DAY)
      return ! entry GetTransPowerSold



      ENTRY SIMULATE_MULTI_PARTY(R_HOURS_IN_MONTH,R_MONTH, &
                                         TIE_LIMIT,SPREADS)


!
! TO REDUCE ARRAY SIZES, ALL REPORTING WILL BE DONE MONTHLY
!     
!
       simulate_multi_party_count=simulate_multi_party_count+1
       


      if(.not. is_unit_in_use(int(trans_tie_unit))) then
        call end_program("transobj:0081 - trans_tie_unit " // &
        "(" // trim(itos(int(trans_tie_unit))) // ") has " // &
        " not been opened.")
        
      end if
      IF(.NOT. LAHEY_LF95()) then
          CALL MG_LOCATE_WRITE(15,9,'Multi-Party Transaction ', &
                                                        ALL_VERSIONS,0)
      end if
!
         UPPER_TRANS_GROUP = GET_NUMBER_OF_ACTIVE_GROUPS()
         TOTAL_BUYER_INDEX = UPPER_TRANS_GROUP + 1
         MAX_TRANS_WITHIN_HOUR = MAX_TRANS_ITERATIONS()
         MAXIMUM_TRANSACTION_SIZE = GET_MAXIMUM_TRANSACTION_SIZE()
!
         ALLOCATE(HOURLY_MARGINAL_COST(0:UPPER_TRANS_GROUP,DAILY_HOURS))
         ALLOCATE(LAST_TRANS_MC(0:UPPER_TRANS_GROUP))
         ALLOCATE(HOURLY_LAST_PRICE(0:UPPER_TRANS_GROUP,DAILY_HOURS))
         ALLOCATE(M_HOURLY_MC_B4_SALES(0:UPPER_TRANS_GROUP,DAILY_HOURS))
         ALLOCATE(LAST_BUYER(UPPER_TRANS_GROUP))
         ALLOCATE(LAST_SELLER(UPPER_TRANS_GROUP))
         ALLOCATE(PRICE_ANCHORED(UPPER_TRANS_GROUP))
         ALLOCATE(HOURLY_LAMDA(UPPER_TRANS_GROUP,DAILY_HOURS))
         ALLOCATE(TRANSACTIONS_WITHIN_HOUR(DAILY_HOURS))
         ALLOCATE(BUY_FOR_TRANSACTION(MAX_TRANS_WITHIN_HOUR))
         ALLOCATE(SELL_FOR_TRANSACTION(MAX_TRANS_WITHIN_HOUR))
         ALLOCATE(CUM_REDUNDANT_TRANSACTION(MAX_TRANS_WITHIN_HOUR))
         ALLOCATE(LAST_BUY_FOR_TRANSACTION(MAX_TRANS_WITHIN_HOUR))
         ALLOCATE(LAST_SELL_FOR_TRANSACTION(MAX_TRANS_WITHIN_HOUR))
         ALLOCATE(LONG_PATH_TRANSACTION(MAX_TRANS_WITHIN_HOUR))
         ALLOCATE(MARGIN_FOR_TRANSACTION(MAX_TRANS_WITHIN_HOUR))
         ALLOCATE(MW_FOR_TRANSACTION(MAX_TRANS_WITHIN_HOUR))
         ALLOCATE(LAST_MW_FOR_TRANSACTION(MAX_TRANS_WITHIN_HOUR))
         ALLOCATE(TIE_FLOW(UPPER_TRANS_GROUP,DAILY_HOURS))
         ALLOCATE(MARGINAL_COST_DELTA(UPPER_TRANS_GROUP,DAILY_HOURS))
         ALLOCATE(M_HOURLY_LOADS(UPPER_TRANS_GROUP,DAILY_HOURS))
         ALLOCATE(TEST_HOUR_TIE_LIMIT(UPPER_TRANS_GROUP, &
                                                    UPPER_TRANS_GROUP))
         ALLOCATE(M_HOURLY_PRO_COST_AFTER_SALES(UPPER_TRANS_GROUP, &
                                                          DAILY_HOURS))
         ALLOCATE(HOURLY_CAPACITY(UPPER_TRANS_GROUP,DAILY_HOURS))
         ALLOCATE(HOURLY_DERIVATIVES(UPPER_TRANS_GROUP,DAILY_HOURS))
         ALLOCATE(HOURLY_EUE(UPPER_TRANS_GROUP,DAILY_HOURS))
         ALLOCATE(M_HOURLY_PRO_COST_B4_SALES(UPPER_TRANS_GROUP, &
                                                         DAILY_HOURS))
         ALLOCATE(M_HOURLY_INCREMENTAL_COST(UPPER_TRANS_GROUP, &
                                                         DAILY_HOURS))
         ALLOCATE(HOURLY_SELL_MWH(TOTAL_BUYER_INDEX,TOTAL_BUYER_INDEX))
         ALLOCATE(MONTHLY_TRANSACTION_MWH(UPPER_TRANS_GROUP, &
                                                    TOTAL_BUYER_INDEX))
         ALLOCATE(TOTAL_DELIVERED_COST(UPPER_TRANS_GROUP, &
                                                    UPPER_TRANS_GROUP))
         ALLOCATE(SELLERS_LOCAL_CAPACITY(UPPER_TRANS_GROUP, &
                                                    UPPER_TRANS_GROUP))
         ALLOCATE(M_MONTHLY_PRO_COST_AFTER_SALES(UPPER_TRANS_GROUP))
         ALLOCATE(M_MONTHLY_PRO_COST_B4_SALES(UPPER_TRANS_GROUP))
         ALLOCATE(M_MONTHLY_LOAD_B4_SALES(UPPER_TRANS_GROUP))
         ALLOCATE(M_MONTHLY_LOAD_AFTER_SALES(UPPER_TRANS_GROUP))
         ALLOCATE(M_PURCHASE_ENERGY(UPPER_TRANS_GROUP))
         ALLOCATE(M_PURCHASE_COSTS(UPPER_TRANS_GROUP))
         ALLOCATE(MARKET_COST_ABOVE_RESOURCES(UPPER_TRANS_GROUP))
         ALLOCATE(M_SALES_ENERGY(UPPER_TRANS_GROUP))
         ALLOCATE(M_SALES_REVENUES(UPPER_TRANS_GROUP))
         ALLOCATE(M_NATIVE_COST(UPPER_TRANS_GROUP))
         ALLOCATE(M_SPINNING_MWH(UPPER_TRANS_GROUP))
         ALLOCATE(ALLOWED_TRANSACTION_PAIR(UPPER_TRANS_GROUP, &
                                                    UPPER_TRANS_GROUP))
         ALLOCATE(TRANSACTIONS_PER_HOUR(UPPER_TRANS_GROUP, &
                                                    UPPER_TRANS_GROUP))
         ALLOCATE(LONG_PATH_TRANSACTION_PAIR(UPPER_TRANS_GROUP, &
                                                    UPPER_TRANS_GROUP))
         ALLOCATE(M_UNSERVED_ENERGY(UPPER_TRANS_GROUP))
         ALLOCATE(M_ABOVE_RESOURCES(UPPER_TRANS_GROUP))
! 09/05/01         
         ALLOCATE(CAPPED_PRICE(UPPER_TRANS_GROUP))
         ALLOCATE(PRICE_MINIMUM(UPPER_TRANS_GROUP))
         ALLOCATE(M_UNSERVED_ENERGY_COST(UPPER_TRANS_GROUP))
         ALLOCATE(NET_MARGIN(UPPER_TRANS_GROUP))
         ALLOCATE(BEST_PRICE_TO_BUYER(UPPER_TRANS_GROUP,2))
         ALLOCATE(M_HOURLY_LOAD_B4_SALES( &
                                      0:UPPER_TRANS_GROUP,DAILY_HOURS))
         ALLOCATE(trans_ror_cap(0:UPPER_TRANS_GROUP))
         ALLOCATE(HOURLY_DUMP_CAPACITY(UPPER_TRANS_GROUP))
         ALLOCATE(HOURLY_DUMP_BEFORE(UPPER_TRANS_GROUP))
         ALLOCATE(TRANS_MUST_CAPACITY(UPPER_TRANS_GROUP))
         ALLOCATE(TRANS_SPINNING_CAPACITY(UPPER_TRANS_GROUP))
         ALLOCATE(HOURLY_SPINNING_CAPACITY(UPPER_TRANS_GROUP))
         ALLOCATE(DAILY_PEAK(UPPER_TRANS_GROUP))
         ALLOCATE(OFF_PEAK_SPINNING_CAPACITY(UPPER_TRANS_GROUP))
         ALLOCATE(TRANS_RAMP_UP(UPPER_TRANS_GROUP))
         ALLOCATE(TRANS_RAMP_DOWN(UPPER_TRANS_GROUP))
         ALLOCATE(TRANS_MAX_IMPORT(UPPER_TRANS_GROUP))
         ALLOCATE(TRANS_MAX_EXPORT(UPPER_TRANS_GROUP))
         ALLOCATE(LAST_HOUR_SELL(UPPER_TRANS_GROUP))
         ALLOCATE(MONTH_COIN_PEAK(0:UPPER_TRANS_GROUP))
         ALLOCATE(MONTH_NON_COIN_PEAK(0:UPPER_TRANS_GROUP))
         ALLOCATE(HOURLY_TRANSACTION(UPPER_TRANS_GROUP))
         ALLOCATE(MAX_HOURLY_IMPORT(UPPER_TRANS_GROUP))
         ALLOCATE(MAX_HOURLY_EXPORT(UPPER_TRANS_GROUP))
         ALLOCATE(SCARCITY_MULT(0:UPPER_TRANS_GROUP))
         ALLOCATE(HOURLY_TRANS_GROUP_LOAD_ACTIVE(UPPER_TRANS_GROUP))
         ALLOCATE(HOURLY_TRANS_GROUP_GEN_ACTIVE(UPPER_TRANS_GROUP))
!
         ALLOCATE(PRODUCT_PRICE(0:UPPER_TRANS_GROUP,NUM_PRODUCTS))
         ALLOCATE(PRODUCT_VOLATILITY(0:UPPER_TRANS_GROUP,NUM_PRODUCTS))
         ALLOCATE(PRODUCT_QUANTITY(0:UPPER_TRANS_GROUP,NUM_PRODUCTS))
         ALLOCATE(PRODUCT_HEATRATE(0:UPPER_TRANS_GROUP,NUM_PRODUCTS))
         ALLOCATE(PRODUCT_MARGINAL_FUEL( &
                                    0:UPPER_TRANS_GROUP,NUM_PRODUCTS))
         ALLOCATE(PRODUCT_FUEL_PRICE(0:UPPER_TRANS_GROUP,NUM_PRODUCTS))
         ALLOCATE(PRODUCT_HOURS(NUM_PRODUCTS))
         ALLOCATE(PRODUCT_MEAN_RETURN(0:UPPER_TRANS_GROUP,NUM_PRODUCTS))
         ALLOCATE(PRODUCT_SCARCITY(0:UPPER_TRANS_GROUP,NUM_PRODUCTS))
         ALLOCATE(SUM_SQUARED_DEVIATIONS( &
                                     0:UPPER_TRANS_GROUP,NUM_PRODUCTS))
         ALLOCATE(SCARCITY_COST(0:UPPER_TRANS_GROUP,DAILY_HOURS))
         ALLOCATE(ENERGY_COST(0:UPPER_TRANS_GROUP,DAILY_HOURS))
         ALLOCATE(PRODUCT_DAILY_RETURN(0:UPPER_TRANS_GROUP, &
                                        NUM_PRODUCTS,R_HOURS_IN_MONTH))
!
         PRODUCT_PRICE = 0.
         PRODUCT_VOLATILITY = 0.
         PRODUCT_QUANTITY = 0.
         PRODUCT_HEATRATE = 0.
         PRODUCT_MARGINAL_FUEL = 0.
         PRODUCT_FUEL_PRICE = 0.
         PRODUCT_HOURS = 0.
         PRODUCT_MEAN_RETURN = 0.
         PRODUCT_SCARCITY = 0. 
         SUM_SQUARED_DEVIATIONS = 0.
         SCARCITY_COST = 0.
         ENERGY_COST = 0.0
         PRODUCT_DAILY_RETURN = 0.
         HOURLY_LAST_PRICE = 0.
         TIE_FLOW = 0.0
!
         WRITE_DAY_OF_WEEK = 0.
!
         MONTHLY_TRANSACTION_MWH = 0.
         TOTAL_DELIVERED_COST = 999999.
!
         M_MONTHLY_PRO_COST_AFTER_SALES = 0.
         M_MONTHLY_PRO_COST_B4_SALES = 0.
         M_MONTHLY_LOAD_AFTER_SALES = 0.
         M_MONTHLY_LOAD_B4_SALES = 0.
         M_PURCHASE_ENERGY = 0.
         M_PURCHASE_COSTS = 0.
         MARKET_COST_ABOVE_RESOURCES = 0.
         M_SALES_ENERGY = 0.
         M_SALES_REVENUES = 0.
         M_NATIVE_COST = 0.
         M_SPINNING_MWH = 0.
         M_UNSERVED_ENERGY = 0.
         M_ABOVE_RESOURCES = 0.
         M_UNSERVED_ENERGY_COST = 0.
         trans_ror_cap = 0.
         TRANS_MUST_CAPACITY = 0.
         TRANS_SPINNING_CAPACITY = 0.
         HOURLY_SPINNING_CAPACITY = 0.
         OFF_PEAK_SPINNING_CAPACITY = 0.
         TRANS_RAMP_UP = 0.
         TRANS_RAMP_DOWN = 0.
         TRANS_MAX_IMPORT = 0.
         TRANS_MAX_EXPORT = 0.
         MONTH_COIN_PEAK = 0.
         MONTH_NON_COIN_PEAK = 0.
         LAST_HOUR_SELL =  0.d0
         M_HOURLY_LOAD_B4_SALES = 0.d0
         M_HOURLY_MC_B4_SALES = 0.0
         HOURLY_MARGINAL_COST = 0.0
         HOURLY_LAMDA = 0.0
         M_HOURLY_PRO_COST_B4_SALES = 0.0
         
! 101921. MOVED.
         HOURLY_EUE = 0.
         M_HOURLY_PRO_COST_AFTER_SALES = 0.0
         M_HOURLY_INCREMENTAL_COST = 0.0 
         MARGINAL_COST_DELTA = 0.0
         M_HOURLY_LOADS = 0.D0
         PRICE_MINIMUM = 0.10 

         CALL GET_TRANS_ROR_CAPACITY(UPPER_TRANS_GROUP, &
                                                    trans_ror_cap)
         VOID_LOGICAL = GET_TRANS_SPINNING_CAPACITY(UPPER_TRANS_GROUP, &
                                  R_MONTH,globecom_year,TRANS_SPINNING_CAPACITY)
         VOID_LOGICAL = GET_TRANS_PRICE_CAPS(UPPER_TRANS_GROUP, &
                                  R_MONTH,globecom_year,CAPPED_PRICE)
         VOID_LOGICAL = GET_TRANS_PRICE_MINIMUM(UPPER_TRANS_GROUP, &
                                  R_MONTH,globecom_year,PRICE_MINIMUM)
         VOID_LOGICAL = GET_OFF_PEAK_SPINNING_CAPACITY( &
                                  UPPER_TRANS_GROUP, &
                                  R_MONTH,globecom_year, &
                                  OFF_PEAK_SPINNING_CAPACITY)
         VOID_LOGICAL = GET_TRANS_RAMP_RATES(UPPER_TRANS_GROUP, &
                                            R_MONTH,TRANS_RAMP_UP, &
                                            TRANS_RAMP_DOWN)
         VOID_LOGICAL = GET_TRANS_MAX_IMPORT_EXPORT( &
                                            UPPER_TRANS_GROUP, &
                                            R_MONTH, &
                                            TRANS_MAX_IMPORT, &
                                            TRANS_MAX_EXPORT)
         IF(R_MONTH == FIRST_MONTHLY_TRANSACT) THEN
            IF(ALLOCATED(PRODUCT_LAST_PRICE)) &
            ! ADD 6/18/98 MSG. MOVED. 6/22/98. GAT.
                                 DEALLOCATE(PRODUCT_LAST_PRICE)
            IF(ALLOCATED(ANNUAL_UNSERVED_ENERGY_COST)) &
                               DEALLOCATE(ANNUAL_UNSERVED_ENERGY_COST, &
                                          ANNUAL_COST_ABOVE_RESOURCES, &
                                          ANNUAL_UNSERVED_ENERGY, &
                                          ANNUAL_ABOVE_RESOURCES)
            ALLOCATE( &
                  PRODUCT_LAST_PRICE(0:UPPER_TRANS_GROUP,NUM_PRODUCTS))
            ALLOCATE(M_ANNUAL_PURCHASE_ENERGY(UPPER_TRANS_GROUP))
            ALLOCATE(M_ANNUAL_PURCHASE_COSTS(UPPER_TRANS_GROUP))
            ALLOCATE(M_ANNUAL_SALES_ENERGY(UPPER_TRANS_GROUP))
            ALLOCATE(M_ANNUAL_SALES_REVENUES(UPPER_TRANS_GROUP))
            ALLOCATE(M_ANNUAL_NATIVE_COST(UPPER_TRANS_GROUP))
            ALLOCATE(M_ANNUAL_LOAD_B4_SALES(UPPER_TRANS_GROUP))
            ALLOCATE(M_ANNUAL_PRO_COST_B4_SALES(UPPER_TRANS_GROUP))
            ALLOCATE(M_ANNUAL_LOAD_AFTER_SALES(UPPER_TRANS_GROUP))
            ALLOCATE(M_ANNUAL_PRO_COST_AFTER_SALES(UPPER_TRANS_GROUP))
            ALLOCATE(ANNUAL_TL_MWH(UPPER_TRANS_GROUP))
            ALLOCATE(ANNUAL_TL_PEAK(UPPER_TRANS_GROUP))
            ALLOCATE(ANNUAL_COIN_PEAK(0:UPPER_TRANS_GROUP))
            ALLOCATE(ANNUAL_TL_BASE(UPPER_TRANS_GROUP))
            ALLOCATE(ANNUAL_TL_HYDRO_MWH(UPPER_TRANS_GROUP))
            ALLOCATE(ANNUAL_TL_HYDRO_MW(UPPER_TRANS_GROUP))
            ALLOCATE(ANNUAL_TL_HYDRO_ROR(UPPER_TRANS_GROUP))
            ALLOCATE(ANNUAL_SPINNING_MWH(UPPER_TRANS_GROUP))
            ALLOCATE(ANNUAL_EFFECTIVE_CAPACITY(UPPER_TRANS_GROUP))
            ALLOCATE(ANNUAL_UNSERVED_ENERGY(UPPER_TRANS_GROUP,0:12))
            ALLOCATE(ANNUAL_ABOVE_RESOURCES(UPPER_TRANS_GROUP,0:12))
            ALLOCATE(ANNUAL_UNSERVED_ENERGY_COST(UPPER_TRANS_GROUP))
            ALLOCATE(ANNUAL_COST_ABOVE_RESOURCES(UPPER_TRANS_GROUP, &
                                                                 0:12))
            ALLOCATE(ANNUAL_TRANSACTION_MWH(UPPER_TRANS_GROUP, &
                                                    TOTAL_BUYER_INDEX))
            ANNUAL_TL_BASE = 999999.
            PRODUCT_LAST_PRICE = 0.
            M_ANNUAL_PURCHASE_ENERGY = 0.
            M_ANNUAL_PURCHASE_COSTS = 0.
            M_ANNUAL_SALES_ENERGY = 0.
            M_ANNUAL_SALES_REVENUES = 0.
            M_ANNUAL_NATIVE_COST = 0.
            M_ANNUAL_LOAD_B4_SALES = 0.
            M_ANNUAL_LOAD_AFTER_SALES = 0.
            M_ANNUAL_PRO_COST_B4_SALES = 0.
            M_ANNUAL_PRO_COST_AFTER_SALES = 0.
            ANNUAL_TL_MWH = 0.
            ANNUAL_TL_PEAK = 0.
            ANNUAL_COIN_PEAK = 0.
            ANNUAL_TL_HYDRO_MWH = 0.
            ANNUAL_TL_HYDRO_MW = 0.
            ANNUAL_TL_HYDRO_ROR = 0.
            ANNUAL_SPINNING_MWH = 0.
            ANNUAL_EFFECTIVE_CAPACITY = 0.
            ANNUAL_UNSERVED_ENERGY = 0.
            ANNUAL_ABOVE_RESOURCES = 0.
            ANNUAL_UNSERVED_ENERGY_COST = 0.
            ANNUAL_COST_ABOVE_RESOURCES = 0.
            ANNUAL_TRANSACTION_MWH = 0.
            ALLOCATE(ANNUAL_PRODUCT_PRICE(0:UPPER_TRANS_GROUP, &
                                                         NUM_PRODUCTS))
            ALLOCATE(ANNUAL_PRODUCT_SCARCITY(0:UPPER_TRANS_GROUP, &
                                                         NUM_PRODUCTS))
            ALLOCATE(ANNUAL_PRODUCT_QUANTITY(0:UPPER_TRANS_GROUP, &
                                                         NUM_PRODUCTS))
            ALLOCATE(ANNUAL_PRODUCT_HEATRATE(0:UPPER_TRANS_GROUP, &
                                                         NUM_PRODUCTS))
            ALLOCATE(ANNUAL_PRODUCT_MARGINAL_FUEL(0:UPPER_TRANS_GROUP, &
                                                         NUM_PRODUCTS))
            ALLOCATE(ANNUAL_PRODUCT_FUEL_PRICE(0:UPPER_TRANS_GROUP, &
                                                         NUM_PRODUCTS))
            ALLOCATE(ANNUAL_PRODUCT_HOURS(NUM_PRODUCTS))
            ANNUAL_PRODUCT_PRICE = 0.
            ANNUAL_PRODUCT_SCARCITY = 0. 
            ANNUAL_PRODUCT_QUANTITY = 0.
            ANNUAL_PRODUCT_HEATRATE = 0.
            ANNUAL_PRODUCT_MARGINAL_FUEL = 0.
            ANNUAL_PRODUCT_FUEL_PRICE = 0.
            ANNUAL_PRODUCT_HOURS = 0.
!
         ENDIF ! R_MONTH == FIRST_MONTHLY_TRANSACT
!         

         DAY = 1
!         
         LOAD_INCREMENT = TIE_LIMIT/20.
         LOAD_INCREMENT = 10.

         SPREAD_MARGIN = SPREAD_PRICES(SPREADS)
         MAX_TIE_FLOW = 0.
         MIN_TIE_FLOW = TIE_LIMIT
!
! MARKET CLEARS EACH HOUR THROUGH A SERIES OF TRANSACTIONS, STARTING
! WITH THE HIGHEST TRANSACTION MARGIN. MARKET CLEARS WHEN
! THERE ARE NO MARGINS GREATER THAN .1 $/MWH OR NO ADDITIONAL 
! RESOURCES ARE AVAILABLE.
!
! A RESOURCE WITH A NATIVE LOAD IS ASSUMED TO ALWAYS MEET THAT LOAD.
! IF IT CANNOT MEET THAT LOAD WITH ITS OWN RESOURCES IT MUST PAY AN 
! EXTERNAL TRANSACTION GROUP FOR ADDITIONAL RESOURCE. IF AN EXTERNAL 
! SOURCE CAN SUPPLY THAT RESOURCE MORE CHEAPLY, THEN IT WILL BUY 
! FROM THAT ENTITY.
!
! THUS, IF A TRANSACTION GROUP HAS LESS CAPACITY THAN RESOURCE, IT WOULD
! NEVER SELL (CHEAPER THAN THE MARKET, USE IT TO MEET NATIVE LOAD.
! GREATER THAN THE MARKET, DISPLACE IT WITH MARKET RESOURCE.) 
!
! WHAT IS THE MARKET PRICE? IT IS THE POINT WHERE MARGINAL REVENUE 
! EQUALS MARGINAL COST. IF THE ALGORITHM CORRECTLY FINDS THE GREATEST 
! TO SMALLEST NET MARGINS, THE MARKET PRICE SHOULD BE THE PRICE OF 
! THE LAST TRANSACTION  ON THE SYSTEM IN A PARTICULAR HOUR.
!
! USING MARKET PRICES SHOULD BE AN OPTIONAL SWITCH FOR THIS ROUTINE.
! USING AFTER EL LOADS SHOULD BE AN OPTIONAL SWITCH FOR THIS ROUTINE.
! IF TWO RESOURCES HAVE THE HIGHEST MARGIN, THEY MAKE A TRANSACTION.
! AFTER THE TRANSACTION, ONLY THEIR MARGINAL COSTS MUST BE UPDATED.
!     
!
! 4/16/99. GAT. NON-DISPATCHABLE CAPACITY > LOAD ISSUES:
!
!   1. PERMIT A NEGATIVE HOURLY LOAD VALUE
!   2. PRICE THE HOURLY LOAD VALUE AT (?) ZERO
!   3. MAKE SURE THAT NEGATIVE HOURLY LOADS DO NOT CAUSE ERRORS
!   4. MAKE SURE THAT THE SALE DOES NOT EXTEND BEYOND THE NEGATIVE VALUE
!
         SEARCHES_WITHIN_TRANSACTIONS = 0
         FAILED_SEARCH_SPEEDUP = 0
!         
         VOID_LOGICAL = UPDATE_MONTHLY_TIE_COST(R_MONTH)
         VOID_LOGICAL = INIT_DAILY_TIE_REVENUES()
!
         TOTAL_TRANS_PER_MONTH = 0
         TOTAL_ZERO_MW_TRANS_PER_MONTH = 0
!
         NewStructure=.true. !requires one-time inversion of LP matrices
         TRANSACTION_TOLERANCE = 0.1
         HOURLY_DIAGNOSTICS = .TRUE.
!
         YES_MONTH_OUTAGE_EVENTS = OUTAGE_EVENTS_IN_MONTH() 
!
!
         TEMP_L = INIT_DEPTH_PRICE()
!
         DAILY_PRICE = 0.
!
         HOURLY_CAPACITY = 0.
!
!
! 041717. !!!!!!!!!!!!!! TEST !!!!!!!!!!!!!!!!!!!!!
!
         IF(R_MONTH == 1) THEN
!            VOID_LOGICAL = CX_DailyStorePat2()
!
! 082121. INCLUDES LOCAL FOR STORAGE/HYBRID.
!
            VOID_LOGICAL = CX_DailyStorePat3()
!            VOID_LOGICAL = CX_DailyStorePattern()
         ENDIF
!
! 041717. !!!!!!!!!!!!!! TEST !!!!!!!!!!!!!!!!!!!!!
!
!
         LAHEY = LAHEY_LF95()
         IF(.NOT. LAHEY) &
                 CALL MG_LOCATE_WRITE(16,9,'Hour in Month           ', &
                                                        ALL_VERSIONS,0)
! 01/20/03
         LAST_BUY_FOR_TRANSACTION = 0.
         LAST_SELL_FOR_TRANSACTION = 0.
         LAST_MW_FOR_TRANSACTION = 0.d0
!
         MAX_IN_PATHS = GET_MAX_IN_PATHS()

         HOUR_IN_DAY = 1
         IF(TRANSACTION_PERIOD == 'S') THEN
            STEP_4 = .TRUE.
         ELSE
            STEP_4 = .FALSE.
         ENDIF
         IF(STEP_4) THEN
            STEP_4_ADDER = 4
         ELSE
            STEP_4_ADDER = 1
         ENDIF
!

!
         DO HR = 1, R_HOURS_IN_MONTH
!
            IF(STEP_4) THEN
               REMAIN = MOD(REAL(HR+3),4.)
               IF(REMAIN < .001) THEN ! SIMULATE THE HOUR
                        ! DAY = HR/4
               ELSE
                  CYCLE ! DAY = HR/4 + 1
               ENDIF
            ENDIF
!
            IF(.NOT. LAHEY) THEN
               WRITE(SCREEN_MESSAGES,"(I3)") HR
               CALL MG_LOCATE_WRITE(16,23,trim(SCREEN_MESSAGES), &
                                                        ALL_VERSIONS,0)
            ENDIF
!
!
!
            IF(YES_MONTH_OUTAGE_EVENTS) THEN
               TEMP_L = UPDATE_OUTAGE_DATA(HR)
            ENDIF
!
!
!
! MOVED UP. 7/22/98. GAT.
            REMAIN = MOD(FLOAT(HR),24.)
            IF(REMAIN < .001) THEN ! SUMMARIZE THE DAY
               DAY = HR/24
            ELSE
               DAY = HR/24 + 1
            ENDIF
            CALENDAR_DAY_OF_WEEK = GET_DAY_OF_WEEK_4(R_MONTH,DAY)
!
            DAY_TYPE = &
                   GET_DAY_TYPE(HOUR_IN_DAY, &
                                          CALENDAR_DAY_OF_WEEK,R_MONTH)
!
! ARTMAN LOGIC.
!
            L_M = C_M ! C_M FROM MAIN PROGRAM.
!            
            IF(IGNORE_NIGHTIME_CAPVAL .AND. &
               ( APPLY_ENERGY_PRODUCT(HOUR_IN_DAY, &
                                       CALENDAR_DAY_OF_WEEK, &
                                LOCAL_PRODUCT_TYPE(HOUR_IS_5X8)) .OR. &
                APPLY_ENERGY_PRODUCT(HOUR_IN_DAY, &
                                       CALENDAR_DAY_OF_WEEK, &
                              LOCAL_PRODUCT_TYPE(HOUR_IS_WKX16)))) THEN
               L_M = 1               
            ENDIF
!     
            IF(APPLY_ENERGY_PRODUCT(HOUR_IN_DAY, &
                                       CALENDAR_DAY_OF_WEEK, &
                             LOCAL_PRODUCT_TYPE(HOUR_IS_ON_PEAK))) THEN
               PEAK_HOUR = 1
            ELSE
               PEAK_HOUR = 2
            ENDIF
!
            IF(APPLY_ENERGY_PRODUCT(HOUR_IN_DAY, &
                                       CALENDAR_DAY_OF_WEEK, &
                               LOCAL_PRODUCT_TYPE(HOUR_IS_SAX16)) .OR. &
               APPLY_ENERGY_PRODUCT(HOUR_IN_DAY, &
                                       CALENDAR_DAY_OF_WEEK, &
                               LOCAL_PRODUCT_TYPE(HOUR_IS_SUX16))) THEN
               WEEKEND_DAY = .TRUE.
            ELSE
               WEEKEND_DAY = .FALSE.
               IF(PEAK_HOUR ==2) THEN
                  NIGHT = .TRUE.
               ELSE
                  NIGHT = .FALSE.
               ENDIF
            ENDIF
!            
            SCARCITY_MULT = 1.
!
            IF(HOUR_IN_DAY == 1) THEN
               DAILY_PEAK = 0.
            ENDIF
!            
            TRANSACTIONS_WITHIN_HOUR(HOUR_IN_DAY) = 0
            IF(HR > 1) THEN
               LAST_TWH = TWH
               K = 1
               DO I = 1, MAX_IN_PATHS
                     LAST_MW_FOR_TRANSACTION(K) = &
                                       GET_HOUR_PATH_MW(I,SELLER,BUYER)
                     IF(LAST_MW_FOR_TRANSACTION(K) <= 0.1d0 .OR. &
                                    SELLER == 0  .OR. BUYER == 0) CYCLE
                     LAST_SELL_FOR_TRANSACTION(K) = SELLER
                     LAST_BUY_FOR_TRANSACTION(K) = BUYER
                     K = K + 1
               ENDDO
! MOST FREQUENT TRANSACTIONS
! MOST PROFITABLE TRANSACTIONS
! OTHER CRITERIA
               LAST_TWH = K - 1
               LAST_BUY_FOR_TRANSACTION(K:MAX_TRANS_WITHIN_HOUR) = 0
               LAST_SELL_FOR_TRANSACTION(K:MAX_TRANS_WITHIN_HOUR) = 0
               LAST_MW_FOR_TRANSACTION(K:MAX_TRANS_WITHIN_HOUR) = 0.d0
            ENDIF
!
! 101921. MOVED.
!
            BUY_FOR_TRANSACTION = 0.
            SELL_FOR_TRANSACTION = 0.
            CUM_REDUNDANT_TRANSACTION = 0.
            LONG_PATH_TRANSACTION = 0.
            MARGIN_FOR_TRANSACTION = 999999.
            MW_FOR_TRANSACTION = 0.
            LAST_BUYER = 0.
            LAST_SELLER = 0.
            VOID_LOGICAL = INIT_HOURLY_TIE()
! 1/8/03. ALTERED HOUR_PATH_LIMIT TO REFLECT MULTIPLIERS
            VOID_LOGICAL = INIT_HOUR_PATH_LIMIT(PEAK_HOUR,R_MONTH,globecom_year)
! 10/23/02.  TESTING !!!
            VOID_LOGICAL = SET_HOUR_LONG_PATH_WI_PATHS(PEAK_HOUR, &
                                                          R_MONTH,globecom_year)
!
            HOURLY_DUMP_CAPACITY = 0.
            HOURLY_DUMP_BEFORE = 0.
            HOURLY_TRANSACTION = 0.d0
            MAX_HOURLY_IMPORT = 0.
            MAX_HOURLY_EXPORT = 0.
!
            HOURLY_SELL_MWH = 0.
!
            TEST_HOUR_TIE_LIMIT = 0.
            TEMP_CAP = 0.
            DO I = 1, UPPER_TRANS_GROUP

               TG = GET_TRANS_GROUP_INDEX(I)
               DATA_BASE = GET_DATA_BASE_FOR_TRANS(TG,DAY_TYPE)
               IF(DATA_BASE == 0) then
                CYCLE
               end if
               
            ENDDO
!
               M_HOURLY_LOAD_B4_SALES(0,HOUR_IN_DAY) = 0.d0
               HOURLY_MARKET_PRICE = TRANSACT_UNSERVED_COST

!
! INITIAL CONDITIONS FOR REPORTING PURPOSES
!
! TG = COMPRESSED TRANSACTION GROUP INDEX
! I = (ORIGINAL) TRANSACTION GROUP INDEX
! DATA_BASE = TRANSACTION GROUP FOR A DAY TYPE
!
            DO I = 1, UPPER_TRANS_GROUP 

               DO J = 1, UPPER_TRANS_GROUP
                  TEST_HOUR_TIE_LIMIT(I,J) = &
                             HOUR_TIE_LIMIT(I,J)

               ENDDO
!
               IF(WEEKEND_DAY) THEN
                  SCARCITY_MULT(I) = GET_WEEKEND_SCARCITY_MULT(I)
                  HOURLY_SPINNING_CAPACITY(I) = &
                               OFF_PEAK_SPINNING_CAPACITY(I)
               ELSEIF(NIGHT) THEN
                  SCARCITY_MULT(I) = GET_NIGHT_SCARCITY_MULT(I)
                  HOURLY_SPINNING_CAPACITY(I) = &
                             OFF_PEAK_SPINNING_CAPACITY(I)
               ELSE
                  SCARCITY_MULT(I) = 1.0
                  HOURLY_SPINNING_CAPACITY(I) = &
                                TRANS_SPINNING_CAPACITY(I)
               ENDIF
!
               TG = GET_TRANS_GROUP_INDEX(I)
!
! GET LOAD REQUIREMENTS OF THE TRANSACTION GROUP
!
               DATA_BASE = GET_DATA_BASE_FOR_TRANS(TG,DAY_TYPE)
!
               HOURLY_TRANS_GROUP_LOAD_ACTIVE(I) = &
                                             TRANS_GROUP_LOAD_ACTIVE(I)
!
! ADDED 11/9/00. GAT.
! ALTERED 01/21/11. FOR KATHY.
               IF(STEP_4) THEN
                  HOURLY_GRX_STORAGE = 0.0
                  DO J_F = 0, 3
                     L_F = HR + J_F
                     HOURLY_GRX_STORAGE = HOURLY_GRX_STORAGE - &
                           0.25 * GET_GRX_TRANS_HOURLY_STORAGE(I, &
                                                               L_F, &
                                                               R_MONTH)
                  END DO
               ELSE
                 HOURLY_GRX_STORAGE = -GET_GRX_TRANS_HOURLY_STORAGE(I, &
                                                               HR, &
                                                               R_MONTH)
               ENDIF
               IF(HR > 1) THEN
                  HOURLY_DERIVATIVE_ENERGY = &
                       HOURLY_INTERRUPTIBLE_CAPACITY( &
                                      HOUR_IN_DAY, &
                                      PRODUCT_LAST_PRICE(I,3), &
                                      CALENDAR_DAY_OF_WEEK, &
                                      DAY, &
                                      I, &
                                      R_MONTH)
                  HOURLY_FORWARD_SALE = &
                           HOURLY_FORWARD_CONTRACT_ENERGY( &
                                HOUR_IN_DAY, &
                                CALENDAR_DAY_OF_WEEK,DAY,I, &
                                PRODUCT_LAST_PRICE(I,3),R_MONTH) 
                  IF(STEP_4) THEN
                     DO J_F = 1, 3
                        L_F = HOUR_IN_DAY + J_F
                        TEMP_R2 = &
                         HOURLY_FORWARD_CONTRACT_ENERGY( &
                                L_F, &
                                CALENDAR_DAY_OF_WEEK,DAY,I, &
                                PRODUCT_LAST_PRICE(I,3),R_MONTH) 
                     END DO
                  ENDIF
               ELSE
                  HOURLY_FORWARD_SALE = &
                           HOURLY_FORWARD_CONTRACT_ENERGY( &
                                HOUR_IN_DAY, &
                                CALENDAR_DAY_OF_WEEK,DAY,I, &
                                DAILY_PRICE(HOUR_IN_DAY),R_MONTH) 
                  IF(STEP_4) THEN
                     DO J_F = 1, 3
                        L_F = HOUR_IN_DAY + J_F
                        TEMP_R2 = &
                         HOURLY_FORWARD_CONTRACT_ENERGY( &
                                L_F, &
                                CALENDAR_DAY_OF_WEEK,DAY,I, &
                                DAILY_PRICE(HOUR_IN_DAY),R_MONTH) 
                     END DO
                  ENDIF
               ENDIF

               HOURLY_FORWARD_SALE = HOURLY_FORWARD_SALE - &
                                          HOURLY_DERIVATIVE_ENERGY + &
                                          HOURLY_GRX_STORAGE
!
               HOURLY_DERIVATIVES(I,HOUR_IN_DAY) = HOURLY_FORWARD_SALE
!
               IF(HOURLY_TRANS_GROUP_LOAD_ACTIVE(I)) THEN
!
                  IF(USE_TF_FILE) THEN
! NOTE REASSIGNMENT OF HOURLY_LOAD_B4_SALES AND READ ACROSS 24 HOURS 
! 080101
!
                     IF(HOUR_IN_DAY == 1) THEN
                        DO J = 1, 24
                           TEMP_I2 = HR+J-1
                           M_HOURLY_LOAD_B4_SALES(I,J) = REAL( &
                                  GET_TRANS_LOAD_AFTER_EL(TEMP_I2,I),8)
                           DAILY_PEAK(I) = MAX(DAILY_PEAK(I), &
                                     sngl(M_HOURLY_LOAD_B4_SALES(I,J)))
                        ENDDO
                     ENDIF
!                     
                     IF(HR == 196 .AND. R_MONTH == 6) THEN
                        BUYER = BUYER
                     ENDIF
                     
                     HOURLY_SPINNING_CAPACITY(I) = &
                       GET_DAILY_PEAK_SPIN(I, &
                                       R_MONTH, &
                                       globecom_year, &
                                       HOURLY_SPINNING_CAPACITY(I), &
                                       PEAK_HOUR, &
                                       DAILY_PEAK(I))
!
! THIS IS ONE WAY TO CHARACTERIZE DEMAND
!
                     r8LoadB4 = MAX(0.d0, &
                            M_HOURLY_LOAD_B4_SALES(I,HOUR_IN_DAY) - &
                                                 trans_ror_cap(I))
                     r8TEMP = M_HOURLY_LOAD_B4_SALES(I,HOUR_IN_DAY) - &
                                    trans_ror_cap(I) + &
                                            HOURLY_SPINNING_CAPACITY(I) 
! 4/16/99. GAT.
                  ELSE
                     r8TEMP = MAX(0.d0,SYSTEM_HOURLY_LOADS(HR,I))
                     r8LoadB4 = 0.d0
                  ENDIF
!
!
                  r8LoadB4 = r8LoadB4 + HOURLY_FORWARD_SALE
                  r8TEMP = r8TEMP + HOURLY_FORWARD_SALE
                  M_HOURLY_LOAD_B4_SALES(I,HOUR_IN_DAY) = r8LoadB4
                  M_HOURLY_LOAD_B4_SALES(INT(0,2),HOUR_IN_DAY) = &
                        M_HOURLY_LOAD_B4_SALES(INT(0,2),HOUR_IN_DAY) + &
                                                               r8LoadB4
                  M_HOURLY_LOADS(I,HOUR_IN_DAY) = r8TEMP
!
               ELSE
!
                  M_HOURLY_LOAD_B4_SALES(I,HOUR_IN_DAY) = &
                                                   HOURLY_FORWARD_SALE
! 4/16/99. GAT. ! CHANGED SIGN ON trans_ror_cap 9/14/99. GAT.
                  M_HOURLY_LOADS(I,HOUR_IN_DAY) = &
                        HOURLY_FORWARD_SALE - &
                             trans_ror_cap(I) + &
                                            HOURLY_SPINNING_CAPACITY(I)
!
                  r8TEMP = HOURLY_FORWARD_SALE - trans_ror_cap(I)
! FORWARD SALES WITHOUT LOADS.  1/10/01. FOR BURESH.
                  IF(r8TEMP > 0.d0) THEN
                     HOURLY_TRANS_GROUP_LOAD_ACTIVE(I) = .TRUE.
                  ENDIF
               ENDIF
!
! MODIFIED 1/24/02.
! MOVED 3/21/02.
!
               IF(TRANS_MAX_IMPORT(I) >= 0.) THEN
                  MAX_HOURLY_IMPORT(I) = TRANS_MAX_IMPORT(I)
               ELSE
                  MAX_HOURLY_IMPORT(I) = &
                          ESCALATED_MONTHLY_VALUE( &
                                   ABS(TRANS_MAX_IMPORT(I)), &
                                   INT(ABS(TRANS_MAX_IMPORT(I)),2), &
                                               globecom_year, &
                                               R_MONTH,INT(1,2))
               ENDIF

               IF(TRANS_MAX_EXPORT(I) >= 0.) THEN
                  MAX_HOURLY_EXPORT(I) = TRANS_MAX_EXPORT(I)
               ELSE
                  MAX_HOURLY_EXPORT(I) = &
                          ESCALATED_MONTHLY_VALUE( &
                                      ABS(TRANS_MAX_EXPORT(I)), &
                                   INT(ABS(TRANS_MAX_EXPORT(I)),2), &
                                               globecom_year, &
                                               R_MONTH,INT(1,2))
               ENDIF
               IF(r8TEMP < 0.d0) THEN
                  HOURLY_DUMP_CAPACITY(I) = - r8TEMP
                  HOURLY_DUMP_BEFORE(I) = - r8TEMP
                  M_HOURLY_LOADS(I,HOUR_IN_DAY) = &
                                MAX(0.d0,M_HOURLY_LOADS(I,HOUR_IN_DAY))
               ENDIF
               HOURLY_TRANS_GROUP_GEN_ACTIVE(I) = &
                                      TRANS_GROUP_GEN_ACTIVE(DATA_BASE)
!
! GET MARGINAL COST, IF RESOURCES ARE AVAILABLE, OF THE TRANSACTION 
! GROUP
               IF(HOURLY_TRANS_GROUP_GEN_ACTIVE(I) .OR. & 
                                     HOURLY_DUMP_CAPACITY(I) > 0.) THEN
                  IF(HOURLY_TRANS_GROUP_LOAD_ACTIVE(I)) THEN
!
! 06/04/04 NOTE: REDEFINITION OF HOURLY LOAD BEFORE TO INCLUDE SPINNING
!
!                    LOAD_FOR_MC = M_HOURLY_LOAD_B4_SALES(I,HOUR_IN_DAY)
                     LOAD_FOR_MC = M_HOURLY_LOADS(I,HOUR_IN_DAY)
                  ELSE
                     LOAD_FOR_MC = 1.d0 ! ARBITRARY
                  ENDIF
! 4/25/01. GAT.
                  IF(HOURLY_TRANS_GROUP_GEN_ACTIVE(I)) &
                    HOURLY_MARGINAL_COST(I,HOUR_IN_DAY) = & 
! ALSO USED TO SET POS()
                          GET_MARGINAL_COST_AT_MARKET( &
                                    LOAD_FOR_MC,DATA_BASE, &
                                    SCARCITY_COST(I,HOUR_IN_DAY),L_M, &
                                    SCARCITY_MULT(I))
                  IF(TRANS_GROUP_CAP(DATA_BASE) + &
                                             HOURLY_DUMP_CAPACITY(I) < &
                            M_HOURLY_LOAD_B4_SALES(I,HOUR_IN_DAY)) THEN
                     HOURLY_MARGINAL_COST(I,HOUR_IN_DAY) = &
                                                 TRANSACT_UNSERVED_COST
! CHANGED FROM >=. 11/2/99. GAT.
                  ELSEIF(HOURLY_DUMP_CAPACITY(I) >  0.) THEN
                        HOURLY_MARGINAL_COST(I,HOUR_IN_DAY) = &
                                                TRANSACT_ABUNDANCE_COST
                  ENDIF
! 4/26/01. GAT.
                  IF(HOURLY_TRANS_GROUP_GEN_ACTIVE(I)) &
                    M_HOURLY_PRO_COST_B4_SALES(I,HOUR_IN_DAY) = &
                       GET_AVERAGE_COST(HR,DATA_BASE,"Before") * &
                              M_HOURLY_LOAD_B4_SALES(I,HOUR_IN_DAY)
!
               ELSE ! SET PARAMETERS SO THAT NO SALES CAN TAKE PLACE.
                  HOURLY_MARGINAL_COST(I,HOUR_IN_DAY) = &
                                                 TRANSACT_UNSERVED_COST
                  M_HOURLY_PRO_COST_B4_SALES(I,HOUR_IN_DAY) = 999999.  
               ENDIF
               M_HOURLY_MC_B4_SALES(I,HOUR_IN_DAY) = &
                                    HOURLY_MARGINAL_COST(I,HOUR_IN_DAY)
!
! 7/30/99. GAT.
!
               ! hourly_last_price set here.
               HOURLY_LAST_PRICE(I,HOUR_IN_DAY) = &
      HOURLY_MARGINAL_COST(I,HOUR_IN_DAY)
               !
               LAST_TRANS_MC(I) = HOURLY_MARGINAL_COST(I,HOUR_IN_DAY)
            ENDDO
!
             ALLOWED_TRANSACTION_PAIR = 1
             TRANSACTIONS_PER_HOUR = 0
             LONG_PATH_TRANSACTION_PAIR = 0
!
! 5/12/99. GAT. GLOBAL SCARCITY FUNCTION. UPDATED TO INCLUDE ZERO GROUP.
!
! GLOBAL DATABASE.
!
            DATA_BASE = GET_DATA_BASE_FOR_TRANS(INT(0,2),DAY_TYPE)
!            
            r8LoadB4 = M_HOURLY_LOAD_B4_SALES(0,HOUR_IN_DAY)
!
            M_HOURLY_MC_B4_SALES(MARKET_TG,HOUR_IN_DAY) = &
                       GET_MARGINAL_COST_AT_MARKET(r8LoadB4,DATA_BASE, &
                            SCARCITY_COST(MARKET_TG,HOUR_IN_DAY),L_M, &
                            SCARCITY_MULT(MARKET_TG))
!
            IF(GLOBAL_SCARCITY_BUY_N_SELL) THEN
               HOURLY_GLOBAL_SCARCITY = &
                                   SCARCITY_COST(MARKET_TG,HOUR_IN_DAY)
               SCARCITY_COST(MARKET_TG,HOUR_IN_DAY) = 0.
            ELSE
               HOURLY_GLOBAL_SCARCITY = 0.
            ENDIF
!
! FIND DELIVERED COSTS BETWEEN ALL TRANSACTION GROUPS FOR THE NEXT 
! TRANSACTION
!
            FAILED_DIAGNOSTICS = .FALSE.
!
            DO ! DO UNTIL TRANSACTIONS ARE NO LONGER ECONOMIC OR 
               ! PHYICALLY POSSIBLE
!
!
! 01/20/03. BIG TEST OF USING LAST HOUR'S TRANSACTIONS.
!
!         LAST_HOUR_TRANSACTIONS = .TRUE.
!
! 04/25/03. TESTING
!         AGT on 20030429 changed following arrays from REAL to REAL*8:
!  MAX_HOURLY_IMPORT,MAX_HOURLY_EXPORT,HOURLY_TRANSACTION,LAST_HOUR_SELL
!
         REMAIN = MOD(FLOAT(HR),168.) ! RESETS CURVE ONCE A WEEK.
         IF(ABS(REMAIN-1.) > .01  .AND. LAST_HOUR_TRANSACTIONS .AND. &
                    TRANSACTIONS_WITHIN_HOUR(HOUR_IN_DAY) < &
                                         MIN(INT(200,2),LAST_TWH)) THEN

            BUYER = LAST_BUY_FOR_TRANSACTION( &
                               TRANSACTIONS_WITHIN_HOUR(HOUR_IN_DAY)+1)
            SELLER = LAST_SELL_FOR_TRANSACTION( &
                               TRANSACTIONS_WITHIN_HOUR(HOUR_IN_DAY)+1)
!
            SELLER_DB = &
                    GET_DATA_BASE_FOR_TRANS( &
                                GET_TRANS_GROUP_INDEX(SELLER),DAY_TYPE)
            BUYER_DB = &
                    GET_DATA_BASE_FOR_TRANS( &
                                 GET_TRANS_GROUP_INDEX(BUYER),DAY_TYPE)
!
            TRANS_GROUP_SELLER_CAP = TRANS_GROUP_CAP(SELLER_DB) + &
                                           HOURLY_DUMP_CAPACITY(SELLER)
            SELLERS_LOAD = M_HOURLY_LOADS(SELLER,HOUR_IN_DAY)
            BUYERS_LOAD = M_HOURLY_LOADS(BUYER,HOUR_IN_DAY)
!
            TRANS_GROUP_BUYER_CAP = TRANS_GROUP_CAP(BUYER_DB) + &
                                            HOURLY_DUMP_CAPACITY(BUYER)
!
            MAX_LOCAL_IMPORT = MAX(0.d0, &
               MAX_HOURLY_IMPORT(BUYER) + HOURLY_TRANSACTION(BUYER))
            MAX_LOCAL_EXPORT = MAX(0.d0, &
               MAX_HOURLY_EXPORT(SELLER)- HOURLY_TRANSACTION(SELLER))
!
            MAX_HOURLY_RAMP_UP = MAX(0.d0, &
              REAL(TRANS_RAMP_UP(SELLER),8) - &
              HOURLY_TRANSACTION(SELLER)+ LAST_HOUR_SELL(SELLER))
            MAX_HOURLY_RAMP_DOWN = MAX(0.d0, &
              REAL(TRANS_RAMP_DOWN(BUYER),8) - &
              HOURLY_TRANSACTION(BUYER) - LAST_HOUR_SELL(BUYER))
!
            PATH_CAPACITY = HOUR_TIE_LIMIT(SELLER,BUYER)
!
            SELLERS_AVAILABLE_CAPACITY = &
                         MIN( &
                             MAX_HOURLY_RAMP_UP, &
                             MAX_HOURLY_RAMP_DOWN, &
                             MAX_LOCAL_IMPORT, &
                             MAX_LOCAL_EXPORT, &
                             REAL(PATH_CAPACITY,8)*REAL(.99d0,8)) 
                                                             ! 01/23/03.

            BUYERS_LOAD = M_HOURLY_LOADS(BUYER,HOUR_IN_DAY)
!
            TEMP_TRANSACTION = &
                    MIN(sngl(LAST_MW_FOR_TRANSACTION( &
                            TRANSACTIONS_WITHIN_HOUR(HOUR_IN_DAY)+1)), &
                             sngl(SELLERS_AVAILABLE_CAPACITY))
            IF(TEMP_TRANSACTION < 0.1) THEN
               TEMP_TRANSACTION = 0.0
            ENDIF
!
! 07/11/03. TEST
!
!             TEMP_TRANSACTION = NINT(TEMP_TRANSACTION)
!
            TRANSACTIONS_WITHIN_HOUR(HOUR_IN_DAY) = &
                              TRANSACTIONS_WITHIN_HOUR(HOUR_IN_DAY) + 1
            IF(TRANSACTIONS_WITHIN_HOUR(HOUR_IN_DAY) == &
                                         MIN(INT(200,2),LAST_TWH)) THEN

               DO I = 1, UPPER_TRANS_GROUP
                  TG = GET_TRANS_GROUP_INDEX(I)
!
!
! GET MARGINAL COST, IF RESOURCES ARE AVAILABLE, OF THE TRANSACTION 
! GROUP
                  DATA_BASE = GET_DATA_BASE_FOR_TRANS(TG,DAY_TYPE)
!
                     HOURLY_TRANS_GROUP_LOAD_ACTIVE(I) = .TRUE.
!
                  IF(HOURLY_TRANS_GROUP_GEN_ACTIVE(I)) THEN
!
                     IF(HOURLY_TRANS_GROUP_LOAD_ACTIVE(I) .AND. &
                           M_HOURLY_LOADS(I,HOUR_IN_DAY) > 0.01d0) THEN
                        IF(I == SELLER) THEN
                         LOAD_FOR_MC = M_HOURLY_LOADS(I,HOUR_IN_DAY) + &
                                                       TEMP_TRANSACTION
                        ELSEIF(I == BUYER) THEN
                         LOAD_FOR_MC = M_HOURLY_LOADS(I,HOUR_IN_DAY) - &
                                                       TEMP_TRANSACTION
                        ELSE
                           LOAD_FOR_MC = M_HOURLY_LOADS(I,HOUR_IN_DAY)
                        ENDIF
                     ELSE
                        LOAD_FOR_MC = 1.d0 ! ARBITRARY
                     ENDIF
                     HOURLY_MARGINAL_COST(I,HOUR_IN_DAY) = &
                             GET_MARGINAL_COST_AT_MARKET( &
                                    LOAD_FOR_MC,DATA_BASE, &
                                    SCARCITY_COST(I,HOUR_IN_DAY),L_M, &
                                    SCARCITY_MULT(I))
!
                  ELSE
!
!
                  ENDIF
               ENDDO
            ENDIF
         ELSE
!
               IF(FAILED_DIAGNOSTICS) THEN 
               ! MAX TRANSACTIONS AND OTHER DIAGNOSTICS
!
                  EXIT

               ENDIF
               SELLER = 0
               BUYER = 0
               BEST_MARGIN = 0.
               SECOND_BEST_MARGIN = 0.
!
               NET_MARGIN = 0.
               BEST_PRICE_TO_BUYER = 999999.
!
!
!
!
               SELLERS_LOCAL_CAPACITY = 0.
!
               DO I = 1, UPPER_TRANS_GROUP 
                  
                  TG = GET_TRANS_GROUP_INDEX(I)
!
!GET MARGINAL COST, IF RESOURCES ARE AVAILABLE, OF THE TRANSACTION GROUP
!
                  DATA_BASE = GET_DATA_BASE_FOR_TRANS(TG,DAY_TYPE)
!                  
                  IF(HOURLY_TRANS_GROUP_GEN_ACTIVE(I)) THEN
!                    
                     IF(HOURLY_TRANS_GROUP_LOAD_ACTIVE(I) .AND. &
                           M_HOURLY_LOADS(I,HOUR_IN_DAY) > 0.01d0) THEN
                        LOAD_FOR_MC = M_HOURLY_LOADS(I,HOUR_IN_DAY)
                     ELSE
                        LOAD_FOR_MC = 1.d0 ! ARBITRARY
                     ENDIF
!
! 09/10/03. TEST FOR TVA LMP
!                     
                     LOAD_FOR_MC = MAX(LOAD_FOR_MC, &
                                      GET_MUST_RUN_CAPACITY(DATA_BASE))
                     HOURLY_MARGINAL_COST(I,HOUR_IN_DAY) = &
                             GET_MARGINAL_COST_AT_MARKET( &
                                    LOAD_FOR_MC,DATA_BASE, &
                                    SCARCITY_COST(I,HOUR_IN_DAY),L_M, &
                                    SCARCITY_MULT(I))
!
!
                  ELSE 
!
!
                  ENDIF
!
!
! CALCULATE DELIVERED COST OF CURRENT TRANSACTION GROUP, I, TO 
! OTHER GROUPS J
                  IF(HOURLY_TRANS_GROUP_GEN_ACTIVE(I)) THEN
                     
                     SELLER_DB = &
                          GET_DATA_BASE_FOR_TRANS( &
                          GET_TRANS_GROUP_INDEX(I),DAY_TYPE)
                     I_AVAILABLE_CAPACITY = &
                          TRANS_GROUP_CAP(SELLER_DB) + &
                                HOURLY_DUMP_CAPACITY(I) - &
                                          M_HOURLY_LOADS(I,HOUR_IN_DAY)
!                     IF(I_AVAILABLE_CAPACITY < -1.d0) THEN
                     IF(I_AVAILABLE_CAPACITY < -.01d0) THEN
                        HOURLY_MARGINAL_COST(I,HOUR_IN_DAY) = &
                                                 TRANSACT_UNSERVED_COST
                     ELSEIF(HOURLY_DUMP_CAPACITY(I) > 0.) THEN
                        HOURLY_MARGINAL_COST(I,HOUR_IN_DAY) = &
                                                TRANSACT_ABUNDANCE_COST
                     ENDIF
                  ELSEIF(HOURLY_DUMP_CAPACITY(I) > 0.) THEN
                     I_AVAILABLE_CAPACITY = HOURLY_DUMP_CAPACITY(I)
                     HOURLY_MARGINAL_COST(I,HOUR_IN_DAY) = &
                                                TRANSACT_ABUNDANCE_COST
                  ELSE
                     I_AVAILABLE_CAPACITY = 0.d0
                  ENDIF
!     
                  DO J = I-1, 1, -1 ! DON'T CHECK DIAGONAL. 4/21/98. 
!                                     GAT.
! 4/21/98. GAT. TESTING NEW CONSTRAINT LOGIC.
!
                     IF(NEW_TRANS_LOGIC) THEN 
                        IF(.NOT. HOUR_PATH_ACTIVE(I,J) .AND. &
                             LONG_PATH_TRANSACTION_PAIR(I,J) == 0) &
                                      ALLOWED_TRANSACTION_PAIR(I,J) = 0
                        IF(.NOT. HOUR_PATH_ACTIVE(J,I) .AND. &
                             LONG_PATH_TRANSACTION_PAIR(J,I) == 0) &
                                      ALLOWED_TRANSACTION_PAIR(J,I) = 0
                     ELSE
                        IF(HOURLY_TIE_LIMIT(I,J) == 0.0) &
                                      ALLOWED_TRANSACTION_PAIR(I,J) = 0
                        IF(HOURLY_TIE_LIMIT(J,I) == 0.0) &
                                      ALLOWED_TRANSACTION_PAIR(J,I) = 0
                     ENDIF
!
                     IF(HOURLY_TRANS_GROUP_GEN_ACTIVE(J)) THEN 

                        SELLER_DB = &
                          GET_DATA_BASE_FOR_TRANS( &
                             GET_TRANS_GROUP_INDEX(J),DAY_TYPE)
                        J_AVAILABLE_CAPACITY = &
                          TRANS_GROUP_CAP(SELLER_DB) + &
                                   HOURLY_DUMP_CAPACITY(J) - &
                                          M_HOURLY_LOADS(J,HOUR_IN_DAY)
!                        IF(J_AVAILABLE_CAPACITY < -1.d0) THEN
                        IF(J_AVAILABLE_CAPACITY < -.01d0) THEN
                           HOURLY_MARGINAL_COST(J,HOUR_IN_DAY) = &
                                                 TRANSACT_UNSERVED_COST
                        ELSEIF(HOURLY_DUMP_CAPACITY(J) > 0.) THEN
                           HOURLY_MARGINAL_COST(J,HOUR_IN_DAY) = &
                                                TRANSACT_ABUNDANCE_COST
                        ENDIF
                     ELSEIF(HOURLY_DUMP_CAPACITY(J) > 0.) THEN
                        J_AVAILABLE_CAPACITY = &
                           HOURLY_DUMP_CAPACITY(J) - &
                                          M_HOURLY_LOADS(J,HOUR_IN_DAY)
                        HOURLY_MARGINAL_COST(J,HOUR_IN_DAY) = &
                                                TRANSACT_ABUNDANCE_COST
                     ELSE
                        J_AVAILABLE_CAPACITY = 0.d0
                     ENDIF
!                     
                     IF(I_AVAILABLE_CAPACITY > 0.d0 .AND. &
                          ALLOWED_TRANSACTION_PAIR(I,J) > 0 .AND. &
                          HOURLY_TRANS_GROUP_LOAD_ACTIVE(J)) THEN 
                           ! SELLER = I, BUYER = J.

                        TOTAL_DELIVERED_COST(I,J) = &
                                 HOURLY_MARGINAL_COST(I,HOUR_IN_DAY) + &
                                      PATH_WHEELING_CHARGE(I,J, &
                                            PEAK_HOUR,R_MONTH,globecom_year) + &
                                   SCARCITY_COST(MARKET_TG,HOUR_IN_DAY)
!                           
                        IF(TOTAL_DELIVERED_COST(I,J) < &
                                        BEST_PRICE_TO_BUYER(J,1)) THEN
                           BEST_PRICE_TO_BUYER(J,2) = &
                                              BEST_PRICE_TO_BUYER(J,1)
                           BEST_PRICE_TO_BUYER(J,1) = &
                                              TOTAL_DELIVERED_COST(I,J)
                        ELSEIF(TOTAL_DELIVERED_COST(I,J) < & 
                                        BEST_PRICE_TO_BUYER(J,2)) THEN
                           BEST_PRICE_TO_BUYER(J,2) = &
                                              TOTAL_DELIVERED_COST(I,J)
                        ENDIF
!
!
!
!
                        TEMP_MARGIN = &
                                 HOURLY_MARGINAL_COST(J,HOUR_IN_DAY) - &
                                              TOTAL_DELIVERED_COST(I,J)

!                        
                        IF( TEMP_MARGIN > NET_MARGIN(J) .AND. &
               ! NEGATIVE MARGIN MEANS I AM BETTER OFF SELLING TO MYSELF
                          TEMP_MARGIN >=  &
                                  MAX(PATH_SPREAD_RATE(J,I, &
                                         PEAK_HOUR,R_MONTH,globecom_year)+.001, &
                                 PATH_SPREAD_RATE(I,J, &
! 081307. TEMP !!!
                                   PEAK_HOUR,R_MONTH,globecom_year)+.001) .AND. &
! 022207.     
                                 HOURLY_TRANS_GROUP_GEN_ACTIVE(I)) THEN
!     
                              

                              SELLER_DB = &
                                GET_DATA_BASE_FOR_TRANS( &
                                GET_TRANS_GROUP_INDEX(I),DAY_TYPE)
                              BUYER_DB = &
                                GET_DATA_BASE_FOR_TRANS( &
                                GET_TRANS_GROUP_INDEX(J),DAY_TYPE)
!
                              TRANS_GROUP_SELLER_CAP = &
                                          TRANS_GROUP_CAP(SELLER_DB) + &
                                           HOURLY_DUMP_CAPACITY(I)
                              SELLERS_LOAD = &
                                     M_HOURLY_LOADS(I,HOUR_IN_DAY)
                              BUYERS_LOAD = &
                                      M_HOURLY_LOADS(J,HOUR_IN_DAY)
!
                              TRANS_GROUP_BUYER_CAP = &
                                      TRANS_GROUP_CAP(BUYER_DB) + &
                                            HOURLY_DUMP_CAPACITY(J)
!
                              MAX_LOCAL_IMPORT = MAX(0.d0, &
                                      MAX_HOURLY_IMPORT(J) + &
                                            HOURLY_TRANSACTION(J))
                              MAX_LOCAL_EXPORT = MAX(0.d0, &
                                      MAX_HOURLY_EXPORT(I) - &
                                            HOURLY_TRANSACTION(I))
!
                              MAX_HOURLY_RAMP_UP = &
                                   MAX(0.d0,REAL(TRANS_RAMP_UP(I),8) - &
                                           HOURLY_TRANSACTION(I) + &
                                                LAST_HOUR_SELL(I))
                              MAX_HOURLY_RAMP_DOWN = &
                                 MAX(0.d0,REAL(TRANS_RAMP_DOWN(J),8) - &
                                           HOURLY_TRANSACTION(J) - &
                                                LAST_HOUR_SELL(J))
!
                              PATH_CAPACITY = HOUR_TIE_LIMIT(I,J)
!
                              IF(HOURLY_PRICE_RECONCILE) THEN
                                 SELLER_AT_BUYERS_COST = &
                                   GET_PRODUCTION_LEVEL_AT_MARKET( &
                                   MAX(0.,HOURLY_MARGINAL_COST( &
                                                    J,HOUR_IN_DAY)- &
                                             TIE_COST_OF_BEST_MARGIN), &
                                                         SELLER_DB,L_M)
                              ELSE
                                 SELLER_AT_BUYERS_COST = &
                                   GET_PRODUCTION_LEVEL_AT_MARKET( &
                                         HOURLY_MARGINAL_COST( &
                                                       J,HOUR_IN_DAY), &
                                                         SELLER_DB,L_M)


                              ENDIF
!
                              SELLERS_AVAILABLE_CAPACITY = &
                                   MIN( &
                                      MIN(TRANS_GROUP_SELLER_CAP, &
                                            SELLER_AT_BUYERS_COST) - &
                                              SELLERS_LOAD, &
                                              MAX_HOURLY_RAMP_UP, &
                                              MAX_HOURLY_RAMP_DOWN, &
                                              MAX_LOCAL_IMPORT, &
                                              MAX_LOCAL_EXPORT, &
                                              REAL(PATH_CAPACITY,8))
!
                              SELLERS_LOCAL_CAPACITY(I,J) = &
                                             SELLERS_AVAILABLE_CAPACITY
!
!


!
                              IF(SELLERS_AVAILABLE_CAPACITY>0.5d0) THEN
                                 IF(TEMP_MARGIN > NET_MARGIN(J)) THEN
                                    NET_MARGIN(J) = TEMP_MARGIN
                                 ENDIF
                                 IF(TEMP_MARGIN > BEST_MARGIN) THEN
                                    BUYER = J
                                    SELLER = I
                                    SECOND_BEST_MARGIN = BEST_MARGIN
                                    BEST_MARGIN = NET_MARGIN(J)
                                    TIE_COST_OF_BEST_MARGIN = &
                                      PATH_WHEELING_CHARGE(I,J, &
                                              PEAK_HOUR,R_MONTH,globecom_year)
                                 ENDIF
                              ENDIF
                        ENDIF
                     ENDIF
!
                     IF(I /= J .AND. &
! 022207.                     
! 081307. TEMP !!!
                          HOURLY_TRANS_GROUP_GEN_ACTIVE(J) .AND. &
                             J_AVAILABLE_CAPACITY > 0.d0 .AND. &
                             ALLOWED_TRANSACTION_PAIR(J,I) > 0 .AND. &
                             HOURLY_TRANS_GROUP_LOAD_ACTIVE(I)) THEN
! SELLER = J, BUYER = I, ! DOES THE OTHER SIDE OF THE DIAGONAL

                        TOTAL_DELIVERED_COST(J,I) = &
                                 HOURLY_MARGINAL_COST(J,HOUR_IN_DAY) + &
                                      PATH_WHEELING_CHARGE(J,I, &
                                            PEAK_HOUR,R_MONTH,globecom_year) + &
                                   SCARCITY_COST(MARKET_TG,HOUR_IN_DAY)
!                           
                        IF(TOTAL_DELIVERED_COST(J,I) < &
                                        BEST_PRICE_TO_BUYER(I,1)) THEN
                           BEST_PRICE_TO_BUYER(I,2) = &
                                              BEST_PRICE_TO_BUYER(I,1)
                           BEST_PRICE_TO_BUYER(I,1) = &
                                              TOTAL_DELIVERED_COST(J,I)
                        ELSEIF(TOTAL_DELIVERED_COST(J,I) < & 
                                        BEST_PRICE_TO_BUYER(I,2)) THEN
                           BEST_PRICE_TO_BUYER(I,2) = &
                                              TOTAL_DELIVERED_COST(J,I)
                        ENDIF
!
!
                        TEMP_MARGIN = &
                                 HOURLY_MARGINAL_COST(I,HOUR_IN_DAY) - &
                                              TOTAL_DELIVERED_COST(J,I)

                        IF(TEMP_MARGIN > NET_MARGIN(I) .AND. & 
              ! NEGATIVE MARGIN MEANS I AM BETTER OFF SELLING TO MYSELFI
                          TEMP_MARGIN >= &
                                  MAX(PATH_SPREAD_RATE(J,I, &
                                         PEAK_HOUR,R_MONTH,globecom_year)+.001, &
                                 PATH_SPREAD_RATE(I,J, &
                                    PEAK_HOUR,R_MONTH,globecom_year)+.001)) THEN

                              SELLER_DB = &
                                GET_DATA_BASE_FOR_TRANS( &
                                GET_TRANS_GROUP_INDEX(J),DAY_TYPE)

                              BUYER_DB = &
                                GET_DATA_BASE_FOR_TRANS( &
                                GET_TRANS_GROUP_INDEX(I),DAY_TYPE)
!
                                 TRANS_GROUP_SELLER_CAP = &
                                          TRANS_GROUP_CAP(SELLER_DB) + &
                                           HOURLY_DUMP_CAPACITY(I)
                              SELLERS_LOAD = &
                                     M_HOURLY_LOADS(J,HOUR_IN_DAY)
                              BUYERS_LOAD = &
                                      M_HOURLY_LOADS(I,HOUR_IN_DAY)
!
                              TRANS_GROUP_BUYER_CAP = &
                                      TRANS_GROUP_CAP(BUYER_DB) + &
                                            HOURLY_DUMP_CAPACITY(I)
!
                              MAX_LOCAL_IMPORT = MAX(0.d0, &
                                      MAX_HOURLY_IMPORT(I) + &
                                            HOURLY_TRANSACTION(I))
                              MAX_LOCAL_EXPORT = MAX(0.d0, &
                                      MAX_HOURLY_EXPORT(J) - &
                                            HOURLY_TRANSACTION(J))
!
                              MAX_HOURLY_RAMP_UP = &
                                   MAX(0.d0,REAL(TRANS_RAMP_UP(J),8) - &
                                           HOURLY_TRANSACTION(J) + &
                                                LAST_HOUR_SELL(J))
                              MAX_HOURLY_RAMP_DOWN = &
                                 MAX(0.d0,REAL(TRANS_RAMP_DOWN(I),8) - &
                                           HOURLY_TRANSACTION(I) - &
                                                LAST_HOUR_SELL(I))
!
                              PATH_CAPACITY = HOUR_TIE_LIMIT(J,I)
!
                              IF(HOURLY_PRICE_RECONCILE) THEN
                                 TEMP_R1 = &
                                      MAX(0.,HOURLY_MARGINAL_COST( &
                                                       I,HOUR_IN_DAY)- &
                                              TIE_COST_OF_BEST_MARGIN)
                                 SELLER_AT_BUYERS_COST = &
                                   GET_PRODUCTION_LEVEL_AT_MARKET( &
                                                         TEMP_R1, &
                                                         SELLER_DB,L_M)
                              ELSE
                                 SELLER_AT_BUYERS_COST = &
                                   GET_PRODUCTION_LEVEL_AT_MARKET( &
                                         HOURLY_MARGINAL_COST( &
                                                       I,HOUR_IN_DAY), &
                                                         SELLER_DB,L_M)
!

                              ENDIF
!
                              SELLERS_AVAILABLE_CAPACITY = &
                                   MIN( &
                                      MIN(TRANS_GROUP_SELLER_CAP, &
                                            SELLER_AT_BUYERS_COST) - &
                                              SELLERS_LOAD, &
                                              MAX_HOURLY_RAMP_UP, &
                                              MAX_HOURLY_RAMP_DOWN, &
                                              MAX_LOCAL_IMPORT, &
                                              MAX_LOCAL_EXPORT, &
                                              REAL(PATH_CAPACITY,8))
!
                              SELLERS_LOCAL_CAPACITY(J,I) = &
                                             SELLERS_AVAILABLE_CAPACITY
!

!
                              IF(SELLERS_AVAILABLE_CAPACITY>0.5d0) THEN
                                 IF(TEMP_MARGIN > NET_MARGIN(I)) THEN
                                    NET_MARGIN(I) = TEMP_MARGIN
                                 ENDIF
                                 IF(TEMP_MARGIN > BEST_MARGIN) THEN
                                    BUYER = I
                                    SELLER = J
                                    SECOND_BEST_MARGIN = BEST_MARGIN
                                    BEST_MARGIN = NET_MARGIN(I)

                                    TIE_COST_OF_BEST_MARGIN = &
                                      PATH_WHEELING_CHARGE(J,I, &
                                              PEAK_HOUR,R_MONTH,globecom_year)
                                 ENDIF
                              ENDIF
                        ENDIF
                     ENDIF
!                     

                  ENDDO ! J
!
               ENDDO ! DELIVERED COSTS TO ALL TRANSACTION GROUPS
!

!
!
! FIND POTENTIAL (NOT ECONOMIC) SIZE OF THE TRANSACTION.
!
               IF( SELLER > 0 .AND. BUYER > 0 .AND. &
                              BEST_MARGIN > TRANSACTION_TOLERANCE) THEN 
                               ! DID THE MODEL FIND A VALID TRANSACTION?
!
!

!
! 10/24/02. GAT.
!
                  VOID_LOGICAL = SET_HOUR_LONG_CAP_TWH(SELLER,BUYER)

!
                  SELLER_DB = &
                    GET_DATA_BASE_FOR_TRANS( &
                                GET_TRANS_GROUP_INDEX(SELLER),DAY_TYPE)
                  BUYER_DB = &
                    GET_DATA_BASE_FOR_TRANS( &
                                 GET_TRANS_GROUP_INDEX(BUYER),DAY_TYPE)
!
!
                 TRANS_GROUP_SELLER_CAP = TRANS_GROUP_CAP(SELLER_DB) + &
                                           HOURLY_DUMP_CAPACITY(SELLER)
                  TRANS_GROUP_BUYER_CAP = TRANS_GROUP_CAP(BUYER_DB) + &
                                            HOURLY_DUMP_CAPACITY(BUYER)
!
                  TRANSACTIONS_WITHIN_HOUR(HOUR_IN_DAY) = &
                              TRANSACTIONS_WITHIN_HOUR(HOUR_IN_DAY) + 1
!
! 102307.
!            
! Back to original. 111307. IN TEMPORARILY FOR PAC !
!
                  SELLERS_LOAD = M_HOURLY_LOADS(SELLER,HOUR_IN_DAY)

!
!                     
!                     
! 4/21/98. GAT. TESTING NEW CONSTRAINT LOGIC.
!
                  IF(NEW_TRANS_LOGIC) THEN
                     IF(DUTCH_AUCTION_LOGIC .AND. NEW_LONG_PATH) THEN
                        IF(LONG_PATH_TRANSACTION_PAIR(SELLER,BUYER) &
                                                             /= 0) THEN
                           L_P = &
                               LONG_PATH_TRANSACTION_PAIR(SELLER,BUYER)
                           PATH_CAPACITY = &
                                MIN(HOUR_TIE_LIMIT(SELLER,L_P), &
                                    HOUR_TIE_LIMIT(L_P,BUYER))
                        ELSE
                           PATH_CAPACITY = HOUR_TIE_LIMIT(SELLER,BUYER)
                        ENDIF
                     ELSE
                        PATH_CAPACITY = HOUR_TIE_LIMIT(SELLER,BUYER)
                     ENDIF
     
                  ELSE
                     PATH_CAPACITY = HOURLY_TIE_LIMIT(SELLER,BUYER)
                  ENDIF
!
                  MAX_LOCAL_IMPORT = MAX(0.d0, &
                                      MAX_HOURLY_IMPORT(BUYER) + &
                                             HOURLY_TRANSACTION(BUYER))
                  MAX_LOCAL_EXPORT = MAX(0.d0, &
                                      MAX_HOURLY_EXPORT(SELLER) - &
                                            HOURLY_TRANSACTION(SELLER))
!
                  MAX_HOURLY_RAMP_UP = MAX(0.,sngl( &
                    REAL(TRANS_RAMP_UP(SELLER),8) - &
                    HOURLY_TRANSACTION(SELLER)+LAST_HOUR_SELL(SELLER)))
                  MAX_HOURLY_RAMP_DOWN = MAX(0.,sngl( &
                    REAL(TRANS_RAMP_DOWN(BUYER),8) - &
                    HOURLY_TRANSACTION(BUYER) -LAST_HOUR_SELL(BUYER)))
!
                  SELLERS_AVAILABLE_CAPACITY = &
                       MIN(TRANS_GROUP_SELLER_CAP - SELLERS_LOAD, &
                                              MAX_HOURLY_RAMP_UP, &
                                              MAX_HOURLY_RAMP_DOWN, &
                                              MAX_LOCAL_IMPORT, &
                                              MAX_LOCAL_EXPORT, &
                                              REAL(PATH_CAPACITY,8), &
                                      REAL(MAXIMUM_TRANSACTION_SIZE,8))
!
                  BUYERS_LOAD = M_HOURLY_LOADS(BUYER,HOUR_IN_DAY)
!
! 06/03/04.
                  IF(TRANS_GROUP_BUYER_CAP < BUYERS_LOAD) THEN
                     BUYERS_UNSERVED_ENERGY = &
                                    BUYERS_LOAD - TRANS_GROUP_BUYER_CAP
                  ELSE
                     BUYERS_UNSERVED_ENERGY = 0.
                  ENDIF
!                  
! 102513. REMOVED FOR AMEREN.
!

!
                  IF(HOURLY_TRANS_GROUP_GEN_ACTIVE(BUYER)) THEN
!
! 5/22/02. I THINK THIS "IF" CONDITION IS WRONG.
!


                        BUYER_AT_SELLERS_COST = &
                          GET_PRODUCTION_LEVEL_AT_MARKET( &
                            HOURLY_MARGINAL_COST(SELLER,HOUR_IN_DAY) + &
                                              TIE_COST_OF_BEST_MARGIN, &
                                                          BUYER_DB,L_M)

                  ELSE
                     BUYER_AT_SELLERS_COST = 0.d0
                  ENDIF
!
!THE AMOUNT THAT THE BUYER IS ABLE TO PRODUCE AT THE SELLERS COST SHOULD
!BE LESS THAN OR EQUAL TO THE AMOUNT THAT THE BUYER IS ABLE PRODUCE AT
!ITS OWN COST. IE SELLERS_COST + TIE_COST... < BUYERS_COST AND 
! BUYERS_COST'S ARE NON-DECREASING.
!
                  TEMP_BUYER_TRANSACTION = &
                                    BUYERS_LOAD - BUYER_AT_SELLERS_COST
!
!
!
                  IF(HOURLY_PRICE_RECONCILE) THEN
                     SELLER_AT_BUYERS_COST = &
                      GET_PRODUCTION_LEVEL_AT_MARKET( &
                       MAX(0.,HOURLY_MARGINAL_COST(BUYER,HOUR_IN_DAY)- &
                                             TIE_COST_OF_BEST_MARGIN), &
                                                         SELLER_DB,L_M)
                  ELSE
                     SELLER_AT_BUYERS_COST = &
                       GET_PRODUCTION_LEVEL_AT_MARKET( &
                          HOURLY_MARGINAL_COST(BUYER,HOUR_IN_DAY), &
                                                         SELLER_DB,L_M)
                  ENDIF
                  TEMP_SELLER_TRANSACTION = &
                          HOURLY_DUMP_CAPACITY(SELLER) + &
                                   SELLER_AT_BUYERS_COST - SELLERS_LOAD
!
                  TEMP_INTERP_TRANSACTION = MAX(0.d0,MIN( &
                               SELLERS_AVAILABLE_CAPACITY,BUYERS_LOAD, &
                       TEMP_BUYER_TRANSACTION,TEMP_SELLER_TRANSACTION))
!
                  TEMP_TRANSACTION = MAX(0.,MIN( &
                   sngl(SELLERS_AVAILABLE_CAPACITY),sngl(BUYERS_LOAD)))
!
!
! DUTCH AUCTION LOGIC.
!
! SELLER SELLS AT SECOND BEST PRICE
!
                  SELLER_SECOND_BEST_TRANS = &
                       GET_PRODUCTION_LEVEL_AT_MARKET( &
                                   BEST_PRICE_TO_BUYER(BUYER,2), &
                                   SELLER_DB,L_M) + &
                                          HOURLY_DUMP_CAPACITY(SELLER) &
                                                         - SELLERS_LOAD
                  APPLY_SECOND_BEST_TRAN = .FALSE.
                  IF(BUYER_DB > 0 .AND. &
                    SELLER_SECOND_BEST_TRANS < &
                             TEMP_INTERP_TRANSACTION .AND. &
                    BUYERS_LOAD - SELLER_SECOND_BEST_TRANS > 0.d0) THEN
                     BUYERS_COST = &
                          GET_MARGINAL_COST_AT_MARKET( &
                            BUYERS_LOAD-SELLER_SECOND_BEST_TRANS, &
                                BUYER_DB, &
                                SCARCITY_COST(BUYER,HOUR_IN_DAY),L_M, &
                                SCARCITY_MULT(BUYER))
                     IF(BUYERS_COST > BEST_PRICE_TO_BUYER(BUYER,2)) THEN
                        APPLY_SECOND_BEST_TRAN = .TRUE.
                     ENDIF
                  ENDIF
!                  
                  MINIMUM_MARGIN = &
                               MAX(PATH_SPREAD_RATE(BUYER,SELLER, &
                                         PEAK_HOUR,R_MONTH,globecom_year)+.001, &
                                   PATH_SPREAD_RATE(SELLER,BUYER, &
                                         PEAK_HOUR,R_MONTH,globecom_year)+.001, &
                                   SECOND_BEST_MARGIN)

               ELSE
!
                  EXIT ! NO TRANSACTIONS POSSIBLE AFTER 
                       ! CALCULATING MARGINS. GO ON TO THE NEXT HOUR.
               ENDIF
!
               HOURLY_DUMP_USED = 0.
               CONVERGENCE_PATH = -99.
! 06/03/04.
               IF(YES_STRICTLY_POSITIVE_MARGIN .AND. &
                       BUYERS_UNSERVED_ENERGY > 0.1 .AND. & 
                              sngl(TEMP_INTERP_TRANSACTION) > 0.1) THEN
                 TEMP_TRANSACTION = MIN(REAL(TEMP_INTERP_TRANSACTION), &
                                            BUYERS_UNSERVED_ENERGY+.01)
                  R8TEMP = SELLERS_LOAD+REAL(TEMP_TRANSACTION,8)
                  SELLERS_COST = &
                    GET_MARGINAL_COST_AT_MARKET( &
                    R8TEMP, &
                     SELLER_DB,SCARCITY_COST(SELLER,HOUR_IN_DAY),L_M, &
                     SCARCITY_MULT(SELLER))
                IF(BUYER_DB > 0 .AND. BUYERS_LOAD - TEMP_TRANSACTION < &
                                            TRANS_GROUP_BUYER_CAP) THEN
                     R8TEMP = BUYERS_LOAD-REAL(TEMP_TRANSACTION,8)
                     BUYERS_COST = &
                          GET_MARGINAL_COST_AT_MARKET( &
                                R8TEMP,BUYER_DB, &
                                SCARCITY_COST(BUYER,HOUR_IN_DAY),L_M, &
                                SCARCITY_MULT(BUYER))
                  ELSE
                     BUYERS_COST = &
                                HOURLY_MARGINAL_COST(BUYER,HOUR_IN_DAY)
                     IF(BUYER_DB > 0) THEN
                        r8TEMP = PUT_BUYERS_LOAD( &
                           sngl(BUYERS_LOAD-TEMP_TRANSACTION),BUYER_DB)
                     ENDIF
                  ENDIF
                  CONVERGENCE_PATH = 41.
! 070108. MAJOR LOGIC CHANGE: TREAT DUMP UP TO ZERO DUMP
!
               ELSEIF(HOURLY_DUMP_CAPACITY(SELLER) > .01) THEN
                 HOURLY_DUMP_USED = MIN(sngl(TEMP_INTERP_TRANSACTION), &
                                          HOURLY_DUMP_CAPACITY(SELLER))
!
! 052808. TEST TO ADDRESS THE ERCOT WIND PROBLEM.
!     
                  TEMP_TRANSACTION = HOURLY_DUMP_USED
!                  TEMP_TRANSACTION = 0.
                  CONVERGENCE_PATH = 1.
!
!

               ELSEIF(TEMP_INTERP_TRANSACTION > 0.5d0) THEN
                                              ! MINIMUM SIZE TRANSACTION
                  TEMP_INTERP_TRANSACTION = TEMP_INTERP_TRANSACTION - &
                                           HOURLY_DUMP_CAPACITY(SELLER)
                  HOURLY_DUMP_USED = HOURLY_DUMP_CAPACITY(SELLER)
!                  
                  TEMP_TRANSACTION = TEMP_INTERP_TRANSACTION
                  MIN_TRANSACTION = 0.d0
                  MAX_TRANSACTION = TEMP_INTERP_TRANSACTION
                  R8TEMP = SELLERS_LOAD+REAL(TEMP_TRANSACTION,8)
                  SELLERS_COST = &
                    GET_MARGINAL_COST_AT_MARKET( &
                     R8TEMP, &
                     SELLER_DB,SCARCITY_COST(SELLER,HOUR_IN_DAY),L_M, &
                     SCARCITY_MULT(SELLER))
                IF(BUYER_DB > 0 .AND. BUYERS_LOAD - TEMP_TRANSACTION < &
                                            TRANS_GROUP_BUYER_CAP) THEN
                     R8TEMP = BUYERS_LOAD-REAL(TEMP_TRANSACTION,8)
                     BUYERS_COST = &
                          GET_MARGINAL_COST_AT_MARKET( &
                                R8TEMP,BUYER_DB, &
                                SCARCITY_COST(BUYER,HOUR_IN_DAY),L_M, &
                                SCARCITY_MULT(BUYER))
                  ELSE
                     BUYERS_COST = &
                                HOURLY_MARGINAL_COST(BUYER,HOUR_IN_DAY)
                     IF(BUYER_DB > 0) THEN
                        r8TEMP = PUT_BUYERS_LOAD( &
                           sngl(BUYERS_LOAD-TEMP_TRANSACTION),BUYER_DB)
                     ENDIF
                  ENDIF
!
! TEST FOR SPECIAL CONDITIONS:
!
!     1. SELLER SELLS TEMP_INTERP_TRANSACTION, ITS COSTS DON'T CHANGE
!        AND THE BUYERS COSTS ARE STILL GREATER THAN THE SELLER.
!        (INELASTIC (UNDEFINED?)SUPPLY)
!
!     2. BUYER BUYS TEMP_INTERP_TRANSACTION, ITS COSTS DON'T CHANGE
!        AND THE SELLERS COSTS ARE STILL LESS THAN THE BUYER.
!        (INELASTIC (UNDEFINED?) DEMAND)
!
!        IF EITHER CONDITION OCCURS, ALLOW THE ENTIRE TRANSACTION
!        TO TAKE PLACE.
!
! THIS SECTION MOVED UP TO ACCOMODATE SPECIAL CONDITIONS.
!
! 11/5/99. SHOULDN'T THIS INCLUDE SCARCITY?
!
                  IF(.NOT. HOURLY_PRICE_RECONCILE) THEN
                     MIN_PRICE = &
                       REAL(HOURLY_MARGINAL_COST(SELLER,HOUR_IN_DAY) + &
                                             TIE_COST_OF_BEST_MARGIN,8)
                     MAX_PRICE = HOURLY_MARGINAL_COST(BUYER,HOUR_IN_DAY)
                  ELSE
                     MIN_PRICE = MAX(0.d0, &
                       REAL(HOURLY_MARGINAL_COST(SELLER,HOUR_IN_DAY) - &
                                            TIE_COST_OF_BEST_MARGIN,8))
                     MAX_PRICE = &
                       MAX(MIN_PRICE, &
                        REAL(HOURLY_MARGINAL_COST(BUYER,HOUR_IN_DAY) - &
                                            TIE_COST_OF_BEST_MARGIN,8))
                  ENDIF
                  SEARCHES_WITHIN_TRANSACTIONS = 0
                  TEMP_TRANSACTION = 0.
                  TRANSACTION_COMPLETED = .FALSE.
!
!
!
                  SELLERS_PRICE_DIDNT_CHANGE = &
                         SELLERS_COST == &
                             HOURLY_MARGINAL_COST(SELLER,HOUR_IN_DAY) 
                  BUYERS_PRICE_DIDNT_CHANGE = & 
                         BUYERS_COST ==  &
                                HOURLY_MARGINAL_COST(BUYER,HOUR_IN_DAY)
                  TOTAL_DELIVERED_COST(SELLER,BUYER) = SELLERS_COST + &
                                           TIE_COST_OF_BEST_MARGIN + &
                                   SCARCITY_COST(MARKET_TG,HOUR_IN_DAY)
                  IF(.NOT. YES_STRICTLY_POSITIVE_MARGIN .OR. &
                            TOTAL_DELIVERED_COST(SELLER,BUYER) < & 
                                                      BUYERS_COST) THEN
                     IF(SELLERS_PRICE_DIDNT_CHANGE .AND. &
                       (BUYERS_LOAD == TEMP_INTERP_TRANSACTION) ) THEN
                        TEMP_TRANSACTION = BUYERS_LOAD
                        TRANSACTION_COMPLETED = .TRUE.
                        CONVERGENCE_PATH = 21.
!
! IF YOU CAN SELL THE WHOLE AMOUNT AND THE DELIVERED COST IS STILL LESS,
! THEN DO IT.
!

                     ELSEIF(BUYERS_PRICE_DIDNT_CHANGE .AND. &
                           TEMP_INTERP_TRANSACTION == &
                           SELLERS_AVAILABLE_CAPACITY)THEN
                        TEMP_TRANSACTION = SELLERS_AVAILABLE_CAPACITY
                        TRANSACTION_COMPLETED = .TRUE.
                        CONVERGENCE_PATH = 22.
                     ENDIF
                  ENDIF
!

!
                  IF(TOTAL_DELIVERED_COST(SELLER,BUYER) &
                                         + MINIMUM_MARGIN >= &
                                                      BUYERS_COST) THEN
! SEARCH FOR MAXIMUM POSSIBLE TRANSACTION
!
! ASSUME FUNCTIONS ARE APPROXIMATELY LINEAR AND SOLVE FOR 
! THE INTERSECTION OF THE TWO LINES.
! THE TWO LINES HAVE COMMON MINIMUM AND MAXIMUM PRICES (I THINK).
! FURTHER ASSUME THAT THE BUYER IS A TRADITIONAL DOWNWARD SLOPING DEMAND
! CURVE, AND THE SELLER IS THE TRADITIONAL UPWARD SLOPING DEMAND CURVE.
! ALIGN THE TWO LINES SO THAT THE MAX PRICE IS SET TO THE ORIGIN.
! THIS MEANS THAT THE INTERSECTION OCCURS AT A NEGATIVE TRANSACTION 
! VALUE. THE ASSUMPTION OF LINEARITY MAY BE TESTED BY SEEING HOW close 
! THE RECALCULATED PRICES ARE TO THE PRICES ESTIMATED BY THE INTERSECTED
! LINES.
!

!
! TWO EQUATIONS, TWO UNKNOWNS
!                     
                     IF(YES_STRICTLY_POSITIVE_MARGIN) THEN
                        TEMP_R2 = TIE_COST_OF_BEST_MARGIN + &
                                   SCARCITY_COST(MARKET_TG,HOUR_IN_DAY)
                        CALL FIND_PRICE_AND_QUANT( &
                                   HR, &
                                   R_MONTH, &
                                   TEMP_TRANSACTION, &
                                   TEMP_INTERP_TRANSACTION, &
                                   TRANSACTION_COMPLETED, &
                                   BUYER_DB, &
                                   SELLER_DB, &
                                   BUYERS_COST, &
                                   SELLERS_COST, &
                                   CONVERGENCE_PATH, &
                                   SELLERS_LOAD, &
                                   BUYERS_LOAD, &
                                   SCARCITY_COST(SELLER,HOUR_IN_DAY), &
                                   SCARCITY_COST(BUYER,HOUR_IN_DAY), &
                                   SCARCITY_MULT(SELLER), &
                                   SCARCITY_MULT(BUYER), &
                                   L_M, &
                                   TEMP_R2, &
                                   TOTAL_DELIVERED_COST(SELLER,BUYER), &
                                   SEARCHES_WITHIN_TRANSACTIONS, &
                                   TWH, &
                                   HOURLY_DUMP_USED)
                     ENDIF

                     DO WHILE(.NOT. TRANSACTION_COMPLETED)
                        IF(TEMP_SELLER_TRANSACTION == MIN_TRANSACTION) &
                                                                   THEN
                           TEMP_TRANSACTION = MIN_TRANSACTION
                           TRANSACTION_COMPLETED = .TRUE.
                        ELSEIF(TEMP_BUYER_TRANSACTION == &
                                                  MIN_TRANSACTION) THEN
                           TEMP_TRANSACTION = MAX_TRANSACTION
                           TRANSACTION_COMPLETED = .TRUE.
                           CONVERGENCE_PATH = 23.
                        ELSE
                           DENOM = &
                              TEMP_SELLER_TRANSACTION - MIN_TRANSACTION
                           IF(DENOM == 0.d0) THEN
                              WRITE(4,*)'*** line 6351 TRANSOBJ.FOR ***'
                              WRITE(4,*) &
                                       "IRREGULAR COST CURVES DETECTED"
                       er_message='See WARNING MESSAGES -transobj.for-9'
                              call end_program(er_message)
                           ENDIF
                           SELLER_SLOPE = (MAX_PRICE - MIN_PRICE) /DENOM
                                                                ! unused

                           DENOM = &
                               MIN_TRANSACTION - TEMP_BUYER_TRANSACTION
                           IF(DENOM == 0.d0) THEN
                              WRITE(4,*)'*** line 6363 TRANSOBJ.FOR ***'
                              WRITE(4,*) &
                                       "IRREGULAR COST CURVES DETECTED"
                     er_message='See WARNING MESSAGES -transobj.for-10'
                              call end_program(er_message)
                           ENDIF
                           BUYER_SLOPE = (MAX_PRICE - MIN_PRICE) / DENOM
                                                                ! unused
                           IF(SELLER_SLOPE == BUYER_SLOPE) THEN

                              WRITE(4,*) "Slopes identical",SELLER_SLOPE
                              SELLER_SLOPE = BUYER_SLOPE + .1d0 ! unused
                                  
                           ENDIF


!
                           DENOM_1 = &
                              TEMP_SELLER_TRANSACTION - MIN_TRANSACTION
                           DENOM_2 = &
                               TEMP_BUYER_TRANSACTION - MIN_TRANSACTION
!
                           DENOM = (DENOM_1*DENOM_2)/(DENOM_1+DENOM_2)
                           TEMP_TRANSACTION = TEMP_TRANSACTION  + DENOM
!
                           IF(TEMP_TRANSACTION > &
                                  sngl(SELLERS_AVAILABLE_CAPACITY - &
                                          HOURLY_DUMP_USED)) THEN
                              TEMP_TRANSACTION = &
                                       SELLERS_AVAILABLE_CAPACITY - &
                                                       HOURLY_DUMP_USED
!                              IF(.NOT. PRICE_LAST_MW) THEN
                                 TRANSACTION_COMPLETED = .TRUE.
                                 CONVERGENCE_PATH = 24.
!                              ELSE
!                                 TEMP_TRANSACTION = TEMP_TRANSACTION/5.
!                              ENDIF
                           ENDIF
!
                        ENDIF
!
                        R8TEMP = SELLERS_LOAD+REAL(TEMP_TRANSACTION,8)
                        SELLERS_COST = &
                          GET_MARGINAL_COST_AT_MARKET( &
                                   R8TEMP, &
                                   SELLER_DB, &
                               SCARCITY_COST(SELLER,HOUR_IN_DAY),L_M, &
                               SCARCITY_MULT(SELLER))
                        TOTAL_DELIVERED_COST(SELLER,BUYER) = &
                                SELLERS_COST + &
                                         TIE_COST_OF_BEST_MARGIN + &
                                   SCARCITY_COST(MARKET_TG,HOUR_IN_DAY)
                        IF(BUYER_DB > 0 .AND. &
                          BUYERS_LOAD - TEMP_TRANSACTION < &
                                            TRANS_GROUP_BUYER_CAP) THEN
                           R8TEMP = BUYERS_LOAD-REAL(TEMP_TRANSACTION,8)
                           BUYERS_COST = &
                             GET_MARGINAL_COST_AT_MARKET( &
                             R8TEMP,BUYER_DB, &
                             SCARCITY_COST(BUYER,HOUR_IN_DAY),L_M, &
                             SCARCITY_MULT(BUYER))
                        ELSE
                           BUYERS_COST = &
                                HOURLY_MARGINAL_COST(BUYER,HOUR_IN_DAY)
                           IF(BUYER_DB > 0) THEN
                              r8TEMP = PUT_BUYERS_LOAD( &
                                   sngl(BUYERS_LOAD-TEMP_TRANSACTION), &
                                                              BUYER_DB)
                           ENDIF
                        ENDIF
                        SEARCHES_WITHIN_TRANSACTIONS = &
                                       SEARCHES_WITHIN_TRANSACTIONS + 1
                        IF(SEARCHES_WITHIN_TRANSACTIONS > 5) THEN
                           TRANSACTION_COMPLETED = .TRUE.
                        ELSEIF(SELLERS_COST == &
                        HOURLY_MARGINAL_COST(SELLER,HOUR_IN_DAY) .AND. &
                          BUYERS_COST == &
                          HOURLY_MARGINAL_COST(BUYER,HOUR_IN_DAY)) THEN
!
!                          TRANSACTION WAS NOT BIG ENOUGH. FLAT PART OF 
!                          THE COST CURVES.
                           MIN_TRANSACTION = TEMP_TRANSACTION

                        ELSE
                           TRANSACTION_COMPLETED = .TRUE.
                           CONVERGENCE_PATH = 25.
                        ENDIF
                     ENDDO
                  ELSE ! THE COST CURVES DO NOT INTERSECT.
!                        IE YOU CAN DO THE TRANSACTION WITHOUT COSTS
                     TEMP_TRANSACTION = TEMP_INTERP_TRANSACTION
!                     

                     TRANSACTION_COMPLETED = .TRUE.
                     CONVERGENCE_PATH = 26.
                  ENDIF
               ELSEIF(TWH <= 2*UPPER_TRANS_GROUP  .AND. &
                                   TEMP_INTERP_TRANSACTION > 0.d0) THEN
                                          ! ADDED FOR SRP. 8/24/99. GAT.
                  TEMP_TRANSACTION = TEMP_INTERP_TRANSACTION
                  TRANSACTION_COMPLETED = .TRUE.
                  CONVERGENCE_PATH = 3.
! DEFAULT CASE. 071109 (CONVERGENT CASE = -99)                  
               ELSEIF(TEMP_TRANSACTION > 0.) THEN 
                  MIN_TRANSACTION = 0.0d0
                  MAX_TRANSACTION = TEMP_TRANSACTION
                  R8TEMP = SELLERS_LOAD+REAL(TEMP_TRANSACTION,8)
                  SELLERS_COST = &
                       GET_MARGINAL_COST_AT_MARKET( &
                                R8TEMP, &
                                SELLER_DB, &
                                SCARCITY_COST(SELLER,HOUR_IN_DAY),L_M, &
                                SCARCITY_MULT(SELLER))
                  TOTAL_DELIVERED_COST(SELLER,BUYER) = SELLERS_COST + &
                                         TIE_COST_OF_BEST_MARGIN + &
                                   SCARCITY_COST(MARKET_TG,HOUR_IN_DAY)
                IF(BUYER_DB > 0 .AND. BUYERS_LOAD - TEMP_TRANSACTION < &
                                            TRANS_GROUP_BUYER_CAP) THEN
                     R8TEMP = BUYERS_LOAD-REAL(TEMP_TRANSACTION,8)
                     BUYERS_COST = &
                       GET_MARGINAL_COST_AT_MARKET( &
                                R8TEMP, &
                                BUYER_DB, &
                                SCARCITY_COST(BUYER,HOUR_IN_DAY),L_M, &
                                SCARCITY_MULT(BUYER))
                  ELSE
                     BUYERS_COST = &
                                HOURLY_MARGINAL_COST(BUYER,HOUR_IN_DAY) 
                     IF(BUYER_DB > 0) THEN
                        r8TEMP = PUT_BUYERS_LOAD( &
                           sngl(BUYERS_LOAD-TEMP_TRANSACTION),BUYER_DB)
                     ENDIF
                  ENDIF
                  IF(TOTAL_DELIVERED_COST(SELLER,BUYER) + & 
                                    MINIMUM_MARGIN >= BUYERS_COST) THEN 
!                  
! IF IT CAN TAKE TEMP_TRANSACTION, THEN DON'T BOTHER TO SEARCH.
!
                     SEARCHES_WITHIN_TRANSACTIONS = 0
!                     
                     DO
!
                        SEARCHES_WITHIN_TRANSACTIONS = &
                                       SEARCHES_WITHIN_TRANSACTIONS + 1
!     
                        IF(ABS(BUYERS_COST - &
                                   TOTAL_DELIVERED_COST(SELLER,BUYER)) &
                                          < TRANSACTION_TOLERANCE .OR. &
                                 SEARCHES_WITHIN_TRANSACTIONS > 5) THEN
                           EXIT                        
                        ENDIF
!     
                        TEMP_TRANSACTION_CHANGE = TEMP_TRANSACTION
                        TEMP_TRANSACTION = &
                                   (MAX_TRANSACTION+MIN_TRANSACTION)/2.
                        TEMP_TRANSACTION_CHANGE = &
                             ABS(TEMP_TRANSACTION_CHANGE - &
                                                      TEMP_TRANSACTION)
!
                        R8TEMP = SELLERS_LOAD+REAL(TEMP_TRANSACTION,8)
                        SELLERS_COST = &
                          GET_MARGINAL_COST_AT_MARKET( &
                                   R8TEMP, &
                                   SELLER_DB, &
                               SCARCITY_COST(SELLER,HOUR_IN_DAY),L_M, &
                               SCARCITY_MULT(SELLER))
                        TOTAL_DELIVERED_COST(SELLER,BUYER) = &
                                SELLERS_COST + &
                                         TIE_COST_OF_BEST_MARGIN + &
                                   SCARCITY_COST(MARKET_TG,HOUR_IN_DAY)
                        IF(BUYER_DB > 0 .AND. &
                             BUYERS_LOAD - TEMP_TRANSACTION < &
                                            TRANS_GROUP_BUYER_CAP) THEN
                           R8TEMP = BUYERS_LOAD-REAL(TEMP_TRANSACTION,8)
                           BUYERS_COST = &
                             GET_MARGINAL_COST_AT_MARKET( &
                                R8TEMP, &
                                BUYER_DB, &
                                SCARCITY_COST(BUYER,HOUR_IN_DAY),L_M, &
                                SCARCITY_MULT(BUYER))
                        ELSE
                           BUYERS_COST = &
                                HOURLY_MARGINAL_COST(BUYER,HOUR_IN_DAY) 
                           IF(BUYER_DB > 0) THEN
                              r8TEMP = PUT_BUYERS_LOAD( &
                                sngl(BUYERS_LOAD-TEMP_TRANSACTION), &
                                                              BUYER_DB)
                           ENDIF
                        ENDIF
!
                        IF(TOTAL_DELIVERED_COST(SELLER,BUYER) < &
                                                      BUYERS_COST) THEN 
                           MIN_TRANSACTION = TEMP_TRANSACTION
                        ELSE
                           MAX_TRANSACTION = TEMP_TRANSACTION
                        ENDIF
                     ENDDO
                  ENDIF
               ENDIF ! DIFFERENT BUYER/SELLER TRANSACTIONS
          ENDIF ! PREVIOUS HOUR'S LOGIC
!
! HOURLY_DUMP LOGIC
!
               HOURLY_DUMP_CAPACITY(SELLER) = &
                        HOURLY_DUMP_CAPACITY(SELLER) - HOURLY_DUMP_USED
               IF(HOURLY_DUMP_CAPACITY(SELLER) < .01) & ! 070108.
                                    HOURLY_DUMP_CAPACITY(SELLER) = 0.0
!
! FOR TRACKING REDUNDENT TRANSACTIONS
!              
               TWH = TRANSACTIONS_WITHIN_HOUR(HOUR_IN_DAY)
!
               TEMP_R1 = PATH_WHEELING_CHARGE(SELLER,BUYER, &
                                    PEAK_HOUR,R_MONTH,globecom_year)
!
!
               IF(HOURLY_DIAGNOSTICS) THEN
!
                  TOTAL_TRANS_PER_MONTH = TOTAL_TRANS_PER_MONTH + 1
!                  
                  BUY_FOR_TRANSACTION(TWH) = BUYER
                  SELL_FOR_TRANSACTION(TWH) = SELLER
                  MARGIN_FOR_TRANSACTION(TWH) = BEST_MARGIN
                  MW_FOR_TRANSACTION(TWH)= TEMP_TRANSACTION
!
!                  MIN_ITER_B4_LONG_PATH = 3 * UPPER_TRANS_GROUP
!
                  IF(YES_DISALLOW_REDUNDANT_TRANS) THEN
                     MIN_ITER_B4_LONG_PATH = 10
                  ELSE
                     MIN_ITER_B4_LONG_PATH = 1000
                  ENDIF
!
                  IF(TWH > MIN_ITER_B4_LONG_PATH) THEN
                     IF((BUY_FOR_TRANSACTION(TWH) == &
                                   BUY_FOR_TRANSACTION(TWH-1)) .AND. &
                          (SELL_FOR_TRANSACTION(TWH) == &
                                   SELL_FOR_TRANSACTION(TWH-1)) ) THEN
                        CUM_REDUNDANT_TRANSACTION(TWH) = 1
                     ENDIF
                     CUM_REDUNDANT_TRANSACTION(TWH) = &
                             CUM_REDUNDANT_TRANSACTION(TWH) + &
                                       CUM_REDUNDANT_TRANSACTION(TWH-1) 
! 032110.
!                     IF(CUM_REDUNDANT_TRANSACTION(TWH) > 6) THEN
                     IF(CUM_REDUNDANT_TRANSACTION(TWH) > 3) THEN
! TEST DISALLOWING THE REDUNDANT PATH
!
! 01/07/04. ADDED FOR HORNUNG
!
                        IF(YES_DISALLOW_REDUNDANT_TRANS) THEN
                           ALLOWED_TRANSACTION_PAIR(SELLER,BUYER) = 0 
                        ELSE
! 12/19/02. PUT-IN.
                           CUM_REDUNDANT_TRANSACTION(TWH) = 1
                        ENDIF
                     ENDIF
!
                     IF(DUTCH_AUCTION_LOGIC .AND. NEW_LONG_PATH) THEN
                        IF(SELL_FOR_TRANSACTION(TWH) == & 
                                      BUY_FOR_TRANSACTION(TWH-1) .AND. &
                             SELL_FOR_TRANSACTION(TWH) /=  &
                                       BUY_FOR_TRANSACTION(TWH) .AND. &
                             LONG_PATH_TRANSACTION_PAIR( &
                                SELL_FOR_TRANSACTION(TWH-1), &
                                BUY_FOR_TRANSACTION(TWH)) == 0 .AND. &
                             ALLOWED_TRANSACTION_PAIR( &
                                SELL_FOR_TRANSACTION(TWH-1), &
                               BUY_FOR_TRANSACTION(TWH-1)) == 1  .AND. &
                             ALLOWED_TRANSACTION_PAIR( &
                                SELL_FOR_TRANSACTION(TWH), &
                                BUY_FOR_TRANSACTION(TWH)) == 1) THEN
                           LONG_PATH_TRANSACTION(TWH) = 1
!
!!
!! TEST ALLOWING THE LONG PATH
!!
!
!
!
                           LONG_PATH_TRANSACTION_PAIR( &
                             SELL_FOR_TRANSACTION(TWH-1), &
                             BUY_FOR_TRANSACTION(TWH)) = &
                                              SELL_FOR_TRANSACTION(TWH)
                           ALLOWED_TRANSACTION_PAIR( &
                                   SELL_FOR_TRANSACTION(TWH-1), &
                                        BUY_FOR_TRANSACTION(TWH)) = 1
                        ENDIF
                        LONG_PATH_TRANSACTION(TWH) = &
                             LONG_PATH_TRANSACTION(TWH) + &
                                           LONG_PATH_TRANSACTION(TWH-1) 
                     ELSE
! 062104 FIRST CONDITION.                     
!
                        IF((BUY_FOR_TRANSACTION(TWH) == &
                                   SELL_FOR_TRANSACTION(TWH-1)) .AND. &
                          (SELL_FOR_TRANSACTION(TWH-1) == &
                                   BUY_FOR_TRANSACTION(TWH-2))  .AND. &
                          (BUY_FOR_TRANSACTION(TWH-2) == &
                                  SELL_FOR_TRANSACTION(TWH-3))  .AND. &
                          (SELL_FOR_TRANSACTION(TWH) == &
                                   SELL_FOR_TRANSACTION(TWH-2)) .AND. &
                          (BUY_FOR_TRANSACTION(TWH-1) == &
                                  BUY_FOR_TRANSACTION(TWH-3))) THEN
                           LONG_PATH_TRANSACTION(TWH) = 1
!
!
! 8/5/02. FOR BURESH: SEEING LONG-PATH VIOLATED TRANSMISSION CONSTRAINTS
!
                           IF(BURESH) THEN
                              L_P = BUY_FOR_TRANSACTION(TWH)
! THIS DEFINES THE MODEL CREATED LONG PATH     
                                 TEMP_R1 = &
                                      MIN(HOUR_TIE_LIMIT( &
                                       SELL_FOR_TRANSACTION(TWH),L_P), &
                                       HOUR_TIE_LIMIT(L_P, &
                                           BUY_FOR_TRANSACTION(TWH-1)))
                                 IF(TEMP_R1 < 0.) THEN
                                    TEMP_R1 = 0.
                                 ENDIF
                                 TEMP_L = &
                                       SET_HOUR_LONG_PATH_PARAMS( &
                                          TEMP_R1, &
                                          SELL_FOR_TRANSACTION(TWH), &
                                          BUY_FOR_TRANSACTION(TWH-1), &
                                          L_P)
!
                              IF(TEMP_L) THEN                           
                                 LONG_PATH_TRANSACTION_PAIR( &
                                   SELL_FOR_TRANSACTION(TWH), &
                                        BUY_FOR_TRANSACTION(TWH-1)) = 1
                                 ALLOWED_TRANSACTION_PAIR( &
                                   SELL_FOR_TRANSACTION(TWH), &
                                        BUY_FOR_TRANSACTION(TWH-1)) = 1
                              ENDIF
!
                           ELSE
                              LONG_PATH_TRANSACTION_PAIR( &
                                   SELL_FOR_TRANSACTION(TWH), &
                                        BUY_FOR_TRANSACTION(TWH-1)) = 1
                              ALLOWED_TRANSACTION_PAIR( &
                                   SELL_FOR_TRANSACTION(TWH), &
                                        BUY_FOR_TRANSACTION(TWH-1)) = 1
                           ENDIF ! BURESH
!
                        ENDIF
                        LONG_PATH_TRANSACTION(TWH) = &
                             LONG_PATH_TRANSACTION(TWH) + &
                                           LONG_PATH_TRANSACTION(TWH-1) 
!
                        IF(LONG_PATH_TRANSACTION(TWH) > 1) THEN
!                        FAILED_DIAGNOSTICS = .TRUE.
                        ENDIF
                     ENDIF ! DUTCH_AUCTION
                  ENDIF
!
!     
               ENDIF
               IF(TWH > MAX_TRANS_WITHIN_HOUR-1) THEN
                  FAILED_DIAGNOSTICS = .TRUE.
               ENDIF
!
! 07/11/03. TEST
!
!               TEMP_TRANSACTION = 
!     +                             NINT(TEMP_TRANSACTION)
!
               IF(TEMP_TRANSACTION == 0.) THEN
!               
! THINK THAT THERE ARE TOO MANY 0. MW TRANSACTION.
! NEED TO TRAP ON THE FREQUENCY.
!
                  CYCLE 
               ENDIF
!
               SELLERS_LOAD = SELLERS_LOAD + TEMP_TRANSACTION 
               BUYERS_LOAD = BUYERS_LOAD - TEMP_TRANSACTION
               IF(HOURLY_DUMP_USED > .01) THEN
                  SELLERS_LOAD = SELLERS_LOAD - TEMP_TRANSACTION 
               ENDIF
               M_HOURLY_LOADS(SELLER,HOUR_IN_DAY) = SELLERS_LOAD
               M_HOURLY_LOADS(BUYER,HOUR_IN_DAY) = BUYERS_LOAD
!
               HOURLY_MARGINAL_COST(SELLER,HOUR_IN_DAY) = SELLERS_COST
               HOURLY_MARGINAL_COST(BUYER,HOUR_IN_DAY) = BUYERS_COST
! REMOVE AS A SELLER IF IT SELLS ALL GENERATION.
               IF( .NOT. DUTCH_AUCTION_LOGIC .AND. &
                 sngl(TRANS_GROUP_SELLER_CAP - &
                             M_HOURLY_LOADS(SELLER,HOUR_IN_DAY)) < &
                                                   LOAD_INCREMENT) THEN
                  HOURLY_TRANS_GROUP_GEN_ACTIVE(SELLER) = .FALSE.
               ENDIF
! CHANGING TO -0.01 CAUSING "PARAMETER VIOLATION" ERROR. 8/24/99. GAT.
               IF(M_HOURLY_LOADS(BUYER,HOUR_IN_DAY) <= 0.0d0) THEN
                  HOURLY_TRANS_GROUP_LOAD_ACTIVE(BUYER) = .FALSE.
! 01/27/03. ELSE CONDITION ADDED TO ACCOMODATE PRELOADING CODE.
               ENDIF
!
! 7/30/99. GAT.
!
! SELLER SHOULD RECEIVE THE VALUE OF THE BUYER'S AND GLOBAL SCARCITY
!
               ! Hourly_last_price set here.
             HOURLY_LAST_PRICE(SELLER,HOUR_IN_DAY) = &
            HOURLY_MARGINAL_COST(SELLER,HOUR_IN_DAY) + &
              SCARCITY_COST(MARKET_TG,HOUR_IN_DAY)

               IF(L_M > 1 .AND. .NOT. DUTCH_AUCTION_LOGIC_2) THEN
                  HOURLY_LAST_PRICE(SELLER,HOUR_IN_DAY) = &
                         HOURLY_MARGINAL_COST(SELLER,HOUR_IN_DAY) + &
                          SCARCITY_COST(BUYER,HOUR_IN_DAY) 
               ENDIF
! 10/25/02.
               TRANSACTIONS_PER_HOUR(SELLER,BUYER) = &
                                TRANSACTIONS_PER_HOUR(SELLER,BUYER) + 1
! 12/19/02. PUT-IN.
               IF(TRANSACTIONS_PER_HOUR(SELLER,BUYER) > 30) THEN
                  ALLOWED_TRANSACTION_PAIR(SELLER,BUYER) = 0
               ENDIF
!               
               IF(RUN_HOUR_DIAG_REPORT .AND. &
                                   DAY == DIAGNOSTIC_DAY_OF_MONTH) THEN
                  IF(HOURLY_TRANS_GROUP_GEN_ACTIVE(SELLER)) THEN
                     SELLER_ACTIVE = 1.0
                  ELSE
                     SELLER_ACTIVE = 0.0
                  ENDIF
                  IF(HOURLY_TRANS_GROUP_LOAD_ACTIVE(BUYER)) THEN
                     BUYER_ACTIVE = 1.0
                  ELSE
                     BUYER_ACTIVE = 0.0
                  ENDIF
!
                  IF(BUYER_DB > 0 .AND.  BUYERS_LOAD > 100.d0) THEN
                     R8TEMP = BUYERS_LOAD-100.d0
                     UP_100MW_BUY = &
                             GET_MARGINAL_COST_AT_MARKET( &
                                R8TEMP, &
                                BUYER_DB, &
                                SCARCITY_COST(BUYER,HOUR_IN_DAY),L_M, &
                                SCARCITY_MULT(BUYER))
                  ELSE
                     UP_100MW_BUY = &
                                HOURLY_MARGINAL_COST(BUYER,HOUR_IN_DAY) 
                  ENDIF
!                  
!
                  IF(BUYER_DB > 0 .AND. &
                             BUYERS_LOAD + 100.d0 < &
                                            TRANS_GROUP_BUYER_CAP) THEN
                     R8TEMP = BUYERS_LOAD + 100.d0
                     DOWN_100MW_BUY = &
                             GET_MARGINAL_COST_AT_MARKET( &
                                R8TEMP, &
                                BUYER_DB, &
                                SCARCITY_COST(BUYER,HOUR_IN_DAY),L_M, &
                                SCARCITY_MULT(BUYER))
                  ELSE
                     DOWN_100MW_BUY = &
                                HOURLY_MARGINAL_COST(BUYER,HOUR_IN_DAY) 
                  ENDIF
!
!
                  IF(SELLERS_LOAD > 100.d0) THEN
                     R8TEMP = SELLERS_LOAD-100.d0
                     UP_100MW_SELL = &
                             GET_MARGINAL_COST_AT_MARKET( &
                                R8TEMP, &
                                SELLER_DB, &
                                SCARCITY_COST(SELLER,HOUR_IN_DAY),L_M, &
                                SCARCITY_MULT(SELLER))
                  ELSE
                     UP_100MW_SELL = &
                               HOURLY_MARGINAL_COST(SELLER,HOUR_IN_DAY)
                  ENDIF
!
!
                  IF(SELLERS_LOAD+100.d0 <  TRANS_GROUP_SELLER_CAP) THEN
                     R8TEMP = SELLERS_LOAD + 100.d0
                     DOWN_100MW_SELL = &
                             GET_MARGINAL_COST_AT_MARKET( &
                                R8TEMP, &
                                SELLER_DB, &
                                SCARCITY_COST(SELLER,HOUR_IN_DAY),L_M, &
                                SCARCITY_MULT(SELLER))
                  ELSE
                     DOWN_100MW_SELL = &
                               HOURLY_MARGINAL_COST(SELLER,HOUR_IN_DAY) 
                  ENDIF
!
                  IF(BUYER_DB > 0 .AND.  BUYERS_LOAD > 25.d0) THEN
                     R8TEMP = BUYERS_LOAD-25.d0
                     UP_25MW_BUY = &
                             GET_MARGINAL_COST_AT_MARKET( &
                                R8TEMP, &
                                BUYER_DB, &
                                SCARCITY_COST(BUYER,HOUR_IN_DAY),L_M, &
                                SCARCITY_MULT(BUYER))
                  ELSE
                     UP_25MW_BUY = &
                                HOURLY_MARGINAL_COST(BUYER,HOUR_IN_DAY) 
                  ENDIF
!                  
!
                  IF(BUYER_DB > 0 .AND. &
                             BUYERS_LOAD + 25.d0 < &
                                            TRANS_GROUP_BUYER_CAP) THEN
                     R8TEMP = BUYERS_LOAD + 25.d0
                     DOWN_25MW_BUY = &
                             GET_MARGINAL_COST_AT_MARKET( &
                                R8TEMP, &
                                BUYER_DB, &
                                SCARCITY_COST(BUYER,HOUR_IN_DAY),L_M, &
                                SCARCITY_MULT(BUYER))
                  ELSE
                     DOWN_25MW_BUY = &
                                HOURLY_MARGINAL_COST(BUYER,HOUR_IN_DAY) 
                  ENDIF
                  IF(SELLERS_LOAD > 25.d0) THEN
                     R8TEMP = SELLERS_LOAD-25.d0
                     UP_25MW_SELL = &
                             GET_MARGINAL_COST_AT_MARKET( &
                                R8TEMP, &
                                SELLER_DB, &
                                SCARCITY_COST(SELLER,HOUR_IN_DAY),L_M, &
                                SCARCITY_MULT(SELLER))
                  ELSE
                     UP_25MW_SELL = &
                               HOURLY_MARGINAL_COST(SELLER,HOUR_IN_DAY)
                  ENDIF
!
!
                  IF(SELLERS_LOAD+25.d0 <  TRANS_GROUP_SELLER_CAP) THEN
                     R8TEMP = SELLERS_LOAD+25.d0
                     DOWN_25MW_SELL = &
                             GET_MARGINAL_COST_AT_MARKET( &
                                R8TEMP, &
                                SELLER_DB, &
                                SCARCITY_COST(SELLER,HOUR_IN_DAY),L_M, &
                                SCARCITY_MULT(SELLER))
                  ELSE
                     DOWN_25MW_SELL = &
                               HOURLY_MARGINAL_COST(SELLER,HOUR_IN_DAY) 
                  ENDIF
!                  
                  HOUR_DIAG_REC = RPTREC(HOUR_DIAG_UNIT)
                  call check_unit(HOUR_DIAG_UNIT,"transobj:0042")
                  WRITE(HOUR_DIAG_UNIT,REC=HOUR_DIAG_REC) & 
                       PRT_ENDPOINT(), &
                       FLOAT(CURRENT_YEAR), &
                       CL_MONTH_NAME(R_MONTH), &
                       FLOAT(HR), &
                       FLOAT(TWH), &
                       FLOAT(BUYER), &
                       FLOAT(SELLER), &
                       BEST_MARGIN, &
                       TEMP_TRANSACTION, &
                       FLOAT(LONG_PATH_TRANSACTION(TWH)), &
                       BUYERS_COST, &
                       SELLERS_COST, &
                       TOTAL_DELIVERED_COST(SELLER,BUYER), &
                       SELLER_ACTIVE, &
                       HOURLY_LAST_PRICE(SELLER,HOUR_IN_DAY), &
                       BUYER_ACTIVE, &
                       CONVERGENCE_PATH, &
                       PATH_CAPACITY, &
                       FLOAT(SEARCHES_WITHIN_TRANSACTIONS), &
                       FLOAT(ALLOWED_TRANSACTION_PAIR(SELLER,BUYER)), &
                       sngl(BUYERS_LOAD), &
                       sngl(SELLERS_LOAD), &
                       LAST_TRANS_MC(BUYER), &
                       LAST_TRANS_MC(SELLER), &
                       FLOAT(CUM_REDUNDANT_TRANSACTION(TWH)), &
                       FLOAT(TRANSACTIONS_PER_HOUR(SELLER,BUYER)), &
                       UP_100MW_BUY, &
                       DOWN_100MW_BUY, &
                       UP_100MW_SELL, &
                       DOWN_100MW_SELL, &
                       UP_25MW_BUY, &
                       DOWN_25MW_BUY, &
                       UP_25MW_SELL, &
                       DOWN_25MW_SELL
                  HOUR_DIAG_REC = HOUR_DIAG_REC + 1
! RESET, JUST IN CASE.
                  IF(BUYER_DB > 0 .AND. &
                             BUYERS_LOAD < TRANS_GROUP_BUYER_CAP) THEN
                     DOWN_100MW_BUY = &
                             GET_MARGINAL_COST_AT_MARKET( &
                                BUYERS_LOAD, &
                                BUYER_DB, &
                                SCARCITY_COST(BUYER,HOUR_IN_DAY),L_M, &
                                SCARCITY_MULT(BUYER))
                  ENDIF
                  IF(SELLERS_LOAD <  TRANS_GROUP_SELLER_CAP) THEN
                     DOWN_100MW_SELL = &
                             GET_MARGINAL_COST_AT_MARKET( &
                                SELLERS_LOAD, &
                                SELLER_DB, &
                                SCARCITY_COST(SELLER,HOUR_IN_DAY),L_M, &
                                SCARCITY_MULT(SELLER))
                  ENDIF
               ENDIF
!
               LAST_TRANS_MC(BUYER) = &
                                HOURLY_MARGINAL_COST(BUYER,HOUR_IN_DAY)
               LAST_TRANS_MC(SELLER) = &
                               HOURLY_MARGINAL_COST(SELLER,HOUR_IN_DAY)
!
               HOURLY_LAST_PRICE(BUYER,HOUR_IN_DAY) = &
                                     TOTAL_DELIVERED_COST(SELLER,BUYER)
! 01/31/02. FOR LGE.
!
! LAST BUYER ADDED 8/26/99. GAT. FOR SELANDER.
!
               LAST_BUYER(SELLER) = BUYER
               LAST_BUYER(BUYER) = 0
!
               LAST_SELLER(BUYER) = SELLER
               LAST_SELLER(SELLER) = 0
!
!
               HOURLY_TRANSACTION(SELLER) = &
              HOURLY_TRANSACTION(SELLER) + TEMP_TRANSACTION
               HOURLY_TRANSACTION(BUYER) = &
              HOURLY_TRANSACTION(BUYER) -  TEMP_TRANSACTION
               IF(DUTCH_AUCTION_LOGIC .AND. NEW_LONG_PATH .AND. &
                    LONG_PATH_TRANSACTION_PAIR(SELLER,BUYER) /= 0) THEN
                  L_P = LONG_PATH_TRANSACTION_PAIR(SELLER,BUYER)
                  VOID_LOGICAL = &
                    RECORD_TRANSACTION_TO_TIE(SELLER,L_P, &
                                          TEMP_TRANSACTION,HOUR_IN_DAY)
                  VOID_LOGICAL = &
                    REDUCE_PATH_CAPACITY(TEMP_TRANSACTION, &
                                          SELLER,L_P, &
                                          PEAK_HOUR,R_MONTH,globecom_year)
                  VOID_LOGICAL = &
                    RECORD_TRANSACTION_TO_TIE(L_P,BUYER, &
                                          TEMP_TRANSACTION,HOUR_IN_DAY)
                  VOID_LOGICAL = &
                    REDUCE_PATH_CAPACITY(TEMP_TRANSACTION, &
                                          L_P,BUYER, &
                                          PEAK_HOUR,R_MONTH,globecom_year)
!
                  PATH_CAPACITY = &
                                MIN(HOUR_TIE_LIMIT(SELLER,L_P), &
                                    HOUR_TIE_LIMIT(L_P,BUYER))
!
                  HOURLY_SELL_MWH(SELLER,L_P) = &
                                HOURLY_SELL_MWH(SELLER,L_P) + &
                                                       TEMP_TRANSACTION
                  HOURLY_SELL_MWH(TOTAL_BUYER_INDEX,L_P) = &
                             HOURLY_SELL_MWH(TOTAL_BUYER_INDEX,L_P) + &
                                                       TEMP_TRANSACTION
                  HOURLY_SELL_MWH(SELLER,TOTAL_BUYER_INDEX) = &
                           HOURLY_SELL_MWH(SELLER,TOTAL_BUYER_INDEX) + &
                                                       TEMP_TRANSACTION
!
                  HOURLY_SELL_MWH(L_P,BUYER) = &
                                HOURLY_SELL_MWH(L_P,BUYER) + &
                                                       TEMP_TRANSACTION
                  HOURLY_SELL_MWH(TOTAL_BUYER_INDEX,BUYER) = &
                            HOURLY_SELL_MWH(TOTAL_BUYER_INDEX,BUYER) + &
                                                       TEMP_TRANSACTION
                  HOURLY_SELL_MWH(L_P,TOTAL_BUYER_INDEX) = &
                             HOURLY_SELL_MWH(L_P,TOTAL_BUYER_INDEX) + &
                                                       TEMP_TRANSACTION
!
                  IF(PATH_CAPACITY == 0.) &
                             ALLOWED_TRANSACTION_PAIR(SELLER,BUYER) = 0
!     
               ELSE
! 12/29/00.  PROPERLY CAPTURE TIME CONSTRAINTS
                  VOID_LOGICAL = &
                    REDUCE_PATH_CAPACITY( TEMP_TRANSACTION, &
                                          SELLER,BUYER, &
                                          PEAK_HOUR,R_MONTH,globecom_year)
!
                  HOURLY_SELL_MWH(SELLER,BUYER) = &
                                HOURLY_SELL_MWH(SELLER,BUYER) + &
                                                       TEMP_TRANSACTION
                  HOURLY_SELL_MWH(TOTAL_BUYER_INDEX,BUYER) = &
                            HOURLY_SELL_MWH(TOTAL_BUYER_INDEX,BUYER) + &
                                                       TEMP_TRANSACTION
                  HOURLY_SELL_MWH(SELLER,TOTAL_BUYER_INDEX) = &
                           HOURLY_SELL_MWH(SELLER,TOTAL_BUYER_INDEX) + &
                                                       TEMP_TRANSACTION
               ENDIF
!
!
               TEMP_TRANSACTION = 0.
               IF(DC_LOAD_FLOW) THEN               
                  call LPsolver(NewStructure) 
                  ! optimize the inter-nodal flows within limits
                  call LPreport ! of inter-nodal flows
               ENDIF
!
! 02/13/04. PER MARK TO CLEAR THE TRANSACTIONS
!
            ENDDO ! DO UNTIL TRANSACTIONS ARE NO LONGER ECONOMIC OR 
            ! PHYICALLY POSSIBLE
            CALL RW_PROCESS_MESSAGES()
!
! 1/3/3.
!
            TEMP_L = HOURLY_PATHS_REPORTS(HOUR_IN_DAY, &
                                         PEAK_HOUR,R_MONTH,globecom_year) 
!
            MONTHLY_MARKET_PRICES(HR) = MAX(BUYERS_COST,SELLERS_COST)
!
! ADDED FOR DUKE ENERGY. 01/24/02.
!
!            PRICE_LAST_MW = .TRUE.
!
!            LAMBDA_PRICE = .TRUE.
!
            IF(PRICE_LAST_MW) THEN
               DO I = 1, UPPER_TRANS_GROUP
                  IF(LAST_BUYER(I) == 0) THEN 
                  ! LAST TRANSACTION WAS A BUY
                     SELLER = LAST_SELLER(I)
                     BUYER = I
                     IF(MAX_HOURLY_IMPORT(BUYER) + & 
                     ! HOURLY_TRANSACTION IS NEGATIVE FOR BUYER
                                 HOURLY_TRANSACTION(BUYER) < .1d0) THEN
                        BUYER_DB = GET_DATA_BASE_FOR_TRANS( &
                                 GET_TRANS_GROUP_INDEX(BUYER),DAY_TYPE)
                        IF(TRANS_GROUP_CAP(BUYER_DB) < &
                                M_HOURLY_LOADS(BUYER,HOUR_IN_DAY)) THEN
                           HOURLY_LAST_PRICE(BUYER,HOUR_IN_DAY) = &
                                                 TRANSACT_UNSERVED_COST
                        ELSE
                           HOURLY_LAST_PRICE(BUYER,HOUR_IN_DAY) = &
                             MAX(HOURLY_LAST_PRICE(BUYER,HOUR_IN_DAY), &
                               HOURLY_MARGINAL_COST(BUYER,HOUR_IN_DAY))
                        ENDIF
                     ENDIF
                  ENDIF
               ENDDO
            ELSEIF(LAMBDA_PRICE) THEN
               DO I = 1, UPPER_TRANS_GROUP
                  IF(LAST_BUYER(I) == 0) THEN 
                  ! LAST TRANSACTION WAS A BUY
                     SELLER = LAST_SELLER(I)
                     BUYER = I
                     IF(MAX_HOURLY_IMPORT(BUYER) + & 
                     ! HOURLY_TRANSACTION IS NEGATIVE FOR BUYER
                                 HOURLY_TRANSACTION(BUYER) < .1d0) THEN
                        BUYER_DB = GET_DATA_BASE_FOR_TRANS( &
                                 GET_TRANS_GROUP_INDEX(BUYER),DAY_TYPE)
                        IF(TRANS_GROUP_CAP(BUYER_DB) < &
                                M_HOURLY_LOADS(BUYER,HOUR_IN_DAY)) THEN
                           HOURLY_LAST_PRICE(BUYER,HOUR_IN_DAY) = &
                                                 TRANSACT_UNSERVED_COST
                        ELSE
                           HOURLY_LAST_PRICE(BUYER,HOUR_IN_DAY) = &
                             MAX(HOURLY_LAST_PRICE(BUYER,HOUR_IN_DAY), &
                               HOURLY_MARGINAL_COST(BUYER,HOUR_IN_DAY))
                        ENDIF
                     ELSE ! BOUGHT LESS THAN THE IMPORT CONSTRAINT
                        HOURLY_LAST_PRICE(BUYER,HOUR_IN_DAY) = &
                             MAX(HOURLY_LAST_PRICE(BUYER,HOUR_IN_DAY), &
                               HOURLY_MARGINAL_COST(BUYER,HOUR_IN_DAY))
                     ENDIF
                  ENDIF
               ENDDO
            ENDIF
!                  
            IF(HOURLY_PRICE_RECONCILE .AND. &
                      TWH > 0 .AND. BUY_FOR_TRANSACTION(TWH) > 0) THEN
!
               PRICE_ANCHORED = 0
!
! GOAL IS TO ANCHOR THE PRICE OF EACH TRANSACTION GROUP IN AN HOUR 
!
               PRICE_ANCHORED(BUY_FOR_TRANSACTION(TWH)) = 1
               PRICE_ANCHORED(SELL_FOR_TRANSACTION(TWH)) = 1
!
! ASSUME THAT THE LAST TRANSACTIONS ARE THE CLOSEST TO MARKET CLEARING
!
               DO J = TWH-1, 1, -1
!
                  BUYER = BUY_FOR_TRANSACTION(J) 
                  SELLER = SELL_FOR_TRANSACTION(J)
!                  
! ADDED 5/10/02. FOR DUKE AUSTRALIA
!
                  IF(MAX_HOURLY_IMPORT(BUYER) + &
                                HOURLY_TRANSACTION(BUYER) < .1d0) CYCLE
!
                  IF(PRICE_ANCHORED(BUYER) > 0 .AND. &
                                     PRICE_ANCHORED(SELLER) == 0 ) THEN
!
                     IF(HOURLY_LAST_PRICE(BUYER,HOUR_IN_DAY) - &
                           PATH_WHEELING_CHARGE(SELLER,BUYER, &
                                     PEAK_HOUR,R_MONTH,globecom_year) < 0.) THEN
                        BUYER = BUYER
                     ENDIF
!
                     HOURLY_LAST_PRICE(SELLER,HOUR_IN_DAY) = &
                         MAX(TRANSACT_ABUNDANCE_COST, &
                            HOURLY_LAST_PRICE(BUYER,HOUR_IN_DAY) - &
                               PATH_WHEELING_CHARGE(SELLER,BUYER, &
                                               PEAK_HOUR,R_MONTH,globecom_year))
                     PRICE_ANCHORED(SELLER) = 1
                  ELSEIF(PRICE_ANCHORED(SELLER) > 0 .AND. &
                                       PRICE_ANCHORED(BUYER) == 0) THEN
!
                     HOURLY_LAST_PRICE(BUYER,HOUR_IN_DAY) = &
                         HOURLY_LAST_PRICE(SELLER,HOUR_IN_DAY) + &
                               PATH_WHEELING_CHARGE(SELLER,BUYER, &
                                                PEAK_HOUR,R_MONTH,globecom_year) 
                     PRICE_ANCHORED(BUYER) = 1
                  ELSE
!                    OTHER CONDITIONS
                  ENDIF
!
! CONDITIONS:
!   
! EARLY TRANSACTIONS GET SEALED-OFF
!
! 
!               
               ENDDO
            ENDIF
!            
            IF(YES_TRANSACT_DAILY_REPORTS) THEN
               DO J = 1, NUM_PRODUCTS
                  IF(APPLY_ENERGY_PRODUCT(HOUR_IN_DAY, &
                      CALENDAR_DAY_OF_WEEK,LOCAL_PRODUCT_TYPE(J))) THEN
                     PRODUCT_PRICE(0,J) = PRODUCT_PRICE(0,J) + &
                                              MONTHLY_MARKET_PRICES(HR) 
                     PRODUCT_QUANTITY(0,J) = PRODUCT_QUANTITY(0,J) + & 
                            M_HOURLY_LOAD_B4_SALES(0,HOUR_IN_DAY)
                     PRODUCT_HOURS(J) = PRODUCT_HOURS(J) + 1.
!
                     TEMP_PRODUCT_HOURS = PRODUCT_HOURS(J)
!
                     IF(PRODUCT_FIRST_PERIOD(J)) THEN
                        PRODUCT_LAST_PRICE(0,J) = &
                                              MONTHLY_MARKET_PRICES(HR)
                        PRODUCT_FIRST_PERIOD(J) = .FALSE.
                     ENDIF
!
                     PRODUCT_DAILY_RETURN(0,J,TEMP_PRODUCT_HOURS) = & 
                          LOG(MAX(1.01, &
                                MAX(MONTHLY_MARKET_PRICES(HR),.001)/ &
                                  MAX(.0001,PRODUCT_LAST_PRICE(0,J))))
!
                     PRODUCT_MEAN_RETURN(0,J) = &
                          PRODUCT_MEAN_RETURN(0,J) + &
                           PRODUCT_DAILY_RETURN(0,J,TEMP_PRODUCT_HOURS)
!
                     PRODUCT_LAST_PRICE(0,J) = MONTHLY_MARKET_PRICES(HR)
!                     
                  ENDIF
               ENDDO
            ENDIF
!
            IF(BUYERS_COST == TRANSACT_UNSERVED_COST) THEN
               MONTHLY_MARKET_PRICES(HR) = SELLERS_COST
            ENDIF
!
            IF(sngl(M_HOURLY_LOAD_B4_SALES(0,HOUR_IN_DAY)) > &
                    MONTH_COIN_PEAK(0)) THEN
               DO I = 0, UPPER_TRANS_GROUP
                  MONTH_COIN_PEAK(I) = &
                            M_HOURLY_LOAD_B4_SALES(I,HOUR_IN_DAY)
               ENDDO
            ENDIF
!
! ADDED FOR JONES. 12/11/01. 
!
            I = 0
            SELLER_DB = GET_DATA_BASE_FOR_TRANS(MARKET_TG,DAY_TYPE)
!
            TEMP_L = SAVE_DEPTH_OF_MARKET_DATA( &
                                 MARKET_TG, &
                                 HOUR_IN_DAY, &
                                 SELLER_DB, &
                          sngl(M_HOURLY_LOAD_B4_SALES(I,HOUR_IN_DAY)), &
                                 L_M, &
                                 SCARCITY_MULT(I))
!
            DO I = 1, UPPER_TRANS_GROUP ! SUMMARIZE THE HOUR

               TG = GET_TRANS_GROUP_INDEX(I)
!
               SELLER_DB = GET_DATA_BASE_FOR_TRANS(TG,DAY_TYPE)
!
! 1/21/99. GAT. TAKE SPINNING OUT OF GENERATION CALCULATIONS
!
! TESTING DEPTH OF MARKET. 2/22/01.
!
               TEMP_L = &
                       SAVE_DEPTH_OF_MARKET_DATA( & 
                                         TG, &
                                         HOUR_IN_DAY, &
                                         SELLER_DB, &
                                  sngl(M_HOURLY_LOADS(I,HOUR_IN_DAY)), &
                                                       L_M, &
                                         SCARCITY_MULT(I))
! 041008. FOR BURESH.
       TEMP_L = SAVE_TRANS_DEPTH_DATA(I, &
                                         HOUR_IN_DAY, &
                                         SELLER_DB, &
                                  sngl(M_HOURLY_LOADS(I,HOUR_IN_DAY)), &
                                                                  L_M, &
                                         SCARCITY_MULT(I))
!
               IF( REPORT_AREA_ACTIVE(I) .AND. &
                                         YES_TRANS_TO_FROM_REPORT) THEN
                        IF (I == 1) THEN
                           TRANS_TRN_REC = RPTREC(TRANS_TRN_UNIT)
                        call check_unit(TRANS_TRN_UNIT,"transobj:0044")
                           WRITE(TRANS_TRN_UNIT,REC= TRANS_TRN_REC) &
                             PRT_ENDPOINT(), &
                             FLOAT(CURRENT_YEAR), &
                             CL_MONTH_NAME(R_MONTH), &
                             FLOAT(DAY), &
                             MULTI_AREA_NAME(0), &
                             FLOAT(HOUR_IN_DAY), &
                             (HOURLY_SELL_MWH(TOTAL_BUYER_INDEX,J), &
                                  J=1,MIN(INT(31,2),TOTAL_BUYER_INDEX))
                           TRANS_TRN_REC = TRANS_TRN_REC + 1
                        ENDIF
                        TRANS_TRN_REC = RPTREC(TRANS_TRN_UNIT)
                        call check_unit(TRANS_TRN_UNIT,"transobj:0046")
                        WRITE(TRANS_TRN_UNIT,REC= TRANS_TRN_REC) &
                          PRT_ENDPOINT(), &
                          FLOAT(CURRENT_YEAR), &
                          CL_MONTH_NAME(R_MONTH), &
                          FLOAT(DAY), &
                          MULTI_AREA_NAME(I), &
                          FLOAT(HOUR_IN_DAY), &
                          (HOURLY_SELL_MWH(I,J), &
                                  J=1,MIN(INT(31,2),TOTAL_BUYER_INDEX))
                        TRANS_TRN_REC = TRANS_TRN_REC + 1
               ENDIF
               MONTH_NON_COIN_PEAK(I) = MAX(MONTH_NON_COIN_PEAK(I), &
                           sngl(M_HOURLY_LOAD_B4_SALES(I,HOUR_IN_DAY)))
!
               IF(HOURLY_SPINNING_CAPACITY(I) /= 0.0) THEN
                  M_HOURLY_LOADS(I,HOUR_IN_DAY) = &
                       M_HOURLY_LOADS(I,HOUR_IN_DAY) - &
                                  HOURLY_SPINNING_CAPACITY(I)
               ENDIF
! 070108. TOOK OUT HOURLY DUMP BEFORE TO BE CONSISTENT               
               IF(HOURLY_DUMP_BEFORE(I) /= 0.0) THEN
                  M_HOURLY_LOADS(I,HOUR_IN_DAY) = &
                       M_HOURLY_LOADS(I,HOUR_IN_DAY) + &
                                                HOURLY_DUMP_CAPACITY(I)
               ENDIF
! MOVED FROM INSIDE THE ABOVE IF STATEMENT TO SEE WHETHER IT FIXES THE 
! PROBLEM.
               IF(SELLER_DB > 0) THEN
! 091712. TO GET ENERGY COST
                  ENERGY_COST(I,HOUR_IN_DAY) = & 
                  ! THIS RESETS THE POINT IN THE COST CURVE
                       GET_MARGINAL_COST_AT_MARKET( &
                              M_HOURLY_LOADS(I,HOUR_IN_DAY), &
                              SELLER_DB, &
                              SCARCITY_COST(I,HOUR_IN_DAY),E_M, &
                              SCARCITY_MULT(I))
                  SELLERS_COST = &
                  ! THIS RESETS THE POINT IN THE COST CURVE
                       GET_MARGINAL_COST_AT_MARKET( &
                              M_HOURLY_LOADS(I,HOUR_IN_DAY), &
                              SELLER_DB, &
                              SCARCITY_COST(I,HOUR_IN_DAY),L_M, &
                              SCARCITY_MULT(I))
               ENDIF
!
!
! 6/30/99. GAT. .5 ADDED TO I_AVAILABLE TO AVOID ROUNDING ERROR
!                 IN MMTB.
!
! 070108. TOOK OUT HOURLY DUMP BEFORE
!
               I_AVAILABLE_CAPACITY = &
                          TRANS_GROUP_CAP(SELLER_DB) + &
                                    HOURLY_DUMP_CAPACITY(I) - &
                                          M_HOURLY_LOADS(I,HOUR_IN_DAY)
!
! TESTING !!! 12/28/01. GAT.
!
               IF(.NOT. HOURLY_PRICE_RECONCILE) THEN
                  IF(LAST_BUYER(I) /= 0) THEN 
                  ! LAST TRANSACTION WAS A SELL
                     HOURLY_LAST_PRICE(I,HOUR_IN_DAY) = &
                          HOURLY_MARGINAL_COST(I,HOUR_IN_DAY) + &
                          SCARCITY_COST(MARKET_TG,HOUR_IN_DAY)
! 12/27/01.
                     IF(L_M > 1 .AND. MODIFIED_DUTCH_AUCTION) THEN
                        HOURLY_LAST_PRICE(I,HOUR_IN_DAY) = MIN( &
                          TRANSACT_UNSERVED_COST, &
                          HOURLY_MARGINAL_COST(I,HOUR_IN_DAY) + &
                          SCARCITY_COST(LAST_BUYER(I),HOUR_IN_DAY), &
                          GET_LAST_TRANS_PRICE(LAST_BUYER(I),L_M))
                     ELSEIF(L_M > 1 .AND. &
                                        .NOT. DUTCH_AUCTION_LOGIC) THEN
                        HOURLY_LAST_PRICE(I,HOUR_IN_DAY) = MIN( &
                          TRANSACT_UNSERVED_COST, &
                          HOURLY_MARGINAL_COST(I,HOUR_IN_DAY) + &
                          SCARCITY_COST(LAST_BUYER(I),HOUR_IN_DAY), &
                          GET_LAST_TRANS_PRICE(LAST_BUYER(I),L_M))
                     ENDIF
                  ELSEIF(TRANS_GROUP_CAP(SELLER_DB) < &
                           M_HOURLY_LOAD_B4_SALES(I,HOUR_IN_DAY) .AND. &
                                              LAST_SELLER(I) > 0 .AND. &
                                              DUTCH_AUCTION_LOGIC) THEN 
!
! TRAP FOR EARLY BUY PRICES IN SHORT REGIONS
!     
                     HOURLY_LAST_PRICE(I,HOUR_IN_DAY) = &
                       HOURLY_LAST_PRICE(LAST_SELLER(I),HOUR_IN_DAY) + &
                               PATH_WHEELING_CHARGE(LAST_SELLER(I),I, &
                                              PEAK_HOUR,R_MONTH,globecom_year) 
                  ENDIF
               ENDIF
! 03/28/03. FOR ELENA.
               IF(PRICE_LAST_MW_AS_UNSERVED) THEN
                  IF(TRANS_GROUP_CAP(SELLER_DB) < &
                                M_HOURLY_LOADS(I,HOUR_IN_DAY)) THEN
                     HOURLY_LAST_PRICE(I,HOUR_IN_DAY) = &
                                                 TRANSACT_UNSERVED_COST
                  ENDIF
               ENDIF
!
               IF(GLOBAL_SCARCITY_BUY_N_SELL) THEN
                  HOURLY_LAST_PRICE(I,HOUR_IN_DAY) = &
                       HOURLY_LAST_PRICE(I,HOUR_IN_DAY) + & 
                                                 HOURLY_GLOBAL_SCARCITY
               ENDIF
!
! 120909.
!
               IF(YES_USE_PRICE_MINIMUMS) THEN
                  IF(HOURLY_DUMP_CAPACITY(I) > 0.1) THEN
                     HOURLY_LAST_PRICE(I,HOUR_IN_DAY) = PRICE_MINIMUM(I)
                  ENDIF
               ENDIF

               IF(USE_PRICE_CAPS) THEN
                  HOURLY_LAST_PRICE(I,HOUR_IN_DAY) = &
                       MIN(HOURLY_LAST_PRICE(I,HOUR_IN_DAY), &
                                                       CAPPED_PRICE(I))
               ENDIF
!
! 091712. FOR KATHY.
!
               
               IF(SCARCITY_COST(I,HOUR_IN_DAY) > 0.001) THEN
                  SCARCITY_COST(I,HOUR_IN_DAY) = &
                    MAX(0.0, &
                       HOURLY_LAST_PRICE(I,HOUR_IN_DAY) - &
                                            ENERGY_COST(I,HOUR_IN_DAY))
               ENDIF
!
               IF(I_AVAILABLE_CAPACITY < 0.d0) THEN
!
                  M_HOURLY_LOADS(I,HOUR_IN_DAY) = &
                               M_HOURLY_LOADS(I,HOUR_IN_DAY) + &
                                                   I_AVAILABLE_CAPACITY
                  IF(I_AVAILABLE_CAPACITY < -.01D0) THEN
!
                     WRITE(4,*) "In hour ",HR," and month ",R_MONTH
                     WRITE(4,*) "and Transaction Group ",TG
                     WRITE(4,*) "the Transaction load exceeded the"
                     WRITE(4,*) "available capacity and was reduced by"
                     WRITE(4,*) -I_AVAILABLE_CAPACITY," MWs."
!
                     M_UNSERVED_ENERGY(I) = M_UNSERVED_ENERGY(I) - &
                                                   I_AVAILABLE_CAPACITY
                     HOURLY_EUE(I,HOUR_IN_DAY) = -I_AVAILABLE_CAPACITY
!
! 7/29/99.
!
                     M_UNSERVED_ENERGY_COST(I) = &
                          M_UNSERVED_ENERGY_COST(I) + &
                                TRANSACT_UNSERVED_COST * &
                                              ABS(I_AVAILABLE_CAPACITY)
                  ENDIF
               ELSE
                  HOURLY_EUE(I,HOUR_IN_DAY) = 0.
!
               ENDIF
!
!
               DATA_BASE = GET_DATA_BASE_FOR_TRANS(TG,DAY_TYPE)
! 070108.
               IF(DATA_BASE > 0) THEN
                  HOURLY_CAPACITY(I,HOUR_IN_DAY) = &
                              TRANS_GROUP_CAP(DATA_BASE) + &
                                               HOURLY_DUMP_CAPACITY(I)
               ENDIF
!
! 060408. BIG DEFINITIONAL CHANGE FOR TEXAS WIND.
!
! 070108.
                  TIE_FLOW(I,HOUR_IN_DAY) = &
                             M_HOURLY_LOADS(I,HOUR_IN_DAY) - &
                             M_HOURLY_LOAD_B4_SALES(I,HOUR_IN_DAY) - &
                                              HOURLY_DUMP_CAPACITY(I)
!
               LAST_HOUR_SELL(I) = TIE_FLOW(I,HOUR_IN_DAY)
!
               IF(DATA_BASE > 0 .AND. &
                                TRANS_GROUP_GEN_ACTIVE(DATA_BASE)) THEN
                  M_HOURLY_PRO_COST_AFTER_SALES(I,HOUR_IN_DAY) = &
                          GET_AVERAGE_COST(HR,DATA_BASE,"After ") * & 
                                          M_HOURLY_LOADS(I,HOUR_IN_DAY)
!
                  DO J = 1, UPPER_TRANS_GROUP
                     MONTHLY_TRANSACTION_MWH(I,J) = &
                             MONTHLY_TRANSACTION_MWH(I,J) + &
                                 MAX(0.,HOURLY_SELL_MWH(I,J) - &
                                         HOURLY_SELL_MWH(J,I))
                     MONTHLY_TRANSACTION_MWH(I,TOTAL_BUYER_INDEX) = &
                        MONTHLY_TRANSACTION_MWH(I,TOTAL_BUYER_INDEX) + &
                                 MAX(0.,HOURLY_SELL_MWH(I,J) - &
                                         HOURLY_SELL_MWH(J,I))
                  ENDDO
!
! ALTERED. 1/23/98. GAT. THIS IS NEEDED IN TOTAL RETAIL ENVIRONMENT
!
                  IF(ABS(TIE_FLOW(I,HOUR_IN_DAY)) > 0.0001 .OR. &
                                         PRICE_ONLY_WHOLESALE_REV) THEN
!
! UNION DERIVATION OF TOTAL REVENUE: SPOT ENERGY SALES REVENUE
!
                     HOURLY_REVENUE = &
                       HOURLY_LAST_PRICE(I,HOUR_IN_DAY) * &
                               M_HOURLY_LOADS(I,HOUR_IN_DAY)
!                     IF(HOURLY_REVENUE 
                     AVE_SALE = PUT_MARKET_REV_AND_EXP(HR, &
                               HOURLY_REVENUE, &
                               sngl(M_HOURLY_LOADS(I,HOUR_IN_DAY)), &
                               DATA_BASE,R_MONTH,CURRENT_YEAR)
                  ELSE
                     HOURLY_REVENUE = &
                       HOURLY_LAST_PRICE(I,HOUR_IN_DAY) * &
                               M_HOURLY_LOADS(I,HOUR_IN_DAY)
                     AVE_SALE = PUT_MARKET_REV_AND_EXP(HR, &
                               HOURLY_REVENUE, &
                               sngl(M_HOURLY_LOADS(I,HOUR_IN_DAY)), &
                               DATA_BASE,R_MONTH,CURRENT_YEAR)
                  ENDIF
!
               ELSE
                  M_HOURLY_PRO_COST_AFTER_SALES(I,HOUR_IN_DAY) = &
                                                                999999.
!                  HOURLY_CAPACITY(I,HOUR_IN_DAY) = 0.
!
                  HOURLY_REVENUE = 0.
                  AVE_SALE = 0.
!                  
               ENDIF
! 6/23/98. GAT. 
!
! 9/16/98. GAT. ALTERED FOR GREATER CONSISTENCY WITH THE 
!               NEW ENERGY PRODUCTS FILE.
!
               IF(M_HOURLY_LOAD_B4_SALES(I,HOUR_IN_DAY) > 0.d0) THEN
                  TEMP_NATIVE_COST = &
                                  HOURLY_LAST_PRICE(I,HOUR_IN_DAY) * &
                                  M_HOURLY_LOAD_B4_SALES(I,HOUR_IN_DAY)
                  M_NATIVE_COST(I) = M_NATIVE_COST(I) + &
                                                    TEMP_NATIVE_COST
                  YES_PUT_AC_HOURLY_COST = &
                     PUT_AC_HOURLY_COST_AT_MARKET( & 
                                 TG, &
                                 HOURLY_LAST_PRICE(I,HOUR_IN_DAY), &
                                 TEMP_NATIVE_COST, &
                                 HR,R_MONTH)
               ENDIF
!
               IF(TIE_FLOW(I,HOUR_IN_DAY) /= 0.) THEN
                  IF(DATA_BASE > 0) THEN
                     M_HOURLY_INCREMENTAL_COST(I,HOUR_IN_DAY) = &
                       (M_HOURLY_PRO_COST_AFTER_SALES(I,HOUR_IN_DAY) - &
                           M_HOURLY_PRO_COST_B4_SALES(I,HOUR_IN_DAY))/ &
                                              TIE_FLOW(I,HOUR_IN_DAY)
                  ELSE
                     M_HOURLY_INCREMENTAL_COST(I,HOUR_IN_DAY) = &
                                                                999999.
                  ENDIF
                  IF(TIE_FLOW(I,HOUR_IN_DAY) > 0.) THEN
!                  
                     HOURLY_LAMDA(I,HOUR_IN_DAY) = &
                                    M_HOURLY_MC_B4_SALES(I,HOUR_IN_DAY)
!                     
                     M_SALES_ENERGY(I) = M_SALES_ENERGY(I) + & 
                                              TIE_FLOW(I,HOUR_IN_DAY)  
                     M_SALES_REVENUES(I) = M_SALES_REVENUES(I) + &
                                     HOURLY_LAST_PRICE(I,HOUR_IN_DAY)* &
                                      TIE_FLOW(I,HOUR_IN_DAY)  
                  ELSE
!                  
                     HOURLY_LAMDA(I,HOUR_IN_DAY) = &
                                    HOURLY_MARGINAL_COST(I,HOUR_IN_DAY)
!
                     M_PURCHASE_ENERGY(I) = M_PURCHASE_ENERGY(I) - &
                                              TIE_FLOW(I,HOUR_IN_DAY)
                     M_PURCHASE_COSTS(I) = M_PURCHASE_COSTS(I) - &
                                    HOURLY_LAST_PRICE(I,HOUR_IN_DAY) * &
                                            TIE_FLOW(I,HOUR_IN_DAY)
                     I_AVAILABLE_CAPACITY = &
                          TRANS_GROUP_CAP(SELLER_DB) + &
                              HOURLY_DUMP_BEFORE(I) - &
                                 M_HOURLY_LOAD_B4_SALES(I,HOUR_IN_DAY)
                     IF(I_AVAILABLE_CAPACITY < 0.d0) THEN
                        MARKET_COST_ABOVE_RESOURCES(I) = &
                               MARKET_COST_ABOVE_RESOURCES(I) - &
                                   I_AVAILABLE_CAPACITY * &
                                       HOURLY_LAST_PRICE(I,HOUR_IN_DAY)
                     ENDIF
                  ENDIF
               ELSE
                  M_HOURLY_INCREMENTAL_COST(I,HOUR_IN_DAY) = 0.
!                  
                  HOURLY_LAMDA(I,HOUR_IN_DAY) = &
                                    M_HOURLY_MC_B4_SALES(I,HOUR_IN_DAY)
!     
               ENDIF
!
               M_MONTHLY_PRO_COST_AFTER_SALES(I) = &
                            M_MONTHLY_PRO_COST_AFTER_SALES(I) + &
                           M_HOURLY_PRO_COST_AFTER_SALES(I,HOUR_IN_DAY)
               M_MONTHLY_PRO_COST_B4_SALES(I) = &
                               M_MONTHLY_PRO_COST_B4_SALES(I) + &
                              M_HOURLY_PRO_COST_B4_SALES(I,HOUR_IN_DAY)
               M_MONTHLY_LOAD_B4_SALES(I) = &
                           M_MONTHLY_LOAD_B4_SALES(I) + &
                                  M_HOURLY_LOAD_B4_SALES(I,HOUR_IN_DAY)
               M_MONTHLY_LOAD_AFTER_SALES(I) = &
                                M_MONTHLY_LOAD_AFTER_SALES(I) + &
                                          M_HOURLY_LOADS(I,HOUR_IN_DAY)
               MARGINAL_COST_DELTA(I,HOUR_IN_DAY) = &
                       HOURLY_LAST_PRICE(I,HOUR_IN_DAY) - & 
                               M_HOURLY_INCREMENTAL_COST(I,HOUR_IN_DAY)
! 8/1/01.
               M_SPINNING_MWH(I) = M_SPINNING_MWH(I) + &
                                            HOURLY_SPINNING_CAPACITY(I)


               IF(HOURLY_TRANS_GROUP_GEN_ACTIVE(I) .AND. &
                                                    SELLER_DB > 0) THEN
       ! HOURLY_MARGINAL_FT isn't set in this routine. It's passed to
       ! WRITE_CONTRIBUTION_REPORT and used there, then set there.
       ! Then it's used subsequently here.  Is it supposed to start
       ! at zero and be set in the function? (entry)
                  HOURLY_MARGINAL_HEATRATE = &
                       WRITE_CONTRIBUTION_REPORT( &
                                   HR, &
                                   I, &
                                   SELLER_DB, &
                                   MULTI_AREA_NAME(I), &
                                   HOURLY_LAST_PRICE(I,HOUR_IN_DAY), &
                                   I, &
                                   SELLER_DB, &
                                   HOURLY_MARGINAL_FUEL_PRICE, &
                                   HOURLY_MARGINAL_FT)

                        
               ELSE
                  HOURLY_MARGINAL_HEATRATE = 0.
                  HOURLY_MARGINAL_FUEL_PRICE = 0.
               ENDIF
               IF(YES_TRANSACT_DAILY_REPORTS) THEN
                  DO J = 1, NUM_PRODUCTS
                     IF(APPLY_ENERGY_PRODUCT(HOUR_IN_DAY, &
                                      CALENDAR_DAY_OF_WEEK, &
                                           LOCAL_PRODUCT_TYPE(J))) THEN
                        PRODUCT_PRICE(I,J) = PRODUCT_PRICE(I,J) + & 
                                       HOURLY_LAST_PRICE(I,HOUR_IN_DAY)
                       PRODUCT_SCARCITY(I,J) = PRODUCT_SCARCITY(I,J) + &
                                           SCARCITY_COST(I,HOUR_IN_DAY)
                       PRODUCT_QUANTITY(I,J) = PRODUCT_QUANTITY(I,J) + &
                                          M_HOURLY_LOADS(I,HOUR_IN_DAY)
!
                        PRODUCT_HEATRATE(I,J) = &
                                      PRODUCT_HEATRATE(I,J) + & 
                                               HOURLY_MARGINAL_HEATRATE
!
                        PRODUCT_HEATRATE(0,J) = &
                                      PRODUCT_HEATRATE(0,J) + &
                                               HOURLY_MARGINAL_HEATRATE
                        PRODUCT_MARGINAL_FUEL(I,J) = &
                                      PRODUCT_MARGINAL_FUEL(I,J) + &
                                                HOURLY_MARGINAL_FT
!
                        PRODUCT_MARGINAL_FUEL(0,J) = &
                                      PRODUCT_MARGINAL_FUEL(0,J) + &
                                                   HOURLY_MARGINAL_FT
                        PRODUCT_FUEL_PRICE(I,J) = &
                                      PRODUCT_FUEL_PRICE(I,J) + &
                                             HOURLY_MARGINAL_FUEL_PRICE
!
                        PRODUCT_FUEL_PRICE(0,J) = &
                                      PRODUCT_FUEL_PRICE(0,J) + &
                                             HOURLY_MARGINAL_FUEL_PRICE
!
!                       PRODUCT_HOURS(J) = PRODUCT_HOURS(J) + 1.
!
!                       TEMP_PRODUCT_HOURS = PRODUCT_HOURS(J)
!
                        IF(PRODUCT_FIRST_PERIOD(J)) THEN
                           PRODUCT_LAST_PRICE(I,J) = &
                                       HOURLY_LAST_PRICE(I,HOUR_IN_DAY)
                        ENDIF
!
                        PRODUCT_DAILY_RETURN(I,J,TEMP_PRODUCT_HOURS) = &
                           LOG(MAX(1.01, &
                                 MAX(HOURLY_LAST_PRICE(I,HOUR_IN_DAY), &
                                                             .001)/ &
                                  MAX(.0001,PRODUCT_LAST_PRICE(I,J))))
                        PRODUCT_MEAN_RETURN(I,J) = &
                          PRODUCT_MEAN_RETURN(I,J) + &
                                           PRODUCT_DAILY_RETURN(I,J, &
                                                    TEMP_PRODUCT_HOURS)
!
                           PRODUCT_LAST_PRICE(I,J) = &
                                       HOURLY_LAST_PRICE(I,HOUR_IN_DAY)
                     ENDIF ! APPLY THIS PRODUCT
                  ENDDO ! J PRODUCTS
!                     
               ENDIF ! APPLY ENERGY PRODUCT
            ENDDO ! UPPER_TRANS_GROUP
!
            LOCAL_MARKET_PRICE(HOUR_IN_DAY) = MONTHLY_MARKET_PRICES(HR)
!
            IF(STEP_4) THEN
               REMAIN = ABS(21 - HOUR_IN_DAY)
               I = 0
               SELLER_DB = GET_DATA_BASE_FOR_TRANS(MARKET_TG,DAY_TYPE)
! 040109.
               TEMP_L = SAVE_DEPTH_OF_MARKET_DATA( &
                                 MARKET_TG, &
                                 HOUR_IN_DAY+1_2, &
                                 SELLER_DB, &
                          sngl(M_HOURLY_LOAD_B4_SALES(I,HOUR_IN_DAY)), &
                                 L_M, &
                                 SCARCITY_MULT(I))
               TEMP_L = SAVE_DEPTH_OF_MARKET_DATA( &
                                 MARKET_TG, &
                                 HOUR_IN_DAY+2_2, &
                                 SELLER_DB, &
                          sngl(M_HOURLY_LOAD_B4_SALES(I,HOUR_IN_DAY)), &
                                 L_M, &
                                 SCARCITY_MULT(I))
               TEMP_L = SAVE_DEPTH_OF_MARKET_DATA( &
                                 MARKET_TG, &
                                 HOUR_IN_DAY+3_2, &
                                 SELLER_DB, &
                          sngl(M_HOURLY_LOAD_B4_SALES(I,HOUR_IN_DAY)), &
                                 L_M, &
                                 SCARCITY_MULT(I))
               DO I = 1, UPPER_TRANS_GROUP 
! 070209. REMOVED.
                     HOURLY_LAST_PRICE(I,HOUR_IN_DAY+1_2) = &
                                HOURLY_LAST_PRICE(I,HOUR_IN_DAY)
                     HOURLY_LAST_PRICE(I,HOUR_IN_DAY+2_2) = &
                                HOURLY_LAST_PRICE(I,HOUR_IN_DAY) 
                     HOURLY_LAST_PRICE(I,HOUR_IN_DAY+3_2) = &
                                HOURLY_LAST_PRICE(I,HOUR_IN_DAY)
! 100112. SCARCITY COST FOR KATHY.
                     SCARCITY_COST(I,HOUR_IN_DAY+1_2) = &
                                SCARCITY_COST(I,HOUR_IN_DAY)
                     SCARCITY_COST(I,HOUR_IN_DAY+2_2) = &
                                SCARCITY_COST(I,HOUR_IN_DAY)
                     SCARCITY_COST(I,HOUR_IN_DAY+3_2) = &
                                SCARCITY_COST(I,HOUR_IN_DAY)
                     TIE_FLOW(I,HOUR_IN_DAY+1_2) = &
                                TIE_FLOW(I,HOUR_IN_DAY)
                     TIE_FLOW(I,HOUR_IN_DAY+2_2) = &
                                TIE_FLOW(I,HOUR_IN_DAY)
                     TIE_FLOW(I,HOUR_IN_DAY+3_2) = &
                                TIE_FLOW(I,HOUR_IN_DAY)
                     HOURLY_DERIVATIVES(I,HOUR_IN_DAY+1_2) = &
                                HOURLY_DERIVATIVES(I,HOUR_IN_DAY)
                     HOURLY_DERIVATIVES(I,HOUR_IN_DAY+2_2) = &
                                HOURLY_DERIVATIVES(I,HOUR_IN_DAY)
                     HOURLY_DERIVATIVES(I,HOUR_IN_DAY+3_2) = &
                                HOURLY_DERIVATIVES(I,HOUR_IN_DAY)
                     M_HOURLY_LOAD_B4_SALES(I,HOUR_IN_DAY+1_2) = &
                            M_HOURLY_LOAD_B4_SALES(I,HOUR_IN_DAY)
                     M_HOURLY_LOAD_B4_SALES(I,HOUR_IN_DAY+2_2) = &
                            M_HOURLY_LOAD_B4_SALES(I,HOUR_IN_DAY)
                     M_HOURLY_LOAD_B4_SALES(I,HOUR_IN_DAY+3_2) = &
                            M_HOURLY_LOAD_B4_SALES(I,HOUR_IN_DAY)
! 040109.
                     TG = GET_TRANS_GROUP_INDEX(I)
!
                     SELLER_DB = GET_DATA_BASE_FOR_TRANS(TG,DAY_TYPE)
!
                     TEMP_L = &
                       SAVE_DEPTH_OF_MARKET_DATA( &
                                         TG, &
                                         HOUR_IN_DAY+1_2, &
                                         SELLER_DB, &
                                  sngl(M_HOURLY_LOADS(I,HOUR_IN_DAY)), &
                                                                  L_M, &
                                         SCARCITY_MULT(I))
                     TEMP_L = &
                       SAVE_TRANS_DEPTH_DATA( &
                                         I, &
                                         HOUR_IN_DAY+1_2, &
                                         SELLER_DB, &
                                  sngl(M_HOURLY_LOADS(I,HOUR_IN_DAY)), &
                                                                  L_M, &
                                         SCARCITY_MULT(I))
                     TEMP_L = &
                       SAVE_DEPTH_OF_MARKET_DATA( &
                                         TG, &
                                         HOUR_IN_DAY+2_2, &
                                         SELLER_DB, &
                                  sngl(M_HOURLY_LOADS(I,HOUR_IN_DAY)), &
                                                                  L_M, &
                                         SCARCITY_MULT(I))
                     TEMP_L = &
                       SAVE_TRANS_DEPTH_DATA( & 
                                         I, &
                                         HOUR_IN_DAY+2_2, &
                                         SELLER_DB, &
                                  sngl(M_HOURLY_LOADS(I,HOUR_IN_DAY)), &
                                                                  L_M, &
                                         SCARCITY_MULT(I))
                     TEMP_L = &
                       SAVE_DEPTH_OF_MARKET_DATA( &
                                         TG, &
                                         HOUR_IN_DAY+3_2, &
                                         SELLER_DB, &
                                  sngl(M_HOURLY_LOADS(I,HOUR_IN_DAY)), &
                                                                  L_M, &
                                         SCARCITY_MULT(I))
                     TEMP_L = &
                       SAVE_TRANS_DEPTH_DATA( &
                                         I, &
                                         HOUR_IN_DAY+3_2, &
                                         SELLER_DB, &
                                  sngl(M_HOURLY_LOADS(I,HOUR_IN_DAY)), &
                                                                  L_M, &
                                         SCARCITY_MULT(I))
               ENDDO
            ELSE
               REMAIN = MOD(FLOAT(HR),24.)
            ENDIF
            IF(REMAIN < .001) THEN ! SUMMARIZE THE DAY
               START_HOUR = Max(1,HR - 23) 
               ! 9/25/09 fix becasue of -2 value for start_hour msg)
!
               WRITE_DAY_OF_WEEK(DAY) = CALENDAR_DAY_OF_WEEK
!
               IF(YES_TRANSACT_HOURLY_REPORTS .OR. &
                                          YES_HOURLY_PRICE_REPORT) THEN
                  TRANS_MRK_REC = RPTREC(TRANS_MRK_UNIT)
                  call check_unit(TRANS_MRK_UNIT,"transobj:0047")
                  WRITE(TRANS_MRK_UNIT,REC= TRANS_MRK_REC) & 
                    PRT_ENDPOINT(), &
                    FLOAT(CURRENT_YEAR), &
                    CL_MONTH_NAME(R_MONTH), &
                    FLOAT(DAY), &
                    (MONTHLY_MARKET_PRICES(RPT_HR), &
                                       RPT_HR=START_HOUR,START_HOUR+23)
                  TRANS_MRK_REC = TRANS_MRK_REC + 1
               ENDIF
!
! 05/09/01.
! TESTING FOR TVA W/O THIS CONDITION AND MOVED FROM 
! YES_TRANSACT_HOURLY_REPORTS:
!
               CALL WRITE_DAILY_MARKET_DATA( R_MONTH, &
                                                  DAY, &
                                                 CALENDAR_DAY_OF_WEEK, &
                                                  LOCAL_MARKET_PRICE)
!
               TEMP_L =  WRITE_DAILY_THERMAL_UNIT(R_MONTH,DAY)
!
              TEMP_L = DAILY_PATHS_REPORTS(gc_end_point,CURRENT_YEAR,DAY, &
                                           CL_MONTH_NAME(R_MONTH), &
                                           STEP_4)
!
               IF(YES_TRANSACT_HOURLY_REPORTS) THEN
                  TRANS_MCB_REC = RPTREC(TRANS_MCB_UNIT)
                  call check_unit(TRANS_MCB_UNIT,"transobj:0048")
                  WRITE(TRANS_MCB_UNIT,REC= TRANS_MCB_REC) &
                       PRT_ENDPOINT(), &
                       FLOAT(CURRENT_YEAR), &
                       CL_MONTH_NAME(R_MONTH), &
                       FLOAT(DAY), &
                       MULTI_AREA_NAME(0), &
                       (M_HOURLY_MC_B4_SALES(INT(0,2),RPT_HR), &
                                                  RPT_HR=1,DAILY_HOURS)
                  TRANS_MCB_REC = TRANS_MCB_REC + 1
               ENDIF
               IF(YES_HOURLY_AFTER_HYDRO_REPORT .OR. &
                                      YES_TRANSACT_HOURLY_REPORTS) THEN
                        TRANS_LDS_REC = RPTREC(TRANS_LDS_UNIT)
                  call check_unit(TRANS_LDS_UNIT,"transobj:0049")
                  WRITE(TRANS_LDS_UNIT,REC= TRANS_LDS_REC) &
                          PRT_ENDPOINT(), &
                          FLOAT(CURRENT_YEAR), &
                          CL_MONTH_NAME(R_MONTH), &
                          FLOAT(DAY), &
                          MULTI_AREA_NAME(0), &
                          (sngl(M_HOURLY_LOAD_B4_SALES(0,RPT_HR)), &
                                                  RPT_HR=1,DAILY_HOURS)
                  TRANS_LDS_REC = TRANS_LDS_REC + 1
               ENDIF

               IF(YES_HOURLY_CAPACITY_COST_REPORT .OR. &
                                      YES_TRANSACT_HOURLY_REPORTS) THEN
                  if(.not. is_unit_in_use(int(trans_sca_unit_loc)) .or. &
                    trans_sca_unit_loc==0) then
                        er_message="transobj:0001 - cannot write " // &
         " to trans sca unit " // trim(itos(int(trans_sca_unit_loc)))
                        call end_program(er_message)
                  end if
                  TRANS_SCA_REC = RPTREC(trans_sca_unit_loc)
                  call check_unit(trans_sca_unit_loc,"transobj:0050")
                  WRITE(trans_sca_unit_loc,REC= TRANS_SCA_REC) &
                       PRT_ENDPOINT(), &
                       FLOAT(CURRENT_YEAR), &
                       CL_MONTH_NAME(R_MONTH), &
                       FLOAT(DAY), &
                       MULTI_AREA_NAME(0), &
                       (SCARCITY_COST(0,RPT_HR), &
                                                  RPT_HR=1,DAILY_HOURS)
                  TRANS_SCA_REC = TRANS_SCA_REC + 1
               ENDIF
!
               TEMP_L = WRITE_DEPTH_OF_MARKET_REPORT(DAY,R_MONTH)
               TEMP_L = INIT_DEPTH_PRICE()
!                  
               DO I = 1, UPPER_TRANS_GROUP 
                  
                  IF( REPORT_AREA_ACTIVE(I) .AND. &
                       (YES_TRANSACT_HOURLY_REPORTS .OR. &
                                         YES_HOURLY_PRICE_REPORT)) THEN
                     TG = GET_TRANS_GROUP_INDEX(I)
                     TRANS_HMP_REC = RPTREC(TRANS_HMP_UNIT)
       
                     ! Fifth write - write hourly_last_price
                     call check_unit(TRANS_HMP_UNIT,"transobj:0051")
                     WRITE(TRANS_HMP_UNIT,REC= TRANS_HMP_REC) & 
                       PRT_ENDPOINT(), &
                       FLOAT(CURRENT_YEAR), &
                       CL_MONTH_NAME(R_MONTH), &
                       FLOAT(DAY), &
                       MULTI_AREA_NAME(I), &
                       (HOURLY_LAST_PRICE(I,RPT_HR), &
                                                 RPT_HR=1,DAILY_HOURS)
                     TRANS_HMP_REC = TRANS_HMP_REC + 1
                  ENDIF
                  CREATE_HOURLY_PRICE = GET_CREATE_HOURLY_PRICE(I)
                IF(GET_CREATE_HOURLY_PRICE(I) .OR. MRX_ACTIVE) THEN
                     CALL WRITE_USER_MARKET_DATA( &
                                              R_MONTH, &
                                              DAY, &
                                              CALENDAR_DAY_OF_WEEK, &
                                              I, &
                                             GET_HOURLY_PRICE_NAME(I), &
                                              CREATE_HOURLY_PRICE)
                     CALL WRITE_USER_TIE_FLOW_DATA( &
                                              R_MONTH, &
                                              DAY, &
                                              CALENDAR_DAY_OF_WEEK, &
                                              I, &
                                             GET_HOURLY_PRICE_NAME(I), &
                                              CREATE_HOURLY_PRICE)
                  ENDIF
                  IF(REPORT_AREA_ACTIVE(I) .AND. &
                                      YES_TRANSACT_HOURLY_REPORTS) THEN
                     TRANS_PRI_REC = RPTREC(TRANS_PRI_UNIT)
                     call check_unit(trans_pri_unit,"transobj:0052")
                     WRITE(TRANS_PRI_UNIT,REC= TRANS_PRI_REC) & 
                       PRT_ENDPOINT(), &
                       FLOAT(CURRENT_YEAR), &
                       CL_MONTH_NAME(R_MONTH), &
                       FLOAT(DAY), &
                       MULTI_AREA_NAME(I), &
                       (HOURLY_MARGINAL_COST(I,RPT_HR), &
                                                  RPT_HR=1,DAILY_HOURS)
                     TRANS_PRI_REC = TRANS_PRI_REC + 1
                     TRANS_MCB_REC = RPTREC(TRANS_MCB_UNIT)
                     call check_unit(trans_mcb_unit,"transobj:0053")
                     WRITE(TRANS_MCB_UNIT,REC= TRANS_MCB_REC) &
                       PRT_ENDPOINT(), &
                       FLOAT(CURRENT_YEAR), &
                       CL_MONTH_NAME(R_MONTH), &
                       FLOAT(DAY), &
                       MULTI_AREA_NAME(I), &
                       (M_HOURLY_MC_B4_SALES(I,RPT_HR), &
                                                  RPT_HR=1,DAILY_HOURS)
                    TRANS_MCB_REC = TRANS_MCB_REC + 1 
                     TRANS_LMD_REC = RPTREC(TRANS_LMD_UNIT)
                     call check_unit(TRANS_LMD_UNIT,"transobj:0054")
                    WRITE(TRANS_LMD_UNIT,REC= TRANS_LMD_REC) &
                      PRT_ENDPOINT(), &
                      FLOAT(CURRENT_YEAR), &
                      CL_MONTH_NAME(R_MONTH), &
                      FLOAT(DAY), &
                      MULTI_AREA_NAME(I), &
                      (HOURLY_LAMDA(I,RPT_HR), &
                                                  RPT_HR=1,DAILY_HOURS)
                     TRANS_LMD_REC = TRANS_LMD_REC + 1
                     TRANS_PB4_REC = RPTREC(TRANS_PB4_UNIT)
                     call check_unit(TRANS_PB4_UNIT,"transobj:0055")
                     WRITE(TRANS_PB4_UNIT,REC= TRANS_PB4_REC) &
                       PRT_ENDPOINT(), &
                       FLOAT(CURRENT_YEAR), &
                       CL_MONTH_NAME(R_MONTH), &
                       FLOAT(DAY), &
                       MULTI_AREA_NAME(I), &
                       (M_HOURLY_PRO_COST_B4_SALES(I,RPT_HR), &
                                                  RPT_HR=1,DAILY_HOURS)
                     TRANS_PB4_REC = TRANS_PB4_REC + 1
                     TRANS_PAF_REC = RPTREC(TRANS_PAF_UNIT)
                     call check_unit(TRANS_PAF_UNIT,"transobj:0056")
                     WRITE(TRANS_PAF_UNIT,REC= TRANS_PAF_REC) & 
                       PRT_ENDPOINT(), &
                       FLOAT(CURRENT_YEAR), &
                       CL_MONTH_NAME(R_MONTH), &
                       FLOAT(DAY), &
                       MULTI_AREA_NAME(I), &
                       (M_HOURLY_PRO_COST_AFTER_SALES(I,RPT_HR), &
                                                  RPT_HR=1,DAILY_HOURS)
                     TRANS_PAF_REC = TRANS_PAF_REC + 1
                     TRANS_EUE_REC = RPTREC(TRANS_EUE_UNIT)
                     call check_unit(TRANS_EUE_UNIT,"transobj:0057")
                     WRITE(TRANS_EUE_UNIT,REC= TRANS_EUE_REC) &
                       PRT_ENDPOINT(), &
                       FLOAT(CURRENT_YEAR), &
                       CL_MONTH_NAME(R_MONTH), &
                       FLOAT(DAY), &
                       MULTI_AREA_NAME(I), &
                       (HOURLY_EUE(I,RPT_HR),RPT_HR=1,DAILY_HOURS)
                     TRANS_EUE_REC = TRANS_EUE_REC + 1
                     TRANS_INC_REC = RPTREC(TRANS_INC_UNIT)
                     call check_unit(TRANS_INC_UNIT,"transobj:0058")
                     WRITE(TRANS_INC_UNIT,REC= TRANS_INC_REC) &
                       PRT_ENDPOINT(), &
                       FLOAT(CURRENT_YEAR), &
                       CL_MONTH_NAME(R_MONTH), &
                       FLOAT(DAY), &
                       MULTI_AREA_NAME(I), &
                       (M_HOURLY_INCREMENTAL_COST(I,RPT_HR), &
                                                  RPT_HR=1,DAILY_HOURS)
                     TRANS_INC_REC = TRANS_INC_REC + 1
                     TRANS_DEL_REC = RPTREC(TRANS_DEL_UNIT)
                     call check_unit(TRANS_DEL_UNIT,"transobj:0059")
                     WRITE(TRANS_DEL_UNIT,REC= TRANS_DEL_REC) &
                       PRT_ENDPOINT(), &
                       FLOAT(CURRENT_YEAR), &
                       CL_MONTH_NAME(R_MONTH), &
                       FLOAT(DAY), &
                       MULTI_AREA_NAME(I), &
                       (MARGINAL_COST_DELTA(I,RPT_HR), &
                                                  RPT_HR=1,DAILY_HOURS)
                     TRANS_DEL_REC = TRANS_DEL_REC + 1
                  ENDIF ! YES_TRANSACT_HOURLY_REPORTS
!     

                  ! yes_transact_hourly_reports=.true./
                  IF(REPORT_AREA_ACTIVE(I) .AND. &
                          (YES_HOURLY_AFTER_TRANS_REPORT .OR. &
                                     YES_TRANSACT_HOURLY_REPORTS)) THEN
                     TRANS_LAS_REC = RPTREC(TRANS_LAS_UNIT)
               if(.not. is_unit_in_use(int(trans_sca_unit_loc)) .or. &
                    trans_sca_unit_loc==0) then
                    er_message="transobj:0002 - cannot write " // &
                    "to unit " // trim(itos(int(trans_las_unit)))
                    call end_program(er_message)
                    
               end if
                     this_unit= TRANS_LAS_UNIT
                     call check_unit(this_unit,"transobj:0060")
                     WRITE(TRANS_LAS_UNIT,REC= TRANS_LAS_REC) &
                          PRT_ENDPOINT(), &
                          FLOAT(CURRENT_YEAR), &
                          CL_MONTH_NAME(R_MONTH), &
                          FLOAT(DAY), &
                          MULTI_AREA_NAME(I), &
                          (sngl(M_HOURLY_LOADS(I,RPT_HR)), &
                                                  RPT_HR=1,DAILY_HOURS)
                     TRANS_LAS_REC = TRANS_LAS_REC + 1
                  
                  ENDIF
                  IF(REPORT_AREA_ACTIVE(I) .AND. &
                    (YES_HOURLY_AFTER_HYDRO_REPORT .OR. &
                                     YES_TRANSACT_HOURLY_REPORTS)) THEN
                        TRANS_LDS_REC = RPTREC(TRANS_LDS_UNIT)
                        call check_unit(TRANS_LDS_UNIT,"transobj:0061")
                        WRITE(TRANS_LDS_UNIT,REC= TRANS_LDS_REC) & 
                          PRT_ENDPOINT(), &
                          FLOAT(CURRENT_YEAR), &
                          CL_MONTH_NAME(R_MONTH), &
                          FLOAT(DAY), &
                          MULTI_AREA_NAME(I), &
                          (sngl(M_HOURLY_LOAD_B4_SALES(I,RPT_HR)), &
                                                  RPT_HR=1,DAILY_HOURS)
                        TRANS_LDS_REC = TRANS_LDS_REC + 1
                  ENDIF
                  IF(REPORT_AREA_ACTIVE(I) .AND. &
                       (YES_HOURLY_DERIVATIVES_REPORT .OR. &
                                     YES_TRANSACT_HOURLY_REPORTS)) THEN
                     TRANS_DER_REC = RPTREC(TRANS_DER_UNIT)
                     call check_unit(TRANS_DER_UNIT,"transobj:0062")
                     WRITE(TRANS_DER_UNIT,REC= TRANS_DER_REC) &
                       PRT_ENDPOINT(), &
                       FLOAT(CURRENT_YEAR), &
                       CL_MONTH_NAME(R_MONTH), &
                       FLOAT(DAY), &
                       MULTI_AREA_NAME(I), &
                       (HOURLY_DERIVATIVES(I,RPT_HR), &
                                                  RPT_HR=1,DAILY_HOURS)
                     TRANS_DER_REC = TRANS_DER_REC + 1
                  ENDIF
                  IF(REPORT_AREA_ACTIVE(I) .AND. &
                    (YES_HOURLY_CAPACITY_REPORT .OR. &
                                     YES_TRANSACT_HOURLY_REPORTS)) THEN
                     TRANS_CAP_REC = RPTREC(TRANS_CAP_UNIT)

                     WRITE(TRANS_CAP_UNIT,REC= TRANS_CAP_REC) & 
                       PRT_ENDPOINT(), &
                       FLOAT(CURRENT_YEAR), &
                       CL_MONTH_NAME(R_MONTH), &
                       FLOAT(DAY), &
                       MULTI_AREA_NAME(I), &
                       (HOURLY_CAPACITY(I,RPT_HR), &
                                                  RPT_HR=1,DAILY_HOURS)
                     TRANS_CAP_REC = TRANS_CAP_REC + 1
                  ENDIF
 ! 10/22/03
                  IF(REPORT_AREA_ACTIVE(I) .AND. &
                    (HOURLY_TRANSACTION_REPORT .OR. &
                                     YES_TRANSACT_HOURLY_REPORTS)) THEN
                     TRANS_TIE_REC = RPTREC(TRANS_TIE_UNIT)

                     WRITE(TRANS_TIE_UNIT,REC= TRANS_TIE_REC) &
                       PRT_ENDPOINT(), &
                       FLOAT(CURRENT_YEAR), &
                       CL_MONTH_NAME(R_MONTH), &
                       FLOAT(DAY), &
                       MULTI_AREA_NAME(I), &
                       (TIE_FLOW(I,RPT_HR),RPT_HR=1,DAILY_HOURS)
                     TRANS_TIE_REC = TRANS_TIE_REC + 1
                  ENDIF
                 IF(REPORT_AREA_ACTIVE(I) .AND. &
                       (YES_HOURLY_CAPACITY_COST_REPORT .OR. &
                                     YES_TRANSACT_HOURLY_REPORTS)) THEN
                     TRANS_SCA_REC = RPTREC(trans_sca_unit_loc)

                     WRITE(trans_sca_unit_loc,REC= TRANS_SCA_REC) &
                       PRT_ENDPOINT(), &
                       FLOAT(CURRENT_YEAR), &
                       CL_MONTH_NAME(R_MONTH), &
                       FLOAT(DAY), &
                       MULTI_AREA_NAME(I), &
                       (SCARCITY_COST(I,RPT_HR), &
                                                  RPT_HR=1,DAILY_HOURS)
                     TRANS_SCA_REC = TRANS_SCA_REC + 1
                  ENDIF
!                  
                 VOID_LOGICAL = WRITE_DAILY_TIE_REVENUES(CURRENT_YEAR, &
                                R_MONTH,I,DAY, &
                                MULTI_AREA_NAME(I))
               ENDDO ! TRANSACTION GROUPS
!
               HOUR_IN_DAY = 1 ! RESET FOR THE NEXT DAY
!               
               VOID_LOGICAL = INIT_DAILY_TIE_REVENUES()
!               
!     
               HOURLY_LAST_PRICE = TRANSACT_UNSERVED_COST
               HOURLY_CAPACITY = 0.
!
!
            ELSE
               HOUR_IN_DAY = HOUR_IN_DAY + STEP_4_ADDER
            ENDIF
         ENDDO ! HOUR
         CALL CLS(16,9,33)
!
!
! 6/22/98.
!
         TOTAL_LOAD_BEFORE_HYDRO = 0.
         TOTAL_HYDRO_GENERATION = 0.
         TOTAL_HYDRO_CAPACITY = 0.
         TOTAL_HYDRO_ROR = 0.
         TOTAL_EFFECTIVE_CAPACITY = 0.
         TOTAL_NATIVE_COST = 0.
         TOTAL_SPINNING_MWH = 0.
         TOTAL_NON_COIN_PEAK = 0.
         TOTAL_NON_COIN_BASE = 0.
         TOTAL_UNSERVED = 0.
         TOTAL_ABOVE_RESOURCES = 0.
         TOTAL_UNSERVED_COST = 0.
         TOTAL_COST_ABOVE_RESOURCES = 0.
!         
         PURCHASE_ENERGY(R_MONTH) = 0.
         PURCHASE_COSTS(R_MONTH) = 0.
         SALES_ENERGY(R_MONTH) = 0.
         SALES_REVENUE(R_MONTH) = 0.
         MONTHLY_LOAD_B4_SALES(R_MONTH) = 0.
         MONTHLY_LOAD_AFTER_SALES(R_MONTH) = 0.
         MONTHLY_PRO_COST_B4_SALES(R_MONTH) = 0.
         MONTHLY_PRO_COST_AFTER_SALES(R_MONTH) = 0.
!         
         DO I = 1, UPPER_TRANS_GROUP

            TG = GET_TRANS_GROUP_INDEX(I)
            
            DAY_TYPE = 1 ! REPORTING IS BASED ON FIRST DAY TYPE DEFINED.
            DATA_BASE = GET_DATA_BASE_FOR_TRANS(TG,DAY_TYPE)
! 3/1/00. INCLUDING HYDRO CONTRIBUTION SEPARATELY
            MONTHLY_EFFECTIVE_CAPACITY = TRANS_GROUP_CAP(DATA_BASE)

            ANNUAL_EFFECTIVE_CAPACITY(I) = &
              ANNUAL_EFFECTIVE_CAPACITY(I) + MONTHLY_EFFECTIVE_CAPACITY
!            
            IF(M_PURCHASE_ENERGY(I) > 0.) THEN
               AVE_BUY = M_PURCHASE_COSTS(I) / M_PURCHASE_ENERGY(I)
            ELSE
               AVE_BUY = 0.
            ENDIF
            IF(M_SALES_ENERGY(I) > 0.) THEN
               AVE_SELL = M_SALES_REVENUES(I) / M_SALES_ENERGY(I)
            ELSE
               AVE_SELL = 0.
            ENDIF
            IF(M_MONTHLY_LOAD_B4_SALES(I) > 0.) THEN
               AVE_B4 = M_MONTHLY_PRO_COST_B4_SALES(I) / &
                                             M_MONTHLY_LOAD_B4_SALES(I)
            ELSE
               AVE_B4 = 0.
            ENDIF
            IF(M_MONTHLY_LOAD_AFTER_SALES(I) > 0.) THEN
               AVE_AFTER = M_MONTHLY_PRO_COST_AFTER_SALES(I) / &
                                          M_MONTHLY_LOAD_AFTER_SALES(I)
            ELSE
               AVE_AFTER = 0.
            ENDIF
            IF( ABS(M_MONTHLY_LOAD_AFTER_SALES(I) - &
                             M_MONTHLY_LOAD_B4_SALES(I)) > 0.) THEN
               MONTHLY_INCREMENTAL_COST = &
                    (M_MONTHLY_PRO_COST_AFTER_SALES(I) - &
                                   M_MONTHLY_PRO_COST_B4_SALES(I))/ &
                    (M_MONTHLY_LOAD_AFTER_SALES(I) -  &
                                        M_MONTHLY_LOAD_B4_SALES(I))
            ELSE
               MONTHLY_INCREMENTAL_COST = 0.
            ENDIF
!
!
            IF(USE_TF_FILE) THEN
!
               WH_MTH_ENERGY = GET_WH_MONTH_ENERGY(R_MONTH,I)
!
               TEMP_TL_MWH = GET_MONTHLY_TL_MWH(I) + WH_MTH_ENERGY
               TEMP_TL_HYDRO_MWH = GET_MONTHLY_TL_HYDRO_MWH(I) + &
                              WH_MTH_ENERGY + &
                                 trans_ror_cap(I)*R_HOURS_IN_MONTH
!               
               TEMP_TL_HYDRO_MW = GET_MONTHLY_TL_HYDRO_MW(I) + &
                                       GET_WH_MONTH_CAPACITY(R_MONTH,I)
               TEMP_TL_HYDRO_ROR = trans_ror_cap(I)
!               
               TEMP_TL_PEAK = MAX(0.,GET_TRANS_PEAK_AFTER_EL(I)- &
                                                 trans_ror_cap(I))
               TEMP_TL_BASE = MAX(0.,GET_TRANS_BASE_AFTER_EL(I)- &
                                                 trans_ror_cap(I))
!               
            ELSE
               TEMP_TL_MWH = 0.
               TEMP_TL_HYDRO_MWH = 0.
               TEMP_TL_HYDRO_MW = 0.
               TEMP_TL_HYDRO_ROR = 0.
!               
               TEMP_TL_PEAK = 0.
               TEMP_TL_BASE = 0.
!
            ENDIF
!
! HOURLY CALC. 8/1/01.
!
            M_SPINNING_MWH(I) = M_SPINNING_MWH(I) + &
                                            HOURLY_SPINNING_CAPACITY(I)
!
!               
!
            TBVars(R_MONTH,I,23)= M_PURCHASE_ENERGY(I)
            TBVars(R_MONTH,I,24)= M_SALES_ENERGY(I)
            IF(YES_TRANSACT_MONTHLY_REPORTS) THEN
               TRANS_ANN_REC = RPTREC(TRANS_ANN_UNIT)
               ! msgtrmon.rpt (Monthly Summary)

               WRITE(TRANS_ANN_UNIT,REC= TRANS_ANN_REC) &
                    PRT_ENDPOINT(), &
                    FLOAT(CURRENT_YEAR), &
                    CL_MONTH_NAME(R_MONTH), &
                    MULTI_AREA_NAME(I), &
                    M_PURCHASE_ENERGY(I), &
                    M_PURCHASE_COSTS(I)/1000000., &
                    AVE_BUY, &
                    M_SALES_ENERGY(I), &
                    M_SALES_REVENUES(I)/1000000., &
                    AVE_SELL, &
                    M_MONTHLY_LOAD_B4_SALES(I), &
                    M_MONTHLY_PRO_COST_B4_SALES(I)/1000000., &
                    AVE_B4, &
                    M_MONTHLY_LOAD_AFTER_SALES(I), &
                    M_MONTHLY_PRO_COST_AFTER_SALES(I)/1000000., &
                    AVE_AFTER, &
                    MONTHLY_INCREMENTAL_COST, &
                    TEMP_TL_MWH, &
                    TEMP_TL_HYDRO_MWH, &
                    MONTHLY_EFFECTIVE_CAPACITY, &
                    MONTH_NON_COIN_PEAK(I), &
                    TEMP_TL_BASE, &
                    M_NATIVE_COST(I)/1000000., &
                    M_SPINNING_MWH(I), &
                    M_UNSERVED_ENERGY(I), &
                    M_UNSERVED_ENERGY_COST(I)/1000000., &
                    MARKET_COST_ABOVE_RESOURCES(I)/1000000., &
                    MONTH_COIN_PEAK(I), &
                    TEMP_TL_HYDRO_MW, &
                    TEMP_TL_HYDRO_ROR, &
                    M_ABOVE_RESOURCES(I)
               TRANS_ANN_REC = TRANS_ANN_REC + 1
!
!     
               TRANS_EXC_REC = RPTREC(TRANS_EXC_UNIT)

               WRITE(TRANS_EXC_UNIT,REC= TRANS_EXC_REC) &
                    PRT_ENDPOINT(), &
                    FLOAT(CURRENT_YEAR), &
                    CL_MONTH_NAME(R_MONTH), &
                    MULTI_AREA_NAME(I), &
                    (MONTHLY_TRANSACTION_MWH(I,J), &
                                  J=1,MIN(INT(31,2),TOTAL_BUYER_INDEX))
               TRANS_EXC_REC = TRANS_EXC_REC + 1
               DO J = 1, TOTAL_BUYER_INDEX
                  ANNUAL_TRANSACTION_MWH(I,J) =  &
                          ANNUAL_TRANSACTION_MWH(I,J) + &
                                           MONTHLY_TRANSACTION_MWH(I,J)
               ENDDO
            ENDIF
!
            M_ANNUAL_PURCHASE_ENERGY(I) = &
                                  M_ANNUAL_PURCHASE_ENERGY(I) + &
                                                   M_PURCHASE_ENERGY(I)
            M_ANNUAL_PURCHASE_COSTS(I) = M_ANNUAL_PURCHASE_COSTS(I) + &
                                                    M_PURCHASE_COSTS(I)
            M_ANNUAL_SALES_ENERGY(I) = M_ANNUAL_SALES_ENERGY(I) + &
                                                      M_SALES_ENERGY(I)
            M_ANNUAL_SALES_REVENUES(I) = M_ANNUAL_SALES_REVENUES(I) + &
                                                    M_SALES_REVENUES(I)
            M_ANNUAL_NATIVE_COST(I) = M_ANNUAL_NATIVE_COST(I) + &
                                                   M_NATIVE_COST(I)
            M_ANNUAL_LOAD_B4_SALES(I) = M_ANNUAL_LOAD_B4_SALES(I) + & 
                                             M_MONTHLY_LOAD_B4_SALES(I)
            M_ANNUAL_LOAD_AFTER_SALES(I) = &
                             M_ANNUAL_LOAD_AFTER_SALES(I) + &
                                          M_MONTHLY_LOAD_AFTER_SALES(I)
            M_ANNUAL_PRO_COST_B4_SALES(I) = &
                             M_ANNUAL_PRO_COST_B4_SALES(I) + &
                                         M_MONTHLY_PRO_COST_B4_SALES(I)
            M_ANNUAL_PRO_COST_AFTER_SALES(I) = &
                                M_ANNUAL_PRO_COST_AFTER_SALES(I) + & 
                                      M_MONTHLY_PRO_COST_AFTER_SALES(I)
!     
            ANNUAL_TL_MWH(I) = ANNUAL_TL_MWH(I) + TEMP_TL_MWH
!            
            ANNUAL_TL_PEAK(I) = &
                         MAX(ANNUAL_TL_PEAK(I),MONTH_NON_COIN_PEAK(I))
            ANNUAL_TL_BASE(I) = MIN(ANNUAL_TL_BASE(I),TEMP_TL_BASE)
!            
            ANNUAL_TL_HYDRO_MWH(I) = ANNUAL_TL_HYDRO_MWH(I) + &
                                                      TEMP_TL_HYDRO_MWH
            ANNUAL_TL_HYDRO_MW(I) = ANNUAL_TL_HYDRO_MW(I) + &
                                                       TEMP_TL_HYDRO_MW
            ANNUAL_TL_HYDRO_ROR(I) = ANNUAL_TL_HYDRO_ROR(I) + &
                                                      TEMP_TL_HYDRO_ROR
!
            ANNUAL_SPINNING_MWH(I) = ANNUAL_SPINNING_MWH(I) + &
                                                      M_SPINNING_MWH(I)
!     
            ANNUAL_UNSERVED_ENERGY(I,R_MONTH) = &
                          ANNUAL_UNSERVED_ENERGY(I,R_MONTH) + &
                                                   M_UNSERVED_ENERGY(I)
            ANNUAL_UNSERVED_ENERGY(I,0) = &
                          ANNUAL_UNSERVED_ENERGY(I,0) + &
                                                   M_UNSERVED_ENERGY(I)
            ANNUAL_ABOVE_RESOURCES(I,R_MONTH) = &
                          ANNUAL_ABOVE_RESOURCES(I,R_MONTH) + &
                                                   M_ABOVE_RESOURCES(I)
            ANNUAL_ABOVE_RESOURCES(I,0) = &
                          ANNUAL_ABOVE_RESOURCES(I,0) + &
                                                   M_ABOVE_RESOURCES(I)
            ANNUAL_UNSERVED_ENERGY_COST(I) = &
                    ANNUAL_UNSERVED_ENERGY_COST(I) + &
                                              M_UNSERVED_ENERGY_COST(I)
            ANNUAL_COST_ABOVE_RESOURCES(I,0) = &
                    ANNUAL_COST_ABOVE_RESOURCES(I,0) + &
                                         MARKET_COST_ABOVE_RESOURCES(I)
            ANNUAL_COST_ABOVE_RESOURCES(I,R_MONTH) = &
                    ANNUAL_COST_ABOVE_RESOURCES(I,R_MONTH) + &
                                         MARKET_COST_ABOVE_RESOURCES(I)
!     
!
!
            PURCHASE_ENERGY(R_MONTH) = PURCHASE_ENERGY(R_MONTH) + &
                                                   M_PURCHASE_ENERGY(I) 
            PURCHASE_COSTS(R_MONTH) = PURCHASE_COSTS(R_MONTH) + &
                                                    M_PURCHASE_COSTS(I) 
            SALES_ENERGY(R_MONTH) = SALES_ENERGY(R_MONTH) + &
                                                      M_SALES_ENERGY(I) 
            SALES_REVENUE(R_MONTH) = SALES_REVENUE(R_MONTH) + &
                                                    M_SALES_REVENUES(I) 
!            
            MONTHLY_LOAD_B4_SALES(R_MONTH) = &
                          MONTHLY_LOAD_B4_SALES(R_MONTH) + &
                                             M_MONTHLY_LOAD_B4_SALES(I)
!
            MONTHLY_LOAD_AFTER_SALES(R_MONTH) = &
                          MONTHLY_LOAD_AFTER_SALES(R_MONTH) + &
                                          M_MONTHLY_LOAD_AFTER_SALES(I)
!
            MONTHLY_PRO_COST_B4_SALES(R_MONTH) = &
                       MONTHLY_PRO_COST_B4_SALES(R_MONTH) + &
                                         M_MONTHLY_PRO_COST_B4_SALES(I)
            MONTHLY_PRO_COST_AFTER_SALES(R_MONTH) = &
                       MONTHLY_PRO_COST_AFTER_SALES(R_MONTH) + &
                                      M_MONTHLY_PRO_COST_AFTER_SALES(I)
            TOTAL_LOAD_BEFORE_HYDRO = TOTAL_LOAD_BEFORE_HYDRO  + &
                                                            TEMP_TL_MWH
            TOTAL_HYDRO_GENERATION = TOTAL_HYDRO_GENERATION + &
                                                      TEMP_TL_HYDRO_MWH
            TOTAL_HYDRO_CAPACITY = TOTAL_HYDRO_CAPACITY + &
                                                       TEMP_TL_HYDRO_MW
            TOTAL_HYDRO_ROR = TOTAL_HYDRO_ROR + TEMP_TL_HYDRO_ROR
            TOTAL_EFFECTIVE_CAPACITY = TOTAL_EFFECTIVE_CAPACITY + &
                                             MONTHLY_EFFECTIVE_CAPACITY
            TOTAL_SPINNING_MWH = TOTAL_SPINNING_MWH + M_SPINNING_MWH(I)
            TOTAL_NATIVE_COST = TOTAL_NATIVE_COST + &
                                                   M_NATIVE_COST(I)
            MONTH_NON_COIN_PEAK(0) = MONTH_NON_COIN_PEAK(0) + &
                                                 MONTH_NON_COIN_PEAK(I)
            TOTAL_NON_COIN_BASE = TOTAL_NON_COIN_BASE + TEMP_TL_BASE
            TOTAL_UNSERVED = TOTAL_UNSERVED + M_UNSERVED_ENERGY(I)
            TOTAL_ABOVE_RESOURCES = TOTAL_ABOVE_RESOURCES + &
                                                   M_ABOVE_RESOURCES(I)
            TOTAL_UNSERVED_COST = TOTAL_UNSERVED_COST + &
                                              M_UNSERVED_ENERGY_COST(I)
            TOTAL_COST_ABOVE_RESOURCES = TOTAL_COST_ABOVE_RESOURCES + &
                                         MARKET_COST_ABOVE_RESOURCES(I)
!
         ENDDO ! 1, UPPER_TRANS_GROUP 
!
         IF(MONTH_COIN_PEAK(0) > ANNUAL_COIN_PEAK(0)) THEN
            DO I = 0, UPPER_TRANS_GROUP
               ANNUAL_COIN_PEAK(I) = MONTH_COIN_PEAK(I)
            ENDDO
         ENDIF
!
         IF(YES_TRANSACT_MONTHLY_REPORTS) THEN
            IF(PURCHASE_ENERGY(R_MONTH) > 0.) THEN
               AVE_BUY = PURCHASE_COSTS(R_MONTH) / &
                                    PURCHASE_ENERGY(R_MONTH)
            ELSE
               AVE_BUY = 0.
            ENDIF
            IF(SALES_ENERGY(R_MONTH) > 0.) THEN
               AVE_SELL = SALES_REVENUE(R_MONTH) / &
                                    SALES_ENERGY(R_MONTH)
            ELSE
               AVE_SELL = 0.
            ENDIF
            IF(MONTHLY_LOAD_B4_SALES(R_MONTH) > 0.) THEN
               AVE_B4 = MONTHLY_PRO_COST_B4_SALES(R_MONTH) / &
                                    MONTHLY_LOAD_B4_SALES(R_MONTH)
            ELSE
               AVE_B4 = 0.
            ENDIF
            IF(MONTHLY_LOAD_AFTER_SALES(R_MONTH) > 0.) THEN
               AVE_AFTER = MONTHLY_PRO_COST_AFTER_SALES(R_MONTH) / &
                                    MONTHLY_LOAD_AFTER_SALES(R_MONTH)
            ELSE
               AVE_AFTER = 0.
            ENDIF
!               
            TBVars(R_MONTH,0,23)= PURCHASE_ENERGY(I)
            TBVars(R_MONTH,0,24)= SALES_ENERGY(I)
            TRANS_ANN_REC = RPTREC(TRANS_ANN_UNIT)
               ! msgtrmon.rpt (Monthly Summary)

            WRITE(TRANS_ANN_UNIT,REC= TRANS_ANN_REC) &
                    PRT_ENDPOINT(), &
                    FLOAT(CURRENT_YEAR), &
                    CL_MONTH_NAME(R_MONTH), &
                    MULTI_AREA_NAME(0), &
                    PURCHASE_ENERGY(R_MONTH), &
                    PURCHASE_COSTS(R_MONTH)/1000000., &
                    AVE_BUY, &
                    SALES_ENERGY(R_MONTH), &
                    SALES_REVENUE(R_MONTH)/1000000., &
                    AVE_SELL, &
                    MONTHLY_LOAD_B4_SALES(R_MONTH), &
                    MONTHLY_PRO_COST_B4_SALES(R_MONTH)/1000000., &
                    AVE_B4, &
                    MONTHLY_LOAD_AFTER_SALES(R_MONTH), &
                    MONTHLY_PRO_COST_AFTER_SALES(R_MONTH)/1000000., &
                    AVE_AFTER, &
                    MONTHLY_INCREMENTAL_COST, &
                    TOTAL_LOAD_BEFORE_HYDRO, &
                    TOTAL_HYDRO_GENERATION, &
                    TOTAL_EFFECTIVE_CAPACITY, &
                    MONTH_NON_COIN_PEAK(0), &
                    TOTAL_NON_COIN_BASE, &
                    TOTAL_NATIVE_COST/1000000., &
                    TOTAL_SPINNING_MWH, &
                    TOTAL_UNSERVED, &
                    TOTAL_UNSERVED_COST/1000000., &
                    TOTAL_COST_ABOVE_RESOURCES/1000000., &
                    MONTH_COIN_PEAK(0), &
                    TOTAL_HYDRO_CAPACITY, &
                    TOTAL_HYDRO_ROR, &
                    TOTAL_ABOVE_RESOURCES
            TRANS_ANN_REC = TRANS_ANN_REC + 1
!
         ENDIF
         IF(YES_TRANSACT_MONTHLY_REPORTS) THEN
!
               DAY_WEEK_REC = RPTREC(DAY_WEEK_UNIT)

               WRITE(DAY_WEEK_UNIT,REC=DAY_WEEK_REC) &
                    PRT_ENDPOINT(), &
                    FLOAT(CURRENT_YEAR), &
                    CL_MONTH_NAME(R_MONTH), &
                    WRITE_DAY_OF_WEEK
               DAY_WEEK_REC = DAY_WEEK_REC + 1
         ENDIF
!
         IF(YES_TRANSACT_DAILY_REPORTS) THEN
!
!
            DO J = 1, NUM_PRODUCTS
!
!
               TEMP_PRODUCT_HOURS = PRODUCT_HOURS(J)
!               
               DO HR = 1, TEMP_PRODUCT_HOURS
!
!
                  IF(PRODUCT_HOURS(J) <= 1.) CYCLE
!
                  DEVIATIONS_FROM_MEAN = &
                          PRODUCT_DAILY_RETURN(0,J,HR) - &
                                               PRODUCT_MEAN_RETURN(0,J)
                  SUM_SQUARED_DEVIATIONS(0,J)  = &
                       SUM_SQUARED_DEVIATIONS(0,J) + & 
                              DEVIATIONS_FROM_MEAN*DEVIATIONS_FROM_MEAN
!                     
                  DO I = 1, UPPER_TRANS_GROUP
                     DEVIATIONS_FROM_MEAN = &
                          PRODUCT_DAILY_RETURN(I,J,HR) - &
                                               PRODUCT_MEAN_RETURN(I,J)
                     SUM_SQUARED_DEVIATIONS(I,J) = &
                       SUM_SQUARED_DEVIATIONS(I,J) + &
                              DEVIATIONS_FROM_MEAN*DEVIATIONS_FROM_MEAN
                  ENDDO
               ENDDO
            ENDDO
!     
            DO I = 0, UPPER_TRANS_GROUP 
!         
               DO J = 1, NUM_PRODUCTS
                  IF(I == 0) THEN
                     IF(PRODUCT_HOURS (J) > 1.) THEN
!
                        ANNUAL_PRODUCT_PRICE(0,J) = &
                                ANNUAL_PRODUCT_PRICE(0,J) + &
                                                     PRODUCT_PRICE(0,J)
                        ANNUAL_PRODUCT_SCARCITY(0,J) = &
                                ANNUAL_PRODUCT_SCARCITY(0,J) + &
                                                  PRODUCT_SCARCITY(0,J)
                        ANNUAL_PRODUCT_QUANTITY(0,J) = &
                                ANNUAL_PRODUCT_QUANTITY(0,J) + &
                                                  PRODUCT_QUANTITY(0,J)
                        ANNUAL_PRODUCT_HEATRATE(0,J) = &
                                ANNUAL_PRODUCT_HEATRATE(0,J) + &
                                                  PRODUCT_HEATRATE(0,J)
                        ANNUAL_PRODUCT_MARGINAL_FUEL(0,J) = &
                                ANNUAL_PRODUCT_MARGINAL_FUEL(0,J) + &
                                             PRODUCT_MARGINAL_FUEL(0,J)
                        ANNUAL_PRODUCT_FUEL_PRICE(0,J) = &
                                ANNUAL_PRODUCT_FUEL_PRICE(0,J) + &
                                                PRODUCT_FUEL_PRICE(0,J)
                        ANNUAL_PRODUCT_HOURS(J) = &
                                ANNUAL_PRODUCT_HOURS(J) + &
                                                       PRODUCT_HOURS(J)
!
                        PRODUCT_PRICE(0,J) = &
                                  PRODUCT_PRICE(0,J) / PRODUCT_HOURS(J)
                        PRODUCT_SCARCITY(0,J) = &
                               PRODUCT_SCARCITY(0,J) / PRODUCT_HOURS(J)
                        PRODUCT_VOLATILITY(0,J) = &  
                          ( (1./(PRODUCT_HOURS(J)-1.))* &
                                      SUM_SQUARED_DEVIATIONS(0,J) )**.5
                        PRODUCT_HEATRATE(0,J) = &
                             PRODUCT_HEATRATE(0,J) / &
                                 (PRODUCT_HOURS(J) * UPPER_TRANS_GROUP)
                        PRODUCT_MARGINAL_FUEL(0,J) = &
                             PRODUCT_MARGINAL_FUEL(0,J) / &
                                 (PRODUCT_HOURS(J) * UPPER_TRANS_GROUP)
                        PRODUCT_FUEL_PRICE(0,J) = &
                             PRODUCT_FUEL_PRICE(0,J) / & 
                                 (PRODUCT_HOURS(J) * UPPER_TRANS_GROUP)
                     ELSE
                        PRODUCT_VOLATILITY(0,J) = 0.
                     ENDIF
                     DAILY_PRODUCTS_REC = RPTREC(DAILY_PRODUCTS_UNIT)

                     WRITE(DAILY_PRODUCTS_UNIT,REC=DAILY_PRODUCTS_REC) &
                       PRT_ENDPOINT(), &
                       FLOAT(CURRENT_YEAR), &
                       CL_MONTH_NAME(R_MONTH), &
                       MULTI_AREA_NAME(0), &
                       LOCAL_PRODUCT_TYPE(J), &
                       PRODUCT_HOURS(J), &
                       PRODUCT_PRICE(0,J), &
                       PRODUCT_VOLATILITY(0,J), &
                       PRODUCT_QUANTITY(0,J), &
                       PRODUCT_SCARCITY(0,J), &
                       PRODUCT_HEATRATE(0,J), &
                       PRODUCT_FUEL_PRICE(0,J), &
                       EV_DATA_SOURCE, &
                       PRODUCT_MARGINAL_FUEL(0,J)*100.
                     DAILY_PRODUCTS_REC = DAILY_PRODUCTS_REC + 1

                  ELSE ! I > 0
!
                     ANNUAL_PRODUCT_PRICE(I,J) = &
                                ANNUAL_PRODUCT_PRICE(I,J) + &
                                                     PRODUCT_PRICE(I,J)
                     ANNUAL_PRODUCT_SCARCITY(I,J) = &
                                ANNUAL_PRODUCT_SCARCITY(I,J) + &
                                                  PRODUCT_SCARCITY(I,J)
                     ANNUAL_PRODUCT_QUANTITY(I,J) = &
                                ANNUAL_PRODUCT_QUANTITY(I,J) + &
                                                  PRODUCT_QUANTITY(I,J)
                     ANNUAL_PRODUCT_HEATRATE(I,J) = &
                                ANNUAL_PRODUCT_HEATRATE(I,J) + &
                                                  PRODUCT_HEATRATE(I,J)
                     ANNUAL_PRODUCT_MARGINAL_FUEL(I,J) = &
                                ANNUAL_PRODUCT_MARGINAL_FUEL(I,J) + &
                                             PRODUCT_MARGINAL_FUEL(I,J)
                     ANNUAL_PRODUCT_FUEL_PRICE(I,J) = &
                                ANNUAL_PRODUCT_FUEL_PRICE(I,J) + &
                                                PRODUCT_FUEL_PRICE(I,J)
!
                     IF(PRODUCT_HOURS (J) > 1.) THEN
                        PRODUCT_PRICE(I,J) = &
                                  PRODUCT_PRICE(I,J) / PRODUCT_HOURS(J)
                        PRODUCT_SCARCITY(I,J) = &
                               PRODUCT_SCARCITY(I,J) / PRODUCT_HOURS(J)
                        PRODUCT_VOLATILITY(I,J) = &
                          ( (1./(PRODUCT_HOURS(J)-1.))* &
                                      SUM_SQUARED_DEVIATIONS(I,J) )**.5
                        PRODUCT_HEATRATE(I,J) = &
                          PRODUCT_HEATRATE(I,J)/PRODUCT_HOURS(J)
                        PRODUCT_MARGINAL_FUEL(I,J) = &
                          PRODUCT_MARGINAL_FUEL(I,J)/PRODUCT_HOURS(J)
                        PRODUCT_FUEL_PRICE(I,J) = &
                          PRODUCT_FUEL_PRICE(I,J)/PRODUCT_HOURS(J)
                     ELSE
                        PRODUCT_VOLATILITY(I,J) = 0.
                        PRODUCT_HEATRATE(I,J) = 0.
                        PRODUCT_MARGINAL_FUEL(I,J) = 0.
                        PRODUCT_FUEL_PRICE(I,J) = 0.
                     ENDIF
                     DAILY_PRODUCTS_REC = RPTREC(DAILY_PRODUCTS_UNIT)

                     WRITE(DAILY_PRODUCTS_UNIT,REC=DAILY_PRODUCTS_REC) &
                       PRT_ENDPOINT(), &
                       FLOAT(CURRENT_YEAR), &
                       CL_MONTH_NAME(R_MONTH), &
                       MULTI_AREA_NAME(I), &
                       LOCAL_PRODUCT_TYPE(J), &
                       PRODUCT_HOURS(J), &
                       PRODUCT_PRICE(I,J), &
                       PRODUCT_VOLATILITY(I,J), &
                       PRODUCT_QUANTITY(I,J), &
                       PRODUCT_SCARCITY(I,J), &
                       PRODUCT_HEATRATE(I,J), &
                       PRODUCT_FUEL_PRICE(I,J), &
                       EV_DATA_SOURCE, &
                       PRODUCT_MARGINAL_FUEL(I,J)*100.
                     DAILY_PRODUCTS_REC = DAILY_PRODUCTS_REC + 1
                  ENDIF ! I > 0
               ENDDO ! PRODUCTS
            ENDDO ! TRANS GROUPS
         ENDIF ! YES DAILY PRODUCTS REPORT
!
!
! 6/22/98.
!
         TOTAL_LOAD_BEFORE_HYDRO = 0.
         TOTAL_HYDRO_GENERATION = 0.
         TOTAL_HYDRO_CAPACITY = 0.
         TOTAL_HYDRO_ROR = 0.
         TOTAL_EFFECTIVE_CAPACITY = 0.
         TOTAL_NATIVE_COST = 0.
         TOTAL_SPINNING_MWH = 0.
         TOTAL_NON_COIN_PEAK = 0.
         TOTAL_NON_COIN_BASE = 0.
         TOTAL_UNSERVED = 0.
         TOTAL_ABOVE_RESOURCES = 0.
         TOTAL_UNSERVED_COST = 0.
         TOTAL_COST_ABOVE_RESOURCES = 0.
!         
         ANNUAL_PURCHASES = 0.
         ANNUAL_PURCHASE_COST = 0.
         ANNUAL_SALES = 0.
         ANNUAL_SALES_REVENUE = 0.
!
         ANNUAL_LOAD_B4_SALES = 0.
!
         ANNUAL_LOAD_AFTER_SALES = 0.
!
         ANNUAL_PRO_COST_B4_SALES = 0.
         ANNUAL_PRO_COST_AFTER_SALES = 0.
!         
!         
         IF(R_MONTH == LAST_MONTHLY_TRANSACT ) THEN
!         
            IF(YES_TRANSACT_DAILY_REPORTS) THEN
               DO I = 0, UPPER_TRANS_GROUP
                  DO J = 1, NUM_PRODUCTS
                     IF(I == 0) THEN
                        IF(ANNUAL_PRODUCT_HOURS (J) > 1.) THEN
                           ANNUAL_PRODUCT_PRICE(0,J) = &
                                  ANNUAL_PRODUCT_PRICE(0,J) / &
                                                ANNUAL_PRODUCT_HOURS(J)
                           ANNUAL_PRODUCT_SCARCITY(0,J) = &
                                  ANNUAL_PRODUCT_SCARCITY(0,J) / &
                                                ANNUAL_PRODUCT_HOURS(J)
                           ANNUAL_PRODUCT_HEATRATE(0,J) = &
                                  ANNUAL_PRODUCT_HEATRATE(0,J) / &
                                                ANNUAL_PRODUCT_HOURS(J)
                           ANNUAL_PRODUCT_MARGINAL_FUEL(0,J) = &
                                  ANNUAL_PRODUCT_MARGINAL_FUEL(0,J) / &
                                                ANNUAL_PRODUCT_HOURS(J)
                           ANNUAL_PRODUCT_FUEL_PRICE(0,J) = &
                                  ANNUAL_PRODUCT_FUEL_PRICE(0,J) / &
                                                ANNUAL_PRODUCT_HOURS(J)
                        ELSE
!                          PRODUCT_VOLATILITY(0,J) = 0.
                        ENDIF
                        DAILY_PRODUCTS_REC = RPTREC(DAILY_PRODUCTS_UNIT)

                        WRITE(DAILY_PRODUCTS_UNIT, &
                                              REC=DAILY_PRODUCTS_REC) &
                          PRT_ENDPOINT(), &
                          FLOAT(CURRENT_YEAR), &
                          CL_MONTH_NAME(13), &
                          MULTI_AREA_NAME(0), &
                          LOCAL_PRODUCT_TYPE(J), &
                          ANNUAL_PRODUCT_HOURS(J), &
                          ANNUAL_PRODUCT_PRICE(0,J), &
                          PRODUCT_VOLATILITY(0,J), &
                          ANNUAL_PRODUCT_QUANTITY(0,J), &
                          ANNUAL_PRODUCT_SCARCITY(0,J), &
                          ANNUAL_PRODUCT_HEATRATE(0,J), &
                          ANNUAL_PRODUCT_FUEL_PRICE(0,J), &
                          EV_DATA_SOURCE,& 
                          ANNUAL_PRODUCT_MARGINAL_FUEL(0,J)*100.
                        DAILY_PRODUCTS_REC = DAILY_PRODUCTS_REC + 1
                     ELSE ! I > 0
!         
                        IF(ANNUAL_PRODUCT_HOURS (J) > 1.) THEN
                           ANNUAL_PRODUCT_PRICE(I,J) = &
                                  ANNUAL_PRODUCT_PRICE(I,J) / &
                                                ANNUAL_PRODUCT_HOURS(J)
                           ANNUAL_PRODUCT_SCARCITY(I,J) = &
                                  ANNUAL_PRODUCT_SCARCITY(I,J) / &
                                                ANNUAL_PRODUCT_HOURS(J)
                           ANNUAL_PRODUCT_HEATRATE(I,J) = &
                                  ANNUAL_PRODUCT_HEATRATE(I,J) / &
                                                ANNUAL_PRODUCT_HOURS(J)
                           ANNUAL_PRODUCT_MARGINAL_FUEL(I,J) = &
                                  ANNUAL_PRODUCT_MARGINAL_FUEL(I,J) / &
                                                ANNUAL_PRODUCT_HOURS(J)
                           ANNUAL_PRODUCT_FUEL_PRICE(I,J) = &
                                  ANNUAL_PRODUCT_FUEL_PRICE(I,J) / &
                                                ANNUAL_PRODUCT_HOURS(J)
                        ELSE
                        ENDIF
                        DAILY_PRODUCTS_REC = RPTREC(DAILY_PRODUCTS_UNIT)

                        WRITE(DAILY_PRODUCTS_UNIT, &
                                              REC=DAILY_PRODUCTS_REC) &
                          PRT_ENDPOINT(), &
                          FLOAT(CURRENT_YEAR), &
                          CL_MONTH_NAME(13), &
                          MULTI_AREA_NAME(I), &
                          LOCAL_PRODUCT_TYPE(J), &
                          ANNUAL_PRODUCT_HOURS(J), &
                          ANNUAL_PRODUCT_PRICE(I,J), &
                          PRODUCT_VOLATILITY(I,J), &
                          ANNUAL_PRODUCT_QUANTITY(I,J), &
                          ANNUAL_PRODUCT_SCARCITY(I,J), &
                          ANNUAL_PRODUCT_HEATRATE(I,J), &
                          ANNUAL_PRODUCT_FUEL_PRICE(I,J), &
                          EV_DATA_SOURCE, &
                          ANNUAL_PRODUCT_MARGINAL_FUEL(I,J)*100.
                        DAILY_PRODUCTS_REC = DAILY_PRODUCTS_REC + 1
                     ENDIF ! I = 0
                  ENDDO ! PRODUCTS
               ENDDO ! I = ?
            ENDIF ! WRITE DAILY REPORTS FOR ANNUAL
            DO I = 1, UPPER_TRANS_GROUP
               IF(M_ANNUAL_PURCHASE_ENERGY(I) > 0.) THEN
                  AVE_BUY = M_ANNUAL_PURCHASE_COSTS(I) / &
                                            M_ANNUAL_PURCHASE_ENERGY(I)
               ELSE
                  AVE_BUY = 0.
               ENDIF
               IF(M_ANNUAL_SALES_ENERGY(I) > 0.) THEN
                  AVE_SELL = M_ANNUAL_SALES_REVENUES(I) / &
                                               M_ANNUAL_SALES_ENERGY(I)
               ELSE
                  AVE_SELL = 0.
               ENDIF
               IF(M_ANNUAL_LOAD_B4_SALES(I) > 0.) THEN
                  AVE_B4 = M_ANNUAL_PRO_COST_B4_SALES(I) / &
                                              M_ANNUAL_LOAD_B4_SALES(I)
               ELSE
                  AVE_B4 = 0.
               ENDIF
               IF(M_ANNUAL_LOAD_AFTER_SALES(I) > 0.) THEN
                  AVE_AFTER = M_ANNUAL_PRO_COST_AFTER_SALES(I) / &
                                           M_ANNUAL_LOAD_AFTER_SALES(I)
               ELSE
                  AVE_AFTER = 0.
               ENDIF
               IF( ABS(M_ANNUAL_LOAD_AFTER_SALES(I) - &
                                  M_ANNUAL_LOAD_B4_SALES(I)) > 0.) THEN
                  MONTHLY_INCREMENTAL_COST = &
                    (M_ANNUAL_PRO_COST_AFTER_SALES(I) - &
                                   M_ANNUAL_PRO_COST_B4_SALES(I))/ &
                    (M_ANNUAL_LOAD_AFTER_SALES(I) - &
                                  M_ANNUAL_LOAD_B4_SALES(I))
               ELSE
                  MONTHLY_INCREMENTAL_COST = 0.
               ENDIF
!
               TBVars(13,I,23)= M_ANNUAL_PURCHASE_ENERGY(I)
               TBVars(13,I,24)= M_ANNUAL_SALES_ENERGY(I)
               IF(YES_TRANSACT_MONTHLY_REPORTS) THEN
                  TRANS_ANN_REC = RPTREC(TRANS_ANN_UNIT)
               ! msgtrmon.rpt (Monthly Summary)

                  WRITE(TRANS_ANN_UNIT,REC= TRANS_ANN_REC) &
                    PRT_ENDPOINT(), &
                    FLOAT(CURRENT_YEAR), &
                    CL_MONTH_NAME(13), &
                    MULTI_AREA_NAME(I), &
                    M_ANNUAL_PURCHASE_ENERGY(I), &
                    M_ANNUAL_PURCHASE_COSTS(I)/1000000., &
                    AVE_BUY, &
                    M_ANNUAL_SALES_ENERGY(I), &
                    M_ANNUAL_SALES_REVENUES(I)/1000000., &
                    AVE_SELL, &
                    M_ANNUAL_LOAD_B4_SALES(I), &
                    M_ANNUAL_PRO_COST_B4_SALES(I)/1000000., &
                    AVE_B4, &
                    M_ANNUAL_LOAD_AFTER_SALES(I), &
                    M_ANNUAL_PRO_COST_AFTER_SALES(I)/1000000., &
                    AVE_AFTER, &
                    MONTHLY_INCREMENTAL_COST, &
                    ANNUAL_TL_MWH(I), &
                    ANNUAL_TL_HYDRO_MWH(I), &
                    ANNUAL_EFFECTIVE_CAPACITY(I)/12., &
                    ANNUAL_TL_PEAK(I), &
                    ANNUAL_TL_BASE(I), &
                    M_ANNUAL_NATIVE_COST(I)/1000000., &
                    ANNUAL_SPINNING_MWH(I), &
                    ANNUAL_UNSERVED_ENERGY(I,0), &
                    ANNUAL_UNSERVED_ENERGY_COST(I)/1000000., &
                    ANNUAL_COST_ABOVE_RESOURCES(I,0)/1000000., &
                    ANNUAL_COIN_PEAK(I), &
                    ANNUAL_TL_HYDRO_MW(I)/12., &
                    ANNUAL_TL_HYDRO_ROR(I)/12., &
                    ANNUAL_ABOVE_RESOURCES(I,0)
                  TRANS_ANN_REC = TRANS_ANN_REC + 1
!     
                  ANNUAL_PURCHASES = ANNUAL_PURCHASES + &
                                      M_ANNUAL_PURCHASE_ENERGY(I)
                  ANNUAL_PURCHASE_COST = ANNUAL_PURCHASE_COST + &
                                             M_ANNUAL_PURCHASE_COSTS(I)
                  ANNUAL_SALES = ANNUAL_SALES + M_ANNUAL_SALES_ENERGY(I)
                  ANNUAL_SALES_REVENUE = ANNUAL_SALES_REVENUE + &
                                             M_ANNUAL_SALES_REVENUES(I)
!
                  ANNUAL_LOAD_B4_SALES = ANNUAL_LOAD_B4_SALES + &
                                              M_ANNUAL_LOAD_B4_SALES(I)
!
                  ANNUAL_LOAD_AFTER_SALES = ANNUAL_LOAD_AFTER_SALES + &
                                           M_ANNUAL_LOAD_AFTER_SALES(I)
!
                 ANNUAL_PRO_COST_B4_SALES = ANNUAL_PRO_COST_B4_SALES + &
                                     M_ANNUAL_PRO_COST_B4_SALES(I)
                  ANNUAL_PRO_COST_AFTER_SALES = &
                              ANNUAL_PRO_COST_AFTER_SALES + &
                                  M_ANNUAL_PRO_COST_AFTER_SALES(I)
!     
                  TOTAL_LOAD_BEFORE_HYDRO = TOTAL_LOAD_BEFORE_HYDRO + &
                                                       ANNUAL_TL_MWH(I)
                  TOTAL_HYDRO_GENERATION = TOTAL_HYDRO_GENERATION + &
                                                 ANNUAL_TL_HYDRO_MWH(I)
                  TOTAL_HYDRO_CAPACITY = TOTAL_HYDRO_CAPACITY + &
                                                  ANNUAL_TL_HYDRO_MW(I)
                  TOTAL_HYDRO_ROR = TOTAL_HYDRO_ROR + &
                                                 ANNUAL_TL_HYDRO_ROR(I)
                 TOTAL_EFFECTIVE_CAPACITY = TOTAL_EFFECTIVE_CAPACITY + &
                                           ANNUAL_EFFECTIVE_CAPACITY(I)
                  TOTAL_SPINNING_MWH = TOTAL_SPINNING_MWH + &
                                                 ANNUAL_SPINNING_MWH(I)
                  TOTAL_NATIVE_COST = TOTAL_NATIVE_COST + &
                                                M_ANNUAL_NATIVE_COST(I)
                  TOTAL_NON_COIN_PEAK =  &
                                TOTAL_NON_COIN_PEAK + ANNUAL_TL_PEAK(I)
                  TOTAL_NON_COIN_BASE = &
                                TOTAL_NON_COIN_BASE + ANNUAL_TL_BASE(I)
!    
                  TOTAL_UNSERVED = TOTAL_UNSERVED + &
                                            ANNUAL_UNSERVED_ENERGY(I,0)
                  TOTAL_ABOVE_RESOURCES = TOTAL_ABOVE_RESOURCES + &
                                            ANNUAL_ABOVE_RESOURCES(I,0)
                  TOTAL_UNSERVED_COST = TOTAL_UNSERVED_COST + &
                                       ANNUAL_UNSERVED_ENERGY_COST(I)
                  TOTAL_COST_ABOVE_RESOURCES = &
                       TOTAL_COST_ABOVE_RESOURCES + & 
                                       ANNUAL_COST_ABOVE_RESOURCES(I,0)
!     
                  TRANS_EXC_REC = RPTREC(TRANS_EXC_UNIT)

                  WRITE(TRANS_EXC_UNIT,REC= TRANS_EXC_REC) & 
                    PRT_ENDPOINT(), &
                    FLOAT(CURRENT_YEAR), &
                    CL_MONTH_NAME(13), &
                    MULTI_AREA_NAME(I), &
                    (ANNUAL_TRANSACTION_MWH(I,J), &
                                  J=1,MIN(INT(31,2),TOTAL_BUYER_INDEX))
                  TRANS_EXC_REC = TRANS_EXC_REC + 1
               ENDIF
!
            ENDDO ! TRANSACT GROUPS LOOP FOR ANNUAL
!
            IF(YES_TRANSACT_MONTHLY_REPORTS) THEN
               IF(PURCHASE_ENERGY(R_MONTH) > 0.) THEN
                  AVE_BUY = PURCHASE_COSTS(R_MONTH) / &
                                    PURCHASE_ENERGY(R_MONTH)
               ELSE
                  AVE_BUY = 0.
               ENDIF
               IF(SALES_ENERGY(R_MONTH) > 0.) THEN
                  AVE_SELL = SALES_REVENUE(R_MONTH) / &
                                    SALES_ENERGY(R_MONTH)
               ELSE
                  AVE_SELL = 0.
               ENDIF
               IF(MONTHLY_LOAD_B4_SALES(R_MONTH) > 0.) THEN
                  AVE_B4 = MONTHLY_PRO_COST_B4_SALES(R_MONTH) / &
                                    MONTHLY_LOAD_B4_SALES(R_MONTH)
               ELSE
                  AVE_B4 = 0.
               ENDIF
               IF(MONTHLY_LOAD_AFTER_SALES(R_MONTH) > 0.) THEN
                  AVE_AFTER = MONTHLY_PRO_COST_AFTER_SALES(R_MONTH) / &
                                    MONTHLY_LOAD_AFTER_SALES(R_MONTH)
               ELSE
                  AVE_AFTER = 0.
               ENDIF
!            
               TBVars(13,0,23)= ANNUAL_PURCHASES
               TBVars(13,0,24)= ANNUAL_SALES
               TRANS_ANN_REC = RPTREC(TRANS_ANN_UNIT)
               ! msgtrmon.rpt (Monthly Summary)

               WRITE(TRANS_ANN_UNIT,REC= TRANS_ANN_REC) &
                    PRT_ENDPOINT(), &
                    FLOAT(CURRENT_YEAR), &
                    CL_MONTH_NAME(13), &
                    MULTI_AREA_NAME(0), &
                    ANNUAL_PURCHASES, &
                    ANNUAL_PURCHASE_COST/1000000., &
                    AVE_BUY, &
                    ANNUAL_SALES, &
                    ANNUAL_SALES_REVENUE/1000000., &
                    AVE_SELL, &
                    ANNUAL_LOAD_B4_SALES, &
                    ANNUAL_PRO_COST_B4_SALES/1000000., &
                    AVE_B4, &
                    ANNUAL_LOAD_AFTER_SALES, &
                    ANNUAL_PRO_COST_AFTER_SALES/1000000., &
                    AVE_AFTER, &
                    MONTHLY_INCREMENTAL_COST, &
                    TOTAL_LOAD_BEFORE_HYDRO, &
                    TOTAL_HYDRO_GENERATION, &
                    TOTAL_EFFECTIVE_CAPACITY/12., &
                    TOTAL_NON_COIN_PEAK, &
                    TOTAL_NON_COIN_BASE, &
                    TOTAL_NATIVE_COST/1000000., &
                    TOTAL_SPINNING_MWH, &
                    TOTAL_UNSERVED, &
                    TOTAL_UNSERVED_COST/1000000., &
                    TOTAL_COST_ABOVE_RESOURCES/1000000., &
                    ANNUAL_COIN_PEAK(0), &
                    TOTAL_HYDRO_CAPACITY/12., &
                    TOTAL_HYDRO_ROR/12., &
                    TOTAL_ABOVE_RESOURCES
                  TRANS_ANN_REC = TRANS_ANN_REC + 1
            ENDIF
!
            DEALLOCATE( &
                 M_ANNUAL_PURCHASE_ENERGY, &
                 M_ANNUAL_PURCHASE_COSTS, &
                 M_ANNUAL_SALES_ENERGY, &
                 M_ANNUAL_SALES_REVENUES, &
                 M_ANNUAL_NATIVE_COST, &
                 M_ANNUAL_LOAD_B4_SALES, &
                 M_ANNUAL_PRO_COST_B4_SALES, &
                 M_ANNUAL_LOAD_AFTER_SALES, &
                 M_ANNUAL_PRO_COST_AFTER_SALES, &
                 ANNUAL_TRANSACTION_MWH, &
                 ANNUAL_TL_MWH, &
                 ANNUAL_TL_PEAK, &
                 ANNUAL_COIN_PEAK, &
                 ANNUAL_TL_BASE, &
                 ANNUAL_EFFECTIVE_CAPACITY, &
                 ANNUAL_TL_HYDRO_MWH, &
                 ANNUAL_TL_HYDRO_MW, &
                 ANNUAL_TL_HYDRO_ROR, &
                 ANNUAL_SPINNING_MWH, &
                 ANNUAL_PRODUCT_PRICE, &
                 ANNUAL_PRODUCT_SCARCITY, &
                 ANNUAL_PRODUCT_QUANTITY, &
                 ANNUAL_PRODUCT_HEATRATE, &
                 ANNUAL_PRODUCT_MARGINAL_FUEL, &
                 ANNUAL_PRODUCT_FUEL_PRICE, &
                 ANNUAL_PRODUCT_HOURS)

         ENDIF ! R_MONTH == LAST_MONTHLY_TRANSACT 

         I = 0
         DEALLOCATE( HOURLY_MARGINAL_COST, &
                    HOURLY_LAST_PRICE, &
                    LAST_TRANS_MC)
         DEALLOCATE( TIE_FLOW, &
                    MARGINAL_COST_DELTA, &
                    M_HOURLY_PRO_COST_B4_SALES, &
                    M_HOURLY_PRO_COST_AFTER_SALES, &
                    HOURLY_CAPACITY, &
                    HOURLY_DERIVATIVES, &
                    HOURLY_EUE, &
                    CAPPED_PRICE, &
                    PRICE_MINIMUM, &
                    M_HOURLY_INCREMENTAL_COST)
!
! ARRAYS THAT WERE NOT BEING DEALLOCATED AS OF 8/8/01 MSG         
         DEALLOCATE(CUM_REDUNDANT_TRANSACTION, &
                   TRANSACTIONS_PER_HOUR, &
                   LONG_PATH_TRANSACTION, &
                   MW_FOR_TRANSACTION, &
                   LAST_MW_FOR_TRANSACTION, &
                   M_HOURLY_MC_B4_SALES, &
                   HOURLY_SELL_MWH, &
                   MONTHLY_TRANSACTION_MWH, &
                   TOTAL_DELIVERED_COST, &
                   SELLERS_LOCAL_CAPACITY, &
                   ALLOWED_TRANSACTION_PAIR, &
                   LONG_PATH_TRANSACTION_PAIR, &
                   OFF_PEAK_SPINNING_CAPACITY)
! END ARRAYS
!
         DEALLOCATE( &
           PRODUCT_PRICE, &
           PRODUCT_VOLATILITY, &
           PRODUCT_QUANTITY, &
           PRODUCT_HEATRATE, &
           PRODUCT_MARGINAL_FUEL, &
           PRODUCT_FUEL_PRICE, &
           PRODUCT_HOURS, &
           PRODUCT_MEAN_RETURN, &
           PRODUCT_SCARCITY, &
           SUM_SQUARED_DEVIATIONS, &
           SCARCITY_COST, &
           ENERGY_COST, &
           PRODUCT_DAILY_RETURN, &
           BUY_FOR_TRANSACTION, &
           LAST_BUY_FOR_TRANSACTION, &
           LAST_BUYER, &
           LAST_SELLER, &
           PRICE_ANCHORED, &
           SELL_FOR_TRANSACTION, &
           LAST_SELL_FOR_TRANSACTION, &
           MONTH_COIN_PEAK, &
           MONTH_NON_COIN_PEAK, &
           MARGIN_FOR_TRANSACTION)
!
         DEALLOCATE( TRANSACTIONS_WITHIN_HOUR)
         DEALLOCATE( M_MONTHLY_PRO_COST_AFTER_SALES)
         DEALLOCATE( M_MONTHLY_PRO_COST_B4_SALES)
         DEALLOCATE( M_MONTHLY_LOAD_AFTER_SALES)
         DEALLOCATE( M_HOURLY_LOAD_B4_SALES)
         DEALLOCATE( M_UNSERVED_ENERGY, &
                    M_ABOVE_RESOURCES, &
                    M_UNSERVED_ENERGY_COST)
         DEALLOCATE( NET_MARGIN,BEST_PRICE_TO_BUYER)
         DEALLOCATE( HOURLY_LAMDA)
         DEALLOCATE( HOURLY_TRANS_GROUP_LOAD_ACTIVE)
         DEALLOCATE( HOURLY_TRANS_GROUP_GEN_ACTIVE)
         DEALLOCATE( M_PURCHASE_ENERGY, &
                    M_PURCHASE_COSTS, &
                    MARKET_COST_ABOVE_RESOURCES, &
                    M_SALES_ENERGY, &
                    M_SALES_REVENUES, &
                    M_NATIVE_COST, &
                    M_SPINNING_MWH, &
                    M_MONTHLY_LOAD_B4_SALES, &
                    M_HOURLY_LOADS, &
                    TEST_HOUR_TIE_LIMIT, &
                    trans_ror_cap, &
                    HOURLY_DUMP_CAPACITY, &
                    HOURLY_DUMP_BEFORE, &
                    TRANS_MUST_CAPACITY, &
                    TRANS_SPINNING_CAPACITY, &
                    HOURLY_SPINNING_CAPACITY, &
                    DAILY_PEAK, &
                    TRANS_RAMP_UP, &
                    TRANS_RAMP_DOWN, &
                    TRANS_MAX_IMPORT, &
                    TRANS_MAX_EXPORT, &
                    LAST_HOUR_SELL, &
                    HOURLY_TRANSACTION, &
                    MAX_HOURLY_IMPORT, &
                    MAX_HOURLY_EXPORT, &
                    SCARCITY_MULT)
         IF(ALLOCATED( MONTHLY_MARKET_PRICES)) &
                                     DEALLOCATE( MONTHLY_MARKET_PRICES)
!
         CALL CLS(15,9,33)
!
      RETURN

      ENTRY GET_ANNUAL_UNSERVED_COST(R_TRANS_GROUP, &
                                    R_ANNUAL_UNSERVED_COST, &
                                    R_ANNUAL_UNSERVED_ENERGY, &
                                    R_MONTH)

!
         IF(ALLOCATED(ANNUAL_COST_ABOVE_RESOURCES) .AND. &
                                         PRICE_ONLY_WHOLESALE_REV) THEN
            IF(R_TRANS_GROUP > 0 .AND. &
                               R_TRANS_GROUP <= UPPER_TRANS_GROUP) THEN
               R_ANNUAL_UNSERVED_COST = &
                           ANNUAL_COST_ABOVE_RESOURCES(R_TRANS_GROUP, &
                                                       R_MONTH)
               R_ANNUAL_UNSERVED_ENERGY = &
                                ANNUAL_ABOVE_RESOURCES(R_TRANS_GROUP, &
                                                       R_MONTH)

            ELSE
               R_ANNUAL_UNSERVED_COST = 0.
               WRITE(4,*) "UNTRAPPED TRANS GROUP IN UNSERVED CALC", &
                                                         R_TRANS_GROUP
            ENDIF
         ELSE
            R_ANNUAL_UNSERVED_COST = 0.
         ENDIF
!
      RETURN
      END subroutine REGIONAL_TRANSACTION_ANALYSIS
