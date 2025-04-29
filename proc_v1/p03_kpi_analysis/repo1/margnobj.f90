
!     MARGNOBJ.FOR
!     Copyright(c) M.S.Gerber & Associates 2000
!
!     Created: 6/25/2003 4:08:49 PM
!     Author : GREG TURK
!     Last change:
!            MSG 7/5/2016 11:05:24 AM



!
!     CALCULATE THE MARGINAL COST OF A THERMAL SYSTEM
!

!
!

      RECURSIVE FUNCTION CAL_MARGINAL_COST(ISEAS,NBLOK2,MWBLOK,UNIT, &
                                   MAINTENANCE_RATE,BLKNO,NUNITS, &
                                   PERIOD_COUNTER,BASE,PEAK, &
                                   UNIT_CAP_AFTER_LOSSES,UNITCAP)
      use end_routine, only: end_program, er_message
      use SpinDriftLib
      use prod_arrays_dimensions
      use logging
      use miscmod
      use grx_planning_routines
      use rptreccontrol
      use eco
      USE IREC_ENDPOINT_CONTROL
      USE ANNUAL_MARG_COST_ALLOCAT_ARRAYS
      use params
      use kepcocom
      use prodcom
      USE SIZECOM
      use hesi
      use spindriftlib
      use prod_arrays_dimensions
      use prodcom
      use tf_objt_interfaces
      use mod_base_year
      use end_routine, only: er_message, end_program
      use program_state
      use debugtrace
      use pos_trace
      use energy_trace
      
      implicit none
      SAVE
      real (kind=4) :: lastcuu=0
      integer (kind=4) :: numcmme=0
      integer :: allocstat
      integer, save :: file_trace_cmme=0 ! cal_mar_monthly_energy_routine
      integer, save :: file_trace_wcr=0
      integer, save :: file_trace_gac=0 !
      integer, parameter :: MAX_PROD_TYPES=18
      LOGICAL(kind=1) :: CAL_MARGINAL_COST,GET_THOSE_MARGINAL_COSTS, &
                  APPLY_TRANS_REV_TO_WHOLESALE, &
                  VOID_LOGICAL,GET_SCARCITY_INFO,DRATE_TRANSACT, &
                  RECALCULATE_OUTAGES,CAL_OUTAGE_UNIT_GENERATION, &
                  WRITE_DAILY_THERMAL_UNIT, &
                  DATE_ACTIVE, &
                  YES_DAILY_THERMAL_RPT_ACTIVE, &
                  YES_REPORT_DAILY_PRIME_MOVER, &
                  YES_REPORT_DAILY_THERMAL, &
                  REPORT_DAILY_THERMAL, &
                  DAILY_THERMAL_REPORT_ACTIVE, &
                  REPORT_DAILY_PRIME_MOVER, &
                  UPDATE_OUTAGE_DATA, &
                  OUTAGES_ADJUSTED, &
                  ADJUST_FOR_OUTAGES, &
                  OUTAGE_EVENTS_IN_HOUR, &
                  YES_OUTAGE_EVENTS_IN_HOUR, &
                  PRICE_ONLY_WHOLESALE_REV, & !  /.TRUE./,
                  NEW_SCARCITY_LOGIC, & !  /.FALSE./,
                  OUTAGES_IN_MONTH ! /.FALSE./
      LOGICAL(kind=1) :: &
                  ECITY, & !  /.FALSE./,
                  HOOSIER, & !  /.FALSE./,
                  UNIT_COMMITMENT_LOGIC, & !  /.FALSE./,
                  SCHEDULE_FOR, & !  /.FALSE./,
                  SCHEDULE_MOR, & !  /.FALSE./,
                  YES_HOOSIER, &
                  YES_UNIT_COMMITMENT_LOGIC, &
                  INIT_DEPTH_PRICE, &
                  YES_DETAILED_FOR, &
                  YES_DETAILED_MAINTENANCE, &
                  SPEED_INDEX, &
                  YES_ECITIES_UNITS_ACTIVE, &
                  PRICE_MODE=.FALSE., &
                  YES_STRICT_MARKET_PRICE, &
                  YES_MULTI_AREA_PRICE
      REAL(kind=4) :: R_SYSTEM_MARGINAL_COST,GET_PRODUCTION_LEVEL, &
                  GET_MARGINAL_COST,GET_AVERAGE_COST, &
                  PUT_MARKET_REV_AND_EXP,REV,SALE,MONTHLY_SALE, & !  /0./,
                  UNIT_CONTRIB,MW_AFTER_MAINTENANCE,MN_RATE, &
                  PUT_BUYERS_LOAD, &
                  GET_CLOSEST_MARGINAL_COST, &
                  GET_LAST_TRANS_PRICE, &

                  MONTHLY_ECONOMY_BOUGHT,MONTHLY_ECONOMY_COST, &
                  MONTHLY_ECONOMY_SOLD,MONTHLY_ECONOMY_REVENUE, &
                  ANNUAL_ECONOMY_BOUGHT,ANNUAL_ECONOMY_COST, &
                  ANNUAL_ECONOMY_SOLD,ANNUAL_ECONOMY_REVENUE, &
                  POS_PERCENT, &
                  TOTAL_POS_PERCENT, &
                  CUM_LOAD,CUM_GEN,CUM_DIFF,LOST_ENERGY, &
                  UNIT_CAP_AFTER_LOSSES(*), &
                  UNITCAP(*), &
                  R_AVAIL(*), &
                  R_OUT(*)
      REAL(kind=4) :: &
                  R_DAILY_OUT(*), &
                  GET_BLOCK_AVAIL_4_MONTH, &
                  GET_BLOCK_AVAIL_4_HOUR, &
                  GET_BLOCK_OUTAGE_4_HOUR, &
                  MONTHLY_MARKET_SALES=0., &
                  MARKET_FLASH,TEMP_TRANSACTION, &
                  R_BUYER_MC,R_BUYER_LOAD, &
                  R_SELLER_MC,R_SELLER_LOAD, &
                  R_SELLER_LIMIT, &
                  MAX_SELL_CAPACITY, &
                  MAX_BUY_CAPACITY, &
                  SUM_AVERAGE_COST_SALES=0., &
                  SUM_PRODUCTION_SALES=0., &
                  FIRST_CAPACITY_VALUE, &
                  FIRST_CAPACITY_PERCENT, &
                  FIRST_CAPACITY_MW, &
                  SECOND_CAPACITY_VALUE, &
                  SECOND_CAPACITY_PERCENT, &
                  SECOND_CAPACITY_MW
      REAL(kind=4) :: &
                  THIRD_CAPACITY_VALUE, &
                  THIRD_CAPACITY_PERCENT, &
                  THIRD_CAPACITY_MW, &
                  ADDITIONAL_CAPACITY_MW(7), &
                  CAPACITY_ADDER, &
                  TOTAL_SCARCITY_MW, &
                  SCARCITY_PERCENT, &
                  TEMP_CAP, &
                  DAYS_PER_TRANS_PERIOD, &
                  SCARCITY_COST, &
                  R_SCARCITY_MULT, &
                  D_SCARCITY_MULT, &
                  GET_OUTAGE_CAPACITY, &
                  GET_BLOCK_OUTAGE_CAPACITY, &
                  TEMP_TL_PEAK, &
                  MONTH_PEAK_BY_CG, &
                  SEPA_CAPACITY=41.9, &
                  OUTAGE_HOURS_IN_PERIOD=0, &
                  R4_TEMP,R_FT
      REAL(kind=4) :: &
                  GET_NIGHT_SCARCITY_MULT, &
                  GET_WEEKEND_SCARCITY_MULT, &
                  MEROM1_EFFECTIVE_CAP=0., &
                  MEROM2_EFFECTIVE_CAP=0., &
                  RATTS1_EFFECTIVE_CAP=0., &
                  RATTS2_EFFECTIVE_CAP=0., &
                  R_MEROM1_EFFECTIVE_CAP, &
                  R_MEROM2_EFFECTIVE_CAP, &
                  R_RATTS1_EFFECTIVE_CAP, &
                  R_RATTS2_EFFECTIVE_CAP, &
                  INCREMENTAL_HEATRATE, &
                  INCREMENTAL_FUEL_PRICE, &
                  R_DELIVERED_PRICE, &
                  GET_CAPPED_PRICE, &
                  CAT1_TRANS_EA, &
                  CAT2_TRANS_EA, &
                  MCG1_TRANS_EA, &
                  MCG2_TRANS_EA, &
                  GET_MONTHLY_CONTINGENT_CAP
      REAL(kind=4) :: &
                  MONTHLY_CONTINGENT_CAPACITY, &
                  GET_ANNUAL_CL_CAPACITY, &
                  TEMP_OUTAGE, &
                  GET_VAR
      REAL(kind=8) :: & !  REAL(kind=4 before 20030429
                  MAX_SELL_PRICE,MIN_SELL_PRICE, &
                  SELLER_SLOPE,SELLER_INTERCEPT, &
                  SELLER_TEST_CAP,SELLER_TEST_PRICE, &
                  SELLER_LOWER_CAP,SELLER_UPPER_CAP, &
                  SELLER_START_CAP,BUYER_START_CAP, &
                  MAX_BUY_PRICE,MIN_BUY_PRICE, &
                  BUYER_SLOPE,BUYER_INTERCEPT, &
                  BUYER_TEST_CAP,BUYER_TEST_PRICE, &
                  BUYER_LOWER_CAP,BUYER_UPPER_CAP, &
                  R8_LOAD, &
                  R_GET_MARGINAL_COST_AT_MARKET
!
      INTEGER :: SAVE_OUTAGE_BLOCKS=0,OUTAGE_BLOCK, & !  08/28/01
                  TEMP_I1,IOS
!
      INTEGER(kind=2) :: GET_PRIMARY_MOVER, &
                  GET_PRIMARY_MOVER_INDEX, &
                  GET_THERMAL_STATE_INDEX, &
                  LM,PM, &
                  MARGINAL_UNIT_TECH_NO, &
                  MARGINAL_UNIT_TECH_HEADER
      INTEGER(kind=4) :: &
                  SAVE_START_RPT_DATE, &
                  SAVE_END_RPT_DATE, &
                  TEST_DATE
      INTEGER(kind=2) :: MAX_DATA_BASES, & !  /0/,
                  MAX_DATA_BASES_LAST_ITER, &
                  DATA_BASES,TRANS_GROUP_POINTS, &
                  MAX_RESOURCE_ID_NO, &
                  CONTRIB_INTERVALS,SAVE_HALF_POINT,HALF_POINT, &
                  MAX_TRANS_GROUPS,TRANS_GROUP,DAY_TYPE,MAX_DAY_TYPES, &
                  GET_MAX_TRANS_GROUPS,GET_MAX_DAY_TYPES, &
                  GET_MAX_TRANS_GROUP_NUMBER,MAX_TRANS_GROUP_NUMBER, & !  /0/,
                  GET_LAST_DAY_TYPE,R_DATA_BASES, &
                  LAST_BLOCK_NUMBER,CURRENT_BLOCK_NUMBER, &
                  BASE_WHOLESALE_INTERVAL,BASE_RETAIL_INTERVAL, &
                  MAX_COST_INDEX,ARRAY_POINTR, &
                  R_BUYER_DB,R_SELLER_DB, &
                  BUYER_POSITION,SELLER_POSITION, &
                  PRODUCTS,GET_TRANS_FOR_DATA_BASE,TG,C_M, & !  =0,
                  S_M=2,L_M, &
                  D_TG, &
                  TRANSACTION_MODE,FIRST_INTERVAL, &
                  R_YEAR, &
                  NUM_DETAILED_OUTAGE_DATABASES=0
      INTEGER(kind=2) :: &
                  OUTAGES_IN_MONTH_BY_TG, &
                  GET_TRANS_GROUP_POSITION, &
                  UNIT_TO_BLOCK(MAX_CL_UNITS,2), &
                  GET_UNIT_TO_OUTAGE_BLOCK, &
                  GET_TRANS_UNIT_TO_BLOCK, &
                  R_UNIT,R_BLOCK, &
                  S,ISEAS, &
                  TEMP_I2, &
                  GET_TECH_SCAR_MONTH_VECTOR, &
                  GET_NO_START_UP_UNITS, &
                  TOTAL_START_UP_UNITS, &
                  GET_START_UP_POSITION, &
                  TEMP_LAST_POINT, &
                  START_UP_INDEX, &
                  GET_START_UP_INDEX, &
                  R_TG, &
                  TEMP_I, &
                  DEPTH_TARGET_GROUP=0, &
                  M1_U=0,M2_U=0,R1_U=0,R2_U=0,WI_U=0
      INTEGER(kind=2) :: &
                  PE_U=0,DG_U=0,BE_U=0,WO_U=0, &
                  R_M1_U,R_M2_U,R_R1_U,R_R2_U,R_WI_U, &
                  R_PE_U,R_DG_U,R_BE_U,R_WO_U, &
                  GET_HOOSIER_INDICES, &
                  R_SELLER_DATA_BASES, &
                  R_SELLER_TRANS, &
                  CAT1_B=0,CAT2_B=0, &
                  MCG1_B=0,MCG2_B=0, &
                  CAT1_U=0,CAT2_U=0, &
                  MCG1_U=0,MCG2_U=0, &
                  FT,MAX_FUEL_TYPES=6
      integer (kind=2) :: SAVE_PRIMARY_MOVER(MAX_CL_UNITS)
!
      PARAMETER ( &
! 1 = ENERGY, 2 = CAPACITY, 3 = ENERGY + CAPACITY, 4 = TECHNOLOGY SCARCITY, 5 = ENERGY + CAPACITY + TECHNOLOGY SCARCITY
                  PRODUCTS = 5, &
!     +            MAX_TRANS_GROUPS = 25 & 
                  MAX_TRANS_GROUPS = 100, & !  12/4/98. GAT.
                  MAX_DAY_TYPES = 10, &
                  TRANS_GROUP_POINTS=275, &
                  CONTRIB_INTERVALS=50, &
                  MAX_COST_INDEX = 100, &
                  SAVE_HALF_POINT=75, &
                  MAX_RESOURCE_ID_NO=9999)
! 05/07/03. TECHNOLOGY SCARCITY
!
      LOGICAL(kind=1) :: YES_TECHNOLOGY_SCARCITY, &
                USE_TECH_SCAR_THIS_MONTH, &
                TECH_PERCENT_TRANS_CAP, &
                YES_TECH_PERCENT_TRANS_CAP, &
                MARGINAL_UNIT_BY_TECH_ACTIVE, &
                YES_MARGINAL_UNIT_ACTIVE, &
                WRITE_MARGINAL_UNIT_REPORT, &
                MARGIN_UNIT_REPORT_NOT_OPEN=.TRUE., &
                UU_REPORT_NOT_OPEN=.TRUE., &
                UK_REPORT_NOT_OPEN=.TRUE., &
                UV_REPORT_NOT_OPEN=.TRUE., &
                REPORT_THIS_CL_UNIT
      INTEGER(kind=4) :: MARGINAL_UNIT_REC=0,UU_DAILY_REC=0 &
                ,UV_HOURLY_REC=0,UK_DAILY_REC=0
      INTEGER(kind=2) :: TECH_POINTS=5, &
                TECH_BREAK_INTERVAL(20), &
                TECH_BREAK_COUNT, &
                UU_REPORT_VARIABLES, &
                UK_REPORT_VARIABLES, &
                UV_REPORT_VARIABLES, &
                UU_DAILY_NO, &
                UK_DAILY_NO, &
                UV_HOURLY_NO, &
                UU_DAILY_HEADER, &
                UK_DAILY_HEADER, &
                UV_HOURLY_HEADER
      REAL(kind=4) :: TECH_WEIGHTED_PERCENT(5),TECH_BREAK_TEST, &
                TECH_BREAK_MW_PERCENT_CHANGE, &
                TECH_BREAK_MW(20), &
                TECH_BREAK_STRENGTH(20), &
                TECH_PERCENT_INPUT, &
                GET_TECH_PERCENT_INPUT, &
                GET_TECH_POWER, &
                TECHNOLOGY_SCARCITY_POWER, &
                UNIT_ENERGY,UNIT_HEAT, &
                UNIT_CAPACITY, &
                RSP,RPM,RTG,RFT, &
                AHR,ONLINE_DATE, &
                HESI_ID_NUM_4_UNIT, &
                HESI_SECOND_UNIT_ID_NUM
!
      INTEGER(kind=2) :: I,J,BLOCK,NBLOK2, &
               Z,Z_C,UNIT(*),INTERVALS,BlkDbDx, &
               U,B,BLKNO(*),NUNITS,K,PERIOD_COUNTER,NUNITS_SAVE, &
               LAST_INTERVAL,HOURLY_LOAD_OUT,LAST_DATA_INTERVAL=0, &
               get_TRANSACTION_GROUP, &
               GET_THERMAL_ONLINE
       integer (kind=2) :: imm_U
!
!
!

      LOGICAL(kind=1) :: FIRST_OUTAGE_HOUR_IN_MONTH=.FALSE.
      REAL :: & !  :
               ADDITIONAL_CAPACITY_VALUE(7), &
               ADDITIONAL_CAPACITY_PERCENT(7), &
               TRANSACT_ABUNDANCE_COST, &
               GET_TRANSACT_ABUNDANCE_COST
      REAL(kind=8) :: & !  these were REAL before 20030429
               GET_MUST_RUN_CAPACITY
!
      REAL :: &
               DX, &
               P,Q,MWBLOK(*),MARGINAL_COST(0:1000), &
               MARGINAL_PROB(0:1000), &
               MARGINAL_PROB_FOR_SEG,TOTAL_PROB_FOR_SEG, &
               MAINTENANCE_RATE(*), &
               AVE_COST(1000),TOTAL_COST,MAX_POINTS,ADJ_TOTAL_CAP, &
               LAST_BLOCK_COST,BASE,PEAK,MAX_ARRAY_SIZE, &
               TEMP_MARGIN_COST, &
               TEMP_CONTRIBUTION, &
               OLD_FIRST_ECONOMY_COST, &
!     +         TRANS_GROUP_CAP &
               LAST_EXPECTED_MARGINAL_COST, &
               UNIT_DEVIATION,REMAINING_ENERGY,TEMP_ENERGY, &
               TEMP_HEAT, &
               TEMP_R,TEMP_R2,A_CO,B_CO,C_CO, &
               HOURS_MULT, &
               GET_HEAT_RATE_FACTOR, &
               R_CAL_MAR_HOURLY_ENERGY, &
               CAL_MAR_HOURLY_WHOLESALE
      INTEGER :: DATABASE_COUNTER,DATABASE
!
      REAL(kind=8) :: CAL_MAR_MONTHLY_ENERGY_routine, &
               TRANS_GROUP_CAP, &
               GET_PRODUCTION_LEVEL_AT_MARKET

      LOGICAL(kind=1) :: POINT_NOT_FOUND
      INTEGER(kind=2) :: NEW_POINT,NPC,R_I,R_NEW_POINT, &
                  SAVE_SEASON, &
                  R_TRANS_GROUP,CURRENT_GUESS,LAST_BLOCK, &
       LAST_PROB_BLOCK,start_val,end_val,BLOCK_POSITION,SAVE_YEAR, &
                  SET_TRANS_FOR_DATA_BASE, &
                  MPOS, &
                  divisor
!
      REAL(kind=4) :: REMAIN, &  ! DIVISOR,
               R_MARGIN_COST,SYS_MARKET_PRICE, &
               R_LOAD,R_MARKET_PRICE, &
               GET_TOTAL_INCREMENTAL_COST, &
               CAPACITY_ASSIGNED
!
! MARGINAL REPORT VARIABLES
!
      LOGICAL(kind=1) :: MARGINAL_REPORT_NOT_OPEN=.TRUE., &
                  TECH_SCAR_REPORT_NOT_OPEN=.TRUE., &
                  TECH_SCAR_REPORT,YES_TECH_SCAR_REPORT
      INTEGER(kind=2) :: MARGINAL_NO,MARGINAL_COST_HEADER, &
                  TECH_SCAR_NO,TECH_SCAR_HEADER, &

                  NBLOK2_SAVE,SYS_TRANS_GROUP
      INTEGER :: MARGINAL_REC,TECH_SCAR_REC
      INTEGER(kind=4) :: DATA_BASIS
      CHARACTER(len=1) :: LOGIC_TYPE
      CHARACTER(len=1) :: TRANSACTION_PERIOD,TRANS_TIME_FRAME
      LOGICAL(kind=1) :: GET_START_UP_LOGIC
      CHARACTER(len=6) :: BEFORE_OR_AFTER_SALES
      CHARACTER(len=9) :: &
            CL_MONTH_NAME(13)=(/'January  ','February ' &
            ,'March    ','April    ','May      ', &
            'June     ','July     ','August   ', &
            'September','October  ','November ', &
            'December ','Annual   '/)
! ECITY
      CHARACTER(len=5) :: CL_UNIQUE_RPT_STR
      CHARACTER(len=20) :: SPECIAL_ID_NAME
      CHARACTER(len=25) :: CL_TRANS_NAME
      CHARACTER(len=14) :: PRIME_MOVER_NAME(12)= &
        (/'Combined Cycle','Fuel Cell     ','Geothermal    ', &
        'GT Gas Turbine','Hydro         ','IC Gas Turbine', &
        'Unkown        ','Nuclear       ','Solar         ', &
        'Steam         ','Wind          ','NUG           '/)
      CHARACTER(len=35) :: LOCAL_GROUP_NAME, &
                   GET_GROUP_NAME,R_TRANS_NAME
      INTEGER(kind=2) :: RESERVE_CAP_NO(2)=(/0,0/), &

                SUPPLEMENTAL_CAP_NO(2)=(/0,0/),GET_DUKE_RESERVE_CAP_NO
      REAL(kind=4) :: RESERVE_CAPACITY(2)=(/0.,0./), &
             GET_DUKE_RESERVE_CAPACITY,GET_CPL_RESERVE_CAPACITY, &
             SUPPLEMENTAL_CAPACITY(2), &
             GET_DUKE_SUP_CAPACITY, &
             GET_CPL_SUP_CAPACITY, &
             RES_N_SUP_CAPACITY(2), &
             RETAINED_AVAILABLE(2)=(/0.,0./), &
             DUKE_RETAINED_AVAIL(800), &
             GET_DUKE_RETAINED_CAPACITY, &
             GET_DUKE_HOURLY_RETAINED, &
             GET_DUKE_BEFORE_RETAINED, &
             BEFORE_RETAINED=0, &
             GET_MONTHLY_DUKE_RESERVE_ENERGY
!
!
! MULTIPLE DATA BASES
!
      INTEGER(kind=2) :: CUM_HOURS, &
                  SAVE_HOURS_IN_MONTH=0,GET_DATA_BASE_FOR_UNIT, &
                  LOCAL_HOURS_IN_MONTH=0,LAST_HOUR_IN_MONTH
!
! HOURLY MARGINAL COSTING FOR ARTMAN AND OTHERS. GAT. 10/17/95.
!
      REAL :: R_DX,R_MARGINAL_COST(0:1000), &
            ENERGY(2,MAX_CL_UNITS),LEFT_HEAT(MAX_CL_UNITS), &
            DAILY_PM_TG_ENERGY(16,256), &
            DAILY_PM_TG_CAPACITY(16,256)
            
      real :: HOURLY_ENERGY(24,MAX_CL_UNITS)
      
      real :: DAILY_ENERGY(2,MAX_CL_UNITS)
      
      real :: DAILY_HEAT(2,MAX_CL_UNITS)
      
      real :: DAILY_PM_TG_HEAT(16,256), &
            MONTH_HEAT(2,MAX_CL_UNITS), &
            MONTH_LEFT_HEAT,MONTH_RIGHT_HEAT, &
            RIGHT_HEAT(MAX_CL_UNITS), &
            PERCENT_HOURS_IN_MONTH(2)
      real :: lastphm=0
      LOGICAL(kind=1) :: LOOKING_AHEAD
!      INTEGER(kind=2   AFTER,U_AFTER,B_AFTER,
!     +            TEMP_U,TEMP_B
      INTEGER(kind=2) :: TEMP_BLOCK
      REAL(kind=8) :: & !  REAL before 20030429
            GET_TOP_CAP_SCARCITY_COST, &
            GET_TOP_CAP_MARGINAL_COST
      REAL :: DISPATCHING_COST, &
            INTERVAL_CAPACITY, &
            TOP_CAP_PERCENT, &
            GET_TOP_CAP_PRODUCTION_LEVEL,TOP_CAP_MARGIN_COST
      LOGICAL(kind=1) :: WRITE_TO_MC_DATA_BASE, &
                  MARGINAL_DATA_BASE_NOT_OPEN=.TRUE., &
                  TRANS_GROUP_GEN_ACTIVE
      INTEGER(kind=2) :: MARGINAL_DATA_NO=0,MARGINAL_DATA_BASE_HEADER, &
                  COST_POINT, &
                  FIRST_MONTHLY_TRANSACT, &
                  FIRST_LAST_MONTH_OF_TRANSACT, &
                  R_LAST_MONTH

!
! TRANSACT DETAILED REPORT OVERHEAD
!
      LOGICAL(kind=1) :: TRANS_CONTRIB_NOT_OPEN=.TRUE., &
                YES_MARGINAL_UNIT_REPORT=.FALSE.,MARGINAL_UNIT_REPORT, &
                YES_MARGINAL_COST_MODULE, &
                MARGINAL_COST_SWITCH
      INTEGER(kind=2) :: TRANS_CONTRIBUTION_RPT_HEADER, &
                  TRANS_CON_UNIT,R_HOUR,R_MONTH,LOCAL_HOUR=0
      INTEGER :: TRANS_CON_REC,MARGINAL_DATA_REC
      INTEGER :: BLOK_ARRAY_ALLOCATOR=0
      INTEGER :: MAX_COST_DATABASES=0
!

!
      INTEGER(kind=2) :: DAY_TYPE_COUNTER

      REAL(kind=4) :: WRITE_CONTRIBUTION_REPORT, &
               R_HOURLY_MARGINAL_FUEL_PRICE
      INTEGER(kind=4) :: values_2_zero, &
                DEPTH_MARKET_REC=0, &
                TRANS_DEPTH_MARKET_REC=0
      LOGICAL(kind=1) :: DEALLO_MONTH_MARG_COST_ARRAYS, &
                R_DEALLO_ANN_MARG_COSTS_ARRAYS, &
                HUGE_MODEL_RUN=.FALSE.
!
      INTEGER(kind=2) :: SAVE_UPPER_TRANS_GROUP, & !  =0,
                CALANDER_DAY_OF_WEEK,INT_DAY,INT_HOUR_IN_DAY, &
                HOUR_IN_DAY, &
                GET_TRANS_GROUP_INDEX,GET_DATA_BASE_FOR_TRANS, &
                GET_DAY_TYPE
!
! DEPTH OF MARKET REPORTING. 02/22/01.
!
      LOGICAL(kind=1) :: DEPTH_OF_MARKET, &
                  YES_DEPTH_OF_MARKET=.FALSE., &
                  DEPTH_MARKET_REPORT_NOT_OPEN=.TRUE., &
                  TRANS_DEPTH_REPORT_NOT_OPEN=.TRUE.
      INTEGER(kind=2) :: DEPTH_MARKET_INTERVALS=3, &
                  DEPTH_MARKET_NUM, &
                  DEPTH_MARKET_UNIT=0, &
                  DEPTH_MARKET_RPT_HEADER, &
                  TRANS_DEPTH_MARKET_UNIT=0, &
                  TRANS_DEPTH_MARKET_RPT_HEADER, &
                  GET_NUM_CREATE_HOURLY_PRICE, &
                  GET_CREATE_HOURLY_PRICE_POS, &
                  GET_CREATE_HOURLY_PRICE_INDEX, &
                  NUM_CREATE_HOURLY_PRICE
      REAL(kind=4) :: DEPTH_MARKET_INTERVAL_SIZE=0
!
      LOGICAL(kind=1) :: WRITE_DEPTH_OF_MARKET_REPORT, &
                SAVE_DEPTH_OF_MARKET_DATA, &
                SAVE_TRANS_DEPTH_DATA, &
                GET_CREATE_HOURLY_PRICE, &
                TEMP_L, &
                GET_TF_GROUP_NAME
      REAL(kind=4) :: MINIMUM_INTERVAL,LOCAL_INTERVAL,DEPTH_AFTER_LOAD, &
               D_SCARCITY_COST
      CHARACTER(len=6) :: LOCAL_INTERVAL_CHR
      INTEGER(kind=2) :: D_HOUR,D_DATA_BASES,D_L_M,R_DAY,HR
      INTEGER ALLOCATED_DATA_BASES
      INTEGER(kind=2) :: MAX_BLOCK_GUESS=4000
      integer :: max_firstelem, max_secondelem
      
      LOGICAL(kind=1) :: LAHEY_LF95,TRANSACT_ANALYST_ONLY, &
                INIT_FILE_IS_ACTIVE,FIRST_PASS_MAX_BLOCK_GUESS=.FALSE.

      HESI_SECOND_UNIT_ID_NUM=0

      IF(.NOT. LAHEY_LF95()) &
            CALL MG_LOCATE_WRITE(15,9,'Transaction Allocation', &
                                                         ALL_VERSIONS,0)
      IF(globecom_YEAR == 1 .AND. ISEAS == 1) THEN
         BLOK_ARRAY_ALLOCATOR = 0
         MAX_COST_DATABASES = 0
      ENDIF

      PRICE_MODE = YES_STRICT_MARKET_PRICE() .OR. &
                                  YES_MULTI_AREA_PRICE()
!
      YES_DAILY_THERMAL_RPT_ACTIVE = &
               DAILY_THERMAL_REPORT_ACTIVE(SAVE_START_RPT_DATE, &
                                           SAVE_END_RPT_DATE)
      REPORT_DAILY_PRIME_MOVER = YES_REPORT_DAILY_PRIME_MOVER()
      REPORT_DAILY_THERMAL = YES_REPORT_DAILY_THERMAL()
!
      DAILY_ENERGY = 0.0
      call write_daily_energy_trace("margnobj:A002", daily_energy(1,1))
      HOURLY_ENERGY = 0.0
      DAILY_HEAT = 0.0
      DAILY_PM_TG_ENERGY = 0.0
      DAILY_PM_TG_CAPACITY = 0.0
      DAILY_PM_TG_HEAT = 0.0
      MONTH_HEAT = 0.0
      TEMP_ENERGY = 0.0
!
      SCHEDULE_FOR = YES_DETAILED_FOR()
      SCHEDULE_MOR = YES_DETAILED_MAINTENANCE()
!
      SAVE_UPPER_TRANS_GROUP = GET_NUMBER_OF_ACTIVE_GROUPS()
      MAX_TRANS_GROUP_NUMBER = GET_MAX_TRANS_GROUP_NUMBER()
!
      TRANSACT_ABUNDANCE_COST = GET_TRANSACT_ABUNDANCE_COST()
      UNIT_COMMITMENT_LOGIC = YES_UNIT_COMMITMENT_LOGIC()
!
      C_M = TRANSACTION_MODE()
      YES_TECHNOLOGY_SCARCITY =  C_M > 3
      IF(YES_TECHNOLOGY_SCARCITY) THEN
         USE_TECH_SCAR_THIS_MONTH = .TRUE.
         TEMP_I2 = GET_TECH_SCAR_MONTH_VECTOR()
         IF(TEMP_I2 > 0) THEN
            USE_TECH_SCAR_THIS_MONTH = &
                  GET_VAR(FLOAT(TEMP_I2),ISEAS, &
                                            'TECH_SCAR_THIS_MONTH') > 0.
         ENDIF
         TECH_PERCENT_TRANS_CAP = YES_TECH_PERCENT_TRANS_CAP()
         TECH_PERCENT_INPUT = GET_TECH_PERCENT_INPUT()
         TECHNOLOGY_SCARCITY_POWER = GET_TECH_POWER()
      ENDIF
!
      ECITY = .FALSE.
!
      MONTHLY_SALE = 0.
!
      PRICE_ONLY_WHOLESALE_REV = APPLY_TRANS_REV_TO_WHOLESALE()
!
      MAX_DATA_BASES = SET_TRANS_FOR_DATA_BASE()
      if(MAX_DATA_BASES<0) then
        stop "MAX_DATA_BASES"
      end if
      

!
      TECH_SCAR_REPORT = YES_TECH_SCAR_REPORT()
!
      IF(NBLOK2 <= 0) THEN
         WRITE(4,*) "ATTEMPTING TO RUN TRANSACT WITHOUT ANY"
         WRITE(4,*) "ACTIVE DISPATCHABLE RESOURCES. SEE THE"
         WRITE(4,*) "CAPACITY LIMITED DATA FILE."
         WRITE(4,*) '*** line 646 MARGNOBJ.FOR ***'
         WRITE(4,*) "NBLOK2=",NBLOK2

      ENDIF

!
         FIRST_MONTHLY_TRANSACT = &
                              FIRST_LAST_MONTH_OF_TRANSACT(R_LAST_MONTH)
! 11/19/04
         IF(.NOT. FIRST_PASS_MAX_BLOCK_GUESS) THEN
            FIRST_PASS_MAX_BLOCK_GUESS = .TRUE.
            MAX_BLOCK_GUESS = MIN(MAX_DISPATCH_BLOCKS, &
                     SAVE_UPPER_TRANS_GROUP*6*(AVAIL_DATA_YEARS-globecom_YEAR)+ &
                                                                 NBLOK2)
         ENDIF

         IF(ISEAS == FIRST_MONTHLY_TRANSACT .OR. &
                                     BLOK_ARRAY_ALLOCATOR < NBLOK2) THEN

            BLOK_ARRAY_ALLOCATOR = &
                             MAX(BLOK_ARRAY_ALLOCATOR,INT(NBLOK2),4000)

            MAX_COST_DATABASES = INT(2 * &
                              MIN(MAX_DAY_TYPES,GET_LAST_DAY_TYPE())) * &
                                              INT(BLOK_ARRAY_ALLOCATOR)

!

            IF(.NOT. LAHEY_LF95()) THEN
               WRITE(SCREEN_MESSAGES,"(I4)") NBLOK2
               IF(TRANSACT_ANALYST_ONLY() .AND. &
                                       .NOT. INIT_FILE_IS_ACTIVE()) THEN
                  CALL MG_LOCATE_WRITE(16,70,trim(SCREEN_MESSAGES), &
                                                         ALL_VERSIONS,0)
               ELSE
                  CALL MG_LOCATE_WRITE(20,64,trim(SCREEN_MESSAGES), &
                                                         ALL_VERSIONS,0)
               ENDIF
            ENDIF

               ! INSIDE CAL_MARGINAL_COST
               VOID_LOGICAL = DEALLO_ANNUAL_MARG_COSTS_ARRAYS()
               ALLOCATE(CUM_UNIT_UTIL(TRANS_GROUP_POINTS, &
                                                  0:MAX_COST_DATABASES))
               ALLOCATE(CURRENT_UNIT_UTIL(TRANS_GROUP_POINTS, &
                                                  0:MAX_COST_DATABASES))
               ALLOCATE(TOT_EMBED_COST_BY_POINT(TRANS_GROUP_POINTS, &
                                                        MAX_DATA_BASES))
               ALLOCATE(CUM_UNIT_UTIL_zero(TRANS_GROUP_POINTS, &
                                                        MAX_DATA_BASES))
               ALLOCATE(CURRENT_UNIT_UTIL_zero(TRANS_GROUP_POINTS, &
                                                        MAX_DATA_BASES))
               ALLOCATE(BlockIncrCost(BLOK_ARRAY_ALLOCATOR))
               ALLOCATE(COST_CURVE_POINTER_BY(0:BLOK_ARRAY_ALLOCATOR, &
                                                        MAX_DATA_BASES))

               ALLOCATE(OUTAGE_BLOCKS_4_DB(MAX_DATA_BASES), stat=allocstat)
               call check_alloc("margnobj:0021", "OUTAGE_BLOCKS_4_DB", allocstat)

               
               allocate(OUTAGE_BLOCK_N_DB( &
                  0:BLOK_ARRAY_ALLOCATOR,MAX_DATA_BASES),stat=allocstat)
               call check_alloc("margnobj:0022", "OUTAGE_BLOCK_N_DB", allocstat)

               
               allocate(OUTAGE_BLOCK_REVERSE_INDEX(0:BLOK_ARRAY_ALLOCATOR, &
                         MAX_DATA_BASES), stat=allocstat)
               call check_alloc("margnobj:0023", "OUTAGE_BLOCK_N_DB", allocstat)

               
!            ENDIF ! ALLOCATE ARRAYS MSG


      ENDIF  ! 8/13/01
!
! OVERHEAD FOR DEPTH OF MARKET REPORT
!
      IF(ISEAS == FIRST_MONTHLY_TRANSACT) THEN
         YES_DEPTH_OF_MARKET = &
                         DEPTH_OF_MARKET(DEPTH_MARKET_INTERVALS, &
                                             DEPTH_MARKET_INTERVAL_SIZE, &
                                             DEPTH_TARGET_GROUP)
         IF(YES_DEPTH_OF_MARKET) THEN
           DEPTH_MARKET_INTERVALS = MAX(INT(3,2),DEPTH_MARKET_INTERVALS)
           DEPTH_MARKET_INTERVAL_SIZE = MAX(1., &
                                             DEPTH_MARKET_INTERVAL_SIZE)
            IF(ALLOCATED(DEPTH_PRICE)) DEALLOCATE(DEPTH_PRICE)
            ALLOCATE(DEPTH_PRICE(DEPTH_MARKET_INTERVALS,24))
            DEPTH_PRICE = 0.
            IF(DEPTH_MARKET_REPORT_NOT_OPEN) THEN
               DEPTH_MARKET_NUM = 24 ! ?
               DEPTH_MARKET_UNIT = &
                               DEPTH_MARKET_RPT_HEADER(DEPTH_MARKET_NUM, &
                                                       DEPTH_MARKET_REC)
               DEPTH_MARKET_REPORT_NOT_OPEN = .FALSE.
            ENDIF
            NUM_CREATE_HOURLY_PRICE = GET_NUM_CREATE_HOURLY_PRICE()
            IF(NUM_CREATE_HOURLY_PRICE > 0) THEN
               IF(ALLOCATED(TRANS_DEPTH_PRICE)) &
                                           DEALLOCATE(TRANS_DEPTH_PRICE)
               ALLOCATE(TRANS_DEPTH_PRICE( &
                              NUM_CREATE_HOURLY_PRICE, &
                              DEPTH_MARKET_INTERVALS, &
                              24))
               TRANS_DEPTH_PRICE = 0.
               IF(TRANS_DEPTH_REPORT_NOT_OPEN) THEN
                  DEPTH_MARKET_NUM = 24 ! ?
                  TRANS_DEPTH_MARKET_UNIT = &
                     TRANS_DEPTH_MARKET_RPT_HEADER(DEPTH_MARKET_NUM, &
                                                 TRANS_DEPTH_MARKET_REC)
                  TRANS_DEPTH_REPORT_NOT_OPEN = .FALSE.
               ENDIF
            ENDIF ! NUM_CREATE_HOURLY_PRICE > 0
         ENDIF
      ENDIF
! END DEPTH OF MARKET REPORT OVERHEAD
      DATA_BASIS = INT(TRANS_GROUP_POINTS)*INT(MAX_DATA_BASES)
      TOT_EMBED_COST_BY_POINT = 0.
      CUM_UNIT_UTIL_zero = 0.
      CURRENT_UNIT_UTIL_zero = 0.
      CUM_UNIT_UTIL = 0.
      COST_CURVE_POINTER_BY = 0
!      TRANS_BLOCK_CAPACITY = 0.
      BlockIncrCost = 0.
!      OUTAGE_UNIT_UTIL = 0.
      OUTAGE_BLOCKS_4_DB = 0
      values_2_zero = INT(MAX_DATA_BASES*(BLOK_ARRAY_ALLOCATOR+1))
!      OUTAGE_BLOCK_N_DB = 0.
      OUTAGE_BLOCK_N_DB = 0

      OUTAGE_BLOCK_REVERSE_INDEX = 0.
      UNIT_TO_BLOCK = 0
         ALLOCATE ( &
               NEW_BLOCK_NUMBER(TRANS_GROUP_POINTS,CONTRIB_INTERVALS, &
                            MAX_DATA_BASES), stat=allocstat)
         call check_alloc("margnobj:0024", "NEW_BLOCK_NUMBER", allocstat)
         
         ALLOCATE(NEW_MARGINAL_DATA_BASE(TRANS_GROUP_POINTS, &
                                      CONTRIB_INTERVALS,MAX_DATA_BASES))
         call check_alloc("margnobj:0025", "NEW_MARGINAL_DATA_BASE", allocstat)
         
         ALLOCATE(LODDUR_FOR_MC(0:1000,MAX_DATA_BASES))
         call check_alloc("margnobj:0026", "LODDUR_FOR_MC", allocstat)
         ALLOCATE(LPROB_FOR_MC(0:1000,2,MAX_DATA_BASES))
         call check_alloc("margnobj:0027", "LPROB_FOR_MC", allocstat)
         ALLOCATE(ACTIVE_UNIT(NUNITS))
         call check_alloc("margnobj:0028", "ACTIVE_UNIT", allocstat)
         
         ALLOCATE(START_UP_POSITION(NUNITS,SAVE_UPPER_TRANS_GROUP))
         call check_alloc("margnobj:0029", "START_UP_POSITION", allocstat)
         ALLOCATE(SCARCITY_MULT(SAVE_UPPER_TRANS_GROUP,2))
         call check_alloc("margnobj:0030", "SCARCITY_MULT", allocstat)
         ALLOCATE(SALES_B4_AFTER(0:TRANS_GROUP_POINTS,2,MAX_DATA_BASES))
         call check_alloc("margnobj:0031", "SALES_B4_AFTER", allocstat)
         
         ALLOCATE(WHOLESALE_REV_AND_EXP(TRANS_GROUP_POINTS,2, &
                                                        MAX_DATA_BASES))
        call check_alloc("margnobj:0032", "WHOLESALE_REV_AND_EXP", allocstat)
         ALLOCATE(WHOLESALE_SAL_AND_PUR(TRANS_GROUP_POINTS,2, &
                                                        MAX_DATA_BASES))
                                                        
         call check_alloc("margnobj:0033", "WHOLESALE_SAL_AND_PUR", allocstat)
         ALLOCATE(RETAIL_REV_AND_EXP(TRANS_GROUP_POINTS,2, &
                                                        MAX_DATA_BASES))
         call check_alloc("margnobj:0034", "RETAIL_REV_AND_EXP", allocstat)
                                                        
         ALLOCATE(RETAIL_SAL_AND_PUR(TRANS_GROUP_POINTS,2, &
                                                        MAX_DATA_BASES))
         call check_alloc("margnobj:0035", "RETAIL_SAL_AND_PUR", allocstat)
                                                        
         ALLOCATE(NEW_POINT_COUNT(TRANS_GROUP_POINTS,MAX_DATA_BASES))
         call check_alloc("margnobj:0036", "NEW_POINT_COUNT", allocstat)
         
         
         ALLOCATE(NEW_LODDUR(TRANS_GROUP_POINTS,MAX_DATA_BASES))
         call check_alloc("margnobj:0037", "NEW_LODDUR", allocstat)
         
         ALLOCATE(EXPECTED_MARGINAL_COST(TRANS_GROUP_POINTS, &
                                                        MAX_DATA_BASES))
         ALLOCATE(TRANSITION_MARGINAL_COST(TRANS_GROUP_POINTS, &
                                               MAX_DATA_BASES,PRODUCTS))
         ALLOCATE(CAPACITY_GIVEN_MARKET(TRANS_GROUP_POINTS, &
                                                        MAX_DATA_BASES))
         ALLOCATE(BLOCK_B4_MARKET(TRANS_GROUP_POINTS,MAX_DATA_BASES))
         ALLOCATE(REMAINING_CONTRIBUTION_MW(TRANS_GROUP_POINTS, &
                                                        MAX_DATA_BASES))
         ALLOCATE( &
               CUM_INTERVAL_CAPACITY(TRANS_GROUP_POINTS,MAX_DATA_BASES))
         ALLOCATE(CUM_CAP(MAX_DATA_BASES))
         ALLOCATE(LAST_DB_POINT(MAX_DATA_BASES), stat=allocstat)
         call check_alloc("margnobj:0019", "LAST_DB_POINT", allocstat)
         ALLOCATE(CumDbDx(MAX_DATA_BASES))
         ALLOCATE(DB_ADJ_TOTAL_CAP(MAX_DATA_BASES))
         ALLOCATE(DB_CAP_AFTER_OUTAGES(MAX_DATA_BASES))
         ALLOCATE(DB_DX(MAX_DATA_BASES))
         ALLOCATE(PREVIOUS_INTERVAL(MAX_DATA_BASES))
         ALLOCATE(I_CORRECT(MAX_DATA_BASES))
         ALLOCATE(ACTIVE_DATA_BASE(MAX_DATA_BASES))
         ALLOCATE(DETAILED_OUTAGE_DATA_BASE(MAX_DATA_BASES))
         ALLOCATE(CURRENT(MAX_DATA_BASES))
         ALLOCATE(NEXT(MAX_DATA_BASES))
         ALLOCATE(POS(MAX_DATA_BASES))
         ALLOCATE(NATIVE_POS(MAX_DATA_BASES))
         ALLOCATE(CUM_COST(MAX_DATA_BASES))
         ALLOCATE(CUM_PROB(MAX_DATA_BASES))
         ALLOCATE(SAVE_LOAD(MAX_DATA_BASES))
         ALLOCATE(NATIVE_POS_PERCENT(MAX_DATA_BASES))
         ALLOCATE(CUM_SALES_AFTER(MAX_DATA_BASES))
         ALLOCATE(CUM_MUST_RUN_MW(MAX_DATA_BASES))
         ALLOCATE(CUM_MUST_RUN_POSITION(MAX_DATA_BASES))
         ALLOCATE(UNIT_ENERGY_B4_AFTER(BLOK_ARRAY_ALLOCATOR,2, &
                                                        MAX_DATA_BASES))

      YES_MARGINAL_UNIT_REPORT = MARGINAL_UNIT_REPORT()
      IF(TRANS_CONTRIB_NOT_OPEN .AND. &
                             YES_MARGINAL_UNIT_REPORT .AND. &
                                               .NOT. TESTING_PLAN ) THEN
         TRANS_CONTRIB_NOT_OPEN = .FALSE.
         TRANS_CON_UNIT = TRANS_CONTRIBUTION_RPT_HEADER(TRANS_CON_REC)
      ENDIF



      SAVE_YEAR = globecom_year + get_BASE_YEAR()
      SAVE_SEASON = PERIOD_COUNTER

! USED TO VERIFY GENERATION IN cal_mar_monthly_energy_routine

      SUM_AVERAGE_COST_SALES = 0.0
      SUM_PRODUCTION_SALES = 0.0
      RESERVE_CAPACITY(1) = 0.
      RESERVE_CAPACITY(2) = 0.
      RETAINED_AVAILABLE(1) = 0.
      RETAINED_AVAILABLE(2) = 0.
      SUPPLEMENTAL_CAPACITY(1) = 0.
      SUPPLEMENTAL_CAPACITY(2) = 0.
      RESERVE_CAP_NO(1) = 0
      RESERVE_CAP_NO(2) = 0
      SUPPLEMENTAL_CAP_NO(1) = 0
      SUPPLEMENTAL_CAP_NO(2) = 0
      RES_N_SUP_CAPACITY(1) = 0.
      RES_N_SUP_CAPACITY(2) = 0.

      DUKE_RETAINED_AVAIL = 0.

      ALLOCATE(UNIT_SAVE(NBLOK2),BLKNO_SAVE(NBLOK2)) ! 11/28/00.

      UNIT_SAVE(1:NBLOK2) = UNIT(1:NBLOK2)
      BLKNO_SAVE = 0
! 051608. WORK AROUND
      DO U = 1, NUNITS
          imm_u=U
          SAVE_PRIMARY_MOVER(U) =  GET_PRIMARY_MOVER(imm_u)
      END DO

      CALL GET_CUM_HOURS_N_HOURS_IN_MONTH(SAVE_SEASON, &
                                          SAVE_HOURS_IN_MONTH,CUM_HOURS)

      TRANSACTION_PERIOD = TRANS_TIME_FRAME()

      IF(TRANSACTION_PERIOD == 'S') THEN
         LOCAL_HOURS_IN_MONTH = SAVE_HOURS_IN_MONTH
      call check_hours_in_month(local_hours_in_month, "marnobj:0001")

      ELSEIF(TRANSACTION_PERIOD == 'D') THEN
         LOCAL_HOURS_IN_MONTH = 24
      ELSEIF(TRANSACTION_PERIOD == 'W') THEN
         LOCAL_HOURS_IN_MONTH = 168
      ELSE


         LOCAL_HOURS_IN_MONTH = SAVE_HOURS_IN_MONTH
         call check_hours_in_month(local_hours_in_month, "margnobj:0004")

      ENDIF


      NBLOK2_SAVE = NBLOK2
      NUNITS_SAVE = NUNITS

! MARGINAL REPORT OVERHEAD

      YES_MARGINAL_COST_MODULE = MARGINAL_COST_SWITCH()
      WRITE_TO_MC_DATA_BASE = YES_MARGINAL_COST_MODULE .AND. &
                                                   HOURLY_LOAD_OUT() > 0
      IF(YES_MARGINAL_COST_MODULE .AND. MARGINAL_REPORT_NOT_OPEN) THEN
         MARGINAL_NO = MARGINAL_COST_HEADER(MARGINAL_REC)
         MARGINAL_REPORT_NOT_OPEN = .FALSE.

      ENDIF

      ALLOCATE(UNIT_DISP_COST(NUNITS,2),BLOCK_DISP_COST(NBLOK2))
      CALL GET_BLOCK_DISP_COST(UNIT,BLKNO,UNIT_DISP_COST,NBLOK2)
      CALL GET_ADJ_TOTAL_CAP(ADJ_TOTAL_CAP)

      NUM_DETAILED_OUTAGE_DATABASES = 0

      ALLOCATE(SAVE_TRANS_FOR_DATA_BASE(MAX_DATA_BASES))

      DO DATA_BASES = 1, MAX_DATA_BASES
         DB_ADJ_TOTAL_CAP(DATA_BASES) = 0.
         DB_CAP_AFTER_OUTAGES(DATA_BASES) = 0.

         ACTIVE_DATA_BASE(DATA_BASES) = .FALSE.

         SAVE_TRANS_FOR_DATA_BASE(DATA_BASES) = &
                                     GET_TRANS_FOR_DATA_BASE(DATA_BASES)
         TG = SAVE_TRANS_FOR_DATA_BASE(DATA_BASES)
         IF(TG > 0) THEN

            TG = GET_TRANS_GROUP_POSITION(TG)

            IF(TG > 0) THEN
!               IF(OUTAGES_IN_MONTH_BY_TG(TG)  > 0 .OR. 1 == 1 .OR.
               IF(OUTAGES_IN_MONTH_BY_TG(TG)  > 0 .OR. &
                                             UNIT_COMMITMENT_LOGIC) THEN
                  DETAILED_OUTAGE_DATA_BASE(DATA_BASES) = .TRUE.
                  NUM_DETAILED_OUTAGE_DATABASES = &
                                       NUM_DETAILED_OUTAGE_DATABASES + 1
               ELSE
                  DETAILED_OUTAGE_DATA_BASE(DATA_BASES) = .FALSE.
               ENDIF
            ELSE
               DETAILED_OUTAGE_DATA_BASE(DATA_BASES) = .FALSE.
            ENDIF
         ELSE
            DETAILED_OUTAGE_DATA_BASE(DATA_BASES) = .FALSE.
         ENDIF

      ENDDO

! NOTE CHANGE IN DATA_BASIS FOR ALLOCATE VS CINITW
      DATA_BASIS = MAX(INT(BLOK_ARRAY_ALLOCATOR), &
                           INT(BLOK_ARRAY_ALLOCATOR)* &
                                    INT(NUM_DETAILED_OUTAGE_DATABASES))
      ALLOCATE(OUTAGE_UNIT_INDEX(0:DATA_BASIS))
      ALLOCATE(OUTAGE_BLOCK_INDEX(0:DATA_BASIS), stat=allocstat)
      call check_alloc("margnobj:0020", "OUTAGE_BLOCK_INDEX", allocstat)
      ALLOCATE(TRANS_BLOCK_CAPACITY(0:DATA_BASIS))
      ALLOCATE(CUM_TC_UNIT_MWH(NUNITS))
      OUTAGE_UNIT_INDEX = 0
      OUTAGE_BLOCK_INDEX = 0
      TRANS_BLOCK_CAPACITY = 0
      CUM_TC_UNIT_MWH = 0.
      ALLOCATED_DATA_BASES = DATA_BASIS

      FIRST_OUTAGE_HOUR_IN_MONTH = .TRUE.

      MAX_POINTS = 800.
      MAX_ARRAY_SIZE = 1000.
!      MAX_INTERVALS = NINT(MAX_ARRAY_SIZE)
      DX = (ADJ_TOTAL_CAP-1.)/(MAX_POINTS+1.)


!      DATA_BASES = 0.


! USED FOR MONTHLY ENERGY AND TOTAL COST ROUTINE

      SALES_B4_AFTER = 0.
      WHOLESALE_REV_AND_EXP = 0.
      WHOLESALE_SAL_AND_PUR = 0.
      RETAIL_REV_AND_EXP = 0.
      RETAIL_SAL_AND_PUR = 0.
      UNIT_ENERGY_B4_AFTER = 0.
      CUM_INTERVAL_CAPACITY = 0.

      ALLOCATE(FIRST_ECONOMY_COST(MAX_DATA_BASES))
      FIRST_ECONOMY_COST = 0.

      ALLOCATE(MUST_RUN_COST(MAX_DATA_BASES))
      MUST_RUN_COST = TRANSACT_ABUNDANCE_COST

      ALLOCATE(MW_AFTER_OUTAGES(NBLOK2,MAX_DATA_BASES))

      MW_AFTER_OUTAGES = 0.

      ALLOCATE(MW_DIFF(0:NBLOK2,MAX_DATA_BASES))

      MW_DIFF = 0.

      DRATE_TRANSACT = .TRUE.
! HEC
      M1_U = 0
      M2_U = 0
      R1_U = 0
      R2_U = 0
      WI_U = 0
      PE_U = 0
      DG_U = 0
      BE_U = 0
      WO_U = 0

      MEROM1_EFFECTIVE_CAP = 0.
      MEROM2_EFFECTIVE_CAP = 0.
      RATTS1_EFFECTIVE_CAP = 0.
      RATTS2_EFFECTIVE_CAP = 0.

      CAT1_B = 0
      CAT1_U = 0
      CAT2_B = 0
      CAT2_U = 0
      MCG1_B = 0
      MCG1_U = 0
      MCG2_B = 0
      MCG2_U = 0

      I = 1

      DO BLOCK = 1, NBLOK2
! 11/28/00.
         BLKNO_SAVE(BLOCK) = BLKNO(BLOCK)

         U = UNIT(BLOCK)
         B = MAX(INT(1,2),BLKNO(BLOCK))

         BLOCK_DISP_COST(BLOCK) = UNIT_DISP_COST(U,B)/10.

         IF(WABASH_VALLEY) THEN
            IF(WABASH_IM_NIPSCO) THEN
               IF ((U == GIBSON_BACKUP_POINTER .OR. &
                     U == GIBSON_POINTER .OR. &
                         U == PSI_AREA_LAST_RESOURCE_POINTER)) THEN
                  CYCLE
               ENDIF
               IF(U == GIBSON_BACKUP_POINTER) THEN
                  ARRAY_POINTR = &
                      CL_ALLOCATE_POINTR(CL_RESOURCE_ID(GIBSON_POINTER))
                  MWBLOK(BLOCK) = MWBLOK(BLOCK) * &
                    (1. - &
                    AREA_CAPACITY_LOSSES(1,PERIOD_COUNTER,ARRAY_POINTR))
               ELSE
                  ARRAY_POINTR = &
                              CL_ALLOCATE_POINTR(CL_RESOURCE_ID(U))
                  UNITCAP(U) = UNITCAP(U) + MWBLOK(BLOCK)
                  MWBLOK(BLOCK) = MWBLOK(BLOCK) * &
                    (1. - &
                    AREA_CAPACITY_LOSSES(1,PERIOD_COUNTER,ARRAY_POINTR))
               ENDIF
               UNIT_CAP_AFTER_LOSSES(U) = MWBLOK(BLOCK) + &
                                           UNIT_CAP_AFTER_LOSSES(U)
            ELSE
!
               IF(U == GIBSON_BACKUP_POINTER) THEN
                  ARRAY_POINTR = &
                      CL_ALLOCATE_POINTR(CL_RESOURCE_ID(GIBSON_POINTER))
                  MWBLOK(BLOCK) = MWBLOK(BLOCK) * &
                    (1. - &
                    AREA_CAPACITY_LOSSES(1,PERIOD_COUNTER,ARRAY_POINTR))
               ELSE
                  ARRAY_POINTR = &
                              CL_ALLOCATE_POINTR(CL_RESOURCE_ID(U))
                  UNITCAP(U) = UNITCAP(U) + MWBLOK(BLOCK)
                  MWBLOK(BLOCK) = MWBLOK(BLOCK) * &
                    (1. - &
                    AREA_CAPACITY_LOSSES(1,PERIOD_COUNTER,ARRAY_POINTR))
               ENDIF
               UNIT_CAP_AFTER_LOSSES(U) = MWBLOK(BLOCK) + &
                                           UNIT_CAP_AFTER_LOSSES(U)
            ENDIF
         ENDIF ! WABASH VALLEY
!
! ADDED 3/25/00. FOR ELECTRICITIES.
!
         IF(SPECIAL_ID_NAME(U) /= ' ' .AND. &
                                      SPECIAL_ID_NAME(U) /= 'None') THEN
!            ECITY = .TRUE.
            IF(SPECIAL_ID_NAME(U) == 'Reserve-Duke') THEN
               IF(B < 2) THEN
!                  TEMP_TL_PEAK = GET_TRANS_PEAK_AFTER_EL(1)
!
!                  CALL GET_TRANS_PEAK_B4_HYDRO(TEMP_TL_PEAK,1)
!
                  TEMP_TL_PEAK = MONTH_PEAK_BY_CG(INT(1,2))
                  TEMP_TL_PEAK = TEMP_TL_PEAK - SEPA_CAPACITY
!                  RESERVE_BLOCK = BLOCK
               ENDIF
               RESERVE_CAPACITY(1) = RESERVE_CAPACITY(1) + &
                                          MWBLOK(BLOCK)* &
                                                (1.-MAINTENANCE_RATE(U))
               RES_N_SUP_CAPACITY(1) = RES_N_SUP_CAPACITY(1) + &
                                          MWBLOK(BLOCK)* &
                                                (1.-MAINTENANCE_RATE(U))
               IF(RESERVE_CAPACITY(1) > 0.) THEN
                  RESERVE_CAP_NO(1) = U
               ENDIF
               ECITY = .TRUE.
               CYCLE
            ELSEIF(SPECIAL_ID_NAME(U) == 'Reserve-CPL') THEN
               RESERVE_CAPACITY(2) = RESERVE_CAPACITY(2) + &
                                          MWBLOK(BLOCK)* &
                                                (1.-MAINTENANCE_RATE(U))
               RES_N_SUP_CAPACITY(2) = RES_N_SUP_CAPACITY(2) + &
                                          MWBLOK(BLOCK)* &
                                                (1.-MAINTENANCE_RATE(U))
               IF(RESERVE_CAPACITY(2) > 0.) THEN
                  RESERVE_CAP_NO(2) = U
               ENDIF
               ECITY = .TRUE.
               CYCLE
            ELSEIF(SPECIAL_ID_NAME(U) == 'Supplemental-Duke') THEN
               SUPPLEMENTAL_CAPACITY(1) = SUPPLEMENTAL_CAPACITY(1) + &
                                       MWBLOK(BLOCK)* &
                                                (1.-MAINTENANCE_RATE(U))
               RES_N_SUP_CAPACITY(1) = RES_N_SUP_CAPACITY(1) + &
                                       MWBLOK(BLOCK)* &
                                                (1.-MAINTENANCE_RATE(U))
               IF(SUPPLEMENTAL_CAPACITY(1) > 0.) THEN
                  SUPPLEMENTAL_CAP_NO(1) = U
               ENDIF
               ECITY = .TRUE.
               CYCLE
            ELSEIF(SPECIAL_ID_NAME(U) == 'Supplemental-CPL') THEN
               SUPPLEMENTAL_CAPACITY(2) = SUPPLEMENTAL_CAPACITY(2) + &
                                       MWBLOK(BLOCK)* &
                                                (1.-MAINTENANCE_RATE(U))
               RES_N_SUP_CAPACITY(2) = RES_N_SUP_CAPACITY(2) + &
                                       MWBLOK(BLOCK)* &
                                                (1.-MAINTENANCE_RATE(U))
               IF(SUPPLEMENTAL_CAPACITY(2) > 0.) THEN
                  SUPPLEMENTAL_CAP_NO(2) = U
               ENDIF
               ECITY = .TRUE.
               CYCLE
            ELSEIF(SPECIAL_ID_NAME(U) == 'Catawba-Duke' .OR. &
                              SPECIAL_ID_NAME(U) == 'McGuire-Duke') THEN
               RETAINED_AVAILABLE(1) = RETAINED_AVAILABLE(1) + &
                                    MWBLOK(BLOCK)*EAVAIL(U)* &
                                                (1.-MAINTENANCE_RATE(U))
               IF(UNITNM(U) ==     'Catawba 1') THEN
                  CAT1_B = BLOCK
                  CAT1_U = U
               ELSEIF(UNITNM(U) == 'Catawba 2') THEN
                  CAT2_B = BLOCK
                  CAT2_U = U
               ELSEIF(UNITNM(U) == 'McGuire 1') THEN
                  MCG1_B = BLOCK
                  MCG1_U = U
               ELSEIF(UNITNM(U) == 'McGuire 2') THEN
                  MCG2_B = BLOCK
                  MCG2_U = U
               ENDIF
               ECITY = .TRUE.
            ELSEIF(SPECIAL_ID_NAME(U) == 'Bruns1' .OR. &
                   SPECIAL_ID_NAME(U) == 'Bruns2' .OR. &
                   SPECIAL_ID_NAME(U) == 'Harris1' .OR. &
                   SPECIAL_ID_NAME(U) == 'Mayo1' .OR. &
                   SPECIAL_ID_NAME(U) == 'Rox4' ) THEN
               RETAINED_AVAILABLE(2) = RETAINED_AVAILABLE(2) + &
                                    MWBLOK(BLOCK)*EAVAIL(U)* &
                                                (1.-MAINTENANCE_RATE(U))
               ECITY = .TRUE.
            ELSEIF(SPECIAL_ID_NAME(U) == 'Merom1' .OR. &
                   SPECIAL_ID_NAME(U) == 'Merom2' .OR. &
                   SPECIAL_ID_NAME(U) == 'Ratts1' .OR. &
                   SPECIAL_ID_NAME(U) == 'Ratts2' .OR. &
                   SPECIAL_ID_NAME(U) == 'Williams' .OR. &
                   SPECIAL_ID_NAME(U) == 'Peco' .OR. &
                   SPECIAL_ID_NAME(U) == 'DistGen' .OR. &
                   SPECIAL_ID_NAME(U) == 'Bedford' .OR. &
                   SPECIAL_ID_NAME(U) == 'Worthington' &
                                                         ) THEN

               HOOSIER = .TRUE.

! CHECK FOR SPECIAL HOOSIER UNITS, THEN CYCLE

               IF(    SPECIAL_ID_NAME(U) == 'Merom1') THEN
                  M1_U = U
                  MEROM1_EFFECTIVE_CAP = MEROM1_EFFECTIVE_CAP + &
                                    MWBLOK(BLOCK) * &
                                                (1.-MAINTENANCE_RATE(U))
               ELSEIF(SPECIAL_ID_NAME(U) == 'Merom2') THEN
                  M2_U = U
                  MEROM2_EFFECTIVE_CAP = MEROM2_EFFECTIVE_CAP + &
                                    MWBLOK(BLOCK) * &
                                                (1.-MAINTENANCE_RATE(U))
               ELSEIF(SPECIAL_ID_NAME(U) == 'Ratts1') THEN
                  R1_U = U
                  RATTS1_EFFECTIVE_CAP = RATTS1_EFFECTIVE_CAP + &
                                    MWBLOK(BLOCK) * &
                                                (1.-MAINTENANCE_RATE(U))
               ELSEIF(SPECIAL_ID_NAME(U) == 'Ratts2') THEN
                  R2_U = U
                  RATTS2_EFFECTIVE_CAP = RATTS2_EFFECTIVE_CAP + &
                                    MWBLOK(BLOCK) * &
                                                (1.-MAINTENANCE_RATE(U))
               ELSEIF(SPECIAL_ID_NAME(U) == 'Williams') THEN
                  WI_U = U
                  CYCLE
               ELSEIF(SPECIAL_ID_NAME(U) == 'Peco') THEN
                  PE_U = U
                  CYCLE
               ELSEIF(SPECIAL_ID_NAME(U) == 'DistGen') THEN
                  DG_U = U
                  CYCLE
               ELSEIF(SPECIAL_ID_NAME(U) == 'Bedford') THEN
                  BE_U = U
                  CYCLE
               ELSEIF(SPECIAL_ID_NAME(U) == 'Worthington') THEN
                  WO_U = U
                  CYCLE
               ENDIF
!
            ENDIF
         ENDIF




         DAY_TYPE_COUNTER = 1
         DATA_BASES = GET_DATA_BASE_FOR_UNIT(U,DAY_TYPE_COUNTER)

         IF(DATA_BASES < 1 .OR. DATA_BASES > MAX_DATA_BASES) then
             CYCLE
         end if

         IF(DATA_BASES < 1 .OR. DATA_BASES > MAX_DATA_BASES) THEN
            WRITE(4,*) "Transact was unable to assign a resource"
            WRITE(4,*) "to a Transaction Group. Unit = ",U
            WRITE(4,*) "Block = ",BLOCK
         WRITE(4,*) '*** line 712 MARGNOBJ.FOR ***'
         er_message='See WARNING MESSAGES -MARGNOBJ.FOR-2'
         call end_program(er_message)
         ENDIF
!
         IF(FIRST_ECONOMY_COST(DATA_BASES) == 0.0 .AND. &
                                                  BLKNO(BLOCK) > 0) THEN
            FIRST_ECONOMY_COST(DATA_BASES) = &
                                       MAX(BLOCK_DISP_COST(BLOCK),.0001)
            IF(MUST_RUN_COST(DATA_BASES) < .0001) THEN
               MUST_RUN_COST(DATA_BASES) = &
                                          FIRST_ECONOMY_COST(DATA_BASES)
            ENDIF
         ENDIF
!
         DO WHILE(DATA_BASES > 0)
            IF(SCHEDULE_MOR) THEN
! 041012. ADDED SO THAT MONTHLY MAINTENANCE PERCENT IS ACTIVE AS
!         DERATE WHEN DETAILED MAINTENANCE = TRUE.
               IF(ABS(MNRATE(U,ISEAS)) > 0.001) THEN
                  IF(MNRATE(U,ISEAS) < 0.) THEN
                     MN_RATE = MIN(100., &
                           GET_VAR(MNRATE(U,ISEAS),globecom_YEAR,UNITNM(U)))/100.
                  ELSE
                     MN_RATE = MIN(100.,MNRATE(U,ISEAS))/100.
                  ENDIF
               ELSE
                  MN_RATE = 0.0
               ENDIF
               MW_AFTER_MAINTENANCE = MWBLOK(BLOCK) * (1. - MN_RATE)
!
            ELSE
               MW_AFTER_MAINTENANCE = &
                                MWBLOK(BLOCK) * (1.-MAINTENANCE_RATE(U))
            ENDIF
            IF(SCHEDULE_FOR) THEN
               P = 1.0
            ELSE
               P = EAVAIL(U)
            ENDIF
!
            IF(DRATE_TRANSACT) THEN
               DB_ADJ_TOTAL_CAP(DATA_BASES) = &
                           DB_ADJ_TOTAL_CAP(DATA_BASES) + &
                                        MW_AFTER_MAINTENANCE * P
            ELSE
               DB_ADJ_TOTAL_CAP(DATA_BASES) = &
                           DB_ADJ_TOTAL_CAP(DATA_BASES) + &
                                                    MW_AFTER_MAINTENANCE
            ENDIF
            U = UNIT(BLOCK)
            DB_CAP_AFTER_OUTAGES(DATA_BASES) = &
                        DB_CAP_AFTER_OUTAGES(DATA_BASES) + &
                                        MW_AFTER_MAINTENANCE * P
            DAY_TYPE_COUNTER = DAY_TYPE_COUNTER + 1
            DATA_BASES = GET_DATA_BASE_FOR_UNIT(U,DAY_TYPE_COUNTER)
         ENDDO
!
!
      ENDDO ! BLOCK
!
      IF(ECITY) THEN
         BEFORE_RETAINED = 0.
         DO U = 1, NUNITS
            IF(SPECIAL_ID_NAME(U) == ' ' .OR. &
                                     SPECIAL_ID_NAME(U) == 'None') CYCLE
            IF(         UNITNM(U) ==     'Catawba 1' .OR. &
                           UNITNM(U) ==     'Catawba 2' .OR. &
                           UNITNM(U) ==     'McGuire 1' .OR. &
                           UNITNM(U) ==     'McGuire 2' ) THEN
               BEFORE_RETAINED = BEFORE_RETAINED + &
                               GET_ANNUAL_CL_CAPACITY(INT(1,2),U,ISEAS)
            ENDIF
         ENDDO
      ENDIF
!
! KEEP THIS CALC IN FOR NON-TRANSACT C
!
      IF(RESERVE_CAP_NO(1) > 0) THEN
         RESERVE_CAPACITY(1) = &
                     MAX(0.,MIN(TEMP_TL_PEAK,829.6) - &
                                                  RETAINED_AVAILABLE(1))
!         MWBLOK(RESERVE_BLOCK) = RESERVE_CAPACITY(1)
      ENDIF


      MARGINAL_COST(0) = 0.
      MARGINAL_PROB(0) = 0.



      NEW_BLOCK_NUMBER = 0
      NEW_MARGINAL_DATA_BASE = 0

      NEW_POINT_COUNT = 0
      REMAINING_CONTRIBUTION_MW = 0

      EXPECTED_MARGINAL_COST = 0.
      CAPACITY_GIVEN_MARKET = 0.d0
      BLOCK_B4_MARKET = 0.

      TRANSITION_MARGINAL_COST =  0.d0

      DO DATA_BASES = 1, MAX_DATA_BASES



         MUST_RUN_COST(DATA_BASES) = MAX(0.,MUST_RUN_COST(DATA_BASES))

         LPROB_FOR_MC(0,1,DATA_BASES) = 1.
         LPROB_FOR_MC(0,2,DATA_BASES) = 1.
         CUM_CAP(DATA_BASES) = 0.
         CumDbDx(DATA_BASES) = 0
         CUM_MUST_RUN_MW(DATA_BASES) = 0.
         CUM_MUST_RUN_POSITION(DATA_BASES) = 0
         DX = (DB_ADJ_TOTAL_CAP(DATA_BASES)-1.)/(MAX_POINTS+1.)
         I_CORRECT(DATA_BASES) = MIN(DX/2.,1.)
         DB_DX(DATA_BASES) = DB_ADJ_TOTAL_CAP(DATA_BASES)/(MAX_POINTS)

         PREVIOUS_INTERVAL(DATA_BASES) = 1

         CURRENT(DATA_BASES) = 1
      ENDDO

      DO I = 1, 1000
         DO DATA_BASES = 1, MAX_DATA_BASES

            LODDUR_FOR_MC(I,DATA_BASES) = DB_DX(DATA_BASES)*FLOAT(I)
            LPROB_FOR_MC(I,1,DATA_BASES) = 0.
            LPROB_FOR_MC(I,2,DATA_BASES) = 0.

            IF(I <= 200) THEN
               REMAIN = MOD(I,INT(8,2))
               IF(REMAIN==0) NEW_POINT = I/8
            ELSEIF(I <= 400) THEN
               REMAIN = MOD((I-200),4)
               IF(REMAIN==0) NEW_POINT = ((I-200)/4) + 25
            ELSEIF(I <= 800) THEN
               REMAIN = MOD((I-400),2)
               IF(REMAIN==0) NEW_POINT = ((I-400)/2) + 75
            ELSE
               REMAIN = 1
            ENDIF
            IF(REMAIN==0) NEW_LODDUR(NEW_POINT,DATA_BASES) = &
                                             LODDUR_FOR_MC(I,DATA_BASES)
         ENDDO
         MARGINAL_COST(I) = 0.
         MARGINAL_PROB(I) = 0.
      ENDDO



      LOOKING_AHEAD = .FALSE.
      LAST_INTERVAL = 0

      OLD_FIRST_ECONOMY_COST = 0.
      I = 0
      DO
         I = I + 1
         IF(I > NBLOK2) THEN
            WRITE(4,*) "SIMULATION UNABLE TO FIND FIRST ECONOMIC UNIT"
            OLD_FIRST_ECONOMY_COST = 10.
            EXIT
         ENDIF
         IF(BLKNO(I) < 1) CYCLE
         OLD_FIRST_ECONOMY_COST = &
                          UNIT_DISP_COST(UNIT(I),MAX(INT(1,2),BLKNO(I)))
         EXIT
      ENDDO

      LAST_BLOCK_COST = &
                UNIT_DISP_COST(UNIT(NBLOK2),MAX(INT(1,2),BLKNO(NBLOK2)))

      IF(.NOT. LAHEY_LF95()) &
        CALL MG_LOCATE_WRITE(15,9,'Transaction Overhead',ALL_VERSIONS,0)

      ACTIVE_UNIT = 0

      ALLOCATE(START_UP_UNIT_BY_TG(SAVE_UPPER_TRANS_GROUP))
!
      START_UP_UNIT_BY_TG = 0.
      ALLOCATE(MARGINAL_UNIT_BY_TECH(SAVE_UPPER_TRANS_GROUP, &
                                                        MAX_PROD_TYPES))
      MARGINAL_UNIT_BY_TECH = 0
!
      YES_MARGINAL_UNIT_ACTIVE = MARGINAL_UNIT_BY_TECH_ACTIVE()
!
      IF( YES_MARGINAL_UNIT_ACTIVE .AND. &
                       MARGIN_UNIT_REPORT_NOT_OPEN) THEN
         MARGINAL_UNIT_TECH_NO = &
                       MARGINAL_UNIT_TECH_HEADER(int(MAX_PROD_TYPES,2), &
                                                      MARGINAL_UNIT_REC)
         MARGIN_UNIT_REPORT_NOT_OPEN = .FALSE.
      ENDIF


      START_UP_POSITION = 0
      SCARCITY_MULT = 0.

      TOTAL_START_UP_UNITS = 0

      ALLOCATE(OUTAGE_DB_USED(MAX_TRANS_GROUP_NUMBER))
      ALLOCATE(COMMITMENT_DB(MAX_TRANS_GROUP_NUMBER))

      COMMITMENT_DB = 0


      SAVE_OUTAGE_BLOCKS = 1
      DATABASE_COUNTER = 0

      DO BLOCK = 1, NBLOK2

         U = UNIT(BLOCK)
         B = MAX(INT(1,2),BLKNO(BLOCK))

! NOTE: DOUBLE INDEX
! Legacy values:
! TIMESCALLED = 122850
! SAVE_TG_FILE_EXISTS = .true.
! R_TRANS_GROUP = 7
! GET_TRANS_GROUP_POSITION = 1


         TG = get_TRANSACTION_GROUP(U)
         TG = GET_TRANS_GROUP_POSITION(TG)

         BlockIncrCost(BLOCK)=GET_TOTAL_INCREMENTAL_COST(U,B)



         IF(WABASH_VALLEY) THEN
            IF(WABASH_IM_NIPSCO .AND. &
                (U == GIBSON_BACKUP_POINTER .OR. &
                     U == GIBSON_POINTER .OR. &
                         U == PSI_AREA_LAST_RESOURCE_POINTER)) THEN
               CYCLE
            ENDIF
         ENDIF

! ADDED 3/25/00. FOR ELECTRICITIES.

         IF(U == RESERVE_CAP_NO(1) .OR. &
                                       U == SUPPLEMENTAL_CAP_NO(1) .OR. &
               U == RESERVE_CAP_NO(2) .OR. &
                                       U == SUPPLEMENTAL_CAP_NO(2)) THEN
            CYCLE
         ENDIF

         IF(.NOT. LOOKING_AHEAD) THEN

            IF(SCHEDULE_FOR) THEN ! FOR OUTAGES HANDLED ELSEWHERE
               P = 1.
            ELSE
               P = EAVAIL(U)
            ENDIF
            Q = 1. - P

            IF(SCHEDULE_MOR) THEN
! 041012. ADDED SO THAT MONTHLY MAINTENANCE PERCENT IS ACTIVE AS
!         DERATE WHEN DETAILED MAINTENANCE = TRUE.
               IF(ABS(MNRATE(U,ISEAS)) > 0.001) THEN
                  IF(MNRATE(U,ISEAS) < 0.) THEN
                     MN_RATE = MIN(100., &
                           GET_VAR(MNRATE(U,ISEAS),globecom_YEAR,UNITNM(U)))/100.
                  ELSE
                     MN_RATE = MIN(100.,MNRATE(U,ISEAS))/100.
                  ENDIF
               ELSE
                  MN_RATE = 0.0
               ENDIF
               MW_AFTER_MAINTENANCE = MWBLOK(BLOCK) * (1. - MN_RATE)

            ELSE
               MW_AFTER_MAINTENANCE = &
                                MWBLOK(BLOCK) * (1.-MAINTENANCE_RATE(U))
            ENDIF

            IF(MW_AFTER_MAINTENANCE <= 0. .OR. P == 0.) CYCLE

            IF(BLKNO(BLOCK) > 0) THEN
               DISPATCHING_COST = UNIT_DISP_COST(U,B)
            ELSE
               DISPATCHING_COST = MIN(UNIT_DISP_COST(U,B), &
                                                 OLD_FIRST_ECONOMY_COST)
            ENDIF

         ENDIF

         DAY_TYPE_COUNTER = 1
         DATA_BASES = GET_DATA_BASE_FOR_UNIT(U,DAY_TYPE_COUNTER)
         DO WHILE(DATA_BASES > 0)
            ACTIVE_DATA_BASE(DATA_BASES) = .TRUE.
            DATABASE_COUNTER = DATABASE_COUNTER + 1
            COST_CURVE_POINTER_BY(BLOCK,DATA_BASES) = DATABASE_COUNTER

            BlkDbDx = nint(MW_AFTER_MAINTENANCE/DB_DX(DATA_BASES)) 

            MW_AFTER_OUTAGES(BLOCK,DATA_BASES) = &
                                                MW_AFTER_MAINTENANCE * P
! DRATE
            IF(DRATE_TRANSACT) THEN
               CUM_CAP(DATA_BASES) = CUM_CAP(DATA_BASES) + &
                                      MW_AFTER_OUTAGES(BLOCK,DATA_BASES)
            ELSE
               CUM_CAP(DATA_BASES) = CUM_CAP(DATA_BASES) + &
                                                    MW_AFTER_MAINTENANCE
            ENDIF
            CumDbDx(DATA_BASES) = &
                        INT(CUM_CAP(DATA_BASES) / DB_DX(DATA_BASES)) + 1
            MW_AFTER_OUTAGES(BLOCK,DATA_BASES) = &
                                                MW_AFTER_MAINTENANCE * P
            IF(BLKNO(BLOCK) == 0) CUM_MUST_RUN_MW(DATA_BASES) = &
                          CUM_MUST_RUN_MW(DATA_BASES) + &
                                      MW_AFTER_OUTAGES(BLOCK,DATA_BASES)

            INTERVALS = CumDbDx(DATA_BASES)

            INTERVALS = MIN(INTERVALS,INT(999,2))

            NEXT(DATA_BASES) = MOD(CURRENT(DATA_BASES),INT(2,2)) + 1

            CAPACITY_ASSIGNED = 0.

!            NEW_POINT = 0

            TOTAL_PROB_FOR_SEG = 0.

            IF(.NOT. DRATE_TRANSACT) THEN
               FIRST_INTERVAL = 1
            ELSE
               FIRST_INTERVAL = PREVIOUS_INTERVAL(DATA_BASES)
               PREVIOUS_INTERVAL(DATA_BASES) = INTERVALS
            ENDIF

!            IF(DATA_BASES == 2 .OR. DATA_BASES == 3) THEN

            IF(DETAILED_OUTAGE_DATA_BASE(DATA_BASES)) THEN


               IF(SAVE_OUTAGE_BLOCKS > ALLOCATED_DATA_BASES) THEN

                  WRITE(SCREEN_MESSAGES,"(I5,2X,I5,2X,I5)") &
                                                    SAVE_OUTAGE_BLOCKS, &
                                                    DATA_BASES, &
                                                    ALLOCATED_DATA_BASES
                  CALL MG_LOCATE_WRITE(4,15,trim(SCREEN_MESSAGES), &
                                                         ALL_VERSIONS,0)
                  ALLOCATE(TEMP_TRANSFER_ARRAY1(0:ALLOCATED_DATA_BASES))
                  ALLOCATE(TEMP_TRANSFER_ARRAY2(0:ALLOCATED_DATA_BASES))
                  ALLOCATE(TEMP_TRANSFER_ARRAY3(0:ALLOCATED_DATA_BASES))
                  DO I = 0, ALLOCATED_DATA_BASES
                     TEMP_TRANSFER_ARRAY1(I) = OUTAGE_UNIT_INDEX(I)
                     TEMP_TRANSFER_ARRAY2(I) = OUTAGE_BLOCK_INDEX(I)
                     TEMP_TRANSFER_ARRAY3(I) = TRANS_BLOCK_CAPACITY(I)
                  ENDDO
                  DEALLOCATE(OUTAGE_UNIT_INDEX,OUTAGE_BLOCK_INDEX, &
                             TRANS_BLOCK_CAPACITY)
                  ALLOCATE( &
                          OUTAGE_UNIT_INDEX(0:ALLOCATED_DATA_BASES+100))
                  ALLOCATE( &
                         OUTAGE_BLOCK_INDEX(0:ALLOCATED_DATA_BASES+100))
                  ALLOCATE( &
                       TRANS_BLOCK_CAPACITY(0:ALLOCATED_DATA_BASES+100))
                  DO I = 0, ALLOCATED_DATA_BASES
                     OUTAGE_UNIT_INDEX(I) = TEMP_TRANSFER_ARRAY1(I)
                     OUTAGE_BLOCK_INDEX(I) = TEMP_TRANSFER_ARRAY2(I)
                     TRANS_BLOCK_CAPACITY(I) = TEMP_TRANSFER_ARRAY3(I)
                  ENDDO
                  DO I = ALLOCATED_DATA_BASES+1,ALLOCATED_DATA_BASES+100
                     OUTAGE_UNIT_INDEX(I) = 0
                     OUTAGE_BLOCK_INDEX(I) = 0
                     TRANS_BLOCK_CAPACITY(I) = 0.
                  ENDDO
                  DEALLOCATE(TEMP_TRANSFER_ARRAY1, &
                             TEMP_TRANSFER_ARRAY2, &
                             TEMP_TRANSFER_ARRAY3)
                  ALLOCATED_DATA_BASES = ALLOCATED_DATA_BASES + 100
               ENDIF
               OUTAGE_UNIT_INDEX(SAVE_OUTAGE_BLOCKS) = U
               OUTAGE_BLOCKS_4_DB(DATA_BASES) = &
                                      OUTAGE_BLOCKS_4_DB(DATA_BASES) + 1
               OUTAGE_BLOCK_N_DB( &
                       OUTAGE_BLOCKS_4_DB(DATA_BASES),DATA_BASES) = &
                                                      SAVE_OUTAGE_BLOCKS
               OUTAGE_BLOCK_INDEX(SAVE_OUTAGE_BLOCKS) = BLOCK
               OUTAGE_BLOCK_REVERSE_INDEX(BLOCK,DATA_BASES) = &
                                                      SAVE_OUTAGE_BLOCKS

               UNIT_TO_BLOCK(U,B) = BLOCK ! UNIQUE AND DENSE

! INDEXED ON TG, NOT DB

               IF(ACTIVE_UNIT(U) == 0) THEN
                  ACTIVE_UNIT(U) = 1
                  VOID_LOGICAL = GET_START_UP_LOGIC(U,LOGIC_TYPE)
                  IF(LOGIC_TYPE /= 'F') THEN
                     TOTAL_START_UP_UNITS = TOTAL_START_UP_UNITS + 1
                     START_UP_UNIT_BY_TG(TG) = &
                                             START_UP_UNIT_BY_TG(TG) + 1

                     START_UP_POSITION(START_UP_UNIT_BY_TG(TG),TG) = U
                  ENDIF
               ENDIF
               TRANS_BLOCK_CAPACITY(SAVE_OUTAGE_BLOCKS) = &
                                      MW_AFTER_OUTAGES(BLOCK,DATA_BASES)

               SAVE_OUTAGE_BLOCKS = SAVE_OUTAGE_BLOCKS + 1
            ENDIF

            DO I = FIRST_INTERVAL, INTERVALS

! 3/5/98. GAT. MARK THINKS THAT Z SHOULD GO TO ZERO.

               IF(DRATE_TRANSACT) THEN
                  MARGINAL_PROB_FOR_SEG = 1.0
               ELSE
                  z=intervals-i+1 ! positive, on [1,intervals]

                  z_c = max(INT(0,4),INT(z-BlkDbDx,4))

                  LPROB_FOR_MC(Z,NEXT(DATA_BASES),DATA_BASES) = &
                    P * LPROB_FOR_MC(Z,CURRENT(DATA_BASES),DATA_BASES) + &
                    Q * LPROB_FOR_MC(Z_C,CURRENT(DATA_BASES),DATA_BASES)

                  MARGINAL_PROB_FOR_SEG = P * &
                   (LPROB_FOR_MC(Z_C,CURRENT(DATA_BASES),DATA_BASES) - &
                      LPROB_FOR_MC(Z,CURRENT(DATA_BASES),DATA_BASES))

                  TOTAL_PROB_FOR_SEG = TOTAL_PROB_FOR_SEG + &
                                                   MARGINAL_PROB_FOR_SEG

                  IF(MARGINAL_PROB_FOR_SEG <= 0. .AND. &
                        CAPACITY_ASSIGNED/ &
                        MW_AFTER_OUTAGES(BLOCK,DATA_BASES) > .9999) THEN
                     CYCLE
                  ENDIF
               ENDIF ! DRATE OR CONVOLVE



               IF(DRATE_TRANSACT) THEN
                  IF    (I <= 200) THEN
                     DIVISOR = 8
                     REMAIN = MOD(I,DIVISOR)

                     NEW_POINT = I/DIVISOR
                  ELSEIF(I <= 400) THEN
                     DIVISOR = 4
                     REMAIN = MOD((I-INT(200,2)),DIVISOR)

                     NEW_POINT = ((I-200)/DIVISOR) + 25
                  ELSEIF(I <= 800) THEN
                     DIVISOR = 2
                     REMAIN = MOD((I-INT(400,2)),DIVISOR)

                     NEW_POINT = ((I-400)/DIVISOR) + 75
                  else
               remain=1 ! to preclude prior 0-value allowing entre below
                  ENDIF
               ELSE
                  IF    (I <= 200) THEN
                     DIVISOR = 8
                     IF(REMAIN==0) NEW_POINT = I/DIVISOR
                  ELSEIF(I <= 400) THEN
                     DIVISOR = 4
                     IF(REMAIN==0) NEW_POINT = ((I-200)/DIVISOR) + 25
                  ELSEIF(I <= 800) THEN
                     DIVISOR = 2
                     REMAIN = MOD((I-INT(400,2)),DIVISOR)
                     IF(REMAIN==0) NEW_POINT = ((I-400)/DIVISOR) + 75
                  else
               remain=1 ! to preclude prior 0-value allowing entre below
                  ENDIF
               ENDIF


               IF(REMAIN == 0.) THEN
                  COST_POINT = NEW_POINT
               ELSE
               COST_POINT = MIN(NEW_POINT + INT(1,2),TRANS_GROUP_POINTS)
               ENDIF


               IF(CUM_INTERVAL_CAPACITY(COST_POINT,DATA_BASES) < &
                                FLOAT(DIVISOR) * DB_DX(DATA_BASES)) THEN
                  INTERVAL_CAPACITY = DB_DX(DATA_BASES)
                  IF( I == INTERVALS) THEN
!
                     TEMP_CONTRIBUTION = &
                           MW_AFTER_OUTAGES(BLOCK,DATA_BASES) &
                                                     - CAPACITY_ASSIGNED

                  ELSEIF(MARGINAL_PROB_FOR_SEG > 0.) THEN
                     TEMP_CONTRIBUTION = &
                         MAX(0.,amin1(MW_AFTER_OUTAGES(BLOCK,DATA_BASES) &
                                                   - CAPACITY_ASSIGNED, &
                             MARGINAL_PROB_FOR_SEG * INTERVAL_CAPACITY))

                  ELSE
                     TEMP_CONTRIBUTION = 0.
                  ENDIF

                  IF(DRATE_TRANSACT) THEN
                     TEMP_CONTRIBUTION = MAX(0.,MIN(TEMP_CONTRIBUTION, &
                        FLOAT(DIVISOR) * DB_DX(DATA_BASES) - &
                          CUM_INTERVAL_CAPACITY(COST_POINT,DATA_BASES)))
                  ENDIF

                  CAPACITY_ASSIGNED = CAPACITY_ASSIGNED + &
                                                       TEMP_CONTRIBUTION

                  IF(CAPACITY_ASSIGNED > &
                                MW_AFTER_OUTAGES(BLOCK,DATA_BASES)) THEN
                     CAPACITY_ASSIGNED = CAPACITY_ASSIGNED
                  ENDIF
!
                  CUM_UNIT_UTIL(COST_POINT,DATABASE_COUNTER) = &
                          CUM_UNIT_UTIL(COST_POINT,DATABASE_COUNTER) &
                                                    +  TEMP_CONTRIBUTION
!
                  CUM_UNIT_UTIL_zero(COST_POINT,DATA_BASES) = &
                               CUM_UNIT_UTIL_zero(COST_POINT,DATA_BASES) &
                               + TEMP_CONTRIBUTION
                  CUM_INTERVAL_CAPACITY(COST_POINT,DATA_BASES) = &
                          CUM_INTERVAL_CAPACITY(COST_POINT,DATA_BASES) + &
                                                       TEMP_CONTRIBUTION
               ENDIF ! CAPACITY IS AVAILABLE FOR ACCUMULATION
!
               INTERVAL_CAPACITY = FLOAT(DIVISOR) * DB_DX(DATA_BASES)
               IF(REMAIN==0 .AND. &
                         NEW_POINT_COUNT(NEW_POINT,DATA_BASES) < &
                                               CONTRIB_INTERVALS-1) THEN

                  NEW_POINT_COUNT(NEW_POINT,DATA_BASES) = &
                               NEW_POINT_COUNT(NEW_POINT,DATA_BASES) + 1
                  NPC = NEW_POINT_COUNT(NEW_POINT,DATA_BASES)

                  TEMP_CONTRIBUTION = MARGINAL_PROB_FOR_SEG* &
                     amin1(MW_AFTER_MAINTENANCE,INTERVAL_CAPACITY)

                  NEW_MARGINAL_DATA_BASE(NEW_POINT,NPC,DATA_BASES) = &
                      NEW_MARGINAL_DATA_BASE(NEW_POINT,NPC,DATA_BASES) + &
                                                   MARGINAL_PROB_FOR_SEG
                  NEW_BLOCK_NUMBER(NEW_POINT,NPC,DATA_BASES) = BLOCK

                  NEW_MARGINAL_DATA_BASE(NEW_POINT, &
                                     CONTRIB_INTERVALS,DATA_BASES) = &
                       NEW_MARGINAL_DATA_BASE(NEW_POINT, &
                                     CONTRIB_INTERVALS,DATA_BASES) + &
                                                   MARGINAL_PROB_FOR_SEG
                  EXPECTED_MARGINAL_COST(NEW_POINT,DATA_BASES) = & !  UNITS ARE $/MWH
                        EXPECTED_MARGINAL_COST(NEW_POINT,DATA_BASES) + &
                              DISPATCHING_COST*MARGINAL_PROB_FOR_SEG/10.
                  IF(MARGINAL_PROB_FOR_SEG > .0000001) THEN
                     NEW_BLOCK_NUMBER(NEW_POINT, &
                                     CONTRIB_INTERVALS,DATA_BASES) = NPC
                  ENDIF
               ENDIF ! ASSIGNMENT OF VALUES AT NEW_POINT

               IF(DATA_BASES == 1) THEN
                  MARGINAL_PROB(I) = &
                  MARGINAL_PROB(I) + MARGINAL_PROB_FOR_SEG
                  MARGINAL_COST(I) = &
                  MARGINAL_COST(I) + &
                                  MARGINAL_PROB_FOR_SEG*DISPATCHING_COST
                  IF(MARGINAL_PROB(I) > 0. .AND. I > LAST_INTERVAL) &
                     LAST_INTERVAL = I
               ENDIF
            ENDDO ! INTERVALS
!
            CURRENT(DATA_BASES) = NEXT(DATA_BASES)
            IF( ABS(TOTAL_PROB_FOR_SEG - 1.) > .1) THEN
               TOTAL_PROB_FOR_SEG = TOTAL_PROB_FOR_SEG

            ENDIF
!
            DAY_TYPE_COUNTER = DAY_TYPE_COUNTER + 1
            DATA_BASES = GET_DATA_BASE_FOR_UNIT(U,DAY_TYPE_COUNTER)
!
         ENDDO ! DATA BASES
!
      ENDDO ! BLOCKS
!
      CALL CLS(15,9,33)
!
      TOTAL_COST = 0.
      K = 1
      DO I = 1, LAST_INTERVAL
         IF(MARGINAL_PROB(I) <= 0.) CYCLE
!
         MARGINAL_COST(I) = MARGINAL_COST(I) / MARGINAL_PROB(I)
         TOTAL_COST = TOTAL_COST+MARGINAL_COST(I) * DB_DX(1)
         AVE_COST(I) = TOTAL_COST/(FLOAT(I)*DB_DX(1))
!
         IF(YES_MARGINAL_COST_MODULE .AND. .NOT. TESTING_PLAN) THEN
            WRITE(MARGINAL_NO,REC=MARGINAL_REC) PRT_ENDPOINT(), &
                                          FLOAT(SAVE_YEAR), &
                                          CL_MONTH_NAME(PERIOD_COUNTER), &
                                          FLOAT(K), &
                                          MARGINAL_COST(I)/10., &
                                          LODDUR_FOR_MC(I,1), &
                                          100.*MARGINAL_PROB(I), &
                                          AVE_COST(I)/10.
            MARGINAL_REC = MARGINAL_REC + 1
         ENDIF
!
         K = K + 1
!
!
      ENDDO
      DO I = LAST_INTERVAL + 1, 1000
         MARGINAL_COST(I) = MARGINAL_COST(I-1)
      ENDDO
!
      IF(WRITE_TO_MC_DATA_BASE .AND. MARGINAL_DATA_BASE_NOT_OPEN) THEN
         LAST_DATA_INTERVAL = LAST_INTERVAL
         MARGINAL_DATA_NO = MARGINAL_DATA_BASE_HEADER( &
                                   LAST_DATA_INTERVAL,MARGINAL_DATA_REC)
         MARGINAL_DATA_BASE_NOT_OPEN = .FALSE.
      ENDIF
!
!
      IF(WRITE_TO_MC_DATA_BASE) THEN
!
         WRITE(MARGINAL_DATA_NO,REC=MARGINAL_DATA_REC) gc_end_point, &
                                 globecom_YEAR+get_BASE_YEAR(), &
                                 PERIOD_COUNTER,LAST_DATA_INTERVAL, &
                                 LAST_BLOCK_COST,CUM_CAP, &
                                 CUM_MUST_RUN_MW,BASE,PEAK
         MARGINAL_DATA_REC = MARGINAL_DATA_REC + 1
!
         WRITE(MARGINAL_DATA_NO,REC=MARGINAL_DATA_REC) &
                            (LODDUR_FOR_MC(I,1),I=1, LAST_DATA_INTERVAL)
         MARGINAL_DATA_REC = MARGINAL_DATA_REC + 1
         WRITE(MARGINAL_DATA_NO,REC=MARGINAL_DATA_REC) &
                          (MARGINAL_COST(I)/10.,I=1, LAST_DATA_INTERVAL)
         MARGINAL_DATA_REC = MARGINAL_DATA_REC + 1
         WRITE(MARGINAL_DATA_NO,REC=MARGINAL_DATA_REC) &
                                   (AVE_COST(I),I=1, LAST_DATA_INTERVAL)
         MARGINAL_DATA_REC = MARGINAL_DATA_REC + 1
         WRITE(MARGINAL_DATA_NO,REC=MARGINAL_DATA_REC) &
                              (MARGINAL_PROB(I),I=1, LAST_DATA_INTERVAL)
         MARGINAL_DATA_REC = MARGINAL_DATA_REC + 1

      ENDIF

      IF(.NOT. LAHEY_LF95()) &
             CALL MG_LOCATE_WRITE(15,9,'Transaction Processing', &
                                                         ALL_VERSIONS,0)
!
      LAST_DB_POINT = 0
!
      DO I = 1, MAX_DATA_BASES
         IF(.NOT. ACTIVE_DATA_BASE(I)) CYCLE ! ALL DATABASES ARE ACTIVE BY DEFINITION
         LAST_EXPECTED_MARGINAL_COST = 0.
         LAST_BLOCK_NUMBER = 0
!

!
         TG = SAVE_TRANS_FOR_DATA_BASE(I)
         VOID_LOGICAL = GET_SCARCITY_INFO(TG, &
                                          FIRST_CAPACITY_VALUE, &
                                          FIRST_CAPACITY_PERCENT, &
                                          SECOND_CAPACITY_VALUE, &
                                          SECOND_CAPACITY_PERCENT, &
                                          THIRD_CAPACITY_VALUE, &
                                          THIRD_CAPACITY_PERCENT, &
                                          SAVE_SEASON,globecom_YEAR, &
                                          CAPACITY_ADDER, &
                                          ADDITIONAL_CAPACITY_VALUE, &
                                          ADDITIONAL_CAPACITY_PERCENT)
!
         TOTAL_SCARCITY_MW = DB_CAP_AFTER_OUTAGES(I) + CAPACITY_ADDER
!
         FIRST_CAPACITY_MW = TOTAL_SCARCITY_MW * &
                                             FIRST_CAPACITY_PERCENT/100.
         SECOND_CAPACITY_MW = TOTAL_SCARCITY_MW * &
                                            SECOND_CAPACITY_PERCENT/100.
         THIRD_CAPACITY_MW = TOTAL_SCARCITY_MW * &
                                             THIRD_CAPACITY_PERCENT/100.
! 5/8/00. MODIFIED FOR 10 POINT GLOBAL SCARCITY.
         IF(TG >= 0) THEN
            DO S = 1, 7
               ADDITIONAL_CAPACITY_MW(S) = &
                     TOTAL_SCARCITY_MW * &
                              ADDITIONAL_CAPACITY_PERCENT(S)/100.
            ENDDO

! 051212. ALWAYS ASSUMES A 10 POINT CURVE.

            NEW_SCARCITY_LOGIC = .TRUE.


         ENDIF

! FLIPPING THE NEW_POINT LOOP AND THE DISPATCH BLOCK LOOP WILL DECREASE
! RUN TIME IN THESE LOOPS BECAUSE THERE ARE ONLY A FEW ACTIVE COST VECTORS
! FOR EACH DISPATCH BLOCK. THE ACTIVE COST VECTORS NEED TO BE FOUND BEFORE
! STEPPING THROUGH THE INDIVIDUAL COST VECTOR POINTS MSG 10/26/00

         DO NEW_POINT = 1, TRANS_GROUP_POINTS
            IF(NEW_MARGINAL_DATA_BASE(NEW_POINT, &
                                         CONTRIB_INTERVALS,I) > 0.) THEN

               LAST_DB_POINT(I) = NEW_POINT
!
               EXPECTED_MARGINAL_COST(NEW_POINT,I) = &
                  MAX(LAST_EXPECTED_MARGINAL_COST, &
                  EXPECTED_MARGINAL_COST(NEW_POINT,I) / &
                  NEW_MARGINAL_DATA_BASE(NEW_POINT,CONTRIB_INTERVALS,I))
               LAST_EXPECTED_MARGINAL_COST = &
                                     EXPECTED_MARGINAL_COST(NEW_POINT,I)
!
               CURRENT_BLOCK_NUMBER = &
                               MAX(LAST_BLOCK_NUMBER, &
                                        NEW_BLOCK_NUMBER(NEW_POINT,1,I))
               IF(BLKNO(CURRENT_BLOCK_NUMBER) > 0) THEN
                  IF(NEW_POINT > 1) THEN
                     TRANSITION_MARGINAL_COST(NEW_POINT,I,1) = &
                      MAX(REAL(BLOCK_DISP_COST(CURRENT_BLOCK_NUMBER),8), &
                              TRANSITION_MARGINAL_COST(NEW_POINT-1,I,1))
                  ELSE ! ADDED 9/17/99. GAT. FOR SRP.
                     TRANSITION_MARGINAL_COST(NEW_POINT,I,1) = &
                                   BLOCK_DISP_COST(CURRENT_BLOCK_NUMBER)
                  ENDIF
               ELSE
                  TRANSITION_MARGINAL_COST(NEW_POINT,I,1) = &
                              MIN(BLOCK_DISP_COST(CURRENT_BLOCK_NUMBER), &
                                                       MUST_RUN_COST(I))
               ENDIF
!
!
               BLOCK_B4_MARKET(NEW_POINT,I) = CURRENT_BLOCK_NUMBER
               LAST_BLOCK_NUMBER = CURRENT_BLOCK_NUMBER
!
            ELSEIF(NEW_POINT > 1) THEN
               BLOCK_B4_MARKET(NEW_POINT,I) = &
                                          BLOCK_B4_MARKET(NEW_POINT-1,I)
            ENDIF
!
!
! ARRAY SIZE ADJUSTED LOGIC MSG 10/26/00
!
               DO BLOCK = 1, NBLOK2
                  DATABASE = COST_CURVE_POINTER_BY(BLOCK,I)
                  IF(DATABASE == 0) then
            CYCLE  ! THIS DATABASE HAS NO INFORMATION FOR THIS DISPATCH POINT
                  end if
                  IF(NEW_POINT > 1) THEN
                     IF(CUM_UNIT_UTIL(NEW_POINT-1, &
                                                  DATABASE) == 0.) CYCLE
!
! 2/23/98. GAT. MOVED FROM BELOW
!
                     IF(BLOCK > BLOCK_B4_MARKET(NEW_POINT,I)) THEN
!
                        CUM_UNIT_UTIL(NEW_POINT,DATABASE) = &
                               CUM_UNIT_UTIL(NEW_POINT,DATABASE) &
                               + CUM_UNIT_UTIL(NEW_POINT-1,DATABASE)
!
                        CUM_UNIT_UTIL_zero(NEW_POINT,I) = &
                               CUM_UNIT_UTIL_zero(NEW_POINT,I) &
                               + CUM_UNIT_UTIL(NEW_POINT-1,DATABASE)
                        CUM_UNIT_UTIL_zero(NEW_POINT-1,I) = &
                               CUM_UNIT_UTIL_zero(NEW_POINT-1,I) &
                               - CUM_UNIT_UTIL(NEW_POINT-1,DATABASE)
                        CAPACITY_GIVEN_MARKET(NEW_POINT-1,I) = &
                                       CUM_UNIT_UTIL_zero(NEW_POINT-1,I)
!
                        CUM_UNIT_UTIL(NEW_POINT-1,DATABASE) = 0.
!
                        CYCLE
!
                     ENDIF
!
                     CUM_UNIT_UTIL(NEW_POINT,DATABASE) = &
                               CUM_UNIT_UTIL(NEW_POINT,DATABASE) &
                               + CUM_UNIT_UTIL(NEW_POINT-1,DATABASE)
!
                     CUM_UNIT_UTIL_zero(NEW_POINT,I) = &
                               CUM_UNIT_UTIL_zero(NEW_POINT,I) &
                               + CUM_UNIT_UTIL(NEW_POINT-1,DATABASE)
!
                  ENDIF
!
                  TOT_EMBED_COST_BY_POINT(NEW_POINT,I) = &
                           TOT_EMBED_COST_BY_POINT(NEW_POINT,I) &
                           + BlockIncrCost(BLOCK) * &
                                   CUM_UNIT_UTIL(NEW_POINT,DATABASE)
               ENDDO ! NBLOK2
!
! 2/23/98. GAT. MOVED FROM ABOVE. WVPA WAS SEEING INCONSISTENCY
!          BETWEEN CUM_UNIT_UTIL AND CAPACITY GIVEN MARKET.
!
!
               CAPACITY_GIVEN_MARKET(NEW_POINT,I) = &
                                         CUM_UNIT_UTIL_zero(NEW_POINT,I)
!
               IF(CUM_MUST_RUN_MW(I) > 0. .AND. &
                     CUM_MUST_RUN_POSITION(I) == 0 .AND. REAL( &
                     CUM_MUST_RUN_MW(I),8) < &
                                CAPACITY_GIVEN_MARKET(NEW_POINT,I)) THEN
                  CUM_MUST_RUN_POSITION(I) = NEW_POINT - 1
               ENDIF



! TEST FOR GAPS IN EMBEDDED ENERGY CALCULATION
! DUE TO UNITS NOT CONTRIBUTING TO THE MARGIN
!
            IF(NEW_POINT <= 25 ) THEN
               DIVISOR = 8
            ELSEIF(NEW_POINT <= 75) THEN
               DIVISOR = 4
            ELSEIF(NEW_POINT <= 275) THEN
               DIVISOR = 2
            ENDIF
            INTERVAL_CAPACITY = FLOAT(DIVISOR) * DB_DX(I)
            IF( ABS( NEW_MARGINAL_DATA_BASE(NEW_POINT, &
                                                 CONTRIB_INTERVALS,I) - &
               (CUM_UNIT_UTIL_zero(NEW_POINT,I)/INTERVAL_CAPACITY) ) > &
                                                             .0001) THEN
               INTERVAL_CAPACITY = INTERVAL_CAPACITY ! GAPPING PROBLEM
            ENDIF
!
!
!           SCARCITY FUNCTION
!
            TEMP_CAP = CAPACITY_GIVEN_MARKET(NEW_POINT,I)
!
            IF(TEMP_CAP < FIRST_CAPACITY_MW) THEN
               TRANSITION_MARGINAL_COST(NEW_POINT,I,2) = 0.
            ELSEIF(TEMP_CAP > THIRD_CAPACITY_MW) THEN
               TRANSITION_MARGINAL_COST(NEW_POINT,I,2) = &
                                                    THIRD_CAPACITY_VALUE
            ELSEIF(TEMP_CAP >= SECOND_CAPACITY_MW) THEN
               IF(THIRD_CAPACITY_MW - SECOND_CAPACITY_MW > .01) THEN
                  SCARCITY_PERCENT = (TEMP_CAP - SECOND_CAPACITY_MW) / &
                                (THIRD_CAPACITY_MW - SECOND_CAPACITY_MW)
               ELSE
                  SCARCITY_PERCENT = 0.
               ENDIF
               TRANSITION_MARGINAL_COST(NEW_POINT,I,2) = &
                  (1.- SCARCITY_PERCENT) * SECOND_CAPACITY_VALUE + &
                                 SCARCITY_PERCENT * THIRD_CAPACITY_VALUE
            ELSE
               IF(SECOND_CAPACITY_MW - FIRST_CAPACITY_MW > .01) THEN
                  SCARCITY_PERCENT = (TEMP_CAP - FIRST_CAPACITY_MW) / &
                                (SECOND_CAPACITY_MW - FIRST_CAPACITY_MW)
               ELSE
                  SCARCITY_PERCENT = 0.
               ENDIF
               TRANSITION_MARGINAL_COST(NEW_POINT,I,2) = &
                  (1.- SCARCITY_PERCENT) * FIRST_CAPACITY_VALUE + &
                                SCARCITY_PERCENT * SECOND_CAPACITY_VALUE
            ENDIF

!            NEW_SCARCITY_LOGIC. FOR ARTMAN. 10/25/99. GAT.
!            HUNT FOR THE INTERVAL ABOVE THE THIRD_CAP THAT CONTAINS TEMP_CAP

            IF(NEW_SCARCITY_LOGIC .AND. &
                                      TEMP_CAP > THIRD_CAPACITY_MW) THEN

! TRAP THE BOUNDARIES, JUST LIKE ABOVE. ELSE, FIND THE INTERVAL (1,6)
!
               IF(TEMP_CAP < ADDITIONAL_CAPACITY_MW(1)) THEN
                  IF(ADDITIONAL_CAPACITY_MW(1) - &
                                           THIRD_CAPACITY_MW > .01) THEN
                     SCARCITY_PERCENT = (TEMP_CAP - THIRD_CAPACITY_MW) / &
                         (ADDITIONAL_CAPACITY_MW(1) - THIRD_CAPACITY_MW)
                  ELSE
                     SCARCITY_PERCENT = 0.
                  ENDIF
                  TRANSITION_MARGINAL_COST(NEW_POINT,I,2) = &
                     (1.- SCARCITY_PERCENT) * THIRD_CAPACITY_VALUE + &
                      SCARCITY_PERCENT * ADDITIONAL_CAPACITY_VALUE(1)
               ELSEIF(TEMP_CAP >= ADDITIONAL_CAPACITY_MW(7)) THEN
                  TRANSITION_MARGINAL_COST(NEW_POINT,I,2) = &
                                         ADDITIONAL_CAPACITY_VALUE(7)
               ELSE
                  DO S = 1, 6
                     IF(TEMP_CAP >= ADDITIONAL_CAPACITY_MW(S+1)) CYCLE
                     IF(ADDITIONAL_CAPACITY_MW(S+1) - &
                                    ADDITIONAL_CAPACITY_MW(S) > .01)THEN
                        SCARCITY_PERCENT = &
                           (TEMP_CAP - ADDITIONAL_CAPACITY_MW(S)) / &
                                 (ADDITIONAL_CAPACITY_MW(S+1) - &
                                              ADDITIONAL_CAPACITY_MW(S))
                     ELSE
                        SCARCITY_PERCENT = 0.
                     ENDIF
                     TRANSITION_MARGINAL_COST(NEW_POINT,I,2) = &
                        (1.- SCARCITY_PERCENT) * &
                                       ADDITIONAL_CAPACITY_VALUE(S) + &
                        SCARCITY_PERCENT * &
                                       ADDITIONAL_CAPACITY_VALUE(S+1)
                     EXIT
                  ENDDO
                  IF(S == 7) THEN
                     WRITE(4,*) "Untrapped value in new scarcity"
                     WRITE(4,*) "function.  No scarcity value used."
                  ENDIF
               ENDIF ! BOUNDARIES
            ENDIF ! NEW_SCARCITY_LOGIC

            TRANSITION_MARGINAL_COST(NEW_POINT,I,3) = &
                           TRANSITION_MARGINAL_COST(NEW_POINT,I,1) + &
                                 TRANSITION_MARGINAL_COST(NEW_POINT,I,2)

         ENDDO ! NEW_POINT

! 05/07/03. TECHNOLOGY SCARCITY

         IF(YES_TECHNOLOGY_SCARCITY) THEN

! 06/26/03.

            IF(USE_TECH_SCAR_THIS_MONTH) THEN

! FIND TECH BREAKS AND STRENGTH OF THE BREAK

               TECH_BREAK_INTERVAL = 0
               TECH_BREAK_MW = 0.
               TECH_BREAK_STRENGTH = 0.


               TECH_BREAK_COUNT = 1
               TECH_BREAK_INTERVAL(TECH_BREAK_COUNT) = 1
               TECH_BREAK_MW(TECH_BREAK_COUNT) = 0.

               DO NEW_POINT = 2, TRANS_GROUP_POINTS
                  TECH_BREAK_TEST = &
                     TRANSITION_MARGINAL_COST(NEW_POINT,I,1)/ &
                          TRANSITION_MARGINAL_COST(NEW_POINT-1,I,1) - 1.
                  IF(TECH_BREAK_TEST > .20) THEN ! ARBITRARY
                     TECH_BREAK_COUNT = TECH_BREAK_COUNT + 1
                     IF(TECH_BREAK_COUNT <= 20) THEN
                        TECH_BREAK_INTERVAL(TECH_BREAK_COUNT) = &
                                                               NEW_POINT
                        TECH_BREAK_MW(TECH_BREAK_COUNT) = &
                                      CAPACITY_GIVEN_MARKET(NEW_POINT,I)
                        TECH_BREAK_STRENGTH(TECH_BREAK_COUNT) = &
                                                         TECH_BREAK_TEST
                     ELSE
                        WRITE(4,*) "EXCEEDED TECH BREAK POINTS"
                        TECH_BREAK_COUNT = 20

                     ENDIF
                  ENDIF
               ENDDO


               IF(TECH_PERCENT_TRANS_CAP) THEN
                  TECH_BREAK_TEST = TECH_PERCENT_INPUT * &
                             CAPACITY_GIVEN_MARKET(TRANS_GROUP_POINTS,I)
               ELSE
               ENDIF

               IF(TECH_BREAK_TEST <= 0.) THEN
                  TECH_BREAK_TEST = .00001
               ENDIF

               DO J = 2, TECH_BREAK_COUNT
                  DO NEW_POINT = 1, TECH_BREAK_INTERVAL(J) - 1
                     IF(CAPACITY_GIVEN_MARKET(NEW_POINT,I) < &
                               TECH_BREAK_MW(J) - TECH_BREAK_TEST) CYCLE
! (0,1)
                     TECH_BREAK_MW_PERCENT_CHANGE = &
                           MAX(0.,REAL((TECH_BREAK_TEST - &
                              TECH_BREAK_MW(J) + &
                                  CAPACITY_GIVEN_MARKET(NEW_POINT,I)))/ &
                                                        TECH_BREAK_TEST)
!
! 05/08/03. TOM PREFERS THE SQUARED TERM
!
                     TECH_BREAK_MW_PERCENT_CHANGE = &
                           TECH_BREAK_STRENGTH(J) * &
                              TECH_BREAK_MW_PERCENT_CHANGE ** &
                                               TECHNOLOGY_SCARCITY_POWER
!     +                        TECH_BREAK_MW_PERCENT_CHANGE *
!     +                             TECH_BREAK_MW_PERCENT_CHANGE *
!     +                              TECH_BREAK_MW_PERCENT_CHANGE *
                     TRANSITION_MARGINAL_COST(NEW_POINT,I,4) = &
                        TRANSITION_MARGINAL_COST(NEW_POINT,I,4) + &
                    MAX(0.,REAL(TRANSITION_MARGINAL_COST(NEW_POINT,I,1)* &
                                          TECH_BREAK_MW_PERCENT_CHANGE))
                  ENDDO
               ENDDO
            ENDIF ! USE_TECH_SCAR_THIS_MONTH
            DO NEW_POINT = 1, TRANS_GROUP_POINTS
               TRANSITION_MARGINAL_COST(NEW_POINT,I,5) = &
                          TRANSITION_MARGINAL_COST(NEW_POINT,I,3) + &
                                 TRANSITION_MARGINAL_COST(NEW_POINT,I,4)
               TRANSITION_MARGINAL_COST(NEW_POINT,I,4) = &
                          TRANSITION_MARGINAL_COST(NEW_POINT,I,1) + &
                                 TRANSITION_MARGINAL_COST(NEW_POINT,I,4)
            ENDDO
         ENDIF

         IF(TECH_SCAR_REPORT) THEN
            IF(TECH_SCAR_REPORT_NOT_OPEN) THEN
               TECH_SCAR_NO = TECH_SCAR_HEADER(INT(PRODUCTS+1,2), &
                                                          TECH_SCAR_REC)
               TECH_SCAR_REPORT_NOT_OPEN = .FALSE.
            ENDIF

! NEED AN INDEX?

            IF(TG > 0) THEN
               LOCAL_GROUP_NAME = &
                            GET_GROUP_NAME(GET_TRANS_GROUP_POSITION(TG))
            ELSE
               LOCAL_GROUP_NAME = 'System              '
            ENDIF
!
            DO NEW_POINT = 1, TRANS_GROUP_POINTS
               WRITE(TECH_SCAR_NO,REC=TECH_SCAR_REC) &
                    PRT_ENDPOINT(), &
                    FLOAT(SAVE_YEAR), &
                    CL_MONTH_NAME(PERIOD_COUNTER), &
                    LOCAL_GROUP_NAME, &
                    FLOAT(NEW_POINT), &
                    (SNGL(TRANSITION_MARGINAL_COST(NEW_POINT,I,J)), &
                                                          J=1,PRODUCTS), &
                    SNGL(CAPACITY_GIVEN_MARKET(NEW_POINT,I))
                  TECH_SCAR_REC = TECH_SCAR_REC + 1
            ENDDO
         ENDIF


      ENDDO ! DATA_BASES

      CALL CLS(15,9,33)

      CAL_MARGINAL_COST = .TRUE.

! THIS INITIALIZES THE OUTAGES TO TRUE FOR THE MONTH

! 7/18/00.

      IF(ALLOCATED(OUTAGE_SINGULAR))DEALLOCATE(OUTAGE_SINGULAR,STAT=IOS)
      IF(ALLOCATED(ADJUST_BLOCK)) DEALLOCATE(ADJUST_BLOCK,STAT=IOS)
      IF(ALLOCATED(DERATE_BLOCK)) DEALLOCATE(DERATE_BLOCK,STAT=IOS)
      IF(ALLOCATED(OUTAGE_TYPE)) DEALLOCATE(OUTAGE_TYPE,STAT=IOS)
      ALLOCATE(OUTAGE_SINGULAR(0:SAVE_OUTAGE_BLOCKS))
      ALLOCATE(ADJUST_BLOCK(0:SAVE_OUTAGE_BLOCKS))
      ALLOCATE(DERATE_BLOCK(0:SAVE_OUTAGE_BLOCKS))
      ALLOCATE(OUTAGE_TYPE(0:SAVE_OUTAGE_BLOCKS))
      OUTAGE_SINGULAR = 1.
      DERATE_BLOCK = 1.
      OUTAGE_TYPE = 0.



      DO J = 1, SAVE_UPPER_TRANS_GROUP

         SCARCITY_MULT(J,1) = GET_NIGHT_SCARCITY_MULT(J)
         SCARCITY_MULT(J,2) = GET_WEEKEND_SCARCITY_MULT(J)
      ENDDO

! 6/27/02.
      IF(UNIT_COMMITMENT_LOGIC .OR. PRICE_MODE) THEN

         ALLOCATE(HOURLY_OUTAGE_STATE(NBLOK2,SAVE_HOURS_IN_MONTH))
         ALLOCATE(HOURLY_OUTAGE_TYPE(NBLOK2,SAVE_HOURS_IN_MONTH))
         ALLOCATE(HOURLY_LAST_STATE(NBLOK2))
         ALLOCATE(HOURLY_LAST_DERATE(NBLOK2))
         ALLOCATE(HOURLY_LAST_TYPE(NBLOK2))

!        01/06/01. GAT


         HOURLY_OUTAGE_STATE = 0.
         HOURLY_OUTAGE_TYPE = 0.
         HOURLY_LAST_STATE = 1.
         HOURLY_LAST_DERATE = 1.
         HOURLY_LAST_TYPE = 0.

         INT_DAY = 1
         INT_HOUR_IN_DAY = 0
         call check_hours_in_month(SAVE_HOURS_IN_MONTH, "margnobj:0006")
         DO LOCAL_HOUR = 1, SAVE_HOURS_IN_MONTH
            REMAIN = MOD(FLOAT(LOCAL_HOUR),24.)

            IF(INT_HOUR_IN_DAY < 24) THEN
               INT_HOUR_IN_DAY = INT_HOUR_IN_DAY + 1
            ELSE
               INT_HOUR_IN_DAY = 1
               INT_DAY = INT_DAY + 1
            ENDIF

            CALL GET_DAY_OF_WEEK_IN_MONTH( &
                                     INT_DAY,ISEAS,CALANDER_DAY_OF_WEEK)
            DAY_TYPE = &
                    GET_DAY_TYPE(INT_HOUR_IN_DAY, &
                                           CALANDER_DAY_OF_WEEK,ISEAS)

            DO J = 1, SAVE_UPPER_TRANS_GROUP
               TG = GET_TRANS_GROUP_INDEX(J)
               IF(TG == 0) CYCLE
               I = GET_DATA_BASE_FOR_TRANS(TG,DAY_TYPE) ! I = DATA_BASE
               IF(I == 0) CYCLE

               IF(.NOT. DETAILED_OUTAGE_DATA_BASE(I)) CYCLE

               IF(   .NOT. ACTIVE_DATA_BASE(I) .OR. &
                              SAVE_OUTAGE_BLOCKS < 1) CYCLE

               COMMITMENT_DB(TG) = 1

               TG = GET_TRANS_GROUP_POSITION(TG)

               YES_OUTAGE_EVENTS_IN_HOUR = &
                                    OUTAGE_EVENTS_IN_HOUR(LOCAL_HOUR,TG)

               ADJUST_BLOCK = 0.

               IF(YES_OUTAGE_EVENTS_IN_HOUR)THEN

                  OUTAGES_ADJUSTED = ADJUST_FOR_OUTAGES( &
                                                LOCAL_HOUR, &
                                                OUTAGE_SINGULAR, &
                                                TG, &
                                                I, &
                                                OUTAGE_TYPE, &
                                                ADJUST_BLOCK, &
                                                DERATE_BLOCK)
               ENDIF

! MODEL ONLY GETS TO THIS POINT IF THE DATABASE IS DEFINED IN
! THIS HOUR.  IF NOT, THE HOURLY STATE WILL REVERT TO ZERO.


               DO TEMP_BLOCK = 1, OUTAGE_BLOCKS_4_DB(I)
! NOTE: DOUBLE INDEX
                  OUTAGE_BLOCK = OUTAGE_BLOCK_N_DB(TEMP_BLOCK,I)
                  BLOCK = OUTAGE_BLOCK_INDEX(OUTAGE_BLOCK)
                  U = OUTAGE_UNIT_INDEX(OUTAGE_BLOCK)

! OUTAGE_BLOCK IN HOURLY_OUTAGE_STATE SHOULD CORRESPOND TO
! THE GLOBAL CONCEPT OF BLOCK. MAYBE IT MUST BE DIMENSIONED
! AS RESOURCES BY TRANSACTION GROUP.

! UPDATE OUTAGE, NOT DERATE

                  IF(YES_OUTAGE_EVENTS_IN_HOUR .AND. &
                                  ADJUST_BLOCK(OUTAGE_BLOCK) == 1.) THEN

                     HOURLY_LAST_STATE(BLOCK) = &
                                           OUTAGE_SINGULAR(OUTAGE_BLOCK)

                     HOURLY_OUTAGE_STATE(BLOCK,LOCAL_HOUR) = &
                                         HOURLY_LAST_STATE(BLOCK) * &
!                                             ATTEMPT TO DO DERATES
                                            HOURLY_LAST_DERATE(BLOCK) * &
                                      TRANS_BLOCK_CAPACITY(OUTAGE_BLOCK)

                     HOURLY_LAST_TYPE(BLOCK) = &
                                               OUTAGE_TYPE(OUTAGE_BLOCK)
                     HOURLY_OUTAGE_TYPE(BLOCK,LOCAL_HOUR) = &
                                               OUTAGE_TYPE(OUTAGE_BLOCK)

! UPDATE DERATE, NOT OUTAGE

                  ELSEIF(YES_OUTAGE_EVENTS_IN_HOUR .AND. &
                                 ADJUST_BLOCK(OUTAGE_BLOCK) == 2.) THEN

                     HOURLY_LAST_DERATE(BLOCK) = &
                                              DERATE_BLOCK(OUTAGE_BLOCK)
                     HOURLY_OUTAGE_STATE(BLOCK,LOCAL_HOUR) = &
                                         HOURLY_LAST_STATE(BLOCK) * &
                                            HOURLY_LAST_DERATE(BLOCK) * &
                                      TRANS_BLOCK_CAPACITY(OUTAGE_BLOCK)
!
                     HOURLY_LAST_TYPE(BLOCK) = &
                                               OUTAGE_TYPE(OUTAGE_BLOCK)
                     HOURLY_OUTAGE_TYPE(BLOCK,LOCAL_HOUR) = &
                                               OUTAGE_TYPE(OUTAGE_BLOCK)
!
! UPDATE OUTAGE AND DERATE
!
                  ELSEIF(YES_OUTAGE_EVENTS_IN_HOUR .AND. &
                                  ADJUST_BLOCK(OUTAGE_BLOCK) > 2.) THEN
!
                     HOURLY_LAST_STATE(BLOCK) = &
                                           OUTAGE_SINGULAR(OUTAGE_BLOCK)
                     HOURLY_LAST_DERATE(BLOCK) = &
                                              DERATE_BLOCK(OUTAGE_BLOCK)
                     HOURLY_OUTAGE_STATE(BLOCK,LOCAL_HOUR) = &
                                         HOURLY_LAST_STATE(BLOCK) * &
                                            HOURLY_LAST_DERATE(BLOCK) * & 
                                      TRANS_BLOCK_CAPACITY(OUTAGE_BLOCK)
!
                     HOURLY_LAST_TYPE(BLOCK) = &
                                               OUTAGE_TYPE(OUTAGE_BLOCK)
                     HOURLY_OUTAGE_TYPE(BLOCK,LOCAL_HOUR) = &
                                               OUTAGE_TYPE(OUTAGE_BLOCK)
                  ELSE
                     HOURLY_OUTAGE_STATE(BLOCK,LOCAL_HOUR) = &
                                              HOURLY_LAST_STATE(BLOCK) * &
                                             HOURLY_LAST_DERATE(BLOCK) * & 
                                      TRANS_BLOCK_CAPACITY(OUTAGE_BLOCK)
!
                     HOURLY_OUTAGE_TYPE(BLOCK,LOCAL_HOUR) = &
                                                 HOURLY_LAST_TYPE(BLOCK)
                  ENDIF
                  CUM_TC_UNIT_MWH(U) = CUM_TC_UNIT_MWH(U) + &
                                   HOURLY_OUTAGE_STATE(BLOCK,LOCAL_HOUR)
               ENDDO ! BLOCK IN OUTAGE DATABASE
!
            ENDDO ! TRANSACTION GROUP
            IF(ECITY) THEN
!
! 11/5/01. ECITIES RESERVE CAPACITY IF TRANSACT OUTAGES
! 02/01/02.
!
               IF(RESERVE_CAP_NO(1) > 0) THEN
                  IF(CAT1_B > 0 .AND. CAT1_U > 0) THEN
                     CAT1_TRANS_EA = &
                                  HOURLY_OUTAGE_STATE(CAT1_B,LOCAL_HOUR)
                  ELSE
                     CAT1_TRANS_EA = 0.
                  ENDIF
                  IF(CAT2_B > 0 .AND. CAT2_U > 0) THEN
                     CAT2_TRANS_EA = &
                                  HOURLY_OUTAGE_STATE(CAT2_B,LOCAL_HOUR)
                  ELSE
                     CAT2_TRANS_EA = 0.
                  ENDIF
                  IF(MCG1_B > 0 .AND. MCG1_U > 0) THEN
                     MCG1_TRANS_EA = &
                                  HOURLY_OUTAGE_STATE(MCG1_B,LOCAL_HOUR)
                  ELSE
                     MCG1_TRANS_EA = 0.
                  ENDIF
                  IF(MCG2_B > 0 .AND. MCG2_U > 0) THEN
                     MCG2_TRANS_EA = &
                                  HOURLY_OUTAGE_STATE(MCG2_B,LOCAL_HOUR)
                  ELSE
                     MCG2_TRANS_EA = 0.
                  ENDIF

                  DUKE_RETAINED_AVAIL(LOCAL_HOUR) = &
                                    CAT1_TRANS_EA + &
                                    CAT2_TRANS_EA + &
                                    MCG1_TRANS_EA + &
                                    MCG2_TRANS_EA

                  RESERVE_CAPACITY(1) = 0.
               ENDIF ! RESERVE CAPACITY CALC

            ENDIF ! ECITIES
         ENDDO ! HOURS IN MONTH

! WRITE HOURLY UNIT AVAILABILITY REPORT (DAYS TYPES/TRANSACT OUTAGE)

      ENDIF ! UNIT COMMITMENT ACTIVE

! ADDED 08/18/03.


      CALL RW_PROCESS_MESSAGES()
!
      RETURN

      ENTRY GET_DUKE_BEFORE_RETAINED()

         GET_DUKE_BEFORE_RETAINED = BEFORE_RETAINED
      RETURN


      ENTRY GET_BLOCK_AVAIL_4_HOUR(R_UNIT,R_BLOCK,R_HOUR)

!
!
         IF(R_UNIT > NUNITS_SAVE .OR. R_BLOCK > NBLOK2_SAVE) THEN
            GET_BLOCK_AVAIL_4_HOUR = 0.
            RETURN
         ENDIF
!
         BLOCK = UNIT_TO_BLOCK(R_UNIT,R_BLOCK)

         IF(BLOCK > 0.AND. BLOCK <= NBLOK2_SAVE) THEN
            GET_BLOCK_AVAIL_4_HOUR = HOURLY_OUTAGE_STATE(BLOCK,R_HOUR)
         ELSE
            GET_BLOCK_AVAIL_4_HOUR = 0.
         ENDIF
      RETURN

      ENTRY GET_BLOCK_OUTAGE_4_HOUR(R_UNIT,R_BLOCK,R_HOUR)

!
!
         BLOCK = UNIT_TO_BLOCK(R_UNIT,R_BLOCK)

         IF(BLOCK > 0 .AND. BLOCK <= NBLOK2_SAVE) THEN
            GET_BLOCK_OUTAGE_4_HOUR = HOURLY_OUTAGE_TYPE(BLOCK,R_HOUR)
         ELSE
            GET_BLOCK_OUTAGE_4_HOUR = 0.
         ENDIF
      RETURN
!
!

      ENTRY YES_HOOSIER()

         YES_HOOSIER = HOOSIER
      RETURN

      ENTRY GET_HOOSIER_INDICES(R_M1_U,R_M2_U,R_R1_U,R_R2_U, &
                                    R_MEROM1_EFFECTIVE_CAP, &
                                    R_MEROM2_EFFECTIVE_CAP, &
                                    R_RATTS1_EFFECTIVE_CAP, &
                                    R_RATTS2_EFFECTIVE_CAP, &
                                    R_WI_U, &
                                    R_PE_U, &
                                    R_DG_U, &
                                    R_BE_U, &
                                    R_WO_U)

!
         R_M1_U = M1_U
         R_M2_U = M2_U
         R_R1_U = R1_U
         R_R2_U = R2_U
!
         R_WI_U = WI_U
         R_PE_U = PE_U
         R_DG_U = DG_U
         R_BE_U = BE_U
         R_WO_U = WO_U
!
         GET_HOOSIER_INDICES = 1
!
         R_MEROM1_EFFECTIVE_CAP = MEROM1_EFFECTIVE_CAP
         R_MEROM2_EFFECTIVE_CAP = MEROM2_EFFECTIVE_CAP
         R_RATTS1_EFFECTIVE_CAP = RATTS1_EFFECTIVE_CAP
         R_RATTS2_EFFECTIVE_CAP = RATTS2_EFFECTIVE_CAP

!
      RETURN
!
!

      ENTRY          GET_BLOCK_AVAIL_4_MONTH(R_UNIT, &
                     R_BLOCK,R_AVAIL,R_OUT,R_DAILY_OUT)

         INT_DAY = SAVE_HOURS_IN_MONTH/24

         BLOCK = UNIT_TO_BLOCK(R_UNIT,R_BLOCK)
         IF(R_BLOCK < 1 .OR. R_BLOCK > 2 .OR. BLOCK > NBLOK2_SAVE) THEN
            BLOCK = BLOCK
         ENDIF



         DO I = 1, INT_DAY
            LOCAL_HOUR = (I-1)*24 + 1
            IF(BLOCK > 0) THEN
               R_AVAIL(LOCAL_HOUR) = &
                                   HOURLY_OUTAGE_STATE(BLOCK,LOCAL_HOUR)
               R_AVAIL(LOCAL_HOUR+ 1) = &
                                HOURLY_OUTAGE_STATE(BLOCK,LOCAL_HOUR+ 1)
               R_AVAIL(LOCAL_HOUR+ 2) = &
                                HOURLY_OUTAGE_STATE(BLOCK,LOCAL_HOUR+ 2)
               R_AVAIL(LOCAL_HOUR+ 3) = &
                                HOURLY_OUTAGE_STATE(BLOCK,LOCAL_HOUR+ 3)
               R_AVAIL(LOCAL_HOUR+ 4) = &
                                HOURLY_OUTAGE_STATE(BLOCK,LOCAL_HOUR+ 4)
               R_AVAIL(LOCAL_HOUR+ 5) = &
                                HOURLY_OUTAGE_STATE(BLOCK,LOCAL_HOUR+ 5)
               R_AVAIL(LOCAL_HOUR+ 6) = &
                                HOURLY_OUTAGE_STATE(BLOCK,LOCAL_HOUR+ 6)
               R_AVAIL(LOCAL_HOUR+ 7) = &
                                HOURLY_OUTAGE_STATE(BLOCK,LOCAL_HOUR+ 7)
               R_AVAIL(LOCAL_HOUR+ 8) = &
                                HOURLY_OUTAGE_STATE(BLOCK,LOCAL_HOUR+ 8)
               R_AVAIL(LOCAL_HOUR+ 9) = &
                                HOURLY_OUTAGE_STATE(BLOCK,LOCAL_HOUR+ 9)
               R_AVAIL(LOCAL_HOUR+10) = &
                                HOURLY_OUTAGE_STATE(BLOCK,LOCAL_HOUR+10)
               R_AVAIL(LOCAL_HOUR+11) = &
                                HOURLY_OUTAGE_STATE(BLOCK,LOCAL_HOUR+11)
               R_AVAIL(LOCAL_HOUR+12) = &
                                HOURLY_OUTAGE_STATE(BLOCK,LOCAL_HOUR+12)
               R_AVAIL(LOCAL_HOUR+13) = &
                                HOURLY_OUTAGE_STATE(BLOCK,LOCAL_HOUR+13)
               R_AVAIL(LOCAL_HOUR+14) = &
                                HOURLY_OUTAGE_STATE(BLOCK,LOCAL_HOUR+14)
               R_AVAIL(LOCAL_HOUR+15) = &
                                HOURLY_OUTAGE_STATE(BLOCK,LOCAL_HOUR+15)
               R_AVAIL(LOCAL_HOUR+16) = &
                                HOURLY_OUTAGE_STATE(BLOCK,LOCAL_HOUR+16)
               R_AVAIL(LOCAL_HOUR+17) = &
                                HOURLY_OUTAGE_STATE(BLOCK,LOCAL_HOUR+17)
               R_AVAIL(LOCAL_HOUR+18) = &
                                HOURLY_OUTAGE_STATE(BLOCK,LOCAL_HOUR+18)
               R_AVAIL(LOCAL_HOUR+19) = &
                                HOURLY_OUTAGE_STATE(BLOCK,LOCAL_HOUR+19)
               R_AVAIL(LOCAL_HOUR+20) = &
                                HOURLY_OUTAGE_STATE(BLOCK,LOCAL_HOUR+20)
               R_AVAIL(LOCAL_HOUR+21) = &
                                HOURLY_OUTAGE_STATE(BLOCK,LOCAL_HOUR+21)
               R_AVAIL(LOCAL_HOUR+22) = &
                                HOURLY_OUTAGE_STATE(BLOCK,LOCAL_HOUR+22)
               R_AVAIL(LOCAL_HOUR+23) = &
                                HOURLY_OUTAGE_STATE(BLOCK,LOCAL_HOUR+23)
!
               R_OUT(LOCAL_HOUR) = &
                                   HOURLY_OUTAGE_TYPE(BLOCK,LOCAL_HOUR)
               R_OUT(LOCAL_HOUR+ 1) = &
                                HOURLY_OUTAGE_TYPE(BLOCK,LOCAL_HOUR+ 1)
               R_OUT(LOCAL_HOUR+ 2) = &
                                HOURLY_OUTAGE_TYPE(BLOCK,LOCAL_HOUR+ 2)
               R_OUT(LOCAL_HOUR+ 3) = &
                                HOURLY_OUTAGE_TYPE(BLOCK,LOCAL_HOUR+ 3)
               R_OUT(LOCAL_HOUR+ 4) = &
                                HOURLY_OUTAGE_TYPE(BLOCK,LOCAL_HOUR+ 4)
               R_OUT(LOCAL_HOUR+ 5) = &
                                HOURLY_OUTAGE_TYPE(BLOCK,LOCAL_HOUR+ 5)
               R_OUT(LOCAL_HOUR+ 6) = &
                                HOURLY_OUTAGE_TYPE(BLOCK,LOCAL_HOUR+ 6)
               R_OUT(LOCAL_HOUR+ 7) = &
                                HOURLY_OUTAGE_TYPE(BLOCK,LOCAL_HOUR+ 7)
               R_OUT(LOCAL_HOUR+ 8) = &
                                HOURLY_OUTAGE_TYPE(BLOCK,LOCAL_HOUR+ 8)
               R_OUT(LOCAL_HOUR+ 9) = &
                                HOURLY_OUTAGE_TYPE(BLOCK,LOCAL_HOUR+ 9)
               R_OUT(LOCAL_HOUR+10) = &
                                HOURLY_OUTAGE_TYPE(BLOCK,LOCAL_HOUR+10)
               R_OUT(LOCAL_HOUR+11) = &
                                HOURLY_OUTAGE_TYPE(BLOCK,LOCAL_HOUR+11)
               R_OUT(LOCAL_HOUR+12) = &
                                HOURLY_OUTAGE_TYPE(BLOCK,LOCAL_HOUR+12)
               R_OUT(LOCAL_HOUR+13) = &
                                HOURLY_OUTAGE_TYPE(BLOCK,LOCAL_HOUR+13)
               R_OUT(LOCAL_HOUR+14) = &
                                HOURLY_OUTAGE_TYPE(BLOCK,LOCAL_HOUR+14)
               R_OUT(LOCAL_HOUR+15) = &
                                HOURLY_OUTAGE_TYPE(BLOCK,LOCAL_HOUR+15)
               R_OUT(LOCAL_HOUR+16) = &
                                HOURLY_OUTAGE_TYPE(BLOCK,LOCAL_HOUR+16)
               R_OUT(LOCAL_HOUR+17) = &
                                HOURLY_OUTAGE_TYPE(BLOCK,LOCAL_HOUR+17)
               R_OUT(LOCAL_HOUR+18) = &
                                HOURLY_OUTAGE_TYPE(BLOCK,LOCAL_HOUR+18)
               R_OUT(LOCAL_HOUR+19) = &
                                HOURLY_OUTAGE_TYPE(BLOCK,LOCAL_HOUR+19)
               R_OUT(LOCAL_HOUR+20) = &
                                HOURLY_OUTAGE_TYPE(BLOCK,LOCAL_HOUR+20)
               R_OUT(LOCAL_HOUR+21) = &
                                HOURLY_OUTAGE_TYPE(BLOCK,LOCAL_HOUR+21)
               R_OUT(LOCAL_HOUR+22) = &
                                HOURLY_OUTAGE_TYPE(BLOCK,LOCAL_HOUR+22)
               R_OUT(LOCAL_HOUR+23) = &
                                HOURLY_OUTAGE_TYPE(BLOCK,LOCAL_HOUR+23)
!
               R_DAILY_OUT(I) = HOURLY_OUTAGE_TYPE(BLOCK,LOCAL_HOUR)+ &
                                HOURLY_OUTAGE_TYPE(BLOCK,LOCAL_HOUR+ 1)+ &
                                HOURLY_OUTAGE_TYPE(BLOCK,LOCAL_HOUR+ 2)+ &
                                HOURLY_OUTAGE_TYPE(BLOCK,LOCAL_HOUR+ 3)+ &
                                HOURLY_OUTAGE_TYPE(BLOCK,LOCAL_HOUR+ 4)+ &
                                HOURLY_OUTAGE_TYPE(BLOCK,LOCAL_HOUR+ 5)+ &
                                HOURLY_OUTAGE_TYPE(BLOCK,LOCAL_HOUR+ 6)+ &
                                HOURLY_OUTAGE_TYPE(BLOCK,LOCAL_HOUR+ 7)+ &
                                HOURLY_OUTAGE_TYPE(BLOCK,LOCAL_HOUR+ 8)+ &
                                HOURLY_OUTAGE_TYPE(BLOCK,LOCAL_HOUR+ 9)+ &
                                HOURLY_OUTAGE_TYPE(BLOCK,LOCAL_HOUR+10)+ &
                                HOURLY_OUTAGE_TYPE(BLOCK,LOCAL_HOUR+11)+ &
                                HOURLY_OUTAGE_TYPE(BLOCK,LOCAL_HOUR+12)+ &
                                HOURLY_OUTAGE_TYPE(BLOCK,LOCAL_HOUR+13)+ &
                                HOURLY_OUTAGE_TYPE(BLOCK,LOCAL_HOUR+14)+ &
                                HOURLY_OUTAGE_TYPE(BLOCK,LOCAL_HOUR+15)+ &
                                HOURLY_OUTAGE_TYPE(BLOCK,LOCAL_HOUR+16)+ &
                                HOURLY_OUTAGE_TYPE(BLOCK,LOCAL_HOUR+17)+ &
                                HOURLY_OUTAGE_TYPE(BLOCK,LOCAL_HOUR+18)+ &
                                HOURLY_OUTAGE_TYPE(BLOCK,LOCAL_HOUR+19)+ &
                                HOURLY_OUTAGE_TYPE(BLOCK,LOCAL_HOUR+20)+ &
                                HOURLY_OUTAGE_TYPE(BLOCK,LOCAL_HOUR+21)+ &
                                HOURLY_OUTAGE_TYPE(BLOCK,LOCAL_HOUR+22)+ &
                                HOURLY_OUTAGE_TYPE(BLOCK,LOCAL_HOUR+23)
!
            ELSE
               R_AVAIL(LOCAL_HOUR) = 0.
               R_AVAIL(LOCAL_HOUR+ 1) = 0.
               R_AVAIL(LOCAL_HOUR+ 2) = 0.
               R_AVAIL(LOCAL_HOUR+ 3) = 0.
               R_AVAIL(LOCAL_HOUR+ 4) = 0.
               R_AVAIL(LOCAL_HOUR+ 5) = 0.
               R_AVAIL(LOCAL_HOUR+ 6) = 0.
               R_AVAIL(LOCAL_HOUR+ 7) = 0.
               R_AVAIL(LOCAL_HOUR+ 8) = 0.
               R_AVAIL(LOCAL_HOUR+ 9) = 0.
               R_AVAIL(LOCAL_HOUR+10) = 0.
               R_AVAIL(LOCAL_HOUR+11) = 0.
               R_AVAIL(LOCAL_HOUR+12) = 0.
               R_AVAIL(LOCAL_HOUR+13) = 0.
               R_AVAIL(LOCAL_HOUR+14) = 0.
               R_AVAIL(LOCAL_HOUR+15) = 0.
               R_AVAIL(LOCAL_HOUR+16) = 0.
               R_AVAIL(LOCAL_HOUR+17) = 0.
               R_AVAIL(LOCAL_HOUR+18) = 0.
               R_AVAIL(LOCAL_HOUR+19) = 0.
               R_AVAIL(LOCAL_HOUR+20) = 0.
               R_AVAIL(LOCAL_HOUR+21) = 0.
               R_AVAIL(LOCAL_HOUR+22) = 0.
               R_AVAIL(LOCAL_HOUR+23) = 0.
!
               R_OUT(LOCAL_HOUR) = 0.
               R_OUT(LOCAL_HOUR+ 1) = 0.
               R_OUT(LOCAL_HOUR+ 2) = 0.
               R_OUT(LOCAL_HOUR+ 3) = 0.
               R_OUT(LOCAL_HOUR+ 4) = 0.
               R_OUT(LOCAL_HOUR+ 5) = 0.
               R_OUT(LOCAL_HOUR+ 6) = 0.
               R_OUT(LOCAL_HOUR+ 7) = 0.
               R_OUT(LOCAL_HOUR+ 8) = 0.
               R_OUT(LOCAL_HOUR+ 9) = 0.
               R_OUT(LOCAL_HOUR+10) = 0.
               R_OUT(LOCAL_HOUR+11) = 0.
               R_OUT(LOCAL_HOUR+12) = 0.
               R_OUT(LOCAL_HOUR+13) = 0.
               R_OUT(LOCAL_HOUR+14) = 0.
               R_OUT(LOCAL_HOUR+15) = 0.
               R_OUT(LOCAL_HOUR+16) = 0.
               R_OUT(LOCAL_HOUR+17) = 0.
               R_OUT(LOCAL_HOUR+18) = 0.
               R_OUT(LOCAL_HOUR+19) = 0.
               R_OUT(LOCAL_HOUR+20) = 0.
               R_OUT(LOCAL_HOUR+21) = 0.
               R_OUT(LOCAL_HOUR+22) = 0.
               R_OUT(LOCAL_HOUR+23) = 0.
!
               R_DAILY_OUT(I) = 0.
            ENDIF
         ENDDO
         GET_BLOCK_AVAIL_4_MONTH = INT_DAY
!
      RETURN

      ENTRY YES_ECITIES_UNITS_ACTIVE()

!
         YES_ECITIES_UNITS_ACTIVE = ECITY
!
      RETURN
!
!


      ENTRY GET_DUKE_HOURLY_RETAINED(R_HOUR)

!
         GET_DUKE_HOURLY_RETAINED = DUKE_RETAINED_AVAIL(R_HOUR)
!
      RETURN

      ENTRY GET_DUKE_RETAINED_CAPACITY()

!
         GET_DUKE_RETAINED_CAPACITY = RETAINED_AVAILABLE(1)
!
      RETURN

      ENTRY GET_DUKE_RESERVE_CAP_NO()

!
         GET_DUKE_RESERVE_CAP_NO = RESERVE_CAP_NO(1)
!
      RETURN


      ENTRY GET_CPL_RESERVE_CAPACITY()

!
         GET_CPL_RESERVE_CAPACITY = RESERVE_CAPACITY(2)
!
      RETURN

      ENTRY GET_DUKE_SUP_CAPACITY()

!
         GET_DUKE_SUP_CAPACITY = SUPPLEMENTAL_CAPACITY(1)
!
      RETURN

      ENTRY GET_CPL_SUP_CAPACITY()

!
         GET_CPL_SUP_CAPACITY = SUPPLEMENTAL_CAPACITY(2)
!
      RETURN
!

      ENTRY GET_NO_START_UP_UNITS(R_TG)

!
!
         GET_NO_START_UP_UNITS = START_UP_UNIT_BY_TG(R_TG)
!
      RETURN
!

      ENTRY GET_START_UP_POSITION(R_UNIT,R_TG)

!

         GET_START_UP_POSITION = START_UP_POSITION(R_UNIT,R_TG)

         IF(GET_START_UP_POSITION == 0) THEN
            WRITE(4,*) "ILLEGAL START-UP UNIT"
            WRITE(4,*) '*** line 2909 MARGNOBJ.FOR ***'
            er_message='See WARNING MESSAGES -MARGNOBJ.FOR-4'
            call end_program(er_message)
         ENDIF
!
      RETURN
!

      ENTRY GET_THOSE_MARGINAL_COSTS(R_DX,R_MARGINAL_COST)

!
!
         R_DX = DB_DX(1)
         DO I = 0, 1000
            R_MARGINAL_COST(I) = MARGINAL_COST(I)
         ENDDO
!
         GET_THOSE_MARGINAL_COSTS = .TRUE.
      RETURN

      ENTRY TRANS_GROUP_CAP(R_DATA_BASES)

!
!
         IF(R_DATA_BASES > 0 .AND. ACTIVE_DATA_BASE(R_DATA_BASES)) THEN

            TRANS_GROUP_CAP = CAPACITY_GIVEN_MARKET( &
                               LAST_DB_POINT(R_DATA_BASES),R_DATA_BASES)
         ELSE
            TRANS_GROUP_CAP = 0.D0
         ENDIF
      RETURN


      ENTRY TRANS_GROUP_GEN_ACTIVE(R_DATA_BASES)

         IF(R_DATA_BASES > 0 .AND. &
                  R_DATA_BASES <= MAX_DATA_BASES .AND. &
                                   LAST_DB_POINT(R_DATA_BASES) > 0) THEN

            TRANS_GROUP_GEN_ACTIVE = ACTIVE_DATA_BASE(R_DATA_BASES)
         ELSE
            TRANS_GROUP_GEN_ACTIVE = .FALSE.
         ENDIF
      RETURN

      ENTRY WRITE_CONTRIBUTION_REPORT( &
                                    R_HOUR, &
                                    R_TRANS_GROUP, &
                                    R_DATA_BASES, &
                                    R_TRANS_NAME, &
                                    R_DELIVERED_PRICE, &
                                    R_SELLER_TRANS, &
                                    R_SELLER_DATA_BASES, &
                                    R_HOURLY_MARGINAL_FUEL_PRICE, &
                                    R_FT)

!
! CALCULATE FOR AVERAGE HEATRATE BY TG
!
!

         if(file_trace_wcr==0) then
           file_trace_wcr=open_trace("WRITE_CONTRIBUTION_REPORT.trace", &
            rq_wcr)
         end if

         call write_trace_int2s(file_trace_wcr, "POS", POS)


         IF(R_SELLER_TRANS /= R_TRANS_GROUP) THEN ! BUYER
            MPOS = MAX(INT(1,2),POS(R_SELLER_DATA_BASES))
            ! call write_trace_int2(file_trace_wcr, "MPOS", MPOS)
            NPC = &
                  NEW_BLOCK_NUMBER(MPOS,CONTRIB_INTERVALS, &
                                                    R_SELLER_DATA_BASES)
            ! call write_trace_int2(file_trace_wcr, "NPC", NPC)
            
            BLOCK_POSITION = &
                  NEW_BLOCK_NUMBER(MPOS,NPC,R_SELLER_DATA_BASES)
            call write_trace_int2(file_trace_wcr, "BLOCK_POSITION", &
                BLOCK_POSITION)
                
         ELSE                                     ! SELLER
            MPOS = MAX(INT(1,2),POS(R_DATA_BASES))
            ! call write_trace_int2(file_trace_wcr, "2. MPOS", MPOS)
            NPC = &
                  NEW_BLOCK_NUMBER(MPOS,CONTRIB_INTERVALS,R_DATA_BASES)
            ! call write_trace_int2(file_trace_wcr, "2. NPC", NPC)
            
            ! mpos=68/22
            ! NPC=1/1
            !r_data_bases=2/2            
            BLOCK_POSITION = &
                  NEW_BLOCK_NUMBER(MPOS,NPC,R_DATA_BASES)
            call write_trace_int2(file_trace_wcr, "2. BLOCK_POSITION", &
                BLOCK_POSITION)
                
         ENDIF
!
         IF(BLOCK_POSITION <= 0) THEN
            WRITE(4,*) "IN MARGINAL UNIT REPORT, BLOCK POSITION"
            WRITE(4,*) "WAS DRIVEN TO  ",BLOCK_POSITION
            BLOCK_POSITION = 1
         ENDIF
! 051608.
         U = UNIT_SAVE(BLOCK_POSITION)
         call write_trace_int2(file_trace_wcr, "2. U", U)

         FT =  SAVE_PRIMARY_MOVER(U)
         call write_trace_int2(file_trace_wcr, "FT", FT)
         
         FT = MAX(INT(0,2),MIN(FT,MAX_FUEL_TYPES))
         ! call write_trace_int2(file_trace_wcr, "2. FT", FT)
         
         IF(FT == 2) THEN
            R_FT = 1.0
         ELSE
            R_FT = 0.0
         ENDIF
         ! call write_trace_real4(file_trace_wcr, "R_FT", R_FT)
         
!
         B = MAX(INT(1,2),BLKNO_SAVE(BLOCK_POSITION))
         ! call write_trace_int2(file_trace_wcr, "B", B)
!
         ! call write_trace_real(file_trace_wcr, "INCREMENTAL_HEATRATE", &
            ! INCREMENTAL_HEATRATE)
         call write_trace_int2(file_trace_wcr, "2. BLOCK_POSITION", &
            BLOCK_POSITION)

         CALL GET_INHEAT_AND_FUEL(INCREMENTAL_HEATRATE, &
                                                      BLOCK_POSITION)
         INCREMENTAL_FUEL_PRICE = &
                     UNIT_DISP_COST(U,B)/10.
      ! call write_trace_real(file_trace_wcr, "2. INCREMENTAL_FUEL_PRICE", &
            ! INCREMENTAL_FUEL_PRICE)
            
! 092310. FOR JEAN AND FALL 10 REF CASE.
!
         IF(YES_MARGINAL_UNIT_ACTIVE) THEN
            ! call write_trace_int2(file_trace_wcr, "U", U)
            
            GET_PRIMARY_MOVER_INDEX_called_from="margnobj:0011"
            PM = GET_PRIMARY_MOVER_INDEX(U)
            ! call write_trace_int2(file_trace_wcr, "1. PM", PM)
            
            max_firstelem=ubound(MARGINAL_UNIT_BY_TECH,1)
            max_secondelem=ubound(MARGINAL_UNIT_BY_TECH,2)
            if(max_firstelem<R_TRANS_GROUP .or. &
                max_secondelem<PM) then
                
                er_message="marngnobj:0001 - " // &
                "An attempt is being made to access array bounds in " // &
                "marginal_unit_by_tech, which has these bounds: (" // &
                trim(itos(max_firstelem)) // "," // &
                trim (itos(max_secondelem)) // ") ." // &
                "Attempt was made to access (" // &
                trim(itos(int(R_TRANS_GROUP))) // "," // &
                trim(itos(int(PM))) // ")"
                
                call end_program(er_message)
            end if
            MARGINAL_UNIT_BY_TECH(R_TRANS_GROUP,PM) = &
                             MARGINAL_UNIT_BY_TECH(R_TRANS_GROUP,PM) + 1
            IF(PM == 10) THEN
               LM = MIN(PM + FT + 4,MAX_PROD_TYPES)
               MARGINAL_UNIT_BY_TECH(R_TRANS_GROUP,LM) = &
                             MARGINAL_UNIT_BY_TECH(R_TRANS_GROUP,LM) + 1
            ENDIF
            ! Ask Greg - what's Marginal_unit_by_tech?
            MARGINAL_UNIT_BY_TECH(R_TRANS_GROUP,14) = &
                             MARGINAL_UNIT_BY_TECH(R_TRANS_GROUP,14) + 1
         ENDIF

         R_HOURLY_MARGINAL_FUEL_PRICE = INCREMENTAL_FUEL_PRICE

         IF(YES_MARGINAL_UNIT_REPORT) THEN

             TRANS_CON_REC = RPTREC(TRANS_CON_UNIT)
             WRITE(TRANS_CON_UNIT,REC=TRANS_CON_REC) &
                     PRT_ENDPOINT(), &
                     FLOAT(SAVE_YEAR), &
                     CL_MONTH_NAME(SAVE_SEASON), &
                     FLOAT(R_HOUR), &
                     R_TRANS_NAME, &
                     FLOAT(BLOCK_POSITION), &
                     FLOAT(U), &
                     FLOAT(B), &
                     INCREMENTAL_HEATRATE, &
                     INCREMENTAL_FUEL_PRICE
            TRANS_CON_REC = TRANS_CON_REC + 1

         ENDIF
         WRITE_CONTRIBUTION_REPORT = INCREMENTAL_HEATRATE

      RETURN



      ENTRY WRITE_MARGINAL_UNIT_REPORT()

         WRITE_MARGINAL_UNIT_REPORT = YES_MARGINAL_UNIT_ACTIVE
         IF(YES_MARGINAL_UNIT_ACTIVE .AND. &
                                  ALLOCATED(MARGINAL_UNIT_BY_TECH)) THEN
            DO I = 1, SAVE_UPPER_TRANS_GROUP
               LOCAL_GROUP_NAME = GET_GROUP_NAME(I)
               MARGINAL_UNIT_REC = RPTREC(MARGINAL_UNIT_TECH_NO)
               IF(MARGINAL_UNIT_BY_TECH(I,14) > 0.0) THEN
                  DO J = 1, MAX_PROD_TYPES
                     IF(J == 14) CYCLE
                     MARGINAL_UNIT_BY_TECH(I,J) = &
                                 100.0* MARGINAL_UNIT_BY_TECH(I,J) / &
                                             MARGINAL_UNIT_BY_TECH(I,14)
                  END DO
                  MARGINAL_UNIT_BY_TECH(I,14) = &
                                 100.0* MARGINAL_UNIT_BY_TECH(I,14) / &
                                             MARGINAL_UNIT_BY_TECH(I,14)
               ENDIF
               if(MARGINAL_UNIT_TECH_NO<0) then
                stop "MARGINAL_UNIT_TECH_NO"
               end if
               
               WRITE(MARGINAL_UNIT_TECH_NO,REC=MARGINAL_UNIT_REC) &
                     PRT_ENDPOINT(), &
                     FLOAT(SAVE_YEAR), &
                     CL_MONTH_NAME(SAVE_SEASON), &
                     LOCAL_GROUP_NAME, &
                     (MARGINAL_UNIT_BY_TECH(I,J),J= 1, MAX_PROD_TYPES)
               MARGINAL_UNIT_REC = MARGINAL_UNIT_REC + 1
            END DO
         ENDIF
      RETURN

      ENTRY GET_PRODUCTION_LEVEL(R_MARGIN_COST,R_TRANS_GROUP, &
                                             R_MARKET_PRICE)

!
         HALF_POINT = SAVE_HALF_POINT
         TEMP_MARGIN_COST = &
                          SYSTEM_MARGINAL_COST(HALF_POINT,R_TRANS_GROUP, &
                                                         R_MARKET_PRICE)
         IF(R_MARGIN_COST > TEMP_MARGIN_COST) THEN
            start_val = HALF_POINT
            end_val = TRANS_GROUP_POINTS
            POINT_NOT_FOUND = .TRUE.
         ELSEIF(R_MARGIN_COST < TEMP_MARGIN_COST) THEN
            start_val = 1
            END_val = HALF_POINT
            POINT_NOT_FOUND = .TRUE.
         ELSE
            POINT_NOT_FOUND = .FALSE.
         ENDIF

         DO WHILE (POINT_NOT_FOUND)
            HALF_POINT = start_val + (END_val-start_val)/2
            IF(HALF_POINT == start_val) THEN


               EXIT
            ENDIF
            TEMP_MARGIN_COST = &
                          SYSTEM_MARGINAL_COST(HALF_POINT,R_TRANS_GROUP, &
                                                         R_MARKET_PRICE)
            IF(R_MARGIN_COST > TEMP_MARGIN_COST) THEN
               start_val = HALF_POINT
            ELSEIF(R_MARGIN_COST < TEMP_MARGIN_COST) THEN
               END_val = HALF_POINT
            ELSE
               EXIT
            ENDIF
         ENDDO
! Newton's Method (said by authors to converge 'quadratically' fast on a
! solution in the continuous domain, which we do not have here):
!      Start     =Trans_Group_Points/4 ! arbitrarily chosen
!      Half_Point=Trans_Group_Points/2 ! arbitrarily chosen, distinct from Start
!      BegError=R_Margin_Cost-System_Marginal_Cost(Start,
!     +   R_TRANS_GROUP,R_MARKET_PRICE)
!      EndError=R_Margin_Cost-System_Marginal_Cost(Half_Point,
!     +   R_TRANS_GROUP,R_MARKET_PRICE)
!      Slope=(EndError-BegError)/float(Half_Point-Start)
!      do ! search for zero error w.r.t. target R_Margin_Cost
!        xError=EndError/Slope
!        Distant=iabs(Half_Point-Start)>1
!        Start=Half_Point
!        Half_Point=Start-nint(xError) ! abs(xError)<0.5 may cause early exit
!        RootFound=Half_Point==Start
!        if(RootFound .and. Distant) then
!        ! perturb Half_Point, as Slope may not be appropriate
!          if     (xError<0.0) then
!            Half_Point=Start+1
!          else if(xError>0.0) then
!            Half_Point=Start-1
!          end if
!        end if
!        if     (Half_Point<1) then ! enforce domain restrictions
!          Half_Point=1
!        else if(Half_Point>Trans_Group_Points) then
!          Half_Point=Trans_Group_Points
!        end if
!        if(Half_Point==Start) exit ! no further resolution is possible
!        ! recompute Slope at the latest Half_Point
!        BegError=EndError
!        EndError=R_Margin_Cost-System_Marginal_Cost(Half_Point,
!     +     R_TRANS_GROUP,R_MARKET_PRICE)
!        Slope=(EndError-BegError)/float(Half_Point-Start)
!      end do
! (end of suggested replacement of binary search with Newton's Method)
         IF(HALF_POINT <= 1) THEN
            GET_PRODUCTION_LEVEL = CUM_MUST_RUN_MW(R_TRANS_GROUP)
         ELSEIF(HALF_POINT >= TRANS_GROUP_POINTS) THEN
            GET_PRODUCTION_LEVEL =NEW_LODDUR( &
                                       TRANS_GROUP_POINTS,R_TRANS_GROUP)
            IF(GET_PRODUCTION_LEVEL > &
                                  CUM_CAP(R_TRANS_GROUP)) THEN
               GET_PRODUCTION_LEVEL = CUM_CAP(R_TRANS_GROUP)
            ENDIF
         ELSE
            TEMP_MARGIN_COST = &
                          SYSTEM_MARGINAL_COST(HALF_POINT,R_TRANS_GROUP, &
                                                         R_MARKET_PRICE)
            GET_PRODUCTION_LEVEL = NEW_LODDUR(HALF_POINT,R_TRANS_GROUP)
!
            TOP_CAP_MARGIN_COST = &
                      SYSTEM_MARGINAL_COST(HALF_POINT+INT(1,2), &
                                                       R_TRANS_GROUP, &
                                                       R_MARKET_PRICE)
            IF(TOP_CAP_MARGIN_COST > TEMP_MARGIN_COST) THEN
               TOP_CAP_PERCENT = &
                  (R_MARGIN_COST-TEMP_MARGIN_COST)/ &
                  ( TOP_CAP_MARGIN_COST - TEMP_MARGIN_COST)
            ELSE
               TOP_CAP_PERCENT = 0.
            ENDIF
            GET_TOP_CAP_PRODUCTION_LEVEL = &
                                  NEW_LODDUR(HALF_POINT+INT(1,2), &
                                                          R_TRANS_GROUP)
            GET_PRODUCTION_LEVEL = &
                     (1.-TOP_CAP_PERCENT) * GET_PRODUCTION_LEVEL + &
                          TOP_CAP_PERCENT * GET_TOP_CAP_PRODUCTION_LEVEL
!
            IF(GET_PRODUCTION_LEVEL < &
                                    CUM_MUST_RUN_MW(R_TRANS_GROUP)) THEN
               GET_PRODUCTION_LEVEL = CUM_MUST_RUN_MW(R_TRANS_GROUP)
            ENDIF
            IF(GET_PRODUCTION_LEVEL > CUM_CAP(R_TRANS_GROUP)) THEN
               GET_PRODUCTION_LEVEL = CUM_CAP(R_TRANS_GROUP)
            ENDIF
         ENDIF
      RETURN

      ENTRY GET_PRODUCTION_LEVEL_AT_MARKET(R_MARKET_PRICE,R_DATA_BASES, &
                                           L_M)

!
         start_val=INT(MIN(31999.,R_MARKET_PRICE*10.),2)
         SPEED_INDEX = .FALSE.

            HALF_POINT = SAVE_HALF_POINT

            TEMP_MARGIN_COST = &
                   TRANSITION_MARGINAL_COST(HALF_POINT,R_DATA_BASES,L_M)
!
            IF(L_M < 0 .OR. L_M > 5) THEN
               WRITE(4,*) "BAD PRODUCTS INDEX ",L_M
               er_message='Stop requested from MARGNOBJ SIID189'
               call end_program(er_message)
            ELSEIF(HALF_POINT < 1 .OR. HALF_POINT > 275) THEN
               WRITE(4,*) "BAD COST POINT INDEX ",HALF_POINT
               er_message='Stop requested from MARGNOBJ SIID190'
               call end_program(er_message)
            ELSEIF(R_DATA_BASES < 1 .OR. &
                                     R_DATA_BASES > MAX_DATA_BASES) THEN
               WRITE(4,*) "BAD DATABASE INDEX ",R_DATA_BASES
               er_message='Stop requested from MARGNOBJ SIID191'
               er_message=trim(er_message) // " R_DATA_BASES=" // &
                trim(itos(int(r_data_bases))) // " MAX_DATA_BASES=" // &
                trim(itos(int(MAX_DATA_BASES)))
               call end_program(er_message)
            ENDIF
            IF(R_MARKET_PRICE > &
               TRANSITION_MARGINAL_COST(LAST_DB_POINT(R_DATA_BASES), &
                                                 R_DATA_BASES,L_M)) THEN
               HALF_POINT = LAST_DB_POINT(R_DATA_BASES)
               POINT_NOT_FOUND = .FALSE.
            ELSEIF(R_MARKET_PRICE < &
                      TRANSITION_MARGINAL_COST(1,R_DATA_BASES,L_M)) THEN
               HALF_POINT = 0
               POINT_NOT_FOUND = .FALSE.
            ELSEIF(R_MARKET_PRICE > TEMP_MARGIN_COST) THEN
               START_val = HALF_POINT
               END_val = LAST_DB_POINT(R_DATA_BASES)
               POINT_NOT_FOUND = .TRUE.
            ELSEIF(R_MARKET_PRICE < TEMP_MARGIN_COST) THEN
               start_val = 1
               END_val = HALF_POINT
               POINT_NOT_FOUND = .TRUE.
            ELSE
               POINT_NOT_FOUND = .FALSE.
            ENDIF
!         ENDIF

         DO WHILE (POINT_NOT_FOUND)
            HALF_POINT = start_val + (END_val-start_val)/2
            IF(HALF_POINT == start_val) THEN
               EXIT
            ENDIF

            TEMP_MARGIN_COST = &
                   TRANSITION_MARGINAL_COST(HALF_POINT,R_DATA_BASES,L_M)
            IF(R_MARKET_PRICE > TEMP_MARGIN_COST) THEN
               start_val = HALF_POINT
            ELSEIF(R_MARKET_PRICE < TEMP_MARGIN_COST) THEN
               END_val= HALF_POINT
            ELSE
               EXIT
            ENDIF
         ENDDO
         IF(HALF_POINT <= 1) THEN
            GET_PRODUCTION_LEVEL_AT_MARKET = &
                                           CUM_MUST_RUN_MW(R_DATA_BASES)
            HALF_POINT = CUM_MUST_RUN_POSITION(R_DATA_BASES)
         ELSEIF(HALF_POINT >= LAST_DB_POINT(R_DATA_BASES)) THEN
            GET_PRODUCTION_LEVEL_AT_MARKET = &
                        CAPACITY_GIVEN_MARKET( &
                               LAST_DB_POINT(R_DATA_BASES),R_DATA_BASES)
            IF(GET_PRODUCTION_LEVEL_AT_MARKET > &
                   CAPACITY_GIVEN_MARKET( &
                         LAST_DB_POINT(R_DATA_BASES),R_DATA_BASES)) THEN
               GET_PRODUCTION_LEVEL_AT_MARKET = CAPACITY_GIVEN_MARKET( &
                               LAST_DB_POINT(R_DATA_BASES),R_DATA_BASES)
            ENDIF
         ELSE

            TEMP_MARGIN_COST = &
                   TRANSITION_MARGINAL_COST(HALF_POINT,R_DATA_BASES,L_M)
            GET_PRODUCTION_LEVEL_AT_MARKET = &
                          CAPACITY_GIVEN_MARKET(HALF_POINT,R_DATA_BASES)
!

            TOP_CAP_MARGIN_COST = &
                 TRANSITION_MARGINAL_COST(HALF_POINT+1,R_DATA_BASES,L_M)
            IF(TOP_CAP_MARGIN_COST > TEMP_MARGIN_COST) THEN
               TOP_CAP_PERCENT = &
                  (R_MARKET_PRICE-TEMP_MARGIN_COST)/ &
                  ( TOP_CAP_MARGIN_COST - TEMP_MARGIN_COST)
            ELSE
               TOP_CAP_PERCENT = 0.
            ENDIF
            GET_TOP_CAP_PRODUCTION_LEVEL = &
                        CAPACITY_GIVEN_MARKET(HALF_POINT+1,R_DATA_BASES)
            GET_PRODUCTION_LEVEL_AT_MARKET = &
                 (1.-TOP_CAP_PERCENT) * GET_PRODUCTION_LEVEL_AT_MARKET + &
                     TOP_CAP_PERCENT  * GET_TOP_CAP_PRODUCTION_LEVEL
!
            IF(GET_PRODUCTION_LEVEL_AT_MARKET < &
                                    CUM_MUST_RUN_MW(R_DATA_BASES)) THEN
               GET_PRODUCTION_LEVEL_AT_MARKET = &
                                           CUM_MUST_RUN_MW(R_DATA_BASES)
               HALF_POINT = CUM_MUST_RUN_POSITION(R_DATA_BASES)
            ENDIF

            IF(GET_PRODUCTION_LEVEL_AT_MARKET > CAPACITY_GIVEN_MARKET( &
                         LAST_DB_POINT(R_DATA_BASES),R_DATA_BASES)) THEN
               GET_PRODUCTION_LEVEL_AT_MARKET = &
                        CAPACITY_GIVEN_MARKET( &
                               LAST_DB_POINT(R_DATA_BASES),R_DATA_BASES)
            ENDIF
         ENDIF
         IF(HALF_POINT >= 1) THEN
            IF(GET_PRODUCTION_LEVEL_AT_MARKET == &
                             REAL(CUM_MUST_RUN_MW(R_DATA_BASES),8)) THEN
               SUM_PRODUCTION_SALES = SUM_PRODUCTION_SALES + &
                                          GET_PRODUCTION_LEVEL_AT_MARKET
            ELSEIF (HALF_POINT /= LAST_DB_POINT(R_DATA_BASES)) THEN
               SUM_PRODUCTION_SALES = SUM_PRODUCTION_SALES + &
                        (1.-TOP_CAP_PERCENT)* &
                        CAPACITY_GIVEN_MARKET(HALF_POINT,R_DATA_BASES) + &
                        TOP_CAP_PERCENT* &
                        CAPACITY_GIVEN_MARKET(HALF_POINT+1,R_DATA_BASES)

            ELSE
               SUM_PRODUCTION_SALES = SUM_PRODUCTION_SALES + &
                      CAPACITY_GIVEN_MARKET(LAST_DB_POINT(R_DATA_BASES), &
                                                           R_DATA_BASES)
            ENDIF
!
         ENDIF


         POS(R_DATA_BASES) = HALF_POINT
         call write_trace_pos("margnobj:0012", POS(R_DATA_BASES))
         SAVE_LOAD(R_DATA_BASES) = GET_PRODUCTION_LEVEL_AT_MARKET
      RETURN

      ENTRY GET_CAPPED_PRICE(R_DATA_BASES)

         GET_CAPPED_PRICE = &
                  TRANSITION_MARGINAL_COST( &
                           LAST_DB_POINT(R_DATA_BASES),R_DATA_BASES,S_M)
      RETURN

      ENTRY GET_MARGINAL_COST(R_LOAD,R_TRANS_GROUP,R_MARGIN_COST)

!
         HALF_POINT = SAVE_HALF_POINT
         SAVE_LOAD(R_TRANS_GROUP) = R_LOAD
         IF(sngl(SAVE_LOAD(R_TRANS_GROUP)) > &
                              NEW_LODDUR(HALF_POINT,R_TRANS_GROUP)) THEN
            start_val = HALF_POINT
            END_val = TRANS_GROUP_POINTS
            POINT_NOT_FOUND = .TRUE.
         ELSEIF(sngl(SAVE_LOAD(R_TRANS_GROUP)) < &
                              NEW_LODDUR(HALF_POINT,R_TRANS_GROUP)) THEN
            start_val = 1
            END_val = HALF_POINT
            POINT_NOT_FOUND = .TRUE.
         ELSE
            POINT_NOT_FOUND = .FALSE.
         ENDIF
!
         DO WHILE (POINT_NOT_FOUND)
            HALF_POINT = start_val + (end_val -start_val)/2
            IF(HALF_POINT == start_val) EXIT
            IF(sngl(SAVE_LOAD(R_TRANS_GROUP)) > &
                              NEW_LODDUR(HALF_POINT,R_TRANS_GROUP)) THEN
               start_val = HALF_POINT
            ELSEIF(sngl(SAVE_LOAD(R_TRANS_GROUP)) < &
                              NEW_LODDUR(HALF_POINT,R_TRANS_GROUP)) THEN
               end_val = HALF_POINT
            ELSE
               EXIT
            ENDIF
         ENDDO
         POS(R_TRANS_GROUP) = HALF_POINT
         call write_trace_pos("margnobj:0013", POS(R_TRANS_GROUP))
!
         IF(HALF_POINT > TRANS_GROUP_POINTS) THEN
            POS(R_TRANS_GROUP) = TRANS_GROUP_POINTS + 1
            call write_trace_pos("margnobj:0014", POS(R_TRANS_GROUP))
            GET_MARGINAL_COST = LAST_BLOCK_COST
         ELSE
            IF(HALF_POINT == TRANS_GROUP_POINTS) THEN
               TOP_CAP_PERCENT = 0.0
            ELSE
               TOP_CAP_PERCENT = &
                  (SAVE_LOAD(R_TRANS_GROUP) - &
                            NEW_LODDUR(HALF_POINT  ,R_TRANS_GROUP))/ &
                           (NEW_LODDUR(HALF_POINT+1,R_TRANS_GROUP) - &
                            NEW_LODDUR(HALF_POINT  ,R_TRANS_GROUP))
            ENDIF
            GET_MARGINAL_COST = &
                  SYSTEM_MARGINAL_COST(POS(R_TRANS_GROUP),R_TRANS_GROUP, &
                                                          R_MARGIN_COST)
            IF(TOP_CAP_PERCENT > 0.) THEN
               GET_TOP_CAP_MARGINAL_COST = &
                  SYSTEM_MARGINAL_COST(POS(R_TRANS_GROUP)+INT(1,2), &
                                            R_TRANS_GROUP,R_MARGIN_COST)
               GET_MARGINAL_COST = &
                     (1.-TOP_CAP_PERCENT) * GET_MARGINAL_COST + &
                             TOP_CAP_PERCENT * GET_TOP_CAP_MARGINAL_COST
            ENDIF
         ENDIF
      RETURN



! DEPTH OF MARKET CALLS

!

      ENTRY INIT_DEPTH_PRICE() ! CALLED DAILY

         INIT_DEPTH_PRICE = .FALSE.
!
         IF(.NOT. YES_DEPTH_OF_MARKET) RETURN
!
         DEPTH_PRICE = 0.
         IF(NUM_CREATE_HOURLY_PRICE > 0) THEN
            TRANS_DEPTH_PRICE = 0.
         ENDIF
         INIT_DEPTH_PRICE = .TRUE.
      RETURN
!

      ENTRY SAVE_DEPTH_OF_MARKET_DATA( R_TG, &
                                       D_HOUR, &
                                       D_DATA_BASES, &
                                       R_LOAD, &
                                       D_L_M, &
                                       D_SCARCITY_MULT)

!
         SAVE_DEPTH_OF_MARKET_DATA = .FALSE.
!
         IF(.NOT. YES_DEPTH_OF_MARKET) RETURN
!
         IF(R_TG == DEPTH_TARGET_GROUP .AND. YES_DEPTH_OF_MARKET) THEN
            MINIMUM_INTERVAL = -1. * DEPTH_MARKET_INTERVAL_SIZE * &
                                           (DEPTH_MARKET_INTERVALS-1)/2.
            LOCAL_INTERVAL = MINIMUM_INTERVAL
            DO I = 1, DEPTH_MARKET_INTERVALS
               DEPTH_AFTER_LOAD = MAX(0.,R_LOAD+LOCAL_INTERVAL)
               DEPTH_AFTER_LOAD = &
                  MIN(REAL(DEPTH_AFTER_LOAD,8), &
                        CAPACITY_GIVEN_MARKET( &
                              LAST_DB_POINT(D_DATA_BASES),D_DATA_BASES))
               DEPTH_PRICE(I,D_HOUR) = &
                   GET_MARGINAL_COST_AT_MARKET(REAL(DEPTH_AFTER_LOAD,8), &
                                                    D_DATA_BASES, &
                                                    D_SCARCITY_COST, &
                                                    D_L_M, &
                                                    D_SCARCITY_MULT)
               LOCAL_INTERVAL = LOCAL_INTERVAL + &
                                              DEPTH_MARKET_INTERVAL_SIZE
            ENDDO
            SAVE_DEPTH_OF_MARKET_DATA = .TRUE.
         ENDIF
         RETURN

      ENTRY SAVE_TRANS_DEPTH_DATA( &
                                       R_TG, &
                                       D_HOUR, &
                                       D_DATA_BASES, &
                                       R_LOAD, &
                                       D_L_M, &
                                       D_SCARCITY_MULT)

!
         SAVE_TRANS_DEPTH_DATA = .FALSE.
!
         TEMP_I = GET_CREATE_HOURLY_PRICE_INDEX(R_TG)
         IF(TEMP_I == 0) RETURN
!         IF(.NOT. YES_DEPTH_OF_MARKET) RETURN
         IF(YES_DEPTH_OF_MARKET .AND. NUM_CREATE_HOURLY_PRICE > 0) THEN
            MINIMUM_INTERVAL = -1. * DEPTH_MARKET_INTERVAL_SIZE * &
                                           (DEPTH_MARKET_INTERVALS-1)/2.
            LOCAL_INTERVAL = MINIMUM_INTERVAL
            TEMP_I = GET_CREATE_HOURLY_PRICE_INDEX(R_TG)
            DO I = 1, DEPTH_MARKET_INTERVALS
               DEPTH_AFTER_LOAD = MAX(0.,R_LOAD+LOCAL_INTERVAL)
               DEPTH_AFTER_LOAD = &
                  MIN(REAL(DEPTH_AFTER_LOAD,8), &
                        CAPACITY_GIVEN_MARKET( &
                              LAST_DB_POINT(D_DATA_BASES),D_DATA_BASES))
               TRANS_DEPTH_PRICE(TEMP_I,I,D_HOUR) = &
                   GET_MARGINAL_COST_AT_MARKET(REAL(DEPTH_AFTER_LOAD,8), &
                                                    D_DATA_BASES, &
                                                    D_SCARCITY_COST, &
                                                    D_L_M, &
                                                    D_SCARCITY_MULT)
               LOCAL_INTERVAL = LOCAL_INTERVAL + &
                                              DEPTH_MARKET_INTERVAL_SIZE
            ENDDO
            SAVE_TRANS_DEPTH_DATA = .TRUE.
         ENDIF
      RETURN
!

      ENTRY WRITE_DEPTH_OF_MARKET_REPORT(R_DAY,R_MONTH) 
         ! AT THE END OF EACH DAY      

         WRITE_DEPTH_OF_MARKET_REPORT = .FALSE.

         IF(.NOT. YES_DEPTH_OF_MARKET) RETURN

         IF(YES_DEPTH_OF_MARKET) THEN
            MINIMUM_INTERVAL = -1. * DEPTH_MARKET_INTERVAL_SIZE * &
                                           (DEPTH_MARKET_INTERVALS-1)/2.
            LOCAL_INTERVAL = MINIMUM_INTERVAL
            DO I = 1, DEPTH_MARKET_INTERVALS
               INQUIRE(UNIT=DEPTH_MARKET_UNIT,NEXTREC=DEPTH_MARKET_REC)
               WRITE(DEPTH_MARKET_UNIT,REC=DEPTH_MARKET_REC) &
                        PRT_ENDPOINT(), &
                        FLOAT(SAVE_YEAR), &
                        CL_MONTH_NAME(R_MONTH), &
                        FLOAT(R_DAY), &
                        LOCAL_INTERVAL, &
                        (DEPTH_PRICE(I,HR),HR=1,24)
!               DEPTH_MARKET_REC = DEPTH_MARKET_REC + 1
               LOCAL_INTERVAL = LOCAL_INTERVAL + &
                                              DEPTH_MARKET_INTERVAL_SIZE
            ENDDO
            IF(NUM_CREATE_HOURLY_PRICE > 0) THEN
               ! SAVE_UPPER_TRANS_GROUP
               DO  J = 1, NUM_CREATE_HOURLY_PRICE 
                  
                  LOCAL_INTERVAL = MINIMUM_INTERVAL
                  TEMP_I = GET_CREATE_HOURLY_PRICE_POS(J)
                  DO I = 1, DEPTH_MARKET_INTERVALS


                     WRITE(LOCAL_INTERVAL_CHR,'(I6)') &
                                                     INT(LOCAL_INTERVAL)
!
                     TRANS_DEPTH_MARKET_REC = &
                                       RPTREC(TRANS_DEPTH_MARKET_UNIT)
                     WRITE(TRANS_DEPTH_MARKET_UNIT, &
                                             REC=TRANS_DEPTH_MARKET_REC) &
                        PRT_ENDPOINT(), &
                        FLOAT(SAVE_YEAR), &
                        CL_MONTH_NAME(R_MONTH), &
                        FLOAT(R_DAY), &
                        GET_GROUP_NAME(TEMP_I), &
                        LOCAL_INTERVAL_CHR, &
                        (TRANS_DEPTH_PRICE(J,I,HR),HR=1,24)
                     LOCAL_INTERVAL = LOCAL_INTERVAL + &
                                              DEPTH_MARKET_INTERVAL_SIZE
                  ENDDO
               ENDDO
            ENDIF
            WRITE_DEPTH_OF_MARKET_REPORT = .TRUE.
         ENDIF
      RETURN
!

      ENTRY GET_CLOSEST_MARGINAL_COST(R_MARKET_PRICE,R_DATA_BASES,L_M)

         DO I = 1, LAST_DB_POINT(R_DATA_BASES)
            GET_CLOSEST_MARGINAL_COST = &
                            TRANSITION_MARGINAL_COST(I,R_DATA_BASES,L_M)
            IF(GET_CLOSEST_MARGINAL_COST + .01 >= R_MARKET_PRICE) EXIT
         ENDDO
      RETURN

      ENTRY GET_MARGINAL_COST_AT_MARKET(R8_LOAD,R_DATA_BASES, &
                                        SCARCITY_COST,L_M, &
                                        R_SCARCITY_MULT) &
                  RESULT(R_GET_MARGINAL_COST_AT_MARKET)

!
         HALF_POINT = SAVE_HALF_POINT
         SAVE_LOAD(R_DATA_BASES) = R8_LOAD
         IF(R_DATA_BASES > MAX_DATA_BASES) THEN
            WRITE(4,*) "MAX DATABASES EXCEEDED"
         ENDIF
         IF(LAST_DB_POINT(R_DATA_BASES) > 275) THEN
            WRITE(4,*) "MAX DATABASE TOP POINT EXCEEDED"
            WRITE(4,*) "R_DATA_BASES =",R_DATA_BASES
            WRITE(4,*) "LAST_DB_POINT =",LAST_DB_POINT(R_DATA_BASES)
         ENDIF
         IF(SAVE_LOAD(R_DATA_BASES) < &
                             CAPACITY_GIVEN_MARKET(1,R_DATA_BASES)) THEN
            start_val = 0
            end_val = 1
            HALF_POINT = 0
            POINT_NOT_FOUND = .FALSE.
         ELSEIF(SAVE_LOAD(R_DATA_BASES) > &
                    CAPACITY_GIVEN_MARKET( &
                         LAST_DB_POINT(R_DATA_BASES),R_DATA_BASES)) THEN
            POINT_NOT_FOUND = .FALSE.
            HALF_POINT = LAST_DB_POINT(R_DATA_BASES)
         ELSEIF(SAVE_LOAD(R_DATA_BASES) > &
                    CAPACITY_GIVEN_MARKET(HALF_POINT,R_DATA_BASES)) THEN
            start_val = HALF_POINT
            end_val = LAST_DB_POINT(R_DATA_BASES)
            POINT_NOT_FOUND = .TRUE.
         ELSEIF(SAVE_LOAD(R_DATA_BASES) < &
                    CAPACITY_GIVEN_MARKET(HALF_POINT,R_DATA_BASES)) THEN
            start_val = 1
            end_val = HALF_POINT
            POINT_NOT_FOUND = .TRUE.
         ELSE
            POINT_NOT_FOUND = .FALSE.
         ENDIF
!
         DO WHILE (POINT_NOT_FOUND)
            HALF_POINT = start_val + (end_val -start_val)/2
            IF(HALF_POINT == start_val) EXIT
            IF(SAVE_LOAD(R_DATA_BASES) > &
                    CAPACITY_GIVEN_MARKET(HALF_POINT,R_DATA_BASES)) THEN
               start_val = HALF_POINT
            ELSEIF(SAVE_LOAD(R_DATA_BASES) < &
                    CAPACITY_GIVEN_MARKET(HALF_POINT,R_DATA_BASES)) THEN
               end_val = HALF_POINT
            ELSE
               EXIT
            ENDIF
         ENDDO
         POS(R_DATA_BASES) = HALF_POINT
         call write_trace_pos("margnobj:0015", POS(R_DATA_BASES))
!
! ADDED SCARCITY_COST IN FIRST TWO IF'S ON 3/6/01.
!
         IF(S_M /= 2 .OR. POS(R_DATA_BASES) < 0 .OR. &
                                POS(R_DATA_BASES) > 275) THEN
            SCREEN_MESSAGES = 'Market Parameter Violated   '
            CALL MG_LOCATE_WRITE(12,26,trim(SCREEN_MESSAGES), &
                                                         ALL_VERSIONS,2)
            WRITE(SCREEN_MESSAGES,"(I3)") POS(R_DATA_BASES)
            CALL MG_LOCATE_WRITE(16,23,trim(SCREEN_MESSAGES), &
                                                         ALL_VERSIONS,0)
         ENDIF
!
         IF(HALF_POINT == 0) THEN
            R_GET_MARGINAL_COST_AT_MARKET = &
                            TRANSITION_MARGINAL_COST(1,R_DATA_BASES,L_M)
            SCARCITY_COST = &
               TRANSITION_MARGINAL_COST(1,R_DATA_BASES,S_M)
         ELSEIF(HALF_POINT >= LAST_DB_POINT(R_DATA_BASES)) THEN
            R_GET_MARGINAL_COST_AT_MARKET = &
                  TRANSITION_MARGINAL_COST(POS(R_DATA_BASES), &
                                                       R_DATA_BASES,L_M)
            SCARCITY_COST = &
               TRANSITION_MARGINAL_COST(POS(R_DATA_BASES), &
                                                       R_DATA_BASES,S_M)
         ELSE

            IF(CAPACITY_GIVEN_MARKET(HALF_POINT+1,R_DATA_BASES) == &
                    CAPACITY_GIVEN_MARKET(HALF_POINT,R_DATA_BASES)) THEN
               TOP_CAP_PERCENT = 0.
            ELSE
               TOP_CAP_PERCENT = &
                  (SAVE_LOAD(R_DATA_BASES) - &
                      CAPACITY_GIVEN_MARKET(HALF_POINT  ,R_DATA_BASES))/ &
                     (CAPACITY_GIVEN_MARKET(HALF_POINT+1,R_DATA_BASES) - &
                      CAPACITY_GIVEN_MARKET(HALF_POINT  ,R_DATA_BASES))
            ENDIF

            R_GET_MARGINAL_COST_AT_MARKET = &
               TRANSITION_MARGINAL_COST(POS(R_DATA_BASES), &
                                                       R_DATA_BASES,L_M)
            SCARCITY_COST = &
               TRANSITION_MARGINAL_COST(POS(R_DATA_BASES), &
                                                       R_DATA_BASES,S_M)
            IF(TOP_CAP_PERCENT > 0.) THEN

               GET_TOP_CAP_MARGINAL_COST = &
                  TRANSITION_MARGINAL_COST(POS(R_DATA_BASES)+1, &
                                                       R_DATA_BASES,L_M)
               R_GET_MARGINAL_COST_AT_MARKET = &
                  (1.-TOP_CAP_PERCENT) * R_GET_MARGINAL_COST_AT_MARKET + &
                        TOP_CAP_PERCENT  * GET_TOP_CAP_MARGINAL_COST
!
               GET_TOP_CAP_SCARCITY_COST = &
                  TRANSITION_MARGINAL_COST(POS(R_DATA_BASES)+1, &
                                                       R_DATA_BASES,S_M)
               SCARCITY_COST = &
                    (1.-TOP_CAP_PERCENT) * SCARCITY_COST + &
                        TOP_CAP_PERCENT  * GET_TOP_CAP_SCARCITY_COST
            ENDIF
         ENDIF
!
! ADDED FOR ARTMAN. 03/06/01.
!
         R_GET_MARGINAL_COST_AT_MARKET = &
                     (R_GET_MARGINAL_COST_AT_MARKET - SCARCITY_COST) + &
                                         SCARCITY_COST * R_SCARCITY_MULT
         SCARCITY_COST = SCARCITY_COST * R_SCARCITY_MULT

      RETURN

      ENTRY GET_LAST_TRANS_PRICE(R_DATA_BASES,L_M)

            GET_LAST_TRANS_PRICE = &
                  TRANSITION_MARGINAL_COST(LAST_DB_POINT(R_DATA_BASES), &
                                                       R_DATA_BASES,L_M)
      RETURN

      ENTRY MARKET_FLASH( &
                        R_BUYER_MC, &
                        R_BUYER_LOAD, &
                        R_BUYER_DB, &
                        R_SELLER_MC, &
                        R_SELLER_LOAD, &
                        R_SELLER_LIMIT, &
                        R_SELLER_DB, &
                        L_M)

         IF(R_BUYER_MC <= R_SELLER_MC) THEN
            WRITE(4,*) "INVALID TRANSACTION ATTEMPTED IN MC"
            WRITE(4,*) '*** line 3723 MARGNOBJ.FOR ***'
            er_message='See WARNING MESSAGES -MARGNOBJ.FOR-7'
            call end_program(er_message)
         ENDIF
         BUYER_POSITION = POS(R_BUYER_DB)
         BUYER_START_CAP = R_BUYER_LOAD

         SELLER_POSITION = POS(R_SELLER_DB)
         SELLER_START_CAP = R_SELLER_LOAD

         MARKET_FLASH = MIN(R_BUYER_LOAD,R_SELLER_LIMIT)

         DO WHILE(REAL(R_BUYER_LOAD,8) < &
                       CAPACITY_GIVEN_MARKET(BUYER_POSITION,R_BUYER_DB))
            BUYER_POSITION = BUYER_POSITION + 1
         ENDDO
         DO WHILE(REAL(R_SELLER_LOAD,8) < &
                     CAPACITY_GIVEN_MARKET(SELLER_POSITION,R_SELLER_DB))
            SELLER_POSITION = SELLER_POSITION + 1
         ENDDO
!
! TEST INCREASINGLY LARGER AMOUNTS OF TRANSACTION UNTIL
! A COST OR CAPACITY CONSTRAINT IS VIOLATED.
! GOAL IS TO SELL AS MUCH AS POSSIBLE WITHOUT SEEING SELLER'S PRICE
! BECOME HIGHER THAN THE BUYER'S PRICE
!
         BUYER_POSITION = BUYER_POSITION + 1
         DO
            SELLER_TEST_PRICE = &
               TRANSITION_MARGINAL_COST(SELLER_POSITION+1, &
                                                        R_SELLER_DB,L_M)
            SELLER_TEST_CAP = &
              CAPACITY_GIVEN_MARKET(SELLER_POSITION+1,R_SELLER_DB)
! WHAT BUY PRICE DO I SEE FOR THAT SELLER_TEST_CAP?
            BUYER_TEST_PRICE = &
                 TRANSITION_MARGINAL_COST(BUYER_POSITION-1, &
                                                         R_BUYER_DB,L_M)
            BUYER_TEST_CAP = &
                      CAPACITY_GIVEN_MARKET(BUYER_POSITION-1,R_BUYER_DB)
            IF(BUYER_START_CAP - BUYER_TEST_CAP < &
                                 SELLER_TEST_CAP - SELLER_START_CAP)THEN
               IF(BUYER_TEST_PRICE <= SELLER_TEST_PRICE) THEN
                  SELLER_POSITION = SELLER_POSITION + 1
                  BUYER_POSITION = BUYER_POSITION + 1
               ELSE
                  BUYER_POSITION = BUYER_POSITION - 1
               ENDIF
            ELSEIF(BUYER_TEST_PRICE > SELLER_TEST_PRICE) THEN
               EXIT
            ELSE
               BUYER_POSITION = BUYER_POSITION - 1
            ENDIF
         ENDDO
         ! FOR NOW, start_val WITH POS. LATER FIND EXACT STARTING POSITION
         DO 
            BUYER_TEST_CAP = &
                      CAPACITY_GIVEN_MARKET(BUYER_POSITION-1,R_BUYER_DB)
            BUYER_TEST_PRICE = &
               TRANSITION_MARGINAL_COST(BUYER_POSITION-1,R_BUYER_DB,L_M)
            SELLER_TEST_CAP = &
                    CAPACITY_GIVEN_MARKET(SELLER_POSITION+1,R_SELLER_DB)
            SELLER_TEST_PRICE = &
               TRANSITION_MARGINAL_COST(SELLER_POSITION+1, &
                                                        R_SELLER_DB,L_M)
            IF(BUYER_TEST_CAP - BUYER_START_CAP > MARKET_FLASH .AND. &
                  SELLER_START_CAP - SELLER_TEST_CAP > &
                                                      MARKET_FLASH) THEN
!              SOLUTION WITHIN CURRENT INTERVALS
               EXIT
            ELSEIF(BUYER_START_CAP - BUYER_TEST_CAP > MARKET_FLASH) THEN
!              SELL MAY STILL HAVE MULTIPLE INTERVALS
            ELSEIF(SELLER_TEST_CAP - SELLER_START_CAP > &
                                                      MARKET_FLASH) THEN
!              BUY MAY STILL HAVE MULTIPLE INTERVALS
            ELSEIF(SELLER_TEST_PRICE >= BUYER_TEST_PRICE) THEN
! IS THIS REALLY THE BEST TRACK?
               EXIT

            ELSE ! KEEP ON TRUCKING

               R_BUYER_MC = BUYER_TEST_PRICE
               R_SELLER_MC = SELLER_TEST_PRICE

               BUYER_POSITION = BUYER_POSITION - 1
               SELLER_POSITION = SELLER_POSITION + 1

               CYCLE
            ENDIF
         ENDDO ! SEARCH FOR THE BUYER AND SELLER INTERVALS THAT SATISFY CONSTRAINTS

         SELLER_LOWER_CAP = &
               CAPACITY_GIVEN_MARKET(SELLER_POSITION,R_SELLER_DB) - &
                                                        SELLER_START_CAP
         SELLER_UPPER_CAP = &
               CAPACITY_GIVEN_MARKET(SELLER_POSITION+1,R_SELLER_DB) - &
                                                        SELLER_START_CAP
         BUYER_LOWER_CAP = BUYER_START_CAP - &
                      CAPACITY_GIVEN_MARKET(BUYER_POSITION-1,R_BUYER_DB)
         BUYER_UPPER_CAP = BUYER_START_CAP - &
                      CAPACITY_GIVEN_MARKET(BUYER_POSITION,R_BUYER_DB)
!
         IF(SELLER_UPPER_CAP < BUYER_LOWER_CAP .AND. &
                                 BUYER_POSITION < POS(R_BUYER_DB) ) THEN
            BUYER_POSITION = BUYER_POSITION + 1
            BUYER_LOWER_CAP = BUYER_START_CAP - &
                      CAPACITY_GIVEN_MARKET(BUYER_POSITION-1,R_BUYER_DB)
            BUYER_UPPER_CAP = BUYER_START_CAP - &
                      CAPACITY_GIVEN_MARKET(BUYER_POSITION,R_BUYER_DB)
         ENDIF
         IF(BUYER_UPPER_CAP < SELLER_LOWER_CAP .AND. &
                                SELLER_POSITION > POS(R_SELLER_DB)) THEN
            SELLER_POSITION = SELLER_POSITION - 1
            SELLER_LOWER_CAP = SELLER_START_CAP - &
                    CAPACITY_GIVEN_MARKET(SELLER_POSITION,R_SELLER_DB)
            SELLER_UPPER_CAP = SELLER_START_CAP - &
                    CAPACITY_GIVEN_MARKET(SELLER_POSITION+1,R_SELLER_DB)
         ENDIF
!
         MAX_SELL_PRICE = &
               TRANSITION_MARGINAL_COST(SELLER_POSITION+1, &
                                                        R_SELLER_DB,L_M)
         MIN_SELL_PRICE = &
               TRANSITION_MARGINAL_COST(SELLER_POSITION,R_SELLER_DB,L_M)
         MAX_SELL_CAPACITY = SELLER_UPPER_CAP - SELLER_LOWER_CAP
!
         SELLER_SLOPE = (MAX_SELL_PRICE - MIN_SELL_PRICE) / &
                                                       MAX_SELL_CAPACITY
         SELLER_INTERCEPT = MIN_SELL_PRICE
!
         MAX_BUY_PRICE = &
                 TRANSITION_MARGINAL_COST(BUYER_POSITION,R_BUYER_DB,L_M)
         MIN_BUY_PRICE = &
               TRANSITION_MARGINAL_COST(BUYER_POSITION-1,R_BUYER_DB,L_M)
!
         MAX_BUY_CAPACITY = BUYER_LOWER_CAP - BUYER_UPPER_CAP
!
         BUYER_SLOPE = (MIN_BUY_PRICE - MAX_BUY_PRICE) / &
                                                        MAX_BUY_CAPACITY
         BUYER_INTERCEPT = MAX_BUY_PRICE - BUYER_SLOPE * &
                                    (BUYER_UPPER_CAP - SELLER_LOWER_CAP)
         IF(SELLER_SLOPE == BUYER_SLOPE) THEN
            WRITE(4,*) "Slopes identical",SELLER_SLOPE
            SELLER_SLOPE = BUYER_SLOPE + .1

         ENDIF
!
         TEMP_TRANSACTION = (BUYER_INTERCEPT - SELLER_INTERCEPT)/ &
                                            (SELLER_SLOPE - BUYER_SLOPE)
!
         POS(R_BUYER_DB) = BUYER_POSITION - 1
         call write_trace_pos("margnobj:0016", POS(R_BUYER_DB))
         POS(R_SELLER_DB) = SELLER_POSITION
         call write_trace_pos("margnobj:0017", POS(R_SELLER_DB))
!
         MARKET_FLASH = SELLER_LOWER_CAP + TEMP_TRANSACTION
!
      RETURN

      ENTRY PUT_BUYERS_LOAD(R_LOAD,R_DATA_BASES)

!
         IF(ACTIVE_DATA_BASE(R_DATA_BASES)) THEN
            SAVE_LOAD(R_DATA_BASES) = R_LOAD
            PUT_BUYERS_LOAD = SAVE_LOAD(R_DATA_BASES)
!         IF(R_LOAD > CUM_CAP(R_DATA_BASES) ) THEN
            IF(REAL(R_LOAD,8) > CAPACITY_GIVEN_MARKET( &
                         LAST_DB_POINT(R_DATA_BASES),R_DATA_BASES)) THEN
               POS(R_DATA_BASES) = LAST_DB_POINT(R_DATA_BASES)
               call write_trace_pos("margnobj:0018", POS(R_DATA_BASES))
            ENDIF
         ELSE
!            R_DATA_BASES = R_DATA_BASES
         ENDIF
      RETURN

      ENTRY GET_AVERAGE_COST(R_HOUR,R_DATA_BASES,BEFORE_OR_AFTER_SALES)

      if(file_trace_gac==0) then
        file_trace_gac=open_trace("get_average_cost.trace", rq_gac)
      end if
      
!
! POS_PERCENT = 1 APPLIES ALL WEIGHT TO THE TOP INTERVAL
!
!
         MPOS = POS(R_DATA_BASES)
         IF(MPOS == 0) THEN
! CONDITION ADDED 11/13/01.
            IF(CAPACITY_GIVEN_MARKET(1,R_DATA_BASES) > 0.d0) THEN
               GET_AVERAGE_COST = &
                  TOT_EMBED_COST_BY_POINT(1,R_DATA_BASES)/ &
                                   CAPACITY_GIVEN_MARKET(1,R_DATA_BASES)
                    call write_trace_real4(file_trace_gac, "9. GAC", &
                GET_AVERAGE_COST)
            ELSE
               GET_AVERAGE_COST = 0.
                    call write_trace_real4(file_trace_gac, "10. GAC", &
                GET_AVERAGE_COST)
            ENDIF
            
            IF(BEFORE_OR_AFTER_SALES == "Before") THEN
                  SALES_B4_AFTER(0,1,R_DATA_BASES) = &
                                   SALES_B4_AFTER(0,1,R_DATA_BASES) + 1.
                  call write_trace_real4(file_trace_gac, "6. SBA", &
                    SALES_B4_AFTER(0, 1, R_DATA_BASES))
                  NATIVE_POS_PERCENT(R_DATA_BASES) = 0.
                  NATIVE_POS(R_DATA_BASES) = MPOS
            ELSE
                  SALES_B4_AFTER(0,2,R_DATA_BASES) = &
                                   SALES_B4_AFTER(0,2,R_DATA_BASES) + 1.
                  call write_trace_real4(file_trace_gac, "5. SBA", &
                    SALES_B4_AFTER(0, 2, R_DATA_BASES))
                  POS_PERCENT = 0.
            ENDIF
         ELSEIF(MPOS >= LAST_DB_POINT(R_DATA_BASES)) THEN
            IF(CAPACITY_GIVEN_MARKET(MPOS,R_DATA_BASES) > 0.d0) THEN
               GET_AVERAGE_COST = &
                  TOT_EMBED_COST_BY_POINT(LAST_DB_POINT(R_DATA_BASES), &
                                                          R_DATA_BASES)/ &
                                CAPACITY_GIVEN_MARKET(MPOS,R_DATA_BASES)
                    call write_trace_real4(file_trace_gac, "8. GAC", &
                GET_AVERAGE_COST)
            ELSE
               GET_AVERAGE_COST = 0.
                    call write_trace_real4(file_trace_gac, "7. GAC", &
                GET_AVERAGE_COST)
            ENDIF
            IF(BEFORE_OR_AFTER_SALES == "Before") THEN
               SALES_B4_AFTER(LAST_DB_POINT(R_DATA_BASES), &
                                                       1,R_DATA_BASES) = &
                  SALES_B4_AFTER(LAST_DB_POINT(R_DATA_BASES), &
                                                    1,R_DATA_BASES) + 1.
               NATIVE_POS_PERCENT(R_DATA_BASES) = 1.
               NATIVE_POS(R_DATA_BASES) = MPOS
            ELSE
               SALES_B4_AFTER(LAST_DB_POINT(R_DATA_BASES), &
                                                       2,R_DATA_BASES) = &
                  SALES_B4_AFTER(LAST_DB_POINT(R_DATA_BASES), &
                                                    2,R_DATA_BASES) + 1.
               POS_PERCENT = 1.
!
               SUM_AVERAGE_COST_SALES = SUM_AVERAGE_COST_SALES + &
                      CAPACITY_GIVEN_MARKET(LAST_DB_POINT(R_DATA_BASES), &
                                                           R_DATA_BASES)
!
            ENDIF
         ELSE
            IF(SAVE_LOAD(R_DATA_BASES) > 0.d0) THEN
               IF(BEFORE_OR_AFTER_SALES == "Before") THEN


                  IF(CAPACITY_GIVEN_MARKET(MPOS,R_DATA_BASES) /= &
                        CAPACITY_GIVEN_MARKET(MPOS+1,R_DATA_BASES)) THEN
       call write_trace_int2(file_trace_gac, "R/DB", R_DATA_BASES)
       call write_trace_int2(file_trace_gac, "MPOS", MPOS)
       call write_trace_real8(file_trace_gac, "SAVE_LOAD", &
        SAVE_LOAD(R_DATA_BASES))
      call write_trace_real8(file_trace_gac, "1. CAPACITY_GIVEN_MARKET", &
        CAPACITY_GIVEN_MARKET(MPOS,R_DATA_BASES))
      call write_trace_real8(file_trace_gac, "2. CAPACITY_GIVEN_MARKET", &
       CAPACITY_GIVEN_MARKET(MPOS+1,R_DATA_BASES))
       

                     GET_AVERAGE_COST = &
                        (SAVE_LOAD(R_DATA_BASES) - &
                              CAPACITY_GIVEN_MARKET(MPOS,R_DATA_BASES))/ &
                        (CAPACITY_GIVEN_MARKET(MPOS+1,R_DATA_BASES) - &
                               CAPACITY_GIVEN_MARKET(MPOS,R_DATA_BASES))
                    call write_trace_real4(file_trace_gac, "6. GAC", &
                GET_AVERAGE_COST)
                  ELSE
                     GET_AVERAGE_COST = 0.0 ! ABITRARY
                    call write_trace_real4(file_trace_gac, "5. GAC", &
                GET_AVERAGE_COST)
                  ENDIF
                  IF(GET_AVERAGE_COST > 1.0 .OR. &
                                            GET_AVERAGE_COST < 0.0) THEN
                     WRITE(4,*) "INCONSISTENT MARGINAL AND AVERAGE"
                     WRITE(4,*) "COSTING CALCULATIONS IN TRANSACT"
                     WRITE(4,*) "BEFORE TRANSACTIONS IN HOUR ",R_HOUR
                     WRITE(4,*) '*** line 3976 MARGNOBJ.FOR ***'
                     er_message='See WARNING MESSAGES -MARGNOBJ.FOR-8'
                     call end_program(er_message)
                  ENDIF
                  call write_trace_real4(file_trace_gac, "11. GAC", &
                    GET_AVERAGE_COST)
                    
                  SALES_B4_AFTER(MPOS,1,R_DATA_BASES) = &
                     SALES_B4_AFTER(MPOS,1,R_DATA_BASES) + &
                                                   (1.-GET_AVERAGE_COST)
                  call write_trace_real4(file_trace_gac, "4. SBA", &
                    SALES_B4_AFTER(MPOS, 1, R_DATA_BASES))
         call write_trace_int2(file_trace_gac, "3. MPOS", MPOS)
         call write_trace_int2(file_trace_gac, "3. R/DB", R_DATA_BASES)
         call write_trace_real4(file_trace_gac, "12. GAC", &
            GET_AVERAGE_COST)
                  call write_trace_real4(file_trace_gac, "7. SBA", &
                    SALES_B4_AFTER(MPOS+1,1,R_DATA_BASES))

                  SALES_B4_AFTER(MPOS+1,1,R_DATA_BASES) = &
                     SALES_B4_AFTER(MPOS+1,1,R_DATA_BASES) + &
                                                        GET_AVERAGE_COST
                  call write_trace_real4(file_trace_gac, "3. SBA", &
                    SALES_B4_AFTER(MPOS+1, 1, R_DATA_BASES))
                  NATIVE_POS_PERCENT(R_DATA_BASES) = GET_AVERAGE_COST
                  NATIVE_POS(R_DATA_BASES) = MPOS
               ELSE
! 1/14/98. GAT. FOUND BUG IN THIS STATEMENT.
                  IF(CAPACITY_GIVEN_MARKET(MPOS,R_DATA_BASES) /= &
                        CAPACITY_GIVEN_MARKET(MPOS+1,R_DATA_BASES)) THEN
                     POS_PERCENT = &
                     (SAVE_LOAD(R_DATA_BASES) - &
                              CAPACITY_GIVEN_MARKET(MPOS,R_DATA_BASES))/ &
                     (CAPACITY_GIVEN_MARKET(MPOS+1,R_DATA_BASES) - &
                               CAPACITY_GIVEN_MARKET(MPOS,R_DATA_BASES))
                  ELSE
                     POS_PERCENT = 0.0 ! ABITRARY
                  ENDIF
                  IF(POS_PERCENT > 1.0 .OR. POS_PERCENT < 0.0) THEN
                     WRITE(4,*) "INCONSISTENT MARGINAL AND AVERAGE"
                     WRITE(4,*) "COSTING CALCULATIONS IN TRANSACT"
                     WRITE(4,*) "AFTER TRANSACTIONS IN HOUR ",R_HOUR
                     WRITE(4,*) '*** line 4004 MARGNOBJ.FOR ***'
                     er_message='See WARNING MESSAGES -MARGNOBJ.FOR-9'
                     call end_program(er_message)
                  ENDIF

                  SALES_B4_AFTER(MPOS,2,R_DATA_BASES) = &
                     SALES_B4_AFTER(MPOS,2,R_DATA_BASES) + &
                                                      (1. - POS_PERCENT)
                  call write_trace_real4(file_trace_gac, "1. SBA", &
                    SALES_B4_AFTER(MPOS, 2, R_DATA_BASES))
                    
                  SALES_B4_AFTER(MPOS+1,2,R_DATA_BASES) = &
                     SALES_B4_AFTER(MPOS+1,2,R_DATA_BASES) + POS_PERCENT
                  call write_trace_real4(file_trace_gac, "2. SBA", &
                    SALES_B4_AFTER(MPOS+1, 2, R_DATA_BASES))
                  
                  GET_AVERAGE_COST = POS_PERCENT
                    call write_trace_real4(file_trace_gac, "4. GAC", &
                GET_AVERAGE_COST)
!
                  SUM_AVERAGE_COST_SALES = SUM_AVERAGE_COST_SALES + &
                        (1.-POS_PERCENT)* &
                              CAPACITY_GIVEN_MARKET(MPOS,R_DATA_BASES) + &
                        POS_PERCENT* &
                              CAPACITY_GIVEN_MARKET(MPOS+1,R_DATA_BASES)
!
               ENDIF ! BEFORE_OR_AFTER_SALES
!
               GET_AVERAGE_COST = GET_AVERAGE_COST * &
                     (TOT_EMBED_COST_BY_POINT(MPOS+1,R_DATA_BASES) - &
                      TOT_EMBED_COST_BY_POINT(MPOS,R_DATA_BASES))
                    call write_trace_real4(file_trace_gac, "3. GAC", &
                GET_AVERAGE_COST)
!
               GET_AVERAGE_COST = (GET_AVERAGE_COST + &
                           TOT_EMBED_COST_BY_POINT(MPOS,R_DATA_BASES))/ &
                                                SAVE_LOAD(R_DATA_BASES)
               call write_trace_real4(file_trace_gac, "2. GAC", &
                GET_AVERAGE_COST)
            ELSE
               GET_AVERAGE_COST = 0.
               call write_trace_real4(file_trace_gac, "1. GAC", &
                GET_AVERAGE_COST)
                
            ENDIF
!
         ENDIF

      RETURN

      ENTRY PUT_MARKET_REV_AND_EXP(R_HOUR,REV,SALE,R_DATA_BASES,R_MONTH, &
                                   R_YEAR)

!
! REV PASSED FROM TRANSACT IS BASED UPON A TOTAL REVENUE AT MARKET PRICE
!
! REV = 1, EXP = 2
! SAL = 1, PUR = 2
!
         IF(R_HOUR == 1) THEN
            MONTHLY_MARKET_SALES = SALE
         ELSE
            MONTHLY_MARKET_SALES = MONTHLY_MARKET_SALES + SALE
         ENDIF
!
         IF( ABS(MONTHLY_MARKET_SALES - &
                                      SUM_AVERAGE_COST_SALES) > 1.) THEN
            DATA_BASES = DATA_BASES
         ENDIF
!
!
         IF(REV*SALE < -0.000001) THEN
            WRITE(4,*) "UNECONOMIC TRANSACTION IN MARKET PRICING"
            WRITE(4,*) "REVENUE = ",REV," QUANTITY= ",SALE
            WRITE(4,*) '*** line 4069 MARGNOBJ.FOR ***'

            RETURN ! 1/17/02
         ENDIF

!
         MONTHLY_SALE = MONTHLY_SALE + SALE
!
         PUT_MARKET_REV_AND_EXP = 0.
         IF(SALE == 0.0) THEN
            RETURN
         ENDIF
         PUT_MARKET_REV_AND_EXP = REV/SALE
         TOTAL_POS_PERCENT = POS_PERCENT ! FROM GET_AVERAGE_COST ABOVE
!
! TRACKS PRICE AT RETAIL AND WHOLESALE, IF IT IS A SALE
!
         IF(.NOT. PRICE_ONLY_WHOLESALE_REV) THEN
!
! PRICE ANY SALE AT THE MARKET PRICE. CALL ALL SALES WHOLESALE.
! PRICE ANY PURCHASE AT THE MARKET PRICE.
!
            MPOS = POS(R_DATA_BASES)
!
            IF(MPOS > 0) THEN
               WHOLESALE_REV_AND_EXP(MPOS,1,R_DATA_BASES) = &
                     PUT_MARKET_REV_AND_EXP * &
                         (1.-POS_PERCENT) * &
                              CAPACITY_GIVEN_MARKET(MPOS,R_DATA_BASES) + &
                              WHOLESALE_REV_AND_EXP(MPOS,1,R_DATA_BASES)
               WHOLESALE_SAL_AND_PUR(MPOS,1,R_DATA_BASES) = &
                         (1.-POS_PERCENT) * &
                              CAPACITY_GIVEN_MARKET(MPOS,R_DATA_BASES) + &
                              WHOLESALE_SAL_AND_PUR(MPOS,1,R_DATA_BASES)
            ENDIF
            IF(MPOS < LAST_DB_POINT(R_DATA_BASES)) THEN
               WHOLESALE_REV_AND_EXP(MPOS+1,1,R_DATA_BASES) = &
                        PUT_MARKET_REV_AND_EXP * &
                           POS_PERCENT * &
                            CAPACITY_GIVEN_MARKET(MPOS+1,R_DATA_BASES) + &
                            WHOLESALE_REV_AND_EXP(MPOS+1,1,R_DATA_BASES)
               WHOLESALE_SAL_AND_PUR(MPOS+1,1,R_DATA_BASES) = &
                           POS_PERCENT * &
                            CAPACITY_GIVEN_MARKET(MPOS+1,R_DATA_BASES) + &
                            WHOLESALE_SAL_AND_PUR(MPOS+1,1,R_DATA_BASES)
            ELSE ! 100% IS ALLOCATED
               WHOLESALE_REV_AND_EXP(MPOS,1,R_DATA_BASES) = &
                     PUT_MARKET_REV_AND_EXP * &
                              CAPACITY_GIVEN_MARKET(MPOS,R_DATA_BASES) + &
                              WHOLESALE_REV_AND_EXP(MPOS,1,R_DATA_BASES)
               WHOLESALE_SAL_AND_PUR(MPOS,1,R_DATA_BASES) = &
                              CAPACITY_GIVEN_MARKET(MPOS,R_DATA_BASES) + &
                              WHOLESALE_SAL_AND_PUR(MPOS,1,R_DATA_BASES)
            ENDIF

            IF(MPOS > 0) THEN

               UNIT_DEVIATION = 0.

               IF(ABS(UNIT_DEVIATION) > .1) THEN
                  UNIT_DEVIATION = UNIT_DEVIATION
                  WRITE(4,*) "Unit generation does not equal the "// &
                          "whole unit sales and revenue "// &
                          "in hour ",R_HOUR," and data base ", &
                                                            R_DATA_BASES
                  WRITE(4,*) "In month ",R_MONTH," and year ",R_YEAR, &
                          " deviation ",UNIT_DEVIATION," position ",MPOS
               ENDIF
            ENDIF
!
         ELSEIF(PRICE_ONLY_WHOLESALE_REV) THEN

! PRICE ABOVE NATIVE LOAD AT WHOLESALE, NATIVE LOAD AT RETAIL (PURCHASE_EXPENSE?)

            IF(MPOS > NATIVE_POS(R_DATA_BASES) .OR. &
                 (MPOS == NATIVE_POS(R_DATA_BASES) .AND. &
                   POS_PERCENT > NATIVE_POS_PERCENT(R_DATA_BASES))) THEN

! TOTAL SALES > NATIVE LOAD => NET SELLER TO THE MARKET
!
!
! NATIVE ALLOCATED TO RETAIL
!
               MPOS = NATIVE_POS(R_DATA_BASES)
               POS_PERCENT = NATIVE_POS_PERCENT(R_DATA_BASES)
!
               RETAIL_REV_AND_EXP(MPOS,1,R_DATA_BASES) = &
                     PUT_MARKET_REV_AND_EXP * &
                         (1.-POS_PERCENT) * &
                              CAPACITY_GIVEN_MARKET(MPOS,R_DATA_BASES) + &
                              RETAIL_REV_AND_EXP(MPOS,1,R_DATA_BASES)
               RETAIL_SAL_AND_PUR(MPOS,1,R_DATA_BASES) = &
                         (1.-POS_PERCENT) * &
                              CAPACITY_GIVEN_MARKET(MPOS,R_DATA_BASES) + &
                              RETAIL_SAL_AND_PUR(MPOS,1,R_DATA_BASES)
               IF(MPOS < LAST_DB_POINT(R_DATA_BASES)) THEN
                  RETAIL_REV_AND_EXP(MPOS+1,1,R_DATA_BASES) = &
                        PUT_MARKET_REV_AND_EXP * &
                           POS_PERCENT * &
                            CAPACITY_GIVEN_MARKET(MPOS+1,R_DATA_BASES) + &
                            RETAIL_REV_AND_EXP(MPOS+1,1,R_DATA_BASES)
                  RETAIL_SAL_AND_PUR(MPOS+1,1,R_DATA_BASES) = &
                           POS_PERCENT * &
                            CAPACITY_GIVEN_MARKET(MPOS+1,R_DATA_BASES) + &
                            RETAIL_SAL_AND_PUR(MPOS+1,1,R_DATA_BASES)
               ELSE ! 100% IS ALLOCATED
!
! 2/18/98. GAT. TESTING FOR TVA
!

                  RETAIL_REV_AND_EXP(MPOS,1,R_DATA_BASES) = &
                     PUT_MARKET_REV_AND_EXP * &
                              CAPACITY_GIVEN_MARKET(MPOS,R_DATA_BASES) + &
                              RETAIL_REV_AND_EXP(MPOS,1,R_DATA_BASES)
                  RETAIL_SAL_AND_PUR(MPOS,1,R_DATA_BASES) = &
                              CAPACITY_GIVEN_MARKET(MPOS,R_DATA_BASES) + &
                              RETAIL_SAL_AND_PUR(MPOS,1,R_DATA_BASES)
               ENDIF
!
! NET THE RETAIL PORTION OUT IMMEDIATELY
!
               WHOLESALE_REV_AND_EXP(MPOS,1,R_DATA_BASES) = &
                     -1.*PUT_MARKET_REV_AND_EXP * &
                         (1.-POS_PERCENT) * &
                              CAPACITY_GIVEN_MARKET(MPOS,R_DATA_BASES) + &
                              WHOLESALE_REV_AND_EXP(MPOS,1,R_DATA_BASES)
               WHOLESALE_SAL_AND_PUR(MPOS,1,R_DATA_BASES) = &
                         -1.*(1.-POS_PERCENT) * &
                              CAPACITY_GIVEN_MARKET(MPOS,R_DATA_BASES) + &
                              WHOLESALE_SAL_AND_PUR(MPOS,1,R_DATA_BASES)
               IF(MPOS < LAST_DB_POINT(R_DATA_BASES)) THEN
                  WHOLESALE_REV_AND_EXP(MPOS+1,1,R_DATA_BASES) = &
                        -1.*PUT_MARKET_REV_AND_EXP * &
                           POS_PERCENT * &
                            CAPACITY_GIVEN_MARKET(MPOS+1,R_DATA_BASES) + &
                            WHOLESALE_REV_AND_EXP(MPOS+1,1,R_DATA_BASES)
                  WHOLESALE_SAL_AND_PUR(MPOS+1,1,R_DATA_BASES) = &
                           -1.*POS_PERCENT * &
                            CAPACITY_GIVEN_MARKET(MPOS+1,R_DATA_BASES) + &
                            WHOLESALE_SAL_AND_PUR(MPOS+1,1,R_DATA_BASES)
               ELSE ! 100% IS ALLOCATED
!
! 2/18/98. GAT. CHANGED FOR TVA.
!
                  WHOLESALE_REV_AND_EXP(MPOS,1,R_DATA_BASES) = &
                    -1.*PUT_MARKET_REV_AND_EXP * &
                              CAPACITY_GIVEN_MARKET(MPOS,R_DATA_BASES) + &
                              WHOLESALE_REV_AND_EXP(MPOS,1,R_DATA_BASES)
                  WHOLESALE_SAL_AND_PUR(MPOS,1,R_DATA_BASES) = &
                          -1.*CAPACITY_GIVEN_MARKET(MPOS,R_DATA_BASES) + &
                              WHOLESALE_SAL_AND_PUR(MPOS,1,R_DATA_BASES)
               ENDIF
!
! TOTAL (WHOLESALE = TOTAL - RETAIL)
!
               MPOS = POS(R_DATA_BASES)
               POS_PERCENT = TOTAL_POS_PERCENT
!
               WHOLESALE_REV_AND_EXP(MPOS,1,R_DATA_BASES) = &
                     PUT_MARKET_REV_AND_EXP * &
                         (1.-POS_PERCENT) * &
                              CAPACITY_GIVEN_MARKET(MPOS,R_DATA_BASES) + &
                              WHOLESALE_REV_AND_EXP(MPOS,1,R_DATA_BASES)
               WHOLESALE_SAL_AND_PUR(MPOS,1,R_DATA_BASES) = &
                         (1.-POS_PERCENT) * &
                              CAPACITY_GIVEN_MARKET(MPOS,R_DATA_BASES) + &
                              WHOLESALE_SAL_AND_PUR(MPOS,1,R_DATA_BASES)
               IF(MPOS < LAST_DB_POINT(R_DATA_BASES)) THEN
                  WHOLESALE_REV_AND_EXP(MPOS+1,1,R_DATA_BASES) = &
                        PUT_MARKET_REV_AND_EXP * &
                         POS_PERCENT * &
                            CAPACITY_GIVEN_MARKET(MPOS+1,R_DATA_BASES) + &
                            WHOLESALE_REV_AND_EXP(MPOS+1,1,R_DATA_BASES)
                  WHOLESALE_SAL_AND_PUR(MPOS+1,1,R_DATA_BASES) = &
                           POS_PERCENT * &
                            CAPACITY_GIVEN_MARKET(MPOS+1,R_DATA_BASES) + &
                            WHOLESALE_SAL_AND_PUR(MPOS+1,1,R_DATA_BASES)
               ELSE ! 100% IS ALLOCATED
                  WHOLESALE_REV_AND_EXP(MPOS,1,R_DATA_BASES) = &
                     PUT_MARKET_REV_AND_EXP * &
                              CAPACITY_GIVEN_MARKET(MPOS,R_DATA_BASES) + &
                              WHOLESALE_REV_AND_EXP(MPOS,1,R_DATA_BASES)
                  WHOLESALE_SAL_AND_PUR(MPOS,1,R_DATA_BASES) = &
                              CAPACITY_GIVEN_MARKET(MPOS,R_DATA_BASES) + &
                              WHOLESALE_SAL_AND_PUR(MPOS,1,R_DATA_BASES)
               ENDIF
!

!
            ELSE
!
!
! TOTAL SALES <= NATIVE LOAD => NET BUYER FROM THE MARKET
!
!
! NATIVE LESS WHOLESALE PURCHASE ALLOCATED TO RETAIL
!
               MPOS = POS(R_DATA_BASES)
               POS_PERCENT = TOTAL_POS_PERCENT
!
               RETAIL_REV_AND_EXP(MPOS,1,R_DATA_BASES) = &
                     PUT_MARKET_REV_AND_EXP * &
                         (1.-POS_PERCENT) * &
                              CAPACITY_GIVEN_MARKET(MPOS,R_DATA_BASES) + &
                              RETAIL_REV_AND_EXP(MPOS,1,R_DATA_BASES)
               RETAIL_SAL_AND_PUR(MPOS,1,R_DATA_BASES) = &
                         (1.-POS_PERCENT) * &
                              CAPACITY_GIVEN_MARKET(MPOS,R_DATA_BASES) + &
                              RETAIL_SAL_AND_PUR(MPOS,1,R_DATA_BASES)
               IF(MPOS < LAST_DB_POINT(R_DATA_BASES)) THEN
                  RETAIL_REV_AND_EXP(MPOS+1,1,R_DATA_BASES) = &
                        PUT_MARKET_REV_AND_EXP * &
                           POS_PERCENT * &
                            CAPACITY_GIVEN_MARKET(MPOS+1,R_DATA_BASES) + &
                            RETAIL_REV_AND_EXP(MPOS+1,1,R_DATA_BASES)
                  RETAIL_SAL_AND_PUR(MPOS+1,1,R_DATA_BASES) = &
                           POS_PERCENT * &
                            CAPACITY_GIVEN_MARKET(MPOS+1,R_DATA_BASES) + &
                            RETAIL_SAL_AND_PUR(MPOS+1,1,R_DATA_BASES)
               ELSE ! 100% IS ALLOCATED
                  RETAIL_REV_AND_EXP(MPOS,1,R_DATA_BASES) = &
                     PUT_MARKET_REV_AND_EXP * &
                              CAPACITY_GIVEN_MARKET(MPOS,R_DATA_BASES) + &
                              WHOLESALE_REV_AND_EXP(MPOS,1,R_DATA_BASES)
                  RETAIL_SAL_AND_PUR(MPOS,1,R_DATA_BASES) = &
                              CAPACITY_GIVEN_MARKET(MPOS,R_DATA_BASES) + &
                              WHOLESALE_SAL_AND_PUR(MPOS,1,R_DATA_BASES)
               ENDIF
!
               WHOLESALE_REV_AND_EXP(MPOS,2,R_DATA_BASES) = &
                     -1.*PUT_MARKET_REV_AND_EXP * &
                         (1.-POS_PERCENT) * &
                              CAPACITY_GIVEN_MARKET(MPOS,R_DATA_BASES) + &
                              WHOLESALE_REV_AND_EXP(MPOS,2,R_DATA_BASES)
               WHOLESALE_SAL_AND_PUR(MPOS,2,R_DATA_BASES) = &
                         -1.*(1.-POS_PERCENT) * &
                              CAPACITY_GIVEN_MARKET(MPOS,R_DATA_BASES) + &
                              WHOLESALE_SAL_AND_PUR(MPOS,2,R_DATA_BASES)
               IF(MPOS < LAST_DB_POINT(R_DATA_BASES)) THEN
                  WHOLESALE_REV_AND_EXP(MPOS+1,2,R_DATA_BASES) = &
                        -1.*PUT_MARKET_REV_AND_EXP * &
                         POS_PERCENT * &
                            CAPACITY_GIVEN_MARKET(MPOS+1,R_DATA_BASES) + &
                            WHOLESALE_REV_AND_EXP(MPOS+1,2,R_DATA_BASES)
                  WHOLESALE_SAL_AND_PUR(MPOS+1,2,R_DATA_BASES) = &
                            -1.*POS_PERCENT * &
                            CAPACITY_GIVEN_MARKET(MPOS+1,R_DATA_BASES) + &
                            WHOLESALE_SAL_AND_PUR(MPOS+1,2,R_DATA_BASES)
!
! 1/27/98. GAT. CHANGED CODE BELOW FROM ,1, TO ,2, AND MULT * -1
!
               ELSE ! 100% IS ALLOCATED
                  WHOLESALE_REV_AND_EXP(MPOS,2,R_DATA_BASES) = &
                         -PUT_MARKET_REV_AND_EXP * &
                              CAPACITY_GIVEN_MARKET(MPOS,R_DATA_BASES) + &
                              WHOLESALE_REV_AND_EXP(MPOS,2,R_DATA_BASES)
                  WHOLESALE_SAL_AND_PUR(MPOS,2,R_DATA_BASES) = &
                             -CAPACITY_GIVEN_MARKET(MPOS,R_DATA_BASES) + &
                              WHOLESALE_SAL_AND_PUR(MPOS,2,R_DATA_BASES)
               ENDIF
!
! TOTAL (WHOLESALE = TOTAL - RETAIL)
!
               MPOS = NATIVE_POS(R_DATA_BASES)
               POS_PERCENT = NATIVE_POS_PERCENT(R_DATA_BASES)
!
               WHOLESALE_REV_AND_EXP(MPOS,2,R_DATA_BASES) = &
                     PUT_MARKET_REV_AND_EXP * &
                         (1.-POS_PERCENT) * &
                              CAPACITY_GIVEN_MARKET(MPOS,R_DATA_BASES) + &
                              WHOLESALE_REV_AND_EXP(MPOS,2,R_DATA_BASES)
               WHOLESALE_SAL_AND_PUR(MPOS,2,R_DATA_BASES) = &
                         (1.-POS_PERCENT) * &
                              CAPACITY_GIVEN_MARKET(MPOS,R_DATA_BASES) + &
                              WHOLESALE_SAL_AND_PUR(MPOS,2,R_DATA_BASES)
               IF(MPOS < LAST_DB_POINT(R_DATA_BASES)) THEN
                  WHOLESALE_REV_AND_EXP(MPOS+1,2,R_DATA_BASES) = &
                        PUT_MARKET_REV_AND_EXP * &
                         POS_PERCENT * &
                            CAPACITY_GIVEN_MARKET(MPOS+1,R_DATA_BASES) + &
                            WHOLESALE_REV_AND_EXP(MPOS+1,2,R_DATA_BASES)
                  WHOLESALE_SAL_AND_PUR(MPOS+1,2,R_DATA_BASES) = &
                            POS_PERCENT * &
                            CAPACITY_GIVEN_MARKET(MPOS+1,R_DATA_BASES) + &
                            WHOLESALE_SAL_AND_PUR(MPOS+1,2,R_DATA_BASES)
!
! 1/27/98. GAT. CHANGED CODE BELOW FROM ,1, TO ,2,
!
               ELSE ! 100% IS ALLOCATED
                  WHOLESALE_REV_AND_EXP(MPOS,2,R_DATA_BASES) = &
                     PUT_MARKET_REV_AND_EXP * &
                              CAPACITY_GIVEN_MARKET(MPOS,R_DATA_BASES) + &
                              WHOLESALE_REV_AND_EXP(MPOS,2,R_DATA_BASES)
                  WHOLESALE_SAL_AND_PUR(MPOS,2,R_DATA_BASES) = &
                              CAPACITY_GIVEN_MARKET(MPOS,R_DATA_BASES) + &
                              WHOLESALE_SAL_AND_PUR(MPOS,2,R_DATA_BASES)
               ENDIF
            ENDIF ! NET SALE OR PURCHASE
!
         ENDIF ! PRICING REV AT WHOLESALE
         IF(YES_DAILY_THERMAL_RPT_ACTIVE) THEN
            IF(TRANSACTION_PERIOD == 'S') THEN
               HOURS_MULT = 4.0
               LAST_HOUR_IN_MONTH = SAVE_HOURS_IN_MONTH -3
               call check_hours_in_month(last_HOUR_IN_MONTH, "margnobj:0007")
            ELSE
               HOURS_MULT = 1.0
               LAST_HOUR_IN_MONTH = SAVE_HOURS_IN_MONTH
               call check_hours_in_month(last_HOUR_IN_MONTH, "margnobj:0008")
            ENDIF
            DO BLOCK = 1, NBLOK2_SAVE
               DATABASE = COST_CURVE_POINTER_BY(BLOCK,R_DATA_BASES)
               IF(DATABASE == 0) CYCLE
               U = UNIT_SAVE(BLOCK)
               B = MAX(INT(1,2),BLKNO_SAVE(BLOCK))
               TEMP_ENERGY = &
                        (1.-POS_PERCENT) * &
                            CUM_UNIT_UTIL(MPOS,DATABASE) + &
                                 POS_PERCENT * &
                                       CUM_UNIT_UTIL(MPOS+1,DATABASE)
!
               IF(U == 1) THEN
                  U = U
               ENDIF
               IF(B <= 1) THEN
                  TEMP_HEAT = COEFF(1,U) * TEMP_ENERGY
               ELSEIF(B == 2 .AND.  TEMP_ENERGY > 0.01) THEN
                  J = UNIT_TO_BLOCK(U,1)
                  IF(J < 1) THEN
                     J = J
                  ENDIF
                  TEMP_R =  MW_AFTER_OUTAGES(J,R_DATA_BASES)
                  TEMP_R2 =  MW_AFTER_OUTAGES(BLOCK,R_DATA_BASES)
                  IF(TEMP_ENERGY > TEMP_R2) THEN
                     TEMP_R = TEMP_R
                  ENDIF
                  A_CO = (COEFF(2,U)-COEFF(3,U))/ &
                                                (-2.*TEMP_R2)
                  B_CO = COEFF(2,U) - (COEFF(2,U) - COEFF(3,U)) * &
                              TEMP_R/(-1.*TEMP_R2)
                  C_CO = ((COEFF(1,U)-B_CO)* &
                                     TEMP_R) - &
                                   A_CO*TEMP_R*TEMP_R
                  TEMP_HEAT =  & !  TOTAL HEAT FOR THE BLOCK: NOTE MIN BLOCK SUBTRACTED BELOW
                             (A_CO* &
                             (TEMP_R+TEMP_ENERGY)* &
                             (TEMP_R+TEMP_ENERGY) + &
                             B_CO* &
                             (TEMP_R+TEMP_ENERGY)+ &
                             C_CO)              - TEMP_R*COEFF(1,U) ! MIN BLOCK HEAT
               ELSE
                  TEMP_HEAT = 0.
               ENDIF
               HOUR_IN_DAY = NINT(MOD(FLOAT(R_HOUR),24.))
               IF(HOUR_IN_DAY == 0) then
                HOUR_IN_DAY = 24
               end if
               DAILY_ENERGY(B,U) = DAILY_ENERGY(B,U) + &
                                                TEMP_ENERGY * HOURS_MULT
               call write_daily_energy_trace("margnobj:A001", &
                DAILY_ENERGY(B,U))
               HOURLY_ENERGY(HOUR_IN_DAY,U) = &
                           HOURLY_ENERGY(HOUR_IN_DAY,U) + &
                                                TEMP_ENERGY * HOURS_MULT
               DAILY_HEAT(B,U) = DAILY_HEAT(B,U) + &
                                       0.001 * TEMP_HEAT * HOURS_MULT
               MONTH_HEAT(B,U) = MONTH_HEAT(B,U) + &
                                               TEMP_HEAT * HOURS_MULT



            ENDDO

         ENDIF
      RETURN

!
      ENTRY WRITE_DAILY_THERMAL_UNIT(R_MONTH,R_DAY)
!

         DATE_ACTIVE = .FALSE.
         IF(SAVE_START_RPT_DATE <= 0 .OR. SAVE_END_RPT_DATE <= 0) THEN
            DATE_ACTIVE = .TRUE.
         ELSE ! ASSUMES SIMULATION AFTER 2000 AND BEFORE 2100
            TEST_DATE = R_MONTH*10000 + R_DAY*100 + SAVE_YEAR-2000
            IF(TEST_DATE >= SAVE_START_RPT_DATE .AND. &
                                   TEST_DATE <= SAVE_END_RPT_DATE) THEN
               DATE_ACTIVE = .TRUE.
            ENDIF
         ENDIF
         IF(YES_DAILY_THERMAL_RPT_ACTIVE) THEN ! .AND. R_MONTH < 2 .AND.
!     +                                                   R_DAY < 2) THEN
            WRITE_DAILY_THERMAL_UNIT = .TRUE.
            IF(UU_REPORT_NOT_OPEN .AND. REPORT_DAILY_THERMAL .AND. &
                                                       DATE_ACTIVE) THEN
               UU_REPORT_NOT_OPEN = .FALSE.
               UU_REPORT_VARIABLES = 11
               UU_DAILY_NO = UU_DAILY_HEADER(UU_REPORT_VARIABLES, &
                                                UU_DAILY_REC)
            ENDIF
            IF(UV_REPORT_NOT_OPEN .AND. DATE_ACTIVE) THEN
               UV_REPORT_NOT_OPEN = .FALSE.
               UV_REPORT_VARIABLES = 24
               UV_HOURLY_NO = UV_HOURLY_HEADER(UV_REPORT_VARIABLES, &
                                                UV_HOURLY_REC)
            ENDIF
            DO I = 1, NUNITS_SAVE
! ENERGY AND HEAT CALC
               UNIT_ENERGY = DAILY_ENERGY(1,I) + DAILY_ENERGY(2,I)
               UNIT_HEAT = DAILY_HEAT(1,I) + DAILY_HEAT(2,I)
               UNIT_CAPACITY = MW(2,I)
               cur_called_from="mo:1"
               CL_TRANS_NAME = UNITNM(I)//CL_UNIQUE_RPT_STR(I)
               RSP = FLOAT(GET_THERMAL_STATE_INDEX(I))
               K = MAX(INT(1,2),get_TRANSACTION_GROUP(I))
               RTG = FLOAT(K)
            RFT = MAX(INT(1,2),MIN(GET_PRIMARY_MOVER(I),MAX_FUEL_TYPES))
               ONLINE_DATE = 190000.+FLOAT(GET_THERMAL_ONLINE(I))
               HESI_ID_NUM_4_UNIT = GET_HESI_UNIT_ID_NUM(I, &
                              HESI_SECOND_UNIT_ID_NUM)
! 041612. USED TO ACCUMULATE TO PRIMARY MOVERS.
               GET_PRIMARY_MOVER_INDEX_called_from="margnobj:0012"
               J = GET_PRIMARY_MOVER_INDEX(I)
               RPM = FLOAT(J)
               DAILY_PM_TG_ENERGY(J,K) = &
                                   DAILY_PM_TG_ENERGY(J,K) + UNIT_ENERGY
               DAILY_PM_TG_CAPACITY(J,K) = &
                              DAILY_PM_TG_CAPACITY(J,K) +  UNIT_CAPACITY
               DAILY_PM_TG_HEAT(J,K) = &
                                      DAILY_PM_TG_HEAT(J,K) +  UNIT_HEAT
!
               IF(UNIT_ENERGY > 0.0001) THEN
                  AHR = UNIT_HEAT*1000.0 / UNIT_ENERGY
               ELSE
                  AHR = 0.0
               ENDIF
!                  IF(I /= 1) CYCLE
               IF(REPORT_THIS_CL_UNIT(I) .AND. &
                                    DATE_ACTIVE .AND. &
                                              REPORT_DAILY_THERMAL) THEN
                  UU_DAILY_REC = RPTREC(UU_DAILY_NO)
                  WRITE(UU_DAILY_NO,REC=UU_DAILY_REC) &
                           PRT_ENDPOINT(), &
                           FLOAT(SAVE_YEAR), &
                           CL_MONTH_NAME(R_MONTH), &
                           FLOAT(R_DAY), &
                           CL_TRANS_NAME, &
                           UNIT_ENERGY, &
                           UNIT_HEAT, &
                           RSP, &
                           RTG, &
                           RPM, &
                           RFT, &
                           AHR, &
                           UNIT_CAPACITY, &
                           ONLINE_DATE, &
                           HESI_ID_NUM_4_UNIT, &
                           HESI_SECOND_UNIT_ID_NUM
!
                  UU_DAILY_REC = UU_DAILY_REC + 1

!
                  CYCLE ! TO AVOID CREATING A LARGER REPORT
!
                  UV_HOURLY_REC = RPTREC(UV_HOURLY_NO)
                  WRITE(UV_HOURLY_NO,REC=UV_HOURLY_REC) &
                           PRT_ENDPOINT(), &
                           FLOAT(SAVE_YEAR), &
                           CL_MONTH_NAME(R_MONTH), &
                           FLOAT(R_DAY), &
                           CL_TRANS_NAME, &
                           (HOURLY_ENERGY(J,I),J=1,24)

                  UV_HOURLY_REC = UV_HOURLY_REC + 1
               ENDIF
            ENDDO
            IF(REPORT_DAILY_PRIME_MOVER .AND. DATE_ACTIVE) THEN
               IF(UK_REPORT_NOT_OPEN) THEN
                  UK_REPORT_NOT_OPEN = .FALSE.
                  UK_REPORT_VARIABLES = 36
                  UK_DAILY_NO = UK_DAILY_HEADER(UK_REPORT_VARIABLES, &
                                                UK_DAILY_REC)
                  if(UK_DAILY_NO<0) then
                    stop "UK_DAILY_NO"
                  end if
                  
               ENDIF
!               DO I = 1, 12
                  DO J = 1, SAVE_UPPER_TRANS_GROUP
! ENERGY AND HEAT CALC
                     K = MAX(INT(1,2),GET_TRANS_GROUP_INDEX(J))

                     RPM = FLOAT(I)
                     RTG = FLOAT(K)

                     LOCAL_GROUP_NAME = GET_GROUP_NAME(J)
                     IF(UNIT_ENERGY > 0.0001) THEN
                        AHR = UNIT_HEAT*1000.0 / UNIT_ENERGY
                     ELSE
                        AHR = 0.0
                     ENDIF
                     UK_DAILY_REC = RPTREC(UK_DAILY_NO)
                     WRITE(UK_DAILY_NO,REC=UK_DAILY_REC) &
                           PRT_ENDPOINT(), &
                           FLOAT(SAVE_YEAR), &
                           CL_MONTH_NAME(R_MONTH), &
                           FLOAT(R_DAY), &
                           LOCAL_GROUP_NAME, &
                           (DAILY_PM_TG_HEAT(I,K),I=1,12), &
                           (DAILY_PM_TG_ENERGY(I,K),I=1,12), &
                           (DAILY_PM_TG_CAPACITY(I,K),I=1,12)
                     UK_DAILY_REC = UK_DAILY_REC + 1
                  ENDDO ! J
!               ENDDO ! I
            ENDIF
         ELSE
            WRITE_DAILY_THERMAL_UNIT = .FALSE.
         ENDIF
         DAILY_ENERGY = 0.0
       call write_daily_energy_trace("margnobj:A002", daily_energy(1,1))
         DAILY_HEAT = 0.0
         DAILY_PM_TG_ENERGY = 0.0
         DAILY_PM_TG_CAPACITY = 0.0
         DAILY_PM_TG_HEAT = 0.0
         HOURLY_ENERGY = 0.0
      RETURN



      ENTRY RECALCULATE_OUTAGES()




!     GET LIST OF ALL UNITS ON-OUTAGE
!     SORT UNITS ON-OUTAGE ACCORDING TO DISPATCH ORDER



         RECALCULATE_OUTAGES = .TRUE.
      RETURN

      ENTRY CAL_MAR_MONTHLY_ENERGY_routine( ENERGY,BLKNO,UNIT, &
                           LEFT_HEAT,RIGHT_HEAT, &
                           REMAINING_ENERGY)
         if(file_trace_cmme==0) then
            file_trace_cmme=open_trace("cal_mar_monthly_energy.trace", &
                rq_cmme)
         end if
         numcmme=numcmme+1
         MONTHLY_ECONOMY_BOUGHT = 0.
         MONTHLY_ECONOMY_COST = 0.
         MONTHLY_ECONOMY_SOLD = 0.
         MONTHLY_ECONOMY_REVENUE = 0.
         IF( ABS(MONTHLY_MARKET_SALES - &
                                      SUM_AVERAGE_COST_SALES) > 1.) THEN
            DATA_BASES = DATA_BASES
         ENDIF
!
         cal_mar_monthly_energy_routine = 0.
         CUM_SALES_AFTER = 0.
!
         OUTAGE_DB_USED = 0

! 7/13/01.
! HEAT HAS ALREADY BEEN CALCULATED IN COMMITMENT ROUTINE.
!
         IF(.NOT. UNIT_COMMITMENT_LOGIC) THEN
            RIGHT_HEAT = 1.
!

         ENDIF
!
         CALL GET_DAYS_PER_TRANS_PERIOD(DAYS_PER_TRANS_PERIOD)
!
         DO DATA_BASES = 1, MAX_DATA_BASES
!
!

!
          IF(.NOT. ACTIVE_DATA_BASE(DATA_BASES)) CYCLE
!
          TRANS_GROUP = SAVE_TRANS_FOR_DATA_BASE(DATA_BASES)
          IF(TRANS_GROUP == 0) CYCLE ! EACH DAY TYPE HAS CENTRAL DISPATCH
           IF( DETAILED_OUTAGE_DATA_BASE(DATA_BASES) &
                                            .AND. OUTAGES_IN_MONTH) THEN
!
               IF(OUTAGE_DB_USED(TRANS_GROUP) > 0) THEN
                  IF(COMMITMENT_DB(TRANS_GROUP) > 0) THEN
                     CYCLE
                  ENDIF
               ELSE
                  OUTAGE_DB_USED(TRANS_GROUP) = 1
               ENDIF
               call check_hours_in_month(local_hours_IN_MONTH, "margnobj:0009")
               LOCAL_HOUR = LOCAL_HOURS_IN_MONTH + 1
               DO I = 1, LAST_DB_POINT(DATA_BASES)
                  R4_TEMP = &
                          CAL_MAR_HOURLY_ENERGY(DATA_BASES,I)
               ENDDO
!
               DO TEMP_BLOCK = 1, OUTAGE_BLOCKS_4_DB(DATA_BASES)
                  ! Exception access violation here.
                  OUTAGE_BLOCK = &
                                OUTAGE_BLOCK_N_DB(TEMP_BLOCK,DATA_BASES)
                  BLOCK = OUTAGE_BLOCK_INDEX(OUTAGE_BLOCK)
                  U = OUTAGE_UNIT_INDEX(OUTAGE_BLOCK)
                  B = MAX(INT(1,2),BLKNO(BLOCK))

                  START_UP_INDEX = GET_START_UP_INDEX(U)
                  IF(START_UP_INDEX == 0 .OR. &
                                      .NOT. UNIT_COMMITMENT_LOGIC) THEN

                     LEFT_HEAT(U) = 1.
                     RIGHT_HEAT(U) = 1.
               call check_hours_in_month(local_hours_IN_MONTH, "margnobj:0010")
                     ENERGY(1,U) = ENERGY(1,U) + &
                             OUTAGE_ENERGY(OUTAGE_BLOCK)/ &
                                           FLOAT(LOCAL_HOURS_IN_MONTH)

                     MON_ECO_SALES_ENRG_FROM(U) = &
                         MON_ECO_SALES_ENRG_FROM(U) + &
                             OUTAGE_WHOLESALE_ENERGY(OUTAGE_BLOCK,2) * &
                                                 DAYS_PER_TRANS_PERIOD
                     MON_ECO_SALES_REV_FROM(U) = &
                             MON_ECO_SALES_REV_FROM(U) + &
                             OUTAGE_WHOLESALE_REVENUE(OUTAGE_BLOCK,2) * &
                                                 DAYS_PER_TRANS_PERIOD
! 6/28/02.
                     MON_ECO_PUCH_ENRG_FROM(U) = &
                         MON_ECO_PUCH_ENRG_FROM(U) + &
                             OUTAGE_WHOLESALE_ENERGY(OUTAGE_BLOCK,1) * &
                                                 DAYS_PER_TRANS_PERIOD
                     MON_ECO_PUCH_COST_FROM(U) = &
                             MON_ECO_PUCH_COST_FROM(U) + &
                             OUTAGE_WHOLESALE_REVENUE(OUTAGE_BLOCK,1) * &
                                                 DAYS_PER_TRANS_PERIOD

                  ENDIF

               ENDDO
           ELSE
            BASE_WHOLESALE_INTERVAL = 9999
            BASE_RETAIL_INTERVAL = 9999
            CUM_LOAD = 0.
            CUM_GEN = 0.
            LOST_ENERGY = 0.
            DO I = 1, LAST_DB_POINT(DATA_BASES)

               IF(CUM_UNIT_UTIL(I,DATABASE) > 0.0 .AND. &
                                       TRANSACTION_PERIOD == 'S') THEN
                   CUM_UNIT_UTIL(I,DATABASE) = 4.0 * &
                                              CUM_UNIT_UTIL(I,DATABASE)

               ENDIF

               SALES_B4_AFTER(I,1,DATA_BASES) = DAYS_PER_TRANS_PERIOD * &
                                         SALES_B4_AFTER(I,1,DATA_BASES)
               SALES_B4_AFTER(I,2,DATA_BASES) = DAYS_PER_TRANS_PERIOD * &
                                         SALES_B4_AFTER(I,2,DATA_BASES)
               WHOLESALE_REV_AND_EXP(I,1,DATA_BASES) = &
                        WHOLESALE_REV_AND_EXP(I,1,DATA_BASES) * &
                                                  DAYS_PER_TRANS_PERIOD
               WHOLESALE_REV_AND_EXP(I,2,DATA_BASES) = &
                        WHOLESALE_REV_AND_EXP(I,2,DATA_BASES) * &
                                                  DAYS_PER_TRANS_PERIOD
               WHOLESALE_SAL_AND_PUR(I,1,DATA_BASES) = &
                        WHOLESALE_SAL_AND_PUR(I,1,DATA_BASES) * &
                                                  DAYS_PER_TRANS_PERIOD
               WHOLESALE_SAL_AND_PUR(I,2,DATA_BASES) = &
                        WHOLESALE_SAL_AND_PUR(I,2,DATA_BASES) * &
                                                  DAYS_PER_TRANS_PERIOD
!
               IF( ABS(SALES_B4_AFTER(I,1,DATA_BASES)) < 0.01 .AND. &
                   ABS(SALES_B4_AFTER(I,2,DATA_BASES)) < 0.01) CYCLE
!
               PERCENT_HOURS_IN_MONTH(1) = &
                     SALES_B4_AFTER(I,1,DATA_BASES) / &
                                            FLOAT(LOCAL_HOURS_IN_MONTH)
               PERCENT_HOURS_IN_MONTH(2) = &
                     SALES_B4_AFTER(I,2,DATA_BASES) / &
                                            FLOAT(LOCAL_HOURS_IN_MONTH)
!
               CUM_LOAD = CUM_LOAD + &
                    SALES_B4_AFTER(I,2,DATA_BASES) * &
                                    CAPACITY_GIVEN_MARKET(I,DATA_BASES)

               NPC = 1
               IF(BASE_WHOLESALE_INTERVAL == 9999 .AND. &
                               PERCENT_HOURS_IN_MONTH(2) > .000001) THEN
                  BASE_WHOLESALE_INTERVAL = I
               ENDIF
               IF(BASE_RETAIL_INTERVAL == 9999 .AND. &
                               PERCENT_HOURS_IN_MONTH(1) > .000001) THEN
                  BASE_RETAIL_INTERVAL = I
               ENDIF
!
!
               DO BLOCK = 1, NBLOK2_SAVE
!
!
                  DATABASE = COST_CURVE_POINTER_BY(BLOCK,DATA_BASES)
                  IF(DATABASE == 0) then
                     ! THIS DATABASE HAS NO INFORMATION 
                     ! FOR THIS DISPATCH POINT
                       CYCLE  
                  end if
!

! MOVED FROM BELOW CUM_GEN
!
                  U = UNIT(BLOCK)
                  B = MAX(INT(1,2),BLKNO(BLOCK))
!
                  IF(CAPACITY_GIVEN_MARKET(I,DATA_BASES) > 0.d0) THEN
                     UNIT_CONTRIB = &
                           CUM_UNIT_UTIL(I,DATABASE)/ &
                                     CAPACITY_GIVEN_MARKET(I,DATA_BASES)
                  ELSE
                     UNIT_CONTRIB = 0.
                  ENDIF

                  IF(BLOCK > BLOCK_B4_MARKET(I,DATA_BASES)) THEN
                     LOST_ENERGY = LOST_ENERGY + &
                           CUM_UNIT_UTIL(I,DATABASE) * &
                                          SALES_B4_AFTER(I,2,DATA_BASES)
!                     CYCLE
                  ENDIF
!
                  IF(PRICE_ONLY_WHOLESALE_REV .AND. &
                                      BASE_RETAIL_INTERVAL /= 9999) THEN

!
                     MON_ECO_SALES_REV_FROM(U) = &
                           MON_ECO_SALES_REV_FROM(U) + &
                              UNIT_CONTRIB * &
                                WHOLESALE_REV_AND_EXP(I,1,DATA_BASES)
                     MON_ECO_SALES_ENRG_FROM(U) = &
                           MON_ECO_SALES_ENRG_FROM(U) + &
                              UNIT_CONTRIB * &
                                WHOLESALE_SAL_AND_PUR(I,1,DATA_BASES)
                     MON_ECO_PUCH_COST_FROM(U) = &
                           MON_ECO_PUCH_COST_FROM(U) + &
                              UNIT_CONTRIB * &
                                   WHOLESALE_REV_AND_EXP(I,2,DATA_BASES)
                     MON_ECO_PUCH_ENRG_FROM(U) = &
                           MON_ECO_PUCH_ENRG_FROM(U) + &
                              UNIT_CONTRIB * &
                                   WHOLESALE_SAL_AND_PUR(I,2,DATA_BASES)
!
!
! TEMP CODE. 12/15/98. GAT.
!
                  ELSEIF(PRICE_ONLY_WHOLESALE_REV .AND. &
                                   BASE_WHOLESALE_INTERVAL /= 9999) THEN
                     MON_ECO_SALES_REV_FROM(U) = &
                           MON_ECO_SALES_REV_FROM(U) + &
                              UNIT_CONTRIB * &
                                WHOLESALE_REV_AND_EXP(I,1,DATA_BASES)
                     MON_ECO_SALES_ENRG_FROM(U) = &
                           MON_ECO_SALES_ENRG_FROM(U) + &
                              UNIT_CONTRIB * &
                                WHOLESALE_SAL_AND_PUR(I,1,DATA_BASES)
                     MON_ECO_PUCH_COST_FROM(U) = &
                           MON_ECO_PUCH_COST_FROM(U) + &
                              UNIT_CONTRIB * &
                                   WHOLESALE_REV_AND_EXP(I,2,DATA_BASES)
                     MON_ECO_PUCH_ENRG_FROM(U) = &
                           MON_ECO_PUCH_ENRG_FROM(U) + &
                              UNIT_CONTRIB * &
                                   WHOLESALE_SAL_AND_PUR(I,2,DATA_BASES)
                  ELSEIF(BASE_WHOLESALE_INTERVAL /= 9999) THEN
!
                     MON_ECO_SALES_REV_FROM(U) = &
                           MON_ECO_SALES_REV_FROM(U) + &
                              UNIT_CONTRIB * &
                                   WHOLESALE_REV_AND_EXP(I,1,DATA_BASES)
                     MON_ECO_SALES_ENRG_FROM(U) = &
                           MON_ECO_SALES_ENRG_FROM(U) + &
                              UNIT_CONTRIB * &
                                   WHOLESALE_SAL_AND_PUR(I,1,DATA_BASES)
                  ENDIF
!
                  U = UNIT(BLOCK)
                  B = MAX(INT(1,2),BLKNO(BLOCK))
!
                  IF(NPC <= NEW_BLOCK_NUMBER( &
                                   I,CONTRIB_INTERVALS,DATA_BASES)) THEN
                     CURRENT_BLOCK_NUMBER = &
                                      NEW_BLOCK_NUMBER(I,NPC,DATA_BASES)
!
                     IF(I == BASE_WHOLESALE_INTERVAL) THEN
                        LEFT_HEAT(U) = 1.
                     ELSEIF(BLOCK >= CURRENT_BLOCK_NUMBER) THEN
                        LEFT_HEAT(U) = &
                              MAX(LEFT_HEAT(U), 1. - &
                                 CUM_SALES_AFTER(DATA_BASES)/ &
                                            FLOAT(LOCAL_HOURS_IN_MONTH))
                        RIGHT_HEAT(U) = &
                           MAX(0.,MIN(RIGHT_HEAT(U), 1. - &
                                 CUM_SALES_AFTER(DATA_BASES)/ &
                                           FLOAT(LOCAL_HOURS_IN_MONTH)))
                     ELSEIF(NPC > 1 .AND. BLOCK > &
                                  NEW_BLOCK_NUMBER(I,1,DATA_BASES)) THEN
                        LEFT_HEAT(U) = LEFT_HEAT(U)
                     ENDIF
                  ENDIF
                  IF(BLOCK == &
                       NEW_BLOCK_NUMBER(I,NPC,DATA_BASES)) NPC = NPC + 1


                  TEMP_ENERGY = CUM_UNIT_UTIL(I,DATABASE) * &
                                PERCENT_HOURS_IN_MONTH(2)
                                
              if(lastcuu/=CUM_UNIT_UTIL(I,DATABASE) .or. lastphm/= &
                PERCENT_HOURS_IN_MONTH(2)) then
                lastcuu=CUM_UNIT_UTIL(I, DATABASE)
                lastphm=PERCENT_HOURS_IN_MONTH(2)
                 ! Direct writes are faster due to fewer lines
                 if(file_trace_cmme/=BAD_TRACE_HANDLE) then
                     write(file_trace_cmme, *) "CUU ", &
                        CUM_UNIT_UTIL(I,DATABASE),  &
                        " PHM ", &
                        PERCENT_HOURS_IN_MONTH(2)
                 end if
             end if

                  
                  ENERGY(B,U) = ENERGY(B,U) + TEMP_ENERGY

                  REMAINING_ENERGY = REMAINING_ENERGY - TEMP_ENERGY
!
                  UNIT_DEVIATION = &
                           CUM_UNIT_UTIL(I,DATABASE) * &
                                             PERCENT_HOURS_IN_MONTH(2) - &
                              UNIT_CONTRIB * &
                                  WHOLESALE_SAL_AND_PUR(I,1,DATA_BASES)/ &
                                             FLOAT(LOCAL_HOURS_IN_MONTH)
                  IF(ABS(UNIT_DEVIATION) > .001) THEN
                     UNIT_DEVIATION = UNIT_DEVIATION
                  ENDIF
!
                  CUM_GEN = CUM_GEN + &
                              CUM_UNIT_UTIL(I,DATABASE) * &
                                    SALES_B4_AFTER(I,2,DATA_BASES)

!

               ENDDO ! BLOCK COUNTER
!
               CUM_SALES_AFTER(DATA_BASES) = &
                                  CUM_SALES_AFTER(DATA_BASES) + &
                                          SALES_B4_AFTER(I,2,DATA_BASES)
!
               CUM_DIFF = CUM_GEN - CUM_LOAD
               IF(ABS(CUM_DIFF) > 0.1) THEN
                  CUM_DIFF = CUM_DIFF
               ENDIF
!
            ENDDO ! TRANS_GROUP_POINTS
           ENDIF ! EVENT CALENDAR CALCULATIONS
!
         ENDDO ! DATA_BASES
!
! ADDED 3/25/00. FOR ELECTRICITIES.
!
         IF(ECITY) THEN
            IF(RESERVE_CAP_NO(1) > 0) THEN
!               CALL GET_MONTHLY_DUKE_RESERVE_ENERGY(TEMP_ENERGY)
               TEMP_ENERGY = GET_MONTHLY_DUKE_RESERVE_ENERGY()
               TEMP_ENERGY = TEMP_ENERGY / LOCAL_HOURS_IN_MONTH
               B = 1
               ENERGY(B,RESERVE_CAP_NO(1)) = &
                               ENERGY(B,RESERVE_CAP_NO(1)) + TEMP_ENERGY

                 
               REMAINING_ENERGY = REMAINING_ENERGY - TEMP_ENERGY
            ELSEIF(RESERVE_CAP_NO(2) > 0) THEN
               CALL GET_MONTHLY_CPL_RESERVE_ENERGY(TEMP_ENERGY)
               TEMP_ENERGY = TEMP_ENERGY / LOCAL_HOURS_IN_MONTH
               B = 1
               ENERGY(B,RESERVE_CAP_NO(2)) = &
                               ENERGY(B,RESERVE_CAP_NO(2)) + TEMP_ENERGY

            ELSEIF(SUPPLEMENTAL_CAP_NO(1) > 0) THEN
               CALL GET_MONTHLY_DUKE_SUP_ENERGY(TEMP_ENERGY)
               TEMP_ENERGY = TEMP_ENERGY / LOCAL_HOURS_IN_MONTH
               B = 1
               ENERGY(B,SUPPLEMENTAL_CAP_NO(1)) = &
                          ENERGY(B,SUPPLEMENTAL_CAP_NO(1)) + TEMP_ENERGY

               REMAINING_ENERGY = REMAINING_ENERGY - TEMP_ENERGY
            ELSEIF(SUPPLEMENTAL_CAP_NO(2) > 0) THEN
               CALL GET_MONTHLY_CPL_SUP_ENERGY(TEMP_ENERGY)
               TEMP_ENERGY = TEMP_ENERGY / LOCAL_HOURS_IN_MONTH
               B = 1
               ENERGY(B,SUPPLEMENTAL_CAP_NO(2)) = &
                          ENERGY(B,SUPPLEMENTAL_CAP_NO(2)) + TEMP_ENERGY

               REMAINING_ENERGY = REMAINING_ENERGY - TEMP_ENERGY
            ENDIF
         ENDIF
!
!
         ANNUAL_ECONOMY_BOUGHT = ANNUAL_ECONOMY_BOUGHT + &
                                                  MONTHLY_ECONOMY_BOUGHT
         ANNUAL_ECONOMY_COST = ANNUAL_ECONOMY_COST + &
                                                    MONTHLY_ECONOMY_COST
         ANNUAL_ECONOMY_SOLD = ANNUAL_ECONOMY_SOLD     + &
                                                    MONTHLY_ECONOMY_SOLD
         ANNUAL_ECONOMY_REVENUE = ANNUAL_ECONOMY_REVENUE + &
                                                 MONTHLY_ECONOMY_REVENUE
         IF(YES_DAILY_THERMAL_RPT_ACTIVE) THEN
            DO U = 1, NUNITS_SAVE
               LEFT_HEAT(U) = MONTH_HEAT(1,U)/ &
                                              FLOAT(SAVE_HOURS_IN_MONTH)
               RIGHT_HEAT(U) = MONTH_HEAT(2,U)/ &
                                              FLOAT(SAVE_HOURS_IN_MONTH)

            ENDDO
         ENDIF
      RETURN

      ENTRY CAL_MAR_HOURLY_ENERGY(R_I,R_NEW_POINT) &
               RESULT(R_CAL_MAR_HOURLY_ENERGY)

         R_CAL_MAR_HOURLY_ENERGY = 0.0
         CAL_MAR_HOURLY_WHOLESALE = 0.0


         DO TEMP_BLOCK = 1, OUTAGE_BLOCKS_4_DB(R_I)
            OUTAGE_BLOCK = OUTAGE_BLOCK_N_DB(TEMP_BLOCK,R_I)

            BLOCK = OUTAGE_BLOCK_INDEX(OUTAGE_BLOCK)
            U = OUTAGE_UNIT_INDEX(OUTAGE_BLOCK)
            DATABASE = COST_CURVE_POINTER_BY(BLOCK,R_I)
! TEST WHOLESALE


            IF( ABS(WHOLESALE_SAL_AND_PUR(R_NEW_POINT,1,R_I)) > 1.) THEN

                  IF(CAPACITY_GIVEN_MARKET(R_NEW_POINT,R_I) > 0.d0) THEN
                     UNIT_CONTRIB = &
                           CUM_UNIT_UTIL(R_NEW_POINT,DATABASE)/ &
                                  CAPACITY_GIVEN_MARKET(R_NEW_POINT,R_I)
                  ELSE
                     UNIT_CONTRIB = 0.
                  ENDIF
                  IF(UNIT_CONTRIB > 1.001) THEN
                     WRITE(4,*) "PROBLEM IN CREATING HOURLY TRANSACTION"
                     WRITE(4,*) "OUTAGES."
                     WRITE(4,*) '*** line 4877 MARGNOBJ.FOR ***'
                     er_message='See WARNING MESSAGES -MARGNOBJ.FOR-11'
                     call end_program(er_message)
                  ENDIF
                  OUTAGE_WHOLESALE_ENERGY(OUTAGE_BLOCK,2) = &
                             OUTAGE_WHOLESALE_ENERGY(OUTAGE_BLOCK,2) + &
                              UNIT_CONTRIB * &
                                WHOLESALE_SAL_AND_PUR(R_NEW_POINT,1,R_I)
                  CAL_MAR_HOURLY_WHOLESALE = &
                             CAL_MAR_HOURLY_WHOLESALE + &
                              UNIT_CONTRIB * &
                                WHOLESALE_SAL_AND_PUR(R_NEW_POINT,1,R_I)
!
                  OUTAGE_WHOLESALE_REVENUE(OUTAGE_BLOCK,2) = &
                             OUTAGE_WHOLESALE_REVENUE(OUTAGE_BLOCK,2) + &
                              UNIT_CONTRIB * &
                                WHOLESALE_REV_AND_EXP(R_NEW_POINT,1,R_I)
!
            ENDIF
            IF( ABS(WHOLESALE_SAL_AND_PUR(R_NEW_POINT,2,R_I)) > 1.) THEN
!
                  IF(CAPACITY_GIVEN_MARKET(R_NEW_POINT,R_I) > 0.d0) THEN
                     UNIT_CONTRIB = &
                           CUM_UNIT_UTIL(R_NEW_POINT,DATABASE)/ &
                                  CAPACITY_GIVEN_MARKET(R_NEW_POINT,R_I)
                  ELSE
                     UNIT_CONTRIB = 0.
                  ENDIF
                  IF(UNIT_CONTRIB > 1.001) THEN
                     WRITE(4,*) "PROBLEM IN CREATING HOURLY TRANSACTION"
                     WRITE(4,*) "OUTAGES."
                     WRITE(4,*) '*** line 4877 MARGNOBJ.FOR ***'
                     er_message='See WARNING MESSAGES -MARGNOBJ.FOR-12'
                     call end_program(er_message)
                  ENDIF
                  OUTAGE_WHOLESALE_ENERGY(OUTAGE_BLOCK,1) = &
                             OUTAGE_WHOLESALE_ENERGY(OUTAGE_BLOCK,1) + &
                              UNIT_CONTRIB * &
                                WHOLESALE_SAL_AND_PUR(R_NEW_POINT,2,R_I)
!
                  OUTAGE_WHOLESALE_REVENUE(OUTAGE_BLOCK,1) = &
                             OUTAGE_WHOLESALE_REVENUE(OUTAGE_BLOCK,1) + &
                              UNIT_CONTRIB * &
                                WHOLESALE_REV_AND_EXP(R_NEW_POINT,2,R_I)
!
            ENDIF
!
! SECOND ATTEMPT TO ADJUST CAPACITIES FOR TRANSACT OUTAGE FILE
! IMPLEMENT DECONVOLVE KIND OF LOGIC, TAKING MW'S OFF OF THE ORIGINAL
! CUM_UNIT_UTIL(I,DATABASE).

            IF (SALES_B4_AFTER(R_NEW_POINT,2,R_I) > 0. .AND. &
                                                    LOCAL_HOUR > 1) THEN
               IF(CUM_UNIT_UTIL(R_NEW_POINT,DATABASE) > 0.) THEN
!
                  IF(TRANSACTION_PERIOD == 'S') THEN
                     TEMP_ENERGY = 4.0 * &
                           CUM_UNIT_UTIL(R_NEW_POINT,DATABASE) * &
                                       SALES_B4_AFTER(R_NEW_POINT,2,R_I)
                  ELSE
                     TEMP_ENERGY = &
                           CUM_UNIT_UTIL(R_NEW_POINT,DATABASE) * &
                                       SALES_B4_AFTER(R_NEW_POINT,2,R_I)
                  ENDIF
                  OUTAGE_ENERGY(OUTAGE_BLOCK) = &
                             OUTAGE_ENERGY(OUTAGE_BLOCK) + TEMP_ENERGY
!
                  R_CAL_MAR_HOURLY_ENERGY = &
                                       R_CAL_MAR_HOURLY_ENERGY + &
                                                        TEMP_ENERGY
!
!
! NEED TO RECOMPUTE THE COST CURVES ABOVE.
!
! HEAT CALCULATIONS GO HERE. UNITS BELOW THE MARGIN HAVE FULL LOAD
! HEAT VALUES.  UNITS ABOVE THE MARGIN HAVE MARGINAL HEAT VALUES.
!
!                       LEFT() =
!                       RIGHT() =
!
! CALCULATE WHOLESALE MARKET BOUGHT AND SOLD, EXPENSE AND REVENUE HERE.
!
!
               ENDIF ! CUM_UNIT_UTIL > 0. ! UNIT CONTRIBUTES
            ENDIF ! SALES_B4_AFTER !
!
! THIS CODE RESETS THE CURVES FOR THE NEXT OUTAGE CONDITIONS.
!
!
! 3/29/02. TO INCORPORATE PARTIAL OUTAGES INTO THE NEW LOGIC
!
            IF(ADJUST_BLOCK(OUTAGE_BLOCK) >= 2.) THEN
               TEMP_OUTAGE = OUTAGE_SINGULAR(OUTAGE_BLOCK) * &
                                              DERATE_BLOCK(OUTAGE_BLOCK)
            ELSE

               TEMP_OUTAGE = OUTAGE_SINGULAR(OUTAGE_BLOCK)

            ENDIF
!
            CUM_UNIT_UTIL(R_NEW_POINT,DATABASE) = &
                           CURRENT_UNIT_UTIL(R_NEW_POINT,DATABASE) * &
                                                             TEMP_OUTAGE

            IF( ABS(TEMP_OUTAGE - 1.0) > .00001 .AND. &
                          CURRENT_UNIT_UTIL(R_NEW_POINT,DATABASE) > 0. &
                                                                  ) THEN
               CUM_UNIT_UTIL_zero(R_NEW_POINT,R_I) = &
                               CUM_UNIT_UTIL_zero(R_NEW_POINT,R_I) &
                               - CURRENT_UNIT_UTIL(R_NEW_POINT,DATABASE) &
                               + CUM_UNIT_UTIL(R_NEW_POINT,DATABASE)
            ENDIF
!
         ENDDO ! BLOCKS
!
!
         CAPACITY_GIVEN_MARKET(R_NEW_POINT,R_I) = &
                                     CUM_UNIT_UTIL_zero(R_NEW_POINT,R_I)

         IF(LOCAL_HOUR > 1) THEN
            SALES_B4_AFTER(R_NEW_POINT,2,R_I) = 0.0
            WHOLESALE_SAL_AND_PUR(R_NEW_POINT,1,R_I) = 0.
            WHOLESALE_REV_AND_EXP(R_NEW_POINT,1,R_I) = 0.
            WHOLESALE_SAL_AND_PUR(R_NEW_POINT,2,R_I) = 0.
            WHOLESALE_REV_AND_EXP(R_NEW_POINT,2,R_I) = 0.
         ENDIF

      RETURN


      ENTRY GET_BLOCK_OUTAGE_CAPACITY(R_UNIT,R_BLOCK,R_DATA_BASES)



         TEMP_I1 = UNIT_TO_BLOCK(R_UNIT,R_BLOCK)
         IF(R_BLOCK < 1 .OR. R_BLOCK > 2 .OR. &
                                             TEMP_I1 > NBLOK2_SAVE) THEN
            BLOCK = BLOCK
         ENDIF

         TEMP_I1 = OUTAGE_BLOCK_REVERSE_INDEX(TEMP_I1,R_DATA_BASES)
         IF(TEMP_I1 == 0) THEN
            GET_BLOCK_OUTAGE_CAPACITY = 0.
         ELSE
            GET_BLOCK_OUTAGE_CAPACITY = TRANS_BLOCK_CAPACITY(TEMP_I1)
         ENDIF
      RETURN


      ENTRY GET_UNIT_TO_OUTAGE_BLOCK(R_UNIT,R_BLOCK,R_DATA_BASES)



         TEMP_I1 = UNIT_TO_BLOCK(R_UNIT,R_BLOCK)
         IF(R_BLOCK < 1 .OR. R_BLOCK > 2 .OR. &
                                             TEMP_I1 > NBLOK2_SAVE) THEN
            BLOCK = BLOCK
         ENDIF

         GET_UNIT_TO_OUTAGE_BLOCK = &
                        OUTAGE_BLOCK_REVERSE_INDEX(TEMP_I1,R_DATA_BASES)
      RETURN


      ENTRY GET_TRANS_UNIT_TO_BLOCK(R_UNIT,R_BLOCK)



         GET_TRANS_UNIT_TO_BLOCK = UNIT_TO_BLOCK(R_UNIT,R_BLOCK)
      RETURN


!
      ENTRY GET_OUTAGE_CAPACITY(R_BLOCK)


         I = 2 ! THIS WILL CHANGE
         GET_OUTAGE_CAPACITY = &
                         MW_AFTER_OUTAGES(OUTAGE_BLOCK_INDEX(R_BLOCK),I)
      RETURN



      ENTRY UPDATE_OUTAGE_DATA(R_HOUR)

!     AT THE BEGINNING OR ENDING (TO RESET) OF THE OUTAGE STATE





      UPDATE_OUTAGE_DATA = .FALSE.

      LOCAL_HOUR = R_HOUR

      DO I = 2, MAX_DATA_BASES
         IF(.NOT. DETAILED_OUTAGE_DATA_BASE(I)) CYCLE
         TG = SAVE_TRANS_FOR_DATA_BASE(I)
         IF(TG == 0) CYCLE

!
!     WHEN DOES THE NEXT OUTAGE OCCUR?
!     ENERGY CONTRIBUTIONS WILL BE CALCULATED HOURLY, INSTEAD OF MONTHLY
!
!     NEVER ACCUMULATE ENERGY/HEAT IN THE FIRST HOUR.
!     ALWAYS ACCUMULATE ENERGY/HEAT IN THE LAST HOUR.

         IF(FIRST_OUTAGE_HOUR_IN_MONTH) THEN ! INITIAL CONDITION
! ADDED HERE 7/1/02 FOR MODIFICATIONS IN CAL_MARGINAL_COST
            OUTAGE_SINGULAR = 1.
            FIRST_OUTAGE_HOUR_IN_MONTH = .FALSE.
            OUTAGE_HOURS_IN_PERIOD = 0.
            OUTAGES_IN_MONTH = .FALSE.
            ALLOCATE(OUTAGE_ENERGY(SAVE_OUTAGE_BLOCKS))
            ALLOCATE(OUTAGE_WHOLESALE_ENERGY(SAVE_OUTAGE_BLOCKS,2))
            ALLOCATE(OUTAGE_WHOLESALE_REVENUE(SAVE_OUTAGE_BLOCKS,2))

            DERATE_BLOCK = 0.

            OUTAGE_ENERGY = 0.
            OUTAGE_WHOLESALE_ENERGY = 0.
            OUTAGE_WHOLESALE_REVENUE = 0.
            CURRENT_UNIT_UTIL_zero = CUM_UNIT_UTIL_zero

            CURRENT_UNIT_UTIL = CUM_UNIT_UTIL


         ENDIF

         OUTAGE_HOURS_IN_PERIOD = OUTAGE_HOURS_IN_PERIOD + 1

! NEED A TRANSLATION/MAPPING OF DATA BASES TO OUTAGES
! USING I INDEX FOR THE MOMENT.

         IF(   .NOT. ACTIVE_DATA_BASE(I) .OR. &
                              SAVE_OUTAGE_BLOCKS < 1) CYCLE

         TG = GET_TRANS_GROUP_POSITION(TG)

         IF( .NOT. OUTAGE_EVENTS_IN_HOUR(R_HOUR,TG)) CYCLE

!     CHANGES THE STATE BY BLOCK FOR EACH UNIT ON-TO-OFF, OFF-TO-ON, ETC.

!         IF (OUTAGE_EVENTS_IN_HOUR(R_HOUR,TG)) THEN
!
! MOVED 6/13/02.
!
         ADJUST_BLOCK = 0.
!
         OUTAGES_ADJUSTED = ADJUST_FOR_OUTAGES( &
                                                R_HOUR, &
                                                OUTAGE_SINGULAR, &
                                                TG, &
                                                I, &
                                                OUTAGE_TYPE, &
                                                ADJUST_BLOCK, &
                                                DERATE_BLOCK)
!
!         ELSE
!            OUTAGES_ADJUSTED = .FALSE.
!         ENDIF
!
         IF(.NOT. OUTAGES_ADJUSTED) CYCLE
!
         OUTAGES_IN_MONTH = .TRUE.
!
! 09/11/03. MUST RESET HOURLY.
!
         CUM_MUST_RUN_MW(I) = 0.
         CUM_MUST_RUN_POSITION(I) = 0
!
! 09/11/03. RECALCULATE MUST RUN.
!
         DO TEMP_BLOCK = 1, OUTAGE_BLOCKS_4_DB(I)
            OUTAGE_BLOCK = OUTAGE_BLOCK_N_DB(TEMP_BLOCK,I)

            BLOCK = OUTAGE_BLOCK_INDEX(OUTAGE_BLOCK)
            IF(BLKNO_SAVE(BLOCK) == 0) THEN
               IF(ADJUST_BLOCK(OUTAGE_BLOCK) >= 2.) THEN
                  TEMP_OUTAGE = OUTAGE_SINGULAR(OUTAGE_BLOCK) * &
                                              DERATE_BLOCK(OUTAGE_BLOCK)
               ELSE
                  TEMP_OUTAGE = OUTAGE_SINGULAR(OUTAGE_BLOCK)
               ENDIF
               CUM_MUST_RUN_MW(I) = CUM_MUST_RUN_MW(I) + &
                                 TEMP_OUTAGE * MW_AFTER_OUTAGES(BLOCK,I)
            ELSE
               EXIT
            ENDIF
         ENDDO
!
! BIG-TIME TEST REMOVING I=2
!
!      I = 2
!
! VARIABLES THAT MUST BE INITIALIZED:
!

         DO NEW_POINT = 1, TRANS_GROUP_POINTS


            CUM_UNIT_UTIL_zero(NEW_POINT,I) = &
                                     CURRENT_UNIT_UTIL_zero(NEW_POINT,I)

            TOT_EMBED_COST_BY_POINT(NEW_POINT,I) = 0.
         ENDDO

         LAST_BLOCK_NUMBER = 0
!
         LAST_DB_POINT(I) = 0

!
         D_TG = SAVE_TRANS_FOR_DATA_BASE(I)
!
         VOID_LOGICAL = GET_SCARCITY_INFO(D_TG, &
                                          FIRST_CAPACITY_VALUE, &
                                          FIRST_CAPACITY_PERCENT, &
                                          SECOND_CAPACITY_VALUE, &
                                          SECOND_CAPACITY_PERCENT, &
                                          THIRD_CAPACITY_VALUE, &
                                          THIRD_CAPACITY_PERCENT, &
                                          SAVE_SEASON,globecom_YEAR, &
                                          CAPACITY_ADDER, &
                                          ADDITIONAL_CAPACITY_VALUE, &
                                          ADDITIONAL_CAPACITY_PERCENT)
!
         DO NEW_POINT = 1, TRANS_GROUP_POINTS
            IF(NEW_MARGINAL_DATA_BASE(NEW_POINT,CONTRIB_INTERVALS,I) &
                                                              > 0.) THEN
!
!               LAST_DB_POINT(I) = NEW_POINT ! CALCULATED BELOW
!
!
               CURRENT_BLOCK_NUMBER = &
                               MAX(LAST_BLOCK_NUMBER, &
                                        NEW_BLOCK_NUMBER(NEW_POINT,1,I))
               IF(BLKNO_SAVE(CURRENT_BLOCK_NUMBER) > 0) THEN
                  IF(NEW_POINT > 1) THEN
                     TRANSITION_MARGINAL_COST(NEW_POINT,I,1) = &
                      MAX(REAL(BLOCK_DISP_COST(CURRENT_BLOCK_NUMBER),8), &
                              TRANSITION_MARGINAL_COST(NEW_POINT-1,I,1))
                  ENDIF
               ELSE
                  TRANSITION_MARGINAL_COST(NEW_POINT,I,1) = &
                              MIN(BLOCK_DISP_COST(CURRENT_BLOCK_NUMBER), &
                                                       MUST_RUN_COST(I))
               ENDIF

               LAST_BLOCK_NUMBER = CURRENT_BLOCK_NUMBER
!
            ELSEIF(NEW_POINT > 1) THEN
!               OUTAGE_B4_MARKET(NEW_POINT) =
!     +                                   OUTAGE_B4_MARKET(NEW_POINT-1)
            ENDIF
!
! 09/30/00. GAT.
!
! ROUTINE CALCULATES GENERATION, WHOLESALE, AND UPDATES CURVES
!
            R4_TEMP = CAL_MAR_HOURLY_ENERGY(I,NEW_POINT)
!
!
            IF(CUM_MUST_RUN_MW(I) > 0. .AND. &
                     CUM_MUST_RUN_POSITION(I) == 0 .AND. REAL( &
                     CUM_MUST_RUN_MW(I),8) < &
                                CAPACITY_GIVEN_MARKET(NEW_POINT,I)) THEN
                  CUM_MUST_RUN_POSITION(I) = NEW_POINT - 1
            ENDIF
!
!               ENDIF
            IF(NEW_POINT <= 25 ) THEN
               DIVISOR = 8
            ELSEIF(NEW_POINT <= 75) THEN
               DIVISOR = 4
            ELSEIF(NEW_POINT <= 275) THEN
               DIVISOR = 2
            ENDIF
            INTERVAL_CAPACITY = FLOAT(DIVISOR) * DB_DX(I)
            IF( ABS( NEW_MARGINAL_DATA_BASE(NEW_POINT, &
                                                 CONTRIB_INTERVALS,I) - &
             (CURRENT_UNIT_UTIL_zero(NEW_POINT,I)/INTERVAL_CAPACITY) ) > &
                                                             .0001) THEN
               INTERVAL_CAPACITY = INTERVAL_CAPACITY ! GAPPING PROBLEM
            ENDIF
            IF(NEW_POINT == TRANS_GROUP_POINTS .AND. &
                          CURRENT_UNIT_UTIL_zero(NEW_POINT,I) > 0.) THEN
               DB_CAP_AFTER_OUTAGES(I) = &
                                     CURRENT_UNIT_UTIL_zero(NEW_POINT,I)
               LAST_DB_POINT(I) = NEW_POINT
            ELSEIF(NEW_POINT > 1 .AND. &
                     CURRENT_UNIT_UTIL_zero(NEW_POINT,I) == 0 &
                   .AND. CURRENT_UNIT_UTIL_zero(NEW_POINT-1,I) > 0) THEN
               DB_CAP_AFTER_OUTAGES(I) = &
                                   CURRENT_UNIT_UTIL_zero(NEW_POINT-1,I)
               LAST_DB_POINT(I) = NEW_POINT - 1
            ENDIF
         ENDDO ! NEW_POINT
!
! MODIFIED TO CAPTURE SCARCITY FUNCTION CHANGES DUE TO OUTAGES. 5/3/01.
!
         TOTAL_SCARCITY_MW = &
                    MAX(CAPACITY_GIVEN_MARKET(LAST_DB_POINT(I),I),.01d0)
!
!         TOTAL_SCARCITY_MW = DB_CAP_AFTER_OUTAGES(I) + CAPACITY_ADDER
!
         FIRST_CAPACITY_MW = TOTAL_SCARCITY_MW * &
                                             FIRST_CAPACITY_PERCENT/100.
         SECOND_CAPACITY_MW = TOTAL_SCARCITY_MW * &
                                            SECOND_CAPACITY_PERCENT/100.
         THIRD_CAPACITY_MW = TOTAL_SCARCITY_MW * &
                                             THIRD_CAPACITY_PERCENT/100.
         DO S = 1, 7
            ADDITIONAL_CAPACITY_MW(S) = &
                     TOTAL_SCARCITY_MW * &
                              ADDITIONAL_CAPACITY_PERCENT(S)/100.
            NEW_SCARCITY_LOGIC = ADDITIONAL_CAPACITY_MW(1) > 0. .AND. &
                                    ADDITIONAL_CAPACITY_VALUE(1) > 0.
         ENDDO
!
         DO NEW_POINT = 1, LAST_DB_POINT(I)
!
!           SCARCITY FUNCTION
!
            TEMP_CAP = MAX(CAPACITY_GIVEN_MARKET(NEW_POINT,I),.01d0)
!
            IF(TEMP_CAP < FIRST_CAPACITY_MW) THEN
               TRANSITION_MARGINAL_COST(NEW_POINT,I,2) = 0.
            ELSEIF(TEMP_CAP > THIRD_CAPACITY_MW) THEN
               TRANSITION_MARGINAL_COST(NEW_POINT,I,2) = &
                                                    THIRD_CAPACITY_VALUE
            ELSEIF(TEMP_CAP >= SECOND_CAPACITY_MW) THEN
               IF(THIRD_CAPACITY_MW - SECOND_CAPACITY_MW > .001) THEN
                  SCARCITY_PERCENT = (TEMP_CAP - SECOND_CAPACITY_MW) / &
                                (THIRD_CAPACITY_MW - SECOND_CAPACITY_MW)
               ELSE
                  SCARCITY_PERCENT = 0.
               ENDIF
               TRANSITION_MARGINAL_COST(NEW_POINT,I,2) = &
                  (1.- SCARCITY_PERCENT) * SECOND_CAPACITY_VALUE + &
                                 SCARCITY_PERCENT * THIRD_CAPACITY_VALUE
            ELSE
               SCARCITY_PERCENT = (TEMP_CAP - FIRST_CAPACITY_MW) / &
                                (SECOND_CAPACITY_MW - FIRST_CAPACITY_MW)
               TRANSITION_MARGINAL_COST(NEW_POINT,I,2) = &
                  (1.- SCARCITY_PERCENT) * FIRST_CAPACITY_VALUE + &
                                SCARCITY_PERCENT * SECOND_CAPACITY_VALUE
            ENDIF

            IF(NEW_SCARCITY_LOGIC .AND. &
                                      TEMP_CAP > THIRD_CAPACITY_MW) THEN
!
! TRAP THE BOUNDARIES, JUST LIKE ABOVE. ELSE, FIND THE INTERVAL (1,6)
!
               IF(TEMP_CAP < ADDITIONAL_CAPACITY_MW(1)) THEN
                  IF(ADDITIONAL_CAPACITY_MW(1) - &
                                           THIRD_CAPACITY_MW > .01) THEN
                     SCARCITY_PERCENT = (TEMP_CAP - THIRD_CAPACITY_MW) / &
                         (ADDITIONAL_CAPACITY_MW(1) - THIRD_CAPACITY_MW)
                  ELSE
                     SCARCITY_PERCENT = 0.
                  ENDIF
                  TRANSITION_MARGINAL_COST(NEW_POINT,I,2) = &
                     (1.- SCARCITY_PERCENT) * THIRD_CAPACITY_VALUE + &
                      SCARCITY_PERCENT * ADDITIONAL_CAPACITY_VALUE(1)
               ELSEIF(TEMP_CAP > ADDITIONAL_CAPACITY_MW(7)) THEN
                  TRANSITION_MARGINAL_COST(NEW_POINT,I,2) = &
                                         ADDITIONAL_CAPACITY_VALUE(7)
               ELSE
                  DO S = 1, 6
                     IF(TEMP_CAP - &
                                ADDITIONAL_CAPACITY_MW(S+1) > .01) CYCLE
                     IF(ADDITIONAL_CAPACITY_MW(S+1) - &
                                    ADDITIONAL_CAPACITY_MW(S) > .01)THEN
                        SCARCITY_PERCENT = &
                           (TEMP_CAP - ADDITIONAL_CAPACITY_MW(S)) / &
                                 (ADDITIONAL_CAPACITY_MW(S+1) - &
                                              ADDITIONAL_CAPACITY_MW(S))
                     ELSE
                        SCARCITY_PERCENT = 0.
                     ENDIF
                     TRANSITION_MARGINAL_COST(NEW_POINT,I,2) = &
                        (1.- SCARCITY_PERCENT) * &
                                       ADDITIONAL_CAPACITY_VALUE(S) + &
                        SCARCITY_PERCENT * &
                                       ADDITIONAL_CAPACITY_VALUE(S+1)
                     EXIT
                  ENDDO
                  IF(S == 7) THEN
                     WRITE(4,*) "Untrapped value in new scarcity"
                     WRITE(4,*) "function.  No scarcity value used."
                  ENDIF
               ENDIF ! BOUNDARIES
            ENDIF ! NEW_SCARCITY_LOGIC
!
            TRANSITION_MARGINAL_COST(NEW_POINT,I,3) = &
                           TRANSITION_MARGINAL_COST(NEW_POINT,I,1) + &
                                 TRANSITION_MARGINAL_COST(NEW_POINT,I,2)

         ENDDO
! 05/07/03. TECHNOLOGY SCARCITY
         IF(YES_TECHNOLOGY_SCARCITY) THEN

! FIND TECH BREAKS AND STRENGTH OF THE BREAK

            TECH_BREAK_INTERVAL = 0
            TECH_BREAK_MW = 0.
            TECH_BREAK_STRENGTH = 0.


            TECH_BREAK_COUNT = 1
            TECH_BREAK_INTERVAL(TECH_BREAK_COUNT) = 1
            TECH_BREAK_MW(TECH_BREAK_COUNT) = 0.

            DO NEW_POINT = 2, TRANS_GROUP_POINTS
               TECH_BREAK_TEST = &
                  TRANSITION_MARGINAL_COST(NEW_POINT,I,1)/ &
                          TRANSITION_MARGINAL_COST(NEW_POINT-1,I,1) - 1.
               IF(TECH_BREAK_TEST > .20) THEN ! ARBITRARY
                  TECH_BREAK_COUNT = TECH_BREAK_COUNT + 1
                  IF(TECH_BREAK_COUNT <= 20) THEN
                     TECH_BREAK_INTERVAL(TECH_BREAK_COUNT) = NEW_POINT
                     TECH_BREAK_MW(TECH_BREAK_COUNT) = &
                                      CAPACITY_GIVEN_MARKET(NEW_POINT,I)
                     TECH_BREAK_STRENGTH(TECH_BREAK_COUNT) = &
                                                         TECH_BREAK_TEST
                  ELSE
                     WRITE(4,*) "EXCEEDED TECH BREAK POINTS"
                     TECH_BREAK_COUNT = 20

                  ENDIF
               ENDIF
               TRANSITION_MARGINAL_COST(NEW_POINT,I,4) = 0.
               TRANSITION_MARGINAL_COST(NEW_POINT,I,5) = 0.
            ENDDO
!

            IF(TECH_PERCENT_TRANS_CAP) THEN
               TECH_BREAK_TEST = TECH_PERCENT_INPUT * &
                             CAPACITY_GIVEN_MARKET(TRANS_GROUP_POINTS,I)
            ELSE
            ENDIF
!
            IF(TECH_BREAK_TEST <= 0.) THEN
               TECH_BREAK_TEST = .00001
            ENDIF
!
            DO J = 2, TECH_BREAK_COUNT
               DO NEW_POINT = 1, TECH_BREAK_INTERVAL(J) - 1
                  IF(CAPACITY_GIVEN_MARKET(NEW_POINT,I) < &
                               TECH_BREAK_MW(J) - TECH_BREAK_TEST) CYCLE
! (0,1)
                  TECH_BREAK_MW_PERCENT_CHANGE = &
                        MAX(0.,REAL((TECH_BREAK_TEST - &
                              TECH_BREAK_MW(J) + &
                                  CAPACITY_GIVEN_MARKET(NEW_POINT,I))/ &
                                                       TECH_BREAK_TEST))
!
! 05/08/03. TOM PREFERS THE SQUARED TERM
!
                  TECH_BREAK_MW_PERCENT_CHANGE = &
                        TECH_BREAK_MW_PERCENT_CHANGE * &
                              TECH_BREAK_MW_PERCENT_CHANGE * &
                                               TECH_BREAK_STRENGTH(J)
                  TRANSITION_MARGINAL_COST(NEW_POINT,I,4) = &
                     TRANSITION_MARGINAL_COST(NEW_POINT,I,4) + &
                    MAX(0.,REAL(TRANSITION_MARGINAL_COST(NEW_POINT,I,1)* &
                                          TECH_BREAK_MW_PERCENT_CHANGE))
               ENDDO
            ENDDO
            DO NEW_POINT = 1, TRANS_GROUP_POINTS
               TRANSITION_MARGINAL_COST(NEW_POINT,I,5) = &
                          TRANSITION_MARGINAL_COST(NEW_POINT,I,3) + &
                                 TRANSITION_MARGINAL_COST(NEW_POINT,I,4)
               TRANSITION_MARGINAL_COST(NEW_POINT,I,4) = &
                          TRANSITION_MARGINAL_COST(NEW_POINT,I,1) + &
                                 TRANSITION_MARGINAL_COST(NEW_POINT,I,4)
            ENDDO
         ENDIF
!
         UPDATE_OUTAGE_DATA = .TRUE.
!
         OUTAGE_HOURS_IN_PERIOD = 0.
!
      ENDDO ! DATA_BASES
!
      RETURN

!
      ! AT THE END OF THE OUTAGE STATE
      ENTRY CAL_OUTAGE_UNIT_GENERATION(R_TRANS_GROUP) 
!


         DO OUTAGE_BLOCK = 1, SAVE_OUTAGE_BLOCKS

            BLOCK = OUTAGE_BLOCK_INDEX(OUTAGE_BLOCK)
            U = OUTAGE_UNIT_INDEX(OUTAGE_BLOCK)

         ENDDO

         CAL_OUTAGE_UNIT_GENERATION = .TRUE.

      RETURN

      ENTRY GET_MAX_TRANS_GROUPS()

         GET_MAX_TRANS_GROUPS = MAX_TRANS_GROUPS
      RETURN

      ENTRY GET_MAX_DAY_TYPES()


         GET_MAX_DAY_TYPES = MIN(MAX_DAY_TYPES,GET_LAST_DAY_TYPE())
      RETURN

      ENTRY GET_MUST_RUN_CAPACITY(R_DATA_BASES)


         GET_MUST_RUN_CAPACITY = CUM_MUST_RUN_MW(R_DATA_BASES)
      RETURN

      ENTRY DEALLO_MONTH_MARG_COST_ARRAYS()


      DEALLO_MONTH_MARG_COST_ARRAYS = .TRUE.
      IF(ALLOCATED(ACTIVE_DATA_BASE)) then
            DEALLOCATE( ACTIVE_DATA_BASE,DETAILED_OUTAGE_DATA_BASE, & 
               ACTIVE_UNIT,START_UP_POSITION,MARGINAL_UNIT_BY_TECH, &
               SCARCITY_MULT,CURRENT,NEXT,POS,NATIVE_POS, &
               NEW_POINT_COUNT,NEW_BLOCK_NUMBER,LODDUR_FOR_MC, &
               LPROB_FOR_MC,CUM_CAP,LAST_DB_POINT,CumDbDx, &
               DB_ADJ_TOTAL_CAP,DB_CAP_AFTER_OUTAGES,DB_DX, &
               PREVIOUS_INTERVAL,I_CORRECT,NEW_LODDUR, &
               NEW_MARGINAL_DATA_BASE,EXPECTED_MARGINAL_COST, &
               TRANSITION_MARGINAL_COST,CAPACITY_GIVEN_MARKET, &
               BLOCK_B4_MARKET,CUM_COST,CUM_PROB,SAVE_LOAD, &
               NATIVE_POS_PERCENT,REMAINING_CONTRIBUTION_MW, &
               SALES_B4_AFTER,WHOLESALE_REV_AND_EXP, &
               WHOLESALE_SAL_AND_PUR,RETAIL_REV_AND_EXP, &
               RETAIL_SAL_AND_PUR,CUM_SALES_AFTER, &
               CUM_INTERVAL_CAPACITY,CUM_MUST_RUN_MW, &
               CUM_MUST_RUN_POSITION)
         end if
         IF(ALLOCATED(UNIT_ENERGY_B4_AFTER)) &
                                        DEALLOCATE(UNIT_ENERGY_B4_AFTER)
         IF(ALLOCATED(UNIT_DISP_COST)) &
                              DEALLOCATE(UNIT_DISP_COST,BLOCK_DISP_COST)
         IF(ALLOCATED(SAVE_TRANS_FOR_DATA_BASE)) &
                              DEALLOCATE(SAVE_TRANS_FOR_DATA_BASE)
         IF(ALLOCATED(FIRST_ECONOMY_COST))DEALLOCATE(FIRST_ECONOMY_COST, &
                                                          MUST_RUN_COST)
         IF(ALLOCATED(MW_AFTER_OUTAGES)) DEALLOCATE(MW_AFTER_OUTAGES)
         IF(ALLOCATED(MW_DIFF)) DEALLOCATE(MW_DIFF)
         IF(ALLOCATED(OUTAGE_UNIT_INDEX)) DEALLOCATE(OUTAGE_UNIT_INDEX)
         IF(ALLOCATED(CUM_TC_UNIT_MWH)) DEALLOCATE(CUM_TC_UNIT_MWH)
         IF(ALLOCATED(OUTAGE_BLOCK_INDEX)) &
                                          DEALLOCATE(OUTAGE_BLOCK_INDEX)
         IF(ALLOCATED(TRANS_BLOCK_CAPACITY)) &
                                        DEALLOCATE(TRANS_BLOCK_CAPACITY)
         IF(ALLOCATED(UNIT_SAVE)) DEALLOCATE(UNIT_SAVE,BLKNO_SAVE)
         IF(ALLOCATED(START_UP_UNIT_BY_TG)) &
                                        DEALLOCATE (START_UP_UNIT_BY_TG)
         IF(ALLOCATED(OUTAGE_DB_USED)) DEALLOCATE (OUTAGE_DB_USED)

         IF(ALLOCATED(COMMITMENT_DB)) DEALLOCATE (COMMITMENT_DB)
         IF(ALLOCATED(HOURLY_OUTAGE_STATE)) &
                                DEALLOCATE(HOURLY_OUTAGE_STATE, &
                                           HOURLY_OUTAGE_TYPE, &
                                           HOURLY_LAST_STATE, &
                                           HOURLY_LAST_DERATE, &
                                           HOURLY_LAST_TYPE)
         IF(ALLOCATED(OUTAGE_ENERGY)) &
                                    DEALLOCATE(OUTAGE_ENERGY, &
                                               OUTAGE_WHOLESALE_ENERGY, &
                                               OUTAGE_WHOLESALE_REVENUE)
         IF(ALLOCATED(START_UP_UNIT_BY_TG)) &
                                         DEALLOCATE(START_UP_UNIT_BY_TG)

      RETURN

      ENTRY DEALLO_ANNUAL_MARG_COSTS_ARRAYS() &
               RESULT(R_DEALLO_ANN_MARG_COSTS_ARRAYS)

!         IF(ALLOCATED(DEPTH_PRICE)) DEALLOCATE(DEPTH_PRICE)
         IF(ALLOCATED(CUM_UNIT_UTIL)) DEALLOCATE(CUM_UNIT_UTIL)
         IF(ALLOCATED(CURRENT_UNIT_UTIL)) DEALLOCATE(CURRENT_UNIT_UTIL)
         IF(ALLOCATED(CUM_UNIT_UTIL_zero)) &
                                  DEALLOCATE(CUM_UNIT_UTIL_zero)
         IF(ALLOCATED(CURRENT_UNIT_UTIL_zero)) &
                                  DEALLOCATE(CURRENT_UNIT_UTIL_zero)
         IF(ALLOCATED(TOT_EMBED_COST_BY_POINT)) &
                                  DEALLOCATE(TOT_EMBED_COST_BY_POINT)
         IF(ALLOCATED(COST_CURVE_POINTER_BY)) &
                                  DEALLOCATE(COST_CURVE_POINTER_BY)
         IF(ALLOCATED(BlockIncrCost)) DEALLOCATE(BlockIncrCost)

         IF(ALLOCATED(OUTAGE_BLOCKS_4_DB)) &
                                  DEALLOCATE(OUTAGE_BLOCKS_4_DB)
         IF(ALLOCATED(OUTAGE_BLOCK_N_DB)) &
                                  DEALLOCATE(OUTAGE_BLOCK_N_DB)
         IF(ALLOCATED(OUTAGE_BLOCK_REVERSE_INDEX)) &
                                  DEALLOCATE(OUTAGE_BLOCK_REVERSE_INDEX)

         R_DEALLO_ANN_MARG_COSTS_ARRAYS = .TRUE.
      RETURN

      ENTRY SYSTEM_MARGINAL_COST(CURRENT_GUESS,SYS_TRANS_GROUP, &
                                 SYS_MARKET_PRICE) &
                        RESULT(R_SYSTEM_MARGINAL_COST)

!
         BLOCK_POSITION = &
                       NEW_BLOCK_NUMBER(CURRENT_GUESS,1,SYS_TRANS_GROUP)
         IF(BLOCK_POSITION > NBLOK2_SAVE .OR. BLOCK_POSITION < 1) THEN
            WRITE(4,*) "An invalid transaction occurred"
            WRITE(4,*) "in Transact Analyst. Block number ", &
                                                          BLOCK_POSITION
            WRITE(4,*) "for Transaction Group ",SYS_TRANS_GROUP
            WRITE(4,*) "was not within the range of defined groups."
         ENDIF
         U = UNIT_SAVE(BLOCK_POSITION)
         B = MAX(INT(1,2),BLKNO_SAVE(BLOCK_POSITION))

         CUM_PROB(SYS_TRANS_GROUP) = 0.
         CUM_COST(SYS_TRANS_GROUP) = 0.

         IF(UNIT_DISP_COST(U,B)/10. < SYS_MARKET_PRICE) THEN

            LAST_PROB_BLOCK = &
                     NEW_BLOCK_NUMBER(CURRENT_GUESS, &
                                      CONTRIB_INTERVALS,SYS_TRANS_GROUP)

            LAST_BLOCK = LAST_PROB_BLOCK

            BLOCK_POSITION = &
                           NEW_BLOCK_NUMBER(CURRENT_GUESS, &
                                             LAST_BLOCK,SYS_TRANS_GROUP)
            IF( LAST_BLOCK <= 0 .OR. BLOCK_POSITION > NBLOK2_SAVE) THEN
               WRITE(4,*) "MARGINAL COSTING INCORRECTLY INDEXING"
               WRITE(4,*) '*** line 5588 MARGNOBJ.FOR ***'
               er_message='See WARNING MESSAGES -MARGNOBJ.FOR-13'
               call end_program(er_message)
            ENDIF
            U = UNIT_SAVE(BLOCK_POSITION)
            B = MAX(INT(1,2),BLKNO_SAVE(BLOCK_POSITION))
            DO WHILE(UNIT_DISP_COST(U,B)/10. > SYS_MARKET_PRICE .AND. &
                                                         LAST_BLOCK > 0)
               LAST_BLOCK = LAST_BLOCK - 1
               BLOCK_POSITION = &
                           NEW_BLOCK_NUMBER(CURRENT_GUESS, &
                                             LAST_BLOCK,SYS_TRANS_GROUP)
               U = UNIT_SAVE(BLOCK_POSITION)
               B = MAX(INT(1,2),BLKNO_SAVE(BLOCK_POSITION))
            ENDDO
!
            DO WHILE(LAST_BLOCK > 0) ! THIS CAN BE REMOVED IF I SAVE WEIGHTED MARGINAL COSTS AT CALC TIME.
               CUM_PROB(SYS_TRANS_GROUP) = CUM_PROB(SYS_TRANS_GROUP) + &
                                   NEW_MARGINAL_DATA_BASE(CURRENT_GUESS, &
                                             LAST_BLOCK,SYS_TRANS_GROUP)
               BLOCK_POSITION = &
                           NEW_BLOCK_NUMBER(CURRENT_GUESS, &
                                             LAST_BLOCK,SYS_TRANS_GROUP)
               U = UNIT_SAVE(BLOCK_POSITION)
               B = MAX(INT(1,2),BLKNO_SAVE(BLOCK_POSITION))
!
               CUM_COST(SYS_TRANS_GROUP) = CUM_COST(SYS_TRANS_GROUP) + &
                                             UNIT_DISP_COST(U,B)/10. * &
                                   NEW_MARGINAL_DATA_BASE(CURRENT_GUESS, &
                                             LAST_BLOCK,SYS_TRANS_GROUP)
               LAST_BLOCK = LAST_BLOCK - 1
            ENDDO
         ENDIF

         IF(CUM_PROB(SYS_TRANS_GROUP) == 0.) THEN
            R_SYSTEM_MARGINAL_COST = UNIT_DISP_COST(U,B)/10. ! TRANSACTION GROUP IS ABOVE MARKET FOR ALL PROB STATES
         ELSE
            R_SYSTEM_MARGINAL_COST = CUM_COST(SYS_TRANS_GROUP) / &
                                               CUM_PROB(SYS_TRANS_GROUP) ! SAVE THESE TWO VALUES FOR LATER
         ENDIF

      RETURN
      END


!     PRODUCE CALENDAR CORRECT HOURLY MARGINAL COSTS
      SUBROUTINE CAL_HOURLY_MC_BEFORE_EL(START_MONTH,END_MONTH)
      use end_routine, only: end_program, er_message
      use grx_planning_routines

      use SpinDriftLib
      use prod_arrays_dimensions
      use globecom
      use forecast
      SAVE


      LOGICAL(kind=1) :: GET_THOSE_MARGINAL_COSTS, HAVE_MARGINAL_COSTS
      INTEGER(kind=2) :: MONTH,DAY,MC_YEAR,TEMPER=0,DELTMP=0
      INTEGER(kind=4) :: LDE_RECORD_COUNT=0
      INTEGER(kind=2) :: IHRL(24)=0,K
      INTEGER(kind=4) :: LOADS(24)=0,REC_LENGHT
      INTEGER(kind=2) :: DAYWEK,TIMZON,I,J,DAYS_PER_MONTH(12),DAY_COUNT, &
                MONTH_COUNT
      CHARACTER(len=8) :: EEICODE
      LOGICAL(kind=1) :: LEAP_YEAR,NEW_LDE_FORMAT=.FALSE.
      REAL :: REMAIN
      DATA DAYS_PER_MONTH/31,29,31,30,31,30,31,31,30,31,30,31/
!
!     MARGINAL COSTING VARIABLES

      INTEGER(kind=2) :: START_MONTH,END_MONTH,LOWER_MC
      REAL :: DX,MARGINAL_COST(0:1000),HOURLY_MARGINAL_COST(24)

      INTEGER(kind=2) :: HOURLY_LOAD_OUT,FILE_EXT,START_DAY,END_DAY=0,MO
      CHARACTER(len=5) :: BSYRLOAD
      CHARACTER(len=2) :: HOURLY_CHAR
      CHARACTER(len=64) :: OUTPUT_DIRECTORY,FILE_NAME

! END DATA DECLARATIONS

! THIS ROUTINE IS NOT CALLED UNLESS THE MARGINAL COST MODULE IS ACTIVE

      IF(TESTING_PLAN) RETURN

      HAVE_MARGINAL_COSTS =  GET_THOSE_MARGINAL_COSTS(DX,MARGINAL_COST)

      IF(START_MONTH == 1) THEN

         LDE_RECORD_COUNT = 1
         START_DAY = 0
         END_DAY = 0

         CALL SET_HOURLY_LOAD_OUT ! OPENS THE BINARY HOURLY FILE AS UNIT 29
         REWIND(29) ! 29 IS THE UNIT FOR THE REFERENCE LOAD FILE.

         FILE_EXT = INT(HOURLY_LOAD_OUT())
         IF(FILE_EXT == 0) THEN
            FILE_NAME = trim(OUTPUT_DIRECTORY())//"MCE"// &
                                              trim(BSYRLOAD())//".B00"
            WRITE(4,*) 'There is no unique hourly load file for year', &
                                                        globecom_YEAR
            WRITE(4,*) 'in the hourly marginal cost file.'
            WRITE(4,*) ' '
         ELSEIF(FILE_EXT < 10) THEN
            FILE_NAME = trim(OUTPUT_DIRECTORY())//"MCE"// &
                            trim(BSYRLOAD())//".B0"//CHAR(FILE_EXT+48)
         ELSE
            HOURLY_CHAR(1:1) = CHAR(48 + INT(FLOAT(FILE_EXT)/10.))
            HOURLY_CHAR(2:2) = CHAR(48 + MOD(FILE_EXT,INT(10,2)))
            FILE_NAME = trim(OUTPUT_DIRECTORY())//"MCE"// &
                     trim(BSYRLOAD())//".B"//HOURLY_CHAR
         ENDIF
         OPEN(8,FILE=FILE_NAME)

      ENDIF




      DAY_COUNT = 1
      MONTH_COUNT = START_MONTH

      INQUIRE(29,RECL=REC_LENGHT)
      NEW_LDE_FORMAT = REC_LENGHT > 80

! READ ONCE TO SEE IF ITS A LEAP YEAR

      IF(NEW_LDE_FORMAT) THEN
         READ(29,REC=LDE_RECORD_COUNT,ERR=200) &
                     MONTH,DAY,MC_YEAR,EEICODE,DAYWEK,TIMZON,TEMPER, &
                     DELTMP,LOADS
      ELSE
         READ(29,REC=LDE_RECORD_COUNT,ERR=200) &
                     MONTH,DAY,MC_YEAR,EEICODE,DAYWEK,TIMZON,TEMPER, &
                     DELTMP,IHRL
      ENDIF

! SET LEAP YEAR SWITCH

      IF(FLOAT(MC_YEAR) < 64. ) THEN
         REMAIN = MOD(FLOAT(MC_YEAR), 4.)
      ELSE
         REMAIN = MOD(FLOAT(MC_YEAR)-64.,4.)
      ENDIF
      IF( REMAIN < .001) THEN
         LEAP_YEAR = .TRUE.
      ELSE
         LEAP_YEAR = .FALSE.
      ENDIF


      DO MO = START_MONTH, END_MONTH
         START_DAY = END_DAY + 1
         END_DAY = DAYS_PER_MONTH(MO) + END_DAY
         DO J = START_DAY , END_DAY

!           READ THE BINARY DATA FILE

            IF(NEW_LDE_FORMAT) THEN
               READ(29,REC=LDE_RECORD_COUNT,ERR=200) &
                     MONTH,DAY,MC_YEAR,EEICODE,DAYWEK,TIMZON,TEMPER, &
                     DELTMP,LOADS
            ELSE
               READ(29,REC=LDE_RECORD_COUNT,ERR=200) &
                     MONTH,DAY,MC_YEAR,EEICODE,DAYWEK,TIMZON,TEMPER, &
                     DELTMP,IHRL
               DO K = 1, 24
                  LOADS(K) = IHRL(K)
               ENDDO
            ENDIF
!           CALCULATE HOURLY MARGINAL COST FOR EACH DAY OF THE YEAR
            DO K = 1, 24
               LOWER_MC = INT(LOADS(K)/DX)
               IF(LOWER_MC < 1 .OR. LOWER_MC > 1000) THEN
                  WRITE(4,*) 'HOURLY MC IS OUT OF RANGE'
               ENDIF
               HOURLY_MARGINAL_COST(K) = MARGINAL_COST(LOWER_MC)/10.
            ENDDO

!           CONVERT THE ABOVE TO MARGINAL COSTS IN THE
!                 FOLLOWING NERC LOAD DATA FORMAT

            IF( .NOT. (.NOT. (LEAP_YEAR) &
                         .AND. MONTH_COUNT==2 .AND. DAY_COUNT==29)) THEN
               WRITE(8,3000) MONTH,DAY,MC_YEAR,1,EEICODE,DAYWEK,TIMZON, &
                        (HOURLY_MARGINAL_COST(I),I=1,12)
               WRITE(8,3000) MONTH,DAY,MC_YEAR,2,EEICODE,DAYWEK,TIMZON, &
                        (HOURLY_MARGINAL_COST(I),I=13,24)
            ENDIF
            DAY_COUNT = DAY_COUNT + 1
            IF(DAY_COUNT > DAYS_PER_MONTH(MONTH_COUNT)) THEN
               MONTH_COUNT=MONTH_COUNT + 1
               DAY_COUNT = 1
            ENDIF
            LDE_RECORD_COUNT=LDE_RECORD_COUNT + 1
         ENDDO ! END OF DAY COUNTER START_DAY, END_DAY
      ENDDO ! END OF MONTH COUNTER


      IF(END_MONTH == 12) THEN
         CLOSE(8)
         CLOSE(29)
      ENDIF

      RETURN

  200 WRITE(4,*) 'ERROR READING THE HOURLY LOAD FILE'
      er_message='Stop requested from MARGNOBJ SIID198'
      call end_program(er_message)

 3000 FORMAT(3I2,I1,A8,2I1,3X,12F5.1)

      END


!     PRODUCE CALENDAR INCORRECT HOURLY MARGINAL COSTS
!     AFTER THEY HAVE BEEN OPERATED UPON BY ENRGLIMT.FOR



      FUNCTION CAL_HOURLY_MC_AFTER_EL(LPROB,LODDUR)
      use end_routine
      use SpinDriftLib
      use prod_arrays_dimensions
      use mod_base_year
      USE SIZECOM
      use globecom
      SAVE

!
      LOGICAL(kind=1) :: GET_THOSE_MARGINAL_COSTS,HAVE_MARGINAL_COSTS, &
                  CAL_HOURLY_MC_AFTER_EL, &
                  DEALLO_CAL_HOURLY_MC_AFTER_EL
      INTEGER(kind=2) :: I,ISEAS,MONTH,DAY,MC_YEAR,DAYWEK,TIMZON, &
                  DAYS_PER_MONTH(12),WRITE_YEAR,MAX_DATA_BASES,R_HOUR, &
                  R_TRANS_GROUP
      PARAMETER   (MAX_DATA_BASES=6)
      INTEGER(kind=4) :: DAILY_MW(24)
      REAL(kind=4) :: LPROB(*),LODDUR(*), &
                  GET_TRANS_GROUP_HOURLY_LOAD
      INTEGER(kind=2) :: CUM_HOURS_BY_MONTH(0:12),CUM_HOURS
      CHARACTER(len=2) :: HOURLY_CHAR
      CHARACTER(len=8) :: EEICODE
      REAL(kind=8) :: DEMAND_AFTER_EL,TOTAL_DEMAND_AFTER_EL, &
                      GET_DEMAND_AFTER_EL
      INTEGER(kind=4) :: I4_ENERGY,TOTAL_I4_ENERGY
      REAL(kind=4) :: R4_ENERGY,TOTAL_R4_ENERGY,LOAD_ADJUSTER
      DATA DAYS_PER_MONTH/31,28,31,30,31,30,31,31,30,31,30,31/
      DATA CUM_HOURS_BY_MONTH/0,744,1416,2160,2880,3624,4344,5088,5832, &
                                                    6552,7296,8016,8760/
!
!     MARGINAL COSTING VARIABLES
!

      INTEGER(kind=2) :: START_MONTH,END_MONTH,LOWER_MC, &
          LOAD_HOURS_IN_PERIOD,SORT_POS(:),EL_POS,HR,HOUR_IN_THE_SEASON

      INTEGER(kind=2) :: TEMPER=0,DELTMP=0, &
                DAY_OF_YEAR=0,LAST_EL_POS
      REAL :: MC_DX,MARGINAL_COST(0:1000),HOURLY_MARGINAL_COST(:), &
                CURRENT_PROB,CURRENT_MW(:,:)
!
      CHARACTER(len=5) :: BSYRLOAD
      CHARACTER(len=64) :: FILE_NAME,OUTPUT_DIRECTORY
      ALLOCATABLE :: HOURLY_MARGINAL_COST,SORT_POS,CURRENT_MW
!
!
      REAL(kind=4) :: AEL_PEAK(12,2),AEL_PEAKMAX, &
               AEL_BASE(12),AEL_BASEMIN, &
               AEL_ENERGY(12),AEL_ENRGYEAR
!

!
!
! END DATA DECLARATIONS



      DEMAND_AFTER_EL = GET_DEMAND_AFTER_EL(ISEAS,START_MONTH,END_MONTH)

      MC_YEAR = get_BASE_YEAR() + globecom_YEAR - 1900
      IF(MC_YEAR < 100) THEN
         WRITE_YEAR = MC_YEAR
      ELSE
         WRITE_YEAR = MC_YEAR - 100
      ENDIF


      IF(START_MONTH == 1) THEN

         AEL_PEAKMAX = 0.0
         AEL_BASEMIN = 999999.
         AEL_ENRGYEAR = 0.0

         TOTAL_DEMAND_AFTER_EL = 0.
         TOTAL_R4_ENERGY = 0.
         TOTAL_I4_ENERGY = 0
         DAY_OF_YEAR = 1
         IF(MC_YEAR < 100) THEN
            HOURLY_CHAR(1:1) = CHAR(48 + INT(FLOAT(MC_YEAR)/10.))
         ELSE
            HOURLY_CHAR(1:1) = CHAR(48 + INT(FLOAT(MC_YEAR-100)/10.))
         ENDIF
         HOURLY_CHAR(2:2) = CHAR(48 + MOD(MC_YEAR,INT(10,2)))

         FILE_NAME = trim(OUTPUT_DIRECTORY())//"MCA"// &
                                   trim(BSYRLOAD())//".B"//HOURLY_CHAR
         OPEN(1721,FILE=FILE_NAME)

!         IF(HOURLY_OUTPUT_NOT_OPEN) THEN

            FILE_NAME = trim(OUTPUT_DIRECTORY())//"MHA"// &
                                   trim(BSYRLOAD())//".B"//HOURLY_CHAR
            OPEN(1722,FILE=FILE_NAME,ACCESS="DIRECT", &
                           STATUS="UNKNOWN",FORM="UNFORMATTED",RECL=118)

      ENDIF

      HAVE_MARGINAL_COSTS = &
                           GET_THOSE_MARGINAL_COSTS(MC_DX,MARGINAL_COST)

      CALL GET_CHRONO_HOURS_PER_MONTH(ISEAS,LOAD_HOURS_IN_PERIOD)

      IF(ALLOCATED(SORT_POS)) DEALLOCATE(SORT_POS)
      IF(ALLOCATED(CURRENT_MW)) DEALLOCATE(CURRENT_MW)
      ALLOCATE(SORT_POS(LOAD_HOURS_IN_PERIOD), &
                      CURRENT_MW(LOAD_HOURS_IN_PERIOD,0:MAX_DATA_BASES))
      CURRENT_MW = 0.

      CALL GET_CHRONO_HOUR_DISTRIBUTION(ISEAS,SORT_POS)


      ALLOCATE(HOURLY_MARGINAL_COST(LOAD_HOURS_IN_PERIOD)) ! DEALLOCATED AT END OF ROUTINE



      EL_POS = 2
      LAST_EL_POS = 1
      TOTAL_R4_ENERGY = 0.

      DO HR = 1, LOAD_HOURS_IN_PERIOD

         CURRENT_PROB = 1. - FLOAT(HR-1)/FLOAT(LOAD_HOURS_IN_PERIOD)

         DO WHILE(CURRENT_PROB <= LPROB(EL_POS))
            EL_POS = EL_POS + 1
            LAST_EL_POS = EL_POS - 1
         ENDDO
!
         HOUR_IN_THE_SEASON = SORT_POS(HR)

         IF(LPROB(EL_POS) - LPROB(EL_POS+1) < .000001 .AND. &
                                               LPROB(EL_POS) > 0.) THEN
            EL_POS = EL_POS + 1
         ENDIF
!
         CURRENT_MW(HOUR_IN_THE_SEASON,1) = LODDUR(LAST_EL_POS) + &
                        (LODDUR(EL_POS) - LODDUR(LAST_EL_POS)) * &
                        (LPROB(LAST_EL_POS)-CURRENT_PROB)/ &
                        (LPROB(LAST_EL_POS)-LPROB(EL_POS))

         TOTAL_R4_ENERGY=TOTAL_R4_ENERGY + &
                                        CURRENT_MW(HOUR_IN_THE_SEASON,1)

         LOWER_MC = INT(CURRENT_MW(HOUR_IN_THE_SEASON,1)/MC_DX)

         IF(LOWER_MC < 1 .OR. LOWER_MC > 1000) THEN
!           WRITE(4,*) 'HOURLY MC IS OUT OF RANGE' ! FIX THE DX PROBLEM 2/22/96
            HOURLY_MARGINAL_COST(HOUR_IN_THE_SEASON) = &
                                             MARGINAL_COST(1000)/10.

         ELSE
            HOURLY_MARGINAL_COST(HOUR_IN_THE_SEASON) = &
                                             MARGINAL_COST(LOWER_MC)/10.
         ENDIF

      ENDDO

      DAYWEK = 1

      DAY = 1
      EEICODE = '12345678'
      TIMZON = 1

      LOAD_ADJUSTER = DEMAND_AFTER_EL/TOTAL_R4_ENERGY
      DO MONTH = START_MONTH, END_MONTH

         AEL_PEAK(MONTH,1) = 0.0
         AEL_PEAK(MONTH,2) = 0.0
         AEL_BASE(MONTH) = 999999.
         AEL_ENERGY(MONTH) = 0.0

         CUM_HOURS = CUM_HOURS_BY_MONTH(MONTH-1) - &
                                       CUM_HOURS_BY_MONTH(START_MONTH-1)

         I4_ENERGY = 0.
         R4_ENERGY = 0.
         DO DAY = 1, DAYS_PER_MONTH(MONTH)

            HR = (DAY-1)*24 + 1 + CUM_HOURS

            WRITE(1721,3000) &
                           MONTH,DAY,WRITE_YEAR,1,EEICODE,DAYWEK,TIMZON, &
                                    (HOURLY_MARGINAL_COST(I),I=HR,HR+11)
            WRITE(1721,3000) &
                           MONTH,DAY,WRITE_YEAR,2,EEICODE,DAYWEK,TIMZON, &
                                 (HOURLY_MARGINAL_COST(I),I=HR+12,HR+23)

            DO I = 1, 24
               CURRENT_MW(HR+I-1,1) = CURRENT_MW(HR+I-1,1)*LOAD_ADJUSTER
               CURRENT_MW(HR+I-1,0) = CURRENT_MW(HR+I-1,0) + &
                                                    CURRENT_MW(HR+I-1,0)
               DAILY_MW(I) = NINT(CURRENT_MW(HR+I-1,1))
               I4_ENERGY = I4_ENERGY + DAILY_MW(I)
               R4_ENERGY = R4_ENERGY + CURRENT_MW(HR+I-1,1)
!
! ENSURE THAT WE HAVE THE MONTHLY PEAKS FOR TRANSFORMATIONS
!
           AEL_PEAK(MONTH,1) = MAX(AEL_PEAK(MONTH,1),REAL(DAILY_MW(I)))
!
               IF(DAYWEK < 6) THEN
           AEL_PEAK(MONTH,2) = MAX(AEL_PEAK(MONTH,2),REAL(DAILY_MW(I)))
               ENDIF
               AEL_BASE(MONTH) = MIN(AEL_BASE(MONTH),REAL(DAILY_MW(I)))
               AEL_ENERGY(MONTH) =AEL_ENERGY(MONTH) + DAILY_MW(I)
            ENDDO

            WRITE(1722) MONTH,DAY,WRITE_YEAR,EEICODE, &
                                    DAYWEK,TIMZON,TEMPER,DELTMP,DAILY_MW
            DAY_OF_YEAR = DAY_OF_YEAR + 1
            IF(DAY_OF_YEAR == 60) THEN

               WRITE(1722) &
                  MONTH,DAY,WRITE_YEAR,EEICODE, &
                                    DAYWEK,TIMZON,TEMPER,DELTMP,DAILY_MW
               DAY_OF_YEAR = DAY_OF_YEAR + 1
            ENDIF

            IF(DAYWEK < 7) THEN
               DAYWEK = DAYWEK + 1
            ELSE
               DAYWEK = 1
            ENDIF

         ENDDO ! DAY
         TOTAL_R4_ENERGY = TOTAL_R4_ENERGY + R4_ENERGY
         TOTAL_I4_ENERGY = TOTAL_I4_ENERGY + I4_ENERGY


         AEL_PEAKMAX = MAX(AEL_PEAKMAX,AEL_PEAK(MONTH,1), &
                                                      AEL_PEAK(MONTH,2))
         AEL_BASEMIN = MIN(AEL_BASEMIN,AEL_BASE(MONTH))
         AEL_ENRGYEAR = AEL_ENRGYEAR + AEL_ENERGY(MONTH)

      ENDDO ! MONTH

      TOTAL_DEMAND_AFTER_EL = TOTAL_DEMAND_AFTER_EL + DEMAND_AFTER_EL
      IF(END_MONTH == 12) THEN
         MONTH= get_BASE_YEAR() + globecom_YEAR
         WRITE(9,*) MONTH,'  AFTER_EL=',TOTAL_DEMAND_AFTER_EL, &
                          '  I4_ENERGY=',TOTAL_I4_ENERGY, &
                          '  R4_ENERGY=',INT(TOTAL_R4_ENERGY), &
                 '  DIFF=',INT(TOTAL_DEMAND_AFTER_EL) - TOTAL_I4_ENERGY
      ENDIF


      IF(END_MONTH == 12) THEN
         WRITE(1722,REC=367) (AEL_PEAK(MONTH,1),MONTH=1,12),AEL_PEAKMAX
         WRITE(1722,REC=368) AEL_BASE,AEL_BASEMIN
         WRITE(1722,REC=369) AEL_ENERGY,AEL_ENRGYEAR

      ENDIF

      DEALLOCATE(HOURLY_MARGINAL_COST,SORT_POS)

      CAL_HOURLY_MC_AFTER_EL = .TRUE.

      RETURN


      ENTRY GET_TRANS_GROUP_HOURLY_LOAD(R_HOUR,R_TRANS_GROUP)

         GET_TRANS_GROUP_HOURLY_LOAD = CURRENT_MW(R_HOUR,R_TRANS_GROUP)
      RETURN

      ENTRY DEALLO_CAL_HOURLY_MC_AFTER_EL()


         DEALLO_CAL_HOURLY_MC_AFTER_EL = .TRUE.
         IF(ALLOCATED(SORT_POS)) DEALLOCATE(SORT_POS)
         IF(ALLOCATED(CURRENT_MW)) DEALLOCATE(CURRENT_MW)
      RETURN


 3000 FORMAT(3I2,I1,A8,2I1,3X,12F5.1)

      END



      REAL(kind=4) function DGauss(z)
      REAL(kind=4) :: z,t, &
        MaxReal, &
        SqrtMR, &
        MaxLn, &
        LimLn
      parameter( &
        MaxReal= 9.0E+37, &
        SqrtMR = 1.0e-19, &
        MaxLn  = 87.39  , & !  ln(MaxReal)
        LimLn  = 43.2)

      t=-0.5*z*z
      if(t<-LimLn) then
        DGauss=SqrtMR  ! preclude run-time errors
      else
        DGauss=0.3989423*exp(t) ! normal density function
      end if
      return
      end



      subroutine FillTableCCN
      use mcbtn
      use end_routine
      SAVE


      INTEGER(kind=2) :: iz
      REAL(kind=4) :: y,z,Density,DGauss,CCNFMD

      do iz=-LimCentiSD,LimCentiSD
        z=0.01*float(iz) ! step-size is one hundredth of a standard-deviation
        Density=DGauss(z)
        if(z>=0.0) then
          y=              CCNFMD( z)
        else
          y=(1.0/Density)-CCNFMD(-z)
        end if
        CCNormOrdinate(iz)=y*Density
      end do
      return
      end ! subroutine FillTableCCN


      REAL(kind=4) function CCNormOrd(xCCNOmMean)
      use mcbtn

      SAVE


      REAL(kind=4) :: xCCNOmMean
      INTEGER(kind=2) :: iz

      iz=ifix(xCCNOmMean/HundSD)
      if    (iz<-LimCentiSD) then
        CCNormOrd=1.0
      elseif(iz> LimCentiSD) then
        CCNormOrd=0.0
      else
        CCNormOrd=CCNormOrdinate(iz)
      end if
      return
      end


      subroutine SortIncr(a,b,nItems)
      ! Sorts a & b by incr order of a; adapted from the source on page 110 of
      ! SOFTWARE TOOLS IN PASCAL, by Kernighan & Plauger, Addison-Wesley 1981
      SAVE
      REAL(kind=4) :: a(*),b(*),Hold
      INTEGER(kind=2) :: nItems,i,j,k,Gap

      Gap=nItems/2
      do while(Gap>0)
        do i=Gap,nItems-1 ! for 0-based array
          j=i-Gap
          do while(j>=0)  ! for 0-based array
            k=j+Gap
            if(a(j)<=a(k)) then
              j=-1 ! break the do-while loop
            else ! interchange values in both arrays
              Hold=a(j)
              a(j)=a(k)
              a(k)=Hold
              Hold=b(j)
              b(j)=b(k)
              b(k)=Hold
            end if
            j=j-Gap
          end do
        end do
        Gap=Gap/2
      end do
      return
      end ! subroutine SortIncr


      subroutine MergeDups(a,b,nUnique)
      ! accumulate values of b over indices for which values of a are identical
      use SpinDriftLib
      use prod_arrays_dimensions
      use mcbtn
      use end_routine
      SAVE


      REAL(kind=4) :: a(*),b(*),bAccum
      INTEGER(kind=2) :: nUnique,i,j,k
      INTEGER(kind=4) :: nB
      REAL(kind=4) :: a_temp_array(:),b_temp_array(:)
      allocatable :: a_temp_array,b_temp_array

      allocate(a_temp_array(0:nUnique),b_temp_array(0:nUnique))
      i=nUnique-1
      do while(i>0) ! loop downward to minimize total bytes moved
        if(a(i)==a(i-1)) then
          bAccum=b(i)
          j=i-1
          do while((j>=0) .and. (a(i)==a(j)))
            bAccum=bAccum+b(j)
            if(PrtDetail>0)WRITE(4,'(1x,i4,2i5,f9.4)')nUnique,i,j,bAccum
            j=j-1
          end do
          j=j+1 ! now the (lowest) index of last element matching a(i)
          nB=4*(nUnique-i) ! for 4-byte reals
          nB=nUnique-i-1
          do k = 0, nB
             a_temp_array(k) = a(i+k)
             b_temp_array(k) = b(i+k)
          enddo
          do k = 0, nB
             a(j+k) = a_temp_array(k)
             b(j+k) = b_temp_array(k)
          enddo
!          call cmove(a(i),a(j),nB)
!          call cmove(b(i),b(j),nB)
          b(j)=bAccum
          nUnique=nUnique-i+j
          if(PrtDetail>0) WRITE(4,'(1x,i4,2i4,f9.4,i3,a)') &
            nUnique,i,j,bAccum,i-j,' deleted'
          i=j
        end if
        i=i-1
      end do
      deallocate(a_temp_array,b_temp_array)
      return
      end ! subroutine MergeDups


      subroutine InitNodeValues(kINV)
      use mcbtn
      use end_routine
      SAVE


      INTEGER(kind=2) :: kINV

      ! assign values for a single pair of branches
      VectSizeElem=2
      VectSizeByte=8 ! for 4-byte reals
      NodeCOut(0)=0.0
      NodeCOut(1)=BlockC(kINV)
      NodeProb(0)=BlockP(kINV)
      NodeProb(1)=BlockQ(kINV)
      return
      end


      subroutine FillNodeValues(kFNV)
      use SpinDriftLib
      use prod_arrays_dimensions
      use mcbtn
      use end_routine
      SAVE


      INTEGER(kind=2) :: kFNV,i,j
      LOGICAL(kind=1) :: DupsLikely
      REAL(kind=4) :: BranchCOut,BranchProb

      ! compute 2^kFNV end-points for binary tree
      VectSizeByte=VectSizeElem*4 ! needed if binary tree has been compressed
      PrevProb = NodeProb

      PrevCOut = NodeCOut
!      call cmove(NodeCOut,PrevCOut,VectSizeByte)
    ! j=2**kFNV ! if 2^kFNV end-points are needed
      j=VectSizeElem*2 ! if binary tree has been compressed

      BranchCOut=BlockC(kFNV)
      BranchProb=BlockQ(kFNV)

      do i = 0, VectSizeElem-1 ! EM32 replacement for cmove
         NodeProb(VectSizeElem+i) = PrevProb(i)
      enddo

      do i = 0, VectSizeElem-1 ! EM32 replacement for cmove
         NodeCOut(VectSizeElem+i) = PrevCOut(i)
      enddo

      do while(j>VectSizeElem) ! loop downward for speed advantage
        j=j-1
        NodeProb(j)=NodeProb(j)*BranchProb
        NodeCOut(j)=NodeCOut(j)+BranchCOut
      end do

      BranchProb=BlockP(kFNV)
      NodeProb = PrevProb ! p-multiplied nodes
!      call cmove(PrevProb,NodeProb,VectSizeByte) ! p-multiplied nodes
      NodeCOut = PrevCOut
!      call cmove(PrevCOut,NodeCOut,VectSizeByte)
      do while(j>0)
        j=j-1
        NodeProb(j)=NodeProb(j)*BranchProb
      end do

      DupsLikely=.false. ! true if any later kFNV in the tree has same capacity
      j=2
      do while((j<=kFNV) .and. (.not. DupsLikely)) ! scan for pairs of equal cap
        do i=1,j-1
          DupsLikely=DupsLikely.or.(BlockC(i)==BlockC(j))
        end do
        j=j+1
      end do
      VectSizeElem=VectSizeElem*2
      VectSizeByte=VectSizeByte*2
      call SortIncr(NodeCOut,NodeProb,VectSizeElem)
      if(DupsLikely) call MergeDups(NodeCOut,NodeProb,VectSizeElem) ! else don't waste the effort
      return
      end ! subroutine FillNodeValues


      REAL(kind=4) function NodeProbWeightedLDC(xNPWD,k,jPt)
      use mcbtn
      use end_routine
      SAVE
      REAL(kind=4) :: xNPWD
      INTEGER(kind=2) :: k,jPt ! passed only for use if PrtDetail>0


! use NodeProb(x) to weight LDC, where by construction
! NodeProb(x) is the probability of capacity outage equal to x, i.e.,
! NodeProb(x)=Prob(capacity outage=x), whence e.g.,
! NodeProb(x) after block#1 of capacity=1 with q1=0.1 and
!                   block#2 of capacity=2 with q2=0.3:
! NodeProb(0)=Prob(outage=0)=p1*p2=0.9*0.7=0.63
! NodeProb(1)=Prob(outage=1)=q1*p2=0.1*0.7=0.07
! NodeProb(2)=Prob(outage=2)=p1*q2=0.9*0.3=0.27
! NodeProb(3)=Prob(outage=3)=q1*q2=0.1*0.3=0.03
!    ( note that this is neither a cumulative curve, nor monotonic )
!    1.0
!    0.9
!    0.8
!    0.7        0.63
!    0.6         ^
!    0.5         |
!    0.4         |                      0.27
!    0.3         |                       ^
!    0.2         |          0.07         |          0.03
!    0.1         |           ^           |           ^
!    0.0 --------+-----------+-----------+-----------+-----------+-----------+
!     x:         0           1           2           3           4           5

!    Thus with probability NodeProb(0) the loaded block sees LDC(x),
!    with probability NodeProb(1) the loaded block sees LDC(x-1),
!    with probability NodeProb(2) the loaded block sees LDC(x-2), etc.
!    Since in general the prior-blocks' capacities are not on a grid, the above
!    cannot be combined into a probabilistic-weighted array (i.e., an ELDC).
!    However, when LDC(x) has a functional form (as LDC=1 if x<=Lc else 0), we
!    can evaluate the expected ELDC ordinate at x as the sum
!    across i of NodeProb(i)*LDC(x-NodeCOut(i)).
!    For this binary-valued LDC, the terms of the summation are zero for all
!    x>NodeCOut(i), i.e., for all i having NodeCOut(i)<x.
      INTEGER(kind=2) :: i
      REAL(kind=4) :: Accum,CCNormOrd

      Accum=0.0
      i=VectSizeElem-1

      if(UsingMoments) then ! model LDC as comp. cumulative Normal distr.
        if(PrtDetail>0) WRITE(4,'(1x,i4,i5,f10.3,3f9.3,a)') &
          k,jPt,MeanM3s,MeanP3s,NDMean,StdDev,' NormParam'
        do while((i>=0) .and. (NodeCOut(i)+MeanM3s>=xNPWD)) ! LDC has unity height
          nAugmLDC=nAugmLDC+1
          Accum=Accum+NodeProb(i)
          if(PrtDetail>1) WRITE(4,'(1x,i4,2i5,2f8.2,2f9.6,a)') &
            k,jPt,i,xNPWD,MeanM3s,NodeCOut(i),NodeProb(i),Accum, &
            ' NCO+Mm3s>=xNPWD 1'
          i=i-1
        end do
        do while((i>=0) .and. (NodeCOut(i)+MeanP3s>=xNPWD))
          nEvalLDC=nEvalLDC+1
          Accum=Accum+CCNormOrd(xNPWD-NodeCOut(i)-NDMean)*NodeProb(i)
          if(PrtDetail>1) WRITE(4,'(1x,i4,2i5,2f8.2,2f9.6,a)') &
            k,jPt,i,xNPWD,MeanM3s,NodeCOut(i),NodeProb(i),Accum, &
            ' NCO+Mp3s>=xNPWD 2'
          i=i-1
        end do ! with either (i<0) or (NodeCOut(i)+MeanP3s<xNPWD)

      else ! LDC is binary valued, either 0 or 1
        if(k==1) then ! the first block loaded sees LDC with probability 1
          if(xNPWD<=0) Accum=1.0 ! else Accum retains its initial value of 0
        else
          do while((i>=0) .and. (NodeCOut(i)>=xNPWD)) ! LDC has unity height
            nAugmLDC=nAugmLDC+1
            Accum=Accum+NodeProb(i)
            if(PrtDetail>1) WRITE(4,'(1x,i4,2i5,2f8.2,2f9.6,a)') &
              k,jPt,i,xNPWD,NodeCOut(i),NodeProb(i),Accum,' NCO>=xNPWD'
            i=i-1
          end do ! with either (i<0) or (NodeCOut(i)<xNPWD)
        end if
      end if

      NodeProbWeightedLDC=Accum
      return
      end function NodeProbWeightedLDC
      
      subroutine check_hours_in_month(hours, key)
        use end_routine
        use string
        implicit none
        integer (kind=2), intent(in) :: hours
        character (len=*), intent(in) :: key
        
        if(hours>744) then
            call end_program("margnobj:0003 - hours value of " // &
            trim(itos(int(hours))) // " exceeds reality.")
        end if
      end subroutine check_hours_in_month
      
      

