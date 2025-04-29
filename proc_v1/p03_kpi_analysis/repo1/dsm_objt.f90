!     *************************************************************
!     DSM_OBJT.FOR
!     Created: 10/23/02 11:28:46 AM
!     Author : msg
!     Last change: MSG 1/10/2010 3:00:56 PM
!     *************************************************************

!      ***********************************************************
      RECURSIVE SUBROUTINE MDSLDCAP
!      ***********************************************************
!
!  READS INTO CORE THE CONTENTS OF THE LOAD MANAGEMENT DEVICES
!  INPUT FILE.
      use dsmfrcom
      use lamcom
      use endpoint
      use dsf
      use dsfc
      USE IREC_ENDPOINT_CONTROL
      use SpinDriftLib
      use prod_arrays_dimensions
      use kepcocom
      use sizecom
      use lamcom
      use dsmfrcom
      use globecom
      use dsm
      use retslcom
      use dsf2
      use forecast
      use kootenay
      use enrg_helper
      use mod_base_year
      use rangechecker

      implicit none
      save
      INCLUDE 'DSMCOM.MON'

      



!
!  DSM FINANCIAL VARIABLES 11/11/93
!
!
      REAL*4 &
           GREG_PEAK_RESERVE_ALLOCATION(3,SYSTEM_CLASS_NUM,12), &
           GREG_ENERGY_RESERVE_ALLOCATION(3,SYSTEM_CLASS_NUM,12)
      REAL ZERO/0./
      LOGICAL*1 LAHEY_LF95
      INTEGER*2 DSM_OPTIONS_ADDED
      INTEGER*2 LF_DELETE(:)
      REAL BASE_PROG_EXP_SAVE(:), &
           BASE_CUST_EXP_SAVE(:), &
           ONGO_PROG_EXP_SAVE(:), &
           ONGO_CUST_EXP_SAVE(:), &
           ONGO_NEW_CUST_EXP_SAVE(:), &
           ONGO_KWH_EXP_SAVE(:), &
           ONGO_KW_EXP_SAVE(:), &
           BASE_PROG_CAP_SAVE(:), &
           ONGO_PROG_CAP_SAVE(:), &
           BASE_CUST_CAP_SAVE(:), &
           ONGO_CUST_CAP_SAVE(:), &
           ONGO_NEW_CUST_CAP_SAVE(:), &
           ONGO_KWH_CAP_SAVE(:), &
           ONGO_KW_CAP_SAVE(:), &
           REBATE_CUST_EXP_SAVE(:), &
           REBATE_NEW_CUST_EXP_SAVE(:), &
           REBATE_CUST_CAP_SAVE(:), &
           REBATE_NEW_CUST_CAP_SAVE(:), &
           PARTICIPANT_CUST_COST_SAVE(:), &
           PARTICIPANT_NEW_CUST_COST_SAVE(:), &
           PARTICIPANT_COSTS_BY(:), &
           UTIL_NON_ELEC_CUST_COST_SAVE(:), &
           UTIL_NON_ELEC_NEW_COST_SAVE(:), &
           THIRD_PARTY_CUST_COST_SAVE(:), &
           THIRD_PARTY_NEW_CUST_COST_SAVE(:), &
           OTH_PARTICIPANT_CUST_COST_SAVE(:), &
           OTH_PARTICIPANT_NEW_COST_SAVE(:)
!
      ALLOCATABLE :: BASE_PROG_EXP_SAVE, &
                     ONGO_PROG_EXP_SAVE, &
                     BASE_CUST_EXP_SAVE, &
                     ONGO_CUST_EXP_SAVE, &
                     ONGO_NEW_CUST_EXP_SAVE, &
                     ONGO_KWH_EXP_SAVE, &
                     ONGO_KW_EXP_SAVE, &
                     BASE_PROG_CAP_SAVE, &
                     ONGO_PROG_CAP_SAVE, &
                     BASE_CUST_CAP_SAVE, &
                     ONGO_CUST_CAP_SAVE, &
                     ONGO_NEW_CUST_CAP_SAVE, &
                     ONGO_KWH_CAP_SAVE, &
                     ONGO_KW_CAP_SAVE, &
                     REBATE_CUST_EXP_SAVE, &
                     REBATE_NEW_CUST_EXP_SAVE, &
                     REBATE_CUST_CAP_SAVE, &
                     REBATE_NEW_CUST_CAP_SAVE, &
                     PARTICIPANT_CUST_COST_SAVE, &
                     PARTICIPANT_NEW_CUST_COST_SAVE, &
                     PARTICIPANT_COSTS_BY, &
                     UTIL_NON_ELEC_CUST_COST_SAVE, &
                     UTIL_NON_ELEC_NEW_COST_SAVE, &
                     THIRD_PARTY_CUST_COST_SAVE, &
                     THIRD_PARTY_NEW_CUST_COST_SAVE, &
                     OTH_PARTICIPANT_CUST_COST_SAVE, &
                     OTH_PARTICIPANT_NEW_COST_SAVE, &
                     LF_DELETE
!
!
      INTEGER*2 HOUR,IREC,PEAK_MONTH,MONTH,YR,R_YEAR,R_MONTH
      INTEGER IOS
      INTEGER*2 CURRENT_MONTH,LAST_LOCATION/0/,LOCATION,CURRENT_YEAR
      INTEGER*2 R_DEVICE_NUM,R_ACCEPTANCE_NUM,R_DSM_START_YEAR
      INTEGER*2 ADDED_LM_PROGRAMS(*)
      INTEGER*2 HR,MO
      LOGICAL*1 ACCEPTANCE_ACTIVE,RESPONSE_ACTIVE,LMF_FILE_EXISTS
      INTEGER*1 L,LAST_CLASS_IN_FORECAST/2/
!
!  USED IN FINANCIAL SECTION
!
      LOGICAL*1   BAD_ACCEPTANCE_POINTER,DSM_FINANCIAL_REPORT_SWITCH, &
                  LM_COST_REPORT_NOT_OPEN/.TRUE./, &
                  DSM_RATE_REPORT_NOT_OPEN/.TRUE./
      INTEGER*2   LM_COST_REPORT_HEADER,LM_COST_NO,DSM_YEAR
      INTEGER LM_COST_REC
      INTEGER*2 ACCEPTANCE_POINTER
      REAL MILLION,THOUSAND,LOCAL_FREE_RIDERS,LOCAL_FREE_DRIVERS
      INTEGER*2 ACCTNO
      PARAMETER (MILLION=10.**6,THOUSAND=10.**3)
!
!  USED IN DSM CLASS RATES SECTION
!
      INTEGER*2 R_CLASS,DSM_RATE_REPORT_HEADER,DSM_RATE_NO
      INTEGER*4 DSM_RATE_REC
      REAL RETIRING_CUSTOMERS
      REAL CLASS_RATES,CLASS_COSTS_TOTAL,CLASS_COSTS_DEMAND, &
                  CLASS_COSTS_ENERGY,CLASS_COSTS_CUSTOMER, &
                  CLASS_PEAK,CLASS_CUSTOMERS, &
                  PARTICIPANT_PENETRATION,PARTICIPANT_REVENUES, &
                  NON_PARTICIPANT_REVENUES,NON_PARTICIPANTS, &
                  NON_PARTICIPANT_CUSTOMER_USAGE,DSM_CUSTOMER_USAGE, &
                  PARTICIPANT_CUSTOMER_USAGE, &
                  ADJUSTED_DEVICE_ENERGY
      REAL*8 CLASS_MWH,PARTICIPANT_MWH
      INTEGER*2 VOID_INT2,ESCALATE_ALL_VALUES
!
!  FROM MDSLDCAP
!
      LOGICAL*1  &
                POOLING_TRANSACTIONS, &
                LDMGT_ACTIVE, &
                VOID_LOGICAL, &
                NOT_SYSTEM_BASED_FORECAST/.FALSE./
      LOGICAL*4 R_REAL_ADDITION
      INTEGER*2   REFERENCE_DAY_COUNT(2,12),REFERENCE_PEAK_HOUR(2,12), &
                  PEAK_HOUR,PEAK_MW_DAY,PEAK_MW_HOUR, &
                  APP_YEAR
      LOGICAL*1 CLASS_EXISTS_loc(MAX_LOAD_CLASSES), &
                CLASS_IN_AREA_1(MAX_LOAD_CLASSES)
      INTEGER*2 CLASS,FIRST_CLASS,DAY
      CHARACTER*9 LOADS_MONTH_NAME
      CHARACTER*5  LD_FIN_FIL,TEMP_FIL
!
!
      INTEGER*2 I,THIS_YEAR_IS,R_YR
      REAL CAPACITY_PEAKS, &
           PLANNING_PEAK, &
           PEAK_BEFORE_DSM, &
           R_ENERGY
      REAL GET_VAR,DELTA_CLASS_MW_FROM_DSM(SYSTEM_CLASS_NUM), &
           MONTHLY_DSM_ENERGY_BY_CLASS(12,SYSTEM_CLASS_NUM)
      LOGICAL*1 POOLING_YES,WRITE_TO_DSM_DATA_BASE
!      REAL HIGHEST_BEFORE_DSM_PEAK
!
!  INCLUDE SYSTEM LEVEL COMMON BLOCK
!
      REAL SYSTEM_FORECAST_PEAK
!  8/17/93 NOTE THE DSM WANTS TO ADJUSTMENT MONTH AND YEAR. NEEDED TO
!
!  INCLUDE CLASS LEVEL COMMON BLOCK
!  INCLUDE LOAD MANAGEMENT COMMON BLOCK
!
      REAL AFTER_LM_FACTOR(:),BEFORE_LM_FACTOR(:)
      ALLOCATABLE :: AFTER_LM_FACTOR,BEFORE_LM_FACTOR
!
!  REQUIRED BUFFERS
!
      REAL CUSCAL,SYSDIF(3),DSM_CAP(3,AVAIL_DATA_YEARS,2), &
           STODIF,TEMP_FACTOR, &
           LOAD_CREDIT,DIFSYS, &
           R_DSM_CAP(3,*), &
           R_DSM_CAPACITY, &
           CUSCAL_AFTER,CUSCAL_BEFORE, &
           CLASS_PEAK_AFTER_DSM(SYSTEM_CLASS_NUM-1), &
           R_DSM_PEAK,R_DSM_ENER
      INTEGER*2 APPLICATION,DEVICE_NUM,APP_ID_NUM
!
!  END MDSLDCAP VARIABLES
!
      INTEGER*2 TOTAL_DAYS
      REAL DEVICE_AVE_ENERGY,DEVICE_AVE_SALES
!
!  INCLUDE COMMON BLOCK OF ALL FORECAST TYPICAL CURVES FOR THIS YEAR
!
      REAL FORECAST_LOADS(24,3,12,*)
!
!  REQUIRED BUFFERS
!
      REAL LODDIF(24,3,3),LDMAX
!
!  INCLUDE LOAD MANAGEMENT COMMON BLOCK
!
      INTEGER*2 RESPONSE_NUM,MAX_RESPONSE_NUM,DSM_RESPONSE_CURVES
      INTEGER*2 ACTIVE_DSM_RESPONSE_CURVES
      REAL BASDEV(:,:), &
           LMGDEV(:,:), &
           CUSTOMERS_BY_DAY_TYPE(:,:), &
           BEFORE_LM_ADJUSTMENT(:), &
           AFTER_LM_ADJUSTMENT(:)
      INTEGER*2 LM_LOAD_UNITS(:), &
                APPLICATION_ID(:), &
                APPLICATION_CLASS(:), &
                DEVICE_APPLICATION(:), &
                FIRST_MONTH, &
                LAST_MONTH
      INTEGER*2 RESPONSE_DATA_LOCATION(:,:), &
                RESPONSE_LOCATION_POINTER(:)
      ALLOCATABLE :: RESPONSE_DATA_LOCATION, &
                RESPONSE_LOCATION_POINTER
      CHARACTER*1 OPERATING_METHOD(:), &
                  DSM_ENERGY_CLASSIFICATION(:)
      REAL DSM_CAP_PLAN_FACTOR(:)
      ALLOCATABLE :: BASDEV,LMGDEV,CUSTOMERS_BY_DAY_TYPE, &
                     BEFORE_LM_ADJUSTMENT,AFTER_LM_ADJUSTMENT, &
                     LM_LOAD_UNITS,APPLICATION_ID,APPLICATION_CLASS, &
                     OPERATING_METHOD,DSM_ENERGY_CLASSIFICATION, &
                     DSM_CAP_PLAN_FACTOR, &
                     DEVICE_APPLICATION
      INTEGER*2 DSM_DEVICE_RECORDS,MAX_DSM_DEVICE_NUM, &
                DSM_FINANCIAL_RECORDS/0/

!  ACCEPTANCE DATA

      CHARACTER*32 DSM_DEVICE_NAME(:)
      CHARACTER*1 PROGRAM_DSM_ALLOCATION_METHOD(:)
      INTEGER*2 RESPONSE_POINTER_FOR(:,:), &
                DSM_START_YEAR(:),ALLOC
      REAL:: FREE_RIDERS(:), &
           FREE_DRIVERS(:), &
           PROGRAM_LOSS_FACTOR(:), &

           R_DSM_PEAK_RESERVE_ALLOCATION(3,MAX_LOAD_CLASSES), &
           R_DSM_ENER_RESERVE_ALLOCATION(3,MAX_LOAD_CLASSES), &
           R_DSM_ENER_ALLOCATION(MAX_LOAD_CLASSES)

      LOGICAL*1 REMEMBER_MONTHLY_DSM_DATA
      REAL MONTHLY_DSM_DATA_BASE(:,:,:)
      ALLOCATABLE :: FREE_RIDERS, &
                     FREE_DRIVERS, &
                     PROGRAM_LOSS_FACTOR, &
                     DSM_START_YEAR, &
                     DSM_DEVICE_NAME, &
                     RESPONSE_POINTER_FOR, &
                     PROGRAM_DSM_ALLOCATION_METHOD, &
                     MONTHLY_DSM_DATA_BASE
!
      INTEGER*2 CLASS_NUM,APP_NUM,APP_DELETE,RES_DELETE
      REAL  CUSTOMERS(30),NEW_CUSTOMERS(30),BASE_CUST, &
            R_RESERVES_FROM_DEVICE_PEAK
!
      CHARACTER*32 DSM_CLASS_NAME,DEVICE_NAME

!
!      DECLARES FOR DSM_FINANCIAL_COST
!
      LOGICAL*1 FIRST_DEVICE
      LOGICAL*1 DSM_FINANCIAL_REPORT,R_ACTIVE_DSM_PROG, &
                  FOUND_AN_ACTIVE_PROGRAM
      INTEGER*2 DSM_ACCTS,TOP_OF_REPORT, &
                REPORT_COUNTER
      REAL TOTAL_REBATE_EXPENSE, &
           PROGRAM_EXPENSES, &
           CUSTOMER_EXPENSES,NEW_CUSTOMER_EXPENSES,KWH_EXPENSES, &
           KW_EXPENSES,TOTAL_DSM_EXPENSES,BASE_YEAR_EXPENSES, &
           PROGRAM_CAPITAL, &
           CUSTOMER_CAPITAL,NEW_CUSTOMER_CAPITAL,KWH_CAPITAL, &
           KW_CAPITAL,TOTAL_DSM_CAPITAL,BASE_YEAR_CAPITAL, &
           REBATE_CUST_CAPITAL,REBATE_NEW_CUST_CAPITAL, &
           REBATE_CUST_EXPENSE,REBATE_NEW_CUST_EXPENSE, &
           BASE_YEAR_PROG_EXPENSES,BASE_YEAR_CUST_EXPENSES, &
           BASE_YEAR_PROG_CAPITAL,BASE_YEAR_CUST_CAPITAL, &
           PARTICIPANT_CUSTOMER_COSTS,PARTICIPANT_NEW_CUSTOMER_COSTS, &
           TOTAL_PROGRAM_PARTICIPANT_COSTS,FREE_FACTOR, &
           DSM_PURCHASE_POWER, &
           TOTAL_DSM_PURCHASE_POWER, &
           TOTAL_DSM_SALES_REVENUE, &
           DSM_SALES_REVENUE

      INTEGER*2 ACCEPTANCE_NUM,BEGIN_APP_YEAR
!
!      CAPACITY PLANNING SECTION
!
      INTEGER*2 OPERATION_LIFE,R_OPERATION_LIFE
!

!      DECLARATION FOR LM_CAPACITY_PLANNING_ADDITIONS
!
      LOGICAL*1 R_GOOD_PROGRAM
      INTEGER*2   RESPONSE_NUM_BY_(12),RESPONSE_MN,R_APPLICATION, &
                  TEMP_RESPONSE_NUM,START_YEAR
      REAL*4   TEMP_BEFORE_LM_FACTOR(12),TEMP_AFTER_LM_FACTOR(12), &
               R_FIRST_YEAR_CAPACITY,SAVE_LM_RESOURCE_ADJUSTMENT, &
               SAVE_LM_PLANNED_ADJUSTMENT,UPDATE_NET_PLANNING_PEAK
!
!      DECLARATIONS FOR MONTHLY DSM PATTERN REPORT
!
      LOGICAL*1 MON_DSM_REPORT_NOT_OPEN/.TRUE./, &
                  MONTHLY_DSM_REPORT_SWITCH,LM_DAYS_REPORT
      CHARACTER*8 DAY_TITLE(3)
      CHARACTER*9 WRITE_MONTH_NAME(12)
      INTEGER*2 MON_DSM_NO,MON_DSM_PATTERN_HEADER
      INTEGER*4 MON_DSM_REC
!
!   ASSET CLASS
!
      INTEGER*2 NUM_OF_DSM_CLASSES, &
                 MAX_DSM_CLASS_ID_NUM, &
                DSM_ASSET_CLASS_POINTER(:)
      ALLOCATABLE :: DSM_ASSET_CLASS_POINTER
      INTEGER*2  R_NUM_OF_DSM_CLASSES, &
                 R_MAX_DSM_CLASS_NUM, &
                 R_DSM_CLASS_POINTERS(*)
      REAL PROGRAM_UTIL_NON_ELEC_COSTS, &
           PROGRAM_THIRD_PARTY_COSTS, &
           PROGRAM_OTH_PARTICIPANT_COSTS

!  DSM_START_YEAR WILL BECOME A VECTOR WITH
!  THE DSM FINANCIAL COMPONENT. DONE.
!
!  READ THE INFORMATION TO
!  APPLY DEVICE # TO CLASS #
!
!
! END DATA DECLARATIONS
!
      NOT_SYSTEM_BASED_FORECAST = .NOT. SYSTEM_BASED_FORECAST()
      APP_NUM = 1
      IF(ACCEPTANCE_ACTIVE(DSM_DEVICE_RECORDS,MAX_DSM_DEVICE_NUM)) THEN
         IF(.NOT. ALLOCATED(DEVICE_APPLICATION)) THEN
            ALLOCATE(DEVICE_APPLICATION(MAX_DSM_DEVICE_NUM), &
                     CUSTOMERS_BY_DAY_TYPE(3,DSM_DEVICE_RECORDS), &
                     APPLICATION_ID(DSM_DEVICE_RECORDS), &
                     APPLICATION_CLASS(DSM_DEVICE_RECORDS), &
                     OPERATING_METHOD(DSM_DEVICE_RECORDS), &
                     DSM_CAP_PLAN_FACTOR(DSM_DEVICE_RECORDS), &
                     DSM_ENERGY_CLASSIFICATION(DSM_DEVICE_RECORDS), &
                     FREE_RIDERS(DSM_DEVICE_RECORDS), &
                     FREE_DRIVERS(DSM_DEVICE_RECORDS), &
                     PROGRAM_LOSS_FACTOR(DSM_DEVICE_RECORDS), &
                     DSM_START_YEAR(DSM_DEVICE_RECORDS), &
                     DSM_DEVICE_NAME(DSM_DEVICE_RECORDS), &
                     RESPONSE_POINTER_FOR(3,DSM_DEVICE_RECORDS), &
                     PROGRAM_DSM_ALLOCATION_METHOD(DSM_DEVICE_RECORDS))
         ELSE
            DEALLOCATE(DEVICE_APPLICATION)
            ALLOCATE(DEVICE_APPLICATION(MAX_DSM_DEVICE_NUM))
         ENDIF
         DSM_CAP = 0.
         DEVICE_APPLICATION = 0
         CALL OPEN_DSM_ACCEPTENCE_FILE(10)
         REMEMBER_MONTHLY_DSM_DATA = .FALSE.
         IREC = 0
         DOWHILE (IREC < DSM_DEVICE_RECORDS)
            IREC = IREC + 1
            READ(10,REC=IREC,IOSTAT=IOS) APP_DELETE, &
                                DEVICE_NAME, &
                                APP_ID_NUM, &
                                RESPONSE_POINTER_FOR(3,APP_NUM), &
                                RESPONSE_POINTER_FOR(1,APP_NUM), &
                                RESPONSE_POINTER_FOR(2,APP_NUM), &
                                APPLICATION_CLASS(APP_NUM), &
                                DSM_CLASS_NAME, &
                                CUSTOMERS_BY_DAY_TYPE(1,APP_NUM), &
                                CUSTOMERS_BY_DAY_TYPE(2,APP_NUM), &
                                CUSTOMERS_BY_DAY_TYPE(3,APP_NUM), &
                                OPERATING_METHOD(APP_NUM), &
                                CUSTOMERS, &
                                DSM_CAP_PLAN_FACTOR(APP_NUM), &
                                DSM_START_YEAR(APP_NUM), &
                                BASE_CUST, &
                                DSM_ENERGY_CLASSIFICATION(APP_NUM), &
                                FREE_RIDERS(APP_NUM), &
                                FREE_DRIVERS(APP_NUM), &
                                PROGRAM_LOSS_FACTOR(APP_NUM), &
                                NEW_CUSTOMERS, &
                                PROGRAM_DSM_ALLOCATION_METHOD(APP_NUM)
            IF(IOS /= 0) EXIT
            IF(APP_DELETE > 7) CYCLE
            IF(APP_ID_NUM > 0) THEN
                  CLASS_NUM = APPLICATION_CLASS(APP_NUM)
!
                  CUSTOMERS_BY_DAY_TYPE(1,APP_NUM) = &
                            CUSTOMERS_BY_DAY_TYPE(1,APP_NUM)/100.
                  CUSTOMERS_BY_DAY_TYPE(2,APP_NUM) = &
                            CUSTOMERS_BY_DAY_TYPE(2,APP_NUM)/100.
                  CUSTOMERS_BY_DAY_TYPE(3,APP_NUM) = &
                            CUSTOMERS_BY_DAY_TYPE(3,APP_NUM)/100.

                  PROGRAM_LOSS_FACTOR(APP_NUM) = &
                                   PROGRAM_LOSS_FACTOR(APP_NUM)/100.
                  IF(CLASS_NUM > 0 .AND. &
                                    CLASS_NUM <= SYSTEM_CLASS_NUM) THEN
!
                     REMEMBER_MONTHLY_DSM_DATA = &
                          PROGRAM_DSM_ALLOCATION_METHOD(APP_NUM) /= 'S'
!
                     DSM_DEVICE_NAME(APP_NUM) = DEVICE_NAME
                     APPLICATION_ID(APP_NUM) = APP_ID_NUM
                     DEVICE_APPLICATION(APP_ID_NUM) = APP_NUM
                     DO I = 1, AVAIL_DATA_YEARS
                        LM_CUSTOMERS(I,APP_NUM) = CUSTOMERS(I)
                        IF(NEW_CUSTOMERS(I) == 0.) THEN
                           IF(I == 1) THEN
                              NEW_LM_CUSTOMERS(I,APP_NUM) = &
                                         MAX(0.,CUSTOMERS(I)-BASE_CUST)
                           ELSE
                              NEW_LM_CUSTOMERS(I,APP_NUM) = MAX(0., &
                                            LM_CUSTOMERS(I,APP_NUM) - &
                                             LM_CUSTOMERS(I-1,APP_NUM))
                           ENDIF
                        ELSE
                           NEW_LM_CUSTOMERS(I,APP_NUM)=NEW_CUSTOMERS(I)
                        ENDIF
                        IF(LM_CUSTOMERS(I,APP_NUM) < &
                                   NEW_LM_CUSTOMERS(I,APP_NUM) .OR. &
                               (I > 1 .AND. LM_CUSTOMERS(I,APP_NUM) > &
                                  LM_CUSTOMERS(I-1,APP_NUM) + &
                                     NEW_LM_CUSTOMERS(I,APP_NUM))) THEN
                           WRITE(4,*) 'For endpoint',gc_end_point
                           WRITE(4,*) 'device',APP_NUM,' , year',I
                           WRITE(4,*) 'the number of Customers', &
                                       LM_CUSTOMERS(I,APP_NUM)
                           WRITE(4,*) 'is inconsistent with New'// &
                                      ' Customers', &
                                       NEW_LM_CUSTOMERS(I,APP_NUM)
                           WRITE(4,*) 'plus previous year '// &
                                  'Customers',LM_CUSTOMERS(I-1,APP_NUM)
                           WRITE(4,*) ' '
                        ENDIF
                     ENDDO
                     APP_NUM = APP_NUM + 1
                  ELSE
                     WRITE(4,*) 'Invalid class number,',CLASS_NUM, &
                                ' for the ',trim(DSM_CLASS_NAME), &
                                ' class found in record',IREC
                  ENDIF
            ELSE
               WRITE(4,*) 'Invalid load management device number,', &
                          APP_ID_NUM,' for device ',trim(DEVICE_NAME), &
                          ' found in record',IREC
            ENDIF
         ENDDO
         CLOSE(10)
      ENDIF
      DELTA_CLASS_SALES_FROM_DSM = 0.

      APP_NUM = APP_NUM - 1
      LDMGT_ACTIVE = APP_NUM > 0
      CALL STORE_LDMGT_ACTIVE(LDMGT_ACTIVE)
      APPLICATION_NUM = APP_NUM
      IF(.NOT. LDMGT_ACTIVE) RETURN
!
!
      IF(RESPONSE_ACTIVE(DSM_RESPONSE_CURVES,MAX_RESPONSE_NUM)) THEN
         IF(.NOT. ALLOCATED(BASDEV)) THEN
            ALLOCATE(BASDEV(24,DSM_RESPONSE_CURVES), &
                     LMGDEV(24,DSM_RESPONSE_CURVES), &
                     BEFORE_LM_ADJUSTMENT(DSM_RESPONSE_CURVES), &
                     AFTER_LM_ADJUSTMENT(DSM_RESPONSE_CURVES), &
                     LM_LOAD_UNITS(DSM_RESPONSE_CURVES), &
                     RESPONSE_LOCATION_POINTER(MAX_RESPONSE_NUM), &
                     RESPONSE_DATA_LOCATION(12,MAX_RESPONSE_NUM), &
                     AFTER_LM_FACTOR(DSM_RESPONSE_CURVES), &
                     BEFORE_LM_FACTOR(DSM_RESPONSE_CURVES))
         ELSE
            DEALLOCATE(RESPONSE_DATA_LOCATION, &
                       RESPONSE_LOCATION_POINTER)
            ALLOCATE(RESPONSE_DATA_LOCATION(12,MAX_RESPONSE_NUM), &
                     RESPONSE_LOCATION_POINTER(MAX_RESPONSE_NUM))
         ENDIF
         RESPONSE_DATA_LOCATION = 0
         CALL OPEN_DSM_RESPONSE_FILE(10)
         IREC = 0
         LAST_LOCATION = 0
         RESPONSE_NUM = 1
         RESPONSE_LOCATION_POINTER = 0
!
         DOWHILE (IREC < DSM_RESPONSE_CURVES)
            IREC = IREC + 1
            READ(10,REC=IREC,IOSTAT=IOS) RES_DELETE,DEVICE_NUM, &
                                     DEVICE_NAME, &
                                     FIRST_MONTH, &
                                     LM_LOAD_UNITS(RESPONSE_NUM), &
                                    (BASDEV(HR,RESPONSE_NUM),HR=1,24), &
                                    (LMGDEV(HR,RESPONSE_NUM),HR=1,24), &
                                   BEFORE_LM_ADJUSTMENT(RESPONSE_NUM), &
                                    AFTER_LM_ADJUSTMENT(RESPONSE_NUM), &
                                     LAST_MONTH
            IF(IOS /= 0) EXIT
            IF(RES_DELETE > 7) CYCLE
           IF(DEVICE_NUM > 0 .AND. DEVICE_NUM <= MAX_RESPONSE_NUM) THEN
               LOCATION = RESPONSE_LOCATION_POINTER(DEVICE_NUM)
               IF(LOCATION == 0) THEN
                  LAST_LOCATION = LAST_LOCATION + 1
                  RESPONSE_LOCATION_POINTER(DEVICE_NUM) = LAST_LOCATION
                  LOCATION = LAST_LOCATION
               ENDIF
               IF(LAST_MONTH >= FIRST_MONTH) THEN
                  DO MO = FIRST_MONTH,LAST_MONTH
                     RESPONSE_DATA_LOCATION(MO,LOCATION) = RESPONSE_NUM
                  ENDDO
               ELSE
                  DO MO = FIRST_MONTH, 12
                     RESPONSE_DATA_LOCATION(MO,LOCATION) = RESPONSE_NUM
                  ENDDO
                  DO MO = 1, LAST_MONTH
                     RESPONSE_DATA_LOCATION(MO,LOCATION) = RESPONSE_NUM
                  ENDDO
               ENDIF
               RESPONSE_NUM = RESPONSE_NUM + 1
            ENDIF
         ENDDO
         CLOSE(10)
      ENDIF
      ACTIVE_DSM_RESPONSE_CURVES = RESPONSE_NUM - 1
      IF(ACTIVE_DSM_RESPONSE_CURVES == 0) RETURN

      IF(DSM_RESPONSE_CURVES > 0.) THEN
         AFTER_LM_FACTOR = 1.
         BEFORE_LM_FACTOR = 1.
      ENDIF
         DSM_DEVICE_PEAK = 0.
!
         DAY = 3 ! ONLY CHECKING PEAK DAY
!
      DO YR = 1, AVAIL_DATA_YEARS
!          YR = R_YEAR
         CURRENT_YEAR = get_BASE_YEAR() + YR
         SYSTEM_FORECAST_PEAK = CAPACITY_PEAKS(YR)
         MONTH = PEAK_MONTH(YR)
         PEAK_HOUR = HISTORICAL_PEAK_HOUR(1,MONTH)
!
         SYSDIF(1) = 0.
         SYSDIF(2) = 0.
         SYSDIF(3) = 0.
         LOAD_CREDIT = 0.
         DO APPLICATION = 1, APPLICATION_NUM
            IF(DSM_START_YEAR(APPLICATION) > CURRENT_YEAR) CYCLE
            IF(FREE_RIDERS(APPLICATION) >= 0.) THEN
               LOCAL_FREE_RIDERS = FREE_RIDERS(APPLICATION)/100.
            ELSE
               LOCAL_FREE_RIDERS = &
                   GET_VAR(FREE_RIDERS(APPLICATION),YR,"RIDER")/100.
            ENDIF
            IF(FREE_DRIVERS(APPLICATION) >= 0.) THEN
               LOCAL_FREE_DRIVERS = FREE_DRIVERS(APPLICATION)/100.
            ELSE
               LOCAL_FREE_DRIVERS = &
                  GET_VAR(FREE_DRIVERS(APPLICATION),YR,"DRIVER")/100.
            ENDIF
           APP_YEAR = YR-MAX(DSM_START_YEAR(APPLICATION)-get_BASE_YEAR()-1,0)
            CUSCAL = LM_CUSTOMERS(APP_YEAR,APPLICATION) * &
                   (1. - LOCAL_FREE_RIDERS  + LOCAL_FREE_DRIVERS) * &
                            CUSTOMERS_BY_DAY_TYPE(DAY,APPLICATION)
!
            DEVICE_NUM = RESPONSE_POINTER_FOR(DAY,APPLICATION)
!
!  CALCULATE THE AMOUNT OF DSM DUE TO THE LOAD MANAGEMENT DEVICE
!
            IF(CUSCAL == 0. .OR. DEVICE_NUM == 0) CYCLE
            LOCATION = RESPONSE_LOCATION_POINTER(DEVICE_NUM)
            IF(LOCATION < 0 .OR. LOCATION > LAST_LOCATION) THEN
               WRITE(4,*) "Response curves not found for Response"
               WRITE(4,*) "Pointer",DEVICE_NUM," and Application", &
                                           DSM_DEVICE_NAME(APPLICATION)
            ENDIF
            RESPONSE_NUM = RESPONSE_DATA_LOCATION(MONTH,LOCATION)
            IF(RESPONSE_NUM < 1) CYCLE
            IF(LM_LOAD_UNITS(RESPONSE_NUM) == 1) THEN
               CUSCAL = CUSCAL &
                     /(1.-PROGRAM_LOSS_FACTOR(APPLICATION))
            ELSE
               CUSCAL = CUSCAL &
                     /(1000. * (1.-PROGRAM_LOSS_FACTOR(APPLICATION)))
            ENDIF
            STODIF = (LMGDEV(PEAK_HOUR,RESPONSE_NUM) * &
                                     AFTER_LM_FACTOR(RESPONSE_NUM) - &
                            BASDEV(PEAK_HOUR,RESPONSE_NUM) * &
                                     BEFORE_LM_FACTOR(RESPONSE_NUM)) * &
                              CUSCAL * DSM_CAP_PLAN_FACTOR(APPLICATION)
!
            IF(OPERATING_METHOD(APPLICATION) == 'E') THEN
               SYSDIF(1) = SYSDIF(1) + STODIF
            ELSEIF(OPERATING_METHOD(APPLICATION) == 'I') THEN
               SYSDIF(2) = SYSDIF(2) + STODIF
            ELSEIF(OPERATING_METHOD(APPLICATION) == 'P') THEN
               SYSDIF(3) = SYSDIF(3) + STODIF
            ELSEIF(OPERATING_METHOD(APPLICATION) == 'L') THEN
               LOAD_CREDIT = LOAD_CREDIT + STODIF
            ENDIF
            DSM_DEVICE_PEAK(YR,APPLICATION) = -STODIF
!
         ENDDO   ! APPLICATION
!
!  FIND THE PEAK AFTER DSM FOR THE YEAR, RECORD THE AMOUNT
!  OF DSM BY OPERATING METHOD, AND THE PEAK PLANNING CAPACITY.
!
         IF(LOAD_CREDIT /= 0.) THEN
            PLANNING_PEAK = SAVE_LM_PLANNED_ADJUSTMENT(YR,LOAD_CREDIT)
         ENDIF
         if (yr==1) then
             call check_bounds_r_3s(dsm_cap, "dsm_objt:0001", &
            "dsm_cap", int(3,2), YR, int(1,2))
         end if
         DSM_CAP(1,YR,1) = -SYSDIF(1)
         DSM_CAP(2,YR,1) = -SYSDIF(2)
         DSM_CAP(3,YR,1) = -SYSDIF(3)


         IF(YR <  AVAIL_DATA_YEARS) THEN
            THIS_YEAR_IS = YR + get_BASE_YEAR()
            ! DSM_RESPONSE_CURVES
            DO DEVICE_NUM = 1, ACTIVE_DSM_RESPONSE_CURVES 
               TEMP_FACTOR = AFTER_LM_ADJUSTMENT(DEVICE_NUM)
               IF(TEMP_FACTOR < 0.) THEN
                  WRITE(DEVICE_NAME,1000) DEVICE_NUM,THIS_YEAR_IS
                  TEMP_FACTOR = GET_VAR(TEMP_FACTOR,YR,DEVICE_NAME)
               ENDIF
               AFTER_LM_FACTOR(DEVICE_NUM) = TEMP_FACTOR * &
                                            AFTER_LM_FACTOR(DEVICE_NUM)
!
               TEMP_FACTOR = BEFORE_LM_ADJUSTMENT(DEVICE_NUM)
               IF(TEMP_FACTOR < 0.) THEN
                  WRITE(DEVICE_NAME,1000) DEVICE_NUM,THIS_YEAR_IS
                  TEMP_FACTOR = GET_VAR(TEMP_FACTOR,YR,DEVICE_NAME)
               ENDIF
               BEFORE_LM_FACTOR(DEVICE_NUM) = TEMP_FACTOR * &
                                           BEFORE_LM_FACTOR(DEVICE_NUM)
            ENDDO
         ENDIF
      ENDDO

      TEMP_FIL = LD_FIN_FIL()
      IF(TEMP_FIL == 'NONE') RETURN
!      IF(.NOT. (INDEX(LD_FIN_FIL(),'NONE') == 0)) RETURN
!
!  DSM FINANCIAL CALCUALTIONS
!
!      *************************************************************
!      ENTRY DSM_FIN
!      *************************************************************
!
!
      NUM_OF_DSM_CLASSES = 0
      MAX_DSM_CLASS_ID_NUM = 0
      IF(.NOT. LMF_FILE_EXISTS(DSM_FINANCIAL_RECORDS)) RETURN
!
      IF(ALLOCATED(BASE_PROG_EXP_SAVE)) THEN
         DEALLOCATE(BASE_PROG_EXP_SAVE, &
                    ONGO_PROG_EXP_SAVE, &
                    BASE_CUST_EXP_SAVE, &
                    ONGO_CUST_EXP_SAVE, &
                    ONGO_NEW_CUST_EXP_SAVE, &
                    ONGO_KWH_EXP_SAVE, &
                    ONGO_KW_EXP_SAVE, &
                    BASE_PROG_CAP_SAVE, &
                    ONGO_PROG_CAP_SAVE, &
                    BASE_CUST_CAP_SAVE, &
                    ONGO_CUST_CAP_SAVE, &
                    ONGO_NEW_CUST_CAP_SAVE, &
                    ONGO_KWH_CAP_SAVE, &
                    ONGO_KW_CAP_SAVE, &
                    REBATE_CUST_EXP_SAVE, &
                    REBATE_NEW_CUST_EXP_SAVE, &
                    REBATE_CUST_CAP_SAVE, &
                    REBATE_NEW_CUST_CAP_SAVE, &
                    PARTICIPANT_CUST_COST_SAVE, &
                    PARTICIPANT_NEW_CUST_COST_SAVE, &
                    PARTICIPANT_COSTS_BY, &
                    UTIL_NON_ELEC_CUST_COST_SAVE, &
                    UTIL_NON_ELEC_NEW_COST_SAVE, &
                    THIRD_PARTY_CUST_COST_SAVE, &
                    THIRD_PARTY_NEW_CUST_COST_SAVE, &
                    OTH_PARTICIPANT_CUST_COST_SAVE, &
                    OTH_PARTICIPANT_NEW_COST_SAVE, &
                    DSM_ASSET_CLASS_POINTER, &
                    LF_DELETE)
      ENDIF
      ALLOCATE(BASE_PROG_EXP_SAVE(DSM_FINANCIAL_RECORDS), &
               ONGO_PROG_EXP_SAVE(DSM_FINANCIAL_RECORDS), &
               BASE_CUST_EXP_SAVE(DSM_FINANCIAL_RECORDS), &
               ONGO_CUST_EXP_SAVE(DSM_FINANCIAL_RECORDS), &
               ONGO_NEW_CUST_EXP_SAVE(DSM_FINANCIAL_RECORDS), &
               ONGO_KWH_EXP_SAVE(DSM_FINANCIAL_RECORDS), &
               ONGO_KW_EXP_SAVE(DSM_FINANCIAL_RECORDS), &
               BASE_PROG_CAP_SAVE(DSM_FINANCIAL_RECORDS), &
               ONGO_PROG_CAP_SAVE(DSM_FINANCIAL_RECORDS), &
               BASE_CUST_CAP_SAVE(DSM_FINANCIAL_RECORDS), &
               ONGO_CUST_CAP_SAVE(DSM_FINANCIAL_RECORDS), &
               ONGO_NEW_CUST_CAP_SAVE(DSM_FINANCIAL_RECORDS), &
               ONGO_KWH_CAP_SAVE(DSM_FINANCIAL_RECORDS), &
               ONGO_KW_CAP_SAVE(DSM_FINANCIAL_RECORDS), &
               REBATE_CUST_EXP_SAVE(DSM_FINANCIAL_RECORDS), &
               REBATE_NEW_CUST_EXP_SAVE(DSM_FINANCIAL_RECORDS), &
               REBATE_CUST_CAP_SAVE(DSM_FINANCIAL_RECORDS), &
               REBATE_NEW_CUST_CAP_SAVE(DSM_FINANCIAL_RECORDS), &
               PARTICIPANT_CUST_COST_SAVE(DSM_FINANCIAL_RECORDS), &
               PARTICIPANT_NEW_CUST_COST_SAVE(DSM_FINANCIAL_RECORDS), &
               PARTICIPANT_COSTS_BY(APPLICATION_NUM), &
               UTIL_NON_ELEC_CUST_COST_SAVE(DSM_FINANCIAL_RECORDS), &
               UTIL_NON_ELEC_NEW_COST_SAVE(DSM_FINANCIAL_RECORDS), &
               THIRD_PARTY_CUST_COST_SAVE(DSM_FINANCIAL_RECORDS), &
               THIRD_PARTY_NEW_CUST_COST_SAVE(DSM_FINANCIAL_RECORDS), &
               OTH_PARTICIPANT_CUST_COST_SAVE(DSM_FINANCIAL_RECORDS), &
               OTH_PARTICIPANT_NEW_COST_SAVE(DSM_FINANCIAL_RECORDS), &
               DSM_ASSET_CLASS_POINTER(1024), &
               LF_DELETE(DSM_FINANCIAL_RECORDS))
!
      DSM_ASSET_CLASS_POINTER = 0
      CALL OPEN_DSM_FINANCIAL_FILE(10)
!
      IREC = 0
      DEVICE_NUM = 1
      DOWHILE (DEVICE_NUM <= DSM_FINANCIAL_RECORDS)
         DSM_FINANCIAL_ACTIVE(DEVICE_NUM) = .FALSE.
         IREC = IREC + 1
         READ(10,REC=IREC,IOSTAT=IOS) &
               LF_DELETE(DEVICE_NUM), &
               ACCTNO,DESC(DEVICE_NUM), &
               ACCEPTANCE_POINTER, &
               CONSTRUCTION_PERIOD(DEVICE_NUM), &
               COLLECT(DEVICE_NUM), &
               BASE_PROG_EXP(DEVICE_NUM),ONGO_PROG_EXP(DEVICE_NUM), &
               ONGO_PROG_EXP_ESCAL(DEVICE_NUM), &
               BASE_CUST_EXP(DEVICE_NUM),ONGO_CUST_EXP(DEVICE_NUM), &
               ONGO_CUST_EXP_ESCAL(DEVICE_NUM), &
               ONGO_NEW_CUST_EXP(DEVICE_NUM), &
               ONGO_NEW_CUST_EXP_ESCAL(DEVICE_NUM), &
              ONGO_KWH_EXP(DEVICE_NUM),ONGO_KWH_EXP_ESCAL(DEVICE_NUM), &
               ONGO_KW_EXP(DEVICE_NUM),ONGO_KW_EXP_ESCAL(DEVICE_NUM), &
               BOKLF(DEVICE_NUM),TAXLF(DEVICE_NUM), &
               ADRLIFE(DEVICE_NUM),DEPMET(DEVICE_NUM), &
               DBRATE(DEVICE_NUM), &
               SERVICEMO(DEVICE_NUM),REG_TREAT(DEVICE_NUM), &
               TAXEXP(DEVICE_NUM),REGULATORY_ALLOCATOR(DEVICE_NUM), &
               BASE_PROG_CAP(DEVICE_NUM),ONGO_PROG_CAP(DEVICE_NUM), &
               ONGO_PROG_CAP_ESCAL(DEVICE_NUM), &
               BASE_CUST_CAP(DEVICE_NUM),ONGO_CUST_CAP(DEVICE_NUM), &
               ONGO_CUST_CAP_ESCAL(DEVICE_NUM), &
               ONGO_NEW_CUST_CAP(DEVICE_NUM), &
               ONGO_NEW_CUST_CAP_ESCAL(DEVICE_NUM), &
               ONGO_KWH_CAP(DEVICE_NUM), &
               ONGO_KWH_CAP_ESCAL(DEVICE_NUM), &
               ONGO_KW_CAP(DEVICE_NUM), &
               ONGO_KW_CAP_ESCAL(DEVICE_NUM), &
               REBATE_CUST_EXP(DEVICE_NUM), &
               REBATE_NEW_CUST_EXP(DEVICE_NUM), &
               REBATE_EXP_ESCAL(DEVICE_NUM), &
               REBATE_CUST_CAP(DEVICE_NUM), &
               REBATE_NEW_CUST_CAP(DEVICE_NUM), &
               REBATE_CAP_ESCAL(DEVICE_NUM), &
               PARTICIPANT_CUST_COST(DEVICE_NUM), &
               PARTICIPANT_NEW_CUST_COST(DEVICE_NUM), &
               PARTICIPANT_COST_ESCAL(DEVICE_NUM), &
               UTIL_NON_ELEC_CUST_COST(DEVICE_NUM), &
               UTIL_NON_ELEC_NEW_CUST_COST(DEVICE_NUM), &
               UTIL_NON_ELEC_COST_ESCAL(DEVICE_NUM), &
               THIRD_PARTY_CUST_COST(DEVICE_NUM), &
               THIRD_PARTY_NEW_CUST_COST(DEVICE_NUM), &
               THIRD_PARTY_COST_ESCAL(DEVICE_NUM), &
               OTH_PARTICIPANT_CUST_COST(DEVICE_NUM), &
               OTH_PARTICIPANT_NEW_CUST_COST(DEVICE_NUM), &
               OTH_PARTICIPANT_COST_ESCAL(DEVICE_NUM), &
               START_PROG_EXP_ESCAL(DEVICE_NUM), &
               BASE_CUST_EXP_ESCAL(DEVICE_NUM), &
               REBATE_NEW_EXP_ESCAL(DEVICE_NUM), &
               START_PROG_CAP_ESCAL(DEVICE_NUM), &
               BASE_CUST_CAP_ESCAL(DEVICE_NUM), &
               REBATE_NEW_CAP_ESCAL(DEVICE_NUM), &
               PARTICIPANT_NEW_COST_ESCAL(DEVICE_NUM), &
               UTIL_NON_ELEC_NEW_COST_ESCAL(DEVICE_NUM), &
               THIRD_PARTY_NEW_COST_ESCAL(DEVICE_NUM), &
               OTH_PARTICIPANT_NEW_COST_ESCAL(DEVICE_NUM), &
               DSM_ASSET_CLASS_NUM(DEVICE_NUM), &
               DSM_ASSET_CLASS_VECTOR(DEVICE_NUM)

         IF(IOS /= 0) EXIT
         IF(LF_DELETE(DEVICE_NUM) > 7) CYCLE
!
         BAD_ACCEPTANCE_POINTER = .FALSE.
!          IF(ACCEPTANCE_POINTER > MAX_DSM_DEVICE_NUM .OR.
!      +                                  ACCEPTANCE_POINTER < 0) THEN
!             BAD_ACCEPTANCE_POINTER = .TRUE.
         IF(DEVICE_APPLICATION(ACCEPTANCE_POINTER) == 0) THEN
            BAD_ACCEPTANCE_POINTER = .TRUE.
         ENDIF
         IF(BAD_ACCEPTANCE_POINTER) THEN
!
!  WARN IF THERE IS NOT A CORRESPONDING DEVICE IN THE
!  ACCEPTANCE FILE.
!
            WRITE(4,*) 'For Endpoint',gc_end_point,'   Device Number,'
            WRITE(4,*) 'pointer in record',IREC
            WRITE(4,*) 'in the Load Management Financial'
            WRITE(4,*) 'file was specified as:', ACCEPTANCE_POINTER
            WRITE(4,*) 'There is no corresponding device number '
            WRITE(4,*) 'in the Load Management Acceptance file.'
            WRITE(4,*)
            CYCLE
         ENDIF
!
         CALL SET_ASSET_CLASSES(DSM_ASSET_CLASS_NUM(DEVICE_NUM), &
                                NUM_OF_DSM_CLASSES, &
                                 MAX_DSM_CLASS_ID_NUM, &
                                DSM_ASSET_CLASS_POINTER)
!
         DEVICE_APPLIED_TO(DEVICE_NUM) = ACCEPTANCE_POINTER
         DSM_FINANCIAL_ACTIVE(DEVICE_NUM) = .TRUE.
         REGULATORY_ALLOCATOR(DEVICE_NUM) = &
                                  REGULATORY_ALLOCATOR(DEVICE_NUM)/100.
         DBRATE(DEVICE_NUM) = DBRATE(DEVICE_NUM)/100.
         TAXEXP(DEVICE_NUM) = TAXEXP(DEVICE_NUM)/100.
         BASE_CUST_EXP(DEVICE_NUM) = BASE_CUST_EXP(DEVICE_NUM)/MILLION
         ONGO_CUST_EXP(DEVICE_NUM) = ONGO_CUST_EXP(DEVICE_NUM)/MILLION
         ONGO_NEW_CUST_EXP(DEVICE_NUM) = &
                               ONGO_NEW_CUST_EXP(DEVICE_NUM)/MILLION
         REBATE_CUST_EXP(DEVICE_NUM) = &
                                 REBATE_CUST_EXP(DEVICE_NUM)/MILLION
         REBATE_NEW_CUST_EXP(DEVICE_NUM) = &
                             REBATE_NEW_CUST_EXP(DEVICE_NUM)/MILLION
         ONGO_KWH_EXP(DEVICE_NUM) = ONGO_KWH_EXP(DEVICE_NUM)/MILLION
         ONGO_KW_EXP(DEVICE_NUM) = ONGO_KW_EXP(DEVICE_NUM)/THOUSAND
!
         BASE_CUST_CAP(DEVICE_NUM) = BASE_CUST_CAP(DEVICE_NUM)/MILLION
         ONGO_CUST_CAP(DEVICE_NUM) = ONGO_CUST_CAP(DEVICE_NUM)/MILLION
         ONGO_NEW_CUST_CAP(DEVICE_NUM) = &
                               ONGO_NEW_CUST_CAP(DEVICE_NUM)/MILLION
         REBATE_CUST_CAP(DEVICE_NUM) = &
                                 REBATE_CUST_CAP(DEVICE_NUM)/MILLION
         REBATE_NEW_CUST_CAP(DEVICE_NUM) = &
                             REBATE_NEW_CUST_CAP(DEVICE_NUM)/MILLION
         ONGO_KWH_CAP(DEVICE_NUM) = ONGO_KWH_CAP(DEVICE_NUM)/MILLION
         ONGO_KW_CAP(DEVICE_NUM) = ONGO_KW_CAP(DEVICE_NUM)/THOUSAND
!
         PARTICIPANT_CUST_COST(DEVICE_NUM) = &
                              PARTICIPANT_CUST_COST(DEVICE_NUM)/MILLION
         PARTICIPANT_NEW_CUST_COST(DEVICE_NUM) = &
                          PARTICIPANT_NEW_CUST_COST(DEVICE_NUM)/MILLION
         UTIL_NON_ELEC_CUST_COST(DEVICE_NUM) = &
                            UTIL_NON_ELEC_CUST_COST(DEVICE_NUM)/MILLION
         UTIL_NON_ELEC_NEW_CUST_COST(DEVICE_NUM) = &
                        UTIL_NON_ELEC_NEW_CUST_COST(DEVICE_NUM)/MILLION
         THIRD_PARTY_CUST_COST(DEVICE_NUM) = &
                              THIRD_PARTY_CUST_COST(DEVICE_NUM)/MILLION
         THIRD_PARTY_NEW_CUST_COST(DEVICE_NUM) = &
                          THIRD_PARTY_NEW_CUST_COST(DEVICE_NUM)/MILLION
         OTH_PARTICIPANT_CUST_COST(DEVICE_NUM) = &
                          OTH_PARTICIPANT_CUST_COST(DEVICE_NUM)/MILLION
         OTH_PARTICIPANT_NEW_CUST_COST(DEVICE_NUM) = &
                      OTH_PARTICIPANT_NEW_CUST_COST(DEVICE_NUM)/MILLION
!
         BASE_PROG_EXP_SAVE(DEVICE_NUM) = BASE_PROG_EXP(DEVICE_NUM)
         ONGO_PROG_EXP_SAVE(DEVICE_NUM) = ONGO_PROG_EXP(DEVICE_NUM)
         BASE_CUST_EXP_SAVE(DEVICE_NUM) = BASE_CUST_EXP(DEVICE_NUM)
         ONGO_CUST_EXP_SAVE(DEVICE_NUM) = ONGO_CUST_EXP(DEVICE_NUM)
         ONGO_NEW_CUST_EXP_SAVE(DEVICE_NUM) = &
                                          ONGO_NEW_CUST_EXP(DEVICE_NUM)
         ONGO_KWH_EXP_SAVE(DEVICE_NUM) = ONGO_KWH_EXP(DEVICE_NUM)
         ONGO_KW_EXP_SAVE(DEVICE_NUM) = ONGO_KW_EXP(DEVICE_NUM)
         BASE_PROG_CAP_SAVE(DEVICE_NUM) = BASE_PROG_CAP(DEVICE_NUM)
         ONGO_PROG_CAP_SAVE(DEVICE_NUM) = ONGO_PROG_CAP(DEVICE_NUM)
         BASE_CUST_CAP_SAVE(DEVICE_NUM) = BASE_CUST_CAP(DEVICE_NUM)
         ONGO_CUST_CAP_SAVE(DEVICE_NUM) = ONGO_CUST_CAP(DEVICE_NUM)
         ONGO_NEW_CUST_CAP_SAVE(DEVICE_NUM) = &
                                          ONGO_NEW_CUST_CAP(DEVICE_NUM)
         ONGO_KWH_CAP_SAVE(DEVICE_NUM) = ONGO_KWH_CAP(DEVICE_NUM)
         ONGO_KW_CAP_SAVE(DEVICE_NUM) = ONGO_KW_CAP(DEVICE_NUM)
         REBATE_CUST_EXP_SAVE(DEVICE_NUM) = REBATE_CUST_EXP(DEVICE_NUM)
         REBATE_NEW_CUST_EXP_SAVE(DEVICE_NUM) = &
                                    REBATE_NEW_CUST_EXP(DEVICE_NUM)
         REBATE_CUST_CAP_SAVE(DEVICE_NUM) = REBATE_CUST_CAP(DEVICE_NUM)
         REBATE_NEW_CUST_CAP_SAVE(DEVICE_NUM) = &
                                        REBATE_NEW_CUST_CAP(DEVICE_NUM)
         PARTICIPANT_CUST_COST_SAVE(DEVICE_NUM) = &
                                      PARTICIPANT_CUST_COST(DEVICE_NUM)
        PARTICIPANT_NEW_CUST_COST_SAVE(DEVICE_NUM) = &
                                  PARTICIPANT_NEW_CUST_COST(DEVICE_NUM)
         UTIL_NON_ELEC_CUST_COST_SAVE(DEVICE_NUM) = &
                                    UTIL_NON_ELEC_CUST_COST(DEVICE_NUM)
         UTIL_NON_ELEC_NEW_COST_SAVE(DEVICE_NUM) = &
                                UTIL_NON_ELEC_NEW_CUST_COST(DEVICE_NUM)
         THIRD_PARTY_CUST_COST_SAVE(DEVICE_NUM) = &
                                      THIRD_PARTY_CUST_COST(DEVICE_NUM)
         THIRD_PARTY_NEW_CUST_COST_SAVE(DEVICE_NUM) = &
                                  THIRD_PARTY_NEW_CUST_COST(DEVICE_NUM)
         OTH_PARTICIPANT_CUST_COST_SAVE(DEVICE_NUM) = &
                                  OTH_PARTICIPANT_CUST_COST(DEVICE_NUM)
         OTH_PARTICIPANT_NEW_COST_SAVE(DEVICE_NUM) = &
                              OTH_PARTICIPANT_NEW_CUST_COST(DEVICE_NUM)
!
         DEVICE_NUM = DEVICE_NUM + 1
      ENDDO
      CLOSE(10)
      RETURN
! ********************************************************************
      ENTRY RETURN_NUM_OF_DSM_CLASSES(R_NUM_OF_DSM_CLASSES, &
                                      R_MAX_DSM_CLASS_NUM)
! ********************************************************************
         R_NUM_OF_DSM_CLASSES = NUM_OF_DSM_CLASSES
         R_MAX_DSM_CLASS_NUM = MAX_DSM_CLASS_ID_NUM
      RETURN
! ********************************************************************
      ENTRY RETURN_DSM_CLASS_POINTER(R_DSM_CLASS_POINTERS)
! ********************************************************************
         R_DSM_CLASS_POINTERS(1:MAX_DSM_CLASS_ID_NUM) = &
                        DSM_ASSET_CLASS_POINTER(1:MAX_DSM_CLASS_ID_NUM)
      RETURN
! ********************************************************************
      ENTRY LM_CAPACITY_PLANNING_ADDITIONS(R_APPLICATION, &
                                                 R_FIRST_YEAR_CAPACITY)
! ********************************************************************
      APPLICATION = DEVICE_APPLICATION(R_APPLICATION)
      R_GOOD_PROGRAM = .TRUE.
      DAY = 3 ! ONLY CHECKING PEAK DAY
      DEVICE_NUM = RESPONSE_POINTER_FOR(DAY,APPLICATION)
      IF(DEVICE_NUM == 0) THEN
         R_GOOD_PROGRAM = .FALSE.
         R_FIRST_YEAR_CAPACITY = 0.
         RETURN
      ENDIF
      LOCATION = RESPONSE_LOCATION_POINTER(DEVICE_NUM)
      DO MONTH = 1 , 12
         RESPONSE_NUM_BY_(MONTH) = &
                                 RESPONSE_DATA_LOCATION(MONTH,LOCATION)
         TEMP_RESPONSE_NUM = RESPONSE_NUM_BY_(MONTH)
         IF(TEMP_RESPONSE_NUM < 1) CYCLE
         TEMP_BEFORE_LM_FACTOR(MONTH) = BEFORE_LM_FACTOR( &
                                                     TEMP_RESPONSE_NUM)
         TEMP_AFTER_LM_FACTOR(MONTH) = AFTER_LM_FACTOR( &
                                                     TEMP_RESPONSE_NUM)
      ENDDO
      START_YEAR =  DSM_START_YEAR(APPLICATION) - get_BASE_YEAR()
      IF(FREE_RIDERS(APPLICATION) >= 0.) &
             LOCAL_FREE_RIDERS = FREE_RIDERS(APPLICATION)/100.
      IF(FREE_DRIVERS(APPLICATION) >= 0.) &
             LOCAL_FREE_DRIVERS = FREE_DRIVERS(APPLICATION)/100.
      DO YR = START_YEAR , AVAIL_DATA_YEARS
         IF(FREE_RIDERS(APPLICATION) < 0.) LOCAL_FREE_RIDERS = &
                   GET_VAR(FREE_RIDERS(APPLICATION),YR,"RIDER")/100.
         IF(FREE_DRIVERS(APPLICATION) < 0.) LOCAL_FREE_DRIVERS = &
                   GET_VAR(FREE_DRIVERS(APPLICATION),YR,"DRIVER")/100.
         APP_YEAR = YR - START_YEAR + 1
         SYSTEM_FORECAST_PEAK = CAPACITY_PEAKS(YR)
         MONTH = PEAK_MONTH(YR)
         TEMP_RESPONSE_NUM = RESPONSE_NUM_BY_(MONTH)
         IF(TEMP_RESPONSE_NUM < 1) CYCLE
         PEAK_HOUR = HISTORICAL_PEAK_HOUR(1,MONTH)
         SYSDIF(1) = 0.
         SYSDIF(2) = 0.
         SYSDIF(3) = 0.
         LOAD_CREDIT = 0.
         CUSCAL = LM_CUSTOMERS(APP_YEAR,APPLICATION) * &
                   (1. - LOCAL_FREE_RIDERS  + LOCAL_FREE_DRIVERS) * &
                            CUSTOMERS_BY_DAY_TYPE(DAY,APPLICATION)
         IF(LM_LOAD_UNITS(TEMP_RESPONSE_NUM) == 1) THEN
            CUSCAL = CUSCAL &
               /(1.-PROGRAM_LOSS_FACTOR(APPLICATION))
         ELSE
            CUSCAL = CUSCAL &
               /(1000. * (1.-PROGRAM_LOSS_FACTOR(APPLICATION)))
         ENDIF
         STODIF = (LMGDEV(PEAK_HOUR,TEMP_RESPONSE_NUM) * &
                               TEMP_AFTER_LM_FACTOR(MONTH) - &
                      BASDEV(PEAK_HOUR,TEMP_RESPONSE_NUM) * &
                               TEMP_BEFORE_LM_FACTOR(MONTH)) * &
                         CUSCAL * DSM_CAP_PLAN_FACTOR(APPLICATION)
         IF(YR == APP_YEAR) R_FIRST_YEAR_CAPACITY = STODIF
         IF(OPERATING_METHOD(APPLICATION) == 'E') THEN
            SYSDIF(1) = SYSDIF(1) + STODIF
         ELSEIF(OPERATING_METHOD(APPLICATION) == 'I') THEN
            SYSDIF(2) = SYSDIF(2) + STODIF
         ELSEIF(OPERATING_METHOD(APPLICATION) == 'P') THEN
            SYSDIF(3) = SYSDIF(3) + STODIF
         ELSEIF(OPERATING_METHOD(APPLICATION) == 'L') THEN
            LOAD_CREDIT = LOAD_CREDIT + STODIF
         ENDIF
         IF(LOAD_CREDIT /= 0.) THEN
            PLANNING_PEAK = SAVE_LM_RESOURCE_ADJUSTMENT(YR,LOAD_CREDIT)
            IF(YR == START_YEAR) &
                           PLANNING_PEAK = UPDATE_NET_PLANNING_PEAK(YR)
         ENDIF
         DSM_CAP(1,YR,2) = DSM_CAP(1,YR,2) - SYSDIF(1)
         DSM_CAP(2,YR,2) = DSM_CAP(2,YR,2) - SYSDIF(2)
         DSM_CAP(3,YR,2) = DSM_CAP(3,YR,2) - SYSDIF(3)
         IF(YR <  AVAIL_DATA_YEARS) THEN
            THIS_YEAR_IS = YR + get_BASE_YEAR()
            DO RESPONSE_MN = 1 , 12
               TEMP_RESPONSE_NUM = RESPONSE_NUM_BY_(RESPONSE_MN)
               IF(TEMP_RESPONSE_NUM < 1) CYCLE
               TEMP_FACTOR = AFTER_LM_ADJUSTMENT(TEMP_RESPONSE_NUM)
               IF(TEMP_FACTOR < 0.) THEN
                  WRITE(DEVICE_NAME,1000) TEMP_RESPONSE_NUM, &
                                                       THIS_YEAR_IS
                  TEMP_FACTOR = GET_VAR(TEMP_FACTOR,YR,DEVICE_NAME)
               ENDIF
               TEMP_AFTER_LM_FACTOR(RESPONSE_MN) = TEMP_FACTOR * &
                                   TEMP_AFTER_LM_FACTOR(RESPONSE_MN)
               TEMP_FACTOR = BEFORE_LM_ADJUSTMENT(TEMP_RESPONSE_NUM)
               IF(TEMP_FACTOR < 0.) THEN
                 WRITE(DEVICE_NAME,1000) TEMP_RESPONSE_NUM,THIS_YEAR_IS
                  TEMP_FACTOR = GET_VAR(TEMP_FACTOR,YR,DEVICE_NAME)
               ENDIF
               TEMP_BEFORE_LM_FACTOR(RESPONSE_MN) = TEMP_FACTOR * &
                                     TEMP_BEFORE_LM_FACTOR(RESPONSE_MN)
            ENDDO ! ANNUAL/MONTHLY UPDATE FOR RESPONSE CURVES
         ENDIF ! SECTION FOR TEMP UPDATE TO THE ADJ FACTORS
      ENDDO ! YEAR COUNTER: APP_YEAR , AVAIL_DATA_YEARS
      RETURN
! ********************************************************************
      ENTRY CHECK_WRITE_TO_DSM_DATA_BASE(WRITE_TO_DSM_DATA_BASE)
! ********************************************************************
         IF(REMEMBER_MONTHLY_DSM_DATA) THEN ! CAN ADD OTHER CONDITIONS
            IF(ALLOCATED(MONTHLY_DSM_DATA_BASE)) &
                          DEALLOCATE(MONTHLY_DSM_DATA_BASE)
            ALLOCATE(MONTHLY_DSM_DATA_BASE(25,APPLICATION_NUM,3))

            WRITE_TO_DSM_DATA_BASE = .TRUE.
         ELSE
            WRITE_TO_DSM_DATA_BASE = .FALSE.
         ENDIF

         GREG_PEAK_RESERVE_ALLOCATION = 0.
         GREG_ENERGY_RESERVE_ALLOCATION = 0.

      RETURN
! ********************************************************************
      ENTRY FREE_UP_DSM_DATA_BASE
! ********************************************************************
         DEALLOCATE(MONTHLY_DSM_DATA_BASE)
      RETURN
! ********************************************************************
      ENTRY WRITE_MONTHLY_DSM_REPORT(R_MONTH)
! ********************************************************************
         MONTH = R_MONTH
         IF(MON_DSM_REPORT_NOT_OPEN) THEN
            MON_DSM_NO = MON_DSM_PATTERN_HEADER(MON_DSM_REC)
            MON_DSM_REPORT_NOT_OPEN = .FALSE.
            DAY_TITLE(1) = 'Weekdays'
            DAY_TITLE(2) = 'Weekends'
            DAY_TITLE(3) = 'Peakdays'
            DO MO = 1 , 12
               WRITE_MONTH_NAME(MO) = LOADS_MONTH_NAME(MO)
            ENDDO
         ENDIF
         YR = globecom_year + get_BASE_YEAR()
         DO APPLICATION = 1 , APPLICATION_NUM
            IF(DSM_START_YEAR(APPLICATION) > YR) CYCLE
            DO DAY = 1 , 3
               WRITE(MON_DSM_NO,REC=MON_DSM_REC) PRT_ENDPOINT(), &
                  FLOAT(YR), &
                  WRITE_MONTH_NAME(MONTH), &
                  DSM_DEVICE_NAME(APPLICATION), &
                  DAY_TITLE(DAY), &
                  (MONTHLY_DSM_DATA_BASE( &
                                   HOUR,APPLICATION,DAY),HOUR=1,25)
               MON_DSM_REC = MON_DSM_REC + 1
            ENDDO
         ENDDO
      RETURN
! ********************************************************************
      ENTRY GET_RESERVES_FROM_DEVICE_PEAK(R_YR,R_CLASS, &
                                           R_RESERVES_FROM_DEVICE_PEAK)
! ********************************************************************
         R_RESERVES_FROM_DEVICE_PEAK = 0.
         CALL GET_LDMGT_ACTIVE(LDMGT_ACTIVE)
         IF(.NOT. LDMGT_ACTIVE) RETURN
         DO APPLICATION = 1 , APPLICATION_NUM
            ALLOC = INDEX('LMR', &
                            PROGRAM_DSM_ALLOCATION_METHOD(APPLICATION))
            IF(ALLOC > 1 .OR. &
                       R_CLASS /= APPLICATION_CLASS(APPLICATION)) CYCLE
          R_RESERVES_FROM_DEVICE_PEAK = R_RESERVES_FROM_DEVICE_PEAK + &
                                      DSM_DEVICE_PEAK(R_YR,APPLICATION)
         ENDDO
      RETURN
! ********************************************************************
      ENTRY GET_CLASS_MW_FROM_DSM(R_YR,DELTA_CLASS_MW_FROM_DSM)
! ********************************************************************
         DELTA_CLASS_MW_FROM_DSM(1:SYSTEM_CLASS_NUM) = 0.
         CALL GET_LDMGT_ACTIVE(LDMGT_ACTIVE)
         IF(.NOT. LDMGT_ACTIVE) RETURN
!
         YR = MIN(R_YR,get_globecom_study_period())
!
         POOLING_YES = POOLING_TRANSACTIONS()
         DO APPLICATION = 1 , APPLICATION_NUM
            IF(DSM_DEVICE_PEAK(YR,APPLICATION) == 0.0) CYCLE
            CLASS = APPLICATION_CLASS(APPLICATION)
            IF(CLASS > SYSTEM_CLASS_NUM .OR. CLASS < 1) CYCLE
            IF(CLASS <= MAX_LOAD_CLASSES) &
                  DELTA_CLASS_MW_FROM_DSM(CLASS) = &
                           DELTA_CLASS_MW_FROM_DSM(CLASS) + &
                              DSM_DEVICE_PEAK(YR,APPLICATION) * &
                                (1. - PROGRAM_LOSS_FACTOR(APPLICATION))
            DELTA_CLASS_MW_FROM_DSM(SYSTEM_CLASS_NUM) = &
                  DELTA_CLASS_MW_FROM_DSM(SYSTEM_CLASS_NUM) + &
                              DSM_DEVICE_PEAK(YR,APPLICATION) * &
                                (1. - PROGRAM_LOSS_FACTOR(APPLICATION))
         ENDDO
      RETURN
! ********************************************************************
      ENTRY TEST_CLASS_DSM_STORAGE
! ********************************************************************
      RETURN
! ********************************************************************
      ENTRY STORE_CLASS_MW_RESERVE_ALLOC(R_MONTH,PEAK_MW_HOUR, &
                                          PEAK_MW_DAY,PEAK_BEFORE_DSM)
! ********************************************************************
         DO APPLICATION = 1 , APPLICATION_NUM

            PEAK_MW_HOUR = HISTORICAL_PEAK_HOUR(1,R_MONTH)
            PEAK_MW_DAY = 3
            DIFSYS = MONTHLY_DSM_DATA_BASE(PEAK_MW_HOUR, &
                                               APPLICATION,PEAK_MW_DAY)
            IF(DIFSYS == 0.) CYCLE
            CLASS = APPLICATION_CLASS(APPLICATION)
            IF(CLASS > SYSTEM_CLASS_NUM .OR. CLASS < 1) CYCLE
            ALLOC = INDEX('LMR', &
                            PROGRAM_DSM_ALLOCATION_METHOD(APPLICATION))
            LDMAX = (PKPKD(PEAK_MW_HOUR,PEAK_MW_DAY,R_MONTH)- &
                          INTERCEPT(PEAK_MW_DAY,R_MONTH))/ &
                                             SLOPE(PEAK_MW_DAY,R_MONTH)
            IF(OPERATING_METHOD(APPLICATION) == 'E' .OR. &
                       OPERATING_METHOD(APPLICATION) == 'L')  THEN
               STODIF = DIFSYS
            ELSEIF(OPERATING_METHOD(APPLICATION) == 'I')  THEN
               STODIF =(DIFSYS + ((LDMAX+DIFSYS)/LDMAX - 1.)* &
                                                    PEAK_BEFORE_DSM)/2.
            ELSEIF(OPERATING_METHOD(APPLICATION) == 'P')  THEN
               STODIF = ((LDMAX+DIFSYS)/LDMAX-1.)  * PEAK_BEFORE_DSM
            ENDIF
            IF( CLASS <= MAX_LOAD_CLASSES) THEN

               GREG_PEAK_RESERVE_ALLOCATION(ALLOC,CLASS,R_MONTH) = &
                       GREG_PEAK_RESERVE_ALLOCATION(ALLOC,CLASS, &
                                                      R_MONTH) + STODIF
               GREG_ENERGY_RESERVE_ALLOCATION(ALLOC,CLASS,R_MONTH) = &
                 GREG_ENERGY_RESERVE_ALLOCATION(ALLOC,CLASS,R_MONTH) + &
                      MONTHLY_DSM_DATA_BASE(25,APPLICATION,PEAK_MW_DAY)
            ENDIF

            GREG_PEAK_RESERVE_ALLOCATION(ALLOC, &
                                           SYSTEM_CLASS_NUM,R_MONTH) = &
               GREG_PEAK_RESERVE_ALLOCATION(ALLOC, &
                                SYSTEM_CLASS_NUM,R_MONTH) + STODIF
            GREG_ENERGY_RESERVE_ALLOCATION(ALLOC, &
                                           SYSTEM_CLASS_NUM,R_MONTH) = &
                 GREG_ENERGY_RESERVE_ALLOCATION(ALLOC, &
                                           SYSTEM_CLASS_NUM,R_MONTH) + &
                      MONTHLY_DSM_DATA_BASE(25,APPLICATION,PEAK_MW_DAY)
         ENDDO
      RETURN
! ********************************************************************
      ENTRY GET_CLASS_MW_RESERVE_ALLOC(R_MONTH, &
                                       R_DSM_PEAK_RESERVE_ALLOCATION)
! ********************************************************************
!         IF(ALLOCATED(DSM_PEAK_RESERVE_ALLOCATION)) THEN
         CALL GET_LDMGT_ACTIVE(LDMGT_ACTIVE)
         IF(.NOT. LDMGT_ACTIVE) RETURN
            MONTH = R_MONTH
            DO CLASS = 1, MAX_LOAD_CLASSES
               R_DSM_PEAK_RESERVE_ALLOCATION(1,CLASS) = &
                         GREG_PEAK_RESERVE_ALLOCATION(1,CLASS,MONTH)
               R_DSM_PEAK_RESERVE_ALLOCATION(2,CLASS) = &
                         GREG_PEAK_RESERVE_ALLOCATION(2,CLASS,MONTH)
               R_DSM_PEAK_RESERVE_ALLOCATION(3,CLASS) = &
                         GREG_PEAK_RESERVE_ALLOCATION(3,CLASS,MONTH)
            ENDDO

      RETURN
! ********************************************************************
      ENTRY GET_CLASS_ENERGY_RESERVE_ALLOC(R_MONTH, &
                                       R_DSM_ENER_RESERVE_ALLOCATION)
! ********************************************************************
!         IF(ALLOCATED(GREG_ENERGY_RESERVE_ALLOCATION)) THEN
         CALL GET_LDMGT_ACTIVE(LDMGT_ACTIVE)
         IF(.NOT. LDMGT_ACTIVE) THEN
            R_DSM_ENER_RESERVE_ALLOCATION(1:3,1:MAX_LOAD_CLASSES) = 0.
            RETURN
         ENDIF
            MONTH = R_MONTH
            DO CLASS = 1, MAX_LOAD_CLASSES
               R_DSM_ENER_RESERVE_ALLOCATION(1,CLASS) = &
                         GREG_ENERGY_RESERVE_ALLOCATION(1,CLASS,MONTH)
               R_DSM_ENER_RESERVE_ALLOCATION(2,CLASS) = &
                         GREG_ENERGY_RESERVE_ALLOCATION(2,CLASS,MONTH)
               R_DSM_ENER_RESERVE_ALLOCATION(3,CLASS) = &
                         GREG_ENERGY_RESERVE_ALLOCATION(3,CLASS,MONTH)
            ENDDO

      RETURN
! ********************************************************************
      ENTRY GET_CLASS_DSM_ENERGY(R_MONTH,R_CLASS,R_ENERGY)
! ********************************************************************
         R_ENERGY = MONTHLY_DSM_ENERGY_BY_CLASS(R_MONTH,R_CLASS)
      RETURN
! ********************************************************************
      ENTRY GET_CLASS_DSM_ENERGY_ALLOC(R_MONTH,R_DSM_ENER_ALLOCATION)
! ********************************************************************
!         IF(ALLOCATED(GREG_ENERGY_RESERVE_ALLOCATION)) THEN
         CALL GET_LDMGT_ACTIVE(LDMGT_ACTIVE)
         IF(.NOT. LDMGT_ACTIVE) THEN
            R_DSM_ENER_ALLOCATION(1:MAX_LOAD_CLASSES) = 0.
            RETURN
         ENDIF
            MONTH = R_MONTH
            DO CLASS = 1, MAX_LOAD_CLASSES
               R_DSM_ENER_ALLOCATION(CLASS) = &
                          GREG_ENERGY_RESERVE_ALLOCATION(1,CLASS,MONTH)
            ENDDO

      RETURN
! ********************************************************************
      ENTRY GET_CLASS_PEAK_ENER_ALLOC(R_MONTH, &
                                       R_DSM_PEAK_RESERVE_ALLOCATION, &
                                       R_DSM_ENER_RESERVE_ALLOCATION)
! ********************************************************************
!         IF(ALLOCATED(DSM_PEAK_RESERVE_ALLOCATION)) THEN
         CALL GET_LDMGT_ACTIVE(LDMGT_ACTIVE)
         IF(.NOT. LDMGT_ACTIVE) THEN
            R_DSM_PEAK_RESERVE_ALLOCATION(1:3,1:MAX_LOAD_CLASSES) = 0.
            R_DSM_ENER_RESERVE_ALLOCATION(1:3,1:MAX_LOAD_CLASSES) = 0.
            RETURN
         ENDIF
            MONTH = R_MONTH
            DO CLASS = 1, MAX_LOAD_CLASSES

               R_DSM_PEAK_RESERVE_ALLOCATION(1,CLASS) = &
                         GREG_PEAK_RESERVE_ALLOCATION(1,CLASS,MONTH)
               R_DSM_PEAK_RESERVE_ALLOCATION(2,CLASS) = &
                         GREG_PEAK_RESERVE_ALLOCATION(2,CLASS,MONTH)
               R_DSM_PEAK_RESERVE_ALLOCATION(3,CLASS) = &
                         GREG_PEAK_RESERVE_ALLOCATION(3,CLASS,MONTH)
!
               R_DSM_ENER_RESERVE_ALLOCATION(1,CLASS) = &
                         GREG_ENERGY_RESERVE_ALLOCATION(1,CLASS,MONTH)
               R_DSM_ENER_RESERVE_ALLOCATION(2,CLASS) = &
                         GREG_ENERGY_RESERVE_ALLOCATION(2,CLASS,MONTH)
               R_DSM_ENER_RESERVE_ALLOCATION(3,CLASS) = &
                         GREG_ENERGY_RESERVE_ALLOCATION(3,CLASS,MONTH)
            ENDDO

         GREG_PEAK_RESERVE_ALLOCATION = 0.
         GREG_ENERGY_RESERVE_ALLOCATION = 0.

      RETURN
! ********************************************************************
      ENTRY DSM_PEAK_ENER_RESERVE_ALLOC(R_DSM_PEAK,R_DSM_ENER, &
                                                       R_CLASS,R_MONTH)
! ********************************************************************
         CALL GET_LDMGT_ACTIVE(LDMGT_ACTIVE)
         IF(.NOT. LDMGT_ACTIVE) THEN
            R_DSM_PEAK = 0.
            R_DSM_ENER = 0.
            RETURN
         ENDIF
         CLASS = R_CLASS
         MONTH = R_MONTH

            R_DSM_PEAK = GREG_PEAK_RESERVE_ALLOCATION(1,CLASS,MONTH)
            R_DSM_ENER = GREG_ENERGY_RESERVE_ALLOCATION(1,CLASS,MONTH)

      RETURN
!  COPYRIGHT (C) 1987 M.S. GERBER & ASSOCIATES, INC. ALL RIGHTS RESERVED

!      ***********************************************
      ENTRY LODMGT(LODDIF,CURRENT_MONTH,FORECAST_LOADS)
!      ***********************************************
!
!  CONTROLS SIMULATION OF EFFECTS OF USING VARIOUS LOAD MANAGEMENT
!  DEVICES.
!
      VOID_LOGICAL = GET_HISTORICAL_DAYS_PEAK_HR(REFERENCE_DAY_COUNT, &
                                                 REFERENCE_PEAK_HOUR)
      MONTH = CURRENT_MONTH
      CURRENT_YEAR = get_BASE_YEAR() + globecom_year
      CALL GET_CLASS_EXISTS(CLASS_EXISTS_loc)
      IF(.NOT. LAHEY_LF95()) CALL DISPLAY_DSM_ACTIVE
      FIRST_CLASS = 1
!
      IF(MONTH == 1) THEN
         DSM_DEVICE_ENERGY = 0.
         MONTHLY_DSM_ENERGY_BY_CLASS = 0.
         DELTA_CLASS_SALES_FROM_DSM = 0.
         MONTHLY_DSM_REPORT_SWITCH = LM_DAYS_REPORT()
         IF(globecom_year == 1) THEN
            AFTER_LM_FACTOR = 1.
            BEFORE_LM_FACTOR = 1.
         ENDIF
      ENDIF
      CLASS_PEAK_AFTER_DSM = 0.
!
      IF(REMEMBER_MONTHLY_DSM_DATA .OR. MONTHLY_DSM_REPORT_SWITCH) &
               MONTHLY_DSM_DATA_BASE = 0.
!
      FOUND_AN_ACTIVE_PROGRAM = .FALSE.
!
!  LOOP BY DAY TYPE
!
      DO DAY = 1 , 3
!
            DO APPLICATION = 1, APPLICATION_NUM
               IF(FREE_RIDERS(APPLICATION) >= 0.) THEN
                  LOCAL_FREE_RIDERS = FREE_RIDERS(APPLICATION)/100.
               ELSE
                  LOCAL_FREE_RIDERS = &
                    GET_VAR(FREE_RIDERS(APPLICATION),globecom_year,"RIDER")/100.
               ENDIF
               IF(FREE_DRIVERS(APPLICATION) >= 0.) THEN
                  LOCAL_FREE_DRIVERS = FREE_DRIVERS(APPLICATION)/100.
               ELSE
                  LOCAL_FREE_DRIVERS = &
                  GET_VAR(FREE_DRIVERS(APPLICATION),globecom_year,"DRIVER")/100.
               ENDIF
               CLASS = APPLICATION_CLASS(APPLICATION)

               DEVICE_AVE_ENERGY = 0.
               IF(DSM_START_YEAR(APPLICATION) > CURRENT_YEAR) CYCLE
               APP_YEAR = globecom_year - &
                         MAX(DSM_START_YEAR(APPLICATION)-get_BASE_YEAR()-1,0)
               CUSCAL = LM_CUSTOMERS(APP_YEAR,APPLICATION) * &
                   (1. - LOCAL_FREE_RIDERS  + LOCAL_FREE_DRIVERS) * &
                                 CUSTOMERS_BY_DAY_TYPE(DAY,APPLICATION)
!
               DEVICE_NUM = RESPONSE_POINTER_FOR(DAY,APPLICATION)
!
!  CALCULATE THE AMOUNT OF DSM DUE TO THE LOAD MANAGEMENT DEVICE
!
               IF(CUSCAL == 0. .OR. DEVICE_NUM == 0) CYCLE
               LOCATION = RESPONSE_LOCATION_POINTER(DEVICE_NUM)
               RESPONSE_NUM = RESPONSE_DATA_LOCATION(MONTH,LOCATION)
               IF(RESPONSE_NUM < 1) CYCLE
               IF(LM_LOAD_UNITS(RESPONSE_NUM) == 1) THEN
                  CUSCAL = CUSCAL &
                        /(1.-PROGRAM_LOSS_FACTOR(APPLICATION))
               ELSE
                  CUSCAL = CUSCAL &
                       /(1000. * (1.-PROGRAM_LOSS_FACTOR(APPLICATION)))
               ENDIF
!
               IF(DAY == 1) THEN
                  TOTAL_DAYS = REFERENCE_DAY_COUNT(DAY,MONTH) - 4
               ELSEIF(DAY == 2) THEN
                  TOTAL_DAYS = REFERENCE_DAY_COUNT(DAY,MONTH)
               ELSE
                  TOTAL_DAYS = 4
               ENDIF
!
               CUSCAL_AFTER = CUSCAL * AFTER_LM_FACTOR(RESPONSE_NUM)
               CUSCAL_BEFORE = CUSCAL * BEFORE_LM_FACTOR(RESPONSE_NUM)
!
               FOUND_AN_ACTIVE_PROGRAM = .TRUE.
!
               DO HOUR = 1, 24
!
!  CALCULATE DIFFERENCE IN HOURLY AVERAGE LOAD
!      DUE TO THE LOAD MANAGEMENT DEVICE.
!
!  STODIF COMPUTES DSM CONTRIBUTIONS FOR EACH CLASS
!  TO BE USED FOR FORECASTED DAYS REPORTS
!
                  DIFSYS = CUSCAL_AFTER * LMGDEV(HOUR,RESPONSE_NUM) - &
                              CUSCAL_BEFORE * BASDEV(HOUR,RESPONSE_NUM)
                 LDMAX = (PKPKD(HOUR,DAY,MONTH)-INTERCEPT(DAY,MONTH))/ &
                                       SLOPE(DAY,MONTH)
                  IF(OPERATING_METHOD(APPLICATION) == 'E' .OR. &
                            OPERATING_METHOD(APPLICATION) == 'L')  THEN
                     LODDIF(HOUR,DAY,1) = LODDIF(HOUR,DAY,1) + DIFSYS
                     STODIF = DIFSYS
                  ELSEIF(OPERATING_METHOD(APPLICATION) == 'I')  THEN
                     LODDIF(HOUR,DAY,2) = LODDIF(HOUR,DAY,2) + DIFSYS
                     STODIF = (DIFSYS + ((LDMAX+DIFSYS)/LDMAX - 1.) * &
                     FORECAST_LOADS(HOUR,DAY,MONTH,SYSTEM_CLASS_NUM))/2.
                  ELSEIF(OPERATING_METHOD(APPLICATION) == 'P')  THEN
                     LODDIF(HOUR,DAY,3) = LODDIF(HOUR,DAY,3) + DIFSYS
                     STODIF = ((LDMAX+DIFSYS)/LDMAX-1.)  * &
                        FORECAST_LOADS(HOUR,DAY,MONTH,SYSTEM_CLASS_NUM)
                  ENDIF
                  DEVICE_AVE_ENERGY = DEVICE_AVE_ENERGY  - STODIF
                  IF(REMEMBER_MONTHLY_DSM_DATA .OR. &
                                            MONTHLY_DSM_REPORT_SWITCH) &
                      MONTHLY_DSM_DATA_BASE(HOUR,APPLICATION,DAY) = &
                         MONTHLY_DSM_DATA_BASE(HOUR,APPLICATION,DAY) + &
                                                                 DIFSYS
!
!  ADJUST CLASS FORECAST PEAKS BY THE AMOUNT OF CHANGE
!  DUE TO LOAD MGT IN THE SYSTEM PEAK HOUR
!
                  IF(CLASS < SYSTEM_CLASS_NUM .AND. &
                                        NOT_SYSTEM_BASED_FORECAST) THEN
                     FORECAST_LOADS(HOUR,DAY,MONTH,CLASS) = &
                                 FORECAST_LOADS(HOUR,DAY,MONTH,CLASS)+ &
                                   STODIF* &
                                  (1.-PROGRAM_LOSS_FACTOR(APPLICATION))
                     CLASS_PEAK_AFTER_DSM(CLASS) = &
                                      MAX(CLASS_PEAK_AFTER_DSM(CLASS), &
                                  FORECAST_LOADS(HOUR,DAY,MONTH,CLASS))
                  ENDIF
               ENDDO !HOUR
!
               DEVICE_AVE_ENERGY = DEVICE_AVE_ENERGY * FLOAT(TOTAL_DAYS)
               DSM_DEVICE_ENERGY(APPLICATION) = &
                               DSM_DEVICE_ENERGY(APPLICATION) + &
                                                       DEVICE_AVE_ENERGY
               IF(REMEMBER_MONTHLY_DSM_DATA .OR. &
                                            MONTHLY_DSM_REPORT_SWITCH) &
                     MONTHLY_DSM_DATA_BASE(25,APPLICATION,DAY) = &
                           MONTHLY_DSM_DATA_BASE(25,APPLICATION,DAY) - &
                                                       DEVICE_AVE_ENERGY
!
               DEVICE_AVE_SALES = DEVICE_AVE_ENERGY * &
                                 (1. - PROGRAM_LOSS_FACTOR(APPLICATION))
!
               MONTHLY_DSM_ENERGY_BY_CLASS(CURRENT_MONTH,CLASS) = &
                    MONTHLY_DSM_ENERGY_BY_CLASS(CURRENT_MONTH,CLASS) + &
                                                        DEVICE_AVE_SALES
!
               IF(DSM_ENERGY_CLASSIFICATION(APPLICATION) == 'P') THEN
                  DELTA_PURCHASES_FROM_DSM(CLASS) = DEVICE_AVE_SALES + &
                                         DELTA_PURCHASES_FROM_DSM(CLASS)
               ELSEIF(DSM_ENERGY_CLASSIFICATION(APPLICATION) == 'O')THEN
                 DELTA_SYSTEM_SALES_FROM_DSM(CLASS)=DEVICE_AVE_SALES + &
                                      DELTA_SYSTEM_SALES_FROM_DSM(CLASS)
               ELSE
                DELTA_CLASS_SALES_FROM_DSM(CLASS) = DEVICE_AVE_SALES + &
                                       DELTA_CLASS_SALES_FROM_DSM(CLASS)
               ENDIF
               IF(CLASS < SYSTEM_CLASS_NUM) THEN
                  DELTA_CLASS_SALES_FROM_DSM(SYSTEM_CLASS_NUM) = &
                        DEVICE_AVE_SALES + &
                            DELTA_CLASS_SALES_FROM_DSM(SYSTEM_CLASS_NUM)
                  DELTA_PURCHASES_FROM_DSM(SYSTEM_CLASS_NUM) = &
                        DEVICE_AVE_SALES + &
                              DELTA_PURCHASES_FROM_DSM(SYSTEM_CLASS_NUM)
                  DELTA_SYSTEM_SALES_FROM_DSM(SYSTEM_CLASS_NUM) = &
                        DEVICE_AVE_SALES + &
                           DELTA_SYSTEM_SALES_FROM_DSM(SYSTEM_CLASS_NUM)
               ENDIF
            ENDDO !APPLICATION
      ENDDO !DAY
      IF(MONTHLY_DSM_REPORT_SWITCH .AND. &
                        FOUND_AN_ACTIVE_PROGRAM .AND. &
                                          .NOT. TESTING_PLAN) &
                                   CALL WRITE_MONTHLY_DSM_REPORT(MONTH)
      RETURN
!
!  COPYRIGHT (C) 1987 M.S. GERBER & ASSOCIATES, INC. ALL RIGHTS RESERVED
!      ***********************************************
      ENTRY WABASH_LODMGT(LODDIF,CURRENT_MONTH,FORECAST_LOADS)
!      ***********************************************
!
!  CONTROLS SIMULATION OF EFFECTS OF USING VARIOUS LOAD MANAGEMENT
!  DEVICES.
!
      IF(WABASH_IM_NIPSCO_PSI) THEN
         LAST_CLASS_IN_FORECAST = 3
      ELSE
         LAST_CLASS_IN_FORECAST = 2
      ENDIF
      VOID_LOGICAL = GET_HISTORICAL_DAYS_PEAK_HR(REFERENCE_DAY_COUNT, &
                                                 REFERENCE_PEAK_HOUR)
      MONTH = CURRENT_MONTH
      CURRENT_YEAR = get_BASE_YEAR() + globecom_year
      FIRST_CLASS = 1
      CALL GET_CLASS_EXISTS(CLASS_EXISTS_loc)
      CALL GET_CLASS_IN_AREA(CLASS_IN_AREA_1,FIRST_CLASS)
      IF(.NOT. LAHEY_LF95()) CALL DISPLAY_DSM_ACTIVE
!
      IF(REMEMBER_MONTHLY_DSM_DATA .OR. MONTHLY_DSM_REPORT_SWITCH) &
               MONTHLY_DSM_DATA_BASE = 0.
!
      IF(MONTH == 1) THEN
         DSM_DEVICE_ENERGY = 0.
         DELTA_CLASS_SALES_FROM_DSM = 0.
         IF(globecom_year == 1) THEN
            AFTER_LM_FACTOR = 1.
            BEFORE_LM_FACTOR = 1.
         ENDIF
      ENDIF
      CLASS_PEAK_AFTER_DSM = 0.
!
      FOUND_AN_ACTIVE_PROGRAM = .FALSE.
!
!  LOOP BY DAY TYPE
!
      DO DAY = 1 , 3
!
            DO APPLICATION = 1, APPLICATION_NUM
               IF(FREE_RIDERS(APPLICATION) >= 0.) THEN
                  LOCAL_FREE_RIDERS = FREE_RIDERS(APPLICATION)/100.
               ELSE
                  LOCAL_FREE_RIDERS = &
                     GET_VAR(FREE_RIDERS(APPLICATION),globecom_year,"RIDER")/100.
               ENDIF
               IF(FREE_DRIVERS(APPLICATION) >= 0.) THEN
                  LOCAL_FREE_DRIVERS = FREE_DRIVERS(APPLICATION)/100.
               ELSE
                  LOCAL_FREE_DRIVERS = &
                   GET_VAR(FREE_DRIVERS(APPLICATION),globecom_year,"DRIVER")/100.
               ENDIF
               CLASS = APPLICATION_CLASS(APPLICATION)
!                IF(CLASS < SYSTEM_CLASS_NUM) THEN
!                   IF(.NOT. CLASS_EXISTS_loc(CLASS)) CYCLE
!                ENDIF
               DEVICE_AVE_ENERGY = 0.
               IF(DSM_START_YEAR(APPLICATION) > CURRENT_YEAR) CYCLE
               APP_YEAR = globecom_year - &
                          MAX(DSM_START_YEAR(APPLICATION)-get_BASE_YEAR()-1,0)
               CUSCAL = LM_CUSTOMERS(APP_YEAR,APPLICATION) * &
                   (1. - LOCAL_FREE_RIDERS  + LOCAL_FREE_DRIVERS) * &
                                  CUSTOMERS_BY_DAY_TYPE(DAY,APPLICATION)
!
               DEVICE_NUM = RESPONSE_POINTER_FOR(DAY,APPLICATION)
!
!  CALCULATE THE AMOUNT OF DSM DUE TO THE LOAD MANAGEMENT DEVICE
!
               IF(CUSCAL == 0. .OR. DEVICE_NUM == 0) CYCLE
               LOCATION = RESPONSE_LOCATION_POINTER(DEVICE_NUM)
               RESPONSE_NUM = RESPONSE_DATA_LOCATION(MONTH,LOCATION)
               IF(RESPONSE_NUM < 1) CYCLE
               IF(LM_LOAD_UNITS(RESPONSE_NUM) == 1) THEN
                  CUSCAL = CUSCAL/(1.-PROGRAM_LOSS_FACTOR(APPLICATION))
               ELSE
                  CUSCAL = CUSCAL/ &
                         (1000. * (1.-PROGRAM_LOSS_FACTOR(APPLICATION)))
               ENDIF
!
               IF(DAY == 1) THEN
                  TOTAL_DAYS = REFERENCE_DAY_COUNT(DAY,MONTH) - 4
               ELSEIF(DAY == 2) THEN
                  TOTAL_DAYS = REFERENCE_DAY_COUNT(DAY,MONTH)
               ELSE
                  TOTAL_DAYS = 4
               ENDIF
!
               CUSCAL_AFTER = CUSCAL * AFTER_LM_FACTOR(RESPONSE_NUM)
               CUSCAL_BEFORE = CUSCAL * BEFORE_LM_FACTOR(RESPONSE_NUM)
!
               FOUND_AN_ACTIVE_PROGRAM = .TRUE.
!
               DO HOUR = 1, 24
!
!  CALCULATE DIFFERENCE IN HOURLY AVERAGE LOAD
!      DUE TO THE LOAD MANAGEMENT DEVICE.
!
!  STODIF COMPUTES DSM CONTRIBUTIONS FOR EACH CLASS
!  TO BE USED FOR FORECASTED DAYS REPORTS
!
                  DIFSYS = CUSCAL_AFTER * LMGDEV(HOUR,RESPONSE_NUM) - &
                               CUSCAL_BEFORE * BASDEV(HOUR,RESPONSE_NUM)
                 LDMAX = (PKPKD(HOUR,DAY,MONTH)-INTERCEPT(DAY,MONTH))/ &
                                       SLOPE(DAY,MONTH)
                  IF(OPERATING_METHOD(APPLICATION) == 'E' .OR. &
                             OPERATING_METHOD(APPLICATION) == 'L')  THEN
                     STODIF = DIFSYS
                  ELSEIF(OPERATING_METHOD(APPLICATION) == 'I')  THEN
                     STODIF = (DIFSYS + ((LDMAX+DIFSYS)/LDMAX - 1.) * &
                     FORECAST_LOADS(HOUR,DAY,MONTH,SYSTEM_CLASS_NUM))/2.
                  ELSEIF(OPERATING_METHOD(APPLICATION) == 'P')  THEN
                     STODIF = ((LDMAX+DIFSYS)/LDMAX-1.)  * &
                         FORECAST_LOADS(HOUR,DAY,MONTH,SYSTEM_CLASS_NUM)
                  ENDIF
                  DEVICE_AVE_ENERGY = DEVICE_AVE_ENERGY  - STODIF
!  IF A LOAD REDUCTION PROGRAM MODIFY THE LOAD
                  IF(PROGRAM_DSM_ALLOCATION_METHOD(APPLICATION)=='L') &
                                                                    THEN
                     L = MAX(1, &
                          INDEX('LEIP',OPERATING_METHOD(APPLICATION))-1)
!
!  REPLACED WITH THE STATEMENT BELOW FOR HOOSIER 12/21/95
!

                     IF(L>=1 .AND. L<=3 .AND. CLASS_IN_AREA_1(CLASS)) &
                            LODDIF(HOUR,DAY,L)=LODDIF(HOUR,DAY,L)+DIFSYS
!
!  ADJUST CLASS FORECAST PEAKS BY THE AMOUNT OF CHANGE
!  DUE TO LOAD MGT IN THE SYSTEM PEAK HOUR
!
                     IF(CLASS < SYSTEM_CLASS_NUM .AND. &
                                         NOT_SYSTEM_BASED_FORECAST) THEN
                        FORECAST_LOADS(HOUR,DAY,MONTH,CLASS) = &
                                 FORECAST_LOADS(HOUR,DAY,MONTH,CLASS)+ &
                                   STODIF* &
                                   (1.-PROGRAM_LOSS_FACTOR(APPLICATION))
                        CLASS_PEAK_AFTER_DSM(CLASS) = &
                                      MAX(CLASS_PEAK_AFTER_DSM(CLASS), &
                                  FORECAST_LOADS(HOUR,DAY,MONTH,CLASS))
                     ENDIF
                  ENDIF
                  IF(REMEMBER_MONTHLY_DSM_DATA .OR. &
                                            MONTHLY_DSM_REPORT_SWITCH) &
                      MONTHLY_DSM_DATA_BASE(HOUR,APPLICATION,DAY) = &
                         MONTHLY_DSM_DATA_BASE(HOUR,APPLICATION,DAY) + &
                                                                  DIFSYS
               ENDDO !HOUR
!
               DEVICE_AVE_ENERGY = DEVICE_AVE_ENERGY * FLOAT(TOTAL_DAYS)
               DSM_DEVICE_ENERGY(APPLICATION) = &
                              DSM_DEVICE_ENERGY(APPLICATION) + &
                                                       DEVICE_AVE_ENERGY
               IF(REMEMBER_MONTHLY_DSM_DATA .OR. &
                                            MONTHLY_DSM_REPORT_SWITCH) &
                     MONTHLY_DSM_DATA_BASE(25,APPLICATION,DAY) = &
                           MONTHLY_DSM_DATA_BASE(25,APPLICATION,DAY) - &
                                                       DEVICE_AVE_ENERGY
!
               IF(PROGRAM_DSM_ALLOCATION_METHOD(APPLICATION)/='L') CYCLE
               DEVICE_AVE_SALES = DEVICE_AVE_ENERGY * &
                                 (1. - PROGRAM_LOSS_FACTOR(APPLICATION))
               IF(DSM_ENERGY_CLASSIFICATION(APPLICATION) == 'P') THEN
                  DELTA_PURCHASES_FROM_DSM(CLASS) = DEVICE_AVE_SALES + &
                                         DELTA_PURCHASES_FROM_DSM(CLASS)
               ELSEIF(DSM_ENERGY_CLASSIFICATION(APPLICATION) == 'O')THEN
                DELTA_SYSTEM_SALES_FROM_DSM(CLASS)=DEVICE_AVE_SALES + &
                                      DELTA_SYSTEM_SALES_FROM_DSM(CLASS)
               ELSE
                DELTA_CLASS_SALES_FROM_DSM(CLASS) = DEVICE_AVE_SALES + &
                                       DELTA_CLASS_SALES_FROM_DSM(CLASS)
               ENDIF
               IF(CLASS < SYSTEM_CLASS_NUM) THEN
                  IF(DSM_ENERGY_CLASSIFICATION(APPLICATION) == 'P') THEN
                     DELTA_PURCHASES_FROM_DSM(SYSTEM_CLASS_NUM) = &
                           DEVICE_AVE_SALES + &
                              DELTA_PURCHASES_FROM_DSM(SYSTEM_CLASS_NUM)
                 ELSEIF(DSM_ENERGY_CLASSIFICATION(APPLICATION) == 'O') &
                                                                    THEN
                     DELTA_SYSTEM_SALES_FROM_DSM(SYSTEM_CLASS_NUM) = &
                           DEVICE_AVE_SALES + &
                           DELTA_SYSTEM_SALES_FROM_DSM(SYSTEM_CLASS_NUM)
                  ELSE
                     DELTA_CLASS_SALES_FROM_DSM(SYSTEM_CLASS_NUM) = &
                        DEVICE_AVE_SALES + &
                            DELTA_CLASS_SALES_FROM_DSM(SYSTEM_CLASS_NUM)
                  ENDIF
               ENDIF
            ENDDO !APPLICATION
      ENDDO !DAY
      IF(MONTHLY_DSM_REPORT_SWITCH .AND. &
                        FOUND_AN_ACTIVE_PROGRAM .AND. &
                                          .NOT. TESTING_PLAN) &
                                   CALL WRITE_MONTHLY_DSM_REPORT(MONTH)
!      IF(MONTHLY_DSM_REPORT_SWITCH .AND. .NOT. TESTING_PLAN)
!     +                             CALL WRITE_MONTHLY_DSM_REPORT(MONTH)
      RETURN
! ********************************************************************
      ENTRY UPDATE_DSM_RESPONSE_CURVES
! ********************************************************************
!
!  ADJUST THE HOURLY VALUES
!
         THIS_YEAR_IS = globecom_year + get_BASE_YEAR()
         ! DSM_RESPONSE_CURVES
         DO DEVICE_NUM = 1, ACTIVE_DSM_RESPONSE_CURVES 
            TEMP_FACTOR = AFTER_LM_ADJUSTMENT(DEVICE_NUM)
            IF(TEMP_FACTOR < 0.) THEN
               WRITE(DEVICE_NAME,1000) DEVICE_NUM,THIS_YEAR_IS
               TEMP_FACTOR = GET_VAR(TEMP_FACTOR,globecom_year,DEVICE_NAME)
            ENDIF
            AFTER_LM_FACTOR(DEVICE_NUM) = TEMP_FACTOR * &
                                          AFTER_LM_FACTOR(DEVICE_NUM)
!
            TEMP_FACTOR = BEFORE_LM_ADJUSTMENT(DEVICE_NUM)
            IF(TEMP_FACTOR < 0.) THEN
               WRITE(DEVICE_NAME,1000) DEVICE_NUM,THIS_YEAR_IS
               TEMP_FACTOR = GET_VAR(TEMP_FACTOR,globecom_year,DEVICE_NAME)
            ENDIF
            BEFORE_LM_FACTOR(DEVICE_NUM) = TEMP_FACTOR * &
                                         BEFORE_LM_FACTOR(DEVICE_NUM)
         ENDDO
      RETURN
! ********************************************************************
      ENTRY GET_DEVICE_APPLICATION(R_DEVICE_NUM,R_ACCEPTANCE_NUM)
! ********************************************************************
         IF(R_DEVICE_NUM > 0 .AND. &
                                R_DEVICE_NUM <= MAX_DSM_DEVICE_NUM) THEN
            R_ACCEPTANCE_NUM = DEVICE_APPLICATION(R_DEVICE_NUM)
         ELSE
            R_ACCEPTANCE_NUM = 0
         ENDIF
      RETURN
! ********************************************************************
      ENTRY GET_DSM_START_YEAR(R_ACCEPTANCE_NUM,R_DSM_START_YEAR)
! ********************************************************************
         R_DSM_START_YEAR = DSM_START_YEAR(R_ACCEPTANCE_NUM)
      RETURN
!
! ********************************************************************
      ENTRY LM_OPTION_CHECK(R_DEVICE_NUM,R_ACTIVE_DSM_PROG)
! ********************************************************************
         R_ACTIVE_DSM_PROG = .FALSE.
         IF(R_DEVICE_NUM > 0 .AND. &
                                R_DEVICE_NUM <= MAX_DSM_DEVICE_NUM) THEN
            IF(DEVICE_APPLICATION(R_DEVICE_NUM) > 0) THEN
               IF(DSM_START_YEAR(DEVICE_APPLICATION(R_DEVICE_NUM)) > &
                                               gc_last_study_year) THEN
                  R_ACTIVE_DSM_PROG = .TRUE.
               ENDIF
            ENDIF
         ENDIF
      RETURN
! ********************************************************************
      ENTRY ADD_NEW_LM_UNIT(R_DSM_START_YEAR,R_ACCEPTANCE_NUM, &
                            R_OPERATION_LIFE,R_REAL_ADDITION)
! ********************************************************************
         OPERATION_LIFE = R_OPERATION_LIFE
         ACCEPTANCE_NUM = DEVICE_APPLICATION(R_ACCEPTANCE_NUM)
         DSM_START_YEAR(ACCEPTANCE_NUM) = R_DSM_START_YEAR
         IF(R_REAL_ADDITION) THEN
!            ADD_CAPACITY_TO_DSM_CAP
         ENDIF
!         IF(OPERATION_LIFE > 0 .AND.
!     +                   OPERATION_LIFE < AVAIL_DATA_YEARS) THEN
!            DO I = OPERATION_LIFE + 1 , AVAIL_DATA_YEARS
!               NEW_LM_CUSTOMERS(I,ACCEPTANCE_NUM) = 0.0
!               LM_CUSTOMERS(I,ACCEPTANCE_NUM) = 0.0
!            ENDDO
!         ENDIF
      RETURN
! ********************************************************************
      ENTRY RESET_LM_ADDITIONS(ADDED_LM_PROGRAMS,DSM_OPTIONS_ADDED)
! ********************************************************************
         DO I = 1, DSM_OPTIONS_ADDED
            ACCEPTANCE_NUM = ADDED_LM_PROGRAMS(I)
            IF(ACCEPTANCE_NUM < 1) EXIT
            ACCEPTANCE_NUM = DEVICE_APPLICATION(ACCEPTANCE_NUM)
            DSM_START_YEAR(ACCEPTANCE_NUM) = 2200
         ENDDO
! ********************************************************************
      ENTRY RESET_LM_COSTS
! ********************************************************************
         DO I = 1, DSM_FINANCIAL_RECORDS
            BASE_PROG_EXP(I) = BASE_PROG_EXP_SAVE(I)
            ONGO_PROG_EXP(I) = ONGO_PROG_EXP_SAVE(I)
            BASE_CUST_EXP(I) = BASE_CUST_EXP_SAVE(I)
            ONGO_CUST_EXP(I) = ONGO_CUST_EXP_SAVE(I)
            ONGO_NEW_CUST_EXP(I) = ONGO_NEW_CUST_EXP_SAVE(I)
            ONGO_KWH_EXP(I) = ONGO_KWH_EXP_SAVE(I)
            ONGO_KW_EXP(I) = ONGO_KW_EXP_SAVE(I)
            BASE_PROG_CAP(I) = BASE_PROG_CAP_SAVE(I)
            ONGO_PROG_CAP(I) = ONGO_PROG_CAP_SAVE(I)
            BASE_CUST_CAP(I) = BASE_CUST_CAP_SAVE(I)
            ONGO_CUST_CAP(I) = ONGO_CUST_CAP_SAVE(I)
            ONGO_NEW_CUST_CAP(I) = ONGO_NEW_CUST_CAP_SAVE(I)
            ONGO_KWH_CAP(I) = ONGO_KWH_CAP_SAVE(I)
            ONGO_KW_CAP(I) = ONGO_KW_CAP_SAVE(I)
            REBATE_CUST_EXP(I) = REBATE_CUST_EXP_SAVE(I)
            REBATE_NEW_CUST_EXP(I) = REBATE_NEW_CUST_EXP_SAVE(I)
            REBATE_CUST_CAP(I) = REBATE_CUST_CAP_SAVE(I)
            REBATE_NEW_CUST_CAP(I) = REBATE_NEW_CUST_CAP_SAVE(I)
            PARTICIPANT_CUST_COST(I) = PARTICIPANT_CUST_COST_SAVE(I)
            PARTICIPANT_NEW_CUST_COST(I) = &
                                       PARTICIPANT_NEW_CUST_COST_SAVE(I)
            UTIL_NON_ELEC_CUST_COST(I) = &
                                         UTIL_NON_ELEC_CUST_COST_SAVE(I)
            UTIL_NON_ELEC_NEW_CUST_COST(I) = &
                                          UTIL_NON_ELEC_NEW_COST_SAVE(I)
            THIRD_PARTY_CUST_COST(I) = THIRD_PARTY_CUST_COST_SAVE(I)
            THIRD_PARTY_NEW_CUST_COST(I) = &
                                       THIRD_PARTY_NEW_CUST_COST_SAVE(I)
            OTH_PARTICIPANT_CUST_COST(I) = &
                                       OTH_PARTICIPANT_CUST_COST_SAVE(I)
            OTH_PARTICIPANT_NEW_CUST_COST(I) = &
                                        OTH_PARTICIPANT_NEW_COST_SAVE(I)
         ENDDO
      RETURN
! ********************************************************************
      ENTRY GET_DSM_CAP(R_DSM_CAP)
! ********************************************************************
         DO YR = 1, get_globecom_study_period()
            R_DSM_CAP(1,YR) = DSM_CAP(1,YR,1) + DSM_CAP(1,YR,2)
            R_DSM_CAP(2,YR) = DSM_CAP(2,YR,1) + DSM_CAP(2,YR,2)
            R_DSM_CAP(3,YR) = DSM_CAP(3,YR,1) + DSM_CAP(3,YR,2)
         ENDDO
      RETURN
! ********************************************************************
      ENTRY GET_DSM_CAPACITY(R_DSM_CAPACITY,R_YEAR)
! ********************************************************************
         YR = MIN(get_globecom_study_period(),R_YEAR)
         R_DSM_CAPACITY = DSM_CAP(1,YR,1) + DSM_CAP(1,YR,2) + &
                          DSM_CAP(2,YR,1) + DSM_CAP(2,YR,2) + &
                          DSM_CAP(3,YR,1) + DSM_CAP(3,YR,2)
      RETURN
! ********************************************************************
      ENTRY GET_NEW_DSM_CAPACITY(R_DSM_CAPACITY,R_YEAR)
! ********************************************************************
         YR = MIN(get_globecom_study_period(),R_YEAR)
         R_DSM_CAPACITY = DSM_CAP(1,YR,2) + &
                          DSM_CAP(2,YR,2) + &
                          DSM_CAP(3,YR,2)
      RETURN
! ********************************************************************
      ENTRY GET_OLD_DSM_CAPACITY(R_DSM_CAPACITY,R_YEAR)
! ********************************************************************
         YR = MIN(get_globecom_study_period(),R_YEAR)
         R_DSM_CAPACITY = DSM_CAP(1,YR,1) + &
                          DSM_CAP(2,YR,1) + &
                          DSM_CAP(3,YR,1)
      RETURN
 1000 FORMAT('Device #',I2,' in ',I2,'/',I4)
! ********************************************************************
! *                                                                    *
! *                        DSM_FINANCIAL_COSTS                         *
! *                                                                    *
! *         COPYRIGHT (C) 1990   M.S. GERBER & ASSOCIATES, INC         *
! *                       ALL RIGHTS RESERVED                          *
! *                                                                    *
! ********************************************************************
!                                                                      *
!      PURPOSE:                                                        *
!         DSM_FINANCIAL_COSTS CALCULATES THE DSM COSTS THAT ARE        *
!         EXPENSED AND CAPTIALIZED.  FOR THE CAPITIALIZED COSTS        *
!         BOOK AND TAX ITEMS ARE CALCULATED.                           *
!                                                                      *
! ********************************************************************
      ENTRY DSM_FINANCIAL_COST
! ********************************************************************
!      INITIALIZE VARIABLES
!
      CALL SET_UP_ANNUAL_DSM_ASSET_ARRAYS
      CALL INIT_ANNUAL_DSM_ASSET_ARRAYS
      IF(globecom_year == 1) THEN
         CALL SET_UP_ZERO_STUDY_DSM_ARRAYS
      ENDIF
!
      DSM_CASH = 0.
      DSM_DEFERRED_DEBIT = 0.
      DSM_REGULATED_DEFERRED_DEBIT = 0.
      DSM_GPV = 0.
      DSM_REGULATED_NPV = 0.
      DSM_AFUDC = 0.
      RB_DSM_AFUDC = 0.
      DSM_TAX_EXPENSE = 0.
      RB_DSM_TAX_EXPENSE = 0.
      TOP_OF_REPORT = 0
      REPORT_COUNTER = 0
      TOTAL_PARTICIPANT_COSTS = 0.
      TOTAL_UTIL_NON_ELEC_COSTS = 0.
      TOTAL_THIRD_PARTY_COSTS = 0.
      TOTAL_OTH_PARTICIPANT_COSTS = 0.
      TOTAL_DSM_PURCHASE_POWER = 0.
      TOTAL_DSM_SALES_REVENUE = 0.
!
      DSM_EXPENSE_COLECTION = 0.
      DSM_REBATE_COLECTION = 0.
      PARTICIPANT_COSTS_BY = 0.
!
      DSM_YEAR = MIN(globecom_year,get_globecom_study_period())
      DSM_FINANCIAL_REPORT_SWITCH = DSM_FINANCIAL_REPORT()
      FIRST_DEVICE = .TRUE.
!
!  ESCALATE THE COSTS
!
      VOID_INT2 = ESCALATE_ALL_VALUES(BASE_PROG_EXP, &
                                       START_PROG_EXP_ESCAL, &
                                         MAX_DSM_FINANCIAL_RECORDS,DESC)
      VOID_INT2 = ESCALATE_ALL_VALUES( &
           ONGO_PROG_EXP,ONGO_PROG_EXP_ESCAL, &
                                         MAX_DSM_FINANCIAL_RECORDS,DESC)
!
      VOID_INT2 = ESCALATE_ALL_VALUES( &
        ONGO_CUST_EXP,ONGO_CUST_EXP_ESCAL, &
                                      MAX_DSM_FINANCIAL_RECORDS,DESC)
      VOID_INT2 = ESCALATE_ALL_VALUES( &
      BASE_CUST_EXP,BASE_CUST_EXP_ESCAL, &
                                         MAX_DSM_FINANCIAL_RECORDS,DESC)
!
      VOID_INT2 = ESCALATE_ALL_VALUES(ONGO_NEW_CUST_EXP, &
                                        ONGO_NEW_CUST_EXP_ESCAL, &
                                         MAX_DSM_FINANCIAL_RECORDS,DESC)
!
      VOID_INT2 = ESCALATE_ALL_VALUES(ONGO_KWH_EXP,ONGO_KWH_EXP_ESCAL, &
                                         MAX_DSM_FINANCIAL_RECORDS,DESC)
!
      VOID_INT2 = ESCALATE_ALL_VALUES(ONGO_KW_EXP,ONGO_KW_EXP_ESCAL, &
                                         MAX_DSM_FINANCIAL_RECORDS,DESC)
!
      VOID_INT2 = ESCALATE_ALL_VALUES( &
      REBATE_CUST_EXP,REBATE_EXP_ESCAL, &
                                         MAX_DSM_FINANCIAL_RECORDS,DESC)
      VOID_INT2 = ESCALATE_ALL_VALUES(REBATE_NEW_CUST_EXP, &
                                         REBATE_NEW_EXP_ESCAL, &
                                         MAX_DSM_FINANCIAL_RECORDS,DESC)
!
!  CAPITAL COST ITEMS
!
      VOID_INT2 = ESCALATE_ALL_VALUES(BASE_PROG_CAP, &
                                       START_PROG_CAP_ESCAL, &
                                         MAX_DSM_FINANCIAL_RECORDS,DESC)
      VOID_INT2 = ESCALATE_ALL_VALUES( &
      ONGO_PROG_CAP,ONGO_PROG_CAP_ESCAL, &
                                         MAX_DSM_FINANCIAL_RECORDS,DESC)
!
      VOID_INT2 = ESCALATE_ALL_VALUES( &
      ONGO_CUST_CAP,ONGO_CUST_CAP_ESCAL, &
                                         MAX_DSM_FINANCIAL_RECORDS,DESC)
      VOID_INT2 = ESCALATE_ALL_VALUES( &
      BASE_CUST_CAP,ONGO_CUST_CAP_ESCAL, &
                                         MAX_DSM_FINANCIAL_RECORDS,DESC)
!
      VOID_INT2 = ESCALATE_ALL_VALUES(ONGO_NEW_CUST_CAP, &
                                            ONGO_NEW_CUST_CAP_ESCAL, &
                                         MAX_DSM_FINANCIAL_RECORDS,DESC)
!
      VOID_INT2 = ESCALATE_ALL_VALUES(ONGO_KWH_CAP,ONGO_KWH_CAP_ESCAL, &
                                         MAX_DSM_FINANCIAL_RECORDS,DESC)
!
      VOID_INT2 = ESCALATE_ALL_VALUES(ONGO_KW_CAP,ONGO_KW_CAP_ESCAL, &
                                         MAX_DSM_FINANCIAL_RECORDS,DESC)
!
      VOID_INT2 = ESCALATE_ALL_VALUES( &
      REBATE_CUST_CAP,REBATE_CAP_ESCAL, &
                                         MAX_DSM_FINANCIAL_RECORDS,DESC)
      VOID_INT2 = ESCALATE_ALL_VALUES(REBATE_NEW_CUST_CAP, &
                                         REBATE_NEW_CAP_ESCAL, &
                                         MAX_DSM_FINANCIAL_RECORDS,DESC)
!
!  NON-UTILITY COST ITEMS
!
      VOID_INT2 = ESCALATE_ALL_VALUES(PARTICIPANT_CUST_COST, &
                                         PARTICIPANT_COST_ESCAL, &
                                         MAX_DSM_FINANCIAL_RECORDS,DESC)
      VOID_INT2 = ESCALATE_ALL_VALUES(PARTICIPANT_NEW_CUST_COST, &
                                         PARTICIPANT_NEW_COST_ESCAL, &
                                         MAX_DSM_FINANCIAL_RECORDS,DESC)
!
      VOID_INT2 = ESCALATE_ALL_VALUES(UTIL_NON_ELEC_CUST_COST, &
                                         UTIL_NON_ELEC_COST_ESCAL, &
                                         MAX_DSM_FINANCIAL_RECORDS,DESC)
      VOID_INT2 = ESCALATE_ALL_VALUES(UTIL_NON_ELEC_NEW_CUST_COST, &
                                         UTIL_NON_ELEC_NEW_COST_ESCAL, &
                                         MAX_DSM_FINANCIAL_RECORDS,DESC)
!
      VOID_INT2 = ESCALATE_ALL_VALUES(THIRD_PARTY_CUST_COST, &
                                         THIRD_PARTY_COST_ESCAL, &
                                         MAX_DSM_FINANCIAL_RECORDS,DESC)
      VOID_INT2 = ESCALATE_ALL_VALUES(THIRD_PARTY_NEW_CUST_COST, &
                                         THIRD_PARTY_NEW_COST_ESCAL, &
                                         MAX_DSM_FINANCIAL_RECORDS,DESC)
!
      VOID_INT2 = ESCALATE_ALL_VALUES(OTH_PARTICIPANT_CUST_COST, &
                                         OTH_PARTICIPANT_COST_ESCAL, &
                                         MAX_DSM_FINANCIAL_RECORDS,DESC)
      VOID_INT2 = ESCALATE_ALL_VALUES(OTH_PARTICIPANT_NEW_CUST_COST, &
                                       OTH_PARTICIPANT_NEW_COST_ESCAL, &
                                         MAX_DSM_FINANCIAL_RECORDS,DESC)
!
      DO DSM_ACCTS = 1, MAX_DSM_FINANCIAL_RECORDS
         IF(.NOT. DSM_FINANCIAL_ACTIVE(DSM_ACCTS)) CYCLE  ! GOTO 100
!
!  SET VALUES TO ZERO
!
         TOTAL_DSM_EXPENSES = 0.
         TOTAL_REBATE_EXPENSE = 0.
         DSM_PURCHASE_POWER = 0.
         DSM_SALES_REVENUE = 0.
         TOTAL_DSM_CAPITAL = 0.
         TOTAL_PROGRAM_PARTICIPANT_COSTS = 0.
         BASE_YEAR_PROG_EXPENSES = 0.
         BASE_YEAR_CUST_EXPENSES = 0.
         BASE_YEAR_PROG_CAPITAL = 0.
         BASE_YEAR_CUST_CAPITAL = 0.
         DEVICE_NUM = DEVICE_APPLIED_TO(DSM_ACCTS)
         CALL GET_DEVICE_APPLICATION(DEVICE_NUM,ACCEPTANCE_NUM)
         IF(ACCEPTANCE_NUM == 0) THEN
            WRITE(4,*) 'In year',get_BASE_YEAR()+globecom_year,' for end point', &
                                                          gc_end_point
            WRITE(4,*) 'there was no corresponding record in the '
            WRITE(4,*) 'Acceptance file for device',DEVICE_NUM
            WRITE(4,*) 'specified in record',DSM_ACCTS,' of the'
            WRITE(4,*) 'Load Management Financial File.'
            WRITE(4,*)
            CYCLE
         ENDIF
!          FREE_FACTOR = (1.-FREE_DRIVERS(ACCEPTANCE_NUM))/
!      +                          (1.-FREE_RIDERS(ACCEPTANCE_NUM))
         IF(FREE_RIDERS(ACCEPTANCE_NUM) >= 0.) THEN
            LOCAL_FREE_RIDERS = FREE_RIDERS(ACCEPTANCE_NUM)/100.
         ELSE
            LOCAL_FREE_RIDERS = GET_VAR(FREE_RIDERS(ACCEPTANCE_NUM), &
                                                  DSM_YEAR,"RIDER")/100.
         ENDIF
         IF(FREE_DRIVERS(ACCEPTANCE_NUM) >= 0.) THEN
            LOCAL_FREE_DRIVERS = FREE_DRIVERS(ACCEPTANCE_NUM)/100.
         ELSE
            LOCAL_FREE_DRIVERS = GET_VAR(FREE_DRIVERS(ACCEPTANCE_NUM), &
                                                 DSM_YEAR,"DRIVER")/100.
         ENDIF
         FREE_FACTOR = &
                   (1. - LOCAL_FREE_RIDERS  + LOCAL_FREE_DRIVERS)
         IF(DSM_FINANCIAL_REPORT_SWITCH .AND. FIRST_DEVICE) THEN
!
            FIRST_DEVICE = .FALSE. ! USED FOR REPORT HEADER
!
         ENDIF
!
! IF THE YEAR IS EQUAL TO OR GREATER THAN BEGIN YEAR CALCULATE THE COSTS
!

            BEGIN_APP_YEAR = DSM_START_YEAR(ACCEPTANCE_NUM) - get_BASE_YEAR()
            IF(DSM_YEAR < BEGIN_APP_YEAR) CYCLE
!
!
               APP_YEAR = DSM_YEAR - MAX(BEGIN_APP_YEAR-1,0)
               IF(BEGIN_APP_YEAR == DSM_YEAR) THEN
                  BASE_YEAR_PROG_EXPENSES = BASE_PROG_EXP(DSM_ACCTS)
                  BASE_YEAR_CUST_EXPENSES = &
                           BASE_CUST_EXP(DSM_ACCTS) * FREE_FACTOR * &
                                   LM_CUSTOMERS(APP_YEAR,ACCEPTANCE_NUM)
                  BASE_YEAR_EXPENSES = &
                           BASE_YEAR_PROG_EXPENSES + &
                                       BASE_YEAR_CUST_EXPENSES
!
                  BASE_YEAR_PROG_CAPITAL = BASE_PROG_CAP(DSM_ACCTS)
                  BASE_YEAR_CUST_CAPITAL = &
                           BASE_CUST_CAP(DSM_ACCTS) * FREE_FACTOR * &
                                   LM_CUSTOMERS(APP_YEAR,ACCEPTANCE_NUM)
                  BASE_YEAR_CAPITAL = BASE_YEAR_PROG_CAPITAL + &
                                                  BASE_YEAR_CUST_CAPITAL
               ELSE
                  BASE_YEAR_EXPENSES = 0.
                  BASE_YEAR_CAPITAL = 0.
               ENDIF
!
               PROGRAM_EXPENSES = ONGO_PROG_EXP(DSM_ACCTS)
!
               CUSTOMER_EXPENSES=ONGO_CUST_EXP(DSM_ACCTS) * &
                                   LM_CUSTOMERS(APP_YEAR,ACCEPTANCE_NUM)
!
               NEW_CUSTOMER_EXPENSES = ONGO_NEW_CUST_EXP(DSM_ACCTS) * &
                             NEW_LM_CUSTOMERS(APP_YEAR,ACCEPTANCE_NUM)
!
               KWH_EXPENSES = ONGO_KWH_EXP(DSM_ACCTS) * &
                                   DSM_DEVICE_ENERGY(ACCEPTANCE_NUM)
!
               KW_EXPENSES = ONGO_KW_EXP(DSM_ACCTS) * &
                                DSM_DEVICE_PEAK(DSM_YEAR,ACCEPTANCE_NUM)
!
               REBATE_CUST_EXPENSE = REBATE_CUST_EXP(DSM_ACCTS) * &
                                   LM_CUSTOMERS(APP_YEAR,ACCEPTANCE_NUM)
!
             REBATE_NEW_CUST_EXPENSE = REBATE_NEW_CUST_EXP(DSM_ACCTS)* &
                               NEW_LM_CUSTOMERS(APP_YEAR,ACCEPTANCE_NUM)
!
               IF(DSM_ENERGY_CLASSIFICATION(ACCEPTANCE_NUM) == 'P') THEN
                  DSM_PURCHASE_POWER = PROGRAM_EXPENSES + &
                                       CUSTOMER_EXPENSES + &
                                       NEW_CUSTOMER_EXPENSES + &
                                       KWH_EXPENSES + &
                                       KW_EXPENSES + &
                                       BASE_YEAR_EXPENSES
                TOTAL_DSM_PURCHASE_POWER = TOTAL_DSM_PURCHASE_POWER + &
                                                      DSM_PURCHASE_POWER
               ELSEIF( &
                     DSM_ENERGY_CLASSIFICATION(ACCEPTANCE_NUM)=='O')THEN
                  DSM_SALES_REVENUE = PROGRAM_EXPENSES + &
                                      CUSTOMER_EXPENSES + &
                                      NEW_CUSTOMER_EXPENSES + &
                                      KWH_EXPENSES + &
                                      KW_EXPENSES + &
                                      BASE_YEAR_EXPENSES
                  TOTAL_DSM_SALES_REVENUE = TOTAL_DSM_SALES_REVENUE + &
                                                       DSM_SALES_REVENUE
               ELSE
                  TOTAL_DSM_EXPENSES = PROGRAM_EXPENSES + &
                                       CUSTOMER_EXPENSES + &
                                       NEW_CUSTOMER_EXPENSES + &
                                       KWH_EXPENSES + &
                                       KW_EXPENSES + &
                                       BASE_YEAR_EXPENSES
!
                  TOTAL_REBATE_EXPENSE = REBATE_CUST_EXPENSE + &
                                         REBATE_NEW_CUST_EXPENSE
               ENDIF
                  IF(COLLECT(DSM_ACCTS)=="A") THEN
                   DSM_EXPENSE_COLECTION(1)=DSM_EXPENSE_COLECTION(1) + &
                                              TOTAL_DSM_EXPENSES
                   DSM_REBATE_COLECTION(1) = DSM_REBATE_COLECTION(1) + &
                                               TOTAL_REBATE_EXPENSE
                     TOTAL_DSM_ADJ_CLAUSE_COLLECTONS = &
                                     TOTAL_DSM_ADJ_CLAUSE_COLLECTONS + &
                                       TOTAL_DSM_EXPENSES + &
                                       TOTAL_REBATE_EXPENSE + &
                                       DSM_PURCHASE_POWER - &
                                       DSM_SALES_REVENUE
                  ELSE IF(COLLECT(DSM_ACCTS)=="N") THEN
!  NOT COLLECTED FOR PRICE DRIVER
                   DSM_EXPENSE_COLECTION(3)=DSM_EXPENSE_COLECTION(3) + &
                                              TOTAL_DSM_EXPENSES
                   DSM_REBATE_COLECTION(3) = DSM_REBATE_COLECTION(3) + &
                                               TOTAL_REBATE_EXPENSE
                     TOTAL_DSM_BTL_EXPENSES = TOTAL_DSM_BTL_EXPENSES + &
                                              TOTAL_DSM_EXPENSES + &
                                              TOTAL_REBATE_EXPENSE + &
                                              DSM_PURCHASE_POWER
                     TOTAL_DSM_BTL_INCOME = TOTAL_DSM_BTL_INCOME + &
                                            DSM_SALES_REVENUE
                  ELSE
!  COLLECTED IN BASE RATES
                   DSM_EXPENSE_COLECTION(2)=DSM_EXPENSE_COLECTION(2) + &
                                              TOTAL_DSM_EXPENSES
                   DSM_REBATE_COLECTION(2) = DSM_REBATE_COLECTION(2) + &
                                               TOTAL_REBATE_EXPENSE
                  ENDIF
!
!            CALCULATE CAPITAL COSTS
!
               PROGRAM_CAPITAL = ONGO_PROG_CAP(DSM_ACCTS)
!
               CUSTOMER_CAPITAL=ONGO_CUST_CAP(DSM_ACCTS) * &
                                   LM_CUSTOMERS(APP_YEAR,ACCEPTANCE_NUM)
!
               NEW_CUSTOMER_CAPITAL = ONGO_NEW_CUST_CAP(DSM_ACCTS) * &
                               NEW_LM_CUSTOMERS(APP_YEAR,ACCEPTANCE_NUM)
!
               KWH_CAPITAL = ONGO_KWH_CAP(DSM_ACCTS) * &
                               DSM_DEVICE_ENERGY(ACCEPTANCE_NUM)
!
               KW_CAPITAL = ONGO_KW_CAP(DSM_ACCTS) * &
                                DSM_DEVICE_PEAK(DSM_YEAR,ACCEPTANCE_NUM)
!
               REBATE_CUST_CAPITAL = REBATE_CUST_CAP(DSM_ACCTS) * &
                                   LM_CUSTOMERS(APP_YEAR,ACCEPTANCE_NUM)
!
             REBATE_NEW_CUST_CAPITAL = REBATE_NEW_CUST_CAP(DSM_ACCTS)* &
                               NEW_LM_CUSTOMERS(APP_YEAR,ACCEPTANCE_NUM)
!
               TOTAL_DSM_CAPITAL = PROGRAM_CAPITAL + &
                                   CUSTOMER_CAPITAL + &
                                   NEW_CUSTOMER_CAPITAL + &
                                   KWH_CAPITAL + &
                                   KW_CAPITAL + &
                                   BASE_YEAR_CAPITAL + &
                                   REBATE_CUST_CAPITAL + &
                                   REBATE_NEW_CUST_CAPITAL
!
!  CALCULATES TOTAL NON-UTILITY COSTS
!
               PARTICIPANT_CUSTOMER_COSTS = &
                             PARTICIPANT_CUST_COST(DSM_ACCTS) * &
                                  LM_CUSTOMERS(APP_YEAR,ACCEPTANCE_NUM)
               PARTICIPANT_NEW_CUSTOMER_COSTS = &
                          PARTICIPANT_NEW_CUST_COST(DSM_ACCTS) * &
                               NEW_LM_CUSTOMERS(APP_YEAR,ACCEPTANCE_NUM)
               TOTAL_PROGRAM_PARTICIPANT_COSTS = &
                                    PARTICIPANT_CUSTOMER_COSTS + &
                                         PARTICIPANT_NEW_CUSTOMER_COSTS
               TOTAL_PARTICIPANT_COSTS = TOTAL_PARTICIPANT_COSTS + &
                                         TOTAL_PROGRAM_PARTICIPANT_COSTS
               PARTICIPANT_COSTS_BY(ACCEPTANCE_NUM) = &
                                PARTICIPANT_COSTS_BY(ACCEPTANCE_NUM) + &
                                         TOTAL_PROGRAM_PARTICIPANT_COSTS
!
               PROGRAM_UTIL_NON_ELEC_COSTS = &
                            UTIL_NON_ELEC_CUST_COST(DSM_ACCTS) * &
                               LM_CUSTOMERS(APP_YEAR,ACCEPTANCE_NUM) + &
                            UTIL_NON_ELEC_NEW_CUST_COST(DSM_ACCTS) * &
                               NEW_LM_CUSTOMERS(APP_YEAR,ACCEPTANCE_NUM)
               TOTAL_UTIL_NON_ELEC_COSTS = TOTAL_UTIL_NON_ELEC_COSTS + &
                                             PROGRAM_UTIL_NON_ELEC_COSTS
!
               PROGRAM_THIRD_PARTY_COSTS = &
                            THIRD_PARTY_CUST_COST(DSM_ACCTS) * &
                               LM_CUSTOMERS(APP_YEAR,ACCEPTANCE_NUM) + &
                            THIRD_PARTY_NEW_CUST_COST(DSM_ACCTS) * &
                               NEW_LM_CUSTOMERS(APP_YEAR,ACCEPTANCE_NUM)
               TOTAL_THIRD_PARTY_COSTS = TOTAL_THIRD_PARTY_COSTS + &
                                               PROGRAM_THIRD_PARTY_COSTS
!
               PROGRAM_OTH_PARTICIPANT_COSTS = &
                         OTH_PARTICIPANT_CUST_COST(DSM_ACCTS) * &
                               LM_CUSTOMERS(APP_YEAR,ACCEPTANCE_NUM) + &
                         OTH_PARTICIPANT_NEW_CUST_COST(DSM_ACCTS) * &
                               NEW_LM_CUSTOMERS(APP_YEAR,ACCEPTANCE_NUM)
             TOTAL_OTH_PARTICIPANT_COSTS=TOTAL_OTH_PARTICIPANT_COSTS + &
                                           PROGRAM_OTH_PARTICIPANT_COSTS
!
             IF(DSM_FINANCIAL_REPORT() .AND. .NOT. TESTING_PLAN .AND. &
                                         LF_DELETE(DSM_ACCTS) <= 5) THEN
!
!               BEGINING OF THE DSM FINANCIAL REPORT
!
                  IF(LM_COST_REPORT_NOT_OPEN) THEN
                     LM_COST_NO = LM_COST_REPORT_HEADER(LM_COST_REC)
                     LM_COST_REPORT_NOT_OPEN = .FALSE.
                  ENDIF
                  WRITE(LM_COST_NO,REC=LM_COST_REC) PRT_ENDPOINT(), &
                     FLOAT(globecom_year+get_BASE_YEAR()),DESC(DSM_ACCTS), &
                     'Escalation Cost/Unit ($/Unit) ', &
                     BASE_PROG_EXP(DSM_ACCTS), &
                     ONGO_PROG_EXP(DSM_ACCTS), &
                     1000000.*BASE_CUST_EXP(DSM_ACCTS), &
                     1000000.*ONGO_CUST_EXP(DSM_ACCTS), &
                     1000000.*ONGO_NEW_CUST_EXP(DSM_ACCTS), &
                     1000000.*ONGO_KWH_EXP(DSM_ACCTS), &
                     1000000.*ONGO_KW_EXP(DSM_ACCTS), &
                     0.0, &
                     1000000.*REBATE_CUST_EXP(DSM_ACCTS), &
                     1000000.*REBATE_NEW_CUST_EXP(DSM_ACCTS), &
                     0.0, &
                     BASE_PROG_CAP(DSM_ACCTS), &
                     ONGO_PROG_CAP(DSM_ACCTS), &
                     1000000.*BASE_CUST_CAP(DSM_ACCTS), &
                     1000000.*ONGO_CUST_CAP(DSM_ACCTS), &
                     1000000.*ONGO_NEW_CUST_CAP(DSM_ACCTS), &
                     1000000.*ONGO_KWH_CAP(DSM_ACCTS), &
                     1000000.*ONGO_KW_CAP(DSM_ACCTS), &
                     1000000.*REBATE_CUST_CAP(DSM_ACCTS), &
                     1000000.*REBATE_NEW_CUST_CAP(DSM_ACCTS), &
                     0.0, &
                     1000000.*PARTICIPANT_CUST_COST(DSM_ACCTS), &
                     1000000.*PARTICIPANT_NEW_CUST_COST(DSM_ACCTS), &
                     0.0
                  LM_COST_REC = LM_COST_REC + 1
                  WRITE(LM_COST_NO,REC=LM_COST_REC) PRT_ENDPOINT(), &
                     FLOAT(globecom_year+get_BASE_YEAR()),DESC(DSM_ACCTS), &
                     'Units                         ', &
                     1.0, &
                     1.0, &
                     LM_CUSTOMERS(APP_YEAR,ACCEPTANCE_NUM), &
                     LM_CUSTOMERS(APP_YEAR,ACCEPTANCE_NUM), &
                     NEW_LM_CUSTOMERS(APP_YEAR,ACCEPTANCE_NUM), &
                     DSM_DEVICE_ENERGY(ACCEPTANCE_NUM), &
                     DSM_DEVICE_PEAK(DSM_YEAR,ACCEPTANCE_NUM), &
                     0.0, &
                     LM_CUSTOMERS(APP_YEAR,ACCEPTANCE_NUM), &
                     NEW_LM_CUSTOMERS(APP_YEAR,ACCEPTANCE_NUM), &
                     0.0, &
                     1.0, &
                     1.0, &
                     LM_CUSTOMERS(APP_YEAR,ACCEPTANCE_NUM), &
                     LM_CUSTOMERS(APP_YEAR,ACCEPTANCE_NUM), &
                     NEW_LM_CUSTOMERS(APP_YEAR,ACCEPTANCE_NUM), &
                     DSM_DEVICE_ENERGY(ACCEPTANCE_NUM), &
                     DSM_DEVICE_PEAK(DSM_YEAR,ACCEPTANCE_NUM), &
                     LM_CUSTOMERS(APP_YEAR,ACCEPTANCE_NUM), &
                     NEW_LM_CUSTOMERS(APP_YEAR,ACCEPTANCE_NUM), &
                     0.0, &
                     LM_CUSTOMERS(APP_YEAR,ACCEPTANCE_NUM), &
                     NEW_LM_CUSTOMERS(APP_YEAR,ACCEPTANCE_NUM), &
                     0.0
                  LM_COST_REC = LM_COST_REC + 1
                  WRITE(LM_COST_NO,REC=LM_COST_REC) PRT_ENDPOINT(), &
                     FLOAT(globecom_year+get_BASE_YEAR()),DESC(DSM_ACCTS), &
                     'Total Utility Cost ($ mm)     ', &
                     BASE_YEAR_PROG_EXPENSES, &
                     PROGRAM_EXPENSES, &
                     BASE_YEAR_CUST_EXPENSES, &
                     CUSTOMER_EXPENSES, &
                     NEW_CUSTOMER_EXPENSES, &
                     KWH_EXPENSES, &
                     KW_EXPENSES, &
                     BASE_YEAR_PROG_EXPENSES+ &
                     PROGRAM_EXPENSES+ &
                     BASE_YEAR_CUST_EXPENSES+ &
                     CUSTOMER_EXPENSES+ &
                     NEW_CUSTOMER_EXPENSES+ &
                     KWH_EXPENSES+ &
                     KW_EXPENSES, &
                     REBATE_CUST_EXPENSE, &
                     REBATE_NEW_CUST_EXPENSE, &
                     REBATE_CUST_EXPENSE+ &
                     REBATE_NEW_CUST_EXPENSE, &
                     BASE_YEAR_PROG_CAPITAL, &
                     PROGRAM_CAPITAL, &
                     BASE_YEAR_CUST_CAPITAL, &
                     CUSTOMER_CAPITAL, &
                     NEW_CUSTOMER_CAPITAL, &
                     KWH_CAPITAL, &
                     KW_CAPITAL, &
                     REBATE_CUST_CAPITAL, &
                     REBATE_NEW_CUST_CAPITAL, &
                     BASE_YEAR_PROG_CAPITAL+ &
                     PROGRAM_CAPITAL+ &
                     BASE_YEAR_CUST_CAPITAL+ &
                     CUSTOMER_CAPITAL+ &
                     NEW_CUSTOMER_CAPITAL+ &
                     KWH_CAPITAL+ &
                     KW_CAPITAL+ &
                     REBATE_CUST_CAPITAL+ &
                     REBATE_NEW_CUST_CAPITAL, &
                     PARTICIPANT_CUSTOMER_COSTS, &
                     PARTICIPANT_NEW_CUSTOMER_COSTS, &
                     TOTAL_PROGRAM_PARTICIPANT_COSTS
                  LM_COST_REC = LM_COST_REC + 1
                  WRITE(LM_COST_NO,REC=LM_COST_REC) PRT_ENDPOINT(), &
                     FLOAT(globecom_year+get_BASE_YEAR()),DESC(DSM_ACCTS), &
                     'Free Rider Cost Impact ($ mm) ', &
                     0.0, &
                     0.0, &
                     BASE_YEAR_CUST_EXPENSES*LOCAL_FREE_RIDERS, &
                     CUSTOMER_EXPENSES*LOCAL_FREE_RIDERS, &
                     NEW_CUSTOMER_EXPENSES*LOCAL_FREE_RIDERS, &
                     KWH_EXPENSES*LOCAL_FREE_RIDERS, &
                     KW_EXPENSES*LOCAL_FREE_RIDERS, &
                     BASE_YEAR_CUST_EXPENSES*LOCAL_FREE_RIDERS+ &
                     CUSTOMER_EXPENSES*LOCAL_FREE_RIDERS+ &
                     NEW_CUSTOMER_EXPENSES*LOCAL_FREE_RIDERS+ &
                     KWH_EXPENSES*LOCAL_FREE_RIDERS+ &
                     KW_EXPENSES*LOCAL_FREE_RIDERS, &
                     REBATE_CUST_EXPENSE*LOCAL_FREE_RIDERS, &
                     REBATE_NEW_CUST_EXPENSE*LOCAL_FREE_RIDERS, &
                     REBATE_CUST_EXPENSE*LOCAL_FREE_RIDERS+ &
                     REBATE_NEW_CUST_EXPENSE*LOCAL_FREE_RIDERS, &
                     0.0, &
                     0.0, &
                     BASE_YEAR_CUST_CAPITAL*LOCAL_FREE_RIDERS, &
                     CUSTOMER_CAPITAL*LOCAL_FREE_RIDERS, &
                     NEW_CUSTOMER_CAPITAL*LOCAL_FREE_RIDERS, &
                     KWH_CAPITAL*LOCAL_FREE_RIDERS, &
                     KW_CAPITAL*LOCAL_FREE_RIDERS, &
                     REBATE_CUST_CAPITAL*LOCAL_FREE_RIDERS, &
                     REBATE_NEW_CUST_CAPITAL*LOCAL_FREE_RIDERS, &
                     BASE_YEAR_CUST_CAPITAL*LOCAL_FREE_RIDERS+ &
                     CUSTOMER_CAPITAL*LOCAL_FREE_RIDERS+ &
                     NEW_CUSTOMER_CAPITAL*LOCAL_FREE_RIDERS+ &
                     KWH_CAPITAL*LOCAL_FREE_RIDERS+ &
                     KW_CAPITAL*LOCAL_FREE_RIDERS+ &
                     REBATE_CUST_CAPITAL*LOCAL_FREE_RIDERS+ &
                     REBATE_NEW_CUST_CAPITAL*LOCAL_FREE_RIDERS, &
                     PARTICIPANT_CUSTOMER_COSTS*LOCAL_FREE_RIDERS, &
                     PARTICIPANT_NEW_CUSTOMER_COSTS*LOCAL_FREE_RIDERS, &
                     (PARTICIPANT_CUSTOMER_COSTS+ &
                     PARTICIPANT_NEW_CUSTOMER_COSTS)*LOCAL_FREE_RIDERS
                  LM_COST_REC = LM_COST_REC + 1
                  WRITE(LM_COST_NO,REC=LM_COST_REC) PRT_ENDPOINT(), &
                     FLOAT(globecom_year+get_BASE_YEAR()),DESC(DSM_ACCTS), &
                     'Free Driver Savings ($ mm)    ', &
                     0.0, &
                     0.0, &
                     BASE_YEAR_CUST_EXPENSES*LOCAL_FREE_DRIVERS, &
                     CUSTOMER_EXPENSES*LOCAL_FREE_DRIVERS, &
                     NEW_CUSTOMER_EXPENSES*LOCAL_FREE_DRIVERS, &
                     KWH_EXPENSES*LOCAL_FREE_DRIVERS, &
                     KW_EXPENSES*LOCAL_FREE_DRIVERS, &
                     BASE_YEAR_CUST_EXPENSES*LOCAL_FREE_DRIVERS+ &
                     CUSTOMER_EXPENSES*LOCAL_FREE_DRIVERS+ &
                     NEW_CUSTOMER_EXPENSES*LOCAL_FREE_DRIVERS+ &
                     KWH_EXPENSES*LOCAL_FREE_DRIVERS+ &
                     KW_EXPENSES*LOCAL_FREE_DRIVERS, &
                     REBATE_CUST_EXPENSE*LOCAL_FREE_DRIVERS, &
                     REBATE_NEW_CUST_EXPENSE*LOCAL_FREE_DRIVERS, &
                     REBATE_CUST_EXPENSE*LOCAL_FREE_DRIVERS+ &
                     REBATE_NEW_CUST_EXPENSE*LOCAL_FREE_DRIVERS, &
                     0.0, &
                     0.0, &
                     BASE_YEAR_CUST_CAPITAL*LOCAL_FREE_DRIVERS, &
                     CUSTOMER_CAPITAL*LOCAL_FREE_DRIVERS, &
                     NEW_CUSTOMER_CAPITAL*LOCAL_FREE_DRIVERS, &
                     KWH_CAPITAL*LOCAL_FREE_DRIVERS, &
                     KW_CAPITAL*LOCAL_FREE_DRIVERS, &
                     REBATE_CUST_CAPITAL*LOCAL_FREE_DRIVERS, &
                     REBATE_NEW_CUST_CAPITAL*LOCAL_FREE_DRIVERS, &
                     BASE_YEAR_CUST_CAPITAL*LOCAL_FREE_DRIVERS+ &
                     CUSTOMER_CAPITAL*LOCAL_FREE_DRIVERS+ &
                     NEW_CUSTOMER_CAPITAL*LOCAL_FREE_DRIVERS+ &
                     KWH_CAPITAL*LOCAL_FREE_DRIVERS+ &
                     KW_CAPITAL*LOCAL_FREE_DRIVERS+ &
                     REBATE_CUST_CAPITAL*LOCAL_FREE_DRIVERS+ &
                     REBATE_NEW_CUST_CAPITAL*LOCAL_FREE_DRIVERS, &
                     PARTICIPANT_CUSTOMER_COSTS*LOCAL_FREE_DRIVERS, &
                    PARTICIPANT_NEW_CUSTOMER_COSTS*LOCAL_FREE_DRIVERS, &
                     (PARTICIPANT_CUSTOMER_COSTS+ &
                     PARTICIPANT_NEW_CUSTOMER_COSTS)*LOCAL_FREE_DRIVERS
                  LM_COST_REC = LM_COST_REC + 1
               ENDIF
!
!  CALCULATES GPV ADDITIONS, ANNUAL BOOK DEP, CUMULATIVE BOOK DEP,
!   AFUDC, CASH INVESTMENT, NET DEFERRED DEBIT ADDITIONS, 
! ANNUAL AMORTIZATIONS,
!   TAX DEP ADDITIONS, DEFERRED TAXES BASIS
!
               IF(TOTAL_DSM_CAPITAL /= 0.) &
                               CALL DSM_CAPITAL_ITEMS(DSM_ACCTS, &
                                                      TOTAL_DSM_CAPITAL)
!
               IF(DSM_FINANCIAL_REPORT()) THEN
!
!
!
               ENDIF
!
!  ENDIF FOR  COST CALCULATIONS
!
!
!  ASSET CLASS ALLOCATIONS
!
         CALL DSM_ASSET_ALLOCATION(DSM_ACCTS,TOTAL_DSM_EXPENSES, &
                               TOTAL_REBATE_EXPENSE, &
                               DSM_PURCHASE_POWER, &
                               DSM_SALES_REVENUE, &
                               TOTAL_DSM_CAPITAL, &
                               TOTAL_PROGRAM_PARTICIPANT_COSTS, &
                               PROGRAM_UTIL_NON_ELEC_COSTS, &
                               PROGRAM_THIRD_PARTY_COSTS, &
                               PROGRAM_OTH_PARTICIPANT_COSTS, &
                             DSM_DEVICE_PEAK(DSM_YEAR,ACCEPTANCE_NUM), &
                               DSM_DEVICE_ENERGY(ACCEPTANCE_NUM))
      ENDDO
!
      CALL SUM_DSM_ASSET_CLASSES
      RETURN
! ********************************************************************
      ENTRY WRITE_DSM_RATE_REPORT(R_CLASS,CLASS_RATES, &
                  CLASS_COSTS_TOTAL,CLASS_COSTS_DEMAND, &
                  CLASS_COSTS_ENERGY,CLASS_COSTS_CUSTOMER, &
                  CLASS_PEAK,CLASS_MWH, &
                  CLASS_CUSTOMERS)
! ********************************************************************
!
         IF(.NOT. LMF_FILE_EXISTS(DSM_FINANCIAL_RECORDS) .OR. &
                                                   TESTING_PLAN) RETURN
         DSM_YEAR = MIN(globecom_year,get_globecom_study_period())
         DO APPLICATION = 1 , APPLICATION_NUM
            BEGIN_APP_YEAR = DSM_START_YEAR(APPLICATION) - get_BASE_YEAR()
            IF( APPLICATION_CLASS(APPLICATION) /= R_CLASS .OR. &
                                        BEGIN_APP_YEAR > DSM_YEAR) CYCLE
            APP_YEAR = DSM_YEAR - MAX(BEGIN_APP_YEAR-1,0)
            CUSCAL = LM_CUSTOMERS(APP_YEAR,APPLICATION)
            IF(FREE_RIDERS(APPLICATION) >= 0.) THEN
               LOCAL_FREE_RIDERS = FREE_RIDERS(APPLICATION)/100.
            ELSE
               LOCAL_FREE_RIDERS = GET_VAR(FREE_RIDERS(APPLICATION), &
                                                  DSM_YEAR,"RIDER")/100.
            ENDIF
            IF(FREE_DRIVERS(APPLICATION) >= 0.) THEN
               LOCAL_FREE_DRIVERS = FREE_DRIVERS(APPLICATION)/100.
            ELSE
               LOCAL_FREE_DRIVERS = GET_VAR(FREE_DRIVERS(APPLICATION), &
                                                 DSM_YEAR,"DRIVER")/100.
            ENDIF
            IF(DSM_RATE_REPORT_NOT_OPEN) THEN
               DSM_RATE_NO = DSM_RATE_REPORT_HEADER(DSM_RATE_REC)
               DSM_RATE_REPORT_NOT_OPEN = .FALSE.
            ENDIF
            IF(DSM_YEAR+get_BASE_YEAR() > DSM_START_YEAR(APPLICATION) .AND. &
                                                     DSM_YEAR > 1) THEN
               RETIRING_CUSTOMERS = &
                        LM_CUSTOMERS(APP_YEAR-1,APPLICATION) + &
                        NEW_LM_CUSTOMERS(APP_YEAR,APPLICATION) - CUSCAL
            ELSE
               RETIRING_CUSTOMERS = 0.0
            ENDIF
!
            ADJUSTED_DEVICE_ENERGY =DSM_DEVICE_ENERGY(APPLICATION) * &
               (1. - PROGRAM_LOSS_FACTOR(APPLICATION) )
            NON_PARTICIPANT_CUSTOMER_USAGE = &
               (CLASS_MWH + ADJUSTED_DEVICE_ENERGY)/ &
                                             MAX(1.,CLASS_CUSTOMERS)
            DSM_CUSTOMER_USAGE = ADJUSTED_DEVICE_ENERGY/MAX(1.,CUSCAL)
            PARTICIPANT_CUSTOMER_USAGE = &
                NON_PARTICIPANT_CUSTOMER_USAGE - DSM_CUSTOMER_USAGE
!
            PARTICIPANT_PENETRATION = CUSCAL/MAX(CLASS_CUSTOMERS,1.)
            PARTICIPANT_MWH = PARTICIPANT_CUSTOMER_USAGE * CUSCAL
            PARTICIPANT_REVENUES = PARTICIPANT_MWH * CLASS_RATES/ &
                                                           MILLION
            NON_PARTICIPANT_REVENUES = CLASS_COSTS_TOTAL - &
                                                PARTICIPANT_REVENUES
            NON_PARTICIPANTS = CLASS_CUSTOMERS - CUSCAL
            WRITE(DSM_RATE_NO,REC=DSM_RATE_REC) PRT_ENDPOINT(), &
               FLOAT(globecom_year+get_BASE_YEAR()),CLASS_NAME(R_CLASS), &
               DSM_DEVICE_NAME(APPLICATION),  &
               CLASS_COSTS_TOTAL*1000., &
               SNGL(CLASS_MWH), &
               CLASS_RATES, &
               CLASS_CUSTOMERS, &
               CLASS_COSTS_TOTAL*MILLION/MAX(CLASS_CUSTOMERS,1.), &
               1000*SNGL(CLASS_MWH)/MAX(CLASS_CUSTOMERS,1.),   &
               PARTICIPANT_REVENUES*1000., &
               ADJUSTED_DEVICE_ENERGY, &
               SNGL(PARTICIPANT_MWH), &
               CUSCAL, &
               PARTICIPANT_REVENUES*MILLION/MAX(CUSCAL,.00001), &
               PARTICIPANT_COSTS_BY(APPLICATION)*MILLION/ &
                                            MAX(CUSCAL,.00001), &
             (PARTICIPANT_COSTS_BY(APPLICATION)+PARTICIPANT_REVENUES) &
                                          *MILLION/MAX(CUSCAL,.00001), &
               SNGL(1000*PARTICIPANT_MWH/MAX(CUSCAL,.00001)),   &
               NON_PARTICIPANT_REVENUES*1000., &
               SNGL(CLASS_MWH-PARTICIPANT_MWH), &
               NON_PARTICIPANTS, &
               NON_PARTICIPANT_REVENUES*MILLION/NON_PARTICIPANTS, &
               SNGL(1000*(CLASS_MWH-PARTICIPANT_MWH))/ &
                                        MAX(NON_PARTICIPANTS,1.),   &
               NEW_LM_CUSTOMERS(APP_YEAR,APPLICATION), &
               RETIRING_CUSTOMERS,   &
               CUSCAL*LOCAL_FREE_RIDERS, &
               CUSCAL*LOCAL_FREE_DRIVERS, &
               CUSCAL*(1. - LOCAL_FREE_RIDERS + LOCAL_FREE_DRIVERS)
            DSM_RATE_REC = DSM_RATE_REC + 1
         ENDDO
      RETURN
 1002 FORMAT(T54,A)
!  REPORT DESCRIPTION AND COLUMN HEADINGS
 1003 FORMAT (/// &
      T10,A,T42,A/ &
      T10,A,T40,I3,T65,A,T95,I3// &
      T10,A,T55,A,T85,A,T105,A/ &
      T55,A,T105,A &
      //, &
& !  FORMAT FOR THE EXPENSES SECTION
      T10,A, &
      2(/T10,A,T55,F12.3,T105,F10.3), &
      3(/T10,A,T55,F12.3,T80,I12,T105,F10.3), &
      /T10,A,T55,F12.3,T80,I12,T105,F10.3, &
      /T10,A,T55,F12.3,T80,I12,T105,F10.3, &
        /T10,A,T105,F10.3, &
      2(/T10,A,T55,F12.3,T80,I12,T105,F10.3), &
        /T10,A,T105,F10.3 &
      // &
 & !  FORMAT FOR THE CAPITAL COST SECTION &
      T10,A, &
      2(/T10,A,T55,F12.3,T105,F10.3), &
      3(/T10,A,T55,F12.3,T80,I12,T105,F10.3), &
      /T10,A,T55,F12.3,T80,I12,T105,F10.3, &
      /T10,A,T55,F12.3,T80,I12,T105,F10.3, &
      2(/T10,A,T55,F12.3,T80,I12,T105,F10.3), &
        /T10,A,T105,F10.3 &
       // &
& !  FORMAT FOR THE NON-UTILITY COST SECTION &
      T10,A, &
      2(/T10,A,T55,F12.3,T80,I12,T105,F10.3) &
        /T10,A,T105,F10.3)
!
      END SUBROUTINE MDSLDCAP
! ********************************************************************
! *                                                                    *
! *                      DSM_CAPITAL_ITEMS                             *
! *                                                                    *
! *         COPYRIGHT (C) 1990   M.S. GERBER & ASSOCIATES, INC         *
! *                      ALL RIGHTS RESERVED                           *
! *                                                                    *
! ********************************************************************
!                                                                      *
!     PURPOSE:                                                         *
!        DSM_CAPITAL_ITEMS CALCULATES GPV ADDITIONS, ANNUAL BOOK DEP,  *
!        CUMULATIVE BOOK DEP, AFUDC, CASH INVESTMENT,                  *
!        NET DEFERRED DEBIT ADDITIONS, ANNUAL AMORTIZATIONS,           *
!        TAX DEP ADDITIONS, DEFERRED TAXES BASIS                       *
!                                                                      *
! ********************************************************************
!
      SUBROUTINE DSM_CAPITAL_ITEMS(DSM_ACCTS,TOTAL_DSM_CAPITAL)
      use dsf
      use dsf2
      use dsfc
      use dsmfrcom
      use SpinDriftLib
             use prod_arrays_dimensions
      use sizecom
      use globecom
      use enrg_helper
      INTEGER*2 DSM_ACCTS,YEAR_PLUS_ONE,MID_YEAR
      CHARACTER*4 DB_METH
      REAL TOTAL_DSM_CAPITAL,AFUDC,DB_RATE_150
      REAL FA_AFUDC_RATE
      REAL TAX_DEP(MAX_FINANCIAL_SIMULATION_YEARS), &
           TAX_SL_DEP(MAX_FINANCIAL_SIMULATION_YEARS), &
           ADR_TAX_DEP(MAX_FINANCIAL_SIMULATION_YEARS), &
           BOOK_DEP(MAX_FINANCIAL_SIMULATION_YEARS), &
           PROPERTY_ESCALATION
      REAL :: TAX_VALUE,REG_ALLOCATOR
      REAL :: ESCALATOR,TOTAL_BOOK_DEP
!
!  OLD VARIABLES
!
      INTEGER*2 YR

!
!  DSM FINANCIAL EXTERNAL VARIABLES
!
!
!  FINANCIAL RESULTS COMMON BLOCK

      DB_METH = 'DB'
      MID_YEAR = 7
      DB_RATE_150 = 150.
      YEAR_PLUS_ONE = globecom_year + 1
!
!  CALCULATE AFUDC IF THE UNIT IS TO BE IN GPV
!
      IF(REG_TREAT(DSM_ACCTS) == 'D') THEN
         AFUDC = 0.
      ELSE
         AFUDC  = FA_AFUDC_RATE() * CONSTRUCTION_PERIOD(DSM_ACCTS) * &
                                                  TOTAL_DSM_CAPITAL/2.
      ENDIF
!
!  CALCULATE BOOK DEPRECIATION ON GPV WITH AFUDC
!
      CALL DSM_BOOK_DEP_CAL(AFUDC+TOTAL_DSM_CAPITAL,BOOK_DEP, &
                            BOKLF(DSM_ACCTS),SERVICEMO(DSM_ACCTS))
!
!  SUM THE RESULTS INTO THE DSM FINANCIAL RESULTS ARRAYS AND VALUES
!
      DSM_CASH = DSM_CASH + TOTAL_DSM_CAPITAL
      REG_ALLOCATOR = REGULATORY_ALLOCATOR(DSM_ACCTS)
      IF(REG_TREAT(DSM_ACCTS) == 'D') THEN
         DSM_DEFERRED_DEBIT = DSM_DEFERRED_DEBIT + TOTAL_DSM_CAPITAL
         DSM_TAX_EXPENSE = DSM_TAX_EXPENSE + TOTAL_DSM_CAPITAL
         RB_DSM_TAX_EXPENSE = RB_DSM_TAX_EXPENSE + REG_ALLOCATOR * &
                                                       TOTAL_DSM_CAPITAL
         DSM_REGULATED_DEFERRED_DEBIT = DSM_REGULATED_DEFERRED_DEBIT + &
                                     TOTAL_DSM_CAPITAL * REG_ALLOCATOR
         DSM_DEFERRED_TAX_BASIS(YEAR_PLUS_ONE) = &
                               DSM_DEFERRED_TAX_BASIS(YEAR_PLUS_ONE) + &
                                 TOTAL_DSM_CAPITAL * TAXEXP(DSM_ACCTS)
         RB_DSM_DEFERRED_TAX_BASIS(YEAR_PLUS_ONE) = &
                            RB_DSM_DEFERRED_TAX_BASIS(YEAR_PLUS_ONE) + &
                   TOTAL_DSM_CAPITAL * TAXEXP(DSM_ACCTS) * REG_ALLOCATOR
         DO YR = YEAR_PLUS_ONE, MAX_FINANCIAL_SIMULATION_YEARS
            DSM_AMORT(YR) = DSM_AMORT(YR) + BOOK_DEP(YR)
            RB_DSM_AMORT(YR) = RB_DSM_AMORT(YR) + BOOK_DEP(YR) * &
                                         REG_ALLOCATOR
            DSM_DEFERRED_TAX_BASIS(YR) = DSM_DEFERRED_TAX_BASIS(YR) - &
                                        BOOK_DEP(YR) * TAXEXP(DSM_ACCTS)
            RB_DSM_DEFERRED_TAX_BASIS(YR) = &
                                       RB_DSM_DEFERRED_TAX_BASIS(YR) - &
                        BOOK_DEP(YR) * TAXEXP(DSM_ACCTS) * REG_ALLOCATOR
         ENDDO
         CALL DSM_CLASS_DEBITS(DSM_ACCTS,TOTAL_DSM_CAPITAL,BOOK_DEP)
      ELSE
         DSM_GPV = DSM_GPV + TOTAL_DSM_CAPITAL
         DSM_REGULATED_NPV = DSM_REGULATED_NPV + &
                             (AFUDC + TOTAL_DSM_CAPITAL) * REG_ALLOCATOR
         DSM_AFUDC = DSM_AFUDC + AFUDC
         RB_DSM_AFUDC = RB_DSM_AFUDC + AFUDC * REG_ALLOCATOR
         TAX_VALUE = TOTAL_DSM_CAPITAL * (1. - TAXEXP(DSM_ACCTS))
         DSM_TAX_EXPENSE = DSM_TAX_EXPENSE + TOTAL_DSM_CAPITAL-TAX_VALUE
         RB_DSM_TAX_EXPENSE = RB_DSM_TAX_EXPENSE + REG_ALLOCATOR * &
                                           (TOTAL_DSM_CAPITAL-TAX_VALUE)
         CALL DSM_TAX_DEP_CAL(TAXLF(DSM_ACCTS),DEPMET(DSM_ACCTS), &
                                    DBRATE(DSM_ACCTS),TAX_VALUE,TAX_DEP)
         DSM_DEFERRED_TAX_BASIS(YEAR_PLUS_ONE) = &
                               DSM_DEFERRED_TAX_BASIS(YEAR_PLUS_ONE) + &
                                           TOTAL_DSM_CAPITAL - TAX_VALUE
         RB_DSM_DEFERRED_TAX_BASIS(YEAR_PLUS_ONE) = &
                            RB_DSM_DEFERRED_TAX_BASIS(YEAR_PLUS_ONE) + &
                                  (TOTAL_DSM_CAPITAL - TAX_VALUE) * &
                                         REG_ALLOCATOR
         CALL DSM_BOOK_DEP_CAL(TOTAL_DSM_CAPITAL, &
                                   TAX_SL_DEP,BOKLF(DSM_ACCTS),MID_YEAR)
         IF(INDEX(DEPMET(DSM_ACCTS),'SL') == 0) &
            CALL DSM_TAX_DEP_CAL(ADRLIFE(DSM_ACCTS),DB_METH, &
                                      DB_RATE_150,TAX_VALUE,ADR_TAX_DEP)
         DSM_GPV_PROPERTY_TAX(globecom_year) = &
            DSM_GPV_PROPERTY_TAX(globecom_YEAR) + &
            TOTAL_DSM_CAPITAL
            
         DSM_NPV_PROPERTY_TAX(globecom_year) = DSM_NPV_PROPERTY_TAX(globecom_year)+ &
                                      TOTAL_DSM_CAPITAL - BOOK_DEP(globecom_year)
         ESCALATOR = 1.
         TOTAL_BOOK_DEP = 0.
         DO YR = YEAR_PLUS_ONE, MAX_FINANCIAL_SIMULATION_YEARS
            ESCALATOR = ESCALATOR * (1. + PROPERTY_ESCALATION(YR))
            DSM_GPV_PROPERTY_TAX(YR) = DSM_GPV_PROPERTY_TAX(YR) + &
                                           TOTAL_DSM_CAPITAL * ESCALATOR
            TOTAL_BOOK_DEP = TOTAL_BOOK_DEP + BOOK_DEP(YR)
            DSM_NPV_PROPERTY_TAX(YR) = DSM_NPV_PROPERTY_TAX(YR) + &
                          (TOTAL_DSM_CAPITAL-TOTAL_BOOK_DEP) * ESCALATOR
            DSM_BOOK_DEP(YR) = DSM_BOOK_DEP(YR) + BOOK_DEP(YR)
            DSM_REGULATED_BOOK_DEP(YR) = DSM_REGULATED_BOOK_DEP(YR) + &
                                            BOOK_DEP(YR) * REG_ALLOCATOR
!
            DSM_TAX_DEP(YR) = DSM_TAX_DEP(YR) + TAX_DEP(YR)
            DSM_SL_TAX_DEP(YR) = DSM_SL_TAX_DEP(YR) + TAX_SL_DEP(YR)
            DSM_REGULATED_TAX_DEP(YR) = DSM_REGULATED_TAX_DEP(YR) + &
                                             TAX_DEP(YR) * REG_ALLOCATOR
            DSM_DEFERRED_TAX_BASIS(YR) = DSM_DEFERRED_TAX_BASIS(YR) + &
                                            TAX_DEP(YR) - TAX_SL_DEP(YR)
            RB_DSM_DEFERRED_TAX_BASIS(YR) = &
                                       RB_DSM_DEFERRED_TAX_BASIS(YR) + &
                                     (TAX_DEP(YR) - TAX_SL_DEP(YR)) * &
                                         REG_ALLOCATOR
            IF(INDEX(DEPMET(DSM_ACCTS),'SL') == 0) &
                  DSM_TAX_PREF_DEP(YR) = DSM_TAX_PREF_DEP(YR) + &
                                           TAX_DEP(YR) - ADR_TAX_DEP(YR)
         ENDDO
         CALL DSM_CLASS_GPV(DSM_ACCTS,TOTAL_DSM_CAPITAL,BOOK_DEP, &
                            AFUDC,TAX_DEP,TAX_SL_DEP,ADR_TAX_DEP)
      ENDIF
      RETURN
      END
! ********************************************************************
!*                                                                     *
!*                       DSM_BOOK_DEP_CAL                              *
!*                                                                     *
!*          COPYRIGHT (C) 1982, 1990   M.S. GERBER & ASSOCIATES, INC   *
!*                       ALL RIGHTS RESERVED                           *
!*                                                                     *
! ********************************************************************
!                                                                      *
!     PURPOSE:                                                         *
!        DSM_BOOK_DEP CALCULATES THE BOOK DEPRECIATION/AMORTIZATION    *
!        FOR CAPITIALIZED DSM PROJECTS USING STRAIGHT LINE WITH        *
!        HALF YEAR CONVENTION                                          *
!                                                                      *
! ********************************************************************
!
      SUBROUTINE DSM_BOOK_DEP_CAL(BOOK_VALUE,BOOK_DEP, &
                                  BOOK_LIFE,SERVICE_MO)
!
      use SpinDriftLib
      use dsmfrcom
             use prod_arrays_dimensions
      use sizecom
      use globecom
      use dsf
      use dsf2
      implicit none
      INTEGER*2 SERVICE_MO
      REAL BOOK_VALUE,BOOK_DEP(MAX_FINANCIAL_SIMULATION_YEARS),BOOK_LIFE
!
!  STUFF TO GET ASSET VECTOR
!
      CHARACTER*1 DATA_TYPE
      REAL VECTOR_DATA(AVAIL_DATA_YEARS)
      INTEGER*2 VECTOR
!
!  OLD VARIABLES
!
      INTEGER*2 I,M,YR
      REAL DEPSL,NET_BOOK_VALUE
!  COMMON BLOCK VARIABLES WORLD
!
!  DSM FINANCIAL EXTERNAL VARIABLES
!
!
!  FINANCIAL RESULTS COMMON BLOCK
!
      
!
!      IF THE BOOK LIFE IS LESS THAN ONE YEAR THE ASSUMPTION IS THAT
!      THE TOTAL AMOUNT OF CEP AND AFDC2 ARE TO BE EXPENSED IN ONE
!      YEAR
!
!
!      INITIALIZE VARIABLES
!
      BOOK_DEP = 0.
      YR = globecom_year + 1
      IF(BOOK_LIFE <= 1.0 .AND. BOOK_LIFE >= 0.) THEN
         BOOK_DEP(YR) = BOOK_VALUE
         GOTO 100
      ENDIF
!
!      IF THE BOOK LIFE IS GREATER THAN 98 YEARS DEPRECIATION IS NOT
!      TAKEN
!
      IF(BOOK_LIFE > 1) THEN
         IF(BOOK_LIFE <= 98.0 .AND. BOOK_VALUE /= 0.) THEN
!
!      CALCULATE BOOK DEPRECIATION ON BRICKS AND MORTAR
!
            DEPSL = BOOK_VALUE / BOOK_LIFE
            BOOK_DEP(YR) = FLOAT(13-SERVICE_MO)*DEPSL / 12.
            NET_BOOK_VALUE = BOOK_VALUE - BOOK_DEP(YR)
            M = globecom_year + 2
            IF(M <= MAX_FINANCIAL_SIMULATION_YEARS) THEN
               DO I = M, MAX_FINANCIAL_SIMULATION_YEARS
                  IF(ABS(NET_BOOK_VALUE) < ABS(DEPSL)) THEN
                     BOOK_DEP(I) = BOOK_DEP(I) + NET_BOOK_VALUE
                     GOTO 100
                  ELSE
                     BOOK_DEP(I) = BOOK_DEP(I) + DEPSL
                     NET_BOOK_VALUE = NET_BOOK_VALUE - DEPSL
                  ENDIF
               ENDDO
            ENDIF
         ENDIF
      ELSE
         VECTOR = BOOK_LIFE
         CALL GET_DSM_VAR(VECTOR,DATA_TYPE,VECTOR_DATA)
         BOOK_DEP(YR) = MIN(BOOK_VALUE,VECTOR_DATA(1))
         NET_BOOK_VALUE = BOOK_VALUE - BOOK_DEP(YR)
         YR = YR + 1
         I = 2
         DOWHILE (NET_BOOK_VALUE > 0. .AND. &
                                   YR <= MAX_FINANCIAL_SIMULATION_YEARS)
            BOOK_DEP(YR) = MIN(VECTOR_DATA(I),NET_BOOK_VALUE)
            NET_BOOK_VALUE = NET_BOOK_VALUE - BOOK_DEP(YR)
            YR = YR + 1
            I = I + 1
         ENDDO
!
!  GET POINTER FROM ASSET VECTORS
!
      ENDIF
  100 RETURN
      END
! **********************************************************************
!
!              FUNCTION TO READ VARIABLE UNIT PARAMETERS FOR MIDAS
!                    COPYRIGHT (C) 1986
!                         M.S. GERBER & ASSOCIATES, INC.
!                               ALL RIGHTS RESERVED
!
! **********************************************************************
!
      SUBROUTINE GET_DSM_VAR(VECTOR,DATA_TYPE,DATA)
      use end_routine, only: end_program, er_message
!
       use SpinDriftLib
             use prod_arrays_dimensions
      use sizecom
      CHARACTER*1 DATA_TYPE
      INTEGER*2 IREC,VECTOR
      REAL DATA(AVAIL_DATA_YEARS)
      IREC = ABS(VECTOR)
      READ(82,REC=IREC,ERR=10) DATA_TYPE,DATA
      RETURN

   10 WRITE(SCREEN_MESSAGES,"(A,I4,A)") &
   'Error reading variable asset '// &
                              'vector file at record',IREC,' In DSM FIN'
      CALL MG_LOCATE_WRITE(20,2,trim(SCREEN_MESSAGES), &
                                                         ALL_VERSIONS,1)
      er_message='stop requested from DSM_OBJT SIID78'
      call end_program(er_message)
      END
! ********************************************************************
! *                                                                    *
! *                    DSM_TAX_DEP_CAL                                 *
! *                                                                    *
! *         COPYRIGHT (C) 1982,1990 M.S. GERBER & ASSOCIATES, INC.     *
! *                      ALL RIGHTS RESERVED                           *
! *                                                                    *
! ********************************************************************
! *                                                                    *
! *   PURPOSE:                                                         *
! *      DEPTAX CALCULATES THE ANNUAL DEPRECIATION FOR FEDERAL TAXES   *
! *      BASED ON THE ACCELERATED COST RECOVERY SYSTEM (ACRS) AS       *
! *      SPECIFIED IN THE ECONOMIC RECOVERY TAX ACT (ERTA) OF 1981, OR *
! *      THE ASSET DEPRECIATION RANGE (ADR), OR STRAIGHT LINE.         *
! *      THE MEHTOD USED IS SPECIFIED FOR EACH ACCOUNT                 *
! *                                                                    *
! ********************************************************************
!
      SUBROUTINE DSM_TAX_DEP_CAL(TAXLF,DEPMET,DBRATE,TAX_VALUE, &
                                 TAXDP)
!
       use SpinDriftLib
             use prod_arrays_dimensions
      use globecom
      use sizecom
!      INTERNAL VARIABLES
      LOGICAL*1 NO_SWITCH
      INTEGER*2 YR
      REAL TAX_VALUE
      INTEGER*2 YREND,SUMYD,L,LIFECK,IYR,M
      REAL TAXLF,DBRATE,TAXDPC,GPVF,DEPDB,DEPSL
      REAL NPVFT
!      EXTERNAL VARIABLES
      CHARACTER*4 DEPMET
      REAL TAXDP(MAX_FINANCIAL_SIMULATION_YEARS)
!
      TAXDP = 0.
!
!      IF THE TAX LIFE IS GREATER THAN 98 YEARS, IT IS ASSUMED
!      THAT NO TAX DEPRECIATION WILL OCURR
!
      IF(TAXLF > 98.0 .OR. TAX_VALUE == 0.) then
        GOTO 200
      endif
      IF(TAXLF <= 1.0) THEN
         TAXDP(globecom_year+1) = TAX_VALUE
        GOTO 200
      ENDIF
!
      YR = globecom_year + 1
      IF(INDEX(DEPMET,'DB') /= 0 .OR. INDEX(DEPMET,'ADR') /= 0) THEN
!      IF(DEPMET == DB .OR. DEPMET == ADR) THEN
         NO_SWITCH = INDEX(DEPMET,'DBNS') /= 0
         NPVFT = TAX_VALUE / 2.
         TAXDPC = 0.0
         GPVF = TAX_VALUE
         DO L = YR, MAX_FINANCIAL_SIMULATION_YEARS
            DEPDB = DBRATE * NPVFT / TAXLF
            DEPSL = NPVFT/(TAXLF - FLOAT(L - YR))
            IF(DEPDB > DEPSL .OR. NO_SWITCH) THEN
!               THEN TAKE THE DB VALUE FOR THE EXPENSE
               TAXDP(L) = TAXDP(L) + DEPDB
               TAXDPC =  TAXDPC + DEPDB
               NPVFT = GPVF - TAXDPC
!            ELSE TAKE THE STRAIGHT LINE UNTIL NPV IS ZERO
            ELSE
               DO M = L, MAX_FINANCIAL_SIMULATION_YEARS
                  IF(NPVFT < DEPSL) THEN
                     TAXDP(M) = TAXDP(M) + NPVFT
                     GOTO 200
                  ENDIF
                  TAXDP(M) = TAXDP(M) + DEPSL
                  NPVFT = NPVFT - DEPSL
               ENDDO
               GOTO 200
            ENDIF
         ENDDO
         GOTO 200
      ENDIF

      IF(INDEX(DEPMET,'SL') /= 0) THEN
         TAXDP(YR) = TAX_VALUE/(2. * TAXLF)
         NPVFT = TAX_VALUE - TAXDP(YR)
         DEPSL = TAX_VALUE / TAXLF
         M = YR + 1
         IF (M > MAX_FINANCIAL_SIMULATION_YEARS) GOTO 200
         DO L = M, MAX_FINANCIAL_SIMULATION_YEARS
            IF(NPVFT < DEPSL) THEN
               TAXDP(L) = TAXDP(L) + NPVFT
               GOTO 200
            ELSE
               TAXDP(L) = TAXDP(L) + DEPSL
               NPVFT = NPVFT - DEPSL
            ENDIF
         ENDDO
         GOTO 200
      ENDIF
!

      IF(INDEX(DEPMET,'SYD') /= 0) THEN
         LIFECK = INT(TAXLF + 0.001)
         SUMYD = LIFECK * (LIFECK + 1) / 2
         YREND = YR + LIFECK - 1
         IF(YREND > MAX_FINANCIAL_SIMULATION_YEARS) &
                                 YREND = MAX_FINANCIAL_SIMULATION_YEARS
         DO L = YR, YREND
            IYR = TAXLF  - (L - YR)
            TAXDP(L) = TAXDP(L)+TAX_VALUE*FLOAT(IYR)/FLOAT(SUMYD)
         ENDDO
      ENDIF
  200 RETURN
      END
! ********************************************************************
! *                                                                    *
! *                    ZERO_DSM_RESULTS                                *
! *                                                                    *
! *         COPYRIGHT (C) 1990 M.S. GERBER & ASSOCIATES, INC.          *
! *                      ALL RIGHTS RESERVED                           *
! *                                                                    *
! ********************************************************************
! *                                                                    *
! *   PURPOSE:                                                         *
! *      SETS THE DSM FINANCIAL RESULTS TO ZERO.                       *
! *                                                                    *
! ********************************************************************
!
      SUBROUTINE ZERO_DSM_RESULTS
!
       use SpinDriftLib
             use prod_arrays_dimensions
      use sizecom
      REAL DSM_VALUES

      COMMON/DSM_FINANCIAL_RESULTS/DSM_VALUES( &
                            2*3+16*1+12*MAX_FINANCIAL_SIMULATION_YEARS)
!
         DSM_VALUES = 0.
      RETURN
      END
! ********************************************************************
      FUNCTION DSM_DATA_CALLS()
      INTEGER*2 R_YEAR
      REAL*4   R_DSM_CAPACITY,LM_PLANNING_CAPACITY,DSM_DATA_CALLS, &
               NEW_LM_PLANNING_CAPACITY,OLD_LM_PLANNING_CAPACITY
         DSM_DATA_CALLS = 1.
      RETURN
! ********************************************************************
      ENTRY LM_PLANNING_CAPACITY(R_YEAR)
! ********************************************************************
         CALL GET_DSM_CAPACITY(R_DSM_CAPACITY,R_YEAR)
         LM_PLANNING_CAPACITY = R_DSM_CAPACITY
      RETURN
! ********************************************************************
      ENTRY OLD_LM_PLANNING_CAPACITY(R_YEAR)
! ********************************************************************
         CALL GET_OLD_DSM_CAPACITY(R_DSM_CAPACITY,R_YEAR)
         OLD_LM_PLANNING_CAPACITY = R_DSM_CAPACITY
      RETURN
! ********************************************************************
      ENTRY NEW_LM_PLANNING_CAPACITY(R_YEAR)
! ********************************************************************
         CALL GET_NEW_DSM_CAPACITY(R_DSM_CAPACITY,R_YEAR)
         NEW_LM_PLANNING_CAPACITY = R_DSM_CAPACITY
      RETURN
      END
! ********************************************************************
!
!  ROUTINE TO ALLOCATE DSM EXPENSE AND REVENUES TO ASSET CLASSES
!  COPYRIGHT (c) 1995 M.S. GERBER & ASSOCIATES, INC.
!  ALL RIGHTS RESERVED
!
! ********************************************************************
      SUBROUTINE DSM_ASSET_ALLOCATION(DSM_ACCTS,TOTAL_DSM_EXPENSES, &
                                      TOTAL_REBATE_EXPENSE, &
                                      DSM_PURCHASE_POWER, &
                                      DSM_SALES_REVENUE, &
                                      TOTAL_DSM_CAPITAL, &
                                      TOTAL_PROGRAM_PARTICIPANT_COSTS, &
                                      PROGRAM_UTIL_NON_ELEC_COSTS, &
                                      PROGRAM_THIRD_PARTY_COSTS, &
                                      PROGRAM_OTH_PARTICIPANT_COSTS, &
                                      DSM_DEVICE_PEAK, &
                                      DSM_DEVICE_ENERGY)
      use dsf
      use dsmfrcom
      use dsf2
      use dsfc

      use SpinDriftLib
             use prod_arrays_dimensions
      use sizecom
      use globecom

      
!
      CHARACTER*1 DUMMY_TYPE
      INTEGER*2 DSM_ACCTS,YR
      INTEGER*4 VALUES_2_INIT
      REAL TOTAL_DSM_EXPENSES, &
           TOTAL_REBATE_EXPENSE, &
           DSM_PURCHASE_POWER, &
           DSM_SALES_REVENUE, &
           TOTAL_DSM_CAPITAL, &
           TOTAL_PROGRAM_PARTICIPANT_COSTS, &
           PROGRAM_UTIL_NON_ELEC_COSTS, &
           PROGRAM_THIRD_PARTY_COSTS, &
           PROGRAM_OTH_PARTICIPANT_COSTS, &
           DSM_DEVICE_PEAK, &
           DSM_DEVICE_ENERGY
      REAL DSM_CLASS_EXPENSES(:), &
           DSM_CLASS_REBATES(:), &
           DSM_CLASS_PURCHASE_POWER(:), &
           DSM_CLASS_SALES_REVENUE(:), &
           DSM_BTL_EXPENSE(:), &
           DSM_BTL_REVENUE(:), &
           DSM_ADJ_CLAUSE_COLLECTIONS(:), &
           DSM_CLASS_CAPITIAL(:), &
           DSM_CLASS_CAPACITY(:), &
           DSM_CLASS_ENERGY(:), &
           DSM_CLASS_PARTICIPANT_COSTS(:), &
           DSM_CLASS_UTIL_NON_ELEC_COSTS(:), &
           DSM_CLASS_THIRD_PARTY_COSTS(:), &
           DSM_CLASS_OTH_PARTICIPANT_COSTS(:)
      ALLOCATABLE ::  DSM_CLASS_EXPENSES, &
                      DSM_CLASS_REBATES, &
                      DSM_CLASS_PURCHASE_POWER, &
                      DSM_CLASS_SALES_REVENUE, &
                      DSM_BTL_EXPENSE, &
                      DSM_BTL_REVENUE, &
                      DSM_ADJ_CLAUSE_COLLECTIONS, &
                      DSM_CLASS_CAPITIAL, &
                      DSM_CLASS_CAPACITY, &
                      DSM_CLASS_ENERGY, &
                      DSM_CLASS_PARTICIPANT_COSTS, &
                      DSM_CLASS_UTIL_NON_ELEC_COSTS, &
                      DSM_CLASS_THIRD_PARTY_COSTS, &
                      DSM_CLASS_OTH_PARTICIPANT_COSTS
      SAVE DSM_CLASS_EXPENSES, &
           DSM_CLASS_REBATES, &
           DSM_CLASS_PURCHASE_POWER, &
           DSM_CLASS_SALES_REVENUE, &
           DSM_BTL_EXPENSE, &
           DSM_BTL_REVENUE, &
           DSM_ADJ_CLAUSE_COLLECTIONS, &
           DSM_CLASS_CAPITIAL, &
           DSM_CLASS_CAPACITY, &
           DSM_CLASS_ENERGY, &
           DSM_CLASS_PARTICIPANT_COSTS, &
           DSM_CLASS_UTIL_NON_ELEC_COSTS, &
           DSM_CLASS_THIRD_PARTY_COSTS, &
           DSM_CLASS_OTH_PARTICIPANT_COSTS
      INTEGER*2 NUM_OF_DSM_CLASSES, &
                MAX_DSM_CLASS_NUM, &
                ASSET_CLASS_POINTER(:)
      ALLOCATABLE :: ASSET_CLASS_POINTER
      SAVE NUM_OF_DSM_CLASSES, &
           MAX_DSM_CLASS_NUM, &
           ASSET_CLASS_POINTER
      REAL ASSET_CLASS_LIST(:),ASSET_ALLOCATION_LIST(:)
      ALLOCATABLE :: ASSET_CLASS_LIST,ASSET_ALLOCATION_LIST
      SAVE ASSET_CLASS_LIST,ASSET_ALLOCATION_LIST
      REAL ASSET_ALLOCATOR
      INTEGER (kind=2) ::  ASSET_CLASS
      integer (kind=2) :: ASSET_ALLOCATION_VECTOR,CLASS_POINTER
      INTEGER*2 R_YR,R_CLASS
      LOGICAL*1 R_CLASS_EXISTS
      REAL R_DSM_EXPENSE, &
           R_DSM_REBATE, &
           R_DSM_PURCHASE_POWER, &
           R_DSM_SALES_REVENUE, &
           R_DSM_BTL_EXPENSE, &
           R_DSM_BTL_REVENUE, &
           R_DSM_ADJ_CLAUSE_COLLECTIONS, &
           R_DSM_CAPITIAL, &
           R_DSM_CAPACITY, &
           R_DSM_ENERGY, &
           R_DSM_PARTICIPANT_COSTS, &
           R_DSM_UTIL_NON_ELEC_COSTS, &
           R_DSM_THIRD_PARTY_COSTS, &
           R_DSM_OTH_PARTICIPANT_COSTS
      REAL R_DSM_BOOK_DEP, &
           R_DSM_CUM_BOOK_DEP, &
           R_DSM_GPV, &
           R_DSM_AFUDC, &
           R_DSM_TAX_DEP, &
           R_DSM_TAX_EXPENSE, &
           R_DSM_AMORT, &
           R_DSM_DEFERRED_DEBIT, &
           R_DSM_DEFERRED_TAX_BASIS, &
           R_DSM_TAX_PREF_DEP, &
           R_RB_DSM_AMORT, &
           R_RB_DSM_DEFERRED_TAX_BASIS, &
           R_RB_DSM_AFUDC, &
           R_RB_DSM_TAX_EXPENSE, &
           R_DSM_REGULATED_BOOK_DEP, &
           R_DSM_REGULATED_TAX_DEP, &
           R_DSM_REGULATED_DEFERRED_DEBIT, &
           R_DSM_REGULATED_NPV, &
           R_DSM_GPV_PROPERTY_TAX, &
           R_DSM_NPV_PROPERTY_TAX, &
           R_DSM_SL_TAX_DEP
!
!  CAPITIALIZED AND DEFERRED DSM INVESTMENT
!
      REAL*4 BOOK_DEP(*),REG_ALLOCATOR
      REAL ALLOCATED_DSM_BOOK_DEP,ALLOCATED_DSM_CAPITAL
      REAL ALLOCATED_DSM_AFUDC,AFUDC,TAX_VALUE,ESCALATOR,TOTAL_BOOK_DEP
      REAL PROPERTY_ESCALATION,TAX_DEP(*),TAX_SL_DEP(*),ADR_TAX_DEP(*)
      REAL DSM_BOOK_DEP_loc(:,:), &
           DSM_CUM_BOOK_DEP(:,:), &
           DSM_GPV_loc(:), &
           DSM_AFUDC_loc(:), &
           DSM_TAX_DEP_loc(:,:), &
           DSM_TAX_EXPENSE_loc(:), &
           DSM_AMORT_loc(:,:), &
           DSM_DEFERRED_DBT(:), &
           DSM_DFRD_TAX_BASIS_loc(:,:), &
           DSM_TAX_PREF_DEP_loc(:,:), &
           RB_DSM_AMORT_loc(:,:), &
           RB_DSM_DFRD_TAX_BASIS_loc(:,:), &
           RB_DSM_AFUDC_loc(:), &
           RB_DSM_TAX_EXPENSE_loc(:), &
           DSM_REGULATED_BOOK_DEP_loc(:,:), &
           DSM_REGULATED_TAX_DEP_loc(:,:), &
           DSM_REGULATED_DFRD_DBT_loc(:), &
           DSM_REGULATED_NPV_loc(:), &
           DSM_GPV_PROPERTY_TAX_loc(:,:), &
           DSM_NPV_PROPERTY_TAX_loc(:,:), &
           DSM_SL_TAX_DEP_loc(:,:)
      ALLOCATABLE :: DSM_BOOK_DEP_loc,DSM_CUM_BOOK_DEP, &
                     DSM_GPV_loc, &
                     DSM_AFUDC_loc, &
                     DSM_TAX_DEP_loc, &
                     DSM_TAX_EXPENSE_loc, &
                     DSM_AMORT_loc, &
                     DSM_DEFERRED_DBT, &
                     DSM_DFRD_TAX_BASIS_loc, &
                     DSM_TAX_PREF_DEP_loc, &
                     RB_DSM_AMORT_loc, &
                     RB_DSM_DFRD_TAX_BASIS_loc, &
                     RB_DSM_AFUDC_loc, &
                     RB_DSM_TAX_EXPENSE_loc, &
                     DSM_REGULATED_BOOK_DEP_loc, &
                     DSM_REGULATED_TAX_DEP_loc, &
                     DSM_REGULATED_DFRD_DBT_loc, &
                     DSM_REGULATED_NPV_loc, &
                     DSM_GPV_PROPERTY_TAX_loc, &
                     DSM_NPV_PROPERTY_TAX_loc, &
                     DSM_SL_TAX_DEP_loc
      SAVE DSM_BOOK_DEP_loc,DSM_CUM_BOOK_DEP, &
           DSM_GPV_loc, &
           DSM_AFUDC_loc, &
           DSM_TAX_DEP_loc, &
           DSM_TAX_EXPENSE_loc, &
           DSM_AMORT_loc, &
           DSM_DEFERRED_DBT, &
           DSM_DFRD_TAX_BASIS_loc, &
           DSM_TAX_PREF_DEP_loc, &
           RB_DSM_AMORT_loc, &
           RB_DSM_DFRD_TAX_BASIS_loc, &
           RB_DSM_AFUDC_loc, &
           RB_DSM_TAX_EXPENSE_loc, &
           DSM_REGULATED_BOOK_DEP_loc, &
           DSM_REGULATED_TAX_DEP_loc, &
           DSM_REGULATED_DFRD_DBT_loc, &
           DSM_REGULATED_NPV_loc, &
           DSM_GPV_PROPERTY_TAX_loc, &
           DSM_NPV_PROPERTY_TAX_loc, &
           DSM_SL_TAX_DEP_loc
!
         ALLOCATED_DSM_AFUDC=0. ! Spurious Lahey warning prevented.
         ASSET_CLASS = DSM_ASSET_CLASS_NUM(DSM_ACCTS)
         ASSET_CLASS = DSM_ASSET_CLASS_NUM(DSM_ACCTS)
         ASSET_ALLOCATION_VECTOR = DSM_ASSET_CLASS_VECTOR(DSM_ACCTS)
!
         IF(ASSET_CLASS < 0) THEN
            CALL GET_ASSET_VAR(ABS(ASSET_CLASS), &
                                    DUMMY_TYPE,ASSET_CLASS_LIST)
            CALL GET_ASSET_VAR(ABS(ASSET_ALLOCATION_VECTOR), &
                                    DUMMY_TYPE,ASSET_ALLOCATION_LIST)
         ELSE
            ASSET_CLASS_LIST(1) = ASSET_CLASS
            ASSET_CLASS_LIST(2) = 0.
            ASSET_ALLOCATION_LIST(1) = 100.
            ASSET_ALLOCATION_LIST(2) = 0.
         ENDIF
         CLASS_POINTER = 1
         DO
            ASSET_CLASS = ASSET_CLASS_LIST(CLASS_POINTER)
            CALL CHECK_IF_CLASS_DEFINED(ASSET_CLASS)
            ASSET_CLASS = ASSET_CLASS + 1
            IF(ASSET_CLASS > 0) ASSET_CLASS = &
                                        ASSET_CLASS_POINTER(ASSET_CLASS)
            ASSET_ALLOCATOR = ASSET_ALLOCATION_LIST(CLASS_POINTER)/100.
!
            IF(COLLECT(DSM_ACCTS)=="N") THEN
               DSM_BTL_EXPENSE(ASSET_CLASS) = (TOTAL_DSM_EXPENSES + &
                                 TOTAL_REBATE_EXPENSE + &
                               DSM_PURCHASE_POWER) * ASSET_ALLOCATOR + &
                                 DSM_BTL_EXPENSE(ASSET_CLASS)
               DSM_BTL_REVENUE(ASSET_CLASS) = &
                                 DSM_SALES_REVENUE * ASSET_ALLOCATOR + &
                                   DSM_BTL_REVENUE(ASSET_CLASS)
            ELSE
               IF(COLLECT(DSM_ACCTS)=="A") THEN
                  DSM_ADJ_CLAUSE_COLLECTIONS(ASSET_CLASS) = &
                                (TOTAL_DSM_EXPENSES + &
                                 TOTAL_REBATE_EXPENSE + &
                                 DSM_PURCHASE_POWER - &
                                DSM_SALES_REVENUE) * ASSET_ALLOCATOR + &
                                DSM_ADJ_CLAUSE_COLLECTIONS(ASSET_CLASS)
               ENDIF
               DSM_CLASS_EXPENSES(ASSET_CLASS) = &
                                TOTAL_DSM_EXPENSES * ASSET_ALLOCATOR + &
                                  DSM_CLASS_EXPENSES(ASSET_CLASS)
               DSM_CLASS_REBATES(ASSET_CLASS) = &
                              TOTAL_REBATE_EXPENSE * ASSET_ALLOCATOR + &
                                DSM_CLASS_REBATES(ASSET_CLASS)
               DSM_CLASS_PURCHASE_POWER(ASSET_CLASS) = &
                                DSM_PURCHASE_POWER * ASSET_ALLOCATOR + &
                                  DSM_CLASS_PURCHASE_POWER(ASSET_CLASS)
               DSM_CLASS_SALES_REVENUE(ASSET_CLASS) = &
                                 DSM_SALES_REVENUE * ASSET_ALLOCATOR + &
                                   DSM_CLASS_SALES_REVENUE(ASSET_CLASS)
            ENDIF
!
            DSM_CLASS_CAPITIAL(ASSET_CLASS) = &
                                 TOTAL_DSM_CAPITAL * ASSET_ALLOCATOR + &
                                   DSM_CLASS_CAPITIAL(ASSET_CLASS)
            DSM_CLASS_CAPACITY(ASSET_CLASS) = &
                                   DSM_DEVICE_PEAK * ASSET_ALLOCATOR + &
                                     DSM_CLASS_CAPACITY(ASSET_CLASS)
            DSM_CLASS_ENERGY(ASSET_CLASS) = &
                                 DSM_DEVICE_ENERGY * ASSET_ALLOCATOR + &
                                   DSM_CLASS_ENERGY(ASSET_CLASS)
            DSM_CLASS_PARTICIPANT_COSTS(ASSET_CLASS) = &
                   TOTAL_PROGRAM_PARTICIPANT_COSTS * ASSET_ALLOCATOR + &
                     DSM_CLASS_PARTICIPANT_COSTS(ASSET_CLASS)
            DSM_CLASS_UTIL_NON_ELEC_COSTS(ASSET_CLASS) = &
                       PROGRAM_UTIL_NON_ELEC_COSTS * ASSET_ALLOCATOR + &
                         DSM_CLASS_UTIL_NON_ELEC_COSTS(ASSET_CLASS)
            DSM_CLASS_THIRD_PARTY_COSTS(ASSET_CLASS) = &
                         PROGRAM_THIRD_PARTY_COSTS * ASSET_ALLOCATOR + &
                           DSM_CLASS_THIRD_PARTY_COSTS(ASSET_CLASS)
            DSM_CLASS_OTH_PARTICIPANT_COSTS(ASSET_CLASS) = &
                     PROGRAM_OTH_PARTICIPANT_COSTS * ASSET_ALLOCATOR + &
                       DSM_CLASS_OTH_PARTICIPANT_COSTS(ASSET_CLASS)
!
            CLASS_POINTER = CLASS_POINTER + 1
            IF(CLASS_POINTER > AVAIL_DATA_YEARS) EXIT
            IF(ASSET_CLASS_LIST(CLASS_POINTER) == 0. .OR. &
                           ASSET_CLASS_LIST(CLASS_POINTER) ==-99.) EXIT
         ENDDO ! ASSET CLASSES
!
      RETURN
! ********************************************************************
      ENTRY DSM_CLASS_DEBITS(DSM_ACCTS,TOTAL_DSM_CAPITAL,BOOK_DEP)
! ********************************************************************
!
         ASSET_CLASS = DSM_ASSET_CLASS_NUM(DSM_ACCTS)
         ASSET_ALLOCATION_VECTOR = DSM_ASSET_CLASS_VECTOR(DSM_ACCTS)
!
         IF(ASSET_CLASS < 0) THEN
            CALL GET_ASSET_VAR(ABS(ASSET_CLASS), &
                                    DUMMY_TYPE,ASSET_CLASS_LIST)
            CALL GET_ASSET_VAR(ABS(ASSET_ALLOCATION_VECTOR), &
                                    DUMMY_TYPE,ASSET_ALLOCATION_LIST)
         ELSE
            ASSET_CLASS_LIST(1) = ASSET_CLASS
            ASSET_CLASS_LIST(2) = 0.
            ASSET_ALLOCATION_LIST(1) = 100.
            ASSET_ALLOCATION_LIST(2) = 0.
         ENDIF
         CLASS_POINTER = 1
         REG_ALLOCATOR = REGULATORY_ALLOCATOR(DSM_ACCTS)
         DO
            ASSET_CLASS = ASSET_CLASS_LIST(CLASS_POINTER)
            CALL CHECK_IF_CLASS_DEFINED(ASSET_CLASS)
            ASSET_CLASS = ASSET_CLASS + 1
            IF(ASSET_CLASS > 0) ASSET_CLASS = &
                                       ASSET_CLASS_POINTER(ASSET_CLASS)
            ASSET_ALLOCATOR = ASSET_ALLOCATION_LIST(CLASS_POINTER)/100.
!
            ALLOCATED_DSM_CAPITAL = TOTAL_DSM_CAPITAL * ASSET_ALLOCATOR
            DSM_DEFERRED_DBT(ASSET_CLASS) = &
                                     DSM_DEFERRED_DBT(ASSET_CLASS) + &
                                                  ALLOCATED_DSM_CAPITAL
           DSM_TAX_EXPENSE_loc(ASSET_CLASS) = &
           DSM_TAX_EXPENSE_loc(ASSET_CLASS)+ &
                                                  ALLOCATED_DSM_CAPITAL
            RB_DSM_TAX_EXPENSE_loc(ASSET_CLASS) = &
                  RB_DSM_TAX_EXPENSE_loc(ASSET_CLASS) +REG_ALLOCATOR * &
                                                  ALLOCATED_DSM_CAPITAL
            DSM_REGULATED_DFRD_DBT_loc(ASSET_CLASS) = &
                            DSM_REGULATED_DFRD_DBT_loc(ASSET_CLASS)+ &
                             REG_ALLOCATOR * ALLOCATED_DSM_CAPITAL
            DSM_DFRD_TAX_BASIS_loc(ASSET_CLASS,globecom_year+1) = &
                     DSM_DFRD_TAX_BASIS_loc(ASSET_CLASS,globecom_year+1) + &
                              TAXEXP(DSM_ACCTS) * ALLOCATED_DSM_CAPITAL
            RB_DSM_DFRD_TAX_BASIS_loc(ASSET_CLASS,globecom_year+1) = &
                       RB_DSM_DFRD_TAX_BASIS_loc(ASSET_CLASS,globecom_year+1) + &
              ALLOCATED_DSM_CAPITAL * TAXEXP(DSM_ACCTS) * REG_ALLOCATOR
            DO YR = globecom_year+1, MAX_FINANCIAL_SIMULATION_YEARS
               ALLOCATED_DSM_BOOK_DEP = BOOK_DEP(YR) * ASSET_ALLOCATOR
               DSM_AMORT_loc(ASSET_CLASS,YR) = &
               DSM_AMORT_loc(ASSET_CLASS,YR) + ALLOCATED_DSM_BOOK_DEP
               RB_DSM_AMORT_loc(ASSET_CLASS,YR) = &
                                  RB_DSM_AMORT_loc(ASSET_CLASS,YR) + &
                                 ALLOCATED_DSM_BOOK_DEP * REG_ALLOCATOR
               DSM_DFRD_TAX_BASIS_loc(ASSET_CLASS,YR) = &
                              DSM_DFRD_TAX_BASIS_loc(ASSET_CLASS,YR) - &
                             ALLOCATED_DSM_BOOK_DEP * TAXEXP(DSM_ACCTS)
               RB_DSM_DFRD_TAX_BASIS_loc(ASSET_CLASS,YR) = &
                           RB_DSM_DFRD_TAX_BASIS_loc(ASSET_CLASS,YR) - &
                          ALLOCATED_DSM_BOOK_DEP * TAXEXP(DSM_ACCTS) * &
                                                          REG_ALLOCATOR
            ENDDO
!
            CLASS_POINTER = CLASS_POINTER + 1
            IF(CLASS_POINTER > AVAIL_DATA_YEARS) EXIT
            IF(ASSET_CLASS_LIST(CLASS_POINTER) == 0. .OR. &
                           ASSET_CLASS_LIST(CLASS_POINTER) ==-99.) EXIT
         ENDDO ! ASSET CLASSES
!
      RETURN
! ********************************************************************
      ENTRY DSM_CLASS_GPV(DSM_ACCTS,TOTAL_DSM_CAPITAL,BOOK_DEP, &
                          AFUDC,TAX_DEP,TAX_SL_DEP,ADR_TAX_DEP)
! ********************************************************************
!
         ASSET_CLASS = DSM_ASSET_CLASS_NUM(DSM_ACCTS)
         ASSET_ALLOCATION_VECTOR = DSM_ASSET_CLASS_VECTOR(DSM_ACCTS)
!
         IF(ASSET_CLASS < 0) THEN
            CALL GET_ASSET_VAR(ABS(ASSET_CLASS), &
                                    DUMMY_TYPE,ASSET_CLASS_LIST)
            CALL GET_ASSET_VAR(ABS(ASSET_ALLOCATION_VECTOR), &
                                    DUMMY_TYPE,ASSET_ALLOCATION_LIST)
         ELSE
            ASSET_CLASS_LIST(1) = ASSET_CLASS
            ASSET_CLASS_LIST(2) = 0.
            ASSET_ALLOCATION_LIST(1) = 100.
            ASSET_ALLOCATION_LIST(2) = 0.
         ENDIF
         CLASS_POINTER = 1
         REG_ALLOCATOR = REGULATORY_ALLOCATOR(DSM_ACCTS)
         DO
            ASSET_CLASS = ASSET_CLASS_LIST(CLASS_POINTER)
            CALL CHECK_IF_CLASS_DEFINED(ASSET_CLASS)
            ASSET_CLASS = ASSET_CLASS + 1
            IF(ASSET_CLASS > 0) ASSET_CLASS = &
                                       ASSET_CLASS_POINTER(ASSET_CLASS)
            ASSET_ALLOCATOR = ASSET_ALLOCATION_LIST(CLASS_POINTER)/100.
!
            ALLOCATED_DSM_CAPITAL = TOTAL_DSM_CAPITAL * ASSET_ALLOCATOR
            ALLOCATED_DSM_AFUDC = AFUDC * ASSET_ALLOCATOR 
               DSM_GPV_loc(ASSET_CLASS) = DSM_GPV_loc(ASSET_CLASS) + &
                                                  ALLOCATED_DSM_CAPITAL
            DSM_REGULATED_NPV_loc(ASSET_CLASS) = &
                              DSM_REGULATED_NPV_loc(ASSET_CLASS) + &
                             (ALLOCATED_DSM_AFUDC + &
                              ALLOCATED_DSM_CAPITAL) * REG_ALLOCATOR
            DSM_AFUDC_loc(ASSET_CLASS) = DSM_AFUDC_loc(ASSET_CLASS) + &
                                     ALLOCATED_DSM_AFUDC
       RB_DSM_AFUDC_loc(ASSET_CLASS) = RB_DSM_AFUDC_loc(ASSET_CLASS) + &
                                     ALLOCATED_DSM_AFUDC * REG_ALLOCATOR
            TAX_VALUE = ALLOCATED_DSM_CAPITAL * (1.-TAXEXP(DSM_ACCTS))
          DSM_TAX_EXPENSE_loc(ASSET_CLASS) = &
            DSM_TAX_EXPENSE_loc(ASSET_CLASS)+ &
                                         ALLOCATED_DSM_CAPITAL-TAX_VALUE
            RB_DSM_TAX_EXPENSE_loc(ASSET_CLASS) = &
                 RB_DSM_TAX_EXPENSE_loc(ASSET_CLASS) + REG_ALLOCATOR * &
                                       (ALLOCATED_DSM_CAPITAL-TAX_VALUE)

            DSM_DFRD_TAX_BASIS_loc(ASSET_CLASS,globecom_year+1) = &
                          DSM_DFRD_TAX_BASIS_loc(ASSET_CLASS,globecom_year+1) + &
                                       ALLOCATED_DSM_CAPITAL - TAX_VALUE
            RB_DSM_DFRD_TAX_BASIS_loc(ASSET_CLASS,globecom_year+1) = &
                       RB_DSM_DFRD_TAX_BASIS_loc(ASSET_CLASS,globecom_year+1) + &
                                 (ALLOCATED_DSM_CAPITAL - TAX_VALUE) * &
                                         REG_ALLOCATOR

            DSM_GPV_PROPERTY_TAX_loc(ASSET_CLASS,globecom_year) = &
                          DSM_GPV_PROPERTY_TAX_loc(ASSET_CLASS,globecom_year) + &
                                                   ALLOCATED_DSM_CAPITAL
            DSM_NPV_PROPERTY_TAX_loc(ASSET_CLASS,globecom_year) = &
                          DSM_NPV_PROPERTY_TAX_loc(ASSET_CLASS,globecom_year) + &
                                ALLOCATED_DSM_CAPITAL - BOOK_DEP(globecom_year)
            ESCALATOR = 1.
            TOTAL_BOOK_DEP = 0.
            DO YR = globecom_year+1, MAX_FINANCIAL_SIMULATION_YEARS
               ESCALATOR = ESCALATOR * (1. + PROPERTY_ESCALATION(YR))
               DSM_GPV_PROPERTY_TAX_loc(ASSET_CLASS,YR) = &
                            DSM_GPV_PROPERTY_TAX_loc(ASSET_CLASS,YR) + &
                                       ALLOCATED_DSM_CAPITAL * ESCALATOR
               TOTAL_BOOK_DEP = TOTAL_BOOK_DEP + ASSET_ALLOCATOR * &
                                                            BOOK_DEP(YR)
               DSM_NPV_PROPERTY_TAX_loc(ASSET_CLASS,YR) = &
                     DSM_NPV_PROPERTY_TAX_loc(ASSET_CLASS,YR) + &
                      (ALLOCATED_DSM_CAPITAL-TOTAL_BOOK_DEP) * ESCALATOR
               DSM_BOOK_DEP_loc(ASSET_CLASS,YR) = &
                                    DSM_BOOK_DEP_loc(ASSET_CLASS,YR) + &
                                          ASSET_ALLOCATOR * BOOK_DEP(YR)
               DSM_CUM_BOOK_DEP(ASSET_CLASS,YR) = TOTAL_BOOK_DEP + &
                                        DSM_CUM_BOOK_DEP(ASSET_CLASS,YR)
               DSM_REGULATED_BOOK_DEP_loc(ASSET_CLASS,YR) = &
                          DSM_REGULATED_BOOK_DEP_loc(ASSET_CLASS,YR) + &
                          ASSET_ALLOCATOR * BOOK_DEP(YR) * REG_ALLOCATOR
!
             DSM_TAX_DEP_loc(ASSET_CLASS,YR)= &
             DSM_TAX_DEP_loc(ASSET_CLASS,YR) + &
                                           ASSET_ALLOCATOR * TAX_DEP(YR)
               DSM_SL_TAX_DEP_loc(ASSET_CLASS,YR) = &
                                  DSM_SL_TAX_DEP_loc(ASSET_CLASS,YR) + &
                                        ASSET_ALLOCATOR * TAX_SL_DEP(YR)
               DSM_REGULATED_TAX_DEP_loc(ASSET_CLASS,YR) = &
                           DSM_REGULATED_TAX_DEP_loc(ASSET_CLASS,YR) + &
                           ASSET_ALLOCATOR * TAX_DEP(YR) * REG_ALLOCATOR
               DSM_DFRD_TAX_BASIS_loc(ASSET_CLASS,YR) = &
                              DSM_DFRD_TAX_BASIS_loc(ASSET_CLASS,YR) + &
                        ASSET_ALLOCATOR * (TAX_DEP(YR) - TAX_SL_DEP(YR))
               RB_DSM_DFRD_TAX_BASIS_loc(ASSET_CLASS,YR) = &
                        RB_DSM_DFRD_TAX_BASIS_loc(ASSET_CLASS,YR) + &
                      ASSET_ALLOCATOR * (TAX_DEP(YR)-TAX_SL_DEP(YR)) * &
                                                           REG_ALLOCATOR
               IF(INDEX(DEPMET(DSM_ACCTS),'SL') == 0) &
                     DSM_TAX_PREF_DEP_loc(ASSET_CLASS,YR) = &
                                DSM_TAX_PREF_DEP_loc(ASSET_CLASS,YR) + &
                       ASSET_ALLOCATOR * (TAX_DEP(YR) - ADR_TAX_DEP(YR))
            ENDDO
            CLASS_POINTER = CLASS_POINTER + 1
            IF(CLASS_POINTER > AVAIL_DATA_YEARS) EXIT
            IF(ASSET_CLASS_LIST(CLASS_POINTER) == 0. .OR. &
                            ASSET_CLASS_LIST(CLASS_POINTER) ==-99.) EXIT
         ENDDO
      RETURN
! ********************************************************************
      ENTRY SUM_DSM_ASSET_CLASSES
! ********************************************************************
         DO ASSET_CLASS = 1, NUM_OF_DSM_CLASSES
            DSM_BTL_EXPENSE(0) = DSM_BTL_EXPENSE(0) + &
                                            DSM_BTL_EXPENSE(ASSET_CLASS)
            DSM_BTL_REVENUE(0) = DSM_BTL_REVENUE(0) + &
                                            DSM_BTL_REVENUE(ASSET_CLASS)
          DSM_ADJ_CLAUSE_COLLECTIONS(0)=DSM_ADJ_CLAUSE_COLLECTIONS(0)+ &
                                 DSM_ADJ_CLAUSE_COLLECTIONS(ASSET_CLASS)
            DSM_CLASS_EXPENSES(0) = DSM_CLASS_EXPENSES(0) + &
                                         DSM_CLASS_EXPENSES(ASSET_CLASS)
            DSM_CLASS_REBATES(0) = DSM_CLASS_REBATES(0) + &
                                          DSM_CLASS_REBATES(ASSET_CLASS)
           DSM_CLASS_PURCHASE_POWER(0) = DSM_CLASS_PURCHASE_POWER(0) + &
                                   DSM_CLASS_PURCHASE_POWER(ASSET_CLASS)
            DSM_CLASS_SALES_REVENUE(0) = DSM_CLASS_SALES_REVENUE(0) + &
                                    DSM_CLASS_SALES_REVENUE(ASSET_CLASS)
!
            DSM_CLASS_CAPITIAL(0) = DSM_CLASS_CAPITIAL(0) + &
                                         DSM_CLASS_CAPITIAL(ASSET_CLASS)
            DSM_CLASS_CAPACITY(0) = DSM_CLASS_CAPACITY(0) + &
                                         DSM_CLASS_CAPACITY(ASSET_CLASS)
            DSM_CLASS_ENERGY(0) = DSM_CLASS_ENERGY(0) + &
                                           DSM_CLASS_ENERGY(ASSET_CLASS)
            DSM_CLASS_PARTICIPANT_COSTS(0) = &
                                      DSM_CLASS_PARTICIPANT_COSTS(0) + &
                                DSM_CLASS_PARTICIPANT_COSTS(ASSET_CLASS)
            DSM_CLASS_UTIL_NON_ELEC_COSTS(0) = &
                                    DSM_CLASS_UTIL_NON_ELEC_COSTS(0) + &
                              DSM_CLASS_UTIL_NON_ELEC_COSTS(ASSET_CLASS)
            DSM_CLASS_THIRD_PARTY_COSTS(0) = &
                           DSM_CLASS_THIRD_PARTY_COSTS(0) + &
                           DSM_CLASS_THIRD_PARTY_COSTS(ASSET_CLASS)
            DSM_CLASS_OTH_PARTICIPANT_COSTS(0) = &
                                  DSM_CLASS_OTH_PARTICIPANT_COSTS(0) + &
                            DSM_CLASS_OTH_PARTICIPANT_COSTS(ASSET_CLASS)
            DSM_AFUDC_loc(0) = DSM_AFUDC_loc(0) + &
            DSM_AFUDC_loc(ASSET_CLASS)
!
            DO YR = 1, MAX_FINANCIAL_SIMULATION_YEARS
!
!  GPV DSM
!
               DSM_GPV_PROPERTY_TAX_loc(0,YR) = &
                                    DSM_GPV_PROPERTY_TAX_loc(0,YR) + &
                                DSM_GPV_PROPERTY_TAX_loc(ASSET_CLASS,YR)
               DSM_NPV_PROPERTY_TAX_loc(0,YR) = &
                                    DSM_NPV_PROPERTY_TAX_loc(0,YR) + &
                                DSM_NPV_PROPERTY_TAX_loc(ASSET_CLASS,YR)
               DSM_BOOK_DEP_loc(0,YR) = DSM_BOOK_DEP_loc(0,YR) + &
                                    DSM_BOOK_DEP_loc(ASSET_CLASS,YR)
               DSM_REGULATED_BOOK_DEP_loc(0,YR) = &
                                  DSM_REGULATED_BOOK_DEP_loc(0,YR) + &
                              DSM_REGULATED_BOOK_DEP_loc(ASSET_CLASS,YR)
!
               DSM_TAX_DEP_loc(0,YR) = DSM_TAX_DEP_loc(0,YR) + &
                                         DSM_TAX_DEP_loc(ASSET_CLASS,YR)
               DSM_SL_TAX_DEP_loc(0,YR) = DSM_SL_TAX_DEP_loc(0,YR) + &
                                      DSM_SL_TAX_DEP_loc(ASSET_CLASS,YR)
               DSM_REGULATED_TAX_DEP_loc(0,YR) = &
                                   DSM_REGULATED_TAX_DEP_loc(0,YR) + &
                               DSM_REGULATED_TAX_DEP_loc(ASSET_CLASS,YR)
               DSM_DFRD_TAX_BASIS_loc(0,YR) = &
                                  DSM_DFRD_TAX_BASIS_loc(0,YR) + &
                                  DSM_DFRD_TAX_BASIS_loc(ASSET_CLASS,YR)
               RB_DSM_DFRD_TAX_BASIS_loc(0,YR) = &
                               RB_DSM_DFRD_TAX_BASIS_loc(0,YR) + &
                               RB_DSM_DFRD_TAX_BASIS_loc(ASSET_CLASS,YR)
             DSM_TAX_PREF_DEP_loc(0,YR) = DSM_TAX_PREF_DEP_loc(0,YR) + &
                                    DSM_TAX_PREF_DEP_loc(ASSET_CLASS,YR)
!
!  DEFERRED DEBIT ITEMS
!
               DSM_AMORT_loc(0,YR) = DSM_AMORT_loc(0,YR) + &
                                           DSM_AMORT_loc(ASSET_CLASS,YR)
               RB_DSM_AMORT_loc(0,YR) = RB_DSM_AMORT_loc(0,YR) + &
                                    RB_DSM_AMORT_loc(ASSET_CLASS,YR)
               DSM_DFRD_TAX_BASIS_loc(0,YR) = &
                                  DSM_DFRD_TAX_BASIS_loc(0,YR) + &
                                  DSM_DFRD_TAX_BASIS_loc(ASSET_CLASS,YR)
               RB_DSM_DFRD_TAX_BASIS_loc(0,YR) = &
                               RB_DSM_DFRD_TAX_BASIS_loc(0,YR) + &
                               RB_DSM_DFRD_TAX_BASIS_loc(ASSET_CLASS,YR)
            ENDDO

         ENDDO
      RETURN
! ********************************************************************
      ENTRY SET_UP_ANNUAL_DSM_ASSET_ARRAYS
! ********************************************************************
         CALL RETURN_NUM_OF_DSM_CLASSES(NUM_OF_DSM_CLASSES, &
                                        MAX_DSM_CLASS_NUM)
!
         IF(MAX_DSM_CLASS_NUM > 0) THEN
            IF(ALLOCATED(ASSET_CLASS_POINTER)) &
                                         DEALLOCATE(ASSET_CLASS_POINTER)
            ALLOCATE(ASSET_CLASS_POINTER(MAX_DSM_CLASS_NUM))
            CALL RETURN_DSM_CLASS_POINTER(ASSET_CLASS_POINTER)
         ENDIF
!
         IF(ALLOCATED(DSM_CLASS_EXPENSES)) &
                           DEALLOCATE(DSM_CLASS_EXPENSES, &
                                      DSM_CLASS_REBATES, &
                                      DSM_CLASS_PURCHASE_POWER, &
                                      DSM_CLASS_SALES_REVENUE, &
                                      DSM_BTL_EXPENSE, &
                                      DSM_BTL_REVENUE, &
                                      DSM_ADJ_CLAUSE_COLLECTIONS, &
                                      DSM_CLASS_CAPITIAL, &
                                      DSM_CLASS_CAPACITY, &
                                      DSM_CLASS_ENERGY, &
                                      DSM_CLASS_PARTICIPANT_COSTS, &
                                      DSM_CLASS_UTIL_NON_ELEC_COSTS, &
                                      DSM_CLASS_THIRD_PARTY_COSTS, &
                                      DSM_CLASS_OTH_PARTICIPANT_COSTS, &
                                      DSM_AFUDC_loc, &
                                      DSM_TAX_EXPENSE_loc, &
                                      RB_DSM_AFUDC_loc, &
                                      RB_DSM_TAX_EXPENSE_loc)
         ALLOCATE(DSM_CLASS_EXPENSES(0:NUM_OF_DSM_CLASSES), &
                  DSM_CLASS_REBATES(0:NUM_OF_DSM_CLASSES), &
                  DSM_CLASS_PURCHASE_POWER(0:NUM_OF_DSM_CLASSES), &
                  DSM_CLASS_SALES_REVENUE(0:NUM_OF_DSM_CLASSES), &
                  DSM_BTL_EXPENSE(0:NUM_OF_DSM_CLASSES), &
                  DSM_BTL_REVENUE(0:NUM_OF_DSM_CLASSES), &
                  DSM_ADJ_CLAUSE_COLLECTIONS(0:NUM_OF_DSM_CLASSES), &
                  DSM_CLASS_CAPITIAL(0:NUM_OF_DSM_CLASSES), &
                  DSM_CLASS_CAPACITY(0:NUM_OF_DSM_CLASSES), &
                  DSM_CLASS_ENERGY(0:NUM_OF_DSM_CLASSES), &
                  DSM_CLASS_PARTICIPANT_COSTS(0:NUM_OF_DSM_CLASSES), &
                  DSM_CLASS_UTIL_NON_ELEC_COSTS(0:NUM_OF_DSM_CLASSES), &
                  DSM_CLASS_THIRD_PARTY_COSTS(0:NUM_OF_DSM_CLASSES), &
                DSM_CLASS_OTH_PARTICIPANT_COSTS(0:NUM_OF_DSM_CLASSES), &
                  DSM_TAX_EXPENSE_loc(0:NUM_OF_DSM_CLASSES), &
                  DSM_AFUDC_loc(0:NUM_OF_DSM_CLASSES), &
                  RB_DSM_AFUDC_loc(0:NUM_OF_DSM_CLASSES), &
                  RB_DSM_TAX_EXPENSE_loc(0:NUM_OF_DSM_CLASSES))
!
         IF(ALLOCATED(ASSET_CLASS_LIST)) &
                      DEALLOCATE(ASSET_CLASS_LIST,ASSET_ALLOCATION_LIST)
         ALLOCATE(ASSET_CLASS_LIST(AVAIL_DATA_YEARS), &
                                ASSET_ALLOCATION_LIST(AVAIL_DATA_YEARS))
      RETURN
! ********************************************************************
      ENTRY INIT_ANNUAL_DSM_ASSET_ARRAYS
! ********************************************************************
         DSM_CLASS_EXPENSES = 0.
         DSM_CLASS_REBATES = 0.
         DSM_CLASS_PURCHASE_POWER = 0.
         DSM_CLASS_SALES_REVENUE = 0.
         DSM_BTL_EXPENSE = 0.
         DSM_BTL_REVENUE = 0.
         DSM_ADJ_CLAUSE_COLLECTIONS = 0.
!
         DSM_CLASS_CAPITIAL = 0.
         DSM_CLASS_CAPACITY = 0.
         DSM_CLASS_ENERGY = 0.
         DSM_CLASS_PARTICIPANT_COSTS = 0.
         DSM_CLASS_UTIL_NON_ELEC_COSTS = 0.
         DSM_CLASS_THIRD_PARTY_COSTS = 0.
         DSM_CLASS_OTH_PARTICIPANT_COSTS = 0.
!
         DSM_AFUDC_loc = 0.
         DSM_TAX_EXPENSE_loc = 0.
         RB_DSM_AFUDC_loc = 0.
         RB_DSM_TAX_EXPENSE_loc = 0.
      RETURN
!
! ********************************************************************
      ENTRY SET_UP_ZERO_STUDY_DSM_ARRAYS
! ********************************************************************
!
         IF(ALLOCATED(DSM_BOOK_DEP_loc)) &
                         DEALLOCATE(DSM_BOOK_DEP_loc,DSM_CUM_BOOK_DEP, &
                                      DSM_GPV_loc, &
                                      DSM_TAX_DEP_loc, &
                                      DSM_AMORT_loc, &
                                      DSM_DEFERRED_DBT, &
                                      DSM_DFRD_TAX_BASIS_loc, &
                                      DSM_TAX_PREF_DEP_loc, &
                                      RB_DSM_AMORT_loc, &
                                      RB_DSM_DFRD_TAX_BASIS_loc, &
                                      DSM_REGULATED_BOOK_DEP_loc, &
                                      DSM_REGULATED_TAX_DEP_loc, &
                                      DSM_REGULATED_DFRD_DBT_loc, &
                                      DSM_REGULATED_NPV_loc, &
                                      DSM_GPV_PROPERTY_TAX_loc, &
                                      DSM_NPV_PROPERTY_TAX_loc, &
                                      DSM_SL_TAX_DEP_loc)
         ALLOCATE(DSM_BOOK_DEP_loc(0:NUM_OF_DSM_CLASSES, &
                                      MAX_FINANCIAL_SIMULATION_YEARS), &
                  DSM_CUM_BOOK_DEP(0:NUM_OF_DSM_CLASSES, &
                                      MAX_FINANCIAL_SIMULATION_YEARS), &
                  DSM_GPV_loc(0:NUM_OF_DSM_CLASSES), &
                  DSM_TAX_DEP_loc(0:NUM_OF_DSM_CLASSES, &
                                      MAX_FINANCIAL_SIMULATION_YEARS), &
                  DSM_AMORT_loc(0:NUM_OF_DSM_CLASSES, &
                                      MAX_FINANCIAL_SIMULATION_YEARS), &
                  DSM_DEFERRED_DBT(0:NUM_OF_DSM_CLASSES), &
                  DSM_DFRD_TAX_BASIS_loc(0:NUM_OF_DSM_CLASSES, &
                                      MAX_FINANCIAL_SIMULATION_YEARS), &
                  DSM_TAX_PREF_DEP_loc(0:NUM_OF_DSM_CLASSES, &
                                      MAX_FINANCIAL_SIMULATION_YEARS), &
                  RB_DSM_AMORT_loc(0:NUM_OF_DSM_CLASSES, &
                                      MAX_FINANCIAL_SIMULATION_YEARS), &
                  RB_DSM_DFRD_TAX_BASIS_loc(0:NUM_OF_DSM_CLASSES, &
                                      MAX_FINANCIAL_SIMULATION_YEARS), &
                  DSM_REGULATED_BOOK_DEP_loc(0:NUM_OF_DSM_CLASSES, &
                                      MAX_FINANCIAL_SIMULATION_YEARS), &
                  DSM_REGULATED_TAX_DEP_loc(0:NUM_OF_DSM_CLASSES, &
                                      MAX_FINANCIAL_SIMULATION_YEARS), &
                  DSM_REGULATED_DFRD_DBT_loc(0:NUM_OF_DSM_CLASSES), &
                  DSM_REGULATED_NPV_loc(0:NUM_OF_DSM_CLASSES), &
                  DSM_GPV_PROPERTY_TAX_loc(0:NUM_OF_DSM_CLASSES, &
                                      MAX_FINANCIAL_SIMULATION_YEARS), &
                  DSM_NPV_PROPERTY_TAX_loc(0:NUM_OF_DSM_CLASSES, &
                                      MAX_FINANCIAL_SIMULATION_YEARS), &
                  DSM_SL_TAX_DEP_loc(0:NUM_OF_DSM_CLASSES, &
                                        MAX_FINANCIAL_SIMULATION_YEARS))
!
         DSM_GPV_loc = 0.
         DSM_DEFERRED_DBT = 0.
         DSM_REGULATED_DFRD_DBT_loc = 0.
         DSM_REGULATED_NPV_loc = 0.
!
         DSM_BOOK_DEP_loc = 0.
         DSM_CUM_BOOK_DEP = 0.
         DSM_TAX_DEP_loc = 0.
         DSM_AMORT_loc = 0.
         DSM_DFRD_TAX_BASIS_loc = 0.
         DSM_TAX_PREF_DEP_loc = 0.
         RB_DSM_AMORT_loc = 0.
         RB_DSM_DFRD_TAX_BASIS_loc = 0.
         DSM_REGULATED_BOOK_DEP_loc = 0.
         DSM_REGULATED_TAX_DEP_loc = 0.
         DSM_GPV_PROPERTY_TAX_loc = 0.
         DSM_NPV_PROPERTY_TAX_loc = 0.
         DSM_SL_TAX_DEP_loc = 0.
      RETURN
! ********************************************************************
      ENTRY DSM_EXPENSE_INFO(R_CLASS,R_CLASS_EXISTS, &
                             R_DSM_EXPENSE, &
                             R_DSM_REBATE, &
                             R_DSM_PURCHASE_POWER, &
                             R_DSM_SALES_REVENUE, &
                             R_DSM_BTL_EXPENSE, &
                             R_DSM_BTL_REVENUE, &
                             R_DSM_ADJ_CLAUSE_COLLECTIONS, &
                             R_DSM_CAPITIAL, &
                             R_DSM_CAPACITY, &
                             R_DSM_ENERGY, &
                             R_DSM_PARTICIPANT_COSTS, &
                             R_DSM_UTIL_NON_ELEC_COSTS, &
                             R_DSM_THIRD_PARTY_COSTS, &
                             R_DSM_OTH_PARTICIPANT_COSTS)
! ********************************************************************
!
         R_CLASS_EXISTS = .FALSE.
         R_DSM_ADJ_CLAUSE_COLLECTIONS = 0.
         R_DSM_CAPITIAL = 0.
         R_DSM_CAPACITY = 0.
         R_DSM_ENERGY = 0.
         R_DSM_PARTICIPANT_COSTS = 0.
         R_DSM_UTIL_NON_ELEC_COSTS = 0.
         R_DSM_THIRD_PARTY_COSTS = 0.
         R_DSM_OTH_PARTICIPANT_COSTS = 0.
         IF(R_CLASS <= MAX_DSM_CLASS_NUM .AND. &
                                     ALLOCATED(DSM_CLASS_EXPENSES)) THEN
            IF(R_CLASS == 0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
               R_CLASS_EXISTS = .TRUE.
               R_DSM_EXPENSE = R_DSM_EXPENSE + &
                                         DSM_CLASS_EXPENSES(ASSET_CLASS)
               R_DSM_REBATE = R_DSM_REBATE + &
                                          DSM_CLASS_REBATES(ASSET_CLASS)
               R_DSM_PURCHASE_POWER = R_DSM_PURCHASE_POWER + &
                                   DSM_CLASS_PURCHASE_POWER(ASSET_CLASS)
               R_DSM_SALES_REVENUE = R_DSM_SALES_REVENUE + &
                                 DSM_CLASS_SALES_REVENUE(ASSET_CLASS)
               R_DSM_BTL_EXPENSE = R_DSM_BTL_EXPENSE + &
                                         DSM_BTL_EXPENSE(ASSET_CLASS)
               R_DSM_BTL_REVENUE = R_DSM_BTL_REVENUE + &
                                            DSM_BTL_REVENUE(ASSET_CLASS)
               R_DSM_ADJ_CLAUSE_COLLECTIONS = &
                             DSM_ADJ_CLAUSE_COLLECTIONS(ASSET_CLASS)
               R_DSM_CAPITIAL = DSM_CLASS_CAPITIAL(ASSET_CLASS)
               R_DSM_CAPACITY = DSM_CLASS_CAPACITY(ASSET_CLASS)
               R_DSM_ENERGY = DSM_CLASS_ENERGY(ASSET_CLASS)
               R_DSM_PARTICIPANT_COSTS = &
                                DSM_CLASS_PARTICIPANT_COSTS(ASSET_CLASS)
               R_DSM_UTIL_NON_ELEC_COSTS = &
                              DSM_CLASS_UTIL_NON_ELEC_COSTS(ASSET_CLASS)
               R_DSM_THIRD_PARTY_COSTS = &
                                DSM_CLASS_THIRD_PARTY_COSTS(ASSET_CLASS)
               R_DSM_OTH_PARTICIPANT_COSTS = &
                            DSM_CLASS_OTH_PARTICIPANT_COSTS(ASSET_CLASS)
            ENDIF
         ENDIF
      RETURN
! ********************************************************************
      ENTRY DSM_ASSET_INFO(R_YR,R_CLASS,R_CLASS_EXISTS, &
                           R_DSM_BOOK_DEP, &
                           R_DSM_CUM_BOOK_DEP, &
                           R_DSM_GPV, &
                           R_DSM_AFUDC, &
                           R_DSM_TAX_DEP, &
                           R_DSM_TAX_EXPENSE, &
                           R_DSM_AMORT, &
                           R_DSM_DEFERRED_DEBIT, &
                           R_DSM_DEFERRED_TAX_BASIS, &
                           R_DSM_TAX_PREF_DEP, &
                           R_RB_DSM_AMORT, &
                           R_RB_DSM_DEFERRED_TAX_BASIS, &
                           R_RB_DSM_AFUDC, &
                           R_RB_DSM_TAX_EXPENSE, &
                           R_DSM_REGULATED_BOOK_DEP, &
                           R_DSM_REGULATED_TAX_DEP, &
                           R_DSM_REGULATED_DEFERRED_DEBIT, &
                           R_DSM_REGULATED_NPV, &
                           R_DSM_GPV_PROPERTY_TAX, &
                           R_DSM_NPV_PROPERTY_TAX, &
                           R_DSM_SL_TAX_DEP)
! ********************************************************************
         R_CLASS_EXISTS = .FALSE.
         R_DSM_CUM_BOOK_DEP = 0.
         R_DSM_AFUDC = 0.
         R_DSM_TAX_EXPENSE = 0.
         R_DSM_DEFERRED_DEBIT = 0.
         R_DSM_TAX_PREF_DEP = 0.
         R_RB_DSM_AMORT = 0.
         R_RB_DSM_DEFERRED_TAX_BASIS = 0.
         R_RB_DSM_AFUDC = 0.
         R_RB_DSM_TAX_EXPENSE = 0.
         R_DSM_REGULATED_BOOK_DEP = 0.
         R_DSM_REGULATED_TAX_DEP = 0.
         R_DSM_REGULATED_DEFERRED_DEBIT = 0.
         R_DSM_REGULATED_NPV = 0.
         R_DSM_GPV_PROPERTY_TAX = 0.
         R_DSM_NPV_PROPERTY_TAX = 0.
         R_DSM_SL_TAX_DEP = 0.
         IF(R_CLASS <= MAX_DSM_CLASS_NUM .AND. &
                                       ALLOCATED(DSM_BOOK_DEP_loc)) THEN
            IF(R_CLASS == 0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
               R_CLASS_EXISTS = .TRUE.
               R_DSM_BOOK_DEP = R_DSM_BOOK_DEP + &
                                      DSM_BOOK_DEP_loc(ASSET_CLASS,R_YR)
               R_DSM_CUM_BOOK_DEP = DSM_CUM_BOOK_DEP(ASSET_CLASS,R_YR)
               R_DSM_GPV = R_DSM_GPV + DSM_GPV_loc(ASSET_CLASS)
               R_DSM_AFUDC = DSM_AFUDC_loc(ASSET_CLASS)
               R_DSM_TAX_EXPENSE = DSM_TAX_EXPENSE_loc(ASSET_CLASS)
               R_DSM_DEFERRED_DEBIT = DSM_DEFERRED_DBT(ASSET_CLASS)
               R_RB_DSM_AFUDC = RB_DSM_AFUDC_loc(ASSET_CLASS)
              R_RB_DSM_TAX_EXPENSE = RB_DSM_TAX_EXPENSE_loc(ASSET_CLASS)
               R_DSM_REGULATED_NPV = DSM_REGULATED_NPV_loc(ASSET_CLASS)
               R_DSM_REGULATED_DEFERRED_DEBIT = &
                               DSM_REGULATED_DFRD_DBT_loc(ASSET_CLASS)
!
           R_DSM_TAX_DEP=R_DSM_TAX_DEP+DSM_TAX_DEP_loc(ASSET_CLASS,R_YR)
             R_DSM_AMORT = R_DSM_AMORT + DSM_AMORT_loc(ASSET_CLASS,R_YR)
               R_DSM_DEFERRED_TAX_BASIS = R_DSM_DEFERRED_TAX_BASIS + &
                                DSM_DFRD_TAX_BASIS_loc(ASSET_CLASS,R_YR)
             R_DSM_TAX_PREF_DEP = DSM_TAX_PREF_DEP_loc(ASSET_CLASS,R_YR)
               R_RB_DSM_AMORT = RB_DSM_AMORT_loc(ASSET_CLASS,R_YR)
               R_RB_DSM_DEFERRED_TAX_BASIS = &
                             RB_DSM_DFRD_TAX_BASIS_loc(ASSET_CLASS,R_YR)
               R_DSM_REGULATED_BOOK_DEP = &
                            DSM_REGULATED_BOOK_DEP_loc(ASSET_CLASS,R_YR)
               R_DSM_GPV_PROPERTY_TAX = &
                             DSM_GPV_PROPERTY_TAX_loc(ASSET_CLASS,R_YR)
               R_DSM_NPV_PROPERTY_TAX = &
                              DSM_NPV_PROPERTY_TAX_loc(ASSET_CLASS,R_YR)
               R_DSM_REGULATED_TAX_DEP = &
                            DSM_REGULATED_TAX_DEP_loc(ASSET_CLASS,R_YR)
               R_DSM_SL_TAX_DEP = DSM_SL_TAX_DEP_loc(ASSET_CLASS,R_YR)
            ENDIF
         ENDIF
      RETURN
      END

