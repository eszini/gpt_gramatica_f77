!     Last change: msg 8/1/2017 10:07:42 AM
!            MSG 6/5/2016 9:12:27 PM



!
!        PROGRAM TO READ MULTI-TAB INFORMATION ON LOADS
!                 AND CONVERT TO BINARY FORMAT
!                       COPYRIGHT (C) 1997-99
!      ALL RIGHTS RESERVED M.S. GERBER & ASSOCIATES, INC.
!

!
      SUBROUTINE TF_OBJECT
      use end_routine, only: end_program, er_message
      use spindriftlib
      use prod_arrays_dimensions
      use sizecom
	  use mod_base_year
      save

      INTEGER*1 FORECAST_GROWTH_YEARS/AVAIL_DATA_YEARS/
      LOGICAL*1 SAVE_TF_FILE_EXISTS/.FALSE./,R_TF_FILE_EXISTS,
     +          R_TF_FILE_USED,SAVE_TF_FILE_USED/.FALSE./
      CHARACTER*6    BASECASE_MKT_AREA_ID_tf1,
     +               BASECASE_TRANS_AREA_ID,
     +               BASECASE_NERC_SUB_ID
      CHARACTER*1 EXTENSION_PERIOD_GROWTH_SWITCH/'X'/,TABLE_ACTIVE,
     +            THREE_FACTOR_TRANSFORM,
     +            JURISDICTIONAL_CUSTOMER,
     +            FUEL_COST_RECOVERY_THROUGH_FAC,
     +            INTERRUPTIBLE_PRICING_ACTIVE
      INTEGER*2   INUNIT,IREC,DELETE,LRECL/1024/,
     +            R_LRECL,
     +            SAVE_TRANS_LOAD_TABLES/0/,R_TRANS_LOAD_TABLES,
     +            TEMP_YEAR,ASSET_CLASS_ID,
     +            R_NUM_OF_TRANS_CLASSES,NUM_OF_OL_TRANS_CLASSES/0/,
     +            R_MAX_TRANS_CLASS_NUM,MAX_OL_TRANS_CLASS_ID_NUM/0/,
     +            R_NUM_OF_ASSET_CLASSES,NUM_OF_OL_ASSET_CLASSES/0/,
     +            R_MAX_ASSET_CLASS_NUM,MAX_OL_ASSET_CLASS_ID_NUM/0/,
     +            R_NUM_OF_CUST_CLASSES,NUM_OF_OL_CUST_CLASSES/0/,
     +            R_MAX_CUST_CLASS_NUM,MAX_OL_CUST_CLASS_ID_NUM/0/,
     +            NUM_OF_BC_TRANS_CLASSES/0/,
     +            MAX_BC_TRANS_CLASS_ID_NUM/0/,
     +            NUM_OF_BC_ASSET_CLASSES/0/,
     +            MAX_BC_ASSET_CLASS_ID_NUM/0/,
     +            NUM_OF_BC_CUST_CLASSES/0/,
     +            MAX_BC_CUST_CLASS_ID_NUM/0/,
     +            TEMP_TRANS_CLASS_POINTER(:),
     +            TEMP_ASSET_CLASS_POINTER(:),
     +            TEMP_CUST_CLASS_POINTER(:),
     +            FILE_TABLE_INDEX,
     +            LOAD_DISPATCH_POSITION
      INTEGER*4 IOS
      ALLOCATABLE ::
     +            TEMP_TRANS_CLASS_POINTER,
     +            TEMP_ASSET_CLASS_POINTER,
     +            TEMP_CUST_CLASS_POINTER
      CHARACTER*5 TRANS_FORECAST_FILE,OVERLAY_FAMILY_NAME,BSYRLOAD
      CHARACTER*256 FILE_NAME
      CHARACTER*256 OUTPUT_DIRECTORY
      CHARACTER*256 BASE_FILE_DIRECTORY
      CHARACTER*152 MESSAGE
      LOGICAL*4 FILE_EXISTS
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER*1024 RECLN
! DECLARATION FOR SYSTEM FORECAST VARIABLES
      INTEGER*2 YEAR
!
! SIMULATION VARIABLES
!
      INTEGER*2

     +                     CUSTOMER_GROUP, ! INT2

     +                     CUSTOMER_GROUP_2,
     +                     TRANSACTION_GROUP, ! INT2
     +                     REFERENCE_LOAD_NUMBER ! INT2
      REAL*4
     +                     MARKET_ENERGY_PRICE, ! REAL4
     +                     MONTHLY_ENERGY_PRICE_PATTERN, ! REAL4
     +                     MARKET_DEMAND_PRICE, ! REAL4
     +                     MONTHLY_DEMAND_PRICE_PATTERN, ! REAL4
     +                     MARKET_CUSTOMER_PRICE, ! REAL4
     +                     ASSET_CLASS_ID_REAL, ! REAL4
     +                     ASSET_CLASS_REV_ALLOC_VECTOR, ! REAL4
     +                     ANNUAL_ENERGY, ! REAL4
     +                     ANNUAL_PEAK, ! REAL4
     +                     ANNUAL_CUSTOMERS, ! REAL4
     +                     ANNUAL_MULTIPLIER, ! REAL4
     +                     MONTHLY_ENERGY(12), ! (12) REAL4
     +                     MONTHLY_PEAK(12), ! (12) REAL4
     +                     MONTHLY_CUSTOMERS(12), ! (12) REAL4
     +                     DIST_ENERGY_LOSS_FACTOR, ! REAL4
     +                     TRANS_ENERGY_LOSS_FACTOR, ! REAL4
     +                     PEAK_LOSS_FACTOR, ! REAL4
     +                     PEAK_COIN_FACTOR, ! REAL4
     +                     DISTRIBUTION_PRICE, ! REAL4
     +                     TRANSMISSION_PRICE, !REAL4!
     +                     MINIMUM_MARKET_PRICE,
     +                     MAXIMUM_MARKET_PRICE,
     +                     INDEXED_ENERGY_PRICE,
     +                     BASE_COST_OF_FAC_FUEL,
     +                     INTERRUPTIBLE_MARKUP_PERENT,
     +                     INTERRUPTIBLE_MARKUP_CAP,
     +                     INTERRUPTIBLE_MAX_CAPACITY,
     +                     INTERRUPTIBLE_MIN_CAPACITY,
     +                     INTERRUPTIBLE_MARKUP_ADDER
      CHARACTER*30
     +                     CUSTOMER_CLASS_NAME, ! CHAR*30
     +                     CALCULATION_MODE*1,CALCULATION_MODE_OUT*1,
     +                     REFERENCE_LOAD_NAME*5, ! CHAR*5
     +                     COMMENT*50, ! CHAR*50
     +                     MONTHLY_UNITS*1, ! ADDED 5/19/98. GAT.
     +                     PRICE_INDEX_ACTIVE*1, ! ADDED 9/30/98. GAT.
     +                     REVENUE_CLASS_ENERGY*50,
     +                     REVENUE_CLASS_DEMAND*50,
     +                     REVENUE_CLASS_CUST*50,
     +                     REVENUE_INDEX_ENERGY*50

      CHARACTER*40         SCENARIO_VARIABLE
!
      CHARACTER*20   TRANS_GROUP_NAME,
     +               RTO_ISO_GROUP_NAME
      CHARACTER*6    PRIMARY_STATE
      INTEGER*2
     +               capacity_market_pointer_tfo
      REAL*4         CAPACITY_MARKET_COIN_ADJ_FACT
      CHARACTER*5
     +               CAPACITY_MARKET_MONTH
      CHARACTER*20   CAPACITY_MARKET_COST_ASSIGN,
     +               CAPACITY_MARKET_EXP_COLLECT
!
      INTEGER HESI_TRANS_AREA_ID_NUM

      CHARACTER*17 FILE_TYPE/'Transact Forecast'/
      LOGICAL*1 LAHEY_LF95
      CHARACTER*30 SCREEN_OUTPUT
      CHARACTER*2 R_TSYFRC_OL
!
      INTEGER*2 TF_YEAR,STUDY_BASE_YEAR


! MULTI-FILE VARIABLES


      INTEGER FILE_NUMBER,FILE_ID
      INTEGER (KIND=4), PARAMETER :: MAX_TF_FILES=17
      INTEGER*2 BC_MASTR_REC,CUR_REC,UNIT_NO
      INTEGER*2 OL_MASTR_REC
      CHARACTER*1 DEMAND_PRICING_METHOD
      CHARACTER*2 TSYFRC_OL/'BC'/,
     +          TF_MULTI_OL_CODES(0:MAX_TF_FILES-1)/MAX_TF_FILES*'BC'/
      CHARACTER*5 TF_FILE_BASE_NAMES(0:MAX_TF_FILES-1),
     +            VOID_CHR,BASE_FILE_NAME,
     +            OVERLAY_FAMILY_NAMES(0:MAX_TF_FILES-1)
      CHARACTER*2 TF_FILE_CODES(0:MAX_TF_FILES-1)/
     +                                'TF','T1','T2','T3','T4','T5',
     +                                'T6','T7','T8','T9','T0',
     +                                'TB','TD','TH','TI','TA','TC'/,
     +            FILE_CODE
      CHARACTER*6 TF_FILE_BINARY_NAMES(0:MAX_TF_FILES-1)/'TSYFCF',
     +                                                   'TSYFC1',
     +                                                   'TSYFC2',
     +                                                   'TSYFC3',
     +                                                   'TSYFC4',
     +                                                   'TSYFC5',
     +                                                   'TSYFC6',
     +                                                   'TSYFC7',
     +                                                   'TSYFC8',
     +                                                   'TSYFC9',
     +                                                   'TSYFC0',
     +                                                   'TSYFCB',
     +                                                   'TSYFCD',
     +                                                   'TSYFCH',
     +                                                   'TSYFCI',
     +                                                   'TSYFCJ',
     +                                                   'TSYFCK'/,
     +            BINARY_FILE_NAME
      LOGICAL ACTIVE_BASE_TF_FILES(0:MAX_TF_FILES-1)/
     +                                           MAX_TF_FILES*.FALSE./,
     +        ACTIVE_OVERLAY_TF_FILES(0:MAX_TF_FILES-1)/
     +                                            MAX_TF_FILES*.FALSE./
      LOGICAL*1 OVERLAY_NAME_ACTIVE(0:MAX_TF_FILES-1),
     +          TF_OVERLAY_MASTR_FILE_OPEN/.FALSE./
      INTEGER*2 INTRA_ASSET_CLASS_ID,INTRA_ASSET_CLASS_ALLOCATION
      CHARACTER*30 INTRA_ACCOUNT_CLASSIFICATION
      CHARACTER*3 INTRA_EXPENSE_COLLECTION
      CHARACTER*1 INTRA_COMPANY_TRANSACTION
      REAL (KIND=4) :: LAST_YR_ENERGY(12),LAST_YR_PEAK(12)



!
!          ROUTINE TO CONVERT METAFILE FILES TO DIRECT-ACESS BINARY
!          COPYRIGHT (C) 1983, 84, 85  M.S. GERBER & ASSOCIATES, INC.
!

!
! CONVERT THE SYSTEM-FORECAST FILE
!
!

      ENTRY TF_MAKEBIN

      STUDY_BASE_YEAR = get_BASE_YEAR()

      STUDY_BASE_YEAR = get_base_year()
      TF_YEAR = 0
      VOID_CHR = TRANS_FORECAST_FILE(TF_FILE_BASE_NAMES)

      IF(.NOT. LAHEY_LF95())
     +       CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
      IF(EXTENSION_PERIOD_GROWTH_SWITCH == 'F')
     +                         FORECAST_GROWTH_YEARS = AVAIL_DATA_YEARS
      BC_MASTR_REC = 0
      SAVE_TRANS_LOAD_TABLES = 0
      DO FILE_ID = 0, MAX_TF_FILES-1

         BASE_FILE_NAME = TF_FILE_BASE_NAMES(FILE_ID)
         FILE_NAME = trim(BASE_FILE_DIRECTORY())//
     +                        trim(TF_FILE_CODES(FILE_ID))//"B"//
     +                                    trim(BASE_FILE_NAME)//".DAT"
         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
         IF(FILE_EXISTS) THEN
!
            IF(.NOT. SAVE_TF_FILE_EXISTS) THEN
               ALLOCATE(TEMP_TRANS_CLASS_POINTER(0:1023),
     +                  TEMP_ASSET_CLASS_POINTER(0:1023),
     +                  TEMP_CUST_CLASS_POINTER(0:1023))
               TEMP_TRANS_CLASS_POINTER = 0
               TEMP_ASSET_CLASS_POINTER = 0
               TEMP_CUST_CLASS_POINTER = 0
!
               OPEN(12,FILE=trim(OUTPUT_DIRECTORY())//"BCTSYFC.BIN",
     +                     ACCESS="DIRECT",STATUS="REPLACE",RECL=LRECL)
               SAVE_TF_FILE_EXISTS = .TRUE.
            ENDIF
!

            IF(LAHEY_LF95()) THEN
               SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
               CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
            ELSE
              CALL MG_LOCATE_WRITE(16,30,BASE_FILE_NAME,ALL_VERSIONS,0)
            ENDIF
!
            ACTIVE_BASE_TF_FILES(FILE_ID) = .TRUE.
            MONTHLY_ENERGY = 0.
            MONTHLY_PEAK = 0.
            MONTHLY_CUSTOMERS = 0.
            OPEN(10,FILE=FILE_NAME)
            OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BC"//
     +                 TF_FILE_BINARY_NAMES(FILE_ID)//".BIN",
     +                     ACCESS="DIRECT",STATUS="REPLACE",RECL=LRECL)
!
            CUSTOMER_GROUP = 0
            CUSTOMER_GROUP_2 = -9999
            CUSTOMER_CLASS_NAME = 'Unassigned'
            CALCULATION_MODE = 'M'
            TRANSACTION_GROUP = 1
            MARKET_ENERGY_PRICE = 0
            MONTHLY_ENERGY_PRICE_PATTERN = 0.
            MARKET_DEMAND_PRICE = 0
            MONTHLY_DEMAND_PRICE_PATTERN = 0.
            MARKET_CUSTOMER_PRICE = 0.
            ASSET_CLASS_ID_REAL = 0.
            ASSET_CLASS_REV_ALLOC_VECTOR = 0.
            REFERENCE_LOAD_NAME = BSYRLOAD()
            REFERENCE_LOAD_NUMBER = 0
            ANNUAL_ENERGY = 8760.
            ANNUAL_PEAK = 1.
            ANNUAL_CUSTOMERS = 1.
            ANNUAL_MULTIPLIER = 1.
            COMMENT = 'None'
            DIST_ENERGY_LOSS_FACTOR = 0.
            TRANS_ENERGY_LOSS_FACTOR = 0.
            PEAK_LOSS_FACTOR = 0.
            PEAK_COIN_FACTOR = 1.
            DISTRIBUTION_PRICE = 0.
            TRANSMISSION_PRICE = 0.
            MINIMUM_MARKET_PRICE = 0.
            MAXIMUM_MARKET_PRICE = 999999.
            INDEXED_ENERGY_PRICE = 0.
!
            INTERRUPTIBLE_PRICING_ACTIVE = 'F'
            INTERRUPTIBLE_MARKUP_PERENT = 0.
            INTERRUPTIBLE_MARKUP_CAP = 999999.
            INTERRUPTIBLE_MAX_CAPACITY = 0.
            INTERRUPTIBLE_MIN_CAPACITY = 999999.
            INTERRUPTIBLE_MARKUP_ADDER = 0.
!
!            SAVE_TRANS_LOAD_TABLES = 0 moved outside file loop 4/17/01
!
            TABLE_ACTIVE = 'T'
            BASECASE_MKT_AREA_ID_tf1 = 'BLANK ' ! CHAR*6
            BASECASE_TRANS_AREA_ID = 'BLANK ' ! CHAR*6
            BASECASE_NERC_SUB_ID = 'BLANK ' ! CHAR*6
            MONTHLY_UNITS = 'E'
            PRICE_INDEX_ACTIVE = 'F'
            REVENUE_CLASS_ENERGY = 'Secondary Sales'
            REVENUE_CLASS_DEMAND = 'CapacitySales'
            REVENUE_CLASS_CUST = 'Customer Revenues'
            REVENUE_INDEX_ENERGY = 'Residential Sales'
            DEMAND_PRICING_METHOD = 'M'
            SCENARIO_VARIABLE =
     +                       '                                        '
            THREE_FACTOR_TRANSFORM = 'F'
            LOAD_DISPATCH_POSITION = -99
!
            JURISDICTIONAL_CUSTOMER = 'N'
            FUEL_COST_RECOVERY_THROUGH_FAC = 'N'
            BASE_COST_OF_FAC_FUEL = 0.
!
            capacity_market_pointer_tfo = 0
            CAPACITY_MARKET_COIN_ADJ_FACT = 1.0
            CAPACITY_MARKET_MONTH = 'JUL'
            CAPACITY_MARKET_COST_ASSIGN = 'NONE'
            CAPACITY_MARKET_EXP_COLLECT = 'Base Rates'
!
            IREC = 0
            READ(10,*) DELETE
            DO ! TABLES
               TF_YEAR = 1
               INTRA_COMPANY_TRANSACTION = 'N'
               INTRA_ASSET_CLASS_ID = 0
               INTRA_ASSET_CLASS_ALLOCATION = 0
               INTRA_ACCOUNT_CLASSIFICATION = ' '
               INTRA_EXPENSE_COLLECTION = 'ATL'
               DO ! YEAR-BASED RECORDS
                  READ(10,1000,IOSTAT=IOS) RECLN

                  ! END OF TABLE ! EXIT AT BOTTOM OF IF
                  IF(RECLN(1:1) == '7' .OR. IOS /= 0) THEN
                     IF(TF_YEAR <= AVAIL_DATA_YEARS) THEN
                        DO TF_YEAR = TF_YEAR, AVAIL_DATA_YEARS
                           DO UNIT_NO = 11, 12
                              IF(UNIT_NO == 11) THEN
                                 IREC = IREC + 1
                                 CUR_REC = IREC
                              ELSE
                                 BC_MASTR_REC = BC_MASTR_REC + 1
                                 CUR_REC = BC_MASTR_REC
                              ENDIF
!
                              FILE_TABLE_INDEX = (FILE_ID+1)*1000 +
     +                                                   CUSTOMER_GROUP

                              WRITE(UNIT_NO,REC=CUR_REC) DELETE,
     +                           TF_YEAR+STUDY_BASE_YEAR, ! INT2
     +                           CUSTOMER_GROUP, ! INT2
     +                           CUSTOMER_CLASS_NAME, ! CHAR*30
     +                           CALCULATION_MODE_OUT, ! CHAR*1
     +                           TRANSACTION_GROUP, ! INT2
     +                           MARKET_ENERGY_PRICE, ! REAL4
     +                           MONTHLY_ENERGY_PRICE_PATTERN, ! REAL4
     +                           MARKET_DEMAND_PRICE, ! REAL4
     +                           MONTHLY_DEMAND_PRICE_PATTERN, ! REAL4
     +                           MARKET_CUSTOMER_PRICE, ! REAL4
     +                           ASSET_CLASS_ID_REAL, ! REAL4
     +                           ASSET_CLASS_REV_ALLOC_VECTOR, ! REAL4
     +                           REFERENCE_LOAD_NAME, ! CHAR*5
     +                           REFERENCE_LOAD_NUMBER, ! INT2
     +                           ANNUAL_ENERGY, ! REAL4
     +                           ANNUAL_PEAK, ! REAL4
     +                           ANNUAL_CUSTOMERS, ! REAL4
     +                           ANNUAL_MULTIPLIER, ! REAL4
     +                           MONTHLY_ENERGY, ! (12) REAL4
     +                           MONTHLY_PEAK, ! (12) REAL4
     +                           MONTHLY_CUSTOMERS, ! (12) REAL4
!     +                           COMMENT, ! CHAR*50
     +                           DIST_ENERGY_LOSS_FACTOR, ! REAL4
     +                           TRANS_ENERGY_LOSS_FACTOR, ! REAL4
     +                           PEAK_LOSS_FACTOR, ! REAL4
     +                           PEAK_COIN_FACTOR, ! REAL4
     +                           DISTRIBUTION_PRICE, ! REAL4
     +                           TRANSMISSION_PRICE, !REAL4
     +                           TABLE_ACTIVE,
     +                           BASECASE_MKT_AREA_ID_tf1, ! CHAR*6
     +                           BASECASE_TRANS_AREA_ID, ! CHAR*6
     +                           BASECASE_NERC_SUB_ID, ! CHAR*6
     +                           MONTHLY_UNITS,
     +                           MINIMUM_MARKET_PRICE,
     +                           MAXIMUM_MARKET_PRICE,
     +                           INDEXED_ENERGY_PRICE,
     +                           PRICE_INDEX_ACTIVE,
     +                           HESI_TRANS_AREA_ID_NUM,
     +                           REVENUE_CLASS_ENERGY,
     +                           REVENUE_CLASS_DEMAND,
     +                           REVENUE_CLASS_CUST,
     +                           REVENUE_INDEX_ENERGY,
     +                           DEMAND_PRICING_METHOD,
     +                           INTRA_COMPANY_TRANSACTION,
     +                           INTRA_ASSET_CLASS_ID,
     +                           INTRA_ASSET_CLASS_ALLOCATION,
     +                           INTRA_ACCOUNT_CLASSIFICATION,
     +                           INTRA_EXPENSE_COLLECTION,
     +                           FILE_TABLE_INDEX,
     +                           SCENARIO_VARIABLE,
     +                           LOAD_DISPATCH_POSITION,
     +                           THREE_FACTOR_TRANSFORM,
     +                           JURISDICTIONAL_CUSTOMER,
     +                           FUEL_COST_RECOVERY_THROUGH_FAC,
     +                           BASE_COST_OF_FAC_FUEL,
     +                           CUSTOMER_GROUP_2,
     +                           INTERRUPTIBLE_PRICING_ACTIVE,
     +                           INTERRUPTIBLE_MARKUP_PERENT,
     +                           INTERRUPTIBLE_MARKUP_CAP,
     +                           INTERRUPTIBLE_MAX_CAPACITY,
     +                           INTERRUPTIBLE_MIN_CAPACITY,
     +                           INTERRUPTIBLE_MARKUP_ADDER,
     +                           TRANS_GROUP_NAME,             ! 95
     +                           RTO_ISO_GROUP_NAME,
     +                           PRIMARY_STATE,
     +                           CAPACITY_MARKET_MONTH,
     +                           capacity_market_pointer_tfo,
     +                           CAPACITY_MARKET_COST_ASSIGN,
     +                           CAPACITY_MARKET_EXP_COLLECT,
     +                           CAPACITY_MARKET_COIN_ADJ_FACT

                           ENDDO
                        ENDDO
                     ENDIF ! DETECTED NEW TABLE OR END OF FILE
!
                   EXIT ! LEAVE LOOP (GO TO NEXT TABLE OR EXIT ROUTINE)
!
                  ENDIF
                  RECLN=trim(RECLN)//
     +                      ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
                  READ(RECLN,*,iostat=ios) DELETE,
     +                        YEAR, ! INT2
     +                        CUSTOMER_GROUP, ! INT2
     +                        CUSTOMER_CLASS_NAME, ! CHAR*30
     +                        CALCULATION_MODE, ! CHAR*1
     +                        TRANSACTION_GROUP, ! INT2
     +                        MARKET_ENERGY_PRICE, ! REAL4
     +                        MONTHLY_ENERGY_PRICE_PATTERN, ! REAL4
     +                        MARKET_DEMAND_PRICE, ! REAL4
     +                        MONTHLY_DEMAND_PRICE_PATTERN, ! REAL4
     +                        MARKET_CUSTOMER_PRICE, ! REAL4
     +                        ASSET_CLASS_ID_REAL, ! REAL4
     +                        ASSET_CLASS_REV_ALLOC_VECTOR, ! REAL4
     +                        REFERENCE_LOAD_NAME, ! CHAR*5
     +                        REFERENCE_LOAD_NUMBER, ! INT2
     +                        ANNUAL_ENERGY, ! REAL4
     +                        ANNUAL_PEAK, ! REAL4
     +                        ANNUAL_CUSTOMERS, ! REAL4
     +                        ANNUAL_MULTIPLIER, ! REAL4
     +                        MONTHLY_ENERGY, ! (12) REAL4
     +                        MONTHLY_PEAK, ! (12) REAL4
     +                        MONTHLY_CUSTOMERS, ! (12) REAL4
     +                        COMMENT, ! CHAR*50
     +                        DIST_ENERGY_LOSS_FACTOR, ! REAL4
     +                        TRANS_ENERGY_LOSS_FACTOR, ! REAL4
     +                        PEAK_LOSS_FACTOR, ! REAL4
     +                        PEAK_COIN_FACTOR, ! REAL4
     +                        DISTRIBUTION_PRICE, ! REAL4
     +                        TRANSMISSION_PRICE, !REAL4
     +                        TABLE_ACTIVE,
     +                        BASECASE_MKT_AREA_ID_tf1, ! CHAR*6
     +                        BASECASE_TRANS_AREA_ID, ! CHAR*6
     +                        BASECASE_NERC_SUB_ID, ! CHAR*6
     +                        MONTHLY_UNITS,
     +                        MINIMUM_MARKET_PRICE,
     +                        MAXIMUM_MARKET_PRICE,
     +                        INDEXED_ENERGY_PRICE,
     +                        PRICE_INDEX_ACTIVE,
     +                        HESI_TRANS_AREA_ID_NUM,
     +                        REVENUE_CLASS_ENERGY,
     +                        REVENUE_CLASS_DEMAND,
     +                        REVENUE_CLASS_CUST,
     +                        REVENUE_INDEX_ENERGY,
     +                        DEMAND_PRICING_METHOD, ! 76
! added 11/6/02 MSG for FE
     +                        INTRA_COMPANY_TRANSACTION,
     +                        INTRA_ASSET_CLASS_ID,
     +                        INTRA_ASSET_CLASS_ALLOCATION,
     +                        INTRA_ACCOUNT_CLASSIFICATION,
     +                        INTRA_EXPENSE_COLLECTION,
     +                        SCENARIO_VARIABLE,
     +                        LOAD_DISPATCH_POSITION,
     +                        THREE_FACTOR_TRANSFORM,
     +                        JURISDICTIONAL_CUSTOMER,
     +                        FUEL_COST_RECOVERY_THROUGH_FAC,
     +                        BASE_COST_OF_FAC_FUEL,
     +                        CUSTOMER_GROUP_2,
     +                        INTERRUPTIBLE_PRICING_ACTIVE,
     +                        INTERRUPTIBLE_MARKUP_PERENT,
     +                        INTERRUPTIBLE_MARKUP_CAP,
     +                        INTERRUPTIBLE_MAX_CAPACITY,
     +                        INTERRUPTIBLE_MIN_CAPACITY,
     +                        INTERRUPTIBLE_MARKUP_ADDER,
     +                        TRANS_GROUP_NAME,             ! 95
     +                        RTO_ISO_GROUP_NAME,
     +                        PRIMARY_STATE,
     +                        CAPACITY_MARKET_MONTH,
     +                        capacity_market_pointer_tfo,
     +                        CAPACITY_MARKET_COST_ASSIGN,
     +                        CAPACITY_MARKET_EXP_COLLECT,
     +                        CAPACITY_MARKET_COIN_ADJ_FACT
!
! CONVERT GROWTH RATE TO VALUES, SAVE PRIOR YEAR 2/12/11 MSG
       if(ios/=0) then
        er_message="tf_objt:0003 - error reading file."
        call end_program(er_message)
       endif
                  CALCULATION_MODE_OUT = CALCULATION_MODE
                  IF(CALCULATION_MODE == 'G') THEN

                     MONTHLY_ENERGY = (1. + MONTHLY_ENERGY/100.)
     +                                                 * LAST_YR_ENERGY
                     MONTHLY_PEAK = (1.+MONTHLY_PEAK/100.)
     +                                                   * LAST_YR_PEAK
                     ! C is treated the same as M
                     CALCULATION_MODE_OUT = 'C'
                  ENDIF
                  LAST_YR_ENERGY = MONTHLY_ENERGY
                  LAST_YR_PEAK = MONTHLY_PEAK
!
                  IREC = IREC + 1
!
                  READ(RECLN,*,iostat=ios) DELETE,YEAR
                 TEMP_YEAR = MIN(AVAIL_DATA_YEARS,YEAR-STUDY_BASE_YEAR)
!

                  ASSET_CLASS_ID = ASSET_CLASS_ID_REAL
                 IF(TEMP_TRANS_CLASS_POINTER(TRANSACTION_GROUP)==0)THEN
                    NUM_OF_BC_TRANS_CLASSES = NUM_OF_BC_TRANS_CLASSES+1
                     MAX_BC_TRANS_CLASS_ID_NUM = MAX(
     +                     MAX_BC_TRANS_CLASS_ID_NUM,TRANSACTION_GROUP)
                     TEMP_TRANS_CLASS_POINTER(TRANSACTION_GROUP) = 1
                  ENDIF
                 IF(TEMP_ASSET_CLASS_POINTER(ASSET_CLASS_ID) == 0) THEN
                    NUM_OF_BC_ASSET_CLASSES = NUM_OF_BC_ASSET_CLASSES+1
                     MAX_BC_ASSET_CLASS_ID_NUM = MAX(
     +                        MAX_BC_ASSET_CLASS_ID_NUM,ASSET_CLASS_ID)
                     TEMP_ASSET_CLASS_POINTER(ASSET_CLASS_ID) = 1
                  ENDIF
                  IF(INTRA_COMPANY_TRANSACTION == 'Y') THEN
                     IF(TEMP_ASSET_CLASS_POINTER(INTRA_ASSET_CLASS_ID)
     +                                                       == 0) THEN
                        NUM_OF_BC_ASSET_CLASSES =
     +                                      NUM_OF_BC_ASSET_CLASSES + 1
                        MAX_BC_ASSET_CLASS_ID_NUM = MAX(
     +                  MAX_BC_ASSET_CLASS_ID_NUM,INTRA_ASSET_CLASS_ID)
                       TEMP_ASSET_CLASS_POINTER(INTRA_ASSET_CLASS_ID)=1
                     ENDIF
                  ENDIF
                  IF(TEMP_CUST_CLASS_POINTER(CUSTOMER_GROUP) == 0) THEN
                    NUM_OF_BC_CUST_CLASSES = NUM_OF_BC_CUST_CLASSES + 1
                     MAX_BC_CUST_CLASS_ID_NUM = MAX(
     +                         MAX_BC_CUST_CLASS_ID_NUM,CUSTOMER_GROUP)
                     TEMP_CUST_CLASS_POINTER(CUSTOMER_GROUP) = 1
                  ENDIF
!
                  IF(TEMP_YEAR > TF_YEAR) THEN ! FILL IN MISSING YEARS
                     IF(IREC == 1) THEN
                       WRITE(4,*)"The first year of the first table in"
                        WRITE(4,*)"the Transact Forecast file is ",YEAR
                        WRITE(4,*)"while the base year set in project"
                        WRITE(4,*)"information is ",STUDY_BASE_YEAR
                        WRITE(4,*)"First forecast year in the Transact"
                       WRITE(4,*)"Forecast file first year must be one"
                        WRITE(4,*)"year greater than the base year."
                        WRITE(4,*)" "
            er_message='tf_objt:0002 - Year mismatch see WARNINGS file.'
                        call end_program(er_message)
                     ENDIF
                     DO TF_YEAR = TF_YEAR, TEMP_YEAR - 1
                        BC_MASTR_REC = BC_MASTR_REC + 1
                        DO UNIT_NO = 11, 12
                           IF(UNIT_NO == 11) THEN
                              CUR_REC = IREC
                           ELSE
                              CUR_REC = BC_MASTR_REC
                           ENDIF
!
                           FILE_TABLE_INDEX = (FILE_ID+1)*1000 +
     +                                                   CUSTOMER_GROUP
!
                           WRITE(UNIT_NO,REC=CUR_REC) DELETE,
     +                        TF_YEAR+STUDY_BASE_YEAR, ! INT2
     +                        CUSTOMER_GROUP, ! INT2
     +                        CUSTOMER_CLASS_NAME, ! CHAR*30
     +                        CALCULATION_MODE_OUT, ! CHAR*1
     +                        TRANSACTION_GROUP, ! INT2
     +                        MARKET_ENERGY_PRICE, ! REAL4
     +                        MONTHLY_ENERGY_PRICE_PATTERN, ! REAL4
     +                        MARKET_DEMAND_PRICE, ! REAL4
     +                        MONTHLY_DEMAND_PRICE_PATTERN, ! REAL4
     +                        MARKET_CUSTOMER_PRICE, ! REAL4
     +                        ASSET_CLASS_ID_REAL, ! REAL4
     +                        ASSET_CLASS_REV_ALLOC_VECTOR, ! REAL4
     +                        REFERENCE_LOAD_NAME, ! CHAR*5
     +                        REFERENCE_LOAD_NUMBER, ! INT2
     +                        ANNUAL_ENERGY, ! REAL4
     +                        ANNUAL_PEAK, ! REAL4
     +                        ANNUAL_CUSTOMERS, ! REAL4
     +                        ANNUAL_MULTIPLIER, ! REAL4
     +                        MONTHLY_ENERGY, ! (12) REAL4
     +                        MONTHLY_PEAK, ! (12) REAL4
     +                        MONTHLY_CUSTOMERS, ! (12) REAL4
!     +                        COMMENT, ! CHAR*50
     +                        DIST_ENERGY_LOSS_FACTOR, ! REAL4
     +                        TRANS_ENERGY_LOSS_FACTOR, ! REAL4
     +                        PEAK_LOSS_FACTOR, ! REAL4
     +                        PEAK_COIN_FACTOR, ! REAL4
     +                        DISTRIBUTION_PRICE, ! REAL4
     +                        TRANSMISSION_PRICE, !REAL4
     +                        TABLE_ACTIVE,
     +                        BASECASE_MKT_AREA_ID_tf1, ! CHAR*6
     +                        BASECASE_TRANS_AREA_ID, ! CHAR*6
     +                        BASECASE_NERC_SUB_ID, ! CHAR*6
     +                        MONTHLY_UNITS,
     +                        MINIMUM_MARKET_PRICE,
     +                        MAXIMUM_MARKET_PRICE,
     +                        INDEXED_ENERGY_PRICE,
     +                        PRICE_INDEX_ACTIVE,
     +                        HESI_TRANS_AREA_ID_NUM,
     +                        REVENUE_CLASS_ENERGY,
     +                        REVENUE_CLASS_DEMAND,
     +                        REVENUE_CLASS_CUST,
     +                        REVENUE_INDEX_ENERGY,
     +                        DEMAND_PRICING_METHOD,
     +                        INTRA_COMPANY_TRANSACTION,
     +                        INTRA_ASSET_CLASS_ID,
     +                        INTRA_ASSET_CLASS_ALLOCATION,
     +                        INTRA_ACCOUNT_CLASSIFICATION,
     +                        INTRA_EXPENSE_COLLECTION,
     +                        FILE_TABLE_INDEX,
     +                        SCENARIO_VARIABLE,
     +                        LOAD_DISPATCH_POSITION,
     +                        THREE_FACTOR_TRANSFORM,
     +                        JURISDICTIONAL_CUSTOMER,
     +                        FUEL_COST_RECOVERY_THROUGH_FAC,
     +                        BASE_COST_OF_FAC_FUEL,
     +                        CUSTOMER_GROUP_2,
     +                        INTERRUPTIBLE_PRICING_ACTIVE,
     +                        INTERRUPTIBLE_MARKUP_PERENT,
     +                        INTERRUPTIBLE_MARKUP_CAP,
     +                        INTERRUPTIBLE_MAX_CAPACITY,
     +                        INTERRUPTIBLE_MIN_CAPACITY,
     +                        INTERRUPTIBLE_MARKUP_ADDER,
     +                        TRANS_GROUP_NAME,             ! 95
     +                        RTO_ISO_GROUP_NAME,
     +                        PRIMARY_STATE,
     +                        CAPACITY_MARKET_MONTH,
     +                        capacity_market_pointer_tfo,
     +                        CAPACITY_MARKET_COST_ASSIGN,
     +                        CAPACITY_MARKET_EXP_COLLECT,
     +                        CAPACITY_MARKET_COIN_ADJ_FACT
                        ENDDO
                        IREC = IREC + 1
                     ENDDO


                  ENDIF


                  DO UNIT_NO = 11, 12
                     IF(UNIT_NO == 11) THEN
                        CUR_REC = IREC
                     ELSE
                        BC_MASTR_REC = BC_MASTR_REC + 1
                        CUR_REC = BC_MASTR_REC
                     ENDIF
!
                     FILE_TABLE_INDEX = (FILE_ID+1)*1000 +
     +                                                   CUSTOMER_GROUP
!
                     WRITE(UNIT_NO,REC=CUR_REC) DELETE,
     +                        TF_YEAR+STUDY_BASE_YEAR, ! INT2
     +                        CUSTOMER_GROUP, ! INT2
     +                        CUSTOMER_CLASS_NAME, ! CHAR*30
     +                        CALCULATION_MODE_OUT, ! CHAR*1
     +                        TRANSACTION_GROUP, ! INT2
     +                        MARKET_ENERGY_PRICE, ! REAL4
     +                        MONTHLY_ENERGY_PRICE_PATTERN, ! REAL4
     +                        MARKET_DEMAND_PRICE, ! REAL4
     +                        MONTHLY_DEMAND_PRICE_PATTERN, ! REAL4
     +                        MARKET_CUSTOMER_PRICE, ! REAL4
     +                        ASSET_CLASS_ID_REAL, ! REAL4
     +                        ASSET_CLASS_REV_ALLOC_VECTOR, ! REAL4
     +                        REFERENCE_LOAD_NAME, ! CHAR*5
     +                        REFERENCE_LOAD_NUMBER, ! INT2
     +                        ANNUAL_ENERGY, ! REAL4
     +                        ANNUAL_PEAK, ! REAL4
     +                        ANNUAL_CUSTOMERS, ! REAL4
     +                        ANNUAL_MULTIPLIER, ! REAL4
     +                        MONTHLY_ENERGY, ! (12) REAL4
     +                        MONTHLY_PEAK, ! (12) REAL4
     +                        MONTHLY_CUSTOMERS, ! (12) REAL4
!     +                        COMMENT, ! CHAR*50
     +                        DIST_ENERGY_LOSS_FACTOR, ! REAL4
     +                        TRANS_ENERGY_LOSS_FACTOR, ! REAL4
     +                        PEAK_LOSS_FACTOR, ! REAL4
     +                        PEAK_COIN_FACTOR, ! REAL4
     +                        DISTRIBUTION_PRICE, ! REAL4
     +                        TRANSMISSION_PRICE, !REAL4
     +                        TABLE_ACTIVE,
     +                        BASECASE_MKT_AREA_ID_tf1, ! CHAR*6
     +                        BASECASE_TRANS_AREA_ID, ! CHAR*6
     +                        BASECASE_NERC_SUB_ID, ! CHAR*6
     +                        MONTHLY_UNITS,
     +                        MINIMUM_MARKET_PRICE,
     +                        MAXIMUM_MARKET_PRICE,
     +                        INDEXED_ENERGY_PRICE,
     +                        PRICE_INDEX_ACTIVE,
     +                        HESI_TRANS_AREA_ID_NUM,
     +                        REVENUE_CLASS_ENERGY,
     +                        REVENUE_CLASS_DEMAND,
     +                        REVENUE_CLASS_CUST,
     +                        REVENUE_INDEX_ENERGY,
     +                        DEMAND_PRICING_METHOD,
     +                        INTRA_COMPANY_TRANSACTION,
     +                        INTRA_ASSET_CLASS_ID,
     +                        INTRA_ASSET_CLASS_ALLOCATION,
     +                        INTRA_ACCOUNT_CLASSIFICATION,
     +                        INTRA_EXPENSE_COLLECTION,
     +                        FILE_TABLE_INDEX,
     +                        SCENARIO_VARIABLE,
     +                        LOAD_DISPATCH_POSITION,
     +                        THREE_FACTOR_TRANSFORM,
     +                        JURISDICTIONAL_CUSTOMER,
     +                        FUEL_COST_RECOVERY_THROUGH_FAC,
     +                        BASE_COST_OF_FAC_FUEL,
     +                        CUSTOMER_GROUP_2,
     +                        INTERRUPTIBLE_PRICING_ACTIVE,
     +                        INTERRUPTIBLE_MARKUP_PERENT,
     +                        INTERRUPTIBLE_MARKUP_CAP,
     +                        INTERRUPTIBLE_MAX_CAPACITY,
     +                        INTERRUPTIBLE_MIN_CAPACITY,
     +                        INTERRUPTIBLE_MARKUP_ADDER,
     +                        TRANS_GROUP_NAME,             ! 95
     +                        RTO_ISO_GROUP_NAME,
     +                        PRIMARY_STATE,
     +                        CAPACITY_MARKET_MONTH,
     +                        capacity_market_pointer_tfo,
     +                        CAPACITY_MARKET_COST_ASSIGN,
     +                        CAPACITY_MARKET_EXP_COLLECT,
     +                        CAPACITY_MARKET_COIN_ADJ_FACT
                  ENDDO ! WRITE TO UNITS 11 & 12
                  TF_YEAR = TF_YEAR + 1
!                  CALL CALCULATE_TSYTEM_PEAKS(TSY_FC_DATA,TF_YEAR)
               ENDDO ! LOAD GROUP
               SAVE_TRANS_LOAD_TABLES = SAVE_TRANS_LOAD_TABLES + 1
               IF(IOS /= 0) EXIT
            ENDDO ! READ TABLES
            CLOSE(10)
            CLOSE(11)
         ELSE IF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
            CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "tf_objt:1113")
         ENDIF ! FILE EXISTS
      ENDDO ! FILE LOOP
      IF(.NOT. SAVE_TF_FILE_EXISTS) THEN
         OPEN(12,FILE=trim(OUTPUT_DIRECTORY())//"BCTSYFC.BIN",
     +                     ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
!
         SAVE_TF_FILE_EXISTS = .FALSE.
!
      ENDIF
      CLOSE(12)
      IF(ALLOCATED(TEMP_TRANS_CLASS_POINTER))
     +      DEALLOCATE(TEMP_TRANS_CLASS_POINTER,
     +                 TEMP_ASSET_CLASS_POINTER,
     +                 TEMP_CUST_CLASS_POINTER)
      RETURN





!
!          ROUTINE TO CREATE OVERLAY FILES
!          COPYRIGHT (C) 1984-88  M.S. GERBER & ASSOCIATES, INC.
!          COPYRIGHT (C) 1991-92  M.S. GERBER & ASSOCIATES, INC.
!

!
! OVERLAY THE SYSTEM-FORECAST FILE

      ENTRY TF_MAKEOVL(OVERLAY_FAMILY_NAMES,OVERLAY_NAME_ACTIVE)


      CALL LOCATE(10,51)
      DO FILE_ID = 0, MAX_TF_FILES-1
         IF(ACTIVE_BASE_TF_FILES(FILE_ID)) THEN
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
            EXIT
         ENDIF
      ENDDO
      DO FILE_ID = 0, MAX_TF_FILES-1
         IF(.NOT. ACTIVE_BASE_TF_FILES(FILE_ID)) CYCLE
         IF(.NOT. TF_OVERLAY_MASTR_FILE_OPEN) THEN
            OPEN(13,FILE=trim(OUTPUT_DIRECTORY())//"OLTSYFC.BIN",
     +                     STATUS='REPLACE',ACCESS="DIRECT",RECL=LRECL)
            TF_OVERLAY_MASTR_FILE_OPEN = .TRUE.
            OL_MASTR_REC = 0
            TSYFRC_OL = 'OL' ! AT LEAST ONE OVERLAY FIEL WAS USED
            NUM_OF_OL_TRANS_CLASSES = 0
            MAX_OL_TRANS_CLASS_ID_NUM = 0
            NUM_OF_OL_ASSET_CLASSES = 0
            MAX_OL_ASSET_CLASS_ID_NUM = 0
            NUM_OF_OL_CUST_CLASSES = 0
!
            MAX_OL_CUST_CLASS_ID_NUM = 0
!
!
            ALLOCATE(TEMP_TRANS_CLASS_POINTER(0:1023),
     +               TEMP_ASSET_CLASS_POINTER(0:1023),
     +               TEMP_CUST_CLASS_POINTER(0:1023))
            TEMP_TRANS_CLASS_POINTER = 0
            TEMP_ASSET_CLASS_POINTER = 0
            TEMP_CUST_CLASS_POINTER = 0
         ENDIF

         IF(.NOT. OVERLAY_NAME_ACTIVE(FILE_ID)) THEN
! WRITE THE CURRENT STATE OF THE BINARY FILE TO THE MASTER OL FILE
            OPEN(10,FILE=trim(OUTPUT_DIRECTORY())//
     +              TF_MULTI_OL_CODES(FILE_ID)//
     +                  TF_FILE_BINARY_NAMES(FILE_ID)//".BIN",
     +                     STATUS="UNKNOWN",ACCESS="DIRECT",RECL=LRECL)
            IREC = 0
            DO
               IREC = IREC + 1
               READ(10,REC=IREC,IOSTAT=IOS) RECLN(1:LRECL)
               IF(IOS /= 0) EXIT
               OL_MASTR_REC = OL_MASTR_REC + 1
               WRITE(13,REC=OL_MASTR_REC) RECLN(1:LRECL)
            ENDDO
            CLOSE(10)
      ! CREATE INDIVIDUAL BINARY OVERLAY FILE AND ADD TO MASTER OL FILE

         ELSE

            FILE_NAME=trim(OUTPUT_DIRECTORY())//
     +              trim(TF_FILE_CODES(FILE_ID))//"O"//
     +                     trim(OVERLAY_FAMILY_NAMES(FILE_ID))//".DAT"
            OPEN(10,FILE=FILE_NAME)
            READ(10,*) DELETE
            TF_YEAR = 0
            INUNIT = 12
            IF(TF_MULTI_OL_CODES(FILE_ID) == 'BC') THEN
               OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BC"//
     +                   TF_FILE_BINARY_NAMES(FILE_ID)//".BIN",
     +                                      ACCESS="DIRECT",RECL=LRECL)
               INUNIT = 11
            ENDIF
!
            OPEN(12,FILE=trim(OUTPUT_DIRECTORY())//"OL"//
     +                   TF_FILE_BINARY_NAMES(FILE_ID)//".BIN",
     +          STATUS="UNKNOWN",ACCESS="DIRECT",RECL=LRECL,IOSTAT=IOS)
            IF(IOS /= 0) THEN
               CALL IOSTAT_MSG(IOS,MESSAGE)
               WRITE(4,*) trim(MESSAGE)
               WRITE(4,*) '*** line 189 TF_OBJT.FOR ***'
               er_message='See WARNING MESSAGES -tf_objt.for-1'
               call end_program(er_message)
            ENDIF
            IREC = 0
            DELETE = 1
            READ(10,1000,IOSTAT=IOS) RECLN
!
            DOWHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE
!
               READ(10,1000,IOSTAT=IOS) RECLN
!
            ENDDO

            DO ! TABLES AND YEARS, COUNTING IS DONE INSIDE
               IREC = IREC + 1
               READ(INUNIT,REC=IREC,IOSTAT=IOS)
     +                     DELETE,
     +                     YEAR, ! INT2
     +                     CUSTOMER_GROUP, ! INT2
     +                     CUSTOMER_CLASS_NAME, ! CHAR*30
     +                     CALCULATION_MODE, ! CHAR*1
     +                     TRANSACTION_GROUP, ! INT2
     +                     MARKET_ENERGY_PRICE, ! REAL4
     +                     MONTHLY_ENERGY_PRICE_PATTERN, ! REAL4
     +                     MARKET_DEMAND_PRICE, ! REAL4
     +                     MONTHLY_DEMAND_PRICE_PATTERN, ! REAL4
     +                     MARKET_CUSTOMER_PRICE, ! REAL4
     +                     ASSET_CLASS_ID_REAL, ! REAL4
     +                     ASSET_CLASS_REV_ALLOC_VECTOR, ! REAL4
     +                     REFERENCE_LOAD_NAME, ! CHAR*5
     +                     REFERENCE_LOAD_NUMBER, ! INT2
     +                     ANNUAL_ENERGY, ! REAL4
     +                     ANNUAL_PEAK, ! REAL4
     +                     ANNUAL_CUSTOMERS, ! REAL4
     +                     ANNUAL_MULTIPLIER, ! REAL4
     +                     MONTHLY_ENERGY, ! (12) REAL4
     +                     MONTHLY_PEAK, ! (12) REAL4
     +                     MONTHLY_CUSTOMERS, ! (12) REAL4
 !    +                     COMMENT, ! CHAR*50
     +                     DIST_ENERGY_LOSS_FACTOR, ! REAL4
     +                     TRANS_ENERGY_LOSS_FACTOR, ! REAL4
     +                     PEAK_LOSS_FACTOR, ! REAL4
     +                     PEAK_COIN_FACTOR, ! REAL4
     +                     DISTRIBUTION_PRICE, ! REAL4
     +                     TRANSMISSION_PRICE, !REAL4
     +                     TABLE_ACTIVE,
     +                     BASECASE_MKT_AREA_ID_tf1, ! CHAR*6
     +                     BASECASE_TRANS_AREA_ID, ! CHAR*6
     +                     BASECASE_NERC_SUB_ID, ! CHAR*6
     +                     MONTHLY_UNITS,
     +                     MINIMUM_MARKET_PRICE,
     +                     MAXIMUM_MARKET_PRICE,
     +                     INDEXED_ENERGY_PRICE,
     +                     PRICE_INDEX_ACTIVE,
     +                     HESI_TRANS_AREA_ID_NUM,
     +                     REVENUE_CLASS_ENERGY,
     +                     REVENUE_CLASS_DEMAND,
     +                     REVENUE_CLASS_CUST,
     +                     REVENUE_INDEX_ENERGY,
     +                     DEMAND_PRICING_METHOD,
     +                     INTRA_COMPANY_TRANSACTION,
     +                     INTRA_ASSET_CLASS_ID,
     +                     INTRA_ASSET_CLASS_ALLOCATION,
     +                     INTRA_ACCOUNT_CLASSIFICATION,
     +                     INTRA_EXPENSE_COLLECTION,
     +                     FILE_TABLE_INDEX,
     +                     SCENARIO_VARIABLE,
     +                     LOAD_DISPATCH_POSITION,
     +                     THREE_FACTOR_TRANSFORM,
     +                     JURISDICTIONAL_CUSTOMER,
     +                     FUEL_COST_RECOVERY_THROUGH_FAC,
     +                     BASE_COST_OF_FAC_FUEL,
     +                     CUSTOMER_GROUP_2,
     +                     INTERRUPTIBLE_PRICING_ACTIVE,
     +                     INTERRUPTIBLE_MARKUP_PERENT,
     +                     INTERRUPTIBLE_MARKUP_CAP,
     +                     INTERRUPTIBLE_MAX_CAPACITY,
     +                     INTERRUPTIBLE_MIN_CAPACITY,
     +                     INTERRUPTIBLE_MARKUP_ADDER,
     +                     TRANS_GROUP_NAME,             ! 95
     +                     RTO_ISO_GROUP_NAME,
     +                     PRIMARY_STATE,
     +                     CAPACITY_MARKET_MONTH,
     +                     capacity_market_pointer_tfo,
     +                     CAPACITY_MARKET_COST_ASSIGN,
     +                     CAPACITY_MARKET_EXP_COLLECT,
     +                     CAPACITY_MARKET_COIN_ADJ_FACT
               IF(IOS /= 0) EXIT ! END OF BINARY FILE
!
               IF(YEAR == TF_YEAR) THEN
                  RECLN=trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
                  READ(RECLN,*,iostat=ios)
     +                     DELETE,
     +                     TF_YEAR, ! INT2
     +                     CUSTOMER_GROUP, ! INT2
     +                     CUSTOMER_CLASS_NAME, ! CHAR*30
     +                     CALCULATION_MODE, ! CHAR*1
     +                     TRANSACTION_GROUP, ! INT2
     +                     MARKET_ENERGY_PRICE, ! REAL4
     +                     MONTHLY_ENERGY_PRICE_PATTERN, ! REAL4
     +                     MARKET_DEMAND_PRICE, ! REAL4
     +                     MONTHLY_DEMAND_PRICE_PATTERN, ! REAL4
     +                     MARKET_CUSTOMER_PRICE, ! REAL4
     +                     ASSET_CLASS_ID_REAL, ! REAL4
     +                     ASSET_CLASS_REV_ALLOC_VECTOR, ! REAL4
     +                     REFERENCE_LOAD_NAME, ! CHAR*5
     +                     REFERENCE_LOAD_NUMBER, ! INT2
     +                     ANNUAL_ENERGY, ! REAL4
     +                     ANNUAL_PEAK, ! REAL4
     +                     ANNUAL_CUSTOMERS, ! REAL4
     +                     ANNUAL_MULTIPLIER, ! REAL4
     +                     MONTHLY_ENERGY, ! (12) REAL4
     +                     MONTHLY_PEAK, ! (12) REAL4
     +                     MONTHLY_CUSTOMERS, ! (12) REAL4
     +                     COMMENT, ! CHAR*50
     +                     DIST_ENERGY_LOSS_FACTOR, ! REAL4
     +                     TRANS_ENERGY_LOSS_FACTOR, ! REAL4
     +                     PEAK_LOSS_FACTOR, ! REAL4
     +                     PEAK_COIN_FACTOR, ! REAL4
     +                     DISTRIBUTION_PRICE, ! REAL4
     +                     TRANSMISSION_PRICE, !REAL4
     +                     TABLE_ACTIVE,
     +                     BASECASE_MKT_AREA_ID_tf1, ! CHAR*6
     +                     BASECASE_TRANS_AREA_ID, ! CHAR*6
     +                     BASECASE_NERC_SUB_ID, ! CHAR*6
     +                     MONTHLY_UNITS,
     +                     MINIMUM_MARKET_PRICE,
     +                     MAXIMUM_MARKET_PRICE,
     +                     INDEXED_ENERGY_PRICE,
     +                     PRICE_INDEX_ACTIVE,
     +                     HESI_TRANS_AREA_ID_NUM,
     +                     REVENUE_CLASS_ENERGY,
     +                     REVENUE_CLASS_DEMAND,
     +                     REVENUE_CLASS_CUST,
     +                     REVENUE_INDEX_ENERGY,
     +                     DEMAND_PRICING_METHOD,
     +                     INTRA_COMPANY_TRANSACTION,
     +                     INTRA_ASSET_CLASS_ID,
     +                     INTRA_ASSET_CLASS_ALLOCATION,
     +                     INTRA_ACCOUNT_CLASSIFICATION,
     +                     INTRA_EXPENSE_COLLECTION,
     +                     SCENARIO_VARIABLE,
     +                     LOAD_DISPATCH_POSITION,
     +                     THREE_FACTOR_TRANSFORM,
     +                     JURISDICTIONAL_CUSTOMER,
     +                     FUEL_COST_RECOVERY_THROUGH_FAC,
     +                     BASE_COST_OF_FAC_FUEL,
     +                     CUSTOMER_GROUP_2,
     +                     INTERRUPTIBLE_PRICING_ACTIVE,
     +                     INTERRUPTIBLE_MARKUP_PERENT,
     +                     INTERRUPTIBLE_MARKUP_CAP,
     +                     INTERRUPTIBLE_MAX_CAPACITY,
     +                     INTERRUPTIBLE_MIN_CAPACITY,
     +                     INTERRUPTIBLE_MARKUP_ADDER,
     +                     TRANS_GROUP_NAME,             ! 95
     +                     RTO_ISO_GROUP_NAME,
     +                     PRIMARY_STATE,
     +                     CAPACITY_MARKET_MONTH,
     +                     capacity_market_pointer_tfo,
     +                     CAPACITY_MARKET_COST_ASSIGN,
     +                     CAPACITY_MARKET_EXP_COLLECT,
     +                     CAPACITY_MARKET_COIN_ADJ_FACT

       if(ios/=0) then
        er_message="tf_objt:0007 - error reading file."
        call end_program(er_message)
       endif

                  READ(10,1000,IOSTAT=IOS) RECLN
       if(ios/=0) then
        er_message="tf_objt:0008 - error reading file."
        call end_program(er_message)
       endif
                  DOWHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE
!
                     READ(10,1000,IOSTAT=IOS) RECLN
                     if(ios/=0) then
                        er_message="tf_objt:0009 - error reading file."
                        call end_program(er_message)
                    endif
!
                  ENDDO
                  READ(RECLN,*,IOSTAT=ios) DELETE,TF_YEAR
                  if(ios/=0) then
                    er_message="tf_objt:0010 - error reading file."
                    call end_program(er_message)
                  endif
               ENDIF

               ASSET_CLASS_ID = ASSET_CLASS_ID_REAL
              IF(TEMP_TRANS_CLASS_POINTER(TRANSACTION_GROUP) == 0) THEN
                  NUM_OF_OL_TRANS_CLASSES = NUM_OF_OL_TRANS_CLASSES + 1
                  MAX_OL_TRANS_CLASS_ID_NUM = MAX(
     +                     MAX_OL_TRANS_CLASS_ID_NUM,TRANSACTION_GROUP)
                  TEMP_TRANS_CLASS_POINTER(TRANSACTION_GROUP) = 1
               ENDIF
               IF(TEMP_ASSET_CLASS_POINTER(ASSET_CLASS_ID) == 0) THEN
                  NUM_OF_OL_ASSET_CLASSES = NUM_OF_OL_ASSET_CLASSES + 1
                  MAX_OL_ASSET_CLASS_ID_NUM = MAX(
     +                        MAX_OL_ASSET_CLASS_ID_NUM,ASSET_CLASS_ID)
                  TEMP_ASSET_CLASS_POINTER(ASSET_CLASS_ID) = 1
               ENDIF
               IF(INTRA_COMPANY_TRANSACTION == 'Y') THEN
                  IF(TEMP_ASSET_CLASS_POINTER(INTRA_ASSET_CLASS_ID)
     +                                                       == 0) THEN
                     NUM_OF_OL_ASSET_CLASSES =
     +                                      NUM_OF_OL_ASSET_CLASSES + 1
                     MAX_OL_ASSET_CLASS_ID_NUM = MAX(
     +                  MAX_OL_ASSET_CLASS_ID_NUM,INTRA_ASSET_CLASS_ID)
                     TEMP_ASSET_CLASS_POINTER(INTRA_ASSET_CLASS_ID) = 1
                  ENDIF
               ENDIF
               IF(TEMP_CUST_CLASS_POINTER(CUSTOMER_GROUP) == 0) THEN
                  NUM_OF_OL_CUST_CLASSES = NUM_OF_OL_CUST_CLASSES + 1
                  MAX_OL_CUST_CLASS_ID_NUM = MAX(
     +                         MAX_OL_CUST_CLASS_ID_NUM,CUSTOMER_GROUP)
                  TEMP_CUST_CLASS_POINTER(CUSTOMER_GROUP) = 1
               ENDIF

! CONVERT GROWTH RATE TO VALUES, SAVE PRIOR YEAR 2/12/11 MSG
                  IF(CALCULATION_MODE == 'G') THEN

                     MONTHLY_ENERGY = (1. + MONTHLY_ENERGY/100.)
     +                                                 * LAST_YR_ENERGY
                     MONTHLY_PEAK = (1.+MONTHLY_PEAK/100.)
     +                                                   * LAST_YR_PEAK
                     ! C is treated the same as M
                     CALCULATION_MODE = 'C'
                  ELSEIF(CALCULATION_MODE == 'C') THEN
                     IF(MAXVAL(MONTHLY_ENERGY) <= 100.)
     +                      MONTHLY_ENERGY = (1. + MONTHLY_ENERGY/100.)
     +                                                 * LAST_YR_ENERGY
                     IF(MAXVAL(MONTHLY_PEAK) <= 100.)
     +                      MONTHLY_PEAK = (1.+MONTHLY_PEAK/100.)
     +                                                   * LAST_YR_PEAK
                  ENDIF
                  LAST_YR_ENERGY = MONTHLY_ENERGY
                  LAST_YR_PEAK = MONTHLY_PEAK
!
               OL_MASTR_REC = OL_MASTR_REC + 1
               DO UNIT_NO = 12, 13
                  IF(UNIT_NO == 12) CUR_REC = IREC
                  IF(UNIT_NO == 13) CUR_REC = OL_MASTR_REC
!
                  FILE_TABLE_INDEX = (FILE_ID+1)*1000 + CUSTOMER_GROUP
!
                  WRITE(UNIT_NO,REC=CUR_REC)  DELETE,
     +                     YEAR, ! INT2
     +                     CUSTOMER_GROUP, ! INT2
     +                     CUSTOMER_CLASS_NAME, ! CHAR*30
     +                     CALCULATION_MODE, ! CHAR*1
     +                     TRANSACTION_GROUP, ! INT2
     +                     MARKET_ENERGY_PRICE, ! REAL4
     +                     MONTHLY_ENERGY_PRICE_PATTERN, ! REAL4
     +                     MARKET_DEMAND_PRICE, ! REAL4
     +                     MONTHLY_DEMAND_PRICE_PATTERN, ! REAL4
     +                     MARKET_CUSTOMER_PRICE, ! REAL4
     +                     ASSET_CLASS_ID_REAL, ! REAL4
     +                     ASSET_CLASS_REV_ALLOC_VECTOR, ! REAL4
     +                     REFERENCE_LOAD_NAME, ! CHAR*5
     +                     REFERENCE_LOAD_NUMBER, ! INT2
     +                     ANNUAL_ENERGY, ! REAL4
     +                     ANNUAL_PEAK, ! REAL4
     +                     ANNUAL_CUSTOMERS, ! REAL4
     +                     ANNUAL_MULTIPLIER, ! REAL4
     +                     MONTHLY_ENERGY, ! (12) REAL4
     +                     MONTHLY_PEAK, ! (12) REAL4
     +                     MONTHLY_CUSTOMERS, ! (12) REAL4
!     +                     COMMENT, ! CHAR*50
     +                     DIST_ENERGY_LOSS_FACTOR, ! REAL4
     +                     TRANS_ENERGY_LOSS_FACTOR, ! REAL4
     +                     PEAK_LOSS_FACTOR, ! REAL4
     +                     PEAK_COIN_FACTOR, ! REAL4
     +                     DISTRIBUTION_PRICE, ! REAL4
     +                     TRANSMISSION_PRICE, !REAL4
     +                     TABLE_ACTIVE,
     +                     BASECASE_MKT_AREA_ID_tf1, ! CHAR*6
     +                     BASECASE_TRANS_AREA_ID, ! CHAR*6
     +                     BASECASE_NERC_SUB_ID, ! CHAR*6
     +                     MONTHLY_UNITS,
     +                     MINIMUM_MARKET_PRICE,
     +                     MAXIMUM_MARKET_PRICE,
     +                     INDEXED_ENERGY_PRICE,
     +                     PRICE_INDEX_ACTIVE,
     +                     HESI_TRANS_AREA_ID_NUM,
     +                     REVENUE_CLASS_ENERGY,
     +                     REVENUE_CLASS_DEMAND,
     +                     REVENUE_CLASS_CUST,
     +                     REVENUE_INDEX_ENERGY,
     +                     DEMAND_PRICING_METHOD,
     +                     INTRA_COMPANY_TRANSACTION,
     +                     INTRA_ASSET_CLASS_ID,
     +                     INTRA_ASSET_CLASS_ALLOCATION,
     +                     INTRA_ACCOUNT_CLASSIFICATION,
     +                     INTRA_EXPENSE_COLLECTION,
     +                     FILE_TABLE_INDEX,
     +                     SCENARIO_VARIABLE,
     +                     LOAD_DISPATCH_POSITION,
     +                     THREE_FACTOR_TRANSFORM,
     +                     JURISDICTIONAL_CUSTOMER,
     +                     FUEL_COST_RECOVERY_THROUGH_FAC,
     +                     BASE_COST_OF_FAC_FUEL,
     +                     CUSTOMER_GROUP_2,
     +                     INTERRUPTIBLE_PRICING_ACTIVE,
     +                     INTERRUPTIBLE_MARKUP_PERENT,
     +                     INTERRUPTIBLE_MARKUP_CAP,
     +                     INTERRUPTIBLE_MAX_CAPACITY,
     +                     INTERRUPTIBLE_MIN_CAPACITY,
     +                     INTERRUPTIBLE_MARKUP_ADDER,
     +                     TRANS_GROUP_NAME,             ! 95
     +                     RTO_ISO_GROUP_NAME,
     +                     PRIMARY_STATE,
     +                     CAPACITY_MARKET_MONTH,
     +                     capacity_market_pointer_tfo,
     +                     CAPACITY_MARKET_COST_ASSIGN,
     +                     CAPACITY_MARKET_EXP_COLLECT,
     +                     CAPACITY_MARKET_COIN_ADJ_FACT
               ENDDO ! WRITE TO BINARY FILES LOOP
!            CALL CALCULATE_TSYTEM_PEAKS(TSY_FC_DATA,TF_YEAR)
            ENDDO ! END TABLE LOOP
            CLOSE(10)
            CLOSE(12)
            IF(TF_MULTI_OL_CODES(FILE_ID) == 'BC') CLOSE(11)
            TF_MULTI_OL_CODES(FILE_ID) = 'OL'
         ENDIF
      ENDDO ! FILE LOOP
      IF(TF_OVERLAY_MASTR_FILE_OPEN) CLOSE(13)
      IF(ALLOCATED(TEMP_TRANS_CLASS_POINTER))
     +      DEALLOCATE(TEMP_TRANS_CLASS_POINTER,
     +                 TEMP_ASSET_CLASS_POINTER,
     +                 TEMP_CUST_CLASS_POINTER)
      TF_OVERLAY_MASTR_FILE_OPEN = .FALSE.
      RETURN



      ENTRY RESET_TSYFRC_OL


         TSYFRC_OL = 'BC'
         DO FILE_ID = 0, MAX_TF_FILES-1
            TF_MULTI_OL_CODES(FILE_ID) = 'BC'
            ACTIVE_OVERLAY_TF_FILES(FILE_ID) = .FALSE.
         ENDDO
!        SAVE_TF_FILE_USED = .FALSE.
!        TF_OVERLAY_MASTR_FILE_OPEN = .FALSE.
      RETURN

      ENTRY RETURN_TSYFRC_OL(R_TSYFRC_OL,R_LRECL)

         R_TSYFRC_OL = TSYFRC_OL
         R_LRECL = LRECL
      RETURN

      ENTRY DOES_TF_FILE_EXIST(R_TF_FILE_EXISTS)

         R_TF_FILE_EXISTS = SAVE_TF_FILE_EXISTS
      RETURN

      ENTRY GET_TRANS_LOAD_TABLES(R_TRANS_LOAD_TABLES)

         R_TRANS_LOAD_TABLES = SAVE_TRANS_LOAD_TABLES
      RETURN

      ENTRY TF_FILE_USED_THIS_ENDPOINT(R_TF_FILE_USED)

         R_TF_FILE_USED = SAVE_TF_FILE_USED
!        SAVE_TF_FILE_USED = .TRUE.
      RETURN


      ENTRY RETURN_TF_GROUP_INFO(R_NUM_OF_TRANS_CLASSES,
     +                           R_MAX_TRANS_CLASS_NUM,
     +                           R_NUM_OF_ASSET_CLASSES,
     +                           R_MAX_ASSET_CLASS_NUM,
     +                           R_NUM_OF_CUST_CLASSES,
     +                           R_MAX_CUST_CLASS_NUM)

         IF(TSYFRC_OL == 'OL') THEN
            R_NUM_OF_TRANS_CLASSES = NUM_OF_OL_TRANS_CLASSES
            R_MAX_TRANS_CLASS_NUM = MAX(MAX_OL_TRANS_CLASS_ID_NUM,
     +                                       MAX_BC_TRANS_CLASS_ID_NUM)
            R_NUM_OF_ASSET_CLASSES = NUM_OF_OL_ASSET_CLASSES
            R_MAX_ASSET_CLASS_NUM = MAX(1,MAX_OL_ASSET_CLASS_ID_NUM,
     +                                       MAX_BC_ASSET_CLASS_ID_NUM)
            R_NUM_OF_CUST_CLASSES = NUM_OF_OL_CUST_CLASSES
            R_MAX_CUST_CLASS_NUM = MAX(MAX_OL_CUST_CLASS_ID_NUM,
     +                                        MAX_BC_CUST_CLASS_ID_NUM)
         ELSE
            R_NUM_OF_TRANS_CLASSES = NUM_OF_BC_TRANS_CLASSES
            R_MAX_TRANS_CLASS_NUM = MAX_BC_TRANS_CLASS_ID_NUM
            R_NUM_OF_ASSET_CLASSES = NUM_OF_BC_ASSET_CLASSES
            R_MAX_ASSET_CLASS_NUM = MAX(1,MAX_BC_ASSET_CLASS_ID_NUM)
            R_NUM_OF_CUST_CLASSES = NUM_OF_BC_CUST_CLASSES
            R_MAX_CUST_CLASS_NUM = MAX_BC_CUST_CLASS_ID_NUM
         ENDIF
      RETURN
!
 1000 FORMAT(A)
 1010 FORMAT('&',A)

      END

!
!

!

!
!        PROGRAM TO MANAGE CUSTOMER FORECASTS, REVENUES AND COSTS
!           BY TRANSACTION GROUPASSET CLASS AND CUSTOMER GROUP
!
!                       COPYRIGHT (C) 1997,98
!          ALL RIGHTS RESERVED M.S. GERBER & ASSOCIATES, INC.
!


!
      FUNCTION MANAGE_TRANSACTION_FORECASTS(R_SORTED_OPTIONS,
     +                                      R_MUST_RUN_OPTIONS,
     +                                      R_UNIT_FOR_OUTAGE_BLOCK,
     +                                      R_BLOCK_FOR_OUTAGE_BLOCK,
     +                                      MONTH_VARS,
     +                                      R_HYDRO_LOAD,
     +                                      R_DISPATCH_COST_FOR_BLOCK,
     +                                      R_INCREMENTAL_FUEL_COST,
     +                                      R_RETAIL_REVENUE_FOR_BLOCK,
     +                                      R_GENERATION_BY_SEGMENT,
     +                                      TEMP_HOURLY_INDEX)
      use production
      use tf_decs
      use end_routine
      use fuelused_decs
      use rptreccontrol
      use grx_planning_routines
      use spindriftlib
      use prod_arrays_dimensions
      use irec_endpoint_control
      use trancom
      use sizecom
      use globecom
      use mthnmcom
      use namescom
      use pp_objt_interfaces
      use cla2_decs
      use tf_objt_interfaces
	  use mod_base_year
	  use nunits_shared
      use load_trace
      use sic_trace

      SAVE

      INTEGER*2   R_CLASS,MO,R_MONTH,
     +            GLOBAL_PEAK_MONTH/0/,
     +            VOID_I,UPDATE_PLANNING_PEAK_MONTH,
     +            LREC,J,SI,X
      REAL (KIND=4) :: R_MONTHLY_PURCHASED_POWER(0:12),
     +                 R_MONTHLY_PURCHASED_CAPACITY(0:12)
      REAL*4      MONTH_VARS(0:12,1:*),R_MONTHLY_VALUES(0:12),
     +            R_HOUR_PRICE,TEMP_PRICE,FINAL_REVENUE,
     +            TEMP_LOAD,
     +            TEMP_COST,
     +            VOID_R,
     +            UPDATE_PEAK_AFTER_FEEDBACK,
     +            HYDRO_WEEK_PLANNING_PEAK,
     +            GET_HYDRO_WEEK_PLANNING_PEAK
!
      INTEGER*2   DELETE,EXTENSION_PERIOD_START,TEMP_I,
     +            TRANS_YEAR_RECORD,TEMP_YEAR,TRANS,I,TRANS_LOOP_POS,
     +            MAX_TRANS_LOAD_TABLES/0/,R_YEAR,R_ISEAS,R_HOURS,
     +            MAX_TRANS_LOAD_GROUPS/0/,TG,TGP,
     +            TG_COUNTER(:),
     +            LOAD_DISPATCH_POSITION(:,:),
     +            LOAD_DISPATCH_INDEX(:,:),
     +            TEMP_DISPATCH_POSITION,
     +            LAST_TABLE_FOR_TG(:),
     +            LAST_TABLE_FOR_CG(:),
     +            LAST_TABLE_FOR_CG2(:),
     +            TABLE_DAY_SHIFT(:),
     +            LOCAL_YEAR/0/,
     +            FIRST_DAY_OF_YEAR,
     +            GET_MAX_TRANS_GROUP_NUMBER,
     +            DAY,
     +            HOUR_IN_DAY,
     +            R_YEAR_TEMP
      INTEGER IOS,UNIT_NO
      REAL*4      R_HR_COST,AC_COST_FACTOR,
     +            GET_TRANS_GROUP_PEAK,
     +            GET_SCENARIO_ELECTRIC_DEMAND,
     +            GET_PARAM_ELECTRIC_DEMAND,
     +            GET_SCENARIO_PEAK,
     +            GET_PARAM_PEAK,
     +            GET_SCENARIO_ENERGY,
     +            GET_PARAM_ENERGY,
     +            MONTHLY_MAINT_VECTOR,
     +            HOURLY_LOAD_FROM_AC_TG,
     +            MONTH_PEAK_BY_CG,
     +            R_HYDRO_LOAD(*),
     +            TEMP_R,
     +            TEMP1_R,
     +            TEMP2_R,
     +            INTERVAL_CAPACITY
      LOGICAL*4 LOAD_FILE_OPEN,FILE_EXISTS

      LOGICAL*1   MANAGE_TRANSACTION_FORECASTS,
     +            ANNUAL_TRANSACTION_LOADS,
     +            MONTHLY_TRANSACTION_LOADS,
     +            MONTHLY_TRANSACTION_DURATION,
     +            DEALLOCATE_ANNUAL_TRANS_LOADS,
     +            DEALLOCATE_MONTHLY_TRANS_LOADS,
     +            TF_FILE_EXISTS,
     +            SAVE_TF_FILE_EXISTS/.FALSE./,
     +            WD_FILE_EXISTS/.FALSE./,
     +            WH_FILE_EXISTS/.FALSE./,
     +            GET_WD_LOAD,
     +            YES_USE_TF_FILE_FOR_PRICE,
     +            YES_USE_TF_FILE_FOR_MULTIAREA,
     +            TF_GROUP1_ACTIVE/.FALSE./,
     +            TF_ANY_GROUP_ACTIVE/.FALSE./,
     +            RUN_TRANSACT/.FALSE./,
     +            TRANS_GROUP_ACTIVE_SWITCH,
     +            IN_ACTIVE_LOADS_MARKET_AREA,
     +            TF_FILE_LOAD_GROUP_ACTIVE,
     +            TF_IPL_ELECTIC_PLAN_COST,
     +            TF_FILE_USED,
     +            GET_TRANS_HOUR_DISTRIBUTION,
     +            GET_HYDRO_HOUR_DISTRIBUTION,
     +            PUT_AC_HOURLY_COST_AT_MARKET,
     +            YES_FIRST_TABLE,
     +            OPEN_SCENARIO_HOURLY_DEMAND,
     +            USE_SCENARIO_HOURLY_DEMAND,
     +            GET_HYDRO_LOAD_AFTER_EL,
     +            PUT_HYDRO_LOAD_AFTER_EL,
     +            TEMP_L,
     +            GET_WEEKLY_HYDRO_FORECASTS,
     +            WEEKLY_HYDRO_FORECASTS,
     +            PRICE_ONLY_WHOLESALE_REV/.FALSE./,
     +            IGNORE_NAT_PUR_USE_W/.FALSE./,
     +            IGNORE_NATIVE_PURCHASE_USE_W,
     +            USING_INDEXED_PRICING/.FALSE./,
     +            APPLY_TRANS_REV_TO_WHOLESALE,
     +            GET_MONTHLY_MAINTENANCE_PENALTY,
     +            GET_TG_2_HYDRO_WEEK,TG_2_HYDRO_WEEK,
     +            GET_FILE_TABLE_DATA,
     +            GET_TG_CG_DATA,
     +            GET_TG_CG2_DATA,
     +            ASSET_ANALYST_ONLY,
     +            YES_ASSET_ANALYST_ONLY,
     +            USE_MARKET_AREA_REPORT_ID/.FALSE./,
     +            YES_USE_MARKET_AREA_REPORT_ID,
     +            ALLOCATE_BLOCKS_2_CUSTOMERS,
     +            INIT_MONTH_ALLOC_BLOCKS_2_CUST,
     +            USE_MONTHLY_NONLINEAR_LOADS,
     +            USE_NONLINEAR_R4,
     +            ACTIVE_WD_FORECAST,
     +            YES_HOURLY_CUST_MARGIN_REPORT,
     +            HOURLY_CUST_MARGIN_REPORT,
     +            YES_TRANS_INTR_REPORT,
     +            TRANS_INTR_REPORT,
     +            YES_MID_TERM_PEAK_UNCER,
     +            MID_TERM_PEAK_UNCER,
     +            YES_ZONAL_LEVEL_MARKET,
     +            ZONAL_LEVEL_MARKET,
     +            GET_TG_MONTH_SUM_B4_HYDRO,
     +            INTERRUPTIBLE_PRICING
      character(len=20) :: cla_return_unitnm ! External
      INTEGER*2   TRANS_LOAD_GROUPS_INDEX(:),
     +            CUST_CLASS_GROUPS_INDEX(:),
     +            CUST2_CLASS_GROUPS_INDEX(:),
     +            ASSET_CLASS_GROUPS_INDEX(:),
     +            ASSET_2_TRANS_INDEX(:,:),
     +            NUMBER_ASSET_2_TRANS(:),
     +            ASSET_TRANSACTION_CROSS_INDEX(:,:),
     +            NUMBER_TRANS_PER_AC_TG(:,:),
     +            TRANS_WITHIN_AC_TG(:,:,:),
     +            FIRST_AC_TG(:),
     +            FIRST_TABLE_TG(:),
     +            TRANS_LOAD_2_TRANS_GROUPS(:),
     +            TRANS_LOAD_GROUP_2_TG(:),
     +            CUST_CLASS_GROUP_2_CG(:),
     +            CUST2_CLASS_GROUP_2_CG(:),
     +            ASSET_CLASS_GROUP_2_AC(:),
     +            MAX_TRANS_GROUPS/256/,
     +            MAX_TRANS_GROUPS_FROM_TG/0/,
     +            HIGHEST_TG_COUNT,
     +            GET_MAX_TRANS_GROUPS,
     +            NUM_OF_TRANS_CLASSES,
     +            MAX_TRANS_CLASS_NUM,
     +            NUM_OF_ASSET_CLASSES,
     +            MAX_ASSET_CLASS_NUM,
     +            NUM_OF_CUST_CLASSES,
     +            MAX_CUST_CLASS_NUM,
     +            GET_TRANS_GROUP_POSITION,
     +            R_LOAD_GROUP,
     +            R_MUST_RUN_OPTIONS(*),L,U,B,
     +            R_UNIT_FOR_OUTAGE_BLOCK(*),
     +            R_BLOCK_FOR_OUTAGE_BLOCK(*),
     +            R_SORTED_OPTIONS(*),
     +            TG_FROM_TRANS_LOAD_GROUP,
     +            DAY_OF_WEEK(:,:),
     +            WD_INDEX(:),
     +            MARKET_AREA_LOOKUP,
     +            R_HR_IN_MONTH,LOCAL_MONTH,
     +            NUMBER_OF_CLASSES,
     +            AC_POSITION,
     +            CALENDAR_DAY_OF_WEEK,
     +            NUMBER_OF_HYDRO_GROUPS/0/,
     +            GET_NUMBER_OF_HYDRO_GROUPS,
     +            HOUR_IN_WEEK,
     +            REV_CLASS_INDEX(:,:),
     +            INCOME_STATEMENT_POSITION,
     +            RI,
     +            FILE_TABLE_INDEX,
     +            R_INDEX,
     +            R_HOUR,
     +            R_TOTAL_DISPATCH_BLOCKS,
     +            NUNITS,
     +            R_HOURLY_DISPATCH_BLOCKS,
     +            R_LAST_HOUR,
     +            FILE_TABLE_2_TRANS_INDEX(0:15999), ! 10/28/02.
     +            MAX_TRANS_GROUP_NUMBER,
     +            MAX_CUST_GROUP_NUMBER,
     +            MAX_CUST2_GROUP_NUMBER,
     +            LAST_BLOCK,
     +            MIN_CAPACITY_BLOCK,
     +            MAX_CAPACITY_BLOCK
      CHARACTER*1 TABLE_ACTIVE(:),MONTHLY_UNITS(:),
     +            PRICE_INDEX_ACTIVE(:),
     +            THREE_FACTOR_TRANSFORM(:),
     +            JURISDICTIONAL_CUSTOMER(:),
     +            FUEL_COST_RECOVERY_THROUGH_FAC(:),
     +            INTERRUPTIBLE_PRICING_ACTIVE(:)
      CHARACTER*5 MARKET_ID
      CHARACTER*6 BASECASE_MKT_AREA_ID_tfo(:),
     +            BASECASE_TRANS_AREA_ID(:),
     +            BASECASE_NERC_SUB_ID(:)
      CHARACTER*20 MARKET_AREA_NAME
      CHARACTER*30 LOCAL_CUSTOMER_NAME(:),TEMP_CUSTOMER_NAME
      CHARACTER*50 REVENUE_CLASS_ENERGY,
     +             REVENUE_CLASS_DEMAND,
     +             REVENUE_CLASS_CUST,
     +             REVENUE_INDEX_ENERGY
      CHARACTER*1 DEMAND_PRICING_METHOD(:)
      INTEGER*2 INTRA_ASSET_CLASS_ID(:),
     + INTRA_ASSET_CLASS_ALLOCATION(:),
     +            SCENARIO_INDEX(:),GET_SCENARIO_INDEX
      CHARACTER*30 INTRA_ACCOUNT_CLASSIFICATION(:)
      CHARACTER*40 SCENARIO_VARIABLE
      CHARACTER*3 INTRA_EXPENSE_COLLECTION(:)
      CHARACTER*1 INTRA_COMPANY_TRANSACTION(:)
      INTEGER HESI_TRANS_AREA_ID_NUM
      REAL
     +     TRANS_MONTHLY_ENERGY(:,:),
     +     WH_TRANS_MONTHLY_ENERGY(:,:),
     +     WH_TRANS_MONTHLY_CAPACITY(:,:),
     +     TRANS_MONTHLY_PEAK(:,:),
     +     TRANS_MONTHLY_CUSTOMERS(:,:),
     +     TRANS_HOURLY_LOAD(:,:),
     +     WH_LOADS_PER_HOUR(:,:),
     +     HYDRO_HOURLY_LOAD(:,:),
     +     TABLE_HOURLY_LOAD(:,:),
     +     REF_HOURLY_LOAD,
     +     WH_TRANS_ALLOC(:,:),
     +     ASSET_CLASS_HOURLY_LOAD(:,:,:),
     +     MONTHLY_AC_COST_AT_MARKET(:,:),
     +     MONTHLY_AC_CONTRACT_REVENUE(:,:,:,:,:),
     +     MONTHLY_AC_CONTRACT_EXPENSE(:,:,:),
     +     GET_WH_LOADS_PER_HOUR,
     +     GET_SCENARIO_BY_INDEX,
     +     LATIN_HYPERCUBE_ENERGY,
     +     LATIN_HYPERCUBE_PEAK,
     +     R_GENERATION_BY_SEGMENT(*),
     +     TEMP_GEN,
     +     R_MARKET_PRICE,
     +     LOCAL_AVERAGE_PRICE,
     +     R_WHOLESALE_PURCHASE,
     +     R_WHOLESALE_SALES,
     +     R_WHOLESALE_PRICE,
     +     AVE_REV,
     +     R_UNSERVED_ENERGY,
     +     REMAINING_UNSERVED,
     +     GET_LAST_THIS_YR_ENERGY,
     +     HOURLY_INTERRUPIBLE_REVENUE(:,:,:),
     +     R_UNSERVED_COST,
     +     R_THERMAL_LOAD,
     +     NET_LOAD_ADJUSTMENTS,
     +     R_4_ZERO/0./,
     +     R_DISPATCH_COST_FOR_BLOCK(*),
     +     R_INCREMENTAL_FUEL_COST(*),
     +     R_RETAIL_REVENUE_FOR_BLOCK(*),
     +     LOAD_DISPATCH_COST(:,:),
     +     LOAD_DISPATCH_REV(:,:),
     +     LOAD_DISPATCH_BY_BLOCK(:,:),
     +     LOAD_DISPATCH_COST_BY_UNIT(:,:),
     +     LOAD_DISPATCH_REV_BY_UNIT(:,:),
     +     LOAD_DISPATCH_BY_UNIT(:,:),
     +     HOURLY_CUST_MARGIN_DB(:,:),
     +     LOCAL_MUST_RUN_OPTIONS(:),
     +     ANN_LOAD_DISPATCH_COST_BY_UNIT(:,:,:),
     +     ANN_LOAD_DISPATCH_REV_BY_UNIT(:,:,:),
     +     ANN_LOAD_DISPATCH_BY_UNIT(:,:,:),
     +     MONTHLY_INTERRUPTIBLE_REVENUE(:,:),
     +     BASE_COST_OF_FAC_FUEL(:)
!     +     MONTHLY_INDEXED_ENERGY_REVENUE(:,:,:) OUT 2/11/2/.
      ALLOCATABLE :: TRANS_MONTHLY_ENERGY,
     +               WH_TRANS_MONTHLY_ENERGY,
     +               WH_TRANS_MONTHLY_CAPACITY,
     +               TRANS_MONTHLY_PEAK,
     +               TRANS_MONTHLY_CUSTOMERS,
     +               TRANS_LOAD_GROUPS_INDEX,
     +               CUST_CLASS_GROUPS_INDEX,
     +               CUST2_CLASS_GROUPS_INDEX,
     +               TRANS_LOAD_2_TRANS_GROUPS,
     +               TRANS_LOAD_GROUP_2_TG,
     +               CUST_CLASS_GROUP_2_CG,
     +               CUST2_CLASS_GROUP_2_CG,
     +               TRANS_HOURLY_LOAD,
     +               WH_LOADS_PER_HOUR,
     +               HYDRO_HOURLY_LOAD,
     +               TABLE_HOURLY_LOAD,
     +               WH_TRANS_ALLOC,
     +               ASSET_CLASS_HOURLY_LOAD,
     +               MONTHLY_AC_COST_AT_MARKET,
     +               MONTHLY_AC_CONTRACT_REVENUE,
     +               MONTHLY_AC_CONTRACT_EXPENSE,
!     +               MONTHLY_INDEXED_ENERGY_REVENUE,
     +               ASSET_CLASS_GROUPS_INDEX,
     +               ASSET_2_TRANS_INDEX,
     +               NUMBER_ASSET_2_TRANS,
     +               ASSET_TRANSACTION_CROSS_INDEX,
     +               NUMBER_TRANS_PER_AC_TG,
     +               TRANS_WITHIN_AC_TG,
     +               FIRST_AC_TG,
     +               FIRST_TABLE_TG,
     +               ASSET_CLASS_GROUP_2_AC,
     +               DAY_OF_WEEK,
     +               TABLE_ACTIVE,
     +               BASECASE_MKT_AREA_ID_tfo,
     +               WD_INDEX,
     +               BASECASE_TRANS_AREA_ID,
     +               BASECASE_NERC_SUB_ID,
     +               LAST_TABLE_FOR_TG,
     +               TG_COUNTER,
     +               LOAD_DISPATCH_POSITION,
     +               LOAD_DISPATCH_INDEX,
     +               LOAD_DISPATCH_COST,
     +               LOCAL_MUST_RUN_OPTIONS,
     +               LOAD_DISPATCH_COST_BY_UNIT,
     +               LOAD_DISPATCH_REV,
     +               LOAD_DISPATCH_REV_BY_UNIT,
     +               LOAD_DISPATCH_BY_BLOCK,
     +               LOAD_DISPATCH_BY_UNIT,
     +               HOURLY_CUST_MARGIN_DB,
     +               ANN_LOAD_DISPATCH_COST_BY_UNIT,
     +               ANN_LOAD_DISPATCH_REV_BY_UNIT,
     +               ANN_LOAD_DISPATCH_BY_UNIT,
     +               MONTHLY_INTERRUPTIBLE_REVENUE,
     +               LAST_TABLE_FOR_CG,
     +               LAST_TABLE_FOR_CG2,
     +               TABLE_DAY_SHIFT,
     +               MONTHLY_UNITS,
     +               PRICE_INDEX_ACTIVE,
     +               THREE_FACTOR_TRANSFORM,
     +               JURISDICTIONAL_CUSTOMER,
     +               FUEL_COST_RECOVERY_THROUGH_FAC,
     +               BASE_COST_OF_FAC_FUEL
      CHARACTER*256 OUTPUT_DIRECTORY,FILE_NAME
      CHARACTER*2 TSYFRC_OL
      INTEGER*2 ASSET_CLASS,
     +          ASSET_VECTOR
      INTEGER*2 NUMBER_OF_RATE_CLASSES,
     +           MAX_RATE_CLASS_ID_NUM,
     +          RATE_ASSET_CLASS_POINTER(1024)

      integer (kind=2) :: R_TG
      INTEGER* 2  HR_IN_MONTH,IREC,ALINE_LOAD_DATA,DA,DAYS_IN_MONTH,
     +            TIMZON,TEMPER,DELTMP,
     +            LDE_MONTH,LDE_DAY,LDE_YEAR,CURRENT_TRANS_GROUP,
     +            LOAD_UNIT,HR,MAX_HOURS_IN_MONTH/744/,R_HR,R_CG,
     +            R_PA,K,R_CM,
     +            GET_TRANS_LOAD_2_TRANS_GROUPS,
     +            GET_TRANS_LOAD_AGGREGATION,
     +            TRANS_GROUP,
     +            GET_HG_FROM_TG,
     +            MONTHLY_MAINTENANCE_PENALTY,
     +            SCENARIO_HOURLY_UNIT/2201/,
     +            TG_POSITION_IN_TG_FILE,
     +            R_AC,
     +            TSYFRC_REC
      INTEGER*4::DAILY_LOADS_I4(24),VALUES_2_ZERO,WD_DAILY_LOADS_I4(24)
      INTEGER*2   CHRONO_TRANS_HOUR(800)
     +               /
     +               01,02,03,04,05,06,07,08,09,10,
     +               11,12,13,14,15,16,17,18,19,20,
     +               21,22,23,24,25,26,27,28,29,30,
     +               31,32,33,34,35,36,37,38,39,40,
     +               41,42,43,44,45,46,47,48,49,50,
     +               51,52,53,54,55,56,57,58,59,60,
     +               61,62,63,64,65,66,67,68,69,70,
     +               71,72,73,74,75,76,77,78,79,80,
     +               81,82,83,84,85,86,87,88,89,90,
     +               91,92,93,94,95,96,97,98,99,100,
     +               101,102,103,104,105,106,107,108,109,110,
     +               111,112,113,114,115,116,117,118,119,120,
     +               121,122,123,124,125,126,127,128,129,130,
     +               131,132,133,134,135,136,137,138,139,140,
     +               141,142,143,144,145,146,147,148,149,150,
     +               151,152,153,154,155,156,157,158,159,160,
     +               161,162,163,164,165,166,167,168,169,170,
     +               171,172,173,174,175,176,177,178,179,180,
     +               181,182,183,184,185,186,187,188,189,190,
     +               191,192,193,194,195,196,197,198,199,200,
     +               201,202,203,204,205,206,207,208,209,210,
     +               211,212,213,214,215,216,217,218,219,220,
     +               221,222,223,224,225,226,227,228,229,230,
     +               231,232,233,234,235,236,237,238,239,240,
     +               241,242,243,244,245,246,247,248,249,250,
     +               251,252,253,254,255,256,257,258,259,260,
     +               261,262,263,264,265,266,267,268,269,270,
     +               271,272,273,274,275,276,277,278,279,280,
     +               281,282,283,284,285,286,287,288,289,290,
     +               291,292,293,294,295,296,297,298,299,300,
     +               301,302,303,304,305,306,307,308,309,310,
     +               311,312,313,314,315,316,317,318,319,320,
     +               321,322,323,324,325,326,327,328,329,330,
     +               331,332,333,334,335,336,337,338,339,340,
     +               341,342,343,344,345,346,347,348,349,350,
     +               351,352,353,354,355,356,357,358,359,360,
     +               361,362,363,364,365,366,367,368,369,370,
     +               371,372,373,374,375,376,377,378,379,380,
     +               381,382,383,384,385,386,387,388,389,390,
     +               391,392,393,394,395,396,397,398,399,400,
     +               401,402,403,404,405,406,407,408,409,410,
     +               411,412,413,414,415,416,417,418,419,420,
     +               421,422,423,424,425,426,427,428,429,430,
     +               431,432,433,434,435,436,437,438,439,440,
     +               441,442,443,444,445,446,447,448,449,450,
     +               451,452,453,454,455,456,457,458,459,460,
     +               461,462,463,464,465,466,467,468,469,470,
     +               471,472,473,474,475,476,477,478,479,480,
     +               481,482,483,484,485,486,487,488,489,490,
     +               491,492,493,494,495,496,497,498,499,500,
     +               501,502,503,504,505,506,507,508,509,510,
     +               511,512,513,514,515,516,517,518,519,520,
     +               521,522,523,524,525,526,527,528,529,530,
     +               531,532,533,534,535,536,537,538,539,540,
     +               541,542,543,544,545,546,547,548,549,550,
     +               551,552,553,554,555,556,557,558,559,560,
     +               561,562,563,564,565,566,567,568,569,570,
     +               571,572,573,574,575,576,577,578,579,580,
     +               581,582,583,584,585,586,587,588,589,590,
     +               591,592,593,594,595,596,597,598,599,600,
     +               601,602,603,604,605,606,607,608,609,610,
     +               611,612,613,614,615,616,617,618,619,620,
     +               621,622,623,624,625,626,627,628,629,630,
     +               631,632,633,634,635,636,637,638,639,640,
     +               641,642,643,644,645,646,647,648,649,650,
     +               651,652,653,654,655,656,657,658,659,660,
     +               661,662,663,664,665,666,667,668,669,670,
     +               671,672,673,674,675,676,677,678,679,680,
     +               681,682,683,684,685,686,687,688,689,690,
     +               691,692,693,694,695,696,697,698,699,700,
     +               701,702,703,704,705,706,707,708,709,710,
     +               711,712,713,714,715,716,717,718,719,720,
     +               721,722,723,724,725,726,727,728,729,730,
     +               731,732,733,734,735,736,737,738,739,740,
     +               741,742,743,744,745,746,747,748,749,750,
     +               751,752,753,754,755,756,757,758,759,760,
     +               761,762,763,764,765,766,767,768,769,770,
     +               771,772,773,774,775,776,777,778,779,780,
     +               781,782,783,784,785,786,787,788,789,790,
     +               791,792,793,794,795,796,797,798,799,800/,
     +            TEMP_HOURLY_INDEX(*)
      REAL*4  PERIOD_TRANSACTION_DEMAND,HOURLY_FORWARD_CONTRACT_ENERGY,
     +         HOURLY_FORWARD_SALE,HOURLY_TRANSACTION_LOAD,
     +         MONTH_BASE_LOAD, GET_VAR,
     +         TABLE_ENERGY_PRICE(:),LOCAL_DEMAND_PRICE,
     +         MONTHLY_MULT,
     +         HOURLY_MULT,
     +         SCENARIO_DAILY_LOADS_R4(24),
     +         DAILY_LOADS_R4(24),
     +         SCENARIO_ANNUAL_MULTIPLIER/1/,
     +         TABLE_SCENARIO_MULTIPLIER,
     +         TABLE_ANNUAL_MULTIPLIER/1/,
     +         TEMP_CHRONO_LOAD(:),
!
! DEFAULT MAINTENANCE PENALTY: USED FOR SERC.
!
     +         MAINTENANCE_PENALTY(12)/1.05,1.05,1.05,1.10,
     +                                 1.05,1.05,1.00,1.00,
     +                                 1.00,1.10,1.10,1.05/,
     +         R_MAINTENANCE_PENALTY(12)

      ALLOCATABLE :: TEMP_CHRONO_LOAD,TABLE_ENERGY_PRICE

      CHARACTER*2 LOAD_FILE_CHAR_EXT
      CHARACTER*5 BSYRLOAD
      CHARACTER*8 :: EEICODE=" "


! SIMULATION VARIABLES
!
      INTEGER*2 :: CUSTOMER_GROUP(:), ! INT2
     +                     CUSTOMER_GROUP_2(:),
     +                     TRANSACTION_GROUP(:), ! INT2
     +                     REFERENCE_LOAD_NUMBER(:), ! INT2
     +                     GLOBAL_PA_PEAK_MONTH(:),
     +                     GLOBAL_CM_PEAK_MONTH(:),
     +                     GET_PA_PEAK_MONTH,
     +                     GET_CM_PEAK_MONTH,
     +                     PA,CM,
     +                     TG_2_CAPACITY_MARKET(:),
     +                     REF_LEAP_YEAR_DAY_SHIFT(:),
     +                     GET_PA_FROM_TG
       integer (kind=2), allocatable ::    TG_2_PLANNING_AREA(:)

       INTEGER (kind=2) :: GET_CM_FROM_TG,
     +                     NUMBER_OF_ACTIVE_GROUPS,
     +                     GET_BELONGS_TO_GROUP
      REAL*4
     +                     MARKET_ENERGY_PRICE(:), ! REAL4
     +                     MONTHLY_ENERGY_PRICE_PATTERN(:), ! REAL4
     +                     MARKET_DEMAND_PRICE(:), ! REAL4
     +                     MONTHLY_DEMAND_PRICE_PATTERN(:), ! REAL4
     +                     MARKET_CUSTOMER_PRICE(:), ! REAL4
     +                     ASSET_CLASS_ID(:), ! REAL4
     +                     ASSET_CLASS_REV_ALLOC_VECTOR(:), ! REAL4
     +                     ANNUAL_ENERGY(:), ! REAL4
     +                     ANNUAL_PEAK(:), ! REAL4
     +                     ANNUAL_CUSTOMERS(:), ! REAL4
     +                     ANNUAL_MULTIPLIER(:), ! REAL4
     +                     MONTHLY_ENERGY(:,:), ! (12) REAL4
     +                     MONTHLY_PEAK(:,:), ! (12) REAL4
     +                     MONTHLY_CUSTOMERS(:,:), ! (12) REAL4
     +                     DIST_ENERGY_LOSS_FACTOR(:), ! REAL4
     +                     TRANS_ENERGY_LOSS_FACTOR(:), ! REAL4
     +                     PEAK_LOSS_FACTOR(:), ! REAL4
     +                     PEAK_COIN_FACTOR(:), ! REAL4
     +                     DISTRIBUTION_PRICE(:), ! REAL4
     +                     TRANSMISSION_PRICE(:), !REAL4!
     +                     LAST_THIS_YR_ENERGY(:,:,:),
     +                     LAST_THIS_YR_PEAK(:,:,:),
     +                     MONTHLY_TRANS_PEAK(:),
     +                     MONTHLY_TRANS_BASE(:),
     +                     MONTHLY_TABLE_ENERGY(:),
     +                     MONTHLY_TABLE_SALES_ENERGY(:),
     +                     MONTHLY_TABLE_PEAK_SALES(:,:),
     +                     MONTHLY_HYDRO_PEAK(:),
     +                     MONTHLY_HYDRO_BASE(:),
     +                     MONTHLY_TABLE_PEAK(:),
     +                     TABLE_ENERGY_REVENUE(:),
     +                     TABLE_DEMAND_REVENUE(:),
     +                     TABLE_CUSTOMER_REVENUE(:),
     +                     TRANS_ENERGY_REVENUE(:),
     +                     TRANS_DEMAND_REVENUE(:),
     +                     TRANS_CUSTOMER_REVENUE(:),
     +                     CLASS_ENERGY_REVENUE(:),
     +                     TRANS_INDEXED_REVENUE(:),
     +                     CLASS_PEAK_REVENUE(:),
     +                     CLASS_CUSTOMER_REVENUE(:),
     +                     CAP_CUSTOMER_EXPENSE(:),
     +                     TF_TG_CAP_MARKET_MW(:),
     +                     TF_TG_CAP_MARKET_COST(:),
     +                     R_TG_ENERGY,
     +                     R_TG_PEAK,
     +                     R_TG_BASE,
     +                     GET_TF_TG_CAP_MARKET_MW,
     +                     GET_TF_TG_CAP_MARKET_COST,
     +                     MONTHLY_CLASS_ENERGY(:),
     +                     MONTHLY_CLASS_PEAK(:),
     +                     MONTHLY_CLASS_CUSTOMERS(:),
     +                     ANNUAL_CLASS_ENERGY_REVENUE(:),
     +                     ANNUAL_TRANS_INDEXED_REVENUE(:),
     +                     ANNUAL_CLASS_PEAK_REVENUE(:),
     +                     ANNUAL_CLASS_CUSTOMER_REVENUE(:),
     +                     ANNUAL_CAP_CUSTOMER_EXPENSE(:),
     +                     ANNUAL_CLASS_ENERGY(:),
     +                     ANNUAL_CLASS_PEAK(:),
     +                     ANNUAL_CLASS_CUSTOMERS(:),
     +                     MINIMUM_MARKET_PRICE(:),
     +                     MAXIMUM_MARKET_PRICE(:),
     +                     INDEXED_ENERGY_PRICE(:),
     +                     TF_PLANNING_PEAK(:,:),
     +                     PA_PLANNING_PEAK(:,:),
     +                     CM_PLANNING_PEAK(:,:),
     +                     GET_PA_PEAK,
     +                     GLOBAL_PA_PEAK(:),
     +                     GLOBAL_CM_PEAK(:),
     +                     GLOBAL_PEAK,
     +                     ENERGY_LOSS_MULT(:),
     +                     R_13_ENERGY(0:12),
     +                     R_13_PEAK(0:12),
     +                     R_13_CUSTOMERS(0:12),
     +                     TG_CG_DATABASE(:,:,:,:),
     +                     TG_CG2_DATABASE(:,:,:,:),
     +                     INTERRUPTIBLE_MARKUP_PERENT(:),
     +                     INTERRUPTIBLE_MARKUP_CAP(:),
     +                     INTERRUPTIBLE_MAX_CAPACITY(:),
     +                     INTERRUPTIBLE_MIN_CAPACITY(:),
     +                     INTERRUPTIBLE_MARKUP_ADDER(:),
     +                     CUM_BLOCK_GENERATION(:,:)

!
!
      CHARACTER*20         TRANS_GROUP_NAME,
     +                     RTO_ISO_GROUP_NAME
      CHARACTER*6          PRIMARY_STATE
      INTEGER*2            capacity_market_pointer_array_tfo(:),
     +                     CAP_MARKET_MONTH_NO(:),
     +                     CAP_MARKET_TYPE_INDEX(:),
     +                     CAP_MARKET_MONTH_NO_INDEX,
     +                     TEMP_I2
      REAL*4               CAPACITY_MARKET_COIN_ADJ_FACT(:),
     +                     PRIMARY_FUEL_ID,
     +                     CAP_MARKET_RATE,
     +                     PLAN_FACTOR,
     +                     TEMP_R4
      CHARACTER*5
     +                     CAPACITY_MARKET_MONTH(:)
      CHARACTER*20         CAPACITY_MARKET_COST_ASSIGN(:),
     +                     CAPACITY_MARKET_EXP_COLLECT(:)
!
      REAL*8               MONTHLY_TRANS_ENERGY(:),
     +                     MONTHLY_HYDRO_ENERGY(:)
!
      CHARACTER*30
     +                     CUSTOMER_CLASS_NAME(:), ! CHAR*30
     +                     CALCULATION_MODE(:)*1 ! CHAR*1
      CHARACTER*5          REFERENCE_LOAD_NAME(:),TEMP_LOAD_NAME
      INTEGER*2            TEMP_LOAD_NUMBER
      ALLOCATABLE ::
     +                     CUSTOMER_GROUP, ! INT2
     +                     CUSTOMER_GROUP_2,
     +                     INTERRUPTIBLE_PRICING_ACTIVE,
     +                     INTERRUPTIBLE_MARKUP_PERENT,
     +                     INTERRUPTIBLE_MARKUP_CAP,
     +                     INTERRUPTIBLE_MAX_CAPACITY,
     +                     INTERRUPTIBLE_MIN_CAPACITY,
     +                     INTERRUPTIBLE_MARKUP_ADDER,
     +                     CAPACITY_MARKET_MONTH,
     +                     capacity_market_pointer_array_tfo,
     +                     CAP_MARKET_MONTH_NO,
     +                     CAP_MARKET_TYPE_INDEX,
     +                     CAPACITY_MARKET_EXP_COLLECT,
     +                     CAPACITY_MARKET_COST_ASSIGN,
     +                     CAPACITY_MARKET_COIN_ADJ_FACT,
     +                     SCENARIO_INDEX,
     +                     CUM_BLOCK_GENERATION,
     +                     HOURLY_INTERRUPIBLE_REVENUE,
     +                     CUSTOMER_CLASS_NAME, ! CHAR*30
     +                     CALCULATION_MODE, ! CHAR*1
     +                     TRANSACTION_GROUP, ! INT2
     +                     MARKET_ENERGY_PRICE, ! REAL4
     +                     MONTHLY_ENERGY_PRICE_PATTERN, ! REAL4
     +                     MARKET_DEMAND_PRICE, ! REAL4
     +                     MONTHLY_DEMAND_PRICE_PATTERN, ! REAL4
     +                     MARKET_CUSTOMER_PRICE, ! REAL4
     +                     ASSET_CLASS_ID, ! REAL4
     +                     ASSET_CLASS_REV_ALLOC_VECTOR, ! REAL4
     +                     REFERENCE_LOAD_NAME, ! CHAR*5
     +                     REFERENCE_LOAD_NUMBER, ! INT2
     +                     ANNUAL_ENERGY, ! REAL4
     +                     ANNUAL_PEAK, ! REAL4
     +                     ANNUAL_CUSTOMERS, ! REAL4
     +                     ANNUAL_MULTIPLIER, ! REAL4
     +                     MONTHLY_ENERGY, ! (12) REAL4
     +                     MONTHLY_PEAK, ! (12) REAL4
     +                     MONTHLY_CUSTOMERS, ! (12) REAL4
     +                     TG_CG_DATABASE,
     +                     TG_CG2_DATABASE,
     +                     DIST_ENERGY_LOSS_FACTOR, ! REAL4
     +                     TRANS_ENERGY_LOSS_FACTOR, ! REAL4
     +                     PEAK_LOSS_FACTOR, ! REAL4
     +                     PEAK_COIN_FACTOR, ! REAL4
     +                     DISTRIBUTION_PRICE, ! REAL4
     +                     TRANSMISSION_PRICE, !REAL4
     +                     LAST_THIS_YR_ENERGY,
     +                     LAST_THIS_YR_PEAK,
     +                     MONTHLY_TRANS_ENERGY,
     +                     MONTHLY_HYDRO_ENERGY,
     +                     MONTHLY_TRANS_PEAK,
     +                     MONTHLY_TRANS_BASE,
     +                     MONTHLY_HYDRO_PEAK,
     +                     MONTHLY_HYDRO_BASE,
     +                     MONTHLY_TABLE_ENERGY,
     +                     MONTHLY_TABLE_SALES_ENERGY,
     +                     MONTHLY_TABLE_PEAK_SALES,
     +                     MONTHLY_TABLE_PEAK,
     +                     TABLE_ENERGY_REVENUE,
     +                     TABLE_DEMAND_REVENUE,
     +                     TABLE_CUSTOMER_REVENUE,
     +                     TRANS_ENERGY_REVENUE,
     +                     TRANS_DEMAND_REVENUE,
     +                     TRANS_CUSTOMER_REVENUE,
     +                     CLASS_ENERGY_REVENUE,
     +                     TRANS_INDEXED_REVENUE,
     +                     CLASS_PEAK_REVENUE,
     +                     CLASS_CUSTOMER_REVENUE,
     +                     CAP_CUSTOMER_EXPENSE,
     +                     TF_TG_CAP_MARKET_MW,
     +                     TF_TG_CAP_MARKET_COST,
     +                     MONTHLY_CLASS_ENERGY,
     +                     MONTHLY_CLASS_PEAK,
     +                     MONTHLY_CLASS_CUSTOMERS,
     +                     ANNUAL_CLASS_ENERGY_REVENUE,
     +                     ANNUAL_TRANS_INDEXED_REVENUE,
     +                     ANNUAL_CLASS_PEAK_REVENUE,
     +                     ANNUAL_CLASS_CUSTOMER_REVENUE,
     +                     ANNUAL_CAP_CUSTOMER_EXPENSE,
     +                     ANNUAL_CLASS_ENERGY,
     +                     ANNUAL_CLASS_PEAK,
     +                     ANNUAL_CLASS_CUSTOMERS,
     +                     LOCAL_CUSTOMER_NAME,
     +                     MINIMUM_MARKET_PRICE,
     +                     MAXIMUM_MARKET_PRICE,
     +                     INDEXED_ENERGY_PRICE,
     +                     TF_PLANNING_PEAK,
     +                     PA_PLANNING_PEAK,
     +                     CM_PLANNING_PEAK,
     +                     GLOBAL_PA_PEAK,
     +                     GLOBAL_CM_PEAK,
     +                     GLOBAL_PA_PEAK_MONTH,
     +                     GLOBAL_CM_PEAK_MONTH,
     +                     TG_2_CAPACITY_MARKET,
     +                     ENERGY_LOSS_MULT,
     +                     REF_LEAP_YEAR_DAY_SHIFT,
     +                     REV_CLASS_INDEX,
     +                     DEMAND_PRICING_METHOD,
     +                     INTRA_COMPANY_TRANSACTION,
     +                     INTRA_ASSET_CLASS_ID,
     +                     INTRA_ASSET_CLASS_ALLOCATION,
     +                     INTRA_ACCOUNT_CLASSIFICATION,
     +                     INTRA_EXPENSE_COLLECTION
!
      REAL*4   AEL_PEAK(12),AEL_PEAKMAX,
     +         AEL_BASE(12),AEL_BASEMIN,
     +         AEL_ENERGY(12),AEL_ENRGYEAR,
     +         FINAL_HOURLY_LOAD,
     +         FINAL_HOURLY_SALES,
     +         MONTH_AVE_ENERGY,
     +         MONTH_AVE_HIST_ENERGY,
     +         MONTHLY_SLOPE,MONTHLY_INTERCEPT,
     +         TOTAL_CLASS_ENERGY_REVENUE,
     +         TOTAL_TRANS_INDEXED_REVENUE,
     +         TOTAL_CLASS_PEAK_REVENUE,
     +         TOTAL_CLASS_CUSTOMER_REVENUE,
     +         TOTAL_CAP_CUSTOMER_EXPENSE,
     +         TOTAL_MONTHLY_CLASS_ENERGY,
     +         TOTAL_MONTHLY_CLASS_PEAK,
     +         TOTAL_MONTHLY_CLASS_CUSTOMERS,
     +         GET_GROUP_PEAK_ON_PEAK_MONTH,
     +         GET_PEAK_ON_PA_PEAK_MONTH,
     +         GET_CUST_GROUP_PEAK,
     +         GET_CUST_GROUP_ENERGY,
     +         REMAIN,
     +         R_MONTH_COST,
     +         R_MONTH_REVENUE,
     +         PEAK_LOSS_MULT/1.0/,
     +         WEEKLY_HYDRO_ENERGY,
     +         WEEKLY_HYDRO_LOADS(168),
     +         TF_LOADS_PER_HOUR(800),
     +         WEEKLY_HYDRO_MINIMUM_MW,
     +         WEEKLY_HYDRO_MAXIMUM_MW,
     +         SUM_24_I4,PEAK_24_I4,BASE_24_I4,
     +         GET_WH_MONTH_CAPACITY,
     +         GET_WH_MONTH_ENERGY,
     +         FUEL_AND_PURCHASE_COST_CAP,
     +         SYS_EMERGENCY_MW_FLOOR,
     +         SYS_EMERGENCY_COST_CAP
!
!
! CUSTOMER ANALYST DECLARATIONS
!
      LOGICAL*1   WRITE_MONTHLY_CLASS_SUMMARY,
     +            INIT_ANN_ALLOC_BLOCKS_2_CUST,
     +            INDIANA_FAC,
     +            RESOURCE_TO_LOAD_ALLOC,
     +            YES_RESOURCE_TO_LOAD_ALLOC,
     +            CALENDAR_CORRECT/.FALSE./,YES_CALANDER_CORRECT,
     +            APPLY_LEAP_YEAR_LOGIC,ADJUST_FOR_LEAP_YEAR/.FALSE./,
     +            PUT_MONTHLY_EP_COST_REV
      INTEGER*2 MAX_CUST_CLASS_GROUPS/0/,CG,MAX_CLASS_GROUPS/2048/,
     +          MAX_ASSET_CLASS_GROUPS/0/,AC,
     +          MAX_CUST2_CLASS_GROUPS/0/,CG2,MAX_CLASS2_GROUPS/2048/,
     +          HG
      integer (kind=2) :: MAX_ASSET_GROUPS
!
! DETAILED REPORT OVERHEAD
!
      LOGICAL*1   TF_CLASS_REPORT_NOT_OPEN/.TRUE./,
     +            TRANS_WH_REPORT_NOT_OPEN/.TRUE./,
     +            MARKET_AREA_REPORT_NOT_OPEN/.TRUE./,
     +            LOAD_BY_BLOCK_RPT_NOT_OPEN/.TRUE./,
     +            HOURLY_CUST_MARGIN_NOT_OPEN/.TRUE./,
     +            TRANS_INTR_NOT_OPEN/.TRUE./,
     +            IPL_FAC_RPT_NOT_OPEN/.TRUE./,
     +            YES_LOAD_MARKET_AREAS_REPORT,
     +            MARKET_AREA_REPORT,
     +            R_BY_UNIT,IPL_FAC_REPORT,YES_IPL_FAC_REPORT,
     +            YES_MONTHLY_CLASS_REPORTS,TF_CLASS_SUMMARY_REPORTS
!
      INTEGER*2   LAST_SEASON/0/,
     +            TRANS_CLASS_SUM_UNIT,TRANS_CLASS_SUM_HEADER,
     +            CURRENT_MONTH,LEAP_YEAR_DAY_SHIFT,
     +            ANNUAL_COUNTER/0/,PRODUCTION_PERIODS,SAVE_HOURS/0/,
     +            TRANS_WH_UNIT/0/,WH_HOURLY_HEADER,
     +            MARKET_AREA_UNIT/0/,MARKET_AREA_RPT_HEADER,
     +            LOAD_BY_BLOCK_NO/0/,MON_LOAD_BY_BLOCK_HEADER,
     +            HOURLY_CUST_MARGIN_NO/0/,HOURLY_CUST_MARGIN_HEADER,
     +            TRANS_INTR_NO/0/,TRANS_INTR_RPT_HEADER,
     +            HOURLY_CUST_MARGIN_VAR,
     +            IPL_FAC_NO/0/,
     +            LOAD_BLOCK_VAR,IPL_FAC_HEADER,IPL_FAC_VAR
      INTEGER TRANS_CLASS_SUM_REC,TRANS_WH_REC,MARKET_AREA_REC,
     +         LOAD_BLOCK_REC,IPL_FAC_REC,HOURLY_CUST_MARGIN_REC,
     +         TRANS_INTR_REC
      REAL*4   IPL_FAC_DB(15),ANN_IPL_FAC_DB(15),
     +         ELECT_PLAN_DATABASE(0:12,3),
     +         R_ELECT_PLAN_FUEL(0:12),
     +         R_ELECT_PLAN_PURCHASE(0:12)
      CHARACTER*9 CL_MONTH_NAME(13)/
     +      'January','February','March','April','May','June',
     +      'July','August','September','October',
     +      'November','December','Annual'/
      CHARACTER*4  VAR_NUM_STR
      CHARACTER*5  GET_SCENAME
      CHARACTER*15 LEFT_JUSTIFY_I2_IN_STR
      CHARACTER* 20 CL_NAME
      CHARACTER* 22 CL_BLOCK_NAME
      LOGICAL*1 LAHEY_LF95
      INTEGER START_REC,ROLLOVER_VALUE
!
      INTEGER*2 RETURN_ANNUL_CUSTOMER_VARIABLES
      REAL*4 ANNUAL_VARS_Purchased_Power,
     +       ANNUAL_VARS_Secondary_Sales,
     +       ANNUAL_VARS_Capacity_Sales,
     +       ANNUAL_VARS_Customer_Revenue,
     +       ANNUAL_VAR_Residential_Revenue,
     +       ANNUAL_VARS_Competitive_Sales,
     +       ANNUAL_VARS_Utility_Sales,
     +       ANNUAL_VARS_Commercial_Revenues,
     +       ANNUAL_VARS_Industrial_Revenues,
     +       ANNUAL_VARS_Lighting_Revenues,
     +       ANNUAL_VARS_Bulk_Power_Revenues,
     +       ANNUAL_VARS_Government_Sales,
     +       ANNUAL_VARS_Capacity_Purchases,
     +       FE_Competitive_Unit_Sales

      INTEGER*2 RETURN_MONTH_CUSTOMER_VARIABLES,
     +          RETURN_MONTH_CUSTOMER_REVENUES,
     +          RETURN_CUSTOMER_CASH_REVENUES
      INTEGER*2 RETURN_FE_PNL_REVENUES
      INTEGER*2 ACCT,ITRA_ID
      REAL R_UTILITY_SALES,
     +     R_UTILITY_SALES_QUANT,
     +     R_UTILITY_SALES_LOSS,
     +     R_COMPETITIVE_SALES(9),
     +     R_COMPETITIVE_SALES_QUANT(9),
     +     R_COMPETITIVE_LOSS(9)
      REAL*4 NOT_AVAIL
      PARAMETER(NOT_AVAIL=-999999.)
      REAL*4 ENRG_AVERAGE_REVENUE,
     +       INDEXED_ENRG_AVERAGE_REVENUE,
     +       PEAK_AVERAGE_REVENUE,
     +       CUSTOMER_AVERAGE_REVENUE,
     +       PRICING_PEAK
      INTEGER (KIND = 2) :: LOCAL_DATA_YEAR
      INTEGER (KIND=2) :: DUMB_INT2
      integer (kind=2) :: gtgp_index
      integer :: file_trace_atl
      REAL (KIND=4) :: DUMB_REAL
      CHARACTER (LEN=5) :: DUMB_CHR5
      CHARACTER (LEN=1) :: DUMB_CHR1
      REAL (KIND=4), ALLOCATABLE, DIMENSION(:) ::
     +                      ESC_MARKET_ENERGY_PRICE,
     +                      ESC_MARKET_DEMAND_PRICE,
     +                      ESC_MARKET_CUSTOMER_PRICE,
     +                      ESC_DISTRIBUTION_PRICE,
     +                      ESC_TRANSMISSION_PRICE,
     +                      ESC_INDEXED_ENERGY_PRICE
!


!
! END DATA DECLARATIONS
!
!     CALLED FROM INSIDE PROCOST, BEFORE PERIOD LOOP
!
         MANAGE_TRANSACTION_FORECASTS = .TRUE.
      RETURN


      ENTRY ANNUAL_TRANSACTION_LOADS(R_YEAR_TEMP)
         if (file_trace_atl==0) then
            file_trace_atl=open_trace("annual_transaction_loads.trace", 
     +          rq_atl)
         end if
         


         CALL DOES_TF_FILE_EXIST(TF_FILE_EXISTS)
         call write_trace_bool1(file_trace_atl, "TF_FILE_EXISTS", 
     + TF_FILE_EXISTS)
         SAVE_TF_FILE_EXISTS = TF_FILE_EXISTS
         IF(.NOT. TF_FILE_EXISTS) then
            call write_trace_message(file_trace_atl, "1. RETURN")
            RETURN
         end if
!
! WHEN THE 30TH YEAR IS REACHED SAVE THE 29TH YEAR
!

       call write_trace_int2(file_trace_atl, "AVAIL_DATA_YEARS", 
     + AVAIL_DATA_YEARS)
     
         IF(R_YEAR_TEMP == AVAIL_DATA_YEARS) THEN ! STORE THE VALUES
            IF(ALLOCATED(ESC_MARKET_ENERGY_PRICE))
     +                       DEALLOCATE(ESC_MARKET_ENERGY_PRICE,
     +                                  ESC_MARKET_DEMAND_PRICE,
     +                                  ESC_MARKET_CUSTOMER_PRICE,
     +                                  ESC_DISTRIBUTION_PRICE,
     +                                  ESC_TRANSMISSION_PRICE,
     +                                  ESC_INDEXED_ENERGY_PRICE)
             ALLOCATE(
     +           ESC_MARKET_ENERGY_PRICE(MAX_TRANS_LOAD_TABLES),
     +           ESC_MARKET_DEMAND_PRICE(MAX_TRANS_LOAD_TABLES),
     +           ESC_MARKET_CUSTOMER_PRICE(MAX_TRANS_LOAD_TABLES),
     +           ESC_DISTRIBUTION_PRICE(MAX_TRANS_LOAD_TABLES),
     +           ESC_TRANSMISSION_PRICE(MAX_TRANS_LOAD_TABLES),
     +           ESC_INDEXED_ENERGY_PRICE(MAX_TRANS_LOAD_TABLES))
     
     
     
            ESC_MARKET_ENERGY_PRICE = MARKET_ENERGY_PRICE
            
            ESC_MARKET_DEMAND_PRICE = MARKET_DEMAND_PRICE
            ESC_MARKET_CUSTOMER_PRICE = MARKET_CUSTOMER_PRICE
            ESC_DISTRIBUTION_PRICE = DISTRIBUTION_PRICE
            ESC_TRANSMISSION_PRICE = TRANSMISSION_PRICE
            ESC_INDEXED_ENERGY_PRICE = INDEXED_ENERGY_PRICE
         ENDIF
         call write_trace_int2(file_trace_atl, "R_YEAR_TEMP", 
     + R_YEAR_TEMP)
         call write_trace_int2(file_trace_atl, "EXTENSION_PERIOD_START",
     + EXTENSION_PERIOD_START())
     
         IF(R_YEAR_TEMP >= EXTENSION_PERIOD_START()) THEN
            IF(R_YEAR_TEMP <= AVAIL_DATA_YEARS) THEN ! READ FROM FILE
! READ PRICES/COSTS FROM DATA FILE
               FILE_NAME = trim(OUTPUT_DIRECTORY())//
     +                                           TSYFRC_OL//"TSYFC.BIN"
               OPEN(FILE=FILE_NAME,UNIT=951,ACCESS='DIRECT',
     +                                                 RECL=TSYFRC_REC)
               TRANS_YEAR_RECORD = R_YEAR_TEMP

          call write_trace_int2(file_trace_atl, "MAX_TRANS_LOAD_TABLES",
     + MAX_TRANS_LOAD_TABLES)
               DO TRANS = 1, MAX_TRANS_LOAD_TABLES

                  READ(951,REC=TRANS_YEAR_RECORD) DELETE,
     +                     TEMP_YEAR, ! INT2
     +                     DUMB_INT2, ! CUSTOMER_GROUP(TRANS), ! INT2
     +                     CUSTOMER_CLASS_NAME(TRANS), ! CHAR*30
     +                     CALCULATION_MODE(TRANS), ! CHAR*1
     +                     DUMB_INT2, ! TRANSACTION_GROUP(TRANS), ! IN
     +                     MARKET_ENERGY_PRICE(TRANS), ! REAL4
     +                     MONTHLY_ENERGY_PRICE_PATTERN(TRANS), ! REAL4
     +                     MARKET_DEMAND_PRICE(TRANS), ! REAL4
     +                     MONTHLY_DEMAND_PRICE_PATTERN(TRANS), ! REAL4
     +                     MARKET_CUSTOMER_PRICE(TRANS), ! REAL4
     +                     DUMB_REAL, ! ASSET_CLASS_ID(TRANS), ! REAL4
     +                     DUMB_REAL,
     +                     DUMB_CHR5,
     +                     DUMB_INT2,
     +                     DUMB_REAL, ! ANNUAL_ENERGY(TRANS), ! REAL4
     +                     DUMB_REAL, ! ANNUAL_PEAK(TRANS), ! REAL4
     +                     DUMB_REAL,
     +                     DUMB_REAL,
     +                     (DUMB_REAL,I=1,12),
     +                     (DUMB_REAL,I=1,12),
     +                     (DUMB_REAL,I=1,12),
     +                     DUMB_REAL,
     +                     DUMB_REAL,
     +                     DUMB_REAL,
     +                     DUMB_REAL,
     +                     DISTRIBUTION_PRICE(TRANS), ! REAL4
     +                     TRANSMISSION_PRICE(TRANS), !REAL4
     +                     DUMB_CHR1, ! TABLE_ACTIVE(TRANS),
     +                     BASECASE_MKT_AREA_ID_tfo(TRANS),
     +                     BASECASE_TRANS_AREA_ID(TRANS),
     +                     BASECASE_NERC_SUB_ID(TRANS),
     +                     MONTHLY_UNITS(TRANS),
     +                     MINIMUM_MARKET_PRICE(TRANS),
     +                     MAXIMUM_MARKET_PRICE(TRANS),
     +                     INDEXED_ENERGY_PRICE(TRANS),
     +                     PRICE_INDEX_ACTIVE(TRANS),
     +                     HESI_TRANS_AREA_ID_NUM,
     +                     REVENUE_CLASS_ENERGY,
     +                     REVENUE_CLASS_DEMAND,
     +                     REVENUE_CLASS_CUST,
     +                     REVENUE_INDEX_ENERGY,
     +                     DEMAND_PRICING_METHOD(TRANS),
     +                     INTRA_COMPANY_TRANSACTION(TRANS),
     +                     INTRA_ASSET_CLASS_ID(TRANS),
     +                     INTRA_ASSET_CLASS_ALLOCATION(TRANS),
     +                     INTRA_ACCOUNT_CLASSIFICATION(TRANS),
     +                     INTRA_EXPENSE_COLLECTION(TRANS),
     +                     FILE_TABLE_INDEX,
     +                     SCENARIO_VARIABLE,
     +                     TEMP_DISPATCH_POSITION,
     +                     THREE_FACTOR_TRANSFORM(TRANS),
     +                     JURISDICTIONAL_CUSTOMER(TRANS),
     +                     FUEL_COST_RECOVERY_THROUGH_FAC(TRANS),
     +                     BASE_COST_OF_FAC_FUEL(TRANS),
     +                     DUMB_INT2, ! CUSTOMER_GROUP_2(TRANS),
     +                     INTERRUPTIBLE_PRICING_ACTIVE(TRANS),
     +                     INTERRUPTIBLE_MARKUP_PERENT(TRANS),
     +                     INTERRUPTIBLE_MARKUP_CAP(TRANS),
     +                     INTERRUPTIBLE_MAX_CAPACITY(TRANS),
     +                     INTERRUPTIBLE_MIN_CAPACITY(TRANS),
     +                     INTERRUPTIBLE_MARKUP_ADDER(TRANS),
     +                     TRANS_GROUP_NAME,
     +                     RTO_ISO_GROUP_NAME,
     +                     PRIMARY_STATE,
     +                     CAPACITY_MARKET_MONTH(TRANS),
     +                     capacity_market_pointer_array_tfo(TRANS),
     +                     CAPACITY_MARKET_COST_ASSIGN(TRANS),
     +                     CAPACITY_MARKET_EXP_COLLECT(TRANS),
     +                     CAPACITY_MARKET_COIN_ADJ_FACT(TRANS)
!
                TRANS_YEAR_RECORD = TRANS_YEAR_RECORD+AVAIL_DATA_YEARS
                
               if(file_trace_atl/=BAD_TRACE_HANDLE) then
                write(file_trace_atl, *) 
     + "TEMP_YEAR ", TEMP_YEAR,
     + "CUSTOMER_CLASS_NAME ", CUSTOMER_CLASS_NAME(TRANS), 
     + "CALCULATION_MODE ", CALCULATION_MODE(TRANS),
     + "MARKET_ENERGY_PRICE ", MARKET_ENERGY_PRICE(TRANS), 
     + "MONTHLY_ENERGY_PRICE_PATTERN ", 
     + MONTHLY_ENERGY_PRICE_PATTERN(TRANS), 
     + "MARKET_DEMAND_PRICE ", MARKET_DEMAND_PRICE(TRANS), 
     + "MONTHLY_DEMAND_PRICE_PATTERN ", 
     + MONTHLY_DEMAND_PRICE_PATTERN(TRANS), 
     + "MARKET_CUSTOMER_PRICE ", MARKET_CUSTOMER_PRICE(TRANS),
     + "DISTRIBUTION_PRICE ", DISTRIBUTION_PRICE(TRANS),
     + "TRANSMISSION_PRICE ", TRANSMISSION_PRICE(TRANS),
     + "BASECASE_MARKET_AREA_ID ",BASECASE_MKT_AREA_ID_tfo(TRANS),
     + "BASECASE_TRANS_AREA_ID ", BASECASE_TRANS_AREA_ID(TRANS),
     + "BASECASE_NERC_SUB_ID ", BASECASE_NERC_SUB_ID(TRANS),
     + "MONTHLY_UNITS ", MONTHLY_UNITS(TRANS),
     + "MINIMUM_MARKET_PRICE ", MINIMUM_MARKET_PRICE(TRANS),
     + "MAXIMUM_MARKET_PRICE ", MAXIMUM_MARKET_PRICE(TRANS),
     + "INDEXED_ENERGY_PRICE ", INDEXED_ENERGY_PRICE(TRANS),
     + "PRICE_INDEX_ACTIVE ", PRICE_INDEX_ACTIVE(TRANS),
     + "HESI_TRANS_AREA_ID_NUM ", HESI_TRANS_AREA_ID_NUM,
     + "REVENUE_CLASS_ENERGY ", REVENUE_CLASS_ENERGY,
     + "REVENUE_CLASS_DEMAND ", REVENUE_CLASS_DEMAND,
     + "REVENUE_CLASS_CUST ", REVENUE_CLASS_CUST,
     + "REVENUE_INDEX_ENERGY ", REVENUE_INDEX_ENERGY,
     + "DEMAND_PRICING_METHOD ", DEMAND_PRICING_METHOD(TRANS),
     + "INTRA_COMPANY_TRANSACTION ", INTRA_COMPANY_TRANSACTION(TRANS),
     + "INTRA_ASSET_CLASS_ID ", INTRA_ASSET_CLASS_ID(TRANS),
     + "INTRA_ASSET_CLASS_ALLOCATION ", 
     + INTRA_ASSET_CLASS_ALLOCATION(TRANS),
     + "INTRA_ACCOUNT_CLASSIFICATION ", 
     + INTRA_ACCOUNT_CLASSIFICATION(TRANS),
     + "INTRA_EXPENSE_COLLECTION ", INTRA_EXPENSE_COLLECTION(TRANS),
     + "FILE_TABLE_INDEX ", FILE_TABLE_INDEX,
     + "SCENARIO_VARIABLE ", SCENARIO_VARIABLE,
     + "TEMP_DISPATCH_POSITION ", TEMP_DISPATCH_POSITION,
     + "THREE_FACTOR_TRANSFORM ", THREE_FACTOR_TRANSFORM(TRANS),
     + "JURISDICTIONAL_CUSTOMER ", JURISDICTIONAL_CUSTOMER(TRANS),
     + "FUEL_COST_RECOVERY_THROUGH_FAC ", 
     + FUEL_COST_RECOVERY_THROUGH_FAC(TRANS),
     + "BASE_COST_OF_FAC_FUEL ", BASE_COST_OF_FAC_FUEL(TRANS),
     + "INTERRUPTIBLE_PRICING_ACTIVE ", 
     + INTERRUPTIBLE_PRICING_ACTIVE(TRANS),
     + "INTERRUPTIBLE_MARKUP_PERENT ", 
     + INTERRUPTIBLE_MARKUP_PERENT(TRANS),
     + "INTERRUPTIBLE_MARKUP_CAP ", INTERRUPTIBLE_MARKUP_CAP(TRANS),
     + "INTERRUPTIBLE_MAX_CAPACITY ", INTERRUPTIBLE_MAX_CAPACITY(TRANS),
     + "INTERRUPTIBLE_MIN_CAPACITY ", INTERRUPTIBLE_MIN_CAPACITY(TRANS),
     + "INTERRUPTIBLE_MARKUP_ADDER ", INTERRUPTIBLE_MARKUP_ADDER(TRANS),
     + "TRANS_GROUP_NAME ", TRANS_GROUP_NAME,
     + "RTO_ISO_GROUP_NAME ", RTO_ISO_GROUP_NAME,
     + "PRIMARY_STATE ", PRIMARY_STATE,
     + "CAPACITY_MARKET_MONTH ", CAPACITY_MARKET_MONTH(TRANS),
     + "capacity_market_pointer_array ", 
     + capacity_market_pointer_array_tfo(TRANS),
     + "CAPACITY_MARKET_COST_ASSIGN ", 
     + CAPACITY_MARKET_COST_ASSIGN(TRANS),
     + "CAPACITY_MARKET_EXP_COLLECT ", 
     + CAPACITY_MARKET_EXP_COLLECT(TRANS),
     + "CAPACITY_MARKET_COIN_ADJ_FACT ",  
     + CAPACITY_MARKET_COIN_ADJ_FACT(TRANS),
     + "RECORD ", TRANS_YEAR_RECORD-1
               end if
               
               ENDDO
               CLOSE(951)
               ! CALCULATE IMPLIED ESCALATION RATES
               IF(R_YEAR_TEMP == AVAIL_DATA_YEARS) THEN
                 DO TRANS = 1, MAX_TRANS_LOAD_TABLES
                    CALL IMPLIED_ESC(ESC_MARKET_ENERGY_PRICE(TRANS),
     +                               MARKET_ENERGY_PRICE(TRANS))
                    CALL IMPLIED_ESC(ESC_MARKET_DEMAND_PRICE(TRANS),
     +                               MARKET_DEMAND_PRICE(TRANS))
                    CALL IMPLIED_ESC(ESC_MARKET_CUSTOMER_PRICE(TRANS),
     +                               MARKET_CUSTOMER_PRICE(TRANS))
                    CALL IMPLIED_ESC(ESC_DISTRIBUTION_PRICE(TRANS),
     +                               DISTRIBUTION_PRICE(TRANS))
                    CALL IMPLIED_ESC(ESC_TRANSMISSION_PRICE(TRANS),
     +                               TRANSMISSION_PRICE(TRANS))
                    CALL IMPLIED_ESC(ESC_INDEXED_ENERGY_PRICE(TRANS),
     +                               INDEXED_ENERGY_PRICE(TRANS))
                 ENDDO
               ENDIF
            ELSE ! ESCALATE PRICES/COSTS
               MARKET_ENERGY_PRICE = MARKET_ENERGY_PRICE *
     +                                          ESC_MARKET_ENERGY_PRICE
               MARKET_DEMAND_PRICE = MARKET_DEMAND_PRICE *
     +                                          ESC_MARKET_DEMAND_PRICE
               MARKET_CUSTOMER_PRICE = MARKET_CUSTOMER_PRICE *
     +                                        ESC_MARKET_CUSTOMER_PRICE
               DISTRIBUTION_PRICE = DISTRIBUTION_PRICE *
     +                                           ESC_DISTRIBUTION_PRICE
               TRANSMISSION_PRICE = TRANSMISSION_PRICE *
     +                                           ESC_TRANSMISSION_PRICE
               INDEXED_ENERGY_PRICE = INDEXED_ENERGY_PRICE *
     +                                         ESC_INDEXED_ENERGY_PRICE
            ENDIF
            MONTHLY_AC_COST_AT_MARKET = 0.
            MONTHLY_AC_CONTRACT_REVENUE = 0.
            MONTHLY_AC_CONTRACT_EXPENSE = 0.
            RETURN !FORECASTS DO NOT CHANGE
         ENDIF
         R_YEAR = MIN(R_YEAR_TEMP ,AVAIL_DATA_YEARS)
         RUN_TRANSACT = YES_RUN_TRANSACT()
         MARKET_AREA_REPORT = YES_LOAD_MARKET_AREAS_REPORT()
         USE_MARKET_AREA_REPORT_ID = YES_USE_MARKET_AREA_REPORT_ID()
!
         ZONAL_LEVEL_MARKET = YES_ZONAL_LEVEL_MARKET()
!
         YES_ASSET_ANALYST_ONLY = ASSET_ANALYST_ONLY()
!
! 10/13/04. FOR BURESH
!
         MID_TERM_PEAK_UNCER = YES_MID_TERM_PEAK_UNCER()
!
         IF(.NOT. RUN_TRANSACT .AND. .NOT.
     +                                   YES_ASSET_ANALYST_ONLY) RETURN
!
         LOCAL_DATA_YEAR = R_YEAR + get_BASE_YEAR()
         LOCAL_YEAR = R_YEAR_TEMP + get_BASE_YEAR()
!
         CALL TF_FILE_USED_THIS_ENDPOINT(TF_FILE_USED)
!
!         IF(R_YEAR == 1) THEN
         IF(.NOT. TF_FILE_USED) THEN
!
            CALL GET_TRANS_LOAD_TABLES(MAX_TRANS_LOAD_TABLES)

         ENDIF
!
         CALL RETURN_TSYFRC_OL(TSYFRC_OL,TSYFRC_REC)
!
! 256 FOR NATIONAL DATA BASE
!
!         MAX_TRANS_GROUPS_FROM_TG = GET_MAX_TRANS_GROUPS()
         MAX_TRANS_GROUPS_FROM_TG = MAX(1,GET_MAX_TRANS_GROUP_NUMBER())
!
         CALL RETURN_TF_GROUP_INFO( NUM_OF_TRANS_CLASSES,
     +                              MAX_TRANS_CLASS_NUM,
     +                              NUM_OF_ASSET_CLASSES,
     +                              MAX_ASSET_CLASS_NUM,
     +                              NUM_OF_CUST_CLASSES,
     +                              MAX_CUST_CLASS_NUM)
         MAX_TRANS_GROUPS = MAX(MAX_TRANS_CLASS_NUM,
     +                                        MAX_TRANS_GROUPS_FROM_TG)
         MAX_ASSET_GROUPS = MAX_ASSET_CLASS_NUM
         MAX_CLASS_GROUPS = MAX(MAX_CUST_CLASS_NUM,1)
!
       NUMBER_OF_ACTIVE_GROUPS = GET_NUMBER_OF_ACTIVE_GROUPS()
!
         MONTHLY_MAINT_VECTOR = FLOAT(MONTHLY_MAINTENANCE_PENALTY())
         IF(MONTHLY_MAINT_VECTOR == 0) THEN
            MAINTENANCE_PENALTY(1) = 1.0
            MAINTENANCE_PENALTY(2) = 1.0
            MAINTENANCE_PENALTY(3) = 1.0
            MAINTENANCE_PENALTY(4) = 1.0
            MAINTENANCE_PENALTY(5) = 1.0
            MAINTENANCE_PENALTY(6) = 1.0
            MAINTENANCE_PENALTY(7) = 1.0
            MAINTENANCE_PENALTY(8) = 1.0
            MAINTENANCE_PENALTY(9) = 1.0
            MAINTENANCE_PENALTY(10) = 1.0
            MAINTENANCE_PENALTY(11) = 1.0
            MAINTENANCE_PENALTY(12) = 1.0
         ELSE
            DO LOCAL_MONTH = 1, 12
               MAINTENANCE_PENALTY(LOCAL_MONTH) =
     +                 GET_VAR(MONTHLY_MAINT_VECTOR,LOCAL_MONTH,
     +                                          "Maintenance Penalty ")
            ENDDO
         ENDIF
! 8/30/01
         PRICE_ONLY_WHOLESALE_REV = APPLY_TRANS_REV_TO_WHOLESALE()
         IGNORE_NAT_PUR_USE_W = IGNORE_NATIVE_PURCHASE_USE_W()
!
         FILE_NAME = trim(OUTPUT_DIRECTORY())//TSYFRC_OL//"TSYFC.BIN"
         INQUIRE(FILE=FILE_NAME,NUMBER=UNIT_NO,
     +             OPENED=LOAD_FILE_OPEN,EXIST=FILE_EXISTS)
         IF(LOAD_FILE_OPEN) THEN
            CLOSE(UNIT_NO,IOSTAT=IOS)
            WRITE(4,*) "TRANSACTION LOAD FILE IS OPEN BEFORE"
            WRITE(4,*) "IT IS EXPECTED.  THIS MAY CAUSE SIMULATION"
            WRITE(4,*) "PROBLEMS. FILE HAS BEEN CLOSED."
         ENDIF
         OPEN(FILE=FILE_NAME,UNIT=951,ACCESS='DIRECT',RECL=TSYFRC_REC)


         NUMBER_OF_RATE_CLASSES = 0
         MAX_RATE_CLASS_ID_NUM = 0
!
         IF( .NOT. ALLOCATED(CUSTOMER_GROUP)) THEN
            ALLOCATE(CUSTOMER_GROUP(MAX_TRANS_LOAD_TABLES)) ! INT2
            ALLOCATE(CUSTOMER_GROUP_2(MAX_TRANS_LOAD_TABLES))
            ALLOCATE(
     +            INTERRUPTIBLE_PRICING_ACTIVE(MAX_TRANS_LOAD_TABLES),
     +            INTERRUPTIBLE_MARKUP_PERENT(MAX_TRANS_LOAD_TABLES),
     +            INTERRUPTIBLE_MARKUP_CAP(MAX_TRANS_LOAD_TABLES),
     +            INTERRUPTIBLE_MAX_CAPACITY(MAX_TRANS_LOAD_TABLES),
     +            INTERRUPTIBLE_MIN_CAPACITY(MAX_TRANS_LOAD_TABLES),
     +            INTERRUPTIBLE_MARKUP_ADDER(MAX_TRANS_LOAD_TABLES),
     +            CAPACITY_MARKET_MONTH(MAX_TRANS_LOAD_TABLES),
     +        capacity_market_pointer_array_tfo(MAX_TRANS_LOAD_TABLES),
     +            CAP_MARKET_MONTH_NO(MAX_TRANS_LOAD_TABLES),
     +            CAP_MARKET_TYPE_INDEX(MAX_TRANS_LOAD_TABLES),
     +            CAPACITY_MARKET_EXP_COLLECT(MAX_TRANS_LOAD_TABLES),
     +            CAPACITY_MARKET_COST_ASSIGN(MAX_TRANS_LOAD_TABLES),
     +            CAPACITY_MARKET_COIN_ADJ_FACT(MAX_TRANS_LOAD_TABLES),
     +       HOURLY_INTERRUPIBLE_REVENUE(MAX_TRANS_LOAD_TABLES,24,4))
            ALLOCATE(SCENARIO_INDEX(MAX_TRANS_LOAD_TABLES))
            ALLOCATE(TRANSACTION_GROUP(MAX_TRANS_LOAD_TABLES)) ! INT2
           ALLOCATE(REFERENCE_LOAD_NUMBER(MAX_TRANS_LOAD_TABLES)) ! INT2
            ALLOCATE(MARKET_ENERGY_PRICE(MAX_TRANS_LOAD_TABLES)) ! REAL4
            ALLOCATE(MONTHLY_ENERGY_PRICE_PATTERN(
     +                                  MAX_TRANS_LOAD_TABLES))  ! REAL4
            ALLOCATE(MARKET_DEMAND_PRICE(MAX_TRANS_LOAD_TABLES)) ! REAL4
            ALLOCATE(MONTHLY_DEMAND_PRICE_PATTERN(
     +                                  MAX_TRANS_LOAD_TABLES)) ! REAL4
          ALLOCATE(MARKET_CUSTOMER_PRICE(MAX_TRANS_LOAD_TABLES)) ! REAL4
            ALLOCATE(ASSET_CLASS_ID(MAX_TRANS_LOAD_TABLES)) ! REAL4
            ALLOCATE(ASSET_CLASS_REV_ALLOC_VECTOR(
     +                               MAX_TRANS_LOAD_TABLES))  ! REAL4
            ALLOCATE(ANNUAL_ENERGY(MAX_TRANS_LOAD_TABLES)) ! REAL4
            ALLOCATE(ANNUAL_PEAK(MAX_TRANS_LOAD_TABLES)) ! REAL4
            ALLOCATE(ANNUAL_CUSTOMERS(MAX_TRANS_LOAD_TABLES)) ! REAL4
            ALLOCATE(ANNUAL_MULTIPLIER(MAX_TRANS_LOAD_TABLES)) ! REAL4
         ALLOCATE(MONTHLY_ENERGY(12,MAX_TRANS_LOAD_TABLES)) ! (12) REAL4
          ALLOCATE(MONTHLY_PEAK(12,MAX_TRANS_LOAD_TABLES)) ! (12) REAL4
      ALLOCATE(MONTHLY_CUSTOMERS(12,MAX_TRANS_LOAD_TABLES)) ! (12) REAL4
        ALLOCATE(DIST_ENERGY_LOSS_FACTOR(MAX_TRANS_LOAD_TABLES)) ! REAL4
       ALLOCATE(TRANS_ENERGY_LOSS_FACTOR(MAX_TRANS_LOAD_TABLES)) ! REAL4
            ALLOCATE(PEAK_LOSS_FACTOR(MAX_TRANS_LOAD_TABLES)) ! REAL4
            ALLOCATE(PEAK_COIN_FACTOR(MAX_TRANS_LOAD_TABLES)) ! REAL4
            ALLOCATE(DISTRIBUTION_PRICE(MAX_TRANS_LOAD_TABLES)) ! REAL4
            ALLOCATE(TRANSMISSION_PRICE(MAX_TRANS_LOAD_TABLES)) !REAL4!
          ALLOCATE(CUSTOMER_CLASS_NAME(MAX_TRANS_LOAD_TABLES)) !
            ALLOCATE(CALCULATION_MODE(MAX_TRANS_LOAD_TABLES)) ! CHAR*1
           ALLOCATE(REFERENCE_LOAD_NAME(MAX_TRANS_LOAD_TABLES)) ! CHAR*5
            ALLOCATE(TRANS_MONTHLY_ENERGY(12,MAX_TRANS_LOAD_TABLES))
            ALLOCATE(WH_TRANS_MONTHLY_ENERGY(12,
     +                                       MAX_TRANS_GROUPS_FROM_TG))
            ALLOCATE(WH_TRANS_MONTHLY_CAPACITY(12,
     +                                       MAX_TRANS_GROUPS_FROM_TG))
            ALLOCATE(TRANS_MONTHLY_PEAK(12,MAX_TRANS_LOAD_TABLES))
            ALLOCATE(TRANS_MONTHLY_CUSTOMERS(12,MAX_TRANS_LOAD_TABLES))
            ALLOCATE(TABLE_ACTIVE(MAX_TRANS_LOAD_TABLES))
            ALLOCATE(BASECASE_MKT_AREA_ID_tfo(MAX_TRANS_LOAD_TABLES))
            ALLOCATE(WD_INDEX(MAX_TRANS_LOAD_TABLES))
            ALLOCATE(BASECASE_TRANS_AREA_ID(MAX_TRANS_LOAD_TABLES))
            ALLOCATE(BASECASE_NERC_SUB_ID(MAX_TRANS_LOAD_TABLES))
            ALLOCATE(MONTHLY_UNITS(MAX_TRANS_LOAD_TABLES))
            ALLOCATE(PRICE_INDEX_ACTIVE(MAX_TRANS_LOAD_TABLES))
            ALLOCATE(THREE_FACTOR_TRANSFORM(MAX_TRANS_LOAD_TABLES))
            ALLOCATE(JURISDICTIONAL_CUSTOMER(MAX_TRANS_LOAD_TABLES))
            ALLOCATE(FUEL_COST_RECOVERY_THROUGH_FAC(
     +                                          MAX_TRANS_LOAD_TABLES))
            ALLOCATE(BASE_COST_OF_FAC_FUEL(MAX_TRANS_LOAD_TABLES))
            ALLOCATE(MINIMUM_MARKET_PRICE(MAX_TRANS_LOAD_TABLES))
            ALLOCATE(MAXIMUM_MARKET_PRICE(MAX_TRANS_LOAD_TABLES))
            ALLOCATE(INDEXED_ENERGY_PRICE(MAX_TRANS_LOAD_TABLES))
            ALLOCATE(TG_COUNTER(MAX_TRANS_GROUPS))
            ALLOCATE(LOAD_DISPATCH_POSITION(MAX_TRANS_LOAD_TABLES,
     +                                               MAX_TRANS_GROUPS))
            ALLOCATE(LOAD_DISPATCH_INDEX(MAX_TRANS_LOAD_TABLES,
     +                                               MAX_TRANS_GROUPS))
!
            ALLOCATE(TRANS_LOAD_GROUPS_INDEX(MAX_TRANS_GROUPS))
            ALLOCATE(CUST_CLASS_GROUPS_INDEX(0:MAX_CLASS_GROUPS))
            ALLOCATE(CUST2_CLASS_GROUPS_INDEX(0:MAX_CLASS2_GROUPS))
            ALLOCATE(ASSET_CLASS_GROUPS_INDEX(0:MAX_ASSET_GROUPS))
            ALLOCATE(ASSET_2_TRANS_INDEX(0:MAX_ASSET_GROUPS,
     +                                               MAX_TRANS_GROUPS))
            ALLOCATE(NUMBER_ASSET_2_TRANS(MAX_TRANS_GROUPS))
            ALLOCATE(ASSET_TRANSACTION_CROSS_INDEX(0:MAX_ASSET_GROUPS,
     +                                               MAX_TRANS_GROUPS))
            ALLOCATE(NUMBER_TRANS_PER_AC_TG(0:MAX_ASSET_GROUPS,
     +                                               MAX_TRANS_GROUPS))
            ALLOCATE(TRANS_WITHIN_AC_TG(0:MAX_ASSET_GROUPS,
     +                         MAX_TRANS_GROUPS,MAX_TRANS_LOAD_TABLES))
            ALLOCATE(FIRST_AC_TG(0:MAX_ASSET_GROUPS))
            ALLOCATE(FIRST_TABLE_TG(0:MAX_TRANS_GROUPS))
            ALLOCATE(TRANS_LOAD_2_TRANS_GROUPS(MAX_TRANS_GROUPS))
            ALLOCATE(TRANS_LOAD_GROUP_2_TG(MAX_TRANS_GROUPS))
            ALLOCATE(CUST_CLASS_GROUP_2_CG(MAX_CLASS_GROUPS+1))
            ALLOCATE(CUST2_CLASS_GROUP_2_CG(MAX_CLASS_GROUPS+1))
            ALLOCATE(ASSET_CLASS_GROUP_2_AC(0:MAX_ASSET_GROUPS+1))
            ALLOCATE(LAST_TABLE_FOR_TG(MAX_TRANS_GROUPS))
            ALLOCATE(LAST_TABLE_FOR_CG(MAX_CLASS_GROUPS+1))
            ALLOCATE(LAST_TABLE_FOR_CG2(MAX_CLASS_GROUPS+1))
            ALLOCATE(TG_CG_DATABASE(0:12,0:MAX_TRANS_GROUPS,
     +                                           0:MAX_CLASS_GROUPS,3))
            ALLOCATE(TG_CG2_DATABASE(0:12,0:MAX_TRANS_GROUPS,
     +                                           0:MAX_CLASS_GROUPS,3))
            ALLOCATE(TABLE_DAY_SHIFT(MAX_TRANS_LOAD_TABLES))
            ALLOCATE(TF_PLANNING_PEAK(0:MAX_TRANS_GROUPS,12))
            ALLOCATE(PA_PLANNING_PEAK(0:NUMBER_OF_ACTIVE_GROUPS,12))
            ALLOCATE(CM_PLANNING_PEAK(0:NUMBER_OF_ACTIVE_GROUPS,12))
            ALLOCATE(GLOBAL_PA_PEAK(0:NUMBER_OF_ACTIVE_GROUPS))
            ALLOCATE(GLOBAL_CM_PEAK(0:NUMBER_OF_ACTIVE_GROUPS))
            ALLOCATE(GLOBAL_PA_PEAK_MONTH(0:NUMBER_OF_ACTIVE_GROUPS))
            ALLOCATE(GLOBAL_CM_PEAK_MONTH(0:NUMBER_OF_ACTIVE_GROUPS))
            ALLOCATE(TG_2_PLANNING_AREA(MAX_TRANS_GROUPS_FROM_TG))
            ALLOCATE(TG_2_CAPACITY_MARKET(MAX_TRANS_GROUPS_FROM_TG))
            ALLOCATE(REV_CLASS_INDEX(0:MAX_TRANS_LOAD_TABLES,4))
            ALLOCATE(ENERGY_LOSS_MULT(MAX_TRANS_LOAD_TABLES))
            ALLOCATE(REF_LEAP_YEAR_DAY_SHIFT(MAX_TRANS_LOAD_TABLES))
            ALLOCATE(DEMAND_PRICING_METHOD(MAX_TRANS_LOAD_TABLES))
           ALLOCATE(MONTHLY_TABLE_PEAK_SALES(MAX_TRANS_LOAD_TABLES,12))
            ALLOCATE(INTRA_COMPANY_TRANSACTION(MAX_TRANS_LOAD_TABLES))
            ALLOCATE(INTRA_ASSET_CLASS_ID(MAX_TRANS_LOAD_TABLES))
            ALLOCATE(INTRA_ASSET_CLASS_ALLOCATION(
     +                                          MAX_TRANS_LOAD_TABLES))
            ALLOCATE(INTRA_ACCOUNT_CLASSIFICATION(
     +                                          MAX_TRANS_LOAD_TABLES))
            ALLOCATE(INTRA_EXPENSE_COLLECTION(MAX_TRANS_LOAD_TABLES))
         ENDIF
!
         WH_TRANS_MONTHLY_ENERGY = 0.
         MONTHLY_TABLE_PEAK_SALES = 0.
         WH_TRANS_MONTHLY_CAPACITY = 0.
         TF_PLANNING_PEAK = 0.
!
! ALLOWS FOR REGIONAL RESERVE PLANNING: E.G. BC, ALBERTA IN THE WSCC
!
         PA_PLANNING_PEAK = 0.
         CM_PLANNING_PEAK = 0.
         GLOBAL_PA_PEAK = 0.
         GLOBAL_CM_PEAK = 0.
         GLOBAL_PA_PEAK_MONTH = 0
         GLOBAL_CM_PEAK_MONTH = 0
         TG_2_PLANNING_AREA = 0
         TG_2_CAPACITY_MARKET = 0
!
         ENERGY_LOSS_MULT = 1.
         REF_LEAP_YEAR_DAY_SHIFT = 0
!
! WD_INDEX 5/13/01.
         WD_INDEX = -1
         CALL DOES_WEATHER_DEMAND_FILE_EXIST(WD_FILE_EXISTS)
! WH_INDEX 6/11/01.
         CALL DOES_WEEKLY_HYDRO_FILE_EXIST(WH_FILE_EXISTS)
!
         TRANS_LOAD_GROUPS_INDEX = 0
         CUST_CLASS_GROUPS_INDEX = 0
         CUST2_CLASS_GROUPS_INDEX = 0
         ASSET_CLASS_GROUPS_INDEX = 0
!
         ! WHICH ASSET CLASSES BELONG TO A TRANSACTION GROUP
         ASSET_2_TRANS_INDEX = 0

         ! HOW MANY ASSET CLASSES BELONG TO THE TRANSACTION GROUP
         NUMBER_ASSET_2_TRANS = 0
         ASSET_TRANSACTION_CROSS_INDEX = 0
! 9/30/98. GAT.
         NUMBER_TRANS_PER_AC_TG = 0
         TRANS_WITHIN_AC_TG = 0
!
         FIRST_AC_TG = 0
         FIRST_TABLE_TG = 0
         TRANS_LOAD_2_TRANS_GROUPS = 0
         TRANS_LOAD_GROUP_2_TG = 0.
         CUST_CLASS_GROUP_2_CG = 0
         CUST2_CLASS_GROUP_2_CG = 0
         ASSET_CLASS_GROUP_2_AC = 0
         LAST_TABLE_FOR_TG = 0
         LAST_TABLE_FOR_CG = 0
         LAST_TABLE_FOR_CG2 = 0
         TABLE_DAY_SHIFT = 0
         REV_CLASS_INDEX = 0
!
         LOAD_DISPATCH_INDEX = 0
         TG_COUNTER = 0
         LOAD_DISPATCH_POSITION = 0
         TG_CG_DATABASE = 0.
         TG_CG2_DATABASE = 0.
!
! 02/25/05.
!
         IF(ALLOCATED(MONTHLY_INTERRUPTIBLE_REVENUE))
     +                       DEALLOCATE(MONTHLY_INTERRUPTIBLE_REVENUE)
         ALLOCATE(MONTHLY_INTERRUPTIBLE_REVENUE(
     +                                      0:MAX_ASSET_GROUPS+1,0:12))
         MONTHLY_INTERRUPTIBLE_REVENUE = 0.
!
         CALENDAR_CORRECT = YES_CALANDER_CORRECT()

         CALL DAYWEEK(INT2(1),INT2(2),LOCAL_DATA_YEAR,
     +                                            CALENDAR_DAY_OF_WEEK)

         APPLY_LEAP_YEAR_LOGIC = .TRUE.
         REMAIN = MOD(get_BASE_YEAR()+R_YEAR-1964.,4.)
         IF( REMAIN < .001 .AND. APPLY_LEAP_YEAR_LOGIC) THEN
            ADJUST_FOR_LEAP_YEAR = .TRUE.
         ELSE
            ADJUST_FOR_LEAP_YEAR = .FALSE.
         ENDIF
!
         USING_INDEXED_PRICING = .FALSE.
!
         TRANS_YEAR_RECORD = R_YEAR
         MAX_TRANS_LOAD_GROUPS = 0
         MAX_CUST_CLASS_GROUPS = 0
         MAX_CUST2_CLASS_GROUPS = 0
         MAX_ASSET_CLASS_GROUPS = 0
!
         MAX_TRANS_GROUP_NUMBER = 0
         MAX_CUST_GROUP_NUMBER = 0
         MAX_CUST2_GROUP_NUMBER = 0
!
         NUMBER_OF_HYDRO_GROUPS = GET_NUMBER_OF_HYDRO_GROUPS()
!
         SCENARIO_ANNUAL_MULTIPLIER = 1.0
!
         INTERRUPTIBLE_PRICING = .FALSE.
!
         CAP_MARKET_MONTH_NO = 0
         CAP_MARKET_TYPE_INDEX = 0
         capacity_market_pointer_array_tfo = 0
!
         IF(.NOT. ALLOCATED( LAST_THIS_YR_ENERGY) ) THEN
            ! 1=LAST_YEAR, 2=THIS YEAR
            ALLOCATE(LAST_THIS_YR_ENERGY(2,12,0:MAX_TRANS_LOAD_TABLES))
            ALLOCATE(LAST_THIS_YR_PEAK(2,12,0:MAX_TRANS_LOAD_TABLES))
            LAST_THIS_YR_ENERGY = 0.
            LAST_THIS_YR_PEAK = 0.
         ENDIF
!
!         DO I =1, NUMBER_OF_ACTIVE_GROUPS
!         ENDDO
!
         FILE_TABLE_2_TRANS_INDEX = 0
!
         DO I = 1, 12
            FUT_PEAK(3,I) = 0. ! THIS SHOULD BE AFTER HYDRO
            LAST_THIS_YR_ENERGY(2,I,0) = 0.0
            LAST_THIS_YR_PEAK(2,I,0) = 0.0
!            TF_PLANNING_PEAK(I) = 0.0
         ENDDO
!
         HIGHEST_TG_COUNT = 0
         call write_trace_message(file_trace_atl, "2. read")
         DO TRANS = 1, MAX_TRANS_LOAD_TABLES
            READ(951,REC=TRANS_YEAR_RECORD) DELETE,
     +                     TEMP_YEAR, ! INT2
     +                     CUSTOMER_GROUP(TRANS), ! INT2
     +                     CUSTOMER_CLASS_NAME(TRANS), ! CHAR*30
     +                     CALCULATION_MODE(TRANS), ! CHAR*1
     +                     TRANSACTION_GROUP(TRANS), ! INT2
     +                     MARKET_ENERGY_PRICE(TRANS), ! REAL4
     +                     MONTHLY_ENERGY_PRICE_PATTERN(TRANS), ! REAL4
     +                     MARKET_DEMAND_PRICE(TRANS), ! REAL4
     +                     MONTHLY_DEMAND_PRICE_PATTERN(TRANS), ! REAL4
     +                     MARKET_CUSTOMER_PRICE(TRANS), ! REAL4
     +                     ASSET_CLASS_ID(TRANS), ! REAL4
     +                     ASSET_CLASS_REV_ALLOC_VECTOR(TRANS), ! REAL4
     +                     REFERENCE_LOAD_NAME(TRANS), ! CHAR*5
     +                     REFERENCE_LOAD_NUMBER(TRANS), ! INT2
     +                     ANNUAL_ENERGY(TRANS), ! REAL4
     +                     ANNUAL_PEAK(TRANS), ! REAL4
     +                     ANNUAL_CUSTOMERS(TRANS), ! REAL4
     +                     ANNUAL_MULTIPLIER(TRANS), ! REAL4
     +                 (MONTHLY_ENERGY(I,TRANS),I=1,12), ! (12) REAL4
     +                     (MONTHLY_PEAK(I,TRANS),I=1,12), ! (12) REAL4
     +               (MONTHLY_CUSTOMERS(I,TRANS),I=1,12), ! (12) REAL4
     +                     DIST_ENERGY_LOSS_FACTOR(TRANS), ! REAL4
     +                     TRANS_ENERGY_LOSS_FACTOR(TRANS), ! REAL4
     +                     PEAK_LOSS_FACTOR(TRANS), ! REAL4
     +                     PEAK_COIN_FACTOR(TRANS), ! REAL4
     +                     DISTRIBUTION_PRICE(TRANS), ! REAL4
     +                     TRANSMISSION_PRICE(TRANS), !REAL4
     +                     TABLE_ACTIVE(TRANS),
     +                     BASECASE_MKT_AREA_ID_tfo(TRANS),
     +                     BASECASE_TRANS_AREA_ID(TRANS),
     +                     BASECASE_NERC_SUB_ID(TRANS),
     +                     MONTHLY_UNITS(TRANS),
     +                     MINIMUM_MARKET_PRICE(TRANS),
     +                     MAXIMUM_MARKET_PRICE(TRANS),
     +                     INDEXED_ENERGY_PRICE(TRANS),
     +                     PRICE_INDEX_ACTIVE(TRANS),
     +                     HESI_TRANS_AREA_ID_NUM,
     +                     REVENUE_CLASS_ENERGY,
     +                     REVENUE_CLASS_DEMAND,
     +                     REVENUE_CLASS_CUST,
     +                     REVENUE_INDEX_ENERGY,
     +                     DEMAND_PRICING_METHOD(TRANS),
     +                     INTRA_COMPANY_TRANSACTION(TRANS),
     +                     INTRA_ASSET_CLASS_ID(TRANS),
     +                     INTRA_ASSET_CLASS_ALLOCATION(TRANS),
     +                     INTRA_ACCOUNT_CLASSIFICATION(TRANS),
     +                     INTRA_EXPENSE_COLLECTION(TRANS),
     +                     FILE_TABLE_INDEX,
     +                     SCENARIO_VARIABLE,
     +                     TEMP_DISPATCH_POSITION,
     +                     THREE_FACTOR_TRANSFORM(TRANS),
     +                     JURISDICTIONAL_CUSTOMER(TRANS),
     +                     FUEL_COST_RECOVERY_THROUGH_FAC(TRANS),
     +                     BASE_COST_OF_FAC_FUEL(TRANS),
     +                     CUSTOMER_GROUP_2(TRANS),
     +                     INTERRUPTIBLE_PRICING_ACTIVE(TRANS),
     +                     INTERRUPTIBLE_MARKUP_PERENT(TRANS),
     +                     INTERRUPTIBLE_MARKUP_CAP(TRANS),
     +                     INTERRUPTIBLE_MAX_CAPACITY(TRANS),
     +                     INTERRUPTIBLE_MIN_CAPACITY(TRANS),
     +                     INTERRUPTIBLE_MARKUP_ADDER(TRANS),
     +                     TRANS_GROUP_NAME,
     +                     RTO_ISO_GROUP_NAME,
     +                     PRIMARY_STATE,
     +                     CAPACITY_MARKET_MONTH(TRANS),
     +                     capacity_market_pointer_array_tfo(TRANS),
     +                     CAPACITY_MARKET_COST_ASSIGN(TRANS),
     +                     CAPACITY_MARKET_EXP_COLLECT(TRANS),
     +                     CAPACITY_MARKET_COIN_ADJ_FACT(TRANS)
!
       if(file_trace_atl/=BAD_TRACE_HANDLE) then
         write(file_trace_atl,*) TRANS_YEAR_RECORD, 
     +  "TEMP_YEAR ", TEMP_YEAR, 
     +  "CUSTOMER_GROUP ", CUSTOMER_GROUP(TRANS), 
     +  "CUSTOMER_CLASS_NAME ", CUSTOMER_CLASS_NAME(TRANS),
     +  "CALCULATION_MODE ", CALCULATION_MODE(TRANS), 
     +  "TRANSACTION_GROUP ", TRANSACTION_GROUP(TRANS), 
     +  "MARKET_ENERGY_PRICE ", MARKET_ENERGY_PRICE(TRANS), 
     +  "MONTHLY_ENERGY_PRICE_PATTERN ", 
     + MONTHLY_ENERGY_PRICE_PATTERN(TRANS), 
     +  "MARKET_DEMAND_PRICE ", MARKET_DEMAND_PRICE(TRANS), 
     +  "MONTHLY_DEMAND_PRICE_PATTERN ", 
     + MONTHLY_DEMAND_PRICE_PATTERN(TRANS), 
     +  "MARKET_CUSTOMER_PRICE ", MARKET_CUSTOMER_PRICE(TRANS), 
     +  "ASSET_CLASS_ID ", ASSET_CLASS_ID(TRANS), 
     +  "ASSET_CLASS_REV_ALLOC_VECTOR ", 
     + ASSET_CLASS_REV_ALLOC_VECTOR(TRANS), 
     +  "REFERENCE_LOAD_NAME ", REFERENCE_LOAD_NAME(TRANS), 
     +  "REFERENCE_LOAD_NUMBER ", REFERENCE_LOAD_NUMBER(TRANS), 
     +  "ANNUAL_ENERGY ", ANNUAL_ENERGY(TRANS), 
     +  "ANNUAL_PEAK ", ANNUAL_PEAK(TRANS), 
     +  "ANNUAL_CUSTOMERS ", ANNUAL_CUSTOMERS(TRANS), 
     +  "ANNUAL_MULTIPLIER ", ANNUAL_MULTIPLIER(TRANS), 
     +  "(MONTHLY_ENERGY ", (MONTHLY_ENERGY(I,TRANS),I=1,12), 
     +  "(MONTHLY_PEAK ", (MONTHLY_PEAK(I,TRANS),I=1,12), 
     +  "(MONTHLY_CUSTOMERS ", (MONTHLY_CUSTOMERS(I,TRANS),I=1,12), 
     +  "DIST_ENERGY_LOSS_FACTOR ", DIST_ENERGY_LOSS_FACTOR(TRANS),
     +  "TRANS_ENERGY_LOSS_FACTOR ", TRANS_ENERGY_LOSS_FACTOR(TRANS),
     +  "PEAK_LOSS_FACTOR ", PEAK_LOSS_FACTOR(TRANS), 
     +  "PEAK_COIN_FACTOR ", PEAK_COIN_FACTOR(TRANS), 
     +  "DISTRIBUTION_PRICE ", DISTRIBUTION_PRICE(TRANS),
     +  "TRANSMISSION_PRICE ", TRANSMISSION_PRICE(TRANS),
     +  "TABLE_ACTIVE ", TABLE_ACTIVE(TRANS),
     +  "BASECASE_MARKET_AREA_ID ", BASECASE_MKT_AREA_ID_tfo(TRANS), 
     +  "BASECASE_TRANS_AREA_ID ", BASECASE_TRANS_AREA_ID(TRANS),
     +  "BASECASE_NERC_SUB_ID ", BASECASE_NERC_SUB_ID(TRANS),
     +  "MONTHLY_UNITS ", MONTHLY_UNITS(TRANS),
     +  "MINIMUM_MARKET_PRICE ", MINIMUM_MARKET_PRICE(TRANS),
     +  "MAXIMUM_MARKET_PRICE ", MAXIMUM_MARKET_PRICE(TRANS),
     +  "INDEXED_ENERGY_PRICE ", INDEXED_ENERGY_PRICE(TRANS),
     +  "PRICE_INDEX_ACTIVE ", PRICE_INDEX_ACTIVE(TRANS),
     +  "HESI_TRANS_AREA_ID_NUM ", HESI_TRANS_AREA_ID_NUM,
     +  "REVENUE_CLASS_ENERGY ", REVENUE_CLASS_ENERGY,
     +  "REVENUE_CLASS_DEMAND ", REVENUE_CLASS_DEMAND,
     +  "REVENUE_CLASS_CUST ", REVENUE_CLASS_CUST,
     +  "REVENUE_INDEX_ENERGY ", REVENUE_INDEX_ENERGY,
     +  "DEMAND_PRICING_METHOD ", DEMAND_PRICING_METHOD(TRANS),
     +  "INTRA_COMPANY_TRANSACTION ", INTRA_COMPANY_TRANSACTION(TRANS),
     +  "INTRA_ASSET_CLASS_ID ", INTRA_ASSET_CLASS_ID(TRANS),
     +  "INTRA_ASSET_CLASS_ALLOCATION ", 
     + INTRA_ASSET_CLASS_ALLOCATION(TRANS),
     +  "INTRA_ACCOUNT_CLASSIFICATION ", 
     + INTRA_ACCOUNT_CLASSIFICATION(TRANS),
     +  "INTRA_EXPENSE_COLLECTION ", INTRA_EXPENSE_COLLECTION(TRANS),
     +  "FILE_TABLE_INDEX ", FILE_TABLE_INDEX,
     +  "SCENARIO_VARIABLE ", SCENARIO_VARIABLE,
     +  "TEMP_DISPATCH_POSITION ", TEMP_DISPATCH_POSITION,
     +  "THREE_FACTOR_TRANSFORM ", THREE_FACTOR_TRANSFORM(TRANS),
     +  "JURISDICTIONAL_CUSTOMER ", JURISDICTIONAL_CUSTOMER(TRANS),
     +  "FUEL_COST_RECOVERY_THROUGH_FAC ", 
     + FUEL_COST_RECOVERY_THROUGH_FAC(TRANS),
     +  "BASE_COST_OF_FAC_FUEL ", BASE_COST_OF_FAC_FUEL(TRANS),
     +  "CUSTOMER_GROUP_2 ", CUSTOMER_GROUP_2(TRANS),
     +  "INTERRUPTIBLE_PRICING_ACTIVE ", 
     + INTERRUPTIBLE_PRICING_ACTIVE(TRANS),
     +  "INTERRUPTIBLE_MARKUP_PERENT ", 
     + INTERRUPTIBLE_MARKUP_PERENT(TRANS),
     +  "INTERRUPTIBLE_MARKUP_CAP ", INTERRUPTIBLE_MARKUP_CAP(TRANS),
     +  "INTERRUPTIBLE_MAX_CAPACITY ", 
     + INTERRUPTIBLE_MAX_CAPACITY(TRANS),
     +  "INTERRUPTIBLE_MIN_CAPACITY ", 
     + INTERRUPTIBLE_MIN_CAPACITY(TRANS),
     +  "INTERRUPTIBLE_MARKUP_ADDER ", 
     + INTERRUPTIBLE_MARKUP_ADDER(TRANS),
     +  "TRANS_GROUP_NAME ", TRANS_GROUP_NAME,            
     +  "RTO_ISO_GROUP_NAME ", RTO_ISO_GROUP_NAME,
     +  "PRIMARY_STATE ", PRIMARY_STATE,
     +  "CAPACITY_MARKET_MONTH ", CAPACITY_MARKET_MONTH(TRANS),
     +  "CAPACITY_MARKET_POINTER ", 
     + capacity_market_pointer_array_tfo(TRANS),
     +  "CAPACITY_MARKET_COST_ASSIGN ", 
     + CAPACITY_MARKET_COST_ASSIGN(TRANS),
     +  "CAPACITY_MARKET_EXP_COLLECT ", 
     + CAPACITY_MARKET_EXP_COLLECT(TRANS),
     +  "CAPACITY_MARKET_COIN_ADJ_FACT ", 
     + CAPACITY_MARKET_COIN_ADJ_FACT(TRANS)
       end if
            TRANS_YEAR_RECORD = TRANS_YEAR_RECORD + AVAIL_DATA_YEARS
!
! 10/03/02. FOR REGIONAL CONSOLIDATION. NOTE REASSIGNMENT.
! 05/07/03. ALTERED.
!
            IF(INTERRUPTIBLE_PRICING_ACTIVE(TRANS) /= 'F')
     +                                   INTERRUPTIBLE_PRICING = .TRUE.
!
!
! 10/15/04.
!
            IF(ZONAL_LEVEL_MARKET) THEN
               MARKET_ID = BASECASE_MKT_AREA_ID_tfo(TRANS)
               TRANSACTION_GROUP(TRANS) =
     +                                    MARKET_AREA_LOOKUP(MARKET_ID)
            ENDIF
!
            IF( .NOT. YES_ASSET_ANALYST_ONLY) THEN
               TRANSACTION_GROUP(TRANS) =
     +                   GET_BELONGS_TO_GROUP(TRANSACTION_GROUP(TRANS))
            ENDIF
!
            IF(TRANSACTION_GROUP(TRANS) == 0) then
                call write_trace_message(file_trace_atl, "1. cycle")
                CYCLE ! SO THAT IT ISN'T STOPPED BELOW
            endif
!

       call write_trace_int2(file_trace_atl, "TRANS", TRANS)
       
            ! Divergent call happens here.
            TG = TRANSACTION_GROUP(TRANS)
       call write_trace_int2(file_trace_atl, "TG", TG)
! 062404
            TGP = GET_TRANS_GROUP_POSITION(TG)
            CG = CUSTOMER_GROUP(TRANS)
            CG2 = CUSTOMER_GROUP_2(TRANS)
            AC = ASSET_CLASS_ID(TRANS)
!
            REV_CLASS_INDEX(TRANS,1) =
     +                  INCOME_STATEMENT_POSITION(REVENUE_CLASS_ENERGY)
            REV_CLASS_INDEX(TRANS,2) =
     +                  INCOME_STATEMENT_POSITION(REVENUE_CLASS_DEMAND)
            REV_CLASS_INDEX(TRANS,3) =
     +                    INCOME_STATEMENT_POSITION(REVENUE_CLASS_CUST)
            REV_CLASS_INDEX(TRANS,4) =
     +                  INCOME_STATEMENT_POSITION(REVENUE_INDEX_ENERGY)
!
            IF(PRICE_INDEX_ACTIVE(TRANS) == 'T') THEN
               USING_INDEXED_PRICING = .TRUE.
            ENDIF
!
            WD_INDEX(TRANS) =
     +          MARKET_AREA_LOOKUP(BASECASE_MKT_AREA_ID_tfo(TRANS)(1:5))
! 9/28/98. GAT.
            IF(CALCULATION_MODE(TRANS) == 'S' .AND.
     +                                 TABLE_ACTIVE(TRANS) == 'T') THEN
               SCENARIO_ANNUAL_MULTIPLIER = ANNUAL_MULTIPLIER(TRANS)
               TABLE_ACTIVE(TRANS) = 'F'
               CYCLE
            ENDIF
!
            IF(TG < 1 .OR. TG > MAX_TRANS_GROUPS) THEN
               WRITE(4,*) "In the Transaction Forecast file,"
               WRITE(4,*) "in Table ",CUSTOMER_CLASS_NAME(TRANS)
               WRITE(4,*) "and Table number ",TRANS
               WRITE(4,*) "the Transaction Group ",TG
               WRITE(4,*) "is outside the known range."
               WRITE(4,*) '*** line 1851 TF_OBJT.FOR ***'
!
               CYCLE

            ENDIF
            IF( (RUN_TRANSACT .AND.
     +               (.NOT. TRANS_GROUP_ACTIVE_SWITCH(TG)) .OR.
     +                .NOT. IN_ACTIVE_LOADS_MARKET_AREA(
     +                                BASECASE_MKT_AREA_ID_tfo(TRANS)))
     +                            .OR.
     +                                TABLE_ACTIVE(TRANS) == 'F') CYCLE
!
            FILE_TABLE_2_TRANS_INDEX(FILE_TABLE_INDEX) = TRANS
!
            PEAK_COIN_FACTOR(TRANS) = PEAK_COIN_FACTOR(TRANS) / 100.
!
            IF(TRANS_LOAD_GROUPS_INDEX(TG) == 0) THEN
!               FIRST_TABLE_TG(GET_TRANS_GROUP_POSITION(TG)) = TRANS
               MAX_TRANS_LOAD_GROUPS = MAX_TRANS_LOAD_GROUPS + 1
               FIRST_TABLE_TG(MAX_TRANS_LOAD_GROUPS) = TRANS
               TRANS_LOAD_GROUPS_INDEX(TG) = MAX_TRANS_LOAD_GROUPS
              TRANS_LOAD_2_TRANS_GROUPS(GET_TRANS_GROUP_POSITION(TG)) =
     +                                            MAX_TRANS_LOAD_GROUPS
               TRANS_LOAD_GROUP_2_TG(MAX_TRANS_LOAD_GROUPS) = TG
!
               IF(.NOT. YES_ASSET_ANALYST_ONLY) THEN

                  gtgp_index=GET_TRANS_GROUP_POSITION(TG)
                  TG_2_PLANNING_AREA(MAX_TRANS_LOAD_GROUPS) =
     +               GET_PA_FROM_TG(gtgp_index)
                  TG_2_CAPACITY_MARKET(MAX_TRANS_LOAD_GROUPS) =
     +               GET_CM_FROM_TG(gtgp_index)
!
               ENDIF
               
               
               
           MAX_TRANS_GROUP_NUMBER = MAX(MAX_TRANS_GROUP_NUMBER,TG)
!
            ENDIF
!
            IF(CUST_CLASS_GROUPS_INDEX(CG) == 0) THEN
               MAX_CUST_CLASS_GROUPS = MAX_CUST_CLASS_GROUPS + 1
               CUST_CLASS_GROUPS_INDEX(CG) = MAX_CUST_CLASS_GROUPS
               CUST_CLASS_GROUP_2_CG(MAX_CUST_CLASS_GROUPS) = CG
!
               MAX_CUST_GROUP_NUMBER = MAX(MAX_CUST_GROUP_NUMBER,CG)
            ENDIF
!
            IF(CUST2_CLASS_GROUPS_INDEX(CG2) == 0) THEN
               MAX_CUST2_CLASS_GROUPS = MAX_CUST2_CLASS_GROUPS + 1
               CUST2_CLASS_GROUPS_INDEX(CG2) = MAX_CUST2_CLASS_GROUPS
               CUST2_CLASS_GROUP_2_CG(MAX_CUST2_CLASS_GROUPS) = CG2
!
               MAX_CUST2_GROUP_NUMBER = MAX(MAX_CUST2_GROUP_NUMBER,CG2)
            ENDIF
!
!
            IF(ASSET_CLASS_GROUPS_INDEX(AC) == 0) THEN
               MAX_ASSET_CLASS_GROUPS = MAX_ASSET_CLASS_GROUPS + 1
               ASSET_CLASS_GROUPS_INDEX(AC) = MAX_ASSET_CLASS_GROUPS
               ASSET_CLASS_GROUP_2_AC(MAX_ASSET_CLASS_GROUPS) = AC
               FIRST_AC_TG(AC) = TG
            ENDIF
            IF(INTRA_COMPANY_TRANSACTION(TRANS) == 'Y') THEN
               ITRA_ID = INTRA_ASSET_CLASS_ID(TRANS)
               IF(ASSET_CLASS_GROUPS_INDEX(ITRA_ID) == 0) THEN
                  MAX_ASSET_CLASS_GROUPS = MAX_ASSET_CLASS_GROUPS + 1
                  ASSET_CLASS_GROUPS_INDEX(ITRA_ID) =
     +                                           MAX_ASSET_CLASS_GROUPS
                 ASSET_CLASS_GROUP_2_AC(MAX_ASSET_CLASS_GROUPS)=ITRA_ID

               ENDIF
            ENDIF
!
! THIS IS USED TO EFFICIENTLY GROUP CLASSES INTO TRANSACTION GROUPS,
! CUSTOMER GROUPS
!
            LAST_TABLE_FOR_TG(TRANS_LOAD_GROUPS_INDEX(TG)) = TRANS
!
            LAST_TABLE_FOR_CG(CUST_CLASS_GROUPS_INDEX(CG)) = TRANS
            LAST_TABLE_FOR_CG2(CUST2_CLASS_GROUPS_INDEX(CG2)) = TRANS

!!!! NOTE DOUBLE INDEX ON TG TO SAVE SPACE !!!
!
            TG = TRANS_LOAD_GROUPS_INDEX(TG)
!
            CG = CUST_CLASS_GROUPS_INDEX(CG)
            CG2 = CUST2_CLASS_GROUPS_INDEX(CG2)
!
            AC = ASSET_CLASS_GROUPS_INDEX(AC)
!
            PA = TG_2_PLANNING_AREA(TG)
            CM = TG_2_CAPACITY_MARKET(TG)
!
            SCENARIO_INDEX(TRANS) =
     +                            GET_SCENARIO_INDEX(SCENARIO_VARIABLE)
!
! INDEXED ENERGY REVENUES BY TAB
!
            NUMBER_TRANS_PER_AC_TG(AC,TG) =
     +                                NUMBER_TRANS_PER_AC_TG(AC,TG) + 1
            TEMP_I = NUMBER_TRANS_PER_AC_TG(AC,TG)
            ! POTENTIALLY BIG ARRAY
            TRANS_WITHIN_AC_TG(AC,TG,TEMP_I) = TRANS
!
! HOURLY CUSTOMER COSTS BY ASSET CLASS
!
            IF(ASSET_TRANSACTION_CROSS_INDEX(AC,TG) == 0) THEN
               ASSET_TRANSACTION_CROSS_INDEX(AC,TG) = 1
               NUMBER_ASSET_2_TRANS(TG) = NUMBER_ASSET_2_TRANS(TG) + 1
               ASSET_2_TRANS_INDEX(NUMBER_ASSET_2_TRANS(TG),TG) = AC
            ENDIF
!
! COST OF SERVICE STUFF
!
            TG_COUNTER(TG) = TG_COUNTER(TG) + 1
            HIGHEST_TG_COUNT = MAX(HIGHEST_TG_COUNT,TG_COUNTER(TG))
            IF(TEMP_DISPATCH_POSITION == -99) THEN
               LOAD_DISPATCH_POSITION(TG_COUNTER(TG),TG) = TRANS
            ELSE
               LOAD_DISPATCH_POSITION(TG_COUNTER(TG),TG) =
     +                                           TEMP_DISPATCH_POSITION
            ENDIF
!
            LOAD_DISPATCH_INDEX(TG_COUNTER(TG),TG) = TG_COUNTER(TG)
!
!
            LOAD_UNIT = 2100 + TG
!
            LOCAL_MONTH = 7 ! FOR NOW
!
!
            IF(TG == 0) THEN
               CYCLE ! IT IS NOT AN ACTIVE TRANSACTION GROUP
            ENDIF
!
            CAP_MARKET_MONTH_NO(TRANS) =
     +          CAP_MARKET_MONTH_NO_INDEX(CAPACITY_MARKET_MONTH(TRANS))
            IF(CAPACITY_MARKET_COST_ASSIGN(TRANS) ==
     +                                     'Capacity Purchases  ') THEN
               CAP_MARKET_TYPE_INDEX(TRANS) = 1
            ELSEIF(CAPACITY_MARKET_COST_ASSIGN(TRANS) ==
     +                                     'Purchased Power     ') THEN
               CAP_MARKET_TYPE_INDEX(TRANS) = 2
            ELSEIF(CAPACITY_MARKET_COST_ASSIGN(TRANS) ==
     +                                     'Capacity Sales      ') THEN
               CAP_MARKET_TYPE_INDEX(TRANS) = 3
            ENDIF
!
            IF(.NOT. YES_ASSET_ANALYST_ONLY) THEN
               CALL OPEN_TRANS_HOURLY_LOAD_FILE(
     +                                     REFERENCE_LOAD_NAME(TRANS),
     +                                    REFERENCE_LOAD_NUMBER(TRANS),
     +                                    R_YEAR,LOAD_UNIT,LOCAL_MONTH,
     +                                     SCENARIO_INDEX(TRANS))
!
               IF(LAHEY_LF95()) THEN
                  READ(LOAD_UNIT,REC=368) AEL_PEAK,AEL_PEAKMAX
                  READ(LOAD_UNIT,REC=369) AEL_BASE,AEL_BASEMIN
                  READ(LOAD_UNIT,REC=370) AEL_ENERGY,AEL_ENRGYEAR
                  START_REC = 2
               ELSE
                  READ(LOAD_UNIT,REC=367) AEL_PEAK,AEL_PEAKMAX
                  READ(LOAD_UNIT,REC=368) AEL_BASE,AEL_BASEMIN
                  READ(LOAD_UNIT,REC=369) AEL_ENERGY,AEL_ENRGYEAR
                  START_REC = 1
               ENDIF

               READ(LOAD_UNIT,REC=START_REC+1)
     +                               LDE_MONTH,LDE_DAY,LDE_YEAR,
     +                               EEICODE,FIRST_DAY_OF_YEAR,
     +                               TIMZON,TEMPER,
     +                               DELTMP,DAILY_LOADS_I4

            ENDIF
!
            IF(LDE_YEAR <= 200) THEN
               REMAIN = MOD(LDE_YEAR-64.,4.)
            ELSE
               REMAIN = MOD(LDE_YEAR-1964.,4.)
            ENDIF
            IF(REMAIN < .001) THEN
               REF_LEAP_YEAR_DAY_SHIFT(TRANS) = -1
            ENDIF
!
            TABLE_DAY_SHIFT(TRANS) = CALENDAR_DAY_OF_WEEK -
     +                                                FIRST_DAY_OF_YEAR

            IF(DIST_ENERGY_LOSS_FACTOR(TRANS) == 100. .OR.
     +               TRANS_ENERGY_LOSS_FACTOR(TRANS) == 100. .OR.
     +                            PEAK_LOSS_FACTOR(TRANS) == 100.) THEN
               WRITE(4,*) "LOSS FACTORS IN TRANSACTION FORECAST TABLE"
               WRITE(4,*) CUSTOMER_CLASS_NAME(TRANS), "EQUALS 100%"
               WRITE(4,*) "LOSSES IGNORED"
               ENERGY_LOSS_MULT(TRANS) = 1.0
               PEAK_LOSS_MULT = 1.0
            ELSE
               ENERGY_LOSS_MULT(TRANS) = 1./
     +               ((1.-DIST_ENERGY_LOSS_FACTOR(TRANS)/100.)*
     +                       (1.-TRANS_ENERGY_LOSS_FACTOR(TRANS)/100.))
               PEAK_LOSS_MULT = 1./
     +                        (1.-PEAK_LOSS_FACTOR(TRANS)/100.)
            ENDIF
!
            DO LOCAL_MONTH = 1, 12
               IF(MONTHLY_UNITS(TRANS) == 'C') THEN
                  MONTHLY_MULT = MONTHLY_CUSTOMERS(LOCAL_MONTH,TRANS)
               ELSE
                  MONTHLY_MULT = 1.0
               ENDIF
               IF(ANNUAL_MULTIPLIER(TRANS) >= 0.) THEN
                  TABLE_ANNUAL_MULTIPLIER = ANNUAL_MULTIPLIER(TRANS)
               ELSE
                  TABLE_ANNUAL_MULTIPLIER =
     +                  GET_VAR(ANNUAL_MULTIPLIER(TRANS),LOCAL_MONTH,
     +                                          "Annual Load Multipli")
               ENDIF
               IF(SCENARIO_ANNUAL_MULTIPLIER >= 0.) THEN
                 TABLE_SCENARIO_MULTIPLIER = SCENARIO_ANNUAL_MULTIPLIER
               ELSE
                  TABLE_SCENARIO_MULTIPLIER =
     +                  GET_VAR(SCENARIO_ANNUAL_MULTIPLIER,LOCAL_MONTH,
     +                                          "Scenario Multiplier ")
               ENDIF
!
!
               IF(CG < 0 .OR. CG > MAX(1,MAX_CLASS_GROUPS)) THEN
                  WRITE(4,*) "INVALID INDEXING OF CUSTOMER CLASSES IN"
                  WRITE(4,*)
     +                     "THE TRANSACT FORECAST FILE FOR YEAR ",
     +                     LOCAL_DATA_YEAR
                  WRITE(4,*) "AND SEASON ",LOCAL_MONTH
                  WRITE(4,*) "AND TABLE ",TRANS
                  WRITE(4,*) "FATAL ERROR"

                  WRITE(4,*) '*** line 2019 TF_OBJT.FOR ***'
                  er_message='See WARNING MESSAGES -tf_objt.for-3'
                  call end_program(er_message)
               ENDIF

               IF(    CALCULATION_MODE(TRANS) == 'R') THEN ! REFERENCE
!! NO TRANSFORM REQUIRED
                  LAST_THIS_YR_ENERGY(2,LOCAL_MONTH,TRANS) =
     +                                          AEL_ENERGY(LOCAL_MONTH)
                  LAST_THIS_YR_PEAK(2,LOCAL_MONTH,TRANS) =
     +                                            AEL_PEAK(LOCAL_MONTH)
               ELSEIF(CALCULATION_MODE(TRANS) == 'M' .OR.
     +                   CALCULATION_MODE(TRANS) == 'C') THEN ! MONTHLY
! USE PEAK AND ENERGY FROM MONTHLY INPUTS
                  LAST_THIS_YR_ENERGY(2,LOCAL_MONTH,TRANS) =
     +                                MONTHLY_ENERGY(LOCAL_MONTH,TRANS)
                  LAST_THIS_YR_PEAK(2,LOCAL_MONTH,TRANS) =
     +                                  MONTHLY_PEAK(LOCAL_MONTH,TRANS)
               ELSEIF(CALCULATION_MODE(TRANS) == 'G') THEN ! ANNUAL
!! GROW THE MONTHLY PEAK AND ENERGY FROM PREVIOUS YEAR VALUES
                  IF(R_YEAR > 1) THEN
                     LAST_THIS_YR_ENERGY(2,LOCAL_MONTH,TRANS) =
     +                     LAST_THIS_YR_ENERGY(1,LOCAL_MONTH,TRANS) *
     +                    (1. + MONTHLY_ENERGY(LOCAL_MONTH,TRANS)/100.)
                     LAST_THIS_YR_PEAK(2,LOCAL_MONTH,TRANS) =
     +                     LAST_THIS_YR_PEAK(1,LOCAL_MONTH,TRANS) *
     +                      (1. + MONTHLY_PEAK(LOCAL_MONTH,TRANS)/100.)
                  ELSE
                     WRITE(4,*) "GROWTH MODE FOR"
                     WRITE(4,*) CUSTOMER_CLASS_NAME(TRANS)
                     WRITE(4,*) REFERENCE_LOAD_NAME(TRANS)," IN YEAR"
                     WRITE(4,*) R_YEAR," IS OUTSIDE OF RANGE"
                     WRITE(4,*) "YOU NEED TO HAVE AN INITIAL YEAR."
                     WRITE(4,*) "SEE YOUR TRANSACTION LOADS FILE"
                     WRITE(4,*) '*** line 2066 TF_OBJT.FOR ***'
                     er_message='See WARNING MESSAGES -tf_objt.for-5'
                     call end_program(er_message)
                  ENDIF
               ELSEIF(CALCULATION_MODE(TRANS) == 'A') THEN ! GROWTH
! ALLOCATE ANNUAL TO MONTHS
                  IF(AEL_PEAKMAX > 0. AND. AEL_ENRGYEAR > 0.) THEN
                     LAST_THIS_YR_ENERGY(2,LOCAL_MONTH,TRANS) =
     +                 ANNUAL_ENERGY(TRANS) * AEL_ENERGY(LOCAL_MONTH) /
     +                                                     AEL_ENRGYEAR
                     LAST_THIS_YR_PEAK(2,LOCAL_MONTH,TRANS) =
     +                     ANNUAL_PEAK(TRANS) * AEL_PEAK(LOCAL_MONTH) /
     +                                                      AEL_PEAKMAX
                  ELSE
                     WRITE(4,*) "HISTORIC PEAK AND ENERGY FOR"
                     WRITE(4,*) CUSTOMER_CLASS_NAME(TRANS)
                     WRITE(4,*) REFERENCE_LOAD_NAME(TRANS)," IN YEAR"
                     WRITE(4,*) LOCAL_DATA_YEAR," IS OUTSIDE OF RANGE"
                     WRITE(4,*) "SEE YOUR TRANSACTION LOADS FILE"
                     WRITE(4,*) '*** line 2085 TF_OBJT.FOR ***'
                     er_message='See WARNING MESSAGES -tf_objt.for-6'
                     call end_program(er_message)
                  ENDIF
               ELSE
                  WRITE(4,*) "UNKNOWN TRANSACTION FILE GROWTH MODE FOR"
                  WRITE(4,*) CUSTOMER_CLASS_NAME(TRANS)," IN YEAR"
                  WRITE(4,*) LOCAL_DATA_YEAR
                  WRITE(4,*) "SEE YOUR TRANSACTION LOADS FILE"
                  WRITE(4,*) '*** line 2094 TF_OBJT.FOR ***'
                  er_message='See WARNING MESSAGES -tf_objt.for-7'
                  call end_program(er_message)
               ENDIF
! THIS YEAR'S DATA BECOMES LAST YEAR'S DATA NEXT YEAR. GOT IT?
! 5/28/99. GAT. Moooved.
               LAST_THIS_YR_ENERGY(1,LOCAL_MONTH,TRANS) =
     +                        LAST_THIS_YR_ENERGY(2,LOCAL_MONTH,TRANS)

               LAST_THIS_YR_ENERGY(1,LOCAL_MONTH,0) =
     +                        LAST_THIS_YR_ENERGY(2,LOCAL_MONTH,0)

               LAST_THIS_YR_PEAK(1,LOCAL_MONTH,TRANS) =
     +                        LAST_THIS_YR_PEAK(2,LOCAL_MONTH,TRANS)

               LAST_THIS_YR_PEAK(1,LOCAL_MONTH,0) =
     +                        LAST_THIS_YR_PEAK(2,LOCAL_MONTH,0)

               TG_CG_DATABASE(LOCAL_MONTH,TG,CG,1) =
     +               TG_CG_DATABASE(LOCAL_MONTH,TG,CG,1) +
     +                     LAST_THIS_YR_ENERGY(2,LOCAL_MONTH,TRANS) *
     +                                        TABLE_ANNUAL_MULTIPLIER *
     +                                                   MONTHLY_MULT *
     +                                      TABLE_SCENARIO_MULTIPLIER *
     +               GET_SCENARIO_ELECTRIC_DEMAND(R_YEAR,LOCAL_MONTH) *
     +              GET_PARAM_ELECTRIC_DEMAND(R_YEAR,LOCAL_MONTH,TGP) *
     +                        GET_SCENARIO_ENERGY(R_YEAR,LOCAL_MONTH) *
     +                         GET_PARAM_ENERGY(R_YEAR,LOCAL_MONTH,TGP)
               TG_CG_DATABASE(LOCAL_MONTH,TG,CG,2) =
     +               TG_CG_DATABASE(LOCAL_MONTH,TG,CG,2) +
     +                     LAST_THIS_YR_PEAK(2,LOCAL_MONTH,TRANS) *
     +                                        TABLE_ANNUAL_MULTIPLIER *
     +                                                   MONTHLY_MULT *
     +                                      TABLE_SCENARIO_MULTIPLIER *
     +               GET_SCENARIO_ELECTRIC_DEMAND(R_YEAR,LOCAL_MONTH) *
     +              GET_PARAM_ELECTRIC_DEMAND(R_YEAR,LOCAL_MONTH,TGP) *
     +                          GET_SCENARIO_PEAK(R_YEAR,LOCAL_MONTH) *
     +                         GET_PARAM_PEAK(R_YEAR,LOCAL_MONTH,TGP) *
     +                                                PEAK_LOSS_MULT
               TG_CG_DATABASE(LOCAL_MONTH,TG,CG,3) =
     +               TG_CG_DATABASE(LOCAL_MONTH,TG,CG,3) +
     +                             MONTHLY_CUSTOMERS(LOCAL_MONTH,TRANS)
!
!
! 02/12/04. FOR WVPA MONTHLY MIDAS
!
               TG_CG2_DATABASE(LOCAL_MONTH,TG,CG2,1) =
     +               TG_CG2_DATABASE(LOCAL_MONTH,TG,CG2,1) +
     +                     LAST_THIS_YR_ENERGY(2,LOCAL_MONTH,TRANS) *
     +                                        TABLE_ANNUAL_MULTIPLIER *
     +                                                   MONTHLY_MULT *
     +                                      TABLE_SCENARIO_MULTIPLIER *
     +               GET_SCENARIO_ELECTRIC_DEMAND(R_YEAR,LOCAL_MONTH) *
     +              GET_PARAM_ELECTRIC_DEMAND(R_YEAR,LOCAL_MONTH,TGP) *
     +                        GET_SCENARIO_ENERGY(R_YEAR,LOCAL_MONTH) *
     +                         GET_PARAM_ENERGY(R_YEAR,LOCAL_MONTH,TGP)
               TG_CG2_DATABASE(LOCAL_MONTH,TG,CG2,2) =
     +               TG_CG2_DATABASE(LOCAL_MONTH,TG,CG2,2) +
     +                     LAST_THIS_YR_PEAK(2,LOCAL_MONTH,TRANS) *
     +                                        TABLE_ANNUAL_MULTIPLIER *
     +                                                   MONTHLY_MULT *
     +                                      TABLE_SCENARIO_MULTIPLIER *
     +               GET_SCENARIO_ELECTRIC_DEMAND(R_YEAR,LOCAL_MONTH) *
     +              GET_PARAM_ELECTRIC_DEMAND(R_YEAR,LOCAL_MONTH,TGP) *
     +                          GET_SCENARIO_PEAK(R_YEAR,LOCAL_MONTH) *
     +                         GET_PARAM_PEAK(R_YEAR,LOCAL_MONTH,TGP) *
     +                                               PEAK_LOSS_MULT
               TG_CG2_DATABASE(LOCAL_MONTH,TG,CG2,3) =
     +               TG_CG2_DATABASE(LOCAL_MONTH,TG,CG2,3) +
     +                             MONTHLY_CUSTOMERS(LOCAL_MONTH,TRANS)
!
               LATIN_HYPERCUBE_ENERGY = 1.0
               LATIN_HYPERCUBE_PEAK = 1.0
!               SCENARIO_INDEX = GET_SCENARIO_INDEX(SCENARIO_VARIABLE)
! THIS IS THE DEFAULT CASE
               IF(SCENARIO_INDEX(TRANS) == -99) THEN
!
! 11/25/02. NOT ACTIVE: NO SCENARIO MAKER IMPACT
!
               ELSEIF(SCENARIO_INDEX(TRANS) == 0) THEN
                  LATIN_HYPERCUBE_ENERGY =
     +               GET_SCENARIO_ELECTRIC_DEMAND(R_YEAR,LOCAL_MONTH) *
     +              GET_PARAM_ELECTRIC_DEMAND(R_YEAR,LOCAL_MONTH,TGP) *
     +                        GET_SCENARIO_ENERGY(R_YEAR,LOCAL_MONTH) *
     +                         GET_PARAM_ENERGY(R_YEAR,LOCAL_MONTH,TGP)
!
                  LATIN_HYPERCUBE_PEAK =
     +               GET_SCENARIO_ELECTRIC_DEMAND(R_YEAR,LOCAL_MONTH) *
     +              GET_PARAM_ELECTRIC_DEMAND(R_YEAR,LOCAL_MONTH,TGP) *
     +                          GET_SCENARIO_PEAK(R_YEAR,LOCAL_MONTH) *
     +                           GET_PARAM_PEAK(R_YEAR,LOCAL_MONTH,TGP)
               ELSEIF(SCENARIO_INDEX(TRANS) == 4) THEN
                  LATIN_HYPERCUBE_ENERGY =
     +               GET_SCENARIO_ELECTRIC_DEMAND(R_YEAR,LOCAL_MONTH) *
     +                GET_PARAM_ELECTRIC_DEMAND(R_YEAR,LOCAL_MONTH,TGP)
!
                  LATIN_HYPERCUBE_PEAK =
     +               GET_SCENARIO_ELECTRIC_DEMAND(R_YEAR,LOCAL_MONTH) *
     +                GET_PARAM_ELECTRIC_DEMAND(R_YEAR,LOCAL_MONTH,TGP)
               ELSEIF(SCENARIO_INDEX(TRANS) == 15) THEN
                  LATIN_HYPERCUBE_PEAK =
     +                          GET_SCENARIO_PEAK(R_YEAR,LOCAL_MONTH) *
     +                           GET_PARAM_PEAK(R_YEAR,LOCAL_MONTH,TGP)
               ELSEIF(SCENARIO_INDEX(TRANS) == 16) THEN
                  LATIN_HYPERCUBE_ENERGY =
     +                        GET_SCENARIO_ENERGY(R_YEAR,LOCAL_MONTH) *
     +                         GET_PARAM_ENERGY(R_YEAR,LOCAL_MONTH,TGP)
!
               ELSE ! SOME DEFINED DISTRIBUTION
!
                  LATIN_HYPERCUBE_ENERGY =
     +                           GET_SCENARIO_BY_INDEX(R_YEAR,
     +                               LOCAL_MONTH,SCENARIO_INDEX(TRANS))
                  LATIN_HYPERCUBE_PEAK = LATIN_HYPERCUBE_ENERGY
               ENDIF
!
               LAST_THIS_YR_ENERGY(2,LOCAL_MONTH,TRANS) =
     +                     LAST_THIS_YR_ENERGY(2,LOCAL_MONTH,TRANS) *
     +                                        TABLE_ANNUAL_MULTIPLIER *
     +                                                   MONTHLY_MULT *
     +                                      TABLE_SCENARIO_MULTIPLIER *
     +                                         LATIN_HYPERCUBE_ENERGY *
     +                     ENERGY_LOSS_MULT(TRANS)   ! ADDED 01/04/01.
!
               LAST_THIS_YR_PEAK(2,LOCAL_MONTH,TRANS) =
     +                     LAST_THIS_YR_PEAK(2,LOCAL_MONTH,TRANS) *
     +                                        TABLE_ANNUAL_MULTIPLIER *
     +                                                   MONTHLY_MULT *
     +                                      TABLE_SCENARIO_MULTIPLIER *
     +                                           LATIN_HYPERCUBE_PEAK *
     +                               PEAK_LOSS_MULT   ! ADDED 01/04/01.
!
               LAST_THIS_YR_ENERGY(2,LOCAL_MONTH,0) =
     +                      LAST_THIS_YR_ENERGY(2,LOCAL_MONTH,0) +
     +                         LAST_THIS_YR_ENERGY(2,LOCAL_MONTH,TRANS)
               LAST_THIS_YR_PEAK(2,LOCAL_MONTH,0) =
     +                      LAST_THIS_YR_PEAK(2,LOCAL_MONTH,0) +
     +                         LAST_THIS_YR_PEAK(2,LOCAL_MONTH,TRANS)
!
               IF(TG_COUNTER(TG) == 1) THEN
                  HYDRO_WEEK_PLANNING_PEAK =
     +               GET_HYDRO_WEEK_PLANNING_PEAK(
     +                                        TRANSACTION_GROUP(TRANS),
     +                                              R_YEAR,LOCAL_MONTH)
               ELSE
                  HYDRO_WEEK_PLANNING_PEAK = 0.
               ENDIF
!
! 10/13/04
!
               IF(MID_TERM_PEAK_UNCER) THEN ! DEFAULT
                  TF_PLANNING_PEAK(0,LOCAL_MONTH) =
     +                     TF_PLANNING_PEAK(0,LOCAL_MONTH) +
     +                        LAST_THIS_YR_PEAK(2,LOCAL_MONTH,TRANS) *
     +                          PEAK_COIN_FACTOR(TRANS) -
     +                              HYDRO_WEEK_PLANNING_PEAK
!
                  PA_PLANNING_PEAK(PA,LOCAL_MONTH) =
     +                     PA_PLANNING_PEAK(PA,LOCAL_MONTH) +
     +                         LAST_THIS_YR_PEAK(2,LOCAL_MONTH,TRANS) *
     +                          PEAK_COIN_FACTOR(TRANS) -
     +                              HYDRO_WEEK_PLANNING_PEAK
                  CM_PLANNING_PEAK(CM,LOCAL_MONTH) =
     +                     CM_PLANNING_PEAK(CM,LOCAL_MONTH) +
     +                         LAST_THIS_YR_PEAK(2,LOCAL_MONTH,TRANS) *
     +                          PEAK_COIN_FACTOR(TRANS) -
     +                              HYDRO_WEEK_PLANNING_PEAK
                  TF_PLANNING_PEAK(TG,LOCAL_MONTH) =
     +                     TF_PLANNING_PEAK(TG,LOCAL_MONTH) +
     +                         LAST_THIS_YR_PEAK(2,LOCAL_MONTH,TRANS) *
     +                          PEAK_COIN_FACTOR(TRANS) -
     +                              HYDRO_WEEK_PLANNING_PEAK
               ELSE
                  TEMP_R =
     +                          GET_SCENARIO_PEAK(R_YEAR,LOCAL_MONTH) *
     +                         GET_PARAM_PEAK(R_YEAR,LOCAL_MONTH,TGP)
                  IF(TEMP_R > 0.0) THEN
                     TEMP_R = 1./TEMP_R
                     TF_PLANNING_PEAK(0,LOCAL_MONTH) =
     +                     TF_PLANNING_PEAK(0,LOCAL_MONTH) +
     +                         LAST_THIS_YR_PEAK(2,LOCAL_MONTH,TRANS) *
     +                          TEMP_R *
     +                          PEAK_COIN_FACTOR(TRANS) -
     +                              HYDRO_WEEK_PLANNING_PEAK
!
                     PA_PLANNING_PEAK(PA,LOCAL_MONTH) =
     +                     PA_PLANNING_PEAK(PA,LOCAL_MONTH) +
     +                         LAST_THIS_YR_PEAK(2,LOCAL_MONTH,TRANS) *
     +                          TEMP_R *
     +                          PEAK_COIN_FACTOR(TRANS) -
     +                              HYDRO_WEEK_PLANNING_PEAK
                     CM_PLANNING_PEAK(CM,LOCAL_MONTH) =
     +                     CM_PLANNING_PEAK(CM,LOCAL_MONTH) +
     +                         LAST_THIS_YR_PEAK(2,LOCAL_MONTH,TRANS) *
     +                          TEMP_R *
     +                          PEAK_COIN_FACTOR(TRANS) -
     +                              HYDRO_WEEK_PLANNING_PEAK
                     TF_PLANNING_PEAK(TG,LOCAL_MONTH) =
     +                     TF_PLANNING_PEAK(TG,LOCAL_MONTH) +
     +                         LAST_THIS_YR_PEAK(2,LOCAL_MONTH,TRANS) *
     +                          TEMP_R *
     +                          PEAK_COIN_FACTOR(TRANS) -
     +                              HYDRO_WEEK_PLANNING_PEAK
                  ENDIF
               ENDIF


               FUT_PEAK(3,LOCAL_MONTH) = FUT_PEAK(3,LOCAL_MONTH) +
     +                          LAST_THIS_YR_PEAK(2,LOCAL_MONTH,TRANS)

            ENDDO ! LOCAL_MONTH FOR PEAK AND ENERGY FORECAST
!
            ANNUAL_PEAK(TRANS) = 0.
            DO LOCAL_MONTH = 1, 12
               MONTHLY_TABLE_PEAK_SALES(TRANS,LOCAL_MONTH) =
     +            LAST_THIS_YR_PEAK(2,LOCAL_MONTH,TRANS)/PEAK_LOSS_MULT
               ANNUAL_PEAK(TRANS) = MAX(ANNUAL_PEAK(TRANS),
     +                     MONTHLY_TABLE_PEAK_SALES(TRANS,LOCAL_MONTH))
            ENDDO
            CALL CLOSE_TRANS_HOURLY_LOAD_FILE(LOAD_UNIT)
!
         ENDDO ! TABLES
!
! NEED TO CYCLE AROUND TG
!
         CALL INT2_Sort(TG_COUNTER(TG),
     +                     LOAD_DISPATCH_INDEX(1,TG),
     +                     LOAD_DISPATCH_POSITION(1,TG))

!
! FOR USE BY CAPACITY PLANNING PROGRAMS. 10/20/98. GAT.
!
         GLOBAL_PEAK = 0.
         GLOBAL_PEAK_MONTH = 0
!
         DO LOCAL_MONTH = 1, 12
            IF(TF_PLANNING_PEAK(0,LOCAL_MONTH) < GLOBAL_PEAK) CYCLE
            GLOBAL_PEAK = TF_PLANNING_PEAK(0,LOCAL_MONTH)
            GLOBAL_PEAK_MONTH = LOCAL_MONTH
         ENDDO
!
! 09/11/02.
!
         DO I = 1, NUMBER_OF_ACTIVE_GROUPS
!
            DO LOCAL_MONTH = 1, 12
               PA = TG_2_PLANNING_AREA(I)
               IF(PA_PLANNING_PEAK(PA,LOCAL_MONTH) <
     +                                        GLOBAL_PA_PEAK(PA)) CYCLE
               GLOBAL_PA_PEAK(PA) = PA_PLANNING_PEAK(PA,LOCAL_MONTH)
               GLOBAL_PA_PEAK_MONTH(PA) = LOCAL_MONTH
            ENDDO
            DO LOCAL_MONTH = 1, 12
               CM = TG_2_CAPACITY_MARKET(I)
               IF(CM_PLANNING_PEAK(CM,LOCAL_MONTH) <
     +                                        GLOBAL_CM_PEAK(CM)) CYCLE
               GLOBAL_CM_PEAK(CM) = CM_PLANNING_PEAK(CM,LOCAL_MONTH)
               GLOBAL_CM_PEAK_MONTH(CM) = LOCAL_MONTH
            ENDDO
!
         ENDDO

! CALCULATE IMPLIED ESCALATION RATES

         IF(R_YEAR_TEMP == AVAIL_DATA_YEARS) THEN
           ! CALCULATE IMPLIED ESCALATION RATES
           DO TRANS = 1, MAX_TRANS_LOAD_TABLES
              CALL IMPLIED_ESC(ESC_MARKET_ENERGY_PRICE(TRANS),
     +                         MARKET_ENERGY_PRICE(TRANS))
              CALL IMPLIED_ESC(ESC_MARKET_DEMAND_PRICE(TRANS),
     +                         MARKET_DEMAND_PRICE(TRANS))
              CALL IMPLIED_ESC(ESC_MARKET_CUSTOMER_PRICE(TRANS),
     +                         MARKET_CUSTOMER_PRICE(TRANS))
              CALL IMPLIED_ESC(ESC_DISTRIBUTION_PRICE(TRANS),
     +                         DISTRIBUTION_PRICE(TRANS))
              CALL IMPLIED_ESC(ESC_TRANSMISSION_PRICE(TRANS),
     +                         TRANSMISSION_PRICE(TRANS))
              CALL IMPLIED_ESC(ESC_INDEXED_ENERGY_PRICE(TRANS),
     +                         INDEXED_ENERGY_PRICE(TRANS))
           ENDDO
         ENDIF
         IF(GLOBAL_PEAK_MONTH /= 0) THEN
            VOID_R = UPDATE_PEAK_AFTER_FEEDBACK(globecom_YEAR,
     + GLOBAL_PEAK)
            VOID_I = UPDATE_PLANNING_PEAK_MONTH(globecom_YEAR,
     + GLOBAL_PEAK_MONTH)
         ELSE
           WRITE(4,*) "TOTAL DEMAND ON THE SYSTEM FROM THE TRANSACTION"
            WRITE(4,*) "FORECAST FILE DOES NOT PRODUCE POSITIVE PEAKS."
            WRITE(4,*) '*** line 2183 TF_OBJT.FOR ***'
            er_message='See WARNING MESSAGES -tf_objt.for-8'
            call end_program(er_message)
         ENDIF

         CLOSE(951)

!
! ANNUAL ASSET ANALYST VARIABLES
!
         IF(ALLOCATED(MONTHLY_AC_COST_AT_MARKET))
     +                            DEALLOCATE(MONTHLY_AC_COST_AT_MARKET)
         IF(ALLOCATED(MONTHLY_AC_CONTRACT_REVENUE))
     +                          DEALLOCATE(MONTHLY_AC_CONTRACT_REVENUE)
         IF(ALLOCATED(MONTHLY_AC_CONTRACT_EXPENSE))
     +                          DEALLOCATE(MONTHLY_AC_CONTRACT_EXPENSE)

         ALLOCATE(MONTHLY_AC_COST_AT_MARKET(
     +                                  0:MAX_ASSET_CLASS_GROUPS,0:12))
!msg thinks the order is 3=BUSBAR and 2=meter NOT 1=$,2=BUSBAR,3=METER
         ALLOCATE(MONTHLY_AC_CONTRACT_REVENUE(
     +                      -1:MAX_ASSET_CLASS_GROUPS,4,0:12,
     +                                             LAST_INCOME_LINE,3))
!msg thinks the order is 3=BUSBAR and 2=meter NOT 1=$,2=BUSBAR,3=METER
         ALLOCATE(MONTHLY_AC_CONTRACT_EXPENSE(
     +                      -1:MAX_ASSET_CLASS_GROUPS,0:12,
     +                                             LAST_EXPENSE_ITEM))

         MONTHLY_AC_COST_AT_MARKET = 0.
         MONTHLY_AC_CONTRACT_REVENUE = 0.
         MONTHLY_AC_CONTRACT_EXPENSE = 0.
!
         ANNUAL_TRANSACTION_LOADS = .TRUE.
      RETURN


      ENTRY MONTHLY_TRANSACTION_LOADS(R_YEAR_TEMP,R_ISEAS,R_HOURS)


!
! CALANDER CORRECT: ASSUME INITIALLY THAT THE USER TAKES CARE OF THIS.
! WHEN TO SUM?, WHAT IS THE BEST LOOPING PROCEDURE?
! INITIALLY ASSUME THAT THE MODEL IS ALWAYS USING A REFERENCE LOAD.
! INITIALLY ASSUME THAT THE REFERENCE LOADS ARE ALL WORKING OFF OF THE
! SAME BASE YEAR LOAD.
!
! MAX_TRANS_LOAD_TABLES ARE THE NUMBER OF ACTIVE TABLES IN THE FILE
! MAX_TRANS_LOAD_GROUPS ARE THE NUMBER OF TRANSACTION GROUPS (TG) WITH
! LOADS
!     => SUM OF MAX_TRANS_LOAD_TABLES FOR A TG
!
!         CALL DOES_TF_FILE_EXIST(TF_FILE_EXISTS)
         R_YEAR = R_YEAR_TEMP
         IF(.NOT. SAVE_TF_FILE_EXISTS .OR. .NOT. RUN_TRANSACT .AND.
     +                             .NOT. YES_ASSET_ANALYST_ONLY) RETURN
!
         SAVE_HOURS = R_HOURS
!
         IF(R_HOURS == 0 .OR. MAX_TRANS_LOAD_TABLES == 0 .OR.
     +                                 MAX_TRANS_LOAD_GROUPS == 0) THEN
            IF(R_HOURS == 0) THEN
               WRITE(4,*) "NO HOURS FOR THE MONTH IN THE TRANSACTION"
               WRITE(4,*) "FORECAST FILE"
               WRITE(4,*) '*** line 2236 TF_OBJT.FOR ***'
               er_message='See WARNING MESSAGES -tf_objt.for-9'
               call end_program(er_message)
            ELSEIF(MAX_TRANS_LOAD_GROUPS == 0) THEN
               WRITE(4,*) "Transact File exists, but Transact did not"
               WRITE(4,*) "find any Transaction Tables with active"
               WRITE(4,*) "Transaction Groups."
               WRITE(4,*) "Check Transact Groups files for active "
               WRITE(4,*) "Groups."
            ELSE
               WRITE(4,*) "Transact File exists, but Transact did not"
               WRITE(4,*) "find any valid Transaction Tables."
               WRITE(4,*) "Check Transact Forecast Overlays"
            ENDIF

         ENDIF

         IF(ALLOCATED(DAY_OF_WEEK)) DEALLOCATE(DAY_OF_WEEK)
         ALLOCATE(DAY_OF_WEEK(31,0:MAX_TRANS_LOAD_TABLES))
!
         IF(R_ISEAS == 1) THEN
            IF(ALLOCATED(ANNUAL_CLASS_ENERGY_REVENUE))
     +         DEALLOCATE(
     +                     ANNUAL_CLASS_ENERGY_REVENUE,
     +                     ANNUAL_CLASS_PEAK_REVENUE,
     +                     ANNUAL_CLASS_CUSTOMER_REVENUE,
     +                     ANNUAL_CAP_CUSTOMER_EXPENSE,
     +                     ANNUAL_CLASS_ENERGY,
     +                     ANNUAL_CLASS_PEAK,
     +                     ANNUAL_CLASS_CUSTOMERS,
     +                     ANNUAL_TRANS_INDEXED_REVENUE)
            ALLOCATE(
     +           ANNUAL_CLASS_ENERGY_REVENUE(0:MAX_CUST_CLASS_GROUPS))
            ALLOCATE(
     +           ANNUAL_CLASS_PEAK_REVENUE(0:MAX_CUST_CLASS_GROUPS))
            ALLOCATE(
     +          ANNUAL_CLASS_CUSTOMER_REVENUE(0:MAX_CUST_CLASS_GROUPS))
            ALLOCATE(
     +           ANNUAL_CAP_CUSTOMER_EXPENSE(0:MAX_CUST_CLASS_GROUPS))
            ALLOCATE(
     +           ANNUAL_CLASS_ENERGY(0:MAX_CUST_CLASS_GROUPS))
            ALLOCATE(
     +           ANNUAL_CLASS_PEAK(0:MAX_CUST_CLASS_GROUPS))
            ALLOCATE(
     +           ANNUAL_CLASS_CUSTOMERS(0:MAX_CUST_CLASS_GROUPS))
            ALLOCATE(
     +           ANNUAL_TRANS_INDEXED_REVENUE(0:MAX_CUST_CLASS_GROUPS))

            ANNUAL_CLASS_ENERGY_REVENUE = 0.
            ANNUAL_CLASS_PEAK_REVENUE = 0.
            ANNUAL_CLASS_CUSTOMER_REVENUE = 0.
            ANNUAL_CAP_CUSTOMER_EXPENSE = 0.
            ANNUAL_CLASS_ENERGY = 0.
            ANNUAL_CLASS_PEAK = 0.
            ANNUAL_CLASS_CUSTOMERS = 0.
!
            ANNUAL_TRANS_INDEXED_REVENUE = 0.
!
         ENDIF
!
         IF(ALLOCATED(TRANS_HOURLY_LOAD))
     +         DEALLOCATE( TRANS_HOURLY_LOAD,
     +                     WH_LOADS_PER_HOUR,
     +                     HYDRO_HOURLY_LOAD,
     +                     TABLE_HOURLY_LOAD,
     +                     MONTHLY_TRANS_ENERGY,
     +                     MONTHLY_HYDRO_ENERGY,
     +                     MONTHLY_TRANS_PEAK,
     +                     MONTHLY_TRANS_BASE,
     +                     MONTHLY_HYDRO_PEAK,
     +                     MONTHLY_HYDRO_BASE,
     +                     MONTHLY_TABLE_ENERGY,
     +                     MONTHLY_TABLE_SALES_ENERGY,
     +                     MONTHLY_TABLE_PEAK,
     +                     TABLE_ENERGY_PRICE,
     +                     TABLE_ENERGY_REVENUE,
     +                     TABLE_DEMAND_REVENUE,
     +                     TABLE_CUSTOMER_REVENUE,
     +                     TRANS_ENERGY_REVENUE,
     +                     TRANS_DEMAND_REVENUE,
     +                     TRANS_CUSTOMER_REVENUE,
     +                     CLASS_ENERGY_REVENUE,
     +                     TRANS_INDEXED_REVENUE,
     +                     CLASS_PEAK_REVENUE,
     +                     CLASS_CUSTOMER_REVENUE,
     +                     CAP_CUSTOMER_EXPENSE,
     +                     TF_TG_CAP_MARKET_MW,
     +                     TF_TG_CAP_MARKET_COST,
     +                     MONTHLY_CLASS_ENERGY,
     +                     MONTHLY_CLASS_PEAK,
     +                     MONTHLY_CLASS_CUSTOMERS,
     +                     LOCAL_CUSTOMER_NAME,
     +                     ASSET_CLASS_HOURLY_LOAD)
         IF(ALLOCATED(WH_TRANS_ALLOC)) DEALLOCATE (WH_TRANS_ALLOC)
         ALLOCATE (WH_TRANS_ALLOC(0:MAX_TRANS_LOAD_TABLES,
     +                                          MAX_TRANS_LOAD_GROUPS))
         ALLOCATE(TRANS_HOURLY_LOAD(
     +                     MAX_HOURS_IN_MONTH,0:MAX_TRANS_LOAD_GROUPS))
         ALLOCATE(WH_LOADS_PER_HOUR(
     +                     MAX_HOURS_IN_MONTH,0:MAX_TRANS_LOAD_GROUPS))
         ALLOCATE(HYDRO_HOURLY_LOAD( ! NEED TO COUNT HYDRO GROUPS
     +                    MAX_HOURS_IN_MONTH,0:NUMBER_OF_HYDRO_GROUPS))
         ALLOCATE(TABLE_HOURLY_LOAD(
     +                     MAX_HOURS_IN_MONTH,0:MAX_TRANS_LOAD_TABLES))
         ALLOCATE(MONTHLY_TRANS_ENERGY(0:MAX_TRANS_LOAD_GROUPS))
         ALLOCATE(MONTHLY_HYDRO_ENERGY(0:NUMBER_OF_HYDRO_GROUPS))
         ALLOCATE(MONTHLY_TRANS_PEAK(0:MAX_TRANS_LOAD_GROUPS))
         ALLOCATE(MONTHLY_TRANS_BASE(0:MAX_TRANS_LOAD_GROUPS))
!
         ALLOCATE(MONTHLY_HYDRO_PEAK(0:NUMBER_OF_HYDRO_GROUPS))
         ALLOCATE(MONTHLY_HYDRO_BASE(0:NUMBER_OF_HYDRO_GROUPS))
!
         ALLOCATE(MONTHLY_TABLE_ENERGY(0:MAX_TRANS_LOAD_TABLES))
         ALLOCATE(MONTHLY_TABLE_SALES_ENERGY(
     +                                        0:MAX_TRANS_LOAD_TABLES))
         ALLOCATE(MONTHLY_TABLE_PEAK(0:MAX_TRANS_LOAD_TABLES))
         ALLOCATE(TABLE_ENERGY_PRICE(MAX_TRANS_LOAD_TABLES))
         ALLOCATE(TABLE_ENERGY_REVENUE(0:MAX_TRANS_LOAD_TABLES))
         ALLOCATE(TABLE_DEMAND_REVENUE(0:MAX_TRANS_LOAD_TABLES))
         ALLOCATE(TABLE_CUSTOMER_REVENUE(0:MAX_TRANS_LOAD_TABLES))
!
         ALLOCATE(TRANS_ENERGY_REVENUE(0:MAX_TRANS_LOAD_GROUPS))
         ALLOCATE(TRANS_DEMAND_REVENUE(0:MAX_TRANS_LOAD_GROUPS))
         ALLOCATE(TRANS_CUSTOMER_REVENUE(0:MAX_TRANS_LOAD_GROUPS))
!
         ALLOCATE(CLASS_ENERGY_REVENUE(0:MAX_CUST_CLASS_GROUPS))
         ALLOCATE(TRANS_INDEXED_REVENUE(0:MAX_CUST_CLASS_GROUPS))
!         ALLOCATE(TRANS_INDEXED_REVENUE(0:MAX_TRANS_LOAD_TABLES))
         ALLOCATE(CLASS_PEAK_REVENUE(0:MAX_CUST_CLASS_GROUPS))
         ALLOCATE(CLASS_CUSTOMER_REVENUE(0:MAX_CUST_CLASS_GROUPS))
         ALLOCATE(CAP_CUSTOMER_EXPENSE(0:MAX_CUST_CLASS_GROUPS))
         ALLOCATE(TF_TG_CAP_MARKET_MW(0:MAX_TRANS_GROUPS))
         ALLOCATE(TF_TG_CAP_MARKET_COST(0:MAX_TRANS_GROUPS))
         ALLOCATE(MONTHLY_CLASS_ENERGY(0:MAX_CUST_CLASS_GROUPS))
         ALLOCATE(MONTHLY_CLASS_PEAK(0:MAX_CUST_CLASS_GROUPS))
         ALLOCATE(MONTHLY_CLASS_CUSTOMERS(0:MAX_CUST_CLASS_GROUPS))
         ALLOCATE(LOCAL_CUSTOMER_NAME(0:MAX_CUST_CLASS_GROUPS))
         ALLOCATE(ASSET_CLASS_HOURLY_LOAD(MAX_HOURS_IN_MONTH,
     +                                       0:MAX_ASSET_CLASS_GROUPS,
     +                                       0:MAX_TRANS_LOAD_GROUPS))
!
         TABLE_ENERGY_PRICE = 0.
!
         TRANS_HOURLY_LOAD = 0.
         WH_LOADS_PER_HOUR = 0.
         HYDRO_HOURLY_LOAD = 0. ! NEED TO COUNT HYDRO GROUPS
         TABLE_HOURLY_LOAD = 0.
         MONTHLY_TABLE_ENERGY = 0.
         MONTHLY_TABLE_SALES_ENERGY = 0.
         MONTHLY_TABLE_PEAK = 0.
         TRANS_ENERGY_REVENUE = 0.
         TRANS_DEMAND_REVENUE = 0.
         TRANS_CUSTOMER_REVENUE = 0.
         CLASS_ENERGY_REVENUE = 0.
         CLASS_PEAK_REVENUE = 0.
         CLASS_CUSTOMER_REVENUE = 0.
         CAP_CUSTOMER_EXPENSE = 0.
         TF_TG_CAP_MARKET_MW = 0.0
         TF_TG_CAP_MARKET_COST = 0.0
         MONTHLY_CLASS_ENERGY = 0.
         MONTHLY_CLASS_PEAK = 0.
         MONTHLY_CLASS_CUSTOMERS = 0.
         TRANS_INDEXED_REVENUE = 0.
         ASSET_CLASS_HOURLY_LOAD = 0.
!
!
         DAYS_IN_MONTH = R_HOURS / 24
!
         TF_GROUP1_ACTIVE = .FALSE.
         TF_ANY_GROUP_ACTIVE = .FALSE.
!
         USE_SCENARIO_HOURLY_DEMAND =
     +         OPEN_SCENARIO_HOURLY_DEMAND(R_YEAR,SCENARIO_HOURLY_UNIT)
!
         DO HR = 1, 24
            SCENARIO_DAILY_LOADS_R4(HR) = 1.0
         ENDDO
!
         DO TRANS = 0, MAX_TRANS_LOAD_GROUPS
            MONTHLY_TRANS_ENERGY(TRANS) = 0.
            MONTHLY_TRANS_PEAK(TRANS) = 0.
            MONTHLY_TRANS_BASE(TRANS) = 9999999.
         ENDDO
!
         CALL INITIALIZE_TRANS_LOAD_PROB(MAX_TRANS_LOAD_GROUPS)
!
! ADDED FOR PACIFICORP. 050101. GAT.
!
!
         DO TRANS = 0, NUMBER_OF_HYDRO_GROUPS
            MONTHLY_HYDRO_ENERGY(TRANS) = 0.
            MONTHLY_HYDRO_PEAK(TRANS) = 0.
            MONTHLY_HYDRO_BASE(TRANS) = 9999999.
         ENDDO
!
         IF(NUMBER_OF_HYDRO_GROUPS > 0) THEN
            CALL INITIALIZE_HYDRO_LOAD_PROB(NUMBER_OF_HYDRO_GROUPS)
         ENDIF
!
         IF(ADJUST_FOR_LEAP_YEAR .AND. R_ISEAS > 2) THEN
            LEAP_YEAR_DAY_SHIFT = 1
         ELSE
            LEAP_YEAR_DAY_SHIFT = 0
         ENDIF

         WH_TRANS_ALLOC = 0.
         WEEKLY_HYDRO_FORECASTS = .FALSE.
         IF(WH_FILE_EXISTS) THEN
            DO TG = 1, MAX_TRANS_LOAD_GROUPS
               TEMP_R = 0.
               DO TRANS = 1, MAX_TRANS_LOAD_TABLES
                  IF(TABLE_ACTIVE(TRANS) == 'F') CYCLE
!
                  IF( .NOT. IN_ACTIVE_LOADS_MARKET_AREA(
     +                          BASECASE_MKT_AREA_ID_tfo(TRANS))) CYCLE
!
                  I = TRANSACTION_GROUP(TRANS)

                  IF(I == 0 .OR.
     +                           TRANS_LOAD_GROUPS_INDEX(I) == 0) CYCLE
!
                  IF(.NOT. YES_ASSET_ANALYST_ONLY) THEN
                     TG_POSITION_IN_TG_FILE =
     +                                      GET_TRANS_GROUP_POSITION(I)
                  ENDIF
!
                  IF(I == 0 .OR. TG > MAX_TRANS_GROUPS) THEN
                    CYCLE ! DOES NOT CONTRIBUTE TO TRANSACTION LOADS
                  endif
!
                  WH_TRANS_ALLOC(TRANS,TG) =
     +                             LAST_THIS_YR_ENERGY(2,R_ISEAS,TRANS)
                  WH_TRANS_ALLOC(0,TG) = WH_TRANS_ALLOC(0,TG) +
     +                                         WH_TRANS_ALLOC(TRANS,TG)
!
               END DO
               IF(WH_TRANS_ALLOC(0,TG) > 0.) THEN
                  DO TRANS = 1, MAX_TRANS_LOAD_TABLES
                     WH_TRANS_ALLOC(TRANS,TG) =
     +                          WH_TRANS_ALLOC(TRANS,TG)/
     +                                             WH_TRANS_ALLOC(0,TG)
                     TEMP_R = TEMP_R + WH_TRANS_ALLOC(TRANS,TG)
                 END DO
               ENDIF
!
               HOUR_IN_WEEK = 0
!
               TRANS = 1
               ROLLOVER_VALUE = 366
               IF(R_ISEAS > 2) THEN
                  LREC = IREC - 1 + DA + TABLE_DAY_SHIFT(TRANS) +
     +                                 LEAP_YEAR_DAY_SHIFT +
     +                                   REF_LEAP_YEAR_DAY_SHIFT(TRANS)
               ELSE
                  LREC = IREC - 1 + DA + TABLE_DAY_SHIFT(TRANS) +
     +                                             LEAP_YEAR_DAY_SHIFT
               ENDIF
               IF(LREC <= 0) THEN
                  LREC = LREC + ROLLOVER_VALUE - 1
               ELSEIF(LREC > ROLLOVER_VALUE) THEN
                  LREC = LREC - ROLLOVER_VALUE
               ENDIF
!
               IF(LAHEY_LF95()) LREC = LREC + 1
!
               HR_IN_MONTH = 0
!
! 1. NEED TO FIX THE DAY OFFSET FOR LREC
! 2. NEED TO GET WORKING FOR MULTIPLE TG'S
!
!
               DO DA = 1, DAYS_IN_MONTH, 7
                  LREC = DA + 1
                  WEEKLY_HYDRO_FORECASTS =
     +                  GET_WEEKLY_HYDRO_FORECASTS(
     +                                 DA,
     +                                 R_ISEAS,
     +                                 R_YEAR,
     +          GET_TRANS_GROUP_POSITION(TG), ! THIS NEEDS TO BE INDEXED
     +                           LOAD_UNIT, ! TO CREATE WEEKLY PATTERN
     +                                 WEEKLY_HYDRO_ENERGY,
     +                                 WEEKLY_HYDRO_MINIMUM_MW,
     +                                 WEEKLY_HYDRO_MAXIMUM_MW,
     +                                 WEEKLY_HYDRO_LOADS,
     +                                 HOUR_IN_WEEK,
     +                                 LREC) ! 168 VALUES
                  DO I =  1, 168
                     HR_IN_MONTH = HR_IN_MONTH + 1
                     WH_LOADS_PER_HOUR(HR_IN_MONTH,TG) =
     +                                            WEEKLY_HYDRO_LOADS(I)
                     WH_TRANS_MONTHLY_ENERGY(R_ISEAS,TG) =
     +                           WH_TRANS_MONTHLY_ENERGY(R_ISEAS,TG) +
     +                                WH_LOADS_PER_HOUR(HR_IN_MONTH,TG)
                     WH_TRANS_MONTHLY_CAPACITY(R_ISEAS,TG) =
     +                       MAX(WH_TRANS_MONTHLY_CAPACITY(R_ISEAS,TG),
     +                               WH_LOADS_PER_HOUR(HR_IN_MONTH,TG))
                     IF(HR_IN_MONTH >= DAYS_IN_MONTH*24) EXIT
                  ENDDO
!
               END DO
            END DO
         ENDIF
!
         DO TRANS = 1, MAX_TRANS_LOAD_TABLES
!
            IF(TABLE_ACTIVE(TRANS) == 'F') CYCLE
!
            IF( .NOT. IN_ACTIVE_LOADS_MARKET_AREA(
     +                           BASECASE_MKT_AREA_ID_tfo(TRANS))) CYCLE
!
            TG = TRANSACTION_GROUP(TRANS)

            IF(TG == 0 .OR. TRANS_LOAD_GROUPS_INDEX(TG) == 0) CYCLE
!
            IF(.NOT. YES_ASSET_ANALYST_ONLY) THEN
               TG_POSITION_IN_TG_FILE = GET_TRANS_GROUP_POSITION(TG)
            ENDIF
!
            IF(TG == 0 .OR. TG > MAX_TRANS_GROUPS) then
                CYCLE ! DOES NOT CONTRIBUTE TO TRANSACTION LOADS
            endif
!
            IF(TG == 1) TF_GROUP1_ACTIVE = .TRUE.
            TF_ANY_GROUP_ACTIVE = .TRUE.
!
! NOTE DOUBLE INDEX ON TG TO SAVE SPACE
!
            TG = TRANS_LOAD_GROUPS_INDEX(TG)
!
            IF(TG == 0) THEN
               CYCLE ! IT IS NOT AN ACTIVE TRANSACTION GROUP
            ENDIF
!
            CG = CUSTOMER_GROUP(TRANS)
!
            IF(USE_MARKET_AREA_REPORT_ID) THEN
               WRITE(MARKET_AREA_NAME,*) WD_INDEX(TRANS)
            ELSE
               MARKET_AREA_NAME = BASECASE_MKT_AREA_ID_tfo(TRANS)(1:5)
            ENDIF
!
            IF(CG < 0 .OR. CG > MAX_CLASS_GROUPS) THEN
               WRITE(4,*) "INVALID INDEXING OF CUSTOMER CLASSES IN"
               WRITE(4,*) "THE TRANSACT FORECAST FILE FOR YEAR ",
     +                                                  LOCAL_DATA_YEAR
               WRITE(4,*) "AND SEASON ",R_ISEAS
               WRITE(4,*) "AND TABLE ",TRANS
               WRITE(4,*) "RESET TABLE INDEX TO 1"
               CG = 1
               WRITE(4,*) '*** line 2447 TF_OBJT.FOR ***'
               er_message='See WARNING MESSAGES -tf_objt.for-11'
               call end_program(er_message)
            ENDIF
!
            CG = CUST_CLASS_GROUPS_INDEX(CG)
            CG2 = CUST2_CLASS_GROUPS_INDEX(CG2)
            IF(CG < 0 .OR. CG > MAX_CUST_CLASS_GROUPS) THEN
               WRITE(4,*) "INVALID INDEXING OF CUSTOMER CLASSES IN"
               WRITE(4,*) "THE TRANSACT FORECAST FILE FOR YEAR ",
     +                                                  LOCAL_DATA_YEAR
               WRITE(4,*) "AND SEASON ",R_ISEAS
               WRITE(4,*) "AND TABLE ",TRANS
               WRITE(4,*) "RESET TABLE INDEX TO 1"
               CG = 1
               WRITE(4,*) '*** line 2459 TF_OBJT.FOR ***'
               er_message='See WARNING MESSAGES -tf_objt.for-12'
               call end_program(er_message)
            ENDIF
!
! NEEDS TO BE MOVED UP.
!
            IF(TRANS_WH_REPORT_NOT_OPEN .AND. WH_FILE_EXISTS) THEN
               TRANS_WH_REPORT_NOT_OPEN = .FALSE.
               TRANS_WH_UNIT = WH_HOURLY_HEADER(TRANS_WH_REC)
            ENDIF
            IF(MARKET_AREA_REPORT_NOT_OPEN .AND.
     +                                         MARKET_AREA_REPORT) THEN
               MARKET_AREA_REPORT_NOT_OPEN = .FALSE.
               MARKET_AREA_UNIT =
     +                          MARKET_AREA_RPT_HEADER(MARKET_AREA_REC)
            ENDIF
            TF_LOADS_PER_HOUR = 0.
!
            AC = ASSET_CLASS_ID(TRANS)
            AC = ASSET_CLASS_GROUPS_INDEX(AC)

            TG_2_HYDRO_WEEK =
     +                    GET_TG_2_HYDRO_WEEK(TRANSACTION_GROUP(TRANS))

            USE_MONTHLY_NONLINEAR_LOADS =
     +                             THREE_FACTOR_TRANSFORM(TRANS) == 'T'
            IF(USE_MONTHLY_NONLINEAR_LOADS)
     +                            CALL INIT_DAILY_NONLIN_LOADS(R_HOURS)
!
! 050101. FOR PACIFICORP/NW HYDRO
            HG = GET_HG_FROM_TG(TG)
!
            LOAD_UNIT = 2100 + TG
!            LOAD_UNIT = 2101
!
            CALL OPEN_TRANS_HOURLY_LOAD_FILE(
     +                                     REFERENCE_LOAD_NAME(TRANS),
     +                                    REFERENCE_LOAD_NUMBER(TRANS),
     +                                     R_YEAR,LOAD_UNIT,R_ISEAS,
     +                                     SCENARIO_INDEX(TRANS))
            HR_IN_MONTH = 0
            IREC = ALINE_LOAD_DATA(TRANS,R_ISEAS)
!
            IF(LAHEY_LF95()) THEN
               READ(LOAD_UNIT,REC=368) AEL_PEAK,AEL_PEAKMAX
               READ(LOAD_UNIT,REC=369) AEL_BASE,AEL_BASEMIN
               READ(LOAD_UNIT,REC=370) AEL_ENERGY,AEL_ENRGYEAR
               ROLLOVER_VALUE = 366 ! 367
            ELSE
               READ(LOAD_UNIT,REC=367) AEL_PEAK,AEL_PEAKMAX
               READ(LOAD_UNIT,REC=368) AEL_BASE,AEL_BASEMIN
               READ(LOAD_UNIT,REC=369) AEL_ENERGY,AEL_ENRGYEAR
               ROLLOVER_VALUE = 366
            ENDIF

! DAY SHIFT LOGIC: HISTORICAL PEAK AND ENERGY GIVEN SHIFTS
!
            IF(R_ISEAS > 2) THEN
               LREC = TABLE_DAY_SHIFT(TRANS) + LEAP_YEAR_DAY_SHIFT +
     +                                   REF_LEAP_YEAR_DAY_SHIFT(TRANS)
            ELSE
               LREC = TABLE_DAY_SHIFT(TRANS) + LEAP_YEAR_DAY_SHIFT
            ENDIF
!
            IF( (CALENDAR_CORRECT .AND. LREC /= 0) .OR.
     +          (REF_LEAP_YEAR_DAY_SHIFT(TRANS) /= 0 .AND.
     +                                        R_ISEAS == 2) .OR.
     + USE_MONTHLY_NONLINEAR_LOADS ) THEN ! 10/21/03.
!
               IF(LAHEY_LF95()) IREC = IREC - 1
!                 The section below requires
!                 that the record offset be added before the read
!                 This reduction of the IREC is to offset the function
!                 ALINE_LOAD_DATA
!
               AEL_PEAK(R_ISEAS) = 0.
               AEL_BASE(R_ISEAS) = 999999.
               AEL_ENERGY(R_ISEAS) = 0.
!
               DO DA = 1, DAYS_IN_MONTH
                  IF(R_ISEAS > 2) THEN
                     LREC = IREC - 1 + DA + TABLE_DAY_SHIFT(TRANS) +
     +                                 LEAP_YEAR_DAY_SHIFT +
     +                                   REF_LEAP_YEAR_DAY_SHIFT(TRANS)
                  ELSE
                     LREC = IREC - 1 + DA + TABLE_DAY_SHIFT(TRANS) +
     +                                              LEAP_YEAR_DAY_SHIFT
                  ENDIF

                  IF(LREC <= 0) THEN
                     LREC = LREC + ROLLOVER_VALUE - 1

                  ELSEIF(LREC > ROLLOVER_VALUE) THEN
                     LREC = LREC - ROLLOVER_VALUE

                  ENDIF
!
                  IF(LAHEY_LF95()) LREC = LREC + 1
!
                  IF(LREC < 1 .OR. LREC > 367) THEN
                     WRITE(4,*) "BAD REFERENCE LOAD VALUE"
                     WRITE(4,*) "RECORD", LREC,"IREC",IREC
                    WRITE(4,*) "TABLE DAY SHIFT",TABLE_DAY_SHIFT(TRANS)
                     WRITE(4,*) "DAY",DA,"MONTH",R_ISEAS
                     WRITE(4,*) "ROLLOVER VALUE",ROLLOVER_VALUE
                     WRITE(4,*) "LEAP YEAR DAY SHIFT",
     +                                              LEAP_YEAR_DAY_SHIFT
                     WRITE(4,*) "REFERENCE LEAP YEAR DAY SHIFT",
     +                                   REF_LEAP_YEAR_DAY_SHIFT(TRANS)
                     er_message='Stop requested from tf_objt SIID282'
                     call end_program(er_message)
                  ENDIF
!
                  READ(LOAD_UNIT,REC=LREC)
     +                               LDE_MONTH,LDE_DAY,LDE_YEAR,
     +                               EEICODE,DAY_OF_WEEK(DA,TRANS),
     +                               TIMZON,TEMPER,
     +                               DELTMP,DAILY_LOADS_I4
!                  IF(LREC == 0) THEN
                  IF(LDE_MONTH == 2 .AND. LDE_DAY == 29) THEN
                     IF(SUM_24_I4(DAILY_LOADS_I4) == 0) THEN
                        READ(LOAD_UNIT,REC=LREC-7)
     +                               LDE_MONTH,LDE_DAY,LDE_YEAR,
     +                               EEICODE,DAY_OF_WEEK(DA,TRANS),
     +                               TIMZON,TEMPER,
     +                               DELTMP,DAILY_LOADS_I4
                     ENDIF
                  ENDIF
!
                  AEL_ENERGY(R_ISEAS) = AEL_ENERGY(R_ISEAS) +
     +                                        SUM_24_I4(DAILY_LOADS_I4)
!
                  AEL_PEAK(R_ISEAS) = MAX(AEL_PEAK(R_ISEAS),
     +                                      PEAK_24_I4(DAILY_LOADS_I4))
                  AEL_BASE(R_ISEAS) = MIN(AEL_BASE(R_ISEAS),
     +                                      BASE_24_I4(DAILY_LOADS_I4))
!
!
! 10/20/03. CALLED BY SWITCH ABOVE
!
                  IF(USE_MONTHLY_NONLINEAR_LOADS) then
                     call write_daily_load_argument_trace_i4(
     +                  "tf_objt:0023", DA, DAILY_LOADS_I4)
                         ! TODO: this passes an array of ints to a routine
                         ! that receives and array of floats.  Fix.
                         CALL ACCUM_DAILY_I4_NONLIN_LOADS(
     +                                                   DA,
     +                                                  DAILY_LOADS_I4)
     


                  end if
!
!
               ENDDO
            ELSEIF(CALENDAR_CORRECT .AND. LAHEY_LF95())THEN
               IREC = IREC - 1  ! The section below requires
            ENDIF
!
!
!
! AT THIS POINT WE KNOW THE MONTHLY PEAK AND ENERGY. PRIME TIME TO
! CALCULATE
! REVENUES BY CUSTOMER CLASS, ASSET CLASS, ETC.
!
            MONTH_AVE_ENERGY =
     +                     LAST_THIS_YR_ENERGY(2,R_ISEAS,TRANS)/R_HOURS
            MONTH_AVE_HIST_ENERGY = AEL_ENERGY(R_ISEAS)/R_HOURS
!
            IF(AEL_PEAK(R_ISEAS) <= MONTH_AVE_HIST_ENERGY .OR.
     +                        USE_MONTHLY_NONLINEAR_LOADS .OR.
     +                             CALCULATION_MODE(TRANS) == 'R') THEN
               MONTHLY_SLOPE = 1.0
               MONTHLY_INTERCEPT = 0.0
!
!
! 10/21/03.
!
               IF(USE_MONTHLY_NONLINEAR_LOADS) then
               
                 CALL CONVERT_MONTHLY_NONLIN_LOADS(
     +          LAST_THIS_YR_PEAK(2,R_ISEAS,TRANS),   ! FORECAST PEAK
     + LAST_THIS_YR_ENERGY(2,R_ISEAS,TRANS), ! FORECAST ENERGY
     +                      R_HOURS)
               end if
       
!
!
            ELSE
               MONTHLY_SLOPE =
     +               (LAST_THIS_YR_PEAK(2,R_ISEAS,TRANS) -
     +                                               MONTH_AVE_ENERGY)/
     +                      (AEL_PEAK(R_ISEAS) - MONTH_AVE_HIST_ENERGY)
               MONTHLY_INTERCEPT = LAST_THIS_YR_PEAK(2,R_ISEAS,TRANS) -
     +                                MONTHLY_SLOPE * AEL_PEAK(R_ISEAS)
! 110211. UPDATED FOR EE PROGRAMS IN FALL 2011 REFERENCE CASE.
               IF(MONTHLY_SLOPE < 0.0 .AND.
     +            .NOT. (LAST_THIS_YR_PEAK(2,R_ISEAS,TRANS) < 0.0 .AND.
     +               LAST_THIS_YR_ENERGY(2,R_ISEAS,TRANS) < 0.0) ) THEN
                  IF(LAST_THIS_YR_PEAK(2,R_ISEAS,TRANS) <
     +                                           MONTH_AVE_ENERGY) THEN
                     WRITE(4,*) "INPUT PEAK AND ENERGY FOR"
                     WRITE(4,*) CUSTOMER_CLASS_NAME(TRANS)," IN YEAR"
                     WRITE(4,*) LOCAL_DATA_YEAR," AND MONTH ",R_ISEAS
                     WRITE(4,*) "IS OUTSIDE OF RANGE"
                     WRITE(4,*) "CHECK THE MONTH LOAD FACTOR IN YOUR"
                     WRITE(4,*) "TRANSACTION LOADS FILE"
!
                   MONTH_AVE_ENERGY = MONTH_AVE_ENERGY * .98 ! ARBITRARY
                     MONTHLY_SLOPE =
     +                     (LAST_THIS_YR_PEAK(2,R_ISEAS,TRANS) -
     +                                               MONTH_AVE_ENERGY)/
     +                      (AEL_PEAK(R_ISEAS) - MONTH_AVE_HIST_ENERGY)
                     MONTHLY_INTERCEPT =
     +                     LAST_THIS_YR_PEAK(2,R_ISEAS,TRANS) -
     +                                MONTHLY_SLOPE * AEL_PEAK(R_ISEAS)
!
                 ELSEIF(AEL_PEAK(R_ISEAS) < MONTH_AVE_HIST_ENERGY) THEN
                     WRITE(4,*) "HISTORIC LOAD PATTERNS FOR"
                     WRITE(4,*) CUSTOMER_CLASS_NAME(TRANS)
                     WRITE(4,*) REFERENCE_LOAD_NAME(TRANS)," IN YEAR"
                     WRITE(4,*) LOCAL_DATA_YEAR," AND MONTH ",R_ISEAS
                     WRITE(4,*) "IS OUTSIDE OF RANGE"
                     WRITE(4,*) "CHECK THE MONTH LOAD FACTOR IN YOUR"
                     WRITE(4,*) "TRANSACTION LOADS FILE"
                     WRITE(4,*) '*** line 2599 TF_OBJT.FOR ***'
                     er_message='See WARNING MESSAGES -tf_objt.for-13'
                     call end_program(er_message)
                  ENDIF
               ENDIF
            ENDIF

            YES_FIRST_TABLE = FIRST_TABLE_TG(TG) == TRANS

            HOUR_IN_WEEK = 0
!
            DO DA = 1, DAYS_IN_MONTH
!

! DAY SHIFT LOGIC BY TABLE
!
               IF(CALENDAR_CORRECT) THEN
                  IF(R_ISEAS > 2) THEN
                     LREC = IREC + TABLE_DAY_SHIFT(TRANS) +
     +                          LEAP_YEAR_DAY_SHIFT +
     +                                   REF_LEAP_YEAR_DAY_SHIFT(TRANS)
                  ELSE
                     LREC = IREC + TABLE_DAY_SHIFT(TRANS) +
     +                                              LEAP_YEAR_DAY_SHIFT
                  ENDIF

                  IF(LREC <= 0) THEN
                     LREC = LREC + ROLLOVER_VALUE - 1
                  ELSEIF(LREC > ROLLOVER_VALUE) THEN
                     LREC = LREC - ROLLOVER_VALUE
                  ENDIF
                  IF(LAHEY_LF95()) LREC = LREC + 1
               ELSE
                  LREC = IREC
               ENDIF

!
!               IF(LAHEY_LF95()) LREC = LREC + 1
               READ(LOAD_UNIT,REC=LREC) LDE_MONTH,LDE_DAY,LDE_YEAR,
     +                               EEICODE,DAY_OF_WEEK(DA,TRANS),
     +                               TIMZON,TEMPER,
     +                               DELTMP,DAILY_LOADS_I4
               IF(LDE_MONTH == 2 .AND. LDE_DAY == 29) THEN
                  IF(SUM_24_I4(DAILY_LOADS_I4) == 0) THEN
                        READ(LOAD_UNIT,REC=LREC-7)
     +                               LDE_MONTH,LDE_DAY,LDE_YEAR,
     +                               EEICODE,DAY_OF_WEEK(DA,TRANS),
     +                               TIMZON,TEMPER,
     +                               DELTMP,DAILY_LOADS_I4
                  ENDIF
               ENDIF
!
! ADDED 11/7/99. GAT.
!
               IF(USE_SCENARIO_HOURLY_DEMAND) THEN
                  READ(SCENARIO_HOURLY_UNIT,REC=LREC)
     +                               LDE_MONTH,LDE_DAY,LDE_YEAR,
     +                               EEICODE,DAY_OF_WEEK(DA,TRANS),
     +                               TIMZON,TEMPER,
     +                               DELTMP,SCENARIO_DAILY_LOADS_R4
               ENDIF
!
!   IF WEATHER ANALYST CALCULATED LOADS THEN OVERWRITE DAILY_LOADS_I4
!
! 031108. NEW DEF OF ACTIVE_WD_FORECAST FOR DOUG AND ACUPOWER.
!
               ACTIVE_WD_FORECAST = .FALSE.
               IF(WD_FILE_EXISTS) THEN
                  J = WD_INDEX(TRANS)
                  IF(J >= 0) THEN
                     ACTIVE_WD_FORECAST =
     +                      GET_WD_LOAD(J,R_YEAR,R_ISEAS,DA,
     +                                  DAILY_LOADS_I4,MONTHLY_SLOPE,
     +                                  MONTHLY_INTERCEPT)

                  ENDIF
               ENDIF
               USE_NONLINEAR_R4 = .FALSE.
               IF(USE_MONTHLY_NONLINEAR_LOADS .AND.
     + .NOT. ACTIVE_WD_FORECAST) THEN ! 10/21/03
! NOTE SPECIAL R*4 TO GET HIGHER PRECISION ON THE TRANSFORM.
                  CALL GET_DAILY_R4_NONLIN_LOADS(DA,DAILY_LOADS_R4)
                  USE_NONLINEAR_R4 = .TRUE.
               ENDIF

               IREC = IREC + 1
               DO HR = 1, 24
                  HR_IN_MONTH = HR_IN_MONTH + 1
!
                  IF(USE_NONLINEAR_R4) THEN
                     REF_HOURLY_LOAD = DAILY_LOADS_R4(HR)
                  ELSE
                     REF_HOURLY_LOAD = ! TABLE CONTRIBUTION
     +                                              DAILY_LOADS_I4(HR)
                  ENDIF

                     HOURLY_FORWARD_SALE = 0.

                  FINAL_HOURLY_LOAD =
     +               (MONTHLY_INTERCEPT +
     +                                 MONTHLY_SLOPE * REF_HOURLY_LOAD)
!
! TESTING. 11/7/99. GAT.
!
                  IF(USE_SCENARIO_HOURLY_DEMAND) THEN
                     FINAL_HOURLY_LOAD = FINAL_HOURLY_LOAD *
     +                                      SCENARIO_DAILY_LOADS_R4(HR)
                  ENDIF
!
! PRE-WEEKLY HYDRO LOADS
!
! HOURLY LOAD OF TABLE AFTER SCENARIO MAKER STUFF
!
                  TF_LOADS_PER_HOUR(HR_IN_MONTH) = FINAL_HOURLY_LOAD
                  IF(ABS(ENERGY_LOSS_MULT(TRANS)) > .0001) THEN
                     FINAL_HOURLY_SALES = FINAL_HOURLY_LOAD /
     +                                          ENERGY_LOSS_MULT(TRANS)
                  ELSE
                     FINAL_HOURLY_SALES = 0.
                  ENDIF

                  IF(WH_FILE_EXISTS) THEN
                     IF(WEEKLY_HYDRO_FORECASTS) THEN

                        TEMP_R = WH_LOADS_PER_HOUR(HR_IN_MONTH,TG) *
     +                                          WH_TRANS_ALLOC(TRANS,TG)
                        FINAL_HOURLY_LOAD = FINAL_HOURLY_LOAD - TEMP_R

                     ENDIF
                  ENDIF
!
                  MONTH_BASE_LOAD = MIN(MONTH_BASE_LOAD,
     +                                               FINAL_HOURLY_LOAD)
!
                  ASSET_CLASS_HOURLY_LOAD(HR_IN_MONTH,AC,TG) =
     +                    ASSET_CLASS_HOURLY_LOAD(HR_IN_MONTH,AC,TG) +
     +                          FINAL_HOURLY_LOAD + HOURLY_FORWARD_SALE
                  ! SUM OF TABLE CONTRIBUTIONS FOR A TG
                  ASSET_CLASS_HOURLY_LOAD(HR_IN_MONTH,0,TG) =
     +                      ASSET_CLASS_HOURLY_LOAD(HR_IN_MONTH,0,TG) +
     +                          FINAL_HOURLY_LOAD + HOURLY_FORWARD_SALE
!
                  TRANS_HOURLY_LOAD(HR_IN_MONTH,TG) =
     +                      TRANS_HOURLY_LOAD(HR_IN_MONTH,TG) +
     +                          FINAL_HOURLY_LOAD + HOURLY_FORWARD_SALE
!
! NEED TO CHANGE THE INDEX FROM TG TO HG DUE TO HYDRO AGGREGATION
! TG => HG
!
                  HYDRO_HOURLY_LOAD(HR_IN_MONTH,HG) =
     +                      HYDRO_HOURLY_LOAD(HR_IN_MONTH,HG) +
     +                          FINAL_HOURLY_LOAD + HOURLY_FORWARD_SALE
!
                  ! SUM OF TABLE CONTRIBUTIONS FOR A TG
                  TRANS_HOURLY_LOAD(HR_IN_MONTH,0) =
     +                      TRANS_HOURLY_LOAD(HR_IN_MONTH,0) +
     +                          FINAL_HOURLY_LOAD + HOURLY_FORWARD_SALE
                  TABLE_HOURLY_LOAD(HR_IN_MONTH,TRANS) =
     +                      TABLE_HOURLY_LOAD(HR_IN_MONTH,TRANS) +
     +                          FINAL_HOURLY_LOAD + HOURLY_FORWARD_SALE
                  TABLE_HOURLY_LOAD(HR_IN_MONTH,0) =
     +                      TABLE_HOURLY_LOAD(HR_IN_MONTH,0) +
     +                          FINAL_HOURLY_LOAD + HOURLY_FORWARD_SALE
                  PERIOD_TRANSACTION_DEMAND =
     +                        PERIOD_TRANSACTION_DEMAND +
     +                          FINAL_HOURLY_LOAD + HOURLY_FORWARD_SALE
!
                  IF(TRANS == LAST_TABLE_FOR_TG(TG)) THEN
                     MONTHLY_TRANS_ENERGY(TG) =
     +                        MONTHLY_TRANS_ENERGY(TG) +
     +                                TRANS_HOURLY_LOAD(HR_IN_MONTH,TG)
                     MONTHLY_TRANS_ENERGY(0) =
     +                        MONTHLY_TRANS_ENERGY(0) +
     +                                TRANS_HOURLY_LOAD(HR_IN_MONTH,TG)
                     MONTHLY_TRANS_PEAK(TG) =
     +                       MAX(MONTHLY_TRANS_PEAK(TG),
     +                               TRANS_HOURLY_LOAD(HR_IN_MONTH,TG))
                     MONTHLY_TRANS_PEAK(0) =
     +                       MAX(MONTHLY_TRANS_PEAK(0),
     +                               TRANS_HOURLY_LOAD(HR_IN_MONTH,TG))
                     MONTHLY_TRANS_BASE(TG) =
     +                       MIN(MONTHLY_TRANS_BASE(TG),
     +                               TRANS_HOURLY_LOAD(HR_IN_MONTH,TG))
                     MONTHLY_TRANS_BASE(0) =
     +                       MIN(MONTHLY_TRANS_BASE(0),
     +                               TRANS_HOURLY_LOAD(HR_IN_MONTH,TG))
!
                     MONTHLY_HYDRO_ENERGY(HG) =
     +                        MONTHLY_HYDRO_ENERGY(HG) +
     +                                HYDRO_HOURLY_LOAD(HR_IN_MONTH,HG)
                     MONTHLY_HYDRO_ENERGY(0) =
     +                        MONTHLY_HYDRO_ENERGY(0) +
     +                                HYDRO_HOURLY_LOAD(HR_IN_MONTH,HG)
                     MONTHLY_HYDRO_PEAK(HG) =
     +                       MAX(MONTHLY_HYDRO_PEAK(HG),
     +                               HYDRO_HOURLY_LOAD(HR_IN_MONTH,HG))
                     MONTHLY_HYDRO_PEAK(0) =
     +                       MAX(MONTHLY_HYDRO_PEAK(0),
     +                               HYDRO_HOURLY_LOAD(HR_IN_MONTH,HG))
                     MONTHLY_HYDRO_BASE(HG) =
     +                       MIN(MONTHLY_HYDRO_BASE(HG),
     +                               HYDRO_HOURLY_LOAD(HR_IN_MONTH,HG))
                     MONTHLY_HYDRO_BASE(0) =
     +                       MIN(MONTHLY_HYDRO_BASE(0),
     +                               HYDRO_HOURLY_LOAD(HR_IN_MONTH,HG))
                  ENDIF
!
!
                  MONTHLY_TABLE_ENERGY(TRANS) =
     +                        MONTHLY_TABLE_ENERGY(TRANS) +
     +                          FINAL_HOURLY_LOAD + HOURLY_FORWARD_SALE
                  MONTHLY_TABLE_SALES_ENERGY(TRANS) =
     +                        MONTHLY_TABLE_SALES_ENERGY(TRANS) +
     +                                               FINAL_HOURLY_SALES
                  MONTHLY_TABLE_PEAK(TRANS) =
     +                       MAX(MONTHLY_TABLE_PEAK(TRANS),
     +                         FINAL_HOURLY_LOAD + HOURLY_FORWARD_SALE)
!
               ENDDO ! HOURS
               IF(TG_2_HYDRO_WEEK  .AND. .NOT. TESTING_PLAN) THEN
                  WRITE(TRANS_WH_UNIT,REC=TRANS_WH_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(LOCAL_YEAR),
     +               CL_MONTH_NAME(R_ISEAS),
     +               FLOAT(DA),
     +               'INPUT LOADS '//CUSTOMER_CLASS_NAME(TRANS)(1:8),
     +               (TF_LOADS_PER_HOUR(J),
     +                                 J=HR_IN_MONTH-23,HR_IN_MONTH)
                  TRANS_WH_REC = TRANS_WH_REC + 1
!
                  WRITE(TRANS_WH_UNIT,REC=TRANS_WH_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(LOCAL_YEAR),
     +               CL_MONTH_NAME(R_ISEAS),
     +               FLOAT(DA),
     +               'WEEKLY HYDR '//CUSTOMER_CLASS_NAME(TRANS)(1:8),
     +               (WH_LOADS_PER_HOUR(J,TG) *
     +                           WH_TRANS_ALLOC(TRANS,TG),
     +                                 J=HR_IN_MONTH-23,HR_IN_MONTH)
                  TRANS_WH_REC = TRANS_WH_REC + 1
!
                  WRITE(TRANS_WH_UNIT,REC=TRANS_WH_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(LOCAL_YEAR),
     +               CL_MONTH_NAME(R_ISEAS),
     +               FLOAT(DA),
     +               'AFTR WEEKLY '//CUSTOMER_CLASS_NAME(TRANS)(1:8),
     +               (TF_LOADS_PER_HOUR(J)-
     +                     WH_LOADS_PER_HOUR(J,TG)*
     +                           WH_TRANS_ALLOC(TRANS,TG),
     +                                 J=HR_IN_MONTH-23,HR_IN_MONTH)
                  TRANS_WH_REC = TRANS_WH_REC + 1
               ENDIF
               IF(MARKET_AREA_REPORT  .AND. .NOT. TESTING_PLAN) THEN
                  MARKET_AREA_REC = rptrec(MARKET_AREA_UNIT)
                  WRITE(MARKET_AREA_UNIT,REC=MARKET_AREA_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(LOCAL_YEAR),
     +               CL_MONTH_NAME(R_ISEAS),
     +               FLOAT(DA),
     +               MARKET_AREA_NAME,
     +               (TF_LOADS_PER_HOUR(J),
     +                                 J=HR_IN_MONTH-23,HR_IN_MONTH)
!     +               FLOAT(TG)
                  MARKET_AREA_REC = MARKET_AREA_REC + 1
!
               ENDIF
            ENDDO ! DAYS
!
            CALL CLOSE_TRANS_HOURLY_LOAD_FILE(LOAD_UNIT)
! THIS YEAR'S DATA BECOMES LAST YEAR'S DATA NEXT YEAR. GOT IT?
!            LAST_THIS_YR_ENERGY(1,R_ISEAS,TRANS) =
!     +                        LAST_THIS_YR_ENERGY(2,R_ISEAS,TRANS)
!            LAST_THIS_YR_ENERGY(1,R_ISEAS,0) =
!     +                        LAST_THIS_YR_ENERGY(2,R_ISEAS,0)
!            LAST_THIS_YR_PEAK(1,R_ISEAS,TRANS) =
!     +                        LAST_THIS_YR_PEAK(2,R_ISEAS,TRANS)
!            LAST_THIS_YR_PEAK(1,R_ISEAS,0) =
!     +                        LAST_THIS_YR_PEAK(2,R_ISEAS,0)
!
! REVENUE FORECASTS CALCULATION
!
            TABLE_ENERGY_PRICE(TRANS) = MARKET_ENERGY_PRICE(TRANS)
            IF(MONTHLY_ENERGY_PRICE_PATTERN(TRANS) /= 0.) THEN
               TABLE_ENERGY_PRICE(TRANS) = TABLE_ENERGY_PRICE(TRANS) *
     +             GET_VAR(MONTHLY_ENERGY_PRICE_PATTERN(TRANS),
     +                                  R_ISEAS,"ForcastPrice Pattern")
            ENDIF
!
            ! TODO:  If it's a KW to MW conversion, create a KW/MW
            ! conversion routine and call it. Don't put the code here
            ! and put a comment above it.  In the (at least) three
            ! places
            ! KW and MW are converted to each other, they should ALL
            ! call the same routine.

            ! KW TO MW CONVERSION
            LOCAL_DEMAND_PRICE = 1000. * MARKET_DEMAND_PRICE(TRANS)
            IF(MONTHLY_DEMAND_PRICE_PATTERN(TRANS) /= 0.) THEN
               LOCAL_DEMAND_PRICE = LOCAL_DEMAND_PRICE *
     +             GET_VAR(MONTHLY_DEMAND_PRICE_PATTERN(TRANS),
     +                                  R_ISEAS,"ForcastPrice Pattern")
            ENDIF
!
! 01/06/01. GAT
!
            TABLE_ENERGY_REVENUE(TRANS) = TABLE_ENERGY_PRICE(TRANS) *
     +                                MONTHLY_TABLE_SALES_ENERGY(TRANS)
!
            IF(DEMAND_PRICING_METHOD(TRANS) == 'A') THEN
               PRICING_PEAK = ANNUAL_PEAK(TRANS)
            ELSE
               PRICING_PEAK = MONTHLY_TABLE_PEAK_SALES(TRANS,R_ISEAS)
            ENDIF
            TABLE_DEMAND_REVENUE(TRANS) = LOCAL_DEMAND_PRICE *
     +                                                     PRICING_PEAK
            TABLE_CUSTOMER_REVENUE(TRANS) =
     +                        MARKET_CUSTOMER_PRICE(TRANS) *
     +                                 MONTHLY_CUSTOMERS(R_ISEAS,TRANS)

            TRANS_ENERGY_REVENUE(TG) = TRANS_ENERGY_REVENUE(TG) +
     +                                      TABLE_ENERGY_REVENUE(TRANS)
            TRANS_DEMAND_REVENUE(TG) = TRANS_DEMAND_REVENUE(TG) +
     +                                      TABLE_DEMAND_REVENUE(TRANS)
            TRANS_CUSTOMER_REVENUE(TG) = TRANS_CUSTOMER_REVENUE(TG) +
     +                                    TABLE_CUSTOMER_REVENUE(TRANS)
!
            MONTHLY_CLASS_ENERGY(CG) = MONTHLY_CLASS_ENERGY(CG) +
     +                                      MONTHLY_TABLE_ENERGY(TRANS)
!
            MONTHLY_CLASS_CUSTOMERS(CG) = MONTHLY_CLASS_CUSTOMERS(CG) +
     +                                 MONTHLY_CUSTOMERS(R_ISEAS,TRANS)
!
            MONTHLY_CLASS_PEAK(CG) = MONTHLY_CLASS_PEAK(CG)
     +                               + PRICING_PEAK
!
            CLASS_ENERGY_REVENUE(CG) = CLASS_ENERGY_REVENUE(CG) +
     +                                      TABLE_ENERGY_REVENUE(TRANS)
            CLASS_PEAK_REVENUE(CG) = CLASS_PEAK_REVENUE(CG) +
     +                                      TABLE_DEMAND_REVENUE(TRANS)
            CLASS_CUSTOMER_REVENUE(CG) = CLASS_CUSTOMER_REVENUE(CG) +
     +                                    TABLE_CUSTOMER_REVENUE(TRANS)
!
! SECOND INDEX: 1 = ENERGY, 2 = PEAK, 3 = CUSTOMERS
! FIFTH INDEX:  1 = $, 2 = UNITS AT BUSBAR (ENERGY, PEAK, CUSTOMERS);
! 3 = UNITS AT METER
!
            RI = REV_CLASS_INDEX(TRANS,1) ! REVENUE FROM FIXED
            !ENERGY PRICING
            MONTHLY_AC_CONTRACT_REVENUE(AC,1,R_ISEAS,RI,1) =
     +                 MONTHLY_AC_CONTRACT_REVENUE(AC,1,R_ISEAS,RI,1) +
     +                                      TABLE_ENERGY_REVENUE(TRANS)
            MONTHLY_AC_CONTRACT_REVENUE(AC,1,0,RI,1) =
     +                    MONTHLY_AC_CONTRACT_REVENUE(AC,1,0,RI,1) +
     +                                      TABLE_ENERGY_REVENUE(TRANS)
!
            MONTHLY_AC_CONTRACT_REVENUE(AC,1,R_ISEAS,RI,2) =
     +                 MONTHLY_AC_CONTRACT_REVENUE(AC,1,R_ISEAS,RI,2) +
     +                                      MONTHLY_TABLE_ENERGY(TRANS)
            MONTHLY_AC_CONTRACT_REVENUE(AC,1,0,RI,2) =
     +                    MONTHLY_AC_CONTRACT_REVENUE(AC,1,0,RI,2) +
     +                                      MONTHLY_TABLE_ENERGY(TRANS)
! 01/06/02.
            MONTHLY_AC_CONTRACT_REVENUE(AC,1,R_ISEAS,RI,3) =
     +                 MONTHLY_AC_CONTRACT_REVENUE(AC,1,R_ISEAS,RI,3) +
     +                                MONTHLY_TABLE_SALES_ENERGY(TRANS)
            MONTHLY_AC_CONTRACT_REVENUE(AC,1,0,RI,3) =
     +                    MONTHLY_AC_CONTRACT_REVENUE(AC,1,0,RI,3) +
     +                                MONTHLY_TABLE_SALES_ENERGY(TRANS)
!
            RI = REV_CLASS_INDEX(TRANS,2) ! REVENUE FROM DEMAND PRICING
            MONTHLY_AC_CONTRACT_REVENUE(AC,2,R_ISEAS,RI,1) =
     +                 MONTHLY_AC_CONTRACT_REVENUE(AC,2,R_ISEAS,RI,1) +
     +                                      TABLE_DEMAND_REVENUE(TRANS)
            MONTHLY_AC_CONTRACT_REVENUE(AC,2,0,RI,1) =
     +                       MONTHLY_AC_CONTRACT_REVENUE(AC,2,0,RI,1) +
     +                                      TABLE_DEMAND_REVENUE(TRANS)
!
            MONTHLY_AC_CONTRACT_REVENUE(AC,2,R_ISEAS,RI,2) =
     +                 MONTHLY_AC_CONTRACT_REVENUE(AC,2,R_ISEAS,RI,2) +
     +                                                     PRICING_PEAK
            MONTHLY_AC_CONTRACT_REVENUE(AC,2,0,RI,2) =
     +                       MONTHLY_AC_CONTRACT_REVENUE(AC,2,0,RI,2) +
     +                                                     PRICING_PEAK
           RI = REV_CLASS_INDEX(TRANS,3) ! REVENUE FROM CUSTOMER PRICING
            MONTHLY_AC_CONTRACT_REVENUE(AC,3,R_ISEAS,RI,1) =
     +                 MONTHLY_AC_CONTRACT_REVENUE(AC,3,R_ISEAS,RI,1) +
     +                                    TABLE_CUSTOMER_REVENUE(TRANS)
            MONTHLY_AC_CONTRACT_REVENUE(AC,3,0,RI,1) =
     +                       MONTHLY_AC_CONTRACT_REVENUE(AC,3,0,RI,1) +
     +                                    TABLE_CUSTOMER_REVENUE(TRANS)
!
            MONTHLY_AC_CONTRACT_REVENUE(AC,3,R_ISEAS,RI,2) =
     +                 MONTHLY_AC_CONTRACT_REVENUE(AC,3,R_ISEAS,RI,2) +
     +                                 MONTHLY_CUSTOMERS(R_ISEAS,TRANS)
            MONTHLY_AC_CONTRACT_REVENUE(AC,3,0,RI,2) =
     +                       MONTHLY_AC_CONTRACT_REVENUE(AC,3,0,RI,2) +
     +                                 MONTHLY_CUSTOMERS(R_ISEAS,TRANS)
!
!
           LOCAL_CUSTOMER_NAME(CG) = CUSTOMER_CLASS_NAME(TRANS)(1:24)//
     +                                   BASECASE_MKT_AREA_ID_tfo(TRANS)

           IF(INTRA_COMPANY_TRANSACTION(TRANS) == 'Y') THEN
               RI = INCOME_STATEMENT_POSITION(
     +                             INTRA_ACCOUNT_CLASSIFICATION(TRANS))
               IF(INTRA_EXPENSE_COLLECTION(TRANS) == 'BTL') RI = 28
               AC = INTRA_ASSET_CLASS_ID(TRANS)
               AC = ASSET_CLASS_GROUPS_INDEX(AC)
               MONTHLY_AC_CONTRACT_EXPENSE(AC,R_ISEAS,RI) =
     +                       MONTHLY_AC_CONTRACT_EXPENSE(AC,R_ISEAS,RI)
     +                        + TABLE_ENERGY_REVENUE(TRANS)
     +                        + TABLE_DEMAND_REVENUE(TRANS)
     +                        + TABLE_CUSTOMER_REVENUE(TRANS)
               MONTHLY_AC_CONTRACT_EXPENSE(AC,0,RI) =
     +                        MONTHLY_AC_CONTRACT_EXPENSE(AC,0,RI)
     +                        + TABLE_ENERGY_REVENUE(TRANS)
     +                        + TABLE_DEMAND_REVENUE(TRANS)
     +                        + TABLE_CUSTOMER_REVENUE(TRANS)
               MONTHLY_AC_CONTRACT_EXPENSE(-1,R_ISEAS,RI) =
     +                       MONTHLY_AC_CONTRACT_EXPENSE(-1,R_ISEAS,RI)
     +                        + TABLE_ENERGY_REVENUE(TRANS)
     +                        + TABLE_DEMAND_REVENUE(TRANS)
     +                        + TABLE_CUSTOMER_REVENUE(TRANS)
               MONTHLY_AC_CONTRACT_EXPENSE(-1,0,RI) =
     +                        MONTHLY_AC_CONTRACT_EXPENSE(-1,0,RI)
     +                        + TABLE_ENERGY_REVENUE(TRANS)
     +                        + TABLE_DEMAND_REVENUE(TRANS)
     +                        + TABLE_CUSTOMER_REVENUE(TRANS)
       RI = REV_CLASS_INDEX(TRANS,1) ! REVENUE FROM FIXED ENERGY PRICING
               MONTHLY_AC_CONTRACT_REVENUE(-1,1,R_ISEAS,RI,1) =
     +                   MONTHLY_AC_CONTRACT_REVENUE(-1,1,R_ISEAS,RI,1)
     +                    + TABLE_ENERGY_REVENUE(TRANS)
               MONTHLY_AC_CONTRACT_REVENUE(-1,1,0,RI,1) =
     +                         MONTHLY_AC_CONTRACT_REVENUE(-1,1,0,RI,1)
     +                          + TABLE_ENERGY_REVENUE(TRANS)
             RI = REV_CLASS_INDEX(TRANS,2) ! REVENUE FROM DEMAND PRICING
               MONTHLY_AC_CONTRACT_REVENUE(-1,2,R_ISEAS,RI,1) =
     +                   MONTHLY_AC_CONTRACT_REVENUE(-1,2,R_ISEAS,RI,1)
     +                    + TABLE_DEMAND_REVENUE(TRANS)
               MONTHLY_AC_CONTRACT_REVENUE(-1,2,0,RI,1) =
     +                         MONTHLY_AC_CONTRACT_REVENUE(-1,2,0,RI,1)
     +                          + TABLE_DEMAND_REVENUE(TRANS)
          RI = REV_CLASS_INDEX(TRANS,3) ! REVENUE FROM CUSTOMER PRICING
               MONTHLY_AC_CONTRACT_REVENUE(-1,3,R_ISEAS,RI,1) =
     +                   MONTHLY_AC_CONTRACT_REVENUE(-1,3,R_ISEAS,RI,1)
     +                    + TABLE_CUSTOMER_REVENUE(TRANS)
               MONTHLY_AC_CONTRACT_REVENUE(-1,3,0,RI,1) =
     +                         MONTHLY_AC_CONTRACT_REVENUE(-1,3,0,RI,1)
     +                          + TABLE_CUSTOMER_REVENUE(TRANS)
            ENDIF
!
! MONTHLY CONTRACT REVENUES REPORT
!
!
! 100907. CAPACITY MARKETS EXPENSES.
!
            IF(capacity_market_pointer_array_tfo(TRANS) > 0) THEN
               PRIMARY_FUEL_ID = -1.*
     +     (10000. + FLOAT(capacity_market_pointer_array_tfo(TRANS)))

               CALL GET_A_CAP_MARKET_PRICE(real(PRIMARY_FUEL_ID),
     +      CAP_MARKET_RATE, R_ISEAS, R_YEAR)


!
! IF AN ANNUAL CAPACITY CHARGE IN A GIVEN MONTH
! EXPENSES ONLY.
!
               IF(CAP_MARKET_MONTH_NO(TRANS) /= 13) THEN
                  TEMP_I2 = CAP_MARKET_MONTH_NO(TRANS)
               ELSE
                  TEMP_I2 = R_ISEAS
               ENDIF
               PLAN_FACTOR = PEAK_COIN_FACTOR(TRANS)

               PLAN_FACTOR = PLAN_FACTOR *
     +                             CAPACITY_MARKET_COIN_ADJ_FACT(TRANS)

               TEMP_R4 = LAST_THIS_YR_PEAK(2,TEMP_I2,TRANS)
!
               TEMP_R4 = TEMP_R4 * PLAN_FACTOR
               TF_TG_CAP_MARKET_MW(TG) = TF_TG_CAP_MARKET_MW(TG) +
     +                                                          TEMP_R4
               TEMP_R4 = TEMP_R4 * CAP_MARKET_RATE
               TF_TG_CAP_MARKET_COST(TG) =
     +                              TF_TG_CAP_MARKET_COST(TG) + TEMP_R4
               CAP_CUSTOMER_EXPENSE(CG) = CAP_CUSTOMER_EXPENSE(CG) +
     +                                                  TEMP_R4 * 0.001
!
! CAPACITY PURCHASE - SHOULD BE PURCHASED CAPACITY, NOT PURCHASED GAS
!
               IF(CAP_MARKET_TYPE_INDEX(TRANS) == 1) THEN
                  MONTHLY_AC_CONTRACT_EXPENSE(AC,R_ISEAS,
     +                                Purchased_Capacity_to_Level_RM) =
     +                  MONTHLY_AC_CONTRACT_EXPENSE(AC,R_ISEAS,
     +                                  Purchased_Capacity_to_Level_RM)
     +                                                + TEMP_R4 * 1000.
                  MONTHLY_AC_CONTRACT_EXPENSE(AC,0,
     +                                Purchased_Capacity_to_Level_RM) =
     +                  MONTHLY_AC_CONTRACT_EXPENSE(AC,0,
     +                                  Purchased_Capacity_to_Level_RM)
     +                                                + TEMP_R4 * 1000.
!
               ELSEIF(CAP_MARKET_TYPE_INDEX(TRANS) == 2) THEN
! PURCHASE POWER - WORKS. NO NEED FOR MODIFICATION.
                  MONTHLY_AC_CONTRACT_EXPENSE(AC,R_ISEAS,
     +                                               PurchasedPower) =
     +                  MONTHLY_AC_CONTRACT_EXPENSE(AC,R_ISEAS,
     +                                               PurchasedPower) +
     +                                                  TEMP_R4 * 1000.
                  MONTHLY_AC_CONTRACT_EXPENSE(AC,0,PurchasedPower) =
     +                  MONTHLY_AC_CONTRACT_EXPENSE(AC,0,
     +                             PurchasedPower) + TEMP_R4 * 1000.
               ELSEIF(CAP_MARKET_TYPE_INDEX(TRANS) == 3) THEN


                  RI = CapacitySales
                  MONTHLY_AC_CONTRACT_REVENUE(AC,2,R_ISEAS,RI,3) =

     +                 MONTHLY_AC_CONTRACT_REVENUE(AC,2,R_ISEAS,RI,1) -
     +                                                  TEMP_R4 * 1000.
                  MONTHLY_AC_CONTRACT_REVENUE(AC,2,0,RI,3) =
     +                       MONTHLY_AC_CONTRACT_REVENUE(AC,2,0,RI,1) -
     +                                                  TEMP_R4 * 1000.
               ENDIF
            ENDIF ! CAPACITY MARKET POINTER /= 0
!
         ENDDO ! TRANSACTION TABLES
!
! CREATES LDC'S THAT ARE PASSED INTO HYDRO LOGIC.
!
         ! EXCLUDING TRANS = 0 (SYSTEM) FOR NOW
         DO TRANS = 1, MAX_TRANS_LOAD_GROUPS
! DOUBLE INDEX
            TG = TRANS_LOAD_GROUP_2_TG(TRANS)
            TG = GET_TRANS_GROUP_POSITION(TG)

            TRANS_LOOP_POS = TRANS
            CALL TRANSACT_LOAD_PROB(R_HOURS,
     +                              TRANS_HOURLY_LOAD(1,TRANS),
     +                              WH_LOADS_PER_HOUR(1,TRANS),
     +                              MONTHLY_TRANS_ENERGY(TRANS),
     +                              MONTHLY_TRANS_PEAK(TRANS),
     +                              MONTHLY_TRANS_BASE(TRANS),
     +                              TRANS_LOOP_POS,
     +                              R_ISEAS,
     +                              TG)
         ENDDO
!
! CREATES LDC'S THAT ARE PASSED INTO HYDRO LOGIC.
!
         ! EXCLUDING TRANS = 0 (SYSTEM) FOR NOW
         DO TRANS = 1, NUMBER_OF_HYDRO_GROUPS
!
            HG = GET_HG_FROM_TG(TG)

            CALL HYDRO_LOAD_PROB(R_HOURS,
     +                              HYDRO_HOURLY_LOAD(1,TRANS),
     +                              MONTHLY_HYDRO_ENERGY(TRANS),
     +                              MONTHLY_HYDRO_PEAK(TRANS),
     +                              MONTHLY_HYDRO_BASE(TRANS),
     +                              TRANS,
     +                              R_ISEAS,
     +                              HG)
         ENDDO
!
! 11/7/99. GAT.
!
         IF(USE_SCENARIO_HOURLY_DEMAND) THEN
            CALL CLOSE_SCEN_HOURLY_LOAD_FILE(SCENARIO_HOURLY_UNIT)
         ENDIF
!
!         DEALLOCATE( REF_HOURLY_LOAD)
         MONTHLY_TRANSACTION_LOADS = .TRUE.
      RETURN
!
! 3/19/02. FOR BURESH TO GET INDEXED REVENUES INTO THE REPORT
!          CALL AFTER TRANSACT.


      ENTRY GET_TG_MONTH_SUM_B4_HYDRO(R_TG,R_TG_ENERGY,R_TG_PEAK,
     +                                                      R_TG_BASE)


         IF(ALLOCATED(MONTHLY_TRANS_ENERGY) .AND. R_TG > 0) THEN
            GET_TG_MONTH_SUM_B4_HYDRO = .TRUE.
            TG = TRANS_LOAD_GROUPS_INDEX(R_TG)
            R_TG_ENERGY = MONTHLY_TRANS_ENERGY(TG)
            R_TG_PEAK = MONTHLY_TRANS_PEAK(TG)
            R_TG_BASE = MONTHLY_TRANS_BASE(TG)
         ELSE
            GET_TG_MONTH_SUM_B4_HYDRO = .FALSE.
            R_TG_ENERGY = 0.0
            R_TG_PEAK = 0.0
            R_TG_BASE = 0.0
         ENDIF
      RETURN


      ENTRY GET_TF_TG_CAP_MARKET_MW(R_TG)


         IF(ALLOCATED(TF_TG_CAP_MARKET_MW)) THEN
            GET_TF_TG_CAP_MARKET_MW = TF_TG_CAP_MARKET_MW(R_TG)
         ELSE
            GET_TF_TG_CAP_MARKET_MW = 0.0
         ENDIF
      RETURN


      ENTRY GET_TF_TG_CAP_MARKET_COST(R_TG)


         IF(ALLOCATED(TF_TG_CAP_MARKET_COST)) THEN
            GET_TF_TG_CAP_MARKET_COST = TF_TG_CAP_MARKET_COST(R_TG)
         ELSE
            GET_TF_TG_CAP_MARKET_COST = 0.0
         ENDIF
      RETURN
!

      ENTRY WRITE_MONTHLY_CLASS_SUMMARY(R_ISEAS)

         YES_MONTHLY_CLASS_REPORTS = TF_CLASS_SUMMARY_REPORTS()
         IF(TF_CLASS_REPORT_NOT_OPEN .AND.
     +                  YES_MONTHLY_CLASS_REPORTS .AND.

     +                                        .NOT. TESTING_PLAN ) THEN
            TF_CLASS_REPORT_NOT_OPEN = .FALSE.
            TRANS_CLASS_SUM_UNIT = TRANS_CLASS_SUM_HEADER(
     +                                             TRANS_CLASS_SUM_REC)
            LAST_SEASON = get_PRODUCTION_PERIODS_in()

            CL_MONTH_NAME(LAST_SEASON+1) = 'Annual'
            ANNUAL_COUNTER = LAST_SEASON + 1
         ENDIF
         IF(YES_MONTHLY_CLASS_REPORTS .AND. .NOT. TESTING_PLAN) THEN
            TOTAL_CLASS_ENERGY_REVENUE = 0.
            TOTAL_CLASS_PEAK_REVENUE = 0.
            TOTAL_CLASS_CUSTOMER_REVENUE = 0.
            TOTAL_CAP_CUSTOMER_EXPENSE = 0.
            TOTAL_TRANS_INDEXED_REVENUE = 0.
            TOTAL_MONTHLY_CLASS_ENERGY = 0.
            TOTAL_MONTHLY_CLASS_PEAK = 0.
            TOTAL_MONTHLY_CLASS_CUSTOMERS = 0.
!
! 3/19/02. CHANGED NOT_AVAIL TO 0. TO ACCOMODATE DATA TRANSFERS.
!
            DO TRANS = 1, MAX_CUST_CLASS_GROUPS
               ENRG_AVERAGE_REVENUE = 0.
               IF(MONTHLY_CLASS_ENERGY(TRANS) /= 0.)
     +           ENRG_AVERAGE_REVENUE = CLASS_ENERGY_REVENUE(TRANS)/
     +                                  MONTHLY_CLASS_ENERGY(TRANS)
!
               INDEXED_ENRG_AVERAGE_REVENUE = 0.
               IF(MONTHLY_CLASS_ENERGY(TRANS) /= 0.)
     +           INDEXED_ENRG_AVERAGE_REVENUE =
     +                     TRANS_INDEXED_REVENUE(TRANS)/
     +                                  MONTHLY_CLASS_ENERGY(TRANS)
!
               PEAK_AVERAGE_REVENUE = 0.
               IF(MONTHLY_CLASS_PEAK(TRANS) /= 0.)
     +           PEAK_AVERAGE_REVENUE = CLASS_PEAK_REVENUE(TRANS)/
     +                                  MONTHLY_CLASS_PEAK(TRANS)
               CUSTOMER_AVERAGE_REVENUE = 0.
               IF(MONTHLY_CLASS_CUSTOMERS(TRANS) /= 0.)
     +          CUSTOMER_AVERAGE_REVENUE=CLASS_CUSTOMER_REVENUE(TRANS)/
     +                                   MONTHLY_CLASS_CUSTOMERS(TRANS)
               TRANS_CLASS_SUM_REC = rptrec(TRANS_CLASS_SUM_UNIT)
               WRITE(TRANS_CLASS_SUM_UNIT,REC=TRANS_CLASS_SUM_REC)
     +            PRT_ENDPOINT(),
     +            FLOAT(LOCAL_YEAR),
     +            CL_MONTH_NAME(R_ISEAS),
     +            LOCAL_CUSTOMER_NAME(TRANS),
     +            CLASS_ENERGY_REVENUE(TRANS),
     +            CLASS_PEAK_REVENUE(TRANS),
     +            CLASS_CUSTOMER_REVENUE(TRANS),
     +            MONTHLY_CLASS_ENERGY(TRANS),
     +            MONTHLY_CLASS_PEAK(TRANS),
     +            MONTHLY_CLASS_CUSTOMERS(TRANS),
     +            CLASS_ENERGY_REVENUE(TRANS)
     +             + CLASS_PEAK_REVENUE(TRANS)
     +             + CLASS_CUSTOMER_REVENUE(TRANS)
     +             + TRANS_INDEXED_REVENUE(TRANS),
     +            ENRG_AVERAGE_REVENUE,
     +            PEAK_AVERAGE_REVENUE,
     +            CUSTOMER_AVERAGE_REVENUE,
     +            TRANS_INDEXED_REVENUE(TRANS),
     +            INDEXED_ENRG_AVERAGE_REVENUE,
     +            CAP_CUSTOMER_EXPENSE(TRANS)
               TRANS_CLASS_SUM_REC = TRANS_CLASS_SUM_REC + 1
!
              TOTAL_CLASS_ENERGY_REVENUE = TOTAL_CLASS_ENERGY_REVENUE +
     +                                      CLASS_ENERGY_REVENUE(TRANS)
               TOTAL_CLASS_PEAK_REVENUE = TOTAL_CLASS_PEAK_REVENUE +
     +                                    CLASS_PEAK_REVENUE(TRANS)
               TOTAL_CLASS_CUSTOMER_REVENUE =
     +                     TOTAL_CLASS_CUSTOMER_REVENUE +
     +                                    CLASS_CUSTOMER_REVENUE(TRANS)
              TOTAL_CAP_CUSTOMER_EXPENSE = TOTAL_CAP_CUSTOMER_EXPENSE +
     +                                      CAP_CUSTOMER_EXPENSE(TRANS)
              TOTAL_MONTHLY_CLASS_ENERGY = TOTAL_MONTHLY_CLASS_ENERGY +
     +                                      MONTHLY_CLASS_ENERGY(TRANS)
               TOTAL_MONTHLY_CLASS_PEAK = TOTAL_MONTHLY_CLASS_PEAK +
     +                                       MONTHLY_CLASS_PEAK(TRANS)
               TOTAL_MONTHLY_CLASS_CUSTOMERS =
     +                     TOTAL_MONTHLY_CLASS_CUSTOMERS +
     +                                   MONTHLY_CLASS_CUSTOMERS(TRANS)
               TOTAL_TRANS_INDEXED_REVENUE =
     +                       TOTAL_TRANS_INDEXED_REVENUE +
     +                                     TRANS_INDEXED_REVENUE(TRANS)
               ANNUAL_CLASS_ENERGY_REVENUE(TRANS) =
     +                             ANNUAL_CLASS_ENERGY_REVENUE(TRANS) +
     +                                      CLASS_ENERGY_REVENUE(TRANS)
               ANNUAL_CLASS_PEAK_REVENUE(TRANS) =
     +                              ANNUAL_CLASS_PEAK_REVENUE(TRANS) +
     +                                        CLASS_PEAK_REVENUE(TRANS)
               ANNUAL_CLASS_CUSTOMER_REVENUE(TRANS) =
     +                           ANNUAL_CLASS_CUSTOMER_REVENUE(TRANS) +
     +                                    CLASS_CUSTOMER_REVENUE(TRANS)
               ANNUAL_CAP_CUSTOMER_EXPENSE(TRANS) =
     +                            ANNUAL_CAP_CUSTOMER_EXPENSE(TRANS) +
     +                                      CAP_CUSTOMER_EXPENSE(TRANS)
               ANNUAL_CLASS_ENERGY(TRANS) =
     +                              ANNUAL_CLASS_ENERGY(TRANS) +
     +                                      MONTHLY_CLASS_ENERGY(TRANS)
! SUM OF PEAKS REPLACED BY MAX OF PEAKS 2/12/11 MSG

               ANNUAL_CLASS_PEAK(TRANS) =
     +                              MAX(ANNUAL_CLASS_PEAK(TRANS),
     +                                       MONTHLY_CLASS_PEAK(TRANS))
               ANNUAL_CLASS_CUSTOMERS(TRANS) =
     +                              ANNUAL_CLASS_CUSTOMERS(TRANS) +
     +                                   MONTHLY_CLASS_CUSTOMERS(TRANS)
               ANNUAL_TRANS_INDEXED_REVENUE(TRANS) =
     +                       ANNUAL_TRANS_INDEXED_REVENUE(TRANS) +
     +                                     TRANS_INDEXED_REVENUE(TRANS)
            ENDDO
            ENRG_AVERAGE_REVENUE = 0.
            IF(TOTAL_MONTHLY_CLASS_ENERGY /= 0.)
     +        ENRG_AVERAGE_REVENUE = TOTAL_CLASS_ENERGY_REVENUE/
     +                               TOTAL_MONTHLY_CLASS_ENERGY
!
            INDEXED_ENRG_AVERAGE_REVENUE = 0.
            IF(TOTAL_MONTHLY_CLASS_ENERGY /= 0.)
     +        INDEXED_ENRG_AVERAGE_REVENUE =
     +                  TOTAL_TRANS_INDEXED_REVENUE/
     +                               TOTAL_MONTHLY_CLASS_ENERGY
!
            PEAK_AVERAGE_REVENUE = 0.
            IF(TOTAL_MONTHLY_CLASS_PEAK /= 0.)
     +        PEAK_AVERAGE_REVENUE = TOTAL_CLASS_PEAK_REVENUE/
     +                               TOTAL_MONTHLY_CLASS_PEAK
            CUSTOMER_AVERAGE_REVENUE = 0.
            IF(TOTAL_MONTHLY_CLASS_CUSTOMERS /= 0.)
     +        CUSTOMER_AVERAGE_REVENUE = TOTAL_CLASS_CUSTOMER_REVENUE/
     +                                   TOTAL_MONTHLY_CLASS_CUSTOMERS
            TRANS_CLASS_SUM_REC = rptrec(TRANS_CLASS_SUM_UNIT)
            WRITE(TRANS_CLASS_SUM_UNIT,REC=TRANS_CLASS_SUM_REC)
     +            PRT_ENDPOINT(),
     +            FLOAT(LOCAL_YEAR),
     +            CL_MONTH_NAME(R_ISEAS),
     +            'Total                         ',
     +            TOTAL_CLASS_ENERGY_REVENUE,
     +            TOTAL_CLASS_PEAK_REVENUE,
     +            TOTAL_CLASS_CUSTOMER_REVENUE,
     +            TOTAL_MONTHLY_CLASS_ENERGY,
     +            TOTAL_MONTHLY_CLASS_PEAK,
     +            TOTAL_MONTHLY_CLASS_CUSTOMERS,
     +            TOTAL_CLASS_ENERGY_REVENUE
     +             + TOTAL_CLASS_PEAK_REVENUE
     +             + TOTAL_CLASS_CUSTOMER_REVENUE
     +             + TOTAL_TRANS_INDEXED_REVENUE,
     +            ENRG_AVERAGE_REVENUE,
     +            PEAK_AVERAGE_REVENUE,
     +            CUSTOMER_AVERAGE_REVENUE,
     +            TOTAL_TRANS_INDEXED_REVENUE,
     +            INDEXED_ENRG_AVERAGE_REVENUE,
     +            TOTAL_CAP_CUSTOMER_EXPENSE
            TRANS_CLASS_SUM_REC = TRANS_CLASS_SUM_REC + 1
            IF(R_ISEAS == 12) THEN
!
               TOTAL_CLASS_ENERGY_REVENUE = 0.
               TOTAL_CLASS_PEAK_REVENUE = 0.
               TOTAL_CLASS_CUSTOMER_REVENUE = 0.
               TOTAL_CAP_CUSTOMER_EXPENSE = 0.
               TOTAL_TRANS_INDEXED_REVENUE = 0.
               TOTAL_MONTHLY_CLASS_ENERGY = 0.
               TOTAL_MONTHLY_CLASS_PEAK = 0.
               TOTAL_MONTHLY_CLASS_CUSTOMERS = 0.
!
               DO TRANS = 1, MAX_CUST_CLASS_GROUPS
                  ENRG_AVERAGE_REVENUE = 0.
                  IF(ANNUAL_CLASS_ENERGY(TRANS) /= 0.)
     +              ENRG_AVERAGE_REVENUE =
     +                              ANNUAL_CLASS_ENERGY_REVENUE(TRANS)/
     +                                   ANNUAL_CLASS_ENERGY(TRANS)
!
                  INDEXED_ENRG_AVERAGE_REVENUE = 0.
                  IF(ANNUAL_CLASS_ENERGY(TRANS) /= 0.)
     +               INDEXED_ENRG_AVERAGE_REVENUE =
     +                     ANNUAL_TRANS_INDEXED_REVENUE(TRANS)/
     +                                  ANNUAL_CLASS_ENERGY(TRANS)
!
                  PEAK_AVERAGE_REVENUE = 0.
                  IF(ANNUAL_CLASS_PEAK(TRANS) /= 0.)
     +              PEAK_AVERAGE_REVENUE =
     +                                ANNUAL_CLASS_PEAK_REVENUE(TRANS)/
     +                                    ANNUAL_CLASS_PEAK(TRANS)
                  CUSTOMER_AVERAGE_REVENUE = 0.
                  IF(ANNUAL_CLASS_CUSTOMERS(TRANS) /= 0.)
     +              CUSTOMER_AVERAGE_REVENUE =
     +                            ANNUAL_CLASS_CUSTOMER_REVENUE(TRANS)/
     +                                  ANNUAL_CLASS_CUSTOMERS(TRANS)
                 TRANS_CLASS_SUM_REC = rptrec(TRANS_CLASS_SUM_UNIT)
                  WRITE(TRANS_CLASS_SUM_UNIT,REC=TRANS_CLASS_SUM_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(LOCAL_YEAR),
     +               CL_MONTH_NAME(13),
     +               LOCAL_CUSTOMER_NAME(TRANS),
     +               ANNUAL_CLASS_ENERGY_REVENUE(TRANS),
     +               ANNUAL_CLASS_PEAK_REVENUE(TRANS),
     +               ANNUAL_CLASS_CUSTOMER_REVENUE(TRANS),
     +               ANNUAL_CLASS_ENERGY(TRANS),
     +               ANNUAL_CLASS_PEAK(TRANS),
     +               ANNUAL_CLASS_CUSTOMERS(TRANS),
     +               ANNUAL_CLASS_ENERGY_REVENUE(TRANS)
     +                + ANNUAL_CLASS_PEAK_REVENUE(TRANS)
     +                + ANNUAL_CLASS_CUSTOMER_REVENUE(TRANS)
     +                + ANNUAL_TRANS_INDEXED_REVENUE(TRANS),
     +               ENRG_AVERAGE_REVENUE,
     +               PEAK_AVERAGE_REVENUE,
     +               CUSTOMER_AVERAGE_REVENUE,
     +               ANNUAL_TRANS_INDEXED_REVENUE(TRANS),
     +               INDEXED_ENRG_AVERAGE_REVENUE,
     +               ANNUAL_CAP_CUSTOMER_EXPENSE(TRANS)
                  TRANS_CLASS_SUM_REC = TRANS_CLASS_SUM_REC + 1
!
                  TOTAL_CLASS_ENERGY_REVENUE =
     +                     TOTAL_CLASS_ENERGY_REVENUE +
     +                               ANNUAL_CLASS_ENERGY_REVENUE(TRANS)
                  TOTAL_CLASS_PEAK_REVENUE = TOTAL_CLASS_PEAK_REVENUE +
     +                                 ANNUAL_CLASS_PEAK_REVENUE(TRANS)
                  TOTAL_CLASS_CUSTOMER_REVENUE =
     +                     TOTAL_CLASS_CUSTOMER_REVENUE +
     +                             ANNUAL_CLASS_CUSTOMER_REVENUE(TRANS)
                  TOTAL_CAP_CUSTOMER_EXPENSE =
     +                     TOTAL_CAP_CUSTOMER_EXPENSE +
     +                               ANNUAL_CAP_CUSTOMER_EXPENSE(TRANS)
                  TOTAL_MONTHLY_CLASS_ENERGY =
     +                     TOTAL_MONTHLY_CLASS_ENERGY +
     +                                       ANNUAL_CLASS_ENERGY(TRANS)
                  TOTAL_MONTHLY_CLASS_PEAK = TOTAL_MONTHLY_CLASS_PEAK +
     +                                         ANNUAL_CLASS_PEAK(TRANS)
                  TOTAL_MONTHLY_CLASS_CUSTOMERS =
     +                     TOTAL_MONTHLY_CLASS_CUSTOMERS +
     +                                    ANNUAL_CLASS_CUSTOMERS(TRANS)
                  TOTAL_TRANS_INDEXED_REVENUE =
     +                       TOTAL_TRANS_INDEXED_REVENUE +
     +                              ANNUAL_TRANS_INDEXED_REVENUE(TRANS)
               ENDDO
               ENRG_AVERAGE_REVENUE = 0.
               IF(TOTAL_MONTHLY_CLASS_ENERGY /= 0.)
     +           ENRG_AVERAGE_REVENUE = TOTAL_CLASS_ENERGY_REVENUE/
     +                                  TOTAL_MONTHLY_CLASS_ENERGY
!
               INDEXED_ENRG_AVERAGE_REVENUE = 0.
               IF(TOTAL_MONTHLY_CLASS_ENERGY /= 0.)
     +           INDEXED_ENRG_AVERAGE_REVENUE =
     +                     TOTAL_TRANS_INDEXED_REVENUE/
     +                                  TOTAL_MONTHLY_CLASS_ENERGY
!
               PEAK_AVERAGE_REVENUE = 0.
               IF(TOTAL_MONTHLY_CLASS_PEAK /= 0.)
     +           PEAK_AVERAGE_REVENUE = TOTAL_CLASS_PEAK_REVENUE/
     +                                   TOTAL_MONTHLY_CLASS_PEAK
               CUSTOMER_AVERAGE_REVENUE = 0.
               IF(TOTAL_MONTHLY_CLASS_CUSTOMERS /= 0.)
     +           CUSTOMER_AVERAGE_REVENUE=TOTAL_CLASS_CUSTOMER_REVENUE/
     +                                    TOTAL_MONTHLY_CLASS_CUSTOMERS
               TRANS_CLASS_SUM_REC = rptrec(TRANS_CLASS_SUM_UNIT)
               WRITE(TRANS_CLASS_SUM_UNIT,REC=TRANS_CLASS_SUM_REC)
     +            PRT_ENDPOINT(),
     +            FLOAT(LOCAL_YEAR),
     +            CL_MONTH_NAME(13),
     +            'Total                         ',
     +            TOTAL_CLASS_ENERGY_REVENUE,
     +            TOTAL_CLASS_PEAK_REVENUE,
     +            TOTAL_CLASS_CUSTOMER_REVENUE,
     +            TOTAL_MONTHLY_CLASS_ENERGY,
     +            TOTAL_MONTHLY_CLASS_PEAK,
     +            TOTAL_MONTHLY_CLASS_CUSTOMERS,
     +            TOTAL_CLASS_ENERGY_REVENUE
     +             + TOTAL_CLASS_PEAK_REVENUE
     +             + TOTAL_CLASS_CUSTOMER_REVENUE
     +             + TOTAL_TRANS_INDEXED_REVENUE,
     +            ENRG_AVERAGE_REVENUE,
     +            PEAK_AVERAGE_REVENUE,
     +            CUSTOMER_AVERAGE_REVENUE,
     +            TOTAL_TRANS_INDEXED_REVENUE,
     +            INDEXED_ENRG_AVERAGE_REVENUE,
     +            TOTAL_CAP_CUSTOMER_EXPENSE
               TRANS_CLASS_SUM_REC = TRANS_CLASS_SUM_REC + 1
            ENDIF ! R_ISEAS == 12
         ENDIF
         WRITE_MONTHLY_CLASS_SUMMARY = .TRUE.
      RETURN


      ENTRY INIT_ANN_ALLOC_BLOCKS_2_CUST()


!
         NUNITS = get_nunits()
!
! 11/11/03
!
         IF(HIGHEST_TG_COUNT == 0 .OR.
     +                            MAX_TRANS_GROUPS_FROM_TG == 0) RETURN
!
         IF(ALLOCATED(ANN_LOAD_DISPATCH_COST_BY_UNIT))
     +                       DEALLOCATE(ANN_LOAD_DISPATCH_COST_BY_UNIT)
         ALLOCATE(ANN_LOAD_DISPATCH_COST_BY_UNIT(NUNITS+2,
     +                                        HIGHEST_TG_COUNT+1,
     +                                       MAX_TRANS_GROUPS_FROM_TG))
         IF(ALLOCATED(ANN_LOAD_DISPATCH_REV_BY_UNIT))
     +                        DEALLOCATE(ANN_LOAD_DISPATCH_REV_BY_UNIT)
         ALLOCATE(ANN_LOAD_DISPATCH_REV_BY_UNIT(NUNITS+2,
     +                                        HIGHEST_TG_COUNT+1,
     +                                       MAX_TRANS_GROUPS_FROM_TG))
         IF(ALLOCATED(ANN_LOAD_DISPATCH_BY_UNIT))
     +                            DEALLOCATE(ANN_LOAD_DISPATCH_BY_UNIT)
         ALLOCATE(ANN_LOAD_DISPATCH_BY_UNIT(NUNITS+2,
     +                                      HIGHEST_TG_COUNT+1,
     +                                      MAX_TRANS_GROUPS_FROM_TG))
         ANN_LOAD_DISPATCH_COST_BY_UNIT = 0.
         ANN_LOAD_DISPATCH_REV_BY_UNIT = 0.
         ANN_LOAD_DISPATCH_BY_UNIT = 0.
!
!
         RESOURCE_TO_LOAD_ALLOC = YES_RESOURCE_TO_LOAD_ALLOC(
     +                                    FUEL_AND_PURCHASE_COST_CAP,
     +                                    SYS_EMERGENCY_MW_FLOOR,
     +                                    SYS_EMERGENCY_COST_CAP)
!
         INIT_ANN_ALLOC_BLOCKS_2_CUST = .TRUE.
!
         IPL_FAC_REPORT = YES_IPL_FAC_REPORT()
         ANN_IPL_FAC_DB = 0.
         ELECT_PLAN_DATABASE = 0.
         YES_HOURLY_CUST_MARGIN_REPORT = HOURLY_CUST_MARGIN_REPORT()
         YES_TRANS_INTR_REPORT = TRANS_INTR_REPORT()
         IF(IPL_FAC_REPORT) THEN
         ENDIF
!
! CALCULABLE WITHIN TF
!
! IPL_FAC_DB(1) = Native load for the month
! IPL_FAC_DB(2) = Elect plan mwh
! IPL_FAC_DB(3) = IPL_FAC_DB(2)/IPL_FAC_DB(1)
!
! NEED TO USE NEW SWITCHES TO ALLOCATE LOAD
!
! MONTHLY TRANSACT C
!

! NEED CAPACITY, FUEL COST AT MINIMUM LOAD AND TOTAL SERVICE HOURS
!
      RETURN


      ENTRY INIT_MONTH_ALLOC_BLOCKS_2_CUST(R_TG)


!
! CUSTOMER PRIORITY = 1 => NATIVE
! CUSTOMER PRIORITY = 2 => ELECT PLAN
! CUSTOMER PRIORITY = 3 => WHOLESALE
!
!
         IPL_FAC_DB = 0.
!
         IF(IPL_FAC_REPORT) THEN
            J = 0
            DOWHILE(J < TG_COUNTER(R_TG))
               J = J + 1
!
!               K = LOAD_DISPATCH_POSITION(J,R_TG)
!               IF(K < 1 .OR. K > 2) CYCLE
! 010908.
!
               IF(JURISDICTIONAL_CUSTOMER(J) /= 'T') CYCLE
               K = 1
!
               IPL_FAC_DB(K) = IPL_FAC_DB(K) + MONTHLY_TABLE_ENERGY(J)
               IPL_FAC_DB(13) = IPL_FAC_DB(13) +
     +                        BASE_COST_OF_FAC_FUEL(J)*
     +                                          MONTHLY_TABLE_ENERGY(J)
!
            ENDDO
            IF(IPL_FAC_DB(1)+IPL_FAC_DB(2) > 0.01) THEN
               IPL_FAC_DB(3) = IPL_FAC_DB(2)/
     +                                    (IPL_FAC_DB(1)+IPL_FAC_DB(2))
            ELSE
               IPL_FAC_DB(3) = 0.
            ENDIF
!
            ANN_IPL_FAC_DB(1) = ANN_IPL_FAC_DB(1) + IPL_FAC_DB(1)
            ANN_IPL_FAC_DB(2) = ANN_IPL_FAC_DB(2) + IPL_FAC_DB(2)
            ANN_IPL_FAC_DB(13) = ANN_IPL_FAC_DB(13) + IPL_FAC_DB(13)
!
         ENDIF
!
      RETURN

      ENTRY ALLOCATE_BLOCKS_2_CUSTOMERS(
     +                         R_HOUR,                        ! I2
     +                         R_TG,                          ! I2
     +                         R_MONTH,                       ! I2
     +                         R_TOTAL_DISPATCH_BLOCKS,       ! I2
     +                         R_HOURLY_DISPATCH_BLOCKS,      ! I2(*)
     + R_GENERATION_BY_SEGMENT,       ! R4(*) vs R4(:,:)
     + R_DISPATCH_COST_FOR_BLOCK,     ! R4(*) vs R4(:)
     +                         R_INCREMENTAL_FUEL_COST,
     + R_RETAIL_REVENUE_FOR_BLOCK,    ! R4(*) vs R4(:)
     + R_MARKET_PRICE,                ! R4
     + R_WHOLESALE_PURCHASE,          ! R4
     + R_LAST_HOUR,                   ! I2
     + R_MUST_RUN_OPTIONS,            ! R4(*) vs R4(MAX_OUTAGE_BLOCKS)
     + R_UNIT_FOR_OUTAGE_BLOCK,       ! I2(*) vs
     + R_BLOCK_FOR_OUTAGE_BLOCK,      ! I2(*) vs
     + R_SORTED_OPTIONS,              ! I2(*) vs
     + R_THERMAL_LOAD,                ! R4 vs R4(:)
     +                         R_BY_UNIT,                     ! L1
     +                         R_UNSERVED_ENERGY,
     +                         R_UNSERVED_COST,
     +                         R_WHOLESALE_SALES,
     +                         R_WHOLESALE_PRICE)

!
! ROUTINE ALLOCATES RESOURCES TO LOADS
!
         IF(HIGHEST_TG_COUNT == 0 .OR.
     +                            MAX_TRANS_GROUPS_FROM_TG == 0) RETURN
         IF(LOAD_BY_BLOCK_RPT_NOT_OPEN) THEN
            LOAD_BY_BLOCK_RPT_NOT_OPEN = .FALSE.
            LOAD_BLOCK_VAR = HIGHEST_TG_COUNT + 1

            LOAD_BY_BLOCK_NO =

     +          MON_LOAD_BY_BLOCK_HEADER(LOAD_BLOCK_REC,LOAD_BLOCK_VAR)
         ENDIF
!
         IF(TRANS_INTR_NOT_OPEN) THEN
            TRANS_INTR_NOT_OPEN = .FALSE.
            TRANS_INTR_NO = TRANS_INTR_RPT_HEADER(TRANS_INTR_REC)
         ENDIF
         IF(HOURLY_CUST_MARGIN_NOT_OPEN) THEN
            HOURLY_CUST_MARGIN_NOT_OPEN = .FALSE.

            HOURLY_CUST_MARGIN_VAR = HIGHEST_TG_COUNT + 1
            HOURLY_CUST_MARGIN_NO =
     +           HOURLY_CUST_MARGIN_HEADER(HOURLY_CUST_MARGIN_REC,
     +                                     HOURLY_CUST_MARGIN_VAR)
            OPEN(10,FILE='MSG'//trim(GET_SCENAME())//'.BXA',
     +                                               STATUS="REPLACE")
      DO J = 1, HOURLY_CUST_MARGIN_VAR-1  ! RESIDENTIAL,V,0,,S,,,,,,,,""
!               K = LOAD_DISPATCH_INDEX(J,R_TG)
               VAR_NUM_STR = LEFT_JUSTIFY_I2_IN_STR(J-1_2)
               WRITE(10,*) CUSTOMER_CLASS_NAME(J)//
     +                                      ',V,'//trim(VAR_NUM_STR)//
     +                                                 ',,S,,,,,,,,""'
            ENDDO
            VAR_NUM_STR = LEFT_JUSTIFY_I2_IN_STR(J-1_2)
            WRITE(10,*) 'Wholesale Sales     '//
     +                                      ',V,'//trim(VAR_NUM_STR)//
     +                                                  ',,S,,,,,,,,""'
            CLOSE(10)
         ENDIF

!
         IF(IPL_FAC_RPT_NOT_OPEN) THEN
            IPL_FAC_RPT_NOT_OPEN = .FALSE.
            IPL_FAC_VAR = 15
            IPL_FAC_NO = IPL_FAC_HEADER(IPL_FAC_REC,IPL_FAC_VAR)
         ENDIF
         IF(R_HOUR == 1) THEN
!
            DAY = 1
            HOUR_IN_DAY = 1
            HOURLY_INTERRUPIBLE_REVENUE = 0.
!
            IF(ALLOCATED(HOURLY_CUST_MARGIN_DB))
     +                               DEALLOCATE(HOURLY_CUST_MARGIN_DB)
!
            ALLOCATE(HOURLY_CUST_MARGIN_DB(4,HOURLY_CUST_MARGIN_VAR))
!

!
            IF (ALLOCATED(LOAD_DISPATCH_BY_BLOCK))
     +                         DEALLOCATE (LOAD_DISPATCH_BY_BLOCK,
     +                                     LOAD_DISPATCH_COST,
     +                                     LOCAL_MUST_RUN_OPTIONS,
     +                                     LOAD_DISPATCH_COST_BY_UNIT,
     +                                     LOAD_DISPATCH_REV,
     +                                     LOAD_DISPATCH_REV_BY_UNIT,
     +                                     LOAD_DISPATCH_BY_UNIT)
            ALLOCATE(LOAD_DISPATCH_BY_BLOCK(
     +                     (R_TOTAL_DISPATCH_BLOCKS+2),LOAD_BLOCK_VAR))
            ALLOCATE(LOAD_DISPATCH_COST(
     +                     (R_TOTAL_DISPATCH_BLOCKS+2),LOAD_BLOCK_VAR))
            ALLOCATE(LOCAL_MUST_RUN_OPTIONS(R_TOTAL_DISPATCH_BLOCKS))
            ALLOCATE(LOAD_DISPATCH_COST_BY_UNIT(
     +                                        NUNITS+2,LOAD_BLOCK_VAR))
            ALLOCATE(LOAD_DISPATCH_REV(
     +                     (R_TOTAL_DISPATCH_BLOCKS+2),LOAD_BLOCK_VAR))
           ALLOCATE(LOAD_DISPATCH_REV_BY_UNIT(NUNITS+2,LOAD_BLOCK_VAR))
            ALLOCATE(LOAD_DISPATCH_BY_UNIT(NUNITS+2,LOAD_BLOCK_VAR))
!
!
            LOAD_DISPATCH_BY_BLOCK = 0.
            LOAD_DISPATCH_COST = 0.
            LOAD_DISPATCH_REV = 0.
            LOAD_DISPATCH_COST_BY_UNIT = 0.
            LOAD_DISPATCH_REV_BY_UNIT = 0.
            LOAD_DISPATCH_BY_UNIT = 0.
         ENDIF
!
         HOURLY_CUST_MARGIN_DB = 0.
!
         ALLOCATE_BLOCKS_2_CUSTOMERS = .TRUE.
!
         REMAINING_UNSERVED = R_UNSERVED_ENERGY
!
         NET_LOAD_ADJUSTMENTS = TABLE_HOURLY_LOAD(R_HOUR,0) -
     +                                                   R_THERMAL_LOAD
!
! 11/25/03.
! MUST-RUN OPTIONS DEFINES THE DISPATCH ORDER.  NEED A NEW DISPATCH
! ORDER
! FOR THE IPL FAC.
!
!
         IF(.NOT. RESOURCE_TO_LOAD_ALLOC) THEN
            K = 0
            DO  J = 1, 2
               DO L = 1, R_HOURLY_DISPATCH_BLOCKS
                  I = R_MUST_RUN_OPTIONS(L)
                  B = MAX(1,R_BLOCK_FOR_OUTAGE_BLOCK(I))
!
!
                  IF(B /= J) CYCLE
!
                  IF(B == 1) THEN
                     IPL_FAC_DB(4) = IPL_FAC_DB(4) +
     +                                       R_GENERATION_BY_SEGMENT(I)
                  ENDIF
!
                  K = K + 1
                  LOCAL_MUST_RUN_OPTIONS(K) = I
               ENDDO
            ENDDO
         ELSE
            DO L = 1, R_HOURLY_DISPATCH_BLOCKS
               LOCAL_MUST_RUN_OPTIONS(L) = R_MUST_RUN_OPTIONS(L)
            ENDDO
         ENDIF
!
         J = 0
         LAST_BLOCK = 1
         TEMP_GEN = 999999999.
         DOWHILE(J < LOAD_BLOCK_VAR)
            J = J + 1
!
            IF(J < LOAD_BLOCK_VAR) THEN
               K = LOAD_DISPATCH_INDEX(J,R_TG)
               IF(K == 0) CYCLE
! 02/23/04.
               IF( .NOT. IN_ACTIVE_LOADS_MARKET_AREA(
     +                               BASECASE_MKT_AREA_ID_tfo(K))) CYCLE

            X = LOAD_DISPATCH_POSITION(K,R_TG) ! 02/02/04. CHANGED INDEX

               TEMP_LOAD = TABLE_HOURLY_LOAD(R_HOUR,K)
               AVE_REV = TABLE_ENERGY_PRICE(K)
            ELSE
               IF(R_WHOLESALE_SALES < .001) CYCLE
               K = LOAD_BLOCK_VAR
               TEMP_LOAD = R_WHOLESALE_SALES
               AVE_REV = R_WHOLESALE_PRICE
            ENDIF
!
            IF(NET_LOAD_ADJUSTMENTS > 0.) THEN
               B = R_TOTAL_DISPATCH_BLOCKS+2
               U = NUNITS+2
               IF(TEMP_LOAD > NET_LOAD_ADJUSTMENTS) THEN
                  LOAD_DISPATCH_BY_BLOCK(B,K) =
     +               LOAD_DISPATCH_BY_BLOCK(B,K) + NET_LOAD_ADJUSTMENTS
                  LOAD_DISPATCH_BY_UNIT(U,K) =
     +                LOAD_DISPATCH_BY_UNIT(U,K) + NET_LOAD_ADJUSTMENTS
                  HOURLY_CUST_MARGIN_DB(1,K) =
     +                HOURLY_CUST_MARGIN_DB(1,K) + NET_LOAD_ADJUSTMENTS
                  ANN_LOAD_DISPATCH_BY_UNIT(U,K,R_TG) =
     +                 ANN_LOAD_DISPATCH_BY_UNIT(U,K,R_TG) +
     +                                             NET_LOAD_ADJUSTMENTS
                  TEMP_LOAD = TEMP_LOAD - NET_LOAD_ADJUSTMENTS
                  NET_LOAD_ADJUSTMENTS = 0.
! GO TO THERMAL UNITS
               ELSE
                  LOAD_DISPATCH_BY_BLOCK(B,K) =
     +                LOAD_DISPATCH_BY_BLOCK(B,K) + TEMP_LOAD
                  LOAD_DISPATCH_BY_UNIT(U,K) =
     +                 LOAD_DISPATCH_BY_UNIT(U,K) + TEMP_LOAD
                  HOURLY_CUST_MARGIN_DB(1,K) =
     +                  HOURLY_CUST_MARGIN_DB(1,K) + TEMP_LOAD
                  ANN_LOAD_DISPATCH_BY_UNIT(U,K,R_TG) =
     +                 ANN_LOAD_DISPATCH_BY_UNIT(U,K,R_TG) + TEMP_LOAD
                  NET_LOAD_ADJUSTMENTS =
     +                                 NET_LOAD_ADJUSTMENTS - TEMP_LOAD
                  TEMP_LOAD = 0.
                  CYCLE ! GET THE NEXT LOAD
               ENDIF
            ENDIF
! 083106. NEW TEST CONDITION.
            IF(K > LOAD_BLOCK_VAR) THEN
               WRITE(4,*) "LIMIT EXCEEDED IN RESOURCE LOAD ALLOCATION"
               er_message='Stop requested from tf_objt SIID284'
               call end_program(er_message)
            ENDIF
!
!
            DO L = LAST_BLOCK, R_HOURLY_DISPATCH_BLOCKS
!
! 11/25/03.
!
               I = LOCAL_MUST_RUN_OPTIONS(L)
!               I = R_MUST_RUN_OPTIONS(L)
!
               U = R_UNIT_FOR_OUTAGE_BLOCK(I)
               B = MAX(1,R_BLOCK_FOR_OUTAGE_BLOCK(I))
! 02/02/04 TO TRACK UNITS BY CLASS
!
               IF(I == 0 .OR. U == 0) THEN
                  CYCLE
               ENDIF
               IF(U > NUNITS+2 .OR. U < 0 .OR.
     +              I > R_TOTAL_DISPATCH_BLOCKS+2 .OR. I < 0 .OR.
     +                     K-1 > MAX_TRANS_LOAD_TABLES .OR. K < 0) THEN
                  er_message='Stop requested from tf_objt SIID285'
                  call end_program(er_message)
               ENDIF
               cl_name = CLA_RETURN_UNITNM(U)
!
               TEMP_GEN = MIN(TEMP_GEN,R_GENERATION_BY_SEGMENT(I))
               IF(TEMP_GEN <= .001) THEN
                  TEMP_GEN = 999999999.
                  CYCLE
               ENDIF
!
               IF(RESOURCE_TO_LOAD_ALLOC) THEN
                  TEMP_COST = R_DISPATCH_COST_FOR_BLOCK(I)
               ELSE
                  TEMP_COST = R_INCREMENTAL_FUEL_COST(I)
               ENDIF
!
!
!               IF(TEMP_LOAD < 0.0 .OR. TEMP_GEN < 0.0) THEN
!                  TEMP_LOAD = TEMP_LOAD
!               ENDIF
!
               IF(TEMP_LOAD > TEMP_GEN) THEN
! ASSIGN ALL OF BLOCK TO THE LOAD, RECORD THE COST, REDUCE TEMP LOAD,
! CYCLE FOR NEXT GEN
                  R_RETAIL_REVENUE_FOR_BLOCK(I) =
     +                        R_RETAIL_REVENUE_FOR_BLOCK(I) +
     +                                    TEMP_GEN*AVE_REV
!
                  LOAD_DISPATCH_BY_BLOCK(I,K) =
     +                           LOAD_DISPATCH_BY_BLOCK(I,K) + TEMP_GEN
                  LOAD_DISPATCH_BY_UNIT(U,K) =
     +                            LOAD_DISPATCH_BY_UNIT(U,K) + TEMP_GEN
                  HOURLY_CUST_MARGIN_DB(1,K) =
     +                            HOURLY_CUST_MARGIN_DB(1,K) + TEMP_GEN
                  HOURLY_CUST_MARGIN_DB(2,K) =
     +                            HOURLY_CUST_MARGIN_DB(2,K) +
     +                                     TEMP_GEN*TEMP_COST
                  HOURLY_CUST_MARGIN_DB(3,K) =
     +                            HOURLY_CUST_MARGIN_DB(3,K) +
     +                                   TEMP_GEN*AVE_REV
                  LOAD_DISPATCH_COST(I,K) =
     +                        LOAD_DISPATCH_COST(I,K) +
     +                             TEMP_GEN*TEMP_COST
                  LOAD_DISPATCH_COST_BY_UNIT(U,K) =
     +                        LOAD_DISPATCH_COST_BY_UNIT(U,K) +
     +                             TEMP_GEN*TEMP_COST
                  LOAD_DISPATCH_REV(I,K) =
     +                        LOAD_DISPATCH_REV(I,K) +
     +                             TEMP_GEN*AVE_REV
                  LOAD_DISPATCH_REV_BY_UNIT(U,K) =
     +                        LOAD_DISPATCH_REV_BY_UNIT(U,K) +
     +                             TEMP_GEN*AVE_REV
!
                  ANN_LOAD_DISPATCH_BY_UNIT(U,K,R_TG) =
     +                   ANN_LOAD_DISPATCH_BY_UNIT(U,K,R_TG) + TEMP_GEN
                  ANN_LOAD_DISPATCH_COST_BY_UNIT(U,K,R_TG) =
     +                     ANN_LOAD_DISPATCH_COST_BY_UNIT(U,K,R_TG) +
     +                             TEMP_GEN*TEMP_COST
                  ANN_LOAD_DISPATCH_REV_BY_UNIT(U,K,R_TG) =
     +                       ANN_LOAD_DISPATCH_REV_BY_UNIT(U,K,R_TG) +
     +                             TEMP_GEN*AVE_REV

                  IF(JURISDICTIONAL_CUSTOMER(J) == 'T') THEN
                     IPL_FAC_DB(8) = IPL_FAC_DB(8) +
     +                             TEMP_GEN*TEMP_COST
                  ENDIF
!                  ENDIF
!
                  TEMP_LOAD = TEMP_LOAD - TEMP_GEN
                  TEMP_GEN = 999999999.
                  CYCLE
               ELSE
! ASSIGN ALL OF LOAD TO THE BLOCK, RECORD THE COST, REDUCE TEMP GEN,
!EXIT FOR NEXT LOAD
                  R_RETAIL_REVENUE_FOR_BLOCK(I) =
     +                        R_RETAIL_REVENUE_FOR_BLOCK(I) +
     +                                   TEMP_LOAD*AVE_REV
!
                  LOAD_DISPATCH_BY_BLOCK(I,K) =
     +                          LOAD_DISPATCH_BY_BLOCK(I,K) + TEMP_LOAD
                  LOAD_DISPATCH_BY_UNIT(U,K) =
     +                           LOAD_DISPATCH_BY_UNIT(U,K) + TEMP_LOAD
                  HOURLY_CUST_MARGIN_DB(1,K) =
     +                           HOURLY_CUST_MARGIN_DB(1,K) + TEMP_LOAD
                  HOURLY_CUST_MARGIN_DB(2,K) =
     +                            HOURLY_CUST_MARGIN_DB(2,K) +
     +                                     TEMP_LOAD*TEMP_COST
                  HOURLY_CUST_MARGIN_DB(3,K) =
     +                            HOURLY_CUST_MARGIN_DB(3,K) +
     +                                   TEMP_LOAD*AVE_REV
                  LOAD_DISPATCH_COST(I,K) =
     +                        LOAD_DISPATCH_COST(I,K) +
     +                            TEMP_LOAD*TEMP_COST
!
                  LOAD_DISPATCH_COST_BY_UNIT(U,K) =
     +                        LOAD_DISPATCH_COST_BY_UNIT(U,K) +
     +                            TEMP_LOAD*TEMP_COST
                  LOAD_DISPATCH_REV(I,K) =
     +                        LOAD_DISPATCH_REV(I,K) +
     +                                   TEMP_LOAD*AVE_REV
                  LOAD_DISPATCH_REV_BY_UNIT(U,K) =
     +                        LOAD_DISPATCH_REV_BY_UNIT(U,K) +
     +                                   TEMP_LOAD*AVE_REV
!
                  ANN_LOAD_DISPATCH_BY_UNIT(U,K,R_TG) =
     +                  ANN_LOAD_DISPATCH_BY_UNIT(U,K,R_TG) + TEMP_LOAD
                  ANN_LOAD_DISPATCH_COST_BY_UNIT(U,K,R_TG) =
     +                       ANN_LOAD_DISPATCH_COST_BY_UNIT(U,K,R_TG) +
     +                            TEMP_LOAD*TEMP_COST
                  ANN_LOAD_DISPATCH_REV_BY_UNIT(U,K,R_TG) =
     +                        ANN_LOAD_DISPATCH_REV_BY_UNIT(U,K,R_TG) +
     +                                   TEMP_LOAD*AVE_REV
!
!                  IF(B == 1 .AND. X == 1) THEN
!                     IPL_FAC_DB(6) = IPL_FAC_DB(6) +
!     +                            TEMP_LOAD*TEMP_COST
!                  ELSEIF(X == 2) THEN
                  IF(JURISDICTIONAL_CUSTOMER(J) == 'T') THEN
                     IPL_FAC_DB(8) = IPL_FAC_DB(8) +
     +                            TEMP_LOAD*TEMP_COST
                  ENDIF
!                  ENDIF
!
                  TEMP_GEN = TEMP_GEN - TEMP_LOAD
                  LAST_BLOCK = L
                  EXIT
               ENDIF
!
            ENDDO ! THERMAL UNIT BLOCKS
!
! WHOLESALE PURCHASE PORTION
! 02/11/04. UNSERVED INCORPORATED
!
!
            IF(L > R_HOURLY_DISPATCH_BLOCKS) THEN
               TEMP_GEN = MIN(TEMP_GEN,R_WHOLESALE_PURCHASE)
!
               IF(REMAINING_UNSERVED > 0.1)THEN
                  IF(TEMP_GEN - REMAINING_UNSERVED > TEMP_LOAD) THEN
                     LOCAL_AVERAGE_PRICE = R_MARKET_PRICE
                  ELSEIF(TEMP_LOAD > REMAINING_UNSERVED) THEN

                     LOCAL_AVERAGE_PRICE =
     +                 ( REMAINING_UNSERVED*R_UNSERVED_COST +
     +                (TEMP_LOAD-REMAINING_UNSERVED)* R_MARKET_PRICE )/
     +                                                        TEMP_LOAD
                     REMAINING_UNSERVED = 0.
                  ELSE
                     LOCAL_AVERAGE_PRICE = R_UNSERVED_COST
                    REMAINING_UNSERVED = REMAINING_UNSERVED - TEMP_LOAD
                  ENDIF
               ELSE
                  LOCAL_AVERAGE_PRICE = R_MARKET_PRICE
               ENDIF
!
!
! ASSIGN ALL OF LOAD TO THE BLOCK, RECORD THE COST, REDUCE TEMP GEN,
! EXIT FOR NEXT LOAD
!
               B = R_TOTAL_DISPATCH_BLOCKS+1
!
               L = R_HOURLY_DISPATCH_BLOCKS + 1
               LOAD_DISPATCH_BY_BLOCK(B,K) =
     +                          LOAD_DISPATCH_BY_BLOCK(B,K) + TEMP_LOAD
               LOAD_DISPATCH_BY_UNIT(NUNITS+1,K) =
     +                  LOAD_DISPATCH_BY_UNIT(NUNITS+1,K) +
     +                                                        TEMP_LOAD
              HOURLY_CUST_MARGIN_DB(1,K) = HOURLY_CUST_MARGIN_DB(1,K) +
     +                                                        TEMP_LOAD
              HOURLY_CUST_MARGIN_DB(2,K) = HOURLY_CUST_MARGIN_DB(2,K) +
     +                                    TEMP_LOAD*LOCAL_AVERAGE_PRICE
              HOURLY_CUST_MARGIN_DB(3,K) = HOURLY_CUST_MARGIN_DB(3,K) +
     +                                   TEMP_LOAD*AVE_REV
               LOAD_DISPATCH_COST(B,K) =
     +              LOAD_DISPATCH_COST(B,K) +
     +                            TEMP_LOAD*LOCAL_AVERAGE_PRICE
               LOAD_DISPATCH_COST_BY_UNIT(NUNITS+1,K) =
     +              LOAD_DISPATCH_COST_BY_UNIT(NUNITS+1,K) +
     +                            TEMP_LOAD*LOCAL_AVERAGE_PRICE
               LOAD_DISPATCH_REV(B,K) =
     +              LOAD_DISPATCH_REV(B,K) +
     +                                   TEMP_LOAD*AVE_REV
               LOAD_DISPATCH_REV_BY_UNIT(NUNITS+1,K) =
     +              LOAD_DISPATCH_REV_BY_UNIT(NUNITS+1,K) +
     +                                   TEMP_LOAD*AVE_REV
!
               ANN_LOAD_DISPATCH_BY_UNIT(NUNITS+1,K,R_TG) =
     +                  ANN_LOAD_DISPATCH_BY_UNIT(NUNITS+1,K,R_TG) +
     +                                                        TEMP_LOAD
               ANN_LOAD_DISPATCH_COST_BY_UNIT(NUNITS+1,K,R_TG) =
     +              ANN_LOAD_DISPATCH_COST_BY_UNIT(NUNITS+1,K,R_TG) +
     +                            TEMP_LOAD*LOCAL_AVERAGE_PRICE
               ANN_LOAD_DISPATCH_REV_BY_UNIT(NUNITS+1,K,R_TG) =
     +              ANN_LOAD_DISPATCH_REV_BY_UNIT(NUNITS+1,K,R_TG) +
     +                                   TEMP_LOAD*AVE_REV

               IF(JURISDICTIONAL_CUSTOMER(J) == 'T') THEN
                  IPL_FAC_DB(9) = IPL_FAC_DB(9) +
     +                                    TEMP_LOAD*LOCAL_AVERAGE_PRICE
               ENDIF
!               ENDIF
!
               TEMP_GEN = TEMP_GEN - TEMP_LOAD
               LAST_BLOCK = L
            ENDIF
!
         ENDDO ! CUSTOMER PRIORITY
!
! 02/25/04.
!
         IF(YES_HOURLY_CUST_MARGIN_REPORT) THEN
            WRITE(HOURLY_CUST_MARGIN_NO,REC=HOURLY_CUST_MARGIN_REC)
     +                  PRT_ENDPOINT(),
     +                  FLOAT(LOCAL_YEAR),
     +                  CL_MONTH_NAME(R_MONTH),
     +                  FLOAT(R_HOUR),
     +                  'Energy   ',
     +                  (HOURLY_CUST_MARGIN_DB(1,K),
     +                                      K=1,HOURLY_CUST_MARGIN_VAR)
            HOURLY_CUST_MARGIN_REC = HOURLY_CUST_MARGIN_REC + 1
!
            WRITE(HOURLY_CUST_MARGIN_NO,REC=HOURLY_CUST_MARGIN_REC)
     +                  PRT_ENDPOINT(),
     +                  FLOAT(LOCAL_YEAR),
     +                  CL_MONTH_NAME(R_MONTH),
     +                  FLOAT(R_HOUR),
     +                  'Cost     ',
     +                  (HOURLY_CUST_MARGIN_DB(2,K),
     +                                      K=1,HOURLY_CUST_MARGIN_VAR)
            HOURLY_CUST_MARGIN_REC = HOURLY_CUST_MARGIN_REC + 1
!
            WRITE(HOURLY_CUST_MARGIN_NO,REC=HOURLY_CUST_MARGIN_REC)
     +                  PRT_ENDPOINT(),
     +                  FLOAT(LOCAL_YEAR),
     +                  CL_MONTH_NAME(R_MONTH),
     +                  FLOAT(R_HOUR),
     +                  'Revenue  ',
     +                  (HOURLY_CUST_MARGIN_DB(3,K),
     +                                      K=1,HOURLY_CUST_MARGIN_VAR)
            HOURLY_CUST_MARGIN_REC = HOURLY_CUST_MARGIN_REC + 1
!
            DO K = 1, HOURLY_CUST_MARGIN_VAR
               IF(HOURLY_CUST_MARGIN_DB(1,K) < .01) CYCLE
               HOURLY_CUST_MARGIN_DB(4,K) =
     +                (HOURLY_CUST_MARGIN_DB(3,K) -
     +                                 HOURLY_CUST_MARGIN_DB(2,K))/
     +                                       HOURLY_CUST_MARGIN_DB(1,K)
            ENDDO
!
            WRITE(HOURLY_CUST_MARGIN_NO,REC=HOURLY_CUST_MARGIN_REC)
     +                  PRT_ENDPOINT(),
     +                  FLOAT(LOCAL_YEAR),
     +                  CL_MONTH_NAME(R_MONTH),
     +                  FLOAT(R_HOUR),
     +                  'Margin   ',
     +                  (HOURLY_CUST_MARGIN_DB(4,K),
     +                                      K=1,HOURLY_CUST_MARGIN_VAR)
            HOURLY_CUST_MARGIN_REC = HOURLY_CUST_MARGIN_REC + 1
         ENDIF
!
! 02/24/05. TVA INTERRUPTIBLE LOGIC
!
      IF(INTERRUPTIBLE_PRICING) THEN
         REMAIN = MOD(FLOAT(R_HOUR),24.)
!
!         LAST_BLOCK = 1
!         TEMP_GEN = 999999999.
!
! SET-UP ARRAYS FOR INTERVAL REVENUE CALCULATION
! MOVES BACKWARDS FROM THE LAST RESOURCE SERVING NATIVE LOAD.
!
         IF(ALLOCATED(CUM_BLOCK_GENERATION))
     +                                 DEALLOCATE(CUM_BLOCK_GENERATION)
         ALLOCATE(CUM_BLOCK_GENERATION(LAST_BLOCK,3))
         CUM_BLOCK_GENERATION = 0.
!
! NOTE: RELIES ON LAST_BLOCK AND TEMP_GEN FROM ABOVE
!
         DO L = LAST_BLOCK, 1, -1
            IF(L > R_HOURLY_DISPATCH_BLOCKS) THEN
               CUM_BLOCK_GENERATION(L,1) = R_WHOLESALE_PURCHASE
! NEED TO DO UNSERVED CALC
               CUM_BLOCK_GENERATION(L,2) = R_MARKET_PRICE
               CUM_BLOCK_GENERATION(L,3) = R_WHOLESALE_PURCHASE
            ELSE
               I = LOCAL_MUST_RUN_OPTIONS(L)
               U = R_UNIT_FOR_OUTAGE_BLOCK(I)
               B = MAX(1,R_BLOCK_FOR_OUTAGE_BLOCK(I))
! 02/02/04 TO TRACK UNITS BY CLASS
               cl_name = CLA_RETURN_UNITNM(U)
!
               IF(I == 0 .OR. U == 0) THEN
                  CYCLE
               ENDIF
!
!               TEMP_R = MIN(TEMP_R,R_GENERATION_BY_SEGMENT(I))
               TEMP_R = R_GENERATION_BY_SEGMENT(I)
!               IF(TEMP_R <= .001) THEN
!                  TEMP_R = 999999999.
!                  CYCLE
!               ENDIF
               IF(RESOURCE_TO_LOAD_ALLOC) THEN
                  TEMP_COST = R_DISPATCH_COST_FOR_BLOCK(I)
               ELSE
                  TEMP_COST = R_INCREMENTAL_FUEL_COST(I)
               ENDIF
!
               IF(L == LAST_BLOCK) THEN
                  CUM_BLOCK_GENERATION(L,1) = TEMP_R
               ELSE
                  CUM_BLOCK_GENERATION(L,1) =
     +                             CUM_BLOCK_GENERATION(L+1,1) + TEMP_R
               ENDIF
               CUM_BLOCK_GENERATION(L,2) = TEMP_COST
               CUM_BLOCK_GENERATION(L,3) = TEMP_R
            ENDIF
         END DO
!
         J = 0
!
         DOWHILE(J < LOAD_BLOCK_VAR)
            J = J + 1
            IF(J < LOAD_BLOCK_VAR) THEN
               K = LOAD_DISPATCH_INDEX(J,R_TG)
!
! 03/15/05.  CHECK THIS.
!
               AC = ASSET_CLASS_ID(K)
               AC = ASSET_CLASS_GROUPS_INDEX(AC)
!
               IF(K == 0) CYCLE
! CACLULATE THE HOURLY INTERRUPTIBLE REVENUE
!
               IF(INTERRUPTIBLE_PRICING_ACTIVE(K) /= 'F') THEN
!
!                  TEMP2_R = HOURLY_CUST_MARGIN_DB(1,K) ! GENERATION
                  IF(INTERRUPTIBLE_PRICING_ACTIVE(K) == 'T') THEN
                     TEMP1_R = HOURLY_CUST_MARGIN_DB(2,K) ! COST
                     TEMP2_R = HOURLY_CUST_MARGIN_DB(1,K) ! GENERATION
! AVERAGE COST AFTER MARK ADDER
                  ELSEIF(INTERRUPTIBLE_PRICING_ACTIVE(K) == 'C') THEN
!

                     TEMP1_R = 0. ! FIND THE TOTAL COST OF SERVING THE
                     ! INTERVAL
!
! THIS MOVES THE ORIGIN BACK TO ZERO.  MUCH MORE CONVENIENT.
!
                     TEMP_R = MAX(0.,INTERRUPTIBLE_MAX_CAPACITY(K) -
     +                               INTERRUPTIBLE_MIN_CAPACITY(K))
                     TEMP2_R = TEMP_R
                     MIN_CAPACITY_BLOCK = LAST_BLOCK
                     MAX_CAPACITY_BLOCK = 1
                     DO L = LAST_BLOCK, 1, -1
                        IF(CUM_BLOCK_GENERATION(L,1) + .01 <
     +                             INTERRUPTIBLE_MIN_CAPACITY(K)) CYCLE
                        MIN_CAPACITY_BLOCK = L
                        EXIT
                     ENDDO
                     DO L = MIN_CAPACITY_BLOCK, 1, -1
                        IF(CUM_BLOCK_GENERATION(L,1) + .01 <
     +                             INTERRUPTIBLE_MAX_CAPACITY(K)) CYCLE
                        MAX_CAPACITY_BLOCK = L
                        EXIT
                     ENDDO
!
                     DO L = MIN_CAPACITY_BLOCK, MAX_CAPACITY_BLOCK, -1
                        IF(L == MIN_CAPACITY_BLOCK) THEN
                           IF(L < LAST_BLOCK) THEN
                              INTERVAL_CAPACITY =
     +                           INTERRUPTIBLE_MIN_CAPACITY(K) -
     +                                      CUM_BLOCK_GENERATION(L+1,1)
                           ELSE
                              INTERVAL_CAPACITY =
     +                              CUM_BLOCK_GENERATION(L,3) -
     +                                    INTERRUPTIBLE_MIN_CAPACITY(K)
                           ENDIF
                        ELSE
                          INTERVAL_CAPACITY = CUM_BLOCK_GENERATION(L,3)
                        ENDIF
                        TEMP1_R = TEMP1_R +
     +                      CUM_BLOCK_GENERATION(L,2) *
     +                             MIN(TEMP_R,INTERVAL_CAPACITY)
                        TEMP_R = TEMP_R -
     +                             MIN(TEMP_R,INTERVAL_CAPACITY)
                     ENDDO
                  ELSE
!                    NOT DEFINED
                  ENDIF
! AVERAGE COST AFTER ADDER
!
                  IF(TEMP2_R > .1) THEN
! AVERAGE COST OF THE CUSTOMER
                     TEMP_R = TEMP1_R/TEMP2_R
                  ELSE
                     TEMP_R = 0.
                  ENDIF
! 02/28/05.
                 HOURLY_INTERRUPIBLE_REVENUE(K,HOUR_IN_DAY,3) = TEMP_R
!
                  TEMP_R = TEMP_R + INTERRUPTIBLE_MARKUP_ADDER(K)
!
! AVERAGE COST AFTER ADDER, MARKUP WITH CAP
                  TEMP_R = TEMP_R +
     +                             MIN(.01 * TEMP_R *
     +                                  INTERRUPTIBLE_MARKUP_PERENT(K),
     +                                     INTERRUPTIBLE_MARKUP_CAP(K))
!
! TOTAL COST TO INTERRUPTIBLE CUSTOMER (REVENUE TO THE GENERATOR)
!
                  HOURLY_INTERRUPIBLE_REVENUE(K,HOUR_IN_DAY,1) =
     +                                                           TEMP_R
                  HOURLY_INTERRUPIBLE_REVENUE(K,HOUR_IN_DAY,2) =
     +                              TEMP_R * HOURLY_CUST_MARGIN_DB(1,K)
                  HOURLY_INTERRUPIBLE_REVENUE(K,HOUR_IN_DAY,4) =
     +                                       HOURLY_CUST_MARGIN_DB(1,K)
!
! TO PASS TO ASSET ANALYST. MAY WANT TO INDEX TO ASSET CLASS HERE?
!
                  MONTHLY_INTERRUPTIBLE_REVENUE(AC,R_MONTH) =
     +                   MONTHLY_INTERRUPTIBLE_REVENUE(AC,R_MONTH) +
     +                     HOURLY_INTERRUPIBLE_REVENUE(K,HOUR_IN_DAY,2)
                  MONTHLY_INTERRUPTIBLE_REVENUE(AC,0) =
     +                   MONTHLY_INTERRUPTIBLE_REVENUE(AC,0) +
     +                     HOURLY_INTERRUPIBLE_REVENUE(K,HOUR_IN_DAY,2)

                  IF(REMAIN < .001 .AND. YES_TRANS_INTR_REPORT) THEN
                     TEMP_CUSTOMER_NAME = CUSTOMER_CLASS_NAME(K)(1:24)
     +                              //' PRICE'
                     WRITE(TRANS_INTR_NO,REC=TRANS_INTR_REC)
     +                  PRT_ENDPOINT(),
     +                  FLOAT(LOCAL_YEAR),
     +                  CL_MONTH_NAME(R_MONTH),
     +                  FLOAT(DAY),
     +                  TEMP_CUSTOMER_NAME,
     +                  (HOURLY_INTERRUPIBLE_REVENUE(K,I,1),I=1,24)
                     TRANS_INTR_REC = TRANS_INTR_REC + 1
!
                     TEMP_CUSTOMER_NAME = CUSTOMER_CLASS_NAME(K)(1:24)
     +                              //' COST '
                     WRITE(TRANS_INTR_NO,REC=TRANS_INTR_REC)
     +                  PRT_ENDPOINT(),
     +                  FLOAT(LOCAL_YEAR),
     +                  CL_MONTH_NAME(R_MONTH),
     +                  FLOAT(DAY),
     +                  TEMP_CUSTOMER_NAME,
     +                  (HOURLY_INTERRUPIBLE_REVENUE(K,I,3),I=1,24)
                     TRANS_INTR_REC = TRANS_INTR_REC + 1
!
                     TEMP_CUSTOMER_NAME = CUSTOMER_CLASS_NAME(K)(1:24)
     +                              //' REVEN'
                     WRITE(TRANS_INTR_NO,REC=TRANS_INTR_REC)
     +                  PRT_ENDPOINT(),
     +                  FLOAT(LOCAL_YEAR),
     +                  CL_MONTH_NAME(R_MONTH),
     +                  FLOAT(DAY),
     +                  TEMP_CUSTOMER_NAME,
     +                  (HOURLY_INTERRUPIBLE_REVENUE(K,I,2),I=1,24)
                     TRANS_INTR_REC = TRANS_INTR_REC + 1
!
                     TEMP_CUSTOMER_NAME = CUSTOMER_CLASS_NAME(K)(1:24)
     +                              //' QUANT'
                     WRITE(TRANS_INTR_NO,REC=TRANS_INTR_REC)
     +                  PRT_ENDPOINT(),
     +                  FLOAT(LOCAL_YEAR),
     +                  CL_MONTH_NAME(R_MONTH),
     +                  FLOAT(DAY),
     +                  TEMP_CUSTOMER_NAME,
     +                  (HOURLY_INTERRUPIBLE_REVENUE(K,I,4),I=1,24)
                     TRANS_INTR_REC = TRANS_INTR_REC + 1
                  ENDIF ! REPORT INTERRUPTIBLE REVENUE
               ENDIF ! CUSTOMER IS ON INTERRUPTIBLE CONTRACT
            ENDIF ! VALID CUSTOMER GROUP
         ENDDO
         IF(HOUR_IN_DAY < 24) THEN
            HOUR_IN_DAY = HOUR_IN_DAY + 1
         ELSE
            HOUR_IN_DAY = 1
            DAY = DAY + 1
            HOURLY_INTERRUPIBLE_REVENUE = 0.
         ENDIF

      ENDIF ! INTERRUPTIBLE REVENUE LOGIC
!
         IF(R_HOUR == R_LAST_HOUR) THEN
            IF(R_BY_UNIT) THEN
               DO I = 1, NUNITS + 2
                  IF(I <= NUNITS) THEN
                     cl_name=CLA_RETURN_UNITNM(I)
                     CL_BLOCK_NAME = CL_NAME
                  ELSEIF(I == NUNITS + 1) THEN
                     CL_BLOCK_NAME = 'Wholesale Purchases   '
                  ELSE ! NUNITS + 2
                     CL_BLOCK_NAME = 'Other Resources       '
                  ENDIF
!
                  WRITE(LOAD_BY_BLOCK_NO,REC=LOAD_BLOCK_REC)
     +                  PRT_ENDPOINT(),
     +                  FLOAT(LOCAL_YEAR),
     +                  CL_MONTH_NAME(R_MONTH),
     +                  CL_BLOCK_NAME,
     +                  'Energy   ',
     +                  (LOAD_DISPATCH_BY_UNIT(I,K),K=1,LOAD_BLOCK_VAR)
                  LOAD_BLOCK_REC = LOAD_BLOCK_REC + 1
!
                  WRITE(LOAD_BY_BLOCK_NO,REC=LOAD_BLOCK_REC)
     +                  PRT_ENDPOINT(),
     +                  FLOAT(LOCAL_YEAR),
     +                  CL_MONTH_NAME(R_MONTH),
     +                  CL_BLOCK_NAME,
     +                  'Cost     ',
     +                  (LOAD_DISPATCH_COST_BY_UNIT(I,K),
     +                                              K=1,LOAD_BLOCK_VAR)
                  LOAD_BLOCK_REC = LOAD_BLOCK_REC + 1
!
                  WRITE(LOAD_BY_BLOCK_NO,REC=LOAD_BLOCK_REC)
     +                  PRT_ENDPOINT(),
     +                  FLOAT(LOCAL_YEAR),
     +                  CL_MONTH_NAME(R_MONTH),
     +                  CL_BLOCK_NAME,
     +                  'Revenue  ',
     +                  (LOAD_DISPATCH_REV_BY_UNIT(I,K),
     +                                              K=1,LOAD_BLOCK_VAR)
                  LOAD_BLOCK_REC = LOAD_BLOCK_REC + 1
!
                  IF(R_MONTH == 12) THEN
                     WRITE(LOAD_BY_BLOCK_NO,REC=LOAD_BLOCK_REC)
     +                  PRT_ENDPOINT(),
     +                  FLOAT(LOCAL_YEAR),
     +                  CL_MONTH_NAME(13),
     +                  CL_BLOCK_NAME,
     +                  'Energy   ',
     +                  (ANN_LOAD_DISPATCH_BY_UNIT(I,K,R_TG),
     +                                              K=1,LOAD_BLOCK_VAR)
                     LOAD_BLOCK_REC = LOAD_BLOCK_REC + 1
!
                     WRITE(LOAD_BY_BLOCK_NO,REC=LOAD_BLOCK_REC)
     +                  PRT_ENDPOINT(),
     +                  FLOAT(LOCAL_YEAR),
     +                  CL_MONTH_NAME(13),
     +                  CL_BLOCK_NAME,
     +                  'Cost     ',
     +                  (ANN_LOAD_DISPATCH_COST_BY_UNIT(I,K,R_TG),
     +                                              K=1,LOAD_BLOCK_VAR)
                     LOAD_BLOCK_REC = LOAD_BLOCK_REC + 1
!
                     WRITE(LOAD_BY_BLOCK_NO,REC=LOAD_BLOCK_REC)
     +                  PRT_ENDPOINT(),
     +                  FLOAT(LOCAL_YEAR),
     +                  CL_MONTH_NAME(13),
     +                  CL_BLOCK_NAME,
     +                  'Revenue  ',
     +                  (ANN_LOAD_DISPATCH_REV_BY_UNIT(I,K,R_TG),
     +                                              K=1,LOAD_BLOCK_VAR)
                     LOAD_BLOCK_REC = LOAD_BLOCK_REC + 1
                  ENDIF
!
               ENDDO ! UNIT COUNTER
            ELSE
               DO L = 1, R_TOTAL_DISPATCH_BLOCKS + 2
                  IF(L <= R_TOTAL_DISPATCH_BLOCKS) THEN
                     I = R_SORTED_OPTIONS(L)
                     U = R_UNIT_FOR_OUTAGE_BLOCK(I)
                     B = R_BLOCK_FOR_OUTAGE_BLOCK(I)
                     CL_NAME = CLA_RETURN_UNITNM(U)
                     IF(B == 2) THEN
                        CL_BLOCK_NAME = CL_NAME//'02'
                     ELSE
                        CL_BLOCK_NAME = CL_NAME//'01'
                     ENDIF
                  ELSEIF(L == R_TOTAL_DISPATCH_BLOCKS + 1) THEN
                     CL_BLOCK_NAME = 'Wholesale Purchases   '
                     I = R_TOTAL_DISPATCH_BLOCKS + 1
                  ELSE ! NUNITS + 2
                     CL_BLOCK_NAME = 'Other Resources       '
                     I = R_TOTAL_DISPATCH_BLOCKS + 2
                  ENDIF
!
                  IF( I < 1 .OR. I > R_TOTAL_DISPATCH_BLOCKS+2) THEN
!
                     WRITE(4,*) "PROBLEM WITH UNIT BLOCK COUNTER"
                  ELSEIF(LOAD_BLOCK_VAR < 0 .OR. LOAD_BLOCK_VAR >
     +                                         TG_COUNTER(R_TG)+1) THEN
                     WRITE(4,*) "PROBLEM WITH CUSTOMER BLOCK COUNTER"
                  ELSE
!
                     WRITE(LOAD_BY_BLOCK_NO,REC=LOAD_BLOCK_REC)
     +                  PRT_ENDPOINT(),
     +                  FLOAT(LOCAL_YEAR),
     +                  CL_MONTH_NAME(R_MONTH),
     +                  CL_BLOCK_NAME,
     +                  'Energy   ',
     +                 (LOAD_DISPATCH_BY_BLOCK(I,K),K=1,LOAD_BLOCK_VAR)
                     LOAD_BLOCK_REC = LOAD_BLOCK_REC + 1
!
                     WRITE(LOAD_BY_BLOCK_NO,REC=LOAD_BLOCK_REC)
     +                  PRT_ENDPOINT(),
     +                  FLOAT(LOCAL_YEAR),
     +                  CL_MONTH_NAME(R_MONTH),
     +                  CL_BLOCK_NAME,
     +                  'Cost     ',
     +                  (LOAD_DISPATCH_COST(I,K),K=1,LOAD_BLOCK_VAR)
                     LOAD_BLOCK_REC = LOAD_BLOCK_REC + 1
!
                     WRITE(LOAD_BY_BLOCK_NO,REC=LOAD_BLOCK_REC)
     +                  PRT_ENDPOINT(),
     +                  FLOAT(LOCAL_YEAR),
     +                  CL_MONTH_NAME(R_MONTH),
     +                  CL_BLOCK_NAME,
     +                  'Revenue  ',
     +                  (LOAD_DISPATCH_REV(I,K),K=1,LOAD_BLOCK_VAR)
                     LOAD_BLOCK_REC = LOAD_BLOCK_REC + 1
!
                  ENDIF
               ENDDO ! BLOCK COUNTER
            ENDIF ! BY UNIT OR BLOCK
            IF(IPL_FAC_REPORT) THEN
!
               ANN_IPL_FAC_DB(4) = ANN_IPL_FAC_DB(4) + IPL_FAC_DB(4)
               ANN_IPL_FAC_DB(6) = ANN_IPL_FAC_DB(6) + IPL_FAC_DB(6)
               ANN_IPL_FAC_DB(7) = ANN_IPL_FAC_DB(7) + IPL_FAC_DB(7)
               ANN_IPL_FAC_DB(8) = ANN_IPL_FAC_DB(8) + IPL_FAC_DB(8)
               ANN_IPL_FAC_DB(9) = ANN_IPL_FAC_DB(9) + IPL_FAC_DB(9)
!
!
               IPL_FAC_DB(10) = IPL_FAC_DB(8) + IPL_FAC_DB(9)
               ANN_IPL_FAC_DB(10) = ANN_IPL_FAC_DB(10) + IPL_FAC_DB(10)
!
               IPL_FAC_DB(4) = IPL_FAC_DB(4)/R_LAST_HOUR
               IPL_FAC_DB(5) = IPL_FAC_DB(4) * IPL_FAC_DB(3)
               IF(IPL_FAC_DB(4) > .01) THEN
                  IPL_FAC_DB(7) = IPL_FAC_DB(6)/
     +                                      (R_LAST_HOUR*IPL_FAC_DB(4))
               ELSE
                  IPL_FAC_DB(7) = 0.
               ENDIF

               IF(IPL_FAC_DB(1) > .01) THEN
                  IPL_FAC_DB(11) = IPL_FAC_DB(10)/IPL_FAC_DB(1)
               ELSE
                  IPL_FAC_DB(11) = 0.
               ENDIF
!
               IPL_FAC_DB(12) = IPL_FAC_DB(5) *
     +                                      IPL_FAC_DB(7) * R_LAST_HOUR

               IF(IPL_FAC_DB(1) > 0.0001) THEN
                  IPL_FAC_DB(13) = IPL_FAC_DB(13) / IPL_FAC_DB(1)
               ELSE
                  IPL_FAC_DB(13) = 0.0
               ENDIF
!               IPL_FAC_DB(14) = IPL_FAC_DB(12) - IPL_FAC_DB(13)
               IPL_FAC_DB(14) = IPL_FAC_DB(11) - IPL_FAC_DB(13)
               IPL_FAC_DB(15) = IPL_FAC_DB(7) - IPL_FAC_DB(11)
!
! 03/05/04.
!
               ELECT_PLAN_DATABASE(R_MONTH,1:3) =
     +                        ELECT_PLAN_DATABASE(R_MONTH,1:3)
     +                        + IPL_FAC_DB(8:10)
!
               WRITE(IPL_FAC_NO,REC=IPL_FAC_REC)
     +                  PRT_ENDPOINT(),
     +                  FLOAT(LOCAL_YEAR),
     +                  CL_MONTH_NAME(R_MONTH),
     +                  'Variable ',
     +                  (IPL_FAC_DB(K),K=1,IPL_FAC_VAR)
               IPL_FAC_REC = IPL_FAC_REC + 1
               IF(R_MONTH == 12) THEN
!
                  IF(ANN_IPL_FAC_DB(1) + ANN_IPL_FAC_DB(2)> 0.01) THEN
                     ANN_IPL_FAC_DB(3) =
     +                        ANN_IPL_FAC_DB(2)/
     +                            (ANN_IPL_FAC_DB(1)+ANN_IPL_FAC_DB(2))
                  ELSE
                     ANN_IPL_FAC_DB(3) = 0.
                  ENDIF
!
                  ANN_IPL_FAC_DB(4) = ANN_IPL_FAC_DB(4)/8760.
                  ANN_IPL_FAC_DB(5) = ANN_IPL_FAC_DB(4) *
     +                                               ANN_IPL_FAC_DB(3)
                  IF(ANN_IPL_FAC_DB(4) > .01) THEN
                     ANN_IPL_FAC_DB(7) = ANN_IPL_FAC_DB(6)/
     +                                       (8760.*ANN_IPL_FAC_DB(4))
                  ELSE
                     ANN_IPL_FAC_DB(7) = 0.
                  ENDIF
! 010908. TRIGGERS OFF OF 1 INSTEAD OF 2.
!                  IF(ANN_IPL_FAC_DB(2) > .01) THEN
                  IF(ANN_IPL_FAC_DB(1) > .01) THEN
                     ANN_IPL_FAC_DB(11) = ANN_IPL_FAC_DB(10)/
     +                                                ANN_IPL_FAC_DB(1)
                  ELSE
                     ANN_IPL_FAC_DB(11) = 0.
                  ENDIF
                  ANN_IPL_FAC_DB(12) = ANN_IPL_FAC_DB(5) *
     +                                        ANN_IPL_FAC_DB(7) * 8760.

                  IF(ANN_IPL_FAC_DB(1) > 0.01) THEN
                     ANN_IPL_FAC_DB(13) = ANN_IPL_FAC_DB(13) /
     +                                                ANN_IPL_FAC_DB(1)
                  ELSE
                     ANN_IPL_FAC_DB(13) = 0.0
                  ENDIF
                  ANN_IPL_FAC_DB(14) = ANN_IPL_FAC_DB(11) -
     +                                               ANN_IPL_FAC_DB(13)
                  ANN_IPL_FAC_DB(15) = ANN_IPL_FAC_DB(7) -
     +                                               ANN_IPL_FAC_DB(11)
!
                  WRITE(IPL_FAC_NO,REC=IPL_FAC_REC)
     +                  PRT_ENDPOINT(),
     +                  FLOAT(LOCAL_YEAR),
     +                  CL_MONTH_NAME(13),
     +                  'Variable ',
     +                  (ANN_IPL_FAC_DB(K),K=1,IPL_FAC_VAR)
                  IPL_FAC_REC = IPL_FAC_REC + 1
               ENDIF
            ENDIF
         ENDIF ! HOUR = LAST_HOUR
!
      RETURN


      ENTRY GET_WH_LOADS_PER_HOUR(R_HOUR,R_TG)

         IF(ALLOCATED(WH_LOADS_PER_HOUR)) THEN
            IF(R_TG > 0 .AND. R_TG <= MAX_TRANS_LOAD_GROUPS) THEN
               GET_WH_LOADS_PER_HOUR = WH_LOADS_PER_HOUR(R_HOUR,R_TG)
            ELSE
               GET_WH_LOADS_PER_HOUR = 0.0
            ENDIF
         ELSE
            GET_WH_LOADS_PER_HOUR = 0.0
         ENDIF
      RETURN

      ENTRY GET_WH_MONTH_CAPACITY(R_MONTH,R_TG)

         IF(ALLOCATED(WH_TRANS_MONTHLY_CAPACITY)) THEN
            IF(R_TG > 0 .AND. R_TG <= MAX_TRANS_GROUPS_FROM_TG) THEN
                  GET_WH_MONTH_CAPACITY =
     +                          WH_TRANS_MONTHLY_CAPACITY(R_MONTH,R_TG)
            ELSE
               GET_WH_MONTH_CAPACITY = 0.0
            ENDIF
         ELSE
            GET_WH_MONTH_CAPACITY = 0.
         ENDIF
      RETURN

      ENTRY GET_WH_MONTH_ENERGY(R_MONTH,R_TG)

!         GET_WH_MONTH_ENERGY = WH_TRANS_MONTHLY_ENERGY(R_MONTH,R_TG)
         IF(ALLOCATED(WH_TRANS_MONTHLY_ENERGY)) THEN
            IF(R_TG > 0 .AND. R_TG <= MAX_TRANS_GROUPS) THEN
               GET_WH_MONTH_ENERGY =
     +                            WH_TRANS_MONTHLY_ENERGY(R_MONTH,R_TG)
            ELSE
               GET_WH_MONTH_ENERGY = 0.0
            ENDIF
         ELSE
            GET_WH_MONTH_ENERGY = 0.
         ENDIF
      RETURN


      ENTRY DEALLOCATE_ANNUAL_TRANS_LOADS


         DEALLOCATE_ANNUAL_TRANS_LOADS = .TRUE.
         IF(ALLOCATED(MONTHLY_AC_COST_AT_MARKET))
     +                   DEALLOCATE(MONTHLY_AC_COST_AT_MARKET,
     +                              MONTHLY_AC_CONTRACT_REVENUE)
!     +                              MONTHLY_INDEXED_ENERGY_REVENUE)

         IF(ALLOCATED(CUSTOMER_GROUP))
     +      DEALLOCATE(
     +            CUSTOMER_GROUP,
     +            TRANSACTION_GROUP,
     +            REFERENCE_LOAD_NUMBER,
     +            MARKET_ENERGY_PRICE,
     +            MONTHLY_ENERGY_PRICE_PATTERN,
     +            MARKET_DEMAND_PRICE,
     +            MONTHLY_DEMAND_PRICE_PATTERN,
     +            MARKET_CUSTOMER_PRICE,
     +            ASSET_CLASS_ID,
     +            ASSET_CLASS_REV_ALLOC_VECTOR,
     +            ANNUAL_ENERGY,
     +            ANNUAL_PEAK,
     +            ANNUAL_CUSTOMERS,
     +            ANNUAL_MULTIPLIER,
     +            MONTHLY_ENERGY,
     +            MONTHLY_PEAK,
     +            MONTHLY_CUSTOMERS,
     +            DIST_ENERGY_LOSS_FACTOR,
     +            TRANS_ENERGY_LOSS_FACTOR,
     +            PEAK_LOSS_FACTOR,
     +            PEAK_COIN_FACTOR,
     +            DISTRIBUTION_PRICE,
     +            TRANSMISSION_PRICE,
     +            CUSTOMER_CLASS_NAME,
     +            CALCULATION_MODE,
     +            REFERENCE_LOAD_NAME,
     +            TRANS_MONTHLY_ENERGY,
     +            WH_TRANS_MONTHLY_ENERGY,
     +            WH_TRANS_MONTHLY_CAPACITY,
     +            TRANS_MONTHLY_PEAK,
     +            TRANS_MONTHLY_CUSTOMERS,
     +            TABLE_ACTIVE,
     +            BASECASE_MKT_AREA_ID_tfo,
     +            WD_INDEX,
     +            BASECASE_TRANS_AREA_ID,
     +            BASECASE_NERC_SUB_ID,
     +            MONTHLY_UNITS,
     +            PRICE_INDEX_ACTIVE,
     +            THREE_FACTOR_TRANSFORM,
     +            JURISDICTIONAL_CUSTOMER,
     +            FUEL_COST_RECOVERY_THROUGH_FAC,
     +            BASE_COST_OF_FAC_FUEL,
     +            CUSTOMER_GROUP_2,
     +            INTERRUPTIBLE_PRICING_ACTIVE,
     +            INTERRUPTIBLE_MARKUP_PERENT,
     +            INTERRUPTIBLE_MARKUP_CAP,
     +            INTERRUPTIBLE_MAX_CAPACITY,
     +            INTERRUPTIBLE_MIN_CAPACITY,
     +            INTERRUPTIBLE_MARKUP_ADDER,
     +            CAPACITY_MARKET_MONTH,
     +            capacity_market_pointer_array_tfo,
     +            CAP_MARKET_MONTH_NO,
     +            CAP_MARKET_TYPE_INDEX,
     +            CAPACITY_MARKET_EXP_COLLECT,
     +            CAPACITY_MARKET_COST_ASSIGN,
     +            CAPACITY_MARKET_COIN_ADJ_FACT,
     +            SCENARIO_INDEX,
     +            HOURLY_INTERRUPIBLE_REVENUE,
     +            MINIMUM_MARKET_PRICE,
     +            MAXIMUM_MARKET_PRICE,
     +            INDEXED_ENERGY_PRICE,
     +            TG_COUNTER,
     +            LOAD_DISPATCH_POSITION,
     +            LOAD_DISPATCH_INDEX,
     +            TF_PLANNING_PEAK,
     +            PA_PLANNING_PEAK,
     +            CM_PLANNING_PEAK,
     +            GLOBAL_PA_PEAK,
     +            GLOBAL_CM_PEAK,
     +            GLOBAL_PA_PEAK_MONTH,
     +            GLOBAL_CM_PEAK_MONTH,
     +            TG_2_PLANNING_AREA,
     +            TG_2_CAPACITY_MARKET,
     +            ENERGY_LOSS_MULT,
     +            REF_LEAP_YEAR_DAY_SHIFT,
!
     +            TG_CG_DATABASE,
     +            TG_CG2_DATABASE,
     +            TRANS_LOAD_GROUPS_INDEX,
     +            CUST_CLASS_GROUPS_INDEX,
     +            CUST2_CLASS_GROUPS_INDEX,
     +            ASSET_CLASS_GROUPS_INDEX,
     +            ASSET_2_TRANS_INDEX,
     +            NUMBER_ASSET_2_TRANS,
     +            ASSET_TRANSACTION_CROSS_INDEX,
     +            NUMBER_TRANS_PER_AC_TG,
     +            TRANS_WITHIN_AC_TG,
     +            FIRST_AC_TG,
     +            FIRST_TABLE_TG,
     +            TRANS_LOAD_2_TRANS_GROUPS,
     +            TRANS_LOAD_GROUP_2_TG,
     +            CUST_CLASS_GROUP_2_CG,
     +            CUST2_CLASS_GROUP_2_CG,
     +            ASSET_CLASS_GROUP_2_AC,
     +            LAST_TABLE_FOR_TG,
     +            LAST_TABLE_FOR_CG,
     +            LAST_TABLE_FOR_CG2,
     +            TABLE_DAY_SHIFT,
     +            REV_CLASS_INDEX,
     +            DEMAND_PRICING_METHOD,
     +            INTRA_COMPANY_TRANSACTION,
     +            INTRA_ASSET_CLASS_ID,
     +            INTRA_ASSET_CLASS_ALLOCATION,
     +            INTRA_ACCOUNT_CLASSIFICATION,
     +            INTRA_EXPENSE_COLLECTION,
     +            MONTHLY_TABLE_PEAK_SALES)


      RETURN

      ENTRY DEALLOCATE_MONTHLY_TRANS_LOADS


         DEALLOCATE_MONTHLY_TRANS_LOADS = .TRUE.
         IF(ALLOCATED(DAY_OF_WEEK)) DEALLOCATE(DAY_OF_WEEK)
         IF(ALLOCATED(ANNUAL_CLASS_ENERGY_REVENUE))
     +          DEALLOCATE(ANNUAL_CLASS_ENERGY_REVENUE,
     +                     ANNUAL_TRANS_INDEXED_REVENUE,
     +                     ANNUAL_CLASS_PEAK_REVENUE,
     +                     ANNUAL_CLASS_CUSTOMER_REVENUE,
     +                     ANNUAL_CAP_CUSTOMER_EXPENSE,
     +                     ANNUAL_CLASS_ENERGY,
     +                     ANNUAL_CLASS_PEAK,
     +                     ANNUAL_CLASS_CUSTOMERS)
          IF(ALLOCATED(TRANS_HOURLY_LOAD))
     +          DEALLOCATE(TRANS_HOURLY_LOAD,
     +                     WH_LOADS_PER_HOUR,
     +                     HYDRO_HOURLY_LOAD,
     +                     TABLE_HOURLY_LOAD,
     +                     MONTHLY_TRANS_ENERGY,
     +                     MONTHLY_HYDRO_ENERGY,
     +                     MONTHLY_TRANS_PEAK,
     +                     MONTHLY_TRANS_BASE,
     +                     MONTHLY_HYDRO_PEAK,
     +                     MONTHLY_HYDRO_BASE,
     +                     MONTHLY_TABLE_ENERGY,
     +                     MONTHLY_TABLE_SALES_ENERGY,
     +                     MONTHLY_TABLE_PEAK,
     +                     TABLE_ENERGY_PRICE,
     +                     TABLE_ENERGY_REVENUE,
     +                     TABLE_DEMAND_REVENUE,
     +                     TABLE_CUSTOMER_REVENUE,
     +                     TRANS_ENERGY_REVENUE,
     +                     TRANS_DEMAND_REVENUE,
     +                     TRANS_CUSTOMER_REVENUE,
     +                     CLASS_ENERGY_REVENUE,
     +                     TRANS_INDEXED_REVENUE,
     +                     CLASS_PEAK_REVENUE,
     +                     CLASS_CUSTOMER_REVENUE,
     +                     CAP_CUSTOMER_EXPENSE,
     +                     TF_TG_CAP_MARKET_MW,
     +                     TF_TG_CAP_MARKET_COST,
     +                     MONTHLY_CLASS_ENERGY,
     +                     MONTHLY_CLASS_PEAK,
     +                     MONTHLY_CLASS_CUSTOMERS,
     +                     LOCAL_CUSTOMER_NAME,
     +                     ASSET_CLASS_HOURLY_LOAD)
      RETURN


      ENTRY GET_LAST_THIS_YR_ENERGY(R_TG)

         IF(R_TG < 0 .OR. R_TG > MAX_TRANS_GROUP_NUMBER .OR.

     +                       .NOT. ALLOCATED(LAST_THIS_YR_ENERGY)) THEN
            GET_LAST_THIS_YR_ENERGY = 0.0
         ELSE
            GET_LAST_THIS_YR_ENERGY =
     +                            LAST_THIS_YR_ENERGY(2,1,R_TG) +
     +                            LAST_THIS_YR_ENERGY(2,2,R_TG) +
     +                            LAST_THIS_YR_ENERGY(2,3,R_TG) +
     +                            LAST_THIS_YR_ENERGY(2,4,R_TG) +
     +                            LAST_THIS_YR_ENERGY(2,5,R_TG) +
     +                            LAST_THIS_YR_ENERGY(2,6,R_TG) +
     +                            LAST_THIS_YR_ENERGY(2,7,R_TG) +
     +                            LAST_THIS_YR_ENERGY(2,8,R_TG) +
     +                            LAST_THIS_YR_ENERGY(2,9,R_TG) +
     +                            LAST_THIS_YR_ENERGY(2,10,R_TG) +
     +                            LAST_THIS_YR_ENERGY(2,11,R_TG) +
     +                            LAST_THIS_YR_ENERGY(2,12,R_TG)
         ENDIF
      RETURN

      ENTRY GET_FILE_TABLE_DATA(R_INDEX,
     +                           R_13_ENERGY,
     +                           R_13_PEAK,
     +                           R_13_CUSTOMERS)

         IF(ALLOCATED(TRANS_LOAD_2_TRANS_GROUPS)) THEN
            IF(R_INDEX >= 0 .AND. R_INDEX < 16000) THEN
               TRANS = FILE_TABLE_2_TRANS_INDEX(R_INDEX)
!
               R_13_ENERGY(0) = 0.
               R_13_PEAK(0) = 0.
               R_13_CUSTOMERS(0) = 0.
               DO LOCAL_MONTH = 1, 12
                  R_13_ENERGY(LOCAL_MONTH) =
     +                         LAST_THIS_YR_ENERGY(2,LOCAL_MONTH,TRANS)
                  R_13_ENERGY(0) = R_13_ENERGY(0) +
     +                         LAST_THIS_YR_ENERGY(2,LOCAL_MONTH,TRANS)
                  R_13_PEAK(LOCAL_MONTH) =
     +                           LAST_THIS_YR_PEAK(2,LOCAL_MONTH,TRANS)
                  R_13_PEAK(0) = MAX(R_13_PEAK(0) ,
     +                          LAST_THIS_YR_PEAK(2,LOCAL_MONTH,TRANS))
                  R_13_CUSTOMERS(LOCAL_MONTH) =
     +                             MONTHLY_CUSTOMERS(LOCAL_MONTH,TRANS)
                  R_13_CUSTOMERS(0) = R_13_CUSTOMERS(0) +
     +                             MONTHLY_CUSTOMERS(LOCAL_MONTH,TRANS)
               ENDDO
!
               GET_FILE_TABLE_DATA = .TRUE.
            ELSE
               GET_FILE_TABLE_DATA = .FALSE.
            ENDIF
         ELSE
            GET_FILE_TABLE_DATA = .FALSE.
         ENDIF
      RETURN

      ENTRY GET_TG_CG_DATA(      R_TG,
     +                           R_CG,
     +                           R_13_ENERGY,
     +                           R_13_PEAK,
     +                           R_13_CUSTOMERS)

         GET_TG_CG_DATA = .FALSE.
         IF(ALLOCATED(TRANS_LOAD_2_TRANS_GROUPS)) THEN
            IF(R_TG > 0 .AND. R_TG <= MAX_TRANS_GROUP_NUMBER .AND.
     +               R_CG > 0 .AND. R_CG <= MAX_CUST_GROUP_NUMBER) THEN
! 042809. TG CG IS STILL UNIQUE
               TG = GET_BELONGS_TO_GROUP(R_TG)
!
!               IF(TG < 1 .OR. TG > 11) RETURN
               IF(TG < 1) RETURN
               TG = TRANS_LOAD_GROUPS_INDEX(TG)
!               TG = TRANS_LOAD_GROUPS_INDEX(R_TG)
!
               CG = CUST_CLASS_GROUPS_INDEX(R_CG)
!
               IF(TG > 0 .AND. CG > 0) THEN
                  R_13_ENERGY(1:12) = TG_CG_DATABASE(1:12,TG,CG,1)
                  R_13_ENERGY(0) = SUM(TG_CG_DATABASE(1:12,TG,CG,1))
                  R_13_PEAK(1:12) = TG_CG_DATABASE(1:12,TG,CG,2)
                  R_13_PEAK(0) = MAXVAL(R_13_PEAK(1:12))
                  R_13_CUSTOMERS(1:12) = TG_CG_DATABASE(1:12,TG,CG,3)
                  R_13_CUSTOMERS(0) = SUM(TG_CG_DATABASE(1:12,TG,CG,3))
               ELSE
!                  WRITE(4,*) "UNABLE TO FIND TABLES FOR RPS DATA"
!                  WRITE(4,*) "IN TRANSACT FORECAST FILES"
!                  WRITE(4,*) "TRANS GROUP = ",R_TG
!                  WRITE(4,*) "CUSTOMER GROUP = ",R_CG
                  R_13_ENERGY(1:12) = 0.0
                  R_13_ENERGY(0) = 0.0
                  R_13_PEAK(1:12) = 0.0
                  R_13_PEAK(0) = 0.0
                  R_13_CUSTOMERS(1:12) = 0.0
                  R_13_CUSTOMERS(0) = 0.0
               ENDIF
!
               GET_TG_CG_DATA = .TRUE.
            ENDIF
         ENDIF
      RETURN

      ENTRY GET_TG_CG2_DATA(     R_TG,
     +                           R_CG,
     +                           R_13_ENERGY,
     +                           R_13_PEAK,
     +                           R_13_CUSTOMERS)

         GET_TG_CG2_DATA = .FALSE.
         IF(ALLOCATED(TRANS_LOAD_2_TRANS_GROUPS)) THEN
            IF(R_TG > 0 .AND. R_TG <= MAX_TRANS_GROUP_NUMBER .AND.
     +              R_CG > 0 .AND. R_CG <= MAX_CUST2_GROUP_NUMBER) THEN
!
               TG = TRANS_LOAD_GROUPS_INDEX(R_TG)
!
               CG2 = CUST2_CLASS_GROUPS_INDEX(R_CG)
!

               R_13_ENERGY(1:12) = TG_CG2_DATABASE(1:12,TG,CG2,1)
               R_13_ENERGY(0) = SUM(TG_CG2_DATABASE(1:12,TG,CG2,1))
               R_13_PEAK(1:12) = TG_CG2_DATABASE(1:12,TG,CG2,2)
               R_13_PEAK(0) = MAXVAL(R_13_PEAK(1:12))
               R_13_CUSTOMERS(1:12) = TG_CG2_DATABASE(1:12,TG,CG2,3)
               R_13_CUSTOMERS(0) = SUM(TG_CG2_DATABASE(1:12,TG,CG2,3))
!
               GET_TG_CG2_DATA = .TRUE.
            ENDIF
         ENDIF
      RETURN

!
      ENTRY GET_TRANS_GROUP_PEAK(R_TG,R_MONTH)

!
         IF(ALLOCATED(TRANS_LOAD_2_TRANS_GROUPS)) THEN
            IF(R_TG > 0 .AND. R_TG <= MAX_TRANS_GROUPS) THEN
               TG = TRANS_LOAD_2_TRANS_GROUPS(R_TG)
               IF(TG > 0 .AND. TG <= MAX_TRANS_LOAD_GROUPS) THEN
                  GET_TRANS_GROUP_PEAK = TF_PLANNING_PEAK(TG,R_MONTH)
               ELSE
                  GET_TRANS_GROUP_PEAK = 0.
               ENDIF
            ELSEIF(R_TG == 0)THEN
               GET_TRANS_GROUP_PEAK = 0.
            ELSE
               GET_TRANS_GROUP_PEAK = 0.
!
               WRITE(4,*) "BAD MONTHLY LOAD VALUE FROM THE TRANSACTION"
               WRITE(4,*) "FORECAST FILE.  TRANSACTION GROUP = ",R_TG
               WRITE(4,*) "HOUR = ",R_MONTH
               WRITE(4,*) '*** line 3276 TF_OBJT.FOR ***'

            ENDIF
         ELSE
            GET_TRANS_GROUP_PEAK = 0.
         ENDIF
      RETURN

!
      ENTRY GET_CUST_GROUP_PEAK(R_TG,R_MONTH)

!
         IF(R_TG > 0 .AND. R_TG <= MAX_CLASS_GROUPS) THEN
            CG = CUST_CLASS_GROUPS_INDEX(R_TG)
            GET_CUST_GROUP_PEAK = MONTHLY_CLASS_PEAK(CG)
         ELSEIF(R_TG == 0)THEN
            GET_CUST_GROUP_PEAK = 0.
         ELSE
            GET_CUST_GROUP_PEAK = 0.
            WRITE(4,*) "BAD MONTHLY LOAD VALUE FROM THE TRANSACTION"
            WRITE(4,*) "FORECAST FILE.  CUSTOMER GROUP = ",R_TG
            WRITE(4,*) "MONTH = ",R_MONTH
            WRITE(4,*) '*** line 3295 TF_OBJT.FOR ***'

         ENDIF
      RETURN

!
      ENTRY GET_CUST_GROUP_ENERGY(R_TG,R_MONTH)

!
         IF(R_TG > 0 .AND. R_TG <= MAX_CLASS_GROUPS) THEN
            CG = CUST_CLASS_GROUPS_INDEX(R_TG)
            GET_CUST_GROUP_ENERGY = MONTHLY_CLASS_ENERGY(CG)
         ELSEIF(R_TG == 0)THEN
            GET_CUST_GROUP_ENERGY = 0.
         ELSE
            GET_CUST_GROUP_ENERGY = 0.
            WRITE(4,*) "BAD MONTHLY LOAD VALUE FROM THE TRANSACTION"
            WRITE(4,*) "FORECAST FILE.  CUSTOMER GROUP = ",R_TG
            WRITE(4,*) "MONTH = ",R_MONTH
            WRITE(4,*) '*** line 3314 TF_OBJT.FOR ***'

         ENDIF
      RETURN

!
      ENTRY GET_GROUP_PEAK_ON_PEAK_MONTH(R_TG)

!
         IF(ALLOCATED(TF_PLANNING_PEAK)) THEN
            IF(R_TG > 0  .AND. R_TG <= MAX_TRANS_GROUPS) THEN
               TG = TRANS_LOAD_2_TRANS_GROUPS(R_TG)
               IF(TG > 0 .AND. TG <= MAX_TRANS_LOAD_GROUPS) THEN
                  GET_GROUP_PEAK_ON_PEAK_MONTH =
     +                           TF_PLANNING_PEAK(TG,GLOBAL_PEAK_MONTH)

               ELSE
                  GET_GROUP_PEAK_ON_PEAK_MONTH = 0.
               ENDIF
            ELSEIF(R_TG == 0) THEN
               GET_GROUP_PEAK_ON_PEAK_MONTH =
     +                         TF_PLANNING_PEAK(R_TG,GLOBAL_PEAK_MONTH)
            ELSE
               GET_GROUP_PEAK_ON_PEAK_MONTH = 0.
               WRITE(4,*) "BAD MONTHLY LOAD VALUE FROM THE TRANSACTION"
               WRITE(4,*) "FORECAST FILE.  TRANSACTION GROUP = ",R_TG
               WRITE(4,*) '*** line 3339 TF_OBJT.FOR ***'

            ENDIF
         ELSE
            GET_GROUP_PEAK_ON_PEAK_MONTH = 0.
         ENDIF
      RETURN


      ENTRY GET_PA_PEAK_MONTH(R_PA)


         IF(ALLOCATED(GLOBAL_PA_PEAK_MONTH)) THEN
            GET_PA_PEAK_MONTH = GLOBAL_PA_PEAK_MONTH(R_PA)
         ELSE
            GET_PA_PEAK_MONTH = 7
         ENDIF
      RETURN


      ENTRY GET_CM_PEAK_MONTH(R_CM)


         IF(ALLOCATED(GLOBAL_CM_PEAK_MONTH)) THEN
            GET_CM_PEAK_MONTH = GLOBAL_CM_PEAK_MONTH(R_CM)
         ELSE
            GET_CM_PEAK_MONTH = 7
         ENDIF
      RETURN

      ENTRY GET_PEAK_ON_PA_PEAK_MONTH(R_TG,R_PA)

!
         IF(ALLOCATED(TF_PLANNING_PEAK)) THEN
            IF(R_TG > 0  .AND. R_TG <= MAX_TRANS_GROUPS) THEN
               TG = TRANS_LOAD_2_TRANS_GROUPS(R_TG)
               IF(TG > 0 .AND. TG <= MAX_TRANS_LOAD_GROUPS) THEN
                  GET_PEAK_ON_PA_PEAK_MONTH =
     +                  TF_PLANNING_PEAK(TG,GLOBAL_PA_PEAK_MONTH(R_PA))

               ELSE
                  GET_PEAK_ON_PA_PEAK_MONTH = 0.
               ENDIF
            ELSEIF(R_TG == 0) THEN
               GET_PEAK_ON_PA_PEAK_MONTH =
     +                TF_PLANNING_PEAK(R_TG,GLOBAL_PA_PEAK_MONTH(R_PA))
            ELSE
               GET_PEAK_ON_PA_PEAK_MONTH = 0.
               WRITE(4,*) "BAD MONTHLY LOAD VALUE FROM THE TRANSACTION"
               WRITE(4,*) "FORECAST FILE.  TRANSACTION GROUP = ",R_TG
               WRITE(4,*) '*** line 3831 TF_OBJT.FOR ***'

            ENDIF
         ELSE
            GET_PEAK_ON_PA_PEAK_MONTH = 0.
         ENDIF
      RETURN

!
      ENTRY GET_PA_PEAK(R_PA,R_MONTH)

!
         IF(ALLOCATED(TF_PLANNING_PEAK)) THEN
            IF(R_PA > 0  .AND. R_PA <= MAX_TRANS_GROUPS) THEN
                  GET_PA_PEAK = PA_PLANNING_PEAK(R_PA,R_MONTH)
            ELSEIF(R_PA == 0) THEN
               GET_PA_PEAK = PA_PLANNING_PEAK(R_PA,R_MONTH)
            ELSE
               GET_PA_PEAK = 0.
               WRITE(4,*) "BAD MONTHLY LOAD VALUE FROM THE TRANSACTION"
               WRITE(4,*) "FORECAST FILE.  TRANSACTION GROUP = ",R_PA
               WRITE(4,*) '*** line 3831 TF_OBJT.FOR ***'

            ENDIF
         ELSE
            GET_PA_PEAK = 0.
         ENDIF
      RETURN

!
      ENTRY GET_MONTHLY_MAINTENANCE_PENALTY(R_MAINTENANCE_PENALTY)
!

!
         GET_MONTHLY_MAINTENANCE_PENALTY = .TRUE.
!
         R_MAINTENANCE_PENALTY(1) = MAINTENANCE_PENALTY(1)
         R_MAINTENANCE_PENALTY(2) = MAINTENANCE_PENALTY(2)
         R_MAINTENANCE_PENALTY(3) = MAINTENANCE_PENALTY(3)
         R_MAINTENANCE_PENALTY(4) = MAINTENANCE_PENALTY(4)
         R_MAINTENANCE_PENALTY(5) = MAINTENANCE_PENALTY(5)
         R_MAINTENANCE_PENALTY(6) = MAINTENANCE_PENALTY(6)
         R_MAINTENANCE_PENALTY(7) = MAINTENANCE_PENALTY(7)
         R_MAINTENANCE_PENALTY(8) = MAINTENANCE_PENALTY(8)
         R_MAINTENANCE_PENALTY(9) = MAINTENANCE_PENALTY(9)
         R_MAINTENANCE_PENALTY(10) = MAINTENANCE_PENALTY(10)
         R_MAINTENANCE_PENALTY(11) = MAINTENANCE_PENALTY(11)
         R_MAINTENANCE_PENALTY(12) = MAINTENANCE_PENALTY(12)
      RETURN

!
      ENTRY PUT_AC_HOURLY_COST_AT_MARKET(R_TG,R_HOUR_PRICE,R_HR_COST,
     +                                   R_HR_IN_MONTH,R_MONTH)

!
         PUT_AC_HOURLY_COST_AT_MARKET = .FALSE.
! 8/30/01
         IF(.NOT. SAVE_TF_FILE_EXISTS) RETURN
         IF(PRICE_ONLY_WHOLESALE_REV .AND.
     +                              .NOT. USING_INDEXED_PRICING) RETURN
!
         IF(R_TG > MAX_TRANS_GROUPS) RETURN
!
         TG = TRANS_LOAD_GROUPS_INDEX(R_TG)
         IF(ASSET_CLASS_HOURLY_LOAD(HR_IN_MONTH,0,TG) > 0.) THEN
            AC_COST_FACTOR = R_HR_COST /
     +                      ASSET_CLASS_HOURLY_LOAD(R_HR_IN_MONTH,0,TG)
            NUMBER_OF_CLASSES = NUMBER_ASSET_2_TRANS(TG)
            DO AC_POSITION = 1, NUMBER_OF_CLASSES
               AC = ASSET_2_TRANS_INDEX(AC_POSITION,TG)
! 09/18/03. FOR MACY TO IGNORE BUILDING PURCHASE POWER EXPENSES WHEN
!           HE IS LOOKING AT ONLY THE GENERATION SIDE OF THE BUSINESS.
               IF(.NOT. PRICE_ONLY_WHOLESALE_REV) THEN

                  MONTHLY_AC_COST_AT_MARKET(AC,R_MONTH) =
     +               MONTHLY_AC_COST_AT_MARKET(AC,R_MONTH) +
     +                   ASSET_CLASS_HOURLY_LOAD(R_HR_IN_MONTH,AC,TG) *
     +                                                   AC_COST_FACTOR
                  MONTHLY_AC_COST_AT_MARKET(AC,0) =
     +               MONTHLY_AC_COST_AT_MARKET(AC,0) +
     +                   ASSET_CLASS_HOURLY_LOAD(R_HR_IN_MONTH,AC,TG) *
     +                                                   AC_COST_FACTOR
              ! NOT IGNORE NATIVE PURCHASE USING PRICING WHOLESALE = 'T'
               ENDIF
!
! FANCY INDEXED PRICING
! 02/10/03. NEED DYNAMIC UPDATING OF AC FOR EACH CLASS.
!
               IF(USING_INDEXED_PRICING) THEN
                  TEMP_I = NUMBER_TRANS_PER_AC_TG(AC,TG)
                  DO I = 1, TEMP_I
                     TRANS = TRANS_WITHIN_AC_TG(AC,TG,I)
! REASSIGNED ON 02/10/03.
                     AC = ASSET_CLASS_ID(TRANS)
                     AC = ASSET_CLASS_GROUPS_INDEX(AC)
!
                     IF(PRICE_INDEX_ACTIVE(TRANS) /= 'T') CYCLE
!
                     TEMP_PRICE = R_HOUR_PRICE +
     +                                      INDEXED_ENERGY_PRICE(TRANS)
                     TEMP_LOAD = TABLE_HOURLY_LOAD(R_HR_IN_MONTH,TRANS)
                     IF(TEMP_PRICE >= MINIMUM_MARKET_PRICE(TRANS) .AND.
     +                  TEMP_PRICE <= MAXIMUM_MARKET_PRICE(TRANS)) THEN
                        FINAL_REVENUE = TEMP_PRICE * TEMP_LOAD
                     ELSEIF(TEMP_PRICE <
     +                                MINIMUM_MARKET_PRICE(TRANS)) THEN
                        FINAL_REVENUE = MINIMUM_MARKET_PRICE(TRANS) *
     +                                                        TEMP_LOAD
                     ELSEIF(TEMP_PRICE >
     +                                MAXIMUM_MARKET_PRICE(TRANS)) THEN
                        FINAL_REVENUE = MAXIMUM_MARKET_PRICE(TRANS) *
     +                                                        TEMP_LOAD
                     ENDIF
! 3/19/02. FOR TRANS REVENUE REPORT
! 11/28/05. CHANGED DIMENSION.
!
                     CG = CUSTOMER_GROUP(TRANS)
                     CG = CUST_CLASS_GROUPS_INDEX(CG)
                     TRANS_INDEXED_REVENUE(CG) =
     +                        TRANS_INDEXED_REVENUE(CG) + FINAL_REVENUE

                     RI = REV_CLASS_INDEX(TRANS,4)
                     MONTHLY_AC_CONTRACT_REVENUE(AC,4,R_MONTH,RI,1) =
     +                 MONTHLY_AC_CONTRACT_REVENUE(AC,4,R_MONTH,RI,1) +
     +                                                    FINAL_REVENUE
                     MONTHLY_AC_CONTRACT_REVENUE(AC,4,0,RI,1) =
     +                    MONTHLY_AC_CONTRACT_REVENUE(AC,4,0,RI,1) +
     +                                                   FINAL_REVENUE

                  ENDDO ! TABLES
               ENDIF ! INDEXED PRICING
            ENDDO ! ASSET CLASSES

            PUT_AC_HOURLY_COST_AT_MARKET = .TRUE.
         ENDIF
      RETURN

!
      ENTRY PUT_MONTHLY_EP_COST_REV(R_AC,
     +                              R_MONTH,
     +                              R_MONTH_COST,
     +                              R_MONTH_REVENUE)

!
         PUT_MONTHLY_EP_COST_REV = .FALSE.
!
         ASSET_CLASS = ASSET_CLASS_GROUPS_INDEX(R_AC)
!
         MONTHLY_AC_COST_AT_MARKET(ASSET_CLASS,R_MONTH) =
     +               MONTHLY_AC_COST_AT_MARKET(ASSET_CLASS,R_MONTH) +
     +                                                     R_MONTH_COST

         MONTHLY_AC_COST_AT_MARKET(ASSET_CLASS,0) =
     +           MONTHLY_AC_COST_AT_MARKET(ASSET_CLASS,0) +
     +                                                     R_MONTH_COST
         PUT_MONTHLY_EP_COST_REV = .FALSE.
      RETURN

      ENTRY RETURN_ANNUL_CUSTOMER_VARIABLES(R_CLASS,
     +                                  ANNUAL_VARS_Purchased_Power,
     +                                  ANNUAL_VARS_Secondary_Sales,
     +                                  ANNUAL_VARS_Capacity_Sales,
     +                                  ANNUAL_VARS_Customer_Revenue,
     +                                  ANNUAL_VAR_Residential_Revenue,
     +                                  ANNUAL_VARS_Competitive_Sales,
     +                                  ANNUAL_VARS_Utility_Sales,
     +                                ANNUAL_VARS_Commercial_Revenues,
     +                                ANNUAL_VARS_Industrial_Revenues,
     +                                  ANNUAL_VARS_Lighting_Revenues,
     +                                 ANNUAL_VARS_Bulk_Power_Revenues,
     +                                  ANNUAL_VARS_Government_Sales,
     +                                  FE_Competitive_Unit_Sales,
     +                                  ANNUAL_VARS_Capacity_Purchases)

!         CALL DOES_TF_FILE_EXIST(TF_FILE_EXISTS)
         FE_Competitive_Unit_Sales = 0.
         IF(.NOT. SAVE_TF_FILE_EXISTS .OR. .NOT. RUN_TRANSACT) RETURN
!
         RETURN_ANNUL_CUSTOMER_VARIABLES = 1
         IF(R_CLASS-1 <= MAX_ASSET_GROUPS .AND.
     +                                       MAX_ASSET_GROUPS > 0) THEN
            IF(R_CLASS <= 0) THEN
               ASSET_CLASS = R_CLASS
            ELSE
               ASSET_CLASS = ASSET_CLASS_GROUPS_INDEX(R_CLASS-1)
            ENDIF
            IF(ASSET_CLASS >= 0) THEN

               ANNUAL_VARS_Purchased_Power =
     +                ANNUAL_VARS_Purchased_Power
     +               + MONTHLY_AC_COST_AT_MARKET(ASSET_CLASS,0)*.000001
               ANNUAL_VARS_Purchased_Power =
     +             .000001 * MONTHLY_AC_CONTRACT_EXPENSE(ASSET_CLASS,0,
     +                                                 PurchasedPower)
     +              + ANNUAL_VARS_Purchased_Power
               ANNUAL_VARS_Capacity_Purchases =
     +             .000001 * MONTHLY_AC_CONTRACT_EXPENSE(ASSET_CLASS,0,
     +                                  Purchased_Capacity_to_Level_RM)
               ANNUAL_VARS_Customer_Revenue = .000001 *
     +                     MONTHLY_INTERRUPTIBLE_REVENUE(ASSET_CLASS,0)
     +                      + ANNUAL_VARS_Customer_Revenue
!
               DO RI = 1, 4 ! across the three revenue sources
                  ANNUAL_VAR_Residential_Revenue =
     +              ANNUAL_VAR_Residential_Revenue + .000001 *
     +                 MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,RI,0,
     +                                                   Residential,1)
                  ANNUAL_VARS_Commercial_Revenues =
     +              ANNUAL_VARS_Commercial_Revenues + .000001 *
     +                MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,RI,0,
     +                                                    Commercial,1)
                  ANNUAL_VARS_Industrial_Revenues =
     +              ANNUAL_VARS_Industrial_Revenues + .000001 *
     +                MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,RI,0,
     +                                                    Industrial,1)
                  ANNUAL_VARS_Lighting_Revenues =
     +              ANNUAL_VARS_Lighting_Revenues + .000001 *
     +                MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,RI,0,
     +                                                      Lighting,1)
                  ANNUAL_VARS_Bulk_Power_Revenues =
     +              ANNUAL_VARS_Bulk_Power_Revenues + .000001 *
     +                MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,RI,0,

     +                                                    BulkPower,1)

                  ANNUAL_VARS_Government_Sales =
     +              ANNUAL_VARS_Government_Sales + .000001 *
     +                MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,RI,0,
     +                                                    Government,1)
                  ANNUAL_VARS_Secondary_Sales =
     +              ANNUAL_VARS_Secondary_Sales + .000001 *
     +                MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,RI,0,
! Secondary Sales renamed to secondary_sales (left) and SecondarySales 
! (right).
! Going with SecondarySales across codebase. Const.
! Merge3issue 
     +                                               SecondarySales,1)

                  ANNUAL_VARS_Capacity_Sales =
     +             ANNUAL_VARS_Capacity_Sales + .000001 *
     +                MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,RI,0,

     +                                                CapacitySales,1)

                  ANNUAL_VARS_Customer_Revenue =
     +              ANNUAL_VARS_Customer_Revenue + .000001 *
     +                 MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,RI,0,
     +                                             Customer_Revenues,1)
!
                  ANNUAL_VARS_Utility_Sales =
     +              ANNUAL_VARS_Utility_Sales + .000001 *
     +                MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,RI,0,
     +                                                 UtilitySales,1)
                  ANNUAL_VARS_Competitive_Sales =
     +              ANNUAL_VARS_Competitive_Sales + .000001 *
     +                MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,RI,0,

     +                                             CompetitiveSales,1)


                  DO ACCT = 22, 30
                     FE_Competitive_Unit_Sales =
     +                  FE_Competitive_Unit_Sales + .000001 *
     +                   MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,RI,0,
     +                                                          ACCT,1)
                  ENDDO
               ENDDO
            ENDIF
         ENDIF
      RETURN

      ENTRY RETURN_FE_PNL_REVENUES(R_CLASS,
     +                             R_UTILITY_SALES,
     +                             R_UTILITY_SALES_QUANT,
     +                             R_UTILITY_SALES_LOSS,
     +                             R_COMPETITIVE_SALES,
     +                             R_COMPETITIVE_SALES_QUANT,
     +                             R_COMPETITIVE_LOSS)

         R_UTILITY_SALES = 0.
         R_UTILITY_SALES_QUANT = 0.
         RETURN_FE_PNL_REVENUES = 1
         DO ACCT = 1, 9

            R_COMPETITIVE_SALES(ACCT) = 0.0
            R_COMPETITIVE_SALES_QUANT(ACCT) = 0.0
            R_COMPETITIVE_LOSS(ACCT) = 0.0
         ENDDO
!         CALL DOES_TF_FILE_EXIST(TF_FILE_EXISTS)
         IF(.NOT. SAVE_TF_FILE_EXISTS .OR. .NOT. RUN_TRANSACT) RETURN
!
         RETURN_ANNUL_CUSTOMER_VARIABLES = 1
!        1=$,2=METER,3=BUSBAR
         IF(R_CLASS-1 <= MAX_ASSET_GROUPS .AND. R_CLASS-1 >= 0) THEN
            ASSET_CLASS = ASSET_CLASS_GROUPS_INDEX(R_CLASS-1)
            ! todo: Remove IF statement and promote associated ELSE.
         if(.FALSE.) then
            IF(ASSET_CLASS >= 0) THEN
               R_UTILITY_SALES = .000001 *
     +             (MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,1,0,20,1)
     +              + MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,2,0,20,1)
     +              + MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,3,0,20,1)
     +             + MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,4,0,20,1))
               R_UTILITY_SALES_QUANT = .000001 *
     +              MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,1,0,20,3)

               R_UTILITY_SALES_LOSS = .000001 *
     +              (MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,1,0,20,2)
     +              -MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,1,0,20,3))
               DO ACCT = 11, 19
!
                  R_COMPETITIVE_SALES(ACCT) = .000001 *
     +             (MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,1,0,ACCT,1)
     +               + MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,2,0,
     +                                                          ACCT,1)
     +               + MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,3,0,
     +                                                          ACCT,1)
     +               + MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,4,0,
     +                                                         ACCT,1))
                  R_COMPETITIVE_SALES_QUANT(ACCT) = .000001 *
     +              MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,1,0,ACCT,3)

                  R_COMPETITIVE_LOSS(ACCT) = .000001 *
     +            (MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,1,0,ACCT,2)
     +            -MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,1,0,ACCT,3))
               ENDDO
            ENDIF
         else
!
! 8/2/02. TO GET WORKING FOR KEVIN WEAVER.
!
            IF(ASSET_CLASS >= 0) THEN

               I = 20

               R_UTILITY_SALES = .000001 *
     +             (MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,1,0,I,1)
     +              + MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,2,0,I,1)
     +              + MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,3,0,I,1)
     +              + MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,4,0,I,1))
               R_UTILITY_SALES_QUANT = .000001 *
     +             (MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,1,0,I,3)
     +              + MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,2,0,I,2)
     +              + MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,3,0,I,2))
               R_UTILITY_SALES_LOSS = .000001 *
     +              (MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,1,0,I,2)
     +               -MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,1,0,I,3))

!               I = 21c
!
!               R_COMPETITIVE_SALES(I+2) = .000001 *
!     +              (MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,1,0,I,1)
!     +               + MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,2,0,
!     +                                                           I,1)
!     +               + MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,3,0,
!     +                                                           I,1)
!     +               + MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,4,0,
!     +                                                          I,1))
!               R_COMPETITIVE_SALES_QUANT(I+2) = .000001 *
!     +              (MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,1,0,I,3)
!     +               + MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,2,0,
!     +                                                           I,2)
!     +               + MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,3,0,
!     +                                                          I,2))
!               R_COMPETITIVE_LOSS(I+2) = .000001 *
!     +            (MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,1,0,I,2)
!     +             -MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,1,0,I,3))

               DO ACCT = 1, 9

                  I = ACCT + 21

                  R_COMPETITIVE_SALES(ACCT) = .000001 *
     +              (MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,1,0,I,1)
     +               + MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,2,0,
     +                                                           I,1)
     +               + MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,3,0,
     +                                                           I,1)
     +               + MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,4,0,
     +                                                          I,1))
                  R_COMPETITIVE_SALES_QUANT(ACCT) = .000001 *
     +              (MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,1,0,I,3)
     +               + MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,2,0,
     +                                                           I,2)
     +               + MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,3,0,
     +                                                          I,2))
                  R_COMPETITIVE_LOSS(ACCT) = .000001 *
     +              (MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,1,0,I,2)
     +              - MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,1,0,I,3))
               ENDDO
            ENDIF
          endif
         ENDIF
      RETURN


      ENTRY RETURN_MONTH_CUSTOMER_VARIABLES(R_CLASS,
     +                                     R_MONTHLY_PURCHASED_POWER,
     +                                    R_MONTHLY_PURCHASED_CAPACITY)


!         CALL DOES_TF_FILE_EXIST(TF_FILE_EXISTS)
         R_MONTHLY_PURCHASED_POWER(0:12) = 0.
         R_MONTHLY_PURCHASED_CAPACITY(0:12) = 0.
         IF(.NOT. SAVE_TF_FILE_EXISTS .OR. .NOT. RUN_TRANSACT) RETURN
!
         RETURN_MONTH_CUSTOMER_VARIABLES = 1
         IF(R_CLASS-1 <= MAX_ASSET_GROUPS .AND. R_CLASS-1 >= 0) THEN
            ASSET_CLASS = ASSET_CLASS_GROUPS_INDEX(R_CLASS-1)
            IF(ASSET_CLASS >= 0) THEN
!
               DO MO = 0, 12
                  R_MONTHLY_PURCHASED_POWER(MO) =
     +               .000001*(MONTHLY_AC_COST_AT_MARKET(ASSET_CLASS,MO)
     +                + MONTHLY_AC_CONTRACT_EXPENSE(ASSET_CLASS,MO,
     +                                                PurchasedPower))
!
                  R_MONTHLY_PURCHASED_CAPACITY(MO) =
     +              .000001*MONTHLY_AC_CONTRACT_EXPENSE(ASSET_CLASS,MO,
     +                                  Purchased_Capacity_to_Level_RM)
               ENDDO
            ENDIF
         ELSEIF(R_CLASS == -1) THEN
!
            DO MO = 0, 12
               R_MONTHLY_PURCHASED_POWER(MO) =
     +               .000001 * MONTHLY_AC_CONTRACT_EXPENSE(-1,MO,
     +                                                 PurchasedPower)
               R_MONTHLY_PURCHASED_CAPACITY(MO) =
     +               .000001*MONTHLY_AC_CONTRACT_EXPENSE(-1,MO,
     +                                  Purchased_Capacity_to_Level_RM)
            ENDDO
!
         ENDIF
      RETURN


      ENTRY RETURN_MONTH_CUSTOMER_REVENUES(R_CLASS,MONTH_VARS)



!         CALL DOES_TF_FILE_EXIST(TF_FILE_EXISTS)
         IF(.NOT. SAVE_TF_FILE_EXISTS .OR. .NOT. RUN_TRANSACT) RETURN
!
         RETURN_MONTH_CUSTOMER_REVENUES = 1
         IF(R_CLASS-1 <= MAX_ASSET_GROUPS .AND. R_CLASS-1 >= 0) THEN
            ASSET_CLASS = ASSET_CLASS_GROUPS_INDEX(R_CLASS-1)
         ELSEIF(R_CLASS == -1) THEN
            ASSET_CLASS = -1
         ELSE
            RETURN
         ENDIF
            IF(ASSET_CLASS >= 0 .OR. R_CLASS == -1) THEN
!
               DO MO = 0, 12
                  MONTH_VARS(MO,Customer_Revenues) =
     +               MONTH_VARS(MO,Customer_Revenues) + .000001 *
     +                    MONTHLY_INTERRUPTIBLE_REVENUE(ASSET_CLASS,MO)
!
                  DO RI = 1, 4 ! across the four revenue sources
                     MONTH_VARS(MO,Residential) =
     +                  MONTH_VARS(MO,Residential) + .000001 *
     +                   MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,RI,MO,
     +                                                   Residential,1)
                     MONTH_VARS(MO,Commercial) =
     +                  MONTH_VARS(MO,Commercial) + .000001 *
     +                   MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,RI,MO,
     +                                                    Commercial,1)
                     MONTH_VARS(MO,Industrial) =
     +                  MONTH_VARS(MO,Industrial) + .000001 *
     +                   MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,RI,MO,
     +                                                    Industrial,1)
                     MONTH_VARS(MO,Lighting) =
     +                  MONTH_VARS(MO,Lighting) + .000001 *
     +                   MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,RI,MO,

     +                                                      Lighting,1)
                     MONTH_VARS(MO,BulkPower) =
     +                  MONTH_VARS(MO,BulkPower) + .000001 *
     +                   MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,RI,MO,
     +                                                    BulkPower,1)
                     MONTH_VARS(MO,Government) =
     +                  MONTH_VARS(MO,Government) + .000001 *
     +                   MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,RI,MO,
     +                                                    Government,1)
                     MONTH_VARS(MO,SecondarySales) =
     +                  MONTH_VARS(MO,SecondarySales) + .000001 *
     +                   MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,RI,MO,
     +                                               SecondarySales,1)
                     MONTH_VARS(MO,CapacitySales) =
     +                  MONTH_VARS(MO,CapacitySales) + .000001 *
     +                   MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,RI,MO,
     +                                                CapacitySales,1)
                     MONTH_VARS(MO,Customer_Revenues) =
     +                MONTH_VARS(MO,Customer_Revenues) + .000001 *
     +                 MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,RI,MO,
     +                                             Customer_Revenues,1)
                     MONTH_VARS(MO,UtilitySales) =
     +                  MONTH_VARS(MO,UtilitySales) + .000001 *
     +                   MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,RI,MO,
     +                                                 UtilitySales,1)
                     MONTH_VARS(MO,CompetitiveSales) =
     +                  MONTH_VARS(MO,CompetitiveSales) + .000001 *
     +                   MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,RI,MO,
     +                                             CompetitiveSales,1)


                  ENDDO
               ENDDO
            ENDIF
      RETURN

      ENTRY RETURN_CUSTOMER_CASH_REVENUES(R_CLASS,MONTH_VARS,
     +                                    R_MONTHLY_VALUES)


!         CALL DOES_TF_FILE_EXIST(TF_FILE_EXISTS)
         IF(.NOT. SAVE_TF_FILE_EXISTS .OR. .NOT. RUN_TRANSACT) RETURN
!
         RETURN_CUSTOMER_CASH_REVENUES = 1
         IF(R_CLASS-1 <= MAX_ASSET_GROUPS .AND. R_CLASS-1 >= 0) THEN
            ASSET_CLASS = ASSET_CLASS_GROUPS_INDEX(R_CLASS-1)
         ELSEIF(R_CLASS == -1) THEN
            ASSET_CLASS = -1
         ELSE
            RETURN
         ENDIF
            IF(ASSET_CLASS >= 0 .OR. R_CLASS == -1) THEN
               IF(ASSET_CLASS == -1) THEN
                  DO MO = 0, 12
!
                     R_MONTHLY_VALUES(MO) = .000001 *
     +                     MONTHLY_AC_CONTRACT_EXPENSE(ASSET_CLASS,MO,
     +                                                 PurchasedPower)
                  ENDDO
               ELSE
                  DO MO = 0, 12
!
                     R_MONTHLY_VALUES(MO) =
     +               .000001*(MONTHLY_AC_COST_AT_MARKET(ASSET_CLASS,MO)
     +                + MONTHLY_AC_CONTRACT_EXPENSE(ASSET_CLASS,MO,
     +                                                PurchasedPower))
                  ENDDO
               ENDIF
            ENDIF
            IF(ASSET_CLASS >= 0 .OR. R_CLASS == -1) THEN
!
               DO MO = 0, 12
!
                  MONTH_VARS(MO,Cash_Relationship_Revenues) =
     +               MONTH_VARS(MO,Cash_Relationship_Revenues)+.000001*
     +                    MONTHLY_INTERRUPTIBLE_REVENUE(ASSET_CLASS,MO)
                  DO RI = 1, 4 ! across the four revenue sources
                     MONTH_VARS(MO,Cash_Residential) =
     +                  MONTH_VARS(MO,Cash_Residential) + .000001 *
     +                   MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,RI,MO,
     +                                                   Residential,1)
                     MONTH_VARS(MO,Cash_Commercial) =
     +                  MONTH_VARS(MO,Cash_Commercial) + .000001 *
     +                   MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,RI,MO,

     +                                                    Commercial,1)
                     MONTH_VARS(MO,Cash_Industrial) =
     +                  MONTH_VARS(MO,Cash_Industrial) + .000001 *
     +                   MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,RI,MO,
     +                                                    Industrial,1)
                     MONTH_VARS(MO,Cash_Lighting) =
     +                  MONTH_VARS(MO,Cash_Lighting) + .000001 *
     +                   MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,RI,MO,
     +                                                      Lighting,1)
                     MONTH_VARS(MO,CashBulkPower) =
     +                  MONTH_VARS(MO,CashBulkPower) + .000001 *
     +                   MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,RI,MO,
     +                                                    BulkPower,1)
                     MONTH_VARS(MO,Cash_Government) =
     +                  MONTH_VARS(MO,Cash_Government) + .000001 *
     +                   MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,RI,MO,
     +                                                    Government,1)
                     MONTH_VARS(MO,CashSecondarySales) =
     +                  MONTH_VARS(MO,CashSecondarySales) + .000001 *
     +                   MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,RI,MO,
     +                                               SecondarySales,1)
                     MONTH_VARS(MO,CashCapacitySales) =
     +                  MONTH_VARS(MO,CashCapacitySales) + .000001 *
     +                   MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,RI,MO,
     +                                                CapacitySales,1)
                     MONTH_VARS(MO,Cash_Relationship_Revenues) =
     +               MONTH_VARS(MO,Cash_Relationship_Revenues)+.000001*
     +                  MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,RI,MO,
     +                                             Customer_Revenues,1)
                     MONTH_VARS(MO,Cash_Utility_Sales) =
     +                  MONTH_VARS(MO,Cash_Utility_Sales) + .000001 *
     +                   MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,RI,MO,
     +                                                 UtilitySales,1)
                     MONTH_VARS(MO,Cash_Competitive_Sales) =
     +                 MONTH_VARS(MO,Cash_Competitive_Sales) + .000001*
     +                   MONTHLY_AC_CONTRACT_REVENUE(ASSET_CLASS,RI,MO,
     +                                             CompetitiveSales,1)


                  ENDDO
               ENDDO
            ENDIF
      RETURN

!
      ENTRY GET_TRANS_HOUR_DISTRIBUTION(R_LOAD_GROUP,TEMP_HOURLY_INDEX)
!

!
         ALLOCATE(TEMP_CHRONO_LOAD(SAVE_HOURS))

         DO I = 1, SAVE_HOURS
            TEMP_CHRONO_LOAD(I) = TRANS_HOURLY_LOAD(I,R_LOAD_GROUP)
            TEMP_HOURLY_INDEX(I) = CHRONO_TRANS_HOUR(I)
         ENDDO
         call write_sic_trace("tf_objt:0020",save_hours)
        CALL SortIncrPos(SAVE_HOURS,TEMP_HOURLY_INDEX,TEMP_CHRONO_LOAD)
!
         IF(ALLOCATED(TEMP_CHRONO_LOAD)) DEALLOCATE(TEMP_CHRONO_LOAD)
!
         GET_TRANS_HOUR_DISTRIBUTION = .TRUE.

      RETURN

!
      ENTRY GET_HYDRO_LOAD_AFTER_EL(   R_HYDRO_LOAD,
     +                                 R_HOURS,
     +                                 R_LOAD_GROUP)
!



         DO I = 1, R_HOURS
            R_HYDRO_LOAD(I) = HYDRO_HOURLY_LOAD(I,R_LOAD_GROUP)
         ENDDO
         GET_HYDRO_LOAD_AFTER_EL = .TRUE.
      RETURN

!
      ENTRY PUT_HYDRO_LOAD_AFTER_EL(   R_HYDRO_LOAD,
     +                                 R_HOURS,
     +                                 R_LOAD_GROUP)

         DO I = 1, R_HOURS
            HYDRO_HOURLY_LOAD(I,R_LOAD_GROUP) =  R_HYDRO_LOAD(I)
         ENDDO
         PUT_HYDRO_LOAD_AFTER_EL = .TRUE.
      RETURN

!
      ENTRY GET_HYDRO_HOUR_DISTRIBUTION(R_LOAD_GROUP,TEMP_HOURLY_INDEX)
!

!
         ALLOCATE(TEMP_CHRONO_LOAD(SAVE_HOURS))

         DO I = 1, SAVE_HOURS
            TEMP_CHRONO_LOAD(I) = HYDRO_HOURLY_LOAD(I,R_LOAD_GROUP)
            TEMP_HOURLY_INDEX(I) = CHRONO_TRANS_HOUR(I)
         ENDDO
         call write_sic_trace("tf_objt:0021",save_hours)
        CALL SortIncrPos(SAVE_HOURS,TEMP_HOURLY_INDEX,TEMP_CHRONO_LOAD)
!
         IF(ALLOCATED(TEMP_CHRONO_LOAD)) DEALLOCATE(TEMP_CHRONO_LOAD)
!
         GET_HYDRO_HOUR_DISTRIBUTION = .TRUE.

      RETURN

      ENTRY TG_FROM_TRANS_LOAD_GROUP(R_LOAD_GROUP)

         TG_FROM_TRANS_LOAD_GROUP = TRANS_LOAD_GROUP_2_TG(R_LOAD_GROUP)
      RETURN

      ENTRY MONTH_PEAK_BY_CG(R_CG)

         MONTH_PEAK_BY_CG = MONTHLY_CLASS_PEAK(R_CG)
      RETURN

      ENTRY HOURLY_LOAD_FROM_AC_TG(R_HOURS,R_AC,R_TG)


         IF(R_TG > 0 .AND. R_TG <= MAX_TRANS_GROUPS) THEN
            TG = TRANS_LOAD_2_TRANS_GROUPS(R_TG)
            IF(TG > 0 .AND. TG <= MAX_TRANS_LOAD_GROUPS) THEN
               HOURLY_LOAD_FROM_AC_TG =
     +                    ASSET_CLASS_HOURLY_LOAD(R_HOURS,R_AC,TG)
            ELSE
               HOURLY_LOAD_FROM_AC_TG = 0.
            ENDIF
         ELSE
            HOURLY_LOAD_FROM_AC_TG = 0.
         ENDIF
      RETURN

      ENTRY HOURLY_TRANSACTION_LOAD(R_HR,R_TG)

!
! R_TG COMES IN INDEXED TO TRANS_GROUPS. MUST RE-INDEX TO TRANSACTION
! LOADS.
! 0 IS A SPECIAL CASE = TOTAL SYSTEM LOADS
!
         IF(.NOT. ALLOCATED(TRANS_HOURLY_LOAD)) THEN
            HOURLY_TRANSACTION_LOAD = 0.
         ELSEIF(R_TG > 0 .AND. R_TG <= MAX_TRANS_GROUPS) THEN
            TRANS_GROUP = TRANS_LOAD_2_TRANS_GROUPS(R_TG)
            IF(TRANS_GROUP > 0 .AND.
     +                       TRANS_GROUP <= MAX_TRANS_LOAD_GROUPS) THEN
               HOURLY_TRANSACTION_LOAD =
     +                              TRANS_HOURLY_LOAD(R_HR,TRANS_GROUP)
            ELSE
               HOURLY_TRANSACTION_LOAD = 0.
            ENDIF
         ELSEIF(R_TG == 0)THEN
            HOURLY_TRANSACTION_LOAD = TRANS_HOURLY_LOAD(R_HR,0)
         ELSE
            HOURLY_TRANSACTION_LOAD = 0.

         ENDIF
      RETURN


      ENTRY GET_TRANS_LOAD_2_TRANS_GROUPS(R_TG)


         IF(ALLOCATED(TRANS_LOAD_2_TRANS_GROUPS)) THEN
            IF(R_TG > 0 .AND. R_TG <= MAX_TRANS_GROUPS) THEN
               GET_TRANS_LOAD_2_TRANS_GROUPS =
     +                                  TRANS_LOAD_2_TRANS_GROUPS(R_TG)
            ELSE
               GET_TRANS_LOAD_2_TRANS_GROUPS = 0
            ENDIF
         ELSE
            GET_TRANS_LOAD_2_TRANS_GROUPS = 0
         ENDIF
      RETURN


      ENTRY GET_TRANS_LOAD_AGGREGATION(R_TG)


! COMES IN AS TRANS LOAD NUMBER
!
         IF(R_TG > 0 .AND. R_TG <= MAX_TRANS_GROUPS) THEN
            TRANS_GROUP = TRANS_LOAD_2_TRANS_GROUPS(R_TG)
            IF(TRANS_GROUP > 0) THEN
               GET_TRANS_LOAD_AGGREGATION = GET_HG_FROM_TG(TRANS_GROUP)
            ELSE
               GET_TRANS_LOAD_AGGREGATION = 0
            ENDIF
         ELSE
            GET_TRANS_LOAD_AGGREGATION = 0
         ENDIF
!
      RETURN


      ENTRY YES_USE_TF_FILE_FOR_PRICE()

         YES_USE_TF_FILE_FOR_PRICE = TF_GROUP1_ACTIVE
      RETURN

      ENTRY YES_USE_TF_FILE_FOR_MULTIAREA()

         YES_USE_TF_FILE_FOR_MULTIAREA = TF_ANY_GROUP_ACTIVE
      RETURN

      ENTRY TF_FILE_LOAD_GROUP_ACTIVE(R_TG)



         IF(ALLOCATED(TRANS_LOAD_GROUPS_INDEX)) THEN
            IF(R_TG > 0 .AND. R_TG <= MAX_TRANS_GROUPS) THEN
               TF_FILE_LOAD_GROUP_ACTIVE =
     +                               TRANS_LOAD_GROUPS_INDEX(R_TG) /= 0
            ELSE
               TF_FILE_LOAD_GROUP_ACTIVE = .FALSE.
            ENDIF
         ELSE
            TF_FILE_LOAD_GROUP_ACTIVE = .FALSE.
         ENDIF
      RETURN


      ENTRY TF_IPL_ELECTIC_PLAN_COST(R_ELECT_PLAN_FUEL,
     +                               R_ELECT_PLAN_PURCHASE)


         ELECT_PLAN_DATABASE(0,:) =

     +                           SUM(ELECT_PLAN_DATABASE(1:12,:),DIM=1)
         ELECT_PLAN_DATABASE = ELECT_PLAN_DATABASE/100000.
         R_ELECT_PLAN_FUEL(:) = ELECT_PLAN_DATABASE(:,1)
         R_ELECT_PLAN_PURCHASE(:) = ELECT_PLAN_DATABASE(:,2)
      RETURN
      END function MANAGE_TRANSACTION_FORECASTS
!
!
!

!
      SUBROUTINE OPEN_TRANS_HOURLY_LOAD_FILE(R_LOAD_NAME,R_LOAD_NUMBER,
     +                                      R_YEAR,R_LOAD_UNIT,R_MONTH,
     +                                       R_SCENARIO_INDEX)
      use end_routine, only: end_program, er_message
      use grx_planning_routines
      use miscmod

      logical :: file_open
      integer (kind=2) :: timeshere=0
      integer :: ios
      INTEGER(kind=2) ::  HOURLY_LOAD_IN,R_LOAD_NUMBER,R_YEAR,
     + R_LOAD_UNIT,
     +            SAVE_LOAD_UNIT=0,R_MONTH,SCENARIO_NUMBER,
     +            R_SCENARIO_INDEX
!
      CHARACTER*2 LOAD_FILE_CHAR_EXT
      LOGICAL*4   FILE_EXISTS
      REAL*4      GET_SCENARIO_LOAD_SHAPE
      CHARACTER*5 R_LOAD_NAME
      CHARACTER*256 LOAD_NAME,INPUT_LOAD_NAME,FILE_NAME,
     +              OUTPUT_DIRECTORY,BASE_FILE_DIRECTORY,
     +              LDE_FILE_DIRECTORY


         
         SAVE_LOAD_UNIT = R_LOAD_UNIT
!
         SCENARIO_NUMBER =
     +                    NINT(GET_SCENARIO_LOAD_SHAPE(R_YEAR,R_MONTH))

         IF(SCENARIO_NUMBER >= 5 .AND. R_SCENARIO_INDEX /= -99) THEN !
!
           IF(SCENARIO_NUMBER > 1900 .AND. SCENARIO_NUMBER < 2000) THEN
               SCENARIO_NUMBER = SCENARIO_NUMBER - 1900
            ELSE
               SCENARIO_NUMBER = SCENARIO_NUMBER - 2000
            ENDIF

            FILE_NAME = trim(LDE_FILE_DIRECTORY())//"LDE"//
     +                           trim(R_LOAD_NAME)//".B"//
     +                              LOAD_FILE_CHAR_EXT(SCENARIO_NUMBER)
!
            INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
            IF(.NOT. FILE_EXISTS) THEN
               FILE_NAME = trim(OUTPUT_DIRECTORY())//"LDE"//
     +                           trim(R_LOAD_NAME)//".B"//
     +                              LOAD_FILE_CHAR_EXT(SCENARIO_NUMBER)
               INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
            ENDIF
!
            IF(.NOT. FILE_EXISTS) THEN
               WRITE(4,*) "Scenario Maker Load Shape file ",FILE_NAME
               WRITE(4,*) "does not exist in year ",R_YEAR
               WRITE(4,*) "for reference file ",R_LOAD_NAME
               WRITE(4,*) "and reference number ",SCENARIO_NUMBER
               WRITE(4,*) '*** line 3744 TF_OBJT.FOR ***'
               er_message='See WARNING MESSAGES -tf_objt.for-21'
               call end_program(er_message)
            ENDIF
!
         ELSE
!
            IF(R_LOAD_NUMBER == 0) THEN
               INPUT_LOAD_NAME = trim(R_LOAD_NAME)//".BIN"
               LOAD_NAME = trim(R_LOAD_NAME)//".B00"
            ELSE
               INPUT_LOAD_NAME = trim(R_LOAD_NAME)//".B"//
     +                                LOAD_FILE_CHAR_EXT(R_LOAD_NUMBER)
               LOAD_NAME = INPUT_LOAD_NAME
            ENDIF
!

            FILE_NAME = trim(LDE_FILE_DIRECTORY())//"LDE"//
     +                           trim(R_LOAD_NAME)//".B"//
     +                                LOAD_FILE_CHAR_EXT(R_LOAD_NUMBER)
!
            INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
            IF(.NOT. FILE_EXISTS) THEN
               FILE_NAME = trim(OUTPUT_DIRECTORY())//"LDE"//
     +                           trim(R_LOAD_NAME)//".B"//
     +                                LOAD_FILE_CHAR_EXT(R_LOAD_NUMBER)
               INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
            ENDIF
!
            IF(.NOT. FILE_EXISTS) THEN
               WRITE(4,*) "Transaction loads file ",FILE_NAME
               WRITE(4,*) "does not exist in year ",R_YEAR
               WRITE(4,*) "for reference file ",R_LOAD_NAME
               WRITE(4,*) "and reference number ",R_LOAD_NUMBER
               WRITE(4,*) ' TF_OBJT.FOR ***'
               er_message="File " // trim(file_name) //
     +          " does not exist. See WARNING MESSAGES " //
     +          "-tf_objt.for-22"
               call end_program(er_message)
            ENDIF
         ENDIF ! USE SCENARIO LOAD SHAPE
!
         
         
         OPEN(R_LOAD_UNIT,FILE=FILE_NAME,ACCESS="DIRECT",
     +                                           STATUS="OLD",RECL=118)
!
      RETURN

      ENTRY CLOSE_TRANS_HOURLY_LOAD_FILE(R_LOAD_UNIT)
         timeshere=timeshere+1
         file_name=get_filename_from_unit(int(r_load_unit))

         if(timeshere>=14) then
            timeshere=timeshere ! Debugstop
         end if
     
     
         inquire(unit=r_load_unit, opened=file_open)
         if(file_open) then
            CLOSE(R_LOAD_UNIT, iostat=ios)
            if(ios/=0) then
                er_message="tf_objt:0017 - Close(R_LOAD_UNIT) " //
     + "caused error " // trim(itos(ios)) // "."
                call end_program(er_message)
            end if
         else
            er_message="tf_objt:0016 - unit " //
     +     trim(itos(int(r_load_unit))) // " is not  open, and an " //
     +     "attempt is being made to close it."
       call end_program(er_message)

         end if
!         CLOSE(2101)
      RETURN
      END
!
!
!

!
      SUBROUTINE OPEN_SCEN_HOURLY_LOAD_FILE(R_LOAD_NAME,R_LOAD_NUMBER,
     +                                       R_YEAR,R_LOAD_UNIT)
      use end_routine

!
      INTEGER*2   HOURLY_LOAD_IN,R_LOAD_NUMBER,R_YEAR,R_LOAD_UNIT,
     +            SAVE_LOAD_UNIT/0/
!
      CHARACTER*2 LOAD_FILE_CHAR_EXT
      LOGICAL*4 FILE_EXISTS
      CHARACTER*5 R_LOAD_NAME
      CHARACTER*256 LOAD_NAME,INPUT_LOAD_NAME,FILE_NAME,
     +              OUTPUT_DIRECTORY,BASE_FILE_DIRECTORY,
     +              SHB_FILE_DIRECTORY
!     +              LDE_FILE_DIRECTORY
!
!
! END DATA DECLARATIONS
!
         SAVE_LOAD_UNIT = R_LOAD_UNIT
         IF(R_LOAD_NUMBER == 0) THEN
            INPUT_LOAD_NAME = trim(R_LOAD_NAME)//".SIN"
            LOAD_NAME = trim(R_LOAD_NAME)//".S00"
         ELSE
            INPUT_LOAD_NAME = trim(R_LOAD_NAME)//".S"//
     +                                LOAD_FILE_CHAR_EXT(R_LOAD_NUMBER)
            LOAD_NAME = INPUT_LOAD_NAME
         ENDIF
!
!         FILE_NAME = trim(BASE_FILE_DIRECTORY())//"SHB"//
         FILE_NAME = trim(SHB_FILE_DIRECTORY())//"SHB"//
     +                           trim(R_LOAD_NAME)//".S"//
     +                                LOAD_FILE_CHAR_EXT(R_LOAD_NUMBER)
!
         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
         IF(.NOT. FILE_EXISTS) THEN
            FILE_NAME = trim(OUTPUT_DIRECTORY())//"SHB"//
     +                           trim(R_LOAD_NAME)//".S"//
     +                                LOAD_FILE_CHAR_EXT(R_LOAD_NUMBER)
         ENDIF
!
         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
         IF(.NOT. FILE_EXISTS) THEN
            WRITE(4,*) "SCENaction loads file ",FILE_NAME
            WRITE(4,*) "does not exist in year ",R_YEAR
            WRITE(4,*) "for reference file ",R_LOAD_NAME
            WRITE(4,*) "and reference number ",R_LOAD_NUMBER
            WRITE(4,*) '*** line 3841 TF_OBJT.FOR ***'
            er_message='See WARNING MESSAGES -tf_objt.for-23'
            call end_program(er_message)
         ENDIF
!         CLOSE(R_LOAD_UNIT)
         OPEN(R_LOAD_UNIT,FILE=FILE_NAME,ACCESS="DIRECT",
     +                                           STATUS="OLD",RECL=118)

      RETURN

      ENTRY CLOSE_SCEN_HOURLY_LOAD_FILE(R_LOAD_UNIT)

         CLOSE(R_LOAD_UNIT)
!         CLOSE(2101)
      RETURN
      END


!
!        PROGRAM TO READ MULTI-TAB INFORMATION ON TRANSACTION GROUPS
!                 AND CONVERT TO BINARY FORMAT
!                       COPYRIGHT (C) 1998
!      ALL RIGHTS RESERVED M.S. GERBER & ASSOCIATES, INC.
!


!
      SUBROUTINE TG_OBJECT
      use end_routine, only: end_program, er_message
      use logging

      use spindriftlib
      use prod_arrays_dimensions
      USE SIZECOM

      LOGICAL*1 SAVE_TG_FILE_EXISTS/.FALSE./,R_TG_FILE_EXISTS
      INTEGER*2   UNIT_NUM/10/,INUNIT,IREC,DELETE,LRECL/1052/,
     +            SAVE_TRANS_GROUPS_TABLES/0/,R_TRANS_GROUPS_TABLES,
     +            SAVE_TRANS_GROUPS_RECORDS/0/,R_TRANS_GROUPS_RECORDS
      INTEGER IOS
      CHARACTER*5 TRANS_GROUPS_FILE,OVERLAY_FAMILY_NAME
      CHARACTER*3 HOURLY_PRICE_NAME
      CHARACTER*256 FILE_NAME,FILE_NAME_OVL
      CHARACTER*256 OUTPUT_DIRECTORY
      CHARACTER*256 BASE_FILE_DIRECTORY
      CHARACTER*152 MESSAGE
      LOGICAL*4 FILE_EXISTS
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER*1024 RECLN
!
! SIMULATION VARIABLES
!
      CHARACTER*35 GROUP_NAME
      CHARACTER*40 SCENARIO_VARIABLE
      CHARACTER*60 LONG_TRANS_GROUP_NAME
      CHARACTER*1 GROUP_ACTIVE,SPINNING_UNITS,
     +            OFF_PEAK_SPINNING_UNITS,
     +            REPORT_CL_CAPACITY,
     +            TIME_ZONE,NOX_SEASON,PURCHASE_POWER_ASSIGN,
     +            CREATE_HOURLY_PRICE
      CHARACTER*2 ST_LHS_FOR_PRICES
      INTEGER*2   TRANSACTION_GROUP,
     +            ASSET_CLASS_ID,
     +            ASSET_CLASS_REV_ALLOC_VECTOR,
     +            PURCHASE_ASSET_CLASS_ID,
     +            PURCHASE_ASSET_ALLOC_VECTOR,
     +            RTO_GROUP,
     +            NOX_YEAR,
     +            END_NOX_YEAR,
     +            HYDRO_LOAD_AGGREGATION,
     +            PLANNING_AREA,
     +            MRX_ICAP_UNIT_LINK,
     +            REGIONAL_CAPACITY_MARKET
      CHARACTER*6 BASECASE_MKT_AREA_ID_tf2,
     +            BASE_CASE_TRANS_AREA_ID,BASECASE_SUBREGION_ID,
     +                                 PRIMARY_STATE,
     +                                 PRIMARY_SUPERREGION

      REAL*4 SPINNING_RESERVE,
     +    OFF_PEAK_SPINNING_RESERVE,
     +    MAX_HOURLY_RAMP_UP,
     +    MAX_HOURLY_RAMP_DOWN,
     +    FIRST_CAPACITY_VALUE,
     +    FIRST_CAPACITY_PERCENT,
     +    SECOND_CAPACITY_VALUE,
     +    SECOND_CAPACITY_PERCENT,
     +    THIRD_CAPACITY_VALUE,
     +    THIRD_CAPACITY_PERCENT,
     +    CAPACITY_ADDER,
     +    ADDITIONAL_CAPACITY_VALUE(7),
     +    ADDITIONAL_CAPACITY_PERCENT(7),
     +    MRX_VOLATILITY_MULT,
     +    NIGHT_SCARCITY_MULT,
     +    WEEKEND_SCARCITY_MULT,
     +    PRICE_CAP,
     +    PRICE_MINIMUM,
     +    MAX_HOURLY_TG_IMPORT,
     +    MAX_HOURLY_TG_EXPORT
      CHARACTER*50 COMMENT
!
! FILE MANAGEMENT VARIABLES
!
      CHARACTER*17 FILE_TYPE/'Transact Group   '/
      CHARACTER*2 TGROUP_OL/'BC'/,R_TGROUP_OL
      LOGICAL*1 LAHEY_LF95
      CHARACTER*30 SCREEN_OUTPUT
!
! SPCapEx VARIABLES 11/18/06
!
      REAL (KIND=4) :: LOAD_PLANNING_FACTOR ! 58
      CHARACTER (LEN=256) :: LOAD_FILE_FOR  ! 59
      INTEGER (KIND=2) :: LOAD_SCENARIO_NUM ! 60
      CHARACTER (LEN=256) :: PRICE_FILE_FOR
      INTEGER (KIND=2) :: PRICE_SCENARIO_NUM
      REAL (KIND=4) :: ZONE_MAX_RESERVE
      REAL (KIND=4) :: ZONE_TARGET_RESERVE   ! 64
      CHARACTER (LEN=256) :: CAPEX_COMMENT  ! 65
      CHARACTER (LEN=256) :: MARKET_AREA_LOAD_FILE_FOR  ! 66
      INTEGER (KIND=2) :: MARKET_AREA_LOAD_SCENARIO_NUM ! 67
      CHARACTER (LEN=6) :: PRE_DISPATCH_HYDRO  ! 68
      CHARACTER (LEN=6) :: AGGREGATE_THERMAL   ! 69
      INTEGER (KIND=2) :: MAXIMUM_AGGREGATION_INTERVALS ! 70
! END SPCapEx VARIABLES
      REAL (KIND=4) :: PRICE_ESCALATION_RATE ! 71
      REAL (KIND=4) :: MINIMUM_CAPACITY_TESTING_RATIO=1.0, ! 72
     +                 MAXIMUM_CAPACITY_TESTING_RATIO=1.5 ! 73
      CHARACTER (LEN=1) :: SAVE_MRX_EXPANSION_PLAN="F" ! 74

!          ROUTINE TO CONVERT FROM ASCII TO DIRECT-ACESS BINARY
!          COPYRIGHT (C) 1983-98  M.S. GERBER & ASSOCIATES, INC.
!

!
! CONVERT THE TRANSACTION GROUPS FILE
!
!

      ENTRY TG_MAKEBIN


      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//TRANS_GROUPS_FILE()
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_LOCATE_WRITE(16,30,TRANS_GROUPS_FILE(),ALL_VERSIONS,0)
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
      ENDIF
      FILE_NAME = trim(BASE_FILE_DIRECTORY())//
     +                      "TGB"//trim(TRANS_GROUPS_FILE())//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      IF(FILE_EXISTS) THEN
!
         SAVE_TG_FILE_EXISTS = .TRUE.
!
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCTGROUP.BIN",
     +                     ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
!
!
         SAVE_TRANS_GROUPS_TABLES = 0
!
         GROUP_NAME = 'Unassigned          '
         GROUP_ACTIVE = 'T'
         TRANSACTION_GROUP = 1
         BASECASE_MKT_AREA_ID_tf2 = 'BLANK ' ! CHAR*6
         BASE_CASE_TRANS_AREA_ID = 'BLANK ' ! CHAR*6
         BASECASE_SUBREGION_ID = 'BLANK ' ! CHAR*6
         SPINNING_UNITS = 'M'
         OFF_PEAK_SPINNING_UNITS = 'Z'
         SPINNING_RESERVE = 0.0
         OFF_PEAK_SPINNING_RESERVE = -99999.
! ADDED 4/14/98. GAT.
         MAX_HOURLY_RAMP_UP = 999999.0
         MAX_HOURLY_RAMP_DOWN = 999999.0
         FIRST_CAPACITY_VALUE = 1.0
         FIRST_CAPACITY_PERCENT = 20.0
         SECOND_CAPACITY_VALUE = 5.0
         SECOND_CAPACITY_PERCENT = 50.0
         THIRD_CAPACITY_VALUE = 200.0
         THIRD_CAPACITY_PERCENT = 100.0
         CAPACITY_ADDER = 0.
         REPORT_CL_CAPACITY = 'T'
         TIME_ZONE = 'E'
         NOX_SEASON = 'F'
!
         ADDITIONAL_CAPACITY_VALUE(1) = 0.0
         ADDITIONAL_CAPACITY_PERCENT(1) = 0.0
         ADDITIONAL_CAPACITY_VALUE(2) = 0.0
         ADDITIONAL_CAPACITY_PERCENT(2) = 0.0
         ADDITIONAL_CAPACITY_VALUE(3) = 0.0
         ADDITIONAL_CAPACITY_PERCENT(3) = 0.0
         ADDITIONAL_CAPACITY_VALUE(4) = 0.0
         ADDITIONAL_CAPACITY_PERCENT(4) = 0.0
         ADDITIONAL_CAPACITY_VALUE(5) = 0.0
         ADDITIONAL_CAPACITY_PERCENT(5) = 0.0
         ADDITIONAL_CAPACITY_VALUE(6) = 0.0
         ADDITIONAL_CAPACITY_PERCENT(6) = 0.0
         ADDITIONAL_CAPACITY_VALUE(7) = 0.0
         ADDITIONAL_CAPACITY_PERCENT(7) = 0.0
!
         PURCHASE_POWER_ASSIGN = 'U'
         PURCHASE_ASSET_CLASS_ID = 0
         PURCHASE_ASSET_ALLOC_VECTOR = 0
!
         CREATE_HOURLY_PRICE = 'F'
         HOURLY_PRICE_NAME = '   '
         RTO_GROUP = 0
         NOX_YEAR = 0
         END_NOX_YEAR = 2100
         ST_LHS_FOR_PRICES = '  '
         MRX_VOLATILITY_MULT = 1.0
!
         NIGHT_SCARCITY_MULT = 1.0
         WEEKEND_SCARCITY_MULT = 1.0
         PRICE_CAP = 999999.
         PRICE_MINIMUM = 0.10
!         REGIONAL_CAPACITY_MARKET = -99999
         MAX_HOURLY_TG_IMPORT = 999999.
         MAX_HOURLY_TG_EXPORT = 999999.
         HYDRO_LOAD_AGGREGATION = 0
         PLANNING_AREA = 0
         SCENARIO_VARIABLE = '                                        '
         LONG_TRANS_GROUP_NAME = '                                    '
         PRICE_ESCALATION_RATE = 0.
!
         IREC = 0
         READ(10,*) DELETE
         DO ! TABLES
            DO ! GROUP-BASED RECORDS
               READ(10,1000,IOSTAT=IOS) RECLN
!
               IF(IOS /= 0) EXIT ! END OF FILE
!
               IF(RECLN(1:1) == '7') THEN ! END OF TABLE ! EXIT AT
               !BOTTOM OF IF
                  EXIT ! GO TO NEXT TABLE
               ENDIF
!
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               REGIONAL_CAPACITY_MARKET = -29999
               READ(RECLN,*,ERR=200)   DELETE,
     +                                 GROUP_NAME,
     +                                 GROUP_ACTIVE,
     +                                 TRANSACTION_GROUP,
     +                                 BASECASE_MKT_AREA_ID_tf2,
     +                                 BASE_CASE_TRANS_AREA_ID,
     +                                 BASECASE_SUBREGION_ID,
     +                                 SPINNING_UNITS,
     +                                 SPINNING_RESERVE,
     +                                 COMMENT,
     +                                 MAX_HOURLY_RAMP_UP,
     +                                 MAX_HOURLY_RAMP_DOWN,
     +                                 FIRST_CAPACITY_VALUE,
     +                                 FIRST_CAPACITY_PERCENT,
     +                                 SECOND_CAPACITY_VALUE,
     +                                 SECOND_CAPACITY_PERCENT,
     +                                 THIRD_CAPACITY_VALUE,
     +                                 THIRD_CAPACITY_PERCENT,
     +                                 REPORT_CL_CAPACITY,
     +                                 ASSET_CLASS_ID,
     +                                 ASSET_CLASS_REV_ALLOC_VECTOR,
     +                                 TIME_ZONE,
     +                                 CAPACITY_ADDER,
     +                                 NOX_SEASON,
     +                                 PURCHASE_POWER_ASSIGN,
     +                                 PURCHASE_ASSET_CLASS_ID,
     +                                 PURCHASE_ASSET_ALLOC_VECTOR,
     +                                 CREATE_HOURLY_PRICE,
     +                                 HOURLY_PRICE_NAME,
     +                                 ADDITIONAL_CAPACITY_VALUE,
     +                                 ADDITIONAL_CAPACITY_PERCENT,
     +                                 RTO_GROUP,
     +                                 MRX_VOLATILITY_MULT,
     +                                 NOX_YEAR,
     +                                 NIGHT_SCARCITY_MULT,
     +                                 WEEKEND_SCARCITY_MULT,
     +                                 HYDRO_LOAD_AGGREGATION,
     +                                 OFF_PEAK_SPINNING_RESERVE,
     +                                 OFF_PEAK_SPINNING_UNITS,
     +                                 PRICE_CAP,
     +                                 MAX_HOURLY_TG_IMPORT,
     +                                 MAX_HOURLY_TG_EXPORT,
     +                                 PLANNING_AREA,
     +                                 SCENARIO_VARIABLE,
     +                                 END_NOX_YEAR,
     +                                 ST_LHS_FOR_PRICES,
! SPCapEx Variables place holders before any new SP variables
     +                                 LOAD_PLANNING_FACTOR, ! 58
     +                                 LOAD_FILE_FOR,  ! 59
     +                                 LOAD_SCENARIO_NUM, ! 60
     +                                 PRICE_FILE_FOR,
     +                                 PRICE_SCENARIO_NUM,
     +                                 ZONE_MAX_RESERVE,
     +                                 ZONE_TARGET_RESERVE,   ! 64
     +                                 CAPEX_COMMENT,  ! 65
     +                                 MARKET_AREA_LOAD_FILE_FOR,  ! 66
     +                              MARKET_AREA_LOAD_SCENARIO_NUM, ! 67
     +                                 PRE_DISPATCH_HYDRO,  ! 68
     +                                 AGGREGATE_THERMAL,   ! 69
     +                              MAXIMUM_AGGREGATION_INTERVALS, ! 70
     +                                 PRICE_ESCALATION_RATE, ! 71
     +                             MINIMUM_CAPACITY_TESTING_RATIO, ! 72
     +                             MAXIMUM_CAPACITY_TESTING_RATIO, ! 73
     +                                 SAVE_MRX_EXPANSION_PLAN, ! 74
     +                                 LONG_TRANS_GROUP_NAME,
     +                                 PRIMARY_STATE,
     +                                 PRIMARY_SUPERREGION,
     +                                 MRX_ICAP_UNIT_LINK,  ! 78
     +                                 PRICE_MINIMUM,
     +                                 REGIONAL_CAPACITY_MARKET
!
               IREC = IREC + 1
               WRITE(11,REC=IREC)      DELETE,
     +                                 GROUP_NAME,
     +                                 GROUP_ACTIVE,
     +                                 TRANSACTION_GROUP,
     +                                 BASECASE_MKT_AREA_ID_tf2,
     +                                 BASE_CASE_TRANS_AREA_ID,
     +                                 BASECASE_SUBREGION_ID,
     +                                 SPINNING_UNITS,
     +                                 SPINNING_RESERVE,
     +                                 COMMENT,
     +                                 MAX_HOURLY_RAMP_UP,
     +                                 MAX_HOURLY_RAMP_DOWN,
     +                                 FIRST_CAPACITY_VALUE,
     +                                 FIRST_CAPACITY_PERCENT,
     +                                 SECOND_CAPACITY_VALUE,
     +                                 SECOND_CAPACITY_PERCENT,
     +                                 THIRD_CAPACITY_VALUE,
     +                                 THIRD_CAPACITY_PERCENT,
     +                                 REPORT_CL_CAPACITY,
     +                                 ASSET_CLASS_ID,
     +                                 ASSET_CLASS_REV_ALLOC_VECTOR,
     +                                 TIME_ZONE,
     +                                 CAPACITY_ADDER,
     +                                 NOX_SEASON,
     +                                 PURCHASE_POWER_ASSIGN,
     +                                 PURCHASE_ASSET_CLASS_ID,
     +                                 PURCHASE_ASSET_ALLOC_VECTOR,
     +                                 CREATE_HOURLY_PRICE,
     +                                 HOURLY_PRICE_NAME,
     +                                 ADDITIONAL_CAPACITY_VALUE,
     +                                 ADDITIONAL_CAPACITY_PERCENT,
     +                                 RTO_GROUP,
     +                                 MRX_VOLATILITY_MULT,
     +                                 NOX_YEAR,
     +                                 NIGHT_SCARCITY_MULT,
     +                                 WEEKEND_SCARCITY_MULT,
     +                                 HYDRO_LOAD_AGGREGATION,
     +                                 OFF_PEAK_SPINNING_RESERVE,
     +                                 OFF_PEAK_SPINNING_UNITS,
     +                                 PRICE_CAP,
     +                                 MAX_HOURLY_TG_IMPORT,
     +                                 MAX_HOURLY_TG_EXPORT,
     +                                 PLANNING_AREA,
     +                                 SCENARIO_VARIABLE,
     +                                 END_NOX_YEAR,
     +                                 ST_LHS_FOR_PRICES,
     +                                 PRICE_ESCALATION_RATE, ! 71
     +                             MINIMUM_CAPACITY_TESTING_RATIO, ! 72
     +                             MAXIMUM_CAPACITY_TESTING_RATIO, ! 73
     +                                 SAVE_MRX_EXPANSION_PLAN, ! 74
     +                                 LONG_TRANS_GROUP_NAME,
     +                                 PRIMARY_STATE,
     +                                 PRIMARY_SUPERREGION,
     +                                 MRX_ICAP_UNIT_LINK,  ! 78
     +                                 PRICE_MINIMUM,
     +                                 REGIONAL_CAPACITY_MARKET
            ENDDO ! TRANSACTION GROUPS
            SAVE_TRANS_GROUPS_TABLES = SAVE_TRANS_GROUPS_TABLES + 1
            IF(IOS /= 0) EXIT
         ENDDO ! READ TABLES
         CLOSE(10)
      ELSE IF(INDEX(TRANS_GROUPS_FILE(),'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "tf_objt:1112")
      ELSE
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCTGROUP.BIN",
     +                     ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
!
         SAVE_TG_FILE_EXISTS = .FALSE.
!
      ENDIF
      SAVE_TRANS_GROUPS_RECORDS = IREC


      CLOSE(11)
      RETURN


!          ROUTINE TO CREATE OVERLAY FILES
!          COPYRIGHT (C) 1984-88  M.S. GERBER & ASSOCIATES, INC.
!          COPYRIGHT (C) 1991-92  M.S. GERBER & ASSOCIATES, INC.


! OVERLAY THE SYSTEM-FORECAST FILE

      ENTRY TG_MAKEOVL(OVERLAY_FAMILY_NAME)

      CALL LOCATE(10,51)
      CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
      FILE_NAME_OVL=trim(OUTPUT_DIRECTORY())//"TGO"//
     +                               trim(OVERLAY_FAMILY_NAME)//".DAT"
      OPEN(10,FILE=FILE_NAME_OVL)
      READ(10,*) DELETE
      INUNIT = 12
      IF(TGROUP_OL == 'BC') THEN
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCTGROUP.BIN",
     +                                      ACCESS="DIRECT",RECL=LRECL)
         INUNIT = 11
      ENDIF

      FILE_NAME = trim(OUTPUT_DIRECTORY())//"OLTGROUP.BIN"

      OPEN(12,FILE=FILE_NAME,ACCESS="DIRECT",
     +                          STATUS="UNKNOWN",RECL=LRECL,IOSTAT=IOS)
      IF(IOS /= 0) THEN
         CALL IOSTAT_MSG(IOS,MESSAGE)
         WRITE(4,*) trim(MESSAGE)
         WRITE(4,*) '*** line 4176 TF_OBJT.FOR ***'
         er_message='See WARNING MESSAGES -tf_objt.for-24'
         call end_program(er_message)
      ENDIF
      IREC = 0
      DELETE = 1
!
      READ(10,1000,IOSTAT=IOS) RECLN
      DOWHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE
!
         READ(10,1000,IOSTAT=IOS) RECLN
!
      ENDDO
!
      DO
         IREC = IREC + 1
         READ(INUNIT,REC=IREC,IOSTAT=IOS)  DELETE,
     +                                 GROUP_NAME,
     +                                 GROUP_ACTIVE,
     +                                 TRANSACTION_GROUP,
     +                                 BASECASE_MKT_AREA_ID_tf2,
     +                                 BASE_CASE_TRANS_AREA_ID,
     +                                 BASECASE_SUBREGION_ID,
     +                                 SPINNING_UNITS,
     +                                 SPINNING_RESERVE,
     +                                 COMMENT,
     +                                 MAX_HOURLY_RAMP_UP,
     +                                 MAX_HOURLY_RAMP_DOWN,
     +                                 FIRST_CAPACITY_VALUE,
     +                                 FIRST_CAPACITY_PERCENT,
     +                                 SECOND_CAPACITY_VALUE,
     +                                 SECOND_CAPACITY_PERCENT,
     +                                 THIRD_CAPACITY_VALUE,
     +                                 THIRD_CAPACITY_PERCENT,
     +                                 REPORT_CL_CAPACITY,
     +                                 ASSET_CLASS_ID,
     +                                 ASSET_CLASS_REV_ALLOC_VECTOR,
     +                                 TIME_ZONE,
     +                                 CAPACITY_ADDER,
     +                                 NOX_SEASON,
     +                                 PURCHASE_POWER_ASSIGN,
     +                                 PURCHASE_ASSET_CLASS_ID,
     +                                 PURCHASE_ASSET_ALLOC_VECTOR,
     +                                 CREATE_HOURLY_PRICE,
     +                                 HOURLY_PRICE_NAME,
     +                                 ADDITIONAL_CAPACITY_VALUE,
     +                                 ADDITIONAL_CAPACITY_PERCENT,
     +                                 RTO_GROUP,
     +                                 MRX_VOLATILITY_MULT,
     +                                 NOX_YEAR,
     +                                 NIGHT_SCARCITY_MULT,
     +                                 WEEKEND_SCARCITY_MULT,
     +                                 HYDRO_LOAD_AGGREGATION,
     +                                 OFF_PEAK_SPINNING_RESERVE,
     +                                 OFF_PEAK_SPINNING_UNITS,
     +                                 PRICE_CAP,
     +                                 MAX_HOURLY_TG_IMPORT,
     +                                 MAX_HOURLY_TG_EXPORT,
     +                                 PLANNING_AREA,
     +                                 SCENARIO_VARIABLE,
     +                                 END_NOX_YEAR,
     +                                 ST_LHS_FOR_PRICES,
     +                                 PRICE_ESCALATION_RATE, ! 71
     +                              MINIMUM_CAPACITY_TESTING_RATIO, ! 72
     +                              MAXIMUM_CAPACITY_TESTING_RATIO, ! 73
     +                                 SAVE_MRX_EXPANSION_PLAN, ! 74
     +                                 LONG_TRANS_GROUP_NAME,
     +                                 PRIMARY_STATE,
     +                                 PRIMARY_SUPERREGION,
     +                                 MRX_ICAP_UNIT_LINK,  ! 78
     +                                 PRICE_MINIMUM,
     +                                 REGIONAL_CAPACITY_MARKET
         IF(IOS /= 0) EXIT
!        READ(10,1000,IOSTAT=IOS) RECLN
!        IF(IOS == 0) THEN
         RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
         READ(RECLN,*,ERR=200)         DELETE,
     +                                 GROUP_NAME,
     +                                 GROUP_ACTIVE,
     +                                 TRANSACTION_GROUP,
     +                                 BASECASE_MKT_AREA_ID_tf2,
     +                                 BASE_CASE_TRANS_AREA_ID,
     +                                 BASECASE_SUBREGION_ID,
     +                                 SPINNING_UNITS,
     +                                 SPINNING_RESERVE,
     +                                 COMMENT,
     +                                 MAX_HOURLY_RAMP_UP,
     +                                 MAX_HOURLY_RAMP_DOWN,
     +                                 FIRST_CAPACITY_VALUE,
     +                                 FIRST_CAPACITY_PERCENT,
     +                                 SECOND_CAPACITY_VALUE,
     +                                 SECOND_CAPACITY_PERCENT,
     +                                 THIRD_CAPACITY_VALUE,
     +                                 THIRD_CAPACITY_PERCENT,
     +                                 REPORT_CL_CAPACITY,
     +                                 ASSET_CLASS_ID,
     +                                 ASSET_CLASS_REV_ALLOC_VECTOR,
     +                                 TIME_ZONE,
     +                                 CAPACITY_ADDER,
     +                                 NOX_SEASON,
     +                                 PURCHASE_POWER_ASSIGN,
     +                                 PURCHASE_ASSET_CLASS_ID,
     +                                 PURCHASE_ASSET_ALLOC_VECTOR,
     +                                 CREATE_HOURLY_PRICE,
     +                                 HOURLY_PRICE_NAME,
     +                                 ADDITIONAL_CAPACITY_VALUE,
     +                                 ADDITIONAL_CAPACITY_PERCENT,
     +                                 RTO_GROUP,
     +                                 MRX_VOLATILITY_MULT,
     +                                 NOX_YEAR,
     +                                 NIGHT_SCARCITY_MULT,
     +                                 WEEKEND_SCARCITY_MULT,
     +                                 HYDRO_LOAD_AGGREGATION,
     +                                 OFF_PEAK_SPINNING_RESERVE,
     +                                 OFF_PEAK_SPINNING_UNITS,
     +                                 PRICE_CAP,
     +                                 MAX_HOURLY_TG_IMPORT,
     +                                 MAX_HOURLY_TG_EXPORT,
     +                                 PLANNING_AREA,
     +                                 SCENARIO_VARIABLE,
     +                                 END_NOX_YEAR,
     +                                 ST_LHS_FOR_PRICES,
! SPCapEx Variables place holders
     +                                 LOAD_PLANNING_FACTOR, ! 58
     +                                 LOAD_FILE_FOR,  ! 59
     +                                 LOAD_SCENARIO_NUM, ! 60
     +                                 PRICE_FILE_FOR,
     +                                 PRICE_SCENARIO_NUM,
     +                                 ZONE_MAX_RESERVE,
     +                                 ZONE_TARGET_RESERVE,   ! 64
     +                                 COMMENT,  ! 65
     +                                 MARKET_AREA_LOAD_FILE_FOR,  ! 66
     +                              MARKET_AREA_LOAD_SCENARIO_NUM, ! 67
     +                                 PRE_DISPATCH_HYDRO,  ! 68
     +                                 AGGREGATE_THERMAL,   ! 69
     +                              MAXIMUM_AGGREGATION_INTERVALS, ! 70
     +                                 PRICE_ESCALATION_RATE, ! 71
     +                              MINIMUM_CAPACITY_TESTING_RATIO, ! 72
     +                              MAXIMUM_CAPACITY_TESTING_RATIO, ! 73
     +                                 SAVE_MRX_EXPANSION_PLAN, ! 74
     +                                 LONG_TRANS_GROUP_NAME,
     +                                 PRIMARY_STATE,
     +                                 PRIMARY_SUPERREGION,
     +                                 MRX_ICAP_UNIT_LINK,  ! 78
     +                                 PRICE_MINIMUM,
     +                                 REGIONAL_CAPACITY_MARKET

         READ(10,1000,IOSTAT=IOS) RECLN
         DOWHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE
!
            READ(10,1000,IOSTAT=IOS) RECLN
!
         ENDDO
!         ENDIF
!
         WRITE(12,REC=IREC)            DELETE,
     +                                 GROUP_NAME,
     +                                 GROUP_ACTIVE,
     +                                 TRANSACTION_GROUP,
     +                                 BASECASE_MKT_AREA_ID_tf2,
     +                                 BASE_CASE_TRANS_AREA_ID,
     +                                 BASECASE_SUBREGION_ID,
     +                                 SPINNING_UNITS,
     +                                 SPINNING_RESERVE,
     +                                 COMMENT,
     +                                 MAX_HOURLY_RAMP_UP,
     +                                 MAX_HOURLY_RAMP_DOWN,
     +                                 FIRST_CAPACITY_VALUE,
     +                                 FIRST_CAPACITY_PERCENT,
     +                                 SECOND_CAPACITY_VALUE,
     +                                 SECOND_CAPACITY_PERCENT,
     +                                 THIRD_CAPACITY_VALUE,
     +                                 THIRD_CAPACITY_PERCENT,
     +                                 REPORT_CL_CAPACITY,
     +                                 ASSET_CLASS_ID,
     +                                 ASSET_CLASS_REV_ALLOC_VECTOR,
     +                                 TIME_ZONE,
     +                                 CAPACITY_ADDER,
     +                                 NOX_SEASON,
     +                                 PURCHASE_POWER_ASSIGN,
     +                                 PURCHASE_ASSET_CLASS_ID,
     +                                 PURCHASE_ASSET_ALLOC_VECTOR,
     +                                 CREATE_HOURLY_PRICE,
     +                                 HOURLY_PRICE_NAME,
     +                                 ADDITIONAL_CAPACITY_VALUE,
     +                                 ADDITIONAL_CAPACITY_PERCENT,
     +                                 RTO_GROUP,
     +                                 MRX_VOLATILITY_MULT,
     +                                 NOX_YEAR,
     +                                 NIGHT_SCARCITY_MULT,
     +                                 WEEKEND_SCARCITY_MULT,
     +                                 HYDRO_LOAD_AGGREGATION,
     +                                 OFF_PEAK_SPINNING_RESERVE,
     +                                 OFF_PEAK_SPINNING_UNITS,
     +                                 PRICE_CAP,
     +                                 MAX_HOURLY_TG_IMPORT,
     +                                 MAX_HOURLY_TG_EXPORT,
     +                                 PLANNING_AREA,
     +                                 SCENARIO_VARIABLE,
     +                                 END_NOX_YEAR,
     +                                 ST_LHS_FOR_PRICES,
     +                                 PRICE_ESCALATION_RATE, ! 71
     +                              MINIMUM_CAPACITY_TESTING_RATIO, ! 72
     +                              MAXIMUM_CAPACITY_TESTING_RATIO, ! 73
     +                                 SAVE_MRX_EXPANSION_PLAN, ! 74
     +                                 LONG_TRANS_GROUP_NAME,
     +                                 PRIMARY_STATE,
     +                                 PRIMARY_SUPERREGION,
     +                                 MRX_ICAP_UNIT_LINK,  ! 78
     +                                 PRICE_MINIMUM,
     +                                 REGIONAL_CAPACITY_MARKET
      ENDDO
      IF(IREC-1 /= SAVE_TRANS_GROUPS_RECORDS) THEN
         WRITE(4,*) "TRANSACTION GROUP OVERLAY DIFFERENT LENGTH"
         WRITE(4,*) "THAN THE BASE FILE. OVERLAY MUST BE THE SAME"
         WRITE(4,*) "LENGTH. ",TRIM(FILE_NAME_OVL)
      ENDIF
      CLOSE(10)
      CLOSE(12)
      IF(TGROUP_OL == 'BC') CLOSE(11)
      TGROUP_OL = 'OL'
      RETURN


      ENTRY RESET_TGROUP_OL

         TGROUP_OL = 'BC'
      RETURN


      ENTRY RETURN_TGROUP_OL(R_TGROUP_OL)

         R_TGROUP_OL = TGROUP_OL
      RETURN

      ENTRY DOES_TG_FILE_EXIST(R_TG_FILE_EXISTS)

         R_TG_FILE_EXISTS = SAVE_TG_FILE_EXISTS
      RETURN

      ENTRY GET_TRANS_GROUPS_TABLES(R_TRANS_GROUPS_TABLES)

         R_TRANS_GROUPS_TABLES = SAVE_TRANS_GROUPS_TABLES
      RETURN

      ENTRY GET_TRANS_GROUPS_RECORDS(R_TRANS_GROUPS_RECORDS)

         R_TRANS_GROUPS_RECORDS = SAVE_TRANS_GROUPS_RECORDS
      RETURN

      ENTRY OPEN_TG_FILE

         OPEN(UNIT_NUM,FILE=trim(OUTPUT_DIRECTORY())//TGROUP_OL//
     +        "TGROUP.BIN",ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      RETURN
!

      ENTRY CLOSE_TG_FILE



         CLOSE(UNIT_NUM)
      RETURN
!

  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from tf_objt SIID289'
      call end_program(er_message)
!
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END

      FUNCTION READ_TRANS_GROUPS_DATA()

      use spindriftlib
      use prod_arrays_dimensions
      use tf_decs
      USE TRANS_GROUP_VARIABLES
      USE GRX_PLANNING_ROUTINES
      use logging
      use tf_decs
      use cla2_decs
      use debugtrace
      implicit none
      integer :: file_trace_gtgp=0
      integer (kind=4) :: gtgp_count=0
      
      integer (kind=2) :: GET_CAPACITY_MARKET_POSITION ! External
      integer (kind=2) :: GET_PLANNING_AREA_POSITION ! External
      integer (kind=2) :: GET_MAX_TRANS_GROUP_NUMBER ! External


      SAVE

      integer  :: timescalled=0, gtgi_numcalls=0

      LOGICAL*1 SAVE_TG_FILE_EXISTS/.FALSE./
      INTEGER*4 VALUES_2_ZERO
      character*512 :: tmp,tmp2
      INTEGER*2 DELETE,CURRENT_RECORD,TRANS_GROUP,GET_TRANS_GROUPS,
     +          TRANS_GROUPS_RECORDS,R_TRANS_GROUP,
     +          R_ASSET_CLASS,
     +          SAVE_TRANS_GROUPS_RECORDS/0/,
     +          TEMP_I,I,HG,PA,
     +          FIRST_TRANSACT_REPORTING_GROUP,
     +          FIRST_REPORTING_GROUP/0/,
     +          REGIONAL_CAPACITY_MARKET(:)
      LOGICAL*1 READ_TRANS_GROUPS_DATA,TRANS_GROUP_ACTIVE_SWITCH,
     +          MOD_TRANS_GROUP_ACTIVE_SWITCH,
     +          GET_SCARCITY_INFO,GET_TRANS_SPINNING_CAPACITY,
     +          GET_TRANS_PRICE_CAPS,
     +          GET_TRANS_PRICE_MINIMUM,
     +          GET_OFF_PEAK_SPINNING_CAPACITY,
     +          GET_TRANS_RAMP_RATES,
     +          GET_ONE_TRANS_RAMP_RATES,
     +          GET_TRANS_MAX_IMPORT_EXPORT,
     +          HYDRO_AGGREGATION/.FALSE./,
     +          PLANNING_AREA/.FALSE./,
     +          CAPACITY_MARKET/.FALSE./,
     +          GET_TG_PRICE_MULT,
     +          GET_ONE_TG_PRICE_MULT
      LOGICAL*1 TG_FILE_EXISTS
      CHARACTER*(*) R_GET_GROUP_NAME
      CHARACTER*3 HOURLY_PRICE_NAME(:)
      CHARACTER*3 R_GET_HOURLY_PRICE_NAME
      CHARACTER*(*) R_GET_TRANSACTION_GROUP_NAME
      CHARACTER*40 SCENARIO_VARIABLE
      LOGICAL*1 GET_TF_HOURLY_PRICE_NAME,GET_TF_GROUP_NAME,
     +          GET_TF_TRANSACTION_GROUP_NAME,
     +          GET_TF_TRANS_GROUP_NOX_SEASON
      integer (kind=2) ::  R_TG
!
! SIMULATION VARIABLES
!
      CHARACTER*35 GROUP_NAME(:)
      CHARACTER (len=1),  allocatable :: GROUP_ACTIVE(:),
     + SPINNING_UNITS(:),
     +            OFF_PEAK_SPINNING_UNITS(:),
     +            REPORT_CL_CAPACITY(:),TIME_ZONE(:),
     +            NOX_SEASON(:),
     +            PURCHASE_POWER_ASSIGN(:),
     +            CREATE_HOURLY_PRICE(:)

      character (len=1) :: R_PURCHASE_POWER_ASSIGN,
     + R_GET_TRANS_GROUP_NOX_SEASON
      LOGICAL*1 GET_PURCHASE_POWER_ASSIGN
      LOGICAL*1   GET_REPORT_CL_CAPACITY,
     +            GET_CREATE_HOURLY_PRICE,
     +            GET_ST_LHS_FOR_PRICES,
     +            GET_BASECASE_MARKET_AREA_ID

      integer (kind=2) :: ! Externals
     +            GET_SCENARIO_INDEX,
     +            GET_NUM_CREATE_HOURLY_PRICE,
     +            GET_CREATE_HOURLY_PRICE_INDEX,
     +            GET_ASSET_CLASS_2_TG,
     +            GET_TRANS_GROUP_NOX_YEAR,
     +            GET_TRANS_GROUP_END_NOX_YEAR,
     +            GET_NUMBER_OF_PLANNING_GROUPS,
     +            GET_NUMBER_OF_CAPACITY_MARKETS,
     +            GET_NUMBER_OF_HYDRO_GROUPS,
     +            GET_HG_FROM_TG,
     +            GET_PA_FROM_TG,
     +            GET_CM_FROM_TG,
     +            GET_PA_VALUE_FROM_TG,
     +            GET_CM_INDEX_FROM_TG,
     +            GET_NUMBER_OF_ACTIVE_GROUPS,
     +            GET_TRANS_GROUP_INDEX,
     +            GET_TRANS_GROUP_POSITION,
     +            GET_MAX_TRANS_GROUP_NUMBER
     +            GET_CAPACITY_MARKET_POSITION,
     +            GET_PURCHASE_ASSET_CLASS_ID,
     +            GET_AC_FOR_TG

      integer (kind=2) ::
     +            R_NUMBER_OF_TRANS_GROUPS,
     +            TRANS,R_MONTH,TG,R_YEAR,
     +            R_ON_OR_OFF_PEAK,
     +            DAY_TYPE,DATA_BASE,GET_DATA_BASE_FOR_TRANS,
     +            FUNCTION_HOLDER2, GET_MAX_TRANS_GROUPS,
     +            AC

      integer (kind=2) :: NUMBER_OF_ACTIVE_GROUPS

      integer (kind=2), parameter :: MAX_TRANS_GROUP_INDEX=256
      integer (kind=2)  ::
     +            SCENARIO_INDEX=0,
     +            NUM_CREATE_HOURLY_PRICE=0,
     +            MAX_ASSET_GROUPS=0,
     +            NUMBER_OF_HYDRO_GROUPS=0,
     +            NUMBER_OF_PLANNING_GROUPS=0,
     +            NUMBER_OF_CAPACITY_MARKETS=0,
     +            MAX_TRANS_GROUP_NUMBER=0,
     +            MAX_ASSET_CLASS_GROUPS=0,
     +            GET_CREATE_HOURLY_PRICE_POS



      INTEGER (kind=2), allocatable, save  ::  TRANSACTION_GROUP(:),
     +            TG_SCENARIO_VARIABLE_INDEX(:),
     +            CREATE_HOURLY_PRICE_INDEX(:),
     +            CREATE_HOURLY_PRICE_POS(:),
     +            HYDRO_GROUP_2_TG(:),
     +            TRANS_GROUP_POSITION(:),
     +            TG_2_HYDRO_GROUP(:),
     +            TG_2_PLANNING_AREA(:),
     +            TG_2_CAPACITY_MARKET(:),
     +            HYDRO_AGGREGATION_POSITION(:),
     +            HYDRO_AGGREGATION_INDEX(:),
     +            PLANNING_AREA_POSITION(:),
     +            CAPACITY_MARKET_POSITION(:),
     +            PLANNING_AREA_INDEX(:),
     +            CAPACITY_MARKET_INDEX(:),
     +            ASSET_CLASS_ID(:),
     +            ASSET_CLASS_REV_ALLOC_VECTOR(:),
     +            ASSET_CLASS_GROUPS_INDEX(:),
     +            ASSET_CLASS_2_TG(:),
     +            ASSET_CLASS_GROUP_2_AC(:),
     +            RTO_GROUP(:),
     +            NOX_YEAR(:),
     +            END_NOX_YEAR(:),
     +            HYDRO_LOAD_AGGREGATION(:),
     +            REGIONAL_PLANNING_AREA(:),
     +            PURCHASE_ASSET_CLASS_ID(:),
     +            PURCHASE_ASSET_ALLOC_VECTOR(:)

      integer (kind=2), allocatable :: TRANS_GROUP_INDEX(:)




      CHARACTER*6 BASECASE_MKT_AREA_ID_tf3(:),
     +            BASE_CASE_TRANS_AREA_ID(:),BASECASE_SUBREGION_ID(:),
     +            R_BASECASE_MARKET_AREA_ID,
     +            PRIMARY_STATE,
     +            PRIMARY_SUPERREGION
      CHARACTER*2 ST_LHS_FOR_PRICES(:),
     +            R_ST_LHS_FOR_PRICES
      INTEGER*2 BELONGS_TO_GROUP(0:MAX_TRANS_GROUP_INDEX),
     +            GET_BELONGS_TO_GROUP,
     +            R_PRICE_VARIABLE(*),
     +            R_ONE_PRICE_VARIABLE,
     +            GET_MRX_ICAP_UNIT_LINK
      REAL*4 SPINNING_RESERVE(:),
     +       OFF_PEAK_SPINNING_RESERVE(:),
     +       MAX_HOURLY_RAMP_UP(:),
     +       MAX_HOURLY_RAMP_DOWN(:),
     +       FIRST_CAPACITY_VALUE(:),
     +       FIRST_CAPACITY_PERCENT(:),
     +       SECOND_CAPACITY_VALUE(:),
     +       SECOND_CAPACITY_PERCENT(:),
     +       THIRD_CAPACITY_VALUE(:),
     +       THIRD_CAPACITY_PERCENT(:),
     +       CAPACITY_ADDER(:),
     +       ADDITIONAL_CAPACITY_VALUE(:,:),
     +       ADDITIONAL_CAPACITY_PERCENT(:,:),
     +       MRX_VOLATILITY_MULT(:),
     +       NIGHT_SCARCITY_MULT(:),
     +       WEEKEND_SCARCITY_MULT(:),
     +       PRICE_CAP(:),
     +       PRICE_MINIMUM(:),
     +       MAX_HOURLY_TG_IMPORT(:),
     +       MAX_HOURLY_TG_EXPORT(:),
     +       GET_NIGHT_SCARCITY_MULT,
     +       GET_WEEKEND_SCARCITY_MULT,
     +       R_ADDITIONAL_VALUE(7),
     +       R_ADDITIONAL_PERCENT(7),
     +       R_FIRST_CAP,
     +       R_FIRST_PERCENT,
     +       R_SECOND_CAP,
     +       R_SECOND_PERCENT,
     +       R_THIRD_CAP,
     +       R_THIRD_PERCENT,
     +       R_CAPACITY_ADDER,
     +       GET_DAILY_PEAK_SPIN,
     +       R_CURRENT_SPIN,
     +       R_DAILY_PEAK,
     +       R_CAPACITY(*),
     +       TEMP_CAPACITY,
     +       ESCALATED_MONTHLY_VALUE,
     +       R_RAMP_UP(*),
     +       R_RAMP_DOWN(*),
     +       R_PRICE_MULT(*),
     +       R_ONE_PRICE_MULT,
     +       R_RAMP_ONE_UP,
     +       R_RAMP_ONE_DOWN,
     +       GET_SCENARIO_BY_INDEX,
     +       GET_TRANS_GROUP_PEAK,
!     +       TRANS_GROUP_CAP,
     +       GLOBAL_SCARCITY,
     +       GET_GLOBAL_SCARCITY,
     +       GET_MRX_VOLATILITY_MULT,
     +       GET_VAR,
     +       GET_OFF_PEAK_SPIN_FOR_TG,
     +       GET_TRANS_SPIN_FOR_TG
      REAL*8 TRANS_GROUP_CAP
      CHARACTER*50 COMMENT
      ALLOCATABLE ::
     +     GROUP_NAME,
     +     BASECASE_MKT_AREA_ID_tf3,
     +     BASE_CASE_TRANS_AREA_ID,
     +     BASECASE_SUBREGION_ID,
     +     SPINNING_RESERVE,
     +     OFF_PEAK_SPINNING_RESERVE,
     +     MAX_HOURLY_RAMP_UP,
     +     MAX_HOURLY_RAMP_DOWN,
     +     FIRST_CAPACITY_VALUE,
     +     FIRST_CAPACITY_PERCENT,
     +     SECOND_CAPACITY_VALUE,
     +     SECOND_CAPACITY_PERCENT,
     +     THIRD_CAPACITY_VALUE,
     +     THIRD_CAPACITY_PERCENT,
     +     ADDITIONAL_CAPACITY_VALUE,
     +     ADDITIONAL_CAPACITY_PERCENT,
     +     ST_LHS_FOR_PRICES,
     +     MRX_VOLATILITY_MULT,
     +     NIGHT_SCARCITY_MULT,
     +     WEEKEND_SCARCITY_MULT,
     +     PRICE_CAP,
     +     PRICE_MINIMUM,
     +     REGIONAL_CAPACITY_MARKET,
     +     MAX_HOURLY_TG_IMPORT,
     +     MAX_HOURLY_TG_EXPORT,
     +     CAPACITY_ADDER,
     +     HOURLY_PRICE_NAME
      REAL*4 R_TRANS_CAP,R_TRANS_MAX_CAP
      REAL*4 TRANS_GROUP_SCARCITY_VALUE
      REAL*4 SCARCITY_VALUES(10),
     +       SCARCITY_CAP_PERCENT(10),
     +       TOTAL_SCARCITY_CAPACITY,SLOPE,
     +       SCARCITY_CAPACITY_ADDER,
     +       NEW_SCARCITY_VALUES(7),
     +       NEW_SCARCITY_CAP_PERCENT(7),
     +       SCARCITY_VALUES_1,
     +       SCARCITY_CAP_PERCENT_1,
     +       SCARCITY_VALUES_2,
     +       SCARCITY_CAP_PERCENT_2,
     +       SCARCITY_VALUES_3,
     +       SCARCITY_CAP_PERCENT_3,
     +       GET_MIN_CAP_TESTING_RATIO,
     +       GET_MAX_CAP_TESTING_RATIO
      LOGICAL*1 VOID_LOGICAL,STORE_TG_SCARCITY_INFO
      REAL*4 TRANS_GLOBAL_SCARCITY_VALUE
      INTEGER VALUES_2_SET
      CHARACTER*60 TEMP_LONG_TRANS_GROUP_NAME

!
!
! END DATA DECLARATIONS
!
         READ_TRANS_GROUPS_DATA = .FALSE.
         SAVE_TG_FILE_EXISTS = .FALSE.
!
!
         CALL DOES_TG_FILE_EXIST(TG_FILE_EXISTS)
!
! DEFAULT VALUE. E.G. 25. DOES NOT IMPACT MAX_TRANS_GROUP_INDEX BELOW.
!
! 1/18/99. GAT. OUT.
!

!
         IF(.NOT. TG_FILE_EXISTS) RETURN
!
! 256 FOR NATIONAL DATA BASE
!
!         MAX_TRANS_GROUPS = GET_MAX_TRANS_GROUPS()
!
         IF(ALLOCATED(TRANS_GROUP_INDEX))
     +               DEALLOCATE(TRANS_GROUP_INDEX,
     +                     TG_SCENARIO_VARIABLE_INDEX,
     +                     HYDRO_GROUP_2_TG,
     +                     TRANS_GROUP_POSITION,
     +                     TG_2_HYDRO_GROUP,
     +                     TG_2_PLANNING_AREA,
     +                     TG_2_CAPACITY_MARKET,
     +                     HYDRO_AGGREGATION_POSITION,
     +                     HYDRO_AGGREGATION_INDEX,
     +                     PLANNING_AREA_POSITION,
     +                     CAPACITY_MARKET_POSITION,
     +                     PLANNING_AREA_INDEX,
     +                     CAPACITY_MARKET_INDEX,
     +                     ASSET_CLASS_GROUPS_INDEX,
     +                     ASSET_CLASS_2_TG,
     +                     ASSET_CLASS_GROUP_2_AC,
     +                     ns_tf_decs%report_this_group)
         ALLOCATE(TRANS_GROUP_INDEX(0:MAX_TRANS_GROUP_INDEX))
         ALLOCATE(TG_SCENARIO_VARIABLE_INDEX(0:MAX_TRANS_GROUP_INDEX))
         ALLOCATE(HYDRO_GROUP_2_TG(0:MAX_TRANS_GROUP_INDEX))

         ALLOCATE(TRANS_GROUP_POSITION(0:MAX_TRANS_GROUP_INDEX))
         ALLOCATE(ns_tf_decs%report_this_group(0:MAX_TRANS_GROUP_INDEX))
         ALLOCATE(TG_2_HYDRO_GROUP(0:MAX_TRANS_GROUP_INDEX))
         ALLOCATE(TG_2_PLANNING_AREA(0:MAX_TRANS_GROUP_INDEX))
         ALLOCATE(TG_2_CAPACITY_MARKET(0:MAX_TRANS_GROUP_INDEX))
         ALLOCATE(HYDRO_AGGREGATION_POSITION(0:MAX_TRANS_GROUP_INDEX))
         ALLOCATE(HYDRO_AGGREGATION_INDEX(0:MAX_TRANS_GROUP_INDEX))
         ALLOCATE(PLANNING_AREA_POSITION(0:MAX_TRANS_GROUP_INDEX))
         ALLOCATE(CAPACITY_MARKET_POSITION(0:MAX_TRANS_GROUP_INDEX))
         ALLOCATE(PLANNING_AREA_INDEX(0:MAX_TRANS_GROUP_INDEX))
         ALLOCATE(CAPACITY_MARKET_INDEX(0:MAX_TRANS_GROUP_INDEX))
         ALLOCATE(ASSET_CLASS_GROUPS_INDEX(0:MAX_ASSET_GROUPS))
         ALLOCATE(ASSET_CLASS_2_TG(0:MAX_ASSET_GROUPS))
         ALLOCATE(ASSET_CLASS_GROUP_2_AC(0:MAX_TRANS_GROUP_INDEX))

         TRANS_GROUP_INDEX = 0
         TG_SCENARIO_VARIABLE_INDEX = 0
         HYDRO_GROUP_2_TG = 0
         TRANS_GROUP_POSITION = 0
         TG_2_HYDRO_GROUP = 0
         TG_2_PLANNING_AREA = 0
         TG_2_CAPACITY_MARKET = 0
         HYDRO_AGGREGATION_POSITION = 0
         HYDRO_AGGREGATION_INDEX = 0
         PLANNING_AREA_POSITION = 0
         CAPACITY_MARKET_POSITION = 0
         PLANNING_AREA_INDEX = 0
         CAPACITY_MARKET_INDEX = 0
         ASSET_CLASS_GROUPS_INDEX = 0
         ASSET_CLASS_2_TG = 0
         ASSET_CLASS_GROUP_2_AC = 0
!
         CALL GET_TRANS_GROUPS_RECORDS(TRANS_GROUPS_RECORDS)
         SAVE_TRANS_GROUPS_RECORDS = TRANS_GROUPS_RECORDS
!
         CALL OPEN_TG_FILE
!
         IF(ALLOCATED(GROUP_NAME)) DEALLOCATE(
     +                                 GROUP_NAME,
     +                                 GROUP_ACTIVE,
     +                                 TRANSACTION_GROUP,
     +                                 TG_BASECASE_MARKET_AREA_ID,
     +                                 TRANS_GROUP_FULL_NAME,
     +                                 BASECASE_MKT_AREA_ID_tf3,
     +                                 BASE_CASE_TRANS_AREA_ID,
     +                                 BASECASE_SUBREGION_ID,
     +                                 SPINNING_UNITS,
     +                                 OFF_PEAK_SPINNING_UNITS,
     +                                 SPINNING_RESERVE,
     +                                 OFF_PEAK_SPINNING_RESERVE,
     +                                 MAX_HOURLY_RAMP_UP,
     +                                 MAX_HOURLY_RAMP_DOWN,
     +                                 FIRST_CAPACITY_VALUE,
     +                                 FIRST_CAPACITY_PERCENT,
     +                                 SECOND_CAPACITY_VALUE,
     +                                 SECOND_CAPACITY_PERCENT,
     +                                 THIRD_CAPACITY_VALUE,
     +                                 THIRD_CAPACITY_PERCENT,
     +                                 ADDITIONAL_CAPACITY_VALUE,
     +                                 ADDITIONAL_CAPACITY_PERCENT,
     +                                 RTO_GROUP,
     +                                 NOX_YEAR,
     +                                 END_NOX_YEAR,
     +                                 ST_LHS_FOR_PRICES,
     +                                 MRX_VOLATILITY_MULT,
     +                                 REPORT_CL_CAPACITY,
!     +                                 ns_tf_decs%report_this_group,
     +                                 ASSET_CLASS_ID,
     +                                 ASSET_CLASS_REV_ALLOC_VECTOR,
     +                                 TIME_ZONE,
     +                                 CAPACITY_ADDER,
     +                                 NOX_SEASON,
     +                                 NIGHT_SCARCITY_MULT,
     +                                 WEEKEND_SCARCITY_MULT,
     +                                 PRICE_CAP,
     +                                 PRICE_MINIMUM,
     +                                 REGIONAL_CAPACITY_MARKET,
     +                                 MAX_HOURLY_TG_IMPORT,
     +                                 MAX_HOURLY_TG_EXPORT,
     +                                 HYDRO_LOAD_AGGREGATION,
     +                                 REGIONAL_PLANNING_AREA,
     +                                 PURCHASE_POWER_ASSIGN,
     +                                 CREATE_HOURLY_PRICE,
     +                                 CREATE_HOURLY_PRICE_INDEX,
     +                                 CREATE_HOURLY_PRICE_POS,
     +                                 HOURLY_PRICE_NAME,
     +                                 PURCHASE_ASSET_CLASS_ID,
     +                                 PURCHASE_ASSET_ALLOC_VECTOR)
         ALLOCATE(GROUP_NAME(0:TRANS_GROUPS_RECORDS))
         GROUP_NAME(0) = "All TGroups"
         ALLOCATE(GROUP_ACTIVE(TRANS_GROUPS_RECORDS))
         ALLOCATE(TRANSACTION_GROUP(TRANS_GROUPS_RECORDS))
         ALLOCATE(TRANS_GROUP_FULL_NAME(TRANS_GROUPS_RECORDS))
         TRANS_GROUP_FULL_NAME = ' '
         ALLOCATE(TG_BASECASE_MARKET_AREA_ID(TRANS_GROUPS_RECORDS))
         ALLOCATE(BASECASE_MKT_AREA_ID_tf3(TRANS_GROUPS_RECORDS))
         ALLOCATE(BASE_CASE_TRANS_AREA_ID(TRANS_GROUPS_RECORDS))
         ALLOCATE(BASECASE_SUBREGION_ID(TRANS_GROUPS_RECORDS))
         ALLOCATE(SPINNING_UNITS(TRANS_GROUPS_RECORDS))
         ALLOCATE(OFF_PEAK_SPINNING_UNITS(TRANS_GROUPS_RECORDS))
         ALLOCATE(SPINNING_RESERVE(TRANS_GROUPS_RECORDS))
         ALLOCATE(OFF_PEAK_SPINNING_RESERVE(TRANS_GROUPS_RECORDS))
         ALLOCATE(MAX_HOURLY_RAMP_UP(TRANS_GROUPS_RECORDS))
         ALLOCATE(MAX_HOURLY_RAMP_DOWN(TRANS_GROUPS_RECORDS))
         ALLOCATE(FIRST_CAPACITY_VALUE(0:TRANS_GROUPS_RECORDS))
         ALLOCATE(FIRST_CAPACITY_PERCENT(0:TRANS_GROUPS_RECORDS))
         ALLOCATE(SECOND_CAPACITY_VALUE(0:TRANS_GROUPS_RECORDS))
         ALLOCATE(SECOND_CAPACITY_PERCENT(0:TRANS_GROUPS_RECORDS))
         ALLOCATE(THIRD_CAPACITY_VALUE(0:TRANS_GROUPS_RECORDS))
         ALLOCATE(THIRD_CAPACITY_PERCENT(0:TRANS_GROUPS_RECORDS))
         ALLOCATE(ADDITIONAL_CAPACITY_VALUE(0:TRANS_GROUPS_RECORDS,7))
        ALLOCATE(ADDITIONAL_CAPACITY_PERCENT(0:TRANS_GROUPS_RECORDS,7))
         ALLOCATE(RTO_GROUP(TRANS_GROUPS_RECORDS))
         ALLOCATE(NOX_YEAR(TRANS_GROUPS_RECORDS))
         ALLOCATE(END_NOX_YEAR(TRANS_GROUPS_RECORDS))
         ALLOCATE(ST_LHS_FOR_PRICES(TRANS_GROUPS_RECORDS))
         ALLOCATE(MRX_VOLATILITY_MULT(TRANS_GROUPS_RECORDS))
         ALLOCATE(NIGHT_SCARCITY_MULT(TRANS_GROUPS_RECORDS))
         ALLOCATE(WEEKEND_SCARCITY_MULT(TRANS_GROUPS_RECORDS))
         ALLOCATE(PRICE_CAP(TRANS_GROUPS_RECORDS))
         ALLOCATE(PRICE_MINIMUM(TRANS_GROUPS_RECORDS))
         ALLOCATE(REGIONAL_CAPACITY_MARKET(TRANS_GROUPS_RECORDS))
         ALLOCATE(MAX_HOURLY_TG_IMPORT(TRANS_GROUPS_RECORDS))
         ALLOCATE(MAX_HOURLY_TG_EXPORT(TRANS_GROUPS_RECORDS))
         ALLOCATE(HYDRO_LOAD_AGGREGATION(TRANS_GROUPS_RECORDS))
         ALLOCATE(REGIONAL_PLANNING_AREA(TRANS_GROUPS_RECORDS))
         ALLOCATE(REPORT_CL_CAPACITY(TRANS_GROUPS_RECORDS))
         ALLOCATE(ASSET_CLASS_ID(TRANS_GROUPS_RECORDS))
         ALLOCATE(ASSET_CLASS_REV_ALLOC_VECTOR(TRANS_GROUPS_RECORDS))
         ALLOCATE(TIME_ZONE(TRANS_GROUPS_RECORDS))
         ALLOCATE(CAPACITY_ADDER(TRANS_GROUPS_RECORDS))
         ALLOCATE(NOX_SEASON(TRANS_GROUPS_RECORDS))
         ALLOCATE(PURCHASE_POWER_ASSIGN(TRANS_GROUPS_RECORDS))
         ALLOCATE(CREATE_HOURLY_PRICE(TRANS_GROUPS_RECORDS))
         ALLOCATE(CREATE_HOURLY_PRICE_INDEX(TRANS_GROUPS_RECORDS))
         ALLOCATE(CREATE_HOURLY_PRICE_POS(TRANS_GROUPS_RECORDS))
         ALLOCATE(HOURLY_PRICE_NAME(TRANS_GROUPS_RECORDS))
         ALLOCATE(PURCHASE_ASSET_CLASS_ID(TRANS_GROUPS_RECORDS))
         ALLOCATE(PURCHASE_ASSET_ALLOC_VECTOR(TRANS_GROUPS_RECORDS))
! MOVED 5/8/00
         IF(ALLOCATED(PRICE_ESCALATION_RATE))
     +                      DEALLOCATE(PRICE_ESCALATION_RATE,
     +                              MINIMUM_CAPACITY_TESTING_RATIO, ! 72
     +                              MAXIMUM_CAPACITY_TESTING_RATIO, ! 73
     +                                 SAVE_MRX_EXPANSION_PLAN,
     +                                 LONG_TRANS_GROUP_NAME,
     +                                 MRX_ICAP_UNIT_LINK) ! 74
         ALLOCATE(PRICE_ESCALATION_RATE(TRANS_GROUPS_RECORDS),
     +        MINIMUM_CAPACITY_TESTING_RATIO(TRANS_GROUPS_RECORDS), ! 72
     +        MAXIMUM_CAPACITY_TESTING_RATIO(TRANS_GROUPS_RECORDS), ! 73
     +            SAVE_MRX_EXPANSION_PLAN(TRANS_GROUPS_RECORDS),
     +            LONG_TRANS_GROUP_NAME(TRANS_GROUPS_RECORDS),
     +            MRX_ICAP_UNIT_LINK(TRANS_GROUPS_RECORDS)) ! 74
         PRICE_ESCALATION_RATE = 0.
!
         FIRST_CAPACITY_VALUE = 0.
         FIRST_CAPACITY_PERCENT = 0.
         SECOND_CAPACITY_VALUE = 0.
         SECOND_CAPACITY_PERCENT = 0.
         THIRD_CAPACITY_VALUE = 0.
         THIRD_CAPACITY_PERCENT = 0.
         CAPACITY_ADDER = 0.
         ADDITIONAL_CAPACITY_VALUE = 0.
         ADDITIONAL_CAPACITY_PERCENT = 0.
!
         BELONGS_TO_GROUP = 0
!
         GLOBAL_SCARCITY = GET_GLOBAL_SCARCITY(
     +                           FIRST_CAPACITY_VALUE(0),
     +                           SECOND_CAPACITY_VALUE(0),
     +                           THIRD_CAPACITY_VALUE(0),
     +                           FIRST_CAPACITY_PERCENT(0),
     +                           SECOND_CAPACITY_PERCENT(0),
     +                           THIRD_CAPACITY_PERCENT(0),
     +                           ADDITIONAL_CAPACITY_VALUE(0,1),
     +                           ADDITIONAL_CAPACITY_VALUE(0,2),
     +                           ADDITIONAL_CAPACITY_VALUE(0,3),
     +                           ADDITIONAL_CAPACITY_VALUE(0,4),
     +                           ADDITIONAL_CAPACITY_VALUE(0,5),
     +                           ADDITIONAL_CAPACITY_VALUE(0,6),
     +                           ADDITIONAL_CAPACITY_VALUE(0,7),
     +                           ADDITIONAL_CAPACITY_PERCENT(0,1),
     +                           ADDITIONAL_CAPACITY_PERCENT(0,2),
     +                           ADDITIONAL_CAPACITY_PERCENT(0,3),
     +                           ADDITIONAL_CAPACITY_PERCENT(0,4),
     +                           ADDITIONAL_CAPACITY_PERCENT(0,5),
     +                           ADDITIONAL_CAPACITY_PERCENT(0,6),
     +                           ADDITIONAL_CAPACITY_PERCENT(0,7))
!

         TRANS_GROUP = 1
         NUMBER_OF_ACTIVE_GROUPS = 0
         NUMBER_OF_HYDRO_GROUPS = 0
         NUMBER_OF_PLANNING_GROUPS = 0
         NUMBER_OF_CAPACITY_MARKETS = 0
         MAX_TRANS_GROUP_NUMBER = 0
         MAX_ASSET_CLASS_GROUPS = 0
         HYDRO_AGGREGATION = .FALSE.
         PLANNING_AREA = .FALSE.
         CAPACITY_MARKET = .FALSE.
         FIRST_REPORTING_GROUP = 0
         NUM_CREATE_HOURLY_PRICE = 0
         CREATE_HOURLY_PRICE_INDEX = 0
         CREATE_HOURLY_PRICE_POS = 0
!
         group_name="" ! Prevent uninitialized values

         GROUP_NAME(0) = "System"
         DO CURRENT_RECORD = 1, TRANS_GROUPS_RECORDS
            READ(10,REC=CURRENT_RECORD) DELETE,
     +                      GROUP_NAME(TRANS_GROUP),
     +                      GROUP_ACTIVE(TRANS_GROUP),
     +                      TRANSACTION_GROUP(TRANS_GROUP),
     +                      BASECASE_MKT_AREA_ID_tf3(TRANS_GROUP),
     +                      BASE_CASE_TRANS_AREA_ID(TRANS_GROUP),
     +                      BASECASE_SUBREGION_ID(TRANS_GROUP),
     +                      SPINNING_UNITS(TRANS_GROUP),
     +                      SPINNING_RESERVE(TRANS_GROUP),
     + ! REPLACES COMMENT which holds full name
     +                      TRANS_GROUP_FULL_NAME(TRANS_GROUP),
     +                      MAX_HOURLY_RAMP_UP(TRANS_GROUP),
     +                      MAX_HOURLY_RAMP_DOWN(TRANS_GROUP),
     +                      FIRST_CAPACITY_VALUE(TRANS_GROUP),
     +                      FIRST_CAPACITY_PERCENT(TRANS_GROUP),
     +                      SECOND_CAPACITY_VALUE(TRANS_GROUP),
     +                      SECOND_CAPACITY_PERCENT(TRANS_GROUP),
     +                      THIRD_CAPACITY_VALUE(TRANS_GROUP),
     +                      THIRD_CAPACITY_PERCENT(TRANS_GROUP),
     +                      REPORT_CL_CAPACITY(TRANS_GROUP),
     +                      ASSET_CLASS_ID(TRANS_GROUP),
     +                      ASSET_CLASS_REV_ALLOC_VECTOR(TRANS_GROUP),
     +                      TIME_ZONE(TRANS_GROUP),
     +                      CAPACITY_ADDER(TRANS_GROUP),
     +                      NOX_SEASON(TRANS_GROUP),
     +                      PURCHASE_POWER_ASSIGN(TRANS_GROUP),
     +                      PURCHASE_ASSET_CLASS_ID(TRANS_GROUP),
     +                      PURCHASE_ASSET_ALLOC_VECTOR(TRANS_GROUP),
     +                      CREATE_HOURLY_PRICE(TRANS_GROUP),
     +                      HOURLY_PRICE_NAME(TRANS_GROUP),
     +                      (ADDITIONAL_CAPACITY_VALUE(
     +                                          TRANS_GROUP,I),I=1,7),
     +                      (ADDITIONAL_CAPACITY_PERCENT(
     +                                          TRANS_GROUP,I),I=1,7),
     +                      RTO_GROUP(TRANS_GROUP),
     +                      MRX_VOLATILITY_MULT(TRANS_GROUP),
     +                      NOX_YEAR(TRANS_GROUP),
     +                      NIGHT_SCARCITY_MULT(TRANS_GROUP),
     +                      WEEKEND_SCARCITY_MULT(TRANS_GROUP),
     +                      HYDRO_LOAD_AGGREGATION(TRANS_GROUP),
     +                      OFF_PEAK_SPINNING_RESERVE(TRANS_GROUP),
     +                      OFF_PEAK_SPINNING_UNITS(TRANS_GROUP),
     +                      PRICE_CAP(TRANS_GROUP),
     +                      MAX_HOURLY_TG_IMPORT(TRANS_GROUP),
     +                      MAX_HOURLY_TG_EXPORT(TRANS_GROUP),
     +                      REGIONAL_PLANNING_AREA(TRANS_GROUP),
     +                      SCENARIO_VARIABLE,
     +                      END_NOX_YEAR(TRANS_GROUP),
     +                      ST_LHS_FOR_PRICES(TRANS_GROUP),
     +                      PRICE_ESCALATION_RATE(TRANS_GROUP),
     +             MINIMUM_CAPACITY_TESTING_RATIO(TRANS_GROUP), ! 72
     +               MAXIMUM_CAPACITY_TESTING_RATIO(TRANS_GROUP), ! 73
     +                      SAVE_MRX_EXPANSION_PLAN(TRANS_GROUP), ! 74
     +                      TEMP_LONG_TRANS_GROUP_NAME,
     +                      PRIMARY_STATE,
     +                      PRIMARY_SUPERREGION,
     +                      MRX_ICAP_UNIT_LINK(TRANS_GROUP),  ! 78
     +                      PRICE_MINIMUM(TRANS_GROUP),
     +                      REGIONAL_CAPACITY_MARKET(TRANS_GROUP)
            IF(GROUP_ACTIVE(TRANS_GROUP) == 'F') CYCLE
            TG_BASECASE_MARKET_AREA_ID(TRANS_GROUP) =
     +                             BASECASE_MKT_AREA_ID_tf3(TRANS_GROUP)

!  TODO: Grab transgroups
!
! 10/03/02. REGIONAL CONSOLIDATION FOR BURESH.
!
            IF(TRANSACTION_GROUP(TRANS_GROUP) < 0) THEN
               DO I = 1, 30
                  TEMP_I = INT2(
     +                 GET_VAR(FLOAT(TRANSACTION_GROUP(TRANS_GROUP)),I,
     +                                         "Transaction Group   "))
!
                  IF(TEMP_I == 0) EXIT
!
                  BELONGS_TO_GROUP(TEMP_I) =
     +                              ABS(TRANSACTION_GROUP(TRANS_GROUP))
               ENDDO
               TRANSACTION_GROUP(TRANS_GROUP) =
     +                              ABS(TRANSACTION_GROUP(TRANS_GROUP))
            ELSE
               BELONGS_TO_GROUP(TRANSACTION_GROUP(TRANS_GROUP)) =
     +                                   TRANSACTION_GROUP(TRANS_GROUP)
            ENDIF
!
            IF(MAX_HOURLY_RAMP_UP(TRANS_GROUP) == 0. .AND.
     +                    MAX_HOURLY_RAMP_DOWN(TRANS_GROUP) == 0.) THEN
! ASSUME THAT THEY JUST ASSIGNED A ZERO DEFAULT
               MAX_HOURLY_RAMP_UP(TRANS_GROUP) = 9999999.
               MAX_HOURLY_RAMP_DOWN(TRANS_GROUP) = 9999999.
            ENDIF
!
! OFF-PEAK INHERITS PEAK VALUES
!
            IF(OFF_PEAK_SPINNING_RESERVE(TRANS_GROUP) == -99999.) THEN
               OFF_PEAK_SPINNING_RESERVE(TRANS_GROUP) =
     +                                    SPINNING_RESERVE(TRANS_GROUP)
            ENDIF
!
            IF(OFF_PEAK_SPINNING_UNITS(TRANS_GROUP) == 'Z') THEN
               OFF_PEAK_SPINNING_UNITS(TRANS_GROUP) =
     +                                      SPINNING_UNITS(TRANS_GROUP)
            ENDIF

            IF(TRANS_GROUP_POSITION(TRANSACTION_GROUP(TRANS_GROUP)) ==
     +                                                          0) THEN
               NUMBER_OF_ACTIVE_GROUPS = NUMBER_OF_ACTIVE_GROUPS + 1
               MAX_TRANS_GROUP_NUMBER = MIN(MAX_TRANS_GROUP_INDEX,
     +                                  MAX(MAX_TRANS_GROUP_NUMBER,
     +                                 TRANSACTION_GROUP(TRANS_GROUP)))
               TRANS_GROUP_INDEX(NUMBER_OF_ACTIVE_GROUPS) =
     +                                   TRANSACTION_GROUP(TRANS_GROUP)
               TRANS_GROUP_POSITION(TRANSACTION_GROUP(TRANS_GROUP)) =
     +                                          NUMBER_OF_ACTIVE_GROUPS
!
               ns_tf_decs%report_this_group(NUMBER_OF_ACTIVE_GROUPS) =
     +                                  REPORT_CL_CAPACITY(TRANS_GROUP)
!
               IF(FIRST_REPORTING_GROUP == 0 .AND.
     +                     REPORT_CL_CAPACITY(TRANS_GROUP) == 'T') THEN
                  FIRST_REPORTING_GROUP = NUMBER_OF_ACTIVE_GROUPS
               ENDIF
!
! USUAL CASE
! 4/30/01. GAT.
!
               IF(REGIONAL_PLANNING_AREA(TRANS_GROUP) == 0) THEN
!
                  TG_2_PLANNING_AREA(NUMBER_OF_ACTIVE_GROUPS) = 0
!
! EMPLOY LOAD AGGREGATION TO A NEW GROUP.
               ELSEIF( PLANNING_AREA_POSITION(
     +                  REGIONAL_PLANNING_AREA(TRANS_GROUP)) == 0) THEN
!
                  PLANNING_AREA = .TRUE.
!
                  NUMBER_OF_PLANNING_GROUPS =
     +                                    NUMBER_OF_PLANNING_GROUPS + 1

                  TG_2_PLANNING_AREA(NUMBER_OF_ACTIVE_GROUPS) =
     +                                        NUMBER_OF_PLANNING_GROUPS
!
                  PLANNING_AREA_POSITION(
     +                  REGIONAL_PLANNING_AREA(TRANS_GROUP)) =
     +                                        NUMBER_OF_PLANNING_GROUPS
                  PLANNING_AREA_INDEX(NUMBER_OF_PLANNING_GROUPS) =
     +                              REGIONAL_PLANNING_AREA(TRANS_GROUP)
!
! EMPLOY LOAD AGGREGATION TO A GROUP THAT HAS BEEN PREVIOUSLY USED.
!
               ELSE
                  DO PA = 1, NUMBER_OF_PLANNING_GROUPS
                     IF(REGIONAL_PLANNING_AREA(TRANS_GROUP) /=
     +                                   PLANNING_AREA_INDEX(PA)) CYCLE
                     TG_2_PLANNING_AREA(NUMBER_OF_ACTIVE_GROUPS) = PA

                     EXIT
                  ENDDO
               ENDIF
!
! 022110
!
               IF(REGIONAL_CAPACITY_MARKET(TRANS_GROUP) <=
     +                                                   -29998) THEN
                  REGIONAL_CAPACITY_MARKET(TRANS_GROUP) =
     +                              REGIONAL_PLANNING_AREA(TRANS_GROUP)
               ENDIF
               IF(REGIONAL_CAPACITY_MARKET(TRANS_GROUP) == 0) THEN
!
                  TG_2_CAPACITY_MARKET(NUMBER_OF_ACTIVE_GROUPS) = 0
               ELSEIF( CAPACITY_MARKET_POSITION(
     +                REGIONAL_CAPACITY_MARKET(TRANS_GROUP)) == 0) THEN
!
!
                  CAPACITY_MARKET = .TRUE.
!
                  NUMBER_OF_CAPACITY_MARKETS =
     +                                   NUMBER_OF_CAPACITY_MARKETS + 1
                  TG_2_CAPACITY_MARKET(NUMBER_OF_ACTIVE_GROUPS) =
     +                                       NUMBER_OF_CAPACITY_MARKETS
!
                  CAPACITY_MARKET_POSITION(
     +                  REGIONAL_CAPACITY_MARKET(TRANS_GROUP)) =
     +                                       NUMBER_OF_CAPACITY_MARKETS
                  CAPACITY_MARKET_INDEX(NUMBER_OF_CAPACITY_MARKETS) =
     +                            REGIONAL_CAPACITY_MARKET(TRANS_GROUP)
               ELSE
                  DO PA = 1, NUMBER_OF_CAPACITY_MARKETS
                     IF(REGIONAL_CAPACITY_MARKET(TRANS_GROUP) /=
     +                                 CAPACITY_MARKET_INDEX(PA)) CYCLE
                     TG_2_CAPACITY_MARKET(NUMBER_OF_ACTIVE_GROUPS) = PA
                     EXIT
                  ENDDO
               ENDIF
!
! 11/20/02.
!
               TG_SCENARIO_VARIABLE_INDEX(NUMBER_OF_ACTIVE_GROUPS) =
     +                            GET_SCENARIO_INDEX(SCENARIO_VARIABLE)
!
               LONG_TRANS_GROUP_NAME(NUMBER_OF_ACTIVE_GROUPS) =
     +                                       TEMP_LONG_TRANS_GROUP_NAME
!
!
               IF(CREATE_HOURLY_PRICE(TRANS_GROUP) == 'T') THEN
                  NUM_CREATE_HOURLY_PRICE = NUM_CREATE_HOURLY_PRICE + 1
                  CREATE_HOURLY_PRICE_INDEX(TRANS_GROUP) =
     +                                          NUM_CREATE_HOURLY_PRICE
                  CREATE_HOURLY_PRICE_POS(NUM_CREATE_HOURLY_PRICE) =
     +                                                      TRANS_GROUP
               ENDIF
!
! 03/26/03. HARD-WIRED TO AVOID INADERTANT ACTIVATION (E.G. LGE)
!
               HYDRO_LOAD_AGGREGATION(TRANS_GROUP) = 0
!
               IF(HYDRO_LOAD_AGGREGATION(TRANS_GROUP) == 0) THEN

                  TG_2_HYDRO_GROUP(NUMBER_OF_ACTIVE_GROUPS) = 0
!
! EMPLOY LOAD AGGREGATION TO A NEW GROUP.
!
               ELSEIF( HYDRO_AGGREGATION_POSITION(
     +                  HYDRO_LOAD_AGGREGATION(TRANS_GROUP)) == 0) THEN
!
                  HYDRO_AGGREGATION = .TRUE.
!
                  NUMBER_OF_HYDRO_GROUPS = NUMBER_OF_HYDRO_GROUPS + 1
!
                  HYDRO_GROUP_2_TG(NUMBER_OF_HYDRO_GROUPS) =
     +                                          NUMBER_OF_ACTIVE_GROUPS
                  TG_2_HYDRO_GROUP(NUMBER_OF_ACTIVE_GROUPS) =
     +                                           NUMBER_OF_HYDRO_GROUPS
!
                  HYDRO_AGGREGATION_POSITION(
     +                  HYDRO_LOAD_AGGREGATION(TRANS_GROUP)) =
     +                                           NUMBER_OF_HYDRO_GROUPS
                  HYDRO_AGGREGATION_INDEX(NUMBER_OF_HYDRO_GROUPS) =
     +                              HYDRO_LOAD_AGGREGATION(TRANS_GROUP)
!
! EMPLOY LOAD AGGREGATION TO A GROUP THAT HAS BEEN PREVIOUSLY USED.
!
               ELSE
                  DO HG = 1, NUMBER_OF_HYDRO_GROUPS
                     IF(HYDRO_LOAD_AGGREGATION(TRANS_GROUP) /=
     +                               HYDRO_AGGREGATION_INDEX(HG)) CYCLE
                     TG_2_HYDRO_GROUP(NUMBER_OF_ACTIVE_GROUPS) = HG
                     HYDRO_GROUP_2_TG(HG) = NUMBER_OF_ACTIVE_GROUPS
                     EXIT
                  ENDDO
               ENDIF
!
!
!              ADDED 10/7/98. GAT. TO CAPTURE TRANSACT TRANSMISSION
!                       REVENUES/EXPENSES FOR ASSET ANALYST.
!
               AC = ASSET_CLASS_ID(TRANS_GROUP)
    !        Collect list of Transaction Group Names and IDs



               IF(ASSET_CLASS_GROUPS_INDEX(AC) == 0) THEN
                  MAX_ASSET_CLASS_GROUPS = MAX_ASSET_CLASS_GROUPS + 1
                  ASSET_CLASS_GROUPS_INDEX(AC) = MAX_ASSET_CLASS_GROUPS
                  ASSET_CLASS_GROUP_2_AC(MAX_ASSET_CLASS_GROUPS) = AC
!
! TEMP. 12/27/01.  NOTE RE-ASSIGNMENT.
!
                  AC = PURCHASE_ASSET_CLASS_ID(TRANS_GROUP)
                  ASSET_CLASS_2_TG(AC) = NUMBER_OF_ACTIVE_GROUPS
               ELSE
                  WRITE(4,*) "Duplicate Asset Class Detected in"
                  WRITE(4,*) "the Transaction Group file."
                  WRITE(4,*) "Transaction Group Name = ",
     +                                          GROUP_NAME(TRANS_GROUP)
                  WRITE(4,*) "Asset Class Id = ",
     +                                      ASSET_CLASS_ID(TRANS_GROUP)
                  WRITE(4,*) "Renumber or accumulate groups."
                  WRITE(4,*) " "
               ENDIF
!
               TRANS_GROUP = TRANS_GROUP + 1
!
            ELSE
               WRITE(4,*) "Duplicate Transaction Group Detected in"
               WRITE(4,*) "the Transaction Group file."
               WRITE(4,*) "Transaction Group Name = ",
     +                                          GROUP_NAME(TRANS_GROUP)
               WRITE(4,*) "Renumber or accumulate groups."
               WRITE(4,*) " "
            ENDIF
         ENDDO
!
!
         CALL CLOSE_TG_FILE
         PRICE_ESCALATION_RATE = 1. + PRICE_ESCALATION_RATE/100.
         VOID_LOGICAL =  STORE_TG_SCARCITY_INFO(TRANS_GROUPS_RECORDS,
     +                                      FIRST_CAPACITY_VALUE,
     +                                      FIRST_CAPACITY_PERCENT,
     +                                      SECOND_CAPACITY_VALUE,
     +                                      SECOND_CAPACITY_PERCENT,
     +                                      THIRD_CAPACITY_VALUE,
     +                                      THIRD_CAPACITY_PERCENT,
     +                                      CAPACITY_ADDER,
     +                                      ADDITIONAL_CAPACITY_VALUE,
     +                                     ADDITIONAL_CAPACITY_PERCENT)
         SAVE_TG_FILE_EXISTS = .TRUE.
         READ_TRANS_GROUPS_DATA = .TRUE.
      RETURN

            ENTRY GET_MIN_CAP_TESTING_RATIO(R_TRANS_GROUP)

               GET_MIN_CAP_TESTING_RATIO =
     +                    MINIMUM_CAPACITY_TESTING_RATIO(R_TRANS_GROUP)
            RETURN

            ENTRY GET_MAX_CAP_TESTING_RATIO(R_TRANS_GROUP)

               GET_MAX_CAP_TESTING_RATIO =
     +                    MAXIMUM_CAPACITY_TESTING_RATIO(R_TRANS_GROUP)
            RETURN

      ENTRY GET_BASECASE_MARKET_AREA_ID(R_BASECASE_MARKET_AREA_ID,
     +                                  R_TRANS_GROUP)



         GET_BASECASE_MARKET_AREA_ID = .TRUE.
         R_BASECASE_MARKET_AREA_ID =
     +                           BASECASE_MKT_AREA_ID_tf3(R_TRANS_GROUP)

      RETURN

      ENTRY GET_TG_PRICE_MULT(R_PRICE_MULT,
     +                        R_PRICE_VARIABLE,
     +                        R_NUMBER_OF_TRANS_GROUPS,
     +                        R_MONTH,
     +                        R_YEAR)

         IF(TG_FILE_EXISTS) THEN
            GET_TG_PRICE_MULT = .TRUE.
            DO TRANS = 1, R_NUMBER_OF_TRANS_GROUPS
               TEMP_I = TG_SCENARIO_VARIABLE_INDEX(TRANS)
               IF( TEMP_I > 0) THEN
                  R_PRICE_MULT(TRANS) =
     +               GET_SCENARIO_BY_INDEX(R_YEAR,R_MONTH,TEMP_I)
                  R_PRICE_VARIABLE(TRANS) = 1
               ELSEIF(TEMP_I == -99) THEN
                  R_PRICE_MULT(TRANS) = 1.0
                  R_PRICE_VARIABLE(TRANS) = 1
               ELSE ! DON'T WANT ANY ELECTRICITY PRICE IMPACT.
                  R_PRICE_MULT(TRANS) = 1.0
                  R_PRICE_VARIABLE(TRANS) = 0
               ENDIF
            ENDDO
         ELSE
            GET_TG_PRICE_MULT = .FALSE.
            DO TRANS = 1, R_NUMBER_OF_TRANS_GROUPS
               R_PRICE_MULT(TRANS) = 1.0
               R_PRICE_VARIABLE(TRANS) = 0
            ENDDO
         ENDIF
      RETURN

      ENTRY GET_ONE_TG_PRICE_MULT(
     +                        R_ONE_PRICE_MULT,
     +              R_ONE_PRICE_VARIABLE, ! DEFINED AS ONE BY CALLER
     +                        R_TRANS_GROUP, ! SINGULAR
     +                        R_MONTH,
     +                        R_YEAR)

         TRANS = R_TRANS_GROUP
         IF(TG_FILE_EXISTS) THEN
            GET_TG_PRICE_MULT = .TRUE.

               TEMP_I = TG_SCENARIO_VARIABLE_INDEX(TRANS)
               IF( TEMP_I > 0) THEN
                  R_ONE_PRICE_MULT =
     +               GET_SCENARIO_BY_INDEX(R_YEAR,R_MONTH,TEMP_I)
                  R_ONE_PRICE_VARIABLE = 1
               ELSEIF(TEMP_I == -99) THEN
                  R_ONE_PRICE_MULT = 1.0
                  R_ONE_PRICE_VARIABLE = 1
               ELSE ! DON'T WANT ANY ELECTRICITY PRICE IMPACT.
                  R_ONE_PRICE_MULT = 1.0
                  R_ONE_PRICE_VARIABLE = 0
               ENDIF
!            ENDDO
         ELSE
            GET_TG_PRICE_MULT = .FALSE.
!            DO TRANS = 1, R_NUMBER_OF_TRANS_GROUPS
            R_ONE_PRICE_MULT = 1.0
            R_ONE_PRICE_VARIABLE = 0
!            ENDDO
         ENDIF
      RETURN

      ENTRY GET_MRX_ICAP_UNIT_LINK(R_TRANS_GROUP)

         IF(TG_FILE_EXISTS) THEN
            IF(MRX_ICAP_UNIT_LINK(R_TRANS_GROUP) < 0) THEN
            ELSE
               GET_MRX_ICAP_UNIT_LINK=MRX_ICAP_UNIT_LINK(R_TRANS_GROUP)
            ENDIF
         ELSE
            GET_MRX_ICAP_UNIT_LINK = 0
         ENDIF
      RETURN

      ENTRY GET_ASSET_CLASS_2_TG(R_ASSET_CLASS)

         IF(TG_FILE_EXISTS) THEN
            GET_ASSET_CLASS_2_TG = ASSET_CLASS_2_TG(R_ASSET_CLASS)
         ELSE
            GET_ASSET_CLASS_2_TG = 0
         ENDIF
      RETURN

      ENTRY GET_BELONGS_TO_GROUP(R_TRANS_GROUP) ! NOT INDEXED

         IF(R_TRANS_GROUP > 0 .AND. R_TRANS_GROUP <=

     +                                      MAX_TRANS_GROUP_INDEX) THEN
            GET_BELONGS_TO_GROUP = BELONGS_TO_GROUP(R_TRANS_GROUP)
         ELSE
            GET_BELONGS_TO_GROUP = R_TRANS_GROUP
         ENDIF
      RETURN


      ENTRY FIRST_TRANSACT_REPORTING_GROUP()

         FIRST_TRANSACT_REPORTING_GROUP = FIRST_REPORTING_GROUP
      RETURN

      ENTRY GET_HG_FROM_TG(R_TRANS_GROUP)


! USES DOUBLE INDEX (TG).
!
         GET_HG_FROM_TG = TG_2_HYDRO_GROUP(R_TRANS_GROUP)
      RETURN


      ENTRY GET_NUMBER_OF_PLANNING_GROUPS

         GET_NUMBER_OF_PLANNING_GROUPS = NUMBER_OF_PLANNING_GROUPS
      RETURN

      ENTRY GET_NUMBER_OF_CAPACITY_MARKETS

         GET_NUMBER_OF_CAPACITY_MARKETS = NUMBER_OF_CAPACITY_MARKETS
      RETURN

      ENTRY GET_PA_FROM_TG(R_TRANS_GROUP)

! USES DOUBLE INDEX (TG).
!
         GET_PA_FROM_TG = TG_2_PLANNING_AREA(R_TRANS_GROUP)
      RETURN

      ENTRY GET_CM_FROM_TG(R_TRANS_GROUP)

         GET_CM_FROM_TG = TG_2_CAPACITY_MARKET(R_TRANS_GROUP)
      RETURN

      ENTRY GET_PLANNING_AREA_POSITION(R_TRANS_GROUP)
      ! First call, r_trans_group=127/127
      ! planning_area_position(tg)=0/0
      ! debugstop

        
         GET_PLANNING_AREA_POSITION =
     +                            PLANNING_AREA_POSITION(R_TRANS_GROUP)

         ! Get_planning_area_position=0/0
         ! Planning_area_position mostly zeros with value
         ! at 116/116 
         if(get_planning_area_position/=0) then
             ! r_trans_group=116
             ! planning_area_position(116)=1/1
            get_planning_area_position=get_planning_area_position ! Debugstop
            ! get_planning_area_position=0/0
         end if
      RETURN

      ENTRY GET_CAPACITY_MARKET_POSITION(R_TRANS_GROUP)

         GET_CAPACITY_MARKET_POSITION =
     +                          CAPACITY_MARKET_POSITION(R_TRANS_GROUP)
      RETURN

      ENTRY GET_PA_VALUE_FROM_TG(R_TRANS_GROUP)


! USES DOUBLE INDEX (TG).
!
         GET_PA_VALUE_FROM_TG = PLANNING_AREA_INDEX(R_TRANS_GROUP)
      RETURN

      ENTRY GET_CM_INDEX_FROM_TG(R_TRANS_GROUP)

! USES DOUBLE INDEX (TG).
!
         GET_CM_INDEX_FROM_TG = CAPACITY_MARKET_INDEX(R_TRANS_GROUP)
      RETURN


      ENTRY GET_MRX_VOLATILITY_MULT(R_TRANS_GROUP)

         GET_MRX_VOLATILITY_MULT = MRX_VOLATILITY_MULT(R_TRANS_GROUP)
      RETURN

      ENTRY GET_NIGHT_SCARCITY_MULT(R_TRANS_GROUP)

         GET_NIGHT_SCARCITY_MULT = NIGHT_SCARCITY_MULT(R_TRANS_GROUP)
      RETURN

      ENTRY GET_WEEKEND_SCARCITY_MULT(R_TRANS_GROUP)

         GET_WEEKEND_SCARCITY_MULT =
     +                             WEEKEND_SCARCITY_MULT(R_TRANS_GROUP)
      RETURN

      ENTRY GET_PURCHASE_ASSET_CLASS_ID(R_TRANS_GROUP)

         GET_PURCHASE_ASSET_CLASS_ID =
     +                           PURCHASE_ASSET_CLASS_ID(R_TRANS_GROUP)
      RETURN

      ENTRY GET_PURCHASE_POWER_ASSIGN(R_TRANS_GROUP,
     +                                R_PURCHASE_POWER_ASSIGN)

         GET_PURCHASE_POWER_ASSIGN = .TRUE.
         R_PURCHASE_POWER_ASSIGN = PURCHASE_POWER_ASSIGN(R_TRANS_GROUP)
      RETURN

      ENTRY GET_CREATE_HOURLY_PRICE(R_TRANS_GROUP)

         GET_CREATE_HOURLY_PRICE =
     +     CREATE_HOURLY_PRICE(R_TRANS_GROUP) == 'T'
      RETURN

      ENTRY GET_NUM_CREATE_HOURLY_PRICE()

         GET_NUM_CREATE_HOURLY_PRICE = NUM_CREATE_HOURLY_PRICE
      RETURN

      ENTRY GET_CREATE_HOURLY_PRICE_INDEX(R_TRANS_GROUP)

         GET_CREATE_HOURLY_PRICE_INDEX =
     +                         CREATE_HOURLY_PRICE_INDEX(R_TRANS_GROUP)
      RETURN

      ENTRY GET_CREATE_HOURLY_PRICE_POS(R_TRANS_GROUP)

         GET_CREATE_HOURLY_PRICE_POS =
     +                          CREATE_HOURLY_PRICE_POS(R_TRANS_GROUP)
      RETURN
!
!

      ENTRY GET_DAILY_PEAK_SPIN(R_TRANS_GROUP,
     +                          R_MONTH,
     +                          R_YEAR,
     +                          R_CURRENT_SPIN,
     +                          R_ON_OR_OFF_PEAK,
     +                          R_DAILY_PEAK)

         IF(R_ON_OR_OFF_PEAK == 1) THEN ! PEAK
            IF(SPINNING_UNITS(R_TRANS_GROUP) == 'D') THEN
               IF(SPINNING_RESERVE(R_TRANS_GROUP) < 0.) THEN
                  TEMP_CAPACITY =
     +                  ESCALATED_MONTHLY_VALUE(TEMP_CAPACITY,
     +                      INT2(ABS(SPINNING_RESERVE(R_TRANS_GROUP))),
     +                                          R_YEAR,R_MONTH,INT2(1))
               ELSE
                  TEMP_CAPACITY = SPINNING_RESERVE(R_TRANS_GROUP)
               ENDIF
               IF(.NOT. SAVE_TG_FILE_EXISTS) THEN
                  GET_DAILY_PEAK_SPIN = 0.
               ELSE
                  GET_DAILY_PEAK_SPIN = R_DAILY_PEAK *
     +                             TEMP_CAPACITY/100.
               ENDIF
            ELSE
               GET_DAILY_PEAK_SPIN = R_CURRENT_SPIN
            ENDIF
         ELSE !OFF-PEAK
            IF(OFF_PEAK_SPINNING_UNITS(R_TRANS_GROUP) == 'D') THEN
               IF(OFF_PEAK_SPINNING_RESERVE(R_TRANS_GROUP) < 0.) THEN
                  TEMP_CAPACITY = ESCALATED_MONTHLY_VALUE(TEMP_CAPACITY,
     +             INT2(ABS(OFF_PEAK_SPINNING_RESERVE(R_TRANS_GROUP))),
     +                                          R_YEAR,R_MONTH,INT2(1))
               ELSE
                  TEMP_CAPACITY =
     +                         OFF_PEAK_SPINNING_RESERVE(R_TRANS_GROUP)
               ENDIF
               IF(.NOT. SAVE_TG_FILE_EXISTS) THEN
                  GET_DAILY_PEAK_SPIN = 0.
               ELSE
                GET_DAILY_PEAK_SPIN = R_DAILY_PEAK * TEMP_CAPACITY/100.
               ENDIF
            ELSE
               GET_DAILY_PEAK_SPIN = R_CURRENT_SPIN
            ENDIF
         ENDIF
      RETURN

      ENTRY GET_TRANS_SPINNING_CAPACITY(
     +               R_NUMBER_OF_TRANS_GROUPS,R_MONTH,
     +               R_YEAR,R_CAPACITY)

!
         IF(NUMBER_OF_ACTIVE_GROUPS /= R_NUMBER_OF_TRANS_GROUPS) THEN
            WRITE(4,*) "During Multi-area simulation, the model"
            WRITE(4,*) "detected a mismatch in the number of"
            WRITE(4,*) "active Transaction Groups"
            WRITE(4,*) "within Spinning Reserve logic."
            WRITE(4,*) '*** line 5086 TF_OBJT.FOR ***'

         ENDIF
         DO TRANS = 1, R_NUMBER_OF_TRANS_GROUPS
            IF(SPINNING_RESERVE(TRANS) < 0.) THEN

               TEMP_CAPACITY = ESCALATED_MONTHLY_VALUE(TEMP_CAPACITY,
     +                              INT2(ABS(SPINNING_RESERVE(TRANS))),
     +                                          R_YEAR,R_MONTH,INT2(1))
            ELSE
               TEMP_CAPACITY = SPINNING_RESERVE(TRANS)
            ENDIF
            IF(SPINNING_UNITS(TRANS) == 'M') THEN
               R_CAPACITY(TRANS) = TEMP_CAPACITY
            ELSEIF(SPINNING_UNITS(TRANS) == 'P') THEN
               IF(.NOT. SAVE_TG_FILE_EXISTS) THEN
                  R_CAPACITY(TRANS) = 0.
               ELSE
                  R_CAPACITY(TRANS) =
     +                         GET_TRANS_GROUP_PEAK(TRANS,R_MONTH) *
     +                             TEMP_CAPACITY/100.
               ENDIF
            ELSEIF(SPINNING_UNITS(TRANS) == 'C') THEN

               IF(.NOT. SAVE_TG_FILE_EXISTS) THEN
                  TG = TRANS
               ELSEIF(TRANS > 0 .AND.
     +                            TRANS <= MAX_TRANS_GROUP_NUMBER) THEN
                  TG = TRANS_GROUP_INDEX(TRANS)
               ELSE
                  WRITE(4,*) "Invalid value for index to "//
     +                        "transaction group"
                  WRITE(4,*) "Transaction Group = ",TRANS
               ENDIF
            DAY_TYPE = 1 ! REPORTING IS BASED ON FIRST DAY TYPE DEFINED.
               DATA_BASE = GET_DATA_BASE_FOR_TRANS(TG,DAY_TYPE)
               R_CAPACITY(TRANS)  = SNGL(TRANS_GROUP_CAP(DATA_BASE)) *
     +                             TEMP_CAPACITY/100.
            ELSE ! ASSUME UNITS = FALSE
               R_CAPACITY(TRANS) = 0.

            ENDIF
         ENDDO
         GET_TRANS_SPINNING_CAPACITY = .TRUE.
      RETURN
!

      ENTRY GET_TRANS_SPIN_FOR_TG(
     +               R_TG,
     +               R_MONTH,
     +               R_YEAR)

!
         IF(NUMBER_OF_ACTIVE_GROUPS /= R_NUMBER_OF_TRANS_GROUPS) THEN
            WRITE(4,*) "During Multi-area simulation, the model"
            WRITE(4,*) "detected a mismatch in the number of"
            WRITE(4,*) "active Transaction Groups"
            WRITE(4,*) "within Spinning Reserve logic."
            WRITE(4,*) '*** line 5086 TF_OBJT.FOR ***'

         ENDIF
!         DO TRANS = 1, R_NUMBER_OF_TRANS_GROUPS
            TRANS = R_TG
            IF(SPINNING_RESERVE(TRANS) < 0.) THEN

               TEMP_CAPACITY = ESCALATED_MONTHLY_VALUE(TEMP_CAPACITY,
     +                              INT2(ABS(SPINNING_RESERVE(TRANS))),
     +                                          R_YEAR,R_MONTH,INT2(1))
            ELSE
               TEMP_CAPACITY = SPINNING_RESERVE(TRANS)
            ENDIF
            IF(SPINNING_UNITS(TRANS) == 'M') THEN
               GET_TRANS_SPIN_FOR_TG = TEMP_CAPACITY
            ELSEIF(SPINNING_UNITS(TRANS) == 'P') THEN
               IF(.NOT. SAVE_TG_FILE_EXISTS) THEN
                  GET_TRANS_SPIN_FOR_TG = 0.
               ELSE
                  GET_TRANS_SPIN_FOR_TG =
     +                         GET_TRANS_GROUP_PEAK(TRANS,R_MONTH) *
     +                             TEMP_CAPACITY/100.
               ENDIF
            ELSEIF(SPINNING_UNITS(TRANS) == 'C') THEN

               IF(.NOT. SAVE_TG_FILE_EXISTS) THEN
                  TG = TRANS
               ELSEIF(TRANS > 0 .AND.
     +                            TRANS <= MAX_TRANS_GROUP_NUMBER) THEN
                  TG = TRANS_GROUP_INDEX(TRANS)
               ELSE
                  WRITE(4,*) "Invalid value for index to "//
     +                        "transaction group"
                  WRITE(4,*) "Transaction Group = ",TRANS
               ENDIF
            DAY_TYPE = 1 ! REPORTING IS BASED ON FIRST DAY TYPE DEFINED.
               DATA_BASE = GET_DATA_BASE_FOR_TRANS(TG,DAY_TYPE)
               GET_TRANS_SPIN_FOR_TG  =
     +                  SNGL(TRANS_GROUP_CAP(DATA_BASE)) *
     +                             TEMP_CAPACITY/100.
            ELSE ! ASSUME UNITS = FALSE
               GET_TRANS_SPIN_FOR_TG = 0.

            ENDIF
!         ENDDO
      RETURN
!


      ENTRY GET_TRANS_PRICE_MINIMUM(
     +               R_NUMBER_OF_TRANS_GROUPS,R_MONTH,
     +               R_YEAR,R_CAPACITY)


!
         IF(NUMBER_OF_ACTIVE_GROUPS /= R_NUMBER_OF_TRANS_GROUPS) THEN
            WRITE(4,*) "During Multi-area simulation, the model"
            WRITE(4,*) "detected a mismatch in the number of"
            WRITE(4,*) "active Transaction Groups"
            WRITE(4,*) "within Price Minimum logic."
            WRITE(4,*) '*** line 9532 TF_OBJT.FOR ***'

         ENDIF
         DO TRANS = 1, R_NUMBER_OF_TRANS_GROUPS
            IF(PRICE_MINIMUM(TRANS) < 0.) THEN
               R_CAPACITY(TRANS) =
     +                           ESCALATED_MONTHLY_VALUE(TEMP_CAPACITY,
     +                               INT2(ABS(PRICE_MINIMUM(TRANS))),
     +                                         R_YEAR,R_MONTH,INT2(1))
            ELSE
               R_CAPACITY(TRANS) = PRICE_MINIMUM(TRANS)
            ENDIF
         ENDDO
         GET_TRANS_PRICE_MINIMUM = .TRUE.
      RETURN

      ENTRY GET_TRANS_PRICE_CAPS(
     +               R_NUMBER_OF_TRANS_GROUPS,R_MONTH,
     +               R_YEAR,R_CAPACITY)

!
         IF(NUMBER_OF_ACTIVE_GROUPS /= R_NUMBER_OF_TRANS_GROUPS) THEN
            WRITE(4,*) "During Multi-area simulation, the model"
            WRITE(4,*) "detected a mismatch in the number of"
            WRITE(4,*) "active Transaction Groups"
            WRITE(4,*) "within Price Cap logic."
            WRITE(4,*) '*** line 5148 TF_OBJT.FOR ***'

         ENDIF
         DO TRANS = 1, R_NUMBER_OF_TRANS_GROUPS
            IF(PRICE_CAP(TRANS) < 0.) THEN
               R_CAPACITY(TRANS) =
     +                           ESCALATED_MONTHLY_VALUE(TEMP_CAPACITY,
     +                               INT2(ABS(PRICE_CAP(TRANS))),
     +                                          R_YEAR,R_MONTH,INT2(1))
            ELSE
               R_CAPACITY(TRANS) = PRICE_CAP(TRANS)
            ENDIF
         ENDDO
         GET_TRANS_PRICE_CAPS = .TRUE.
      RETURN
!
!

      ENTRY GET_OFF_PEAK_SPINNING_CAPACITY(
     +               R_NUMBER_OF_TRANS_GROUPS,R_MONTH,
     +               R_YEAR,R_CAPACITY)

!
         IF(NUMBER_OF_ACTIVE_GROUPS /= R_NUMBER_OF_TRANS_GROUPS) THEN
            WRITE(4,*) "During Multi-area simulation, the model"
            WRITE(4,*) "detected a mismatch in the number of"
            WRITE(4,*) "active Transaction Groups"
            WRITE(4,*) "within Spinning Reserve logic."
            WRITE(4,*) '*** line 5176 TF_OBJT.FOR ***'

         ENDIF
         DO TRANS = 1, R_NUMBER_OF_TRANS_GROUPS
            IF(OFF_PEAK_SPINNING_RESERVE(TRANS) < 0.) THEN

               TEMP_CAPACITY = ESCALATED_MONTHLY_VALUE(TEMP_CAPACITY,
     +                     INT2(ABS(OFF_PEAK_SPINNING_RESERVE(TRANS))),
     +                                          R_YEAR,R_MONTH,INT2(1))
            ELSE
               TEMP_CAPACITY = OFF_PEAK_SPINNING_RESERVE(TRANS)
            ENDIF
            IF(OFF_PEAK_SPINNING_UNITS(TRANS) == 'M') THEN
               R_CAPACITY(TRANS) = TEMP_CAPACITY
            ELSEIF(OFF_PEAK_SPINNING_UNITS(TRANS) == 'P') THEN
               IF(.NOT. SAVE_TG_FILE_EXISTS) THEN
                  R_CAPACITY(TRANS) = 0.
               ELSE
                  R_CAPACITY(TRANS) =
     +                         GET_TRANS_GROUP_PEAK(TRANS,R_MONTH) *
     +                             TEMP_CAPACITY/100.
               ENDIF
            ELSEIF(OFF_PEAK_SPINNING_UNITS(TRANS) == 'C') THEN

               IF(.NOT. SAVE_TG_FILE_EXISTS) THEN
                  TG = TRANS
               ELSEIF(TRANS > 0 .AND.
     +                            TRANS <= MAX_TRANS_GROUP_NUMBER) THEN
                  TG = TRANS_GROUP_INDEX(TRANS)
               ELSE
                  WRITE(4,*) "Invalid value for index to "//
     +                        "transaction group"
                  WRITE(4,*) "Transaction Group = ",TRANS
               ENDIF
            DAY_TYPE = 1 ! REPORTING IS BASED ON FIRST DAY TYPE DEFINED.
               DATA_BASE = GET_DATA_BASE_FOR_TRANS(TG,DAY_TYPE)
               R_CAPACITY(TRANS)  = SNGL(TRANS_GROUP_CAP(DATA_BASE)) *
     +                             TEMP_CAPACITY/100.
            ELSE ! ASSUME UNITS = FALSE
               R_CAPACITY(TRANS) = 0.

            ENDIF
         ENDDO
         GET_OFF_PEAK_SPINNING_CAPACITY = .TRUE.
      RETURN

      ENTRY GET_OFF_PEAK_SPIN_FOR_TG(
     +               R_TG,
     +               R_MONTH,
     +               R_YEAR)

!
         IF(NUMBER_OF_ACTIVE_GROUPS < R_TG) THEN
            WRITE(4,*) "During Multi-area simulation, the model"
            WRITE(4,*) "detected a mismatch in the number of"
            WRITE(4,*) "active Transaction Groups"
            WRITE(4,*) "within Spinning Reserve logic."
            WRITE(4,*) '*** line 6915 TF_OBJT.FOR ***'

         ENDIF
!         DO TRANS = 1, R_NUMBER_OF_TRANS_GROUPS
            TRANS = R_TG
            IF(OFF_PEAK_SPINNING_RESERVE(TRANS) < 0.) THEN

               TEMP_CAPACITY = ESCALATED_MONTHLY_VALUE(TEMP_CAPACITY,
     +                     INT2(ABS(OFF_PEAK_SPINNING_RESERVE(TRANS))),
     +                                          R_YEAR,R_MONTH,INT2(1))
            ELSE
               TEMP_CAPACITY = OFF_PEAK_SPINNING_RESERVE(TRANS)
            ENDIF
            IF(OFF_PEAK_SPINNING_UNITS(TRANS) == 'M') THEN
               GET_OFF_PEAK_SPIN_FOR_TG = TEMP_CAPACITY
            ELSEIF(OFF_PEAK_SPINNING_UNITS(TRANS) == 'P') THEN
               IF(.NOT. SAVE_TG_FILE_EXISTS) THEN
                  GET_OFF_PEAK_SPIN_FOR_TG = 0.
               ELSE
                  GET_OFF_PEAK_SPIN_FOR_TG =
     +                         GET_TRANS_GROUP_PEAK(TRANS,R_MONTH) *
     +                             TEMP_CAPACITY/100.
               ENDIF
            ELSEIF(OFF_PEAK_SPINNING_UNITS(TRANS) == 'C') THEN

               IF(.NOT. SAVE_TG_FILE_EXISTS) THEN
                  TG = TRANS
               ELSEIF(TRANS > 0 .AND.
     +                            TRANS <= MAX_TRANS_GROUP_NUMBER) THEN
                  TG = TRANS_GROUP_INDEX(TRANS)
               ELSE
                  WRITE(4,*) "Invalid value for index to "//
     +                        "transaction group"
                  WRITE(4,*) "Transaction Group = ",TRANS
               ENDIF
            DAY_TYPE = 1 ! REPORTING IS BASED ON FIRST DAY TYPE DEFINED.
               DATA_BASE = GET_DATA_BASE_FOR_TRANS(TG,DAY_TYPE)
               GET_OFF_PEAK_SPIN_FOR_TG  =
     +                  SNGL(TRANS_GROUP_CAP(DATA_BASE)) *
     +                             TEMP_CAPACITY/100.
            ELSE ! ASSUME UNITS = FALSE
               GET_OFF_PEAK_SPIN_FOR_TG = 0.

            ENDIF

      RETURN

      ENTRY GET_TRANS_RAMP_RATES(
     +                      R_NUMBER_OF_TRANS_GROUPS,R_MONTH,
     +                      R_RAMP_UP,
     +                      R_RAMP_DOWN)

         IF(NUMBER_OF_ACTIVE_GROUPS /= R_NUMBER_OF_TRANS_GROUPS) THEN
            WRITE(4,*) "During Multi-area simulation, the model"
            WRITE(4,*) "detected a mismatch in the number of"
            WRITE(4,*) "active Transaction Groups"
            WRITE(4,*) "within RAMP RATES logic."
            WRITE(4,*) '*** line 5237 TF_OBJT.FOR ***'

         ENDIF
         DO TRANS = 1, R_NUMBER_OF_TRANS_GROUPS
            R_RAMP_UP(TRANS) = MAX_HOURLY_RAMP_UP(TRANS)
            R_RAMP_DOWN(TRANS) = MAX_HOURLY_RAMP_DOWN(TRANS)
         ENDDO
         GET_TRANS_RAMP_RATES = .TRUE.
      RETURN

      ENTRY GET_ONE_TRANS_RAMP_RATES(
     +                      R_TG,
     +                      R_MONTH,
     +                      R_RAMP_ONE_UP,
     +                      R_RAMP_ONE_DOWN)

         IF(R_TG > NUMBER_OF_ACTIVE_GROUPS) THEN
            WRITE(4,*) "During Multi-area simulation, the model"
            WRITE(4,*) "detected a mismatch of an"
            WRITE(4,*) "active Transaction Group"
            WRITE(4,*) "within RAMP RATES logic."
            WRITE(4,*) '*** line 6733 TF_OBJT.FOR ***'

         ENDIF
         R_RAMP_ONE_UP = MAX_HOURLY_RAMP_UP(R_TG)
         R_RAMP_ONE_DOWN = MAX_HOURLY_RAMP_DOWN(R_TG)
         GET_ONE_TRANS_RAMP_RATES = .TRUE.
      RETURN

      ENTRY GET_TRANS_MAX_IMPORT_EXPORT(
     +                      R_NUMBER_OF_TRANS_GROUPS,
     +                      R_MONTH,
     +                      R_RAMP_UP,
     +                      R_RAMP_DOWN)

         IF(NUMBER_OF_ACTIVE_GROUPS /= R_NUMBER_OF_TRANS_GROUPS) THEN
            WRITE(4,*) "During Multi-area simulation, the model"
            WRITE(4,*) "detected a mismatch in the number of"
            WRITE(4,*) "active Transaction Groups"
            WRITE(4,*) "within RAMP RATES logic."
            WRITE(4,*) '*** line 5237 TF_OBJT.FOR ***'

         ENDIF
         DO TRANS = 1, R_NUMBER_OF_TRANS_GROUPS
            R_RAMP_UP(TRANS) = MAX_HOURLY_TG_IMPORT(TRANS)
            R_RAMP_DOWN(TRANS) = MAX_HOURLY_TG_EXPORT(TRANS)
         ENDDO
         GET_TRANS_RAMP_RATES = .TRUE.
      RETURN

      ENTRY GET_AC_FOR_TG(R_TG)


! ASSUMES IT COMES IN AS AN INDEXED NUMBER.
         IF(.NOT. SAVE_TG_FILE_EXISTS) THEN
            GET_AC_FOR_TG = R_TG
         ELSEIF(R_TG > 0 .AND. R_TG <= MAX_TRANS_GROUP_NUMBER) THEN
            GET_AC_FOR_TG = ASSET_CLASS_GROUP_2_AC(R_TG)
         ELSE
            WRITE(4,*) "Invalid value for index to transaction group"
            WRITE(4,*) "Transaction Group = ",R_TG ! CHANGED 4/20/00.
         ENDIF
      RETURN

      ENTRY GET_REPORT_CL_CAPACITY(R_TG)

         IF(R_TG <= 0) THEN
            GET_REPORT_CL_CAPACITY = .FALSE.
         ELSE

            IF(.NOT. SAVE_TG_FILE_EXISTS) THEN
               TRANS_GROUP = R_TG
            ELSEIF(R_TG > 0 .AND. R_TG <= MAX_TRANS_GROUP_NUMBER) THEN
               TRANS_GROUP = TRANS_GROUP_POSITION(R_TG)
            ELSE
               WRITE(4,*) "Invalid value for POSITION to "//
     +                                              "transaction group"
               WRITE(4,*) "Transaction Group = ",R_TG,
     +                                         " in Report CL Capacity"
!
               TRANS_GROUP = 0
!
            ENDIF
            IF(TRANS_GROUP <= 0) THEN
!
               GET_REPORT_CL_CAPACITY = .FALSE.
            ELSE
               GET_REPORT_CL_CAPACITY =
     +                ns_tf_decs%report_this_group(TRANS_GROUP) == 'T'
            ENDIF
         ENDIF
      RETURN

      ENTRY GET_TF_HOURLY_PRICE_NAME(R_TRANS_GROUP,

     +                                        R_GET_HOURLY_PRICE_NAME)

         R_GET_HOURLY_PRICE_NAME = HOURLY_PRICE_NAME(R_TRANS_GROUP)
         GET_TF_HOURLY_PRICE_NAME = .TRUE.
      RETURN

      ENTRY GET_TF_GROUP_NAME(R_TRANS_GROUP,R_GET_GROUP_NAME)  ! LOGIC

         R_GET_GROUP_NAME = GROUP_NAME(R_TRANS_GROUP)
         GET_TF_GROUP_NAME = .TRUE.
      RETURN

      ENTRY GET_TF_TRANSACTION_GROUP_NAME(R_TRANS_GROUP,
     +                          R_GET_TRANSACTION_GROUP_NAME)  ! LOGICAL



         IF(.NOT. SAVE_TG_FILE_EXISTS) THEN
            IF(R_TRANS_GROUP <= 0 .OR.
     +                  R_TRANS_GROUP > SAVE_TRANS_GROUPS_RECORDS) THEN
               TRANS_GROUP = 1
            ELSE
               TRANS_GROUP = R_TRANS_GROUP
            ENDIF
         ELSEIF(R_TRANS_GROUP > 0 .AND.
     +                    R_TRANS_GROUP <= MAX_TRANS_GROUP_NUMBER) THEN
            TRANS_GROUP = TRANS_GROUP_POSITION(R_TRANS_GROUP)
         ELSE
            WRITE(4,*) "Invalid value for POSITION to "//
     +                                            "transaction group"
            WRITE(4,*) "Transaction Group = ",R_TRANS_GROUP,
     +                                  " in TF Transaction Group Name"
!
            TRANS_GROUP = 1
!
         ENDIF
         IF(TRANS_GROUP <= 0 .OR.
     +                    TRANS_GROUP > SAVE_TRANS_GROUPS_RECORDS) THEN
            TRANS_GROUP = 1
         ENDIF
         R_GET_TRANSACTION_GROUP_NAME = GROUP_NAME(TRANS_GROUP)
         GET_TF_TRANSACTION_GROUP_NAME = .TRUE.
      RETURN


      ENTRY GET_TF_TRANS_GROUP_NOX_SEASON(R_TRANS_GROUP,
     +                         R_GET_TRANS_GROUP_NOX_SEASON)  ! LOGICAL

         GET_TF_TRANS_GROUP_NOX_SEASON = .TRUE.
         IF(.NOT. SAVE_TG_FILE_EXISTS) THEN
            R_GET_TRANS_GROUP_NOX_SEASON = 'F'
         ELSEIF(R_TRANS_GROUP > 0 .AND.
     +                    R_TRANS_GROUP <= MAX_TRANS_GROUP_NUMBER) THEN
            TG = TRANS_GROUP_POSITION(R_TRANS_GROUP)
            IF(TG == 0) THEN
               R_GET_TRANS_GROUP_NOX_SEASON = 'F'
            ELSE
               R_GET_TRANS_GROUP_NOX_SEASON = NOX_SEASON(TG)
            ENDIF
         ELSE
            R_GET_TRANS_GROUP_NOX_SEASON = 'F'
         ENDIF
      RETURN
      ! ENTRY GET_REPORT_TRANS_GROUP moved to tf_objt



      ENTRY TRANS_GLOBAL_SCARCITY_VALUE(R_MONTH,
     +                                  R_YEAR,
     +                                  R_TRANS_CAP,
     +                                  R_TRANS_MAX_CAP)

         TRANS_GLOBAL_SCARCITY_VALUE = 0.
!
! MOVED 5/8/00.  return 9/7/01 msg
!
        GLOBAL_SCARCITY = GET_GLOBAL_SCARCITY(SCARCITY_VALUES(1),
     +                                         SCARCITY_VALUES(2),
     +                                         SCARCITY_VALUES(3),
     +                                         SCARCITY_CAP_PERCENT(1),
     +                                         SCARCITY_CAP_PERCENT(2),
     +                                         SCARCITY_CAP_PERCENT(3),
     +                                         SCARCITY_VALUES(4),
     +                                         SCARCITY_VALUES(5),
     +                                         SCARCITY_VALUES(6),
     +                                         SCARCITY_VALUES(7),
     +                                         SCARCITY_VALUES(8),
     +                                         SCARCITY_VALUES(9),
     +                                         SCARCITY_VALUES(10),
     +                                         SCARCITY_CAP_PERCENT(4),
     +                                         SCARCITY_CAP_PERCENT(5),
     +                                         SCARCITY_CAP_PERCENT(6),
     +                                         SCARCITY_CAP_PERCENT(7),
     +                                         SCARCITY_CAP_PERCENT(8),
     +                                         SCARCITY_CAP_PERCENT(9),
     +                                        SCARCITY_CAP_PERCENT(10))

         TOTAL_SCARCITY_CAPACITY = R_TRANS_MAX_CAP
         SCARCITY_CAP_PERCENT(1) = SCARCITY_CAP_PERCENT(1)/100.
         IF(R_TRANS_CAP <= SCARCITY_CAP_PERCENT(1) *
     +                                  TOTAL_SCARCITY_CAPACITY) RETURN
         DO I = 2, 10
            SCARCITY_CAP_PERCENT(I) = SCARCITY_CAP_PERCENT(I)/100.
            IF(R_TRANS_CAP <= SCARCITY_CAP_PERCENT(I) *
     +                                    TOTAL_SCARCITY_CAPACITY) THEN
               SLOPE = 0.
              IF(SCARCITY_CAP_PERCENT(I)-SCARCITY_CAP_PERCENT(I-1)/=0.)
     +             SLOPE = (SCARCITY_VALUES(I) - SCARCITY_VALUES(I-1))/
     +            (SCARCITY_CAP_PERCENT(I) - SCARCITY_CAP_PERCENT(I-1))
               TRANS_GLOBAL_SCARCITY_VALUE = SLOPE *
     +                             (R_TRANS_CAP/TOTAL_SCARCITY_CAPACITY
     +                                     - SCARCITY_CAP_PERCENT(I-1))
     +                               + SCARCITY_VALUES(I-1)
               EXIT
            ELSEIF(SCARCITY_CAP_PERCENT(I) <= 0.) THEN
               TRANS_GLOBAL_SCARCITY_VALUE = SCARCITY_VALUES(I-1)
               EXIT
            ENDIF
            TRANS_GLOBAL_SCARCITY_VALUE = SCARCITY_VALUES(I)
         ENDDO
      RETURN


      ENTRY GET_MAX_TRANS_GROUP_NUMBER

         GET_MAX_TRANS_GROUP_NUMBER = MAX_TRANS_GROUP_NUMBER
      RETURN

      ENTRY GET_NUMBER_OF_ACTIVE_GROUPS

         GET_NUMBER_OF_ACTIVE_GROUPS = NUMBER_OF_ACTIVE_GROUPS
      RETURN

      ENTRY GET_NUMBER_OF_HYDRO_GROUPS

         GET_NUMBER_OF_HYDRO_GROUPS = NUMBER_OF_HYDRO_GROUPS
      RETURN


      ENTRY TRANS_GROUP_ACTIVE_SWITCH(R_TRANS_GROUP)

         IF(.NOT. SAVE_TG_FILE_EXISTS) THEN
            TRANS_GROUP_ACTIVE_SWITCH = .TRUE.
         ELSEIF(R_TRANS_GROUP > 0 .AND.
     +                     R_TRANS_GROUP <= MAX_TRANS_GROUP_INDEX) THEN
            IF(TRANS_GROUP_POSITION(R_TRANS_GROUP) > 0 .AND.
     +            TRANS_GROUP_POSITION(R_TRANS_GROUP) <=
     +                                     MAX_TRANS_GROUP_NUMBER) THEN
               TRANS_GROUP_ACTIVE_SWITCH =
     +                GROUP_ACTIVE(TRANS_GROUP_POSITION(R_TRANS_GROUP))
     +                                                           == 'T'

            ELSE
               TRANS_GROUP_ACTIVE_SWITCH = .FALSE.
            ENDIF
         ELSE
            TRANS_GROUP_ACTIVE_SWITCH = .FALSE.
         ENDIF
      RETURN

!
! 07/28/04. MODIFIED TO TAKE INTO ACCOUNT TRANSACTION GROUP POINTERS
!
      ENTRY MOD_TRANS_GROUP_ACTIVE_SWITCH(R_TRANS_GROUP)

         IF(.NOT. SAVE_TG_FILE_EXISTS) THEN
            MOD_TRANS_GROUP_ACTIVE_SWITCH = .TRUE.
         ELSEIF(R_TRANS_GROUP > 0 .AND.
     +                     R_TRANS_GROUP <= MAX_TRANS_GROUP_INDEX) THEN
            IF(TRANS_GROUP_POSITION(R_TRANS_GROUP) > 0 .AND.
     +            TRANS_GROUP_POSITION(R_TRANS_GROUP) <=
     +                                     MAX_TRANS_GROUP_NUMBER) THEN
               MOD_TRANS_GROUP_ACTIVE_SWITCH =
     +                GROUP_ACTIVE(TRANS_GROUP_POSITION(R_TRANS_GROUP))
     +                                                           == 'T'
            ELSEIF(TRANS_GROUP_POSITION(R_TRANS_GROUP) == 0) THEN
               IF(BELONGS_TO_GROUP(R_TRANS_GROUP) > 0) THEN
                  MOD_TRANS_GROUP_ACTIVE_SWITCH = .TRUE.
               ELSE
                  MOD_TRANS_GROUP_ACTIVE_SWITCH = .FALSE.
               ENDIF
            ELSE
               MOD_TRANS_GROUP_ACTIVE_SWITCH = .FALSE.
            ENDIF
         ELSE
            MOD_TRANS_GROUP_ACTIVE_SWITCH = .FALSE.
         ENDIF
      RETURN

      ENTRY GET_TRANS_GROUP_INDEX(R_TRANS_GROUP)

         gtgi_numcalls=gtgi_numcalls+1
         IF(.NOT. SAVE_TG_FILE_EXISTS) THEN
            FUNCTION_HOLDER2 = R_TRANS_GROUP
         ELSEIF(R_TRANS_GROUP > 0 .AND.
     +                    R_TRANS_GROUP <= MAX_TRANS_GROUP_NUMBER) THEN
            FUNCTION_HOLDER2 = TRANS_GROUP_INDEX(R_TRANS_GROUP)
         ELSE
            WRITE(4,*) "Invalid value for index to transaction group"
            WRITE(4,*) "Transaction Group = ",R_TRANS_GROUP
         ENDIF
         GET_TRANS_GROUP_INDEX = FUNCTION_HOLDER2
      RETURN

      ENTRY GET_TRANS_GROUP_POSITION(R_TRANS_GROUP)
      !
         gtgp_count=gtgp_count+1
         if(gtgp_count==48427) then
            gtgp_count=gtgp_count ! Debugstop
         end if
         if(file_trace_gtgp==0) then
            ! This tracefile generates gigabytes of output. Consider
            ! not turning on during runs or reducing the amount of
            ! data it produces, if the size is an issue.
            file_trace_gtgp=open_trace("get_trans_group_position.trace", 
     +           rq_gtgp)

         call write_trace_int2s(file_trace_gtgp, "TRANS_GROUP_POSITION", 
     +       TRANS_GROUP_POSITION)
         end if
         call write_trace_int4(file_trace_gtgp, "callcount", gtgp_count)
         if(gtgp_count==48427) then
            gtgp_count=gtgp_count ! Debugstop
         end if
         
         call write_trace_int2(file_trace_gtgp, "R_TRANS_GROUP", 
     + R_TRANS_GROUP)

     
         IF(.NOT. SAVE_TG_FILE_EXISTS) THEN
            GET_TRANS_GROUP_POSITION = R_TRANS_GROUP
            
         ELSEIF(R_TRANS_GROUP > 0 .AND.
     +                    R_TRANS_GROUP <= MAX_TRANS_GROUP_NUMBER) THEN

            GET_TRANS_GROUP_POSITION =
     +                              TRANS_GROUP_POSITION(R_TRANS_GROUP)

         ELSE

            GET_TRANS_GROUP_POSITION = 0

         ENDIF
         call write_trace_int2(file_trace_gtgp, "Returning ", 
     + GET_TRANS_GROUP_POSITION)
     
      RETURN


      ENTRY GET_TRANS_GROUP_NOX_YEAR(R_TRANS_GROUP)


         IF(.NOT. SAVE_TG_FILE_EXISTS) THEN
            GET_TRANS_GROUP_NOX_YEAR = 2004
         ELSEIF(R_TRANS_GROUP > 0 .AND.
     +                    R_TRANS_GROUP <= MAX_TRANS_GROUP_NUMBER) THEN
            TG = TRANS_GROUP_POSITION(R_TRANS_GROUP)
            IF(TG == 0) THEN
               GET_TRANS_GROUP_NOX_YEAR = 2004
            ELSE
               GET_TRANS_GROUP_NOX_YEAR = NOX_YEAR(TG)
            ENDIF
         ELSE
            GET_TRANS_GROUP_NOX_YEAR = 2004
         ENDIF
      RETURN


      ENTRY GET_ST_LHS_FOR_PRICES(R_ST_LHS_FOR_PRICES,R_TRANS_GROUP)

         IF(.NOT. SAVE_TG_FILE_EXISTS) THEN
            GET_ST_LHS_FOR_PRICES = .FALSE.
            R_ST_LHS_FOR_PRICES = '  '
         ELSE
!            TG = TRANS_GROUP_POSITION(R_TRANS_GROUP)
            TG = R_TRANS_GROUP
            IF(TG == 0) THEN
               R_ST_LHS_FOR_PRICES = '  '
            ELSE
               R_ST_LHS_FOR_PRICES = ST_LHS_FOR_PRICES(TG)
            ENDIF
            GET_ST_LHS_FOR_PRICES = .TRUE.
         ENDIF
      RETURN


      ENTRY GET_TRANS_GROUP_END_NOX_YEAR(R_TRANS_GROUP)


         IF(.NOT. SAVE_TG_FILE_EXISTS) THEN
            GET_TRANS_GROUP_NOX_YEAR = 2100
         ELSEIF(R_TRANS_GROUP > 0 .AND.
     +                    R_TRANS_GROUP <= MAX_TRANS_GROUP_NUMBER) THEN
            TG = TRANS_GROUP_POSITION(R_TRANS_GROUP)
            IF(TG == 0) THEN
               GET_TRANS_GROUP_NOX_YEAR = 2100
            ELSE
               GET_TRANS_GROUP_END_NOX_YEAR = END_NOX_YEAR(TG)
            ENDIF
         ELSE
            GET_TRANS_GROUP_END_NOX_YEAR = 2100
         ENDIF
      RETURN
      END FUNCTION READ_TRANS_GROUPS_DATA

      ! Single_area_extention_period_price_escalation
      ! renamed to sgl_area_ext_period_price_esc
      ! merge2issue
      SUBROUTINE SGL_AREA_EXT_PERIOD_PRICE_ESC(R_YEAR,

     +                                              LOCAL_MARKET_PRICE)
      use spindriftlib
      use prod_arrays_dimensions
      USE TRANS_GROUP_VARIABLES

         INTEGER (KIND=2) :: R_YEAR
         REAL (KIND=4) :: LOCAL_MARKET_PRICE(24)
!
            LOCAL_MARKET_PRICE = LOCAL_MARKET_PRICE *
     +              PRICE_ESCALATION_RATE(1)**(R_YEAR-AVAIL_DATA_YEARS)
      END SUBROUTINE

      SUBROUTINE MULTI_AREA_EXT_PERIOD_PRICE_ESC(R_YEAR,

     +                                               TRANS_GROUP,
     +                                              LOCAL_MARKET_PRICE)
      use spindriftlib
      use prod_arrays_dimensions
      USE TRANS_GROUP_VARIABLES

         INTEGER (KIND=2) :: R_YEAR,TRANS_GROUP
         REAL (KIND=4) :: LOCAL_MARKET_PRICE(24)
!
            LOCAL_MARKET_PRICE = LOCAL_MARKET_PRICE *
     +                           PRICE_ESCALATION_RATE(TRANS_GROUP)**
     +                                        (R_YEAR-AVAIL_DATA_YEARS)
      END SUBROUTINE

!
!
      FUNCTION SAVE_TF_CHAR_VARIABLES()
      implicit none
      character(len=1) :: SAVE_TF_CHAR_VARIABLES
      INTEGER (kind=2) ::  R_TRANS_GROUP
      CHARACTER(len=1) :: GET_TRANS_GROUP_NOX_SEASON,
     +            R_GET_TRANS_GROUP_NOX_SEASON
      CHARACTER(len=3) :: GET_HOURLY_PRICE_NAME,R_GET_HOURLY_PRICE_NAME
      CHARACTER(len=35) :: GET_GROUP_NAME,R_GET_GROUP_NAME
      CHARACTER (len=35) :: GET_TRANSACTION_GROUP_NAME,
     +             R_GET_TRANSACTION_GROUP_NAME
      LOGICAL*1 VOID_LOGICAL,GET_TF_HOURLY_PRICE_NAME,
     +          GET_TF_GROUP_NAME,GET_TF_TRANSACTION_GROUP_NAME,
     +          GET_TF_TRANS_GROUP_NOX_SEASON
         SAVE_TF_CHAR_VARIABLES = 'X'
      RETURN

      ENTRY GET_HOURLY_PRICE_NAME(R_TRANS_GROUP) ! CHARACTER


         VOID_LOGICAL = GET_TF_HOURLY_PRICE_NAME(R_TRANS_GROUP,
     +                              R_GET_HOURLY_PRICE_NAME) ! CHARACTER
         GET_HOURLY_PRICE_NAME = R_GET_HOURLY_PRICE_NAME
      RETURN


      ENTRY GET_GROUP_NAME(R_TRANS_GROUP)  ! CHARACTER

          ! CHARACTER
         VOID_LOGICAL=GET_TF_GROUP_NAME(R_TRANS_GROUP,R_GET_GROUP_NAME)
         GET_GROUP_NAME = R_GET_GROUP_NAME
      RETURN

      ENTRY GET_TRANSACTION_GROUP_NAME(R_TRANS_GROUP)  ! CHARACTER



         VOID_LOGICAL = GET_TF_TRANSACTION_GROUP_NAME(R_TRANS_GROUP,
     +                        R_GET_TRANSACTION_GROUP_NAME)  ! CHARACTER
         GET_TRANSACTION_GROUP_NAME = R_GET_TRANSACTION_GROUP_NAME
      RETURN


      ENTRY GET_TRANS_GROUP_NOX_SEASON(R_TRANS_GROUP)  ! CHARACTER


         VOID_LOGICAL = GET_TF_TRANS_GROUP_NOX_SEASON(R_TRANS_GROUP,
     +                                    R_GET_TRANS_GROUP_NOX_SEASON)
         GET_TRANS_GROUP_NOX_SEASON = R_GET_TRANS_GROUP_NOX_SEASON
      RETURN
      END

!     ROUTINE TO CALCULATE THE TRANS LOAD PROBABILITY
!                 CURVE ON AN ODD SPACED GRID

      SUBROUTINE TRANSACT_LOAD_PROB(HOURS_INCREMENT,
     +                              TRANS_LOAD,
     +                              WH_LOAD,
     +                              SUM_TRANS_LOADS,
     +                              MAX_TRANS_LOAD,
     +                              MIN_TRANS_LOAD,
     +                              R_TRANS,
     +                              R_ISEAS,
     +                              R_TG)
      use mod_base_year
      use rptreccontrol
      use grx_planning_routines
      use spindriftlib
      use prod_arrays_dimensions
      use tf_decs
      USE IREC_ENDPOINT_CONTROL
      USE SIZECOM
      use globecom
      SAVE

      INTEGER*2 LOAD_POINTS
      PARAMETER (LOAD_POINTS=79)
      INTEGER*2 I,HR,IMAX,HOURS_INCREMENT,IPEAK,COUNT,
     +            INTERVALS,COUNTER,POINTS_IN_CURVE,
     +            R_TRANS,R_LOAD_POINTS,R_MAX_TRANS_LOAD_GROUPS,
     +            SAVE_MAX_TRANS_LOAD_GROUPS/0/,
     +            SAVE_TARGET_TRANS_GROUP/0/,
     +            TG_FROM_TRANS_LOAD_GROUP,
     +            R_TG,R_TRANS_OUT
      REAL BASE,OBS(LOAD_POINTS),DELTA_PROB,AREA,
     +     MIN_LPROB,OBSERVATIONS,
     +     R_LPROB(*),R_LODDUR(*)
      REAL
     +     LPROB(:,:),
     +     DX(:),
     +     R_DX,
     +     PEAK(:),
     +     R_PEAK,
     +     R_BASE,
     +     LODDUR(:,:),
     +     TRANS_LOAD(*),
     +     WH_LOAD(*),
     +     AVE_ENERGY,
     +     MAX_TRANS_LOAD,
     +     MIN_TRANS_LOAD,LOAD_VAL,PEAK_DX,BASE_DX,
     +     BASE_ADJUSTMENT,INTERVAL_HOURS
      REAL*8   SUM_TRANS_LOADS,
     +         DEMAND(:),
     +         R_DEMAND,
     +         ENERGY(LOAD_POINTS),ALPHA,PRECISN
!
      ALLOCATABLE :: LPROB,DX,PEAK,LODDUR,DEMAND
!
      REAL*4 TEMP_INTERVALS,TEMP_VALUE
      LOGICAL*1 SET_POINTS
!
      INTEGER*2   CURRENT_MONTH,
     +            LAST_SEASON/0/,PRODUCTION_PERIODS,CURRENT_YEAR
!      CHARACTER*9 CL_MONTH_NAME(13)
      CHARACTER*20 MONTH_NAME
! REPORTING
      INTEGER*2   DAYS_IN_MONTH,
     +            DAY,
     +            RPT_HR,
     +            R_ISEAS,
     +            TRANS_LBH_HOUR,
     +            TRANS_LBH_RPT_HEADER,TRANS_LBH_UNIT,
     +            DAILY_HOURS/24/,
     +            START_HOUR
      INTEGER TRANS_LBH_REC
      REAL*4      REMAIN
      LOGICAL*1
     +            TRANS_HOURLY_REPORT_NOT_OPEN/.TRUE./,
     +            YES_TRANSACT_HOURLY_REPORTS,
     +            TRANSACT_HOURLY_REPORTS,
     +            HOURLY_LOAD_BEFORE_HYDRO,
     +            YES_HOURLY_LOAD_BEFORE_HYDRO,
     +            WRITE_HOURLY_REPORT
      CHARACTER*9 CL_MONTH_NAME(13)
     +                         /'January','February','March','April',
     +                        'May','June','July','August','September',
     +                        'October','November','December','Annual'/
      CHARACTER*35 GET_GROUP_NAME

      IF(MAX_TRANS_LOAD == MIN_TRANS_LOAD) THEN ! FLAT LOAD CURVE
         MAX_TRANS_LOAD = MAX_TRANS_LOAD * 1.000001
         MIN_TRANS_LOAD = 2. * MIN_TRANS_LOAD - MAX_TRANS_LOAD
         TRANS_LOAD(HOURS_INCREMENT) = MAX_TRANS_LOAD
         TRANS_LOAD(HOURS_INCREMENT-1) = MIN_TRANS_LOAD
      ENDIF

      CURRENT_YEAR = get_BASE_YEAR() + globecom_year

      BASE_ADJUSTMENT = -999.
      INTERVALS = LOAD_POINTS/2 + 1
      DX(R_TRANS) =
     +          MAX((MAX_TRANS_LOAD-MIN_TRANS_LOAD)/FLOAT(INTERVALS-1),
     +                                                            0.01)
      POINTS_IN_CURVE = LOAD_POINTS
      COUNT = LOAD_POINTS
!
      PEAK(R_TRANS) = MAX_TRANS_LOAD
      BASE = MIN_TRANS_LOAD
!
      DO I = 1, LOAD_POINTS
         ENERGY(I)= 0.
         OBS(I) = 0.
         LPROB(I,R_TRANS) = 0.
      ENDDO
      LPROB(1,R_TRANS) = 1.
!
      LODDUR(1,R_TRANS) = BASE
      COUNT = 1
      DOWHILE (LODDUR(COUNT,R_TRANS) <
     +                   PEAK(R_TRANS) .AND. COUNT + 2 <= LOAD_POINTS )
         COUNT = COUNT + 2
         LODDUR(COUNT,R_TRANS) = LODDUR(COUNT-2,R_TRANS) + DX(R_TRANS)
      ENDDO
      IF(LODDUR(COUNT,R_TRANS) < PEAK(R_TRANS)) THEN
         TEMP_VALUE = LODDUR(COUNT,R_TRANS) - PEAK(R_TRANS)
      ENDIF
      IF(PEAK(R_TRANS) - LODDUR(COUNT-2,R_TRANS) < .0001*DX(R_TRANS))
     +                                                COUNT = COUNT - 2
      LODDUR(COUNT,R_TRANS) = PEAK(R_TRANS)
      POINTS_IN_CURVE = COUNT
      IMAX = COUNT
      INTERVALS = COUNT/2 + 1
      IPEAK = INTERVALS - 1
      DO I = COUNT + 1, LOAD_POINTS
         LODDUR(I,R_TRANS) = LODDUR(I-1,R_TRANS) + DX(R_TRANS)
      ENDDO

!     PLACES TRANS LOADS INTO LOAD_POINTS/2 - 1 INTERVALS

      DAYS_IN_MONTH = HOURS_INCREMENT/24
!
      DAY = 1
      CURRENT_YEAR = get_BASE_YEAR() + globecom_year
!
      YES_TRANSACT_HOURLY_REPORTS = TRANSACT_HOURLY_REPORTS()
      YES_HOURLY_LOAD_BEFORE_HYDRO = HOURLY_LOAD_BEFORE_HYDRO()
      WRITE_HOURLY_REPORT =
     +       (YES_TRANSACT_HOURLY_REPORTS .OR.
     +                        YES_HOURLY_LOAD_BEFORE_HYDRO)
     +                                         .AND. .NOT. TESTING_PLAN
      IF(TRANS_HOURLY_REPORT_NOT_OPEN .AND. WRITE_HOURLY_REPORT) THEN
         TRANS_HOURLY_REPORT_NOT_OPEN = .FALSE.
         TRANS_LBH_UNIT = TRANS_LBH_RPT_HEADER(TRANS_LBH_REC)
      ENDIF
!
      DO HR = 1,HOURS_INCREMENT
!
         IF(WRITE_HOURLY_REPORT) THEN
            REMAIN = MOD(FLOAT(HR),24.)
            IF(REMAIN < .001) THEN ! SUMMARIZE THE DAY
               DAY = HR/24
               START_HOUR = HR - 23
               IF(GET_REPORT_TRANS_GROUP(R_TG)) THEN ! 12/28/01.
                  TRANS_LBH_REC = rptrec(TRANS_LBH_UNIT)
                  WRITE(TRANS_LBH_UNIT,REC=TRANS_LBH_REC)
     +                     PRT_ENDPOINT(),
     +                     FLOAT(CURRENT_YEAR),
     +                     CL_MONTH_NAME(R_ISEAS),
     +                     FLOAT(DAY),
     +                     GET_GROUP_NAME(R_TG),
     +                     (TRANS_LOAD(RPT_HR)+WH_LOAD(RPT_HR),
     +                                 RPT_HR=START_HOUR,START_HOUR+23)
!     +                     FLOAT(R_TG)
                  TRANS_LBH_REC = TRANS_LBH_REC + 1
               ENDIF
            ELSE
               DAY = HR/24 + 1
            ENDIF
         ENDIF
!
         IF(TRANS_LOAD(HR) .GE. BASE) THEN
            I = AINT((TRANS_LOAD(HR) +.0001 - BASE)/DX(R_TRANS)) + 1
            IF(I.GT.IPEAK) I = IPEAK
            OBS(I) = OBS(I) + 1.
            ENERGY(I) = ENERGY(I) + TRANS_LOAD(HR)
         ENDIF
      ENDDO
!
!
      MIN_LPROB = 1./FLOAT(HOURS_INCREMENT)
      OBSERVATIONS = 0.
      DO I = 2 , INTERVALS
         COUNT = 2*(I) - 2
         OBSERVATIONS = OBSERVATIONS + OBS(I-1)
         DELTA_PROB = 1. - OBSERVATIONS/FLOAT(HOURS_INCREMENT)
!
!
!
         IF(OBS(I-1) .LE. 0.) THEN
            LPROB(COUNT+1,R_TRANS) = LPROB(COUNT-1,R_TRANS)
            LPROB(COUNT,R_TRANS) = LPROB(COUNT-1,R_TRANS)
            LODDUR(COUNT,R_TRANS) =
     +           (LODDUR(COUNT+1,R_TRANS)+LODDUR(COUNT-1,R_TRANS))/2.
         ELSE
            LODDUR(COUNT,R_TRANS) = ENERGY(I-1)/OBS(I-1)
            LPROB(COUNT+1,R_TRANS) = DELTA_PROB
            IF(LPROB(COUNT+1,R_TRANS) <
     +                           MIN_LPROB) LPROB(COUNT+1,R_TRANS) = 0.
            AREA = (ENERGY(I-1)-LODDUR(COUNT-1,R_TRANS)*OBS(I-1))/
     +         FLOAT(HOURS_INCREMENT) +
     +         LPROB(COUNT+1,R_TRANS)*
     +                (LODDUR(COUNT+1,R_TRANS)-LODDUR(COUNT-1,R_TRANS))
            LPROB(COUNT,R_TRANS) = (2*AREA -
     +         (LPROB(COUNT-1,R_TRANS)*
     +                (LODDUR(COUNT,R_TRANS)-LODDUR(COUNT-1,R_TRANS)) +
     +         LPROB(COUNT+1,R_TRANS)*
     +            (LODDUR(COUNT+1,R_TRANS)-LODDUR(COUNT,R_TRANS)))  ) /
     +         (LODDUR(COUNT+1,R_TRANS)-LODDUR(COUNT-1,R_TRANS))
!
            IF( ENERGY(I-1)/(OBS(I-1) * LODDUR(COUNT+1,R_TRANS))
     +                                  > .999999 ) THEN
               LODDUR(COUNT,R_TRANS) = LODDUR(COUNT+1,R_TRANS)
               IF(LODDUR(COUNT+1,R_TRANS) < 10000) THEN
                  LODDUR(COUNT+1,R_TRANS) =
     +                                   LODDUR(COUNT+1,R_TRANS) + .001
               ELSE
                  LODDUR(COUNT+1,R_TRANS) =
     +                                    LODDUR(COUNT+1,R_TRANS) + .01
               ENDIF
            ENDIF
            IF(LPROB(COUNT,R_TRANS) .GT. LPROB(COUNT-1,R_TRANS)) THEN
               LPROB(COUNT,R_TRANS) = LPROB(COUNT-1,R_TRANS)
            ELSEIF(LPROB(COUNT,R_TRANS) .LT.
     +                                    LPROB(COUNT+1,R_TRANS) ) THEN
               LPROB(COUNT,R_TRANS) = LPROB(COUNT+1,R_TRANS)
            ENDIF
         ENDIF
      ENDDO

!     THIS SECTION TAKES THE ROUNDING ERROR FROM THE
!     PREVIOUS ALGORITHM (SINGLE PRECISION CALC.) AND
!     DISTRIBUTES IT EVENLY ACROSS LOAD_POINTS-1 POINTS.

      COUNTER = 0.
      BASE = LODDUR(1,R_TRANS)
   50 CALL INTEG8(DEMAND(R_TRANS),
     +            LODDUR(1,R_TRANS),LPROB(1,R_TRANS),IMAX,
     +            HOURS_INCREMENT,BASE)
      IF(BASE_ADJUSTMENT == -999.) THEN
         ALPHA = LODDUR(2,R_TRANS) * FLOAT(HOURS_INCREMENT)
         ALPHA = (SUM_TRANS_LOADS-ALPHA)/(DEMAND(R_TRANS)-ALPHA)
      ELSE
         ALPHA = 1.
      ENDIF
      DO I = 2,IMAX
         LPROB(I,R_TRANS) = LPROB(I,R_TRANS)*ALPHA
      ENDDO
      LPROB(IMAX,R_TRANS) = 0.
      BASE = LODDUR(1,R_TRANS)
      CALL INTEG8(DEMAND(R_TRANS),
     +   LODDUR(1,R_TRANS),LPROB(1,R_TRANS),IMAX,
     +                      HOURS_INCREMENT,BASE)
      PRECISN = 1.
      IF(SUM_TRANS_LOADS .GT. DEMAND(R_TRANS)) THEN
         PRECISN = DEMAND(R_TRANS)/SUM_TRANS_LOADS
      ELSE
         PRECISN = SUM_TRANS_LOADS/DEMAND(R_TRANS)
      ENDIF
      COUNTER = COUNTER + 1
      IF(PRECISN .LT. .999999 .AND. COUNTER .LT. 3) GOTO 50

!     WRITE THE RESULTS TO BE READ BY PROCOST

      I = POINTS_IN_CURVE
      DOWHILE (LPROB(I,R_TRANS) == 0.)
         I = I - 1
      ENDDO
      POINTS_IN_CURVE = I + 1
      DX(R_TRANS) = (LODDUR(POINTS_IN_CURVE,R_TRANS)-BASE)/
     +                                         FLOAT(POINTS_IN_CURVE-1)
      DX(R_TRANS) = MAX(DX(R_TRANS),0.01)

      RETURN

      ENTRY INITIALIZE_TRANS_LOAD_PROB(R_MAX_TRANS_LOAD_GROUPS)

         IF(ALLOCATED(DEMAND)) DEALLOCATE(DEMAND,DX,PEAK,LODDUR,LPROB)
         ALLOCATE(DEMAND(R_MAX_TRANS_LOAD_GROUPS))
         ALLOCATE(DX(R_MAX_TRANS_LOAD_GROUPS))
         ALLOCATE(PEAK(R_MAX_TRANS_LOAD_GROUPS))
         ALLOCATE(LODDUR(LOAD_POINTS,R_MAX_TRANS_LOAD_GROUPS))
         ALLOCATE(LPROB(LOAD_POINTS,R_MAX_TRANS_LOAD_GROUPS))
!         DO I = 1, R_MAX_TRANS_LOAD_GROUPS
            DEMAND(1:R_MAX_TRANS_LOAD_GROUPS) = 0.
!         ENDDO
         DX = 0.
         PEAK = 0.
         LPROB = 0.
         LODDUR = 0.
         SAVE_MAX_TRANS_LOAD_GROUPS = R_MAX_TRANS_LOAD_GROUPS
      RETURN

      ENTRY GET_MAX_TRANS_LOAD_GROUPS(R_MAX_TRANS_LOAD_GROUPS)

         R_MAX_TRANS_LOAD_GROUPS = SAVE_MAX_TRANS_LOAD_GROUPS
      RETURN

      ENTRY GET_TRANS_LOAD_PROB(R_DEMAND,R_DX,R_PEAK,R_BASE,
     +                          R_LPROB,R_LODDUR,R_LOAD_POINTS,
     +                          R_TRANS)


         R_DEMAND = DEMAND(R_TRANS)
         R_DX = DX(R_TRANS)
         R_PEAK = PEAK(R_TRANS)
         R_BASE = LODDUR(1,R_TRANS)
         R_LOAD_POINTS = LOAD_POINTS
         DO I = 1, LOAD_POINTS
            R_LPROB(I) = LPROB(I,R_TRANS)
            R_LODDUR(I) = LODDUR(I,R_TRANS)
         ENDDO
         SAVE_TARGET_TRANS_GROUP = TG_FROM_TRANS_LOAD_GROUP(R_TRANS)
      RETURN


      ENTRY GET_TRANS_PEAK_B4_HYDRO(R_PEAK,R_TRANS)

         R_PEAK = PEAK(R_TRANS)
      RETURN

      ENTRY GET_TARGET_TRANS_GROUP(R_TRANS_OUT)

         R_TRANS_OUT = SAVE_TARGET_TRANS_GROUP
      RETURN

      ENTRY PUT_TARGET_TRANS_GROUP(R_TRANS)

         SAVE_TARGET_TRANS_GROUP = TG_FROM_TRANS_LOAD_GROUP(R_TRANS)
      RETURN
      END

!
!     ROUTINE TO CALCULATE THE HYDRO LOAD PROBABILITY
!                 CURVE ON AN ODD SPACED GRID
!

      SUBROUTINE HYDRO_LOAD_PROB(HOURS_INCREMENT,
     +                              HYDRO_LOAD,
     +                              SUM_HYDRO_LOADS,
     +                              MAX_HYDRO_LOAD,
     +                              MIN_HYDRO_LOAD,
     +                              R_HYDRO,
     +                              R_ISEAS,
     +                              R_TG)

      use spindriftlib
      use prod_arrays_dimensions
      USE SIZECOM
      use globecom
      use mod_base_year
      SAVE

      INTEGER*2 LOAD_POINTS
      PARAMETER (LOAD_POINTS=79)
      INTEGER*2 I,HR,IMAX,HOURS_INCREMENT,IPEAK,COUNT,
     +            INTERVALS,COUNTER,POINTS_IN_CURVE,
     +            R_HYDRO,R_LOAD_POINTS,R_MAX_HYDRO_LOAD_GROUPS,
     +            SAVE_MAX_HYDRO_LOAD_GROUPS/0/,
     +            SAVE_TARGET_HYDRO_GROUP/0/,
     +            TG_FROM_HYDRO_LOAD_GROUP,
     +            R_TG
      REAL BASE,OBS(LOAD_POINTS),DELTA_PROB,AREA,
     +     MIN_LPROB,OBSERVATIONS,
     +     R_LPROB(*),R_LODDUR(*)
      REAL
     +     LPROB(:,:),
     +     DX(:),
     +     R_DX,
     +     PEAK(:),
     +     R_PEAK,
     +     R_BASE,
     +     LODDUR(:,:),
     +     HYDRO_LOAD(*),
     +     AVE_ENERGY,
     +     MAX_HYDRO_LOAD,
     +     MIN_HYDRO_LOAD,LOAD_VAL,PEAK_DX,BASE_DX,
     +     BASE_ADJUSTMENT,INTERVAL_HOURS
      REAL*8   SUM_HYDRO_LOADS,
     +         DEMAND(:),
     +         R_DEMAND,
     +         ENERGY(LOAD_POINTS),ALPHA,PRECISN
!
      ALLOCATABLE :: LPROB,DX,PEAK,LODDUR,DEMAND
!
      REAL*4 TEMP_INTERVALS,TEMP_VALUE
      LOGICAL*1 SET_POINTS
!
      INTEGER*2   CURRENT_MONTH,
     +            LAST_SEASON/0/,PRODUCTION_PERIODS,CURRENT_YEAR
!      CHARACTER*9 CL_MONTH_NAME(13)
      CHARACTER*20 MONTH_NAME
! REPORTING
      INTEGER*2   DAYS_IN_MONTH,
     +            DAY,
     +            RPT_HR,
     +            R_ISEAS,
     +            HYDRO_LBH_HOUR,
     +            HYDRO_LBH_RPT_HEADER,HYDRO_LBH_UNIT,
     +            DAILY_HOURS/24/,
     +            START_HOUR
      INTEGER HYDRO_LBH_REC
      REAL*4      REMAIN
      LOGICAL*1
     +            HYDRO_HOURLY_REPORT_NOT_OPEN/.TRUE./,
     +            YES_HYDRO_HOURLY_REPORTS,
     +            HYDRO_HOURLY_REPORTS,
     +            GET_REPORT_HYDRO_GROUP,
     +            HOURLY_LOAD_BEFORE_HYDRO,
     +            YES_HOURLY_LOAD_BEFORE_HYDRO,
     +            WRITE_HOURLY_REPORT
      CHARACTER*9 CL_MONTH_NAME(13)
     +                         /'January','February','March','April',
     +                        'May','June','July','August','September',
     +                        'October','November','December','Annual'/
      CHARACTER*35 GET_GROUP_NAME

      MAX_HYDRO_LOAD = 0.
      MIN_HYDRO_LOAD = 9999999.
      SUM_HYDRO_LOADS = 0.
      DO HR = 1, HOURS_INCREMENT
         MAX_HYDRO_LOAD = MAX(MAX_HYDRO_LOAD,HYDRO_LOAD(HR))
         MIN_HYDRO_LOAD = MIN(MIN_HYDRO_LOAD,HYDRO_LOAD(HR))
         SUM_HYDRO_LOADS = SUM_HYDRO_LOADS + HYDRO_LOAD(HR)
      ENDDO
!
! 10/27/98. GAT.
!
!
      IF(MAX_HYDRO_LOAD == MIN_HYDRO_LOAD) THEN ! FLAT LOAD CURVE
         MAX_HYDRO_LOAD = MAX_HYDRO_LOAD * 1.000001
         MIN_HYDRO_LOAD = 2. * MIN_HYDRO_LOAD - MAX_HYDRO_LOAD
         HYDRO_LOAD(HOURS_INCREMENT) = MAX_HYDRO_LOAD
         HYDRO_LOAD(HOURS_INCREMENT-1) = MIN_HYDRO_LOAD
      ENDIF
!
!

      CURRENT_YEAR = get_BASE_YEAR() + globecom_YEAR


      BASE_ADJUSTMENT = -999.
      INTERVALS = LOAD_POINTS/2 + 1
      DX(R_HYDRO) =
     +          MAX((MAX_HYDRO_LOAD-MIN_HYDRO_LOAD)/FLOAT(INTERVALS-1),
     +                                                            0.01)
      POINTS_IN_CURVE = LOAD_POINTS
      COUNT = LOAD_POINTS
!
      PEAK(R_HYDRO) = MAX_HYDRO_LOAD
      BASE = MIN_HYDRO_LOAD
!
      DO I = 1, LOAD_POINTS
         ENERGY(I)= 0.
         OBS(I) = 0.
!         LPROB(I,R_HYDRO) = 0.
      ENDDO
      LPROB(1,R_HYDRO) = 1.
!
      LODDUR(1,R_HYDRO) = BASE
      COUNT = 1
      DOWHILE (LODDUR(COUNT,R_HYDRO) <
     +                   PEAK(R_HYDRO) .AND. COUNT + 2 <= LOAD_POINTS )
         COUNT = COUNT + 2
         LODDUR(COUNT,R_HYDRO) = LODDUR(COUNT-2,R_HYDRO) + DX(R_HYDRO)
      ENDDO
      IF(LODDUR(COUNT,R_HYDRO) < PEAK(R_HYDRO)) THEN
         TEMP_VALUE = LODDUR(COUNT,R_HYDRO) - PEAK(R_HYDRO)
      ENDIF
      IF(PEAK(R_HYDRO) - LODDUR(COUNT-2,R_HYDRO) < .0001*DX(R_HYDRO))
     +                                                COUNT = COUNT - 2
      LODDUR(COUNT,R_HYDRO) = PEAK(R_HYDRO)
      POINTS_IN_CURVE = COUNT
      IMAX = COUNT
      INTERVALS = COUNT/2 + 1
      IPEAK = INTERVALS - 1
      DO I = COUNT + 1, LOAD_POINTS
         LODDUR(I,R_HYDRO) = LODDUR(I-1,R_HYDRO) + DX(R_HYDRO)
      ENDDO

!     PLACES HYDRO LOADS INTO LOAD_POINTS/2 - 1 INTERVALS

      DAYS_IN_MONTH = HOURS_INCREMENT/24
!
      DAY = 1
      CURRENT_YEAR = get_BASE_YEAR() + globecom_YEAR
!
! NOTE 9/6/01 YES_HYDRO_HOURLY_REPORTS IS NOT DEFINED
      YES_HOURLY_LOAD_BEFORE_HYDRO = HOURLY_LOAD_BEFORE_HYDRO()

      WRITE_HOURLY_REPORT = YES_HOURLY_LOAD_BEFORE_HYDRO
     +                                         .AND. .NOT. TESTING_PLAN
      IF(HYDRO_HOURLY_REPORT_NOT_OPEN .AND. WRITE_HOURLY_REPORT) THEN
         HYDRO_HOURLY_REPORT_NOT_OPEN = .FALSE.

      ENDIF

      DO HR = 1,HOURS_INCREMENT

         IF(WRITE_HOURLY_REPORT) THEN
            REMAIN = MOD(FLOAT(HR),24.)
            IF(REMAIN < .001) THEN ! SUMMARIZE THE DAY
               DAY = HR/24
               START_HOUR = HR - 23

            ELSE
               DAY = HR/24 + 1
            ENDIF
         ENDIF
!
         IF(HYDRO_LOAD(HR) .GE. BASE) THEN
            I = AINT((HYDRO_LOAD(HR) +.0001 - BASE)/DX(R_HYDRO)) + 1
            IF(I.GT.IPEAK) I = IPEAK
            OBS(I) = OBS(I) + 1.
            ENERGY(I) = ENERGY(I) + HYDRO_LOAD(HR)
         ENDIF
      ENDDO
!
!
      MIN_LPROB = 1./FLOAT(HOURS_INCREMENT)
      OBSERVATIONS = 0.
      DO I = 2 , INTERVALS
         COUNT = 2*(I) - 2
         OBSERVATIONS = OBSERVATIONS + OBS(I-1)
         DELTA_PROB = 1. - OBSERVATIONS/FLOAT(HOURS_INCREMENT)
!
!
!
         IF(OBS(I-1) .LE. 0.) THEN
            LPROB(COUNT+1,R_HYDRO) = LPROB(COUNT-1,R_HYDRO)
            LPROB(COUNT,R_HYDRO) = LPROB(COUNT-1,R_HYDRO)
            LODDUR(COUNT,R_HYDRO) =
     +           (LODDUR(COUNT+1,R_HYDRO)+LODDUR(COUNT-1,R_HYDRO))/2.
         ELSE
            LODDUR(COUNT,R_HYDRO) = ENERGY(I-1)/OBS(I-1)
            LPROB(COUNT+1,R_HYDRO) = DELTA_PROB
            IF(LPROB(COUNT+1,R_HYDRO) <
     +                           MIN_LPROB) LPROB(COUNT+1,R_HYDRO) = 0.
            AREA = (ENERGY(I-1)-LODDUR(COUNT-1,R_HYDRO)*OBS(I-1))/
     +         FLOAT(HOURS_INCREMENT) +
     +         LPROB(COUNT+1,R_HYDRO)*
     +                (LODDUR(COUNT+1,R_HYDRO)-LODDUR(COUNT-1,R_HYDRO))
            LPROB(COUNT,R_HYDRO) = (2*AREA -
     +         (LPROB(COUNT-1,R_HYDRO)*
     +                (LODDUR(COUNT,R_HYDRO)-LODDUR(COUNT-1,R_HYDRO)) +
     +         LPROB(COUNT+1,R_HYDRO)*
     +            (LODDUR(COUNT+1,R_HYDRO)-LODDUR(COUNT,R_HYDRO)))  ) /
     +         (LODDUR(COUNT+1,R_HYDRO)-LODDUR(COUNT-1,R_HYDRO))
!
            IF( ENERGY(I-1)/(OBS(I-1) * LODDUR(COUNT+1,R_HYDRO))
     +                                  > .999999 ) THEN
               LODDUR(COUNT,R_HYDRO) = LODDUR(COUNT+1,R_HYDRO)
               IF(LODDUR(COUNT+1,R_HYDRO) < 10000) THEN
                  LODDUR(COUNT+1,R_HYDRO) =
     +                                   LODDUR(COUNT+1,R_HYDRO) + .001
               ELSE
                  LODDUR(COUNT+1,R_HYDRO) =
     +                                    LODDUR(COUNT+1,R_HYDRO) + .01
               ENDIF
            ENDIF
            IF(LPROB(COUNT,R_HYDRO) .GT. LPROB(COUNT-1,R_HYDRO)) THEN
               LPROB(COUNT,R_HYDRO) = LPROB(COUNT-1,R_HYDRO)
            ELSEIF(LPROB(COUNT,R_HYDRO) .LT.
     +                                    LPROB(COUNT+1,R_HYDRO) ) THEN
               LPROB(COUNT,R_HYDRO) = LPROB(COUNT+1,R_HYDRO)
            ENDIF
         ENDIF
      ENDDO

!     THIS SECTION TAKES THE ROUNDING ERROR FROM THE
!     PREVIOUS ALGORITHM (SINGLE PRECISION CALC.) AND
!     DISTRIBUTES IT EVENLY ACROSS LOAD_POINTS-1 POINTS.

      COUNTER = 0.
      BASE = LODDUR(1,R_HYDRO)
   50 CALL INTEG8(DEMAND(R_HYDRO),
     +            LODDUR(1,R_HYDRO),LPROB(1,R_HYDRO),IMAX,
     +            HOURS_INCREMENT,BASE)
      IF(BASE_ADJUSTMENT == -999.) THEN
         ALPHA = LODDUR(2,R_HYDRO) * FLOAT(HOURS_INCREMENT)
         ALPHA = (SUM_HYDRO_LOADS-ALPHA)/(DEMAND(R_HYDRO)-ALPHA)
      ELSE
         ALPHA = 1.
      ENDIF
      DO I = 2,IMAX
         LPROB(I,R_HYDRO) = LPROB(I,R_HYDRO)*ALPHA
      ENDDO
      LPROB(IMAX,R_HYDRO) = 0.
      BASE = LODDUR(1,R_HYDRO)
      CALL INTEG8(DEMAND(R_HYDRO),
     +   LODDUR(1,R_HYDRO),LPROB(1,R_HYDRO),IMAX,
     +                      HOURS_INCREMENT,BASE)
      PRECISN = 1.
      IF(SUM_HYDRO_LOADS .GT. DEMAND(R_HYDRO)) THEN
         PRECISN = DEMAND(R_HYDRO)/SUM_HYDRO_LOADS
      ELSE
         PRECISN = SUM_HYDRO_LOADS/DEMAND(R_HYDRO)
      ENDIF
      COUNTER = COUNTER + 1
      IF(PRECISN .LT. .999999 .AND. COUNTER .LT. 3) GOTO 50

!     WRITE THE RESULTS TO BE READ BY PROCOST

      I = POINTS_IN_CURVE
      DOWHILE (LPROB(I,R_HYDRO) == 0.)
         I = I - 1
      ENDDO
      POINTS_IN_CURVE = I + 1
      DX(R_HYDRO) = (LODDUR(POINTS_IN_CURVE,R_HYDRO)-BASE)/
     +                                         FLOAT(POINTS_IN_CURVE-1)
      DX(R_HYDRO) = MAX(DX(R_HYDRO),0.01)

      RETURN


      ENTRY INITIALIZE_HYDRO_LOAD_PROB(R_MAX_HYDRO_LOAD_GROUPS)

         IF(ALLOCATED(DEMAND)) DEALLOCATE(DEMAND,DX,PEAK,LODDUR,LPROB)
         ALLOCATE(DEMAND(R_MAX_HYDRO_LOAD_GROUPS))
         ALLOCATE(DX(R_MAX_HYDRO_LOAD_GROUPS))
         ALLOCATE(PEAK(R_MAX_HYDRO_LOAD_GROUPS))
         ALLOCATE(LODDUR(1000,R_MAX_HYDRO_LOAD_GROUPS))
         ALLOCATE(LPROB(1000,R_MAX_HYDRO_LOAD_GROUPS))

         DEMAND(1:R_MAX_HYDRO_LOAD_GROUPS) = 0.

         DX = 0.
         PEAK = 0.
         LPROB = 0.
         LODDUR = 0.
         SAVE_MAX_HYDRO_LOAD_GROUPS = R_MAX_HYDRO_LOAD_GROUPS
      RETURN


      ENTRY GET_MAX_HYDRO_LOAD_GROUPS(R_MAX_HYDRO_LOAD_GROUPS)

         R_MAX_HYDRO_LOAD_GROUPS = SAVE_MAX_HYDRO_LOAD_GROUPS
      RETURN

      ENTRY GET_HYDRO_LOAD_PROB(R_DEMAND,R_DX,R_PEAK,R_BASE,
     +                          R_LPROB,R_LODDUR,R_LOAD_POINTS,
     +                          R_HYDRO)

         R_DEMAND = DEMAND(R_HYDRO)
         R_DX = DX(R_HYDRO)
         R_PEAK = PEAK(R_HYDRO)
         R_BASE = LODDUR(1,R_HYDRO)
         R_LOAD_POINTS = LOAD_POINTS
         DO I = 1, LOAD_POINTS
            R_LPROB(I) = LPROB(I,R_HYDRO)
            R_LODDUR(I) = LODDUR(I,R_HYDRO)
         ENDDO

      RETURN

      ENTRY PUT_HYDRO_LOAD_PROB(R_DEMAND,R_DX,R_PEAK,R_BASE,
     +                          R_LPROB,R_LODDUR,R_LOAD_POINTS,
     +                          R_HYDRO)


         DEMAND(R_HYDRO) = R_DEMAND
         DX(R_HYDRO) = R_DX
         PEAK(R_HYDRO) = R_PEAK
         LODDUR(1,R_HYDRO) = R_BASE

         DO I = 1, R_LOAD_POINTS
            LPROB(I,R_HYDRO) = R_LPROB(I)
            LODDUR(I,R_HYDRO) = R_LODDUR(I)
         ENDDO

      RETURN


      ENTRY GET_HYDRO_PEAK_B4_HYDRO(R_PEAK,R_HYDRO)

         R_PEAK = PEAK(R_HYDRO)
      RETURN

      ENTRY GET_TARGET_HYDRO_GROUP(R_HYDRO)

         R_HYDRO = SAVE_TARGET_HYDRO_GROUP
      RETURN
      END

      function GET_MONTHLY_TL_HYDRO_MWH(R_TRANS_GROUP)
      use tf_decs
        integer(kind=2) :: GET_TRANS_LOAD_2_TRANS_GROUPS
        real(kind=4) :: GET_MONTHLY_TL_HYDRO_MWH
        integer (kind=2) :: r_trans_group

         IF(ALLOCATED(ns_tf_decs%MONTHLY_TRANS_LOAD_HYDRO_MWH)) THEN
            TRANS_POSITION =
     +                     GET_TRANS_LOAD_2_TRANS_GROUPS(R_TRANS_GROUP)

            GET_MONTHLY_TL_HYDRO_MWH =
     +      ns_tf_decs%MONTHLY_TRANS_LOAD_HYDRO_MWH(TRANS_POSITION)
         ELSE
            GET_MONTHLY_TL_HYDRO_MWH = 0.
         ENDIF
      end function get_monthly_tl_hydro_mwh

!     A SUBROUTINE TO PRODUCE CALENDAR CORRECT HOURLY MARGINAL COSTS
!                  BY MULTI-AREA TRANSACTION GROUP
!        AFTER THEY HAVE BEEN OPERATED UPON BY ENRGLIMT.FOR
      FUNCTION TRANS_LOAD_PROB_2_HOURS(LPROB4,LODDUR4,DEMAND_AFTER_EL,
     +                                 TRANS_EL_GENERATION,
     +                                 TRANS_EL_CAPACITY4,
     +                                 PEAK_AFTER_EL4,R_ISEAS,
     +                                 LOAD_HOURS_IN_PERIOD,
     +                                 R_TRANS_GROUP,R_LDCPTS_AFTER_EL)

!     20020414 AGT changed most REAL*4 to real*8, most FLOAT to dble;
!        and added copying loops at entry and exit
      use debugtrace
      use spindriftlib
      use prod_arrays_dimensions
      USE IREC_ENDPOINT_CONTROL
      use rptreccontrol
      use grx_planning_routines
      use sizecom
      use globecom
      use mod_base_year
      use tf_decs
      SAVE

!
      integer :: file_trace_tlp2h=0
      INTEGER*2   I,R_ISEAS,MONTH,DAY,MC_YEAR,
     +            R_HR,MAX_TRANS_LOAD_GROUPS,
     +            R_TRANS_GROUP,R_LDCPTS_AFTER_EL
      INTEGER*4   DAILY_MW(24),VALUES_2_ZERO
      REAL*4      LPROB4(CONVOLUTION_POINTS), ! callers' variables
     +            LODDUR4(CONVOLUTION_POINTS),
     +            TRANS_EL_CAPACITY4,
     +            PEAK_AFTER_EL4,
     +        TRANS_LOAD_PROB_2_HOURS, ! callers' assumed function-types
     +            HYDRO_LOAD_PROB_2_HOURS
      real*8      LPROB(CONVOLUTION_POINTS),LODDUR(CONVOLUTION_POINTS),
     +            TRANS_EL_CAPACITY,
     +            PEAK_AFTER_EL,
     +            CALCULATED_PEAK,CALCULATED_BASE,PEAK_DIFF,BASE_DIFF,
     +            AVE_ENERGY_DIFF,
     +            AVE_GENERATION
      REAL*8      DEMAND_AFTER_EL,
     +            TRANS_EL_GENERATION,
     +            TOTAL_DEMAND_AFTER_EL
      INTEGER*4   I4_ENERGY,TOTAL_I4_ENERGY
      real*8      R8_ENERGY,TOTAL_R8_ENERGY,LOAD_ADJUSTER,
     +            REAL8_ZERO/0.D0/
!
!     MARGINAL COSTING VARIABLES
!
      LOGICAL*1   FOUND_SORTED_HOURS,GET_THOSE_SORTED_HOURS,
     +            HOURLY_OUTPUT_NOT_OPEN/.TRUE./,
     +            INIT_TRANS_LOAD_AFTER_EL,VOID_LOGICAL,
     +            GET_TRANS_HOUR_DISTRIBUTION,
     +            GET_HYDRO_HOUR_DISTRIBUTION,
     +            GET_HYDRO_LOAD_AFTER_EL,
     +            PUT_HYDRO_LOAD_AFTER_EL,
     +            TEMP_L
      INTEGER*2   START_MONTH,END_MONTH,LOWER_MC,LOAD_HOURS_IN_PERIOD,
     +           SORT_POS(:),EL_POS,HR,ADJUSTED_HOUR,HOUR_IN_THE_SEASON
      INTEGER*2   LAST_EL_POS,TARGET_TRANS_GROUP,
     +            GET_TRANS_GROUP_POSITION,
     +            GET_TRANS_LOAD_2_TRANS_GROUPS,
     +            ZERO_GROUP/0/,
     +            R_HYDRO_GROUP
      real*4      GET_MONTHLY_TL_HYDRO_MWH,
     +            GET_MONTHLY_TL_HYDRO_MW,
     +            GET_TRANS_LOAD_AFTER_EL,
     +            GET_TRANS_HOURLY_HYDRO,
     +            GET_MONTHLY_TL_MWH,
     +            HOURLY_TRANSACTION_LOAD,
     +            GET_TRANS_PEAK_AFTER_EL,
     +            GET_TRANS_BASE_AFTER_EL
      real*8      CURRENT_PROB,
     +            TRANS_LOAD_AFTER_EL(:,:),
     +            TRANS_HOURLY_HYDRO(:,:),
     +            MONTHLY_TRANS_LOAD_MWH(:),
     +            MONTHLY_TRANS_PEAK(:),
     +            MONTHLY_TRANS_BASE(:),
     +            MONTHLY_TRANS_LOAD_HYDRO_MW(:),
     +            HOURLY_HYDRO_OUT(:),
     +            TEST_MIN
      ALLOCATABLE :: SORT_POS,TRANS_LOAD_AFTER_EL,
     +               TRANS_HOURLY_HYDRO,
     +               MONTHLY_TRANS_LOAD_MWH,
     +               MONTHLY_TRANS_PEAK,
     +               MONTHLY_TRANS_BASE,
     +               MONTHLY_TRANS_LOAD_HYDRO_MW,
     +               HOURLY_HYDRO_OUT

! passed to subroutines, these ...
! ... cannot be made real*8
      real*4 temp_ratio,HYDRO_LOAD_AFTER_EL(800),
     +                  HYDRO_LOAD_BEFORE_EL(800)
!
!
! DETAILED HYDRO_AG REPORT
!
      LOGICAL*1   WRITE_HOURLY_REPORT/.FALSE./,
     +            HYDRO_AG_REPORT_NOT_OPEN/.TRUE./
      INTEGER*2   START_HOUR,
     +            RPT_HOUR,HYDRO_AG_NO,HYDRO_AG_HEADER,
     +            CURRENT_YEAR,RPT_HR
      INTEGER*4   HYDRO_AG_REC/0/
      CHARACTER*9 CL_MONTH_NAME(13)
     +                         /'January','February','March','April',
     +                        'May','June','July','August','September',
     +                        'October','November','December','Annual'/
      real*8 REMAIN,RELATIVE_SIZE
      CHARACTER*35 GET_GROUP_NAME,TEMP_STR
!
      INTEGER*2 TRANS_LOAD_POINTS/79/,
     +            LAST_TRANS_POS,
     +            TRANS_POS
      REAL*8 TRANS_DEMAND
      REAL*4 TRANS_DX, ! REAL*4 args to external GET_TRANS_LOAD_PROB
     +       TRANS_PEAK,
     +       TRANS_BASE,
     +       TRANS_LPROB(79),
     +       TRANS_LODDUR(79),
     +       TEMP_LOAD_BEFORE_EL,
     +       TEMP_LOAD_AFTER_EL
     
      if(file_trace_tlp2h==0) then
        file_trace_tlp2h=open_trace("TRANS_LOAD_PROB_2_HOURS.trace", 
     +      rq_tf_tlp2h)
        call write_trace_message(file_trace_tlp2h, 
     +      "Tracing TRANS_LOAD_PROB_2_HOURS function.")
      end if
      
!     copy values from caller's REAL*4 variables to local real*8
      TRANS_EL_CAPACITY=dble(TRANS_EL_CAPACITY4)
      PEAK_AFTER_EL    =dble(PEAK_AFTER_EL4)
      do I=1,CONVOLUTION_POINTS
         LPROB (I)=dble(LPROB4 (I))
         LODDUR(I)=dble(LODDUR4(I))
      end do


      IF(ALLOCATED(HOURLY_HYDRO_TF)) DEALLOCATE(HOURLY_HYDRO_TF,
     +                                       HOURLY_HYDRO_OUT)
      ALLOCATE(HOURLY_HYDRO_TF(LOAD_HOURS_IN_PERIOD),
     +         HOURLY_HYDRO_OUT(LOAD_HOURS_IN_PERIOD))
      HOURLY_HYDRO_TF = REAL8_ZERO
      HOURLY_HYDRO_OUT = REAL8_ZERO
!
!
      MONTHLY_TRANS_LOAD_MWH(R_TRANS_GROUP) =
     +                            (TRANS_EL_GENERATION+DEMAND_AFTER_EL)
      ns_tf_decs%MONTHLY_TRANS_LOAD_HYDRO_MWH(R_TRANS_GROUP) =
     + TRANS_EL_GENERATION
      MONTHLY_TRANS_LOAD_HYDRO_MW(R_TRANS_GROUP) = TRANS_EL_CAPACITY
      MONTHLY_TRANS_PEAK(R_TRANS_GROUP) = PEAK_AFTER_EL
      MONTHLY_TRANS_BASE(R_TRANS_GROUP) = LODDUR(1)
!
!
!      IF(START_MONTH == 1) THEN ! START_MONTH NOT SET
      IF(R_ISEAS == 1) THEN
!
         TOTAL_DEMAND_AFTER_EL = 0.
         TOTAL_R8_ENERGY = 0.
         TOTAL_I4_ENERGY = 0
!
      ENDIF


      IF(ALLOCATED(SORT_POS)) DEALLOCATE(SORT_POS)
      ALLOCATE(SORT_POS(LOAD_HOURS_IN_PERIOD))

      VOID_LOGICAL = GET_TRANS_HOUR_DISTRIBUTION(R_TRANS_GROUP,
     +                                                SORT_POS)
!
!
!
      LPROB(R_LDCPTS_AFTER_EL+1) = 0.0
      LODDUR(R_LDCPTS_AFTER_EL+1) = LODDUR(R_LDCPTS_AFTER_EL)
!
      EL_POS = 2
      LAST_EL_POS = 1
      TOTAL_R8_ENERGY = 0.
!
      CALCULATED_PEAK = 0.
      CALCULATED_BASE = 9999999.
!
      CALL GET_TARGET_TRANS_GROUP(TARGET_TRANS_GROUP)
      TRANS_POSITION = GET_TRANS_GROUP_POSITION(TARGET_TRANS_GROUP)

!
! MAJOR RE-WRITE OF THE ROUTINE: 02/21/02.
!
!
      CALL GET_TRANS_LOAD_PROB( TRANS_DEMAND,
     +                          TRANS_DX,
     +                          TRANS_PEAK,
     +                          TRANS_BASE,
     +                          TRANS_LPROB,
     +                          TRANS_LODDUR,
     +                          TRANS_LOAD_POINTS,
     +                          R_TRANS_GROUP)

!
      EL_POS = 2
      LAST_EL_POS = 1
      TRANS_POS = 2
      LAST_TRANS_POS = 1
      TOTAL_R8_ENERGY = 0.
!
      CALCULATED_PEAK = 0.
      CALCULATED_BASE = 9999999.
!
      DO HR = 1, LOAD_HOURS_IN_PERIOD
!
!
         HOUR_IN_THE_SEASON = SORT_POS(HR)
!
         IF(ABS(TRANS_EL_GENERATION) <= 0.0001) THEN
            TRANS_LOAD_AFTER_EL(HOUR_IN_THE_SEASON,R_TRANS_GROUP) =
     +                      HOURLY_TRANSACTION_LOAD(HOUR_IN_THE_SEASON,
     +                                                  TRANS_POSITION)
            TRANS_LOAD_AFTER_EL(HOUR_IN_THE_SEASON,ZERO_GROUP) =
     +                      HOURLY_TRANSACTION_LOAD(HOUR_IN_THE_SEASON,
     +                                                  TRANS_POSITION)
         ELSE
!
            CURRENT_PROB = 1. - dble(HR-1)/dble(LOAD_HOURS_IN_PERIOD)
!
            DOWHILE(CURRENT_PROB <= LPROB(EL_POS))
               EL_POS = EL_POS + 1
               LAST_EL_POS = EL_POS - 1
            ENDDO
!
            IF(LPROB(EL_POS) - LPROB(EL_POS+1) < .000001 .AND.
     +                                         LPROB(EL_POS) > 0.) THEN
               EL_POS = EL_POS + 1
            ENDIF
!
! Greg, the above has a problem when three or more LPROBs are the same
! value.  This happened with the WSCC area 12/167/98
!
            if(abs(LPROB(LAST_EL_POS)-LPROB(EL_POS)) > .000001) then
               temp_ratio = (LPROB(LAST_EL_POS)-CURRENT_PROB)/
     +                       (LPROB(LAST_EL_POS)-LPROB(EL_POS))
            endif

            TEMP_LOAD_AFTER_EL =
     +                  LODDUR(LAST_EL_POS) +
     +              (LODDUR(EL_POS) - LODDUR(LAST_EL_POS)) * temp_ratio

            DOWHILE(CURRENT_PROB <= TRANS_LPROB(TRANS_POS))
               TRANS_POS = TRANS_POS + 1
               LAST_TRANS_POS = TRANS_POS - 1
            ENDDO
!
            IF(TRANS_POS < TRANS_LOAD_POINTS) THEN
               IF(TRANS_LPROB(TRANS_POS) -
     +            TRANS_LPROB(TRANS_POS+1) < .000001 .AND.
     +                                TRANS_LPROB(TRANS_POS) > 0.) THEN
                  TRANS_POS = TRANS_POS + 1
               ENDIF
            ENDIF
!
! Greg, the above has a problem when three or more LPROBs are the same
! value.  This happened with the WSCC area 12/167/98
!
           if(abs(TRANS_LPROB(LAST_TRANS_POS)-TRANS_LPROB(TRANS_POS)) >
     +                                                    .000001) then
               temp_ratio = (TRANS_LPROB(LAST_TRANS_POS)-CURRENT_PROB)/
     +                    (TRANS_LPROB(LAST_TRANS_POS) -
     +                                          TRANS_LPROB(TRANS_POS))
            endif
!
            TEMP_LOAD_BEFORE_EL =
     +                  TRANS_LODDUR(LAST_TRANS_POS) +
     +               (TRANS_LODDUR(TRANS_POS) -
     +                       TRANS_LODDUR(LAST_TRANS_POS)) * temp_ratio
!
            HOURLY_HYDRO_TF(HOUR_IN_THE_SEASON) =
     +                         TEMP_LOAD_BEFORE_EL - TEMP_LOAD_AFTER_EL

            IF( ABS(HOURLY_HYDRO_TF(HOUR_IN_THE_SEASON)) < .001) THEN
               TEMP_LOAD_BEFORE_EL = TEMP_LOAD_AFTER_EL
               HOURLY_HYDRO_TF(HOUR_IN_THE_SEASON) =
     +                         TEMP_LOAD_BEFORE_EL - TEMP_LOAD_AFTER_EL
            ENDIF
!
            TRANS_LOAD_AFTER_EL(HOUR_IN_THE_SEASON,R_TRANS_GROUP) =
     +            HOURLY_TRANSACTION_LOAD(HOUR_IN_THE_SEASON,
     +                                             TRANS_POSITION) +
     +                         TEMP_LOAD_AFTER_EL - TEMP_LOAD_BEFORE_EL

         ENDIF

         CALCULATED_PEAK = MAX(CALCULATED_PEAK,
     +           HOURLY_HYDRO_TF(HOUR_IN_THE_SEASON))
         CALCULATED_BASE = MIN(CALCULATED_BASE,
     +                    HOURLY_HYDRO_TF(HOUR_IN_THE_SEASON))
!
         TOTAL_R8_ENERGY = TOTAL_R8_ENERGY +
     +                       HOURLY_HYDRO_TF(HOUR_IN_THE_SEASON)
!
         IF(HOUR_IN_THE_SEASON == 15) THEN
            CALCULATED_PEAK = CALCULATED_PEAK
         ENDIF
!
      ENDDO
!
      IF(TOTAL_R8_ENERGY > 0.00001) THEN
         LOAD_ADJUSTER = TRANS_EL_GENERATION/TOTAL_R8_ENERGY
      ELSE
         LOAD_ADJUSTER = 1.0
      ENDIF
!
      TEST_MIN =        TRANS_EL_GENERATION  -
     +                       CALCULATED_BASE*dble(LOAD_HOURS_IN_PERIOD)
!
      AVE_GENERATION =      TRANS_EL_GENERATION /LOAD_HOURS_IN_PERIOD
! 061609. FOR SMALL HYDRO IN FPL PROBLEM
      IF(DEMAND_AFTER_EL > 0.0001) THEN
         RELATIVE_SIZE = ABS(TRANS_EL_GENERATION - TOTAL_R8_ENERGY)/
     +                                                  DEMAND_AFTER_EL
      ELSE
         RELATIVE_SIZE = 1.0
      ENDIF

      HOURLY_HYDRO_OUT = HOURLY_HYDRO_TF
      
      IF( (LOAD_ADJUSTER > 1.000001 .OR. LOAD_ADJUSTER < .999999) .AND.
     +         TRANS_EL_CAPACITY - CALCULATED_BASE > .1 .AND.
     +               CALCULATED_PEAK - CALCULATED_BASE > .1 .AND.
     +                                  TEST_MIN > 0. .AND.
     +                                  TEST_MIN < 75000000. .AND.
     +                         AVE_GENERATION < TRANS_EL_CAPACITY) THEN

        call write_trace_message(file_trace_tlp2h, 
     +      "HOURLY_HYDRO_TF arguments " // 
     +      "from wh_objt:TRANS_LOAD_PROB_2_HOURS")
            
        call write_trace_real8s(file_trace_tlp2h, "HOURLY_HYDRO_TF", 
     +      HOURLY_HYDRO_TF)
        call write_trace_real8(file_trace_tlp2h, "CALCULATED_BASE", 
     +       CALCULATED_BASE)
        call write_trace_real8(file_trace_tlp2h, "CALCULATED_PEAK", 
     +       CALCULATED_PEAK)
        call write_trace_real8s(file_trace_tlp2h, "HOURLY_HYDRO_OUT", 
     +       HOURLY_HYDRO_OUT)
        call write_trace_real8(file_trace_tlp2h, "TRANS_EL_CAPACITY", 
     +       TRANS_EL_CAPACITY)
        call write_trace_real8(file_trace_tlp2h, "TRANS_EL_GENERATION", 
     +      TRANS_EL_GENERATION)
        call write_trace_int2(file_trace_tlp2h, "LOAD_HOURS_IN_PERIOD", 
     +       LOAD_HOURS_IN_PERIOD)

               call MONTHNonlinearlyMap(
     +           HOURLY_HYDRO_TF,
     +           CALCULATED_BASE,
     +           CALCULATED_PEAK,
     +           HOURLY_HYDRO_OUT,
     +           CALCULATED_BASE,
     +           TRANS_EL_CAPACITY,
     +           TRANS_EL_GENERATION,
     +           LOAD_HOURS_IN_PERIOD)

      ELSEIF(     TRANS_EL_GENERATION  > 0.) THEN
         IF(TEST_MIN <= 0.) THEN
            call write_trace_message(file_trace_tlp2h, 
     +          "1: See .MSG file (unit 4)")
            WRITE(4,*) "HYDRO MINIMUM EXCEEDED IN MONTHNONLINEARMAP"
            WRITE(4,*) "TRANSACTION GROUP INDEX = ",R_TRANS_GROUP,
     +                                             " In month ",R_ISEAS
         ELSEIF(TEST_MIN >= 75000000.) THEN
            call write_trace_message(file_trace_tlp2h, 
     +          "2. See .MSG file (unit 4)")
                
            WRITE(4,*) "HYDRO MAXIMUM EXCEEDED IN MONTHNONLINEARMAP"
            WRITE(4,*) "TRANSACTION GROUP INDEX = ",R_TRANS_GROUP,
     +                                             " In month ",R_ISEAS
         ENDIF
      ENDIF
! 01/06/03.
      DO HOUR_IN_THE_SEASON = 1, LOAD_HOURS_IN_PERIOD
         TRANS_LOAD_AFTER_EL(HOUR_IN_THE_SEASON,R_TRANS_GROUP) =
     +            HOURLY_TRANSACTION_LOAD(HOUR_IN_THE_SEASON,
     +                                                TRANS_POSITION) -
     +                             HOURLY_HYDRO_OUT(HOUR_IN_THE_SEASON)
         TRANS_HOURLY_HYDRO(HOUR_IN_THE_SEASON,R_TRANS_GROUP) =
     +                             HOURLY_HYDRO_OUT(HOUR_IN_THE_SEASON)
      ENDDO
!
      TOTAL_DEMAND_AFTER_EL = TOTAL_DEMAND_AFTER_EL + DEMAND_AFTER_EL
!
      DEALLOCATE(SORT_POS)
!
      TRANS_LOAD_PROB_2_HOURS = LOAD_ADJUSTER
!
!     copy values to caller's REAL*4 variables from local real*8
      TRANS_EL_CAPACITY4=sngl(TRANS_EL_CAPACITY)
      PEAK_AFTER_EL4=sngl(PEAK_AFTER_EL)
      do I=1,CONVOLUTION_POINTS
         LPROB4 (I)=sngl(LPROB (I))
         LODDUR4(I)=sngl(LODDUR(I))
      end do
      RETURN

      ENTRY HYDRO_LOAD_PROB_2_HOURS(LPROB4,LODDUR4,DEMAND_AFTER_EL,
     +                                 TRANS_EL_GENERATION,
     +                                 TRANS_EL_CAPACITY4,
     +                                 PEAK_AFTER_EL4,
     +                                 R_ISEAS,
     +                                 LOAD_HOURS_IN_PERIOD,
     +                                 R_TRANS_GROUP,
     +                                 R_LDCPTS_AFTER_EL,
     +                                 R_HYDRO_GROUP)

!
!
!     copy values from caller's REAL*4 variables to local real*8
      TRANS_EL_CAPACITY=dble(TRANS_EL_CAPACITY4)
      PEAK_AFTER_EL    =dble(PEAK_AFTER_EL4)
      do I=1,CONVOLUTION_POINTS
         LPROB (I)=dble(LPROB4 (I))
         LODDUR(I)=dble(LODDUR4(I))
      end do
      TEMP_L = GET_HYDRO_LOAD_AFTER_EL(HYDRO_LOAD_BEFORE_EL,
     +                                 LOAD_HOURS_IN_PERIOD,
     +                                 R_HYDRO_GROUP)
!
      HYDRO_LOAD_AFTER_EL = 0.
!
      MONTHLY_TRANS_LOAD_MWH(R_TRANS_GROUP) =
     +                            (TRANS_EL_GENERATION+DEMAND_AFTER_EL)
      ns_tf_decs%MONTHLY_TRANS_LOAD_HYDRO_MWH(R_TRANS_GROUP) =
     + TRANS_EL_GENERATION
      MONTHLY_TRANS_LOAD_HYDRO_MW(R_TRANS_GROUP) = TRANS_EL_CAPACITY
      MONTHLY_TRANS_PEAK(R_TRANS_GROUP) = PEAK_AFTER_EL
      MONTHLY_TRANS_BASE(R_TRANS_GROUP) = LODDUR(1)
!
!
!      IF(START_MONTH == 1) THEN ! START_MONTH NOT SET
      IF(R_ISEAS == 1) THEN
!
         TOTAL_DEMAND_AFTER_EL = 0.
         TOTAL_R8_ENERGY = 0.
         TOTAL_I4_ENERGY = 0
!
      ENDIF
!
!      CALL GET_CHRONO_HOURS_PER_MONTH(R_ISEAS,LOAD_HOURS_IN_PERIOD)
!
      IF(ALLOCATED(SORT_POS)) DEALLOCATE(SORT_POS)
      ALLOCATE(SORT_POS(LOAD_HOURS_IN_PERIOD))
!
!
!      CALL GET_CHRONO_HOUR_DISTRIBUTION(R_ISEAS,SORT_POS)
!
!
      VOID_LOGICAL = GET_HYDRO_HOUR_DISTRIBUTION(R_HYDRO_GROUP,
     +                                                SORT_POS)
!
!
!
      LPROB(R_LDCPTS_AFTER_EL+1) = 0.0
      LODDUR(R_LDCPTS_AFTER_EL+1) = LODDUR(R_LDCPTS_AFTER_EL)
!
      EL_POS = 2
      LAST_EL_POS = 1
      TOTAL_R8_ENERGY = 0.
!
      CALCULATED_PEAK = 0.
      CALCULATED_BASE = 9999999.
!
      CALL GET_TARGET_TRANS_GROUP(TARGET_TRANS_GROUP)
      TRANS_POSITION = GET_TRANS_GROUP_POSITION(TARGET_TRANS_GROUP)
!
      DO HR = 1, LOAD_HOURS_IN_PERIOD

         HOUR_IN_THE_SEASON = SORT_POS(HR)

         IF(ABS(TRANS_EL_GENERATION) <= 0.0001) THEN
            HYDRO_LOAD_AFTER_EL(HOUR_IN_THE_SEASON) =
     +                         HYDRO_LOAD_BEFORE_EL(HOUR_IN_THE_SEASON)
!
            TRANS_LOAD_AFTER_EL(HOUR_IN_THE_SEASON,R_TRANS_GROUP) =
     +                      HOURLY_TRANSACTION_LOAD(HOUR_IN_THE_SEASON,
     +                                                  TRANS_POSITION)
            TRANS_LOAD_AFTER_EL(HOUR_IN_THE_SEASON,ZERO_GROUP) =
     +                      HOURLY_TRANSACTION_LOAD(HOUR_IN_THE_SEASON,
     +                                                  TRANS_POSITION)
         ELSE

            CURRENT_PROB = 1. - dble(HR-1)/dble(LOAD_HOURS_IN_PERIOD)

            DOWHILE(CURRENT_PROB <= LPROB(EL_POS))
               EL_POS = EL_POS + 1
               LAST_EL_POS = EL_POS - 1
            ENDDO

            IF(LPROB(EL_POS) - LPROB(EL_POS+1) < .000001 .AND.

     +                                         LPROB(EL_POS) > 0.) THEN
               EL_POS = EL_POS + 1
            ENDIF
!
! Greg, the above has a problem when three or more LPROBs are the same
! value.  This happened with the WSCC area 12/167/98
!
            if(abs(LPROB(LAST_EL_POS)-LPROB(EL_POS)) > .000001) then
               temp_ratio = (LPROB(LAST_EL_POS)-CURRENT_PROB)/
     +                       (LPROB(LAST_EL_POS)-LPROB(EL_POS))
            endif

            HYDRO_LOAD_AFTER_EL(HOUR_IN_THE_SEASON) =
     +                  LODDUR(LAST_EL_POS) +
     +              (LODDUR(EL_POS) - LODDUR(LAST_EL_POS)) * temp_ratio

         ENDIF
!
         CALCULATED_PEAK = MAX(CALCULATED_PEAK,
     +            dble(HYDRO_LOAD_AFTER_EL(HOUR_IN_THE_SEASON)))
         CALCULATED_BASE = MIN(CALCULATED_BASE,
     +            dble(HYDRO_LOAD_AFTER_EL(HOUR_IN_THE_SEASON)))
!
         TOTAL_R8_ENERGY = TOTAL_R8_ENERGY +
     +             HYDRO_LOAD_AFTER_EL(HOUR_IN_THE_SEASON)
!
      ENDDO
!
      PEAK_DIFF = PEAK_AFTER_EL - CALCULATED_PEAK
      BASE_DIFF = LODDUR(1) - CALCULATED_BASE
      AVE_ENERGY_DIFF = ABS(     DEMAND_AFTER_EL  - TOTAL_R8_ENERGY)/
     +                                  dble(MAX(HOUR_IN_THE_SEASON,1))
!
      IF(TOTAL_R8_ENERGY > 0.00001) THEN
         LOAD_ADJUSTER = DEMAND_AFTER_EL/TOTAL_R8_ENERGY
      ELSE
         LOAD_ADJUSTER = 1.0
      ENDIF
      IF(AVE_ENERGY_DIFF > 1.) THEN
         DAY = 1
         CURRENT_YEAR = get_BASE_YEAR() + globecom_YEAR
         WRITE_HOURLY_REPORT = .TRUE.
         IF(HYDRO_AG_REPORT_NOT_OPEN .AND. WRITE_HOURLY_REPORT) THEN
            HYDRO_AG_REPORT_NOT_OPEN = .FALSE.
            HYDRO_AG_NO = HYDRO_AG_HEADER(HYDRO_AG_REC)
         ENDIF
!
         TOTAL_R8_ENERGY = 0.
         DO HR = 1, LOAD_HOURS_IN_PERIOD
            HYDRO_LOAD_AFTER_EL(HR) =
     +              HYDRO_LOAD_AFTER_EL(HR)*LOAD_ADJUSTER

           TOTAL_R8_ENERGY = TOTAL_R8_ENERGY + HYDRO_LOAD_AFTER_EL(HR)
!
! FANCY STUFF: ALLOW FOR NEGATIVE TRANSACTION GROUP LOADS.
!
            TRANS_LOAD_AFTER_EL(HR,R_TRANS_GROUP) =
     +                 HOURLY_TRANSACTION_LOAD(HR,TRANS_POSITION) -
     +                 (HYDRO_LOAD_BEFORE_EL(HR) -
     +                                        HYDRO_LOAD_AFTER_EL(HR))
            TRANS_LOAD_AFTER_EL(HR,ZERO_GROUP) =
     +                  TRANS_LOAD_AFTER_EL(HR,ZERO_GROUP) -
     +                (HYDRO_LOAD_BEFORE_EL(HR) -
     +                                        HYDRO_LOAD_AFTER_EL(HR))

            IF(WRITE_HOURLY_REPORT) THEN
               REMAIN = MOD(dble(HR),24.)
               IF(REMAIN < .001) THEN ! SUMMARIZE THE DAY
                  DAY = HR/24
                  START_HOUR = HR - 23
                  IF(GET_REPORT_TRANS_GROUP(R_TRANS_GROUP)) THEN
!
                     TEMP_STR = GET_GROUP_NAME(R_TRANS_GROUP)
                     WRITE(HYDRO_AG_NO,REC=HYDRO_AG_REC)
     +                     PRT_ENDPOINT(),
     +                     FLOAT(CURRENT_YEAR),
     +                     CL_MONTH_NAME(R_ISEAS),
     +                     FLOAT(DAY),
     +                     "Before "//TEMP_STR(1:28),
     +                     (HYDRO_LOAD_BEFORE_EL(RPT_HR),
     +                                 RPT_HR=START_HOUR,START_HOUR+23)
                     HYDRO_AG_REC = HYDRO_AG_REC + 1
!
                     WRITE(HYDRO_AG_NO,REC=HYDRO_AG_REC)
     +                     PRT_ENDPOINT(),
     +                     FLOAT(CURRENT_YEAR),
     +                     CL_MONTH_NAME(R_ISEAS),
     +                     FLOAT(DAY),
     +                     GET_GROUP_NAME(R_TRANS_GROUP),(sngl
     +                     (TRANS_LOAD_AFTER_EL(RPT_HR,R_TRANS_GROUP)),
     +                                 RPT_HR=START_HOUR,START_HOUR+23)
                     HYDRO_AG_REC = HYDRO_AG_REC + 1
!
                  ENDIF ! GET_REPORT_TRANS
               ELSE
                  DAY = HR/24 + 1
               ENDIF ! REMAIN
            ENDIF ! WRITE_HOURLY_REPORT
         ENDDO
      ENDIF
!
      TOTAL_DEMAND_AFTER_EL = TOTAL_DEMAND_AFTER_EL + DEMAND_AFTER_EL
!
      DEALLOCATE(SORT_POS)
!
      TEMP_L = PUT_HYDRO_LOAD_AFTER_EL(HYDRO_LOAD_AFTER_EL,
     +                                 LOAD_HOURS_IN_PERIOD,
     +                                 R_HYDRO_GROUP)
!
      HYDRO_LOAD_PROB_2_HOURS = LOAD_ADJUSTER
!
!     copy values to caller's REAL*4 variables from local real*8
      TRANS_EL_CAPACITY4=sngl(TRANS_EL_CAPACITY)
      PEAK_AFTER_EL4    =sngl(PEAK_AFTER_EL)
      do I=1,CONVOLUTION_POINTS
         LPROB4 (I)=sngl(LPROB (I))
         LODDUR4(I)=sngl(LODDUR(I))
      end do
      RETURN

      ENTRY GET_TRANS_LOAD_AFTER_EL(R_HR,R_TRANS_GROUP)

         TRANS_POSITION = GET_TRANS_LOAD_2_TRANS_GROUPS(R_TRANS_GROUP)
         IF(TRANS_POSITION > 0) THEN
            GET_TRANS_LOAD_AFTER_EL =
     +         sngl(TRANS_LOAD_AFTER_EL(R_HR,TRANS_POSITION))
         ELSE
            GET_TRANS_LOAD_AFTER_EL = 0.
         ENDIF
      RETURN

      ENTRY GET_TRANS_HOURLY_HYDRO(R_HR,R_TRANS_GROUP)

         IF(ALLOCATED(TRANS_HOURLY_HYDRO)) THEN
            TRANS_POSITION =
     +                     GET_TRANS_LOAD_2_TRANS_GROUPS(R_TRANS_GROUP)
            GET_TRANS_HOURLY_HYDRO =
     +         sngl(TRANS_HOURLY_HYDRO(R_HR,TRANS_POSITION))
         ELSE
            GET_TRANS_HOURLY_HYDRO = 0.
         ENDIF
      RETURN

      ENTRY GET_TRANS_PEAK_AFTER_EL(R_TRANS_GROUP)

         IF(ALLOCATED(MONTHLY_TRANS_PEAK)) THEN
            TRANS_POSITION =
     +                     GET_TRANS_LOAD_2_TRANS_GROUPS(R_TRANS_GROUP)
            GET_TRANS_PEAK_AFTER_EL =
     +               sngl(MONTHLY_TRANS_PEAK(TRANS_POSITION))
         ELSE
            GET_TRANS_PEAK_AFTER_EL = 0.
         ENDIF
      RETURN

      ENTRY GET_TRANS_BASE_AFTER_EL(R_TRANS_GROUP)

         IF(ALLOCATED(MONTHLY_TRANS_BASE)) THEN
            TRANS_POSITION =

     +                     GET_TRANS_LOAD_2_TRANS_GROUPS(R_TRANS_GROUP)
            GET_TRANS_BASE_AFTER_EL =
     +                         sngl(MONTHLY_TRANS_BASE(TRANS_POSITION))
         ELSE
            GET_TRANS_BASE_AFTER_EL = 0.
         ENDIF
      RETURN


      ENTRY GET_MONTHLY_TL_MWH(R_TRANS_GROUP)

         TRANS_POSITION = GET_TRANS_LOAD_2_TRANS_GROUPS(R_TRANS_GROUP)
         IF(TRANS_POSITION > 0.) THEN
            GET_MONTHLY_TL_MWH =
     +         sngl(MONTHLY_TRANS_LOAD_MWH(TRANS_POSITION))
         ELSE
            GET_MONTHLY_TL_MWH = 0.
         ENDIF
      RETURN


      ENTRY GET_MONTHLY_TL_HYDRO_MW(R_TRANS_GROUP)



         IF(ALLOCATED(MONTHLY_TRANS_LOAD_HYDRO_MW)) THEN
            TRANS_POSITION =
     +                     GET_TRANS_LOAD_2_TRANS_GROUPS(R_TRANS_GROUP)
            GET_MONTHLY_TL_HYDRO_MW =
     +         sngl(MONTHLY_TRANS_LOAD_HYDRO_MW(TRANS_POSITION))
         ELSE
            GET_MONTHLY_TL_HYDRO_MW = 0.
         ENDIF
      RETURN


      ENTRY INIT_TRANS_LOAD_AFTER_EL(MAX_TRANS_LOAD_GROUPS,
     +                                            LOAD_HOURS_IN_PERIOD)


!
         IF(MAX_TRANS_LOAD_GROUPS*LOAD_HOURS_IN_PERIOD == 0) THEN
            WRITE(4,*) "BAD TRANSACTIONS LOAD DATA IN PROCOST"
            WRITE(4,*) '*** line 6965 TF_OBJT.FOR ***'

         ENDIF
         IF(ALLOCATED(TRANS_LOAD_AFTER_EL))
     +               DEALLOCATE( TRANS_LOAD_AFTER_EL,
     +                           TRANS_HOURLY_HYDRO,
     +                           MONTHLY_TRANS_LOAD_MWH,
     +              ns_tf_decs%MONTHLY_TRANS_LOAD_HYDRO_MWH,
     +                           MONTHLY_TRANS_LOAD_HYDRO_MW,
     +                           MONTHLY_TRANS_PEAK,
     +                           MONTHLY_TRANS_BASE)
         ALLOCATE(TRANS_LOAD_AFTER_EL(
     +                   LOAD_HOURS_IN_PERIOD,0:MAX_TRANS_LOAD_GROUPS))
         ALLOCATE(TRANS_HOURLY_HYDRO(
     +                   LOAD_HOURS_IN_PERIOD,0:MAX_TRANS_LOAD_GROUPS))
         ALLOCATE(MONTHLY_TRANS_LOAD_MWH(0:MAX_TRANS_LOAD_GROUPS))
        ALLOCATE(
     + ns_tf_decs%MONTHLY_TRANS_LOAD_HYDRO_MWH(0:MAX_TRANS_LOAD_GROUPS))
         ALLOCATE(MONTHLY_TRANS_LOAD_HYDRO_MW(0:MAX_TRANS_LOAD_GROUPS))
         ALLOCATE(MONTHLY_TRANS_PEAK(0:MAX_TRANS_LOAD_GROUPS))
         ALLOCATE(MONTHLY_TRANS_BASE(0:MAX_TRANS_LOAD_GROUPS))
!
         TRANS_LOAD_AFTER_EL = REAL8_ZERO
         TRANS_HOURLY_HYDRO = REAL8_ZERO
         MONTHLY_TRANS_LOAD_MWH = REAL8_ZERO
         ns_tf_decs%MONTHLY_TRANS_LOAD_HYDRO_MWH = REAL8_ZERO
         MONTHLY_TRANS_LOAD_HYDRO_MW = REAL8_ZERO
         MONTHLY_TRANS_PEAK = REAL8_ZERO
         MONTHLY_TRANS_BASE = REAL8_ZERO
!
         INIT_TRANS_LOAD_AFTER_EL = .TRUE.
!
      RETURN
      END
!
!
!
!
!                  ROUTINE TO CREATE A SCENARIO MAKER FILE
!
!                           COPYRIGHT (C) 1998
!                        M.S. GERBER & ASSOCIATES, INC.
!                           ALL RIGHTS RESERVED


      SUBROUTINE SCENARIO_MAKER_OBJECT
      use end_routine, only: end_program, er_message
      use rptreccontrol
      use grx_planning_routines
      use mod_base_year
      use spindriftlib
      use prod_arrays_dimensions
      USE IREC_ENDPOINT_CONTROL
      use sizecom
      implicit none

      INTEGER*2
     +                  SCENARIO_YEAR,
     +                  TEMP_SCENARIO_YEAR,
     +                  TF_YEAR,TEMP_YEAR,year_ord,
     +                  STUDY_BASE_YEAR/0/
      CHARACTER*1
     +                  RECORD_IS_ACTIVE,
     +                  TIME_FRAME,
     +                  TEMP_RECORD_IS_ACTIVE

      REAL*4
     +                  MONTHLY_VALUES(12),
     +                  ANNUAL_VALUE

      CHARACTER*40
     +                  SCENARIO_VARIABLE,
     +                  TEMP_SCENARIO_VARIABLE
      INTEGER*2 DELETE,INUNIT,IREC,LRECL/128/,TEMP_DELETE,IREC_OFFSET
      INTEGER IOS,IOS_BASE
      INTEGER*2   UNIT_NUM/10/,
     +               R_MAKER_TABLES,
     +               HOURLY_REFERENCE_NUMBER,
     +               FIRST_IREC,LAST_IREC
      CHARACTER*5 BASE_FILE_NAME,OVERLAY_FAMILY_NAME,
     +            SCENARIO_MAKER_FILE,
     +            HOURLY_REFERENCE_NAME
      CHARACTER*256 FILE_NAME
      CHARACTER*256 BASE_FILE_DIRECTORY
      CHARACTER*256 DATA_DRIVE,OUTPUT_DIRECTORY
      LOGICAL*4   FILE_EXISTS,SCENARIO_MAKER_FILE_EXISTS/.FALSE./,
     +            R_SCENARIO_MAKER_FILE_EXISTS
!

! DECLARATION FOR DBREAD COMMON BLOCK

      CHARACTER*1024 RECLN
! DECLARATION FOR DAY TYPE DETERMINANTS
      CHARACTER*16 FILE_TYPE/'Scenario Maker  '/
      CHARACTER*2 SCENARIO_MAKER_OL/'BC'/
!
! MULTI-FILE VARIABLES
!
      INTEGER FILE_NUMBER,FILE_ID
      INTEGER*2 R_FILE_NUMBER
      INTEGER MAX_SM_FILES
      INTEGER*2 BC_MASTR_REC,CUR_REC,UNIT_NO
      INTEGER*2 OL_MASTR_REC
      PARAMETER (MAX_SM_FILES=10)
      INTEGER*2 MAKER_TABLES_IN_FILE(0:MAX_SM_FILES-1)
      CHARACTER*2
     +            SM_MULTI_OL_CODES(0:9)/MAX_SM_FILES*'BC'/
      CHARACTER*5 SM_FILE_BASE_NAMES(0:MAX_SM_FILES-1),
     +            VOID_CHR,
     +            OVERLAY_FAMILY_NAMES(0:MAX_SM_FILES-1)
      CHARACTER*2 SM_FILE_CODES(0:MAX_SM_FILES-1)/
     +                                'SM','S1','S2','S3','S4','S5',
     +                                'S6','S7','S8','S9'/,
     +            FILE_CODE
      CHARACTER*6 SM_FILE_BINARY_NAMES(0:MAX_SM_FILES-1)/'SCENM',
     +                                                   'SCEN1',
     +                                                   'SCEN2',
     +                                                   'SCEN3',
     +                                                   'SCEN4',
     +                                                   'SCEN5',
     +                                                   'SCEN6',
     +                                                   'SCEN7',
     +                                                   'SCEN8',
     +                                                   'SCEN9'/,
     +            BINARY_FILE_NAME
      LOGICAL ACTIVE_BASE_SM_FILES(0:MAX_SM_FILES-1)/
     +                                           MAX_SM_FILES*.FALSE./,
     +        ACTIVE_OVERLAY_SM_FILES(0:MAX_SM_FILES-1)/
     +                                            MAX_SM_FILES*.FALSE./
      LOGICAL*1 OVERLAY_NAME_ACTIVE(0:MAX_SM_FILES-1),
     +          SM_OVERLAY_MASTR_FILE_OPEN/.FALSE./,
     +          CHECK_4_SCENARIO_YEAR/.FALSE./,
     +          RECORD_BASED_OVERLAYS/.TRUE./,
     +          RECORD_BASED_STOCHASTIC_OLS
      LOGICAL*1 LAHEY_LF95
      CHARACTER*30 SCREEN_OUTPUT

! CONVERT THE DAY_TYPE FILE

      ENTRY SCENARIO_MAKER_MAKEBIN


      VOID_CHR = SCENARIO_MAKER_FILE(SM_FILE_BASE_NAMES)
!
      STUDY_BASE_YEAR = get_base_year()
      TF_YEAR = 0

      DATA_DRIVE = OUTPUT_DIRECTORY()

      IF(.NOT. LAHEY_LF95())
     +       CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
      MAKER_TABLES_IN_FILE = 0
      DO FILE_ID = 0, MAX_SM_FILES-1
         ACTIVE_BASE_SM_FILES(FILE_ID) = .FALSE.
         BASE_FILE_NAME = SM_FILE_BASE_NAMES(FILE_ID)
         IF(INDEX(BASE_FILE_NAME,'NONE') /= 0) CYCLE
         FILE_CODE = SM_FILE_CODES(FILE_ID)
         BINARY_FILE_NAME = SM_FILE_BINARY_NAMES(FILE_ID)
!
         FILE_NAME = trim(BASE_FILE_DIRECTORY())//FILE_CODE//
     +                               "B"//trim(BASE_FILE_NAME)//".DAT"
         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
         IF(FILE_EXISTS) THEN
!
            IF(LAHEY_LF95()) THEN
               SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
               CALL MG_CLEAR_LINE_WRITE(16,30,34,trim(SCREEN_OUTPUT),
     +                                                  ALL_VERSIONS,0)
            ELSE
               CALL MG_CLEAR_LINE_WRITE(16,30,34,trim(BASE_FILE_NAME),
     +                                                  ALL_VERSIONS,0)
            ENDIF
            ACTIVE_BASE_SM_FILES(FILE_ID) = .TRUE.
!
            SCENARIO_MAKER_FILE_EXISTS = FILE_EXISTS
!
            OPEN(10,FILE=FILE_NAME)

            OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//
     +                     "BC"//trim(BINARY_FILE_NAME)//".BIN",
     +                     ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)

            IREC = 0
!
            READ(10,*) DELETE
            DO ! TABLES
               TF_YEAR = 1
               DO ! year_ord-BASED RECORDS
                  READ(10,1000,IOSTAT=IOS) RECLN
!                 ! END OF TABLE !
                  IF(RECLN(1:1) == '7' .OR. IOS /= 0) THEN
                     IF(TF_YEAR <= AVAIL_DATA_YEARS) THEN
                        DO TF_YEAR = TF_YEAR, AVAIL_DATA_YEARS
                           IREC = IREC + 1
                           WRITE(11,REC=IREC) DELETE,
     +                        TF_YEAR+STUDY_BASE_YEAR, ! INT2
     +                        RECORD_IS_ACTIVE,
     +                        SCENARIO_VARIABLE,
     +                        TIME_FRAME,
     +                        ANNUAL_VALUE,
     +                        MONTHLY_VALUES,
     +                        HOURLY_REFERENCE_NAME,
     +                        HOURLY_REFERENCE_NUMBER
                        ENDDO
                     ENDIF ! DETECTED NEW TABLE OR END OF FILE
!
                  EXIT ! LEAVE LOOP (GO TO NEXT TABLE OR EXIT ROUTINE)
                  ELSE
!
!
! 02/04/03. ROUTINE FOR BURESH. TO TRAP FOR MULTIPLE VARIABLES WITHIN
! ONE TAB.
!
                     READ(RECLN,*,ERR=200)
     +                     TEMP_DELETE,
     +                     TEMP_SCENARIO_YEAR,
     +                     TEMP_RECORD_IS_ACTIVE,
     +                     TEMP_SCENARIO_VARIABLE
                     IF(TF_YEAR > 1 .AND.
     +                        TEMP_SCENARIO_VARIABLE /=
     +                                          SCENARIO_VARIABLE) THEN
                        IF(TF_YEAR <= AVAIL_DATA_YEARS) THEN
                           DO TF_YEAR = TF_YEAR, AVAIL_DATA_YEARS
                              IREC = IREC + 1
                              WRITE(11,REC=IREC) DELETE,
     +                           TF_YEAR+STUDY_BASE_YEAR, ! INT2
     +                           RECORD_IS_ACTIVE,
     +                           SCENARIO_VARIABLE,
     +                           TIME_FRAME,
     +                           ANNUAL_VALUE,
     +                           MONTHLY_VALUES,
     +                           HOURLY_REFERENCE_NAME,
     +                           HOURLY_REFERENCE_NUMBER
                           ENDDO
                        ENDIF ! DETECTED NEW TABLE OR END OF FILE
               ! SO THAT IT PROCESSES THE FIRST RECORD OF NEXT VARIABLE
                        TF_YEAR = 1
                        MAKER_TABLES_IN_FILE(FILE_ID) =
     +                                MAKER_TABLES_IN_FILE(FILE_ID) + 1

                     ENDIF
                  ENDIF
                  RECLN =
     +                 trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
                  READ(RECLN,*,ERR=200) DELETE,
     +                     SCENARIO_YEAR,
     +                     RECORD_IS_ACTIVE,
     +                     SCENARIO_VARIABLE,
     +                     TIME_FRAME,
     +                     ANNUAL_VALUE,
     +                     MONTHLY_VALUES,
     +                     HOURLY_REFERENCE_NAME,
     +                     HOURLY_REFERENCE_NUMBER
!
                  IREC = IREC + 1
!
                  READ(RECLN,*,ERR=200) DELETE,year_ord
                  TEMP_YEAR =
     +                  MIN(AVAIL_DATA_YEARS,year_ord - STUDY_BASE_YEAR)
                  IF(TEMP_YEAR > TF_YEAR) THEN ! FILL IN MISSING YEARS
                     IF(IREC == 1) THEN
                        WRITE(4,*)
     +                        "The first year_ord of the first table in"
                      WRITE(4,*) "the Scenario Maker file is ",year_ord
                     WRITE(4,*) "while the base year_ord set in project"
                        WRITE(4,*) "information is ",STUDY_BASE_YEAR
                        WRITE(4,*)
     +                         "First forecast year_ord in the Transact"
                        WRITE(4,*)
     +                        "Forecast file first year_ord must be one"
                   WRITE(4,*) "year_ord greater than the base year_ord."
                        WRITE(4,*) '*** line 7022 TF_OBJT.FOR ***'

                     ENDIF
                     DO TF_YEAR = TF_YEAR, TEMP_YEAR - 1
                        WRITE(11,REC=IREC) DELETE,
     +                     TF_YEAR+STUDY_BASE_YEAR, ! INT2
     +                     RECORD_IS_ACTIVE,
     +                     SCENARIO_VARIABLE,
     +                     TIME_FRAME,
     +                     ANNUAL_VALUE,
     +                     MONTHLY_VALUES,
     +                     HOURLY_REFERENCE_NAME,
     +                     HOURLY_REFERENCE_NUMBER
                        IREC = IREC + 1
                     ENDDO

                  ENDIF

                  WRITE(11,REC=IREC) DELETE,
     +                     TF_YEAR+STUDY_BASE_YEAR, ! INT2
     +                     RECORD_IS_ACTIVE,
     +                     SCENARIO_VARIABLE,
     +                     TIME_FRAME,
     +                     ANNUAL_VALUE,
     +                     MONTHLY_VALUES,
     +                     HOURLY_REFERENCE_NAME,
     +                     HOURLY_REFERENCE_NUMBER
                  TF_YEAR = TF_YEAR + 1
               ENDDO ! LOAD GROUP
               MAKER_TABLES_IN_FILE(FILE_ID) =
     +                                MAKER_TABLES_IN_FILE(FILE_ID) + 1
               IF(IOS /= 0) EXIT
            ENDDO ! READ TABLES
            CLOSE(10)
            CLOSE(11)
!
         ELSE IF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
            CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "tf_objt:1111")
         ENDIF
      ENDDO ! FILE TYPES
      RETURN

! OVERLAY THE DAY TYPE FILE

      ENTRY SCENARIO_MAKER_MAKEOVL(OVERLAY_FAMILY_NAME,FILE_NUMBER)

      IF(.NOT. ACTIVE_BASE_SM_FILES(FILE_NUMBER)) RETURN
      FILE_CODE = SM_FILE_CODES(FILE_NUMBER)
      BINARY_FILE_NAME = SM_FILE_BINARY_NAMES(FILE_NUMBER)
!
      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_CLEAR_LINE_WRITE(17,9,36,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      FILE_NAME = trim(OUTPUT_DIRECTORY())//FILE_CODE//"O"//
     +                               trim(OVERLAY_FAMILY_NAME)//".DAT"
      OPEN(10,FILE=FILE_NAME)
      READ(10,*) DELETE
      INUNIT = 12
      IF(SM_MULTI_OL_CODES(FILE_NUMBER) == 'BC') THEN
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BC"//
     +                      trim(BINARY_FILE_NAME)//".BIN",
     +                     ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         INUNIT = 11
      ENDIF
      OPEN(12,FILE=trim(OUTPUT_DIRECTORY())//"OL"//
     +                      trim(BINARY_FILE_NAME)//".BIN",
     +                     ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      IREC = 0
!

! the makebin code for this file forces the year variable to be
! based on
! the base year.  When doing overlays the overlay process does
! not happen
! because of the year check. The year check isn't needed.
! For this
! reason it has been removed. MSG 10/4/02

      TF_YEAR = 0
      READ(10,1000,IOSTAT=IOS) RECLN
      DOWHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE
!
         READ(10,1000,IOSTAT=IOS) RECLN
!
      ENDDO
!
      READ(RECLN,*,ERR=200) DELETE,
     +                      TF_YEAR
!
      IREC = 0
!
      IREC_OFFSET = 0
      IF(.NOT. RECORD_BASED_STOCHASTIC_OLS()) THEN
         CHECK_4_SCENARIO_YEAR = .TRUE.
         RECORD_BASED_OVERLAYS = .FALSE.
      ENDIF
      FIRST_IREC = 1
      LAST_IREC = 30
      DO ! TABLES AND YEARS, COUNTING IS DONE INSIDE
         DO IREC = FIRST_IREC, LAST_IREC
!
! READ THE NEXT BINARY RECORD
!
!            IREC = IREC + 1
            READ(INUNIT,REC=IREC,IOSTAT=IOS_BASE)
     +                     DELETE,
     +                     SCENARIO_YEAR,
     +                     RECORD_IS_ACTIVE,
     +                     SCENARIO_VARIABLE,
     +                     TIME_FRAME,
     +                     ANNUAL_VALUE,
     +                     MONTHLY_VALUES,
     +                     HOURLY_REFERENCE_NAME,
     +                     HOURLY_REFERENCE_NUMBER
            IF(IOS_BASE /= 0) EXIT ! END OF BINARY FILE
!
! DOES THE year_ord IN THE BINARY RECORD MATCH THE year_ord IN
! THE OVERLAY DAT FILE
!
            IF(TF_YEAR < SCENARIO_YEAR .AND.
     +                     CHECK_4_SCENARIO_YEAR .AND.
     +                                .NOT. RECORD_BASED_OVERLAYS) THEN
!
               DOWHILE(TF_YEAR < SCENARIO_YEAR .AND.
     +                                      DELETE /= 7 .AND. IOS == 0)
                  READ(10,1000,IOSTAT=IOS) RECLN
                  READ(RECLN,*,ERR=200) DELETE,
     +                                  TF_YEAR
                  IREC_OFFSET = IREC_OFFSET + 1
               ENDDO
            ENDIF
!
           IF(SCENARIO_YEAR == TF_YEAR .OR. RECORD_BASED_OVERLAYS) THEN
               RECLN = trim(RECLN)//',,,,,,,,,,,,,'
               READ(RECLN,*,ERR=200)
     +                     DELETE,
     +                     TF_YEAR, ! SCENARIO_YEAR, 10/4/02
     +                     RECORD_IS_ACTIVE,
     +                     SCENARIO_VARIABLE,
     +                     TIME_FRAME,
     +                     ANNUAL_VALUE,
     +                     MONTHLY_VALUES,
     +                     HOURLY_REFERENCE_NAME,
     +                     HOURLY_REFERENCE_NUMBER
               READ(10,1000,IOSTAT=IOS) RECLN
!
               DOWHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE
!
                  READ(10,1000,IOSTAT=IOS) RECLN
!
               ENDDO
               READ(RECLN,*,ERR=200) DELETE,
     +                               TF_YEAR
            ENDIF
!
            WRITE(12,REC=IREC)  DELETE,
     +                     SCENARIO_YEAR,
     +                     RECORD_IS_ACTIVE,
     +                     SCENARIO_VARIABLE,
     +                     TIME_FRAME,
     +                     ANNUAL_VALUE,
     +                     MONTHLY_VALUES,
     +                     HOURLY_REFERENCE_NAME,
     +                     HOURLY_REFERENCE_NUMBER
!
! TAKES CARE OF BINARY FILE GOING TO THE NEXT TABLE
!
            IF(SCENARIO_YEAR - STUDY_BASE_YEAR + IREC_OFFSET >=
     +                                           AVAIL_DATA_YEARS) THEN
!               IREC = IREC + IREC_OFFSET
               CHECK_4_SCENARIO_YEAR = .FALSE.
! 011310.
               IF(TF_YEAR > SCENARIO_YEAR) THEN
                  DOWHILE(TF_YEAR > SCENARIO_YEAR)
                     READ(10,1000,IOSTAT=IOS) RECLN
                     IF(IOS /= 0) EXIT
                     READ(RECLN,*,ERR=200) DELETE,
     +                                  TF_YEAR
                  ENDDO
               ENDIF
            ENDIF

!
         ENDDO
!
         IF(IOS_BASE /= 0) EXIT
!
         FIRST_IREC = FIRST_IREC + 30
         LAST_IREC = LAST_IREC + 30
         IREC_OFFSET = 0
         CHECK_4_SCENARIO_YEAR = .TRUE.
!
      ENDDO
      CLOSE(10)
      CLOSE(12)
!
      IF(IREC <= 1) THEN
         CALL MG_LOCATE_WRITE(21,0,
     +                       'Scenario Maker Overlay Error'//FILE_NAME,
     +                        ALL_VERSIONS,0)
      ENDIF
!
!      OVERLAY_TRANSACTIONS_IN_FILE = IREC
!
      IF(SM_MULTI_OL_CODES(FILE_NUMBER) == 'BC') CLOSE(11)
      SM_MULTI_OL_CODES(FILE_NUMBER) = 'OL'
      RETURN
!

  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from tf_objt SIID290'
      call end_program(er_message)
  300 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      CALL MG_LOCATE_WRITE(21,0,
     +            'Error reading the Scenario Maker record. Look for'//
     +                                   ' a "," in a character name.',
     +                     ALL_VERSIONS,1)
      er_message='stop requested from tf_objt SIID291'
      call end_program(er_message)

      ENTRY RESET_SCENARIO_MAKER_OL

         DO FILE_ID = 0, MAX_SM_FILES-1
            SM_MULTI_OL_CODES(FILE_ID) = 'BC'
!            LAST_FUEL_PRICE_OL_TYPE(FILE_ID) = 'OL'
         ENDDO
      RETURN


      ENTRY OPEN_SCENARIO_MAKER_FILE(R_FILE_NUMBER)


         OPEN(UNIT_NUM,
     +      FILE=trim(OUTPUT_DIRECTORY())//
     +                     SM_MULTI_OL_CODES(R_FILE_NUMBER)//
     +          trim(SM_FILE_BINARY_NAMES(R_FILE_NUMBER))//'.BIN',
     +                      ACCESS="DIRECT",STATUS="OLD",RECL=LRECL)
      RETURN


      ENTRY CLOSE_SCENARIO_MAKER_FILE

         CLOSE(UNIT_NUM)
      RETURN

      ENTRY DOES_SCENARIO_MAKER_FILE_EXIST(
     + R_SCENARIO_MAKER_FILE_EXISTS)

         R_SCENARIO_MAKER_FILE_EXISTS = SCENARIO_MAKER_FILE_EXISTS
      RETURN

      ENTRY IS_ACTIVE_BASE_SM_FILES(R_SCENARIO_MAKER_FILE_EXISTS,
     +                                                 R_FILE_NUMBER)

         R_SCENARIO_MAKER_FILE_EXISTS =
     +                             ACTIVE_BASE_SM_FILES(R_FILE_NUMBER)
      RETURN


      ENTRY GET_MAKER_TABLES(R_MAKER_TABLES,R_FILE_NUMBER)



         R_MAKER_TABLES = MAKER_TABLES_IN_FILE(R_FILE_NUMBER)
      RETURN

 1000 FORMAT(A)
 1010 FORMAT('&',A)

        end subroutine SCENARIO_MAKER_OBJECT

!
!        PROGRAM TO READ MULTI-TAB INFORMATION ON GLOBAL SCENARIOS
!        FOR FORWARD PRICE DEVELOPMENT AND CONVERT TO BINARY FORMAT
!                       COPYRIGHT (C) 1998
!           ALL RIGHTS RESERVED M.S. GERBER & ASSOCIATES, INC.

!
!
!  READ FOR EACH ENDPOINT
!
!
      FUNCTION READ_SCENARIO_MAKER_DATA()
!

      use spindriftlib
      use mod_base_year
      use prod_arrays_dimensions
      USE IREC_ENDPOINT_CONTROL
      use rptreccontrol
      use grx_planning_routines
      use sizecom
      use globecom
      use rangechecker
      SAVE
      integer :: dbg_index1, dbg_index2, dbg_index3


      LOGICAL*1      READ_SCENARIO_MAKER_DATA,
     +               SAVE_SCENARIO_MAKER_STATUS/.FALSE./,
     +               OPEN_SCENARIO_HOURLY_DEMAND,
     +               GET_SCENARIO_PRICE_NUM,
     +               SCENARIO_VARIABLE_ACTIVE(:),
     +               IS_SCENARIO_VARIABLE_ACTIVE
      LOGICAL*4      SCENARIO_MAKER_FILE_EXISTS
      INTEGER*2      YR,MAKER_TABLES/0/,DELETE,
     +               R_MONTH,MAKER_MONTHS,
     +               R_YEAR,R_TEMP_YEAR,
     +               R_LOAD_UNIT,
     +               MO,
     +               LOCAL_MONTHS,LOCAL_YEARS,NUM_SCEN_VAR,IREC,
     +               FILE_ID
! INPUT DATA LIST
      INTEGER*4         VALUES_2_ZERO

      INTEGER*2
     +                  SCENARIO_YEAR,
     +                  TABLE,
     +                  SCENARIO_INDEX,
     +                  TEMP_HOURLY_REFERENCE_NUMBER,
     +                  HOURLY_REFERENCE_NUMBER(:,:),
     +                  LOAD_N,
     +                  R_SCENARIO_INDEX,
     +                  GET_SCENARIO_INDEX,
     +                  LAST_YEAR
      CHARACTER*1
     +                  RECORD_IS_ACTIVE,
     +                  TEMP_TIME_FRAME,
     +                  TIME_FRAME(:,:)
      CHARACTER*5       HOURLY_REFERENCE_NAME(:,:),
     +                  TEMP_HOURLY_REFERENCE_NAME
      CHARACTER*3       R_REFERENCE_NUM
      CHARACTER*40
     +                  SCENARIO_VARIABLE
      CHARACTER*25      SCENARIO_VARIABLE_NAME(:)
      REAL*4
     +                  ANNUAL_VALUE,
     +                  MONTHLY_VALUES(12),
     +                  SCEN_MAKER_VARIABLE(:,:,:),
     +                  GET_SCENARIO_ELECTRIC_DEMAND,
     +                  GET_SCENARIO_PEAK,
     +                  GET_SCENARIO_ENERGY,
     +                  GET_SCENARIO_GAS_PRICES,
     +                  GET_SCENARIO_OIL_PRICES,
     +                  GET_SCENARIO_COAL_PRICES,
     +                  GET_SCENARIO_URANIUM_PRICES,
     +                  GET_SCENARIO_NUCLEAR_AVAIL,
     +                  GET_SCENARIO_COAL_AVAIL,
     +                  GET_SCENARIO_GAS_AVAIL,
     +                  GET_SCENARIO_OIL_AVAIL,
     +                  GET_SCENARIO_OTHER_AVAIL,
     +                  GET_SCENARIO_GAS_BASIS_AVAIL,
     +                  GET_SCENARIO_HYDRO_WATER_YEAR,
     +                  GET_SCENARIO_TRANSMISSION_AVAIL,
     +                  GET_SCENARIO_EXPAND_CAP_COST,
     +                  GET_SCENARIO_SO2_PRICES,
     +                  GET_SCENARIO_NOX_PRICES,
     +                  GET_SCENARIO_CO2_PRICES,
     +                  GET_SCENARIO_HG_PRICES,
     +                  GET_SCENARIO_TRANS_DERATE,
     +                  GET_SCENARIO_ANN_NOX_PRICES,
     +                  GET_SCENARIO_RESERVE_MARGIN,
     +                  GET_SCENARIO_INTEREST_RATE,
     +                  GET_SCENARIO_ELECTRIC_PRICE,
     +                  GET_SCENARIO_LOAD_SHAPE,
     +                  GET_SCENARIO_FOR_SEED,
     +                  GET_SCENARIO_RES_ENERGY,
     +                  GET_SCENARIO_COM_ENERGY,
     +                  GET_SCENARIO_IND_ENERGY,
     +                  GET_SCENARIO_RES_GAS,
     +                  GET_SCENARIO_COM_GAS,
     +                  GET_SCENARIO_IND_GAS,
     +                  GET_SCENARIO_FOS_FUEL,
     +                  GET_SCENARIO_PUR_POWER,
     +                  GET_SCENARIO_PUR_GAS,
     +                  GET_SCENARIO_GAS_DEMAND,
     +                  GET_SCENARIO_BY_INDEX
      ALLOCATABLE ::
     +                  SCEN_MAKER_VARIABLE,
     +                  TIME_FRAME,
     +                  HOURLY_REFERENCE_NAME,
     +                  HOURLY_REFERENCE_NUMBER,
     +                  SCENARIO_VARIABLE_ACTIVE,
     +                  SCENARIO_VARIABLE_NAME
!
! 04/16/03. DETAILED REPORT FOR TORNADO CHARTS
!
      LOGICAL*1 SCENARIO_MAKER_REPORT_NOT_OPEN/.TRUE./,
     +          SCENARIO_MAKER_REPORT,
     +          YES_MONTHLY_SCEN_MAKER_REPORT
      INTEGER*2 SCENARIO_MAKER_RPT_HEADER,
     +          SCENARIO_MAKER_UNIT/0/,
     +          THIS_YEAR
      INTEGER   SCENARIO_MAKER_REC
      REAL*4    AVERAGE_MULTIPLIER,
     +          STDDEV_OF_MULTIPLIER,
     +          STDDEV_OF_MULT_ERROR,
     +          VOLATILITY

!
! END DATA DECLARATIONS
!
!
!
!
         READ_SCENARIO_MAKER_DATA = .FALSE.

!
        CALL DOES_SCENARIO_MAKER_FILE_EXIST(SCENARIO_MAKER_FILE_EXISTS)

         IF(SCENARIO_MAKER_FILE_EXISTS) THEN
            MAKER_MONTHS = 360
            LOCAL_MONTHS = 12
            LOCAL_YEARS = AVAIL_DATA_YEARS
!
! NUMBER OF SCENARIO VARIABLES
! ADDED OIL 5/1/99. ! RESERVE MARGIN ADDED 9/6/99.
! Elect Price Added 9/28/99.
! PEAK ADDED 11/30/99.
! INTEREST RATE ADDED 03/11/02
! GAS, OIL, OTHER AVAILABILITY AND GAS BASIS ADDED 07/21/03.
!
            NUM_SCEN_VAR = 63
!
! 11/25/02.
!
            SCENARIO_MAKER_REPORT = YES_MONTHLY_SCEN_MAKER_REPORT()
!
            IF(ALLOCATED(SCENARIO_VARIABLE_ACTIVE))
     +                             DEALLOCATE(SCENARIO_VARIABLE_ACTIVE,
     +                                          SCENARIO_VARIABLE_NAME)
            ALLOCATE(SCENARIO_VARIABLE_ACTIVE(NUM_SCEN_VAR))
            ALLOCATE(SCENARIO_VARIABLE_NAME(NUM_SCEN_VAR))
            DO TABLE = 1, NUM_SCEN_VAR
               SCENARIO_VARIABLE_ACTIVE(TABLE) = .FALSE.
            ENDDO

            IF( ALLOCATED(SCEN_MAKER_VARIABLE) )
     +                DEALLOCATE(SCEN_MAKER_VARIABLE,
     +                           TIME_FRAME,
     +                           HOURLY_REFERENCE_NAME,
     +                           HOURLY_REFERENCE_NUMBER)
            ALLOCATE(TIME_FRAME(NUM_SCEN_VAR,AVAIL_DATA_YEARS))
            ALLOCATE(HOURLY_REFERENCE_NAME(NUM_SCEN_VAR,
     +                                               AVAIL_DATA_YEARS))
            ALLOCATE(HOURLY_REFERENCE_NUMBER(NUM_SCEN_VAR,
     +                                               AVAIL_DATA_YEARS))
            ALLOCATE(SCEN_MAKER_VARIABLE(NUM_SCEN_VAR,LOCAL_MONTHS,
     +                                               AVAIL_DATA_YEARS))
!
           TIME_FRAME(4,:) = 'M'
           TIME_FRAME(14,:) = 'M'
!
!
            SCEN_MAKER_VARIABLE = 1.
!
            LAST_YEAR = gc_endyr - get_BASE_YEAR()
!
            DO FILE_ID = 0, 9
               CALL IS_ACTIVE_BASE_SM_FILES(SCENARIO_MAKER_FILE_EXISTS,
     +                                                         FILE_ID)
               IF(.NOT. SCENARIO_MAKER_FILE_EXISTS) CYCLE
               CALL OPEN_SCENARIO_MAKER_FILE(FILE_ID)
!
               CALL GET_MAKER_TABLES(MAKER_TABLES,FILE_ID)
!

               DO TABLE = 1, MAKER_TABLES
                  DO YR = 1, AVAIL_DATA_YEARS
                     IREC = (TABLE-1)*AVAIL_DATA_YEARS + YR
                     READ(10,REC=IREC) DELETE,
     +                     SCENARIO_YEAR,
     +                     RECORD_IS_ACTIVE,
     +                     SCENARIO_VARIABLE,
     +                     TEMP_TIME_FRAME,
     +                     ANNUAL_VALUE,
     +                     MONTHLY_VALUES,
     +                     TEMP_HOURLY_REFERENCE_NAME,
     +                     TEMP_HOURLY_REFERENCE_NUMBER
!
                     IF(RECORD_IS_ACTIVE == 'F' .OR.
     +                                   RECORD_IS_ACTIVE == 'N') CYCLE
!
                     SCENARIO_INDEX =
     +                            GET_SCENARIO_INDEX(SCENARIO_VARIABLE)
! 11/20/02.
                     IF(SCENARIO_INDEX == 0) THEN
                        IF(YR == 1) THEN
                          WRITE(4,*) "UNDEFINED SCENARIO MAKER VARIABLE"
                          WRITE(4,*) SCENARIO_VARIABLE
                        ENDIF
                        CYCLE
                     ENDIF
!
                     SCENARIO_VARIABLE_ACTIVE(SCENARIO_INDEX) = .TRUE.
                     SCENARIO_VARIABLE_NAME(SCENARIO_INDEX) =
     +                                                SCENARIO_VARIABLE
                     HOURLY_REFERENCE_NAME(SCENARIO_INDEX,YR) = '     '
!
                     TIME_FRAME(SCENARIO_INDEX,YR) = TEMP_TIME_FRAME
                     DO MO = 1, 12
                        IF(SCENARIO_INDEX == 17 .OR.
     +                                       SCENARIO_INDEX == 18) THEN
                           IF(TEMP_TIME_FRAME == 'A') THEN
                              SCEN_MAKER_VARIABLE(
     +                                          SCENARIO_INDEX,MO,YR) =
     +                                                     ANNUAL_VALUE
                           ELSEIF(TEMP_TIME_FRAME == 'M') THEN
                              SCEN_MAKER_VARIABLE(
     +                                          SCENARIO_INDEX,MO,YR) =
     +                                               MONTHLY_VALUES(MO)
                           ELSE ! IMPLICITLY HOURLY
!
                              HOURLY_REFERENCE_NAME(
     +                                   SCENARIO_INDEX,YR) =
     +                                       TEMP_HOURLY_REFERENCE_NAME
                              HOURLY_REFERENCE_NUMBER(
     +                                    SCENARIO_INDEX,YR)=
     +                                     TEMP_HOURLY_REFERENCE_NUMBER
!
                           ENDIF
                       ELSE
                           IF(TEMP_TIME_FRAME == 'A') THEN
                              SCEN_MAKER_VARIABLE(
     +                                          SCENARIO_INDEX,MO,YR) =
     +                              SCEN_MAKER_VARIABLE(
     +                                          SCENARIO_INDEX,MO,YR) *
     +                                                     ANNUAL_VALUE
                           ELSEIF(TEMP_TIME_FRAME == 'M') THEN
                              SCEN_MAKER_VARIABLE(
     +                                          SCENARIO_INDEX,MO,YR) =
     +                              SCEN_MAKER_VARIABLE(
     +                                          SCENARIO_INDEX,MO,YR) *
     +                                               MONTHLY_VALUES(MO)
                           ELSE ! IMPLICITLY HOURLY
!
                              HOURLY_REFERENCE_NAME(
     +                                   SCENARIO_INDEX,YR) =
     +                                       TEMP_HOURLY_REFERENCE_NAME
                              HOURLY_REFERENCE_NUMBER(
     +                                    SCENARIO_INDEX,YR)=
     +                                     TEMP_HOURLY_REFERENCE_NUMBER
!
                           ENDIF
                       ENDIF
                     ENDDO ! MONTHS
                  ENDDO ! RECORDS (YEARS)
               ENDDO ! TABLES
               READ_SCENARIO_MAKER_DATA = .TRUE.
               CALL CLOSE_SCENARIO_MAKER_FILE
            ENDDO ! FILE TYPES
!
            IF(SCENARIO_MAKER_REPORT) THEN
!
               IF(SCENARIO_MAKER_REPORT_NOT_OPEN) THEN
                  SCENARIO_MAKER_REPORT_NOT_OPEN = .FALSE.
                  SCENARIO_MAKER_UNIT =
     +                    SCENARIO_MAKER_RPT_HEADER(SCENARIO_MAKER_REC)
               ENDIF
!
           DO YR = 1, LAST_YEAR ! 080117 gc_endyr ! 070717 AVAIL_DATA_YEARS
                  THIS_YEAR = YR + get_BASE_YEAR()
                  DO SCENARIO_INDEX = 1, NUM_SCEN_VAR
!
                     IF(.NOT.
     +                  SCENARIO_VARIABLE_ACTIVE(SCENARIO_INDEX)) CYCLE
!
                     AVERAGE_MULTIPLIER = .08333333*(
     +                     SCEN_MAKER_VARIABLE(SCENARIO_INDEX,1,YR)+
     +                     SCEN_MAKER_VARIABLE(SCENARIO_INDEX,2,YR)+
     +                     SCEN_MAKER_VARIABLE(SCENARIO_INDEX,3,YR)+
     +                     SCEN_MAKER_VARIABLE(SCENARIO_INDEX,4,YR)+
     +                     SCEN_MAKER_VARIABLE(SCENARIO_INDEX,5,YR)+
     +                     SCEN_MAKER_VARIABLE(SCENARIO_INDEX,6,YR)+
     +                     SCEN_MAKER_VARIABLE(SCENARIO_INDEX,7,YR)+
     +                     SCEN_MAKER_VARIABLE(SCENARIO_INDEX,8,YR)+
     +                     SCEN_MAKER_VARIABLE(SCENARIO_INDEX,9,YR)+
     +                     SCEN_MAKER_VARIABLE(SCENARIO_INDEX,10,YR)+
     +                     SCEN_MAKER_VARIABLE(SCENARIO_INDEX,11,YR)+
     +                     SCEN_MAKER_VARIABLE(SCENARIO_INDEX,12,YR))
                     STDDEV_OF_MULTIPLIER = 2.0
                     STDDEV_OF_MULT_ERROR = 3.0
                     VOLATILITY = 4.0
!
                     WRITE(SCENARIO_MAKER_UNIT,REC=SCENARIO_MAKER_REC)
     +                        PRT_ENDPOINT(),
     +                        FLOAT(THIS_YEAR),
     +                        'Product  ',
     +                        SCENARIO_VARIABLE_NAME(SCENARIO_INDEX),
     +                        (SCEN_MAKER_VARIABLE(
     +                                  SCENARIO_INDEX,MO,YR),MO=1,12),
     +                        AVERAGE_MULTIPLIER,
     +                        STDDEV_OF_MULTIPLIER,
     +                        STDDEV_OF_MULT_ERROR,
     +                        VOLATILITY
                     SCENARIO_MAKER_REC = SCENARIO_MAKER_REC + 1
                  ENDDO
               ENDDO
!
            ENDIF
         ENDIF
!
         SAVE_SCENARIO_MAKER_STATUS = READ_SCENARIO_MAKER_DATA
      RETURN

      ENTRY IS_SCENARIO_VARIABLE_ACTIVE(R_SCENARIO_INDEX)

         IS_SCENARIO_VARIABLE_ACTIVE =
     +                       SCENARIO_VARIABLE_ACTIVE(R_SCENARIO_INDEX)
      RETURN

      ENTRY OPEN_SCENARIO_HOURLY_DEMAND(R_TEMP_YEAR,R_LOAD_UNIT)



         IF(SAVE_SCENARIO_MAKER_STATUS) THEN
            SCENARIO_INDEX = 4
            R_YEAR = MIN(R_TEMP_YEAR,AVAIL_DATA_YEARS)
            IF(TIME_FRAME(SCENARIO_INDEX,R_YEAR) == 'H') THEN
               OPEN_SCENARIO_HOURLY_DEMAND = .TRUE.
               TEMP_HOURLY_REFERENCE_NAME =
     +                   HOURLY_REFERENCE_NAME(SCENARIO_INDEX,R_YEAR)
               TEMP_HOURLY_REFERENCE_NUMBER =
     +                  HOURLY_REFERENCE_NUMBER(SCENARIO_INDEX,R_YEAR)
!
               CALL
     +           OPEN_SCEN_HOURLY_LOAD_FILE(TEMP_HOURLY_REFERENCE_NAME,
     +                                    TEMP_HOURLY_REFERENCE_NUMBER,
     +                                     R_YEAR,R_LOAD_UNIT)
             ELSE
               OPEN_SCENARIO_HOURLY_DEMAND = .FALSE.
            ENDIF
         ELSE
         ENDIF
      RETURN


      ENTRY GET_SCENARIO_PRICE_NUM(R_TEMP_YEAR,R_REFERENCE_NUM)

         GET_SCENARIO_PRICE_NUM = .FALSE.
         IF(SAVE_SCENARIO_MAKER_STATUS) THEN
            SCENARIO_INDEX = 14
            IF(TIME_FRAME(SCENARIO_INDEX,R_YEAR) == 'H') THEN
               R_YEAR = MIN(R_TEMP_YEAR,AVAIL_DATA_YEARS)
               R_REFERENCE_NUM =
     +                HOURLY_REFERENCE_NAME(SCENARIO_INDEX,R_YEAR)(1:3)
               IF(R_REFERENCE_NUM /= '   ') THEN
                  GET_SCENARIO_PRICE_NUM = .TRUE.
               ENDIF
            ENDIF
         ENDIF
      RETURN

      ENTRY GET_SCENARIO_FOR_SEED(R_TEMP_YEAR,R_MONTH) !

         IF(.NOT. SAVE_SCENARIO_MAKER_STATUS) THEN
            GET_SCENARIO_FOR_SEED = 1.0
         ELSE
            SCENARIO_INDEX = 18
            R_YEAR = MIN(R_TEMP_YEAR,AVAIL_DATA_YEARS)
            GET_SCENARIO_FOR_SEED =
     +               SCEN_MAKER_VARIABLE(SCENARIO_INDEX,R_MONTH,R_YEAR)
         ENDIF
      RETURN

      ENTRY GET_SCENARIO_LOAD_SHAPE(R_TEMP_YEAR,R_MONTH) !

         IF(.NOT. SAVE_SCENARIO_MAKER_STATUS) THEN
            GET_SCENARIO_LOAD_SHAPE = 1.0
         ELSE
            SCENARIO_INDEX = 17
            R_YEAR = MIN(R_TEMP_YEAR,AVAIL_DATA_YEARS)
            GET_SCENARIO_LOAD_SHAPE =
     +               SCEN_MAKER_VARIABLE(SCENARIO_INDEX,R_MONTH,R_YEAR)
         ENDIF
      RETURN


      ENTRY GET_SCENARIO_COAL_PRICES(R_TEMP_YEAR,R_MONTH) ! DONE.

         IF(.NOT. SAVE_SCENARIO_MAKER_STATUS) THEN
            GET_SCENARIO_COAL_PRICES = 1.0
         ELSE
            SCENARIO_INDEX = 1
            R_YEAR = MIN(R_TEMP_YEAR,AVAIL_DATA_YEARS)
            GET_SCENARIO_COAL_PRICES =
     +               SCEN_MAKER_VARIABLE(SCENARIO_INDEX,R_MONTH,R_YEAR)
         ENDIF
      RETURN

      ENTRY GET_SCENARIO_GAS_PRICES(R_TEMP_YEAR,R_MONTH) ! DONE.

         IF(.NOT. SAVE_SCENARIO_MAKER_STATUS) THEN
            GET_SCENARIO_GAS_PRICES = 1.0
         ELSE
            R_YEAR = MIN(R_TEMP_YEAR,AVAIL_DATA_YEARS)
            SCENARIO_INDEX = 2
            GET_SCENARIO_GAS_PRICES =
     +               SCEN_MAKER_VARIABLE(SCENARIO_INDEX,R_MONTH,R_YEAR)
         ENDIF
      RETURN

      ENTRY GET_SCENARIO_OIL_PRICES(R_TEMP_YEAR,R_MONTH) ! DONE.

         IF(.NOT. SAVE_SCENARIO_MAKER_STATUS) THEN
            GET_SCENARIO_OIL_PRICES = 1.0
         ELSE
            SCENARIO_INDEX = 3
            R_YEAR = MIN(R_TEMP_YEAR,AVAIL_DATA_YEARS)
            GET_SCENARIO_OIL_PRICES =
     +               SCEN_MAKER_VARIABLE(SCENARIO_INDEX,R_MONTH,R_YEAR)
         ENDIF
      RETURN

      ENTRY GET_SCENARIO_URANIUM_PRICES(R_TEMP_YEAR,R_MONTH) ! DONE.

         IF(.NOT. SAVE_SCENARIO_MAKER_STATUS) THEN
            GET_SCENARIO_URANIUM_PRICES = 1.0
!            GET_SCENARIO_URANIUM_PRICES = 54.0
         ELSE
            SCENARIO_INDEX = 54
            R_YEAR = MIN(R_TEMP_YEAR,AVAIL_DATA_YEARS)
            GET_SCENARIO_URANIUM_PRICES =
     +               SCEN_MAKER_VARIABLE(SCENARIO_INDEX,R_MONTH,R_YEAR)
         ENDIF
      RETURN

      ENTRY GET_SCENARIO_ELECTRIC_DEMAND(R_TEMP_YEAR,R_MONTH) ! DONE.

         IF(.NOT. SAVE_SCENARIO_MAKER_STATUS) THEN
            GET_SCENARIO_ELECTRIC_DEMAND = 1.0
         ELSE
            SCENARIO_INDEX = 4
            R_YEAR = MIN(R_TEMP_YEAR,AVAIL_DATA_YEARS)
            GET_SCENARIO_ELECTRIC_DEMAND =
     +               SCEN_MAKER_VARIABLE(SCENARIO_INDEX,R_MONTH,R_YEAR)
         ENDIF
      RETURN

      ENTRY GET_SCENARIO_PEAK(R_TEMP_YEAR,R_MONTH) ! DONE.

         IF(.NOT. SAVE_SCENARIO_MAKER_STATUS) THEN
            GET_SCENARIO_PEAK = 1.0
         ELSE
            SCENARIO_INDEX = 15
            R_YEAR = MIN(R_TEMP_YEAR,AVAIL_DATA_YEARS)
            GET_SCENARIO_PEAK =
     +               SCEN_MAKER_VARIABLE(SCENARIO_INDEX,R_MONTH,R_YEAR)
         ENDIF
      RETURN

      ENTRY GET_SCENARIO_ENERGY(R_TEMP_YEAR,R_MONTH) ! DONE. ADDED 5/15/00.

         IF(.NOT. SAVE_SCENARIO_MAKER_STATUS) THEN
            GET_SCENARIO_ENERGY = 1.0
         ELSE
            SCENARIO_INDEX = 16
            R_YEAR = MIN(R_TEMP_YEAR,AVAIL_DATA_YEARS)
            GET_SCENARIO_ENERGY =
     +               SCEN_MAKER_VARIABLE(SCENARIO_INDEX,R_MONTH,R_YEAR)
         ENDIF
      RETURN

      ENTRY GET_SCENARIO_NUCLEAR_AVAIL(R_TEMP_YEAR,R_MONTH) ! DONE.

         IF(.NOT. SAVE_SCENARIO_MAKER_STATUS) THEN
            GET_SCENARIO_NUCLEAR_AVAIL = 1.0
         ELSE
            SCENARIO_INDEX = 5
            R_YEAR = MIN(R_TEMP_YEAR,AVAIL_DATA_YEARS)
            GET_SCENARIO_NUCLEAR_AVAIL =
     +               SCEN_MAKER_VARIABLE(SCENARIO_INDEX,R_MONTH,R_YEAR)
         ENDIF
      RETURN

      ENTRY GET_SCENARIO_COAL_AVAIL(R_TEMP_YEAR,R_MONTH) ! DONE.

         IF(.NOT. SAVE_SCENARIO_MAKER_STATUS) THEN
            GET_SCENARIO_COAL_AVAIL = 1.0
         ELSE
            SCENARIO_INDEX = 6
            R_YEAR = MIN(R_TEMP_YEAR,AVAIL_DATA_YEARS)
            GET_SCENARIO_COAL_AVAIL =
     +               SCEN_MAKER_VARIABLE(SCENARIO_INDEX,R_MONTH,R_YEAR)
         ENDIF
      RETURN

      ENTRY GET_SCENARIO_GAS_AVAIL(R_TEMP_YEAR,R_MONTH) ! DONE.

         IF(.NOT. SAVE_SCENARIO_MAKER_STATUS) THEN
            GET_SCENARIO_GAS_AVAIL = 1.0
         ELSE
            SCENARIO_INDEX = 43
            R_YEAR = MIN(R_TEMP_YEAR,AVAIL_DATA_YEARS)
            GET_SCENARIO_GAS_AVAIL =
     +               SCEN_MAKER_VARIABLE(SCENARIO_INDEX,R_MONTH,R_YEAR)
         ENDIF
      RETURN

      ENTRY GET_SCENARIO_OIL_AVAIL(R_TEMP_YEAR,R_MONTH) ! DONE.

         IF(.NOT. SAVE_SCENARIO_MAKER_STATUS) THEN
            GET_SCENARIO_OIL_AVAIL = 1.0
         ELSE
            SCENARIO_INDEX = 44
            R_YEAR = MIN(R_TEMP_YEAR,AVAIL_DATA_YEARS)
            GET_SCENARIO_OIL_AVAIL =
     +               SCEN_MAKER_VARIABLE(SCENARIO_INDEX,R_MONTH,R_YEAR)
         ENDIF
      RETURN


      ENTRY GET_SCENARIO_OTHER_AVAIL(R_TEMP_YEAR,R_MONTH) ! DONE.

         IF(.NOT. SAVE_SCENARIO_MAKER_STATUS) THEN
            GET_SCENARIO_OTHER_AVAIL = 1.0
         ELSE
            SCENARIO_INDEX = 45
            R_YEAR = MIN(R_TEMP_YEAR,AVAIL_DATA_YEARS)
            GET_SCENARIO_OTHER_AVAIL =
     +               SCEN_MAKER_VARIABLE(SCENARIO_INDEX,R_MONTH,R_YEAR)
         ENDIF
      RETURN


      ENTRY GET_SCENARIO_GAS_BASIS_AVAIL(R_TEMP_YEAR,R_MONTH)


         IF(.NOT. SAVE_SCENARIO_MAKER_STATUS) THEN
            GET_SCENARIO_GAS_BASIS_AVAIL = 1.0
         ELSE
            SCENARIO_INDEX = 38
            R_YEAR = MIN(R_TEMP_YEAR,AVAIL_DATA_YEARS)
            GET_SCENARIO_GAS_BASIS_AVAIL =
     +               SCEN_MAKER_VARIABLE(SCENARIO_INDEX,R_MONTH,R_YEAR)
         ENDIF
      RETURN

      ENTRY GET_SCENARIO_HYDRO_WATER_YEAR(R_TEMP_YEAR,R_MONTH)

         IF(.NOT. SAVE_SCENARIO_MAKER_STATUS) THEN
            GET_SCENARIO_HYDRO_WATER_YEAR = 1.0
         ELSE

            SCENARIO_INDEX = 7
            R_YEAR = MIN(R_TEMP_YEAR,AVAIL_DATA_YEARS)
            dbg_index1=ubound(SCEN_MAKER_VARIABLE,1)
            dbg_index2=ubound(SCEN_MAKER_VARIABLE,2)
            dbg_index3=ubound(SCEN_MAKER_VARIABLE,3)

            if(dbg_index1<scenario_index .or. dbg_index2<r_month .or.
     +        dbg_index3<r_year) then
                call end_program("tf_objt:0011 - Range check " //
     +              "error accessing SCEN_MAKER_VARIABLE array.")
            end if
            GET_SCENARIO_HYDRO_WATER_YEAR =
     +               SCEN_MAKER_VARIABLE(SCENARIO_INDEX,R_MONTH,R_YEAR)
         ENDIF
      RETURN

      ENTRY GET_SCENARIO_TRANSMISSION_AVAIL(R_TEMP_YEAR,
     + R_MONTH)

       if(r_temp_year<0) then
        call end_program("tf_objt:0013 - temp_year value " //
     +     trim(itos(int(r_temp_year))) // " is invalid.")
       end if
         dbg_index1=ubound(SCEN_MAKER_VARIABLE,1)
            dbg_index2=ubound(SCEN_MAKER_VARIABLE,2)
            dbg_index3=ubound(SCEN_MAKER_VARIABLE,3)

            if(dbg_index1<scenario_index .or. dbg_index2<r_month .or.
     +        dbg_index3<r_year) then
               call end_program("tf_objt:0012 - out of bounds on " //
     +         "scen_maker_variable array in " //
     +         "GET_SCENARIO_TRANSMISSION_AVAIL routine.")
            end if
         IF(.NOT. SAVE_SCENARIO_MAKER_STATUS) THEN
            GET_SCENARIO_TRANSMISSION_AVAIL = 1.0
         ELSE
            SCENARIO_INDEX = 8
            R_YEAR = MIN(R_TEMP_YEAR,AVAIL_DATA_YEARS)
            GET_SCENARIO_TRANSMISSION_AVAIL =
     +               SCEN_MAKER_VARIABLE(SCENARIO_INDEX,R_MONTH,R_YEAR)
         ENDIF
      RETURN

      ENTRY GET_SCENARIO_EXPAND_CAP_COST(R_TEMP_YEAR,R_MONTH)

         IF(.NOT. SAVE_SCENARIO_MAKER_STATUS) THEN
            GET_SCENARIO_EXPAND_CAP_COST = 1.0
         ELSE
            SCENARIO_INDEX = 9
            R_YEAR = MIN(R_TEMP_YEAR,AVAIL_DATA_YEARS)
            GET_SCENARIO_EXPAND_CAP_COST =
     +               SCEN_MAKER_VARIABLE(SCENARIO_INDEX,R_MONTH,R_YEAR)
         ENDIF
      RETURN


      ENTRY GET_SCENARIO_SO2_PRICES(R_TEMP_YEAR,R_MONTH)

         IF(.NOT. SAVE_SCENARIO_MAKER_STATUS) THEN
            GET_SCENARIO_SO2_PRICES = 1.0
         ELSE
            SCENARIO_INDEX = 10
            R_YEAR = MIN(R_TEMP_YEAR,AVAIL_DATA_YEARS)
            if(r_year==0) then
                call end_program("tf_objt:0022 - " // 
     +    "r_year value of zero is invalid.")
            end if

     
            GET_SCENARIO_SO2_PRICES =
     +               SCEN_MAKER_VARIABLE(SCENARIO_INDEX,R_MONTH,R_YEAR)
         ENDIF
      RETURN

      ENTRY GET_SCENARIO_NOX_PRICES(R_TEMP_YEAR,R_MONTH)

         IF(.NOT. SAVE_SCENARIO_MAKER_STATUS) THEN
            GET_SCENARIO_NOX_PRICES = 1.0
         ELSE
            SCENARIO_INDEX = 11
            R_YEAR = MIN(R_TEMP_YEAR,AVAIL_DATA_YEARS)
            GET_SCENARIO_NOX_PRICES =
     +               SCEN_MAKER_VARIABLE(SCENARIO_INDEX,R_MONTH,R_YEAR)
         ENDIF
      RETURN


      ENTRY GET_SCENARIO_CO2_PRICES(R_TEMP_YEAR,R_MONTH)


         IF(.NOT. SAVE_SCENARIO_MAKER_STATUS) THEN
            GET_SCENARIO_CO2_PRICES = 1.0
         ELSE
            SCENARIO_INDEX = 12
            R_YEAR = MIN(R_TEMP_YEAR,AVAIL_DATA_YEARS)
            GET_SCENARIO_CO2_PRICES =
     +               SCEN_MAKER_VARIABLE(SCENARIO_INDEX,R_MONTH,R_YEAR)
         ENDIF
      RETURN

      ENTRY GET_SCENARIO_HG_PRICES(R_TEMP_YEAR,R_MONTH)

         IF(.NOT. SAVE_SCENARIO_MAKER_STATUS) THEN
            GET_SCENARIO_HG_PRICES = 1.0
         ELSE
            SCENARIO_INDEX = 57 ! CHANGED FROM 46 ON 060408.
            R_YEAR = MIN(R_TEMP_YEAR,AVAIL_DATA_YEARS)
            GET_SCENARIO_HG_PRICES =
     +               SCEN_MAKER_VARIABLE(SCENARIO_INDEX,R_MONTH,R_YEAR)
         ENDIF
      RETURN


      ENTRY GET_SCENARIO_TRANS_DERATE(R_TEMP_YEAR,R_MONTH)

         IF(.NOT. SAVE_SCENARIO_MAKER_STATUS) THEN
            GET_SCENARIO_TRANS_DERATE = 1.0
         ELSE
            SCENARIO_INDEX = 63
            R_YEAR = MIN(R_TEMP_YEAR,AVAIL_DATA_YEARS)
            GET_SCENARIO_TRANS_DERATE =
     +               SCEN_MAKER_VARIABLE(SCENARIO_INDEX,R_MONTH,R_YEAR)
         ENDIF
      RETURN

      ENTRY GET_SCENARIO_ANN_NOX_PRICES(R_TEMP_YEAR,R_MONTH)

         IF(.NOT. SAVE_SCENARIO_MAKER_STATUS) THEN
            GET_SCENARIO_ANN_NOX_PRICES = 1.0
         ELSE
            SCENARIO_INDEX = 47
            R_YEAR = MIN(R_TEMP_YEAR,AVAIL_DATA_YEARS)
            GET_SCENARIO_ANN_NOX_PRICES =
     +               SCEN_MAKER_VARIABLE(SCENARIO_INDEX,R_MONTH,R_YEAR)
         ENDIF
      RETURN

      ENTRY GET_SCENARIO_RESERVE_MARGIN(R_TEMP_YEAR,R_MONTH)

         IF(.NOT. SAVE_SCENARIO_MAKER_STATUS) THEN
            GET_SCENARIO_RESERVE_MARGIN = 1.0
         ELSE
            SCENARIO_INDEX = 13
            R_YEAR = MIN(R_TEMP_YEAR,AVAIL_DATA_YEARS)
            GET_SCENARIO_RESERVE_MARGIN =
     +               SCEN_MAKER_VARIABLE(SCENARIO_INDEX,R_MONTH,R_YEAR)
         ENDIF
      RETURN


      ENTRY GET_SCENARIO_INTEREST_RATE(R_TEMP_YEAR,R_MONTH)

         IF(.NOT. SAVE_SCENARIO_MAKER_STATUS) THEN
            GET_SCENARIO_INTEREST_RATE = 1.0
         ELSE
            SCENARIO_INDEX = 19
            R_YEAR = MIN(R_TEMP_YEAR,AVAIL_DATA_YEARS)
            GET_SCENARIO_INTEREST_RATE =
     +               SCEN_MAKER_VARIABLE(SCENARIO_INDEX,R_MONTH,R_YEAR)
         ENDIF
      RETURN

      ENTRY GET_SCENARIO_ELECTRIC_PRICE(R_TEMP_YEAR,R_MONTH)

         IF(.NOT. SAVE_SCENARIO_MAKER_STATUS) THEN
            GET_SCENARIO_ELECTRIC_PRICE = 1.0
         ELSE
            SCENARIO_INDEX = 14
            R_YEAR = MIN(R_TEMP_YEAR,AVAIL_DATA_YEARS)
            GET_SCENARIO_ELECTRIC_PRICE =
     +               SCEN_MAKER_VARIABLE(SCENARIO_INDEX,R_MONTH,R_YEAR)
         ENDIF
      RETURN

      ENTRY GET_SCENARIO_RES_ENERGY(R_TEMP_YEAR,R_MONTH)

         IF(.NOT. SAVE_SCENARIO_MAKER_STATUS) THEN
            GET_SCENARIO_RES_ENERGY = 1.0
         ELSE
            SCENARIO_INDEX = 20
            R_YEAR = MIN(R_TEMP_YEAR,AVAIL_DATA_YEARS)
            GET_SCENARIO_RES_ENERGY =
     +               SCEN_MAKER_VARIABLE(SCENARIO_INDEX,R_MONTH,R_YEAR)
         ENDIF
      RETURN

      ENTRY GET_SCENARIO_COM_ENERGY(R_TEMP_YEAR,R_MONTH)

         IF(.NOT. SAVE_SCENARIO_MAKER_STATUS) THEN
            GET_SCENARIO_COM_ENERGY = 1.0
         ELSE
            SCENARIO_INDEX = 21
            R_YEAR = MIN(R_TEMP_YEAR,AVAIL_DATA_YEARS)
            GET_SCENARIO_COM_ENERGY =
     +               SCEN_MAKER_VARIABLE(SCENARIO_INDEX,R_MONTH,R_YEAR)
         ENDIF
      RETURN

      ENTRY GET_SCENARIO_IND_ENERGY(R_TEMP_YEAR,R_MONTH)

         IF(.NOT. SAVE_SCENARIO_MAKER_STATUS) THEN
            GET_SCENARIO_IND_ENERGY = 1.0
         ELSE
            SCENARIO_INDEX = 22
            R_YEAR = MIN(R_TEMP_YEAR,AVAIL_DATA_YEARS)
            GET_SCENARIO_IND_ENERGY =
     +               SCEN_MAKER_VARIABLE(SCENARIO_INDEX,R_MONTH,R_YEAR)
         ENDIF
      RETURN
      ENTRY GET_SCENARIO_RES_GAS(R_TEMP_YEAR,R_MONTH)

         IF(.NOT. SAVE_SCENARIO_MAKER_STATUS) THEN
            GET_SCENARIO_RES_GAS = 1.0
         ELSE
            SCENARIO_INDEX = 59
            R_YEAR = MIN(R_TEMP_YEAR,AVAIL_DATA_YEARS)
            GET_SCENARIO_RES_GAS =
     +               SCEN_MAKER_VARIABLE(SCENARIO_INDEX,R_MONTH,R_YEAR)
         ENDIF
      RETURN

      ENTRY GET_SCENARIO_COM_GAS(R_TEMP_YEAR,R_MONTH)

         IF(.NOT. SAVE_SCENARIO_MAKER_STATUS) THEN
            GET_SCENARIO_COM_GAS = 1.0
         ELSE
            SCENARIO_INDEX = 60
            R_YEAR = MIN(R_TEMP_YEAR,AVAIL_DATA_YEARS)
            GET_SCENARIO_COM_GAS =
     +               SCEN_MAKER_VARIABLE(SCENARIO_INDEX,R_MONTH,R_YEAR)
         ENDIF
      RETURN
      ENTRY GET_SCENARIO_IND_GAS(R_TEMP_YEAR,R_MONTH)

         IF(.NOT. SAVE_SCENARIO_MAKER_STATUS) THEN
            GET_SCENARIO_IND_GAS = 1.0
         ELSE
            SCENARIO_INDEX = 61
            R_YEAR = MIN(R_TEMP_YEAR,AVAIL_DATA_YEARS)
            GET_SCENARIO_IND_GAS =
     +              SCEN_MAKER_VARIABLE(SCENARIO_INDEX,R_MONTH,R_YEAR)
         ENDIF
      RETURN
      ENTRY GET_SCENARIO_FOS_FUEL(R_TEMP_YEAR,R_MONTH)

         IF(.NOT. SAVE_SCENARIO_MAKER_STATUS) THEN
            GET_SCENARIO_FOS_FUEL = 1.0
         ELSE
            SCENARIO_INDEX = 23
            R_YEAR = MIN(R_TEMP_YEAR,AVAIL_DATA_YEARS)
            GET_SCENARIO_FOS_FUEL =
     +               SCEN_MAKER_VARIABLE(SCENARIO_INDEX,R_MONTH,R_YEAR)
         ENDIF
      RETURN
      ENTRY GET_SCENARIO_PUR_POWER(R_TEMP_YEAR,R_MONTH)

         IF(.NOT. SAVE_SCENARIO_MAKER_STATUS) THEN
            GET_SCENARIO_PUR_POWER = 1.0
         ELSE
            SCENARIO_INDEX = 24
            R_YEAR = MIN(R_TEMP_YEAR,AVAIL_DATA_YEARS)
            GET_SCENARIO_PUR_POWER =
     +               SCEN_MAKER_VARIABLE(SCENARIO_INDEX,R_MONTH,R_YEAR)
         ENDIF
      RETURN

      ENTRY GET_SCENARIO_PUR_GAS(R_TEMP_YEAR,R_MONTH)

         IF(.NOT. SAVE_SCENARIO_MAKER_STATUS) THEN
            GET_SCENARIO_PUR_GAS = 1.0
         ELSE
            SCENARIO_INDEX = 25
            R_YEAR = MIN(R_TEMP_YEAR,AVAIL_DATA_YEARS)
            GET_SCENARIO_PUR_GAS =
     +               SCEN_MAKER_VARIABLE(SCENARIO_INDEX,R_MONTH,R_YEAR)
         ENDIF
      RETURN
      ENTRY GET_SCENARIO_GAS_DEMAND(R_TEMP_YEAR,R_MONTH)

         IF(.NOT. SAVE_SCENARIO_MAKER_STATUS) THEN
            GET_SCENARIO_GAS_DEMAND = 1.0
         ELSE
            SCENARIO_INDEX = 49
            R_YEAR = MIN(R_TEMP_YEAR,AVAIL_DATA_YEARS)
            GET_SCENARIO_GAS_DEMAND =
     +               SCEN_MAKER_VARIABLE(SCENARIO_INDEX,R_MONTH,R_YEAR)
         ENDIF
      RETURN
      ENTRY GET_SCENARIO_BY_INDEX(R_TEMP_YEAR,R_MONTH,R_SCENARIO_INDEX)

         IF(.NOT. SAVE_SCENARIO_MAKER_STATUS) THEN
            GET_SCENARIO_BY_INDEX = 1.0
         ELSE
            SCENARIO_INDEX = R_SCENARIO_INDEX
            IF(SCENARIO_INDEX > NUM_SCEN_VAR .OR.
     +                                        SCENARIO_INDEX <= 0) THEN
               GET_SCENARIO_BY_INDEX = 1.0
            ELSE
               R_YEAR = MIN(R_TEMP_YEAR,AVAIL_DATA_YEARS)
               GET_SCENARIO_BY_INDEX =
     +               SCEN_MAKER_VARIABLE(SCENARIO_INDEX,R_MONTH,R_YEAR)
            ENDIF
         ENDIF
      RETURN
!
!
      END
!
!
