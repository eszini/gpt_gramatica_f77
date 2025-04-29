!     Last change: 
!            MSG 5/22/2016 5:36:58 PM


!        PROGRAM TO READ MULTI-TAB INFORMATION ON LOADS
!                 AND CONVERT TO BINARY FORMAT
!                       COPYRIGHT (C) 1997-99
!      ALL RIGHTS RESERVED M.S. GERBER & ASSOCIATES, INC.


!
      SUBROUTINE COAL_DEMAND_OBJECT
      use end_routine, only: end_program, er_message


      use spindriftlib
      use prod_arrays_dimensions
      USE SIZECOM


      INTEGER*1 FORECAST_GROWTH_YEARS/AVAIL_DATA_YEARS/
      LOGICAL*1 SAVE_COAL_DEMAND_FILE_EXISTS/.FALSE./,
     +          R_COAL_DEMAND_FILE_EXISTS,
     +          R_COAL_DEMAND_FILE_USED,
     +          SAVE_COAL_DEMAND_FILE_USED/.FALSE./
      CHARACTER*6    BASECASE_MARKET_AREA_ID,
     +               BASECASE_TRANS_AREA_ID, 
     +               BASECASE_NERC_SUB_ID
      CHARACTER*1 EXTENSION_PERIOD_GROWTH_SWITCH/'X'/,TABLE_ACTIVE,
     +            THREE_FACTOR_TRANSFORM,
     +            JURISDICTIONAL_CUSTOMER,
     +            FUEL_COST_RECOVERY_THROUGH_FAC,
     +            SPECIAL_CUSTOMER_TYPE
      INTEGER*2   INUNIT,IREC,DELETE,LRECL/638/,
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
      CHARACTER*5 COAL_DEMAND_FORECAST_FILE,OVERLAY_FAMILY_NAME,BSYRLOAD
      CHARACTER*256 FILE_NAME
      CHARACTER*256 OUTPUT_DIRECTORY
      CHARACTER*256 BASE_FILE_DIRECTORY
      CHARACTER*152 MESSAGE
      LOGICAL*4 FILE_EXISTS
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER*1024 RECLN
! DECLARATION FOR SYSTEM FORECAST VARIABLES
      INTEGER*2 YEAR
!      REAL TSY_FC_DATA(4,12)
!      REAL MARKET_RATES_CUSTOMERS(4) !3/26/95 MARKET PRICES AT SYSTEM LEVEL MSG
!
! SIMULATION VARIABLES
!
      INTEGER*2
!     +                     YEAR, ! INT2
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
     +                     INTERRUPTIBLE_MARKUP_ADDER,
     +                     INTERRUPTIBLE_PERCENT
      CHARACTER*30
     +                     CUSTOMER_CLASS_NAME, ! CHAR*30
     +                     CALCULATION_MODE*1, ! CHAR*1
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
      INTEGER HESI_TRANS_AREA_ID_NUM

      CHARACTER*17 FILE_TYPE/'Coal Demand      '/
      LOGICAL*1 LAHEY_LF95
      CHARACTER*30 SCREEN_OUTPUT
      CHARACTER*2 R_COAL_DEMAND_OL

      INTEGER*2 COAL_DEMAND_YEAR,STUDY_BASE_YEAR

      SAVE STUDY_BASE_YEAR
!
! MULTI-FILE VARIABLES
!
      INTEGER FILE_NUMBER,FILE_ID
      INTEGER MAX_COAL_DEMAND_FILES
      INTEGER*2 BC_MASTR_REC,CUR_REC,UNIT_NO
      INTEGER*2 OL_MASTR_REC
      CHARACTER*1 DEMAND_PRICING_METHOD 
      PARAMETER (MAX_COAL_DEMAND_FILES=9)
      CHARACTER*2 COAL_DEMAND_OL/'BC'/,
     +            COAL_DEMAND_MULTI_OL_CODES(1:9)
     +                                      /MAX_COAL_DEMAND_FILES*'BC'/
      CHARACTER*5 COAL_DEMAND_FILE_BASE_NAMES(
     +                                       MAX_COAL_DEMAND_FILES),
     +            VOID_CHR,BASE_FILE_NAME,
     +            COAL_DEMAND_BASE_NAME,
     +            OVERLAY_FAMILY_NAMES(MAX_COAL_DEMAND_FILES)
      CHARACTER*2 COAL_DEMAND_FILE_CODES(0:MAX_COAL_DEMAND_FILES-1)/
     +                            'H1','H2','H3','H4','H5',
     +                            'H6','H7','H8','H9'/,
     +            FILE_CODE
      CHARACTER*6 COAL_DEMAND_FILE_BINARY_NAMES(
     +                         MAX_COAL_DEMAND_FILES)/
     +                                                   'CSYFC1',
     +                                                   'CSYFC2',
     +                                                   'CSYFC3',
     +                                                   'CSYFC4',
     +                                                   'CSYFC5',
     +                                                   'CSYFC6',
     +                                                   'CSYFC7',
     +                                                   'CSYFC8',
     +                                                   'CSYFC9'/,
     +            BINARY_FILE_NAME
      LOGICAL ACTIVE_BASE_COAL_DEMAND_FILES(MAX_COAL_DEMAND_FILES)/
     +                                   MAX_COAL_DEMAND_FILES*.FALSE./,
     +        ACTIVE_OVERLAY_COAL_DEMAND_FILES(
     +                                       MAX_COAL_DEMAND_FILES)/
     +                                    MAX_COAL_DEMAND_FILES*.FALSE./
      LOGICAL*1 OVERLAY_NAME_ACTIVE(MAX_COAL_DEMAND_FILES),
     +          COAL_DEMAND_OVERLAY_MASTR_FILE_OPEN/.FALSE./
      INTEGER*2 INTRA_ASSET_CLASS_ID,INTRA_ASSET_CLASS_ALLOCATION
      CHARACTER*30 INTRA_ACCOUNT_CLASSIFICATION
      CHARACTER*3 INTRA_EXPENSE_COLLECTION
      CHARACTER*1 INTRA_COMPANY_TRANSACTION


!          ROUTINE TO CONVERT METAFILE FILES TO DIRECT-ACESS BINARY
!          COPYRIGHT (C) 1983, 84, 85  M.S. GERBER & ASSOCIATES, INC.



! CONVERT THE SYSTEM-FORECAST FILE


      ENTRY COAL_DEMAND_FORECAST_MAKEBIN

      STUDY_BASE_YEAR = get_base_year()

      COAL_DEMAND_YEAR = 0
! SINGLE FILE FOR NOW      
!      COAL_DEMAND_BASE_NAME = COAL_DEMAND_FORECAST_FILE()
      VOID_CHR = COAL_DEMAND_FORECAST_FILE(COAL_DEMAND_FILE_BASE_NAMES)
      IF(.NOT. LAHEY_LF95())
     +       CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
      IF(EXTENSION_PERIOD_GROWTH_SWITCH == 'F')
     +                          FORECAST_GROWTH_YEARS = AVAIL_DATA_YEARS
      BC_MASTR_REC = 0
      SAVE_TRANS_LOAD_TABLES = 0
      DO FILE_ID = 1, MAX_COAL_DEMAND_FILES
!         
         BASE_FILE_NAME = COAL_DEMAND_FILE_BASE_NAMES(FILE_ID)
!         BASE_FILE_NAME = COAL_DEMAND_BASE_NAME
         FILE_NAME = trim(BASE_FILE_DIRECTORY())//
     +                    trim(COAL_DEMAND_FILE_CODES(FILE_ID))//"B"//
     +                                    trim(BASE_FILE_NAME)//".DAT"
         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
         IF(FILE_EXISTS) THEN
!
            IF(.NOT. SAVE_COAL_DEMAND_FILE_EXISTS) THEN
               ALLOCATE(TEMP_TRANS_CLASS_POINTER(0:1023),
     +                  TEMP_ASSET_CLASS_POINTER(0:1023),
     +                  TEMP_CUST_CLASS_POINTER(0:1023))
               TEMP_TRANS_CLASS_POINTER = 0
               TEMP_ASSET_CLASS_POINTER = 0
               TEMP_CUST_CLASS_POINTER = 0
!
               OPEN(12,FILE=trim(OUTPUT_DIRECTORY())//"BCCSYFC.BIN",
     +                      ACCESS="DIRECT",STATUS="REPLACE",RECL=LRECL)
               SAVE_COAL_DEMAND_FILE_EXISTS = .TRUE.
            ENDIF
!         


            IF(LAHEY_LF95()) THEN
               SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
               CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
            ELSE
               CALL MG_LOCATE_WRITE(16,30,BASE_FILE_NAME,ALL_VERSIONS,0)
            ENDIF
!
            ACTIVE_BASE_COAL_DEMAND_FILES(FILE_ID) = .TRUE.
            MONTHLY_ENERGY = 0.
            MONTHLY_PEAK = 0.
            MONTHLY_CUSTOMERS = 0.
            OPEN(10,FILE=FILE_NAME)
            OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BC"//
     +                 COAL_DEMAND_FILE_BINARY_NAMES(FILE_ID)//".BIN",
     +                      ACCESS="DIRECT",STATUS="REPLACE",RECL=LRECL)
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
            SPECIAL_CUSTOMER_TYPE = 'F'
            INTERRUPTIBLE_MARKUP_PERENT = 0.
            INTERRUPTIBLE_MARKUP_CAP = 999999.
            INTERRUPTIBLE_MAX_CAPACITY = 0.
            INTERRUPTIBLE_MIN_CAPACITY = 999999.
            INTERRUPTIBLE_MARKUP_ADDER = 0.
            INTERRUPTIBLE_PERCENT = 0.
!
!            SAVE_TRANS_LOAD_TABLES = 0 moved outside file loop 4/17/01
!
            TABLE_ACTIVE = 'T'
            BASECASE_MARKET_AREA_ID = 'BLANK ' ! CHAR*6
            BASECASE_TRANS_AREA_ID = 'BLANK ' ! CHAR*6
            BASECASE_NERC_SUB_ID = 'BLANK ' ! CHAR*6
            MONTHLY_UNITS = 'E'
            PRICE_INDEX_ACTIVE = 'F'
            REVENUE_CLASS_ENERGY = 'Secondary Sales'
            REVENUE_CLASS_DEMAND = 'Capacity Sales'
            REVENUE_CLASS_CUST = 'Customer Revenues'
            REVENUE_INDEX_ENERGY = 'Residential Sales'
            DEMAND_PRICING_METHOD = 'M'
            SCENARIO_VARIABLE = 
     +                        '                                        '
            THREE_FACTOR_TRANSFORM = 'F'
            LOAD_DISPATCH_POSITION = -99
!
            JURISDICTIONAL_CUSTOMER = 'N'
            FUEL_COST_RECOVERY_THROUGH_FAC = 'N'
            BASE_COST_OF_FAC_FUEL = 0.
!
            IREC = 0
            READ(10,*) DELETE
            DO ! TABLES
               COAL_DEMAND_YEAR = 1
               INTRA_COMPANY_TRANSACTION = 'N'
               INTRA_ASSET_CLASS_ID = 0
               INTRA_ASSET_CLASS_ALLOCATION = 0
               INTRA_ACCOUNT_CLASSIFICATION = ' '
               INTRA_EXPENSE_COLLECTION = 'ATL'
               DO ! YEAR-BASED RECORDS
                  READ(10,1000,IOSTAT=IOS) RECLN
!                  
!                  IF(IOS /= 0) EXIT ! END OF FILE
!
                  IF(RECLN(1:1) == '7' .OR. IOS /= 0) THEN ! END OF TABLE ! EXIT AT BOTTOM OF IF
                     IF(COAL_DEMAND_YEAR <= AVAIL_DATA_YEARS) THEN
                        DO COAL_DEMAND_YEAR = 
     +                                COAL_DEMAND_YEAR, AVAIL_DATA_YEARS
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
     +                                                    CUSTOMER_GROUP
!
                              WRITE(UNIT_NO,REC=CUR_REC) DELETE,
     +                           COAL_DEMAND_YEAR+STUDY_BASE_YEAR, ! INT2
     +                           CUSTOMER_GROUP, ! INT2 
     +                           CUSTOMER_CLASS_NAME, ! CHAR*30
     +                           CALCULATION_MODE, ! CHAR*1
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
     +                           BASECASE_MARKET_AREA_ID, ! CHAR*6
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
     +                           SPECIAL_CUSTOMER_TYPE,
     +                           INTERRUPTIBLE_MARKUP_PERENT,
     +                           INTERRUPTIBLE_MARKUP_CAP,
     +                           INTERRUPTIBLE_MAX_CAPACITY,
     +                           INTERRUPTIBLE_MIN_CAPACITY,
     +                           INTERRUPTIBLE_MARKUP_ADDER,
     +                           INTERRUPTIBLE_PERCENT

                           ENDDO
                        ENDDO
                     ENDIF ! DETECTED NEW TABLE OR END OF FILE
!
                     EXIT ! LEAVE LOOP (GO TO NEXT TABLE OR EXIT ROUTINE)
!                     
                  ENDIF
                  RECLN=trim(RECLN)//
     +                       ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
                  READ(RECLN,*,ERR=200) DELETE,
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
     +                        BASECASE_MARKET_AREA_ID, ! CHAR*6
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
     +                        SPECIAL_CUSTOMER_TYPE,
     +                        INTERRUPTIBLE_MARKUP_PERENT,
     +                        INTERRUPTIBLE_MARKUP_CAP,
     +                        INTERRUPTIBLE_MAX_CAPACITY,
     +                        INTERRUPTIBLE_MIN_CAPACITY,
     +                        INTERRUPTIBLE_MARKUP_ADDER,
     +                        INTERRUPTIBLE_PERCENT
!
                  IREC = IREC + 1
!                  
                  READ(RECLN,*,ERR=200) DELETE,YEAR
                  TEMP_YEAR = MIN(AVAIL_DATA_YEARS,YEAR-STUDY_BASE_YEAR)
!
                  ASSET_CLASS_ID = ASSET_CLASS_ID_REAL
                  IF(TEMP_TRANS_CLASS_POINTER(TRANSACTION_GROUP)==0)THEN
                     NUM_OF_BC_TRANS_CLASSES = NUM_OF_BC_TRANS_CLASSES+1
                     MAX_BC_TRANS_CLASS_ID_NUM = MAX(
     +                      MAX_BC_TRANS_CLASS_ID_NUM,TRANSACTION_GROUP)
                     TEMP_TRANS_CLASS_POINTER(TRANSACTION_GROUP) = 1
                  ENDIF
                  IF(TEMP_ASSET_CLASS_POINTER(ASSET_CLASS_ID) == 0) THEN
                     NUM_OF_BC_ASSET_CLASSES = NUM_OF_BC_ASSET_CLASSES+1
                     MAX_BC_ASSET_CLASS_ID_NUM = MAX(
     +                         MAX_BC_ASSET_CLASS_ID_NUM,ASSET_CLASS_ID)
                     TEMP_ASSET_CLASS_POINTER(ASSET_CLASS_ID) = 1
                  ENDIF
                  IF(INTRA_COMPANY_TRANSACTION == 'Y') THEN
                     IF(TEMP_ASSET_CLASS_POINTER(INTRA_ASSET_CLASS_ID)
     +                                                        == 0) THEN
                        NUM_OF_BC_ASSET_CLASSES =
     +                                       NUM_OF_BC_ASSET_CLASSES + 1
                        MAX_BC_ASSET_CLASS_ID_NUM = MAX(
     +                   MAX_BC_ASSET_CLASS_ID_NUM,INTRA_ASSET_CLASS_ID)
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
                  IF(TEMP_YEAR > COAL_DEMAND_YEAR) THEN ! FILL IN MISSING YEARS
                     IF(IREC == 1) THEN
                        WRITE(4,*)"The first year of the first table in"
                        WRITE(4,*)"the Transact Forecast file is ",YEAR
                        WRITE(4,*)"while the base year set in project"
                        WRITE(4,*)"information is ",STUDY_BASE_YEAR
                        WRITE(4,*)"First forecast year in the Transact"
                        WRITE(4,*)"Forecast file first year must be one"
                        WRITE(4,*)"year greater than the base year."
                        WRITE(4,*)" " 
          er_message='coal_obj:0002 - Year mismatch see WARNINGS file.'
                        call end_program(er_message)
                     ENDIF
                     DO COAL_DEMAND_YEAR = COAL_DEMAND_YEAR, 
     +                                                     TEMP_YEAR - 1
                        BC_MASTR_REC = BC_MASTR_REC + 1
                        DO UNIT_NO = 11, 12
                           IF(UNIT_NO == 11) THEN
                              CUR_REC = IREC
                           ELSE
                              CUR_REC = BC_MASTR_REC
                           ENDIF
!
                           FILE_TABLE_INDEX = (FILE_ID+1)*1000 + 
     +                                                    CUSTOMER_GROUP
!
                           WRITE(UNIT_NO,REC=CUR_REC) DELETE,
     +                        COAL_DEMAND_YEAR+STUDY_BASE_YEAR, ! INT2
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
!     +                        COMMENT, ! CHAR*50
     +                        DIST_ENERGY_LOSS_FACTOR, ! REAL4
     +                        TRANS_ENERGY_LOSS_FACTOR, ! REAL4
     +                        PEAK_LOSS_FACTOR, ! REAL4
     +                        PEAK_COIN_FACTOR, ! REAL4
     +                        DISTRIBUTION_PRICE, ! REAL4
     +                        TRANSMISSION_PRICE, !REAL4
     +                        TABLE_ACTIVE,
     +                        BASECASE_MARKET_AREA_ID, ! CHAR*6
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
     +                        SPECIAL_CUSTOMER_TYPE,
     +                        INTERRUPTIBLE_MARKUP_PERENT,
     +                        INTERRUPTIBLE_MARKUP_CAP,
     +                        INTERRUPTIBLE_MAX_CAPACITY,
     +                        INTERRUPTIBLE_MIN_CAPACITY,
     +                        INTERRUPTIBLE_MARKUP_ADDER,
     +                        INTERRUPTIBLE_PERCENT
                        ENDDO
                        IREC = IREC + 1
                     ENDDO
                  
!                     COAL_DEMAND_YEAR = COAL_DEMAND_YEAR + 1 ! OUT. 11/25/98. GAT.
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
     +                                                    CUSTOMER_GROUP
!
                     WRITE(UNIT_NO,REC=CUR_REC) DELETE,
     +                        COAL_DEMAND_YEAR+STUDY_BASE_YEAR, ! INT2
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
!     +                        COMMENT, ! CHAR*50
     +                        DIST_ENERGY_LOSS_FACTOR, ! REAL4
     +                        TRANS_ENERGY_LOSS_FACTOR, ! REAL4
     +                        PEAK_LOSS_FACTOR, ! REAL4
     +                        PEAK_COIN_FACTOR, ! REAL4
     +                        DISTRIBUTION_PRICE, ! REAL4
     +                        TRANSMISSION_PRICE, !REAL4
     +                        TABLE_ACTIVE,
     +                        BASECASE_MARKET_AREA_ID, ! CHAR*6
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
     +                        SPECIAL_CUSTOMER_TYPE,
     +                        INTERRUPTIBLE_MARKUP_PERENT,
     +                        INTERRUPTIBLE_MARKUP_CAP,
     +                        INTERRUPTIBLE_MAX_CAPACITY,
     +                        INTERRUPTIBLE_MIN_CAPACITY,
     +                        INTERRUPTIBLE_MARKUP_ADDER,
     +                        INTERRUPTIBLE_PERCENT
                  ENDDO ! WRITE TO UNITS 11 & 12
                  COAL_DEMAND_YEAR = COAL_DEMAND_YEAR + 1
!                  CALL CALCULATE_TSYTEM_PEAKS(TSY_FC_DATA,COAL_DEMAND_YEAR)
               ENDDO ! LOAD GROUP
               SAVE_TRANS_LOAD_TABLES = SAVE_TRANS_LOAD_TABLES + 1
               IF(IOS /= 0) EXIT
            ENDDO ! READ TABLES
            CLOSE(10)
            CLOSE(11)
         ELSE IF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
            CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "coal_obj:0006")
         ENDIF ! FILE EXISTS
      ENDDO ! FILE LOOP
      IF(.NOT. SAVE_COAL_DEMAND_FILE_EXISTS) THEN
         OPEN(12,FILE=trim(OUTPUT_DIRECTORY())//"BCCSYFC.BIN",
     +                      ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
!
         SAVE_COAL_DEMAND_FILE_EXISTS = .FALSE.
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

      ENTRY COAL_DEMAND_FORECAST_MAKEOVL(OVERLAY_FAMILY_NAMES,
     +                                              OVERLAY_NAME_ACTIVE)

      CALL LOCATE(10,51)
      DO FILE_ID = 1, MAX_COAL_DEMAND_FILES
         IF(ACTIVE_BASE_COAL_DEMAND_FILES(FILE_ID)) THEN
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
            EXIT
         ENDIF
      ENDDO
      DO FILE_ID = 1, MAX_COAL_DEMAND_FILES
         IF(.NOT. ACTIVE_BASE_COAL_DEMAND_FILES(FILE_ID)) CYCLE
         IF(.NOT. COAL_DEMAND_OVERLAY_MASTR_FILE_OPEN) THEN
            OPEN(13,FILE=trim(OUTPUT_DIRECTORY())//"OLCSYFC.BIN",
     +                      STATUS='REPLACE',ACCESS="DIRECT",RECL=LRECL)
            COAL_DEMAND_OVERLAY_MASTR_FILE_OPEN = .TRUE.
            OL_MASTR_REC = 0
            COAL_DEMAND_OL = 'OL' ! AT LEAST ONE OVERLAY FIEL WAS USED
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
     +              COAL_DEMAND_MULTI_OL_CODES(FILE_ID)//
     +                  COAL_DEMAND_FILE_BINARY_NAMES(FILE_ID)//".BIN",
     +                      STATUS="UNKNOWN",ACCESS="DIRECT",RECL=LRECL)
            IREC = 0
            DO
               IREC = IREC + 1
               READ(10,REC=IREC,IOSTAT=IOS) RECLN(1:LRECL)
               IF(IOS /= 0) EXIT
               OL_MASTR_REC = OL_MASTR_REC + 1
               WRITE(13,REC=OL_MASTR_REC) RECLN(1:LRECL)
            ENDDO
            CLOSE(10)     
         ELSE ! CREATE INDIVIDUAL BINARY OVERLAY FILE AND ADD TO MASTER OL FILE
!      
            FILE_NAME=trim(OUTPUT_DIRECTORY())//
     +              trim(COAL_DEMAND_FILE_CODES(FILE_ID))//"O"//
     +                     trim(OVERLAY_FAMILY_NAMES(FILE_ID))//".DAT"
            OPEN(10,FILE=FILE_NAME)
            READ(10,*) DELETE
            COAL_DEMAND_YEAR = 0
            INUNIT = 12
            IF(COAL_DEMAND_MULTI_OL_CODES(FILE_ID) == 'BC') THEN
               OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BC"//
     +                   COAL_DEMAND_FILE_BINARY_NAMES(FILE_ID)//".BIN",
     +                                       ACCESS="DIRECT",RECL=LRECL)
               INUNIT = 11
            ENDIF
!
            OPEN(12,FILE=trim(OUTPUT_DIRECTORY())//"OL"//
     +                   COAL_DEMAND_FILE_BINARY_NAMES(FILE_ID)//".BIN",
     +           STATUS="UNKNOWN",ACCESS="DIRECT",RECL=LRECL,IOSTAT=IOS)
            IF(IOS /= 0) THEN
               CALL IOSTAT_MSG(IOS,MESSAGE)
               WRITE(4,*) trim(MESSAGE)
               WRITE(4,*) '*** line 189 COAL_DEMAND_OBJT.FOR ***'
               er_message='See WARNING MESSAGES-coal-obj-11'
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
            READ(RECLN,*,ERR=200) DELETE,COAL_DEMAND_YEAR
!
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
     +                     BASECASE_MARKET_AREA_ID, ! CHAR*6
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
     +                     SPECIAL_CUSTOMER_TYPE,
     +                     INTERRUPTIBLE_MARKUP_PERENT,
     +                     INTERRUPTIBLE_MARKUP_CAP,
     +                     INTERRUPTIBLE_MAX_CAPACITY,
     +                     INTERRUPTIBLE_MIN_CAPACITY,
     +                     INTERRUPTIBLE_MARKUP_ADDER,
     +                     INTERRUPTIBLE_PERCENT
               IF(IOS /= 0) EXIT ! END OF BINARY FILE
!
               IF(YEAR == COAL_DEMAND_YEAR) THEN
                  RECLN=trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
                  READ(RECLN,*,ERR=200)  
     +                     DELETE,
     +                     COAL_DEMAND_YEAR, ! INT2
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
     +                     BASECASE_MARKET_AREA_ID, ! CHAR*6
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
     +                     SPECIAL_CUSTOMER_TYPE,
     +                     INTERRUPTIBLE_MARKUP_PERENT,
     +                     INTERRUPTIBLE_MARKUP_CAP,
     +                     INTERRUPTIBLE_MAX_CAPACITY,
     +                     INTERRUPTIBLE_MIN_CAPACITY,
     +                     INTERRUPTIBLE_MARKUP_ADDER,
     +                     INTERRUPTIBLE_PERCENT
                  READ(10,1000,IOSTAT=IOS) RECLN
!
                  DOWHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE 
!              
                     READ(10,1000,IOSTAT=IOS) RECLN
!         
                  ENDDO
                  READ(RECLN,*,ERR=200) DELETE,COAL_DEMAND_YEAR
               ENDIF
!         IF(COAL_DEMAND_YEAR /= YEAR - STUDY_BASE_YEAR) THEN
!            WRITE(4,*) 'The Overlay System Forecast File  ',
!     +                                     OVERLAY_FAMILY_NAME,','
!            WRITE(4,*) 'Record',COAL_DEMAND_YEAR,'  and Year',YEAR
!            WRITE(4,*) 'is inconsistent with the Base Year',
!     +                                              STUDY_BASE_YEAR
!            WRITE(4,*) 'in Set Parameters. Either reset Base '
!            WRITE(4,*) 'Year in Set Parameters or the Year in'
!            WRITE(4,*) 'System Forecast.'
!            WRITE(4,*) ' '
!         ENDIF
!
               ASSET_CLASS_ID = ASSET_CLASS_ID_REAL
               IF(TEMP_TRANS_CLASS_POINTER(TRANSACTION_GROUP) == 0) THEN
                  NUM_OF_OL_TRANS_CLASSES = NUM_OF_OL_TRANS_CLASSES + 1
                  MAX_OL_TRANS_CLASS_ID_NUM = MAX(
     +                      MAX_OL_TRANS_CLASS_ID_NUM,TRANSACTION_GROUP)
                  TEMP_TRANS_CLASS_POINTER(TRANSACTION_GROUP) = 1
               ENDIF
               IF(TEMP_ASSET_CLASS_POINTER(ASSET_CLASS_ID) == 0) THEN
                  NUM_OF_OL_ASSET_CLASSES = NUM_OF_OL_ASSET_CLASSES + 1
                  MAX_OL_ASSET_CLASS_ID_NUM = MAX(
     +                         MAX_OL_ASSET_CLASS_ID_NUM,ASSET_CLASS_ID)
                  TEMP_ASSET_CLASS_POINTER(ASSET_CLASS_ID) = 1
               ENDIF
               IF(INTRA_COMPANY_TRANSACTION == 'Y') THEN
                  IF(TEMP_ASSET_CLASS_POINTER(INTRA_ASSET_CLASS_ID)
     +                                                        == 0) THEN
                     NUM_OF_OL_ASSET_CLASSES =
     +                                       NUM_OF_OL_ASSET_CLASSES + 1
                     MAX_OL_ASSET_CLASS_ID_NUM = MAX(
     +                   MAX_OL_ASSET_CLASS_ID_NUM,INTRA_ASSET_CLASS_ID)
                     TEMP_ASSET_CLASS_POINTER(INTRA_ASSET_CLASS_ID) = 1
                  ENDIF
               ENDIF
               IF(TEMP_CUST_CLASS_POINTER(CUSTOMER_GROUP) == 0) THEN
                  NUM_OF_OL_CUST_CLASSES = NUM_OF_OL_CUST_CLASSES + 1
                  MAX_OL_CUST_CLASS_ID_NUM = MAX(
     +                          MAX_OL_CUST_CLASS_ID_NUM,CUSTOMER_GROUP)
                  TEMP_CUST_CLASS_POINTER(CUSTOMER_GROUP) = 1
               ENDIF

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
     +                     BASECASE_MARKET_AREA_ID, ! CHAR*6
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
     +                     SPECIAL_CUSTOMER_TYPE,
     +                     INTERRUPTIBLE_MARKUP_PERENT,
     +                     INTERRUPTIBLE_MARKUP_CAP,
     +                     INTERRUPTIBLE_MAX_CAPACITY,
     +                     INTERRUPTIBLE_MIN_CAPACITY,
     +                     INTERRUPTIBLE_MARKUP_ADDER,
     +                     INTERRUPTIBLE_PERCENT
               ENDDO ! WRITE TO BINARY FILES LOOP
!            CALL CALCULATE_TSYTEM_PEAKS(TSY_FC_DATA,COAL_DEMAND_YEAR)
            ENDDO ! END TABLE LOOP
            CLOSE(10)
            CLOSE(12)
            IF(COAL_DEMAND_MULTI_OL_CODES(FILE_ID) == 'BC') CLOSE(11)
            COAL_DEMAND_MULTI_OL_CODES(FILE_ID) = 'OL'
         ENDIF
      ENDDO ! FILE LOOP
      IF(COAL_DEMAND_OVERLAY_MASTR_FILE_OPEN) CLOSE(13)
      IF(ALLOCATED(TEMP_TRANS_CLASS_POINTER))
     +      DEALLOCATE(TEMP_TRANS_CLASS_POINTER,
     +                 TEMP_ASSET_CLASS_POINTER,
     +                 TEMP_CUST_CLASS_POINTER)
      COAL_DEMAND_OVERLAY_MASTR_FILE_OPEN = .FALSE.
      RETURN
!

      ENTRY COAL_DEMAND_GET_PEAKS ! this is not called by any routine MSG 4/18/01

!
      OPEN(10,FILE=trim(OUTPUT_DIRECTORY())//COAL_DEMAND_OL//
     +                           "CSYFC.BIN",ACCESS="DIRECT",RECL=LRECL)
      IREC = 0
      DO
         IREC = IREC + 1
         READ(10,REC=IREC,IOSTAT=IOS)  DELETE,
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
     +                     BASECASE_MARKET_AREA_ID, ! CHAR*6
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
     +                     SPECIAL_CUSTOMER_TYPE,
     +                     INTERRUPTIBLE_MARKUP_PERENT,
     +                     INTERRUPTIBLE_MARKUP_CAP,
     +                     INTERRUPTIBLE_MAX_CAPACITY,
     +                     INTERRUPTIBLE_MIN_CAPACITY,
     +                     INTERRUPTIBLE_MARKUP_ADDER,
     +                     INTERRUPTIBLE_PERCENT
         IF(IOS /= 0) EXIT

      ENDDO
      CLOSE(10)
      RETURN

  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from coal_obj SIID43'
      call end_program(er_message)


      ENTRY RESET_COAL_DEMAND_OL

         COAL_DEMAND_OL = 'BC'
         DO FILE_ID = 0, MAX_COAL_DEMAND_FILES-1 
            COAL_DEMAND_MULTI_OL_CODES(FILE_ID) = 'BC'
            ACTIVE_OVERLAY_COAL_DEMAND_FILES(FILE_ID) = .FALSE.
         ENDDO
!        SAVE_COAL_DEMAND_FILE_USED = .FALSE.
!        COAL_DEMAND_OVERLAY_MASTR_FILE_OPEN = .FALSE.
      RETURN
!

      ENTRY RETURN_COAL_DEMAND_OL(R_COAL_DEMAND_OL,R_LRECL)

         R_COAL_DEMAND_OL = COAL_DEMAND_OL
         R_LRECL = LRECL
      RETURN

      ENTRY DOES_COAL_DEMAND_FILE_EXIST(R_COAL_DEMAND_FILE_EXISTS)

         R_COAL_DEMAND_FILE_EXISTS = SAVE_COAL_DEMAND_FILE_EXISTS
      RETURN

      ENTRY GET_COAL_DEMAND_TABLES(R_TRANS_LOAD_TABLES)

         R_TRANS_LOAD_TABLES = SAVE_TRANS_LOAD_TABLES
      RETURN

      ENTRY COAL_DEMAND_FILE_USED_THIS_ENDPOINT(R_COAL_DEMAND_FILE_USED)

         R_COAL_DEMAND_FILE_USED = SAVE_COAL_DEMAND_FILE_USED
!        SAVE_COAL_DEMAND_FILE_USED = .TRUE.
      RETURN

      ENTRY RETURN_COAL_DEMAND_GROUP_INFO(R_NUM_OF_TRANS_CLASSES,
     +                           R_MAX_TRANS_CLASS_NUM,
     +                           R_NUM_OF_ASSET_CLASSES,
     +                           R_MAX_ASSET_CLASS_NUM,
     +                           R_NUM_OF_CUST_CLASSES,
     +                           R_MAX_CUST_CLASS_NUM)

         IF(COAL_DEMAND_OL == 'OL') THEN
            R_NUM_OF_TRANS_CLASSES = NUM_OF_OL_TRANS_CLASSES
            R_MAX_TRANS_CLASS_NUM = MAX(MAX_OL_TRANS_CLASS_ID_NUM,
     +                                        MAX_BC_TRANS_CLASS_ID_NUM)
            R_NUM_OF_ASSET_CLASSES = NUM_OF_OL_ASSET_CLASSES
            R_MAX_ASSET_CLASS_NUM = MAX(1,MAX_OL_ASSET_CLASS_ID_NUM,
     +                                        MAX_BC_ASSET_CLASS_ID_NUM)
            R_NUM_OF_CUST_CLASSES = NUM_OF_OL_CUST_CLASSES
            R_MAX_CUST_CLASS_NUM = MAX(MAX_OL_CUST_CLASS_ID_NUM,
     +                                         MAX_BC_CUST_CLASS_ID_NUM)
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
      FUNCTION MANAGE_COAL_DEMAND_FORECASTS()
      use end_routine, only: end_program, er_message
!         

      use spindriftlib
      use prod_arrays_dimensions
      USE SIZECOM
      use globecom
      use mthnmcom
      use namescom
      use trancom
      use tf_objt_interfaces
      SAVE

!      
      INTEGER*2   R_CLASS,MO,R_MONTH,
     +            GLOBAL_PEAK_MONTH/0/,
     +            VOID_I,UPDATE_PLANNING_PEAK_MONTH,
     +            LREC,J,SI,X
      REAL*4      
     +            R_MONTHLY_VALUES(0:12),
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
     +            MAX_COAL_LOAD_GROUPS/0/,TG,TGP,
     +            GET_MAX_COAL_LOAD_GROUPS,
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
     +            GET_MAX_COAL_GROUP_NUMBER,
     +            DAY, 
     +            HOUR_IN_DAY,
     +            R_COAL_GROUP
      INTEGER IOS,UNIT_NO
      REAL*4      R_HR_COST,AC_COST_FACTOR,
     +            GET_TRANS_GROUP_PEAK,
     +            GET_SCENARIO_ELECTRIC_DEMAND,
     +            GET_SCENARIO_COAL_DEMAND,
     +            GET_PARAM_ELECTRIC_DEMAND,
     +            GET_SCENARIO_PEAK,
     +            GET_PARAM_PEAK,
     +            GET_SCENARIO_ENERGY,
     +            GET_PARAM_ENERGY,
     +            MONTHLY_MAINT_VECTOR,
     +            HOURLY_LOAD_FROM_AC_TG,
     +            MONTH_PEAK_BY_CG,
!     +            R_HYDRO_LOAD(*),
     +            TEMP_R,
     +            TEMP1_R,
     +            TEMP2_R,
     +            INTERVAL_CAPACITY,
     +            R_PEAK,R_VOLUME
      LOGICAL*4 LOAD_FILE_OPEN,FILE_EXISTS
      LOGICAL*1   MANAGE_COAL_DEMAND_FORECASTS,
     +            ANNUAL_COAL_DEMAND,
     +            MONTHLY_COAL_DEMAND,
     +            MONTHLY_TRANSACTION_DURATION,
     +            DEALLOCATE_ANNUAL_COAL_LOADS,
     +            DEALLOCATE_MONTHLY_COAL_DEMAND,
     +            COAL_DEMAND_FILE_EXISTS,
     +            SAVE_COAL_DEMAND_FILE_EXISTS/.FALSE./,
     +            WD_FILE_EXISTS/.FALSE./,
     +            GW_FILE_EXISTS/.FALSE./,
     +            WH_FILE_EXISTS/.FALSE./,
     +            GET_WD_LOAD,
     +            GET_GW_LOAD,
     +            YES_USE_COAL_DEMAND_FILE_FOR_PRICE,
     +            YES_USE_COAL_DEMAND_FILE_FOR_MULTIAREA,
     +            COAL_DEMAND_GROUP1_ACTIVE/.FALSE./,
     +            COAL_DEMAND_ANY_GROUP_ACTIVE/.FALSE./,
     +            YES_RUN_COAL_MODEL/.FALSE./,RUN_COAL_MODEL,
     +            COAL_GROUP_ACTIVE_SWITCH,
     +            IN_ACTIVE_MARKET_AREA,
     +            COAL_DEMAND_FILE_LOAD_GROUP_ACTIVE,
     +            COAL_DEMAND_IPL_ELECTIC_PLAN_COST,
     +            COAL_DEMAND_FILE_USED,
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
     +            ACTIVE_WD_FORECAST,
     +            ACTIVE_GW_FORECAST,
     +            YES_HOURLY_CUST_MARGIN_REPORT,
     +            HOURLY_CUST_MARGIN_REPORT,
     +            YES_TRANS_INTR_REPORT,
     +            TRANS_INTR_REPORT,
     +            YES_MID_TERM_PEAK_UNCER,
     +            MID_TERM_PEAK_UNCER,
     +            YES_ZONAL_LEVEL_MARKET,
     +            ZONAL_LEVEL_MARKET,
     +            INTERRUPTIBLE_PRICING,
     +            COAL_DEMAND_GROUP_ACTIVE,
     +            GET_COAL_ANNUAL_PEAK_VOLUME,
     +            DEMAND_GROUP_AVAILABLE(:)
      INTEGER*2   COAL_LOAD_GROUPS_INDEX(:),
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
     +            COAL_LOAD_2_TRANS_GROUPS(:),
     +            COAL_LOAD_GROUP_2_TG(:),
     +            CUST_CLASS_GROUP_2_CG(:),
     +            CUST2_CLASS_GROUP_2_CG(:),
     +            ASSET_CLASS_GROUP_2_AC(:),
     +            MAX_TRANS_GROUPS/500/,
     +            MAX_TRANS_GROUPS_FROM_TG/0/,
     +            HIGHEST_TG_COUNT,
     +            GET_MAX_TRANS_GROUPS,
     +            NUM_OF_TRANS_CLASSES,
     +            MAX_TRANS_CLASS_NUM,
     +            NUM_OF_ASSET_CLASSES,
     +            MAX_ASSET_CLASS_NUM,
     +            NUM_OF_CUST_CLASSES,
     +            MAX_CUST_CLASS_NUM,
     +            GET_COAL_GROUP_POSITION,
     +            GET_COAL_GROUP_STATE_POSITION,
     +            TEMP_I2,
     +            STATE_POSITION,
     +            GET_COAL_NODE_STATE_PROVINCE,
     +            R_LOAD_GROUP,

     +            TG_FROM_TRANS_LOAD_GROUP,
     +            DAY_OF_WEEK(:,:),
     +            WD_INDEX(:),
     +            GW_INDEX(:),
     +            MK,
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
     +            R_HOURLY_DISPATCH_BLOCKS,
     +            R_LAST_HOUR,
     +            FILE_TABLE_2_TRANS_INDEX(0:15999), ! 10/28/02. 
     +            MAX_COAL_GROUP_NUMBER,
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
     +            SPECIAL_CUSTOMER_TYPE(:)
      CHARACTER*2 C2_NAME
      CHARACTER*5 MARKET_ID
      CHARACTER*6 BASECASE_MARKET_AREA_ID(:), 
     +            BASECASE_TRANS_AREA_ID(:),
     +            BASECASE_NERC_SUB_ID(:)
      CHARACTER*20 MARKET_AREA_NAME
      CHARACTER*30 GET_COAL_GROUP_NAME
      CHARACTER*256 TEMP_STR
      CHARACTER*30 LOCAL_CUSTOMER_NAME(:),TEMP_CUSTOMER_NAME
      CHARACTER*50 REVENUE_CLASS_ENERGY,
     +             REVENUE_CLASS_DEMAND,
     +             REVENUE_CLASS_CUST,
     +             REVENUE_INDEX_ENERGY
      CHARACTER*1 DEMAND_PRICING_METHOD(:)
      INTEGER*2 INTRA_ASSET_CLASS_ID(:),INTRA_ASSET_CLASS_ALLOCATION(:),
     +            SCENARIO_INDEX,GET_SCENARIO_INDEX
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
     +     TRANS_DAILY_LOAD(:,:),
     +     INTER_DAILY_AVAIL(:,:),
     +     CUSTOMER_DAILY_LOAD(:,:),
     +     WH_LOADS_PER_HOUR(:,:),
     +     HYDRO_HOURLY_LOAD(:,:),
     +     TABLE_HOURLY_LOAD(:,:),
     +     REF_HOURLY_LOAD,
     +     WH_TRANS_ALLOC(:,:),
     +     ASSET_CLASS_HOURLY_LOAD(:,:,:),
     +     MONTHLY_AC_COST_AT_MARKET(:,:),
     +     MONTHLY_AC_CONTRACT_REVENUE(:,:,:,:,:),
     +     MONTHLY_AC_CONTRACT_EXPENSE(:,:,:),
     +     GET_SCENARIO_BY_INDEX,
     +     LATIN_HYPERCUBE_ENERGY,
     +     LATIN_HYPERCUBE_PEAK,
!     +     R_GENERATION_BY_SEGMENT(*),
     +     TEMP_GEN,
     +     R_MARKET_PRICE,
     +     LOCAL_AVERAGE_PRICE,
     +     R_WHOLESALE_PURCHASE,
     +     R_WHOLESALE_SALES,
     +     R_WHOLESALE_PRICE,
     +     AVE_REV,
     +     R_UNSERVED_ENERGY,
     +     REMAINING_UNSERVED,
     +     HOURLY_INTERRUPIBLE_REVENUE(:,:,:),
     +     R_UNSERVED_COST,
     +     R_THERMAL_LOAD,
     +     NET_LOAD_ADJUSTMENTS,
     +     R_4_ZERO/0./,
!     +     R_DISPATCH_COST_FOR_BLOCK(*),
!     +     R_INCREMENTAL_FUEL_COST(*),
!     +     R_RETAIL_REVENUE_FOR_BLOCK(*),
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
     +               COAL_LOAD_GROUPS_INDEX,
     +               CUST_CLASS_GROUPS_INDEX,
     +               CUST2_CLASS_GROUPS_INDEX,
     +               COAL_LOAD_2_TRANS_GROUPS,
     +               COAL_LOAD_GROUP_2_TG,                
     +               CUST_CLASS_GROUP_2_CG,
     +               CUST2_CLASS_GROUP_2_CG,
     +               TRANS_HOURLY_LOAD,
     +               CUSTOMER_DAILY_LOAD,
     +               TRANS_DAILY_LOAD,
     +               INTER_DAILY_AVAIL,
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
     +               BASECASE_MARKET_AREA_ID,
     +               WD_INDEX,
     +               GW_INDEX,
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
     +               BASE_COST_OF_FAC_FUEL,
     +               DEMAND_GROUP_AVAILABLE
      CHARACTER*256 OUTPUT_DIRECTORY,FILE_NAME
      CHARACTER*2 COAL_DEMAND_OL
      INTEGER*2 ASSET_VECTOR
      INTEGER*2 NUMBER_OF_RATE_CLASSES,
     +           MAX_RATE_CLASS_ID_NUM,
     +          RATE_ASSET_CLASS_POINTER(1024)
!
      INTEGER* 2  HR_IN_MONTH,IREC,ALINE_LOAD_DATA,DA,DAYS_IN_MONTH,
     +            TIMZON,TEMPER,DELTMP,
     +            LDE_MONTH,LDE_DAY,LDE_YEAR,CURRENT_TRANS_GROUP,
     +            LOAD_UNIT,HR,MAX_HOURS_IN_MONTH/744/,R_HR,R_TG,R_CG,
     +            MAX_DAYS_IN_MONTH,GS,
     +            R_PA,K,
     +            GET_COAL_LOAD_2_TRANS_GROUPS,
     +            GET_TRANS_LOAD_AGGREGATION,
     +            TRANS_GROUP,
     +            GET_HG_FROM_TG,
     +            MONTHLY_MAINTENANCE_PENALTY,
     +            SCENARIO_HOURLY_UNIT/2201/,
     +            TG_POSITION_IN_TG_FILE,
     +            R_AC,
     +            TSYFRC_REC
      INTEGER*4   DAILY_LOADS_I4(24),VALUES_2_ZERO,WD_DAILY_LOADS_I4(24)
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
     +               791,792,793,794,795,796,797,798,799,800/
!     +            TEMP_HOURLY_INDEX(*)
      REAL*4 DAYS_PER_MONTH(12)/31,28,31,30,31,30,31,31,30,31,30,31/
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
      CHARACTER*8 EEICODE=" "

!
! SIMULATION VARIABLES
      
      INTEGER*2
     +                     CUSTOMER_GROUP(:), ! INT2 
     +                     CUSTOMER_GROUP_2(:),
     +                     iPort(:),
     +                     SNamePos(:),
     +                     COAL_GROUP_ID(:), ! INT2
     +                     REFERENCE_LOAD_NUMBER(:), ! INT2
     +                     GLOBAL_PA_PEAK_MONTH(:),
     +                     PA,
     +                     TG_2_PLANNING_AREA(:),
     +                     REF_LEAP_YEAR_DAY_SHIFT(:),
     +                     MONTHLY_MARKET_COUNTER(500),
     +                     NUMBER_OF_ACTIVE_GROUPS,
     +                     GET_BELONGS_TO_COAL_GROUP
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
     +                     MONTHLY_CLASS_ENERGY(:),
     +                     MONTHLY_CLASS_PEAK(:),
     +                     MONTHLY_CLASS_CUSTOMERS(:),
     +                     ANNUAL_CLASS_ENERGY_REVENUE(:),
     +                     ANNUAL_TRANS_INDEXED_REVENUE(:),
     +                     ANNUAL_CLASS_PEAK_REVENUE(:),
     +                     ANNUAL_CLASS_CUSTOMER_REVENUE(:),
     +                     ANNUAL_CLASS_ENERGY(:),
     +                     ANNUAL_CLASS_PEAK(:),
     +                     ANNUAL_CLASS_CUSTOMERS(:),
     +                     MINIMUM_MARKET_PRICE(:),
     +                     MAXIMUM_MARKET_PRICE(:),
     +                     INDEXED_ENERGY_PRICE(:),
     +                     COAL_DEMAND_PLANNING_PEAK(:,:),
     +                     PA_PLANNING_PEAK(:,:),
     +                     GET_PA_PEAK,
     +                     GLOBAL_PA_PEAK(:),
     +                     GLOBAL_PEAK,
     +                     ENERGY_LOSS_MULT(:),
     +                     R_13_ENERGY(0:12),
     +                     R_13_PEAK(0:12),
     +                     R_13_CUSTOMERS(0:12),
     +                     PROD_BY_MK_BY_MWH(3),
     +                     NG_BTUS_BY_GSP_BY_FUEL(3),
     +                     TG_CG_DATABASE(:,:,:,:),
     +                     TG_CG2_DATABASE(:,:,:,:),
     +                     INTERRUPTIBLE_MARKUP_PERENT(:),
     +                     INTERRUPTIBLE_MARKUP_CAP(:),
     +                     INTERRUPTIBLE_MAX_CAPACITY(:),
     +                     INTERRUPTIBLE_MIN_CAPACITY(:),
     +                     INTERRUPTIBLE_MARKUP_ADDER(:),
     +                     INTERRUPTIBLE_PERCENT(:),
     +                     CUM_BLOCK_GENERATION(:,:),
     +                     ANNUAL_TG_PEAK(:,:),
     +                     ANNUAL_TG_VOLUME(:)

!
!     
      REAL*8               MONTHLY_TRANS_ENERGY(:),
     +                     MONTHLY_HYDRO_ENERGY(:)
!      
      CHARACTER*30
     +                     CUSTOMER_CLASS_NAME(:), ! CHAR*30
     +                     CALCULATION_MODE(:)*1 ! CHAR*1
      CHARACTER*5          REFERENCE_LOAD_NAME(:),TEMP_LOAD_NAME ! CHAR*5
      INTEGER*2            TEMP_LOAD_NUMBER
      ALLOCATABLE ::
     +                     CUSTOMER_GROUP, ! INT2 
     +                     iPort,
     +                     SNamePos,
     +                     CUSTOMER_GROUP_2,
     +                     SPECIAL_CUSTOMER_TYPE,
     +                     INTERRUPTIBLE_MARKUP_PERENT,
     +                     INTERRUPTIBLE_MARKUP_CAP,
     +                     INTERRUPTIBLE_MAX_CAPACITY,
     +                     INTERRUPTIBLE_MIN_CAPACITY,
     +                     INTERRUPTIBLE_MARKUP_ADDER,
     +                     INTERRUPTIBLE_PERCENT,
     +                     CUM_BLOCK_GENERATION,
     +                     HOURLY_INTERRUPIBLE_REVENUE,
     +                     CUSTOMER_CLASS_NAME, ! CHAR*30
     +                     CALCULATION_MODE, ! CHAR*1
     +                     COAL_GROUP_ID, ! INT2
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
     +                     MONTHLY_CLASS_ENERGY,
     +                     MONTHLY_CLASS_PEAK,
     +                     MONTHLY_CLASS_CUSTOMERS,
     +                     ANNUAL_CLASS_ENERGY_REVENUE,
     +                     ANNUAL_TRANS_INDEXED_REVENUE,
     +                     ANNUAL_CLASS_PEAK_REVENUE,
     +                     ANNUAL_CLASS_CUSTOMER_REVENUE,
     +                     ANNUAL_CLASS_ENERGY,
     +                     ANNUAL_CLASS_PEAK,
     +                     ANNUAL_CLASS_CUSTOMERS,
     +                     LOCAL_CUSTOMER_NAME,
     +                     MINIMUM_MARKET_PRICE,
     +                     MAXIMUM_MARKET_PRICE,
     +                     INDEXED_ENERGY_PRICE,
     +                     COAL_DEMAND_PLANNING_PEAK,
     +                     PA_PLANNING_PEAK,
     +                     GLOBAL_PA_PEAK,
     +                     GLOBAL_PA_PEAK_MONTH,
     +                     TG_2_PLANNING_AREA,
     +                     ENERGY_LOSS_MULT,
     +                     REF_LEAP_YEAR_DAY_SHIFT,
     +                     REV_CLASS_INDEX,
     +                     DEMAND_PRICING_METHOD,
     +                     INTRA_COMPANY_TRANSACTION,
     +                     INTRA_ASSET_CLASS_ID,
     +                     INTRA_ASSET_CLASS_ALLOCATION,
     +                     INTRA_ACCOUNT_CLASSIFICATION,
     +                     INTRA_EXPENSE_COLLECTION,
     +                     ANNUAL_TG_PEAK,
     +                     ANNUAL_TG_VOLUME
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
!     +         ENERGY_LOSS_MULT/1.0/,
     +         PEAK_LOSS_MULT/1.0/,
     +         WEEKLY_HYDRO_ENERGY,
     +         WEEKLY_HYDRO_LOADS(168),
     +         COAL_DEMAND_LOADS_PER_HOUR(800),
     +         WEEKLY_HYDRO_MINIMUM_MW,
     +         WEEKLY_HYDRO_MAXIMUM_MW,
     +         SUM_24_I4,PEAK_24_I4,BASE_24_I4,
     +         SUM_24_R4,PEAK_24_R4,BASE_24_R4,
     +         GET_WH_MONTH_CAPACITY,
     +         GET_WH_MONTH_ENERGY,
     +         FUEL_AND_PURCHASE_COST_CAP,
     +         SYS_EMERGENCY_MW_FLOOR,
     +         SYS_EMERGENCY_COST_CAP
!
!
! CUSTOMER ANALYST DECLARATIONS
!
      LOGICAL*1   WRITE_MONTHLY_COAL_DEMAND_CLASS_SUMMARY,
     +            INIT_ANN_ALLOC_BLOCKS_2_CUST,
     +            INDIANA_FAC,
     +            RESOURCE_TO_LOAD_ALLOC,
     +            YES_RESOURCE_TO_LOAD_ALLOC,
     +            CALENDAR_CORRECT/.FALSE./,YES_CALANDER_CORRECT,
     +            APPLY_LEAP_YEAR_LOGIC,ADJUST_FOR_LEAP_YEAR/.FALSE./,
     +            PUT_MONTHLY_EP_COST_REV,
     +            COAL_ONLY,COAL_MODEL_ONLY,
     +            YES_REGION_POWER_MAP,REGION_OR_STATE_POWER_MAP 
      INTEGER*2 MAX_CUST_CLASS_GROUPS/0/,CG,MAX_CLASS_GROUPS/2048/,
     +          MAX_ASSET_CLASS_GROUPS/0/,AC,MAX_ASSET_GROUPS/2048/,
     +          MAX_CUST2_CLASS_GROUPS/0/,CG2,MAX_CLASS2_GROUPS/2048/,
     +          HG
!
! DETAILED REPORT OVERHEAD
!
      LOGICAL*1   COAL_DEMAND_CLASS_REPORT_NOT_OPEN/.TRUE./,
     +            TRANS_WH_REPORT_NOT_OPEN/.TRUE./,
     +            COAL_MARKET_AREA_REPORT_NOT_OPEN/.TRUE./,
     +            LOAD_BY_BLOCK_RPT_NOT_OPEN/.TRUE./,
     +            HOURLY_CUST_MARGIN_NOT_OPEN/.TRUE./,
     +            TRANS_INTR_NOT_OPEN/.TRUE./,
     +            IPL_FAC_RPT_NOT_OPEN/.TRUE./,
     +            DAILY_COAL_DEMAND_REPORT,
     +            COAL_MARKET_AREA_REPORT,
     +            R_BY_UNIT,IPL_FAC_REPORT,YES_IPL_FAC_REPORT,
     +            YES_MONTHLY_CLASS_REPORTS,
     +            MONTHLY_COAL_DEMAND_REPORT,
     +            LDG_OR_LDE
!
      INTEGER*2   LAST_SEASON/0/,
     +            TRANS_CLASS_SUM_UNIT,MONTHLY_COAL_DEMAND_HEADER,
     +            VARIABLE_NUMBER,
     +            CURRENT_MONTH,LEAP_YEAR_DAY_SHIFT,
     +            ANNUAL_COUNTER/0/,PRODUCTION_PERIODS,SAVE_HOURS/0/,
     +            TRANS_WH_UNIT/0/,WH_HOURLY_HEADER,
     +            COAL_MARKET_AREA_UNIT/0/,COAL_MARKET_AREA_RPT_HEADER,
     +            COAL_MARKET_TRANS_UNIT/0/,
     +            COAL_MARKET_TRANS_RPT_HEADER,
     +            LOAD_BY_BLOCK_NO/0/,MON_LOAD_BY_BLOCK_HEADER,
     +            HOURLY_CUST_MARGIN_NO/0/,HOURLY_CUST_MARGIN_HEADER,
     +            TRANS_INTR_NO/0/,TRANS_INTR_RPT_HEADER, 
     +            HOURLY_CUST_MARGIN_VAR,
     +            IPL_FAC_NO/0/,
     +            LOAD_BLOCK_VAR,IPL_FAC_HEADER,IPL_FAC_VAR,
     +            COAL_ID,R_DA
      INTEGER TRANS_CLASS_SUM_REC,TRANS_WH_REC,COAL_MARKET_AREA_REC,
     +         COAL_MARKET_TRANS_REC,
     +         LOAD_BLOCK_REC,IPL_FAC_REC,HOURLY_CUST_MARGIN_REC,
     +         TRANS_INTR_REC
      REAL*4   IPL_FAC_DB(15),ANN_IPL_FAC_DB(15),
     +         ELECT_PLAN_DATABASE(0:12,3),
     +         R_ELECT_PLAN_FUEL(0:12),
     +         R_ELECT_PLAN_PURCHASE(0:12),
     +         GET_DAILY_COAL_DEMAND_BY_NODE,
     +         GET_DAILY_COAL_DEMAND_BY_GS,
     +         R_INTERRUPTIBLE
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
!

! END DATA DECLARATIONS
!
!     CALLED FROM INSIDE PROCOST, BEFORE PERIOD LOOP
!
         MANAGE_COAL_DEMAND_FORECASTS = .TRUE.
      RETURN

      ENTRY ANNUAL_COAL_DEMAND(R_YEAR)


         CALL DOES_COAL_DEMAND_FILE_EXIST(COAL_DEMAND_FILE_EXISTS)
         SAVE_COAL_DEMAND_FILE_EXISTS = COAL_DEMAND_FILE_EXISTS
         IF(.NOT. COAL_DEMAND_FILE_EXISTS) RETURN 
         IF(R_YEAR >= EXTENSION_PERIOD_START()) RETURN !FORECASTS DO NOT CHANGE
         YES_RUN_COAL_MODEL = RUN_COAL_MODEL()
         COAL_MARKET_AREA_REPORT = DAILY_COAL_DEMAND_REPORT()
         USE_MARKET_AREA_REPORT_ID = YES_USE_MARKET_AREA_REPORT_ID()
!
         COAL_ONLY = COAL_MODEL_ONLY()
         YES_REGION_POWER_MAP = REGION_OR_STATE_POWER_MAP() 
!
         ZONAL_LEVEL_MARKET = YES_ZONAL_LEVEL_MARKET()
!
         YES_ASSET_ANALYST_ONLY = ASSET_ANALYST_ONLY()
!
! 10/13/04. FOR BURESH
!         
         MID_TERM_PEAK_UNCER = YES_MID_TERM_PEAK_UNCER()
!         
         IF(.NOT. YES_RUN_COAL_MODEL .AND. .NOT.
     +                                    YES_ASSET_ANALYST_ONLY) RETURN
!
         LOCAL_YEAR = R_YEAR + get_BASE_YEAR()
!         
         CALL COAL_DEMAND_FILE_USED_THIS_ENDPOINT(COAL_DEMAND_FILE_USED)
!         
!         IF(R_YEAR == 1) THEN
         IF(.NOT. COAL_DEMAND_FILE_USED) THEN
!
            CALL GET_COAL_DEMAND_TABLES(MAX_TRANS_LOAD_TABLES)
!            
!            
!            CLOSE(951,IOSTAT=IOS)
!            
         ENDIF
!
         CALL RETURN_COAL_DEMAND_OL(COAL_DEMAND_OL,TSYFRC_REC)
!
! 500 FOR NORTH AMERICAN DATA BASE
!
         MAX_TRANS_GROUPS_FROM_TG = MAX(1,GET_MAX_COAL_GROUP_NUMBER()) ! 10/03/02.
!
         CALL RETURN_COAL_DEMAND_GROUP_INFO( NUM_OF_TRANS_CLASSES,
     +                              MAX_TRANS_CLASS_NUM,
     +                              NUM_OF_ASSET_CLASSES,
     +                              MAX_ASSET_CLASS_NUM,
     +                              NUM_OF_CUST_CLASSES,
     +                              MAX_CUST_CLASS_NUM)
         MAX_TRANS_GROUPS = MAX(MAX_TRANS_CLASS_NUM,
     +                                         MAX_TRANS_GROUPS_FROM_TG)
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
     +                                           "Maintenance Penalty ")
            ENDDO
         ENDIF
! 8/30/01
         PRICE_ONLY_WHOLESALE_REV = APPLY_TRANS_REV_TO_WHOLESALE()
         IGNORE_NAT_PUR_USE_W = IGNORE_NATIVE_PURCHASE_USE_W()
!     
         FILE_NAME = trim(OUTPUT_DIRECTORY())//
     +                                       COAL_DEMAND_OL//"CSYFC.BIN"
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
            ALLOCATE(iPort(MAX_TRANS_LOAD_TABLES),
     +               SNamePos(MAX_TRANS_LOAD_TABLES))
            ALLOCATE(CUSTOMER_GROUP_2(MAX_TRANS_LOAD_TABLES))
            ALLOCATE(
     +            SPECIAL_CUSTOMER_TYPE(MAX_TRANS_LOAD_TABLES),
     +            INTERRUPTIBLE_MARKUP_PERENT(MAX_TRANS_LOAD_TABLES),
     +            INTERRUPTIBLE_MARKUP_CAP(MAX_TRANS_LOAD_TABLES),
     +            INTERRUPTIBLE_MAX_CAPACITY(MAX_TRANS_LOAD_TABLES),
     +            INTERRUPTIBLE_MIN_CAPACITY(MAX_TRANS_LOAD_TABLES),
     +            INTERRUPTIBLE_MARKUP_ADDER(MAX_TRANS_LOAD_TABLES),
     +            INTERRUPTIBLE_PERCENT(MAX_TRANS_LOAD_TABLES),
     +            HOURLY_INTERRUPIBLE_REVENUE(
     +                                      MAX_TRANS_LOAD_TABLES,24,4))
            ALLOCATE(COAL_GROUP_ID(MAX_TRANS_LOAD_TABLES)) ! INT2
            ALLOCATE(REFERENCE_LOAD_NUMBER(MAX_TRANS_LOAD_TABLES)) ! INT2
            ALLOCATE(MARKET_ENERGY_PRICE(MAX_TRANS_LOAD_TABLES)) ! REAL4
            ALLOCATE(ANNUAL_TG_PEAK(0:12,MAX_TRANS_GROUPS))
            ALLOCATE(ANNUAL_TG_VOLUME(0:MAX_TRANS_GROUPS))
            ALLOCATE(MONTHLY_ENERGY_PRICE_PATTERN(
     +                                           MAX_TRANS_LOAD_TABLES))  ! REAL4
            ALLOCATE(MARKET_DEMAND_PRICE(MAX_TRANS_LOAD_TABLES)) ! REAL4
            ALLOCATE(MONTHLY_DEMAND_PRICE_PATTERN(
     +                                           MAX_TRANS_LOAD_TABLES)) ! REAL4
            ALLOCATE(MARKET_CUSTOMER_PRICE(MAX_TRANS_LOAD_TABLES)) ! REAL4
            ALLOCATE(ASSET_CLASS_ID(MAX_TRANS_LOAD_TABLES)) ! REAL4
            ALLOCATE(ASSET_CLASS_REV_ALLOC_VECTOR(
     +                                           MAX_TRANS_LOAD_TABLES))  ! REAL4
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
            ALLOCATE(CUSTOMER_CLASS_NAME(MAX_TRANS_LOAD_TABLES)) ! CHAR*30
            ALLOCATE(CALCULATION_MODE(MAX_TRANS_LOAD_TABLES)) ! CHAR*1
            ALLOCATE(REFERENCE_LOAD_NAME(MAX_TRANS_LOAD_TABLES)) ! CHAR*5
            ALLOCATE(TRANS_MONTHLY_ENERGY(12,MAX_TRANS_LOAD_TABLES))
            ALLOCATE(DEMAND_GROUP_AVAILABLE(MAX_TRANS_GROUPS))
            ALLOCATE(WH_TRANS_MONTHLY_ENERGY(12,
     +                                        MAX_TRANS_GROUPS_FROM_TG))
            ALLOCATE(WH_TRANS_MONTHLY_CAPACITY(12,
     +                                        MAX_TRANS_GROUPS_FROM_TG))
            ALLOCATE(TRANS_MONTHLY_PEAK(12,MAX_TRANS_LOAD_TABLES))
            ALLOCATE(TRANS_MONTHLY_CUSTOMERS(12,MAX_TRANS_LOAD_TABLES))
            ALLOCATE(TABLE_ACTIVE(MAX_TRANS_LOAD_TABLES))
            ALLOCATE(BASECASE_MARKET_AREA_ID(MAX_TRANS_LOAD_TABLES))
            ALLOCATE(WD_INDEX(MAX_TRANS_LOAD_TABLES))
            ALLOCATE(GW_INDEX(MAX_TRANS_LOAD_TABLES))
            ALLOCATE(BASECASE_TRANS_AREA_ID(MAX_TRANS_LOAD_TABLES))
            ALLOCATE(BASECASE_NERC_SUB_ID(MAX_TRANS_LOAD_TABLES))
            ALLOCATE(MONTHLY_UNITS(MAX_TRANS_LOAD_TABLES))
            ALLOCATE(PRICE_INDEX_ACTIVE(MAX_TRANS_LOAD_TABLES))
            ALLOCATE(THREE_FACTOR_TRANSFORM(MAX_TRANS_LOAD_TABLES))
            ALLOCATE(JURISDICTIONAL_CUSTOMER(MAX_TRANS_LOAD_TABLES))
            ALLOCATE(FUEL_COST_RECOVERY_THROUGH_FAC(
     +                                           MAX_TRANS_LOAD_TABLES))
            ALLOCATE(BASE_COST_OF_FAC_FUEL(MAX_TRANS_LOAD_TABLES))
            ALLOCATE(MINIMUM_MARKET_PRICE(MAX_TRANS_LOAD_TABLES))
            ALLOCATE(MAXIMUM_MARKET_PRICE(MAX_TRANS_LOAD_TABLES))
            ALLOCATE(INDEXED_ENERGY_PRICE(MAX_TRANS_LOAD_TABLES))
            ALLOCATE(TG_COUNTER(MAX_TRANS_GROUPS))
            ALLOCATE(LOAD_DISPATCH_POSITION(MAX_TRANS_LOAD_TABLES,
     +                                                MAX_TRANS_GROUPS))
            ALLOCATE(LOAD_DISPATCH_INDEX(MAX_TRANS_LOAD_TABLES,
     +                                                MAX_TRANS_GROUPS))
!
            ALLOCATE(COAL_LOAD_GROUPS_INDEX(MAX_TRANS_GROUPS))
            ALLOCATE(CUST_CLASS_GROUPS_INDEX(0:MAX_CLASS_GROUPS))
            ALLOCATE(CUST2_CLASS_GROUPS_INDEX(0:MAX_CLASS2_GROUPS))
            ALLOCATE(ASSET_CLASS_GROUPS_INDEX(0:MAX_ASSET_GROUPS))
            ALLOCATE(ASSET_2_TRANS_INDEX(0:MAX_ASSET_GROUPS,
     +                                                MAX_TRANS_GROUPS))
            ALLOCATE(NUMBER_ASSET_2_TRANS(MAX_TRANS_GROUPS))
            ALLOCATE(ASSET_TRANSACTION_CROSS_INDEX(0:MAX_ASSET_GROUPS,
     +                                                MAX_TRANS_GROUPS))
            ALLOCATE(NUMBER_TRANS_PER_AC_TG(0:MAX_ASSET_GROUPS,
     +                                                MAX_TRANS_GROUPS))
            ALLOCATE(TRANS_WITHIN_AC_TG(0:MAX_ASSET_GROUPS,
     +                          MAX_TRANS_GROUPS,MAX_TRANS_LOAD_TABLES))
            ALLOCATE(FIRST_AC_TG(0:MAX_ASSET_GROUPS))
            ALLOCATE(FIRST_TABLE_TG(0:MAX_TRANS_GROUPS))
            ALLOCATE(COAL_LOAD_2_TRANS_GROUPS(MAX_TRANS_GROUPS))
            ALLOCATE(COAL_LOAD_GROUP_2_TG(MAX_TRANS_GROUPS))
            ALLOCATE(CUST_CLASS_GROUP_2_CG(MAX_CLASS_GROUPS+1))
            ALLOCATE(CUST2_CLASS_GROUP_2_CG(MAX_CLASS_GROUPS+1))
            ALLOCATE(ASSET_CLASS_GROUP_2_AC(0:MAX_ASSET_GROUPS+1))
            ALLOCATE(LAST_TABLE_FOR_TG(MAX_TRANS_GROUPS))
            ALLOCATE(LAST_TABLE_FOR_CG(MAX_CLASS_GROUPS+1))
            ALLOCATE(LAST_TABLE_FOR_CG2(MAX_CLASS_GROUPS+1))
            ALLOCATE(TG_CG_DATABASE(0:12,0:MAX_TRANS_GROUPS,
     +                                            0:MAX_CLASS_GROUPS,3))
            ALLOCATE(TG_CG2_DATABASE(0:12,0:MAX_TRANS_GROUPS,
     +                                            0:MAX_CLASS_GROUPS,3))
            ALLOCATE(TABLE_DAY_SHIFT(MAX_TRANS_LOAD_TABLES))
            ALLOCATE(COAL_DEMAND_PLANNING_PEAK(0:MAX_TRANS_GROUPS,12))
            ALLOCATE(PA_PLANNING_PEAK(0:NUMBER_OF_ACTIVE_GROUPS,12))
            ALLOCATE(GLOBAL_PA_PEAK(0:NUMBER_OF_ACTIVE_GROUPS))
            ALLOCATE(GLOBAL_PA_PEAK_MONTH(0:NUMBER_OF_ACTIVE_GROUPS))
            ALLOCATE(TG_2_PLANNING_AREA(MAX_TRANS_GROUPS_FROM_TG))
            ALLOCATE(REV_CLASS_INDEX(0:MAX_TRANS_LOAD_TABLES,4))
            ALLOCATE(ENERGY_LOSS_MULT(MAX_TRANS_LOAD_TABLES))
            ALLOCATE(REF_LEAP_YEAR_DAY_SHIFT(MAX_TRANS_LOAD_TABLES))
            ALLOCATE(DEMAND_PRICING_METHOD(MAX_TRANS_LOAD_TABLES))
            ALLOCATE(MONTHLY_TABLE_PEAK_SALES(MAX_TRANS_LOAD_TABLES,12))
            ALLOCATE(INTRA_COMPANY_TRANSACTION(MAX_TRANS_LOAD_TABLES))
            ALLOCATE(INTRA_ASSET_CLASS_ID(MAX_TRANS_LOAD_TABLES))
            ALLOCATE(INTRA_ASSET_CLASS_ALLOCATION(
     +                                           MAX_TRANS_LOAD_TABLES))
            ALLOCATE(INTRA_ACCOUNT_CLASSIFICATION(
     +                                           MAX_TRANS_LOAD_TABLES))
            ALLOCATE(INTRA_EXPENSE_COLLECTION(MAX_TRANS_LOAD_TABLES))
         ENDIF
!
         WH_TRANS_MONTHLY_ENERGY = 0.
         MONTHLY_TABLE_PEAK_SALES = 0.
         WH_TRANS_MONTHLY_CAPACITY = 0.
         COAL_DEMAND_PLANNING_PEAK = 0.
!
         DEMAND_GROUP_AVAILABLE = .FALSE.
!         
! ALLOWS FOR REGIONAL RESERVE PLANNING: E.G. BC, ALBERTA IN THE WSCC
!
         PA_PLANNING_PEAK = 0.
         GLOBAL_PA_PEAK = 0.
         GLOBAL_PA_PEAK_MONTH = 0
         TG_2_PLANNING_AREA = 0
!
         ENERGY_LOSS_MULT = 1.
         REF_LEAP_YEAR_DAY_SHIFT = 0
!
! WD_INDEX 5/13/01.
         WD_INDEX = -1
         CALL DOES_WEATHER_DEMAND_FILE_EXIST(WD_FILE_EXISTS)
         GW_INDEX = -1
         CALL DOES_COAL_WEATHER_DEMAND_FILE_EXIST(GW_FILE_EXISTS)
! WH_INDEX 6/11/01.
         CALL DOES_WEEKLY_HYDRO_FILE_EXIST(WH_FILE_EXISTS)
!
         COAL_LOAD_GROUPS_INDEX = 0
         CUST_CLASS_GROUPS_INDEX = 0
         CUST2_CLASS_GROUPS_INDEX = 0
         ASSET_CLASS_GROUPS_INDEX = 0
!
         ASSET_2_TRANS_INDEX = 0 ! WHICH ASSET CLASSES BELONG TO A TRANSACTION GROUP
         NUMBER_ASSET_2_TRANS = 0 ! HOW MANY ASSET CLASSES BELONG TO THE TRANSACTION GROUP
         ASSET_TRANSACTION_CROSS_INDEX = 0
! 9/30/98. GAT.
         NUMBER_TRANS_PER_AC_TG = 0
         TRANS_WITHIN_AC_TG = 0
!
         FIRST_AC_TG = 0
         FIRST_TABLE_TG = 0
         COAL_LOAD_2_TRANS_GROUPS = 0
         COAL_LOAD_GROUP_2_TG = 0.
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
     +                         DEALLOCATE(MONTHLY_INTERRUPTIBLE_REVENUE)
         ALLOCATE(MONTHLY_INTERRUPTIBLE_REVENUE(
     +                                       0:MAX_ASSET_GROUPS+1,0:12))
         MONTHLY_INTERRUPTIBLE_REVENUE = 0.
!
         CALENDAR_CORRECT = YES_CALANDER_CORRECT()
!
!         CALL GET_DAY_OF_WEEK_IN_MONTH(INT2(1),INT2(1),
!     +                                             CALANDER_DAY_OF_WEEK)
         CALL DAYWEEK(INT2(1),INT2(2),LOCAL_YEAR,CALENDAR_DAY_OF_WEEK)
!         CALL DAYWEEK(INT2(1),INT2(1),LOCAL_YEAR,CALENDAR_DAY_OF_WEEK)
!
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
         MAX_COAL_LOAD_GROUPS = 0
         MAX_CUST_CLASS_GROUPS = 0
         MAX_CUST2_CLASS_GROUPS = 0
         MAX_ASSET_CLASS_GROUPS = 0
!
         MAX_COAL_GROUP_NUMBER = 0
         MAX_CUST_GROUP_NUMBER = 0
         MAX_CUST2_GROUP_NUMBER = 0
!
         NUMBER_OF_HYDRO_GROUPS = GET_NUMBER_OF_HYDRO_GROUPS()
!
         SCENARIO_ANNUAL_MULTIPLIER = 1.0
!
         INTERRUPTIBLE_PRICING = .FALSE.
!
         IF(.NOT. ALLOCATED( LAST_THIS_YR_ENERGY) ) THEN
            ALLOCATE(LAST_THIS_YR_ENERGY(2,12,0:MAX_TRANS_LOAD_TABLES)) ! 1=LAST_YEAR, 2=THIS YEAR
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
!            COAL_DEMAND_PLANNING_PEAK(I) = 0.0
         ENDDO
!
         iPort = 0
         SNamePos = 0
         CUSTOMER_CLASS_NAME = "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ"
         HIGHEST_TG_COUNT = 0
         ANNUAL_TG_PEAK = 0
         ANNUAL_TG_VOLUME = 0
!
         DO TRANS = 1, MAX_TRANS_LOAD_TABLES
            READ(951,REC=TRANS_YEAR_RECORD) DELETE,
     +                     TEMP_YEAR, ! INT2
     +                     CUSTOMER_GROUP(TRANS), ! INT2 
     +                     CUSTOMER_CLASS_NAME(TRANS), ! CHAR*30
     +                     CALCULATION_MODE(TRANS), ! CHAR*1
     +                     COAL_GROUP_ID(TRANS), ! INT2
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
     +                     (MONTHLY_ENERGY(I,TRANS),I=1,12), ! (12) REAL4
     +                     (MONTHLY_PEAK(I,TRANS),I=1,12), ! (12) REAL4
     +                     (MONTHLY_CUSTOMERS(I,TRANS),I=1,12), ! (12) REAL4
     +                     DIST_ENERGY_LOSS_FACTOR(TRANS), ! REAL4
     +                     TRANS_ENERGY_LOSS_FACTOR(TRANS), ! REAL4
     +                     PEAK_LOSS_FACTOR(TRANS), ! REAL4
     +                     PEAK_COIN_FACTOR(TRANS), ! REAL4
     +                     DISTRIBUTION_PRICE(TRANS), ! REAL4
     +                     TRANSMISSION_PRICE(TRANS), !REAL4
     +                     TABLE_ACTIVE(TRANS),
     +                     BASECASE_MARKET_AREA_ID(TRANS), 
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
     +                     SPECIAL_CUSTOMER_TYPE(TRANS),
     +                     INTERRUPTIBLE_MARKUP_PERENT(TRANS),
     +                     INTERRUPTIBLE_MARKUP_CAP(TRANS),
     +                     INTERRUPTIBLE_MAX_CAPACITY(TRANS),
     +                     INTERRUPTIBLE_MIN_CAPACITY(TRANS),
     +                     INTERRUPTIBLE_MARKUP_ADDER(TRANS),
     +                     INTERRUPTIBLE_PERCENT(TRANS)
!     
            TRANS_YEAR_RECORD = TRANS_YEAR_RECORD + 30
!
! 10/03/02. FOR REGIONAL CONSOLIDATION. NOTE REASSIGNMENT.
! 05/07/03. ALTERED.
!
!
! 081306. ADDED THREE DIFFERENT SWITCHES FROM THE INTERRUPTIBLE_ACTIVE SWITCH.
!
            IF(SPECIAL_CUSTOMER_TYPE(TRANS) == 'I') THEN
               INTERRUPTIBLE_PRICING = .TRUE.
            ELSEIF(SPECIAL_CUSTOMER_TYPE(TRANS) == 'P') THEN
               IF(.NOT. COAL_ONLY) CYCLE
            ELSEIF(SPECIAL_CUSTOMER_TYPE(TRANS) == 'S') THEN
            ENDIF
!
!
! 10/15/04.
!
            IF(ZONAL_LEVEL_MARKET) THEN
               MARKET_ID = BASECASE_MARKET_AREA_ID(TRANS)
               COAL_GROUP_ID(TRANS) = 
     +                                     MARKET_AREA_LOOKUP(MARKET_ID)
            ENDIF
!
            IF( .NOT. YES_ASSET_ANALYST_ONLY) THEN
               COAL_GROUP_ID(TRANS) = 
     +                GET_BELONGS_TO_COAL_GROUP(COAL_GROUP_ID(TRANS))
            ENDIF
!     
            IF(COAL_GROUP_ID(TRANS) == 0) CYCLE ! SO THAT IT ISN'T STOPPED BELOW
!
!            COAL_ID = COAL_GROUP_ID(TRANS)
            TG = COAL_GROUP_ID(TRANS)
            DEMAND_GROUP_AVAILABLE(TG) = .TRUE.
!
            IF(CUSTOMER_GROUP_2(TRANS) == -9999) 
     +                   CUSTOMER_GROUP_2(TRANS) = CUSTOMER_GROUP(TRANS)
!
!            TG = COAL_GROUP_ID(TRANS)
! 062404            
            TGP = GET_COAL_GROUP_POSITION(TG)
            CG = CUSTOMER_GROUP(TRANS)
            CG2 = CUSTOMER_GROUP_2(TRANS)
            AC = ASSET_CLASS_ID(TRANS)
!
            REV_CLASS_INDEX(TRANS,1) = 
     +                   INCOME_STATEMENT_POSITION(REVENUE_CLASS_ENERGY)
            REV_CLASS_INDEX(TRANS,2) = 
     +                   INCOME_STATEMENT_POSITION(REVENUE_CLASS_DEMAND)
            REV_CLASS_INDEX(TRANS,3) = 
     +                     INCOME_STATEMENT_POSITION(REVENUE_CLASS_CUST)
            REV_CLASS_INDEX(TRANS,4) = 
     +                   INCOME_STATEMENT_POSITION(REVENUE_INDEX_ENERGY)
!
            IF(PRICE_INDEX_ACTIVE(TRANS) == 'T') THEN
               USING_INDEXED_PRICING = .TRUE.
            ENDIF
!             
            WD_INDEX(TRANS) = 
     +           MARKET_AREA_LOOKUP(BASECASE_MARKET_AREA_ID(TRANS)(1:5))
            GW_INDEX(TRANS) = 
     +            MARKET_AREA_LOOKUP(
     +                              BASECASE_MARKET_AREA_ID(TRANS)(1:5))
! 9/28/98. GAT.
            IF(CALCULATION_MODE(TRANS) == 'S' .AND. 
     +                                  TABLE_ACTIVE(TRANS) == 'T') THEN
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
               WRITE(4,*) '*** line 1851 COAL_DEMAND_OBJT.FOR ***'
!               
               CYCLE

            ENDIF
            IF( (YES_RUN_COAL_MODEL .AND. 
     +               .NOT. COAL_GROUP_ACTIVE_SWITCH(TG)) .OR.
     +                                 TABLE_ACTIVE(TRANS) == 'F') CYCLE
!
            FILE_TABLE_2_TRANS_INDEX(FILE_TABLE_INDEX) = TRANS
!
            PEAK_COIN_FACTOR(TRANS) = PEAK_COIN_FACTOR(TRANS) / 100.
!
            INTERRUPTIBLE_PERCENT(TRANS) = 
     +                               INTERRUPTIBLE_PERCENT(TRANS) * 0.01 
!
            IF(COAL_LOAD_GROUPS_INDEX(TG) == 0) THEN
!               FIRST_TABLE_TG(GET_COAL_GROUP_POSITION(TG)) = TRANS
               MAX_COAL_LOAD_GROUPS = MAX_COAL_LOAD_GROUPS + 1
               FIRST_TABLE_TG(MAX_COAL_LOAD_GROUPS) = TRANS
               COAL_LOAD_GROUPS_INDEX(TG) = MAX_COAL_LOAD_GROUPS
               COAL_LOAD_2_TRANS_GROUPS(GET_COAL_GROUP_POSITION(TG)) = 
     +                                             MAX_COAL_LOAD_GROUPS
               COAL_LOAD_GROUP_2_TG(MAX_COAL_LOAD_GROUPS) = TG
!
               IF(.NOT. YES_ASSET_ANALYST_ONLY) THEN
                  TG_2_PLANNING_AREA(MAX_COAL_LOAD_GROUPS) = 
     +                      GET_PA_FROM_TG(GET_COAL_GROUP_POSITION(TG))
!
               ENDIF
               MAX_COAL_GROUP_NUMBER = MAX(MAX_COAL_GROUP_NUMBER,TG)
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
     +                                            MAX_ASSET_CLASS_GROUPS
                  ASSET_CLASS_GROUP_2_AC(MAX_ASSET_CLASS_GROUPS)=ITRA_ID
!                 FIRST_AC_TG(AC) = TG
               ENDIF
            ENDIF
!            
! THIS IS USED TO EFFICIENTLY GROUP CLASSES INTO TRANSACTION GROUPS, CUSTOMER GROUPS
!
            LAST_TABLE_FOR_TG(COAL_LOAD_GROUPS_INDEX(TG)) = TRANS
!
            LAST_TABLE_FOR_CG(CUST_CLASS_GROUPS_INDEX(CG)) = TRANS
            LAST_TABLE_FOR_CG2(CUST2_CLASS_GROUPS_INDEX(CG2)) = TRANS
!
! FOR USE BY MAINTENANCE PROGRAM
!
!            DO I = 1, 12
!               FUT_PEAK(3,I) = FUT_PEAK(3,I) + 
!     +                      MONTHLY_PEAK(I,TRANS)*MAINTENANCE_PENALTY(I)
!            ENDDO
!
!
!
! 9/28/98. GAT. MOVED THIS CODE FROM MONTHLY_TRANSACTION_LOADS
!
!            
!            
!!!! NOTE DOUBLE INDEX ON TG TO SAVE SPACE !!!
!
            TG = COAL_LOAD_GROUPS_INDEX(TG) 
!
            CG = CUST_CLASS_GROUPS_INDEX(CG) 
            CG2 = CUST2_CLASS_GROUPS_INDEX(CG2)
!
            AC = ASSET_CLASS_GROUPS_INDEX(AC)
!
            PA = TG_2_PLANNING_AREA(TG)
!
            SCENARIO_INDEX = GET_SCENARIO_INDEX(SCENARIO_VARIABLE)
!
! INDEXED ENERGY REVENUES BY TAB 
!
            NUMBER_TRANS_PER_AC_TG(AC,TG) = 
     +                                 NUMBER_TRANS_PER_AC_TG(AC,TG) + 1
            TEMP_I = NUMBER_TRANS_PER_AC_TG(AC,TG) 
            TRANS_WITHIN_AC_TG(AC,TG,TEMP_I) = TRANS ! POTENTIALLY BIG ARRAY
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
     +                                            TEMP_DISPATCH_POSITION
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
            IF(.NOT. YES_ASSET_ANALYST_ONLY) THEN
               CALL OPEN_COAL_HOURLY_LOAD_FILE(
     +                                     REFERENCE_LOAD_NAME(TRANS),
     +                                     REFERENCE_LOAD_NUMBER(TRANS),
     +                                     R_YEAR,LOAD_UNIT,LOCAL_MONTH,
     +                                     SCENARIO_INDEX,
     +                                     LDG_OR_LDE)
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
!           READ(LOAD_UNIT,REC=367) AEL_PEAK,AEL_PEAKMAX !replaced with above 4/18/01 for LF95
!           READ(LOAD_UNIT,REC=368) AEL_BASE,AEL_BASEMIN
!           READ(LOAD_UNIT,REC=369) AEL_ENERGY,AEL_ENRGYEAR
!
! DAY LOAD SHIFT LOGIC
!
!
!               READ(LOAD_UNIT,REC=START_REC) LDE_MONTH,LDE_DAY,LDE_YEAR,
!     +                               EEICODE,FIRST_DAY_OF_YEAR,
!     +                               TIMZON,TEMPER,
!     +                               DELTMP,DAILY_LOADS_I4
               READ(LOAD_UNIT,REC=START_REC+1) 
     +                               LDE_MONTH,LDE_DAY,LDE_YEAR,
     +                               EEICODE,FIRST_DAY_OF_YEAR,
     +                               TIMZON,TEMPER,
     +                               DELTMP,DAILY_LOADS_R4
!            IF(FIRST_DAY_OF_YEAR == CALENDAR_DAY_OF_WEEK) THEN 
!               TABLE_DAY_SHIFT(TRANS) = 0
!            ELSEIF (FIRST_DAY_OF_YEAR < CALENDAR_DAY_OF_WEEK) THEN
!
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
     +                                                 FIRST_DAY_OF_YEAR 
!     
!            ELSE
!            ENDIF
!
            IF(DIST_ENERGY_LOSS_FACTOR(TRANS) == 100. .OR.
     +               TRANS_ENERGY_LOSS_FACTOR(TRANS) == 100. .OR.
     +                             PEAK_LOSS_FACTOR(TRANS) == 100.) THEN
               WRITE(4,*) "LOSS FACTORS IN TRANSACTION FORECAST TABLE"
               WRITE(4,*) CUSTOMER_CLASS_NAME(TRANS), "EQUALS 100%"
               WRITE(4,*) "LOSSES IGNORED"
               ENERGY_LOSS_MULT(TRANS) = 1.0
               PEAK_LOSS_MULT = 1.0
            ELSE
               ENERGY_LOSS_MULT(TRANS) = 1./
     +               ((1.-DIST_ENERGY_LOSS_FACTOR(TRANS)/100.)*
     +                        (1.-TRANS_ENERGY_LOSS_FACTOR(TRANS)/100.))
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
     +                                           "Annual Load Multipli")
               ENDIF
               IF(SCENARIO_ANNUAL_MULTIPLIER >= 0.) THEN
                  TABLE_SCENARIO_MULTIPLIER = SCENARIO_ANNUAL_MULTIPLIER
               ELSE
                  TABLE_SCENARIO_MULTIPLIER = 
     +                  GET_VAR(SCENARIO_ANNUAL_MULTIPLIER,LOCAL_MONTH,
     +                                           "Scenario Multiplier ")
               ENDIF
!
!
               IF(CG < 0 .OR. CG > MAX(1,MAX_CLASS_GROUPS)) THEN
                  WRITE(4,*) "INVALID INDEXING OF CUSTOMER CLASSES IN"
                  WRITE(4,*) 
     +                     "THE TRANSACT FORECAST FILE FOR YEAR ",
     +                     LOCAL_YEAR
                  WRITE(4,*) "AND SEASON ",LOCAL_MONTH
                  WRITE(4,*) "AND TABLE ",TRANS
                  WRITE(4,*) "FATAL ERROR"

                  WRITE(4,*) '*** line 2019 COAL_DEMAND_OBJT.FOR ***'
                  er_message='See WARNING MESSAGES-coal-obj-10'
                  call end_program(er_message)
               ENDIF

               IF(    CALCULATION_MODE(TRANS) == 'R') THEN ! REFERENCE
!! NO TRANSFORM REQUIRED
! 112008. Divided by 1000 per LDG file format.
                  LAST_THIS_YR_ENERGY(2,LOCAL_MONTH,TRANS) = 
     +                                   0.001 * AEL_ENERGY(LOCAL_MONTH)
                  LAST_THIS_YR_PEAK(2,LOCAL_MONTH,TRANS) = 
     +                                   0.001 * AEL_PEAK(LOCAL_MONTH)
               ELSEIF(CALCULATION_MODE(TRANS) == 'M') THEN ! MONTHLY
! USE PEAK AND ENERGY FROM MONTHLY INPUTS
                  LAST_THIS_YR_ENERGY(2,LOCAL_MONTH,TRANS) = 
     +                                 MONTHLY_ENERGY(LOCAL_MONTH,TRANS)
! 071306. 
                  LAST_THIS_YR_PEAK(2,LOCAL_MONTH,TRANS) = 
     +                               MONTHLY_PEAK(LOCAL_MONTH,TRANS)/24.
               ELSEIF(CALCULATION_MODE(TRANS) == 'G') THEN ! ANNUAL
!! GROW THE MONTHLY PEAK AND ENERGY FROM PREVIOUS YEAR VALUES
                  IF(R_YEAR > 1) THEN
                     LAST_THIS_YR_ENERGY(2,LOCAL_MONTH,TRANS) = 
     +                     LAST_THIS_YR_ENERGY(1,LOCAL_MONTH,TRANS) *
     +                     (1. + MONTHLY_ENERGY(LOCAL_MONTH,TRANS)/100.)
                     LAST_THIS_YR_PEAK(2,LOCAL_MONTH,TRANS) =
     +                     LAST_THIS_YR_PEAK(1,LOCAL_MONTH,TRANS) *
     +                       (1. + MONTHLY_PEAK(LOCAL_MONTH,TRANS)/100.)
                  ELSE
                     WRITE(4,*) "GROWTH MODE FOR"
                     WRITE(4,*) CUSTOMER_CLASS_NAME(TRANS)
                     WRITE(4,*) REFERENCE_LOAD_NAME(TRANS)," IN YEAR"
                     WRITE(4,*) R_YEAR," IS OUTSIDE OF RANGE"
                     WRITE(4,*) "YOU NEED TO HAVE AN INITIAL YEAR."
                     WRITE(4,*) "SEE YOUR TRANSACTION LOADS FILE"
                     WRITE(4,*) '*** line 2066 COAL_DEMAND_OBJT.FOR ***'
                     er_message='See WARNING MESSAGES-coal-obj-9'
                     call end_program(er_message)
                  ENDIF
               ELSEIF(CALCULATION_MODE(TRANS) == 'A') THEN ! GROWTH
! ALLOCATE ANNUAL TO MONTHS
                  IF(AEL_PEAKMAX > 0. AND. AEL_ENRGYEAR > 0.) THEN
                     LAST_THIS_YR_ENERGY(2,LOCAL_MONTH,TRANS) =             
     +                  ANNUAL_ENERGY(TRANS) * AEL_ENERGY(LOCAL_MONTH) /
     +                                                      AEL_ENRGYEAR
                     LAST_THIS_YR_PEAK(2,LOCAL_MONTH,TRANS) = 
     +                     ANNUAL_PEAK(TRANS) * AEL_PEAK(LOCAL_MONTH) /
     +                                                       AEL_PEAKMAX
                  ELSE                                         
                     WRITE(4,*) "HISTORIC PEAK AND ENERGY FOR"
                     WRITE(4,*) CUSTOMER_CLASS_NAME(TRANS)
                     WRITE(4,*) REFERENCE_LOAD_NAME(TRANS)," IN YEAR"
                     WRITE(4,*) LOCAL_YEAR," IS OUTSIDE OF RANGE"
                     WRITE(4,*) "SEE YOUR TRANSACTION LOADS FILE"
                     WRITE(4,*) '*** line 2085 COAL_DEMAND_OBJT.FOR ***'
                     er_message='See WARNING MESSAGES-coal-obj-9'
                     call end_program(er_message)
                  ENDIF
               ELSE
                  WRITE(4,*) "UNKNOWN TRANSACTION FILE GROWTH MODE FOR"
                  WRITE(4,*) CUSTOMER_CLASS_NAME(TRANS)," IN YEAR"
                  WRITE(4,*) LOCAL_YEAR
                  WRITE(4,*) "SEE YOUR TRANSACTION LOADS FILE"
                  WRITE(4,*) '*** line 2094 COAL_DEMAND_OBJT.FOR ***'
                  er_message='See WARNING MESSAGES-coal-obj-8'
                  call end_program(er_message)
               ENDIF
! THIS YEAR'S DATA BECOMES LAST YEAR'S DATA NEXT YEAR. GOT IT?
! 5/28/99. GAT. Moooved.
               LAST_THIS_YR_ENERGY(1,LOCAL_MONTH,TRANS) =
     +                        LAST_THIS_YR_ENERGY(2,LOCAL_MONTH,TRANS)
!     +                                        * ANNUAL_MULTIPLIER(TRANS) 
               LAST_THIS_YR_ENERGY(1,LOCAL_MONTH,0) =
     +                        LAST_THIS_YR_ENERGY(2,LOCAL_MONTH,0)
!     +                                        * ANNUAL_MULTIPLIER(TRANS) 
               LAST_THIS_YR_PEAK(1,LOCAL_MONTH,TRANS) =
     +                        LAST_THIS_YR_PEAK(2,LOCAL_MONTH,TRANS)
!     +                                        * ANNUAL_MULTIPLIER(TRANS) 
               LAST_THIS_YR_PEAK(1,LOCAL_MONTH,0) =
     +                        LAST_THIS_YR_PEAK(2,LOCAL_MONTH,0)
!     +                                        * ANNUAL_MULTIPLIER(TRANS) 
!
! 10/31/02. FOR FE ENTERPRISE RISK MANANGEMENT
!
               TG_CG_DATABASE(LOCAL_MONTH,TG,CG,1) = 
     +               TG_CG_DATABASE(LOCAL_MONTH,TG,CG,1) +
     +                     LAST_THIS_YR_ENERGY(2,LOCAL_MONTH,TRANS) *
     +                                        TABLE_ANNUAL_MULTIPLIER *
     +                                                    MONTHLY_MULT *
     +                                      TABLE_SCENARIO_MULTIPLIER *
     +                GET_SCENARIO_ELECTRIC_DEMAND(R_YEAR,LOCAL_MONTH) *
     +                GET_SCENARIO_COAL_DEMAND(R_YEAR,LOCAL_MONTH) *
     +               GET_PARAM_ELECTRIC_DEMAND(R_YEAR,LOCAL_MONTH,TGP) *   
     +                         GET_SCENARIO_ENERGY(R_YEAR,LOCAL_MONTH) * 
     +                         GET_PARAM_ENERGY(R_YEAR,LOCAL_MONTH,TGP)
               TG_CG_DATABASE(LOCAL_MONTH,TG,CG,2) = 
     +               TG_CG_DATABASE(LOCAL_MONTH,TG,CG,2) +
     +                     LAST_THIS_YR_PEAK(2,LOCAL_MONTH,TRANS) *
     +                                        TABLE_ANNUAL_MULTIPLIER *
     +                                                    MONTHLY_MULT *
     +                                      TABLE_SCENARIO_MULTIPLIER *
     +                GET_SCENARIO_ELECTRIC_DEMAND(R_YEAR,LOCAL_MONTH) *
     +                GET_SCENARIO_COAL_DEMAND(R_YEAR,LOCAL_MONTH) *
     +               GET_PARAM_ELECTRIC_DEMAND(R_YEAR,LOCAL_MONTH,TGP) *   
     +                           GET_SCENARIO_PEAK(R_YEAR,LOCAL_MONTH) * 
     +                          GET_PARAM_PEAK(R_YEAR,LOCAL_MONTH,TGP) *
     +                                                  PEAK_LOSS_MULT   
               TG_CG_DATABASE(LOCAL_MONTH,TG,CG,3) = 
     +               TG_CG_DATABASE(LOCAL_MONTH,TG,CG,3) +
     +                              MONTHLY_CUSTOMERS(LOCAL_MONTH,TRANS)
!
!
! 02/12/04. FOR WVPA MONTHLY MIDAS
!
               TG_CG2_DATABASE(LOCAL_MONTH,TG,CG2,1) = 
     +               TG_CG2_DATABASE(LOCAL_MONTH,TG,CG2,1) +
     +                     LAST_THIS_YR_ENERGY(2,LOCAL_MONTH,TRANS) *
     +                                        TABLE_ANNUAL_MULTIPLIER *
     +                                                    MONTHLY_MULT *
     +                                      TABLE_SCENARIO_MULTIPLIER *
     +                GET_SCENARIO_ELECTRIC_DEMAND(R_YEAR,LOCAL_MONTH) *
     +                GET_SCENARIO_COAL_DEMAND(R_YEAR,LOCAL_MONTH) *
     +               GET_PARAM_ELECTRIC_DEMAND(R_YEAR,LOCAL_MONTH,TGP) *   
     +                         GET_SCENARIO_ENERGY(R_YEAR,LOCAL_MONTH) * 
     +                         GET_PARAM_ENERGY(R_YEAR,LOCAL_MONTH,TGP)
               TG_CG2_DATABASE(LOCAL_MONTH,TG,CG2,2) = 
     +               TG_CG2_DATABASE(LOCAL_MONTH,TG,CG2,2) +
     +                     LAST_THIS_YR_PEAK(2,LOCAL_MONTH,TRANS) *
     +                                        TABLE_ANNUAL_MULTIPLIER *
     +                                                    MONTHLY_MULT *
     +                                      TABLE_SCENARIO_MULTIPLIER *
     +                GET_SCENARIO_ELECTRIC_DEMAND(R_YEAR,LOCAL_MONTH) *
     +                GET_SCENARIO_COAL_DEMAND(R_YEAR,LOCAL_MONTH) *
     +               GET_PARAM_ELECTRIC_DEMAND(R_YEAR,LOCAL_MONTH,TGP) *   
     +                           GET_SCENARIO_PEAK(R_YEAR,LOCAL_MONTH) * 
     +                          GET_PARAM_PEAK(R_YEAR,LOCAL_MONTH,TGP) *
     +                                                  PEAK_LOSS_MULT   
               TG_CG2_DATABASE(LOCAL_MONTH,TG,CG2,3) = 
     +               TG_CG2_DATABASE(LOCAL_MONTH,TG,CG2,3) +
     +                              MONTHLY_CUSTOMERS(LOCAL_MONTH,TRANS)
!
               LATIN_HYPERCUBE_ENERGY = 1.0
               LATIN_HYPERCUBE_PEAK = 1.0
!               SCENARIO_INDEX = GET_SCENARIO_INDEX(SCENARIO_VARIABLE)
! THIS IS THE DEFAULT CASE               
               IF(SCENARIO_INDEX == -99) THEN 
!               
! 11/25/02. NOT ACTIVE: NO SCENARIO MAKER IMPACT               
!
               ELSEIF(SCENARIO_INDEX == 0) THEN 
                  LATIN_HYPERCUBE_ENERGY = 
     +                GET_SCENARIO_COAL_DEMAND(R_YEAR,LOCAL_MONTH) *
     +                GET_SCENARIO_ELECTRIC_DEMAND(R_YEAR,LOCAL_MONTH) *
     +               GET_PARAM_ELECTRIC_DEMAND(R_YEAR,LOCAL_MONTH,TGP) * 
     +                         GET_SCENARIO_ENERGY(R_YEAR,LOCAL_MONTH) * 
     +                         GET_PARAM_ENERGY(R_YEAR,LOCAL_MONTH,TGP)
!
                  LATIN_HYPERCUBE_PEAK = 
     +                GET_SCENARIO_COAL_DEMAND(R_YEAR,LOCAL_MONTH) *
     +                GET_SCENARIO_ELECTRIC_DEMAND(R_YEAR,LOCAL_MONTH) *
     +               GET_PARAM_ELECTRIC_DEMAND(R_YEAR,LOCAL_MONTH,TGP) * 
     +                           GET_SCENARIO_PEAK(R_YEAR,LOCAL_MONTH) * 
     +                           GET_PARAM_PEAK(R_YEAR,LOCAL_MONTH,TGP) 
!               ELSEIF(SCENARIO_INDEX == 4) THEN
               ELSEIF(SCENARIO_INDEX == 49) THEN
                  LATIN_HYPERCUBE_ENERGY = 
     +                GET_SCENARIO_COAL_DEMAND(R_YEAR,LOCAL_MONTH) *
     +                GET_SCENARIO_ELECTRIC_DEMAND(R_YEAR,LOCAL_MONTH) *
     +                GET_PARAM_ELECTRIC_DEMAND(R_YEAR,LOCAL_MONTH,TGP) 
!
                  LATIN_HYPERCUBE_PEAK = 
     +                GET_SCENARIO_COAL_DEMAND(R_YEAR,LOCAL_MONTH) *
     +                GET_SCENARIO_ELECTRIC_DEMAND(R_YEAR,LOCAL_MONTH) *
     +                GET_PARAM_ELECTRIC_DEMAND(R_YEAR,LOCAL_MONTH,TGP) 
               ELSEIF(SCENARIO_INDEX == 15) THEN
                  LATIN_HYPERCUBE_PEAK = 
     +                           GET_SCENARIO_PEAK(R_YEAR,LOCAL_MONTH) * 
     +                           GET_PARAM_PEAK(R_YEAR,LOCAL_MONTH,TGP) 
               ELSEIF(SCENARIO_INDEX == 16) THEN
                  LATIN_HYPERCUBE_ENERGY = 
     +                         GET_SCENARIO_ENERGY(R_YEAR,LOCAL_MONTH) * 
     +                         GET_PARAM_ENERGY(R_YEAR,LOCAL_MONTH,TGP)
!
               ELSE ! SOME DEFINED DISTRIBUTION
!               
                  LATIN_HYPERCUBE_ENERGY = 
     +                           GET_SCENARIO_BY_INDEX(R_YEAR,
     +                                       LOCAL_MONTH,SCENARIO_INDEX)
                  LATIN_HYPERCUBE_PEAK = LATIN_HYPERCUBE_ENERGY
               ENDIF
!
               LAST_THIS_YR_ENERGY(2,LOCAL_MONTH,TRANS) = 
     +                     LAST_THIS_YR_ENERGY(2,LOCAL_MONTH,TRANS) *
     +                                        TABLE_ANNUAL_MULTIPLIER *
     +                                                    MONTHLY_MULT *
     +                                      TABLE_SCENARIO_MULTIPLIER *
     +                                          LATIN_HYPERCUBE_ENERGY *
     +                                         ENERGY_LOSS_MULT(TRANS)   ! ADDED 01/04/01. 
!
               LAST_THIS_YR_PEAK(2,LOCAL_MONTH,TRANS) =
     +                     LAST_THIS_YR_PEAK(2,LOCAL_MONTH,TRANS) *
     +                                        TABLE_ANNUAL_MULTIPLIER *
     +                                                    MONTHLY_MULT *
     +                                      TABLE_SCENARIO_MULTIPLIER *
     +                                            LATIN_HYPERCUBE_PEAK *
     +                                                  PEAK_LOSS_MULT   ! ADDED 01/04/01. 
!
               LAST_THIS_YR_ENERGY(2,LOCAL_MONTH,0) =             
     +                      LAST_THIS_YR_ENERGY(2,LOCAL_MONTH,0) +             
     +                          LAST_THIS_YR_ENERGY(2,LOCAL_MONTH,TRANS)             
               LAST_THIS_YR_PEAK(2,LOCAL_MONTH,0) =             
     +                      LAST_THIS_YR_PEAK(2,LOCAL_MONTH,0) +             
     +                          LAST_THIS_YR_PEAK(2,LOCAL_MONTH,TRANS)             
!
               IF(TG_COUNTER(TG) == 1) THEN
                  HYDRO_WEEK_PLANNING_PEAK = 
     +               GET_HYDRO_WEEK_PLANNING_PEAK(
     +                                         COAL_GROUP_ID(TRANS),
     +                                               R_YEAR,LOCAL_MONTH)
               ELSE
                  HYDRO_WEEK_PLANNING_PEAK = 0.
               ENDIF
!
               ANNUAL_TG_PEAK(LOCAL_MONTH,TG) = 
     +               ANNUAL_TG_PEAK(LOCAL_MONTH,TG) + 
     +                        LAST_THIS_YR_PEAK(2,LOCAL_MONTH,TRANS)*24.
               ANNUAL_TG_VOLUME(TG) = ANNUAL_TG_VOLUME(TG) +
     +                          LAST_THIS_YR_ENERGY(2,LOCAL_MONTH,TRANS)             
               ANNUAL_TG_VOLUME(0) = ANNUAL_TG_VOLUME(0) +
     +                          LAST_THIS_YR_ENERGY(2,LOCAL_MONTH,TRANS)             
!
! 10/13/04
!
               IF(MID_TERM_PEAK_UNCER) THEN ! DEFAULT
                  COAL_DEMAND_PLANNING_PEAK(0,LOCAL_MONTH) = 
     +                     COAL_DEMAND_PLANNING_PEAK(0,LOCAL_MONTH) +
     +                          LAST_THIS_YR_PEAK(2,LOCAL_MONTH,TRANS) * 
     +                          PEAK_COIN_FACTOR(TRANS) -
     +                              HYDRO_WEEK_PLANNING_PEAK
!
                  PA_PLANNING_PEAK(PA,LOCAL_MONTH) = 
     +                     PA_PLANNING_PEAK(PA,LOCAL_MONTH) +
     +                          LAST_THIS_YR_PEAK(2,LOCAL_MONTH,TRANS) * 
     +                          PEAK_COIN_FACTOR(TRANS) -
     +                              HYDRO_WEEK_PLANNING_PEAK
                  COAL_DEMAND_PLANNING_PEAK(TG,LOCAL_MONTH) = 
     +                     COAL_DEMAND_PLANNING_PEAK(TG,LOCAL_MONTH) +
     +                          LAST_THIS_YR_PEAK(2,LOCAL_MONTH,TRANS) * 
     +                          PEAK_COIN_FACTOR(TRANS) - 
     +                              HYDRO_WEEK_PLANNING_PEAK
               ELSE
                  TEMP_R =
     +                           GET_SCENARIO_PEAK(R_YEAR,LOCAL_MONTH) * 
     +                           GET_PARAM_PEAK(R_YEAR,LOCAL_MONTH,TGP) 
                  IF(TEMP_R > 0.0) THEN
                     TEMP_R = 1./TEMP_R
                     COAL_DEMAND_PLANNING_PEAK(0,LOCAL_MONTH) = 
     +                     COAL_DEMAND_PLANNING_PEAK(0,LOCAL_MONTH) +
     +                          LAST_THIS_YR_PEAK(2,LOCAL_MONTH,TRANS) * 
     +                          TEMP_R *
     +                          PEAK_COIN_FACTOR(TRANS) -
     +                              HYDRO_WEEK_PLANNING_PEAK
!
                     PA_PLANNING_PEAK(PA,LOCAL_MONTH) = 
     +                     PA_PLANNING_PEAK(PA,LOCAL_MONTH) +
     +                          LAST_THIS_YR_PEAK(2,LOCAL_MONTH,TRANS) * 
     +                          TEMP_R *
     +                          PEAK_COIN_FACTOR(TRANS) -
     +                              HYDRO_WEEK_PLANNING_PEAK
                     COAL_DEMAND_PLANNING_PEAK(TG,LOCAL_MONTH) = 
     +                     COAL_DEMAND_PLANNING_PEAK(TG,LOCAL_MONTH) +
     +                          LAST_THIS_YR_PEAK(2,LOCAL_MONTH,TRANS) * 
     +                          TEMP_R *
     +                          PEAK_COIN_FACTOR(TRANS) - 
     +                              HYDRO_WEEK_PLANNING_PEAK
                  ENDIF
               ENDIF
!
! FOR USE BY MAINTENANCE PROGRAM
! 10/16/02. MOVED PENALTY FACTOR INTO CLA_OBJECT.
!
!               FUT_PEAK(3,LOCAL_MONTH) = FUT_PEAK(3,LOCAL_MONTH) + 
!     +                          LAST_THIS_YR_PEAK(2,LOCAL_MONTH,TRANS) *
!     +                          MAINTENANCE_PENALTY(LOCAL_MONTH) 
               FUT_PEAK(3,LOCAL_MONTH) = FUT_PEAK(3,LOCAL_MONTH) + 
     +                          LAST_THIS_YR_PEAK(2,LOCAL_MONTH,TRANS)
! OUT 11/3/99. GAT.
!     +                          PEAK_COIN_FACTOR(TRANS)
!     
            ENDDO ! LOCAL_MONTH FOR PEAK AND ENERGY FORECAST
!
            ANNUAL_PEAK(TRANS) = 0.
            DO LOCAL_MONTH = 1, 12
               MONTHLY_TABLE_PEAK_SALES(TRANS,LOCAL_MONTH) =
     +             LAST_THIS_YR_PEAK(2,LOCAL_MONTH,TRANS)/PEAK_LOSS_MULT
               ANNUAL_PEAK(TRANS) = MAX(ANNUAL_PEAK(TRANS),
     +                      MONTHLY_TABLE_PEAK_SALES(TRANS,LOCAL_MONTH))
            ENDDO
            CALL CLOSE_COAL_HOURLY_LOAD_FILE(LOAD_UNIT)
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
            IF(COAL_DEMAND_PLANNING_PEAK(0,LOCAL_MONTH) < 
     +                                                GLOBAL_PEAK) CYCLE
            GLOBAL_PEAK = COAL_DEMAND_PLANNING_PEAK(0,LOCAL_MONTH)
            GLOBAL_PEAK_MONTH = LOCAL_MONTH
         ENDDO
!
! 09/11/02.           
!
         DO TG = 1, MAX_TRANS_GROUPS ! NUMBER_OF_ACTIVE_GROUPS
!
            DO LOCAL_MONTH = 1, 12
!               PA = TG_2_PLANNING_AREA(I)
!
!               
!
               IF(ANNUAL_TG_PEAK(LOCAL_MONTH,TG) < 
     +                                       ANNUAL_TG_PEAK(0,TG)) CYCLE
!
               ANNUAL_TG_PEAK(0,TG) = ANNUAL_TG_PEAK(LOCAL_MONTH,TG)
!     

            ENDDO
!            
         ENDDO

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
     +                                   0:MAX_ASSET_CLASS_GROUPS,0:12))
         ALLOCATE(MONTHLY_AC_CONTRACT_REVENUE(
     +                      -1:MAX_ASSET_CLASS_GROUPS,4,0:12,
     +                                           0:LAST_INCOME_LINE,3)) !msg thinks the order is 3=BUSBAR and 2=meter NOT 1=$,2=BUSBAR,3=METER
         ALLOCATE(MONTHLY_AC_CONTRACT_EXPENSE(
     +                      -1:MAX_ASSET_CLASS_GROUPS,0:12,
     +                                            0:LAST_EXPENSE_ITEM)) !msg thinks the order is 3=BUSBAR and 2=meter NOT 1=$,2=BUSBAR,3=METER
!     +            MONTHLY_INDEXED_ENERGY_REVENUE(
!     +                                 0:MAX_ASSET_CLASS_GROUPS,0:12,3))
         MONTHLY_AC_COST_AT_MARKET = 0.
         MONTHLY_AC_CONTRACT_REVENUE = 0.
         MONTHLY_AC_CONTRACT_EXPENSE = 0.
!
! 1201407
!
!
         call IndexedSortAlphaOrder(MAX_TRANS_LOAD_TABLES,
     +                              iPort,
     +                              SNamePos,
     +                              CUSTOMER_CLASS_NAME)
!            
!            
         ANNUAL_COAL_DEMAND = .TRUE.
      RETURN

      ENTRY MONTHLY_COAL_DEMAND(R_YEAR,R_ISEAS,R_HOURS)

!
! CALANDER CORRECT: ASSUME INITIALLY THAT THE USER TAKES CARE OF THIS.
! WHEN TO SUM?, WHAT IS THE BEST LOOPING PROCEDURE?
! INITIALLY ASSUME THAT THE MODEL IS ALWAYS USING A REFERENCE LOAD.
! INITIALLY ASSUME THAT THE REFERENCE LOADS ARE ALL WORKING OFF OF THE SAME BASE YEAR LOAD.
!
! MAX_TRANS_LOAD_TABLES ARE THE NUMBER OF ACTIVE TABLES IN THE FILE
! MAX_COAL_LOAD_GROUPS ARE THE NUMBER OF TRANSACTION GROUPS (TG) WITH LOADS
!     => SUM OF MAX_TRANS_LOAD_TABLES FOR A TG 
!
!         CALL DOES_COAL_DEMAND_FILE_EXIST(COAL_DEMAND_FILE_EXISTS)
         IF(.NOT. SAVE_COAL_DEMAND_FILE_EXISTS .OR. 
     +                              .NOT. YES_RUN_COAL_MODEL .AND.
     +                              .NOT. YES_ASSET_ANALYST_ONLY) RETURN
!
         SAVE_HOURS = R_HOURS         
         MAX_DAYS_IN_MONTH = SAVE_HOURS/24
!
         IF(R_HOURS == 0 .OR. MAX_TRANS_LOAD_TABLES == 0 .OR.
     +                                  MAX_COAL_LOAD_GROUPS == 0) THEN
            IF(R_HOURS == 0) THEN
               WRITE(4,*) "NO HOURS FOR THE MONTH IN THE TRANSACTION"
               WRITE(4,*) "FORECAST FILE"
               WRITE(4,*) '*** line 2236 COAL_DEMAND_OBJT.FOR ***'
               er_message='See WARNING MESSAGES-coal-obj-7'
               call end_program(er_message)
            ELSEIF(MAX_COAL_LOAD_GROUPS == 0) THEN
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
     +                     ANNUAL_CLASS_ENERGY,
     +                     ANNUAL_CLASS_PEAK,
     +                     ANNUAL_CLASS_CUSTOMERS,
     +                     ANNUAL_TRANS_INDEXED_REVENUE)
            ALLOCATE(
     +           ANNUAL_CLASS_ENERGY_REVENUE(0:MAX_CUST_CLASS_GROUPS))
            ALLOCATE(
     +           ANNUAL_CLASS_PEAK_REVENUE(0:MAX_CUST_CLASS_GROUPS))
            ALLOCATE(
     +           ANNUAL_CLASS_CUSTOMER_REVENUE(0:MAX_CUST_CLASS_GROUPS))
            ALLOCATE(
     +           ANNUAL_CLASS_ENERGY(0:MAX_CUST_CLASS_GROUPS))
            ALLOCATE(
     +           ANNUAL_CLASS_PEAK(0:MAX_CUST_CLASS_GROUPS))
            ALLOCATE(
     +           ANNUAL_CLASS_CUSTOMERS(0:MAX_CUST_CLASS_GROUPS))
            ALLOCATE(
     +           ANNUAL_TRANS_INDEXED_REVENUE(0:MAX_CUST_CLASS_GROUPS))
!     +           ANNUAL_TRANS_INDEXED_REVENUE(0:MAX_TRANS_LOAD_TABLES))
!
            ANNUAL_CLASS_ENERGY_REVENUE = 0.
            ANNUAL_CLASS_PEAK_REVENUE = 0.
            ANNUAL_CLASS_CUSTOMER_REVENUE = 0.
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
     +                     CUSTOMER_DAILY_LOAD,
     +                     TRANS_DAILY_LOAD,
     +                     INTER_DAILY_AVAIL,
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
     +                     MONTHLY_CLASS_ENERGY,
     +                     MONTHLY_CLASS_PEAK,
     +                     MONTHLY_CLASS_CUSTOMERS,
     +                     LOCAL_CUSTOMER_NAME,
     +                     ASSET_CLASS_HOURLY_LOAD)
         ALLOCATE(TRANS_HOURLY_LOAD(
     +                      MAX_HOURS_IN_MONTH,0:MAX_COAL_LOAD_GROUPS))
         ALLOCATE(TRANS_DAILY_LOAD(
     +                     0:MAX_DAYS_IN_MONTH,0:MAX_COAL_LOAD_GROUPS))
         ALLOCATE(INTER_DAILY_AVAIL(
     +                      MAX_DAYS_IN_MONTH,0:MAX_COAL_LOAD_GROUPS))
         ALLOCATE(CUSTOMER_DAILY_LOAD(
     +                      MAX_DAYS_IN_MONTH,0:MAX_CUST_CLASS_GROUPS))
         ALLOCATE(WH_LOADS_PER_HOUR(
     +                      MAX_HOURS_IN_MONTH,0:MAX_COAL_LOAD_GROUPS))
         ALLOCATE(HYDRO_HOURLY_LOAD( ! NEED TO COUNT HYDRO GROUPS
     +                     MAX_HOURS_IN_MONTH,0:NUMBER_OF_HYDRO_GROUPS))
         ALLOCATE(TABLE_HOURLY_LOAD(
     +                      MAX_HOURS_IN_MONTH,0:MAX_TRANS_LOAD_TABLES))
         ALLOCATE(MONTHLY_TRANS_ENERGY(0:MAX_COAL_LOAD_GROUPS))
         ALLOCATE(MONTHLY_HYDRO_ENERGY(0:NUMBER_OF_HYDRO_GROUPS))
         ALLOCATE(MONTHLY_TRANS_PEAK(0:MAX_COAL_LOAD_GROUPS))
         ALLOCATE(MONTHLY_TRANS_BASE(0:MAX_COAL_LOAD_GROUPS))
!
         ALLOCATE(MONTHLY_HYDRO_PEAK(0:NUMBER_OF_HYDRO_GROUPS))
         ALLOCATE(MONTHLY_HYDRO_BASE(0:NUMBER_OF_HYDRO_GROUPS))
!
         ALLOCATE(MONTHLY_TABLE_ENERGY(0:MAX_TRANS_LOAD_TABLES))
         ALLOCATE(MONTHLY_TABLE_SALES_ENERGY(
     +                                         0:MAX_TRANS_LOAD_TABLES))
         ALLOCATE(MONTHLY_TABLE_PEAK(0:MAX_TRANS_LOAD_TABLES))
         ALLOCATE(TABLE_ENERGY_PRICE(MAX_TRANS_LOAD_TABLES))
         ALLOCATE(TABLE_ENERGY_REVENUE(0:MAX_TRANS_LOAD_TABLES))
         ALLOCATE(TABLE_DEMAND_REVENUE(0:MAX_TRANS_LOAD_TABLES))
         ALLOCATE(TABLE_CUSTOMER_REVENUE(0:MAX_TRANS_LOAD_TABLES))
!
         ALLOCATE(TRANS_ENERGY_REVENUE(0:MAX_COAL_LOAD_GROUPS))
         ALLOCATE(TRANS_DEMAND_REVENUE(0:MAX_COAL_LOAD_GROUPS))
         ALLOCATE(TRANS_CUSTOMER_REVENUE(0:MAX_COAL_LOAD_GROUPS))
!
         ALLOCATE(CLASS_ENERGY_REVENUE(0:MAX_CUST_CLASS_GROUPS))
         ALLOCATE(TRANS_INDEXED_REVENUE(0:MAX_CUST_CLASS_GROUPS))
!         ALLOCATE(TRANS_INDEXED_REVENUE(0:MAX_TRANS_LOAD_TABLES))
         ALLOCATE(CLASS_PEAK_REVENUE(0:MAX_CUST_CLASS_GROUPS))
         ALLOCATE(CLASS_CUSTOMER_REVENUE(0:MAX_CUST_CLASS_GROUPS))
         ALLOCATE(MONTHLY_CLASS_ENERGY(0:MAX_CUST_CLASS_GROUPS))
         ALLOCATE(MONTHLY_CLASS_PEAK(0:MAX_CUST_CLASS_GROUPS))
         ALLOCATE(MONTHLY_CLASS_CUSTOMERS(0:MAX_CUST_CLASS_GROUPS))
         ALLOCATE(LOCAL_CUSTOMER_NAME(0:MAX_CUST_CLASS_GROUPS))
         ALLOCATE(ASSET_CLASS_HOURLY_LOAD(MAX_HOURS_IN_MONTH,
     +                                       0:MAX_ASSET_CLASS_GROUPS,
     +                                       0:MAX_COAL_LOAD_GROUPS))
!
         TABLE_ENERGY_PRICE = 0.
!
         TRANS_HOURLY_LOAD = 0.
         TRANS_DAILY_LOAD = 0.
         INTER_DAILY_AVAIL = 0.
         CUSTOMER_DAILY_LOAD = 0.
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
         MONTHLY_CLASS_ENERGY = 0.
         MONTHLY_CLASS_PEAK = 0.
         MONTHLY_CLASS_CUSTOMERS = 0.
         TRANS_INDEXED_REVENUE = 0.
         ASSET_CLASS_HOURLY_LOAD = 0.
!         
!
         DAYS_IN_MONTH = R_HOURS / 24
!
         COAL_DEMAND_GROUP1_ACTIVE = .FALSE.
         COAL_DEMAND_ANY_GROUP_ACTIVE = .FALSE.
!
         USE_SCENARIO_HOURLY_DEMAND = 
     +          OPEN_SCENARIO_HOURLY_DEMAND(R_YEAR,SCENARIO_HOURLY_UNIT)
!     
         DO HR = 1, 24
            SCENARIO_DAILY_LOADS_R4(HR) = 1.0
         ENDDO
!
         DO TRANS = 0, MAX_COAL_LOAD_GROUPS
            MONTHLY_TRANS_ENERGY(TRANS) = 0.
            MONTHLY_TRANS_PEAK(TRANS) = 0.
            MONTHLY_TRANS_BASE(TRANS) = 9999999.
         ENDDO
!         
! REMOVED FOR COAL MODEL
!
!         CALL INITIALIZE_TRANS_LOAD_PROB(MAX_COAL_LOAD_GROUPS)
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
! REMOVED FOR COAL MODEL
!
!         IF(NUMBER_OF_HYDRO_GROUPS > 0) THEN
!            CALL INITIALIZE_HYDRO_LOAD_PROB(NUMBER_OF_HYDRO_GROUPS)
!         ENDIF
!
         IF(ADJUST_FOR_LEAP_YEAR .AND. R_ISEAS > 2) THEN
            LEAP_YEAR_DAY_SHIFT = 1
         ELSE
            LEAP_YEAR_DAY_SHIFT = 0
         ENDIF
!
! 12/23/05. MOVE WEEKLY HYDRO ABOVE. CALCULATE
!
         IF(ALLOCATED(WH_TRANS_ALLOC)) DEALLOCATE (WH_TRANS_ALLOC)
         ALLOCATE (WH_TRANS_ALLOC(0:MAX_TRANS_LOAD_TABLES,
     +                                           MAX_COAL_LOAD_GROUPS))
         WH_TRANS_ALLOC = 0.
         WEEKLY_HYDRO_FORECASTS = .FALSE.
         IF(WH_FILE_EXISTS) THEN
            DO TG = 1, MAX_COAL_LOAD_GROUPS 
               TEMP_R = 0.
               DO TRANS = 1, MAX_TRANS_LOAD_TABLES
                  IF(TABLE_ACTIVE(TRANS) == 'F') CYCLE
!
                  IF( .NOT. IN_ACTIVE_MARKET_AREA(
     +                            BASECASE_MARKET_AREA_ID(TRANS))) CYCLE
!
                  I = COAL_GROUP_ID(TRANS)
!            
! 5/20/02. GAT. NEW CONDITION TO AVOID WARNING IN GET_COAL_GROUP_POSITION           
!
                  IF(I == 0 .OR. 
     +                           COAL_LOAD_GROUPS_INDEX(I) == 0) CYCLE
!
                  IF(.NOT. YES_ASSET_ANALYST_ONLY) THEN            
                     TG_POSITION_IN_TG_FILE = 
     +                                      GET_COAL_GROUP_POSITION(I)
                  ENDIF
!            
                  IF(I == 0 .OR. TG > MAX_TRANS_GROUPS) CYCLE ! DOES NOT CONTRIBUTE TO TRANSACTION LOADS
!                  
                  WH_TRANS_ALLOC(TRANS,TG) = 
     +                              LAST_THIS_YR_ENERGY(2,R_ISEAS,TRANS)
                  WH_TRANS_ALLOC(0,TG) = WH_TRANS_ALLOC(0,TG) +
     +                                          WH_TRANS_ALLOC(TRANS,TG)
!     
               END DO
               IF(WH_TRANS_ALLOC(0,TG) > 0.) THEN
                  DO TRANS = 1, MAX_TRANS_LOAD_TABLES
                     WH_TRANS_ALLOC(TRANS,TG) = 
     +                          WH_TRANS_ALLOC(TRANS,TG)/
     +                                              WH_TRANS_ALLOC(0,TG)
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
     +                                    REF_LEAP_YEAR_DAY_SHIFT(TRANS)
               ELSE
                  LREC = IREC - 1 + DA + TABLE_DAY_SHIFT(TRANS) + 
     +                                               LEAP_YEAR_DAY_SHIFT
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
     +                                 GET_COAL_GROUP_POSITION(TG), ! THIS NEEDS TO BE INDEXED
     +                                 LOAD_UNIT, ! TO CREATE WEEKLY PATTERN
     +                                 WEEKLY_HYDRO_ENERGY,
     +                                 WEEKLY_HYDRO_MINIMUM_MW,
     +                                 WEEKLY_HYDRO_MAXIMUM_MW,
     +                                 WEEKLY_HYDRO_LOADS,
     +                                 HOUR_IN_WEEK,
     +                                 LREC) ! 168 VALUES
                  DO I =  1, 168
                     HR_IN_MONTH = HR_IN_MONTH + 1
                     WH_LOADS_PER_HOUR(HR_IN_MONTH,TG) =
     +                                             WEEKLY_HYDRO_LOADS(I)
                     WH_TRANS_MONTHLY_ENERGY(R_ISEAS,TG) = 
     +                           WH_TRANS_MONTHLY_ENERGY(R_ISEAS,TG) +
     +                                 WH_LOADS_PER_HOUR(HR_IN_MONTH,TG)
                     WH_TRANS_MONTHLY_CAPACITY(R_ISEAS,TG) = 
     +                        MAX(WH_TRANS_MONTHLY_CAPACITY(R_ISEAS,TG),
     +                                WH_LOADS_PER_HOUR(HR_IN_MONTH,TG))
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
            IF( .NOT. IN_ACTIVE_MARKET_AREA(
     +                            BASECASE_MARKET_AREA_ID(TRANS))) CYCLE
!
            TG = COAL_GROUP_ID(TRANS)
!            
! 5/20/02. GAT. NEW CONDITION TO AVOID WARNING IN GET_COAL_GROUP_POSITION           
!
            IF(TG == 0 .OR. COAL_LOAD_GROUPS_INDEX(TG) == 0) CYCLE
!
            IF(.NOT. YES_ASSET_ANALYST_ONLY) THEN            
               TG_POSITION_IN_TG_FILE = GET_COAL_GROUP_POSITION(TG)
            ENDIF
!            
            IF(TG == 0 .OR. TG > MAX_TRANS_GROUPS) CYCLE ! DOES NOT CONTRIBUTE TO TRANSACTION LOADS
! 081308. 
            IF(SPECIAL_CUSTOMER_TYPE(TRANS) == 'P' .AND. 
     +                                            .NOT. COAL_ONLY) CYCLE
!
            IF(TG == 1) COAL_DEMAND_GROUP1_ACTIVE = .TRUE.
            COAL_DEMAND_ANY_GROUP_ACTIVE = .TRUE.
!            
! NOTE DOUBLE INDEX ON TG TO SAVE SPACE
!
            TG = COAL_LOAD_GROUPS_INDEX(TG) 
!
            IF(TG == 0) THEN
               CYCLE ! IT IS NOT AN ACTIVE TRANSACTION GROUP
            ENDIF
!
            CG = CUSTOMER_GROUP(TRANS) 
!
!            IF(USE_MARKET_AREA_REPORT_ID) THEN
!               WRITE(MARKET_AREA_NAME,*) WD_INDEX(TRANS)
!            ELSE
!               MARKET_AREA_NAME = BASECASE_MARKET_AREA_ID(TRANS)(1:5)
!            ENDIF
            IF(USE_MARKET_AREA_REPORT_ID) THEN
               WRITE(MARKET_AREA_NAME,*) GW_INDEX(TRANS)
            ELSE
               MARKET_AREA_NAME = BASECASE_MARKET_AREA_ID(TRANS)(1:5)
            ENDIF
!
            IF(CG < 0 .OR. CG > MAX_CLASS_GROUPS) THEN
               WRITE(4,*) "INVALID INDEXING OF CUSTOMER CLASSES IN"
               WRITE(4,*) "THE TRANSACT FORECAST FILE FOR YEAR ",
     +                                                        LOCAL_YEAR
               WRITE(4,*) "AND SEASON ",R_ISEAS
               WRITE(4,*) "AND TABLE ",TRANS
               WRITE(4,*) "RESET TABLE INDEX TO 1"
               CG = 1
               WRITE(4,*) '*** line 2447 COAL_DEMAND_OBJT.FOR ***'
               er_message='See WARNING MESSAGES-coal-obj-7'
               call end_program(er_message)
            ENDIF
!
            CG = CUST_CLASS_GROUPS_INDEX(CG) 
            CG2 = CUST2_CLASS_GROUPS_INDEX(CG2)
            IF(CG < 0 .OR. CG > MAX_CUST_CLASS_GROUPS) THEN
               WRITE(4,*) "INVALID INDEXING OF CUSTOMER CLASSES IN"
               WRITE(4,*) "THE TRANSACT FORECAST FILE FOR YEAR ",
     +                                                        LOCAL_YEAR
               WRITE(4,*) "AND SEASON ",R_ISEAS
               WRITE(4,*) "AND TABLE ",TRANS
               WRITE(4,*) "RESET TABLE INDEX TO 1"
               CG = 1
               WRITE(4,*) '*** line 2459 COAL_DEMAND_OBJT.FOR ***'
               er_message='See WARNING MESSAGES-coal-obj-6'
               call end_program(er_message)
            ENDIF
!
! NEEDS TO BE MOVED UP.
!
            IF(TRANS_WH_REPORT_NOT_OPEN .AND. WH_FILE_EXISTS) THEN
               TRANS_WH_REPORT_NOT_OPEN = .FALSE.
               TRANS_WH_UNIT = WH_HOURLY_HEADER(TRANS_WH_REC)
            ENDIF
            IF(COAL_MARKET_AREA_REPORT_NOT_OPEN .AND. 
     +                                     COAL_MARKET_AREA_REPORT) THEN
               COAL_MARKET_AREA_REPORT_NOT_OPEN = .FALSE.
               COAL_MARKET_AREA_UNIT = 
     +                COAL_MARKET_AREA_RPT_HEADER(COAL_MARKET_AREA_REC,
     +                                            MAX_CUST_CLASS_GROUPS)

               OPEN(10,FILE="MSG"//TRIM(GET_SCENAME())//".GCD")

               K = 0
               DO J = 1, MAX_TRANS_LOAD_TABLES
                  I = COAL_GROUP_ID(J)
                  IF(I == 0 .OR. COAL_LOAD_GROUPS_INDEX(I) == 0) CYCLE
                  WRITE(TEMP_STR,'(I6)') K
                  TEMP_STR = '"'//CUSTOMER_CLASS_NAME(J)//'"'//
     +                       ',V2,'//TRIM(TEMP_STR)//
     +                               ',,S,,,,,,,,"Total COAL Demanded",'
                  WRITE(10,'(A)') TRIM(TEMP_STR)
                  K = K + 1
               END DO
               CLOSE(10)
               COAL_MARKET_TRANS_UNIT = 
     +               COAL_MARKET_TRANS_RPT_HEADER(COAL_MARKET_TRANS_REC,
     +                                            MAX_COAL_LOAD_GROUPS)
!               OPEN(10,FILE="MSGDAILYREGION.RPT")
               OPEN(10,FILE="MSG"//TRIM(GET_SCENAME())//".GDR")
               DO J = 1,  MAX_COAL_LOAD_GROUPS
!                  IF(COAL_LOAD_GROUP_2_TG(J) == 0) CYCLE
!                  I = FIRST_TABLE_TG(J)
                  I = COAL_LOAD_GROUP_2_TG(J)
!                  I = COAL_LOAD_GROUPS_INDEX(I)
                  WRITE(TEMP_STR,'(I6)') J-1
                  TEMP_STR = '"'//GET_COAL_GROUP_NAME(I)//'"'//
!                  TEMP_STR = '"'//BASECASE_MARKET_AREA_ID(I)//'"'//
     +                       ',V2,'//TRIM(TEMP_STR)//
     +                               ',,S,,,,,,,,"Total COAL Demanded",'
                  WRITE(10,'(A)') TRIM(TEMP_STR)
               END DO
               CLOSE(10)
            ENDIF
            COAL_DEMAND_LOADS_PER_HOUR = 0.
!
            AC = ASSET_CLASS_ID(TRANS)
            AC = ASSET_CLASS_GROUPS_INDEX(AC)
!
!            TG_2_HYDRO_WEEK = GET_TG_2_HYDRO_WEEK(TG)
            TG_2_HYDRO_WEEK = 
     +                     GET_TG_2_HYDRO_WEEK(COAL_GROUP_ID(TRANS))
!
! 10/20/03. NEED A SWITCH IN COAL_DEMAND FILE BY TABLE
!
!
! 10/20/03. FOR DEVELOPMENT PURPOSES.  NEED TO ADD SWITCH IN COAL_DEMAND FILE.
!
            USE_MONTHLY_NONLINEAR_LOADS = 
     +                              THREE_FACTOR_TRANSFORM(TRANS) == 'T'
            IF(USE_MONTHLY_NONLINEAR_LOADS)
     +                             CALL INIT_DAILY_NONLIN_LOADS(R_HOURS)
!     
! 050101. FOR PACIFICORP/NW HYDRO
            HG = GET_HG_FROM_TG(TG)
!
            LOAD_UNIT = 2100 + TG
!            LOAD_UNIT = 2101
!
            SCENARIO_INDEX = -99
!
            CALL OPEN_COAL_HOURLY_LOAD_FILE(
     +                                     REFERENCE_LOAD_NAME(TRANS),
     +                                     REFERENCE_LOAD_NUMBER(TRANS),
     +                                     R_YEAR,LOAD_UNIT,R_ISEAS,
     +                                     SCENARIO_INDEX,
     +                                     LDG_OR_LDE)
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
!           READ(LOAD_UNIT,REC=367) AEL_PEAK,AEL_PEAKMAX ! remove 4/18/01 for the above MSG
!           READ(LOAD_UNIT,REC=368) AEL_BASE,AEL_BASEMIN
!           READ(LOAD_UNIT,REC=369) AEL_ENERGY,AEL_ENRGYEAR
!
! DAY SHIFT LOGIC: HISTORICAL PEAK AND ENERGY GIVEN SHIFTS
!
            IF(R_ISEAS > 2) THEN
               LREC = TABLE_DAY_SHIFT(TRANS) + LEAP_YEAR_DAY_SHIFT +
     +                                    REF_LEAP_YEAR_DAY_SHIFT(TRANS)
            ELSE
               LREC = TABLE_DAY_SHIFT(TRANS) + LEAP_YEAR_DAY_SHIFT
            ENDIF
!            
            IF( (CALENDAR_CORRECT .AND. LREC /= 0) .OR.
     +          (REF_LEAP_YEAR_DAY_SHIFT(TRANS) /= 0 .AND. 
     +                                        R_ISEAS == 2) .OR.
     +                                USE_MONTHLY_NONLINEAR_LOADS ) THEN ! 10/21/03.
!
               IF(LAHEY_LF95()) IREC = IREC - 1  ! The section below requires 
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
     +                                    REF_LEAP_YEAR_DAY_SHIFT(TRANS)
                  ELSE
                     LREC = IREC - 1 + DA + TABLE_DAY_SHIFT(TRANS) + 
     +                                               LEAP_YEAR_DAY_SHIFT
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
     +                                               LEAP_YEAR_DAY_SHIFT
                     WRITE(4,*) "REFERENCE LEAP YEAR DAY SHIFT",
     +                                    REF_LEAP_YEAR_DAY_SHIFT(TRANS)
                     er_message='Stop requested from coal_obj SIID52'
                     call end_program(er_message)
                  ENDIF
!
                  IF(LDG_OR_LDE) THEN
                     READ(LOAD_UNIT,REC=LREC) 
     +                               LDE_MONTH,LDE_DAY,LDE_YEAR,
     +                               EEICODE,DAY_OF_WEEK(DA,TRANS),
     +                               TIMZON,TEMPER,
     +                               DELTMP,DAILY_LOADS_R4
                  ELSE
                     READ(LOAD_UNIT,REC=LREC) 
     +                               LDE_MONTH,LDE_DAY,LDE_YEAR,
     +                               EEICODE,DAY_OF_WEEK(DA,TRANS),
     +                               TIMZON,TEMPER,
     +                               DELTMP,DAILY_LOADS_I4
                     DAILY_LOADS_R4 = DAILY_LOADS_I4
                  ENDIF
!                  IF(LREC == 0) THEN
                  IF(LDE_MONTH == 2 .AND. LDE_DAY == 29) THEN
                     IF(SUM_24_R4(DAILY_LOADS_R4) < 0.001) THEN
                        IF(LDG_OR_LDE) THEN
                           READ(LOAD_UNIT,REC=LREC-7) 
     +                               LDE_MONTH,LDE_DAY,LDE_YEAR,
     +                               EEICODE,DAY_OF_WEEK(DA,TRANS),
     +                               TIMZON,TEMPER,
     +                               DELTMP,DAILY_LOADS_R4
                        ELSE
                           READ(LOAD_UNIT,REC=LREC) 
     +                               LDE_MONTH,LDE_DAY,LDE_YEAR,
     +                               EEICODE,DAY_OF_WEEK(DA,TRANS),
     +                               TIMZON,TEMPER,
     +                               DELTMP,DAILY_LOADS_I4
                           DAILY_LOADS_R4 = DAILY_LOADS_I4
                        ENDIF
                     ENDIF
                  ENDIF
!
                  AEL_ENERGY(R_ISEAS) = AEL_ENERGY(R_ISEAS) + 
     +                                         SUM_24_R4(DAILY_LOADS_R4)
!
                  AEL_PEAK(R_ISEAS) = MAX(AEL_PEAK(R_ISEAS), 
     +                                       PEAK_24_R4(DAILY_LOADS_R4))
                  AEL_BASE(R_ISEAS) = MIN(AEL_BASE(R_ISEAS), 
     +                                       BASE_24_R4(DAILY_LOADS_R4))
!
!
! 10/20/03. CALLED BY SWITCH ABOVE
!
                  IF(USE_MONTHLY_NONLINEAR_LOADS)
     +                    CALL ACCUM_DAILY_I4_NONLIN_LOADS(
     +                                                   DA,
     +                                                   DAILY_LOADS_R4)
!     
!
               ENDDO
            ELSEIF(CALENDAR_CORRECT .AND. LAHEY_LF95())THEN
               IREC = IREC - 1  ! The section below requires 
            ENDIF
!
!
!
! AT THIS POINT WE KNOW THE MONTHLY PEAK AND ENERGY. PRIME TIME TO CALCULATE
! REVENUES BY CUSTOMER CLASS, ASSET CLASS, ETC.  
!
            MONTH_AVE_ENERGY = 
     +                     LAST_THIS_YR_ENERGY(2,R_ISEAS,TRANS)/R_HOURS
            MONTH_AVE_HIST_ENERGY = AEL_ENERGY(R_ISEAS)/R_HOURS
!            
            IF(AEL_PEAK(R_ISEAS) <= MONTH_AVE_HIST_ENERGY .OR. 
     +                        USE_MONTHLY_NONLINEAR_LOADS .OR.
     +                              CALCULATION_MODE(TRANS) == 'R') THEN
!               MONTHLY_SLOPE = 1.0
               MONTHLY_SLOPE = 0.001
               MONTHLY_INTERCEPT = 0.0
!
!
! 10/21/03. 
!
               IF(USE_MONTHLY_NONLINEAR_LOADS)
     +            CALL CONVERT_MONTHLY_NONLIN_LOADS(
     +                      LAST_THIS_YR_PEAK(2,R_ISEAS,TRANS),   ! FORECAST PEAK
     +                      LAST_THIS_YR_ENERGY(2,R_ISEAS,TRANS), ! FORECAST ENERGY
     +                      R_HOURS)
!     
!
            ELSE
               MONTHLY_SLOPE = 
     +               (LAST_THIS_YR_PEAK(2,R_ISEAS,TRANS) - 
     +                                                MONTH_AVE_ENERGY)/
     +                       (AEL_PEAK(R_ISEAS) - MONTH_AVE_HIST_ENERGY)
               MONTHLY_INTERCEPT = LAST_THIS_YR_PEAK(2,R_ISEAS,TRANS) -
     +                                 MONTHLY_SLOPE * AEL_PEAK(R_ISEAS)
               IF(MONTHLY_SLOPE < 0.0) THEN
                  IF(LAST_THIS_YR_PEAK(2,R_ISEAS,TRANS) < 
     +                                            MONTH_AVE_ENERGY) THEN
                     WRITE(4,*) "INPUT PEAK AND ENERGY FOR"
                     WRITE(4,*) CUSTOMER_CLASS_NAME(TRANS)," IN YEAR"
                     WRITE(4,*) LOCAL_YEAR," AND MONTH ",R_ISEAS
                     WRITE(4,*) "IS OUTSIDE OF RANGE"
                     WRITE(4,*) "CHECK THE MONTH LOAD FACTOR IN YOUR" 
                     WRITE(4,*) "TRANSACTION LOADS FILE"
!
                     MONTH_AVE_ENERGY = MONTH_AVE_ENERGY * .98 ! ARBITRARY
                     MONTHLY_SLOPE = 
     +                     (LAST_THIS_YR_PEAK(2,R_ISEAS,TRANS) - 
     +                                                MONTH_AVE_ENERGY)/
     +                       (AEL_PEAK(R_ISEAS) - MONTH_AVE_HIST_ENERGY)
                     MONTHLY_INTERCEPT = 
     +                     LAST_THIS_YR_PEAK(2,R_ISEAS,TRANS) -
     +                                 MONTHLY_SLOPE * AEL_PEAK(R_ISEAS)
!                     
                  ELSEIF(AEL_PEAK(R_ISEAS) < MONTH_AVE_HIST_ENERGY) THEN
                     WRITE(4,*) "HISTORIC LOAD PATTERNS FOR"
                     WRITE(4,*) CUSTOMER_CLASS_NAME(TRANS)
                     WRITE(4,*) REFERENCE_LOAD_NAME(TRANS)," IN YEAR"
                     WRITE(4,*) LOCAL_YEAR," AND MONTH ",R_ISEAS
                     WRITE(4,*) "IS OUTSIDE OF RANGE"
                     WRITE(4,*) "CHECK THE MONTH LOAD FACTOR IN YOUR" 
                     WRITE(4,*) "TRANSACTION LOADS FILE"
                     WRITE(4,*) '*** line 2599 COAL_DEMAND_OBJT.FOR ***'
                     er_message='See WARNING MESSAGES-coal-obj-5'
                     call end_program(er_message)
                  ENDIF
               ENDIF
            ENDIF
!            IREC = ALINE_LOAD_DATA(TRANS,R_ISEAS) ! UNNECESSARY?
!
            YES_FIRST_TABLE = FIRST_TABLE_TG(TG) == TRANS
!
!            WEEKLY_HYDRO_FORECASTS = .FALSE.
!
            HOUR_IN_WEEK = 0
!            
            DO DA = 1, DAYS_IN_MONTH
!
!               CALL GET_DAY_OF_WEEK_IN_MONTH(    ! doesn't have any effect CALENDAR_DAY_OF_WEEK not used
!     +                                 DA,R_ISEAS,CALENDAR_DAY_OF_WEEK)
!
! DAY SHIFT LOGIC BY TABLE
!
               IF(CALENDAR_CORRECT) THEN
                  IF(R_ISEAS > 2) THEN
                     LREC = IREC + TABLE_DAY_SHIFT(TRANS) + 
     +                          LEAP_YEAR_DAY_SHIFT +
     +                                    REF_LEAP_YEAR_DAY_SHIFT(TRANS) ! 07/30/03. 
                  ELSE
                     LREC = IREC + TABLE_DAY_SHIFT(TRANS) + 
     +                                               LEAP_YEAR_DAY_SHIFT
                  ENDIF
!                  LREC = IREC + TABLE_DAY_SHIFT(TRANS) + 
!     +                                               LEAP_YEAR_DAY_SHIFT
!                  IF(LREC <= 0) THEN
!                     LREC = LREC + 365
!                  ELSEIF(LREC > 366) THEN
!                     LREC = LREC - 366
!                  ENDIF
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
               IF(LDG_OR_LDE) THEN
                  READ(LOAD_UNIT,REC=LREC) LDE_MONTH,LDE_DAY,LDE_YEAR,
     +                               EEICODE,DAY_OF_WEEK(DA,TRANS),
     +                               TIMZON,TEMPER,
     +                               DELTMP,DAILY_LOADS_R4
               ELSE
                  READ(LOAD_UNIT,REC=LREC) 
     +                               LDE_MONTH,LDE_DAY,LDE_YEAR,
     +                               EEICODE,DAY_OF_WEEK(DA,TRANS),
     +                               TIMZON,TEMPER,
     +                               DELTMP,DAILY_LOADS_I4
                  DAILY_LOADS_R4 = DAILY_LOADS_I4
               ENDIF
               IF(LDE_MONTH == 2 .AND. LDE_DAY == 29) THEN ! LREC == 60) THEN
                  IF(SUM_24_R4(DAILY_LOADS_R4) == 0) THEN
                        IF(LDG_OR_LDE) THEN
                           READ(LOAD_UNIT,REC=LREC-7) 
     +                               LDE_MONTH,LDE_DAY,LDE_YEAR,
     +                               EEICODE,DAY_OF_WEEK(DA,TRANS),
     +                               TIMZON,TEMPER,
     +                               DELTMP,DAILY_LOADS_R4
                        ELSE
                           READ(LOAD_UNIT,REC=LREC) 
     +                               LDE_MONTH,LDE_DAY,LDE_YEAR,
     +                               EEICODE,DAY_OF_WEEK(DA,TRANS),
     +                               TIMZON,TEMPER,
     +                               DELTMP,DAILY_LOADS_I4
                           DAILY_LOADS_R4 = DAILY_LOADS_I4
                        ENDIF
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
               ACTIVE_GW_FORECAST = .FALSE.
!               IF(WD_FILE_EXISTS) THEN
               IF(GW_FILE_EXISTS) THEN
                  J = GW_INDEX(TRANS)
                  IF(J >= 0) THEN
                     ACTIVE_GW_FORECAST = 
     +                      GET_GW_LOAD(J,R_YEAR,R_ISEAS,DA,
     +                                  DAILY_LOADS_R4,MONTHLY_SLOPE,
     +                                  MONTHLY_INTERCEPT)
!                     MONTHLY_SLOPE = 1.0
!                     MONTHLY_INTERCEPT = 0.0
!                     DO HR = 1, 24
!                        DAILY_LOADS_I4(HR) = WD_DAILY_LOADS_I4(HR)
!                     ENDDO
                  ENDIF
               ELSEIF(USE_MONTHLY_NONLINEAR_LOADS) THEN ! 10/21/03
! NOTE SPECIAL R*4 TO GET HIGHER PRECISION ON THE TRANSFORM.               
                  CALL GET_DAILY_R4_NONLIN_LOADS(DA,DAILY_LOADS_R4)
               ENDIF
!
!               IF(WH_FILE_EXISTS .AND. 
!     +                               LAST_TABLE_FOR_TG(TG)== TRANS) THEN
!
!                  WEEKLY_HYDRO_FORECASTS = 
!     +                  GET_WEEKLY_HYDRO_FORECASTS(
!     +                                 DA,
!     +                                 R_ISEAS,
!     +                                 R_YEAR,
!     +                                 COAL_GROUP_ID(TRANS), ! THIS NEEDS TO BE INDEXED
!     +                                 LOAD_UNIT, ! TO CREATE WEEKLY PATTERN
!     +                                 WEEKLY_HYDRO_ENERGY,
!     +                                 WEEKLY_HYDRO_MINIMUM_MW,
!     +                                 WEEKLY_HYDRO_MAXIMUM_MW,
!     +                                 WEEKLY_HYDRO_LOADS,
!     +                                 HOUR_IN_WEEK,
!     +                                 LREC) ! 168 VALUES
!     
!               ENDIF
!
               IREC = IREC + 1
               DO HR = 1, 24
                  HR_IN_MONTH = HR_IN_MONTH + 1
!                  
                  IF(USE_MONTHLY_NONLINEAR_LOADS .AND. 
     +                                    .NOT. ACTIVE_GW_FORECAST) THEN
!     +                                    .NOT. ACTIVE_WD_FORECAST) THEN
                     REF_HOURLY_LOAD = DAILY_LOADS_R4(HR) 
                  ELSE
                     REF_HOURLY_LOAD = ! TABLE CONTRIBUTION
     +                                                DAILY_LOADS_R4(HR) 
                  ENDIF
!
!                  IF(YES_FIRST_TABLE) THEN
!                     HOURLY_FORWARD_SALE = 
!     +                           HOURLY_FORWARD_CONTRACT_ENERGY(HR,
!     +                                       CALENDAR_DAY_OF_WEEK,DA,TG,
!     +                                       R_HOUR_PRICE,
!     +                                       R_MONTH)
!                  ELSE
                     HOURLY_FORWARD_SALE = 0.
!                  ENDIF
!
                  FINAL_HOURLY_LOAD = 
     +               (MONTHLY_INTERCEPT + 
     +                                  MONTHLY_SLOPE * REF_HOURLY_LOAD)
!
! TESTING. 11/7/99. GAT.
!
                  IF(USE_SCENARIO_HOURLY_DEMAND) THEN
                     FINAL_HOURLY_LOAD = FINAL_HOURLY_LOAD *
     +                                       SCENARIO_DAILY_LOADS_R4(HR)
                  ENDIF
!
! PRE-WEEKLY HYDRO LOADS
!
! HOURLY LOAD OF TABLE AFTER SCENARIO MAKER STUFF
!
                  COAL_DEMAND_LOADS_PER_HOUR(HR_IN_MONTH) = 
     +                                                 FINAL_HOURLY_LOAD
                  IF(ABS(ENERGY_LOSS_MULT(TRANS)) > .0001) THEN
                     FINAL_HOURLY_SALES = FINAL_HOURLY_LOAD /
     +                                           ENERGY_LOSS_MULT(TRANS)
                  ELSE
                     FINAL_HOURLY_SALES = 0.
                  ENDIF                     
!
!
!
                  IF(WH_FILE_EXISTS) THEN
                     IF(WEEKLY_HYDRO_FORECASTS) THEN !  .OR. 
!     +                                            HOUR_IN_WEEK > 0) THEN
!                        HOUR_IN_WEEK = HOUR_IN_WEEK + 1
!                        WH_LOADS_PER_HOUR(HR_IN_MONTH,TG) = 
!     +                                  WEEKLY_HYDRO_LOADS(HOUR_IN_WEEK)
!                        FINAL_HOURLY_LOAD = FINAL_HOURLY_LOAD -
!     +                                  WEEKLY_HYDRO_LOADS(HOUR_IN_WEEK)
!                        WH_LOADS_PER_HOUR(HR_IN_MONTH,TG) = 
!     +                     WH_LOADS_PER_HOUR(HR_IN_MONTH,TG) +
!     +                       WEEKLY_HYDRO_LOADS_FOR_MONTH(HR_IN_MONTH) *
!     +                                          WH_TRANS_ALLOC(TRANS,TG)
                        TEMP_R = WH_LOADS_PER_HOUR(HR_IN_MONTH,TG) *
     +                                          WH_TRANS_ALLOC(TRANS,TG)
                        FINAL_HOURLY_LOAD = FINAL_HOURLY_LOAD - TEMP_R
!                        WH_TRANS_MONTHLY_ENERGY(R_ISEAS,TG) = 
!     +                           WH_TRANS_MONTHLY_ENERGY(R_ISEAS,TG) +
!     +                                 WH_LOADS_PER_HOUR(HR_IN_MONTH,TG)
!                        WH_TRANS_MONTHLY_CAPACITY(R_ISEAS,TG) = 
!     +                        MAX(WH_TRANS_MONTHLY_CAPACITY(R_ISEAS,TG),
!     +                                WH_LOADS_PER_HOUR(HR_IN_MONTH,TG))
                     ENDIF
                  ENDIF
!
                  MONTH_BASE_LOAD = MIN(MONTH_BASE_LOAD,
     +                                                FINAL_HOURLY_LOAD)
!
                  ASSET_CLASS_HOURLY_LOAD(HR_IN_MONTH,AC,TG) = 
     +                    ASSET_CLASS_HOURLY_LOAD(HR_IN_MONTH,AC,TG) +
     +                          FINAL_HOURLY_LOAD + HOURLY_FORWARD_SALE
                  ASSET_CLASS_HOURLY_LOAD(HR_IN_MONTH,0,TG) = ! SUM OF TABLE CONTRIBUTIONS FOR A TG
     +                      ASSET_CLASS_HOURLY_LOAD(HR_IN_MONTH,0,TG) +
     +                          FINAL_HOURLY_LOAD + HOURLY_FORWARD_SALE
!
                  TRANS_HOURLY_LOAD(HR_IN_MONTH,TG) = 
     +                      TRANS_HOURLY_LOAD(HR_IN_MONTH,TG) +
     +                          FINAL_HOURLY_LOAD + HOURLY_FORWARD_SALE
                  TRANS_DAILY_LOAD(DA,TG) = TRANS_DAILY_LOAD(DA,TG) +
     +                          FINAL_HOURLY_LOAD + HOURLY_FORWARD_SALE
                  TRANS_DAILY_LOAD(0,TG) = TRANS_DAILY_LOAD(0,TG) +
     +                          FINAL_HOURLY_LOAD + HOURLY_FORWARD_SALE
                  TRANS_DAILY_LOAD(0,0) = TRANS_DAILY_LOAD(0,0) +
     +                          FINAL_HOURLY_LOAD + HOURLY_FORWARD_SALE
                  IF(SPECIAL_CUSTOMER_TYPE(TRANS) == 'I' .AND.
     +                      INTERRUPTIBLE_PERCENT(TRANS) > .0001) THEN 
                     INTER_DAILY_AVAIL(DA,TG) = 
     +                     INTER_DAILY_AVAIL(DA,TG) +
     +                           FINAL_HOURLY_LOAD*
     +                                 INTERRUPTIBLE_PERCENT(TRANS)
                  ENDIF
                  CUSTOMER_DAILY_LOAD(DA,CG) = 
     +                  CUSTOMER_DAILY_LOAD(DA,CG) +
     +                          FINAL_HOURLY_LOAD + HOURLY_FORWARD_SALE
!
! NEED TO CHANGE THE INDEX FROM TG TO HG DUE TO HYDRO AGGREGATION
! TG => HG
!
                  HYDRO_HOURLY_LOAD(HR_IN_MONTH,HG) = 
     +                      HYDRO_HOURLY_LOAD(HR_IN_MONTH,HG) +
     +                          FINAL_HOURLY_LOAD + HOURLY_FORWARD_SALE
!
                  TRANS_HOURLY_LOAD(HR_IN_MONTH,0) = ! SUM OF TABLE CONTRIBUTIONS FOR A TG
     +                      TRANS_HOURLY_LOAD(HR_IN_MONTH,0) +
     +                          FINAL_HOURLY_LOAD + HOURLY_FORWARD_SALE
                  TRANS_DAILY_LOAD(DA,0) = TRANS_DAILY_LOAD(DA,0) +
     +                          FINAL_HOURLY_LOAD + HOURLY_FORWARD_SALE
                  CUSTOMER_DAILY_LOAD(DA,0) = 
     +                     CUSTOMER_DAILY_LOAD(DA,0) +
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
     +                                 TRANS_HOURLY_LOAD(HR_IN_MONTH,TG) 
                     MONTHLY_TRANS_ENERGY(0) = 
     +                        MONTHLY_TRANS_ENERGY(0) +
     +                                 TRANS_HOURLY_LOAD(HR_IN_MONTH,TG) 
                     MONTHLY_TRANS_PEAK(TG) = 
     +                       MAX(MONTHLY_TRANS_PEAK(TG),
     +                                TRANS_HOURLY_LOAD(HR_IN_MONTH,TG))
                     MONTHLY_TRANS_PEAK(0) = 
     +                       MAX(MONTHLY_TRANS_PEAK(0),
     +                                TRANS_HOURLY_LOAD(HR_IN_MONTH,TG))
                     MONTHLY_TRANS_BASE(TG) = 
     +                       MIN(MONTHLY_TRANS_BASE(TG),
     +                                TRANS_HOURLY_LOAD(HR_IN_MONTH,TG))
                     MONTHLY_TRANS_BASE(0) = 
     +                       MIN(MONTHLY_TRANS_BASE(0),
     +                                TRANS_HOURLY_LOAD(HR_IN_MONTH,TG))
!     
                     MONTHLY_HYDRO_ENERGY(HG) = 
     +                        MONTHLY_HYDRO_ENERGY(HG) +
     +                                 HYDRO_HOURLY_LOAD(HR_IN_MONTH,HG) 
                     MONTHLY_HYDRO_ENERGY(0) = 
     +                        MONTHLY_HYDRO_ENERGY(0) +
     +                                 HYDRO_HOURLY_LOAD(HR_IN_MONTH,HG) 
                     MONTHLY_HYDRO_PEAK(HG) = 
     +                       MAX(MONTHLY_HYDRO_PEAK(HG),
     +                                HYDRO_HOURLY_LOAD(HR_IN_MONTH,HG))
                     MONTHLY_HYDRO_PEAK(0) = 
     +                       MAX(MONTHLY_HYDRO_PEAK(0),
     +                                HYDRO_HOURLY_LOAD(HR_IN_MONTH,HG))
                     MONTHLY_HYDRO_BASE(HG) = 
     +                       MIN(MONTHLY_HYDRO_BASE(HG),
     +                                HYDRO_HOURLY_LOAD(HR_IN_MONTH,HG))
                     MONTHLY_HYDRO_BASE(0) = 
     +                       MIN(MONTHLY_HYDRO_BASE(0),
     +                                HYDRO_HOURLY_LOAD(HR_IN_MONTH,HG))
                  ENDIF
!
!
                  MONTHLY_TABLE_ENERGY(TRANS) = 
     +                        MONTHLY_TABLE_ENERGY(TRANS) +
     +                          FINAL_HOURLY_LOAD + HOURLY_FORWARD_SALE
                  MONTHLY_TABLE_SALES_ENERGY(TRANS) =
     +                        MONTHLY_TABLE_SALES_ENERGY(TRANS) +
     +                                                FINAL_HOURLY_SALES
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
     +               (COAL_DEMAND_LOADS_PER_HOUR(J),
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
     +               (COAL_DEMAND_LOADS_PER_HOUR(J)-
     +                     WH_LOADS_PER_HOUR(J,TG)*
     +                           WH_TRANS_ALLOC(TRANS,TG),
     +                                 J=HR_IN_MONTH-23,HR_IN_MONTH)
                  TRANS_WH_REC = TRANS_WH_REC + 1
               ENDIF
            ENDDO ! DAYS
            CALL CLOSE_COAL_HOURLY_LOAD_FILE(LOAD_UNIT)
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
            LOCAL_DEMAND_PRICE = 1000. * MARKET_DEMAND_PRICE(TRANS) ! KW TO MW CONVERSION
            IF(MONTHLY_DEMAND_PRICE_PATTERN(TRANS) /= 0.) THEN
               LOCAL_DEMAND_PRICE = LOCAL_DEMAND_PRICE *
     +             GET_VAR(MONTHLY_DEMAND_PRICE_PATTERN(TRANS),
     +                                  R_ISEAS,"ForcastPrice Pattern")
            ENDIF
!
! 01/06/01. GAT
!
            TABLE_ENERGY_REVENUE(TRANS) = TABLE_ENERGY_PRICE(TRANS) *
     +                                 MONTHLY_TABLE_SALES_ENERGY(TRANS) 
!     
            IF(DEMAND_PRICING_METHOD(TRANS) == 'A') THEN
               PRICING_PEAK = ANNUAL_PEAK(TRANS)
            ELSE
               PRICING_PEAK = MONTHLY_TABLE_PEAK_SALES(TRANS,R_ISEAS)
            ENDIF
            TABLE_DEMAND_REVENUE(TRANS) = LOCAL_DEMAND_PRICE *
     +                                                      PRICING_PEAK
            TABLE_CUSTOMER_REVENUE(TRANS) = 
     +                        MARKET_CUSTOMER_PRICE(TRANS) *
     +                                  MONTHLY_CUSTOMERS(R_ISEAS,TRANS)

            TRANS_ENERGY_REVENUE(TG) = TRANS_ENERGY_REVENUE(TG) + 
     +                                       TABLE_ENERGY_REVENUE(TRANS) 
            TRANS_DEMAND_REVENUE(TG) = TRANS_DEMAND_REVENUE(TG) + 
     +                                       TABLE_DEMAND_REVENUE(TRANS) 
            TRANS_CUSTOMER_REVENUE(TG) = TRANS_CUSTOMER_REVENUE(TG) + 
     +                                     TABLE_CUSTOMER_REVENUE(TRANS) 
!
            MONTHLY_CLASS_ENERGY(CG) = MONTHLY_CLASS_ENERGY(CG) + 
     +                                       MONTHLY_TABLE_ENERGY(TRANS)
!
            MONTHLY_CLASS_CUSTOMERS(CG) = MONTHLY_CLASS_CUSTOMERS(CG) + 
     +                                  MONTHLY_CUSTOMERS(R_ISEAS,TRANS)
!
            MONTHLY_CLASS_PEAK(CG) = MONTHLY_CLASS_PEAK(CG)
     +                               + PRICING_PEAK
!
            CLASS_ENERGY_REVENUE(CG) = CLASS_ENERGY_REVENUE(CG) + 
     +                                       TABLE_ENERGY_REVENUE(TRANS) 
            CLASS_PEAK_REVENUE(CG) = CLASS_PEAK_REVENUE(CG) + 
     +                                       TABLE_DEMAND_REVENUE(TRANS) 
            CLASS_CUSTOMER_REVENUE(CG) = CLASS_CUSTOMER_REVENUE(CG) + 
     +                                     TABLE_CUSTOMER_REVENUE(TRANS) 
!
! SECOND INDEX: 1 = ENERGY, 2 = PEAK, 3 = CUSTOMERS
! FIFTH INDEX:  1 = $, 2 = UNITS AT BUSBAR (ENERGY, PEAK, CUSTOMERS); 3 = UNITS AT METER
!
            RI = REV_CLASS_INDEX(TRANS,1) ! REVENUE FROM FIXED ENERGY PRICING
            MONTHLY_AC_CONTRACT_REVENUE(AC,1,R_ISEAS,RI,1) = 
     +                  MONTHLY_AC_CONTRACT_REVENUE(AC,1,R_ISEAS,RI,1) +
     +                                       TABLE_ENERGY_REVENUE(TRANS) 
            MONTHLY_AC_CONTRACT_REVENUE(AC,1,0,RI,1) = 
     +                    MONTHLY_AC_CONTRACT_REVENUE(AC,1,0,RI,1) +
     +                                       TABLE_ENERGY_REVENUE(TRANS) 
!
            MONTHLY_AC_CONTRACT_REVENUE(AC,1,R_ISEAS,RI,2) = 
     +                  MONTHLY_AC_CONTRACT_REVENUE(AC,1,R_ISEAS,RI,2) +
     +                                       MONTHLY_TABLE_ENERGY(TRANS) 
            MONTHLY_AC_CONTRACT_REVENUE(AC,1,0,RI,2) = 
     +                    MONTHLY_AC_CONTRACT_REVENUE(AC,1,0,RI,2) +
     +                                       MONTHLY_TABLE_ENERGY(TRANS) 
! 01/06/02.
            MONTHLY_AC_CONTRACT_REVENUE(AC,1,R_ISEAS,RI,3) = 
     +                  MONTHLY_AC_CONTRACT_REVENUE(AC,1,R_ISEAS,RI,3) +
     +                                 MONTHLY_TABLE_SALES_ENERGY(TRANS)
            MONTHLY_AC_CONTRACT_REVENUE(AC,1,0,RI,3) = 
     +                    MONTHLY_AC_CONTRACT_REVENUE(AC,1,0,RI,3) +
     +                                 MONTHLY_TABLE_SALES_ENERGY(TRANS)
!     
            RI = REV_CLASS_INDEX(TRANS,2) ! REVENUE FROM DEMAND PRICING
            MONTHLY_AC_CONTRACT_REVENUE(AC,2,R_ISEAS,RI,1) = 
     +                  MONTHLY_AC_CONTRACT_REVENUE(AC,2,R_ISEAS,RI,1) +
     +                                       TABLE_DEMAND_REVENUE(TRANS) 
            MONTHLY_AC_CONTRACT_REVENUE(AC,2,0,RI,1) = 
     +                       MONTHLY_AC_CONTRACT_REVENUE(AC,2,0,RI,1) +
     +                                       TABLE_DEMAND_REVENUE(TRANS) 
!
            MONTHLY_AC_CONTRACT_REVENUE(AC,2,R_ISEAS,RI,2) = 
     +                  MONTHLY_AC_CONTRACT_REVENUE(AC,2,R_ISEAS,RI,2) +
     +                                                      PRICING_PEAK
            MONTHLY_AC_CONTRACT_REVENUE(AC,2,0,RI,2) = 
     +                       MONTHLY_AC_CONTRACT_REVENUE(AC,2,0,RI,2) +
     +                                                      PRICING_PEAK
            RI = REV_CLASS_INDEX(TRANS,3) ! REVENUE FROM CUSTOMER PRICING
            MONTHLY_AC_CONTRACT_REVENUE(AC,3,R_ISEAS,RI,1) = 
     +                  MONTHLY_AC_CONTRACT_REVENUE(AC,3,R_ISEAS,RI,1) +
     +                                     TABLE_CUSTOMER_REVENUE(TRANS) 
            MONTHLY_AC_CONTRACT_REVENUE(AC,3,0,RI,1) = 
     +                       MONTHLY_AC_CONTRACT_REVENUE(AC,3,0,RI,1) +
     +                                     TABLE_CUSTOMER_REVENUE(TRANS) 
!
            MONTHLY_AC_CONTRACT_REVENUE(AC,3,R_ISEAS,RI,2) = 
     +                  MONTHLY_AC_CONTRACT_REVENUE(AC,3,R_ISEAS,RI,2) +
     +                                  MONTHLY_CUSTOMERS(R_ISEAS,TRANS)
            MONTHLY_AC_CONTRACT_REVENUE(AC,3,0,RI,2) = 
     +                       MONTHLY_AC_CONTRACT_REVENUE(AC,3,0,RI,2) +
     +                                  MONTHLY_CUSTOMERS(R_ISEAS,TRANS)
!
!
            LOCAL_CUSTOMER_NAME(CG) = CUSTOMER_CLASS_NAME(TRANS)(1:24)//
     +                                    BASECASE_MARKET_AREA_ID(TRANS)
!
           IF(INTRA_COMPANY_TRANSACTION(TRANS) == 'Y') THEN
               RI = INCOME_STATEMENT_POSITION(
     +                              INTRA_ACCOUNT_CLASSIFICATION(TRANS))
               IF(INTRA_EXPENSE_COLLECTION(TRANS) == 'BTL') RI = 28
               AC = INTRA_ASSET_CLASS_ID(TRANS)
               AC = ASSET_CLASS_GROUPS_INDEX(AC)
               MONTHLY_AC_CONTRACT_EXPENSE(AC,R_ISEAS,RI) =
     +                        MONTHLY_AC_CONTRACT_EXPENSE(AC,R_ISEAS,RI)
     +                        + TABLE_ENERGY_REVENUE(TRANS)
     +                        + TABLE_DEMAND_REVENUE(TRANS)
     +                        + TABLE_CUSTOMER_REVENUE(TRANS)
               MONTHLY_AC_CONTRACT_EXPENSE(AC,0,RI) =
     +                        MONTHLY_AC_CONTRACT_EXPENSE(AC,0,RI)
     +                        + TABLE_ENERGY_REVENUE(TRANS)
     +                        + TABLE_DEMAND_REVENUE(TRANS)
     +                        + TABLE_CUSTOMER_REVENUE(TRANS)
               MONTHLY_AC_CONTRACT_EXPENSE(-1,R_ISEAS,RI) =
     +                        MONTHLY_AC_CONTRACT_EXPENSE(-1,R_ISEAS,RI)
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
     +                    MONTHLY_AC_CONTRACT_REVENUE(-1,1,R_ISEAS,RI,1)
     +                    + TABLE_ENERGY_REVENUE(TRANS)
               MONTHLY_AC_CONTRACT_REVENUE(-1,1,0,RI,1) =
     +                          MONTHLY_AC_CONTRACT_REVENUE(-1,1,0,RI,1)
     +                          + TABLE_ENERGY_REVENUE(TRANS)
               RI = REV_CLASS_INDEX(TRANS,2) ! REVENUE FROM DEMAND PRICING
               MONTHLY_AC_CONTRACT_REVENUE(-1,2,R_ISEAS,RI,1) =
     +                    MONTHLY_AC_CONTRACT_REVENUE(-1,2,R_ISEAS,RI,1)
     +                    + TABLE_DEMAND_REVENUE(TRANS)
               MONTHLY_AC_CONTRACT_REVENUE(-1,2,0,RI,1) =
     +                          MONTHLY_AC_CONTRACT_REVENUE(-1,2,0,RI,1)
     +                          + TABLE_DEMAND_REVENUE(TRANS)
               RI = REV_CLASS_INDEX(TRANS,3) ! REVENUE FROM CUSTOMER PRICING
               MONTHLY_AC_CONTRACT_REVENUE(-1,3,R_ISEAS,RI,1) =
     +                    MONTHLY_AC_CONTRACT_REVENUE(-1,3,R_ISEAS,RI,1)
     +                    + TABLE_CUSTOMER_REVENUE(TRANS)
               MONTHLY_AC_CONTRACT_REVENUE(-1,3,0,RI,1) =
     +                          MONTHLY_AC_CONTRACT_REVENUE(-1,3,0,RI,1)
     +                          + TABLE_CUSTOMER_REVENUE(TRANS)
            ENDIF
!
!     
!
! MONTHLY CONTRACT REVENUES REPORT           
!
         ENDDO ! TRANSACTION TABLES
! 071406.
         DO DA = 1,  MAX_DAYS_IN_MONTH 
            IF(COAL_MARKET_AREA_REPORT  .AND. .NOT. TESTING_PLAN) THEN
               WRITE(COAL_MARKET_AREA_UNIT,REC=COAL_MARKET_AREA_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(LOCAL_YEAR),
     +               CL_MONTH_NAME(R_ISEAS),
     +               FLOAT(DA),
     +               (CUSTOMER_DAILY_LOAD(DA,J),
     +                                 J=1, MAX_CUST_CLASS_GROUPS)
               COAL_MARKET_AREA_REC = COAL_MARKET_AREA_REC + 1
!               
               WRITE(COAL_MARKET_TRANS_UNIT,REC=COAL_MARKET_TRANS_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(LOCAL_YEAR),
     +               CL_MONTH_NAME(R_ISEAS),
     +               FLOAT(DA),
     +               (TRANS_DAILY_LOAD(DA,J),
     +                                 J=1, MAX_COAL_LOAD_GROUPS)
               COAL_MARKET_TRANS_REC = COAL_MARKET_TRANS_REC + 1
!
            ENDIF
         END DO
!            
!
! CREATES LDC'S THAT ARE PASSED INTO HYDRO LOGIC.
!
!         DO TRANS = 1, MAX_COAL_LOAD_GROUPS ! EXCLUDING TRANS = 0 (SYSTEM) FOR NOW
! DOUBLE INDEX 
!            TG = COAL_LOAD_GROUP_2_TG(TRANS) 
!            TG = GET_COAL_GROUP_POSITION(TG)

!            TRANS_LOOP_POS = TRANS
!            CALL TRANSACT_LOAD_PROB(R_HOURS,
!     +                              TRANS_HOURLY_LOAD(1,TRANS),
!     +                              WH_LOADS_PER_HOUR(1,TRANS),
!     +                              MONTHLY_TRANS_ENERGY(TRANS),
!     +                              MONTHLY_TRANS_PEAK(TRANS),
!     +                              MONTHLY_TRANS_BASE(TRANS),
!     +                              TRANS_LOOP_POS,
!     +                              R_ISEAS,
!     +                              TG)
!         ENDDO
!
! CREATES LDC'S THAT ARE PASSED INTO HYDRO LOGIC.
!
!         DO TRANS = 1, NUMBER_OF_HYDRO_GROUPS ! EXCLUDING TRANS = 0 (SYSTEM) FOR NOW
!
!            HG = GET_HG_FROM_TG(TG)

!            CALL HYDRO_LOAD_PROB(R_HOURS,
!     +                              HYDRO_HOURLY_LOAD(1,TRANS),
!     +                              MONTHLY_HYDRO_ENERGY(TRANS),
!     +                              MONTHLY_HYDRO_PEAK(TRANS),
!     +                              MONTHLY_HYDRO_BASE(TRANS),
!     +                              TRANS,
!     +                              R_ISEAS,
!     +                              HG)
!         ENDDO
!     
! 11/7/99. GAT.
!
         IF(USE_SCENARIO_HOURLY_DEMAND) THEN
            CALL CLOSE_SCEN_HOURLY_LOAD_FILE(SCENARIO_HOURLY_UNIT)
         ENDIF
!
!         DEALLOCATE( REF_HOURLY_LOAD)
         MONTHLY_COAL_DEMAND = .TRUE.
      RETURN
!
! 3/19/02. FOR BURESH TO GET INDEXED REVENUES INTO THE REPORT
!          CALL AFTER TRANSACT.
!      

      ENTRY GET_COAL_ANNUAL_PEAK_VOLUME(R_COAL_GROUP,R_PEAK,R_VOLUME)

         IF( .NOT. SAVE_COAL_DEMAND_FILE_EXISTS) THEN
            R_PEAK = 0.0
            R_VOLUME = 0.0
            GET_COAL_ANNUAL_PEAK_VOLUME = .FALSE. 
         ELSEIF(R_COAL_GROUP == 0) THEN ! ASSUME CALL FOR SYSTEM
            TG = 0
            R_PEAK = ANNUAL_TG_PEAK(0,1) ! ASSUMES JANUARY SYSTEM PEAK
            R_VOLUME = ANNUAL_TG_VOLUME(TG)
            GET_COAL_ANNUAL_PEAK_VOLUME = .TRUE. 
         ELSE
            TG = COAL_LOAD_GROUPS_INDEX(R_COAL_GROUP)
            R_PEAK = ANNUAL_TG_PEAK(0,TG)
            R_VOLUME = ANNUAL_TG_VOLUME(TG)
            GET_COAL_ANNUAL_PEAK_VOLUME = .TRUE. 
         ENDIF
      RETURN

      ENTRY COAL_DEMAND_GROUP_ACTIVE(R_COAL_GROUP)

         IF( .NOT. SAVE_COAL_DEMAND_FILE_EXISTS) THEN
            COAL_DEMAND_GROUP_ACTIVE = .TRUE. 
         ELSE
            COAL_DEMAND_GROUP_ACTIVE = 
     +                              DEMAND_GROUP_AVAILABLE(R_COAL_GROUP)
         ENDIF
      RETURN

      ENTRY WRITE_MONTHLY_COAL_DEMAND_CLASS_SUMMARY(R_YEAR,R_ISEAS)

         YES_MONTHLY_CLASS_REPORTS = MONTHLY_COAL_DEMAND_REPORT() 
         IF(COAL_DEMAND_CLASS_REPORT_NOT_OPEN .AND. 
     +                  YES_MONTHLY_CLASS_REPORTS .AND. 
     +                                         .NOT. TESTING_PLAN ) THEN
            COAL_DEMAND_CLASS_REPORT_NOT_OPEN = .FALSE.
            VARIABLE_NUMBER = 12
            TRANS_CLASS_SUM_UNIT = 
     +               MONTHLY_COAL_DEMAND_HEADER(VARIABLE_NUMBER,
     +                                              TRANS_CLASS_SUM_REC)
            LAST_SEASON = get_PRODUCTION_PERIODS_in()

            CL_MONTH_NAME(LAST_SEASON+1) = 'Annual'
            ANNUAL_COUNTER = LAST_SEASON + 1
         ENDIF
         IF(YES_MONTHLY_CLASS_REPORTS .AND. .NOT. TESTING_PLAN) THEN
            TOTAL_CLASS_ENERGY_REVENUE = 0.
            TOTAL_CLASS_PEAK_REVENUE = 0.
            TOTAL_CLASS_CUSTOMER_REVENUE = 0.
            TOTAL_TRANS_INDEXED_REVENUE = 0.
            TOTAL_MONTHLY_CLASS_ENERGY = 0.
            TOTAL_MONTHLY_CLASS_PEAK = 0.
            TOTAL_MONTHLY_CLASS_CUSTOMERS = 0.
            MONTHLY_MARKET_COUNTER = 0
!
! 3/19/02. CHANGED NOT_AVAIL TO 0. TO ACCOMODATE DATA TRANSFERS.
!
            DO I = 1, MAX_TRANS_LOAD_TABLES ! MAX_CUST_CLASS_GROUPS
               TRANS = iPort(I)
               CG = CUSTOMER_GROUP(TRANS) 
               CG = CUST_CLASS_GROUPS_INDEX(CG) 
!               
! DOUBLE INDEX THEN USE TRANS
!

               IF(TABLE_ACTIVE(TRANS) == 'F') CYCLE
               IF(SPECIAL_CUSTOMER_TYPE(TRANS) == 'P' .AND. 
     +                                            .NOT. COAL_ONLY) CYCLE
!               
               ENRG_AVERAGE_REVENUE = 0.
               IF(MONTHLY_CLASS_ENERGY(CG) /= 0.)
     +           ENRG_AVERAGE_REVENUE = CLASS_ENERGY_REVENUE(CG)/
     +                                  MONTHLY_CLASS_ENERGY(CG)
!
               INDEXED_ENRG_AVERAGE_REVENUE = 0.
               IF(MONTHLY_CLASS_ENERGY(CG) /= 0.)
     +           INDEXED_ENRG_AVERAGE_REVENUE = 
     +                     TRANS_INDEXED_REVENUE(CG)/
     +                                  MONTHLY_CLASS_ENERGY(CG)
!     
               PEAK_AVERAGE_REVENUE = 0.
               IF(MONTHLY_CLASS_PEAK(CG) /= 0.)
     +           PEAK_AVERAGE_REVENUE = CLASS_PEAK_REVENUE(CG)/
     +                                  MONTHLY_CLASS_PEAK(CG)
               CUSTOMER_AVERAGE_REVENUE = 0.
               IF(MONTHLY_CLASS_CUSTOMERS(CG) /= 0.)
     +           CUSTOMER_AVERAGE_REVENUE=CLASS_CUSTOMER_REVENUE(CG)/
     +                                    MONTHLY_CLASS_CUSTOMERS(CG)
               WRITE(TRANS_CLASS_SUM_UNIT,REC=TRANS_CLASS_SUM_REC)
     +            PRT_ENDPOINT(),
     +            FLOAT(LOCAL_YEAR),
     +            CL_MONTH_NAME(R_ISEAS),
     +            CUSTOMER_CLASS_NAME(TRANS), 

     +            CLASS_ENERGY_REVENUE(CG),
     +            CLASS_PEAK_REVENUE(CG),
     +            CLASS_CUSTOMER_REVENUE(CG),
     +            MONTHLY_CLASS_ENERGY(CG),
     +            MONTHLY_CLASS_PEAK(CG)*24.,
     +            MONTHLY_CLASS_CUSTOMERS(CG),
     +            CLASS_ENERGY_REVENUE(CG)
     +             + CLASS_PEAK_REVENUE(CG)
     +             + CLASS_CUSTOMER_REVENUE(CG)
     +             + TRANS_INDEXED_REVENUE(CG),
     +            ENRG_AVERAGE_REVENUE,
     +            PEAK_AVERAGE_REVENUE,
     +            CUSTOMER_AVERAGE_REVENUE,
     +            TRANS_INDEXED_REVENUE(CG),
     +            INDEXED_ENRG_AVERAGE_REVENUE
               TRANS_CLASS_SUM_REC = TRANS_CLASS_SUM_REC + 1
!     
               TOTAL_CLASS_ENERGY_REVENUE = TOTAL_CLASS_ENERGY_REVENUE +
     +                                      CLASS_ENERGY_REVENUE(CG)
               TOTAL_CLASS_PEAK_REVENUE = TOTAL_CLASS_PEAK_REVENUE +
     +                                    CLASS_PEAK_REVENUE(CG)
               TOTAL_CLASS_CUSTOMER_REVENUE = 
     +                     TOTAL_CLASS_CUSTOMER_REVENUE + 
     +                                     CLASS_CUSTOMER_REVENUE(CG)
               TOTAL_MONTHLY_CLASS_ENERGY = TOTAL_MONTHLY_CLASS_ENERGY +
     +                                       MONTHLY_CLASS_ENERGY(CG)
               TOTAL_MONTHLY_CLASS_PEAK = TOTAL_MONTHLY_CLASS_PEAK +
     +                                         MONTHLY_CLASS_PEAK(CG)
               TOTAL_MONTHLY_CLASS_CUSTOMERS = 
     +                     TOTAL_MONTHLY_CLASS_CUSTOMERS + 
     +                                    MONTHLY_CLASS_CUSTOMERS(CG)
               TOTAL_TRANS_INDEXED_REVENUE = 
     +                       TOTAL_TRANS_INDEXED_REVENUE +
     +                                      TRANS_INDEXED_REVENUE(CG)
               ANNUAL_CLASS_ENERGY_REVENUE(CG) = 
     +                              ANNUAL_CLASS_ENERGY_REVENUE(CG) +
     +                                       CLASS_ENERGY_REVENUE(CG)
               ANNUAL_CLASS_PEAK_REVENUE(CG) = 
     +                              ANNUAL_CLASS_PEAK_REVENUE(CG) +
     +                                         CLASS_PEAK_REVENUE(CG)
               ANNUAL_CLASS_CUSTOMER_REVENUE(CG) = 
     +                            ANNUAL_CLASS_CUSTOMER_REVENUE(CG) +
     +                                     CLASS_CUSTOMER_REVENUE(CG)
               ANNUAL_CLASS_ENERGY(CG) = 
     +                              ANNUAL_CLASS_ENERGY(CG) +
     +                                       MONTHLY_CLASS_ENERGY(CG)
               ANNUAL_CLASS_PEAK(CG) = 
     +                       MAX(ANNUAL_CLASS_PEAK(CG),
     +                                        MONTHLY_CLASS_PEAK(CG))
               ANNUAL_CLASS_CUSTOMERS(CG) = 
     +                              ANNUAL_CLASS_CUSTOMERS(CG) +
     +                                    MONTHLY_CLASS_CUSTOMERS(CG)
               ANNUAL_TRANS_INDEXED_REVENUE(CG) = 
     +                       ANNUAL_TRANS_INDEXED_REVENUE(CG) +
     +                                      TRANS_INDEXED_REVENUE(CG)
            ENDDO 
! 120606. MAJOR REWRITE. THIS SECTION COUNTS OFF OF THE COAL NODE
!         INDICES, NOT COAL DEMAND INDICES.
            IF(.NOT. COAL_ONLY) THEN
               DO TRANS = 1, MAX_TRANS_LOAD_TABLES

                     IF(SPECIAL_CUSTOMER_TYPE(TRANS) /= 'P') CYCLE

                     COAL_ID = COAL_GROUP_ID(TRANS)
                     IF(.NOT. COAL_GROUP_ACTIVE_SWITCH(COAL_ID)) CYCLE
!     +                               GET_COAL_GROUP_STATE_POSITION(TRANS)
                     IF(COAL_ID <= 0) THEN
!
                        CYCLE

                     ENDIF
!
! NOTE THE INDEX DOES NOT CHANGE.
!
                     TG = COAL_LOAD_GROUPS_INDEX(COAL_ID) 
                     TEMP_I2 = 
     +                     GET_COAL_NODE_STATE_PROVINCE(COAL_ID,C2_NAME)
! TEMP_I2 RETURNS THE COAL_NODE_ID(TRANS) FROM THE COAL NODES FILE.    
! TG IS THE INDEX THAT TRANS_DAILY_LOAD REQUIRES TO INCREMENT DEMAND.
                     TEMP_CUSTOMER_NAME = "Electric Generation     "//
     +                                                          C2_NAME
!                 ENDIF
                  CALL GET_NG_BY_REGION_BY_MMBTU(COAL_ID,
     +                                           NG_BTUS_BY_GSP_BY_FUEL)
! 1: MONTHLY COAL DEMAND (MMcf/m), 
! 2: MONTHLY PEAK DAY DEMAND (MMcf/d),
! 3: MONTHLY TOTAL COAL CUSTOMERS (#)
                  NG_BTUS_BY_GSP_BY_FUEL(1) = 
     +                                   NG_BTUS_BY_GSP_BY_FUEL(1)/1030.
                  NG_BTUS_BY_GSP_BY_FUEL(2) = 
     +                                   NG_BTUS_BY_GSP_BY_FUEL(2)/1030.
                  NG_BTUS_BY_GSP_BY_FUEL(3) = 
     +                                   NG_BTUS_BY_GSP_BY_FUEL(3)
                  REF_HOURLY_LOAD = NG_BTUS_BY_GSP_BY_FUEL(1)/
     +                                           DAYS_PER_MONTH(R_ISEAS)
! AS A NEXT STEP I CAN GRAB THE TRANS_DAILY_LOAD AVE AND MAX TO MAKE
! SLOPE AND INTERCEPT.     
                  DO DA = 1, DAYS_PER_MONTH(R_ISEAS) 
                     MONTHLY_SLOPE = 1.0
                     MONTHLY_INTERCEPT = 0.0
                     FINAL_HOURLY_LOAD = 
     +                  (MONTHLY_INTERCEPT + 
     +                                  MONTHLY_SLOPE * REF_HOURLY_LOAD)
                     TRANS_DAILY_LOAD(DA,TG) = TRANS_DAILY_LOAD(DA,TG) +
     +                                                FINAL_HOURLY_LOAD 
                     TRANS_DAILY_LOAD(0,TG) = TRANS_DAILY_LOAD(0,TG) +
     +                                                FINAL_HOURLY_LOAD 
                     TRANS_DAILY_LOAD(0,0) = TRANS_DAILY_LOAD(0,0) +
     +                                                FINAL_HOURLY_LOAD 
                     TRANS_DAILY_LOAD(DA,0) = TRANS_DAILY_LOAD(DA,0) +
     +                                                FINAL_HOURLY_LOAD
                  END DO
!
                  
                  WRITE(TRANS_CLASS_SUM_UNIT,REC=TRANS_CLASS_SUM_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(LOCAL_YEAR),
     +               CL_MONTH_NAME(R_ISEAS),
     +               CUSTOMER_CLASS_NAME(TRANS), 
     +               CLASS_ENERGY_REVENUE(CG),
     +               CLASS_PEAK_REVENUE(CG),
     +               CLASS_CUSTOMER_REVENUE(CG),
     +               NG_BTUS_BY_GSP_BY_FUEL(1),
     +               NG_BTUS_BY_GSP_BY_FUEL(2),
     +               NG_BTUS_BY_GSP_BY_FUEL(3),
     +               CLASS_ENERGY_REVENUE(CG)
     +                  + CLASS_PEAK_REVENUE(CG)
     +                  + CLASS_CUSTOMER_REVENUE(CG)
     +                  + TRANS_INDEXED_REVENUE(CG),
     +               ENRG_AVERAGE_REVENUE,
     +               PEAK_AVERAGE_REVENUE,
     +               CUSTOMER_AVERAGE_REVENUE,
     +               TRANS_INDEXED_REVENUE(CG),
     +               INDEXED_ENRG_AVERAGE_REVENUE
                  TRANS_CLASS_SUM_REC = TRANS_CLASS_SUM_REC + 1
!                  
                  TOTAL_MONTHLY_CLASS_ENERGY = 
     +                  TOTAL_MONTHLY_CLASS_ENERGY +
     +                                         NG_BTUS_BY_GSP_BY_FUEL(1)
                  TOTAL_MONTHLY_CLASS_PEAK = TOTAL_MONTHLY_CLASS_PEAK +
     +                                         NG_BTUS_BY_GSP_BY_FUEL(2)
                  TOTAL_MONTHLY_CLASS_CUSTOMERS = 
     +                     TOTAL_MONTHLY_CLASS_CUSTOMERS + 
     +                                         NG_BTUS_BY_GSP_BY_FUEL(3)
!
               ENDDO ! CUSTOMER GROUPS.
            ENDIF ! NOT COAL ONLY.
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
            WRITE(TRANS_CLASS_SUM_UNIT,REC=TRANS_CLASS_SUM_REC)
     +            PRT_ENDPOINT(),
     +            FLOAT(LOCAL_YEAR),
     +            CL_MONTH_NAME(R_ISEAS),
     +            'Total                         ',
     +            TOTAL_CLASS_ENERGY_REVENUE,
     +            TOTAL_CLASS_PEAK_REVENUE,
     +            TOTAL_CLASS_CUSTOMER_REVENUE,
     +            TOTAL_MONTHLY_CLASS_ENERGY,
     +            TOTAL_MONTHLY_CLASS_PEAK*24.,
     +            TOTAL_MONTHLY_CLASS_CUSTOMERS,
     +            TOTAL_CLASS_ENERGY_REVENUE
     +             + TOTAL_CLASS_PEAK_REVENUE
     +             + TOTAL_CLASS_CUSTOMER_REVENUE
     +             + TOTAL_TRANS_INDEXED_REVENUE,
     +            ENRG_AVERAGE_REVENUE,
     +            PEAK_AVERAGE_REVENUE,
     +            CUSTOMER_AVERAGE_REVENUE,
     +            TOTAL_TRANS_INDEXED_REVENUE,
     +            INDEXED_ENRG_AVERAGE_REVENUE
            TRANS_CLASS_SUM_REC = TRANS_CLASS_SUM_REC + 1
            IF(R_ISEAS == 12) THEN
!            
               TOTAL_CLASS_ENERGY_REVENUE = 0.
               TOTAL_CLASS_PEAK_REVENUE = 0.
               TOTAL_CLASS_CUSTOMER_REVENUE = 0.
               TOTAL_TRANS_INDEXED_REVENUE = 0.
               TOTAL_MONTHLY_CLASS_ENERGY = 0.
               TOTAL_MONTHLY_CLASS_PEAK = 0.
               TOTAL_MONTHLY_CLASS_CUSTOMERS = 0.
               MONTHLY_MARKET_COUNTER = 0
!               
               DO I = 1, MAX_TRANS_LOAD_TABLES ! MAX_CUST_CLASS_GROUPS
!
! 021208.
!               
                  TRANS = iPort(I)
                  CG = CUSTOMER_GROUP(TRANS) 
                  CG = CUST_CLASS_GROUPS_INDEX(CG) 
                  ENRG_AVERAGE_REVENUE = 0.
                  IF(ANNUAL_CLASS_ENERGY(CG) /= 0.)
     +              ENRG_AVERAGE_REVENUE =
     +                               ANNUAL_CLASS_ENERGY_REVENUE(CG)/
     +                                   ANNUAL_CLASS_ENERGY(CG)
!
                  INDEXED_ENRG_AVERAGE_REVENUE = 0.
                  IF(ANNUAL_CLASS_ENERGY(CG) /= 0.)
     +               INDEXED_ENRG_AVERAGE_REVENUE = 
     +                     ANNUAL_TRANS_INDEXED_REVENUE(CG)/
     +                                  ANNUAL_CLASS_ENERGY(CG)
!     
                  PEAK_AVERAGE_REVENUE = 0.
                  IF(ANNUAL_CLASS_PEAK(CG) /= 0.)
     +              PEAK_AVERAGE_REVENUE =
     +                            ANNUAL_CLASS_PEAK_REVENUE(CG)/
     +                                    (12.*ANNUAL_CLASS_PEAK(CG))
                  CUSTOMER_AVERAGE_REVENUE = 0.
                  IF(ANNUAL_CLASS_CUSTOMERS(CG) /= 0.)
     +              CUSTOMER_AVERAGE_REVENUE =
     +                             ANNUAL_CLASS_CUSTOMER_REVENUE(CG)/
     +                                  ANNUAL_CLASS_CUSTOMERS(CG)
                  IF(.NOT. (SPECIAL_CUSTOMER_TYPE(TRANS) == 'P') .OR. 
     +                                                   COAL_ONLY) THEN
                     WRITE(TRANS_CLASS_SUM_UNIT,REC=TRANS_CLASS_SUM_REC)
     +                  PRT_ENDPOINT(),
     +                  FLOAT(LOCAL_YEAR),
     +                  CL_MONTH_NAME(13),
     +                  CUSTOMER_CLASS_NAME(TRANS), 
     +                  ANNUAL_CLASS_ENERGY_REVENUE(CG),
     +                  ANNUAL_CLASS_PEAK_REVENUE(CG),
     +                  ANNUAL_CLASS_CUSTOMER_REVENUE(CG),
     +                  ANNUAL_CLASS_ENERGY(CG),
     +                  ANNUAL_CLASS_PEAK(CG),
     +                  ANNUAL_CLASS_CUSTOMERS(CG)/12.,
     +                  ANNUAL_CLASS_ENERGY_REVENUE(CG)
     +                  + ANNUAL_CLASS_PEAK_REVENUE(CG)
     +                  + ANNUAL_CLASS_CUSTOMER_REVENUE(CG)
     +                  + ANNUAL_TRANS_INDEXED_REVENUE(CG),
     +                  ENRG_AVERAGE_REVENUE,
     +                  PEAK_AVERAGE_REVENUE,
     +                  CUSTOMER_AVERAGE_REVENUE,
     +                  ANNUAL_TRANS_INDEXED_REVENUE(CG),
     +                  INDEXED_ENRG_AVERAGE_REVENUE
                     TRANS_CLASS_SUM_REC = TRANS_CLASS_SUM_REC + 1
!     
                     TOTAL_CLASS_ENERGY_REVENUE = 
     +                     TOTAL_CLASS_ENERGY_REVENUE +
     +                                ANNUAL_CLASS_ENERGY_REVENUE(CG)
                     TOTAL_CLASS_PEAK_REVENUE = 
     +                              TOTAL_CLASS_PEAK_REVENUE +
     +                                  ANNUAL_CLASS_PEAK_REVENUE(CG)
                     TOTAL_CLASS_CUSTOMER_REVENUE = 
     +                     TOTAL_CLASS_CUSTOMER_REVENUE + 
     +                              ANNUAL_CLASS_CUSTOMER_REVENUE(CG)
                     TOTAL_MONTHLY_CLASS_ENERGY = 
     +                     TOTAL_MONTHLY_CLASS_ENERGY +
     +                                        ANNUAL_CLASS_ENERGY(CG)
                     TOTAL_MONTHLY_CLASS_PEAK = 
     +                     TOTAL_MONTHLY_CLASS_PEAK +
     +                                          ANNUAL_CLASS_PEAK(CG)
                     TOTAL_MONTHLY_CLASS_CUSTOMERS = 
     +                     TOTAL_MONTHLY_CLASS_CUSTOMERS + 
     +                                     ANNUAL_CLASS_CUSTOMERS(CG)
                     TOTAL_TRANS_INDEXED_REVENUE = 
     +                       TOTAL_TRANS_INDEXED_REVENUE +
     +                               ANNUAL_TRANS_INDEXED_REVENUE(CG)
                  ELSE

                     COAL_ID = COAL_GROUP_ID(TRANS) ! TRANS OR I?
                     IF(.NOT. COAL_GROUP_ACTIVE_SWITCH(COAL_ID)) CYCLE
                     IF(COAL_ID <= 0) THEN
                        CYCLE
                     ENDIF
                     CALL GET_ANNUAL_NG_BY_REGION_BY_MMBTU(
     +                                   COAL_ID,NG_BTUS_BY_GSP_BY_FUEL)
                     NG_BTUS_BY_GSP_BY_FUEL(1) = 
     +                                   NG_BTUS_BY_GSP_BY_FUEL(1)/1030.
                     NG_BTUS_BY_GSP_BY_FUEL(2) = 
     +                                   NG_BTUS_BY_GSP_BY_FUEL(2)/1030.

                     TEMP_I2 = 
     +                     GET_COAL_NODE_STATE_PROVINCE(COAL_ID,C2_NAME)
                     TEMP_CUSTOMER_NAME = "Electric Generation     "//
     +                                                           C2_NAME
                     WRITE(TRANS_CLASS_SUM_UNIT,REC=TRANS_CLASS_SUM_REC)
     +                  PRT_ENDPOINT(),
     +                  FLOAT(LOCAL_YEAR),
     +                  CL_MONTH_NAME(13),
     +                  CUSTOMER_CLASS_NAME(TRANS), 
     +                  ANNUAL_CLASS_ENERGY_REVENUE(CG),
     +                  ANNUAL_CLASS_PEAK_REVENUE(CG),
     +                  ANNUAL_CLASS_CUSTOMER_REVENUE(CG),
     +                  NG_BTUS_BY_GSP_BY_FUEL(1),
     +                  NG_BTUS_BY_GSP_BY_FUEL(2),
     +                  NG_BTUS_BY_GSP_BY_FUEL(3),
     +                  ANNUAL_CLASS_ENERGY_REVENUE(CG)
     +                     + ANNUAL_CLASS_PEAK_REVENUE(CG)
     +                     + ANNUAL_CLASS_CUSTOMER_REVENUE(CG)
     +                     + ANNUAL_TRANS_INDEXED_REVENUE(CG),
     +                  ENRG_AVERAGE_REVENUE,
     +                  PEAK_AVERAGE_REVENUE,
     +                  CUSTOMER_AVERAGE_REVENUE,
     +                  ANNUAL_TRANS_INDEXED_REVENUE(CG),
     +                  INDEXED_ENRG_AVERAGE_REVENUE
                     TRANS_CLASS_SUM_REC = TRANS_CLASS_SUM_REC + 1
!                     
                     TOTAL_MONTHLY_CLASS_ENERGY = 
     +                  TOTAL_MONTHLY_CLASS_ENERGY +
     +                                         NG_BTUS_BY_GSP_BY_FUEL(1)
                     TOTAL_MONTHLY_CLASS_PEAK = 
     +                                 TOTAL_MONTHLY_CLASS_PEAK +
     +                                         NG_BTUS_BY_GSP_BY_FUEL(2)
                     TOTAL_MONTHLY_CLASS_CUSTOMERS = 
     +                     TOTAL_MONTHLY_CLASS_CUSTOMERS + 
     +                                         NG_BTUS_BY_GSP_BY_FUEL(3)
                  ENDIF
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
               WRITE(TRANS_CLASS_SUM_UNIT,REC=TRANS_CLASS_SUM_REC)
     +            PRT_ENDPOINT(),
     +            FLOAT(LOCAL_YEAR),
     +            CL_MONTH_NAME(13),
     +            'Total                         ',
     +            TOTAL_CLASS_ENERGY_REVENUE,
     +            TOTAL_CLASS_PEAK_REVENUE,
     +            TOTAL_CLASS_CUSTOMER_REVENUE,
     +            TOTAL_MONTHLY_CLASS_ENERGY,
     +            TOTAL_MONTHLY_CLASS_PEAK*24.,
     +            TOTAL_MONTHLY_CLASS_CUSTOMERS/12.,
     +            TOTAL_CLASS_ENERGY_REVENUE
     +             + TOTAL_CLASS_PEAK_REVENUE
     +             + TOTAL_CLASS_CUSTOMER_REVENUE
     +             + TOTAL_TRANS_INDEXED_REVENUE,
     +            ENRG_AVERAGE_REVENUE,
     +            PEAK_AVERAGE_REVENUE,
     +            CUSTOMER_AVERAGE_REVENUE,
     +            TOTAL_TRANS_INDEXED_REVENUE,
     +            INDEXED_ENRG_AVERAGE_REVENUE
               TRANS_CLASS_SUM_REC = TRANS_CLASS_SUM_REC + 1
            ENDIF ! R_ISEAS == 12
         ENDIF
         WRITE_MONTHLY_COAL_DEMAND_CLASS_SUMMARY = .TRUE.
      RETURN

      ENTRY GET_MAX_COAL_LOAD_GROUPS()

         GET_MAX_COAL_LOAD_GROUPS = MAX_COAL_LOAD_GROUPS
      RETURN

      ENTRY GET_DAILY_COAL_DEMAND_BY_NODE(R_DA,R_TG,R_INTERRUPTIBLE)

         IF( .NOT. SAVE_COAL_DEMAND_FILE_EXISTS) THEN
            GET_DAILY_COAL_DEMAND_BY_NODE = 0.0
         ELSEIF(R_TG == 0 .AND. R_DA == 0) THEN
            GET_DAILY_COAL_DEMAND_BY_NODE = TRANS_DAILY_LOAD(R_DA,R_TG)
            R_INTERRUPTIBLE = 0
         ELSE
            GS = COAL_LOAD_GROUPS_INDEX(R_TG) ! COAL_LOAD_2_TRANS_GROUPS(R_TG)
            IF(GS > 0 .AND. GS <= MAX_COAL_LOAD_GROUPS) THEN
               GET_DAILY_COAL_DEMAND_BY_NODE = TRANS_DAILY_LOAD(R_DA,GS)
               IF(R_DA > 0) THEN
                  R_INTERRUPTIBLE = INTER_DAILY_AVAIL(R_DA,GS)
               ELSE
                  R_INTERRUPTIBLE = 0
               ENDIF
            ELSE
               GET_DAILY_COAL_DEMAND_BY_NODE = 0.0
               R_INTERRUPTIBLE = 0.0
            ENDIF
         ENDIF
      RETURN

      ENTRY GET_DAILY_COAL_DEMAND_BY_GS(R_DA,R_TG,R_INTERRUPTIBLE)

         IF( .NOT. SAVE_COAL_DEMAND_FILE_EXISTS) THEN
            GET_DAILY_COAL_DEMAND_BY_GS = 0.0
         ELSE
            GS = COAL_LOAD_GROUPS_INDEX(R_TG) ! COAL_LOAD_2_TRANS_GROUPS(R_TG)
            IF(GS > 0 .AND. GS <= MAX_COAL_LOAD_GROUPS) THEN
               GET_DAILY_COAL_DEMAND_BY_GS = 
     +                                         TRANS_DAILY_LOAD(R_DA,GS)
               R_INTERRUPTIBLE = INTER_DAILY_AVAIL(R_DA,GS)
            ELSE
               GET_DAILY_COAL_DEMAND_BY_GS = 0.0
               R_INTERRUPTIBLE = 0.0
            ENDIF
         ENDIF
      RETURN

      ENTRY DEALLOCATE_ANNUAL_COAL_LOADS

!
         DEALLOCATE_ANNUAL_COAL_LOADS = .TRUE.
         IF(ALLOCATED(MONTHLY_AC_COST_AT_MARKET)) 
     +                   DEALLOCATE(MONTHLY_AC_COST_AT_MARKET,
     +                              MONTHLY_AC_CONTRACT_REVENUE)
!     +                              MONTHLY_INDEXED_ENERGY_REVENUE)
      
         IF(ALLOCATED(CUSTOMER_GROUP))
     +      DEALLOCATE(
     +            CUSTOMER_GROUP,
     +            iPort,
     +            SNamePos,
     +            COAL_GROUP_ID,
     +            REFERENCE_LOAD_NUMBER,
     +            MARKET_ENERGY_PRICE,
     +            ANNUAL_TG_PEAK,
     +            ANNUAL_TG_VOLUME,
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
     +            DEMAND_GROUP_AVAILABLE,
     +            WH_TRANS_MONTHLY_ENERGY,
     +            WH_TRANS_MONTHLY_CAPACITY,
     +            TRANS_MONTHLY_PEAK,
     +            TRANS_MONTHLY_CUSTOMERS,
     +            TABLE_ACTIVE,
     +            BASECASE_MARKET_AREA_ID,
     +            WD_INDEX,
     +            GW_INDEX,
     +            BASECASE_TRANS_AREA_ID,
     +            BASECASE_NERC_SUB_ID,
     +            MONTHLY_UNITS,
     +            PRICE_INDEX_ACTIVE,
     +            THREE_FACTOR_TRANSFORM,
     +            JURISDICTIONAL_CUSTOMER,
     +            FUEL_COST_RECOVERY_THROUGH_FAC,
     +            BASE_COST_OF_FAC_FUEL,
     +            CUSTOMER_GROUP_2,
     +            SPECIAL_CUSTOMER_TYPE,
     +            INTERRUPTIBLE_MARKUP_PERENT,
     +            INTERRUPTIBLE_MARKUP_CAP,
     +            INTERRUPTIBLE_MAX_CAPACITY,
     +            INTERRUPTIBLE_MIN_CAPACITY,
     +            INTERRUPTIBLE_MARKUP_ADDER,
     +            INTERRUPTIBLE_PERCENT,
     +            HOURLY_INTERRUPIBLE_REVENUE,
     +            MINIMUM_MARKET_PRICE,
     +            MAXIMUM_MARKET_PRICE,
     +            INDEXED_ENERGY_PRICE,
     +            TG_COUNTER,
     +            LOAD_DISPATCH_POSITION,
     +            LOAD_DISPATCH_INDEX,
!     +            TF_PLANNING_PEAK,
     +            COAL_DEMAND_PLANNING_PEAK,
     +            PA_PLANNING_PEAK,
     +            GLOBAL_PA_PEAK,
     +            GLOBAL_PA_PEAK_MONTH,
     +            TG_2_PLANNING_AREA,
     +            ENERGY_LOSS_MULT,
     +            REF_LEAP_YEAR_DAY_SHIFT,
!
     +            TG_CG_DATABASE,
     +            TG_CG2_DATABASE,
!     +            TRANS_LOAD_GROUPS_INDEX,
     +            COAL_LOAD_GROUPS_INDEX,
     +            COAL_LOAD_2_TRANS_GROUPS,
     +            COAL_LOAD_GROUP_2_TG,
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
      
      ENTRY DEALLOCATE_MONTHLY_COAL_DEMAND
      
!
         DEALLOCATE_MONTHLY_COAL_DEMAND = .TRUE.
         IF(ALLOCATED(DAY_OF_WEEK)) DEALLOCATE(DAY_OF_WEEK)
         IF(ALLOCATED(ANNUAL_CLASS_ENERGY_REVENUE)) 
     +          DEALLOCATE(ANNUAL_CLASS_ENERGY_REVENUE,
     +                     ANNUAL_TRANS_INDEXED_REVENUE,
     +                     ANNUAL_CLASS_PEAK_REVENUE,
     +                     ANNUAL_CLASS_CUSTOMER_REVENUE,
     +                     ANNUAL_CLASS_ENERGY,
     +                     ANNUAL_CLASS_PEAK,
     +                     ANNUAL_CLASS_CUSTOMERS)
          IF(ALLOCATED(TRANS_HOURLY_LOAD)) 
     +          DEALLOCATE(TRANS_HOURLY_LOAD,
     +                     TRANS_DAILY_LOAD,
     +                     INTER_DAILY_AVAIL,
     +                     CUSTOMER_DAILY_LOAD,
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
     +                     MONTHLY_CLASS_ENERGY,
     +                     MONTHLY_CLASS_PEAK,
     +                     MONTHLY_CLASS_CUSTOMERS,
     +                     LOCAL_CUSTOMER_NAME,
     +                     ASSET_CLASS_HOURLY_LOAD)
      RETURN
      END

!
      SUBROUTINE OPEN_COAL_HOURLY_LOAD_FILE(R_LOAD_NAME,R_LOAD_NUMBER,
     +                                       R_YEAR,R_LOAD_UNIT,R_MONTH,
     +                                       R_SCENARIO_INDEX,
     +                                       R_LDG_OR_LDE)
      use end_routine, only: end_program, er_message
      use grx_planning_routines
!         

!
      LOGICAL*1   R_LDG_OR_LDE
      INTEGER*2   HOURLY_LOAD_IN,R_LOAD_NUMBER,R_YEAR,R_LOAD_UNIT,
     +            SAVE_LOAD_UNIT/0/,R_MONTH,SCENARIO_NUMBER,
     +            R_SCENARIO_INDEX
!
      CHARACTER*2 LOAD_FILE_CHAR_EXT
      LOGICAL*4   FILE_EXISTS
      REAL*4      GET_SCENARIO_LOAD_SHAPE
      CHARACTER*5 R_LOAD_NAME
      CHARACTER*256 LOAD_NAME,INPUT_LOAD_NAME,FILE_NAME,
     +              OUTPUT_DIRECTORY,BASE_FILE_DIRECTORY,
     +              LDE_FILE_DIRECTORY,
     +              LDG_FILE_DIRECTORY
      
!
! END DATA DECLARATIONS
!
         SAVE_LOAD_UNIT = R_LOAD_UNIT
!
! 100208. SCENARIO COAL LOAD SHAPE?
         SCENARIO_NUMBER = 
     +                     NINT(GET_SCENARIO_LOAD_SHAPE(R_YEAR,R_MONTH))
         R_LDG_OR_LDE = .TRUE.
!
         IF(SCENARIO_NUMBER >= 5 .AND. R_SCENARIO_INDEX /= -99) THEN ! 
!
            IF(SCENARIO_NUMBER > 1900 .AND. SCENARIO_NUMBER < 2000) THEN
               SCENARIO_NUMBER = SCENARIO_NUMBER - 1900
            ELSE
               SCENARIO_NUMBER = SCENARIO_NUMBER - 2000
            ENDIF
!
!         
            FILE_NAME = trim(LDG_FILE_DIRECTORY())//"LDG"//
     +                           trim(R_LOAD_NAME)//".P"//
     +                               LOAD_FILE_CHAR_EXT(SCENARIO_NUMBER)
!     
            INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
            IF(.NOT. FILE_EXISTS) THEN
               FILE_NAME = trim(OUTPUT_DIRECTORY())//"LDG"//
     +                           trim(R_LOAD_NAME)//".P"//
     +                               LOAD_FILE_CHAR_EXT(SCENARIO_NUMBER)
               INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
! STILL CHECK FOR LDE IF LDG DOESN'T EXIST               
               IF(.NOT. FILE_EXISTS) THEN
                  R_LDG_OR_LDE = .FALSE.
                  FILE_NAME = trim(LDE_FILE_DIRECTORY())//"LDE"//
     +                           trim(R_LOAD_NAME)//".B"//
     +                               LOAD_FILE_CHAR_EXT(SCENARIO_NUMBER)
     
                  INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
                  IF(.NOT. FILE_EXISTS) THEN
                     FILE_NAME = trim(OUTPUT_DIRECTORY())//"LDE"//
     +                           trim(R_LOAD_NAME)//".B"//
     +                               LOAD_FILE_CHAR_EXT(SCENARIO_NUMBER)
                     INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
                  ENDIF
               ENDIF
            ENDIF
!         
            IF(.NOT. FILE_EXISTS) THEN
               WRITE(4,*) "Scenario Maker Load Shape file ",FILE_NAME
               WRITE(4,*) "does not exist in year ",R_YEAR
               WRITE(4,*) "for reference file ",R_LOAD_NAME
               WRITE(4,*) "and reference number ",SCENARIO_NUMBER
               WRITE(4,*) '*** line 5328 COAL_OBJT.FOR ***'
               er_message='See WARNING MESSAGES-coal-obj-4'
               call end_program(er_message)
            ENDIF
!            
         ELSE
!
            IF(R_LOAD_NUMBER == 0) THEN
               INPUT_LOAD_NAME = trim(R_LOAD_NAME)//".BIN"
               LOAD_NAME = trim(R_LOAD_NAME)//".B00"
            ELSE
               INPUT_LOAD_NAME = trim(R_LOAD_NAME)//".P"//
     +                                 LOAD_FILE_CHAR_EXT(R_LOAD_NUMBER)
               LOAD_NAME = INPUT_LOAD_NAME
            ENDIF
!         
            FILE_NAME = trim(LDG_FILE_DIRECTORY())//"LDG"//
     +                           trim(R_LOAD_NAME)//".P"//
     +                                 LOAD_FILE_CHAR_EXT(R_LOAD_NUMBER)
!     
            INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
            IF(.NOT. FILE_EXISTS) THEN
               FILE_NAME = trim(OUTPUT_DIRECTORY())//"LDG"//
     +                           trim(R_LOAD_NAME)//".P"//
     +                                 LOAD_FILE_CHAR_EXT(R_LOAD_NUMBER)
               INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
               
! STILL CHECK FOR LDE IF LDG DOESN'T EXIST               
               IF(.NOT. FILE_EXISTS) THEN
                  R_LDG_OR_LDE = .FALSE.
                  FILE_NAME = trim(LDE_FILE_DIRECTORY())//"LDE"//
     +                           trim(R_LOAD_NAME)//".B"//
     +                                 LOAD_FILE_CHAR_EXT(R_LOAD_NUMBER)
!     
                  INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
                  IF(.NOT. FILE_EXISTS) THEN
                     FILE_NAME = trim(OUTPUT_DIRECTORY())//"LDE"//
     +                           trim(R_LOAD_NAME)//".B"//
     +                                 LOAD_FILE_CHAR_EXT(R_LOAD_NUMBER)
                     INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
                  ENDIF
               ENDIF
            ENDIF
!         
            IF(.NOT. FILE_EXISTS) THEN
               WRITE(4,*) "Transaction loads file ",FILE_NAME
               WRITE(4,*) "does not exist in year ",R_YEAR
               WRITE(4,*) "for reference file ",R_LOAD_NAME
               WRITE(4,*) "and reference number ",R_LOAD_NUMBER
               WRITE(4,*) '*** line 5370 COAL_OBJT.FOR ***'
               er_message='See WARNING MESSAGES-coal-obj-3'
               call end_program(er_message)
            ENDIF
         ENDIF ! USE SCENARIO LOAD SHAPE
!         
         OPEN(R_LOAD_UNIT,FILE=FILE_NAME,ACCESS="DIRECT",
     +                                            STATUS="OLD",RECL=118)
!
      RETURN

      ENTRY CLOSE_COAL_HOURLY_LOAD_FILE(R_LOAD_UNIT)

         CLOSE(R_LOAD_UNIT)
!         CLOSE(2101)
      RETURN
      END

!
!        PROGRAM TO READ MULTI-TAB INFORMATION ON COAL SUPPLY NODES
!                 AND CONVERT TO BINARY FORMAT
!                       COPYRIGHT (C) 2006
!      ALL RIGHTS RESERVED GLOBAL ENERGY DECISIONS
!

!
! 070606. SUBSTANTIALLY CRIBBED FROM TG_OBJECT
! 070606. SUBSTANTIALLY CRIBBED FROM GS_OBJECT
! CS = COAL SUPPLY
!
      SUBROUTINE CS_OBJECT
      use end_routine, only: end_program, er_message
!         

      use spindriftlib
      use prod_arrays_dimensions
      USE SIZECOM

      LOGICAL*1 SAVE_CS_FILE_EXISTS/.FALSE./,R_CS_FILE_EXISTS
      INTEGER*2   UNIT_NUM/10/,INUNIT,IREC,DELETE,LRECL/924/,
     +            SAVE_COAL_GROUPS_TABLES/0/,R_COAL_GROUPS_TABLES,
     +            SAVE_COAL_GROUPS_RECORDS/0/,R_COAL_GROUPS_RECORDS
      INTEGER IOS
      CHARACTER*5 COAL_SUPPLY_FORECAST_FILE,OVERLAY_FAMILY_NAME
      CHARACTER*3 HOURLY_PRICE_NAME
      CHARACTER*255 SAVE_BASE_FILE_NAME,R_TEMP_NAME,FILE_NAME_OVL
      CHARACTER*256 FILE_NAME
      CHARACTER*256 OUTPUT_DIRECTORY
      CHARACTER*256 BASE_FILE_DIRECTORY
      CHARACTER*152 MESSAGE
      LOGICAL*4 FILE_EXISTS
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER*1024 RECLN
!
! SIMULATION VARIABLES
!
      CHARACTER*20 GROUP_NAME
      CHARACTER*40 SCENARIO_VARIABLE
      CHARACTER*1 GROUP_ACTIVE,SPINNING_UNITS,
     +            OFF_PEAK_SPINNING_UNITS,
     +            REPORT_CL_CAPACITY,
     +            TIME_ZONE,NOX_SEASON,PURCHASE_POWER_ASSIGN,
     +            CREATE_HOURLY_PRICE,
     +            SUPPLY_TYPE,
     +            DYNAMIC_EXTRACT_RATE
      CHARACTER*2 ST_LHS_FOR_PRICES
      INTEGER*2   COAL_GROUP_ID,
     +            ASSET_CLASS_ID,
     +            ASSET_CLASS_REV_ALLOC_VECTOR,
     +            PURCHASE_ASSET_CLASS_ID,
     +            PURCHASE_ASSET_ALLOC_VECTOR,
     +            RTO_GROUP,
     +            NOX_YEAR,
     +            END_NOX_YEAR,
     +            HYDRO_LOAD_AGGREGATION,
     +            PLANNING_AREA,
     +            BASIN_START_YEAR
      CHARACTER*6 BASECASE_MARKET_AREA_ID,
     +            BASE_CASE_COAL_AREA_ID,BASECASE_SUBREGION_ID

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
     +    MAX_HOURLY_CS_IMPORT,
     +    MAX_HOURLY_CS_EXPORT,
     +    EXTRACTION_COST_ESC_RATE,
     +    RESERVE_APPRECIATION_PERCENT,
     +    MIN_BASIN_COST
      CHARACTER*50 COMMENT
!      
! FILE MANAGEMENT VARIABLES
!
      CHARACTER*17 FILE_TYPE/'COAL_SUP Group   '/
      CHARACTER*2 CSGROUP_OL/'BC'/,R_CSGROUP_OL
      LOGICAL*1 LAHEY_LF95
      CHARACTER*30 SCREEN_OUTPUT
!
! SPCapEx VARIABLES 11/18/06
!
      REAL (KIND=4) :: LOAD_PLANNING_FACTOR ! 58
      CHARACTER (LEN=20) :: LOAD_FILE_FOR  ! 59
      INTEGER (KIND=2) :: LOAD_SCENARIO_NUM ! 60
      CHARACTER (LEN=20) :: PRICE_FILE_FOR
      INTEGER (KIND=2) :: PRICE_SCENARIO_NUM
      REAL (KIND=4) :: ZONE_MAX_RESERVE
      REAL (KIND=4) :: ZONE_TARGET_RESERVE   ! 64
      CHARACTER (LEN=20) :: CAPEX_COMMENT  ! 65
      CHARACTER (LEN=20) :: MARKET_AREA_LOAD_FILE_FOR  ! 66
      INTEGER (KIND=2) :: MARKET_AREA_LOAD_SCENARIO_NUM ! 67
      CHARACTER (LEN=6) :: PRE_DISPATCH_HYDRO  ! 68
      CHARACTER (LEN=6) :: AGGREGATE_THERMAL   ! 69
      INTEGER (KIND=2) :: MAX_THERMAL_AGG_INTERVALS ! 70
! SP_COAL_MODEL
      REAL*4   OPERATING_COSTS(30), ! (25),
     +         CAPACITY_VALUES(30), ! (25),
     +         POTENTIAL_RESERVES,
     +         PROVEN_RESERVES,
     +         MAX_DAILY_EXTRACTION_RATE,
     +         MAX_MONTHLY_EXTRACTION_RATE,
     +         SHRINKAGE_PERCENT,
     +         SHRINKAGE_COST,
     +         MAX_DAILY_HARD_LIMIT,
     +         MIN_DAILY_LIMIT
      REAL*8 CUM_RES_REMAIN(30) ! (25)
      SAVE SAVE_BASE_FILE_NAME
!
!
!      
!

!
!          ROUTINE TO CONVERT FROM ASCII TO DIRECT-ACESS BINARY
!          COPYRIGHT (C) 1983-98  M.S. GERBER & ASSOCIATES, INC.
!

!
! CONVERT THE COAL_SUPPLY GROUPS FILE
!
!

      ENTRY COAL_SUPPLY_FORECAST_MAKEBIN

      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//
     +                                       COAL_SUPPLY_FORECAST_FILE()
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_LOCATE_WRITE(16,30,
     +                       COAL_SUPPLY_FORECAST_FILE(),ALL_VERSIONS,0)
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
      ENDIF
      FILE_NAME = trim(BASE_FILE_DIRECTORY())//
     +                "CSB"//trim(COAL_SUPPLY_FORECAST_FILE())//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      IF(FILE_EXISTS) THEN
!
         SAVE_CS_FILE_EXISTS = .TRUE.
!
         SAVE_BASE_FILE_NAME = FILE_NAME         
!
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCCSGROUP.BIN",
     +                      ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
!         
!
         SAVE_COAL_GROUPS_TABLES = 0
!
         GROUP_NAME = 'Unassigned          '
         GROUP_ACTIVE = 'T'
         DYNAMIC_EXTRACT_RATE = 'F'
         MIN_BASIN_COST = 0.0
         COAL_GROUP_ID = 1
         BASECASE_MARKET_AREA_ID = 'BLANK ' ! CHAR*6
         BASE_CASE_COAL_AREA_ID = 'BLANK ' ! CHAR*6
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
         MAX_HOURLY_CS_IMPORT = 999999.
         MAX_HOURLY_CS_EXPORT = 999999.
         HYDRO_LOAD_AGGREGATION = 0
         PLANNING_AREA = 0
         SCENARIO_VARIABLE = '                                        '
!
         SUPPLY_TYPE = 'G'
         EXTRACTION_COST_ESC_RATE = 0.0
         RESERVE_APPRECIATION_PERCENT = 0.0
         BASIN_START_YEAR = 2000
!
         IREC = 0
         READ(10,*) DELETE
         DO ! TABLES
            DO ! GROUP-BASED RECORDS
               READ(10,1000,IOSTAT=IOS) RECLN
!               
               IF(IOS /= 0) EXIT ! END OF FILE
!
               IF(RECLN(1:1) == '7') THEN ! END OF TABLE ! EXIT AT BOTTOM OF IF
                  EXIT ! GO TO NEXT TABLE
               ENDIF
!               
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,ERR=200)   DELETE,
     +                                 GROUP_NAME,                ! 1
     +                                 GROUP_ACTIVE,
     +                                 COAL_GROUP_ID,
     +                                 BASECASE_MARKET_AREA_ID,
     +                                 BASE_CASE_COAL_AREA_ID,
     +                                 BASECASE_SUBREGION_ID,
     +                                 SPINNING_UNITS,
     +                                 SPINNING_RESERVE,
     +                                 COMMENT,
     +                                 MAX_HOURLY_RAMP_UP,        ! 10
     +                                 MAX_HOURLY_RAMP_DOWN,      ! 11
     +                                 CAPACITY_VALUES(1),      ! 12
     +                                 CUM_RES_REMAIN(1),    ! 13
     +                                 CAPACITY_VALUES(2),     ! 14
     +                                 CUM_RES_REMAIN(2),   ! 15
     +                                 CAPACITY_VALUES(3),      ! 16
     +                                 CUM_RES_REMAIN(3),    ! 17
     +                                 REPORT_CL_CAPACITY,
     +                                 ASSET_CLASS_ID,
     +                                 ASSET_CLASS_REV_ALLOC_VECTOR, ! 20
     +                                 TIME_ZONE,
     +                                 POTENTIAL_RESERVES,           ! 22
     +                                 NOX_SEASON,
     +                                 PURCHASE_POWER_ASSIGN,
     +                                 PURCHASE_ASSET_CLASS_ID,
     +                                 PURCHASE_ASSET_ALLOC_VECTOR,
     +                                 CREATE_HOURLY_PRICE,
     +                                 HOURLY_PRICE_NAME,
     +                                 CAPACITY_VALUES(4),    ! 29-35
     +                                 CAPACITY_VALUES(5),    ! 29-35
     +                                 CAPACITY_VALUES(6),    ! 29-35
     +                                 CAPACITY_VALUES(7),    ! 29-35
     +                                 CAPACITY_VALUES(8),    ! 29-35
     +                                 CAPACITY_VALUES(9),    ! 29-35
     +                                 CAPACITY_VALUES(10),    ! 29-35
     +                                 CUM_RES_REMAIN(4),  ! 36-42
     +                                 CUM_RES_REMAIN(5),  ! 36-42
     +                                 CUM_RES_REMAIN(6),  ! 36-42
     +                                 CUM_RES_REMAIN(7),  ! 36-42
     +                                 CUM_RES_REMAIN(8),  ! 36-42
     +                                 CUM_RES_REMAIN(9),  ! 36-42
     +                                 CUM_RES_REMAIN(10),  ! 36-42
     +                                 RTO_GROUP,
     +                                 MRX_VOLATILITY_MULT,
     +                                 NOX_YEAR,
     +                                 NIGHT_SCARCITY_MULT,          ! 46
     +                                 PROVEN_RESERVES,              ! 47
     +                                 HYDRO_LOAD_AGGREGATION,
     +                                 OFF_PEAK_SPINNING_RESERVE,
     +                                 OFF_PEAK_SPINNING_UNITS,      ! 50
     +                                 PRICE_CAP,                    ! 51
     +                                 MAX_DAILY_EXTRACTION_RATE,    ! 52
     +                                 MAX_MONTHLY_EXTRACTION_RATE,
     +                                 PLANNING_AREA,
     +                                 SCENARIO_VARIABLE,
     +                                 END_NOX_YEAR,
     +                                 ST_LHS_FOR_PRICES,
! SPCapEx Variables place holders before any new SP variables
     +                                 LOAD_PLANNING_FACTOR,         ! 58
     +                                 LOAD_FILE_FOR,                ! 59
     +                                 LOAD_SCENARIO_NUM,            ! 60
     +                                 PRICE_FILE_FOR,
     +                                 PRICE_SCENARIO_NUM,
     +                                 ZONE_MAX_RESERVE,
     +                                 ZONE_TARGET_RESERVE,   ! 64
     +                                 CAPEX_COMMENT,  ! 65
     +                                 MARKET_AREA_LOAD_FILE_FOR,  ! 66
     +                                 MARKET_AREA_LOAD_SCENARIO_NUM, ! 67
     +                                 PRE_DISPATCH_HYDRO,  ! 68
     +                                 AGGREGATE_THERMAL,   ! 69
     +                                 MAX_THERMAL_AGG_INTERVALS,    ! 70
     +                                 OPERATING_COSTS(1),         ! 71-80
     +                                 OPERATING_COSTS(2),         ! 71-80
     +                                 OPERATING_COSTS(3),         ! 71-80
     +                                 OPERATING_COSTS(4),         ! 71-80
     +                                 OPERATING_COSTS(5),         ! 71-80
     +                                 OPERATING_COSTS(6),         ! 71-80
     +                                 OPERATING_COSTS(7),         ! 71-80
     +                                 OPERATING_COSTS(8),         ! 71-80
     +                                 OPERATING_COSTS(9),         ! 71-80
     +                                 OPERATING_COSTS(10),         ! 71-80
     +                                 CAPACITY_VALUES(11),              ! 81-95
     +                                 CAPACITY_VALUES(12),              ! 81-95
     +                                 CAPACITY_VALUES(13),              ! 81-95
     +                                 CAPACITY_VALUES(14),              ! 81-95
     +                                 CAPACITY_VALUES(15),              ! 81-95
     +                                 CAPACITY_VALUES(16),              ! 81-95
     +                                 CAPACITY_VALUES(17),              ! 81-95
     +                                 CAPACITY_VALUES(18),              ! 81-95
     +                                 CAPACITY_VALUES(19),              ! 81-95
     +                                 CAPACITY_VALUES(20),              ! 81-95
     +                                 CAPACITY_VALUES(21),              ! 81-95
     +                                 CAPACITY_VALUES(22),              ! 81-95
     +                                 CAPACITY_VALUES(23),              ! 81-95
     +                                 CAPACITY_VALUES(24),              ! 81-95
     +                                 CAPACITY_VALUES(25),              ! 81-95
     +                                 OPERATING_COSTS(11),              ! 81-95
     +                                 OPERATING_COSTS(12),              ! 81-95
     +                                 OPERATING_COSTS(13),              ! 81-95
     +                                 OPERATING_COSTS(14),              ! 81-95
     +                                 OPERATING_COSTS(15),              ! 81-95
     +                                 OPERATING_COSTS(16),              ! 81-95
     +                                 OPERATING_COSTS(17),              ! 81-95
     +                                 OPERATING_COSTS(18),              ! 81-95
     +                                 OPERATING_COSTS(19),              ! 81-95
     +                                 OPERATING_COSTS(20),              ! 81-95
     +                                 OPERATING_COSTS(21),              ! 81-95
     +                                 OPERATING_COSTS(22),              ! 81-95
     +                                 OPERATING_COSTS(23),              ! 81-95
     +                                 OPERATING_COSTS(24),              ! 81-95
     +                                 OPERATING_COSTS(25),              ! 81-95
     +                                 CUM_RES_REMAIN(11),              ! 81-95
     +                                 CUM_RES_REMAIN(12),              ! 81-95
     +                                 CUM_RES_REMAIN(13),              ! 81-95
     +                                 CUM_RES_REMAIN(14),              ! 81-95
     +                                 CUM_RES_REMAIN(15),              ! 81-95
     +                                 CUM_RES_REMAIN(16),              ! 81-95
     +                                 CUM_RES_REMAIN(17),              ! 81-95
     +                                 CUM_RES_REMAIN(18),              ! 81-95
     +                                 CUM_RES_REMAIN(19),              ! 81-95
     +                                 CUM_RES_REMAIN(20),              ! 81-95
     +                                 CUM_RES_REMAIN(21),              ! 81-95
     +                                 CUM_RES_REMAIN(22),              ! 81-95
     +                                 CUM_RES_REMAIN(23),              ! 81-95
     +                                 CUM_RES_REMAIN(24),              ! 81-95
     +                                 CUM_RES_REMAIN(25),              ! 81-95
     +                                 SHRINKAGE_PERCENT,            ! 126
     +                                 SHRINKAGE_COST,                ! 127 
     +                                 SUPPLY_TYPE, ! C*1
     +                                 EXTRACTION_COST_ESC_RATE, ! R*4
     +                                 BASIN_START_YEAR, ! I*2
     +                                 RESERVE_APPRECIATION_PERCENT,
     +                                 MAX_DAILY_HARD_LIMIT,
     +                                 MIN_DAILY_LIMIT,
     +                                 DYNAMIC_EXTRACT_RATE,
     +                                 MIN_BASIN_COST
!
               IREC = IREC + 1
               WRITE(11,REC=IREC)      DELETE,
     +                                 GROUP_NAME,                ! 1
     +                                 GROUP_ACTIVE,
     +                                 COAL_GROUP_ID,
     +                                 BASECASE_MARKET_AREA_ID,
     +                                 BASE_CASE_COAL_AREA_ID,
     +                                 BASECASE_SUBREGION_ID,
     +                                 SPINNING_UNITS,
     +                                 SPINNING_RESERVE,
     +                                 COMMENT,
     +                                 MAX_HOURLY_RAMP_UP,        ! 10
     +                                 MAX_HOURLY_RAMP_DOWN,      ! 11
     +                                 CAPACITY_VALUES(1),      ! 12
     +                                 CUM_RES_REMAIN(1),    ! 13
     +                                 CAPACITY_VALUES(2),     ! 14
     +                                 CUM_RES_REMAIN(2),   ! 15
     +                                 CAPACITY_VALUES(3),      ! 16
     +                                 CUM_RES_REMAIN(3),    ! 17
     +                                 REPORT_CL_CAPACITY,
     +                                 ASSET_CLASS_ID,
     +                                 ASSET_CLASS_REV_ALLOC_VECTOR, ! 20
     +                                 TIME_ZONE,
     +                                 POTENTIAL_RESERVES,           ! 22
     +                                 NOX_SEASON,
     +                                 PURCHASE_POWER_ASSIGN,
     +                                 PURCHASE_ASSET_CLASS_ID,
     +                                 PURCHASE_ASSET_ALLOC_VECTOR,
     +                                 CREATE_HOURLY_PRICE,
     +                                 HOURLY_PRICE_NAME,
     +                                 CAPACITY_VALUES(4),    ! 29-35
     +                                 CAPACITY_VALUES(5),    ! 29-35
     +                                 CAPACITY_VALUES(6),    ! 29-35
     +                                 CAPACITY_VALUES(7),    ! 29-35
     +                                 CAPACITY_VALUES(8),    ! 29-35
     +                                 CAPACITY_VALUES(9),    ! 29-35
     +                                 CAPACITY_VALUES(10),    ! 29-35
     +                                 CUM_RES_REMAIN(4),  ! 36-42
     +                                 CUM_RES_REMAIN(5),  ! 36-42
     +                                 CUM_RES_REMAIN(6),  ! 36-42
     +                                 CUM_RES_REMAIN(7),  ! 36-42
     +                                 CUM_RES_REMAIN(8),  ! 36-42
     +                                 CUM_RES_REMAIN(9),  ! 36-42
     +                                 CUM_RES_REMAIN(10),  ! 36-42
     +                                 RTO_GROUP,
     +                                 MRX_VOLATILITY_MULT,
     +                                 NOX_YEAR,
     +                                 NIGHT_SCARCITY_MULT,          ! 46
     +                                 PROVEN_RESERVES,              ! 47
     +                                 HYDRO_LOAD_AGGREGATION,
     +                                 OFF_PEAK_SPINNING_RESERVE,
     +                                 OFF_PEAK_SPINNING_UNITS,      ! 50
     +                                 PRICE_CAP,                    ! 51
     +                                 MAX_DAILY_EXTRACTION_RATE,    ! 52
     +                                 MAX_MONTHLY_EXTRACTION_RATE,
     +                                 PLANNING_AREA,
     +                                 SCENARIO_VARIABLE,
     +                                 END_NOX_YEAR,
     +                                 ST_LHS_FOR_PRICES,
! SPCapEx Variables place holders before any new SP variables
     +                                 LOAD_PLANNING_FACTOR,         ! 58
     +                                 LOAD_FILE_FOR,                ! 59
     +                                 LOAD_SCENARIO_NUM,            ! 60
     +                                 PRICE_FILE_FOR,
     +                                 PRICE_SCENARIO_NUM,
     +                                 ZONE_MAX_RESERVE,
     +                                 ZONE_TARGET_RESERVE,   ! 64
     +                                 CAPEX_COMMENT,  ! 65
     +                                 MARKET_AREA_LOAD_FILE_FOR,  ! 66
     +                                 MARKET_AREA_LOAD_SCENARIO_NUM, ! 67
     +                                 PRE_DISPATCH_HYDRO,  ! 68
     +                                 AGGREGATE_THERMAL,   ! 69
     +                                 MAX_THERMAL_AGG_INTERVALS,    ! 70
     +                                 OPERATING_COSTS(1),         ! 71-80
     +                                 OPERATING_COSTS(2),         ! 71-80
     +                                 OPERATING_COSTS(3),         ! 71-80
     +                                 OPERATING_COSTS(4),         ! 71-80
     +                                 OPERATING_COSTS(5),         ! 71-80
     +                                 OPERATING_COSTS(6),         ! 71-80
     +                                 OPERATING_COSTS(7),         ! 71-80
     +                                 OPERATING_COSTS(8),         ! 71-80
     +                                 OPERATING_COSTS(9),         ! 71-80
     +                                 OPERATING_COSTS(10),         ! 71-80
     +                                 CAPACITY_VALUES(11),              ! 81-95
     +                                 CAPACITY_VALUES(12),              ! 81-95
     +                                 CAPACITY_VALUES(13),              ! 81-95
     +                                 CAPACITY_VALUES(14),              ! 81-95
     +                                 CAPACITY_VALUES(15),              ! 81-95
     +                                 CAPACITY_VALUES(16),              ! 81-95
     +                                 CAPACITY_VALUES(17),              ! 81-95
     +                                 CAPACITY_VALUES(18),              ! 81-95
     +                                 CAPACITY_VALUES(19),              ! 81-95
     +                                 CAPACITY_VALUES(20),              ! 81-95
     +                                 CAPACITY_VALUES(21),              ! 81-95
     +                                 CAPACITY_VALUES(22),              ! 81-95
     +                                 CAPACITY_VALUES(23),              ! 81-95
     +                                 CAPACITY_VALUES(24),              ! 81-95
     +                                 CAPACITY_VALUES(25),              ! 81-95
     +                                 OPERATING_COSTS(11),              ! 81-95
     +                                 OPERATING_COSTS(12),              ! 81-95
     +                                 OPERATING_COSTS(13),              ! 81-95
     +                                 OPERATING_COSTS(14),              ! 81-95
     +                                 OPERATING_COSTS(15),              ! 81-95
     +                                 OPERATING_COSTS(16),              ! 81-95
     +                                 OPERATING_COSTS(17),              ! 81-95
     +                                 OPERATING_COSTS(18),              ! 81-95
     +                                 OPERATING_COSTS(19),              ! 81-95
     +                                 OPERATING_COSTS(20),              ! 81-95
     +                                 OPERATING_COSTS(21),              ! 81-95
     +                                 OPERATING_COSTS(22),              ! 81-95
     +                                 OPERATING_COSTS(23),              ! 81-95
     +                                 OPERATING_COSTS(24),              ! 81-95
     +                                 OPERATING_COSTS(25),              ! 81-95
     +                                 CUM_RES_REMAIN(11),              ! 81-95
     +                                 CUM_RES_REMAIN(12),              ! 81-95
     +                                 CUM_RES_REMAIN(13),              ! 81-95
     +                                 CUM_RES_REMAIN(14),              ! 81-95
     +                                 CUM_RES_REMAIN(15),              ! 81-95
     +                                 CUM_RES_REMAIN(16),              ! 81-95
     +                                 CUM_RES_REMAIN(17),              ! 81-95
     +                                 CUM_RES_REMAIN(18),              ! 81-95
     +                                 CUM_RES_REMAIN(19),              ! 81-95
     +                                 CUM_RES_REMAIN(20),              ! 81-95
     +                                 CUM_RES_REMAIN(21),              ! 81-95
     +                                 CUM_RES_REMAIN(22),              ! 81-95
     +                                 CUM_RES_REMAIN(23),              ! 81-95
     +                                 CUM_RES_REMAIN(24),              ! 81-95
     +                                 CUM_RES_REMAIN(25),              ! 81-95
     +                                 SHRINKAGE_PERCENT,            ! 126
     +                                 SHRINKAGE_COST,                ! 127 
     +                                 SUPPLY_TYPE, ! C*1
     +                                 EXTRACTION_COST_ESC_RATE, ! R*4
     +                                 BASIN_START_YEAR, ! I*2
     +                                 RESERVE_APPRECIATION_PERCENT,
     +                                 MAX_DAILY_HARD_LIMIT,
     +                                 MIN_DAILY_LIMIT,
     +                                 DYNAMIC_EXTRACT_RATE,
     +                                 MIN_BASIN_COST
!
            ENDDO ! COAL_SUPPLY GROUPS
            SAVE_COAL_GROUPS_TABLES = SAVE_COAL_GROUPS_TABLES + 1
            IF(IOS /= 0) EXIT
         ENDDO ! READ TABLES
         CLOSE(10)
      ELSE IF(INDEX(COAL_SUPPLY_FORECAST_FILE(),'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "coal_obj:0005")
      ELSE
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCCSGROUP.BIN",
     +                      ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
!
         SAVE_CS_FILE_EXISTS = .FALSE.
!         
      ENDIF
      SAVE_COAL_GROUPS_RECORDS = IREC
!      ENDFILE(11)
      CLOSE(11)
      RETURN

!
!          ROUTINE TO CREATE OVERLAY FILES
!          COPYRIGHT (C) 1984-88  M.S. GERBER & ASSOCIATES, INC.
!          COPYRIGHT (C) 1991-92  M.S. GERBER & ASSOCIATES, INC.
!

!
! OVERLAY THE SYSTEM-FORECAST FILE

      ENTRY COAL_SUPPLY_FORECAST_MAKEOVL(OVERLAY_FAMILY_NAME)


      CALL LOCATE(10,51)
      CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
      FILE_NAME_OVL=trim(OUTPUT_DIRECTORY())//"CSO"//
     +                               trim(OVERLAY_FAMILY_NAME)//".DAT"
      OPEN(10,FILE=FILE_NAME_OVL)
      READ(10,*) DELETE
      INUNIT = 12
      IF(CSGROUP_OL == 'BC') THEN
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCCSGROUP.BIN",
     +                                       ACCESS="DIRECT",RECL=LRECL)
         INUNIT = 11
      ENDIF

      FILE_NAME = trim(OUTPUT_DIRECTORY())//"OLCSGROUP.BIN"

      OPEN(12,FILE=FILE_NAME,ACCESS="DIRECT", 
     +                           STATUS="UNKNOWN",RECL=LRECL,IOSTAT=IOS)
      IF(IOS /= 0) THEN
         CALL IOSTAT_MSG(IOS,MESSAGE)
         WRITE(4,*) trim(MESSAGE)
         WRITE(4,*) '*** line 4176 TF_OBJT.FOR ***'
         er_message='See WARNING MESSAGES-coal-obj-2'
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
     +                                 GROUP_NAME,                ! 1
     +                                 GROUP_ACTIVE,
     +                                 COAL_GROUP_ID,
     +                                 BASECASE_MARKET_AREA_ID,
     +                                 BASE_CASE_COAL_AREA_ID,
     +                                 BASECASE_SUBREGION_ID,
     +                                 SPINNING_UNITS,
     +                                 SPINNING_RESERVE,
     +                                 COMMENT,
     +                                 MAX_HOURLY_RAMP_UP,        ! 10
     +                                 MAX_HOURLY_RAMP_DOWN,      ! 11
     +                                 CAPACITY_VALUES(1),      ! 12
     +                                 CUM_RES_REMAIN(1),    ! 13
     +                                 CAPACITY_VALUES(2),     ! 14
     +                                 CUM_RES_REMAIN(2),   ! 15
     +                                 CAPACITY_VALUES(3),      ! 16
     +                                 CUM_RES_REMAIN(3),    ! 17
     +                                 REPORT_CL_CAPACITY,
     +                                 ASSET_CLASS_ID,
     +                                 ASSET_CLASS_REV_ALLOC_VECTOR, ! 20
     +                                 TIME_ZONE,
     +                                 POTENTIAL_RESERVES,           ! 22
     +                                 NOX_SEASON,
     +                                 PURCHASE_POWER_ASSIGN,
     +                                 PURCHASE_ASSET_CLASS_ID,
     +                                 PURCHASE_ASSET_ALLOC_VECTOR,
     +                                 CREATE_HOURLY_PRICE,
     +                                 HOURLY_PRICE_NAME,
     +                                 CAPACITY_VALUES(4:10),    ! 29-35
     +                                 CUM_RES_REMAIN(4:10),  ! 36-42
     +                                 RTO_GROUP,
     +                                 MRX_VOLATILITY_MULT,
     +                                 NOX_YEAR,
     +                                 NIGHT_SCARCITY_MULT,          ! 46
     +                                 PROVEN_RESERVES,              ! 47
     +                                 HYDRO_LOAD_AGGREGATION,
     +                                 OFF_PEAK_SPINNING_RESERVE,
     +                                 OFF_PEAK_SPINNING_UNITS,      ! 50
     +                                 PRICE_CAP,                    ! 51
     +                                 MAX_DAILY_EXTRACTION_RATE,    ! 52
     +                                 MAX_MONTHLY_EXTRACTION_RATE,
     +                                 PLANNING_AREA,
     +                                 SCENARIO_VARIABLE,
     +                                 END_NOX_YEAR,
     +                                 ST_LHS_FOR_PRICES,
! SPCapEx Variables place holders before any new SP variables
     +                                 LOAD_PLANNING_FACTOR,         ! 58
     +                                 LOAD_FILE_FOR,                ! 59
     +                                 LOAD_SCENARIO_NUM,            ! 60
     +                                 PRICE_FILE_FOR,
     +                                 PRICE_SCENARIO_NUM,
     +                                 ZONE_MAX_RESERVE,
     +                                 ZONE_TARGET_RESERVE,   ! 64
     +                                 CAPEX_COMMENT,  ! 65
     +                                 MARKET_AREA_LOAD_FILE_FOR,  ! 66
     +                                 MARKET_AREA_LOAD_SCENARIO_NUM, ! 67
     +                                 PRE_DISPATCH_HYDRO,  ! 68
     +                                 AGGREGATE_THERMAL,   ! 69
     +                                 MAX_THERMAL_AGG_INTERVALS,    ! 70
! COAL MODEL     
     +                                 OPERATING_COSTS(1:10),         ! 71-80
     +                                 CAPACITY_VALUES(11:25),              ! 81-95
     +                                 OPERATING_COSTS(11:25),        ! 96-110
     +                                 CUM_RES_REMAIN(11:25),         ! 111-125     
     +                                 SHRINKAGE_PERCENT,            ! 126
     +                                 SHRINKAGE_COST,                ! 127 
     +                                 SUPPLY_TYPE, ! C*1
     +                                 EXTRACTION_COST_ESC_RATE, ! R*4
     +                                 BASIN_START_YEAR, ! I*2
     +                                 RESERVE_APPRECIATION_PERCENT,
     +                                 MAX_DAILY_HARD_LIMIT,
     +                                 MIN_DAILY_LIMIT,
     +                                 DYNAMIC_EXTRACT_RATE,
     +                                 MIN_BASIN_COST
         IF(IOS /= 0) EXIT

         RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
         READ(RECLN,*,ERR=200)         DELETE,
     +                                 GROUP_NAME,                ! 1
     +                                 GROUP_ACTIVE,
     +                                 COAL_GROUP_ID,
     +                                 BASECASE_MARKET_AREA_ID,
     +                                 BASE_CASE_COAL_AREA_ID,
     +                                 BASECASE_SUBREGION_ID,
     +                                 SPINNING_UNITS,
     +                                 SPINNING_RESERVE,
     +                                 COMMENT,
     +                                 MAX_HOURLY_RAMP_UP,        ! 10
     +                                 MAX_HOURLY_RAMP_DOWN,      ! 11
     +                                 CAPACITY_VALUES(1),      ! 12
     +                                 CUM_RES_REMAIN(1),    ! 13
     +                                 CAPACITY_VALUES(2),     ! 14
     +                                 CUM_RES_REMAIN(2),   ! 15
     +                                 CAPACITY_VALUES(3),      ! 16
     +                                 CUM_RES_REMAIN(3),    ! 17
     +                                 REPORT_CL_CAPACITY,
     +                                 ASSET_CLASS_ID,
     +                                 ASSET_CLASS_REV_ALLOC_VECTOR, ! 20
     +                                 TIME_ZONE,
     +                                 POTENTIAL_RESERVES,           ! 22
     +                                 NOX_SEASON,
     +                                 PURCHASE_POWER_ASSIGN,
     +                                 PURCHASE_ASSET_CLASS_ID,
     +                                 PURCHASE_ASSET_ALLOC_VECTOR,
     +                                 CREATE_HOURLY_PRICE,
     +                                 HOURLY_PRICE_NAME,
     +                                 CAPACITY_VALUES(4:10),    ! 29-35
     +                                 CUM_RES_REMAIN(4:10),  ! 36-42
     +                                 RTO_GROUP,
     +                                 MRX_VOLATILITY_MULT,
     +                                 NOX_YEAR,
     +                                 NIGHT_SCARCITY_MULT,          ! 46
     +                                 PROVEN_RESERVES,              ! 47
     +                                 HYDRO_LOAD_AGGREGATION,
     +                                 OFF_PEAK_SPINNING_RESERVE,
     +                                 OFF_PEAK_SPINNING_UNITS,      ! 50
     +                                 PRICE_CAP,                    ! 51
     +                                 MAX_DAILY_EXTRACTION_RATE,    ! 52
     +                                 MAX_MONTHLY_EXTRACTION_RATE,
     +                                 PLANNING_AREA,
     +                                 SCENARIO_VARIABLE,
     +                                 END_NOX_YEAR,
     +                                 ST_LHS_FOR_PRICES,
! SPCapEx Variables place holders before any new SP variables
     +                                 LOAD_PLANNING_FACTOR,         ! 58
     +                                 LOAD_FILE_FOR,                ! 59
     +                                 LOAD_SCENARIO_NUM,            ! 60
     +                                 PRICE_FILE_FOR,
     +                                 PRICE_SCENARIO_NUM,
     +                                 ZONE_MAX_RESERVE,
     +                                 ZONE_TARGET_RESERVE,   ! 64
     +                                 CAPEX_COMMENT,  ! 65
     +                                 MARKET_AREA_LOAD_FILE_FOR,  ! 66
     +                                 MARKET_AREA_LOAD_SCENARIO_NUM, ! 67
     +                                 PRE_DISPATCH_HYDRO,  ! 68
     +                                 AGGREGATE_THERMAL,   ! 69
     +                                 MAX_THERMAL_AGG_INTERVALS,    ! 70
     +                                 OPERATING_COSTS(1:10),         ! 71-80
     +                                 CAPACITY_VALUES(11:25),              ! 81-95
     +                                 OPERATING_COSTS(11:25),        ! 96-110
     +                                 CUM_RES_REMAIN(11:25),         ! 111-125     
     +                                 SHRINKAGE_PERCENT,            ! 126
     +                                 SHRINKAGE_COST,                ! 127 
     +                                 SUPPLY_TYPE, ! C*1
     +                                 EXTRACTION_COST_ESC_RATE, ! R*4
     +                                 BASIN_START_YEAR, ! I*2
     +                                 RESERVE_APPRECIATION_PERCENT,
     +                                 MAX_DAILY_HARD_LIMIT,
     +                                 MIN_DAILY_LIMIT,
     +                                 DYNAMIC_EXTRACT_RATE,
     +                                 MIN_BASIN_COST
!
         READ(10,1000,IOSTAT=IOS) RECLN
         DOWHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE 
!              
            READ(10,1000,IOSTAT=IOS) RECLN
!         
         ENDDO
!         ENDIF
!
         WRITE(12,REC=IREC)            DELETE,        
     +                                 GROUP_NAME,                ! 1
     +                                 GROUP_ACTIVE,
     +                                 COAL_GROUP_ID,
     +                                 BASECASE_MARKET_AREA_ID,
     +                                 BASE_CASE_COAL_AREA_ID,
     +                                 BASECASE_SUBREGION_ID,
     +                                 SPINNING_UNITS,
     +                                 SPINNING_RESERVE,
     +                                 COMMENT,
     +                                 MAX_HOURLY_RAMP_UP,        ! 10
     +                                 MAX_HOURLY_RAMP_DOWN,      ! 11
     +                                 CAPACITY_VALUES(1),      ! 12
     +                                 CUM_RES_REMAIN(1),    ! 13
     +                                 CAPACITY_VALUES(2),     ! 14
     +                                 CUM_RES_REMAIN(2),   ! 15
     +                                 CAPACITY_VALUES(3),      ! 16
     +                                 CUM_RES_REMAIN(3),    ! 17
     +                                 REPORT_CL_CAPACITY,
     +                                 ASSET_CLASS_ID,
     +                                 ASSET_CLASS_REV_ALLOC_VECTOR, ! 20
     +                                 TIME_ZONE,
     +                                 POTENTIAL_RESERVES,           ! 22
     +                                 NOX_SEASON,
     +                                 PURCHASE_POWER_ASSIGN,
     +                                 PURCHASE_ASSET_CLASS_ID,
     +                                 PURCHASE_ASSET_ALLOC_VECTOR,
     +                                 CREATE_HOURLY_PRICE,
     +                                 HOURLY_PRICE_NAME,
     +                                 CAPACITY_VALUES(4:10),    ! 29-35
     +                                 CUM_RES_REMAIN(4:10),  ! 36-42
     +                                 RTO_GROUP,
     +                                 MRX_VOLATILITY_MULT,
     +                                 NOX_YEAR,
     +                                 NIGHT_SCARCITY_MULT,          ! 46
     +                                 PROVEN_RESERVES,              ! 47
     +                                 HYDRO_LOAD_AGGREGATION,
     +                                 OFF_PEAK_SPINNING_RESERVE,
     +                                 OFF_PEAK_SPINNING_UNITS,      ! 50
     +                                 PRICE_CAP,                    ! 51
     +                                 MAX_DAILY_EXTRACTION_RATE,    ! 52
     +                                 MAX_MONTHLY_EXTRACTION_RATE,
     +                                 PLANNING_AREA,
     +                                 SCENARIO_VARIABLE,
     +                                 END_NOX_YEAR,
     +                                 ST_LHS_FOR_PRICES,
! SPCapEx Variables place holders before any new SP variables
     +                                 LOAD_PLANNING_FACTOR,         ! 58
     +                                 LOAD_FILE_FOR,                ! 59
     +                                 LOAD_SCENARIO_NUM,            ! 60
     +                                 PRICE_FILE_FOR,
     +                                 PRICE_SCENARIO_NUM,
     +                                 ZONE_MAX_RESERVE,
     +                                 ZONE_TARGET_RESERVE,   ! 64
     +                                 CAPEX_COMMENT,  ! 65
     +                                 MARKET_AREA_LOAD_FILE_FOR,  ! 66
     +                                 MARKET_AREA_LOAD_SCENARIO_NUM, ! 67
     +                                 PRE_DISPATCH_HYDRO,  ! 68
     +                                 AGGREGATE_THERMAL,   ! 69
     +                                 MAX_THERMAL_AGG_INTERVALS,    ! 70
     +                                 OPERATING_COSTS(1:10),         ! 71-80
     +                                 CAPACITY_VALUES(11:25),              ! 81-95
     +                                 OPERATING_COSTS(11:25),        ! 96-110
     +                                 CUM_RES_REMAIN(11:25),         ! 111-125     
     +                                 SHRINKAGE_PERCENT,            ! 126
     +                                 SHRINKAGE_COST,                ! 127 
     +                                 SUPPLY_TYPE, ! C*1
     +                                 EXTRACTION_COST_ESC_RATE, ! R*4
     +                                 BASIN_START_YEAR, ! I*2
     +                                 RESERVE_APPRECIATION_PERCENT,
     +                                 MAX_DAILY_HARD_LIMIT,
     +                                 MIN_DAILY_LIMIT,
     +                                 DYNAMIC_EXTRACT_RATE,
     +                                 MIN_BASIN_COST
      ENDDO
      IF(IREC /= SAVE_COAL_GROUPS_RECORDS) THEN
         WRITE(4,*) "COAL_SUPPLY GROUP OVERLAY DIFFERENT LENGTH"
         WRITE(4,*) "THAN THE BASE FILE. OVERLAY MUST BE THE SAME"
         WRITE(4,*) "LENGTH. ",FILE_NAME_OVL
      ENDIF
      CLOSE(10)
      CLOSE(12)
      IF(CSGROUP_OL == 'BC') CLOSE(11)
      CSGROUP_OL = 'OL'
      RETURN
!

      ENTRY RESET_COAL_SUPPLY_OL

         CSGROUP_OL = 'BC'
      RETURN
!

      ENTRY RETURN_CSGROUP_OL(R_CSGROUP_OL)

         R_CSGROUP_OL = CSGROUP_OL
      RETURN

      ENTRY DOES_CS_FILE_EXIST(R_CS_FILE_EXISTS)

         R_CS_FILE_EXISTS = SAVE_CS_FILE_EXISTS
      RETURN

      ENTRY GET_COAL_GROUPS_TABLES(R_COAL_GROUPS_TABLES)

         R_COAL_GROUPS_TABLES = SAVE_COAL_GROUPS_TABLES
      RETURN

      ENTRY GET_COAL_GROUPS_RECORDS(R_COAL_GROUPS_RECORDS)

         R_COAL_GROUPS_RECORDS = SAVE_COAL_GROUPS_RECORDS
      RETURN

      ENTRY OPEN_CS_FILE

         OPEN(UNIT_NUM,FILE=trim(OUTPUT_DIRECTORY())//CSGROUP_OL//
     +        "CSGROUP.BIN",ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      RETURN
!

      ENTRY GetCOALSupplyBaseName(R_TEMP_NAME)

         R_TEMP_NAME = SAVE_BASE_FILE_NAME
      RETURN

      ENTRY CLOSE_CS_FILE

         CLOSE(UNIT_NUM)
      RETURN

  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from coal_obj SIID56'
      call end_program(er_message)

 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END
!
!
!
!      

!
      FUNCTION MANAGE_COAL_SUPPLY_FORECASTS()
      use end_routine, only: end_program, er_message
!         

!
!
!

      use spindriftlib
      use prod_arrays_dimensions

      use globecom
      SAVE

      LOGICAL*1 SAVE_CS_FILE_EXISTS/.FALSE./
      INTEGER*4 VALUES_2_ZERO,
     +          GET_SUPPLY_CURVE_POINTS
      INTEGER*2 DELETE,CURRENT_RECORD,COAL_GROUP,GET_COAL_GROUPS,
     +          COAL_REGION,
     +          COAL_GROUPS_RECORDS,R_COAL_GROUP,
     +          R_ASSET_CLASS,CS,GLOBAL_CS,
     +          SAVE_COAL_GROUPS_RECORDS/0/,
     +          TEMP_I,I,HG,PA,Q,
     +          FIRST_COAL_SUP_REPORTING_GROUP,
     +          FIRST_REPORTING_GROUP/0/,
     +          R_MAX_S,
     +          R_FIRST_S,
     +          R_LAST_S,
     +          R_CS
      LOGICAL*1 MANAGE_COAL_SUPPLY_FORECASTS,COAL_GROUP_ACTIVE_SWITCH,
     +          MOD_COAL_GROUP_ACTIVE_SWITCH,
     +          GET_SCARCITY_INFO,GET_COAL_SPINNING_CAPACITY,
     +          GET_COAL_PRICE_CAPS,
     +          GET_OFF_PEAK_SPINNING_CAPACITY,
     +          GET_COAL_RAMP_RATES,
     +          GET_ONE_COAL_RAMP_RATES,
     +          GET_COAL_MAX_IMPORT_EXPORT,
     +          HYDRO_AGGREGATION/.FALSE./,
     +          GET_CS_PRICE_MULT,
     +          GET_ONE_CS_PRICE_MULT,
     +          ANNUAL_COAL_SUPPLY_CURVES, 
     +          MONTHLY_COAL_SUPPLY_CURVES,
     +          SCENARIO_COAL_SUPPLY_CURVES,
     +          WRITE_SUPPLY_CURVE_REPORT,
     +          GET_SUPPLY_COST_CAPACITY_CURVES,
     +          PUT_SUPPLY_CAPACITY_CURVES,
     +          put_monthly_lp_remaining_COAL,
     +          annual_lp_basin_COAL,
     +          FOUND_FIRST_S,
     +          DYNAMIC_EXTRACT_RATE(:)
      LOGICAL*1 CS_FILE_EXISTS
!      SAVE      COAL_GROUPS_DATA
      CHARACTER*20 R_GET_GROUP_NAME
      CHARACTER*3 HOURLY_PRICE_NAME(:)
      CHARACTER*3 R_GET_HOURLY_PRICE_NAME
      CHARACTER*20 R_GET_COAL_GROUP_ID_NAME
      CHARACTER*40 SCENARIO_VARIABLE
      LOGICAL*1 GET_TF_HOURLY_PRICE_NAME,GET_TF_GROUP_NAME,
     +          GET_TF_COAL_GROUP_ID_NAME,
     +          GET_TF_COAL_GROUP_NOX_SEASON
!
! SIMULATION VARIABLES
!
      CHARACTER*20 GROUP_NAME(:)
      CHARACTER (len=1), allocatable, save :: GROUP_ACTIVE(:),
     + SPINNING_UNITS(:),
     +            OFF_PEAK_SPINNING_UNITS(:),
     +            REPORT_CL_CAPACITY(:),TIME_ZONE(:),
     +            REPORT_THIS_GROUP(:),
     +            NOX_SEASON(:),
     +            PURCHASE_POWER_ASSIGN(:),
     +            CREATE_HOURLY_PRICE(:),
     +            SUPPLY_TYPE(:)
     
      character (len=1) :: R_GET_COAL_GROUP_NOX_SEASON,
     + R_PURCHASE_POWER_ASSIGN, DYNAMIC_EXTRACT_RATE_STR
     
      LOGICAL*1 GET_PURCHASE_POWER_ASSIGN
      LOGICAL*1   GET_REPORT_CL_CAPACITY,GET_REPORT_COAL_GROUP,
     +            GET_CREATE_HOURLY_PRICE,
     +            GET_ST_LHS_FOR_PRICES,
     +            GET_BASECASE_MARKET_AREA_ID,
     +            SUPPLY_GROUP_AVAILABLE(:)
      INTEGER*2   COAL_GROUP_ID(:),
     +            COAL_GROUP_ID_POSITION(:),
     +            INTERVAL_INDEX(:), ! ALLOCATABLE 
     +            FIRST_CS(:),
     +            GROUPS_PER_CS(:),
     +            POINTS_PER_CS(:),
     +            CS_GROUP_INDEX(:),
     +            TEMP_CS_SUPPLY_POSITION(:),
     +            CS_SUPPLY_POSITION(:,:),
     +            J,K,L,M,N,P,
     +            CS_SCENARIO_VARIABLE_INDEX(:),
     +            SCENARIO_INDEX,
     +            GET_SCENARIO_INDEX,
     +            HYDRO_GROUP_2_CS(:),
!     +            PLANNING_AREA_2_CS(:),
     +            COAL_GROUP_POSITION(:),
     +            CS_2_HYDRO_GROUP(:),
     +            CS_2_PLANNING_AREA(:),
     +            HYDRO_AGGREGATION_POSITION(:),
     +            HYDRO_AGGREGATION_INDEX(:),
     +            PLANNING_AREA_POSITION(:),
     +            PLANNING_AREA_INDEX(:),
     +            ASSET_CLASS_ID(:),
     +            ASSET_CLASS_REV_ALLOC_VECTOR(:),
     +            ASSET_CLASS_GROUPS_INDEX(:),
     +            ASSET_CLASS_2_CS(:),
     +            GET_ASSET_CLASS_2_CS,
     +            ASSET_CLASS_GROUP_2_AC(:), 
     +            RTO_GROUP(:),
     +            NOX_YEAR(:),
     +            END_NOX_YEAR(:),
     +            BASIN_START_YEAR(:),
     +            HYDRO_LOAD_AGGREGATION(:),
     +            CS_REGIONAL_PLANNING_AREA(:),
     +            GET_COAL_GROUP_NOX_YEAR,
     +            GET_COAL_GROUP_END_NOX_YEAR,
     +            MAX_ASSET_GROUPS,
     +            GET_NUM_OF_COAL_SUPPLY_REGIONS,
     +            NUMBER_OF_ACTIVE_REGIONS/0/,
     +            NUMBER_OF_HYDRO_GROUPS/0/,
     +            NUMBER_OF_PLANNING_GROUPS/0/,
     +            GET_NUMBER_OF_PLANNING_GROUPS,
     +            GET_NUMBER_OF_HYDRO_GROUPS,
     +            GET_HG_FROM_CS,
     +            GET_PA_FROM_CS,
     +            GET_PA_VALUE_FROM_CS,
     +            GET_NUMBER_OF_ACTIVE_GROUPS,
     +            MAX_COAL_GROUP_INDEX,GET_MAX_COAL_GROUPS,
     +            MAX_COAL_GROUP_NUMBER/0/,
     +            GET_COAL_GROUP_INDEX,
     +            GET_COAL_GROUP_POSITION,
     +            GET_MAX_COAL_GROUP_NUMBER,
     +            PURCHASE_ASSET_CLASS_ID(:),
     +            GET_PURCHASE_ASSET_CLASS_ID,
     +            PURCHASE_ASSET_ALLOC_VECTOR(:),
     +            GET_AC_FOR_CS,
     +            AC,MAX_ASSET_CLASS_GROUPS/0/,
     +            R_NUMBER_OF_COAL_GROUPS,
     +            COAL,R_MONTH,R_YEAR,
     +            R_ON_OR_OFF_PEAK,
     +            DAY_TYPE,DATA_BASE,GET_DATA_BASE_FOR_COAL,
     +            FUNCTION_HOLDER2,
     +            GET_PLANNING_AREA_POSITION,
     +            PLANNING_AREA,
     +            LOCAL_MONTH
      PARAMETER( MAX_COAL_GROUP_INDEX=2048,MAX_ASSET_GROUPS=2048)
      CHARACTER*6 BASECASE_MARKET_AREA_ID(:),
     +            BASE_CASE_COAL_AREA_ID(:),BASECASE_SUBREGION_ID(:),
     +            R_BASECASE_MARKET_AREA_ID
      CHARACTER*2 ST_LHS_FOR_PRICES(:),
     +            R_ST_LHS_FOR_PRICES
      INTEGER*2 BELONCS_TO_GROUP(MAX_COAL_GROUP_INDEX),
     +            R_ONE_PRICE_VARIABLE,COAL_ID
      INTEGER*4   MAX_POINTS_IN_CS,
     +            GET_MAX_POINTS_IN_CS,
     +            LOCAL_MAX_POINTS_IN_CS
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
     +       MAX_HOURLY_CS_IMPORT(:),
     +       MAX_HOURLY_CS_EXPORT(:),
     +       MIN_BASIN_COST(:),R_MIN_BASIN_COST,
     +       R_ANNUAL_PRODUCTION_TARGET,
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
      +       TEMP_CAPACITY,
     +       REAL4_ONE/1./,
     +       ESCALATED_MONTHLY_VALUE,
 
     +       R_ONE_PRICE_MULT,
     +       R_RAMP_ONE_UP,
     +       R_RAMP_ONE_DOWN,
     +       GET_SCENARIO_BY_INDEX,
     +       GET_COAL_GROUP_PEAK,
     +       GLOBAL_SCARCITY,
     +       GET_GLOBAL_SCARCITY,
     +       GET_MRX_VOLATILITY_MULT,
     +       GET_VAR,
     +       GET_OFF_PEAK_SPIN_FOR_CS,
     +       GET_COAL_SPIN_FOR_CS,
     +       TEMP_R,
     +       R_COST_CURVE,       ! (*),
     +       R_CAPACITY_CURVE,   ! (*),
     +       R_EXTRACT_CURVE,    !(*),
     +       R_EXTRACT_RATE,
     +       R_HARD_EXTRACT_RATE,
     +       R_MIN_EXTRACT_RATE,
     +       TEMP_SYSTEM_SUPPLY_CAPACITY_CURVE(:),
     +       R_SHRINKAGE_COST,
     +       R_SHRINKAGE_PERCENT,
     +       ANNUAL_MONTHLY_COST_ESC,
     +       ANNUAL_RESERVE_APPRECIATION,
     +       GET_SYSTEM_DAILY_EXTRACT_RATE,
     +       TOTAL_COAL_RESERVE,
     +       DAYS_REMAIN_IN_YEAR(12)/365.,334.,306.,275.,245.,214.,
     +                               184.,153.,122., 92., 61., 31./
      REAL*4
     +       OPERATING_COSTS(:,:),
     +       CAPACITY_VALUES(:,:),
     +       POTENTIAL_RESERVES(:),
     +       PROVEN_RESERVES(:),
     +       MAX_DAILY_EXTRACTION_RATE(:),
     +       MAX_DAILY_EXTRACTION_IN_MONTH(:),
     +       MAX_MONTHLY_EXTRACTION_RATE(:),
     +       SHRINKAGE_PERCENT(:),
     +       SHRINKAGE_COST(:),
     +       EXTRACTION_COST_ESC_RATE(:),
     +       RESERVE_APPRECIATION_PERCENT(:),
     +       MAX_DAILY_HARD_LIMIT(:),
     +       MIN_DAILY_LIMIT(:),
     +       CS_SUPPLY_COST_CURVE(:,:),
     +       CS_SUPPLY_EXTRACT_CURVE(:,:),
     +       TEMP_CS_SUPPLY_CAPACITY_CURVE(:),
     +       TEMP_CS_SUPPLY_COST_CURVE(:),
     +       CS_SUPPLY_CAPACITY_CURVE(:,:)
      REAL*8 COAL_GROUP_CAP
      REAL*8 INT_RES_REMAIN(:,:)
      CHARACTER*50 COMMENT
      ALLOCATABLE ::
     +     GROUP_NAME,
     +     GROUP_ACTIVE,
     +     SUPPLY_GROUP_AVAILABLE,
     +     COAL_GROUP_ID,
     +     COAL_GROUP_ID_POSITION,
     +     FIRST_CS,
     +     INTERVAL_INDEX,
     +     GROUPS_PER_CS,
     +     POINTS_PER_CS,
!     +     R_COST_CURVE,
!     +     R_CAPACITY_CURVE,
!     +     R_EXTRACT_CURVE,
     +     CS_SUPPLY_CAPACITY_CURVE,
     +     CS_SUPPLY_EXTRACT_CURVE,
     +     CS_SUPPLY_COST_CURVE,
     +     TEMP_CS_SUPPLY_CAPACITY_CURVE,
     +     TEMP_SYSTEM_SUPPLY_CAPACITY_CURVE,
     +     TEMP_CS_SUPPLY_COST_CURVE,
     +     TEMP_CS_SUPPLY_POSITION,
     +     CS_SUPPLY_POSITION,
     +     BASECASE_MARKET_AREA_ID,
     +     BASE_CASE_COAL_AREA_ID,
     +     BASECASE_SUBREGION_ID,
     +     SPINNING_UNITS,
     +     OFF_PEAK_SPINNING_UNITS,
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
     +     RTO_GROUP,
     +     NOX_YEAR,
     +     END_NOX_YEAR,
     +     BASIN_START_YEAR,
     +     ST_LHS_FOR_PRICES,
     +     MRX_VOLATILITY_MULT,
     +     NIGHT_SCARCITY_MULT,
     +     WEEKEND_SCARCITY_MULT,
     +     PRICE_CAP,
     +     MAX_HOURLY_CS_IMPORT,
     +     MAX_HOURLY_CS_EXPORT,
     +     HYDRO_LOAD_AGGREGATION,
     +     CS_REGIONAL_PLANNING_AREA,
     +     CAPACITY_ADDER,
     +     CS_GROUP_INDEX,
     +     CS_SCENARIO_VARIABLE_INDEX,
     +     HYDRO_GROUP_2_CS,
!     +     PLANNING_AREA_2_CS,
     +     COAL_GROUP_POSITION,
     +     CS_2_HYDRO_GROUP,
     +     CS_2_PLANNING_AREA,
     +     HYDRO_AGGREGATION_POSITION,
     +     HYDRO_AGGREGATION_INDEX,
     +     PLANNING_AREA_POSITION,
     +     PLANNING_AREA_INDEX,
     +     ASSET_CLASS_GROUPS_INDEX,
     +     ASSET_CLASS_2_CS,
     +     ASSET_CLASS_GROUP_2_AC, 
     +     REPORT_CL_CAPACITY,
     +     REPORT_THIS_GROUP,
     +     ASSET_CLASS_ID,
     +     ASSET_CLASS_REV_ALLOC_VECTOR,
     +     TIME_ZONE,
     +     NOX_SEASON,
     +     PURCHASE_POWER_ASSIGN,
     +     CREATE_HOURLY_PRICE,
     +     SUPPLY_TYPE,
     +     HOURLY_PRICE_NAME,
     +     PURCHASE_ASSET_CLASS_ID,
     +     PURCHASE_ASSET_ALLOC_VECTOR,
     +     OPERATING_COSTS,
     +     CAPACITY_VALUES,
     +     INT_RES_REMAIN,
     +     POTENTIAL_RESERVES,
     +     PROVEN_RESERVES,
     +     MAX_DAILY_EXTRACTION_RATE,
     +     MAX_DAILY_EXTRACTION_IN_MONTH,
     +     MAX_MONTHLY_EXTRACTION_RATE,
     +     SHRINKAGE_PERCENT,
     +     SHRINKAGE_COST,
     +     EXTRACTION_COST_ESC_RATE,
     +     RESERVE_APPRECIATION_PERCENT,
     +     MAX_DAILY_HARD_LIMIT,
     +     MIN_DAILY_LIMIT,
     +     DYNAMIC_EXTRACT_RATE,
     +     MIN_BASIN_COST
      REAL*4 R_COAL_CAP,R_COAL_MAX_CAP
      REAL*4 COAL_GROUP_SCARCITY_VALUE
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
     +       SCARCITY_CAP_PERCENT_3
      LOGICAL*1 VOID_LOGICAL,COAL_SUPPLY_GROUP_ACTIVE
!      STORE_CS_SCARCITY_INFO
      REAL*4 COAL_GLOBAL_SCARCITY_VALUE
      INTEGER VALUES_2_SET
!
! SPCapEx VARIABLES 11/18/06
!
      REAL (KIND=4) :: LOAD_PLANNING_FACTOR ! 58
      CHARACTER (LEN=20) :: LOAD_FILE_FOR  ! 59
      INTEGER (KIND=2) :: LOAD_SCENARIO_NUM ! 60
      CHARACTER (LEN=20) :: PRICE_FILE_FOR
      INTEGER (KIND=2) :: PRICE_SCENARIO_NUM
      REAL (KIND=4) :: ZONE_MAX_RESERVE
      REAL (KIND=4) :: ZONE_TARGET_RESERVE   ! 64
      CHARACTER (LEN=20) :: CAPEX_COMMENT  ! 65
      CHARACTER (LEN=20) :: MARKET_AREA_LOAD_FILE_FOR  ! 66
      INTEGER (KIND=2) :: MARKET_AREA_LOAD_SCENARIO_NUM ! 67
      CHARACTER (LEN=6) :: PRE_DISPATCH_HYDRO  ! 68
      CHARACTER (LEN=6) :: AGGREGATE_THERMAL   ! 69
      INTEGER (KIND=2) :: MAX_THERMAL_AGG_INTERVALS ! 70
      LOGICAL*1   FOUND_FIRST_INTERVAL
      INTEGER*2   MAX_UNIQUE_INTERVALS,
     +            NO_UNIQUE_INTERVALS
      REAL*4      TOLERANCE
      LOGICAL*1 YES_MONTHLY_SUPPLY_REPORTS,MONTHLY_COAL_SUPPLY_REPORT,
     +          COAL_SUPPLY_CURVE_REPORT_NOT_OPEN/.TRUE./,
     +          GET_GN_GROUP_NAME,TEMP_L
      INTEGER*2 VARIABLE_NUMBER,
     +          MONTHLY_COAL_SUPPLY_HEADER,
     +          MONTHLY_COAL_SUPPLY_UNIT,
     +          LAST_SEASON,PRODUCTION_PERIODS,ANNUAL_COUNTER/0/,
     +          LOCAL_YEAR,BASIN_YEAR
      REAL*4 DAYS_PER_MONTH(12)/31,28,31,30,31,30,31,31,30,31,30,31/
      INTEGER   COAL_SUPPLY_REC
      CHARACTER*9 CL_MONTH_NAME(13)/
     +      'January','February','March','April','May','June',
     +      'July','August','September','October',
     +      'November','December','Annual'/
      CHARACTER*30 TEMP_NODE_NAME     
      CHARACTER*30 TEMP_NAME
      CHARACTER*30 TEMP_INTERVAL_NUM
      CHARACTER* 256 TEMP_STR

         MANAGE_COAL_SUPPLY_FORECASTS = .FALSE.
         SAVE_CS_FILE_EXISTS = .FALSE.
!         
!
         CALL DOES_CS_FILE_EXIST(CS_FILE_EXISTS)


         IF(.NOT. CS_FILE_EXISTS) RETURN

         IF(ALLOCATED(CS_GROUP_INDEX)) 
     +               DEALLOCATE(CS_GROUP_INDEX,
     +                     CS_SCENARIO_VARIABLE_INDEX,
     +                     HYDRO_GROUP_2_CS,
     +                     COAL_GROUP_POSITION,
     +                     CS_2_HYDRO_GROUP,
     +                     CS_2_PLANNING_AREA,
     +                     HYDRO_AGGREGATION_POSITION,
     +                     HYDRO_AGGREGATION_INDEX,
     +                     PLANNING_AREA_POSITION,
     +                     PLANNING_AREA_INDEX,
     +                     ASSET_CLASS_GROUPS_INDEX,
     +                     ASSET_CLASS_2_CS,
     +                     ASSET_CLASS_GROUP_2_AC,
     +                     REPORT_THIS_GROUP,
     +                     SUPPLY_GROUP_AVAILABLE)
         ALLOCATE(SUPPLY_GROUP_AVAILABLE(MAX_COAL_GROUP_INDEX))
         ALLOCATE(CS_GROUP_INDEX(MAX_COAL_GROUP_INDEX))
         ALLOCATE(CS_SCENARIO_VARIABLE_INDEX(MAX_COAL_GROUP_INDEX))
         ALLOCATE(HYDRO_GROUP_2_CS(MAX_COAL_GROUP_INDEX))
         ALLOCATE(COAL_GROUP_POSITION(MAX_COAL_GROUP_INDEX))
         ALLOCATE(REPORT_THIS_GROUP(MAX_COAL_GROUP_INDEX))
         ALLOCATE(CS_2_HYDRO_GROUP(MAX_COAL_GROUP_INDEX))
         ALLOCATE(CS_2_PLANNING_AREA(MAX_COAL_GROUP_INDEX))
         ALLOCATE(HYDRO_AGGREGATION_POSITION(MAX_COAL_GROUP_INDEX))
         ALLOCATE(HYDRO_AGGREGATION_INDEX(MAX_COAL_GROUP_INDEX))
         ALLOCATE(PLANNING_AREA_POSITION(MAX_COAL_GROUP_INDEX))
         ALLOCATE(PLANNING_AREA_INDEX(MAX_COAL_GROUP_INDEX))
         ALLOCATE(ASSET_CLASS_GROUPS_INDEX(0:MAX_ASSET_GROUPS))
         ALLOCATE(ASSET_CLASS_2_CS(0:MAX_ASSET_GROUPS))
         ALLOCATE(ASSET_CLASS_GROUP_2_AC(MAX_COAL_GROUP_INDEX))

         CS_GROUP_INDEX = 0
         CS_SCENARIO_VARIABLE_INDEX = 0
         HYDRO_GROUP_2_CS = 0
         COAL_GROUP_POSITION = 0
         CS_2_HYDRO_GROUP = 0
         CS_2_PLANNING_AREA = 0
         HYDRO_AGGREGATION_POSITION = 0
         HYDRO_AGGREGATION_INDEX = 0
         PLANNING_AREA_POSITION = 0
         PLANNING_AREA_INDEX = 0
         ASSET_CLASS_GROUPS_INDEX = 0
         ASSET_CLASS_2_CS = 0
         ASSET_CLASS_GROUP_2_AC = 0
!
         SUPPLY_GROUP_AVAILABLE = .FALSE.
!
         CALL GET_COAL_GROUPS_RECORDS(COAL_GROUPS_RECORDS)
         SAVE_COAL_GROUPS_RECORDS = COAL_GROUPS_RECORDS
!
         CALL OPEN_CS_FILE
!
         IF(ALLOCATED(GROUP_NAME)) DEALLOCATE(
     +                                 GROUP_NAME,
     +                                 GROUP_ACTIVE,
     +                                 COAL_GROUP_ID,
     +                                 COAL_GROUP_ID_POSITION,
     +                                 BASECASE_MARKET_AREA_ID,
     +                                 BASE_CASE_COAL_AREA_ID,
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
     +                                 ADDITIONAL_CAPACITY_PERCENT)
         IF(ALLOCATED(RTO_GROUP)) DEALLOCATE(
     +                                 RTO_GROUP,
     +                                 NOX_YEAR,
     +                                 END_NOX_YEAR,
     +                                 BASIN_START_YEAR,
     +                                 ST_LHS_FOR_PRICES,
     +                                 MRX_VOLATILITY_MULT,
     +                                 REPORT_CL_CAPACITY,
     +                                 ASSET_CLASS_ID,
     +                                 ASSET_CLASS_REV_ALLOC_VECTOR,
     +                                 TIME_ZONE,
     +                                 CAPACITY_ADDER,
     +                                 NOX_SEASON,
     +                                 NIGHT_SCARCITY_MULT,
     +                                 WEEKEND_SCARCITY_MULT,
     +                                 PRICE_CAP,
     +                                 MAX_HOURLY_CS_IMPORT,
     +                                 MAX_HOURLY_CS_EXPORT,
     +                                 HYDRO_LOAD_AGGREGATION,
     +                                 CS_REGIONAL_PLANNING_AREA,
     +                                 PURCHASE_POWER_ASSIGN,
     +                                 CREATE_HOURLY_PRICE,
     +                                 SUPPLY_TYPE,
     +                                 HOURLY_PRICE_NAME,
     +                                 PURCHASE_ASSET_CLASS_ID,
     +                                 PURCHASE_ASSET_ALLOC_VECTOR,
     +                                 OPERATING_COSTS,
     +                                 CAPACITY_VALUES)
         IF(ALLOCATED(INT_RES_REMAIN)) 
     +                      DEALLOCATE(INT_RES_REMAIN,
     +                                 POTENTIAL_RESERVES,
     +                                 PROVEN_RESERVES,
     +                                 MAX_DAILY_EXTRACTION_RATE)
         IF(ALLOCATED(MAX_DAILY_EXTRACTION_IN_MONTH)) DEALLOCATE(
     +                                 MAX_DAILY_EXTRACTION_IN_MONTH)
         IF(ALLOCATED(MAX_MONTHLY_EXTRACTION_RATE)) DEALLOCATE(
     +                                 MAX_MONTHLY_EXTRACTION_RATE,
     +                                 SHRINKAGE_PERCENT,
     +                                 SHRINKAGE_COST,
     +                                 EXTRACTION_COST_ESC_RATE,
     +                                 RESERVE_APPRECIATION_PERCENT,
     +                                 MAX_DAILY_HARD_LIMIT,
     +                                 MIN_DAILY_LIMIT,
     +                                 DYNAMIC_EXTRACT_RATE,
     +                                 MIN_BASIN_COST)
         ALLOCATE(GROUP_NAME(COAL_GROUPS_RECORDS))
         ALLOCATE(GROUP_ACTIVE(COAL_GROUPS_RECORDS))
         ALLOCATE(COAL_GROUP_ID(MAX_COAL_GROUP_INDEX))
         ALLOCATE(COAL_GROUP_ID_POSITION(0:COAL_GROUPS_RECORDS))
         ALLOCATE(BASECASE_MARKET_AREA_ID(COAL_GROUPS_RECORDS))
         ALLOCATE(BASE_CASE_COAL_AREA_ID(COAL_GROUPS_RECORDS))
         ALLOCATE(BASECASE_SUBREGION_ID(COAL_GROUPS_RECORDS))
         ALLOCATE(SPINNING_UNITS(COAL_GROUPS_RECORDS))
         ALLOCATE(OFF_PEAK_SPINNING_UNITS(COAL_GROUPS_RECORDS))
         ALLOCATE(SPINNING_RESERVE(COAL_GROUPS_RECORDS))
         ALLOCATE(OFF_PEAK_SPINNING_RESERVE(COAL_GROUPS_RECORDS))
         ALLOCATE(MAX_HOURLY_RAMP_UP(COAL_GROUPS_RECORDS))
         ALLOCATE(MAX_HOURLY_RAMP_DOWN(COAL_GROUPS_RECORDS))
         ALLOCATE(FIRST_CAPACITY_VALUE(0:COAL_GROUPS_RECORDS))
         ALLOCATE(FIRST_CAPACITY_PERCENT(0:COAL_GROUPS_RECORDS))
         ALLOCATE(SECOND_CAPACITY_VALUE(0:COAL_GROUPS_RECORDS))
         ALLOCATE(SECOND_CAPACITY_PERCENT(0:COAL_GROUPS_RECORDS))
         ALLOCATE(THIRD_CAPACITY_VALUE(0:COAL_GROUPS_RECORDS))
         ALLOCATE(THIRD_CAPACITY_PERCENT(0:COAL_GROUPS_RECORDS))
         ALLOCATE(ADDITIONAL_CAPACITY_VALUE(0:COAL_GROUPS_RECORDS,7))
         ALLOCATE(ADDITIONAL_CAPACITY_PERCENT(0:COAL_GROUPS_RECORDS,7))
         ALLOCATE(RTO_GROUP(COAL_GROUPS_RECORDS))
         ALLOCATE(NOX_YEAR(COAL_GROUPS_RECORDS))
         ALLOCATE(END_NOX_YEAR(COAL_GROUPS_RECORDS))
         ALLOCATE(BASIN_START_YEAR(COAL_GROUPS_RECORDS))
         ALLOCATE(ST_LHS_FOR_PRICES(COAL_GROUPS_RECORDS))
         ALLOCATE(MRX_VOLATILITY_MULT(COAL_GROUPS_RECORDS))
         ALLOCATE(NIGHT_SCARCITY_MULT(COAL_GROUPS_RECORDS))
         ALLOCATE(WEEKEND_SCARCITY_MULT(COAL_GROUPS_RECORDS))
         ALLOCATE(PRICE_CAP(COAL_GROUPS_RECORDS))
         ALLOCATE(MAX_HOURLY_CS_IMPORT(COAL_GROUPS_RECORDS))
         ALLOCATE(MAX_HOURLY_CS_EXPORT(COAL_GROUPS_RECORDS))
         ALLOCATE(HYDRO_LOAD_AGGREGATION(COAL_GROUPS_RECORDS))
         ALLOCATE(CS_REGIONAL_PLANNING_AREA(COAL_GROUPS_RECORDS))
         ALLOCATE(REPORT_CL_CAPACITY(COAL_GROUPS_RECORDS))
         ALLOCATE(ASSET_CLASS_ID(COAL_GROUPS_RECORDS))
         ALLOCATE(ASSET_CLASS_REV_ALLOC_VECTOR(COAL_GROUPS_RECORDS))
         ALLOCATE(TIME_ZONE(COAL_GROUPS_RECORDS))
         ALLOCATE(CAPACITY_ADDER(COAL_GROUPS_RECORDS))
         ALLOCATE(NOX_SEASON(COAL_GROUPS_RECORDS))
         ALLOCATE(PURCHASE_POWER_ASSIGN(COAL_GROUPS_RECORDS))
         ALLOCATE(CREATE_HOURLY_PRICE(COAL_GROUPS_RECORDS))
         ALLOCATE(SUPPLY_TYPE(COAL_GROUPS_RECORDS))
         ALLOCATE(HOURLY_PRICE_NAME(COAL_GROUPS_RECORDS))
         ALLOCATE(PURCHASE_ASSET_CLASS_ID(COAL_GROUPS_RECORDS))
         ALLOCATE(PURCHASE_ASSET_ALLOC_VECTOR(COAL_GROUPS_RECORDS))
         ALLOCATE(OPERATING_COSTS(30,COAL_GROUPS_RECORDS)) ! (25,COAL_GROUPS_RECORDS))
         ALLOCATE(CAPACITY_VALUES(30,COAL_GROUPS_RECORDS)) ! (25,COAL_GROUPS_RECORDS))
         ALLOCATE(INT_RES_REMAIN(30,COAL_GROUPS_RECORDS)) ! (25,COAL_GROUPS_RECORDS))
         ALLOCATE(POTENTIAL_RESERVES(COAL_GROUPS_RECORDS))
         ALLOCATE(PROVEN_RESERVES(COAL_GROUPS_RECORDS))
         ALLOCATE(MAX_DAILY_EXTRACTION_RATE(COAL_GROUPS_RECORDS))
         ALLOCATE(MAX_DAILY_EXTRACTION_IN_MONTH(COAL_GROUPS_RECORDS))
         ALLOCATE(MAX_MONTHLY_EXTRACTION_RATE(COAL_GROUPS_RECORDS))
         ALLOCATE(SHRINKAGE_PERCENT(COAL_GROUPS_RECORDS))
         ALLOCATE(SHRINKAGE_COST(COAL_GROUPS_RECORDS)) 
         ALLOCATE(EXTRACTION_COST_ESC_RATE(COAL_GROUPS_RECORDS))
         ALLOCATE(RESERVE_APPRECIATION_PERCENT(COAL_GROUPS_RECORDS))
         ALLOCATE(MAX_DAILY_HARD_LIMIT(COAL_GROUPS_RECORDS))
         ALLOCATE(MIN_DAILY_LIMIT(COAL_GROUPS_RECORDS))
         ALLOCATE(DYNAMIC_EXTRACT_RATE(COAL_GROUPS_RECORDS))
         ALLOCATE(MIN_BASIN_COST(COAL_GROUPS_RECORDS))
! MOVED 5/8/00
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
         CAPACITY_VALUES = 0.
         OPERATING_COSTS = 0.
         INT_RES_REMAIN = 0.
!
         BELONCS_TO_GROUP = 0
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

         COAL_GROUP = 1 ! NUMBER OF SUPPLY GROUPS ACROSS ALL REGIONS
         COAL_REGION = 1 ! COMPOSED OF COAL GROUPS
         NUMBER_OF_ACTIVE_REGIONS = 0
         NUMBER_OF_HYDRO_GROUPS = 0
         NUMBER_OF_PLANNING_GROUPS = 0
         MAX_COAL_GROUP_NUMBER = 0
         MAX_ASSET_CLASS_GROUPS = 0
         HYDRO_AGGREGATION = .FALSE.
!         PLANNING_AREA = .FALSE.
         FIRST_REPORTING_GROUP = 0
!
         DO CURRENT_RECORD = 1, COAL_GROUPS_RECORDS
            READ(10,REC=CURRENT_RECORD) DELETE,
     +                          GROUP_NAME(COAL_GROUP),                ! 1
     +                          GROUP_ACTIVE(COAL_GROUP),
     +                          COAL_GROUP_ID(COAL_GROUP),
     +                          BASECASE_MARKET_AREA_ID(COAL_GROUP),
     +                          BASE_CASE_COAL_AREA_ID(COAL_GROUP),
     +                          BASECASE_SUBREGION_ID(COAL_GROUP),
     +                          SPINNING_UNITS(COAL_GROUP),
     +                          SPINNING_RESERVE(COAL_GROUP),
     +                          COMMENT,
     +                          MAX_HOURLY_RAMP_UP(COAL_GROUP),        ! 10
     +                          MAX_HOURLY_RAMP_DOWN(COAL_GROUP),      ! 11
     +                          CAPACITY_VALUES(1,COAL_GROUP),      ! 12
     +                          INT_RES_REMAIN(1,COAL_GROUP),    ! 13
     +                          CAPACITY_VALUES(2,COAL_GROUP),     ! 14
     +                          INT_RES_REMAIN(2,COAL_GROUP),   ! 15
     +                          CAPACITY_VALUES(3,COAL_GROUP),      ! 16
     +                          INT_RES_REMAIN(3,COAL_GROUP),    ! 17
     +                          REPORT_CL_CAPACITY(COAL_GROUP),
     +                          ASSET_CLASS_ID(COAL_GROUP),
     +                         ASSET_CLASS_REV_ALLOC_VECTOR(COAL_GROUP), ! 20
     +                          TIME_ZONE(COAL_GROUP),
     +                          POTENTIAL_RESERVES(COAL_GROUP),           ! 22
     +                          NOX_SEASON(COAL_GROUP),
     +                          PURCHASE_POWER_ASSIGN(COAL_GROUP),
     +                          PURCHASE_ASSET_CLASS_ID(COAL_GROUP),
     +                          PURCHASE_ASSET_ALLOC_VECTOR(COAL_GROUP),
     +                          CREATE_HOURLY_PRICE(COAL_GROUP),
     +                          HOURLY_PRICE_NAME(COAL_GROUP),
     +                          CAPACITY_VALUES(4:10,COAL_GROUP),    ! 29-35
     +                          INT_RES_REMAIN(4:10,COAL_GROUP),  ! 36-42
     +                          RTO_GROUP(COAL_GROUP),
     +                          MRX_VOLATILITY_MULT(COAL_GROUP),
     +                          NOX_YEAR(COAL_GROUP),
     +                          NIGHT_SCARCITY_MULT(COAL_GROUP),          ! 46
     +                          PROVEN_RESERVES(COAL_GROUP),              ! 47
     +                          HYDRO_LOAD_AGGREGATION(COAL_GROUP),
     +                          OFF_PEAK_SPINNING_RESERVE(COAL_GROUP),
     +                          OFF_PEAK_SPINNING_UNITS(COAL_GROUP),      ! 50
     +                          PRICE_CAP(COAL_GROUP),                    ! 51
     +                          MAX_DAILY_EXTRACTION_RATE(COAL_GROUP),    ! 52
     +                          MAX_MONTHLY_EXTRACTION_RATE(COAL_GROUP),
     +                          PLANNING_AREA,
     +                          SCENARIO_VARIABLE,
     +                          END_NOX_YEAR(COAL_GROUP),
     +                          ST_LHS_FOR_PRICES(COAL_GROUP),
! SPCapEx Variables place holders before any new SP variables
     +                          LOAD_PLANNING_FACTOR,         ! 58
     +                          LOAD_FILE_FOR,                ! 59
     +                          LOAD_SCENARIO_NUM,            ! 60
     +                          PRICE_FILE_FOR,
     +                          PRICE_SCENARIO_NUM,
     +                          ZONE_MAX_RESERVE,
     +                          ZONE_TARGET_RESERVE,   ! 64
     +                          CAPEX_COMMENT,  ! 65
     +                          MARKET_AREA_LOAD_FILE_FOR,  ! 66
     +                          MARKET_AREA_LOAD_SCENARIO_NUM, ! 67
     +                          PRE_DISPATCH_HYDRO,  ! 68
     +                          AGGREGATE_THERMAL,   ! 69
     +                          MAX_THERMAL_AGG_INTERVALS,    ! 70
! 070806.     
     +                          OPERATING_COSTS(1,COAL_GROUP),         ! 71-80
     +                          OPERATING_COSTS(2,COAL_GROUP),         ! 71-80
     +                          OPERATING_COSTS(3,COAL_GROUP),         ! 71-80
     +                          OPERATING_COSTS(4,COAL_GROUP),         ! 71-80
     +                          OPERATING_COSTS(5,COAL_GROUP),         ! 71-80
     +                          OPERATING_COSTS(6,COAL_GROUP),         ! 71-80
     +                          OPERATING_COSTS(7,COAL_GROUP),         ! 71-80
     +                          OPERATING_COSTS(8,COAL_GROUP),         ! 71-80
     +                          OPERATING_COSTS(9,COAL_GROUP),         ! 71-80
     +                          OPERATING_COSTS(10,COAL_GROUP),         ! 71-80
     +                          CAPACITY_VALUES(11:25,COAL_GROUP),              ! 81-95
     +                          OPERATING_COSTS(11:25,COAL_GROUP),        ! 96-110
     +                          INT_RES_REMAIN(11:25,COAL_GROUP),         ! 111-125     
     +                          SHRINKAGE_PERCENT(COAL_GROUP),            ! 126
     +                          SHRINKAGE_COST(COAL_GROUP),                ! 127 
     +                          SUPPLY_TYPE(COAL_GROUP), ! C*1
     +                          EXTRACTION_COST_ESC_RATE(COAL_GROUP), ! R*4
     +                          BASIN_START_YEAR(COAL_GROUP), ! I*2
     +                         RESERVE_APPRECIATION_PERCENT(COAL_GROUP),
     +                          MAX_DAILY_HARD_LIMIT(COAL_GROUP),
     +                          MIN_DAILY_LIMIT(COAL_GROUP),
     +                          DYNAMIC_EXTRACT_RATE_STR,
     +                          MIN_BASIN_COST(COAL_GROUP)
!
            COAL_ID = COAL_GROUP_ID(COAL_GROUP)
!
            IF(GROUP_ACTIVE(COAL_GROUP) == 'F' .OR. 
     +                      .NOT. COAL_GROUP_ACTIVE_SWITCH(COAL_ID)) CYCLE
!
! 072306. MAKE INT_RES_REMAIN INTERVAL RESERVE REMAINING FOR A MORE NATURAL
!         TREATMENT IN THE COAL MODEL.
!
            CAPACITY_VALUES(26,COAL_GROUP) = 
     +                                    CAPACITY_VALUES(25,COAL_GROUP)
            CAPACITY_VALUES(27,COAL_GROUP) = 
     +                                    CAPACITY_VALUES(25,COAL_GROUP)
            CAPACITY_VALUES(28,COAL_GROUP) = 
     +                                    CAPACITY_VALUES(25,COAL_GROUP)
            CAPACITY_VALUES(29,COAL_GROUP) = 
     +                                    CAPACITY_VALUES(25,COAL_GROUP)
            CAPACITY_VALUES(30,COAL_GROUP) = 
     +                                    CAPACITY_VALUES(25,COAL_GROUP)
            OPERATING_COSTS(26,COAL_GROUP) = 
     +                                    OPERATING_COSTS(25,COAL_GROUP)
            OPERATING_COSTS(27,COAL_GROUP) = 
     +                                    OPERATING_COSTS(25,COAL_GROUP)
            OPERATING_COSTS(28,COAL_GROUP) = 
     +                                    OPERATING_COSTS(25,COAL_GROUP)
            OPERATING_COSTS(29,COAL_GROUP) = 
     +                                    OPERATING_COSTS(25,COAL_GROUP)
            OPERATING_COSTS(30,COAL_GROUP) = 
     +                                    OPERATING_COSTS(25,COAL_GROUP)
            INT_RES_REMAIN(26,COAL_GROUP) =      
     +                                     INT_RES_REMAIN(25,COAL_GROUP)
            INT_RES_REMAIN(27,COAL_GROUP) =      
     +                                     INT_RES_REMAIN(25,COAL_GROUP)
            INT_RES_REMAIN(28,COAL_GROUP) =      
     +                                     INT_RES_REMAIN(25,COAL_GROUP)
            INT_RES_REMAIN(29,COAL_GROUP) =      
     +                                     INT_RES_REMAIN(25,COAL_GROUP)
            INT_RES_REMAIN(30,COAL_GROUP) =      
     +                                     INT_RES_REMAIN(25,COAL_GROUP)
!            
            DO I = 25, 2,-1
               IF(ABS(INT_RES_REMAIN(I,COAL_GROUP)) < 0.0001) CYCLE
               INT_RES_REMAIN(I,COAL_GROUP) = MAX(0., 
     +                  INT_RES_REMAIN(I,COAL_GROUP) - 
     +                                   INT_RES_REMAIN(I-1,COAL_GROUP))


            END DO            
!            
            INT_RES_REMAIN(26,COAL_GROUP) =      
     +                                     INT_RES_REMAIN(25,COAL_GROUP)
            INT_RES_REMAIN(27,COAL_GROUP) =      
     +                                     INT_RES_REMAIN(25,COAL_GROUP)
            INT_RES_REMAIN(28,COAL_GROUP) =      
     +                                     INT_RES_REMAIN(25,COAL_GROUP)
            INT_RES_REMAIN(29,COAL_GROUP) =      
     +                                     INT_RES_REMAIN(25,COAL_GROUP)
            INT_RES_REMAIN(30,COAL_GROUP) =      
     +                                     INT_RES_REMAIN(25,COAL_GROUP)
! ORIGINAL ID.
!
            DYNAMIC_EXTRACT_RATE(COAL_GROUP) = 
     +                                   DYNAMIC_EXTRACT_RATE_STR == 'T'
!
            SUPPLY_GROUP_AVAILABLE(COAL_ID) = .TRUE.
!
! 10/03/02. REGIONAL CONSOLIDATION FOR BURESH.
!
            IF(COAL_GROUP_ID(COAL_GROUP) < 0) THEN
               DO I = 1, 30
                  TEMP_I = INT2(
     +                 GET_VAR(FLOAT(COAL_GROUP_ID(COAL_GROUP)),I,
     +                                          "COAL_SUPPLY Group   "))
!
                  IF(TEMP_I == 0) EXIT
!
                  BELONCS_TO_GROUP(TEMP_I) = 
     +                               ABS(COAL_GROUP_ID(COAL_GROUP))
               ENDDO
               COAL_GROUP_ID(COAL_GROUP) = 
     +                               ABS(COAL_GROUP_ID(COAL_GROUP))
            ELSE
               BELONCS_TO_GROUP(COAL_GROUP_ID(COAL_GROUP)) = 
     +                                    COAL_GROUP_ID(COAL_GROUP)
            ENDIF
!
            IF(MAX_HOURLY_RAMP_UP(COAL_GROUP) == 0. .AND.
     +                     MAX_HOURLY_RAMP_DOWN(COAL_GROUP) == 0.) THEN
! ASSUME THAT THEY JUST ASSIGNED A ZERO DEFAULT
               MAX_HOURLY_RAMP_UP(COAL_GROUP) = 9999999.
               MAX_HOURLY_RAMP_DOWN(COAL_GROUP) = 9999999.            
            ENDIF
!
! OFF-PEAK INHERITS PEAK VALUES
!            
            IF(OFF_PEAK_SPINNING_RESERVE(COAL_GROUP) == -99999.) THEN
               OFF_PEAK_SPINNING_RESERVE(COAL_GROUP) =
     +                                     SPINNING_RESERVE(COAL_GROUP)
            ENDIF
!            
            IF(OFF_PEAK_SPINNING_UNITS(COAL_GROUP) == 'Z') THEN
               OFF_PEAK_SPINNING_UNITS(COAL_GROUP) =
     +                                       SPINNING_UNITS(COAL_GROUP)
            ENDIF
            IF(COAL_GROUP_POSITION(COAL_GROUP_ID(COAL_GROUP)) == 
     +                                                           0) THEN
               NUMBER_OF_ACTIVE_REGIONS = NUMBER_OF_ACTIVE_REGIONS + 1
               MAX_COAL_GROUP_NUMBER = MIN(MAX_COAL_GROUP_INDEX,
     +                                  MAX(MAX_COAL_GROUP_NUMBER,
     +                                  COAL_GROUP_ID(COAL_GROUP)))
               CS_GROUP_INDEX(NUMBER_OF_ACTIVE_REGIONS) =
     +                                    COAL_GROUP_ID(COAL_GROUP)
               COAL_GROUP_POSITION(COAL_GROUP_ID(COAL_GROUP)) =
     +                                          NUMBER_OF_ACTIVE_REGIONS
!
               REPORT_THIS_GROUP(NUMBER_OF_ACTIVE_REGIONS) =
     +                                   REPORT_CL_CAPACITY(COAL_GROUP)
!
               IF(FIRST_REPORTING_GROUP == 0 .AND. 
     +                      REPORT_CL_CAPACITY(COAL_GROUP) == 'T') THEN
                  FIRST_REPORTING_GROUP = NUMBER_OF_ACTIVE_REGIONS
               ENDIF
!
! 11/20/02.
!               
               CS_SCENARIO_VARIABLE_INDEX(NUMBER_OF_ACTIVE_REGIONS) = 
     +                             GET_SCENARIO_INDEX(SCENARIO_VARIABLE)
!
! 03/26/03. HARD-WIRED TO AVOID INADERTANT ACTIVATION (E.G. LGE)
!
               HYDRO_LOAD_AGGREGATION(COAL_GROUP) = 0 
!
               IF(HYDRO_LOAD_AGGREGATION(COAL_GROUP) == 0) THEN
!

                  CS_2_HYDRO_GROUP(NUMBER_OF_ACTIVE_REGIONS) = 0
!
! EMPLOY LOAD AGGREGATION TO A NEW GROUP.
!
               ELSEIF( HYDRO_AGGREGATION_POSITION(
     +                   HYDRO_LOAD_AGGREGATION(COAL_GROUP)) == 0) THEN
!     
                  HYDRO_AGGREGATION = .TRUE.
!                  
                  NUMBER_OF_HYDRO_GROUPS = NUMBER_OF_HYDRO_GROUPS + 1
!
                  HYDRO_GROUP_2_CS(NUMBER_OF_HYDRO_GROUPS) =
     +                                          NUMBER_OF_ACTIVE_REGIONS
                  CS_2_HYDRO_GROUP(NUMBER_OF_ACTIVE_REGIONS) =
     +                                            NUMBER_OF_HYDRO_GROUPS
!
                  HYDRO_AGGREGATION_POSITION(
     +                  HYDRO_LOAD_AGGREGATION(COAL_GROUP)) = 
     +                                            NUMBER_OF_HYDRO_GROUPS
                  HYDRO_AGGREGATION_INDEX(NUMBER_OF_HYDRO_GROUPS) = 
     +                               HYDRO_LOAD_AGGREGATION(COAL_GROUP)
!
! EMPLOY LOAD AGGREGATION TO A GROUP THAT HAS BEEN PREVIOUSLY USED.
!
               ELSE 
                  DO HG = 1, NUMBER_OF_HYDRO_GROUPS
                     IF(HYDRO_LOAD_AGGREGATION(COAL_GROUP) /= 
     +                                HYDRO_AGGREGATION_INDEX(HG)) CYCLE
                     CS_2_HYDRO_GROUP(NUMBER_OF_ACTIVE_REGIONS) = HG
                     HYDRO_GROUP_2_CS(HG) = NUMBER_OF_ACTIVE_REGIONS
                     EXIT
                  ENDDO
               ENDIF
!            
!
!              ADDED 10/7/98. GAT. TO CAPTURE COAL_SUP COALMISSION 
!                       REVENUES/EXPENSES FOR ASSET ANALYST.
!
               AC = ASSET_CLASS_ID(COAL_GROUP)
               IF(ASSET_CLASS_GROUPS_INDEX(AC) == 0) THEN
                  MAX_ASSET_CLASS_GROUPS = MAX_ASSET_CLASS_GROUPS + 1
                  ASSET_CLASS_GROUPS_INDEX(AC) = MAX_ASSET_CLASS_GROUPS
                  ASSET_CLASS_GROUP_2_AC(MAX_ASSET_CLASS_GROUPS) = AC
!                 
! TEMP. 12/27/01.  NOTE RE-ASSIGNMENT.
!
                  AC = PURCHASE_ASSET_CLASS_ID(COAL_GROUP)
                  ASSET_CLASS_2_CS(AC) = NUMBER_OF_ACTIVE_REGIONS
               ELSE
                  WRITE(4,*) "Duplicate Asset Class Detected in"
                  WRITE(4,*) "the COAL_SUPPLY Group file."
                  WRITE(4,*) "COAL_SUPPLY Group Name = ",
     +                                           GROUP_NAME(COAL_GROUP)
                  WRITE(4,*) "Asset Class Id = ",
     +                                       ASSET_CLASS_ID(COAL_GROUP)
                  WRITE(4,*) "Renumber or accumulate groups."
                  WRITE(4,*) " "
               ENDIF
!
!               COAL_GROUP = COAL_GROUP + 1
               COAL_REGION = COAL_REGION + 1
!

            ENDIF
            CS = NUMBER_OF_ACTIVE_REGIONS
!
!  072106. BIG DIMENSIONING CHANGE: COAL_GROUPS AGGREGATE TO COAL_REGIONS
!            
            COAL_GROUP = COAL_GROUP + 1
!            

         ENDDO ! COAL GROUPS RECORDS
!
         COAL_GROUP = COAL_GROUP - 1
!            
         CALL CLOSE_CS_FILE

!
! COAL_GROUP_SORT AND STARTING POSITION OF EACH GROUP
!         
         IF(NUMBER_OF_ACTIVE_REGIONS > 0) THEN
            IF(ALLOCATED(FIRST_CS)) 
     +            DEALLOCATE( FIRST_CS,
     +                        GROUPS_PER_CS,
     +                        POINTS_PER_CS)
            ALLOCATE(FIRST_CS(CS),
     +               GROUPS_PER_CS(CS),
     +               POINTS_PER_CS(CS))
            FIRST_CS = 0
            GROUPS_PER_CS = 0
            POINTS_PER_CS = 0
            COAL_GROUP_ID_POSITION = 0
            K = 1
            DO I = 1, COAL_GROUP ! CS INDEX
               L = CS_GROUP_INDEX(I)
               DO J = 1, COAL_GROUP ! FIRST_CS
                  IF(COAL_GROUP_ID(J) == L) THEN
                     COAL_GROUP_ID_POSITION(K) = J
!
                     DO M = 1, 30 ! 25
                        IF(INT_RES_REMAIN(M,J) < 0.000001) EXIT
                        POINTS_PER_CS(I) = POINTS_PER_CS(I) +1
                     ENDDO
!                     
                     GROUPS_PER_CS(I) = GROUPS_PER_CS(I) + 1
                     IF(K == 1) THEN ! 012308. ADDED
                        FIRST_CS(I) = K
                     ELSEIF(COAL_GROUP_ID(COAL_GROUP_ID_POSITION(K)) /= 
     +                         COAL_GROUP_ID(
     +                                COAL_GROUP_ID_POSITION(K-1))) THEN
                        FIRST_CS(I) = K
                     ENDIF
                     K = K + 1
                  ENDIF
               END DO 
            END DO
         ENDIF
         GLOBAL_CS = CS         
!
!      RETURN
! CURRENTLY ONLY CALLED ONCE.
!
! 101006. BIG REDEFINITION OF CS_ ARRAYS AND CREATE INITIAL ARRAYS 
!         AND MONTHLY CALLS TO SUPPLY CURVES.
!
! THIS NEEDS TO BE IDENTIFIED ABOVE. IT IS WAY TOO BIG.
!
         MAX_POINTS_IN_CS = MAXVAL(POINTS_PER_CS)
!         
         IF(ALLOCATED(TEMP_CS_SUPPLY_CAPACITY_CURVE))
     +      DEALLOCATE(TEMP_CS_SUPPLY_CAPACITY_CURVE,
     +                  TEMP_CS_SUPPLY_COST_CURVE,
     +                  TEMP_CS_SUPPLY_POSITION,
     +                  TEMP_SYSTEM_SUPPLY_CAPACITY_CURVE,
     +                  INTERVAL_INDEX)
         ALLOCATE(TEMP_CS_SUPPLY_CAPACITY_CURVE(MAX_POINTS_IN_CS),
     +            TEMP_SYSTEM_SUPPLY_CAPACITY_CURVE(MAX_POINTS_IN_CS),
     +            TEMP_CS_SUPPLY_COST_CURVE(MAX_POINTS_IN_CS),
     +            TEMP_CS_SUPPLY_POSITION(MAX_POINTS_IN_CS),
     +            INTERVAL_INDEX(MAX_POINTS_IN_CS))
!
         IF (ALLOCATED(CS_SUPPLY_CAPACITY_CURVE))
     +        DEALLOCATE(CS_SUPPLY_CAPACITY_CURVE,
     +                   CS_SUPPLY_POSITION,
     +                   CS_SUPPLY_EXTRACT_CURVE,
     +                   CS_SUPPLY_COST_CURVE)
         ALLOCATE(   CS_SUPPLY_CAPACITY_CURVE(MAX_POINTS_IN_CS,CS),
     +               CS_SUPPLY_EXTRACT_CURVE(MAX_POINTS_IN_CS,CS),
     +               CS_SUPPLY_POSITION(MAX_POINTS_IN_CS,CS),
     +               CS_SUPPLY_COST_CURVE(MAX_POINTS_IN_CS,CS))



!
! CALCULATE CAPACITY AND COST CURVES
!         
         CS_SUPPLY_CAPACITY_CURVE = 0.
         CS_SUPPLY_EXTRACT_CURVE = 0.
         CS_SUPPLY_COST_CURVE = 0.
!
         TEMP_CS_SUPPLY_CAPACITY_CURVE = 0.
         TEMP_CS_SUPPLY_COST_CURVE = 999999.
!
! FIND ALL TIME-VARYING VECTORS. FIND THE MIN AND MAX PRICE AND QUANTITY 
! FOR EACH SUPPLY CURVE TO KNOW HOW TO SPLIT THE COSTS.
!
         DO I = 1, GLOBAL_CS ! CS INDEX
            L = 1
            DO J = 1, MAX_POINTS_IN_CS
               TEMP_CS_SUPPLY_POSITION(J) = J 
            END DO
            DO J = FIRST_CS(I), FIRST_CS(I)+GROUPS_PER_CS(I)-1
               M = COAL_GROUP_ID_POSITION(J) ! RETURNS COAL_GROUP
!
! 111807. REMOVED THIS CONDITION
!               IF(SUPPLY_TYPE(M) == 'L') CYCLE 
!
!               IF(1+get_BASE_YEAR() < BASIN_START_YEAR(M)) CYCLE
!
               DO Q = 1, 31 ! 031408 changed from 25.
!               
                  IF(BASIN_START_YEAR(M) < get_BASE_YEAR() + 1) THEN
                     IF(BASIN_START_YEAR(M)+Q <= get_BASE_YEAR()) THEN
                        CYCLE
                     ELSE
                        K = BASIN_START_YEAR(M) + Q - get_BASE_YEAR()
                     ENDIF
                  ELSE
                     K = Q + BASIN_START_YEAR(M) - 1 - get_BASE_YEAR()
                  ENDIF
                  IF(K < 1 .OR. K  > 30) CYCLE ! 25) CYCLE
!               
! 081006. OUT TO ATTEMPT SPARSE 
!
                  IF(ABS(INT_RES_REMAIN(K,M))< 0.00001) EXIT

                     TEMP_R = INT_RES_REMAIN(K,M)

                  CS_SUPPLY_CAPACITY_CURVE(K,I) = TEMP_R
                  CS_SUPPLY_COST_CURVE(K,I) = OPERATING_COSTS(K,I) +
     +                                              CAPACITY_VALUES(K,I)

                  L = L + 1
               END DO ! K 25 POINT COAL_GROUP CURVE
            END DO ! J COAL_GROUP WITHIN CS
            L = L - 1 ! SAVE AS AN ARRAY?
!
! PERFORM MONTHLY COST CALCULATIONS. MAINTAIN BASIN IDENTITIES. 
! HANDLE GEOLOGICAL, LNG AND STORAGE.
!
! SORT THE TEMP_CS ARRAYS BY COST.  THIS SHOULD BE DONE ON A MONTHLY BASIS
!            IF(L <= 25) THEN ! NO NEED TO AGGREGATE
!            DO K = 1, 25 
            IF(L > MAX_POINTS_IN_CS) THEN ! NO NEED TO AGGREGATE
               WRITE(4,*) "BOUND LIMIT EXCEEDED IN COAL BASIN CURVES"
               WRITE(4,*) "BOUND = ",MAX_POINTS_IN_CS
               WRITE(4,*) " COAL INTERVALS = ",L
               call end_program("coal_obj:0006 - bound limit " //
     + "exceeded.")
            ENDIF

            DO K = 1, 30 ! 25 
               IF(CS_SUPPLY_COST_CURVE(K,I) >= 999998.) THEN
                  CS_SUPPLY_COST_CURVE(K,I) = 0.0
               ENDIF
            END DO
         ENDDO ! CS COUNTER
!         
         SAVE_CS_FILE_EXISTS = .TRUE.
         MANAGE_COAL_SUPPLY_FORECASTS = .TRUE.
!
!         
         RETURN
!         

         ENTRY GET_NUM_OF_COAL_SUPPLY_REGIONS()

            GET_NUM_OF_COAL_SUPPLY_REGIONS = NUMBER_OF_ACTIVE_REGIONS
         RETURN

         ENTRY ANNUAL_COAL_SUPPLY_CURVES(R_YEAR)

            ANNUAL_COAL_SUPPLY_CURVES = .FALSE.
            IF(.NOT. SAVE_CS_FILE_EXISTS) RETURN
            ANNUAL_COAL_SUPPLY_CURVES = .TRUE.
! 031308. temp fix. determine why 25th interval = 0.
            L = R_YEAR ! MIN(24,R_YEAR)
            LOCAL_MONTH = 1
            DO I = 1, GLOBAL_CS ! CS INDEX
               IF(SUPPLY_TYPE(I) == 'L') THEN
                  IF(ABS(MAX_DAILY_EXTRACTION_RATE(I)) < 0.0000001) THEN
                     MAX_DAILY_EXTRACTION_IN_MONTH(I) = 999999999.
                  ELSEIF(MAX_DAILY_EXTRACTION_RATE(I) < -0.1) THEN
                     MAX_DAILY_EXTRACTION_IN_MONTH(I) = 
     +                  ESCALATED_MONTHLY_VALUE(
     +                     REAL4_ONE,
     +                     INT2(ABS(MAX_DAILY_EXTRACTION_RATE(I))),
     +                                       R_YEAR,LOCAL_MONTH,INT2(1))
                  ELSE
                     MAX_DAILY_EXTRACTION_IN_MONTH(I) =
     +                              MAX_DAILY_EXTRACTION_RATE(I)
                  ENDIF
                  IF(R_YEAR+get_BASE_YEAR() >= BASIN_START_YEAR(I)) THEN 
                     IF(INT_RES_REMAIN(L,I) < 0.) THEN
                        TEMP_R = 
     +                     ESCALATED_MONTHLY_VALUE(
     +                                       REAL4_ONE,
     +                                       INT2(INT_RES_REMAIN(K,M)),
     +                                       R_YEAR,LOCAL_MONTH,INT2(1))
                     ELSE
                        TEMP_R = MIN(INT_RES_REMAIN(L,I),
     +                              .000001*DAYS_PER_MONTH(LOCAL_MONTH)*
     +                              MAX_DAILY_EXTRACTION_IN_MONTH(I)) ! TCF.
                     ENDIF
                     IF(ABS(TEMP_R) < 0.00001) EXIT
                     CS_SUPPLY_CAPACITY_CURVE(L,I) = TEMP_R
                  ENDIF
               ELSE
                  IF(RESERVE_APPRECIATION_PERCENT(I) >= 0.) THEN ! ASSUME POSITIVE ESCALATION
!                  
                     ANNUAL_RESERVE_APPRECIATION = 
     +                  ( 1. + 
     +                    RESERVE_APPRECIATION_PERCENT(I) 
     +                                           /100.)! **(MAX(1,R_YEAR))
                  ELSE
                     ANNUAL_RESERVE_APPRECIATION = 
     +                  ESCALATED_MONTHLY_VALUE(
     +                      REAL4_ONE,
     +                     INT2(ABS(RESERVE_APPRECIATION_PERCENT(I))),
     +                                  R_YEAR,LOCAL_MONTH,INT2(1))/100.
                  ENDIF
                  CS_SUPPLY_CAPACITY_CURVE(L,I) = 
     +                     CS_SUPPLY_CAPACITY_CURVE(L,I) * 
     +                           ANNUAL_RESERVE_APPRECIATION
                  IF(R_YEAR > 1) THEN
                     CS_SUPPLY_CAPACITY_CURVE(L,I) = 
     +                     CS_SUPPLY_CAPACITY_CURVE(L,I) +
     +                           MAX(0.,CS_SUPPLY_CAPACITY_CURVE(L-1,I))
                  ENDIF
               ENDIF
            ENDDO
         RETURN

         ENTRY MONTHLY_COAL_SUPPLY_CURVES(R_MONTH,R_YEAR)

!
! 050107. REDEFINITION OF SUPPLY CURVES:
!         1. EACH LINE ON THE SUPPLY CURVE CORRESPONDS TO A YEAR GOING FORWARD
!         2. THE AMOUNT IN THE YEAR THAT IS AVAILABLE IS EQUAL TO THE INCREMENT
!            PLUS ANY UNUSED FROM PREVIOUS YEARS.
!         3. PREVIOUS YEARS MUST BE CARRIED FORWARD.
!         4. IF CURRENT YEAR IS EXHUASTED, DO NOT EXTRACT MORE.
!         5. APPRECIATION IS CUMULATIVE.  
!
         MONTHLY_COAL_SUPPLY_CURVES = .FALSE.
         IF(.NOT. SAVE_CS_FILE_EXISTS) RETURN
         MONTHLY_COAL_SUPPLY_CURVES =  .TRUE.
         DO I = 1, GLOBAL_CS ! CS INDEX
            L = 1
            TEMP_CS_SUPPLY_CAPACITY_CURVE = 0.
            TEMP_CS_SUPPLY_COST_CURVE = 999999.
            DO J = 1, MAX_POINTS_IN_CS
               TEMP_CS_SUPPLY_POSITION(J) = J 
            END DO
            DO J = FIRST_CS(I), FIRST_CS(I)+GROUPS_PER_CS(I)-1
               M = COAL_GROUP_ID_POSITION(J) ! RETURNS COAL_GROUP
!
               IF(EXTRACTION_COST_ESC_RATE(M) >= 0.) THEN ! ASSUME CONSTANT ESCALATION
                  
                  ANNUAL_MONTHLY_COST_ESC = 
     +               ( 1. + 
     +                    EXTRACTION_COST_ESC_RATE(M) 
     +                                           /100.)**(MAX(1,R_YEAR))
               ELSE
                  ANNUAL_MONTHLY_COST_ESC = 
     +               ESCALATED_MONTHLY_VALUE(
     +                  REAL4_ONE,
     +                  INT2(ABS(EXTRACTION_COST_ESC_RATE(M))),
     +                                      R_YEAR,R_MONTH,INT2(1))/100.
               ENDIF
!
!
! 10/23/06. ADDED PER THE 10/16/06 MEETING.
! 120306. ONLY ACTIVE FOR R_MONTH = 1.
!                   
               IF(R_MONTH == 1) THEN
                  IF(RESERVE_APPRECIATION_PERCENT(M) >= 0.) THEN ! ASSUME POSITIVE ESCALATION
!                  
                     ANNUAL_RESERVE_APPRECIATION = 
     +                  ( 1. + 
     +                    RESERVE_APPRECIATION_PERCENT(M) 
     +                                           /100.)! **(MAX(1,R_YEAR))
                  ELSE
                     ANNUAL_RESERVE_APPRECIATION = 

     +                  ESCALATED_MONTHLY_VALUE(
     +                      REAL4_ONE,
     +                     INT2(ABS(RESERVE_APPRECIATION_PERCENT(M))),
     +                                      R_YEAR,R_MONTH,INT2(1))/100.
                  ENDIF
               ELSE
                  ANNUAL_RESERVE_APPRECIATION = 1.0
               ENDIF
!
! 092806. ADDED FOR LNG MODELING.
!               
               IF(ABS(MAX_DAILY_EXTRACTION_RATE(M)) < 0.0000001) THEN
                  MAX_DAILY_EXTRACTION_IN_MONTH(M) = 999999999.
               ELSEIF(MAX_DAILY_EXTRACTION_RATE(M) < -0.1) THEN
! ASSUMES A VALUE
                  MAX_DAILY_EXTRACTION_IN_MONTH(M) = 
     +               ESCALATED_MONTHLY_VALUE(

     +                  REAL4_ONE,
     +                  INT2(ABS(MAX_DAILY_EXTRACTION_RATE(M))),
     +                                           R_YEAR,R_MONTH,INT2(1))
               ELSE
                  MAX_DAILY_EXTRACTION_IN_MONTH(M) =
     +                              MAX_DAILY_EXTRACTION_RATE(M)
               ENDIF
               DO K = 1, 30 ! 25
!               
! 081006. OUT TO ATTEMPT SPARSE 
!
                  IF(ABS(INT_RES_REMAIN(K,M))< 0.00001) EXIT
                  IF(OPERATING_COSTS(K,M) < 0) THEN
!                     TEMP_R = TEMP_R ! ESTABLISH POINTER STUFF
! 092906
!
! ASSUMES A VALUE
                     TEMP_R = 
     +                  ESCALATED_MONTHLY_VALUE(
!     +                                        ABS(OPERATING_COSTS(K,M)),
     +                                       REAL4_ONE,
     +                                       INT2(OPERATING_COSTS(K,M)),
     +                                        R_YEAR,R_MONTH,INT2(1))
                  ELSE
                     TEMP_R = OPERATING_COSTS(K,M)
                  ENDIF
!                  CS_SUPPLY_COST_CURVE(L,I) = TEMP_R
!
! NOTE: COSTS ARE ACCUMULATED (OPERATING + CAPACITY)
!
                  IF(CAPACITY_VALUES(K,M) < 0) THEN
!                     TEMP_R = TEMP_R ! ESTABLISH POINTER STUFF
! 092906
!
! ASSUMES A VALUE
                     TEMP_R = 
     +                  ESCALATED_MONTHLY_VALUE(
     +                                       REAL4_ONE,
     +                                       INT2(CAPACITY_VALUES(K,M)),
     +                                        R_YEAR,R_MONTH,INT2(1))
                  ELSE
                     TEMP_R = CAPACITY_VALUES(K,M) + TEMP_R
                  ENDIF
!                  
                  CS_SUPPLY_COST_CURVE(L,I) = 
     +                                    TEMP_R*ANNUAL_MONTHLY_COST_ESC
                  CS_SUPPLY_EXTRACT_CURVE(L,I) = 
     +                                  MAX_DAILY_EXTRACTION_IN_MONTH(M)
!
!
! 101006. ADD LNG HERE.
!
                  IF(SUPPLY_TYPE(M) == 'L' .AND. 
     +                           R_YEAR+get_BASE_YEAR() >= 
     +                                         BASIN_START_YEAR(M)) THEN 
!
! RELY ON BOTH THE RESERVE AND THE EXTRACTION RATE.
!
!                     IF(ABS(INT_RES_REMAIN(K,M))< 0.00001) EXIT
                     IF(INT_RES_REMAIN(K,M) < 0.) THEN
! 092906
!
! ASSUMES A VALUE.
                        TEMP_R = 
     +                     ESCALATED_MONTHLY_VALUE(
     +                                        REAL4_ONE,
!     +                                        ABS(INT_RES_REMAIN(K,M)),
     +                                        INT2(INT_RES_REMAIN(K,M)),
     +                                        R_YEAR,R_MONTH,INT2(1))
                     ELSE
                        TEMP_R = MIN(INT_RES_REMAIN(K,M),
     +                              .000001*DAYS_PER_MONTH(R_MONTH)*
     +                              MAX_DAILY_EXTRACTION_IN_MONTH(M)) ! TCF.
                     ENDIF
                     IF(ABS(TEMP_R) < 0.00001) EXIT
                     CS_SUPPLY_CAPACITY_CURVE(L,I) = TEMP_R
!
! ADD A NEW BASIN HERE
!
                  ELSEIF(R_YEAR+get_BASE_YEAR() == BASIN_START_YEAR(M) .AND.
     +                                               R_MONTH == 1) THEN
                     IF(ABS(INT_RES_REMAIN(K,M))< 0.00001) EXIT
!
                     IF(INT_RES_REMAIN(K,M) < 0.) THEN
! 092906
!
! ASSUMES A VALUE.
                        TEMP_R = 
     +                     ESCALATED_MONTHLY_VALUE(
     +                                         REAL4_ONE,
!     +                                        ABS(INT_RES_REMAIN(K,M)),
     +                                        INT2(INT_RES_REMAIN(K,M)),
     +                                        R_YEAR,R_MONTH,INT2(1))
                     ELSE
                        TEMP_R = INT_RES_REMAIN(K,M)
                     ENDIF
                     CS_SUPPLY_CAPACITY_CURVE(L,I) = TEMP_R
                  ENDIF
!
! 102306.
!
                  CS_SUPPLY_CAPACITY_CURVE(L,I) = 
     +                     CS_SUPPLY_CAPACITY_CURVE(L,I)*
     +                              ANNUAL_RESERVE_APPRECIATION
!                  
! 101006.
! CALCULATE THE NUMBER OF INTERVALS TO ALLOW FOR A MONTH HERE. 
! STORE AS NEW VARAIBLE. THE MAXIMUM SIZE VARIABLE IS PASSED TO THE 
! THE PRICING MODEL. THIS WILL DRAMATICALLY REDUCE THE SIZE OF THE
! SEARCH ARRAY.
!
                  L = L + 1                  
!
               ENDDO ! K            
            ENDDO ! J
            L = L - 1 ! SAVE AS AN ARRAY?
! 
! 101406. SORT CS FROM HIGH TO LOW AND PUT LNG IN THE FIRST INTERVALS.
!
            TEMP_I = L
            TEMP_CS_SUPPLY_COST_CURVE(:) = CS_SUPPLY_COST_CURVE(:,I)
!             
            CALL REAL4_Sort(TEMP_I,
     +                    TEMP_CS_SUPPLY_POSITION,
     +                    TEMP_CS_SUPPLY_COST_CURVE)
            CS_SUPPLY_POSITION(:,I) = TEMP_CS_SUPPLY_POSITION(:)
!            
         ENDDO ! I
      RETURN

      ENTRY WRITE_SUPPLY_CURVE_REPORT(R_MONTH,R_YEAR)

! WRITE THE SUPPLY CURVE REPORT.
         IF( .NOT. SAVE_CS_FILE_EXISTS) THEN
            WRITE_SUPPLY_CURVE_REPORT =  .FALSE.
            RETURN
         ENDIF
         YES_MONTHLY_SUPPLY_REPORTS = MONTHLY_COAL_SUPPLY_REPORT() 
         IF(COAL_SUPPLY_CURVE_REPORT_NOT_OPEN .AND. 
     +                                  YES_MONTHLY_SUPPLY_REPORTS) THEN
            COAL_SUPPLY_CURVE_REPORT_NOT_OPEN = .FALSE.
            VARIABLE_NUMBER = MAX_POINTS_IN_CS ! 25
            MONTHLY_COAL_SUPPLY_UNIT = 
     +               MONTHLY_COAL_SUPPLY_HEADER(VARIABLE_NUMBER,
     +                                                  COAL_SUPPLY_REC)
            LAST_SEASON = get_PRODUCTION_PERIODS_in()
            CL_MONTH_NAME(LAST_SEASON+1) = 'Annual'
            ANNUAL_COUNTER = LAST_SEASON + 1
!
            OPEN(10,FILE="MSGMonthlyCOALSupplyCurve.RPT")
            DO J = 1, MAX_POINTS_IN_CS 
!               CS = GET_COAL_GROUP_INDEX(J)
               WRITE(TEMP_STR,'(I4)') J-1
               WRITE(TEMP_INTERVAL_NUM,'(I4)') J
               TEMP_STR = 
     +            '"Interval'//TRIM(TEMP_INTERVAL_NUM)//'"'//
     +                       ',V2,'//TRIM(TEMP_STR)//
     +                         ',,S,,,,,,,,"COAL Curve by Interval",'
               WRITE(10,'(A)') TRIM(TEMP_STR)
            END DO
            CLOSE(10)
         ENDIF
!         
         LOCAL_YEAR = R_YEAR + get_BASE_YEAR()
!
         TEMP_SYSTEM_SUPPLY_CAPACITY_CURVE = 0.
!
! NEED TO REWRITE TO ALIGN THE LENGTH OF THE RPT.
!
         LOCAL_MAX_POINTS_IN_CS = MIN(30,MAX_POINTS_IN_CS) ! 25,MAX_POINTS_IN_CS)
!         
         DO I = 1, GLOBAL_CS ! CS INDEX
            IF(YES_MONTHLY_SUPPLY_REPORTS) THEN
               J = CS_GROUP_INDEX(I)
               TEMP_L = GET_GN_GROUP_NAME(J,TEMP_NODE_NAME)  ! LOGICAL
               TEMP_NAME = TEMP_NODE_NAME(1:20)//' Cost $/MCF'
!               
               TEMP_CS_SUPPLY_CAPACITY_CURVE = 0.
               TEMP_CS_SUPPLY_COST_CURVE = 0.
! WRITE THE SUPPLY CURVE AS CUMULATIVE.
               DO L = 1, POINTS_PER_CS(I) 
                  K = CS_SUPPLY_POSITION(L,I)
                  IF(K <= 0 .OR. K > POINTS_PER_CS(I)) THEN
                     K = K
                  ENDIF
                  IF(L == 1)THEN
                     TEMP_CS_SUPPLY_CAPACITY_CURVE(L) =
     +                                     CS_SUPPLY_CAPACITY_CURVE(K,I)
                     TEMP_CS_SUPPLY_COST_CURVE(L) = 
     +                                     CS_SUPPLY_COST_CURVE(K,I)
                  ELSEIF(CS_SUPPLY_CAPACITY_CURVE(K,I) > .0001) THEN
                     TEMP_CS_SUPPLY_CAPACITY_CURVE(L) =
     +                      TEMP_CS_SUPPLY_CAPACITY_CURVE(L-1) +
     +                                     CS_SUPPLY_CAPACITY_CURVE(K,I)
                     TEMP_CS_SUPPLY_COST_CURVE(L) = 
     +                                     CS_SUPPLY_COST_CURVE(K,I)
                  ENDIF
                  TEMP_SYSTEM_SUPPLY_CAPACITY_CURVE(L) = 
     +                  TEMP_SYSTEM_SUPPLY_CAPACITY_CURVE(L) +
     +                                     CS_SUPPLY_CAPACITY_CURVE(K,I)
               ENDDO
               WRITE(MONTHLY_COAL_SUPPLY_UNIT,REC=COAL_SUPPLY_REC)
     +            PRT_ENDPOINT(),
     +            FLOAT(LOCAL_YEAR),
     +            CL_MONTH_NAME(R_MONTH),
     +            TEMP_NAME,
     +            (TEMP_CS_SUPPLY_COST_CURVE(L),L=1,MAX_POINTS_IN_CS) 
               COAL_SUPPLY_REC = COAL_SUPPLY_REC + 1
!               
               TEMP_NAME = TEMP_NODE_NAME(1:20)//' Reserve TCF'
               WRITE(MONTHLY_COAL_SUPPLY_UNIT,REC=COAL_SUPPLY_REC)
     +            PRT_ENDPOINT(),
     +            FLOAT(LOCAL_YEAR),
     +            CL_MONTH_NAME(R_MONTH),
     +            TEMP_NAME,
     +            (TEMP_CS_SUPPLY_CAPACITY_CURVE(L),
     +                                             L=1,MAX_POINTS_IN_CS) 
               COAL_SUPPLY_REC = COAL_SUPPLY_REC + 1
            ENDIF
! NOTE: CAN ADD OTHER COST COMPONENTS HERE: SCARCITY OR OPERATION RELATIVE TO OIL
         ENDDO  ! CS INDEX
         TEMP_NAME = 'System Non-Cumulative TCF     '
         WRITE(MONTHLY_COAL_SUPPLY_UNIT,REC=COAL_SUPPLY_REC)
     +            PRT_ENDPOINT(),
     +            FLOAT(LOCAL_YEAR),
     +            CL_MONTH_NAME(R_MONTH),
     +            TEMP_NAME,
     +            (TEMP_SYSTEM_SUPPLY_CAPACITY_CURVE(L),
     +                                             L=1,MAX_POINTS_IN_CS) 
         COAL_SUPPLY_REC = COAL_SUPPLY_REC + 1
!
         TOTAL_COAL_RESERVE = 0.
         DO L = 1, MAX_POINTS_IN_CS 
            TOTAL_COAL_RESERVE = TOTAL_COAL_RESERVE + 
     +                              TEMP_SYSTEM_SUPPLY_CAPACITY_CURVE(L)
         END DO
!
!
         WRITE_SUPPLY_CURVE_REPORT =  .TRUE.
      RETURN

      ENTRY GET_MAX_POINTS_IN_CS

         GET_MAX_POINTS_IN_CS = MAX_POINTS_IN_CS
      RETURN

      ENTRY GET_SUPPLY_CURVE_POINTS(R_CS)

         CS =  COAL_GROUP_POSITION(R_CS)
         IF(CS <= 0) THEN
            GET_SUPPLY_CURVE_POINTS = 0
         ELSE
            DO K = 1, POINTS_PER_CS(CS) ! 25 ! THIS NEEDS TO BE REDIMENSIONED. NEED TO 
!
! ALLOWS FOR DEPLETION FROM THE TOP TO THE BOTTOM.
!         
               IF(CS_SUPPLY_CAPACITY_CURVE(K,CS) > 0.00001) THEN
                  GET_SUPPLY_CURVE_POINTS = K
               ENDIF
            ENDDO
         ENDIF
      RETURN

      ENTRY GET_SYSTEM_DAILY_EXTRACT_RATE(R_YEAR,R_MONTH)
      
         LOCAL_YEAR = R_YEAR + get_base_year()

         TEMP_R = 0.0
         DO I = 1, MAX_COAL_GROUP_INDEX 
            CS =  COAL_GROUP_POSITION(I)
            IF(CS <= 0) CYCLE
            IF(BASIN_START_YEAR(CS) > LOCAL_YEAR) THEN
               TEMP_R = TEMP_R + .00001
!            ELSEIF(DYNAMIC_EXTRACT_RATE(CS)) THEN
!               TEMP_R = TEMP_R + 
!               R_HARD_EXTRACT_RATE = 1.05*R_EXTRACT_RATE
!            ELSEIF(ABS(MAX_DAILY_HARD_LIMIT(CS)) < 0.0000001) THEN
!               TEMP_R = TEMP_R + 
            ELSEIF(MAX_DAILY_EXTRACTION_RATE(CS) < -0.1) THEN
               TEMP_R = TEMP_R +
     +                  ESCALATED_MONTHLY_VALUE(
     +                     REAL4_ONE,
     +                     INT2(ABS(MAX_DAILY_EXTRACTION_RATE(CS))),
     +                                       R_YEAR,R_MONTH,INT2(1))
            ELSE
               TEMP_R = TEMP_R + MAX_DAILY_EXTRACTION_RATE(CS)
            ENDIF
         END DO
         GET_SYSTEM_DAILY_EXTRACT_RATE = TEMP_R
      RETURN

      ENTRY GET_SUPPLY_COST_CAPACITY_CURVES(
     +                        R_CS,
     +                        R_MAX_S,
     +                        R_COST_CURVE,
     +                        R_CAPACITY_CURVE,
     +                        R_MIN_EXTRACT_RATE,
     +                        R_EXTRACT_RATE,
     +                        R_HARD_EXTRACT_RATE,
     +                        R_SHRINKAGE_COST,
     +                        R_SHRINKAGE_PERCENT,
     +                        R_FIRST_S,
     +                        R_YEAR,
     +                        R_MONTH,
     +                        R_MIN_BASIN_COST,
     +                        R_ANNUAL_PRODUCTION_TARGET)

!
! 101006. THIS IS CALLED MONTHLY BY SUPPLY BASIN.
! 050107. REDEFINITION OF BASIN: THIS NOW TAKES ONLY THE CURRENT BASIN IN A GIVEN YEAR.
!
         GET_SUPPLY_COST_CAPACITY_CURVES = .FALSE. 
         R_MAX_S = 0

         R_EXTRACT_RATE = 0.
! NOTE: INDEX ON CS
         CS =  COAL_GROUP_POSITION(R_CS)
!
         LOCAL_YEAR = R_YEAR + get_BASE_YEAR()
!
         IF(CS == 0) RETURN
         BASIN_YEAR = MAX(0,LOCAL_YEAR - 
     +                        MAX(get_BASE_YEAR(),BASIN_START_YEAR(CS)-1))
         GET_SUPPLY_COST_CAPACITY_CURVES = .TRUE. 
!         
         FOUND_FIRST_S = .FALSE.
!
! 051107. JUMP ONLY TO CURRENT YEAR.
!         
!         DO L = 1, POINTS_PER_CS(CS) ! 25 ! THIS NEEDS TO BE REDIMENSIONED. NEED TO 
!
            L = R_YEAR
!
            K = L ! 061507. CS_SUPPLY_POSITION(L,CS)

            R_COST_CURVE = CS_SUPPLY_COST_CURVE(K,CS)
            R_EXTRACT_CURVE = CS_SUPPLY_EXTRACT_CURVE(K,CS)
! TCF TO MCF CONVERSION            
!
            R_CAPACITY_CURVE = 
     +                           CS_SUPPLY_CAPACITY_CURVE(K,CS)*1000000. 
!
!            R_CAPACITY_CURVE(K) = INT_RES_REMAIN(K,CS)*1000000.
!            
!         END DO
! 092906         
!
! THESE ARE DIMENSIONED M NOT CS.
!
!         R_EXTRACT_RATE =  MAX_DAILY_EXTRACTION_RATE(CS)! MUST DETERMINE A METHOD (PROBABLY A CURVE)
!
         R_MIN_BASIN_COST = MIN_BASIN_COST(CS)
         IF(DYNAMIC_EXTRACT_RATE(CS)) THEN

            IF(R_MONTH < 3) THEN
               R_EXTRACT_RATE = R_CAPACITY_CURVE / 
     +             (DAYS_REMAIN_IN_YEAR(R_MONTH) - 12. +FLOAT(R_MONTH)) 
            ELSE
               R_EXTRACT_RATE = R_CAPACITY_CURVE / 
     +                               (DAYS_REMAIN_IN_YEAR(R_MONTH) + 1.) ! 071508. ONE LESS DAY.
            ENDIF
         ELSEIF(ABS(MAX_DAILY_EXTRACTION_RATE(CS)) < 0.0000001) THEN
            R_EXTRACT_RATE = 999999999.
         ELSEIF(MAX_DAILY_EXTRACTION_RATE(CS) < -0.1) THEN

            R_EXTRACT_RATE = 
     +                  ESCALATED_MONTHLY_VALUE(
     +                     REAL4_ONE,
     +                     INT2(ABS(MAX_DAILY_EXTRACTION_RATE(CS))),
     +                                       R_YEAR,R_MONTH,INT2(1))
         ELSE
            R_EXTRACT_RATE = MAX_DAILY_EXTRACTION_RATE(CS)
         ENDIF
!         R_HARD_EXTRACT_RATE = MAX_DAILY_HARD_LIMIT(CS)
         IF(BASIN_START_YEAR(CS) > LOCAL_YEAR) THEN
            R_HARD_EXTRACT_RATE = 0.01
            R_EXTRACT_RATE = 0.001
            R_CAPACITY_CURVE = .0000001 ! 070208.
         ELSEIF(DYNAMIC_EXTRACT_RATE(CS)) THEN
            R_HARD_EXTRACT_RATE = 1.05*R_EXTRACT_RATE
         ELSEIF(ABS(MAX_DAILY_HARD_LIMIT(CS)) < 0.0000001) THEN
            R_EXTRACT_RATE = 999999999.
         ELSEIF(MAX_DAILY_HARD_LIMIT(CS) < -0.1) THEN
            R_HARD_EXTRACT_RATE = 
     +                  ESCALATED_MONTHLY_VALUE(
     +                     REAL4_ONE,
     +                     INT2(ABS(MAX_DAILY_HARD_LIMIT(CS))),
     +                                       R_YEAR,R_MONTH,INT2(1))
         ELSE
            R_HARD_EXTRACT_RATE = MAX_DAILY_HARD_LIMIT(CS)
         ENDIF
!         
         IF(BASIN_START_YEAR(CS) > LOCAL_YEAR) THEN
            R_MIN_EXTRACT_RATE = 0.0
         ELSEIF(DYNAMIC_EXTRACT_RATE(CS)) THEN
            R_MIN_EXTRACT_RATE = R_EXTRACT_RATE * 0.90
         ELSEIF(ABS(MIN_DAILY_LIMIT(CS)) < 0.0000001) THEN
            R_MIN_EXTRACT_RATE = 0.0
         ELSEIF(MIN_DAILY_LIMIT(CS) < -0.1) THEN
            R_MIN_EXTRACT_RATE = 
     +                  ESCALATED_MONTHLY_VALUE(
     +                     REAL4_ONE,
     +                     INT2(ABS(MIN_DAILY_LIMIT(CS))),
     +                                       R_YEAR,R_MONTH,INT2(1))
         ELSEIF(MIN_DAILY_LIMIT(CS) > 0.0 .AND. 
     +                                   MIN_DAILY_LIMIT(CS) < 1.0) THEN
            R_MIN_EXTRACT_RATE = MIN_DAILY_LIMIT(CS) * R_EXTRACT_RATE
         ELSE
            R_MIN_EXTRACT_RATE = MIN_DAILY_LIMIT(CS)
         ENDIF
!
! 072208. CHECK FOR EXHUASTED ANNUAL PRODUCTION
!
         R_MIN_EXTRACT_RATE = MIN(R_MIN_EXTRACT_RATE,
     +                          CS_SUPPLY_CAPACITY_CURVE(K,CS)*1000000.)
         R_EXTRACT_RATE = MIN(R_EXTRACT_RATE,
     +                          CS_SUPPLY_CAPACITY_CURVE(K,CS)*1000000.)
         R_HARD_EXTRACT_RATE = MIN(R_HARD_EXTRACT_RATE,
     +                          CS_SUPPLY_CAPACITY_CURVE(K,CS)*1000000.)
!
!         R_MIN_EXTRACT_RATE = MIN_DAILY_LIMIT(CS)
!         
         R_SHRINKAGE_COST = SHRINKAGE_COST(CS)
         R_SHRINKAGE_PERCENT = SHRINKAGE_PERCENT(CS)/100. ! CONVERT TO PERCENTAGE
         IF(BASIN_YEAR > 0) THEN
            R_ANNUAL_PRODUCTION_TARGET = 
     +                           INT_RES_REMAIN(BASIN_YEAR,CS)*1000000.
         ELSE
            R_ANNUAL_PRODUCTION_TARGET = 0.0
         ENDIF
!         R_ANNUAL_PRODUCTION_TARGET = INT_RES_REMAIN(R_YEAR,CS)*1000000.
      RETURN

      ENTRY annual_lp_basin_COAL(R_YEAR)

         annual_lp_basin_COAL = .false.
         if(R_YEAR == 1) RETURN
         K = R_YEAR 
         DO I = 1, GLOBAL_CS ! CS INDEX
            CS_SUPPLY_CAPACITY_CURVE(K,CS) = 
     +                  CS_SUPPLY_CAPACITY_CURVE(K,I) +
     +                       MAX(0.,CS_SUPPLY_CAPACITY_CURVE(K-1,I))
         ENDDO
         annual_lp_basin_COAL = .true.
      RETURN

      ENTRY put_monthly_lp_remaining_COAL(
     +                                 R_CS,
     +                                 R_CAPACITY_CURVE,
     +                                 R_YEAR)

         put_monthly_lp_remaining_COAL = .FALSE. 
! NOTE: INDEX ON CS
         CS =  COAL_GROUP_POSITION(R_CS)
!
         IF(CS <= 0) RETURN
!
         K = R_YEAR
!
!         IF(CS_SUPPLY_CAPACITY_CURVE(K,CS) >= 0.00001) THEN
!
! TCF TO MCF CONVERSION            
!
            CS_SUPPLY_CAPACITY_CURVE(K,CS) = 
     +                  MAX(0.,CS_SUPPLY_CAPACITY_CURVE(K,CS) - 
     +                                   R_CAPACITY_CURVE * 0.000001)
!
            put_monthly_lp_remaining_COAL = .TRUE. 
!         ENDIF
      RETURN

      ENTRY PUT_SUPPLY_CAPACITY_CURVES(R_CS,
     +                                 R_CAPACITY_CURVE,
     +                                 R_FIRST_S,
     +                                 R_LAST_S)


         PUT_SUPPLY_CAPACITY_CURVES = .FALSE. 
         CS =  COAL_GROUP_POSITION(R_CS)
!
         IF(CS <= 0) RETURN
!
         DO L = 1, POINTS_PER_CS(CS) ! 25 ! THIS NEEDS TO BE REDIMENSIONED. NEED TO 
!
            K = CS_SUPPLY_POSITION(L,CS)
            IF(CS_SUPPLY_CAPACITY_CURVE(K,CS) < 0.00001) CYCLE ! EXIT
!            R_MAX_S = K 
!            R_COST_CURVE(K) = CS_SUPPLY_COST_CURVE(K,CS)
! TCF TO MCF CONVERSION            
!
            CS_SUPPLY_CAPACITY_CURVE(K,CS) = 
     +                  MAX(0.,CS_SUPPLY_CAPACITY_CURVE(K,CS) - 
     +                                   R_CAPACITY_CURVE * 0.000001)
!

         END DO
         PUT_SUPPLY_CAPACITY_CURVES = .TRUE. 
      RETURN

      ENTRY COAL_SUPPLY_GROUP_ACTIVE(R_COAL_GROUP)

         IF( .NOT. SAVE_CS_FILE_EXISTS) THEN
            COAL_SUPPLY_GROUP_ACTIVE = .TRUE. 
         ELSE
            COAL_SUPPLY_GROUP_ACTIVE = 
     +                              SUPPLY_GROUP_AVAILABLE(R_COAL_GROUP)
         ENDIF
      RETURN
      END

!
      subroutine REAL4_Sort(iSup,iPos,a)
!

! sorts iSup items into order based on values in key (1-based) array a;
! Shell sort adapted from the source on page 110 of
! SOFTWARE TOOLS IN PASCAL, by Kernighan & Plauger, Addison-Wesley 1981
      integer*2 i,j,k,p,q,Gap,iSup,iPos(iSup)
      real*4 a(iSup)
      logical*1 Increasing,Decreasing
!
      Increasing = .TRUE.
      Decreasing=.not.Increasing
      Gap=iSup/2
      do while(Gap>0)
         do i=Gap+1,iSup
           j=i-Gap
           do while(j>0)
             k=j+Gap
             p=iPos(j)
             q=iPos(k)

             if((Increasing.and.(a(p)<=a(q))).or.
     +         (Decreasing.and.(a(p)>=a(q)))) then
               j=0 ! break the while loop (assign j=-1 for 0-based arrays)
             else ! interchange the indexing array values
               iPos(k)=p
               iPos(j)=q
             end if
             j=j-Gap
           end do
         end do
         Gap=Gap/2
      end do
      end ! subroutine REAL4_Sort

!
!                  ROUTINE TO CONVERT COAL_STORAGE FILE
!
!                          COPYRIGHT (C) 2007
!                        GLOBAL ENERGY DECISIONS
!                          ALL RIGHTS RESERVED
!

!
!
      SUBROUTINE COAL_STORAGE_OBJECT
!

      use spindriftlib
      use prod_arrays_dimensions
      USE SIZECOM

      INTEGER*2   UNIT_NUM/10/
      INTEGER*2 INUNIT,IREC,LRECL/318/ ! ADDED 20 VARIABLES ON 3/18/07.
!      INTEGER IOS
!      
      INTEGER*2   SAVE_NUMBER_OF_COAL_STORAGE/0/,
     +            R_NUMBER_OF_COAL_STORAGE,R_WHEELS,
     +            COAL_STORAGE_OWNER,
     +            Storage_Start_Year
      INTEGER*2   TRANS_LINE_INDEX
      CHARACTER*5 BASE_FILE_NAME,OVERLAY_FAMILY_NAME,
     +            COAL_STORAGE_FILE 
      CHARACTER*256 FILE_NAME
      CHARACTER*255 SAVE_BASE_FILE_NAME,R_TEMP_NAME
      CHARACTER*256 BASE_FILE_DIRECTORY
      CHARACTER*256 DATA_DRIVE,OUTPUT_DIRECTORY
      CHARACTER*1  COAL_Storage_Active
      LOGICAL*4    FILE_EXISTS/.FALSE./,
     +             COAL_STORAGE_FILE_EXISTS/.FALSE./,
     +             R_COAL_STORAGE_FILE_EXISTS
! DECLARATION FOR DBREAD COMMON BLOCK

! DECLARATION FOR TRANSACT COAL_STORAGE DETERMINANTS
      CHARACTER*16 FILE_TYPE/'COAL STORAGE FORECAST '/
      CHARACTER*2  COAL_STORAGE_OL/'BC'/
      LOGICAL*1 LAHEY_LF95
      CHARACTER*30 SCREEN_OUTPUT

! 070906.
      INTEGER (KIND=2) :: DELETE,YR,ZONE,POINTR,get_BASE_YEAR(),
     +                    I,
     +                    TieID
      INTEGER (KIND=4) :: IOS
      CHARACTER (LEN=4096) RECLN
      REAL (KIND=4) :: 
     +            Storage_Initial_Capacity,
     +            Storage_Demand_Markets,
     +            Storage_Demand_Allocation,
     +            Storage_Maximum_Capacity,
     +            Storage_Demand_Slope,
     +            Storage_Demand_Intercept,
     +            Maximum_Daily_Injection,
     +            Maximum_Daily_Withdrawal,
     +            Storage_Weekly_Min_Capacity,
     +            Storage_Weekly_Max_Capacity,
     +            Storage_Scalar
      INTEGER (KIND=2) :: Storage_ID_Number
      CHARACTER (LEN=31) :: Storage_Name
      SAVE SAVE_BASE_FILE_NAME
! CONVERT THE COAL_STORAGE FILE
!
!
!

      ENTRY COAL_STORAGE_MAKEBIN

      BASE_FILE_NAME = COAL_STORAGE_FILE()
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME = trim(BASE_FILE_DIRECTORY())//
     +                             "GGB"//trim(BASE_FILE_NAME)//'.DAT'
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      COAL_STORAGE_FILE_EXISTS = FILE_EXISTS
      IF(FILE_EXISTS) THEN
         SAVE_BASE_FILE_NAME = FILE_NAME
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_LOCATE_WRITE(16,30,BASE_FILE_NAME,ALL_VERSIONS,0)
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         ENDIF
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=trim(DATA_DRIVE)//
     +                      "BCCOAL_STORAGE.BIN",ACCESS="DIRECT",
     +                                      STATUS="UNKNOWN",RECL=LRECL)
         IREC = 1
!
         READ(10,1000,IOSTAT=IOS) RECLN
!         
         DO
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(IOS /= 0) EXIT
            READ(RECLN,*,ERR=200) DELETE
!            
            IF(DELETE == 7) CYCLE ! NEW TABLE
!            
            RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
!            
!            
            READ(RECLN,*,ERR=200) DELETE,
     +                               Storage_Name,
     +                               COAL_Storage_Active,
     +                               Storage_ID_Number,
     +                               Storage_Initial_Capacity,
     +                               Storage_Demand_Markets,
     +                               Storage_Demand_Allocation,
     +                               Storage_Maximum_Capacity,
     +                               Storage_Demand_Slope,
     +                               Storage_Demand_Intercept,
     +                               Maximum_Daily_Injection,
     +                               Maximum_Daily_Withdrawal,
     +                               Storage_Weekly_Min_Capacity,
     +                               Storage_Weekly_Max_Capacity,
     +                               Storage_Scalar,
     +                               Storage_Start_Year
            WRITE(11,REC=IREC) DELETE,
     +                               Storage_Name,
     +                               COAL_Storage_Active,
     +                               Storage_ID_Number,
     +                               Storage_Initial_Capacity,
     +                               Storage_Demand_Markets,
     +                               Storage_Demand_Allocation,
     +                               Storage_Maximum_Capacity,
     +                               Storage_Demand_Slope,
     +                               Storage_Demand_Intercept,
     +                               Maximum_Daily_Injection,
     +                               Maximum_Daily_Withdrawal,
     +                               Storage_Weekly_Min_Capacity,
     +                               Storage_Weekly_Max_Capacity,
     +                               Storage_Scalar,
     +                               Storage_Start_Year
            IREC = IREC + 1
         ENDDO 
         CLOSE(10)
         CLOSE(11)
      ELSE IF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "coal_obj:0004")
      ENDIF
!
      SAVE_NUMBER_OF_COAL_STORAGE = MAX(0,IREC-1)
!
      RETURN



! OVERLAY THE COAL_STORAGE FILE

      ENTRY COAL_STORAGE_MAKEOVL(OVERLAY_FAMILY_NAME)

!
      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME = trim(DATA_DRIVE)//"GGO"//
     +                               trim(OVERLAY_FAMILY_NAME)//'.DAT'
      OPEN(10,FILE=FILE_NAME)
      READ(10,*) DELETE
      INUNIT = 12
      IF(COAL_STORAGE_OL == 'BC') THEN
         OPEN(11,FILE=trim(DATA_DRIVE)//
     +                             "BCCOAL_STORAGE.BIN",ACCESS="DIRECT",
     +                                                       RECL=LRECL)
         INUNIT = 11
      ENDIF
      OPEN(12,FILE=trim(DATA_DRIVE)//
     +                             "OLCOAL_STORAGE.BIN",ACCESS="DIRECT",
     +                                      STATUS="UNKNOWN",RECL=LRECL)
      IREC = 0
!
      READ(10,1000,IOSTAT=IOS) RECLN
      DOWHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE 
!              
         READ(10,1000,IOSTAT=IOS) RECLN
!         
      ENDDO
!      
      DO
!
         IREC = IREC + 1
         READ(INUNIT,REC=IREC,IOSTAT=IOS) DELETE,
     +                               Storage_Name,
     +                               COAL_Storage_Active,
     +                               Storage_ID_Number,
     +                               Storage_Initial_Capacity,
     +                               Storage_Demand_Markets,
     +                               Storage_Demand_Allocation,
     +                               Storage_Maximum_Capacity,
     +                               Storage_Demand_Slope,
     +                               Storage_Demand_Intercept,
     +                               Maximum_Daily_Injection,
     +                               Maximum_Daily_Withdrawal,
     +                               Storage_Weekly_Min_Capacity,
     +                               Storage_Weekly_Max_Capacity,
     +                               Storage_Scalar,
     +                               Storage_Start_Year
         IF(IOS /= 0) EXIT

         RECLN = trim(RECLN)//',,,,,,,,,,,,,'
         READ(RECLN,*,ERR=300) DELETE,
     +                               Storage_Name,
     +                               COAL_Storage_Active,
     +                               Storage_ID_Number,
     +                               Storage_Initial_Capacity,
     +                               Storage_Demand_Markets,
     +                               Storage_Demand_Allocation,
     +                               Storage_Maximum_Capacity,
     +                               Storage_Demand_Slope,
     +                               Storage_Demand_Intercept,
     +                               Maximum_Daily_Injection,
     +                               Maximum_Daily_Withdrawal,
     +                               Storage_Weekly_Min_Capacity,
     +                               Storage_Weekly_Max_Capacity,
     +                               Storage_Scalar,
     +                               Storage_Start_Year
!         ENDIF
!
         READ(10,1000,IOSTAT=IOS) RECLN
         DOWHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE 
!              
            READ(10,1000,IOSTAT=IOS) RECLN
!         
         ENDDO
         WRITE(12,REC=IREC) DELETE,
     +                               Storage_Name,
     +                               COAL_Storage_Active,
     +                               Storage_ID_Number,
     +                               Storage_Initial_Capacity,
     +                               Storage_Demand_Markets,
     +                               Storage_Demand_Allocation,
     +                               Storage_Maximum_Capacity,
     +                               Storage_Demand_Slope,
     +                               Storage_Demand_Intercept,
     +                               Maximum_Daily_Injection,
     +                               Maximum_Daily_Withdrawal,
     +                               Storage_Weekly_Min_Capacity,
     +                               Storage_Weekly_Max_Capacity,
     +                               Storage_Scalar,
     +                               Storage_Start_Year
! 
      ENDDO
      CLOSE(10)
      CLOSE(12)
      IF(COAL_STORAGE_OL == 'BC') CLOSE(11)
      COAL_STORAGE_OL = 'OL'
      RETURN
!

  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from coal_obj SIID58'
      call end_program(er_message)

  300 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      CALL MG_LOCATE_WRITE(21,0,
     +               'Error reading the COAL Storage record. Look for'//
     +                                    ' a "," in a character name.',
     +                     ALL_VERSIONS,1)
      er_message='stop requested from coal_obj SIID59'
      call end_program(er_message)
!

      ENTRY RESET_COAL_STORAGE_OL

         COAL_STORAGE_OL = 'BC'
      RETURN
!

      ENTRY OPEN_COAL_STORAGE_FILE

         OPEN(UNIT_NUM,
     +         FILE=trim(OUTPUT_DIRECTORY())//COAL_STORAGE_OL//
     +         "COAL_STORAGE.BIN",ACCESS="DIRECT",STATUS="UNKNOWN",
     +                                                   RECL=LRECL)
      RETURN
!

      ENTRY CLOSE_COAL_STORAGE_FILE

         CLOSE(UNIT_NUM)
      RETURN

      ENTRY GetCOALStorageBaseName(R_TEMP_NAME)

         R_TEMP_NAME = SAVE_BASE_FILE_NAME
      RETURN

      ENTRY DOES_COAL_STORAGE_FILE_EXIST(R_COAL_STORAGE_FILE_EXISTS)

         R_COAL_STORAGE_FILE_EXISTS = COAL_STORAGE_FILE_EXISTS
      RETURN

      ENTRY GET_MAX_COAL_STORAGE(R_NUMBER_OF_COAL_STORAGE)

         R_NUMBER_OF_COAL_STORAGE = SAVE_NUMBER_OF_COAL_STORAGE
      RETURN
!
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END

!
!
!     MAINTAINS LIST OF FROM/TO COAL_STORAGE AS WELL AS MW LOADING ON COAL_STORAGE
!     AND TIES
!
!
      RECURSIVE FUNCTION MANAGE_COAL_STORAGE_FORECASTS()
!
!         

      use spindriftlib
      use prod_arrays_dimensions
      USE SIZECOM

      SAVE
      LOGICAL*1   SAVE_COAL_STORAGE_FILE_EXISTS/.FALSE./,
     +            REDUCE_COAL_STORAGE_CAPACITY,
     +            GET_SCENARIO_PRICE_NUM,
     +            TEMP_L,
     +            ANNUAL_COAL_STORAGE_FORECAST,
     +            MONTHLY_COAL_STORAGE_FORECAST,
     +            GET_COAL_ANNUAL_PEAK_VOLUME,
     +            PROCESS_ANNUAL_COAL_STORAGE
      INTEGER*4   VALUES_2_ZERO
      INTEGER*2   I,J,K,CURRENT_RECORD,TRANS_GROUP,
     +            TEMP_I2,Z,
     +            R_NUM_OF_PRICING_GROUPS,
     +            GET_COAL_STORAGE,COAL_STORAGE_RECORDS,
     +            R_TRANS_GROUP,
     +            SAVE_COAL_STORAGE_RECORDS/0/,
     +            MAX_WHEELS/0/,MAX_COAL_STORAGE,
     +            MAX_IN_COAL_STORAGE,
     +            MAX_OUT_COAL_STORAGE,
     +            GET_MAX_IN_COAL_STORAGE,
     +            LOCAL_MAX_WHEELS,
     +            ACTIVE_LINK/0/,
     +            GET_ACTIVE_LINK_NUMBER,
     +            GET_COAL_STORAGE_ID,
     +            LOCAL_COAL_STORAGE_NUMBER,
     +            BEG_FO_HR_TL,
     +            END_FO_HR_TL,
     +            CURRENT_SELLER_COAL_STORAGE,
     +            CURRENT_BUYER_COAL_STORAGE,
     +            R_LP,
     +            L_LP,
     +            L1,L2,
     +            SAVE_MAX_COAL_STORAGE/0/,
     +            R_MONTH,R_YEAR,R_NODE
      INTEGER*2   R_SELLER,R_BUYER
      INTEGER*2   GET_HOUR_COAL_STORAGE_INDEX,
     +            GET_HOUR_COAL_STORAGE_FOR_LONG_COAL_STORAGE,
     +            GET_HOUR_WHEEL_COAL_STORAGE,
     +            R_COAL_STORAGE
!
      INTEGER*2   L,NUM,LAST_NUM
      REAL*4      GET_SEASON_CONSTRAINT_LIMIT,
     +            R_CAPACITY,CURRENT_COAL_STORAGE_MW,

     +            TEMP_CAPACITY,
     +            HOUR_TIE_LIMIT,
     +            COAL_STORAGE_WHEELING_CHARGE,COAL_STORAGE_SPREAD_RATE,
     +            LOCAL_WHEELING_CHARGE,LOCAL_PRICE_DELTA,
     +            LOCAL_DELTA_MULT,
     +            COAL_STORAGE_PRICE_DELTA,
     +            GET_PRICING_GROUP_SELL_PRICE,
     +            GET_PRICING_GROUP_BUY_PRICE,
     +            R_PRICING_GROUP_QUANT,
     +            GET_SCENARIO_TRANSMISSION_AVAIL,
     +            SCENARIO_TRANSMISSION_AVAIL,
     +            LOCAL_WHEEL_MULT,
     +            LOCAL_SPREAD_MULT,
     +            ESCALATED_MONTHLY_VALUE,
     +            REAL4_ONE/1./,
     +            CONSTRAINT_MULT,
     +            GET_TRANS_LINE_MULT,
     +            GET_CONSTRAINT_MULT,
     +            TEMP_R,
     +            GET_COAL_TRANSPORT_QUANT,
     +            GET_COAL_TRANSPORT_LOSSES,
     +            GET_COAL_TRANS_CONGEST_COST,
     +            R_CF,
     +            R_LOCAL_QUANT,
     +            R_LINK_CAP_USED,
     +            R_LINK_CAP,
     +            AVAIL_COAL_GIVEN_LINK_CONGEST,
     +            MAX_CAP,
     +            R_CONGESTION_QUANT,
     +            ANNUAL_PEAK,
     +            ANNUAL_VOLUME,
     +            DEMAND_PEAK,
     +            DEMAND_AVE
      LOGICAL*1   MANAGE_COAL_STORAGE_FORECASTS,
     +            TRANS_GROUP_ACTIVE_SWITCH,
     +            UPDATE_COAL_STORAGE_DATA,
     +            INIT_HOUR_COAL_STORAGE_LIMIT,
     +            COAL_STORAGE_DATA_READ/.TRUE./,
     +            GET_COAL_STORAGE_PARAMS,
     +            DETAILED_CONSTRAINTS_ACTIVE/.FALSE./,
     +            ARE_DETAILED_CONSTRAINTS_ACTIVE,
     +            COAL_DEMAND_GROUP_ACTIVE,
     +            COAL_SUPPLY_GROUP_ACTIVE
      LOGICAL*4   COAL_STORAGE_FILE_EXISTS
      CHARACTER*1 COAL_STORAGE_ACTIVE(:)
!
!
      LOGICAL*1 SET_HOUR_LONG_COAL_STORAGE_WI_COAL_STORAGE,
     +          SET_HOUR_LONG_CAP_TWH

      INTEGER*2 COAL_STORAGE_LONG_COAL_STORAGE/0/,L_BUYER,L_SELLER
      REAL*4    L_CAPACITY
!     
      CHARACTER*2   LOAD_FILE_CHAR_EXT
      CHARACTER*256 FILE_NAME,
     +              PRB_FILE_DIRECTORY
      CHARACTER*8 EEICODE=" "
      INTEGER*2 TIMZON,TEMPER,DELTMP,DAY,
     +          CURRENT_HR,DA,DAYS_IN_MONTH,LDE_YEAR,
     +          MARKET_DAY_OF_WEEK,CALENDAR_DAY_OF_WEEK,
     +          LDE_DAY,LDE_MONTH,DAY_WEEK,R_HOURS_IN_MONTH,
     +          ALINE_LOAD_DATA,IREC,HR,
     +          NUM_TRANS_GROUPS,
     +          MAX_COAL_GROUP_NUMBER,
     +          MAX_DEPTH/30/,
     +          GET_NUMBER_OF_ACTIVE_GROUPS,
     +          GET_MAX_COAL_GROUP_NUMBER,
     +          GET_TRANS_GROUP_INDEX,
     +          NUMBER_OF_TG,
     +          TRANS_ID,
     +          TEMP_K,
     +          NUM_STORAGE_NODES,
     +          NUM_DEMAND_MARKETS,
     +          NUM_ALLOC_MARKETS
      REAL*4    LOCAL_MARKET_PRICE(24)
!
      LOGICAL*4 FILE_EXISTS
!      
! 010303.
!
         INTEGER*2   R_END_POINT,R_DAY
         CHARACTER*9 R_MONTH_NAME
      LOGICAL*1   COAL_GROUP_ACTIVE_SWITCH,
     +            PROCESS_DAILY_STORAGE
      REAL*4 GET_NODE_DAILY_STORAGE,
     +       DAILY_MIN,DAILY_MAX,
     +       ADJUSTED_DEMAND,
     +       DAMPENING_FACTOR,
     +       GLOBAL_CF
! 070906.
      INTEGER (KIND=2) :: DELETE,YR,ZONE,POINTR
     +                    GET_COAL_GROUP_POSITION,
     +                    LOCAL_DAY,
     +                    LOCAL_YEAR/0/,
     +                    get_BASE_YEAR()
      INTEGER (KIND=4) :: IOS
      CHARACTER (LEN=4096) RECLN
      REAL (KIND=4) :: MONTHLY_DATA(12),
     +            Storage_Initial_Capacity(:),
     +            Storage_Current_Capacity(:),
     +            Storage_Demand_Markets(:),
     +            Storage_Demand_Allocation(:),
     +            Storage_Maximum_Capacity(:),
     +            Storage_Demand_Slope(:),
     +            Storage_Demand_Intercept(:),
     +            Maximum_Daily_Injection(:),
     +            Maximum_Daily_Withdrawal(:),
     +            Storage_Weekly_Min_Capacity(:),
     +            Storage_Weekly_Max_Capacity(:),
     +            Storage_Scalar(:),
     +            GET_VAR,
     +            TEMP_R4,
     +            DAILY_STORAGE_DEMAND(0:30),
     +            DAILY_STORAGE_ALLOC(0:30),
     +            DAILY_STORAGE_BY_GS(:),
     +            INTER,
     +            DAILY_INJECT_WITHDRAWAL(:),
     +            GET_DAILY_COAL_DEMAND_BY_NODE
      INTEGER (KIND=2) :: Storage_ID_Number(:),
     +                    MAX_STORAGE_ID,
     +                    Num_Storage_Demand_Markets(:),
     +                    Num_Storage_Alloc_Markets(:),
     +                    Storage_Demand_Index(:,:),
     +                    Storage_Alloc_Index(:,:),
     +                    Storage_Start_Year(:)
      CHARACTER (LEN=31) :: Storage_Name(:)
      ALLOCATABLE :: 
     +            Storage_Name,
     +            COAL_Storage_Active,
     +            Storage_ID_Number,
     +            Storage_Initial_Capacity,
     +            Storage_Current_Capacity,
     +            DAILY_INJECT_WITHDRAWAL,
     +            Storage_Demand_Markets,
     +            DAILY_STORAGE_BY_GS,
     +            Num_Storage_Alloc_Markets,
     +            Storage_Demand_Index,
     +            Storage_Alloc_Index,
     +            Storage_Demand_Allocation,
     +            Storage_Maximum_Capacity,
     +            Storage_Demand_Slope,
     +            Storage_Demand_Intercept,
     +            Maximum_Daily_Injection,
     +            Maximum_Daily_Withdrawal,
     +            Storage_Weekly_Min_Capacity,
     +            Storage_Weekly_Max_Capacity,
     +            Storage_Scalar,
     +            Storage_Start_Year,
     +            Num_Storage_Demand_Markets
!      
! END DATA DECLARATIONS      
!
         MANAGE_COAL_STORAGE_FORECASTS = .FALSE.
         SAVE_COAL_STORAGE_FILE_EXISTS = .FALSE.
!
         CALL DOES_COAL_STORAGE_FILE_EXIST(COAL_STORAGE_FILE_EXISTS)
!         
         IF(.NOT. COAL_STORAGE_FILE_EXISTS) RETURN
!
         CALL GET_MAX_COAL_STORAGE(MAX_IN_COAL_STORAGE) 
! 061207. SO THAT EACH LINK MAY BE BI-DIRECTIONAL.
         MAX_OUT_COAL_STORAGE = MAX_IN_COAL_STORAGE
!
         SAVE_MAX_COAL_STORAGE = MAX_IN_COAL_STORAGE
!
         CALL OPEN_COAL_STORAGE_FILE
!
         MAX_COAL_GROUP_NUMBER = GET_MAX_COAL_GROUP_NUMBER()
!
! FOR NOW. THIS IMPOSES A STRICT CONDITION ON USER TO CREATE STORAGE NODES.
! WOULD BE BETTER IF WE COULD REPRESENT EACH STORAGE FACILITY IN THIS FILE 
! AND THEN AGGREGATE ACCORDING TO THE NODES FILE.
!
         NUM_STORAGE_NODES =  MAX_OUT_COAL_STORAGE 
!
         NUMBER_OF_TG = 0
!
         IF(ALLOCATED(Storage_Name))
     +        DEALLOCATE(
     +            Storage_Name,
     +            COAL_Storage_Active,
     +            Storage_ID_Number,
     +            Num_Storage_Demand_Markets,
     +            Num_Storage_Alloc_Markets,
     +            Storage_Demand_Index,
     +            Storage_Alloc_Index,
     +            DAILY_STORAGE_BY_GS,
     +            Storage_Initial_Capacity,
     +            Storage_Current_Capacity,
     +            DAILY_INJECT_WITHDRAWAL,
     +            Storage_Demand_Markets,
     +            Storage_Demand_Allocation,
     +            Storage_Maximum_Capacity,
     +            Storage_Demand_Slope,
     +            Storage_Demand_Intercept,
     +            Maximum_Daily_Injection,
     +            Maximum_Daily_Withdrawal,
     +            Storage_Weekly_Min_Capacity,
     +            Storage_Weekly_Max_Capacity,
     +            Storage_Scalar,
     +            Storage_Start_Year)
         ALLOCATE(Storage_Name(MAX_OUT_COAL_STORAGE),
     +            COAL_Storage_Active(MAX_OUT_COAL_STORAGE),
     +            Storage_ID_Number(MAX_OUT_COAL_STORAGE),
     +            Num_Storage_Demand_Markets(MAX_OUT_COAL_STORAGE),
     +            Num_Storage_Alloc_Markets(MAX_OUT_COAL_STORAGE),
     +            Storage_Demand_Index(MAX_OUT_COAL_STORAGE,30),
     +            Storage_Alloc_Index(MAX_OUT_COAL_STORAGE,30),
     +            DAILY_STORAGE_BY_GS(0:MAX_COAL_GROUP_NUMBER),
     +            Storage_Initial_Capacity(0:MAX_OUT_COAL_STORAGE),
     +            Storage_Current_Capacity(0:MAX_OUT_COAL_STORAGE),
     +            DAILY_INJECT_WITHDRAWAL(0:MAX_OUT_COAL_STORAGE),
     +            Storage_Demand_Markets(MAX_OUT_COAL_STORAGE),
     +            Storage_Demand_Allocation(MAX_OUT_COAL_STORAGE),
     +            Storage_Maximum_Capacity(0:MAX_OUT_COAL_STORAGE),
     +            Storage_Demand_Slope(MAX_OUT_COAL_STORAGE),
     +            Storage_Demand_Intercept(MAX_OUT_COAL_STORAGE),
     +            Maximum_Daily_Injection(MAX_OUT_COAL_STORAGE),
     +            Maximum_Daily_Withdrawal(MAX_OUT_COAL_STORAGE),
     +            Storage_Weekly_Min_Capacity(MAX_OUT_COAL_STORAGE),
     +            Storage_Weekly_Max_Capacity(MAX_OUT_COAL_STORAGE),
     +            Storage_Scalar(MAX_OUT_COAL_STORAGE),
     +            Storage_Start_Year(MAX_OUT_COAL_STORAGE))
!         
         Num_Storage_Demand_Markets = 0
         Num_Storage_Alloc_Markets = 0
         DAILY_STORAGE_BY_GS = 0
         Storage_Demand_Index = 0
         Storage_Alloc_Index = 0
         Storage_Initial_Capacity = 0.
         Storage_Current_Capacity = 0.
         Storage_Maximum_Capacity = 0.
!
         ACTIVE_LINK = 1
! 
         DO CURRENT_RECORD = 1, MAX_IN_COAL_STORAGE
            READ(10,REC=CURRENT_RECORD) DELETE,
     +            Storage_Name(ACTIVE_LINK),
     +            COAL_Storage_Active(ACTIVE_LINK),
     +            Storage_ID_Number(ACTIVE_LINK),
     +            Storage_Initial_Capacity(ACTIVE_LINK),
     +            Storage_Demand_Markets(ACTIVE_LINK),
     +            Storage_Demand_Allocation(ACTIVE_LINK),
     +            Storage_Maximum_Capacity(ACTIVE_LINK),
     +            Storage_Demand_Slope(ACTIVE_LINK),
     +            Storage_Demand_Intercept(ACTIVE_LINK),
     +            Maximum_Daily_Injection(ACTIVE_LINK),
     +            Maximum_Daily_Withdrawal(ACTIVE_LINK),
     +            Storage_Weekly_Min_Capacity(ACTIVE_LINK),
     +            Storage_Weekly_Max_Capacity(ACTIVE_LINK),
     +            Storage_Scalar(ACTIVE_LINK),
     +            Storage_Start_Year(ACTIVE_LINK)
!
            Storage_Maximum_Capacity(0) = 
     +            Storage_Maximum_Capacity(0) +
     +                  Storage_Maximum_Capacity(ACTIVE_LINK)
            Storage_Current_Capacity(ACTIVE_LINK) =
     +                     MAX(0.,Storage_Initial_Capacity(ACTIVE_LINK))
            Storage_Current_Capacity(0) =
     +                  Storage_Current_Capacity(0) +
     +                           Storage_Current_Capacity(ACTIVE_LINK) 
!
            MAX_STORAGE_ID = MAX(MAX_STORAGE_ID,
     +                                   Storage_ID_Number(ACTIVE_LINK))
            IF(INDEX(COAL_Storage_Active(ACTIVE_LINK),'No') /= 0) CYCLE
!
            TEMP_R4 = Storage_Demand_Markets(ACTIVE_LINK)
            if(TEMP_R4 > 0.001) then
               I = 1
               TEMP_I2 = INT(Storage_Demand_Markets(ACTIVE_LINK))
               Storage_Demand_Index(ACTIVE_LINK,I) = TEMP_I2
               Num_Storage_Demand_Markets(ACTIVE_LINK) = 
     +                       Num_Storage_Demand_Markets(ACTIVE_LINK) + 1
            elseif(TEMP_R4 < -0.001) then
               DO I = 1, 30
                  TEMP_I2 = INT(GET_VAR(TEMP_R4,I,
     +                                          "Storage Demand Marke"))
                  IF(TEMP_I2 == 0) EXIT
                  Storage_Demand_Index(ACTIVE_LINK,I) = TEMP_I2
                  Num_Storage_Demand_Markets(ACTIVE_LINK) = 
     +                       Num_Storage_Demand_Markets(ACTIVE_LINK) + 1
            END DO
            endif
            TEMP_R4 = Storage_Demand_Allocation(ACTIVE_LINK)
            if(TEMP_R4 > 0.0001) then
               I = 1
               TEMP_I2 = INT(TEMP_R4)
               Storage_Alloc_Index(ACTIVE_LINK,I) = TEMP_I2
               Num_Storage_Alloc_Markets(ACTIVE_LINK) = 
     +                       Num_Storage_Alloc_Markets(ACTIVE_LINK) + 1
            elseif(TEMP_R4 < -0.001) then
               DO I = 1, 30
                  TEMP_I2 = INT(GET_VAR(TEMP_R4,I,
     +                                          "Storage Alloc Market"))
                  IF(TEMP_I2 == 0) EXIT
                  Storage_Alloc_Index(ACTIVE_LINK,I) = TEMP_I2
                  Num_Storage_Alloc_Markets(ACTIVE_LINK) = 
     +                       Num_Storage_Alloc_Markets(ACTIVE_LINK) + 1
               END DO
            endif
!
            ACTIVE_LINK = ACTIVE_LINK + 1
!
         ENDDO
!
         ACTIVE_LINK = ACTIVE_LINK - 1
!
         SAVE_COAL_STORAGE_FILE_EXISTS = .TRUE.
         MANAGE_COAL_STORAGE_FORECASTS = .TRUE.
!
      RETURN
! 021308.      

      ENTRY PROCESS_ANNUAL_COAL_STORAGE(R_YEAR)

         PROCESS_ANNUAL_COAL_STORAGE = .FALSE. 
         IF(.NOT. SAVE_COAL_STORAGE_FILE_EXISTS) RETURN
         LOCAL_YEAR = R_YEAR + get_base_year()
         DO I = 1, NUM_STORAGE_NODES
            NUM_DEMAND_MARKETS = Num_Storage_Demand_Markets(I)
            DEMAND_PEAK = 0.0
            DEMAND_AVE = 0.0
            DO J = 1, NUM_DEMAND_MARKETS
               TEMP_I2 = Storage_Demand_Index(I,J)
               TEMP_L = GET_COAL_ANNUAL_PEAK_VOLUME(   TEMP_I2,
     +                                                ANNUAL_PEAK,
     +                                                ANNUAL_VOLUME)
               DEMAND_PEAK = DEMAND_PEAK + ANNUAL_PEAK*0.9911 ! due to load shape
               DEMAND_AVE = DEMAND_AVE + ANNUAL_VOLUME
            ENDDO
            DEMAND_AVE = DEMAND_AVE / 365.
            TEMP_R4 = DEMAND_PEAK - DEMAND_AVE
            IF(DEMAND_PEAK <= 0.0 .OR. DEMAND_AVE <= 0.0 .OR.
     +                                              TEMP_R4 < .001) THEN
               Storage_Demand_Slope(I) = 1.0
               Storage_Demand_Intercept(I) = 0.0
            ELSE  
! 030908. temp for europe: to get storage to grow with demand.
               Storage_Demand_Slope(I) = .84
!               Maximum_Daily_Withdrawal(I) /
!     +                                                           TEMP_R4
               Storage_Demand_Intercept(I) = 0.84 * DEMAND_AVE
!!     +                      Maximum_Daily_Withdrawal(I) *
!     +                                         (1.- DEMAND_PEAK/TEMP_R4)
            ENDIF
         ENDDO
         PROCESS_ANNUAL_COAL_STORAGE = .TRUE.
      RETURN

      ENTRY PROCESS_DAILY_STORAGE(R_DAY,R_MONTH,R_YEAR)

         PROCESS_DAILY_STORAGE = .FALSE. 
         IF(.NOT. SAVE_COAL_STORAGE_FILE_EXISTS) RETURN
         PROCESS_DAILY_STORAGE = .TRUE. 
         DAILY_STORAGE_BY_GS = 0.0
         DAILY_INJECT_WITHDRAWAL = 0.0
!

         IF(Storage_Maximum_Capacity(0) > 0.0) THEN
            GLOBAL_CF = Storage_Current_Capacity(0)/
     +                                       Storage_Maximum_Capacity(0)
         ELSE
            GLOBAL_CF = 0.0
         ENDIF
         DAMPENING_FACTOR = MIN(1.0,MAX(0.0,3.8906*GLOBAL_CF-2.9121))
!
!
! THE GET_VAR'S SHOULD ONLY GET CALLED ONCE ABOVE AND PUT INTO ARRAYS.
!         
         DO I = 1, NUM_STORAGE_NODES
            IF(LOCAL_YEAR < Storage_Start_Year(I)) CYCLE   
! THIS IS THE INDEPENDENT VARIABLE         
            NUM_DEMAND_MARKETS = Num_Storage_Demand_Markets(I)
            DAILY_STORAGE_DEMAND = 0.0
            DO J = 1, NUM_DEMAND_MARKETS

               TEMP_I2 = Storage_Demand_Index(I,J)
               TEMP_R4 = 
     +                GET_DAILY_COAL_DEMAND_BY_NODE(R_DAY,TEMP_I2,INTER)
               DAILY_STORAGE_DEMAND(J) = DAILY_STORAGE_DEMAND(J) +
     +                                                           TEMP_R4
               DAILY_STORAGE_DEMAND(0) = DAILY_STORAGE_DEMAND(0) +
     +                                                           TEMP_R4
            END DO
            LOCAL_DAY = MIN(30,R_DAY)
            DAILY_MIN = 1000.*
     +                  ESCALATED_MONTHLY_VALUE(
     +                     REAL4_ONE,
     +                     INT2(ABS(Storage_Weekly_Min_Capacity(I))),
     +                                       LOCAL_DAY,R_MONTH,INT2(1))
            DAILY_MAX = 1000.*
     +                  ESCALATED_MONTHLY_VALUE(
     +                     REAL4_ONE,
     +                     INT2(ABS(Storage_Weekly_Max_Capacity(I))),
     +                                       LOCAL_DAY,R_MONTH,INT2(1))
!
! 010508


            DAILY_INJECT_WITHDRAWAL(I) = ! ADJUSTED_DEMAND * 
     +                                           DAILY_STORAGE_DEMAND(0)
!     
! POSITIVE IS WITHDRAWAL. NEGATIVE IS INJECTION.            
            DAILY_INJECT_WITHDRAWAL(I) = 
     +         DAILY_INJECT_WITHDRAWAL(I) * Storage_Demand_Slope(I) - 
     +                                       Storage_Demand_Intercept(I)

            IF(Storage_Scalar(I) > 0.0) THEN
               ADJUSTED_DEMAND = Storage_Scalar(I) ! 1.0
            ELSE
               ADJUSTED_DEMAND = 
     +                  ESCALATED_MONTHLY_VALUE(
     +                     REAL4_ONE,
     +                     INT2(ABS(Storage_Scalar(I))),
     +                                       R_YEAR,R_MONTH,INT2(1))
            ENDIF
            IF(DAILY_INJECT_WITHDRAWAL(I) > 0.0) THEN
               DAILY_INJECT_WITHDRAWAL(I) = 
     +            MIN(DAILY_INJECT_WITHDRAWAL(I),
     +                                      Maximum_Daily_Withdrawal(I))

            ELSE
               DAILY_INJECT_WITHDRAWAL(I) = 
     +            MAX(DAILY_INJECT_WITHDRAWAL(I),
     +                                      -Maximum_Daily_Injection(I))

            ENDIF
! 021408. MOVED BELOW THE MAX'S.
            DAILY_INJECT_WITHDRAWAL(I) = DAILY_INJECT_WITHDRAWAL(I) * 
     +                                                   ADJUSTED_DEMAND
            DAILY_INJECT_WITHDRAWAL(0) = 
     +            DAILY_INJECT_WITHDRAWAL(0) + 
     +                     DAILY_INJECT_WITHDRAWAL(I)
!         ENDDO

            Storage_Current_Capacity(I) = Storage_Current_Capacity(I) -
     +                                        DAILY_INJECT_WITHDRAWAL(I)
            Storage_Current_Capacity(0) = Storage_Current_Capacity(0) -
     +                                        DAILY_INJECT_WITHDRAWAL(I)
         ENDDO
!
         DO I = 1, NUM_STORAGE_NODES
!
            IF(LOCAL_YEAR < Storage_Start_Year(I)) CYCLE   
!
! THIS IS THE DEPENDENT VARIABLE            
            DAILY_STORAGE_ALLOC = 0.0
            NUM_ALLOC_MARKETS = Num_Storage_Alloc_Markets(I)
            DO J = 1, NUM_ALLOC_MARKETS

               TEMP_I2 = Storage_Alloc_Index(I,J)
               TEMP_R4 = 
     +                GET_DAILY_COAL_DEMAND_BY_NODE(R_DAY,TEMP_I2,INTER)
               DAILY_STORAGE_ALLOC(J) = DAILY_STORAGE_ALLOC(J) +
     +                                                           TEMP_R4
               DAILY_STORAGE_ALLOC(0) = DAILY_STORAGE_ALLOC(0) +
     +                                                           TEMP_R4
            END DO
            IF(DAILY_STORAGE_ALLOC(0) > 0.0) THEN
               NUM_ALLOC_MARKETS = Num_Storage_Alloc_Markets(I)
               DO J = 1, NUM_ALLOC_MARKETS
                  TEMP_I2 = Storage_Alloc_Index(I,J)
                  TEMP_R4 = DAILY_STORAGE_ALLOC(J) /
     +                                           DAILY_STORAGE_ALLOC(0)
                  TEMP_R4 = TEMP_R4 * DAILY_INJECT_WITHDRAWAL(I)
                  DAILY_STORAGE_BY_GS(TEMP_I2) = 
     +                   DAILY_STORAGE_BY_GS(TEMP_I2) + TEMP_R4
                  DAILY_STORAGE_BY_GS(0) = 
     +                   DAILY_STORAGE_BY_GS(0) + TEMP_R4
               ENDDO
            ENDIF
!            
         END DO
      RETURN

      ENTRY GET_NODE_DAILY_STORAGE(R_NODE)

!
! R_NODE IS GS THE COAL NODE INDEX
!
         GET_NODE_DAILY_STORAGE = DAILY_STORAGE_BY_GS(R_NODE) 
!         
      RETURN
      END

!
!                  ROUTINE TO CONVERT COAL_LINKS FILE
!
!                          COPYRIGHT (C) 2006
!                        GLOBAL ENERGY DECISIONS
!                          ALL RIGHTS RESERVED
!

!
! 070906. CREATED.
! 070906. MAY NEED TO MAKE THIS A MULTI-FILE SPECIFICATION.
!
      SUBROUTINE COAL_LINK_OBJECT
!

      use spindriftlib
      use prod_arrays_dimensions
      USE SIZECOM

      INTEGER*2   UNIT_NUM/10/
      INTEGER*2 INUNIT,IREC,LRECL/328/ ! ADDED 20 VARIABLES ON 3/18/07.
!      INTEGER IOS
!      
      INTEGER*2   SAVE_NUMBER_OF_COAL_LINKS/0/,
     +            R_NUMBER_OF_COAL_LINKS,R_WHEELS,
     +            COAL_LINK_OWNER
      INTEGER*2   TRANS_LINE_INDEX
      CHARACTER*5 BASE_FILE_NAME,OVERLAY_FAMILY_NAME,
     +            COAL_TRANSPORT_FILE 
      CHARACTER*256 FILE_NAME
      CHARACTER*255 SAVE_BASE_FILE_NAME,R_TEMP_NAME
      CHARACTER*256 BASE_FILE_DIRECTORY
      CHARACTER*256 DATA_DRIVE,OUTPUT_DIRECTORY
      CHARACTER*1  COAL_LINK_ACTIVE
      LOGICAL*4    FILE_EXISTS/.FALSE./,COAL_LINK_FILE_EXISTS/.FALSE./,
     +             R_COAL_LINK_FILE_EXISTS
! DECLARATION FOR DBREAD COMMON BLOCK
!      CHARACTER*1024 RECLN
! DECLARATION FOR TRANSACT COAL_LINK DETERMINANTS
      CHARACTER*16 FILE_TYPE/'COAL LINK FORECAST    '/
      CHARACTER*2  COAL_LINK_OL/'BC'/
      LOGICAL*1 LAHEY_LF95
      CHARACTER*30 SCREEN_OUTPUT

! 070906.

      INTEGER (KIND=2) :: DELETE,YR,ZONE,POINTR,
     +                    PRC_MULTI_ZONE_TIE_LIMIT_FILES,I,

     +                    TieID
      INTEGER (KIND=4) :: IOS
      CHARACTER (LEN=4096) RECLN
      REAL (KIND=4) :: ExpansionCost,
     +                 ExpansionCostEscalation,
     +                 MaximumExpansion,
     +                 ExpansionCCRF,
     +                 PipelineID,
     +                 PipelineCapacity,
     +                 PipelineDistance,
     +                 CONGESTION_COST_ESC,
     +                 OUTAGE_RATES,
     +                 LINE_LOSSES_A_TO_B,
     +                 LINE_LOSSES_B_TO_A,
     +                 ZONE_A_TO_B_PEAK_TIES,
     +                 ZONE__A_TO_B_OFF_PEAK_TIES,
     +                 ZONE__A_TO_B_WHEELING_RATES,
     +                 ZONE_B_TO_A_PEAK_TIES,
     +                 ZONE__B_TO_A_OFF_PEAK_TIES,
     +                 ZONE__B_TO_A_WHEELING_RATES,
     +                 ESCALATION_RATE,
     +                 ZONE_A_TO_B_PLANNING_FACTOR,
     +                 ZONE_B_TO_A_PLANNING_FACTOR,
     +                 CONGESTION_PERCENT(10),
     +                 CONGESTION_COST(10)
      INTEGER (KIND=2) :: ZONE_A_GROUP_ID,
     +                    ZONE_B_GROUP_ID,
     +                    FIND_TRANS_GROUP_ID
      CHARACTER (LEN=31) :: ZONE_A,ZONE_B,
     +                      COMMENT,
     +                      ZONE_A_FULL_NAME,
     +                      ZONE_B_FULL_NAME

      CHARACTER (LEN=3) :: ActiveConstraint
      CHARACTER (LEN=50) :: TieDescription
      SAVE SAVE_BASE_FILE_NAME
! CONVERT THE COAL_LINK FILE
!
!
!

      ENTRY COAL_LINK_MAKEBIN

      BASE_FILE_NAME = COAL_TRANSPORT_FILE ()
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME = trim(BASE_FILE_DIRECTORY())//
     +                             "GTB"//trim(BASE_FILE_NAME)//'.DAT'
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      COAL_LINK_FILE_EXISTS = FILE_EXISTS
      IF(FILE_EXISTS) THEN
         SAVE_BASE_FILE_NAME = FILE_NAME
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_LOCATE_WRITE(16,30,BASE_FILE_NAME,ALL_VERSIONS,0)
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         ENDIF
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=trim(DATA_DRIVE)//
     +                      "BCCOAL_LINK.BIN",ACCESS="DIRECT",
     +                                      STATUS="UNKNOWN",RECL=LRECL)
         IREC = 1
!
!         
!         
         CONGESTION_PERCENT = 0.0 
         CONGESTION_COST = 0.0
         CONGESTION_COST_ESC = 0.0
!
!
         READ(10,1000,IOSTAT=IOS) RECLN
!         
         DO
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(IOS /= 0) EXIT
            READ(RECLN,*,ERR=200) DELETE
!            
            IF(DELETE == 7) CYCLE ! NEW TABLE
!            
            RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
!            
!            
            READ(RECLN,*,ERR=200) DELETE,
     +                               TieDescription,
     +                               ActiveConstraint,
     +                               ExpansionCost,
     +                               ExpansionCostEscalation,
     +                               LINE_LOSSES_A_TO_B,
     +                               OUTAGE_RATES,
     +                               ZONE_A,
     +                               ZONE_A_TO_B_PEAK_TIES,
     +                               ZONE__A_TO_B_OFF_PEAK_TIES,
     +                               ZONE__A_TO_B_WHEELING_RATES,
     +                               ZONE_B,
     +                               ZONE_B_TO_A_PEAK_TIES,
     +                               ZONE__B_TO_A_OFF_PEAK_TIES,
     +                               ZONE__B_TO_A_WHEELING_RATES,
     +                               TieID,
     +                               COMMENT,
     +                               ZONE_A_GROUP_ID,
     +                               ZONE_B_GROUP_ID,
     +                               ZONE_A_TO_B_PLANNING_FACTOR,
     +                               ZONE_B_TO_A_PLANNING_FACTOR,
     +                               LINE_LOSSES_B_TO_A,
     +                               CONGESTION_PERCENT,
     +                               CONGESTION_COST,
     +                               PipelineDistance,
     +                               MaximumExpansion,
     +                               ExpansionCCRF,
     +                               PipelineID,
     +                               PipelineCapacity,
     +                               CONGESTION_COST_ESC
            WRITE(11,REC=IREC) DELETE,
     +                               TieDescription,
     +                               ActiveConstraint,
     +                               ExpansionCost,
     +                               ExpansionCostEscalation,
     +                               LINE_LOSSES_A_TO_B,
     +                               OUTAGE_RATES,
     +                               ZONE_A,
     +                               ZONE_A_TO_B_PEAK_TIES,
     +                               ZONE__A_TO_B_OFF_PEAK_TIES,
     +                               ZONE__A_TO_B_WHEELING_RATES,
     +                               ZONE_B,
     +                               ZONE_B_TO_A_PEAK_TIES,
     +                               ZONE__B_TO_A_OFF_PEAK_TIES,
     +                               ZONE__B_TO_A_WHEELING_RATES,
     +                               TieID,
     +                               COMMENT,
     +                               ZONE_A_GROUP_ID,
     +                               ZONE_B_GROUP_ID,
     +                               ZONE_A_TO_B_PLANNING_FACTOR,
     +                               ZONE_B_TO_A_PLANNING_FACTOR,
     +                               LINE_LOSSES_B_TO_A,
     +                               CONGESTION_PERCENT,
     +                               CONGESTION_COST,
     +                               PipelineDistance,
     +                               MaximumExpansion,
     +                               ExpansionCCRF,
     +                               PipelineID,
     +                               PipelineCapacity,
     +                               CONGESTION_COST_ESC
            IREC = IREC + 1
         ENDDO 
         CLOSE(10)
         CLOSE(11)
      ELSE IF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "coal_objt:0003")
      ENDIF
!
      SAVE_NUMBER_OF_COAL_LINKS = MAX(0,IREC-1)
!
      RETURN



! OVERLAY THE COAL_LINK FILE

      ENTRY COAL_LINK_MAKEOVL(OVERLAY_FAMILY_NAME)

!
      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME = trim(DATA_DRIVE)//"GTO"//
     +                               trim(OVERLAY_FAMILY_NAME)//'.DAT'
      OPEN(10,FILE=FILE_NAME)
      READ(10,*) DELETE
      INUNIT = 12
      IF(COAL_LINK_OL == 'BC') THEN
         OPEN(11,FILE=trim(DATA_DRIVE)//
     +                               "BCCOAL_LINK.BIN",ACCESS="DIRECT",
     +                                                       RECL=LRECL)
         INUNIT = 11
      ENDIF
      OPEN(12,FILE=trim(DATA_DRIVE)//
     +                               "OLCOAL_LINK.BIN",ACCESS="DIRECT",
     +                                      STATUS="UNKNOWN",RECL=LRECL)
      IREC = 0
!
      READ(10,1000,IOSTAT=IOS) RECLN
      DOWHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE 
!              
         READ(10,1000,IOSTAT=IOS) RECLN
!         
      ENDDO
!      
      DO
!
         IREC = IREC + 1
         READ(INUNIT,REC=IREC,IOSTAT=IOS) DELETE,
     +                               TieDescription,
     +                               ActiveConstraint,
     +                               ExpansionCost,
     +                               ExpansionCostEscalation,
     +                               LINE_LOSSES_A_TO_B,
     +                               OUTAGE_RATES,
     +                               ZONE_A,
     +                               ZONE_A_TO_B_PEAK_TIES,
     +                               ZONE__A_TO_B_OFF_PEAK_TIES,
     +                               ZONE__A_TO_B_WHEELING_RATES,
     +                               ZONE_B,
     +                               ZONE_B_TO_A_PEAK_TIES,
     +                               ZONE__B_TO_A_OFF_PEAK_TIES,
     +                               ZONE__B_TO_A_WHEELING_RATES,
     +                               TieID,
     +                               COMMENT,
     +                               ZONE_A_GROUP_ID,
     +                               ZONE_B_GROUP_ID,
     +                               ZONE_A_TO_B_PLANNING_FACTOR,
     +                               ZONE_B_TO_A_PLANNING_FACTOR,
     +                               LINE_LOSSES_B_TO_A,
     +                               CONGESTION_PERCENT,
     +                               CONGESTION_COST,
     +                               PipelineDistance,
     +                               MaximumExpansion,
     +                               ExpansionCCRF,
     +                               PipelineID,
     +                               PipelineCapacity,
     +                               CONGESTION_COST_ESC
         IF(IOS /= 0) EXIT

         RECLN = trim(RECLN)//',,,,,,,,,,,,,'
         READ(RECLN,*,ERR=300) DELETE,
     +                               TieDescription,
     +                               ActiveConstraint,
     +                               ExpansionCost,
     +                               ExpansionCostEscalation,
     +                               LINE_LOSSES_A_TO_B,
     +                               OUTAGE_RATES,
     +                               ZONE_A,
     +                               ZONE_A_TO_B_PEAK_TIES,
     +                               ZONE__A_TO_B_OFF_PEAK_TIES,
     +                               ZONE__A_TO_B_WHEELING_RATES,
     +                               ZONE_B,
     +                               ZONE_B_TO_A_PEAK_TIES,
     +                               ZONE__B_TO_A_OFF_PEAK_TIES,
     +                               ZONE__B_TO_A_WHEELING_RATES,
     +                               TieID,
     +                               COMMENT,
     +                               ZONE_A_GROUP_ID,
     +                               ZONE_B_GROUP_ID,
     +                               ZONE_A_TO_B_PLANNING_FACTOR,
     +                               ZONE_B_TO_A_PLANNING_FACTOR,
     +                               LINE_LOSSES_B_TO_A,
     +                               CONGESTION_PERCENT,
     +                               CONGESTION_COST,
     +                               PipelineDistance,
     +                               MaximumExpansion,
     +                               ExpansionCCRF,
     +                               PipelineID,
     +                               PipelineCapacity,
     +                               CONGESTION_COST_ESC
!         ENDIF
!
         READ(10,1000,IOSTAT=IOS) RECLN
         DOWHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE 
!              
            READ(10,1000,IOSTAT=IOS) RECLN
!         
         ENDDO
         WRITE(12,REC=IREC) DELETE,
     +                               TieDescription,
     +                               ActiveConstraint,
     +                               ExpansionCost,
     +                               ExpansionCostEscalation,
     +                               LINE_LOSSES_A_TO_B,
     +                               OUTAGE_RATES,
     +                               ZONE_A,
     +                               ZONE_A_TO_B_PEAK_TIES,
     +                               ZONE__A_TO_B_OFF_PEAK_TIES,
     +                               ZONE__A_TO_B_WHEELING_RATES,
     +                               ZONE_B,
     +                               ZONE_B_TO_A_PEAK_TIES,
     +                               ZONE__B_TO_A_OFF_PEAK_TIES,
     +                               ZONE__B_TO_A_WHEELING_RATES,
     +                               TieID,
     +                               COMMENT,
     +                               ZONE_A_GROUP_ID,
     +                               ZONE_B_GROUP_ID,
     +                               ZONE_A_TO_B_PLANNING_FACTOR,
     +                               ZONE_B_TO_A_PLANNING_FACTOR,
     +                               LINE_LOSSES_B_TO_A,
     +                               CONGESTION_PERCENT,
     +                               CONGESTION_COST,
     +                               PipelineDistance,
     +                               MaximumExpansion,
     +                               ExpansionCCRF,
     +                               PipelineID,
     +                               PipelineCapacity,
     +                               CONGESTION_COST_ESC
! 
      ENDDO
      CLOSE(10)
      CLOSE(12)
      IF(COAL_LINK_OL == 'BC') CLOSE(11)
      COAL_LINK_OL = 'OL'
      RETURN
!

  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from coal_obj SIID60'
      call end_program(er_message)

  300 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      CALL MG_LOCATE_WRITE(21,0,
     +                  'Error reading the COAL Link record. Look for'//
     +                                    ' a "," in a character name.',
     +                     ALL_VERSIONS,1)
      er_message='stop requested from coal_obj SIID61'
      call end_program(er_message)
!

      ENTRY RESET_COAL_LINK_OL

         COAL_LINK_OL = 'BC'
      RETURN
!

      ENTRY OPEN_COAL_LINK_FILE

         OPEN(UNIT_NUM,FILE=trim(OUTPUT_DIRECTORY())//COAL_LINK_OL//
     +     "COAL_LINK.BIN",ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      RETURN
!

      ENTRY CLOSE_COAL_LINK_FILE

         CLOSE(UNIT_NUM)
      RETURN

      ENTRY GetCOALLinkBaseName(R_TEMP_NAME)

         R_TEMP_NAME = SAVE_BASE_FILE_NAME
      RETURN

      ENTRY DOES_COAL_LINK_FILE_EXIST(R_COAL_LINK_FILE_EXISTS)

         R_COAL_LINK_FILE_EXISTS = COAL_LINK_FILE_EXISTS
      RETURN

      ENTRY GET_MAX_COAL_LINKS(R_NUMBER_OF_COAL_LINKS)

         R_NUMBER_OF_COAL_LINKS = SAVE_NUMBER_OF_COAL_LINKS
      RETURN
!
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END

!
!
!     MAINTAINS LIST OF FROM/TO COAL_LINKS AS WELL AS MW LOADING ON COAL_LINKS
!     AND TIES
!
!
      RECURSIVE FUNCTION MANAGE_COAL_LINK_FORECASTS()
      use end_routine, only: end_program, er_message
!
!         

      use spindriftlib
      use prod_arrays_dimensions
      USE SIZECOM

      SAVE
      LOGICAL*1   SAVE_COAL_LINK_FILE_EXISTS/.FALSE./,
     +            REDUCE_COAL_LINK_CAPACITY,
     +            HOUR_COAL_LINK_ACTIVE,
     +            THIS_IS_A_LONG_COAL_LINK,
     +            SET_HOUR_LONG_COAL_LINK_PARAMS,
     +            MULTI_AREA_PRICING_FOR_MONTH,
     +            MULTI_AREA_BUY_PRICE_FOR_MONTH,
     +            GET_SCENARIO_PRICE_NUM,
     +            TEMP_L,
     +            ANNUAL_COAL_LINK_FORECAST,
     +            MONTHLY_COAL_LINK_INIT,
     +            MONTHLY_COAL_LINK_FORECAST      
      INTEGER*4   VALUES_2_ZERO
      INTEGER*2   I,J,K,CURRENT_RECORD,TRANS_GROUP,
     +            TEMP_I2,Z,
     +            R_NUM_OF_PRICING_GROUPS,
     +            GET_COAL_LINK,COAL_LINK_RECORDS,
     +            R_TRANS_GROUP,
     +            SAVE_COAL_LINK_RECORDS/0/,
     +            MAX_WHEELS/0/,MAX_COAL_LINKS,
     +            MAX_IN_COAL_LINKS,
     +            MAX_OUT_COAL_LINKS,
     +            GET_MAX_IN_COAL_LINKS,
     +            LOCAL_MAX_WHEELS,
     +            ACTIVE_LINK/0/,
     +            GET_ACTIVE_LINK_NUMBER,
     +            GET_COAL_LINK_ID,
     +            LOCAL_COAL_LINK_NUMBER,
     +            BEG_FO_HR_TL,
     +            END_FO_HR_TL,
     +            CURRENT_SELLER_COAL_LINK,
     +            CURRENT_BUYER_COAL_LINK,
     +            R_LP,
     +            L_LP,
     +            L1,L2,
     +            SAVE_MAX_COAL_LINKS/0/,
     +            R_MONTH,R_YEAR
      INTEGER*2   R_SELLER,R_BUYER
      INTEGER*2   GET_HOUR_COAL_LINKS_INDEX,
     +            GET_HOUR_COAL_LINK_FOR_LONG_COAL_LINK,
     +            GET_HOUR_WHEEL_COAL_LINK,
     +            R_COAL_LINK,
     +            R_DEPTH,
     +            RECURSIVE_COUNT
!
      INTEGER     TOTAL_PATHS_FOR_I,
     +            TOTAL_PATHS_FOR_SYSTEM,
     +            GET_TOTAL_PATHS_FOR_SYSTEM,
     +            R_PATH,
     +            J_PATH(:),   ! BIG ARRAY, VALUE IS ALSO BIG.
     +            PATH(:),   ! INDEX IS VERY BIG. VALUE OF PATH IS SMALL
     +            PARENT(:), ! VERY BIG ARRAY, VALUE IS ALSO BIG.
     +            NODE(:),
     +            I_PATH(:),
     +            I_PARENT(:), ! VERY BIG ARRAY, VALUE IS ALSO BIG.
     +            NUMBER_OF_PATHS_FOR_I(:),
     +            PATH_FOR_J_TO_K(:,:), ! HIGHLY INDEXED ARRAY: COUNT,VLI
     +            GET_PATH_FOR_J_TO_K,
     +            GET_PARENT_OF_LEAF,
     +            GET_PARENT_PATH_OF_LEAF,
     +            GET_LEAF_OF_PATH,
     +            LAST_NODE_SUM,
     +            NUMBER_OF_J,NUMBER_OF_K,M,N,P,Q,LAST_Q,
     +            GET_NUMBER_OF_SUPPLY_PATHS,
     +            NUMBER_OF_DEMAND_PATHS
      INTEGER*4   
!     +            J_PATH(:),   ! BIG 
     +            K_PATH(:),   ! BIG
     +            SET_OF_J(:), ! BIG
     +            SET_OF_K(:) ! BIG
      INTEGER*2   L,NUM,LAST_NUM,
     +            R_A,R_B,R_C,

     +            ACTIVE_NODES,
     +            GET_NODE_OF_PATH,
     +            GET_NUMBER_OF_COAL_ACTIVE_GROUPS,
     +            GET_MAX_C_FOR_,
     +            MAX_C_FOR_(:),
     +            MIN_C_FOR_(:)
      REAL*4      GET_SEASON_CONSTRAINT_LIMIT,
     +            R_CAPACITY,CURRENT_COAL_LINK_MW,

     +            TEMP_CAPACITY,
     +            HOUR_TIE_LIMIT,
     +            COAL_LINK_WHEELING_CHARGE,COAL_LINK_SPREAD_RATE,
     +            LOCAL_WHEELING_CHARGE,LOCAL_PRICE_DELTA,
     +            LOCAL_DELTA_MULT,
     +            COAL_LINK_PRICE_DELTA,
     +            GET_PRICING_GROUP_SELL_PRICE,
     +            GET_PRICING_GROUP_BUY_PRICE,
     +            R_PRICING_GROUP_QUANT,
     +            GET_SCENARIO_TRANSMISSION_AVAIL,
     +            SCENARIO_TRANSMISSION_AVAIL,
     +            LOCAL_WHEEL_MULT,
     +            LOCAL_SPREAD_MULT,
     +            ESCALATED_MONTHLY_VALUE,
     +            REAL4_ONE/1./,
     +            CONSTRAINT_MULT,
     +            GET_TRANS_LINE_MULT,
     +            GET_CONSTRAINT_MULT,
     +            TEMP_R,
     +            R_CONGEST_VECTOR(10), ! NUMBER OF CONGESTION POINTS
     +            GET_COAL_TRANSPORT_RATE,
     +            GET_COAL_TRANSPORT_QUANT,
     +            GET_COAL_LINK_QUANT,
     +            GET_COAL_TRANSPORT_LOSSES,
     +            GET_COAL_TRANS_CONGEST_COST,
     +            R_CF,
     +            R_LOCAL_QUANT,
     +            R_LINK_CAP_USED,
     +            R_LINK_CAP,
     +            AVAIL_COAL_GIVEN_LINK_CONGEST,
     +            MAX_CAP,
     +            R_CONGESTION_QUANT

      LOGICAL*1   MANAGE_COAL_LINK_FORECASTS,TRANS_GROUP_ACTIVE_SWITCH,
     +            UPDATE_COAL_LINK_DATA,
     +            GET_COAL_TRANSPORT_CONGESTION,
     +            INIT_HOUR_COAL_LINK_LIMIT,
     +            COAL_LINK_DATA_READ/.TRUE./,
     +            GET_COAL_LINK_PARAMS,
     +            DETAILED_CONSTRAINTS_ACTIVE/.FALSE./,
     +            ARE_DETAILED_CONSTRAINTS_ACTIVE,
     +            COAL_DEMAND_GROUP_ACTIVE,
     +            COAL_SUPPLY_GROUP_ACTIVE,
     +            REDUNDANT_PATH,
     +            ACTIVE_DEMAND_NODE(:),
     +            ACTIVE_SUPPLY_NODE(:),
     +            TEST_COAL_LINK_CF
      LOGICAL*4   COAL_LINK_FILE_EXISTS
      CHARACTER*1 COAL_LINK_ACTIVE
!
!
      LOGICAL*1 SET_HOUR_LONG_COAL_LINK_WI_COAL_LINKS,
     +          SET_HOUR_LONG_CAP_TWH

      INTEGER*2 COAL_LINKS_LONG_COAL_LINKS/0/,L_BUYER,L_SELLER
      REAL*4    L_CAPACITY
!     
      CHARACTER*2   LOAD_FILE_CHAR_EXT
      CHARACTER*256 FILE_NAME,
     +              PRB_FILE_DIRECTORY
      CHARACTER*8 EEICODE=" "
      integer (kind=2), save, allocatable :: TRANS_GROUP_POSITION(:)
      
      INTEGER*2 TIMZON,TEMPER,DELTMP,DAY,
     +          CURRENT_HR,DA,DAYS_IN_MONTH,LDE_YEAR,
     +          MARKET_DAY_OF_WEEK,CALENDAR_DAY_OF_WEEK,
     +          LDE_DAY,LDE_MONTH,DAY_WEEK,R_HOURS_IN_MONTH,
     +          ALINE_LOAD_DATA,IREC,HR,
     +          NUM_TRANS_GROUPS,
     +          MAX_COAL_GROUP_NUMBER,
     +          MAX_DEPTH/30/,
     +          GET_NUMBER_OF_ACTIVE_GROUPS,
     +          GET_MAX_COAL_GROUP_NUMBER,
     +          GET_TRANS_GROUP_INDEX,
     +          NUMBER_OF_TG,
     +          TRANS_ID,
     +          TEMP_K,
     +          iPort(:),
     +          SNamePos(:),
     +          GET_LINK_ALPHA_ORDER
      REAL*4    LOCAL_MARKET_PRICE(24)
!
      LOGICAL*4 FILE_EXISTS
      integer*2 iLine,iNode,jNode,COAL_LINKID
      real*4 Impedance,LineVoltageLevel,LinePowerLimit
!      
! 010303.
!
         INTEGER*2   R_END_POINT,R_DAY
         CHARACTER*9 R_MONTH_NAME
         LOGICAL*1   HOURLY_FLOW_SUMMARY_REPORT,
     +               HOURLY_FLOW_REPORT_NOT_OPEN/.TRUE./,
     +               DAILY_COAL_LINKS_REPORTS,
     +               YES_WRITE_DAILY_COAL_LINKS_REPORTS,
     +               HOURLY_COAL_LINKS_REPORTS,
     +               WRITE_DAILY_COAL_LINKS_REPORTS/.FALSE./,
     +               COAL_GROUP_ACTIVE_SWITCH,
     +               LINK_PAIR(:,:),
     +               GET_LINK_NAME
! 070906.
      INTEGER (KIND=2) :: DELETE,YR,ZONE,POINTR,
     +                    TieID(:),A_ID,B_ID,
     +                    LINK_INDEX(:,:),
     +                    I_TO_K_PATHS(:,:),
     +                    J_TO_K_PATHS(:,:),
     +                    VLI(:,:), ! VALID LINK INDEX: J,K 
     +                    VLI_COUNT,
     +                    GET_NUMBER_OF_J_TO_K_PATHS,
     +                    MAX_I_TO_K_PATHS/100/,
     +                    MAX_J_TO_K_PATHS/500/, ! BIG: NEEDS TO BE BIGGER?
     +                    GET_COAL_GROUP_POSITION,
     +                    TieID_INDEX(:,:),
     +                    GET_TieID_INDEX
      INTEGER (KIND=4) :: IOS
      CHARACTER (LEN=4096) RECLN
      REAL (KIND=4) :: MONTHLY_DATA(12),
     +                 ExpansionCost(:),
     +                 ExpansionCostEscalation(:),
     +                 OUTAGE_RATES,
     +                 LINE_LOSSES_A_TO_B(:),
     +                 LINE_LOSSES_B_TO_A(:),
     +                 CONGESTION_PERCENT(:,:),
     +                 CONGESTION_COST(:,:),
     +                 MaximumExpansion(:),
     +                 PipelineDistance,
     +                 ExpansionCCRF(:),
     +                 PipelineID,
     +                 PipelineCapacity,
     +                 CONGESTION_COST_ESC(:),
     +                 CONGESTION_COST_NOMINAL(:),
     +                 MONTHLY_CONGEST_PERCENT(:,:),
     +                 MONTHLY_CONGEST_COST(:,:),
     +                 ZONE_A_TO_B_PEAK_TIES(:),
     +                 ZONE__A_TO_B_OFF_PEAK_TIES(:),
     +                 ZONE__A_TO_B_WHEELING_RATES(:),
     +                 ZONE_B_TO_A_PEAK_TIES(:),
     +                 ZONE__B_TO_A_OFF_PEAK_TIES(:),
     +                 ZONE__B_TO_A_WHEELING_RATES(:),
     +                 ESCALATION_RATE(:),
     +                 ZONE_A_TO_B_PLANNING_FACTOR,
     +                 ZONE_B_TO_A_PLANNING_FACTOR,
     +                 GET_TRANS_CONGEST_COST,
     +                 R_UTIL
      INTEGER (KIND=2) :: ZONE_A_GROUP_ID(:),
     +                    ZONE_B_GROUP_ID(:),
     +                    FIND_TRANS_GROUP_ID,
     +                    DEPTH,
     +                    R_LINK_GROUP,
     +                    GET_MAX_LINK_ID,
     +                    MAX_LINK_ID/0/
      CHARACTER (LEN=31) :: ZONE_A(:),
     +                      ZONE_B(:),
     +                      COMMENT,
     +                      ZONE_A_FULL_NAME,
     +                      ZONE_B_FULL_NAME

      CHARACTER (LEN=3) :: ActiveConstraint
      CHARACTER (LEN=50) :: TieDescription(:),R_GET_TIE_NAME
      CHARACTER (LEN=256) :: PATH_STRING,LINK_STRING
      ALLOCATABLE :: TRANS_GROUP_POSITION,
     +               iPort,
     +               SNamePos,
     +               TieID,
     +               TieID_INDEX,
     +               ExpansionCost,
     +               ExpansionCostEscalation,
     +               MAX_C_FOR_,
     +               MIN_C_FOR_,
     +               LINE_LOSSES_A_TO_B,
     +               LINE_LOSSES_B_TO_A,
     +               CONGESTION_PERCENT,
     +               CONGESTION_COST,
     +               MaximumExpansion,
     +               ExpansionCCRF,
     +               CONGESTION_COST_ESC,
     +               CONGESTION_COST_NOMINAL,
     +               MONTHLY_CONGEST_PERCENT,
     +               MONTHLY_CONGEST_COST,
     +               ZONE_A_TO_B_PEAK_TIES,
     +               ZONE__A_TO_B_OFF_PEAK_TIES,
     +               ZONE__A_TO_B_WHEELING_RATES,
     +               ZONE_B_TO_A_PEAK_TIES,
     +               ZONE__B_TO_A_OFF_PEAK_TIES,
     +               ZONE__B_TO_A_WHEELING_RATES,
     +               ESCALATION_RATE,
     +               ZONE_A_GROUP_ID,
     +               ZONE_B_GROUP_ID,
     +               ZONE_A,
     +               ZONE_B,
     +               TieDescription,
     +               LINK_PAIR,
     +               LINK_INDEX,
     +               I_TO_K_PATHS,
     +               J_TO_K_PATHS,
     +               PATH_FOR_J_TO_K,
     +               VLI,

     +               J_PATH,   ! BIG 
     +               K_PATH,   ! BIG
     +               SET_OF_J, ! BIG
     +               SET_OF_K, ! BIG
     +               PATH,   ! VERY BIG
     +               PARENT,
     +               NODE, ! VERY BIG
     +               I_PATH,   ! VERY BIG
     +               I_PARENT,
     +               NUMBER_OF_PATHS_FOR_I,
     +               ACTIVE_DEMAND_NODE,
     +               ACTIVE_SUPPLY_NODE
!     
!      
! END DATA DECLARATIONS      
!
!
         MANAGE_COAL_LINK_FORECASTS = .FALSE.
         SAVE_COAL_LINK_FILE_EXISTS = .FALSE.
!         
!
         CALL DOES_COAL_LINK_FILE_EXIST(COAL_LINK_FILE_EXISTS)
!         
         IF(.NOT. COAL_LINK_FILE_EXISTS) RETURN
!
!

         CALL GET_MAX_COAL_LINKS(MAX_IN_COAL_LINKS) 
! 061207. SO THAT EACH LINK MAY BE BI-DIRECTIONAL.

         MAX_OUT_COAL_LINKS = 2 * MAX_IN_COAL_LINKS
!

         SAVE_MAX_COAL_LINKS = MAX_IN_COAL_LINKS
!
         CALL OPEN_COAL_LINK_FILE
!
!         NUM_TRANS_GROUPS = GET_NUMBER_OF_ACTIVE_GROUPS()
!
! THIS IS FROM THE COAL_NODES FILE
         MAX_COAL_GROUP_NUMBER = GET_MAX_COAL_GROUP_NUMBER()
!         
         NUMBER_OF_TG = 0
!
         IF(ALLOCATED(TRANS_GROUP_POSITION))
     +        DEALLOCATE(TRANS_GROUP_POSITION,
     +               iPort,
     +               SNamePos,
     +               TieID,
     +               TieID_INDEX,
     +               ExpansionCost,
     +               ExpansionCostEscalation,
     +               MAX_C_FOR_,
     +               MIN_C_FOR_,
     +               LINE_LOSSES_A_TO_B,
     +               LINE_LOSSES_B_TO_A,
     +               CONGESTION_PERCENT,
     +               CONGESTION_COST,
     +               MaximumExpansion,
     +               ExpansionCCRF,
     +               CONGESTION_COST_ESC,
     +               CONGESTION_COST_NOMINAL,
     +               MONTHLY_CONGEST_PERCENT,
     +               MONTHLY_CONGEST_COST,
     +               ZONE_A_TO_B_PEAK_TIES,
     +               ZONE__A_TO_B_OFF_PEAK_TIES,
     +               ZONE__A_TO_B_WHEELING_RATES,
     +               ZONE_B_TO_A_PEAK_TIES,
     +               ZONE__B_TO_A_OFF_PEAK_TIES,
     +               ZONE__B_TO_A_WHEELING_RATES,
     +               ESCALATION_RATE,
     +               ZONE_A_GROUP_ID,
     +               ZONE_B_GROUP_ID,
     +               ZONE_A,
     +               ZONE_B,
     +               TieDescription,
     +               LINK_PAIR,
     +               LINK_INDEX,
     +               I_TO_K_PATHS,
     +               J_TO_K_PATHS,
     +               PATH_FOR_J_TO_K,
     +               VLI,
     +               ACTIVE_DEMAND_NODE,
     +               ACTIVE_SUPPLY_NODE)
         ALLOCATE(TRANS_GROUP_POSITION(-1:MAX_COAL_GROUP_NUMBER),
     +               TieID(MAX_OUT_COAL_LINKS),
     +               TieID_INDEX(MAX_COAL_GROUP_NUMBER,
     +                                MAX_COAL_GROUP_NUMBER),
     +               ExpansionCost(MAX_OUT_COAL_LINKS),
     +               ExpansionCostEscalation(MAX_OUT_COAL_LINKS),
     +               MAX_C_FOR_(MAX_OUT_COAL_LINKS),
     +               MIN_C_FOR_(0:MAX_OUT_COAL_LINKS),
     +               LINE_LOSSES_A_TO_B(MAX_OUT_COAL_LINKS),
     +               LINE_LOSSES_B_TO_A(MAX_OUT_COAL_LINKS),
     +               CONGESTION_PERCENT(MAX_OUT_COAL_LINKS,10),
     +               CONGESTION_COST(MAX_OUT_COAL_LINKS,10),
     +               MaximumExpansion(MAX_OUT_COAL_LINKS),
     +               ExpansionCCRF(MAX_OUT_COAL_LINKS),
     +               CONGESTION_COST_ESC(MAX_OUT_COAL_LINKS),
     +               CONGESTION_COST_NOMINAL(MAX_OUT_COAL_LINKS),
     +               MONTHLY_CONGEST_PERCENT(MAX_OUT_COAL_LINKS,10),
     +               MONTHLY_CONGEST_COST(MAX_OUT_COAL_LINKS,10),
     +               ZONE_A_TO_B_PEAK_TIES(MAX_OUT_COAL_LINKS),
     +               ZONE__A_TO_B_OFF_PEAK_TIES(MAX_OUT_COAL_LINKS),
     +               ZONE__A_TO_B_WHEELING_RATES(MAX_OUT_COAL_LINKS),
     +               ZONE_B_TO_A_PEAK_TIES(MAX_OUT_COAL_LINKS),
     +               ZONE__B_TO_A_OFF_PEAK_TIES(MAX_OUT_COAL_LINKS),
     +               ZONE__B_TO_A_WHEELING_RATES(MAX_OUT_COAL_LINKS),
     +               ESCALATION_RATE(MAX_OUT_COAL_LINKS),
     +               ZONE_A_GROUP_ID(MAX_OUT_COAL_LINKS),
     +               ZONE_B_GROUP_ID(MAX_OUT_COAL_LINKS),
     +               ZONE_A(MAX_OUT_COAL_LINKS),
     +               ZONE_B(MAX_OUT_COAL_LINKS),
     +               TieDescription(MAX_OUT_COAL_LINKS),
     +               iPort(MAX_OUT_COAL_LINKS),
     +               SNamePos(MAX_OUT_COAL_LINKS),
     +               LINK_PAIR(MAX_COAL_GROUP_NUMBER,
     +                                MAX_COAL_GROUP_NUMBER), ! NEED TO KNOW THE MAX VALUE
     +               LINK_INDEX(MAX_COAL_GROUP_NUMBER,
     +                                MAX_COAL_GROUP_NUMBER), ! NEED TO KNOW THE MAX VALUE
     +               I_TO_K_PATHS(MAX_COAL_GROUP_NUMBER,
     +                                MAX_COAL_GROUP_NUMBER),
     +               J_TO_K_PATHS(MAX_COAL_GROUP_NUMBER,
     +                                MAX_COAL_GROUP_NUMBER),
     +               PATH_FOR_J_TO_K(MAX_J_TO_K_PATHS,
     +                                MAX_OUT_COAL_LINKS), ! MAX COUNT FOR J,K BY MAX TOTAL LINKS INCL. RESERVE
     +               VLI(MAX_COAL_GROUP_NUMBER,MAX_COAL_GROUP_NUMBER),
     +               ACTIVE_DEMAND_NODE(MAX_COAL_GROUP_NUMBER),
     +               ACTIVE_SUPPLY_NODE(MAX_COAL_GROUP_NUMBER))
!         
           TRANS_GROUP_POSITION = 0
!         
! CHANGE TO COAL.
!
!         DO J = 1, NUM_TRANS_GROUPS
!             TRANS_ID = GET_TRANS_GROUP_INDEX(J)
!             TRANS_GROUP_POSITION(TRANS_ID) = J
!         ENDDO
!
         TRANS_GROUP_POSITION(-1) = -1         
!
         LINK_PAIR = .FALSE.
         LINK_INDEX = 0
         TieID_INDEX = 0
!
         ACTIVE_LINK = 1
!
         MAX_C_FOR_ = 0
         MIN_C_FOR_ = 0 
!
         VLI_COUNT = 0
         VLI = 0
!
         iPort = 0
         SNamePos = 0
         TieDescription = 'ZZZZZZZZZZZZZ'
         CONGESTION_COST_NOMINAL = 1.0
! 061207. EACH BIDIRECTIONAL BECOMES TWO LINKS.
         DO CURRENT_RECORD = 1, MAX_IN_COAL_LINKS
            READ(10,REC=CURRENT_RECORD) DELETE,
     +                  TieDescription(ACTIVE_LINK),
     +                  ActiveConstraint,
     +                  ExpansionCost(ACTIVE_LINK),
     +                  ExpansionCostEscalation(ACTIVE_LINK),
     +                  LINE_LOSSES_A_TO_B(ACTIVE_LINK),
     +                  OUTAGE_RATES,
     +                  ZONE_A(ACTIVE_LINK),
     +                  ZONE_A_TO_B_PEAK_TIES(ACTIVE_LINK),
     +                  ZONE__A_TO_B_OFF_PEAK_TIES(ACTIVE_LINK),
     +                  ZONE__A_TO_B_WHEELING_RATES(ACTIVE_LINK),
     +                  ZONE_B(ACTIVE_LINK),
     +                  ZONE_B_TO_A_PEAK_TIES(ACTIVE_LINK),
     +                  ZONE__B_TO_A_OFF_PEAK_TIES(ACTIVE_LINK),
     +                  ZONE__B_TO_A_WHEELING_RATES(ACTIVE_LINK),
     +                  TieID(ACTIVE_LINK),
     +                  COMMENT,
     +                  ZONE_A_GROUP_ID(ACTIVE_LINK),
     +                  ZONE_B_GROUP_ID(ACTIVE_LINK),
     +                  ZONE_A_TO_B_PLANNING_FACTOR,
     +                  ZONE_B_TO_A_PLANNING_FACTOR,
     +                  LINE_LOSSES_B_TO_A(ACTIVE_LINK),
     +                  (CONGESTION_PERCENT(ACTIVE_LINK,I),I=1,10),
     +                  (CONGESTION_COST(ACTIVE_LINK,I),I=1,10),
     +                  PipelineDistance,
     +                  MaximumExpansion(ACTIVE_LINK),
     +                  ExpansionCCRF(ACTIVE_LINK),
     +                  PipelineID,
     +                  PipelineCapacity,
     +                  CONGESTION_COST_ESC(ACTIVE_LINK)
!
            A_ID = ZONE_A_GROUP_ID(ACTIVE_LINK)
            B_ID = ZONE_B_GROUP_ID(ACTIVE_LINK)
!
            MAX_LINK_ID = MAX(MAX_LINK_ID,TieID(ACTIVE_LINK))
!
! 110107. REMOVE BIDIRECTIONAL.
!
!
! 073106. BIG CHANGE: POSITION OF THE COAL NODES FILE.
!
!            A_ID = GET_COAL_GROUP_POSITION(A_ID)            
!            B_ID = GET_COAL_GROUP_POSITION(B_ID)
!
! NEED TO RE-INDEX A_ID AND B_ID BY THE COAL NODE POSITION.
! THIS WILL PROVIDE A CONSISTENT BASIS FOR OPERATING ON SUPPLY, DEMAND AND LINKS.
!
! INITIAL CHECK
!
            IF(INDEX(ActiveConstraint,'No') /= 0 .OR. 
     +            .NOT. COAL_GROUP_ACTIVE_SWITCH(A_ID) .OR.
     +            .NOT. COAL_GROUP_ACTIVE_SWITCH(B_ID)) CYCLE
!
            DO I = 1, 10
               IF(ABS(CONGESTION_COST(ACTIVE_LINK,I)) < .0001) EXIT
               MAX_C_FOR_(ACTIVE_LINK) = I
!
! ALLOW FOR CONGESTION COST TO START AT THE FIRST INTERVAL.
! OTHERWISE ZERO UNTIL THE FIRST INTERVAL.
!
               IF(I == 1 .AND. 
     +              ABS(CONGESTION_PERCENT(ACTIVE_LINK,I)) < .0001) THEN 
                  MIN_C_FOR_(ACTIVE_LINK) = 1
               ENDIF
            END DO            
!
            IF(ZONE_A_TO_B_PEAK_TIES(ACTIVE_LINK) > 0. .AND. 
     +                    LINE_LOSSES_A_TO_B(ACTIVE_LINK) < 99.999) THEN
               LINK_PAIR(A_ID,B_ID) = .TRUE.
               LINK_INDEX(A_ID,B_ID) = ACTIVE_LINK
!               
               VLI_COUNT = VLI_COUNT + 1
               VLI(A_ID,B_ID) = VLI_COUNT
!               
            ENDIF
!            IF(ZONE_B_TO_A_PEAK_TIES(ACTIVE_LINK) > 0. .AND. 
!     +                    LINE_LOSSES_B_TO_A(ACTIVE_LINK) < 99.999) THEN
!               LINK_PAIR(B_ID,A_ID) = .TRUE.
!               LINK_INDEX(B_ID,A_ID) = -ACTIVE_LINK ! REVERSE PATH AS NEGATIVE
!               
!               VLI_COUNT = VLI_COUNT + 1
!               VLI(B_ID,A_ID) = VLI_COUNT
!               
!            ENDIF
!            IF(LINK_PAIR(A_ID,B_ID)) THEN 
!               ACTIVE_LINK = ACTIVE_LINK + 1
!               TieID_INDEX(A_ID,B_ID) = TieID(ACTIVE_LINK)     
!            ENDIF
! BIDIRECTIONAL IS THE EXCEPTION.
!            IF( .NOT. LINK_PAIR(B_ID,A_ID)) THEN
!               CYCLE
!            ENDIF
!            TieID_INDEX(B_ID,A_ID) = TieID(ACTIVE_LINK)     
!
! 031307. NOTE: SAME LINK, BIDIRECTIONAL
!
!
!            TieDescription(ACTIVE_LINK) = 
!     +                  TRIM(TieDescription(ACTIVE_LINK-1)(1:28)//' BI')
!            ExpansionCost(ACTIVE_LINK) = ExpansionCost(ACTIVE_LINK-1)
!            ExpansionCostEscalation(ACTIVE_LINK) = 
!     +                            ExpansionCostEscalation(ACTIVE_LINK-1)
!            ZONE_A(ACTIVE_LINK) = ZONE_B(ACTIVE_LINK-1)
!            ZONE_B(ACTIVE_LINK) = ZONE_A(ACTIVE_LINK-1)
!            ZONE_A_TO_B_PEAK_TIES(ACTIVE_LINK) = 
!     +                              ZONE_B_TO_A_PEAK_TIES(ACTIVE_LINK-1)
!            ZONE__A_TO_B_OFF_PEAK_TIES(ACTIVE_LINK) =
!     +                         ZONE__B_TO_A_OFF_PEAK_TIES(ACTIVE_LINK-1)
!            ZONE__A_TO_B_WHEELING_RATES(ACTIVE_LINK) =
!     +                        ZONE__B_TO_A_WHEELING_RATES(ACTIVE_LINK-1)
!            TieID(ACTIVE_LINK) = TieID(ACTIVE_LINK-1)
!            ZONE_A_GROUP_ID(ACTIVE_LINK) =
!     +                                    ZONE_B_GROUP_ID(ACTIVE_LINK-1)
!            ZONE_B_GROUP_ID(ACTIVE_LINK) =
!     +                                    ZONE_A_GROUP_ID(ACTIVE_LINK-1)
!            LINE_LOSSES_A_TO_B(ACTIVE_LINK) =
!     +                                 LINE_LOSSES_B_TO_A(ACTIVE_LINK-1)
!            CONGESTION_PERCENT(ACTIVE_LINK,:) =
!     +                               CONGESTION_PERCENT(ACTIVE_LINK-1,:)
!            CONGESTION_COST(ACTIVE_LINK,:) =
!     +                                  CONGESTION_COST(ACTIVE_LINK-1,:)
            ACTIVE_LINK = ACTIVE_LINK + 1
!
!
         ENDDO
!         
         call IndexedSortAlphaOrder(MAX_OUT_COAL_LINKS,
     +                              iPort,SNamePos,TieDescription)
!
         ACTIVE_LINK = ACTIVE_LINK - 1
!
         SAVE_COAL_LINK_FILE_EXISTS = .TRUE.
         MANAGE_COAL_LINK_FORECASTS = .TRUE.
!
      RETURN

      ENTRY GET_LINK_ALPHA_ORDER(R_LINK_GROUP)

         GET_LINK_ALPHA_ORDER = iPort(R_LINK_GROUP)
      RETURN

      ENTRY GET_MAX_LINK_ID()

         GET_MAX_LINK_ID = MAX_LINK_ID
      RETURN

      ENTRY GET_LINK_NAME(R_LINK_GROUP,R_GET_TIE_NAME)  ! CHARACTER*31

         GET_LINK_NAME = .TRUE.
         R_GET_TIE_NAME = TieDescription(R_LINK_GROUP)
      RETURN


      ENTRY GET_ACTIVE_LINK_NUMBER()

         GET_ACTIVE_LINK_NUMBER = ACTIVE_LINK
      RETURN

      ENTRY GET_COAL_LINK_ID(R_LINK_GROUP)

!
! WE ARE REMOVING BIDIRECTIONAL NODES FROM THE DATABASE.
!
         IF(R_LINK_GROUP < 1 .OR. R_LINK_GROUP > ACTIVE_LINK) THEN
            GET_COAL_LINK_ID = 1
         ELSE
            GET_COAL_LINK_ID = TieID(R_LINK_GROUP)
         ENDIF
      RETURN

      ENTRY MONTHLY_COAL_LINK_INIT(R_YEAR)

         MONTHLY_COAL_LINK_INIT = .FALSE.
         IF( .NOT. SAVE_COAL_LINK_FILE_EXISTS) RETURN
         MONTHLY_COAL_LINK_INIT = .TRUE.
         DO I = 1, MAX_OUT_COAL_LINKS
            TEMP_R = CONGESTION_COST_ESC(I)
            IF(TEMP_R < -0.1) THEN ! VECTOR ESCALATION
!
! HARD-WIRED TO ANNUAL ESCALATION VECTOR FOR NOW.
!            
               CONGESTION_COST_NOMINAL(I) = 
     +            ESCALATED_MONTHLY_VALUE(CONGESTION_COST_NOMINAL(I),
     +                                           ABS(INT2(TEMP_R)),
     +                                           R_YEAR,INT2(1),INT2(1))
            ELSEIF(TEMP_R > 0.0001) THEN ! CONSTANT ESCALATION
               CONGESTION_COST_NOMINAL(I) = (1.0+TEMP_R*0.01)**R_YEAR
            ELSE
!               CONGESTION_COST_NOMINAL(I) = 1.0 ! NO ESCALATION
            ENDIF
         END DO
      RETURN

!
!
      ENTRY ANNUAL_COAL_LINK_FORECAST(R_YEAR)
!
! 071706.
!
!

! THIS SHOULD BE MOVED INTO COAL MODEL
! 042007. THIS SHOULD BE READ ONCE AFTER THE NODES AND DEMAND FILES.
!         RIGHT NOW IT IS CONDITIONED ON ACTIVE DEMAND AND SUPPLY NODES
!         WHICH ARE ANNUAL.
!         PROBLEM: DEMAND NODES ARE SET-UP ANNUALLY.
         ANNUAL_COAL_LINK_FORECAST = .FALSE.
         IF( .NOT. SAVE_COAL_LINK_FILE_EXISTS) RETURN
         ACTIVE_NODES = GET_NUMBER_OF_COAL_ACTIVE_GROUPS()
!         
         IF(ALLOCATED(J_PATH))
     +      DEALLOCATE(
     +            J_PATH,   ! BIG 
     +            K_PATH,   ! BIG
     +            SET_OF_J, ! BIG
     +            SET_OF_K, ! BIG
     +            PATH,   ! VERY BIG
     +            PARENT,
     +            NODE, ! VERY BIG
     +            I_PATH,   ! VERY BIG
     +            I_PARENT,
     +            NUMBER_OF_PATHS_FOR_I) ! VERY BIG
         ALLOCATE(
     +            J_PATH(0:100000),   ! SMALL?
     +            K_PATH(0:100000),   ! SMALL?
     +            SET_OF_J(100000), ! BIG
     +            SET_OF_K(100000), ! BIG
     +            PATH(0:512000),   ! VERY BIG
     +            PARENT(0:512000),
     +            NODE(0:512000), ! VERY BIG
     +            I_PATH(0:512000),   ! VERY BIG
     +            I_PARENT(0:512000), ! VERY BIG
     +            NUMBER_OF_PATHS_FOR_I(MAX_COAL_GROUP_NUMBER)) 
!         IF(ALLOCATED(NUMBER_OF_ACTIVE_PATHS))
!     +      DEALLOCATE(
!     +               NUMBER_OF_ACTIVE_PATHS, ! ACTIVE LINKS-1
!     +               NUM_ACTIVE_START_END,
!     +               LINK_MEMORY,            ! ACTIVE_NODES
!     +               DEPTH_OF_PATH,          ! ACTIVE_NODES,ACTIVE_LINKS-1
!     +               INDEX_OF_PATH_LINK)     ! ACTIVE_NODES,ACTIVE_LINKS-1,ACTIVE_LINKS-1
!
!         I = ACTIVE_LINK
!         J = ACTIVE_NODES
!     
!         ALLOCATE(   NUMBER_OF_ACTIVE_PATHS(I), ! ACTIVE LINKS-1
!     +               NUM_ACTIVE_START_END(J,J),
!     +               LINK_MEMORY(J,J,J),            ! ACTIVE_NODES
!     +               DEPTH_OF_PATH(J,J,I),          ! ACTIVE_NODES,ACTIVE NODES ACTIVE NODES
!     +               INDEX_OF_PATH_LINK(J,I,I))     ! ACTIVE_NODES,ACTIVE_LINKS-1,ACTIVE_LINKS-1
!
!         NUMBER_OF_ACTIVE_PATHS = 0
!         NUM_ACTIVE_START_END = 0
!         LINK_MEMORY = 0
!         DEPTH_OF_PATH = 0
!         INDEX_OF_PATH_LINK = 0
!!
!
! ACTIVE_LINKS WILL PROVIDE THE ARRAY SIZE FOR THE LINK DIMENSIONS BELOW.
!         
!         NUMBER_OF_ACTIVE_DEMAND_NODES = 0
!         NUMBER_OF_ACTIVE_SUPPLY_NODES = 0 
!
         I_TO_K_PATHS = 0
         J_TO_K_PATHS = 0
         PATH_FOR_J_TO_K = 0
!
!
         J_PATH = 0
         K_PATH = 0
         SET_OF_J = 0
         SET_OF_K = 0
         PATH = 0
         PARENT = 0
         NODE = 0
         NUMBER_OF_PATHS_FOR_I = 0
!         
         ACTIVE_DEMAND_NODE = .FALSE.
         ACTIVE_SUPPLY_NODE = .FALSE.
!
! THIS IS TO OBTAIN THE SETS OF INITIAL AND TERMINATION NODES
!
         DO I = 1, MAX_COAL_GROUP_NUMBER ! ACTIVE_NODES
!            J = COAL_GROUP_POSITION(I) ! CAN GET RID IF I INDEX
!            
            ACTIVE_DEMAND_NODE(I) = COAL_DEMAND_GROUP_ACTIVE(I) 
!            
            ACTIVE_SUPPLY_NODE(I) = COAL_SUPPLY_GROUP_ACTIVE(I)
!            
         ENDDO
!
! THIS IS TO OBTAIN THE NUMBER OF VALID PATHS.
! RESTICT THE "FROM" NODE J TO ONLY THE FIRST OCCURANCE THAT CONNECTS TO
! THE "TO" NODE K.
!
!         DO I = 1, MAX_COAL_GROUP_NUMBER ! ACTIVE_NODES ! INITIAL (SUPPLY) PATH
         TOTAL_PATHS_FOR_SYSTEM = 0
         NUMBER_OF_DEMAND_PATHS = 0
         DO I = 1,  MAX_COAL_GROUP_NUMBER ! ACTIVE_NODES ! INITIAL (SUPPLY) PATH
            IF(.NOT. ACTIVE_SUPPLY_NODE(I)) CYCLE
!
!            A_ID = GET_COAL_GROUP_POSITION(I)
!            
            I_PATH = 0
            I_PARENT = 0
!            
            J_PATH = 0
            K_PATH = 0
            SET_OF_J = 0
            SET_OF_K = 0
!            
            NUMBER_OF_J = 1
            LAST_NODE_SUM = TOTAL_PATHS_FOR_SYSTEM
            TOTAL_PATHS_FOR_I = 0

            J_PATH(TOTAL_PATHS_FOR_I) = TOTAL_PATHS_FOR_I
            I_PATH(TOTAL_PATHS_FOR_I) = I
            SET_OF_J = 0
            SET_OF_J(1) = I
            DO DEPTH = 1, MIN(MAX_DEPTH,MAX_COAL_GROUP_NUMBER)
! SET_OF_J SET FROM THE PREVIOUS DEPTH
! FIND THE NUMBER OF K FOR SET J.            
               NUMBER_OF_K = 0
               DO M = 1, NUMBER_OF_J
                  J = SET_OF_J(M)
                  DO K = 1, MAX_COAL_GROUP_NUMBER ! ACTIVE_NODES ! 
!                     
                     IF(I == K) CYCLE
                     IF(J == K) CYCLE
                     IF(I_TO_K_PATHS(I,K) > MAX_I_TO_K_PATHS) CYCLE
                     IF(J_TO_K_PATHS(I,K) >= MAX_J_TO_K_PATHS) CYCLE
!
!                     K = N ! ?
!
                     IF(.NOT. LINK_PAIR(J,K)) CYCLE
!
! CHECK FOR REDUNDANCY. 
!
                     IF(DEPTH > 1) THEN
                        REDUNDANT_PATH = .FALSE.
                        P = J_PATH(M)
                        DO 
                           P = I_PARENT(P)
                           Q = I_PATH(P)
                           IF(Q == K) THEN 
                              REDUNDANT_PATH = .TRUE.
                              EXIT
                           ENDIF
                           IF(Q == I) EXIT
                        ENDDO
                        IF(REDUNDANT_PATH) CYCLE
                     ENDIF
!
                     NUMBER_OF_PATHS_FOR_I(I) = 
     +                                      NUMBER_OF_PATHS_FOR_I(I) + 1

                     TOTAL_PATHS_FOR_I = TOTAL_PATHS_FOR_I + 1
                     TOTAL_PATHS_FOR_SYSTEM = TOTAL_PATHS_FOR_SYSTEM + 1
!
                     I_TO_K_PATHS(I,K) = I_TO_K_PATHS(I,K) + 1

                     I_PATH(TOTAL_PATHS_FOR_I) = K
                     I_PARENT(TOTAL_PATHS_FOR_I) = J_PATH(M)
!
                     IF(TOTAL_PATHS_FOR_SYSTEM > 511000) THEN
                        WRITE(4,*) "TOTAL PATHS EXCEEDED"
                        er_message='Stop requested from coal_obj SIID63'
                        call end_program(er_message)
                     ENDIF                     
!
                     PATH(TOTAL_PATHS_FOR_SYSTEM) = K
                     IF(J_PATH(M) > 0) THEN
                        PARENT(TOTAL_PATHS_FOR_SYSTEM) = 
     +                                         J_PATH(M) + LAST_NODE_SUM
                     ELSE
                        PARENT(TOTAL_PATHS_FOR_SYSTEM) = J_PATH(M)
                     ENDIF
                     IF(LAST_NODE_SUM < 0) THEN
                        LAST_NODE_SUM = LAST_NODE_SUM
                     ENDIF
                     NODE(TOTAL_PATHS_FOR_SYSTEM) = I
!                     
                     NUMBER_OF_K = NUMBER_OF_K + 1
                     SET_OF_K(NUMBER_OF_K) = K
                     K_PATH(NUMBER_OF_K) = TOTAL_PATHS_FOR_I
!
                     WRITE(PATH_STRING,'(I4)') K
!
!                     P = TOTAL_PATHS_FOR_SYSTEM
                     P = TOTAL_PATHS_FOR_I
                     Z = 0
                     IF(ACTIVE_DEMAND_NODE(K)) THEN
                        NUMBER_OF_DEMAND_PATHS = 
     +                                        NUMBER_OF_DEMAND_PATHS + 1
                        Q = K
                        DO 
                           P = I_PARENT(P)
                           LAST_Q = Q
                           Q = I_PATH(P)
!                           
                           IF(Z >= 0) THEN
                              J_TO_K_PATHS(Q,LAST_Q) = 
     +                                        J_TO_K_PATHS(Q,LAST_Q) + 1

                              PATH_FOR_J_TO_K(
     +                           J_TO_K_PATHS(Q,LAST_Q),VLI(Q,LAST_Q)) = 
     +                                            TOTAL_PATHS_FOR_SYSTEM
                           ENDIF
!     
                           WRITE(LINK_STRING,'(I4)') Q
                           PATH_STRING = TRIM(PATH_STRING)//'  '//
     +                                                 TRIM(LINK_STRING)
                           Z = Z + 1
! 021307. LIMIT SIZE TO REASONABLE LEVEL (?)                           
                           IF(Q == I .OR. Z > 
     +                        MAX_COAL_GROUP_NUMBER) EXIT ! 32000) EXIT
!                     
                        ENDDO
!                     
                        WRITE(4,'(A)') TRIM(PATH_STRING)
!                        
                     ENDIF
!
! BUILDING OUT THE PATHS. NEED TO IDENTIFY THE PATHS ENDING WITH A DEMAND LATER.
!     
                  ENDDO ! N (SET OF K)
               ENDDO ! M (SET OF J)
!              
!
! NOTE: DIMENSION REASSIGNMENT.
!
               SET_OF_J = SET_OF_K
               J_PATH = K_PATH
               NUMBER_OF_J = NUMBER_OF_K
!               
               SET_OF_K = 0
               K_PATH = 0
!               
            ENDDO ! DEPTH
         ENDDO ! I
         ANNUAL_COAL_LINK_FORECAST  = .TRUE.
         RETURN

      ENTRY GET_PATH_FOR_J_TO_K(R_A,R_B,R_C)

         GET_PATH_FOR_J_TO_K = PATH_FOR_J_TO_K(R_C,VLI(R_A,R_B))
      RETURN

      ENTRY GET_NUMBER_OF_J_TO_K_PATHS(R_A,R_B)

         GET_NUMBER_OF_J_TO_K_PATHS = J_TO_K_PATHS(R_A,R_B)
      RETURN

      ENTRY GET_NUMBER_OF_SUPPLY_PATHS(R_A)

         GET_NUMBER_OF_SUPPLY_PATHS = NUMBER_OF_PATHS_FOR_I(R_A)
      RETURN

!
      ENTRY MONTHLY_COAL_LINK_FORECAST(R_YEAR,R_MONTH)
! 031807.
!

         MONTHLY_COAL_LINK_FORECAST = .FALSE.
         IF( .NOT. SAVE_COAL_LINK_FILE_EXISTS) RETURN
         MONTHLY_COAL_LINK_FORECAST = .TRUE.
         DO I = 1, ACTIVE_LINK
            DO J = 1, 10
               IF(CONGESTION_PERCENT(I,J) < 0.0) THEN
                  MONTHLY_CONGEST_PERCENT(I,J) = 
     +                  ESCALATED_MONTHLY_VALUE(REAL4_ONE,
     +                                    INT2(CONGESTION_PERCENT(I,J)),
     +                                           R_YEAR,R_MONTH,INT2(1))
               ELSE
                  MONTHLY_CONGEST_PERCENT(I,J) = CONGESTION_PERCENT(I,J)
               ENDIF
               MONTHLY_CONGEST_PERCENT(I,J) = 
     +           MAX(0.0,MIN(0.99999,MONTHLY_CONGEST_PERCENT(I,J)*0.01))
!     
               IF(CONGESTION_COST(I,J) < 0.0) THEN
                  MONTHLY_CONGEST_COST(I,J) = 
     +                  ESCALATED_MONTHLY_VALUE(REAL4_ONE,
     +                                    INT2(CONGESTION_COST(I,J)),
     +                                           R_YEAR,R_MONTH,INT2(1))
               ELSE
                  MONTHLY_CONGEST_COST(I,J) = CONGESTION_COST(I,J)
               ENDIF
            END DO 
         END DO
      RETURN

      ENTRY GET_TRANS_CONGEST_COST(R_A,R_B,R_UTIL)

         I = LINK_INDEX(R_A,R_B)
         DO J = 1, 10 
            IF(MONTHLY_CONGEST_PERCENT(I,J) > R_UTIL .AND. J <10 ) CYCLE
            IF(J == 1) THEN
               GET_TRANS_CONGEST_COST = 0.0
            ELSEIF(J == 10) THEN
               GET_TRANS_CONGEST_COST = MONTHLY_CONGEST_COST(I,J)
            ELSE
! INTERPOLATE BETWEEN POINTS            
               GET_TRANS_CONGEST_COST = MONTHLY_CONGEST_COST(I,J)
            ENDIF
         END DO
      RETURN

         ENTRY GET_TieID_INDEX(R_A,R_B) ! THIS IS USED FOR REPORTING PURPOSES

            GET_TieID_INDEX = TieID_INDEX(R_A,R_B)
         RETURN

!         ENTRY GET_PATH_LINK_INFO(R_NODE,R_PATH_NO,R_PARENT_NO)

!         RETURN

         ENTRY GET_TOTAL_PATHS_FOR_SYSTEM

            GET_TOTAL_PATHS_FOR_SYSTEM = TOTAL_PATHS_FOR_SYSTEM
         RETURN

         ENTRY GET_NODE_OF_PATH(R_PATH)

            GET_NODE_OF_PATH = NODE(R_PATH) 
         RETURN

         ENTRY GET_LEAF_OF_PATH(R_PATH)

            GET_LEAF_OF_PATH = PATH(R_PATH)
         RETURN

         ENTRY GET_PARENT_OF_LEAF(R_PATH)

            GET_PARENT_OF_LEAF =  PATH(PARENT(R_PATH))
         RETURN

         ENTRY GET_PARENT_PATH_OF_LEAF(R_PATH)

            GET_PARENT_PATH_OF_LEAF =  PARENT(R_PATH)
         RETURN


         ENTRY GET_COAL_TRANSPORT_RATE(R_A,R_YEAR,R_MONTH)

            IF(R_A > 0 .AND. R_A <= ACTIVE_LINK) THEN
              TEMP_R = ZONE__A_TO_B_WHEELING_RATES(R_A)
              IF(TEMP_R < -0.1) THEN
                  TEMP_R = 
     +                  ESCALATED_MONTHLY_VALUE(REAL4_ONE,
     +                                           ABS(INT2(TEMP_R)),
     +                                           R_YEAR,R_MONTH,INT2(1))
               ENDIF
               GET_COAL_TRANSPORT_RATE = TEMP_R
            ELSE
               GET_COAL_TRANSPORT_RATE = 0.0
            ENDIF

         RETURN

         ENTRY GET_COAL_TRANSPORT_CONGESTION(R_A,R_CONGEST_VECTOR)

            IF(R_A > 0 .AND. R_A <= ACTIVE_LINK) THEN
              GET_COAL_TRANSPORT_CONGESTION = .TRUE. 
!
               DO I = 1, 10
                  R_CONGEST_VECTOR(I) = CONGESTION_COST_NOMINAL(R_A) * 
     _                                            CONGESTION_COST(R_A,I)
               END DO               
            ELSE
               R_CONGEST_VECTOR = 0.0
               GET_COAL_TRANSPORT_CONGESTION = .FALSE. 
            ENDIF
         RETURN

         ENTRY GET_COAL_TRANSPORT_QUANT(R_A,R_B,R_YEAR,R_MONTH)

            IF(.NOT. LINK_PAIR(R_A,R_B)) THEN
! THIS IS A PROBLEM AND SHOULD BE FLAGGED.            
               GET_COAL_TRANSPORT_QUANT = 0 
            ELSE
! NEED TO CONSIDER POINTER ON QUANTS            
               IF(LINK_INDEX(R_A,R_B) > 0) THEN
                  TEMP_R = 
     +               ZONE_A_TO_B_PEAK_TIES(LINK_INDEX(R_A,R_B))
                  IF(TEMP_R < 0.) THEN
                     TEMP_R = 
     +                  ESCALATED_MONTHLY_VALUE(REAL4_ONE,
     +                                           INT2(TEMP_R),
     +                                           R_YEAR,R_MONTH,INT2(1))
                  ENDIF
               ELSE
                  TEMP_R = 
     +               ZONE_B_TO_A_PEAK_TIES(
     +                                         ABS(LINK_INDEX(R_A,R_B)))
                  IF(TEMP_R < 0.) THEN
                     TEMP_R = 
     +                  ESCALATED_MONTHLY_VALUE(REAL4_ONE,
     +                                           INT2(TEMP_R),
     +                                           R_YEAR,R_MONTH,INT2(1))
                  ENDIF
               ENDIF
               GET_COAL_TRANSPORT_QUANT = TEMP_R
            ENDIF
         RETURN

         ENTRY GET_COAL_LINK_QUANT(R_A,R_YEAR,R_MONTH)

! NEED TO CONSIDER POINTER ON QUANTS            
            IF(R_A > 0 .AND. R_A <= ACTIVE_LINK) THEN
              TEMP_R = ZONE_A_TO_B_PEAK_TIES(R_A)
              IF(TEMP_R < -0.1) THEN
                  TEMP_R = 
     +                  ESCALATED_MONTHLY_VALUE(REAL4_ONE,
     +                                           ABS(INT2(TEMP_R)),
     +                                           R_YEAR,R_MONTH,INT2(1))
               ENDIF
               GET_COAL_LINK_QUANT = TEMP_R
            ELSE
               GET_COAL_LINK_QUANT = 0.0
            ENDIF
         RETURN

         ENTRY GET_COAL_TRANSPORT_LOSSES(R_A,R_B,R_YEAR,R_MONTH)

            IF(.NOT. LINK_PAIR(R_A,R_B)) THEN
! THIS IS A PROBLEM AND SHOULD BE FLAGGED.            
               GET_COAL_TRANSPORT_LOSSES = 0 
            ELSE
! NEED TO CONSIDER POINTER ON LOSSES            
               IF(LINK_INDEX(R_A,R_B) > 0) THEN
                  TEMP_R = 
     +               LINE_LOSSES_A_TO_B(LINK_INDEX(R_A,R_B))
                  IF(TEMP_R < 0.) THEN
                     TEMP_R = 
     +                  ESCALATED_MONTHLY_VALUE(REAL4_ONE,
     +                                           INT2(TEMP_R),
     +                                           R_YEAR,R_MONTH,INT2(1))
                  ENDIF
               ELSE
                  TEMP_R = 
     +               LINE_LOSSES_B_TO_A(ABS(LINK_INDEX(R_A,R_B)))
                  IF(TEMP_R < 0.) THEN
                     TEMP_R = 
     +                  ESCALATED_MONTHLY_VALUE(REAL4_ONE,
     +                                           INT2(TEMP_R),
     +                                           R_YEAR,R_MONTH,INT2(1))
                  ENDIF
               ENDIF
               GET_COAL_TRANSPORT_LOSSES = TEMP_R/100.
            ENDIF
         RETURN

         ENTRY GET_MAX_C_FOR_(R_A,R_B,R_C)

            IF(.NOT. LINK_PAIR(R_A,R_B)) THEN
               GET_MAX_C_FOR_ = 0
            ELSE
               GET_MAX_C_FOR_ = MAX_C_FOR_(LINK_INDEX(R_A,R_B))
            ENDIF
!            
            R_C = MIN_C_FOR_(LINK_INDEX(R_A,R_B))
!            
         RETURN

         ENTRY TEST_COAL_LINK_CF(R_A,R_B,R_C,R_CF)

            IF(.NOT. LINK_PAIR(R_A,R_B)) THEN !  .OR. R_C < 1) THEN
               TEST_COAL_LINK_CF = .FALSE.
            ELSE
               TEST_COAL_LINK_CF = .FALSE.
               IF(LINK_INDEX(R_A,R_B) > 0) THEN
                  IF(R_C == 0) THEN
                     TEST_COAL_LINK_CF = .TRUE.
                  ELSEIF(R_C == MAX_C_FOR_(LINK_INDEX(R_A,R_B)) .AND.
     +                  R_CF >= 
     +                CONGESTION_PERCENT(LINK_INDEX(R_A,R_B),R_C)) THEN
                     TEST_COAL_LINK_CF = .TRUE.
! DON'T ENFORCE LOWER BOUND. A PREVIOUS PATH'S CONSTRAINT MAY HAVE CREATED IT

                  ELSEIF(R_CF < 
     +              CONGESTION_PERCENT(LINK_INDEX(R_A,R_B),R_C+1)) THEN
                     TEST_COAL_LINK_CF = .TRUE.
                  ENDIF
               ENDIF
               IF(LINK_INDEX(R_B,R_A) > 0) THEN
                  IF(R_C == 0) THEN
                     TEST_COAL_LINK_CF = .TRUE.
                  ELSEIF(R_C == MAX_C_FOR_(LINK_INDEX(R_B,R_A)) .AND.
     +                  R_CF >= 
     +                CONGESTION_PERCENT(LINK_INDEX(R_B,R_A),R_C)) THEN
                     TEST_COAL_LINK_CF = .TRUE.
                  ELSEIF(R_CF >= 
     +                CONGESTION_PERCENT(LINK_INDEX(R_B,R_A),R_C) .AND.
     +             R_CF < 
     +              CONGESTION_PERCENT(LINK_INDEX(R_B,R_A),R_C+1)) THEN
                     TEST_COAL_LINK_CF = .TRUE.
                  ENDIF
               ENDIF
            ENDIF
         RETURN

         ENTRY  AVAIL_COAL_GIVEN_LINK_CONGEST(R_A,
     +                                       R_B,
     +                                       R_C,
     +                                       R_LINK_CAP_USED,
     +                                       R_LINK_CAP)


            IF(R_C == 0 .AND. MAX_C_FOR_(LINK_INDEX(R_A,R_B)) == 0) THEN
               AVAIL_COAL_GIVEN_LINK_CONGEST = 
     +                                      R_LINK_CAP - R_LINK_CAP_USED
            ELSE
               IF(R_C < MAX_C_FOR_(LINK_INDEX(R_A,R_B))) THEN
                  AVAIL_COAL_GIVEN_LINK_CONGEST = R_LINK_CAP * 
     +                CONGESTION_PERCENT(LINK_INDEX(R_A,R_B),R_C+1) -
     +                                                   R_LINK_CAP_USED
               ELSE 
                 AVAIL_COAL_GIVEN_LINK_CONGEST = 
     +                                      R_LINK_CAP - R_LINK_CAP_USED
               ENDIF
            ENDIF
         RETURN

         ENTRY GET_COAL_TRANS_CONGEST_COST(R_A,R_B,R_C,R_YEAR,R_MONTH)

! NEED TO CONSIDER RESERVE DIRECTION CONGESTION COST.
            IF(.NOT. LINK_PAIR(R_A,R_B) .OR. R_C < 1) THEN
               GET_COAL_TRANS_CONGEST_COST = 0 
            ELSE
! NEED TO CONSIDER POINTER ON CONGESTION_COST 
               TEMP_R = 0.0
               IF(LINK_INDEX(R_A,R_B) > 0) THEN
                  TEMP_R = 
     +               CONGESTION_COST(LINK_INDEX(R_A,R_B),R_C)
                  IF(TEMP_R < 0.) THEN
                     TEMP_R = 
     +                  ESCALATED_MONTHLY_VALUE(REAL4_ONE,
     +                                           INT2(TEMP_R),
     +                                           R_YEAR,R_MONTH,INT2(1))
                  ENDIF
               ENDIF
               GET_COAL_TRANS_CONGEST_COST = TEMP_R 

            ENDIF
         RETURN
      END           

!
!        PROGRAM TO READ MULTI-TAB INFORMATION ON COAL NODE NODES
!                 AND CONVERT TO BINARY FORMAT
!                       COPYRIGHT (C) 2006
!      ALL RIGHTS RESERVED GLOBAL ENERGY DECISIONS
!

!
! 070606. SUBSTANTIALLY CRIBBED FROM TG_OBJECT
! GN = COAL NODE
!
      SUBROUTINE CN_OBJECT
      use end_routine, only: end_program, er_message
!         

!
! 120606. ADDED COAL NODE LAT/LONG AND STATE/PROVINCE VARAIBLES.
!

      use spindriftlib
      use prod_arrays_dimensions
      USE SIZECOM

      LOGICAL*1 SAVE_GN_FILE_EXISTS/.FALSE./,R_GN_FILE_EXISTS
      INTEGER*2   UNIT_NUM/10/,INUNIT,IREC,DELETE,LRECL/842/,
     +            SAVE_COAL_GROUPS_TABLES/0/,R_COAL_GROUPS_TABLES,
     +            SAVE_COAL_GROUPS_RECORDS/0/,R_COAL_GROUPS_RECORDS
      INTEGER IOS
      CHARACTER*5 COAL_NODE_FILE,OVERLAY_FAMILY_NAME
      CHARACTER*3 HOURLY_PRICE_NAME
      CHARACTER*255 SAVE_BASE_FILE_NAME,R_TEMP_NAME,FILE_NAME_OVL
      CHARACTER*256 FILE_NAME
      CHARACTER*256 OUTPUT_DIRECTORY
      CHARACTER*256 BASE_FILE_DIRECTORY
      CHARACTER*152 MESSAGE
      LOGICAL*4 FILE_EXISTS
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER*1024 RECLN
!
! SIMULATION VARIABLES
!
      CHARACTER*30 GROUP_NAME ! changed 090508.
      CHARACTER*40 SCENARIO_VARIABLE
      CHARACTER*1 GROUP_ACTIVE,SPINNING_UNITS,
     +            OFF_PEAK_SPINNING_UNITS,
     +            REPORT_CL_CAPACITY,
     +            TIME_ZONE,NOX_SEASON,PURCHASE_POWER_ASSIGN,
     +            CREATE_HOURLY_PRICE
      CHARACTER*2 ST_LHS_FOR_PRICES
      INTEGER*2   COAL_NODE_GROUP,
     +            ASSET_CLASS_ID,
     +            ASSET_CLASS_REV_ALLOC_VECTOR,
     +            PURCHASE_ASSET_CLASS_ID,
     +            PURCHASE_ASSET_ALLOC_VECTOR,
     +            RTO_GROUP,
     +            NOX_YEAR,
     +            END_NOX_YEAR,
     +            HYDRO_LOAD_AGGREGATION,
     +            PLANNING_AREA,
     +            COAL_HUB_REFERENCE
      CHARACTER*6 BASECASE_MARKET_AREA_ID,
     +            BASE_CASE_COAL_AREA_ID,BASECASE_SUBREGION_ID

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
     +    MAX_HOURLY_GN_IMPORT,
     +    MAX_HOURLY_GN_EXPORT
      CHARACTER*50 COMMENT
!      
! FILE MANAGEMENT VARIABLES
!
      CHARACTER*17 FILE_TYPE/'COAL_SUP Group   '/
      CHARACTER*2 GNGROUP_OL/'BC'/,R_GNGROUP_OL
      LOGICAL*1 LAHEY_LF95
      CHARACTER*30 SCREEN_OUTPUT
!
! SPCapEx VARIABLES 11/18/06
!
      REAL*4   OPERATING_COSTS(25),
     +         CAPACITY_VALUES(25),
     +         CUM_RES_REMAIN(25),
     +         POTENTIAL_RESERVES,
     +         PROVEN_RESERVES,
     +         MAX_DAILY_EXTRACTION_RATE,
     +         MAX_MONTHLY_EXTRACTION_RATE,
     +         SHRINKAGE_PERCENT,
     +         SHRINKAGE_COST     
      REAL (KIND=8) :: COAL_NODE_LATITUDE,COAL_NODE_LONGITUDE
      CHARACTER(LEN=6) :: COAL_NODE_STATE_PROVINCE,COAL_NODE_COUNTRY
      CHARACTER(LEN=12) :: COAL_NODE_TYPE
      SAVE SAVE_BASE_FILE_NAME
!      
!
!      
!

!
!          ROUTINE TO CONVERT FROM ASCII TO DIRECT-ACESS BINARY
!          COPYRIGHT (C) 1983-98  M.S. GERBER & ASSOCIATES, INC.
!

!
! CONVERT THE COAL_NODE GROUPS FILE
!
!

      ENTRY COAL_NODE_MAKEBIN

      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//
     +                                        COAL_NODE_FILE()
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_LOCATE_WRITE(16,30,
     +                        COAL_NODE_FILE(),ALL_VERSIONS,0)
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
      ENDIF
      FILE_NAME = trim(BASE_FILE_DIRECTORY())//
     +                           "GNB"//trim(COAL_NODE_FILE())//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      IF(FILE_EXISTS) THEN
!
         SAVE_BASE_FILE_NAME = FILE_NAME
!         
         SAVE_GN_FILE_EXISTS = .TRUE.
!         
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCGNGROUP.BIN",
     +                      ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
!         
!
         SAVE_COAL_GROUPS_TABLES = 0
!
         GROUP_NAME = 'Unassigned                    '
         GROUP_ACTIVE = 'T'
         COAL_NODE_GROUP = 1
         BASECASE_MARKET_AREA_ID = 'BLANK ' ! CHAR*6
         BASE_CASE_COAL_AREA_ID = 'BLANK ' ! CHAR*6
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
         MAX_HOURLY_GN_IMPORT = 999999.
         MAX_HOURLY_GN_EXPORT = 999999.
         HYDRO_LOAD_AGGREGATION = 0
         PLANNING_AREA = 0
         SCENARIO_VARIABLE = '                                        '
         COAL_NODE_STATE_PROVINCE = "#N/A"
!
         IREC = 0
         READ(10,*) DELETE
         DO ! TABLES
            DO ! GROUP-BASED RECORDS
               READ(10,1000,IOSTAT=IOS) RECLN
!               
               IF(IOS /= 0) EXIT ! END OF FILE
!
               IF(RECLN(1:1) == '7') THEN ! END OF TABLE ! EXIT AT BOTTOM OF IF
                  EXIT ! GO TO NEXT TABLE
               ENDIF
!               
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,ERR=200)   DELETE,
     +                                 GROUP_NAME,                ! 1
     +                                 GROUP_ACTIVE,
     +                                 COAL_NODE_GROUP,
     +                                 BASECASE_MARKET_AREA_ID,
     +                                 BASE_CASE_COAL_AREA_ID,
     +                                 BASECASE_SUBREGION_ID,
     +                                 SPINNING_UNITS,
     +                                 SPINNING_RESERVE,
     +                                 COMMENT,
     +                                 MAX_HOURLY_RAMP_UP,        ! 10
     +                                 MAX_HOURLY_RAMP_DOWN,      ! 11
     +                                 CAPACITY_VALUES(1),      ! 12
     +                                 CUM_RES_REMAIN(1),    ! 13
     +                                 CAPACITY_VALUES(2),     ! 14
     +                                 CUM_RES_REMAIN(2),   ! 15
     +                                 CAPACITY_VALUES(3),      ! 16
     +                                 CUM_RES_REMAIN(3),    ! 17
     +                                 REPORT_CL_CAPACITY,
     +                                 ASSET_CLASS_ID,
     +                                 ASSET_CLASS_REV_ALLOC_VECTOR, ! 20
     +                                 TIME_ZONE,
     +                                 POTENTIAL_RESERVES,           ! 22
     +                                 NOX_SEASON,
     +                                 PURCHASE_POWER_ASSIGN,
     +                                 PURCHASE_ASSET_CLASS_ID,
     +                                 PURCHASE_ASSET_ALLOC_VECTOR,
     +                                 CREATE_HOURLY_PRICE,
     +                                 HOURLY_PRICE_NAME,
     +                                 CAPACITY_VALUES(4),    ! 29-35
     +                                 CAPACITY_VALUES(5),    ! 29-35
     +                                 CAPACITY_VALUES(6),    ! 29-35
     +                                 CAPACITY_VALUES(7),    ! 29-35
     +                                 CAPACITY_VALUES(8),    ! 29-35
     +                                 CAPACITY_VALUES(9),    ! 29-35
     +                                 CAPACITY_VALUES(10),    ! 29-35
     +                                 CUM_RES_REMAIN(4),  ! 36-42
     +                                 CUM_RES_REMAIN(5),  ! 36-42
     +                                 CUM_RES_REMAIN(6),  ! 36-42
     +                                 CUM_RES_REMAIN(7),  ! 36-42
     +                                 CUM_RES_REMAIN(8),  ! 36-42
     +                                 CUM_RES_REMAIN(9),  ! 36-42
     +                                 CUM_RES_REMAIN(10),  ! 36-42
     +                                 RTO_GROUP,
     +                                 MRX_VOLATILITY_MULT,
     +                                 NOX_YEAR,
     +                                 NIGHT_SCARCITY_MULT,          ! 46
     +                                 PROVEN_RESERVES,              ! 47
     +                                 HYDRO_LOAD_AGGREGATION,
     +                                 OFF_PEAK_SPINNING_RESERVE,
     +                                 OFF_PEAK_SPINNING_UNITS,      ! 50
     +                                 PRICE_CAP,                    ! 51
     +                                 MAX_DAILY_EXTRACTION_RATE,    ! 52
     +                                 MAX_MONTHLY_EXTRACTION_RATE,
     +                                 PLANNING_AREA,
     +                                 SCENARIO_VARIABLE,
     +                                 END_NOX_YEAR,
     +                                 ST_LHS_FOR_PRICES,
     +                                 COAL_NODE_LATITUDE,
     +                                 COAL_NODE_LONGITUDE,
     +                                 COAL_NODE_STATE_PROVINCE,
     +                                 COAL_NODE_TYPE,
     +                                 COAL_HUB_REFERENCE,            ! 62
     +                                 COAL_NODE_COUNTRY
!
               IREC = IREC + 1
               WRITE(11,REC=IREC)      DELETE,
     +                                 GROUP_NAME,                ! 1
     +                                 GROUP_ACTIVE,
     +                                 COAL_NODE_GROUP,
     +                                 BASECASE_MARKET_AREA_ID,
     +                                 BASE_CASE_COAL_AREA_ID,
     +                                 BASECASE_SUBREGION_ID,
     +                                 SPINNING_UNITS,
     +                                 SPINNING_RESERVE,
     +                                 COMMENT,
     +                                 MAX_HOURLY_RAMP_UP,        ! 10
     +                                 MAX_HOURLY_RAMP_DOWN,      ! 11
     +                                 CAPACITY_VALUES(1),      ! 12
     +                                 CUM_RES_REMAIN(1),    ! 13
     +                                 CAPACITY_VALUES(2),     ! 14
     +                                 CUM_RES_REMAIN(2),   ! 15
     +                                 CAPACITY_VALUES(3),      ! 16
     +                                 CUM_RES_REMAIN(3),    ! 17
     +                                 REPORT_CL_CAPACITY,
     +                                 ASSET_CLASS_ID,
     +                                 ASSET_CLASS_REV_ALLOC_VECTOR, ! 20
     +                                 TIME_ZONE,
     +                                 POTENTIAL_RESERVES,           ! 22
     +                                 NOX_SEASON,
     +                                 PURCHASE_POWER_ASSIGN,
     +                                 PURCHASE_ASSET_CLASS_ID,
     +                                 PURCHASE_ASSET_ALLOC_VECTOR,
     +                                 CREATE_HOURLY_PRICE,
     +                                 HOURLY_PRICE_NAME,
     +                                 CAPACITY_VALUES(4),    ! 29-35
     +                                 CAPACITY_VALUES(5),    ! 29-35
     +                                 CAPACITY_VALUES(6),    ! 29-35
     +                                 CAPACITY_VALUES(7),    ! 29-35
     +                                 CAPACITY_VALUES(8),    ! 29-35
     +                                 CAPACITY_VALUES(9),    ! 29-35
     +                                 CAPACITY_VALUES(10),    ! 29-35
     +                                 CUM_RES_REMAIN(4),  ! 36-42
     +                                 CUM_RES_REMAIN(5),  ! 36-42
     +                                 CUM_RES_REMAIN(6),  ! 36-42
     +                                 CUM_RES_REMAIN(7),  ! 36-42
     +                                 CUM_RES_REMAIN(8),  ! 36-42
     +                                 CUM_RES_REMAIN(9),  ! 36-42
     +                                 CUM_RES_REMAIN(10),  ! 36-42
     +                                 RTO_GROUP,
     +                                 MRX_VOLATILITY_MULT,
     +                                 NOX_YEAR,
     +                                 NIGHT_SCARCITY_MULT,          ! 46
     +                                 PROVEN_RESERVES,              ! 47
     +                                 HYDRO_LOAD_AGGREGATION,
     +                                 OFF_PEAK_SPINNING_RESERVE,
     +                                 OFF_PEAK_SPINNING_UNITS,      ! 50
     +                                 PRICE_CAP,                    ! 51
     +                                 MAX_DAILY_EXTRACTION_RATE,    ! 52
     +                                 MAX_MONTHLY_EXTRACTION_RATE,
     +                                 PLANNING_AREA,
     +                                 SCENARIO_VARIABLE,
     +                                 END_NOX_YEAR,
     +                                 ST_LHS_FOR_PRICES,
     +                                 COAL_NODE_LATITUDE,
     +                                 COAL_NODE_LONGITUDE,
     +                                 COAL_NODE_STATE_PROVINCE,
     +                                 COAL_NODE_TYPE,
     +                                 COAL_HUB_REFERENCE,
     +                                 COAL_NODE_COUNTRY
!
            ENDDO ! COAL_NODE GROUPS
            SAVE_COAL_GROUPS_TABLES = SAVE_COAL_GROUPS_TABLES + 1
            IF(IOS /= 0) EXIT
         ENDDO ! READ TABLES
         CLOSE(10)
      ELSE IF(INDEX(COAL_NODE_FILE(),'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "coal_obj:0002")
      ELSE
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCGNGROUP.BIN",
     +                      ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
!
         SAVE_GN_FILE_EXISTS = .FALSE.
!         
      ENDIF
      SAVE_COAL_GROUPS_RECORDS = IREC
!      ENDFILE(11)
      CLOSE(11)
      RETURN

!
!          ROUTINE TO CREATE OVERLAY FILES
!          COPYRIGHT (C) 1984-88  M.S. GERBER & ASSOCIATES, INC.
!          COPYRIGHT (C) 1991-92  M.S. GERBER & ASSOCIATES, INC.
!

!
! OVERLAY THE SYSTEM-FORECAST FILE

      ENTRY COAL_NODE_MAKEOVL(OVERLAY_FAMILY_NAME)

      CALL LOCATE(10,51)
      CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
      FILE_NAME_OVL=trim(OUTPUT_DIRECTORY())//"GNO"//
     +                               trim(OVERLAY_FAMILY_NAME)//".DAT"
      OPEN(10,FILE=FILE_NAME_OVL)
      READ(10,*) DELETE
      INUNIT = 12
      IF(GNGROUP_OL == 'BC') THEN
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCGNGROUP.BIN",
     +                                       ACCESS="DIRECT",RECL=LRECL)
         INUNIT = 11
      ENDIF

      FILE_NAME = trim(OUTPUT_DIRECTORY())//"OLGNGROUP.BIN"

      OPEN(12,FILE=FILE_NAME,ACCESS="DIRECT", 
     +                           STATUS="UNKNOWN",RECL=LRECL,IOSTAT=IOS)
      IF(IOS /= 0) THEN
         CALL IOSTAT_MSG(IOS,MESSAGE)
         WRITE(4,*) trim(MESSAGE)
         WRITE(4,*) '*** line 4176 TF_OBJT.FOR ***'
         er_message='See WARNING MESSAGES-coal-obj-1'
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
     +                                 GROUP_NAME,                ! 1
     +                                 GROUP_ACTIVE,
     +                                 COAL_NODE_GROUP,
     +                                 BASECASE_MARKET_AREA_ID,
     +                                 BASE_CASE_COAL_AREA_ID,
     +                                 BASECASE_SUBREGION_ID,
     +                                 SPINNING_UNITS,
     +                                 SPINNING_RESERVE,
     +                                 COMMENT,
     +                                 MAX_HOURLY_RAMP_UP,        ! 10
     +                                 MAX_HOURLY_RAMP_DOWN,      ! 11
     +                                 CAPACITY_VALUES(1),      ! 12
     +                                 CUM_RES_REMAIN(1),    ! 13
     +                                 CAPACITY_VALUES(2),     ! 14
     +                                 CUM_RES_REMAIN(2),   ! 15
     +                                 CAPACITY_VALUES(3),      ! 16
     +                                 CUM_RES_REMAIN(3),    ! 17
     +                                 REPORT_CL_CAPACITY,
     +                                 ASSET_CLASS_ID,
     +                                 ASSET_CLASS_REV_ALLOC_VECTOR, ! 20
     +                                 TIME_ZONE,
     +                                 POTENTIAL_RESERVES,           ! 22
     +                                 NOX_SEASON,
     +                                 PURCHASE_POWER_ASSIGN,
     +                                 PURCHASE_ASSET_CLASS_ID,
     +                                 PURCHASE_ASSET_ALLOC_VECTOR,
     +                                 CREATE_HOURLY_PRICE,
     +                                 HOURLY_PRICE_NAME,
     +                                 CAPACITY_VALUES(4:10),    ! 29-35
     +                                 CUM_RES_REMAIN(4:10),  ! 36-42
     +                                 RTO_GROUP,
     +                                 MRX_VOLATILITY_MULT,
     +                                 NOX_YEAR,
     +                                 NIGHT_SCARCITY_MULT,          ! 46
     +                                 PROVEN_RESERVES,              ! 47
     +                                 HYDRO_LOAD_AGGREGATION,
     +                                 OFF_PEAK_SPINNING_RESERVE,
     +                                 OFF_PEAK_SPINNING_UNITS,      ! 50
     +                                 PRICE_CAP,                    ! 51
     +                                 MAX_DAILY_EXTRACTION_RATE,    ! 52
     +                                 MAX_MONTHLY_EXTRACTION_RATE,
     +                                 PLANNING_AREA,
     +                                 SCENARIO_VARIABLE,
     +                                 END_NOX_YEAR,
     +                                 ST_LHS_FOR_PRICES,
     +                                 COAL_NODE_LATITUDE,
     +                                 COAL_NODE_LONGITUDE,
     +                                 COAL_NODE_STATE_PROVINCE,
     +                                 COAL_NODE_TYPE,
     +                                 COAL_HUB_REFERENCE,
     +                                 COAL_NODE_COUNTRY
         IF(IOS /= 0) EXIT
!        READ(10,1000,IOSTAT=IOS) RECLN
!        IF(IOS == 0) THEN
         RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
         READ(RECLN,*,ERR=200)         DELETE,
     +                                 GROUP_NAME,                ! 1
     +                                 GROUP_ACTIVE,
     +                                 COAL_NODE_GROUP,
     +                                 BASECASE_MARKET_AREA_ID,
     +                                 BASE_CASE_COAL_AREA_ID,
     +                                 BASECASE_SUBREGION_ID,
     +                                 SPINNING_UNITS,
     +                                 SPINNING_RESERVE,
     +                                 COMMENT,
     +                                 MAX_HOURLY_RAMP_UP,        ! 10
     +                                 MAX_HOURLY_RAMP_DOWN,      ! 11
     +                                 CAPACITY_VALUES(1),      ! 12
     +                                 CUM_RES_REMAIN(1),    ! 13
     +                                 CAPACITY_VALUES(2),     ! 14
     +                                 CUM_RES_REMAIN(2),   ! 15
     +                                 CAPACITY_VALUES(3),      ! 16
     +                                 CUM_RES_REMAIN(3),    ! 17
     +                                 REPORT_CL_CAPACITY,
     +                                 ASSET_CLASS_ID,
     +                                 ASSET_CLASS_REV_ALLOC_VECTOR, ! 20
     +                                 TIME_ZONE,
     +                                 POTENTIAL_RESERVES,           ! 22
     +                                 NOX_SEASON,
     +                                 PURCHASE_POWER_ASSIGN,
     +                                 PURCHASE_ASSET_CLASS_ID,
     +                                 PURCHASE_ASSET_ALLOC_VECTOR,
     +                                 CREATE_HOURLY_PRICE,
     +                                 HOURLY_PRICE_NAME,
     +                                 CAPACITY_VALUES(4:10),    ! 29-35
     +                                 CUM_RES_REMAIN(4:10),  ! 36-42
     +                                 RTO_GROUP,
     +                                 MRX_VOLATILITY_MULT,
     +                                 NOX_YEAR,
     +                                 NIGHT_SCARCITY_MULT,          ! 46
     +                                 PROVEN_RESERVES,              ! 47
     +                                 HYDRO_LOAD_AGGREGATION,
     +                                 OFF_PEAK_SPINNING_RESERVE,
     +                                 OFF_PEAK_SPINNING_UNITS,      ! 50
     +                                 PRICE_CAP,                    ! 51
     +                                 MAX_DAILY_EXTRACTION_RATE,    ! 52
     +                                 MAX_MONTHLY_EXTRACTION_RATE,
     +                                 PLANNING_AREA,
     +                                 SCENARIO_VARIABLE,
     +                                 END_NOX_YEAR,
     +                                 ST_LHS_FOR_PRICES,
     +                                 COAL_NODE_LATITUDE,
     +                                 COAL_NODE_LONGITUDE,
     +                                 COAL_NODE_STATE_PROVINCE,
     +                                 COAL_NODE_TYPE,
     +                                 COAL_HUB_REFERENCE,
     +                                 COAL_NODE_COUNTRY
!
         READ(10,1000,IOSTAT=IOS) RECLN
         DOWHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE 
!              
            READ(10,1000,IOSTAT=IOS) RECLN
!         
         ENDDO
!         ENDIF
!
         WRITE(12,REC=IREC)            DELETE,        
     +                                 GROUP_NAME,                ! 1
     +                                 GROUP_ACTIVE,
     +                                 COAL_NODE_GROUP,
     +                                 BASECASE_MARKET_AREA_ID,
     +                                 BASE_CASE_COAL_AREA_ID,
     +                                 BASECASE_SUBREGION_ID,
     +                                 SPINNING_UNITS,
     +                                 SPINNING_RESERVE,
     +                                 COMMENT,
     +                                 MAX_HOURLY_RAMP_UP,        ! 10
     +                                 MAX_HOURLY_RAMP_DOWN,      ! 11
     +                                 CAPACITY_VALUES(1),      ! 12
     +                                 CUM_RES_REMAIN(1),    ! 13
     +                                 CAPACITY_VALUES(2),     ! 14
     +                                 CUM_RES_REMAIN(2),   ! 15
     +                                 CAPACITY_VALUES(3),      ! 16
     +                                 CUM_RES_REMAIN(3),    ! 17
     +                                 REPORT_CL_CAPACITY,
     +                                 ASSET_CLASS_ID,
     +                                 ASSET_CLASS_REV_ALLOC_VECTOR, ! 20
     +                                 TIME_ZONE,
     +                                 POTENTIAL_RESERVES,           ! 22
     +                                 NOX_SEASON,
     +                                 PURCHASE_POWER_ASSIGN,
     +                                 PURCHASE_ASSET_CLASS_ID,
     +                                 PURCHASE_ASSET_ALLOC_VECTOR,
     +                                 CREATE_HOURLY_PRICE,
     +                                 HOURLY_PRICE_NAME,
     +                                 CAPACITY_VALUES(4:10),    ! 29-35
     +                                 CUM_RES_REMAIN(4:10),  ! 36-42
     +                                 RTO_GROUP,
     +                                 MRX_VOLATILITY_MULT,
     +                                 NOX_YEAR,
     +                                 NIGHT_SCARCITY_MULT,          ! 46
     +                                 PROVEN_RESERVES,              ! 47
     +                                 HYDRO_LOAD_AGGREGATION,
     +                                 OFF_PEAK_SPINNING_RESERVE,
     +                                 OFF_PEAK_SPINNING_UNITS,      ! 50
     +                                 PRICE_CAP,                    ! 51
     +                                 MAX_DAILY_EXTRACTION_RATE,    ! 52
     +                                 MAX_MONTHLY_EXTRACTION_RATE,
     +                                 PLANNING_AREA,
     +                                 SCENARIO_VARIABLE,
     +                                 END_NOX_YEAR,
     +                                 ST_LHS_FOR_PRICES,
     +                                 COAL_NODE_LATITUDE,
     +                                 COAL_NODE_LONGITUDE,
     +                                 COAL_NODE_STATE_PROVINCE,
     +                                 COAL_NODE_TYPE,
     +                                 COAL_HUB_REFERENCE,
     +                                 COAL_NODE_COUNTRY
      ENDDO
      IF(IREC /= SAVE_COAL_GROUPS_RECORDS) THEN
         WRITE(4,*) "COAL_NODE GROUP OVERLAY DIFFERENT LENGTH"
         WRITE(4,*) "THAN THE BASE FILE. OVERLAY MUST BE THE SAME"
         WRITE(4,*) "LENGTH. ",FILE_NAME_OVL
      ENDIF
      CLOSE(10)
      CLOSE(12)
      IF(GNGROUP_OL == 'BC') CLOSE(11)
      GNGROUP_OL = 'OL'
      RETURN
!

      ENTRY RESET_COAL_NODE_OL

         GNGROUP_OL = 'BC'
      RETURN
!

      ENTRY RETURN_GNGROUP_OL(R_GNGROUP_OL)

         R_GNGROUP_OL = GNGROUP_OL
      RETURN

      ENTRY DOES_GN_FILE_EXIST(R_GN_FILE_EXISTS)

         R_GN_FILE_EXISTS = SAVE_GN_FILE_EXISTS
      RETURN

      ENTRY GET_COAL_NODE_TABLES(R_COAL_GROUPS_TABLES)

         R_COAL_GROUPS_TABLES = SAVE_COAL_GROUPS_TABLES
      RETURN

      ENTRY GET_COAL_NODE_RECORDS(R_COAL_GROUPS_RECORDS)

         R_COAL_GROUPS_RECORDS = SAVE_COAL_GROUPS_RECORDS
      RETURN

      ENTRY GetCOALNodeBaseName(R_TEMP_NAME)

         R_TEMP_NAME = SAVE_BASE_FILE_NAME
      RETURN

      ENTRY OPEN_GN_FILE

         OPEN(UNIT_NUM,FILE=trim(OUTPUT_DIRECTORY())//GNGROUP_OL//
     +        "GNGROUP.BIN",ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      RETURN
!

      ENTRY CLOSE_GN_FILE

         CLOSE(UNIT_NUM)
      RETURN

  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from coal_obj SIID64'
      call end_program(er_message)

 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END

!
      FUNCTION MANAGE_COAL_NODE_FORECASTS()

      use spindriftlib
      use prod_arrays_dimensions

      SAVE
      LOGICAL*1 SAVE_GN_FILE_EXISTS/.FALSE./
      INTEGER*4 VALUES_2_ZERO
      INTEGER*2 DELETE,CURRENT_RECORD,COAL_GROUP,GET_COAL_GROUPS,
     +          COAL_GROUPS_RECORDS,R_COAL_GROUP,
     +          R_ASSET_CLASS,GN,
     +          SAVE_COAL_GROUPS_RECORDS/0/,
     +          TEMP_I,I,HG,PA,
     +          FIRST_COAL_SUP_REPORTING_GROUP,
     +          FIRST_REPORTING_GROUP/0/,
     +          PLANNING_AREA
      LOGICAL*1 MANAGE_COAL_NODE_FORECASTS,COAL_GROUP_ACTIVE_SWITCH,
     +          MOD_COAL_GROUP_ACTIVE_SWITCH,
     +          GET_SCARCITY_INFO,GET_COAL_SPINNING_CAPACITY,
     +          GET_COAL_PRICE_CAPS,
     +          GET_OFF_PEAK_SPINNING_CAPACITY,
     +          GET_COAL_RAMP_RATES,
     +          GET_ONE_COAL_RAMP_RATES,
     +          GET_COAL_MAX_IMPORT_EXPORT,
     +          HYDRO_AGGREGATION/.FALSE./,
!     +          PLANNING_AREA/.FALSE./,
     +          GET_GN_PRICE_MULT,
     +          GET_ONE_GN_PRICE_MULT,
     +          GET_GN_GROUP_NAME
      LOGICAL*1 GN_FILE_EXISTS
!      SAVE      COAL_GROUPS_DATA
      CHARACTER*30 R_GET_GROUP_NAME
      CHARACTER*3 HOURLY_PRICE_NAME(:)
      CHARACTER*3 R_GET_HOURLY_PRICE_NAME
      CHARACTER*30 R_GET_COAL_NODE_GROUP_NAME
      CHARACTER*40 SCENARIO_VARIABLE
      LOGICAL*1 GET_TF_HOURLY_PRICE_NAME,GET_TF_GROUP_NAME,
     +          GET_TF_COAL_NODE_GROUP_NAME,
     +          GET_TF_COAL_GROUP_NOX_SEASON
!
! SIMULATION VARIABLES
!
      CHARACTER*30 GROUP_NAME(:)
      CHARACTER*1 GROUP_ACTIVE(:),SPINNING_UNITS(:),
     +            OFF_PEAK_SPINNING_UNITS(:),
     +            REPORT_CL_CAPACITY(:),TIME_ZONE(:),
     +            REPORT_THIS_GROUP(:),
     +            NOX_SEASON(:),R_GET_COAL_GROUP_NOX_SEASON,
     +            PURCHASE_POWER_ASSIGN(:),R_PURCHASE_POWER_ASSIGN,
     +            CREATE_HOURLY_PRICE(:)
      LOGICAL*1 GET_PURCHASE_POWER_ASSIGN
      LOGICAL*1   GET_REPORT_CL_CAPACITY,GET_REPORT_COAL_GROUP,
     +            GET_CREATE_HOURLY_PRICE,
     +            GET_ST_LHS_FOR_PRICES,
     +            GET_BASECASE_MARKET_AREA_ID
      INTEGER*2   COAL_NODE_GROUP(:),
     +            COUNTRY_NODE_INDEX(:),
     +            GET_COAL_NODE_COUNTRY_INDEX,
     +            COUNTRY_NODE_COUNT(0:100),
     +            MAX_COUNTRIES,
     +            GET_NUM_COUNTRIES,
     +            COAL_GROUP_INDEX(:),
     +            GN_SCENARIO_VARIABLE_INDEX(:),
     +            SCENARIO_INDEX,
     +            GET_SCENARIO_INDEX,
     +            HYDRO_GROUP_2_GN(:),
!     +            PLANNING_AREA_2_GN(:),
     +            COAL_GROUP_POSITION(:),
     +            MAX_COAL_REGION_NO/500/,
     +            MAX_COUNTRY_NO/0/,
     +            GET_COUNTRY_INDEX,
     +            COAL_GROUP_STATE_POSITION(:),
     +            NODE_FROM_STATE_POSITION(:),
     +            GN_2_HYDRO_GROUP(:),
     +            GN_2_PLANNING_AREA(:),
     +            HYDRO_AGGREGATION_POSITION(:),
     +            HYDRO_AGGREGATION_INDEX(:),
     +            PLANNING_AREA_POSITION(:),
     +            PLANNING_AREA_INDEX(:),
     +            ASSET_CLASS_ID(:),
     +            ASSET_CLASS_REV_ALLOC_VECTOR(:),
     +            ASSET_CLASS_GROUPS_INDEX(:),
     +            ASSET_CLASS_2_GN(:),
     +            GET_ASSET_CLASS_2_GN,
     +            ASSET_CLASS_GROUP_2_AC(:), 
     +            RTO_GROUP(:),
     +            NOX_YEAR(:),
     +            END_NOX_YEAR(:),
     +            HYDRO_LOAD_AGGREGATION(:),
     +            GN_REGIONAL_PLANNING_AREA(:),
     +            GET_COAL_GROUP_NOX_YEAR,
     +            GET_COAL_GROUP_END_NOX_YEAR,
     +            MAX_ASSET_GROUPS,
     +            NUMBER_OF_ACTIVE_COAL_GROUPS/0/,
     +            NUMBER_OF_HYDRO_GROUPS/0/,
     +            NUMBER_OF_PLANNING_GROUPS/0/,
     +            GET_NUMBER_OF_PLANNING_GROUPS,
     +            GET_NUMBER_OF_HYDRO_GROUPS,
     +            GET_HG_FROM_GN,
     +            GET_PA_FROM_GN,
     +            GET_PA_VALUE_FROM_GN,
     +            GET_NUMBER_OF_COAL_ACTIVE_GROUPS,
     +            MAX_COAL_GROUP_INDEX,GET_MAX_COAL_GROUPS,
     +            MAX_COAL_GROUP_NUMBER/0/,
     +            GET_COAL_GROUP_INDEX,
     +            GET_COAL_GROUP_POSITION,
     +            GET_COAL_GROUP_STATE_POSITION,
     +            GET_NODE_FROM_STATE_POSITION,
     +            R_STATE_POSITION,
     +            GET_MAX_COAL_GROUP_NUMBER,
     +            PURCHASE_ASSET_CLASS_ID(:),
     +            GET_PURCHASE_ASSET_CLASS_ID,
     +            PURCHASE_ASSET_ALLOC_VECTOR(:),
     +            R_GN,GET_AC_FOR_GN,
     +            AC,MAX_ASSET_CLASS_GROUPS/0/,
     +            R_NUMBER_OF_COAL_GROUPS,
     +            COAL,R_MONTH,R_YEAR,
     +            R_ON_OR_OFF_PEAK,
     +            DAY_TYPE,DATA_BASE,GET_DATA_BASE_FOR_COAL,
     +            FUNCTION_HOLDER2,
     +            GET_PLANNING_AREA_POSITION,
     +            GET_BELONGS_TO_COAL_GROUP,
     +            COAL_HUB_REFERENCE_INDEX(:),
     +            COAL_HUB_REFERENCE,
     +            GET_COAL_HUB_REFERENCE,
     +            iPort(:),SNamePos(:)
      PARAMETER( MAX_COAL_GROUP_INDEX=2048,MAX_ASSET_GROUPS=2048)
      CHARACTER*6 BASECASE_MARKET_AREA_ID(:),
     +            BASE_CASE_COAL_AREA_ID(:),BASECASE_SUBREGION_ID(:),
     +            R_BASECASE_MARKET_AREA_ID
      CHARACTER*2 ST_LHS_FOR_PRICES(:),
     +            R_ST_LHS_FOR_PRICES
      INTEGER*2 BELONGS_TO_COAL_GROUP(MAX_COAL_GROUP_INDEX),
!     +            R_PRICE_VARIABLE(*),
     +            R_ONE_PRICE_VARIABLE
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
     +       MAX_HOURLY_GN_IMPORT(:),
     +       MAX_HOURLY_GN_EXPORT(:),
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
      +       TEMP_CAPACITY,
     +       R_ONE_PRICE_MULT,
     +       R_RAMP_ONE_UP,
     +       R_RAMP_ONE_DOWN,
     +       GET_SCENARIO_BY_INDEX,
     +       GET_COAL_GROUP_PEAK,
     +       GLOBAL_SCARCITY,
     +       GET_GLOBAL_SCARCITY,
     +       GET_MRX_VOLATILITY_MULT,
     +       GET_VAR,
     +       GET_OFF_PEAK_SPIN_FOR_GN,
     +       GET_COAL_SPIN_FOR_GN
      REAL*4
     +       OPERATING_COSTS(:,:),
     +       CAPACITY_VALUES(:,:),
     +       CUM_RES_REMAIN(:,:),
     +       POTENTIAL_RESERVES(:),
     +       PROVEN_RESERVES(:),
     +       MAX_DAILY_EXTRACTION_RATE(:),
     +       MAX_MONTHLY_EXTRACTION_RATE(:),
     +       SHRINKAGE_PERCENT(:),
     +       SHRINKAGE_COST(:),
     +       LOCAL_SHRINKAGE_COST,LOCAL_SHRINKAGE_PERCENT     
      REAL*8 COAL_GROUP_CAP
      CHARACTER*50 COMMENT
      REAL (KIND=8) :: COAL_NODE_LATITUDE(:),
     +                    COAL_NODE_LONGITUDE(:)
      CHARACTER (LEN=6) COAL_NODE_STATE_PROVINCE(:),
     +                  LOCAL_STATE_PROVINCE_NAME,
     +                  COAL_NODE_COUNTRY(:),
     +                  LOCAL_COUNTRY_NAME,
     +                  TEMP_STATE
      CHARACTER (LEN=12) :: COAL_NODE_TYPE
      INTEGER (KIND=2) :: COAL_NODE_TYPE_INDEX(:),GET_COAL_NODE_TYPE_INDEX,
     +                 COAL_NODE_TYPE_COUNT(5),GET_COAL_NODE_TYPE_COUNT,
     +                 WORLD_DATABASE_INDEX(:),GET_WORLD_DATABASE_INDEX,
     +                 GET_NODE_ALPHA_ORDER
      ALLOCATABLE ::
     +     GROUP_NAME,
     +     iPort,SNamePos,
     +     WORLD_DATABASE_INDEX,
     +     GROUP_ACTIVE,
     +     COAL_NODE_GROUP,
     +     COUNTRY_NODE_INDEX,
     +     BASECASE_MARKET_AREA_ID,
     +     BASE_CASE_COAL_AREA_ID,
     +     BASECASE_SUBREGION_ID,
     +     SPINNING_UNITS,
     +     OFF_PEAK_SPINNING_UNITS,
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
     +     RTO_GROUP,
     +     NOX_YEAR,
     +     END_NOX_YEAR,
     +     ST_LHS_FOR_PRICES,
     +     COAL_NODE_LATITUDE,
     +     COAL_NODE_LONGITUDE,
     +     COAL_NODE_STATE_PROVINCE,
     +     COAL_NODE_COUNTRY,
     +     COAL_NODE_TYPE_INDEX,
     +     COAL_HUB_REFERENCE_INDEX,
     +     MRX_VOLATILITY_MULT,
     +     NIGHT_SCARCITY_MULT,
     +     WEEKEND_SCARCITY_MULT,
     +     PRICE_CAP,
     +     MAX_HOURLY_GN_IMPORT,
     +     MAX_HOURLY_GN_EXPORT,
     +     HYDRO_LOAD_AGGREGATION,
     +     GN_REGIONAL_PLANNING_AREA,
     +     CAPACITY_ADDER,
     +     COAL_GROUP_INDEX,
     +     GN_SCENARIO_VARIABLE_INDEX,
     +     HYDRO_GROUP_2_GN,
!     +     PLANNING_AREA_2_GN,
     +     COAL_GROUP_POSITION,
     +     COAL_GROUP_STATE_POSITION,
     +     NODE_FROM_STATE_POSITION,
     +     GN_2_HYDRO_GROUP,
     +     GN_2_PLANNING_AREA,
     +     HYDRO_AGGREGATION_POSITION,
     +     HYDRO_AGGREGATION_INDEX,
     +     PLANNING_AREA_POSITION,
     +     PLANNING_AREA_INDEX,
     +     ASSET_CLASS_GROUPS_INDEX,
     +     ASSET_CLASS_2_GN,
     +     ASSET_CLASS_GROUP_2_AC, 
     +     REPORT_CL_CAPACITY,
     +     REPORT_THIS_GROUP,
     +     ASSET_CLASS_ID,
     +     ASSET_CLASS_REV_ALLOC_VECTOR,
     +     TIME_ZONE,
     +     NOX_SEASON,
     +     PURCHASE_POWER_ASSIGN,
     +     CREATE_HOURLY_PRICE,
     +     HOURLY_PRICE_NAME,
     +     PURCHASE_ASSET_CLASS_ID,
     +     PURCHASE_ASSET_ALLOC_VECTOR,
     +     OPERATING_COSTS,
     +     CAPACITY_VALUES,
     +     CUM_RES_REMAIN,
     +     POTENTIAL_RESERVES,
     +     PROVEN_RESERVES,
     +     MAX_DAILY_EXTRACTION_RATE,
     +     MAX_MONTHLY_EXTRACTION_RATE,
     +     SHRINKAGE_PERCENT,
     +     SHRINKAGE_COST     
      REAL*4 R_COAL_CAP,R_COAL_MAX_CAP
      REAL*4 COAL_GROUP_SCARCITY_VALUE
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
     +       SCARCITY_CAP_PERCENT_3
      LOGICAL*1 VOID_LOGICAL
!      STORE_GN_SCARCITY_INFO
      REAL*4 COAL_GLOBAL_SCARCITY_VALUE
      INTEGER VALUES_2_SET
      CHARACTER*2 R_CHAR2_NAME
      INTEGER (KIND=2) :: TEMP_I2,STATE_ID_LOOKUP,
     +                    STATE_2_COAL_REGION_LOOKUP,
     +                    GET_COAL_NODE_STATE_PROVINCE
!      SAVE GROUP_NAME,
!     +     GROUP_ACTIVE,
!     +     COAL_NODE_GROUP,
!     +     BASECASE_MARKET_AREA_ID,
!     +     BASE_CASE_COAL_AREA_ID,
!     +     BASECASE_SUBREGION_ID,
!     +     SPINNING_UNITS,
!     +     OFF_PEAK_SPINNING_UNITS,
!     +     SPINNING_RESERVE,
!     +     OFF_PEAK_SPINNING_RESERVE,
!     +     MAX_HOURLY_RAMP_UP,
!     +     MAX_HOURLY_RAMP_DOWN,
!     +     FIRST_CAPACITY_VALUE,
!     +     FIRST_CAPACITY_PERCENT,
!     +     SECOND_CAPACITY_VALUE,
!     +     SECOND_CAPACITY_PERCENT,
!     +     THIRD_CAPACITY_VALUE,
!     +     THIRD_CAPACITY_PERCENT,
!     +     ADDITIONAL_CAPACITY_VALUE,
!     +     ADDITIONAL_CAPACITY_PERCENT,
!     +     RTO_GROUP,
!     +     NOX_YEAR,
!     +     MRX_VOLATILITY_MULT,
!     +     NIGHT_SCARCITY_MULT,
!     +     WEEKEND_SCARCITY_MULT,
!     +     HYDRO_LOAD_AGGREGATION,
!     +     CAPACITY_ADDER,
!     +     COAL_GROUP_INDEX,
!     +     HYDRO_GROUP_2_GN,
!     +     COAL_GROUP_POSITION,
!     +     GN_2_HYDRO_GROUP,
!     +     HYDRO_AGGREGATION_POSITION,
!     +     HYDRO_AGGREGATION_INDEX,
!     +     ASSET_CLASS_GROUPS_INDEX,
!     +     ASSET_CLASS_GROUP_2_AC, 
!     +     REPORT_CL_CAPACITY,
!     +     ASSET_CLASS_ID,
!     +     ASSET_CLASS_REV_ALLOC_VECTOR,
!     +     TIME_ZONE,
!     +     NOX_SEASON,
!     +     PURCHASE_POWER_ASSIGN,
!     +     CREATE_HOURLY_PRICE,
!     +     HOURLY_PRICE_NAME,
!     +     PURCHASE_ASSET_CLASS_ID,
!     +     PURCHASE_ASSET_ALLOC_VECTOR
!
!      
! END DATA DECLARATIONS      
!
         MANAGE_COAL_NODE_FORECASTS = .FALSE.
         SAVE_GN_FILE_EXISTS = .FALSE.
!         
!
         CALL DOES_GN_FILE_EXIST(GN_FILE_EXISTS)
!         
! DEFAULT VALUE. E.G. 25. DOES NOT IMPACT MAX_COAL_GROUP_INDEX BELOW.
!
! 1/18/99. GAT. OUT.
!
!         NUMBER_OF_ACTIVE_COAL_GROUPS = GET_MAX_COAL_GROUPS() 
!         
         IF(.NOT. GN_FILE_EXISTS) RETURN
!
! 500 FOR NORTH AMERICAN DATA BASE
!        
!         MAX_COAL_GROUPS = GET_MAX_COAL_GROUPS()
!
         IF(ALLOCATED(COAL_GROUP_INDEX)) 
     +               DEALLOCATE(COAL_GROUP_INDEX,
     +                     GN_SCENARIO_VARIABLE_INDEX,
     +                     HYDRO_GROUP_2_GN,
!     +                     PLANNING_AREA_2_GN,
     +                     COAL_GROUP_POSITION,
     +                     COAL_GROUP_STATE_POSITION,
     +                     NODE_FROM_STATE_POSITION,
     +                     GN_2_HYDRO_GROUP,
     +                     GN_2_PLANNING_AREA,
     +                     HYDRO_AGGREGATION_POSITION,
     +                     HYDRO_AGGREGATION_INDEX,
     +                     PLANNING_AREA_POSITION,
     +                     PLANNING_AREA_INDEX,
     +                     ASSET_CLASS_GROUPS_INDEX,
     +                     ASSET_CLASS_2_GN,
     +                     ASSET_CLASS_GROUP_2_AC,
     +                     REPORT_THIS_GROUP)
         ALLOCATE(COAL_GROUP_INDEX(MAX_COAL_GROUP_INDEX))
         ALLOCATE(GN_SCENARIO_VARIABLE_INDEX(MAX_COAL_GROUP_INDEX))
         ALLOCATE(HYDRO_GROUP_2_GN(MAX_COAL_GROUP_INDEX))
!         ALLOCATE(PLANNING_AREA_2_GN(MAX_COAL_GROUP_INDEX))
         ALLOCATE(COAL_GROUP_POSITION(MAX_COAL_GROUP_INDEX))
         ALLOCATE(COAL_GROUP_STATE_POSITION(MAX_COAL_GROUP_INDEX))
         ALLOCATE(NODE_FROM_STATE_POSITION(MAX_COAL_REGION_NO))
         ALLOCATE(REPORT_THIS_GROUP(MAX_COAL_GROUP_INDEX))
         ALLOCATE(GN_2_HYDRO_GROUP(MAX_COAL_GROUP_INDEX))
         ALLOCATE(GN_2_PLANNING_AREA(MAX_COAL_GROUP_INDEX))
         ALLOCATE(HYDRO_AGGREGATION_POSITION(MAX_COAL_GROUP_INDEX))
         ALLOCATE(HYDRO_AGGREGATION_INDEX(MAX_COAL_GROUP_INDEX))
         ALLOCATE(PLANNING_AREA_POSITION(MAX_COAL_GROUP_INDEX))
         ALLOCATE(PLANNING_AREA_INDEX(MAX_COAL_GROUP_INDEX))
         ALLOCATE(ASSET_CLASS_GROUPS_INDEX(0:MAX_ASSET_GROUPS))
         ALLOCATE(ASSET_CLASS_2_GN(0:MAX_ASSET_GROUPS))
         ALLOCATE(ASSET_CLASS_GROUP_2_AC(MAX_COAL_GROUP_INDEX))

         COAL_GROUP_INDEX = 0
         GN_SCENARIO_VARIABLE_INDEX = 0
         HYDRO_GROUP_2_GN = 0
         COAL_GROUP_POSITION = 0
         COAL_GROUP_STATE_POSITION = 0
         NODE_FROM_STATE_POSITION = 0
         GN_2_HYDRO_GROUP = 0
         GN_2_PLANNING_AREA = 0
         HYDRO_AGGREGATION_POSITION = 0
         HYDRO_AGGREGATION_INDEX = 0
         PLANNING_AREA_POSITION = 0
         PLANNING_AREA_INDEX = 0
         ASSET_CLASS_GROUPS_INDEX = 0
         ASSET_CLASS_2_GN = 0
         ASSET_CLASS_GROUP_2_AC = 0
!
         CALL GET_COAL_NODE_RECORDS(COAL_GROUPS_RECORDS)
         SAVE_COAL_GROUPS_RECORDS = COAL_GROUPS_RECORDS
!
         CALL OPEN_GN_FILE
!
         IF(ALLOCATED(GROUP_NAME)) DEALLOCATE(
     +                                 GROUP_NAME,
     +                                 iPort,
     +                                 SNamePos,
     +                                 WORLD_DATABASE_INDEX,
     +                                 GROUP_ACTIVE,
     +                                 COAL_NODE_GROUP,
     +                                 COUNTRY_NODE_INDEX,
     +                                 BASECASE_MARKET_AREA_ID,
     +                                 BASE_CASE_COAL_AREA_ID,
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
     +                                 COAL_NODE_LATITUDE,
     +                                 COAL_NODE_LONGITUDE,
     +                                 COAL_NODE_STATE_PROVINCE,
     +                                 COAL_NODE_COUNTRY,
     +                                 COAL_NODE_TYPE_INDEX,
     +                                 COAL_HUB_REFERENCE_INDEX,
     +                                 MRX_VOLATILITY_MULT,
     +                                 REPORT_CL_CAPACITY,
!     +                                 REPORT_THIS_GROUP,
     +                                 ASSET_CLASS_ID,
     +                                 ASSET_CLASS_REV_ALLOC_VECTOR,
     +                                 TIME_ZONE,
     +                                 CAPACITY_ADDER,
     +                                 NOX_SEASON,
     +                                 NIGHT_SCARCITY_MULT,
     +                                 WEEKEND_SCARCITY_MULT,
     +                                 PRICE_CAP,
     +                                 MAX_HOURLY_GN_IMPORT,
     +                                 MAX_HOURLY_GN_EXPORT,
     +                                 HYDRO_LOAD_AGGREGATION,
     +                                 GN_REGIONAL_PLANNING_AREA,
     +                                 PURCHASE_POWER_ASSIGN,
     +                                 CREATE_HOURLY_PRICE,
     +                                 HOURLY_PRICE_NAME,
     +                                 PURCHASE_ASSET_CLASS_ID,
     +                                 PURCHASE_ASSET_ALLOC_VECTOR,
     +                                 OPERATING_COSTS,
     +                                 CAPACITY_VALUES,
     +                                 CUM_RES_REMAIN,
     +                                 POTENTIAL_RESERVES,
     +                                 PROVEN_RESERVES,
     +                                 MAX_DAILY_EXTRACTION_RATE,
     +                                 MAX_MONTHLY_EXTRACTION_RATE,
     +                                 SHRINKAGE_PERCENT,
     +                                 SHRINKAGE_COST )
         ALLOCATE(GROUP_NAME(COAL_GROUPS_RECORDS))
         ALLOCATE(iPort(COAL_GROUPS_RECORDS),
     +            SNamePos(COAL_GROUPS_RECORDS))
         ALLOCATE(WORLD_DATABASE_INDEX(COAL_GROUPS_RECORDS))
         ALLOCATE(GROUP_ACTIVE(COAL_GROUPS_RECORDS))
         ALLOCATE(COAL_NODE_GROUP(COAL_GROUPS_RECORDS))
         ALLOCATE(COUNTRY_NODE_INDEX(MAX_COAL_GROUP_INDEX))
         ALLOCATE(BASECASE_MARKET_AREA_ID(COAL_GROUPS_RECORDS))
         ALLOCATE(BASE_CASE_COAL_AREA_ID(COAL_GROUPS_RECORDS))
         ALLOCATE(BASECASE_SUBREGION_ID(COAL_GROUPS_RECORDS))
         ALLOCATE(SPINNING_UNITS(COAL_GROUPS_RECORDS))
         ALLOCATE(OFF_PEAK_SPINNING_UNITS(COAL_GROUPS_RECORDS))
         ALLOCATE(SPINNING_RESERVE(COAL_GROUPS_RECORDS))
         ALLOCATE(OFF_PEAK_SPINNING_RESERVE(COAL_GROUPS_RECORDS))
         ALLOCATE(MAX_HOURLY_RAMP_UP(COAL_GROUPS_RECORDS))
         ALLOCATE(MAX_HOURLY_RAMP_DOWN(COAL_GROUPS_RECORDS))
         ALLOCATE(FIRST_CAPACITY_VALUE(0:COAL_GROUPS_RECORDS))
         ALLOCATE(FIRST_CAPACITY_PERCENT(0:COAL_GROUPS_RECORDS))
         ALLOCATE(SECOND_CAPACITY_VALUE(0:COAL_GROUPS_RECORDS))
         ALLOCATE(SECOND_CAPACITY_PERCENT(0:COAL_GROUPS_RECORDS))
         ALLOCATE(THIRD_CAPACITY_VALUE(0:COAL_GROUPS_RECORDS))
         ALLOCATE(THIRD_CAPACITY_PERCENT(0:COAL_GROUPS_RECORDS))
         ALLOCATE(ADDITIONAL_CAPACITY_VALUE(0:COAL_GROUPS_RECORDS,7))
         ALLOCATE(ADDITIONAL_CAPACITY_PERCENT(0:COAL_GROUPS_RECORDS,7))
         ALLOCATE(RTO_GROUP(COAL_GROUPS_RECORDS))
         ALLOCATE(NOX_YEAR(COAL_GROUPS_RECORDS))
         ALLOCATE(END_NOX_YEAR(COAL_GROUPS_RECORDS))
         ALLOCATE(ST_LHS_FOR_PRICES(COAL_GROUPS_RECORDS))
         ALLOCATE(COAL_NODE_LATITUDE(COAL_GROUPS_RECORDS))
         ALLOCATE(COAL_NODE_LONGITUDE(COAL_GROUPS_RECORDS))
!         
         ALLOCATE(COAL_NODE_STATE_PROVINCE(MAX_COAL_GROUP_INDEX)) 
         ALLOCATE(COAL_NODE_COUNTRY(MAX_COAL_GROUP_INDEX))
         ALLOCATE(COAL_NODE_TYPE_INDEX(MAX_COAL_GROUP_INDEX)) 
         ALLOCATE(COAL_HUB_REFERENCE_INDEX(MAX_COAL_GROUP_INDEX))
!         
         ALLOCATE(MRX_VOLATILITY_MULT(COAL_GROUPS_RECORDS))
         ALLOCATE(NIGHT_SCARCITY_MULT(COAL_GROUPS_RECORDS))
         ALLOCATE(WEEKEND_SCARCITY_MULT(COAL_GROUPS_RECORDS))
         ALLOCATE(PRICE_CAP(COAL_GROUPS_RECORDS))
         ALLOCATE(MAX_HOURLY_GN_IMPORT(COAL_GROUPS_RECORDS))
         ALLOCATE(MAX_HOURLY_GN_EXPORT(COAL_GROUPS_RECORDS))
         ALLOCATE(HYDRO_LOAD_AGGREGATION(COAL_GROUPS_RECORDS))
         ALLOCATE(GN_REGIONAL_PLANNING_AREA(COAL_GROUPS_RECORDS))
         ALLOCATE(REPORT_CL_CAPACITY(COAL_GROUPS_RECORDS))
         ALLOCATE(ASSET_CLASS_ID(COAL_GROUPS_RECORDS))
         ALLOCATE(ASSET_CLASS_REV_ALLOC_VECTOR(COAL_GROUPS_RECORDS))
         ALLOCATE(TIME_ZONE(COAL_GROUPS_RECORDS))
         ALLOCATE(CAPACITY_ADDER(COAL_GROUPS_RECORDS))
         ALLOCATE(NOX_SEASON(COAL_GROUPS_RECORDS))
         ALLOCATE(PURCHASE_POWER_ASSIGN(COAL_GROUPS_RECORDS))
         ALLOCATE(CREATE_HOURLY_PRICE(COAL_GROUPS_RECORDS))
         ALLOCATE(HOURLY_PRICE_NAME(COAL_GROUPS_RECORDS))
         ALLOCATE(PURCHASE_ASSET_CLASS_ID(COAL_GROUPS_RECORDS))
         ALLOCATE(PURCHASE_ASSET_ALLOC_VECTOR(COAL_GROUPS_RECORDS))
         ALLOCATE(OPERATING_COSTS(25,COAL_GROUPS_RECORDS))
         ALLOCATE(CAPACITY_VALUES(25,COAL_GROUPS_RECORDS))
         ALLOCATE(CUM_RES_REMAIN(25,COAL_GROUPS_RECORDS))
         ALLOCATE(POTENTIAL_RESERVES(COAL_GROUPS_RECORDS))
         ALLOCATE(PROVEN_RESERVES(COAL_GROUPS_RECORDS))
         ALLOCATE(MAX_DAILY_EXTRACTION_RATE(COAL_GROUPS_RECORDS))
         ALLOCATE(MAX_MONTHLY_EXTRACTION_RATE(COAL_GROUPS_RECORDS))
         ALLOCATE(SHRINKAGE_PERCENT(COAL_GROUPS_RECORDS))
         ALLOCATE(SHRINKAGE_COST(COAL_GROUPS_RECORDS)) 
! MOVED 5/8/00
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
         COAL_NODE_STATE_PROVINCE = '  '
         COAL_NODE_COUNTRY = '  '
         COAL_NODE_TYPE_INDEX = 0
         COAL_HUB_REFERENCE_INDEX = 0
         COAL_NODE_TYPE_COUNT = 0 
!
         BELONGS_TO_COAL_GROUP = 0
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

         COAL_GROUP = 1
         NUMBER_OF_ACTIVE_COAL_GROUPS = 0
         NUMBER_OF_HYDRO_GROUPS = 0
         NUMBER_OF_PLANNING_GROUPS = 0
         MAX_COAL_GROUP_NUMBER = 0
         MAX_ASSET_CLASS_GROUPS = 0
         HYDRO_AGGREGATION = .FALSE.
         PLANNING_AREA = 0 ! .FALSE.
         FIRST_REPORTING_GROUP = 0
         WORLD_DATABASE_INDEX = 0
         iPort = 0
         SNamePos = 0
         GROUP_NAME = 'ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ'
         COUNTRY_NODE_INDEX = 0
         COUNTRY_NODE_COUNT = 0 
!
         DO CURRENT_RECORD = 1, COAL_GROUPS_RECORDS
            READ(10,REC=CURRENT_RECORD) DELETE,
     +                          GROUP_NAME(COAL_GROUP),                ! 1
     +                          GROUP_ACTIVE(COAL_GROUP),
     +                          COAL_NODE_GROUP(COAL_GROUP),
     +                          BASECASE_MARKET_AREA_ID(COAL_GROUP),
     +                          BASE_CASE_COAL_AREA_ID(COAL_GROUP),
     +                          BASECASE_SUBREGION_ID(COAL_GROUP),
     +                          SPINNING_UNITS(COAL_GROUP),
     +                          SPINNING_RESERVE(COAL_GROUP),
     +                          COMMENT,
     +                          MAX_HOURLY_RAMP_UP(COAL_GROUP),        ! 10
     +                          MAX_HOURLY_RAMP_DOWN(COAL_GROUP),      ! 11
     +                          CAPACITY_VALUES(1,COAL_GROUP),      ! 12
     +                          CUM_RES_REMAIN(1,COAL_GROUP),    ! 13
     +                          CAPACITY_VALUES(2,COAL_GROUP),     ! 14
     +                          CUM_RES_REMAIN(2,COAL_GROUP),   ! 15
     +                          CAPACITY_VALUES(3,COAL_GROUP),      ! 16
     +                          CUM_RES_REMAIN(3,COAL_GROUP),    ! 17
     +                          REPORT_CL_CAPACITY(COAL_GROUP),
     +                          ASSET_CLASS_ID(COAL_GROUP),
     +                         ASSET_CLASS_REV_ALLOC_VECTOR(COAL_GROUP), ! 20
     +                          TIME_ZONE(COAL_GROUP),
     +                          POTENTIAL_RESERVES(COAL_GROUP),           ! 22
     +                          NOX_SEASON(COAL_GROUP),
     +                          PURCHASE_POWER_ASSIGN(COAL_GROUP),
     +                          PURCHASE_ASSET_CLASS_ID(COAL_GROUP),
     +                          PURCHASE_ASSET_ALLOC_VECTOR(COAL_GROUP),
     +                          CREATE_HOURLY_PRICE(COAL_GROUP),
     +                          HOURLY_PRICE_NAME(COAL_GROUP),
     +                          CAPACITY_VALUES(4:10,COAL_GROUP),    ! 29-35
     +                          CUM_RES_REMAIN(4:10,COAL_GROUP),  ! 36-42
     +                          RTO_GROUP(COAL_GROUP),
     +                          MRX_VOLATILITY_MULT(COAL_GROUP),
     +                          NOX_YEAR(COAL_GROUP),
     +                          NIGHT_SCARCITY_MULT(COAL_GROUP),          ! 46
     +                          PROVEN_RESERVES(COAL_GROUP),              ! 47
     +                          HYDRO_LOAD_AGGREGATION(COAL_GROUP),
     +                          OFF_PEAK_SPINNING_RESERVE(COAL_GROUP),
     +                          OFF_PEAK_SPINNING_UNITS(COAL_GROUP),      ! 50
     +                          PRICE_CAP(COAL_GROUP),                    ! 51
     +                          MAX_DAILY_EXTRACTION_RATE(COAL_GROUP),    ! 52
     +                          MAX_MONTHLY_EXTRACTION_RATE(COAL_GROUP),
     +                          PLANNING_AREA,
     +                          SCENARIO_VARIABLE,
     +                          END_NOX_YEAR(COAL_GROUP),
     +                          ST_LHS_FOR_PRICES(COAL_GROUP),
     +                          COAL_NODE_LATITUDE(COAL_GROUP),
     +                          COAL_NODE_LONGITUDE(COAL_GROUP),
     +                          LOCAL_STATE_PROVINCE_NAME,
     +                          COAL_NODE_TYPE,
     +                          COAL_HUB_REFERENCE,
     +                          LOCAL_COUNTRY_NAME
            IF(GROUP_ACTIVE(COAL_GROUP) == 'F') CYCLE
!
! 10/03/02. REGIONAL CONSOLIDATION FOR BURESH.
!
            IF(COAL_NODE_GROUP(COAL_GROUP) < 0) THEN
               DO I = 1, 30
                  TEMP_I = INT2(
     +                 GET_VAR(FLOAT(COAL_NODE_GROUP(COAL_GROUP)),I,
     +                                          "COAL_NODE Group   "))
!
                  IF(TEMP_I == 0) EXIT
!
                  BELONGS_TO_COAL_GROUP(TEMP_I) = 
     +                               ABS(COAL_NODE_GROUP(COAL_GROUP))
               ENDDO
               COAL_NODE_GROUP(COAL_GROUP) = 
     +                               ABS(COAL_NODE_GROUP(COAL_GROUP))
            ELSE
               BELONGS_TO_COAL_GROUP(COAL_NODE_GROUP(COAL_GROUP)) = 
     +                                    COAL_NODE_GROUP(COAL_GROUP)
            ENDIF
!
            IF(MAX_HOURLY_RAMP_UP(COAL_GROUP) == 0. .AND.
     +                     MAX_HOURLY_RAMP_DOWN(COAL_GROUP) == 0.) THEN
! ASSUME THAT THEY JUST ASSIGNED A ZERO DEFAULT
               MAX_HOURLY_RAMP_UP(COAL_GROUP) = 9999999.
               MAX_HOURLY_RAMP_DOWN(COAL_GROUP) = 9999999.            
            ENDIF
!
! OFF-PEAK INHERITS PEAK VALUES
!            
            IF(OFF_PEAK_SPINNING_RESERVE(COAL_GROUP) == -99999.) THEN
               OFF_PEAK_SPINNING_RESERVE(COAL_GROUP) =
     +                                     SPINNING_RESERVE(COAL_GROUP)
            ENDIF
!            
            IF(OFF_PEAK_SPINNING_UNITS(COAL_GROUP) == 'Z') THEN
               OFF_PEAK_SPINNING_UNITS(COAL_GROUP) =
     +                                       SPINNING_UNITS(COAL_GROUP)
            ENDIF
!            
! MOVED 10/26/00. GAT. FOR WVPA POINTER CAPABILITY.
!            IF(SPINNING_UNITS(COAL_GROUP) == 'P' .OR.
!     +                          SPINNING_UNITS(COAL_GROUP) == 'C') THEN
!               SPINNING_RESERVE(COAL_GROUP) = 
!     +                              SPINNING_RESERVE(COAL_GROUP) / 100.
!            ENDIF
            IF(COAL_GROUP_POSITION(COAL_NODE_GROUP(COAL_GROUP)) == 
     +                                                           0) THEN
               NUMBER_OF_ACTIVE_COAL_GROUPS = 
     +                                  NUMBER_OF_ACTIVE_COAL_GROUPS + 1
               MAX_COAL_GROUP_NUMBER = MIN(MAX_COAL_GROUP_INDEX,
     +                                  MAX(MAX_COAL_GROUP_NUMBER,
     +                                  COAL_NODE_GROUP(COAL_GROUP)))
               COAL_GROUP_INDEX(NUMBER_OF_ACTIVE_COAL_GROUPS) =
     +                                    COAL_NODE_GROUP(COAL_GROUP)
               COAL_GROUP_POSITION(COAL_NODE_GROUP(COAL_GROUP)) =
     +                                      NUMBER_OF_ACTIVE_COAL_GROUPS
!
! 120606.
!
               GN = COAL_NODE_GROUP(COAL_GROUP)
               COAL_NODE_STATE_PROVINCE(GN) = 
     +                                         LOCAL_STATE_PROVINCE_NAME
               COAL_NODE_COUNTRY(GN) = 
     +                                                LOCAL_COUNTRY_NAME
               COUNTRY_NODE_INDEX(GN) = 
     +                             GET_COUNTRY_INDEX(LOCAL_COUNTRY_NAME)
               IF(COUNTRY_NODE_INDEX(GN) > 0 .AND.
     +                         COUNTRY_NODE_INDEX(GN) < 101) THEN
                  IF(COUNTRY_NODE_COUNT(COUNTRY_NODE_INDEX(GN)) 
     +                                                        == 0) THEN
                     MAX_COUNTRIES = MAX_COUNTRIES + 1
                  ENDIF
               ENDIF
               COUNTRY_NODE_COUNT(COUNTRY_NODE_INDEX(GN)) =
     +             COUNTRY_NODE_COUNT(COUNTRY_NODE_INDEX(GN)) + 1
               IF(COAL_NODE_TYPE == 'DEMAND') THEN
                  COAL_NODE_TYPE_INDEX(GN) = 1
                  COAL_NODE_TYPE_COUNT(1) = COAL_NODE_TYPE_COUNT(1) + 1
               ELSEIF(COAL_NODE_TYPE == 'BASIN') THEN
                  COAL_NODE_TYPE_INDEX(GN) = 2
                  COAL_NODE_TYPE_COUNT(2) = COAL_NODE_TYPE_COUNT(2) + 1
               ELSEIF(COAL_NODE_TYPE == 'LNG') THEN
                  COAL_NODE_TYPE_INDEX(GN) = 3
                  COAL_NODE_TYPE_COUNT(3) = COAL_NODE_TYPE_COUNT(3) + 1
               ELSEIF(COAL_NODE_TYPE == 'INTERMEDIATE') THEN
                  COAL_NODE_TYPE_INDEX(GN) = 4
                  COAL_NODE_TYPE_COUNT(4) = COAL_NODE_TYPE_COUNT(4) + 1
               ELSEIF(COAL_NODE_TYPE == 'MARKET_PRICE') THEN
                  COAL_NODE_TYPE_INDEX(GN) = 5
                  COAL_NODE_TYPE_COUNT(5) = COAL_NODE_TYPE_COUNT(5) + 1
               ENDIF
               COAL_HUB_REFERENCE_INDEX(GN) = COAL_HUB_REFERENCE
               IF(COAL_NODE_TYPE_INDEX(GN) == 1) THEN
                  TEMP_STATE = LOCAL_STATE_PROVINCE_NAME(1:2)
                  TEMP_I2 = 
     +               STATE_2_COAL_REGION_LOOKUP(TEMP_STATE)
!               TEMP_I2 = STATE_ID_LOOKUP(LOCAL_STATE_PROVINCE_NAME)
!
! 012308. RESET FOR EUROPE. NEED TO EXTEND THE STATE_ID_LOOKUP
!
                  TEMP_I2 = MAX(1,MIN(500,TEMP_I2))
                  COAL_GROUP_STATE_POSITION(GN) = TEMP_I2
                  NODE_FROM_STATE_POSITION(TEMP_I2) = 
     +                                      NUMBER_OF_ACTIVE_COAL_GROUPS
               ELSE
                  TEMP_I2 = -1
               ENDIF
!               
!
!
               REPORT_THIS_GROUP(NUMBER_OF_ACTIVE_COAL_GROUPS) =
     +                                   REPORT_CL_CAPACITY(COAL_GROUP)
!
               IF(FIRST_REPORTING_GROUP == 0 .AND. 
     +                      REPORT_CL_CAPACITY(COAL_GROUP) == 'T') THEN
                  FIRST_REPORTING_GROUP = NUMBER_OF_ACTIVE_COAL_GROUPS
               ENDIF
!
               IF(INDEX(COMMENT,'Europe') /= 0) THEN
                  WORLD_DATABASE_INDEX(COAL_GROUP) = 2
               ENDIF
!
! 11/20/02.
!               
               GN_SCENARIO_VARIABLE_INDEX(
     +                                 NUMBER_OF_ACTIVE_COAL_GROUPS) = 
     +                             GET_SCENARIO_INDEX(SCENARIO_VARIABLE)
!
! 03/26/03. HARD-WIRED TO AVOID INADERTANT ACTIVATION (E.G. LGE)
!
               HYDRO_LOAD_AGGREGATION(COAL_GROUP) = 0 
!
               IF(HYDRO_LOAD_AGGREGATION(COAL_GROUP) == 0) THEN
                  GN_2_HYDRO_GROUP(NUMBER_OF_ACTIVE_COAL_GROUPS) = 0
!
! EMPLOY LOAD AGGREGATION TO A NEW GROUP.
!
               ELSEIF( HYDRO_AGGREGATION_POSITION(
     +                   HYDRO_LOAD_AGGREGATION(COAL_GROUP)) == 0) THEN
!     
                  HYDRO_AGGREGATION = .TRUE.
!                  
                  NUMBER_OF_HYDRO_GROUPS = NUMBER_OF_HYDRO_GROUPS + 1
!
                  HYDRO_GROUP_2_GN(NUMBER_OF_HYDRO_GROUPS) =
     +                                      NUMBER_OF_ACTIVE_COAL_GROUPS
                  GN_2_HYDRO_GROUP(NUMBER_OF_ACTIVE_COAL_GROUPS) =
     +                                            NUMBER_OF_HYDRO_GROUPS
!
                  HYDRO_AGGREGATION_POSITION(
     +                  HYDRO_LOAD_AGGREGATION(COAL_GROUP)) = 
     +                                            NUMBER_OF_HYDRO_GROUPS
                  HYDRO_AGGREGATION_INDEX(NUMBER_OF_HYDRO_GROUPS) = 
     +                               HYDRO_LOAD_AGGREGATION(COAL_GROUP)
!
! EMPLOY LOAD AGGREGATION TO A GROUP THAT HAS BEEN PREVIOUSLY USED.
!
               ELSE 
                  DO HG = 1, NUMBER_OF_HYDRO_GROUPS
                     IF(HYDRO_LOAD_AGGREGATION(COAL_GROUP) /= 
     +                                HYDRO_AGGREGATION_INDEX(HG)) CYCLE
                     GN_2_HYDRO_GROUP(NUMBER_OF_ACTIVE_COAL_GROUPS) = HG
                     HYDRO_GROUP_2_GN(HG) = NUMBER_OF_ACTIVE_COAL_GROUPS
                     EXIT
                  ENDDO
               ENDIF
!            
!
!              ADDED 10/7/98. GAT. TO CAPTURE COAL_SUP COALMISSION 
!                       REVENUES/EXPENSES FOR ASSET ANALYST.
!
               AC = ASSET_CLASS_ID(COAL_GROUP)
               IF(ASSET_CLASS_GROUPS_INDEX(AC) == 0) THEN
                  MAX_ASSET_CLASS_GROUPS = MAX_ASSET_CLASS_GROUPS + 1
                  ASSET_CLASS_GROUPS_INDEX(AC) = MAX_ASSET_CLASS_GROUPS
                  ASSET_CLASS_GROUP_2_AC(MAX_ASSET_CLASS_GROUPS) = AC
!                 
! TEMP. 12/27/01.  NOTE RE-ASSIGNMENT.
!
                  AC = PURCHASE_ASSET_CLASS_ID(COAL_GROUP)
                  ASSET_CLASS_2_GN(AC) = NUMBER_OF_ACTIVE_COAL_GROUPS
               ELSE
                  WRITE(4,*) "Duplicate Asset Class Detected in"
                  WRITE(4,*) "the COAL_NODE Group file."
                  WRITE(4,*) "COAL_NODE Group Name = ",
     +                                           GROUP_NAME(COAL_GROUP)
                  WRITE(4,*) "Asset Class Id = ",
     +                                       ASSET_CLASS_ID(COAL_GROUP)
                  WRITE(4,*) "Renumber or accumulate groups."
                  WRITE(4,*) " "
               ENDIF
!
               COAL_GROUP = COAL_GROUP + 1
!
            ELSE
               WRITE(4,*) "Duplicate COAL_NODE Group Detected in"
               WRITE(4,*) "the COAL_NODE Group file."
               WRITE(4,*) "COAL_NODE Group Name = ",
     +                                           GROUP_NAME(COAL_GROUP)
               WRITE(4,*) "Renumber or accumulate groups."
               WRITE(4,*) " "
            ENDIF
         ENDDO ! RECORD LOOP
!
         call IndexedSortAlphaOrder(COAL_GROUPS_RECORDS,
     +                              iPort,SNamePos,GROUP_NAME)
!            
         CALL CLOSE_GN_FILE
         SAVE_GN_FILE_EXISTS = .TRUE.
         MANAGE_COAL_NODE_FORECASTS = .TRUE.
      RETURN

      ENTRY GET_NUM_COUNTRIES()

         GET_NUM_COUNTRIES = MAX_COUNTRIES
      RETURN

      ENTRY GET_NODE_ALPHA_ORDER(R_COAL_GROUP)

         GET_NODE_ALPHA_ORDER = iPort(R_COAL_GROUP)
      RETURN

      ENTRY GET_COAL_NODE_TYPE_COUNT(R_COAL_GROUP) ! R_COAL_GROUP IS LNG'S

! 1=DEMAND, 2=BASIN, 3=LNG, 4=INTERMED, 5=MARKET PRICE
         GET_COAL_NODE_TYPE_COUNT= COAL_NODE_TYPE_COUNT(R_COAL_GROUP)
      RETURN

      ENTRY GET_WORLD_DATABASE_INDEX(R_COAL_GROUP)

         IF(.NOT. SAVE_GN_FILE_EXISTS) THEN
            GET_WORLD_DATABASE_INDEX = 0
         ELSEIF(R_COAL_GROUP > 0 .AND.
     +                       R_COAL_GROUP <= MAX_COAL_GROUP_NUMBER) THEN
            COAL_GROUP = COAL_GROUP_POSITION(R_COAL_GROUP)
            GET_WORLD_DATABASE_INDEX = WORLD_DATABASE_INDEX(COAL_GROUP)
         ELSE
            GET_WORLD_DATABASE_INDEX = 0
         ENDIF
!         WRITE(4,*) R_COAL_GROUP,COAL_GROUP,
!     +                                   WORLD_DATABASE_INDEX(COAL_GROUP)
      RETURN

      ENTRY GET_COAL_NODE_COUNTRY_INDEX(R_COAL_GROUP)

         GET_COAL_NODE_COUNTRY_INDEX = COUNTRY_NODE_INDEX(R_COAL_GROUP)
      RETURN

      ENTRY GET_COAL_NODE_TYPE_INDEX(R_COAL_GROUP)

         GET_COAL_NODE_TYPE_INDEX = COAL_NODE_TYPE_INDEX(R_COAL_GROUP)
      RETURN

      ENTRY GET_COAL_HUB_REFERENCE(R_COAL_GROUP)

         GET_COAL_HUB_REFERENCE = COAL_HUB_REFERENCE_INDEX(R_COAL_GROUP)
      RETURN

      ENTRY GET_COAL_GROUP_POSITION(R_COAL_GROUP)

         IF(.NOT. SAVE_GN_FILE_EXISTS) THEN
            GET_COAL_GROUP_POSITION = R_COAL_GROUP
         ELSEIF(R_COAL_GROUP > 0 .AND.
     +                       R_COAL_GROUP <= MAX_COAL_GROUP_NUMBER) THEN
            GET_COAL_GROUP_POSITION = 
     +                               COAL_GROUP_POSITION(R_COAL_GROUP)
         ELSE
!         
            GET_COAL_GROUP_POSITION = 0
!            
         ENDIF
      RETURN

      ENTRY GET_COAL_NODE_STATE_PROVINCE(R_COAL_GROUP,R_CHAR2_NAME)

         GET_COAL_NODE_STATE_PROVINCE = 1
         IF(.NOT. SAVE_GN_FILE_EXISTS) THEN
            R_CHAR2_NAME = COAL_NODE_STATE_PROVINCE(R_COAL_GROUP)
         ELSEIF(R_COAL_GROUP > 0 .AND.
     +                       R_COAL_GROUP <= MAX_COAL_GROUP_NUMBER) THEN
!            TEMP_I2 = COAL_GROUP_INDEX(R_COAL_GROUP)
!            IF(TEMP_I2 > 0) THEN
            IF(R_COAL_GROUP > 0 .AND. 
     +                        R_COAL_GROUP <= MAX_COAL_GROUP_INDEX) THEN
               R_CHAR2_NAME = COAL_NODE_STATE_PROVINCE(R_COAL_GROUP)
               GET_COAL_NODE_STATE_PROVINCE = R_COAL_GROUP
            ELSE
               R_CHAR2_NAME = '  '
            ENDIF
         ELSE
!         
            R_CHAR2_NAME = '  '
!            
         ENDIF
      RETURN

      ENTRY GET_NODE_FROM_STATE_POSITION(R_STATE_POSITION)

         IF(.NOT. SAVE_GN_FILE_EXISTS) THEN
            GET_COAL_GROUP_STATE_POSITION = R_STATE_POSITION
         ELSEIF(R_STATE_POSITION > 0 .AND.
     +                       R_STATE_POSITION <= 
     +                                          MAX_COAL_REGION_NO) THEN
               GET_NODE_FROM_STATE_POSITION = 
     +                        NODE_FROM_STATE_POSITION(R_STATE_POSITION)
         ELSE
!         
               GET_NODE_FROM_STATE_POSITION = 0
!            
         ENDIF
      RETURN

      ENTRY GET_COAL_GROUP_STATE_POSITION(R_COAL_GROUP)

         IF(.NOT. SAVE_GN_FILE_EXISTS) THEN
            GET_COAL_GROUP_STATE_POSITION = R_COAL_GROUP
         ELSEIF(R_COAL_GROUP > 0 .AND.
     +                       R_COAL_GROUP <= MAX_COAL_GROUP_NUMBER) THEN
            TEMP_I2 = COAL_GROUP_INDEX(R_COAL_GROUP)
            IF(TEMP_I2 > 0) THEN
               GET_COAL_GROUP_STATE_POSITION = 
     +                                COAL_GROUP_STATE_POSITION(TEMP_I2)
            ELSE
               GET_COAL_GROUP_STATE_POSITION = 0
            ENDIF
         ELSE
!         
            GET_COAL_GROUP_STATE_POSITION = 0
!            
         ENDIF
      RETURN

      ENTRY GET_COAL_GROUP_INDEX(R_COAL_GROUP)

         IF(.NOT. SAVE_GN_FILE_EXISTS) THEN
            GET_COAL_GROUP_INDEX = R_COAL_GROUP
         ELSEIF(R_COAL_GROUP > 0 .AND.
     +                  R_COAL_GROUP <= 
     +                              NUMBER_OF_ACTIVE_COAL_GROUPS) THEN
            GET_COAL_GROUP_INDEX = COAL_GROUP_INDEX(R_COAL_GROUP)
         ELSE
!         
            GET_COAL_GROUP_INDEX = 0
!            
         ENDIF
      RETURN

      ENTRY GET_BELONGS_TO_COAL_GROUP(R_COAL_GROUP) ! NOT INDEXED

         IF(R_COAL_GROUP > 0 .AND. R_COAL_GROUP <= 
     +                                       MAX_COAL_GROUP_INDEX) THEN
            GET_BELONGS_TO_COAL_GROUP = 
     +                              BELONGS_TO_COAL_GROUP(R_COAL_GROUP)
         ELSE
            GET_BELONGS_TO_COAL_GROUP = R_COAL_GROUP
         ENDIF
      RETURN

      ENTRY COAL_GROUP_ACTIVE_SWITCH(R_COAL_GROUP)

         IF(.NOT. SAVE_GN_FILE_EXISTS) THEN
            COAL_GROUP_ACTIVE_SWITCH = .TRUE.
         ELSEIF(R_COAL_GROUP > 0 .AND.
     +                      R_COAL_GROUP <= MAX_COAL_GROUP_INDEX) THEN
            IF(COAL_GROUP_POSITION(R_COAL_GROUP) > 0 .AND.
     +            COAL_GROUP_POSITION(R_COAL_GROUP) <=
     +                                      MAX_COAL_GROUP_NUMBER) THEN
               COAL_GROUP_ACTIVE_SWITCH = 
     +                 GROUP_ACTIVE(COAL_GROUP_POSITION(R_COAL_GROUP)) 
     +                                                            == 'T'
            ELSE
               COAL_GROUP_ACTIVE_SWITCH = .FALSE.
            ENDIF
         ELSE
            COAL_GROUP_ACTIVE_SWITCH = .FALSE.
         ENDIF
      RETURN

      ENTRY GET_GN_GROUP_NAME(R_COAL_GROUP,R_GET_GROUP_NAME)  ! LOGICAL

!        
         R_GET_GROUP_NAME = GROUP_NAME(
     +                                COAL_GROUP_POSITION(R_COAL_GROUP))
         GET_GN_GROUP_NAME = .TRUE.
      RETURN

      ENTRY GET_MAX_COAL_GROUP_NUMBER

         GET_MAX_COAL_GROUP_NUMBER = MAX_COAL_GROUP_NUMBER
      RETURN

      ENTRY GET_NUMBER_OF_COAL_ACTIVE_GROUPS

         GET_NUMBER_OF_COAL_ACTIVE_GROUPS = NUMBER_OF_ACTIVE_COAL_GROUPS
      RETURN
      END

      FUNCTION GET_COUNTRY_INDEX(R_COUNTRY_STRING)

         CHARACTER*6 R_COUNTRY_STRING
         INTEGER*2 GET_COUNTRY_INDEX
!         
         SELECT CASE(trim(R_COUNTRY_STRING))
            CASE ('USA') 
               GET_COUNTRY_INDEX = 1
            CASE ('CAN')
               GET_COUNTRY_INDEX = 2
            CASE ('MEX')
               GET_COUNTRY_INDEX = 3
            CASE DEFAULT
               GET_COUNTRY_INDEX = 0
         END SELECT
      END

!
! CHARACTER SECTION
! 
!
!

!
      FUNCTION SAVE_GN_CHAR_VARIABLES()
!

      INTEGER*2 R_COAL_GROUP
      CHARACTER*1 SAVE_GN_CHAR_VARIABLES
      CHARACTER*1 GET_COAL_GROUP_NOX_SEASON,
     +            R_GET_COAL_GROUP_NOX_SEASON
      CHARACTER*3 GET_HOURLY_PRICE_NAME,R_GET_HOURLY_PRICE_NAME
      CHARACTER*30 GET_COAL_GROUP_NAME,R_GET_GROUP_NAME
      CHARACTER*30 GET_COALACTION_GROUP_NAME,
     +             R_GET_COALACTION_GROUP_NAME
      CHARACTER*50 R_GET_LINK_NAME,GET_COAL_LINK_NAME
      LOGICAL*1 VOID_LOGICAL,GET_GN_HOURLY_PRICE_NAME,
     +          GET_GN_GROUP_NAME,GET_GN_COALACTION_GROUP_NAME,
     +          GET_GN_COAL_GROUP_NOX_SEASON,
     +          GET_LINK_NAME
         SAVE_GN_CHAR_VARIABLES = 'X'
      RETURN

!      ENTRY GET_HOURLY_PRICE_NAME(R_COAL_GROUP) ! CHARACTER

!         VOID_LOGICAL = GET_GN_HOURLY_PRICE_NAME(R_COAL_GROUP,
!     +                                        R_GET_HOURLY_PRICE_NAME) ! CHARACTER
!         GET_HOURLY_PRICE_NAME = R_GET_HOURLY_PRICE_NAME
!      RETURN

      ENTRY GET_COAL_GROUP_NAME(R_COAL_GROUP)  ! CHARACTER

         VOID_LOGICAL = GET_GN_GROUP_NAME(R_COAL_GROUP,R_GET_GROUP_NAME)  ! CHARACTER
         GET_COAL_GROUP_NAME = R_GET_GROUP_NAME
      RETURN

      ENTRY GET_COAL_LINK_NAME(R_COAL_GROUP)  ! CHARACTER

!
! R_COAL_GROUP IS THE POSITION WITHIN THE ACTIVE LINK LIST.
!
         VOID_LOGICAL = GET_LINK_NAME(R_COAL_GROUP,R_GET_LINK_NAME)  ! CHARACTER
         GET_COAL_LINK_NAME = R_GET_LINK_NAME
      RETURN

!      ENTRY GET_COALACTION_GROUP_NAME(R_COAL_GROUP)  ! CHARACTER

!         VOID_LOGICAL = GET_GN_COALACTION_GROUP_NAME(R_COAL_GROUP,
!     +                                     R_GET_COALACTION_GROUP_NAME)  ! CHARACTER
!         GET_COALACTION_GROUP_NAME = R_GET_COALACTION_GROUP_NAME
!      RETURN
      END

      FUNCTION MONTHLY_HH_BASIS(R_YEAR,R_MONTH)

      INTEGER*2 HH_ESC(6),R_YEAR,R_MONTH,TEMP_DAY/0/,TEMP_GS/0/
      REAL*4 MONTHLY_HH_BASIS,HH_COEFF(5),HH_MONTHLY_VALUES(6),
     +       ESCALATED_MONTHLY_VALUE,REAL4_ONE/1./,
     +       TEST_EPUC,
     +       CALC_ANNUAL_COAL_EPUC,
     +       GET_DAILY_COAL_DEMAND_BY_NODE,
     +       INTERUPTIBLE
      LOGICAL*1 GET_HH_PARAMETERS,TEMP_L,USE_DYNAMIC_EPUC,
     +          COAL_ONLY,COAL_MODEL_ONLY
      SAVE
! 033109. SHOULD BE FIRST OCCURENCE.
         IF(R_YEAR == 1) THEN
            HH_MONTHLY_VALUES(6) = 1.0
         ENDIF
!         
         TEMP_L = GET_HH_PARAMETERS(HH_ESC,HH_COEFF,USE_DYNAMIC_EPUC)
! WTI         
         HH_MONTHLY_VALUES(1) = ESCALATED_MONTHLY_VALUE(REAL4_ONE,
     +                                           HH_ESC(1),
     +                                           R_YEAR,R_MONTH,INT2(1))
! DEMAND     
         COAL_ONLY = COAL_MODEL_ONLY()
         IF(.NOT. COAL_ONLY) THEN
            HH_MONTHLY_VALUES(2) = 
     +         GET_DAILY_COAL_DEMAND_BY_NODE(TEMP_DAY,TEMP_GS,
     +                                                     INTERUPTIBLE)
         ELSE
            HH_MONTHLY_VALUES(2) = ESCALATED_MONTHLY_VALUE(REAL4_ONE,
     +                                           HH_ESC(2),
     +                                           R_YEAR,R_MONTH,INT2(1))
         ENDIF
! EPUC     
         HH_MONTHLY_VALUES(3) = ESCALATED_MONTHLY_VALUE(REAL4_ONE,
     +                                           HH_ESC(3),
     +                                           R_YEAR,R_MONTH,INT2(1))
! STORAGE     
         HH_MONTHLY_VALUES(4) = ESCALATED_MONTHLY_VALUE(REAL4_ONE,
     +                                           HH_ESC(4),
     +                                           R_YEAR,R_MONTH,INT2(1))
! COAL_MODEL_RESIDUAL_ESC     
         HH_MONTHLY_VALUES(5) = ESCALATED_MONTHLY_VALUE(REAL4_ONE,
     +                                           HH_ESC(5),
     +                                           R_YEAR,R_MONTH,INT2(1))
! COAL_MODEL_INFLATION_ESC     
         IF(HH_ESC(6) > 0) THEN
 !           HH_MONTHLY_VALUES(6) = 1.0
 !        ELSE
            HH_MONTHLY_VALUES(6) = ESCALATED_MONTHLY_VALUE(
     +                                           HH_MONTHLY_VALUES(6),
     +                                           HH_ESC(6),
     +                                           R_YEAR,R_MONTH,INT2(1))
         ENDIF
!
         TEST_EPUC = CALC_ANNUAL_COAL_EPUC(R_YEAR)
         IF(TEST_EPUC > 0.50 .AND. TEST_EPUC < 2.00 .AND. 
     +                                            USE_DYNAMIC_EPUC) THEN
            HH_MONTHLY_VALUES(3) = TEST_EPUC
         ENDIF
!         
         MONTHLY_HH_BASIS = 
     +               HH_MONTHLY_VALUES(6) *
     +                    ( HH_COEFF(1) + 
     +                      HH_COEFF(2) * HH_MONTHLY_VALUES(1) +
     +                      HH_COEFF(3) * HH_MONTHLY_VALUES(2) +
     +                      HH_COEFF(4) * HH_MONTHLY_VALUES(3) +
     +                      HH_COEFF(5) * HH_MONTHLY_VALUES(4) +
     +                                    HH_MONTHLY_VALUES(5) )
      RETURN
      END

      FUNCTION CALC_ANNUAL_COAL_EPUC(R_YEAR)

      LOGICAL*1 GET_COAL_ANNUAL_PEAK_VOLUME, TEMP_L
      INTEGER*2 R_YEAR,ZERO/0/,LOCAL_MONTH/1/
      REAL*4    CALC_ANNUAL_COAL_EPUC,
     +          SYS_PEAK,
     +          SYS_VOLUME,
     +          AVERAGE_SYSTEM_DAILY_DEMAND,
     +          AVERAGE_SYSTEM_DAILY_SUPPLY,
     +          GET_SYSTEM_DAILY_EXTRACT_RATE
     
!
! END DATA DECLARATIONS
!      
         TEMP_L = GET_COAL_ANNUAL_PEAK_VOLUME(ZERO,SYS_PEAK,SYS_VOLUME)
         AVERAGE_SYSTEM_DAILY_DEMAND = SYS_VOLUME / 365.0
!          
         AVERAGE_SYSTEM_DAILY_SUPPLY = 
     +                 GET_SYSTEM_DAILY_EXTRACT_RATE(R_YEAR,LOCAL_MONTH)
         IF(AVERAGE_SYSTEM_DAILY_SUPPLY > 0.01) THEN
            CALC_ANNUAL_COAL_EPUC = AVERAGE_SYSTEM_DAILY_DEMAND /
     +                                       AVERAGE_SYSTEM_DAILY_SUPPLY
         ELSE
            CALC_ANNUAL_COAL_EPUC = 9999.0
         ENDIF
!
! EPUC SHOULD BE BETWEEN 0.80 AND 0.99 OTHERWISE THERE WILL BE OVER OR UNDER SUPPLY
!         
      RETURN
      END FUNCTION
!      

!
!      SUBROUTINE COAL_PRICING_ROUTINE
      FUNCTION COAL_PRICING_ROUTINE()
!      

!

      use sizecom
      use globecom

!
      LOGICAL*1 DAILY_COAL_SELL_BUY_REPORT_NOT_OPEN/.TRUE./,
     +          YES_DAILY_COAL_SELL_BUY_REPORT,
     +          DAILY_COAL_SELL_BUY_REPORT,
     +          MONTHLY_COAL_SELL_BUY_REPORT_NOT_OPEN/.TRUE./,
     +          YES_MONTHLY_COAL_SELL_BUY_REPORT,
     +          MONTHLY_COAL_SELL_BUY_REPORT,
     +          MONTHLY_COAL_LINK_REPORT_NOT_OPEN/.TRUE./,
     +          YES_MONTHLY_COAL_LINK_REPORT,
     +          MONTHLY_COAL_LINK_REPORT,
     +          RUN_LP_COAL_MODEL,
     +          GregGetDaysDemand
      INTEGER  DAILY_COAL_SELL_BUY_REC,DAILY_COAL_PRICE_REC,
     +         DAILY_COAL_NODE_VOLUME_REC,
     +         DAILY_COAL_STORAGE_REC,
     +         DAILY_COAL_SUR_DEF_REC,
     +         DAILY_COAL_DIFFER_REC,
     +         DAILY_COAL_UNSERVED_REC,MONTHLY_COAL_PRICE_REC,
     +         MONTHLY_COAL_BALANCE_REC,
     +         MONTHLY_COAL_BASIN_REC,
     +         MONTHLY_COAL_DIFFER_REC,
     +         DAILY_COAL_LINK_REC,
     +         DAILY_COAL_LINK_LIMIT_REC,
     +         DAILY_COAL_LINK_PRICE_REC,
     +         DAILY_COAL_LINK_CF_REC,
     +         MONTHLY_COAL_LINK_PRICE_REC,
     +         MONTHLY_COAL_SELL_BUY_REC,
     +         MONTHLY_COAL_LINK_REC,
     +         MONTHLY_COAL_RESERVE_REC,
     +         DAILY_COAL_AVE_PRICE_REC,
     +         R_ErrOrEnd
      INTEGER*2 DAILY_COAL_SELL_BUY_UNIT,DAILY_COAL_SELL_BUY_HEADER,
     +          DAILY_COAL_LINK_UNIT,DAILY_COAL_LINK_HEADER,
     +          DAILY_COAL_LINK_LIMIT_UNIT,DAILY_COAL_LINK_LIMIT_HEADER,
     +          DAILY_COAL_LINK_PRICE_UNIT,DAILY_COAL_LINK_PRICE_HEADER,
     +          DAILY_COAL_LINK_CF_UNIT,DAILY_COAL_LINK_CF_HEADER,
     +          MONTHLY_COAL_LINK_PRICE_UNIT,
     +                                  MONTHLY_COAL_LINK_PRICE_HEADER,
     +          MONTHLY_COAL_SELL_BUY_UNIT,MONTHLY_COAL_SELL_BUY_HEADER,
     +          MONTHLY_COAL_LINK_UNIT,MONTHLY_COAL_LINK_HEADER,
     +          MONTHLY_COAL_RESERVE_UNIT,MONTHLY_COAL_RESERVE_HEADER,
     +          DAILY_COAL_PRICE_UNIT,DAILY_COAL_PRICE_HEADER,
     +          DAILY_COAL_NODE_VOLUME_UNIT,
     +          DAILY_COAL_NODE_VOLUME_HEADER,
     +          DAILY_COAL_STORAGE_UNIT,DAILY_COAL_STORAGE_HEADER,
     +          DAILY_COAL_SUR_DEF_UNIT,DAILY_COAL_SUR_DEF_HEADER,
     +          DAILY_COAL_AVE_PRICE_UNIT,DAILY_COAL_AVE_PRICE_HEADER,
     +          DAILY_COAL_DIFFER_UNIT,DAILY_COAL_DIFFER_HEADER,
     +          MONTHLY_COAL_PRICE_UNIT,MONTHLY_COAL_PRICE_HEADER,
     +          MONTHLY_COAL_BALANCE_UNIT,MONTHLY_COAL_BALANCE_HEADER,
     +          BALANCE_VARS/13/,
     +          MONTHLY_COAL_DIFFER_UNIT,MONTHLY_COAL_DIFFER_HEADER,
     +          MONTHLY_COAL_BASIN_HEADER,MONTHLY_COAL_BASIN_UNIT,
     +          DAILY_COAL_UNSERVED_UNIT,DAILY_COAL_UNSERVED_HEADER,
     +          COAL_BASIS_ID/317/,COAL_BASIS_POS,
     +          GET_NUM_OF_COAL_SUPPLY_REGIONS,MAX_SUPPLY_NODES,
     +          GET_MAX_COAL_LOAD_GROUPS,MAX_COAL_LOAD_GROUPS,
     +          GET_ACTIVE_LINK_NUMBER
      CHARACTER*5 GET_SCENAME
      CHARACTER (LEN=20) COAL_NODE_TYPE(5)/
     +                                    'State or Province',
     +                                    'Basin',
     +                                    'LNG',
     +                                    'Intermediate',
     +                                    'Delivery Point'/
      CHARACTER* 30 GET_COAL_GROUP_NAME,TEMP_NAME
      CHARACTER*50 GET_COAL_LINK_NAME
      CHARACTER* 256 TEMP_STR,TEMP_STR_2
      INTEGER*2 LAST_SEASON,PRODUCTION_PERIODS,ANNUAL_COUNTER/0/,
     +          LOCAL_YEAR
      CHARACTER*9 CL_MONTH_NAME(13)/
     +      'January','February','March','April','May','June',
     +      'July','August','September','October',
     +      'November','December','Annual'/
!      
      LOGICAL*1   TEMP_L,GET_SUPPLY_COST_CAPACITY_CURVES,
     +            PUT_SUPPLY_CAPACITY_CURVES,
     +            DEMAND_NODE_ACTIVE(:),
     +            IS_DEMAND_NODE(:),
     +            MARGINAL_COSTING_ROUTINE/.TRUE./,
     +            SUPPLY_NODE_ACTIVE(:),
     +            COAL_PRICING_ROUTINE,
     +            MONTHLY_COAL_PRICING,
     +            ANNUAL_LP_COAL_PRICING,
     +            monthly_lp_COAL_pricing,
     +            put_monthly_lp_remaining_COAL,
     +            TEST_COAL_LINK_CF,
     +            POSITION_FOUND,
     +            INCREMENT_PC_COUNT,
     +            COAL_DEMAND_GROUP_ACTIVE,
     +            COAL_SUPPLY_GROUP_ACTIVE,
     +            DIAG_NOT_OPEN/.TRUE./,
     +            RUN_TRANSACT_THIS_MONTH,
     +            NORTH_AMERICAN_DATABASE,
     +            SUPPLY_ACTIVE(:),
     +            ITERATION_ON,
     +            WRITE_LNG_TO_DAT,
     +            PROCESS_DAILY_STORAGE,
     +            STORAGE_ACTIVE,
     +            MONTHLY_COAL_LINK_INIT
      INTEGER*2   R_HOURS_IN_MONTH,DAYS_IN_MONTH,DA,
     +            I,S,T,R_YEAR,R_MONTH,
     +            MAX_NODES,J,K,L,P,D,M,
     +            GET_NODE_OF_PATH,C,
     +            GET_MAX_COAL_GROUP_NUMBER,
     +            MAX_NODE_ID,
     +            MAX_LINK_ID,GET_MAX_LINK_ID,
     +            GET_NUMBER_OF_J_TO_K_PATHS,
     +            GET_NUMBER_OF_COAL_ACTIVE_GROUPS,
     +            MAX_S,MAX_S_FOR_I(:),
     +            MAX_C_FOR_(:,:),
     +            NODE_INDEX(:),
!     +            TOTAL_PC_FOR_(:,:),
     +            CURRENT_C_FOR_(:,:),
     +            GET_MAX_C_FOR_,
     +            FIRST_S,FIRST_S_FOR_I(:),
     +            LAST_S,
     +            MAX_S_IN_MONTH/2/,
     +            DEMAND_GROUPS_SATISFIED,
     +            GS,GET_COAL_GROUP_INDEX,
     +            GET_COAL_GROUP_POSITION,
     +            L_I,L_J,L_K,L_P,L_Q,T_GS,
     +            SOURCE_ID_STORAGE(:),
     +            GSP,LOCAL_GSP,R_GSP,
     +            GET_NODE_FROM_STATE_POSITION,
     +            GET_TieID_INDEX,
     +            MAX_ACTIVE_COAL_LINKS,
     +            MAX_COUNTRIES,CO,
     +            GET_NUM_COUNTRIES,
     +            DAILY_DEMAND_INDEX(:),
     +            GET_COAL_LINK_ID,
     +            NODE_TYPE(:),
     +            NODE_COUNTRY(:),
     +            HH_ESC(4),
     +            GS_POS(:),
     +            GT_POS(:),GT,
     +            GET_COAL_NODE_TYPE_INDEX,
     +            GET_COAL_NODE_COUNTRY_INDEX,
     +            GET_COAL_NODE_TYPE_COUNT,
     +            TEMP_I2,
     +            NUM_LNG,
     +            GET_WORLD_DATABASE_INDEX,
     +            GET_NODE_ALPHA_ORDER,
     +            GET_LINK_ALPHA_ORDER,
     +            ALPHA_J,
     +            GET_ITERATION_NUMBER,
     +            LNG_INDEX(:),
     +            LNG_HUB_INDEX(:),
     +            GET_COAL_HUB_REFERENCE,
     +            HUB_GS,
     +            R_MAX_NODES,
     +            R_MAX_ACTIVE_COAL_LINKS
! 
      INTEGER     TOTAL_PATHS,GET_TOTAL_PATHS_FOR_SYSTEM,PATH,
     +            PATHS_N_CURVES,
     +            TOTAL_DELIVERED_POSITION(:),
     +            DAILY_TOTAL_DELIVERED_POS(:),
     +            TOTAL_REVERSE_POSITION(:),
     +            DAILY_TOTAL_REVERSE_POS(:),
     +            GET_PATH_FOR_J_TO_K,
!     +            PC_FOR_(:,:,:),
     +            PC,TOTAL_PC,
     +            P_M,P_N,L_PC,
     +            PC_PATH(:),PC_S(:),
!     +            PC_C(:),
     +            PC_I(:),
     +            TEMP_I4,
     +            TOTAL_PC_FOR_INTERNAL_TRANS,PC_COUNT,
     +            GET_LEAF_OF_PATH,
     +            GET_PARENT_OF_LEAF,
     +            GET_PARENT_PATH_OF_LEAF,
     +            LOCAL_PATH,
     +            LAST_PATH(:),
     +            DEPTH_OF_LAST_PATH(:),
     +            PARENT_PATH,NODE_AT_DEPTH(:),DEPTH,
     +            PATH_AT_DEPTH(:),
     +            MAX_POINTS_IN_GS,GET_MAX_POINTS_IN_GS,
     +            PATH_2_PC(:,:),LOCAL_PC,Q,TEMP_Q,
     +            GET_NUMBER_OF_SUPPLY_PATHS,
     +            GET_SUPPLY_CURVE_POINTS,
     +            TEMP_I4_1,
     +            TEMP_I4_2,
     +            CALC_SUPPLY_PATHS,
     +            MAX_PC_FOR_LINK/1000/
!
      REAL* 4     COST_CURVE,CAPACITY_CURVE,EXTRACT_CURVE,
     +            MIN_EXTRACT_RATE,
     +            MIN_BASIN_COST,
     +            HARD_EXTRACT_RATE,
     +            ANNUAL_PRODUCTION_TARGET,
     +            TOTAL_DELIVERED_COST(:),
     +            LNG_SCENARIO_PRICE(:,:,:),
     +            DAILY_TOTAL_DELIVERED_COST(:),
     +            TOTAL_DELIVERED_QUANT(:),
     +            DAILY_PRICE(:),
     +            DAILY_MARG_NODE_PRICE(:),
     +            DAILY_AVE_NODE_PRICE(:),
     +            MONTHLY_COAL_BALANCE(:,:,:),
     +            MONTHLY_NODE_PRICE(:),
     +            MONTHLY_NODE_RESERVE(:),
     +            DAILY_LINK_VOLUME(:),
     +            DAILY_LINK_PRICE(:),
     +            DAILY_LINK_CF(:),
     +            DAILY_LINK_LIMIT(:),
     +            DAILY_LINK_COMMOD_COST(:),
     +            DAILY_NODE_VOLUME(:),
     +            DAILY_COAL_SUR_DEF_VOLUME(:),
     +            MONTHLY_NODE_VOLUME(:),
     +            MONTHLY_LINK_VOLUME(:),
     +            MONTHLY_LINK_PRICE(:),
     +            ANNUAL_NODE_PRICE(:),
     +            ANNUAL_NODE_VOLUME(:),
     +            ANNUAL_LINK_PRICE(:),
     +            ANNUAL_LINK_VOLUME(:),
     +            DAILY_DIFFER(:),
     +            MONTHLY_PRICE(:),
     +            MONTHLY_DIFFER(:),
     +            MONTHLY_VOLUME(:),
     +            MONTHLY_IMPORT(:),
     +            MONTHLY_EXPORT(:),
     +            COAL_STORAGE_WITHDRAWAL(:),
     +            REGION_ALLOCATION_STORAGE(:),
     +            A_COEFFICIENT_STORAGE(:),
     +            B_COEFFICIENT_STORAGE(:),
     +            GET_COAL_TRANSPORT_RATE,
     +            DAILY_DEMAND(:),
     +            DAILY_INPUT_DEMAND(:),
     +            LP_DAILY_DEMAND(:),
     +            LP_NODE_COST(:),
     +            DAILY_INTER_AVAIL(:),
     +            DAILY_INTER_USED(:),
     +            GET_COAL_TRANSPORT_QUANT,
     +            GET_COAL_LINK_QUANT,
     +            GET_DAILY_COAL_DEMAND_BY_NODE,
     +            TRANQ,TRAN_S,
     +            GET_COAL_TRANSPORT_LOSSES,
     +            LOCAL_QUANT,
     +            AVAIL_COAL_GIVEN_LINK_CONGEST,
     +            DAILY_BOUGHT(:,:),
     +            MONTHLY_BOUGHT(:,:),
     +            DAILY_SUPPLY(:),
     +            MONTHLY_SUPPLY(:),
     +            SUPPLY_AVAILABLE(:),
     +            LINK_AVAILABLE(:,:),
     +            LINK_LOADING(:,:),
     +            MONTHLY_LINK_LOADING(:,:),
     +            TOTAL_PATH_COST(:),
     +            TOTAL_PATH_QUANT(:),
     +            TOTAL_PATH_LOSSES(:),
     +            LOCAL_COST_CURVE(:),
     +            LOCAL_EXTRACT_CURVE(:),
     +            LOCAL_EXTRACT_IN_DAY(:),
     +            LOCAL_HARD_EXTRACT_IN_DAY(:),
     +            LOCAL_MIN_EXTRACT_IN_DAY(:),
     +            CONSTANT_EXTRACT_IN_DAY(:),
     +            CONSTANT_HARD_EXTRACT_IN_DAY(:),
     +            LOCAL_CAPACITY_CURVE(:),
     +            LOCAL_ANNUAL_PRODUCTION_TARGET(:),
     +            LOCAL_COAL_TRANSPORT_RATE(:,:),
     +            LOCAL_COAL_CONGESTION_COST(:,:),
     +            LOCAL_COAL_TRANSPORT_QUANT(:,:),
     +            LOCAL_COAL_TRANSPORT_LOSSES(:,:),
     +            LOCAL_COAL_SHRINKAGE_PERCENT(:),
     +            LOCAL_COAL_SHRINKAGE_COST(:),
     +            PRODUCTION_SCARCITY(:),
     +            MONTHLY_SUPPLY_BY_BASIN(:),
     +            CONGESTION_QUANT(:,:),
     +            LOCAL_CONGESTION_QUANT,
     +            SHRINKAGE_COST,SHRINKAGE_PERCENT,
     +            FUEL_RETENTION_CHARGE/.01/,
     +            TOTAL_AVAIL,
     +            QUANTITY_TRANSPORTED,
     +            QUANTITY_DEMANDED,
     +            QUANTITY_SUPPLIED,
     +            TEMP_R,
     +            GET_MONTHLY_COAL_PRICE_BY_STATE,
     +            CAP_UTIL,
     +            TEMP_TRANSPORTED,
     +            GET_COAL_TRANS_CONGEST_COST,
     +            CF,
     +            CONGESTION_COST_INCREASE,
!     +            R_NodeCap(*),
     +            FLOAT_DAYS_IN_MONTH,
     +            MONTHLY_HH_BASIS,HH_ADJ,HH_SUPPLY_COST,
     +            HH_NODE_NUM,
     +            TEMP_STORAGE,
     +            MIDWEST_STORAGE,
     +            MIDWEST_DEMAND,
     +            STORAGE_WITHDRAWAL(:),
     +            STORAGE_INJECTION(:),
     +            GET_NODE_DAILY_STORAGE,
     +            TEST_ANN_PROD_PERCENT(10)/
     +                     0.8,0.9,0.95,0.96,0.97,
     +                     0.98,0.985,0.99,0.995,1.0/,
     +            TEST_ANN_PROD_SCARCITY(10)/
     +                      1,2,4,6,9,12,14,16,18,20/,
     +            TEST_DEPLETION
!     
      ALLOCATABLE :: TOTAL_DELIVERED_COST,
     +               LNG_SCENARIO_PRICE,
     +               LNG_INDEX,
     +               LNG_HUB_INDEX,
     +               DAILY_TOTAL_DELIVERED_COST,
     +               TOTAL_DELIVERED_QUANT,
     +               DAILY_PRICE,
     +               DAILY_MARG_NODE_PRICE,
     +               DAILY_AVE_NODE_PRICE,
     +               MONTHLY_NODE_PRICE,
     +               MONTHLY_COAL_BALANCE,
     +               ANNUAL_NODE_PRICE,
     +               ANNUAL_NODE_VOLUME,
     +               ANNUAL_LINK_PRICE,
     +               ANNUAL_LINK_VOLUME,
     +               MONTHLY_NODE_RESERVE,
     +               DAILY_LINK_VOLUME,
     +               DAILY_LINK_PRICE,
     +               DAILY_LINK_CF,
     +               DAILY_LINK_LIMIT,
     +               DAILY_LINK_COMMOD_COST,
     +               DAILY_NODE_VOLUME,
     +               DAILY_COAL_SUR_DEF_VOLUME,
     +               MONTHLY_NODE_VOLUME,
     +               MONTHLY_LINK_VOLUME,
     +               MONTHLY_LINK_PRICE,
     +               DAILY_DIFFER,
     +               MONTHLY_PRICE,
     +               MONTHLY_DIFFER,
     +               MONTHLY_VOLUME,
     +               MONTHLY_IMPORT,
     +               MONTHLY_EXPORT,
     +               COAL_STORAGE_WITHDRAWAL,
     +               REGION_ALLOCATION_STORAGE,
     +               A_COEFFICIENT_STORAGE,
     +               B_COEFFICIENT_STORAGE,
     +               STORAGE_WITHDRAWAL,
     +               STORAGE_INJECTION,
     +               SUPPLY_ACTIVE,
     +               SOURCE_ID_STORAGE,
     +               TOTAL_DELIVERED_POSITION,
     +               DAILY_TOTAL_DELIVERED_POS,
     +               TOTAL_REVERSE_POSITION,
     +               DAILY_TOTAL_REVERSE_POS,
     +               NODE_AT_DEPTH,
     +               PATH_AT_DEPTH,
     +               DAILY_DEMAND,
     +               DAILY_INPUT_DEMAND,
     +               LP_DAILY_DEMAND,
     +               GS_POS,
     +               GT_POS,
     +               LP_NODE_COST,
     +               DAILY_DEMAND_INDEX,
     +               NODE_TYPE,
     +               NODE_COUNTRY,
     +               DAILY_INTER_AVAIL,
     +               DAILY_INTER_USED,
     +               DEMAND_NODE_ACTIVE,
     +               IS_DEMAND_NODE,
     +               SUPPLY_NODE_ACTIVE,
     +               MAX_S_FOR_I,
     +               MAX_C_FOR_,
!     +               TOTAL_PC_FOR_,
!     +               PC_FOR_,
     +               CONGESTION_QUANT,
     +               CURRENT_C_FOR_,
     +               FIRST_S_FOR_I,
     +               PC_PATH,
     +               PC_S,
!     +               PC_C,
     +               PC_I,
     +               LAST_PATH,
     +               DEPTH_OF_LAST_PATH,
     +               PATH_2_PC,
     +               DAILY_BOUGHT,
     +               MONTHLY_BOUGHT,
     +               DAILY_SUPPLY,
     +               MONTHLY_SUPPLY,
     +               SUPPLY_AVAILABLE,
     +               LINK_AVAILABLE,
     +               LINK_LOADING,
     +               MONTHLY_LINK_LOADING,
     +               TOTAL_PATH_COST,
     +               TOTAL_PATH_QUANT,
     +               TOTAL_PATH_LOSSES,
     +               LOCAL_COST_CURVE,
     +               LOCAL_EXTRACT_CURVE,
     +               LOCAL_EXTRACT_IN_DAY,
     +               LOCAL_HARD_EXTRACT_IN_DAY,
     +               LOCAL_MIN_EXTRACT_IN_DAY,
     +               CONSTANT_EXTRACT_IN_DAY,
     +               CONSTANT_HARD_EXTRACT_IN_DAY,
     +               LOCAL_CAPACITY_CURVE,
     +               LOCAL_ANNUAL_PRODUCTION_TARGET,
     +               PRODUCTION_SCARCITY,
     +               LOCAL_COAL_TRANSPORT_RATE,
     +               LOCAL_COAL_CONGESTION_COST,
     +               LOCAL_COAL_TRANSPORT_QUANT,
     +               LOCAL_COAL_TRANSPORT_LOSSES,
     +               LOCAL_COAL_SHRINKAGE_PERCENT,
     +               LOCAL_COAL_SHRINKAGE_COST,
     +               NODE_INDEX,
     +               MONTHLY_SUPPLY_BY_BASIN
         CHARACTER*8 NUMBER_STRING
         CHARACTER*256 FILE_NAME
         CHARACTER*4096 STRING
         save  GS_POS,
     +         GT_POS,
     +         NUM_LNG,
     +         LNG_SCENARIO_PRICE,
     +         LNG_INDEX,
     +         LNG_HUB_INDEX,
     +         MAX_NODES,
     +         NODE_TYPE,
     +         NODE_COUNTRY,
     +         NORTH_AMERICAN_DATABASE,
     +         MONTHLY_NODE_PRICE,
     +         MONTHLY_COAL_BALANCE,
     +         MAX_NODE_ID,
     +         MAX_LINK_ID,
     +         MAX_ACTIVE_COAL_LINKS,
     +         ANNUAL_NODE_PRICE,
     +         ANNUAL_NODE_VOLUME,
     +         ANNUAL_LINK_PRICE,
     +         ANNUAL_LINK_VOLUME
!     
! END DATA DECLARATIONS
!
!
! CALLED ONCE PER ENDPOINT.
!
         MAX_NODES = GET_NUMBER_OF_COAL_ACTIVE_GROUPS()
!
         MAX_NODE_ID = GET_MAX_COAL_GROUP_NUMBER()
         MAX_LINK_ID = GET_MAX_LINK_ID()
         MAX_ACTIVE_COAL_LINKS = GET_ACTIVE_LINK_NUMBER()
!
         IF(ALLOCATED(GS_POS)) DEALLOCATE(GS_POS,GT_POS)
         ALLOCATE(GS_POS(MAX_NODE_ID),GT_POS(MAX_LINK_ID))
         GS_POS = 0
         GT_POS = 0
!         
         DO J = 1,  MAX_NODES
            GS = GET_COAL_GROUP_INDEX(J)
            GS_POS(GS) = J
         ENDDO
         DO J = 1,  MAX_ACTIVE_COAL_LINKS
            GT = GET_COAL_LINK_ID(J)
            GT_POS(GT) = J
         ENDDO
         TEMP_I2 = 3
         NUM_LNG = GET_COAL_NODE_TYPE_COUNT(TEMP_I2)
         IF(ALLOCATED(LNG_SCENARIO_PRICE)) 
     +           DEALLOCATE(LNG_SCENARIO_PRICE,LNG_INDEX,
     +                      LNG_HUB_INDEX,
     +                      NODE_TYPE,
     +                      NODE_COUNTRY)
         ALLOCATE(   LNG_SCENARIO_PRICE(NUM_LNG,12,30),
     +               LNG_INDEX(NUM_LNG),
     +               LNG_HUB_INDEX(NUM_LNG),
     +               NODE_TYPE(MAX_NODES),
     +               NODE_COUNTRY(MAX_NODES))
         LNG_SCENARIO_PRICE = -99.0
         COAL_PRICING_ROUTINE = .TRUE.
         TEMP_I2 = 0
         DO J = 1,  MAX_NODES
            GS = GET_COAL_GROUP_INDEX(J)
            NODE_TYPE(J) = GET_COAL_NODE_TYPE_INDEX(GS)
            IF(NODE_TYPE(J) < 1) THEN
               WRITE(4,*) 'UNDEFINED NODE TYPE FOR NODE',GS
            ENDIF
            NODE_COUNTRY(J) = GET_COAL_NODE_COUNTRY_INDEX(GS)
            IF(NODE_TYPE(J) == 3) THEN
               TEMP_I2 = TEMP_I2 + 1
               LNG_INDEX(TEMP_I2) = GS
               LNG_HUB_INDEX(TEMP_I2) = GET_COAL_HUB_REFERENCE(GS)
            ENDIF
         ENDDO
!
! FOR EACH LNG, FIND THE HUB, OTHERWISE SET TO LNG
!         
         IF(TEMP_I2 > 0) THEN
            DO J = 1, TEMP_I2 
               GS = LNG_INDEX(J)
               HUB_GS = LNG_HUB_INDEX(J)
               DO I = 1,  MAX_NODES
                  IF(GET_COAL_GROUP_INDEX(I) /= HUB_GS) CYCLE
! RESETS THE INDEX                  
                  LNG_HUB_INDEX(J) = I
                  EXIT
               ENDDO ! NODES
! IF HUB INDEX NOT FOUND, RESET TO LNG_INDEX               
               IF(I > MAX_NODES) THEN
                  LNG_HUB_INDEX(J) = J
               ENDIF
            END DO ! LNG
         ENDIF
      RETURN

      ENTRY ANNUAL_LP_COAL_PRICING(R_MAX_NODES,R_MAX_ACTIVE_COAL_LINKS,
     +                            R_YEAR)

         ANNUAL_LP_COAL_PRICING = .TRUE.
         IF(ALLOCATED(ANNUAL_NODE_PRICE)) 
     +                 DEALLOCATE (ANNUAL_NODE_PRICE,
     +                             ANNUAL_NODE_VOLUME,
     +                             ANNUAL_LINK_PRICE,
     +                             ANNUAL_LINK_VOLUME)
         ALLOCATE(ANNUAL_NODE_PRICE(MAX_NODES),
     +            ANNUAL_NODE_VOLUME(MAX_NODES),
     +            ANNUAL_LINK_PRICE(MAX_ACTIVE_COAL_LINKS),
     +            ANNUAL_LINK_VOLUME(MAX_ACTIVE_COAL_LINKS))
         ANNUAL_NODE_PRICE = 0.0
         ANNUAL_NODE_VOLUME = 0.0
         ANNUAL_LINK_PRICE = 0.0
         ANNUAL_LINK_VOLUME = 0.0

         MAX_COUNTRIES = 1
         IF(ALLOCATED(MONTHLY_COAL_BALANCE)) 
     +                                  DEALLOCATE(MONTHLY_COAL_BALANCE)
         ALLOCATE(MONTHLY_COAL_BALANCE(
     +                               BALANCE_VARS,0:12,0:MAX_COUNTRIES))
         MONTHLY_COAL_BALANCE = 0.0
!
         R_MAX_NODES = MAX_NODES
         R_MAX_ACTIVE_COAL_LINKS = MAX_ACTIVE_COAL_LINKS
         TEMP_L = MONTHLY_COAL_LINK_INIT(R_YEAR)
!
      RETURN

      entry monthly_lp_COAL_pricing(R_YEAR,R_MONTH,R_HOURS_IN_MONTH)

         monthly_lp_COAL_pricing = .false.
         IF(.NOT. RUN_TRANSACT_THIS_MONTH(R_MONTH)) RETURN
         monthly_lp_COAL_pricing = .true.
         DAYS_IN_MONTH = R_HOURS_IN_MONTH/24
         FLOAT_DAYS_IN_MONTH = FLOAT(DAYS_IN_MONTH)

         MAX_SUPPLY_NODES = GET_NUM_OF_COAL_SUPPLY_REGIONS()

         IF(MAX_NODES < 1) RETURN
         LOCAL_YEAR = R_YEAR + get_BASE_YEAR()
!
! THIS NEEDS TO BE AUTOMATED. 3 = UK IN THE EUROPEAN DATABASE
         TEMP_I2 = 3 
         NORTH_AMERICAN_DATABASE = 
     +                            GET_WORLD_DATABASE_INDEX(TEMP_I2) == 0
         HH_ADJ = MONTHLY_HH_BASIS(R_YEAR,R_MONTH)
!
         if(allocated(DAILY_DEMAND)) 
     +         deallocate(DAILY_DEMAND,
     +                    DAILY_INPUT_DEMAND,
     +                    DAILY_DEMAND_INDEX,
     +                    DAILY_INTER_AVAIL,
     +                    LP_DAILY_DEMAND,
     +                    LP_NODE_COST,
     +                    LOCAL_COST_CURVE,
     +                    LOCAL_CAPACITY_CURVE,
     +                    LOCAL_ANNUAL_PRODUCTION_TARGET,
     +                    PRODUCTION_SCARCITY,
     +                    LOCAL_EXTRACT_IN_DAY,
     +                    LOCAL_HARD_EXTRACT_IN_DAY,
     +                    LOCAL_MIN_EXTRACT_IN_DAY,
     +                    CONSTANT_EXTRACT_IN_DAY,
     +                    CONSTANT_HARD_EXTRACT_IN_DAY,
     +                    DAILY_MARG_NODE_PRICE,
     +                    DAILY_AVE_NODE_PRICE,
     +                    MONTHLY_NODE_PRICE,
     +                    MONTHLY_NODE_RESERVE,
     +                    DAILY_LINK_VOLUME,
     +                    DAILY_LINK_PRICE,
     +                    DAILY_LINK_CF,
     +                    DAILY_LINK_LIMIT,
     +                    DAILY_LINK_COMMOD_COST,
     +                    DAILY_NODE_VOLUME,
     +                    DAILY_COAL_SUR_DEF_VOLUME,
     +                    MONTHLY_NODE_VOLUME,
     +                    MONTHLY_LINK_VOLUME,
     +                    MONTHLY_LINK_PRICE,
     +                    A_COEFFICIENT_STORAGE,
     +                    B_COEFFICIENT_STORAGE,
     +                    STORAGE_WITHDRAWAL,
     +                    STORAGE_INJECTION,
     +                    SUPPLY_ACTIVE)
         allocate (
     +             DAILY_DEMAND(MAX_NODES),
     +             DAILY_INPUT_DEMAND(MAX_NODES),
     +             DAILY_DEMAND_INDEX(MAX_NODES),
     +             DAILY_INTER_AVAIL(MAX_NODES),
     +             DAILY_MARG_NODE_PRICE(MAX_NODES),
     +             DAILY_AVE_NODE_PRICE(MAX_NODES),
     +             MONTHLY_NODE_PRICE(MAX_NODES),
     +             MONTHLY_NODE_RESERVE(MAX_NODES),
     +             LP_DAILY_DEMAND(MAX_NODE_ID),
     +             LP_NODE_COST(MAX_NODE_ID),
     +             LOCAL_COST_CURVE(MAX_NODE_ID),
     +             LOCAL_CAPACITY_CURVE(MAX_NODE_ID),
     +             LOCAL_ANNUAL_PRODUCTION_TARGET(MAX_NODE_ID),
     +             PRODUCTION_SCARCITY(MAX_NODE_ID),
     +             LOCAL_EXTRACT_IN_DAY(MAX_NODE_ID),
     +             LOCAL_HARD_EXTRACT_IN_DAY(MAX_NODE_ID),
     +             LOCAL_MIN_EXTRACT_IN_DAY(MAX_NODE_ID),
     +             CONSTANT_EXTRACT_IN_DAY(MAX_NODE_ID),
     +             CONSTANT_HARD_EXTRACT_IN_DAY(MAX_NODE_ID),
     +             DAILY_LINK_VOLUME(MAX_ACTIVE_COAL_LINKS),
     +             DAILY_LINK_PRICE(MAX_ACTIVE_COAL_LINKS),
     +             DAILY_LINK_CF(MAX_ACTIVE_COAL_LINKS),
     +             DAILY_LINK_LIMIT(MAX_ACTIVE_COAL_LINKS),
     +             DAILY_LINK_COMMOD_COST(MAX_ACTIVE_COAL_LINKS),
     +             DAILY_NODE_VOLUME(MAX_NODES),
     +             DAILY_COAL_SUR_DEF_VOLUME(MAX_NODES),
     +             MONTHLY_NODE_VOLUME(MAX_NODES),
     +             MONTHLY_LINK_VOLUME(MAX_ACTIVE_COAL_LINKS),
     +             MONTHLY_LINK_PRICE(MAX_ACTIVE_COAL_LINKS),
     +             A_COEFFICIENT_STORAGE(MAX_NODE_ID),
     +             B_COEFFICIENT_STORAGE(MAX_NODE_ID),
     +             STORAGE_WITHDRAWAL(MAX_NODES),
     +             STORAGE_INJECTION(MAX_NODES),
     +             SUPPLY_ACTIVE(MAX_NODES))
! ALL NODES
         YES_DAILY_COAL_SELL_BUY_REPORT = DAILY_COAL_SELL_BUY_REPORT()
         IF(DAILY_COAL_SELL_BUY_REPORT_NOT_OPEN .AND. 
     +                              YES_DAILY_COAL_SELL_BUY_REPORT) THEN
            DAILY_COAL_SELL_BUY_REPORT_NOT_OPEN = .FALSE.
            DAILY_COAL_PRICE_UNIT = 
     +                 DAILY_COAL_PRICE_HEADER(DAILY_COAL_PRICE_REC,
     +                                                        MAX_NODES)
            DAILY_COAL_NODE_VOLUME_UNIT = 
     +           DAILY_COAL_NODE_VOLUME_HEADER(
     +                                 DAILY_COAL_NODE_VOLUME_REC,
     +                                                        MAX_NODES)
            DAILY_COAL_STORAGE_UNIT = 
     +           DAILY_COAL_STORAGE_HEADER(DAILY_COAL_STORAGE_REC,
     +                                                        MAX_NODES)
            DAILY_COAL_SUR_DEF_UNIT = 
     +           DAILY_COAL_SUR_DEF_HEADER(DAILY_COAL_SUR_DEF_REC,
     +                                                        MAX_NODES)
            DAILY_COAL_AVE_PRICE_UNIT = 
     +           DAILY_COAL_AVE_PRICE_HEADER(DAILY_COAL_AVE_PRICE_REC,
     +                                                        MAX_NODES)
            DAILY_COAL_LINK_UNIT = 
     +                 DAILY_COAL_LINK_HEADER(DAILY_COAL_LINK_REC,
     +                                            MAX_ACTIVE_COAL_LINKS)
            DAILY_COAL_LINK_LIMIT_UNIT = 
     +                 DAILY_COAL_LINK_LIMIT_HEADER(
     +                                        DAILY_COAL_LINK_LIMIT_REC,
     +                                        MAX_ACTIVE_COAL_LINKS)
            DAILY_COAL_LINK_PRICE_UNIT = 
     +                 DAILY_COAL_LINK_PRICE_HEADER(
     +                                        DAILY_COAL_LINK_PRICE_REC,
     +                                        MAX_ACTIVE_COAL_LINKS)
            DAILY_COAL_LINK_CF_UNIT = 
     +                 DAILY_COAL_LINK_CF_HEADER(
     +                                         DAILY_COAL_LINK_CF_REC,
     +                                         MAX_ACTIVE_COAL_LINKS)
            MONTHLY_COAL_LINK_PRICE_UNIT = 
     +                 MONTHLY_COAL_LINK_PRICE_HEADER(
     +                                      MONTHLY_COAL_LINK_PRICE_REC,
     +                                      MAX_ACTIVE_COAL_LINKS)
            MONTHLY_COAL_PRICE_UNIT = 
     +                 MONTHLY_COAL_PRICE_HEADER(MONTHLY_COAL_PRICE_REC,
     +                                                        MAX_NODES)
            MONTHLY_COAL_BALANCE_UNIT = 
     +             MONTHLY_COAL_BALANCE_HEADER(MONTHLY_COAL_BALANCE_REC,
     +                                                     BALANCE_VARS)
            MONTHLY_COAL_BASIN_UNIT = 
     +                 MONTHLY_COAL_BASIN_HEADER(MONTHLY_COAL_BASIN_REC,
     +                                                        MAX_NODES)
            MONTHLY_COAL_LINK_UNIT = 
     +                 MONTHLY_COAL_LINK_HEADER(MONTHLY_COAL_LINK_REC,
     +                                            MAX_ACTIVE_COAL_LINKS)
            MONTHLY_COAL_RESERVE_UNIT = 
     +             MONTHLY_COAL_RESERVE_HEADER(MONTHLY_COAL_RESERVE_REC,
     +                                                        MAX_NODES)
!            
            OPEN(10,FILE="MSG"//TRIM(GET_SCENAME())//".GSP")
            DO J = 1,  MAX_NODES
               ALPHA_J = GET_NODE_ALPHA_ORDER(J)
               GS = GET_COAL_GROUP_INDEX(ALPHA_J)
               WRITE(TEMP_STR,'(I6)') ALPHA_J-1
               TEMP_STR = '"'//GET_COAL_GROUP_NAME(GS)//'"'//
     +                     ',V2,'//TRIM(TEMP_STR)//
     +                     ',,S,,,,,,,,"'//
     +                     COAL_NODE_TYPE(NODE_TYPE(ALPHA_J))//
     +                     '",'
               WRITE(10,'(A)') TRIM(TEMP_STR)
            ENDDO
! NODE ID NUMBER
            OPEN(10,FILE="MSG"//TRIM(GET_SCENAME())//".GTP")
            DO J = 1,  MAX_NODES
               ALPHA_J = GET_NODE_ALPHA_ORDER(J)
               GS = GET_COAL_GROUP_INDEX(ALPHA_J)
               WRITE(TEMP_STR,'(I6)') ALPHA_J-1
               WRITE(TEMP_STR_2,'(I6)') GS
               TEMP_STR = '"'//TRIM(TEMP_STR_2)//'"'//
     +                       ',V2,'//TRIM(TEMP_STR)//
     +                     ',,S,,,,,,,,"'//
     +                     COAL_NODE_TYPE(NODE_TYPE(ALPHA_J))//
     +                     '",'
               WRITE(10,'(A)') TRIM(TEMP_STR)
            ENDDO
! ALL LINKS
            OPEN(10,FILE="MSG"//TRIM(GET_SCENAME())//".GLP")
            DO J = 1,  MAX_ACTIVE_COAL_LINKS

               ALPHA_J = GET_LINK_ALPHA_ORDER(J)
               WRITE(TEMP_STR,'(I6)') ALPHA_J-1
               TEMP_STR = '"'//GET_COAL_LINK_NAME(ALPHA_J)//'"'//
     +                       ',V2,'//TRIM(TEMP_STR)//
     +                     ',,S,,,,,,,,"LINK"'

               WRITE(10,'(A)') TRIM(TEMP_STR)
            END DO
            CLOSE(10)
! LINK ID NUMBER            
            OPEN(10,FILE="MSG"//TRIM(GET_SCENAME())//".GMP")
            DO J = 1,  MAX_ACTIVE_COAL_LINKS
!               GS = GET_COAL_GROUP_INDEX(J)
               WRITE(TEMP_STR,'(I6)') J-1
               WRITE(TEMP_STR_2,'(I6)') GET_COAL_LINK_ID(J)
               TEMP_STR = '"'//TRIM(TEMP_STR_2)//'"'//
     +                       ',V2,'//TRIM(TEMP_STR)//
     +                     ',,S,,,,,,,,"LINK"'
               WRITE(10,'(A)') TRIM(TEMP_STR)
            END DO
            CLOSE(10)
! SUPPLY NODES
            OPEN(10,FILE="MSG"//TRIM(GET_SCENAME())//".GSS")
            DO J = 1,  MAX_NODES

               ALPHA_J = GET_NODE_ALPHA_ORDER(J)
               GS = GET_COAL_GROUP_INDEX(ALPHA_J)
               IF(.NOT. COAL_SUPPLY_GROUP_ACTIVE(GS)) CYCLE
               WRITE(TEMP_STR,'(I6)') ALPHA_J-1
               TEMP_STR = '"'//GET_COAL_GROUP_NAME(GS)//'"'//
     +                       ',V2,'//TRIM(TEMP_STR)//
     +                     ',,S,,,,,,,,"'//
     +                     COAL_NODE_TYPE(NODE_TYPE(ALPHA_J))//
     +                     '",'
               WRITE(10,'(A)') TRIM(TEMP_STR)
            END DO
            CLOSE(10)
! DEMAND NODES
            OPEN(10,FILE="MSG"//TRIM(GET_SCENAME())//".GST")
            DO J = 1,  MAX_NODES
               ALPHA_J = GET_NODE_ALPHA_ORDER(J)
               GS = GET_COAL_GROUP_INDEX(ALPHA_J)
               IF(.NOT. COAL_DEMAND_GROUP_ACTIVE(GS)) CYCLE
               WRITE(TEMP_STR,'(I6)') ALPHA_J-1
               TEMP_STR = '"'//GET_COAL_GROUP_NAME(GS)//'"'//
     +                       ',V2,'//TRIM(TEMP_STR)//
     +                     ',,S,,,,,,,,"'//
     +                     COAL_NODE_TYPE(NODE_TYPE(ALPHA_J))//
!     +                     COAL_NODE_TYPE(GET_COAL_NODE_TYPE_INDEX(GS))//
     +                     '",'
               WRITE(10,'(A)') TRIM(TEMP_STR)
            END DO
            CLOSE(10)
! PRICE NODES
            OPEN(10,FILE="MSG"//TRIM(GET_SCENAME())//".GQP")
            DO J = 1,  MAX_NODES
               ALPHA_J = GET_NODE_ALPHA_ORDER(J)
               GS = GET_COAL_GROUP_INDEX(ALPHA_J)
               IF(GET_COAL_NODE_TYPE_INDEX(GS) /= 5) CYCLE
               WRITE(TEMP_STR,'(I6)') ALPHA_J-1
               TEMP_STR = '"'//GET_COAL_GROUP_NAME(GS)//'"'//
     +                       ',V2,'//TRIM(TEMP_STR)//
     +                     ',,S,,,,,,,,"'//
     +                     COAL_NODE_TYPE(NODE_TYPE(ALPHA_J))//
!     +                     COAL_NODE_TYPE(GET_COAL_NODE_TYPE_INDEX(GS))//
     +                     '",'
               WRITE(10,'(A)') TRIM(TEMP_STR)
            END DO
            CLOSE(10)
!
         ENDIF
!         NODE_TYPE = 0
         SUPPLY_ACTIVE = .TRUE.
         LOCAL_COST_CURVE = 0
         LOCAL_CAPACITY_CURVE = 0
         LOCAL_ANNUAL_PRODUCTION_TARGET = 0.0
         LOCAL_EXTRACT_IN_DAY = 9999999.
         LOCAL_HARD_EXTRACT_IN_DAY = 9999999.
         LOCAL_MIN_EXTRACT_IN_DAY = 0.0
         CONSTANT_EXTRACT_IN_DAY = 9999999.
         CONSTANT_HARD_EXTRACT_IN_DAY = 9999999.
         HH_NODE_NUM = 0
! 012608. CONSTANT FOR MONTH.
         DAILY_LINK_LIMIT = 0.
         DAILY_LINK_COMMOD_COST = 0.
         DO J = 1, MAX_ACTIVE_COAL_LINKS
            DAILY_LINK_LIMIT(J) = GET_COAL_LINK_QUANT(J,R_YEAR,R_MONTH)
            DAILY_LINK_COMMOD_COST(J) = 
     +                        GET_COAL_TRANSPORT_RATE(J,R_YEAR,R_MONTH)
         ENDDO
         DO L_I = 1, MAX_NODES 
            GS = GET_COAL_GROUP_INDEX(L_I)
            TEMP_L = GET_SUPPLY_COST_CAPACITY_CURVES(
     +                                    GS,
     +                                    MAX_S,
     +                                    COST_CURVE,
     +                                    CAPACITY_CURVE,
     +                                    MIN_EXTRACT_RATE,
     +                                    EXTRACT_CURVE,
     +                                    HARD_EXTRACT_RATE,
     +                                    SHRINKAGE_COST,
     +                                    SHRINKAGE_PERCENT,
     +                                    FIRST_S,
     +                                    R_YEAR,
     +                                    R_MONTH,
     +                                    MIN_BASIN_COST,
     +                                    ANNUAL_PRODUCTION_TARGET)

            IF(.NOT. TEMP_L) THEN
               SUPPLY_ACTIVE(L_I) = .FALSE.
               CYCLE
            ENDIF
!             
            LOCAL_COST_CURVE(GS) = MAX(0.0,COST_CURVE + SHRINKAGE_COST)
!            IF(.NOT. NORTH_AMERICAN_DATABASE .AND. GS == 117) THEN ! UK Supply
            IF(.NOT. NORTH_AMERICAN_DATABASE .AND. GS == 112) THEN ! Neth Supply
               HH_SUPPLY_COST = LOCAL_COST_CURVE(GS)
            ELSEIF(NORTH_AMERICAN_DATABASE .AND. GS == 121) THEN
               HH_SUPPLY_COST = LOCAL_COST_CURVE(GS)
            ENDIF
            HH_NODE_NUM = L_I
! 112007. CHANGED MINIMUM CAP TO GET APPROXIMATELY ZERO RESERVES
            LOCAL_CAPACITY_CURVE(GS) = MAX(0.001,CAPACITY_CURVE)
            LOCAL_ANNUAL_PRODUCTION_TARGET(GS) = 
     +                                          ANNUAL_PRODUCTION_TARGET
            LOCAL_EXTRACT_IN_DAY(GS) = EXTRACT_CURVE
            LOCAL_HARD_EXTRACT_IN_DAY(GS) = HARD_EXTRACT_RATE
            LOCAL_MIN_EXTRACT_IN_DAY(GS) = MAX(0.0,
     +                                       .99999*MIN_EXTRACT_RATE)
            CONSTANT_EXTRACT_IN_DAY(GS) = EXTRACT_CURVE
            CONSTANT_HARD_EXTRACT_IN_DAY(GS) = HARD_EXTRACT_RATE
!            NODE_TYPE(L_I) = 1
!
         END DO ! MAX_NODES
         DO L_I = 1, MAX_NODES 
            IF(.NOT. SUPPLY_ACTIVE(L_I)) CYCLE
            GS = GET_COAL_GROUP_INDEX(L_I)
!
! 112707. ADDED COEFFICIENT FOR EUROPE.
!            

               LOCAL_COST_CURVE(GS) = MAX(0.0,LOCAL_COST_CURVE(GS) +
     +                                           HH_ADJ-HH_SUPPLY_COST )
            ! TODO: Remove code in IF block
            IF(1 == 2 .AND. 
     +                  LOCAL_ANNUAL_PRODUCTION_TARGET(GS) > 0.001) THEN
               TEST_DEPLETION = 1. - LOCAL_CAPACITY_CURVE(GS)/
     +                                LOCAL_ANNUAL_PRODUCTION_TARGET(GS)
               IF(TEST_DEPLETION > TEST_ANN_PROD_PERCENT(1)) THEN
                  DO I = 2, 10
                     IF(TEST_DEPLETION > 
     +                                  TEST_ANN_PROD_PERCENT(I)) CYCLE
                     EXIT
                  END DO
                  IF(I < 11) THEN ! FOR MONOTONIC INCREASING PERCENT 
                     TEMP_R = TEST_ANN_PROD_SCARCITY(I-1) +
     +                  (TEST_DEPLETION - TEST_ANN_PROD_PERCENT(I-1))
     +                        * (TEST_ANN_PROD_SCARCITY(I) - 
     +                                     TEST_ANN_PROD_SCARCITY(I-1))/
     +                                  (TEST_ANN_PROD_PERCENT(I) -
     +                                     TEST_ANN_PROD_PERCENT(I-1))
!     
                     LOCAL_COST_CURVE(GS) = LOCAL_COST_CURVE(GS) +
     +                                                            TEMP_R
                  ENDIF
               ENDIF
            ENDIF
!
         ENDDO
!
         CALL GregMonthCall(  END_POINT,
     +                        R_YEAR, ! 041909
     +                        R_MONTH,
     +                        LOCAL_COST_CURVE,
     +                        DAILY_LINK_LIMIT,
     +                        DAILY_LINK_COMMOD_COST)

!
         MONTHLY_LINK_PRICE = 0.
         MONTHLY_LINK_VOLUME = 0.     
         MONTHLY_NODE_PRICE = 0.
         MONTHLY_NODE_RESERVE = 0.
         MONTHLY_NODE_VOLUME = 0.
         A_COEFFICIENT_STORAGE = 0.0
         B_COEFFICIENT_STORAGE = 0.0

!
         DO DA = 1, DAYS_IN_MONTH
!
            DAILY_DEMAND_INDEX = 0
            DAILY_DEMAND = 0.
            DAILY_INPUT_DEMAND = 0.
            LP_DAILY_DEMAND = 0.
            DAILY_MARG_NODE_PRICE = 0.
            DAILY_AVE_NODE_PRICE = 0.
            DAILY_LINK_VOLUME = 0.
            DAILY_LINK_PRICE = 0.
            DAILY_LINK_CF = 0.
            DAILY_NODE_VOLUME = 0.
            DAILY_COAL_SUR_DEF_VOLUME = 0.
            STORAGE_WITHDRAWAL = 0.0
            STORAGE_INJECTION = 0.0
            L_I = 1
! NEED TO ESTABLISH THE REGIONS AND THE REGIONAL DEMANDS
! THEN CALCULATE REGIONAL STORAGE.
! COUNT STORAGE AREAS
! ASSIGN DEMAND NODES TO STORAGE AREAS
! ACCUMULATE DAILY DEMAND TO STORAGE AREAS
! SAVE STORAGE EQUATIONS TO STORAGE AREAS
! CREATE MINIMUM AND MAXIMUM STORAGE CAPACITY
! 
            STORAGE_ACTIVE = PROCESS_DAILY_STORAGE(DA,R_MONTH,R_YEAR)
!            
            DO L_K = 1, MAX_NODES
               GS = GET_COAL_GROUP_INDEX(L_K)
               DAILY_INPUT_DEMAND(L_K) =
     +                       GET_DAILY_COAL_DEMAND_BY_NODE(DA,GS,TEMP_R)
               DAILY_DEMAND(L_K) = DAILY_INPUT_DEMAND(L_K)
!
               DAILY_INTER_AVAIL(L_K) = TEMP_R
               IF(DAILY_DEMAND(L_K) > 0.01) THEN

                  IF(STORAGE_ACTIVE) THEN
                     TEMP_STORAGE = GET_NODE_DAILY_STORAGE(GS)
                  ELSE
                     TEMP_STORAGE = 0.0
                  ENDIF
                  IF(ABS(TEMP_STORAGE) > .01) THEN
                     IF(TEMP_STORAGE > 0.0) THEN ! WITHDRAWAL
                        STORAGE_WITHDRAWAL(L_K) = 
     +                            STORAGE_WITHDRAWAL(L_K) + TEMP_STORAGE
                     ELSE ! INJECTION
                        STORAGE_INJECTION(L_K) = 
     +                             STORAGE_INJECTION(L_K) + TEMP_STORAGE
                     ENDIF
                     DAILY_DEMAND(L_K) = 
     +                                  DAILY_DEMAND(L_K) - TEMP_STORAGE
                  ENDIF
!                  
                  DAILY_DEMAND_INDEX(L_I) = L_K
! THE STORAGE EQUATION WAS GENERATING NEGATIVE DEMAND.                  
                  LP_DAILY_DEMAND(GS) = MAX(0.00,DAILY_DEMAND(L_K))
                  L_I = L_I + 1
               ENDIF ! DEMAND > 0

            END DO ! NODE
            CALL GregDayCall(END_POINT,LOCAL_YEAR,R_MONTH,
     +                       DA,
     +                       GS_POS,
     +                       GT_POS,
     +                       LOCAL_CAPACITY_CURVE,
     +                       LOCAL_MIN_EXTRACT_IN_DAY,
     +                       LOCAL_EXTRACT_IN_DAY,
     +                       LOCAL_HARD_EXTRACT_IN_DAY,
     +                       LP_DAILY_DEMAND,
     +                       DAILY_MARG_NODE_PRICE,
     +                       DAILY_AVE_NODE_PRICE,
     +                       DAILY_LINK_VOLUME,
     +                       DAILY_NODE_VOLUME,
     +                       DAILY_COAL_SUR_DEF_VOLUME,
     +                       DAILY_LINK_PRICE)
!     +                       TEMP_R)                  
!
! 122307. TAKE STORAGE OUT. ADD BACK INTO COAL DEMAND.
!
            DO L_K = 1, MAX_NODES
!

               IF(HH_NODE_NUM > 0 .AND. 
     +               NODE_TYPE(L_K) == 1 .AND. 
     +                  DAILY_NODE_VOLUME(L_K) > 0.10 .AND.
     +                        DAILY_COAL_SUR_DEF_VOLUME(L_K) > 0.01) THEN ! DEMAND
                  TEMP_R = DAILY_COAL_SUR_DEF_VOLUME(L_K)
                  DAILY_MARG_NODE_PRICE(L_K) = 
     +               (DAILY_MARG_NODE_PRICE(L_K)* 
     +                  MAX(0.1,DAILY_NODE_VOLUME(L_K)-TEMP_R) +
     +                     TEMP_R * 
     +                       DAILY_MARG_NODE_PRICE(HH_NODE_NUM) * 1.25)/
     +                              DAILY_NODE_VOLUME(L_K)
               ENDIF
!
               IF(NODE_TYPE(L_K) == 1) THEN ! DEMAND
                  IF(STORAGE_WITHDRAWAL(L_K) > 0.001) THEN
                     MONTHLY_COAL_BALANCE(3,R_MONTH,1) = 
     +                     MONTHLY_COAL_BALANCE(3,R_MONTH,1) +
     +                                        STORAGE_WITHDRAWAL(L_K)
                     MONTHLY_COAL_BALANCE(5,R_MONTH,1) = 
     +                     MONTHLY_COAL_BALANCE(5,R_MONTH,1) +
     +                                        STORAGE_WITHDRAWAL(L_K)
                  ENDIF
                  IF(STORAGE_INJECTION(L_K) < -0.001) THEN
                     MONTHLY_COAL_BALANCE(11,R_MONTH,1) = 
     +                     MONTHLY_COAL_BALANCE(11,R_MONTH,1) -
     +                                        STORAGE_INJECTION(L_K)
                     MONTHLY_COAL_BALANCE(13,R_MONTH,1) = 
     +                     MONTHLY_COAL_BALANCE(13,R_MONTH,1) -
     +                                        STORAGE_INJECTION(L_K)
                  ENDIF
                  MONTHLY_COAL_BALANCE(6,R_MONTH,1) = 
     +                     MONTHLY_COAL_BALANCE(6,R_MONTH,1) +
!     +                                        DAILY_NODE_VOLUME(L_K)
     +                                        DAILY_INPUT_DEMAND(L_K)
                  MONTHLY_COAL_BALANCE(13,R_MONTH,1) = 
     +                     MONTHLY_COAL_BALANCE(13,R_MONTH,1) +
!     +                                        DAILY_NODE_VOLUME(L_K)
     +                                        DAILY_INPUT_DEMAND(L_K)
! FUEL USE
                  MONTHLY_COAL_BALANCE(12,R_MONTH,1) = 
     +                     MONTHLY_COAL_BALANCE(12,R_MONTH,1) -
     +                                        DAILY_NODE_VOLUME(L_K)
                  MONTHLY_COAL_BALANCE(13,R_MONTH,1) = 
     +                     MONTHLY_COAL_BALANCE(13,R_MONTH,1) -
     +                                        DAILY_NODE_VOLUME(L_K)
               ELSEIF(NODE_TYPE(L_K) == 2) THEN ! BASIN
                  MONTHLY_COAL_BALANCE(1,R_MONTH,1) = 
     +                     MONTHLY_COAL_BALANCE(1,R_MONTH,1) +
     +                                        DAILY_NODE_VOLUME(L_K)
                  MONTHLY_COAL_BALANCE(5,R_MONTH,1) = 
     +                     MONTHLY_COAL_BALANCE(5,R_MONTH,1) +
     +                                        DAILY_NODE_VOLUME(L_K)
! FUEL USE
                  MONTHLY_COAL_BALANCE(12,R_MONTH,1) = 
     +                     MONTHLY_COAL_BALANCE(12,R_MONTH,1) +
     +                                        DAILY_NODE_VOLUME(L_K)
                  MONTHLY_COAL_BALANCE(13,R_MONTH,1) = 
     +                     MONTHLY_COAL_BALANCE(13,R_MONTH,1) +
     +                                        DAILY_NODE_VOLUME(L_K)
               ELSEIF(NODE_TYPE(L_K) == 3) THEN ! LNG
                  MONTHLY_COAL_BALANCE(4,R_MONTH,1) = 
     +                     MONTHLY_COAL_BALANCE(4,R_MONTH,1) +
     +                                        DAILY_NODE_VOLUME(L_K)
                  MONTHLY_COAL_BALANCE(5,R_MONTH,1) = 
     +                     MONTHLY_COAL_BALANCE(5,R_MONTH,1) +
     +                                        DAILY_NODE_VOLUME(L_K)
! FUEL USE
                  MONTHLY_COAL_BALANCE(12,R_MONTH,1) = 
     +                     MONTHLY_COAL_BALANCE(12,R_MONTH,1) +
     +                                        DAILY_NODE_VOLUME(L_K)
                  MONTHLY_COAL_BALANCE(13,R_MONTH,1) = 
     +                     MONTHLY_COAL_BALANCE(13,R_MONTH,1) +
     +                                        DAILY_NODE_VOLUME(L_K)
               ENDIF               
!            
               GS = GET_COAL_GROUP_INDEX(L_K)
               IF(.NOT. COAL_DEMAND_GROUP_ACTIVE(GS)) CYCLE
               TEMP_STORAGE = STORAGE_WITHDRAWAL(L_K) +
     +                                            STORAGE_INJECTION(L_K)

            ENDDO
            DO L_K = 1, MAX_ACTIVE_COAL_LINKS
               IF(DAILY_LINK_VOLUME(L_K) < 0.001 .OR. 
     +                              DAILY_LINK_LIMIT(L_K) < 0.001) CYCLE
               DAILY_LINK_CF(L_K) = 100.0*DAILY_LINK_VOLUME(L_K)/
     +                                             DAILY_LINK_LIMIT(L_K)
            ENDDO
            IF(YES_DAILY_COAL_SELL_BUY_REPORT) THEN
               WRITE(DAILY_COAL_PRICE_UNIT,REC=DAILY_COAL_PRICE_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(LOCAL_YEAR),
     +               CL_MONTH_NAME(R_MONTH),
     +               FLOAT(DA),
     +               (DAILY_MARG_NODE_PRICE(L_J),L_J=1, MAX_NODES)
               DAILY_COAL_PRICE_REC = DAILY_COAL_PRICE_REC + 1
               WRITE(DAILY_COAL_AVE_PRICE_UNIT,REC=
     +                                         DAILY_COAL_AVE_PRICE_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(LOCAL_YEAR),
     +               CL_MONTH_NAME(R_MONTH),
     +               FLOAT(DA),
     +               (DAILY_AVE_NODE_PRICE(L_J),L_J=1, MAX_NODES)
               DAILY_COAL_AVE_PRICE_REC = DAILY_COAL_AVE_PRICE_REC + 1
               WRITE(DAILY_COAL_NODE_VOLUME_UNIT,
     +                                   REC=DAILY_COAL_NODE_VOLUME_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(LOCAL_YEAR),
     +               CL_MONTH_NAME(R_MONTH),
     +               FLOAT(DA),
     +               (DAILY_NODE_VOLUME(L_J),L_J=1, MAX_NODES)
               DAILY_COAL_NODE_VOLUME_REC = 
     +                                 DAILY_COAL_NODE_VOLUME_REC + 1
               WRITE(DAILY_COAL_STORAGE_UNIT,
     +                                    REC=DAILY_COAL_STORAGE_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(LOCAL_YEAR),
     +               CL_MONTH_NAME(R_MONTH),
     +               FLOAT(DA),
     +               (-STORAGE_WITHDRAWAL(L_J)-STORAGE_INJECTION(L_J),
     +                                                 L_J=1, MAX_NODES)
               DAILY_COAL_STORAGE_REC = DAILY_COAL_STORAGE_REC + 1
!               
               WRITE(DAILY_COAL_SUR_DEF_UNIT,REC=DAILY_COAL_SUR_DEF_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(LOCAL_YEAR),
     +               CL_MONTH_NAME(R_MONTH),
     +               FLOAT(DA),
     +               (DAILY_COAL_SUR_DEF_VOLUME(L_J),L_J=1, MAX_NODES)
               DAILY_COAL_SUR_DEF_REC = DAILY_COAL_SUR_DEF_REC + 1
!
               WRITE(DAILY_COAL_LINK_UNIT,REC=DAILY_COAL_LINK_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(LOCAL_YEAR),
     +               CL_MONTH_NAME(R_MONTH),
     +               FLOAT(DA),
     +               (DAILY_LINK_VOLUME(L_J),L_J=1, 
     +                                            MAX_ACTIVE_COAL_LINKS)
               DAILY_COAL_LINK_REC = DAILY_COAL_LINK_REC + 1
!
               WRITE(DAILY_COAL_LINK_LIMIT_UNIT,
     +                                    REC=DAILY_COAL_LINK_LIMIT_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(LOCAL_YEAR),
     +               CL_MONTH_NAME(R_MONTH),
     +               FLOAT(DA),
     +               (DAILY_LINK_LIMIT(L_J),L_J=1, 
     +                                          MAX_ACTIVE_COAL_LINKS)
               DAILY_COAL_LINK_LIMIT_REC = DAILY_COAL_LINK_LIMIT_REC + 1
!
               WRITE(DAILY_COAL_LINK_PRICE_UNIT,
     +                                    REC=DAILY_COAL_LINK_PRICE_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(LOCAL_YEAR),
     +               CL_MONTH_NAME(R_MONTH),
     +               FLOAT(DA),
     +               (DAILY_LINK_PRICE(L_J),L_J=1, 
     +                                            MAX_ACTIVE_COAL_LINKS)
               DAILY_COAL_LINK_PRICE_REC = DAILY_COAL_LINK_PRICE_REC + 1
               WRITE(DAILY_COAL_LINK_CF_UNIT,
     +                                     REC=DAILY_COAL_LINK_CF_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(LOCAL_YEAR),
     +               CL_MONTH_NAME(R_MONTH),
     +               FLOAT(DA),
     +               (DAILY_LINK_CF(L_J),L_J=1, MAX_ACTIVE_COAL_LINKS)
               DAILY_COAL_LINK_CF_REC = DAILY_COAL_LINK_CF_REC + 1
            ENDIF ! DAILY REPORTING
            DO L_J = 1, MAX_NODES
!               IF(NODE_TYPE(L_J) ==1) THEN
               IF(SUPPLY_ACTIVE(L_J)) THEN
                  GS = GET_COAL_GROUP_INDEX(L_J)
                  LOCAL_CAPACITY_CURVE(GS) = 
     +                      MAX(0.000001,LOCAL_CAPACITY_CURVE(GS) -
     +                                           DAILY_NODE_VOLUME(L_J))
                  IF(NODE_TYPE(L_J) == 3) THEN
                     MONTHLY_NODE_RESERVE(L_J) = 0.0 ! BY ASSUMPTION
                  ELSE
                     MONTHLY_NODE_RESERVE(L_J) = 
     +                                          LOCAL_CAPACITY_CURVE(GS)
                  ENDIF
!
! 121807. BIG CHANGE.
!               
                  LOCAL_EXTRACT_IN_DAY(GS) = 
     +                     MAX(CONSTANT_EXTRACT_IN_DAY(GS)*.000001,
     +                           MIN(LOCAL_EXTRACT_IN_DAY(GS),
     +                                        LOCAL_CAPACITY_CURVE(GS)))
                  LOCAL_HARD_EXTRACT_IN_DAY(GS) = 
     +                MAX(CONSTANT_HARD_EXTRACT_IN_DAY(GS)*.000001,
     +                           MIN(LOCAL_HARD_EXTRACT_IN_DAY(GS),
     +                                        LOCAL_CAPACITY_CURVE(GS)))
! 012108.
                  LOCAL_MIN_EXTRACT_IN_DAY(GS) =
     +                     MAX(0.0,MIN(LOCAL_MIN_EXTRACT_IN_DAY(GS),
     +                                        LOCAL_CAPACITY_CURVE(GS)))
               ENDIF
!               
               MONTHLY_NODE_PRICE(L_J) =
     +                        MONTHLY_NODE_PRICE(L_J) + 
     +                                 DAILY_MARG_NODE_PRICE(L_J) *
     +                                            DAILY_NODE_VOLUME(L_J)
               MONTHLY_NODE_VOLUME(L_J) = 
     +                        MONTHLY_NODE_VOLUME(L_J) + 
     +                                            DAILY_NODE_VOLUME(L_J)
            ENDDO
            DO L_J = 1, MAX_ACTIVE_COAL_LINKS
               MONTHLY_LINK_PRICE(L_J) = MONTHLY_LINK_PRICE(L_J) +
     +                        DAILY_LINK_PRICE(L_J)*
     +                                     DAILY_LINK_VOLUME(L_J)
               MONTHLY_LINK_VOLUME(L_J) = 
     +                           MONTHLY_LINK_VOLUME(L_J) +
     +                                            DAILY_LINK_VOLUME(L_J)
            END DO
         ENDDO ! DA
         DO L_J = 1, MAX_NODES
               MONTHLY_NODE_PRICE(L_J) =
     +               MONTHLY_NODE_PRICE(L_J)/
     +                         MAX(0.01,MONTHLY_NODE_VOLUME(L_J))
               ANNUAL_NODE_PRICE(L_J) =
     +               ANNUAL_NODE_PRICE(L_J) +
     +                    MONTHLY_NODE_PRICE(L_J) *
     +                        MONTHLY_NODE_VOLUME(L_J)
               ANNUAL_NODE_VOLUME(L_J) =
     +               ANNUAL_NODE_VOLUME(L_J) +
     +                           MONTHLY_NODE_VOLUME(L_J)
         ENDDO
         DO L_J = 1, MAX_ACTIVE_COAL_LINKS
            MONTHLY_LINK_PRICE(L_J) =
     +               MONTHLY_LINK_PRICE(L_J)/
     +                                MAX(0.01,MONTHLY_LINK_VOLUME(L_J))
            ANNUAL_LINK_PRICE(L_J) = 
     +            ANNUAL_LINK_PRICE(L_J) +
     +                  MONTHLY_LINK_PRICE(L_J) *
     +                        MONTHLY_LINK_VOLUME(L_J)
            ANNUAL_LINK_VOLUME(L_J) = ANNUAL_LINK_VOLUME(L_J) +
     +                                          MONTHLY_LINK_VOLUME(L_J)
         ENDDO
         IF(YES_DAILY_COAL_SELL_BUY_REPORT) THEN
!
            CO = 1
            WRITE(MONTHLY_COAL_BALANCE_UNIT,REC=
     +                                         MONTHLY_COAL_BALANCE_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(LOCAL_YEAR),
     +               CL_MONTH_NAME(R_MONTH),
     +               'Total ',
     +               (MONTHLY_COAL_BALANCE(L_J,R_MONTH,CO),
     +                                              L_J=1, BALANCE_VARS)
            MONTHLY_COAL_BALANCE_REC = MONTHLY_COAL_BALANCE_REC + 1
!            
            WRITE(MONTHLY_COAL_PRICE_UNIT,REC=MONTHLY_COAL_PRICE_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(LOCAL_YEAR),
     +               CL_MONTH_NAME(R_MONTH),
     +               (MONTHLY_NODE_PRICE(L_J),L_J=1, MAX_NODES)
            MONTHLY_COAL_PRICE_REC = MONTHLY_COAL_PRICE_REC + 1
            WRITE(MONTHLY_COAL_BASIN_UNIT,REC=MONTHLY_COAL_BASIN_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(LOCAL_YEAR),
     +               CL_MONTH_NAME(R_MONTH),
     +               (MONTHLY_NODE_VOLUME(L_J),L_J=1, MAX_NODES)
            MONTHLY_COAL_BASIN_REC = MONTHLY_COAL_BASIN_REC + 1         
            WRITE(MONTHLY_COAL_RESERVE_UNIT,REC=
     +                                       MONTHLY_COAL_RESERVE_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(LOCAL_YEAR),
     +               CL_MONTH_NAME(R_MONTH),
     +               (MONTHLY_NODE_RESERVE(L_J),L_J=1, MAX_NODES)
            MONTHLY_COAL_RESERVE_REC = MONTHLY_COAL_RESERVE_REC + 1         
            WRITE(MONTHLY_COAL_LINK_UNIT,REC=MONTHLY_COAL_LINK_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(LOCAL_YEAR),
     +               CL_MONTH_NAME(R_MONTH),
     +               (MONTHLY_LINK_VOLUME(L_J),
     +                                     L_J=1, MAX_ACTIVE_COAL_LINKS)
            MONTHLY_COAL_LINK_REC = MONTHLY_COAL_LINK_REC + 1         
            WRITE(MONTHLY_COAL_LINK_PRICE_UNIT,
     +                                  REC=MONTHLY_COAL_LINK_PRICE_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(LOCAL_YEAR),
     +               CL_MONTH_NAME(R_MONTH),
     +               (MONTHLY_LINK_PRICE(L_J),
     +                                     L_J=1, MAX_ACTIVE_COAL_LINKS)
            MONTHLY_COAL_LINK_PRICE_REC = 
     +                                 MONTHLY_COAL_LINK_PRICE_REC + 1
            IF(R_MONTH == 12) THEN 
               DO L_J = 1, MAX_NODES
                  ANNUAL_NODE_PRICE(L_J) =
     +                  ANNUAL_NODE_PRICE(L_J)/
     +                         MAX(0.01,ANNUAL_NODE_VOLUME(L_J))
               ENDDO
!            
               WRITE(MONTHLY_COAL_PRICE_UNIT,REC=MONTHLY_COAL_PRICE_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(LOCAL_YEAR),
     +               CL_MONTH_NAME(13),
     +               (ANNUAL_NODE_PRICE(L_J),L_J=1, MAX_NODES)
               MONTHLY_COAL_PRICE_REC = MONTHLY_COAL_PRICE_REC + 1
               WRITE(MONTHLY_COAL_BASIN_UNIT,REC=MONTHLY_COAL_BASIN_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(LOCAL_YEAR),
     +               CL_MONTH_NAME(13),
     +               (ANNUAL_NODE_VOLUME(L_J),L_J=1, MAX_NODES)
               MONTHLY_COAL_BASIN_REC = MONTHLY_COAL_BASIN_REC + 1         
               WRITE(MONTHLY_COAL_LINK_UNIT,REC=MONTHLY_COAL_LINK_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(LOCAL_YEAR),
     +               CL_MONTH_NAME(13),
     +               (ANNUAL_LINK_VOLUME(L_J),
     +                                     L_J=1, MAX_ACTIVE_COAL_LINKS)
               MONTHLY_COAL_LINK_REC = MONTHLY_COAL_LINK_REC + 1         
               WRITE(MONTHLY_COAL_LINK_PRICE_UNIT,
     +                                  REC=MONTHLY_COAL_LINK_PRICE_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(LOCAL_YEAR),
     +               CL_MONTH_NAME(13),
     +               (ANNUAL_LINK_PRICE(L_J)/
     +                        MAX(0.01,ANNUAL_LINK_VOLUME(L_J)),
     +                                     L_J=1, MAX_ACTIVE_COAL_LINKS)
               MONTHLY_COAL_LINK_PRICE_REC = 
     +                                   MONTHLY_COAL_LINK_PRICE_REC + 1
            ENDIF ! R_MONTH == 12
         ENDIF
         TEMP_I2 = 0
         DO L_J = 1, MAX_NODES
            TEMP_R = MAX(0.,MONTHLY_NODE_VOLUME(L_J))
            IF(NODE_TYPE(L_J) == 3 .AND. TEMP_I2 < NUM_LNG) THEN
               TEMP_I2 = TEMP_I2 + 1
               HUB_GS = LNG_HUB_INDEX(TEMP_I2)
               LNG_SCENARIO_PRICE(TEMP_I2,R_MONTH,R_YEAR) = 
     +                                        MONTHLY_NODE_PRICE(HUB_GS)
! 112007. RESET TO ZERO INVENTORY BY ASSUMPTION     
               TEMP_R = 0.001
            ENDIF
            IF(.NOT. SUPPLY_ACTIVE(L_J)) CYCLE
            GS = GET_COAL_GROUP_INDEX(L_J)
            temp_l = put_monthly_lp_remaining_COAL( GS,
     +                                             TEMP_R,
     +                                             R_YEAR)
         END DO

      return

      ENTRY WRITE_LNG_TO_DAT()

         IF(.NOT. ALLOCATED(LNG_SCENARIO_PRICE)) RETURN
         IF(ITERATION_ON()) THEN
            TEMP_I2 = GET_ITERATION_NUMBER()
            IF(TEMP_I2 < 1000 .AND. TEMP_I2 > 0) THEN
               IF(TEMP_I2 < 10) THEN
                  WRITE(STRING,'(I1)') TEMP_I2
                  STRING = '00'//STRING
               ELSEIF(TEMP_I2 < 100) THEN
                  WRITE(STRING,'(I2)') TEMP_I2
                  STRING = '0'//STRING
               ELSE
                  WRITE(STRING,'(I3)') TEMP_I2
               ENDIF
            ELSE
               WRITE(STRING,'(A)') '000'
            ENDIF
         ELSE
            WRITE(STRING,'(A)') '000'
         ENDIF
         STRING = TRIM(STRING)
         IF(NORTH_AMERICAN_DATABASE) THEN
            FILE_NAME = 'Q1BZZ'
         ELSE
            FILE_NAME = 'Q2BZZ'
         ENDIF
         FILE_NAME = TRIM(FILE_NAME)//TRIM(STRING)//'.DAT'
         OPEN(FILE=FILE_NAME,UNIT=10) 
         WRITE(10,'(A)') '9,'
         DO L_J = 1, NUM_LNG
            GS = LNG_INDEX(L_J)     
            WRITE(STRING,'(I3)') GS
            WRITE(STRING,'(A)') 
     +               '1,"'//TRIM(GET_COAL_GROUP_NAME(GS))//'",'//
     +                                                      TRIM(STRING)
            DO I = 1, 30
               DO J = 1, 12
                  IF(I == 1 .AND. LNG_SCENARIO_PRICE(L_J,J,I) < 
     +                                                        -98.) THEN
                     LNG_SCENARIO_PRICE(L_J,J,I) = 0.0
                  ELSEIF(I > 1 .AND. LNG_SCENARIO_PRICE(L_J,J,I) < 
     +                                                        -98.) THEN
                     LNG_SCENARIO_PRICE(L_J,J,I) = 
     +                                     LNG_SCENARIO_PRICE(L_J,J,I-1)
                  ENDIF
                  WRITE(NUMBER_STRING,'(F8.3)') 
     +                                       LNG_SCENARIO_PRICE(L_J,J,I)
                  WRITE(STRING,'(A,A,A)') 
     +                              TRIM(STRING),',',TRIM(NUMBER_STRING)
               ENDDO
            END DO
            WRITE(10,'(A)') STRING
         ENDDO
         CLOSE(10)
         WRITE_LNG_TO_DAT = .TRUE. 
      RETURN

!
      ENTRY MONTHLY_COAL_PRICING(R_YEAR,R_MONTH,R_HOURS_IN_MONTH)
!      

! 050807.
         IF(.NOT. RUN_TRANSACT_THIS_MONTH(R_MONTH)) RETURN
!
         COAL_BASIS_POS = GET_COAL_GROUP_POSITION(COAL_BASIS_ID) ! 317 UNTIL DEFINED IN PROD PARAM
!
         IF(COAL_BASIS_POS == 0) THEN
            COAL_BASIS_POS = 1
         ENDIF
!
         MONTHLY_COAL_PRICING = .FALSE.
!
         MAX_NODES = GET_NUMBER_OF_COAL_ACTIVE_GROUPS() ! GET_MAX_COAL_GROUP_NUMBER()
!
         IF(MAX_NODES < 1) RETURN
!         
         DAYS_IN_MONTH = R_HOURS_IN_MONTH/24
         TOTAL_PATHS = GET_TOTAL_PATHS_FOR_SYSTEM()
!         MAX_POINTS_IN_GS = GET_MAX_POINTS_IN_GS()
!
!         CALL GET_MAX_COAL_LINKS(MAX_IN_COAL_LINKS) 
! 101106 !!!         
! MAX_POINTS_IN_GS MUST BE PROPOGATED THROUGHOUT THIS PROGRAM         
! 032907. RECALC ON PATHS_N_CURVES
!
         PATHS_N_CURVES = 0       
         CALC_SUPPLY_PATHS = 0
! 050107.
         MAX_SUPPLY_NODES = GET_NUM_OF_COAL_SUPPLY_REGIONS()
         MAX_COAL_LOAD_GROUPS = GET_MAX_COAL_LOAD_GROUPS()
         IF(ALLOCATED(NODE_INDEX)) DEALLOCATE(
     +                                       NODE_INDEX,
     +                                       MONTHLY_SUPPLY_BY_BASIN)
         ALLOCATE(NODE_INDEX(MAX_NODES),
     +            MONTHLY_SUPPLY_BY_BASIN(MAX_SUPPLY_NODES))
         NODE_INDEX = 0
         MONTHLY_SUPPLY_BY_BASIN = 0.
!
         DO I = 1, MAX_NODES 

!
! 050107. NOTE: NODES MAY BE EITHER SUPPLY OR DEMAND
            GS = GET_COAL_GROUP_INDEX(I)
            NODE_INDEX(GS) = I
            TEMP_I4_1 = GET_SUPPLY_CURVE_POINTS(GS)
            TEMP_I4_2 = GET_NUMBER_OF_SUPPLY_PATHS(GS) ! I IS THE COAL ID HERE
            CALC_SUPPLY_PATHS = CALC_SUPPLY_PATHS + TEMP_I4_2
            IF(TEMP_I4_1 > 0) THEN !  .AND. TEMP_I4_2 > 0) THEN
               PATHS_N_CURVES = PATHS_N_CURVES + TEMP_I4_1*(TEMP_I4_2+1)
            ENDIF
         END DO
         IF (CALC_SUPPLY_PATHS < TOTAL_PATHS) THEN
            WRITE(4,*) 'MISMATCH IN THE NUMBER OF PATHS' 
            WRITE(4,*) 'IN MARKET COAL PRICE'
         END IF   

         LOCAL_YEAR = YEAR + get_BASE_YEAR()
!         
         IF(ALLOCATED(TOTAL_DELIVERED_COST))
     +      DEALLOCATE( TOTAL_DELIVERED_COST,
     +                  DAILY_TOTAL_DELIVERED_COST,
     +                  TOTAL_DELIVERED_QUANT,
     +                  DAILY_PRICE,
     +                  DAILY_DIFFER,
     +                  MONTHLY_PRICE,
     +                  MONTHLY_DIFFER,
     +                  MONTHLY_VOLUME,
     +                  MONTHLY_IMPORT,
     +                  MONTHLY_EXPORT,
     +                  COAL_STORAGE_WITHDRAWAL,
     +                  REGION_ALLOCATION_STORAGE,
     +                  A_COEFFICIENT_STORAGE,
     +                  B_COEFFICIENT_STORAGE,
     +                  SOURCE_ID_STORAGE,
     +                  NODE_AT_DEPTH,
     +                  PATH_AT_DEPTH,
     +                  TOTAL_DELIVERED_POSITION,
     +                  DAILY_TOTAL_DELIVERED_POS,
     +                  TOTAL_REVERSE_POSITION,
     +                  DAILY_TOTAL_REVERSE_POS,
     +                  DAILY_DEMAND,
     +                  DAILY_DEMAND_INDEX,
     +                  DAILY_INTER_AVAIL,
     +                  DAILY_INTER_USED,
     +                  DEMAND_NODE_ACTIVE,
     +                  IS_DEMAND_NODE,
     +                  SUPPLY_NODE_ACTIVE,
     +                  MAX_S_FOR_I,
     +                  MAX_C_FOR_,
     +                  CONGESTION_QUANT,
     +                  CURRENT_C_FOR_,
     +                  FIRST_S_FOR_I,
     +                  PC_PATH,
     +                  PC_S,
     +                  PC_I,
     +                  LAST_PATH,
     +                  DEPTH_OF_LAST_PATH,
     +                  PATH_2_PC,
     +                  DAILY_BOUGHT,
     +                  MONTHLY_BOUGHT,
     +                  DAILY_SUPPLY,
     +                  MONTHLY_SUPPLY,
     +                  SUPPLY_AVAILABLE,
     +                  LINK_AVAILABLE,
     +                  LINK_LOADING,
     +                  MONTHLY_LINK_LOADING,
     +                  TOTAL_PATH_COST,
     +                  TOTAL_PATH_QUANT,
     +                  TOTAL_PATH_LOSSES,
     +                  LOCAL_COST_CURVE,
     +                  LOCAL_EXTRACT_CURVE,
     +                  LOCAL_EXTRACT_IN_DAY,
     +                  LOCAL_CAPACITY_CURVE,
     +                  LOCAL_COAL_TRANSPORT_RATE,
     +                  LOCAL_COAL_CONGESTION_COST,
     +                  LOCAL_COAL_TRANSPORT_QUANT,
     +                  LOCAL_COAL_TRANSPORT_LOSSES,
     +                  LOCAL_COAL_SHRINKAGE_PERCENT,
     +                  LOCAL_COAL_SHRINKAGE_COST)
         ALLOCATE(TOTAL_DELIVERED_COST(PATHS_N_CURVES),
     +            DAILY_TOTAL_DELIVERED_COST(PATHS_N_CURVES),
     +            TOTAL_DELIVERED_QUANT(PATHS_N_CURVES),
     +            DAILY_PRICE(MAX_NODES),
     +            DAILY_DIFFER(MAX_NODES),
     +            MONTHLY_PRICE(MAX_NODES),
     +            MONTHLY_DIFFER(MAX_NODES),
     +            MONTHLY_VOLUME(MAX_NODES),
     +            MONTHLY_IMPORT(MAX_NODES),
     +            MONTHLY_EXPORT(MAX_NODES),
     +            COAL_STORAGE_WITHDRAWAL(MAX_NODES),
     +            REGION_ALLOCATION_STORAGE(MAX_NODES),
     +            A_COEFFICIENT_STORAGE(MAX_NODES),
     +            B_COEFFICIENT_STORAGE(MAX_NODES),
     +            SOURCE_ID_STORAGE(MAX_NODES),
     +            NODE_AT_DEPTH(MAX_NODES),
     +            PATH_AT_DEPTH(MAX_NODES),
     +            TOTAL_DELIVERED_POSITION(PATHS_N_CURVES),
     +            DAILY_TOTAL_DELIVERED_POS(PATHS_N_CURVES),
     +            TOTAL_REVERSE_POSITION(PATHS_N_CURVES),
     +            DAILY_TOTAL_REVERSE_POS(PATHS_N_CURVES),
     +            DAILY_DEMAND(MAX_NODES),
     +            DAILY_DEMAND_INDEX(MAX_NODES),
     +            DAILY_INTER_AVAIL(MAX_NODES),
     +            DAILY_INTER_USED(MAX_NODES),
     +            DEMAND_NODE_ACTIVE(MAX_NODES),
     +            IS_DEMAND_NODE(MAX_NODES),
     +            SUPPLY_NODE_ACTIVE(MAX_NODES),
     +            MAX_S_FOR_I(MAX_NODES),
     +            MAX_C_FOR_(MAX_NODES,MAX_NODES),
     +            CONGESTION_QUANT(MAX_NODES,MAX_NODES),
     +            CURRENT_C_FOR_(MAX_NODES,MAX_NODES),
     +            FIRST_S_FOR_I(MAX_NODES),
     +            PC_PATH(PATHS_N_CURVES),
     +            PC_S(PATHS_N_CURVES),
     +            PC_I(PATHS_N_CURVES),
     +            LAST_PATH(MAX_NODES),
     +            DEPTH_OF_LAST_PATH(MAX_NODES),
     +            PATH_2_PC(MAX_NODES,MAX_NODES),
     +            DAILY_BOUGHT(MAX_NODES,MAX_NODES),
     +            MONTHLY_BOUGHT(MAX_NODES,MAX_NODES),
     +            DAILY_SUPPLY(MAX_NODES),
     +            MONTHLY_SUPPLY(MAX_NODES),
     +            SUPPLY_AVAILABLE(MAX_NODES),
     +            LINK_AVAILABLE(MAX_NODES,MAX_NODES),
     +            LINK_LOADING(MAX_NODES,MAX_NODES),
     +            MONTHLY_LINK_LOADING(MAX_NODES,MAX_NODES),
     +            TOTAL_PATH_COST(0:TOTAL_PATHS),
     +            TOTAL_PATH_QUANT(0:TOTAL_PATHS),
     +            TOTAL_PATH_LOSSES(0:TOTAL_PATHS),
     +            LOCAL_COST_CURVE(MAX_NODES),
     +            LOCAL_EXTRACT_CURVE(MAX_NODES),
     +            LOCAL_EXTRACT_IN_DAY(MAX_NODES),
     +            LOCAL_CAPACITY_CURVE(MAX_NODES),
     +            LOCAL_COAL_TRANSPORT_RATE(MAX_NODES,MAX_NODES),
     +            LOCAL_COAL_CONGESTION_COST(MAX_NODES,MAX_NODES),
     +            LOCAL_COAL_TRANSPORT_QUANT(MAX_NODES,MAX_NODES),
     +            LOCAL_COAL_TRANSPORT_LOSSES(MAX_NODES,MAX_NODES),
     +            LOCAL_COAL_SHRINKAGE_PERCENT(MAX_NODES),
     +            LOCAL_COAL_SHRINKAGE_COST(MAX_NODES))
         YES_MONTHLY_COAL_SELL_BUY_REPORT = 
     +                                    MONTHLY_COAL_SELL_BUY_REPORT()
         IF(YES_MONTHLY_COAL_SELL_BUY_REPORT .AND.
     +                       MONTHLY_COAL_SELL_BUY_REPORT_NOT_OPEN) THEN
            MONTHLY_COAL_SELL_BUY_UNIT = 
     +           MONTHLY_COAL_SELL_BUY_HEADER(MONTHLY_COAL_SELL_BUY_REC,
     +                                                        MAX_NODES)
         ENDIF
         YES_MONTHLY_COAL_LINK_REPORT = MONTHLY_COAL_LINK_REPORT()
         IF(YES_MONTHLY_COAL_LINK_REPORT .AND.
     +                        MONTHLY_COAL_LINK_REPORT_NOT_OPEN) THEN
            MONTHLY_COAL_LINK_UNIT = 
     +                 MONTHLY_COAL_LINK_HEADER(MONTHLY_COAL_LINK_REC,
     +                                                        MAX_NODES)
         ENDIF
         YES_DAILY_COAL_SELL_BUY_REPORT = DAILY_COAL_SELL_BUY_REPORT()
         IF(DAILY_COAL_SELL_BUY_REPORT_NOT_OPEN .AND. 
     +                              YES_DAILY_COAL_SELL_BUY_REPORT) THEN
            DAILY_COAL_SELL_BUY_REPORT_NOT_OPEN = .FALSE.
            DAILY_COAL_SELL_BUY_UNIT = 
     +               DAILY_COAL_SELL_BUY_HEADER(DAILY_COAL_SELL_BUY_REC,
     +                                                        MAX_NODES)
            DAILY_COAL_LINK_UNIT = 
     +                 DAILY_COAL_LINK_HEADER(DAILY_COAL_LINK_REC,
     +                                                        MAX_NODES)
            DAILY_COAL_PRICE_UNIT = 
     +                 DAILY_COAL_PRICE_HEADER(DAILY_COAL_PRICE_REC,
     +                                                        MAX_NODES)
            DAILY_COAL_DIFFER_UNIT = 
     +                 DAILY_COAL_DIFFER_HEADER(DAILY_COAL_DIFFER_REC,
     +                                                        MAX_NODES)
            MONTHLY_COAL_PRICE_UNIT = 
     +                 MONTHLY_COAL_PRICE_HEADER(MONTHLY_COAL_PRICE_REC,
     +                                                        MAX_NODES)
! SUPPLY     
            MONTHLY_COAL_BASIN_UNIT = 
     +                 MONTHLY_COAL_BASIN_HEADER(MONTHLY_COAL_BASIN_REC,
     +                                                 MAX_SUPPLY_NODES)
!     
            MONTHLY_COAL_DIFFER_UNIT = 
     +               MONTHLY_COAL_DIFFER_HEADER(MONTHLY_COAL_DIFFER_REC,
     +                                                        MAX_NODES)

! ALL NODES
            OPEN(10,FILE="MSG"//TRIM(GET_SCENAME())//".GSP")
            DO J = 1,  MAX_NODES
!               I = J ! TEMP. TEST. COAL_LOAD_GROUP_2_TG(J)
               GS = GET_COAL_GROUP_INDEX(J)
               WRITE(TEMP_STR,'(I6)') J-1
               TEMP_STR = '"'//GET_COAL_GROUP_NAME(GS)//'"'//
     +                       ',V2,'//TRIM(TEMP_STR)//
     +                        ',,S,,,,,,,,"Total COAL Sold and Bought",'
               WRITE(10,'(A)') TRIM(TEMP_STR)
            END DO
            CLOSE(10)
! SUPPLY NODES
            OPEN(10,FILE="MSG"//TRIM(GET_SCENAME())//".GSS")
            DO J = 1,  MAX_NODES
!               I = J ! TEMP. TEST. COAL_LOAD_GROUP_2_TG(J)
               GS = GET_COAL_GROUP_INDEX(J)
               IF(.NOT. COAL_SUPPLY_GROUP_ACTIVE(GS)) CYCLE
               WRITE(TEMP_STR,'(I6)') J-1
               TEMP_STR = '"'//GET_COAL_GROUP_NAME(GS)//'"'//
     +                       ',V2,'//TRIM(TEMP_STR)//
     +                        ',,S,,,,,,,,"Total COAL Sold and Bought",'
               WRITE(10,'(A)') TRIM(TEMP_STR)
            END DO
            CLOSE(10)
! DEMAND NODES
            OPEN(10,FILE="MSG"//TRIM(GET_SCENAME())//".GST")
            DO J = 1,  MAX_NODES
!               I = J ! TEMP. TEST. COAL_LOAD_GROUP_2_TG(J)
               GS = GET_COAL_GROUP_INDEX(J)
               IF(.NOT. COAL_DEMAND_GROUP_ACTIVE(GS)) CYCLE
               WRITE(TEMP_STR,'(I6)') J-1
               TEMP_STR = '"'//GET_COAL_GROUP_NAME(GS)//'"'//
     +                       ',V2,'//TRIM(TEMP_STR)//
     +                        ',,S,,,,,,,,"Total COAL Sold and Bought",'
               WRITE(10,'(A)') TRIM(TEMP_STR)
            END DO
            CLOSE(10)
!
            LAST_SEASON = get_PRODUCTION_PERIODS_in()
            CL_MONTH_NAME(LAST_SEASON+1) = 'Annual'
            ANNUAL_COUNTER = LAST_SEASON + 1
!            
         ENDIF
!
         TOTAL_DELIVERED_COST = 0.
         TOTAL_DELIVERED_QUANT = 0.
         TOTAL_DELIVERED_POSITION = 99999999
         TOTAL_REVERSE_POSITION = 99999999
!
         TOTAL_PATH_QUANT = 999999999999. ! MODEL TAKES MIN
         TOTAL_PATH_COST = 0.
         TOTAL_PATH_LOSSES = 0.
!         
         MAX_S_FOR_I = 0
         MAX_C_FOR_ = 0 ! TO ALLOW FOR NO COST

         FIRST_S_FOR_I = 0
!
         PC_PATH = 0
         PC_S = 0
!         PC_C = 0
         PC_I = 0
         DEPTH_OF_LAST_PATH = 0
         DAILY_BOUGHT = 0.
         DAILY_SUPPLY = 0.
         SUPPLY_AVAILABLE = 0.
         LINK_AVAILABLE = 0.
!         LINK_LOADING = 0.
         MONTHLY_BOUGHT = 0.
         MONTHLY_LINK_LOADING = 0.
!         
         LOCAL_COST_CURVE = 0.
         LOCAL_EXTRACT_CURVE = 0.
         LOCAL_EXTRACT_IN_DAY = 0.
         LOCAL_CAPACITY_CURVE = 0.
         LOCAL_COAL_TRANSPORT_RATE = 0.
         LOCAL_COAL_CONGESTION_COST = 0.
         LOCAL_COAL_TRANSPORT_QUANT = 0.
         LOCAL_COAL_TRANSPORT_LOSSES = 0.
         LOCAL_COAL_SHRINKAGE_PERCENT = 0.
         LOCAL_COAL_SHRINKAGE_COST = 0.
! SORTED COST CURVE IS THE SAME FOR THE ENTIRE MONTH (CURRENT CONFIG)         
         PC = 1! PATH AND CURVE COUNTER
         TOTAL_DELIVERED_POSITION(PC) = PC
! CONSIDER TRANSACTIONS WITH THE NODE (SUPPLY I SELLS TO DEMAND I, ETC.)
!
! 100206. 
!
!        I = 1
! MAY NEED TO RE-INDEX BECAUSE OF "BLANK" SUPPLY CURVES.
!
         PATH_2_PC = 0
!
!
!         DO L_I = 1, MAX_NODES 
         DO L_I = 1, MAX_NODES 
!            L_I = GET_COAL_GROUP_POSITION(I)
            GS = GET_COAL_GROUP_INDEX(L_I)
            TEMP_L = GET_SUPPLY_COST_CAPACITY_CURVES(
     +              GS,MAX_S,COST_CURVE,CAPACITY_CURVE,EXTRACT_CURVE,
     +                            SHRINKAGE_COST,SHRINKAGE_PERCENT,
     +                            FIRST_S,R_YEAR)

            IF(.NOT. TEMP_L) CYCLE
!                        
            MAX_S_FOR_I(L_I) = MIN(MAX_S,MAX_S_IN_MONTH)
            FIRST_S_FOR_I(L_I) = FIRST_S
!            IF(MAX_S > MAX_POINTS_IN_GS) THEN
!               WRITE(4,*) 'INTERNAL CONFLICT IN SUPPLY BASIN SIZE'
!            ENDIF
            LOCAL_COAL_SHRINKAGE_PERCENT(L_I) = SHRINKAGE_PERCENT
            LOCAL_COAL_SHRINKAGE_COST(L_I) = SHRINKAGE_COST
!
! 050107. NEW BASIN DEFINITION
!
            S = 1
!            
          
               T = S + FIRST_S_FOR_I(L_I) - 1
               TOTAL_DELIVERED_COST(PC) = COST_CURVE +
     +                                    LOCAL_COAL_SHRINKAGE_COST(L_I)
!     +                                                    SHRINKAGE_COST
               TOTAL_DELIVERED_QUANT(PC) = CAPACITY_CURVE
!
               LOCAL_COST_CURVE(L_I) = COST_CURVE
               LOCAL_EXTRACT_CURVE(L_I) = EXTRACT_CURVE
               LOCAL_CAPACITY_CURVE(L_I) = CAPACITY_CURVE
!               
               SUPPLY_AVAILABLE(L_I) = CAPACITY_CURVE
!
               PC_PATH(PC) = 0
               PC_S(PC) = S
               PC_I(PC) = L_I
! 112206
               PATH_2_PC(L_I,L_I) = PC
!
! 040907. TRANSMISSION CONGESTION. MUST TRACK THIS CASE THROUGH.
!
               C = 0 ! RESERVE 0 FOR ZERO CONGESTION? A GOOD FEATURE?
               

               TOTAL_DELIVERED_POSITION(PC) = PC
               PC = PC + 1

         END DO ! MAX_NODES
!
         TOTAL_PC_FOR_INTERNAL_TRANS = PC - 1 
!         
         DO PATH = 1, TOTAL_PATHS
!
! DETERMINE THE TOTAL TRANSPORTATION COST FOR THE TREE USING THE 
! ORIGINAL PATHS SET.  THEN, ADD THE APPROPRIATE SUPPLY COST AT THE ROOT OF THE
! TREE FOR THE PC SET AND RECORD THE TOTAL COST FOR THE PC SET.
!
!
            I = GET_NODE_OF_PATH(PATH)
            L_I = GET_COAL_GROUP_POSITION(I) ! COAL ID
            J = GET_PARENT_OF_LEAF(PATH) ! PARENT OF THE LEAF
            L_J = GET_COAL_GROUP_POSITION(J)
            PARENT_PATH = GET_PARENT_PATH_OF_LEAF(PATH)
            K = GET_LEAF_OF_PATH(PATH)   ! LEAF OF PATH
            L_K = GET_COAL_GROUP_POSITION(K)
!
! THIS IS TO ADJUST TO HAVING MULTIPLE ROOTS OF THE TREE
! 
!
            IF(J == 0) THEN
               J = I
               L_J = L_I
            ENDIF
!
            IF(J < 0 .OR. J > 31000 .OR. K < 0 .OR. K > 31000) THEN
               WRITE(4,*) "EXCEEDED INDEX IN COAL PRICING",J,K
            ENDIF
!
! 041307. NOTE: C FOR THE BOTTOM INTERVAL IS SET TO EITHER 0 OR 1 HERE
!
!            MAX_C_FOR_(L_J,L_K) = GET_MAX_C_FOR_(L_J,L_K,C)
            MAX_C_FOR_(L_J,L_K) = GET_MAX_C_FOR_(J,K,C)
!            CURRENT_C_FOR_(L_J,L_K) = C
!
            LOCAL_COAL_TRANSPORT_RATE(L_J,L_K) = 0. ! REDEFINED 031409.
!            LOCAL_COAL_TRANSPORT_RATE(L_J,L_K) = 
!     +                        GET_COAL_TRANSPORT_RATE(J,K,R_YEAR,R_MONTH)
            LOCAL_COAL_CONGESTION_COST(L_J,L_K) = 
     +                 GET_COAL_TRANS_CONGEST_COST(J,K,C,R_YEAR,R_MONTH)
            LOCAL_COAL_TRANSPORT_QUANT(L_J,L_K) = 
     +                      GET_COAL_TRANSPORT_QUANT(J,K,R_YEAR,R_MONTH)
            LOCAL_COAL_TRANSPORT_LOSSES(L_J,L_K) = 
     +                     GET_COAL_TRANSPORT_LOSSES(J,K,R_YEAR,R_MONTH)
! 042407. FUEL RETENTION IS "HARD-WIRED" TO 1% OF THE TRANSPORT RATE.
            TOTAL_PATH_COST(PATH) = TOTAL_PATH_COST(PARENT_PATH) + 
     +          (1.+FUEL_RETENTION_CHARGE)*
     +                   LOCAL_COAL_TRANSPORT_RATE(L_J,L_K) +
     +                               LOCAL_COAL_CONGESTION_COST(L_J,L_K)
            TOTAL_PATH_QUANT(PATH) = MIN(TOTAL_PATH_QUANT(PARENT_PATH),
     +                              LOCAL_COAL_TRANSPORT_QUANT(L_J,L_K))
            TOTAL_PATH_LOSSES(PATH) = 
     +            MAX(0.,MIN(1,
     +                  1. - (1. - TOTAL_PATH_LOSSES(PARENT_PATH)) *
     +                     (1. - LOCAL_COAL_TRANSPORT_LOSSES(L_J,L_K))))
            IF(TOTAL_PATH_LOSSES(PATH) > .9) THEN
               TOTAL_PATH_LOSSES(PATH) = TOTAL_PATH_LOSSES(PATH)
            ENDIF

! 041307.                   
! 043007. MOVED UP TO ACCOUNT FOR NON-DEMAND PATHS FOR CONGESTION PRICING.
!            TOTAL_PC_FOR_(L_J,L_K) = TOTAL_PC_FOR_(L_J,L_K) + 1
!            PC_FOR_(TOTAL_PC_FOR_(L_J,L_K),L_J,L_K) = PC
!
! 042007. HUGE CHANGE. REMOVE ALL NON-DEMAND PC'S
! 050307. PUT BACK IN TO MATCH PATHS LIST WITH PATHS LIST IN LINKS FILE
!
!            IF(.NOT. COAL_DEMAND_GROUP_ACTIVE(K)) CYCLE
!            
!!
! ENUMERATE VALID SUPPLY CURVE POINTS AGAINST PATHS.
!
! 101006. WHAT IS THE DIFFERENCE BETWEEN: TOTAL_DELIVERED_QUANT(PC) AND SUPPLY_AVAILABLE(S,I)
!
            GS = L_I ! 040307. CHANGED.
!            GS = I ! FROM ABOVE.
!
! RESET S TO ONE VALUE (CURRENT S)
!

               S = 1
!
               PATH_2_PC(L_I,L_K) = PC

               TOTAL_DELIVERED_COST(PC) = 
     +                      LOCAL_COST_CURVE(GS) +
     +                                     TOTAL_PATH_COST(PATH)
     
               TOTAL_DELIVERED_QUANT(PC) = 
     +                 MIN(LOCAL_CAPACITY_CURVE(GS),
     +                                           TOTAL_PATH_QUANT(PATH))
               PC_PATH(PC) = PATH
               PC_S(PC) = S

               TOTAL_DELIVERED_POSITION(PC) = PC
               PC = PC + 1

         END DO ! NEXT PATH
!
         TOTAL_PC = PC - 1

         CALL REAL4_CUSTOM_Sort(
     +            TOTAL_PC, ! PATHS_N_CURVES,           ! INT*4
     +            TOTAL_DELIVERED_POSITION, ! INT*4
     +            TOTAL_DELIVERED_COST)     ! REAL*4
!         
! SET TRANSPORT SYSTEM
! SET THE POINT ON THE COST CURVE
! SET INITIAL DEMANDS
!          
! INITIAL DIAGNOSTIC
!
! TEMPORARY
!
! TEST. 091406.
!
         DO PC_COUNT = 1, TOTAL_PC
            TOTAL_REVERSE_POSITION(TOTAL_DELIVERED_POSITION(PC_COUNT))
     +                                                        = PC_COUNT
         END DO
         IF(R_MONTH == 1 .AND. R_YEAR == 1) THEN
            OPEN(10,'PC_DIAG.TXT')
!
            DO PC_COUNT = 1, TOTAL_PC
               PC = TOTAL_DELIVERED_POSITION(PC_COUNT)
               PATH = PC_PATH(PC)
               IF(PC > TOTAL_PC_FOR_INTERNAL_TRANS) THEN
                  I = GET_NODE_OF_PATH(PATH)
                  L_I = GET_COAL_GROUP_POSITION(I)
                  J = GET_PARENT_OF_LEAF(PATH)
                  L_J = GET_COAL_GROUP_POSITION(J)
                  K = GET_LEAF_OF_PATH(PATH)
                  L_K = GET_COAL_GROUP_POSITION(K)
                  Q = GET_PARENT_PATH_OF_LEAF(PATH)
               ELSE
                  I = PC_I(PC)
                  J = PC_I(PC)
                  K = PC_I(PC)
                  Q = PC_I(PC)
               ENDIF
               WRITE(10,*) PC_COUNT,PC,PATH,I,J,K,Q,
     +                  TOTAL_DELIVERED_QUANT(PC),
     +                  TOTAL_DELIVERED_COST(PC),
     +                  TOTAL_PATH_LOSSES(PATH)
            END DO
            CLOSE(10)
         ENDIF
!                  
!         OPEN(10,'DAILY_SELL_BUY.TXT')
         MONTHLY_SUPPLY = 0.
         MONTHLY_PRICE = 0.
         MONTHLY_DIFFER = 0.
         MONTHLY_VOLUME = 0.
         MONTHLY_IMPORT = 0.
         MONTHLY_EXPORT = 0.
!
! 101506. STORAGE IS BASED UPON GS INDEX
!         
! Y = AX + B
! WHERE B IS THE NEGATIVE OF THE BALANCE WHERE THERE IS NO
! INJECTION AND NO WITHDRAWAL.
!
         SOURCE_ID_STORAGE = 0
         REGION_ALLOCATION_STORAGE = 1.0
! WESTERN: 
         A_COEFFICIENT_STORAGE(1) = .6415
         B_COEFFICIENT_STORAGE(1) = -5790.5
         SOURCE_ID_STORAGE(1) = 1
! CENTRAL: 
         A_COEFFICIENT_STORAGE(2) = .7545
         B_COEFFICIENT_STORAGE(2) = -3236.4
         SOURCE_ID_STORAGE(2) = 2
! MIDWEST: 
         A_COEFFICIENT_STORAGE(4) = .9095
         B_COEFFICIENT_STORAGE(4) = -9850.2
         SOURCE_ID_STORAGE(4) = 4
! NORTHEAST: 
         A_COEFFICIENT_STORAGE(5) = .8841
         B_COEFFICIENT_STORAGE(5) = -9204.3
         SOURCE_ID_STORAGE(5) = 5
! BRITISH COLUMBIA: 
         A_COEFFICIENT_STORAGE(8) = .4052
         B_COEFFICIENT_STORAGE(8) = -4334.1
         SOURCE_ID_STORAGE(8) = 4
         REGION_ALLOCATION_STORAGE(8) = .0623
! ALBERTA: 
         A_COEFFICIENT_STORAGE(9) = .4052
         B_COEFFICIENT_STORAGE(9) = -4334.1
         SOURCE_ID_STORAGE(9) = 4
         REGION_ALLOCATION_STORAGE(9) = .5126
! SASK: 
         A_COEFFICIENT_STORAGE(10) = .4052
         B_COEFFICIENT_STORAGE(10) = -4334.1
         SOURCE_ID_STORAGE(10) = 4
         REGION_ALLOCATION_STORAGE(10) = .0663
! ONTARIO: 
         A_COEFFICIENT_STORAGE(12) = .4052
         B_COEFFICIENT_STORAGE(12) = -4334.1
         SOURCE_ID_STORAGE(12) = 4
         REGION_ALLOCATION_STORAGE(12) = .3510
! QUEBEC: 
         A_COEFFICIENT_STORAGE(13) = .4052
         B_COEFFICIENT_STORAGE(13) = -4334.1
         SOURCE_ID_STORAGE(13) = 4
         REGION_ALLOCATION_STORAGE(13) = .0790
!
         DO DA = 1, DAYS_IN_MONTH
            DEMAND_GROUPS_SATISFIED = 0
            COAL_STORAGE_WITHDRAWAL = 0.
            DAILY_PRICE = 0.
            DAILY_DIFFER = 0.
            DAILY_INTER_USED = 0.
            LAST_PATH = 0
!
! 050407. RESET DAILY FOR CONGESTION            
!
            DAILY_TOTAL_DELIVERED_POS  = TOTAL_DELIVERED_POSITION
            DAILY_TOTAL_DELIVERED_COST = TOTAL_DELIVERED_COST
            DAILY_TOTAL_REVERSE_POS = TOTAL_REVERSE_POSITION
!
            DAILY_DEMAND_INDEX = 0
            DAILY_DEMAND = 0.
            L_I = 1            
!
            DO L_K = 1, MAX_NODES
!
! STORAGE ESTIMATE.
!
               GS = GET_COAL_GROUP_INDEX(L_K)

               DAILY_INTER_AVAIL(L_K) = TEMP_R
               IF(DAILY_DEMAND(L_K) > .0001) THEN
                  DEMAND_NODE_ACTIVE(L_K) = .TRUE.
                  IS_DEMAND_NODE(L_K) = .TRUE.
               ELSE
                  DEMAND_NODE_ACTIVE(L_K) = .FALSE.
                  IS_DEMAND_NODE(L_K) = .FALSE.
                  DEMAND_GROUPS_SATISFIED = DEMAND_GROUPS_SATISFIED + 1
               ENDIF
!               SUPPLY_NODE_ACTIVE(K) = 
!     +                          LOCAL_CAPACITY_CURVE(:,K) > .0001
            END DO
!            
! SUPPLY VARIABLES RESET DAILY.
!
            DAILY_BOUGHT = 0.
            DAILY_SUPPLY = 0.
            LINK_AVAILABLE = 0.
            LINK_LOADING = 0.
            LOCAL_EXTRACT_IN_DAY = LOCAL_EXTRACT_CURVE
!            
            CURRENT_C_FOR_ = 0
            CONGESTION_QUANT = 999999.0
!
! CONTINUE ASSIGNMENTS UNTIL 
!  1. ALL DEMAND IS SATISFIED OR            
!  2. NO MORE COAL CAN BE TRANSPORTED OR
!  3. NO MORE COAL CAN BE SUPPLIED
!
!
!            DO PC_COUNT = 1, TOTAL_PC 
            PC_COUNT = 0
            INCREMENT_PC_COUNT = .TRUE. 
!            
! 060107.
!
            RUN_LP_COAL_MODEL = .TRUE.
!            
            IF(RUN_LP_COAL_MODEL) THEN            
               CALL GregsRoutine()
            ELSE
             DOWHILE( (PC_COUNT < TOTAL_PC .OR. 
     +                                         .NOT. INCREMENT_PC_COUNT) 
     +                        .AND. DEMAND_GROUPS_SATISFIED < MAX_NODES)
               IF(INCREMENT_PC_COUNT) THEN
                  PC_COUNT = PC_COUNT + 1
               ENDIF
!               
!               IF(DEMAND_GROUPS_SATISFIED == MAX_NODES) THEN
!                  EXIT
!               ENDIF
!
               INCREMENT_PC_COUNT = .TRUE. 
!               
               PC = DAILY_TOTAL_DELIVERED_POS(PC_COUNT)
               PATH = PC_PATH(PC)
               NODE_AT_DEPTH = 0
               PATH_AT_DEPTH = 0
               IF(PC > TOTAL_PC_FOR_INTERNAL_TRANS) THEN
                  I = GET_NODE_OF_PATH(PATH)
                  L_I = GET_COAL_GROUP_POSITION(I)
                  K = GET_LEAF_OF_PATH(PATH)
                  L_K = GET_COAL_GROUP_POSITION(K)
!
! 112106. QUICK CHECK TO CYCLE IF NOT LOADS
!
                  IF(DAILY_DEMAND(L_K) < .0001) CYCLE
!                  
! TRANVERSE EACH LINK FROM LEAF TO ROOT TO CALCULATE LOCAL_QUANT WITH LOSSES
                  LOCAL_PATH = PATH
                  DEPTH = 1
                  NODE_AT_DEPTH(DEPTH) = L_K
                  PATH_AT_DEPTH(DEPTH) = PATH
                  S = PC_S(PC)
!                  
! PROBLEM: THERE ARE MANY C'S FOR A GIVEN PC 
!
!                  C = PC_C(PC)
                  LOCAL_QUANT = MIN(SUPPLY_AVAILABLE(L_I),
     +                                      LOCAL_EXTRACT_IN_DAY(L_I))
!                  LOCAL_QUANT = 999999999999.
!                  
                  DO  
                     P = GET_LEAF_OF_PATH(LOCAL_PATH)
                     L_P = GET_COAL_GROUP_POSITION(P)
                     Q = GET_PARENT_OF_LEAF(LOCAL_PATH)
                     L_Q = GET_COAL_GROUP_POSITION(Q)
                     PATH_AT_DEPTH(DEPTH) = LOCAL_PATH
                     DEPTH = DEPTH + 1
! ROOT OF NODE.                     
                     IF(L_Q == 0)THEN 
                        L_Q = L_I
                        Q = I
                     ENDIF
!                     
                     NODE_AT_DEPTH(DEPTH) = L_Q
!                     
                     LOCAL_QUANT = 
     +                  MAX(0.,MIN(LOCAL_QUANT,
     +                        LOCAL_COAL_TRANSPORT_QUANT(L_Q,L_P)-
     +                                          LINK_LOADING(L_Q,L_P)))
!
                     IF(LOCAL_QUANT < .01) EXIT
!                     
!
! 041107. TRANSMISSION CONGESTION ADDED.
! IF CF IS NOT WITHIN THE C INTERVAL, THEN CYCLE.
! NEED TO EXPLICITLY CONSIDER THE NO CONGESTION INVERVAL.
! MAY NEED TO INC C = 0
! PROBLEM: THERE ARE MANY C FOR A GIVEN PC
! 041307.
! SOLUTION: IF CONGESTION FUNCTION ACTIVE, DYNAMICALLY ADJUST THE 
! CAPACITY FOR THE TRANSACTION AND THEN, IF THAT CAPACITY IS BINDING, 
! ADJUST ALL PC THAT USE THE C CONSTRAINT ACROSS ALL C CONSTRAINTS.
! THIS IS ACCOMPLISHED BY RECALCULATING THE TOTAL DELIVERED COST OF EACH
! PC THAT IS BINDING AND THEN MOVE IT UP THE STACK TO THE NEXT LOCATION AND
! SHIFT ALL OTHER ELEMENTS DOWNWARD.
! THE SET OF ALL LINKS TO BE CONSIDERED IS WELL DEFINED BY PC. ASSUME THAT THE
! DEPTH OF PC IS NO MORE THAT 1000 NODES (999 LINKS). HOW MANY PC SHARE THE
! SAME LINK?   
! 
                     IF(LOCAL_COAL_TRANSPORT_QUANT(L_Q,L_P) > 0.0001 
     +                                                            ) THEN
                        CF = LINK_LOADING(L_Q,L_P)/
     +                        LOCAL_COAL_TRANSPORT_QUANT(L_Q,L_P)
!
                        C = CURRENT_C_FOR_(L_Q,L_P)  
!
                        IF(.NOT. TEST_COAL_LINK_CF(Q,P,C,CF)) THEN
                           LOCAL_QUANT = 0.0
                           EXIT
                        ELSE
                           TEMP_R = 
     +                        AVAIL_COAL_GIVEN_LINK_CONGEST(Q,
     +                                                     P,
     +                                                     C,
     +                                           LINK_LOADING(L_Q,L_P),
     +                              LOCAL_COAL_TRANSPORT_QUANT(L_Q,L_P))
!
! IT WILL BE FAST TO TRAVERSE THESE INDICES AT THE END OF THIS LOOP
!
                           LOCAL_QUANT = MIN(LOCAL_QUANT,TEMP_R)
                           CONGESTION_QUANT(L_Q,L_P) = TEMP_R
!     
                        ENDIF
!           
                     ELSE
                        CF = 0.0
                     ENDIF
!                     
                     IF(L_Q == L_I .OR. DEPTH == MAX_NODES-1 .OR.
     +                                        LOCAL_QUANT < 0.01) EXIT
                     LOCAL_PATH = GET_PARENT_PATH_OF_LEAF(LOCAL_PATH)
                  END DO ! DEPTH

! THIS IS THE MAXIMUM THAT CAN BE:
!
!  1. SUPPLIED FROM THIS PRICE POINT;
!  2. TRANSPORTED ACROSS THIS LINE;
!  3. DELIVERED TO THIS CUSTOMER
!
! NEED TO DETERMINE THE MOST BINDING CONSTRAINT:
!  1. IF SUPPLY, THEN START UPSTREAM WITH SUPPLY AND MOVE DOWNSTREAM TO DEMAND
!  2. IF DEMAND, THEN START DOWNSTREAM WITH DEMAND AND MOVE UPSTREAM TO SUPPLY
!  3. IF TRANSPORT, THEN START MIDSTREAM AND MOVE UP AND DOWNSTREAM
!
                  TRANQ = MIN(
     +                     LOCAL_QUANT, ! TRANSPORT
     +                     DAILY_DEMAND(L_K))  ! DEMAND
!
                  TOTAL_AVAIL = MIN(1.,(1.-TOTAL_PATH_LOSSES(PATH)*
     +                          (1.-LOCAL_COAL_SHRINKAGE_PERCENT(L_I))))
!
                  IF(TOTAL_AVAIL < .0000001) CYCLE
! 040307. QUANTITY TRANSPORTED SHOULD VARY BY LINK.
                  IF( ABS(TRANQ-SUPPLY_AVAILABLE(L_I)) < .0001) THEN
                     QUANTITY_SUPPLIED = TRANQ
                     QUANTITY_TRANSPORTED = QUANTITY_SUPPLIED *
     +                            (1.-LOCAL_COAL_SHRINKAGE_PERCENT(L_I))
                     QUANTITY_DEMANDED = QUANTITY_TRANSPORTED *
     +                                      (1.-TOTAL_PATH_LOSSES(PATH))
                  ELSEIF(ABS(TRANQ-DAILY_DEMAND(L_K)) < .0001) THEN
                     QUANTITY_DEMANDED = TRANQ
                     QUANTITY_TRANSPORTED = QUANTITY_DEMANDED /
     +                                      (1.-TOTAL_PATH_LOSSES(PATH))
                     QUANTITY_SUPPLIED = QUANTITY_TRANSPORTED /
     +                            (1.-LOCAL_COAL_SHRINKAGE_PERCENT(L_I))
                  ELSE ! ASSUME THAT IT IS A TRANSPORT CONSTRAINT
                     QUANTITY_TRANSPORTED = TRANQ
                     QUANTITY_DEMANDED = QUANTITY_TRANSPORTED *
     +                                      (1.-TOTAL_PATH_LOSSES(PATH))
                     QUANTITY_SUPPLIED =  QUANTITY_TRANSPORTED /
     +                            (1.-LOCAL_COAL_SHRINKAGE_PERCENT(L_I))
                  ENDIF
               ELSE ! SERVED BY INTERNAL
                  IF(PC > PATHS_N_CURVES .OR. PC < 1) THEN
                     WRITE(4,*) 'EXCEEDED SIZE OF TOPOLOGY'
                  ENDIF
                  L_I = PC_I(PC)
                  L_J = PC_I(PC)
                  L_K = PC_I(PC)
! ???
                  S = PC_S(PC)
!                  C = PC_C(PC)
                  TRANQ = 
     +                  MIN(TOTAL_DELIVERED_QUANT(PC),DAILY_DEMAND(L_K),
     +                                      LOCAL_EXTRACT_IN_DAY(L_I))
                  IF( ABS(TRANQ -
     +                     MIN(SUPPLY_AVAILABLE(L_I),
     +                       LOCAL_EXTRACT_IN_DAY(L_I))) < .0001) THEN
                     QUANTITY_SUPPLIED = TRANQ
                     QUANTITY_DEMANDED = QUANTITY_SUPPLIED *
     +                            (1.-LOCAL_COAL_SHRINKAGE_PERCENT(L_I))
                  ELSEIF(ABS(TRANQ-DAILY_DEMAND(L_K)) < .0001) THEN
                     QUANTITY_DEMANDED = TRANQ
                     QUANTITY_SUPPLIED = QUANTITY_DEMANDED /
     +                            (1.-LOCAL_COAL_SHRINKAGE_PERCENT(L_I))
                  ENDIF
                  QUANTITY_TRANSPORTED = 0.0

               ENDIF

               IF(.NOT. DEMAND_NODE_ACTIVE(L_K)) CYCLE
!               
               IF(TRANQ > 0.000001 .OR. C == 0) THEN
!
                  LAST_PATH(L_K) = PATH
                  DEPTH_OF_LAST_PATH(L_K) = DEPTH
!                  
                  DAILY_DEMAND(L_K) = DAILY_DEMAND(L_K) - 
     +                                                 QUANTITY_DEMANDED
!                  DAILY_DEMAND(K) = DAILY_DEMAND(K) - TRANQ
                  IF(DAILY_DEMAND(L_K) > .0001) THEN
                     DEMAND_NODE_ACTIVE(L_K) = .TRUE.
                  ELSE
                     DEMAND_NODE_ACTIVE(L_K) = .FALSE.
                     DEMAND_GROUPS_SATISFIED = 
     +                                       DEMAND_GROUPS_SATISFIED + 1
                  ENDIF
                  LOCAL_EXTRACT_IN_DAY(L_I) = 
     +                      MAX(0.0,LOCAL_EXTRACT_IN_DAY(L_I) - TRANQ)
                  DAILY_PRICE(L_K) = MAX(DAILY_PRICE(L_K),
     +                                   DAILY_TOTAL_DELIVERED_COST(PC))
                  MONTHLY_PRICE(L_K) = MONTHLY_PRICE(L_K) + 
     +                              DAILY_PRICE(L_K) * QUANTITY_DEMANDED
                  MONTHLY_VOLUME(L_K) = MONTHLY_VOLUME(L_K) +
     +                                                 QUANTITY_DEMANDED
                  DAILY_BOUGHT(L_I,L_K) = DAILY_BOUGHT(L_I,L_K) + 
     +                                                 QUANTITY_DEMANDED
                  MONTHLY_BOUGHT(L_I,L_K) = MONTHLY_BOUGHT(L_I,L_K) + 
     +                                                 QUANTITY_DEMANDED
!                  
                  SUPPLY_AVAILABLE(L_I) = 
     +               MAX(0.,SUPPLY_AVAILABLE(L_I) - QUANTITY_SUPPLIED)


                  DAILY_SUPPLY(L_I) = DAILY_SUPPLY(L_I) + 
     +                                                 QUANTITY_SUPPLIED
                  MONTHLY_SUPPLY(L_I) = MONTHLY_SUPPLY(L_I) + 
     +                                                 QUANTITY_SUPPLIED

! TOUGH PART. REDUCE EXISTING SYSTEM LINKS LOADING AND TOTAL_DELIVERED_QUANT EFFICIENTLY                  
! SOLUTION: TEST 
! RECURSIVE ROUTINE TO REDUCE THE QUANTITY ALONG EACH PATH.
! 040307. NEED TO REFLECT THE LOSSES AT EACH LINK
!
                  IF(PC > TOTAL_PC_FOR_INTERNAL_TRANS) THEN
                     TEMP_TRANSPORTED = QUANTITY_DEMANDED 
                     DO D = 1, DEPTH-1
                        L_P = NODE_AT_DEPTH(D)
!                        L_P = GET_COAL_GROUP_POSITION(P)
                        L_Q = NODE_AT_DEPTH(D+1)
                        IF(L_Q == 0) L_Q = L_I
!                        L_Q = GET_COAL_GROUP_POSITION(Q)
!
! NET OUT THE REVERSE TRANSACTION, IF ONE.
!                        
! 040307.
!
                        IF(LOCAL_COAL_TRANSPORT_LOSSES(L_Q,L_P) > 
     +                                                      0.0001 .AND.
     +                        LOCAL_COAL_TRANSPORT_LOSSES(L_Q,L_P) < 
     +                                                      0.9999) THEN
                           TEMP_TRANSPORTED = TEMP_TRANSPORTED / 
     +                       (1. - LOCAL_COAL_TRANSPORT_LOSSES(L_Q,L_P))
                        ENDIF
!
                        IF(LINK_LOADING(L_P,L_Q) > 0.) THEN
                           TEMP_R = MIN(LINK_LOADING(L_P,L_Q),
!     +                                              TEMP_TRANSPORTED)
     +                                             QUANTITY_TRANSPORTED)
                           LINK_AVAILABLE(L_P,L_Q) = 
     +                        LINK_AVAILABLE(L_P,L_Q) + TEMP_R
                           LINK_LOADING(L_P,L_Q) = 
     +                           LINK_LOADING(L_P,L_Q) - TEMP_R
                           MONTHLY_LINK_LOADING(L_P,L_Q) = 
     +                           MONTHLY_LINK_LOADING(L_P,L_Q) - TEMP_R
!                           TEMP_TRANSPORTED = 
!     +                             MAX(0.,TEMP_TRANSPORTED - TEMP_R)
                           QUANTITY_TRANSPORTED = 
     +                             MAX(0.,QUANTITY_TRANSPORTED - TEMP_R)
                        ENDIF
!                        
! QUANTITY TRANSPORTED IS NOW THE AMOUNT AFTER NETTING THE REVERSE TRANSACTION
!
                        LINK_AVAILABLE(L_Q,L_P) = 
     +                    LINK_AVAILABLE(L_Q,L_P) - QUANTITY_TRANSPORTED
!    +                                               TEMP_TRANSPORTED
                        LINK_LOADING(L_Q,L_P) = LINK_LOADING(L_Q,L_P) +
!     +                                              TEMP_TRANSPORTED
     +                                              QUANTITY_TRANSPORTED
!
                        MONTHLY_IMPORT(L_P) = MONTHLY_IMPORT(L_P) +
     +                                              QUANTITY_TRANSPORTED
                        MONTHLY_EXPORT(L_Q) = MONTHLY_EXPORT(L_Q) +
     +                                              QUANTITY_TRANSPORTED
!     
                        MONTHLY_LINK_LOADING(L_Q,L_P) = 
     +                        MONTHLY_LINK_LOADING(L_Q,L_P) +
!     +                                              TEMP_TRANSPORTED
     +                                              QUANTITY_TRANSPORTED
                        C = CURRENT_C_FOR_(L_Q,L_P)
                        IF(R_YEAR == 1 .AND. R_MONTH == 1) THEN
                           IF(DIAG_NOT_OPEN) THEN
                              OPEN(4429,FILE='DAILY_COAL_DIAG.CSV')
                              DIAG_NOT_OPEN = .FALSE.
                              WRITE(4429,*) 'MONTH,DAY,PC_COUNT,'//
     +                                      'PC,D,FROM,TO,'//
     +                                      'TRANSPORT_QUANT,TRANQ,'//
     +                                      'LINK_LOADING,C,'//
     +                                      'CONGEST QUANT,'//
     +                                      'TOTAL_DEL_COST'
                           ENDIF
                           LOCAL_PATH = PATH_AT_DEPTH(D) 
                           P = GET_LEAF_OF_PATH(LOCAL_PATH)
                           Q = GET_PARENT_OF_LEAF(LOCAL_PATH)
                           IF(Q == 0) THEN
                              TEMP_Q = I
                           ELSE
                              TEMP_Q = Q
                           ENDIF
                           WRITE(4429,*) 
     +                           R_MONTH,',',
     +                           DA,',',
     +                           PC_COUNT,',',
     +                           PC,',',
     +                           D,',',
     +                           TEMP_Q,',',
     +                           P,',',
     +                          LOCAL_COAL_TRANSPORT_QUANT(L_Q,L_P),',',
     +                           TRANQ,',', 
     +                           LINK_LOADING(L_Q,L_P),',',
     +                           C,',',
     +                           CONGESTION_QUANT(L_Q,L_P),',',
     +                           DAILY_TOTAL_DELIVERED_COST(PC)
                        ENDIF

                        IF(D < DEPTH-1) THEN
                           DAILY_PRICE(L_Q) = DAILY_PRICE(L_P) - 
     +                              LOCAL_COAL_TRANSPORT_RATE(L_Q,L_P) -
     +                               LOCAL_COAL_CONGESTION_COST(L_Q,L_P)
                           MONTHLY_PRICE(L_Q) = MONTHLY_PRICE(L_Q) + 
     +                              DAILY_PRICE(L_Q) * QUANTITY_DEMANDED
                           MONTHLY_VOLUME(L_Q) = MONTHLY_VOLUME(L_Q) +
     +                                                 QUANTITY_DEMANDED
                           DAILY_BOUGHT(L_I,L_Q) = 
     +                              DAILY_BOUGHT(L_I,L_Q) + 
     +                                                 QUANTITY_DEMANDED
                           MONTHLY_BOUGHT(L_I,L_Q) = 
     +                              MONTHLY_BOUGHT(L_I,L_Q) + 
     +                                                 QUANTITY_DEMANDED
                        ENDIF

!
                        IF(CONGESTION_QUANT(L_Q,L_P) - 
     +                                QUANTITY_TRANSPORTED < .0001) THEN
                           LOCAL_PATH = PATH_AT_DEPTH(D) ! 043007 ! WHY + 1?
                           P = GET_LEAF_OF_PATH(LOCAL_PATH)
                           Q = GET_PARENT_OF_LEAF(LOCAL_PATH)
!
! NOTE: TWO POINTS ALONG THE CONGESTION COST CURVE.
!
                           LOCAL_COAL_CONGESTION_COST(L_Q,L_P) = 
     +                           GET_COAL_TRANS_CONGEST_COST(Q,P,C+1,
     +                                                R_YEAR,R_MONTH)
!
                           CONGESTION_COST_INCREASE = 
     +                        MAX(0.0,
     +                           LOCAL_COAL_CONGESTION_COST(L_Q,L_P) - 
     +                                GET_COAL_TRANS_CONGEST_COST(Q,P,C,
     +                                                  R_YEAR,R_MONTH))
!
                           CURRENT_C_FOR_(L_Q,L_P) = 
     +                                       CURRENT_C_FOR_(L_Q,L_P) + 1
!                           DO L = 1, TOTAL_PC_FOR_(L_Q,L_P)
                           DO L = 1, GET_NUMBER_OF_J_TO_K_PATHS(Q,P)
!
! 050107. NOTE: GET INDEX FROM LINKS DATABASE. 
!         NOTE: GOING FROM PATH TO PC
!
!                              L_PC = PC_FOR_(L,L_Q,L_P)
!
                              L_PC = GET_PATH_FOR_J_TO_K(Q,P,L) +
     +                                       TOTAL_PC_FOR_INTERNAL_TRANS
!     
                              P_M = DAILY_TOTAL_REVERSE_POS(L_PC)
!                           
                              DAILY_TOTAL_DELIVERED_COST(L_PC) = 
     +                             DAILY_TOTAL_DELIVERED_COST(L_PC) +
     +                                          CONGESTION_COST_INCREASE
!                             P_M = L_PC 
                              POSITION_FOUND = .FALSE.
                              DOWHILE(P_M < TOTAL_PC .AND. 
     +                                             .NOT. POSITION_FOUND)
                                 P_N = DAILY_TOTAL_DELIVERED_POS(P_M+1)
                                 IF(DAILY_TOTAL_DELIVERED_COST(P_N) -
     +                              DAILY_TOTAL_DELIVERED_COST(L_PC) >=
     +                                                       0.001) THEN
                                    POSITION_FOUND = .TRUE.
                                    DAILY_TOTAL_DELIVERED_POS(P_M) 
     +                                                            = L_PC
!
! THIS ALLOWS FOR THE POSSIBILITY OF REUSING THE CURRENT PC
!                                    
!                                    IF(P_M == PC_COUNT) THEN
                                    IF(P_M == PC_COUNT) THEN
                                       INCREMENT_PC_COUNT = .FALSE.
                                    ENDIF
                                 ELSE
                                    DAILY_TOTAL_DELIVERED_POS(P_M) = P_N
                                    DAILY_TOTAL_REVERSE_POS(P_N) = P_M
!
                                    DAILY_TOTAL_DELIVERED_POS(P_M+1) = 
     +                                                             L_PC
                                    DAILY_TOTAL_REVERSE_POS(L_PC) = 
     +                                                           P_M + 1
!                                    
                                    P_M = P_M + 1
                                 ENDIF
                              END DO ! CHANGE POSITIONS

                              IF(DAILY_TOTAL_DELIVERED_POS(P_M) == 
     +                                 PC .AND. 
     +                                        .NOT. POSITION_FOUND) THEN
                                 INCREMENT_PC_COUNT = .FALSE.
                              ENDIF
                           END DO ! SET OF PATHS WITH COMMON LINK
!                        
                           LOCAL_PATH = 
     +                               GET_PARENT_PATH_OF_LEAF(LOCAL_PATH)
                        ENDIF
!
                     ENDDO ! DEPTH
                  ENDIF
! MUST ALSO CONSIDER LOSSES.      
! INC ARRAYS FOR PRICING.
               ENDIF
             END DO ! PC_COUNT PATH/SUPPLY CURVE ENUMERATION.
            ENDIF ! IF LP COAL MODEL OR PATHS ALGORITHM
!
            IF(DA == 1 .AND. R_MONTH == 1 .AND. R_YEAR == 1) THEN
! FROM,TO I,J            
               OPEN(10,FILE='DAILY_ARCLINK_FILE.CSV')
               WRITE(10,*) 
     +               'ENDPOINT,YEAR,MONTH,'//
     +               'DAY,LINK ID,FROM,TO,VOLUME,CAP_UTIL'
               DO I = 1, MAX_NODES
                  DO J = 1, MAX_NODES
                     L_I = GET_COAL_GROUP_INDEX(I)
                     L_J = GET_COAL_GROUP_INDEX(J)
                     IF(LINK_LOADING(I,J) <= .01) CYCLE
!                     IF(GET_TieID_INDEX(L_I,L_J) < 1) CYCLE
                     IF(LOCAL_COAL_TRANSPORT_QUANT(I,J) > .0001) THEN
                        CAP_UTIL = LINK_LOADING(I,J) /
     +                                   LOCAL_COAL_TRANSPORT_QUANT(I,J)
                     ELSE
                        CAP_UTIL = 0.
                     ENDIF
                     WRITE(10,*) 
     +                  END_POINT,',',
     +                  LOCAL_YEAR,',',
     +                  CL_MONTH_NAME(R_MONTH) ,',',
     +                  DA,',',
     +                  GET_TieID_INDEX(L_I,L_J),',',
     +                  L_I,',',
     +                  L_J,',',
     +                  LINK_LOADING(I,J),',',
     +                  CAP_UTIL
                  END DO 
               END DO
               CLOSE(10)
            ENDIF
! FROM,TO I,J            
            IF(DA == 1 .AND. R_MONTH == 1 .AND. R_YEAR == 1) THEN
               IF(R_MONTH == 1) THEN
                  OPEN(4433,FILE='MONTH_ARC_LINK_FILE.CSV')
                  WRITE(4433,*) 
     +             'ENDPOINT,YEAR,MONTH,LINK ID,FROM,TO,VOLUME,CAP_UTIL'
               ENDIF
               DO I = 1, MAX_NODES
                  DO J = 1, MAX_NODES
                     L_I = GET_COAL_GROUP_INDEX(I)
                     L_J = GET_COAL_GROUP_INDEX(J)
                     IF(MONTHLY_LINK_LOADING(I,J) <= .01) CYCLE
!                     IF(GET_TieID_INDEX(L_I,L_J) < 1) CYCLE
                     IF(LOCAL_COAL_TRANSPORT_QUANT(I,J) > .0001) THEN
                        CAP_UTIL = MONTHLY_LINK_LOADING(I,J) /
     +                              (FLOAT(DAYS_IN_MONTH)*
     +                                  LOCAL_COAL_TRANSPORT_QUANT(I,J))
                     ELSE
                        CAP_UTIL = 0.
                     ENDIF
                     WRITE(4433,*) 
     +                  END_POINT,',',
     +                  LOCAL_YEAR,',',
     +                  CL_MONTH_NAME(R_MONTH) ,',',
     +                  GET_TieID_INDEX(L_I,L_J),',',
     +                  L_I,',',
     +                  L_J,',',
     +                  MONTHLY_LINK_LOADING(I,J),',',
     +                  CAP_UTIL
                  END DO 
               END DO
               IF(R_MONTH == 12) THEN
                  CLOSE(4433)
                  CLOSE(4431)
               ENDIF
            ENDIF
!
! 051007.
!
            MARGINAL_COSTING_ROUTINE = .TRUE.
            IF(MARGINAL_COSTING_ROUTINE) THEN
               DO L_K = 1, MAX_NODES
                  IF(.NOT. IS_DEMAND_NODE(L_K)) CYCLE
                  LOCAL_PATH = LAST_PATH(L_K)
                  IF(LOCAL_PATH == 0) THEN
                     DAILY_PRICE(L_K) = LOCAL_COST_CURVE(L_K) +
     +                                    LOCAL_COAL_SHRINKAGE_COST(L_K)
                  ELSE
                     DEPTH = DEPTH_OF_LAST_PATH(L_K)
                     PATH_AT_DEPTH = 0
! ROOT IS IDENTIFIED FIRST
                     I = GET_NODE_OF_PATH(LOCAL_PATH)
                     L_I = GET_COAL_GROUP_POSITION(I)
! LOCAL_PATH = 0 IF D = DEPTH                     
                     DO  D = 1, DEPTH-1
                        PATH_AT_DEPTH(D) = LOCAL_PATH
                        LOCAL_PATH = GET_PARENT_PATH_OF_LEAF(LOCAL_PATH)
                     END DO
                     TEMP_R = LOCAL_COST_CURVE(L_I) +
     +                                    LOCAL_COAL_SHRINKAGE_COST(L_I)
                     DAILY_PRICE(L_I) = MAX(DAILY_PRICE(L_I),TEMP_R)
                     DO  D = DEPTH,2,-1
                        IF(D == DEPTH) THEN
                           Q = I
                        ELSE
                           Q = GET_LEAF_OF_PATH(PATH_AT_DEPTH(D))
                        ENDIF
                        L_Q = GET_COAL_GROUP_POSITION(Q)
                        P = GET_LEAF_OF_PATH(PATH_AT_DEPTH(D-1))
                        L_P = GET_COAL_GROUP_POSITION(P)
                        TEMP_R = TEMP_R + 
     +                        LOCAL_COAL_CONGESTION_COST(L_Q,L_P) +
     +                                LOCAL_COAL_TRANSPORT_RATE(L_Q,L_P)
                        DAILY_PRICE(L_P) = MAX(DAILY_PRICE(L_P),
     +                                                           TEMP_R)
                     ENDDO ! DEPTH
                  ENDIF ! LOCAL_PATH
               END DO ! MAX_NODES
            ENDIF ! MARGINAL COSTING ROUTINE
!            
! NEED TO INC INTERRUPTIBLE AS PART OF THE REPORT.            
!
            DO L_I = 1, MAX_NODES  
!
               IF(L_I == COAL_BASIS_POS) THEN
                  DAILY_DIFFER(L_I) = DAILY_PRICE(L_I)
               ELSEIF(DAILY_PRICE(L_I) > 0.000001) THEN
                  DAILY_DIFFER(L_I) = DAILY_PRICE(L_I) - 
     +                                       DAILY_PRICE(COAL_BASIS_POS)            
               ELSE
                  DAILY_DIFFER(L_I) = 0.00
               ENDIF
               MONTHLY_DIFFER(L_I) = MONTHLY_DIFFER(L_I) + 
     +                                        DAILY_DIFFER(L_I)
!
               IF(DAILY_DEMAND(L_I) < 0.001 .OR. 
     +                            DAILY_INTER_AVAIL(L_I) < 0.0001) CYCLE
               DAILY_INTER_USED(L_I) = 
     +                     MIN(DAILY_DEMAND(L_I),DAILY_INTER_AVAIL(L_I))
               DAILY_DEMAND(L_I) = MAX(0.0,
     +                        DAILY_DEMAND(L_I) - DAILY_INTER_USED(L_I))
            END DO
!           RECORD SOLUTION
!              DAILY COAL PRICES
!              DAILY SOURCE USAGE
!              DAILY LINK LOADING
!              DAILY SINK DEMAND (DONE)
            IF(YES_DAILY_COAL_SELL_BUY_REPORT) THEN
               WRITE(DAILY_COAL_PRICE_UNIT,REC=DAILY_COAL_PRICE_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(LOCAL_YEAR),
     +               CL_MONTH_NAME(R_MONTH),
     +               FLOAT(DA),
     +               (DAILY_PRICE(L_J),L_J=1, MAX_NODES)
               DAILY_COAL_PRICE_REC = DAILY_COAL_PRICE_REC + 1
               WRITE(DAILY_COAL_DIFFER_UNIT,REC=DAILY_COAL_DIFFER_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(LOCAL_YEAR),
     +               CL_MONTH_NAME(R_MONTH),
     +               FLOAT(DA),
     +               (DAILY_DIFFER(L_J),L_J=1, MAX_NODES)
               DAILY_COAL_DIFFER_REC = DAILY_COAL_DIFFER_REC + 1

               DO L_I = 1, MAX_NODES
                  GS = GET_COAL_GROUP_INDEX(L_I)
                  TEMP_NAME = GET_COAL_GROUP_NAME(GS)
                  WRITE(DAILY_COAL_SELL_BUY_UNIT,
     +                                      REC=DAILY_COAL_SELL_BUY_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(LOCAL_YEAR),
     +               CL_MONTH_NAME(R_MONTH),
     +               FLOAT(DA),
     +               TEMP_NAME,
     +               (DAILY_BOUGHT(L_I,L_J),L_J=1, MAX_NODES)
                  DAILY_COAL_SELL_BUY_REC = DAILY_COAL_SELL_BUY_REC + 1
                  WRITE(DAILY_COAL_LINK_UNIT,
     +                                       REC=DAILY_COAL_LINK_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(LOCAL_YEAR),
     +               CL_MONTH_NAME(R_MONTH),
     +               FLOAT(DA),
     +               TEMP_NAME,
     +               (LINK_LOADING(L_J,L_I),L_J=1, MAX_NODES)
                  DAILY_COAL_LINK_REC = DAILY_COAL_LINK_REC + 1

               ENDDO
            ENDIF
         END DO ! DAY IN MONTH
! MAKE ITS OWN SWITCH?         
         IF(YES_DAILY_COAL_SELL_BUY_REPORT) THEN
            DO L_I = 1, MAX_NODES 
!               
               IF(MONTHLY_VOLUME(L_I) > .0000001 .AND. 
     +                                MONTHLY_PRICE(L_I) > .000001) THEN
                  MONTHLY_PRICE(L_I) = MONTHLY_PRICE(L_I)/
     +                                               MONTHLY_VOLUME(L_I)
               ELSE
                  MONTHLY_PRICE(L_I) = 0.
               ENDIF
!
               IF(DAYS_IN_MONTH > 0) THEN
!               
                  IF(L_I == COAL_BASIS_POS) THEN
                     MONTHLY_DIFFER(L_I) = MONTHLY_PRICE(L_I)
                  ELSE
                     MONTHLY_DIFFER(L_I) = MONTHLY_DIFFER(L_I)/
     +                                              FLOAT(DAYS_IN_MONTH)
                  ENDIF
               ENDIF
            ENDDO
            IF(R_YEAR == 1) THEN
               IF(R_MONTH == 1) THEN
                  OPEN(4431,FILE='MONTH_ARCL_NODE_FILE.CSV')
                  WRITE(4431,*) 
     +             'ENDPOINT,YEAR,MONTH,NODE NAME,PRICE,BASIS,'//
     +             'DEMAND,IMPORT,EXPORT'
               ENDIF
               DO I = 1, MAX_NODES
!
                  GS = GET_COAL_GROUP_INDEX(I)
                  TEMP_NAME = GET_COAL_GROUP_NAME(GS)
!                     
                  WRITE(4431,*) 
     +                  END_POINT,',',
     +                  LOCAL_YEAR,',',
     +                  CL_MONTH_NAME(R_MONTH) ,',',
     +                  TEMP_NAME,',',
     +                  MONTHLY_PRICE(I),',',
     +                  MONTHLY_DIFFER(I),',',
     +                  MONTHLY_VOLUME(I),',',
     +                  MONTHLY_IMPORT(I),',',
     +                  MONTHLY_EXPORT(I)
!     
               ENDDO
!     
            ENDIF
            WRITE(MONTHLY_COAL_PRICE_UNIT,REC=MONTHLY_COAL_PRICE_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(LOCAL_YEAR),
     +               CL_MONTH_NAME(R_MONTH),
     +               (MONTHLY_PRICE(L_J),L_J=1, MAX_NODES)
            MONTHLY_COAL_PRICE_REC = MONTHLY_COAL_PRICE_REC + 1
! 042307.            
            WRITE(MONTHLY_COAL_DIFFER_UNIT,REC=MONTHLY_COAL_DIFFER_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(LOCAL_YEAR),
     +               CL_MONTH_NAME(R_MONTH),
     +               (MONTHLY_DIFFER(L_J),L_J=1, MAX_NODES)
            MONTHLY_COAL_DIFFER_REC = MONTHLY_COAL_DIFFER_REC + 1
         ENDIF
         IF(YES_MONTHLY_COAL_LINK_REPORT .OR. 
     +                            YES_MONTHLY_COAL_SELL_BUY_REPORT) THEN
            DO L_I = 1, MAX_NODES
               GS = GET_COAL_GROUP_INDEX(L_I)
               TEMP_NAME = GET_COAL_GROUP_NAME(GS)
               IF(YES_MONTHLY_COAL_SELL_BUY_REPORT) THEN
                  WRITE(MONTHLY_COAL_SELL_BUY_UNIT,
     +                                    REC=MONTHLY_COAL_SELL_BUY_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(LOCAL_YEAR),
     +               CL_MONTH_NAME(R_MONTH),
!     +               FLOAT(DA),
     +               TEMP_NAME,
     +               (MONTHLY_BOUGHT(L_I,L_J),L_J=1, MAX_NODES)
                  MONTHLY_COAL_SELL_BUY_REC = 
     +                                    MONTHLY_COAL_SELL_BUY_REC + 1
               ENDIF
               IF(YES_MONTHLY_COAL_LINK_REPORT) THEN 
                  WRITE(MONTHLY_COAL_LINK_UNIT,
     +                                       REC=MONTHLY_COAL_LINK_REC)
     +               PRT_ENDPOINT(),
     +               FLOAT(LOCAL_YEAR),
     +               CL_MONTH_NAME(R_MONTH),
!     +               FLOAT(DA),
     +               TEMP_NAME,
     +               (MONTHLY_LINK_LOADING(L_J,L_I),L_J=1, MAX_NODES)
                  MONTHLY_COAL_LINK_REC = MONTHLY_COAL_LINK_REC + 1
               ENDIF
            ENDDO
         ENDIF
!         CLOSE(10)
!
! PASS SUPPLY SOLUTION TO THE SUPPLY CURVES FOR NEXT MONTH
!            
         DO L_I = 1, MAX_NODES 
            GS = GET_COAL_GROUP_INDEX(L_I)
            IF(MAX_S_FOR_I(L_I) == 0) CYCLE
            DO S = 1, MAX_S_FOR_I(L_I)
!
! 040407. MAJOR REWRITE ON INDICES WITH OFFSET T.
!            
               T = S + FIRST_S_FOR_I(L_I) - 1
               CAPACITY_CURVE = MONTHLY_SUPPLY(L_I)
            ENDDO
            FIRST_S = FIRST_S_FOR_I(L_I)
            LAST_S = FIRST_S_FOR_I(L_I) + MAX_S_FOR_I(L_I) - 1
            TEMP_L = PUT_SUPPLY_CAPACITY_CURVES(GS,CAPACITY_CURVE,
     +                                          FIRST_S,LAST_S)
         ENDDO
!
         MONTHLY_COAL_PRICING = .TRUE.
!                  
!        SUMMARIZE MONTH
!        ACCUMULATE INFORMATION FOR ANNUAL REPORTING
      RETURN

      ENTRY GET_MONTHLY_COAL_PRICE_BY_STATE(R_GSP)

!
         IF(ALLOCATED(MONTHLY_NODE_PRICE)) THEN
            LOCAL_GSP = GET_NODE_FROM_STATE_POSITION(R_GSP)
            IF(LOCAL_GSP > 0) THEN
! CONVERT FROM $/MCF TO $/MMBTU: 1 MCF = 1.03 MMBTU
               GET_MONTHLY_COAL_PRICE_BY_STATE = 
     +                         MONTHLY_NODE_PRICE(LOCAL_GSP)*0.970873786
            ELSE
               GET_MONTHLY_COAL_PRICE_BY_STATE = -99.
            ENDIF
         ELSE
            GET_MONTHLY_COAL_PRICE_BY_STATE = -99.
         ENDIF
!      
      RETURN

!      ENTRY GregGetDaysDemand(R_ErrOrEnd,R_NodeCap,R_MAX_NODES)

!         R_ErrOrEnd = 0
!         GregGetDaysDemand = .TRUE. 
!!        
!         DO I = 1, R_MAX_NODES
!            L_I = DAILY_DEMAND_INDEX(I)
!            R_NodeCap(I) = DAILY_DEMAND(L_I) 
!         END DO
!      RETURN
!      
      END FUNCTION

!
      subroutine REAL4_CUSTOM_Sort(iSup,iPos,a)
!

! sorts iSup items into order based on values in key (1-based) array a;
! Shell sort adapted from the source on page 110 of
! SOFTWARE TOOLS IN PASCAL, by Kernighan & Plauger, Addison-Wesley 1981
      integer*4 i,j,k,p,q,Gap,iSup,iPos(iSup)
      real*4 a(iSup)
      logical*1 Increasing,Decreasing
!
      Increasing = .TRUE.
      Decreasing=.not.Increasing
      Gap=iSup/2
      do while(Gap>0)
         do i=Gap+1,iSup
           j=i-Gap
           do while(j>0)
             k=j+Gap
             p=iPos(j)
             q=iPos(k)
!            write(*,'(7i6)') iSup,Gap,i,j,k,p,q
             if((Increasing.and.(a(p)<=a(q))).or.
     +         (Decreasing.and.(a(p)>=a(q)))) then
               j=0 ! break the while loop (assign j=-1 for 0-based arrays)
             else ! interchange the indexing array values
               iPos(k)=p
               iPos(j)=q
             end if
             j=j-Gap
           end do
         end do
         Gap=Gap/2
      end do
      return
      end ! subroutine REAL4_CUSTOM_Sort

!
!        PROGRAM TO READ MULTI-TAB INFORMATION ON COAL_WEATHER DEMAND
!                 AND CONVERT TO BINARY FORMAT
!                       COPYRIGHT (C) 2001
!      ALL RIGHTS RESERVED M.S. GERBER & ASSOCIATES, INC.
!

!
      SUBROUTINE COAL_WEATHER_DEMAND_OBJECT
      use end_routine, only: end_program, er_message
!         

      use spindriftlib
      use prod_arrays_dimensions
      USE SIZECOM

      LOGICAL*1 SAVE_GW_FILE_EXISTS/.FALSE./,R_GW_FILE_EXISTS,
     +          R_GW_FILE_USED,SAVE_GW_FILE_USED/.FALSE./
      INTEGER*2   INUNIT,IREC,DELETE,LRECL/229/,get_BASE_YEAR(),
     +            R_LRECL,
     +            SAVE_TRANS_LOAD_RECORDS/0/,R_TRANS_LOAD_RECORDS,
     +            TEMP_YEAR,
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
     +            TRANSACTION_GROUP,
     +            CUSTOMER_GROUP
      INTEGER*4 IOS,IOS_BASE
      ALLOCATABLE ::
     +            TEMP_TRANS_CLASS_POINTER,
     +            TEMP_ASSET_CLASS_POINTER,
     +            TEMP_CUST_CLASS_POINTER
      CHARACTER*5 COAL_WEATHER_DEMAND_FILE,OVERLAY_FAMILY_NAME,BSYRLOAD
      CHARACTER*256 FILE_NAME
      CHARACTER*256 OUTPUT_DIRECTORY
      CHARACTER*256 BASE_FILE_DIRECTORY
      CHARACTER*152 MESSAGE
      LOGICAL*4 FILE_EXISTS
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER*1024 RECLN
! DECLARATION FOR SYSTEM FORECAST VARIABLES
      INTEGER*2 YEAR,UNIT_NUM/10/
!
! SIMULATION VARIABLES
!
      CHARACTER*1      
     +                     RECORD_IS_ACTIVE ! C 1
      CHARACTER*20
     +                     FORECAST_SOURCE ! C 20
      CHARACTER*32
     +                     MARKET_AREA_COMPANY_NAME ! C 32
      CHARACTER*5
     +                     MARKET_AREA_COMPANY_ID, ! C 5
     +                     REFERENCE_LOAD_NAME ! C 5
      INTEGER*2 
     +                     REFERENCE_LOAD_NUMBER ! I 2
      INTEGER*4
     +                     DATE_OF_FORECAST_ACTUAL_DATA ! I 4
      CHARACTER*9
     +                     DAY_OF_WEEK ! C 9
      REAL*4
     +                     HOUR_OF_DAY(24), ! 24x R 4
     +                     ASSET_CLASS_ID,
     +                     ASSET_CLASS_REV_ALLOC_VECTOR
      CHARACTER*50
     +                     COMMENT ! C 50
!
      CHARACTER*17 FILE_TYPE/'COAL_WEATHER Demand   '/
      CHARACTER*2 COAL_WEATHER_DEMAND_OL/'BC'/,R_COAL_WEATHER_DEMAND_OL
      LOGICAL*1 LAHEY_LF95
      CHARACTER*30 SCREEN_OUTPUT
!
      INTEGER*2 STUDY_BASE_YEAR
!
      SAVE STUDY_BASE_YEAR
!

!
!          ROUTINE TO CONVERT METAFILE FILES TO DIRECT-ACESS BINARY
!          COPYRIGHT (C) 1983, 84, 85  M.S. GERBER & ASSOCIATES, INC.
!

!
!          CREATE THE BASE FILE
!

      ENTRY COAL_WEATHER_DEMAND_MAKEBIN


      STUDY_BASE_YEAR = get_base_year()

      FILE_NAME = trim(BASE_FILE_DIRECTORY())//
     +                 "GWB"//trim(COAL_WEATHER_DEMAND_FILE())//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
!
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT =
     +                trim(FILE_TYPE)//'-'//COAL_WEATHER_DEMAND_FILE()
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_LOCATE_WRITE(16,30,COAL_WEATHER_DEMAND_FILE(),
     +                                                   ALL_VERSIONS,0)
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         ENDIF
!
         NUM_OF_BC_TRANS_CLASSES = 0
         MAX_BC_TRANS_CLASS_ID_NUM = 0
         NUM_OF_BC_ASSET_CLASSES = 0
         MAX_BC_ASSET_CLASS_ID_NUM = 0
         NUM_OF_BC_CUST_CLASSES = 0
         MAX_BC_CUST_CLASS_ID_NUM = 0
!
         TRANSACTION_GROUP = 1
!         
         ALLOCATE(   TEMP_TRANS_CLASS_POINTER(0:1023),
     +               TEMP_ASSET_CLASS_POINTER(0:1023),
     +               TEMP_CUST_CLASS_POINTER(0:1023))
         TEMP_TRANS_CLASS_POINTER = 0
         TEMP_ASSET_CLASS_POINTER = 0
         TEMP_CUST_CLASS_POINTER = 0
!
         SAVE_GW_FILE_EXISTS = .TRUE.
!         
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCGWYFC.BIN",
     +                      ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
!         
         ASSET_CLASS_ID = 0.
         ASSET_CLASS_REV_ALLOC_VECTOR = 0.
!
         SAVE_TRANS_LOAD_RECORDS = 0
!
         CUSTOMER_GROUP = 0
!
         IREC = 0
         READ(10,*) DELETE
         DO ! TABLES
            DO ! RECORDS
               READ(10,1000,IOSTAT=IOS) RECLN

               IF(RECLN(1:1) == '7' .OR. IOS /= 0) THEN ! END OF TABLE ! EXIT AT BOTTOM OF IF
!
                  EXIT ! LEAVE LOOP (GO TO NEXT TABLE OR EXIT ROUTINE)
!                  
               ENDIF
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,ERR=200) DELETE,
     +                     RECORD_IS_ACTIVE, ! C 1
     +                     FORECAST_SOURCE, ! C 20
     +                     MARKET_AREA_COMPANY_NAME, ! C 32
     +                     MARKET_AREA_COMPANY_ID, ! C 5
     +                     REFERENCE_LOAD_NAME, ! C 5
     +                     REFERENCE_LOAD_NUMBER, ! I 2
     +                     DATE_OF_FORECAST_ACTUAL_DATA, ! I 4
     +                     DAY_OF_WEEK, ! C 9
     +                     HOUR_OF_DAY, ! 24x R 4
     +                     COMMENT, ! C 50
     +                     TRANSACTION_GROUP
!
               IREC = IREC + 1
!               
!
               IF(TEMP_TRANS_CLASS_POINTER(TRANSACTION_GROUP) == 0) THEN
                  NUM_OF_BC_TRANS_CLASSES = NUM_OF_BC_TRANS_CLASSES + 1
                  MAX_BC_TRANS_CLASS_ID_NUM = MAX(
     +                      MAX_BC_TRANS_CLASS_ID_NUM,TRANSACTION_GROUP)
                  TEMP_TRANS_CLASS_POINTER(TRANSACTION_GROUP) = 1
               ENDIF
               IF(TEMP_ASSET_CLASS_POINTER(ASSET_CLASS_ID) == 0) THEN
                  NUM_OF_BC_ASSET_CLASSES = NUM_OF_BC_ASSET_CLASSES + 1
                  MAX_BC_ASSET_CLASS_ID_NUM = MAX(
     +                      MAX_BC_ASSET_CLASS_ID_NUM,ASSET_CLASS_ID)
                  TEMP_ASSET_CLASS_POINTER(ASSET_CLASS_ID) = 1
               ENDIF
               IF(TEMP_CUST_CLASS_POINTER(CUSTOMER_GROUP) == 0) THEN
                  NUM_OF_BC_CUST_CLASSES = NUM_OF_BC_CUST_CLASSES + 1
                  MAX_BC_CUST_CLASS_ID_NUM = MAX(
     +                      MAX_BC_CUST_CLASS_ID_NUM,CUSTOMER_GROUP)
                  TEMP_CUST_CLASS_POINTER(CUSTOMER_GROUP) = 1
               ENDIF
!
               WRITE(11,REC=IREC) DELETE,
     +                     RECORD_IS_ACTIVE, ! C 1
     +                     FORECAST_SOURCE, ! C 20
     +                     MARKET_AREA_COMPANY_NAME, ! C 32
     +                     MARKET_AREA_COMPANY_ID, ! C 5
     +                     REFERENCE_LOAD_NAME, ! C 5
     +                     REFERENCE_LOAD_NUMBER, ! I 2
     +                     DATE_OF_FORECAST_ACTUAL_DATA, ! I 4
     +                     DAY_OF_WEEK, ! C 9
     +                     HOUR_OF_DAY, ! 24x R 4
     +                     COMMENT, ! C 50
     +                     TRANSACTION_GROUP
            ENDDO ! READ RECORDS
            IF(IOS /= 0) EXIT
         ENDDO ! TABLES
         CLOSE(10)
      ELSE IF(INDEX(COAL_WEATHER_DEMAND_FILE(),'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "coal_obj:0001")
      ELSE
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCGWYFC.BIN",
     +                      ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
!
         SAVE_GW_FILE_EXISTS = .FALSE.
!         
      ENDIF
!      ENDFILE(11)
      CLOSE(11)
      IF(ALLOCATED(TEMP_TRANS_CLASS_POINTER))
     +      DEALLOCATE(    TEMP_TRANS_CLASS_POINTER,
     +                     TEMP_ASSET_CLASS_POINTER,
     +                     TEMP_CUST_CLASS_POINTER)
 !
      SAVE_TRANS_LOAD_RECORDS = IREC 
 !
      RETURN

  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from coal_obj SIID65'
      call end_program(er_message)


!          ROUTINE TO CREATE OVERLAY FILES
!          COPYRIGHT (C) 1984-88  M.S. GERBER & ASSOCIATES, INC.
!          COPYRIGHT (C) 1991-92  M.S. GERBER & ASSOCIATES, INC.




      ENTRY COAL_WEATHER_DEMAND_MAKEOVL(OVERLAY_FAMILY_NAME)

      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF         
      FILE_NAME=trim(OUTPUT_DIRECTORY())//"GWO"//
     +                               trim(OVERLAY_FAMILY_NAME)//".DAT"
      OPEN(10,FILE=FILE_NAME)
      READ(10,*) DELETE
      INUNIT = 12
      IF(COAL_WEATHER_DEMAND_OL == 'BC') THEN
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCGWYFC.BIN",
     +                                       ACCESS="DIRECT",RECL=LRECL)
         INUNIT = 11
      ENDIF
!     INQUIRE(UNIT=12,OPENED=FILE_EXISTS)
      FILE_NAME = trim(OUTPUT_DIRECTORY())//"OLGWYFC.BIN"
      OPEN(12,FILE=FILE_NAME,ACCESS="DIRECT", 
     +                           STATUS="UNKNOWN",RECL=LRECL,IOSTAT=IOS)
      IF(IOS /= 0) THEN
         CALL IOSTAT_MSG(IOS,MESSAGE)
         WRITE(4,*) trim(MESSAGE)
         er_message='in WEEKLY_DEMAND_MAKEOVL see WARNINGS messages'
         call end_program(er_message)
      ENDIF
      IREC = 0
      DELETE = 1
!
      NUM_OF_OL_TRANS_CLASSES = 0
      MAX_OL_TRANS_CLASS_ID_NUM = 0
      NUM_OF_OL_ASSET_CLASSES = 0
      MAX_OL_ASSET_CLASS_ID_NUM = 0
      NUM_OF_OL_CUST_CLASSES = 0
      MAX_OL_CUST_CLASS_ID_NUM = 0
!         
      ALLOCATE(TEMP_TRANS_CLASS_POINTER(0:1023),
     +         TEMP_ASSET_CLASS_POINTER(0:1023),
     +         TEMP_CUST_CLASS_POINTER(0:1023))
      TEMP_TRANS_CLASS_POINTER = 0
      TEMP_ASSET_CLASS_POINTER = 0
      TEMP_CUST_CLASS_POINTER = 0
!
!
!
      DO ! TABLES AND YEARS, COUNTING IS DONE INSIDE
         DO
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(RECLN(1:1) == '7') EXIT
            IREC = IREC + 1
            READ(INUNIT,REC=IREC,IOSTAT=IOS_BASE)  
     +                        DELETE,
     +                        RECORD_IS_ACTIVE, ! C 1
     +                        FORECAST_SOURCE, ! C 20
     +                        MARKET_AREA_COMPANY_NAME, ! C 32
     +                        MARKET_AREA_COMPANY_ID, ! C 5
     +                        REFERENCE_LOAD_NAME, ! C 5
     +                        REFERENCE_LOAD_NUMBER, ! I 2
     +                        DATE_OF_FORECAST_ACTUAL_DATA, ! I 4
     +                        DAY_OF_WEEK, ! C 9
     +                        HOUR_OF_DAY, ! 24x R 4
     +                        COMMENT, ! C 50
     +                        TRANSACTION_GROUP
            IF(IOS_BASE /= 0) EXIT
            IF(IOS == 0) THEN
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,ERR=200) DELETE,
     +                     RECORD_IS_ACTIVE, ! C 1
     +                     FORECAST_SOURCE, ! C 20
     +                     MARKET_AREA_COMPANY_NAME, ! C 32
     +                     MARKET_AREA_COMPANY_ID, ! C 5
     +                     REFERENCE_LOAD_NAME, ! C 5
     +                     REFERENCE_LOAD_NUMBER, ! I 2
     +                     DATE_OF_FORECAST_ACTUAL_DATA, ! I 4
     +                     DAY_OF_WEEK, ! C 9
     +                     HOUR_OF_DAY, ! 24x R 4
     +                     COMMENT, ! C 50
     +                     TRANSACTION_GROUP
            ENDIF
!
!
            IF(TEMP_TRANS_CLASS_POINTER(TRANSACTION_GROUP) == 0) THEN
               NUM_OF_OL_TRANS_CLASSES = NUM_OF_OL_TRANS_CLASSES + 1
               MAX_OL_TRANS_CLASS_ID_NUM = MAX(
     +                      MAX_OL_TRANS_CLASS_ID_NUM,TRANSACTION_GROUP)
               TEMP_TRANS_CLASS_POINTER(TRANSACTION_GROUP) = 1
            ENDIF
            IF(TEMP_ASSET_CLASS_POINTER(ASSET_CLASS_ID) == 0) THEN
               NUM_OF_OL_ASSET_CLASSES = NUM_OF_OL_ASSET_CLASSES + 1
               MAX_OL_ASSET_CLASS_ID_NUM = MAX(
     +                         MAX_OL_ASSET_CLASS_ID_NUM,ASSET_CLASS_ID)
               TEMP_ASSET_CLASS_POINTER(ASSET_CLASS_ID) = 1
            ENDIF
            IF(TEMP_CUST_CLASS_POINTER(CUSTOMER_GROUP) == 0) THEN
               NUM_OF_OL_CUST_CLASSES = NUM_OF_OL_CUST_CLASSES + 1
               MAX_OL_CUST_CLASS_ID_NUM = MAX(
     +                          MAX_OL_CUST_CLASS_ID_NUM,CUSTOMER_GROUP)
               TEMP_CUST_CLASS_POINTER(CUSTOMER_GROUP) = 1
            ENDIF
!
            WRITE(12,REC=IREC) DELETE,
     +                         RECORD_IS_ACTIVE, ! C 1
     +                         FORECAST_SOURCE, ! C 20
     +                         MARKET_AREA_COMPANY_NAME, ! C 32
     +                         MARKET_AREA_COMPANY_ID, ! C 5
     +                         REFERENCE_LOAD_NAME, ! C 5
     +                         REFERENCE_LOAD_NUMBER, ! I 2
     +                         DATE_OF_FORECAST_ACTUAL_DATA, ! I 4
     +                         DAY_OF_WEEK, ! C 9
     +                         HOUR_OF_DAY, ! 24x R 4
     +                         COMMENT, ! C 50
     +                         TRANSACTION_GROUP
         ENDDO ! RECORDS IN TABLES
         IF(IOS_BASE /= 0) EXIT
      ENDDO ! END TABLES
      CLOSE(10)
      CLOSE(12)
      IF(COAL_WEATHER_DEMAND_OL == 'BC') CLOSE(11)
      COAL_WEATHER_DEMAND_OL = 'OL'
      IF(ALLOCATED(TEMP_TRANS_CLASS_POINTER))
     +      DEALLOCATE(    TEMP_TRANS_CLASS_POINTER,
     +                     TEMP_ASSET_CLASS_POINTER,
     +                     TEMP_CUST_CLASS_POINTER)
      RETURN
!
!

      ENTRY RESET_COAL_WEATHER_DEMAND_OL

         COAL_WEATHER_DEMAND_OL = 'BC'
         SAVE_GW_FILE_USED = .FALSE.
      RETURN
!

      ENTRY RETURN_COAL_WEATHER_DEMAND_OL(
     +                                 R_COAL_WEATHER_DEMAND_OL,R_LRECL)

         R_COAL_WEATHER_DEMAND_OL = COAL_WEATHER_DEMAND_OL
         R_LRECL = LRECL
      RETURN

      ENTRY DOES_COAL_WEATHER_DEMAND_FILE_EXIST(R_GW_FILE_EXISTS)

         R_GW_FILE_EXISTS = SAVE_GW_FILE_EXISTS
      RETURN

      ENTRY GET_COAL_WEATHER_DEMAND_RECORDS(R_TRANS_LOAD_RECORDS)

         R_TRANS_LOAD_RECORDS = SAVE_TRANS_LOAD_RECORDS
      RETURN

      ENTRY GW_FILE_USED_THIS_ENDPOINT(R_GW_FILE_USED)

         R_GW_FILE_USED = SAVE_GW_FILE_USED
         SAVE_GW_FILE_USED = .TRUE.
      RETURN

      ENTRY OPEN_COAL_WEATHER_DEMAND_FILE

         OPEN(UNIT_NUM,
     +        FILE=trim(OUTPUT_DIRECTORY())//COAL_WEATHER_DEMAND_OL//
     +        "GWYFC.BIN",ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      RETURN
!

      ENTRY CLOSE_COAL_WEATHER_DEMAND_FILE

         CLOSE(UNIT_NUM)
      RETURN

      ENTRY RETURN_GW_GROUP_INFO(R_NUM_OF_TRANS_CLASSES,
     +                           R_MAX_TRANS_CLASS_NUM,
     +                           R_NUM_OF_ASSET_CLASSES,
     +                           R_MAX_ASSET_CLASS_NUM,
     +                           R_NUM_OF_CUST_CLASSES,
     +                           R_MAX_CUST_CLASS_NUM)

         IF(COAL_WEATHER_DEMAND_OL == 'OL') THEN
            R_NUM_OF_TRANS_CLASSES = NUM_OF_OL_TRANS_CLASSES
            R_MAX_TRANS_CLASS_NUM = MAX_OL_TRANS_CLASS_ID_NUM
            R_NUM_OF_ASSET_CLASSES = NUM_OF_OL_ASSET_CLASSES
            R_MAX_ASSET_CLASS_NUM = MAX_OL_ASSET_CLASS_ID_NUM
            R_NUM_OF_CUST_CLASSES = NUM_OF_OL_CUST_CLASSES
            R_MAX_CUST_CLASS_NUM = MAX_OL_CUST_CLASS_ID_NUM
         ELSE
            R_NUM_OF_TRANS_CLASSES = NUM_OF_BC_TRANS_CLASSES
            R_MAX_TRANS_CLASS_NUM = MAX_BC_TRANS_CLASS_ID_NUM
            R_NUM_OF_ASSET_CLASSES = NUM_OF_BC_ASSET_CLASSES
            R_MAX_ASSET_CLASS_NUM = MAX_BC_ASSET_CLASS_ID_NUM
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
!             PROGRAM TO MANAGE SHORT-TERM DEMAND FORECASTS
!           BY TRANSACTION GROUP ASSET CLASS AND CUSTOMER GROUP
!
!                       COPYRIGHT (C) 2001
!          ALL RIGHTS RESERVED M.S. GERBER & ASSOCIATES, INC.
!

!
      FUNCTION MANAGE_COAL_WEATHER_DEMAND_FORECASTS()
!

!
!
! LOCAL DATA LIST
!

      use spindriftlib
      use prod_arrays_dimensions
      use globecom
      USE SIZECOM
      implicit none
      SAVE

     +               MANAGE_COAL_WEATHER_DEMAND_FORECASTS,
     +               GET_GW_LOAD
      LOGICAL*1      DOES_GW_FILE_EXIST
      INTEGER*2      YR,MAKER_TABLES/0/,DELETE,
     +               R_MONTH,MAKER_MONTHS,
     +               R_YEAR,
     +               R_LOAD_UNIT,
     +               WK,
     +               LOCAL_WEEKS,LOCAL_YEARS,NUM_SCEN_VAR,IREC
!
!
      CHARACTER*1      
     +                     RECORD_IS_ACTIVE ! C 1
      CHARACTER*20
     +                     FORECAST_SOURCE ! C 20
      CHARACTER*32
     +                     MARKET_AREA_COMPANY_NAME ! C 32
      CHARACTER*5
     +                     MARKET_AREA_COMPANY_ID, ! C 5
     +                     REFERENCE_LOAD_NAME ! C 5
      INTEGER*2 
     +                     J,
     +                     REFERENCE_LOAD_NUMBER, ! I 2
     +                     TRANSACTION_GROUP,
     +                     COAL_WEATHER_DEMAND_RECORDS,
     +                     I_HOUR,
     +                     LOCAL_MONTH,
     +                     LOCAL_DAY,
     +                     LOCAL_YEAR,
     +                     MARKET_AREA_COMPANY_ID_NUM,
     +                     MARKET_AREA_LOOKUP,
     +                     R_ID_NUM,R_DAY
      INTEGER*4
     +                     DATE_OF_FORECAST_ACTUAL_DATA, ! I 4
     +                     R_DAILY_I4(24)
      CHARACTER*9
     +                     DAY_OF_WEEK ! C 9
      REAL*4
     +                     HOUR_OF_DAY(24),GW_LOAD(:,:) ! RECORDS X 24 x R 4
      ALLOCATABLE :: 
     +                     GW_LOAD
      CHARACTER*50
     +                     COMMENT ! C 50
!
!     
! INPUT DATA LIST
!
      INTEGER*4         VALUES_2_ZERO

      INTEGER*2         SCENARIO_YEAR,
     +                  TABLE,
     +                  SCENARIO_INDEX,
     +                  LOCAL_MAX_NUM_MARKET_AREAS/0/,
     +                  LOCAL_MAX_HOURS/24/,
     +                  LOCAL_MAX_DAYS/31/,
     +                  LOCAL_MAX_MONTHS/12/,
     +                  LOCAL_MAX_YEARS/4/, ! UP'ED FOR SCOTT 01/08/04
     +                  MIN_YEAR/9999/,
     +                  MAX_YEAR/0/
      REAL*4
     +                  R_MONTHLY_SLOPE,R_MONTHLY_INTERCEPT
      CHARACTER*20
     +                  HYDRO_VARIABLES
      INTEGER*2
     +                  GW_INDEX(:,:,:,:) ! MARKET_ID, LOCAL_MONTH, LOCAL_DAY, LOCAL_YEAR
      ALLOCATABLE :: 
     +                  GW_INDEX
!      
!      SAVE GW_LOAD,GW_INDEX
!
! END DATA DECLARATIONS      
!

!         
         CALL DOES_COAL_WEATHER_DEMAND_FILE_EXIST(DOES_GW_FILE_EXIST)
         IF(DOES_GW_FILE_EXIST) THEN
            CALL OPEN_COAL_WEATHER_DEMAND_FILE
!
            CALL GET_COAL_WEATHER_DEMAND_RECORDS(
     +                                      COAL_WEATHER_DEMAND_RECORDS)
!
            LOCAL_WEEKS = 52
            NUM_SCEN_VAR = 3

            IF( ALLOCATED(GW_LOAD) )
     +                DEALLOCATE(GW_LOAD)
            ALLOCATE(
     +            GW_LOAD(LOCAL_MAX_HOURS,COAL_WEATHER_DEMAND_RECORDS))


            GW_LOAD = 0.

!
!
!     DETERMINE LOCAL_MAX_NUM_MARKET_AREAS, MIN_YEAR, MAX_YEAR
!
            DO IREC = 1, COAL_WEATHER_DEMAND_RECORDS
               READ(10,REC=IREC) DELETE,
     +                     RECORD_IS_ACTIVE, ! C 1
     +                     FORECAST_SOURCE, ! C 20
     +                     MARKET_AREA_COMPANY_NAME, ! C 32
     +                     MARKET_AREA_COMPANY_ID, ! C 5
     +                     REFERENCE_LOAD_NAME, ! C 5
     +                     REFERENCE_LOAD_NUMBER, ! I 2
     +                     DATE_OF_FORECAST_ACTUAL_DATA, ! I 4
     +                     DAY_OF_WEEK, ! C 9
     +                     HOUR_OF_DAY, ! 24x R 4
     +                     COMMENT, ! C 50
     +                     TRANSACTION_GROUP
!
               IF(RECORD_IS_ACTIVE == 'F') CYCLE
!               
!               
               LOCAL_MONTH = DATE_OF_FORECAST_ACTUAL_DATA/10000
               LOCAL_DAY = (DATE_OF_FORECAST_ACTUAL_DATA - 
     +                     LOCAL_MONTH*10000) / 100
!               
               LOCAL_YEAR = DATE_OF_FORECAST_ACTUAL_DATA -
     +                LOCAL_MONTH * 10000 - LOCAL_DAY * 100 
!
!     CALENDAR CORRECT YEAR
!     
               IF(LOCAL_YEAR <= 50) THEN
                     LOCAL_YEAR = LOCAL_YEAR + 2000 - get_BASE_YEAR()
                  ELSE
                     LOCAL_YEAR = LOCAL_YEAR + 1900 - get_BASE_YEAR()   
               ENDIF
               
               IF(LOCAL_YEAR > MAX_YEAR) MAX_YEAR = LOCAL_YEAR
               IF(LOCAL_YEAR < MIN_YEAR) MIN_YEAR = LOCAL_YEAR
               
               MARKET_AREA_COMPANY_ID_NUM =
     +                        MARKET_AREA_LOOKUP(MARKET_AREA_COMPANY_ID)
               LOCAL_MAX_NUM_MARKET_AREAS = 
     +                     MAX(LOCAL_MAX_NUM_MARKET_AREAS,
     +                              MARKET_AREA_COMPANY_ID_NUM+1)
!     
            ENDDO ! RECORDS
!
            IF( ALLOCATED(GW_INDEX) )
     +                DEALLOCATE(GW_INDEX)
            IF(MAX_YEAR <= 0 .OR. LOCAL_MAX_NUM_MARKET_AREAS <= 0) THEN
            ENDIF
            ALLOCATE(
     +            GW_INDEX(0:LOCAL_MAX_NUM_MARKET_AREAS-1,
     +               LOCAL_MAX_MONTHS,
     +               LOCAL_MAX_DAYS,
     +               LOCAL_MAX_YEARS))


            GW_INDEX = 0

            J = 0
            DO IREC = 1, COAL_WEATHER_DEMAND_RECORDS
               READ(10,REC=IREC) DELETE,
     +                     RECORD_IS_ACTIVE, ! C 1
     +                     FORECAST_SOURCE, ! C 20
     +                     MARKET_AREA_COMPANY_NAME, ! C 32
     +                     MARKET_AREA_COMPANY_ID, ! C 5
     +                     REFERENCE_LOAD_NAME, ! C 5
     +                     REFERENCE_LOAD_NUMBER, ! I 2
     +                     DATE_OF_FORECAST_ACTUAL_DATA, ! I 4
     +                     DAY_OF_WEEK, ! C 9
     +                     HOUR_OF_DAY, ! 24x R 4
     +                     COMMENT, ! C 50
     +                     TRANSACTION_GROUP
!
               IF(RECORD_IS_ACTIVE == 'F' .OR. 
     +                                    RECORD_IS_ACTIVE == 'N') CYCLE
               J = J + 1
!               
               DO I_HOUR = 1, 24
                  GW_LOAD(I_HOUR,J) = HOUR_OF_DAY(I_HOUR)
               ENDDO
!               
               LOCAL_MONTH = DATE_OF_FORECAST_ACTUAL_DATA/10000
               LOCAL_DAY = (DATE_OF_FORECAST_ACTUAL_DATA - 
     +                      LOCAL_MONTH*10000) / 100
               LOCAL_YEAR = DATE_OF_FORECAST_ACTUAL_DATA -
     +                LOCAL_MONTH * 10000 - LOCAL_DAY * 100
!
!     CALENDAR CORRECT YEAR
!     
               IF(LOCAL_YEAR <= 50) THEN
                     LOCAL_YEAR = LOCAL_YEAR + 2000 - get_BASE_YEAR()
                  ELSE
                     LOCAL_YEAR = LOCAL_YEAR + 1900 - get_BASE_YEAR()   
               ENDIF

               MARKET_AREA_COMPANY_ID_NUM =
     +            MARKET_AREA_LOOKUP(MARKET_AREA_COMPANY_ID)
     
               GW_INDEX(MARKET_AREA_COMPANY_ID_NUM,LOCAL_MONTH,
     +                  LOCAL_DAY,LOCAL_YEAR) = J
                                   
!     
            ENDDO ! RECORDS
            CALL CLOSE_COAL_WEATHER_DEMAND_FILE
            MANAGE_COAL_WEATHER_DEMAND_FORECASTS = .TRUE.
         ENDIF
!
      RETURN
!      

!
      ENTRY GET_GW_LOAD(R_ID_NUM,R_YEAR,R_MONTH,R_DAY,R_DAILY_I4,
     +                   R_MONTHLY_SLOPE,R_MONTHLY_INTERCEPT)
!


         IF( (R_YEAR >= MIN_YEAR .AND. R_YEAR <= MAX_YEAR) .AND. 
     +      R_MONTH <= LOCAL_MAX_MONTHS .AND.
     +      R_DAY <= LOCAL_MAX_DAYS .AND.
     +      R_ID_NUM <= LOCAL_MAX_NUM_MARKET_AREAS-1) THEN
     
                                
            J = GW_INDEX(R_ID_NUM,R_MONTH,R_DAY,R_YEAR) 
         
            IF(J > 0) THEN
                  R_DAILY_I4(1) = INT(GW_LOAD(1,J))
                  R_DAILY_I4(2) = INT(GW_LOAD(2,J))
                  R_DAILY_I4(3) = INT(GW_LOAD(3,J))
                  R_DAILY_I4(4) = INT(GW_LOAD(4,J))
                  R_DAILY_I4(5) = INT(GW_LOAD(5,J))
                  R_DAILY_I4(6) = INT(GW_LOAD(6,J))
                  R_DAILY_I4(7) = INT(GW_LOAD(7,J))
                  R_DAILY_I4(8) = INT(GW_LOAD(8,J))
                  R_DAILY_I4(9) = INT(GW_LOAD(9,J))
                  R_DAILY_I4(10) = INT(GW_LOAD(10,J))
                  R_DAILY_I4(11) = INT(GW_LOAD(11,J))
                  R_DAILY_I4(12) = INT(GW_LOAD(12,J))
                  R_DAILY_I4(13) = INT(GW_LOAD(13,J))
                  R_DAILY_I4(14) = INT(GW_LOAD(14,J))
                  R_DAILY_I4(15) = INT(GW_LOAD(15,J))
                  R_DAILY_I4(16) = INT(GW_LOAD(16,J))
                  R_DAILY_I4(17) = INT(GW_LOAD(17,J))
                  R_DAILY_I4(18) = INT(GW_LOAD(18,J))
                  R_DAILY_I4(19) = INT(GW_LOAD(19,J))
                  R_DAILY_I4(20) = INT(GW_LOAD(20,J))
                  R_DAILY_I4(21) = INT(GW_LOAD(21,J))
                  R_DAILY_I4(22) = INT(GW_LOAD(22,J))
                  R_DAILY_I4(23) = INT(GW_LOAD(23,J))
                  R_DAILY_I4(24) = INT(GW_LOAD(24,J))
                  R_MONTHLY_SLOPE = 1.0
                  R_MONTHLY_INTERCEPT = 0.0
            ENDIF
            GET_GW_LOAD = .TRUE.
         ENDIF      
      RETURN
!      
      END
! 060107. 
!-----
! B2LogRF is the base-2 logarithm of the max allowable bounds-relaxation factor
      recursive subroutine GregsRoutine(
     +  R_GS_POS,R_GT_POS,
     +  R_DAILY_DEMAND,
     +  R_NODE_PRICE,
     +  R_NODE_LIM,
     +  R_NODE_HARD_LIM,
     +  R_NODE_DAY_LIM,
     +  R_NODE_MIN_LIM,
     +  R_MARG_PRICE,
     +  R_AVE_PRICE,
     +  R_LINK_QUANTITY,
     +  R_NODE_QUANTITY,
     +  R_NODE_SUR_DEF_QUANTITY,
     +  R_LINK_PRICE,
     +  R_LINK_LIM,
     +  R_COMMOD_LINK_COST)
      logical*1 AmortizeUE/.true./,Skeptical/.false./,Feasible,
     +  B1/.true./,B0/.false./,GregGetDaysDemand,TEMP_L,
     +  GET_COAL_TRANSPORT_CONGESTION
      integer*4 ErrOrEnd,j
      integer*2 nNode,nLink,nCCSegs,nDays,iLink,iSeg, ! last 3 for example only
     +  iRelaxBounds,
     +  NodeID(:),NodeType(:), ! coded 1 for Supply, -1 for Demand, else 0
     +  LinkID(:),LinkNdID(:,:),LinkNdSN(:,:), ! sequential (serial) numbers
     +  R_ENDPOINT,R_YEAR,R_MONTH,R_DAY,
     +  R_GS_POS(*),i,k,R_GT_POS(*),Inpfilenum,
     +  PrtDetail
      real*4 PriceSoLR, ! price at all demand nodes of Supply of Last Resort
     +  PriceDoLR, ! price at all supply nodes of Demand of Last Resort
     +  NodeQtyLoB(:),NodeQtyLim(:),NodeQtyHardLim(:),
     +  NodeQty(:),NodeQtyOfLR(:),NodePri(:),NodeMgC(:),
     +  LinkCap(:),LinkChg(:),LinkEff(:),LinkAvC(:),LinkMgC(:),
     +  LinkQty(:),LinkQtySeg(:,:),LinkFunc(:,:,:),LinkQOF(:),
     +  LinkAvQSeg(:,:),LinkExpQty(:),
     +  R_DAILY_DEMAND(*),
     +  R_NODE_PRICE(*),
     +  R_NODE_LIM(*),
     +  R_LINK_LIM(*),
     +  R_COMMOD_LINK_COST(*),
     +  R_NODE_HARD_LIM(*),
     +  R_NODE_DAY_LIM(*),
     +  R_NODE_MIN_LIM(*),
     +  R_MARG_PRICE(*),
     +  R_AVE_PRICE(*),
     +  R_LINK_QUANTITY(*),
     +  R_NODE_QUANTITY(*),
     +  R_NODE_SUR_DEF_QUANTITY(*),
     +  R_LINK_PRICE(*),
     +  CONGEST_VECTOR(10) ! nCCSegs
      character*255 InpCsvFN(4),TempName
      character*27 NodeName(:)
      allocatable ! listed in order of declaration
     +  NodeID,NodeType,
     +  LinkID,LinkNdID,LinkNdSN,
     +  NodeQtyLoB,NodeQtyLim,NodeQtyHardLim,
     +  NodeQty,NodeQtyOfLR,NodePri,NodeMgC,
     +  LinkCap,LinkChg,LinkEff,LinkAvC,LinkMgC,LinkQty,LinkQtySeg,
     +  LinkQOF,LinkFunc,LinkAvQSeg,LinkExpQty,NodeName
      save AmortizeUE, ! listed in order of declaration
     +  PriceSolr,PriceDolr,
     +  NodeID,NodeType,
     +  LinkID,LinkNdID,LinkNdSN,
     +  NodeQtyLoB,NodeQtyLim,NodeQtyHardLim,
     +  NodeQty,NodeQtyOfLR,NodePri,NodeMgC,
     +  LinkCap,LinkChg,LinkEff,LinkAvC,LinkMgC,LinkQty,LinkQtySeg,
     +  LinkQOF,LinkFunc,LinkAvQSeg,LinkExpQty,NodeName, 
     +  nNode,nLink,nCCSegs ! amazingly required for the allocate()s to work
! 011908
!      AmortizeUE= AmortizeUEparm
      call OpenGNWOutFiles(B1) ! unit 4 must be open for error reporting
      InpFileNum = 4
      call OpenGNWInpFiles(InpCsvFN,InpFileNum)
!      call OpenGNWInpFiles()
      call GetNodeCount(nNode)
      call AllocateGNWNodeArrays()
      call GetLinkCount(nNode,nLink,nCCSegs, ! defines nLink and nCCSegs
     +  NodeID,NodeType,
     +  NodeName)
!
      call AllocateGNWLinkArrays()
      call ReadGNWDNodeOrder()
      call ReadGNWInpData(AmortizeUE,nNode,nLink,nCCSegs,
     +  PriceSoLR,PriceDoLR,
     +  NodeID,NodeType,
     +  LinkID,LinkNdID,LinkNdSN,
     +  NodeQtyLoB,NodeQtyLim,NodeQtyHardLim,NodeQty,NodePri,
     +  LinkCap,LinkChg,LinkEff,LinkFunc,
     +  NodeName)
      call WriteStrAt(5,5,'Midas-formatted .CSV-files read')
      call WriteStrAt(5,6,' ') ! needed since PCX begins without a LineFeed
      call AllocateGNWOptimizingArrays()
!
      call OpenGNWOutFiles(B0) ! open other output files
      call InitReportGNWFiles(NodeID,NodeType,LinkID)
!
! 060107. ONLY ONE CALL PER DAY.
      nDays=0
      do ! optimize network usage for one day per iteration
! 060107. CALL THE DEMAND FILE. MAKE SURE ORDER IS RIGHT.
        call GetDaysDemand(ErrOrEnd,NodeQty)
!        TEMP_L = GregGetDaysDemand(ErrOrEnd,NodeQty,nNode)
        if(ErrOrEnd/=0) exit ! error or EoF reading demand data into NodeQty
        nDays=nDays+1
!
      ! here you are free to manipulate the contents of arrays declared above,
      ! but not nNode,nLink,nCCSegs,NodeID,NodeType,LinkNdID,LinkNdSN
!        NodeQtyLim(58) = 0.371 ! during testing (Alaska Prudhoe Bay)
!
        call MinGNWCosts(nCCSegs,
     +    NodeID,NodeType,
     +    LinkID,LinkNdID,LinkNdSN,
     +    NodeQtyLoB,NodeQtyLim,NodeQtyHardLim,
     +    NodeQty,NodePri,PriceSoLR,PriceDoLR,
     +    LinkCap,LinkChg,LinkEff,LinkFunc,
     +    NodeName)
        do iRelaxBounds=1,10
!cc       call WriteLpInputs() ! using only internal variables and arrays
          call SolveGNWLpUsingPcx(Feasible,
     +      nCCSegs,LinkNdSN,NodeQtyHardLim)
        ! after 20070615, always expect Feasible with no bound on last segment;
        ! after 20080103, always expect Feasible when using AmortizeUE
          if(Feasible.or.AmortizeUE) exit ! else try again with relaxed upper-bounds
        end do
        if(iRelaxBounds>10)call ps(1,'bounds relaxed, yet not Feasible')
        if(.not.Feasible)call ps(1,'demand destroyed, yet not Feasible')
!       call WriteStrAt(10,5,'solved LP using PCX.exe')
        call ReportGNWSolutionDetails(nCCSegs,Skeptical,
     +    NodeID,NodeType,LinkID,LinkNdSN,
     +    NodeQtyLoB,NodeQtyLim,NodeQtyHardLim,
     +    NodeQty,NodeQtyOfLR,NodePri,NodeMgC,
     +    LinkCap,LinkChg,LinkEff,LinkAvC,LinkMgC,
     +    LinkQty,LinkQtySeg,LinkFunc,LinkQOF,NodeName)
      ! for example only, accumulate sum of segment usage:
        do iLink=1,nLink
          do iSeg=1,nCCSegs
            LinkAvQSeg(iSeg,iLink)=
     +      LinkAvQSeg(iSeg,iLink)+LinkQtySeg(iSeg,iLink)
!           write(4,'(2i4,2f12.2,a)') iLink,iSeg,LinkQtySeg(iSeg,iLink),
!    +        LinkAvQSeg(iSeg,iLink),' LAQ'
          end do
        end do
      end do ! until ErrOrEnd/=0 reading Demand data
!
    
      call ReleaseGNWNodeArrays()
      call ReleaseGNWLinkArrays()
      call ReleaseOtherGNWArrays()
      call CloseGNWInpFiles()
      call CloseGNWOutFiles()
      return ! subroutine GregsRoutine
!-----
      entry GregMonthCall( R_ENDPOINT,
     +                     R_YEAR,
     +                     R_MONTH,
     +                     R_NODE_PRICE,
     +                     R_LINK_LIM,
     +                     R_COMMOD_LINK_COST)
!     +                     R_NODE_LIM)
         do i = 1, nNode
             NodePri(i) = R_NODE_PRICE(NodeID(i))
         end do
         do i = 1, nLink
            LinkCap(i) = R_LINK_LIM(I)
            LinkChg(i) = R_COMMOD_LINK_COST(I)
            TEMP_L = GET_COAL_TRANSPORT_CONGESTION(I,CONGEST_VECTOR)
         enddo
      return
!-----
      entry GregDayCall( R_ENDPOINT,R_YEAR,
     +                   R_MONTH,R_DAY,
     +                   R_GS_POS,
     +                   R_GT_POS,
     +                   R_NODE_LIM,
     +                   R_NODE_MIN_LIM,
     +                   R_NODE_DAY_LIM,
     +                   R_NODE_HARD_LIM,
     +                   R_DAILY_DEMAND,
     +                   R_MARG_PRICE,
     +                   R_AVE_PRICE,
     +                   R_LINK_QUANTITY,
     +                   R_NODE_QUANTITY,
     +                   R_NODE_SUR_DEF_QUANTITY,
     +                   R_LINK_PRICE)

        do i = 1, nNode
          NodeQty(i) = R_DAILY_DEMAND(NodeID(i))
! PUT A DAILY LIMIT ON R_NODE_LIM USING link attached to the supply node
          NodeQtyLim(i) = MAX(0.0,MIN(R_NODE_LIM(NodeID(i)),
     +                                R_NODE_DAY_LIM(NodeID(i))))
          NodeQtyHardLim(i) = R_NODE_HARD_LIM(NodeID(i))
          NodeQtyLoB(i) = R_NODE_MIN_LIM(NodeID(i))

        end do
        do i = 1, nLink
            LinkCap(i) = LinkCap(i)
        enddo
        call GetReportGNWDim(R_ENDPOINT,R_YEAR,R_MONTH,R_DAY)
        call MinGNWCosts(nCCSegs,
     +    NodeID,NodeType,
     +    LinkID,LinkNdID,LinkNdSN,
     +    NodeQtyLoB,NodeQtyLim,NodeQtyHardLim,
     +    NodeQty,NodePri,PriceSoLR,PriceDoLR,
     +    LinkCap,LinkChg,LinkEff,LinkFunc,
     +    NodeName)
        do iRelaxBounds=1,10
!cc       call WriteLpInputs() ! using only internal variables and arrays
          call SolveGNWLpUsingPcx(Feasible,
     +      nCCSegs,LinkNdSN,NodeQtyHardLim)
        ! after 20070615, always expect Feasible with no bound on last segment;
        ! after 20080103, always expect Feasible when using AmortizeUE
          if(Feasible .or. AmortizeUE) exit ! else try again with relaxed upper-bounds
!          if(Feasible) exit ! else try again with relaxed upper-bounds
        end do
        if(iRelaxBounds>10)call ps(1,'bounds relaxed, yet not Feasible')
        if(.not.Feasible)call ps(1,'demand destroyed, yet not Feasible')
!       call WriteStrAt(10,5,'solved LP using PCX.exe')
!        WRITE(4,*) "NUMBER OF NODES ",nNode
        call ReportGNWSolutionDetails(nCCSegs,Skeptical,
     +    NodeID,NodeType,LinkID,LinkNdSN,
     +    NodeQtyLoB,NodeQtyLim,NodeQtyHardLim,
     +    NodeQty,NodeQtyOfLR,NodePri,NodeMgC,
     +    LinkCap,LinkChg,LinkEff,LinkAvC,LinkMgC,
     +    LinkQty,LinkQtySeg,LinkFunc,LinkQOF,NodeName,PriceSoLR)
        do i = 1, nNode
             k = NodeId(i)
             if(k < 1) cycle
             k = R_GS_POS(k)
             if(k < 1) cycle
             R_MARG_PRICE(k) = NodeMgC(i)
             R_AVE_PRICE(k) = NodePri(i) ! check with Thielke
             R_NODE_QUANTITY(k) = NodeQty(i)
             R_NODE_SUR_DEF_QUANTITY(k) = NodeQtyOfLR(i)
        end do
        do i = 1, nLink
             k = LinkId(i)
             if(k < 1) cycle
             k = R_GT_POS(k)
             if(k < 1) cycle
            R_LINK_QUANTITY(k) = LinkQty(i)
            R_LINK_PRICE(k) = LinkMgc(i)
        enddo
      return ! entry GregDayCall
!-----
      entry FirstGNWCallGreg()
         CALL GetCOALNodeBaseName(TempName)
         InpCsvFN(1) = TRIM(TempName) ! 'GNBNEWIN.DAT'  ! 01 NodeName
         CALL GetCOALLinkBaseName(TempName)
         InpCsvFN(2) = TRIM(TempName) ! 'GTBG0507.DAT'  ! 02 network links
         CALL GetCOALSupplyBaseName(TempName)
         InpCsvFN(3) = TRIM(TempName) ! 'GSBZ0314.DAT'  ! 03 supply-node prices and limits
         InpCsvFN(4) = 'COALNWDmd.CSV'  ! 04 demand-node MMCF by day, one day per record
         call OpenGNWOutFiles(B1) ! unit 4 must be open for error reporting
         InpFileNum = 3
         call OpenGNWInpFiles(InpCsvFN,InpFileNum)
!         call OpenGNWInpFiles(InpCsvFN)
         call GetNodeCount(nNode)
         call AllocateGNWNodeArrays()
         call GetLinkCount(nNode,nLink,nCCSegs, ! deinfes nLink and nCCSegs
     +      NodeID,NodeType,
     +      NodeName)
!
         call AllocateGNWLinkArrays()
         call ReadGNWInpData(AmortizeUE,nNode,nLink,nCCSegs,
     +      PriceSoLR,PriceDoLR,
     +      NodeID,NodeType,
     +      LinkID,LinkNdID,LinkNdSN,
     +      NodeQtyLoB,NodeQtyLim,NodeQtyHardLim,NodeQty,NodePri,
     +      LinkCap,LinkChg,LinkEff,LinkFunc,
     +      NodeName)
         call WriteStrAt(5,5,'Midas-formatted .CSV-files read')
         call WriteStrAt(5,6,' ') ! needed since PCX begins without a LineFeed
         call AllocateGNWOptimizingArrays()
!
         call OpenGNWOutFiles(B0) ! open other output files
         call InitReportGNWFiles(NodeID,NodeType,LinkID)
!
      return ! entry FirstGNWCallGreg
!-----
      entry LastGNWCallGreg
         call ReleaseGNWNodeArrays()
         call ReleaseGNWLinkArrays()
         call ReleaseOtherGNWArrays()
         call CloseGNWInpFiles()
         call CloseGNWOutFiles()
      return
!-----
      entry AllocateGNWNodeArrays
      allocate(NodeID  (nNode))
      allocate(NodeType(nNode))
      allocate(NodeQtyLoB(nNode)) ! min quantity at supply MMCF/D
      allocate(NodeQtyLim(nNode)) ! max quantity at supply MMCF/D
      allocate(NodeQtyHardLim(nNode)) ! max quantity at supply MMCF/D
      allocate(NodeQty (nNode)) ! quantity demand or supply MMCF/D
      allocate(NodeQtyOfLR(nNode)) ! to/from LastResort; useful only if AmortizeUE
      allocate(NodePri (nNode)) ! $/MCF
      allocate(NodeMgC (nNode)) ! $/MCF
      allocate(NodeName(nNode))
      NodeQtyLoB=-1.0     ! default implying N.A.
      NodeQtyLim=-1.0     ! default implying N.A.
      NodeQtyHardLim=-1.0 ! default implying N.A.
      NodeQty=0.0
      NodePri=0.0
      return
!-----
      entry ReleaseGNWNodeArrays
      deallocate(
     +  NodeID,NodeType,NodeQtyLoB,NodeQtyLim,NodeQtyHardLim,
     +  NodeQty,NodeQtyOfLR,NodePri,NodeMgC,NodeName)
      return
!-----
      entry AllocateGNWLinkArrays
      allocate(LinkID (nLink))
      allocate(LinkCap(nLink)) ! A-to-B capacity (nominal 100%)
      allocate(LinkChg(nLink)) ! A-to-B commodity charge at 0%
      allocate(LinkEff(nLink)) ! A-to_B fractional retention of inlet quantity
      allocate(LinkAvC(nLink)) ! weighted average cost $/MCF
      allocate(LinkMgC(nLink)) ! marginal cost $/MCF
      allocate(LinkNdID(0:1,nLink)) ! A-to-B IDs
      allocate(LinkNdSN(0:1,nLink)) ! A-to-B IDs' serial numbers (contiguous)
      allocate(LinkQty(nLink)) ! A-to-B quantity at inlet in MMCF
      allocate(LinkQOF(nLink)) ! A-to-B over-limit quantity at inlet in MMCF
      allocate(LinkExpQty(nLink)) ! economic expansion quantity
      allocate(LinkQtySeg(nCCSegs,nLink)) ! segment usage in MMCF
      allocate(LinkAvQSeg(nCCSegs,nLink)) ! segment usage in MMCF, accumulated
      allocate(LinkFunc(nCCSegs,0:2,nLink)) ! segment x(EoS fract cap),y($/MCF),DeltaX
      LinkAvQSeg=0.0
      return
!-----
      entry ReleaseGNWLinkArrays
      deallocate(LinkID,LinkNdID,LinkNdSN,LinkCap,LinkChg,
     +  LinkEff,LinkAvC,LinkMgC,LinkQty,LinkQtySeg,LinkQOF,LinkFunc,
     +  LinkAvQSeg,LinkExpQty)
      return
!-----
      end ! subroutine GregsRoutine
!-----
!-----
      character*4 function R4toA4(r)
      real*4 r,t
      logical*1 IntegerForm
      character*4 Str4 ! 12345678
      character*8 Str8 ! S0.mEsnn
!
      IntegerForm=.false.
      t=abs(r)
      if((t==0.0).or.(t>=0.95)) then ! exponent sign s is positive (hence redundant)
        if(r<0) t=10.0*t ! allow room for the leading sign
        if(t<=9989.49) then ! nint(r) is no more than 4 chars
          write(Str4,'(i4)') nint(r)
          IntegerForm=.true.
        end if
      end if
      if(.not.IntegerForm) then ! nint(r) is 0 or large
        write(Str8,'(e8.1)') r*0.1 ! to account for skipping the leading '0.'
        if(Str8(7:7)/='0') then ! exponent has 2 NZ digits => t is very large or small
          Str4=Str8(4:5)//Str8(7:8) ! mEnn
          if(Str8(6:6)=='-') Str4(2:2)='n' ! to signify 'E-'
          if(r<0.0) Str4(1:1)='-' ! overwrite m to show the number is negative
        else ! exponent has at most 1 NZ digit
          Str4=Str8(5:5)//Str8(8:8) ! En
          if(Str8(6:6)=='-') Str4(1:1)='n' ! to signify 'E-'
          Str4=Str8(4:4)//Str4 ! prefix the mantissa digit m
          Str4=Str8(1:1)//Str4 ! prefix the sign char S
        end if
      end if
      R4toA4=Str4
      end ! function R4toA4
!-----
      subroutine AppendFixed9(u,r) ! suitable for percentages through 100.0
      integer*4 u
      real*4 r
      if(r<0.0) then ! display negative arg as if zero, but signed
        write(u,'( a9,",")',advance='no') ' -0      '
      elseif(r==0.0) then ! display arg as zero
        write(u,'( a9,",")',advance='no') '  0      '
      elseif(r<=999.95) then
        write(u,'( f9.5,",")',advance='no') r
      else
        write(u,'( f9.4,",")',advance='no') r
      end if
      end

      subroutine AppendInt9(u,r)
      integer*4 u,i
      real*4 r
!
    ! preclude field-width (and integer*4) overflow
      if    (r<-998998.4e3) then
        i=-998998998
      elseif(r> 998998.4e3) then
        i= 998998998
      else
        i=nint(r)
      end if
      write(u,'( i9,",")',advance='no') i
      end
!-----
      subroutine AppendF9p3(u,r) ! suitable only for limited-range r
      integer*4 u
      real*4 r
      character*9 s9
!
      if(abs(r)<0.0005) then ! use a minimal-noise placeholder
        s9='     .0  '
      else
        write(s9,'(f9.3)') r
      end if
      write(u,'( a9,",")',advance='no') s9
      end
!-----
      subroutine AssignR4Array(n,Sour,Dest)
      integer*2 n,i
      real*4 Sour(*),Dest(*)
!
      Dest(1:n)=Sour(1:n)
      end
!-----
      subroutine SkipLines(u,n)
      integer*4 u
      integer*2 n,i
!
      do i=1,n
        read(u,*,err=100,end=100)
      end do
  100 continue
      end
!-----
      subroutine ScaleBy(n,a,c)
    ! multiply each value in array a by constant c
      integer*2 n,i
      real*4 c,a(*)
!
      do i=1,n
        a(i)=a(i)*c
      end do
      end
!-----
      subroutine PairwiseMult(n,a,b)
    ! multiply each value in array a by corresponding value in array b
      integer*2 n,i
      real*4 a(*),b(*)
!
      do i=1,n
        a(i)=a(i)*b(i)
      end do
      end

      integer*2 function IndexIn(i2Array,n,ThisItem)
      integer*2 n,ThisItem,i,i2Array(n)
!
      do i=1,n

        if(i2Array(i)==ThisItem) exit
      end do

    ! caller must decide how to handle the case of index not found
      IndexIn=i
      end

      real*4 function FuncGNWAMtx(j,i)
      integer*4 j,i
      real*4 r4
!
      call ReturnGNWAMtx(j,i,r4)
      FuncGNWAMtx=r4
      end
!-----
      logical*1 function Negat(c)
      character*1 c
!
      if ((c/='T').and.(c/='Y')) then
        Negat=.true.
      else
        Negat=.false.
      end if
      end
!-----
      subroutine SortByAscendingKey(nItems,MajorKey,MinorKey,Val)
!     sorts nItems items of Val array per ascending order of Key arrays,
!     where the result is to vary most rapidly by MajorKey, then by MinorKey
      integer*4 nItems,i,j,k,Gap,MajorKey(*),MinorKey(*),iHold,iHalf
      integer*8 jFull,kFull,FullKey,ShiftFactor/1000000000/ ! <1073741824=2**30
      real*4 Val(*),rHold
      FullKey(iHalf)=MinorKey(iHalf)*ShiftFactor+MajorKey(iHalf)
    ! FullKey could also be cast as real*8
!
      Gap=nItems/2
      do while(Gap>0)
        do i=Gap+1,nItems
          j=i-Gap
          do while(j>0)
            k=j+Gap

            if(FullKey(j)<=FullKey(k)) then
              j=0 ! break the while loop (assign j=-1 for 0-based arrays)
            else ! interchange pairs within ~Key vectors, and within Val
              iHold=MajorKey(j)
              MajorKey(j)=MajorKey(k)
              MajorKey(k)=iHold
              iHold=MinorKey(j)
              MinorKey(j)=MinorKey(k)
              MinorKey(k)=iHold
              rHold=Val(j)
              Val(j)=Val(k)
              Val(k)=rHold

            end if
            j=j-Gap
          end do
        end do
        Gap=Gap/2
      end do

      end ! subroutine SortByAscendingKey
!-----
      real*8 function DotProd8(x,y,n)
      integer*4 n,i
      real*4 x(n),y(n)
      real*8 xySum
!
      xySum=0.0d0
      do i=1,n
        xySum=xySum+x(i)*y(i)
      end do
      DotProd8=xySum
      end
!-----
      character*1 function QuestionIf(p)
      logical p
      if(p) then
        QuestionIf='?'
      else
        QuestionIf=' '
      end if
      end
!-----
      logical*1 function CharIsNumeric(c)
      character*1 c
      CharIsNumeric=(('0'<=c).and.(c<='9')).or.
     +  (c=='.').or.
     +  (c=='+').or.
     +  (c=='-')
      end
!-----
!-----
!-----
      recursive subroutine DerivePriceFromSources(Cyclic,DbgU,
     +  CallerDepth,iDNod,nNode,SupnLk,nLinkInto,
     +  NdLkdInto,LkUsdInto,CycleOrgD,LinkQty,LinkEff,
     +  LinkAvC,LinkMgC,
     +  NodePri,NodeMgC
     +  )
      logical*1 Cyclic
      integer*4 DbgU
      integer*2 CallerDepth,iDNod,nNode,SupnLk,
     +  CallDepth,MinTestDepth,CycOrg,InfCycOrg,
     +  iNode,iLink,kLink,nLinkInto(nNode),
     +  NdLkdInto(SupnLk,nNode),
     +  LkUsdInto(SupnLk,nNode),CycleOrgD(nNode)
      real*4 DNodQty,WtdAvgPri,SourMgCst,SupSourMC,Ration,InfRation,
     +  LinkQty(*),LinkEff(*),
     +  LinkAvC(*),LinkMgC(*),NodePri(nNode),NodeMgC(nNode)
    ! local variables placed on the stack for each call:
    !   CallDepth,iNode,iLink,kLink,DNodQty,WtdAvgPri,
    !   SourMgCst,SupSourMC
      save CycOrg,InfCycOrg,MinTestDepth,InfRation
!
      if(Cyclic) return ! back through the recursive callers to CallerDapth 0
      if(CallerDepth==0) InfCycOrg=0 ! implying 'no cycling yet detected'
      CallDepth=CallerDepth+1
      if(CallDepth>48) call ps(1,
     +  'cycling => excessive depth in DPFS')
      if(CycleOrgD(iDNod)==0) CycleOrgD(iDNod)=-CallDepth ! => 'once visited with unknown NodePri'
    ! weight sourcing nodes' prices by the net Qty into the node at iDNod
      DNodQty=0.0
      do kLink=1,nLinkInto(iDNod)
        iLink=LkUsdInto(kLink,iDNod)
        DNodQty=DNodQty+LinkQty(iLink)*LinkEff(iLink) ! Qty net of losses
      end do
      ! 20080122 expedient:  ignore noisy nodes causing cycling
        do kLink=1,nLinkInto(iDNod)
          iLink=LkUsdInto(kLink,iDNod)
          if(LinkQty(iLink)<=0.0) cycle ! skip links with 0 weight
          Ration=LinkQty(iLink)*LinkEff(iLink)/DNodQty ! weight by inlet Qty net of losses
          iNode=NdLkdInto(kLink,iDNod)
          CycOrg=CycleOrgD(iNode)
          if(CycOrg<0) then ! this is the second pass through iNode
            CycOrg=-CycOrg
            if(InfCycOrg==0) then
              InfCycOrg=CycOrg
              InfRation=0.999 ! preclude zeroing LinkQty if only 1 Source node is active
            else
              if(InfCycOrg>CycOrg) InfCycOrg=CycOrg
            end if
            MinTestDepth=2*CallDepth-InfCycOrg ! CD+(CD-InfCycOrg)
            CycleOrgD(iNode)=CycOrg ! open gate to retaining InfRation, since
          ! this is the first indication that a cycle has completed (and
          ! therefore that all nodes in deeper cycles are in this same loop)
          elseif(CycOrg>0) then
            if((InfRation>Ration).and.(NodePri(iNode)<=0.0))
     +        InfRation=Ration ! retain minimum for the cycle
            if((CallDepth>=MinTestDepth).and.(Ration<1.01*InfRation))
     +        then
              write(DbgU,'(4i4,i3,f7.4,f12.4,a)') CallDepth,iDNod,iNode,
     +          iLink,kLink,Ration,LinkQty(iLink),
     +          ' fraction & Qty zeroed in DPFS to break cycle'

              LinkQty(iLink)=0.0 ! preclude cycling through iLink at later calls

              Cyclic=.true.
              return

            end if

          end if
        end do

      WtdAvgPri=0.0
      SupSourMC=0.0
      if(DNodQty>0.0) then
        do kLink=1,nLinkInto(iDNod)
          iLink=LkUsdInto(kLink,iDNod)
          if(LinkQty(iLink)<=0.0) cycle ! skip links with 0 weight
          iNode=NdLkdInto(kLink,iDNod)
          Ration=LinkQty(iLink)*LinkEff(iLink)/DNodQty ! weight by inlet Qty net of losses
          if(NodePri(iNode)<=0.)call DerivePriceFromSources(Cyclic,DbgU,
     +      CallDepth,iNode,nNode,SupnLk,nLinkInto,
     +      NdLkdInto,LkUsdInto,CycleOrgD,LinkQty,LinkEff,
     +      LinkAvC,LinkMgC,
     +      NodePri,NodeMgC
     +      )
          if(Cyclic) return
!         SourMgCst=(NodeMgC(iNode)+LinkMgC(iLink)/1000.)/LinkEff(iLink) ! $/MCF
          SourMgCst=NodeMgC(iNode)/LinkEff(iLink)+LinkMgC(iLink)/1000. ! $/MCF
          if(SupSourMC<SourMgCst) SupSourMC=SourMgCst
          WtdAvgPri=WtdAvgPri+Ration*
     +      (LinkAvC(iLink)+NodePri(iNode)*1000./LinkEff(iLink)) ! Taudit  0.2427
!    +      (LinkAvC(iLink)+NodePri(iNode)*1000.)                ! Taudit 22.5342
!    +      (LinkAvC(iLink)/LinkEff(iLink)+NodePri(iNode)*1000.) ! Taudit 22.4479
!    +      (LinkAvC(iLink)+NodePri(iNode)*1000.)/LinkEff(iLink) ! Taudit  0.1553
          ! LinkAvC is $/MMCF delivered (escalated for losses), ...
          ! but NodePri is the outlet price into iLink before losses
        end do ! kLink
      end if
      NodePri(iDNod)=WtdAvgPri/1000. ! to retain $/MCF on NodePri
      NodeMgC(iDNod)=SupSourMC
      CycleOrgD(iDNod)=0 ! end scrutiny for cycling through iDNod
      end ! recursive subroutine DerivePriceFromSources
!-----
!Small: NumSegs 2
!Small: (113+2*10) 133 needed if_NumSegs<10 in COALNetwk
      recursive subroutine COALNetwk(AmortizeUEparm,Skeptical,
     +  nNode,nLink,nCCSegs,
     +  NodeID,NodeType,
     +  LinkID,LinkNdID,LinkNdSN,
     +  NodeQtyLoB,NodeQtyLim,NodeQtyHardLim,NodeQty,NodeQtyOfLR,
     +  NodePri,NodeMgC,PriceSoLR,PriceDoLR,
     +  LinkCap,LinkChg,LinkEff,LinkAvC,LinkMgC,
     +  LinkQty,LinkQtySeg,LinkAvQSeg,LinkExpQty,
     +  LinkFunc,LinkQOF,NodeName,InpCsvFN)
!
    ! declarations for interface variables and arrays
      logical*1 AmortizeUEparm,Skeptical
      integer*2
     +  nNode,nLink,nCCSegs,
     +  NodeID  (    *),
     +  NodeType(    *),
     +  LinkID  (    *),
     +  LinkNdID(0:1,*),
     +  LinkNdSN(0:1,*)
      real*4 YearSpan,
     +  PriceSoLR,
     +  PriceDoLR,
     +  NodeQtyLoB(  *),
     +  NodeQtyLim(  *),
     +  NodeQtyHardLim(*),
     +  NodeQty (    *),
     +  NodeQtyOfLR( *),
     +  NodePri (    *),
     +  NodeMgC (    *),
     +  LinkCap (    *),
     +  LinkChg (    *),
     +  LinkEff (    *),
     +  LinkAvC(     *),
     +  LinkMgC(     *),
     +  LinkQty (    *),
     +  LinkQOF (    *),
     +  LinkExpQty(  *),
     +  LinkQtySeg(10,*),
     +  LinkAvQSeg(10,*),
     +  LinkFunc(10,0:2,*)
      character*27
     +  NodeName(    *)
      character*255 InpCsvFN(4)
!
    ! declarations for internal variables and arrays
      character*(3995+10*10) OneLine
      character*255 DirPath/' '/,ErrMsg
      character*255 FileName
      character*50 LinkName,FiveLabels(5)
      character*29 ConsDesc
      character*27 ThisNdName,DNodName(:),VarDesc(:)
      character*25 SDAccums/' Supply & Demand accums, '/
      character*12 OtherField(3:(113+2*10))
      character*9 MonthStr,BaseFileName
      character*8 UbString
      character*4 IDString,EndpointStr,YearStr,DayStr,R4toA4,A4Equiv(:)
      CHARACTER*9 CL_MONTH_NAME(14)/
     +  'January  ','February ','March    ','April    ',
     +  'May      ','June     ','July     ','August   ',
     +  'September','October  ','November ','December ',
     +  'Annual   ','Fiscal Yr'/
      character*1 cDummy,FirstCh,AComma/','/,Quotes/'"'/,
     +  AlarmCh,RelChar(0:2)/'=','<','>'/,QuestionIf
      allocatable DNodName ! fails under F77L3 if placed much lower
      logical*1 B0/.false./,B1/.true./,AmortizeUE,OpenDbgU,
     +  ignored,MinimizingObj,AllBitsZero,Negat,AnyPruned,
     +  HeaderAbsent,Feasible, ! InTolerance,OrigBounds,
     +  OnTgtCol,OnTgtRow,Cyclic
      integer*1 bDummy,ReVec(:) ! rejected by LF95 for unknown reason
      integer*2 i,j,k,iSeg,jSeg,iInpCsv,nInpCsvOfInt,
     +  iNode,kNode,mNode,iSNod,nSNod,iDNod,nDNod,
     +  kLink,nNZPSour,nZPNodesRem,nZPNodesOrg,SrcSN,LinkDir,nSegUsd,
!     +  SrcSN,LinkDir,nSegUsd,
     +  iLink,iLnkP1,mLink,PreviLink,nBidLink,nZcpLink,n0Suppliers,
     +  NodeDir,nLkAtNd,SupnLkAtNd,nLkFrNd,nLkToNd,SupDFRank,SupDFNdSN,
     +  nRCMiter,PrtDetail,nLinkTarg,nNodeTarg,r_PrtDetail,
     +  nSegsOI(-1:1),nLkOfType(-1:1),LinkType(:),nPriorSegs(:),
     +  nAltForThisSorD,nDActiveForS,nLinkFrTo(0:1),LinkFwdRev(0:1),
     +  LkUsdFrom(:),nLinkInto(:),NdLkdInto(:,:),LkUsdInto(:,:),
     +  TNofVar(:),NodeSrcNip(:,:),NodeSNofDFRank(:),CycleOrgD(:),
     +  IndexIn,LengNB,CI_index,
     +  R_ENDPOINT,R_YEAR,R_MONTH,R_DAY
      integer*4 i4,ZeroIfExtant,
     +  InpU,InpCsvU(4),
     +  DbgU/9004/,SnvU/9005/,LkvU/9006/,NacU/9007/,NmcU/9008/,
     +  OutU,OrgCentiSec,EndCentiSec,ExecET,
     +  mReq,iReq,nVar,nTVar,jVar,jCol,ErrOrEnd,LinkNdBi(:),
     +  iUbV,nUbV,jUbVec(:),
     +  iLbV,nLbV,jLbVec(:), ! okay to pass even if never allocated
     +  nLZ,DWord,jBit,MinObj/1/,WrtDetail/1/,
     +  zbColOfNze(:),zbRowOfNze(:),zbColHold(:),zbRowHold(:),
     +  ConsMtSize,ConsMtSLim,iNze,
     +  OneBasedCol,OneBasedRow,zbCol,zbRow,
     +  IndexGap,GlbIndex,LubIndex,MidIndex,PCX_ret_code,PCX_main,jField
      real*8 r8,ObjectvEF(0:2),AccumOfDir(-1:1),LnkQCEF(0:1,0:2),
     +  SpyQCEF(0:1,0:2),DmdQCEF(0:1,0:2), ! Qty:Cst,Exc:FSg:Sum
     +  DotProd8
      real*4 Near0Qty/0.001/,f4,r4,QtySum,CstSum,xPrev,yPrev,SupLinkQty,
     +  CapExpCostRate,CostCriterion,MarginalOamDecr,ExpDollarGain,
     +  PriorExpMult,ExpansionMultiple,ExpansionQty,OverflowFactor,
     +  QtyFSg,SNodPri,SourMgCst,DelivdPri,SimAvgPri,SupSourMC,
     +  SpAlloQty,SpAlloCst,LkAlloCst,SpQtyNode(:),LkCstNode(:),
     +  xFrCap(10),yDpMCF(10),xDelta(10),
     +  NodeSupplyLtd,ThisQtySeg(10),
     +  UbVOrg(:),UbValu(:),LbValu(:), ! okay to pass even if never allocated
     +  OrigUB,cVect(:),bVect(:),xVect(:),ConsMtxNze(:),ConsEHold(:),
     +  NodeSrcQty(:,:),NodeSrcCst(:,:),LinkChgOrg(:),LnkQtyP(:),
     +  LinkExpCst(:),LinkExpLim(:),LinkCCRF(:),DFValue(:),
     +  FuncGNWAMtx,ACMvalue,RCMvalue
     +  ,xVsm(:),AMat(:,:), ! useful only with SMsolver
     +  REAL4_ONE/1.0/,ESCALATED_MONTHLY_VALUE
      allocatable VarDesc,A4Equiv,
     +  LinkNdBi,LinkType,nPriorSegs,
     +  LkUsdFrom,nLinkInto,NdLkdInto,LkUsdInto,
     +  TNofVar,NodeSNofDFRank,CycleOrgD,
     +  cVect,bVect,xVect,ReVec,
     +  jUbVec,UbValu,UbVOrg,
     +  jLbVec,LbValu,ConsMtxNze,ConsEHold,
     +  zbColOfNze,zbRowOfNze,zbColHold,zbRowHold,
     +  NodeSrcNip,NodeSrcQty,NodeSrcCst,LinkChgOrg,LnkQtyP,
     +  LinkExpCst,LinkExpLim,LinkCCRF,DFValue,
     +  xVsm,AMat,LkCstNode,SpQtyNode
      save InpU,InpCsvU,OneLine,
     +  YearStr,MonthStr,DayStr,AmortizeUE,MinimizingObj,Cyclic, ! OrigBounds,
     +  OrgCentiSec,EndCentiSec,ExecET,
     +  iReq,mReq,jVar,nVar,iUbV,nUbV,iLbV,nLbV,
     +  SupDFRank,SupDFNdSN,PrtDetail,
     +  mNode,iSNod,nSNod,nDNod,nTVar,SrcSN,iNode,SupnLkAtNd,WrtDetail,
     +  Near0Qty,NodeSNofDFRank,ThisNdName,nSegsOI,nLkOfType,mLink,
     +  LinkType,LkUsdFrom,nLinkInto,NdLkdInto,LkUsdInto,LinkNdBi,
     +  nPriorSegs,nAltForThisSorD,nDActiveForS,TNofVar,CycleOrgD,
     +  ReVec,cVect,bVect,xVect,VarDesc,A4Equiv,
     +  jUbVec,UbValu,UbVOrg,
     +  jLbVec,LbValu,ConsMtxNze,zbColOfNze,zbRowOfNze,
     +  ConsMtSize,ConsMtSLim,
     +  NodeSrcNip,NodeSrcQty,NodeSrcCst,LinkChgOrg,LnkQtyP,
     +  LinkExpCst,LinkExpLim,LinkCCRF,DFValue,
     +  ObjectvEF,LkCstNode,SpQtyNode
!
      return ! recursive subroutine COALNetwk (declarations only)
!-----
      entry OpenGNWInpFiles(InpCsvFN,nInpCsvOfInt)
      do iInpCsv=1,nInpCsvOfInt ! open CSV files for reading
        InpCsvU(iInpCsv)=0 ! indicating not (yet) opened
        InpU=900+iInpCsv
        FileName=trim(InpCsvFN(iInpCsv))
        ZeroIfExtant=-1 ! OpenFmFile caller will handle absence
        call OpenFmFile(InpU,0,2,ZeroIfExtant,B0,DirPath,FileName)
        write(DbgU,'(3i6,1x,a)') iInpCsv,InpU,ZeroIfExtant,
     +    trim(FileName)
        if(ZeroIfExtant/=0) goto 901
!cc     if(iInpCsv==2) call AlignCols(InpU)
!       call SkipLines(InpU,1) ! the "9," line
        InpCsvU(iInpCsv)=InpU
      end do
      return
  901 call ps(1,'Error opening file in old-mode:  '//trim(FileName))
!-----
      entry OpenGNWOutFiles(OpenDbgU)
      if(OpenDbgU) then
        FileName='COALNetwk_right.dbg'
        ZeroIfExtant=-1 ! OpenFmFile caller will handle absence
        call OpenFmFile(DbgU,2,2,ZeroIfExtant,B0,DirPath,FileName)
        if(ZeroIfExtant/=0) goto 902
      else
        FileName='COALNWSNV.CSV' ! output:  volume from each Supply node
        ZeroIfExtant=-1 ! OpenFmFile caller will handle absence
        call OpenFmFile(SnvU,2,2,ZeroIfExtant,B0,DirPath,FileName)
        if(ZeroIfExtant/=0) goto 902
!
        FileName='COALNWLkV.CSV' ! output:  volume through each Link
        ZeroIfExtant=-1 ! OpenFmFile caller will handle absence
        call OpenFmFile(LkvU,2,2,ZeroIfExtant,B0,DirPath,FileName)
        if(ZeroIfExtant/=0) goto 902
!
        FileName='COALNWAvC.CSV' ! output:  weighted average cost at each node
        ZeroIfExtant=-1 ! OpenFmFile caller will handle absence
        call OpenFmFile(NacU,2,2,ZeroIfExtant,B0,DirPath,FileName)
        if(ZeroIfExtant/=0) goto 902
!
        FileName='COALNWMgC.CSV' ! output:  marginal cost at each node
        ZeroIfExtant=-1 ! OpenFmFile caller will handle absence
        call OpenFmFile(NmcU,2,2,ZeroIfExtant,B0,DirPath,FileName)
        if(ZeroIfExtant/=0) goto 902
      end if
      return ! entry OpenGNWOutFiles
  902 call ps(1,'Error opening file in replace-mode:  '//
     +  trim(FileName))
      return
!-----
      entry CloseGNWInpFiles
      do iInpCsv=1,4 ! 4 ! close input files
        InpU=InpCsvU(iInpCsv)
        close(InpU)
      end do
      return
!-----
      entry CloseGNWOutFiles
      close(SnvU)
      close(LkvU)
      close(NacU)
      close(NmcU)
      close(DbgU)
      return
!-----
      entry GetNodeCount(nNode)
      InpU=InpCsvU(1) ! Node parameters
      nSNod=0
      nDNod=0
      nNode=0
      NodeDir=-2
      do
        read(InpU,'(a)',err=410,end=410) OneLine
        write(DbgU,'(i4,1x,a)') InpU,trim(OneLine)
        read(OneLine,*,err=410,end=410) FirstCh
        if((FirstCh=='9').or.(FirstCh=='7')) then
          if    (CI_index(OneLine,'DEMAND')>0) then
            NodeDir=-1
          elseif(CI_index(OneLine,'SUPPLY')>0) then
            NodeDir=1
          elseif((CI_index(OneLine,'INTERMED')>0).or.
     +           (CI_index(OneLine,'PRICE POINTS')>0)) then
            NodeDir=0
          else
            NodeDir=-2 ! ignorable
          end if
          cycle
        end if
        read(OneLine,*,err=410,end=410) FirstCh,ThisNdName,cDummy
        if((FirstCh/='1').or.Negat(cDummy).or.(NodeDir<-1)) cycle
        nNode=nNode+1
        if    (NodeDir== 1) then
!Small:   if(nSNod>=3) cycle ! 9 20080131
          nSNod=nSNod+1
        elseif(NodeDir==-1) then
!Small:   if(nDNod>=2) cycle ! 5 20080131
          nDNod=nDNod+1
        end if
      end do
  410 write(DbgU,'(i4," EoF"/)') InpU
      if((nSNod==0).or.(nDNod==0)) call ps(1,'No Supply or Demand node')
      write(DbgU,'(3i4,a)') nNode,nSNod,nDNod,' nodes to be allocated'
      if(nNode==0) call ps(1,'Zero nodes; problem is undefined')
      allocate(NodeSNofDFRank(nNode))
      NodeSNofDFRank=0 ! default valid for non-Demand nodes
      SupDFRank=0
      SupDFNdSN=0
      return ! entry GetNodeCount
!-----
      entry GetLinkCount(nNode,nLink,nCCSegs, ! deinfes nLink and nCCSegs
     +  NodeID,NodeType,
     +  NodeName)
      nCCSegs=10 ! inform caller of array sizes
      nSegsOI=nCCsegs
      rewind(InpU)
      iNode=0
      NodeDir=-2
      do
        read(InpU,'(a)',err=415,end=415) OneLine
        write(DbgU,'(i4,1x,a)') InpU,trim(OneLine)
        read(OneLine,*,err=415,end=415) FirstCh
        if((FirstCh=='9').or.(FirstCh=='7')) then
          if    (CI_index(OneLine,'DEMAND')>0) then
            NodeDir=-1
          elseif(CI_index(OneLine,'SUPPLY')>0) then
            NodeDir=1
          elseif((CI_index(OneLine,'INTERMED')>0).or.
     +           (CI_index(OneLine,'PRICE POINTS')>0)) then
            NodeDir=0
          else
            NodeDir=-2 ! ignorable
          end if
          cycle
        end if
        read(OneLine,*,err=415,end=415) FirstCh,ThisNdName,cDummy
        if((FirstCh/='1').or.Negat(cDummy).or.(NodeDir<-1)) cycle
        read(OneLine,*,err=415,end=415) FirstCh,ThisNdName,cDummy,j
!Small: if((NodeDir== 1).and.(iNode==3)) cycle ! 14 20080131
!Small: if((NodeDir==-1).and.(iNode==2)) cycle !  5 20080131
        iNode=iNode+1
        NodeID(iNode)=j
        NodeType(iNode)=NodeDir
        NodeName(iNode)=ThisNdName
        write(DbgU,'(2i5,i3,1x,a)') iNode,j,NodeDir,ThisNdName
      end do
  415 write(DbgU,'(i4," EoF"/)') InpU
      write(DbgU,'(2i5,1x,a)') (j,NodeID(j),NodeName(j),j=1,nNode)
!
!
      InpU=InpCsvU(2) ! Link parameters
      nLink=0
      do
        read(InpU,'(a)',err=425,end=425) OneLine
        write(DbgU,'(i4,1x,a)') InpU,trim(OneLine)
        read(OneLine,*,err=425,end=425) FirstCh,LinkName,cDummy
        if((FirstCh/='1').or.Negat(cDummy)) cycle
        ignored=B0
        read(OneLine,*,err=425,end=421) FirstCh,LinkName,cDummy,
     +    OtherField
  421   j=15
        write(DbgU,'(i4,2(1x,a))') j,OtherField(j),LinkName
        do j=17,18
          read(OtherField(j),*) iNode
          iNode=IndexIn(NodeID,nNode,iNode)
          if(iNode<=nNode) then
            write(DbgU,'(i4,2(1x,a))') j,OtherField(j),NodeName(iNode)
            if(j==17) then
              i=iNode
            else
              k=iNode
            end if
          else
            write(DbgU,'(i4,2(1x,a))') j,OtherField(j),'ID not active'
            Ignored=B1
          end if
        end do
        if(.not.ignored) nLink=nLink+1
      end do
  425 continue ! after 20070830 bidirectional links occupy 2 records
      write(DbgU,'(i4,a)') nLink,' LUB on 1-way links allocated'
      write(DbgU,'(i4," EoF"/)') InpU
      if(nLink==0) call ps(1,'Zero links; problem is undefined')
      return ! entry GetLinkCount
!-----
      entry ReadGNWDNodeOrder
      allocate(DNodName(nNode))
      jCol=InpU
      InpU=InpCsvU(4) ! Demand-node capacities (to consume)
      read(InpU,'(a)',err=429,end=429) OneLine ! skip header line of column titles
      write(DbgU,'(i4,1x,a)') InpU,trim(OneLine)
      do j=1,nNode ! provide defaults valid for non-Demand nodes
        DNodName(j)=' '
      end do
      read(OneLine,*,err=428,end=428) FiveLabels, ! (cDummy,j=1,5),
     +  (DNodName(j),j=1,nNode) ! FwdSlash in 5th label may have caused cDummy to fail above
  428 continue ! write(DbgU,'(1x,3a)') ('[',FiveLabels(j),']',j=1,5)
      do j=1,nNode
        if(LengNB(DNodName(j))<1) exit ! should exit at j>nDNod
        ThisNdName=DNodName(j)
        do iNode=1,nNode
          if(trim(NodeName(iNode))==trim(ThisNdName)) exit
        end do
!Small: if(iNode>nNode) cycle ! 20080131
        if(iNode>nNode)call ps(1,ThisNdName//' NodeName not active')
        if(NodeType(iNode)/=-1)call ps(1,ThisNdName//' not a Demander')
        write(DbgU,'(2i4,2(1x,a))') j,iNode,ThisNdName,'Rk,SN of Demand'
        NodeSNofDFRank(j)=iNode
        SupDFRank=j
        if(SupDFNdSN<iNode) SupDFNdSN=iNode
      end do
      InpU=jCol ! restore condition when called
      deallocate(DNodName)
      return ! entry ReadGNWDNodeOrder
  429 call ps(1,'unexpected EoF or error reading COALNWDmd.CSV')
!-----
      entry DisplayField(jField)
      do
        read(InpU,'(a)',err=430,end=430) OneLine
        read(OneLine,*,err=430,end=430) FirstCh
        if(FirstCh/='1') cycle
        read(OneLine,*,err=430,end=430) FirstCh,ThisNdName,cDummy
        if(Negat(cDummy)) cycle
        read(OneLine,*,err=430,end=430) FirstCh,ThisNdName,cDummy,
     +    OtherField
        write(DbgU,'(" #",i3,1x,a12,1x,a)') jField,OtherField(jField),
     +    trim(OneLine)
      end do
  430 rewind(InpU)
      return
!-----
      entry PUT_PrtDetail(r_PrtDetail)
         PrtDetail = r_PrtDetail
      return
!-----
      entry ReadGNWInpData(AmortizeUEparm,nNode,nLink,nCCSegs,
     +  PriceSoLR,PriceDoLR,
     +  NodeID,NodeType,
     +  LinkID,LinkNdID,LinkNdSN,
     +  NodeQtyLoB,NodeQtyLim,NodeQtyHardLim,NodeQty,NodePri,
     +  LinkCap,LinkChg,LinkEff,LinkFunc,
     +  NodeName)
      AmortizeUE=AmortizeUEparm ! save for use by other entry-points
!
      if(SupDFNdSN==0) then ! assign the order in which NodeID is expected
        iDNod=0
        do iNode=1,nNode
          if(NodeType(iNode)/=-1) cycle
          iDNod=iDNod+1
          NodeSNofDFRank(iDNod)=iNode
          SupDFNdSN=iNode
        end do
        SupDFRank=iDNod
      end if
!
    ! read network data from Midas-formatted CSV files
      allocate(LinkExpCst(nLink))
      allocate(LinkExpLim(nLink))
      allocate(LinkCCRF  (nLink))
      allocate(DFValue(SupDFRank))
      LinkExpCst=-1.0 ! default indicating undefined
      LinkExpLim=0.0
      rewind(InpU)
      call DisplayField(17)
      call DisplayField(18)
      nZcpLink=0
      nBidLink=0
      iLink=0
      do while(iLink<nLink)
        PreviLink=iLink
        iLnkP1=iLink+1 ! index of tentative assignments before iLink is advanced
        read(InpU,'(a)',err=440,end=440) OneLine
        write(DbgU,'(i4,1x,a)') InpU,trim(OneLine)
        read(OneLine,*,err=440,end=440) FirstCh
!cc     if(FirstCh/='1') ToDmdNod=(CI_index(OneLine,'DEMAND')>0)
        if(FirstCh/='1') cycle
        read(OneLine,*,err=440,end=440) FirstCh,LinkName,cDummy
        if(Negat(cDummy)) cycle
        if(iLink==0) call EnumerateFields(OneLine,50,1,DbgU)
        ignored=B0
        read(OneLine,*,err=440,end=431) FirstCh,LinkName,cDummy,
     +    OtherField
  431   j=5
        read(OtherField(j),*) r4
!       if(iLink>nSNod) r4=50.0 ! to exacerbate pricing errors
        LinkEff(iLnkP1)=1.0-r4*0.01
        write(DbgU,'(2i4,f7.3,a)') iLnkP1,j,r4,' % loss'
        j=15
        read(OtherField(j),*) LinkID(iLnkP1)
        write(DbgU,'(3i4,1x,a)') iLnkP1,j,LinkID(iLnkP1),LinkName
        LinkDir=0 ! default, valid for Intermed links
        do j=17,18
          read(OtherField(j),*) iNode
          LinkNdID(j-17,iLnkP1)=iNode
          iNode=IndexIn(NodeID,nNode,iNode)
          if(j==17) k=iNode ! SN of the source node
          if(iNode<=nNode) then
            write(DbgU,'(3i4,2(1x,a))') iLnkP1,iNode,NodeType(iNode),
     +        OtherField(j),NodeName(iNode)
            LinkNdSN(j-17,iLnkP1)=iNode
            NodeDir=NodeType(iNode)
            if(NodeDir/=0) then ! assign at most once per iLink
              if(LinkDir/=0) call ps(1,'LinkDir previously assigned')
              LinkDir=NodeDir
            end if
          else
            write(DbgU,'(2i4,2(1x,a))') iLnkP1,j,OtherField(j),
     +        'ID not active'
            Ignored=B1
            exit ! to count nZcpLink only once
          end if
        end do ! j
        if(ignored) cycle
        if(iNode==k) call ps(1,'Link is recursive')
        j=6
        do k=0, 0 ! 093008. ! 1 ! Fwd then Rev on A-to-B
        ! read Cap(MMCF/D) and CommodityCost($/MCF)
          j=j+2
          read(OtherField(j),*) r4
!cc       if(B0.and.ToDmdNod) r4=r4*100.0 ! kludge for B0
          j=j+2
! 093008.
          if(ABS(r4) <= 0.1) THEN
            cycle ! link may be usable only A-to-B or B-to-A
          ELSEIF(R4 < 0.0) THEN
            R4 = ESCALATED_MONTHLY_VALUE(
     +                     REAL4_ONE,
     +                     INT2(ABS(R4)),INT2(1),INT2(1),INT2(1))
          ENDIF
          iLink=iLnkP1
          LinkCap(iLink)=r4
          read(OtherField(j),*) r4
! 031409.
          if(ABS(r4) <= 0.1) THEN
            cycle ! link may be usable only A-to-B or B-to-A
          ELSEIF(R4 < 0.0) THEN
            R4 = ESCALATED_MONTHLY_VALUE(
     +                     REAL4_ONE,
     +                     INT2(ABS(R4)),INT2(1),INT2(1),INT2(1))
          ENDIF
          LinkChg(iLink)=r4
!
          write(DbgU,'(i4,f9.2,f8.5,a)') iLink, ! j-2,j,
     +      LinkCap(iLink),r4,' cap,chg'
        end do
        if    (iLink==PreviLink) then
          write(DbgU,'(i4,a)') iLink,' link has zero capacity'
          nZcpLink=nZcpLink+1
          cycle
!       elseif(iLink> PreviLink+1) then
!         write(DbgU,'(i4,a)') iLink,' link is bidirectional'
!         nBidLink=nBidLink+1
!         LinkEff(iLink)=LinkEff(iLink-1)
!         LinkID (iLink)=LinkID (iLink-1)
!         do j=0,1 ! copy IDs and their serial numbers in reverse order
!           k=mod(j+1,2)
!           LinkNdID(j,iLink)=LinkNdID(k,iLink-1)
!           LinkNdSN(j,iLink)=LinkNdSN(k,iLink-1)
!         end do
        end if
        nSegUsd=nSegsOI(LinkDir)
        xPrev=0.0
        yPrev=0.0
        do i=1,nSegUsd ! x then y of normalized n-segment cost curve
          if(i<=10) then
            j=i+21
            k=i+31
          else
            j=i+ 44 ! 21+ 13 (skip first 10 yDpMCF plus #42,43,44)
            k=i+299 ! 31+268; 268=275-10+3
          end if
          read(OtherField(j),*) xFrCap(i)
          read(OtherField(k),*) yDpMCF(i)
          if(i==nSegUsd) then
            if(xFrCap(i)<1.0) xFrCap(i)=1.0
!cc         if(yDpMCF(i)<1.0) yDpMCF(i)=1.0 ! kludge for B0 while testing
          end if
!         write(DbgU,'(3i4,2f15.3)') i,j,k,xFrCap(i),yDpMCF(i)
        ! preclude zeroes in xDelta
          if(xFrCap(i)<=xPrev) xFrCap(i)=xPrev*(1.0+0.0001)
          xDelta(i)=amax1(xFrCap(i)-xPrev,0.0)
          if(i>1 .and. yDpMCF(i)<yPrev) then ! extrapolate using yPrev
            r4=yPrev*(1.0+0.0001)
            write(DbgU,'(i4,2f7.4,3f9.4,a)') i,xPrev,xFrCap(i),yPrev,
     +        yDpMCF(i),r4,' decreasing CC precluded'
            yDpMCF(i)=r4
          end if
          xPrev=xFrCap(i)
          yPrev=yDpMCF(i)
        end do ! i-loop
        write(DbgU,'(10f7.4)') (xFrCap(j),j=1,min(nSegUsd,10))
        write(DbgU,'(10f7.4)') (xDelta(j),j=1,min(nSegUsd,10))
        write(DbgU,'(10f7.3)') (yDpMCF(j),j=1,min(nSegUsd,10))
!       do j=0,1 ! same PWL function applies to both directions (if allowed)
          j=0 ! aftet 20070830 bidirectional links occupy 2 records
          LinkFunc(:,0,iLink-j)=xFrCap
          LinkFunc(:,1,iLink-j)=yDpMCF
          LinkFunc(:,2,iLink-j)=xDelta
!         if(iLink==PreviLink+1) exit ! this link is 1-way
!       end do
        r4=0.0
        CapExpCostRate=0.0
        read(OtherField(3),*) CapExpCostRate ! $/(MMCF/D)/mile
        read(OtherField(43),*,err=439) r4 ! LinkLength in miles
        r4=r4*CapExpCostRate ! $/(MMCF/D) to expand this link
        LinkExpCst(iLink)=r4
        read(OtherField(44),*,err=439) LinkCCRF(iLink)
        read(OtherField(45),*,err=439) LinkExpLim(iLink) ! in (MMCF/D)/year
  439   write(DbgU,'(i4,3f9.2,4(1x,a))') iLink,r4,LinkCCRF(iLink),
     +    LinkExpLim(iLink), ! (OtherField(j),j=43,45),
     +    'LXC,GLXRF,LXL'
!
!       if(yDpMCF(nSegUsd)<=0.0) write(DbgU,'(i4,a)') iLink,' 0-cost link'
        write(DbgU,'(4i4,a,2i4/)') iLink-PreviLink,iLink,nZcpLink,
     +    nBidLink,' links added',(LinkNdID(j,iLink),j=0,1)
      end do ! iLink
  440 nLink=iLink
      write(DbgU,'(3i4,a)') nLink,nZcpLink,nBidLink,
     +  ' 1-way links useful'
      write(DbgU,'(i4," EoF"/)') InpU
!
!
      InpU=InpCsvU(3) ! Supply-node prices and limits
!     call DisplayField(12) ! judged useless after 20070827
      call DisplayField(132)
      do
        read(InpU,'(a)',err=450,end=450) OneLine
        write(DbgU,'(i4,1x,a)') InpU,trim(OneLine)
        read(OneLine,*,err=450,end=450) FirstCh
        if(FirstCh/='1') cycle
        read(OneLine,*,err=450,end=450) FirstCh,ThisNdName,cDummy
        if(Negat(cDummy)) cycle
        read(OneLine,*,err=450,end=441) FirstCh,ThisNdName,cDummy,
     +    OtherField
  441   read(OtherField(3),*) j
        iNode=IndexIn(NodeID,nNode,j)
        if(iNode>nNode) then
          write(DbgU,'(2i4,a)') j,iNode,' ID not active'
          cycle
        end if
        if(NodeType(iNode)/=1) then
          write(DbgU,'(3i4,a)') j,iNode,NodeType(iNode),
     +      ' NodeType not Supply'
          cycle
        end if
        if(iNode==1) call EnumerateFields(OneLine,(113+2*10),1,DbgU)
!
!       judged useless after 20070827
!       r4=0.0
!       read(OtherField(12),*,err=442,end=442) r4 ! capacity MMCF/D
! 442   if(r4<0.0) r4=0.0
!       NodeQty(iNode)=r4 ! NodeQty is used to report Qty supplied by sources
!
        r4=-1.0
        read(OtherField(52),*,err=443,end=443) r4 ! nominal capacity limit MMCF/D [supply]
! 443   if(r4<0.0) r4=0.0 ! negative value implies 'revert to mominal capacity'
  443   NodeQtyLim(iNode)=r4
!cc     if(B0.and.(NodeQty(iNode)==0.0).and.(r4>0.0))
!cc  +    NodeQty(iNode)=r4 ! kludge for B0
!
        r4=-1.0
        read(OtherField(132),*,err=444,end=444) r4 ! emergency (hard) capacity limit MMCF/D [supply]
! 444   if(r4<0.0) r4=0.0 ! negative value implies 'revert to nominal capacity'
  444   NodeQtyHardLim(iNode)=r4
!
        r4=-1.0
        read(OtherField(133),*,err=445,end=445) r4 ! capacity lower-bound MMCF/D [supply]
  445   if(r4<0.0) r4=0.0
        NodeQtyLoB(iNode)=r4
!
        r4=0.0
        read(OtherField(71),*,err=446,end=446) r4 ! cost or price $/MCF
  446   if(r4<0.0) r4=0.0
        NodePri(iNode)=r4
!
!        write(DbgU,'(2i4,4f12.2,a)') j,iNode, ! NodeQty(iNode),
!     +    NodeQtyLoB(iNode),NodeQtyLim(iNode),NodeQtyHardLim(iNode), ! 'SCap,'
!     +    r4,' SLoB,SLim,HLim,SPri'
        if(NodeQtyLim(iNode)<0.0) then ! assume it was a 'vector' reference #
!         call EnumerateFields(OneLine,(113+2*10),1,DbgU)
          write(DbgU,'(a)') ' changing negative UpB above to 1e9'
          NodeQtyLim(iNode)=1.0e9
        end if
      end do
  450 write(DbgU,'(i4," EoF"/)') InpU
!
    ! eliminate dysfunctional nodes from the problem
  452 AnyPruned=B0
      mLink=nLink ! save these two for later use internally
      mNode=nNode
      iSNod=0
      iDNod=0
      iNode=1
      SupnLkAtNd=0
      PriceSoLR=0.0

      do while(iNode<=mNode)
        nLkFrNd=0
        nLkToNd=0
        nLkAtNd=0
        NodeDir=NodeType(iNode)
        if    (NodeDir==-1) then
        ! NodeQty is not yet available for testing
          do j=1,SupDFRank
            if(NodeSNofDFRank(j)==iNode) exit
          end do
!         if(NodeQty(iNode)<=0.0) then ! iNode cannot function as a Demand node
          if(j>SupDFRank) nLkAtNd=-2 ! iNode cannot function as a Demand node
        elseif(NodeDir== 1) then
          if((NodePri(iNode)<=0.0).and.B0) ! retain this node
     +      NodePri(iNode)=0.01 ! kludge for B0
          if(NodePri(iNode)<=0.0) nLkAtNd=-1 ! iNode cannot function as a Supply node
        end if
        if(nLkAtNd==0) then ! verify that links exist to this node
          do iLink=1,mLink
            if(LinkNdSN(0,iLink)==iNode) nLkFrNd=nLkFrNd+1
            if(LinkNdSN(1,iLink)==iNode) nLkToNd=nLkToNd+1
          end do
          nLkAtNd=nLkFrNd+nLkToNd
        end if
        write(DbgU,'(7i4,3f12.1,f7.2,1x,a)') iNode,mNode,NodeID(iNode),
     +    nLkFrNd,nLkToNd,nLkAtNd,NodeDir,
     +    NodeQtyLoB(iNode),
     +    NodeQtyLim(iNode),NodeQtyHardLim(iNode),
     +    NodePri(iNode),NodeName(iNode)
        if(nLkAtNd==-2) write(DbgU,'(a)') ' zero usage at above Demand'
        if(nLkAtNd==-1) write(DbgU,'(a)') ' zero price at above Supply'
        if(nLkAtNd== 0) write(DbgU,'(a)') ' no links at the above node'
        if((NodeDir==0).and.(nLkAtNd>0).and.(nLkFrNd*nLkToNd==0))
     +    then ! all links were either From or Into iNode
          UbString='into'
          if(nLkFrNd==0) UbString='from'
          write(DbgU,'(3a)') ' no nodes linked ',trim(UbString),
     +      ' the above intermediate node'
          nLkAtNd=-3 ! node is useless as a pricing point
        end if
        if(nLkAtNd<=0) then
        ! compress-out the dysfunctional node with SN iNode
          AnyPruned=B1
          mNode=mNode-1
          do j=iNode,mNode
            k=j+1
            NodeName(j)=NodeName(k)
            NodeType(j)=NodeType(k)
!           NodeQty (j)=NodeQty (k) ! not yet available for Demand nodes
            NodeQtyLoB(j)=NodeQtyLoB(k)
            NodeQtyLim(j)=NodeQtyLim(k)
            NodeQtyHardLim(j)=NodeQtyHardLim(k)
            NodePri (j)=NodePri (k)
            NodeID  (j)=NodeID  (k)
          end do
!
          if(SupDFNdSN>=iNode) then
            SupDFNdSN=0
            do j=1,SupDFRank ! decrement those Demand nodes' DNodSN > iNode
              k=NodeSNofDFRank(j) ! at most SupDFNdSN
              if(k==iNode) then
                NodeSNofDFRank(j)=0 ! allow DemandFile value to be read & ignored
              elseif(k>iNode) then
                NodeSNofDFRank(j)=k-1 ! nDNod will be reduced, but not SupDFRank
              end if
              SupDFNdSN=max(SupDFNdSN,NodeSNofDFRank(j)) ! revise
            end do
            write(DbgU,'(a,99i3)') ' NdSN',
     +        (NodeSNofDFRank(j),j=1,SupDFRank)
        ! else skip the above loop as ineffective
          end if
          do iLink=1,mLink ! decrement those links' NdSN >= iNode
            do j=0,1
              k=LinkNdSN(j,iLink)
              if(k==iNode) then
                LinkNdSN(j,iLink)=32000 ! mark link as unusable (out-of-bounds)
                write(DbgU,'(4i4,a)') iLink,LinkID(iLink),
     +            j,k,' LinkNdSN before increase'
              elseif(k>=iNode) then
                LinkNdSN(j,iLink)=k-1
!               write(DbgU,'(4i4,a)') iLink,LinkID(iLink),
!    +            j,k-1,' LinkNdSN reduced'
              end if
            end do
          end do
        else ! retain this node
          if(    NodeDir==-1) then
            if(nLkToNd==0) call ps(1,'no links into above Demand node')
            iDNod=iDNod+1
          elseif(NodeDir== 1) then
            if(nLkFrNd==0) call ps(1,'no links from above Supply node')
            iSNod=iSNod+1
          end if
          if(SupnLkAtNd<nLkAtNd) SupnLkAtNd=nLkAtNd
          if(PriceSoLR<NodePri(iNode)) PriceSoLR=NodePri(iNode) ! retain largest
          iNode=iNode+1
        end if
      end do ! while(iNode<=mNode)
      if(AnyPruned) then
        write(DbgU,'(2a)') ' iLk mLk IDi SID DID SrcSN DstSN',
     +    ' reason deleted'
        write(DbgU,'(2a)') ' --- --- --- --- --- ----- -----',
     +    ' ------------------'
      end if
      iLink=1
      do while(iLink<=mLink) ! check for unusable links
        if((LinkNdSN(0,iLink)>mNode).or.
     +     (LinkNdSN(1,iLink)>mNode)) then ! compress-out the unusable link
          AnyPruned=B1
          mLink=mLink-1
          write(DbgU,'(5i4,2i6,a)') iLink,mLink,LinkID(iLink),
     +      (LinkNdID(i,iLink),i=0,1),
     +      (LinkNdSN(i,iLink),i=0,1),' LinkNdSN=>unusable'
          do i=iLink,mLink
            k=i+1
            LinkID (i)=LinkID (k)
            LinkCap(i)=LinkCap(k)
            LinkChg(i)=LinkChg(k)
            LinkEff(i)=LinkEff(k)
            LinkExpCst(i)=LinkExpCst(k)
            LinkExpLim(i)=LinkExpLim(k)
            LinkCCRF  (i)=LinkCCRF  (k)
            do j=0,2
              LinkFunc(:,j,i)=LinkFunc(:,j,k)
              if(j>1) cycle
              LinkNdSN(j,i)=LinkNdSN(j,k)
              LinkNdID(j,i)=LinkNdID(j,k)
            end do
          end do
        else
          iLink=iLink+1
        end if
      end do ! while(iLink)
      write(DbgU,'(2i4,a)')nSNod,iSNod,' Supply nodes with price>0'
      write(DbgU,'(2i4,a)')nDNod,iDNod,' Demand nodes with capacity>0'
      write(DbgU,'(2i4,a)')nNode,mNode,' contiguous nodes after pruning'
      write(DbgU,'(2i4,a/)')nLink,mLink,
     +  ' useful 1-way links after pruning'
      nSNod=iSNod
      nDNod=iDNod
      nLink=mLink
      nNode=mNode ! inform caller of the revised useful limits
      if(AnyPruned) goto 452 ! recheck, in case of interactions
      if(nSNod==0) call ps(1,'zero active Supply nodes; see .dbg-file')
      if(nDNod==0) call ps(1,'zero active Demand nodes; see .dbg-file')
!
!
!
      jCol=int4(nNode)
      allocate(LkCstNode(nNode))
      allocate(SpQtyNode(nNode))
      allocate(LkUsdFrom(nNode))
      allocate(nLinkInto(nNode))
      allocate(NdLkdInto(SupnLkAtNd,nNode))
      allocate(LkUsdInto(SupnLkAtNd,nNode))
      allocate(nPriorSegs(nLink))
      allocate(LinkType  (nLink))
      allocate(LinkChgOrg(nLink))
      allocate(LnkQtyP   (nLink))
      allocate(LinkNdBi  (nLink))
      LkUsdFrom=0
      nLinkInto=0
      NdLkdInto=0
      LkUsdInto=0
      nLkOfType=0
      nPriorSegs(1)=0
      PriceDoLR=1e32
    ! list all nodes connected by a single link
      write(DbgU,'(/a)') ' active links'
      write(DbgU,'(3a)') '  iLk IDi SID DID LinkEff SSN DSN LI',
     +   ' Source-Node name            Drain-Node name'
      write(DbgU,'(3a)') '  --- --- --- --- ------- --- --- --',
     +  (' ---------------------------',j=0,1)
      do iLink=1,nLink
        write(DbgU,'(1x)',advance='no') ! expect advance=no to follow ! write one line for each link
        write(DbgU,'( 4i4,f8.5)',advance='no') iLink,LinkID(iLink),
     +    (LinkNdID(j,iLink),j=0,1),LinkEff(iLink)
        LinkDir=0 ! default, valid for Intermed links
!cc     do j=1,1 ! 0 => LinksFrom, 1 => LinksInto
!cc       k=mod(1+j,2)
          j=1
          k=0
          iNode=LinkNdSN(j,iLink) ! Dest if j==1
          kNode=LinkNdSN(k,iLink) ! Sour if j==1
          NodeDir=NodeType(iNode)
          if(NodeDir/=0) LinkDir=NodeDir
          NodeDir=NodeType(kNode)
          if(NodeDir/=0) LinkDir=NodeDir
          if(NodeDir==1) LkUsdFrom(kNode)=iLink ! unique for Supply nodes
          if((iNode>nNode).or.(kNode>nNode)) cycle ! during debugging
          i=nLinkInto(iNode)+1 ! increment count at one end
          NdLkdInto(i,iNode)=kNode ! store SN of other end
          LkUsdInto(i,iNode)=iLink ! store SN of link used
          nLinkInto(iNode)=i ! store count
          write(DbgU,'( 2i4,i3,2(1x,a))',advance='no') kNode,iNode,i,
     +      NodeName(kNode),NodeName(iNode)
!cc     end do
      ! make LinkNdBi same for A-B and B-A so Bidir links can be netted
        if(iNode<kNode) then
          LinkNdBi(iLink)=65536*int4(iNode)+kNode
        else
          LinkNdBi(iLink)=65536*int4(kNode)+iNode
        end if
        LinkType(iLink)=LinkDir
        nLkOfType(LinkDir)=nLkOfType(LinkDir)+1
        if(iLink<nLink) nPriorSegs(iLink+1)=
     +                  nPriorSegs(iLink)+nSegsOI(LinkDir)
!       write(DbgU,'( i12)',advance='no') LinkNdBi(iLink)
        write(DbgU,'(1x)',advance='yes') ! writes CrLf after blank
        r4=LinkChg(iLink)+LinkFunc(1,1,iLink) ! cost of 1st segment
        if(PriceDoLR>r4) PriceDoLR=r4 ! retain smallest
      end do ! iLink
      write(DbgU,'(4i4,a)') nLkOfType,SupnLkAtNd,
     +  ' >= # links at any node'
!cc   PriceSoLR=PriceSoLR*2.0 ! sufficient to avoid economic use except to gain feasibility
      PriceSoLR=amax1(65.0,PriceSoLR*2.0) ! sufficient to alter usage per GT 20080606
! 081908. TEMPORARY FOR PPL DEMO.
!      PriceSoLR=amax1(15.0,PriceSoLR*2.0) ! sufficient to alter usage per GT 20080606
      PriceDoLR=amax1(0.01,PriceDoLR) ! preclude use of negative prices
      write(DbgU,'(2f10.3,a)') PriceSoLR,PriceDoLR,
     +  ' prices of Last-Resort S & D'
!
!
      write(DbgU,'(/a)') ' node-connectivity-by-link-SN Fwd_Rev matrix:'
!
      write(DbgU,'(1x)',advance='no') ! expect advance=no to follow ! write one line of NodeType headings
      write(DbgU,'( a6)',advance='no') ' Type '
      do iNode=1,nNode
        write(DbgU,'( i5)',advance='no') NodeType(iNode)
      end do
      write(DbgU,'(1x)',advance='yes') ! writes CrLf after blank
!
      write(DbgU,'(1x)',advance='no') ! expect advance=no to follow ! write one line of NodeSN headings
      write(DbgU,'( a6)',advance='no') 'iNode '
      do iNode=1,nNode
        write(DbgU,'( i5)',advance='no') iNode
      end do
      write(DbgU,'( a)',advance='no') ' #Fwd #Rev'
      write(DbgU,'(1x)',advance='yes') ! writes CrLf after blank
!
      write(DbgU,'(1x)',advance='no') ! expect advance=no to follow ! write one line of field-width markers
      write(DbgU,'( a6)',advance='no') ' '
      do iNode=-1,nNode
        write(DbgU,'( a5)',advance='no') ' ----'
      end do
      write(DbgU,'(1x)',advance='yes') ! writes CrLf after blank
!
      do iSNod=1,nNode
        write(DbgU,'(1x)',advance='no') ! expect advance=no to follow ! write one line for each node
        write(DbgU,'( i2,i3,a1)',advance='no') NodeType(iSNod),iSNod,':'
        nLinkFrTo=0
        do iDNod=1,nNode
          if(iDNod==iSNod) then ! mark the diagonal
            write(DbgU,'( a5)',advance='no') '  X  '
          else ! show matrix of iLink (Fwd.Rev)
            do j=0,1
              k=mod(j+1,2)
              do iLink=1,nLink
                if((LinkNdSN(j,iLink)==iSNod).and.
     +             (LinkNdSN(k,iLink)==iDNod)) exit
              end do
              LinkFwdRev(j)=0
              if(iLink<=nLink) then
                LinkFwdRev(j)=iLink
                nLinkFrTo(j)=nLinkFrTo(j)+1
              end if
            end do
            if    (LinkFwdRev(0)+LinkFwdRev(1)==0) then
              write(DbgU,'( 2x,".",2x  )',advance='no')
            elseif(LinkFwdRev(0)==0) then
              write(DbgU,'( 2x,"_",i2.2)',advance='no') LinkFwdRev(1)
            elseif(LinkFwdRev(1)==0) then
              write(DbgU,'( i2,"_",2x  )',advance='no') LinkFwdRev(0)
            else
              write(DbgU,'( i2,"_",i2.2)',advance='no') LinkFwdRev
            end if
          end if
        end do ! iDNod
        write(DbgU,'( 2i5)',advance='no') nLinkFrTo
        write(DbgU,'(1x)',advance='yes') ! writes CrLf after blank
        if((NodeType(iSNod)== 1).and.(nLinkFrTo(0)==0)) call ps(1,
     +    'no links from Supply node')
        if((NodeType(iSNod)==-1).and.(nLinkFrTo(1)==0)) call ps(1,
     +    'no links into Demand node')
      end do ! iSNod
!
!
      write(DbgU,'(/a)') ' node-connectivity-by-link dir matrix:'
!
      write(DbgU,'(1x)',advance='no') ! expect advance=no to follow ! write one line of NodeType headings
      write(DbgU,'( a6)',advance='no') ' Type '
      do iNode=1,nNode
        write(DbgU,'( i1)',advance='no') NodeType(iNode)
      end do
      write(DbgU,'(1x)',advance='yes') ! writes CrLf after blank
!
      write(DbgU,'(1x)',advance='no') ! expect advance=no to follow ! write one line of NodeSN headings
      write(DbgU,'( a6)',advance='no') 'iNode '
      do iNode=1,nNode
        write(DbgU,'( i1)',advance='no') iNode/100
      end do
      write(DbgU,'(1x)',advance='yes') ! writes CrLf after blank
!
      write(DbgU,'(1x)',advance='no') ! expect advance=no to follow ! write one line of NodeSN headings
      write(DbgU,'( a6)',advance='no') 'iNode '
      do iNode=1,nNode
        write(DbgU,'( i1)',advance='no') mod(iNode/10,10)
      end do
      write(DbgU,'(1x)',advance='yes') ! writes CrLf after blank
!
      write(DbgU,'(1x)',advance='no') ! expect advance=no to follow ! write one line of NodeSN headings
      write(DbgU,'( a6)',advance='no') 'iNode '
      do iNode=1,nNode
        write(DbgU,'( i1)',advance='no') mod(iNode,10)
      end do
      write(DbgU,'( a)',advance='no') ' #F #R'
      write(DbgU,'(1x)',advance='yes') ! writes CrLf after blank
!
      write(DbgU,'(1x)',advance='no') ! expect advance=no to follow ! write one line of field-width markers
      write(DbgU,'( a6)',advance='no') ' '
      do iNode=1,nNode
        write(DbgU,'( a1)',advance='no') '-'
      end do
      write(DbgU,'(1x)',advance='yes') ! writes CrLf after blank
!
      do iSNod=1,nNode
        write(DbgU,'(1x)',advance='no') ! expect advance=no to follow ! write one line for each node
        write(DbgU,'( i2,i3,a1)',advance='no') NodeType(iSNod),iSNod,':'
        nLinkFrTo=0
        f4=0.0
        r4=0.0
        do iDNod=1,nNode
          if(iDNod==iSNod) then ! mark the diagonal
            write(DbgU,'( a)',advance='no') char(176)
          else ! show matrix of iLink directions
            do j=0,1
              k=mod(j+1,2)
              do iLink=1,nLink
                if((LinkNdSN(j,iLink)==iSNod).and.
     +             (LinkNdSN(k,iLink)==iDNod)) exit
              end do
              LinkFwdRev(j)=0
              if(iLink<=nLink) then
                LinkFwdRev(j)=iLink
                nLinkFrTo(j)=nLinkFrTo(j)+1
                if(j==0) then
                  f4=f4+LinkCap(iLink)
                else
                  r4=r4+LinkCap(iLink)
                end if
              end if
            end do
            if    (LinkFwdRev(0)+LinkFwdRev(1)==0) then
              write(DbgU,'( ".")',advance='no')
            elseif(LinkFwdRev(0)==0) then
              write(DbgU,'( "\")',advance='no') ! LinkFwdRev(1)
            elseif(LinkFwdRev(1)==0) then
              write(DbgU,'( "/")',advance='no') ! LinkFwdRev(0)
            else
              write(DbgU,'( a1)',advance='no') char(178) ! LinkFwdRev
            end if
          end if
        end do ! iDNod
        write(DbgU,'( 2i3)',advance='no') nLinkFrTo
        write(DbgU,'( 2f10.0)',advance='no') f4,r4
!       if    (NodeType(iSNod)== 1) then
!         write(DbgU,'( f10.0)',advance='no') NodeQty(iSNod)
!       elseif(NodeType(iSNod)==-1) then
!       else
!       end if
        write(DbgU,'(1x)',advance='yes') ! writes CrLf after blank
        if((NodeType(iSNod)== 1).and.(nLinkFrTo(0)==0)) call ps(1,
     +    'no links from Supply node')
        if((NodeType(iSNod)==-1).and.(nLinkFrTo(1)==0)) call ps(1,
     +    'no links into Demand node')
      end do ! iSNod
!
!
!      write(DbgU,'(/a)')
!     +  ' source nodes one path-step from destination on left'
!      do iNode=1,nNode
!        write(DbgU,'(1x)',advance='no') ! expect advance=no to follow
!        write(DbgU,'( i3,i4,a)',advance='no')NodeType(iNode),iNode,':'
!        do k=1,nLinkInto(iNode)
!          write(DbgU,'( i4)',advance='no') NdLkdInto(k,iNode)
!        end do
!        write(DbgU,'(1x)',advance='yes') ! writes CrLf after blank
!      end do
      write(DbgU,'(1x)')
! 031208. MAY BE RESET IN THE GregDayCall entry
      PrtDetail=0 ! for normal operation of the model by a customer
!     PrtDetail=1 ! for debugging details (some)
!     PrtDetail=2 ! for debugging details (more)
!
      MinimizingObj=B1 ! use the PCX default to MINimize its objective
      if(B0) then
        nLbV=0 ! needed as argument to PCX_main()
        nUbV=nLkOfType(-1)*nSegsOI(-1)+
     +       nLkOfType( 0)*nSegsOI( 0)+
     +       nLkOfType( 1)*nSegsOI( 1)
      else
        nLbV=nSNod
        nUbV=nLbV+nLink*nCCSegs ! (nCCSegs-1) if the last segment is unbounded
      end if
      nTVar=nSNod ! count of useful Terminal vars
      if(AmortizeUE) nTVar=nTVar+nSNod+nDNod ! NodeQtyOfLR(nSNod+nDNod) vars are useful
      nVar=nTVar+nUbV-nLbV ! nLbV of the nUbV vars are the same (have LB & UB)
      mReq=nNode
      write(DbgU,'(6i4,4i5,L2,a)') nSegsOI,nLkOfType,nUbV,nSNod,nVar,
     +  mReq,AmortizeUE,' SoI,LoT,Ub,Sn,nV,mR,AUE'
      call timer(OrgCentiSec)
      return ! entry ReadGNWInpData
!-----
      entry GetReportGNWDim(R_ENDPOINT,R_YEAR,R_MONTH,R_DAY)
         WRITE(EndpointStr,'(I4)') R_ENDPOINT
         WRITE(YearStr,'(I4)') R_YEAR
         MonthStr = CL_MONTH_NAME(R_MONTH)
         WRITE(DayStr,'(I4)') R_DAY
         if(DayStr(1:2)=='  ') DayStr=DayStr(3:4) ! trim() doesn't cut leading blanks
      return
!-----
      entry GetDaysDemand(ErrOrEnd,NodeQty)
!     OrigBounds=B1
      InpU=InpCsvU(4)
      read(InpU,'(a)',iostat=ErrOrEnd) OneLine ! line of Demand for one day
      if(ErrOrEnd/=0) then
        write(DbgU,'(i4," EoF"/)') InpU
        return
      end if
      write(DbgU,'(/i4,1x,a)') InpU,trim(OneLine)
      read(OneLine,*,err=512,end=512) cDummy,YearStr,MonthStr,DayStr,
!    +  (NodeQty(NodeSNofDFRank(j)),j=1,SupDFRank)
     +  (DFValue(j),j=1,SupDFRank)
      do j=1,SupDFRank
        k=NodeSNofDFRank(j) ! 0 if this Demand node was pruned out
        if(k>0) NodeQty(k)=DFValue(j) ! else ignore the DFValue read
!       write(DbgU,'(2i4,f10.1,a)') j,k,DFValue(j),' Demand as read'
      end do
      if(DayStr(1:2)=='  ') DayStr=DayStr(3:4) ! trim() doesn't cut leading blanks
      return
  512 call ps(1,'Error or insufficient data in above')
!-----
      entry AllocateGNWOptimizingArrays
      ConsMtSLim=1024 ! initial limit on size of ConsMtxNze,zbColOfNze,zbRowOfNze
      write(DbgU,'(//2i7,a/)') mReq,nVar,
     +  ' #Req & Var in the LP formulation'
!
      allocate(A4Equiv(nVar))
      allocate(VarDesc(nVar))
      allocate(TNofVar(nTVar))
      allocate(cVect(nVar))
      allocate(xVect(nVar))
      allocate(bVect(mReq))
      allocate(ReVec(mReq))
      allocate(ConsMtxNze(ConsMtSLim))
      allocate(zbColOfNze(ConsMtSLim))
      allocate(zbRowOfNze(ConsMtSLim))
      allocate(jUbVec(nUbV))
      allocate(UbValu(nUbV))
      allocate(UbVOrg(nUbV))
      if(nLbV>0) then
      allocate(jLbVec(nLbV))
      allocate(LbValu(nLbV))
      end if
      allocate(NodeSrcNip(mNode,nSNod)) ! number of source paths in cost pool for iNode with iSNod as Supply
      allocate(NodeSrcQty(mNode,nSNod))
      allocate(NodeSrcCst(mNode,nSNod))
      allocate(CycleOrgD(mNode))
      TNofVar=-1 ! default implying N.A.
      return
!-----
      entry MinGNWCosts(nCCSegs,
     +  NodeID,NodeType,
     +  LinkID,LinkNdID,LinkNdSN,
     +  NodeQtyLoB,NodeQtyLim,NodeQtyHardLim,
     +  NodeQty,NodePri,PriceSoLR,PriceDoLR,
     +  LinkCap,LinkChg,LinkEff,LinkFunc,NodeName)
      ConsMtSize=0 ! count of elements used in above arrays
!
      LinkChgOrg(1:mLink)=LinkChg(1:mLink) ! save unperturbed copy
      iReq=0
      jVar=0
      iLbV=0
      iUbV=0
    ! explete the arrays on the LP interface:  cVect,bVect,ReVec,ConsMtxNze
    ! each Supply node has one variable (not upper-bounded)
      do iNode=1,mNode
        if(NodeType(iNode)/=1) cycle ! skip non-Supply nodes
        call GNWIncrVar()
        TNofVar(jVar)=iNode
        call GNWIncrLoB()
        LbValu(iLbV)=NodeQtyLoB(iNode)
      ! without the following, PCX infers that UbValu is zero when LbValu is specified above
        call GNWIncrUpB()
        UbValu(iUbV)=amax1(NodeQtyLim(iNode),NodeQtyHardLim(iNode))
        write(VarDesc(jVar),'(a,i4.4)') 'MMCF_Piped_from_Supply_',
     +    NodeID(iNode)
      ! during the LP solution, treat the node price as a commodity charge
        iLink=LkUsdFrom(iNode)
        NodeSupplyLtd=NodeQtyLim(iNode)
        if(NodeSupplyLtd>=0.0) then
          LinkChg(iLink)=LinkChgOrg(iLink)+NodePri(iNode) ! per GAT 20070702
!         write(DbgU,'(2i4,2e10.3,a)') iLink,iNode,NodePri(iNode),
!    +      LinkChg(iLink),' LinkChg after NodePri zeroing'
          NodePri(iNode)=NodePri(iNode)*1.0e-12 ! until LP is solved
        end if
        cVect(jVar)=NodePri(iNode)*1000. ! in ($/MMCF)
!       write(DbgU,'(3i4,f9.3,2(1x,a))') iNode,jVar,nSNod,cVect(jVar),
!    +    VarDesc(jVar),NodeName(iNode)
      end do
      if(jVar/=nSNod) call ps(1,'error counting Supply nodes')
!
      if(AmortizeUE) then
    ! each Supply node adds one variable (not upper-bounded)
      do iNode=1,mNode
        if(NodeType(iNode)/= 1) cycle ! skip non-Supply nodes
        call GNWIncrVar()
        TNofVar(jVar)=iNode ! here nSNod<jVar<=nSnod*2<nTVar
        write(VarDesc(jVar),'(a,i4.4)') 'MMCF_Dumped_FromSupply_',
     +    NodeID(iNode)
        cVect(jVar)=PriceDoLR*1000. ! in ($/MMCF)
!       write(DbgU,'(3i4,i9,2(1x,a))') iNode,jVar,nSNod,
!    +    nint(cVect(jVar)),VarDesc(jVar),NodeName(iNode)
      end do
    ! each Demand node adds one variable (not upper-bounded)
      do iNode=1,mNode
        if(NodeType(iNode)/=-1) cycle ! skip non-Demand nodes
        call GNWIncrVar()
        TNofVar(jVar)=iNode ! here nSNod*2<jVar<=nSnod*2+nDNod=nTVar
        write(VarDesc(jVar),'(a,i4.4)') 'MMCF_ReducedFromDemand_',
     +    NodeID(iNode)
        cVect(jVar)=PriceSoLR*1000. ! in ($/MMCF)
!       write(DbgU,'(3i4,i9,2(1x,a))') iNode,jVar,nDNod,
!    +    nint(cVect(jVar)),VarDesc(jVar),NodeName(iNode)
      end do
      if(jVar/=nTVar) call ps(1,'error counting slack & surplus nodes')
      end if ! re AmortizeUE
!
    ! each directional link has nSegUsd variables and upper-bounds
      do iLink=1,mLink
        LinkDir=LinkType(iLink)
        r4=LinkCap(iLink)
        NodeSupplyLtd=-1.0 ! indicating 'node supply does not limit link usage'
        iNode=LinkNdSN(0,iLink)
        NodeDir = NodeType(iNode)
        if(LinkDir==1) then ! iLink drains a supply node (uniquely)
          NodeSupplyLtd=NodeQtyLim(iNode)
          if(NodeSupplyLtd>=0.0) r4=NodeSupplyLtd
        end if
        nSegUsd=nSegsOI(LinkDir)
        do j=1,nSegUsd
          call GNWIncrVar() ! incremented jVar is needed by GNWIncrUpB()
          write(VarDesc(jVar),'(a,i1,a,i4.4,a,i4.4)') 'Seg_',j,
     +      '_MMCF_frm_',
     +      LinkNdId(0,iLink),'_to_',
     +      LinkNdId(1,iLink)
!         if(j<nSegUsd) then ! after 20070615, let last segment run unbounded
            call GNWIncrUpB()
            i4=nint(NodeQtyHardLim(iNode))
            if((j==nSegUsd).and.(i4/=-1) .and. (NodeDir == 1) ) then
              UbValu(iUbV)=amax1(abs(NodeQtyHardLim(iNode))-r4,0.0)
            else
              UbValu(iUbV)=r4*LinkFunc(j,2,iLink) ! in MMCF/D
            end if
!           if(NodeSupplyLtd>=0.0)
!           write(DbgU,'(5i6,3e12.5,2(1x,a))') iLink,j,iUbV,jVar,iNode,
!    +        r4,NodeQtyHardLim(iNode),
!    +        UbValu(iUbV),'UbValu',trim(VarDesc(jVar))
!         end if
          cVect(jVar)=1000.*
     +      (LinkChg(iLink)+LinkFunc(j,1,iLink)) ! in ($/MMCF)
          if(cVect(jVar)<=0.0) then
            write(DbgU,'(3i5,f9.1,2(1x,a))') iLink,j,jVar,cVect(jVar),
     +        VarDesc(jVar),'neg-cost link' ! possibly due to artificial
            ! manipulation of LinkChg and LinkFunc
!            
! 070808. removed to allow negative values for european database.            
!
!            call ps(1,'neg-cost link; see .dbg-file for details')
          end if
        end do ! j
      end do ! iLink
!
    ! each node generates one constraint (row in ConsMtx)
      iSNod=0
      iDNod=0
      do iNode=1,mNode
        call GNWIncrReq()
!       write(DbgU,'(1x)',advance='no') ! expect advance=no to follow
!       write(DbgU,'( 2i4,a)',advance='no') iNode,iReq,' jVar: '
        ReVec(iReq)=0 ! for EQL; NodeQtyOfLR is the slack var if AmortizeUE
        bVect(iReq)=0.0 ! net sum of Qty piped out of node
        NodeDir=NodeType(iNode)
        if(NodeDir==-1) then ! for Demand nodes
          bVect(iReq)=NodeQty(iNode)
          if(AmortizeUE) then ! allow Supplier of Last Resort to offload Demand
            iDNod=iDNod+1 ! these slack variables allow feasibility at high price
            call AssignGNWAMtx(int4(nSNod*2+iDNod),iReq,1.0) ! Qty in MMCF supplied by iNode's SoLR
          end if
        elseif(NodeDir==1) then ! for Supply nodes
          iSNod=iSNod+1
          call AssignGNWAMtx(int4(iSNod),iReq,1.0) ! Qty in MMCF supplied by iNode
          if(AmortizeUE) then ! allow Demander of Last Resort to dump Supply
          ! these surplus variables allow feasibility at high price
            call AssignGNWAMtx(int4(nSNod+iSNod),iReq,-1.0) ! Qty in MMCF dumped by iNode's DoLR
          end if
        end if
        jCol=nTVar+1 ! bias to account for 1-based j-loop below
        do iLink=1,mLink ! enter coefficient in matrix for each link touching iNode
          nSegUsd=nSegsOI(LinkType(iLink))
          jCol=jCol+nSegUsd ! must advance before possible cycle below
          if    (LinkNdSN(0,iLink)==iNode) then
            r4=-1.0 ! link's flow is out of iNode
!           write(DbgU,'( i7.5)',advance='no') -(jCol-nSegUsd)
          elseif(LinkNdSN(1,iLink)==iNode) then
            r4=LinkEff(iLink) ! link's flow is into iNode
!           write(DbgU,'( i7.5)',advance='no')  (jCol-nSegUsd)
          else
            cycle
          end if
!         write(DbgU,'(6i5,f5.1,a)') iNode,iReq,jCol,iLink,
!    +      (LinkNdSN(j,iNode),j=0,1),r4,' FinalSegAMtxCoef'
          do j=nSegUsd,1,-1 ! sum Qty across segments
            call AssignGNWAMtx(jCol-j,iReq,r4) ! times Qty in MMCF into iNode
          end do ! decrementing from nSegUsd down to 1
        end do
!       write(DbgU,'(1x)',advance='yes') ! writes CrLf after blank
      end do ! iNode
!
!     call WriteStrAt(8,5,'after  filling constraint matrix')
      if(PrtDetail>1)
     +  write(DbgU,'(i9,a)') ConsMtSize,' = # of non-zeroes in ConsMtx'
    ! sort to facilitate search within ReturnGNWAMtx
      call SortByAscendingKey(
     +  ConsMtSize,zbRowOfNze,zbColOfNze,ConsMtxNze)
!     call WriteStrAt(8,5,'after  sorting constraint matrix')
!
      if((PrtDetail>1).and.(mReq<=999)) then
      ! display upper-bounds
        do iUbV=1,nUbV
          write(DbgU,'(2i6,f17.3,a)') iUbV,jUbVec(iUbV),
     +      UbValu(iUbV),' UBV'
        end do
        write(DbgU,'(a)') ' ConsMtx'
        write(DbgU,'(5x,100i4.3)') (jVar,jVar=1,nVar)
        do iReq=1,mReq ! display entire constraint matrix
          do jVar=1,nVar
          ! needed to preclude 'initiating I/O during I/O' error
            A4Equiv(jVar)=R4toA4(FuncGNWAMtx(jVar,iReq)) ! very slow
          end do
          write(DbgU,'(1x,i4,100a4/(5x,100a4))') iReq,
     +      (A4Equiv(jVar),jVar=1,nVar)
        end do ! iReq
        write(DbgU,'(1x)')
!
      ! display the ConsMtx and vectors b & c
        jCol=min(nVar,50)
        write(DbgU,'(a,50(a,i2.2))')
     +    ' iReq Re         RHS 1+nLZ   {Ai} {c}'
!    +    ,(' A',jVar,jVar=1,jCol)
        write(DbgU,'(51a)')
     +    ' ---- -- ----------- ----- ------ ...'
!         ,(' ---',jVar=1,jCol)
        do iReq=1,mReq
         write(DbgU,'(1x)',advance='no') ! expect advance=no to follow
         write(DbgU,'( i5,i3,f11.0)',advance='no')
     +     iReq,ReVec(iReq),bVect(iReq)
         do jVar=1,nVar
           r4=FuncGNWAMtx(jVar,iReq)
           if(r4==0.0) cycle
           jCol=1
           if(r4<0.0) jCol=-1
           write(DbgU,'( i6)',advance='no') jCol*jVar
         end do
         write(DbgU,'(1x)',advance='yes') ! writes CrLf after blank
!         write(*,'(2i6,a)') iReq,mReq,' row written' ! to account for slowness
        end do ! iReq
        write(DbgU,'(1x)')
        call flush(DbgU)
      end if

      call flush(DbgU)
      UbVOrg(1:nUbV)=UbValu(1:nUbV) ! ssve unperturbed copy
      return ! entry MinGNWCosts
!-----
      entry ReleaseOtherGNWArrays
      deallocate(VarDesc,A4Equiv,LkCstNode,SpQtyNode,
     +  LinkType,LkUsdFrom,nLinkInto,NdLkdInto,LkUsdInto,LinkNdBi,
     +  nPriorSegs,NodeSNofDFRank,TNofVar,
     +  cVect,bVect,xVect,ReVec, ! if allocatable
     +  jUbVec,UbValu,UbVOrg,jLbVec,LbValu,
     +  ConsMtxNze,zbColOfNze,zbRowOfNze,
     +  NodeSrcNip,NodeSrcQty,NodeSrcCst,LinkChgOrg,LnkQtyP,
     +  LinkExpCst,LinkExpLim,LinkCCRF,DFValue,CycleOrgD)
      return
!-----
      entry AssignGNWAMtx(OneBasedCol,OneBasedRow,ACMvalue)
      if(ConsMtSize==ConsMtSLim) then ! move vectors' contents into larger space
        write(4,'(i9,a)') ConsMtSLim,' < # of non-zeroes in ConsMtx'
        allocate(zbColHold(ConsMtSLim)) ! temporary swap-space
        allocate(zbRowHold(ConsMtSLim))
        allocate(ConsEHold(ConsMtSLim))
      ! save copies of full vectors in ~Hold
        zbColHold=zbColOfNze
        zbRowHold=zbRowOfNze
        ConsEHold=ConsMtxNze
        deallocate(zbColOfNze,zbRowOfNze,ConsMtxNze)
        ConsMtSLim=ConsMtSLim*2 ! double vectors' size-limit
        allocate(zbColOfNze(ConsMtSLim))
        allocate(zbRowOfNze(ConsMtSLim))
        allocate(ConsMtxNze(ConsMtSLim))
      ! restore saved values from ~Hold vectors
        zbColOfNze(1:ConsMtSize)=zbColHold ! (1:ConsMtSize)
        zbRowOfNze(1:ConsMtSize)=zbRowHold ! (1:ConsMtSize)
        ConsMtxNze(1:ConsMtSize)=ConsEHold ! (1:ConsMtSize)
        deallocate(zbColHold,zbRowHold,ConsEHold) ! free swap-space
      end if
      ConsMtSize=ConsMtSize+1
!     OneBasedOfs=OneBasedCol+(OneBasedRow-1)*nVar ! for column=major order
!     OneBasedOfs=OneBasedRow+(OneBasedCol-1)*mReq ! for row-major order
      zbColOfNze(ConsMtSize)=OneBasedCol-1 ! note that subscripts here ...
      zbRowOfNze(ConsMtSize)=OneBasedRow-1
!     ConsMtxOfs(ConsMtSize)=OneBasedOfs
      ConsMtxNze(ConsMtSize)=ACMvalue      ! ... are kept as 1-based
      return ! entry AssignGNWAMtx
!-----
      entry ReturnGNWAMtx(OneBasedCol,OneBasedRow,RCMvalue)
    ! use the fact that entries in ConsMtxNze are indexed iReq+(jVar-1)*mReq
      RCMvalue=0.0 ! default commonly valid
!     OneBasedOfs=OneBasedCol+(OneBasedRow-1)*nVar ! for column=major order
!     OneBasedOfs=OneBasedRow+(OneBasedCol-1)*mReq ! for row-major order
!     if(OneBasedOfs>ConsMtxOfs(ConsMtSize)) return
!     if(OneBasedOfs<ConsMtxOfs(1         )) return ! unlikely
!     do MidIndex=1,ConsMtSize
!       if(ConsMtxOfs(MidIndex)==OneBasedOfs) exit
!     end do
!     if(MidIndex<=ConsMtSize) RCMvalue=ConsMtxNze(MidIndex) ! e;se retain 0.0
    ! after 20070919 search separate row-major arrays for zbCol and zbRow
      zbCol=OneBasedCol-1
      zbRow=OneBasedRow-1
      OnTgtCol=B0
      OnTgtRow=B0
      do iNze=1,ConsMtSize
        if(zbColOfNze(iNze)==zbCol) then ! Nze for this column are contiguous
          OnTgtCol=B1
          if(zbRowOfNze(iNze)==zbRow) then ! (iNze) matching zbCol & zbRow
            OnTgtRow=B1
            exit
          end if
        else
          if(OnTgtCol) exit ! early, since loop has passed all possible matches
        end if
      end do
      if(OnTgtRow) RCMvalue=ConsMtxNze(iNze) ! else retain 0.0
    ! else no match implies ConsMtx(Col,Row) is void
      return ! entry ReturnGNWAMtx
!-----
      entry GNWIncrReq
      iReq=iReq+1
      if(iReq>mReq) call ps(1,'allocation error:  insufficient mReq')
      return
!-----
      entry GNWIncrVar
      jVar=jVar+1
      if(jVar>nVar) call ps(1,'allocation error:  insufficient nVar')
      return
!-----
      entry GNWIncrLoB
      iLbV=iLbV+1
      if(iLbV>nLbV) call ps(1,'allocation error:  insufficient nLbV')
      jLbVec(iLbV)=jVar
      return
!-----
      entry GNWIncrUpB
      iUbV=iUbV+1
      if(iUbV>nUbV) call ps(1,'allocation error:  insufficient nUbV')
      jUbVec(iUbV)=jVar
      return
!-----
      entry SolveGNWLpUsingPcx(Feasible,nCCSegs,LinkNdSN,NodeQtyHardLim)
      BaseFileName='PCX_file'//char(0) ! C-language string-terminator

      PCX_ret_code=PCX_main(MinObj,WrtDetail,BaseFileName,
     +  mReq,nVar,nUbV,nLbV,ConsMtSize,ReVec,jUbVec,jLbVec,
     +  zbColOfNze,zbRowOfNze,
     +  bVect,cVect,xVect,UbValu,LbValu,ConsMtxNze)

      ObjectvEF=0.0d0
      do jVar=1,nVar
        if(xVect(jVar)<-0.5) goto 600 ! 20070605 PCX returns xVect(1)=-1.0 if not OPTIMAL
        r4=cVect(jVar)*xVect(jVar)

        ObjectvEF(2)=ObjectvEF(2)+r4
        if(PrtDetail>1) write(DbgU,'(i6,3e13.6,2a)') jVar,cVect(jVar),
     +    xVect(jVar),ObjectvEF(2),' j,cj,xj,obj ',VarDesc(jVar)
      end do
      if(PrtDetail>1)
     +  write(DbgU,'(a,3e13.6)') ' Accumulated objective value: ',
     +  ObjectvEF
      Feasible=B1
      return ! entry SolveGNWLpUsingPcx
!
  600 continue
!     if(.not.OrigBounds)
!cc   call ps(1,'not-optimal error; check *.dbg')
      write(DbgU,'(a)') ' no OPTIMAL solution; relaxing upper-bounds'
    ! relax the upper bound on the last segment of each cost curve
!     OrigBounds=B0 ! give PCX one more try at converging to Optimal Feasible solution
      do iUbV=1,nLbV ! each of nLbV was assigned an upper bound
        iNode=TNofVar(iUbV)
        if(NodeQtyHardLim(iNode)<0.0) cycle ! negative => immutable
        UbValu(iUbV)=UbValu(iUbV)*2.0
!       write(DbgU,'(i6,f16.1,a)') iUbV,UbValu(iUbV),' relaxed UB'
      end do
      iUbV=nLbV
      do iLink=1,mLink
        iUbV=iUbV+nSegsOI(LinkType(iLink))
        iNode=LinkNdSN(0,iLink)
        if(NodeQtyHardLim(iNode)<0.0) cycle ! negative => immutable
        UbValu(iUbV)=UbValu(iUbV)*2.0
!       write(DbgU,'(i6,f16.1,a)') iUbV,UbValu(iUbV),' relaxed UB'
      end do
      call flush(DbgU)
!     FileName='Presumed INFEASIBLE solution; retrying with UB=UB*10'
!     call WriteStrAt(20,5,trim(FileName))
!     write(DbgU,'(1x,a)') trim(FileName)
      Feasible=B0 ! allow caller to handle the problem
      return ! entry SolveGNWLpUsingPcx
!-----
      entry InitReportGNWFiles(NodeID,NodeType,LinkID)
      do OutU=SnvU,NmcU ! presumes they are contiguous and ascending in order
        write(OutU,'(a)',advance='no') ' '
        call AppendStr(OutU,'Endpoint')
        call AppendStr(OutU,'Year')
        call AppendStr(OutU,'Period')
        call AppendStr(OutU,'Day of Month')
      end do
      do iNode=1,mNode
        write(IDString,'(i4)') NodeID(iNode)
        do while(IDString(1:1)==' ')
          IDString=IDString(2:4)
        end do
        if(NodeType(iNode)==1) ! for Supply nodes
     +  write(SnvU,'( 2a)',advance='no') trim(IDString),AComma
        write(NacU,'( 2a)',advance='no') trim(IDString),AComma
        write(NmcU,'( 2a)',advance='no') trim(IDString),AComma
      end do
      do iLink=1,mLink
        write(IDString,'(i4)') LinkID(iLink)
        do while(IDString(1:1)==' ')
          IDString=IDString(2:4)
        end do
        write(LkvU,'( 2a)',advance='no') trim(IDString),AComma
      end do
      do OutU=SnvU,NmcU ! presumes they are contiguous and ascending in order
        write(OutU,'(1x)',advance='yes') ! writes CrLf after blank
      end do
      return ! entry InitReportGNWFiles
!-----
      entry CheckSolution(NodeType,NodeName,NodeQty,LinkEff)
    ! check total cost
      r8=DotProd8(cVect,xVect,nVar)
      write(DbgU,'(//1x,e17.9,a/)') r8,' objective function'
!
    ! check Supply=Demand
      AccumOfDir=0.0d0
      do iNode=1,mNode
        if(NodeType(iNode)/=-1) cycle ! skip non-Demand nodes
        AccumOfDir(-1)=AccumOfDir(-1)+NodeQty(iNode)
        write(DbgU,'(3f13.3,2i5,2(1x,a))') (AccumOfDir(j),j=1,-1,-2),
     +    NodeQty(iNode),0,iNode,'nominal Demand at',NodeName(iNode)
      end do
      write(DbgU,'(2f13.3,2a/)') (AccumOfDir(j),j=1,-1,-2),
     +  SDAccums,'excluding all but nominal Demand'
!
      r4=1.0
      do jVar=1,nTVar
        iNode=TNofVar(jVar) ! should be a Supply or Demand node
        NodeDir=NodeType(iNode) ! accum Supply in (1), Demand in (-1)
        AccumOfDir(NodeDir)=AccumOfDir(NodeDir)+r4*xVect(jVar)
        write(DbgU,'(3f13.3,2i5,2(1x,a))') (AccumOfDir(j),j=1,-1,-2),
     +    r4*xVect(jVar),jVar,iNode,VarDesc(jVar),NodeName(iNode)
        if(jVar==nSNod) then ! conclude nominal Supply vars
          r4=-1.0 ! deduct following vars from their AccumOfDir
          write(DbgU,'(2f13.3,2a/)') (AccumOfDir(j),j=1,-1,-2),
     +      SDAccums,'excluding slack & surplus vars'
        end if
      end do
      write(DbgU,'(2f13.3,2a/)') (AccumOfDir(j),j=1,-1,-2),
     +  SDAccums,'including slack & surplus vars'
!
      jVar=jVar-1
      do iLink=1,mLink ! augment Demand by losses
        nSegUsd=nSegsOI(LinkType(iLink))
        r4=0.0
        do iSeg=1,nSegUsd
          jVar=jVar+1
          r4=r4+xVect(jVar)
        end do
        r4=r4*(1.0-LinkEff(iLink)) ! loss=LinkQty*(fraction of shipment lost)
        AccumOfDir(-1)=AccumOfDir(-1)+r4
        write(DbgU,'(3f13.3,2i5,2(1x,a))') (AccumOfDir(j),j=1,-1,-2),
     +    r4,jVar,iLink,VarDesc(jVar)
      end do
      write(DbgU,'(2f13.3,a2,2a/)') (AccumOfDir(j),j=1,-1,-2),
     +  QuestionIf(dabs(AccumOfDir(1)-AccumOfDir(-1))/AccumOfDir(1)>
     +  1d-5),SDAccums,'including losses in links'

    ! check lower-bounds
      do iLbV=1,nLbV
        jVar=jLbVec(iLbV) ! jVar==iLbV
        iNode=TNofVar(jVar) ! should be a Supply node
        r4=xVect(jVar)
        f4=LbValu(iLbV)
        if(f4<0.01) then
          f4=1.0 ! preclude attempt to divide by 0
        else
          f4=r4/f4 ! should be >=1
        end if
        write(DbgU,'(2i5,2f13.3,f9.3,a2,2a)') iLbV,jVar,LbValu(iLbV),
     +    r4,f4,QuestionIf(f4<0.999),' LoB,Xj,LoB/Xj for ',
     +    trim(NodeName(iNode))
      end do
      write(DbgU,'(1x)')
!
    ! check upper-bounds
      do iUbV=1,nUbV
        jVar=jUbVec(iUbV)
        r4=xVect(jVar)
        f4=UbValu(iUbV)
        if(f4<0.01) then
          f4=r4 ! as if UbValu were 1.0
        else
          f4=r4/f4 ! should be <=1
        end if
        write(DbgU,'(2i5,f13.1,f13.3,f9.5,a2,2a)') iUbV,jVar,
     +    UbValu(iUbV),r4,f4,QuestionIf(f4>1.001),' UpB,Xj,Xj/UpB for ',
     +    trim(VarDesc(jVar))
      end do
      write(DbgU,'(1x)')
!
      do iReq=1,mReq ! cover the entire constraint set (all are equations in COALNetwk)
        write(DbgU,'(1x)',advance='no') ! expect advance=no to follow
        write(DbgU,'( i5,a)',advance='no') iReq,AComma
        jCol=0
        QtySum=0.0
        do jVar=1,nVar
          f4=FuncGNWAMtx(jVar,iReq) ! very slow
          if(f4==0.0) cycle
          jCol=jCol+1 ! count of non-zero elements in row iReq
          r4=xVect(jVar)
          QtySum=QtySum+f4*r4
          if(PrtDetail>1) then
!         ! assignments below needed to preclude 'initiating I/O during I/O' error
!           YearStr=R4toA4(f4) ! very slow
            DayStr =R4toA4(r4) ! very slow
!           write(DbgU,'( i6.6,2a5,a)',advance='no')
!    +        jVar,YearStr,DayStr,AComma
            write(DbgU,'( i6.6,f6.3,a5,a)',advance='no')
     +        jVar,f4,DayStr,AComma
          end if
        end do
        r4=bVect(iReq)
        if(r4<=0.0) then
          f4=QtySum
        else
          f4=QtySum/r4
        end if
        write(DbgU,'( i3,2f13.3,f9.5,a2,a)',advance='no') jCol,QtySum,
     +    r4,f4,QuestionIf(
     +    ((r4/=0.0).and.(abs(f4-1.0)>0.01)).or.
     +    ((r4==0.0).and.(abs(QtySum)>0.01))),' constraint eqn'
        write(DbgU,'(1x)',advance='yes') ! writes CrLf after blank
      end do ! iReq
      write(DbgU,'(1x)')
!
      call flush(DbgU)
      return ! entry CheckSolution
!-----
      entry ReportGNWSolutionDetails(nCCSegs,Skeptical,
     +  NodeID,NodeType,LinkID,LinkNdSN,
     +  NodeQtyLoB,NodeQtyLim,NodeQtyHardLim,NodeQty,NodeQtyOfLR,
     +  NodePri,NodeMgC,
     +  LinkCap,LinkChg,LinkEff,LinkAvC,LinkMgC,
     +  LinkQty,LinkQtySeg,LinkFunc,LinkQOF,NodeName,PriceSoLR)
      do OutU=SnvU,NmcU ! presumes they are contiguous and ascending in order
        if(WrtDetail==0) exit
        write(OutU,'(1x)',advance='no') ! expect advance=no to follow
        write(OutU,'(  a)',advance='no') '1,' ! dummy Endpoint
        write(OutU,'( 2a)',advance='no') YearStr,AComma ! no Quotes
        call AppendStr(OutU,MonthStr)
        write(OutU,'( 2a)',advance='no') trim(DayStr),AComma ! no Quotes
      end do
!
!cc      if(Skeptical)call CheckSolution(NodeType,NodeName,NodeQty,LinkEff)
    ! preclude cycling on bidirectional links' pricing due to numerical noise
      if(Skeptical) PrtDetail=2
      if(PrtDetail>1) write(DbgU,'(3a)')                   ' piLk iLnk',
     + ' js1L  x(js1Lsr) LinkQtyLsr',
     + ' js1G  x(js1Gtr) LinkQtyGtr x(jS1Gtr)- LinkQtyNet',' ---- ----',
     + ' ---- ---------- ----------',
     + ' ---- ---------- ---------- ---------- ----------'
      do PreviLink=1,mLink
      do iLink=PreviLink+1,mLink
        if(LinkNdBi(iLink)==LinkNdBi(PreviLink)) then ! these comprise a bidirectional pair
        ! zero any noise from the PCX solution, problematic if above Near0Qty
          SpyQCEF(0,2)=LinkEff(PreviLink)
          SpyQCEF(1,2)=LinkEff(iLink)
          nSegUsd=nSegsOI(LinkType(iLink)) ! presumed same as for PreviLink
          jVar=nTVar+nPriorSegs(PreviLink)
          do j=0,1 ! for PreviLink and iLink, respectively
            SpyQCEF(j,0)=0.0d0
            SpyQCEF(j,1)=float(jVar+1) ! save starting index in xVect
            do k=1,nSegUsd ! accumulate LinkQty
              jVar=jVar+1
              SpyQCEF(j,0)=SpyQCEF(j,0)+xVect(jVar)
            end do
            jVar=nTVar+nPriorSegs(iLink)
          end do
          j=0
          if(SpyQCEF(0,0)>SpyQCEF(1,0)) j=1 ! j indexes the Lesser  LinkQty
          k=mod(j+1,2)                      ! k indexes the Greater LinkQty
!
          jCol=nint(SpyQCEF(j,1)) ! index of Seg1 of the Lesser
          jVar=nint(SpyQCEF(k,1)) ! index of Seg1 of the Greater
          if(PrtDetail>1) then
            write(DbgU,'(1x)',advance='no') ! expect advance=no to follow
            write(DbgU,'( i4,i5,2(i5,2f11.4))',advance='no')
     +        PreviLink,iLink,
     +        jCol,xVect(jCol),SpyQCEF(j,0),
     +        jVar,xVect(jVar),SpyQCEF(k,0)
          end if
!
        ! note that only LinkQty(j)*LinkEff(j) is effectively reducing LinkQty(k)
          f4=SpyQCEF(j,0)*SpyQCEF(j,2) ! Qty remaining to be amortized in descending order
          r8=SpyQCEF(k,0)-f4 ! Qty net after the amortization
        ! higer-indexed QtySeg must be amortized first, to maintain monotonic price logic
          do j=nSegUsd-1,0,-1
            r4=amin1(f4,xVect(jVar+j)) ! reduction from segment j+1 of Greater
            f4=f4-r4
            xVect(jCol+j)=0.0 ! amortize the Lesser's QtySeg
            xVect(jVar+j)=xVect(jVar+j)-r4 ! carry excess in f4 to lower segs
          end do ! with xVect(jVar) being the net of Greater-Lesser opposing flows
          if(PrtDetail>1) then
            write(DbgU,'( 2f11.4,a)',advance='no') xVect(jVar),r8,
     +        ' net in Greater after zeroing Lesser'
            write(DbgU,'(1x)',advance='yes') ! writes CrLf after blank
          end if
        end if
!       PreviLink=iLink
      end do ! iLink
      end do ! PreviLink loop, added 20070830 to catch discontiguous iLink
      if(Skeptical) PrtDetail=0
      if(Skeptical)call CheckSolution(NodeType,NodeName,NodeQty,LinkEff)
!
      LinkAvC(1:mLink)=0.0 ! cannot assign without domain specified
      LinkMgC(1:mLink)=0.0
      LinkQtySeg(1:nCCSegs,1:mLink)=0.0
      NodeQtyOfLR(1:mNode)=0.0 ! in case iNode neither S nor D
      LnkQtyP=0.0
      HeaderAbsent=B1
      LnkQCEF=0.0d0
      SupLinkQty=0.0
      QtySum=0.0
      CstSum=0.0
      iLink=0
      do jVar=1,nVar
        r4=xVect(jVar) ! MMCF (per day)
!
        cDummy=' '
        if(jVar<=nSNod) then ! for Supply nodes, r4 is quantity output
!         UbString='infinity'
          iNode=TNofVar(jVar)
          NodeQty(iNode)=r4
          if(WrtDetail>0)
     +    write(SnvU,'( f9.2,a)',advance='no') r4,AComma ! node volume
!    +    call AppendFloat9(SnvU,r4)
!    +    call AppendFloat7(SnvU,cVect(jVar)/1000.) ! NodePri(iNode)
        elseif(jVar<=nTVar) then ! this code-branch is entered only if AmortizeUE
          iNode=TNofVar(jVar)
          NodeQtyOfLR(iNode)=r4
          if(WrtDetail>1) then
            if(NodeType(iNode)>0) then
            ! for Supply nodes, r4 is quantity amortized by DoLR
              ErrMsg=' Supply Dumped'
            else
            ! for Demand nodes, r4 is quantity amortized by SoLR
              ErrMsg=' Demand Destroyed'
            end if
            write(DbgU,'(3i4,i3,f9.2,2(1x,a))') jVar,iNode,
     +        NodeID(iNode),NodeType(iNode),r4,NodeName(iNode),
     +        trim(ErrMsg)
          end if
        else ! (jVar>nTVar) for links, r4 is inlet quantity before losses
          iUbV=jVar-nTVar ! ignoring nLbV within nSNod ...
!         iLink=1+(iUbV-1)/nSegUsd
          if((iLink<mLink).and.(iUbV>nPriorSegs(iLink+1))) then
            iLink=iLink+1
            nSegUsd=nSegsOI(LinkType(iLink))
          end if
!         iSeg=iUbV-(iLink-1)*nSegUsd ! 1+mod(iUbV-1,nSegUsd)
          iSeg=iUbV-nPriorSegs(iLink)
          iUbV=iUbV+nLbV ! ... now including nLbV
          LinkQtySeg(iSeg,iLink)=r4
          QtySum=QtySum+r4
          if(iSeg<nCCSegs) CstSum=CstSum+r4*cVect(jVar) ! $ since cVect is in $/MMCF
          if((r4>Near0Qty).and.(iSeg<nSegUsd)) ! latter added 20070702 per GAT
     +      LinkMgC(iLink)=cVect(jVar) ! marginal usage cost $/(MMCF at inlet)

          OrigUB=UbVOrg(iUbV)
          if((r4>OrigUB*1.001).and.(r4>Near0Qty))
     +      cDummy='X' ! allow for some numerical slop
          if(iSeg==nSegUsd) then
            if(WrtDetail>0)

     +      write(LkvU,'( f9.2,a)',advance='no') QtySum,AComma ! link volume
            LinkQty(iLink)=QtySum ! line inlet quantity before losses
            LinkQOF(iLink)=amax1(QtySum-LinkCap(iLink),0.0) ! report QtyOverFlow

            if(iSeg==nCCSegs) then ! only for LinkType(1) if Electric
              QtySum=QtySum-r4 ! after 20070702, exclude nCCSegs-usage from pricing
              LnkQCEF(0,1)=LnkQCEF(0,1)+r4
              LnkQCEF(1,1)=LnkQCEF(1,1)+r4*cVect(jVar)
            end if
            if(QtySum>Near0Qty) then
              LnkQtyP(iLink)=QtySum ! Qty useful for pricing
              LinkAvC(iLink)=CstSum/
     +        (QtySum*LinkEff(iLink)) ! WtdAvg delivered cost $/MMCF
              LnkQCEF(0,0)=LnkQCEF(0,0)+QtySum
              LnkQCEF(1,0)=LnkQCEF(1,0)+CstSum
            end if
            if(SupLinkQty<QtySum) SupLinkQty=QtySum
            if(PrtDetail>0) write(DbgU,'(2i4,i6,6f12.3,a)')iLink,iSeg,
     +        jVar,CstSum,QtySum,LinkEff(iLink),cVect(jVar),
     +        LinkAvC(iLink),LinkMgC(iLink),' link AvC,MgC'
            QtySum=0.0
            CstSum=0.0
          end if
        end if
        if(cDummy/=' ') then
          if(HeaderAbsent) then
            write(DbgU,'(/a)')
     +' jVar Sg       x(j) <=UpBndOrg   OFF  x(j) var description Lnk S'
            write(DbgU,'(a)')
     +' ---- -- ---------- - -------- ----- --------------------- --- -'
            HeaderAbsent=B0
          end if
          OverflowFactor=1.0
          if(OrigUB>0.0) OverflowFactor=r4/OrigUB
          write(UbString,'(f8.1)') OrigUB
          write(DbgU,'(i5,i3,f11.3,2(1x,a),f6.3,1x,a,i4,i2)')
     +      jVar,iSeg,r4,
     +      cDummy,UbString,OverflowFactor,VarDesc(jVar)(7:27),
     +      LinkID(iLink),NodeType(LinkNdSN(0,iLink))
        end if
      end do ! jVar
      call flush(DbgU)
!cc   if(B0) goto 8900 ! return ! pricing is N.A.
    ! preclude cycling on closed paths due to numerical noise
!     write(DbgU,'(2e12.5,a)') 0.0001,SupLinkQty,' SupLinkQty'
      SupLinkQty=SupLinkQty*0.0001
      do iLink=1,mLink
        if(LnkQtyP(iLink)<SupLinkQty) then ! block use of iLink in pricing ...
          LnkQtyP(iLink)=0.0 ! ... without affecting LinkQty reported to caller
!         LinkAvC(iLink)=0.0 ! these two may ...
!         LinkMgC(iLink)=0.0 ! ... remain non-zero
        end if
      end do
!
!
    ! report nodal prices
      NodeSrcNip=0
      NodeSrcQty=0.0
      NodeSrcCst=0.0
!
    ! restore nodal and link prices for Supply nodes
      do iNode=1,mNode
        if(NodeType(iNode)/=1) then ! non-Supply node
          NodePri(iNode)=0.0 ! force refresh by DerivePriceFromSources()
          NodeMgC(iNode)=0.0 ! default for nodes with zero NodePri
        else ! for Supply nodes, assign marginal cost to nominal price
          if(NodeQtyLim(iNode)>=0.0) then
            NodePri(iNode)=NodePri(iNode)/1.0e-12 ! restore nominal price
            iLink=LkUsdFrom(iNode)
!           if(LinkNdSN(0,iLink)/=iNode) call ps(1,'logical error')
            r4=NodePri(iNode)*1000. ! $/MMCF
            LinkAvC(iLink)=LinkAvC(iLink)-r4
            LinkMgC(iLink)=LinkMgC(iLink)-r4
            LinkChg(iLink)=LinkChgOrg(iLink) ! restore unperturbed value
            LnkQCEF(1,0)=LnkQCEF(1,0)-r4*LnkQtyP(iLink) ! ($/MMCF)*MMCF (LinkEff==1.0)
!           nSegUsd=nSegsOI(LinkType(iLink))
!           LnkQCEF(1,1)=LnkQCEF(1,1)-r4*LinkQtySeg(nSegUsd,iLink)
            if(LinkType(iLink)==1)
     +      LnkQCEF(1,1)=LnkQCEF(1,1)-r4*LinkQtySeg(nCCSegs,iLink)
!           write(DbgU,'(2i5,f7.3,e9.2,f17.3,a)')iNode,iLink,NodePri
!    +        (iNode),LinkChg(iLink),LnkQCEF(1,0),
!    +        ' NP,LC,LCS aft restore'
          end if
          NodeMgC(iNode)=NodePri(iNode)
        end if
      end do
    ! recursively trace paths to all sources for each Demand node
      DmdQCEF=0.0d0
      do iNode=1,mNode
        if(NodeType(iNode)/=1) then ! non-Supply node
          do j=1,mLink ! abort loop after mLink Cyclic conditions
            CycleOrgD=0
            Cyclic=B0
            i=0
            call DerivePriceFromSources(Cyclic,DbgU,i,iNode,mNode,
     +        SupnLkAtNd,
     +        nLinkInto,
     +        NdLkdInto(1,1),
     +        LkUsdInto(1,1),CycleOrgD,LnkQtyP,LinkEff,
     +        LinkAvC,LinkMgC,
     +        NodePri,NodeMgC ! these two arrays are filled for iNode sources
     +        )
            if(.not.Cyclic) exit ! else restart at i=0 with revised LinkQtyP
          end do
! 060708
          if((NodeType(iNode)==-1).and.(abs(NodeQtyOfLR(iNode))>0.01))
     +      then ! adjust reported prices just before writing them
! 082108. testing for bomb in this section.
              NodePri(iNode)=(
     +        NodePri(iNode)*NodeQty(iNode)+
     +        PriceSoLR     *NodeQtyOfLR(iNode))/(
     +        NodeQty(iNode)+NodeQtyOfLR(iNode)) ! incorporate OfLR into array prices
!
            NodeMgC(iNode)=PriceSoLR
          end if
         end if
        if(WrtDetail>0) then
          write(NacU,'( f9.5,a)',advance='no') NodePri(iNode),AComma ! node avg cost
          write(NmcU,'( f9.5,a)',advance='no') NodeMgC(iNode),AComma ! node mgn cost
        end if
        if(NodeType(iNode)==0) then ! for non-Supply, non-Demand nodes
          NodeQty(iNode)=0.0
          do k=1,nLinkInto(iNode) ! accumulate links' outlet quantity (MMCF)
            iLink=LkUsdInto(k,iNode)
            NodeQty(iNode)=NodeQty(iNode)+LinkQty(iLink)*LinkEff(iLink)
          end do
        end if ! note that NodeQty is not used to DerivePriceFromSources()
        if(PrtDetail>0)write(DbgU,'(3i4,f9.1,f9.5,f9.3,a)')iNode,NodeID(
     +    iNode),NodeType(iNode),NodeQty(iNode),
     +    NodePri(iNode),NodeMgC(iNode),' node Qty,AvC,MgC'
        if(NodeType(iNode)/=-1) cycle ! skip non-Demand nodes
        DmdQCEF(0,2)=DmdQCEF(0,2)+NodeQty(iNode) ! MMCF
        DmdQCEF(1,2)=DmdQCEF(1,2)+NodeQty(iNode)*NodePri(iNode)*1000. ! $
!       write(DbgU,'(i4,4e12.5,a)') iNode,NodeQty(iNode),NodePri(iNode),
!    +    DmdQCEF(:,2),' DS'
      end do ! iNode
!
      i=0
      nZPNodesRem=0
      do iNode=1,mNode
        if(NodePri(iNode)==0.0) then
          nZPNodesRem=nZPNodesRem+1
          write(DbgU,'(3i4,2f9.4,a)') i,iNode,NodeType(iNode),
     +      NodePri(iNode),NodeMgC(iNode),' NodePri&MgC_org'
        end if
      end do
      do while(nZPNodesRem>0)
        nZPNodesOrg=nZPNodesRem
        i=i+1 ! count iterations
        ! first may not fill all nZPNodesRem if low-iDNod nodes are far from Supply
        do iDNod=1,mNode
          if((NodeType(iDNod)==0).and.(NodePri(iDNod)==0.0)) then
          ! attempt to back-fill missing prices using same (but unweighted)
          ! average price logic as used in DerivePriceFromSources
            SimAvgPri=0.0
            SupSourMC=0.0
            nNZPSour=0
            do kLink=1,nLinkInto(iDNod)
              iLink=LkUsdInto(kLink,iDNod)
              iNode=NdLkdInto(kLink,iDNod)
              if(NodePri(iNode)<=0.0) cycle ! iNode is possibly an Interm node
              nNZPSour=nNZPSour+1
             SourMgCst=NodeMgC(iNode)/LinkEff(iLink)+
     +                                             LinkMgC(iLink)/1000. ! $/MCF
             DelivdPri=NodePri(iNode)/LinkEff(iLink)+
     +                                             LinkAvC(iLink)/1000. ! $/MCF
              SimAvgPri=SimAvgPri+DelivdPri
              if(SupSourMC<SourMgCst) SupSourMC=SourMgCst
            end do
            if(nNZPSour>0) then
              if(nNZPSour>1) SimAvgPri=SimAvgPri/float(nNZPSour)
              NodePri(iDNod)=SimAvgPri
              NodeMgC(iDNod)=SupSourMC
              write(DbgU,'(3i4,2f9.4,a)') i,iDNod,nNZPSour,
     +          NodePri(iDNod),NodeMgC(iDNod),' NodePri&MgC_rev'
              nZPNodesRem=nZPNodesRem-1
            else ! this 0-price was not (yet) filled
              write(DbgU,'(3i4,2f9.4,a)') i,iDNod,nZPNodesRem,
     +          NodePri(iDNod),NodeMgC(iDNod),' NodePri&MgC_rem'
            end if
          end if
        end do ! iDNod with NodeType==0
        if(nZPNodesRem==nZPNodesOrg) exit ! no progress in reducing # of 0-priced nodes
      end do ! while(nZPNodesRem>0)

    ! done using LinkAvC and LinkMgC as the costs of only operating the link,
    ! now express them as the costs of outlet COAL in $/MCF:
      do iLink=1,mLink
        iNode=LinkNdSN(0,iLink) ! base costs on those at source node

        LinkMgC(iLink)=NodeMgC(iNode)/LinkEff(iLink)+LinkMgC(iLink)/
     +    1000. ! formula used in DerivePriceFromSources() for SourMgCst
        LinkAvC(iLink)=NodePri(iNode)/LinkEff(iLink)+LinkAvC(iLink)/
     +    1000. ! same form as above
      end do
      SpyQCEF=0.0d0
      do jVar=1,nSNod ! Supply Nodes' Qty are the first nSNod variables
        r4=xVect(jVar) ! MMCF/D inc last-segment usage
        iNode=TNofVar(jVar)
        iLink=LkUsdFrom(iNode)
        nSegUsd=nSegsOI(LinkType(iLink))
        QtyFsg=LinkQtySeg(nSegUsd,iLink)
        SNodPri=NodePri(iNode)*1000. ! $/MMCF
        r4=r4-QtyFSg
        SpyQCEF(0,0)=SpyQCEF(0,0)+r4 ! MMCF
        SpyQCEF(1,0)=SpyQCEF(1,0)+r4*SNodPri ! $
        SpyQCEF(0,1)=SpyQCEF(0,1)+QtyFSg
        SpyQCEF(1,1)=SpyQCEF(1,1)+QtyFSg*SNodPri
        if(PrtDetail>0)
     +    write(DbgU,'(2i4,5f9.2,2(f12.2,f12.0),a)')
     +    jVar,iNode,NodeQtyLoB(iNode),NodeQtyLim(iNode),NodePri(iNode),
     +    r4,QtyFsg,SpyQCEF(:,0:1),' SNode Limit,Pri,QtyEF,SumQCEF'
      end do
!
      r4=0.0 ! to exclude LnkQ from the allocation of DmdQ
      do j=0,1 ! explete the cumulative arrays
        LnkQCEF(j,2)=LnkQCEF(j,0)+LnkQCEF(j,1)
        SpyQCEF(j,2)=SpyQCEF(j,0)+SpyQCEF(j,1)
      ! allocate DmdQ in proportion wrt SpyQ, DmdC wrt (SpyC+LnkC)
        r8=SpyQCEF(j,2)+LnkQCEF(j,2)*r4
      ! DmdC above ignored the final-segment costs because DemandPri did
        if(j==1) DmdQCEF(j,2)=r8 ! enforce payment for services
        do k=0,1
          DmdQCEF(j,k)=DmdQCEF(j,2)*(
     +    SpyQCEF(j,k)+LnkQCEF(j,k)*r4)/r8
!         write(DbgU,'(2i2,f4.1,-6p,4f11.4)') j,k,r4,r8,
!    +      SpyQCEF(j,k),LnkQCEF(j,k),DmdQCEF(j,k)
        end do
        r4=1.0 ! to inc LnkC in the allocation of DmdC
      end do
      if(PrtDetail>0) write(DbgU,'(2a/,(0p,3f9.1,-6p,6f11.4,a,i1))')
     +  ' SuplyQty  LinkQty DemndQty SupplyCost   LinkCost',
     +  ' DmdAlloCst Suply+Link S+Link-Dmd ObjectivFn audit',(
     +  SpyQCEF(0,k),LnkQCEF(0,k),DmdQCEF(0,k),
     +  SpyQCEF(1,k),LnkQCEF(1,k),DmdQCEF(1,k),
     +  SpyQCEF(1,k)+LnkQCEF(1,k), ! should match the reported objective value
     +  SpyQCEF(1,k)+LnkQCEF(1,k)-DmdQCEF(1,k), ! SC+LC-DP should be 0 if prices are correct
     +  ObjectvEF(k),' audit',k,k=0,2)
 8900 call timer(EndCentiSec)
      ExecET=EndCentiSec-OrgCentiSec
      write(DbgU,'(f8.2,a,2i11)') float(ExecET)*0.01,
     +  ' seconds elapsed during execution ' ! ,OrgCentiSec,EndCentiSec
!
      do OutU=SnvU,NmcU ! presumes they are contiguous and ascending in order
        if(WrtDetail==0) exit
        write(OutU,'(1x)',advance='yes') ! writes CrLf after blank
      end do
      return ! entry ReportGNWSolutionDetails
!-----
    ! NEED to incorporate LinkExpLim below
      entry ReportGNWEconomicExpansion(YearSpan,
     +  LinkCap,LinkFunc,LinkAvQSeg,LinkExpQty)
      jVar=nTVar
      do iLink=1,mLink
        nSegUsd=nSegsOI(LinkType(iLink))
        jVar=jVar+nSegUsd ! valid for the final segment
        LinkExpQty(iLink)=0.0
        ThisQtySeg(:)=LinkAvQSeg(:,iLink) ! before offloading of peak segments
        if(ThisQtySeg(1)<=Near0Qty) then
          write(DbgU,'(/i4,a)') iLink,' link unused'
          cycle
        else
          if(LinkExpCst(iLink)<=0.0) then ! cost data N.A.
            write(DbgU,'(/i4,f12.4,a)') iLink,ThisQtySeg(1),
     +      ' nth link has seg#1 Qty>0 but no expansion-cost data'
            cycle
          end if
        end if
        ExpansionMultiple=1.0 ! of original capacity
      ! cost above which savings can be had by expanding this link's capacity,
      ! in $/(MMCF[per day]) for duration of usage averaged in ThisQtySeg:
        CostCriterion=YearSpan*LinkCCRF(iLink)*LinkExpCst(iLink) ! $/(MMCF[per day])
        write(DbgU,'(1x)')
        do iSeg=nSegUsd,2,-1
          write(DbgU,'(2i4,f14.6,f12.2)') iLink,iSeg,ThisQtySeg(iSeg),
     +      cVect(jVar-nSegUsd+iSeg)
          if(ThisQtySeg(iSeg)<Near0Qty) cycle
          if(iSeg==nSegUsd) then
            r4=ThisQtySeg(iSeg-1)*LinkFunc(iSeg  ,2,iLink)
     +                           /LinkFunc(iSeg-1,2,iLink)
            if(ThisQtySeg(iSeg)>r4) then ! limit ThisQtySeg to that of the
            ! next lower segment as prorated by the segment capacities
              ThisQtySeg(iSeg)=r4
              write(DbgU,'(2i4,f14.6,a)') iLink,iSeg,ThisQtySeg(iSeg),
     +          ' prorated by segment bounds'
            end if
          end if
!         if(ExpansionMultiple==1.0) write(DbgU,'(1x)')
          PriorExpMult=ExpansionMultiple
          ExpansionMultiple=1.0/LinkFunc(iSeg-1,0,iLink) ! if approved as economic
          ExpansionQty=LinkCap(iLink)*(ExpansionMultiple-PriorExpMult)
          MarginalOamDecr=LinkFunc(iSeg,1,iLink) ! high cost of offloaded segment
          do jSeg=iSeg-1,1,-1 ! reduce savings by cost of lower segments
            write(DbgU,'(2i4,3f7.2)') iLink,jSeg,
     +        LinkFunc(jSeg,1,iLink),
     +        LinkFunc(jSeg,2,iLink),MarginalOamDecr
            MarginalOamDecr=MarginalOamDecr
     +        -LinkFunc(jSeg,1,iLink) ! lower cost of segment being loaded
     +        *LinkFunc(jSeg  ,2,iLink) ! relative weight
     +        /LinkFunc(iSeg-1,0,iLink)
          end do
          ExpDollarGain=MarginalOamDecr*ThisQtySeg(iSeg)
     +      -CostCriterion*ExpansionQty
          write(DbgU,'(2i4,f7.4,3f9.2,f14.2,f9.5,a)') iLink,iSeg,
     +      MarginalOamDecr,CostCriterion,
     +      ThisQtySeg(iSeg),ExpansionQty,ExpDollarGain,
     +      ExpansionMultiple,' EEM'
          write(DbgU,'(10f7.1)') (ThisQtySeg(jSeg),jSeg=1,nSegUsd)
          write(DbgU,'(10f7.2)') (LinkFunc(jSeg,1,iLink),jSeg=1,nSegUsd)
          write(DbgU,'(10f7.4)') (LinkFunc(jSeg,2,iLink),jSeg=1,nSegUsd)
          if(ExpDollarGain<0.0) exit ! expansion is not economic
          LinkExpQty(iLink)=LinkCap(iLink)*(ExpansionMultiple-1.0) ! MMCF[per day]
          write(DbgU,'(1x,f9.2,a)') LinkExpQty(iLink),' EEQ approved'
        ! distribute the offloaded quantity over lower-indexed segment(s)
          do jSeg=iSeg-1,1,-1
            ThisQtySeg(jSeg)=ThisQtySeg(jSeg)+ThisQtySeg(iSeg)
     +        *LinkFunc(jSeg  ,2,iLink) ! relative weight
     +        /LinkFunc(iSeg-1,0,iLink)
          end do
        end do ! iSeg
        if(iSeg==1)write(DbgU,'(2i4,f14.6)') iLink,iSeg,ThisQtySeg(iSeg)
      end do ! iLink
      return ! entry ReportGNWEconomicExpansion
!-----
      end ! recursive subroutine COALNetwk
      subroutine OpenFmFile(iUnit,iFS,iCC,iStat,
     +  MustUsePrefix,DirPrefix,FileParm)
      integer*4 iUnit,iFS,iCC,iStat
      integer*2 iCh,j,LengNB
      logical*1 MustUsePrefix,StopIfAbsent
      character*255 DirPrefix,FilePath
      character*255 FileParm,FileName
      character*7
     +  FSType(0:2)/'Old','New','Replace'/, ! Replace is same as Unknown
     +  CCType(2)/'List','Fortran'/
!
      StopIfAbsent=(iStat==0) ! else caller will inspect iStat
      iStat=0 ! default valid if file is extant
      FileName=FileParm
      if(LengNB(FileName)<=0) then
        iStat=-1
        goto 200
      end if
      if(MustUsePrefix) goto 100
    ! look first in current dir
!     write(*,'(i3,2(1x,a))') LengNB(FileName),trim(FileName),FileName
      do j=255,1,-1 ! blank any control characters, since stack-var's tail ...
        iCh=iChar(FileName(j:j))
        if((32<iCh).and.(iCh<123)) exit ! accept ASCII chars on [33,122]
        FileName(j:j)=' ' ! ... from concatenation may contain garbage bytes
      end do
!     write(*,'(i5,3(i3,1x,a),a)') iUnit,LengNB(FileName),FileName,
!    +  iFS,FSType(iFS),iCC,CCType(iCC),' before open()'
      open(iUnit,FileName,form='formatted',status=FSType(iFS),
     +  CarriageControl=CCType(iCC),err=100)
      return
    ! look next in directory specified
  100 if(LengNB(DirPrefix)>0) then
        FilePath=trim(DirPrefix)//FileName
      else ! path prefix is blank
        FilePath=FileName
      end if
      open(iUnit,FilePath,form='formatted',status=FSType(iFS),
     +  CarriageControl=CCType(iCC),err=200,ioStat=iStat)
      return
  200 if(.not.StopIfAbsent) return ! with iStat/=0 implying 'file not found'
      call ps(1,'Error opening formatted file: '//FilePath)
      end ! subroutine OpenFmFile
!-----
      subroutine AppendStr(u,s) ! enclose s in double-quotes, add a trailing comma
      integer*4 u
      character*(*) s
!
      write(u,'( 4a)',advance='no') '"',trim(s),'"',","
      end
!-----
      subroutine WriteStrAt(Row,Col,Message)
      integer*4 Row,Col
      integer*2 jF,LengNB
      character*(*) Message
!
!      write(*,'(1x,a)',advance='yes') trim(Message)
      end
!-----
      subroutine ps(ErrorLevel,message) ! ps is a mnemonic for Print and Stop
!     message codes per GAT on 20060926:
!       14       => user terminated (N.A. to this subroutine)
!       15 or 16 => bomb/crash/error
!       17 or 18 => successful execution
      logical*1 CurrentBatModeState,BatchMode/.true./ ! default implying 'pause to read message'
      logical*4 UnitInUse
      integer*4 iStat,ErrorLevel
      integer*2 ExitCode
      character*(*) message
!
!     opening unit 4 inside a .DLL does not make it writable outside the .DLL;
!     .EXE-caller of .DLL finds UnitInUse below false on 20040628, so the .DLL
!     must write message and flush(4) just before returning to the caller
      inquire(unit=4,opened=UnitInUse,err=100,ioStat=iStat)
      if((iStat==0).and.UnitInUse) then ! file is open
        write(4,'(1x  )',advance='yes') ! terminate prior messages
        write(4,'(1x,a)',advance='yes') message
        CALL FLUSH(4) ! close(4)
      end if
  100 continue

      if(ErrorLevel/=0) then ! allow operator to read the diagnostic message

        if((ErrorLevel==1).and.(.not.BatchMode)) then
			call end_program("coal_obj:0001 - pause encountered. " //
     + "Terminated.")

        endif
        ExitCode=15 ! failure due to error
      else
        ExitCode=17 ! success
      end if

    ! above disabled 20060717 to allow the ExitCode assigned above to be useful
      CALL EXIT(ExitCode)
      return ! pro-forma; unnecessary here
!-----
      entry SetBatchMode
      BatchMode=.true. ! implying 'do not pause to read error message'
      return ! entry SetBatchMode
!-----
      entry GetBatchMode(CurrentBatModeState)
      CurrentBatModeState=BatchMode
      end ! subroutine ps
!-----
      integer*2 function LengNB(s)
    ! returns the length of the non-blank part of s,
    ! but note that leading blanks are not discounted
      integer*2 i
      character*(*) s
      character*1 c
!
      do i=len(s),1,-1 ! get the index of the last printable non-blank char

        c=s(i:i) ! breaking the above into two suffices to solve the stack exhaustion
        if(iChar(c)>32) exit
      end do
      LengNB=i ! possibly 0
      end            
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!      
      subroutine IndexedSortAlphaOrder(nItems,iOrg,sPos,StrKey)
!     sorts nItems strings in StrKey array in alphabetic order(iOrg array)
      integer*2 nItems,i,j,k,Gap,iHold,iOrg(*),sPos(*)
      character*(*) StrKey(*)
!
      do i=1,nItems
        iOrg(i)=i
      end do
      Gap=nItems/2
      do while(Gap>0)
        do i=Gap+1,nItems
          j=i-Gap
          do while(j>0)
            k=j+Gap
            if(LLE(StrKey(iOrg(j)),StrKey(iOrg(k)))) then
              j=0 ! break the while loop (assign j=-1 for 0-based arrays)
            else ! interchange the pair of iOrgs
              iHold=iOrg(j)
              iOrg(j)=iOrg(k)
              iOrg(k)=iHold
            end if
            j=j-Gap
          end do
        end do
        Gap=Gap/2
      end do
      do k=1,nItems ! invert the index array iOrg to assign sorted position sPos
        sPos(iOrg(k))=k
      end do

      RETURN
      end ! subroutine IndexedSortAlphaOrder
!-----
      subroutine EnumerateFields(s,jSupFoI,jOfs,OutUnit) ! useful on lines of DSF files
      character*(*) s
      character*255 Field(385) ! one more than necessary on 20060908
      character*1 DummyCh
      integer*4 i,j,jSupFoI,jOfs,OutUnit,jLimit,nTrunc
!
    ! jSupFoI is the nominal upper-limit on field-indices of interest
    ! jOfs is the number of leading fields to skip in the count
      jLimit=385 ! to force reading beyond the last field of possible interest
      do j=1,jLimit
        Field(j)='<>'
      end do
!
    ! since read(s,*) evidently counts fields before entering the implicit-do
    ! loop, we determine the feasible limit on readable fields by experimentation
      do nTrunc=0,385
        j=0
        read(s,*,err=101,end=101) (DummyCh,i=1,jOfs),
     +    (Field(j),j=1,385-nTrunc)
  101   write(OutUnit,'(3i5,a,i3.3)')jOfs,jSupFoI,j,
     +    ' Ofs,Sup,Max in Enum with #Truncated=',nTrunc
        if(j>0) exit ! with the lowest feasible nTrunc (trailing fields missing)
        if(jLimit<=0) exit
        jLimit=jLimit-1 ! j==0 above => s had too few fields, => error or EoF
      end do
!
      j=0
      read(s,*,err=102,end=102) (DummyCh,i=1,jOfs),(Field(j),j=1,jLimit)
  102 continue ! with (j) expected to be neither in error nor beyond end-of-string

      write(OutUnit,'(3i5,a)')jOfs,jSupFoI,jLimit,' Ofs,Sup,Lim in Enum'
      if(jLimit>jSupFoI) jLimit=jSupFoI ! possibly < 385
      write(OutUnit,'(i4,1x,a)') (j,Field(j),j=1,jLimit)
      end

      integer*2 function CI_index(String,Target) ! case-insensitive index()
      character*(*) String,Target
      character*4095 UcStr,UcTgt
      integer*2 LenStr,LengNB
!
      LenStr=LengNB(String)
      if(LenStr>4095) call ps(1,'CI_index excessive length of arg 1')
      UcStr=ucase(String)
      UcTgt=Target

      CI_index=index(UcStr,Target)
      end

      subroutine MakeUpper(s,n)
      integer*2 n,i
      character*(*) s ! len(s) is undependable
      character*1 c
!
      do i=1,n
        c=s(i:i) ! convert ith char to upper-case
        if(('a'<=c).and.(c<='z')) s(i:i)=char(ichar(c)-32)
      end do
      end
