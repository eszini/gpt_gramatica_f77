
!     RDI_OBJT.FOR
!     Copyright(c) Global Energy Decisions 2000
!
!     Created: 11/7/2006 10:33:11 AM
!     Author : Tom Sweet
!     Last change: TS 11/7/2006 10:46:53 AM


!
!
!

!
!                 ROUTINE TO CONVERT RDI FILE
!
!                      COPYRIGHT (C) 1997
!                 M.S. GERBER & ASSOCIATES, INC.
!                     ALL RIGHTS RESERVED
!

!
      RECURSIVE SUBROUTINE RDI_OBJECT
      use end_routine, only: end_program, er_message
      use filename_tracker
      USE SIZECOM
      USE spindriftlib
      USE prod_arrays_dimensions
      SAVE
!!! RECORD LENGTH UPDATED WITH VARIABLE CHANGES
      LOGICAL(kind=1) :: OHIO_EDISON=.FALSE.
      INTEGER(kind=2) :: DELETE,INUNIT,IREC,LRECL=355 ! ASSUMES 61 VARIABLES
      INTEGER :: IOS
!
      INTEGER(kind=2) :: UNIT_IN_NUM=1752,R_RDI_IN_UNIT, &
                  UNIT_OUT_NUM=1753,R_RDI_OUT_UNIT, &
                  NUMBER_OF_RDI_VARIABLES,MONTHS_PER_PLANT(:),MO, &
                  PLANT,MAX_RDI_PLANTS,RDI_RECORDS=0,RDI_CL_RECORDS=0, &
                  RDI_EL_RECORDS=0,COUNT, &
                  MONTHS_PER_UNIT(12)
      ALLOCATABLE :: MONTHS_PER_PLANT
      PARAMETER (NUMBER_OF_RDI_VARIABLES= 61,MAX_RDI_PLANTS=3000)
      CHARACTER(len=5) :: BASE_FILE_NAME,OVERLAY_FAMILY_NAME, &
                  RDI_POWERDAT_FILE
      CHARACTER(len=4) :: RDI_NAME
      CHARACTER(len=20) :: LAST_PLANT_NAME,LAST_REGION_NAME, &
                   LAST_PRIME_MOVER,LAST_MEMBER_NAME, &
                   NULL_C20='NULL DATA',CHECK_PLANT_NAME(:)
      ALLOCATABLE :: CHECK_PLANT_NAME
      CHARACTER(len=256) :: FILE_NAME,R_FILE_NAME,TEMP_FILE_NAME
      CHARACTER(len=256) :: BASE_FILE_DIRECTORY
      CHARACTER(len=256) :: DATA_DRIVE,OUTPUT_DIRECTORY
      LOGICAL(kind=4) ::   FILE_EXISTS,RDI_FILE_EXISTS=.FALSE., &
                  R_RDI_FILE_EXISTS
!  DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER(len=1024) :: RECLN
!  DECLARATION FOR DAY TYPE DETERMINANTS
!
! RDI DATA BASE PARAMETERS
!
      INTEGER(kind=2) :: NUM_OF_GEN_UNITS=0,NUMBER_OF_COMPANIES, &
                  R_NUMBER_OF_UNITS,R_NUMBER_LOCAL_TYPE
      CHARACTER(len=25) :: RDI_VARIABLE_NAME(:)
!
! RDI VARIABLES
!
      INTEGER(kind=2) :: &
               YEAR, & !  NUMBERS ! INT
               MTH ! INT
      REAL(kind=4) :: &
               PERCENT_OWNED, &
               DEMONSTRATED_CAPACITY_MW, &
               NET_GENER_MWH, &
               CAPACITY_FACTOR_PERCENT, &
               FUEL_COST_USD, &
               FUEL_ADJUSTMENTS_USD, &
               TOTAL_FUEL_COST_USD, &
               INCREMENTAL_FUEL_USDMWH, &
               FUEL_USDMWH, &
               VAR_NON_FUEL_OM_USD, &
               FIXED_NON_FUEL_OM_USD, &
               TOTAL_PROD_COSTS_USD, &
               VARIABLE_OM_USDMWH, &
               TOTAL_VAR_PROD_USDMWH, &
               FIXED_OM_USDKW_MTH, &
               TOTAL_PROD_USDMWH, &
               AVERAGE_HEAT_RATE, &
               DATE ! REAL
      CHARACTER(len=20) :: &
               COMPANY_NAME,COMP_ID,COMP_ABBREV, &
               ENTITY_TYPE,COMPANY_TYPE,ADDRESS_1, &
               ADDRESS_2,CITY,STATE,ZIP_CODE, &
               PHONE_NUM,COUNTRY,NERC_REGION, &
               SUB_REGION,CONTROL_AREA,PLAN_AREA, &
               HOLDING_COMPANY,MEMBER,CENSUS_REGION, &
               STOCK_EXCH,STOCK_SYMBOL,SOURCE_FORM, &
               FISC_YEAR,PEAK_SEASON,PLANT_NAME,PLANT_ID, &
               PLANT_ADDRESS_1,PLANT_ADDRESS_2,PLANT_CITY, &
               PLANT_COUNTY,PL_STATE,PLANT_ZIP_CODE, &
               PLANT_NERC,PLANT_SUB_REGION, &
               PM_ABBREV,PRIME_MOVER,PM_GROUP, &
               PRIME_MOVER_SHORT_DESC, &
               PRIME_MOVER_DESCRIPTION, &
               PRIME_FUEL, &
               PLANT_FLAG ! CHAR
!
      ALLOCATABLE :: RDI_VARIABLE_NAME
!
      CHARACTER(len=16) :: FILE_TYPE='RDI Data        '
      CHARACTER(len=2) :: RDI_OL='BC'
!
! DATA TO SUPPORT MONTHLY TO ANNUAL CONVERSIONS
!
      REAL(kind=4) :: &
            LOCAL_CAP_MONTHS, &
            LOCAL_CAPACITY, &
            LOCAL_GEN_MONTHS, &
            LOCAL_GENERATION, &
            LOCAL_AHR, &
            LOCAL_FUEL_COST, &
            LOCAL_VAROM_COST, &
            MONTHS_TO_YEARS, &
            ANNUAL_GENERATION, &
            ANNUAL_AHR, &
            ANNUAL_FUEL_USDMMBTU, &
            ANNUAL_VAROM_USDMWH, &
            ANNUAL_FUEL_COST, &
            ANNUAL_VAROM_COST, &
            ANNUAL_CAPACITY, &
            ANNUAL_CAP_FACTOR, &
            MONTH_GENERATION(12), &
            MONTH_CAPACITY(12)
      LOGICAL(kind=1) :: LAHEY_LF95
      CHARACTER(len=30) :: SCREEN_OUTPUT
!
! END OF DATA DECLARATIONS
!
!
!
! PER SCOTT JONES COMPANY.TXT 9/5/97. GAT.
!
      ALLOCATE (RDI_VARIABLE_NAME(NUMBER_OF_RDI_VARIABLES))
!
      RDI_VARIABLE_NAME(1) = "Company Name"
      RDI_VARIABLE_NAME(2) = "Comp ID"
      RDI_VARIABLE_NAME(3) = "Comp Abbrev"
      RDI_VARIABLE_NAME(4) = "Entity Type"
      RDI_VARIABLE_NAME(5) = "Company Type"
      RDI_VARIABLE_NAME(6) = "Address 1"
      RDI_VARIABLE_NAME(7) = "Address 2"
      RDI_VARIABLE_NAME(8) = "City"
      RDI_VARIABLE_NAME(9) = "State"
      RDI_VARIABLE_NAME(10) = "Zip Code"
      RDI_VARIABLE_NAME(11) = "Phone NUM"
      RDI_VARIABLE_NAME(12) = "Country"
      RDI_VARIABLE_NAME(13) = "NERC Region"
      RDI_VARIABLE_NAME(14) = "Sub-Region"
      RDI_VARIABLE_NAME(15) = "Control Area"
      RDI_VARIABLE_NAME(16) = "Plan Area"
      RDI_VARIABLE_NAME(17) = "Holding Company"
      RDI_VARIABLE_NAME(18) = "Member"
      RDI_VARIABLE_NAME(19) = "Census Region"
      RDI_VARIABLE_NAME(20) = "Stock Exch"
      RDI_VARIABLE_NAME(21) = "Stock Symbol"
      RDI_VARIABLE_NAME(22) = "Source Form"
      RDI_VARIABLE_NAME(23) = "Fisc Year"
      RDI_VARIABLE_NAME(24) = "Peak Season"
      RDI_VARIABLE_NAME(25) = "Plant Name"
      RDI_VARIABLE_NAME(26) = "Plant ID"
      RDI_VARIABLE_NAME(27) = "Plant Address 1"
      RDI_VARIABLE_NAME(28) = "Plant Address 2"
      RDI_VARIABLE_NAME(29) = "Plant City"
      RDI_VARIABLE_NAME(30) = "Plant County"
      RDI_VARIABLE_NAME(31) = "Pl State"
      RDI_VARIABLE_NAME(32) = "Plant Zip Code"
      RDI_VARIABLE_NAME(33) = "Plant NERC"
      RDI_VARIABLE_NAME(34) = "Plant Sub-Region"
      RDI_VARIABLE_NAME(35) = "PM Abbrev"
      RDI_VARIABLE_NAME(36) = "Prime Mover"
      RDI_VARIABLE_NAME(37) = "PM Group"
      RDI_VARIABLE_NAME(38) = "Prime Mover Short Desc."
      RDI_VARIABLE_NAME(39) = "Prime Mover Description"
      RDI_VARIABLE_NAME(40) = "Prime Fuel"
      RDI_VARIABLE_NAME(41) = "Year"
      RDI_VARIABLE_NAME(42) = "Mth"
      RDI_VARIABLE_NAME(43) = "Plant Flag"
      RDI_VARIABLE_NAME(44) = "% Owned"
      RDI_VARIABLE_NAME(45) = "Demonstrated Capacity MW"
      RDI_VARIABLE_NAME(46) = "Net Gener MWh"
      RDI_VARIABLE_NAME(47) = "Capacity Factor %"
      RDI_VARIABLE_NAME(48) = "Fuel Cost $"
      RDI_VARIABLE_NAME(49) = "Fuel Adjustments $"
      RDI_VARIABLE_NAME(40) = "Total Fuel Cost $"
      RDI_VARIABLE_NAME(51) = "Incremental Fuel $/MWh"
      RDI_VARIABLE_NAME(52) = "Fuel $/MWh"
      RDI_VARIABLE_NAME(53) = "Var Non-Fuel OM $"
      RDI_VARIABLE_NAME(54) = "Fixed Non-Fuel OM $"
      RDI_VARIABLE_NAME(55) = "Total Prod Costs $"
      RDI_VARIABLE_NAME(56) = "Variable OM $/MWh"
      RDI_VARIABLE_NAME(57) = "Total Var Prod $/MWh"
      RDI_VARIABLE_NAME(58) = "Fixed OM $/kW Mth"
      RDI_VARIABLE_NAME(59) = "Total Prod $/MWh"
      RDI_VARIABLE_NAME(60) = "Average Heat Rate"
      RDI_VARIABLE_NAME(61) = "Date"
!
!
!  CONVERT THE RDI FILE

      ENTRY RDI_MAKEBIN

!
      BASE_FILE_NAME = RDI_POWERDAT_FILE()
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME = trim(BASE_FILE_DIRECTORY())// &
                                   "RDB"//trim(BASE_FILE_NAME)//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      RDI_FILE_EXISTS = FILE_EXISTS
         IREC = 1
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
            CALL MG_LOCATE_WRITE(16,30,BASE_FILE_NAME,ALL_VERSIONS,0)
         ENDIF
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCRDICL.BIN",ACCESS="DIRECT", &
                                            STATUS="UNKNOWN",RECL=LRECL)
!
! REFORMATTING PROGRAM BUYS ME: MIDAS DATA EDITOR, STANDARD FORMATTING ON READ
!
         READ(10,*,IOSTAT=IOS) DELETE
         IF(IOS /= 0) THEN
            CALL REFORMAT_POWERDAT_DATA(FILE_NAME)
            READ(10,*,IOSTAT=IOS) DELETE
            IF(IOS /= 0) THEN
               WRITE(4,*) "Unsuccessful conversion of Powerdat"
               WRITE(4,*) '*** line 247 RDI_OBJT.FOR ***'
               er_message='See WARNING MESSAGES -RDI_OBJT.FOR-1'
               call end_program(er_message)
            ENDIF
         ENDIF
!

         NUM_OF_GEN_UNITS = 0
         RDI_CL_RECORDS = 0
         RDI_EL_RECORDS = 0

         DO
            NUM_OF_GEN_UNITS = NUM_OF_GEN_UNITS + 1
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(IOS /= 0) EXIT
            RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
            READ(RECLN,*,ERR=200) DELETE,COMPANY_NAME,COMP_ID, &
                  COMP_ABBREV,ENTITY_TYPE,COMPANY_TYPE,ADDRESS_1, &
                  ADDRESS_2,CITY,STATE,ZIP_CODE,PHONE_NUM,COUNTRY, &
                  NERC_REGION,SUB_REGION,CONTROL_AREA,PLAN_AREA, &
                  HOLDING_COMPANY,MEMBER,CENSUS_REGION,STOCK_EXCH, &
                  STOCK_SYMBOL,SOURCE_FORM,FISC_YEAR,PEAK_SEASON, &
                  PLANT_NAME,PLANT_ID,PLANT_ADDRESS_1,PLANT_ADDRESS_2, &
                  PLANT_CITY,PLANT_COUNTY,PL_STATE,PLANT_ZIP_CODE, &
                  PLANT_NERC,PLANT_SUB_REGION,PM_ABBREV,PRIME_MOVER, &
                  PM_GROUP,PRIME_MOVER_SHORT_DESC, &
                  PRIME_MOVER_DESCRIPTION,PRIME_FUEL,YEAR, & !  NUMBERS ! INT
                  MTH, & !  INT
                  PLANT_FLAG, & !  CHAR
                  PERCENT_OWNED,DEMONSTRATED_CAPACITY_MW,NET_GENER_MWH, &
                  CAPACITY_FACTOR_PERCENT,FUEL_COST_USD, &
                  FUEL_ADJUSTMENTS_USD,TOTAL_FUEL_COST_USD, &
                  INCREMENTAL_FUEL_USDMWH,FUEL_USDMWH, &
                  VAR_NON_FUEL_OM_USD,FIXED_NON_FUEL_OM_USD, &
                  TOTAL_PROD_COSTS_USD,VARIABLE_OM_USDMWH, &
                  TOTAL_VAR_PROD_USDMWH,FIXED_OM_USDKW_MTH, &
                  TOTAL_PROD_USDMWH,AVERAGE_HEAT_RATE,DATE, & !  INT
                  MONTH_GENERATION, & !  12 VALUES
                  MONTH_CAPACITY ! 12 VALUES
            IF(trim(PM_GROUP) == 'HY') THEN
               RDI_EL_RECORDS = RDI_EL_RECORDS + 1
            ELSE
               RDI_CL_RECORDS = RDI_CL_RECORDS + 1
            ENDIF
            WRITE(11,REC=IREC) DELETE, &
                  COMPANY_NAME, &
                  NERC_REGION, &
                  SUB_REGION, &
                  PLANT_NAME, &
                  PM_ABBREV, &
                  PRIME_FUEL, &
                  YEAR, & !  NUMBERS ! INT
                  MTH, & !  INT
                  PERCENT_OWNED, &
                  DEMONSTRATED_CAPACITY_MW, &
                  INCREMENTAL_FUEL_USDMWH, &
                  FUEL_USDMWH, &
                  VARIABLE_OM_USDMWH, &
                  FIXED_OM_USDKW_MTH, &
                  AVERAGE_HEAT_RATE, &
                  MONTH_GENERATION, & !  12 VALUES
                  MONTH_CAPACITY ! 12 VALUES
            IREC = IREC + 1
         ENDDO ! TABLE
         CLOSE(10)
         CLOSE(11)
      ELSE IF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "rdi_objt:1112")
      ENDIF
      RDI_RECORDS = MAX(0,IREC - 1)
      RETURN
!
!
!
!  OVERLAY THE DAY TYPE FILE

      ENTRY RDI_MAKEOVL(OVERLAY_FAMILY_NAME)

!
      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME = get_rdo_filename(data_drive, overlay_family_name)
      OPEN(10,FILE=FILE_NAME)
      INUNIT = 12
      IF(RDI_OL == 'BC') THEN
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCRDILC.BIN", &
                                             ACCESS="DIRECT",RECL=LRECL)
         INUNIT = 11
      ENDIF
      OPEN(12,FILE=trim(DATA_DRIVE)//"OLRDICL.BIN",ACCESS="DIRECT", &
                                            STATUS="UNKNOWN",RECL=LRECL)
      IREC = 0
      READ(10,*) DELETE
      IF(DELETE /= 9) THEN
         CALL REFORMAT_POWERDAT_DATA(FILE_NAME)
      ENDIF
      DO
         READ(10,1000,IOSTAT=IOS) RECLN
         IF(IOS /= 0) EXIT
         IREC = IREC + 1
         READ(INUNIT,REC=IREC,IOSTAT=IOS) &
                  DELETE, &
                  COMPANY_NAME, &
                  NERC_REGION, &
                  SUB_REGION, &
                  PLANT_NAME, &
                  PM_ABBREV, &
                  PRIME_FUEL, &
                  YEAR, & !  NUMBERS ! INT
                  MTH, & !  INT
                  PERCENT_OWNED, &
                  DEMONSTRATED_CAPACITY_MW, &
                  INCREMENTAL_FUEL_USDMWH, &
                  FUEL_USDMWH, &
                  VARIABLE_OM_USDMWH, &
                  FIXED_OM_USDKW_MTH, &
                  AVERAGE_HEAT_RATE
         IF(IOS == 0) THEN
            RECLN = trim(RECLN)//',,,,,,,,,,,,,'
            READ(RECLN,*,ERR=300) DELETE, &
                  COMPANY_NAME, &
                  COMP_ID, &
                  COMP_ABBREV, &
                  ENTITY_TYPE, &
                  COMPANY_TYPE, &
                  ADDRESS_1, &
                  ADDRESS_2, &
                  CITY, &
                  STATE, &
                  ZIP_CODE, &
                  PHONE_NUM, &
                  COUNTRY, &
                  NERC_REGION, &
                  SUB_REGION, &
                  CONTROL_AREA, &
                  PLAN_AREA, &
                  HOLDING_COMPANY, &
                  MEMBER, &
                  CENSUS_REGION, &
                  STOCK_EXCH, &
                  STOCK_SYMBOL, &
                  SOURCE_FORM, &
                  FISC_YEAR, &
                  PEAK_SEASON, &
                  PLANT_NAME, &
                  PLANT_ID, &
                  PLANT_ADDRESS_1, &
                  PLANT_ADDRESS_2, &
                  PLANT_CITY, &
                  PLANT_COUNTY, &
                  PL_STATE, &
                  PLANT_ZIP_CODE, &
                  PLANT_NERC, &
                  PLANT_SUB_REGION, &
                  PM_ABBREV, &
                  PRIME_MOVER, &
                  PM_GROUP, &
                  PRIME_MOVER_SHORT_DESC, &
                  PRIME_MOVER_DESCRIPTION, &
                  PRIME_FUEL, &
                  YEAR, & !  NUMBERS ! INT
                  MTH, & !  INT
                  PLANT_FLAG, & !  CHAR
                  PERCENT_OWNED, &
                  DEMONSTRATED_CAPACITY_MW, &
                  NET_GENER_MWH, &
                  CAPACITY_FACTOR_PERCENT, &
                  FUEL_COST_USD, &
                  FUEL_ADJUSTMENTS_USD, &
                  TOTAL_FUEL_COST_USD, &
                  INCREMENTAL_FUEL_USDMWH, &
                  FUEL_USDMWH, &
                  VAR_NON_FUEL_OM_USD, &
                  FIXED_NON_FUEL_OM_USD, &
                  TOTAL_PROD_COSTS_USD, &
                  VARIABLE_OM_USDMWH, &
                  TOTAL_VAR_PROD_USDMWH, &
                  FIXED_OM_USDKW_MTH, &
                  TOTAL_PROD_USDMWH, &
                  AVERAGE_HEAT_RATE, &
                  DATE ! INT
         ENDIF
         WRITE(11,REC=IREC) DELETE, &
                  COMPANY_NAME, &
                  NERC_REGION, &
                  SUB_REGION, &
                  PLANT_NAME, &
                  PM_ABBREV, &
                  PRIME_FUEL, &
                  YEAR, & !  NUMBERS ! INT
                  MTH, & !  INT
                  PERCENT_OWNED, &
                  DEMONSTRATED_CAPACITY_MW, &
                  INCREMENTAL_FUEL_USDMWH, &
                  FUEL_USDMWH, &
                  VARIABLE_OM_USDMWH, &
                  FIXED_OM_USDKW_MTH, &
                  AVERAGE_HEAT_RATE
      ENDDO
      CLOSE(10)
      CLOSE(12)
      IF(RDI_OL == 'BC') CLOSE(11)
      RDI_OL = 'OL'
      RETURN

  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from RDI_OBJT SIID258'
      call end_program(er_message)

  300 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      CALL MG_LOCATE_WRITE(21,0, &
                           'Error reading the RDI record. Look for'// &
                                          ' a "," in a character name.', &
                           ALL_VERSIONS,1)
      er_message='stop requested from RDI_OBJT SIID259'
      call end_program(er_message)
!

      ENTRY RESET_RDI_POWERDAT_OL

         RDI_OL = 'BC'
      RETURN
!

      ENTRY OPEN_RDI_CL_FILES(R_RDI_IN_UNIT,R_RDI_OUT_UNIT)

         OPEN(UNIT_IN_NUM,FILE=trim(OUTPUT_DIRECTORY())//RDI_OL// &
                "RDICL.BIN",ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         OPEN(UNIT_IN_NUM)
         R_RDI_IN_UNIT = UNIT_IN_NUM
!
         OPEN(UNIT_OUT_NUM,FILE=trim(OUTPUT_DIRECTORY())// &
                             "CLB"//trim(RDI_POWERDAT_FILE())//".DAT")
         R_RDI_OUT_UNIT = UNIT_OUT_NUM
      RETURN
!

      ENTRY OPEN_RDI_EL_FILES(R_RDI_IN_UNIT,R_RDI_OUT_UNIT)

         OPEN(UNIT_IN_NUM,FILE=trim(OUTPUT_DIRECTORY())//RDI_OL// &
                "RDICL.BIN",ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         OPEN(UNIT_IN_NUM)
         R_RDI_IN_UNIT = UNIT_IN_NUM
!
         OPEN(UNIT_OUT_NUM,FILE=trim(OUTPUT_DIRECTORY())// &
                             "ELB"//trim(RDI_POWERDAT_FILE())//".DAT")
         R_RDI_OUT_UNIT = UNIT_OUT_NUM
      RETURN
!

      ENTRY OPEN_RDI_FILE(R_RDI_IN_UNIT)

         OPEN(UNIT_IN_NUM,FILE=trim(OUTPUT_DIRECTORY())//RDI_OL// &
                "RDICL.BIN",ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         OPEN(UNIT_IN_NUM)
         R_RDI_IN_UNIT = UNIT_IN_NUM
      RETURN
!

      ENTRY CLOSE_RDI_FILE

         CLOSE(UNIT_IN_NUM)
      RETURN

      ENTRY CLOSE_BOTH_RDI_FILES

         CLOSE(UNIT_IN_NUM)
         CLOSE(UNIT_OUT_NUM)
      RETURN

      ENTRY DOES_RDI_FILE_EXIST(R_RDI_FILE_EXISTS)

         R_RDI_FILE_EXISTS = RDI_FILE_EXISTS
      RETURN

      ENTRY GET_NUM_OF_GEN_UNITS(R_NUMBER_OF_UNITS)

         R_NUMBER_OF_UNITS = NUM_OF_GEN_UNITS
      RETURN

      ENTRY GET_NUM_OF_RDI_RECORDS(R_NUMBER_OF_UNITS)

         R_NUMBER_OF_UNITS = RDI_RECORDS
      RETURN

      ENTRY GET_NUM_OF_RDI_CL_RECORDS(R_NUMBER_OF_UNITS, &
                                                    R_NUMBER_LOCAL_TYPE)

         R_NUMBER_OF_UNITS = RDI_RECORDS
         R_NUMBER_LOCAL_TYPE = RDI_CL_RECORDS
      RETURN

      ENTRY GET_NUM_OF_RDI_EL_RECORDS(R_NUMBER_OF_UNITS, &
                                                    R_NUMBER_LOCAL_TYPE)

         R_NUMBER_OF_UNITS = RDI_RECORDS
         R_NUMBER_LOCAL_TYPE = RDI_EL_RECORDS
      RETURN
!
!

!
      ENTRY REFORMAT_POWERDAT_DATA(R_FILE_NAME)
!

!
!
         TEMP_FILE_NAME = "RDI"//trim(RDI_POWERDAT_FILE())//".DAT"
         CALL COPY(R_FILE_NAME,TEMP_FILE_NAME) ! MAKE A BACKUP
!
         ALLOCATE(MONTHS_PER_PLANT(MAX_RDI_PLANTS), &
                                       CHECK_PLANT_NAME(MAX_RDI_PLANTS))
!
           MONTHS_PER_PLANT = 0
         IREC = 0
         PLANT = 0
! TO GET THE FIRST RECORD
         CLOSE(10)
         OPEN(10,FILE=R_FILE_NAME)
!
         OHIO_EDISON = .TRUE.
!
         DO
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(IOS /= 0) EXIT
            IF( .NOT. OHIO_EDISON) THEN
               READ(RECLN,*,ERR=200) &
                  COMPANY_NAME, &
                  COMP_ID, &
                  COMP_ABBREV, &
                  ENTITY_TYPE, &
                  COMPANY_TYPE, &
                  ADDRESS_1, &
                  ADDRESS_2, &
                  CITY, &
                  STATE, &
                  ZIP_CODE, &
                  PHONE_NUM, &
                  COUNTRY, &
                  NERC_REGION, &
                  SUB_REGION, &
                  CONTROL_AREA, &
                  PLAN_AREA, &
                  HOLDING_COMPANY, &
                  MEMBER, &
                  CENSUS_REGION, &
                  STOCK_EXCH, &
                  STOCK_SYMBOL, &
                  SOURCE_FORM, &
                  FISC_YEAR, &
                  PEAK_SEASON, &
                  PLANT_NAME, &
                  PLANT_ID, &
                  PLANT_ADDRESS_1, &
                  PLANT_ADDRESS_2, &
                  PLANT_CITY, &
                  PLANT_COUNTY, &
                  PL_STATE, &
                  PLANT_ZIP_CODE, &
                  PLANT_NERC, &
                  PLANT_SUB_REGION, &
                  PM_ABBREV, &
                  PRIME_MOVER, &
                  PM_GROUP, &
                  PRIME_MOVER_SHORT_DESC, &
                  PRIME_MOVER_DESCRIPTION, &
                  PRIME_FUEL, &
                  YEAR, & !  NUMBERS ! INT
                  MTH, & !  INT
                  PLANT_FLAG, & !  CHAR
                  PERCENT_OWNED, &
                  DEMONSTRATED_CAPACITY_MW, &
                  NET_GENER_MWH, &
                  CAPACITY_FACTOR_PERCENT, &
                  FUEL_COST_USD, &
                  FUEL_ADJUSTMENTS_USD, &
                  TOTAL_FUEL_COST_USD, &
                  INCREMENTAL_FUEL_USDMWH, &
                  FUEL_USDMWH, &
                  VAR_NON_FUEL_OM_USD, &
                  FIXED_NON_FUEL_OM_USD, &
                  TOTAL_PROD_COSTS_USD, &
                  VARIABLE_OM_USDMWH, &
                  TOTAL_VAR_PROD_USDMWH, &
                  FIXED_OM_USDKW_MTH, &
                  TOTAL_PROD_USDMWH, &
                  AVERAGE_HEAT_RATE, &
                  DATE ! INT
            ELSE
               READ(RECLN,*,ERR=200) &
                  COMPANY_NAME, &
                  COMP_ABBREV, &
                  ENTITY_TYPE, &
                  COMPANY_TYPE, &
                  CITY, &
                  STATE, &
                  NERC_REGION, &
                  SUB_REGION, &
                  CONTROL_AREA, &
                  PLAN_AREA, &
                  HOLDING_COMPANY, &
                  MEMBER, &
                  STOCK_EXCH, &
                  STOCK_SYMBOL, &
                  SOURCE_FORM, &
                  FISC_YEAR, &
                  PEAK_SEASON, &
                  PLANT_NAME, &
                  PLANT_ID, &
                  PLANT_CITY, &
                  PLANT_COUNTY, &
                  PL_STATE, &
                  PLANT_ZIP_CODE, &
                  PLANT_NERC, &
                  PLANT_SUB_REGION, &
                  PM_ABBREV, &
                  PRIME_MOVER, &
                  PM_GROUP, &
                  PRIME_MOVER_SHORT_DESC, &
                  PRIME_MOVER_DESCRIPTION, &
                  PRIME_FUEL, &
                  YEAR, & !  NUMBERS ! INT
                  MTH, & !  INT
                  PLANT_FLAG, & !  CHAR
                  PERCENT_OWNED, &
                  DEMONSTRATED_CAPACITY_MW, &
                  NET_GENER_MWH, &
                  CAPACITY_FACTOR_PERCENT, &
                  FUEL_COST_USD, &
                  FUEL_ADJUSTMENTS_USD, &
                  TOTAL_FUEL_COST_USD, &
                  INCREMENTAL_FUEL_USDMWH, &
                  FUEL_USDMWH, &
                  VAR_NON_FUEL_OM_USD, &
                  FIXED_NON_FUEL_OM_USD, &
                  TOTAL_PROD_COSTS_USD, &
                  VARIABLE_OM_USDMWH, &
                  TOTAL_VAR_PROD_USDMWH, &
                  FIXED_OM_USDKW_MTH, &
                  TOTAL_PROD_USDMWH, &
                  AVERAGE_HEAT_RATE
!     +            DATE ! INT
            ENDIF
!
            IREC = IREC + 1
!
            IF(IREC == 1 .OR. LAST_PLANT_NAME /= PLANT_NAME .OR. &
                                   LAST_MEMBER_NAME /= MEMBER) THEN
               PLANT = PLANT + 1
               LAST_PLANT_NAME = PLANT_NAME
               LAST_MEMBER_NAME = MEMBER
               LAST_PRIME_MOVER = PRIME_MOVER
               DO COUNT = 1, 12
                  MONTHS_PER_UNIT(COUNT) = 0
                  MONTH_GENERATION(COUNT) = 0
                  MONTH_CAPACITY(COUNT) = 0
               ENDDO
            ELSEIF(LAST_PRIME_MOVER /= PRIME_MOVER) THEN
               PLANT = PLANT + 1
               LAST_PLANT_NAME = PLANT_NAME
               LAST_MEMBER_NAME = MEMBER
               LAST_PRIME_MOVER = PRIME_MOVER
            ENDIF
!
            CHECK_PLANT_NAME(PLANT) = PLANT_NAME
!
            MONTHS_PER_PLANT(PLANT) = MONTHS_PER_PLANT(PLANT) + 1
!
         ENDDO
!
         CLOSE(10)
         OPEN(10,FILE=R_FILE_NAME)
         TEMP_FILE_NAME = "TEMP_RDI.DAT"
         OPEN(1951,FILE=TEMP_FILE_NAME)
         WRITE(1951,*) "9,"
         DELETE = 1
         LAST_REGION_NAME = '                    '
!
         LOCAL_CAP_MONTHS = 0.
         LOCAL_CAPACITY = 0.
         LOCAL_GEN_MONTHS = 0.
         LOCAL_GENERATION = 0.
         LOCAL_AHR = 0.
         LOCAL_FUEL_COST = 0.
         LOCAL_VAROM_COST =  0.
!
!
         MO = 0
         PLANT = 1
         DO
            MO  = MO + 1
!
            IREC = IREC + 1
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(IOS /= 0) EXIT

            IF( .NOT. OHIO_EDISON) THEN
               READ(RECLN,*,ERR=200) &
                  COMPANY_NAME, &
                  COMP_ID, &
                  COMP_ABBREV, &
                  ENTITY_TYPE, &
                  COMPANY_TYPE, &
                  ADDRESS_1, &
                  ADDRESS_2, &
                  CITY, &
                  STATE, &
                  ZIP_CODE, &
                  PHONE_NUM, &
                  COUNTRY, &
                  NERC_REGION, &
                  SUB_REGION, &
                  CONTROL_AREA, &
                  PLAN_AREA, &
                  HOLDING_COMPANY, &
                  MEMBER, &
                  CENSUS_REGION, &
                  STOCK_EXCH, &
                  STOCK_SYMBOL, &
                  SOURCE_FORM, &
                  FISC_YEAR, &
                  PEAK_SEASON, &
                  PLANT_NAME, &
                  PLANT_ID, &
                  PLANT_ADDRESS_1, &
                  PLANT_ADDRESS_2, &
                  PLANT_CITY, &
                  PLANT_COUNTY, &
                  PL_STATE, &
                  PLANT_ZIP_CODE, &
                  PLANT_NERC, &
                  PLANT_SUB_REGION, &
                  PM_ABBREV, &
                  PRIME_MOVER, &
                  PM_GROUP, &
                  PRIME_MOVER_SHORT_DESC, &
                  PRIME_MOVER_DESCRIPTION, &
                  PRIME_FUEL, &
                  YEAR, & !  NUMBERS ! INT
                  MTH, & !  INT
                  PLANT_FLAG, & !  CHAR
                  PERCENT_OWNED, &
                  DEMONSTRATED_CAPACITY_MW, &
                  NET_GENER_MWH, &
                  CAPACITY_FACTOR_PERCENT, &
                  FUEL_COST_USD, &
                  FUEL_ADJUSTMENTS_USD, &
                  TOTAL_FUEL_COST_USD, &
                  INCREMENTAL_FUEL_USDMWH, &
                  FUEL_USDMWH, &
                  VAR_NON_FUEL_OM_USD, &
                  FIXED_NON_FUEL_OM_USD, &
                  TOTAL_PROD_COSTS_USD, &
                  VARIABLE_OM_USDMWH, &
                  TOTAL_VAR_PROD_USDMWH, &
                  FIXED_OM_USDKW_MTH, &
                  TOTAL_PROD_USDMWH, &
                  AVERAGE_HEAT_RATE, &
                  DATE ! INT
            ELSE
               READ(RECLN,*,ERR=200) &
                  COMPANY_NAME, &

                  COMP_ABBREV, &
                  ENTITY_TYPE, &
                  COMPANY_TYPE, &

                  CITY, &
                  STATE, &

                  NERC_REGION, &
                  SUB_REGION, &
                  CONTROL_AREA, &
                  PLAN_AREA, &
                  HOLDING_COMPANY, &
                  MEMBER, &

                  STOCK_EXCH, &
                  STOCK_SYMBOL, &
                  SOURCE_FORM, &
                  FISC_YEAR, &
                  PEAK_SEASON, &
                  PLANT_NAME, &
                  PLANT_ID, &

                  PLANT_CITY, &
                  PLANT_COUNTY, &
                  PL_STATE, &
                  PLANT_ZIP_CODE, &
                  PLANT_NERC, &
                  PLANT_SUB_REGION, &
                  PM_ABBREV, &
                  PRIME_MOVER, &
                  PM_GROUP, &
                  PRIME_MOVER_SHORT_DESC, &
                  PRIME_MOVER_DESCRIPTION, &
                  PRIME_FUEL, &
                  YEAR, & !  NUMBERS ! INT
                  MTH, & !  INT
                  PLANT_FLAG, & !  CHAR
                  PERCENT_OWNED, &
                  DEMONSTRATED_CAPACITY_MW, &
                  NET_GENER_MWH, &
                  CAPACITY_FACTOR_PERCENT, &
                  FUEL_COST_USD, &
                  FUEL_ADJUSTMENTS_USD, &
                  TOTAL_FUEL_COST_USD, &
                  INCREMENTAL_FUEL_USDMWH, &
                  FUEL_USDMWH, &
                  VAR_NON_FUEL_OM_USD, &
                  FIXED_NON_FUEL_OM_USD, &
                  TOTAL_PROD_COSTS_USD, &
                  VARIABLE_OM_USDMWH, &
                  TOTAL_VAR_PROD_USDMWH, &
                  FIXED_OM_USDKW_MTH, &
                  TOTAL_PROD_USDMWH, &
                  AVERAGE_HEAT_RATE

            ENDIF
!
            MONTHS_PER_UNIT(MTH) = MONTHS_PER_UNIT(MTH) + 1
!
            IF(MTH < 1 .OR. MTH > 12) THEN
               WRITE(4,*) "UNEXPECTED MONTH ENCOUNTERED IN RDI DATA"
               WRITE(4,*) "FOR PLANT ",PLANT_NAME
               WRITE(4,*) "AND MONTH ",MTH
               WRITE(4,*) '*** line 931 RDI_OBJT.FOR ***'
               er_message='See WARNING MESSAGES -RDI_OBJT.FOR-2'
               call end_program(er_message)
            ENDIF
!
            IF( MO > 1 .AND. PLANT_NAME /= LAST_PLANT_NAME) THEN

               WRITE(4,*) "INCONSISTENT MONTHS IN RDI DATA"
               WRITE(4,*) "EXPECTED PLANT ", PLANT_NAME
               WRITE(4,*) "UNEXPECTED PLANT ", LAST_PLANT_NAME
               WRITE(4,*) '*** line 939I_OBJT.FOR ***'
               er_message='See WARNING MESSAGES -RDI_OBJT.FOR-3'
               call end_program(er_message)
            ELSE
!
! ACCUMULATE
!
               IF(DEMONSTRATED_CAPACITY_MW > 0.0) THEN
                  LOCAL_CAP_MONTHS = LOCAL_CAP_MONTHS + 1
                  LOCAL_CAPACITY = LOCAL_CAPACITY + &
                                                DEMONSTRATED_CAPACITY_MW
                  MONTH_CAPACITY(MTH) = MONTH_CAPACITY(MTH) + &
                                                DEMONSTRATED_CAPACITY_MW
               ENDIF
!
               IF(NET_GENER_MWH > 0.0) THEN
!
                  LOCAL_GEN_MONTHS = LOCAL_GEN_MONTHS + 1
                  LOCAL_GENERATION = LOCAL_GENERATION + NET_GENER_MWH
                  MONTH_GENERATION(MTH) = MONTH_GENERATION(MTH) + &
                                                           NET_GENER_MWH
                  LOCAL_AHR = LOCAL_AHR + &
                                       NET_GENER_MWH * AVERAGE_HEAT_RATE
                  LOCAL_FUEL_COST = LOCAL_FUEL_COST + FUEL_COST_USD
                  LOCAL_VAROM_COST = LOCAL_VAROM_COST + &
                                                   VAR_NON_FUEL_OM_USD
               ENDIF
!
!
! CALCULATE ANNUAL VALUES FOR THE LAST RECORD, WRITE IT, REINITIALIZE FOR NEXT RECORD
!
!
               IF(MO == MONTHS_PER_PLANT(PLANT)) THEN
                  DO COUNT = 1, 12
                     IF(MONTHS_PER_UNIT(COUNT) > 0) THEN
                        MONTH_GENERATION(COUNT) = &
                                     MONTH_GENERATION(COUNT) / &
                                                  MONTHS_PER_UNIT(COUNT)
                        MONTH_CAPACITY(COUNT) = MONTH_CAPACITY(COUNT) / &
                                                  MONTHS_PER_UNIT(COUNT)
                     ENDIF
                  ENDDO
                  IF(LOCAL_GENERATION > 0.0) THEN
                     MONTHS_TO_YEARS = 12. / LOCAL_GEN_MONTHS
!
                     ANNUAL_GENERATION = LOCAL_GENERATION * &
                                                         MONTHS_TO_YEARS
                     IF(LOCAL_AHR > 0.) THEN
                        ANNUAL_AHR = MIN(30000., &
                                           LOCAL_AHR / LOCAL_GENERATION)
                        ANNUAL_FUEL_USDMMBTU = &
                                       1000. * LOCAL_FUEL_COST/LOCAL_AHR
                     ELSEIF(LOCAL_FUEL_COST > 0.) THEN
                        ANNUAL_AHR = 10000.
                        LOCAL_AHR = ANNUAL_AHR * LOCAL_GENERATION
                        ANNUAL_FUEL_USDMMBTU = &
                                       1000. * LOCAL_FUEL_COST/LOCAL_AHR
                     ELSE
                        ANNUAL_AHR = 0.0
                        ANNUAL_FUEL_USDMMBTU = 0.
                     ENDIF
                     ANNUAL_VAROM_USDMWH = LOCAL_VAROM_COST / &
                                                        LOCAL_GENERATION
!
                     ANNUAL_FUEL_COST = .001 * LOCAL_FUEL_COST * &
                                                         MONTHS_TO_YEARS
                     ANNUAL_VAROM_COST = .001 * LOCAL_FUEL_COST * &
                                                         MONTHS_TO_YEARS
!
                  ELSE
                     ANNUAL_GENERATION = 0.0
                     ANNUAL_AHR = 0.0
                     ANNUAL_FUEL_USDMMBTU = 0.0
                     ANNUAL_VAROM_USDMWH = 0.0
                     ANNUAL_FUEL_COST = 0.0
                     ANNUAL_VAROM_COST = 0.0
                  ENDIF
                  IF(LOCAL_CAPACITY > 0.0) THEN
                     MONTHS_TO_YEARS = 12. / LOCAL_CAP_MONTHS
                     ANNUAL_CAPACITY = LOCAL_CAPACITY / LOCAL_CAP_MONTHS
                     IF(ANNUAL_GENERATION > 0.0) THEN
                        ANNUAL_CAP_FACTOR = ANNUAL_GENERATION / &
                                                (ANNUAL_CAPACITY * 87.6)
                     ELSE
                        ANNUAL_CAP_FACTOR = 0.0
                     ENDIF
                  ELSE
                     ANNUAL_CAPACITY = 0.
                  ENDIF
!
                  IF(.NOT. OHIO_EDISON) THEN
!
                     WRITE(1951,1020) &
                        trim(COMPANY_NAME), &
                        trim(COMP_ID), &
                        trim(COMP_ABBREV), &
                        trim(ENTITY_TYPE), &
                        trim(COMPANY_TYPE), &
                        trim(ADDRESS_1), &
                        trim(ADDRESS_2), &
                        trim(CITY), &
                        trim(STATE), &
                        trim(ZIP_CODE), &
                        trim(PHONE_NUM), &
                        trim(COUNTRY), &
                        trim(NERC_REGION), &
                        trim(SUB_REGION), &
                        trim(CONTROL_AREA), &
                        trim(PLAN_AREA), &
                        trim(HOLDING_COMPANY), &
                        trim(MEMBER), &
                        trim(CENSUS_REGION), &
                        trim(STOCK_EXCH), &
                        trim(STOCK_SYMBOL), &
                        trim(SOURCE_FORM), &
                        trim(FISC_YEAR), &
                        trim(PEAK_SEASON), &
                        trim(PLANT_NAME), &
                        trim(PLANT_ID), &
                        trim(PLANT_ADDRESS_1), &
                        trim(PLANT_ADDRESS_2), &
                        trim(PLANT_CITY), &
                        trim(PLANT_COUNTY), &
                        trim(PL_STATE), &
                        trim(PLANT_ZIP_CODE), &
                        trim(PLANT_NERC), &
                        trim(PLANT_SUB_REGION), &
                        trim(PM_ABBREV), &
                        trim(PRIME_MOVER), &
                        trim(PM_GROUP), &
                        trim(PRIME_MOVER_SHORT_DESC), &
                        trim(PRIME_MOVER_DESCRIPTION), &
                        trim(PRIME_FUEL), & !  40
                        YEAR, & !  NUMBERS ! INT
                        MTH, & !  MONTH INT
                        trim(PLANT_FLAG), & !  CHAR
                        PERCENT_OWNED, &
                        ANNUAL_CAPACITY, & !  HERE AND BELOW MAY NEED TO BE ADJUST FOR ANNUALS
                        ANNUAL_GENERATION, &
                        ANNUAL_CAP_FACTOR, &
                        ANNUAL_FUEL_COST, &
                        FUEL_ADJUSTMENTS_USD, &
                        TOTAL_FUEL_COST_USD, &
                        INCREMENTAL_FUEL_USDMWH, &
                        ANNUAL_FUEL_USDMMBTU, &
                        ANNUAL_VAROM_COST, &
                        FIXED_NON_FUEL_OM_USD, &
                        TOTAL_PROD_COSTS_USD, &
                        ANNUAL_VAROM_USDMWH, &
                        TOTAL_VAR_PROD_USDMWH, &
                        FIXED_OM_USDKW_MTH, &
                        TOTAL_PROD_USDMWH, &
                        ANNUAL_AHR, &
                        DATE, & !  INT
                        MONTH_GENERATION, & !  12 VALUES
                        MONTH_CAPACITY ! 12 VALUES
                  ELSE
                     WRITE(1951,1020) &
                        trim(COMPANY_NAME), &
                        trim(NULL_C20), &
                        trim(COMP_ABBREV), &
                        trim(ENTITY_TYPE), &
                        trim(COMPANY_TYPE), &
                        trim(NULL_C20), &
                        trim(NULL_C20), &
                        trim(CITY), &
                        trim(STATE), &
                        trim(NULL_C20), &
                        trim(NULL_C20), &
                        trim(NULL_C20), &
                        trim(NERC_REGION), &
                        trim(SUB_REGION), &
                        trim(CONTROL_AREA), &
                        trim(PLAN_AREA), &
                        trim(HOLDING_COMPANY), &
                        trim(MEMBER), &
                        trim(NULL_C20), &
                        trim(STOCK_EXCH), &
                        trim(STOCK_SYMBOL), &
                        trim(SOURCE_FORM), &
                        trim(FISC_YEAR), &
                        trim(PEAK_SEASON), &
                        trim(PLANT_NAME), &
                        trim(PLANT_ID), &
                        trim(NULL_C20), &
                        trim(NULL_C20), &
                        trim(PLANT_CITY), &
                        trim(PLANT_COUNTY), &
                        trim(PL_STATE), &
                        trim(PLANT_ZIP_CODE), &
                        trim(PLANT_NERC), &
                        trim(PLANT_SUB_REGION), &
                        trim(PM_ABBREV), &
                        trim(PRIME_MOVER), &
                        trim(PM_GROUP), &
                        trim(PRIME_MOVER_SHORT_DESC), &
                        trim(PRIME_MOVER_DESCRIPTION), &
                        trim(PRIME_FUEL), & !  40
                        YEAR, & !  NUMBERS ! INT
                        MTH, & !  MONTH INT
                        trim(PLANT_FLAG), & !  CHAR
                        PERCENT_OWNED, &
                        ANNUAL_CAPACITY, & !  HERE AND BELOW MAY NEED TO BE ADJUST FOR ANNUALS
                        ANNUAL_GENERATION, &
                        ANNUAL_CAP_FACTOR, &
                        ANNUAL_FUEL_COST, &
                        FUEL_ADJUSTMENTS_USD, &
                        TOTAL_FUEL_COST_USD, &
                        INCREMENTAL_FUEL_USDMWH, &
                        ANNUAL_FUEL_USDMMBTU, &
                        ANNUAL_VAROM_COST, &
                        FIXED_NON_FUEL_OM_USD, &
                        TOTAL_PROD_COSTS_USD, &
                        ANNUAL_VAROM_USDMWH, &
                        TOTAL_VAR_PROD_USDMWH, &
                        FIXED_OM_USDKW_MTH, &
                        TOTAL_PROD_USDMWH, &
                        ANNUAL_AHR, &
                        199701., & !  INT
                        MONTH_GENERATION, & !  12 VALUES
                        MONTH_CAPACITY ! 12 VALUES
                  ENDIF
!
                  LOCAL_CAP_MONTHS = 0.
                  LOCAL_CAPACITY = 0.
                  LOCAL_GEN_MONTHS = 0.
                  LOCAL_GENERATION = 0.
                  LOCAL_AHR = 0.
                  LOCAL_FUEL_COST = 0.
                  LOCAL_VAROM_COST =  0.
                  MO = 0
                  PLANT = PLANT + 1
                  DO COUNT = 1, 12
                     MONTHS_PER_UNIT(COUNT) = 0
                     MONTH_GENERATION(COUNT) = 0
                     MONTH_CAPACITY(COUNT) = 0
                  ENDDO
               ENDIF ! LAST MONTH FOR THE RECORD
            ENDIF !VALID RECORD
!
            LAST_PLANT_NAME = PLANT_NAME
!
         ENDDO ! READING RDI RECORDS
!
         CLOSE(10)
         CLOSE(1951)
!
         CALL COPY(TEMP_FILE_NAME,R_FILE_NAME)
!
!
         OPEN(10,FILE=R_FILE_NAME)
!
         IF(ALLOCATED(MONTHS_PER_PLANT)) &
                     DEALLOCATE(MONTHS_PER_PLANT,CHECK_PLANT_NAME)
!
      RETURN
!
 1000 FORMAT(A)
 1010 FORMAT('&',A)
 1020 FORMAT('1',40(',','"',A,'"'), &
                ',',I4,',',I2,',','"',A,'"',18(',',F10.1),24(',',F10.1))
      END
!
!
!
!
!
!

!
!                 ROUTINE TO CONVERT RDI BASE CASE FILE
!
!                      COPYRIGHT (!) 1997
!                 M.S. GERBER & ASSOCIATES, INC.
!                     ALL RIGHTS RESERVED
!

!
      RECURSIVE SUBROUTINE RBC_OBJECT
      use end_routine, only: end_program, er_message
      use filename_tracker
!

!
!!! RECORD LENGTH UPDATED WITH VARIABLE CHANGES
      USE SIZECOM
      USE spindriftlib
      USE prod_arrays_dimensions
      SAVE
      INTEGER(kind=2) :: DELETE,INUNIT,IREC,LRECL=355 ! ASSUMES 61 VARIABLES
      INTEGER IOS
!
      INTEGER(kind=2) :: UNIT_IN_NUM=1754,R_RBC_IN_UNIT, &
                  UNIT_OUT_NUM=1755,R_RBC_OUT_UNIT, &
                  NUMBER_OF_RBC_VARIABLES,MONTHS_PER_PLANT(:),MO, &
                  PLANT,MAX_RBC_PLANTS,RBC_RECORDS=0,RBC_CL_RECORDS=0, &
                  RBC_EL_RECORDS=0,COUNT, &
                  MONTHS_PER_UNIT(12)
      ALLOCATABLE :: MONTHS_PER_PLANT
      PARAMETER (NUMBER_OF_RBC_VARIABLES= 61,MAX_RBC_PLANTS=3000)
      CHARACTER(len=5) :: BASE_FILE_NAME,OVERLAY_FAMILY_NAME, &
                  RDI_BASECASE_FILE
      CHARACTER(len=4) :: RBC_NAME
      CHARACTER(len=20) :: LAST_PLANT_NAME,LAST_REGION_NAME, &
                   LAST_PRIME_MOVER,NULL_C20='NULL DATA'
      CHARACTER(len=256) :: FILE_NAME,R_FILE_NAME,TEMP_FILE_NAME
      CHARACTER(len=256) :: BASE_FILE_DIRECTORY
      CHARACTER(len=256) :: DATA_DRIVE,OUTPUT_DIRECTORY
      LOGICAL(kind=4) :: FILE_EXISTS,RBC_FILE_EXISTS=.FALSE., &
                  R_RBC_FILE_EXISTS
!  DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER(len=1024) :: RECLN
!  DECLARATION FOR DAY TYPE DETERMINANTS
!
! RBC DATA BASE PARAMETERS
!
      INTEGER(kind=2) :: NUM_OF_GEN_UNITS=0,NUMBER_OF_COMPANIES, &
                  R_NUMBER_OF_UNITS,R_NUMBER_LOCAL_TYPE
      CHARACTER(len=25) :: RBC_VARIABLE_NAME(:)
!
! RBC VARIABLES
!
      INTEGER(kind=2) :: &
               YEAR,MTH ! NUMBERS ! INT
!
!
!
      REAL(kind=4) :: &
               PERCENT_OWNED, &
               DEMONSTRATED_CAPACITY_MW, &
               NET_GENER_MWH, &
               CAPACITY_FACTOR_PERCENT, &
               FUEL_COST_USD, &
               FUEL_ADJUSTMENTS_USD, &
               TOTAL_FUEL_COST_USD, &
               INCREMENTAL_FUEL_USDMWH, &
               FUEL_USDMWH, &
               VAR_NON_FUEL_OM_USD, &
               FIXED_NON_FUEL_OM_USD, &
               TOTAL_PROD_COSTS_USD, &
               VARIABLE_OM_USDMWH, &
               TOTAL_VAR_PROD_USDMWH, &
               FIXED_OM_USDKW_MTH, &
               TOTAL_PROD_USDMWH, &
               AVERAGE_HEAT_RATE, &
               DATE ! REAL
      CHARACTER(len=20) :: &
               COMP_ID, &
               COMP_ABBREV, &
               ADDRESS_1, &
               ADDRESS_2, &
               CITY, &
               ZIP_CODE, &
               PHONE_NUM, &
               SUB_REGION, &
               PLAN_AREA, &
               MEMBER, &
               CENSUS_REGION, &
               STOCK_EXCH, &
               STOCK_SYMBOL, &
               SOURCE_FORM, &
               FISC_YEAR, &
               PEAK_SEASON, &
               PLANT_ADDRESS_1, &
               PLANT_ADDRESS_2, &
               PLANT_CITY, &
               PLANT_COUNTY, &
               PLANT_ZIP_CODE, &
               PLANT_NERC, &
               PLANT_SUB_REGION, &
               PM_ABBREV, &
               PM_GROUP, &
               PRIME_FUEL, &
               PLANT_FLAG ! CHAR
!
      ALLOCATABLE :: RBC_VARIABLE_NAME
!
      CHARACTER(len=16) :: FILE_TYPE='RDI Base Case   '
      CHARACTER(len=2) :: RBC_OL='BC'
!
! DATA TO SUPPORT MONTHLY TO ANNUAL CONVERSIONS
!
      REAL(kind=4) :: &
            LOCAL_CAP_MONTHS, &
            LOCAL_CAPACITY, &
            LOCAL_GEN_MONTHS, &
            LOCAL_GENERATION, &
            LOCAL_AHR, &
            LOCAL_FUEL_COST, &
            LOCAL_VAROM_COST, &
            MONTHS_TO_YEARS, &
            ANNUAL_GENERATION, &
            ANNUAL_AHR, &
            ANNUAL_FUEL_USDMMBTU, &
            ANNUAL_VAROM_USDMWH, &
            ANNUAL_FUEL_COST, &
            ANNUAL_VAROM_COST, &
            ANNUAL_CAPACITY, &
            ANNUAL_CAP_FACTOR, &
            MONTH_GENERATION(12), &
            MONTH_CAPACITY(12)
!
!
!
      CHARACTER(len=20) :: &
                           OPERATOR_NAME, & !  CHAR*20
                           OPERATOR_ID*5, & !  CHAR*5
      	                  OPERATOR_ABBREVIATION*10, & !  CHAR*10
      	                  OPERATOR_ENTITY_TYPE*10, & !  CHAR*10
                           OPERATOR_COMPANY_TYPE*10, & !  CHAR*10
                           OPERATOR_NERC_REGION, & !  CHAR*20
                           OPERATOR_SUB_REGION, & !  CHAR*20
                           OPERATOR_CONTROL_AREA, & !  CHAR*20
                           OPERATOR_PLANNING_AREA, & !  CHAR*20
                           OPERATOR_HOLDING_CO, & !  CHAR*20
                           OPERATOR_STATE*2, & !  CHAR*2
                           OPERATOR_COUNTRY, & !  CHAR*20
                           OPERATOR_MARKET_AREA_ID*10, & !  CHAR*10
                           COMPANY_NAME, & !  CHAR*20
                           ENTITY_ID*5, & !  CHAR*5
                           ABBREVATION*3, & !  CHAR*3
                           STATE*2, & !  CHAR*2
                           ZIP*5, & !  CHAR*5
                           COUNTRY, & !  CHAR*20
                           ENTITY_TYPE*10, & !  CHAR*10
                           COMPANY_TYPE*10, & !  CHAR*10
                           NERC_REGION*10, & !  CHAR*10
                           NERC_SUB_REGION*10, & !  CHAR*10
                           PLANNING_AREA*5, & !  CHAR*5
                           CONTROL_AREA*5, & !  CHAR*5
                           HOLDING_COMPANY*5, & !  CHAR*5
                           TRANSMISSION_AREA*5, & !  CHAR*5
                           MARKET_AREA*5, & !  CHAR*5
                           NERC_ID*5, & !  CHAR*5
                           NERC_SUB_REGION_ID*5, & !  CHAR*5
                           CONTROL_AREA_ID*5, & !  CHAR*5
                           PLANNING_AREA_ID*5, & !  CHAR*5
                           HOLDING_COMPANY_ID*5, & !  CHAR*5
                           TRANSMISSION_AREA_ID*5, & !  CHAR*5
                           MARKET_AREA_ID*5, & !  CHAR*5
                           PLANT_NAME, & !  CHAR*20
                           PLANT_ID*5, & !  CHAR*5
                           UNIT_ID*5, & !  CHAR*1
                           UNIT_STATUS_CODE*2, & ! CHAR*2
!     +                     EFFECTIVE_DATE*8, ! CHAR* & !      +                     EFFECTIVE_DATE*8, ! CHAR*8
                           SERVICE_TYPE*8, & !  CHAR*8
                           UTILITY_NON_UTILITY_OWNED*1, & !  CHAR*1
                           FGD_UNIT_SCRUBBER_Y_N*1, & !  CHAR*1
                           GADS_CATEGORY*4, & !  CHAR*4
                           NERC_REGION_ID*5, & !  CHAR*5
                           NERC_SUBREGION_ID*5, & !  CHAR*5
                           RDI_MARKET_AREA_ID*5, & !  CHAR*5
                           PL_STATE*2, & !  CHAR*2
                           PL_COUNTY, & !  CHAR*20
                           PRIME_MOVER_CODE, & !  CHAR*2
                           PRIME_MOVER_GROUP*2, & !  CHAR*2
                           PRIME_MOVER*2, & !  CHAR*2
                           PRIMARY_FUEL_1*3, & !  CHAR*3
                           PRIMARY_FUEL_2*3, & !  CHAR*3
                           PRIME_MOVER_SHORT_DESC*5, & !  CHAR*5
                           PRIME_MOVER_DESCRIPTION, & ! CHAR*20
!     +                     UNIT_STATUS_CODE, ! CHAR*2 & !      +                     UNIT_STATUS_CODE, ! CHAR*20
                           UNIT_STATUS_CODE_SHORT_DESC*10, & !  CHAR*10
                           UNIT_STATUS_DESCRIPTION ! CHAR*20
!
!
!
      REAL(kind=4) :: &
                           OWNERSHIP_PERCENT, & 
                           SUMMER_CAPABILITY_MW, &
                           WINTER_CAPABILITY_MW, &
                           FULLY_LOADED_TESTED_HEAT_RATE, & 
                           FIXED_OM_COST_USDKW, & 
                           AVG_FUEL_PRICE_CENTSMMBTU, &
                           AVG_FUEL_PRICE_USDMWH, & 
                           NONFUELVARIABLE_OM_COSTUSDMWH
      LOGICAL(kind=1) :: LAHEY_LF95
      CHARACTER(len=30) :: SCREEN_OUTPUT
!
! END OF DATA DECLARATIONS
!
!
!
! PER SCOTT JONES COMPANY.TXT 9/5/97. GAT.
!
      ALLOCATE (RBC_VARIABLE_NAME(NUMBER_OF_RBC_VARIABLES))
!
      RBC_VARIABLE_NAME(1) = "Company Name"
      RBC_VARIABLE_NAME(2) = "Comp ID"
      RBC_VARIABLE_NAME(3) = "Comp Abbrev"
      RBC_VARIABLE_NAME(4) = "Entity Type"
      RBC_VARIABLE_NAME(5) = "Company Type"
      RBC_VARIABLE_NAME(6) = "Address 1"
      RBC_VARIABLE_NAME(7) = "Address 2"
      RBC_VARIABLE_NAME(8) = "City"
      RBC_VARIABLE_NAME(9) = "State"
      RBC_VARIABLE_NAME(10) = "Zip Code"
      RBC_VARIABLE_NAME(11) = "Phone NUM"
      RBC_VARIABLE_NAME(12) = "Country"
      RBC_VARIABLE_NAME(13) = "NERC Region"
      RBC_VARIABLE_NAME(14) = "Sub-Region"
      RBC_VARIABLE_NAME(15) = "Control Area"
      RBC_VARIABLE_NAME(16) = "Plan Area"
      RBC_VARIABLE_NAME(17) = "Holding Company"
      RBC_VARIABLE_NAME(18) = "Member"
      RBC_VARIABLE_NAME(19) = "Census Region"
      RBC_VARIABLE_NAME(20) = "Stock Exch"
      RBC_VARIABLE_NAME(21) = "Stock Symbol"
      RBC_VARIABLE_NAME(22) = "Source Form"
      RBC_VARIABLE_NAME(23) = "Fisc Year"
      RBC_VARIABLE_NAME(24) = "Peak Season"
      RBC_VARIABLE_NAME(25) = "Plant Name"
      RBC_VARIABLE_NAME(26) = "Plant ID"
      RBC_VARIABLE_NAME(27) = "Plant Address 1"
      RBC_VARIABLE_NAME(28) = "Plant Address 2"
      RBC_VARIABLE_NAME(29) = "Plant City"
      RBC_VARIABLE_NAME(30) = "Plant County"
      RBC_VARIABLE_NAME(31) = "Pl State"
      RBC_VARIABLE_NAME(32) = "Plant Zip Code"
      RBC_VARIABLE_NAME(33) = "Plant NERC"
      RBC_VARIABLE_NAME(34) = "Plant Sub-Region"
      RBC_VARIABLE_NAME(35) = "PM Abbrev"
      RBC_VARIABLE_NAME(36) = "Prime Mover"
      RBC_VARIABLE_NAME(37) = "PM Group"
      RBC_VARIABLE_NAME(38) = "Prime Mover Short Desc."
      RBC_VARIABLE_NAME(39) = "Prime Mover Description"
      RBC_VARIABLE_NAME(40) = "Prime Fuel"
      RBC_VARIABLE_NAME(41) = "Year"
      RBC_VARIABLE_NAME(42) = "Mth"
      RBC_VARIABLE_NAME(43) = "Plant Flag"
      RBC_VARIABLE_NAME(44) = "% Owned"
      RBC_VARIABLE_NAME(45) = "Demonstrated Capacity MW"
      RBC_VARIABLE_NAME(46) = "Net Gener MWh"
      RBC_VARIABLE_NAME(47) = "Capacity Factor %"
      RBC_VARIABLE_NAME(48) = "Fuel Cost $"
      RBC_VARIABLE_NAME(49) = "Fuel Adjustments $"
      RBC_VARIABLE_NAME(40) = "Total Fuel Cost $"
      RBC_VARIABLE_NAME(51) = "Incremental Fuel $/MWh"
      RBC_VARIABLE_NAME(52) = "Fuel $/MWh"
      RBC_VARIABLE_NAME(53) = "Var Non-Fuel OM $"
      RBC_VARIABLE_NAME(54) = "Fixed Non-Fuel OM $"
      RBC_VARIABLE_NAME(55) = "Total Prod Costs $"
      RBC_VARIABLE_NAME(56) = "Variable OM $/MWh"
      RBC_VARIABLE_NAME(57) = "Total Var Prod $/MWh"
      RBC_VARIABLE_NAME(58) = "Fixed OM $/kW Mth"
      RBC_VARIABLE_NAME(59) = "Total Prod $/MWh"
      RBC_VARIABLE_NAME(60) = "Average Heat Rate"
      RBC_VARIABLE_NAME(61) = "Date"
!
!
!  CONVERT THE RBC FILE

      ENTRY RBC_MAKEBIN

!
      BASE_FILE_NAME = RDI_BASECASE_FILE()
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME = trim(BASE_FILE_DIRECTORY())// &
                                   "RBB"//trim(BASE_FILE_NAME)//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      RBC_FILE_EXISTS = FILE_EXISTS
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
            CALL MG_LOCATE_WRITE(16,30,BASE_FILE_NAME,ALL_VERSIONS,0)
         ENDIF
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCRBCCL.BIN",ACCESS="DIRECT", &
                                            STATUS="UNKNOWN",RECL=LRECL)
!
! REFORMATTING PROGRAM BUYS ME: MIDAS DATA EDITOR, STANDARD FORMATTING ON READ
!
         READ(10,*,IOSTAT=IOS) DELETE
         IF(IOS /= 0) THEN
            CALL REFORMAT_BASECASE_DATA(FILE_NAME)
            READ(10,*,IOSTAT=IOS) DELETE
            IF(IOS /= 0) THEN
               WRITE(4,*) "Unsuccessful conversion of BASE CASE"
               WRITE(4,*) '*** line 1516 RDI_OBJT.FOR ***'
               er_message='See WARNING MESSAGES -RDI_OBJT.FOR-4'
               call end_program(er_message)
            ENDIF
         ENDIF
!
         IREC = 1
!
!
         NUM_OF_GEN_UNITS = 0
         RBC_CL_RECORDS = 0
         RBC_EL_RECORDS = 0
!
         DO
            NUM_OF_GEN_UNITS = NUM_OF_GEN_UNITS + 1
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(IOS /= 0) EXIT
            RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
            READ(RECLN,*,ERR=200) DELETE, &
                  COMPANY_NAME, &
                  COMP_ID, &
                  COMP_ABBREV, &
                  ENTITY_TYPE, &
                  COMPANY_TYPE, &
                  ADDRESS_1, &
                  ADDRESS_2, &
                  CITY, &
                  STATE, &
                  ZIP_CODE, &
                  PHONE_NUM, &
                  COUNTRY, &
                  NERC_REGION, &
                  SUB_REGION, &
                  CONTROL_AREA, &
                  PLAN_AREA, &
                  HOLDING_COMPANY, &
                  MEMBER, &
                  CENSUS_REGION, &
                  STOCK_EXCH, &
                  STOCK_SYMBOL, &
                  SOURCE_FORM, &
                  FISC_YEAR, &
                  PEAK_SEASON, &
                  PLANT_NAME, &
                  PLANT_ID, &
                  PLANT_ADDRESS_1, &
                  PLANT_ADDRESS_2, &
                  PLANT_CITY, &
                  PLANT_COUNTY, &
                  PL_STATE, &
                  PLANT_ZIP_CODE, &
                  PLANT_NERC, &
                  PLANT_SUB_REGION, &
                  PM_ABBREV, &
                  PRIME_MOVER, &
                  PM_GROUP, &
                  PRIME_MOVER_SHORT_DESC, &
                  PRIME_MOVER_DESCRIPTION, &
                  PRIME_FUEL, &
                  YEAR, & !  NUMBERS ! INT
                  MTH, & !  INT
                  PLANT_FLAG, & !  CHAR
                  PERCENT_OWNED, &
                  DEMONSTRATED_CAPACITY_MW, &
                  NET_GENER_MWH, &
                  CAPACITY_FACTOR_PERCENT, &
                  FUEL_COST_USD, &
                  FUEL_ADJUSTMENTS_USD, &
                  TOTAL_FUEL_COST_USD, &
                  INCREMENTAL_FUEL_USDMWH, &
                  FUEL_USDMWH, &
                  VAR_NON_FUEL_OM_USD, &
                  FIXED_NON_FUEL_OM_USD, &
                  TOTAL_PROD_COSTS_USD, &
                  VARIABLE_OM_USDMWH, &
                  TOTAL_VAR_PROD_USDMWH, &
                  FIXED_OM_USDKW_MTH, &
                  TOTAL_PROD_USDMWH, &
                  AVERAGE_HEAT_RATE, &
                  DATE, & !  INT
                  MONTH_GENERATION, & !  12 VALUES
                  MONTH_CAPACITY ! 12 VALUES
            IF(trim(PM_GROUP) == 'HY') THEN
               RBC_EL_RECORDS = RBC_EL_RECORDS + 1
            ELSE
               RBC_CL_RECORDS = RBC_CL_RECORDS + 1
            ENDIF
            WRITE(11,REC=IREC) DELETE, &
                  COMPANY_NAME, &
                  NERC_REGION, &
                  SUB_REGION, &
                  PLANT_NAME, &
                  PM_ABBREV, &
                  PRIME_FUEL, &
                  YEAR, & !  NUMBERS ! INT
                  MTH, & !  INT
                  PERCENT_OWNED, &
                  DEMONSTRATED_CAPACITY_MW, &
                  INCREMENTAL_FUEL_USDMWH, &
                  FUEL_USDMWH, &
                  VARIABLE_OM_USDMWH, &
                  FIXED_OM_USDKW_MTH, &
                  AVERAGE_HEAT_RATE, &
                  MONTH_GENERATION, & !  12 VALUES
                  MONTH_CAPACITY ! 12 VALUES
            IREC = IREC + 1
         ENDDO ! TABLE
         CLOSE(10)
         CLOSE(11)
      ELSE IF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "rdi_objt:1111")
      ENDIF
      RBC_RECORDS = IREC - 1
      RETURN
!
!
!
!  OVERLAY THE DAY TYPE FILE

      ENTRY RBC_MAKEOVL(OVERLAY_FAMILY_NAME)

!
      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME = get_rbo_filename(data_drive, overlay_family_name)
      OPEN(10,FILE=FILE_NAME)
      INUNIT = 12
      IF(RBC_OL == 'BC') THEN
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCRBCLC.BIN",ACCESS="DIRECT", &
                                                             RECL=LRECL)
         INUNIT = 11
      ENDIF
      OPEN(12,FILE=trim(DATA_DRIVE)//"OLRBCCL.BIN",ACCESS="DIRECT", &
                                            STATUS="UNKNOWN",RECL=LRECL)
      IREC = 0
      READ(10,*) DELETE
      IF(DELETE /= 9) THEN
         CALL REFORMAT_BASECASE_DATA(FILE_NAME)
      ENDIF
      DO
         READ(10,1000,IOSTAT=IOS) RECLN
         IF(IOS /= 0) EXIT
         IREC = IREC + 1
         READ(INUNIT,REC=IREC,IOSTAT=IOS) &
                  DELETE, &
                  COMPANY_NAME, &
                  NERC_REGION, &
                  SUB_REGION, &
                  PLANT_NAME, &
                  PM_ABBREV, &
                  PRIME_FUEL, &
                  YEAR, & !  NUMBERS ! INT
                  MTH, & !  INT
                  PERCENT_OWNED, &
                  DEMONSTRATED_CAPACITY_MW, &
                  INCREMENTAL_FUEL_USDMWH, &
                  FUEL_USDMWH, &
                  VARIABLE_OM_USDMWH, &
                  FIXED_OM_USDKW_MTH, &
                  AVERAGE_HEAT_RATE
         IF(IOS == 0) THEN
            RECLN = trim(RECLN)//',,,,,,,,,,,,,'
            READ(RECLN,*,ERR=300) DELETE, &
                  COMPANY_NAME, &
                  COMP_ID, &
                  COMP_ABBREV, &
                  ENTITY_TYPE, &
                  COMPANY_TYPE, &
                  ADDRESS_1, &
                  ADDRESS_2, &
                  CITY, &
                  STATE, &
                  ZIP_CODE, &
                  PHONE_NUM, &
                  COUNTRY, &
                  NERC_REGION, &
                  SUB_REGION, &
                  CONTROL_AREA, &
                  PLAN_AREA, &
                  HOLDING_COMPANY, &
                  MEMBER, &
                  CENSUS_REGION, &
                  STOCK_EXCH, &
                  STOCK_SYMBOL, &
                  SOURCE_FORM, &
                  FISC_YEAR, &
                  PEAK_SEASON, &
                  PLANT_NAME, &
                  PLANT_ID, &
                  PLANT_ADDRESS_1, &
                  PLANT_ADDRESS_2, &
                  PLANT_CITY, &
                  PLANT_COUNTY, &
                  PL_STATE, &
                  PLANT_ZIP_CODE, &
                  PLANT_NERC, &
                  PLANT_SUB_REGION, &
                  PM_ABBREV, &
                  PRIME_MOVER, &
                  PM_GROUP, &
                  PRIME_MOVER_SHORT_DESC, &
                  PRIME_MOVER_DESCRIPTION, &
                  PRIME_FUEL, &
                  YEAR, & !  NUMBERS ! INT
                  MTH, & !  INT
                  PLANT_FLAG, & !  CHAR
                  PERCENT_OWNED, &
                  DEMONSTRATED_CAPACITY_MW, &
                  NET_GENER_MWH, &
                  CAPACITY_FACTOR_PERCENT, &
                  FUEL_COST_USD, &
                  FUEL_ADJUSTMENTS_USD, &
                  TOTAL_FUEL_COST_USD, &
                  INCREMENTAL_FUEL_USDMWH, &
                  FUEL_USDMWH, &
                  VAR_NON_FUEL_OM_USD, &
                  FIXED_NON_FUEL_OM_USD, &
                  TOTAL_PROD_COSTS_USD, &
                  VARIABLE_OM_USDMWH, &
                  TOTAL_VAR_PROD_USDMWH, &
                  FIXED_OM_USDKW_MTH, &
                  TOTAL_PROD_USDMWH, &
                  AVERAGE_HEAT_RATE, &
                  DATE ! INT
         ENDIF
         WRITE(11,REC=IREC) DELETE, &
                  COMPANY_NAME, &
                  NERC_REGION, &
                  SUB_REGION, &
                  PLANT_NAME, &
                  PM_ABBREV, &
                  PRIME_FUEL, &
                  YEAR, & !  NUMBERS ! INT
                  MTH, & !  INT
                  PERCENT_OWNED, &
                  DEMONSTRATED_CAPACITY_MW, &
                  INCREMENTAL_FUEL_USDMWH, &
                  FUEL_USDMWH, &
                  VARIABLE_OM_USDMWH, &
                  FIXED_OM_USDKW_MTH, &
                  AVERAGE_HEAT_RATE
      ENDDO
      CLOSE(10)
      CLOSE(12)
      IF(RBC_OL == 'BC') CLOSE(11)
      RBC_OL = 'OL'
      RETURN
!

  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from RDI_OBJT SIID263'
      call end_program(er_message)

  300 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      CALL MG_LOCATE_WRITE(21,0, &
                           'Error reading the RDI record. Look for'// &
                                          ' a "," in a character name.', &
                           ALL_VERSIONS,1)
      er_message='stop requested from RDI_OBJT SIID264'
      call end_program(er_message)
!

      ENTRY RESET_RDI_BASECASE_OL

         RBC_OL = 'BC'
      RETURN
!

      ENTRY OPEN_RBC_CL_FILES(R_RBC_IN_UNIT,R_RBC_OUT_UNIT)

         OPEN(UNIT_IN_NUM,FILE=trim(OUTPUT_DIRECTORY())//RBC_OL// &
                "RBCCL.BIN",ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         OPEN(UNIT_IN_NUM)
         R_RBC_IN_UNIT = UNIT_IN_NUM
!
         OPEN(UNIT_OUT_NUM,FILE=trim(OUTPUT_DIRECTORY())// &
                                                         "CLBRBCCL.DAT")
         R_RBC_OUT_UNIT = UNIT_OUT_NUM
      RETURN
!

      ENTRY OPEN_RBC_EL_FILES(R_RBC_IN_UNIT,R_RBC_OUT_UNIT)

         OPEN(UNIT_IN_NUM,FILE=trim(OUTPUT_DIRECTORY())//RBC_OL// &
                "RBCCL.BIN",ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         OPEN(UNIT_IN_NUM)
         R_RBC_IN_UNIT = UNIT_IN_NUM
!
         OPEN(UNIT_OUT_NUM,FILE=trim(OUTPUT_DIRECTORY())// &
                                                         "ELBRBCEL.DAT")
         R_RBC_OUT_UNIT = UNIT_OUT_NUM
      RETURN
!

      ENTRY OPEN_RBC_FILE(R_RBC_IN_UNIT)

         OPEN(UNIT_IN_NUM,FILE=trim(OUTPUT_DIRECTORY())//RBC_OL// &
                "RBCCL.BIN",ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         OPEN(UNIT_IN_NUM)
         R_RBC_IN_UNIT = UNIT_IN_NUM
      RETURN
!

      ENTRY CLOSE_RBC_FILE

         CLOSE(UNIT_IN_NUM)
      RETURN

      ENTRY CLOSE_BOTH_RBC_FILES

         CLOSE(UNIT_IN_NUM)
         CLOSE(UNIT_OUT_NUM)
      RETURN

      ENTRY DOES_RBC_FILE_EXIST(R_RBC_FILE_EXISTS)

         R_RBC_FILE_EXISTS = RBC_FILE_EXISTS
      RETURN

      ENTRY GET_NUM_OF_RBC_UNITS(R_NUMBER_OF_UNITS)

         R_NUMBER_OF_UNITS = NUM_OF_GEN_UNITS
      RETURN

      ENTRY GET_NUM_OF_RBC_RECORDS(R_NUMBER_OF_UNITS)

         R_NUMBER_OF_UNITS = RBC_RECORDS
      RETURN

      ENTRY GET_NUM_OF_RBC_CL_RECORDS(R_NUMBER_OF_UNITS, &
                                                    R_NUMBER_LOCAL_TYPE)

         R_NUMBER_OF_UNITS = RBC_RECORDS
         R_NUMBER_LOCAL_TYPE = RBC_CL_RECORDS
      RETURN

      ENTRY GET_NUM_OF_RBC_EL_RECORDS(R_NUMBER_OF_UNITS, &
                                                    R_NUMBER_LOCAL_TYPE)

         R_NUMBER_OF_UNITS = RBC_RECORDS
         R_NUMBER_LOCAL_TYPE = RBC_EL_RECORDS
      RETURN
!
!

!
      ENTRY REFORMAT_BASECASE_DATA(R_FILE_NAME)
!

!
!
         TEMP_FILE_NAME = "RBC"//trim(RDI_BASECASE_FILE())//".DAT"
         CALL COPY(R_FILE_NAME,TEMP_FILE_NAME) ! MAKE A BACKUP
!
         ALLOCATE(MONTHS_PER_PLANT(MAX_RBC_PLANTS))
!
           MONTHS_PER_PLANT = 0.
         IREC = 0
         PLANT = 0
!
         CLOSE(10)
         OPEN(10,FILE=R_FILE_NAME)
         TEMP_FILE_NAME = "TEMP_RBC.DAT"
         OPEN(1952,FILE=TEMP_FILE_NAME)
         WRITE(1952,*) "9,"
         DELETE = 1
         LAST_REGION_NAME = '                    '
!
         LOCAL_CAP_MONTHS = 0.
         LOCAL_CAPACITY = 0.
         LOCAL_GEN_MONTHS = 0.
         LOCAL_GENERATION = 0.
         LOCAL_AHR = 0.
         LOCAL_FUEL_COST = 0.
         LOCAL_VAROM_COST =  0.
!
!
         MO = 0
         PLANT = 1
         DO
            MO  = MO + 1
!
            IREC = IREC + 1
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(IOS /= 0) EXIT
            READ(RECLN,*,ERR=200) &
                           OPERATOR_NAME, & 
                           OPERATOR_ID, & 
      	                  OPERATOR_ABBREVIATION, &
      	                  OPERATOR_ENTITY_TYPE, & 
                           OPERATOR_COMPANY_TYPE, &
                           OPERATOR_NERC_REGION, & 
                           OPERATOR_SUB_REGION, & 
                           OPERATOR_CONTROL_AREA, &
                           OPERATOR_PLANNING_AREA, &
                           OPERATOR_HOLDING_CO, & 
                           OPERATOR_STATE, & 
                           OPERATOR_COUNTRY, &
                           OPERATOR_MARKET_AREA_ID, & 
                           COMPANY_NAME, &
                           ENTITY_ID, & 
                           ABBREVATION, &
                           STATE, & 
                           ZIP, & 
                           COUNTRY, & 
                           ENTITY_TYPE, & 
                           COMPANY_TYPE, & 
                           NERC_REGION, & 
                           NERC_SUB_REGION, & 
                           PLANNING_AREA, & 
                           CONTROL_AREA, & 
                           HOLDING_COMPANY, & 
                           TRANSMISSION_AREA, & 
                           MARKET_AREA, & 
                           NERC_ID, & 
                           NERC_SUB_REGION_ID, & 
                           CONTROL_AREA_ID, & 
                           PLANNING_AREA_ID, & 
                           HOLDING_COMPANY_ID, &
                           TRANSMISSION_AREA_ID, & 
                           MARKET_AREA_ID, &
                           PLANT_NAME, & 
                           PLANT_ID, & 
                           UNIT_ID, & 
                           UNIT_STATUS_CODE, & 
                           SERVICE_TYPE, & 
                           OWNERSHIP_PERCENT, &
                           UTILITY_NON_UTILITY_OWNED, & 
                           FGD_UNIT_SCRUBBER_Y_N, &
                           GADS_CATEGORY, & 
                           NERC_REGION_ID, & 
                           NERC_SUBREGION_ID, &
                           RDI_MARKET_AREA_ID, &
                           STATE, & 
                           PL_COUNTY, & 
                           PRIME_MOVER_GROUP, &
                           PRIME_MOVER, & 
                           PRIMARY_FUEL_1, & 
                           PRIMARY_FUEL_2, & 
                           SUMMER_CAPABILITY_MW, & 
                           WINTER_CAPABILITY_MW, & 
                           FULLY_LOADED_TESTED_HEAT_RATE, & 
                           FIXED_OM_COST_USDKW, & 
                           AVG_FUEL_PRICE_CENTSMMBTU, & 
                           AVG_FUEL_PRICE_USDMWH, & 
                           NONFUELVARIABLE_OM_COSTUSDMWH, &
                           PRIME_MOVER_CODE, & 
                           PRIME_MOVER_GROUP, & 
                           PRIME_MOVER_SHORT_DESC, & 
                           PRIME_MOVER_DESCRIPTION, & 
                           UNIT_STATUS_CODE, & 
                           UNIT_STATUS_CODE_SHORT_DESC, & 
                           UNIT_STATUS_DESCRIPTION 
!
            IF(SUMMER_CAPABILITY_MW <= 0. .AND. &
                                       WINTER_CAPABILITY_MW <= 0.) then
            	CYCLE
            end if
            WRITE(1952,1020) &
                           trim(OPERATOR_NAME), & !  CHAR*20
                           trim(OPERATOR_ID), & !  CHAR*5
      	                  trim(OPERATOR_ABBREVIATION), & !  CHAR*10
      	                  trim(OPERATOR_ENTITY_TYPE), & !  CHAR*10
                           trim(OPERATOR_COMPANY_TYPE), & !  CHAR*10
                           trim(OPERATOR_NERC_REGION), & !  CHAR*20
                           trim(OPERATOR_SUB_REGION), & !  CHAR*20
                           trim(OPERATOR_CONTROL_AREA), & !  CHAR*20
                           trim(OPERATOR_PLANNING_AREA), & !  CHAR*20
                           trim(OPERATOR_HOLDING_CO), & !  CHAR*20
                           trim(OPERATOR_STATE), & !  CHAR*2
                           trim(OPERATOR_COUNTRY), & !  CHAR*20
                           trim(OPERATOR_MARKET_AREA_ID), & !  CHAR*10
                           trim(COMPANY_NAME), & !  CHAR*20
                           trim(ENTITY_ID), & !  CHAR*5
                           trim(ABBREVATION), & !  CHAR*3
                           trim(STATE), & !  CHAR*2
                           trim(ZIP), & !  CHAR*5
                           trim(COUNTRY), & !  CHAR*20
                           trim(ENTITY_TYPE), & !  CHAR*10
                           trim(COMPANY_TYPE), & !  CHAR*10
                           trim(NERC_REGION), & !  CHAR*10
                           trim(NERC_SUB_REGION), & !  CHAR*10
                           trim(PLANNING_AREA), & !  CHAR*5
                           trim(CONTROL_AREA), & !  CHAR*5
                           trim(HOLDING_COMPANY), & !  CHAR*5
                           trim(TRANSMISSION_AREA), & !  CHAR*5
                           trim(MARKET_AREA), & !  CHAR*5
                           trim(NERC_ID), & !  CHAR*5
                           trim(NERC_SUB_REGION_ID), & !  CHAR*5
                           trim(CONTROL_AREA_ID), & !  CHAR*5
                           trim(PLANNING_AREA_ID), & !  CHAR*5
                           trim(HOLDING_COMPANY_ID), & !  CHAR*5
                           trim(TRANSMISSION_AREA_ID), & !  CHAR*5
                           trim(MARKET_AREA_ID), & !  CHAR*5
                           trim(PLANT_NAME), & !  CHAR*20
                           trim(PLANT_ID), & !  CHAR*5
                           trim(UNIT_ID), & !  CHAR*1
                           trim(UNIT_STATUS_CODE), & ! CHAR*2

                           trim(SERVICE_TYPE), & !  CHAR*8
                           OWNERSHIP_PERCENT, & !  REAL(kind=4
                           trim(UTILITY_NON_UTILITY_OWNED), & !  CHAR*1
                           trim(FGD_UNIT_SCRUBBER_Y_N), & !  CHAR*1
                           trim(GADS_CATEGORY), & !  CHAR*4
                           trim(NERC_REGION_ID), & !  CHAR*5
                           trim(NERC_SUBREGION_ID), & !  CHAR*5
                           trim(RDI_MARKET_AREA_ID), & !  CHAR*5
                           trim(STATE), & !  CHAR*2
                           trim(PL_COUNTY), & !  CHAR*20
                           trim(PRIME_MOVER_GROUP), & !  CHAR*2
                           trim(PRIME_MOVER), & !  CHAR*2
                           trim(PRIMARY_FUEL_1), & !  CHAR*3
                           trim(PRIMARY_FUEL_2), & !  CHAR*3
                           SUMMER_CAPABILITY_MW, & !  REAL(kind=4
                           WINTER_CAPABILITY_MW, & !  REAL(kind=4
                           FULLY_LOADED_TESTED_HEAT_RATE, & !  REAL(kind=4
                           FIXED_OM_COST_USDKW, & !  REAL(kind=4
                           AVG_FUEL_PRICE_CENTSMMBTU, & !  REAL(kind=4
                           AVG_FUEL_PRICE_USDMWH, & !  REAL(kind=4
                           NONFUELVARIABLE_OM_COSTUSDMWH, & !  REAL(kind=4
                           trim(PRIME_MOVER_CODE), & !  CHAR*2
                           trim(PRIME_MOVER_GROUP), & !  CHAR*2
                           trim(PRIME_MOVER_SHORT_DESC), & !  CHAR*5
                           trim(PRIME_MOVER_DESCRIPTION), & !  CHAR*20
                           trim(UNIT_STATUS_CODE), & !  CHAR*20
                           trim(UNIT_STATUS_CODE_SHORT_DESC), & !  CHAR*10
                           trim(UNIT_STATUS_DESCRIPTION) ! CHAR*20
!
         ENDDO ! READING RBC RECORDS
!
         CLOSE(10)
         CLOSE(1952)
         ! TODO: Call COPY_FILE instead
         CALL COPY(TEMP_FILE_NAME,R_FILE_NAME)

         OPEN(10,FILE=R_FILE_NAME)

         IF(ALLOCATED(MONTHS_PER_PLANT)) DEALLOCATE(MONTHS_PER_PLANT)

      RETURN

 1000 FORMAT(A)
 1010 FORMAT('&',A)
 1020 FORMAT('1',40(',','"',A,'"'),',',F8.2, &
                  12(',','"',A,'"'),7(',',F10.1), &
                  7(',','"',A,'"'))
      END

