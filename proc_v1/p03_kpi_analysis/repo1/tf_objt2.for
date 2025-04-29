
!     Scenario Index.for
!     Copyright(c) Global Energy Decisions 2000
!
!     Created: 10/2/2007 2:07:52 PM
!     Author : Tom Sweet
!     Last change: msg 9/21/2021 4:04:50 PM



      FUNCTION GET_SCENARIO_INDEX(SCENARIO_VARIABLE)

C
!
! HIGHEST INDEX = 48 ON 12/23/05
! MARK IS RESERVING INDICES 20-42 AS NUMERICALLY DEFINED
! FOR "GENERAL" DISTRIBUTIONS
!
        use filename_tracker
        use capacity_arrays
        implicit none
        

      CHARACTER(len=*) :: SCENARIO_VARIABLE
      INTEGER(kind=2) :: SCENARIO_INDEX,GET_SCENARIO_INDEX
C
         SELECT CASE(trim(SCENARIO_VARIABLE))
            CASE ('Coal Price')
               SCENARIO_INDEX = 1
            CASE ('Gas Price')
               SCENARIO_INDEX = 2
            CASE ('Oil Price')
               SCENARIO_INDEX = 3
            CASE ('Demand')
               SCENARIO_INDEX = 4
            CASE ('Weather')
               SCENARIO_INDEX = 4
            CASE ('Nuclear Availability')
               SCENARIO_INDEX = 5
            CASE ('Coal Availability')
               SCENARIO_INDEX = 6
            CASE ('Hydro Year')
               SCENARIO_INDEX = 7
            CASE ('Hydro Output')
               SCENARIO_INDEX = 7
            CASE ('Transmission Availability')
               SCENARIO_INDEX = 8
            CASE ('Capital Expansion Cost')
               SCENARIO_INDEX = 9
            CASE ('SO2 Price')
               SCENARIO_INDEX = 10
            CASE ('NOx Price')
               SCENARIO_INDEX = 11
            CASE ('CO2 Price')
               SCENARIO_INDEX = 12
            CASE ('Reserve Margin')
               SCENARIO_INDEX = 13
            CASE ('Electricity Price')
               SCENARIO_INDEX = 14
            CASE ('Peak')
               SCENARIO_INDEX = 15
            CASE ('Energy')
               SCENARIO_INDEX = 16
            CASE ('Load-Shape Year')
               SCENARIO_INDEX = 17
            CASE ('Forced-Outage Seed')
               SCENARIO_INDEX = 18
            CASE ('Interest Rate')
               SCENARIO_INDEX = 19
            CASE ('Residential Energy','Residential')
               SCENARIO_INDEX = 20
            CASE ('Commercial Energy','Commercial')
               SCENARIO_INDEX = 21
            CASE ('Industrial Energy','Industrial')
               SCENARIO_INDEX = 22
            CASE ('Fossil Fuel')
               SCENARIO_INDEX = 23
            CASE ('Purchased Power')
               SCENARIO_INDEX = 24
            CASE ('Purchased Gas')
               SCENARIO_INDEX = 25
            CASE ('Interest Rate [Financial]','General Interest Rate')
               SCENARIO_INDEX = 26
            CASE ('FinancialVar08','STD Interest Rate','STD Interest')
               SCENARIO_INDEX = 27
            CASE ('FinancialVar09','LTD Interest Rate','LTD Interest')
               SCENARIO_INDEX = 26
            CASE ('FinancialVar10')
               SCENARIO_INDEX = 29
            CASE ('FinancialVar11')
               SCENARIO_INDEX = 30
            CASE ('FinancialVar12')
               SCENARIO_INDEX = 31
            CASE ('FinancialVar13')
               SCENARIO_INDEX = 32
            CASE ('FinancialVar14')
               SCENARIO_INDEX = 33
            CASE ('FinancialVar15')
               SCENARIO_INDEX = 34
            CASE ('Gas Availability')
               SCENARIO_INDEX = 43
            CASE ('Oil Availability')
               SCENARIO_INDEX = 44
            CASE ('Other Availability')
               SCENARIO_INDEX = 45
            CASE ('Gas Basis')
               SCENARIO_INDEX = 46
            CASE ('HG Price ')
               SCENARIO_INDEX = 57
!               SCENARIO_INDEX = 47
            CASE ('Annual NOx Price')
              SCENARIO_INDEX = 47
            CASE ('Other Emission Price') ! TMS 20071002
               SCENARIO_INDEX = 48
            CASE ('Gas Demand')
               SCENARIO_INDEX = 49
            CASE ('Potential Reserves')
               SCENARIO_INDEX = 50
!           CASE ('Gas Supply Costs')
            CASE ('Gas Supply Cost')     ! TMS 20071002
!TMS 20071002 SCENARIO_INDEX 52 AND 53 ARE FINANCIAL VARIABLES 24 AND 25
               SCENARIO_INDEX = 51
            CASE ('Uranium Price')
               SCENARIO_INDEX = 54       ! TMS 20071002
            CASE ('OnPeak Electricity Price')
               SCENARIO_INDEX = 55       ! TMS 20071002
            CASE ('OffPeak Electricity Price')
               SCENARIO_INDEX = 56       ! TMS 20071002
            CASE ('CO2 Allocations')
               SCENARIO_INDEX = 58       ! GAT 20100823
            CASE ('Residential Gas')
               SCENARIO_INDEX = 59
            CASE ('Commercial Gas')
               SCENARIO_INDEX = 60
            CASE ('Industrial Gas')
               SCENARIO_INDEX = 61
            CASE ('Power Gas')
               SCENARIO_INDEX = 62
            CASE ('Transmission Derate') ! GAT 052712 for KCPL
               SCENARIO_INDEX = 63
            CASE ('Not Active')
               SCENARIO_INDEX = -99
            CASE DEFAULT
               SCENARIO_INDEX = 0
         END SELECT
         IF(SCENARIO_INDEX == 0) THEN
            IF(INDEX(SCENARIO_VARIABLE,'1') /= 0) SCENARIO_INDEX = 20
            IF(INDEX(SCENARIO_VARIABLE,'2') /= 0) SCENARIO_INDEX = 21
            IF(INDEX(SCENARIO_VARIABLE,'3') /= 0) SCENARIO_INDEX = 22
            IF(INDEX(SCENARIO_VARIABLE,'4') /= 0) SCENARIO_INDEX = 23
            IF(INDEX(SCENARIO_VARIABLE,'5') /= 0) SCENARIO_INDEX = 24
            IF(INDEX(SCENARIO_VARIABLE,'6') /= 0) SCENARIO_INDEX = 25
            IF(INDEX(SCENARIO_VARIABLE,'7') /= 0) SCENARIO_INDEX = 26
            IF(INDEX(SCENARIO_VARIABLE,'8') /= 0) SCENARIO_INDEX = 27
            IF(INDEX(SCENARIO_VARIABLE,'9') /= 0) SCENARIO_INDEX = 28
            IF(INDEX(SCENARIO_VARIABLE,'10') /= 0) SCENARIO_INDEX = 29
            IF(INDEX(SCENARIO_VARIABLE,'11') /= 0) SCENARIO_INDEX = 30
            IF(INDEX(SCENARIO_VARIABLE,'12') /= 0) SCENARIO_INDEX = 31
            IF(INDEX(SCENARIO_VARIABLE,'13') /= 0) SCENARIO_INDEX = 32
            IF(INDEX(SCENARIO_VARIABLE,'14') /= 0) SCENARIO_INDEX = 33
            IF(INDEX(SCENARIO_VARIABLE,'15') /= 0) SCENARIO_INDEX = 34
            IF(INDEX(SCENARIO_VARIABLE,'16') /= 0) SCENARIO_INDEX = 35
            IF(INDEX(SCENARIO_VARIABLE,'17') /= 0) SCENARIO_INDEX = 36
            IF(INDEX(SCENARIO_VARIABLE,'18') /= 0) SCENARIO_INDEX = 37
            IF(INDEX(SCENARIO_VARIABLE,'19') /= 0) SCENARIO_INDEX = 38
            IF(INDEX(SCENARIO_VARIABLE,'20') /= 0) SCENARIO_INDEX = 39
            IF(INDEX(SCENARIO_VARIABLE,'21') /= 0) SCENARIO_INDEX = 40
            IF(INDEX(SCENARIO_VARIABLE,'22') /= 0) SCENARIO_INDEX = 41
            IF(INDEX(SCENARIO_VARIABLE,'23') /= 0) SCENARIO_INDEX = 42
            IF(INDEX(SCENARIO_VARIABLE,'24') /= 0) SCENARIO_INDEX = 52
            IF(INDEX(SCENARIO_VARIABLE,'25') /= 0) SCENARIO_INDEX = 53
         ENDIF
         GET_SCENARIO_INDEX = SCENARIO_INDEX
      RETURN
      END FUNCTION GET_SCENARIO_INDEX
!

C
!                  ROUTINE TO CONVERT UNIT OUTAGES FILE
C
!                          COPYRIGHT (C) 1999
!                     M.S. GERBER & ASSOCIATES, INC.
!                          ALL RIGHTS RESERVED
C

C
      SUBROUTINE UNIT_OUTAGES_OBJECT
      use end_routine, only: end_program, er_message
      use SpinDriftLib
      use prod_arrays_dimensions
      use filename_tracker
      use dr_booth_modules
      USE SIZECOM


      INTEGER(kind=4) ::   OUTAGE_START_DATE(20),

     +            OUTAGE_STOP_DATE(20)
      INTEGER(kind=2) ::   I
      INTEGER(kind=8) ::   UNIT_ID
      CHARACTER(len=20) :: UNIT_NAME
      character(len=20) :: OUTAGE_PRODUCT_TYPE
      CHARACTER(len=1) ::  OUTAGE_RECORD_ACTIVE,
     +             DOWNTIME_TYPE,
     +             DERATE_UNITS
!
      INTEGER(kind=2) ::   DELETE,INUNIT,LRECL=217
      INTEGER :: IOS,IREC,
     +            SAVE_NUMBER_OF_OUTAGES=0,R_NUMBER_OF_OUTAGES
!
      INTEGER(kind=2) ::   UNIT_NUM=10,MAX_WHEELS,
     +            SELLER_TRANSACTION_GROUP,
     +            BUYER_TRANSACTION_GROUP
      PARAMETER   (MAX_WHEELS = 10)
      REAL(kind=4) ::      DERATE_QUANTITY
      INTEGER(kind=2) ::   WHEEL_OUTAGE(MAX_WHEELS),
     +            R_WHEELS
!     +            OUTAGE_OWNER
      CHARACTER(len=5) :: BASE_FILE_NAME,OVERLAY_FAMILY_NAME,
     +            UNIT_OUTAGE_FILE
      CHARACTER(len=20) :: SELLER_NAME,BUYER_NAME
      CHARACTER(len=256) :: FILE_NAME
      CHARACTER(len=256) :: BASE_FILE_DIRECTORY
      CHARACTER(len=256) :: DATA_DRIVE,OUTPUT_DIRECTORY
      CHARACTER(len=1) ::  OUTAGE_ACTIVE
      LOGICAL(kind=4) ::    FILE_EXISTS,OUTAGE_FILE_EXISTS=.FALSE.,
     +             R_OUTAGE_FILE_EXISTS
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER(len=1024) :: RECLN
! DECLARATION FOR TRANSACT OUTAGE DETERMINANTS
      CHARACTER(len=16) :: FILE_TYPE='TRANSACT OUTAGES  '
      CHARACTER(len=2) ::  OUTAGE_OL='BC'
      LOGICAL(kind=1) :: LAHEY_LF95
      CHARACTER(len=30) :: SCREEN_OUTPUT

! CONVERT THE OUTAGE FILE
!
!
!

      ENTRY OUTAGE_MAKEBIN

      BASE_FILE_NAME = UNIT_OUTAGE_FILE()

      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME = get_teb_filename(base_file_name)
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      OUTAGE_FILE_EXISTS = FILE_EXISTS
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_LOCATE_WRITE(16,30,BASE_FILE_NAME,ALL_VERSIONS,0)
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         ENDIF
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=trim(DATA_DRIVE)//
     +                      "BCOUTAGE.BIN",ACCESS="DIRECT",
     +                                     STATUS="UNKNOWN",RECL=LRECL)
         IREC = 1

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
C
C
! JANET ADDED THE FOLLOWING CINITW

            OUTAGE_START_DATE = -999999
            OUTAGE_STOP_DATE = -999999

            READ(RECLN,*,ERR=200) DELETE,
     +            UNIT_NAME,
     +            UNIT_ID,
     +            OUTAGE_RECORD_ACTIVE,
     +               OUTAGE_START_DATE,
     +            OUTAGE_STOP_DATE,
     +            DOWNTIME_TYPE,
     +            DERATE_UNITS,
     +            OUTAGE_PRODUCT_TYPE,
     +            DERATE_QUANTITY
!
! 4/10/02. PROPOGATE DOWN FOR TVA
!
           IF(OUTAGE_START_DATE(1) == -999999) OUTAGE_START_DATE(1) = 0
            DO I = 2, 20
               IF(OUTAGE_START_DATE(I)==-999999)
     +                    OUTAGE_START_DATE(I) = OUTAGE_START_DATE(I-1)
            ENDDO
            IF(OUTAGE_STOP_DATE(1) == -999999) OUTAGE_STOP_DATE(1) = 0
            DO I = 2, 20
               IF(OUTAGE_STOP_DATE(I)==-999999)
     +                     OUTAGE_STOP_DATE(I) = OUTAGE_STOP_DATE(I-1)
            ENDDO
!
            WRITE(11,REC=IREC) DELETE,
     +            UNIT_NAME,
     +            UNIT_ID,
     +            OUTAGE_RECORD_ACTIVE,
     +               OUTAGE_START_DATE,
     +            OUTAGE_STOP_DATE,
     +            DOWNTIME_TYPE,
     +            DERATE_UNITS,
     +            OUTAGE_PRODUCT_TYPE,
     +            DERATE_QUANTITY

            IREC = IREC + 1
         ENDDO
         CLOSE(10)
         CLOSE(11)
      ELSE IF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "tf_objt2:1111")
      ENDIF
!
      SAVE_NUMBER_OF_OUTAGES = MAX(0,IREC-1)
!
      RETURN



! OVERLAY THE OUTAGE FILE

      ENTRY OUTAGE_MAKEOVL(OVERLAY_FAMILY_NAME)


      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_CLEAR_LINE_WRITE(17,9,36,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME = get_teo_filename(DATA_DRIVE, OVERLAY_FAMILY_NAME)
      OPEN(10,FILE=FILE_NAME)
      READ(10,*) DELETE
      INUNIT = 12
      IF(OUTAGE_OL == 'BC') THEN
         OPEN(11,FILE=trim(DATA_DRIVE)//
     +                       "BCOUTAGE.BIN",ACCESS="DIRECT",RECL=LRECL)
         INUNIT = 11
      ENDIF
      OPEN(12,FILE=trim(DATA_DRIVE)//"OLOUTAGE.BIN",ACCESS="DIRECT",
     +                                     STATUS="UNKNOWN",RECL=LRECL)
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
     +            UNIT_NAME,
     +            UNIT_ID,
     +            OUTAGE_RECORD_ACTIVE,
     +               OUTAGE_START_DATE,
     +            OUTAGE_STOP_DATE,
     +            DOWNTIME_TYPE,
     +            DERATE_UNITS,
     +            OUTAGE_PRODUCT_TYPE,
     +            DERATE_QUANTITY

         IF(IOS /= 0) EXIT
!         READ(10,1000,IOSTAT=IOS) RECLN
!         IF(IOS == 0) THEN
         RECLN = trim(RECLN)//',,,,,,,,,,,,,'
         READ(RECLN,*,ERR=300) DELETE,
     +            UNIT_NAME,
     +            UNIT_ID,
     +            OUTAGE_RECORD_ACTIVE,
     +               OUTAGE_START_DATE,
     +            OUTAGE_STOP_DATE,
     +            DOWNTIME_TYPE,
     +            DERATE_UNITS,
     +            OUTAGE_PRODUCT_TYPE,
     +            DERATE_QUANTITY
!         ENDIF
!
         READ(10,1000,IOSTAT=IOS) RECLN
         DOWHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE
!
            READ(10,1000,IOSTAT=IOS) RECLN
!
         ENDDO
         WRITE(12,REC=IREC) DELETE,
     +            UNIT_NAME,
     +            UNIT_ID,
     +            OUTAGE_RECORD_ACTIVE,
     +               OUTAGE_START_DATE,
     +            OUTAGE_STOP_DATE,
     +            DOWNTIME_TYPE,
     +            DERATE_UNITS,
     +            OUTAGE_PRODUCT_TYPE,
     +            DERATE_QUANTITY
!
      ENDDO
      CLOSE(10)
      CLOSE(12)
      IF(OUTAGE_OL == 'BC') CLOSE(11)
      OUTAGE_OL = 'OL'
      RETURN
!


  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from TF_OBJT2 SIID292'
      call end_program(er_message)


  300 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      CALL MG_LOCATE_WRITE(21,0,
     +               'Error reading the Outage record. Look for'//
     +                              ' a "," in a character name.',
     +                                                  ALL_VERSIONS,1)
      er_message='stop requested from TF_OBJT2 SIID293'
      call end_program(er_message)
C

      ENTRY RESET_OUTAGE_OL

         OUTAGE_OL = 'BC'
      RETURN
C

      ENTRY OPEN_OUTAGE_FILE

         OPEN(UNIT_NUM,FILE=trim(OUTPUT_DIRECTORY())//OUTAGE_OL//
     +        "OUTAGE.BIN",ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      RETURN
C

      ENTRY CLOSE_OUTAGE_FILE

         CLOSE(UNIT_NUM)
      RETURN

      ENTRY DOES_OUTAGE_FILE_EXIST(R_OUTAGE_FILE_EXISTS)

         R_OUTAGE_FILE_EXISTS = OUTAGE_FILE_EXISTS
      RETURN

      ENTRY GET_MAX_OUTAGES(R_NUMBER_OF_OUTAGES)

!         R_WHEELS = MAX_WHEELS
         R_NUMBER_OF_OUTAGES = SAVE_NUMBER_OF_OUTAGES
      RETURN
!
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END

!
      FUNCTION READ_TRANS_OUTAGE_DATA()
      use end_routine, only: end_program, er_message

      use mod_base_year
      use SpinDriftLib
      use prod_arrays_dimensions
      use sizecom
      use globecom
      use tf_objt_interfaces
      implicit none
      
      SAVE
      integer (kind=2) :: imm_un
      INTEGER(kind=2) :: MAX_START_STOP,START_STOP ! START = 2, STOP = 3

      PARAMETER   (MAX_START_STOP=20)
      INTEGER(kind=4) ::   OUTAGE_START_DATE(MAX_START_STOP),
     +            OUTAGE_STOP_DATE(MAX_START_STOP),
     +            MAX_OUTAGES,
     +            SAVE_MAX_OUTAGES=0,
     +            SAVE_TOTAL_OUTAGES=0
      INTEGER(kind=8) ::   UNIT_ID(:),LOCAL_UNIT_ID_I8
      INTEGER(kind=2) ::
     +            U_FOR,
     +            TOTAL_INDEXED_OUTAGES,
     +            TG,R_TG,
     +            IND,
     +            TEST_DATE,
     +            TEST_DATE_1,
     +            TEST_DATE_2,
     +            BEGIN_MONTH_DAY(:),
     +            END_MONTH_DAY(:),
     +            ANNUAL_UNIT_NUM(:),
     +            ANNUAL_ACTIVE_OUTAGES=0,
     +            TEST_YEAR,
     +            RECORD_ITEM,
     +            EVENTS_IN_DAY(31,3),
     +            EVENTS_IN_MONTH,
     +            MAX_EVENTS_IN_MONTH=20000,
     +            MAX_TRANS_GROUPS=0,
     +            BEGIN_IN_MONTH

      INTEGER(kind=2) ::
     +            END_IN_MONTH,
     +            BEGIN_IN_DAY,
     +            END_IN_DAY,
     +            BEGIN_IN_HOUR,
     +            END_IN_HOUR,
     +            BEGIN_IN_HOUR_ALAN(:),
     +            END_IN_HOUR_ALAN(:),
     +            DAY,
     +            R_HOUR,
     +            GET_UNIT_TO_OUTAGE_BLOCK,
     +            LOCAL_HOUR,
     +            LAST_HOUR,
     +            MAX_HOURS,
     +            OUTAGE_BLOCK1,
     +            OUTAGE_BLOCK2
      INTEGER(kind=2) ::
     +            UNIT_NUMBER,
     +            GET_RESOURCE_ID_TO_UNIT,
     +            GET_I8_ID_TO_UNIT,
     +            DAYS_IN_MONTH,
     +            GET_TRANS_GROUP_POSITION,
     +            get_TRANSACTION_GROUP,
     +            R_DATA_BASES,
     +            HOURS_IN_MONTH(12)=(/744,672,744,720,744,720,
     +                               744,744,720,744,720,744/),
     +            MAX_FO_PER_MONTH=1,GET_MAX_FO_PER_MONTH
      INTEGER(kind=2) :: GET_ANNUAL_OUTAGE_NUM,R_BEGIN_MONTH_DAY,
     + R_END_MONTH_DAY,
     +            R_UNIT_NUM
      integer (kind=2) :: R_OUTAGE
      REAL(kind=4) ::    R_OUTAGE_SINGULAR(0:*),
     +            R_ADJUST_BLOCK(0:*),
     +            R_DERATE_BLOCK(0:*),
     +            R_OUTAGE_TYPE(0:*),
     +            DERATE_QUANTITY(:),
     +            BLOCK1_DOWN_MULT,
     +            BLOCK2_DOWN_MULT,
     +            BLOCK1_CAPACITY,
     +            BLOCK2_CAPACITY,
     +            TOTAL_UNIT_CAPACITY,
     +            DERATE_MW,
     +            GET_OUTAGE_CAPACITY,
     +            GET_BLOCK_OUTAGE_CAPACITY

      CHARACTER(len=20), allocatable :: UNIT_NAMEs_loc(:),
     +             OUTAGE_PRODUCT_TYPE(:)
      CHARACTER(len=1) ::  OUTAGE_RECORD_ACTIVE,
     +             DOWNTIME_TYPE(:),
     +             DERATE_UNITS(:)
!
      INTEGER(kind=2) :: DELETE
      INTEGER :: IOS,IREC
!
      INTEGER(kind=2) :: UNIT_NUM=10
      LOGICAL(kind=4) :: OUTAGE_FILE_EXISTS=.FALSE.,
     +            DAILY_OUTAGE_CHECK,
     +            APPLY_ENERGY_PRODUCT,
     +            HOURLY_OUTAGE_CHECK
      LOGICAL(kind=1) :: SAVE_OUTAGE_FILE_EXISTS=.FALSE.,
     +            OUTAGE_EVENTS_IN_HOUR,
     +            ADJUST_FOR_OUTAGES,
     +            YES_DETAILED_MAINTENANCE,
     +            YES_DETAILED_FOR,
     +            UNIT_ON_LINE_IN_YEAR,
     +            START_ENCOUNTERED,
     +            STOP_ENCOUNTERED,
     +            INTERCHANGE_FOUND
      INTEGER(kind=2) :: GET_ANNUAL_OUTAGE_DATES, GAOD_result
      INTEGER(kind=4) :: VALUES_2_ZERO
      INTEGER(kind=2) ::
     +            SAVE_TRANS_OUTAGE_RECORDS=0,
     +            MONTHLY_ACTIVE_OUTAGES=0,
     +            R_MONTH,
     +            R_YEAR,
     +            R_NUNITS,
     +            R_DAY_IN_MONTH,
     +            R_DAY_IN_WEEK,
     +            R_HOUR_IN_DAY,
     +            R_UNIT_ID,
     +            NUM_ACTIVE_RECORDS=0
      INTEGER(kind=2) ::
     +            LOCAL_UNIT_ID,
     +            TEMP_UNIT_NUMBER,
     +            TEMP_HIGHEST_UNIT_NUMBER,
     +            NUMBER_OF_UNITS_FOR_ID,
     +            CALENDAR_DAY_OF_WEEK,
     +            GET_DAY_OF_WEEK_4,
     +            LOCAL_MAINT_NUNITS=0,
     +            ACTIVE_MAINT_NUNITS=0,
     +            ACTIVE_FOR_NUNITS=0,
     +            LOCAL_FOR_NUNITS=0,
     +            TEMP_IREC
      LOGICAL(kind=1) :: READ_TRANS_OUTAGE_DATA,
     +            MONTHLY_ACTIVE_TRANS_OUTAGES,
     +            ANNUAL_ACTIVE_TRANS_OUTAGES,
     +            SAVE_MONTHLY_TRANS_OUTAGES=.FALSE.,
     +            OUTAGE_EVENTS_IN_MONTH,
     +            YES_RUN_TRANSACT,
     +            DEALLOCATE_TRANS_OUTAGE_DATA,
     +            ACU_POWER_ID_INDEX,GET_ACU_POWER_ID_INDEX
      INTEGER(KIND=2), ALLOCATABLE :: BEGIN_YEAR(:,:),END_YEAR(:,:)
      INTEGER(KIND=4), ALLOCATABLE :: BEGIN_TO(:,:),END_TO(:,:)
      INTEGER(kind=2) ::
     +               BEGIN_DAY(:,:),
     +               BEGIN_MONTH(:,:),
     +               END_DAY(:,:),
     +               END_MONTH(:,:),
     +               BEGIN_DAY_IN_MONTH(:,:),
     +               END_DAY_IN_MONTH(:,:),
     +               RECORD_ACTIVE_IN_MONTH(:),
     +               OUTAGES_IN_MONTH_BY_TG,
     +               HOURS_IN(12)=(/744,672,744,720,744,720,
     +                            744,744,720,744,720,744/)
      INTEGER(kind=4) ::  EVENTS_RECORD_INDEX(:,:),
     +               EVENTS_LINK_INDEX(:,:),
     +               EVENTS_START_STOP_INDEX(:,:),
     +               EVENTS_IN_HOUR(:,:),
     +               MONTH_EVENT_COUNTER(:),
     +               FIRST_EVENT_IN_HOUR(:,:),
     +               MEC,
     +               I,J,K

      ALLOCATABLE ::
     +            UNIT_ID,
     +            BEGIN_DAY,
     +            BEGIN_MONTH,
     +            END_DAY,
     +            END_MONTH,
     +            BEGIN_DAY_IN_MONTH,
     +            END_DAY_IN_MONTH,
     +            RECORD_ACTIVE_IN_MONTH,
     +            DOWNTIME_TYPE,
     +            DERATE_UNITS
     
      ALLOCATABLE ::
     +            DERATE_QUANTITY,
     +            EVENTS_RECORD_INDEX,
     +            EVENTS_LINK_INDEX,
     +            EVENTS_START_STOP_INDEX,
     +            MONTH_EVENT_COUNTER,
     +            FIRST_EVENT_IN_HOUR,
     +            EVENTS_IN_HOUR,
     +            BEGIN_MONTH_DAY,
     +            END_MONTH_DAY,
     +            ANNUAL_UNIT_NUM,
     +            BEGIN_IN_HOUR_ALAN,
     +            END_IN_HOUR_ALAN
! AUTO-MAINTENANCE
      LOGICAL(kind=1) :: AUTO_MAINT_AS_HOURLY=.FALSE.,
     +            PRESCED_ANN_OUT_ALWAYS=.FALSE.,
     +            AUTO_FOR_AS_HOURLY=.FALSE.
      INTEGER(kind=2) :: BEG_YYMM,BEG_MMDD(2),END_YYMM,END_MMDD(2),U,
     +            OUTAGE_INDEX
      INTEGER(KIND=2), ALLOCATABLE :: CLM_MAINT_INDEX(:)
      INTEGER(KIND=2) :: DAYS_IN_EACH_MONTH
      
      integer (kind=2) :: gtgi_result
!
!
! END DATA DECLARATIONS
!
!
!
         READ_TRANS_OUTAGE_DATA = .FALSE.
         SAVE_OUTAGE_FILE_EXISTS = .FALSE.

         MAX_TRANS_GROUPS = GET_NUMBER_OF_ACTIVE_GROUPS()

         CALL DOES_OUTAGE_FILE_EXIST(OUTAGE_FILE_EXISTS)

         IF(.NOT. OUTAGE_FILE_EXISTS) RETURN

         CALL GET_MAX_OUTAGES(MAX_OUTAGES)
         SAVE_MAX_OUTAGES = MAX_OUTAGES

         CALL OPEN_OUTAGE_FILE
         if(allocated(UNIT_ID)) then
            deallocate(unit_id)
         endif
         if(allocated(BEGIN_DAY)) then
            deallocate(BEGIN_DAY)
         endif
         if(allocated(BEGIN_MONTH)) then
            deallocate(BEGIN_MONTH)
         endif
         if(allocated(BEGIN_TO)) then
            deallocate(BEGIN_TO)
         endif
         if(allocated(END_DAY)) then
            deallocate(END_DAY)
         endif
         if(allocated(END_MONTH)) then
            deallocate(END_MONTH)
         endif
         if(allocated(UNIT_NAMEs_loc)) then
            deallocate(UNIT_NAMEs_loc)
         endif
         if(allocated(END_YEAR)) then
            deallocate(END_YEAR)
         endif
         
         if(allocated(BEGIN_YEAR)) then
            deallocate(BEGIN_YEAR)
         endif
         
         if(allocated(END_TO)) then
            deallocate(END_TO)
         endif
         if(allocated(BEGIN_DAY_IN_MONTH)) then
            deallocate(BEGIN_DAY_IN_MONTH)
         endif
         if(allocated(END_DAY_IN_MONTH)) then
            deallocate(END_DAY_IN_MONTH)
         endif
         if(allocated(RECORD_ACTIVE_IN_MONTH)) then
            deallocate(RECORD_ACTIVE_IN_MONTH)
         endif
         if(allocated(DOWNTIME_TYPE)) then
            deallocate(DOWNTIME_TYPE)
         endif
         if(allocated(DERATE_UNITS)) then
            deallocate(DERATE_UNITS)
         endif
         if(allocated(OUTAGE_PRODUCT_TYPE)) then
            deallocate(OUTAGE_PRODUCT_TYPE)
         endif
         if(allocated(DERATE_QUANTITY))then
            deallocate(DERATE_QUANTITY)
         endif
         

         ALLOCATE(UNIT_NAMEs_loc(MAX_OUTAGES),
     +            UNIT_ID(MAX_OUTAGES),
     +            BEGIN_DAY(MAX_OUTAGES,MAX_START_STOP),
     +            BEGIN_MONTH(MAX_OUTAGES,MAX_START_STOP),
     +            BEGIN_TO(MAX_OUTAGES,MAX_START_STOP),
     +            END_DAY(MAX_OUTAGES,MAX_START_STOP),
     +            END_MONTH(MAX_OUTAGES,MAX_START_STOP),
     +            END_TO(MAX_OUTAGES,MAX_START_STOP),
     +            END_YEAR(MAX_OUTAGES,MAX_START_STOP),
     +            BEGIN_YEAR(MAX_OUTAGES,MAX_START_STOP),
     +            BEGIN_DAY_IN_MONTH(MAX_OUTAGES,MAX_START_STOP),
     +            END_DAY_IN_MONTH(MAX_OUTAGES,MAX_START_STOP),
     +            RECORD_ACTIVE_IN_MONTH(MAX_OUTAGES),
     +            DOWNTIME_TYPE(MAX_OUTAGES),
     +            DERATE_UNITS(MAX_OUTAGES),
     +            OUTAGE_PRODUCT_TYPE(MAX_OUTAGES),
     +            DERATE_QUANTITY(MAX_OUTAGES))


! TAKEN-OUT 8/24/00. GAT.
!         VALUES_2_ZERO = INT(750*MAX_TRANS_GROUPS)


C
! ARRAY MATH USED 11/20/06 DR.G
C
         BEGIN_TO = 0
         END_TO = 0
         BEGIN_DAY = 0
         BEGIN_MONTH = 0
         BEGIN_YEAR = 0
         END_DAY = 0
         END_MONTH = 0
         END_YEAR = 0
         BEGIN_DAY_IN_MONTH = 0
         END_DAY_IN_MONTH = 0
!
!         UNIT_ID = 0D0
!
         ACU_POWER_ID_INDEX = GET_ACU_POWER_ID_INDEX()
C
!         COUNT_ID = 0
C
         DO IREC = 1, MAX_OUTAGES   !loop for multiple tables
            READ(UNIT_NUM,REC=IREC,IOSTAT=IOS)
     +            DELETE,
     +            UNIT_NAMEs_loc(IREC),
     +            UNIT_ID(IREC),
     +            OUTAGE_RECORD_ACTIVE,
     +               OUTAGE_START_DATE,
     +            OUTAGE_STOP_DATE,
     +            DOWNTIME_TYPE(IREC),
     +            DERATE_UNITS(IREC),
     +            OUTAGE_PRODUCT_TYPE(IREC),
     +            DERATE_QUANTITY(IREC)

            IF(OUTAGE_RECORD_ACTIVE == 'F') CYCLE
            IF(UNIT_ID(IREC) == 9999) THEN
               WRITE(4,*)
     +               'MORE THAN 10000 TRANSACT OUTAGES IN SIMULATION'
               er_message='Stop requested from TF_OBJT2 SIID295'
               call end_program(er_message)
            ENDIF
!            COUNT_ID(UNIT_ID(IREC)) = COUNT_ID(UNIT_ID(IREC)) + 1
! E.G. BEGIN_DATE = 062398
!            RECORD_ITEM = COUNT_ID(UNIT_ID(IREC))
!            ID_TO_RECORD(UNIT_ID(IREC),RECORD_ITEM) = IREC
            DO J = 1, MAX_START_STOP
               IF(OUTAGE_START_DATE(J) == 0 .OR.
     +                                  OUTAGE_STOP_DATE(J) == 0) CYCLE
               IF(OUTAGE_START_DATE(J) > 123199) THEN
                  BEGIN_MONTH(IREC,J)=INT(OUTAGE_START_DATE(J)/1000000)
                  BEGIN_DAY(IREC,J) = INT(OUTAGE_START_DATE(J)/10000) -
     +                                          BEGIN_MONTH(IREC,J)*100
                  BEGIN_YEAR(IREC,J) = INT(OUTAGE_START_DATE(J) -
     +                                  BEGIN_MONTH(IREC,J)*1000000 -
     +                                  BEGIN_DAY(IREC,J)*10000) - 2000
               ELSE
                  BEGIN_MONTH(IREC,J) = INT(OUTAGE_START_DATE(J)/10000)
                  BEGIN_DAY(IREC,J) = INT(OUTAGE_START_DATE(J)/100) -
     +                                          BEGIN_MONTH(IREC,J)*100
                  BEGIN_YEAR(IREC,J) = INT(OUTAGE_START_DATE(J) -
     +                                 BEGIN_MONTH(IREC,J)*10000 -
     +                                           BEGIN_DAY(IREC,J)*100)
               ENDIF
! TODO:  Fix years so that they're handled consistently.
! ON 12/27/06 THIS CODE WAS MODIFIED SO THAT TWO-DIGIT YEAR VALUES
! COVER THE YEARS 1999 TO 2098.  TO SCHEDULE MAINTENANCE IN 2099 AND 
! BEYOND
! FOUR-DIGIT YEAR VALUES MUST BE USED.  DR.G
C
               IF(BEGIN_YEAR(IREC,J) == 99) THEN
                  BEGIN_TO(IREC,J) = BEGIN_YEAR(IREC,J)*100 +
     +                                              BEGIN_MONTH(IREC,J)
               ELSE
                  BEGIN_TO(IREC,J)= BEGIN_YEAR(IREC,J)*100 + 10000 +
     +                                              BEGIN_MONTH(IREC,J)
               ENDIF
!
               IF(OUTAGE_STOP_DATE(J) > 123199) THEN
                  END_MONTH(IREC,J)=INT(OUTAGE_STOP_DATE(J)/1000000)
                  END_DAY(IREC,J) = INT(OUTAGE_STOP_DATE(J)/10000) -
     +                                            END_MONTH(IREC,J)*100
                  END_YEAR(IREC,J) = INT(OUTAGE_STOP_DATE(J) -
     +                                  END_MONTH(IREC,J)*1000000 -
     +                                    END_DAY(IREC,J)*10000) - 2000
               ELSE
                  END_MONTH(IREC,J) = INT(OUTAGE_STOP_DATE(J)/10000)
                  END_DAY(IREC,J) = INT(OUTAGE_STOP_DATE(J)/100) -
     +                                            END_MONTH(IREC,J)*100
                  END_YEAR(IREC,J) = INT(OUTAGE_STOP_DATE(J) -
     +                                 END_MONTH(IREC,J)*10000 -
     +                                             END_DAY(IREC,J)*100)
               ENDIF
               IF(END_YEAR(IREC,J) == 99) THEN
                  END_TO(IREC,J) = END_YEAR(IREC,J)*100 +
     +                                                END_MONTH(IREC,J)
               ELSE
                  END_TO(IREC,J) = END_YEAR(IREC,J)*100 + 10000 +
     +                                                END_MONTH(IREC,J)
               ENDIF
!
! 10/11/04. FOR ENTERGY. ! TESTED AND BACK IN ON 10/15/04.
!
               BEGIN_DAY(IREC,J) = MIN(BEGIN_DAY(IREC,J),
     +                         DAYS_IN_EACH_MONTH(BEGIN_MONTH(IREC,J)))
               END_DAY(IREC,J) = MIN(END_DAY(IREC,J),
     +                           DAYS_IN_EACH_MONTH(END_MONTH(IREC,J)))
!
            ENDDO  ! END OF OUTAGE PAIRS
!
!            IF(OUTAGE_RECORD_ACTIVE /= 'T') CYCLE
!
         ENDDO ! END OF OUTAGE RECORDS
!
         CALL CLOSE_OUTAGE_FILE
!
         SAVE_OUTAGE_FILE_EXISTS = .TRUE.
         READ_TRANS_OUTAGE_DATA = .TRUE.
!
      RETURN

C
!
      ENTRY ANNUAL_ACTIVE_TRANS_OUTAGES
!
!

! INCLUDES CREATION OF AN EVENTS CALENDAR
!

C
         ANNUAL_ACTIVE_TRANS_OUTAGES = .FALSE.
!
         IF(.NOT. SAVE_OUTAGE_FILE_EXISTS .OR.
     +                                 .NOT. YES_RUN_TRANSACT()) RETURN
!
!

!
! 12/17/02. ALWAYS DO UNIT OUTAGES.
!
         AUTO_MAINT_AS_HOURLY = .TRUE.

         IF(.NOT. AUTO_MAINT_AS_HOURLY) RETURN
!
         IF(ALLOCATED(BEGIN_MONTH_DAY))
     +                        DEALLOCATE(BEGIN_MONTH_DAY,END_MONTH_DAY,
     +                                    ANNUAL_UNIT_NUM)
         ALLOCATE( BEGIN_MONTH_DAY(MAX_EVENTS_IN_MONTH),
     +             END_MONTH_DAY(MAX_EVENTS_IN_MONTH),
     +             ANNUAL_UNIT_NUM(MAX_EVENTS_IN_MONTH))
         BEGIN_MONTH_DAY = 0
         END_MONTH_DAY = 0
         ANNUAL_UNIT_NUM = 0
!
         TEST_YEAR = globecom_YEAR + get_BASE_YEAR()
         TEST_DATE_1 = 100*(TEST_YEAR - 1900) + 1
         TEST_DATE_2 = 100*(TEST_YEAR - 1900) + 12
!
         ANNUAL_ACTIVE_OUTAGES = 0


!
         DO IREC = 1, SAVE_MAX_OUTAGES
!
            IF(ACU_POWER_ID_INDEX) THEN ! ONLY FINDS THE FIRST OCCURENCE
               LOCAL_UNIT_ID_I8 = UNIT_ID(IREC)
               UNIT_NUMBER = GET_I8_ID_TO_UNIT(LOCAL_UNIT_ID_I8)
               NUMBER_OF_UNITS_FOR_ID = 1
            ELSE
               LOCAL_UNIT_ID = UNIT_ID(IREC)
               UNIT_NUMBER = GET_RESOURCE_ID_TO_UNIT(LOCAL_UNIT_ID,
     + NUMBER_OF_UNITS_FOR_ID) ! FIRST OCCURRENCE
            ENDIF
!
            IF(UNIT_NUMBER <= 0) CYCLE
!
!
            
       IF(.NOT. UNIT_ON_LINE_IN_YEAR(UNIT_NUMBER,globecom_year)) then
        CYCLE
       end if
!
!
! ONLY ACTIVE FOR 7x24 AND FULL OUTAGES
!
            IF(trim(OUTAGE_PRODUCT_TYPE(IREC)) /= '7x24' .OR.
     +                            DOWNTIME_TYPE(IREC) /= 'O') then
                CYCLE
            end if
!


            TG = GET_TRANS_GROUP_POSITION( 
     +           get_TRANSACTION_GROUP(unit_number))
!
            DO J = 1, MAX_START_STOP
               IF(END_TO(IREC,J) == 0) EXIT ! GO ON TO NEXT RECORD
               IF(TEST_DATE_1 > END_TO(IREC,J) .OR.
     + TEST_DATE_2 < BEGIN_TO(IREC,J)) CYCLE ! GO ON TO NEXT OUTAGE
               ANNUAL_ACTIVE_OUTAGES = ANNUAL_ACTIVE_OUTAGES + 1
               IF(TEST_DATE_1 > BEGIN_TO(IREC,J)) THEN
                  BEGIN_IN_DAY = 1
                  BEGIN_IN_MONTH = 1
               ELSE
                  BEGIN_IN_DAY = BEGIN_DAY(IREC,J)
                  BEGIN_IN_MONTH = BEGIN_MONTH(IREC,J)
               ENDIF
               IF(TEST_DATE_2 < END_TO(IREC,J)) THEN
                  END_IN_DAY = 31 ! DAYS IN DECEMBER
                  END_IN_MONTH = 12
               ELSE
                  END_IN_DAY = END_DAY(IREC,J)
                  END_IN_MONTH = END_MONTH(IREC,J)
               ENDIF
!
!
               BEGIN_MONTH_DAY(ANNUAL_ACTIVE_OUTAGES) =
     +                                BEGIN_IN_MONTH*100 + BEGIN_IN_DAY

               END_MONTH_DAY(ANNUAL_ACTIVE_OUTAGES) =
     +                                    END_IN_MONTH*100 + END_IN_DAY

               ANNUAL_UNIT_NUM(ANNUAL_ACTIVE_OUTAGES) = UNIT_NUMBER
!
               ANNUAL_ACTIVE_TRANS_OUTAGES = .TRUE.

            ENDDO ! START_STOP
         ENDDO ! IREC
!
!
      RETURN

!
      ENTRY GET_ANNUAL_OUTAGE_NUM()
!

         IF(YES_RUN_TRANSACT()) THEN
            GET_ANNUAL_OUTAGE_NUM = ANNUAL_ACTIVE_OUTAGES
         ELSE
            GET_ANNUAL_OUTAGE_NUM = 0
         ENDIF
      RETURN

!
      ENTRY GET_ANNUAL_OUTAGE_DATES(R_OUTAGE,
     +                              R_BEGIN_MONTH_DAY,
     +                              R_END_MONTH_DAY,
     +                              R_UNIT_NUM)



         R_BEGIN_MONTH_DAY = BEGIN_MONTH_DAY(R_OUTAGE)


         R_END_MONTH_DAY = END_MONTH_DAY(R_OUTAGE)

         R_UNIT_NUM = ANNUAL_UNIT_NUM(R_OUTAGE)

         GAOD_result = ANNUAL_UNIT_NUM(R_OUTAGE)
         GET_ANNUAL_OUTAGE_DATES=GAOD_result

      RETURN

C
!
      ENTRY MONTHLY_ACTIVE_TRANS_OUTAGES(R_MONTH,R_NUNITS)
!
!
!

C
         MONTHLY_ACTIVE_TRANS_OUTAGES = .FALSE.
!
!
! GRAB AUTO-SCHEDULED (FORCED AND) PLANNED OUTAGES FROM CLM
! MOVED: 02/08/02.
!
!

! 11/21/02. HARD-WIRED OUT
!
         AUTO_MAINT_AS_HOURLY = YES_DETAILED_MAINTENANCE()
!         AUTO_MAINT_AS_HOURLY = .TRUE.
!         PRESCED_ANN_OUT_ALWAYS = .TRUE.
         AUTO_FOR_AS_HOURLY = YES_DETAILED_FOR()
         LOCAL_MAINT_NUNITS = 0
         ACTIVE_MAINT_NUNITS = 0
         ACTIVE_FOR_NUNITS = 0
         LOCAL_FOR_NUNITS = 0
!         IF(YEAR == 8 .AND. R_MONTH == 11) THEN
!            R_MONTH = R_MONTH
!         ENDIF
!

! THIS CALL INCLUDES BOTH AUTO SCHEDULED OUTAGES AND TRANSACT SCHEDULED
! OUTAGES
! I NEED ALAN TO JUST PASS ME BACK THE START-STOP DATES FROM THE 
! AUTOMATIC SCHEDULING PROGRAM

! AND ONLY IF DETAILED MAINTENANCE IS TURNED-ON.
!
! TOOK OUT JANUARY 13TH 2003. I STILL NEED OUTAGES IF
!
         IF(AUTO_MAINT_AS_HOURLY) THEN
            CALL
     +         COUNT_EVENTS_OF_SCHED_OUTAGE(LOCAL_MAINT_NUNITS,R_MONTH)
         ENDIF
!
         IF(AUTO_FOR_AS_HOURLY) THEN
           CALL COUNT_EVENTS_OF_FORCED_OUTAGE(LOCAL_FOR_NUNITS,R_MONTH)

         ENDIF
!
         SAVE_TOTAL_OUTAGES = SAVE_MAX_OUTAGES + LOCAL_MAINT_NUNITS +
     +                                                 LOCAL_FOR_NUNITS

         IF(SAVE_TOTAL_OUTAGES <= 0 .OR.
     +                                 .NOT. YES_RUN_TRANSACT()) RETURN
!
         IF(ALLOCATED(MONTH_EVENT_COUNTER))
     +                                  DEALLOCATE(MONTH_EVENT_COUNTER)
         ALLOCATE( MONTH_EVENT_COUNTER(MAX_TRANS_GROUPS) )

         MONTH_EVENT_COUNTER = 0
!
         IF(ALLOCATED(EVENTS_RECORD_INDEX))
     +           DEALLOCATE(  EVENTS_RECORD_INDEX,
     +                        EVENTS_START_STOP_INDEX,
     +                        EVENTS_IN_HOUR,
     +                        EVENTS_LINK_INDEX,
     +                        FIRST_EVENT_IN_HOUR)
         ALLOCATE(EVENTS_RECORD_INDEX(
     +                           MAX_EVENTS_IN_MONTH,MAX_TRANS_GROUPS),
     +           EVENTS_START_STOP_INDEX(
     +                           MAX_EVENTS_IN_MONTH,MAX_TRANS_GROUPS),
     +           EVENTS_IN_HOUR(MAX_EVENTS_IN_MONTH,MAX_TRANS_GROUPS),
     +           EVENTS_LINK_INDEX(
     +                           MAX_EVENTS_IN_MONTH,MAX_TRANS_GROUPS),
     +           FIRST_EVENT_IN_HOUR(MAX_EVENTS_IN_MONTH,
     +                          MAX_TRANS_GROUPS)) ! NEED TO SIZE USING 
         VALUES_2_ZERO = INT(MAX_EVENTS_IN_MONTH)*
     +                                            INT(MAX_TRANS_GROUPS)
         EVENTS_RECORD_INDEX = 0
         EVENTS_LINK_INDEX = 0
         EVENTS_START_STOP_INDEX = 0
         EVENTS_IN_HOUR = 999
!
         EVENTS_IN_DAY = 0
! RE-INITIALIZED. 8/24/00. GAT.
         FIRST_EVENT_IN_HOUR = 0
!
         MAX_FO_PER_MONTH = GET_MAX_FO_PER_MONTH()
!
         IF(ALLOCATED(BEGIN_IN_HOUR_ALAN))
     +         DEALLOCATE(BEGIN_IN_HOUR_ALAN,END_IN_HOUR_ALAN)
         ALLOCATE(BEGIN_IN_HOUR_ALAN(MAX_FO_PER_MONTH+1),
     +            END_IN_HOUR_ALAN(MAX_FO_PER_MONTH+1))
!
         EVENTS_IN_MONTH = 0
!
!         TG = 1 ! FOR NOW.
!
         TEST_YEAR = globecom_YEAR + get_BASE_YEAR()
         TEST_DATE = 100*(TEST_YEAR - 1900) + R_MONTH
         NUM_ACTIVE_RECORDS = 0
         DAYS_IN_MONTH = HOURS_IN(R_MONTH)/24
!
         MAX_HOURS = HOURS_IN(R_MONTH) + 1
!
         DO IREC = 1, SAVE_MAX_OUTAGES

            IF(ACU_POWER_ID_INDEX) THEN ! ONLY FINDS THE FIRST OCCURENCE
               LOCAL_UNIT_ID_I8 = UNIT_ID(IREC)
               UNIT_NUMBER = GET_I8_ID_TO_UNIT(LOCAL_UNIT_ID_I8)
               NUMBER_OF_UNITS_FOR_ID = 1
            ELSE
               LOCAL_UNIT_ID = UNIT_ID(IREC)
               UNIT_NUMBER = GET_RESOURCE_ID_TO_UNIT(LOCAL_UNIT_ID,
     + NUMBER_OF_UNITS_FOR_ID) ! FIRST OCCURRENCE
            ENDIF
!
            IF(UNIT_NUMBER <= 0 .OR. UNIT_NUMBER > R_NUNITS) CYCLE
!
            
            gtgi_result=get_TRANSACTION_GROUP(unit_number)
            TG =
     +     GET_TRANS_GROUP_POSITION(gtgi_result)
!
            MONTHLY_ACTIVE_OUTAGES = 0
            DO J = 1, MAX_START_STOP
               IF(END_TO(IREC,J) == 0) EXIT ! GO ON TO NEXT RECORD
               IF(TEST_DATE > END_TO(IREC,J) .OR.
     + TEST_DATE < BEGIN_TO(IREC,J)) CYCLE ! GO ON TO NEXT OUTAGE

               MONTHLY_ACTIVE_OUTAGES =
     +                                   MONTHLY_ACTIVE_OUTAGES + 1
               IF(MONTHLY_ACTIVE_OUTAGES == 1) THEN
                  NUM_ACTIVE_RECORDS = NUM_ACTIVE_RECORDS + 1
                  RECORD_ACTIVE_IN_MONTH(NUM_ACTIVE_RECORDS) = IREC
               ENDIF
               IF(TEST_DATE > BEGIN_TO(IREC,J)) THEN
                  BEGIN_DAY_IN_MONTH(IREC,J) = 1
               ELSE
                  BEGIN_DAY_IN_MONTH(IREC,J) = BEGIN_DAY(IREC,J)
               ENDIF
               IF(TEST_DATE < END_TO(IREC,J)) THEN
                  END_DAY_IN_MONTH(IREC,J) = DAYS_IN_MONTH
               ELSE
                  END_DAY_IN_MONTH(IREC,J) = END_DAY(IREC,J)
               ENDIF
!
               BEGIN_IN_DAY = BEGIN_DAY_IN_MONTH(IREC,J)
               END_IN_DAY = END_DAY_IN_MONTH(IREC,J)
! MOVED
               MONTHLY_ACTIVE_TRANS_OUTAGES = .TRUE.
               SAVE_MONTHLY_TRANS_OUTAGES = .TRUE.
!
! OUTAGES (AND OTHER MONTHLY CHARACTERISTICS, WILL BE BASED UPON AN
! EVENTS CALENDAR.
!
! NOTE:  THE EVENTS CALENDAR CAN BE VERY GENERAL: IT CAN HAVE AN
!        UNLIMITED NUMBER OF CHARACTERISTICS IN THE MONTH_EVENT_COUNTER
!        WHICH WILL BE HANDLED WHEN AND ONLY WHEN AN EVENT IS 
! REGISTERED.
!
!
               IF(trim(OUTAGE_PRODUCT_TYPE(IREC)) == '7x24') THEN
                  START_ENCOUNTERED = .TRUE.
                  STOP_ENCOUNTERED = .TRUE.
!
                  BEGIN_IN_HOUR = (BEGIN_IN_DAY-1)*24 + 1
                  END_IN_HOUR = END_IN_DAY*24 + 1 ! CHANGE STATE IS IN 
                   !NEXT HOUR
! START
                  MONTH_EVENT_COUNTER(TG) = MONTH_EVENT_COUNTER(TG) + 1
                  MEC = MONTH_EVENT_COUNTER(TG)
!
                  EVENTS_IN_HOUR(MEC,TG) = BEGIN_IN_HOUR
                  EVENTS_RECORD_INDEX(MEC,TG) = IREC
                  EVENTS_START_STOP_INDEX(MEC,TG) = 2 ! START OUTAGE
                  EVENTS_LINK_INDEX(MEC,TG) = MEC

                  MONTH_EVENT_COUNTER(TG) = MONTH_EVENT_COUNTER(TG) + 1
                  MEC = MONTH_EVENT_COUNTER(TG)
!
                  EVENTS_IN_HOUR(MEC,TG) = END_IN_HOUR
                  EVENTS_RECORD_INDEX(MEC,TG) = IREC
                  EVENTS_START_STOP_INDEX(MEC,TG) = 3 ! STOP OUTAGE
                  EVENTS_LINK_INDEX(MEC,TG) = MEC

               ELSE
                  DO DAY = BEGIN_IN_DAY, END_IN_DAY
! TEST DAY FOR WEEKEND OR WEEKDAY
                     START_ENCOUNTERED = .FALSE.
                     STOP_ENCOUNTERED = .FALSE.
!
                     BEGIN_IN_HOUR = 0
                     END_IN_HOUR = 0

                    CALENDAR_DAY_OF_WEEK=GET_DAY_OF_WEEK_4(R_MONTH,DAY)

                     IF( ( trim(OUTAGE_PRODUCT_TYPE(IREC)) ==
     +                                                     '5x16' .AND.
     +                       CALENDAR_DAY_OF_WEEK < 6 )  .OR.
     +                   ( trim(OUTAGE_PRODUCT_TYPE(IREC)) ==
     +                                                     '6x16' .AND.
     +                                 CALENDAR_DAY_OF_WEEK < 7) ) THEN
!
                           BEGIN_IN_HOUR = (DAY-1)*24 + 7
                           START_ENCOUNTERED = .TRUE.
                           END_IN_HOUR =   (DAY-1)*24 + 23
                           STOP_ENCOUNTERED = .TRUE.
                     ELSE ! COMPLETELY RE-WROTE
                        IF(trim(OUTAGE_PRODUCT_TYPE(IREC)) ==
     +                                                     '2x24') THEN
                           IF(CALENDAR_DAY_OF_WEEK == 6 .OR.
     +                           (CALENDAR_DAY_OF_WEEK == 7 .AND.
     +                                      DAY == BEGIN_IN_DAY) ) THEN
                              BEGIN_IN_HOUR = (DAY-1)*24 + 1
                              START_ENCOUNTERED = .TRUE.
                           ENDIF
                           IF(CALENDAR_DAY_OF_WEEK == 1 .OR.
     +                         (DAY > 1 .AND.
     +                           GET_DAY_OF_WEEK_4(R_MONTH,DAY-1) ==
     +                                                        7 .AND.
     +                           GET_DAY_OF_WEEK_4(R_MONTH,DAY) >
     +                                                       5 ) ) THEN
                              END_IN_HOUR =
     +                           MIN(int((DAY-1)*24 + 1,2),
     +                                         HOURS_IN_MONTH(R_MONTH))
                              STOP_ENCOUNTERED = .TRUE.
                           ENDIF
                        ELSEIF( trim(OUTAGE_PRODUCT_TYPE(IREC)) ==
     +                                                     'Wrap') THEN
                           IF(DAY == BEGIN_IN_DAY) THEN
! NEED TO REGISTER FIRST OUTAGE STATE. POSSIBLY MORE THAN TWO PER DAY
! NOTICE ADDED TO EVENT CALENDAR LOCALLY.
!
                              IF(CALENDAR_DAY_OF_WEEK > 5) THEN
                                 BEGIN_IN_HOUR = (DAY-1)*24 + 1
!
                                 MONTH_EVENT_COUNTER(TG) =
     +                                      MONTH_EVENT_COUNTER(TG) + 1
                                 MEC = MONTH_EVENT_COUNTER(TG)
!
                                 EVENTS_IN_HOUR(MEC,TG) = BEGIN_IN_HOUR
                                 EVENTS_RECORD_INDEX(MEC,TG) = IREC
        EVENTS_START_STOP_INDEX(MEC,TG) = 2 ! START OUTAGE
                                 EVENTS_LINK_INDEX(MEC,TG) =  MEC
!
                              ELSEIF(CALENDAR_DAY_OF_WEEK >= 1 .AND.
     +                                  CALENDAR_DAY_OF_WEEK <= 5) THEN
!
                                 BEGIN_IN_HOUR = (DAY-1)*24 + 1
!
                                 MONTH_EVENT_COUNTER(TG) =
     +                                      MONTH_EVENT_COUNTER(TG) + 1
                                 MEC = MONTH_EVENT_COUNTER(TG)
!
                                 EVENTS_IN_HOUR(MEC,TG) = BEGIN_IN_HOUR
                                 EVENTS_RECORD_INDEX(MEC,TG) = IREC
                    EVENTS_START_STOP_INDEX(MEC,TG) = 2 ! START OUTAGE
                                 EVENTS_LINK_INDEX(MEC,TG) =  MEC
!
                                 END_IN_HOUR = (DAY-1)*24 + 7
!
                                 MONTH_EVENT_COUNTER(TG) =
     +                                      MONTH_EVENT_COUNTER(TG) + 1
                                 MEC = MONTH_EVENT_COUNTER(TG)
!
                                 EVENTS_IN_HOUR(MEC,TG) = END_IN_HOUR
                                 EVENTS_RECORD_INDEX(MEC,TG) = IREC
                       EVENTS_START_STOP_INDEX(MEC,TG) = 3 ! STOP OUTAGE
                                 EVENTS_LINK_INDEX(MEC,TG) =  MEC
!
                                 BEGIN_IN_HOUR = (DAY-1)*24 + 23
                                 START_ENCOUNTERED = .TRUE.
!
                              ENDIF
                           ELSE
                              IF(CALENDAR_DAY_OF_WEEK > 0 .AND.
     +                                   CALENDAR_DAY_OF_WEEK < 6) THEN
                                 START_ENCOUNTERED = .TRUE.
                                 BEGIN_IN_HOUR = (DAY-1)*24 + 23
                                 STOP_ENCOUNTERED = .TRUE.
                                 END_IN_HOUR = (DAY-1)*24 + 7
                              ENDIF
                           ENDIF
                        ELSEIF( trim(OUTAGE_PRODUCT_TYPE(IREC)) ==
     +                                             'Western Wrap') THEN
                           IF(DAY == BEGIN_IN_DAY) THEN
! NEED TO REGISTER FIRST OUTAGE STATE. POSSIBLY MORE THAN TWO PER DAY
! NOTICE ADDED TO EVENT CALENDAR LOCALLY.
!
                              IF(CALENDAR_DAY_OF_WEEK > 6) THEN
                                 BEGIN_IN_HOUR = (DAY-1)*24 + 1
!
                                 MONTH_EVENT_COUNTER(TG) =
     +                                      MONTH_EVENT_COUNTER(TG) + 1
                                 MEC = MONTH_EVENT_COUNTER(TG)
!
                                 EVENTS_IN_HOUR(MEC,TG) = BEGIN_IN_HOUR
                                 EVENTS_RECORD_INDEX(MEC,TG) = IREC
                      EVENTS_START_STOP_INDEX(MEC,TG) = 2 ! START OUTAGE
                                 EVENTS_LINK_INDEX(MEC,TG) =  MEC
!
                              ELSEIF(CALENDAR_DAY_OF_WEEK >= 1 .AND.
     +                                  CALENDAR_DAY_OF_WEEK <= 6) THEN
!
                                 BEGIN_IN_HOUR = (DAY-1)*24 + 1
!
                                 MONTH_EVENT_COUNTER(TG) =
     +                                      MONTH_EVENT_COUNTER(TG) + 1
                                 MEC = MONTH_EVENT_COUNTER(TG)
!
                                 EVENTS_IN_HOUR(MEC,TG) = BEGIN_IN_HOUR
                                 EVENTS_RECORD_INDEX(MEC,TG) = IREC
                      EVENTS_START_STOP_INDEX(MEC,TG) = 2 ! START OUTAGE
                                 EVENTS_LINK_INDEX(MEC,TG) =  MEC
!
                                 END_IN_HOUR = (DAY-1)*24 + 7
!
                                 MONTH_EVENT_COUNTER(TG) =
     +                                      MONTH_EVENT_COUNTER(TG) + 1
                                 MEC = MONTH_EVENT_COUNTER(TG)
!
                                 EVENTS_IN_HOUR(MEC,TG) = END_IN_HOUR
                                 EVENTS_RECORD_INDEX(MEC,TG) = IREC
                       EVENTS_START_STOP_INDEX(MEC,TG) = 3 ! STOP OUTAGE
                                 EVENTS_LINK_INDEX(MEC,TG) =  MEC
!
                                 BEGIN_IN_HOUR = (DAY-1)*24 + 23
                                 START_ENCOUNTERED = .TRUE.
!
                              ENDIF
                           ELSE
                              IF(CALENDAR_DAY_OF_WEEK > 0 .AND.
     +                                   CALENDAR_DAY_OF_WEEK < 7) THEN
                                 START_ENCOUNTERED = .TRUE.
                                 BEGIN_IN_HOUR = (DAY-1)*24 + 23
                                 STOP_ENCOUNTERED = .TRUE.
                                 END_IN_HOUR = (DAY-1)*24 + 7
                              ENDIF
                           ENDIF
                        ELSEIF( trim(OUTAGE_PRODUCT_TYPE(IREC)) ==
     +                                                      '5x8') THEN
                           IF(DAY == BEGIN_IN_DAY) THEN
! NEED TO REGISTER FIRST OUTAGE STATE. POSSIBLY MORE THAN TWO PER DAY
! NOTICE ADDED TO EVENT CALENDAR LOCALLY.
!
                              IF(CALENDAR_DAY_OF_WEEK >= 1 .AND.
     +                                  CALENDAR_DAY_OF_WEEK <= 5) THEN
!
                                 BEGIN_IN_HOUR = (DAY-1)*24 + 1
!
                                 MONTH_EVENT_COUNTER(TG) =
     +                                      MONTH_EVENT_COUNTER(TG) + 1
                                 MEC = MONTH_EVENT_COUNTER(TG)
!
                                 EVENTS_IN_HOUR(MEC,TG) = BEGIN_IN_HOUR
                                 EVENTS_RECORD_INDEX(MEC,TG) = IREC
                      EVENTS_START_STOP_INDEX(MEC,TG) = 2 ! START OUTAGE
                                 EVENTS_LINK_INDEX(MEC,TG) =  MEC
!
                                 END_IN_HOUR = (DAY-1)*24 + 7
!
                                 MONTH_EVENT_COUNTER(TG) =
     +                                      MONTH_EVENT_COUNTER(TG) + 1
                                 MEC = MONTH_EVENT_COUNTER(TG)
!
                                 EVENTS_IN_HOUR(MEC,TG) = END_IN_HOUR
                                 EVENTS_RECORD_INDEX(MEC,TG) = IREC
                       EVENTS_START_STOP_INDEX(MEC,TG) = 3 ! STOP OUTAGE
                                 EVENTS_LINK_INDEX(MEC,TG) =  MEC
!
                                 BEGIN_IN_HOUR = (DAY-1)*24 + 23
                                 START_ENCOUNTERED = .TRUE.
!
                              ENDIF
                           ELSE
                              IF( CALENDAR_DAY_OF_WEEK == 1 .OR.
     +                              (DAY > 1 .AND.
     +                              GET_DAY_OF_WEEK_4(R_MONTH,DAY-1) >
     +                                                       6 ) ) THEN
                                 BEGIN_IN_HOUR = (DAY-1)*24 + 1
!
                                 MONTH_EVENT_COUNTER(TG) =
     +                                      MONTH_EVENT_COUNTER(TG) + 1
                                 MEC = MONTH_EVENT_COUNTER(TG)
!
                                 EVENTS_IN_HOUR(MEC,TG) = BEGIN_IN_HOUR
                                 EVENTS_RECORD_INDEX(MEC,TG) = IREC
                      EVENTS_START_STOP_INDEX(MEC,TG) = 2 ! START OUTAGE
                                 EVENTS_LINK_INDEX(MEC,TG) =  MEC
!
                              ENDIF
                              IF(CALENDAR_DAY_OF_WEEK > 0 .AND.
     +                                   CALENDAR_DAY_OF_WEEK < 6) THEN
                                 START_ENCOUNTERED = .TRUE.
                                 BEGIN_IN_HOUR = (DAY-1)*24 + 23
                                 STOP_ENCOUNTERED = .TRUE.
                                 END_IN_HOUR = (DAY-1)*24 + 7
                              ELSEIF(CALENDAR_DAY_OF_WEEK == 6) THEN
                                 END_IN_HOUR = (DAY-1)*24 + 1
!
                                 MONTH_EVENT_COUNTER(TG) =
     +                                      MONTH_EVENT_COUNTER(TG) + 1
                                 MEC = MONTH_EVENT_COUNTER(TG)
!
                                 EVENTS_IN_HOUR(MEC,TG) = END_IN_HOUR
                                 EVENTS_RECORD_INDEX(MEC,TG) = IREC
                       EVENTS_START_STOP_INDEX(MEC,TG) = 3 ! STOP OUTAGE
                                 EVENTS_LINK_INDEX(MEC,TG) =  MEC
                              ENDIF
                           ENDIF
                        ENDIF ! 2X24, WRAP, 5X8
!
                     ENDIF
!
                    IF(BEGIN_IN_HOUR == 0 .AND. END_IN_HOUR == 0) then
                        CYCLE ! PRODUCT NOT ACTIVE THIS DAY
                    endif

                     IF(START_ENCOUNTERED) THEN
                        MONTH_EVENT_COUNTER(TG) =
     +                                      MONTH_EVENT_COUNTER(TG) + 1
                        MEC = MONTH_EVENT_COUNTER(TG)
!
                        EVENTS_IN_HOUR(MEC,TG) = BEGIN_IN_HOUR
                        EVENTS_RECORD_INDEX(MEC,TG) = IREC
                        ! TODO:  Create start_outage routine.
                EVENTS_START_STOP_INDEX(MEC,TG) = 2 ! START OUTAGE
                        EVENTS_LINK_INDEX(MEC,TG) =  MEC
                     ENDIF

                     IF(STOP_ENCOUNTERED) THEN
                        MONTH_EVENT_COUNTER(TG) =
     +                                      MONTH_EVENT_COUNTER(TG) + 1
                        MEC = MONTH_EVENT_COUNTER(TG)
!
                        EVENTS_IN_HOUR(MEC,TG) = END_IN_HOUR
                        EVENTS_RECORD_INDEX(MEC,TG) = IREC
                        ! TODO: Create stop_outage routine.
                      EVENTS_START_STOP_INDEX(MEC,TG) = 3 ! STOP OUTAGE
                        EVENTS_LINK_INDEX(MEC,TG) =  MEC
                     ENDIF

                  ENDDO ! BEGIN/END DAY
               ENDIF ! PRODUCT TYPE
            ENDDO ! START_STOP
         ENDDO ! IREC
!
! 10/28/03.
!
! ALAN NEED TO PASS ME TOTAL FOR'S PER MONTH AFTER SCHEDULE
!
         TOTAL_INDEXED_OUTAGES = LOCAL_MAINT_NUNITS +
     +                                LOCAL_FOR_NUNITS*MAX_FO_PER_MONTH
!
         IF( TOTAL_INDEXED_OUTAGES > 0) THEN
            IF(ALLOCATED(CLM_MAINT_INDEX)) DEALLOCATE(CLM_MAINT_INDEX)
            ALLOCATE(CLM_MAINT_INDEX(TOTAL_INDEXED_OUTAGES))
            CLM_MAINT_INDEX = 0
         ENDIF
!
         IF(LOCAL_MAINT_NUNITS > 0) THEN
!
!
!
            DO U = 1, LOCAL_MAINT_NUNITS ! ALAN TO SHORTEN THE LIST
!
! OUT TEMP.
! CALLED FROM ALAN'S MAINTENANCE PROGRAM
!
               CALL GET_MAINT_DATE_RANGE(U,
     +                                   UNIT_NUMBER,
     +                                   R_MONTH,
     +                                   BEG_MMDD,
     +                                   END_MMDD)
!
!               BEG_YYMM = BEG_MMDD/100+100*(TEST_YEAR - 1900)
!               END_YYMM = END_MMDD/100+100*(TEST_YEAR - 1900)
!
               IF( (BEG_MMDD(1) == 0 .OR. END_MMDD(1) == 0) .AND.
     +                  (BEG_MMDD(2) == 0 .OR. END_MMDD(2) == 0) ) THEN
                  CYCLE
               ELSEIF( (BEG_MMDD(1) /= 0 .AND. END_MMDD(1) /= 0)) THEN
                  OUTAGE_INDEX = 1
               ELSE
                  OUTAGE_INDEX = 2
               ENDIF
               ACTIVE_MAINT_NUNITS = ACTIVE_MAINT_NUNITS + 1
!
               CLM_MAINT_INDEX(ACTIVE_MAINT_NUNITS) = UNIT_NUMBER
!
               TG =
     +            GET_TRANS_GROUP_POSITION(
     +                        get_TRANSACTION_GROUP(UNIT_NUMBER))

               MONTHLY_ACTIVE_OUTAGES =
     +                                   MONTHLY_ACTIVE_OUTAGES + 1
               IF(MONTHLY_ACTIVE_OUTAGES == 1) THEN
                  NUM_ACTIVE_RECORDS = NUM_ACTIVE_RECORDS + 1
               ENDIF

                  BEGIN_IN_DAY = BEG_MMDD(OUTAGE_INDEX)

                  END_IN_DAY = END_MMDD(OUTAGE_INDEX)

               MONTHLY_ACTIVE_TRANS_OUTAGES = .TRUE.
               SAVE_MONTHLY_TRANS_OUTAGES = .TRUE.
!
! ASSUME AUTO_MAINT ARE ALL 7X24
!
!               IF(trim(OUTAGE_PRODUCT_TYPE(IREC)) == '7x24') THEN
!
               BEGIN_IN_HOUR = (BEGIN_IN_DAY-1)*24 + 1
               END_IN_HOUR = END_IN_DAY*24 + 1 
               ! CHANGE STATE IS IN NEXT HOUR
! START
               MONTH_EVENT_COUNTER(TG) = MONTH_EVENT_COUNTER(TG) + 1
               MEC = MONTH_EVENT_COUNTER(TG)
!
               EVENTS_IN_HOUR(MEC,TG) = BEGIN_IN_HOUR

               ! TRICKY LINK
               EVENTS_RECORD_INDEX(MEC,TG) = -1*ACTIVE_MAINT_NUNITS 
               EVENTS_START_STOP_INDEX(MEC,TG) = 2 ! START OUTAGE
               EVENTS_LINK_INDEX(MEC,TG) = MEC

               MONTH_EVENT_COUNTER(TG) = MONTH_EVENT_COUNTER(TG) + 1
               MEC = MONTH_EVENT_COUNTER(TG)
!
               EVENTS_IN_HOUR(MEC,TG) = END_IN_HOUR
               ! TRICKY LINK
               EVENTS_RECORD_INDEX(MEC,TG) = -1*ACTIVE_MAINT_NUNITS 
               EVENTS_START_STOP_INDEX(MEC,TG) = 3 ! STOP OUTAGE
               EVENTS_LINK_INDEX(MEC,TG) = MEC
!

!
            ENDDO ! UNITS
!
!
!
         ENDIF ! AUTO_MAINT_AS_HOURLY
!
!
         IF(LOCAL_FOR_NUNITS > 0) THEN
!
!
            DO U = 1, LOCAL_FOR_NUNITS ! ALAN TO SHORTEN THE LIST
!
! OUT TEMP.
! CALLED FROM ALAN'S MAINTENANCE PROGRAM
!
!
               BEGIN_IN_HOUR_ALAN = 0
               END_IN_HOUR_ALAN = 0

               CALL GET_FO_HRS_RANGE(U,
     + UNIT_NUMBER,
     + R_MONTH)
!
! 10/24/03. BIG CHANGE. PERMITS FREQUENCY AND DURATION BY UNIT
!

               DO I = 1, MAX_FO_PER_MONTH
!
                  IF(MAX_FO_PER_MONTH > 1) THEN
                     IF(BEGIN_IN_HOUR_ALAN(I) == 0 .OR.
     +                                 END_IN_HOUR_ALAN(I) == 0 ) THEN
                        IF(BEGIN_IN_HOUR_ALAN(I) == 0 .AND.
     +                                 END_IN_HOUR_ALAN(I) == 0 ) THEN
                           EXIT ! GO TO NEXT UNIT IN THE MONTH
                        ELSEIF(BEGIN_IN_HOUR_ALAN(I) == 0 ) THEN
                           BEGIN_IN_HOUR_ALAN(I) = 1
                           OUTAGE_INDEX = I
                         ELSEIF(END_IN_HOUR_ALAN(I) == 0 ) THEN
                           END_IN_HOUR_ALAN(I) = HOURS_IN(R_MONTH)+1
                           OUTAGE_INDEX = I
                        ENDIF
                     ELSE
                        OUTAGE_INDEX = I
                     ENDIF
                  ELSE
                     IF( (BEGIN_IN_HOUR_ALAN(1) == 0 .OR.
     +                         END_IN_HOUR_ALAN(1) == 0) .AND.
     +                   (BEGIN_IN_HOUR_ALAN(2) == 0 .OR.
     +                                 END_IN_HOUR_ALAN(2) == 0) ) THEN
                        CYCLE
                     ELSEIF( (BEGIN_IN_HOUR_ALAN(1) /= 0 .AND.
     +                                  END_IN_HOUR_ALAN(1) /= 0)) THEN
                        OUTAGE_INDEX = 1
                     ELSE
                        OUTAGE_INDEX = 2
                     ENDIF
                  ENDIF
!
                  BEGIN_IN_HOUR = BEGIN_IN_HOUR_ALAN(OUTAGE_INDEX)
                  END_IN_HOUR = END_IN_HOUR_ALAN(OUTAGE_INDEX)
                  IF(BEGIN_IN_HOUR == END_IN_HOUR) then
                    CYCLE ! FORCED OUTAGE = 0
                  endif
!
                  ACTIVE_FOR_NUNITS = ACTIVE_FOR_NUNITS + 1
                  U_FOR = ACTIVE_FOR_NUNITS+ACTIVE_MAINT_NUNITS
!
                  IF(U_FOR > TOTAL_INDEXED_OUTAGES) THEN
                     WRITE(4,*) "IN DETAILED FOR, EXCEEDED THE MAX"
                     WRITE(4,*) "NUMBER OF FORCED OUTAGES"
                     er_message='Stop requested from TF_OBJT2 SIID296'
                     call end_program(er_message)
                  ENDIF
!
                  CLM_MAINT_INDEX(U_FOR) = UNIT_NUMBER
!
                  TG =
     +               GET_TRANS_GROUP_POSITION(
     +                          get_TRANSACTION_GROUP(UNIT_NUMBER))
!
                  MONTHLY_ACTIVE_OUTAGES =
     +                                   MONTHLY_ACTIVE_OUTAGES + 1
                  IF(MONTHLY_ACTIVE_OUTAGES == 1) THEN
                     NUM_ACTIVE_RECORDS = NUM_ACTIVE_RECORDS + 1
                  ENDIF
!
!
                  MONTHLY_ACTIVE_TRANS_OUTAGES = .TRUE.
                  SAVE_MONTHLY_TRANS_OUTAGES = .TRUE.
!
! START
                  MONTH_EVENT_COUNTER(TG) = MONTH_EVENT_COUNTER(TG) + 1
                  MEC = MONTH_EVENT_COUNTER(TG)
!
                  EVENTS_IN_HOUR(MEC,TG) = BEGIN_IN_HOUR
                  EVENTS_RECORD_INDEX(MEC,TG) = -1* U_FOR ! TRICKY LINK
                  EVENTS_START_STOP_INDEX(MEC,TG) = 2 ! START OUTAGE
                  EVENTS_LINK_INDEX(MEC,TG) = MEC

                  MONTH_EVENT_COUNTER(TG) = MONTH_EVENT_COUNTER(TG) + 1
                  MEC = MONTH_EVENT_COUNTER(TG)
!
                  EVENTS_IN_HOUR(MEC,TG) = END_IN_HOUR
                  EVENTS_RECORD_INDEX(MEC,TG) = -1* U_FOR ! TRICKY LINK
                  EVENTS_START_STOP_INDEX(MEC,TG) = 3 ! STOP OUTAGE
                  EVENTS_LINK_INDEX(MEC,TG) = MEC

               ENDDO ! FOR WITHIN THE MONTH
            ENDDO ! UNITS
!
!
!
         ENDIF
!
! IDENTIFY FIRST_EVENT_IN_HOUR
!
         DO TG = 1, MAX_TRANS_GROUPS
!
            IF(MONTH_EVENT_COUNTER(TG) == 0) CYCLE
!
! I NEED TO HAVE AN INDEX FOR THE SORTED ARRAY SO 
! THAT EVENTS_RECORD_INDEX, ETC ARE OK
!
            CALL INT4_Sort(MONTH_EVENT_COUNTER(TG),
     +                    EVENTS_LINK_INDEX(1,TG),EVENTS_IN_HOUR(1,TG))
!
            CALL INT_Sort_4_I4(
     +                    MONTH_EVENT_COUNTER(TG),EVENTS_IN_HOUR(1,TG))
!
            IF(MONTH_EVENT_COUNTER(TG) > 2) THEN
               INTERCHANGE_FOUND = .FALSE.
               I = 0
               DO
                  I = I + 1
                  J = EVENTS_LINK_INDEX(I,TG)
                  K = EVENTS_LINK_INDEX(I+1,TG)
                  IF(EVENTS_IN_HOUR(I,TG) ==
     +                                     EVENTS_IN_HOUR(I+1,TG)) THEN
! PUT STOP EVENTS BEFORE START EVENTS IN EACH HOUR
                     IF(EVENTS_START_STOP_INDEX(J,TG) <
     +                              EVENTS_START_STOP_INDEX(K,TG)) THEN
                        EVENTS_LINK_INDEX(I,TG) = K
                        EVENTS_LINK_INDEX(I+1,TG) = J
                        INTERCHANGE_FOUND = .TRUE.
                     ENDIF
                  ENDIF
                  IF(I + 1 == MONTH_EVENT_COUNTER(TG))THEN
                     IF(INTERCHANGE_FOUND) THEN
                        INTERCHANGE_FOUND = .FALSE.
                        I = 1
                     ELSE
                        EXIT
                     ENDIF
                  ENDIF
               ENDDO
            ENDIF
!
            LOCAL_HOUR = 1
            LAST_HOUR = 999
!            MAX_HOURS = HOURS_IN(R_MONTH) + 1
!            DO J = 1, MAX_EVENTS_IN_MONTH
            DO J = 1, MONTH_EVENT_COUNTER(TG)
               IF( EVENTS_IN_HOUR(J,TG) == MAX_EVENTS_IN_MONTH+1) then
                EXIT ! NO MORE EVENTS
               endif
               IF(EVENTS_IN_HOUR(J,TG)  == LAST_HOUR) CYCLE
               FIRST_EVENT_IN_HOUR(EVENTS_IN_HOUR(J,TG),TG) = J
               LAST_HOUR = EVENTS_IN_HOUR(J,TG)
               IF(LAST_HOUR > MAX_HOURS) THEN

                  CALL MG_LOCATE_WRITE(20,0,
     +                  "A TRANSACT OUTAGE WAS NOT WITHIN THE MONTH",
     +                                                  ALL_VERSIONS,0)
                  er_message='Stop requested from TF_OBJT2 SIID297'
                  call end_program(er_message)
               ENDIF
            ENDDO
         ENDDO ! TG
!
      RETURN

C
      ENTRY OUTAGES_IN_MONTH_BY_TG(R_TG)
!

         IF(.NOT. ALLOCATED(MONTH_EVENT_COUNTER)) THEN
            OUTAGES_IN_MONTH_BY_TG = 0
         ELSE
            OUTAGES_IN_MONTH_BY_TG = MONTH_EVENT_COUNTER(R_TG)
         ENDIF
      RETURN

C
      ENTRY OUTAGE_EVENTS_IN_MONTH()
!

         OUTAGE_EVENTS_IN_MONTH = SAVE_MONTHLY_TRANS_OUTAGES
      RETURN

C
      ENTRY OUTAGE_EVENTS_IN_HOUR(R_HOUR,R_TG)
!

         IF(SAVE_TOTAL_OUTAGES > 0) THEN
            OUTAGE_EVENTS_IN_HOUR =
     +                             FIRST_EVENT_IN_HOUR(R_HOUR,R_TG) > 0
         ELSE
            OUTAGE_EVENTS_IN_HOUR = .FALSE.
         ENDIF
      RETURN

C
      ENTRY ADJUST_FOR_OUTAGES( R_HOUR,
     +                          R_OUTAGE_SINGULAR,
     +                          R_TG,
     +                          R_DATA_BASES,
     +                          R_OUTAGE_TYPE,
     +                          R_ADJUST_BLOCK,
     +                          R_DERATE_BLOCK)
C

         ADJUST_FOR_OUTAGES = .FALSE.
         J = FIRST_EVENT_IN_HOUR(R_HOUR,R_TG)
!
!
         IF(J == 0 .OR. EVENTS_IN_HOUR(J,R_TG) /= R_HOUR) THEN
!            WRITE(6,*) "AN HOURLY OUTAGE WAS DETECTED THAT WAS OUT"
!            WRITE(6,*) "OF ORDER.  PLEASE CHECK TRANSACT OUTAGE FILE"
            CALL MG_LOCATE_WRITE(20,0,
     +                  "AN HOURLY OUTAGE WAS DETECTED THAT WAS OUT",
     +                                                  ALL_VERSIONS,0)
            CALL MG_LOCATE_WRITE(20,0,
     +                  "OF ORDER.  PLEASE CHECK TRANSACT OUTAGE FILE",
     +                                                  ALL_VERSIONS,0)
            er_message='Stop requested from TF_OBJT2 SIID298'
            call end_program(er_message)
         ELSE
            DO
               K = EVENTS_LINK_INDEX(J,R_TG)
!
!
!
!               FOR THE AUTOMAINTENANCE SCHEDULER
!

               START_STOP = EVENTS_START_STOP_INDEX(K,R_TG)
!
! MULTIPLE UNITS WITH SAME ID ASSUMED ADJACENT.
!
               IREC = EVENTS_RECORD_INDEX(K,R_TG)
!
! TRAP FOR EVENTS GENERATED EXTERNALLY
!
               IF(IREC < 0) THEN
!
!
                  TEMP_IREC = -1*IREC
                  IF(TEMP_IREC > 31999 .OR. TEMP_IREC < -31999) THEN
                     SCREEN_MESSAGES = 'Outages exceed month limit'
                     CALL MG_LOCATE_WRITE(12,26,
     +                           trim(SCREEN_MESSAGES),ALL_VERSIONS,2)

                  ENDIF
                  LOCAL_UNIT_ID = CLM_MAINT_INDEX(TEMP_IREC)
!
                  TEMP_UNIT_NUMBER = LOCAL_UNIT_ID
                  TEMP_HIGHEST_UNIT_NUMBER = LOCAL_UNIT_ID
               ELSE

           IF(ACU_POWER_ID_INDEX) THEN ! ONLY FINDS THE FIRST OCCURRENCE
               LOCAL_UNIT_ID_I8 = UNIT_ID(IREC)
               TEMP_UNIT_NUMBER = GET_I8_ID_TO_UNIT(LOCAL_UNIT_ID_I8)
               NUMBER_OF_UNITS_FOR_ID = 1
            ELSE
               LOCAL_UNIT_ID = UNIT_ID(IREC)
               ! FIRST OCCURRENCE
              TEMP_UNIT_NUMBER = GET_RESOURCE_ID_TO_UNIT(LOCAL_UNIT_ID,
     +                                          NUMBER_OF_UNITS_FOR_ID) 
            ENDIF
!
!
! 09/11/01. GAT. TEMP FOR EMPIRE
                  TEMP_HIGHEST_UNIT_NUMBER = TEMP_UNIT_NUMBER

               ENDIF

               IF(TEMP_UNIT_NUMBER <= 0) CYCLE
               DO UNIT_NUMBER = TEMP_UNIT_NUMBER,
     +                                         TEMP_HIGHEST_UNIT_NUMBER
                  imm_un=unit_number
                  gtgi_result=get_TRANSACTION_GROUP(imm_un)
                  TG = GET_TRANS_GROUP_POSITION(gtgi_result)
!
                  ADJUST_FOR_OUTAGES = .TRUE.
!
                  OUTAGE_BLOCK1 = GET_UNIT_TO_OUTAGE_BLOCK(UNIT_NUMBER,
     +                                            INT(1,2),R_DATA_BASES)
                  OUTAGE_BLOCK2 = GET_UNIT_TO_OUTAGE_BLOCK(UNIT_NUMBER,
     +                                            INT(2,2),R_DATA_BASES)
!

                  BLOCK1_CAPACITY =
     +               GET_BLOCK_OUTAGE_CAPACITY(UNIT_NUMBER,INT(1,2),
     +                                                    R_DATA_BASES)
                  BLOCK2_CAPACITY =
     +               GET_BLOCK_OUTAGE_CAPACITY(UNIT_NUMBER,INT(2,2),
     +                                                    R_DATA_BASES)
!
                 TOTAL_UNIT_CAPACITY= BLOCK1_CAPACITY + BLOCK2_CAPACITY
! DERATE
                 IF(IREC > 0 .AND. DOWNTIME_TYPE(ABS(IREC)) == 'D')THEN
                     IF(DERATE_UNITS(IREC) == 'M') THEN
                        DERATE_MW = DERATE_QUANTITY(IREC)
                     ELSE ! ASSUME IT IS PERCENT
                        DERATE_MW = TOTAL_UNIT_CAPACITY *
     +                                       DERATE_QUANTITY(IREC)/100.
                     ENDIF
!
                     R_ADJUST_BLOCK(OUTAGE_BLOCK1) =
     +                              R_ADJUST_BLOCK(OUTAGE_BLOCK1) + 2.0
                     R_ADJUST_BLOCK(OUTAGE_BLOCK2) =
     +                              R_ADJUST_BLOCK(OUTAGE_BLOCK2) + 2.0
!
                     ! CHECK FOR > 0.?
                     IF(DERATE_MW < TOTAL_UNIT_CAPACITY) THEN 
                        IF(DERATE_MW < BLOCK2_CAPACITY) THEN
                           BLOCK2_DOWN_MULT =
     +                           (BLOCK2_CAPACITY - DERATE_MW)/
     +                                                  BLOCK2_CAPACITY
                           BLOCK1_DOWN_MULT = 1.
                        ELSE
                           BLOCK2_DOWN_MULT = 0.
                           BLOCK1_DOWN_MULT =
     +                           (TOTAL_UNIT_CAPACITY  - DERATE_MW)/
     +                                                  BLOCK1_CAPACITY
                        ENDIF
                     ELSE
                        BLOCK1_DOWN_MULT = 0.
                        BLOCK2_DOWN_MULT = 0.
                     ENDIF
!
                     IF(OUTAGE_BLOCK1 > 0) THEN
                        IF(START_STOP == 2) THEN
                           R_DERATE_BLOCK(OUTAGE_BLOCK1) =
     +                                                 BLOCK1_DOWN_MULT
                        ELSEIF(START_STOP == 3) THEN
                           R_DERATE_BLOCK(OUTAGE_BLOCK1) = 1.
                        ENDIF
                     ENDIF
!
                     IF(OUTAGE_BLOCK2 > 0) THEN
                        IF(START_STOP == 2) THEN
                           R_DERATE_BLOCK(OUTAGE_BLOCK2) =
     +                                                 BLOCK2_DOWN_MULT
                        ELSEIF(START_STOP == 3) THEN
                           R_DERATE_BLOCK(OUTAGE_BLOCK2) = 1.
                        ENDIF
                     ENDIF
                  ELSE
!
! OUTAGE AND DETAILED FOR/MOR CALC'S
!
                     R_ADJUST_BLOCK(OUTAGE_BLOCK1) =
     +                              R_ADJUST_BLOCK(OUTAGE_BLOCK1) + 1.0
                     R_ADJUST_BLOCK(OUTAGE_BLOCK2) =
     +                              R_ADJUST_BLOCK(OUTAGE_BLOCK2) + 1.0
!
                     BLOCK1_DOWN_MULT = 0.
                     BLOCK2_DOWN_MULT = 0.
!
                     IF(IREC < 0) THEN
                        IF(START_STOP == 3) THEN
                           R_OUTAGE_TYPE(OUTAGE_BLOCK1) = 0.
                           R_OUTAGE_TYPE(OUTAGE_BLOCK2) = 0.
                        ELSEIF(ABS(IREC) > ACTIVE_MAINT_NUNITS) THEN
                           R_OUTAGE_TYPE(OUTAGE_BLOCK1) = 2.
                           R_OUTAGE_TYPE(OUTAGE_BLOCK2) = 2.
                        ELSE
                           R_OUTAGE_TYPE(OUTAGE_BLOCK1) = 1.
                           R_OUTAGE_TYPE(OUTAGE_BLOCK2) = 1.
                        ENDIF
                     ELSEIF(START_STOP == 2) THEN
                        R_OUTAGE_TYPE(OUTAGE_BLOCK1) = 1.
                        R_OUTAGE_TYPE(OUTAGE_BLOCK2) = 1.
                     ELSE
                        R_OUTAGE_TYPE(OUTAGE_BLOCK1) = 0.
                        R_OUTAGE_TYPE(OUTAGE_BLOCK2) = 0.
                     ENDIF
!
                     IF(OUTAGE_BLOCK1 > 0) THEN
                        IF(START_STOP == 2) THEN
                           R_OUTAGE_SINGULAR(OUTAGE_BLOCK1) =
     +                                                 BLOCK1_DOWN_MULT
! NOTE: >= 3 FOR TVA. PREVIOUSLY == 3 3/29/02.
                        ELSEIF(START_STOP >= 3) THEN
                           R_OUTAGE_SINGULAR(OUTAGE_BLOCK1) = 1.
                        ENDIF
                     ENDIF
!
                     IF(OUTAGE_BLOCK2 > 0) THEN
                        IF(START_STOP == 2) THEN
                           R_OUTAGE_SINGULAR(OUTAGE_BLOCK2) =
     +                                                 BLOCK2_DOWN_MULT
! NOTE: >= 3 FOR TVA. 3/29/02.
                        ELSEIF(START_STOP >= 3) THEN
                           R_OUTAGE_SINGULAR(OUTAGE_BLOCK2) = 1.
                        ENDIF
                     ENDIF
!
                  ENDIF
!
              ENDDO ! MULTIPLE UNITS
!
              J = J + 1 ! GET NEXT EVENT
              IF(EVENTS_IN_HOUR(J,R_TG) /= R_HOUR) then
                EXIT ! NO LONGER THIS HOUR
              endif
            ENDDO
         ENDIF
      RETURN

      ENTRY DEALLOCATE_TRANS_OUTAGE_DATA

         DEALLOCATE_TRANS_OUTAGE_DATA = .TRUE.
         IF(ALLOCATED(UNIT_NAMEs_loc)) DEALLOCATE(
     +                                     UNIT_NAMEs_loc,
     +                                     UNIT_ID,
     +                                     BEGIN_DAY,
     +                                     BEGIN_MONTH,
     +                                     BEGIN_TO,
     +                                     END_DAY,
     +                                     END_MONTH,
     +                                     END_YEAR,
     +                                     BEGIN_YEAR,
     +                                     END_TO,
     +                                     BEGIN_DAY_IN_MONTH,
     +                                     END_DAY_IN_MONTH,
     +                                     RECORD_ACTIVE_IN_MONTH,
     +                                     DOWNTIME_TYPE,
     +                                     DERATE_UNITS,
     +                                     OUTAGE_PRODUCT_TYPE,
     +                                     DERATE_QUANTITY)
!     +                                     MONTH_EVENT_COUNTER,
         IF(ALLOCATED(EVENTS_RECORD_INDEX))
     +           DEALLOCATE(  EVENTS_RECORD_INDEX,
     +                        EVENTS_START_STOP_INDEX,
     +                        EVENTS_IN_HOUR,
     +                        EVENTS_LINK_INDEX,
     +                        FIRST_EVENT_IN_HOUR)
      RETURN
      END

C
      FUNCTION DAILY_OPTION_OBJECT()
      ! ASSUMES CALL OPTIONS
      ! ASSUMES A MAXIMUM LENGTH OF ONE WEEK
      ! CURRENTLY ONLY FOR USE WITH THE PRICE METHOD
      ! NEED TO HOOK INTO THE TRANSACT OUTAGE FILE AND GET OUTAGES
        use energy_trace
        use mod_base_year
        use end_routine
        use rptreccontrol
        use grx_planning_routines
        use eco
        use sizecom
        use prodcom
        use hesi_i4
        use tf_decs
        use capacity_arrays
        use irec_endpoint_control
        use params
        use logging
        use miscmod
        use prim_mover_idx
        use enrglimit
        use tf_objt_interfaces
        use nunits_shared
        use spindriftlib
        use prod_arrays_dimensions
        use dr_booth_modules
        use globecom
      use SpinDriftLib
      use prod_arrays_dimensions
      USE IREC_ENDPOINT_CONTROL
      use params
      use logging
      use miscmod
      use prim_mover_idx
      use hesi_i4
      use dr_booth_modules
      USE SIZECOM
      use globecom
      use prodcom
      use program_state
      implicit none
      SAVE
      real (kind=4) :: HOURLY_HYDRO_loc

      logical (kind=1) :: HOURLY_COMMITMENT ! External
      CHARACTER(len=1) :: START_UP_LOGIC,MARKET_RESOURCE_STR
      LOGICAL(kind=1) :: VOID_LOGICAL,GET_START_UP_LOGIC
      LOGICAL(kind=1) :: DAILY_OPTION_OBJECT,

     +          INIT_HOUR_PATH_LIMIT,
     +          DAILY_OPTION_FOR_MONTH,
     +          TRANC_SCREEN_MESSAGES,
     +          ALREADY_STARTED,
     +          UNIT_UP_YESTERDAY(:),
     +          GET_CLA_UNIT_UP_YESTERDAY,
     +          PUT_CLA_UNIT_UP_YESTERDAY,
     +          STRICT_MARKET_RESOURCE(:),
     +          STRICT_MARKET_W_LOAD_BAL(:),
     +          DSM_MARKET_RESOURCE(:),
     +          GRE_MARKET_RESOURCE(:),
     +          ECITY_MARKET_RESOURCE(:),
     +          ECITY_USED,
     +          TVA_MARKET_RESOURCE(:),
     +          MARKET_FLOOR_UNIT(:),
     +          MARKET_CEILING_UNIT(:),
     +          EMERGENCY_MEETS_SPIN,
     +          YES_EMERGENCY_MEETS_SPIN,
     +          IS_STRICT_MARKET_RESOURCE
      LOGICAL(kind=1) ::
     +          SHUT_DOWN_UNIT,
     +          PRICE_ONLY_WHOLESALE_REV=.FALSE.,
     +          RETAIL_AS_WHOLESALE=.FALSE.,
     +          APPLY_RETAIL_AS_WHOLESALE,
     +          APPLY_TRANS_REV_TO_WHOLESALE,
     +          MUST_RUN_UNIT,
     +          TEST_MONTHLY_MUST_RUN,
     +          YES_HOURLY_UNIT_FUEL_REPORT,HOURLY_UNIT_FUEL_REPORT,
     +          YES_DAILY_COMMITMENT,DAILY_COMMITMENT,
     +          WILLIAMS_ACTIVE,
     +          IS_5X16,APPLY_ENERGY_PRODUCT,
     +          OUT_ALL_DAY,
     +          OUT_BEGINNING_DAY,
     +          OUT_MIDDLE_DAY,
     +          OUT_END_DAY,
     +          BEGINNING_HOURS_TEST,
     +          MORNING_CONSEC_ACTIVE,
     +          ENCOUNTER_POSITIVE_MARGIN
      LOGICAL(kind=1) ::
     +          USE_RETAIL,
     +          SAVE_TRANSACT_C_STATUS,
     +          GET_TRANSACT_C_STATUS,
     +          TEMP_L,
     +          ALLOCATE_BLOCKS_2_CUSTOMERS,
     +          BLOCKS_2_CUSTOMERS_REPORT,
     +          ALLOC_COSTS_BY_UNIT,
     +          GET_ALLOC_COSTS_BY_UNIT,
     +          YES_MONTHLY_LOAD_N_RESOURCES,
     +          MONTHLY_DEPTH_OF_MARKET_DB,
     +          DEPTH_OF_MARKET_DB_ACTIVE,
     +          GET_DEPTH_OF_MARKET_QUANTITY,
     +          MONTHLY_CALL_PUT_CAPACITY,
     +          ANNUAL_CALL_PUT_CAPACITY,
     +          DailyOperMoPumpedStorage,
     +          DailyOperMoPSTransC,
     +          GET_GRX_TRANS_MONTHLY_STORAGE,
     +          CX_STORAGE,
     +          REPORT_THIS_CL_UNIT
      LOGICAL(kind=1) :: DEPTH_OF_MARKET=.FALSE.,DEPTH_OF_MARKET_LOGIC,
     +          ASCENDING=.TRUE., ! .false. => Descending order
     +          DESCENDING=.FALSE., ! .false. => Descending order
     +          GET_ONE_TRANS_RAMP_RATES,
     +          YES_PUT_AC_HOURLY_COST,
     +          PUT_AC_HOURLY_COST_AT_MARKET,
     +          REDUCE_HOURLY_FUEL_DERIVATIVES,
     +          YES_DECOMMIT_THERMAL_RESOURCES,
     +          YES_ALLOW_DECOMMIT_BY_UNIT,
     +          DECOMMIT_THERMAL_RESOURCES,
     +          TRANSACT_C_MONTHLY_INIT,
     +          COMMIT_ON_TOTAL_COST,
     +          YES_COMMIT_ON_TOTAL_COST,
     +          UNIT_UP_LAST_HOUR,
     +          LOOK_AHEAD_LOGIC,
     +          START_TEST,
     +          RUN_TEST,
     +          STOP_TEST,
     +          CURRENT_STATE,
     +          DETAILED_RAMPING
      INTEGER(kind=2) :: MAX_DAYS,R_HOURS_IN_MONTH,MAX_NBLOK,
     +          TRANC_VAR_NUM=8,LAST_I,FT,GET_PRIMARY_MOVER,PM,LM,
     +          GET_PRIMARY_MOVER_INDEX,
     +          MAX_PROD_FUEL_TYPES,MAX_PROD_TYPES,
     +          GET_TRANS_GROUP_INDEX
     
      INTEGER(kind=2), parameter :: max_fuel_types_tfo=9
      PARAMETER(MAX_PROD_FUEL_TYPES=6,MAX_PROD_TYPES=24)
      PARAMETER (MAX_DAYS=31)
      CHARACTER(len=20) :: FUEL_NAME(max_fuel_types_tfo)=
     +  (/                            'Coal                ',
     +                                       'Gas                 ',
     +                                       'Oil                 ',
     +                                       'Uranium             ',
     +                                       'Hydro               ',
     +                                       'Other               ',
     +                                       'Hydro Pumped Storage',
     +                                       'Solar               ',
     +                                       'Wind                '/)
      CHARACTER(len=20) ::
     +         PRICING_GROUP_SELL_NAME(:),
     +         PRICING_GROUP_BUY_NAME(:),
     +         ALL_DEMAND_NAME(:),
     +         TEMP_NAME,
     +         PRODUCT_RPT_NAME(MAX_PROD_TYPES)=(/
     +               'Combined Cycle      ','Fuel Cell           ',
     +               'Geothermal          ','Combustion Turbine  ',
     +               'Hydro               ','Storage             ',
     +               'Internal Combustion ','Other               ',
     +               'Nuclear             ','Solar               ',
     +               'Steam               ','Wind                ',
     +               'Cogeneration        ','Not Assigned        ',
     +               'Total               ',' Steam - Coal       ',
     +               ' Steam - Oil        ',' Steam - Gas        ',
     +               'Storage Generation  ','Storage Pump        ',
     +               'Daily Calls Puts    ', ! 21

     +               'Annual Calls Puts   ','Forwards            ',
     +               'Interruptible       '/) ! 24
      INTEGER*2 I,J,K,S_U,HR,BB,LR,U,B,C,L,M,
     + TEMP_B1,
     + TYPE_OF_OPTION,
     + R_DAYS_IN_MONTH,
     + OPTION_DAY,
     + OPTION_INDEX,
     + LOCAL_HOUR,
     + DAILY_OPTION_STATUS(:,:),
     + ALL_RESOURCE_TYPE(:), ! 1 = THERMAL, 2 = MARKET
     + ALL_RESOURCE_INDEX(:), ! POSITION WITHIN EACH TYPE OF RESOURCE
     + ALL_DEMAND_TYPE(:), ! 1 = RETAIL, 2 = MARKET
     + ALL_DEMAND_INDEX(:), ! POSITION WITHIN EACH TYPE OF RESOURCE
     + ALL_RESOURCE_NUMBER,
     + ALL_DEMAND_NUMBER,
     + MONTHLY_STARTS,
     + NO_START_UP_UNITS,
     + GET_START_UP_POSITION,
     + GET_NO_START_UP_UNITS,
     + BEST_BLOCK(24),
     + START_CONSEC_HOUR,
     + END_CONSEC_HOUR,
     + POSITIVE_HOURS_STARTS,
     + COUNT_CONSEC_HOURS,
     + R_MONTH,
     + R_DATA_BASE,
     + TEMP_I,
     + GET_DATA_BASE_BY_HOUR,
     + DATA_BASE_BY_HOUR(800),
     + HOUR_IN_MONTH,FIRST_HOUR_IN_DAY,
     + CURRENT_DATA_BASE,
     + HOURS_AVAILABLE_IN_DAY(0:31),
     + R_HOUR,
     + CALENDAR_DAY_OF_WEEK,
     + GET_DAY_OF_WEEK_4,
     + LAST_BEGINNING_HOUR,
     + ORIGINAL_VALUE_OF_U(:),
     + O,
     + INCREASING_VALUE_OF_U(:),
     + MINIMUM_UP_INDEX(:),
     + PRICING_GROUP_SELL_INDEX(:),
     + PRICING_GROUP_BUY_INDEX(:),
     + GET_MARKET_RESOURCE_COUNTER,
     + MARKET_RESOURCE_COUNTER(:),
     + UPPER_TRANS_GROUP,
     + PREVIOUS_CONSEC_UP_HOURS,
     + PREVIOUS_CONSEC_DOWN_HOURS,
     + HOURS_SINCE_START_UP,
     + HOURS_SINCE_SHUT_DOWN,
     + NEGATIVE_HOURS,
     + BEST_BLOCK_BY_(744),
     + MIN_UP_TIME_INT,
     + ECITY_S_U/0/
      REAL (KIND=4) :: MARGIN_SINCE_SHUT_DOWN,
     +                 MARGIN_SINCE_START_UP
      REAL(kind=4), allocatable :: PRICING_GROUP_SELL_QUANT_AVAIL(:,:)
      REAL(kind=4) :: OPTION_STRIKE_PRICE(0:MAX_DAYS),
     +          STRIKES(0:MAX_DAYS),
     +          OPTION_PAYOFF(0:MAX_DAYS),
     +          OPTION_COST(0:MAX_DAYS),
     +          DAILY_AVAIL_CAPACITY(0:31),
     +          DAILY_AVAILABILITY,
     +          DAILY_GROSS(0:MAX_DAYS),
     +          WEEKLY_MARGIN(0:24,0:7),
     +          CUM_WEEKLY_MARGIN(1:7),
     +          KEEP_UP_TOMORROW_BENEFIT,
     +          AVOIDED_START_UP_COST,
     +          PRICING_GROUP_SELL_PRICE(:,:),
     +          PRICING_GROUP_SELL_QUANT(:),
     +          PRICING_GROUP_BUY_QUANT_AVAIL(:,:),
     +          PRICING_GROUP_SELL_GEN(:,:),
     +          PRICING_GROUP_BUY_PRICE(:,:),
     +          PRICING_GROUP_BUY_QUANT(:),
     +          ALL_DEMAND_BUY_GEN(:,:),
     +          HOURLY_RESOURCES_AND_MARKETS(:,:),
     +          OPTION_HOURS
      REAL(kind=4) ::
     +          ALL_RESOURCE_PRICE(:),
     +          ALL_RESOURCE_QUANTITY(:),
     +          ALL_RESOURCE_QUANTITY_USED(:),
     +          ALL_DEMAND_SERVED(:,:),
     +          ALL_DEMAND_PRICE(:),
     +          ALL_DEMAND_QUANTITY(:),
     +          MINIMUM_LOAD_HOURS,
     +          DAILY_PRICE(24),
     +          DAILY_PRODUCTS_CAPACITY(24),
     +          R_MONTHLY_PRICE(*),
     +          LOCAL_PRICE(800),
     +          L_MONTHLY_PRICE(800),
     +          HOURLY_MW(24),
     +          STRAT_HOURLY_MW(24,0:5),
     +          BEST_MARGIN(24),
     +          DAILY_CUM_MARGIN(24),
     +          HOURS_IN_DAY=24.,
     +          OPTION_MWH(0:MAX_DAYS),
     +          OPTION_HEAT(0:MAX_DAYS)
      REAL(kind=4) ::
     +          OPTION_INV_HEAT(0:MAX_DAYS),
     +          OPTION_AVERAGE_HEAT,
     +          OPTION_START_UP_COST(0:MAX_DAYS),
     +          BLOCK_PRICE(2),
     +          BLOCK_INC_PRICE(2),
     +          BLOCK_TOTAL_PRICE(2),
     +          GET_TOTAL_INCREMENTAL_COST,
     +          GET_AVERAGE_TOTAL_COST,
     +          GET_INCREMENTAL_FUEL_COST,
     +          SECOND_PRICE(2),
     +          BLOCK_CAPACITY(2),
     +          TEMP_BLOCK_PRICE(2),
     +          GET_ANNUAL_CL_CAPACITY,
     +          TOTAL_CAPACITY,
     +          MONTHS_ACTIVE,
     +          BLOCK1_MARGIN,
     +          BLOCK2_MARGIN,
     +          GET_BLOCK_OUTAGE_CAPACITY,
     +          START_UP_COSTS(:)
      REAL(kind=4) ::
     +          EMERGENCY_CAPACITY(:),
     +          MIN_SPIN_CAP(:),
     +          MAX_SPIN_CAP(:),
     +          GET_MIN_SPIN_CAP,
     +          GET_MAX_SPIN_CAP,
     +          HOURLY_EMERGENCY_CAPACITY(:),
     +          MAX_HOURLY_RAMP_UP(:),
     +          MAX_HOURLY_RAMP_DOWN(:),
     +          SYSTEM_EMERGENCY_AVAILABLE,
     +          GET_EMERGENCY_CAPACITY,
     +          EMERGENCY_HEATRATE(:),
     +          MONTH_UNIT_START_COST(:),
     +          MONTH_OPERATING_HOURS(:),
     +          GET_UNIT_START_UP_COSTS,
     +          THIS_YEAR,
     +          BEST_DAILY_GROSS,
     +          ALL_HOURS_MARGIN,
     +          POSITIVE_HOURS_MARGIN,
     +          ALL_HOURS_COST
      REAL(kind=4) ::
     +          ALL_HOURS_PAYOFF,
     +          ALL_HOURS_MWH(2),
     +          POS_CONSEC_HOURS,
     +          BEST_ACCUM_POS_CONSEC_HOURS,
     +          POS_CONSEC_MARGIN,
     +          ACCUM_POS_CONSEC_MARGIN(24),
     +          BEST_ACCUM_POS_CONSEC_MARGIN,
     +          POS_CONSEC_COST,
     +          POS_CONSEC_PAYOFF,
     +          POS_CONSEC_MWH(2),
     +          MORNING_CONSEC_HOURS,
     +          MORNING_CONSEC_MARGIN,
     +          MORNING_CONSEC_COST,
     +          MORNING_CONSEC_PAYOFF,
     +          MORNING_CONSEC_MWH(2),
     +          POSITIVE_HOURS,
     +          POSITIVE_HOURS_COST,
     +          POSITIVE_HOURS_PAYOFF,
     +          POSITIVE_HOURS_MWH(2)
      REAL(kind=4) ::
     +          ALL_MIN_LOAD_HOURS,
     +          POSITIVE_MIN_LOAD_HOURS,
     +          POS_CONSEC_MIN_LOAD_HOURS,
     +          MORNING_CONSEC_MIN_LOAD_HOURS,
     +          NEXT_DAY_OPTIONALITY,
     +          RAMP_RATE(:),
     +          RAMP_DOWN_RATE(:),
     +          GET_RAMP_RATE,
     +          GET_RAMP_DOWN_RATE,
     +          SYSTEM_RAMP_UP,SYSTEM_RAMP_DOWN,
     +          MIN_UP_TIME(:),
     +          START_UP_COST_PER_MWH(:), ! ASSUMES MINIMUM HOURS
     +          EMERGENCY_COST_PER_MWH(:),
     +          GET_MIN_UP_TIME,
     +          MIN_DOWN_TIME(:),GET_MIN_DOWN_TIME,
     +          GET_FOR_DURATION,FOR_DURATION,
     +          GET_FOR_FREQUENCY,FOR_FREQUENCY,
     +          MONTH_OPTION_HOURS,
     +          MONTH_MINIMUM_LOAD_HOURS
      REAL(kind=4) ::
     +          MONTH_ALL_HOURS_MARGIN,
     +          MONTH_POSITIVE_HOURS_MARGIN,
     +          MONTH_POS_CONSEC_MARGIN,
     +          MONTH_MORNING_CONSEC_MARGIN,
     +          MONTH_THREE_DAY_MARGIN,
     +          MONTH_SEVEN_DAY_MARGIN,
     +          CAPACITY_FACTOR,
     +          MONTHLY_OPTION_ENERGY,
     +          MONTHLY_OPTION_REVENUE,
     +          FIRST_BLOCK_AVAIL(800),
     +          SECOND_BLOCK_AVAIL(800),
     +          FIRST_BLOCK_OUT(800),
     +          SECOND_BLOCK_OUT(800),
     +          FIRST_DAILY_OUT(31),
     +          SECOND_DAILY_OUT(31),
     +          TEMP_R,
     +          TEMP_R2,
     +          TEMP_RL,
     +          TEMP_RM
      REAL(kind=4) ::
     +          LOCAL_LOAD,
     +          MIN_LOCAL_LOAD,
     +          MAX_LOCAL_LOAD,
     +          MIN_DEPTH_MARKET,
     +          MAX_DEPTH_MARKET,
     +          DEPTH_QUANTITY(800),
     +          DEPTH_PRICE(800),
     +          DEPTH_MARGINAL_COST(800),
     +          DEPTH_RESOURCES(800),
     +          GET_BLOCK_AVAIL_4_MONTH,
     +          MINIMUM_ENERGY,
     +          MONTH_MINIMUM_ENERGY,
     +          MONTH_SECOND_ENERGY,
     +          MONTH_LEFT_HEAT,
     +          MONTH_RIGHT_HEAT,
     +          R_TRANS_ROR_CAPACITY,
     +          R_TRANS_SPINNING,
     +          HOURLY_SPINNING_REQUIREMENT,
     +          STRADDLE_CAPACITY
      REAL(kind=4) ::
     +          TEMP_CAPACITY,
     +          PHANTOM_CAPACITY,
     +          HOURLY_COMMITMENT_GENERATION,
     +          HOURLY_COMMITMENT_UNSERVED,
     +          HOURLY_COMMITMENT_LOAD,
     +          HOURLY_COMMITMENT_MC,
     +          HOURLY_COMMITMENT_MC_AT_LOAD,
     +          HOURLY_NATIVE_COST,
     +          ESCALATED_MONTHLY_VALUE,
     +          MAXIMUM_IMPORT_TIE,
     +          MAXIMUM_EXPORT_TIE,
     +          MAXIMUM_FIRST_IMPORT_TIE,
     +          MAXIMUM_FIRST_EXPORT_TIE,
     +          MAXIMUM_SECOND_IMPORT_TIE,
     +          MAXIMUM_SECOND_EXPORT_TIE,
     +          HOURLY_CAPACITY,
     +          HOURLY_HEAT,
     +          DAILY_HEAT,
     +          DAILY_MWH
      REAL(kind=4) ::
     +          PECO_MULT, ! 1=400MW, 2=200MW, 3= 0MW
     +          BEGINNING_HOURS_MARGIN,
     +          BEGINNING_HOURS_COST,
     +          BEGINNING_HOURS_PAYOFF,
     +          BEGINNING_HOURS_MWH(2),
     +          ON_PEAK_BUY_SPREAD,
     +          ON_PEAK_SELL_SPREAD,
     +          ON_PEAK_SECOND_BUY_WHEEL,
     +          ON_PEAK_SECOND_SELL_WHEEL,
     +          ON_PEAK_BUY_WHEEL,
     +          ON_PEAK_SELL_WHEEL,
     +          OFF_PEAK_BUY_SPREAD,
     +          OFF_PEAK_SELL_SPREAD,
     +          OFF_PEAK_BUY_WHEEL,
     +          OFF_PEAK_SELL_WHEEL,
     +          OFF_PEAK_SECOND_BUY_WHEEL,
     +          OFF_PEAK_SECOND_SELL_WHEEL,
     +          LOCAL_SECOND_BUY_WHEEL ,
     +          LOCAL_BUY_WHEEL
      REAL(kind=4) ::
     +          DAILY_BUY_WHEEL(24),
     +          DAILY_SELL_WHEEL(24),
     +          LOCAL_BUY_SPREAD,
     +          LOCAL_SELL_SPREAD,
     +          PATH_SPREAD_RATE,
     +          PATH_WHEELING_CHARGE,
     +          HOURLY_FORWARD_SALE,
     +          HOURLY_FORWARD_CONTRACT_ENERGY,
     +          HOURLY_INTERRUPTIBLE,
     +          HOURLY_INTERRUPTIBLE_CAPACITY,
     +          HOURLY_CONTINGENT_MARKET_CAP,
     +          HOURLY_CONTINGENT,
     +          HOURLY_IMPORT_CAP,
     +          HOURLY_LOAD_FOLLOW(24),
     +          TEMP_LOAD_FOLLOW,
     +          HOURLY_LF_CAPACITY,
     +          GET_ANNUAL_CALL_PUT_CAPACITY,
     +          A_CO,
     +          B_CO
      REAL(kind=4) :: C_CO,
     +          TOTAL_HEAT,
     +          GET_HEAT_RATE_FACTOR,
     +          TOTAL_BLOCK_COST,
     +          PERCENT_RETAIL,
     +          PERCENT_WHOLESALE,
     +          RETAIL_PRICE,
     +          HOURLY_MARKET_PRICE,
     +          PERCENT_OF_COST_2_ALLOCATE,
     +          BLOCK_COST(744,2),
     +          BLOCK_REV(744,2),
     +          BLOCK_MARGIN(744,2),
     +          BEST_BLOCK_MARGIN(744),
     +          BEST_BLOCK_CAPACITY(744),
     +          PRE_COMMIT_MW(744),
     +          LOOK_AHEAD_MARGIN(-3:744),
     +          NEGATIVE_HOURS_MARGIN,
     +          PRODUCT_HOURS_POSIT(3),
     +          LOCAL_DAILY_MARKET_REVENUE,
     +          TEMP_DAILY_PRICE
      INTEGER :: VALUES_2_ZERO
      CHARACTER(len=12) :: TEMP_CHAR_12
!
! DETAILED REPORTING
!

      INTEGER(kind=2), parameter ::   NUm_Prods_loc=15
      integer (kind=2) :: TEMP_PRODUCT_HOURS

      ! TMS 20041129 NUM_PRODS increased by 2 to 15

       CHARACTER (len=20) ::   LOCAL_PRODUCT_TYPE(NUm_Prods_loc)=
     +                                     (/'5x16                ',
     +                                      '6x16                ',
     +                                      '7x16                ',   ! TMS added 20041129 for TVA
     +                                      '5x8                 ',
     +                                      '6x8                 ',
     +                                      '7x8                 ',
     +                                      'Wrap                ',
     +                                      'Western Wrap        ',
     +                                      '7x24                ',   ! Eventually add 6x24 and 5x25 for Completeness
     +                                      '2x24                ',   ! Eventually add Sax24 and Sux24 for Completness
     +                                      'Sax16               ',
     +                                      'Sux16               ',
     +                                      'Wkx16               ',
     +                                      'Wkx8                ',   ! TMS added 20041129 for TVA
     +                                      'Super Peak          '/)   ! Eventually add Sax8 and Sux8 for Completness
         LOGICAL(kind=1) :: CX_SUMMARY_REPORT,COMMITMENT_REPORT,
     +               CX_REPORT_NOT_OPEN=.TRUE.,
     +               C_DEPTH_MARKET_REPORT=.FALSE.,
     +               C_DEPTH_MARKET_NOT_OPEN=.TRUE.,


     +               WRITE_DEPTH_MARGINAL_COST
         INTEGER(kind=2) :: CX_REPORT_VARIABLES,
     +               CX_DAILY_NO=0,
     +               CX_DAILY_HEADER,
     +               C_DEPTH_MARKET_RPT_HEADER,
     +               C_DEPTH_MARKET_UNIT
         INTEGER(kind=4) ::   C_DEPTH_MARKET_REC
         REAL(kind=4) ::  PRODUCT_HOURS(NUm_Prods_loc),
     +               PRODUCT_PRICE(NUm_Prods_loc),
     +               PRODUCT_QUANTITY(NUm_Prods_loc),
     +               PRODUCT_MARGINAL_COST(NUm_Prods_loc)
         LOGICAL(kind=1) :: HX_SUMMARY_REPORT,
     +               HX_REPORT_NOT_OPEN=.TRUE.,
     +               XX_SUMMARY_REPORT,
     +               XX_REPORT_NOT_OPEN=.TRUE.,
     +               KX_SUMMARY_REPORT,
     +               KX_REPORT_NOT_OPEN=.TRUE.,
     +               QX_SUMMARY_REPORT,
     +               QX_REPORT_NOT_OPEN=.TRUE.,
     +               WX_SUMMARY_REPORT,
     +               WX_REPORT_NOT_OPEN=.TRUE.
         INTEGER(kind=2) :: HX_REPORT_VARIABLES,
     +               XX_REPORT_VARIABLES,
     +               HX_HOURLY_NO=0,
     +               HX_HOURLY_HEADER,
     +               KX_HOURLY_NO=0,
     +               KX_HOURLY_HEADER,
     +               XX_HOURLY_NO=0,
     +               XX_HOURLY_HEADER,
     +               QX_HOURLY_NO=0,
     +               WX_HOURLY_NO=0,
     +               C_HOURLY_COST_HEADER,
     +               C_HOURLY_COST_UNIT,
     +               C_HOURLY_MWH_COST_HEADER,
     +               C_HOURLY_MWH_COST_UNIT,
     +               C_HOURLY_SUMMARY_HEADER,
     +               C_HOURLY_SUMMARY_UNIT,
     +               C_TG_HOURLY_SUMMARY_HEADER,
     +               C_TG_HOURLY_SUMMARY_UNIT
         LOGICAL(kind=1) :: DX_SUMMARY_REPORT,
     +               DX_REPORT_NOT_OPEN=.TRUE.,
     +               DAILY_DISPATCH_ORDER_REPORT,
     +               YES_DAILY_DISPATCH_ORDER_RPT
         INTEGER(kind=2) :: DX_REPORT_VARIABLES,
     +               DX_HOURLY_NO=0,
     +               DX_HOURLY_HEADER
         LOGICAL(kind=1) :: OX_SUMMARY_REPORT,
     +               OX_REPORT_NOT_OPEN=.TRUE.,
     +               HOURLY_OUTAGE,
     +               YES_HOURLY_OUTAGE,
     +               HOURLY_SPIN_REPORT,
     +               YES_HOURLY_SPIN_REPORT,
     +               CONTRIBUTES_TO_SPIN(:)
         INTEGER(kind=2) :: OX_REPORT_VARIABLES,
     +               QX_REPORT_VARIABLES,
     +               OX_HOURLY_NO=0,
     +               OX_HOURLY_HEADER,
     +               QX_HOURLY_HEADER,
     +               WX_HOURLY_HEADER,
     +               WX_REPORT_VARIABLES,
     +               MONTH_UNIT_STARTS(:),
     +               MONTH_UNIT_DOWNS(:),
     +               IN_MIN_UP_TIME_STATE(:,:),
     +               IN_MIN_DOWN_TIME_STATE(:,:),
     +               MUST_RUN_BLOCK(:),
     +               UNIT_IS_UP(:)
         LOGICAL(kind=1) :: HEC_SUMMARY_REPORT,
     +               HEC_REPORT_NOT_OPEN=.TRUE.
         INTEGER(kind=2) :: HEC_REPORT_VARIABLES,
     +               HEC_HOURLY_NO=0,
     +               HEC_HOURLY_HEADER
         INTEGER :: CX_DAILY_REC,
     +           HX_HOURLY_REC,
     +           XX_HOURLY_REC,
     +           KX_HOURLY_REC,
     +           QX_HOURLY_REC,
     +           WX_HOURLY_REC,
     +           HEC_HOURLY_REC,
     +           OX_HOURLY_REC,DX_HOURLY_REC,
     +           C_HOURLY_COST_REC,
     +           C_HOURLY_MWH_COST_REC,
     +           C_HOURLY_SUMMARY_REC,
     +           C_TG_HOURLY_SUMMARY_REC
         character(len=20) :: cla_return_unitnm ! External
         LOGICAL(kind=1) :: GET_SPIN_STATUS,

     +            RESOURCE_IS_CHEAPER,
     +            MULTI_MARKET_ACTIVE,
     +            YES_MULTI_MARKET_ACTIVE,
     +            END_OF_UNITS,
     +            END_OF_PRICING_GROUPS,
     +            ALLOW_MARKET_ARBITRAGE,
     +            YES_ALLOW_MARKET_ARBITRAGE,
     +            MARKETS_AVAILABLE
         CHARACTER(len=9) :: R_MONTH_NAME
         CHARACTER(len=10) :: PRODUCT_NAME(3)=(/'7x24     ','5x16     ',
     +                                 'Wrap     '/)
         CHARACTER(len=20) :: OPTION_NAME(:),RETURN_UNITNM,
     +                  ANNUAL_NAME,
     +                  LOCAL_NAME
         CHARACTER(len=35) :: TRANS_GROUP_NAME,
     +                  GET_GROUP_NAME
! ECITIES
      REAL(kind=4) :: BEFORE_RETAINED,GET_DUKE_BEFORE_RETAINED,
     +         MONTHLY_CONTINGENT_CAP,GET_MONTHLY_CONTINGENT_CAP,
     +         HOURLY_AC_LOAD(24),
     +         LOCAL_UNIT_BID,
     +         LOCAL_PRICING_GROUP_BID,
     +         RETAINED_AVAILABLE,GET_DUKE_HOURLY_RETAINED,
     +         RESERVE_CAPACITY,EQUIVALENT_RETAINED,
     +         RESERVE_CAPACITY_USED,
     +         MONTHLY_RESERVE_ENERGY,DAILY_RESERVE_ENERGY(24),
     +         GET_MONTHLY_DUKE_RESERVE_ENERGY,
     +         GET_DUKE_RESERVE_CAPACITY
! 09/16/03.
      LOGICAL(kind=1) :: MON_TRANC_REPORT_NOT_OPEN=.TRUE.,
     +          YES_MON_TRANC_REPORT,WRITE_TRANC_MONTHLY_SUMMARY,
     +          MON_POSIT_REPORT_NOT_OPEN=.TRUE.,
     +          PROD_PROD_REPORT_ACTIVE,
     +          PROD_MW_REPORT_ACTIVE,
     +          TRANSACT_PROD_REPORT,
     +          TRANSACT_MW_REPORT,
     +          PROD_PROD_REPORT_NOT_OPEN=.TRUE.,
     +          PROD_MW_REPORT_NOT_OPEN=.TRUE.
      INTEGER(kind=2) :: MON_TRANC_NUM,MON_TRANC_UNIT,
     +          MON_TRANC_RPT_HEADER,TG,
     +          MON_POSIT_RPT_HEADER,MON_POSIT_NUM,MON_POSIT_UNIT,
     +          TRANS_PROD_PROD_VARIABLES,MAX_PROD_PROD_TYPES=24,
     +          TRANS_PROD_PROD_NO,
     +          TRANS_PROD_MW_NO,
     +          TRANSACT_PROD_PROD_RPT_HEADER,
     +          TRANSACT_PROD_MW_RPT_HEADER,
     +          TRANS_PROD_FUEL_NO,
     +          TRANSACT_PROD_FUEL_RPT_HEADER
      PARAMETER(MON_TRANC_NUM=64)
      PARAMETER(MON_POSIT_NUM=31) ! 27-31 EMISSIONS 02/06/09
      INTEGER :: MON_TRANC_REC,
     +          MON_POSIT_REC,
     +          TRANS_PROD_PROD_REC,
     +          TRANS_PROD_MW_REC,
     +          TRANS_PROD_FUEL_REC

!
! HEC VARIABLES
!
      LOGICAL(kind=1) :: HOOSIER=.FALSE.,YES_HOOSIER,
     +            ECITIES,YES_ECITIES_UNITS_ACTIVE,
     +            END_EFFECTS_TEST,
     +            TRANC_JDA_LOGIC,
     +            GET_TIE_GROUP_LIMIT,
     +            YES_HOURLY_JDA_REPORT,
     +            HOURLY_JDA_REPORT,
     +            MULTI_AREA_PRICING_FOR_MONTH,
     +            MULTI_AREA_BUY_PRICE_FOR_MONTH,
     +            INIT_MONTH_ALLOC_BLOCKS_2_CUST,
     +            HOUR_JDA_REPORT_NOT_OPEN=.TRUE.,
     +            USE_DEPTH_SOLUTION
      INTEGER(kind=2) :: M1_1,M1_2,M2_1,M2_2,R1_1,R1_2,R2_1,R2_2,
     +            M1,M2,R1,R2,GET_HOOSIER_INDICES,DAY,WI,
     +            PE,DG,BE,WO,HEC_REV_VARS=1,
     +            HOUR_JDA_NUM,
     +            HOUR_JDA_UNIT,
     +            HOUR_JDA_RPT_HEADER,
     +            R_CURRENT_YEAR
      INTEGER(kind=4) :: HOUR_JDA_REC
      REAL(kind=4) :: MEROM1_CAPACITY,MEROM2_CAPACITY,
     +            RATTS1_CAPACITY,RATTS2_CAPACITY,
     +            MEROM1_OUTAGE_TYPE,MEROM2_OUTAGE_TYPE,
     +            RATTS1_OUTAGE_TYPE,RATTS2_OUTAGE_TYPE,
     +            HEC_MOR_CAPACITY(24),
     +            DAILY_DSM_MW(24),
     +            DAILY_BLOCK_DSM_MW(24),
     +            MEROM1_PECO_DERATE_CAP,
     +            MEROM2_PECO_DERATE_CAP,
     +            WILLIAMS_OBLIGATION_MW(24),
     +            HEC_UC_RISK_ENERGY(24),HEC_EXCESS_MWH(24),
     +            MONTHLY_HEC_UC_RISK_ENERGY=0.,
     +            HEC_AVAIL_GEN_REQMT(24),HEC_LOAD(24),
     +            HEC_NATIVE_LOAD(24),
     +            HEC_NET_GENERATION(24),
     +            HEC_MARKET_PURCHASES(24),
     +            HEC_MEMBER_LOAD(24),
     +            HEC_EQUIV_GEN(24),
     +            HEC_ACTUAL_GEN(24),
     +            HEC_MARKET_GEN(24)
      REAL(kind=4) ::
     +            GET_BLOCK_AVAIL_4_HOUR,
     +            GET_BLOCK_OUTAGE_4_HOUR,
     +            REMAIN,
     +            HEC_PECO_LOAD(24),
     +            HEC_GEN_BOUGHT(24),
     +            HEC_GEN_SOLD(24),
     +            HEC_TOTAL_SUPPLY(24),
     +            HEC_GEN_AV_4LD_AF_MKT(24),
     +            HEC_MKT_PP(24),
     +            HEC_REM_MKT_PURCH(24),
     +            HEC_TOTAL_DEMAND(24),
     +            HEC_PECO_PRICE(30)=
     +                  (/14.74,15.13,15.75,16.16,16.58,
     +                    0.,0.,0.,0.,0.,0.,0.,0.,0.,
     +                    0.,0.,0.,0.,0.,0.,0.,0.,0.,
     +                    0.,0.,0.,0.,0.,0.,0./),
     +            HEC_OPERATIONS=40.,
     +            HEC_DAILY_PEAK,
     +            DAILY_PEAK
      REAL(kind=4) ::
     +            GET_DAILY_PEAK_SPIN,
     +            TRANS_SPINNING_CAPACITY,
     +            GET_TRANS_SPIN_FOR_TG,
     +            GET_OFF_PEAK_SPIN_FOR_TG,
     +            HOURLY_SPINNING_CAPACITY,
     +            HEC_DAILY_RESERVE,
     +            MEROM1_EFFECTIVE_CAP,
     +            MEROM2_EFFECTIVE_CAP,
     +            RATTS1_EFFECTIVE_CAP,
     +            RATTS2_EFFECTIVE_CAP,
     +            HOURLY_LOAD_FROM_AC_TG,
! HEC CX REPORT
     +            WILLIAMS_OBLIGATION_ENERGY=0.,
     +            WILLIAMS_OBLIGATION_PEAK=0.,
     +            WILLIAMS_OBLIGATION_HOURS=0.,
     +            HEC_EXCESS_ENERGY=0.,
     +            HEC_EXCESS_PEAK=0.,
     +            HEC_EXCESS_HOURS=0.,
     +            HEC_EXCESS_ENERGY_REV=0.,
     +            HEC_EXCESS_ENERGY_COST
      REAL(kind=4) ::
     +            HEC_PECO_ENERGY=0.,
     +            HEC_PECO_PEAK=0.,
     +            HEC_PECO_HOURS=0.,
     +            HEC_PECO_ENERGY_REVENUE=0.,
     +            HEC_NATIVE_LOAD_ENERGY=0.,
     +            HEC_NATIVE_LOAD_PEAK=0.,
     +            HEC_NATIVE_LOAD_HOURS=0.,
     +            HEC_NATIVE_LOAD_ENERGY_REV=0.,
     +            HEC_NATIVE_LOAD_ENERGY_COST,
     +            HEC_MARKET_GEN_ENERGY=0.,
     +            HEC_MARKET_GEN_PEAK=0.,
     +            HEC_MARKET_GEN_HOURS=0.,
     +            HEC_MARKET_GEN_ENERGY_REV=0.,
     +            HEC_GEN_BOUGHT_ENERGY=0.,
     +            HEC_GEN_BOUGHT_PEAK=0.,
     +            HEC_GEN_BOUGHT_HOURS=0.,
     +            HEC_GEN_BOUGHT_ENERGY_REV=0.,
     +            HEC_GEN_SOLD_ENERGY=0.,
     +            HEC_GEN_SOLD_PEAK=0.
      REAL(kind=4) ::
     +            HEC_GEN_SOLD_HOURS=0.,
     +            HEC_GEN_SOLD_ENERGY_REV=0.,
     +            HEC_DAILY_RESERVE_ENERGY=0.,
     +            HEC_DAILY_RESERVE_PEAK=0.,
     +            HEC_DAILY_RESERVE_HOURS=0.,
     +            HEC_DAILY_RESERVE_ENERGY_REV=0.,
     +            HEC_DAILY_RISK_ENERGY=0.,
     +            HEC_DAILY_RISK_PEAK=0.,
     +            HEC_DAILY_RISK_HOURS=0.,
     +            HEC_DAILY_RISK_ENERGY_REV=0.,
     +            HEC_DAILY_NETG_ENERGY=0.,
     +            HEC_DAILY_NETG_PEAK=0.,
     +            HEC_DAILY_NETG_HOURS=0.,
     +            HEC_DAILY_NETG_ENERGY_REV=0.,
! HEC ANNUAL CX REPORT
     +            WILLIAMS_OBLIGATION_ENERGY_AN=0.,
     +            WILLIAMS_OBLIGATION_PEAK_AN=0.,
     +            WILLIAMS_OBLIGATION_HOURS_AN=0.,
     +            HEC_EXCESS_ENERGY_AN=0.,
     +            HEC_EXCESS_PEAK_AN=0.
      REAL(kind=4) ::
     +            HEC_EXCESS_HOURS_AN=0.,
     +            HEC_EXCESS_ENERGY_REV_AN=0.,
     +            HEC_PECO_ENERGY_AN=0.,
     +            HEC_PECO_PEAK_AN=0.,
     +            HEC_PECO_HOURS_AN=0.,
     +            HEC_PECO_ENERGY_REVENUE_AN=0.,
     +            HEC_NATIVE_LOAD_ENERGY_AN=0.,
     +            HEC_NATIVE_LOAD_PEAK_AN=0.,
     +            HEC_NATIVE_LOAD_HOURS_AN=0.,
     +            HEC_NATIVE_LOAD_ENERGY_REV_AN=0.,
     +            HEC_MARKET_GEN_ENERGY_AN=0.,
     +            HEC_MARKET_GEN_PEAK_AN=0.,
     +            HEC_MARKET_GEN_HOURS_AN=0.,
     +            HEC_MARKET_GEN_ENERGY_REV_AN=0.,
     +            HEC_GEN_BOUGHT_ENERGY_AN=0.,
     +            HEC_GEN_BOUGHT_PEAK_AN=0.,
     +            HEC_GEN_BOUGHT_HOURS_AN=0.,
     +            HEC_GEN_BOUGHT_ENERGY_REV_AN=0.,
     +            HEC_GEN_SOLD_ENERGY_AN=0.
      REAL(kind=4) ::
     +            HEC_GEN_SOLD_PEAK_AN=0.,
     +            HEC_GEN_SOLD_HOURS_AN=0.,
     +            HEC_GEN_SOLD_ENERGY_REV_AN=0.,
     +            HEC_DAILY_RESERVE_ENERGY_AN=0.,
     +            HEC_DAILY_RESERVE_PEAK_AN=0.,
     +            HEC_DAILY_RESERVE_HOURS_AN=0.,
     +            HEC_DAILY_RESERVE_ENER_REV_AN=0.,
     +            HEC_DAILY_RISK_ENERGY_AN=0.,
     +            HEC_DAILY_RISK_PEAK_AN=0.,
     +            HEC_DAILY_RISK_HOURS_AN=0.,
     +            HEC_DAILY_RISK_ENERGY_REV_AN=0.,
     +            HEC_DAILY_NETG_ENERGY_AN=0.,
     +            HEC_DAILY_NETG_PEAK_AN=0.,
     +            HEC_DAILY_NETG_HOURS_AN=0.,
     +            HEC_DAILY_NETG_ENERGY_REV_AN=0.
 !
         INTEGER(kind=2) ::
     +            DAILY_OPTION_ANNUAL_VALUES,
     +            GET_TOTAL_START_UP_UNITS,
     +            TOTAL_START_UP_UNITS,
     +            A,
     +            GET_START_UP_INDEX,
     +            R_INDEX,
     +            R_TG,
     +            GLOBAL_BLOCK_INDEX(:),
     +            SORTED_OPTIONS(:),
     +            FLOOR_SORTED_OPTIONS(:),
     +            CEILING_SORTED_OPTIONS(:),
     +            MUST_RUN_OPTIONS(:),
     +            UNIT_FOR_OUTAGE_BLOCK(:)
         INTEGER(kind=2) ::
     +            S_U_FOR_OUTAGE_BLOCK(:),
     +            BLOCK_FOR_OUTAGE_BLOCK(:),
     +            ANNUAL_HOURS_AVAIL(:),
     +            OUTAGE_BLOCK_BY_SEGMENT(:,:),
     +            LOCAL_BLOCK,
     +            SECOND_BLOCK,
     +            GET_TRANS_UNIT_TO_BLOCK,
     +            MAX_J,
     +            TOTAL_HOURLY_BLOCKS,
     +            Z,
     +            PRICING_BLOCK
         REAL(kind=4) ::
     +            ANNUAL_OPTION_COST(:),
     +            ANNUAL_AVAIL_CAPACITY(:),
     +            ANNUAL_OPTION_MWH(:,:),
     +            ANNUAL_OPTION_HEAT(:,:),
     +            ANNUAL_OPTION_INV_HEAT(:,:),
     +            ANNUAL_OPTION_PAYOFF(:,:),
     +            ANNUAL_DAILY_GROSS(:,:),
     +            ANNUAL_OPTION_STATUS(:),
     +            ANNUAL_OPTION_START_UP_COST(:),
     +            ANNUAL_OPTION_HOURS(:),
     +            ANNUAL_MINIMUM_LOAD_HOURS(:),
     +            ANNUAL_ALL_HOURS_MARGIN(:),
     +            ANNUAL_POSITIVE_HOURS_MARGIN(:),
     +            ANNUAL_POS_CONSEC_MARGIN(:),
     +            ANNUAL_MORNING_CONSEC_MARGIN(:),
     +            ANNUAL_STRIKES(:),
     +            ANNUAL_THREE_DAY_MARGIN(:),
     +            ANNUAL_SEVEN_DAY_MARGIN(:),
     +            GENERATION_BY_SEGMENT(:,:)
         REAL(kind=4) ::
     +            DAILY_CUM_DISPATCH_MW(:,:),
     +            DEPTH_HOURLY_DISPATCH_MW(:),
     +            DAILY_CUM_DISPATCH_COST(:,:),
     +            PRODUCT_GEN_BY_S_U(:,:,:),
     +            PRODUCT_FUEL_BY_S_U(:,:,:),
     +            DAILY_GEN_BY_S_U(:,:),
     +            DAILY_COST_BY_S_U(:,:),
     +            DAILY_SPIN_BY_S_U(:,:),
     +            MONTH_MUST_BY_BLOCK(:,:),
     +            DAILY_MUST_RUN_CAPACITY(:),
     +            DAILY_EMERGENCY_CAPACITY(:),
     +            DAILY_HARD_WIRED_BY_BLOCK(:,:),
     +            INCREMENTAL_FUEL_COST(:),
     +            DAILY_SYSTEM_COST_AND_REV(:,:),
     +            MONTHLY_AC_HEC_REVENUE(:,:,:),
     +            UNIT_GEN_IN_SYSTEM(:,:),
     +            COMBINED_GEN_IN_SYSTEM(:,:),
     +            UNIT_SPIN_IN_SYSTEM(:,:),
     +            UNIT_HEAT_PARAM(:,:)
         REAL(kind=4) ::
     +            COMBINED_HEAT_PARAM(:,:),
     +            DISPATCH_COST_FOR_BLOCK(:),
     +            ALT_DISPATCH_COST_FOR_BLOCK(:),
     +            FLOOR_PRICE_FOR_BLOCK(:),
     +            CEILING_PRICE_FOR_BLOCK(:),
     +            GET_MARKET_FLOOR,MARKET_FLOOR,
     +            GET_MARKET_CEILING,MARKET_CEILING,
     +            RETAIL_REVENUE_FOR_BLOCK(:)

       
         ALLOCATABLE ::
     +            ANNUAL_OPTION_COST,
     +            ANNUAL_HOURS_AVAIL,
     +            ANNUAL_AVAIL_CAPACITY,
     +            ANNUAL_OPTION_MWH,
     +            ANNUAL_OPTION_HEAT,
     +            ANNUAL_OPTION_INV_HEAT,
     +            ANNUAL_OPTION_PAYOFF,
     +            ANNUAL_DAILY_GROSS,
     +            ANNUAL_OPTION_STATUS,
     +            ANNUAL_OPTION_START_UP_COST,
     +            ANNUAL_OPTION_HOURS,
     +            ANNUAL_MINIMUM_LOAD_HOURS,
     +            ANNUAL_ALL_HOURS_MARGIN,
     +            ANNUAL_POSITIVE_HOURS_MARGIN,
     +            ANNUAL_POS_CONSEC_MARGIN,
     +            ANNUAL_MORNING_CONSEC_MARGIN,
     +            ANNUAL_STRIKES,
     +            ANNUAL_THREE_DAY_MARGIN,
     +            ANNUAL_SEVEN_DAY_MARGIN
         ALLOCATABLE ::
     +            UNIT_UP_YESTERDAY,
     +            STRICT_MARKET_RESOURCE,
     +            STRICT_MARKET_W_LOAD_BAL,
     +            DSM_MARKET_RESOURCE,
     +            GRE_MARKET_RESOURCE,
     +            ECITY_MARKET_RESOURCE,
     +            TVA_MARKET_RESOURCE,
     +            MARKET_RESOURCE_COUNTER,
     +            GENERATION_BY_SEGMENT,
     +            DAILY_CUM_DISPATCH_MW,
     +            DEPTH_HOURLY_DISPATCH_MW,
     +            DAILY_CUM_DISPATCH_COST,
     +            PRODUCT_GEN_BY_S_U,
     +            PRODUCT_FUEL_BY_S_U,
     +            DAILY_GEN_BY_S_U,
     +            DAILY_SYSTEM_COST_AND_REV,
     +            DAILY_COST_BY_S_U,
     +            DAILY_SPIN_BY_S_U,
     +            MONTH_MUST_BY_BLOCK
         ALLOCATABLE ::
     +            DAILY_MUST_RUN_CAPACITY,
     +            DAILY_EMERGENCY_CAPACITY,
     +            DAILY_HARD_WIRED_BY_BLOCK,
     +            INCREMENTAL_FUEL_COST,
     +            OPTION_NAME,
     +            CONTRIBUTES_TO_SPIN,
     +            MONTH_UNIT_STARTS,
     +            MONTH_UNIT_DOWNS,
     +            MIN_DOWN_TIME,
     +            IN_MIN_DOWN_TIME_STATE,
     +            IN_MIN_UP_TIME_STATE,
     +            START_UP_COSTS,
     +            EMERGENCY_CAPACITY,
     +            MIN_SPIN_CAP,
     +            MAX_SPIN_CAP,
     +            HOURLY_EMERGENCY_CAPACITY,
     +            MAX_HOURLY_RAMP_UP,
     +            MAX_HOURLY_RAMP_DOWN,
     +            EMERGENCY_HEATRATE
         ALLOCATABLE ::
     +            MONTH_UNIT_START_COST,
     +            MONTH_OPERATING_HOURS,
     +            MUST_RUN_BLOCK,
     +            UNIT_IS_UP,
     +            ORIGINAL_VALUE_OF_U,
     +            INCREASING_VALUE_OF_U,
     +            UNIT_GEN_IN_SYSTEM,
     +            COMBINED_GEN_IN_SYSTEM,
     +            UNIT_SPIN_IN_SYSTEM,
     +            UNIT_HEAT_PARAM,
     +            COMBINED_HEAT_PARAM,
     +            OUTAGE_BLOCK_BY_SEGMENT,
     +            MONTHLY_AC_HEC_REVENUE,
     +            GLOBAL_BLOCK_INDEX,
     +            MIN_UP_TIME,
     +            RAMP_RATE,
     +            RAMP_DOWN_RATE,
     +            START_UP_COST_PER_MWH,
     +            EMERGENCY_COST_PER_MWH
         ALLOCATABLE ::
     +            MINIMUM_UP_INDEX,
     +            SORTED_OPTIONS,
     +            FLOOR_SORTED_OPTIONS,
     +            CEILING_SORTED_OPTIONS,
     +            MUST_RUN_OPTIONS,
     +            UNIT_FOR_OUTAGE_BLOCK,
     +            S_U_FOR_OUTAGE_BLOCK,
     +            BLOCK_FOR_OUTAGE_BLOCK,
     +            DISPATCH_COST_FOR_BLOCK,
     +            ALT_DISPATCH_COST_FOR_BLOCK,
     +            FLOOR_PRICE_FOR_BLOCK,
     +            CEILING_PRICE_FOR_BLOCK,
     +            MARKET_FLOOR_UNIT,
     +            MARKET_CEILING_UNIT,
     +            RETAIL_REVENUE_FOR_BLOCK,
     +            DAILY_OPTION_STATUS,
     +            PRICING_GROUP_SELL_PRICE,
     +            PRICING_GROUP_SELL_NAME,
     +            PRICING_GROUP_SELL_QUANT
         ALLOCATABLE :: PRICING_GROUP_BUY_QUANT_AVAIL,
     +            PRICING_GROUP_SELL_GEN,
     +            PRICING_GROUP_SELL_INDEX,
     +            PRICING_GROUP_BUY_PRICE,
     +            PRICING_GROUP_BUY_NAME,
     +            PRICING_GROUP_BUY_QUANT,
     +            ALL_DEMAND_BUY_GEN,
     +            HOURLY_RESOURCES_AND_MARKETS,
     +            PRICING_GROUP_BUY_INDEX,
     +            ALL_RESOURCE_TYPE,
     +            ALL_RESOURCE_INDEX,
     +            ALL_DEMAND_TYPE,
     +            ALL_DEMAND_INDEX,
     +            ALL_RESOURCE_PRICE,
     +            ALL_RESOURCE_QUANTITY,
     +            ALL_RESOURCE_QUANTITY_USED,
     +            ALL_DEMAND_SERVED,
     +            ALL_DEMAND_PRICE,
     +            ALL_DEMAND_QUANTITY,
     +            ALL_DEMAND_NAME
         INTEGER(kind=2) :: HOURS_IN_MONTH,
     +            NUM_OF_PRICING_GROUPS,
     +            NUM_OF_BUY_PRICING_GROUPS,
     +            LOCAL_PRICING_GROUPS,
     +            LOCAL_BUY_PRICING_GROUPS,
     +            OUTAGE_BLOCK,
     +            LAST_BLOCK,
     +            HOUR,
     +            MAX_OUTAGE_BLOCKS,
     +            MAX_BLOCKS_PER_TRANS(:),
     +            NO_START_UP_UNITS_BY_TG(:),
     +            MAX_START_UP_UNITS_BY_TG,
     +            LOCAL_NUNITS,
     +            BLOCK_1,
     +            BLOCK_2,
     +            MARKET_TG=0,SECOND_MARKET_TG=-1,
     +            PEAK_HOUR,
     +            TEMP_I2,
     +            MARGINAL_SELL_AGAINST_PRICE(0:2)
          INTEGER(kind=2) ::
     +            MARGINAL_BUY_AGAINST_PRICE(0:2),
     +            MARGINAL_UNIT_AGAINST_TARGET(0:2),
     +            MARGINAL_UNIT_AGAINST_LOAD(0:2),
     +            year_loc,
     +            TIME_OF_DAY,
     +            NO_MISC_VARS=33,
     +            R_I,R_J
      
      REAL(kind=4) :: HOUR_TIE_LIMIT,
     +         GET_PRICING_GROUP_SELL_PRICE,
     +         GET_PRICING_GROUP_BUY_PRICE,
     +         GET_SEASON_PATH_LIMIT,
     +         HOURLY_NATIVE_LOAD,
     +         EMERGENCY_TARGET_LOAD,
     +         DECOMMIT_TARGET_LOAD,
     +         GET_TRANS_LOAD_AFTER_EL,
     +         GET_TRANS_HOURLY_HYDRO,
     +         TEMP_POSIT_HYDRO,
     +         AVE_BUY,
     +         AVE_ON_PEAK_BUY,
     +         AVE_OFF_PEAK_BUY,
     +         AVE_SELL,
     +         AVE_ON_PEAK_SELL,
     +         AVE_OFF_PEAK_SELL,
     +         EQUIVALENT_THERMAL,
     +         GET_MONTHLY_TL_MWH,
     +         GET_WH_MONTH_ENERGY
      REAL(kind=4) ::
     +         ENRG,
     +         AVE_MW(3),
     +         ANNUAL_TRANC_REPORT(:,:),
     +         SYSTEM_MONTHLY_MISC(:,:),
     +         SYSTEM_MONTHLY_POSIT(:,:,:),
     +         GET_SYSTEM_PROD_BY_TG_BY_MWH,
     +         SYSTEM_PROD_BY_TG_BY_FUEL(:,:,:),
     +          SYSTEM_MARKET_PRICE(:,:),
     +         SYSTEM_MARKET_REVENUE(:,:),
     +         SYSTEM_MARKET_COST(:,:),
     +         SYSTEM_EMERGENCY_CAPACITY(:,:),
     +         SYSTEM_HOURLY_GENERATION(:,:),
     +         SYSTEM_HOURLY_COMMITMENT(:,:),
     +         SYSTEM_HOURLY_DECOMMITTED(:,:),
     +         SYSTEM_HOURLY_PRE_COMMITMENT(:,:),
     +         SYSTEM_HOURLY_AVAILABLE(:,:),
     +         SYSTEM_HOURLY_COMMIT_AVAIL(:,:),
     +         SYSTEM_HOURLY_LOAD(:,:)
      REAL(kind=4) ::
     +         SYSTEM_HOURLY_HYDRO(:,:),
     +         SYSTEM_HOURLY_STORAGE_PUMP(:,:),
     +         SYSTEM_HOURLY_STORAGE_GEN(:,:),
     +         SYSTEM_HOURLY_MC(:,:),
     +         SYSTEM_HOURLY_LR(:,:),
     +         HOURLY_COMMITMENT_LAMDA,
     +         SYSTEM_HOURLY_MC_AT_LOAD(:,:),
     +         SYSTEM_WHOLESALE_GENERATION(:,:),
     +         SYSTEM_COST_OF_MARKET_SALES(:,:,:),
     +         SYSTEM_NATIVE_COST(:,:),
     +         SYSTEM_UNSERVED_ENERGY(:,:),
     +         SYSTEM_ENERGY_ABOVE(:,:),
     +         SYSTEM_DERIVATIVES(:,:),
     +         SYSTEM_EMISSIONS(:,:,:),
     +         SYSTEM_STORAGE(:,:),
     +         SYSTEM_AVAIL_DERIVATIVES(:,:),
     +         SYSTEM_AVAIL_STORAGE(:,:),
     +         SYSTEM_SPIN(:,:),
     +         SYSTEM_MARKET_BOUGHT(:,:)
      REAL(kind=4) ::
     +         SYSTEM_DSM_MW(:,:),
     +         MARKET_PURCHASE_COST(:,:),
     +         TRANC_DATABASE(:,:,:,:),
     +         JDA_BLOCK_CAPACITY,
     +         JOINT_DISPATCH_ORDER(:,:),
     +         JDA_MARKET_PRICE,
     +         JDA_SELL_PRICE,
     +         JDA_BUY_PRICE,
     +         JDA_COMBINED_LOAD,
     +         JDA_MINIMUM_HOURLY_LOAD(0:2),
     +         JDA_TOTAL_COMBINED_LOAD,
     +         HOURLY_DERIVATIVE_LOAD,
     +         WEEKLY_HYDRO_FOR_HR,
     +         GET_WH_LOADS_PER_HOUR,
     +         HOURLY_ENERGY_ABOVE_RESOURCES,
     +         HOURLY_COMMITMENT_SPIN,
     +         SYSTEM_RETAIL_GENERATION,
     +         LOCAL_RETAIL_GENERATION,
     +         SYSTEM_FIRM_GENERATION
      REAL(kind=4) ::
     +         OUTAGE_CAPACITY,
     +         DUMP_ENERGY(24),
     +         SPINNING_IMPACT,
     +         LOCAL_SPIN(0:800),
     +         AVAILABLE_CAPACITY,
     +         LOCAL_CAPACITY,
     +         HOURLY_MARKET_COST,
     +         TRANSACT_UNSERVED_COST=0.,
     +         GET_TRANSACT_UNSERVED_COST,
     +         TRANSACT_ABUNDANCE_COST,
     +         GET_TRANSACT_ABUNDANCE_COST,
     +         DAILY_OUTAGE_VALUE,
     +         TIE_POWER,
     +         LOCAL_POWER,
     +         LOCAL_NET,
     +         AVAILABLE_NOT_GENERATING,
     +         LOCAL_BALANCE,
     +         SYSTEM_BALANCE,
     +         CENTRAL_DISPATCH_GENERATION(0:2)
      REAL(kind=4) ::
     +         CENTRAL_DISPATCH_TARGET(0:2),
     +         INDIVIDUAL_DISPATCH_TRANSFERS(0:2),
     +         INDIVIDUAL_TRANSFER_DEFICIT(0:2),
     +         JDA_GENERATION(0:2,0:3),
     +         JDA_AVAILABLE_COMMITTED(0:2),
     +         MONTHLY_JDA_GENERATION(0:2,0:3),
     +         JDA_PURCHASES(0:2),
     +         JDA_ENERGY_BALANCE(2),
     +         MONTHLY_JDA_ENERGY_BALANCE(2),
     +         MONTHLY_JDA_PURCHASES(0:2),
     +         JDA_MUST_RUN(2),
     +         C_BY_SYSTEM(2),
     +         MONTHLY_JDA_MUST_RUN(2),
     +         JDA_COST(2),
     +         JDA_MARGINAL_COST(2),
     +         MONTHLY_JDA_COST(2),
     +         JOINT_DISPATCH_CONSTRAINT(0:2,0:2),
     +         TEMP_CAP,
     +         TIE_GROUP_LIMIT(3)
      REAL(kind=4) ::
     +         TIME_OF_DAY_MULT,
     +         MARGIN_UNIT_MW,
     +         GET_CONSTRAINT_MULT,
     +         DECOM_UNIT_PROFIT_TEST,
     +         HOURLY_TRANSACTION_LOAD,
     +         GET_CL_TG_CAP_MARKET_MW,
     +         GET_CL_TG_CAP_MARKET_REV,
     +         GET_TF_TG_CAP_MARKET_MW,
     +         GET_TF_TG_CAP_MARKET_COST,
     +         GET_ANNUAL_INTER_CAPACITY

!               REAL (kind=4) ::   SOX,NOX1,NOX2,CO2,OTH2,OTH3

       real (kind=4), allocatable :: DAILY_FUEL_BY_S_U(:,:)
            ALLOCATABLE ::
     +         ANNUAL_TRANC_REPORT,
     +         SYSTEM_MARKET_PRICE,
     +         SYSTEM_MARKET_REVENUE,
     +         SYSTEM_MONTHLY_MISC,
     +         SYSTEM_MONTHLY_POSIT,
     +         SYSTEM_PROD_BY_TG_BY_FUEL,
     +         SYSTEM_MARKET_COST,
     +         SYSTEM_EMERGENCY_CAPACITY,
     +         MAX_BLOCKS_PER_TRANS,
     +         NO_START_UP_UNITS_BY_TG,
     +         SYSTEM_HOURLY_GENERATION,
     +         SYSTEM_HOURLY_COMMITMENT,
     +         SYSTEM_HOURLY_DECOMMITTED,
     +         SYSTEM_HOURLY_PRE_COMMITMENT,
     +         SYSTEM_HOURLY_AVAILABLE,
     +         SYSTEM_HOURLY_COMMIT_AVAIL,
     +         SYSTEM_HOURLY_LOAD,
     +         SYSTEM_HOURLY_HYDRO
            ALLOCATABLE ::
     +         SYSTEM_HOURLY_STORAGE_PUMP,
     +         SYSTEM_HOURLY_STORAGE_GEN,
     +         SYSTEM_HOURLY_MC,
     +         SYSTEM_HOURLY_MC_AT_LOAD,
     +         SYSTEM_HOURLY_LR,
     +         SYSTEM_WHOLESALE_GENERATION,
     +         SYSTEM_COST_OF_MARKET_SALES,
     +         SYSTEM_NATIVE_COST,
     +         SYSTEM_UNSERVED_ENERGY,
     +         SYSTEM_ENERGY_ABOVE
            ALLOCATABLE ::
     +         SYSTEM_DERIVATIVES,
     +         SYSTEM_EMISSIONS,
     +         SYSTEM_STORAGE,
     +         SYSTEM_AVAIL_DERIVATIVES,
     +         SYSTEM_AVAIL_STORAGE,
     +         SYSTEM_SPIN,
     +         SYSTEM_MARKET_BOUGHT,
     +         SYSTEM_DSM_MW,
     +         MARKET_PURCHASE_COST,
     +         TRANC_DATABASE,
     +         JOINT_DISPATCH_ORDER
!
! FUEL INVENTORY VARIABLES. 3/29/01. GAT.
               INTEGER(kind=2) :: FUEL_ID
               LOGICAL(kind=1) :: UNIT_FUEL_INVENTORY_ACTIVE,
     +                   PRIMARY_FUEL_CHEAPER,
     +                   PRIM_FUEL_SOURCE_EXHUASTED,
     +                   ALL_FUEL_SOURCE_EXHUASTED,
     +                   DYNAMIC_FUEL_CHOICE,
     +                   YES_DYNAMIC_FUEL_PRICING,
     +                   YES_DAILY_CALL_PUT_CAPACITY,
     +                   DAILY_CALL_PUT_CAPACITY
!
               REAL(kind=8) ::    MMBTU_FUEL_BALANCE
!
! RAMP RATE VARIABLES
!
      LOGICAL(kind=1) :: RAMPING_UP,RAMPING_DOWN,RAMP_RATE_ACTIVE
      INTEGER(kind=2) :: LAST_HOUR,
     +            HOURS_TO_RAMP_UP,
     +            HOURS_TO_RAMP_DOWN,
     +            REMAINING_RAMP_UP_HOURS,
     +            REMAINING_RAMP_DOWN_HOURS
      LOGICAL(kind=1) :: LAHEY_LF95
!
!
! 01/14/04.  ELENA ROUTINE
! CALCULATE THE MARGINS FOR THE ENTIRE MONTH BY BLOCK AND THEN FIND THE
! PROFIT MAXIMIZING
! 02/05/04  MODIFICATIONS BY TOM SWEET FOR GREATER ADVANCED LOOK AHEAD
! LOGIC AND MININUM DOWN TIME
!
!       NEW DECLARATIONS
!
!
        INTEGER(KIND=2) ::
     +          MIN_DOWN_TIME_INT,
     +          MAX_HOURS_FOR_ECON_TEST,
     +          HOURS_FOR_ECON_TEST
      logical (kind=1) :: YES_HOURLY_COMMITMENT ! External
        LOGICAL
     +          ADVANCED_LOOK_AHEAD_LOGIC,
     +          POSITIVE_HOURLY_MARGIN,
     +          MAX_RECOVERY_OF_SU_COST,
     +          POS_MARGIN_FOR_UP_TIME,
     +          MIN_DOWN_TIME_SATISFIED,
     +          UP_LAST_HOUR,
     +          MIN_RECOVERY_OF_SU_COST,
     +          MIN_UP_TIME_SATISFIED,
     +          ALWAYS_LOSE_MONEY_FOR_UP_TIME,
     +          ALWAYS_LOSE_MONEY_FOR_MAX_TIME,
     +          KEEP_OFF_FOR_MIN_DOWN_TIME,
     +          UNIT_STARTS_THIS_HOUR

!
! END DATA DECLARATIONS
!
         DAILY_OPTION_OBJECT = .TRUE.
!
!
      RETURN

!
      ENTRY DAILY_OPTION_ANNUAL_VALUES()
!

!
         DAILY_OPTION_ANNUAL_VALUES = GET_TOTAL_START_UP_UNITS()
         TOTAL_START_UP_UNITS = DAILY_OPTION_ANNUAL_VALUES
         PRICE_ONLY_WHOLESALE_REV = APPLY_TRANS_REV_TO_WHOLESALE()
         RETAIL_AS_WHOLESALE = APPLY_RETAIL_AS_WHOLESALE()
!
         DYNAMIC_FUEL_CHOICE = YES_DYNAMIC_FUEL_PRICING()
!
         YES_HOURLY_COMMITMENT = HOURLY_COMMITMENT()
         HOURLY_UNIT_FUEL_REPORT = YES_HOURLY_UNIT_FUEL_REPORT()
         YES_HOURLY_OUTAGE = HOURLY_OUTAGE()
         YES_HOURLY_SPIN_REPORT = HOURLY_SPIN_REPORT()
         YES_DAILY_COMMITMENT = DAILY_COMMITMENT()
         DEPTH_OF_MARKET = DEPTH_OF_MARKET_LOGIC()
         YES_DAILY_DISPATCH_ORDER_RPT = DAILY_DISPATCH_ORDER_REPORT()
!
         UPPER_TRANS_GROUP = GET_NUMBER_OF_ACTIVE_GROUPS()
!
         TRANSACT_UNSERVED_COST = GET_TRANSACT_UNSERVED_COST()
         TRANSACT_ABUNDANCE_COST =
     +                           MAX(0.1,GET_TRANSACT_ABUNDANCE_COST())
!
         IF(TOTAL_START_UP_UNITS > 0) THEN
            IF(ALLOCATED(ANNUAL_OPTION_COST))
     +         DEALLOCATE(ANNUAL_OPTION_COST,ANNUAL_HOURS_AVAIL,
     +            ANNUAL_AVAIL_CAPACITY,ANNUAL_OPTION_MWH,
     +            ANNUAL_OPTION_HEAT,ANNUAL_OPTION_INV_HEAT,
     +            ANNUAL_OPTION_PAYOFF,ANNUAL_DAILY_GROSS,
     +            ANNUAL_OPTION_STATUS,ANNUAL_OPTION_START_UP_COST,
     +            ANNUAL_OPTION_HOURS,ANNUAL_MINIMUM_LOAD_HOURS,
     +            ANNUAL_ALL_HOURS_MARGIN,ANNUAL_POSITIVE_HOURS_MARGIN,
     +            ANNUAL_POS_CONSEC_MARGIN,ANNUAL_MORNING_CONSEC_MARGIN,
     +            ANNUAL_STRIKES,ANNUAL_THREE_DAY_MARGIN,
     +            ANNUAL_SEVEN_DAY_MARGIN,MONTHLY_AC_HEC_REVENUE,
     +            UNIT_UP_YESTERDAY,STRICT_MARKET_RESOURCE,
     +            STRICT_MARKET_W_LOAD_BAL,DSM_MARKET_RESOURCE,
     +            GRE_MARKET_RESOURCE,ECITY_MARKET_RESOURCE,
     +            TVA_MARKET_RESOURCE,ANNUAL_TRANC_REPORT)
            ALLOCATE(
     +            ANNUAL_OPTION_COST(TOTAL_START_UP_UNITS),
     +            ANNUAL_HOURS_AVAIL(TOTAL_START_UP_UNITS),
     +            ANNUAL_AVAIL_CAPACITY(TOTAL_START_UP_UNITS),
     +            ANNUAL_OPTION_MWH(TOTAL_START_UP_UNITS,0:12),
     +            ANNUAL_OPTION_HEAT(TOTAL_START_UP_UNITS,0:12),
     +            ANNUAL_OPTION_INV_HEAT(TOTAL_START_UP_UNITS,0:12),
     +            ANNUAL_OPTION_PAYOFF(TOTAL_START_UP_UNITS,0:12),
     +            ANNUAL_DAILY_GROSS(TOTAL_START_UP_UNITS,0:12),
     +            ANNUAL_OPTION_STATUS(TOTAL_START_UP_UNITS),
     +            ANNUAL_OPTION_START_UP_COST(
     +                                 TOTAL_START_UP_UNITS),
     +            ANNUAL_OPTION_HOURS(TOTAL_START_UP_UNITS),
     +            ANNUAL_MINIMUM_LOAD_HOURS(TOTAL_START_UP_UNITS),
     +            ANNUAL_ALL_HOURS_MARGIN(TOTAL_START_UP_UNITS),
     +            ANNUAL_POSITIVE_HOURS_MARGIN(
     +                                      TOTAL_START_UP_UNITS),
     +            ANNUAL_POS_CONSEC_MARGIN(TOTAL_START_UP_UNITS),
     +            ANNUAL_MORNING_CONSEC_MARGIN(TOTAL_START_UP_UNITS),
     +            ANNUAL_STRIKES(TOTAL_START_UP_UNITS))
            ALLOCATE(
     +            ANNUAL_THREE_DAY_MARGIN(TOTAL_START_UP_UNITS),
     +            ANNUAL_SEVEN_DAY_MARGIN(TOTAL_START_UP_UNITS),
     +            UNIT_UP_YESTERDAY(TOTAL_START_UP_UNITS),
     +            STRICT_MARKET_RESOURCE(TOTAL_START_UP_UNITS),
     +            STRICT_MARKET_W_LOAD_BAL(TOTAL_START_UP_UNITS),
     +            DSM_MARKET_RESOURCE(TOTAL_START_UP_UNITS),
     +            GRE_MARKET_RESOURCE(TOTAL_START_UP_UNITS),
     +            ECITY_MARKET_RESOURCE(TOTAL_START_UP_UNITS),
     +            TVA_MARKET_RESOURCE(TOTAL_START_UP_UNITS),
     +            MONTHLY_AC_HEC_REVENUE(HEC_REV_VARS,3,0:12),
     +            ANNUAL_TRANC_REPORT(MON_TRANC_NUM,
     +                                            0:UPPER_TRANS_GROUP))
!
            ANNUAL_HOURS_AVAIL = 0
            MONTHLY_AC_HEC_REVENUE = 0.
            ANNUAL_OPTION_COST = 0.
            ANNUAL_AVAIL_CAPACITY = 0.
            ANNUAL_OPTION_MWH = 0.
            ANNUAL_OPTION_HEAT = 0.
            ANNUAL_OPTION_INV_HEAT = 0.
            ANNUAL_OPTION_PAYOFF = 0.
            ANNUAL_DAILY_GROSS = 0.
            ANNUAL_OPTION_STATUS = 0.
            ANNUAL_OPTION_START_UP_COST = 0.
            ANNUAL_OPTION_HOURS = 0.
            ANNUAL_MINIMUM_LOAD_HOURS = 0.
            ANNUAL_ALL_HOURS_MARGIN = 0.
            ANNUAL_POSITIVE_HOURS_MARGIN = 0.
            ANNUAL_POS_CONSEC_MARGIN = 0.
            ANNUAL_MORNING_CONSEC_MARGIN = 0.
            ANNUAL_STRIKES = 0.
            ANNUAL_THREE_DAY_MARGIN = 0.
            ANNUAL_SEVEN_DAY_MARGIN = 0.
            ANNUAL_TRANC_REPORT = 0.
            ANNUAL_TRANC_REPORT(43,0:UPPER_TRANS_GROUP) = 999999.

         ENDIF
!
      RETURN

!
      ENTRY HOURLY_COMMITMENT_UNSERVED(R_HOUR,R_TG)
!

!
         HOURLY_COMMITMENT_UNSERVED =
     +                              SYSTEM_UNSERVED_ENERGY(R_HOUR,R_TG)
      RETURN

!
      ENTRY HOURLY_ENERGY_ABOVE_RESOURCES(R_HOUR)
!

!
         HOURLY_ENERGY_ABOVE_RESOURCES =
     +                                 SYSTEM_ENERGY_ABOVE(R_HOUR,R_TG)
      RETURN

!
      ENTRY HOURLY_COMMITMENT_SPIN(R_HOUR,R_TG)
!

!
         HOURLY_COMMITMENT_SPIN = SYSTEM_SPIN(R_HOUR,R_TG)
      RETURN

!
      ENTRY HOURLY_COMMITMENT_GENERATION(R_HOUR,R_TG)
!

!
         HOURLY_COMMITMENT_GENERATION =
     +                            SYSTEM_HOURLY_GENERATION(R_HOUR,R_TG)
      RETURN

!
      ENTRY HOURLY_COMMITMENT_LOAD(R_HOUR,R_TG)
!

!
         HOURLY_COMMITMENT_LOAD = SYSTEM_HOURLY_LOAD(R_HOUR,R_TG)
      RETURN

!
      ENTRY HOURLY_DERIVATIVE_LOAD(R_HOUR,R_TG)
!

!
         HOURLY_DERIVATIVE_LOAD = SYSTEM_DERIVATIVES(R_HOUR,R_TG)
      RETURN

!
      ENTRY HOURLY_COMMITMENT_MC(R_HOUR,R_TG)
!

!
         HOURLY_COMMITMENT_MC = SYSTEM_HOURLY_MC(R_HOUR,R_TG)
      RETURN

!
      ENTRY HOURLY_COMMITMENT_MC_AT_LOAD(R_HOUR,R_TG)
!

!
         HOURLY_COMMITMENT_MC_AT_LOAD =
     +                            SYSTEM_HOURLY_MC_AT_LOAD(R_HOUR,R_TG)
      RETURN

      ENTRY HOURLY_COMMITMENT_LAMDA(R_HOUR,R_TG)

         HOURLY_COMMITMENT_LAMDA = SYSTEM_HOURLY_LR(R_HOUR,R_TG)
      RETURN

!
      ENTRY MONTHLY_OPTION_ENERGY(R_INDEX,R_MONTH)
!

!
         MONTHLY_OPTION_ENERGY = ANNUAL_OPTION_MWH(R_INDEX,R_MONTH)
      RETURN

!
      ENTRY MONTHLY_OPTION_REVENUE(R_INDEX,R_MONTH)
!

!
         MONTHLY_OPTION_REVENUE = ANNUAL_OPTION_PAYOFF(R_INDEX,R_MONTH)
      RETURN

!
      ENTRY HOURLY_NATIVE_COST(R_HOUR,R_TG)
!

!
         HOURLY_NATIVE_COST = SYSTEM_NATIVE_COST(R_HOUR,R_TG)
      RETURN
!
!
! 07/22/03.

!
      ENTRY TRANSACT_C_MONTHLY_INIT(R_HOURS_IN_MONTH)
!

!
!         CALL GET_MAX_TRANS_BLOCKS_USED(MAX_NBLOK)
!
         MAX_NBLOK = 0
         MAX_START_UP_UNITS_BY_TG = 0
         IF(ALLOCATED(MAX_BLOCKS_PER_TRANS))
     +                                 DEALLOCATE(MAX_BLOCKS_PER_TRANS,
     +                                         NO_START_UP_UNITS_BY_TG)
         ALLOCATE(MAX_BLOCKS_PER_TRANS(0:UPPER_TRANS_GROUP),
     +                    NO_START_UP_UNITS_BY_TG(0:UPPER_TRANS_GROUP))
         MAX_BLOCKS_PER_TRANS = 0
         NO_START_UP_UNITS_BY_TG = 0
         DO I = 1, UPPER_TRANS_GROUP
            CALL GET_MAX_BLOCKS_PER_TRANS(I,MAX_BLOCKS_PER_TRANS(I))
            MAX_BLOCKS_PER_TRANS(0) = MAX(MAX_BLOCKS_PER_TRANS(0),
     +                                         MAX_BLOCKS_PER_TRANS(I))
            ! COLLAPSED LIST IN MARGNOBJ
            NO_START_UP_UNITS_BY_TG(I) = GET_NO_START_UP_UNITS(I) 
            NO_START_UP_UNITS_BY_TG(0) = NO_START_UP_UNITS_BY_TG(0) +
     +                                       NO_START_UP_UNITS_BY_TG(I)
            MAX_START_UP_UNITS_BY_TG = MAX(MAX_START_UP_UNITS_BY_TG,
     +                                      NO_START_UP_UNITS_BY_TG(I))
         ENDDO
         MAX_NBLOK = MAX_BLOCKS_PER_TRANS(0)
!
         IF(ALLOCATED(SYSTEM_EMERGENCY_CAPACITY)) DEALLOCATE(
     +         SYSTEM_MARKET_PRICE,SYSTEM_MARKET_REVENUE,
     +         SYSTEM_MONTHLY_MISC,SYSTEM_MONTHLY_POSIT,
     +         SYSTEM_MARKET_COST,SYSTEM_EMERGENCY_CAPACITY,
     +         SYSTEM_HOURLY_GENERATION,SYSTEM_HOURLY_COMMITMENT,
     +         SYSTEM_HOURLY_DECOMMITTED,SYSTEM_HOURLY_PRE_COMMITMENT,
     +         SYSTEM_HOURLY_AVAILABLE,SYSTEM_HOURLY_COMMIT_AVAIL,
     +         SYSTEM_HOURLY_LOAD,SYSTEM_HOURLY_HYDRO,
     +         SYSTEM_HOURLY_STORAGE_PUMP,SYSTEM_HOURLY_STORAGE_GEN,
     +         SYSTEM_HOURLY_MC,SYSTEM_HOURLY_MC_AT_LOAD,
     +         SYSTEM_HOURLY_LR,SYSTEM_WHOLESALE_GENERATION,
     +         SYSTEM_COST_OF_MARKET_SALES,SYSTEM_NATIVE_COST,
     +         SYSTEM_UNSERVED_ENERGY,SYSTEM_ENERGY_ABOVE,
     +         SYSTEM_DERIVATIVES,SYSTEM_EMISSIONS,SYSTEM_STORAGE,
     +         SYSTEM_AVAIL_DERIVATIVES,SYSTEM_SPIN,
     +         SYSTEM_MARKET_BOUGHT,SYSTEM_DSM_MW,SYSTEM_AVAIL_STORAGE,
     +         MARKET_PURCHASE_COST,TRANC_DATABASE)
!

         IF(ALLOCATED(SYSTEM_PROD_BY_TG_BY_MWH))
     +                             DEALLOCATE(SYSTEM_PROD_BY_TG_BY_MWH)

         IF(ALLOCATED(SYSTEM_PROD_BY_TG_BY_FUEL))
     +                            DEALLOCATE(SYSTEM_PROD_BY_TG_BY_FUEL)

         IF(ALLOCATED(PRODUCT_GEN_BY_S_U))
     +                                  DEALLOCATE(PRODUCT_GEN_BY_S_U)
         ALLOCATE(PRODUCT_GEN_BY_S_U(0:MAX_START_UP_UNITS_BY_TG,3,
     +                                            0:UPPER_TRANS_GROUP))
         IF(ALLOCATED(PRODUCT_FUEL_BY_S_U))
     +                                  DEALLOCATE(PRODUCT_FUEL_BY_S_U)
         ALLOCATE(PRODUCT_FUEL_BY_S_U(0:MAX_START_UP_UNITS_BY_TG,3,
     +                                            0:UPPER_TRANS_GROUP))
!
         ALLOCATE (
     +         SYSTEM_EMERGENCY_CAPACITY(0:800,0:UPPER_TRANS_GROUP),
     +         SYSTEM_MARKET_PRICE(0:800,0:UPPER_TRANS_GROUP),
     +         SYSTEM_MARKET_REVENUE(0:800,0:UPPER_TRANS_GROUP),
     +         SYSTEM_MONTHLY_MISC(NO_MISC_VARS,0:UPPER_TRANS_GROUP),
     +         SYSTEM_MONTHLY_POSIT(3,MON_POSIT_NUM,
     +                                          0:UPPER_TRANS_GROUP),
     +         SYSTEM_PROD_BY_TG_BY_MWH(3,0:UPPER_TRANS_GROUP,
     +                                                 MAX_PROD_TYPES),

     +         SYSTEM_PROD_BY_TG_BY_FUEL(3,0:UPPER_TRANS_GROUP,
     +                                                 MAX_PROD_TYPES),
     +         SYSTEM_MARKET_COST(0:800,0:UPPER_TRANS_GROUP),
     +         SYSTEM_HOURLY_GENERATION(0:800,0:UPPER_TRANS_GROUP),
     +         SYSTEM_HOURLY_COMMITMENT(0:800,0:UPPER_TRANS_GROUP),
     +         SYSTEM_HOURLY_DECOMMITTED(0:800,0:UPPER_TRANS_GROUP),
     +         SYSTEM_HOURLY_PRE_COMMITMENT(0:800,0:UPPER_TRANS_GROUP),
     +         SYSTEM_HOURLY_AVAILABLE(0:800,0:UPPER_TRANS_GROUP),
     +         SYSTEM_HOURLY_COMMIT_AVAIL(0:800,0:UPPER_TRANS_GROUP),
     +         SYSTEM_HOURLY_LOAD(0:800,0:UPPER_TRANS_GROUP),
     +         SYSTEM_HOURLY_HYDRO(0:800,0:UPPER_TRANS_GROUP))
         ALLOCATE (
     +         SYSTEM_HOURLY_STORAGE_PUMP(0:800,UPPER_TRANS_GROUP),
     +         SYSTEM_HOURLY_STORAGE_GEN(0:800,UPPER_TRANS_GROUP),
     +         SYSTEM_HOURLY_MC(0:800,0:UPPER_TRANS_GROUP),
     +         SYSTEM_HOURLY_MC_AT_LOAD(0:800,0:UPPER_TRANS_GROUP),
     +         SYSTEM_HOURLY_LR(0:800,0:UPPER_TRANS_GROUP))
         ALLOCATE (
     +         SYSTEM_WHOLESALE_GENERATION(0:800,0:UPPER_TRANS_GROUP),
     +        SYSTEM_COST_OF_MARKET_SALES(0:800,0:UPPER_TRANS_GROUP,3),
     +         SYSTEM_NATIVE_COST(0:800,0:UPPER_TRANS_GROUP),
     +         SYSTEM_UNSERVED_ENERGY(0:800,0:UPPER_TRANS_GROUP),
     +         SYSTEM_ENERGY_ABOVE(0:800,0:UPPER_TRANS_GROUP),
     +         SYSTEM_DERIVATIVES(0:800,0:UPPER_TRANS_GROUP),
     +         SYSTEM_EMISSIONS(0:800,0:UPPER_TRANS_GROUP,5),
     +         SYSTEM_STORAGE(0:800,0:UPPER_TRANS_GROUP))
         ALLOCATE (
     +         SYSTEM_AVAIL_DERIVATIVES(0:800,0:UPPER_TRANS_GROUP),
     +         SYSTEM_AVAIL_STORAGE(0:800,0:UPPER_TRANS_GROUP),
     +         SYSTEM_SPIN(0:800,0:UPPER_TRANS_GROUP),
     +         SYSTEM_MARKET_BOUGHT(0:800,0:UPPER_TRANS_GROUP))
         ALLOCATE (
     +         SYSTEM_DSM_MW(0:800,0:UPPER_TRANS_GROUP),
     +         MARKET_PURCHASE_COST(0:800,0:UPPER_TRANS_GROUP))

!
         ALLOCATE (TRANC_DATABASE(1,1,1,1))

         TRANC_DATABASE = 0.
!
         SYSTEM_HOURLY_GENERATION = 0.
         SYSTEM_EMERGENCY_CAPACITY = 0.
         SYSTEM_MARKET_PRICE = 0.
         SYSTEM_MARKET_REVENUE = 0.
         SYSTEM_MARKET_COST = 0.
         SYSTEM_HOURLY_LOAD = 0.
         SYSTEM_HOURLY_HYDRO = 0.
         SYSTEM_HOURLY_STORAGE_PUMP = 0.
         SYSTEM_HOURLY_STORAGE_GEN = 0.
         SYSTEM_HOURLY_MC = 0.
         SYSTEM_HOURLY_MC_AT_LOAD = TRANSACT_UNSERVED_COST
         SYSTEM_HOURLY_LR = 0.
         SYSTEM_UNSERVED_ENERGY = 0.
         SYSTEM_ENERGY_ABOVE = 0.
         SYSTEM_DERIVATIVES = 0.
         SYSTEM_EMISSIONS = 0.
         SYSTEM_AVAIL_DERIVATIVES = 0.
         SYSTEM_STORAGE = 0.
         SYSTEM_AVAIL_STORAGE = 0.
         SYSTEM_SPIN = 0.
         SYSTEM_HOURLY_COMMITMENT = 0.
         SYSTEM_HOURLY_DECOMMITTED = 0.
         SYSTEM_HOURLY_PRE_COMMITMENT = 0.
         SYSTEM_HOURLY_AVAILABLE = 0.
         SYSTEM_HOURLY_COMMIT_AVAIL = 0.
         SYSTEM_WHOLESALE_GENERATION = 0.
         SYSTEM_COST_OF_MARKET_SALES = 0.
         SYSTEM_NATIVE_COST = 0.
         SYSTEM_MARKET_BOUGHT = 0.
         SYSTEM_DSM_MW = 0.
         MARKET_PURCHASE_COST = 0.
!
         SYSTEM_MONTHLY_MISC = 0.
         SYSTEM_MONTHLY_POSIT = 0.
         SYSTEM_PROD_BY_TG_BY_MWH = 0.
!         SYSTEM_PROD_BY_TG_BY_MW = 0.
         SYSTEM_PROD_BY_TG_BY_FUEL = 0.
!
         PRODUCT_GEN_BY_S_U = 0.
         PRODUCT_FUEL_BY_S_U = 0.
         PRODUCT_HOURS_POSIT = 0.
!
         TRANSACT_C_MONTHLY_INIT = .TRUE.
!
!
      RETURN

!
      ENTRY DAILY_OPTION_FOR_MONTH(R_DAYS_IN_MONTH,
     +                             R_MONTH_NAME,
     +                             R_DATA_BASE,
     +                             R_MONTHLY_PRICE,
     +                             R_MONTH,
     +                             R_TG,
     +                             R_TRANS_ROR_CAPACITY,
     +                             R_TRANS_SPINNING)
!

!
! PERFORMS COMMITMENT LOGIC FOR ALL UNITS IN A TRANSACTION GROUP.
!
         TRANC_SCREEN_MESSAGES = .FALSE.
         IF(TRANC_SCREEN_MESSAGES) THEN
            SCREEN_MESSAGES = 'Top of Transact C'
            CALL MG_LOCATE_WRITE(12,26,
     +                           trim(SCREEN_MESSAGES),ALL_VERSIONS,2)
         ENDIF
         ! COLLAPSED LIST IN MARGNOBJ
         NO_START_UP_UNITS = GET_NO_START_UP_UNITS(R_TG) 
!
         DAILY_OPTION_FOR_MONTH = .FALSE.
         SAVE_TRANSACT_C_STATUS = .FALSE.
!
         IF(NO_START_UP_UNITS == 0) RETURN ! NO START_UP UNITS
!
         THIS_YEAR = FLOAT(get_BASE_YEAR() + globecom_YEAR)
!
         TG = R_TG
         SYSTEM_MONTHLY_MISC(12,TG) = 999999. ! BASE LOAD
         SYSTEM_MONTHLY_POSIT(:,15,TG)= 999999.

         IF(.NOT. LAHEY_LF95())
     +            CALL MG_LOCATE_WRITE(17,9,'Transact C              ',
     +                                                  ALL_VERSIONS,0)
         TYPE_OF_OPTION = 1 ! HARD-WIRED FOR NOW.
         IF(TYPE_OF_OPTION == 1) THEN ! PHYSICAL
!
         HOURS_IN_MONTH = 24 * R_DAYS_IN_MONTH
         if(HOURS_IN_MONTH/=744) then
            call end_program("tf_objt2:0001 - HOURS_IN_MONTH was " //
     +  "calculated incorrectly. Value should be 744 and is " //
     +  trim(itos(int(hours_in_month))))
     
         end if
!
         IF(DEPTH_OF_MARKET) THEN
            DEPTH_OF_MARKET_DB_ACTIVE =
     + MONTHLY_DEPTH_OF_MARKET_DB(
     +        gc_end_point,globecom_YEAR,R_MONTH,R_TG,R_DAYS_IN_MONTH,
     +        L_MONTHLY_PRICE)
         ELSE
            if(hours_in_month>744) then
                call end_program ("tf_objt:0001 - hours_in_month " //
     + "value of " // trim(itos(int(hours_in_month))) // 
     + " is invalid.")
            end if
            DO HR = 1, HOURS_IN_MONTH
               L_MONTHLY_PRICE(HR) = R_MONTHLY_PRICE(HR)
            ENDDO
         ENDIF
!
         HOOSIER = YES_HOOSIER()
         ECITIES = YES_ECITIES_UNITS_ACTIVE()
!
         IF(TRANC_SCREEN_MESSAGES) THEN
            SCREEN_MESSAGES = 'TransC Above First Multi Call'
            CALL MG_LOCATE_WRITE(12,26,
     +                           trim(SCREEN_MESSAGES),ALL_VERSIONS,2)
         ENDIF
!
         MULTI_MARKET_ACTIVE = YES_MULTI_MARKET_ACTIVE()
!
         IF(MULTI_MARKET_ACTIVE) THEN
            IF(TRANC_SCREEN_MESSAGES) THEN
               SCREEN_MESSAGES = 'TransC Above Sell Multi'
               CALL MG_LOCATE_WRITE(12,26,
     +                           trim(SCREEN_MESSAGES),ALL_VERSIONS,2)
            ENDIF
            if(hours_in_month>744) then
                call end_program ("tf_objt:0002 - hours_in_month " //
     + "value of " // trim(itos(int(hours_in_month))) // 
     + " is invalid.")
            end if
            DO HR = 1, HOURS_IN_MONTH
               L_MONTHLY_PRICE(HR) = R_MONTHLY_PRICE(HR)
            ENDDO

            TEMP_L = MULTI_AREA_PRICING_FOR_MONTH(HOURS_IN_MONTH,
     +                                         R_MONTH,
     +               globecom_YEAR,
     +                                         R_TG,
     +                                         NUM_OF_PRICING_GROUPS)
            IF(TRANC_SCREEN_MESSAGES) THEN
               SCREEN_MESSAGES = 'TransC Above Buy Multi'
               CALL MG_LOCATE_WRITE(12,26,
     +                           trim(SCREEN_MESSAGES),ALL_VERSIONS,2)
            ENDIF
            if(hours_in_month>744) then
                call end_program ("tf_objt:0003 - hours_in_month " //
     + "value of " // trim(itos(int(hours_in_month))) // 
     + " is invalid.")
            end if
            TEMP_L = MULTI_AREA_BUY_PRICE_FOR_MONTH(
     +                                        HOURS_IN_MONTH,
     +                                        R_MONTH,
     +                                        globecom_YEAR,
     +                                        R_TG,
     +                                       NUM_OF_BUY_PRICING_GROUPS)
            IF(TRANC_SCREEN_MESSAGES) THEN
               SCREEN_MESSAGES = 'TransC Below Buy Multi'
               CALL MG_LOCATE_WRITE(12,26,
     +                           trim(SCREEN_MESSAGES),ALL_VERSIONS,2)
            ENDIF
         ELSE
            NUM_OF_PRICING_GROUPS = 0
            NUM_OF_BUY_PRICING_GROUPS = 0
         ENDIF
!
!
         TEMP_L = INIT_MONTH_ALLOC_BLOCKS_2_CUST(R_TG)
!
         LOCAL_PRICING_GROUPS = MAX(NUM_OF_PRICING_GROUPS,int(1,2))
         LOCAL_BUY_PRICING_GROUPS = 
     +                MAX(NUM_OF_BUY_PRICING_GROUPS,int(1,2))
!
         DECOMMIT_THERMAL_RESOURCES = YES_DECOMMIT_THERMAL_RESOURCES()
!
         COMMIT_ON_TOTAL_COST = YES_COMMIT_ON_TOTAL_COST()
         EMERGENCY_MEETS_SPIN = YES_EMERGENCY_MEETS_SPIN()
         BLOCKS_2_CUSTOMERS_REPORT  = YES_MONTHLY_LOAD_N_RESOURCES()
         ALLOC_COSTS_BY_UNIT = GET_ALLOC_COSTS_BY_UNIT()
!
         YES_ALLOW_MARKET_ARBITRAGE = ALLOW_MARKET_ARBITRAGE()
!
!
!         TEMP_I = GET_DATA_BASE_BY_HOUR(R_TG,DATA_BASE_BY_HOUR)
!
!        ASSUME PHYSICAL FROM THE CAPACITY LIMITED FILE
!
         MAX_OUTAGE_BLOCKS = NO_START_UP_UNITS*2
         ALL_RESOURCE_NUMBER = MAX_OUTAGE_BLOCKS + LOCAL_PRICING_GROUPS
         ! 091504 ! LOCAL_BUY_PRICING_GROUPS + 1 ! 1 IS FOR RETAIL
         ALL_DEMAND_NUMBER = NUM_OF_BUY_PRICING_GROUPS + 1 
!
         IF(TRANC_SCREEN_MESSAGES) THEN
            SCREEN_MESSAGES = 'Transact C Above Alloc'
            CALL MG_LOCATE_WRITE(12,26,
     +                           trim(SCREEN_MESSAGES),ALL_VERSIONS,2)
         ENDIF
!
         IF(ALLOCATED(GENERATION_BY_SEGMENT)) THEN
          DEALLOCATE(GENERATION_BY_SEGMENT,
     +    DAILY_CUM_DISPATCH_MW,DEPTH_HOURLY_DISPATCH_MW,
     +    DAILY_CUM_DISPATCH_COST,DAILY_GEN_BY_S_U,
     +    DAILY_FUEL_BY_S_U,DAILY_SYSTEM_COST_AND_REV,
     +    DAILY_COST_BY_S_U,DAILY_SPIN_BY_S_U,
     +    MONTH_MUST_BY_BLOCK,DAILY_MUST_RUN_CAPACITY,
     +    DAILY_EMERGENCY_CAPACITY,DAILY_HARD_WIRED_BY_BLOCK,
     +    INCREMENTAL_FUEL_COST,OPTION_NAME,
     +    CONTRIBUTES_TO_SPIN,MONTH_UNIT_STARTS,
     +    MONTH_UNIT_DOWNS,IN_MIN_DOWN_TIME_STATE,
     +    IN_MIN_UP_TIME_STATE,START_UP_COSTS,
     +    EMERGENCY_CAPACITY,MIN_SPIN_CAP,MAX_SPIN_CAP,
     +    HOURLY_EMERGENCY_CAPACITY,MAX_HOURLY_RAMP_UP,
     +    MAX_HOURLY_RAMP_DOWN,EMERGENCY_HEATRATE,
     +    MONTH_UNIT_START_COST,MONTH_OPERATING_HOURS,
     +    MUST_RUN_BLOCK,UNIT_IS_UP,ORIGINAL_VALUE_OF_U,
     +    INCREASING_VALUE_OF_U,GLOBAL_BLOCK_INDEX,
     +    MIN_UP_TIME,MIN_DOWN_TIME,RAMP_RATE,RAMP_DOWN_RATE,
     +    START_UP_COST_PER_MWH,EMERGENCY_COST_PER_MWH)
          DEALLOCATE(
     +    MINIMUM_UP_INDEX,SORTED_OPTIONS,FLOOR_SORTED_OPTIONS,
     +    CEILING_SORTED_OPTIONS,MUST_RUN_OPTIONS,
     +    UNIT_FOR_OUTAGE_BLOCK,S_U_FOR_OUTAGE_BLOCK,
     +    BLOCK_FOR_OUTAGE_BLOCK,DISPATCH_COST_FOR_BLOCK,
     +    ALT_DISPATCH_COST_FOR_BLOCK,FLOOR_PRICE_FOR_BLOCK,
     +    CEILING_PRICE_FOR_BLOCK,MARKET_FLOOR_UNIT,
     +    MARKET_CEILING_UNIT,RETAIL_REVENUE_FOR_BLOCK,
     +    MARKET_RESOURCE_COUNTER,DAILY_OPTION_STATUS,
     +    PRICING_GROUP_SELL_PRICE,PRICING_GROUP_SELL_NAME,
     +    PRICING_GROUP_SELL_QUANT,PRICING_GROUP_SELL_QUANT_AVAIL,
     +    PRICING_GROUP_BUY_QUANT_AVAIL,
     +    PRICING_GROUP_SELL_GEN,PRICING_GROUP_SELL_INDEX,
     +    PRICING_GROUP_BUY_PRICE,PRICING_GROUP_BUY_NAME,
     +    PRICING_GROUP_BUY_QUANT,ALL_DEMAND_BUY_GEN,
     +    HOURLY_RESOURCES_AND_MARKETS,PRICING_GROUP_BUY_INDEX,
     +    ALL_RESOURCE_TYPE,ALL_RESOURCE_INDEX,ALL_DEMAND_TYPE,
     +    ALL_DEMAND_INDEX,ALL_RESOURCE_PRICE,ALL_RESOURCE_QUANTITY,
     +    ALL_DEMAND_SERVED,ALL_DEMAND_PRICE,ALL_RESOURCE_QUANTITY_USED,
     +    ALL_DEMAND_QUANTITY,ALL_DEMAND_NAME)
         ENDIF
!
         IF(ALLOCATED(UNIT_GEN_IN_SYSTEM))
     +              DEALLOCATE (UNIT_GEN_IN_SYSTEM,UNIT_SPIN_IN_SYSTEM,
     +                           UNIT_HEAT_PARAM,
     +                           OUTAGE_BLOCK_BY_SEGMENT)
!
         
       ALLOCATE (GENERATION_BY_SEGMENT(MAX_OUTAGE_BLOCKS,800))
       allocate(DAILY_CUM_DISPATCH_MW(MAX_OUTAGE_BLOCKS,24))
       
      allocate(DEPTH_HOURLY_DISPATCH_MW(MAX_OUTAGE_BLOCKS))
      allocate(DAILY_CUM_DISPATCH_COST(MAX_OUTAGE_BLOCKS,24))
      allocate(DAILY_GEN_BY_S_U(0:NO_START_UP_UNITS,24))
      allocate(DAILY_FUEL_BY_S_U(
     + 0:NO_START_UP_UNITS+max_fuel_types_tfo,24))
     
      allocate(DAILY_SYSTEM_COST_AND_REV(0:2,24))
      allocate(DAILY_COST_BY_S_U(0:NO_START_UP_UNITS,24))
      allocate(DAILY_SPIN_BY_S_U(0:NO_START_UP_UNITS,24))
      allocate(MONTH_MUST_BY_BLOCK(MAX_OUTAGE_BLOCKS,800))
      allocate(DAILY_MUST_RUN_CAPACITY(24))
      allocate(DAILY_EMERGENCY_CAPACITY(24))
      allocate(DAILY_HARD_WIRED_BY_BLOCK(24,MAX_OUTAGE_BLOCKS))
      allocate(INCREMENTAL_FUEL_COST(MAX_OUTAGE_BLOCKS))
      allocate(OPTION_NAME(0:NO_START_UP_UNITS))
      allocate(CONTRIBUTES_TO_SPIN(NO_START_UP_UNITS))
      allocate(MONTH_UNIT_STARTS(NO_START_UP_UNITS))
      allocate(MONTH_UNIT_DOWNS(NO_START_UP_UNITS))
      allocate(IN_MIN_DOWN_TIME_STATE(NO_START_UP_UNITS,800))
      allocate(IN_MIN_UP_TIME_STATE(NO_START_UP_UNITS,800))
      allocate(START_UP_COSTS(NO_START_UP_UNITS))
      allocate(EMERGENCY_CAPACITY(NO_START_UP_UNITS))
      allocate(MIN_SPIN_CAP(NO_START_UP_UNITS))
      allocate(MAX_SPIN_CAP(NO_START_UP_UNITS))
      allocate(HOURLY_EMERGENCY_CAPACITY(NO_START_UP_UNITS))
      allocate(MAX_HOURLY_RAMP_UP(0:MAX_OUTAGE_BLOCKS))
      allocate(MAX_HOURLY_RAMP_DOWN(0:MAX_OUTAGE_BLOCKS))
      allocate(EMERGENCY_HEATRATE(NO_START_UP_UNITS))
      allocate(MONTH_UNIT_START_COST(NO_START_UP_UNITS))
      allocate(MONTH_OPERATING_HOURS(NO_START_UP_UNITS))
      allocate(MUST_RUN_BLOCK(MAX_OUTAGE_BLOCKS))
      allocate(UNIT_IS_UP(NO_START_UP_UNITS))
      allocate(ORIGINAL_VALUE_OF_U(NO_START_UP_UNITS))
      allocate(INCREASING_VALUE_OF_U(NO_START_UP_UNITS))
      allocate(GLOBAL_BLOCK_INDEX(MAX_OUTAGE_BLOCKS))
      allocate(MIN_UP_TIME(NO_START_UP_UNITS))
      allocate(MIN_DOWN_TIME(NO_START_UP_UNITS))
      allocate(RAMP_RATE(NO_START_UP_UNITS))
      allocate(RAMP_DOWN_RATE(NO_START_UP_UNITS))
      allocate(START_UP_COST_PER_MWH(NO_START_UP_UNITS))
      allocate(EMERGENCY_COST_PER_MWH(NO_START_UP_UNITS))
      allocate(MINIMUM_UP_INDEX(NO_START_UP_UNITS))
      allocate(SORTED_OPTIONS(MAX_OUTAGE_BLOCKS))
      allocate(FLOOR_SORTED_OPTIONS(MAX_OUTAGE_BLOCKS))
      allocate(CEILING_SORTED_OPTIONS(MAX_OUTAGE_BLOCKS))
      allocate(MUST_RUN_OPTIONS(MAX_OUTAGE_BLOCKS))
      allocate(UNIT_FOR_OUTAGE_BLOCK(MAX_OUTAGE_BLOCKS))
      allocate(S_U_FOR_OUTAGE_BLOCK(MAX_OUTAGE_BLOCKS))
      allocate(BLOCK_FOR_OUTAGE_BLOCK(MAX_OUTAGE_BLOCKS))
      allocate(DISPATCH_COST_FOR_BLOCK(MAX_OUTAGE_BLOCKS))
      allocate(ALT_DISPATCH_COST_FOR_BLOCK(MAX_OUTAGE_BLOCKS))
      allocate(FLOOR_PRICE_FOR_BLOCK(MAX_OUTAGE_BLOCKS))
      allocate(CEILING_PRICE_FOR_BLOCK(MAX_OUTAGE_BLOCKS))
      allocate(MARKET_FLOOR_UNIT(NO_START_UP_UNITS))
      allocate(MARKET_CEILING_UNIT(NO_START_UP_UNITS))
      allocate(RETAIL_REVENUE_FOR_BLOCK(MAX_OUTAGE_BLOCKS))
      allocate(UNIT_GEN_IN_SYSTEM(2,0:NO_START_UP_UNITS))
      allocate(UNIT_HEAT_PARAM(2,0:NO_START_UP_UNITS))
      allocate(UNIT_SPIN_IN_SYSTEM(2,0:NO_START_UP_UNITS))
      allocate(OUTAGE_BLOCK_BY_SEGMENT(2,NO_START_UP_UNITS))
      allocate(MARKET_RESOURCE_COUNTER(TOTAL_START_UP_UNITS))
      allocate(DAILY_OPTION_STATUS(0:MAX_DAYS,NO_START_UP_UNITS))
      allocate(PRICING_GROUP_SELL_PRICE(
     +       0:LOCAL_PRICING_GROUPS,24))
      allocate(PRICING_GROUP_SELL_NAME(LOCAL_PRICING_GROUPS))
      allocate(PRICING_GROUP_SELL_QUANT(0:LOCAL_PRICING_GROUPS))
      
      allocate(PRICING_GROUP_SELL_QUANT_AVAIL(
     + 0:LOCAL_PRICING_GROUPS,24))

      allocate(PRICING_GROUP_BUY_QUANT_AVAIL(
     + 0:LOCAL_BUY_PRICING_GROUPS,24))
     
      allocate(PRICING_GROUP_SELL_GEN(0:LOCAL_PRICING_GROUPS,24))
      allocate(PRICING_GROUP_SELL_INDEX(LOCAL_PRICING_GROUPS))
      
      
      allocate(PRICING_GROUP_BUY_PRICE(
     + LOCAL_BUY_PRICING_GROUPS,24))
      allocate(PRICING_GROUP_BUY_NAME(
     +   LOCAL_BUY_PRICING_GROUPS))
      allocate(PRICING_GROUP_BUY_QUANT(0:
     + LOCAL_BUY_PRICING_GROUPS))
     
      allocate(PRICING_GROUP_BUY_INDEX(LOCAL_BUY_PRICING_GROUPS))
      allocate(ALL_RESOURCE_TYPE(ALL_RESOURCE_NUMBER))
      allocate(ALL_RESOURCE_INDEX(ALL_RESOURCE_NUMBER))
      allocate(ALL_DEMAND_TYPE(ALL_DEMAND_NUMBER))
      allocate(ALL_DEMAND_INDEX(ALL_DEMAND_NUMBER))
      allocate(ALL_RESOURCE_PRICE(ALL_RESOURCE_NUMBER))
      allocate(ALL_RESOURCE_QUANTITY(ALL_RESOURCE_NUMBER))
      allocate(ALL_RESOURCE_QUANTITY_USED(ALL_RESOURCE_NUMBER))
      allocate(ALL_DEMAND_SERVED(0:LOCAL_PRICING_GROUPS,
     + LOCAL_BUY_PRICING_GROUPS))

      allocate(ALL_DEMAND_PRICE(ALL_DEMAND_NUMBER))
      allocate(ALL_DEMAND_QUANTITY(ALL_DEMAND_NUMBER))
      allocate(ALL_DEMAND_NAME(ALL_DEMAND_NUMBER))
      allocate(ALL_DEMAND_BUY_GEN(0:ALL_DEMAND_NUMBER,24))
      allocate(HOURLY_RESOURCES_AND_MARKETS(
     +        0:ALL_RESOURCE_NUMBER, 0:ALL_DEMAND_NUMBER))
     
         IF(TRANC_SCREEN_MESSAGES) THEN
            SCREEN_MESSAGES = 'Transact C Below Alloc'
            CALL MG_LOCATE_WRITE(12,26,
     +                           trim(SCREEN_MESSAGES),ALL_VERSIONS,2)
         ENDIF
!
         UNIT_FOR_OUTAGE_BLOCK = 0
!
         PRICING_GROUP_BUY_PRICE = 0.
         PRICING_GROUP_BUY_QUANT = 0.
         PRICING_GROUP_BUY_INDEX = 0
!
         PRICING_GROUP_SELL_PRICE = 999999999.
         PRICING_GROUP_SELL_QUANT = 0.
         PRICING_GROUP_SELL_INDEX = 0
!
         ALL_RESOURCE_TYPE = 0
         ALL_RESOURCE_INDEX = 0
!
         ALL_RESOURCE_PRICE = 0.
         ALL_RESOURCE_QUANTITY = 0.
!
         ALL_DEMAND_TYPE = 0
         ALL_DEMAND_INDEX = 0
         ALL_DEMAND_PRICE = 0.
         ALL_DEMAND_QUANTITY = 0.
!
         RETAIL_REVENUE_FOR_BLOCK = 0.
!
         MONTH_UNIT_STARTS = 0
         MONTH_UNIT_DOWNS = 0
         IN_MIN_DOWN_TIME_STATE = 0
         IN_MIN_UP_TIME_STATE = 0
         START_UP_COSTS = 0.
!
         EMERGENCY_CAPACITY = 0.
!
         MIN_SPIN_CAP = 0.
         MAX_SPIN_CAP = 0.
!
         INCREMENTAL_FUEL_COST = 0. ! 01/06/04
!
         EMERGENCY_HEATRATE = 0.
         EMERGENCY_COST_PER_MWH = 0.
!
         MONTH_UNIT_START_COST = 0.
         MUST_RUN_BLOCK = 0
         UNIT_IS_UP = 0
!
         DAILY_OPTION_STATUS = 0
!
!
         UNIT_GEN_IN_SYSTEM = 0.
         UNIT_HEAT_PARAM = 0.
         UNIT_SPIN_IN_SYSTEM = 0.
         OUTAGE_BLOCK_BY_SEGMENT = 0
!
         PRODUCT_PRICE = 0.
         PRODUCT_MARGINAL_COST = 0.
         PRODUCT_HOURS = 0.
         PRODUCT_QUANTITY = 0.
!
         TEMP_L = GET_ONE_TRANS_RAMP_RATES(
     +                      R_TG,
     +                      R_MONTH,
     +                      SYSTEM_RAMP_UP,
     +                      SYSTEM_RAMP_DOWN)
!
!
! FOR LACK OF AN EASY WAY TO TURN THESE OFF WITH MULTI-MARKET
!
         IF(.NOT. MULTI_MARKET_ACTIVE) THEN
            PEAK_HOUR = 1
!
            VOID_LOGICAL = INIT_HOUR_PATH_LIMIT(PEAK_HOUR,
     + R_MONTH,globecom_YEAR)
!
       ON_PEAK_BUY_SPREAD = PATH_SPREAD_RATE(MARKET_TG,R_TG,
     +       PEAK_HOUR,R_MONTH,globecom_YEAR)
       ON_PEAK_SELL_SPREAD = PATH_SPREAD_RATE(R_TG,MARKET_TG,
     +    PEAK_HOUR,R_MONTH,globecom_YEAR)
       ON_PEAK_BUY_WHEEL =
     +   MAX(PATH_WHEELING_CHARGE(MARKET_TG,R_TG,
     +    PEAK_HOUR,R_MONTH,globecom_YEAR),
     +           ON_PEAK_BUY_SPREAD)
            ON_PEAK_SELL_WHEEL = MAX(PATH_WHEELING_CHARGE(R_TG,
     +                 MARKET_TG,
     +         PEAK_HOUR,R_MONTH,globecom_YEAR),
     +              ON_PEAK_SELL_SPREAD)
            ON_PEAK_SECOND_BUY_WHEEL =
     + MAX(PATH_WHEELING_CHARGE(SECOND_MARKET_TG,R_TG,
     +                           PEAK_HOUR,R_MONTH,globecom_YEAR),
     +                                 ON_PEAK_BUY_SPREAD)
            ON_PEAK_SECOND_SELL_WHEEL =
     + MAX(PATH_WHEELING_CHARGE(R_TG,SECOND_MARKET_TG,
     +                           PEAK_HOUR,R_MONTH,globecom_YEAR),
     +                                ON_PEAK_SELL_SPREAD)
!
            PEAK_HOUR = 2
!
            VOID_LOGICAL = INIT_HOUR_PATH_LIMIT(PEAK_HOUR,R_MONTH,
     + globecom_YEAR)
!
            OFF_PEAK_BUY_SPREAD = PATH_SPREAD_RATE(MARKET_TG,R_TG,
     +          PEAK_HOUR,R_MONTH,globecom_YEAR)
            OFF_PEAK_SELL_SPREAD = PATH_SPREAD_RATE(R_TG,MARKET_TG,
     +                 PEAK_HOUR,R_MONTH,globecom_YEAR)
            OFF_PEAK_BUY_WHEEL =
     +              MAX(PATH_WHEELING_CHARGE(MARKET_TG,R_TG,
     +                       PEAK_HOUR,R_MONTH,globecom_YEAR),
     +                                OFF_PEAK_BUY_SPREAD)
            OFF_PEAK_SELL_WHEEL = MAX(
     +                             PATH_WHEELING_CHARGE(R_TG,MARKET_TG,
     +                     PEAK_HOUR,R_MONTH,globecom_YEAR),
     +                             OFF_PEAK_SELL_SPREAD)
            OFF_PEAK_SECOND_BUY_WHEEL =
     +              MAX(PATH_WHEELING_CHARGE(SECOND_MARKET_TG,R_TG,
     +                    PEAK_HOUR,R_MONTH,globecom_YEAR),
     +                          OFF_PEAK_BUY_SPREAD)
            OFF_PEAK_SECOND_SELL_WHEEL =
     +                      MAX(PATH_WHEELING_CHARGE(R_TG,
     +                                        SECOND_MARKET_TG,
     +    PEAK_HOUR,R_MONTH,globecom_YEAR),
     +                   OFF_PEAK_SELL_SPREAD)
         ELSE
            ON_PEAK_BUY_SPREAD = 0.
            ON_PEAK_SELL_SPREAD = 0.
            ON_PEAK_BUY_WHEEL = 0.
            ON_PEAK_SELL_WHEEL = 0.
            ON_PEAK_SECOND_BUY_WHEEL = 0.
            ON_PEAK_SECOND_SELL_WHEEL = 0.
            OFF_PEAK_BUY_SPREAD = 0.
            OFF_PEAK_SELL_SPREAD = 0.
            OFF_PEAK_BUY_WHEEL = 0.
            OFF_PEAK_SELL_WHEEL = 0.
            OFF_PEAK_SECOND_BUY_WHEEL = 0.
            OFF_PEAK_SECOND_SELL_WHEEL = 0.
         ENDIF
         IF(TRANC_SCREEN_MESSAGES) THEN
            SCREEN_MESSAGES = 'Transact C Below Init'
            CALL MG_LOCATE_WRITE(12,26,
     +                           trim(SCREEN_MESSAGES),ALL_VERSIONS,2)
         ENDIF
!
         DO DAY = 1, R_DAYS_IN_MONTH

            CALENDAR_DAY_OF_WEEK = GET_DAY_OF_WEEK_4(R_MONTH,DAY)
!
            HR = (DAY-1)*24 + 1
            LOCAL_PRICE(HR) = L_MONTHLY_PRICE(HR)
     +                                            - OFF_PEAK_SELL_WHEEL
            LOCAL_PRICE(1+HR) = L_MONTHLY_PRICE(HR+1)
     +                                            - OFF_PEAK_SELL_WHEEL
            LOCAL_PRICE(2+HR) = L_MONTHLY_PRICE(HR+2)
     +                                            - OFF_PEAK_SELL_WHEEL
            LOCAL_PRICE(3+HR) = L_MONTHLY_PRICE(HR+3)
     +                                            - OFF_PEAK_SELL_WHEEL
            LOCAL_PRICE(4+HR) = L_MONTHLY_PRICE(HR+4)
     +                                            - OFF_PEAK_SELL_WHEEL
            LOCAL_PRICE(5+HR) = L_MONTHLY_PRICE(HR+5)
     +                                            - OFF_PEAK_SELL_WHEEL
            LOCAL_PRICE(6+HR) = L_MONTHLY_PRICE(HR+6)
     +                                            - OFF_PEAK_SELL_WHEEL
!
            LOCAL_PRICE(23+HR) = L_MONTHLY_PRICE(HR+23)
     +                                            - OFF_PEAK_SELL_WHEEL
!
            IF(CALENDAR_DAY_OF_WEEK > 0 .AND.
     +                                   CALENDAR_DAY_OF_WEEK < 6) THEN
               LOCAL_PRICE(7+HR) = L_MONTHLY_PRICE(HR+7)
     +                                             - ON_PEAK_SELL_WHEEL
               LOCAL_PRICE(8+HR) = L_MONTHLY_PRICE(HR+8)
     +                                             - ON_PEAK_SELL_WHEEL
               LOCAL_PRICE(9+HR) = L_MONTHLY_PRICE(HR+9)
     +                                             - ON_PEAK_SELL_WHEEL
               LOCAL_PRICE(10+HR) = L_MONTHLY_PRICE(HR+10)
     +                                             - ON_PEAK_SELL_WHEEL
               LOCAL_PRICE(11+HR) = L_MONTHLY_PRICE(HR+11)
     +                                             - ON_PEAK_SELL_WHEEL
               LOCAL_PRICE(12+HR) = L_MONTHLY_PRICE(HR+12)
     +                                             - ON_PEAK_SELL_WHEEL
               LOCAL_PRICE(13+HR) = L_MONTHLY_PRICE(HR+13)
     +                                             - ON_PEAK_SELL_WHEEL
               LOCAL_PRICE(14+HR) = L_MONTHLY_PRICE(HR+14)
     +                                             - ON_PEAK_SELL_WHEEL
               LOCAL_PRICE(15+HR) = L_MONTHLY_PRICE(HR+15)
     +                                             - ON_PEAK_SELL_WHEEL
               LOCAL_PRICE(16+HR) = L_MONTHLY_PRICE(HR+16)
     +                                             - ON_PEAK_SELL_WHEEL
               LOCAL_PRICE(17+HR) = L_MONTHLY_PRICE(HR+17)
     +                                             - ON_PEAK_SELL_WHEEL
               LOCAL_PRICE(18+HR) = L_MONTHLY_PRICE(HR+18)
     +                                             - ON_PEAK_SELL_WHEEL
               LOCAL_PRICE(19+HR) = L_MONTHLY_PRICE(HR+19)
     +                                             - ON_PEAK_SELL_WHEEL
               LOCAL_PRICE(20+HR) = L_MONTHLY_PRICE(HR+20)
     +                                             - ON_PEAK_SELL_WHEEL
               LOCAL_PRICE(21+HR) = L_MONTHLY_PRICE(HR+21)
     +                                             - ON_PEAK_SELL_WHEEL
               LOCAL_PRICE(22+HR) = L_MONTHLY_PRICE(HR+22)
     +                                             - ON_PEAK_SELL_WHEEL
            ELSE
               LOCAL_PRICE(7+HR) = L_MONTHLY_PRICE(HR+7)
     +                                            - OFF_PEAK_SELL_WHEEL
               LOCAL_PRICE(8+HR) = L_MONTHLY_PRICE(HR+8)
     +                                            - OFF_PEAK_SELL_WHEEL
               LOCAL_PRICE(9+HR) = L_MONTHLY_PRICE(HR+9)
     +                                            - OFF_PEAK_SELL_WHEEL
               LOCAL_PRICE(10+HR) = L_MONTHLY_PRICE(HR+10)
     +                                            - OFF_PEAK_SELL_WHEEL
               LOCAL_PRICE(11+HR) = L_MONTHLY_PRICE(HR+11)
     +                                            - OFF_PEAK_SELL_WHEEL
               LOCAL_PRICE(12+HR) = L_MONTHLY_PRICE(HR+12)
     +                                            - OFF_PEAK_SELL_WHEEL
               LOCAL_PRICE(13+HR) = L_MONTHLY_PRICE(HR+13)
     +                                            - OFF_PEAK_SELL_WHEEL
               LOCAL_PRICE(14+HR) = L_MONTHLY_PRICE(HR+14)
     +                                            - OFF_PEAK_SELL_WHEEL
               LOCAL_PRICE(15+HR) = L_MONTHLY_PRICE(HR+15)
     +                                            - OFF_PEAK_SELL_WHEEL
               LOCAL_PRICE(16+HR) = L_MONTHLY_PRICE(HR+16)
     +                                            - OFF_PEAK_SELL_WHEEL
               LOCAL_PRICE(17+HR) = L_MONTHLY_PRICE(HR+17)
     +                                            - OFF_PEAK_SELL_WHEEL
               LOCAL_PRICE(18+HR) = L_MONTHLY_PRICE(HR+18)
     +                                            - OFF_PEAK_SELL_WHEEL
               LOCAL_PRICE(19+HR) = L_MONTHLY_PRICE(HR+19)
     +                                            - OFF_PEAK_SELL_WHEEL
               LOCAL_PRICE(20+HR) = L_MONTHLY_PRICE(HR+20)
     +                                            - OFF_PEAK_SELL_WHEEL
               LOCAL_PRICE(21+HR) = L_MONTHLY_PRICE(HR+21)
     +                                            - OFF_PEAK_SELL_WHEEL
               LOCAL_PRICE(22+HR) = L_MONTHLY_PRICE(HR+22)
     +                                            - OFF_PEAK_SELL_WHEEL
            ENDIF
!
         ENDDO
!
         ECITY_S_U = 0
!
         BLOCK_1 = -1
         BLOCK_2 = 0
!
         SYSTEM_EMERGENCY_AVAILABLE = 0.
!
!
            DO S_U = 1, NO_START_UP_UNITS
               BLOCK_1 = BLOCK_1 + 2
               BLOCK_2 = BLOCK_2 + 2
!
! SPAN UNITS
!
               OPTION_INDEX = GET_START_UP_POSITION(S_U,R_TG)
!
               REMAINING_RAMP_UP_HOURS = 9999
               REMAINING_RAMP_DOWN_HOURS = 9999
!
               A = GET_START_UP_INDEX(OPTION_INDEX)
!
               ORIGINAL_VALUE_OF_U(S_U) = OPTION_INDEX
               INCREASING_VALUE_OF_U(S_U) = S_U
!
               ANNUAL_NAME = RETURN_UNITNM(OPTION_INDEX)

          OPTION_NAME(S_U)= CLA_RETURN_UNITNM(OPTION_INDEX) 


               TRANS_GROUP_NAME = GET_GROUP_NAME(R_TG)
!
               UNIT_UP_YESTERDAY(A) =
     +                          GET_CLA_UNIT_UP_YESTERDAY(OPTION_INDEX)
!
               MARKET_FLOOR = GET_MARKET_FLOOR(OPTION_INDEX)
               IF(MARKET_FLOOR < 0.) THEN
                  MARKET_FLOOR =
     +                  ESCALATED_MONTHLY_VALUE(MARKET_FLOOR,
     +                                INT(ABS(MARKET_FLOOR),2),
     +                                   globecom_YEAR,R_MONTH,INT(1,2))
               ENDIF
               MARKET_FLOOR_UNIT(S_U) = ABS(MARKET_FLOOR) > .001
               MARKET_CEILING = GET_MARKET_CEILING(OPTION_INDEX)
               IF(MARKET_CEILING < 0.) THEN
                  MARKET_CEILING =
     +                  ESCALATED_MONTHLY_VALUE(MARKET_CEILING,
     +                                INT(ABS(MARKET_CEILING),2),
     +                                   globecom_YEAR,R_MONTH,INT(1,2))
               ENDIF
!
               MARKET_CEILING_UNIT(S_U) = ABS(MARKET_CEILING) > .001
               CONTRIBUTES_TO_SPIN(S_U) = GET_SPIN_STATUS(OPTION_INDEX)
! 031914. ADDED VECTORS FOR DIANE.
               IF(CONTRIBUTES_TO_SPIN(S_U)) THEN
                  MIN_SPIN_CAP(S_U) = GET_MIN_SPIN_CAP(OPTION_INDEX)
                  IF(MIN_SPIN_CAP(S_U) < 0.) THEN
                        MIN_SPIN_CAP(S_U) =
     +                     ESCALATED_MONTHLY_VALUE(MIN_SPIN_CAP(S_U),
     +                                INT(ABS(MIN_SPIN_CAP(S_U)),2),
     +                                   globecom_YEAR,R_MONTH,INT(1,2))
                  ENDIF
                  MAX_SPIN_CAP(S_U) = GET_MAX_SPIN_CAP(OPTION_INDEX)
                  IF(MAX_SPIN_CAP(S_U) < 0.) THEN
                        MAX_SPIN_CAP(S_U) =
     +                     ESCALATED_MONTHLY_VALUE(MAX_SPIN_CAP(S_U),
     +                                INT(ABS(MAX_SPIN_CAP(S_U)),2),
     +                                   globecom_YEAR,R_MONTH,INT(1,2))
                  ENDIF
               ENDIF
               VOID_LOGICAL = GET_START_UP_LOGIC(OPTION_INDEX,
     +                                             START_UP_LOGIC)
!
               MARKET_RESOURCE_COUNTER(S_U) =
     +                                   GET_MARKET_RESOURCE_COUNTER(A)
!
               MUST_RUN_UNIT = (LDTYPE(OPTION_INDEX)  == 'N' .OR.
     +                           LDTYPE(OPTION_INDEX) == 'M' .OR.
     +                           TEST_MONTHLY_MUST_RUN(A,R_MONTH))
!
               IF(MUST_RUN_UNIT) THEN
                  MUST_RUN_BLOCK(BLOCK_1) = 1
               ENDIF
!
               PRIMARY_FUEL_CHEAPER = .TRUE.
!
               CALL GET_DISP_COST_FOR_A_UNIT(OPTION_INDEX,
     +                                   BLOCK_PRICE(1),BLOCK_PRICE(2))
               CALL GET_SECOND_COST_FOR_A_UNIT(OPTION_INDEX,
     +                                 SECOND_PRICE(1),SECOND_PRICE(2))
!
! 11/19/03.
!
               BLOCK_INC_PRICE(1) =
     +                 GET_TOTAL_INCREMENTAL_COST(OPTION_INDEX,INT(1,2))

               BLOCK_INC_PRICE(2) =
     +                 GET_TOTAL_INCREMENTAL_COST(OPTION_INDEX,INT(2,2))

! 080703. DYNAMIC FUEL IS NOW SET IN THE FUEL USED SOURCE FILE.
               IF(SBTUCT(OPTION_INDEX) /= 0.) THEN
                  IF( ABS(BLOCK_PRICE(1)-SECOND_PRICE(1)) < .001 .AND.
     +                      FUELMX(OPTION_INDEX) > 0.9999 .AND.
     +                                        DYNAMIC_FUEL_CHOICE) THEN
                     BLOCK_PRICE(1) = SECOND_PRICE(1)
                     BLOCK_PRICE(2) = SECOND_PRICE(2)
                     PRIMARY_FUEL_CHEAPER = .FALSE.
                  ENDIF
               ENDIF
!
               STRICT_MARKET_RESOURCE(S_U) =
     +                 IS_STRICT_MARKET_RESOURCE(A,MARKET_RESOURCE_STR)
!               STRICT_MARKET_RESOURCE(S_U) = .FALSE.
               STRICT_MARKET_W_LOAD_BAL(S_U) = .FALSE.
               TVA_MARKET_RESOURCE(S_U) = .FALSE.
               DSM_MARKET_RESOURCE(S_U) = .FALSE.
               GRE_MARKET_RESOURCE(S_U) = .FALSE.
               ECITY_MARKET_RESOURCE(S_U) = .FALSE.
               IF(MARKET_RESOURCE_STR == 'M') THEN
                     STRICT_MARKET_RESOURCE(S_U) = .TRUE.
               ELSEIF(MARKET_RESOURCE_STR == 'L') THEN
                  STRICT_MARKET_W_LOAD_BAL(S_U) = .TRUE.
               ELSEIF(MARKET_RESOURCE_STR == 'D') THEN
                  DSM_MARKET_RESOURCE(S_U) = .TRUE.
               ELSEIF(MARKET_RESOURCE_STR == 'G') THEN
                  GRE_MARKET_RESOURCE(S_U) = .TRUE.
               ELSEIF(MARKET_RESOURCE_STR == 'E') THEN
                  ECITY_MARKET_RESOURCE(S_U) = .TRUE.
                  ECITY_S_U = S_U
               ELSEIF(MARKET_RESOURCE_STR == 'T') THEN
                  TVA_MARKET_RESOURCE(S_U) = .TRUE.
               ENDIF
!
               DISPATCH_COST_FOR_BLOCK(BLOCK_1) = BLOCK_PRICE(1)
               DISPATCH_COST_FOR_BLOCK(BLOCK_2) = BLOCK_PRICE(2)
               ALT_DISPATCH_COST_FOR_BLOCK(BLOCK_1) = BLOCK_PRICE(1)
               ALT_DISPATCH_COST_FOR_BLOCK(BLOCK_2) = BLOCK_PRICE(2)

               IF(ABS(MARKET_FLOOR) < .001) THEN
                  FLOOR_PRICE_FOR_BLOCK(BLOCK_1) = BLOCK_PRICE(1)
                  FLOOR_PRICE_FOR_BLOCK(BLOCK_2) = BLOCK_PRICE(2)
               ELSE
                  FLOOR_PRICE_FOR_BLOCK(BLOCK_1) = MARKET_FLOOR
                  FLOOR_PRICE_FOR_BLOCK(BLOCK_2) = MARKET_FLOOR
                  ALT_DISPATCH_COST_FOR_BLOCK(BLOCK_1) = MARKET_FLOOR
                  ALT_DISPATCH_COST_FOR_BLOCK(BLOCK_2) = MARKET_FLOOR
               ENDIF
               IF(ABS(MARKET_CEILING) < .001) THEN
                  CEILING_PRICE_FOR_BLOCK(BLOCK_1) = BLOCK_PRICE(1)
                  CEILING_PRICE_FOR_BLOCK(BLOCK_2) = BLOCK_PRICE(2)
               ELSE
                  CEILING_PRICE_FOR_BLOCK(BLOCK_1) = MARKET_CEILING
                  CEILING_PRICE_FOR_BLOCK(BLOCK_2) = MARKET_CEILING
               ENDIF
!
               INCREMENTAL_FUEL_COST(BLOCK_1) =
     +                  GET_INCREMENTAL_FUEL_COST(OPTION_INDEX,INT(1,2))

               INCREMENTAL_FUEL_COST(BLOCK_2) =
     +                  GET_INCREMENTAL_FUEL_COST(OPTION_INDEX,INT(2,2))

               TEMP_R = GET_BLOCK_AVAIL_4_MONTH(OPTION_INDEX,INT(1,2),
     +                                               FIRST_BLOCK_AVAIL,
     +                                                FIRST_BLOCK_OUT,
     +                                                FIRST_DAILY_OUT)
               TEMP_R = GET_BLOCK_AVAIL_4_MONTH(OPTION_INDEX,INT(2,2),
     +                                              SECOND_BLOCK_AVAIL,
     +                                               SECOND_BLOCK_OUT,
     +                                               SECOND_DAILY_OUT)
!
! INDEX FOR CAPACITY INSIDE MARGNOBJ
!
               BLOCK_CAPACITY(1) =
     +            GET_BLOCK_OUTAGE_CAPACITY(OPTION_INDEX,INT(1,2),
     +                                                     R_DATA_BASE)
               BLOCK_CAPACITY(2) =
     +                  GET_BLOCK_OUTAGE_CAPACITY(OPTION_INDEX,INT(2,2),
     +                                                     R_DATA_BASE)
               IF(BLOCK_CAPACITY(2) > 0.) THEN
                  BLOCK_CAPACITY(2) =
     +                            BLOCK_CAPACITY(1) + BLOCK_CAPACITY(2)
               ENDIF
               EMERGENCY_CAPACITY(S_U) =
     +               GET_EMERGENCY_CAPACITY(OPTION_INDEX,
     +                                         EMERGENCY_HEATRATE(S_U))
              SYSTEM_EMERGENCY_AVAILABLE = SYSTEM_EMERGENCY_AVAILABLE +
     +                                          EMERGENCY_CAPACITY(S_U)
!
               START_UP_COSTS(S_U) =
     + GET_UNIT_START_UP_COSTS(globecom_YEAR,OPTION_INDEX,R_MONTH)

               RAMP_RATE(S_U) =  GET_RAMP_RATE(OPTION_INDEX)
               RAMP_DOWN_RATE(S_U) =  GET_RAMP_DOWN_RATE(OPTION_INDEX)
! ASSUME ZERO IS A MISTAKE.
               IF(RAMP_RATE(S_U) == 0.) RAMP_RATE(S_U) = 999999.
               IF(RAMP_DOWN_RATE(S_U) == 0.)
     +                                    RAMP_DOWN_RATE(S_U) = 999999.
               IF(RAMP_RATE(S_U) / 48. < BLOCK_CAPACITY(2)) THEN
                  RAMP_RATE_ACTIVE = .TRUE.
                  IF(BLOCK_CAPACITY(2) > BLOCK_CAPACITY(1)) THEN
                     HOURS_TO_RAMP_UP = NINT((BLOCK_CAPACITY(2) -
     +                               BLOCK_CAPACITY(1))/RAMP_RATE(S_U))
                     HOURS_TO_RAMP_DOWN = NINT((BLOCK_CAPACITY(2) -
     +                          BLOCK_CAPACITY(1))/RAMP_DOWN_RATE(S_U))
                  ELSE
                     HOURS_TO_RAMP_UP = 0
                     HOURS_TO_RAMP_DOWN = 0
                  ENDIF
               ELSE
                  RAMP_RATE_ACTIVE = .FALSE.
               ENDIF
!
               MIN_UP_TIME(S_U) =  GET_MIN_UP_TIME(OPTION_INDEX)
               MIN_DOWN_TIME(S_U) =  GET_MIN_DOWN_TIME(OPTION_INDEX)
! !!! RESET MINIMUM UP-TIME FOR RAMP UP TO AVOID LOGICAL CONFLICT
               IF(RAMP_RATE(S_U) < 9999.) THEN
                  MIN_UP_TIME(S_U) =
     +                     MAX(int(MIN_UP_TIME(S_U),2),HOURS_TO_RAMP_UP)
               ENDIF

!
               FOR_DURATION = GET_FOR_DURATION(OPTION_INDEX)
               FOR_FREQUENCY = GET_FOR_FREQUENCY(OPTION_INDEX)
!
!
               MONTHLY_STARTS = 0
               MONTH_MINIMUM_ENERGY = 0.
!
! INITIALIZE ACROSS UNITS
!
               OPTION_STRIKE_PRICE = 0.
               STRIKES = 0.
               OPTION_MWH = 0.
               OPTION_HEAT = 0.
               OPTION_INV_HEAT = 0.
               OPTION_PAYOFF = 0.
               OPTION_COST = 0.
               OPTION_START_UP_COST = 0.
               DAILY_GROSS = 0.
! USED FOR LONGER-TERM OPTION VALUATION
               WEEKLY_MARGIN = 0.
               CUM_WEEKLY_MARGIN = 0.
!
!
               HOURS_AVAILABLE_IN_DAY = 0
               DAILY_AVAIL_CAPACITY = 0.
!
!               UNIT_UP_YESTERDAY = .TRUE.
!
               MONTH_OPTION_HOURS = 0.
               MONTH_MINIMUM_LOAD_HOURS = 0.
               MONTH_ALL_HOURS_MARGIN = 0.
               MONTH_POSITIVE_HOURS_MARGIN = 0.
               MONTH_POS_CONSEC_MARGIN = 0.
               MONTH_MORNING_CONSEC_MARGIN = 0.
!
               MONTH_THREE_DAY_MARGIN = 0.
               MONTH_SEVEN_DAY_MARGIN = 0.
!
               TEMP_BLOCK_PRICE(1) = BLOCK_PRICE(1)
               TEMP_BLOCK_PRICE(2) = BLOCK_PRICE(2)
!
! CREATE A DAILY COMMITMENT ORDER TO MEET IMPORT-CONSTRAINED LOAD
!
               TEMP_R = MAX(BLOCK_CAPACITY(1),BLOCK_CAPACITY(2))
               IF(TEMP_R > 0.) THEN
                  IF(MIN_UP_TIME(S_U) > 0.) THEN
                     START_UP_COST_PER_MWH(S_U) = TEMP_BLOCK_PRICE(1) +
     +                    START_UP_COSTS(S_U)/(MIN_UP_TIME(S_U)*TEMP_R)
                  ELSE
                     START_UP_COST_PER_MWH(S_U) = TEMP_BLOCK_PRICE(1) +
     +                          ! 4. IS A GUESS BUT BETTER THAN 1.
     +                         START_UP_COSTS(S_U)/(4.*TEMP_R) 
                  ENDIF
               ELSE
                  START_UP_COST_PER_MWH(S_U) = 0.
               ENDIF
!
               MINIMUM_UP_INDEX(S_U) = S_U
!
! CHECK FUEL INVENTORIES FOR THE MONTH. READJUST AS NEEDED.
!
               FUEL_ID = FUEL_SUPPLY_ID(OPTION_INDEX)
               CALL GET_MMBTU_FUEL_BALANCE(UNIT_FUEL_INVENTORY_ACTIVE,
     +                                      MMBTU_FUEL_BALANCE,FUEL_ID)
               PRIM_FUEL_SOURCE_EXHUASTED = .FALSE.
               ALL_FUEL_SOURCE_EXHUASTED = .FALSE.
! INTERUPTIBLE LOGIC
               IF(UNIT_FUEL_INVENTORY_ACTIVE) THEN
                  IF(MMBTU_FUEL_BALANCE == 0.D0) THEN
                     PRIM_FUEL_SOURCE_EXHUASTED = .TRUE.
                     IF(SBTUCT(OPTION_INDEX) == 0.) THEN
                        ALL_FUEL_SOURCE_EXHUASTED = .TRUE.
                     ENDIF
                  ENDIF
               ENDIF

!
               PREVIOUS_CONSEC_UP_HOURS = 9999
               PREVIOUS_CONSEC_DOWN_HOURS = 0
!
! 11/21/03.
!
               IF(COMMIT_ON_TOTAL_COST) THEN
                  IF(BLOCK_CAPACITY(2) > BLOCK_CAPACITY(1)) THEN
!
                     BLOCK_PRICE(1) = BLOCK_INC_PRICE(1)
                     BLOCK_PRICE(2) =
     +                           (BLOCK_INC_PRICE(1)*BLOCK_CAPACITY(1)+
     +                            BLOCK_INC_PRICE(2)*
     +                     (BLOCK_CAPACITY(2)-BLOCK_CAPACITY(1)))/
     +                                                BLOCK_CAPACITY(2)
!
                  ELSE
!
                     BLOCK_PRICE(1) = 0.
                     BLOCK_PRICE(2) = BLOCK_INC_PRICE(1)
!
                  ENDIF
               ENDIF
!
! 01/14/04.  ELENA ROUTINE
! CALCULATE THE MARGINS FOR THE ENTIRE MONTH BY BLOCK AND THEN FIND THE
! PROFIT MAXIMIZING
! 02/10/04  TOM SWEET'S MODIFICATIONS FOR MIN DOWN TIME AND ADDITIONAL
! TIME FRAME FOR CALCULATING ECONOMICS
!
!
               IF(START_UP_LOGIC == 'M') THEN
!                 LOOK_AHEAD_LOGIC = .TRUE.
                  BLOCK_COST = 0.
                  BLOCK_REV = 0.
                  BLOCK_MARGIN = 0.
                  BEST_BLOCK_CAPACITY = 0.
                  BEST_BLOCK_MARGIN = 0.
                  HOURS_SINCE_START_UP = 36
                  HOURS_SINCE_SHUT_DOWN = 36
                  MARGIN_SINCE_SHUT_DOWN = 0.
                  MARGIN_SINCE_START_UP = 0.
                  UNIT_UP_LAST_HOUR = .TRUE.
                  POSITIVE_HOURS = 0
                  POSITIVE_HOURS_MARGIN = 0.
                  NEGATIVE_HOURS = 0
                  NEGATIVE_HOURS_MARGIN = 0.
!
                  PRE_COMMIT_MW = - 99.

                  MIN_UP_TIME_INT = MIN_UP_TIME(S_U)
                  MIN_DOWN_TIME_INT = MIN_DOWN_TIME(S_U)
                  MAX_HOURS_FOR_ECON_TEST = 96
                  HOURS_FOR_ECON_TEST = MIN(MIN_UP_TIME_INT +
     +                                  MAX(MIN_DOWN_TIME_INT,int(8,2)),
     +                                  MAX_HOURS_FOR_ECON_TEST)


                  ADVANCED_LOOK_AHEAD_LOGIC = .TRUE.
                  POSITIVE_HOURLY_MARGIN = .FALSE.
                  MAX_RECOVERY_OF_SU_COST = .FALSE.
                  POS_MARGIN_FOR_UP_TIME = .FALSE.
                  MIN_DOWN_TIME_SATISFIED = .FALSE.
                  MIN_RECOVERY_OF_SU_COST = .FALSE.
                  MIN_UP_TIME_SATISFIED = .FALSE.
                  ALWAYS_LOSE_MONEY_FOR_UP_TIME = .FALSE.
                  ALWAYS_LOSE_MONEY_FOR_MAX_TIME = .FALSE.
                  KEEP_OFF_FOR_MIN_DOWN_TIME = .FALSE.
                  UNIT_STARTS_THIS_HOUR = .FALSE.
!
! ASSUME THAT THE UNIT IS UP (OR CHECK UP YESTERDAY)
            if(hours_in_month>744) then
                call end_program ("tf_objt:0004 - hours_in_month " //
     + "value of " // trim(itos(int(hours_in_month))) // 
     + " is invalid.")
            end if
                  DO HOUR = 1, HOURS_IN_MONTH
!
                     IF(SECOND_BLOCK_AVAIL(HOUR) < .001) THEN
                        BLOCK_CAPACITY(2) = FIRST_BLOCK_AVAIL(HOUR)

                        BLOCK_COST(HOUR,1) = 0.

                        BLOCK_REV(HOUR,1) = -999999.

                        BLOCK_MARGIN(HOUR,1) = BLOCK_REV(HOUR,1) -
     +                                              BLOCK_COST(HOUR,1)
                BLOCK_COST(HOUR,2) = BLOCK_PRICE(1) * ! USING 1'S COST
     +                                 BLOCK_CAPACITY(2)
                        BLOCK_REV(HOUR,2) = LOCAL_PRICE(HOUR) *
     +                                 BLOCK_CAPACITY(2)
                        BLOCK_MARGIN(HOUR,2) = BLOCK_REV(HOUR,2) -
     +                                               BLOCK_COST(HOUR,2)
                     ELSE
                        BLOCK_CAPACITY(1) = FIRST_BLOCK_AVAIL(HOUR)
                        BLOCK_CAPACITY(2) = BLOCK_CAPACITY(1) +
     +                                         SECOND_BLOCK_AVAIL(HOUR)
                        BLOCK_COST(HOUR,1) = BLOCK_PRICE(1) *
     +                                  BLOCK_CAPACITY(1)
                        BLOCK_REV(HOUR,1) = LOCAL_PRICE(HOUR) *
     +                                  BLOCK_CAPACITY(1)
                        BLOCK_MARGIN(HOUR,1) = BLOCK_REV(HOUR,1) -
     +                                               BLOCK_COST(HOUR,1)
                        BLOCK_COST(HOUR,2) = BLOCK_PRICE(2) *
     +                                 BLOCK_CAPACITY(2)
                        BLOCK_REV(HOUR,2) = LOCAL_PRICE(HOUR) *
     +                                 BLOCK_CAPACITY(2)
                        BLOCK_MARGIN(HOUR,2) = BLOCK_REV(HOUR,2) -
     +                                               BLOCK_COST(HOUR,2)
                     ENDIF
                     IF(BLOCK_MARGIN(HOUR,1) >
     +                                       BLOCK_MARGIN(HOUR,2)) THEN
                        BEST_BLOCK_BY_(HOUR) = 1
                        BEST_BLOCK_MARGIN(HOUR) = BLOCK_MARGIN(HOUR,1)
                        BEST_BLOCK_CAPACITY(HOUR) = BLOCK_CAPACITY(1)
                     ELSE
                        BEST_BLOCK_BY_(HOUR) = 2
                        BEST_BLOCK_MARGIN(HOUR) = BLOCK_MARGIN(HOUR,2)
                        BEST_BLOCK_CAPACITY(HOUR) = BLOCK_CAPACITY(2)
                     ENDIF
!
                  END DO
!
                  CURRENT_STATE = .FALSE.
                  HOURS_SINCE_START_UP = 0
            if(hours_in_month>744) then
                call end_program ("tf_objt:0005 - hours_in_month " //
     + "value of " // trim(itos(int(hours_in_month))) // 
     + " is invalid.")
            end if
                  DO HOUR = 1, HOURS_IN_MONTH
                     LOOK_AHEAD_MARGIN = 0.
                     ! MAX OF MARGIN FOR HOURS_FOR_ECON_TEST
                     LOOK_AHEAD_MARGIN(-3) = -99999999. 
                     ! MIN OF MARGIN FOR HOURS_FOR_ECON_TEST
                     LOOK_AHEAD_MARGIN(-2) = 99999999.  
                     ! MAX OF MARGIN FOR MIN_UP_TIME_INT
                     LOOK_AHEAD_MARGIN(-1) = -99999999. 
                     ! MIN OF MARGIN FOR MIN_UP_TIME_INT
                     LOOK_AHEAD_MARGIN(0) = 99999999.   
                     LOOK_AHEAD_MARGIN(1) = BEST_BLOCK_MARGIN(HOUR)
            if(hours_in_month>744) then
                call end_program ("tf_objt:0006 - hours_in_month " //
     + "value of " // trim(itos(int(hours_in_month))) // 
     + " is invalid.")
            end if
                     MIN_UP_TIME_INT = MIN(HOURS_IN_MONTH-HOUR,
     +                                         INT(MIN_UP_TIME(S_U),2))
                     MIN_DOWN_TIME_INT = MIN(HOURS_IN_MONTH-HOUR,
     +                                        INT(MIN_DOWN_TIME(S_U),2))
                     HOURS_FOR_ECON_TEST = MIN(HOURS_IN_MONTH-HOUR,
     +                                             HOURS_FOR_ECON_TEST)

                     j = 1
                     LOOK_AHEAD_MARGIN(0) = MIN(LOOK_AHEAD_MARGIN(0),
     +                                          LOOK_AHEAD_MARGIN(J))
                     LOOK_AHEAD_MARGIN(-1) =
     +                                     MAX(LOOK_AHEAD_MARGIN(-1),
     +                                          LOOK_AHEAD_MARGIN(J))
                     LOOK_AHEAD_MARGIN(-2) =
     +                                     MIN(LOOK_AHEAD_MARGIN(-2),
     +                                     LOOK_AHEAD_MARGIN(J))
                     LOOK_AHEAD_MARGIN(-3) =
     +                                     MAX(LOOK_AHEAD_MARGIN(-3),
     +                                          LOOK_AHEAD_MARGIN(J))

                     DO J = 2, MIN_UP_TIME_INT
                        LOOK_AHEAD_MARGIN(J) = LOOK_AHEAD_MARGIN(J-1) +
     +                                      BEST_BLOCK_MARGIN(HOUR+J-1)
                       LOOK_AHEAD_MARGIN(0) = MIN(LOOK_AHEAD_MARGIN(0),
     +                                            LOOK_AHEAD_MARGIN(J))
                        LOOK_AHEAD_MARGIN(-1) =
     +                                       MAX(LOOK_AHEAD_MARGIN(-1),
     +                                            LOOK_AHEAD_MARGIN(J))
                        LOOK_AHEAD_MARGIN(-2) =
     +                                       MIN(LOOK_AHEAD_MARGIN(-2),
     +                                        LOOK_AHEAD_MARGIN(J))
                        LOOK_AHEAD_MARGIN(-3) =
     +                                       MAX(LOOK_AHEAD_MARGIN(-3),
     +                                            LOOK_AHEAD_MARGIN(J))
                     ENDDO ! FOR MIN_UP_TIME_INT

                     DO J = MIN_UP_TIME_INT + 1, HOURS_FOR_ECON_TEST
                        LOOK_AHEAD_MARGIN(J) = LOOK_AHEAD_MARGIN(J-1) +
     +                                      BEST_BLOCK_MARGIN(HOUR+J-1)
                        LOOK_AHEAD_MARGIN(-2) =
     +                                       MIN(LOOK_AHEAD_MARGIN(-2),
     +                                        LOOK_AHEAD_MARGIN(J))
                        LOOK_AHEAD_MARGIN(-3) =
     +                                       MAX(LOOK_AHEAD_MARGIN(-3),
     +                                            LOOK_AHEAD_MARGIN(J))
                     END DO ! FOR HOURS_FOR_ECON_TEST

                     IF(ADVANCED_LOOK_AHEAD_LOGIC)THEN
                        IF(HOUR == 1) THEN
                           IF(UNIT_UP_YESTERDAY(A)) THEN
                              HOURS_SINCE_START_UP = 12
                              HOURS_SINCE_SHUT_DOWN = 0
                              UNIT_UP_LAST_HOUR = .TRUE.
                           ELSE
                              HOURS_SINCE_START_UP = 0
                              HOURS_SINCE_SHUT_DOWN = 6
                              UNIT_UP_LAST_HOUR = .FALSE.
                           ENDIF
                        ENDIF

                        IF(BEST_BLOCK_MARGIN(HOUR) > 0.) THEN
                           POSITIVE_HOURLY_MARGIN = .TRUE.
                        ELSE
                           POSITIVE_HOURLY_MARGIN = .FALSE.
                        ENDIF

                        IF(LOOK_AHEAD_MARGIN(-3) >
     +                                        START_UP_COSTS(S_U)) THEN
                           MAX_RECOVERY_OF_SU_COST = .TRUE.
                        ELSE
                           MAX_RECOVERY_OF_SU_COST = .FALSE.
                        ENDIF

                        IF(LOOK_AHEAD_MARGIN(0) > 0. ) THEN
                           POS_MARGIN_FOR_UP_TIME = .TRUE.
                        ELSE
                           POS_MARGIN_FOR_UP_TIME = .FALSE.
                        ENDIF

                        IF(HOURS_SINCE_SHUT_DOWN >=
     +                                      MIN_DOWN_TIME_INT) THEN
                           MIN_DOWN_TIME_SATISFIED = .TRUE.
                        ELSE
                           MIN_DOWN_TIME_SATISFIED = .FALSE.
                        ENDIF

                        IF(LOOK_AHEAD_MARGIN(0) <
     +                                -1.0 * START_UP_COSTS(S_U)) THEN
                           MIN_RECOVERY_OF_SU_COST = .TRUE.
                        ELSE
                           MIN_RECOVERY_OF_SU_COST = .FALSE.
                        ENDIF

                        IF(HOURS_SINCE_START_UP >=
     +                                           MIN_UP_TIME_INT ) THEN
                           MIN_UP_TIME_SATISFIED = .TRUE.
                        ELSE
                           MIN_UP_TIME_SATISFIED = .FALSE.
                        ENDIF

                        IF(LOOK_AHEAD_MARGIN(-1) < 0) THEN
                           ALWAYS_LOSE_MONEY_FOR_UP_TIME = .TRUE.
                        ELSE
                           ALWAYS_LOSE_MONEY_FOR_UP_TIME = .FALSE.
                        ENDIF

                        IF(LOOK_AHEAD_MARGIN(-3) < 0) THEN
                           ALWAYS_LOSE_MONEY_FOR_MAX_TIME = .TRUE.
                        ELSE
                           ALWAYS_LOSE_MONEY_FOR_MAX_TIME = .FALSE.
                        ENDIF

                        IF(HOURS_SINCE_START_UP >=
     +                                           MIN_UP_TIME_INT ) THEN
                           KEEP_OFF_FOR_MIN_DOWN_TIME = .TRUE.
                        ELSE
                           KEEP_OFF_FOR_MIN_DOWN_TIME = .FALSE.
                        ENDIF

                        IF(POSITIVE_HOURLY_MARGIN  .AND.
     +                                    MAX_RECOVERY_OF_SU_COST .AND.
     +                                    POS_MARGIN_FOR_UP_TIME  .AND.
     +                                    MIN_DOWN_TIME_SATISFIED) THEN
                           START_TEST = .TRUE.
                        ELSE
                           START_TEST = .FALSE.
                        ENDIF

                        IF((MIN_RECOVERY_OF_SU_COST .OR.
     +                            ALWAYS_LOSE_MONEY_FOR_MAX_TIME) .AND.
     +                            MIN_UP_TIME_SATISFIED .AND.
     +                            ALWAYS_LOSE_MONEY_FOR_UP_TIME) THEN
                           STOP_TEST = .TRUE.
                        ELSE
                           STOP_TEST = .FALSE.
                        ENDIF

                        IF(POSITIVE_HOURLY_MARGIN .OR.
     +                             ( UNIT_UP_LAST_HOUR .AND.
     +                               .NOT. STOP_TEST )) THEN
                           RUN_TEST = .TRUE.
                        ELSE
                           RUN_TEST = .FALSE.
                        ENDIF

                        IF(UNIT_UP_LAST_HOUR .AND.
     +                                RUN_TEST) THEN
                           CURRENT_STATE = .TRUE.
                        ELSEIF (UNIT_UP_LAST_HOUR .AND.
     +                                .NOT. RUN_TEST) THEN
                           CURRENT_STATE = .FALSE.
                        ELSEIF ( .NOT. UNIT_UP_LAST_HOUR .AND.
     +                                 START_TEST) THEN
                           CURRENT_STATE = .TRUE.
                        ELSE
                           CURRENT_STATE = .FALSE.
                        ENDIF

                        IF(CURRENT_STATE .AND.
     +                                .NOT. UNIT_UP_LAST_HOUR) THEN
                           UNIT_STARTS_THIS_HOUR = .TRUE.
                        ELSE
                           UNIT_STARTS_THIS_HOUR = .FALSE.
                        ENDIF

                        IF(CURRENT_STATE) THEN
                           HOURS_SINCE_START_UP =
     +                                         HOURS_SINCE_START_UP + 1
                           HOURS_SINCE_SHUT_DOWN = 0
                           PRE_COMMIT_MW(HOUR) =
     +                                        BEST_BLOCK_CAPACITY(HOUR)
                        ELSE
                           HOURS_SINCE_START_UP = 0
                           HOURS_SINCE_SHUT_DOWN =
     +                                        HOURS_SINCE_SHUT_DOWN + 1
                           PRE_COMMIT_MW(HOUR) = 0.
                        ENDIF

                        IF(CURRENT_STATE) THEN
                            IF( UNIT_STARTS_THIS_HOUR ) THEN
                                MARGIN_SINCE_START_UP =
     +                                        MARGIN_SINCE_START_UP +
     +                                        BEST_BLOCK_MARGIN(HOUR) -
     +                                        START_UP_COSTS(S_U)
                            ELSE
                                MARGIN_SINCE_START_UP =
     +                                        MARGIN_SINCE_START_UP +
     +                                        BEST_BLOCK_MARGIN(HOUR)
                            ENDIF
!         WHAT IS THE SIGN FOR SHUT DOWN MARGIN I'M ASSUMING NEGATIVE
                        ELSE
                            MARGIN_SINCE_SHUT_DOWN =
     +                                       MARGIN_SINCE_SHUT_DOWN -
     +                                       BEST_BLOCK_MARGIN(HOUR)
                        ENDIF

                        UNIT_UP_LAST_HOUR = CURRENT_STATE
                     ELSE ! OLD LOGIC
                        IF(BEST_BLOCK_MARGIN(HOUR) > 0. .AND.
     +                       LOOK_AHEAD_MARGIN(MIN_UP_TIME_INT) >
     +                                        START_UP_COSTS(S_U)) THEN
                           START_TEST = .TRUE.
                        ELSE
                           START_TEST = .FALSE.
                        ENDIF
                        IF(HOUR == 1) THEN
                           STOP_TEST = .FALSE.
                        ELSEIF(LOOK_AHEAD_MARGIN(0) +
     +                        START_UP_COSTS(S_U) < 0. .AND.
     +                           LOOK_AHEAD_MARGIN(-1) < 0. .AND.
     +                        HOURS_SINCE_START_UP >=
     +                                            MIN_UP_TIME_INT) THEN
                           STOP_TEST = .TRUE.
                        ELSE
                           STOP_TEST = .FALSE.
                        ENDIF
                        IF(HOUR == 1) THEN
                           RUN_TEST = CURRENT_STATE
                        ELSEIF(BEST_BLOCK_MARGIN(HOUR) > 0. .OR.
     +                      (CURRENT_STATE .AND. .NOT. STOP_TEST)) THEN
                           RUN_TEST = .TRUE.
                        ELSE
                           RUN_TEST = .FALSE.
                        ENDIF
                        IF(HOUR == 1) THEN
                           IF(BEST_BLOCK_MARGIN(HOUR) > 0. .AND.
     +                                         UNIT_UP_YESTERDAY(A))
     +                        THEN
                              CURRENT_STATE = .TRUE.
                           ELSE
                              CURRENT_STATE = .FALSE.
                           ENDIF
                        ELSEIF(CURRENT_STATE .AND. RUN_TEST) THEN
                           CURRENT_STATE = .TRUE.
                        ELSEIF(CURRENT_STATE .AND. .NOT. RUN_TEST) THEN
                           CURRENT_STATE = .FALSE.
                        ELSEIF(.NOT. CURRENT_STATE .AND. START_TEST)
     +                     THEN
                           CURRENT_STATE = .TRUE.
                        ELSE
                           CURRENT_STATE = .FALSE.
                        ENDIF
                        IF(CURRENT_STATE) THEN
                           HOURS_SINCE_START_UP =
     +                                         HOURS_SINCE_START_UP + 1
                           PRE_COMMIT_MW(HOUR) =
     +                                        BEST_BLOCK_CAPACITY(HOUR)
                        ELSE
                           HOURS_SINCE_START_UP = 0
                           PRE_COMMIT_MW(HOUR) = 0.
                        ENDIF
                     ENDIF ! ADVANCED_LOOK_AHEAD_LOGIC
                  ENDDO
               ENDIF ! START_UP_LOGIC = 'M'!
!
!
               DO I = 1, R_DAYS_IN_MONTH
!
!
! 02/26/04. PER MARK TO CLEAR THE TRANSACTIONS
!
                  CALL RW_PROCESS_MESSAGES()


                  HR = 24*(I-1) + 1
                  FIRST_HOUR_IN_DAY = HR
!
                  DAILY_PRICE(1) = LOCAL_PRICE(HR)
                  DAILY_PRICE(2) = LOCAL_PRICE(HR+1)
                  DAILY_PRICE(3) = LOCAL_PRICE(HR+2)
                  DAILY_PRICE(4) = LOCAL_PRICE(HR+3)
                  DAILY_PRICE(5) = LOCAL_PRICE(HR+4)
                  DAILY_PRICE(6) = LOCAL_PRICE(HR+5)
                  DAILY_PRICE(7) = LOCAL_PRICE(HR+6)
                  DAILY_PRICE(8) = LOCAL_PRICE(HR+7)
                  DAILY_PRICE(9) = LOCAL_PRICE(HR+8)
                  DAILY_PRICE(10) = LOCAL_PRICE(HR+9)
                  DAILY_PRICE(11) = LOCAL_PRICE(HR+10)
                  DAILY_PRICE(12) = LOCAL_PRICE(HR+11)
                  DAILY_PRICE(13) = LOCAL_PRICE(HR+12)
                  DAILY_PRICE(14) = LOCAL_PRICE(HR+13)
                  DAILY_PRICE(15) = LOCAL_PRICE(HR+14)
                  DAILY_PRICE(16) = LOCAL_PRICE(HR+15)
                  DAILY_PRICE(17) = LOCAL_PRICE(HR+16)
                  DAILY_PRICE(18) = LOCAL_PRICE(HR+17)
                  DAILY_PRICE(19) = LOCAL_PRICE(HR+18)
                  DAILY_PRICE(20) = LOCAL_PRICE(HR+19)
                  DAILY_PRICE(21) = LOCAL_PRICE(HR+20)
                  DAILY_PRICE(22) = LOCAL_PRICE(HR+21)
                  DAILY_PRICE(23) = LOCAL_PRICE(HR+22)
                  DAILY_PRICE(24) = LOCAL_PRICE(HR+23)
!
                  SYSTEM_HOURLY_AVAILABLE(HR,R_TG) =
     +                  SYSTEM_HOURLY_AVAILABLE(HR,R_TG) +
     +                     FIRST_BLOCK_AVAIL(HR) +
     +                        SECOND_BLOCK_AVAIL(HR)
                  SYSTEM_HOURLY_AVAILABLE(HR+1,R_TG) =
     +                  SYSTEM_HOURLY_AVAILABLE(HR+1,R_TG) +
     +                        FIRST_BLOCK_AVAIL(HR+1) +
     +                       SECOND_BLOCK_AVAIL(HR+1)
                  SYSTEM_HOURLY_AVAILABLE(HR+2,R_TG) =
     +                  SYSTEM_HOURLY_AVAILABLE(HR+2,R_TG) +
     +                        FIRST_BLOCK_AVAIL(HR+2) +
     +                       SECOND_BLOCK_AVAIL(HR+2)
                  SYSTEM_HOURLY_AVAILABLE(HR+3,R_TG) =
     +                  SYSTEM_HOURLY_AVAILABLE(HR+3,R_TG) +
     +                        FIRST_BLOCK_AVAIL(HR+3) +
     +                       SECOND_BLOCK_AVAIL(HR+3)
                  SYSTEM_HOURLY_AVAILABLE(HR+4,R_TG) =
     +                  SYSTEM_HOURLY_AVAILABLE(HR+4,R_TG) +
     +                        FIRST_BLOCK_AVAIL(HR+4) +
     +                       SECOND_BLOCK_AVAIL(HR+4)
                  SYSTEM_HOURLY_AVAILABLE(HR+5,R_TG) =
     +                  SYSTEM_HOURLY_AVAILABLE(HR+5,R_TG) +
     +                        FIRST_BLOCK_AVAIL(HR+5) +
     +                       SECOND_BLOCK_AVAIL(HR+5)
                  SYSTEM_HOURLY_AVAILABLE(HR+6,R_TG) =
     +                  SYSTEM_HOURLY_AVAILABLE(HR+6,R_TG) +
     +                        FIRST_BLOCK_AVAIL(HR+6) +
     +                       SECOND_BLOCK_AVAIL(HR+6)
                  SYSTEM_HOURLY_AVAILABLE(HR+7,R_TG) =
     +                  SYSTEM_HOURLY_AVAILABLE(HR+7,R_TG) +
     +                        FIRST_BLOCK_AVAIL(HR+7) +
     +                       SECOND_BLOCK_AVAIL(HR+7)
                  SYSTEM_HOURLY_AVAILABLE(HR+8,R_TG) =
     +                  SYSTEM_HOURLY_AVAILABLE(HR+8,R_TG) +
     +                        FIRST_BLOCK_AVAIL(HR+8) +
     +                       SECOND_BLOCK_AVAIL(HR+8)
                  SYSTEM_HOURLY_AVAILABLE(HR+9,R_TG) =
     +                  SYSTEM_HOURLY_AVAILABLE(HR+9,R_TG) +
     +                        FIRST_BLOCK_AVAIL(HR+9) +
     +                       SECOND_BLOCK_AVAIL(HR+9)
                  SYSTEM_HOURLY_AVAILABLE(HR+10,R_TG) =
     +                  SYSTEM_HOURLY_AVAILABLE(HR+10,R_TG) +
     +                        FIRST_BLOCK_AVAIL(HR+10) +
     +                       SECOND_BLOCK_AVAIL(HR+10)
                  SYSTEM_HOURLY_AVAILABLE(HR+11,R_TG) =
     +                  SYSTEM_HOURLY_AVAILABLE(HR+11,R_TG) +
     +                        FIRST_BLOCK_AVAIL(HR+11) +
     +                       SECOND_BLOCK_AVAIL(HR+11)
                  SYSTEM_HOURLY_AVAILABLE(HR+12,R_TG) =
     +                  SYSTEM_HOURLY_AVAILABLE(HR+12,R_TG) +
     +                        FIRST_BLOCK_AVAIL(HR+12) +
     +                       SECOND_BLOCK_AVAIL(HR+12)
                  SYSTEM_HOURLY_AVAILABLE(HR+13,R_TG) =
     +                  SYSTEM_HOURLY_AVAILABLE(HR+13,R_TG) +
     +                        FIRST_BLOCK_AVAIL(HR+13) +
     +                       SECOND_BLOCK_AVAIL(HR+13)
                  SYSTEM_HOURLY_AVAILABLE(HR+14,R_TG) =
     +                  SYSTEM_HOURLY_AVAILABLE(HR+14,R_TG) +
     +                        FIRST_BLOCK_AVAIL(HR+14) +
     +                       SECOND_BLOCK_AVAIL(HR+14)
                  SYSTEM_HOURLY_AVAILABLE(HR+15,R_TG) =
     +                  SYSTEM_HOURLY_AVAILABLE(HR+15,R_TG) +
     +                        FIRST_BLOCK_AVAIL(HR+15) +
     +                       SECOND_BLOCK_AVAIL(HR+15)
                  SYSTEM_HOURLY_AVAILABLE(HR+16,R_TG) =
     +                  SYSTEM_HOURLY_AVAILABLE(HR+16,R_TG) +
     +                        FIRST_BLOCK_AVAIL(HR+16) +
     +                       SECOND_BLOCK_AVAIL(HR+16)
                  SYSTEM_HOURLY_AVAILABLE(HR+17,R_TG) =
     +                  SYSTEM_HOURLY_AVAILABLE(HR+17,R_TG) +
     +                        FIRST_BLOCK_AVAIL(HR+17) +
     +                       SECOND_BLOCK_AVAIL(HR+17)
                  SYSTEM_HOURLY_AVAILABLE(HR+18,R_TG) =
     +                  SYSTEM_HOURLY_AVAILABLE(HR+18,R_TG) +
     +                        FIRST_BLOCK_AVAIL(HR+18) +
     +                       SECOND_BLOCK_AVAIL(HR+18)
                  SYSTEM_HOURLY_AVAILABLE(HR+19,R_TG) =
     +                  SYSTEM_HOURLY_AVAILABLE(HR+19,R_TG) +
     +                        FIRST_BLOCK_AVAIL(HR+19) +
     +                       SECOND_BLOCK_AVAIL(HR+19)
                  SYSTEM_HOURLY_AVAILABLE(HR+20,R_TG) =
     +                  SYSTEM_HOURLY_AVAILABLE(HR+20,R_TG) +
     +                        FIRST_BLOCK_AVAIL(HR+20) +
     +                       SECOND_BLOCK_AVAIL(HR+20)
                  SYSTEM_HOURLY_AVAILABLE(HR+21,R_TG) =
     +                  SYSTEM_HOURLY_AVAILABLE(HR+21,R_TG) +
     +                        FIRST_BLOCK_AVAIL(HR+21) +
     +                       SECOND_BLOCK_AVAIL(HR+21)
                  SYSTEM_HOURLY_AVAILABLE(HR+22,R_TG) =
     +                  SYSTEM_HOURLY_AVAILABLE(HR+22,R_TG) +
     +                        FIRST_BLOCK_AVAIL(HR+22) +
     +                       SECOND_BLOCK_AVAIL(HR+22)
                  SYSTEM_HOURLY_AVAILABLE(HR+23,R_TG) =
     +                  SYSTEM_HOURLY_AVAILABLE(HR+23,R_TG) +
     +                        FIRST_BLOCK_AVAIL(HR+23) +
     +                       SECOND_BLOCK_AVAIL(HR+23)
!
! ESTABLISH DERIVATIVES FOR GENERATING UNIT PRODUCTS
!
                  DO K = 1, 7
!                  DO K = 2, 2
!                     IF(I+K-1 > R_DAYS_IN_MONTH) THEN
!                        J = K
!                     ELSE
                        J = K + 1
!                     ENDIF
!
!
                     IF(K == 7 .OR. I == 1) THEN
                        IF(I+K-1 >= R_DAYS_IN_MONTH) THEN
                           LR = (I+K-1 - R_DAYS_IN_MONTH)*24
                        ELSE
                           LR = (I+K-1)*24
                        ENDIF
!
                        WEEKLY_MARGIN(1,K) =
     +                   MAX(
     +                     (LOCAL_PRICE(LR+1) -  BLOCK_PRICE(1)) *
     +                                         BLOCK_CAPACITY(1),
     +                     (LOCAL_PRICE(LR+1) -  BLOCK_PRICE(2)) *
     +                                         BLOCK_CAPACITY(2))
                        WEEKLY_MARGIN(2,K) =
     +                   MAX(
     +                     (LOCAL_PRICE(LR+2) -  BLOCK_PRICE(1)) *
     +                                         BLOCK_CAPACITY(1),
     +                     (LOCAL_PRICE(LR+2) -  BLOCK_PRICE(2)) *
     +                                         BLOCK_CAPACITY(2))
                        WEEKLY_MARGIN(3,K) =
     +                   MAX(
     +                     (LOCAL_PRICE(LR+3) -  BLOCK_PRICE(1)) *
     +                                         BLOCK_CAPACITY(1),
     +                     (LOCAL_PRICE(LR+3) -  BLOCK_PRICE(2)) *
     +                                         BLOCK_CAPACITY(2))
                        WEEKLY_MARGIN(4,K) =
     +                   MAX(
     +                     (LOCAL_PRICE(LR+4) -  BLOCK_PRICE(1)) *
     +                                         BLOCK_CAPACITY(1),
     +                     (LOCAL_PRICE(LR+4) -  BLOCK_PRICE(2)) *
     +                                         BLOCK_CAPACITY(2))
                        WEEKLY_MARGIN(5,K) =
     +                   MAX(
     +                     (LOCAL_PRICE(LR+5) -  BLOCK_PRICE(1)) *
     +                                         BLOCK_CAPACITY(1),
     +                     (LOCAL_PRICE(LR+5) -  BLOCK_PRICE(2)) *
     +                                         BLOCK_CAPACITY(2))
                        WEEKLY_MARGIN(6,K) =
     +                   MAX(
     +                     (LOCAL_PRICE(LR+6) -  BLOCK_PRICE(1)) *
     +                                         BLOCK_CAPACITY(1),
     +                     (LOCAL_PRICE(LR+6) -  BLOCK_PRICE(2)) *
     +                                         BLOCK_CAPACITY(2))
                        WEEKLY_MARGIN(7,K) =
     +                   MAX(
     +                     (LOCAL_PRICE(LR+7) -  BLOCK_PRICE(1)) *
     +                                         BLOCK_CAPACITY(1),
     +                     (LOCAL_PRICE(LR+7) -  BLOCK_PRICE(2)) *
     +                                         BLOCK_CAPACITY(2))
                        WEEKLY_MARGIN(8,K) =
     +                   MAX(
     +                     (LOCAL_PRICE(LR+8) -  BLOCK_PRICE(1)) *
     +                                         BLOCK_CAPACITY(1),
     +                     (LOCAL_PRICE(LR+8) -  BLOCK_PRICE(2)) *
     +                                         BLOCK_CAPACITY(2))
                        WEEKLY_MARGIN(9,K) =
     +                   MAX(
     +                     (LOCAL_PRICE(LR+9) -  BLOCK_PRICE(1)) *
     +                                         BLOCK_CAPACITY(1),
     +                     (LOCAL_PRICE(LR+9) -  BLOCK_PRICE(2)) *
     +                                         BLOCK_CAPACITY(2))
                        WEEKLY_MARGIN(10,K) =
     +                   MAX(
     +                     (LOCAL_PRICE(LR+10) -  BLOCK_PRICE(1)) *
     +                                         BLOCK_CAPACITY(1),
     +                     (LOCAL_PRICE(LR+10) -  BLOCK_PRICE(2)) *
     +                                         BLOCK_CAPACITY(2))
                        WEEKLY_MARGIN(11,K) =
     +                   MAX(
     +                     (LOCAL_PRICE(LR+11) -  BLOCK_PRICE(1)) *
     +                                         BLOCK_CAPACITY(1),
     +                     (LOCAL_PRICE(LR+11) -  BLOCK_PRICE(2)) *
     +                                         BLOCK_CAPACITY(2))
                        WEEKLY_MARGIN(12,K) =
     +                   MAX(
     +                     (LOCAL_PRICE(LR+12) -  BLOCK_PRICE(1)) *
     +                                         BLOCK_CAPACITY(1),
     +                     (LOCAL_PRICE(LR+12) -  BLOCK_PRICE(2)) *
     +                                         BLOCK_CAPACITY(2))
                        WEEKLY_MARGIN(13,K) =
     +                   MAX(
     +                     (LOCAL_PRICE(LR+13) -  BLOCK_PRICE(1)) *
     +                                         BLOCK_CAPACITY(1),
     +                     (LOCAL_PRICE(LR+13) -  BLOCK_PRICE(2)) *
     +                                         BLOCK_CAPACITY(2))
                        WEEKLY_MARGIN(14,K) =
     +                   MAX(
     +                     (LOCAL_PRICE(LR+14) -  BLOCK_PRICE(1)) *
     +                                         BLOCK_CAPACITY(1),
     +                     (LOCAL_PRICE(LR+14) -  BLOCK_PRICE(2)) *
     +                                         BLOCK_CAPACITY(2))
                        WEEKLY_MARGIN(15,K) =
     +                   MAX(
     +                     (LOCAL_PRICE(LR+15) -  BLOCK_PRICE(1)) *
     +                                         BLOCK_CAPACITY(1),
     +                     (LOCAL_PRICE(LR+15) -  BLOCK_PRICE(2)) *
     +                                         BLOCK_CAPACITY(2))
                        WEEKLY_MARGIN(16,K) =
     +                   MAX(
     +                     (LOCAL_PRICE(LR+16) -  BLOCK_PRICE(1)) *
     +                                         BLOCK_CAPACITY(1),
     +                     (LOCAL_PRICE(LR+16) -  BLOCK_PRICE(2)) *
     +                                         BLOCK_CAPACITY(2))
                        WEEKLY_MARGIN(17,K) =
     +                   MAX(
     +                     (LOCAL_PRICE(LR+17) -  BLOCK_PRICE(1)) *
     +                                         BLOCK_CAPACITY(1),
     +                     (LOCAL_PRICE(LR+17) -  BLOCK_PRICE(2)) *
     +                                         BLOCK_CAPACITY(2))
                        WEEKLY_MARGIN(18,K) =
     +                   MAX(
     +                     (LOCAL_PRICE(LR+18) -  BLOCK_PRICE(1)) *
     +                                         BLOCK_CAPACITY(1),
     +                     (LOCAL_PRICE(LR+18) -  BLOCK_PRICE(2)) *
     +                                         BLOCK_CAPACITY(2))
                        WEEKLY_MARGIN(19,K) =
     +                   MAX(
     +                     (LOCAL_PRICE(LR+19) -  BLOCK_PRICE(1)) *
     +                                         BLOCK_CAPACITY(1),
     +                     (LOCAL_PRICE(LR+19) -  BLOCK_PRICE(2)) *
     +                                         BLOCK_CAPACITY(2))
                        WEEKLY_MARGIN(20,K) =
     +                   MAX(
     +                     (LOCAL_PRICE(LR+20) -  BLOCK_PRICE(1)) *
     +                                         BLOCK_CAPACITY(1),
     +                     (LOCAL_PRICE(LR+20) -  BLOCK_PRICE(2)) *
     +                                         BLOCK_CAPACITY(2))
                        WEEKLY_MARGIN(21,K) =
     +                   MAX(
     +                     (LOCAL_PRICE(LR+21) -  BLOCK_PRICE(1)) *
     +                                         BLOCK_CAPACITY(1),
     +                     (LOCAL_PRICE(LR+21) -  BLOCK_PRICE(2)) *
     +                                         BLOCK_CAPACITY(2))
                        WEEKLY_MARGIN(22,K) =
     +                   MAX(
     +                     (LOCAL_PRICE(LR+22) -  BLOCK_PRICE(1)) *
     +                                         BLOCK_CAPACITY(1),
     +                     (LOCAL_PRICE(LR+22) -  BLOCK_PRICE(2)) *
     +                                         BLOCK_CAPACITY(2))
                        WEEKLY_MARGIN(23,K) =
     +                   MAX(
     +                     (LOCAL_PRICE(LR+23) -  BLOCK_PRICE(1)) *
     +                                         BLOCK_CAPACITY(1),
     +                     (LOCAL_PRICE(LR+23) -  BLOCK_PRICE(2)) *
     +                                         BLOCK_CAPACITY(2))
                        WEEKLY_MARGIN(24,K) =
     +                   MAX(
     +                     (LOCAL_PRICE(LR+24) -  BLOCK_PRICE(1)) *
     +                                         BLOCK_CAPACITY(1),
     +                     (LOCAL_PRICE(LR+24) -  BLOCK_PRICE(2)) *
     +                                         BLOCK_CAPACITY(2))
                        WEEKLY_MARGIN(0,K) =
     +                  WEEKLY_MARGIN(1,K)+WEEKLY_MARGIN(2,K) +
     +                  WEEKLY_MARGIN(3,K)+WEEKLY_MARGIN(4,K) +
     +                  WEEKLY_MARGIN(5,K)+WEEKLY_MARGIN(6,K) +
     +                  WEEKLY_MARGIN(7,K)+WEEKLY_MARGIN(8,K) +
     +                  WEEKLY_MARGIN(9,K)+WEEKLY_MARGIN(10,K) +
     +                  WEEKLY_MARGIN(11,K)+WEEKLY_MARGIN(12,K) +
     +                  WEEKLY_MARGIN(13,K)+WEEKLY_MARGIN(14,K) +
     +                  WEEKLY_MARGIN(15,K)+WEEKLY_MARGIN(16,K) +
     +                  WEEKLY_MARGIN(17,K)+WEEKLY_MARGIN(18,K) +
     +                  WEEKLY_MARGIN(19,K)+WEEKLY_MARGIN(20,K) +
     +                  WEEKLY_MARGIN(21,K)+WEEKLY_MARGIN(22,K) +
     +                  WEEKLY_MARGIN(23,K)+WEEKLY_MARGIN(24,K)
                     ELSE
                        WEEKLY_MARGIN(0,K) = WEEKLY_MARGIN(0,J)
                        WEEKLY_MARGIN(1,K) = WEEKLY_MARGIN(1,J)
                        WEEKLY_MARGIN(2,K) = WEEKLY_MARGIN(2,J)
                        WEEKLY_MARGIN(3,K) = WEEKLY_MARGIN(3,J)
                        WEEKLY_MARGIN(4,K) = WEEKLY_MARGIN(4,J)
                        WEEKLY_MARGIN(5,K) = WEEKLY_MARGIN(5,J)
                        WEEKLY_MARGIN(6,K) = WEEKLY_MARGIN(6,J)
                        WEEKLY_MARGIN(7,K) = WEEKLY_MARGIN(7,J)
                        WEEKLY_MARGIN(8,K) = WEEKLY_MARGIN(8,J)
                        WEEKLY_MARGIN(9,K) = WEEKLY_MARGIN(9,J)
                        WEEKLY_MARGIN(10,K) = WEEKLY_MARGIN(10,J)
                        WEEKLY_MARGIN(11,K) = WEEKLY_MARGIN(11,J)
                        WEEKLY_MARGIN(12,K) = WEEKLY_MARGIN(12,J)
                        WEEKLY_MARGIN(13,K) = WEEKLY_MARGIN(13,J)
                        WEEKLY_MARGIN(14,K) = WEEKLY_MARGIN(14,J)
                        WEEKLY_MARGIN(15,K) = WEEKLY_MARGIN(15,J)
                        WEEKLY_MARGIN(16,K) = WEEKLY_MARGIN(16,J)
                        WEEKLY_MARGIN(17,K) = WEEKLY_MARGIN(17,J)
                        WEEKLY_MARGIN(18,K) = WEEKLY_MARGIN(18,J)
                        WEEKLY_MARGIN(19,K) = WEEKLY_MARGIN(19,J)
                        WEEKLY_MARGIN(20,K) = WEEKLY_MARGIN(20,J)
                        WEEKLY_MARGIN(21,K) = WEEKLY_MARGIN(21,J)
                        WEEKLY_MARGIN(22,K) = WEEKLY_MARGIN(22,J)
                        WEEKLY_MARGIN(23,K) = WEEKLY_MARGIN(23,J)
                        WEEKLY_MARGIN(24,K) = WEEKLY_MARGIN(24,J)
                     ENDIF
! CALCULATE DAILY MARGINS 1-7 DAYS IN ADVANCE.
                     IF(K == 1) THEN
                        CUM_WEEKLY_MARGIN(K) = WEEKLY_MARGIN(0,K)
                     ELSE
                        CUM_WEEKLY_MARGIN(K) =
     +                           CUM_WEEKLY_MARGIN(K-1) +
     +                                         WEEKLY_MARGIN(0,K)
                     ENDIF
                  ENDDO
!
                  OPTION_HOURS = 0.
                  MINIMUM_LOAD_HOURS = 0.
! THIS SHOULD BE INTERNALLY CALULATED
                  NEXT_DAY_OPTIONALITY = START_UP_COSTS(S_U)/3.
!
                  ALL_HOURS_MARGIN = 0.
                  ALL_HOURS_COST = 0.
                  ALL_HOURS_PAYOFF = 0.
                  ALL_HOURS_MWH(1) = 0.
                  ALL_HOURS_MWH(2) = 0.
!
                  POS_CONSEC_HOURS = 0.
                  POS_CONSEC_MARGIN = 0.
                  ACCUM_POS_CONSEC_MARGIN = 0.
                  BEST_ACCUM_POS_CONSEC_MARGIN = 0.
                  BEST_ACCUM_POS_CONSEC_HOURS = 0
                  POS_CONSEC_COST = 0.
                  POS_CONSEC_PAYOFF = 0.
                  POS_CONSEC_MWH(1) = 0.
                  POS_CONSEC_MWH(2) = 0.
!
                  MORNING_CONSEC_HOURS = 0.
                  MORNING_CONSEC_MARGIN = 0.
                  MORNING_CONSEC_COST = 0.
                  MORNING_CONSEC_PAYOFF = 0.
                  MORNING_CONSEC_MWH(1) = 0.
                  MORNING_CONSEC_MWH(2) = 0.
!
                  POSITIVE_HOURS = 0.
                  POSITIVE_HOURS_MARGIN = 0.
                  POSITIVE_HOURS_COST = 0.
                  POSITIVE_HOURS_PAYOFF = 0.
                  POSITIVE_HOURS_MWH(1) = 0.
                  POSITIVE_HOURS_MWH(2) = 0.
                  POSITIVE_HOURS_STARTS = 0
!
                  ALL_MIN_LOAD_HOURS = 0.
                  POSITIVE_MIN_LOAD_HOURS = 0.
                  POS_CONSEC_MIN_LOAD_HOURS = 0.
                  MORNING_CONSEC_MIN_LOAD_HOURS = 0.
!
                  START_CONSEC_HOUR = 0
                  END_CONSEC_HOUR = 24
!
                  STRAT_HOURLY_MW = 0.
!
! IF POSITIVE, OUTAGE OCCURS IN DAY
!
! 05/15/03. FOR GRE
!
!
                  DAILY_OUTAGE_VALUE = 0.
                  DO LOCAL_HOUR = HR, HR + 23
                     IF(SECOND_BLOCK_AVAIL(LOCAL_HOUR) > 0. .OR.
     +                        FIRST_BLOCK_AVAIL(LOCAL_HOUR) > 0.) CYCLE
                     DAILY_OUTAGE_VALUE = DAILY_OUTAGE_VALUE + 1.
                  ENDDO

                  OUT_ALL_DAY = .FALSE.
                  OUT_BEGINNING_DAY = .FALSE.
                  OUT_MIDDLE_DAY = .FALSE.
                  OUT_END_DAY = .FALSE.
!
                  IF(DAILY_OUTAGE_VALUE >= 24.) THEN
                     OUT_ALL_DAY = .TRUE.
                  ELSEIF(DAILY_OUTAGE_VALUE > 0.) THEN

                     IF(SECOND_BLOCK_AVAIL(HR) <=.0001) THEN
                        OUT_BEGINNING_DAY = .TRUE.
                     ELSEIF(SECOND_BLOCK_AVAIL(HR+23) <= .0001) THEN
                        OUT_END_DAY = .TRUE.
                     ELSE
                        OUT_MIDDLE_DAY = .TRUE.
                     ENDIF
                  ENDIF
!
                  BEGINNING_HOURS_MARGIN = 0.
                  BEGINNING_HOURS_COST = 0.
                  BEGINNING_HOURS_PAYOFF = 0.
                  BEGINNING_HOURS_MWH(1) = 0.
                  BEGINNING_HOURS_MWH(2) = 0.
!
!                  IF(A < 1 .OR. A > TOTAL_START_UP_UNITS) THEN
!                     A = A
!                  ENDIF
!
                  IF(.NOT. OUT_ALL_DAY .AND.
     +                         .NOT. OUT_BEGINNING_DAY .AND.
     +                                       UNIT_UP_YESTERDAY(A)) THEN
                     BEGINNING_HOURS_TEST = .TRUE.
                  ELSE
                     BEGINNING_HOURS_TEST = .FALSE.
                  ENDIF
!
                  MORNING_CONSEC_ACTIVE = .TRUE.
                  ENCOUNTER_POSITIVE_MARGIN = .FALSE.
!
                  DO HR = 1, 24
!
                     HOUR_IN_MONTH = FIRST_HOUR_IN_DAY + HR - 1

                     BLOCK_CAPACITY(1) =
     +                                 FIRST_BLOCK_AVAIL(HOUR_IN_MONTH)
                     BLOCK_CAPACITY(2) =
     +                                SECOND_BLOCK_AVAIL(HOUR_IN_MONTH)
                     IF(BLOCK_CAPACITY(2) > 0.) THEN
!
                        BLOCK_TOTAL_PRICE(1) = BLOCK_INC_PRICE(1)
                        BLOCK_TOTAL_PRICE(2) =
     +                     (BLOCK_INC_PRICE(1)*BLOCK_CAPACITY(1)+
     +                        BLOCK_INC_PRICE(2)*BLOCK_CAPACITY(2))/
     +                           (BLOCK_CAPACITY(1)+BLOCK_CAPACITY(2))
!
                        BLOCK_CAPACITY(2) =
     +                            BLOCK_CAPACITY(1) + BLOCK_CAPACITY(2)
                        BLOCK_PRICE(1) = TEMP_BLOCK_PRICE(1)
                        BLOCK_PRICE(2) = TEMP_BLOCK_PRICE(2)
                     ELSE
!
                        BLOCK_TOTAL_PRICE(1) = 0.
                        BLOCK_TOTAL_PRICE(2) = BLOCK_INC_PRICE(1)
!
                        BLOCK_CAPACITY(2) = BLOCK_CAPACITY(1)
                        BLOCK_PRICE(2) = TEMP_BLOCK_PRICE(1)
!
                        BLOCK_PRICE(1) = 0.
                        BLOCK_CAPACITY(1) = 0.
!
                     ENDIF
!
                     IF(COMMIT_ON_TOTAL_COST) THEN
                        BLOCK_PRICE(1) = BLOCK_TOTAL_PRICE(1)
                        BLOCK_PRICE(2) = BLOCK_TOTAL_PRICE(2)
                     ENDIF
!
                     IF(BLOCK_CAPACITY(2) <= 0.) CYCLE
!
                    DAILY_AVAIL_CAPACITY(I) = DAILY_AVAIL_CAPACITY(I) +
     +                                                BLOCK_CAPACITY(2)
                    DAILY_AVAIL_CAPACITY(0) = DAILY_AVAIL_CAPACITY(0) +
     +                                                BLOCK_CAPACITY(2)
!
                     HOURS_AVAILABLE_IN_DAY(I) =
     +                                    HOURS_AVAILABLE_IN_DAY(I) + 1
                     HOURS_AVAILABLE_IN_DAY(0) =
     +                                    HOURS_AVAILABLE_IN_DAY(0) + 1
!
! CALCULATE HEAT RATE BY BLOCK
!
! CAN QUICKLY INCORPORATE:
!
!        MINIMUM DOWN TIME
!        MINIMUM UP TIME
!        RAMP UP
!        RAMP DOWN
!
!
! CAN FAIRLY QUICKLY INCORPORATE:
!
!        FUEL INVENTORY - BASED ON HOURLY MMBTU'S
!        EMISSIONS - BASED ON HOURLY MMBTU'S
!
!
                     IF(BLOCK_CAPACITY(1) > 0.) THEN
                        BLOCK1_MARGIN =
     +                     (DAILY_PRICE(HR) - BLOCK_PRICE(1)) *
     +                                                BLOCK_CAPACITY(1)
                     ELSE
                        BLOCK1_MARGIN = -999999.
                     ENDIF
                     BLOCK2_MARGIN =
     +                  (DAILY_PRICE(HR) - BLOCK_PRICE(2)) *
     +                                                BLOCK_CAPACITY(2)
                     IF(BLOCK1_MARGIN > BLOCK2_MARGIN) THEN
                        BEST_BLOCK(HR) = 1
                        HOURLY_MW(HR) = BLOCK_CAPACITY(1)
                        BEST_MARGIN(HR) = BLOCK1_MARGIN
                     ELSE
                        BEST_BLOCK(HR) = 2
                        HOURLY_MW(HR) = BLOCK_CAPACITY(2)
                        BEST_MARGIN(HR) = BLOCK2_MARGIN
                     ENDIF
                     BB = BEST_BLOCK(HR)
!
! TAKE ALL HOURS
!
                     ALL_HOURS_MARGIN = ALL_HOURS_MARGIN +
     +                                                  BEST_MARGIN(HR)
                     ALL_HOURS_COST = ALL_HOURS_COST +
     +                          BLOCK_CAPACITY(BB) *
     +                          BLOCK_PRICE(BB)
                     ALL_HOURS_PAYOFF = ALL_HOURS_PAYOFF +
     +                          BLOCK_CAPACITY(BB) *
     +                          DAILY_PRICE(HR)
                     ALL_HOURS_MWH(BB) = ALL_HOURS_MWH(BB) +
     +                                               BLOCK_CAPACITY(BB)
                     IF(BEGINNING_HOURS_TEST) THEN
                        IF(SECOND_BLOCK_OUT(HR) == 0.) THEN
                           BEGINNING_HOURS_MARGIN = ALL_HOURS_MARGIN
                           BEGINNING_HOURS_COST = ALL_HOURS_COST
                           BEGINNING_HOURS_PAYOFF = ALL_HOURS_PAYOFF
                           BEGINNING_HOURS_MWH(BB) = ALL_HOURS_MWH(BB)
                           LAST_BEGINNING_HOUR = HR
                        ELSE
                           BEGINNING_HOURS_TEST = .FALSE.
                        ENDIF
                     ENDIF
                     STRAT_HOURLY_MW(HR,1) = BLOCK_CAPACITY(BB)
                     IF(BB == 1) THEN
                        ALL_MIN_LOAD_HOURS = ALL_MIN_LOAD_HOURS + 1
                     ENDIF
! TAKE ONLY PROFITABLE HOURS
                     IF(BEST_MARGIN(HR) > 0.) THEN
                        POSITIVE_HOURS_MARGIN = POSITIVE_HOURS_MARGIN +
     +                                           BEST_MARGIN(HR)
                        POSITIVE_HOURS_COST = POSITIVE_HOURS_COST +
     +                          BLOCK_CAPACITY(BB) *
     +                          BLOCK_PRICE(BB)
                        POSITIVE_HOURS_PAYOFF = POSITIVE_HOURS_PAYOFF +
     +                          BLOCK_CAPACITY(BB) *
     +                          DAILY_PRICE(HR)
                        POSITIVE_HOURS_MWH(BB) =
     +                           POSITIVE_HOURS_MWH(BB) +
     +                           BLOCK_CAPACITY(BB)
                        STRAT_HOURLY_MW(HR,2) = BLOCK_CAPACITY(BB)
                        POSITIVE_HOURS = POSITIVE_HOURS + 1.
                        IF(BB == 1) THEN
                           POSITIVE_MIN_LOAD_HOURS =
     +                                      POSITIVE_MIN_LOAD_HOURS + 1
                        ENDIF
                        IF(HR == 1) THEN

                           IF(.NOT. UNIT_UP_YESTERDAY(A)) THEN
                              POSITIVE_HOURS_STARTS =
     +                                        POSITIVE_HOURS_STARTS + 1
                           ENDIF
                        ELSEIF(BEST_MARGIN(HR-1) < 0.) THEN
                           POSITIVE_HOURS_STARTS =
     +                                        POSITIVE_HOURS_STARTS + 1
                        ENDIF

                     ENDIF
!
                     DAILY_CUM_MARGIN(HR) = DAILY_CUM_MARGIN(HR) +
     +                                                  BEST_MARGIN(HR)

                     IF(POS_CONSEC_MARGIN == 0. .AND.
     +                                       BEST_MARGIN(HR) > 0.) THEN
                        START_CONSEC_HOUR = HR
                        POS_CONSEC_HOURS = POS_CONSEC_HOURS + 1.
                        IF(BB == 1) THEN
                           POS_CONSEC_MIN_LOAD_HOURS =
     +                                    POS_CONSEC_MIN_LOAD_HOURS + 1
                        ENDIF
                        POS_CONSEC_MARGIN = BEST_MARGIN(HR)
                        POS_CONSEC_COST = POS_CONSEC_COST +
     +                          BLOCK_CAPACITY(BB) *
     +                          BLOCK_PRICE(BB)
                        POS_CONSEC_PAYOFF = POS_CONSEC_PAYOFF +
     +                          BLOCK_CAPACITY(BB) *
     +                          DAILY_PRICE(HR)
                        POS_CONSEC_MWH(BB) = POS_CONSEC_MWH(BB) +

     +                                               BLOCK_CAPACITY(BB)
                        STRAT_HOURLY_MW(HR,3) = BLOCK_CAPACITY(BB)
                     ELSEIF(POS_CONSEC_MARGIN > 0.) THEN

                        POS_CONSEC_HOURS = POS_CONSEC_HOURS + 1.
                        IF(BB == 1) THEN
                           POS_CONSEC_MIN_LOAD_HOURS =
     +                                    POS_CONSEC_MIN_LOAD_HOURS + 1
                        ENDIF
                        POS_CONSEC_MARGIN =
     +                    POS_CONSEC_MARGIN  + BEST_MARGIN(HR)
                        POS_CONSEC_COST = POS_CONSEC_COST +
     +                          BLOCK_CAPACITY(BB) *
     +                          BLOCK_PRICE(BB)
                        POS_CONSEC_PAYOFF = POS_CONSEC_PAYOFF +
     +                          BLOCK_CAPACITY(BB) *
     +                          DAILY_PRICE(HR)
                        POS_CONSEC_MWH(BB) = POS_CONSEC_MWH(BB) +
     +                                               BLOCK_CAPACITY(BB)
                        STRAT_HOURLY_MW(HR,3) = BLOCK_CAPACITY(BB)

                     ENDIF
! 081507. EVALUATE A SHORTENED POS_CONSEC WITH GREATER MARGIN FOR ODEC
                     ACCUM_POS_CONSEC_MARGIN(HR) = POS_CONSEC_MARGIN
                     IF(ACCUM_POS_CONSEC_MARGIN(HR) >
     +                               BEST_ACCUM_POS_CONSEC_MARGIN) THEN
                        BEST_ACCUM_POS_CONSEC_MARGIN =
     +                                      ACCUM_POS_CONSEC_MARGIN(HR)
                        BEST_ACCUM_POS_CONSEC_HOURS = POS_CONSEC_HOURS
                     ENDIF

! TAKE CONSECUTIVE HOURS UNTIL FIRST NON-ECONOMIC HOUR ENCOUNTERED
!
                     IF(MORNING_CONSEC_ACTIVE) THEN
!
!
                        IF(BEST_MARGIN(HR) <= .001 .AND.
     +                                  ENCOUNTER_POSITIVE_MARGIN) THEN
                           MORNING_CONSEC_ACTIVE = .FALSE.
                        ELSE
!
                           IF(BEST_MARGIN(HR) > .001) THEN
                              ENCOUNTER_POSITIVE_MARGIN = .TRUE.
                           ENDIF
!
                           MORNING_CONSEC_HOURS =
     +                                        MORNING_CONSEC_HOURS + 1.
                           IF(BB == 1) THEN
                              MORNING_CONSEC_MIN_LOAD_HOURS =
     +                                MORNING_CONSEC_MIN_LOAD_HOURS + 1
                           ENDIF
                           MORNING_CONSEC_MARGIN =
     +                          MORNING_CONSEC_MARGIN + BEST_MARGIN(HR)
                           MORNING_CONSEC_COST = MORNING_CONSEC_COST +
     +                          BLOCK_CAPACITY(BB) *
     +                          BLOCK_PRICE(BB)
                           MORNING_CONSEC_PAYOFF =
     +                        MORNING_CONSEC_PAYOFF +
     +                          BLOCK_CAPACITY(BB) *
     +                          DAILY_PRICE(HR)
                           MORNING_CONSEC_MWH(BB) =
     +                      MORNING_CONSEC_MWH(BB) + BLOCK_CAPACITY(BB)
                           STRAT_HOURLY_MW(HR,4) = BLOCK_CAPACITY(BB)
                        ENDIF
                     ENDIF
                     IF(START_UP_LOGIC == 'M') THEN
                        STRAT_HOURLY_MW(HR,5) =
     +                                     PRE_COMMIT_MW(HOUR_IN_MONTH)
                     ENDIF
!
                  ENDDO ! !!! HOURS !!!

! THIS SECTION SEEMS TO DETECT FORCED OUTAGES AND THEN ALTERS THE 
! ALL_HOURS STRATEGY ACCORDINGLY

                  IF(.NOT. MUST_RUN_UNIT .AND.
     +                    (OUT_BEGINNING_DAY .OR. OUT_MIDDLE_DAY)) THEN
                     IF(OUT_MIDDLE_DAY .AND.
     +                     BEGINNING_HOURS_MARGIN > ALL_HOURS_MARGIN -
     +                                       START_UP_COSTS(S_U) ) THEN
                        ALL_HOURS_MARGIN = BEGINNING_HOURS_MARGIN
                        ALL_HOURS_COST = BEGINNING_HOURS_COST
                        ALL_HOURS_PAYOFF = BEGINNING_HOURS_PAYOFF
                        ALL_HOURS_MWH(BB) = BEGINNING_HOURS_MWH(BB)
                        DO HR = LAST_BEGINNING_HOUR + 1, 24
                           STRAT_HOURLY_MW(HR,1) = 0.
                        ENDDO
                     ENDIF
                     UNIT_UP_YESTERDAY(A) = .FALSE.
                  ELSEIF(.NOT. UNIT_UP_YESTERDAY(A)) THEN
                     ALL_HOURS_MARGIN =
     +                           ALL_HOURS_MARGIN - START_UP_COSTS(S_U)
                  ENDIF

                  POSITIVE_HOURS_MARGIN = POSITIVE_HOURS_MARGIN -
     +                      POSITIVE_HOURS_STARTS * START_UP_COSTS(S_U)


                  IF(START_CONSEC_HOUR > 1) THEN

                     POS_CONSEC_MARGIN =
     +                      POS_CONSEC_MARGIN - START_UP_COSTS(S_U)
                     BEST_ACCUM_POS_CONSEC_MARGIN =
     +                            BEST_ACCUM_POS_CONSEC_MARGIN -
     +                                              START_UP_COSTS(S_U)
                     ACCUM_POS_CONSEC_MARGIN =
     +                            ACCUM_POS_CONSEC_MARGIN -
     +                                              START_UP_COSTS(S_U)
                  ENDIF
!
! NEED TO TAKE NEXT_DAY_OPTIONALITY OUT OF POS_CONSEC AND INTO 
! MORNING_CONSEC
!
                  IF(.NOT. UNIT_UP_YESTERDAY(A)) THEN
                     MORNING_CONSEC_MARGIN =
     +                      MORNING_CONSEC_MARGIN -
     +                              START_UP_COSTS(S_U)
                  ENDIF

                  BEST_DAILY_GROSS = MAX(ALL_HOURS_MARGIN,
     +                                     POSITIVE_HOURS_MARGIN,
     +                                         POS_CONSEC_MARGIN,
     +                                           MORNING_CONSEC_MARGIN)
                  DAILY_GROSS(I) = MAX(0.,BEST_DAILY_GROSS)
! ONE, THREE, SEVEN DAY LOGIC
! 11/14/03. THERE ARE TWO ISSUES WITH MINIMUM DOWN TIME:
!           1. DON'T BRING UNITS DOWN IF THEY ARE GOING TO BE ECONOMIC 
! TOMORROW
!                 => (CUM GROSS MARGIN OF TODAY AND TOMORROW > 0.)
!           2. IF A UNIT IS DOWN, START MAKE SURE THAT IT DOESN'T 
! VIOLATE MIN DOWN TIME
!
                  SHUT_DOWN_UNIT = .FALSE.
                  IF(ALL_FUEL_SOURCE_EXHUASTED .OR. OUT_ALL_DAY) THEN
                        ! OUT ALL DAY DUE TO OUTAGE
                        SHUT_DOWN_UNIT = .TRUE.
                  ELSEIF(MUST_RUN_UNIT) THEN
!
! CANNOT SHUT DOWN => APPLY ALL HOURS STRATEGY
!
                  ELSEIF(START_UP_LOGIC == 'S') THEN
!
! 11/14/03.
!
                     IF( .NOT. UNIT_UP_YESTERDAY(A) .AND.
     +                        MIN_DOWN_TIME(S_U) >
     +                                 PREVIOUS_CONSEC_DOWN_HOURS) THEN

                        SHUT_DOWN_UNIT = .TRUE.
                     ELSEIF( CUM_WEEKLY_MARGIN(7) < 0. .OR.
     +                        (.NOT. UNIT_UP_YESTERDAY(A) .AND.
     +                CUM_WEEKLY_MARGIN(7) < START_UP_COSTS(S_U))) THEN
                        IF( UNIT_UP_YESTERDAY(A)) THEN
                           IF(MIN_UP_TIME(S_U) <=
     +                                  PREVIOUS_CONSEC_UP_HOURS ) THEN
                              SHUT_DOWN_UNIT = .TRUE.
                           ENDIF
                        ELSE
                           SHUT_DOWN_UNIT = .TRUE.
                        ENDIF
                     ENDIF
                  ELSEIF(START_UP_LOGIC == 'T') THEN
! IF ITS DOWN, KEEP IT DOWN FOR THE MIN DOWN TIME.
                     IF( .NOT. UNIT_UP_YESTERDAY(A) .AND.
     +                        MIN_DOWN_TIME(S_U) >
     +                                 PREVIOUS_CONSEC_DOWN_HOURS) THEN
                        SHUT_DOWN_UNIT = .TRUE.
                     ELSEIF( CUM_WEEKLY_MARGIN(3) < 0. .OR.
     +                        (.NOT. UNIT_UP_YESTERDAY(A) .AND.
     +                CUM_WEEKLY_MARGIN(3) < START_UP_COSTS(S_U))) THEN
! IF NOT PROFITABLE AND IT DOESN'T VIOLATE MIN UP TIME, THEN SHUT DOWN
!
! IF ITS UP, KEEP IT UP FOR THE MINIMUM UP TIME.
!
                        IF( UNIT_UP_YESTERDAY(A)) THEN
                           IF(MIN_UP_TIME(S_U) <=
     +                                  PREVIOUS_CONSEC_UP_HOURS ) THEN
                              SHUT_DOWN_UNIT = .TRUE.
                           ENDIF
                        ELSE
                           SHUT_DOWN_UNIT = .TRUE.
                        ENDIF
                     ENDIF
                  ELSEIF(START_UP_LOGIC == 'O') THEN
                     IF(DAILY_GROSS(I) == 0.) THEN
                        SHUT_DOWN_UNIT = .TRUE.
                     ELSEIF(MIN_UP_TIME(S_U) > 0.) THEN
!
! THIS SECTION ELMINATES STRATEGIES THAT DON'T MEET MIN_UP_TIME 
! REQUIREMENTS
!
                        IF(MIN_UP_TIME(S_U)-
     +                             PREVIOUS_CONSEC_UP_HOURS >= 24) THEN
                           POSITIVE_HOURS_MARGIN = -999999.
                           POS_CONSEC_MARGIN = -999999.
                           MORNING_CONSEC_MARGIN = -999999.
                        ENDIF
                       IF(MIN_UP_TIME(S_U) - POSITIVE_HOURS > .01) THEN
                           POSITIVE_HOURS_MARGIN = -999999.
                        ELSEIF(POSITIVE_HOURS_STARTS > 1) THEN
                           COUNT_CONSEC_HOURS = 0.
                           DO HR = 1, 24
                              IF(STRAT_HOURLY_MW(HR,2) > 0.) THEN
                                 COUNT_CONSEC_HOURS =
     +                                          COUNT_CONSEC_HOURS + 1.
                              ELSEIF(COUNT_CONSEC_HOURS > 0.01 .AND.
     +                                 MIN_UP_TIME(S_U) -
     +                                    COUNT_CONSEC_HOURS > .1) THEN
                                    POSITIVE_HOURS_MARGIN = -999999.
                                    EXIT
                              ELSE
                                 COUNT_CONSEC_HOURS = 0.
                              ENDIF
                           ENDDO
                        ENDIF
                        IF(MIN_UP_TIME(S_U) > POS_CONSEC_HOURS) THEN
                           POS_CONSEC_MARGIN = -999999.
! TEST TO SEE WHETHER THE POS_CONSEC_HOURS CAN BE TRIMMED TO INCREASE 
! PROFIT
                        ELSE

                              TEMP_I2 = 0
                              DO HR = 1, 24
                                 IF(STRAT_HOURLY_MW(HR,3) < .01) CYCLE
                                 TEMP_I2 = TEMP_I2 + 1
                                 IF(TEMP_I2 > MAX(MIN_UP_TIME(S_U),
     +                               BEST_ACCUM_POS_CONSEC_HOURS)) THEN
                                    STRAT_HOURLY_MW(HR,3) = 0.0
                                 ELSE
                                    POS_CONSEC_MARGIN =
     +                                      ACCUM_POS_CONSEC_MARGIN(HR)
                                 ENDIF
                              ENDDO
!                           ENDIF
                        ENDIF
                        IF(MIN_UP_TIME(S_U) >
     +                         MORNING_CONSEC_HOURS +
     +                               PREVIOUS_CONSEC_UP_HOURS .OR.
     +                                    WEEKLY_MARGIN(0,1) > 0.) THEN
                           MORNING_CONSEC_MARGIN = -999999.
                        ENDIF
                     ENDIF ! MINIMUM UP TIME
                     DAILY_GROSS(I) = MAX(0.,ALL_HOURS_MARGIN,
     +                                      POSITIVE_HOURS_MARGIN,
     +                                         POS_CONSEC_MARGIN,
     +                                           MORNING_CONSEC_MARGIN)

                     IF(DAILY_GROSS(I) == ALL_HOURS_MARGIN) THEN
                        DAILY_OPTION_STATUS(I,S_U) = 1
                     ELSEIF(DAILY_GROSS(I) ==
     +                                      POSITIVE_HOURS_MARGIN) THEN
                        DAILY_OPTION_STATUS(I,S_U) = 2
                     ELSEIF(DAILY_GROSS(I) == POS_CONSEC_MARGIN) THEN
                        DAILY_OPTION_STATUS(I,S_U) = 3
                     ELSEIF(DAILY_GROSS(I) ==
     +                                      MORNING_CONSEC_MARGIN) THEN
                        DAILY_OPTION_STATUS(I,S_U) = 4
                     ENDIF
!
                     IF(DAILY_GROSS(I) == 0.) SHUT_DOWN_UNIT = .TRUE.
!
                  ENDIF ! TYPE OF START-UP (ONE-DAY, THREE-DAY,...)
!
!
!                  IF(DAILY_GROSS(I) > 0. .AND. RAMP_RATE_ACTIVE) THEN
                  IF(.NOT. SHUT_DOWN_UNIT .AND. RAMP_RATE_ACTIVE) THEN
                     RAMPING_UP = .FALSE.
                     RAMPING_DOWN = .FALSE.
                     IF(MUST_RUN_UNIT) THEN
                        BB = 1
                     ELSE
                        BB = DAILY_OPTION_STATUS(I,S_U)
                     ENDIF
                     DO HR = 1, 24
!                        IF(STRAT_HOURLY_MW(HR,BB) == 0.) CYCLE
!
                        HOUR_IN_MONTH = FIRST_HOUR_IN_DAY + HR - 1
!
                        IF(.NOT. RAMPING_UP) THEN

                           IF(BEST_BLOCK(HR) < 2 .AND.
     +                                       HOURS_TO_RAMP_UP > 0) THEN
! TEST AHEAD FOR RAMP-UP
                          LAST_HOUR = MIN(HOURS_TO_RAMP_UP+HR,int(24,2))
                              DO J = HR, LAST_HOUR
                                 IF(BEST_BLOCK(J) < 2) CYCLE
                                 RAMPING_UP = .TRUE.
                                 RAMPING_DOWN = .FALSE.
                                 REMAINING_RAMP_UP_HOURS =
     +                                                 HOURS_TO_RAMP_UP
                                 REMAINING_RAMP_DOWN_HOURS = 0
                                 EXIT
                              ENDDO
!
! 12/18/03. TO ADDRESS ELENA PROBLEM
!
                           ELSEIF(.NOT. RAMPING_DOWN .AND.
     +                                       HOURS_TO_RAMP_UP > 0) THEN
                              RAMPING_UP = .TRUE.

                           ENDIF
                           IF(.NOT. RAMPING_UP .AND.
     +                              .NOT. RAMPING_DOWN .AND.
     +                                     HOURS_TO_RAMP_DOWN > 0) THEN
                              LAST_HOUR = MIN(HR + 1,24)

                              DO J = HR, LAST_HOUR
                                 IF(BEST_BLOCK(J) == 2) CYCLE
                                 RAMPING_UP = .FALSE.
                                 RAMPING_DOWN = .TRUE.
                                 REMAINING_RAMP_DOWN_HOURS = 1

                                 REMAINING_RAMP_UP_HOURS = 0
                                 EXIT
                              ENDDO
                           ENDIF
                        ENDIF ! .NOT. RAMPING_UP
!
! BASED UPON ABOVE, RAMP UP, RAMP DOWN, OR NEITHER
!
!                        IF(HR > 1) THEN
                        IF(RAMPING_UP) THEN
!                           IF(STRAT_HOURLY_MW(HR,BB) > 0.) THEN
                              IF(HR > 1) THEN
                                 STRAT_HOURLY_MW(HR,BB) =
     +                            MAX(FIRST_BLOCK_AVAIL(HOUR_IN_MONTH),
     +                                   MIN(STRAT_HOURLY_MW(HR-1,BB) +
     +                                                  RAMP_RATE(S_U),
     +                               FIRST_BLOCK_AVAIL(HOUR_IN_MONTH) +
     +                              SECOND_BLOCK_AVAIL(HOUR_IN_MONTH)))

                              ELSEIF(I > 1) THEN
                                 STRAT_HOURLY_MW(HR,BB) =
     +                                 MIN(
     +                                   GENERATION_BY_SEGMENT(BLOCK_1,
     +                                           FIRST_HOUR_IN_DAY-1) +
     +                                   GENERATION_BY_SEGMENT(BLOCK_2,
     +                                           FIRST_HOUR_IN_DAY-1) +
     +                                                  RAMP_RATE(S_U),
     +                               FIRST_BLOCK_AVAIL(HOUR_IN_MONTH) +
     +                               SECOND_BLOCK_AVAIL(HOUR_IN_MONTH))
                              ENDIF
                              REMAINING_RAMP_UP_HOURS =
     +                                      REMAINING_RAMP_UP_HOURS - 1
                              IF(REMAINING_RAMP_UP_HOURS == 0) THEN
                                 RAMPING_UP = .FALSE.
                              ENDIF

                        ELSEIF(RAMPING_DOWN) THEN ! TESTING

                              IF(HR > 1) THEN
                                 IF(FIRST_BLOCK_AVAIL(HOUR_IN_MONTH) ==
     +                                                          0. .OR.
     +                            (FIRST_BLOCK_AVAIL(HOUR_IN_MONTH-1) -
     +                             STRAT_HOURLY_MW(HR-1,BB) > .01 .AND.
     +                               FIRST_BLOCK_AVAIL(HOUR_IN_MONTH) -
     +                              STRAT_HOURLY_MW(HR,BB) > .01)) THEN
                                    STRAT_HOURLY_MW(HR,BB) = 0.
                                 ELSE
!
! 11/24/03.
!
! CASES:
!    1. ABOVE MIN AND RAMPING DOWN
!    2. AT MIN AND DONE RAMPING
!    3. AT ZERO AND DONE RAMPING
!    4. MUST RUN AND DON'T GO BELOW ZERO
!
                                    IF(MUST_RUN_UNIT .OR.
     +                                STRAT_HOURLY_MW(HR,BB) > 0.) THEN
                                       IF(RAMP_DOWN_RATE(S_U) <
     +                               FIRST_BLOCK_AVAIL(HOUR_IN_MONTH) +
     +                               SECOND_BLOCK_AVAIL(HOUR_IN_MONTH)
     +                                                           ) THEN
                                         STRAT_HOURLY_MW(HR,BB) =
     +                                    MAX(
     +                                   MIN(STRAT_HOURLY_MW(HR-1,BB) -
     +                                             RAMP_DOWN_RATE(S_U),
     +                               FIRST_BLOCK_AVAIL(HOUR_IN_MONTH) +
     +                              SECOND_BLOCK_AVAIL(HOUR_IN_MONTH)),
     +                               FIRST_BLOCK_AVAIL(HOUR_IN_MONTH))
                                       ELSE
! CAN SHUT OFF IN ONE HOUR. DON'T BOTHER TO RAMP DOWN
                                       ENDIF
                                    ELSE
                                       STRAT_HOURLY_MW(HR,BB) = 0.
                                    ENDIF
                                 ENDIF

                              ELSEIF(I > 1) THEN
                                TEMP_R = GENERATION_BY_SEGMENT(BLOCK_1,
     +                                           FIRST_HOUR_IN_DAY-1) +
     +                                   GENERATION_BY_SEGMENT(BLOCK_2,
     +                                            FIRST_HOUR_IN_DAY-1)

                                 IF(FIRST_BLOCK_AVAIL(HOUR_IN_MONTH) ==
     +                                                          0. .OR.
     +                            (FIRST_BLOCK_AVAIL(HOUR_IN_MONTH-1) -
     +                                               TEMP_R > .01 .AND.
     +                               FIRST_BLOCK_AVAIL(HOUR_IN_MONTH) -
     +                              STRAT_HOURLY_MW(HR,BB) > .01)) THEN
                                    STRAT_HOURLY_MW(HR,BB) = 0.
                                 ELSE
                                    STRAT_HOURLY_MW(HR,BB) =
     +                                 MAX(TEMP_R-RAMP_DOWN_RATE(S_U),
     +                                FIRST_BLOCK_AVAIL(HOUR_IN_MONTH))
                                 ENDIF
                              ENDIF
                              REMAINING_RAMP_DOWN_HOURS =
     +                                    REMAINING_RAMP_DOWN_HOURS - 1
!
                              IF(REMAINING_RAMP_DOWN_HOURS == 0) THEN
                                 RAMPING_DOWN = .FALSE.
                              ENDIF
!                           ENDIF ! OTHERWISE DON'T RAMP
                        ENDIF
!                        ENDIF ! HR > 1
                     ENDDO
!
! RECALCULATE DAILY GROSS BY STRATEGY
!
!                     DO HR = 1, 24
!                     ENDDO
                  ENDIF ! END OF RAMPING LOGIC

!
!
                  IF(SHUT_DOWN_UNIT) THEN
! RESOURCE FAILS DAILY TEST. BRING OFF-LINE AT BEGINNING OF DAY.
                     DAILY_OPTION_STATUS(I,S_U) = 0
                     UNIT_UP_YESTERDAY(A) = .FALSE.
                     OPTION_START_UP_COST(I) = 0.
                     STRIKES(I) = 0.
                  ELSE
                     IF(DAILY_GROSS(I) == ALL_HOURS_MARGIN .OR.
     +                              MUST_RUN_UNIT .OR.
     +                                      START_UP_LOGIC /= 'O') THEN
!
! RESOURCE HAS HIGH START-UP COSTS.
!      USE FOR ONLY ONE CONSECTIVE START
!      AND INCUR THE ONE START-UP COST.
! THIS IS A 1X24 OPTION
!
                        IF(.NOT. UNIT_UP_YESTERDAY(A)) THEN
                           MONTHLY_STARTS = MONTHLY_STARTS + 1
                           OPTION_START_UP_COST(I) = START_UP_COSTS(S_U)
                           STRIKES(I) = 1
                        ENDIF
              OPTION_HOURS = MIN(24.0,REAL(HOURS_AVAILABLE_IN_DAY(I),4))
                        MINIMUM_LOAD_HOURS = ALL_MIN_LOAD_HOURS
                        DAILY_OPTION_STATUS(I,S_U) = 1
                        UNIT_UP_YESTERDAY(A) = .TRUE.
                        OPTION_PAYOFF(I) = ALL_HOURS_PAYOFF
                        OPTION_COST(I) = ALL_HOURS_COST
                        OPTION_MWH(I) = ALL_HOURS_MWH(1) +
     +                                            ALL_HOURS_MWH(2)
                     ELSEIF(DAILY_GROSS(I) ==
     +                                      POSITIVE_HOURS_MARGIN) THEN
!
! RESOURCE HAS LOW (OR NO) START-UP COST.
!      USE ONLY IN POSITIVE HOURS
!      AND INCUR THE START-UP COST OF ALL STARTS.
! THIS IS A 1X1 OPTION
!
                        DAILY_OPTION_STATUS(I,S_U) = 2
                        IF(STRAT_HOURLY_MW(24,2) > 0.) THEN
                           UNIT_UP_YESTERDAY(A) = .TRUE.
                        ELSE
                           UNIT_UP_YESTERDAY(A) = .FALSE.
                        ENDIF
                        OPTION_HOURS = POSITIVE_HOURS
                        MINIMUM_LOAD_HOURS = POSITIVE_MIN_LOAD_HOURS
                        OPTION_PAYOFF(I) = POSITIVE_HOURS_PAYOFF
                        OPTION_COST(I) = POSITIVE_HOURS_COST
                        OPTION_MWH(I) = POSITIVE_HOURS_MWH(1) +
     +                                            POSITIVE_HOURS_MWH(2)
                        STRIKES(I) = POSITIVE_HOURS_STARTS
                        OPTION_START_UP_COST(I) =
     +                                 STRIKES(I) * START_UP_COSTS(S_U)
                     ELSEIF(DAILY_GROSS(I) == POS_CONSEC_MARGIN) THEN
!
! 11/12/03.
!
                        IF(START_CONSEC_HOUR > 1) THEN
                           MONTHLY_STARTS = MONTHLY_STARTS + 1
                           STRIKES(I) = 1
                          OPTION_START_UP_COST(I) = START_UP_COSTS(S_U)
                        ENDIF
!
                        DAILY_OPTION_STATUS(I,S_U) = 3
                        UNIT_UP_YESTERDAY(A) = .TRUE.
!
                        OPTION_HOURS = POS_CONSEC_HOURS
                        MINIMUM_LOAD_HOURS = POS_CONSEC_MIN_LOAD_HOURS
                        OPTION_PAYOFF(I) = POS_CONSEC_PAYOFF
                        OPTION_COST(I) = POS_CONSEC_COST
                        OPTION_MWH(I) = POS_CONSEC_MWH(1) +
     +                                            POS_CONSEC_MWH(2)
                     ELSEIF(DAILY_GROSS(I) ==
     +                                      MORNING_CONSEC_MARGIN) THEN

                        IF(.NOT. UNIT_UP_YESTERDAY(A)) THEN
                           MONTHLY_STARTS = MONTHLY_STARTS + 1
                           STRIKES(I) = 1
                          OPTION_START_UP_COST(I) = START_UP_COSTS(S_U)
                        ENDIF
!
                        DAILY_OPTION_STATUS(I,S_U) = 4
                        UNIT_UP_YESTERDAY(A) = .TRUE.
!
                        OPTION_HOURS = MORNING_CONSEC_HOURS
                        MINIMUM_LOAD_HOURS =
     +                                    MORNING_CONSEC_MIN_LOAD_HOURS
                        OPTION_PAYOFF(I) = MORNING_CONSEC_PAYOFF
                        OPTION_COST(I) = MORNING_CONSEC_COST
                        OPTION_MWH(I) = MORNING_CONSEC_MWH(1) +
     +                                            MORNING_CONSEC_MWH(2)
                     ENDIF
!
! 12/18/03. APPEND TOMORROW STRATEGY TO EXISTING STRATEGIES
!
                     BB = DAILY_OPTION_STATUS(I,S_U)
                     IF(BB > 1 .AND.
     +                           STRAT_HOURLY_MW(24,BB) < .01 .AND.
     +                                        I < R_DAYS_IN_MONTH) THEN
!
! FIRST: DETERMINE LAST UP HOUR
!
                        DO HR = 23, 1, -1
                           IF(STRAT_HOURLY_MW(HR,BB) < .01) CYCLE
                           EXIT
                        ENDDO
                        LOCAL_HOUR = (I-1)*24 + HR + 1
! SECOND: DETERMINE WHETHER STAYING UP IS MORE PROFITABLE THAN 
! INCURRING A
!        START TOMORROW (THIS IS WHERE IT WOULD BE USEFUL TO KNOW 
! ALL STRATEGIES IN ADVANCE
! THIRD: IF MORE PROFITABLE, THEN KEEP THE UNIT UP UNTIL THE END 
! OF THE DAY
!        AND SET UNIT_UP_YESTERDAY = TRUE
!
                        LAST_HOUR = (I+1)*24
                        KEEP_UP_TOMORROW_BENEFIT = 0.
                        AVOIDED_START_UP_COST = 0.
                        ALREADY_STARTED = .FALSE.
! TWO ISSUES: IS THE UNIT PROFITABLE TOMORROW?
!             IS IT MORE PROFITABLE WITH A START?
                        DO HR = LOCAL_HOUR, LAST_HOUR
                           BLOCK1_MARGIN =
     +                        (LOCAL_PRICE(HR) - BLOCK_PRICE(1)) *
     +                                                BLOCK_CAPACITY(1)
                           BLOCK2_MARGIN =
     +                        (LOCAL_PRICE(HR) - BLOCK_PRICE(2)) *
     +                                                BLOCK_CAPACITY(2)
!
                           TEMP_R = MAX(BLOCK1_MARGIN,BLOCK2_MARGIN)
!
                           IF(TEMP_R < .01 .AND.
     +                                      .NOT. ALREADY_STARTED) THEN
                              AVOIDED_START_UP_COST =
     +                                 AVOIDED_START_UP_COST - TEMP_R
                           ELSE
                              ALREADY_STARTED = .TRUE.
                           ENDIF
!
                           KEEP_UP_TOMORROW_BENEFIT =
     +                              KEEP_UP_TOMORROW_BENEFIT + TEMP_R
                           IF(KEEP_UP_TOMORROW_BENEFIT >
     +                                      AVOIDED_START_UP_COST .AND.
     +                              AVOIDED_START_UP_COST <
     +                                        START_UP_COSTS(S_U)) THEN
                              UNIT_UP_YESTERDAY(A) = .TRUE.
                              DO J = LOCAL_HOUR, 24
                                 BLOCK1_MARGIN =
     +                              (DAILY_PRICE(J) - BLOCK_PRICE(1)) *
     +                                                BLOCK_CAPACITY(1)
                                 BLOCK2_MARGIN =
     +                              (DAILY_PRICE(J) - BLOCK_PRICE(2)) *
     +                                                BLOCK_CAPACITY(2)
                                 IF(BLOCK1_MARGIN > BLOCK2_MARGIN) THEN
                                    STRAT_HOURLY_MW(J,BB) =
     +                                 MAX(BLOCK_CAPACITY(1),
     +                                     STRAT_HOURLY_MW(J-1,BB)-
     +                                             RAMP_DOWN_RATE(S_U))
                                 ELSE
                                    STRAT_HOURLY_MW(J,BB) =
     +                                  MIN(BLOCK_CAPACITY(2),
     +                                     STRAT_HOURLY_MW(J-1,BB) +
     +                                                  RAMP_RATE(S_U))
                                 ENDIF
                              ENDDO
!
                              EXIT
!
                           ENDIF
                        ENDDO
!
                     ENDIF
!
! 11/13/03. DAILY OPTION STATUS CANNOT CHANGE AFTER THIS POINT
                  ENDIF ! ONE DAY LOGIC
!
! NON-ECONOMIC START-UP'S FROM THE PREVIOUS DAY.
!
                  IF(OUT_ALL_DAY .OR. OUT_END_DAY) THEN
                     UNIT_UP_YESTERDAY(A) = .FALSE.
                  ENDIF
!
!
! ECONOMIC OUTAGE PER SWEET. 6/7/01. GAT.
! 11/13/03. PREVIOUS_CONSEC ADDED TO TRAP FOR PREVIOUS DAYS' UP HOURS 
! AND DOWN HOURS
!
                  BB = DAILY_OPTION_STATUS(I,S_U)
                  DO HR = 1, 24
!
                     IF(STRAT_HOURLY_MW(HR,BB) > 0. .AND.
     +                                     DAILY_GROSS(I) > 0.001) THEN
                        PREVIOUS_CONSEC_UP_HOURS =
     +                                     PREVIOUS_CONSEC_UP_HOURS + 1
                        PREVIOUS_CONSEC_DOWN_HOURS = 0
                        CYCLE
                     ELSE
                        PREVIOUS_CONSEC_DOWN_HOURS =
     +                                   PREVIOUS_CONSEC_DOWN_HOURS + 1
                        PREVIOUS_CONSEC_UP_HOURS = 0
                     ENDIF
!
!                     IF(STRAT_HOURLY_MW(HR,BB) > 0.) CYCLE
!
                     IF( SECOND_BLOCK_OUT(
     +                              FIRST_HOUR_IN_DAY+HR-1) == 0.) THEN
                        SECOND_BLOCK_OUT(FIRST_HOUR_IN_DAY+HR-1) = 4.
                     ENDIF
                  ENDDO
!
                  MINIMUM_ENERGY = OPTION_HOURS * BLOCK_CAPACITY(1)
                  MONTH_MINIMUM_ENERGY = MONTH_MINIMUM_ENERGY +
     +                                                   MINIMUM_ENERGY
                  OPTION_HEAT(I) =
     +                        COEFF(1,OPTION_INDEX) *
     +                                          MINIMUM_ENERGY +
     +                        (OPTION_MWH(I) - MINIMUM_ENERGY) *
     +                        (COEFF(2,OPTION_INDEX) +
     +                                       COEFF(3,OPTION_INDEX))/2.
!
! INTERUPTIBLE/FUEL_SWITCHING LOGIC
!
                  IF(UNIT_FUEL_INVENTORY_ACTIVE .AND.
     +                   .NOT. PRIM_FUEL_SOURCE_EXHUASTED .AND.
     +                             (PRIMARY_FUEL_CHEAPER .OR.
     +                                FUELMX(OPTION_INDEX) < 0.) ) THEN
!
! RESTRICT HOURLY UTILIZATION TO THE HOUR THAT INVENTORY IS EXHAUSTED
!
                     IF(MMBTU_FUEL_BALANCE -
     +                          DBLE(OPTION_HEAT(I)/1000.) < 0.D0) THEN
                        BB = DAILY_OPTION_STATUS(I,S_U)
                        DAILY_HEAT = 0.
                        DAILY_MWH = 0.
!
                        OPTION_INV_HEAT(I) = OPTION_INV_HEAT(I) +
     +                                               MMBTU_FUEL_BALANCE
!
                        DO HR = 1, 24
!
                           HOURLY_CAPACITY = STRAT_HOURLY_MW(HR,BB)
!
                           HOURLY_HEAT =
     +                        (COEFF(1,OPTION_INDEX) *
     +                                          BLOCK_CAPACITY(1) +
     +                        (HOURLY_CAPACITY - BLOCK_CAPACITY(1)) *
     +                        (COEFF(2,OPTION_INDEX) +
     +                                 COEFF(3,OPTION_INDEX))/2.)/1000.
!
                           IF( DBLE(DAILY_HEAT + HOURLY_HEAT) >
     +                                         MMBTU_FUEL_BALANCE) THEN
                              IF(SBTUCT(OPTION_INDEX) == 0.0) THEN
                                 DO J = HR, 24
                                    ! ELIMINATE GENERATION
                                    STRAT_HOURLY_MW(J,BB) = 0. 
                                 ENDDO
!
                                 OPTION_HEAT(I) = MMBTU_FUEL_BALANCE
                                 OPTION_MWH(I) = DAILY_MWH
                                 ALL_FUEL_SOURCE_EXHUASTED = .TRUE.
                              ELSE
                                 BLOCK_PRICE(1) = SECOND_PRICE(1)
                                 BLOCK_PRICE(2) = SECOND_PRICE(2)
                              ENDIF
!
                              MMBTU_FUEL_BALANCE = 0.D0
!
                              PRIM_FUEL_SOURCE_EXHUASTED = .TRUE.
!
                              EXIT
                           ELSE
                              DAILY_HEAT = DAILY_HEAT + HOURLY_HEAT
                              DAILY_MWH = DAILY_MWH + HOURLY_CAPACITY
                           ENDIF
                        ENDDO
!
                     ELSE
!
                        OPTION_INV_HEAT(I) = OPTION_INV_HEAT(I) +
     +                                             OPTION_HEAT(I)/1000.
!
                        MMBTU_FUEL_BALANCE = MAX( 0.D0,
     +                      MMBTU_FUEL_BALANCE -
     +                                   DBLE(OPTION_HEAT(I)/1000.) )
!
                     ENDIF
!
                     CALL REDUCE_LOCAL_MMBTU_FUEL(
     +                                      MMBTU_FUEL_BALANCE,FUEL_ID)
!
                  ENDIF
!
                  IF(OPTION_MWH(I) /= 0.) THEN
                     OPTION_STRIKE_PRICE(I) = OPTION_COST(I)/
     +                                                    OPTION_MWH(I)
                     OPTION_AVERAGE_HEAT = OPTION_HEAT(I)/
     +                                                    OPTION_MWH(I)
                  ELSE
                     OPTION_STRIKE_PRICE(I) = 0.
                     OPTION_AVERAGE_HEAT = 0.
                  ENDIF
!
                  CX_SUMMARY_REPORT = .TRUE.
                  IF(CX_SUMMARY_REPORT) THEN
!
                     IF(CX_REPORT_NOT_OPEN .AND.
     +                                       YES_DAILY_COMMITMENT) THEN
                        CX_REPORT_NOT_OPEN = .FALSE.
                        CX_REPORT_VARIABLES = 22 
                        CX_DAILY_NO =
     +                          CX_DAILY_HEADER(CX_REPORT_VARIABLES,
     +                                          CX_DAILY_REC)
                     ENDIF
! ACCUMULATE FOR MONTHLY
                     DAILY_OPTION_STATUS(0,S_U) =
     +                           DAILY_OPTION_STATUS(0,S_U) +
     +                                       DAILY_OPTION_STATUS(I,S_U)
                     OPTION_MWH(0) = OPTION_MWH(0) + OPTION_MWH(I)
                     OPTION_HEAT(0) = OPTION_HEAT(0) + OPTION_HEAT(I)
                     OPTION_INV_HEAT(0) = OPTION_INV_HEAT(0) +
     +                                               OPTION_INV_HEAT(I)
                     OPTION_PAYOFF(0) = OPTION_PAYOFF(0) +
     +                                                 OPTION_PAYOFF(I)
                     OPTION_COST(0) = OPTION_COST(0) + OPTION_COST(I)
                    OPTION_START_UP_COST(0) = OPTION_START_UP_COST(0) +
     +                                          OPTION_START_UP_COST(I)
                     DAILY_GROSS(0) = DAILY_GROSS(0) +
     +                                            DAILY_GROSS(I)
                     STRIKES(0) = STRIKES(0) + STRIKES(I)
!
                     MONTH_OPTION_HOURS = MONTH_OPTION_HOURS +
     +                                                     OPTION_HOURS
                     MONTH_MINIMUM_LOAD_HOURS =
     +                        MONTH_MINIMUM_LOAD_HOURS +
     +                                               MINIMUM_LOAD_HOURS
!
!
                     MONTH_THREE_DAY_MARGIN = MONTH_THREE_DAY_MARGIN +
     +                                             CUM_WEEKLY_MARGIN(3)
                     MONTH_SEVEN_DAY_MARGIN = MONTH_SEVEN_DAY_MARGIN +
     +                                             CUM_WEEKLY_MARGIN(7)
!
                     IF(OPTION_MWH(I) == 0.) THEN ! NO ENERGY TAKEN

                     ELSE
                        MONTH_ALL_HOURS_MARGIN =
     +                              MONTH_ALL_HOURS_MARGIN +
     +                                                 ALL_HOURS_MARGIN
                        MONTH_POSITIVE_HOURS_MARGIN =
     +                     MONTH_POSITIVE_HOURS_MARGIN +
     +                                            POSITIVE_HOURS_MARGIN
                        MONTH_POS_CONSEC_MARGIN =
     +                                 MONTH_POS_CONSEC_MARGIN +
     +                                                POS_CONSEC_MARGIN
                        MONTH_MORNING_CONSEC_MARGIN =
     +                             MONTH_MORNING_CONSEC_MARGIN +
     +                                            MORNING_CONSEC_MARGIN
                     ENDIF
!
                     IF(BLOCK_CAPACITY(2) > 0.) THEN
                        CAPACITY_FACTOR = OPTION_MWH(I) /
     +                                          (BLOCK_CAPACITY(2)*.24)
                     ELSE
                        CAPACITY_FACTOR = 0.
                     ENDIF
!
                     IF(HOURS_AVAILABLE_IN_DAY(I) > 0) THEN
                        DAILY_AVAILABILITY = DAILY_AVAIL_CAPACITY(I) /
     +                                 FLOAT(HOURS_AVAILABLE_IN_DAY(I))
                     ELSE
                        DAILY_AVAILABILITY = 0.
                     ENDIF
!
                     IF(THIS_YEAR == 0. .OR. gc_end_point == 0) THEN
                        HR = HR
                     ENDIF
!
                     IF(YES_DAILY_COMMITMENT .AND.
     +                          REPORT_THIS_CL_UNIT(OPTION_INDEX)) THEN
                       CX_DAILY_REC = rptrec(CX_DAILY_NO)
                       WRITE(CX_DAILY_NO,REC=CX_DAILY_REC)
     +                  PRT_ENDPOINT(),THIS_YEAR,R_MONTH_NAME,FLOAT(I),
     +                  OPTION_NAME(S_U),OPTION_STRIKE_PRICE(I),
     +                  OPTION_MWH(I),OPTION_PAYOFF(I),OPTION_COST(I),
     +                  DAILY_GROSS(I),
     +                  FLOAT(DAILY_OPTION_STATUS(I,S_U)),
     +                  OPTION_START_UP_COST(I),
     +                  OPTION_START_UP_COST(I) + OPTION_COST(I),
     +                  OPTION_HOURS,MINIMUM_LOAD_HOURS,
     +                  ALL_HOURS_MARGIN,POSITIVE_HOURS_MARGIN,
     +                  POS_CONSEC_MARGIN,STRIKES(I),
     +                  CUM_WEEKLY_MARGIN(3),CUM_WEEKLY_MARGIN(7),
     +                  CAPACITY_FACTOR,OPTION_AVERAGE_HEAT,
     +                  OPTION_INV_HEAT(I),DAILY_AVAILABILITY,
     +                  FLOAT(A),MORNING_CONSEC_MARGIN
                       CX_DAILY_REC = CX_DAILY_REC + 1
                     ENDIF
!
                     IF(I == R_DAYS_IN_MONTH) THEN
!
                        IF( OPTION_MWH(0) > 0.) THEN
                           OPTION_STRIKE_PRICE(0) = OPTION_COST(0)/
     +                                                    OPTION_MWH(0)
                           OPTION_AVERAGE_HEAT = OPTION_HEAT(0)/
     +                                                    OPTION_MWH(0)
                        ELSE
                           OPTION_STRIKE_PRICE(0) = 0.
                           OPTION_AVERAGE_HEAT = 0.
                        ENDIF
!
                        IF(BLOCK_CAPACITY(2) > 0.) THEN
                           CAPACITY_FACTOR = OPTION_MWH(0) /
     +                          (BLOCK_CAPACITY(2)*.24*R_DAYS_IN_MONTH)
                        ELSE
                           CAPACITY_FACTOR = 0.
                        ENDIF
!
                        IF(HOURS_AVAILABLE_IN_DAY(0) > 0) THEN
                           DAILY_AVAILABILITY =
     +                           DAILY_AVAIL_CAPACITY(0) /
     +                                 FLOAT(HOURS_AVAILABLE_IN_DAY(0))
                        ELSE
                           DAILY_AVAILABILITY = 0.
                        ENDIF
!
!
                        IF(YES_DAILY_COMMITMENT .AND.
     +                          REPORT_THIS_CL_UNIT(OPTION_INDEX)) THEN
                          CX_DAILY_REC = rptrec(CX_DAILY_NO)
                          WRITE(CX_DAILY_NO,REC=CX_DAILY_REC)
     +                     PRT_ENDPOINT(),THIS_YEAR,R_MONTH_NAME,
     +                     FLOAT(0),OPTION_NAME(S_U),
     +                     OPTION_STRIKE_PRICE(0),OPTION_MWH(0),
     +                     OPTION_PAYOFF(0),OPTION_COST(0),
     +                     DAILY_GROSS(0),
     +                     FLOAT(DAILY_OPTION_STATUS(0,S_U))/
     +                                                 R_DAYS_IN_MONTH,
     +                     OPTION_START_UP_COST(0),
     +                     OPTION_START_UP_COST(0) + OPTION_COST(0),
     +                     MONTH_OPTION_HOURS,MONTH_MINIMUM_LOAD_HOURS,
     +                     MONTH_ALL_HOURS_MARGIN,
     +                     MONTH_POSITIVE_HOURS_MARGIN,
     +                     MONTH_POS_CONSEC_MARGIN,STRIKES(0),
     +                     MONTH_THREE_DAY_MARGIN,
     +                     MONTH_SEVEN_DAY_MARGIN,CAPACITY_FACTOR,
     +                     OPTION_AVERAGE_HEAT,OPTION_INV_HEAT(0),
     +                     DAILY_AVAILABILITY,FLOAT(A),
     +                     MORNING_CONSEC_MARGIN
                          CX_DAILY_REC = CX_DAILY_REC + 1
                        ENDIF
            if(hours_in_month>744) then
                call end_program ("tf_objt:0007 - hours_in_month " //
     + "value of " // trim(itos(int(hours_in_month))) // 
     + " is invalid.")
            end if
                        IF(MONTH_MINIMUM_ENERGY == 0.) THEN
                           MONTH_MINIMUM_ENERGY =
     +                              OPTION_MWH(0)/FLOAT(HOURS_IN_MONTH)
                           MONTH_SECOND_ENERGY = 0.
                        ELSE
                           MONTH_SECOND_ENERGY =
     +                            (OPTION_MWH(0)-MONTH_MINIMUM_ENERGY)/
     +                                            FLOAT(HOURS_IN_MONTH)
                           MONTH_MINIMUM_ENERGY =
     +                       MONTH_MINIMUM_ENERGY/FLOAT(HOURS_IN_MONTH)
                        ENDIF

! 7/13/01. FOR MORE ACCURATE CALC OF HEAT RATE.
!
                        MONTH_LEFT_HEAT = 1.0
                        IF( MONTH_OPTION_HOURS > 0.) THEN
                           MONTH_RIGHT_HEAT = 1.-
     +                                        MONTH_MINIMUM_LOAD_HOURS/
     +                                               MONTH_OPTION_HOURS
                        ELSE
                           MONTH_RIGHT_HEAT = 1.0
                        ENDIF

! ANNUAL VALUES
!
                        ANNUAL_OPTION_COST(A) = ANNUAL_OPTION_COST(A) +
     +                                                   OPTION_COST(0)
                        ANNUAL_HOURS_AVAIL(A) = ANNUAL_HOURS_AVAIL(A) +
     +                                        HOURS_AVAILABLE_IN_DAY(0)
                        ANNUAL_AVAIL_CAPACITY(A) =
     +                           ANNUAL_AVAIL_CAPACITY(A) +
     +                                          DAILY_AVAIL_CAPACITY(0)
                        ANNUAL_OPTION_MWH(A,R_MONTH) =
     +                                   ANNUAL_OPTION_MWH(A,R_MONTH) +
     +                                                    OPTION_MWH(0)
                        ANNUAL_OPTION_HEAT(A,R_MONTH) =
     +                                  ANNUAL_OPTION_HEAT(A,R_MONTH) +
     +                                                   OPTION_HEAT(0)
                        ANNUAL_OPTION_INV_HEAT(A,R_MONTH) =
     +                              ANNUAL_OPTION_INV_HEAT(A,R_MONTH) +
     +                                               OPTION_INV_HEAT(0)
                        ANNUAL_OPTION_MWH(A,0) =
     +                                    ANNUAL_OPTION_MWH(A,0) +
     +                                                    OPTION_MWH(0)
                        ANNUAL_OPTION_HEAT(A,0) =
     +                                 ANNUAL_OPTION_HEAT(A,0) +
     +                                                   OPTION_HEAT(0)
                        ANNUAL_OPTION_INV_HEAT(A,0) =
     +                                 ANNUAL_OPTION_INV_HEAT(A,0) +
     +                                               OPTION_INV_HEAT(0)
                        ANNUAL_OPTION_PAYOFF(A,R_MONTH) =
     +                              ANNUAL_OPTION_PAYOFF(A,R_MONTH) +
     +                                                 OPTION_PAYOFF(0)
                        ANNUAL_OPTION_PAYOFF(A,0) =
     +                              ANNUAL_OPTION_PAYOFF(A,0) +
     +                                                 OPTION_PAYOFF(0)
                        ANNUAL_DAILY_GROSS(A,R_MONTH) =
     +                              ANNUAL_DAILY_GROSS(A,R_MONTH) +
     +                                                   DAILY_GROSS(0)
                        ANNUAL_DAILY_GROSS(A,0) =
     +                              ANNUAL_DAILY_GROSS(A,0) +
     +                                                   DAILY_GROSS(0)
                        ANNUAL_OPTION_STATUS(A) =
     +                              ANNUAL_OPTION_STATUS(A) +
     +                                       DAILY_OPTION_STATUS(0,S_U)
                        ANNUAL_OPTION_START_UP_COST(A) =
     +                              ANNUAL_OPTION_START_UP_COST(A) +
     +                                          OPTION_START_UP_COST(0)
                        ANNUAL_OPTION_HOURS(A) =
     +                              ANNUAL_OPTION_HOURS(A) +
     +                                               MONTH_OPTION_HOURS
                        ANNUAL_MINIMUM_LOAD_HOURS(A) =
     +                              ANNUAL_MINIMUM_LOAD_HOURS(A) +
     +                                         MONTH_MINIMUM_LOAD_HOURS
                        ANNUAL_STRIKES(A) =
     +                              ANNUAL_STRIKES(A) +
     +                                                  STRIKES(0)
                        ANNUAL_ALL_HOURS_MARGIN(A) =
     +                              ANNUAL_ALL_HOURS_MARGIN(A) +
     +                                           MONTH_ALL_HOURS_MARGIN
                        ANNUAL_POSITIVE_HOURS_MARGIN(A) =
     +                              ANNUAL_POSITIVE_HOURS_MARGIN(A) +
     +                                      MONTH_POSITIVE_HOURS_MARGIN
                        ANNUAL_POS_CONSEC_MARGIN(A) =
     +                              ANNUAL_POS_CONSEC_MARGIN(A) +
     +                                          MONTH_POS_CONSEC_MARGIN
                        ANNUAL_MORNING_CONSEC_MARGIN(A) =
     +                             ANNUAL_MORNING_CONSEC_MARGIN(A) +
     +                                      MONTH_MORNING_CONSEC_MARGIN
                        ANNUAL_THREE_DAY_MARGIN(A) =
     +                              ANNUAL_THREE_DAY_MARGIN(A) +
     +                                           MONTH_THREE_DAY_MARGIN
                        ANNUAL_SEVEN_DAY_MARGIN(A) =
     +                              ANNUAL_SEVEN_DAY_MARGIN(A) +
     +                                           MONTH_SEVEN_DAY_MARGIN
!
!
                     ENDIF
                  ENDIF
!
! 01/15/04. TESTING FOR ELENA
!
                  IF(START_UP_LOGIC == 'M') THEN
                     DAILY_OPTION_STATUS(I,S_U) = 5
                  ENDIF
!
                  BB = DAILY_OPTION_STATUS(I,S_U)
!
!                  IF(YES_HOURLY_COMMITMENT) THEN
!
!                     IF(HX_REPORT_NOT_OPEN) THEN
!                        HX_REPORT_NOT_OPEN = .FALSE.
!                        HX_REPORT_VARIABLES = 24
!                        HX_HOURLY_NO =
!     +                          HX_HOURLY_HEADER(HX_REPORT_VARIABLES,
!     +                                           HX_HOURLY_REC)
!                     ENDIF
!
!                     WRITE(HX_HOURLY_NO,REC=HX_HOURLY_REC)
!     +                  PRT_ENDPOINT(),
!     +                  THIS_YEAR,
!     +                  R_MONTH_NAME,
!     +                  FLOAT(I),
!     +                  OPTION_NAME,
!     +                  (STRAT_HOURLY_MW(J,BB),J=1,24)
!                     HX_HOURLY_REC = HX_HOURLY_REC + 1
!                  ENDIF
                  IF(YES_HOURLY_OUTAGE) THEN
!
                     IF(OX_REPORT_NOT_OPEN) THEN
                        OX_REPORT_NOT_OPEN = .FALSE.
                        OX_REPORT_VARIABLES = 24
                        OX_HOURLY_NO =
     +                          OX_HOURLY_HEADER(OX_REPORT_VARIABLES,
     +                                           OX_HOURLY_REC)
                        QX_REPORT_VARIABLES = 24
                        QX_HOURLY_NO =
     +                          QX_HOURLY_HEADER(QX_REPORT_VARIABLES,
     +                                           QX_HOURLY_REC)
                     ENDIF
!
                     HR = 24*(I-1) + 1
!
                     WRITE(TEMP_CHAR_12,*)
     +                            GET_HESI_UNIT_ID_NUM_I4(OPTION_INDEX)
!
                     QX_HOURLY_REC = rptrec(QX_HOURLY_NO)
                     WRITE(QX_HOURLY_NO,REC=QX_HOURLY_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        FLOAT(I),
     +                        TEMP_CHAR_12,
     +                  (SECOND_BLOCK_OUT(J),J=HR,HR+23)
                      QX_HOURLY_REC = QX_HOURLY_REC + 1
!
!
!                     WRITE(OX_HOURLY_NO,REC=OX_HOURLY_REC)
!     +                  PRT_ENDPOINT(),
!     +                  THIS_YEAR,
!     +                  R_MONTH_NAME,
!     +                  FLOAT(I),
!     +                  OPTION_NAME(S_U),
!     +                  (SECOND_BLOCK_OUT(J),J=HR,HR+23)
                     OX_HOURLY_REC = rptrec(OX_HOURLY_NO)
                     WRITE(OX_HOURLY_NO,REC=OX_HOURLY_REC)
     +                  PRT_ENDPOINT(),
     +                  THIS_YEAR,
     +                  R_MONTH_NAME,
     +                  FLOAT(I),
     +                  OPTION_NAME(S_U),
     +                  (FIRST_BLOCK_AVAIL(J)+
     +                                SECOND_BLOCK_AVAIL(J),J=HR,HR+23)
                     OX_HOURLY_REC = OX_HOURLY_REC + 1
                  ENDIF
                  J = FIRST_HOUR_IN_DAY
!
! GENERATION_BY_SEGMENT USED FOR WHOLESALE/RETAIL/OTHER, SPINNING,
! OTHER RESOURCE CONSIDERATIONS.
!
!
                  SORTED_OPTIONS(BLOCK_1) = BLOCK_1
                  SORTED_OPTIONS(BLOCK_2) = BLOCK_2
! 100506.
                  FLOOR_SORTED_OPTIONS(BLOCK_1) = BLOCK_1
                  CEILING_SORTED_OPTIONS(BLOCK_1) = BLOCK_1
                  FLOOR_SORTED_OPTIONS(BLOCK_2) = BLOCK_2
                  CEILING_SORTED_OPTIONS(BLOCK_2) = BLOCK_2
!
                  GLOBAL_BLOCK_INDEX(BLOCK_1) =
     +                   GET_TRANS_UNIT_TO_BLOCK(OPTION_INDEX,INT(1,2))
                  GLOBAL_BLOCK_INDEX(BLOCK_2) =
     +                   GET_TRANS_UNIT_TO_BLOCK(OPTION_INDEX,INT(2,2))
                  UNIT_FOR_OUTAGE_BLOCK(BLOCK_1) = OPTION_INDEX
                  S_U_FOR_OUTAGE_BLOCK(BLOCK_1) = S_U
                  UNIT_FOR_OUTAGE_BLOCK(BLOCK_2) = OPTION_INDEX
                  S_U_FOR_OUTAGE_BLOCK(BLOCK_2) = S_U
!
                  BLOCK_FOR_OUTAGE_BLOCK(BLOCK_1) = 1
                  BLOCK_FOR_OUTAGE_BLOCK(BLOCK_2) = 2

                  IF(DSM_MARKET_RESOURCE(S_U)) CYCLE
!
                  IF(R_MONTH == 3 .AND. globecom_YEAR == 6) THEN
                     HR = HR
                  ENDIF
!
                  GENERATION_BY_SEGMENT(BLOCK_1,J) =
     +                           MIN(STRAT_HOURLY_MW(1,BB),
     +                                            FIRST_BLOCK_AVAIL(J))
                  GENERATION_BY_SEGMENT(BLOCK_2,J) =
     +                           MAX(0.,STRAT_HOURLY_MW(1,BB) -
     +                                GENERATION_BY_SEGMENT(BLOCK_1,J))
                  GENERATION_BY_SEGMENT(BLOCK_1,J+1) =
     +                           MIN(STRAT_HOURLY_MW(2,BB),
     +                                          FIRST_BLOCK_AVAIL(J+1))
                  GENERATION_BY_SEGMENT(BLOCK_2,J+1) =
     +                           MAX(0.,STRAT_HOURLY_MW(2,BB) -
     +                              GENERATION_BY_SEGMENT(BLOCK_1,J+1))
                  GENERATION_BY_SEGMENT(BLOCK_1,J+2) =
     +                           MIN(STRAT_HOURLY_MW(3,BB),
     +                                          FIRST_BLOCK_AVAIL(J+2))
                  GENERATION_BY_SEGMENT(BLOCK_2,J+2) =
     +                           MAX(0.,STRAT_HOURLY_MW(3,BB) -
     +                              GENERATION_BY_SEGMENT(BLOCK_1,J+2))
                  GENERATION_BY_SEGMENT(BLOCK_1,J+3) =
     +                           MIN(STRAT_HOURLY_MW(4,BB),
     +                                          FIRST_BLOCK_AVAIL(J+3))
                  GENERATION_BY_SEGMENT(BLOCK_2,J+3) =
     +                           MAX(0.,STRAT_HOURLY_MW(4,BB) -
     +                              GENERATION_BY_SEGMENT(BLOCK_1,J+3))
                  GENERATION_BY_SEGMENT(BLOCK_1,J+4) =
     +                           MIN(STRAT_HOURLY_MW(5,BB),
     +                                          FIRST_BLOCK_AVAIL(J+4))
                  GENERATION_BY_SEGMENT(BLOCK_2,J+4) =
     +                           MAX(0.,STRAT_HOURLY_MW(5,BB) -
     +                              GENERATION_BY_SEGMENT(BLOCK_1,J+4))
                  GENERATION_BY_SEGMENT(BLOCK_1,J+5) =
     +                           MIN(STRAT_HOURLY_MW(6,BB),
     +                                          FIRST_BLOCK_AVAIL(J+5))
                  GENERATION_BY_SEGMENT(BLOCK_2,J+5) =
     +                           MAX(0.,STRAT_HOURLY_MW(6,BB) -
     +                              GENERATION_BY_SEGMENT(BLOCK_1,J+5))
                  GENERATION_BY_SEGMENT(BLOCK_1,J+6) =
     +                           MIN(STRAT_HOURLY_MW(7,BB),
     +                                          FIRST_BLOCK_AVAIL(J+6))
                  GENERATION_BY_SEGMENT(BLOCK_2,J+6) =
     +                           MAX(0.,STRAT_HOURLY_MW(7,BB) -
     +                              GENERATION_BY_SEGMENT(BLOCK_1,J+6))
                  GENERATION_BY_SEGMENT(BLOCK_1,J+7) =
     +                           MIN(STRAT_HOURLY_MW(8,BB),
     +                                          FIRST_BLOCK_AVAIL(J+7))
                  GENERATION_BY_SEGMENT(BLOCK_2,J+7) =
     +                           MAX(0.,STRAT_HOURLY_MW(8,BB) -
     +                              GENERATION_BY_SEGMENT(BLOCK_1,J+7))
                  GENERATION_BY_SEGMENT(BLOCK_1,J+8) =
     +                           MIN(STRAT_HOURLY_MW(9,BB),
     +                                          FIRST_BLOCK_AVAIL(J+8))
                  GENERATION_BY_SEGMENT(BLOCK_2,J+8) =
     +                           MAX(0.,STRAT_HOURLY_MW(9,BB) -
     +                              GENERATION_BY_SEGMENT(BLOCK_1,J+8))
                  GENERATION_BY_SEGMENT(BLOCK_1,J+9) =
     +                           MIN(STRAT_HOURLY_MW(10,BB),
     +                                          FIRST_BLOCK_AVAIL(J+9))
                  GENERATION_BY_SEGMENT(BLOCK_2,J+9) =
     +                           MAX(0.,STRAT_HOURLY_MW(10,BB) -
     +                              GENERATION_BY_SEGMENT(BLOCK_1,J+9))
                  GENERATION_BY_SEGMENT(BLOCK_1,J+10) =
     +                           MIN(STRAT_HOURLY_MW(11,BB),
     +                                         FIRST_BLOCK_AVAIL(J+10))
                  GENERATION_BY_SEGMENT(BLOCK_2,J+10) =
     +                           MAX(0.,STRAT_HOURLY_MW(11,BB) -
     +                             GENERATION_BY_SEGMENT(BLOCK_1,J+10))
                  GENERATION_BY_SEGMENT(BLOCK_1,J+11) =
     +                           MIN(STRAT_HOURLY_MW(12,BB),
     +                                         FIRST_BLOCK_AVAIL(J+11))
                  GENERATION_BY_SEGMENT(BLOCK_2,J+11) =
     +                           MAX(0.,STRAT_HOURLY_MW(12,BB) -
     +                             GENERATION_BY_SEGMENT(BLOCK_1,J+11))
                  GENERATION_BY_SEGMENT(BLOCK_1,J+12) =
     +                           MIN(STRAT_HOURLY_MW(13,BB),
     +                                         FIRST_BLOCK_AVAIL(J+12))
                  GENERATION_BY_SEGMENT(BLOCK_2,J+12) =
     +                           MAX(0.,STRAT_HOURLY_MW(13,BB) -
     +                             GENERATION_BY_SEGMENT(BLOCK_1,J+12))
                  GENERATION_BY_SEGMENT(BLOCK_1,J+13) =
     +                           MIN(STRAT_HOURLY_MW(14,BB),
     +                                         FIRST_BLOCK_AVAIL(J+13))
                  GENERATION_BY_SEGMENT(BLOCK_2,J+13) =
     +                           MAX(0.,STRAT_HOURLY_MW(14,BB) -
     +                             GENERATION_BY_SEGMENT(BLOCK_1,J+13))
                  GENERATION_BY_SEGMENT(BLOCK_1,J+14) =
     +                           MIN(STRAT_HOURLY_MW(15,BB),
     +                                         FIRST_BLOCK_AVAIL(J+14))
                  GENERATION_BY_SEGMENT(BLOCK_2,J+14) =
     +                           MAX(0.,STRAT_HOURLY_MW(15,BB) -
     +                             GENERATION_BY_SEGMENT(BLOCK_1,J+14))
                  GENERATION_BY_SEGMENT(BLOCK_1,J+15) =
     +                           MIN(STRAT_HOURLY_MW(16,BB),
     +                                         FIRST_BLOCK_AVAIL(J+15))
                  GENERATION_BY_SEGMENT(BLOCK_2,J+15) =
     +                           MAX(0.,STRAT_HOURLY_MW(16,BB) -
     +                             GENERATION_BY_SEGMENT(BLOCK_1,J+15))
                  GENERATION_BY_SEGMENT(BLOCK_1,J+16) =
     +                           MIN(STRAT_HOURLY_MW(17,BB),
     +                                         FIRST_BLOCK_AVAIL(J+16))
                  GENERATION_BY_SEGMENT(BLOCK_2,J+16) =
     +                           MAX(0.,STRAT_HOURLY_MW(17,BB) -
     +                             GENERATION_BY_SEGMENT(BLOCK_1,J+16))
                  GENERATION_BY_SEGMENT(BLOCK_1,J+17) =
     +                           MIN(STRAT_HOURLY_MW(18,BB),
     +                                         FIRST_BLOCK_AVAIL(J+17))
                  GENERATION_BY_SEGMENT(BLOCK_2,J+17) =
     +                           MAX(0.,STRAT_HOURLY_MW(18,BB) -
     +                             GENERATION_BY_SEGMENT(BLOCK_1,J+17))
                  GENERATION_BY_SEGMENT(BLOCK_1,J+18) =
     +                           MIN(STRAT_HOURLY_MW(19,BB),
     +                                         FIRST_BLOCK_AVAIL(J+18))
                  GENERATION_BY_SEGMENT(BLOCK_2,J+18) =
     +                           MAX(0.,STRAT_HOURLY_MW(19,BB) -
     +                             GENERATION_BY_SEGMENT(BLOCK_1,J+18))
                  GENERATION_BY_SEGMENT(BLOCK_1,J+19) =
     +                           MIN(STRAT_HOURLY_MW(20,BB),
     +                                         FIRST_BLOCK_AVAIL(J+19))
                  GENERATION_BY_SEGMENT(BLOCK_2,J+19) =
     +                           MAX(0.,STRAT_HOURLY_MW(20,BB) -
     +                             GENERATION_BY_SEGMENT(BLOCK_1,J+19))
                  GENERATION_BY_SEGMENT(BLOCK_1,J+20) =
     +                           MIN(STRAT_HOURLY_MW(21,BB),
     +                                         FIRST_BLOCK_AVAIL(J+20))
                  GENERATION_BY_SEGMENT(BLOCK_2,J+20) =
     +                           MAX(0.,STRAT_HOURLY_MW(21,BB) -
     +                             GENERATION_BY_SEGMENT(BLOCK_1,J+20))
                  GENERATION_BY_SEGMENT(BLOCK_1,J+21) =
     +                           MIN(STRAT_HOURLY_MW(22,BB),
     +                                         FIRST_BLOCK_AVAIL(J+21))
                  GENERATION_BY_SEGMENT(BLOCK_2,J+21) =
     +                           MAX(0.,STRAT_HOURLY_MW(22,BB) -
     +                             GENERATION_BY_SEGMENT(BLOCK_1,J+21))
                  GENERATION_BY_SEGMENT(BLOCK_1,J+22) =
     +                           MIN(STRAT_HOURLY_MW(23,BB),
     +                                         FIRST_BLOCK_AVAIL(J+22))
                  GENERATION_BY_SEGMENT(BLOCK_2,J+22) =
     +                           MAX(0.,STRAT_HOURLY_MW(23,BB) -
     +                             GENERATION_BY_SEGMENT(BLOCK_1,J+22))
                  GENERATION_BY_SEGMENT(BLOCK_1,J+23) =
     +                           MIN(STRAT_HOURLY_MW(24,BB),
     +                                         FIRST_BLOCK_AVAIL(J+23))
                  GENERATION_BY_SEGMENT(BLOCK_2,J+23) =
     +                           MAX(0.,STRAT_HOURLY_MW(24,BB) -
     +                             GENERATION_BY_SEGMENT(BLOCK_1,J+23))
!
!
                  SYSTEM_HOURLY_COMMITMENT(J,R_TG) =
     +                      SYSTEM_HOURLY_COMMITMENT(J,R_TG) +
     +                           GENERATION_BY_SEGMENT(BLOCK_1,J) +
     +                           GENERATION_BY_SEGMENT(BLOCK_2,J)
                  SYSTEM_HOURLY_COMMITMENT(J+1,R_TG) =
     +                      SYSTEM_HOURLY_COMMITMENT(J+1,R_TG) +
     +                           GENERATION_BY_SEGMENT(BLOCK_1,J+1) +
     +                           GENERATION_BY_SEGMENT(BLOCK_2,J+1)
                  SYSTEM_HOURLY_COMMITMENT(J+2,R_TG) =
     +                      SYSTEM_HOURLY_COMMITMENT(J+2,R_TG) +
     +                           GENERATION_BY_SEGMENT(BLOCK_1,J+2) +
     +                           GENERATION_BY_SEGMENT(BLOCK_2,J+2)
                  SYSTEM_HOURLY_COMMITMENT(J+3,R_TG) =
     +                      SYSTEM_HOURLY_COMMITMENT(J+3,R_TG) +
     +                           GENERATION_BY_SEGMENT(BLOCK_1,J+3) +
     +                           GENERATION_BY_SEGMENT(BLOCK_2,J+3)
                  SYSTEM_HOURLY_COMMITMENT(J+4,R_TG) =
     +                      SYSTEM_HOURLY_COMMITMENT(J+4,R_TG) +
     +                           GENERATION_BY_SEGMENT(BLOCK_1,J+4) +
     +                           GENERATION_BY_SEGMENT(BLOCK_2,J+4)
                  SYSTEM_HOURLY_COMMITMENT(J+5,R_TG) =
     +                      SYSTEM_HOURLY_COMMITMENT(J+5,R_TG) +
     +                           GENERATION_BY_SEGMENT(BLOCK_1,J+5) +
     +                           GENERATION_BY_SEGMENT(BLOCK_2,J+5)
                  SYSTEM_HOURLY_COMMITMENT(J+6,R_TG) =
     +                      SYSTEM_HOURLY_COMMITMENT(J+6,R_TG) +
     +                           GENERATION_BY_SEGMENT(BLOCK_1,J+6) +
     +                           GENERATION_BY_SEGMENT(BLOCK_2,J+6)
                  SYSTEM_HOURLY_COMMITMENT(J+7,R_TG) =
     +                      SYSTEM_HOURLY_COMMITMENT(J+7,R_TG) +
     +                           GENERATION_BY_SEGMENT(BLOCK_1,J+7) +
     +                           GENERATION_BY_SEGMENT(BLOCK_2,J+7)
                  SYSTEM_HOURLY_COMMITMENT(J+8,R_TG) =
     +                      SYSTEM_HOURLY_COMMITMENT(J+8,R_TG) +
     +                           GENERATION_BY_SEGMENT(BLOCK_1,J+8) +
     +                           GENERATION_BY_SEGMENT(BLOCK_2,J+8)
                  SYSTEM_HOURLY_COMMITMENT(J+9,R_TG) =
     +                      SYSTEM_HOURLY_COMMITMENT(J+9,R_TG) +
     +                           GENERATION_BY_SEGMENT(BLOCK_1,J+9) +
     +                           GENERATION_BY_SEGMENT(BLOCK_2,J+9)
                  SYSTEM_HOURLY_COMMITMENT(J+10,R_TG) =
     +                      SYSTEM_HOURLY_COMMITMENT(J+10,R_TG) +
     +                           GENERATION_BY_SEGMENT(BLOCK_1,J+10) +
     +                           GENERATION_BY_SEGMENT(BLOCK_2,J+10)
                  SYSTEM_HOURLY_COMMITMENT(J+11,R_TG) =
     +                      SYSTEM_HOURLY_COMMITMENT(J+11,R_TG) +
     +                           GENERATION_BY_SEGMENT(BLOCK_1,J+11) +
     +                           GENERATION_BY_SEGMENT(BLOCK_2,J+11)
                  SYSTEM_HOURLY_COMMITMENT(J+12,R_TG) =
     +                      SYSTEM_HOURLY_COMMITMENT(J+12,R_TG) +
     +                           GENERATION_BY_SEGMENT(BLOCK_1,J+12) +
     +                           GENERATION_BY_SEGMENT(BLOCK_2,J+12)

                  SYSTEM_HOURLY_COMMITMENT(J+13,R_TG) =
     +                      SYSTEM_HOURLY_COMMITMENT(J+13,R_TG) +
     +                           GENERATION_BY_SEGMENT(BLOCK_1,J+13) +
     +                           GENERATION_BY_SEGMENT(BLOCK_2,J+13)
                  SYSTEM_HOURLY_COMMITMENT(J+14,R_TG) =
     +                      SYSTEM_HOURLY_COMMITMENT(J+14,R_TG) +
     +                           GENERATION_BY_SEGMENT(BLOCK_1,J+14) +
     +                           GENERATION_BY_SEGMENT(BLOCK_2,J+14)
                  SYSTEM_HOURLY_COMMITMENT(J+15,R_TG) =
     +                      SYSTEM_HOURLY_COMMITMENT(J+15,R_TG) +
     +                           GENERATION_BY_SEGMENT(BLOCK_1,J+15) +
     +                           GENERATION_BY_SEGMENT(BLOCK_2,J+15)
                  SYSTEM_HOURLY_COMMITMENT(J+16,R_TG) =
     +                      SYSTEM_HOURLY_COMMITMENT(J+16,R_TG) +
     +                           GENERATION_BY_SEGMENT(BLOCK_1,J+16) +
     +                           GENERATION_BY_SEGMENT(BLOCK_2,J+16)
                  SYSTEM_HOURLY_COMMITMENT(J+17,R_TG) =
     +                      SYSTEM_HOURLY_COMMITMENT(J+17,R_TG) +
     +                           GENERATION_BY_SEGMENT(BLOCK_1,J+17) +
     +                           GENERATION_BY_SEGMENT(BLOCK_2,J+17)
                  SYSTEM_HOURLY_COMMITMENT(J+18,R_TG) =
     +                      SYSTEM_HOURLY_COMMITMENT(J+18,R_TG) +
     +                           GENERATION_BY_SEGMENT(BLOCK_1,J+18) +
     +                           GENERATION_BY_SEGMENT(BLOCK_2,J+18)
                  SYSTEM_HOURLY_COMMITMENT(J+19,R_TG) =
     +                      SYSTEM_HOURLY_COMMITMENT(J+19,R_TG) +
     +                           GENERATION_BY_SEGMENT(BLOCK_1,J+19) +
     +                           GENERATION_BY_SEGMENT(BLOCK_2,J+19)
                  SYSTEM_HOURLY_COMMITMENT(J+20,R_TG) =
     +                      SYSTEM_HOURLY_COMMITMENT(J+20,R_TG) +
     +                           GENERATION_BY_SEGMENT(BLOCK_1,J+20) +
     +                           GENERATION_BY_SEGMENT(BLOCK_2,J+20)
                  SYSTEM_HOURLY_COMMITMENT(J+21,R_TG) =
     +                      SYSTEM_HOURLY_COMMITMENT(J+21,R_TG) +
     +                           GENERATION_BY_SEGMENT(BLOCK_1,J+21) +
     +                           GENERATION_BY_SEGMENT(BLOCK_2,J+21)
                  SYSTEM_HOURLY_COMMITMENT(J+22,R_TG) =
     +                      SYSTEM_HOURLY_COMMITMENT(J+22,R_TG) +
     +                           GENERATION_BY_SEGMENT(BLOCK_1,J+22) +
     +                           GENERATION_BY_SEGMENT(BLOCK_2,J+22)
                  SYSTEM_HOURLY_COMMITMENT(J+23,R_TG) =
     +                      SYSTEM_HOURLY_COMMITMENT(J+23,R_TG) +
     +                           GENERATION_BY_SEGMENT(BLOCK_1,J+23) +
     +                           GENERATION_BY_SEGMENT(BLOCK_2,J+23)
!
                  IF(GENERATION_BY_SEGMENT(BLOCK_1,J) > 0.)
     +             SYSTEM_HOURLY_COMMIT_AVAIL(J,R_TG) =
     +                  SYSTEM_HOURLY_COMMIT_AVAIL(J,R_TG) +
     +                     FIRST_BLOCK_AVAIL(J) +
     +                        SECOND_BLOCK_AVAIL(J)
                  IF(GENERATION_BY_SEGMENT(BLOCK_1,J+1) > 0.)
     +               SYSTEM_HOURLY_COMMIT_AVAIL(J+1,R_TG) =
     +                  SYSTEM_HOURLY_COMMIT_AVAIL(J+1,R_TG) +
     +                        FIRST_BLOCK_AVAIL(J+1) +
     +                       SECOND_BLOCK_AVAIL(J+1)
                  IF(GENERATION_BY_SEGMENT(BLOCK_1,J+2) > 0.)
     +             SYSTEM_HOURLY_COMMIT_AVAIL(J+2,R_TG) =
     +                  SYSTEM_HOURLY_COMMIT_AVAIL(J+2,R_TG) +
     +                        FIRST_BLOCK_AVAIL(J+2) +
     +                       SECOND_BLOCK_AVAIL(J+2)
                  IF(GENERATION_BY_SEGMENT(BLOCK_1,J+3) > 0.)
     +             SYSTEM_HOURLY_COMMIT_AVAIL(J+3,R_TG) =
     +                  SYSTEM_HOURLY_COMMIT_AVAIL(J+3,R_TG) +
     +                        FIRST_BLOCK_AVAIL(J+3) +
     +                       SECOND_BLOCK_AVAIL(J+3)
                  IF(GENERATION_BY_SEGMENT(BLOCK_1,J+4) > 0.)
     +             SYSTEM_HOURLY_COMMIT_AVAIL(J+4,R_TG) =
     +                  SYSTEM_HOURLY_COMMIT_AVAIL(J+4,R_TG) +
     +                        FIRST_BLOCK_AVAIL(J+4) +
     +                       SECOND_BLOCK_AVAIL(J+4)
                  IF(GENERATION_BY_SEGMENT(BLOCK_1,J+5) > 0.)
     +             SYSTEM_HOURLY_COMMIT_AVAIL(J+5,R_TG) =
     +                  SYSTEM_HOURLY_COMMIT_AVAIL(J+5,R_TG) +
     +                        FIRST_BLOCK_AVAIL(J+5) +
     +                       SECOND_BLOCK_AVAIL(J+5)
                  IF(GENERATION_BY_SEGMENT(BLOCK_1,J+6) > 0.)
     +             SYSTEM_HOURLY_COMMIT_AVAIL(J+6,R_TG) =
     +                  SYSTEM_HOURLY_COMMIT_AVAIL(J+6,R_TG) +
     +                        FIRST_BLOCK_AVAIL(J+6) +
     +                       SECOND_BLOCK_AVAIL(J+6)
                  IF(GENERATION_BY_SEGMENT(BLOCK_1,J+7) > 0.)
     +             SYSTEM_HOURLY_COMMIT_AVAIL(J+7,R_TG) =
     +                  SYSTEM_HOURLY_COMMIT_AVAIL(J+7,R_TG) +
     +                        FIRST_BLOCK_AVAIL(J+7) +
     +                       SECOND_BLOCK_AVAIL(J+7)
                  IF(GENERATION_BY_SEGMENT(BLOCK_1,J+8) > 0.)
     +             SYSTEM_HOURLY_COMMIT_AVAIL(J+8,R_TG) =
     +                  SYSTEM_HOURLY_COMMIT_AVAIL(J+8,R_TG) +
     +                        FIRST_BLOCK_AVAIL(J+8) +
     +                       SECOND_BLOCK_AVAIL(J+8)
                  IF(GENERATION_BY_SEGMENT(BLOCK_1,J+9) > 0.)
     +             SYSTEM_HOURLY_COMMIT_AVAIL(J+9,R_TG) =
     +                  SYSTEM_HOURLY_COMMIT_AVAIL(J+9,R_TG) +
     +                        FIRST_BLOCK_AVAIL(J+9) +
     +                       SECOND_BLOCK_AVAIL(J+9)
                  IF(GENERATION_BY_SEGMENT(BLOCK_1,J+10) > 0.)
     +             SYSTEM_HOURLY_COMMIT_AVAIL(J+10,R_TG) =
     +                  SYSTEM_HOURLY_COMMIT_AVAIL(J+10,R_TG) +
     +                        FIRST_BLOCK_AVAIL(J+10) +
     +                       SECOND_BLOCK_AVAIL(J+10)
                  IF(GENERATION_BY_SEGMENT(BLOCK_1,J+11) > 0.)
     +             SYSTEM_HOURLY_COMMIT_AVAIL(J+11,R_TG) =
     +                  SYSTEM_HOURLY_COMMIT_AVAIL(J+11,R_TG) +
     +                        FIRST_BLOCK_AVAIL(J+11) +
     +                       SECOND_BLOCK_AVAIL(J+11)
                  IF(GENERATION_BY_SEGMENT(BLOCK_1,J+12) > 0.)
     +             SYSTEM_HOURLY_COMMIT_AVAIL(J+12,R_TG) =
     +                  SYSTEM_HOURLY_COMMIT_AVAIL(J+12,R_TG) +
     +                        FIRST_BLOCK_AVAIL(J+12) +
     +                       SECOND_BLOCK_AVAIL(J+12)
                  IF(GENERATION_BY_SEGMENT(BLOCK_1,J+13) > 0.)
     +             SYSTEM_HOURLY_COMMIT_AVAIL(J+13,R_TG) =
     +                  SYSTEM_HOURLY_COMMIT_AVAIL(J+13,R_TG) +
     +                        FIRST_BLOCK_AVAIL(J+13) +
     +                       SECOND_BLOCK_AVAIL(J+13)
                  IF(GENERATION_BY_SEGMENT(BLOCK_1,J+14) > 0.)
     +             SYSTEM_HOURLY_COMMIT_AVAIL(J+14,R_TG) =
     +                  SYSTEM_HOURLY_COMMIT_AVAIL(J+14,R_TG) +
     +                        FIRST_BLOCK_AVAIL(J+14) +
     +                       SECOND_BLOCK_AVAIL(J+14)
                  IF(GENERATION_BY_SEGMENT(BLOCK_1,J+15) > 0.)
     +             SYSTEM_HOURLY_COMMIT_AVAIL(J+15,R_TG) =
     +                  SYSTEM_HOURLY_COMMIT_AVAIL(J+15,R_TG) +
     +                        FIRST_BLOCK_AVAIL(J+15) +
     +                       SECOND_BLOCK_AVAIL(J+15)
                  IF(GENERATION_BY_SEGMENT(BLOCK_1,J+16) > 0.)
     +             SYSTEM_HOURLY_COMMIT_AVAIL(J+16,R_TG) =
     +                  SYSTEM_HOURLY_COMMIT_AVAIL(J+16,R_TG) +
     +                        FIRST_BLOCK_AVAIL(J+16) +
     +                       SECOND_BLOCK_AVAIL(J+16)
                  IF(GENERATION_BY_SEGMENT(BLOCK_1,J+17) > 0.)
     +             SYSTEM_HOURLY_COMMIT_AVAIL(J+17,R_TG) =
     +                  SYSTEM_HOURLY_COMMIT_AVAIL(J+17,R_TG) +
     +                        FIRST_BLOCK_AVAIL(J+17) +
     +                       SECOND_BLOCK_AVAIL(J+17)
                  IF(GENERATION_BY_SEGMENT(BLOCK_1,J+18) > 0.)
     +             SYSTEM_HOURLY_COMMIT_AVAIL(J+18,R_TG) =
     +                  SYSTEM_HOURLY_COMMIT_AVAIL(J+18,R_TG) +
     +                        FIRST_BLOCK_AVAIL(J+18) +
     +                       SECOND_BLOCK_AVAIL(J+18)
                  IF(GENERATION_BY_SEGMENT(BLOCK_1,J+19) > 0.)
     +             SYSTEM_HOURLY_COMMIT_AVAIL(J+19,R_TG) =
     +                  SYSTEM_HOURLY_COMMIT_AVAIL(J+19,R_TG) +
     +                        FIRST_BLOCK_AVAIL(J+19) +
     +                       SECOND_BLOCK_AVAIL(J+19)
                  IF(GENERATION_BY_SEGMENT(BLOCK_1,J+20) > 0.)
     +             SYSTEM_HOURLY_COMMIT_AVAIL(J+20,R_TG) =
     +                  SYSTEM_HOURLY_COMMIT_AVAIL(J+20,R_TG) +
     +                        FIRST_BLOCK_AVAIL(J+20) +
     +                       SECOND_BLOCK_AVAIL(J+20)
                  IF(GENERATION_BY_SEGMENT(BLOCK_1,J+21) > 0.)
     +             SYSTEM_HOURLY_COMMIT_AVAIL(J+21,R_TG) =
     +                  SYSTEM_HOURLY_COMMIT_AVAIL(J+21,R_TG) +
     +                        FIRST_BLOCK_AVAIL(J+21) +
     +                       SECOND_BLOCK_AVAIL(J+21)
                  IF(GENERATION_BY_SEGMENT(BLOCK_1,J+22) > 0.)
     +             SYSTEM_HOURLY_COMMIT_AVAIL(J+22,R_TG) =
     +                  SYSTEM_HOURLY_COMMIT_AVAIL(J+22,R_TG) +
     +                        FIRST_BLOCK_AVAIL(J+22) +
     +                       SECOND_BLOCK_AVAIL(J+22)
                  IF(GENERATION_BY_SEGMENT(BLOCK_1,J+23) > 0.)
     +             SYSTEM_HOURLY_COMMIT_AVAIL(J+23,R_TG) =
     +                  SYSTEM_HOURLY_COMMIT_AVAIL(J+23,R_TG) +
     +                        FIRST_BLOCK_AVAIL(J+23) +
     +                       SECOND_BLOCK_AVAIL(J+23)
!
                  DO K = 1, 24
                     IF(SYSTEM_HOURLY_COMMIT_AVAIL(J+K-1,R_TG) <
     +                      SYSTEM_HOURLY_COMMITMENT(J+K-1,R_TG) ) THEN
                        CAPACITY_FACTOR = CAPACITY_FACTOR
                     ENDIF
                  ENDDO
!
               ENDDO ! DAYS
               IF(START_UP_LOGIC == 'M') THEN
                  IF(THIS_YEAR == 2018 .AND. R_MONTH == 10) THEN
                     START_UP_LOGIC = START_UP_LOGIC
                  ENDIF
            if(hours_in_month>744) then
                call end_program ("tf_objt:0010 - hours_in_month " //
     + "value of " // trim(itos(int(hours_in_month))) // 
     + " is invalid.")
            end if

                  IF(PRE_COMMIT_MW(HOURS_IN_MONTH) > 0.001)THEN
                     UNIT_UP_YESTERDAY(A) = .TRUE.
                  ELSE
                     UNIT_UP_YESTERDAY(A) = .FALSE.
                  ENDIF
               ENDIF
            ENDDO ! UNITS
!
! MOVED 4/11/02.
!
            IF(R_MONTH == 12 .AND. YES_DAILY_COMMITMENT .AND.
     +                          REPORT_THIS_CL_UNIT(OPTION_INDEX)) THEN
!
!               DO A = 1, TOTAL_START_UP_UNITS
               DO OPTION_INDEX = 1, TOTAL_START_UP_UNITS
!
                  MONTHS_ACTIVE = 0.
                  TOTAL_CAPACITY = 0.
!
                  A = GET_START_UP_INDEX(OPTION_INDEX)
                  IF(A <= 0) CYCLE
                  ANNUAL_NAME = CLA_RETURN_UNITNM(OPTION_INDEX)

!
                  DO I = 1, 12
                     TEMP_CAPACITY  = GET_ANNUAL_CL_CAPACITY(
     +                                                     INT(2,2),A,I)
                     IF(TEMP_CAPACITY > 0.) THEN
                        TOTAL_CAPACITY = TOTAL_CAPACITY + TEMP_CAPACITY
                        MONTHS_ACTIVE = MONTHS_ACTIVE + 1
                     ENDIF
                  ENDDO
!
                  IF(MONTHS_ACTIVE == 0.) CYCLE
!
                  IF( ANNUAL_OPTION_MWH(A,0) > 0.) THEN
                     OPTION_STRIKE_PRICE(0) =
     +                              ANNUAL_OPTION_COST(A)/
     +                                           ANNUAL_OPTION_MWH(A,0)
                     OPTION_AVERAGE_HEAT =
     +                                       ANNUAL_OPTION_HEAT(A,0)/
     +                                           ANNUAL_OPTION_MWH(A,0)
                  ELSE
                     OPTION_STRIKE_PRICE(0) = 0.
                  ENDIF
!
                  IF(TOTAL_CAPACITY > 0.1) THEN
                     CAPACITY_FACTOR = ANNUAL_OPTION_MWH(A,0) *
     +                                                   MONTHS_ACTIVE/
     +                                           (TOTAL_CAPACITY*87.60)
                  ELSE
                     CAPACITY_FACTOR = 0.
                  ENDIF
                  IF(ANNUAL_HOURS_AVAIL(A) > 0) THEN
                              DAILY_AVAILABILITY =
     +                           ANNUAL_AVAIL_CAPACITY(A) /
     +                             FLOAT(ANNUAL_HOURS_AVAIL(A))
                  ELSE
                     DAILY_AVAILABILITY = 0.
                  ENDIF
!
!
                  CX_DAILY_REC = rptrec(CX_DAILY_NO)
                  WRITE(CX_DAILY_NO,REC=CX_DAILY_REC)
     +             PRT_ENDPOINT(),THIS_YEAR,'Annual   ',
     +             FLOAT(0),ANNUAL_NAME,OPTION_STRIKE_PRICE(0),
     +             ANNUAL_OPTION_MWH(A,0),ANNUAL_OPTION_PAYOFF(A,0),
     +             ANNUAL_OPTION_COST(A),ANNUAL_DAILY_GROSS(A,0),
     +             ANNUAL_OPTION_STATUS(A)/8760.,
     +             ANNUAL_OPTION_START_UP_COST(A),
     +             (ANNUAL_OPTION_START_UP_COST(A) +
     +                            ANNUAL_OPTION_COST(A)),
     +             ANNUAL_OPTION_HOURS(A),ANNUAL_MINIMUM_LOAD_HOURS(A),
     +             ANNUAL_ALL_HOURS_MARGIN(A),
     +             ANNUAL_POSITIVE_HOURS_MARGIN(A),
     +             ANNUAL_POS_CONSEC_MARGIN(A),ANNUAL_STRIKES(A),
     +             ANNUAL_THREE_DAY_MARGIN(A),
     +             ANNUAL_SEVEN_DAY_MARGIN(A),
     +             CAPACITY_FACTOR,OPTION_AVERAGE_HEAT,
     +             ANNUAL_OPTION_INV_HEAT(A,0),
     +             DAILY_AVAILABILITY,FLOAT(A),
     +             ANNUAL_MORNING_CONSEC_MARGIN(A)

                  CX_DAILY_REC = CX_DAILY_REC + 1
               ENDDO
!
            ENDIF
!
!          MAYBE MOVE THIS ROUTINE BACK INTO PRICE MODULE
!
! CREATE AN INDEX BASED UPON THE GLOBAL BLOCK POSITION
!
!
! SORT ADDITIONS HIGHEST TO LOWEST
!
            CALL Int2_Sort(               MAX_OUTAGE_BLOCKS,
     +                                    SORTED_OPTIONS,
     +                                    GLOBAL_BLOCK_INDEX)
!
! SORT COMMITMENT UNITS LOWEST TO HIGHEST
!
            CALL INDEXED_SORT_ASCENDING(START_UP_COST_PER_MWH,
     +                                  MINIMUM_UP_INDEX,
     +                                  NO_START_UP_UNITS,
     +                                  ASCENDING)
!
! GENERATION AND WHOLESALE/RETAIL CALCULATIONS
!
!
!
! SORT U INDEX HIGHEST TO LOWEST
!
            CALL Int2_Sort(               NO_START_UP_UNITS,
     +                                    INCREASING_VALUE_OF_U,
     +                                    ORIGINAL_VALUE_OF_U)

            LOCAL_SPIN = 0.
            DEPTH_QUANTITY = 0.
            DEPTH_PRICE = 0.
            DEPTH_MARGINAL_COST = 0.
            DEPTH_RESOURCES = 0.
!
! 07/22/03. EXPANDED TO TRANSACT C THIS MIGHT BE A PROBLEM WITH MEMORY
!
            TEMP_L = MONTHLY_CALL_PUT_CAPACITY(
     +                     SYSTEM_DERIVATIVES(0,R_TG), ! (24,31)
     +                     R_MONTH,
     +                     R_TG,
     +                     R_DAYS_IN_MONTH,
     +                     SYSTEM_AVAIL_DERIVATIVES(0,R_TG))
!  072917.
            CX_STORAGE = .TRUE.
            IF(CX_STORAGE) THEN
!
! 092121.
!
               TEMP_L = GET_GRX_TRANS_MONTHLY_STORAGE(
     +                                    R_TG,
     +                                    R_MONTH,
     +                                    SYSTEM_STORAGE(0,R_TG))

            ELSE
               TEMP_L = DailyOperMoPumpedStorage(
     +                     R_TG,
     +                     L_MONTHLY_PRICE,   ! (24,31) hourly detail 
     +                 SYSTEM_STORAGE(0,R_TG), ! (24,31) hourly detail 
     +                     R_MONTH,
     +                     R_DAYS_IN_MONTH,
     +                     SYSTEM_AVAIL_STORAGE(0,R_TG))
            ENDIF
!
!            DAY = 1
            HR = 1

            CALL INDEXED_SORT_ASCENDING(FLOOR_PRICE_FOR_BLOCK,
     +                                  FLOOR_SORTED_OPTIONS,
     +                                  MAX_OUTAGE_BLOCKS,
     +                                  ASCENDING)
            CALL INDEXED_SORT_ASCENDING(CEILING_PRICE_FOR_BLOCK,
     +                                  CEILING_SORTED_OPTIONS,
     +                                  MAX_OUTAGE_BLOCKS,
     +                                  ASCENDING)
!
            IF(get_BASE_YEAR()+globecom_YEAR == 2021) THEN
               I = I
            ENDIF
!
            DO HOUR = 1, HOURS_IN_MONTH
!
               REMAIN = MOD(FLOAT(HOUR),24.)
               IF(REMAIN < .001) THEN ! NEW DAY
                  DAY = HOUR/24
               ELSE
                  DAY = HOUR/24 + 1
               ENDIF
! 021610
               IF(globecom_YEAR == 6 .AND. R_MONTH == 1 
     +    .AND. HOUR == 186) THEN
                  DAY = DAY
               ENDIF
! 04/17/03.
               CALENDAR_DAY_OF_WEEK = GET_DAY_OF_WEEK_4(R_MONTH,DAY)

               IS_5X16 = APPLY_ENERGY_PRODUCT(
     +                                   HR,
     +                                   CALENDAR_DAY_OF_WEEK,
     +                                   '5x16                ')
!
! PATH CAPACITY ADDED. 06/29/01.
!
               IF(IS_5X16) THEN
                  PEAK_HOUR = 1
!
                  VOID_LOGICAL =
     +       INIT_HOUR_PATH_LIMIT(PEAK_HOUR,R_MONTH,globecom_YEAR)
!
                  LOCAL_BUY_WHEEL = ON_PEAK_BUY_WHEEL
                  LOCAL_BUY_SPREAD = MAX(0.,ON_PEAK_BUY_SPREAD -
     + PATH_WHEELING_CHARGE(MARKET_TG,R_TG,
     +                    PEAK_HOUR,R_MONTH,globecom_YEAR))
                  LOCAL_SELL_SPREAD = MAX( 0.,ON_PEAK_SELL_SPREAD -
     +    PATH_WHEELING_CHARGE(R_TG,MARKET_TG,
     +                       PEAK_HOUR,R_MONTH,globecom_YEAR))
                  LOCAL_SECOND_BUY_WHEEL = ON_PEAK_SECOND_BUY_WHEEL
               ELSE
                  PEAK_HOUR = 2
!
                  VOID_LOGICAL =
     +      INIT_HOUR_PATH_LIMIT(PEAK_HOUR,R_MONTH,globecom_YEAR)
!
                  LOCAL_BUY_WHEEL = OFF_PEAK_BUY_WHEEL
                  LOCAL_BUY_SPREAD = MAX(0.,OFF_PEAK_BUY_SPREAD -
     +          PATH_WHEELING_CHARGE(MARKET_TG,R_TG,
     +                             PEAK_HOUR,R_MONTH,globecom_YEAR))
       LOCAL_SELL_SPREAD = MAX( 0.,OFF_PEAK_SELL_SPREAD -
     +          PATH_WHEELING_CHARGE(R_TG,MARKET_TG,
     +                             PEAK_HOUR,R_MONTH,globecom_YEAR))
                  LOCAL_SECOND_BUY_WHEEL = OFF_PEAK_SECOND_BUY_WHEEL
               ENDIF
!
               IF(MULTI_MARKET_ACTIVE) THEN
                  MAXIMUM_FIRST_EXPORT_TIE = 999999.
                  MAXIMUM_SECOND_IMPORT_TIE = 0.
               ELSE
                  MAXIMUM_FIRST_EXPORT_TIE =
     +                     GET_SEASON_PATH_LIMIT(R_TG,MARKET_TG) *
     +                           GET_CONSTRAINT_MULT(R_TG,
     +                                               MARKET_TG,
     +                                               PEAK_HOUR,
     +                                               R_MONTH,
     +                                               globecom_YEAR)
!     +                         HOUR_TIE_LIMIT(R_TG,
!     +                                        MARKET_TG)
                  MAXIMUM_FIRST_IMPORT_TIE =
     +                     GET_SEASON_PATH_LIMIT(MARKET_TG,R_TG) *
     +                           GET_CONSTRAINT_MULT(MARKET_TG,
     +                                               R_TG,
     +                                               PEAK_HOUR,
     +                                               R_MONTH,
     +                                               globecom_YEAR)
!     +                         HOUR_TIE_LIMIT(MARKET_TG,
!     +                                        R_TG)
!
! IF SECONDS NOT DEFINED, THEN RESET TO ZERO
!
                  MAXIMUM_SECOND_EXPORT_TIE =
     +                   GET_SEASON_PATH_LIMIT(R_TG,SECOND_MARKET_TG) *
     +                           GET_CONSTRAINT_MULT(R_TG,
     +                                               SECOND_MARKET_TG,
     +                                               PEAK_HOUR,
     +                                               R_MONTH,
     +                                               globecom_YEAR)
!     +                         HOUR_TIE_LIMIT(R_TG,
!     +                                        SECOND_MARKET_TG)
                  IF(MAXIMUM_SECOND_EXPORT_TIE >= 999998.9) THEN
                     MAXIMUM_SECOND_EXPORT_TIE = 0.
                  ENDIF
                  MAXIMUM_SECOND_IMPORT_TIE =
     +                   GET_SEASON_PATH_LIMIT(SECOND_MARKET_TG,R_TG) *
     +                           GET_CONSTRAINT_MULT(SECOND_MARKET_TG,
     +                                               R_TG,
     +                                               PEAK_HOUR,
     +                                               R_MONTH,
     +                                               globecom_YEAR)
!     +                         HOUR_TIE_LIMIT(SECOND_MARKET_TG,
!     +                                        R_TG)
                  IF(MAXIMUM_SECOND_IMPORT_TIE >= 999998.9) THEN
                     MAXIMUM_SECOND_IMPORT_TIE = 0.
                  ENDIF
               ENDIF
!
               MAXIMUM_IMPORT_TIE = MAXIMUM_FIRST_IMPORT_TIE +
     +                                      MAXIMUM_SECOND_IMPORT_TIE
               MAXIMUM_EXPORT_TIE = MAXIMUM_FIRST_EXPORT_TIE +
     +                                        MAXIMUM_SECOND_EXPORT_TIE
!
               DEPTH_HOURLY_DISPATCH_MW = 0.
!
! 10/11/01. GAT. DAILY OPTION FROM THE ENERGY PRODUCTS FILE.
!
               IF(HR == 1) THEN
!
                  HOURLY_LOAD_FOLLOW = 0.
!
                  DAILY_PEAK = 0.
                  HOURLY_AC_LOAD = 0.
                  DAILY_BUY_WHEEL = 0.
                  DAILY_SELL_WHEEL = 0.
!
                  PRICING_GROUP_SELL_GEN = 0.
                  ALL_DEMAND_BUY_GEN = 0.
!                  PRICING_GROUP_BUY_GEN = 0.
!
                  DO J = 1, 24
                     TEMP_I2 = HOUR+J-1
                     TEMP_R = GET_TRANS_LOAD_AFTER_EL(TEMP_I2,R_TG)
                     DAILY_PEAK = MAX(DAILY_PEAK,TEMP_R)
                  ENDDO
!
                  DAILY_RESERVE_ENERGY = 0.
!
                  DAILY_GEN_BY_S_U = 0.
                  DAILY_FUEL_BY_S_U = 0.
                  DAILY_SYSTEM_COST_AND_REV = 0.
!
                  DAILY_COST_BY_S_U = 0.
                  DAILY_SPIN_BY_S_U = 0.
!
                  DAILY_DSM_MW = 0.
!
! 1/9/02. CHANGED FROM LOCAL PRICE TO L_MONTHLY_PRICE FOR DERIV CALC'S
!DAILY_PRICE(1:24) = L_MONTHLY_PRICE(HOUR:HOUR+23) !USE TO REPLACE BELOW
                  DAILY_PRICE(1) = L_MONTHLY_PRICE(HOUR)
                  DAILY_PRICE(2) = L_MONTHLY_PRICE(HOUR+1)
                  DAILY_PRICE(3) = L_MONTHLY_PRICE(HOUR+2)
                  DAILY_PRICE(4) = L_MONTHLY_PRICE(HOUR+3)
                  DAILY_PRICE(5) = L_MONTHLY_PRICE(HOUR+4)
                  DAILY_PRICE(6) = L_MONTHLY_PRICE(HOUR+5)
                  DAILY_PRICE(7) = L_MONTHLY_PRICE(HOUR+6)
                  DAILY_PRICE(8) = L_MONTHLY_PRICE(HOUR+7)
                  DAILY_PRICE(9) = L_MONTHLY_PRICE(HOUR+8)
                  DAILY_PRICE(10) = L_MONTHLY_PRICE(HOUR+9)
                  DAILY_PRICE(11) = L_MONTHLY_PRICE(HOUR+10)
                  DAILY_PRICE(12) = L_MONTHLY_PRICE(HOUR+11)
                  DAILY_PRICE(13) = L_MONTHLY_PRICE(HOUR+12)
                  DAILY_PRICE(14) = L_MONTHLY_PRICE(HOUR+13)
                  DAILY_PRICE(15) = L_MONTHLY_PRICE(HOUR+14)
                  DAILY_PRICE(16) = L_MONTHLY_PRICE(HOUR+15)
                  DAILY_PRICE(17) = L_MONTHLY_PRICE(HOUR+16)
                  DAILY_PRICE(18) = L_MONTHLY_PRICE(HOUR+17)
                  DAILY_PRICE(19) = L_MONTHLY_PRICE(HOUR+18)
                  DAILY_PRICE(20) = L_MONTHLY_PRICE(HOUR+19)
                  DAILY_PRICE(21) = L_MONTHLY_PRICE(HOUR+20)
                  DAILY_PRICE(22) = L_MONTHLY_PRICE(HOUR+21)
                  DAILY_PRICE(23) = L_MONTHLY_PRICE(HOUR+22)
                  DAILY_PRICE(24) = L_MONTHLY_PRICE(HOUR+23)
!
!
                  DAILY_CUM_DISPATCH_MW = 0.
                  DAILY_CUM_DISPATCH_COST = 0.
!
                  DUMP_ENERGY = 0.
!
                  YES_DAILY_CALL_PUT_CAPACITY =
     +                  DAILY_CALL_PUT_CAPACITY(
     +                              CALENDAR_DAY_OF_WEEK,
     +                              DAY,R_TG,
     +                              DAILY_PRICE,
     +                              DAILY_PRODUCTS_CAPACITY,
     +                              R_MONTH,
     +                              SYSTEM_AVAIL_DERIVATIVES(0,R_TG))
!
! 5/17/02. DAILY MUST FOR TWO REASONS:
!           1. MUST RUN ON THE FIRST BLOCK
!           2. MUST TAKE THE MARKET RESULT
!
                  MONTH_MUST_BY_BLOCK = 0.
                  DAILY_HARD_WIRED_BY_BLOCK = 0.
                  DAILY_MUST_RUN_CAPACITY = 0.
                  DAILY_EMERGENCY_CAPACITY = 0.
!
                  J = HOUR
!
                  DO I = 1, MAX_OUTAGE_BLOCKS
                     OUTAGE_BLOCK = SORTED_OPTIONS(I)

                     S_U = S_U_FOR_OUTAGE_BLOCK(OUTAGE_BLOCK)
                     B = BLOCK_FOR_OUTAGE_BLOCK(OUTAGE_BLOCK)
!
                     OUTAGE_BLOCK_BY_SEGMENT(B,S_U) = OUTAGE_BLOCK
! 07/08/02.
                     IF(TVA_MARKET_RESOURCE(S_U) .OR.
     +                                   GRE_MARKET_RESOURCE(S_U)) THEN

                        CALL GetCapUsedForBlock(
     +                        MARKET_RESOURCE_COUNTER(S_U),
     +                        B,
     +                        R_MONTH,
     +                        DAY,
     +                       DAILY_HARD_WIRED_BY_BLOCK(1,OUTAGE_BLOCK))
!
                     ELSEIF(DSM_MARKET_RESOURCE(S_U)) THEN
!
                        DAILY_BLOCK_DSM_MW = 0.
!
                        CALL GetCapUsedForBlock(
     +                        MARKET_RESOURCE_COUNTER(S_U),
     +                        B,
     +                        R_MONTH,
     +                        DAY,
     +                        DAILY_BLOCK_DSM_MW) ! 24 VALUES
                        DO K = 1, 24
                           DAILY_DSM_MW(K) = DAILY_DSM_MW(K) +
     +                                            DAILY_BLOCK_DSM_MW(K)

                        ENDDO
!
                     ELSEIF(STRICT_MARKET_RESOURCE(S_U)) THEN
                        DAILY_HARD_WIRED_BY_BLOCK(1,OUTAGE_BLOCK) =
     +                         GENERATION_BY_SEGMENT(OUTAGE_BLOCK,J)
                        DAILY_HARD_WIRED_BY_BLOCK(2,OUTAGE_BLOCK) =
     +                         GENERATION_BY_SEGMENT(OUTAGE_BLOCK,J+1)
                        DAILY_HARD_WIRED_BY_BLOCK(3,OUTAGE_BLOCK) =
     +                         GENERATION_BY_SEGMENT(OUTAGE_BLOCK,J+2)
                        DAILY_HARD_WIRED_BY_BLOCK(4,OUTAGE_BLOCK) =
     +                         GENERATION_BY_SEGMENT(OUTAGE_BLOCK,J+3)
                        DAILY_HARD_WIRED_BY_BLOCK(5,OUTAGE_BLOCK) =
     +                         GENERATION_BY_SEGMENT(OUTAGE_BLOCK,J+4)
                        DAILY_HARD_WIRED_BY_BLOCK(6,OUTAGE_BLOCK) =
     +                         GENERATION_BY_SEGMENT(OUTAGE_BLOCK,J+5)
                        DAILY_HARD_WIRED_BY_BLOCK(7,OUTAGE_BLOCK) =
     +                         GENERATION_BY_SEGMENT(OUTAGE_BLOCK,J+6)
                        DAILY_HARD_WIRED_BY_BLOCK(8,OUTAGE_BLOCK) =
     +                         GENERATION_BY_SEGMENT(OUTAGE_BLOCK,J+7)
                        DAILY_HARD_WIRED_BY_BLOCK(9,OUTAGE_BLOCK) =
     +                         GENERATION_BY_SEGMENT(OUTAGE_BLOCK,J+8)
                        DAILY_HARD_WIRED_BY_BLOCK(10,OUTAGE_BLOCK) =
     +                         GENERATION_BY_SEGMENT(OUTAGE_BLOCK,J+9)
                        DAILY_HARD_WIRED_BY_BLOCK(11,OUTAGE_BLOCK) =
     +                         GENERATION_BY_SEGMENT(OUTAGE_BLOCK,J+10)
                        DAILY_HARD_WIRED_BY_BLOCK(12,OUTAGE_BLOCK) =
     +                         GENERATION_BY_SEGMENT(OUTAGE_BLOCK,J+11)
                        DAILY_HARD_WIRED_BY_BLOCK(13,OUTAGE_BLOCK) =
     +                         GENERATION_BY_SEGMENT(OUTAGE_BLOCK,J+12)
                        DAILY_HARD_WIRED_BY_BLOCK(14,OUTAGE_BLOCK) =
     +                         GENERATION_BY_SEGMENT(OUTAGE_BLOCK,J+13)
                        DAILY_HARD_WIRED_BY_BLOCK(15,OUTAGE_BLOCK) =
     +                         GENERATION_BY_SEGMENT(OUTAGE_BLOCK,J+14)
                        DAILY_HARD_WIRED_BY_BLOCK(16,OUTAGE_BLOCK) =
     +                         GENERATION_BY_SEGMENT(OUTAGE_BLOCK,J+15)
                        DAILY_HARD_WIRED_BY_BLOCK(17,OUTAGE_BLOCK) =
     +                         GENERATION_BY_SEGMENT(OUTAGE_BLOCK,J+16)
                        DAILY_HARD_WIRED_BY_BLOCK(18,OUTAGE_BLOCK) =
     +                         GENERATION_BY_SEGMENT(OUTAGE_BLOCK,J+17)
                        DAILY_HARD_WIRED_BY_BLOCK(19,OUTAGE_BLOCK) =
     +                         GENERATION_BY_SEGMENT(OUTAGE_BLOCK,J+18)
                        DAILY_HARD_WIRED_BY_BLOCK(20,OUTAGE_BLOCK) =
     +                         GENERATION_BY_SEGMENT(OUTAGE_BLOCK,J+19)
                        DAILY_HARD_WIRED_BY_BLOCK(21,OUTAGE_BLOCK) =
     +                         GENERATION_BY_SEGMENT(OUTAGE_BLOCK,J+20)
                        DAILY_HARD_WIRED_BY_BLOCK(22,OUTAGE_BLOCK) =
     +                         GENERATION_BY_SEGMENT(OUTAGE_BLOCK,J+21)
                        DAILY_HARD_WIRED_BY_BLOCK(23,OUTAGE_BLOCK) =
     +                         GENERATION_BY_SEGMENT(OUTAGE_BLOCK,J+22)
                        DAILY_HARD_WIRED_BY_BLOCK(24,OUTAGE_BLOCK) =
     +                         GENERATION_BY_SEGMENT(OUTAGE_BLOCK,J+23)
                     ELSEIF(MUST_RUN_BLOCK(OUTAGE_BLOCK) == 1 .OR.
     +                              STRICT_MARKET_W_LOAD_BAL(S_U)) THEN
                        MONTH_MUST_BY_BLOCK(OUTAGE_BLOCK,J) =
     +                         GENERATION_BY_SEGMENT(OUTAGE_BLOCK,J)
                        MONTH_MUST_BY_BLOCK(OUTAGE_BLOCK,J+1) =
     +                         GENERATION_BY_SEGMENT(OUTAGE_BLOCK,J+1)
                        MONTH_MUST_BY_BLOCK(OUTAGE_BLOCK,J+2) =
     +                         GENERATION_BY_SEGMENT(OUTAGE_BLOCK,J+2)
                        MONTH_MUST_BY_BLOCK(OUTAGE_BLOCK,J+3) =
     +                         GENERATION_BY_SEGMENT(OUTAGE_BLOCK,J+3)
                        MONTH_MUST_BY_BLOCK(OUTAGE_BLOCK,J+4) =
     +                         GENERATION_BY_SEGMENT(OUTAGE_BLOCK,J+4)
                        MONTH_MUST_BY_BLOCK(OUTAGE_BLOCK,J+5) =
     +                         GENERATION_BY_SEGMENT(OUTAGE_BLOCK,J+5)
                        MONTH_MUST_BY_BLOCK(OUTAGE_BLOCK,J+6) =
     +                         GENERATION_BY_SEGMENT(OUTAGE_BLOCK,J+6)
                        MONTH_MUST_BY_BLOCK(OUTAGE_BLOCK,J+7) =
     +                         GENERATION_BY_SEGMENT(OUTAGE_BLOCK,J+7)
                        MONTH_MUST_BY_BLOCK(OUTAGE_BLOCK,J+8) =
     +                         GENERATION_BY_SEGMENT(OUTAGE_BLOCK,J+8)
                        MONTH_MUST_BY_BLOCK(OUTAGE_BLOCK,J+9) =
     +                         GENERATION_BY_SEGMENT(OUTAGE_BLOCK,J+9)
                        MONTH_MUST_BY_BLOCK(OUTAGE_BLOCK,J+10) =
     +                         GENERATION_BY_SEGMENT(OUTAGE_BLOCK,J+10)
                        MONTH_MUST_BY_BLOCK(OUTAGE_BLOCK,J+11) =
     +                         GENERATION_BY_SEGMENT(OUTAGE_BLOCK,J+11)
                        MONTH_MUST_BY_BLOCK(OUTAGE_BLOCK,J+12) =
     +                         GENERATION_BY_SEGMENT(OUTAGE_BLOCK,J+12)
                        MONTH_MUST_BY_BLOCK(OUTAGE_BLOCK,J+13) =
     +                         GENERATION_BY_SEGMENT(OUTAGE_BLOCK,J+13)
                        MONTH_MUST_BY_BLOCK(OUTAGE_BLOCK,J+14) =
     +                         GENERATION_BY_SEGMENT(OUTAGE_BLOCK,J+14)
                        MONTH_MUST_BY_BLOCK(OUTAGE_BLOCK,J+15) =
     +                         GENERATION_BY_SEGMENT(OUTAGE_BLOCK,J+15)
                        MONTH_MUST_BY_BLOCK(OUTAGE_BLOCK,J+16) =
     +                         GENERATION_BY_SEGMENT(OUTAGE_BLOCK,J+16)
                        MONTH_MUST_BY_BLOCK(OUTAGE_BLOCK,J+17) =
     +                         GENERATION_BY_SEGMENT(OUTAGE_BLOCK,J+17)
                        MONTH_MUST_BY_BLOCK(OUTAGE_BLOCK,J+18) =
     +                         GENERATION_BY_SEGMENT(OUTAGE_BLOCK,J+18)
                        MONTH_MUST_BY_BLOCK(OUTAGE_BLOCK,J+19) =
     +                         GENERATION_BY_SEGMENT(OUTAGE_BLOCK,J+19)
                        MONTH_MUST_BY_BLOCK(OUTAGE_BLOCK,J+20) =
     +                         GENERATION_BY_SEGMENT(OUTAGE_BLOCK,J+20)
                        MONTH_MUST_BY_BLOCK(OUTAGE_BLOCK,J+21) =
     +                         GENERATION_BY_SEGMENT(OUTAGE_BLOCK,J+21)
                        MONTH_MUST_BY_BLOCK(OUTAGE_BLOCK,J+22) =
     +                         GENERATION_BY_SEGMENT(OUTAGE_BLOCK,J+22)
                        MONTH_MUST_BY_BLOCK(OUTAGE_BLOCK,J+23) =
     +                         GENERATION_BY_SEGMENT(OUTAGE_BLOCK,J+23)
                     ENDIF
                  ENDDO
!
               ENDIF ! FIRST HOUR OF THE DAY
!
!
               DAILY_BUY_WHEEL(HR) = LOCAL_BUY_SPREAD
               DAILY_SELL_WHEEL(HR) = L_MONTHLY_PRICE(HOUR) - ! 090709.
     +                            LOCAL_PRICE(HOUR) + LOCAL_SELL_SPREAD
!
               HOURLY_FORWARD_SALE =
     +                      HOURLY_FORWARD_CONTRACT_ENERGY(
     +                           HR,
     +                           CALENDAR_DAY_OF_WEEK,DAY,R_TG,
     + ! ??? NOT SURE IF I IS RIGHT ???
     +                           DAILY_PRICE(HR),R_MONTH) 
!
! 4/19/02. ADDED.
!
      
      
      
               HOURLY_INTERRUPTIBLE =
     +                      HOURLY_INTERRUPTIBLE_CAPACITY(
     +                                 HR,
     +                                 DAILY_PRICE(HR),
     +                                 CALENDAR_DAY_OF_WEEK,
     +                                 DAY,
     +                                 R_TG,
     +                                 R_MONTH)
!
               ! ALREADY HAVE MONTHLY CALL/PUT
               SYSTEM_DERIVATIVES(HOUR,R_TG) = 
     +              SYSTEM_DERIVATIVES(HOUR,R_TG) +
     +                GET_ANNUAL_CALL_PUT_CAPACITY(HOUR,R_MONTH,R_TG) +
     +                       DAILY_PRODUCTS_CAPACITY(HR)  -
     +                                        HOURLY_FORWARD_SALE +
     +                                             HOURLY_INTERRUPTIBLE
     
               ! ALREADY HAVE MONTHLY DAILY HOURLY CALL PUT
               SYSTEM_AVAIL_DERIVATIVES(HOUR,R_TG) = 
     +               SYSTEM_AVAIL_DERIVATIVES(HOUR,R_TG) -
     +                                              HOURLY_FORWARD_SALE

               SYSTEM_HOURLY_LOAD(HOUR,R_TG) =
     +               MAX(0.,GET_TRANS_LOAD_AFTER_EL(HOUR,R_TG) -
     +                                   R_TRANS_ROR_CAPACITY-
     +                                  SYSTEM_DERIVATIVES(HOUR,R_TG) -
     +                                       SYSTEM_STORAGE(HOUR,R_TG))
!
!
!
               SYSTEM_HOURLY_LOAD(HOUR,R_TG) =
     +                              SYSTEM_HOURLY_LOAD(HOUR,R_TG) -
     +                                                 DAILY_DSM_MW(HR)
!
               SYSTEM_DSM_MW(HOUR,R_TG) =
     +                              SYSTEM_DSM_MW(HOUR,R_TG) +
     +                                                 DAILY_DSM_MW(HR)
!
               SYSTEM_MARKET_PRICE(HOUR,R_TG) = L_MONTHLY_PRICE(HOUR)

! 03207.
               SYSTEM_HOURLY_STORAGE_PUMP(HOUR,R_TG) =
     +                 STORE_PUMP_BY_HOUR(HOUR,R_TG)
               SYSTEM_HOURLY_STORAGE_GEN(HOUR,R_TG) =
     +                 STORE_GEN_BY_HOUR(HOUR,R_TG)
!
! 01/06/03. ADDED ROR CAPACITY
! 033307. MODIFIED TO ACCOUNT FOR PUMPED STORAGE.
!
               SYSTEM_HOURLY_HYDRO(HOUR,R_TG) =
     +                      MAX(0.,GET_TRANS_HOURLY_HYDRO(HOUR,R_TG) +
     +                                           R_TRANS_ROR_CAPACITY
     +                    - (SYSTEM_HOURLY_STORAGE_GEN(HOUR,R_TG) +
     +                          SYSTEM_HOURLY_STORAGE_PUMP(HOUR,R_TG)))
! 10/21/02.
               HOURLY_EMERGENCY_CAPACITY = 0.
               MAX_HOURLY_RAMP_UP = 0.
               MAX_HOURLY_RAMP_DOWN = 0.
!
! MOVED UP FOR ECITIES.
               LOCAL_POWER = 0.
               RESERVE_CAPACITY_USED = 0.
!
!
               IF(THIS_YEAR == 2007 .AND. R_MONTH == 4 .AND.
     +                                     DAY == 2 .AND. HR == 7) THEN
                  K = K
               ENDIF
               IF(ECITIES) THEN ! RESERVE FOR DUKE
!
                  IF(HOUR == 1) THEN
                     BEFORE_RETAINED = GET_DUKE_BEFORE_RETAINED()
! 07/30/04.  TAKEN OUT PER KATHY ANDERSON.
                     MONTHLY_CONTINGENT_CAP = 0.

                     MONTHLY_RESERVE_ENERGY = 0.
                     RESERVE_CAPACITY = 0.
                  ENDIF
! MADE HOURLY 02/01/02.
                  RETAINED_AVAILABLE = GET_DUKE_HOURLY_RETAINED(HOUR)
!
                  HOURLY_AC_LOAD(HR) = SYSTEM_HOURLY_LOAD(HOUR,R_TG) +
     +                                    SYSTEM_DERIVATIVES(HOUR,R_TG)

! CONTINGENT AND HOURLY FORWARD ARE NEGATIVE
!
                  EQUIVALENT_RETAINED =  RETAINED_AVAILABLE

                  RESERVE_CAPACITY_USED =
     +                        MAX(0.,
     +                           MIN(BEFORE_RETAINED +
     +                                  MONTHLY_CONTINGENT_CAP -
     +                                              EQUIVALENT_RETAINED,
     +                              SYSTEM_HOURLY_LOAD(HOUR,R_TG) -
     +                                            EQUIVALENT_RETAINED))

                  RESERVE_CAPACITY =
     +                              MAX(RESERVE_CAPACITY_USED,
     +                                         RESERVE_CAPACITY)

                  MONTHLY_RESERVE_ENERGY =
     +                           MONTHLY_RESERVE_ENERGY +
     +                                          RESERVE_CAPACITY_USED
!
                  DAILY_RESERVE_ENERGY(HR) = RESERVE_CAPACITY_USED
!
                  LOCAL_POWER = LOCAL_POWER + RESERVE_CAPACITY_USED
                  SYSTEM_HOURLY_GENERATION(HOUR,R_TG) =
     +                  SYSTEM_HOURLY_GENERATION(HOUR,R_TG) +
     +                                            RESERVE_CAPACITY_USED
                  SYSTEM_HOURLY_COMMITMENT(HOUR,R_TG) =
     +                  SYSTEM_HOURLY_COMMITMENT(HOUR,R_TG) +
     +                                            RESERVE_CAPACITY_USED
!
               ENDIF ! ECITIES
!
               IF(HOOSIER) THEN

                  IF(HOUR == 1) THEN
!
!                    CALL TO BLOCK INDICES FOR HOOSIER RESOURCES
!
!
                     WILLIAMS_OBLIGATION_ENERGY = 0.
                     WILLIAMS_OBLIGATION_PEAK = 0.
                     WILLIAMS_OBLIGATION_HOURS = 0.
                     MONTHLY_HEC_UC_RISK_ENERGY = 0.
!
                     HEC_EXCESS_ENERGY = 0.
                     HEC_EXCESS_PEAK = 0.
                     HEC_EXCESS_HOURS = 0.
                     HEC_EXCESS_ENERGY_REV = 0.
!
                     HEC_PECO_ENERGY = 0.
                     HEC_PECO_PEAK = 0.
                     HEC_PECO_HOURS = 0.
!
                     HEC_NATIVE_LOAD_ENERGY = 0.
                     HEC_NATIVE_LOAD_PEAK = 0.
                     HEC_NATIVE_LOAD_HOURS = 0.
                     HEC_NATIVE_LOAD_ENERGY_REV = 0.
!
                     HEC_MARKET_GEN_ENERGY = 0.
                     HEC_MARKET_GEN_PEAK = 0.
                     HEC_MARKET_GEN_HOURS = 0.
                     HEC_MARKET_GEN_ENERGY_REV = 0.
!
                     HEC_GEN_BOUGHT_ENERGY = 0.
                     HEC_GEN_BOUGHT_PEAK = 0.
                     HEC_GEN_BOUGHT_HOURS = 0.
                     HEC_GEN_BOUGHT_ENERGY_REV = 0.
!
                     HEC_GEN_SOLD_ENERGY = 0.
                     HEC_GEN_SOLD_PEAK = 0.
                     HEC_GEN_SOLD_HOURS = 0.
                     HEC_GEN_SOLD_ENERGY_REV = 0.
!
                     HEC_DAILY_RESERVE_ENERGY = 0.
                     HEC_DAILY_RESERVE_PEAK = 0.
                     HEC_DAILY_RESERVE_HOURS = 0.
                     HEC_DAILY_RESERVE_ENERGY_REV = 0.
!
                     HEC_DAILY_RISK_ENERGY = 0.
                     HEC_DAILY_RISK_PEAK = 0.
                     HEC_DAILY_RISK_HOURS = 0.
                     HEC_DAILY_RISK_ENERGY_REV = 0.
!
                     HEC_DAILY_NETG_ENERGY = 0.
                     HEC_DAILY_NETG_PEAK = 0.
                     HEC_DAILY_NETG_HOURS = 0.
                     HEC_DAILY_NETG_ENERGY_REV = 0.
!
                     IF(R_MONTH == 1) THEN
! ANNUAL WRITE
                        WILLIAMS_OBLIGATION_ENERGY_AN = 0.
                        WILLIAMS_OBLIGATION_PEAK_AN = 0.
                        WILLIAMS_OBLIGATION_HOURS_AN = 0.
!
                        HEC_EXCESS_ENERGY_AN = 0.
                        HEC_EXCESS_PEAK_AN = 0.
                        HEC_EXCESS_HOURS_AN = 0.
                        HEC_EXCESS_ENERGY_REV_AN = 0.
!
                        HEC_PECO_ENERGY_AN = 0.
                        HEC_PECO_PEAK_AN = 0.
                        HEC_PECO_HOURS_AN = 0.
!
                        HEC_NATIVE_LOAD_ENERGY_AN = 0.
                        HEC_NATIVE_LOAD_PEAK_AN = 0.
                        HEC_NATIVE_LOAD_HOURS_AN = 0.
                        HEC_NATIVE_LOAD_ENERGY_REV_AN = 0.
!
                        HEC_MARKET_GEN_ENERGY_AN = 0.
                        HEC_MARKET_GEN_PEAK_AN = 0.
                        HEC_MARKET_GEN_HOURS_AN = 0.
                        HEC_MARKET_GEN_ENERGY_REV_AN = 0.
!
                        HEC_GEN_BOUGHT_ENERGY_AN = 0.
                        HEC_GEN_BOUGHT_PEAK_AN = 0.
                        HEC_GEN_BOUGHT_HOURS_AN = 0.
                        HEC_GEN_BOUGHT_ENERGY_REV_AN = 0.
!
                        HEC_GEN_SOLD_ENERGY_AN = 0.
                        HEC_GEN_SOLD_PEAK_AN = 0.
                        HEC_GEN_SOLD_HOURS_AN = 0.
                        HEC_GEN_SOLD_ENERGY_REV_AN = 0.
!
                        HEC_DAILY_RESERVE_ENERGY_AN = 0.
                        HEC_DAILY_RESERVE_PEAK_AN = 0.
                        HEC_DAILY_RESERVE_HOURS_AN = 0.
                        HEC_DAILY_RESERVE_ENER_REV_AN = 0.
!
                        HEC_DAILY_RISK_ENERGY_AN = 0.
                        HEC_DAILY_RISK_PEAK_AN = 0.
                        HEC_DAILY_RISK_HOURS_AN = 0.
                        HEC_DAILY_RISK_ENERGY_REV_AN = 0.
!
                        HEC_DAILY_NETG_ENERGY_AN = 0.
                        HEC_DAILY_NETG_PEAK_AN = 0.
                        HEC_DAILY_NETG_HOURS_AN = 0.
                        HEC_DAILY_NETG_ENERGY_REV_AN = 0.
!
                     ENDIF
!
                     TEMP_I = GET_HOOSIER_INDICES(M1,M2,R1,R2,
     +                                  MEROM1_EFFECTIVE_CAP,
     +                                  MEROM2_EFFECTIVE_CAP,
     +                                  RATTS1_EFFECTIVE_CAP,
     +                                  RATTS2_EFFECTIVE_CAP,
     +                                  WI,
     +                                  PE,
     +                                  DG,
     +                                  BE,
     +                                  WO)
!
                     M1_1 = 0
                     M1_2 = 0
                     M2_1 = 0
                     M2_2 = 0
                     R1_1 = 0
                     R1_2 = 0
                     R2_1 = 0
                     R2_2 = 0
!
                     DO I = 1, MAX_OUTAGE_BLOCKS ! DISPATCH ORDER
                        OUTAGE_BLOCK = SORTED_OPTIONS(I)
                        U = UNIT_FOR_OUTAGE_BLOCK(OUTAGE_BLOCK)
                        IF(    U == M1 .AND. M1_1 == 0) THEN
                           M1_1 = OUTAGE_BLOCK
                           M1_2 = M1_1 + 1
                        ELSEIF(U == M2 .AND. M2_1 == 0) THEN
                           M2_1 = OUTAGE_BLOCK
                           M2_2 = M2_1 + 1
                        ELSEIF(U == R1 .AND. R1_1 == 0) THEN
                           R1_1 = OUTAGE_BLOCK
                           R1_2 = R1_1 + 1
                        ELSEIF(U == R2 .AND. R2_1 == 0) THEN
                           R2_1 = OUTAGE_BLOCK
                           R2_2 = R2_1 + 1
                        ENDIF
                     ENDDO
!
                     IF(THIS_YEAR < 2004) THEN
                        WILLIAMS_ACTIVE = .TRUE.
                     ELSE
                        WILLIAMS_ACTIVE = .FALSE.
                     ENDIF
!
                     IF(THIS_YEAR < 2008) THEN
                        PECO_MULT = 1.0
                     ELSEIF(THIS_YEAR < 2009) THEN
                        PECO_MULT = 0.5
                     ELSE
                        PECO_MULT = 0.
                     ENDIF
!
                  ENDIF
                  IF(IS_5X16) THEN
                     HEC_EXCESS_ENERGY_COST = 14.
                  ELSE
                     HEC_EXCESS_ENERGY_COST = 12.
                  ENDIF
!
                  IF(HR == 1) THEN ! FIRST HOUR IN DAY
                     HEC_PECO_LOAD = 400.*PECO_MULT
                     HEC_AVAIL_GEN_REQMT = 0.
                     HEC_EQUIV_GEN = 0.
                     HEC_ACTUAL_GEN = 0.
                     HEC_MARKET_GEN = 0.
                     HEC_LOAD = 0.
                     HEC_NATIVE_LOAD = 0.
                     HEC_NET_GENERATION = 0.
                     HEC_MARKET_PURCHASES = 0.
                     HEC_MEMBER_LOAD = 0.
                     WILLIAMS_OBLIGATION_MW = 0.
                     HEC_UC_RISK_ENERGY = 0.
                     HEC_EXCESS_MWH = 0.
                     HEC_GEN_BOUGHT = 0.
                     HEC_GEN_SOLD = 0.
                     HEC_TOTAL_SUPPLY = 0.
                     HEC_GEN_AV_4LD_AF_MKT = 0.
                     HEC_MKT_PP = 0.
                     HEC_REM_MKT_PURCH = 0.
                     HEC_TOTAL_DEMAND = 0.
                     HEC_MOR_CAPACITY = 0.
!
                     HEC_DAILY_PEAK  = 0.
!
                     DO J = 1, 24
!
! ASSUMES UNIQUE ASSET CLASS FOR MEMBER LOAD.
!
                        HEC_MEMBER_LOAD(J) =
     +                       HOURLY_LOAD_FROM_AC_TG( ! NOTE J COUNT
     +                                          J+HOUR-1,INT(1),INT(1))
!
                        HEC_DAILY_PEAK = MAX(
     +                               HEC_DAILY_PEAK,HEC_MEMBER_LOAD(J))

                     ENDDO
                 HEC_DAILY_RESERVE = HEC_DAILY_PEAK * .04 ! 4% RESERVES
!
                  ENDIF
!
! 4/26/01. MOVED CAPACITY CALC'S INTO HOURLY FROM HOUR=1
!
!
                  IF(M1 > 0) THEN
                     MEROM1_CAPACITY =
     +                     GET_BLOCK_AVAIL_4_HOUR(M1,INT(1,2),HOUR) +
     +                          GET_BLOCK_AVAIL_4_HOUR(M1,INT(2,2),HOUR)
                     MEROM1_OUTAGE_TYPE =
     +                         GET_BLOCK_OUTAGE_4_HOUR(M1,INT(1,2),HOUR)
                  ELSE
                     MEROM1_CAPACITY = 0.
                     MEROM1_OUTAGE_TYPE = 1.
                  ENDIF
                  IF(MEROM1_OUTAGE_TYPE == 1.) THEN
                     HEC_MOR_CAPACITY(HR) = HEC_MOR_CAPACITY(HR) +
     +                        MAX(0.,
     +                          MEROM1_EFFECTIVE_CAP - MEROM1_CAPACITY)
                  ENDIF
!
                  IF(MEROM1_CAPACITY >= 460.*PECO_MULT) THEN
                     MEROM1_PECO_DERATE_CAP = 0.
                  ELSE
                     IF(MEROM1_CAPACITY == 0.) THEN
                        MEROM1_PECO_DERATE_CAP = 200.*PECO_MULT
                     ELSE
                        MEROM1_PECO_DERATE_CAP =
     +                    (460. - MEROM1_CAPACITY) * 0.4347826 *
     +                                                        PECO_MULT
                     ENDIF
                  ENDIF
!
                  IF(M2 > 0) THEN
                     MEROM2_CAPACITY =
     +                     GET_BLOCK_AVAIL_4_HOUR(M2,INT(1,2),HOUR) +
     +                          GET_BLOCK_AVAIL_4_HOUR(M2,INT(2,2),HOUR)
                     MEROM2_OUTAGE_TYPE =
     +                         GET_BLOCK_OUTAGE_4_HOUR(M2,INT(1,2),HOUR)
                  ELSE
                     MEROM2_CAPACITY = 0.
                     MEROM2_OUTAGE_TYPE = 1.
                  ENDIF
                  IF(MEROM2_OUTAGE_TYPE == 1.) THEN
                     HEC_MOR_CAPACITY(HR) = HEC_MOR_CAPACITY(HR) +
     +                        MAX(0.,
     +                          MEROM2_EFFECTIVE_CAP - MEROM2_CAPACITY)
                  ENDIF
!
                  IF(MEROM2_CAPACITY >= 460.*PECO_MULT) THEN
                     MEROM2_PECO_DERATE_CAP = 0.
                  ELSE
                     IF(MEROM2_CAPACITY == 0.) THEN
                        MEROM2_PECO_DERATE_CAP = 200.*PECO_MULT
                     ELSE
                        MEROM2_PECO_DERATE_CAP =
     +                     (460. - MEROM2_CAPACITY) * 0.4347826 *
     +                                                        PECO_MULT
                     ENDIF
                  ENDIF
!
                  IF(R1 > 0) THEN
                     RATTS1_CAPACITY =
     +                     GET_BLOCK_AVAIL_4_HOUR(R1,INT(1,2),HOUR) +
     +                          GET_BLOCK_AVAIL_4_HOUR(R1,INT(2,2),HOUR)
                     RATTS1_OUTAGE_TYPE =
     +                         GET_BLOCK_OUTAGE_4_HOUR(R1,INT(1,2),HOUR)
                  ELSE
                     RATTS1_CAPACITY = 0.
                     RATTS1_OUTAGE_TYPE = 1.
                  ENDIF
                  IF(RATTS1_OUTAGE_TYPE == 1.) THEN
                     HEC_MOR_CAPACITY(HR) = HEC_MOR_CAPACITY(HR) +
     +                        MAX(0.,
     +                          RATTS1_EFFECTIVE_CAP - RATTS1_CAPACITY)
                  ENDIF
                  IF(R2 > 0) THEN
                     RATTS2_CAPACITY =
     +                     GET_BLOCK_AVAIL_4_HOUR(R2,INT(1,2),HOUR) +
     +                          GET_BLOCK_AVAIL_4_HOUR(R2,INT(2,2),HOUR)
                     RATTS2_OUTAGE_TYPE =
     +                         GET_BLOCK_OUTAGE_4_HOUR(R2,INT(1,2),HOUR)
                  ELSE
                     RATTS2_CAPACITY = 0.
                     RATTS2_OUTAGE_TYPE = 1.
                  ENDIF
                  IF(RATTS2_OUTAGE_TYPE == 1.) THEN
                     HEC_MOR_CAPACITY(HR) = HEC_MOR_CAPACITY(HR) +
     +                        MAX(0.,
     +                          RATTS2_EFFECTIVE_CAP - RATTS2_CAPACITY)
                  ENDIF
!
                  HEC_PECO_LOAD(HR) =
     +               HEC_PECO_LOAD(HR) -
     +                           MEROM1_PECO_DERATE_CAP -
     +                                           MEROM2_PECO_DERATE_CAP
!
                  HEC_PECO_ENERGY = HEC_PECO_ENERGY + HEC_PECO_LOAD(HR)
                  HEC_PECO_PEAK = MAX(HEC_PECO_PEAK,HEC_PECO_LOAD(HR))
                  HEC_PECO_HOURS = HEC_PECO_HOURS + 1
!
                  HEC_AVAIL_GEN_REQMT(HR) =
     +                                  MEROM1_EFFECTIVE_CAP +
     +                                  MEROM2_EFFECTIVE_CAP +
     +                                  RATTS1_EFFECTIVE_CAP +
     +                                  RATTS2_EFFECTIVE_CAP -
     +                                     400.*PECO_MULT -
     +                                     HEC_DAILY_RESERVE -
     +                                             HEC_MOR_CAPACITY(HR)
!
                  HEC_EQUIV_GEN(HR) =
     +                  MEROM1_CAPACITY + MEROM2_CAPACITY +
     +                  RATTS1_CAPACITY + RATTS2_CAPACITY -
     +                                     HEC_PECO_LOAD(HR)
!
                  HEC_NATIVE_LOAD(HR) = SYSTEM_HOURLY_LOAD(HOUR,R_TG)

                  HEC_LOAD(HR) =  HEC_NATIVE_LOAD(HR) -
     +                           MEROM1_PECO_DERATE_CAP -
     +                                           MEROM2_PECO_DERATE_CAP
!
! GET PECO CONTRACT OUT.
!
                  IF(M1_1 > 0) THEN
                     HEC_MARKET_GEN(HR) = HEC_MARKET_GEN(HR) +
     +                     GENERATION_BY_SEGMENT(M1_1,HOUR)
                  ENDIF
                  IF(M1_2 > 0) THEN
                     HEC_MARKET_GEN(HR) = HEC_MARKET_GEN(HR) +
     +                     GENERATION_BY_SEGMENT(M1_2,HOUR)
                  ENDIF
                  IF(M2_1 > 0) THEN
                     HEC_MARKET_GEN(HR) = HEC_MARKET_GEN(HR) +
     +                     GENERATION_BY_SEGMENT(M2_1,HOUR)
                  ENDIF
                  IF(M2_2 > 0) THEN
                     HEC_MARKET_GEN(HR) = HEC_MARKET_GEN(HR) +
     +                     GENERATION_BY_SEGMENT(M2_2,HOUR)
                  ENDIF
                  IF(R1_1 > 0) THEN
                     HEC_MARKET_GEN(HR) = HEC_MARKET_GEN(HR) +
     +                     GENERATION_BY_SEGMENT(R1_1,HOUR)
                  ENDIF
                  IF(R1_2 > 0) THEN
                     HEC_MARKET_GEN(HR) = HEC_MARKET_GEN(HR) +
     +                     GENERATION_BY_SEGMENT(R1_2,HOUR)
                  ENDIF
                  IF(R2_1 > 0) THEN
                     HEC_MARKET_GEN(HR) = HEC_MARKET_GEN(HR) +
     +                     GENERATION_BY_SEGMENT(R2_1,HOUR)
                  ENDIF
                  IF(R2_2 > 0) THEN
                     HEC_MARKET_GEN(HR) = HEC_MARKET_GEN(HR) +
     +                     GENERATION_BY_SEGMENT(R2_2,HOUR)
                  ENDIF
!
                  HEC_ACTUAL_GEN(HR) =
     +                        MIN(HEC_EQUIV_GEN(HR)-HEC_DAILY_RESERVE,
     +                                             HEC_NATIVE_LOAD(HR))
!
                  SYSTEM_HOURLY_LOAD(HOUR,R_TG) =
     +                SYSTEM_HOURLY_LOAD(HOUR,R_TG) + HEC_PECO_LOAD(HR)
!
               ENDIF ! HOOSIER
!
!
               IF(PRICE_ONLY_WHOLESALE_REV) THEN
                  HOURLY_NATIVE_LOAD = SYSTEM_HOURLY_LOAD(HOUR,R_TG)

               ELSE
                  HOURLY_NATIVE_LOAD = 0.

               ENDIF

               SYSTEM_RETAIL_GENERATION = 0.
               SYSTEM_FIRM_GENERATION = 0.
!
!
               USE_RETAIL = .TRUE.
!
! 12/05/03. GET HOURLY DATA IN SUPPORT OF MULTI-AREA BUY CALCULATIONS
! 12/09/03. MOVED CODE UP TO INFLUENCE EMERGENCY AND OTHER SYSTEM 
! DECISIONS
!
!           THEN, HAVE THE AREAS COMPETE WITH THERMAL UNITS FOR NATIVE 
! LOAD
!
               IF(NUM_OF_PRICING_GROUPS > 0) THEN
                  PRICING_GROUP_SELL_QUANT = 0.
!
                  DO I = 1, NUM_OF_PRICING_GROUPS
!
! NOTE: THIS IS A DELIVER PRICE INCLUDING WHEELING CHARGE AND PRICE 
! DELTA
!
                     PRICING_GROUP_SELL_PRICE(I,HR) =
     +                  GET_PRICING_GROUP_SELL_PRICE(
     +                        HOUR,
     +                        R_TG,
     +                        I,
     +                        PEAK_HOUR,
     +                        PRICING_GROUP_SELL_QUANT(I),
     +                        PRICING_GROUP_SELL_NAME(I),
     +                        R_MONTH,
     +                        globecom_YEAR)
                     IF(globecom_YEAR > AVAIL_DATA_YEARS) THEN
                        TEMP_DAILY_PRICE =
     +                                   PRICING_GROUP_SELL_PRICE(I,HR)
                        CALL
     +                    MULTI_AREA_EXT_PERIOD_PRICE_ESC(
     +                 globecom_YEAR,R_TG,TEMP_DAILY_PRICE)
                        PRICING_GROUP_SELL_PRICE(I,HR) =
     +                                                 TEMP_DAILY_PRICE
                     ENDIF
                     PRICING_GROUP_SELL_QUANT(0) =
     +                     PRICING_GROUP_SELL_QUANT(0) +
     +                                      PRICING_GROUP_SELL_QUANT(I)
                     PRICING_GROUP_SELL_INDEX(I) = I
                     PRICING_GROUP_SELL_QUANT_AVAIL(I,HR) =
     +                                      PRICING_GROUP_SELL_QUANT(I)
                  END DO
!
                  MAXIMUM_IMPORT_TIE = PRICING_GROUP_SELL_QUANT(0)
!
                  CALL INDEXED_SORT_ASCENDING(
     +                                  PRICING_GROUP_SELL_PRICE(1,HR),
     +                                  PRICING_GROUP_SELL_INDEX,
     +                                  NUM_OF_PRICING_GROUPS,
     +                                  ASCENDING)
               ENDIF
!
! 03/04/04. SEPARATED THE TWO GROUPS
!
               IF(NUM_OF_BUY_PRICING_GROUPS > 0) THEN
                  PRICING_GROUP_BUY_QUANT(0) = 0.
!
                  DO I = 1, NUM_OF_BUY_PRICING_GROUPS
!
                     PRICING_GROUP_BUY_PRICE(I,HR) =
     +                  GET_PRICING_GROUP_BUY_PRICE(
     +                        HOUR,
     +                        R_TG,
     +                        I,
     +                        PEAK_HOUR,
     +                        PRICING_GROUP_BUY_QUANT(I),
     +                        PRICING_GROUP_BUY_NAME(I),
     +                        R_MONTH,
     +                        globecom_YEAR)
!
                     IF(globecom_YEAR > AVAIL_DATA_YEARS) THEN
                        TEMP_DAILY_PRICE = PRICING_GROUP_BUY_PRICE(I,HR)
                        CALL
     +                    MULTI_AREA_EXT_PERIOD_PRICE_ESC(
     +                    globecom_YEAR,R_TG,TEMP_DAILY_PRICE)
                       PRICING_GROUP_BUY_PRICE(I,HR) = TEMP_DAILY_PRICE
                     ENDIF
!
                     PRICING_GROUP_BUY_QUANT(0) =
     +                     PRICING_GROUP_BUY_QUANT(0) +
     +                                       PRICING_GROUP_BUY_QUANT(I)
                     PRICING_GROUP_BUY_INDEX(I) = I
                     PRICING_GROUP_BUY_QUANT_AVAIL(I,HR) =
     +                                       PRICING_GROUP_BUY_QUANT(I)
!
                  END DO

!
! DECENDING. GO AFTER THE HIGHEST PRICED MARKETS FIRST...
!
                  MAXIMUM_EXPORT_TIE = PRICING_GROUP_BUY_QUANT(0)
                  CALL INDEXED_SORT_ASCENDING(
     +                                  PRICING_GROUP_BUY_PRICE(1,HR),
     +                                  PRICING_GROUP_BUY_INDEX,
     +                                  NUM_OF_BUY_PRICING_GROUPS,
     +                                  DESCENDING)
              ENDIF
!
!
! 5/30/02.
!
! TEST COMMITMENT TO MORE RESOURCES TO MEET LOAD HERE.
! CAN BE USED TO TEST WHETHER TO COMMIT UNITS FOR SPINNING, TOO.
!
               AVAILABLE_NOT_GENERATING =
     +                  SYSTEM_HOURLY_COMMIT_AVAIL(HOUR,R_TG) -
     +                      (SYSTEM_HOURLY_COMMITMENT(HOUR,R_TG) -
     +                                       RESERVE_CAPACITY_USED)
!
!
               IF(AVAILABLE_NOT_GENERATING < -.1) THEN
                  WRITE(4,*) "BAD TRANSACT C COMMITMENT BALANCE"
                  WRITE(4,*) "IN MONTH,DAY,HOUR,QUANT",R_MONTH,DAY,HR,
     +                                         AVAILABLE_NOT_GENERATING

               ENDIF

! POINTERS, DAILY SPIN
!
               IF(.NOT. IS_5X16) THEN
                  HOURLY_SPINNING_CAPACITY =
     +      GET_OFF_PEAK_SPIN_FOR_TG(R_TG,
     +                               R_MONTH,
     +                               globecom_YEAR)
               ELSE
                  HOURLY_SPINNING_CAPACITY =
     +       GET_TRANS_SPIN_FOR_TG(R_TG,
     +                        R_MONTH,
     +                        globecom_YEAR)
               ENDIF
!
               HOURLY_SPINNING_CAPACITY =
     +                  GET_DAILY_PEAK_SPIN(
     +                                  R_TG,
     +                                  R_MONTH,
     +                                  globecom_YEAR,
     +                                  HOURLY_SPINNING_CAPACITY,
     +                                  PEAK_HOUR,
     +                                  DAILY_PEAK)
!
               IF(EMERGENCY_MEETS_SPIN) THEN
                  DECOMMIT_TARGET_LOAD = HOURLY_NATIVE_LOAD +
     +                                         HOURLY_SPINNING_CAPACITY
                  EMERGENCY_TARGET_LOAD = HOURLY_NATIVE_LOAD +
     +                                         HOURLY_SPINNING_CAPACITY

               ELSE
                  DECOMMIT_TARGET_LOAD = HOURLY_NATIVE_LOAD
                  EMERGENCY_TARGET_LOAD = HOURLY_NATIVE_LOAD
               ENDIF
!
               IF(THIS_YEAR == 2007 .AND. R_MONTH == 3 .AND.
     +                                (HOUR == 1 .OR. HOUR == 25)) THEN
                  TEMP_R = TEMP_R
               ENDIF
!
               DETAILED_RAMPING = .TRUE.
               IF(DETAILED_RAMPING) THEN
!
! RECALCULATE
!
                  IF(ECITIES) THEN
                     SYSTEM_HOURLY_COMMITMENT(HOUR,R_TG) =
     +                                            RESERVE_CAPACITY_USED
                  ELSE
                     SYSTEM_HOURLY_COMMITMENT(HOUR,R_TG) = 0.
                  ENDIF
!
                  DO I = 1, MAX_OUTAGE_BLOCKS
                     K = SORTED_OPTIONS(I)
!
! 12/20/05.
!
!
                     U = UNIT_FOR_OUTAGE_BLOCK(K)
                     S_U = S_U_FOR_OUTAGE_BLOCK(K)
                     B = BLOCK_FOR_OUTAGE_BLOCK(K)
!
!
                     OUTAGE_CAPACITY = GET_BLOCK_AVAIL_4_HOUR(U,B,HOUR)
!
                     IF(B < 2 .OR. UNIT_IS_UP(S_U) < 1) THEN
!
                        IF(OUTAGE_CAPACITY > .0001) THEN
                           SYSTEM_HOURLY_COMMITMENT(HOUR,R_TG) =
     +                            SYSTEM_HOURLY_COMMITMENT(HOUR,R_TG) +
     +                                    GENERATION_BY_SEGMENT(K,HOUR)
                        ENDIF
                     ELSE
!
                        IF(HOUR > 1) THEN
                           TEMP_I = HOUR -1
                        ELSE
                           TEMP_I = HOUR
                        ENDIF
!
                        MAX_HOURLY_RAMP_UP(K) =
     +                     MIN(OUTAGE_CAPACITY,
     +                           GENERATION_BY_SEGMENT(K,TEMP_I) +
     +                                                 RAMP_RATE(S_U))
                        MAX_HOURLY_RAMP_UP(0) =
     +                     MAX_HOURLY_RAMP_UP(0) +
     +                        MIN(OUTAGE_CAPACITY,
     +                           GENERATION_BY_SEGMENT(K,TEMP_I) +
     +                                                 RAMP_RATE(S_U))
                        MAX_HOURLY_RAMP_DOWN(K) =
     +                      MAX(0.,
     +                           GENERATION_BY_SEGMENT(K,TEMP_I) -
     +                                             RAMP_DOWN_RATE(S_U))
                        MAX_HOURLY_RAMP_DOWN(0) =
     +                     MAX_HOURLY_RAMP_DOWN(0) +
     +                        MAX(0.,
     +                           GENERATION_BY_SEGMENT(K,TEMP_I) -
     +                                             RAMP_DOWN_RATE(S_U))
!
! TEST: DEFINE THE UPPER BOUND ON GEN HERE
!
                        GENERATION_BY_SEGMENT(K,HOUR) =
     +                     MIN(GENERATION_BY_SEGMENT(K,HOUR),
     +                                           MAX_HOURLY_RAMP_UP(K))
                        SYSTEM_HOURLY_COMMITMENT(HOUR,R_TG) =
     +                            SYSTEM_HOURLY_COMMITMENT(HOUR,R_TG) +
     +                                    GENERATION_BY_SEGMENT(K,HOUR)
                     ENDIF
!
                  ENDDO
               ENDIF
!
               IF(SYSTEM_HOURLY_COMMIT_AVAIL(HOUR,R_TG) +
     +                 MAXIMUM_IMPORT_TIE < EMERGENCY_TARGET_LOAD) THEN

! FIRST EMPLOY EMERGENCY CAPACITY OF COMMITTED UNITS: SHOULD BE 
! SORTED PRIOR TO THIS POINT.
!
                  DO K = 1, NO_START_UP_UNITS
                     IF(EMERGENCY_CAPACITY(K) <= 0.) CYCLE
                     HOURLY_EMERGENCY_CAPACITY(K) =
     +                     MIN(EMERGENCY_CAPACITY(K),
     +                        EMERGENCY_TARGET_LOAD -
     +                          SYSTEM_HOURLY_COMMIT_AVAIL(HOUR,R_TG) +
     +                           MAXIMUM_IMPORT_TIE +
     +                           SYSTEM_EMERGENCY_CAPACITY(HR,R_TG))
                     SYSTEM_EMERGENCY_CAPACITY(HOUR,R_TG) =
     +                          SYSTEM_EMERGENCY_CAPACITY(HOUR,R_TG) +
     +                                     HOURLY_EMERGENCY_CAPACITY(K)
                     IF(SYSTEM_HOURLY_COMMIT_AVAIL(HOUR,R_TG) +
     +                     MAXIMUM_IMPORT_TIE +
     +                           SYSTEM_EMERGENCY_CAPACITY(HR,R_TG) >
     +                                      EMERGENCY_TARGET_LOAD) THEN
                        EXIT
                     ENDIF
                  ENDDO
!
                  DO K = 1, NO_START_UP_UNITS
!
! MET THE LOAD CONDITION
!
                     IF(SYSTEM_HOURLY_COMMIT_AVAIL(HOUR,R_TG) +
     +                MAXIMUM_IMPORT_TIE >= EMERGENCY_TARGET_LOAD) EXIT
!
! FIND THE NEXT LOWEST COST TO COMMIT
!
                     J = MINIMUM_UP_INDEX(K)
!
! FIND THE INDEX POSITION OF THE FIRST BLOCK FOR THE LOWEST COST COMMIT
!
                     OUTAGE_BLOCK = OUTAGE_BLOCK_BY_SEGMENT(1,J)
                     SECOND_BLOCK = OUTAGE_BLOCK_BY_SEGMENT(2,J)
!
! FIND ITS ORIGINAL POSITION
!
                     U = UNIT_FOR_OUTAGE_BLOCK(OUTAGE_BLOCK)
                     S_U = S_U_FOR_OUTAGE_BLOCK(OUTAGE_BLOCK)
!
!                     IF(MARKET_RESOURCE_COUNTER(S_U) > 0 .OR.
                     IF(TVA_MARKET_RESOURCE(S_U) .OR.
     +                     GRE_MARKET_RESOURCE(S_U) .OR.
     +                        DSM_MARKET_RESOURCE(S_U) .OR.
     +                                STRICT_MARKET_RESOURCE(S_U) .OR.
     +                      IN_MIN_DOWN_TIME_STATE(S_U,HOUR) > 0) CYCLE
                     B = BLOCK_FOR_OUTAGE_BLOCK(OUTAGE_BLOCK)
!
                     IF(B /= 1) THEN
                        HR = HR
                        er_message='Stop from TF_OBJT2 SIID299'
                        call end_program(er_message)
                     ENDIF
!
                     OUTAGE_CAPACITY =
     +                    MAX(GENERATION_BY_SEGMENT(OUTAGE_BLOCK,HOUR),
     +                          MONTH_MUST_BY_BLOCK(OUTAGE_BLOCK,HOUR))
                     AVAILABLE_CAPACITY =
     +                                 GET_BLOCK_AVAIL_4_HOUR(U,B,HOUR)
!
                     IF(OUTAGE_CAPACITY > 0. .OR.
     +                                   AVAILABLE_CAPACITY <= 0.) THEN

! CONTINUE LOOKING FOR FIRST BLOCKS TO COMMIT
                        CYCLE
                     ENDIF
!
! IF ON IN PREVIOUS HOUR, THEN KEEP-ON.
!
                     IF(HOUR > 1 .AND.
     +                 GENERATION_BY_SEGMENT(OUTAGE_BLOCK,HOUR-1) > 0.)
     +                                                             THEN
                        MAX_J = HOUR
!
                     ELSE
!
! ELSE, BRING UNIT ON-LINE FOR MINIMUM LOADING
! MIN_UP_TIME = 0 .AND. MIN_UP_TIME = 1 HAVE SAME EFFECT.
!
                        MAX_J = MAX(int(0,2),int(MIN_UP_TIME(S_U)-1,2))
                        MAX_J = MIN(HOUR_IN_MONTH,HOUR+MAX_J)
                     ENDIF
!
                     DO J = HOUR, MAX_J
                        IF(J > HOUR .AND.
     +                      GENERATION_BY_SEGMENT(OUTAGE_BLOCK,J) > 0.)
     +                                                            CYCLE
                        B = 1
                        AVAILABLE_CAPACITY =
     +                                  GET_BLOCK_AVAIL_4_HOUR(U,B,J)
                        IF(AVAILABLE_CAPACITY <= .0001) EXIT ! 07/24/03
                        GENERATION_BY_SEGMENT(OUTAGE_BLOCK,J) =
     +                                               AVAILABLE_CAPACITY
                        SYSTEM_HOURLY_COMMITMENT(J,R_TG) =
     +                        SYSTEM_HOURLY_COMMITMENT(J,R_TG) +
     +                                               AVAILABLE_CAPACITY
                        SYSTEM_HOURLY_COMMIT_AVAIL(J,R_TG) =
     +                           SYSTEM_HOURLY_COMMIT_AVAIL(J,R_TG) +
     +                                               AVAILABLE_CAPACITY
                        SYSTEM_HOURLY_PRE_COMMITMENT(J,R_TG) =
     +                           SYSTEM_HOURLY_PRE_COMMITMENT(J,R_TG) +
     +                                               AVAILABLE_CAPACITY

                        MONTH_MUST_BY_BLOCK(OUTAGE_BLOCK,J-HOUR+HR) =
     +                     MIN(AVAILABLE_CAPACITY,
     +                    MONTH_MUST_BY_BLOCK(OUTAGE_BLOCK,J-HOUR+HR) +
     +                                              AVAILABLE_CAPACITY)
!
! ACCOUNT FOR THE SECOND BLOCK AS AVAILABLE
!
                        B = 2
                        AVAILABLE_CAPACITY =
     +                                    GET_BLOCK_AVAIL_4_HOUR(U,B,J)
                        SYSTEM_HOURLY_COMMIT_AVAIL(J,R_TG) =
     +                           SYSTEM_HOURLY_COMMIT_AVAIL(J,R_TG) +
     +                                               AVAILABLE_CAPACITY
!
! 12/02/03. FOR EDE
!
!                        SECOND_BLOCK = OUTAGE_BLOCK_BY_SEGMENT(2,J)
!
                        IF(SECOND_BLOCK > 0 .AND. J > 1) THEN
                           OUTAGE_CAPACITY = 0.
                           IF(DISPATCH_COST_FOR_BLOCK(SECOND_BLOCK) <
     +                                         L_MONTHLY_PRICE(J) .AND.
     +                        GENERATION_BY_SEGMENT(OUTAGE_BLOCK,J-1) >
     +                                                         0.) THEN
!
                             OUTAGE_CAPACITY =
     +                                 MIN(AVAILABLE_CAPACITY,
     +                          GENERATION_BY_SEGMENT(SECOND_BLOCK,J-1)
     +                                                + RAMP_RATE(S_U))
                           ELSEIF(
     +                         GENERATION_BY_SEGMENT(SECOND_BLOCK,J-1)
     +                                                       > 0.) THEN
                            OUTAGE_CAPACITY =
     +                       MAX(GENERATION_BY_SEGMENT(SECOND_BLOCK,J),
     +                                 MIN(AVAILABLE_CAPACITY,
     +                          GENERATION_BY_SEGMENT(SECOND_BLOCK,J-1)
     +                                          - RAMP_DOWN_RATE(S_U)))
                          ENDIF
 !
                           SYSTEM_HOURLY_COMMITMENT(J,R_TG) =
     +                              SYSTEM_HOURLY_COMMITMENT(J,R_TG) +
     +                                                  OUTAGE_CAPACITY
                           GENERATION_BY_SEGMENT(SECOND_BLOCK,J) =
     +                                                  OUTAGE_CAPACITY

                        ENDIF

                     ENDDO
                  ENDDO
               ENDIF
!
! 7/19/02. BRING UNIT SECOND BLOCKS UP TO MEET LOAD WHEN TIE 
! CONSTRAINT BINDING
!
!
! 02/21/03. ADDRESS SORTED_OPTIONS VIOLATING DISPATCH ORDER.
!
               IF(SYSTEM_HOURLY_COMMITMENT(HOUR,R_TG) +
     +                 MAXIMUM_IMPORT_TIE < EMERGENCY_TARGET_LOAD) THEN
                  DO I = 1, MAX_OUTAGE_BLOCKS ! DISPATCH ORDER
!
                     IF(SYSTEM_HOURLY_COMMITMENT(HOUR,R_TG) +
     +                MAXIMUM_IMPORT_TIE >= EMERGENCY_TARGET_LOAD) EXIT
!
                     OUTAGE_BLOCK = SORTED_OPTIONS(I)
!
                     OUTAGE_CAPACITY =
     +                    MAX(GENERATION_BY_SEGMENT(OUTAGE_BLOCK,HOUR),
     +                          MONTH_MUST_BY_BLOCK(OUTAGE_BLOCK,HOUR))
                     U = UNIT_FOR_OUTAGE_BLOCK(OUTAGE_BLOCK)
                     S_U = S_U_FOR_OUTAGE_BLOCK(OUTAGE_BLOCK)
                     B = BLOCK_FOR_OUTAGE_BLOCK(OUTAGE_BLOCK)
!
                     AVAILABLE_CAPACITY =
     +                                 GET_BLOCK_AVAIL_4_HOUR(U,B,HOUR)
!
!
!
                     OUTAGE_CAPACITY = MAX(0.,
     +                            AVAILABLE_CAPACITY - OUTAGE_CAPACITY)
                     IF(B == 1 .OR. OUTAGE_CAPACITY < 0.01) CYCLE
                     IF(GENERATION_BY_SEGMENT(
     +                OUTAGE_BLOCK_BY_SEGMENT(1,S_U),HOUR) <= 0.) CYCLE
!
                     OUTAGE_CAPACITY =
     +                     MIN(OUTAGE_CAPACITY,
     +                         EMERGENCY_TARGET_LOAD -
     +                           (SYSTEM_HOURLY_COMMITMENT(HOUR,R_TG) +
     +                                             MAXIMUM_IMPORT_TIE))
                     GENERATION_BY_SEGMENT(OUTAGE_BLOCK,HOUR) =
     +                       GENERATION_BY_SEGMENT(OUTAGE_BLOCK,HOUR) +
     +                                                  OUTAGE_CAPACITY
                     SYSTEM_HOURLY_COMMITMENT(HOUR,R_TG) =
     +                        SYSTEM_HOURLY_COMMITMENT(HOUR,R_TG) +
     +                                                  OUTAGE_CAPACITY
                     SYSTEM_HOURLY_PRE_COMMITMENT(HOUR,R_TG) =
     +                        SYSTEM_HOURLY_PRE_COMMITMENT(HOUR,R_TG) +
     +                                               AVAILABLE_CAPACITY
                  ENDDO
               ENDIF
!
! COMPLEMENT TO THE EMERGENCY COMMIT IS A DECOMMIT ROUTINE.
! TURN UNITS OFF THAT ARE NOT ECONOMIC DUE TO MINIMUM-UP CONDITIONS.
!
! 08/12/03. CHECK ALL FIRST BLOCKS FOR NON-DECOMMIT.
!
               DO K = 1, MAX_OUTAGE_BLOCKS
                  OUTAGE_BLOCK = SORTED_OPTIONS(K)
!
                  OUTAGE_CAPACITY =
     +                         GENERATION_BY_SEGMENT(OUTAGE_BLOCK,HOUR)
!
!
                  U = UNIT_FOR_OUTAGE_BLOCK(OUTAGE_BLOCK)
                  S_U = S_U_FOR_OUTAGE_BLOCK(OUTAGE_BLOCK)
                  B = BLOCK_FOR_OUTAGE_BLOCK(OUTAGE_BLOCK)
                  IF(B == 1 .AND. OUTAGE_CAPACITY > 0. .AND.
     +                       (.NOT. DECOMMIT_THERMAL_RESOURCES .OR.
     +               .NOT. YES_ALLOW_DECOMMIT_BY_UNIT(U,R_MONTH))) THEN

! 11/20/03. CHANGED FOR SRP FOR BACKWARDS COMPATABILITY
! ALSO CHANGED THE DEFAULT THERMAL VALUE TO FALSE
                     MONTH_MUST_BY_BLOCK(OUTAGE_BLOCK,HOUR) =
     +                                                  OUTAGE_CAPACITY
                  ENDIF
               ENDDO
               IF( SYSTEM_HOURLY_COMMITMENT(HOUR,R_TG) >
     +                  MAXIMUM_EXPORT_TIE + DECOMMIT_TARGET_LOAD) THEN
!
                  DO K = MAX_OUTAGE_BLOCKS, 1, -1
                     IF(SYSTEM_HOURLY_COMMITMENT(HOUR,R_TG) <=
     +                  MAXIMUM_EXPORT_TIE + DECOMMIT_TARGET_LOAD) EXIT
!
                        OUTAGE_BLOCK = SORTED_OPTIONS(K)
!
!
                        OUTAGE_CAPACITY =
     +                         GENERATION_BY_SEGMENT(OUTAGE_BLOCK,HOUR)

                        IF(OUTAGE_CAPACITY <= 0. .OR.
     +                    MONTH_MUST_BY_BLOCK(OUTAGE_BLOCK,HOUR) > 0.)
     +                                                            CYCLE
!
                        U = UNIT_FOR_OUTAGE_BLOCK(OUTAGE_BLOCK)
                        S_U = S_U_FOR_OUTAGE_BLOCK(OUTAGE_BLOCK)
                        B = BLOCK_FOR_OUTAGE_BLOCK(OUTAGE_BLOCK)

                        IF(SYSTEM_HOURLY_COMMITMENT(HOUR,R_TG) -
     +                        OUTAGE_CAPACITY +
     +                           MAXIMUM_IMPORT_TIE >
     +                                       DECOMMIT_TARGET_LOAD) THEN
                           IF(B == 2) THEN
                              TEMP_R =
     +                           MIN(MAX(0.,RAMP_DOWN_RATE(S_U)),
     +                            SYSTEM_HOURLY_COMMITMENT(HOUR,R_TG) -
     +                                       MAXIMUM_EXPORT_TIE -
     +                                            DECOMMIT_TARGET_LOAD)

                           ELSE
                              IF(K > 2) THEN
                                 DECOM_UNIT_PROFIT_TEST =
     +                              START_UP_COST_PER_MWH(S_U) +
     +                                 DISPATCH_COST_FOR_BLOCK(
     +                                          SORTED_OPTIONS(K-1)) -
     +                                 2.*DISPATCH_COST_FOR_BLOCK(
     +                                                    OUTAGE_BLOCK)
                              ELSE
                                 DECOM_UNIT_PROFIT_TEST = 9999.
                              ENDIF
                              IF(DECOM_UNIT_PROFIT_TEST > 0.) THEN
                                 TEMP_R = OUTAGE_CAPACITY
                              ELSE
                                 TEMP_R = 0.
                              ENDIF
                           ENDIF
                           GENERATION_BY_SEGMENT(OUTAGE_BLOCK,HOUR) =
     +                                         OUTAGE_CAPACITY - TEMP_R
                           SYSTEM_HOURLY_COMMITMENT(HOUR,R_TG) =
     +                          SYSTEM_HOURLY_COMMITMENT(HOUR,R_TG) -
     +                                                           TEMP_R
                           SYSTEM_HOURLY_DECOMMITTED(HOUR,R_TG) =
     +                           SYSTEM_HOURLY_DECOMMITTED(HOUR,R_TG) +
     +                                                           TEMP_R
                        ENDIF
! 03/20/03. RECHECK WHETHER WE NEED TO DECOMMIT THE FIRST BLOCK.
                        IF(B == 2 .AND.
     +                     DECOMMIT_THERMAL_RESOURCES .AND.
     +                      YES_ALLOW_DECOMMIT_BY_UNIT(U,R_MONTH) .AND.
     +                        SYSTEM_HOURLY_COMMITMENT(HOUR,R_TG) >
     +                              MAXIMUM_EXPORT_TIE +
     +                                       DECOMMIT_TARGET_LOAD) THEN
!
                           OUTAGE_BLOCK = OUTAGE_BLOCK_BY_SEGMENT(1,S_U)
                           OUTAGE_CAPACITY =
     +                         GENERATION_BY_SEGMENT(OUTAGE_BLOCK,HOUR)
!
                           IF(K > 2) THEN
                              DECOM_UNIT_PROFIT_TEST =
     +                          START_UP_COST_PER_MWH(S_U) +
     +                           DISPATCH_COST_FOR_BLOCK(
     +                                          SORTED_OPTIONS(K-2)) -
     +                         2.*DISPATCH_COST_FOR_BLOCK(OUTAGE_BLOCK)
                           ELSE
                              DECOM_UNIT_PROFIT_TEST = 9999.
                           ENDIF
!
! RECHECK WHETHER WE WILL STILL BE ABLE TO MEET INTERNAL OBLIGATIONS.
!
                           IF(SYSTEM_HOURLY_COMMITMENT(HOUR,R_TG) -
     +                        OUTAGE_CAPACITY +
     +                           MAXIMUM_IMPORT_TIE >
     +                                DECOMMIT_TARGET_LOAD .AND.
     +                                DECOM_UNIT_PROFIT_TEST > 0.) THEN
                             GENERATION_BY_SEGMENT(OUTAGE_BLOCK,HOUR) =
     +                                                               0.
                              SYSTEM_HOURLY_COMMITMENT(HOUR,R_TG) =
     +                           SYSTEM_HOURLY_COMMITMENT(HOUR,R_TG) -
     +                                                  OUTAGE_CAPACITY
                              SYSTEM_HOURLY_DECOMMITTED(HOUR,R_TG) =
     +                           SYSTEM_HOURLY_DECOMMITTED(HOUR,R_TG) +
     +                                                  OUTAGE_CAPACITY
                           ENDIF
!
                        ENDIF
!
                  ENDDO
!
               ENDIF

! SPINNING CALCULATION
!

               IF(HOURLY_SPINNING_CAPACITY > 0.) THEN
                 HOURLY_SPINNING_REQUIREMENT = HOURLY_SPINNING_CAPACITY
                  UNIT_SPIN_IN_SYSTEM = 0.


! 10/21/03. SECOND BLOCK TAGGED UNITS WITH AVAILABLE CAPACITY AND 
! MIN_SPIN_CAP
!
                  DO I = MAX_OUTAGE_BLOCKS, 1, -1 ! DISPATCH ORDER
                     OUTAGE_BLOCK = SORTED_OPTIONS(I)
!
                     U = UNIT_FOR_OUTAGE_BLOCK(OUTAGE_BLOCK)
                     S_U = S_U_FOR_OUTAGE_BLOCK(OUTAGE_BLOCK)
                     B = BLOCK_FOR_OUTAGE_BLOCK(OUTAGE_BLOCK)

                     IF(TVA_MARKET_RESOURCE(S_U) .OR.
     +                     GRE_MARKET_RESOURCE(S_U) .OR.
     +                        DSM_MARKET_RESOURCE(S_U) .OR.
     +                               STRICT_MARKET_RESOURCE(S_U)) CYCLE
!
                     LOCAL_BLOCK = OUTAGE_BLOCK_BY_SEGMENT(1,S_U)
!
                     IF( .NOT. CONTRIBUTES_TO_SPIN(S_U) .OR.
     +                                               B /= 2 .OR.
     +                 GENERATION_BY_SEGMENT(LOCAL_BLOCK,HOUR) == 0.)
     +                                                            CYCLE
! NO SPIN YET ASSIGNED TO UNIT IN THIS HOUR.
                     AVAILABLE_CAPACITY =
     +                         MIN(MIN_SPIN_CAP(S_U),
     +                              GET_BLOCK_AVAIL_4_HOUR(U,B,HOUR))
                     IF(AVAILABLE_CAPACITY > 0.) THEN
                        UNIT_SPIN_IN_SYSTEM(2,S_U) =
     +                     MIN(AVAILABLE_CAPACITY,
     +                                     HOURLY_SPINNING_REQUIREMENT)
                        HOURLY_SPINNING_REQUIREMENT =
     +                        HOURLY_SPINNING_REQUIREMENT -
     +                                       UNIT_SPIN_IN_SYSTEM(2,S_U)
                        IF(HOURLY_SPINNING_REQUIREMENT == 0.) EXIT
                     ENDIF
                  ENDDO
!
! SECOND BLOCK TAGGED UNITS WITH AVAILABLE CAPACITY
!
                  IF(HOURLY_SPINNING_REQUIREMENT > 0.1) THEN
                     DO I = MAX_OUTAGE_BLOCKS, 1, -1 ! DISPATCH ORDER
                        OUTAGE_BLOCK = SORTED_OPTIONS(I)

                        U = UNIT_FOR_OUTAGE_BLOCK(OUTAGE_BLOCK)
                        S_U = S_U_FOR_OUTAGE_BLOCK(OUTAGE_BLOCK)
                        B = BLOCK_FOR_OUTAGE_BLOCK(OUTAGE_BLOCK)

                        IF(TVA_MARKET_RESOURCE(S_U) .OR.
     +                     GRE_MARKET_RESOURCE(S_U) .OR.
     +                        DSM_MARKET_RESOURCE(S_U) .OR.
     +                               STRICT_MARKET_RESOURCE(S_U)) CYCLE
!
                        LOCAL_BLOCK = OUTAGE_BLOCK_BY_SEGMENT(1,S_U)
!
                        IF( .NOT. CONTRIBUTES_TO_SPIN(S_U) .OR.
     +                                               B /= 2 .OR.
     +                   GENERATION_BY_SEGMENT(LOCAL_BLOCK,HOUR) == 0.)
     +                                                            CYCLE
                        AVAILABLE_CAPACITY =
     +                      GET_BLOCK_AVAIL_4_HOUR(U,B,HOUR) -
     +                       GENERATION_BY_SEGMENT(OUTAGE_BLOCK,HOUR) -
     +                                       UNIT_SPIN_IN_SYSTEM(2,S_U)
                        IF(AVAILABLE_CAPACITY > 0.) THEN
                           TEMP_R =
     +                       MIN( AVAILABLE_CAPACITY,
     +                            MAX_SPIN_CAP(S_U),
     +                            HOURLY_SPINNING_REQUIREMENT)
                           UNIT_SPIN_IN_SYSTEM(2,S_U) =
     +                              UNIT_SPIN_IN_SYSTEM(2,S_U) + TEMP_R
                           HOURLY_SPINNING_REQUIREMENT =
     +                             HOURLY_SPINNING_REQUIREMENT - TEMP_R
                           IF(HOURLY_SPINNING_REQUIREMENT == 0.) EXIT
                        ENDIF
                     ENDDO
                  ENDIF
!
! IF STILL NOT SATISFIED, SECOND BLOCK TAGGED UNITS AND DISPLACE 
! GENERATION
!
                  IF(HOURLY_SPINNING_REQUIREMENT > 0.1) THEN
                     DO I = MAX_OUTAGE_BLOCKS, 1, -1 ! DISPATCH ORDER
                        OUTAGE_BLOCK = SORTED_OPTIONS(I)
!
                        U = UNIT_FOR_OUTAGE_BLOCK(OUTAGE_BLOCK)
                        S_U = S_U_FOR_OUTAGE_BLOCK(OUTAGE_BLOCK)
                        B = BLOCK_FOR_OUTAGE_BLOCK(OUTAGE_BLOCK)
!
                        LOCAL_BLOCK = OUTAGE_BLOCK_BY_SEGMENT(1,S_U)
!
                        IF( .NOT. CONTRIBUTES_TO_SPIN(S_U) .OR.
     +                                               B /= 2 .OR.
     +                   GENERATION_BY_SEGMENT(LOCAL_BLOCK,HOUR) == 0.)
     +                                                            CYCLE
                        AVAILABLE_CAPACITY =
     +                         GENERATION_BY_SEGMENT(OUTAGE_BLOCK,HOUR)
                        IF(AVAILABLE_CAPACITY > 0.) THEN
                           TEMP_R =
     +                           MIN(AVAILABLE_CAPACITY,
     +                              MAX_SPIN_CAP(S_U),
     +                                     HOURLY_SPINNING_REQUIREMENT)
                           UNIT_SPIN_IN_SYSTEM(2,S_U) =
     +                        UNIT_SPIN_IN_SYSTEM(2,S_U) + TEMP_R
                           HOURLY_SPINNING_REQUIREMENT =
     +                           HOURLY_SPINNING_REQUIREMENT - TEMP_R
                           IF(HOURLY_SPINNING_REQUIREMENT <= 0.) EXIT
                        ENDIF
                     ENDDO
                  ENDIF
               ENDIF

               IF(gc_end_point == 7 .AND. globecom_YEAR == 30 .AND.
     +                R_MONTH == 4 .AND. DAY == 13 .AND. HR == 18) THEN
!
                  TEMP_R = TEMP_R
!
               ENDIF
!
               IF(DEPTH_OF_MARKET .AND. DEPTH_OF_MARKET_DB_ACTIVE) THEN
                  TEMP_L = .FALSE.
! 060107. NEED A SEPARATE SWITCH?
                  USE_DEPTH_SOLUTION = .TRUE.
                  SYSTEM_BALANCE =
     +                     SYSTEM_HOURLY_COMMIT_AVAIL(HOUR,R_TG) +
     +                          MAXIMUM_IMPORT_TIE - HOURLY_NATIVE_LOAD
                  LOCAL_BALANCE =
     +                     SYSTEM_HOURLY_COMMIT_AVAIL(HOUR,R_TG) +
     +                          MAXIMUM_IMPORT_TIE - HOURLY_NATIVE_LOAD
!
! 2/12/03 ADDED FOR JONES.
!
                  IF(HOUR > 1) THEN
                     MIN_DEPTH_MARKET =
     +                              DEPTH_QUANTITY(HOUR-1) -
     +                                                 SYSTEM_RAMP_DOWN
                     MAX_DEPTH_MARKET =
     +                              DEPTH_QUANTITY(HOUR-1) +
     +                                                  SYSTEM_RAMP_UP
                  ELSE
                     MIN_DEPTH_MARKET = -999999.
                     MAX_DEPTH_MARKET =  999999.
                  ENDIF
!
!                 BUILD AN HOURLY DISPATCH CURVE.
!                 TRYING TO AVOID REDUNDENCY WITH THE CODE BELOW.
!
                  OUTAGE_BLOCK = 0
                  DO I = 1, MAX_OUTAGE_BLOCKS ! DISPATCH ORDER
                     OUTAGE_BLOCK = SORTED_OPTIONS(I)
!
                     U = UNIT_FOR_OUTAGE_BLOCK(OUTAGE_BLOCK)
                     S_U = S_U_FOR_OUTAGE_BLOCK(OUTAGE_BLOCK)
                     B = BLOCK_FOR_OUTAGE_BLOCK(OUTAGE_BLOCK)

                     AVAILABLE_CAPACITY =
     +                        MAX(0.,GET_BLOCK_AVAIL_4_HOUR(U,B,HOUR) -
     +                                      UNIT_SPIN_IN_SYSTEM(B,S_U))

                     IF(TVA_MARKET_RESOURCE(S_U) .OR.
     +                                STRICT_MARKET_RESOURCE(S_U)) THEN
                        AVAILABLE_CAPACITY =
     +                       DAILY_HARD_WIRED_BY_BLOCK(HR,OUTAGE_BLOCK)
                     ENDIF
                     IF(MUST_RUN_BLOCK(OUTAGE_BLOCK) == 1) THEN
                        TEMP_R = TRANSACT_ABUNDANCE_COST
                     ELSE
                        TEMP_R = DISPATCH_COST_FOR_BLOCK(OUTAGE_BLOCK)
                     ENDIF
                     IF(I == 1) THEN
                        DAILY_CUM_DISPATCH_MW(I,HR) =
     +                                               AVAILABLE_CAPACITY
                     ELSE
                        LAST_BLOCK = I - 1
                        DAILY_CUM_DISPATCH_MW(I,HR) =
     +                          DAILY_CUM_DISPATCH_MW(LAST_BLOCK,HR) +
     +                                               AVAILABLE_CAPACITY
                     ENDIF
!
                     DAILY_CUM_DISPATCH_COST(I,HR) = TEMP_R
!
                     LOCAL_LOAD =
     +                         DAILY_CUM_DISPATCH_MW(I,HR) -
     +                                    SYSTEM_HOURLY_LOAD(HOUR,R_TG)

! WRITE THE HOURLY DISPATCH ORDER HERE
!
                     IF(HR == 24 .AND.
     +                               YES_DAILY_DISPATCH_ORDER_RPT) THEN
                        IF(DX_REPORT_NOT_OPEN) THEN
                           DX_REPORT_NOT_OPEN = .FALSE.
                           DX_REPORT_VARIABLES = 24
                           DX_HOURLY_NO =
     +                          DX_HOURLY_HEADER(DX_REPORT_VARIABLES,
     +                                           DX_HOURLY_REC)
                        ENDIF
                        IF(B < 2) THEN
                           LOCAL_NAME = OPTION_NAME(S_U)(1:18)//'Q1'
                        ELSE
                           LOCAL_NAME = OPTION_NAME(S_U)(1:18)//'Q2'
                        ENDIF
                        DX_HOURLY_REC = rptrec(DX_HOURLY_NO)
                        WRITE(DX_HOURLY_NO,REC=DX_HOURLY_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        FLOAT(DAY),
     +                        LOCAL_NAME,
     +                        (DAILY_CUM_DISPATCH_MW(
     +                                           I,J),J=1,24)
                        DX_HOURLY_REC = DX_HOURLY_REC + 1

                     ENDIF
!
                  ENDDO
!
                  TEMP_L = GET_DEPTH_OF_MARKET_QUANTITY(
     +                         MAX_OUTAGE_BLOCKS,
     +                         DAILY_CUM_DISPATCH_COST(1,HR),
     +                         DAILY_CUM_DISPATCH_MW(1,HR),
     +                         SYSTEM_HOURLY_LOAD(HOUR,R_TG),
     +                         L_MONTHLY_PRICE(HOUR),
     +                         AVAILABLE_CAPACITY,
     +                         HR,
     +                         DAY,
     +                         R_TG,
     +                         DEPTH_QUANTITY(HOUR),
     +                         DEPTH_PRICE(HOUR),
     +                         LOCAL_BALANCE,
     +                         SYSTEM_BALANCE,
     +                         MIN_DEPTH_MARKET,
     +                         MAX_DEPTH_MARKET,
     +                         DEPTH_MARGINAL_COST(HOUR),
     +                         DEPTH_RESOURCES(HOUR),
     +                         CALENDAR_DAY_OF_WEEK)
!
! 053107.
! FOR TVA POSITION REPORTING
!
                  DO I = 1, MAX_OUTAGE_BLOCKS ! DISPATCH ORDER
                     OUTAGE_BLOCK = SORTED_OPTIONS(I)
!
                     U = UNIT_FOR_OUTAGE_BLOCK(OUTAGE_BLOCK)
                     S_U = S_U_FOR_OUTAGE_BLOCK(OUTAGE_BLOCK)
                     B = BLOCK_FOR_OUTAGE_BLOCK(OUTAGE_BLOCK)
!
                     IF( DAILY_CUM_DISPATCH_MW(I,HR) - .001 >
     +                                      DEPTH_RESOURCES(HOUR)) EXIT
                     IF(I == 1) THEN
                        DEPTH_HOURLY_DISPATCH_MW(I) =
     +                                    DAILY_CUM_DISPATCH_MW(I,HR) +
     +                                       UNIT_SPIN_IN_SYSTEM(B,S_U)
                     ELSE
                        DEPTH_HOURLY_DISPATCH_MW(I) =
     +                              DAILY_CUM_DISPATCH_MW(I,HR)  -
     +                                  DAILY_CUM_DISPATCH_MW(I-1,HR) +
     +                                       UNIT_SPIN_IN_SYSTEM(B,S_U)
                      ENDIF
                  ENDDO
!
! THE REQUESTED CHANGE TO THE SIMULATION.
!
!                  L_MONTHLY_PRICE(HOUR) = DEPTH_PRICE(HOUR)
!
                  C_DEPTH_MARKET_REPORT = .TRUE.
!
                  IF(C_DEPTH_MARKET_REPORT) THEN
                     DO J = 1, NUm_Prods_loc
                        IF(APPLY_ENERGY_PRODUCT(
     +                                      HR,
     +                                      CALENDAR_DAY_OF_WEEK,
     +                                     LOCAL_PRODUCT_TYPE(J))) THEN
                           PRODUCT_PRICE(J) = PRODUCT_PRICE(J) +
     +                                                DEPTH_PRICE(HOUR)
                           PRODUCT_MARGINAL_COST(J) =
     +                              PRODUCT_MARGINAL_COST(J) +
     +                                        DEPTH_MARGINAL_COST(HOUR)
                           PRODUCT_QUANTITY(J) = PRODUCT_QUANTITY(J) +
     +                                             DEPTH_QUANTITY(HOUR)
                           PRODUCT_HOURS(J) = PRODUCT_HOURS(J) + 1.
                        ENDIF
!
!
                     ENDDO
                  ENDIF
!
                  IF(HOUR == HOURS_IN_MONTH) THEN
!
! WRITES PRODUCT MARGINAL COST REPORT BY DEPTH/MONTH
!
                     TEMP_L =
     +                     WRITE_DEPTH_MARGINAL_COST(
     +                                gc_end_point,
     +                                THIS_YEAR,
     +                                R_MONTH,
     +                                TRANS_GROUP_NAME,
     +                                R_DAYS_IN_MONTH)
!
                     IF(C_DEPTH_MARKET_REPORT) THEN
!
                        IF(C_DEPTH_MARKET_NOT_OPEN) THEN
                           C_DEPTH_MARKET_NOT_OPEN = .FALSE.
                           C_DEPTH_MARKET_UNIT =
     +                           C_DEPTH_MARKET_RPT_HEADER(
     +                                              C_DEPTH_MARKET_REC)
                        ENDIF
!
                        DO J = 1, NUm_Prods_loc
!
                           IF(PRODUCT_HOURS(J) > 0.) THEN
                              PRODUCT_PRICE(J) = PRODUCT_PRICE(J)/
     +                                                 PRODUCT_HOURS(J)
                              PRODUCT_MARGINAL_COST(J) =
     +                                 PRODUCT_MARGINAL_COST(J)/
     +                                                 PRODUCT_HOURS(J)
                           ENDIF
!
                     C_DEPTH_MARKET_REC=rptrec(C_DEPTH_MARKET_UNIT)
                           WRITE(C_DEPTH_MARKET_UNIT,
     +                                          REC=C_DEPTH_MARKET_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        TRANS_GROUP_NAME,
     +                        LOCAL_PRODUCT_TYPE(J),
     +                        PRODUCT_HOURS(J),
     +                        PRODUCT_PRICE(J),
     +                        PRODUCT_MARGINAL_COST(J),
     +                        PRODUCT_QUANTITY(J)

                           C_DEPTH_MARKET_REC = C_DEPTH_MARKET_REC + 1
                        ENDDO
!
                     ENDIF
                  ENDIF
!
               ENDIF ! DEPTH_OF_MARKET AND DEPTH OF MARKET DATABASE
!
! 02/17/03. MAJOR MODEL CHANGE.
! NEED TO PRE-LOAD MUST-RUN CAPACITY HERE (TWO KINDS).  THEN, CYCLE
! CYCLE OVER THOSE UNIT BLOCKS BELOW.
! ACCOUNT FOR ALL ELEMENTS (GENERATION, CONTRIBUTION TO LOAD...)
!
!
!               IF(DAY == 13 .AND. HR == 20) THEN
!                  OUTAGE_CAPACITY = OUTAGE_CAPACITY
!               ENDIF
!
               TOTAL_HOURLY_BLOCKS = 0
               MUST_RUN_OPTIONS = 0
               DO I = 1, MAX_OUTAGE_BLOCKS
                  OUTAGE_BLOCK = SORTED_OPTIONS(I)
                 IF(MONTH_MUST_BY_BLOCK(OUTAGE_BLOCK,HOUR) <= 0.) CYCLE
                  TOTAL_HOURLY_BLOCKS = TOTAL_HOURLY_BLOCKS + 1
                  MUST_RUN_OPTIONS(TOTAL_HOURLY_BLOCKS) = OUTAGE_BLOCK
           B = MAX(int(1,2),int(BLOCK_FOR_OUTAGE_BLOCK(OUTAGE_BLOCK),2))
                  DAILY_MUST_RUN_CAPACITY(HR) =
     +                        DAILY_MUST_RUN_CAPACITY(HR) +
     +                           MONTH_MUST_BY_BLOCK(OUTAGE_BLOCK,HOUR)

               ENDDO
!
!               IF(TOTAL_HOURLY_BLOCKS > 0) THEN
!
! HOURLY DISPATCH ORDER. REMOVES UNITS ON OUTAGE. LOADS MUST-RUN'S 
! FIRST.
!
               DO I = 1, MAX_OUTAGE_BLOCKS
                  OUTAGE_BLOCK = SORTED_OPTIONS(I)
                  U = UNIT_FOR_OUTAGE_BLOCK(OUTAGE_BLOCK)
                  S_U = S_U_FOR_OUTAGE_BLOCK(OUTAGE_BLOCK)
                  B = BLOCK_FOR_OUTAGE_BLOCK(OUTAGE_BLOCK)
                  AVAILABLE_CAPACITY = GET_BLOCK_AVAIL_4_HOUR(U,B,HOUR)
                  IF(AVAILABLE_CAPACITY <= 0.) THEN
                     IF(B == 1) THEN
                        UNIT_IS_UP(S_U) = 0
                     ENDIF
                     CYCLE
                  ENDIF
!                  IF(MUST_RUN_OPTIONS(I) > 0) CYCLE
                  IF(MONTH_MUST_BY_BLOCK(OUTAGE_BLOCK,HOUR) > 0.) CYCLE
                  TOTAL_HOURLY_BLOCKS = TOTAL_HOURLY_BLOCKS + 1
                  MUST_RUN_OPTIONS(TOTAL_HOURLY_BLOCKS) = OUTAGE_BLOCK
               ENDDO
!                  DO I = 1, MAX_OUTAGE_BLOCKS
!                     SORTED_OPTIONS(I) = MUST_RUN_OPTIONS(I)
!                  ENDDO
!               ENDIF
!
! 091106.  MARKET CONTINGENT LOGIC FOR KCPL. 9035.
!
               HOURLY_IMPORT_CAP =
     +            MAX(0.,
     +                MIN(SYSTEM_HOURLY_COMMIT_AVAIL(HOUR,R_TG) +
     +                    MAXIMUM_IMPORT_TIE -
     +                    EMERGENCY_TARGET_LOAD,
     +                     MAXIMUM_IMPORT_TIE))

               HOURLY_CONTINGENT =
     +                       HOURLY_CONTINGENT_MARKET_CAP(
     +                           HR,
     +                           CALENDAR_DAY_OF_WEEK,
     +                           DAY,
     +                           R_TG,
     +                           DAILY_PRICE(HR),
     +                           R_MONTH,
     +                           HOURLY_IMPORT_CAP)
               IF(HOURLY_CONTINGENT > 0.0) THEN
                  HOURLY_NATIVE_LOAD = HOURLY_NATIVE_LOAD +
     +                                                HOURLY_CONTINGENT
                  SYSTEM_DERIVATIVES(HOUR,R_TG) =
     +                  SYSTEM_DERIVATIVES(HOUR,R_TG) -
     +                                                HOURLY_CONTINGENT
               ENDIF
!
! RESOURCES COMMITTED AT THIS POINT.
!
! ECONOMIC DISPATCH ROUTINE
!
!
!
! 12/08/03. MAJOR LOOPING RE-WRITE FOR MULTI-AREA TRANSACT C
!
!
               IF(TOTAL_HOURLY_BLOCKS > 0) THEN
                  END_OF_UNITS = .FALSE.
               ELSE
                  END_OF_UNITS = .TRUE.
               ENDIF
!
!
               IF(TRANC_SCREEN_MESSAGES) THEN
                  SCREEN_MESSAGES = 'Top of Hourly Multi-market Assign'
                  CALL MG_LOCATE_WRITE(12,26,
     +                           trim(SCREEN_MESSAGES),ALL_VERSIONS,2)
               ENDIF
!
!
               IF(MULTI_MARKET_ACTIVE) THEN
!                  END_OF_PRICING_GROUPS = .FALSE.
                  IF(NUM_OF_PRICING_GROUPS == 0) THEN
                     END_OF_PRICING_GROUPS = .TRUE.
                  ELSE
                     END_OF_PRICING_GROUPS = .FALSE.
                  ENDIF
                  RESOURCE_IS_CHEAPER = .FALSE.
!
!
! 01/12/04.
!
! ATTEMPT TO IDENTIFY ALL RESOURCES AND MARKETS THAT WILL BE USED 
! DURING THE DAY
!
                  J = 1
                  DO K = 1,  ALL_DEMAND_NUMBER
                     IF(K == 1) THEN
                        ALL_DEMAND_TYPE(K) = 1
                        ALL_DEMAND_INDEX(K) = 0 ! RETAIL
                        ALL_DEMAND_PRICE(K) = TRANSACT_UNSERVED_COST
                        ALL_DEMAND_QUANTITY(K) = HOURLY_NATIVE_LOAD
                        ALL_DEMAND_NAME(K) = 'Native Gen Retail  '
                     ELSE
                        ALL_DEMAND_TYPE(K) = 2
                        L = PRICING_GROUP_BUY_INDEX(J)
                        ALL_DEMAND_INDEX(K) = L
                        ALL_DEMAND_PRICE(K) =
     +                                    PRICING_GROUP_BUY_PRICE(L,HR)
                        ALL_DEMAND_QUANTITY(K) =
     +                                       PRICING_GROUP_BUY_QUANT(L)
                        ALL_DEMAND_NAME(K) =
     +                                        PRICING_GROUP_BUY_NAME(L)
                        J = J + 1
                     ENDIF
                  ENDDO
                  ALL_DEMAND_SERVED = 0.
                  ALL_RESOURCE_QUANTITY_USED = 0.
!
!                  ALLOW_MARKET_ARBITRAGE = .TRUE. ! MAKE A SWITCH 
! PER KCPL
!
                  I = 1 ! NEXT THERMAL RESOURCE
                  J = 1 ! NEXT ALL RESOURCE POSITION
                  K = 1 ! NEXT CUSTOMER
                  M = ALL_DEMAND_INDEX(K)+1
                  Z = 1 ! NEXT MARKET

                  HOURLY_RESOURCES_AND_MARKETS = 0.
!
! 062906. 020207.
!
                  IF(END_OF_UNITS) THEN
                     REMAIN = MOD(FLOAT(HOUR),24.)
                     IF(REMAIN < .001) THEN ! NEW DAY
                        HR = 1
                     ELSE
                        HR = HR + 1
                     ENDIF
!
                     CYCLE
!
                  ENDIF
!
                  DOWHILE(.NOT. END_OF_PRICING_GROUPS .OR.
     +                                          .NOT. END_OF_UNITS)
!
                     PRICING_BLOCK = PRICING_GROUP_SELL_INDEX(Z)
                     LOCAL_PRICING_GROUP_BID =
     +                       PRICING_GROUP_SELL_PRICE(PRICING_BLOCK,HR)
!
! 062906. TRAP FOR CASE OF END_OF_UNITS = .TRUE. (NO ACTIVE UNITS).
!
                     IF(END_OF_UNITS) THEN
                        LOCAL_UNIT_BID = 999999.
                     ELSE
                        OUTAGE_BLOCK = MUST_RUN_OPTIONS(I)
!
                        U = UNIT_FOR_OUTAGE_BLOCK(OUTAGE_BLOCK)
                        S_U = S_U_FOR_OUTAGE_BLOCK(OUTAGE_BLOCK)
                        B = BLOCK_FOR_OUTAGE_BLOCK(OUTAGE_BLOCK)

                        IF(MONTH_MUST_BY_BLOCK(OUTAGE_BLOCK,HOUR) >
     +                                                        .01) THEN
                           LOCAL_UNIT_BID =
     +                       MIN(DISPATCH_COST_FOR_BLOCK(OUTAGE_BLOCK),
     +                                         TRANSACT_ABUNDANCE_COST)

                        ELSE
                           LOCAL_UNIT_BID =
     +                            DISPATCH_COST_FOR_BLOCK(OUTAGE_BLOCK)
                        ENDIF
                     ENDIF ! END_OF_UNITS
!
                    IF( (LOCAL_PRICING_GROUP_BID < LOCAL_UNIT_BID .AND.
     +                             .NOT. END_OF_PRICING_GROUPS) .OR.
     +                                               END_OF_UNITS) THEN
                        ALL_RESOURCE_TYPE(J) = Z
                        ALL_RESOURCE_INDEX(J) = PRICING_BLOCK
                        ALL_RESOURCE_PRICE(J) = LOCAL_PRICING_GROUP_BID
!
                        ALL_RESOURCE_QUANTITY(J) =
     +                          PRICING_GROUP_SELL_QUANT(PRICING_BLOCK)
                        IF(Z == NUM_OF_PRICING_GROUPS) THEN
                           END_OF_PRICING_GROUPS = .TRUE.
                        ELSE
                           Z = Z + 1
                        ENDIF
                     ELSE
!
! NEED TO TEST FOR:
!        SPINNNING, MARKET RESOURCE, RAMPING, FUEL LIMIT, ...
!
!                        U = UNIT_FOR_OUTAGE_BLOCK(OUTAGE_BLOCK)
!                        S_U = S_U_FOR_OUTAGE_BLOCK(OUTAGE_BLOCK)
!                        B = BLOCK_FOR_OUTAGE_BLOCK(OUTAGE_BLOCK)
                        AVAILABLE_CAPACITY =
     +                                 GET_BLOCK_AVAIL_4_HOUR(U,B,HOUR)
!
                        ALL_RESOURCE_TYPE(J) = 0
                        ALL_RESOURCE_INDEX(J) = OUTAGE_BLOCK
                        ALL_RESOURCE_PRICE(J) = LOCAL_UNIT_BID
                        TEMP_R =
     +                    MAX(GENERATION_BY_SEGMENT(OUTAGE_BLOCK,HOUR),
     +                          MONTH_MUST_BY_BLOCK(OUTAGE_BLOCK,HOUR))
                        TEMP_R = MIN(TEMP_R,
     +                              AVAILABLE_CAPACITY -
     +                                      UNIT_SPIN_IN_SYSTEM(B,S_U))
                        ALL_RESOURCE_QUANTITY(J) = TEMP_R
                        IF(I == TOTAL_HOURLY_BLOCKS) THEN
                           END_OF_UNITS = .TRUE.
                        ELSE
                           I = I + 1
                        ENDIF
                     ENDIF
!
! IF MARGINAL COST TO SELL IS LESS THAN THE PRICE TO BUY, THEN BUY
!
                     TEMP_R = ALL_RESOURCE_QUANTITY(J)
! 091504. BIG CHANGE
                     IF(K < ALL_DEMAND_NUMBER) THEN
                        MARKETS_AVAILABLE = .TRUE.
                     ENDIF
! 091504. BIG CHANGE
                     DOWHILE(MARKETS_AVAILABLE) ! .AND.

                        IF(ALL_DEMAND_QUANTITY(K) > 0.1) THEN
                           IF(YES_ALLOW_MARKET_ARBITRAGE .OR.
     +                              K == 1 .OR.
     +                                  ALL_RESOURCE_TYPE(J) == 0) THEN
                              IF(ALL_DEMAND_QUANTITY(K) > TEMP_R) THEN
                                 ALL_DEMAND_QUANTITY(K) =
     +                                  ALL_DEMAND_QUANTITY(K) - TEMP_R
                                 L = ALL_RESOURCE_TYPE(J)
                                 ALL_DEMAND_SERVED(L,K) =
     +                                  ALL_DEMAND_SERVED(L,K) + TEMP_R
                                 ALL_RESOURCE_QUANTITY_USED(J) =
     +                           ALL_RESOURCE_QUANTITY_USED(J) + TEMP_R
!
                                 ALL_DEMAND_BUY_GEN(M,HR) =
     +                                ALL_DEMAND_BUY_GEN(M,HR) + TEMP_R
!
                                 IF(   TEMP_R > 10000. .OR.
     +                                 TEMP_R < -10000. .OR.
     +                                 J > ALL_RESOURCE_NUMBER .OR.
     +                                 J <= 0 .OR.
     +                                 K <= 0 .OR.
     +                                 K > ALL_DEMAND_NUMBER) THEN
                                    IF(J > ALL_RESOURCE_NUMBER .OR.
     +                                                     J <= 0) THEN
                                       SCREEN_MESSAGES =
     +                                 'J IS TOO BIG                  '
                                       CALL MG_LOCATE_WRITE(12,26,
     +                                     trim(SCREEN_MESSAGES),
     +                                                  ALL_VERSIONS,2)
                                       WRITE(SCREEN_MESSAGES,"(I3)") J
                                       CALL MG_LOCATE_WRITE(16,23,
     +                                          trim(SCREEN_MESSAGES),
     +                                                  ALL_VERSIONS,0)
                                    ELSEIF(K <= 0 .OR.
     +                                 K > ALL_DEMAND_NUMBER) THEN
                                       SCREEN_MESSAGES =
     +                                 'K IS TOO BIG                  '
                                       CALL MG_LOCATE_WRITE(12,26,
     +                                     trim(SCREEN_MESSAGES),
     +                                                  ALL_VERSIONS,2)
                                       WRITE(SCREEN_MESSAGES,"(I3)") K
                                       CALL MG_LOCATE_WRITE(16,23,
     +                                          trim(SCREEN_MESSAGES),
     +                                                  ALL_VERSIONS,0)
                                    ENDIF
                                 ENDIF

                                 HOURLY_RESOURCES_AND_MARKETS(J,K) =
     +                              HOURLY_RESOURCES_AND_MARKETS(J,K) +
     +                                                           TEMP_R
                                 HOURLY_RESOURCES_AND_MARKETS(0,K) =
     +                              HOURLY_RESOURCES_AND_MARKETS(0,K) +
     +                                                           TEMP_R
                                 HOURLY_RESOURCES_AND_MARKETS(J,0) =
     +                              HOURLY_RESOURCES_AND_MARKETS(J,0) +
     +                                                           TEMP_R

                                 EXIT
                              ELSE
                                 L = ALL_RESOURCE_TYPE(J)
!
                                 IF( J < 0 .OR.
     +                               J > ALL_RESOURCE_NUMBER .OR.
     +                                 K <= 0 .OR.
     +                                  K > ALL_DEMAND_NUMBER .OR.
     +                                   L < 0 .OR.
     +                                    L > ALL_RESOURCE_NUMBER) THEN
                                    K = K
                                 ENDIF
!
                                 ALL_DEMAND_SERVED(L,K) =
     +                                 ALL_DEMAND_SERVED(L,K) +
     +                                           ALL_DEMAND_QUANTITY(K)
                                 ALL_RESOURCE_QUANTITY_USED(J) =
     +                               ALL_RESOURCE_QUANTITY_USED(J) +
     +                                           ALL_DEMAND_QUANTITY(K)
                                 TEMP_R = TEMP_R -
     +                                           ALL_DEMAND_QUANTITY(K)
!
                                 ALL_DEMAND_BUY_GEN(M,HR) =
     +                                    ALL_DEMAND_BUY_GEN(M,HR) +
     +                                           ALL_DEMAND_QUANTITY(K)
                                 HOURLY_RESOURCES_AND_MARKETS(J,K) =
     +                              HOURLY_RESOURCES_AND_MARKETS(J,K) +
     +                                           ALL_DEMAND_QUANTITY(K)
                                 HOURLY_RESOURCES_AND_MARKETS(0,K) =
     +                              HOURLY_RESOURCES_AND_MARKETS(0,K) +
     +                                           ALL_DEMAND_QUANTITY(K)
                                 HOURLY_RESOURCES_AND_MARKETS(J,0) =
     +                              HOURLY_RESOURCES_AND_MARKETS(J,0) +
     +                                           ALL_DEMAND_QUANTITY(K)

                                 ALL_DEMAND_QUANTITY(K) =  0.
                                 K = K + 1
                                 M = ALL_DEMAND_INDEX(K)+1
                              ENDIF
!
                              SYSTEM_HOURLY_LR(HOUR,R_TG) =
     +                                            ALL_RESOURCE_PRICE(J)
                           ELSE
                              MARKETS_AVAILABLE = .FALSE.
                           ENDIF
                        ELSE
                           K = K + 1
                           M = ALL_DEMAND_INDEX(K)+1
                        ENDIF

                        IF(K > ALL_DEMAND_NUMBER) THEN
                           MARKETS_AVAILABLE = .FALSE.

! 10/29/04. CHANGED FROM ABOVE.
!
                        ! .001 is an ARBITRARY TOLERANCE
                        ELSEIF(ALL_RESOURCE_PRICE(J) +.001 > 
     +                                        ALL_DEMAND_PRICE(K)) THEN
                           MARKETS_AVAILABLE = .FALSE.
                        ENDIF
!
                     ENDDO ! K (MARKETS) LOOP COUNTER
!
! DO ALL_DEMAND AND COMPARE TO RESOURCES FOR PRICE EQUILIBRATION
!
!
! TEST ECONOMIC STARTS/RAMPS ...
!
                     J = J + 1
!
                  ENDDO ! J (RESOURCES) LOOP COUNTER
!
                  IF(NUM_OF_PRICING_GROUPS > 0) THEN
                     END_OF_PRICING_GROUPS = .FALSE.
                  ENDIF
!
                  END_OF_UNITS = .FALSE.
               ELSE
                  END_OF_PRICING_GROUPS = .TRUE.
                  RESOURCE_IS_CHEAPER = .TRUE.
               ENDIF
!
               I = 1 ! NEXT THERMAL RESOURCE
               Z = 1 ! NEXT MARKET
               L = 0 ! ALL_RESOURCE COUNTER
!
! 12/24/03.
!
               IF(USE_DEPTH_SOLUTION) THEN
                  HOURLY_MARKET_PRICE = DEPTH_PRICE(HOUR)
               ELSE
                  HOURLY_MARKET_PRICE =
     +                        (L_MONTHLY_PRICE(HOUR) + LOCAL_BUY_WHEEL)
               ENDIF
!
!
! 062906. TOTAL HOURLY BLOCKS CONDITION ADDED
!
               DOWHILE( (.NOT. END_OF_UNITS .OR.
     +                             .NOT. END_OF_PRICING_GROUPS) .AND.
     +                                         TOTAL_HOURLY_BLOCKS > 0)

!               DO I = 1, TOTAL_HOURLY_BLOCKS ! DISPATCH ORDER
!
!               DO I = 1, MAX_OUTAGE_BLOCKS ! DISPATCH ORDER
! NEED INDEX HERE
! 02/18/03. CHANGED INDEX
                  IF(USE_DEPTH_SOLUTION) THEN
                     OUTAGE_BLOCK = SORTED_OPTIONS(I)
                     OUTAGE_CAPACITY = DEPTH_HOURLY_DISPATCH_MW(I)
                  ELSE
                     OUTAGE_BLOCK = MUST_RUN_OPTIONS(I)

                     OUTAGE_CAPACITY =
     +                    MAX(GENERATION_BY_SEGMENT(OUTAGE_BLOCK,HOUR),
     +                          MONTH_MUST_BY_BLOCK(OUTAGE_BLOCK,HOUR))
                  ENDIF
!
                  U = UNIT_FOR_OUTAGE_BLOCK(OUTAGE_BLOCK)
                  S_U = S_U_FOR_OUTAGE_BLOCK(OUTAGE_BLOCK)
                  B = BLOCK_FOR_OUTAGE_BLOCK(OUTAGE_BLOCK)
!
                  TEMP_CAPACITY = 0.
                  LOCAL_RETAIL_GENERATION = 0.
!
                  AVAILABLE_CAPACITY = GET_BLOCK_AVAIL_4_HOUR(U,B,HOUR)

! 12/08/03.
!
                  IF(MULTI_MARKET_ACTIVE) THEN
                     L = L + 1
!
                     IF(L > ALL_RESOURCE_NUMBER) THEN
!
                        SCREEN_MESSAGES =
     +                                 'Exceeded MultiMarket Resources'
                        CALL MG_LOCATE_WRITE(12,26,
     +                           trim(SCREEN_MESSAGES),ALL_VERSIONS,2)
!
                     ENDIF
!
                     IF(ALL_RESOURCE_TYPE(L) == 0) THEN
                        RESOURCE_IS_CHEAPER = .TRUE.
                     ELSE
                        RESOURCE_IS_CHEAPER = .FALSE.
                     ENDIF


!
! 12/24/03. BIG CHANGE
!
                     HOURLY_MARKET_PRICE = LOCAL_PRICING_GROUP_BID
!
! 12/08/03.
! SELLS ONLY TO NATIVE LOAD.
! A SEPARATE MARKET WILL BUY FROM NATIVE RESOURCES.
!
!
                     IF(.NOT. RESOURCE_IS_CHEAPER) THEN

                        PRICING_BLOCK = ALL_RESOURCE_INDEX(L)
                        TEMP_R = ALL_RESOURCE_QUANTITY_USED(L)
                        LOCAL_PRICING_GROUP_BID = ALL_RESOURCE_PRICE(L)

                        PRICING_GROUP_SELL_GEN(PRICING_BLOCK,HR) =
     +                      PRICING_GROUP_SELL_GEN(PRICING_BLOCK,HR) +
     +                                                           TEMP_R
                        PRICING_GROUP_SELL_GEN(0,HR) =
     +                      PRICING_GROUP_SELL_GEN(0,HR) + TEMP_R
! LOCAL ENERGY BALANCE
                        LOCAL_POWER = LOCAL_POWER + TEMP_R
!
! SYSTEM CALCULATIONS FOR PURCHASES
!
                        SYSTEM_MARKET_BOUGHT(HOUR,R_TG) =
     +                           SYSTEM_MARKET_BOUGHT(HOUR,R_TG) +
     +                                                           TEMP_R
                        MON_ECO_PUCH_COST_FROM(U) =
     +                        MON_ECO_PUCH_COST_FROM(U) +
     +                             TEMP_R * LOCAL_PRICING_GROUP_BID
                        SYSTEM_MARKET_COST(HOUR,R_TG) =
     +                              SYSTEM_MARKET_COST(HOUR,R_TG) +
     +                                 TEMP_R * LOCAL_PRICING_GROUP_BID
                        MON_ECO_PUCH_ENRG_FROM(U) =
     +                               MON_ECO_PUCH_ENRG_FROM(U) + TEMP_R
!
                        IF(Z == NUM_OF_PRICING_GROUPS) THEN
                           END_OF_PRICING_GROUPS = .TRUE.
                        ELSE
                           Z = Z + 1
                        ENDIF
!
                        CYCLE
                     ELSE

                     ENDIF ! .NOT. RESOURCE IS CHEAPER
                  ENDIF
!
! 12/08/03. AT THIS POINT, SHOULD HAVE THE NEXT RESOURCE IN DISPATCH OR
!           NEXT CHEAPEST MARKET .
!
! POSSIBLY REDUCES GENERATION
!
                  IF(UNIT_SPIN_IN_SYSTEM(B,S_U) > 0.) THEN
                     SPINNING_IMPACT =
     +                  MAX(0.,
     +                       UNIT_SPIN_IN_SYSTEM(B,S_U) -
     +                          (AVAILABLE_CAPACITY - OUTAGE_CAPACITY))
                    OUTAGE_CAPACITY = OUTAGE_CAPACITY - SPINNING_IMPACT
                     AVAILABLE_CAPACITY = MIN(0., AVAILABLE_CAPACITY -
     +                                      UNIT_SPIN_IN_SYSTEM(B,S_U))
                     LOCAL_SPIN(HOUR) = LOCAL_SPIN(HOUR) +
     +                                                  SPINNING_IMPACT
                  ELSE
                     SPINNING_IMPACT = 0.
                  ENDIF

                  LOCAL_NET = LOCAL_POWER - HOURLY_NATIVE_LOAD

                  IF(DSM_MARKET_RESOURCE(S_U)) THEN
                     OUTAGE_CAPACITY = 0.
                  ELSEIF(ECITY_MARKET_RESOURCE(S_U)) THEN
                     OUTAGE_CAPACITY = MIN(OUTAGE_CAPACITY,-LOCAL_NET)
                    LOCAL_RETAIL_GENERATION = LOCAL_RETAIL_GENERATION +
     +                                                  OUTAGE_CAPACITY
                  ELSEIF(GRE_MARKET_RESOURCE(S_U)) THEN ! 10/30/03.
                     OUTAGE_CAPACITY =
     +                  MIN(DAILY_HARD_WIRED_BY_BLOCK(HR,OUTAGE_BLOCK),
     +                                                      -LOCAL_NET)
                    LOCAL_RETAIL_GENERATION = LOCAL_RETAIL_GENERATION +
     +                                                  OUTAGE_CAPACITY
                  ELSEIF(TVA_MARKET_RESOURCE(S_U) .OR.
     +                                STRICT_MARKET_RESOURCE(S_U)) THEN
                     OUTAGE_CAPACITY =
     +                       DAILY_HARD_WIRED_BY_BLOCK(HR,OUTAGE_BLOCK)
                     MON_ECO_SALES_ENRG_FROM(U) =
     +                   MON_ECO_SALES_ENRG_FROM(U) + OUTAGE_CAPACITY
                     TEMP_R = OUTAGE_CAPACITY * (LOCAL_PRICE(HOUR) +
     +                                               LOCAL_SELL_SPREAD)

                     MON_ECO_SALES_REV_FROM(U) =
     +                       MON_ECO_SALES_REV_FROM(U) + TEMP_R
                     SYSTEM_MARKET_REVENUE(HOUR,R_TG) =
     +                        SYSTEM_MARKET_REVENUE(HOUR,R_TG) + TEMP_R
!
! 01/23/04. THIS IMPOSES THE MULTI_MARKET RESOURCE ONTO 
! GENERATION AND OTHER STUFF
!
                  ELSEIF(MULTI_MARKET_ACTIVE) THEN
!
                     OUTAGE_BLOCK = ALL_RESOURCE_INDEX(L)
                     OUTAGE_CAPACITY = ALL_RESOURCE_QUANTITY_USED(L)
!
                     IF(RETAIL_AS_WHOLESALE) THEN
                        TEMP_R = HOURLY_RESOURCES_AND_MARKETS(L,1)
                        TEMP_R2 = TEMP_R * (LOCAL_PRICE(HOUR) +
     +                                               LOCAL_SELL_SPREAD)

                        MON_ECO_SALES_REV_FROM(U) =
     +                        MON_ECO_SALES_REV_FROM(U) +
     +                               TEMP_R2
                        MON_ECO_SALES_ENRG_FROM(U) =
     +                        MON_ECO_SALES_ENRG_FROM(U) + TEMP_R
                     ENDIF
!
! 01/26/04. WHO AM I SERVING? RETAIL OR WHOLESALE? IF WHOLESALE, 
! AT WHAT PRICE?
!
                     DO K = 2, ALL_DEMAND_NUMBER

                        TEMP_R = HOURLY_RESOURCES_AND_MARKETS(L,K)
                        TEMP_R2 = TEMP_R * ALL_DEMAND_PRICE(K) ! +

                        MON_ECO_SALES_ENRG_FROM(U) =
     +                     MON_ECO_SALES_ENRG_FROM(U) + TEMP_R
                        MON_ECO_SALES_REV_FROM(U) =
     +                       MON_ECO_SALES_REV_FROM(U) + TEMP_R2
 !    +                         TEMP_R * LOCAL_PRICE(HOUR)
                        SYSTEM_WHOLESALE_GENERATION(HOUR,R_TG) =
     +                        SYSTEM_WHOLESALE_GENERATION(HOUR,R_TG) +
     +                                                           TEMP_R
                        SYSTEM_MARKET_REVENUE(HOUR,R_TG) =
     +                       SYSTEM_MARKET_REVENUE(HOUR,R_TG) + TEMP_R2

                     ENDDO ! WHOLESALE MARKETS
                  ELSEIF(LOCAL_NET < 0.) THEN ! RETAIL, BUY OR STRADDLE
!
                    IF(LOCAL_NET + OUTAGE_CAPACITY > 0.) THEN ! STRADDLE

!                       BLOCK STRADDLES WHOLESALE AND RETAIL

                        SYSTEM_HOURLY_MC_AT_LOAD(HOUR,R_TG) =
     +                            DISPATCH_COST_FOR_BLOCK(OUTAGE_BLOCK)
! WHOLESALE FOR STRADDLE
                        STRADDLE_CAPACITY =
     +                            MIN(LOCAL_NET + OUTAGE_CAPACITY,
     +                                              MAXIMUM_EXPORT_TIE)
                        SYSTEM_WHOLESALE_GENERATION(HOUR,R_TG) =
     +                        SYSTEM_WHOLESALE_GENERATION(HOUR,R_TG) +
     +                                                STRADDLE_CAPACITY
!
! 5/15/02. NEED TO REDUCE MAXIMUM_EXPORT_TIE AT THIS POINT
!
                        MAXIMUM_EXPORT_TIE = MAXIMUM_EXPORT_TIE -
     +                                                STRADDLE_CAPACITY
!
                        MON_ECO_SALES_ENRG_FROM(U) =
     +                     MON_ECO_SALES_ENRG_FROM(U) +
     +                                                STRADDLE_CAPACITY
                        MON_ECO_SALES_REV_FROM(U) =
     +                       MON_ECO_SALES_REV_FROM(U) +
     +                         STRADDLE_CAPACITY *
     +                          (LOCAL_PRICE(HOUR) + LOCAL_SELL_SPREAD)
                        SYSTEM_MARKET_REVENUE(HOUR,R_TG) =
     +                     SYSTEM_MARKET_REVENUE(HOUR,R_TG) +
     +                             STRADDLE_CAPACITY *
     +                          (LOCAL_PRICE(HOUR) + LOCAL_SELL_SPREAD)

                        SYSTEM_RETAIL_GENERATION =
     +                        SYSTEM_RETAIL_GENERATION - LOCAL_NET

                        LOCAL_RETAIL_GENERATION = - LOCAL_NET 
                        OUTAGE_CAPACITY = STRADDLE_CAPACITY - LOCAL_NET

! SERVING RETAIL LOAD
                     ELSEIF(  ! RETAIL
!     +                     OUTAGE_CAPACITY > 0. .AND.
     +                     USE_RETAIL .AND.
     +                     (SYSTEM_HOURLY_COMMITMENT(HOUR,R_TG)  -
     +                                          LOCAL_SPIN(HOUR)+ .1) -
     +                        (SYSTEM_HOURLY_GENERATION(HOUR,R_TG) +
     +                                    OUTAGE_CAPACITY) >=  0.) THEN
!
! COMMIT AVAILABLE RESOURCES IF NOT ABLE TO IMPORT TO MEET NATIVE DEMAND
!
                        IF( OUTAGE_CAPACITY < AVAILABLE_CAPACITY .AND.
     +                     (HOURLY_NATIVE_LOAD -
     +                       SYSTEM_HOURLY_COMMITMENT(HOUR,R_TG) >
     +                         MAXIMUM_IMPORT_TIE -
     +                           SYSTEM_MARKET_BOUGHT(HOUR,R_TG)) .AND.
     +                                                     B == 2) THEN
!
! 5/30/02. MOVED UP.
! THIS IS WHERE WE NEED TO ADD LOGIC FOR MINIMUM UP TIME.
!
                           OUTAGE_CAPACITY =
     +                             MIN(-1*LOCAL_NET,AVAILABLE_CAPACITY)
                           SYSTEM_HOURLY_COMMITMENT(HOUR,R_TG) =
     +                        SYSTEM_HOURLY_COMMITMENT(HOUR,R_TG) +
     +                                                  OUTAGE_CAPACITY

                        ENDIF
!
                        LOCAL_RETAIL_GENERATION = OUTAGE_CAPACITY
!
                        SYSTEM_RETAIL_GENERATION =
     +                        SYSTEM_RETAIL_GENERATION +
     +                                          LOCAL_RETAIL_GENERATION
                        IF(ABS((SYSTEM_HOURLY_COMMITMENT(HOUR,R_TG)  -
     +                                              LOCAL_SPIN(HOUR)) -
     +                          (SYSTEM_HOURLY_GENERATION(HOUR,R_TG) +
     +                                     OUTAGE_CAPACITY)) < 1.) THEN
                           USE_RETAIL = .FALSE.
                        ENDIF

! BUY ! MARKET IS CHEAPER, SEE WHICH COMMITTED UNITS ARE "DISPLACED"
                     ELSEIF(SYSTEM_MARKET_BOUGHT(HOUR,R_TG) <
     +                                         MAXIMUM_IMPORT_TIE) THEN

! 091109. TEST FOR IPL
                        IF( LOCAL_BUY_WHEEL > 0. .AND.
     +                      HOURLY_MARKET_PRICE >
     +                      DISPATCH_COST_FOR_BLOCK(OUTAGE_BLOCK) .AND.
     +                              DAILY_GEN_BY_S_U(S_U,HR) > 0.) THEN
! UNIT CAN BE BROUGHT-UP TO MEET WHEEL MARGIN
                           OUTAGE_CAPACITY =
     +                             MIN(-1*LOCAL_NET,AVAILABLE_CAPACITY)
                           !
                           LOCAL_RETAIL_GENERATION = OUTAGE_CAPACITY
                           SYSTEM_RETAIL_GENERATION =
     +                                   SYSTEM_RETAIL_GENERATION +
     +                                          LOCAL_RETAIL_GENERATION

! 01/06/04. DON'T DUMP. SELL TO MARKET (AT A LOSS)
!
                           IF(OUTAGE_CAPACITY + LOCAL_NET > .1) THEN
                           ENDIF
! 111406
                        ELSEIF(MARKET_CEILING_UNIT(S_U) .AND.
     +                     CEILING_PRICE_FOR_BLOCK(OUTAGE_BLOCK) <
     +                                         LOCAL_PRICE(HOUR) .AND.
     +  B > 1) THEN ! BLOCK IS SERVING WHOLESALE ONLY
                           IF(HOUR > 1) THEN
                              OUTAGE_CAPACITY =
     +                           MAX(0.,
     +                           MIN(OUTAGE_CAPACITY,
     +                              GENERATION_BY_SEGMENT(
     +                                         OUTAGE_BLOCK,HOUR-1) -
     +                                            RAMP_DOWN_RATE(S_U)))
                           ELSE
                              OUTAGE_CAPACITY =
     +                           MAX(0.,
     +                           OUTAGE_CAPACITY - RAMP_DOWN_RATE(S_U))
                           ENDIF
! CONDITION FROM ABOVE.
                           OUTAGE_CAPACITY =
     +                             MIN(-1*LOCAL_NET,AVAILABLE_CAPACITY)
                           !
                           LOCAL_RETAIL_GENERATION = OUTAGE_CAPACITY
                           SYSTEM_RETAIL_GENERATION =
     +                                   SYSTEM_RETAIL_GENERATION +
     +                                          LOCAL_RETAIL_GENERATION
!
                        ELSE
!
                           TEMP_CAPACITY =
     +                            MIN(-1.*LOCAL_NET,AVAILABLE_CAPACITY,
     +                        MAXIMUM_IMPORT_TIE -
     +                                 SYSTEM_MARKET_BOUGHT(HOUR,R_TG))
                           SYSTEM_MARKET_BOUGHT(HOUR,R_TG) =
     +                           SYSTEM_MARKET_BOUGHT(HOUR,R_TG) +
     +                                                    TEMP_CAPACITY
!
! NEW IMPORT CONSTRAINT LOGIC
!
                           IF(MAXIMUM_SECOND_IMPORT_TIE > 0.01) THEN
                              IF(TEMP_CAPACITY -
     +                            MAXIMUM_SECOND_IMPORT_TIE > .01) THEN
                                 MAXIMUM_FIRST_IMPORT_TIE =
     +                              MAXIMUM_FIRST_IMPORT_TIE -
     +                                    (TEMP_CAPACITY -
     +                                       MAXIMUM_SECOND_IMPORT_TIE)
                                 MAXIMUM_SECOND_IMPORT_TIE = 0.
                                 LOCAL_BUY_WHEEL =
     +                                           LOCAL_SECOND_BUY_WHEEL
                              ELSE
                                 MAXIMUM_SECOND_IMPORT_TIE =
     +                                    MAXIMUM_SECOND_IMPORT_TIE -
     +                                                    TEMP_CAPACITY
                              ENDIF

                           ELSE
                              MAXIMUM_FIRST_IMPORT_TIE =
     +                              MAXIMUM_FIRST_IMPORT_TIE -
     +                                                    TEMP_CAPACITY
                           ENDIF

                           MON_ECO_PUCH_COST_FROM(U) =
     +                        MON_ECO_PUCH_COST_FROM(U) +
     +                             TEMP_CAPACITY *
     +                         (HOURLY_MARKET_PRICE-LOCAL_BUY_SPREAD)

                           SYSTEM_MARKET_COST(HOUR,R_TG) =
     +                              SYSTEM_MARKET_COST(HOUR,R_TG) +
     +                             TEMP_CAPACITY *
     +                         (HOURLY_MARKET_PRICE-LOCAL_BUY_SPREAD)
                           MON_ECO_PUCH_ENRG_FROM(U) =
     +                        MON_ECO_PUCH_ENRG_FROM(U) + TEMP_CAPACITY
                        ENDIF
                     ELSEIF(OUTAGE_CAPACITY < AVAILABLE_CAPACITY) THEN
                        ! BRING THE UNIT UP TO SERVE LOAD ???
                        OUTAGE_CAPACITY =
     +                             MIN(-1*LOCAL_NET,AVAILABLE_CAPACITY)
!
                        LOCAL_RETAIL_GENERATION = OUTAGE_CAPACITY
                        SYSTEM_RETAIL_GENERATION =
     +                                   SYSTEM_RETAIL_GENERATION +
     +                                          LOCAL_RETAIL_GENERATION
                     ELSE ! UNSERVED ???
                     ENDIF
!  111406
                  ELSEIF(OUTAGE_CAPACITY > 0. .AND.
     +                                    MAXIMUM_EXPORT_TIE > 0.) THEN
                       ! BLOCK IS SERVING WHOLESALE ONLY     

                     IF(MARKET_FLOOR_UNIT(S_U) .AND.
     +                  FLOOR_PRICE_FOR_BLOCK(OUTAGE_BLOCK) >
     +                                         LOCAL_PRICE(HOUR) .AND.
     +                                                      B > 1) THEN
                        ! BLOCK IS SERVING WHOLESALE ONLY     
                        IF(HOUR > 1) THEN
                           OUTAGE_CAPACITY =
     +                        MAX(0.,
     +                          MIN(OUTAGE_CAPACITY,
     +                            GENERATION_BY_SEGMENT(
     +                                         OUTAGE_BLOCK,HOUR-1) -
     +                                            RAMP_DOWN_RATE(S_U)))
                        ELSE
                           OUTAGE_CAPACITY =
     +                        MAX(0.,
     +                           OUTAGE_CAPACITY - RAMP_DOWN_RATE(S_U))
                        ENDIF
!
                     ENDIF
!
                     OUTAGE_CAPACITY = MIN(OUTAGE_CAPACITY,
     +                                              MAXIMUM_EXPORT_TIE)

! NEW EXPORT CONSTRAINT LOGIC
!
                     IF(MAXIMUM_SECOND_EXPORT_TIE > 0.) THEN
                        IF(OUTAGE_CAPACITY >
     +                                  MAXIMUM_SECOND_EXPORT_TIE) THEN
                           MAXIMUM_FIRST_EXPORT_TIE =
     +                              MAXIMUM_FIRST_EXPORT_TIE -
     +                                    (OUTAGE_CAPACITY -
     +                                       MAXIMUM_SECOND_EXPORT_TIE)
                           MAXIMUM_SECOND_EXPORT_TIE = 0.
                        ELSE
                           MAXIMUM_SECOND_EXPORT_TIE =
     +                                    MAXIMUM_SECOND_EXPORT_TIE -
     +                                                  OUTAGE_CAPACITY
                        ENDIF

                     ELSE
                        MAXIMUM_FIRST_EXPORT_TIE =
     +                              MAXIMUM_FIRST_EXPORT_TIE -
     +                                                  OUTAGE_CAPACITY
                     ENDIF
! ORIGINAL CONDITION
                     MAXIMUM_EXPORT_TIE =
     +                             MAXIMUM_EXPORT_TIE - OUTAGE_CAPACITY
!
                     MON_ECO_SALES_ENRG_FROM(U) =
     +                   MON_ECO_SALES_ENRG_FROM(U) + OUTAGE_CAPACITY
                     MON_ECO_SALES_REV_FROM(U) =
     +                       MON_ECO_SALES_REV_FROM(U) +
     +                           OUTAGE_CAPACITY *
     +                          (LOCAL_PRICE(HOUR) + LOCAL_SELL_SPREAD)
                     SYSTEM_MARKET_REVENUE(HOUR,R_TG) =
     +                           SYSTEM_MARKET_REVENUE(HOUR,R_TG) +
     +                               OUTAGE_CAPACITY * 
     +                          (LOCAL_PRICE(HOUR) + LOCAL_SELL_SPREAD)
                     SYSTEM_WHOLESALE_GENERATION(HOUR,R_TG) =
     +                        SYSTEM_WHOLESALE_GENERATION(HOUR,R_TG) +
     +                                                  OUTAGE_CAPACITY
!
!
! REASSIGN OUTAGE_CAPACITY TO ACCOUNT FOR MUST TAKE =>
!     FOR GENERATION, NOT FOR IMPORT/EXPORT/REVENUES
! ACCOUNTS FOR MUST TAKE CAPACITY
!
!
!
                  ELSE ! RESOURCES IS ABOVE NATIVES WITHOUT TIE 
! LINE CONSTRAINTS
                     OUTAGE_CAPACITY = 0. ! 5/16/02.
                  ENDIF
!
                  IF(RETAIL_AS_WHOLESALE) THEN
                     MON_ECO_SALES_REV_FROM(U) =
     +                        MON_ECO_SALES_REV_FROM(U) +
     +                               LOCAL_RETAIL_GENERATION *
     +                          (LOCAL_PRICE(HOUR) + LOCAL_SELL_SPREAD)
                     MON_ECO_SALES_ENRG_FROM(U) =
     +                        MON_ECO_SALES_ENRG_FROM(U) +
     +                                          LOCAL_RETAIL_GENERATION
                  ENDIF

                  IF(RETAIL_AS_WHOLESALE .AND. OUTAGE_CAPACITY <
     +                            MONTH_MUST_BY_BLOCK(
     +                                        OUTAGE_BLOCK,HOUR) ) THEN
                     MON_ECO_SALES_REV_FROM(U) =
     +                        MON_ECO_SALES_REV_FROM(U) +
     +                   (MONTH_MUST_BY_BLOCK(OUTAGE_BLOCK,HOUR) -
     +                                               OUTAGE_CAPACITY) *
     +                          (LOCAL_PRICE(HOUR) + LOCAL_SELL_SPREAD)
                     MON_ECO_SALES_ENRG_FROM(U) =
     +                        MON_ECO_SALES_ENRG_FROM(U) +
     +                         MONTH_MUST_BY_BLOCK(OUTAGE_BLOCK,HOUR) -
     +                                  OUTAGE_CAPACITY
                  ENDIF
                  OUTAGE_CAPACITY = MAX(OUTAGE_CAPACITY,
     +                          MONTH_MUST_BY_BLOCK(OUTAGE_BLOCK,HOUR))
!
                  IF(OUTAGE_CAPACITY > 0.) THEN
!
                     SYSTEM_HOURLY_MC(HOUR,R_TG) =
     +                            DISPATCH_COST_FOR_BLOCK(OUTAGE_BLOCK)
                  ENDIF
!
                  DAILY_GEN_BY_S_U(S_U,HR) = DAILY_GEN_BY_S_U(S_U,HR) +
     +                                                  OUTAGE_CAPACITY
                  DAILY_GEN_BY_S_U(0,HR) = DAILY_GEN_BY_S_U(0,HR) +
     +                                                  OUTAGE_CAPACITY
! NEED TO CALCULATE TOTAL AVERAGE COST
!
                  TOTAL_BLOCK_COST = OUTAGE_CAPACITY *
     +                            DISPATCH_COST_FOR_BLOCK(OUTAGE_BLOCK)
                  IF(OUTAGE_CAPACITY > 0.) THEN
                     PERCENT_RETAIL =
     +                          LOCAL_RETAIL_GENERATION/OUTAGE_CAPACITY
                     PERCENT_WHOLESALE = 1. - PERCENT_RETAIL
                  ELSE
                     PERCENT_RETAIL = 0.
                     PERCENT_WHOLESALE = 0.
                  ENDIF
! 1 = RETAIL COST
! 2 = WHOLESALE COST
                  DAILY_SYSTEM_COST_AND_REV(1,HR) =
     +                     DAILY_SYSTEM_COST_AND_REV(1,HR) +
     +                                PERCENT_RETAIL * TOTAL_BLOCK_COST
                  DAILY_SYSTEM_COST_AND_REV(2,HR) =
     +                     DAILY_SYSTEM_COST_AND_REV(2,HR) +
     +                             PERCENT_WHOLESALE * TOTAL_BLOCK_COST
!
                  DAILY_COST_BY_S_U(S_U,HR) =
     +                              DAILY_COST_BY_S_U(S_U,HR) +
     +                                                 TOTAL_BLOCK_COST
                  DAILY_SPIN_BY_S_U(S_U,HR) =
     +                              DAILY_SPIN_BY_S_U(S_U,HR) +
     +                                       UNIT_SPIN_IN_SYSTEM(B,S_U)
                  DAILY_COST_BY_S_U(0,HR) = DAILY_COST_BY_S_U(0,HR) +
     +                                                 TOTAL_BLOCK_COST
!
                  SYSTEM_SPIN(HOUR,R_TG) =
     +                             SYSTEM_SPIN(HOUR,R_TG) +
     +                                       UNIT_SPIN_IN_SYSTEM(B,S_U)
!
! 01/30/02. GAT.
! 01/15/03. RE-WRITTEN FOR QUADRADIC
!
! 12/20/04. TEST
!
                  O = U ! ORIGINAL_VALUE_OF_U(S_U)
!
                  IF(B <= 1) THEN
                     TOTAL_HEAT =
!     +                  GET_HEAT_RATE_FACTOR(U) *
     +                              COEFF(1,O) * OUTAGE_CAPACITY
                     UNIT_HEAT_PARAM(B,S_U) =
     +                              UNIT_HEAT_PARAM(B,S_U) + TOTAL_HEAT
                  ELSEIF(B == 2 .AND.  OUTAGE_CAPACITY > 0.01) THEN
                     TEMP_R =  GET_BLOCK_AVAIL_4_HOUR(U,INT(1,2),HOUR)
                     TEMP_R2 =  GET_BLOCK_AVAIL_4_HOUR(U,INT(2,2),HOUR)
                     IF(AVAILABLE_CAPACITY == 0.) THEN
                        TEMP_R = TEMP_R
                     ENDIF
                     A_CO = (COEFF(2,O)-COEFF(3,O))/
     +                                          (-2.*TEMP_R2)
                     B_CO = COEFF(2,O) - (COEFF(2,O) - COEFF(3,O)) *
     +                        TEMP_R/(-1.*TEMP_R2)
                     C_CO = ((COEFF(1,O)-B_CO)*
     +                               TEMP_R) -
     +                             A_CO*TEMP_R*TEMP_R
         ! TOTAL HEAT FOR THE BLOCK: NOTE MIN BLOCK SUBTRACTED BELOW
                     TOTAL_HEAT =   

     +                       (A_CO*
     +                       (TEMP_R+OUTAGE_CAPACITY)*
     +                       (TEMP_R+OUTAGE_CAPACITY) +
     +                       B_CO*
     +                       (TEMP_R+OUTAGE_CAPACITY)+
     +           C_CO)        - TEMP_R*COEFF(1,O) ! MIN BLOCK HEAT
                     UNIT_HEAT_PARAM(B,S_U) =
     +                             UNIT_HEAT_PARAM(B,S_U) + TOTAL_HEAT
                  ELSE
                     TOTAL_HEAT = 0.
                  ENDIF
! 022308.
                  TOTAL_HEAT = TOTAL_HEAT *
     +                  GET_HEAT_RATE_FACTOR(U)
!
                  DAILY_FUEL_BY_S_U(S_U,HR) =
     +                                      DAILY_FUEL_BY_S_U(S_U,HR) +
     +                                                   TOTAL_HEAT
                  DAILY_FUEL_BY_S_U(0,HR) = DAILY_FUEL_BY_S_U(0,HR) +
     +                                                   TOTAL_HEAT
!
                  TEMP_I = THIS_YEAR
                  TEMP_L = REDUCE_HOURLY_FUEL_DERIVATIVES(
     +                                          HOUR,
     +                                          CL_RESOURCE_ID(U),
     +                                          TOTAL_HEAT,
     +                                          R_MONTH,
     +                                          HOURLY_MARKET_PRICE,
     +                                          TEMP_I)
!     +                                          L_MONTHLY_PRICE(HOUR))
!
                  UNIT_GEN_IN_SYSTEM(B,S_U) =
     +                       UNIT_GEN_IN_SYSTEM(B,S_U) + OUTAGE_CAPACITY

                  UNIT_GEN_IN_SYSTEM(B,0) =
     +                       UNIT_GEN_IN_SYSTEM(B,0) + OUTAGE_CAPACITY
!
! 5/16/02. 07/24/03. CHECK FOR START-UPS AND SHUT-DOWNS.
!
                  IF(B == 1) THEN
                     IF(DAILY_GEN_BY_S_U(S_U,HR) > 0.) THEN
                        IF(HOUR > 1 .AND. UNIT_IS_UP(S_U) == 0) THEN
                           MONTH_UNIT_STARTS(S_U) =
     +                                        MONTH_UNIT_STARTS(S_U) + 1
                           IF(MIN_UP_TIME(S_U) > 0.) THEN
                         MAX_J = MAX(int(0,2),int(MIN_UP_TIME(S_U)-1,2))
                              MAX_J = MIN(HOUR_IN_MONTH,HOUR+MAX_J)
                              DO J = HOUR, MAX_J
                                 LOCAL_CAPACITY =
     +                                     GET_BLOCK_AVAIL_4_HOUR(U,B,J)
                                 IF(LOCAL_CAPACITY <= .0001) then
                                    EXIT ! 07/24/03
                                 endif
!
                                 IN_MIN_UP_TIME_STATE(S_U,J) = 1
                              ENDDO
                           ENDIF
                        ENDIF
                        UNIT_IS_UP(S_U) = 1
                     ELSEIF(DAILY_GEN_BY_S_U(S_U,HR) <= 0.001) THEN
                        IF(HOUR > 1 .AND. UNIT_IS_UP(S_U) == 1) THEN
                           MONTH_UNIT_DOWNS(S_U) =
     +                                        MONTH_UNIT_DOWNS(S_U) + 1
                           IF(MIN_DOWN_TIME(S_U) > 0.) THEN
                       MAX_J = MAX(int(0,2),int(MIN_DOWN_TIME(S_U)-1,2))
                              MAX_J = MIN(HOUR_IN_MONTH,HOUR+MAX_J)
                              DO J = HOUR, MAX_J
                                 IN_MIN_DOWN_TIME_STATE(S_U,J) = 1
                              ENDDO
                           ENDIF
                        ENDIF
                        UNIT_IS_UP(S_U) = 0
                     ENDIF

                  ENDIF

                  SYSTEM_HOURLY_GENERATION(HOUR,R_TG)  =
     +                     SYSTEM_HOURLY_GENERATION(HOUR,R_TG) +
     +                                                   OUTAGE_CAPACITY
!
                  LOCAL_POWER = LOCAL_POWER +
     +                                   OUTAGE_CAPACITY + TEMP_CAPACITY
!
! NOTE: REASSIGNMENT OF GENERATION_BY_SEGMENT
!
                  GENERATION_BY_SEGMENT(OUTAGE_BLOCK,HOUR) =
     +                                                   OUTAGE_CAPACITY
!
!
                  OPTION_INDEX = ORIGINAL_VALUE_OF_U(S_U)



                  IF(RESOURCE_IS_CHEAPER) THEN
                     IF(I == TOTAL_HOURLY_BLOCKS) THEN
                        END_OF_UNITS = .TRUE.
                     ELSE
                        I = I + 1
                     ENDIF
                  ELSE
                     WRITE(4,*) "WITHIN MULTI-AREA TRANSACT C"
                     WRITE(4,*) "INFEASIBLE TRANSFER CONDITION"
                     er_message='Stop requested from TF_OBJT2 SIID300'
                     call end_program(er_message)
                  ENDIF
!
                  IF(SYSTEM_MARKET_BOUGHT(HOUR,TG) > .01) THEN
                     AVE_BUY = SYSTEM_MARKET_COST(HOUR,TG)/
     +              SYSTEM_MARKET_BOUGHT(HOUR,TG)
                     AVE_BUY = AVE_BUY
                  ENDIF
!
               ENDDO ! BLOCKS AND MARKETS

!        GENERATION IS UNCHANGED. REVENUES ASSOCIATED WITH MARKET SALES
!           OFF OF THE MOST EXPENSIVE RESOURCE ARE REDUCED.


               IF(ECITY_S_U > 0) THEN
                  IF(DAILY_GEN_BY_S_U(ECITY_S_U,HR) > 0.1) THEN
                     ECITY_USED = .TRUE.
                  ELSE
                     ECITY_USED = .FALSE.
                  ENDIF
               ELSE
                  ECITY_USED = .FALSE.
               ENDIF
!
               TEMP_R = SYSTEM_WHOLESALE_GENERATION(HOUR,R_TG)

               TEMP_R2 = SYSTEM_MARKET_BOUGHT(HOUR,R_TG) -
     +                                                MIN(LOCAL_NET,0.)
!
!
               HOURLY_LOAD_FOLLOW(HR) =
     +                      HOURLY_LF_CAPACITY(
     +                                 HR,
     +                                 DAILY_PRICE(HR),
     +                                 CALENDAR_DAY_OF_WEEK,
     +                                 DAY,
     +                                 R_TG,
     +                                 R_MONTH,
     +                                 TEMP_R,
     +                                 TEMP_R2,
     +                                 ECITY_USED)
!
!
!
               IF(LOCAL_NET < 0.) THEN
                  TEMP_LOAD_FOLLOW = MAX(0.,HOURLY_LOAD_FOLLOW(HR) -
     +                                                        LOCAL_NET)
               ELSE
                  TEMP_LOAD_FOLLOW = HOURLY_LOAD_FOLLOW(HR)
               ENDIF
!
! ONLY HANDLES PURCHASE/CALL AND SELL/PUT COMBINATIONS.
!
!
               IF(TEMP_LOAD_FOLLOW > 0.1) THEN
!
                  TEMP_R = HOURLY_LOAD_FOLLOW(HR)
!
                  I = TOTAL_HOURLY_BLOCKS
!
                  DOWHILE(I > 0 .AND. TEMP_R > .01)
!
                     OUTAGE_BLOCK = MUST_RUN_OPTIONS(I)
                     U = UNIT_FOR_OUTAGE_BLOCK(OUTAGE_BLOCK)
                     S_U = S_U_FOR_OUTAGE_BLOCK(OUTAGE_BLOCK)
                     B = BLOCK_FOR_OUTAGE_BLOCK(OUTAGE_BLOCK)
!
                     OUTAGE_CAPACITY = MIN(MON_ECO_SALES_ENRG_FROM(U),
     +                                                           TEMP_R)
!
! BACKING-OUT THE TRANSACTION.
! THE CONTRA TRANSACTION OCCURS WITHIN HOURLY_LF_CAPACITY
!
                     TEMP_R = MAX(0.,TEMP_R - OUTAGE_CAPACITY)
!
                     MON_ECO_SALES_ENRG_FROM(U) =
     +                   MON_ECO_SALES_ENRG_FROM(U) - OUTAGE_CAPACITY
                     MON_ECO_SALES_REV_FROM(U) =
     +                       MON_ECO_SALES_REV_FROM(U) -
     +                           OUTAGE_CAPACITY *
     +                          (LOCAL_PRICE(HOUR) + LOCAL_SELL_SPREAD)
                     SYSTEM_MARKET_REVENUE(HOUR,R_TG) =
     +                           SYSTEM_MARKET_REVENUE(HOUR,R_TG) -
     +                             OUTAGE_CAPACITY * 
     +                           (LOCAL_PRICE(HOUR) + LOCAL_SELL_SPREAD)
                     SYSTEM_WHOLESALE_GENERATION(HOUR,R_TG) =
     +                        SYSTEM_WHOLESALE_GENERATION(HOUR,R_TG) -
     +                                                   OUTAGE_CAPACITY
!
                     I = I - 1
!
                  ENDDO
! IMBALANCE.
                  IF(ABS(TEMP_R) > 0.1) THEN
                     WRITE(4,*) "IMBALANCE IN THE LOAD FOLLOWING"
                     WRITE(4,*) "PROGRAM. PLEASE REPORT TO M.S.G."
                     WRITE(4,*) "MONTH DAY HOUR",R_MONTH,DAY,HR
                     WRITE(4,*) "IMBALANCE=",TEMP_R,"CALL"

                  ENDIF
               ELSEIF(TEMP_LOAD_FOLLOW < -0.1) THEN
!
                  TEMP_R = ABS(HOURLY_LOAD_FOLLOW(HR))

                  I = TOTAL_HOURLY_BLOCKS + 1
!
                  DOWHILE(I > 1 .AND. TEMP_R > .01)
                     I = I - 1
!
                     OUTAGE_BLOCK = MUST_RUN_OPTIONS(I)
                     U = UNIT_FOR_OUTAGE_BLOCK(OUTAGE_BLOCK)
                     S_U = S_U_FOR_OUTAGE_BLOCK(OUTAGE_BLOCK)
                     B = BLOCK_FOR_OUTAGE_BLOCK(OUTAGE_BLOCK)
!
                     OUTAGE_CAPACITY = MIN(MON_ECO_PUCH_ENRG_FROM(U),
     +                                                           TEMP_R)
!
! BACKING-OUT THE TRANSACTION.
! THE CONTRA TRANSACTION OCCURS WITHIN HOURLY_LF_CAPACITY
!
                     TEMP_R = MAX(0.,TEMP_R - OUTAGE_CAPACITY)
!
                     MON_ECO_PUCH_ENRG_FROM(U) =
     +                   MON_ECO_PUCH_ENRG_FROM(U) - OUTAGE_CAPACITY
                     MON_ECO_PUCH_COST_FROM(U) =
     +                       MON_ECO_PUCH_COST_FROM(U) -
     +                               OUTAGE_CAPACITY *
     +                          (HOURLY_MARKET_PRICE-LOCAL_BUY_SPREAD)
!     +                           OUTAGE_CAPACITY * LOCAL_PRICE(HOUR)
                     SYSTEM_MARKET_COST(HOUR,R_TG) =
     +                           SYSTEM_MARKET_COST(HOUR,R_TG) -
     +                               OUTAGE_CAPACITY *
     +                          (HOURLY_MARKET_PRICE-LOCAL_BUY_SPREAD)
!                                                    LOCAL_PRICE(HOUR)
                     SYSTEM_MARKET_BOUGHT(HOUR,R_TG) =
     +                        SYSTEM_MARKET_BOUGHT(HOUR,R_TG) -
     +                                                   OUTAGE_CAPACITY
!
!                     I = I + 1
!
                  ENDDO
! IMBALANCE.
                  IF(ABS(TEMP_R) > 0.1) THEN
                     WRITE(4,*) "IMBALANCE IN THE LOAD FOLLOWING"
                     WRITE(4,*) "PROGRAM. PLEASE REPORT TO M.S.G."
                     WRITE(4,*) "MONTH DAY HOUR",R_MONTH,DAY,HR
                     WRITE(4,*) "IMBALANCE=",TEMP_R,"PUT"

                  ENDIF

               ENDIF

!
               TEMP_R = (HOURLY_NATIVE_LOAD +
     +                        HOURLY_LOAD_FOLLOW(HR) +
     +                         SYSTEM_WHOLESALE_GENERATION(HOUR,R_TG)) -
     +                        (SYSTEM_HOURLY_GENERATION(HOUR,R_TG) +
     +                                  SYSTEM_MARKET_BOUGHT(HOUR,R_TG))
               IF( TEMP_R < -.1) THEN
                  DUMP_ENERGY(HR) = - TEMP_R
               ENDIF
               IF(  TEMP_R > .1) THEN
                     SYSTEM_ENERGY_ABOVE(HOUR,R_TG) = TEMP_R
               ELSEIF( TEMP_R < -.1) THEN
                  DUMP_ENERGY(HR) = - TEMP_R
               ENDIF

               IF(TEMP_R  >
     +               MAXIMUM_IMPORT_TIE -
     +                             SYSTEM_MARKET_BOUGHT(HOUR,R_TG)) THEN

                     SYSTEM_UNSERVED_ENERGY(HOUR,R_TG) =
     +                                       TEMP_R -
     +                   (MAXIMUM_IMPORT_TIE-
     +                                  SYSTEM_MARKET_BOUGHT(HOUR,R_TG))

               ENDIF
!
               SYSTEM_MARKET_BOUGHT(HOUR,R_TG) =
     +                  SYSTEM_MARKET_BOUGHT(HOUR,R_TG) +
     +                         MAX(0.,MIN(TEMP_R,
     +                   MAXIMUM_IMPORT_TIE-
     +                                SYSTEM_MARKET_BOUGHT(HOUR,R_TG)))

               SYSTEM_HOURLY_GENERATION(0,R_TG) =
     +                     SYSTEM_HOURLY_GENERATION(0,R_TG) +
     +                               SYSTEM_HOURLY_GENERATION(HOUR,R_TG)
!
               IF(SYSTEM_HOURLY_GENERATION(HOUR,R_TG) <
     +                                          HOURLY_NATIVE_LOAD) THEN
!
                  HOURLY_MARKET_COST =
     +               (HOURLY_NATIVE_LOAD -
     +                           SYSTEM_HOURLY_GENERATION(HOUR,R_TG)) *
     +                         (HOURLY_MARKET_PRICE-LOCAL_BUY_SPREAD)

! 03/08/04. MUST DO THIS WITH MULTI-MARKET AND LOAD FOLLOWING.
!
                  IF(.NOT. MULTI_MARKET_ACTIVE) THEN
                     SYSTEM_MARKET_COST(HOUR,R_TG) =
     +                           SYSTEM_MARKET_COST(HOUR,R_TG) +
     +                                 SYSTEM_ENERGY_ABOVE(HOUR,R_TG)*
     +                            (HOURLY_MARKET_PRICE-LOCAL_BUY_SPREAD)
                  ENDIF
!
                  MARKET_PURCHASE_COST(HOUR,R_TG) = HOURLY_MARKET_COST
                  MARKET_PURCHASE_COST(0,R_TG) =
     +                 MARKET_PURCHASE_COST(0,R_TG) + HOURLY_MARKET_COST
               ENDIF
!
               IF(SYSTEM_WHOLESALE_GENERATION(HOUR,R_TG) > 0.) THEN
! 062106
                  SYSTEM_COST_OF_MARKET_SALES(HOUR,R_TG,1) =
     +                  SYSTEM_COST_OF_MARKET_SALES(HOUR,R_TG,1) +
     +                                   DAILY_SYSTEM_COST_AND_REV(2,HR)
                  IF(IS_5X16) THEN
                     SYSTEM_COST_OF_MARKET_SALES(HOUR,R_TG,2) =
     +                  SYSTEM_COST_OF_MARKET_SALES(HOUR,R_TG,2) +
     +                                   DAILY_SYSTEM_COST_AND_REV(2,HR)
                  ELSE
                     SYSTEM_COST_OF_MARKET_SALES(HOUR,R_TG,3) =
     +                  SYSTEM_COST_OF_MARKET_SALES(HOUR,R_TG,3) +
     +                                   DAILY_SYSTEM_COST_AND_REV(2,HR)
                  ENDIF
!
                  DAILY_SYSTEM_COST_AND_REV(2,HR) =
     +                        DAILY_SYSTEM_COST_AND_REV(2,HR)/
     +                            SYSTEM_WHOLESALE_GENERATION(HOUR,R_TG)
               ENDIF
               IF(SYSTEM_HOURLY_LOAD(HOUR,R_TG) > 0.) THEN
                  DAILY_SYSTEM_COST_AND_REV(1,HR) =
     +                        DAILY_SYSTEM_COST_AND_REV(1,HR)/
     +                                     SYSTEM_HOURLY_LOAD(HOUR,R_TG)
               ENDIF
!
               IF(HOURLY_NATIVE_LOAD > 0.) THEN

                  SYSTEM_NATIVE_COST(HOUR,R_TG) = HOURLY_MARKET_PRICE *
     +                               HOURLY_NATIVE_LOAD

                  IF(PRICE_ONLY_WHOLESALE_REV) THEN
                     IF(SYSTEM_MARKET_BOUGHT(HOUR,R_TG) > 0.) THEN
                        PERCENT_OF_COST_2_ALLOCATE =
     +                              SYSTEM_MARKET_BOUGHT(HOUR,R_TG)/
     +                                                HOURLY_NATIVE_LOAD
                     ELSE
                        PERCENT_OF_COST_2_ALLOCATE = 0.
                     ENDIF
                  ELSE
                     PERCENT_OF_COST_2_ALLOCATE = 1.0
                  ENDIF
!
                  IF(BLOCKS_2_CUSTOMERS_REPORT) THEN
!                     ALLOC_COSTS_BY_UNIT = .FALSE.
                     TEMP_L = ALLOCATE_BLOCKS_2_CUSTOMERS(HOUR,R_TG,
     +                         R_MONTH,MAX_OUTAGE_BLOCKS,
     +                         TOTAL_HOURLY_BLOCKS,
     +                         GENERATION_BY_SEGMENT(1,HOUR),
     +                         DISPATCH_COST_FOR_BLOCK,
     +                         INCREMENTAL_FUEL_COST,
     +                         RETAIL_REVENUE_FOR_BLOCK,
     +                         HOURLY_MARKET_PRICE,
     +                         SYSTEM_MARKET_BOUGHT(HOUR,R_TG),
     +                         HOURS_IN_MONTH,MUST_RUN_OPTIONS,
     +                         UNIT_FOR_OUTAGE_BLOCK,
     +                         BLOCK_FOR_OUTAGE_BLOCK,SORTED_OPTIONS,
     +                         SYSTEM_HOURLY_LOAD(HOUR,R_TG),
     +                         ALLOC_COSTS_BY_UNIT,
     +                         SYSTEM_UNSERVED_ENERGY(HOUR,R_TG),
     +                         TRANSACT_UNSERVED_COST,
     +                         SYSTEM_WHOLESALE_GENERATION(HOUR,R_TG),
     +                         LOCAL_PRICE(HOUR))
                  ENDIF
!
                  SYSTEM_NATIVE_COST(HOUR,R_TG) =
     +                           SYSTEM_NATIVE_COST(HOUR,R_TG) *
     +                                        PERCENT_OF_COST_2_ALLOCATE
! 111407. FOR JONES.
!
                  TEMP_I2 = GET_TRANS_GROUP_INDEX(R_TG)
                  YES_PUT_AC_HOURLY_COST =
     +                PUT_AC_HOURLY_COST_AT_MARKET(
     +                                    TEMP_I2,
     +                                    HOURLY_MARKET_PRICE,
     +                                    SYSTEM_NATIVE_COST(HOUR,R_TG),
     +                                    HOUR,
     +                                    R_MONTH)
               ENDIF
!
               WEEKLY_HYDRO_FOR_HR = GET_WH_LOADS_PER_HOUR(HOUR,TG)
!
! 02/26/04. PER MARK TO CLEAR THE TRANSACTIONS
!
               CALL RW_PROCESS_MESSAGES()
!
! 022707. HOURLY POSITION REPORT FOR TVA
!
               PROD_PROD_REPORT_ACTIVE = TRANSACT_PROD_REPORT()
               PROD_MW_REPORT_ACTIVE = TRANSACT_MW_REPORT()
!
               IF(PROD_PROD_REPORT_ACTIVE) THEN
                  IF(PEAK_HOUR == 1) THEN
                     K = 2
                  ELSE
                     K = 3
                  ENDIF
                  DO U = 1, NO_START_UP_UNITS
                     S_U = U
                     OPTION_INDEX = ORIGINAL_VALUE_OF_U(S_U)
                     GET_PRIMARY_MOVER_INDEX_called_from="tf_objt2:0003"
                     PM = GET_PRIMARY_MOVER_INDEX(OPTION_INDEX)
                     FT = MIN(GET_PRIMARY_MOVER(OPTION_INDEX),
     +                                              MAX_PROD_FUEL_TYPES)
                     IF(PM > 5) PM = PM + 1

                     ENRG = DAILY_GEN_BY_S_U(S_U,HR)
      call write_energy_trace("DAILY_OPTION_FOR_MONTH","tf_objt2:0002", 
     + ENRG)
                     TOTAL_HEAT = DAILY_FUEL_BY_S_U(S_U,HR)
!
                     CALL RETURN_PRIM_EMISSION_RATES(
     +                                         OPTION_INDEX,
     +                                         SOX,NOX1,NOX2,
     +                                         CO2,OTH2,OTH3)
                     SYSTEM_EMISSIONS(HOUR,TG,1) =
     +                              SYSTEM_EMISSIONS(HOUR,TG,1) +
     +                                    TOTAL_HEAT * SOX * 0.0000005
                     SYSTEM_EMISSIONS(HOUR,TG,2) =
     +                              SYSTEM_EMISSIONS(HOUR,TG,2) +
     +                                    TOTAL_HEAT * NOX1 * 0.0000005
                     SYSTEM_EMISSIONS(HOUR,TG,3) =
     +                              SYSTEM_EMISSIONS(HOUR,TG,3) +
     +                                    TOTAL_HEAT * CO2 * 0.0000005
                     SYSTEM_EMISSIONS(HOUR,TG,4) =
     +                              SYSTEM_EMISSIONS(HOUR,TG,4) +
     +                                    TOTAL_HEAT * OTH2 * 0.0000005
                     SYSTEM_EMISSIONS(HOUR,TG,5) =
     +                              SYSTEM_EMISSIONS(HOUR,TG,5) +
     +                                    TOTAL_HEAT * OTH3 * 0.0000005
! ALL HOURS
                     SYSTEM_PROD_BY_TG_BY_MWH(1,TG,PM) =
     +                  SYSTEM_PROD_BY_TG_BY_MWH(1,TG,PM) +
     +                     ENRG
!
                     SYSTEM_PROD_BY_TG_BY_FUEL(1,TG,PM) =
     +                  SYSTEM_PROD_BY_TG_BY_FUEL(1,TG,PM) +
     +                     TOTAL_HEAT
       ! TODO: Resolve...
       ! Comment said this was steam (as did Greg) but for a different
       ! context. pm_steam_ppr and pm_battery are both 11. That's a
       ! problem.  -jtr 
                     IF(PM == pm_steam_ppr) THEN ! STEAM for PPR
                        LM = MIN(PM + FT + INT(4,2),MAX_PROD_TYPES)
                        SYSTEM_PROD_BY_TG_BY_MWH(1,TG,LM) =
     +                     SYSTEM_PROD_BY_TG_BY_MWH(1,TG,LM) + ENRG
                        SYSTEM_PROD_BY_TG_BY_FUEL(1,TG,LM) =
     +                     SYSTEM_PROD_BY_TG_BY_FUEL(1,TG,LM) +
     +                                                        TOTAL_HEAT
                     ENDIF
!   TOTAL
                     SYSTEM_PROD_BY_TG_BY_MWH(1,TG,15) =
     +                   SYSTEM_PROD_BY_TG_BY_MWH(1,TG,15) +
     +                     ENRG
                     SYSTEM_PROD_BY_TG_BY_FUEL(1,TG,15) =
     +                  SYSTEM_PROD_BY_TG_BY_FUEL(1,TG,15) +
     +                     TOTAL_HEAT
! ON-PEAK/OFF PEAK HOURS
                     SYSTEM_PROD_BY_TG_BY_MWH(K,TG,PM) =
     +                  SYSTEM_PROD_BY_TG_BY_MWH(K,TG,PM) + ENRG
                     SYSTEM_PROD_BY_TG_BY_FUEL(K,TG,PM) =
     +                  SYSTEM_PROD_BY_TG_BY_FUEL(K,TG,PM) + TOTAL_HEAT

                     IF(PM == pm_steam_ppr) THEN ! STEAM PPR
                        LM = MIN(PM + FT + int(4,2),MAX_PROD_TYPES)
                        SYSTEM_PROD_BY_TG_BY_MWH(K,TG,LM) =
     +                     SYSTEM_PROD_BY_TG_BY_MWH(K,TG,LM) + ENRG
                        SYSTEM_PROD_BY_TG_BY_FUEL(K,TG,LM) =
     +                     SYSTEM_PROD_BY_TG_BY_FUEL(K,TG,LM) +
     +                                                        TOTAL_HEAT
                     ENDIF
! TOTAL ON OR OFF PEAK
                     SYSTEM_PROD_BY_TG_BY_MWH(K,TG,15) =
     +                   SYSTEM_PROD_BY_TG_BY_MWH(K,TG,15) +
     +                     ENRG
                     SYSTEM_PROD_BY_TG_BY_FUEL(K,TG,15) =
     +                  SYSTEM_PROD_BY_TG_BY_FUEL(K,TG,15) + TOTAL_HEAT
!
                     PRODUCT_GEN_BY_S_U(S_U,1,TG) =
     +                               PRODUCT_GEN_BY_S_U(S_U,1,TG) + ENRG
                     PRODUCT_GEN_BY_S_U(S_U,K,TG) =
     +                               PRODUCT_GEN_BY_S_U(S_U,K,TG) + ENRG
                     PRODUCT_FUEL_BY_S_U(S_U,1,TG) =
     +                        PRODUCT_FUEL_BY_S_U(S_U,1,TG) + TOTAL_HEAT
                     PRODUCT_FUEL_BY_S_U(S_U,K,TG) =
     +                        PRODUCT_FUEL_BY_S_U(S_U,K,TG) + TOTAL_HEAT
                  ENDDO ! UNITS LOOP

                  hourly_hydro_loc =
     +               GET_TRANS_HOURLY_HYDRO(HOUR,R_TG) +
     +                     WEEKLY_HYDRO_FOR_HR -
     +                     (SYSTEM_HOURLY_STORAGE_GEN(HOUR,R_TG) +
     +                           SYSTEM_HOURLY_STORAGE_PUMP(HOUR,R_TG))
                  IF(ABS(hourly_hydro_loc) < .7) THEN
                     hourly_hydro_loc = 0.0
                  ENDIF
!
                  SYSTEM_PROD_BY_TG_BY_MWH(1,TG,8) =
     +                  SYSTEM_PROD_BY_TG_BY_MWH(1,TG,8) +
     +                                 SYSTEM_DERIVATIVES(HOUR,R_TG)
                  SYSTEM_PROD_BY_TG_BY_MWH(K,TG,8) =
     +                  SYSTEM_PROD_BY_TG_BY_MWH(K,TG,8) +
     +                                 SYSTEM_DERIVATIVES(HOUR,R_TG)
                  SYSTEM_PROD_BY_TG_BY_MWH(1,TG,15) =
     +                  SYSTEM_PROD_BY_TG_BY_MWH(1,TG,15) +
     +                                 SYSTEM_DERIVATIVES(HOUR,R_TG)
                  SYSTEM_PROD_BY_TG_BY_MWH(K,TG,15) =
     +                  SYSTEM_PROD_BY_TG_BY_MWH(K,TG,15) +
     +                                 SYSTEM_DERIVATIVES(HOUR,R_TG)
!
                  SYSTEM_PROD_BY_TG_BY_MWH(1,TG,5) =
     +                  SYSTEM_PROD_BY_TG_BY_MWH(1,TG,5) +
     +                     hourly_hydro_loc
                  SYSTEM_PROD_BY_TG_BY_MWH(1,TG,15) =
     +                   SYSTEM_PROD_BY_TG_BY_MWH(1,TG,15) +
     +                     hourly_hydro_loc +
     +                     (SYSTEM_HOURLY_STORAGE_GEN(HOUR,R_TG)+
     +                     SYSTEM_HOURLY_STORAGE_PUMP(HOUR,R_TG))
                  SYSTEM_PROD_BY_TG_BY_MWH(K,TG,5) =
     +                  SYSTEM_PROD_BY_TG_BY_MWH(K,TG,5) +
     +                     hourly_hydro_loc
                  SYSTEM_PROD_BY_TG_BY_MWH(K,TG,15) =
     +                   SYSTEM_PROD_BY_TG_BY_MWH(K,TG,15) +
     +                     hourly_hydro_loc +
     +                     (SYSTEM_HOURLY_STORAGE_GEN(HOUR,R_TG)+
     +                     SYSTEM_HOURLY_STORAGE_PUMP(HOUR,R_TG))
! 060807. PER JONES.
                  SYSTEM_PROD_BY_TG_BY_MWH(1,TG,6) =
     +                  SYSTEM_PROD_BY_TG_BY_MWH(1,TG,6) +
     +                     (SYSTEM_HOURLY_STORAGE_GEN(HOUR,R_TG)+
     +                     SYSTEM_HOURLY_STORAGE_PUMP(HOUR,R_TG))
                  SYSTEM_PROD_BY_TG_BY_MWH(K,TG,6) =
     +                  SYSTEM_PROD_BY_TG_BY_MWH(K,TG,6) +
     +                     (SYSTEM_HOURLY_STORAGE_GEN(HOUR,R_TG)+
     +                     SYSTEM_HOURLY_STORAGE_PUMP(HOUR,R_TG))
                  SYSTEM_PROD_BY_TG_BY_MWH(1,TG,19) =
     +                  SYSTEM_PROD_BY_TG_BY_MWH(1,TG,19) +
     +                     SYSTEM_HOURLY_STORAGE_GEN(HOUR,R_TG)
                  SYSTEM_PROD_BY_TG_BY_MWH(1,TG,20) =
     +                   SYSTEM_PROD_BY_TG_BY_MWH(1,TG,20) +
     +                     SYSTEM_HOURLY_STORAGE_PUMP(HOUR,R_TG)
                  SYSTEM_PROD_BY_TG_BY_MWH(K,TG,19) =
     +                  SYSTEM_PROD_BY_TG_BY_MWH(K,TG,19) +
     +                     SYSTEM_HOURLY_STORAGE_GEN(HOUR,R_TG)
                  SYSTEM_PROD_BY_TG_BY_MWH(K,TG,20) =
     +                   SYSTEM_PROD_BY_TG_BY_MWH(K,TG,20) +
     +                     SYSTEM_HOURLY_STORAGE_PUMP(HOUR,R_TG)
                  SYSTEM_PROD_BY_TG_BY_MWH(1,TG,21) =
     +                   SYSTEM_PROD_BY_TG_BY_MWH(1,TG,21) +
     +                       DAILY_PRODUCTS_CAPACITY(HR)
                  SYSTEM_PROD_BY_TG_BY_MWH(K,TG,21) =
     +                   SYSTEM_PROD_BY_TG_BY_MWH(K,TG,21) +
     +                       DAILY_PRODUCTS_CAPACITY(HR)
                  SYSTEM_PROD_BY_TG_BY_MWH(1,TG,22) =
     +                   SYSTEM_PROD_BY_TG_BY_MWH(1,TG,22) +
     +                 GET_ANNUAL_CALL_PUT_CAPACITY(HOUR,R_MONTH,R_TG)
                  SYSTEM_PROD_BY_TG_BY_MWH(K,TG,22) =
     +                   SYSTEM_PROD_BY_TG_BY_MWH(K,TG,22) +
     +                 GET_ANNUAL_CALL_PUT_CAPACITY(HOUR,R_MONTH,R_TG)
                  SYSTEM_PROD_BY_TG_BY_MWH(1,TG,23) =
     +                   SYSTEM_PROD_BY_TG_BY_MWH(1,TG,23) -
     +                                        HOURLY_FORWARD_SALE
                  SYSTEM_PROD_BY_TG_BY_MWH(K,TG,23) =
     +                   SYSTEM_PROD_BY_TG_BY_MWH(K,TG,23) -
     +                                        HOURLY_FORWARD_SALE
                  SYSTEM_PROD_BY_TG_BY_MWH(1,TG,24) =
     +                   SYSTEM_PROD_BY_TG_BY_MWH(1,TG,24) +
     +                                              HOURLY_INTERRUPTIBLE
                  SYSTEM_PROD_BY_TG_BY_MWH(K,TG,24) =
     +                   SYSTEM_PROD_BY_TG_BY_MWH(K,TG,24) +
     +                                              HOURLY_INTERRUPTIBLE
!
                ENDIF
!
               IF(HR == 24) THEN
                  IF(YES_HOURLY_COMMITMENT .OR.
     +                                     HOURLY_UNIT_FUEL_REPORT) THEN
!
                     DO U = 1, NO_START_UP_UNITS
!
!                  S_U = INCREASING_VALUE_OF_U(U) ADDED FOR LGE TOOK OUT
                        S_U = U
                        OPTION_INDEX = ORIGINAL_VALUE_OF_U(S_U)
                        FT = GET_PRIMARY_MOVER(OPTION_INDEX)
!
                        IF(HOURLY_UNIT_FUEL_REPORT) THEN
                           IF(XX_REPORT_NOT_OPEN) THEN
                              XX_REPORT_NOT_OPEN = .FALSE.
                              XX_REPORT_VARIABLES = 24
                              XX_HOURLY_NO =
     +                          XX_HOURLY_HEADER(XX_REPORT_VARIABLES,
     +                                           XX_HOURLY_REC)
                           ENDIF
                           IF(REPORT_THIS_CL_UNIT(OPTION_INDEX)) THEN
                              XX_HOURLY_REC = rptrec(XX_HOURLY_NO)
                              WRITE(XX_HOURLY_NO,REC=XX_HOURLY_REC)
     +                           PRT_ENDPOINT(),
     +                           THIS_YEAR,
     +                           R_MONTH_NAME,
     +                           FLOAT(DAY),
     +                           OPTION_NAME(S_U),
     +                           (DAILY_FUEL_BY_S_U(S_U,J),J=1,24)
                              XX_HOURLY_REC = XX_HOURLY_REC + 1
                           ENDIF
                           M = NO_START_UP_UNITS + FT
                           DO J = 1, 24
                              DAILY_FUEL_BY_S_U(M,J) =
     +                              DAILY_FUEL_BY_S_U(M,J) +
     +                                        DAILY_FUEL_BY_S_U(S_U,J)
                           END DO
                        ENDIF ! HOURLY HEAT
!
                        IF(YES_HOURLY_COMMITMENT) THEN
                           IF(HX_REPORT_NOT_OPEN) THEN
                              HX_REPORT_NOT_OPEN = .FALSE.
                              HX_REPORT_VARIABLES = 24
                              HX_HOURLY_NO =
     +                          HX_HOURLY_HEADER(HX_REPORT_VARIABLES,
     +                                           HX_HOURLY_REC)
                              KX_HOURLY_NO =
     +                          KX_HOURLY_HEADER(HX_REPORT_VARIABLES,
     +                                           KX_HOURLY_REC)
!                           QX_HOURLY_NO =
!     +                          QX_HOURLY_HEADER(HX_REPORT_VARIABLES,
!     +                                           QX_HOURLY_REC)
                              C_HOURLY_MWH_COST_UNIT =
     +                          C_HOURLY_MWH_COST_HEADER(
     +                                           HX_REPORT_VARIABLES,
     +                                           C_HOURLY_MWH_COST_REC)
                              C_HOURLY_COST_UNIT =
     +                          C_HOURLY_COST_HEADER(
     +                                           HX_REPORT_VARIABLES,
     +                                           C_HOURLY_COST_REC)
                              C_HOURLY_SUMMARY_UNIT =
     +                          C_HOURLY_SUMMARY_HEADER(
     +                                           HX_REPORT_VARIABLES,
     +                                           C_HOURLY_SUMMARY_REC)
                              C_TG_HOURLY_SUMMARY_UNIT =
     +                          C_TG_HOURLY_SUMMARY_HEADER(
     +                                          HX_REPORT_VARIABLES,
     +                                          C_TG_HOURLY_SUMMARY_REC)
                           ENDIF
! 01/05/04.
                           IF(YES_HOURLY_SPIN_REPORT) THEN
!
                              IF(WX_REPORT_NOT_OPEN) THEN
                                 WX_REPORT_NOT_OPEN = .FALSE.
                                 WX_REPORT_VARIABLES = 24
                                 WX_HOURLY_NO =
     +                                 WX_HOURLY_HEADER(
     +                                           WX_REPORT_VARIABLES,
     +                                           WX_HOURLY_REC)
                              ENDIF
!
                              WX_HOURLY_REC = rptrec(WX_HOURLY_NO)
                              WRITE(WX_HOURLY_NO,REC=WX_HOURLY_REC)
     +                           PRT_ENDPOINT(),
     +                           THIS_YEAR,
     +                           R_MONTH_NAME,
     +                           FLOAT(DAY),
     +                           OPTION_NAME(S_U),
     +                           (DAILY_SPIN_BY_S_U(S_U,J),J=1,24)
                              WX_HOURLY_REC = WX_HOURLY_REC + 1
                           ENDIF
!
                           IF(REPORT_THIS_CL_UNIT(OPTION_INDEX)) THEN
                              HX_HOURLY_REC = rptrec(HX_HOURLY_NO)
                              WRITE(HX_HOURLY_NO,REC=HX_HOURLY_REC)
     +                           PRT_ENDPOINT(),
     +                           THIS_YEAR,
     +                           R_MONTH_NAME,
     +                           FLOAT(DAY),
     +                           OPTION_NAME(S_U),
     +                           (DAILY_GEN_BY_S_U(S_U,J),J=1,24)
                              HX_HOURLY_REC = HX_HOURLY_REC + 1
!
                              WRITE(TEMP_CHAR_12,*)
     +                             GET_HESI_UNIT_ID_NUM_I4(OPTION_INDEX)
!
                              KX_HOURLY_REC = rptrec(KX_HOURLY_NO)
                              WRITE(KX_HOURLY_NO,REC=KX_HOURLY_REC)
     +                           PRT_ENDPOINT(),
     +                           THIS_YEAR,
     +                           R_MONTH_NAME,
     +                           FLOAT(DAY),
     +                           TEMP_CHAR_12,
     +                           (DAILY_GEN_BY_S_U(S_U,J),J=1,24)
                              KX_HOURLY_REC = KX_HOURLY_REC + 1

                      C_HOURLY_COST_REC= rptrec(C_HOURLY_COST_UNIT)
                              WRITE(C_HOURLY_COST_UNIT,
     +                                            REC=C_HOURLY_COST_REC)
     +                           PRT_ENDPOINT(),
     +                           THIS_YEAR,
     +                           R_MONTH_NAME,
     +                           FLOAT(DAY),
     +                           OPTION_NAME(S_U),
     +                           (DAILY_COST_BY_S_U(S_U,J),J=1,24)
                              C_HOURLY_COST_REC = C_HOURLY_COST_REC + 1
                           ENDIF ! HOURLY GEN
                        ENDIF ! HOURLY GEN OR HEAT
                     ENDDO ! COMMITMENT UNITS
!
! 12/08/03.
!
                     IF(YES_HOURLY_COMMITMENT .AND.
     +                                         MULTI_MARKET_ACTIVE) THEN
                        DO U = 1, NUM_OF_PRICING_GROUPS
! REPORT THIS MARKET???
                              HX_HOURLY_REC = rptrec(HX_HOURLY_NO)
                              WRITE(HX_HOURLY_NO,REC=HX_HOURLY_REC)
     +                           PRT_ENDPOINT(),
     +                           THIS_YEAR,
     +                           R_MONTH_NAME,
     +                           FLOAT(DAY),
     +                           PRICING_GROUP_SELL_NAME(U),
     +                           (PRICING_GROUP_SELL_GEN(U,J),J=1,24)
                              HX_HOURLY_REC = HX_HOURLY_REC + 1

                        ENDDO
                        DO U = 1, NUM_OF_PRICING_GROUPS
                              TEMP_NAME =
     +                           PRICING_GROUP_SELL_NAME(U)(1:17)//' AV'
!
                              HX_HOURLY_REC = rptrec(HX_HOURLY_NO)
                              WRITE(HX_HOURLY_NO,REC=HX_HOURLY_REC)
     +                           PRT_ENDPOINT(),
     +                           THIS_YEAR,
     +                           R_MONTH_NAME,
     +                           FLOAT(DAY),
     +                           TEMP_NAME,
     +                           (PRICING_GROUP_SELL_QUANT_AVAIL(U,J),
     +                                                           J=1,24)
                              HX_HOURLY_REC = HX_HOURLY_REC + 1
                        ENDDO
! 01/22/04.
                        DO U = 1, ALL_DEMAND_NUMBER
!
                              IF(U == 1) THEN
                                 TEMP_NAME = ALL_DEMAND_NAME(U)
                              ELSE
                                 TEMP_NAME = PRICING_GROUP_BUY_NAME(U-1)
                              ENDIF
!
                              HX_HOURLY_REC = rptrec(HX_HOURLY_NO)
                              WRITE(HX_HOURLY_NO,REC=HX_HOURLY_REC)
     +                           PRT_ENDPOINT(),
     +                           THIS_YEAR,
     +                           R_MONTH_NAME,
     +                           FLOAT(DAY),
     +                           TEMP_NAME,
     +                           (ALL_DEMAND_BUY_GEN(U,J),J=1,24)
                              HX_HOURLY_REC = HX_HOURLY_REC + 1
                        ENDDO
                        DO U = 1, NUM_OF_BUY_PRICING_GROUPS
                              TEMP_NAME =
     +                           PRICING_GROUP_BUY_NAME(U)(1:17)//' AV'
!
                              HX_HOURLY_REC = rptrec(HX_HOURLY_NO)
                              WRITE(HX_HOURLY_NO,REC=HX_HOURLY_REC)
     +                           PRT_ENDPOINT(),
     +                           THIS_YEAR,
     +                           R_MONTH_NAME,
     +                           FLOAT(DAY),
     +                           TEMP_NAME,
     +                           (PRICING_GROUP_BUY_QUANT_AVAIL(U,J),
     +                                                           J=1,24)
                              HX_HOURLY_REC = HX_HOURLY_REC + 1
                        ENDDO
                     ENDIF ! MULTI_MARKET_ACTIVE
! SUMMARY VARIABLES
!                     WRITE(HX_HOURLY_NO,REC=HX_HOURLY_REC)
!     +                        PRT_ENDPOINT(),
!     +                        THIS_YEAR,
!     +                        R_MONTH_NAME,
!     +                        FLOAT(DAY),
!     +                        'Total Resources     ',
!     +                        (DAILY_GEN_BY_S_U(0,J),J=1,24)
!                     HX_HOURLY_REC = HX_HOURLY_REC + 1
!
                     IF(HOURLY_UNIT_FUEL_REPORT) THEN
!                        TEMP_I = NO_START_UP_UNITS + max_fuel_types_tfo
                        DO S_U = 1, max_fuel_types_tfo
                           M = NO_START_UP_UNITS + S_U
                           XX_HOURLY_REC = rptrec(XX_HOURLY_NO)
                           WRITE(XX_HOURLY_NO,REC=XX_HOURLY_REC)
     +                           PRT_ENDPOINT(),
     +                           THIS_YEAR,
     +                           R_MONTH_NAME,
     +                           FLOAT(DAY),
     +                           FUEL_NAME(S_U),
     +                           (DAILY_FUEL_BY_S_U(M,J),J=1,24)
                           XX_HOURLY_REC = XX_HOURLY_REC + 1
                        ENDDO
                     ENDIF
                     IF(YES_HOURLY_COMMITMENT) THEN
                     C_HOURLY_COST_REC= rptrec(C_HOURLY_COST_UNIT)
                        WRITE(C_HOURLY_COST_UNIT,REC=C_HOURLY_COST_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        FLOAT(DAY),
     +                        'Total Resources     ',
     +                        (DAILY_COST_BY_S_U(0,J),J=1,24)
                        C_HOURLY_COST_REC = C_HOURLY_COST_REC + 1

                        IF(ECITIES) THEN
                           HX_HOURLY_REC = rptrec(HX_HOURLY_NO)
                           WRITE(HX_HOURLY_NO,REC=HX_HOURLY_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        FLOAT(DAY),
     +                        'Reserve Capacity    ',
     +                        (DAILY_RESERVE_ENERGY(J),J=1,24)
                           HX_HOURLY_REC = HX_HOURLY_REC + 1
                        ENDIF

! SUMMARY VARIABLES IN SUMMARY REPORT
!
!
                        C_HOURLY_SUMMARY_REC =
     +                                rptrec(C_HOURLY_SUMMARY_UNIT)
                        WRITE(C_HOURLY_SUMMARY_UNIT,
     +                                         REC=C_HOURLY_SUMMARY_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        FLOAT(DAY),
     +                        'Resource Generation ',
     +                        (DAILY_GEN_BY_S_U(0,J),J=1,24)
                        C_HOURLY_SUMMARY_REC = C_HOURLY_SUMMARY_REC + 1
!
                        C_HOURLY_SUMMARY_REC =
     +                               rptrec(C_HOURLY_SUMMARY_UNIT)
                        WRITE(C_HOURLY_SUMMARY_UNIT,
     +                                         REC=C_HOURLY_SUMMARY_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        FLOAT(DAY),
     +                        'Available Committed ',
     +                        (SYSTEM_HOURLY_COMMIT_AVAIL(J,R_TG),
     +                                                   J=HOUR-23,HOUR)
                        C_HOURLY_SUMMARY_REC = C_HOURLY_SUMMARY_REC + 1
!
                        C_HOURLY_SUMMARY_REC =
     +                                rptrec(C_HOURLY_SUMMARY_UNIT)
                        WRITE(C_HOURLY_SUMMARY_UNIT,
     +                                         REC=C_HOURLY_SUMMARY_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        FLOAT(DAY),
     +                        'Emergency Resources ',
     +                        (SYSTEM_HOURLY_PRE_COMMITMENT(J,R_TG),
     +                                                   J=HOUR-23,HOUR)
                        C_HOURLY_SUMMARY_REC = C_HOURLY_SUMMARY_REC + 1
!
                        C_HOURLY_SUMMARY_REC =
     +                                rptrec(C_HOURLY_SUMMARY_UNIT)
                        WRITE(C_HOURLY_SUMMARY_UNIT,
     +                                         REC=C_HOURLY_SUMMARY_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        FLOAT(DAY),
     +                        'Available Resources ',
     +                        (SYSTEM_HOURLY_AVAILABLE(J,R_TG),
     +                                                   J=HOUR-23,HOUR)
                        C_HOURLY_SUMMARY_REC = C_HOURLY_SUMMARY_REC + 1
!
                        C_HOURLY_SUMMARY_REC =
     +                                rptrec(C_HOURLY_SUMMARY_UNIT)
                        WRITE(C_HOURLY_SUMMARY_UNIT,
     +                                         REC=C_HOURLY_SUMMARY_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        FLOAT(DAY),
     +                        'Hourly Derivatives  ',
     +                        (SYSTEM_DERIVATIVES(J,R_TG),
     +                                                   J=HOUR-23,HOUR)
                        C_HOURLY_SUMMARY_REC = C_HOURLY_SUMMARY_REC + 1
!
                        C_HOURLY_SUMMARY_REC =
     +                                rptrec(C_HOURLY_SUMMARY_UNIT)
                        WRITE(C_HOURLY_SUMMARY_UNIT,
     +                                         REC=C_HOURLY_SUMMARY_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        FLOAT(DAY),
     +                        'Load Following      ',
     +                        (HOURLY_LOAD_FOLLOW(J),J=1,24)
                        C_HOURLY_SUMMARY_REC = C_HOURLY_SUMMARY_REC + 1
!
                        C_HOURLY_SUMMARY_REC =
     +                                rptrec(C_HOURLY_SUMMARY_UNIT)
                        WRITE(C_HOURLY_SUMMARY_UNIT,
     +                                         REC=C_HOURLY_SUMMARY_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        FLOAT(DAY),
     +                        'DSM Resources       ',
     +                        (DAILY_DSM_MW(J),J=1,24)
                        C_HOURLY_SUMMARY_REC = C_HOURLY_SUMMARY_REC + 1
!
                        IF(ECITIES) THEN
                           C_HOURLY_SUMMARY_REC =
     +                                rptrec(C_HOURLY_SUMMARY_UNIT)
                           WRITE(C_HOURLY_SUMMARY_UNIT,
     +                                         REC=C_HOURLY_SUMMARY_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        FLOAT(DAY),
     +                        'Reserve Capacity    ',
     +                        (DAILY_RESERVE_ENERGY(J),J=1,24)
                           C_HOURLY_SUMMARY_REC =
     +                                          C_HOURLY_SUMMARY_REC + 1
                        ENDIF
!
                     C_HOURLY_SUMMARY_REC =
     +                                rptrec(C_HOURLY_SUMMARY_UNIT)
                     WRITE(C_HOURLY_SUMMARY_UNIT,
     +                                         REC=C_HOURLY_SUMMARY_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        FLOAT(DAY),
     +                        'Retail Sales        ',
     +                        (SYSTEM_HOURLY_LOAD(J,R_TG),
     +                                                   J=HOUR-23,HOUR)
                        C_HOURLY_SUMMARY_REC = C_HOURLY_SUMMARY_REC + 1
!
                        IF(ECITIES) THEN
                           C_HOURLY_SUMMARY_REC =
     +                                rptrec(C_HOURLY_SUMMARY_UNIT)
                           WRITE(C_HOURLY_SUMMARY_UNIT,
     +                                         REC=C_HOURLY_SUMMARY_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        FLOAT(DAY),
     +                        'Hourly AC Load      ',
     +                        (HOURLY_AC_LOAD(J),J=1,24)
                           C_HOURLY_SUMMARY_REC =
     +                                          C_HOURLY_SUMMARY_REC + 1
                        ENDIF
!
                        END_EFFECTS_TEST = .TRUE.
!
                        IF(END_EFFECTS_TEST) THEN
                           C_HOURLY_SUMMARY_REC =
     +                                rptrec(C_HOURLY_SUMMARY_UNIT)
                           WRITE(C_HOURLY_SUMMARY_UNIT,
     +                                         REC=C_HOURLY_SUMMARY_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        FLOAT(DAY),
     +                        'Buy Wheel Rate      ',
     +                        DAILY_BUY_WHEEL
                           C_HOURLY_SUMMARY_REC =
     +                                          C_HOURLY_SUMMARY_REC + 1
                           C_HOURLY_SUMMARY_REC =
     +                                rptrec(C_HOURLY_SUMMARY_UNIT)
                           WRITE(C_HOURLY_SUMMARY_UNIT,
     +                                         REC=C_HOURLY_SUMMARY_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        FLOAT(DAY),
     +                        'Sell Wheel Rate     ',
     +                        DAILY_SELL_WHEEL
                           C_HOURLY_SUMMARY_REC =
     +                                          C_HOURLY_SUMMARY_REC + 1
                        ENDIF

                        C_HOURLY_SUMMARY_REC =
     +                                rptrec(C_HOURLY_SUMMARY_UNIT)
                        WRITE(C_HOURLY_SUMMARY_UNIT,
     +                                         REC=C_HOURLY_SUMMARY_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        FLOAT(DAY),
     +                        'Wholesale Sales     ',
     +                        (SYSTEM_WHOLESALE_GENERATION(J,R_TG),
     +                                                   J=HOUR-23,HOUR)
                        C_HOURLY_SUMMARY_REC = C_HOURLY_SUMMARY_REC + 1
!
                        C_HOURLY_SUMMARY_REC =
     +                                rptrec(C_HOURLY_SUMMARY_UNIT)
                        WRITE(C_HOURLY_SUMMARY_UNIT,
     +                                         REC=C_HOURLY_SUMMARY_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        FLOAT(DAY),
     +                        'Wholesale Purchases ',
     +                        (SYSTEM_MARKET_BOUGHT(J,R_TG),
     +                                                   J=HOUR-23,HOUR)
                        C_HOURLY_SUMMARY_REC = C_HOURLY_SUMMARY_REC + 1
!
                        C_HOURLY_SUMMARY_REC =
     +                                rptrec(C_HOURLY_SUMMARY_UNIT)
                        WRITE(C_HOURLY_SUMMARY_UNIT,
     +                                         REC=C_HOURLY_SUMMARY_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        FLOAT(DAY),
     +                        'Energy Above Resourc',
     +                        (SYSTEM_ENERGY_ABOVE(J,R_TG),
     +                                                  J=HOUR-23,HOUR)
                        C_HOURLY_SUMMARY_REC = C_HOURLY_SUMMARY_REC + 1
!
                        C_HOURLY_SUMMARY_REC =
     +                                rptrec(C_HOURLY_SUMMARY_UNIT)
                        WRITE(C_HOURLY_SUMMARY_UNIT,
     +                                         REC=C_HOURLY_SUMMARY_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        FLOAT(DAY),
     +                        'Hydro and Pump Stora',
     +                        (SYSTEM_HOURLY_HYDRO(J,R_TG),
     +                                                   J=HOUR-23,HOUR)
                        C_HOURLY_SUMMARY_REC = C_HOURLY_SUMMARY_REC + 1
!
                        C_HOURLY_SUMMARY_REC =
     +                                rptrec(C_HOURLY_SUMMARY_UNIT)
                        WRITE(C_HOURLY_SUMMARY_UNIT,
     +                                         REC=C_HOURLY_SUMMARY_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        FLOAT(DAY),
     +                        'Storage Pump Demand ',
     +                        (SYSTEM_HOURLY_STORAGE_PUMP(J,R_TG),
     +                                                   J=HOUR-23,HOUR)
                        C_HOURLY_SUMMARY_REC = C_HOURLY_SUMMARY_REC + 1
!
                        C_HOURLY_SUMMARY_REC =
     +                                rptrec(C_HOURLY_SUMMARY_UNIT)
                        WRITE(C_HOURLY_SUMMARY_UNIT,
     +                                         REC=C_HOURLY_SUMMARY_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        FLOAT(DAY),
     +                        'Storage Generation  ',
     +                        (SYSTEM_HOURLY_STORAGE_GEN(J,R_TG),
     +                                                   J=HOUR-23,HOUR)
                        C_HOURLY_SUMMARY_REC = C_HOURLY_SUMMARY_REC + 1
!
                        C_HOURLY_SUMMARY_REC =
     +                                rptrec(C_HOURLY_SUMMARY_UNIT)
                        WRITE(C_HOURLY_SUMMARY_UNIT,
     +                                         REC=C_HOURLY_SUMMARY_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        FLOAT(DAY),
     +                        'Dynamic Storage     ',
     +                        (SYSTEM_STORAGE(J,R_TG),
     +                                                J=HOUR-23,HOUR)
                        C_HOURLY_SUMMARY_REC = C_HOURLY_SUMMARY_REC + 1
!
                        C_HOURLY_SUMMARY_REC =
     +                                rptrec(C_HOURLY_SUMMARY_UNIT)
                        WRITE(C_HOURLY_SUMMARY_UNIT,
     +                                         REC=C_HOURLY_SUMMARY_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        FLOAT(DAY),
     +                        'Unserved Energy     ',
     +                        (SYSTEM_UNSERVED_ENERGY(J,R_TG),
     +                                                   J=HOUR-23,HOUR)
                        C_HOURLY_SUMMARY_REC = C_HOURLY_SUMMARY_REC + 1
!
                        C_HOURLY_SUMMARY_REC =
     +                                rptrec(C_HOURLY_SUMMARY_UNIT)
                        WRITE(C_HOURLY_SUMMARY_UNIT,
     +                                         REC=C_HOURLY_SUMMARY_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        FLOAT(DAY),
     +                        'Spinning Reserves   ',
     +                        (SYSTEM_SPIN(J,R_TG),J=HOUR-23,HOUR)
                        C_HOURLY_SUMMARY_REC = C_HOURLY_SUMMARY_REC + 1
!
                        C_HOURLY_SUMMARY_REC =
     +                                rptrec(C_HOURLY_SUMMARY_UNIT)
                        WRITE(C_HOURLY_SUMMARY_UNIT,
     +                                         REC=C_HOURLY_SUMMARY_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        FLOAT(DAY),
     +                        'Must Run Capacity   ',
     +                        (DAILY_MUST_RUN_CAPACITY(J),J=1,24)
                        C_HOURLY_SUMMARY_REC = C_HOURLY_SUMMARY_REC + 1
!
                        C_HOURLY_SUMMARY_REC =
     +                                rptrec(C_HOURLY_SUMMARY_UNIT)
                        WRITE(C_HOURLY_SUMMARY_UNIT,
     +                                         REC=C_HOURLY_SUMMARY_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        FLOAT(DAY),
     +                        'Dump Energy         ',
     +                        (DUMP_ENERGY(J),J=1,24)
                        C_HOURLY_SUMMARY_REC = C_HOURLY_SUMMARY_REC + 1
!
                        C_HOURLY_SUMMARY_REC =
     +                                rptrec(C_HOURLY_SUMMARY_UNIT)
                        WRITE(C_HOURLY_SUMMARY_UNIT,
     +                                         REC=C_HOURLY_SUMMARY_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        FLOAT(DAY),
     +                        'Decommitted Capacity',
     +                        (SYSTEM_HOURLY_DECOMMITTED(J,R_TG),
     +                                                   J=HOUR-23,HOUR)
                        C_HOURLY_SUMMARY_REC = C_HOURLY_SUMMARY_REC + 1
!
                        C_HOURLY_SUMMARY_REC =
     +                                rptrec(C_HOURLY_SUMMARY_UNIT)
                        WRITE(C_HOURLY_SUMMARY_UNIT,
     +                                         REC=C_HOURLY_SUMMARY_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        FLOAT(DAY),
     +                        'SO2 Emissions       ',
     +                        (SYSTEM_EMISSIONS(J,R_TG,1),
     +                                                   J=HOUR-23,HOUR)
                        C_HOURLY_SUMMARY_REC = C_HOURLY_SUMMARY_REC + 1
!
                        C_HOURLY_SUMMARY_REC =
     +                                rptrec(C_HOURLY_SUMMARY_UNIT)
                        WRITE(C_HOURLY_SUMMARY_UNIT,
     +                                         REC=C_HOURLY_SUMMARY_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        FLOAT(DAY),
     +                        'NOx Emissions       ',
     +                        (SYSTEM_EMISSIONS(J,R_TG,2),
     +                                                   J=HOUR-23,HOUR)
                        C_HOURLY_SUMMARY_REC = C_HOURLY_SUMMARY_REC + 1
!
                        C_HOURLY_SUMMARY_REC =
     +                                rptrec(C_HOURLY_SUMMARY_UNIT)
                        WRITE(C_HOURLY_SUMMARY_UNIT,
     +                                         REC=C_HOURLY_SUMMARY_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        FLOAT(DAY),
     +                        'CO2 Emissions       ',
     +                        (SYSTEM_EMISSIONS(J,R_TG,3),
     +                                                   J=HOUR-23,HOUR)
                        C_HOURLY_SUMMARY_REC = C_HOURLY_SUMMARY_REC + 1
!
! 01/05/04
!
                        C_TG_HOURLY_SUMMARY_REC =
     +                             rptrec(C_TG_HOURLY_SUMMARY_UNIT)
                        WRITE(C_TG_HOURLY_SUMMARY_UNIT,
     +                                      REC=C_TG_HOURLY_SUMMARY_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        TRANS_GROUP_NAME,
     +                        FLOAT(DAY),
     +                        'Resource Generation ',
     +                        (DAILY_GEN_BY_S_U(0,J),J=1,24)
                        C_TG_HOURLY_SUMMARY_REC =
     +                                       C_TG_HOURLY_SUMMARY_REC + 1
!
                        C_TG_HOURLY_SUMMARY_REC =
     +                             rptrec(C_TG_HOURLY_SUMMARY_UNIT)
                        WRITE(C_TG_HOURLY_SUMMARY_UNIT,
     +                                      REC=C_TG_HOURLY_SUMMARY_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        TRANS_GROUP_NAME,
     +                        FLOAT(DAY),
     +                        'Available Committed ',
     +                        (SYSTEM_HOURLY_COMMIT_AVAIL(J,R_TG),
     +                                                   J=HOUR-23,HOUR)
                        C_TG_HOURLY_SUMMARY_REC =
     +                                       C_TG_HOURLY_SUMMARY_REC + 1
!
                        C_TG_HOURLY_SUMMARY_REC =
     +                             rptrec(C_TG_HOURLY_SUMMARY_UNIT)
                        WRITE(C_TG_HOURLY_SUMMARY_UNIT,
     +                                      REC=C_TG_HOURLY_SUMMARY_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        TRANS_GROUP_NAME,
     +                        FLOAT(DAY),
     +                        'Emergency Resources ',
     +                        (SYSTEM_HOURLY_PRE_COMMITMENT(J,R_TG),
     +                                                   J=HOUR-23,HOUR)
                        C_TG_HOURLY_SUMMARY_REC =
     +                                       C_TG_HOURLY_SUMMARY_REC + 1
!
                        C_TG_HOURLY_SUMMARY_REC =
     +                             rptrec(C_TG_HOURLY_SUMMARY_UNIT)
                        WRITE(C_TG_HOURLY_SUMMARY_UNIT,
     +                                      REC=C_TG_HOURLY_SUMMARY_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        TRANS_GROUP_NAME,
     +                        FLOAT(DAY),
     +                        'Available Resources ',
     +                        (SYSTEM_HOURLY_AVAILABLE(J,R_TG),
     +                                                   J=HOUR-23,HOUR)
                        C_TG_HOURLY_SUMMARY_REC =
     +                                       C_TG_HOURLY_SUMMARY_REC + 1
!
                        C_TG_HOURLY_SUMMARY_REC =
     +                             rptrec(C_TG_HOURLY_SUMMARY_UNIT)
                        WRITE(C_TG_HOURLY_SUMMARY_UNIT,
     +                                      REC=C_TG_HOURLY_SUMMARY_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        TRANS_GROUP_NAME,
     +                        FLOAT(DAY),
     +                        'Hourly Derivatives  ',
     +                        (SYSTEM_DERIVATIVES(J,R_TG),
     +                                                   J=HOUR-23,HOUR)
                        C_TG_HOURLY_SUMMARY_REC =
     +                                       C_TG_HOURLY_SUMMARY_REC + 1
!
                        C_TG_HOURLY_SUMMARY_REC =
     +                             rptrec(C_TG_HOURLY_SUMMARY_UNIT)
                        WRITE(C_TG_HOURLY_SUMMARY_UNIT,
     +                                      REC=C_TG_HOURLY_SUMMARY_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        TRANS_GROUP_NAME,
     +                        FLOAT(DAY),
     +                        'Hourly Load Follow  ',
     +                        (HOURLY_LOAD_FOLLOW(J),J=1,24)
                        C_TG_HOURLY_SUMMARY_REC =
     +                                       C_TG_HOURLY_SUMMARY_REC + 1
!
                        C_TG_HOURLY_SUMMARY_REC =
     +                             rptrec(C_TG_HOURLY_SUMMARY_UNIT)
                        WRITE(C_TG_HOURLY_SUMMARY_UNIT,
     +                                      REC=C_TG_HOURLY_SUMMARY_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        TRANS_GROUP_NAME,
     +                        FLOAT(DAY),
     +                        'DSM Resources       ',
     +                        (DAILY_DSM_MW(J),J=1,24)
                        C_TG_HOURLY_SUMMARY_REC =
     +                                       C_TG_HOURLY_SUMMARY_REC + 1
!
                        IF(ECITIES) THEN
                           C_TG_HOURLY_SUMMARY_REC =
     +                             rptrec(C_TG_HOURLY_SUMMARY_UNIT)
                           WRITE(C_TG_HOURLY_SUMMARY_UNIT,
     +                                      REC=C_TG_HOURLY_SUMMARY_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        TRANS_GROUP_NAME,
     +                        FLOAT(DAY),
     +                        'Reserve Capacity    ',
     +                        (DAILY_RESERVE_ENERGY(J),J=1,24)
                           C_TG_HOURLY_SUMMARY_REC =
     +                                       C_TG_HOURLY_SUMMARY_REC + 1
                        ENDIF
!
                        C_TG_HOURLY_SUMMARY_REC =
     +                             rptrec(C_TG_HOURLY_SUMMARY_UNIT)
                        WRITE(C_TG_HOURLY_SUMMARY_UNIT,
     +                                      REC=C_TG_HOURLY_SUMMARY_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        TRANS_GROUP_NAME,
     +                        FLOAT(DAY),
     +                        'Retail Sales        ',
     +                        (SYSTEM_HOURLY_LOAD(J,R_TG),
     +                                                   J=HOUR-23,HOUR)
                        C_TG_HOURLY_SUMMARY_REC =
     +                                       C_TG_HOURLY_SUMMARY_REC + 1
!
                        C_TG_HOURLY_SUMMARY_REC =
     +                             rptrec(C_TG_HOURLY_SUMMARY_UNIT)
                        WRITE(C_TG_HOURLY_SUMMARY_UNIT,
     +                                      REC=C_TG_HOURLY_SUMMARY_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        TRANS_GROUP_NAME,
     +                        FLOAT(DAY),
     +                        'Wholesale Sales     ',
     +                        (SYSTEM_WHOLESALE_GENERATION(J,R_TG),
     +                                                   J=HOUR-23,HOUR)
                        C_TG_HOURLY_SUMMARY_REC =
     +                                       C_TG_HOURLY_SUMMARY_REC + 1
!
                        C_TG_HOURLY_SUMMARY_REC =
     +                             rptrec(C_TG_HOURLY_SUMMARY_UNIT)
                        WRITE(C_TG_HOURLY_SUMMARY_UNIT,
     +                                      REC=C_TG_HOURLY_SUMMARY_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        TRANS_GROUP_NAME,
     +                        FLOAT(DAY),
     +                        'Wholesale Purchases ',
     +                        (SYSTEM_MARKET_BOUGHT(J,R_TG),
     +                                                   J=HOUR-23,HOUR)
                        C_TG_HOURLY_SUMMARY_REC =
     +                                       C_TG_HOURLY_SUMMARY_REC + 1
!
                        C_TG_HOURLY_SUMMARY_REC =
     +                             rptrec(C_TG_HOURLY_SUMMARY_UNIT)
                        WRITE(C_TG_HOURLY_SUMMARY_UNIT,
     +                                      REC=C_TG_HOURLY_SUMMARY_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        TRANS_GROUP_NAME,
     +                        FLOAT(DAY),
     +                        'Energy Above Resourc',
     +                        (SYSTEM_ENERGY_ABOVE(J,R_TG),
     +                                                  J=HOUR-23,HOUR)
                        C_TG_HOURLY_SUMMARY_REC =
     +                                       C_TG_HOURLY_SUMMARY_REC + 1
!
                        C_TG_HOURLY_SUMMARY_REC =
     +                             rptrec(C_TG_HOURLY_SUMMARY_UNIT)
                        WRITE(C_TG_HOURLY_SUMMARY_UNIT,
     +                                      REC=C_TG_HOURLY_SUMMARY_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        TRANS_GROUP_NAME,
     +                        FLOAT(DAY),
     +                        'Hydro and Pump Stora',
     +                        (SYSTEM_HOURLY_HYDRO(J,R_TG),
     +                                                   J=HOUR-23,HOUR)
                        C_TG_HOURLY_SUMMARY_REC =
     +                                       C_TG_HOURLY_SUMMARY_REC + 1
!
                        C_TG_HOURLY_SUMMARY_REC =
     +                             rptrec(C_TG_HOURLY_SUMMARY_UNIT)
                        WRITE(C_TG_HOURLY_SUMMARY_UNIT,
     +                                      REC=C_TG_HOURLY_SUMMARY_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        TRANS_GROUP_NAME,
     +                        FLOAT(DAY),
     +                        'Dynamic Storage     ',
     +                        (SYSTEM_STORAGE(J,R_TG),
     +                                                J=HOUR-23,HOUR)
                        C_TG_HOURLY_SUMMARY_REC =
     +                                       C_TG_HOURLY_SUMMARY_REC + 1
!
                        C_TG_HOURLY_SUMMARY_REC =
     +                             rptrec(C_TG_HOURLY_SUMMARY_UNIT)
                        WRITE(C_TG_HOURLY_SUMMARY_UNIT,
     +                                      REC=C_TG_HOURLY_SUMMARY_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        TRANS_GROUP_NAME,
     +                        FLOAT(DAY),
     +                        'Unserved Energy     ',
     +                        (SYSTEM_UNSERVED_ENERGY(J,R_TG),
     +                                                   J=HOUR-23,HOUR)
                        C_TG_HOURLY_SUMMARY_REC =
     +                                       C_TG_HOURLY_SUMMARY_REC + 1
!
                        C_TG_HOURLY_SUMMARY_REC =
     +                             rptrec(C_TG_HOURLY_SUMMARY_UNIT)
                        WRITE(C_TG_HOURLY_SUMMARY_UNIT,
     +                                      REC=C_TG_HOURLY_SUMMARY_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        TRANS_GROUP_NAME,
     +                        FLOAT(DAY),
     +                        'Spinning Reserves   ',
     +                        (SYSTEM_SPIN(J,R_TG),J=HOUR-23,HOUR)
                        C_TG_HOURLY_SUMMARY_REC =
     +                                       C_TG_HOURLY_SUMMARY_REC + 1
!
                        C_TG_HOURLY_SUMMARY_REC =
     +                             rptrec(C_TG_HOURLY_SUMMARY_UNIT)
                        WRITE(C_TG_HOURLY_SUMMARY_UNIT,
     +                                      REC=C_TG_HOURLY_SUMMARY_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        TRANS_GROUP_NAME,
     +                        FLOAT(DAY),
     +                        'Must Run Capacity   ',
     +                        (DAILY_MUST_RUN_CAPACITY(J),J=1,24)
                        C_TG_HOURLY_SUMMARY_REC =
     +                                       C_TG_HOURLY_SUMMARY_REC + 1
!
                        C_TG_HOURLY_SUMMARY_REC =
     +                             rptrec(C_TG_HOURLY_SUMMARY_UNIT)
                        WRITE(C_TG_HOURLY_SUMMARY_UNIT,
     +                                      REC=C_TG_HOURLY_SUMMARY_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        TRANS_GROUP_NAME,
     +                        FLOAT(DAY),
     +                        'Dump Energy         ',
     +                        (DUMP_ENERGY(J),J=1,24)
                        C_TG_HOURLY_SUMMARY_REC =
     +                                       C_TG_HOURLY_SUMMARY_REC + 1
!
                        C_TG_HOURLY_SUMMARY_REC =
     +                             rptrec(C_TG_HOURLY_SUMMARY_UNIT)
                        WRITE(C_TG_HOURLY_SUMMARY_UNIT,
     +                                      REC=C_TG_HOURLY_SUMMARY_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        TRANS_GROUP_NAME,
     +                        FLOAT(DAY),
     +                        'Decommitted Capacity',
     +                        (SYSTEM_HOURLY_DECOMMITTED(J,R_TG),
     +                                                   J=HOUR-23,HOUR)
                        C_TG_HOURLY_SUMMARY_REC =
     +                                       C_TG_HOURLY_SUMMARY_REC + 1
!
!!!!
!
                        C_HOURLY_MWH_COST_REC =
     +                             rptrec(C_HOURLY_MWH_COST_UNIT)
                        WRITE(C_HOURLY_MWH_COST_UNIT,
     +                                        REC=C_HOURLY_MWH_COST_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        FLOAT(DAY),
     +                        'Market Price        ',
     +                        (L_MONTHLY_PRICE(J),J=HOUR-23,HOUR)
                        C_HOURLY_MWH_COST_REC =
     +                                         C_HOURLY_MWH_COST_REC + 1
!
                        DO U = 1, NUM_OF_PRICING_GROUPS
                           C_HOURLY_MWH_COST_REC =
     +                             rptrec(C_HOURLY_MWH_COST_UNIT)
                           WRITE(C_HOURLY_MWH_COST_UNIT,
     +                                        REC=C_HOURLY_MWH_COST_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        FLOAT(DAY),
     +                        PRICING_GROUP_SELL_NAME(U),
     +                        (PRICING_GROUP_SELL_PRICE(U,J),J=1,24)
                           C_HOURLY_MWH_COST_REC =
     +                                         C_HOURLY_MWH_COST_REC + 1
                        END DO
                        IF(NUM_OF_PRICING_GROUPS > 0) THEN
                           C_HOURLY_MWH_COST_REC =
     +                             rptrec(C_HOURLY_MWH_COST_UNIT)
                           WRITE(C_HOURLY_MWH_COST_UNIT,
     +                                        REC=C_HOURLY_MWH_COST_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        FLOAT(DAY),
     +                        'Last Resource Cost  ',
     +                        (SYSTEM_HOURLY_LR(J,R_TG),
     +                                                   J=HOUR-23,HOUR)
                           C_HOURLY_MWH_COST_REC =
     +                                         C_HOURLY_MWH_COST_REC + 1
                        ENDIF
!
                        C_HOURLY_MWH_COST_REC =
     +                             rptrec(C_HOURLY_MWH_COST_UNIT)
                        WRITE(C_HOURLY_MWH_COST_UNIT,
     +                                        REC=C_HOURLY_MWH_COST_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        FLOAT(DAY),
     +                        'System Marginal Cost',
     +                        (SYSTEM_HOURLY_MC(J,R_TG),J=HOUR-23,HOUR)
                        C_HOURLY_MWH_COST_REC =
     +                                         C_HOURLY_MWH_COST_REC + 1
!
                        C_HOURLY_MWH_COST_REC =
     +                             rptrec(C_HOURLY_MWH_COST_UNIT)
                        WRITE(C_HOURLY_MWH_COST_UNIT,
     +                                        REC=C_HOURLY_MWH_COST_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        FLOAT(DAY),
     +                        'Wholesale Prod Cost ',
     +                        (DAILY_SYSTEM_COST_AND_REV(2,J),J=1,24)
                        C_HOURLY_MWH_COST_REC =
     +                                         C_HOURLY_MWH_COST_REC + 1
!
                        C_HOURLY_MWH_COST_REC =
     +                             rptrec(C_HOURLY_MWH_COST_UNIT)
                        WRITE(C_HOURLY_MWH_COST_UNIT,
     +                                        REC=C_HOURLY_MWH_COST_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        FLOAT(DAY),
     +                        'Retail Prod Cost    ',
     +                        (DAILY_SYSTEM_COST_AND_REV(1,J),J=1,24)
                        C_HOURLY_MWH_COST_REC =
     +                                         C_HOURLY_MWH_COST_REC + 1
!!!!
                        IF(DEPTH_OF_MARKET) THEN
!
                           C_HOURLY_MWH_COST_REC =
     +                             rptrec(C_HOURLY_MWH_COST_UNIT)
                           WRITE(C_HOURLY_MWH_COST_UNIT,
     +                                        REC=C_HOURLY_MWH_COST_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        FLOAT(DAY),
     +                        'Depth of Market Pric',
     +                        (DEPTH_PRICE(J),J=HOUR-23,HOUR)
                           C_HOURLY_MWH_COST_REC =
     +                                         C_HOURLY_MWH_COST_REC + 1
!
                           C_HOURLY_MWH_COST_REC =
     +                               rptrec(C_HOURLY_MWH_COST_UNIT)
                           WRITE(C_HOURLY_MWH_COST_UNIT,
     +                                        REC=C_HOURLY_MWH_COST_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        FLOAT(DAY),
     +                        'Depth Marginal Cost ',
     +                        (DEPTH_MARGINAL_COST(J),J=HOUR-23,HOUR)
                           C_HOURLY_MWH_COST_REC =
     +                                         C_HOURLY_MWH_COST_REC + 1
!
                           C_HOURLY_SUMMARY_REC =
     +                                rptrec(C_HOURLY_SUMMARY_UNIT)
                           WRITE(C_HOURLY_SUMMARY_UNIT,REC=
     +                                             C_HOURLY_SUMMARY_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        FLOAT(DAY),
     +                        'Depth of Market Quan',
     +                        (DEPTH_QUANTITY(J),J=HOUR-23,HOUR)
                           C_HOURLY_SUMMARY_REC =
     +                                          C_HOURLY_SUMMARY_REC + 1
!
                           C_HOURLY_SUMMARY_REC =
     +                                rptrec(C_HOURLY_SUMMARY_UNIT)
                           WRITE(C_HOURLY_SUMMARY_UNIT,REC=
     +                                             C_HOURLY_SUMMARY_REC)
     +                        PRT_ENDPOINT(),
     +                        THIS_YEAR,
     +                        R_MONTH_NAME,
     +                        FLOAT(DAY),
     +                        'Depth Resources     ',
     +                        (DEPTH_RESOURCES(J),J=HOUR-23,HOUR)
                           C_HOURLY_SUMMARY_REC =
     +                                          C_HOURLY_SUMMARY_REC + 1
                        ENDIF ! DEPTH OF MARKET
                     ENDIF ! YES HOURLY COMMITMENT REPORT
                  ENDIF ! YES HOURLY COMMITMENT REPORT OR FUEL REPORT
                  DAILY_GEN_BY_S_U = 0.
!
               ENDIF
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! END OF SYSTEM COMMITMENT CALCULATIONS
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
               IF(HOUR == HOURS_IN_MONTH) THEN
                  DO S_U = 1, NO_START_UP_UNITS
                     OPTION_INDEX = GET_START_UP_POSITION(S_U,R_TG)
                     A = GET_START_UP_INDEX(OPTION_INDEX)
            if(hours_in_month>744) then
                call end_program ("tf_objt:0008 - hours_in_month " //
     + "value of " // trim(itos(int(hours_in_month))) // 
     + " is invalid.")
            end if

                     CALL PUT_TRANS_C_ENERGY(A,
     +                         UNIT_GEN_IN_SYSTEM(1,S_U),
     +                         UNIT_GEN_IN_SYSTEM(2,S_U),HOURS_IN_MONTH)
! 092608. TO ADDRESS ODEC PROBLEM.
                     VOID_LOGICAL = UNIT_UP_YESTERDAY(A)
                     VOID_LOGICAL =
     +                     PUT_CLA_UNIT_UP_YESTERDAY(OPTION_INDEX,
     +                                                     VOID_LOGICAL)
!
! 01/24/02. FOR MORE ACCURATE CALC OF HEAT RATE.
! 01/30/02. RE-WRITTEN.
!
            if(hours_in_month>744) then
                call end_program ("tf_objt:0009 - hours_in_month " //
     + "value of " // trim(itos(int(hours_in_month))) // 
     + " is invalid.")
            end if

                     TEMP_R = 1.0
                     MONTH_LEFT_HEAT = TEMP_R*UNIT_HEAT_PARAM(1,S_U)/
     +                                             FLOAT(HOURS_IN_MONTH)
                     MONTH_RIGHT_HEAT = TEMP_R*UNIT_HEAT_PARAM(2,S_U)/
     +                                             FLOAT(HOURS_IN_MONTH)

                     CALL PUT_TRANS_C_HEAT(
     +                        A,
     +                        MONTH_LEFT_HEAT,
     +                        MONTH_RIGHT_HEAT)
!
!
!
                     MONTH_UNIT_START_COST(S_U) =
!     +                           MONTH_UNIT_START_COST(S_U) +
     +                                 MONTH_UNIT_STARTS(S_U) *
     +                                         START_UP_COSTS(S_U)
!
                     CALL PUT_TRANS_C_START_UPS(
     +                        A,
     +                        MONTH_UNIT_STARTS(S_U),
     +                        MONTH_UNIT_START_COST(S_U))
                  ENDDO
               ENDIF
!
               IF(HOOSIER) THEN
!
!                     COPYRIGHT(2001) M.S. GERBER & ASSOCIATES
!                                ALL RIGHTS RESERVED
!
!
                  IF(WILLIAMS_ACTIVE) THEN
!
!                 NEED TO DIFFERENTIATE BETWEEN FOR AND MOR.
!
                     IF(HEC_NATIVE_LOAD(HR) >=
     +                                     HEC_AVAIL_GEN_REQMT(HR)) THEN
!
! PASS ENERGY TO CL UNIT AT ZERO COST
!
                        WILLIAMS_OBLIGATION_MW(HR) =
     +                     HEC_NATIVE_LOAD(HR) - HEC_AVAIL_GEN_REQMT(HR)
                        WILLIAMS_OBLIGATION_ENERGY =
     +                        WILLIAMS_OBLIGATION_ENERGY +
     +                                    WILLIAMS_OBLIGATION_MW(HR)
                        WILLIAMS_OBLIGATION_PEAK =
     +                             MAX(WILLIAMS_OBLIGATION_PEAK,
     +                                       WILLIAMS_OBLIGATION_MW(HR))
                        WILLIAMS_OBLIGATION_HOURS =
     +                                    WILLIAMS_OBLIGATION_HOURS + 1.
!
                        IF(HEC_AVAIL_GEN_REQMT(HR) >
     +                                          HEC_ACTUAL_GEN(HR)) THEN
                           HEC_UC_RISK_ENERGY(HR) =
     +                               HEC_AVAIL_GEN_REQMT(HR) -
     +                                                HEC_ACTUAL_GEN(HR)
                           MONTHLY_HEC_UC_RISK_ENERGY =
     +                           MONTHLY_HEC_UC_RISK_ENERGY +
     +                                        HEC_UC_RISK_ENERGY(HR)
!
! INVOKE FORCED OUTAGE DERIVATIVE
!
                        ENDIF
                     ELSE
! 5/3/01.
                        IF( HEC_NATIVE_LOAD(HR) >
     +                                         HEC_ACTUAL_GEN(HR) ) THEN
                           HEC_UC_RISK_ENERGY(HR) =
     +                               HEC_NATIVE_LOAD(HR) -
     +                                                HEC_ACTUAL_GEN(HR)
                        ENDIF
                     ENDIF
!
!
! 05/15/01. HEC_EXCESS_MWH ALREADY INITIALIZED.
!
                     HEC_EXCESS_MWH(HR) =
     +                  MAX(0.,HEC_MARKET_GEN(HR) -
     +                        (HEC_NATIVE_LOAD(HR) +
     +                           HEC_PECO_LOAD(HR) + HEC_DAILY_RESERVE))
                     IF(HEC_EXCESS_MWH(HR) > 0.) THEN
! 06/04/01. 50MW BLOCKS
                        HEC_EXCESS_MWH(HR) = 50.*
     +                                       INT(HEC_EXCESS_MWH(HR)/50.)
!
                        HEC_EXCESS_ENERGY = HEC_EXCESS_ENERGY +
     +                                             HEC_EXCESS_MWH(HR)
                        HEC_EXCESS_PEAK = MAX(HEC_EXCESS_PEAK,
     +                                             HEC_EXCESS_MWH(HR))
                        HEC_EXCESS_HOURS = HEC_EXCESS_HOURS + 1.
                        HEC_EXCESS_ENERGY_REV =
     +                           HEC_EXCESS_ENERGY_REV +
     +                                   HEC_EXCESS_MWH(HR) *
     +                                            HEC_EXCESS_ENERGY_COST
                     ENDIF
!
                     HEC_NET_GENERATION(HR) =
     +                           HEC_MARKET_GEN(HR) +
     +                           WILLIAMS_OBLIGATION_MW(HR) +
     +                           HEC_UC_RISK_ENERGY(HR) -
     +                           HEC_NATIVE_LOAD(HR) -
     +                           HEC_PECO_LOAD(HR) -
     +                           HEC_EXCESS_MWH(HR) -
     +                           HEC_DAILY_RESERVE
!
                     HEC_MARKET_PURCHASES(HR) =
     +                           HEC_NATIVE_LOAD(HR) +
     +                     HEC_PECO_LOAD(HR) -  ! TOTAL LOAD OBLIGATIONS
!
     +                           HEC_MARKET_GEN(HR) -
     +                 HEC_EXCESS_MWH(HR) - ! AVAILABLE GEN TO MEET LOAD
!
     +                           WILLIAMS_OBLIGATION_MW(HR) -
     +              HEC_UC_RISK_ENERGY(HR) ! REMAINING MARKET PURCHASES
!
                     HEC_GEN_BOUGHT(HR) = MAX(0.,
     +                           HEC_NATIVE_LOAD(HR) +
     +                           HEC_PECO_LOAD(HR) -
     +                                 HEC_MARKET_GEN(HR))
                     HEC_GEN_SOLD(HR) = -1.0 * MAX(0.,
     +                           HEC_MARKET_GEN(HR) -
     +                               ( HEC_NATIVE_LOAD(HR) +
     +                                 HEC_PECO_LOAD(HR)))
                     HEC_TOTAL_SUPPLY(HR) =
     +                           HEC_MARKET_GEN(HR) +
     +                           HEC_GEN_SOLD(HR) +
     +                           HEC_GEN_BOUGHT(HR)
                     HEC_GEN_AV_4LD_AF_MKT(HR) =
     +                           HEC_MARKET_GEN(HR) +
     +                           HEC_GEN_SOLD(HR)
                     HEC_MKT_PP(HR) =
     +                           HEC_NATIVE_LOAD(HR) +
     +                           HEC_PECO_LOAD(HR) -
     +                           HEC_GEN_AV_4LD_AF_MKT(HR)
                     HEC_REM_MKT_PURCH(HR) =
     +                           HEC_MKT_PP(HR) -
     +                           WILLIAMS_OBLIGATION_MW(HR)
                     HEC_TOTAL_DEMAND(HR) =
     +                           HEC_GEN_AV_4LD_AF_MKT(HR) +
     +                           WILLIAMS_OBLIGATION_MW(HR) +
     +                           HEC_REM_MKT_PURCH(HR)
!
! 05/20/01. Begin Monthly Accumulation Hourly Variables
!                     HEC_NATIVE_LOAD
!                     HEC_MARKET_GEN
!                     HEC_GEN_BOUGHT
!                     HEC_GEN_SOLD
!                     HEC_DAILY_RES
!
! for the following subitems
!
!                     ENERGY
!                     PEAK
!                     HOURS
!                     ENERGY_REVENUE
!
                     IF(HEC_NATIVE_LOAD(HR) > 0.) THEN
                        HEC_NATIVE_LOAD_ENERGY =
     +                               HEC_NATIVE_LOAD_ENERGY +
     +                               HEC_NATIVE_LOAD(HR)
                        HEC_NATIVE_LOAD_PEAK =
     +                           MAX(HEC_NATIVE_LOAD_PEAK,
     +                               HEC_NATIVE_LOAD(HR))
                        HEC_NATIVE_LOAD_HOURS =
     +                               HEC_NATIVE_LOAD_HOURS +
     +                               1.
                        HEC_NATIVE_LOAD_ENERGY_REV =
     +                               HEC_NATIVE_LOAD_ENERGY_REV +
     +                               HEC_NATIVE_LOAD(HR) *
     +                               LOCAL_PRICE(HOUR)
                     ENDIF
!
                     IF(HEC_MARKET_GEN(HR) > 0.) THEN
                        HEC_MARKET_GEN_ENERGY =
     +                               HEC_MARKET_GEN_ENERGY +
     +                               HEC_MARKET_GEN(HR)
                        HEC_MARKET_GEN_PEAK =
     +                           MAX(HEC_MARKET_GEN_PEAK,
     +                               HEC_MARKET_GEN(HR))
                        HEC_MARKET_GEN_HOURS =
     +                               HEC_MARKET_GEN_HOURS +
     +                               1.
                        HEC_MARKET_GEN_ENERGY_REV =
     +                               HEC_MARKET_GEN_ENERGY_REV +
     +                               HEC_MARKET_GEN(HR) *
     +                               LOCAL_PRICE(HOUR)
                     ENDIF
!
                     IF(HEC_GEN_BOUGHT(HR) > 0.) THEN
                        HEC_GEN_BOUGHT_ENERGY =
     +                               HEC_GEN_BOUGHT_ENERGY +
     +                               HEC_GEN_BOUGHT(HR)
                        HEC_GEN_BOUGHT_PEAK =
     +                           MAX(HEC_GEN_BOUGHT_PEAK,
     +                               HEC_GEN_BOUGHT(HR))
                        HEC_GEN_BOUGHT_HOURS =
     +                               HEC_GEN_BOUGHT_HOURS +
     +                               1.
                        HEC_GEN_BOUGHT_ENERGY_REV =
     +                               HEC_GEN_BOUGHT_ENERGY_REV +
     +                               HEC_GEN_BOUGHT(HR) *
     +                               LOCAL_PRICE(HOUR)
                     ENDIF
!
! HEC_Gen_sold is a negative number
!
                     IF(HEC_GEN_SOLD(HR) < 0.) THEN
                        HEC_GEN_SOLD_ENERGY =
     +                               HEC_GEN_SOLD_ENERGY +
     +                               HEC_GEN_SOLD(HR)
                        HEC_GEN_SOLD_PEAK =
     +                           MIN(HEC_GEN_SOLD_PEAK,
     +                               HEC_GEN_SOLD(HR))
                        HEC_GEN_SOLD_HOURS =
     +                               HEC_GEN_SOLD_HOURS +
     +                               1.
                        HEC_GEN_SOLD_ENERGY_REV =
     +                               HEC_GEN_SOLD_ENERGY_REV +
     +                               HEC_GEN_SOLD(HR) *
     +                               LOCAL_PRICE(HOUR)
                     ENDIF
!
                     IF(HEC_DAILY_RESERVE > 0.) THEN
                        HEC_DAILY_RESERVE_ENERGY =
     +                               HEC_DAILY_RESERVE_ENERGY +
     +                               HEC_DAILY_RESERVE
                        HEC_DAILY_RESERVE_PEAK =
     +                           MAX(HEC_DAILY_RESERVE_PEAK,
     +                               HEC_DAILY_RESERVE)
                        HEC_DAILY_RESERVE_HOURS =
     +                               HEC_DAILY_RESERVE_HOURS +
     +                               1.
                        HEC_DAILY_RESERVE_ENERGY_REV =
     +                               HEC_DAILY_RESERVE_ENERGY_REV +
     +                               HEC_DAILY_RESERVE *
     +                               LOCAL_PRICE(HOUR)
                     ENDIF
!
                     IF(HEC_UC_RISK_ENERGY(HR) > 0.) THEN
                        HEC_DAILY_RISK_ENERGY =
     +                               HEC_DAILY_RISK_ENERGY +
     +                               HEC_UC_RISK_ENERGY(HR)
                        HEC_DAILY_RISK_PEAK =
     +                           MAX(HEC_DAILY_RISK_PEAK,
     +                               HEC_UC_RISK_ENERGY(HR))
                        HEC_DAILY_RISK_HOURS =
     +                               HEC_DAILY_RISK_HOURS +
     +                               1.
                        HEC_DAILY_RISK_ENERGY_REV =
     +                               HEC_DAILY_RISK_ENERGY_REV +
     +                               HEC_UC_RISK_ENERGY(HR) *
     +                               LOCAL_PRICE(HOUR)
                     ENDIF
!
                     IF(HEC_NET_GENERATION(HR) > 0.) THEN
                        HEC_DAILY_NETG_ENERGY =
     +                               HEC_DAILY_NETG_ENERGY +
     +                               HEC_NET_GENERATION(HR)
                        HEC_DAILY_NETG_PEAK =
     +                           MAX(HEC_DAILY_NETG_PEAK,
     +                               HEC_NET_GENERATION(HR))
                        HEC_DAILY_NETG_HOURS =
     +                               HEC_DAILY_NETG_HOURS +
     +                               1.
                        HEC_DAILY_NETG_ENERGY_REV =
     +                               HEC_DAILY_NETG_ENERGY_REV +
     +                               HEC_NET_GENERATION(HR) *
     +                               LOCAL_PRICE(HOUR)
                     ENDIF
!
!
                  ELSE ! AFTER 2003 NO WILLIAMS
!
!                NOTE:WILLIAMS PROVISIONS ZERO'D OUT AT BEGINNING OF DAY
!
                     IF( HEC_NATIVE_LOAD(HR) >
     +                                         HEC_ACTUAL_GEN(HR) ) THEN
                        HEC_UC_RISK_ENERGY(HR) =
     +                               HEC_NATIVE_LOAD(HR) -
     +                                                HEC_ACTUAL_GEN(HR)
                     ENDIF
                  ENDIF ! WILLIAMS_ACTIVE
!
! HOURLY HOOSIER REPORT FOR TRACKING: WILLIAMS AND PECO
!
                  HEC_SUMMARY_REPORT = .TRUE.
                  IF(HR == 24 .AND.
     +                           HEC_SUMMARY_REPORT .AND.
     +                                       YES_HOURLY_COMMITMENT) THEN
!
                     IF(HEC_REPORT_NOT_OPEN) THEN
                        HEC_REPORT_NOT_OPEN = .FALSE.
                        HEC_REPORT_VARIABLES = 24
                        HEC_HOURLY_NO =
     +                          HEC_HOURLY_HEADER(HEC_REPORT_VARIABLES,
     +                                                   HEC_HOURLY_REC)
                     ENDIF
!
                     HEC_HOURLY_REC = rptrec(HEC_HOURLY_NO)
                     WRITE(HEC_HOURLY_NO,REC=HEC_HOURLY_REC)
     +                  PRT_ENDPOINT(),
     +                  THIS_YEAR,
     +                  R_MONTH_NAME,
     +                  FLOAT(DAY),
     +                  "Hoosier Native Load ",
     +                  (HEC_NATIVE_LOAD(J),J=1,24)
                     HEC_HOURLY_REC = HEC_HOURLY_REC + 1
!
                     HEC_HOURLY_REC = rptrec(HEC_HOURLY_NO)
                     WRITE(HEC_HOURLY_NO,REC=HEC_HOURLY_REC)
     +                  PRT_ENDPOINT(),
     +                  THIS_YEAR,
     +                  R_MONTH_NAME,
     +                  FLOAT(DAY),
     +                  "HEC Member Load     ",
     +                  (HEC_MEMBER_LOAD(J),J=1,24)
                     HEC_HOURLY_REC = HEC_HOURLY_REC + 1
!
                     HEC_HOURLY_REC = rptrec(HEC_HOURLY_NO)
                     WRITE(HEC_HOURLY_NO,REC=HEC_HOURLY_REC)
     +                  PRT_ENDPOINT(),
     +                  THIS_YEAR,
     +                  R_MONTH_NAME,
     +                  FLOAT(DAY),
     +                  "HEC PECO Load       ",
     +                  (HEC_PECO_LOAD(J),J=1,24)
                     HEC_HOURLY_REC = HEC_HOURLY_REC + 1
!
                     HEC_HOURLY_REC = rptrec(HEC_HOURLY_NO)
                     WRITE(HEC_HOURLY_NO,REC=HEC_HOURLY_REC)
     +                  PRT_ENDPOINT(),
     +                  THIS_YEAR,
     +                  R_MONTH_NAME,
     +                  FLOAT(DAY),
     +                  "HEC Daily Reserve   ",
     +                  (HEC_DAILY_RESERVE,J=1,24)
                     HEC_HOURLY_REC = HEC_HOURLY_REC + 1
!
                     HEC_HOURLY_REC = rptrec(HEC_HOURLY_NO)
                     WRITE(HEC_HOURLY_NO,REC=HEC_HOURLY_REC)
     +                  PRT_ENDPOINT(),
     +                  THIS_YEAR,
     +                  R_MONTH_NAME,
     +                  FLOAT(DAY),
     +                  "HEC Actual Gen      ",
     +                  (HEC_ACTUAL_GEN(J),J=1,24)
                     HEC_HOURLY_REC = HEC_HOURLY_REC + 1
!
                     HEC_HOURLY_REC = rptrec(HEC_HOURLY_NO)
                     WRITE(HEC_HOURLY_NO,REC=HEC_HOURLY_REC)
     +                  PRT_ENDPOINT(),
     +                  THIS_YEAR,
     +                  R_MONTH_NAME,
     +                  FLOAT(DAY),
     +                  "HEC Avail Gen Reqmnt",
     +                  (HEC_AVAIL_GEN_REQMT(J),J=1,24)
                     HEC_HOURLY_REC = HEC_HOURLY_REC + 1
!
                     HEC_HOURLY_REC = rptrec(HEC_HOURLY_NO)
                     WRITE(HEC_HOURLY_NO,REC=HEC_HOURLY_REC)
     +                  PRT_ENDPOINT(),
     +                  THIS_YEAR,
     +                  R_MONTH_NAME,
     +                  FLOAT(DAY),
     +                  "Williams Obligation ",
     +                  (WILLIAMS_OBLIGATION_MW(J),J=1,24)
                     HEC_HOURLY_REC = HEC_HOURLY_REC + 1
!
                     HEC_HOURLY_REC = rptrec(HEC_HOURLY_NO)
                     WRITE(HEC_HOURLY_NO,REC=HEC_HOURLY_REC)
     +                  PRT_ENDPOINT(),
     +                  THIS_YEAR,
     +                  R_MONTH_NAME,
     +                  FLOAT(DAY),
     +                  "HEC UC Risk Energy  ",
     +                  (HEC_UC_RISK_ENERGY(J),J=1,24)
                     HEC_HOURLY_REC = HEC_HOURLY_REC + 1
!
                     HEC_HOURLY_REC = rptrec(HEC_HOURLY_NO)
                     WRITE(HEC_HOURLY_NO,REC=HEC_HOURLY_REC)
     +                  PRT_ENDPOINT(),
     +                  THIS_YEAR,
     +                  R_MONTH_NAME,
     +                  FLOAT(DAY),
     +                  "HEC Excess Energy   ",
     +                  (HEC_EXCESS_MWH(J),J=1,24)
                     HEC_HOURLY_REC = HEC_HOURLY_REC + 1
!
                     HEC_HOURLY_REC = rptrec(HEC_HOURLY_NO)
                     WRITE(HEC_HOURLY_NO,REC=HEC_HOURLY_REC)
     +                  PRT_ENDPOINT(),
     +                  THIS_YEAR,
     +                  R_MONTH_NAME,
     +                  FLOAT(DAY),
     +                  "HEC Market Gen      ",
     +                  (HEC_MARKET_GEN(J),J=1,24)
                     HEC_HOURLY_REC = HEC_HOURLY_REC + 1
!
                     HEC_HOURLY_REC = rptrec(HEC_HOURLY_NO)
                     WRITE(HEC_HOURLY_NO,REC=HEC_HOURLY_REC)
     +                  PRT_ENDPOINT(),
     +                  THIS_YEAR,
     +                  R_MONTH_NAME,
     +                  FLOAT(DAY),
     +                  "HEC Total Maint. Cap",
     +                  (HEC_MOR_CAPACITY(J),J=1,24)
                     HEC_HOURLY_REC = HEC_HOURLY_REC + 1
!
                     HEC_HOURLY_REC = rptrec(HEC_HOURLY_NO)
                     WRITE(HEC_HOURLY_NO,REC=HEC_HOURLY_REC)
     +                  PRT_ENDPOINT(),
     +                  THIS_YEAR,
     +                  R_MONTH_NAME,
     +                  FLOAT(DAY),
     +                  "HEC Net Generation  ",
     +                  (HEC_NET_GENERATION(J),J=1,24)
                     HEC_HOURLY_REC = HEC_HOURLY_REC + 1
!
                     HEC_HOURLY_REC = rptrec(HEC_HOURLY_NO)
                     WRITE(HEC_HOURLY_NO,REC=HEC_HOURLY_REC)
     +                  PRT_ENDPOINT(),
     +                  THIS_YEAR,
     +                  R_MONTH_NAME,
     +                  FLOAT(DAY),
     +                  "HEC Gen Bought      ",
     +                  (HEC_GEN_BOUGHT(J),J=1,24)
                     HEC_HOURLY_REC = HEC_HOURLY_REC + 1
!
                     HEC_HOURLY_REC = rptrec(HEC_HOURLY_NO)
                     WRITE(HEC_HOURLY_NO,REC=HEC_HOURLY_REC)
     +                  PRT_ENDPOINT(),
     +                  THIS_YEAR,
     +                  R_MONTH_NAME,
     +                  FLOAT(DAY),
     +                  "HEC Gen Sold        ",
     +                  (HEC_GEN_SOLD(J),J=1,24)
                     HEC_HOURLY_REC = HEC_HOURLY_REC + 1
!
                     HEC_HOURLY_REC = rptrec(HEC_HOURLY_NO)
                     WRITE(HEC_HOURLY_NO,REC=HEC_HOURLY_REC)
     +                  PRT_ENDPOINT(),
     +                  THIS_YEAR,
     +                  R_MONTH_NAME,
     +                  FLOAT(DAY),
     +                  "HEC Total Supply    ",
     +                  (HEC_TOTAL_SUPPLY(J),J=1,24)
                     HEC_HOURLY_REC = HEC_HOURLY_REC + 1
!
                     HEC_HOURLY_REC = rptrec(HEC_HOURLY_NO)
                     WRITE(HEC_HOURLY_NO,REC=HEC_HOURLY_REC)
     +                  PRT_ENDPOINT(),
     +                  THIS_YEAR,
     +                  R_MONTH_NAME,
     +                  FLOAT(DAY),
     +                  "HEC Gen Av4Ld AF Mkt",
     +                  (HEC_GEN_AV_4LD_AF_MKT(J),J=1,24)
                     HEC_HOURLY_REC = HEC_HOURLY_REC + 1
!
                     HEC_HOURLY_REC = rptrec(HEC_HOURLY_NO)
                     WRITE(HEC_HOURLY_NO,REC=HEC_HOURLY_REC)
     +                  PRT_ENDPOINT(),
     +                  THIS_YEAR,
     +                  R_MONTH_NAME,
     +                  FLOAT(DAY),
     +                  "HEC Mkt PP          ",
     +                  (HEC_MKT_PP(J),J=1,24)
                     HEC_HOURLY_REC = HEC_HOURLY_REC + 1
!
                     HEC_HOURLY_REC = rptrec(HEC_HOURLY_NO)
                     WRITE(HEC_HOURLY_NO,REC=HEC_HOURLY_REC)
     +                  PRT_ENDPOINT(),
     +                  THIS_YEAR,
     +                  R_MONTH_NAME,
     +                  FLOAT(DAY),
     +                  "HEC Rem Mkt Purch   ",
     +                  (HEC_REM_MKT_PURCH(J),J=1,24)
                     HEC_HOURLY_REC = HEC_HOURLY_REC + 1
!
                     HEC_HOURLY_REC = rptrec(HEC_HOURLY_NO)
                     WRITE(HEC_HOURLY_NO,REC=HEC_HOURLY_REC)
     +                  PRT_ENDPOINT(),
     +                  THIS_YEAR,
     +                  R_MONTH_NAME,
     +                  FLOAT(DAY),
     +                  "HEC Total Demand    ",
     +                  (HEC_TOTAL_DEMAND(J),J=1,24)
                     HEC_HOURLY_REC = HEC_HOURLY_REC + 1
!
                     HEC_HOURLY_REC = rptrec(HEC_HOURLY_NO)
                     WRITE(HEC_HOURLY_NO,REC=HEC_HOURLY_REC)
     +                  PRT_ENDPOINT(),
     +                  THIS_YEAR,
     +                  R_MONTH_NAME,
     +                  FLOAT(DAY),
     +                  "HEC Market Price    ",
     +                  (LOCAL_PRICE(J),J=HOUR-23,HOUR)
                     HEC_HOURLY_REC = HEC_HOURLY_REC + 1
                  ENDIF
            if(hours_in_month>744) then
                call end_program ("tf_objt:0010 - hours_in_month " //
     + "value of " // trim(itos(int(hours_in_month))) // 
     + " is invalid.")
            end if
                  IF(HOUR == HOURS_IN_MONTH) THEN
!
                     IF(YES_DAILY_COMMITMENT) THEN

!
                        IF(WILLIAMS_OBLIGATION_PEAK > 0.) THEN
                           CAPACITY_FACTOR =
     +                           WILLIAMS_OBLIGATION_ENERGY /
     +                           (WILLIAMS_OBLIGATION_PEAK*.24*
     +                                                  R_DAYS_IN_MONTH)
                        ELSE
                           CAPACITY_FACTOR = 0.
                        ENDIF
!
!
                        CX_DAILY_REC = rptrec(CX_DAILY_NO)
                        WRITE(CX_DAILY_NO,REC=CX_DAILY_REC)
     +                     PRT_ENDPOINT(),THIS_YEAR,R_MONTH_NAME,
     +                     FLOAT( 0), ! DAY = 0
     +                     "Williams Obligation ",
     +                     0., ! STRIKE PRICE
     +                     WILLIAMS_OBLIGATION_ENERGY,
     +                     0., ! PAYOFF
     +                     0., ! COST
     +                     0., ! GROSS
     +                     0.,
     +                     0.,
     +                     0., WILLIAMS_OBLIGATION_HOURS,
     +                     WILLIAMS_OBLIGATION_HOURS,
     +                     0.,
     +                     0.,
     +                     0.,
     +                     WILLIAMS_OBLIGATION_HOURS,
     +                     0.,
     +                     0., CAPACITY_FACTOR,
     +                     0.  ! HEAT RATE
                       CX_DAILY_REC = CX_DAILY_REC + 1
!
                       IF(HEC_EXCESS_PEAK > 0.) THEN
                           CAPACITY_FACTOR =
     +                           HEC_EXCESS_ENERGY /
     +                             (HEC_EXCESS_PEAK*.24*R_DAYS_IN_MONTH)
                        ELSE
                           CAPACITY_FACTOR = 0.
                        ENDIF
                        IF(HEC_EXCESS_ENERGY > 0.) THEN
                           OPTION_STRIKE_PRICE(0) =
     +                              HEC_EXCESS_ENERGY_REV /
     +                                              HEC_EXCESS_ENERGY
                        ELSE
                           OPTION_STRIKE_PRICE(0) = 0.
                        ENDIF

!
                        CX_DAILY_REC = rptrec(CX_DAILY_NO)
                       WRITE(CX_DAILY_NO,REC=CX_DAILY_REC)
     +                     PRT_ENDPOINT(),THIS_YEAR,R_MONTH_NAME,
     +                     FLOAT( 0), ! DAY = 0
     +                     "HEC Excess Energy   ",
     +                     OPTION_STRIKE_PRICE(0), ! STRIKE PRICE
     +                     HEC_EXCESS_ENERGY,
     +                     0., ! PAYOFF
     +                     0., ! COST
     +                     HEC_EXCESS_ENERGY_REV, ! GROSS
     +                     0.,
     +                     0.,
     +                     0.,
     +                     HEC_EXCESS_HOURS, HEC_EXCESS_HOURS,
     +                     0.,
     +                     0.,
     +                     0.,HEC_EXCESS_HOURS,
     +                     0.,
     +                     0.,
     +                     CAPACITY_FACTOR,
     +                     0.  ! HEAT RATE
                       CX_DAILY_REC = CX_DAILY_REC + 1
!
!
                       IF(HEC_PECO_PEAK > 0.) THEN
                           CAPACITY_FACTOR =
     +                           HEC_PECO_ENERGY /
     +                               (HEC_PECO_PEAK*.24*R_DAYS_IN_MONTH)
                        ELSE
                           CAPACITY_FACTOR = 0.
                        ENDIF
!
        HEC_PECO_ENERGY_REVENUE = HEC_PECO_PRICE(globecom_YEAR) *
     +          HEC_PECO_ENERGY
!
                        CX_DAILY_REC = rptrec(CX_DAILY_NO)
                       WRITE(CX_DAILY_NO,REC=CX_DAILY_REC)
     +                     PRT_ENDPOINT(),THIS_YEAR,R_MONTH_NAME,
     +                     FLOAT( 0), ! DAY = 0
     +                     "HEC PECO Load       ",
     +                     HEC_PECO_PRICE(globecom_YEAR), ! STRIKE PRICE
     +                     HEC_PECO_ENERGY,
     +                     0., ! PAYOFF
     +                     0., ! COST
     +                     HEC_PECO_ENERGY_REVENUE, ! GROSS
     +                     0.,
     +                     0.,
     +                     0.,
     +                     HEC_PECO_HOURS,HEC_PECO_HOURS,
     +                     0.,
     +                     0.,
     +                     0.,HEC_PECO_HOURS,
     +                     0.,
     +                     0.,
     +                     CAPACITY_FACTOR,
     +                     0.  ! HEAT RATE
                       CX_DAILY_REC = CX_DAILY_REC + 1
!
!
                       IF(HEC_NATIVE_LOAD_PEAK > 0.) THEN
                           CAPACITY_FACTOR =
     +                           HEC_NATIVE_LOAD_ENERGY /
     +                             (HEC_NATIVE_LOAD_PEAK *
     +                             .24 * R_DAYS_IN_MONTH)
                        ELSE
                           CAPACITY_FACTOR = 0.
                        ENDIF
                        IF(HEC_NATIVE_LOAD_ENERGY > 0.) THEN
                           OPTION_STRIKE_PRICE(0) =
     +                              HEC_NATIVE_LOAD_ENERGY_REV/
     +                              HEC_NATIVE_LOAD_ENERGY
                        ELSE
                           OPTION_STRIKE_PRICE(0) = 0.
                        ENDIF
!
                        CX_DAILY_REC = rptrec(CX_DAILY_NO)
                       WRITE(CX_DAILY_NO,REC=CX_DAILY_REC)
     +                     PRT_ENDPOINT(),THIS_YEAR,R_MONTH_NAME,
     +                     FLOAT( 0), ! DAY = 0
     +                     "HEC Hoosier Native  ",
     +                     OPTION_STRIKE_PRICE(0), ! STRIKE PRICE
     +                     HEC_NATIVE_LOAD_ENERGY,
     +                     0., ! PAYOFF
     +                     0., ! COST
     +                     HEC_NATIVE_LOAD_ENERGY_REV, ! GROSS
     +                     0.,
     +                     0.,
     +                     0.,HEC_NATIVE_LOAD_HOURS,
     +                     HEC_NATIVE_LOAD_HOURS,
     +                     0.,
     +                     0.,
     +                     0.,HEC_NATIVE_LOAD_HOURS,
     +                     0.,
     +                     0.,
     +                     CAPACITY_FACTOR,
     +                     0.  ! HEAT RATE
                       CX_DAILY_REC = CX_DAILY_REC + 1
!
!
                       IF(HEC_MARKET_GEN_PEAK > 0.) THEN
                           CAPACITY_FACTOR =
     +                           HEC_MARKET_GEN_ENERGY /
     +                             (HEC_MARKET_GEN_PEAK *
     +                             .24 * R_DAYS_IN_MONTH)
                        ELSE
                           CAPACITY_FACTOR = 0.
                        ENDIF
                        IF(HEC_MARKET_GEN_ENERGY > 0.) THEN
                           OPTION_STRIKE_PRICE(0) =
     +                              HEC_MARKET_GEN_ENERGY_REV/
     +                              HEC_MARKET_GEN_ENERGY
                        ELSE
                           OPTION_STRIKE_PRICE(0) = 0.
                        ENDIF
!
                        CX_DAILY_REC = rptrec(CX_DAILY_NO)
                       WRITE(CX_DAILY_NO,REC=CX_DAILY_REC)
     +                     PRT_ENDPOINT(),THIS_YEAR,R_MONTH_NAME,
     +                     FLOAT( 0), ! DAY = 0
     +                     "HEC Total Generation",
     +                     OPTION_STRIKE_PRICE(0), ! STRIKE PRICE
     +                     HEC_MARKET_GEN_ENERGY,
     +                     0., ! PAYOFF
     +                     0., ! COST
     +                     HEC_MARKET_GEN_ENERGY_REV, ! GROSS
     +                     0.,
     +                     0.,
     +                     0.,
     +                     HEC_MARKET_GEN_HOURS,HEC_MARKET_GEN_HOURS,
     +                     0.,
     +                     0.,
     +                     0.,HEC_MARKET_GEN_HOURS,
     +                     0.,
     +                     0.,
     +                     CAPACITY_FACTOR,
     +                     0.  ! HEAT RATE
                       CX_DAILY_REC = CX_DAILY_REC + 1
!
!
                       IF(HEC_GEN_BOUGHT_PEAK > 0.) THEN
                           CAPACITY_FACTOR =
     +                           HEC_GEN_BOUGHT_ENERGY /
     +                             (HEC_GEN_BOUGHT_PEAK *
     +                             .24 * R_DAYS_IN_MONTH)
                        ELSE
                           CAPACITY_FACTOR = 0.
                        ENDIF
                        IF(HEC_GEN_BOUGHT_ENERGY > 0.) THEN
                           OPTION_STRIKE_PRICE(0) =
     +                              HEC_GEN_BOUGHT_ENERGY_REV/
     +                              HEC_GEN_BOUGHT_ENERGY
                        ELSE
                           OPTION_STRIKE_PRICE(0) = 0.
                        ENDIF
!
                        CX_DAILY_REC = rptrec(CX_DAILY_NO)
                       WRITE(CX_DAILY_NO,REC=CX_DAILY_REC)
     +                     PRT_ENDPOINT(),THIS_YEAR,R_MONTH_NAME,
     +                     FLOAT( 0), ! DAY = 0
     +                     "HEC Gen Purchases   ",
     +                     OPTION_STRIKE_PRICE(0), ! STRIKE PRICE
     +                     HEC_GEN_BOUGHT_ENERGY,
     +                     0., ! PAYOFF
     +                     0., ! COST
     +                     HEC_GEN_BOUGHT_ENERGY_REV, ! GROSS
     +                     0.,
     +                     0.,
     +                     0.,
     +                     HEC_GEN_BOUGHT_HOURS,HEC_GEN_BOUGHT_HOURS,
     +                     0.,
     +                     0.,
     +                     0.,
     +                     HEC_GEN_BOUGHT_HOURS,
     +                     0.,
     +                     0.,CAPACITY_FACTOR,
     +                     0.  ! HEAT RATE
                       CX_DAILY_REC = CX_DAILY_REC + 1
!
!
!     HEC_GEN_SOLD is a negative number
!
                       IF(HEC_GEN_SOLD_PEAK < 0.) THEN
                           CAPACITY_FACTOR =
     +                           HEC_GEN_SOLD_ENERGY /
     +                             (HEC_GEN_SOLD_PEAK *
     +                              0.24 * R_DAYS_IN_MONTH)
                        ELSE
                           CAPACITY_FACTOR = 0.
                        ENDIF
                        IF(HEC_GEN_SOLD_ENERGY < 0.) THEN
                           OPTION_STRIKE_PRICE(0) =
     +                              HEC_GEN_SOLD_ENERGY_REV/
     +                              HEC_GEN_SOLD_ENERGY
                        ELSE
                           OPTION_STRIKE_PRICE(0) = 0.
                        ENDIF
!
                        CX_DAILY_REC = rptrec(CX_DAILY_NO)
                       WRITE(CX_DAILY_NO,REC=CX_DAILY_REC)
     +                     PRT_ENDPOINT(),THIS_YEAR,R_MONTH_NAME,
     +                     FLOAT( 0), ! DAY = 0
     +                     "HEC Gen Sales       ",
     +                     OPTION_STRIKE_PRICE(0), ! STRIKE PRICE
     +                     HEC_GEN_SOLD_ENERGY,
     +                     0., ! PAYOFF
     +                     0., ! COST
     +                     HEC_GEN_SOLD_ENERGY_REV, ! GROSS
     +                     0.,
     +                     0.,
     +                     0.,
     +                     HEC_GEN_SOLD_HOURS,HEC_GEN_SOLD_HOURS,
     +                     0.,
     +                     0.,
     +                     0.,
     +                     HEC_GEN_SOLD_HOURS,
     +                     0.,
     +                     0.,CAPACITY_FACTOR,
     +                     0.  ! HEAT RATE
                       CX_DAILY_REC = CX_DAILY_REC + 1
!
!
                       IF(HEC_DAILY_RESERVE_PEAK > 0.) THEN
                           CAPACITY_FACTOR =
     +                           HEC_DAILY_RESERVE_ENERGY /
     +                             (HEC_DAILY_RESERVE_PEAK *
     +                             .24 * R_DAYS_IN_MONTH)
                        ELSE
                           CAPACITY_FACTOR = 0.
                        ENDIF
                        IF(HEC_DAILY_RESERVE_ENERGY > 0.) THEN
                           OPTION_STRIKE_PRICE(0) =
     +                              HEC_DAILY_RESERVE_ENERGY_REV/
     +                              HEC_DAILY_RESERVE_ENERGY
                        ELSE
                           OPTION_STRIKE_PRICE(0) = 0.
                        ENDIF
!
                        CX_DAILY_REC = rptrec(CX_DAILY_NO)
                       WRITE(CX_DAILY_NO,REC=CX_DAILY_REC)
     +                     PRT_ENDPOINT(),THIS_YEAR,R_MONTH_NAME,
     +                     FLOAT( 0), ! DAY = 0
     +                     "HEC Daily Reserve   ",
     +                     OPTION_STRIKE_PRICE(0), ! STRIKE PRICE
     +                     HEC_DAILY_RESERVE_ENERGY,
     +                     0., ! PAYOFF
     +                     0., ! COST
     +                     HEC_DAILY_RESERVE_ENERGY_REV, ! GROSS
     +                     0.,
     +                     0.,
     +                     0.,HEC_DAILY_RESERVE_HOURS,
     +                     HEC_DAILY_RESERVE_HOURS,
     +                     0.,
     +                     0.,
     +                     0.,
     +                     HEC_DAILY_RESERVE_HOURS,
     +                     0.,
     +                     0.,CAPACITY_FACTOR,
     +                     0.  ! HEAT RATE
                        CX_DAILY_REC = CX_DAILY_REC + 1
!
                       IF(HEC_DAILY_RISK_PEAK > 0.) THEN
                           CAPACITY_FACTOR =
     +                           HEC_DAILY_RISK_ENERGY /
     +                             (HEC_DAILY_RISK_PEAK *
     +                             .24 * R_DAYS_IN_MONTH)
                        ELSE
                           CAPACITY_FACTOR = 0.
                        ENDIF
                        IF(HEC_DAILY_RISK_ENERGY > 0.) THEN
                           OPTION_STRIKE_PRICE(0) =
     +                              HEC_DAILY_RISK_ENERGY_REV/
     +                              HEC_DAILY_RISK_ENERGY
                        ELSE
                           OPTION_STRIKE_PRICE(0) = 0.
                        ENDIF
!
                        CX_DAILY_REC = rptrec(CX_DAILY_NO)
                       WRITE(CX_DAILY_NO,REC=CX_DAILY_REC)
     +                     PRT_ENDPOINT(),THIS_YEAR,R_MONTH_NAME,
     +                     FLOAT( 0), ! DAY = 0
     +                     "HEC Daily RISK      ",
     +                     OPTION_STRIKE_PRICE(0), ! STRIKE PRICE
     +                     HEC_DAILY_RISK_ENERGY,
     +                     0., ! PAYOFF
     +                     0., ! COST
     +                     HEC_DAILY_RISK_ENERGY_REV, ! GROSS
     +                     0.,
     +                     0.,
     +                     0.,
     +                     HEC_DAILY_RISK_HOURS,HEC_DAILY_RISK_HOURS,
     +                     0.,
     +                     0.,
     +                     0.,
     +                     HEC_DAILY_RISK_HOURS,
     +                     0.,
     +                     0.,CAPACITY_FACTOR,
     +                     0.  ! HEAT RATE
                        CX_DAILY_REC = CX_DAILY_REC + 1
!
                       IF(HEC_DAILY_NETG_PEAK > 0.) THEN
                           CAPACITY_FACTOR =
     +                           HEC_DAILY_NETG_ENERGY /
     +                             (HEC_DAILY_NETG_PEAK *
     +                             .24 * R_DAYS_IN_MONTH)
                        ELSE
                           CAPACITY_FACTOR = 0.
                        ENDIF
                        IF(HEC_DAILY_NETG_ENERGY > 0.) THEN
                           OPTION_STRIKE_PRICE(0) =
     +                              HEC_DAILY_NETG_ENERGY_REV/
     +                              HEC_DAILY_NETG_ENERGY
                        ELSE
                           OPTION_STRIKE_PRICE(0) = 0.
                        ENDIF
!
                        CX_DAILY_REC = rptrec(CX_DAILY_NO)
                       WRITE(CX_DAILY_NO,REC=CX_DAILY_REC)
     +                     PRT_ENDPOINT(),THIS_YEAR,R_MONTH_NAME,
     +                     FLOAT( 0), ! DAY = 0
     +                     "HEC Daily NETG      ",
     +                     OPTION_STRIKE_PRICE(0), ! STRIKE PRICE
     +                     HEC_DAILY_NETG_ENERGY,
     +                     0., ! PAYOFF
     +                     0., ! COST
     +                     HEC_DAILY_NETG_ENERGY_REV, ! GROSS
     +                     0.,
     +                     0.,
     +                     0.,
     +                     HEC_DAILY_NETG_HOURS,HEC_DAILY_NETG_HOURS,
     +                     0.,
     +                     0.,
     +                     0.,
     +                     HEC_DAILY_NETG_HOURS,
     +                     0.,
     +                     0.,CAPACITY_FACTOR,
     +                     0.  ! HEAT RATE
                        CX_DAILY_REC = CX_DAILY_REC + 1
!
! ANNUAL ACCUMULATORS
!
                        WILLIAMS_OBLIGATION_ENERGY_AN =
     +                          WILLIAMS_OBLIGATION_ENERGY_AN +
     +                                        WILLIAMS_OBLIGATION_ENERGY
                        WILLIAMS_OBLIGATION_PEAK_AN =
     +                           MAX(WILLIAMS_OBLIGATION_PEAK_AN,
     +                                         WILLIAMS_OBLIGATION_PEAK)
                        WILLIAMS_OBLIGATION_HOURS_AN =
     +                          WILLIAMS_OBLIGATION_HOURS_AN +
     +                                         WILLIAMS_OBLIGATION_HOURS
!
                        HEC_EXCESS_ENERGY_AN = HEC_EXCESS_ENERGY_AN +
     +                                                 HEC_EXCESS_ENERGY
                        HEC_EXCESS_PEAK_AN = MAX(HEC_EXCESS_PEAK_AN,
     +                                             HEC_EXCESS_PEAK)
                        HEC_EXCESS_HOURS_AN = HEC_EXCESS_HOURS_AN +
     +                                                  HEC_EXCESS_HOURS
                        HEC_EXCESS_ENERGY_REV_AN =
     +                           HEC_EXCESS_ENERGY_REV_AN +
     +                                             HEC_EXCESS_ENERGY_REV
!
                        HEC_PECO_ENERGY_AN = HEC_PECO_ENERGY_AN +
     +                                                   HEC_PECO_ENERGY
                        HEC_PECO_PEAK_AN = MAX(HEC_PECO_PEAK_AN,
     +                                         HEC_PECO_PEAK)
                        HEC_PECO_HOURS_AN = HEC_PECO_HOURS_AN +
     +                                                    HEC_PECO_HOURS
!
                        HEC_NATIVE_LOAD_ENERGY_AN =
     +                               HEC_NATIVE_LOAD_ENERGY_AN +
     +                                            HEC_NATIVE_LOAD_ENERGY
                        HEC_NATIVE_LOAD_PEAK_AN =
     +                           MAX(HEC_NATIVE_LOAD_PEAK_AN,
     +                               HEC_NATIVE_LOAD_PEAK)
                        HEC_NATIVE_LOAD_HOURS_AN =
     +                               HEC_NATIVE_LOAD_HOURS_AN +
     +                                             HEC_NATIVE_LOAD_HOURS
                        HEC_NATIVE_LOAD_ENERGY_REV_AN =
     +                               HEC_NATIVE_LOAD_ENERGY_REV_AN +
     +                               HEC_NATIVE_LOAD_ENERGY_REV
!
                        HEC_MARKET_GEN_ENERGY_AN =
     +                               HEC_MARKET_GEN_ENERGY_AN +
     +                                             HEC_MARKET_GEN_ENERGY
                        HEC_MARKET_GEN_PEAK_AN =
     +                           MAX(HEC_MARKET_GEN_PEAK_AN,
     +                               HEC_MARKET_GEN_PEAK)
                        HEC_MARKET_GEN_HOURS_AN =
     +                               HEC_MARKET_GEN_HOURS_AN +
     +                                              HEC_MARKET_GEN_HOURS
                        HEC_MARKET_GEN_ENERGY_REV_AN =
     +                               HEC_MARKET_GEN_ENERGY_REV_AN +
     +                                         HEC_MARKET_GEN_ENERGY_REV
!
                        HEC_GEN_BOUGHT_ENERGY_AN =
     +                               HEC_GEN_BOUGHT_ENERGY_AN +
     +                                             HEC_GEN_BOUGHT_ENERGY
                        HEC_GEN_BOUGHT_PEAK_AN =
     +                           MAX(HEC_GEN_BOUGHT_PEAK_AN,
     +                               HEC_GEN_BOUGHT_PEAK)
                        HEC_GEN_BOUGHT_HOURS_AN =
     +                               HEC_GEN_BOUGHT_HOURS_AN +
     +                                              HEC_GEN_BOUGHT_HOURS
                        HEC_GEN_BOUGHT_ENERGY_REV_AN =
     +                               HEC_GEN_BOUGHT_ENERGY_REV_AN +
     +                               HEC_GEN_BOUGHT_ENERGY_REV
!
                        HEC_GEN_SOLD_ENERGY_AN =
     +                               HEC_GEN_SOLD_ENERGY_AN +
     +                                               HEC_GEN_SOLD_ENERGY
                        HEC_GEN_SOLD_PEAK_AN =
     +                           MIN(HEC_GEN_SOLD_PEAK_AN,
     +                                                HEC_GEN_SOLD_PEAK)
                        HEC_GEN_SOLD_HOURS_AN =
     +                               HEC_GEN_SOLD_HOURS_AN +
     +                                                HEC_GEN_SOLD_HOURS
                        HEC_GEN_SOLD_ENERGY_REV_AN =
     +                               HEC_GEN_SOLD_ENERGY_REV_AN +
     +                                           HEC_GEN_SOLD_ENERGY_REV
!
                        HEC_DAILY_RESERVE_ENERGY_AN =
     +                               HEC_DAILY_RESERVE_ENERGY_AN +
     +                                          HEC_DAILY_RESERVE_ENERGY
                        HEC_DAILY_RESERVE_PEAK_AN =
     +                           MAX(HEC_DAILY_RESERVE_PEAK_AN,
     +                                           HEC_DAILY_RESERVE_PEAK)
                        HEC_DAILY_RESERVE_HOURS_AN =
     +                               HEC_DAILY_RESERVE_HOURS_AN +
     +                                           HEC_DAILY_RESERVE_HOURS
                        HEC_DAILY_RESERVE_ENER_REV_AN =
     +                               HEC_DAILY_RESERVE_ENER_REV_AN +
     +                                      HEC_DAILY_RESERVE_ENERGY_REV
!
                        HEC_DAILY_RISK_ENERGY_AN =
     +                               HEC_DAILY_RISK_ENERGY_AN +
     +                                          HEC_DAILY_RISK_ENERGY
                        HEC_DAILY_RISK_PEAK_AN =
     +                           MAX(HEC_DAILY_RISK_PEAK_AN,
     +                                           HEC_DAILY_RISK_PEAK)
                        HEC_DAILY_RISK_HOURS_AN =
     +                               HEC_DAILY_RISK_HOURS_AN +
     +                                           HEC_DAILY_RISK_HOURS
                        HEC_DAILY_RISK_ENERGY_REV_AN =
     +                               HEC_DAILY_RISK_ENERGY_REV_AN +
     +                                      HEC_DAILY_RISK_ENERGY_REV
!
                        HEC_DAILY_NETG_ENERGY_AN =
     +                               HEC_DAILY_NETG_ENERGY_AN +
     +                                          HEC_DAILY_NETG_ENERGY
                        HEC_DAILY_NETG_PEAK_AN =
     +                           MAX(HEC_DAILY_NETG_PEAK_AN,
     +                                           HEC_DAILY_NETG_PEAK)
                        HEC_DAILY_NETG_HOURS_AN =
     +                               HEC_DAILY_NETG_HOURS_AN +
     +                                           HEC_DAILY_NETG_HOURS
                        HEC_DAILY_NETG_ENERGY_REV_AN =
     +                               HEC_DAILY_NETG_ENERGY_REV_AN +
     +                                      HEC_DAILY_NETG_ENERGY_REV
!
! ANNUAL WRITE
                        IF(R_MONTH == 12) THEN
                          IF(WILLIAMS_OBLIGATION_PEAK_AN > 0.) THEN
                              CAPACITY_FACTOR =
     +                              100.*WILLIAMS_OBLIGATION_ENERGY_AN /
     +                              (WILLIAMS_OBLIGATION_PEAK_AN*
     +                                     WILLIAMS_OBLIGATION_HOURS_AN)
                          ELSE
                              CAPACITY_FACTOR = 0.
                          ENDIF
!
!
                          CX_DAILY_REC = rptrec(CX_DAILY_NO)
                          WRITE(CX_DAILY_NO,REC=CX_DAILY_REC)
     +                     PRT_ENDPOINT(),THIS_YEAR,'Annual   ',
     +                     FLOAT( 0), ! DAY = 0
     +                     "Williams Obligation ",
     +                     0., ! STRIKE PRICE
     +                     WILLIAMS_OBLIGATION_ENERGY_AN,
     +                     0., ! PAYOFF
     +                     0., ! COST
     +                     0., ! GROSS
     +                     0.,
     +                     0.,
     +                     0.,WILLIAMS_OBLIGATION_HOURS_AN,
     +                     WILLIAMS_OBLIGATION_HOURS_AN,
     +                     0.,
     +                     0.,
     +                     0.,
     +                     WILLIAMS_OBLIGATION_HOURS_AN,
     +                     0.,
     +                     0.,CAPACITY_FACTOR,
     +                     0.  ! HEAT RATE
                         CX_DAILY_REC = CX_DAILY_REC + 1
!
                         IF(HEC_EXCESS_PEAK > 0.) THEN
                           CAPACITY_FACTOR =
     +                           100. * HEC_EXCESS_ENERGY_AN /
     +                          (HEC_EXCESS_PEAK_AN*HEC_EXCESS_HOURS_AN)
                         ELSE
                           CAPACITY_FACTOR = 0.
                         ENDIF
                         IF(HEC_EXCESS_ENERGY_AN > 0.) THEN
                           OPTION_STRIKE_PRICE(0) =
     +                              HEC_EXCESS_ENERGY_REV_AN /
     +                                              HEC_EXCESS_ENERGY_AN
                         ELSE
                           OPTION_STRIKE_PRICE(0) = 0.
                         ENDIF

!
                        CX_DAILY_REC = rptrec(CX_DAILY_NO)
                         WRITE(CX_DAILY_NO,REC=CX_DAILY_REC)
     +                     PRT_ENDPOINT(),THIS_YEAR,'Annual   ',
     +                     FLOAT( 0), ! DAY = 0
     +                     "HEC Excess Energy   ",
     +                     OPTION_STRIKE_PRICE(0), ! STRIKE PRICE
     +                     HEC_EXCESS_ENERGY_AN,
     +                     0., ! PAYOFF
     +                     0., ! COST
     +                     HEC_EXCESS_ENERGY_REV_AN, ! GROSS
     +                     0.,
     +                     0.,
     +                     0.,
     +                     HEC_EXCESS_HOURS_AN,HEC_EXCESS_HOURS_AN,
     +                     0.,
     +                     0.,
     +                     0.,
     +                     HEC_EXCESS_HOURS_AN,
     +                     0.,
     +                     0.,CAPACITY_FACTOR,
     +                     0.  ! HEAT RATE
                         CX_DAILY_REC = CX_DAILY_REC + 1
!
!
                         IF(HEC_PECO_PEAK > 0.) THEN
                           CAPACITY_FACTOR =
     +                           100. * HEC_PECO_ENERGY_AN /
     +                            (HEC_PECO_PEAK_AN*HEC_PECO_HOURS_AN)
                         ELSE
                           CAPACITY_FACTOR = 0.
                         ENDIF
!
                         HEC_PECO_ENERGY_REVENUE_AN =
     +                              HEC_PECO_PRICE(globecom_YEAR) *
     +                                                HEC_PECO_ENERGY_AN
!
                        CX_DAILY_REC = rptrec(CX_DAILY_NO)
                          WRITE(CX_DAILY_NO,REC=CX_DAILY_REC)
     +                     PRT_ENDPOINT(),THIS_YEAR,'Annual   ',
     +                     FLOAT( 0), ! DAY = 0
     +                     "HEC PECO Load       ",
     +                     HEC_PECO_PRICE(globecom_YEAR), ! STRIKE PRICE
     +                     HEC_PECO_ENERGY_AN,
     +                     0., ! PAYOFF
     +                     0., ! COST
     +                     HEC_PECO_ENERGY_REVENUE_AN, ! GROSS
     +                     0.,
     +                     0.,
     +                     0.,
     +                     HEC_PECO_HOURS_AN,HEC_PECO_HOURS_AN,
     +                     0.,
     +                     0.,
     +                     0.,
     +                     HEC_PECO_HOURS_AN,
     +                     0.,
     +                     0.,CAPACITY_FACTOR,
     +                     0.  ! HEAT RATE
                         CX_DAILY_REC = CX_DAILY_REC + 1
!
!
                         IF(HEC_NATIVE_LOAD_PEAK_AN > 0.) THEN
                           CAPACITY_FACTOR =
     +                           100. * HEC_NATIVE_LOAD_ENERGY_AN /
     +                             (HEC_NATIVE_LOAD_PEAK_AN *
     +                                         HEC_NATIVE_LOAD_HOURS_AN)
                         ELSE
                           CAPACITY_FACTOR = 0.
                         ENDIF
                         IF(HEC_NATIVE_LOAD_ENERGY_AN > 0.) THEN
                           OPTION_STRIKE_PRICE(0) =
     +                              HEC_NATIVE_LOAD_ENERGY_REV_AN/
     +                              HEC_NATIVE_LOAD_ENERGY_AN
                         ELSE
                           OPTION_STRIKE_PRICE(0) = 0.
                         ENDIF
!
                        CX_DAILY_REC = rptrec(CX_DAILY_NO)
                         WRITE(CX_DAILY_NO,REC=CX_DAILY_REC)
     +                     PRT_ENDPOINT(),THIS_YEAR,'Annual   ',
     +                     FLOAT( 0), ! DAY = 0
     +                     "HEC Hoosier Native  ",
     +                     OPTION_STRIKE_PRICE(0), ! STRIKE PRICE
     +                     HEC_NATIVE_LOAD_ENERGY_AN,
     +                     0., ! PAYOFF
     +                     0., ! COST
     +                     HEC_NATIVE_LOAD_ENERGY_REV_AN, ! GROSS
     +                     0.,
     +                     0.,
     +                     0.,HEC_NATIVE_LOAD_HOURS_AN,
     +                     HEC_NATIVE_LOAD_HOURS_AN,
     +                     0.,
     +                     0.,
     +                     0.,
     +                     HEC_NATIVE_LOAD_HOURS_AN,
     +                     0.,
     +                     0.,CAPACITY_FACTOR,
     +                     0.  ! HEAT RATE
                         CX_DAILY_REC = CX_DAILY_REC + 1
!
!
                         IF(HEC_MARKET_GEN_PEAK > 0.) THEN
                           CAPACITY_FACTOR =
     +                           100. * HEC_MARKET_GEN_ENERGY_AN /
     +                             (HEC_MARKET_GEN_PEAK_AN *
     +                                          HEC_MARKET_GEN_HOURS_AN)
                         ELSE
                           CAPACITY_FACTOR = 0.
                         ENDIF
                         IF(HEC_MARKET_GEN_ENERGY_AN > 0.) THEN
                           OPTION_STRIKE_PRICE(0) =
     +                              HEC_MARKET_GEN_ENERGY_REV_AN/
     +                              HEC_MARKET_GEN_ENERGY_AN
                         ELSE
                           OPTION_STRIKE_PRICE(0) = 0.
                         ENDIF
!
                        CX_DAILY_REC = rptrec(CX_DAILY_NO)
                         WRITE(CX_DAILY_NO,REC=CX_DAILY_REC)
     +                     PRT_ENDPOINT(),THIS_YEAR,'Annual   ',
     +                     FLOAT( 0), ! DAY = 0
     +                     "HEC Total Generation",
     +                     OPTION_STRIKE_PRICE(0), ! STRIKE PRICE
     +                     HEC_MARKET_GEN_ENERGY_AN,
     +                     0., ! PAYOFF
     +                     0., ! COST
     +                     HEC_MARKET_GEN_ENERGY_REV_AN, ! GROSS
     +                     0.,
     +                     0.,
     +                     0.,HEC_MARKET_GEN_HOURS_AN,
     +                     HEC_MARKET_GEN_HOURS_AN,
     +                     0.,
     +                     0.,
     +                     0.,
     +                     HEC_MARKET_GEN_HOURS_AN,
     +                     0.,
     +                     0.,CAPACITY_FACTOR,
     +                     0.  ! HEAT RATE
                         CX_DAILY_REC = CX_DAILY_REC + 1
!
!
                         IF(HEC_GEN_BOUGHT_PEAK_AN > 0.) THEN
                           CAPACITY_FACTOR =
     +                           100. * HEC_GEN_BOUGHT_ENERGY_AN /
     +                                (HEC_GEN_BOUGHT_PEAK_AN *
     +                                          HEC_GEN_BOUGHT_HOURS_AN)
                         ELSE
                           CAPACITY_FACTOR = 0.
                         ENDIF
                         IF(HEC_GEN_BOUGHT_ENERGY_AN > 0.) THEN
                           OPTION_STRIKE_PRICE(0) =
     +                              HEC_GEN_BOUGHT_ENERGY_REV_AN/
     +                              HEC_GEN_BOUGHT_ENERGY_AN
                         ELSE
                           OPTION_STRIKE_PRICE(0) = 0.
                         ENDIF
!
                        CX_DAILY_REC = rptrec(CX_DAILY_NO)
                         WRITE(CX_DAILY_NO,REC=CX_DAILY_REC)
     +                     PRT_ENDPOINT(),THIS_YEAR,'Annual   ',
     +                     FLOAT( 0), ! DAY = 0
     +                     "HEC Gen Purchases   ",
     +                     OPTION_STRIKE_PRICE(0), ! STRIKE PRICE
     +                     HEC_GEN_BOUGHT_ENERGY_AN,
     +                     0., ! PAYOFF
     +                     0., ! COST
     +                     HEC_GEN_BOUGHT_ENERGY_REV_AN, ! GROSS
     +                     0.,
     +                     0.,
     +                     0.,HEC_GEN_BOUGHT_HOURS_AN,
     +                     HEC_GEN_BOUGHT_HOURS_AN,
     +                     0.,
     +                     0.,
     +                     0.,
     +                     HEC_GEN_BOUGHT_HOURS_AN,
     +                     0.,
     +                     0.,CAPACITY_FACTOR,
     +                     0.  ! HEAT RATE
                         CX_DAILY_REC = CX_DAILY_REC + 1
!
!
!     HEC_GEN_SOLD is a negative number
!
                         IF(HEC_GEN_SOLD_PEAK < 0.) THEN
                           CAPACITY_FACTOR =
     +                           100. * HEC_GEN_SOLD_ENERGY_AN /
     +                                 (HEC_GEN_SOLD_PEAK_AN *
     +                                            HEC_GEN_SOLD_HOURS_AN)
                         ELSE
                           CAPACITY_FACTOR = 0.
                         ENDIF
                         IF(HEC_GEN_SOLD_ENERGY_AN < 0.) THEN
                           OPTION_STRIKE_PRICE(0) =
     +                              HEC_GEN_SOLD_ENERGY_REV_AN/
     +                              HEC_GEN_SOLD_ENERGY_AN
                         ELSE
                           OPTION_STRIKE_PRICE(0) = 0.
                         ENDIF
!
                        CX_DAILY_REC = rptrec(CX_DAILY_NO)
                         WRITE(CX_DAILY_NO,REC=CX_DAILY_REC)
     +                     PRT_ENDPOINT(),THIS_YEAR,'Annual   ',
     +                     FLOAT( 0), ! DAY = 0
     +                     "HEC Gen Sales       ",
     +                     OPTION_STRIKE_PRICE(0), ! STRIKE PRICE
     +                     HEC_GEN_SOLD_ENERGY_AN,
     +                     0., ! PAYOFF
     +                     0., ! COST
     +                     HEC_GEN_SOLD_ENERGY_REV_AN, ! GROSS
     +                     0.,
     +                     0.,
     +                     0.,
     +                     HEC_GEN_SOLD_HOURS_AN,HEC_GEN_SOLD_HOURS_AN,
     +                     0.,
     +                     0.,
     +                     0.,
     +                     HEC_GEN_SOLD_HOURS_AN,
     +                     0.,
     +                     0.,CAPACITY_FACTOR,
     +                     0.  ! HEAT RATE
                        CX_DAILY_REC = CX_DAILY_REC + 1
!
!
                        IF(HEC_DAILY_RESERVE_PEAK_AN > 0.) THEN
                           CAPACITY_FACTOR =
     +                           100. * HEC_DAILY_RESERVE_ENERGY_AN /
     +                             (HEC_DAILY_RESERVE_PEAK_AN *
     +                                       HEC_DAILY_RESERVE_HOURS_AN)
                        ELSE
                           CAPACITY_FACTOR = 0.
                        ENDIF
                        IF(HEC_DAILY_RESERVE_ENERGY_AN > 0.) THEN
                           OPTION_STRIKE_PRICE(0) =
     +                              HEC_DAILY_RESERVE_ENER_REV_AN/
     +                              HEC_DAILY_RESERVE_ENERGY_AN
                        ELSE
                           OPTION_STRIKE_PRICE(0) = 0.
                        ENDIF
!
                        CX_DAILY_REC = rptrec(CX_DAILY_NO)
                        WRITE(CX_DAILY_NO,REC=CX_DAILY_REC)
     +                     PRT_ENDPOINT(),THIS_YEAR,'Annual   ',
     +                     FLOAT( 0), ! DAY = 0
     +                     "HEC Daily Reserve   ",
     +                     OPTION_STRIKE_PRICE(0), ! STRIKE PRICE
     +                     HEC_DAILY_RESERVE_ENERGY_AN,
     +                     0., ! PAYOFF
     +                     0., ! COST
     +                     HEC_DAILY_RESERVE_ENER_REV_AN, ! GROSS
     +                     0.,
     +                     0.,
     +                     0.,HEC_DAILY_RESERVE_HOURS_AN,
     +                     HEC_DAILY_RESERVE_HOURS_AN,
     +                     0.,
     +                     0.,
     +                     0.,
     +                     HEC_DAILY_RESERVE_HOURS_AN,
     +                     0.,
     +                     0.,CAPACITY_FACTOR,
     +                     0.  ! HEAT RATE
                        CX_DAILY_REC = CX_DAILY_REC + 1
!
                        IF(HEC_DAILY_RISK_PEAK_AN > 0.) THEN
                           CAPACITY_FACTOR =
     +                           100. * HEC_DAILY_RISK_ENERGY_AN /
     +                             (HEC_DAILY_RISK_PEAK_AN *
     +                                       HEC_DAILY_RISK_HOURS_AN)
                        ELSE
                           CAPACITY_FACTOR = 0.
                        ENDIF
                        IF(HEC_DAILY_RISK_ENERGY_AN > 0.) THEN
                           OPTION_STRIKE_PRICE(0) =
     +                              HEC_DAILY_RISK_ENERGY_REV_AN/
     +                              HEC_DAILY_RISK_ENERGY_AN
                        ELSE
                           OPTION_STRIKE_PRICE(0) = 0.
                        ENDIF
!
                        CX_DAILY_REC = rptrec(CX_DAILY_NO)
                        WRITE(CX_DAILY_NO,REC=CX_DAILY_REC)
     +                     PRT_ENDPOINT(),THIS_YEAR,'Annual   ',
     +                     FLOAT( 0), ! DAY = 0
     +                     "HEC Daily RISK      ",
     +                     OPTION_STRIKE_PRICE(0), ! STRIKE PRICE
     +                     HEC_DAILY_RISK_ENERGY_AN,
     +                     0., ! PAYOFF
     +                     0., ! COST
     +                     HEC_DAILY_RISK_ENERGY_REV_AN, ! GROSS
     +                     0.,
     +                     0.,
     +                     0.,HEC_DAILY_RISK_HOURS_AN,
     +                     HEC_DAILY_RISK_HOURS_AN,
     +                     0.,
     +                     0.,
     +                     0.,
     +                     HEC_DAILY_RISK_HOURS_AN,
     +                     0.,
     +                     0.,CAPACITY_FACTOR,
     +                     0.  ! HEAT RATE
                        CX_DAILY_REC = CX_DAILY_REC + 1
!
                        IF(HEC_DAILY_NETG_PEAK_AN > 0.) THEN
                           CAPACITY_FACTOR =
     +                           100. * HEC_DAILY_NETG_ENERGY_AN /
     +                             (HEC_DAILY_NETG_PEAK_AN *
     +                                       HEC_DAILY_NETG_HOURS_AN)
                        ELSE
                           CAPACITY_FACTOR = 0.
                        ENDIF
                        IF(HEC_DAILY_NETG_ENERGY_AN > 0.) THEN
                           OPTION_STRIKE_PRICE(0) =
     +                              HEC_DAILY_NETG_ENERGY_REV_AN/
     +                              HEC_DAILY_NETG_ENERGY_AN
                        ELSE
                           OPTION_STRIKE_PRICE(0) = 0.
                        ENDIF
!
                        CX_DAILY_REC = rptrec(CX_DAILY_NO)
                        WRITE(CX_DAILY_NO,REC=CX_DAILY_REC)
     +                     PRT_ENDPOINT(),THIS_YEAR,'Annual   ',
     +                     FLOAT( 0), ! DAY = 0
     +                     "HEC Daily NETG      ",
     +                     OPTION_STRIKE_PRICE(0), ! STRIKE PRICE
     +                     HEC_DAILY_NETG_ENERGY_AN,
     +                     0., ! PAYOFF
     +                     0., ! COST
     +                     HEC_DAILY_NETG_ENERGY_REV_AN, ! GROSS
     +                     0.,
     +                     0.,
     +                     0.,HEC_DAILY_NETG_HOURS_AN,
     +                     HEC_DAILY_NETG_HOURS_AN,
     +                     0.,
     +                     0.,
     +                     0.,
     +                     HEC_DAILY_NETG_HOURS_AN,
     +                     0.,
     +                     0.,CAPACITY_FACTOR,
     +                     0.  ! HEAT RATE
                        CX_DAILY_REC = CX_DAILY_REC + 1
                     ENDIF


                    ENDIF ! DAILY COMMITMENT REPORT
                  ENDIF ! DAYS IN MONTH
               ENDIF ! HOOSIER

               SYSTEM_MARKET_PRICE(0,TG) =
     +                           SYSTEM_MARKET_PRICE(0,TG) +
     +                              SYSTEM_MARKET_PRICE(HOUR,TG)
               SYSTEM_EMERGENCY_CAPACITY(0,TG) =
     +                           SYSTEM_EMERGENCY_CAPACITY(0,TG) +
     +                              SYSTEM_EMERGENCY_CAPACITY(HOUR,TG)

               SYSTEM_HOURLY_COMMITMENT(0,TG) =
     +                           SYSTEM_HOURLY_COMMITMENT(0,TG) +
     +                              SYSTEM_HOURLY_COMMITMENT(HOUR,TG)
               SYSTEM_HOURLY_DECOMMITTED(0,TG) =
     +                           SYSTEM_HOURLY_DECOMMITTED(0,TG) +
     +                              SYSTEM_HOURLY_DECOMMITTED(HOUR,TG)
               SYSTEM_HOURLY_PRE_COMMITMENT(0,TG) =
     +                           SYSTEM_HOURLY_PRE_COMMITMENT(0,TG) +
     +                             SYSTEM_HOURLY_PRE_COMMITMENT(HOUR,TG)
               SYSTEM_HOURLY_AVAILABLE(0,TG) =
     +                           SYSTEM_HOURLY_AVAILABLE(0,TG) +
     +                              SYSTEM_HOURLY_AVAILABLE(HOUR,TG)
               SYSTEM_HOURLY_COMMIT_AVAIL(0,TG) =
     +                           SYSTEM_HOURLY_COMMIT_AVAIL(0,TG) +
     +                              SYSTEM_HOURLY_COMMIT_AVAIL(HOUR,TG)
               SYSTEM_HOURLY_LOAD(0,TG) =
     +                           SYSTEM_HOURLY_LOAD(0,TG) +
     +                              SYSTEM_HOURLY_LOAD(HOUR,TG)
               SYSTEM_HOURLY_HYDRO(0,TG) =
     +                           SYSTEM_HOURLY_HYDRO(0,TG) +
     +                              SYSTEM_HOURLY_HYDRO(HOUR,TG)

               SYSTEM_HOURLY_MC(0,TG) =
     +                           SYSTEM_HOURLY_MC(0,TG) +
     +                              SYSTEM_HOURLY_MC(HOUR,TG)
               SYSTEM_HOURLY_MC_AT_LOAD(0,TG) =
     +                           SYSTEM_HOURLY_MC_AT_LOAD(0,TG) +
     +                              SYSTEM_HOURLY_MC_AT_LOAD(HOUR,TG)
               SYSTEM_NATIVE_COST(0,TG) =
     +                           SYSTEM_NATIVE_COST(0,TG) +
     +                              SYSTEM_NATIVE_COST(HOUR,TG)
               SYSTEM_UNSERVED_ENERGY(0,TG) =
     +                           SYSTEM_UNSERVED_ENERGY(0,TG) +
     +                              SYSTEM_UNSERVED_ENERGY(HOUR,TG)
               SYSTEM_ENERGY_ABOVE(0,TG) =
     +                           SYSTEM_ENERGY_ABOVE(0,TG) +
     +                              SYSTEM_ENERGY_ABOVE(HOUR,TG)
               SYSTEM_DERIVATIVES(0,TG) =
     +                           SYSTEM_DERIVATIVES(0,TG) +
     +                              SYSTEM_DERIVATIVES(HOUR,TG)
               SYSTEM_STORAGE(0,TG) =
     +                           SYSTEM_STORAGE(0,TG) +
     +                              SYSTEM_STORAGE(HOUR,TG)
               SYSTEM_AVAIL_DERIVATIVES(0,TG) =
     +                           SYSTEM_AVAIL_DERIVATIVES(0,TG) +
     +                              SYSTEM_AVAIL_DERIVATIVES(HOUR,TG)
               SYSTEM_AVAIL_STORAGE(0,TG) =
     +                           SYSTEM_AVAIL_STORAGE(0,TG) +
     +                              SYSTEM_AVAIL_STORAGE(HOUR,TG)
               SYSTEM_SPIN(0,TG) =
     +                           SYSTEM_SPIN(0,TG) +
     +                              SYSTEM_SPIN(HOUR,TG)
               SYSTEM_DSM_MW(0,TG) =
     +                           SYSTEM_DSM_MW(0,TG) +
     +                              SYSTEM_DSM_MW(HOUR,TG)
               SYSTEM_WHOLESALE_GENERATION(0,TG) =
     +                           SYSTEM_WHOLESALE_GENERATION(0,TG) +
     +                              SYSTEM_WHOLESALE_GENERATION(HOUR,TG)
               SYSTEM_MARKET_BOUGHT(0,TG) =
     +                           SYSTEM_MARKET_BOUGHT(0,TG) +
     +                              SYSTEM_MARKET_BOUGHT(HOUR,TG)
               SYSTEM_MARKET_REVENUE(0,TG) =
     +                           SYSTEM_MARKET_REVENUE(0,TG) +
     +                                  SYSTEM_MARKET_REVENUE(HOUR,TG)
               SYSTEM_MARKET_COST(0,TG) =
     +                           SYSTEM_MARKET_COST(0,TG) +
     +                                  SYSTEM_MARKET_COST(HOUR,TG)
!
!
! 062106. COST OF MARKET SALES
!
               SYSTEM_MONTHLY_MISC(23,TG) = SYSTEM_MONTHLY_MISC(23,TG) +
     +                          SYSTEM_COST_OF_MARKET_SALES(HOUR,R_TG,1)
!
!
! TRANSACT C POSITION
!
               LOCAL_DAILY_MARKET_REVENUE = LOCAL_PRICE(HOUR)*
     +                              SYSTEM_WHOLESALE_GENERATION(HOUR,TG)
!
               IF(R_MONTH == 2 .AND.
     +                     LOCAL_DAILY_MARKET_REVENUE > 0.1) THEN
                  LOCAL_DAILY_MARKET_REVENUE =
     +                                        LOCAL_DAILY_MARKET_REVENUE
               ENDIF
!
               TEMP_POSIT_HYDRO =
     +               GET_TRANS_HOURLY_HYDRO(HOUR,R_TG)
     +                    - (SYSTEM_HOURLY_STORAGE_GEN(HOUR,R_TG) +
     +                           SYSTEM_HOURLY_STORAGE_PUMP(HOUR,R_TG))
               IF(ABS(TEMP_POSIT_HYDRO) < .7) THEN
                  TEMP_POSIT_HYDRO = 0.0
               ENDIF
!
! NEED TO GET HOURLY LOAD AFTER HYDRO.
!
! DEMAND
! MOVED WEEKLY HYDRO UP

!
                  PRODUCT_HOURS_POSIT(1) = PRODUCT_HOURS_POSIT(1) + 1.
! 051507.
                  SYSTEM_MONTHLY_POSIT(1,1,TG) =
     +               SYSTEM_MONTHLY_POSIT(1,1,TG) +
     +                          HOURLY_TRANSACTION_LOAD(HOUR,TG) +
     +                                        WEEKLY_HYDRO_FOR_HR

                  SYSTEM_MONTHLY_POSIT(1,2,TG) =
     +                        SYSTEM_MONTHLY_POSIT(1,2,TG) -
     +                               MIN(0.,SYSTEM_DERIVATIVES(HOUR,TG))
                  SYSTEM_MONTHLY_POSIT(1,3,TG) =
     +                           SYSTEM_MONTHLY_POSIT(1,3,TG) +
     +                              SYSTEM_WHOLESALE_GENERATION(HOUR,TG)
                  SYSTEM_MONTHLY_POSIT(1,26,TG) =
     +                           SYSTEM_MONTHLY_POSIT(1,26,TG) -
     +                             SYSTEM_HOURLY_STORAGE_PUMP(HOUR,R_TG)
                  SYSTEM_MONTHLY_POSIT(1,4,TG) =
     +                           SYSTEM_MONTHLY_POSIT(1,4,TG) +
     +                                                   DUMP_ENERGY(HR)

! SUPPLY
                  SYSTEM_MONTHLY_POSIT(1,6,TG) =
     +                           SYSTEM_MONTHLY_POSIT(1,6,TG) +
     +                              SYSTEM_HOURLY_GENERATION(HOUR,TG)
                  SYSTEM_MONTHLY_POSIT(1,7,TG) =
     +                           SYSTEM_MONTHLY_POSIT(1,7,TG) +
     +                              SYSTEM_HOURLY_HYDRO(HOUR,TG) +
     +                                 WEEKLY_HYDRO_FOR_HR
                  SYSTEM_MONTHLY_POSIT(1,25,TG) =
     +                           SYSTEM_MONTHLY_POSIT(1,25,TG) +
     +                              SYSTEM_HOURLY_STORAGE_GEN(HOUR,R_TG)
                  SYSTEM_MONTHLY_POSIT(1,8,TG) =
     +                           SYSTEM_MONTHLY_POSIT(1,8,TG) +
     +                            TEMP_POSIT_HYDRO +
     +                            WEEKLY_HYDRO_FOR_HR
                  SYSTEM_MONTHLY_POSIT(1,9,TG) =
     +                           SYSTEM_MONTHLY_POSIT(1,9,TG) +
     +                                              R_TRANS_ROR_CAPACITY
                  SYSTEM_MONTHLY_POSIT(1,10,TG) =
     +                           SYSTEM_MONTHLY_POSIT(1,10,TG) +
     +                               MAX(0.,SYSTEM_DERIVATIVES(HOUR,TG))
                  SYSTEM_MONTHLY_POSIT(1,27,TG) =
     +                           SYSTEM_MONTHLY_POSIT(1,27,TG) +
     +                               MAX(0.,SYSTEM_EMISSIONS(HOUR,TG,1))
                  SYSTEM_MONTHLY_POSIT(1,28,TG) =
     +                           SYSTEM_MONTHLY_POSIT(1,28,TG) +
     +                               MAX(0.,SYSTEM_EMISSIONS(HOUR,TG,2))
                  SYSTEM_MONTHLY_POSIT(1,29,TG) =
     +                           SYSTEM_MONTHLY_POSIT(1,29,TG) +
     +                               MAX(0.,SYSTEM_EMISSIONS(HOUR,TG,3))
                  SYSTEM_MONTHLY_POSIT(1,30,TG) =
     +                           SYSTEM_MONTHLY_POSIT(1,30,TG) +
     +                               MAX(0.,SYSTEM_EMISSIONS(HOUR,TG,4))
                  SYSTEM_MONTHLY_POSIT(1,31,TG) =
     +                           SYSTEM_MONTHLY_POSIT(1,31,TG) +
     +                               MAX(0.,SYSTEM_EMISSIONS(HOUR,TG,5))
                  SYSTEM_MONTHLY_POSIT(1,11,TG) =
     +                           SYSTEM_MONTHLY_POSIT(1,11,TG) +
     +                                     SYSTEM_MARKET_BOUGHT(HOUR,TG)
                  SYSTEM_MONTHLY_POSIT(1,12,TG) =
     +                           SYSTEM_MONTHLY_POSIT(1,12,TG) +
     +                                   SYSTEM_UNSERVED_ENERGY(HOUR,TG)
!                  SYSTEM_MONTHLY_POSIT(1,13,TG) =
!     +                           SYSTEM_MONTHLY_POSIT(1,13,TG) +
!     +                           SYSTEM_MONTHLY_POSIT(1,6,TG) +
!     +                           SYSTEM_MONTHLY_POSIT(1,7,TG) +
!     +                           SYSTEM_MONTHLY_POSIT(1,8,TG) +
!     +                           SYSTEM_MONTHLY_POSIT(1,9,TG) +
!     +                           SYSTEM_MONTHLY_POSIT(1,10,TG) +
!     +                           SYSTEM_MONTHLY_POSIT(1,11,TG) +
!     +                           SYSTEM_MONTHLY_POSIT(1,12,TG)
                  SYSTEM_MONTHLY_POSIT(1,14,TG) =
     +                           MAX(SYSTEM_MONTHLY_POSIT(1,14,TG),
     +                              SYSTEM_HOURLY_LOAD(HOUR,TG) +
     +                              SYSTEM_HOURLY_HYDRO(HOUR,TG)
     +                    + (SYSTEM_HOURLY_STORAGE_GEN(HOUR,R_TG) +
     +                           SYSTEM_HOURLY_STORAGE_PUMP(HOUR,R_TG)))
                  SYSTEM_MONTHLY_POSIT(1,15,TG) =
     +                           MIN(SYSTEM_MONTHLY_POSIT(1,15,TG),
     +                              SYSTEM_HOURLY_LOAD(HOUR,TG) +
     +                              SYSTEM_HOURLY_HYDRO(HOUR,TG)
     +                    + (SYSTEM_HOURLY_STORAGE_GEN(HOUR,R_TG) +
     +                           SYSTEM_HOURLY_STORAGE_PUMP(HOUR,R_TG)))
                  SYSTEM_MONTHLY_POSIT(1,16,TG) =
     +                           SYSTEM_MONTHLY_POSIT(1,16,TG) +
     +                                  SYSTEM_HOURLY_AVAILABLE(HOUR,TG)
                  SYSTEM_MONTHLY_POSIT(1,17,TG) =
     +                           SYSTEM_MONTHLY_POSIT(1,17,TG) +
     +                           SYSTEM_MARKET_COST(HOUR,TG)
                  SYSTEM_MONTHLY_POSIT(1,18,TG) =
     +                           SYSTEM_MONTHLY_POSIT(1,18,TG) +
     +                           LOCAL_DAILY_MARKET_REVENUE
!     +                           SYSTEM_MARKET_REVENUE(HOUR,TG)
                  SYSTEM_MONTHLY_POSIT(1,19,TG) =
     +                           SYSTEM_MONTHLY_POSIT(1,19,TG) +
     +                          SYSTEM_COST_OF_MARKET_SALES(HOUR,R_TG,1)
                  SYSTEM_MONTHLY_POSIT(1,20,TG) =
     +                           SYSTEM_MONTHLY_POSIT(1,20,TG) +
     +                           SYSTEM_NATIVE_COST(HOUR,TG)
!                  SYSTEM_MONTHLY_POSIT(1,21,TG) =
!     +                           SYSTEM_MONTHLY_POSIT(1,21,TG) +
!                  SYSTEM_MONTHLY_POSIT(1,22,TG) =
!     +                           SYSTEM_MONTHLY_POSIT(1,22,TG) +
                  SYSTEM_MONTHLY_POSIT(1,23,TG) =
     +                           SYSTEM_MONTHLY_POSIT(1,23,TG) +
     +                           SYSTEM_SPIN(HOUR,TG)
! END POSITION
               IF(PEAK_HOUR == 1) THEN
!
!
! TRANSACT C POSITION
!
! NEED TO GET HOURLY LOAD AFTER HYDRO.
!
! DEMAND
!
                  PRODUCT_HOURS_POSIT(2) = PRODUCT_HOURS_POSIT(2) + 1.
                  SYSTEM_MONTHLY_POSIT(2,1,TG) =
     +               SYSTEM_MONTHLY_POSIT(2,1,TG) +
     +                          HOURLY_TRANSACTION_LOAD(HOUR,TG) +
     +                                        WEEKLY_HYDRO_FOR_HR

                  SYSTEM_MONTHLY_POSIT(2,2,TG) =
     +                        SYSTEM_MONTHLY_POSIT(2,2,TG) -
     +                               MIN(0.,SYSTEM_DERIVATIVES(HOUR,TG))
                  SYSTEM_MONTHLY_POSIT(2,3,TG) =
     +                           SYSTEM_MONTHLY_POSIT(2,3,TG) +
     +                              SYSTEM_WHOLESALE_GENERATION(HOUR,TG)
                  SYSTEM_MONTHLY_POSIT(2,26,TG) =
     +                           SYSTEM_MONTHLY_POSIT(2,26,TG) -
     +                             SYSTEM_HOURLY_STORAGE_PUMP(HOUR,R_TG)
                  SYSTEM_MONTHLY_POSIT(2,4,TG) =
     +                           SYSTEM_MONTHLY_POSIT(2,4,TG) +
     +                                                   DUMP_ENERGY(HR)

! SUPPLY
                  SYSTEM_MONTHLY_POSIT(2,6,TG) =
     +                           SYSTEM_MONTHLY_POSIT(2,6,TG) +
     +                              SYSTEM_HOURLY_GENERATION(HOUR,TG)
                  SYSTEM_MONTHLY_POSIT(2,7,TG) =
     +                           SYSTEM_MONTHLY_POSIT(2,7,TG) +
     +                              SYSTEM_HOURLY_HYDRO(HOUR,TG) +
     +                              WEEKLY_HYDRO_FOR_HR

                  SYSTEM_MONTHLY_POSIT(2,25,TG) =
     +                           SYSTEM_MONTHLY_POSIT(2,25,TG) +
     +                              SYSTEM_HOURLY_STORAGE_GEN(HOUR,R_TG)
                  SYSTEM_MONTHLY_POSIT(2,8,TG) =
     +                           SYSTEM_MONTHLY_POSIT(2,8,TG) +
     +                                 TEMP_POSIT_HYDRO +
     +                                 WEEKLY_HYDRO_FOR_HR
                  SYSTEM_MONTHLY_POSIT(2,9,TG) =
     +                           SYSTEM_MONTHLY_POSIT(2,9,TG) +
     +                                              R_TRANS_ROR_CAPACITY
                  SYSTEM_MONTHLY_POSIT(2,10,TG) =
     +                           SYSTEM_MONTHLY_POSIT(2,10,TG) +
     +                               MAX(0.,SYSTEM_DERIVATIVES(HOUR,TG))
                  SYSTEM_MONTHLY_POSIT(2,27,TG) =
     +                           SYSTEM_MONTHLY_POSIT(2,27,TG) +
     +                               MAX(0.,SYSTEM_EMISSIONS(HOUR,TG,1))
                  SYSTEM_MONTHLY_POSIT(2,28,TG) =
     +                           SYSTEM_MONTHLY_POSIT(2,28,TG) +
     +                               MAX(0.,SYSTEM_EMISSIONS(HOUR,TG,2))
                  SYSTEM_MONTHLY_POSIT(2,29,TG) =
     +                           SYSTEM_MONTHLY_POSIT(2,29,TG) +
     +                               MAX(0.,SYSTEM_EMISSIONS(HOUR,TG,3))
                  SYSTEM_MONTHLY_POSIT(2,30,TG) =
     +                           SYSTEM_MONTHLY_POSIT(2,30,TG) +
     +                               MAX(0.,SYSTEM_EMISSIONS(HOUR,TG,4))
                  SYSTEM_MONTHLY_POSIT(2,31,TG) =
     +                           SYSTEM_MONTHLY_POSIT(2,31,TG) +
     +                               MAX(0.,SYSTEM_EMISSIONS(HOUR,TG,5))
                  SYSTEM_MONTHLY_POSIT(2,11,TG) =
     +                           SYSTEM_MONTHLY_POSIT(2,11,TG) +
     +                                     SYSTEM_MARKET_BOUGHT(HOUR,TG)
                  SYSTEM_MONTHLY_POSIT(2,12,TG) =
     +                           SYSTEM_MONTHLY_POSIT(2,12,TG) +
     +                                   SYSTEM_UNSERVED_ENERGY(HOUR,TG)
!                  SYSTEM_MONTHLY_POSIT(2,13,TG) =
!     +                           SYSTEM_MONTHLY_POSIT(2,13,TG) +
!     +                           SYSTEM_MONTHLY_POSIT(2,6,TG) +
!     +                           SYSTEM_MONTHLY_POSIT(2,7,TG) +
!     +                           SYSTEM_MONTHLY_POSIT(2,8,TG) +
!     +                           SYSTEM_MONTHLY_POSIT(2,9,TG) +
!     +                           SYSTEM_MONTHLY_POSIT(2,10,TG) +
!     +                           SYSTEM_MONTHLY_POSIT(2,11,TG) +
!     +                           SYSTEM_MONTHLY_POSIT(2,12,TG)
                  SYSTEM_MONTHLY_POSIT(2,14,TG) =
     +                           MAX(SYSTEM_MONTHLY_POSIT(2,14,TG),
     +                              SYSTEM_HOURLY_LOAD(HOUR,TG) +
     +                              SYSTEM_HOURLY_HYDRO(HOUR,TG)
     +                    + (SYSTEM_HOURLY_STORAGE_GEN(HOUR,R_TG) +
     +                           SYSTEM_HOURLY_STORAGE_PUMP(HOUR,R_TG)))
                  SYSTEM_MONTHLY_POSIT(2,15,TG) =
     +                           MIN(SYSTEM_MONTHLY_POSIT(2,15,TG),
     +                              SYSTEM_HOURLY_LOAD(HOUR,TG) +
     +                              SYSTEM_HOURLY_HYDRO(HOUR,TG)
     +                    + (SYSTEM_HOURLY_STORAGE_GEN(HOUR,R_TG) +
     +                           SYSTEM_HOURLY_STORAGE_PUMP(HOUR,R_TG)))
                  SYSTEM_MONTHLY_POSIT(2,16,TG) =
     +                           SYSTEM_MONTHLY_POSIT(2,16,TG) +
     +                                  SYSTEM_HOURLY_AVAILABLE(HOUR,TG)
                  SYSTEM_MONTHLY_POSIT(2,17,TG) =
     +                           SYSTEM_MONTHLY_POSIT(2,17,TG) +
     +                           SYSTEM_MARKET_COST(HOUR,TG)
                  SYSTEM_MONTHLY_POSIT(2,18,TG) =
     +                           SYSTEM_MONTHLY_POSIT(2,18,TG) +
     +                           LOCAL_DAILY_MARKET_REVENUE
!     +                           SYSTEM_MARKET_REVENUE(HOUR,TG)
                  SYSTEM_MONTHLY_POSIT(2,19,TG) =
     +                           SYSTEM_MONTHLY_POSIT(2,19,TG) +
     +                          SYSTEM_COST_OF_MARKET_SALES(HOUR,R_TG,1)
                  SYSTEM_MONTHLY_POSIT(2,20,TG) =
     +                           SYSTEM_MONTHLY_POSIT(2,20,TG) +
     +                           SYSTEM_NATIVE_COST(HOUR,TG)
!                  SYSTEM_MONTHLY_POSIT(2,21,TG) =
!     +                           SYSTEM_MONTHLY_POSIT(2,21,TG) +
!                  SYSTEM_MONTHLY_POSIT(2,22,TG) =
!     +                           SYSTEM_MONTHLY_POSIT(2,22,TG) +
                  SYSTEM_MONTHLY_POSIT(2,23,TG) =
     +                           SYSTEM_MONTHLY_POSIT(2,23,TG) +
     +                           SYSTEM_SPIN(HOUR,TG)
! END POSITION
                  SYSTEM_MONTHLY_MISC(2,TG) =
     +                           SYSTEM_MONTHLY_MISC(2,TG) +
     +                              SYSTEM_WHOLESALE_GENERATION(HOUR,TG)
                  SYSTEM_MONTHLY_MISC(4,TG) =
     +                           SYSTEM_MONTHLY_MISC(4,TG) +
     +                              SYSTEM_MARKET_BOUGHT(HOUR,TG)
                  SYSTEM_MONTHLY_MISC(6,TG) =
     +                           SYSTEM_MONTHLY_MISC(6,TG) +
     +                              SYSTEM_MARKET_REVENUE(HOUR,TG)
                  SYSTEM_MONTHLY_MISC(8,TG) =
     +                           SYSTEM_MONTHLY_MISC(8,TG) +
     +                                   SYSTEM_MARKET_COST(HOUR,TG)
                  IF(SYSTEM_MARKET_BOUGHT(HOUR,TG) > .01) THEN
                     AVE_BUY = SYSTEM_MARKET_COST(HOUR,TG)/
     +                                     SYSTEM_MARKET_BOUGHT(HOUR,TG)
                     AVE_BUY = AVE_BUY
                  ENDIF
!
                  SYSTEM_MONTHLY_MISC(24,TG) =
     +                  SYSTEM_MONTHLY_MISC(24,TG) +
     +                          SYSTEM_COST_OF_MARKET_SALES(HOUR,R_TG,2)
               ELSE
!
! TRANSACT C POSITION
!
! NEED TO GET HOURLY LOAD AFTER HYDRO.
!
! DEMAND
!
                  PRODUCT_HOURS_POSIT(3) = PRODUCT_HOURS_POSIT(3) + 1.
                  SYSTEM_MONTHLY_POSIT(3,1,TG) =
     +               SYSTEM_MONTHLY_POSIT(3,1,TG) +
     +                          HOURLY_TRANSACTION_LOAD(HOUR,TG) +
     +                                        WEEKLY_HYDRO_FOR_HR

                  SYSTEM_MONTHLY_POSIT(3,2,TG) =
     +                        SYSTEM_MONTHLY_POSIT(3,2,TG) -
     +                               MIN(0.,SYSTEM_DERIVATIVES(HOUR,TG))
                  SYSTEM_MONTHLY_POSIT(3,3,TG) =
     +                           SYSTEM_MONTHLY_POSIT(3,3,TG) +
     +                              SYSTEM_WHOLESALE_GENERATION(HOUR,TG)
                  SYSTEM_MONTHLY_POSIT(3,26,TG) =
     +                           SYSTEM_MONTHLY_POSIT(3,26,TG) -
     +                             SYSTEM_HOURLY_STORAGE_PUMP(HOUR,R_TG)
                  SYSTEM_MONTHLY_POSIT(3,4,TG) =
     +                           SYSTEM_MONTHLY_POSIT(3,4,TG) +
     +                                                   DUMP_ENERGY(HR)

! SUPPLY
                  SYSTEM_MONTHLY_POSIT(3,6,TG) =
     +                           SYSTEM_MONTHLY_POSIT(3,6,TG) +
     +                              SYSTEM_HOURLY_GENERATION(HOUR,TG)
                  SYSTEM_MONTHLY_POSIT(3,7,TG) =
     +                           SYSTEM_MONTHLY_POSIT(3,7,TG) +
     +                              SYSTEM_HOURLY_HYDRO(HOUR,TG) +
     +                              WEEKLY_HYDRO_FOR_HR
                  SYSTEM_MONTHLY_POSIT(3,25,TG) =
     +                           SYSTEM_MONTHLY_POSIT(3,25,TG) +
     +                              SYSTEM_HOURLY_STORAGE_GEN(HOUR,R_TG)
                  SYSTEM_MONTHLY_POSIT(3,8,TG) =
     +                           SYSTEM_MONTHLY_POSIT(3,8,TG) +
     +                                 TEMP_POSIT_HYDRO +
     +                                 WEEKLY_HYDRO_FOR_HR
                  SYSTEM_MONTHLY_POSIT(3,9,TG) =
     +                           SYSTEM_MONTHLY_POSIT(3,9,TG) +
     +                                              R_TRANS_ROR_CAPACITY
                  SYSTEM_MONTHLY_POSIT(3,10,TG) =
     +                           SYSTEM_MONTHLY_POSIT(3,10,TG) +
     +                               MAX(0.,SYSTEM_DERIVATIVES(HOUR,TG))
                  SYSTEM_MONTHLY_POSIT(3,27,TG) =
     +                           SYSTEM_MONTHLY_POSIT(3,27,TG) +
     +                               MAX(0.,SYSTEM_EMISSIONS(HOUR,TG,1))
                  SYSTEM_MONTHLY_POSIT(3,28,TG) =
     +                           SYSTEM_MONTHLY_POSIT(3,28,TG) +
     +                               MAX(0.,SYSTEM_EMISSIONS(HOUR,TG,2))
                  SYSTEM_MONTHLY_POSIT(3,29,TG) =
     +                           SYSTEM_MONTHLY_POSIT(3,29,TG) +
     +                               MAX(0.,SYSTEM_EMISSIONS(HOUR,TG,3))
                  SYSTEM_MONTHLY_POSIT(3,30,TG) =
     +                           SYSTEM_MONTHLY_POSIT(3,30,TG) +
     +                               MAX(0.,SYSTEM_EMISSIONS(HOUR,TG,4))
                  SYSTEM_MONTHLY_POSIT(3,31,TG) =
     +                           SYSTEM_MONTHLY_POSIT(3,31,TG) +
     +                               MAX(0.,SYSTEM_EMISSIONS(HOUR,TG,5))
                  SYSTEM_MONTHLY_POSIT(3,11,TG) =
     +                           SYSTEM_MONTHLY_POSIT(3,11,TG) +
     +                                     SYSTEM_MARKET_BOUGHT(HOUR,TG)
                  SYSTEM_MONTHLY_POSIT(3,12,TG) =
     +                           SYSTEM_MONTHLY_POSIT(3,12,TG) +
     +                                   SYSTEM_UNSERVED_ENERGY(HOUR,TG)
!                  SYSTEM_MONTHLY_POSIT(3,13,TG) =
!     +                           SYSTEM_MONTHLY_POSIT(3,13,TG) +
!     +                           SYSTEM_MONTHLY_POSIT(3,6,TG) +
!     +                           SYSTEM_MONTHLY_POSIT(3,7,TG) +
!     +                           SYSTEM_MONTHLY_POSIT(3,8,TG) +
!     +                           SYSTEM_MONTHLY_POSIT(3,9,TG) +
!     +                           SYSTEM_MONTHLY_POSIT(3,10,TG) +
!     +                           SYSTEM_MONTHLY_POSIT(3,11,TG) +
!     +                           SYSTEM_MONTHLY_POSIT(3,12,TG)
                  SYSTEM_MONTHLY_POSIT(3,14,TG) =
     +                           MAX(SYSTEM_MONTHLY_POSIT(3,14,TG),
     +                              SYSTEM_HOURLY_LOAD(HOUR,TG) +
     +                              SYSTEM_HOURLY_HYDRO(HOUR,TG)
     +                    + (SYSTEM_HOURLY_STORAGE_GEN(HOUR,R_TG) +
     +                           SYSTEM_HOURLY_STORAGE_PUMP(HOUR,R_TG)))
                  SYSTEM_MONTHLY_POSIT(3,15,TG) =
     +                           MIN(SYSTEM_MONTHLY_POSIT(3,15,TG),
     +                              SYSTEM_HOURLY_LOAD(HOUR,TG) +
     +                              SYSTEM_HOURLY_HYDRO(HOUR,TG)
     +                    + (SYSTEM_HOURLY_STORAGE_GEN(HOUR,R_TG) +
     +                           SYSTEM_HOURLY_STORAGE_PUMP(HOUR,R_TG)))
                  SYSTEM_MONTHLY_POSIT(3,16,TG) =
     +                           SYSTEM_MONTHLY_POSIT(3,16,TG) +
     +                                  SYSTEM_HOURLY_AVAILABLE(HOUR,TG)
                  SYSTEM_MONTHLY_POSIT(3,17,TG) =
     +                           SYSTEM_MONTHLY_POSIT(3,17,TG) +
     +                           SYSTEM_MARKET_COST(HOUR,TG)
                  SYSTEM_MONTHLY_POSIT(3,18,TG) =
     +                           SYSTEM_MONTHLY_POSIT(3,18,TG) +
     +                           LOCAL_DAILY_MARKET_REVENUE
!     +                           SYSTEM_MARKET_REVENUE(HOUR,TG)
                  SYSTEM_MONTHLY_POSIT(3,19,TG) =
     +                           SYSTEM_MONTHLY_POSIT(3,19,TG) +
     +                          SYSTEM_COST_OF_MARKET_SALES(HOUR,R_TG,1)
                  SYSTEM_MONTHLY_POSIT(3,20,TG) =
     +                           SYSTEM_MONTHLY_POSIT(3,20,TG) +
     +                           SYSTEM_NATIVE_COST(HOUR,TG)
!                  SYSTEM_MONTHLY_POSIT(3,21,TG) =
!     +                           SYSTEM_MONTHLY_POSIT(3,21,TG) +
!                  SYSTEM_MONTHLY_POSIT(3,22,TG) =
!     +                           SYSTEM_MONTHLY_POSIT(3,22,TG) +
                  SYSTEM_MONTHLY_POSIT(3,23,TG) =
     +                           SYSTEM_MONTHLY_POSIT(3,23,TG) +
     +                           SYSTEM_SPIN(HOUR,TG)
! END POSITION
                  SYSTEM_MONTHLY_MISC(3,TG) =
     +                           SYSTEM_MONTHLY_MISC(3,TG) +
     +                              SYSTEM_WHOLESALE_GENERATION(HOUR,TG)
                  SYSTEM_MONTHLY_MISC(5,TG) =
     +                           SYSTEM_MONTHLY_MISC(5,TG) +
     +                              SYSTEM_MARKET_BOUGHT(HOUR,TG)
                  SYSTEM_MONTHLY_MISC(7,TG) =
     +                           SYSTEM_MONTHLY_MISC(7,TG) +
     +                              SYSTEM_MARKET_REVENUE(HOUR,TG)
                  SYSTEM_MONTHLY_MISC(9,TG) =
     +                           SYSTEM_MONTHLY_MISC(9,TG) +
     +                                   SYSTEM_MARKET_COST(HOUR,TG)
!
                  SYSTEM_MONTHLY_MISC(25,TG) =
     +                  SYSTEM_MONTHLY_MISC(25,TG) +
     +                          SYSTEM_COST_OF_MARKET_SALES(HOUR,R_TG,3)
               ENDIF ! PEAK_HOUR
               SYSTEM_MONTHLY_MISC(10,TG) = SYSTEM_MONTHLY_MISC(10,TG) +
     +                                                   DUMP_ENERGY(HR)
               SYSTEM_MONTHLY_MISC(11,TG) =
     +                     MAX(SYSTEM_HOURLY_LOAD(HOUR,TG),
     +                                       SYSTEM_MONTHLY_MISC(11,TG))
               SYSTEM_MONTHLY_MISC(12,TG) =
     +                     MIN(SYSTEM_HOURLY_LOAD(HOUR,TG),
     +                                       SYSTEM_MONTHLY_MISC(12,TG))
               SYSTEM_MONTHLY_MISC(13,TG) = SYSTEM_MONTHLY_MISC(13,TG) +
     +                                 TEMP_POSIT_HYDRO
               SYSTEM_MONTHLY_MISC(14,TG) = SYSTEM_MONTHLY_MISC(14,TG) +
     +                                              R_TRANS_ROR_CAPACITY
! NEED SEPARATE STREAM FOR BUYS AND SELLS
               IF(SYSTEM_DERIVATIVES(HOUR,TG) > 0.) THEN
                  SYSTEM_MONTHLY_MISC(18,TG) =
     +                        SYSTEM_MONTHLY_MISC(18,TG) +
     +                                       SYSTEM_DERIVATIVES(HOUR,TG)
               ELSE
                  SYSTEM_MONTHLY_MISC(17,TG) =
     +                        SYSTEM_MONTHLY_MISC(17,TG) -
     +                                       SYSTEM_DERIVATIVES(HOUR,TG)
               ENDIF
!
               REMAIN = MOD(FLOAT(HOUR),24.)
               IF(REMAIN < .001) THEN ! NEW DAY
!                  DAY = HOUR/24
                  HR = 1
               ELSE
!                  DAY = HOUR/24 + 1
                  HR = HR + 1
               ENDIF
!
            ENDDO ! HOURS IN MONTH
!
            SYSTEM_MARKET_PRICE(0,0) =
     +                           SYSTEM_MARKET_PRICE(0,0) +
     +                              SYSTEM_MARKET_PRICE(0,TG)
            SYSTEM_EMERGENCY_CAPACITY(0,0) =
     +                           SYSTEM_EMERGENCY_CAPACITY(0,0) +
     +                              SYSTEM_EMERGENCY_CAPACITY(0,TG)
            SYSTEM_HOURLY_GENERATION(0,0) =
     +                           SYSTEM_HOURLY_GENERATION(0,0) +
     +                              SYSTEM_HOURLY_GENERATION(0,TG)
            SYSTEM_HOURLY_COMMITMENT(0,0) =
     +                           SYSTEM_HOURLY_COMMITMENT(0,0) +
     +                              SYSTEM_HOURLY_COMMITMENT(0,TG)
            SYSTEM_HOURLY_DECOMMITTED(0,0) =
     +                           SYSTEM_HOURLY_DECOMMITTED(0,0) +
     +                              SYSTEM_HOURLY_DECOMMITTED(0,TG)
            SYSTEM_HOURLY_PRE_COMMITMENT(0,0) =
     +                           SYSTEM_HOURLY_PRE_COMMITMENT(0,0) +
     +                             SYSTEM_HOURLY_PRE_COMMITMENT(0,TG)
            SYSTEM_HOURLY_AVAILABLE(0,0) =
     +                           SYSTEM_HOURLY_AVAILABLE(0,0) +
     +                              SYSTEM_HOURLY_AVAILABLE(0,TG)
            SYSTEM_HOURLY_COMMIT_AVAIL(0,0) =
     +                           SYSTEM_HOURLY_COMMIT_AVAIL(0,0) +
     +                              SYSTEM_HOURLY_COMMIT_AVAIL(0,TG)
            SYSTEM_HOURLY_LOAD(0,0) =
     +                           SYSTEM_HOURLY_LOAD(0,0) +
     +                              SYSTEM_HOURLY_LOAD(0,TG)
            SYSTEM_HOURLY_HYDRO(0,0) =
     +                           SYSTEM_HOURLY_HYDRO(0,0) +
     +                              SYSTEM_HOURLY_HYDRO(0,TG)
            SYSTEM_HOURLY_MC(0,0) =
     +                           SYSTEM_HOURLY_MC(0,0) +
     +                              SYSTEM_HOURLY_MC(0,TG)
            SYSTEM_HOURLY_MC_AT_LOAD(0,0) =
     +                           SYSTEM_HOURLY_MC_AT_LOAD(0,0) +
     +                              SYSTEM_HOURLY_MC_AT_LOAD(0,TG)
            SYSTEM_WHOLESALE_GENERATION(0,0) =
     +                           SYSTEM_WHOLESALE_GENERATION(0,0) +
     +                              SYSTEM_WHOLESALE_GENERATION(0,TG)
            SYSTEM_NATIVE_COST(0,0) =
     +                           SYSTEM_NATIVE_COST(0,0) +
     +                              SYSTEM_NATIVE_COST(0,TG)
            SYSTEM_UNSERVED_ENERGY(0,0) =
     +                           SYSTEM_UNSERVED_ENERGY(0,0) +
     +                              SYSTEM_UNSERVED_ENERGY(0,TG)
            SYSTEM_ENERGY_ABOVE(0,0) =
     +                           SYSTEM_ENERGY_ABOVE(0,0) +
     +                              SYSTEM_ENERGY_ABOVE(0,TG)
            SYSTEM_DERIVATIVES(0,0) =
     +                           SYSTEM_DERIVATIVES(0,0) +
     +                              SYSTEM_DERIVATIVES(0,TG)
            SYSTEM_STORAGE(0,0) =
     +                           SYSTEM_STORAGE(0,0) +
     +                              SYSTEM_STORAGE(0,TG)
            SYSTEM_AVAIL_DERIVATIVES(0,0) =
     +                           SYSTEM_AVAIL_DERIVATIVES(0,0) +
     +                              SYSTEM_AVAIL_DERIVATIVES(0,TG)
            SYSTEM_AVAIL_STORAGE(0,0) =
     +                           SYSTEM_AVAIL_STORAGE(0,0) +
     +                              SYSTEM_AVAIL_STORAGE(0,TG)
            SYSTEM_SPIN(0,0) =
     +                           SYSTEM_SPIN(0,0) +
     +                              SYSTEM_SPIN(0,TG)
            SYSTEM_MARKET_BOUGHT(0,0) =
     +                           SYSTEM_MARKET_BOUGHT(0,0) +
     +                              SYSTEM_MARKET_BOUGHT(0,TG)
            SYSTEM_DSM_MW(0,0) =
     +                           SYSTEM_DSM_MW(0,0) +
     +                              SYSTEM_DSM_MW(0,TG)
            SYSTEM_MARKET_REVENUE(0,0) =
     +                           SYSTEM_MARKET_REVENUE(0,0) +
     +                                  SYSTEM_MARKET_REVENUE(0,TG)
            SYSTEM_MARKET_COST(0,0) =
     +                           SYSTEM_MARKET_COST(0,0) +
     +                                  SYSTEM_MARKET_COST(0,TG)
!
! TRANSACT INPUT
!
            SYSTEM_MONTHLY_MISC(1,TG) = GET_MONTHLY_TL_MWH(TG) +
     +                                   GET_WH_MONTH_ENERGY(R_MONTH,TG)
! TOTAL DEMAND
            SYSTEM_MONTHLY_MISC(15,TG) =
     +                           SYSTEM_MONTHLY_MISC(1,TG) +
     +                           SYSTEM_WHOLESALE_GENERATION(0,TG) +
     +                           SYSTEM_MONTHLY_MISC(10,TG) +
     +                           SYSTEM_MONTHLY_MISC(17,TG)
! TOTAL SUPPLY
            SYSTEM_MONTHLY_MISC(16,TG) =
     +                           SYSTEM_HOURLY_GENERATION(0,TG) +
     +                           SYSTEM_HOURLY_HYDRO(0,TG) +
     +                           SYSTEM_MARKET_BOUGHT(0,TG) +
     +                           SYSTEM_UNSERVED_ENERGY(0,TG) +
     +                           SYSTEM_MONTHLY_MISC(18,TG)
!
! SYSTEM MISC 26-33 ARE CAPACITY MARKET VARIABLES
!
            SYSTEM_MONTHLY_MISC(26,TG) = GET_CL_TG_CAP_MARKET_MW(TG)
            SYSTEM_MONTHLY_MISC(27,TG) = GET_TF_TG_CAP_MARKET_MW(TG)
            SYSTEM_MONTHLY_MISC(28,TG) = GET_ANNUAL_INTER_CAPACITY(TG)
            SYSTEM_MONTHLY_MISC(29,TG) =
     +            SYSTEM_MONTHLY_MISC(26,TG) -
     +                     SYSTEM_MONTHLY_MISC(27,TG) +
     +                                 SYSTEM_MONTHLY_MISC(28,TG)
!
            IF(ABS(SYSTEM_MONTHLY_MISC(27,TG) -
     +                           SYSTEM_MONTHLY_MISC(28,TG)) > .01) THEN
               SYSTEM_MONTHLY_MISC(30,TG) = 100.*
     +            (SYSTEM_MONTHLY_MISC(26,TG)/
     +                  (SYSTEM_MONTHLY_MISC(27,TG) -
     +                           SYSTEM_MONTHLY_MISC(28,TG)) - 1.)
            ELSE
               SYSTEM_MONTHLY_MISC(30,TG) = 0.0
            ENDIF
!
            SYSTEM_MONTHLY_MISC(31,TG) = GET_CL_TG_CAP_MARKET_REV(TG) *
     +                                                          0.000001
            SYSTEM_MONTHLY_MISC(32,TG) = GET_TF_TG_CAP_MARKET_COST(TG)*
     +                                                             0.001
            SYSTEM_MONTHLY_MISC(33,TG) =
     +                     SYSTEM_MONTHLY_MISC(31,TG) -
     +                                        SYSTEM_MONTHLY_MISC(32,TG)
!
            DO I = 1, NO_MISC_VARS
               IF(I == 30) THEN
                  SYSTEM_MONTHLY_MISC(30,0) = 100.*
     +               (SYSTEM_MONTHLY_MISC(26,0)/
     +                  (SYSTEM_MONTHLY_MISC(27,0) -
     +                           SYSTEM_MONTHLY_MISC(28,0)) - 1.)
               ELSE
                  SYSTEM_MONTHLY_MISC(I,0) =
     +                        SYSTEM_MONTHLY_MISC(I,0) +
     +                                         SYSTEM_MONTHLY_MISC(I,TG)
               ENDIF
            ENDDO
!
! 020807.
!
            DO I = 1, 3
! DEMAND
               SYSTEM_MONTHLY_POSIT(I,5,TG) =
     +                           SYSTEM_MONTHLY_POSIT(I,5,TG) +
     +                           SYSTEM_MONTHLY_POSIT(I,1,TG) +
     +                           SYSTEM_MONTHLY_POSIT(I,2,TG) +
     +                           SYSTEM_MONTHLY_POSIT(I,3,TG) +
     +                           SYSTEM_MONTHLY_POSIT(I,4,TG) +
     +                           SYSTEM_MONTHLY_POSIT(I,26,TG)
! SUPPLY
               SYSTEM_MONTHLY_POSIT(I,13,TG) =
     +                           SYSTEM_MONTHLY_POSIT(I,13,TG) +
     +                           SYSTEM_MONTHLY_POSIT(I,6,TG) +
     +                           SYSTEM_MONTHLY_POSIT(I,7,TG) +
!     +                           SYSTEM_MONTHLY_POSIT(I,8,TG) +
!     +                           SYSTEM_MONTHLY_POSIT(I,9,TG) +
     +                           SYSTEM_MONTHLY_POSIT(I,10,TG) +
     +                           SYSTEM_MONTHLY_POSIT(I,11,TG) +
     +                           SYSTEM_MONTHLY_POSIT(I,12,TG) +
     +                           SYSTEM_MONTHLY_POSIT(I,25,TG)
! EQUIVALENT THERMAL CAPACITY
               IF(SYSTEM_MONTHLY_POSIT(I,16,TG) > .1 .AND.
     +                                 PRODUCT_HOURS_POSIT(I) > .1) THEN
                  SYSTEM_MONTHLY_POSIT(I,16,TG) =
     +                  SYSTEM_MONTHLY_POSIT(I,16,TG)/
     +                  PRODUCT_HOURS_POSIT(I)
!               ELSE
!                  SYSTEM_MONTHLY_POSIT(I,16,TG) = 0.
               ENDIF
!
               IF(PRODUCT_HOURS_POSIT(I) > .1) THEN
                  SYSTEM_MONTHLY_POSIT(I,24,TG) =
     +                           SYSTEM_MONTHLY_POSIT(I,1,TG)/
     +                                            PRODUCT_HOURS_POSIT(I)
               ENDIF
!
! 051507.
!
               IF(SYSTEM_MONTHLY_POSIT(I,11,TG) > .1) THEN
                  SYSTEM_MONTHLY_POSIT(I,21,TG) =
     +               SYSTEM_MONTHLY_POSIT(I,17,TG)/
     +                         SYSTEM_MONTHLY_POSIT(I,11,TG)
               ENDIF
               IF(SYSTEM_MONTHLY_POSIT(I,3,TG) > .1) THEN
                  SYSTEM_MONTHLY_POSIT(I,22,TG) =
     +               SYSTEM_MONTHLY_POSIT(I,18,TG)/
     +                         SYSTEM_MONTHLY_POSIT(I,3,TG)
               ENDIF
               SYSTEM_MONTHLY_POSIT(I,17,TG) =
     +                            SYSTEM_MONTHLY_POSIT(I,17,TG)*0.000001
               SYSTEM_MONTHLY_POSIT(I,18,TG) =
     +                            SYSTEM_MONTHLY_POSIT(I,18,TG)*0.000001
               SYSTEM_MONTHLY_POSIT(I,20,TG) =
     +                            SYSTEM_MONTHLY_POSIT(I,20,TG)*0.000001
            END DO
!
            SAVE_TRANSACT_C_STATUS = .TRUE.
!
!           I KNOW:
!
!              1. MONTHLY_STARTS
!              2. MONTHLY_STOPS
!              3. DAILY_OPTION_STATUS
!              4. HOURLY, DAILY, WEEKLY, MONTHLY OPERATION OF EACH UNIT
!           5. HOURLY, DAILY, WEEKLY, MONTHLY PROFITABILITY OF EACH UNIT
!
         ELSEIF(TYPE_OF_OPTION == 2) THEN
!
!        ASSUME FINANCIAL FROM THE ENERGY PRODUCTS FILE
!
         ELSE
!            WRITE(6,*) "UNKNOWN TYPE OF DAILY OPTION"
            CALL MG_LOCATE_WRITE(20,0,"UNKNOWN TYPE OF DAILY OPTION",
     +                                                   ALL_VERSIONS,0)
            er_message='Stop requested from TF_OBJT2 SIID301'
            call end_program(er_message)
         ENDIF

         IF(.NOT. LAHEY_LF95()) CALL MG_LOCATE_WRITE(17,9,
     +                        "                        ",ALL_VERSIONS,0)
!
         DAILY_OPTION_FOR_MONTH = .TRUE.
!
      RETURN

      ENTRY TRANC_JDA_LOGIC(R_HOURS_IN_MONTH,R_MONTH,R_CURRENT_YEAR,
     +                      R_MONTH_NAME)

!
!
!
         TRANC_JDA_LOGIC = .FALSE.
         RETURN
         IF(ALLOCATED(JOINT_DISPATCH_ORDER))
     +                                  DEALLOCATE(JOINT_DISPATCH_ORDER)
!
         MAX_NBLOK = MAX_BLOCKS_PER_TRANS(1) + MAX_BLOCKS_PER_TRANS(2)
         ALLOCATE(JOINT_DISPATCH_ORDER(MAX_NBLOK,4))
!
         LOCAL_NUNITS = GET_NUNITS()
!
         IF(ALLOCATED(COMBINED_GEN_IN_SYSTEM))
     +                                DEALLOCATE(COMBINED_GEN_IN_SYSTEM,
     +                                              COMBINED_HEAT_PARAM)
         ALLOCATE(COMBINED_GEN_IN_SYSTEM(2,LOCAL_NUNITS),
     +               COMBINED_HEAT_PARAM(2,LOCAL_NUNITS))
         COMBINED_GEN_IN_SYSTEM = 0.
         COMBINED_HEAT_PARAM = 0.
!
         HOURLY_JDA_REPORT = YES_HOURLY_JDA_REPORT()
!
         IF(HOUR_JDA_REPORT_NOT_OPEN  .AND.
     +                                     YES_HOURLY_JDA_REPORT()) THEN
            HOUR_JDA_REPORT_NOT_OPEN = .FALSE.
            HOUR_JDA_NUM = 32
            HOUR_JDA_UNIT = HOUR_JDA_RPT_HEADER(HOUR_JDA_NUM,
     +                                                    HOUR_JDA_REC)
         ENDIF
         MONTHLY_JDA_GENERATION(0,0) = 0.
         MONTHLY_JDA_GENERATION(0,1) = 0.
         MONTHLY_JDA_GENERATION(0,2) = 0.
         MONTHLY_JDA_GENERATION(0,3) = 0.
         MONTHLY_JDA_GENERATION(1,0) = 0.
         MONTHLY_JDA_GENERATION(1,1) = 0.
         MONTHLY_JDA_GENERATION(1,2) = 0.
         MONTHLY_JDA_GENERATION(1,3) = 0.
         MONTHLY_JDA_GENERATION(2,0) = 0.
         MONTHLY_JDA_GENERATION(2,1) = 0.
         MONTHLY_JDA_GENERATION(2,2) = 0.
         MONTHLY_JDA_GENERATION(2,3) = 0.
!
         MONTHLY_JDA_PURCHASES(0) = 0.
         MONTHLY_JDA_PURCHASES(1) = 0.
         MONTHLY_JDA_PURCHASES(2) = 0.
!
         MONTHLY_JDA_MUST_RUN(1) = 0.
         MONTHLY_JDA_MUST_RUN(2) = 0.
!
         MONTHLY_JDA_ENERGY_BALANCE(1) = 0.
         MONTHLY_JDA_ENERGY_BALANCE(2) = 0.
!
         MONTHLY_JDA_COST(1) = 0.
         MONTHLY_JDA_COST(2) = 0.
!
         JDA_TOTAL_COMBINED_LOAD = 0.
!
         DO TG = 0, 2
            SYSTEM_HOURLY_GENERATION(0,TG) = 0.
            SYSTEM_WHOLESALE_GENERATION(0,TG) = 0.
            SYSTEM_MARKET_BOUGHT(0,TG) = 0.
            SYSTEM_MARKET_REVENUE(0,TG) = 0.
            SYSTEM_MARKET_COST(0,TG) = 0.
            SYSTEM_MONTHLY_MISC(2,TG) = 0.
            SYSTEM_MONTHLY_MISC(4,TG) = 0.
            SYSTEM_MONTHLY_MISC(6,TG) = 0.
            SYSTEM_MONTHLY_MISC(8,TG) = 0.
            SYSTEM_MONTHLY_MISC(3,TG) = 0.
            SYSTEM_MONTHLY_MISC(5,TG) = 0.
            SYSTEM_MONTHLY_MISC(7,TG) = 0.
            SYSTEM_MONTHLY_MISC(9,TG) = 0.
            SYSTEM_MONTHLY_MISC(10,TG) = 0.
            SYSTEM_MONTHLY_MISC(15,TG) = 0.
            SYSTEM_MONTHLY_MISC(16,TG) = 0.
         END DO
!
         TG = 1 ! ASSUME THE SAME FOR BOTH.
!
         PEAK_HOUR = 1
!
         ON_PEAK_BUY_SPREAD = PATH_SPREAD_RATE(MARKET_TG,TG,
     +   PEAK_HOUR,R_MONTH,globecom_YEAR)
         ON_PEAK_SELL_SPREAD = PATH_SPREAD_RATE(TG,MARKET_TG,
     +        PEAK_HOUR,R_MONTH,globecom_YEAR)
         ON_PEAK_BUY_WHEEL =
     +              MAX(PATH_WHEELING_CHARGE(MARKET_TG,TG,
     +              PEAK_HOUR,R_MONTH,globecom_YEAR),
     +                      ON_PEAK_BUY_SPREAD)
         ON_PEAK_SELL_WHEEL = MAX(PATH_WHEELING_CHARGE(TG,
     +                                                MARKET_TG,
     +                     PEAK_HOUR,R_MONTH,globecom_YEAR),
     +                                              ON_PEAK_SELL_SPREAD)
         ON_PEAK_SECOND_BUY_WHEEL =
     +              MAX(PATH_WHEELING_CHARGE(SECOND_MARKET_TG,TG,
     +                  PEAK_HOUR,R_MONTH,globecom_YEAR),
     +                           ON_PEAK_BUY_SPREAD)
         ON_PEAK_SECOND_SELL_WHEEL =
     +  MAX(PATH_WHEELING_CHARGE(TG,SECOND_MARKET_TG,
     +     PEAK_HOUR,R_MONTH,globecom_YEAR),
     +              ON_PEAK_SELL_SPREAD)
!
         PEAK_HOUR = 2
!
         OFF_PEAK_BUY_SPREAD = PATH_SPREAD_RATE(MARKET_TG,TG,
     +           PEAK_HOUR,R_MONTH,globecom_YEAR)
         OFF_PEAK_SELL_SPREAD = PATH_SPREAD_RATE(TG,MARKET_TG,
     +                PEAK_HOUR,R_MONTH,globecom_YEAR)
         OFF_PEAK_BUY_WHEEL =
     +              MAX(PATH_WHEELING_CHARGE(MARKET_TG,TG,
     +       PEAK_HOUR,R_MONTH,globecom_YEAR),
     +             OFF_PEAK_BUY_SPREAD)
         OFF_PEAK_SELL_WHEEL = MAX(PATH_WHEELING_CHARGE(TG,MARKET_TG,
     +              PEAK_HOUR,R_MONTH,globecom_YEAR),
     +                   OFF_PEAK_SELL_SPREAD)
         OFF_PEAK_SECOND_BUY_WHEEL =
     +              MAX(PATH_WHEELING_CHARGE(SECOND_MARKET_TG,TG,
     +            PEAK_HOUR,R_MONTH,globecom_YEAR),
     +                  OFF_PEAK_BUY_SPREAD)
         OFF_PEAK_SECOND_SELL_WHEEL =
     +   MAX(PATH_WHEELING_CHARGE(TG,
     +                     SECOND_MARKET_TG,
     +                     PEAK_HOUR,R_MONTH,globecom_YEAR),
     +                          OFF_PEAK_SELL_SPREAD)
!
! 09/12/03. BRUTE FORCE.
!
         DO U = 1, LOCAL_NUNITS
            MON_ECO_SALES_ENRG_FROM(U) = 0.
            MON_ECO_SALES_REV_FROM(U) = 0.
            MON_ECO_PUCH_ENRG_FROM(U) = 0.
            MON_ECO_PUCH_COST_FROM(U) = 0.
         ENDDO
!
         TEMP_L = GET_TIE_GROUP_LIMIT(TIE_GROUP_LIMIT)
         year_loc = R_CURRENT_YEAR - get_BASE_YEAR()
!
         DO HOUR = 1, R_HOURS_IN_MONTH
            REMAIN = MOD(FLOAT(HOUR),24.)
            IF(REMAIN < .001) THEN ! NEW DAY
               DAY = HOUR/24
            ELSE
               DAY = HOUR/24 + 1
            ENDIF
!
! JOINT_DISPATCH_CONSTRAINT(AREA,1=SELL,2=BUY)
!
            CALENDAR_DAY_OF_WEEK = GET_DAY_OF_WEEK_4(R_MONTH,DAY)
            TIME_OF_DAY = HOUR - (DAY-1)*24
            IS_5X16 = APPLY_ENERGY_PRODUCT(
     +                                   TIME_OF_DAY,
     +                                   CALENDAR_DAY_OF_WEEK,
     +                                   '5x16                ')
!
! ASSUMES THAT BOTH MARKETS HAVE THE SAME PRICE
!
            JDA_MARKET_PRICE = SYSTEM_MARKET_PRICE(HOUR,1)
            IF(IS_5X16) THEN
               JDA_SELL_PRICE = JDA_MARKET_PRICE - ON_PEAK_SELL_WHEEL
               JDA_BUY_PRICE = JDA_MARKET_PRICE + ON_PEAK_BUY_WHEEL
            ELSE
               JDA_SELL_PRICE = JDA_MARKET_PRICE - OFF_PEAK_SELL_WHEEL
               JDA_BUY_PRICE = JDA_MARKET_PRICE + OFF_PEAK_BUY_WHEEL
            ENDIF
            JDA_COMBINED_LOAD = SYSTEM_HOURLY_LOAD(HOUR,1) +
     +                                   SYSTEM_HOURLY_LOAD(HOUR,2)
            JDA_TOTAL_COMBINED_LOAD = JDA_TOTAL_COMBINED_LOAD +
     +                                                 JDA_COMBINED_LOAD
            JDA_AVAILABLE_COMMITTED(0) = 0.
            JDA_AVAILABLE_COMMITTED(1) = 0.
            JDA_AVAILABLE_COMMITTED(2) = 0.
!
!
! 09/22/03. MORE BRUTE FORCE
!
            JDA_MINIMUM_HOURLY_LOAD(1) = 0.
            JDA_MINIMUM_HOURLY_LOAD(2) = 0.
            DO TG = 1, 2
               DO I = 1,  MAX_BLOCKS_PER_TRANS(TG)
                  JDA_MINIMUM_HOURLY_LOAD(TG) =
     +                     JDA_MINIMUM_HOURLY_LOAD(TG) +
     +                            TRANC_DATABASE(TG,HOUR,I,8)
               END DO
            END DO
!
            JDA_MINIMUM_HOURLY_LOAD(1) = MAX(JDA_MINIMUM_HOURLY_LOAD(1),
     +                                       SYSTEM_HOURLY_LOAD(HOUR,1))
            JDA_MINIMUM_HOURLY_LOAD(2) = MAX(JDA_MINIMUM_HOURLY_LOAD(2),
     +                                       SYSTEM_HOURLY_LOAD(HOUR,2))
            JDA_MINIMUM_HOURLY_LOAD(0) =
     +                  MAX(JDA_COMBINED_LOAD,
     +                     JDA_MINIMUM_HOURLY_LOAD(1) +
     +                        JDA_MINIMUM_HOURLY_LOAD(2))
            JOINT_DISPATCH_ORDER = 0.
            MARGINAL_UNIT_AGAINST_LOAD(0) = 0
            MARGINAL_UNIT_AGAINST_LOAD(1) = 0
            MARGINAL_UNIT_AGAINST_LOAD(2) = 0
!
            MARGINAL_SELL_AGAINST_PRICE(0) = 0
            MARGINAL_SELL_AGAINST_PRICE(1) = 0
            MARGINAL_SELL_AGAINST_PRICE(2) = 0
!
            MARGINAL_BUY_AGAINST_PRICE(0) = 0
            MARGINAL_BUY_AGAINST_PRICE(1) = 0
            MARGINAL_BUY_AGAINST_PRICE(2) = 0
!
! PATH CAPACITY ADDED. 06/29/01.
!
            IF(IS_5X16) THEN
               PEAK_HOUR = 1
            ELSE
               PEAK_HOUR = 2
            ENDIF
            DO I = 0, 2
               DO J = 0, 2
                  IF(I == 0 .AND. J == 0) CYCLE
                  TIME_OF_DAY_MULT =
     +                GET_CONSTRAINT_MULT(I,
     +                                    J,
     +                                    PEAK_HOUR,
     +                                    R_MONTH,
     +                                    year_loc)
                  JOINT_DISPATCH_CONSTRAINT(I,J) =
     +                     GET_SEASON_PATH_LIMIT(I,J) * TIME_OF_DAY_MULT
               ENDDO
            ENDDO
!
            ! EXTRA CONSTRAINT
            JOINT_DISPATCH_CONSTRAINT(0,0) = TIE_GROUP_LIMIT(1) 

            J = 1
            K = 1
            DO I = 1, MAX_NBLOK

               IF(TRANC_DATABASE(1,HOUR,J,7) <
     +                           TRANC_DATABASE(2,HOUR,K,7) .AND.
     +                                 J < MAX_BLOCKS_PER_TRANS(1)) THEN
                  L = 1
                  C = J
                  J = J+1
               ELSEIF(K < MAX_BLOCKS_PER_TRANS(2)) THEN
                  L = 2
                  C = K
                  K = K+1
               ELSE
                  EXIT
               ENDIF

               JDA_AVAILABLE_COMMITTED(L) =
     +                           MAX(JDA_AVAILABLE_COMMITTED(L),
     +                                       TRANC_DATABASE(L,HOUR,C,6))
!
               IF(I > 1) THEN
                  JOINT_DISPATCH_ORDER(I,1) =
     +                     JOINT_DISPATCH_ORDER(I-1,1) +
     +                             TRANC_DATABASE(L,HOUR,C,4)
               ELSE
                  JOINT_DISPATCH_ORDER(I,1) = TRANC_DATABASE(L,HOUR,C,4)
               ENDIF
!
! JOINT DISPATCH COST
               JOINT_DISPATCH_ORDER(I,2) = TRANC_DATABASE(L,HOUR,C,7)
! WHICH SYSTEM IS CHEAPER
               JOINT_DISPATCH_ORDER(I,3) = FLOAT(L)
! POSITION IN DISPATCH OF CHEAPER SYSTEM
               JOINT_DISPATCH_ORDER(I,4) = FLOAT(C)
!
! IS THIS THE MARGINAL UNIT AGAINST LOAD (BASED UPON DISPATCH)?
!
               IF(MARGINAL_UNIT_AGAINST_LOAD(0) == 0 .AND.
     +                        JOINT_DISPATCH_ORDER(I,1) >

     +                                           JDA_COMBINED_LOAD) THEN
                  MARGINAL_UNIT_AGAINST_LOAD(0) = I
               ENDIF
               IF(MARGINAL_UNIT_AGAINST_LOAD(1) == 0 .AND.
     +                        TRANC_DATABASE(1,HOUR,J,6) >
     +                                  SYSTEM_HOURLY_LOAD(HOUR,1)) THEN
                  MARGINAL_UNIT_AGAINST_LOAD(1) = J
               ENDIF
               IF(MARGINAL_UNIT_AGAINST_LOAD(2) == 0 .AND.
     +                        TRANC_DATABASE(2,HOUR,K,6) >
     +                                  SYSTEM_HOURLY_LOAD(HOUR,2)) THEN
                  MARGINAL_UNIT_AGAINST_LOAD(2) = K
               ENDIF
!
! LAST RESOURCE BELOW MARKET FOR SELLS
!
               IF(MARGINAL_SELL_AGAINST_PRICE(0) == 0 .AND.
     +                        JOINT_DISPATCH_ORDER(I,2) >
     +                                            JDA_SELL_PRICE) THEN
                  MARGINAL_SELL_AGAINST_PRICE(0) = MAX(I-1,1)
               ENDIF
               IF(MARGINAL_SELL_AGAINST_PRICE(1) == 0 .AND.
     +                        TRANC_DATABASE(1,HOUR,J,7) >
     +                                            JDA_SELL_PRICE) THEN
                  MARGINAL_SELL_AGAINST_PRICE(1) = MAX(J-1,1)
               ENDIF
               IF(MARGINAL_SELL_AGAINST_PRICE(2) == 0 .AND.
     +                        TRANC_DATABASE(2,HOUR,K,7) >
     +                                            JDA_SELL_PRICE) THEN
                  MARGINAL_SELL_AGAINST_PRICE(2) = MAX(K-1,1)
               ENDIF
! 10/10/03.
               IF(MARGINAL_BUY_AGAINST_PRICE(0) == 0 .AND.
     +                        JOINT_DISPATCH_ORDER(I,2) >
     +                                            JDA_BUY_PRICE) THEN
                  MARGINAL_BUY_AGAINST_PRICE(0) = MAX(I-1,1)
               ENDIF
               IF(MARGINAL_BUY_AGAINST_PRICE(1) == 0 .AND.
     +                        TRANC_DATABASE(1,HOUR,J,7) >
     +                                            JDA_BUY_PRICE) THEN
                  MARGINAL_BUY_AGAINST_PRICE(1) = MAX(J-1,1)
               ENDIF
               IF(MARGINAL_BUY_AGAINST_PRICE(2) == 0 .AND.
     +                        TRANC_DATABASE(2,HOUR,K,7) >
     +                                            JDA_BUY_PRICE) THEN
                  MARGINAL_BUY_AGAINST_PRICE(2) = MAX(K-1,1)
               ENDIF

            ENDDO

            IF(MARGINAL_SELL_AGAINST_PRICE(0) == 0) THEN
               MARGINAL_SELL_AGAINST_PRICE(0) = MAX(1,I-1)
               MARGINAL_SELL_AGAINST_PRICE(1) = MAX_BLOCKS_PER_TRANS(1)
               MARGINAL_SELL_AGAINST_PRICE(2) = MAX_BLOCKS_PER_TRANS(2)
            ENDIF
            IF(MARGINAL_BUY_AGAINST_PRICE(0) == 0) THEN
               MARGINAL_BUY_AGAINST_PRICE(0) = MAX(1,I-1)
               MARGINAL_BUY_AGAINST_PRICE(1) = MAX_BLOCKS_PER_TRANS(1)
               MARGINAL_BUY_AGAINST_PRICE(2) = MAX_BLOCKS_PER_TRANS(2)
            ENDIF
            IF(MARGINAL_UNIT_AGAINST_LOAD(0) == 0) THEN
               MARGINAL_UNIT_AGAINST_LOAD(0) = MAX(1,I-1)
               MARGINAL_UNIT_AGAINST_LOAD(1) = MAX_BLOCKS_PER_TRANS(1)
               MARGINAL_UNIT_AGAINST_LOAD(2) = MAX_BLOCKS_PER_TRANS(2)
            ENDIF

            I = MARGINAL_BUY_AGAINST_PRICE(0)
            J = MARGINAL_UNIT_AGAINST_LOAD(0)
            CENTRAL_DISPATCH_GENERATION(0) = JOINT_DISPATCH_ORDER(I,1)
            INDIVIDUAL_DISPATCH_TRANSFERS(0) =
     +             JOINT_DISPATCH_ORDER(I,1) - JOINT_DISPATCH_ORDER(J,1)
!
            J = MARGINAL_SELL_AGAINST_PRICE(1)
            CENTRAL_DISPATCH_GENERATION(1) = TRANC_DATABASE(1,HOUR,J,6)
            K = MARGINAL_SELL_AGAINST_PRICE(2)
            CENTRAL_DISPATCH_GENERATION(2) = TRANC_DATABASE(2,HOUR,K,6)
!

            I = MARGINAL_BUY_AGAINST_PRICE(1)
            J = MARGINAL_UNIT_AGAINST_LOAD(1)
            INDIVIDUAL_DISPATCH_TRANSFERS(1) =
     +                        TRANC_DATABASE(1,HOUR,I,6)-
     +                                        TRANC_DATABASE(1,HOUR,J,6)

            K = MARGINAL_BUY_AGAINST_PRICE(2)
            L = MARGINAL_UNIT_AGAINST_LOAD(2)
            INDIVIDUAL_DISPATCH_TRANSFERS(2) =
     +                        TRANC_DATABASE(2,HOUR,K,6)-
     +                                        TRANC_DATABASE(2,HOUR,L,6)
!
! TEST INDIVIDUAL PURCHASE CONSTRAINTS
!
            MARGIN_UNIT_MW = 99999.
            INDIVIDUAL_TRANSFER_DEFICIT(0) =
     +              -MIN(0.,
     +                      MIN(JOINT_DISPATCH_CONSTRAINT(2,2),
     +                            JOINT_DISPATCH_CONSTRAINT(0,1) +
     +                            JOINT_DISPATCH_CONSTRAINT(0,2)) +
     +                                 CENTRAL_DISPATCH_GENERATION(0) -
     +                                               JDA_COMBINED_LOAD)

! IMPLEMENT INDIVIDUAL AND JOINT PURCHASE CONSTRAINTS HERE.
!
            IF(INDIVIDUAL_DISPATCH_TRANSFERS(0) < 0.) THEN ! NET BUYER

               IF(INDIVIDUAL_TRANSFER_DEFICIT(0) > 0. ) THEN
! MUST INCREASE INTERNAL TARGET TO MEET LOAD REQUIREMENTS
                  J = MARGINAL_UNIT_AGAINST_LOAD(0)
                  CENTRAL_DISPATCH_TARGET(0) =
     +                      CENTRAL_DISPATCH_GENERATION(0) +
     +                                INDIVIDUAL_TRANSFER_DEFICIT(0)

                  DO I = 1, MAX_NBLOK
                     IF(JOINT_DISPATCH_ORDER(I,1) <
     +                                 CENTRAL_DISPATCH_TARGET(0)) CYCLE
                     MARGINAL_UNIT_AGAINST_TARGET(0) = I
                     MARGIN_UNIT_MW =
     +                        CENTRAL_DISPATCH_TARGET(0) -
     +                                JOINT_DISPATCH_ORDER(MAX(I-1,1),1)
                     EXIT
                  END DO
               ELSE ! ECONOMIC PURCHASE
                  CENTRAL_DISPATCH_TARGET(0) =
     +                                    CENTRAL_DISPATCH_GENERATION(0)
                  MARGINAL_UNIT_AGAINST_TARGET(0) =
     +                                    MARGINAL_BUY_AGAINST_PRICE(0)

               ENDIF
            ELSE ! NET SELLER
!
! CAPTURE THE CASE WHERE IT IS CHEAPER TO SELL TO OWN LOAD RATHER 
! THAN WHEELING POWER IN.
!
               CENTRAL_DISPATCH_TARGET(0) =
     +                                    CENTRAL_DISPATCH_GENERATION(0)
               MARGINAL_UNIT_AGAINST_TARGET(0) =
     +              MAX(MARGINAL_SELL_AGAINST_PRICE(0),
     +                                   MARGINAL_UNIT_AGAINST_LOAD(0))
               MARGINAL_UNIT_AGAINST_TARGET(1) =
     +              MAX(MARGINAL_SELL_AGAINST_PRICE(1),
     +                                   MARGINAL_UNIT_AGAINST_LOAD(1))
               MARGINAL_UNIT_AGAINST_TARGET(2) =
     +              MAX(MARGINAL_SELL_AGAINST_PRICE(2),
     +                                   MARGINAL_UNIT_AGAINST_LOAD(2))

            ENDIF

! CASES:
!
!  BLOCK 1 TO SERVE BLOCK 1
!  BLOCK 2 TO SERVE BLOCK 2
!  BLOCK 1 TO SERVE BLOCK 2
!  BLOCK 2 TO SERVE_BLOCK 1
!  BLOCK 1 TO SERVE MARKET
!  BLOCK 2 SERVE MARKET
!
            JDA_GENERATION(0,0) = 0.
            JDA_GENERATION(0,1) = 0.
            JDA_GENERATION(0,2) = 0.
            JDA_GENERATION(0,3) = 0.
            JDA_GENERATION(1,0) = 0.
            JDA_GENERATION(1,1) = 0.
            JDA_GENERATION(1,2) = 0.
            JDA_GENERATION(1,3) = 0.
            JDA_GENERATION(2,0) = 0.
            JDA_GENERATION(2,1) = 0.
            JDA_GENERATION(2,2) = 0.
            JDA_GENERATION(2,3) = 0.
!
!
            JDA_AVAILABLE_COMMITTED(0) = JDA_AVAILABLE_COMMITTED(1) +
     +                                        JDA_AVAILABLE_COMMITTED(2)
!
            JDA_PURCHASES(0) = 0.
            JDA_PURCHASES(1) = 0.
            JDA_PURCHASES(2) = 0.
!
            JDA_ENERGY_BALANCE(1) = 0.
            JDA_ENERGY_BALANCE(2) = 0.
!
            JDA_MUST_RUN(1) = 0.
            JDA_MUST_RUN(2) = 0.
!
            JDA_COST(1) = 0.
            JDA_COST(2) = 0.
!
            JDA_MARGINAL_COST(1) = 0.
            JDA_MARGINAL_COST(2) = 0.
!
            I = 0
            TEMP_CAP = 0.
            J = 0
!
            C_BY_SYSTEM(1) = 999
            C_BY_SYSTEM(2) = 999
!
! ROUTINUE PERMITS LOOPING WITH PARTIAL UNIT STATES
            DOWHILE(I < MAX_NBLOK .AND. J < 20)
!
               JDA_GENERATION(0,1) = JDA_GENERATION(1,1)+
     +                                               JDA_GENERATION(2,1)
               JDA_GENERATION(0,2) = JDA_GENERATION(1,2)+
     +                                               JDA_GENERATION(2,2)
               JDA_GENERATION(0,3) = JDA_GENERATION(1,3)+
     +                                               JDA_GENERATION(2,3)
               JDA_GENERATION(0,0) = JDA_GENERATION(0,1) +
     +                                       JDA_GENERATION(0,2) +
     +                                               JDA_GENERATION(0,3)
!
!               IF(JOINT_DISPATCH_ORDER(I,2) > .1) THEN
!                  JDA_MARGINAL_COST(L) = JOINT_DISPATCH_ORDER(I,2)
!               ENDIF
!
               J = J + 1
!
!               IF(J > 10) THEN
!                  J = J
!               ENDIF
!
               IF(TEMP_CAP == 0.) THEN
                  J = 0
                  I = I + 1
                  L = INT(JOINT_DISPATCH_ORDER(I,3),2)
                  C = INT(JOINT_DISPATCH_ORDER(I,4),2)
                  TEMP_CAP = TRANC_DATABASE(L,HOUR,C,4)
                  JDA_BLOCK_CAPACITY = 0.
                  B = MAX(1,INT(TRANC_DATABASE(L,HOUR,C,2)))
                  U = INT(TRANC_DATABASE(L,HOUR,C,3))
               ENDIF
               IF(L == 1) THEN
                  M = 2
               ELSE
                  M = 1
               ENDIF
!
               TEMP_R = 0. ! TO CAPTURE ENERGY AND HEAT
!
               IF(C == 0) THEN
                  EXIT
               ENDIF
               IF(U <= 0) THEN
                  IF(TEMP_CAP > .1) THEN
                     TEMP_CAP = TEMP_CAP
                  ENDIF
                  TEMP_CAP = 0.
                  CYCLE
               ENDIF
!                  C_BY_SYSTEM(L) = 0
!                  IF(C_BY_SYSTEM(M) > 0) THEN
!                     CYCLE
!                  ELSE
!                     EXIT
!                  ENDIF
!               ENDIF
!
! 09/22/03. MUST TAKE HOURLY MUST RUN CAPACITY.
! 10/06/03. RE-INSTATE MUST RUN AS NOT CONDITION FOR PURCHASE
!
!               IF(TRANC_DATABASE(L,HOUR,C,8) > .1) THEN
!                  TEMP_R = TRANC_DATABASE(L,HOUR,C,8)
!                  JDA_GENERATION(L,1) = JDA_GENERATION(L,1) + TEMP_R
!                  TEMP_CAP = 0.
!
!
!               IF(I > MARGINAL_UNIT_AGAINST_TARGET(0) .AND.
! 10/13/03. CHANGED ABOVE CONDITION
               IF(C > MARGINAL_UNIT_AGAINST_TARGET(L) .AND.
     +               JOINT_DISPATCH_CONSTRAINT(0,L) -
     +                              JDA_PURCHASES(L) > .1 .AND.
     + .NOT. TRANC_DATABASE(L,HOUR,C,8) > .1) THEN ! PURCHASE
! BLOCK ABOVE CENTRAL DISPATCH GENERATION
!
! THIS NEED TO BE RELAXED IF THE SYSTEMS ARE IN A CONSTRAINED BUY 
! SITUATION
! CASES:
!     NEITHER ENTITY CAN BUY OR SELL WITH EACH OTHER OR MARKET
!     NEITHER ENTITY CAN BUY OR SELL WITH THE MARKET BUT CAN WITH EACH 
! OTHER
!     NEITHER ENTITY CAN BUY OR SELL WITH EACH OTHER BUT CAN WITH THE 
! MARKET
!     ONE ENTITY CAN BUY OR SELL WITH THE MARKET
!

                  IF(C > MARGINAL_UNIT_AGAINST_TARGET(L)) THEN
                     TEMP_CAP = 0.
                     CYCLE
                  ELSE
!
! TWO CASES: STRADDLE AND WHOLE BLOCK
!
                     TEMP_R =
     +                     SYSTEM_HOURLY_LOAD(HOUR,L) +
     +                        SYSTEM_HOURLY_LOAD(HOUR,M) -
     +                              (JDA_GENERATION(L,1) +
     +                                 JDA_GENERATION(M,1) +
     +                                     JDA_GENERATION(L,2) +
     +                                         JDA_GENERATION(M,2) +
     +                                            JDA_PURCHASES(L) +
     +                                                 JDA_PURCHASES(M))
!
! 10/13/03.
!
                     TEMP_R = MIN(TEMP_R,
     +  JOINT_DISPATCH_CONSTRAINT(0,L)-JDA_PURCHASES(L), ! SINGLE IMPORT
     +                     JOINT_DISPATCH_CONSTRAINT(2,2)-
     +               JDA_PURCHASES(L)+JDA_PURCHASES(M))   ! JOINT IMPORT
!
                     TEMP_R =  MAX(0.,MIN(TEMP_R,TEMP_CAP))
                     TEMP_RL = 0.
                     TEMP_RM = 0.
!
! CHECK INDIVIDUAL AND JOINT IMPORT CONSTRAINTS HERE.
!
                     IF( SYSTEM_HOURLY_LOAD(HOUR,L) -
     +                        (JDA_GENERATION(L,1) +
     +                                 JDA_GENERATION(M,2)) > 0.1 ) THEN
                        TEMP_RL =
     +                     MAX(0.,
     +                        MIN(TEMP_R,
     +                             SYSTEM_HOURLY_LOAD(HOUR,L) -
     +                                    (JDA_GENERATION(L,1) +
     +                                            JDA_GENERATION(M,2))))
                        JDA_PURCHASES(L) = JDA_PURCHASES(L) + TEMP_RL
                     ENDIF
!
                     IF( SYSTEM_HOURLY_LOAD(HOUR,M) -
     +                        (JDA_GENERATION(M,1) +
     +                                 JDA_GENERATION(L,2)) > 0.1 ) THEN
                        TEMP_RM =
     +                     MAX(0.,
     +                        MIN(TEMP_R - TEMP_RL,
     +                             SYSTEM_HOURLY_LOAD(HOUR,M) -
     +                                    (JDA_GENERATION(M,1) +
     +                                            JDA_GENERATION(L,2))))
                        JDA_PURCHASES(M) = JDA_PURCHASES(M) + TEMP_RM
                     ENDIF
!
                     TEMP_R = TEMP_RL + TEMP_RM
!
                     MON_ECO_PUCH_ENRG_FROM(U) =
     +                              MON_ECO_PUCH_ENRG_FROM(U) + TEMP_R
!
! WE NEED THE WHEELING CHARGE HERE.
!
                     MON_ECO_PUCH_COST_FROM(U) =
     +                              MON_ECO_PUCH_COST_FROM(U) +
     +                                              TEMP_R*JDA_BUY_PRICE
                     IF( TEMP_R < .1) THEN
                        TEMP_CAP = 0.
                     ELSE
                        TEMP_CAP = MAX(0.,TEMP_CAP - TEMP_R)
                     ENDIF
                  ENDIF
!
!                  EXIT
               ELSE ! GENERATION FOR RETAIL SALE OR WHOLESALE SALE
                  IF(TRANC_DATABASE(L,HOUR,C,8) > .1) THEN
                     JDA_MUST_RUN(L) = JDA_MUST_RUN(L) + TEMP_CAP
                  ENDIF
                  IF(JDA_GENERATION(L,1)+
     +                        JDA_GENERATION(M,2) + .1 <
     +                                  SYSTEM_HOURLY_LOAD(HOUR,L)) THEN

! PARTIAL BLOCK SERVES NATIVE LOAD
                     TEMP_R =  SYSTEM_HOURLY_LOAD(HOUR,L) -
     +                                   (JDA_GENERATION(L,1) +
     +                                              JDA_GENERATION(M,2))
                     IF(I == MARGINAL_UNIT_AGAINST_TARGET(0))THEN
                        TEMP_R = MIN(TEMP_R,MARGIN_UNIT_MW)
                     ENDIF
                     IF(TEMP_R > TEMP_CAP) THEN
                        TEMP_R = TEMP_CAP
                     ENDIF
                     JDA_GENERATION(L,1) = JDA_GENERATION(L,1) +
     +                                                            TEMP_R
!
!
                     IF( ABS(TEMP_R - MARGIN_UNIT_MW) < .01) THEN
                        TEMP_CAP = 0.
                     ELSE
                        TEMP_CAP = TEMP_CAP - TEMP_R
                     ENDIF
!
 !                 ENDIF
                  ELSEIF(JDA_GENERATION(L,2)+JDA_GENERATION(M,1) +.1 <
     +                                  SYSTEM_HOURLY_LOAD(HOUR,M) .AND.
     +                  JDA_GENERATION(L,2) + .1 <
     +                            JOINT_DISPATCH_CONSTRAINT(L,M)  ) THEN
! SERVE OTHER AREA NATIVE LOAD
                     TEMP_R = MIN(TEMP_CAP,
     +                   SYSTEM_HOURLY_LOAD(HOUR,M) -
     +                       (JDA_GENERATION(L,2)+JDA_GENERATION(M,1)),
     +                            JOINT_DISPATCH_CONSTRAINT(L,M)-
     +                                              JDA_GENERATION(L,2))
!
                     IF(I == MARGINAL_UNIT_AGAINST_TARGET(0))THEN
                        TEMP_R = MIN(TEMP_R,MARGIN_UNIT_MW)
                     ENDIF
!
                     JDA_GENERATION(L,2) = JDA_GENERATION(L,2) + TEMP_R
!
                     JDA_COST(L) = JDA_COST(L) +
     +                               TEMP_R * TRANC_DATABASE(L,HOUR,C,7)
                     IF( ABS(TEMP_R - MARGIN_UNIT_MW) < .01) THEN
                        TEMP_CAP = 0.
                     ELSE
                        TEMP_CAP = TEMP_CAP - TEMP_R
                     ENDIF
                  ELSEIF(I  <  MARGINAL_UNIT_AGAINST_TARGET(0)) THEN
!
                     IF(I > MARGINAL_SELL_AGAINST_PRICE(0)) THEN
                        TEMP_CAP = 0.
                        CYCLE
                     ENDIF
!
! WHOLE BLOCK SERVES EXTERNAL MARKET
                     TEMP_R =
     +                  MIN(TEMP_CAP,
     +                     MAX(0.,JOINT_DISPATCH_CONSTRAINT(1,1)-
     +                        (JDA_GENERATION(L,3) +
     +                                            JDA_GENERATION(M,3))),
     +                        MAX(0.,JOINT_DISPATCH_CONSTRAINT(L,0)-
     +                                             JDA_GENERATION(L,3)))
                     IF(TEMP_R < 0.) THEN
                        TEMP_R = 0.
                     ENDIF
                     JDA_GENERATION(L,3) = JDA_GENERATION(L,3) + TEMP_R
!
                     MON_ECO_SALES_ENRG_FROM(U) =
     +                               MON_ECO_SALES_ENRG_FROM(U) + TEMP_R
                     MON_ECO_SALES_REV_FROM(U) =
     +                        MON_ECO_SALES_REV_FROM(U) +
     +                                           TEMP_R*JDA_SELL_PRICE
!
                     TEMP_CAP = 0.
                  ELSEIF(I == MARGINAL_UNIT_AGAINST_TARGET(0)) THEN
! PARTIAL BLOCK SERVES EXTERNAL MARKET
!
                     IF(I > MARGINAL_SELL_AGAINST_PRICE(0)) CYCLE
!
                     TEMP_R =
     +                  MIN(TEMP_CAP,
     +                     MAX(0.,CENTRAL_DISPATCH_TARGET(0) -
     +                                             JDA_GENERATION(0,0)),
     +                     MAX(0.,JOINT_DISPATCH_CONSTRAINT(1,1)-
     +                        (JDA_GENERATION(L,3) +
     +                                            JDA_GENERATION(M,3))),
     +                        MAX(0.,JOINT_DISPATCH_CONSTRAINT(L,0)-
     +                                             JDA_GENERATION(L,3)))
                     IF(TEMP_R < 0.) THEN
                        TEMP_R = 0.
                     ENDIF
                     JDA_GENERATION(L,3) = JDA_GENERATION(L,3) + TEMP_R
!
                     MON_ECO_SALES_ENRG_FROM(U) =
     +                               MON_ECO_SALES_ENRG_FROM(U) + TEMP_R
                     MON_ECO_SALES_REV_FROM(U) =
     +                        MON_ECO_SALES_REV_FROM(U) +
     +                                           TEMP_R*JDA_SELL_PRICE
!
                     TEMP_CAP = 0.
                  ENDIF
! 10/13/03. CHANGED POSITION OF MARGINAL COST
                  IF(JOINT_DISPATCH_ORDER(I,2) > .1 .AND.
     +                                                 TEMP_R > 0.) THEN
                     JDA_MARGINAL_COST(L) = JOINT_DISPATCH_ORDER(I,2)
                  ENDIF
!
!                 UNIT CALCULATIONS FOR GENERATION
!
                  OUTAGE_CAPACITY = TEMP_R
!
                  JDA_BLOCK_CAPACITY = JDA_BLOCK_CAPACITY + TEMP_R
                  IF( JDA_BLOCK_CAPACITY -
     +                       GET_BLOCK_AVAIL_4_HOUR(U,B,HOUR) > .1) THEN
                     OUTAGE_CAPACITY = GET_BLOCK_AVAIL_4_HOUR(U,B,HOUR)-
     +                                                JDA_BLOCK_CAPACITY
                     TEMP_R = OUTAGE_CAPACITY
                  ENDIF
!
! 12/20/04. TEST
!
                  O = ORIGINAL_VALUE_OF_U(S_U)
!
!
                  COMBINED_GEN_IN_SYSTEM(B,U) =
     +                        COMBINED_GEN_IN_SYSTEM(B,U) +
     +                                                   OUTAGE_CAPACITY
                  IF(B <= 1) THEN
                     TOTAL_HEAT = COEFF(1,U) * OUTAGE_CAPACITY
                     COMBINED_HEAT_PARAM(1,U) =
     +                           COMBINED_HEAT_PARAM(1,U) + TOTAL_HEAT
                  ELSEIF(B == 2 .AND.  OUTAGE_CAPACITY > 0.01) THEN
                     TEMP_R =  GET_BLOCK_AVAIL_4_HOUR(U,INT(1,2),HOUR)
                     TEMP_R2 =  GET_BLOCK_AVAIL_4_HOUR(U,INT(2,2),HOUR)
                     A_CO = (COEFF(2,U)-COEFF(3,U))/
     +                                          (-2.*TEMP_R2)
                     B_CO = COEFF(2,U) - (COEFF(2,U) - COEFF(3,U)) *
     +                        TEMP_R/(-1.*TEMP_R2)
                     C_CO = ((COEFF(1,U)-B_CO)*
     +                               TEMP_R) -
     +                             A_CO*TEMP_R*TEMP_R
     
        ! TOTAL HEAT FOR THE BLOCK: NOTE MIN BLOCK SUBTRACTED BELOW
                     TOTAL_HEAT =   
     +                     (A_CO*
     +                      (TEMP_R+OUTAGE_CAPACITY)*
     +                      (TEMP_R+OUTAGE_CAPACITY) +
     +                     B_CO*
     +                      (TEMP_R+OUTAGE_CAPACITY)+
     +                     C_CO)- TEMP_R*COEFF(1,U) ! MIN BLOCK HEAT
                     COMBINED_HEAT_PARAM(B,U) =
     +                           COMBINED_HEAT_PARAM(B,U) + TOTAL_HEAT
                  ELSE
                     TOTAL_HEAT = 0.
                  ENDIF
               ENDIF ! PURCHASE OR GENERATION
!
            ENDDO ! INCREMENT TO NEXT GENERATION BLOCK AS NEEDED
!
! SHOULD ONLY HAVE PURCHASES ABOVE ECONOMIC RESOURCES LEFT
! PUT BACK IN ON 10/03/03
!
            DO  L = 1, 2
               IF(L == 1) THEN
                  M = 2
               ELSE
                  M = 1
               ENDIF
               IF(JDA_GENERATION(M,2) +
     +                  JDA_GENERATION(L,1) +
     +                     JDA_PURCHASES(L) + .1 <
     +                                  SYSTEM_HOURLY_LOAD(HOUR,L)) THEN
                  JDA_PURCHASES(L) =
     +               MAX(0.,
     +                  MIN(JOINT_DISPATCH_CONSTRAINT(0,L),
     +                        SYSTEM_HOURLY_LOAD(HOUR,L) -
     +                         (JDA_GENERATION(M,2) +
     +                                    JDA_GENERATION(L,1) +
     +                                               JDA_PURCHASES(L))))
                  JDA_PURCHASES(0) =  JDA_PURCHASES(0) +
     +                                                  JDA_PURCHASES(L)
               ENDIF
!
               MONTHLY_JDA_PURCHASES(L) = MONTHLY_JDA_PURCHASES(L) +
     +                                                  JDA_PURCHASES(L)
               MONTHLY_JDA_PURCHASES(0) = MONTHLY_JDA_PURCHASES(0) +
     +                                                  JDA_PURCHASES(L)
            ENDDO ! TRANSACTION GROUP FOR PURCHASES
!
            JDA_GENERATION(1,0) = JDA_GENERATION(1,1) +
     +                                 JDA_GENERATION(1,2) +
     +                                    JDA_GENERATION(1,3)
            JDA_GENERATION(2,0) = JDA_GENERATION(2,1) +
     +                                 JDA_GENERATION(2,2) +
     +                                    JDA_GENERATION(2,3)
!
            MONTHLY_JDA_GENERATION(0,0) = MONTHLY_JDA_GENERATION(0,0) +
     +                                         JDA_GENERATION(0,0)
            MONTHLY_JDA_GENERATION(0,1) = MONTHLY_JDA_GENERATION(0,1) +
     +                                         JDA_GENERATION(0,1)
            MONTHLY_JDA_GENERATION(0,2) = MONTHLY_JDA_GENERATION(0,2) +
     +                                         JDA_GENERATION(0,2)
            MONTHLY_JDA_GENERATION(0,3) = MONTHLY_JDA_GENERATION(0,3) +
     +                                         JDA_GENERATION(0,3)
            MONTHLY_JDA_GENERATION(1,0) = MONTHLY_JDA_GENERATION(1,0) +
     +                                         JDA_GENERATION(1,0)
            MONTHLY_JDA_GENERATION(1,1) = MONTHLY_JDA_GENERATION(1,1) +
     +                                         JDA_GENERATION(1,1)
            MONTHLY_JDA_GENERATION(1,2) = MONTHLY_JDA_GENERATION(1,2) +
     +                                         JDA_GENERATION(1,2)
            MONTHLY_JDA_GENERATION(1,3) = MONTHLY_JDA_GENERATION(1,3) +
     +                                         JDA_GENERATION(1,3)
            MONTHLY_JDA_GENERATION(2,0) = MONTHLY_JDA_GENERATION(2,0) +
     +                                         JDA_GENERATION(2,0)
            MONTHLY_JDA_GENERATION(2,1) = MONTHLY_JDA_GENERATION(2,1) +
     +                                         JDA_GENERATION(2,1)
            MONTHLY_JDA_GENERATION(2,2) = MONTHLY_JDA_GENERATION(2,2) +
     +                                         JDA_GENERATION(2,2)
            MONTHLY_JDA_GENERATION(2,3) = MONTHLY_JDA_GENERATION(2,3) +
     +                                         JDA_GENERATION(2,3)
!
            MONTHLY_JDA_COST(1) = MONTHLY_JDA_COST(1) + JDA_COST(1)
            MONTHLY_JDA_COST(2) = MONTHLY_JDA_COST(2) + JDA_COST(2)
!
            IF(JDA_GENERATION(1,2) > 0.) THEN
               JDA_COST(1) = JDA_COST(1)/JDA_GENERATION(1,2)
            ENDIF
            IF(JDA_GENERATION(2,2) > 0.) THEN
               JDA_COST(2) = JDA_COST(2)/JDA_GENERATION(2,2)
            ENDIF
            JDA_ENERGY_BALANCE(1) =
! DEMAND
     +          SYSTEM_HOURLY_LOAD(HOUR,1) + ! NATIVE
     +                              JDA_GENERATION(1,3) + ! MARKET SALES
     + JDA_GENERATION(1,2) - ! INTRA-SALES
! SUPPLY
     +          (JDA_GENERATION(1,0) + ! GENERATION
     +                            JDA_PURCHASES(1) + ! MARKET PURCHASES
     +                        JDA_GENERATION(2,2)) ! INTRA-PURCHASES
            JDA_ENERGY_BALANCE(2) =
! DEMAND
     +          SYSTEM_HOURLY_LOAD(HOUR,2) +
     +                              JDA_GENERATION(2,3) +
     +                                             JDA_GENERATION(2,2) -
! SUPPLY
     +          (JDA_GENERATION(2,0) +
     +                              JDA_PURCHASES(2) +
     +                                          JDA_GENERATION(1,2))
!
            MONTHLY_JDA_ENERGY_BALANCE(1) =
     +               MONTHLY_JDA_ENERGY_BALANCE(1) +
     +                                    JDA_ENERGY_BALANCE(1)
            MONTHLY_JDA_ENERGY_BALANCE(2) =
     +               MONTHLY_JDA_ENERGY_BALANCE(2) +
     +                                    JDA_ENERGY_BALANCE(2)
!
            MONTHLY_JDA_MUST_RUN(1) =
     +                MONTHLY_JDA_MUST_RUN(1) +
     +                                    JDA_MUST_RUN(1)
            MONTHLY_JDA_MUST_RUN(2) =
     +                MONTHLY_JDA_MUST_RUN(2) +
     +                                    JDA_MUST_RUN(2)
!
            IF(HOURLY_JDA_REPORT) THEN
               HR = HOUR - (DAY-1)*24
               HOUR_JDA_REC = rptrec(HOUR_JDA_UNIT)
               WRITE(HOUR_JDA_UNIT,REC=HOUR_JDA_REC)
     +                  PRT_ENDPOINT(),
     +                  FLOAT(R_CURRENT_YEAR),
     +                  R_MONTH_NAME,
     +                  FLOAT(DAY),
     +                  FLOAT(HR),
     +                  JDA_GENERATION,  ! 12
     +                  JDA_COST,        ! 2
     +                  JDA_PURCHASES,    ! 3
     +                  SYSTEM_MARKET_PRICE(HOUR,1),
     +                  JDA_MARGINAL_COST(1),
     +                  JDA_COMBINED_LOAD,
     +                  JDA_AVAILABLE_COMMITTED, ! 3
     +                  JDA_SELL_PRICE,
     +                  JDA_BUY_PRICE,
     +                  SYSTEM_HOURLY_LOAD(HOUR,1),
     +                  SYSTEM_HOURLY_LOAD(HOUR,2),
     +                  JDA_ENERGY_BALANCE, ! 2
     +                  JDA_MUST_RUN, ! 2
     +                  JDA_MARGINAL_COST(2)
               HOUR_JDA_REC = HOUR_JDA_REC + 1
               IF(HOUR == R_HOURS_IN_MONTH) THEN
!
                  IF(MONTHLY_JDA_GENERATION(1,2) > 0.) THEN
                     MONTHLY_JDA_COST(1) = MONTHLY_JDA_COST(1)/
     +                                       MONTHLY_JDA_GENERATION(1,2)
                  ENDIF
                  IF(MONTHLY_JDA_GENERATION(2,2) > 0.) THEN
                     MONTHLY_JDA_COST(2) = MONTHLY_JDA_COST(2)/
     +                                       MONTHLY_JDA_GENERATION(2,2)
                  ENDIF
!
                  HOUR_JDA_REC = rptrec(HOUR_JDA_UNIT)
                  WRITE(HOUR_JDA_UNIT,REC=HOUR_JDA_REC)
     +                  PRT_ENDPOINT(),
     +                  FLOAT(R_CURRENT_YEAR),
     +                  R_MONTH_NAME,
     +                  FLOAT(0),
     +                  FLOAT(0),
     +                  MONTHLY_JDA_GENERATION, ! 12
     +                  MONTHLY_JDA_COST,        ! 2
     +                  MONTHLY_JDA_PURCHASES,  ! 3
     +                  SYSTEM_MARKET_PRICE(HOUR,1),
     +                  JDA_MARGINAL_COST(1),
     +                  JDA_TOTAL_COMBINED_LOAD,
     +                  JDA_AVAILABLE_COMMITTED, ! 3
     +                  JDA_SELL_PRICE,
     +                  JDA_BUY_PRICE,
     +                  SYSTEM_HOURLY_LOAD(HOUR,1),
     +                  SYSTEM_HOURLY_LOAD(HOUR,2),
     +                  MONTHLY_JDA_ENERGY_BALANCE, ! 2
     +                  MONTHLY_JDA_MUST_RUN, ! 2
     +                  JDA_MARGINAL_COST(2)
                  HOUR_JDA_REC = HOUR_JDA_REC + 1
               ENDIF
            ENDIF
!
!            IF( ABS(JDA_GENERATION(0,0)-
!     +                        CENTRAL_DISPATCH_TARGET(0)) > .1) THEN
!               TEMP_R = TEMP_R
!            ENDIF
!
!
!
            DO TG = 1, 2
               IF(TG == 1) THEN
                  M = 2
               ELSE
                  M = 1
               ENDIF
               SYSTEM_HOURLY_GENERATION(0,TG) =
     +                           SYSTEM_HOURLY_GENERATION(0,TG) +
     +                                              JDA_GENERATION(TG,0)
               SYSTEM_WHOLESALE_GENERATION(0,TG) =
     +                           SYSTEM_WHOLESALE_GENERATION(0,TG) +
     +                                              JDA_GENERATION(TG,3)
               SYSTEM_MARKET_BOUGHT(0,TG) =
     +                           SYSTEM_MARKET_BOUGHT(0,TG) +
     +                                                 JDA_PURCHASES(TG)
               SYSTEM_MARKET_REVENUE(0,TG) =
     +                           SYSTEM_MARKET_REVENUE(0,TG) +
     +                               JDA_GENERATION(TG,3)*JDA_SELL_PRICE
               SYSTEM_MARKET_COST(0,TG) =
     +                           SYSTEM_MARKET_COST(0,TG) +
     +                                   JDA_PURCHASES(TG)*JDA_BUY_PRICE
!
               IF(PEAK_HOUR == 1) THEN
                  SYSTEM_MONTHLY_MISC(2,TG) =
     +                           SYSTEM_MONTHLY_MISC(2,TG) +
     +                                              JDA_GENERATION(TG,3)
                  SYSTEM_MONTHLY_MISC(4,TG) =
     +                           SYSTEM_MONTHLY_MISC(4,TG) +
     +                                                 JDA_PURCHASES(TG)
                  SYSTEM_MONTHLY_MISC(6,TG) =
     +                           SYSTEM_MONTHLY_MISC(6,TG) +
     +                             JDA_GENERATION(TG,3)*JDA_SELL_PRICE
                  SYSTEM_MONTHLY_MISC(8,TG) =
     +                           SYSTEM_MONTHLY_MISC(8,TG) +
     +                                JDA_PURCHASES(TG)*JDA_SELL_PRICE
               ELSE
                  SYSTEM_MONTHLY_MISC(3,TG) =
     +                           SYSTEM_MONTHLY_MISC(3,TG) +
     +                                              JDA_GENERATION(TG,3)
                  SYSTEM_MONTHLY_MISC(5,TG) =
     +                           SYSTEM_MONTHLY_MISC(5,TG) +
     +                                                 JDA_PURCHASES(TG)
                  SYSTEM_MONTHLY_MISC(7,TG) =
     +                           SYSTEM_MONTHLY_MISC(7,TG) +
     +                             JDA_GENERATION(TG,3)*JDA_SELL_PRICE
                  SYSTEM_MONTHLY_MISC(9,TG) =
     +                           SYSTEM_MONTHLY_MISC(9,TG) +
     +                                JDA_PURCHASES(TG)*JDA_SELL_PRICE
               ENDIF
            ENDDO
!
         ENDDO ! HOUR IN MONTH
!
         DO TG = 1, 2
!
            IF(TG == 1) THEN
               M = 2
            ELSE
               M = 1
            ENDIF
!
! TRANSFER PRICING INFO.
!

            SYSTEM_MONTHLY_MISC(10,0) = SYSTEM_MONTHLY_MISC(10,0) +
     +                                        SYSTEM_MONTHLY_MISC(10,TG)

            SYSTEM_MONTHLY_MISC(19,TG) = MONTHLY_JDA_GENERATION(TG,2)
            SYSTEM_MONTHLY_MISC(20,TG) = MONTHLY_JDA_GENERATION(M,2)
            SYSTEM_MONTHLY_MISC(21,TG) =
     +                 MONTHLY_JDA_COST(TG)*MONTHLY_JDA_GENERATION(TG,2)
            SYSTEM_MONTHLY_MISC(22,TG) =
     +                   MONTHLY_JDA_COST(M)*MONTHLY_JDA_GENERATION(M,2)
!
            SYSTEM_HOURLY_GENERATION(0,0) =
     +                        SYSTEM_HOURLY_GENERATION(0,0) +
     +                               SYSTEM_HOURLY_GENERATION(0,TG)
            SYSTEM_WHOLESALE_GENERATION(0,0) =
     +                        SYSTEM_WHOLESALE_GENERATION(0,0) +
     +                              SYSTEM_WHOLESALE_GENERATION(0,TG)
            SYSTEM_MARKET_BOUGHT(0,0) =
     +                        SYSTEM_MARKET_BOUGHT(0,0) +
     +                              SYSTEM_MARKET_BOUGHT(0,TG)
            SYSTEM_MARKET_REVENUE(0,0) =
     +                        SYSTEM_MARKET_REVENUE(0,0) +
     +                              SYSTEM_MARKET_REVENUE(0,TG)
            SYSTEM_MARKET_COST(0,0) =
     +                        SYSTEM_MARKET_COST(0,0) +
     +                              SYSTEM_MARKET_COST(0,TG)
            SYSTEM_MONTHLY_MISC(2,0) =
     +                        SYSTEM_MONTHLY_MISC(2,0) +
     +                              SYSTEM_MONTHLY_MISC(2,TG)
            SYSTEM_MONTHLY_MISC(4,0) =
     +                        SYSTEM_MONTHLY_MISC(4,0) +
     +                              SYSTEM_MONTHLY_MISC(4,TG)
            SYSTEM_MONTHLY_MISC(6,0) =
     +                        SYSTEM_MONTHLY_MISC(6,0) +
     +                              SYSTEM_MONTHLY_MISC(6,TG)
            SYSTEM_MONTHLY_MISC(8,0) =
     +                        SYSTEM_MONTHLY_MISC(8,0) +
     +                              SYSTEM_MONTHLY_MISC(8,TG)
            SYSTEM_MONTHLY_MISC(3,0) =
     +                        SYSTEM_MONTHLY_MISC(3,0) +
     +                              SYSTEM_MONTHLY_MISC(3,TG)
            SYSTEM_MONTHLY_MISC(5,0) =
     +                        SYSTEM_MONTHLY_MISC(5,0) +
     +                              SYSTEM_MONTHLY_MISC(5,TG)
            SYSTEM_MONTHLY_MISC(7,0) =
     +                        SYSTEM_MONTHLY_MISC(7,0) +
     +                              SYSTEM_MONTHLY_MISC(7,TG)
            SYSTEM_MONTHLY_MISC(9,0) =
     +                        SYSTEM_MONTHLY_MISC(9,0) +
     +                              SYSTEM_MONTHLY_MISC(9,TG)
            SYSTEM_MONTHLY_MISC(19,0) =
     +                        SYSTEM_MONTHLY_MISC(19,0) +
     +                              SYSTEM_MONTHLY_MISC(19,TG)
            SYSTEM_MONTHLY_MISC(20,0) =
     +                        SYSTEM_MONTHLY_MISC(20,0) +
     +                              SYSTEM_MONTHLY_MISC(20,TG)
            SYSTEM_MONTHLY_MISC(21,0) =
     +                        SYSTEM_MONTHLY_MISC(21,0) +
     +                              SYSTEM_MONTHLY_MISC(21,TG)
            SYSTEM_MONTHLY_MISC(22,0) =
     +                        SYSTEM_MONTHLY_MISC(22,0) +
     +                              SYSTEM_MONTHLY_MISC(22,TG)
! TOTAL DEMAND
            SYSTEM_MONTHLY_MISC(15,TG) =
     +                           SYSTEM_MONTHLY_MISC(1,TG) +
     +                           SYSTEM_WHOLESALE_GENERATION(0,TG) +
     +                           SYSTEM_MONTHLY_MISC(10,TG) +
     +                           SYSTEM_MONTHLY_MISC(17,TG) +
     +                      SYSTEM_MONTHLY_MISC(19,TG) ! TRANSFER SALES
            SYSTEM_MONTHLY_MISC(15,0) = SYSTEM_MONTHLY_MISC(15,0) +
     +                                        SYSTEM_MONTHLY_MISC(15,TG)
! TOTAL SUPPLY
            SYSTEM_MONTHLY_MISC(16,TG) =
     +                           SYSTEM_HOURLY_GENERATION(0,TG) +
     +                           SYSTEM_HOURLY_HYDRO(0,TG) +
     +                           SYSTEM_MARKET_BOUGHT(0,TG) +
     +                           SYSTEM_UNSERVED_ENERGY(0,TG) +
     +                           SYSTEM_MONTHLY_MISC(18,TG) +
     +                  SYSTEM_MONTHLY_MISC(20,TG) ! TRANSFER PURCHASES
            SYSTEM_MONTHLY_MISC(16,0) = SYSTEM_MONTHLY_MISC(16,0) +
     +                                        SYSTEM_MONTHLY_MISC(16,TG)
         ENDDO
!
! NEED TO REDO REPORTING, PARTICULARLY OF THERMAL UNITS, MONTHLY 
! SUMMARY REPORT, ETC.
!
         DO U = 1, LOCAL_NUNITS
!
            CALL PUT_TRANS_C_ENERGY(U,
     +         COMBINED_GEN_IN_SYSTEM(1,U),
     +         COMBINED_GEN_IN_SYSTEM(2,U),
     +         R_HOURS_IN_MONTH)
!
            MONTH_LEFT_HEAT = COMBINED_HEAT_PARAM(1,U)/
     +                                           FLOAT(R_HOURS_IN_MONTH)
            MONTH_RIGHT_HEAT = COMBINED_HEAT_PARAM(2,U)/
     +                                           FLOAT(R_HOURS_IN_MONTH)
!
            CALL PUT_TRANS_C_HEAT(U,
     +                            MONTH_LEFT_HEAT,
     +                            MONTH_RIGHT_HEAT)
!
!
!
!
!            MONTH_UNIT_START_COST(S_U) =
!     +                           MONTH_UNIT_START_COST(S_U) +
!     +                                 MONTH_UNIT_STARTS(S_U) *
!     +                                         START_UP_COSTS(S_U)
!
!            CALL PUT_TRANS_C_START_UPS(
!     +                        A,
!     +                        MONTH_UNIT_STARTS(S_U),
!     +                        MONTH_UNIT_START_COST(S_U))
         ENDDO
!
      RETURN

      ENTRY GET_SYSTEM_PROD_BY_TG_BY_MWH(R_I,R_TG,R_J)

! I = 1 J = 6 FOR 7X24 PUMPED HYDRO
         IF(ALLOCATED(SYSTEM_PROD_BY_TG_BY_MWH)) THEN
            GET_SYSTEM_PROD_BY_TG_BY_MWH =
     +                            SYSTEM_PROD_BY_TG_BY_MWH(R_I,R_TG,R_J)
         ELSE
            GET_SYSTEM_PROD_BY_TG_BY_MWH = 0.0
         ENDIF
      RETURN

      ENTRY WRITE_TRANC_MONTHLY_SUMMARY(R_MONTH,
     +                                  R_MONTH_NAME,
     +                                  R_HOURS_IN_MONTH)

!         YES_MON_TRANC_REPORT = .TRUE.
!         IF(.NOT. YES_MON_TRANC_REPORT) RETURN
!
!         WRITE_TRANC_MONTHLY_SUMMARY = .TRUE.
         IF(MON_TRANC_REPORT_NOT_OPEN) THEN
            MON_TRANC_REPORT_NOT_OPEN = .FALSE.
!            MON_TRANC_NUM = 49
            MON_TRANC_UNIT = MON_TRANC_RPT_HEADER(MON_TRANC_NUM,
     +                                                    MON_TRANC_REC)
         ENDIF
!
! 020707.
!
         IF(MON_POSIT_REPORT_NOT_OPEN) THEN
            MON_POSIT_REPORT_NOT_OPEN = .FALSE.
            MON_POSIT_UNIT = MON_POSIT_RPT_HEADER(MON_POSIT_NUM,
     +                                                    MON_POSIT_REC)
         ENDIF
! 080609
         IF(PROD_MW_REPORT_NOT_OPEN .AND. PROD_MW_REPORT_ACTIVE) THEN
            TRANS_PROD_PROD_VARIABLES = 4 ! FOR THE FORECAST VARIABLE
            TRANS_PROD_MW_NO = TRANSACT_PROD_MW_RPT_HEADER(
     +                                       TRANS_PROD_PROD_VARIABLES,
     +                                       TRANS_PROD_MW_REC)
           PROD_MW_REPORT_NOT_OPEN = .FALSE.
         ENDIF
! 022107.
         IF( PROD_PROD_REPORT_ACTIVE .AND.
     +                                   PROD_PROD_REPORT_NOT_OPEN) THEN
!
! 022807. RESET DIMENSIONS TO ACCOMODATE NEW FORMAT
!
            TRANS_PROD_PROD_VARIABLES = 4 ! FOR THE FORECAST VARIABLE
            TRANS_PROD_PROD_NO = TRANSACT_PROD_PROD_RPT_HEADER(
     +                                       TRANS_PROD_PROD_VARIABLES,
     +                                       TRANS_PROD_PROD_REC)
!            TRANS_PROD_MW_NO = TRANSACT_PROD_MW_RPT_HEADER(
!     +                                       TRANS_PROD_PROD_VARIABLES,
!     +                                       TRANS_PROD_MW_REC)
            TRANS_PROD_FUEL_NO = TRANSACT_PROD_FUEL_RPT_HEADER(
     +                                       TRANS_PROD_PROD_VARIABLES,
     +                                       TRANS_PROD_FUEL_REC)
            PROD_PROD_REPORT_NOT_OPEN = .FALSE.
         ENDIF
!
         DO TG = 0, UPPER_TRANS_GROUP
!         TG = 1
            IF(SYSTEM_WHOLESALE_GENERATION(0,TG) > 0.1) THEN
               AVE_SELL = SYSTEM_MARKET_REVENUE(0,TG)/
     +                                 SYSTEM_WHOLESALE_GENERATION(0,TG)
            ELSE
               AVE_SELL = 0.
            ENDIF
            IF(SYSTEM_MARKET_BOUGHT(0,TG) > 0.1) THEN
               AVE_BUY = SYSTEM_MARKET_COST(0,TG)/
     +                                 SYSTEM_MARKET_BOUGHT(0,TG)
            ELSE
               AVE_BUY = 0.
            ENDIF
            IF(SYSTEM_MONTHLY_MISC(2,TG) > 0.1) THEN
               AVE_ON_PEAK_SELL = SYSTEM_MONTHLY_MISC(6,TG)/
     +                                         SYSTEM_MONTHLY_MISC(2,TG)
            ELSE
               AVE_ON_PEAK_SELL = 0.
            ENDIF
            IF(SYSTEM_MONTHLY_MISC(3,TG) > 0.1) THEN
               AVE_OFF_PEAK_SELL = SYSTEM_MONTHLY_MISC(7,TG)/
     +                                         SYSTEM_MONTHLY_MISC(3,TG)
            ELSE
               AVE_OFF_PEAK_SELL = 0.
            ENDIF
            IF(SYSTEM_MONTHLY_MISC(4,TG) > 0.1) THEN
               AVE_ON_PEAK_BUY = SYSTEM_MONTHLY_MISC(8,TG)/
     +                                         SYSTEM_MONTHLY_MISC(4,TG)
            ELSE
               AVE_ON_PEAK_BUY = 0.
            ENDIF
            IF(SYSTEM_MONTHLY_MISC(5,TG) > 0.1) THEN
               AVE_OFF_PEAK_BUY = SYSTEM_MONTHLY_MISC(9,TG)/
     +                                         SYSTEM_MONTHLY_MISC(5,TG)
            ELSE
               AVE_OFF_PEAK_BUY = 0.
            ENDIF
!
            IF(TG > 0) THEN
               TRANS_GROUP_NAME = GET_GROUP_NAME(TG)
            ELSE
               TRANS_GROUP_NAME = "System              "
            ENDIF
!
            IF(SYSTEM_HOURLY_AVAILABLE(0,TG) > .1) THEN
               EQUIVALENT_THERMAL = SYSTEM_HOURLY_AVAILABLE(0,TG)/
     +                                           FLOAT(R_HOURS_IN_MONTH)
            ELSE
               EQUIVALENT_THERMAL = 0.
            ENDIF
!
            MON_TRANC_REC = rptrec(MON_TRANC_UNIT)
            WRITE(MON_TRANC_UNIT,REC=MON_TRANC_REC)
     +       PRT_ENDPOINT(),THIS_YEAR,R_MONTH_NAME,
     +       TRANS_GROUP_NAME,SYSTEM_MARKET_PRICE(0,TG),
     +       SYSTEM_EMERGENCY_CAPACITY(0,TG),
     +       SYSTEM_HOURLY_GENERATION(0,TG),
     +       SYSTEM_HOURLY_COMMITMENT(0,TG),
     +       SYSTEM_HOURLY_DECOMMITTED(0,TG),
     +       SYSTEM_HOURLY_PRE_COMMITMENT(0,TG),
     +       EQUIVALENT_THERMAL,SYSTEM_HOURLY_COMMIT_AVAIL(0,TG),
     +       SYSTEM_HOURLY_LOAD(0,TG),SYSTEM_HOURLY_HYDRO(0,TG), ! 10
     +       SYSTEM_HOURLY_MC(0,TG),SYSTEM_HOURLY_MC_AT_LOAD(0,TG),
     +       SYSTEM_WHOLESALE_GENERATION(0,TG),SYSTEM_NATIVE_COST(0,TG),
     +       SYSTEM_UNSERVED_ENERGY(0,TG),SYSTEM_ENERGY_ABOVE(0,TG),
     +       SYSTEM_DERIVATIVES(0,TG),SYSTEM_STORAGE(0,TG),
     +       SYSTEM_AVAIL_DERIVATIVES(0,TG),
     + SYSTEM_AVAIL_STORAGE(0,TG), ! 20
     +       SYSTEM_SPIN(0,TG),SYSTEM_MARKET_BOUGHT(0,TG),
     +       SYSTEM_DSM_MW(0,TG),SYSTEM_MARKET_REVENUE(0,TG),
     +       SYSTEM_MARKET_COST(0,TG),AVE_BUY,AVE_SELL,AVE_ON_PEAK_BUY,
     +       AVE_OFF_PEAK_BUY,AVE_ON_PEAK_SELL,AVE_OFF_PEAK_SELL, ! 31
     +       (SYSTEM_MONTHLY_MISC(I,TG),I=1,NO_MISC_VARS)
!
            MON_TRANC_REC = MON_TRANC_REC + 1
!
! 020707.
!
            DO I = 1, 3 ! NUM OF PRODUCTS
               MON_POSIT_REC = rptrec(MON_POSIT_UNIT)
               WRITE(MON_POSIT_UNIT,REC=MON_POSIT_REC)
     +               PRT_ENDPOINT(),
     +               THIS_YEAR,
     +               R_MONTH_NAME,
     +               TRANS_GROUP_NAME,
     +               PRODUCT_NAME(I),
     +               (SYSTEM_MONTHLY_POSIT(I,J,TG),J=1,MON_POSIT_NUM)
!
               MON_POSIT_REC = MON_POSIT_REC + 1
!
            END DO
            IF(PROD_PROD_REPORT_ACTIVE .AND. TG /= 0) THEN
               DO J = 1, MAX_PROD_PROD_TYPES
                  TRANS_PROD_PROD_REC = rptrec(TRANS_PROD_PROD_NO)
                  WRITE(TRANS_PROD_PROD_NO,REC=TRANS_PROD_PROD_REC)
     +               PRT_ENDPOINT(),
     +               THIS_YEAR,
     +               R_MONTH_NAME,
     +               TRANS_GROUP_NAME,
     +               PRODUCT_RPT_NAME(J),
     +               (SYSTEM_PROD_BY_TG_BY_MWH(I,TG,J),I=1,3),
     +               2.0 ! FORECAST
                  TRANS_PROD_PROD_REC = TRANS_PROD_PROD_REC + 1
! 042507.
                  TRANS_PROD_FUEL_REC = rptrec(TRANS_PROD_FUEL_NO)
                  WRITE(TRANS_PROD_FUEL_NO,REC=TRANS_PROD_FUEL_REC)
     +               PRT_ENDPOINT(),
     +               THIS_YEAR,
     +               R_MONTH_NAME,
     +               TRANS_GROUP_NAME,
     +               PRODUCT_RPT_NAME(J),
     +               (SYSTEM_PROD_BY_TG_BY_FUEL(I,TG,J),I=1,3),
     +               2.0 ! FORECAST
                  TRANS_PROD_FUEL_REC = TRANS_PROD_FUEL_REC + 1
               END DO
!
               LOCAL_NAME = '   THERMAL UNIT     '
               TRANS_PROD_PROD_REC = rptrec(TRANS_PROD_PROD_NO)
               WRITE(TRANS_PROD_PROD_NO,REC=TRANS_PROD_PROD_REC)
     +               PRT_ENDPOINT(),
     +               THIS_YEAR,
     +               R_MONTH_NAME,
     +               TRANS_GROUP_NAME,
     +               LOCAL_NAME,
     +               0.0,0.0,0.0,
     +               2.0 ! FORECAST
               TRANS_PROD_PROD_REC = TRANS_PROD_PROD_REC + 1
!
               DO J = 1, NO_START_UP_UNITS_BY_TG(TG)
                  S_U = J
                  OPTION_INDEX = GET_START_UP_POSITION(S_U,TG)
          LOCAL_NAME =CLA_RETURN_UNITNM(OPTION_INDEX) 
     
                  TRANS_PROD_PROD_REC = rptrec(TRANS_PROD_PROD_NO)
                  WRITE(TRANS_PROD_PROD_NO,REC=TRANS_PROD_PROD_REC)
     +               PRT_ENDPOINT(),
     +               THIS_YEAR,
     +               R_MONTH_NAME,
     +               TRANS_GROUP_NAME,
     +               LOCAL_NAME,
     +               (PRODUCT_GEN_BY_S_U(J,I,TG),I=1,3),
     +               2.0 ! FORECAST
                  TRANS_PROD_PROD_REC = TRANS_PROD_PROD_REC + 1
!
                  TRANS_PROD_FUEL_REC = rptrec(TRANS_PROD_FUEL_NO)
                  WRITE(TRANS_PROD_FUEL_NO,REC=TRANS_PROD_FUEL_REC)
     +               PRT_ENDPOINT(),
     +               THIS_YEAR,
     +               R_MONTH_NAME,
     +               TRANS_GROUP_NAME,
     +               LOCAL_NAME,
     +               (PRODUCT_FUEL_BY_S_U(J,I,TG),I=1,3),
     +               2.0 ! FORECAST
                  TRANS_PROD_FUEL_REC = TRANS_PROD_FUEL_REC + 1
               END DO
!
            ENDIF
! 050307. MW REPORT.
       IF(PROD_MW_REPORT_ACTIVE .AND. TG /= 0) THEN ! 12 IS ARBITRARY
               DO J = 1, MAX_PROD_PROD_TYPES
!
                  AVE_MW(1) = SYSTEM_PROD_BY_TG_BY_MWH(1,TG,J)/
     +                                            PRODUCT_HOURS_POSIT(1)
                  AVE_MW(2) = SYSTEM_PROD_BY_TG_BY_MWH(2,TG,J)/
     +                                            PRODUCT_HOURS_POSIT(2)
                  AVE_MW(3) = SYSTEM_PROD_BY_TG_BY_MWH(3,TG,J)/
     +                                            PRODUCT_HOURS_POSIT(3)
!
                  TRANS_PROD_MW_REC = rptrec(TRANS_PROD_MW_NO)
                  WRITE(TRANS_PROD_MW_NO,REC=TRANS_PROD_MW_REC)
     +               PRT_ENDPOINT(),
     +               THIS_YEAR,
     +               R_MONTH_NAME,
     +               TRANS_GROUP_NAME,
     +               PRODUCT_RPT_NAME(J),
     +               AVE_MW,
     +               2.0 ! FORECAST
                  TRANS_PROD_MW_REC = TRANS_PROD_MW_REC + 1
               ENDDO
!
               LOCAL_NAME = '   THERMAL UNIT     '
               TRANS_PROD_MW_REC = rptrec(TRANS_PROD_MW_NO)
               WRITE(TRANS_PROD_MW_NO,REC=TRANS_PROD_MW_REC)
     +               PRT_ENDPOINT(),
     +               THIS_YEAR,
     +               R_MONTH_NAME,
     +               TRANS_GROUP_NAME,
     +               LOCAL_NAME,
     +               0.0,0.0,0.0,
     +               2.0 ! FORECAST
               TRANS_PROD_MW_REC = TRANS_PROD_MW_REC + 1
!
               DO J = 1, NO_START_UP_UNITS_BY_TG(TG)
                  S_U = J
                  OPTION_INDEX = GET_START_UP_POSITION(S_U,TG)
                  LOCAL_NAME=CLA_RETURN_UNITNM(OPTION_INDEX) 
                  AVE_MW(1) = PRODUCT_GEN_BY_S_U(J,1,TG)/
     +                                            PRODUCT_HOURS_POSIT(1)
                  AVE_MW(2) = PRODUCT_GEN_BY_S_U(J,2,TG)/
     +                                            PRODUCT_HOURS_POSIT(2)
                  AVE_MW(3) = PRODUCT_GEN_BY_S_U(J,3,TG)/
     +                                            PRODUCT_HOURS_POSIT(3)
                  TRANS_PROD_MW_REC = rptrec(TRANS_PROD_MW_NO)
                  WRITE(TRANS_PROD_MW_NO,REC=TRANS_PROD_MW_REC)
     +               PRT_ENDPOINT(),
     +               THIS_YEAR,
     +               R_MONTH_NAME,
     +               TRANS_GROUP_NAME,
     +               LOCAL_NAME,
     +               AVE_MW,
     +               2.0 ! FORECAST
                  TRANS_PROD_MW_REC = TRANS_PROD_MW_REC + 1
               ENDDO
            ENDIF
!
            ANNUAL_TRANC_REPORT(1,TG) = ANNUAL_TRANC_REPORT(1,TG) +
     +                                         SYSTEM_MARKET_PRICE(0,TG)
            ANNUAL_TRANC_REPORT(2,TG) = ANNUAL_TRANC_REPORT(2,TG) +
     +               SYSTEM_EMERGENCY_CAPACITY(0,TG)
            ANNUAL_TRANC_REPORT(3,TG) = ANNUAL_TRANC_REPORT(3,TG) +
     +               SYSTEM_HOURLY_GENERATION(0,TG)
            ANNUAL_TRANC_REPORT(4,TG) = ANNUAL_TRANC_REPORT(4,TG) +
     +               SYSTEM_HOURLY_COMMITMENT(0,TG)
            ANNUAL_TRANC_REPORT(5,TG) = ANNUAL_TRANC_REPORT(5,TG) +
     +               SYSTEM_HOURLY_DECOMMITTED(0,TG)
            ANNUAL_TRANC_REPORT(6,TG) = ANNUAL_TRANC_REPORT(6,TG) +
     +               SYSTEM_HOURLY_PRE_COMMITMENT(0,TG)
            ANNUAL_TRANC_REPORT(7,TG) = ANNUAL_TRANC_REPORT(7,TG) +
     +               SYSTEM_HOURLY_AVAILABLE(0,TG)
            ANNUAL_TRANC_REPORT(8,TG) = ANNUAL_TRANC_REPORT(8,TG) +
     +               SYSTEM_HOURLY_COMMIT_AVAIL(0,TG)
            ANNUAL_TRANC_REPORT(9,TG) = ANNUAL_TRANC_REPORT(9,TG) +
     +               SYSTEM_HOURLY_LOAD(0,TG)
            ANNUAL_TRANC_REPORT(10,TG) = ANNUAL_TRANC_REPORT(10,TG) +
     +               SYSTEM_HOURLY_HYDRO(0,TG) ! 10
            ANNUAL_TRANC_REPORT(11,TG) = ANNUAL_TRANC_REPORT(11,TG) +
     +               SYSTEM_HOURLY_MC(0,TG)
            ANNUAL_TRANC_REPORT(12,TG) = ANNUAL_TRANC_REPORT(12,TG) +
     +               SYSTEM_HOURLY_MC_AT_LOAD(0,TG)
            ANNUAL_TRANC_REPORT(13,TG) = ANNUAL_TRANC_REPORT(13,TG) +
     +               SYSTEM_WHOLESALE_GENERATION(0,TG)
            ANNUAL_TRANC_REPORT(14,TG) = ANNUAL_TRANC_REPORT(14,TG) +
     +               SYSTEM_NATIVE_COST(0,TG)
            ANNUAL_TRANC_REPORT(15,TG) = ANNUAL_TRANC_REPORT(15,TG) +
     +               SYSTEM_UNSERVED_ENERGY(0,TG)
            ANNUAL_TRANC_REPORT(16,TG) = ANNUAL_TRANC_REPORT(16,TG) +
     +               SYSTEM_ENERGY_ABOVE(0,TG)
            ANNUAL_TRANC_REPORT(17,TG) = ANNUAL_TRANC_REPORT(17,TG) +
     +               SYSTEM_DERIVATIVES(0,TG)
            ANNUAL_TRANC_REPORT(18,TG) = ANNUAL_TRANC_REPORT(18,TG) +
     +               SYSTEM_STORAGE(0,TG)
            ANNUAL_TRANC_REPORT(19,TG) = ANNUAL_TRANC_REPORT(19,TG) +
     +               SYSTEM_AVAIL_DERIVATIVES(0,TG)
            ANNUAL_TRANC_REPORT(20,TG) = ANNUAL_TRANC_REPORT(20,TG) +
     +               SYSTEM_AVAIL_STORAGE(0,TG) ! 20
            ANNUAL_TRANC_REPORT(21,TG) = ANNUAL_TRANC_REPORT(21,TG) +
     +               SYSTEM_SPIN(0,TG)
            ANNUAL_TRANC_REPORT(22,TG) = ANNUAL_TRANC_REPORT(22,TG) +
     +               SYSTEM_MARKET_BOUGHT(0,TG)
            ANNUAL_TRANC_REPORT(23,TG) = ANNUAL_TRANC_REPORT(23,TG) +
     +               SYSTEM_DSM_MW(0,TG)
            ANNUAL_TRANC_REPORT(24,TG) = ANNUAL_TRANC_REPORT(24,TG) +
     +               SYSTEM_MARKET_REVENUE(0,TG)
            ANNUAL_TRANC_REPORT(25,TG) = ANNUAL_TRANC_REPORT(25,TG) +
     +               SYSTEM_MARKET_COST(0,TG)
            ANNUAL_TRANC_REPORT(26,TG) = ANNUAL_TRANC_REPORT(26,TG) +
     +               AVE_BUY
            ANNUAL_TRANC_REPORT(27,TG) = ANNUAL_TRANC_REPORT(27,TG) +
     +               AVE_SELL
            ANNUAL_TRANC_REPORT(28,TG) = ANNUAL_TRANC_REPORT(28,TG) +
     +               AVE_ON_PEAK_BUY
            ANNUAL_TRANC_REPORT(29,TG) = ANNUAL_TRANC_REPORT(29,TG) +
     +               AVE_OFF_PEAK_BUY
            ANNUAL_TRANC_REPORT(30,TG) = ANNUAL_TRANC_REPORT(30,TG) +
     +               AVE_ON_PEAK_SELL
            ANNUAL_TRANC_REPORT(31,TG) = ANNUAL_TRANC_REPORT(31,TG) +
     +               AVE_OFF_PEAK_SELL ! 31
            DO I =1, NO_MISC_VARS
               IF(I == 26 .OR. I == 27) THEN
                  ANNUAL_TRANC_REPORT(31+I,TG) =
     +                     MAX(ANNUAL_TRANC_REPORT(31+I,TG),
     +                                      SYSTEM_MONTHLY_MISC(I,TG))
               ELSEIF(I == 29) THEN
                  ANNUAL_TRANC_REPORT(31+I,TG) =
     +                     ANNUAL_TRANC_REPORT(57,TG) -
     +                        ANNUAL_TRANC_REPORT(58,TG) +
     +                                      ANNUAL_TRANC_REPORT(59,TG)
               ELSEIF(I < 11 .OR. I > 12) THEN
                  ANNUAL_TRANC_REPORT(31+I,TG) =
     +                     ANNUAL_TRANC_REPORT(31+I,TG) +
     +                                      SYSTEM_MONTHLY_MISC(I,TG)
               ELSEIF(I == 11) THEN ! 11/11/03. PEAK
                  ANNUAL_TRANC_REPORT(31+I,TG) =
     +                     MAX(ANNUAL_TRANC_REPORT(31+I,TG),
     +                                      SYSTEM_MONTHLY_MISC(I,TG))
               ELSE ! 11/11/03. MIN
                  ANNUAL_TRANC_REPORT(31+I,TG) =
     +                     MIN(ANNUAL_TRANC_REPORT(31+I,TG),
     +                                      SYSTEM_MONTHLY_MISC(I,TG))
               ENDIF
            ENDDO
!
         ENDDO
         IF(R_MONTH == 12) THEN
            DO TG = 0, UPPER_TRANS_GROUP
!
               IF(TG > 0) THEN
                  TRANS_GROUP_NAME = GET_GROUP_NAME(TG)
               ELSE
                  TRANS_GROUP_NAME = "System              "
               ENDIF
!
               IF(ANNUAL_TRANC_REPORT(7,TG) > .1) THEN
                  ANNUAL_TRANC_REPORT(7,TG) = ANNUAL_TRANC_REPORT(7,TG)/
     +                                                             8760.
               ELSE
                  ANNUAL_TRANC_REPORT(7,TG) = 0.
               ENDIF
               IF(ANNUAL_TRANC_REPORT(13,TG) > 0.1) THEN
                  ANNUAL_TRANC_REPORT(27,TG) =
     +                     ANNUAL_TRANC_REPORT(24,TG)/
     +                                 ANNUAL_TRANC_REPORT(13,TG)
               ELSE
                  ANNUAL_TRANC_REPORT(27,TG) = 0.
               ENDIF

               IF(ANNUAL_TRANC_REPORT(22,TG) > 0.1) THEN
                  ANNUAL_TRANC_REPORT(26,TG) =
     +                     ANNUAL_TRANC_REPORT(25,TG)/
     +                                 ANNUAL_TRANC_REPORT(22,TG)
               ELSE
                  ANNUAL_TRANC_REPORT(26,TG) = 0.
               ENDIF

               IF(ANNUAL_TRANC_REPORT(33,TG) > 0.1) THEN
                  ANNUAL_TRANC_REPORT(30,TG) =
     +                        ANNUAL_TRANC_REPORT(37,TG)/
     +                                        ANNUAL_TRANC_REPORT(33,TG)
               ELSE
                  ANNUAL_TRANC_REPORT(30,TG) = 0.
               ENDIF
               IF(ANNUAL_TRANC_REPORT(34,TG) > 0.1) THEN
                  ANNUAL_TRANC_REPORT(31,TG) =
     +                        ANNUAL_TRANC_REPORT(38,TG)/
     +                                        ANNUAL_TRANC_REPORT(34,TG)
               ELSE
                  ANNUAL_TRANC_REPORT(31,TG) = 0.
               ENDIF

               IF(ANNUAL_TRANC_REPORT(35,TG) > 0.1) THEN
                  ANNUAL_TRANC_REPORT(28,TG) =
     +                        ANNUAL_TRANC_REPORT(39,TG)/
     +                                        ANNUAL_TRANC_REPORT(35,TG)
               ELSE
                  ANNUAL_TRANC_REPORT(28,TG) = 0.
               ENDIF

               IF(ANNUAL_TRANC_REPORT(36,TG) > 0.1) THEN
                  ANNUAL_TRANC_REPORT(29,TG) =
     +                        ANNUAL_TRANC_REPORT(40,TG)/
     +                                        ANNUAL_TRANC_REPORT(36,TG)
               ELSE
                  ANNUAL_TRANC_REPORT(29,TG) = 0.
               ENDIF
               IF(ABS(ANNUAL_TRANC_REPORT(57,TG) -
     +                           ANNUAL_TRANC_REPORT(58,TG)) > .01) THEN
                  ANNUAL_TRANC_REPORT(61,TG) = 100.*
     +                  (ANNUAL_TRANC_REPORT(57,TG)/
     +                     (ANNUAL_TRANC_REPORT(58,TG) -
     +                           ANNUAL_TRANC_REPORT(59,TG)) - 1.)
               ELSE
                  ANNUAL_TRANC_REPORT(61,TG) = 0.0
               ENDIF
!
               MON_TRANC_REC = rptrec(MON_TRANC_UNIT)
               WRITE(MON_TRANC_UNIT,REC=MON_TRANC_REC)
     +               PRT_ENDPOINT(),
     +               THIS_YEAR,
     +               "Annual   ",
     +               TRANS_GROUP_NAME,
     +               (ANNUAL_TRANC_REPORT(I,TG),I=1,MON_TRANC_NUM)
!
               MON_TRANC_REC = MON_TRANC_REC + 1
            ENDDO
         ENDIF
      RETURN

      ENTRY GET_TRANSACT_C_STATUS()

         GET_TRANSACT_C_STATUS = SAVE_TRANSACT_C_STATUS
      RETURN

      ENTRY GET_MONTHLY_DUKE_RESERVE_ENERGY()

         GET_MONTHLY_DUKE_RESERVE_ENERGY = MONTHLY_RESERVE_ENERGY
      RETURN

      ENTRY GET_DUKE_RESERVE_CAPACITY()

!
         GET_DUKE_RESERVE_CAPACITY = RESERVE_CAPACITY
!
      RETURN
      END
C

!
      subroutine INT_Sort(iSup,a)
!

! sorts iSup items into increasing order based on values in (1-based) ar
!ray a;
! Shell sort adapted from the source on page 110 of
! SOFTWARE TOOLS IN PASCAL, by Kernighan & Plauger, Addison-Wesley 1981
      INTEGER(kind=2) :: iSup,i,j,k,m,Gap,a(*)

      Gap=iSup/2
      do while(Gap>0)
         do i=Gap+1,iSup
            j=i-Gap
            do while(j>0)
               k=j+Gap
               if(a(j)<=a(k)) then
            j=0 ! break the while loop 
               else ! interchange the array values
                  m   =a(j)
                  a(j)=a(k)
                  a(k)=m
               end if
               j=j-Gap
            end do
         end do
         Gap=Gap/2
      end do
      end ! subroutine INT_Sort

!
      subroutine INT_Sort_4_I4(iSup,a)
!

! sorts iSup items into increasing order based on values in (1-based) 
! array a;
! Shell sort adapted from the source on page 110 of
! SOFTWARE TOOLS IN PASCAL, by Kernighan & Plauger, Addison-Wesley 1981
      INTEGER(kind=4) :: iSup,i,j,k,m,Gap,a(*)
      Gap=iSup/2
      do while(Gap>0)
         do i=Gap+1,iSup
            j=i-Gap
            do while(j>0)
               k=j+Gap
               if(a(j)<=a(k)) then
      j=0 ! break the while loop 
               else ! interchange the array values
                  m   =a(j)
                  a(j)=a(k)
                  a(k)=m
               end if
               j=j-Gap
            end do
         end do
         Gap=Gap/2
      end do
      end ! subroutine INT_Sort

!
      subroutine INT2_Sort(iSup,iPos,a)
!

! sorts iSup items into order based on values in key (1-based) array a;
! Shell sort adapted from the source on page 110 of
! SOFTWARE TOOLS IN PASCAL, by Kernighan & Plauger, Addison-Wesley 1981
      INTEGER(kind=2) :: i,j,k,p,q,Gap,iSup,iPos(iSup),a(iSup)
      LOGICAL(kind=1) :: Increasing,Decreasing
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
               j=0 ! break the while loop 
             else ! interchange the indexing array values
               iPos(k)=p
               iPos(j)=q
             end if
             j=j-Gap
           end do
         end do
         Gap=Gap/2
      end do
      end ! subroutine INT2_Sort

!
      subroutine INT4_Sort(iSup,iPos,a)
!

! sorts iSup items into order based on values in key (1-based) array a;
! Shell sort adapted from the source on page 110 of
! SOFTWARE TOOLS IN PASCAL, by Kernighan & Plauger, Addison-Wesley 1981
      INTEGER(kind=4) :: i,j,k,p,q,Gap,iSup,iPos(iSup),a(iSup)
      LOGICAL(kind=1) :: Increasing,Decreasing
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
               j=0 ! break the while loop 
             else ! interchange the indexing array values
               iPos(k)=p
               iPos(j)=q
             end if
             j=j-Gap
           end do
         end do
         Gap=Gap/2
      end do
      end ! subroutine INT2_Sort

      FUNCTION TRANS_GROUP_SCARCITY_VALUE(R_TRANS_GROUP,
     +                                    R_MONTH,
     +                                    R_YEAR,
     +                                    R_TRANS_CAP,
     +                                    R_TRANS_MAX_CAP)

C
C
      INTEGER(kind=2) :: R_TRANS_GROUP,R_MONTH,R_YEAR,I
      REAL     R_TRANS_CAP,
     +         R_TRANS_MAX_CAP,
     +         TRANS_GROUP_SCARCITY_VALUE,
     +         SCARCITY_VALUES_1,
     +         SCARCITY_CAP_PERCENT_1,
     +         SCARCITY_VALUES_2,
     +         SCARCITY_CAP_PERCENT_2,
     +         SCARCITY_VALUES_3,
     +         SCARCITY_CAP_PERCENT_3,
     +         SCARCITY_CAPACITY_ADDER,
     +         NEW_SCARCITY_VALUES(7),
     +         NEW_SCARCITY_CAP_PERCENT(7),
     +         SCARCITY_VALUES(10),
     +         SCARCITY_CAP_PERCENT(10),
     +         TOTAL_SCARCITY_CAPACITY,
     +         SLOPE
      LOGICAL(kind=1) :: GET_SCARCITY_INFO,VOID_LOGICAL
!
! END DATA DECLARATIONS
!
         TRANS_GROUP_SCARCITY_VALUE = 0.
         VOID_LOGICAL = GET_SCARCITY_INFO(R_TRANS_GROUP,
     +                                    SCARCITY_VALUES_1,
     +                                    SCARCITY_CAP_PERCENT_1,
     +                                    SCARCITY_VALUES_2,
     +                                    SCARCITY_CAP_PERCENT_2,
     +                                    SCARCITY_VALUES_3,
     +                                    SCARCITY_CAP_PERCENT_3,
     +                                    R_MONTH,
     +                                    R_YEAR,
     +                                    SCARCITY_CAPACITY_ADDER,
     +                                    NEW_SCARCITY_VALUES,
     +                                    NEW_SCARCITY_CAP_PERCENT)
C
         SCARCITY_VALUES(1)  = SCARCITY_VALUES_1
         SCARCITY_CAP_PERCENT(1) = SCARCITY_CAP_PERCENT_1
         SCARCITY_VALUES(2) = SCARCITY_VALUES_2
         SCARCITY_CAP_PERCENT(2) = SCARCITY_CAP_PERCENT_2
         SCARCITY_VALUES(3) = SCARCITY_VALUES_3
         SCARCITY_CAP_PERCENT(3) = SCARCITY_CAP_PERCENT_3
         DO I = 1, 7
            SCARCITY_VALUES(I+3) = NEW_SCARCITY_VALUES(I)
            SCARCITY_CAP_PERCENT(I+3) = NEW_SCARCITY_CAP_PERCENT(I)
         ENDDO

         TOTAL_SCARCITY_CAPACITY = R_TRANS_MAX_CAP
     +                             + SCARCITY_CAPACITY_ADDER
         SCARCITY_CAP_PERCENT(1) = SCARCITY_CAP_PERCENT(1)/100.
         IF(R_TRANS_CAP <= SCARCITY_CAP_PERCENT(1) *
     +                                   TOTAL_SCARCITY_CAPACITY) RETURN
         DO I = 2, 10
            SCARCITY_CAP_PERCENT(I) = SCARCITY_CAP_PERCENT(I)/100.
            IF(R_TRANS_CAP <= SCARCITY_CAP_PERCENT(I) *
     +                                     TOTAL_SCARCITY_CAPACITY) THEN
               SLOPE = 0.
               IF(SCARCITY_CAP_PERCENT(I)-SCARCITY_CAP_PERCENT(I-1)/=0.)
     +              SLOPE = (SCARCITY_VALUES(I) - SCARCITY_VALUES(I-1))/
     +             (SCARCITY_CAP_PERCENT(I) - SCARCITY_CAP_PERCENT(I-1))
               TRANS_GROUP_SCARCITY_VALUE = SLOPE *
     +                              (R_TRANS_CAP/TOTAL_SCARCITY_CAPACITY
     +                                      - SCARCITY_CAP_PERCENT(I-1))
     +                               + SCARCITY_VALUES(I-1)
               EXIT
            ELSEIF(SCARCITY_CAP_PERCENT(I) <= 0.) THEN
               TRANS_GROUP_SCARCITY_VALUE = SCARCITY_VALUES(I-1)
               EXIT
            ENDIF
            TRANS_GROUP_SCARCITY_VALUE = SCARCITY_VALUES(I)
         ENDDO
      RETURN
      END
!
! **********************************************************************
      FUNCTION CX_UNIT(
     +                         R_TIME_FRAME,R_CX_ITER,R_UNIT_NAME,
     +                         R_BLOCK_CAPACITY,R_BLOCK_HEAT_RATE,
     +                         R_AVE_FUEL_COST,R_AVE_VOM_COST,
     +                         R_AVE_EMIS_COST,R_MONTHLY_CAP_MULT,
     +                         R_MIN_UP_TIME,R_MIN_DOWN_TIME,
     +                         R_START_UP_COSTS,R_MUST_RUN,
     +                         R_OPTION_INDEX,R_MONTH,R_PRICE,
     +                         R_USAGE,R_STRIKE_PRICE,
     +                         R_HOURS,R_STARTS,R_ENERGY,
     +                         R_ENERGY_REVENUE,R_ENERGY_COST,
     +                         R_FUEL_COST,R_VOM_COST,R_EMIS_COST)
! **********************************************************************
!
         use SpinDriftLib
         use prod_arrays_dimensions
         USE IREC_ENDPOINT_CONTROL
         use grx_planning_routines
         USE TRAN_C_VARIABLES
         use rptreccontrol
         
         USE SIZECOM
         use globecom
         use prodcom
         use mod_base_year
         
         implicit none
         real (kind=4) :: ave_buy
         
         LOGICAL(kind=1) ::
     +          UNIT_UP_LAST_HOUR,LOOK_AHEAD_LOGIC,START_TEST,
     +          RUN_TEST,STOP_TEST,CURRENT_STATE,R_UNIT_UP_YESTERDAY,
     +          MUST_RUN_UNIT,REPORT_THIS_CL_UNIT,END_OF_DAY,
     +          YES_HOURLY_COMMITMENT,HOURLY_COMMITMENT,
     +          AJ_SUMMARY_REPORT,AJ_REPORT_NOT_OPEN=.TRUE.,
     +          AK_SUMMARY_REPORT,AK_REPORT_NOT_OPEN=.TRUE.,
     +          TEMP_L,COMMIT_ON_TOTAL_COST,
     +          YES_COMMIT_ON_TOTAL_COST,TEST_MONTHLY_MUST_RUN,
     +          CX_UNIT,IN_PRE_PERIOD,IN_POST_PERIOD,

     +          R_LOGICAL,UNIT_ON_OUTAGE(800),CX_DIAGNOSTIC_REPORTS,
     +          YES_CX_DIAGNOSTIC_REPORTS,R_TIME_FRAME,
     +          R_MUST_RUN(12)
         LOGICAL ::
     +          ADVANCED_LOOK_AHEAD_LOGIC,
     +          POSITIVE_HOURLY_MARGIN,
     +          MAX_RECOVERY_OF_SU_COST,
     +          POS_MARGIN_FOR_UP_TIME,
     +          MIN_DOWN_TIME_SATISFIED,
     +          UP_LAST_HOUR,
     +          MIN_RECOVERY_OF_SU_COST,
     +          MIN_UP_TIME_SATISFIED,
     +          ALWAYS_LOSE_MONEY_FOR_UP_TIME,
     +          ALWAYS_LOSE_MONEY_FOR_MAX_TIME,
     +          KEEP_OFF_FOR_MIN_DOWN_TIME,
     +          UNIT_STARTS_THIS_HOUR
      INTEGER(kind=2) :: J,K,HOURS_LEFT,S_U,OPTION_INDEX,R_OPTION_INDEX,
     +          HOUR,HR,PRESIM_HOURS,POSTSIM_HOURS,DAY,DY,R_MONTH,
     +          MONTH,HOURS_IN_MONTH,CUMULATIVE_HOURS,MIN_UP_TIME_INT,
     +          MIN_DOWN_TIME_INT,MIN_UP_TIME_SAVE,MIN_DOWN_TIME_SAVE,
     +          HOURS_FOR_ECON_TEST,MAX_HOURS_FOR_ECON_TEST,
     +          POSITIVE_HOURS,NEGATIVE_HOURS,A,GET_START_UP_INDEX,
!
     +          AJ_REPORT_VARIABLES,AK_REPORT_VARIABLES,AJ_HOURLY_NO=0,
     +          AJ_HOURLY_HEADER,AK_HOURLY_NO=0,AK_HOURLY_HEADER,
     +          START_MONTH,END_MONTH,BEST_BLOCK_BY_(744),
     +          HOUR_IN_TIMEFRAME,R_MIN_UP_TIME,R_MIN_DOWN_TIME,
     +          R_CX_ITER
      INTEGER :: AJ_HOURLY_REC,
     +          AK_HOURLY_REC,
     +          HOURS_SINCE_START_UP,
     +          HOURS_SINCE_SHUT_DOWN
!     +          HOURS_SINCE_START_UP(:),
!     +          HOURS_SINCE_SHUT_DOWN(:)

      REAL(kind=4) ::
     +          BLOCK_CAPACITY(2),FIAT_BLOCK_CAPACITY(2),
     +          BLOCK_PRICE(2),BLOCK_COST(744,2),BLOCK_FUEL_COST(744,2),
     +          BLOCK_VOM_COST(744,2),BLOCK_EMIS_COST(744,2),
     +          BLOCK_REV(744,2),BLOCK_MARGIN(744,2),
     +          BEST_BLOCK_MARGIN(744),BEST_BLOCK_CAPACITY(744),
     +          LOOK_AHEAD_MARGIN(-4:932),MARGIN_SINCE_SHUT_DOWN,
     +          MARGIN_SINCE_START_UP,CUMULATIVE_MARGIN,
     +          POSITIVE_HOURS_MARGIN,NEGATIVE_HOURS_MARGIN,
     +          UNIT_DISP_BTU_COST,UNIT_FUEL_PARAM(2),
     +          UNIT_NON_FUEL_PARAM(2),TOTAL_BTU_COST,
     +          TOTAL_FUEL_PARAM(2),TOTAL_NON_FUEL_PARAM(2),TEMP_R,
     +          GET_BLOCK_AVAIL_4_MONTH,FIRST_BLOCK_AVAIL(800),
     +          SECOND_BLOCK_AVAIL(800),FIRST_BLOCK_OUT(800),
     +          SECOND_BLOCK_OUT(800),FIRST_DAILY_OUT(31),
     +          SECOND_DAILY_OUT(31),GET_MIN_UP_TIME,GET_MIN_DOWN_TIME,
     +          START_UP_COSTS,R_START_UP_COSTS(12),

     +          GET_TOTAL_INC_COST_PARAMS
      REAL(kind=4) ::
     +          R_PRICE(*),LOCAL_PRICE(8784),R_USAGE(*),
     +          R_STRIKE_PRICE(*),LOCAL_STRIKE_PRICE(8784),
     +          LOCAL_USAGE(8784),LOCAL_FIRST_CAP(8784),
     +          LOCAL_SECOND_CAP(8784),
!     +          NGas_VolatilityByHour(744),
     +          THIS_YEAR,IOTA=0.0001,GET_ANNUAL_CL_CAPACITY,
     +          R_BLOCK_CAPACITY(5),R_BLOCK_HEAT_RATE(5),
     +          R_AVE_FUEL_COST(12),R_AVE_VOM_COST,R_AVE_EMIS_COST,
     +          R_MONTHLY_CAP_MULT(12),
! OUTPUT
     +          R_HOURS,R_STARTS,R_ENERGY,R_ENERGY_REVENUE,
     +          R_ENERGY_COST,MONTH_HOURS(12),MONTH_STARTS(12),
     +          MONTH_ENERGY(12),MONTH_ENERGY_REVENUE(12),
     +          MONTH_ENERGY_COST(12),MONTH_FUEL_COST(12),
     +          MONTH_VOM_COST(12),MONTH_EMIS_COST(12),R_FUEL_COST,
     +          R_VOM_COST,R_EMIS_COST,TOTAL_CUMULATIVE_MARGIN
      CHARACTER(len=9) :: CL_MONTH_NAME(14)=
     +                         (/'January  ','February ',
     +                          'March    ','April    ',
     +                          'May      ','June     ',
     +                          'July     ','August   ',
     +                          'September','October  ',
     +                          'November ','December ',
     +                          'Annual   ','Fiscal   '/)
      CHARACTER(len=20) :: R_UNIT_NAME

               IF(R_TIME_FRAME) THEN
                  START_MONTH = 1
                  END_MONTH = 12
                  LOCAL_PRICE(1:8784) = R_PRICE(1:8784)
!                  LOCAL_USAGE(1:8784) = R_USAGE(1:8784)
               ELSE
                  START_MONTH = R_MONTH
                  END_MONTH = R_MONTH
                  LOCAL_PRICE(1:800) = R_PRICE(1:800)
!                  LOCAL_USAGE(1:800) = R_USAGE(1:800)
               ENDIF
!               LOCAL_USAGE = 0.0
               LOCAL_USAGE = - 99.
               LOCAL_STRIKE_PRICE = 999999.0 ! ASSUMES NOT VIABLE
               R_HOURS = 0.0
               R_STARTS = 0.0
               R_ENERGY = 0.0
               R_ENERGY_REVENUE = 0.0
               R_ENERGY_COST = 0.0
               R_FUEL_COST = 0.0
               R_VOM_COST = 0.0
               R_EMIS_COST = 0.0
               TOTAL_CUMULATIVE_MARGIN = 0.0
!
               MONTH_HOURS = 0.0
               MONTH_STARTS = 0.0
               MONTH_ENERGY = 0.0
               MONTH_ENERGY_REVENUE = 0.0
               MONTH_ENERGY_COST = 0.0
               MONTH_FUEL_COST = 0.0
               MONTH_VOM_COST = 0.0
               MONTH_EMIS_COST = 0.0
!
               HOURS_SINCE_START_UP = 12
               HOURS_SINCE_SHUT_DOWN = 0
               UNIT_UP_LAST_HOUR = .TRUE.
!
! MONTHLY DIMENSION:
            if(hours_in_month>744) then
                call end_program ("tf_objt:0011 - hours_in_month " //
     + "value of " // trim(itos(int(hours_in_month))) // 
     + " is invalid.")
            end if
               DO MONTH = START_MONTH, END_MONTH
                  OPTION_INDEX = R_OPTION_INDEX
                  CALL GET_CUM_HOURS_N_HOURS_IN_MONTH(
     +                                            MONTH,
     +                                            HOURS_IN_MONTH,
     +                                            CUMULATIVE_HOURS)
                  THIS_YEAR = FLOAT(get_BASE_YEAR() + globecom_YEAR)
                  CX_UNIT = .TRUE.

                  MUST_RUN_UNIT = R_MUST_RUN(MONTH)

                  COMMIT_ON_TOTAL_COST = YES_COMMIT_ON_TOTAL_COST()
                  MIN_UP_TIME_SAVE =  R_MIN_UP_TIME

                  MIN_DOWN_TIME_SAVE = R_MIN_DOWN_TIME

                  START_UP_COSTS = R_START_UP_COSTS(MONTH)


!
                  FIAT_BLOCK_CAPACITY = -999999.
!
! 061615. THIS SECTION SHOULD BECOME A GENERAL HOURLY AVAILABILITY 
! PROGRAM
! 062015. NOTE: ADDED R_CAPACITY AS A MULTIPLIER * 100
                  IF(R_TIME_FRAME) THEN
                     FIAT_BLOCK_CAPACITY(1) = R_BLOCK_CAPACITY(1)
                     FIAT_BLOCK_CAPACITY(2) = MAX(R_BLOCK_CAPACITY(2),
     +                                           FIAT_BLOCK_CAPACITY(1))
                     FIRST_BLOCK_AVAIL =
     +                     R_BLOCK_CAPACITY(1) *
     +                                    R_MONTHLY_CAP_MULT(MONTH)
                     SECOND_BLOCK_AVAIL =
     +                     R_BLOCK_CAPACITY(2) *
     +                                    R_MONTHLY_CAP_MULT(MONTH) -
     +                                       R_BLOCK_CAPACITY(1) *
     +                                         R_MONTHLY_CAP_MULT(MONTH)
                     BLOCK_PRICE(1) = R_AVE_FUEL_COST(MONTH) +
     +                                          R_AVE_VOM_COST +
     +                                                   R_AVE_EMIS_COST
                     BLOCK_PRICE(2) = R_AVE_FUEL_COST(MONTH) +
     +                                          R_AVE_VOM_COST +
     +                                                   R_AVE_EMIS_COST

                  ELSE
!
! 072315. NEED TO GET DYNAMIC FUEL PRICING BACK IN.
!
                     CALL GET_DISP_PARAM_FOR_A_UNIT(OPTION_INDEX,
     +                                UNIT_DISP_BTU_COST,
     +                                UNIT_FUEL_PARAM(1),
     +                                UNIT_NON_FUEL_PARAM(1),
     +                                UNIT_FUEL_PARAM(2),
     +                                UNIT_NON_FUEL_PARAM(2))
                     TEMP_R = GET_TOTAL_INC_COST_PARAMS(
     +                                OPTION_INDEX,
     +                                TOTAL_BTU_COST,
     +                                TOTAL_FUEL_PARAM(1),
     +                                TOTAL_NON_FUEL_PARAM(1),
     +                                TOTAL_FUEL_PARAM(2),
     +                                TOTAL_NON_FUEL_PARAM(2))
                     TEMP_R =
     +                    GET_BLOCK_AVAIL_4_MONTH(OPTION_INDEX,INT(1,2),
     +                                                FIRST_BLOCK_AVAIL,
     +                                                FIRST_BLOCK_OUT,
     +                                                FIRST_DAILY_OUT)
                     TEMP_R =
     +                    GET_BLOCK_AVAIL_4_MONTH(OPTION_INDEX,INT(2,2),
     +                                               SECOND_BLOCK_AVAIL,
     +                                               SECOND_BLOCK_OUT,
     +                                               SECOND_DAILY_OUT)
            if(hours_in_month>744) then
                call end_program ("tf_objt:0012 - hours_in_month " //
     + "value of " // trim(itos(int(hours_in_month))) // 
     + " is invalid.")
            end if
                     DO HOUR = 1, HOURS_IN_MONTH
                        FIAT_BLOCK_CAPACITY(1) =
     +                     MAX(FIRST_BLOCK_AVAIL(HOUR),
     +                                           FIAT_BLOCK_CAPACITY(1))
                        FIAT_BLOCK_CAPACITY(2) =
     +                     MAX(SECOND_BLOCK_AVAIL(HOUR),
     +                                           FIAT_BLOCK_CAPACITY(2))
                     ENDDO
                     FIAT_BLOCK_CAPACITY(2) = FIAT_BLOCK_CAPACITY(2) +
     +                                            FIAT_BLOCK_CAPACITY(1)
                  ENDIF
                  IF(FIAT_BLOCK_CAPACITY(2) < 0.001) THEN

                     LOCAL_USAGE = 0.0
                     RETURN ! NO CAPACITY TO EVALUATE
                  ENDIF

                  CX_HOUR_BLOCK_PRICE = 0.0
! START OF THE AI ROUTINE.
                  LOOK_AHEAD_LOGIC = .TRUE.
                  BLOCK_COST = 0.
                  BLOCK_FUEL_COST = 0.
                  BLOCK_VOM_COST = 0.
                  BLOCK_EMIS_COST = 0.
                  BLOCK_REV = 0.
                  BLOCK_MARGIN = 0.
                  BEST_BLOCK_CAPACITY = 0.
                  BEST_BLOCK_MARGIN = 0.
!                  HOURS_SINCE_START_UP = 12
!                  HOURS_SINCE_SHUT_DOWN = 0
                  MARGIN_SINCE_SHUT_DOWN = 0.
                  MARGIN_SINCE_START_UP = 0.
                  CUMULATIVE_MARGIN = 0.
!                  UNIT_UP_LAST_HOUR = .TRUE.
                  POSITIVE_HOURS = 0
                  POSITIVE_HOURS_MARGIN = 0.
                  NEGATIVE_HOURS = 0
                  NEGATIVE_HOURS_MARGIN = 0.
!
                  MIN_UP_TIME_INT = MIN_UP_TIME_SAVE
                  MIN_DOWN_TIME_INT = MIN_DOWN_TIME_SAVE
                  MAX_HOURS_FOR_ECON_TEST = 96
                  HOURS_FOR_ECON_TEST = MIN(MIN_UP_TIME_INT +
     +                                 MAX (MIN_DOWN_TIME_INT,int(8,2)),
     +                                  MAX_HOURS_FOR_ECON_TEST)


                  ADVANCED_LOOK_AHEAD_LOGIC = .TRUE.
                  POSITIVE_HOURLY_MARGIN = .FALSE.
                  MAX_RECOVERY_OF_SU_COST = .FALSE.
                  POS_MARGIN_FOR_UP_TIME = .FALSE.
                  MIN_DOWN_TIME_SATISFIED = .FALSE.
                  MIN_RECOVERY_OF_SU_COST = .FALSE.
                  MIN_UP_TIME_SATISFIED = .FALSE.
                  ALWAYS_LOSE_MONEY_FOR_UP_TIME = .FALSE.
                  ALWAYS_LOSE_MONEY_FOR_MAX_TIME = .FALSE.
                  KEEP_OFF_FOR_MIN_DOWN_TIME = .FALSE.
                  UNIT_STARTS_THIS_HOUR = .FALSE.
!
! ASSUME THAT THE UNIT IS UP (OR CHECK UP YESTERDAY)
            if(hours_in_month>744) then
                call end_program ("tf_objt:0013 - hours_in_month " //
     + "value of " // trim(itos(int(hours_in_month))) // 
     + " is invalid.")
            end if
                  DO HOUR = 1, HOURS_IN_MONTH
                     IF(R_TIME_FRAME) THEN
                        HOUR_IN_TIMEFRAME = HOUR + CUMULATIVE_HOURS - 1
                     ELSE
                        HOUR_IN_TIMEFRAME = HOUR
                     ENDIF
!
!
! 121214. ADDED NGAS VOLATILITY DOWN TO THE HOUR. 12/29/14 REMOVED FOR 
! NOW.
!
                     IF(R_TIME_FRAME) THEN

                     ELSEIF(COMMIT_ON_TOTAL_COST) THEN
                        IF(FIAT_BLOCK_CAPACITY(2) >
     +                                      FIAT_BLOCK_CAPACITY(1)) THEN
! BLOCK 1 INC = TOTAL
                           BLOCK_PRICE(1) =

     +                           TOTAL_BTU_COST*TOTAL_FUEL_PARAM(1)+
     +                                           TOTAL_NON_FUEL_PARAM(1)
! BLOCK 2 INC
                           BLOCK_PRICE(2) =
!     +                        NGas_VolatilityByHour(HOUR)*
     +                           TOTAL_BTU_COST*TOTAL_FUEL_PARAM(2)+
     +                                           TOTAL_NON_FUEL_PARAM(2)
! BLOCK 2 TOTAL
                           BLOCK_PRICE(2) =
     +                         (BLOCK_PRICE(1)*FIAT_BLOCK_CAPACITY(1)+
     +                            BLOCK_PRICE(2)*
     +                                (FIAT_BLOCK_CAPACITY(2)-
     +                                      FIAT_BLOCK_CAPACITY(1)))/
     +                                            FIAT_BLOCK_CAPACITY(2)
                        ELSE
                           BLOCK_PRICE(1) = 999999.0
! BLOCK 2 TOTAL = BLOCK 1 TOTAL (AND INC)
                           BLOCK_PRICE(2) =
!     +                        NGas_VolatilityByHour(HOUR)*
     +                           TOTAL_BTU_COST*TOTAL_FUEL_PARAM(1)+
     +                                           TOTAL_NON_FUEL_PARAM(1)
!
                        ENDIF
                     ELSE
                        IF(FIAT_BLOCK_CAPACITY(2) >
     +                                      FIAT_BLOCK_CAPACITY(1)) THEN
                           BLOCK_PRICE(1) =
!     +                        NGas_VolatilityByHour(HOUR)*
     +                           UNIT_DISP_BTU_COST*UNIT_FUEL_PARAM(1)+
     +                                            UNIT_NON_FUEL_PARAM(1)
                           BLOCK_PRICE(2) =
!     +                        NGas_VolatilityByHour(HOUR)*
     +                           UNIT_DISP_BTU_COST*UNIT_FUEL_PARAM(2)+
     +                                            UNIT_NON_FUEL_PARAM(2)
                        ELSE
                           BLOCK_PRICE(1) = 999999.
                           BLOCK_PRICE(2) =
!     +                        NGas_VolatilityByHour(HOUR)*
     +                           UNIT_DISP_BTU_COST*UNIT_FUEL_PARAM(1)+
     +                                            UNIT_NON_FUEL_PARAM(1)
                        ENDIF
                     ENDIF
!
! 012015. HOUR_BLOCK_PRICE REPLACES DISPATCH_COST_FOR_BLOCK IN MULTIPLE 
! PLACES BELOW
!
                     CX_HOUR_BLOCK_PRICE(1,HOUR) = BLOCK_PRICE(1)
                     CX_HOUR_BLOCK_PRICE(2,HOUR) = BLOCK_PRICE(2)
!
                     BLOCK_CAPACITY(1) = FIRST_BLOCK_AVAIL(HOUR)
                     BLOCK_CAPACITY(2) = BLOCK_CAPACITY(1) +
     +                                          SECOND_BLOCK_AVAIL(HOUR)
                     IF(BLOCK_CAPACITY(2) < .001) THEN
                        UNIT_ON_OUTAGE(HOUR) = .TRUE.
                     ELSE
                        UNIT_ON_OUTAGE(HOUR) = .FALSE.
                     ENDIF

                        BLOCK_COST(HOUR,1) = BLOCK_PRICE(1) *
     +                                 BLOCK_CAPACITY(1)

                        BLOCK_FUEL_COST(HOUR,1) =
     +                           R_AVE_FUEL_COST(MONTH) *
     +                                 BLOCK_CAPACITY(1)

                        BLOCK_VOM_COST(HOUR,1) =
     +                           R_AVE_VOM_COST * BLOCK_CAPACITY(1)

                        BLOCK_EMIS_COST(HOUR,1) =
     +                           R_AVE_EMIS_COST *
     +                                 BLOCK_CAPACITY(1)

                        BLOCK_REV(HOUR,1) =
     +                        LOCAL_PRICE(HOUR_IN_TIMEFRAME) *
     +                                 BLOCK_CAPACITY(1)

                        BLOCK_MARGIN(HOUR,1) = BLOCK_REV(HOUR,1) -
     +                                                BLOCK_COST(HOUR,1)
                        BLOCK_COST(HOUR,2) = BLOCK_PRICE(2) *
     +                                 BLOCK_CAPACITY(2)

                        BLOCK_FUEL_COST(HOUR,2) =
     +                           R_AVE_FUEL_COST(MONTH) *
     +                                 BLOCK_CAPACITY(2)

                        BLOCK_VOM_COST(HOUR,2) =
     +                           R_AVE_VOM_COST * BLOCK_CAPACITY(2)
!     +                                 FIAT_BLOCK_CAPACITY(2)
                        BLOCK_EMIS_COST(HOUR,2) =
     +                           R_AVE_EMIS_COST *
     +                                 BLOCK_CAPACITY(2)
!     +                                 FIAT_BLOCK_CAPACITY(2)
                        BLOCK_REV(HOUR,2) =
     +                        LOCAL_PRICE(HOUR_IN_TIMEFRAME) *
     +                                 BLOCK_CAPACITY(2)
!     +                                 FIAT_BLOCK_CAPACITY(2)
                        BLOCK_MARGIN(HOUR,2) = BLOCK_REV(HOUR,2) -
     +                                                BLOCK_COST(HOUR,2)
!                     ENDIF
                     IF(BLOCK_MARGIN(HOUR,1) >
     +                           BLOCK_MARGIN(HOUR,2) .AND.
     +                                 BLOCK_CAPACITY(1) > 0.00001) THEN
                        BEST_BLOCK_BY_(HOUR) = 1
                        BEST_BLOCK_MARGIN(HOUR) = BLOCK_MARGIN(HOUR,1)
                        BEST_BLOCK_CAPACITY(HOUR) = BLOCK_CAPACITY(1)
                     ELSE
                        BEST_BLOCK_BY_(HOUR) = 2
                        BEST_BLOCK_MARGIN(HOUR) = BLOCK_MARGIN(HOUR,2)
                        BEST_BLOCK_CAPACITY(HOUR) = BLOCK_CAPACITY(2)
                     ENDIF
!
                  END DO
!
!                  CURRENT_STATE = .FALSE.
!                  HOURS_SINCE_START_UP(OPTION_INDEX) = 0
                  DAY = 0
!
!                  IF(FIRST_CALL_TO_ENDPOINT(OPTION_INDEX)) THEN
                  IF(MONTH == 1) THEN
                     PRESIM_HOURS = 168
!                     FIRST_CALL_TO_ENDPOINT(OPTION_INDEX) = .FALSE.
                  ELSE
                     PRESIM_HOURS = 0
                  ENDIF
                  POSTSIM_HOURS = 168
! 040115. IOTA LOGIC ADDED SO THAT IF A UNIT SETS MARGIN IN PRICE LOGIC
!         IT CAN SET MARGIN IN AI LOGIC.
            if(hours_in_month>744) then
                call end_program ("tf_objt:0014 - hours_in_month " //
     + "value of " // trim(itos(int(hours_in_month))) // 
     + " is invalid.")
            end if
                  DO HR = 1, HOURS_IN_MONTH + PRESIM_HOURS
                     IF(HR <= PRESIM_HOURS) THEN
                        HOUR = HR
                        IF(HR == PRESIM_HOURS) THEN
                           CUMULATIVE_MARGIN = 0.0
                        ENDIF
                     ELSE
                        HOUR = HR - PRESIM_HOURS
                     ENDIF
                     IF(R_TIME_FRAME) THEN
                        HOUR_IN_TIMEFRAME = HOUR + CUMULATIVE_HOURS - 1
                     ELSE
                        HOUR_IN_TIMEFRAME = HOUR
                     ENDIF
                     TEMP_R = ABS( INT((HOUR)/24,2) - (HOUR)/24.)
                     IF(TEMP_R < 0.0001) THEN
                        DAY = DAY + 1
                     ENDIF
                     LOOK_AHEAD_MARGIN = 0.
                     LOOK_AHEAD_MARGIN(-4) = -99999999. 
                     LOOK_AHEAD_MARGIN(-3) = -99999999. 
                     LOOK_AHEAD_MARGIN(-2) = 99999999.  
                     LOOK_AHEAD_MARGIN(-1) = -99999999. 
                     LOOK_AHEAD_MARGIN(0) = 99999999.   
                     LOOK_AHEAD_MARGIN(1) = BEST_BLOCK_MARGIN(HOUR)
            if(hours_in_month>744) then
                call end_program ("tf_objt:0015 - hours_in_month " //
     + "value of " // trim(itos(int(hours_in_month))) // 
     + " is invalid.")
            end if
                     HOURS_LEFT = HOURS_IN_MONTH + POSTSIM_HOURS - HOUR
                     MIN_UP_TIME_INT = MIN(HOURS_LEFT,
     +                                            MIN_UP_TIME_SAVE)
                     MIN_DOWN_TIME_INT = MIN(HOURS_LEFT,
     +                                          MIN_DOWN_TIME_SAVE)
                     HOURS_FOR_ECON_TEST = MIN(HOURS_LEFT,
     +                                              HOURS_FOR_ECON_TEST)
!
                     j = 1
                     LOOK_AHEAD_MARGIN(0) = MIN(LOOK_AHEAD_MARGIN(0),
     +                                          LOOK_AHEAD_MARGIN(J))
                     LOOK_AHEAD_MARGIN(-1) =
     +                                     MAX(LOOK_AHEAD_MARGIN(-1),
     +                                          LOOK_AHEAD_MARGIN(J))
                     LOOK_AHEAD_MARGIN(-2) =
     +                                     MIN(LOOK_AHEAD_MARGIN(-2),
     +                                     LOOK_AHEAD_MARGIN(J))
                     LOOK_AHEAD_MARGIN(-3) =
     +                                     MAX(LOOK_AHEAD_MARGIN(-3),
     +                                          LOOK_AHEAD_MARGIN(J))
            if(hours_in_month>744) then
                call end_program ("tf_objt:0016 - hours_in_month " //
     + "value of " // trim(itos(int(hours_in_month))) // 
     + " is invalid.")
            end if
                     DO  J = 2, MIN_UP_TIME_INT
                        K = HOUR + J - 1
                        IF(HOUR + J > HOURS_IN_MONTH) THEN
                           K = K - POSTSIM_HOURS
                        ENDIF
                        LOOK_AHEAD_MARGIN(J) = LOOK_AHEAD_MARGIN(J-1) +
     +                                       BEST_BLOCK_MARGIN(K)
                        LOOK_AHEAD_MARGIN(0) = MIN(LOOK_AHEAD_MARGIN(0),
     +                                             LOOK_AHEAD_MARGIN(J))
                        LOOK_AHEAD_MARGIN(-1) =
     +                                        MAX(LOOK_AHEAD_MARGIN(-1),
     +                                             LOOK_AHEAD_MARGIN(J))
                        LOOK_AHEAD_MARGIN(-2) =
     +                                        MIN(LOOK_AHEAD_MARGIN(-2),
     +                                        LOOK_AHEAD_MARGIN(J))
                        LOOK_AHEAD_MARGIN(-3) =
     +                                        MAX(LOOK_AHEAD_MARGIN(-3),
     +                                             LOOK_AHEAD_MARGIN(J))
                     ENDDO ! FOR MIN_UP_TIME_INT
            if(hours_in_month>744) then
                call end_program ("tf_objt:0017 - hours_in_month " //
     + "value of " // trim(itos(int(hours_in_month))) // 
     + " is invalid.")
            end if
                     IF(HOURS_FOR_ECON_TEST > MIN_UP_TIME_INT) THEN
                        DO J = MIN_UP_TIME_INT + 1, HOURS_FOR_ECON_TEST
                           K = HOUR + J - 1
                           IF(HOUR + J > HOURS_IN_MONTH) THEN
                              K = K - POSTSIM_HOURS
                           ENDIF
                           LOOK_AHEAD_MARGIN(J) =
     +                           LOOK_AHEAD_MARGIN(J-1) +
     +                                       BEST_BLOCK_MARGIN(K)
                           LOOK_AHEAD_MARGIN(-2) =
     +                              MIN(LOOK_AHEAD_MARGIN(-2),
     +                                        LOOK_AHEAD_MARGIN(J))
                           LOOK_AHEAD_MARGIN(-3) =
     +                              MAX(LOOK_AHEAD_MARGIN(-3),
     +                                             LOOK_AHEAD_MARGIN(J))
                           LOOK_AHEAD_MARGIN(-4) =
     +                        MAX(LOOK_AHEAD_MARGIN(-4),
     +                           LOOK_AHEAD_MARGIN(J) -
     +                             LOOK_AHEAD_MARGIN(MIN_UP_TIME_INT))
                        END DO ! FOR HOURS_FOR_ECON_TEST
                     ELSE
                           LOOK_AHEAD_MARGIN(-4) = 0.
                     ENDIF
!
!                     IF(HR == 1) THEN
!                        IF(R_UNIT_UP_YESTERDAY) THEN
!                           HOURS_SINCE_START_UP = 12
!                           HOURS_SINCE_SHUT_DOWN = 0
!                           UNIT_UP_LAST_HOUR = .TRUE.
!                        ELSE
!                           HOURS_SINCE_START_UP = 0
!                           HOURS_SINCE_SHUT_DOWN = 6
!                           UNIT_UP_LAST_HOUR = .FALSE.
!                        ENDIF
!                     ENDIF
! 040115.
                     IF(ABS(BEST_BLOCK_MARGIN(HOUR)) < 0.01) THEN
                        J = J
                     ENDIF
                     IF(BEST_BLOCK_MARGIN(HOUR) > -IOTA) THEN
                        POSITIVE_HOURLY_MARGIN = .TRUE.
                     ELSE
                        POSITIVE_HOURLY_MARGIN = .FALSE.
                     ENDIF
!
!032015. THIS IS FLAWED. THIS SEEMS TO ASSUME THAT THE START UPS
!        ARE MADE UP POSSIBLY AFTER THE MINIMUM UP TIME. HOWEVER,
!        IF THE MARGIN ONLY OCCURS WITHIN THE MIN UP TIME, IT CAN STILL
!        EASILY LOSE MONEY BEFORE THE END OF THE MIN UP TIME.
!
!        SHOULDN'T IT BE:
!
                     IF(LOOK_AHEAD_MARGIN(MIN_UP_TIME_INT) +
     +                            MAX(0.,LOOK_AHEAD_MARGIN(-4)) >
     +                                       START_UP_COSTS - IOTA) THEN
!
!                     IF(LOOK_AHEAD_MARGIN(-3) >
!     +                                         START_UP_COSTS) THEN
                        MAX_RECOVERY_OF_SU_COST = .TRUE.
                     ELSE
                        MAX_RECOVERY_OF_SU_COST = .FALSE.
                     ENDIF
!
! END THIS CONDITION.
!
! 032115. THIS SHOULD BE LOOK_AHEAD_MARGIN(MIN_UP_TIME_INT)
!
                     IF(LOOK_AHEAD_MARGIN(0) > 0. ) THEN

                        POS_MARGIN_FOR_UP_TIME = .TRUE.
                     ELSE
                        POS_MARGIN_FOR_UP_TIME = .FALSE.
                     ENDIF
!
                     IF(HOURS_SINCE_SHUT_DOWN >=
     +                                      MIN_DOWN_TIME_INT) THEN
                        MIN_DOWN_TIME_SATISFIED = .TRUE.
                     ELSE
                        MIN_DOWN_TIME_SATISFIED = .FALSE.
                     ENDIF
!
                     IF(LOOK_AHEAD_MARGIN(0) <
     +                                -1.0 * START_UP_COSTS) THEN
                        MIN_RECOVERY_OF_SU_COST = .TRUE.
                     ELSE
                        MIN_RECOVERY_OF_SU_COST = .FALSE.
                     ENDIF
!
                     IF(HOURS_SINCE_START_UP >=
     +                                           MIN_UP_TIME_INT ) THEN
                        MIN_UP_TIME_SATISFIED = .TRUE.
                     ELSE
                        MIN_UP_TIME_SATISFIED = .FALSE.
                     ENDIF
!
                     IF(LOOK_AHEAD_MARGIN(-1) < 0) THEN
                        ALWAYS_LOSE_MONEY_FOR_UP_TIME = .TRUE.
                     ELSE
                        ALWAYS_LOSE_MONEY_FOR_UP_TIME = .FALSE.
                     ENDIF
!
                     IF(LOOK_AHEAD_MARGIN(-3) < 0) THEN
                        ALWAYS_LOSE_MONEY_FOR_MAX_TIME = .TRUE.
                     ELSE
                        ALWAYS_LOSE_MONEY_FOR_MAX_TIME = .FALSE.
                     ENDIF
!
                     IF(HOURS_SINCE_START_UP >=
     +                                           MIN_UP_TIME_INT ) THEN
                        KEEP_OFF_FOR_MIN_DOWN_TIME = .TRUE.
                     ELSE
                        KEEP_OFF_FOR_MIN_DOWN_TIME = .FALSE.
                     ENDIF
! 032115. NEED TO MODIFIY THIS TO IMPROVE MAX_RECOVERY_OF_SU_COST
!         AND REMOVE POS_MARGIN_FOR_UP_TIME.
! 032315. UP CONSTRAINT .OR. ECONOMICS GIVEN MIN UP/DOWN.
                     IF(.NOT. UNIT_ON_OUTAGE(HOUR) .AND.
     +                          ( MUST_RUN_UNIT .OR.
     +                                 (POSITIVE_HOURLY_MARGIN  .AND.
     +                                  MAX_RECOVERY_OF_SU_COST .AND.
     +                                  POS_MARGIN_FOR_UP_TIME  .AND.
     +                                MIN_DOWN_TIME_SATISFIED) )  ) THEN
                        START_TEST = .TRUE.
                     ELSE
                        START_TEST = .FALSE.
                     ENDIF
! 031520. GAT. IT LOOKS LIKE MIN_RECOVERY_OF_SU_COST PLAYS NO ROLEBELOW.
                     IF((MIN_RECOVERY_OF_SU_COST .OR.
     +                            ALWAYS_LOSE_MONEY_FOR_MAX_TIME) .AND.
     +                            MIN_UP_TIME_SATISFIED .AND.
     +                               ALWAYS_LOSE_MONEY_FOR_UP_TIME) THEN
                        STOP_TEST = .TRUE.
                     ELSE
                        STOP_TEST = .FALSE.
                     ENDIF
! 040515. LOOKS LIKE UNIT_UP_LAST_HOUR IS REDUNDANT.
                     IF(.NOT. UNIT_ON_OUTAGE(HOUR) .AND.
     +                       (MUST_RUN_UNIT .OR.
     +                        POSITIVE_HOURLY_MARGIN .OR.
     +                                          .NOT. STOP_TEST ) ) THEN

                        RUN_TEST = .TRUE.
                     ELSE
                        RUN_TEST = .FALSE.
                     ENDIF

                     IF(UNIT_UP_LAST_HOUR) THEN
                        CURRENT_STATE = RUN_TEST
                     ELSE 
                        CURRENT_STATE = START_TEST
                        UNIT_STARTS_THIS_HOUR = START_TEST
                     ENDIF

                     IF(CURRENT_STATE) THEN
                        HOURS_SINCE_START_UP =
     +                            HOURS_SINCE_START_UP + 1
                        HOURS_SINCE_SHUT_DOWN = 0
                        LOCAL_USAGE(HOUR_IN_TIMEFRAME) =
     +                                         BEST_BLOCK_CAPACITY(HOUR)
                        IF(HR > PRESIM_HOURS) THEN
                           MONTH_HOURS(MONTH) = MONTH_HOURS(MONTH) + 1.0
                           MONTH_ENERGY(MONTH) = MONTH_ENERGY(MONTH) +
     +                                         BEST_BLOCK_CAPACITY(HOUR)
                           MONTH_ENERGY_REVENUE(MONTH) =
     +                           MONTH_ENERGY_REVENUE(MONTH) +
     +                              BLOCK_REV(HOUR,BEST_BLOCK_BY_(HOUR))
                           MONTH_ENERGY_COST(MONTH) =
     +                        MONTH_ENERGY_COST(MONTH) +
     +                             BLOCK_COST(HOUR,BEST_BLOCK_BY_(HOUR))
                           IF(BEST_BLOCK_CAPACITY(HOUR) > 0.0001) THEN
                              LOCAL_STRIKE_PRICE(HOUR_IN_TIMEFRAME) =
     +                            BLOCK_COST(HOUR,BEST_BLOCK_BY_(HOUR))/
     +                                         BEST_BLOCK_CAPACITY(HOUR)
                           ENDIF
                           IF(HOURS_SINCE_START_UP == 1) THEN
                              MONTH_STARTS(MONTH) =
     +                                         MONTH_STARTS(MONTH) + 1.0
                           ENDIF
                           MONTH_FUEL_COST(MONTH) =
     +                           MONTH_FUEL_COST(MONTH) +
     +                             BLOCK_FUEL_COST(
     +                                        HOUR,BEST_BLOCK_BY_(HOUR))
                           MONTH_VOM_COST(MONTH) =
     +                           MONTH_VOM_COST(MONTH) +
     +                             BLOCK_VOM_COST(
     +                                        HOUR,BEST_BLOCK_BY_(HOUR))
                           MONTH_EMIS_COST(MONTH) =
     +                           MONTH_EMIS_COST(MONTH) +
     +                             BLOCK_EMIS_COST(
     +                                        HOUR,BEST_BLOCK_BY_(HOUR))
                        ENDIF
                     ELSE
                        HOURS_SINCE_START_UP = 0
                        HOURS_SINCE_SHUT_DOWN =
     +                           HOURS_SINCE_SHUT_DOWN + 1
                        LOCAL_USAGE(HOUR_IN_TIMEFRAME) = 0.
                     ENDIF
!
                     IF(CURRENT_STATE) THEN
                         IF( UNIT_STARTS_THIS_HOUR ) THEN
                             MARGIN_SINCE_START_UP =
     +                                        MARGIN_SINCE_START_UP +
     +                                        BEST_BLOCK_MARGIN(HOUR) -
     +                                        START_UP_COSTS
                             CUMULATIVE_MARGIN = CUMULATIVE_MARGIN +
     +                                        BEST_BLOCK_MARGIN(HOUR) -
     +                                        START_UP_COSTS
                              MARGIN_SINCE_SHUT_DOWN = 0.0
                              IF(HOURS_SINCE_START_UP /= 1) THEN
                                 MONTH_STARTS(MONTH) =
     +                                               MONTH_STARTS(MONTH)
                              ENDIF
                         ELSE
                             MARGIN_SINCE_START_UP =
     +                                        BEST_BLOCK_MARGIN(HOUR)
                             CUMULATIVE_MARGIN = CUMULATIVE_MARGIN +
     +                                        BEST_BLOCK_MARGIN(HOUR)
                         ENDIF
! 032115. GAT. RESET TO NEGATIVE MARGIN.
                     ELSE
                         MARGIN_SINCE_SHUT_DOWN =
     +                                       MARGIN_SINCE_SHUT_DOWN +
     +                                       BEST_BLOCK_MARGIN(HOUR)
                         MARGIN_SINCE_START_UP = 0.0
                     ENDIF
!
                     UNIT_UP_LAST_HOUR = CURRENT_STATE
!                     UNIT_UP_LAST_HOUR(OPTION_INDEX) = CURRENT_STATE
! HOURLY REPORT FOR AI
                     TEMP_R = ABS( INT((HOUR)/24,2) - (HOUR)/24.)
                     IF(TEMP_R < 0.0001) THEN
                        END_OF_DAY = .TRUE.
                     ELSE
                        END_OF_DAY = .FALSE.
                     ENDIF
!
!                     IF(YES_CX_DIAGNOSTIC_REPORTS .AND.
!     +                     REPORT_THIS_CL_UNIT(OPTION_INDEX) .AND.
!     +                                        HR > PRESIM_HOURS) THEN
! 072815. TEMP.
                     YES_HOURLY_COMMITMENT = .FALSE.
                     IF(YES_HOURLY_COMMITMENT .AND.
     +                     R_CX_ITER == 0 .AND.
     +                        globecom_YEAR == 3 .AND.
     +                           REPORT_THIS_CL_UNIT(OPTION_INDEX) .AND.
     +                                        HR > PRESIM_HOURS) THEN
!                     IF(1 == 1) THEN
                        IF(AK_REPORT_NOT_OPEN) THEN
                              AK_REPORT_NOT_OPEN = .FALSE.
                              AK_REPORT_VARIABLES = 16 ! 14
                              AK_HOURLY_NO =
     +                          AK_HOURLY_HEADER(AK_REPORT_VARIABLES,
     +                                           AK_HOURLY_REC)
                        ENDIF
                        AK_HOURLY_REC = rptrec(AK_HOURLY_NO)
                        WRITE(AK_HOURLY_NO,REC=AK_HOURLY_REC)
     +                   PRT_ENDPOINT(),THIS_YEAR,
     +                   CL_MONTH_NAME(MONTH),R_UNIT_NAME,FLOAT(HOUR),
     +                   LOOK_AHEAD_MARGIN(-4),LOOK_AHEAD_MARGIN(-3),
     +                   LOOK_AHEAD_MARGIN(-2),LOOK_AHEAD_MARGIN(-1),
     +                   LOOK_AHEAD_MARGIN(-0),BEST_BLOCK_MARGIN(HOUR),
     +                   FLOAT(HOURS_SINCE_START_UP),
     +                   FLOAT(HOURS_SINCE_SHUT_DOWN),CUMULATIVE_MARGIN,
     +                   MARGIN_SINCE_START_UP,MARGIN_SINCE_SHUT_DOWN,
     +                   LOCAL_USAGE(HOUR_IN_TIMEFRAME),
     +                   LOOK_AHEAD_MARGIN(MIN_UP_TIME_INT),
     +                   FIRST_BLOCK_AVAIL(HOUR)+
     +                   SECOND_BLOCK_AVAIL(HOUR),
     +                           LOCAL_PRICE(HOUR_IN_TIMEFRAME),
     +                           CX_HOUR_BLOCK_PRICE(
     +                                        BEST_BLOCK_BY_(HOUR),HOUR)
                        AK_HOURLY_REC = AK_HOURLY_REC + 1
                     ENDIF
!
                     IF(R_CX_ITER == 1 .AND.
     +                  YES_HOURLY_COMMITMENT .AND. END_OF_DAY .AND.
     +                       REPORT_THIS_CL_UNIT(OPTION_INDEX) .AND.
     +                                        HR > PRESIM_HOURS) THEN
                        IF(AJ_REPORT_NOT_OPEN) THEN
                              AJ_REPORT_NOT_OPEN = .FALSE.
                              AJ_REPORT_VARIABLES = 24
                              AJ_HOURLY_NO =
     +                          AJ_HOURLY_HEADER(AJ_REPORT_VARIABLES,
     +                                           AJ_HOURLY_REC)
                        ENDIF
                        DY = DAY - PRESIM_HOURS/24
                        AJ_HOURLY_REC = rptrec(AJ_HOURLY_NO)
                        WRITE(AJ_HOURLY_NO,REC=AJ_HOURLY_REC)
     +                           PRT_ENDPOINT(),
     +                           THIS_YEAR,
     +                           CL_MONTH_NAME(MONTH),
     +                           FLOAT(DY),
     +                           R_UNIT_NAME,
     +                           (LOCAL_USAGE(J),J=HOUR_IN_TIMEFRAME-23,
     +                                                HOUR_IN_TIMEFRAME)
                        AJ_HOURLY_REC = AJ_HOURLY_REC + 1
                     ENDIF
!
                  ENDDO ! AI HOURLY COMMITMENT SCHEDULE FOR UNIT
                  IF(CUMULATIVE_MARGIN < -0.001 .AND.
     +                                         .NOT. MUST_RUN_UNIT) THEN
                     WRITE(4,*) "AI COMMITMENT AND DISPATCH ALGORITHM"
                     WRITE(4,*) "HAS GENERATED NEGATIVE MONTHLY"
                     WRITE(4,*) "MARGINS FOR ",R_UNIT_NAME
                     WRITE(4,*) "IN ENDPOINT ",PRT_ENDPOINT()," YEAR "
                     WRITE(4,*) THIS_YEAR," AND MONTH "
                     WRITE(4,*) CL_MONTH_NAME(MONTH)
                     WRITE(4,*) "AS A RESULT, THE"
                     WRITE(4,*) "SIMULATION WAS TERMINATED."
                     WRITE(4,*) "PLEASE REVIEW THE"
                     WRITE(4,*) "DIAGNOSTIC INFORMATION"

                  ENDIF
                  R_HOURS = R_HOURS + MONTH_HOURS(MONTH)
                  R_ENERGY = R_ENERGY + MONTH_ENERGY(MONTH)
                  R_ENERGY_REVENUE = R_ENERGY_REVENUE +
     +                                       MONTH_ENERGY_REVENUE(MONTH)
                  R_ENERGY_COST = R_ENERGY_COST +
     +                                          MONTH_ENERGY_COST(MONTH)
                  R_STARTS = R_STARTS + MONTH_STARTS(MONTH)
                  R_FUEL_COST = R_FUEL_COST + MONTH_FUEL_COST(MONTH)
                  R_VOM_COST = R_VOM_COST + MONTH_VOM_COST(MONTH)
                  R_EMIS_COST = R_EMIS_COST + MONTH_EMIS_COST(MONTH)
                  TOTAL_CUMULATIVE_MARGIN = TOTAL_CUMULATIVE_MARGIN +
     +                                                 CUMULATIVE_MARGIN
               ENDDO ! MONTHLY LOOP
               IF(R_TIME_FRAME) THEN
!                  R_PRICE(1:8784) = LOCAL_PRICE(1:8784)
                  R_USAGE(1:8784) = LOCAL_USAGE(1:8784)
                  R_STRIKE_PRICE(1:8784) = LOCAL_STRIKE_PRICE(1:8784)
               ELSE
!                  R_PRICE(1:800) = LOCAL_PRICE(1:800)
                  R_USAGE(1:800) = LOCAL_USAGE(1:800)
                  R_STRIKE_PRICE(1:800) = LOCAL_STRIKE_PRICE(1:800)
               ENDIF
      RETURN
      END
! **********************************************************************



