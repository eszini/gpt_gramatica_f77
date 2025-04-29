
!
!        PROGRAM TO READ MULTI-TAB INFORMATION ON WEEKLY HYDRO
!                 AND CONVERT TO BINARY FORMAT
!                       COPYRIGHT (C) 2001
!      ALL RIGHTS RESERVED M.S. GERBER & ASSOCIATES, INC.
!

!
      SUBROUTINE WEEKLY_HYDRO_OBJECT
      use end_routine, only: end_program, er_message
      use mod_base_year
      use SpinDriftLib
      USE PROD_ARRAYS_DIMENSIONS
      USE SIZECOM
      USE iostatmsg
      
      implicit none

      INTEGER (KIND=1) :: FORECAST_GROWTH_YEARS=AVAIL_DATA_YEARS
      LOGICAL (KIND=1) :: SAVE_WH_FILE_EXISTS=.FALSE.,R_WH_FILE_EXISTS
      LOGICAL (KIND=1) :: R_WH_FILE_USED,SAVE_WH_FILE_USED=.FALSE.
      CHARACTER (LEN=6) :: BASECASE_MARKET_AREA_ID
      CHARACTER (LEN=6) :: BASECASE_TRANS_AREA_ID
      CHARACTER (LEN=6) :: BASECASE_NERC_SUB_ID
      CHARACTER (LEN=20) :: HYDRO_VARIABLES,HYDRO_TYPE
      CHARACTER (LEN=1) :: EXTENSION_PERIOD_GROWTH_SWITCH='X'
      CHARACTER (LEN=1) :: TABLE_ACTIVE
      INTEGER (KIND=2) :: INUNIT,IREC,DELETE,LRECL=350,base_year_loc
      INTEGER (KIND=2) :: R_LRECL,SAVE_TRANS_LOAD_TABLES=0
      INTEGER (KIND=2) :: R_TRANS_LOAD_TABLES,TEMP_YEAR
      INTEGER (KIND=2) :: R_NUM_OF_TRANS_CLASSES
      INTEGER (KIND=2) :: NUM_OF_OL_TRANS_CLASSES=0
      INTEGER (KIND=2) :: R_MAX_TRANS_CLASS_NUM
      INTEGER (KIND=2) :: MAX_OL_TRANS_CLASS_ID_NUM=0
      INTEGER (KIND=2) :: R_NUM_OF_ASSET_CLASSES
      INTEGER (KIND=2) :: NUM_OF_OL_ASSET_CLASSES=0
      INTEGER (KIND=2) :: R_MAX_ASSET_CLASS_NUM
      INTEGER (KIND=2) :: MAX_OL_ASSET_CLASS_ID_NUM=0
      INTEGER (KIND=2) :: R_NUM_OF_CUST_CLASSES,NUM_OF_OL_CUST_CLASSES=0
      INTEGER (KIND=2) :: R_MAX_CUST_CLASS_NUM
      INTEGER (KIND=2) :: MAX_OL_CUST_CLASS_ID_NUM=0
      INTEGER (KIND=2) :: NUM_OF_BC_TRANS_CLASSES=0
      INTEGER (KIND=2) :: MAX_BC_TRANS_CLASS_ID_NUM=0
      INTEGER (KIND=2) :: NUM_OF_BC_ASSET_CLASSES=0
      INTEGER (KIND=2) :: MAX_BC_ASSET_CLASS_ID_NUM=0
      INTEGER (KIND=2) :: NUM_OF_BC_CUST_CLASSES=0
      INTEGER (KIND=2) :: MAX_BC_CUST_CLASS_ID_NUM=0
      INTEGER (KIND=2) :: TEMP_TRANS_CLASS_POINTER(:)
      INTEGER (KIND=2) :: TEMP_ASSET_CLASS_POINTER(:)
      INTEGER (KIND=2) :: TEMP_CUST_CLASS_POINTER(:)
      INTEGER (KIND=4) :: IOS
      ALLOCATABLE :: TEMP_TRANS_CLASS_POINTER,TEMP_ASSET_CLASS_POINTER
      ALLOCATABLE :: TEMP_CUST_CLASS_POINTER
      CHARACTER (LEN=5) :: WEEKLY_HYDRO_FILE,OVERLAY_FAMILY_NAME
      CHARACTER (LEN=5) :: BSYRLOAD,REFERENCE_LOAD_NAME
      CHARACTER (LEN=256) :: FILE_NAME
      CHARACTER (LEN=256) :: OUTPUT_DIRECTORY
      CHARACTER (LEN=256) :: BASE_FILE_DIRECTORY
      CHARACTER (LEN=152) :: MESSAGE
      LOGICAL (KIND=4) :: FILE_EXISTS
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER (LEN=1024) :: RECLN
! DECLARATION FOR SYSTEM FORECAST VARIABLES
      INTEGER (KIND=2) :: YEAR_loc,UNIT_NUM=10,REFERENCE_LOAD_NUMBER

! SIMULATION VARIABLES
!
      INTEGER (KIND=2) :: CUSTOMER_GROUP,TRANSACTION_GROUP ! INT2

      REAL (KIND=4) :: MARKET_ENERGY_PRICE,MONTHLY_ENERGY_PRICE_PATTERN
      REAL (KIND=4) :: MARKET_DEMAND_PRICE,MONTHLY_DEMAND_PRICE_PATTERN
      REAL (KIND=4) :: MARKET_CUSTOMER_PRICE,ASSET_CLASS_ID  ! REAL4
      REAL (KIND=4) :: ASSET_CLASS_REV_ALLOC_VECTOR,ANNUAL_ENERGY ! REAL4
      REAL (KIND=4) :: ANNUAL_PEAK,ANNUAL_CUSTOMERS,ANNUAL_MULTIPLIER
      REAL (KIND=4) :: WEEKLY_VALUES(52),DIST_ENERGY_LOSS_FACTOR ! REAL4
      REAL (KIND=4) :: TRANS_ENERGY_LOSS_FACTOR,PEAK_LOSS_FACTOR ! REAL4
      REAL (KIND=4) :: PEAK_COIN_FACTOR,DISTRIBUTION_PRICE ! REAL4
      REAL (KIND=4) :: TRANSMISSION_PRICE,MINIMUM_MARKET_PRICE
      REAL (KIND=4) :: MAXIMUM_MARKET_PRICE,INDEXED_ENERGY_PRICE
      REAL (KIND=4) :: PUMPING_CAP_MW,PUMPING_STORAGE_EFFICIENCY
      CHARACTER (LEN=17) :: FILE_TYPE='Weekly Hydro     '
      CHARACTER (LEN=2) :: WEEKLY_HYDRO_OL='BC',R_WEEKLY_HYDRO_OL
      LOGICAL (KIND=1) :: LAHEY_LF95
      CHARACTER (LEN=30) :: SCREEN_OUTPUT
!
      INTEGER (KIND=2) :: WH_YEAR,STUDY_BASE_YEAR
!
      SAVE STUDY_BASE_YEAR
!

!
!          ROUTINE TO CONVERT METAFILE FILES TO DIRECT-ACESS BINARY
!          COPYRIGHT (C) 1983, 84, 85  M.S. GERBER & ASSOCIATES, INC.
!

!
! CONVERT THE SYSTEM-FORECAST FILE
!
!

      ENTRY WEEKLY_HYDRO_MAKEBIN

      STUDY_BASE_YEAR = get_base_year()
      WH_YEAR = 0
      FILE_NAME = trim(BASE_FILE_DIRECTORY())// &
                            "WHB"//trim(WEEKLY_HYDRO_FILE())//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      IF(EXTENSION_PERIOD_GROWTH_SWITCH == 'F') &
                                FORECAST_GROWTH_YEARS = AVAIL_DATA_YEARS
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//WEEKLY_HYDRO_FILE()
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_LOCATE_WRITE(16,30,WEEKLY_HYDRO_FILE(), &
                                                         ALL_VERSIONS,0)
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         ENDIF
!
         CUSTOMER_GROUP = 0
!
         NUM_OF_BC_TRANS_CLASSES = 0
         MAX_BC_TRANS_CLASS_ID_NUM = 0
         NUM_OF_BC_ASSET_CLASSES = 0
         MAX_BC_ASSET_CLASS_ID_NUM = 0
         NUM_OF_BC_CUST_CLASSES = 0
         MAX_BC_CUST_CLASS_ID_NUM = 0
!
         ALLOCATE(   TEMP_TRANS_CLASS_POINTER(0:1023), &
                     TEMP_ASSET_CLASS_POINTER(0:1023), &
                     TEMP_CUST_CLASS_POINTER(0:1023))
         TEMP_TRANS_CLASS_POINTER = 0
         TEMP_ASSET_CLASS_POINTER = 0
         TEMP_CUST_CLASS_POINTER = 0
!
         SAVE_WH_FILE_EXISTS = .TRUE.
!
         WEEKLY_VALUES = 0.

         OPEN(10,FILE=FILE_NAME) !whb file
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCWSYFC.BIN", &
                            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
!
         TRANSACTION_GROUP = 1
         MARKET_ENERGY_PRICE = 0
         MONTHLY_ENERGY_PRICE_PATTERN = 0.
         MARKET_DEMAND_PRICE = 0
         MONTHLY_DEMAND_PRICE_PATTERN = 0.
         MARKET_CUSTOMER_PRICE = 0.
         ASSET_CLASS_ID = 0.
         ASSET_CLASS_REV_ALLOC_VECTOR = 0.

         SAVE_TRANS_LOAD_TABLES = 0

         TABLE_ACTIVE = 'T'
         BASECASE_MARKET_AREA_ID = 'BLANK ' ! CHAR*6
         BASECASE_TRANS_AREA_ID = 'BLANK ' ! CHAR*6
         BASECASE_NERC_SUB_ID = 'BLANK ' ! CHAR*6

         HYDRO_TYPE = "Pondage"
         PUMPING_CAP_MW = 0.
         PUMPING_STORAGE_EFFICIENCY = 0.

         REFERENCE_LOAD_NAME = BSYRLOAD()
         REFERENCE_LOAD_NUMBER = 0

         IREC = 0
         READ(10,*) DELETE ! Whb
         DO ! TABLES
            WH_YEAR = 1
            DO ! YEAR-BASED RECORDS
               READ(10,1000,IOSTAT=IOS) RECLN

               IF(RECLN(1:1) == '7' .OR. IOS /= 0) THEN

                  IF(WH_YEAR <= AVAIL_DATA_YEARS) THEN
                     DO WH_YEAR = WH_YEAR, AVAIL_DATA_YEARS
                        IREC = IREC + 1
                        WRITE(11,REC=IREC) DELETE, &
                           WH_YEAR+STUDY_BASE_YEAR, &
                           TABLE_ACTIVE, & !  C 1
                           HYDRO_VARIABLES, &
                           TRANSACTION_GROUP, & !  I 2
                           BASECASE_MARKET_AREA_ID, & !  C 6
                           BASECASE_TRANS_AREA_ID, &
                           BASECASE_NERC_SUB_ID, & !  C 6
                           MARKET_ENERGY_PRICE, & !  R 4
                           MONTHLY_ENERGY_PRICE_PATTERN, & !  R 4
                           MARKET_DEMAND_PRICE, & !  R 4
                           MONTHLY_DEMAND_PRICE_PATTERN, & !  R 4
                           MARKET_CUSTOMER_PRICE, &
                           DISTRIBUTION_PRICE, & !  R 4
                           TRANSMISSION_PRICE, & !  R 4
                           ASSET_CLASS_ID, & !  R 4
                           ASSET_CLASS_REV_ALLOC_VECTOR, & !  R 4
                           WEEKLY_VALUES, & ! 52 X R 4
                           HYDRO_TYPE, &
                           PUMPING_CAP_MW, &
                           PUMPING_STORAGE_EFFICIENCY, &
                           REFERENCE_LOAD_NAME, &
                           REFERENCE_LOAD_NUMBER
                     ENDDO
                  ENDIF ! DETECTED NEW TABLE OR END OF FILE
!
                  EXIT ! LEAVE LOOP (GO TO NEXT TABLE OR EXIT ROUTINE)
!
               ENDIF
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,ERR=200) DELETE, &
                YEAR_loc, &
                TABLE_ACTIVE, & !  C 1
                           HYDRO_VARIABLES, &
                           TRANSACTION_GROUP, & !  I 2
                           BASECASE_MARKET_AREA_ID, & !  C 6
                           BASECASE_TRANS_AREA_ID, &
                           BASECASE_NERC_SUB_ID, & !  C 6
                           MARKET_ENERGY_PRICE, & !  R 4
                           MONTHLY_ENERGY_PRICE_PATTERN, & !  R 4
                           MARKET_DEMAND_PRICE, & !  R 4
                           MONTHLY_DEMAND_PRICE_PATTERN, & !  R 4
                           MARKET_CUSTOMER_PRICE, &
                           DISTRIBUTION_PRICE, & !  R 4
                           TRANSMISSION_PRICE, & !  R 4
                           ASSET_CLASS_ID, & !  R 4
                           ASSET_CLASS_REV_ALLOC_VECTOR, & !  R 4
                           WEEKLY_VALUES, & ! 52 X R 4
                           HYDRO_TYPE, &
                           PUMPING_CAP_MW, &
                           PUMPING_STORAGE_EFFICIENCY, &
                           REFERENCE_LOAD_NAME, &
                           REFERENCE_LOAD_NUMBER
!
               IREC = IREC + 1
!
               READ(RECLN,*,ERR=200) DELETE,YEAR_loc
            TEMP_YEAR = MIN(AVAIL_DATA_YEARS,YEAR_loc - STUDY_BASE_YEAR)
!
               IF(TEMP_TRANS_CLASS_POINTER(TRANSACTION_GROUP) == 0) THEN
                  NUM_OF_BC_TRANS_CLASSES = NUM_OF_BC_TRANS_CLASSES + 1
                  MAX_BC_TRANS_CLASS_ID_NUM = MAX( &
                            MAX_BC_TRANS_CLASS_ID_NUM,TRANSACTION_GROUP)
                  TEMP_TRANS_CLASS_POINTER(TRANSACTION_GROUP) = 1
               ENDIF
           IF(TEMP_ASSET_CLASS_POINTER(INT(ASSET_CLASS_ID,2)) == 0 )THEN
                  NUM_OF_BC_ASSET_CLASSES = NUM_OF_BC_ASSET_CLASSES + 1
                  MAX_BC_ASSET_CLASS_ID_NUM = MAX( &
                  MAX_BC_ASSET_CLASS_ID_NUM,INT(ASSET_CLASS_ID,2))
                  TEMP_ASSET_CLASS_POINTER(int(ASSET_CLASS_ID,2)) = 1
           ENDIF
           IF(TEMP_CUST_CLASS_POINTER(INT(CUSTOMER_GROUP,2)) == 0) THEN
                  NUM_OF_BC_CUST_CLASSES = NUM_OF_BC_CUST_CLASSES + 1
                  MAX_BC_CUST_CLASS_ID_NUM = MAX( &
                            MAX_BC_CUST_CLASS_ID_NUM,CUSTOMER_GROUP)
                  TEMP_CUST_CLASS_POINTER(CUSTOMER_GROUP) = 1
           ENDIF
!
               IF(TEMP_YEAR > WH_YEAR) THEN ! FILL IN MISSING YEARS
                  IF(IREC == 1) THEN
                     WRITE(4,*) "The first year of the first table in"
                     WRITE(4,*) "the Transact Forecast file is ",year_loc
                     WRITE(4,*) "while the base year set in project"
                     WRITE(4,*) "information is ",STUDY_BASE_YEAR
                     WRITE(4,*) "First forecast year in the Transact"
                     WRITE(4,*) "Forecast file first year must be one"
                     WRITE(4,*) "year greater than the base year."
                     WRITE(4,*) " "
                     er_message='See WARNINGS MESSAGES file.'
                     call end_program(er_message)
                  ENDIF
                  DO WH_YEAR = WH_YEAR, TEMP_YEAR - 1
                     WRITE(11,REC=IREC) DELETE, &
                        WH_YEAR+STUDY_BASE_YEAR, & !  I 2
                           TABLE_ACTIVE, &
                           HYDRO_VARIABLES, & !  S 20
                           TRANSACTION_GROUP, &
                           BASECASE_MARKET_AREA_ID, & !  C 6
                           BASECASE_TRANS_AREA_ID, & !  C 6
                           BASECASE_NERC_SUB_ID, & !  C 6
                           MARKET_ENERGY_PRICE, & !  R 4
                           MONTHLY_ENERGY_PRICE_PATTERN, & !  R 4
                           MARKET_DEMAND_PRICE, & !  R 4
                           MONTHLY_DEMAND_PRICE_PATTERN, & !  R 4
                           MARKET_CUSTOMER_PRICE, & !  R 4
                           DISTRIBUTION_PRICE, & !  R 4
                           TRANSMISSION_PRICE, & !  R 4
                           ASSET_CLASS_ID, & !  R 4
                           ASSET_CLASS_REV_ALLOC_VECTOR, & !  R 4
                           WEEKLY_VALUES, & ! 52 X R 4
                           HYDRO_TYPE, &
                           PUMPING_CAP_MW, &
                           PUMPING_STORAGE_EFFICIENCY, &
                           REFERENCE_LOAD_NAME, &
                           REFERENCE_LOAD_NUMBER
                     IREC = IREC + 1
                  ENDDO

               ENDIF

               WRITE(11,REC=IREC) DELETE, &
                WH_YEAR+STUDY_BASE_YEAR, & !  I 2
                           TABLE_ACTIVE, & !  C 1
                           HYDRO_VARIABLES, & !  S 20
                           TRANSACTION_GROUP, & !  I 2
                           BASECASE_MARKET_AREA_ID, & !  C 6
                           BASECASE_TRANS_AREA_ID, & !  C 6
                           BASECASE_NERC_SUB_ID, & !  C 6
                           MARKET_ENERGY_PRICE, & !  R 4
                           MONTHLY_ENERGY_PRICE_PATTERN, & !  R 4
                           MARKET_DEMAND_PRICE, & !  R 4
                           MONTHLY_DEMAND_PRICE_PATTERN, & !  R 4
                           MARKET_CUSTOMER_PRICE, & !  R 4
                           DISTRIBUTION_PRICE, & !  R 4
                           TRANSMISSION_PRICE, & !  R 4
                           ASSET_CLASS_ID, & !  R 4
                           ASSET_CLASS_REV_ALLOC_VECTOR, & !  R 4
                           WEEKLY_VALUES, & ! 52 X R 4
                           HYDRO_TYPE, &
                           PUMPING_CAP_MW, &
                           PUMPING_STORAGE_EFFICIENCY, &
                           REFERENCE_LOAD_NAME, &
                           REFERENCE_LOAD_NUMBER
               WH_YEAR = WH_YEAR + 1

            ENDDO ! LOAD GROUP
            SAVE_TRANS_LOAD_TABLES = SAVE_TRANS_LOAD_TABLES + 1
            IF(IOS /= 0) EXIT
         ENDDO ! READ TABLES
         CLOSE(10)
      ELSE IF(INDEX(WEEKLY_HYDRO_FILE(),'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "wh_objt:11110")
      ELSE
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCWSYFC.BIN", &
                            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
!
         SAVE_WH_FILE_EXISTS = .FALSE.
!
      ENDIF

      CLOSE(11)
      IF(ALLOCATED(TEMP_TRANS_CLASS_POINTER)) &
            DEALLOCATE(    TEMP_TRANS_CLASS_POINTER, &
                           TEMP_ASSET_CLASS_POINTER, &
                           TEMP_CUST_CLASS_POINTER)
      RETURN

  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from wh_objt SIID346'
      call end_program(er_message)

!
!          ROUTINE TO CREATE OVERLAY FILES
!          COPYRIGHT (C) 1984-88  M.S. GERBER & ASSOCIATES, INC.
!          COPYRIGHT (C) 1991-92  M.S. GERBER & ASSOCIATES, INC.
!

!

      ENTRY WEEKLY_HYDRO_MAKEOVL(OVERLAY_FAMILY_NAME)

      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      FILE_NAME=trim(OUTPUT_DIRECTORY())//"WHO"// &
                                     trim(OVERLAY_FAMILY_NAME)//".DAT"

      
      OPEN(10,FILE=FILE_NAME) ! Whofile
      READ(10,*) DELETE
      WH_YEAR = 0
      INUNIT = 12
      IF(WEEKLY_HYDRO_OL == 'BC') THEN
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCWSYFC.BIN", &
                                             ACCESS="DIRECT",RECL=LRECL)
         INUNIT = 11
      ENDIF

      FILE_NAME = trim(OUTPUT_DIRECTORY())//"OLWSYFC.BIN"
      OPEN(12,FILE=FILE_NAME,ACCESS="DIRECT", &
                                 STATUS="UNKNOWN",RECL=LRECL,IOSTAT=IOS)
      IF(IOS /= 0) THEN
         CALL iostatmsg_unit(IOS,MESSAGE)
         WRITE(4,*) trim(MESSAGE)
         er_message='in WEEKLY_HYDRO_MAKEOVL see WARNINGS messages'
         call end_program(er_message)
      ENDIF
      IREC = 0
      DELETE = 1
      READ(10,1000,IOSTAT=IOS) RECLN
!
      DO WHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE
!
         READ(10,1000,IOSTAT=IOS) RECLN
!
      ENDDO
      READ(RECLN,*,ERR=200) DELETE,WH_YEAR
!
!
!
      NUM_OF_OL_TRANS_CLASSES = 0
      MAX_OL_TRANS_CLASS_ID_NUM = 0
      NUM_OF_OL_ASSET_CLASSES = 0
      MAX_OL_ASSET_CLASS_ID_NUM = 0
      NUM_OF_OL_CUST_CLASSES = 0
      MAX_OL_CUST_CLASS_ID_NUM = 0
!
      ALLOCATE(TEMP_TRANS_CLASS_POINTER(0:1023), &
               TEMP_ASSET_CLASS_POINTER(0:1023), &
               TEMP_CUST_CLASS_POINTER(0:1023))
      TEMP_TRANS_CLASS_POINTER = 0
      TEMP_ASSET_CLASS_POINTER = 0
      TEMP_CUST_CLASS_POINTER = 0


!
      DO ! TABLES AND YEARS, COUNTING IS DONE INSIDE
         IREC = IREC + 1
         READ(INUNIT,REC=IREC,IOSTAT=IOS) &
                           DELETE, &
                           YEAR_loc, &
                           TABLE_ACTIVE, & !  C 1
                           HYDRO_VARIABLES, &
                           TRANSACTION_GROUP, & !  I 2
                           BASECASE_MARKET_AREA_ID, & !  C 6
                           BASECASE_TRANS_AREA_ID, & !  C 6
                           BASECASE_NERC_SUB_ID, & !  C 6
                           MARKET_ENERGY_PRICE, & !  R 4
                           MONTHLY_ENERGY_PRICE_PATTERN, & !  R 4
                           MARKET_DEMAND_PRICE, & !  R 4
                           MONTHLY_DEMAND_PRICE_PATTERN, & !  R 4
                           MARKET_CUSTOMER_PRICE, & !  R 4
                           DISTRIBUTION_PRICE, & !  R 4
                           TRANSMISSION_PRICE, & !  R 4
                           ASSET_CLASS_ID, & !  R 4
                           ASSET_CLASS_REV_ALLOC_VECTOR, & !  R 4
                           WEEKLY_VALUES, & ! 52 X R 4
                           HYDRO_TYPE, &
                           PUMPING_CAP_MW, &
                           PUMPING_STORAGE_EFFICIENCY, &
                           REFERENCE_LOAD_NAME, &
                           REFERENCE_LOAD_NUMBER
         IF(IOS /= 0) EXIT ! END OF BINARY FILE
!
         IF(year_loc == WH_YEAR) THEN
            RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
            READ(RECLN,*,ERR=200) &
                           DELETE, &
                           WH_YEAR, &
                           TABLE_ACTIVE, & !  C 1
                           HYDRO_VARIABLES, &
                           TRANSACTION_GROUP, & !  I 2
                           BASECASE_MARKET_AREA_ID, & !  C 6
                           BASECASE_TRANS_AREA_ID, & !  C 6
                           BASECASE_NERC_SUB_ID, & !  C 6
                           MARKET_ENERGY_PRICE, & !  R 4
                           MONTHLY_ENERGY_PRICE_PATTERN, & !  R 4
                           MARKET_DEMAND_PRICE, & !  R 4
                           MONTHLY_DEMAND_PRICE_PATTERN, & !  R 4
                           MARKET_CUSTOMER_PRICE, & !  R 4
                           DISTRIBUTION_PRICE, & !  R 4
                           TRANSMISSION_PRICE, & !  R 4
                           ASSET_CLASS_ID, & !  R 4
                           ASSET_CLASS_REV_ALLOC_VECTOR, & !  R 4
                           WEEKLY_VALUES, & ! 52 X R 4
                           HYDRO_TYPE, &
                           PUMPING_CAP_MW, &
                           PUMPING_STORAGE_EFFICIENCY, &
                           REFERENCE_LOAD_NAME, &
                           REFERENCE_LOAD_NUMBER
            READ(10,1000,IOSTAT=IOS) RECLN

            DO WHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE
!
               READ(10,1000,IOSTAT=IOS) RECLN
!
            ENDDO
            READ(RECLN,*,ERR=200) DELETE,WH_YEAR
         ENDIF

         IF(TEMP_TRANS_CLASS_POINTER(TRANSACTION_GROUP) == 0) THEN
            NUM_OF_OL_TRANS_CLASSES = NUM_OF_OL_TRANS_CLASSES + 1
            MAX_OL_TRANS_CLASS_ID_NUM = MAX( &
                            MAX_OL_TRANS_CLASS_ID_NUM,TRANSACTION_GROUP)
            TEMP_TRANS_CLASS_POINTER(TRANSACTION_GROUP) = 1
         ENDIF
         IF(TEMP_ASSET_CLASS_POINTER(int(ASSET_CLASS_ID,2)) == 0) THEN
            NUM_OF_OL_ASSET_CLASSES = NUM_OF_OL_ASSET_CLASSES + 1
            MAX_OL_ASSET_CLASS_ID_NUM = MAX( &
                   MAX_OL_ASSET_CLASS_ID_NUM,int(ASSET_CLASS_ID,2))
            TEMP_ASSET_CLASS_POINTER(int(ASSET_CLASS_ID,2)) = 1
         ENDIF
         IF(TEMP_CUST_CLASS_POINTER(CUSTOMER_GROUP) == 0) THEN
            NUM_OF_OL_CUST_CLASSES = NUM_OF_OL_CUST_CLASSES + 1
            MAX_OL_CUST_CLASS_ID_NUM = MAX( &
                            MAX_OL_CUST_CLASS_ID_NUM,CUSTOMER_GROUP)
            TEMP_CUST_CLASS_POINTER(CUSTOMER_GROUP) = 1
         ENDIF
!
         WRITE(12,REC=IREC)  DELETE,YEAR_loc,TABLE_ACTIVE, & !  C 1
                           HYDRO_VARIABLES, & !  S 20
                           TRANSACTION_GROUP, & !  I 2
                           BASECASE_MARKET_AREA_ID, & !  C 6
                           BASECASE_TRANS_AREA_ID, & !  C 6
                           BASECASE_NERC_SUB_ID, & !  C 6
                           MARKET_ENERGY_PRICE, & !  R 4
                           MONTHLY_ENERGY_PRICE_PATTERN, & !  R 4
                           MARKET_DEMAND_PRICE, & !  R 4
                           MONTHLY_DEMAND_PRICE_PATTERN, & !  R 4
                           MARKET_CUSTOMER_PRICE, & !  R 4
                           DISTRIBUTION_PRICE, & !  R 4
                           TRANSMISSION_PRICE, & !  R 4
                           ASSET_CLASS_ID, & !  R 4
                           ASSET_CLASS_REV_ALLOC_VECTOR, & !  R 4
                           WEEKLY_VALUES, & ! 52 X R 4
                           HYDRO_TYPE, &
                           PUMPING_CAP_MW, &
                           PUMPING_STORAGE_EFFICIENCY, &
                           REFERENCE_LOAD_NAME, &
                           REFERENCE_LOAD_NUMBER

      ENDDO
      CLOSE(10)
      CLOSE(12)
      IF(WEEKLY_HYDRO_OL == 'BC') then
        CLOSE(11)
      end if
      WEEKLY_HYDRO_OL = 'OL'
      IF(ALLOCATED(TEMP_TRANS_CLASS_POINTER)) &
            DEALLOCATE(    TEMP_TRANS_CLASS_POINTER, &
                           TEMP_ASSET_CLASS_POINTER, &
                           TEMP_CUST_CLASS_POINTER)
      RETURN
!
!

      ENTRY RESET_WEEKLY_HYDRO_OL

         WEEKLY_HYDRO_OL = 'BC'
         SAVE_WH_FILE_USED = .FALSE.
      RETURN
!

      ENTRY RETURN_WEEKLY_HYDRO_OL(R_WEEKLY_HYDRO_OL,R_LRECL)

         R_WEEKLY_HYDRO_OL = WEEKLY_HYDRO_OL
         R_LRECL = LRECL
      RETURN

      ENTRY DOES_WEEKLY_HYDRO_FILE_EXIST(R_WH_FILE_EXISTS)

         R_WH_FILE_EXISTS = SAVE_WH_FILE_EXISTS
      RETURN

      ENTRY GET_WEEKLY_HYDRO_TABLES(R_TRANS_LOAD_TABLES, &
                                    R_NUM_OF_TRANS_CLASSES)

         R_TRANS_LOAD_TABLES = SAVE_TRANS_LOAD_TABLES
         IF(WEEKLY_HYDRO_OL == 'OL') THEN
            R_NUM_OF_TRANS_CLASSES = NUM_OF_OL_TRANS_CLASSES
         ELSE
            R_NUM_OF_TRANS_CLASSES = NUM_OF_BC_TRANS_CLASSES
         ENDIF
      RETURN

      ENTRY WH_FILE_USED_THIS_ENDPOINT(R_WH_FILE_USED)

         R_WH_FILE_USED = SAVE_WH_FILE_USED
         SAVE_WH_FILE_USED = .TRUE.
      RETURN

      ENTRY OPEN_WEEKLY_HYDRO_FILE

         OPEN(UNIT_NUM, &
              FILE=trim(OUTPUT_DIRECTORY())//WEEKLY_HYDRO_OL// &
              "WSYFC.BIN",ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      RETURN
!

      ENTRY CLOSE_WEEKLY_HYDRO_FILE

         CLOSE(UNIT_NUM)
      RETURN

      ENTRY RETURN_WH_GROUP_INFO(R_NUM_OF_TRANS_CLASSES, &
                                 R_MAX_TRANS_CLASS_NUM, &
                                 R_NUM_OF_ASSET_CLASSES, &
                                 R_MAX_ASSET_CLASS_NUM, &
                                 R_NUM_OF_CUST_CLASSES, &
                                 R_MAX_CUST_CLASS_NUM)

         IF(WEEKLY_HYDRO_OL == 'OL') THEN
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
!        PROGRAM TO MANAGE WEEKLY HYDRO FORECASTS, REVENUES AND COSTS
!           BY TRANSACTION GROUP ASSET CLASS AND CUSTOMER GROUP
!
!                       COPYRIGHT (C) 2001
!          ALL RIGHTS RESERVED M.S. GERBER & ASSOCIATES, INC.
!

!
      FUNCTION MANAGE_WEEKLY_HYDRO_FORECASTS()
      USE SIZECOM
      use SpinDriftLib
      USE PROD_ARRAYS_DIMENSIONS
      use globecom
      use end_routine
      SAVE

      LOGICAL (KIND=1) :: READ_WEEKLY_HYDRO_DATA
      LOGICAL (KIND=1) :: SAVE_WEEKLY_HYDRO_STATUS=.FALSE.
      LOGICAL (KIND=1) :: MANAGE_WEEKLY_HYDRO_FORECASTS
      LOGICAL (KIND=1) :: GET_WEEKLY_HYDRO_FORECASTS
      LOGICAL (KIND=1) :: GET_TG_2_HYDRO_WEEK

      LOGICAL (KIND=1) :: DOES_WH_FILE_EXIST
      INTEGER (KIND=2) :: YR,MAKER_TABLES=0,DELETE,R_MONTH,MAKER_MONTHS
      INTEGER (KIND=2) :: R_DAY,R_YEAR,R_LOAD_UNIT,LOCAL_LOAD_UNIT=3246
      INTEGER (KIND=2) :: WK,LOCAL_WEEKS,LOCAL_YEARS,NUM_SCEN_VAR,IREC
      INTEGER (KIND=2) :: NUM_HYDRO_TYPES,HYDRO_INDEX,DAY,LREC,R_LREC
      INTEGER (KIND=2) :: LOCAL_LREC,TIMZON,TEMPER,DELTMP,LDE_MONTH
      INTEGER (KIND=2) :: LDE_DAY,LDE_YEAR,DAY_OF_WEEK
      INTEGER (KIND=2) :: MAX_TRANS_GROUP_NUMBER=0
      INTEGER (KIND=2) :: GET_MAX_TRANS_GROUP_NUMBER
      INTEGER (KIND=2) :: REFERENCE_LOAD_NUMBER
      INTEGER (KIND=4) :: DAILY_LOADS_I4(24)
!
      CHARACTER (LEN=5) :: REFERENCE_LOAD_NAME
      CHARACTER (LEN=8) :: EEICODE
!
      INTEGER (KIND=2) :: CUM_WEEK_DAYS_IN_YEAR(0:52)
      INTEGER (KIND=2) :: CUM_MONTH_DAYS_FOR_YEAR(13)
      DATA           CUM_WEEK_DAYS_IN_YEAR/0,7,14,21,28,35,42,49,56, &
                                       63,70,77,84,91,98,105,112, &
                                       119,126,133,140,147,154,161, &
                                       168,175,182,189,196,203,210, &
                                       217,224,231,238,245,252,259, &
                                       266,273,280,287,294,301,308, &
                                       315,322,329,336,343,350,357, &
                                       364/, &
                     CUM_MONTH_DAYS_FOR_YEAR/0,31,59,90,120,151,181,212, &
                                             243,273,304,334,365/

!
      INTEGER (KIND=2) :: TRANSACTION_GROUP,NUM_OF_TRANS_CLASSES,TG,R_WK
      INTEGER (KIND=2) :: R_YR,R_TG,CURRENT_DAY_IN_YEAR
      INTEGER (KIND=2) :: CURRENT_WEEK_IN_YEAR,HR,LOCAL_DAY
      INTEGER (KIND=2) :: R_HOUR_IN_WEEK,SAVE_HOUR_IN_WEEK=0
      INTEGER (KIND=2) :: TG_2_HYDRO_WEEK(:)
      REAL (KIND=4) :: MARKET_ENERGY_PRICE ! R 4
      REAL (KIND=4) :: MONTHLY_ENERGY_PRICE_PATTERN ! R 4
      REAL (KIND=4) :: MARKET_DEMAND_PRICE ! R 4
      REAL (KIND=4) :: MONTHLY_DEMAND_PRICE_PATTERN ! R 4
      REAL (KIND=4) :: MARKET_CUSTOMER_PRICE ! R 4
      REAL (KIND=4) :: DISTRIBUTION_PRICE ! R 4
      REAL (KIND=4) :: TRANSMISSION_PRICE ! R 4
      REAL (KIND=4) :: ASSET_CLASS_ID ! R 4
      REAL (KIND=4) :: ASSET_CLASS_REV_ALLOC_VECTOR ! R 4
      REAL (KIND=4) :: WEEKLY_VALUES(52) !52 X R 4
      REAL (KIND=4) :: R_ENERGY,R_MINIMUM_MW,R_MAXIMUM_MW,REMAIN
      REAL (KIND=4) :: R_WEEKLY_LOAD(168),WEEKLY_REFERENCE_LOAD(168)
      REAL (KIND=4) :: PUMP_WEEKLY_LOAD(168)
      REAL (KIND=4) :: SUM_24_I4,PEAK_24_I4,BASE_24_I4
      REAL (KIND=4) :: SUM_24_R4,PEAK_24_R4,BASE_24_R4
      REAL (KIND=4) :: WEEKLY_REFERENCE_PEAK,WEEKLY_REFERENCE_BASE
      REAL (KIND=4) :: WEEKLY_REFERENCE_ENERGY,HYDRO_WATER_YEAR_MULT
      REAL (KIND=4) :: GET_SCENARIO_HYDRO_WATER_YEAR
      REAL (KIND=4) :: GET_HYDRO_WEEK_PLANNING_PEAK
      REAL (KIND=4) :: PUMPING_CAP_MW,PUMPING_STORAGE_EFFICIENCY
      REAL (KIND=4) :: PUMP_AVG_GEN_CAP_MW,PUMP_MIN_GEN_CAP_MW
      REAL (KIND=4) :: PUMP_MAX_GEN_CAP_MW
      CHARACTER (LEN=6) :: BASECASE_MARKET_AREA_ID ! C 6
      CHARACTER (LEN=6) :: BASECASE_TRANS_AREA_ID ! C 6
      CHARACTER (LEN=6) :: BASECASE_NERC_SUB_ID ! C 6
!
! INPUT DATA LIST
!
      INTEGER (KIND=4) :: VALUES_2_ZERO

      INTEGER (KIND=2) :: SCENARIO_YEAR,TABLE,SCENARIO_INDEX
      CHARACTER (LEN=1) :: RECORD_IS_ACTIVE
      CHARACTER (LEN=20) :: HYDRO_VARIABLES,HYDRO_TYPE
      REAL (KIND=4) :: WEEK_HYDRO_VARIABLE(:,:,:,:,:)
      ALLOCATABLE :: WEEK_HYDRO_VARIABLE,TG_2_HYDRO_WEEK
      LOGICAL (KIND=1) :: LAHEY_LF95
      INTEGER (KIND=2) :: LAST_VALID_REC

         READ_WEEKLY_HYDRO_DATA = .FALSE.
!
         CALL DOES_WEEKLY_HYDRO_FILE_EXIST(DOES_WH_FILE_EXIST)
         IF(DOES_WH_FILE_EXIST) THEN
            CALL OPEN_WEEKLY_HYDRO_FILE
!
            CALL GET_WEEKLY_HYDRO_TABLES(MAKER_TABLES, &
                                                   NUM_OF_TRANS_CLASSES)
!
            LOCAL_WEEKS = 52
            LOCAL_YEARS = 30
            NUM_SCEN_VAR = 3
            NUM_HYDRO_TYPES = 2 ! PEAK CLIPPING AND PUMPED STORAGE
!
            MAX_TRANS_GROUP_NUMBER = GET_MAX_TRANS_GROUP_NUMBER()
!
            IF( ALLOCATED(WEEK_HYDRO_VARIABLE) ) &
                                  DEALLOCATE(WEEK_HYDRO_VARIABLE, &
                                             TG_2_HYDRO_WEEK)
            ALLOCATE( &
                  WEEK_HYDRO_VARIABLE(NUM_HYDRO_TYPES, &
                                      NUM_SCEN_VAR,LOCAL_WEEKS, &
                                      LOCAL_YEARS,NUM_OF_TRANS_CLASSES), &
                                TG_2_HYDRO_WEEK(MAX_TRANS_GROUP_NUMBER))
            VALUES_2_ZERO = &
                         INT(NUM_HYDRO_TYPES * NUM_SCEN_VAR * &
                                       LOCAL_WEEKS * LOCAL_YEARS * &
                                                   NUM_OF_TRANS_CLASSES)

            WEEK_HYDRO_VARIABLE = 0.

            TG_2_HYDRO_WEEK = 0

            TG = 0

            DO TABLE = 1, MAKER_TABLES
               DO YR = 1, AVAIL_DATA_YEARS
                  IREC = (TABLE-1)*AVAIL_DATA_YEARS + YR

                  READ(10,REC=IREC) DELETE, &
                  SCENARIO_YEAR, &
                           RECORD_IS_ACTIVE, &
                           HYDRO_VARIABLES, &
                           TRANSACTION_GROUP, & !  I 2
                           BASECASE_MARKET_AREA_ID, & !  C 6
                           BASECASE_TRANS_AREA_ID, & !  C 6
                           BASECASE_NERC_SUB_ID, & !  C 6
                           MARKET_ENERGY_PRICE, & !  R 4
                           MONTHLY_ENERGY_PRICE_PATTERN, & !  R 4
                           MARKET_DEMAND_PRICE, & !  R 4
                           MONTHLY_DEMAND_PRICE_PATTERN, & !  R 4
                           MARKET_CUSTOMER_PRICE, & !  R 4
                           DISTRIBUTION_PRICE, &
                           TRANSMISSION_PRICE, & !  R 4
                           ASSET_CLASS_ID, & !  R 4
                           ASSET_CLASS_REV_ALLOC_VECTOR, & !  R 4
                           WEEKLY_VALUES, & ! 52 X R 4
                           HYDRO_TYPE, &
                           PUMPING_CAP_MW, &
                           PUMPING_STORAGE_EFFICIENCY, &
                           REFERENCE_LOAD_NAME, &
                           REFERENCE_LOAD_NUMBER
!
                  IF(RECORD_IS_ACTIVE == 'F') CYCLE
!
                  SELECT CASE(trim(HYDRO_VARIABLES))
                     CASE ('Energy_GWh')
                        SCENARIO_INDEX = 1
                     CASE ('Mininum_MW')
                        SCENARIO_INDEX = 2
                     CASE ('Maximum_MW')
                        SCENARIO_INDEX = 3
                  END SELECT
!
                  SELECT CASE(trim(HYDRO_TYPE))
                     CASE ('Pondage')
                        HYDRO_INDEX = 1
                     CASE ('Pumped Storage')
                        HYDRO_INDEX = 2
                     CASE DEFAULT
                        HYDRO_INDEX = 1
                  END SELECT
!
                  PUMPING_STORAGE_EFFICIENCY = &
                                          PUMPING_STORAGE_EFFICIENCY*.01
!
                  IF(TG_2_HYDRO_WEEK(TRANSACTION_GROUP) == 0) THEN
                     TG = TG + 1
                     TG_2_HYDRO_WEEK(TRANSACTION_GROUP) = TG
                  ENDIF
!
!                  TG = GET_TRANS_GROUP_POSITION(TRANSACTION_GROUP)
!
                  DO WK = 1, 52
                     WEEK_HYDRO_VARIABLE(HYDRO_INDEX, &
                                            SCENARIO_INDEX,WK,YR,TG) = &
                                                       WEEKLY_VALUES(WK)
                  ENDDO ! MONTHS
               ENDDO ! RECORDS (YEARS)
!
            ENDDO ! TABLES
            READ_WEEKLY_HYDRO_DATA = .TRUE.
            CALL CLOSE_WEEKLY_HYDRO_FILE
         ENDIF
!
         SAVE_WEEKLY_HYDRO_STATUS = READ_WEEKLY_HYDRO_DATA
         MANAGE_WEEKLY_HYDRO_FORECASTS = .TRUE.
      RETURN
!

      ENTRY GET_TG_2_HYDRO_WEEK(R_TG)

         IF(R_TG > 0 .AND. R_TG <= MAX_TRANS_GROUP_NUMBER) THEN
            GET_TG_2_HYDRO_WEEK =    TG_2_HYDRO_WEEK(R_TG) > 0
         ELSE
            GET_TG_2_HYDRO_WEEK = .FALSE.
         ENDIF
      RETURN
!

      ENTRY GET_HYDRO_WEEK_PLANNING_PEAK(R_TG,R_YR,R_MONTH)


         IF(R_TG > 0 .AND. R_TG <= MAX_TRANS_GROUP_NUMBER .AND. &
                                         TG_2_HYDRO_WEEK(R_TG) > 0) THEN
            IF(R_MONTH == 1) THEN ! JAN
               GET_HYDRO_WEEK_PLANNING_PEAK = MAX( &
                   WEEK_HYDRO_VARIABLE(1,3,1,R_YR,R_TG), &
                   WEEK_HYDRO_VARIABLE(1,3,2,R_YR,R_TG), &
                   WEEK_HYDRO_VARIABLE(1,3,3,R_YR,R_TG), &
                   WEEK_HYDRO_VARIABLE(1,3,4,R_YR,R_TG))
            ELSEIF(R_MONTH == 2) THEN
               GET_HYDRO_WEEK_PLANNING_PEAK = MAX( &
                   WEEK_HYDRO_VARIABLE(1,3,5,R_YR,R_TG), &
                   WEEK_HYDRO_VARIABLE(1,3,6,R_YR,R_TG), &
                   WEEK_HYDRO_VARIABLE(1,3,7,R_YR,R_TG), &
                   WEEK_HYDRO_VARIABLE(1,3,8,R_YR,R_TG))
            ELSEIF(R_MONTH == 3) THEN
               GET_HYDRO_WEEK_PLANNING_PEAK = MAX( &
                   WEEK_HYDRO_VARIABLE(1,3,9,R_YR,R_TG), &
                   WEEK_HYDRO_VARIABLE(1,3,10,R_YR,R_TG), &
                   WEEK_HYDRO_VARIABLE(1,3,11,R_YR,R_TG), &
                   WEEK_HYDRO_VARIABLE(1,3,12,R_YR,R_TG))
            ELSEIF(R_MONTH == 4) THEN
               GET_HYDRO_WEEK_PLANNING_PEAK = MAX( &
                   WEEK_HYDRO_VARIABLE(1,3,13,R_YR,R_TG), &
                   WEEK_HYDRO_VARIABLE(1,3,14,R_YR,R_TG), &
                   WEEK_HYDRO_VARIABLE(1,3,15,R_YR,R_TG), &
                   WEEK_HYDRO_VARIABLE(1,3,16,R_YR,R_TG), &
                   WEEK_HYDRO_VARIABLE(1,3,17,R_YR,R_TG))
            ELSEIF(R_MONTH == 5) THEN
               GET_HYDRO_WEEK_PLANNING_PEAK = MAX( &
                   WEEK_HYDRO_VARIABLE(1,3,18,R_YR,R_TG), &
                   WEEK_HYDRO_VARIABLE(1,3,19,R_YR,R_TG), &
                   WEEK_HYDRO_VARIABLE(1,3,20,R_YR,R_TG), &
                   WEEK_HYDRO_VARIABLE(1,3,21,R_YR,R_TG))
            ELSEIF(R_MONTH == 6) THEN
               GET_HYDRO_WEEK_PLANNING_PEAK = MAX( &
                   WEEK_HYDRO_VARIABLE(1,3,22,R_YR,R_TG), &
                   WEEK_HYDRO_VARIABLE(1,3,23,R_YR,R_TG), &
                   WEEK_HYDRO_VARIABLE(1,3,24,R_YR,R_TG), &
                   WEEK_HYDRO_VARIABLE(1,3,25,R_YR,R_TG))
            ELSEIF(R_MONTH == 7) THEN
               GET_HYDRO_WEEK_PLANNING_PEAK = MAX( &
                   WEEK_HYDRO_VARIABLE(1,3,26,R_YR,R_TG), &
                   WEEK_HYDRO_VARIABLE(1,3,27,R_YR,R_TG), &
                   WEEK_HYDRO_VARIABLE(1,3,28,R_YR,R_TG), &
                   WEEK_HYDRO_VARIABLE(1,3,29,R_YR,R_TG), &
                   WEEK_HYDRO_VARIABLE(1,3,30,R_YR,R_TG))
            ELSEIF(R_MONTH == 8) THEN
               GET_HYDRO_WEEK_PLANNING_PEAK = MAX( &
                   WEEK_HYDRO_VARIABLE(1,3,31,R_YR,R_TG), &
                   WEEK_HYDRO_VARIABLE(1,3,32,R_YR,R_TG), &
                   WEEK_HYDRO_VARIABLE(1,3,33,R_YR,R_TG), &
                   WEEK_HYDRO_VARIABLE(1,3,34,R_YR,R_TG))
            ELSEIF(R_MONTH == 9) THEN
               GET_HYDRO_WEEK_PLANNING_PEAK = MAX( &
                   WEEK_HYDRO_VARIABLE(1,3,35,R_YR,R_TG), &
                   WEEK_HYDRO_VARIABLE(1,3,36,R_YR,R_TG), &
                   WEEK_HYDRO_VARIABLE(1,3,37,R_YR,R_TG), &
                   WEEK_HYDRO_VARIABLE(1,3,38,R_YR,R_TG), &
                   WEEK_HYDRO_VARIABLE(1,3,39,R_YR,R_TG))
            ELSEIF(R_MONTH == 10) THEN
               GET_HYDRO_WEEK_PLANNING_PEAK = MAX( &
                   WEEK_HYDRO_VARIABLE(1,3,40,R_YR,R_TG), &
                   WEEK_HYDRO_VARIABLE(1,3,41,R_YR,R_TG), &
                   WEEK_HYDRO_VARIABLE(1,3,42,R_YR,R_TG), &
                   WEEK_HYDRO_VARIABLE(1,3,43,R_YR,R_TG))
            ELSEIF(R_MONTH == 11) THEN
               GET_HYDRO_WEEK_PLANNING_PEAK = MAX( &
                   WEEK_HYDRO_VARIABLE(1,3,44,R_YR,R_TG), &
                   WEEK_HYDRO_VARIABLE(1,3,45,R_YR,R_TG), &
                   WEEK_HYDRO_VARIABLE(1,3,46,R_YR,R_TG), &
                   WEEK_HYDRO_VARIABLE(1,3,47,R_YR,R_TG))
            ELSEIF(R_MONTH == 12) THEN
               GET_HYDRO_WEEK_PLANNING_PEAK = MAX( &
                   WEEK_HYDRO_VARIABLE(1,3,48,R_YR,R_TG), &
                   WEEK_HYDRO_VARIABLE(1,3,49,R_YR,R_TG), &
                   WEEK_HYDRO_VARIABLE(1,3,50,R_YR,R_TG), &
                   WEEK_HYDRO_VARIABLE(1,3,51,R_YR,R_TG), &
                   WEEK_HYDRO_VARIABLE(1,3,52,R_YR,R_TG))
            ENDIF
         ELSE
            GET_HYDRO_WEEK_PLANNING_PEAK = 0.
         ENDIF
      RETURN
!

      ENTRY GET_WEEKLY_HYDRO_FORECASTS( &
                                       R_DAY, &
                                       R_MONTH, &
                                       R_YR, &
                                       R_TG, &
                                       R_LOAD_UNIT, &
                                       R_ENERGY, &
                                       R_MINIMUM_MW, &
                                       R_MAXIMUM_MW, &
                                       R_WEEKLY_LOAD, &
                                       R_HOUR_IN_WEEK, &
                                       R_LREC)

!
! DOUBLE INDEX
!         TG = TG_2_HYDRO_WEEK(R_TG)
!         TG =
!
!         SAVE_HOUR_IN_WEEK = R_HOUR_IN_WEEK
!
         SCENARIO_INDEX = -99
!
         CALL OPEN_TRANS_HOURLY_LOAD_FILE( &
                                           REFERENCE_LOAD_NAME, &
                                           REFERENCE_LOAD_NUMBER, &
                                           R_YR,LOCAL_LOAD_UNIT,R_MONTH, &
                                           SCENARIO_INDEX)
!
         CURRENT_DAY_IN_YEAR = R_DAY + &
                                    CUM_MONTH_DAYS_FOR_YEAR(R_MONTH) - 1
         CURRENT_WEEK_IN_YEAR = CURRENT_DAY_IN_YEAR/7 + 1
         REMAIN = MOD(FLOAT(CURRENT_DAY_IN_YEAR),7.)

         R_ENERGY =     0.
         R_MINIMUM_MW = 0.
         R_MAXIMUM_MW = 0.
!
         GET_WEEKLY_HYDRO_FORECASTS = .FALSE.
!        A NEW WEEK OR FIRST DAY OF THE MONTH
         IF( REMAIN < .00001 .OR. R_DAY == 1) THEN 

            IF(R_DAY == 1 .AND. REMAIN > .00001) THEN
               LOCAL_LREC = LREC + NINT(REMAIN)
            ELSE
               LOCAL_LREC = R_LREC
            ENDIF

            IF(R_TG > 0 .AND. R_TG <= MAX_TRANS_GROUP_NUMBER) THEN
               TG = TG_2_HYDRO_WEEK(R_TG)
            ELSE
               TG = 0
            ENDIF
            IF(TG > 0) THEN
!
               R_WEEKLY_LOAD = 0.
!
               HYDRO_WATER_YEAR_MULT = &
                             GET_SCENARIO_HYDRO_WATER_YEAR(R_YR,R_MONTH)
!
               R_HOUR_IN_WEEK = 0
               R_MAXIMUM_MW = &
                   WEEK_HYDRO_VARIABLE(1,3,CURRENT_WEEK_IN_YEAR,R_YR,TG)
!
               R_ENERGY = MIN( R_MAXIMUM_MW * 168., &
                  WEEK_HYDRO_VARIABLE( &
                                    1,1,CURRENT_WEEK_IN_YEAR,R_YR,TG) * &
                           1000. * & !  GWH TO MWH
                                                  HYDRO_WATER_YEAR_MULT)
!
               R_MINIMUM_MW = MIN( R_MAXIMUM_MW * 168., &
                   WEEK_HYDRO_VARIABLE( &
                                    1,2,CURRENT_WEEK_IN_YEAR,R_YR,TG) * &
                                                  HYDRO_WATER_YEAR_MULT)
!
               WEEKLY_REFERENCE_ENERGY = 0.
               WEEKLY_REFERENCE_PEAK = 0.
               WEEKLY_REFERENCE_BASE = 999999.
!
               LOCAL_DAY = 0
!
               IF(LAHEY_LF95()) THEN
                  LAST_VALID_REC = 366
               ELSE
                  LAST_VALID_REC = 365
               ENDIF
               DO DAY = LOCAL_LREC, LOCAL_LREC+6
!
                  LOCAL_DAY = LOCAL_DAY + 1
!
                  IF(DAY <= LAST_VALID_REC) THEN
                     LREC = DAY
                  ELSE
                     LREC = DAY - LAST_VALID_REC
                  ENDIF
!                  READ(R_LOAD_UNIT,REC=LREC)
                  READ(LOCAL_LOAD_UNIT,REC=LREC) &
                                     LDE_MONTH,LDE_DAY,LDE_YEAR, &
                                     EEICODE,DAY_OF_WEEK, &
                                     TIMZON,TEMPER, &
                                     DELTMP,DAILY_LOADS_I4

                  WEEKLY_REFERENCE_PEAK = MAX(WEEKLY_REFERENCE_PEAK , &
                                             PEAK_24_I4(DAILY_LOADS_I4))
                  WEEKLY_REFERENCE_BASE = MIN(WEEKLY_REFERENCE_BASE , &
                                             BASE_24_I4(DAILY_LOADS_I4))
!
                  HR = (LOCAL_DAY-1)*24 + 1
!
                  WEEKLY_REFERENCE_LOAD(HR) = DAILY_LOADS_I4(1)
                  WEEKLY_REFERENCE_LOAD(HR+1) = DAILY_LOADS_I4(2)
                  WEEKLY_REFERENCE_LOAD(HR+2) = DAILY_LOADS_I4(3)
                  WEEKLY_REFERENCE_LOAD(HR+3) = DAILY_LOADS_I4(4)
                  WEEKLY_REFERENCE_LOAD(HR+4) = DAILY_LOADS_I4(5)
                  WEEKLY_REFERENCE_LOAD(HR+5) = DAILY_LOADS_I4(6)
                  WEEKLY_REFERENCE_LOAD(HR+6) = DAILY_LOADS_I4(7)
                  WEEKLY_REFERENCE_LOAD(HR+7) = DAILY_LOADS_I4(8)
                  WEEKLY_REFERENCE_LOAD(HR+8) = DAILY_LOADS_I4(9)
                  WEEKLY_REFERENCE_LOAD(HR+9) = DAILY_LOADS_I4(10)
                  WEEKLY_REFERENCE_LOAD(HR+10) = DAILY_LOADS_I4(11)
                  WEEKLY_REFERENCE_LOAD(HR+11) = DAILY_LOADS_I4(12)
                  WEEKLY_REFERENCE_LOAD(HR+12) = DAILY_LOADS_I4(13)
                  WEEKLY_REFERENCE_LOAD(HR+13) = DAILY_LOADS_I4(14)
                  WEEKLY_REFERENCE_LOAD(HR+14) = DAILY_LOADS_I4(15)
                  WEEKLY_REFERENCE_LOAD(HR+15) = DAILY_LOADS_I4(16)
                  WEEKLY_REFERENCE_LOAD(HR+16) = DAILY_LOADS_I4(17)
                  WEEKLY_REFERENCE_LOAD(HR+17) = DAILY_LOADS_I4(18)
                  WEEKLY_REFERENCE_LOAD(HR+18) = DAILY_LOADS_I4(19)
                  WEEKLY_REFERENCE_LOAD(HR+19) = DAILY_LOADS_I4(20)
                  WEEKLY_REFERENCE_LOAD(HR+20) = DAILY_LOADS_I4(21)
                  WEEKLY_REFERENCE_LOAD(HR+21) = DAILY_LOADS_I4(22)
                  WEEKLY_REFERENCE_LOAD(HR+22) = DAILY_LOADS_I4(23)
                  WEEKLY_REFERENCE_LOAD(HR+23) = DAILY_LOADS_I4(24)
!
               ENDDO
!
               IF(R_ENERGY > .1 .AND. R_MAXIMUM_MW > R_MINIMUM_MW) THEN

                  call NonlinearlyMap_R4( &
                     WEEKLY_REFERENCE_LOAD, & !  array source of mapping ...
                     WEEKLY_REFERENCE_BASE, &
                     WEEKLY_REFERENCE_PEAK, &
                     R_WEEKLY_LOAD,         & !  array destination of mapping ...
                     R_MINIMUM_MW, &
                     R_MAXIMUM_MW, &
                     R_ENERGY,INT(168,2))    ! ... for all 168 hours of the week
               ENDIF
!
! 11/26/02. WEEKLY PUMPED STORAGE HOOKED-UP FOR TVA.
!
               IF(PUMPING_CAP_MW > 0. .AND. &
                                   PUMPING_STORAGE_EFFICIENCY > 0.) THEN
                  PUMP_MAX_GEN_CAP_MW = &
                              WEEK_HYDRO_VARIABLE( &
                                    2,3,CURRENT_WEEK_IN_YEAR,R_YR,TG)
!
                  PUMP_AVG_GEN_CAP_MW = MIN( PUMP_MAX_GEN_CAP_MW * 168., &
                              WEEK_HYDRO_VARIABLE( &
                                  2,1,CURRENT_WEEK_IN_YEAR,R_YR,TG) * &
                                                              1000. * & !  GWH TO MWH
                                             HYDRO_WATER_YEAR_MULT)/168.
!
                  PUMP_MIN_GEN_CAP_MW = MIN( PUMP_MAX_GEN_CAP_MW * 168., &
                              WEEK_HYDRO_VARIABLE( &
                                    2,2,CURRENT_WEEK_IN_YEAR,R_YR,TG) * &
                                                  HYDRO_WATER_YEAR_MULT)
!
                  IF(PUMP_MAX_GEN_CAP_MW > 0. .AND. &
                                          PUMP_AVG_GEN_CAP_MW > 0.) THEN
                     call OptimizeWeeksPumpedStorage( &
                        PUMPING_CAP_MW, & !  cannot be inferred from efficiency
                        PUMP_MIN_GEN_CAP_MW, & !  due to a RoR component or min-flow reqmt?
                        PUMP_MAX_GEN_CAP_MW, &
                        PUMP_AVG_GEN_CAP_MW, &
                        PUMPING_STORAGE_EFFICIENCY, &
                        WEEKLY_REFERENCE_LOAD, & !  24x7 detail indexed on [1,168]
                        PUMP_WEEKLY_LOAD) ! 24x7 detail indexed on [1,168]
                     DO YR = 1, 168
                        R_WEEKLY_LOAD(YR) = R_WEEKLY_LOAD(YR) + &
                                 PUMP_WEEKLY_LOAD(YR) - &
                                               WEEKLY_REFERENCE_LOAD(YR)
                     ENDDO
                  ENDIF
               ENDIF
!
               GET_WEEKLY_HYDRO_FORECASTS = .TRUE.
!
            ENDIF ! TG HAS DATA
         ENDIF ! NEW WEEK
!
         CALL CLOSE_TRANS_HOURLY_LOAD_FILE(LOCAL_LOAD_UNIT)

      RETURN
!
      END

      SUBROUTINE CONVERT_MONTHLY_NONLIN_LOADS( &
                     R_MAXIMUM_MW,           & !  FORECAST PEAK
                     R_ENERGY,               & !  FORECAST ENERGY
                     R_MONTH_HOURS)

!
! SOMEWHAT COMPLEX. REQUIRES FOUR CALL POINTS
!

      USE ArrayAllocationInterface
      use end_routine
      use debugtrace
      implicit none
      integer :: file_trace_cmnl=0
      INTEGER (KIND=2) :: I,J,R_DAY,R_MONTH_HOURS ! ... for all hours of the MONTH
      INTEGER (KIND=4) :: R_I4_LOADS(24)
      REAL (KIND=4) :: R_MAXIMUM_MW              ! FORECAST PEAK
      REAL (KIND=4) :: R_ENERGY                  ! FORECAST ENERGY
      REAL (KIND=4) :: R_R4_LOADS(24)
      REAL (KIND=8) :: MONTHLY_REFERENCE_LOAD(:) ! LDE REFERENCE SHAPE
      REAL (KIND=8) :: MONTHLY_REFERENCE_BASE    ! LDE BASE
      REAL (KIND=4) :: MONTHLY_REFERENCE_PEAK    ! LDE PEAK
      REAL (KIND=8) :: MONTHLY_FINAL_LOAD(:)     ! FORCAST SHAPE => OUTPUT OF ALGORITHM
      REAL (KIND=8) :: MINIMUM_MW ! FORECAST BASE
      REAL (KIND=8) :: MAXIMUM_MW ! FORECAST PEAK
      real (kind=8) :: minmw8, maxmw8
      REAL (KIND=8) :: ENERGY ! FORECAST ENERGY
      real (kind=8) :: energy8
      
      ALLOCATABLE :: MONTHLY_REFERENCE_LOAD,MONTHLY_FINAL_LOAD
!
! END DATA DECLARATIONS
!
!

         
         if (file_trace_cmnl==0) then
            file_trace_cmnl=open_trace("CONVERT_MONTHLY_NONLIN_LOADS.trace", &
                rq_who_cmnl)
            call write_trace_message(file_trace_cmnl, "Tracing the " // &
            "CONVERT_MONTHLY_NONLIN_LOADS subroutine.")
         end if
         
         MAXIMUM_MW = R_MAXIMUM_MW              ! FORECAST PEAK
         ENERGY = R_ENERGY                   ! FORECAST ENERGY
!
         IF( ABS(MONTHLY_REFERENCE_PEAK) > .1) THEN
            MINIMUM_MW = R_MAXIMUM_MW * (MONTHLY_REFERENCE_BASE/ &
                                                 MONTHLY_REFERENCE_PEAK)
         ELSE ! ARBITRARY
            MINIMUM_MW = 0.
         ENDIF
         minmw8=minimum_mw
         maxmw8=Maximum_MW
         energy8=energy

         ! Note: MONTHLY_REFERENCE_LOAD is not allocated in this routine, 
         ! though it IS declared here.
         
         ! This call is wh_objt:0021
         call MonthNonlinearlyMap( &
                     MONTHLY_REFERENCE_LOAD, & !  LDE REFERENCE SHAPE
                     real(MONTHLY_REFERENCE_BASE,8), & !  LDE BASE
                     real(MONTHLY_REFERENCE_PEAK,8), & !  LDE PEAK
                     MONTHLY_FINAL_LOAD,   & !  FORCAST SHAPE => OUTPUT OF ALGORITHM
                     minmw8,           & !  FORECAST BASE => CALCULATED
                     maxmw8,           & !  FORECAST PEAK
                     energy8,               & !  FORECAST ENERGY
                     R_MONTH_HOURS)          ! ... for all hours of the MONTH
         call write_trace_message(file_trace_cmnl, &
            "Arguments for MonthNonLinearlyMap")
         call write_trace_real8s(file_trace_cmnl, "MONTHLY_REFERENCE_LOAD", &
            MONTHLY_REFERENCE_LOAD)
         call write_trace_real8(file_trace_cmnl, "MONTHLY_REFERENCE_BASE", &
            MONTHLY_REFERENCE_BASE)

         call write_trace_real8s(file_trace_cmnl, "MONTHLY_FINAL_LOAD", &
            MONTHLY_FINAL_LOAD)
         call write_trace_real8(file_trace_cmnl, "MINIMUM_MW", MINIMUM_MW)
         call write_trace_real8(file_trace_cmnl, "MAXIMUM_MW", MAXIMUM_MW)
         call write_trace_real8(file_trace_cmnl, "ENERGY", ENERGY)
         call write_trace_int2(file_trace_cmnl, "R_MONTH_HOURS", &
            R_MONTH_HOURS)
        minimum_mw=minmw8
        MAXIMUM_MW=maxmw8
        energy=energy8
      RETURN

      ENTRY INIT_DAILY_NONLIN_LOADS(R_MONTH_HOURS)


         CALL AllocateArray(MONTHLY_FINAL_LOAD,R_MONTH_HOURS)
         CALL AllocateArray(MONTHLY_REFERENCE_LOAD,R_MONTH_HOURS)

         
         
         MONTHLY_REFERENCE_BASE = 999999.
         MONTHLY_REFERENCE_PEAK = 0.
      RETURN

      ENTRY ACCUM_DAILY_I4_NONLIN_LOADS(R_DAY,R_I4_LOADS)

         DO I = 1, 24
            J = (R_DAY-1)*24 + I
            MONTHLY_REFERENCE_LOAD(J) = real(R_I4_LOADS(I),8)
!
            MONTHLY_REFERENCE_BASE = MIN(MONTHLY_REFERENCE_BASE, &
                                      REAL(MONTHLY_REFERENCE_LOAD(J),4))
            MONTHLY_REFERENCE_PEAK = MAX(MONTHLY_REFERENCE_PEAK, &
                                      REAL(MONTHLY_REFERENCE_LOAD(J),4))
!
         ENDDO
      RETURN

      ENTRY GET_DAILY_R4_NONLIN_LOADS(R_DAY,R_R4_LOADS)

         DO I = 1, 24
            J = (R_DAY-1)*24 + I
            R_R4_LOADS(I) = MONTHLY_FINAL_LOAD(J)
         ENDDO
      RETURN
!
      END
!
!
!

      subroutine NonlinearlyMap(x,xMin,xMax,y,yMin,yMax,ySum,nCases)
      use end_routine, only: end_program, er_message


      LOGICAL (KIND=1) :: yDecreasing
      integer (KIND=2) :: nCases,iCase,iExpo,nxUnique
      real (KIND=8) :: x(*),y(*),yMin,yMax,ySum,xMin,xMax,PrevSumPower
      real (KIND=8) :: ThisSumPower,TargSumPower,ErrorSumPowr
      real (KIND=8) :: PrevExponent,ThisExponent,NextExponent,xSpan
      real (KIND=8) :: ySpan,LoopGain,ErrToler,z(744)
      parameter (ErrToler=0.000001d0)
!
    ! Using a transformation form of
    !   zi=(yi-yMin)/(yMax-yMin)=[(xi-xMin)/(xMax-xMin)]**b,
    ! use Newton's Method to converge on exponent b which makes
    !   sum(yi,over i on [1,nCases])=ySum, or equivalently
    !   sum(yi-yMin,over i on [1,nCases])=ySum-nCases*yMin, or
    !   sum(zi,over i on [1,nCases])=(ySum-nCases*yMin)/(yMax-yMin),
    ! subject (by construction) to
    !   xMin=>yMin and
    !   xMax=>yMax
    ! and assuming the sequence in array x is monotonic
      if(nCases>744) &
        er_message="wh_objt:0005 - array-size limit exceeded in NonlinearlyMap"
        call end_program(er_message)
      xSpan=xMax-xMin
      ySpan=yMax-yMin

      if(ySpan<0.0d0) then
        er_message='Domain/range void/zero NonlinearlyMap SIID349'
        call end_program(er_message)
      endif
    ! begin with an exponent whose effects are simple
      nxUnique=1
      if(xSpan>0.0d0) then
        PrevExponent=1.0d0
        PrevSumPower=0.0d0
        do iCase=1,nCases
          z(iCase)=(x(iCase)-xMin)/xSpan
          PrevSumPower=PrevSumPower+z(iCase) ! for linear redistribution
          if(iCase==1) cycle
          if(z(iCase).ne.z(iCase-1)) then
            nxUnique=nxUnique+1 ! assumes x is monotonic
          end if
        end do
      end if
      if(ySpan>0.0d0) then
        if(nxUnique<=2) then ! either xMin==xMax or array x has only 2 ...
        ! ... distinct values, both of which conditions preclude the ...
        ! ... exponentiation of the above z from affecting ThisSumPower; ...
        ! ... as a subterfuge, make z a linear interpolation on [0,1]
          yDecreasing=(y(1)>yMin) ! assumes x is monotonic
          do iCase=1,nCases
            if(yDecreasing) then
              z(iCase)=real(nCases-iCase,8)/real(nCases-1,8)
            else
              z(iCase)=real(iCase-1,8)/real(nCases-1,8)
            end if
            PrevSumPower=PrevSumPower+z(iCase) ! for linear redistribution
          end do
        end if
        ! to be distributed on [1,nCases]
        TargSumPower=(ySum-yMin*dble(nCases))/ySpan

      end if
      if((TargSumPower<ErrToler).or.(ySpan<=0.0d0)) then
        iExpo=-1 ! indicating loop not entered
      else
      ! perturb PrevExponent slightly to get an initial slope
        ThisExponent=1.01d0
        do iExpo=1,32 ! limit # of iterations
          ThisSumPower=0.0d0
          do iCase=1,nCases

            ! avoid crashing at 0**(ThisExpo<0)
            if(z(iCase)>0.0d0) then
                ThisSumPower= ThisSumPower+z(iCase)**ThisExponent
            end if
          end do
          ErrorSumPowr=ThisSumPower-TargSumPower

          if(dabs(ErrorSumPowr)<=ErrToler*TargSumPower) exit ! criterion met
         if(dabs(ThisSumPower-PrevSumPower)<=ErrToler*PrevSumPower) then
            exit ! preclude div 0
         endif

          if(ThisExponent*PrevExponent<0.0) then ! damp oscillations
            LoopGain=0.5d0
          else
            LoopGain=1.0d0
          end if
          NextExponent=ThisExponent-ErrorSumPowr*LoopGain* &
            (ThisExponent-PrevExponent)/ &
            (ThisSumPower-PrevSumPower)
          PrevExponent=ThisExponent
          PrevSumPower=ThisSumPower
          ThisExponent=NextExponent
        end do
      end if
      if(ThisExponent<0.0001d0) then
      ! iExpo=0 ! implying no alterations made below
      ! stop 'negative exponent in NonlinearlyMap => infeasible request'
        iExpo=0 ! implying 'force all but 1st value to the max'
      end if

    ! having determined the best-fit ThisExponent, assign output values

      do iCase=1,nCases
        if(iExpo>0) then
          if(z(iCase)>0.0d0) then ! avoid crashing at 0**(ThisExpo<0)
            y(iCase)=yMin+ySpan*z(iCase)**ThisExponent
          else
            y(iCase)=yMin
          end if
        elseif(iExpo==0) then ! force all but 1st value to the max
          if(x(iCase)>xMin) then
            y(iCase)=yMax
          else
            y(iCase)=ySum-real(nCases-1,8)*yMax
          end if
      ! else iExpo==-1; retain incoming or assigned curve's shape
        end if

      end do
      end ! subroutine NonlinearlyMap
!-----

      subroutine NonlinearlyMap_R4(x,xMin,xMax,y,yMin,yMax,ySum,nCases)
      use end_routine, only: end_program, er_message


      LOGICAL (KIND=1) :: yDecreasing
      integer (KIND=2) :: nCases,iCase,iExpo,nxUnique
      real (KIND=4) :: x(*),y(*),yMin,yMax,ySum,xMin,xMax
      real (KIND=8) :: PrevSumPower,ThisSumPower,TargSumPower
      real (KIND=8) :: ErrorSumPowr,PrevExponent,ThisExponent
      real (KIND=8) :: NextExponent,xSpan,ySpan,LoopGain,ErrToler,z(744)
      parameter(ErrToler=0.000001d0)
!
    ! Using a transformation form of
    !   zi=(yi-yMin)/(yMax-yMin)=[(xi-xMin)/(xMax-xMin)]**b,
    ! use Newton's Method to converge on exponent b which makes
    !   sum(yi,over i on [1,nCases])=ySum, or equivalently
    !   sum(yi-yMin,over i on [1,nCases])=ySum-nCases*yMin, or
    !   sum(zi,over i on [1,nCases])=(ySum-nCases*yMin)/(yMax-yMin),
    ! subject (by construction) to
    !   xMin=>yMin and
    !   xMax=>yMax
    ! and assuming the sequence in array x is monotonic
      if(nCases>744) then
        call end_program('array-size limit exceeded in NonlinearlyMap')
      endif
      xSpan=xMax-xMin
      ySpan=yMax-yMin

      if(ySpan<0.0d0) then
         er_message='Stop requested from wh_objt SIID350'
         call end_program(er_message)
      endif

    ! begin with an exponent whose effects are simple
      nxUnique=1
      if(xSpan>0.0d0) then
        PrevExponent=1.0d0
        PrevSumPower=0.0d0
        do iCase=1,nCases
          z(iCase)=(x(iCase)-xMin)/xSpan
          PrevSumPower=PrevSumPower+z(iCase) ! for linear redistribution
          if(iCase==1) cycle
          if(z(iCase).ne.z(iCase-1)) then
            nxUnique=nxUnique+1 ! assumes x is monotonic
          endif
        end do
      end if
      if(ySpan>0.0d0) then
        if(nxUnique<=2) then ! either xMin==xMax or array x has only 2 ...
        ! ... distinct values, both of which conditions preclude the ...
        ! ... exponentiation of the above z from affecting ThisSumPower; ...
        ! ... as a subterfuge, make z a linear interpolation on [0,1]
          yDecreasing=(y(1)>yMin) ! assumes x is monotonic
          do iCase=1,nCases
            if(yDecreasing) then
              z(iCase)=real(nCases-iCase,8)/real(nCases-1,8)
            else
              z(iCase)=real(iCase-1,8)/real(nCases-1,8)
            end if
            PrevSumPower=PrevSumPower+z(iCase) ! for linear redistribution
          end do
        end if
        TargSumPower=(ySum-yMin*real(nCases,8))/ySpan ! to be distributed on [1,nCases]

      end if
      if((TargSumPower<ErrToler).or.(ySpan<=0.0d0)) then
        iExpo=-1 ! indicating loop not entered
      else
      ! perturb PrevExponent slightly to get an initial slope
        ThisExponent=1.01d0
        do iExpo=1,32 ! limit # of iterations
          ThisSumPower=0.0d0
          do iCase=1,nCases

            if(z(iCase)>0.0d0)ThisSumPower= & !  avoid crashing at 0**(ThisExpo<0)
                              ThisSumPower+z(iCase)**ThisExponent
          end do
          ErrorSumPowr=ThisSumPower-TargSumPower

          if(dabs(ErrorSumPowr)<=ErrToler*TargSumPower) then
            exit ! criterion met
          end if
          if(dabs(ThisSumPower-PrevSumPower)<=ErrToler*PrevSumPower) then
            exit ! preclude div 0
          end if
          if(ThisExponent*PrevExponent<0.0) then ! damp oscillations
            LoopGain=0.5d0
          else
            LoopGain=1.0d0
          end if
          NextExponent=ThisExponent-ErrorSumPowr*LoopGain* &
            (ThisExponent-PrevExponent)/ &
            (ThisSumPower-PrevSumPower)
          PrevExponent=ThisExponent
          PrevSumPower=ThisSumPower
          ThisExponent=NextExponent
        end do
      end if
      if(ThisExponent<0.0001d0) then

        iExpo=0 ! implying 'force all but 1st value to the max'
      end if

    ! having determined the best-fit ThisExponent, assign output values

      do iCase=1,nCases
        if(iExpo>0) then
          if(z(iCase)>0.0d0) then ! avoid crashing at 0**(ThisExpo<0)
            y(iCase)=yMin+ySpan*z(iCase)**ThisExponent
          else
            y(iCase)=yMin
          end if
        elseif(iExpo==0) then ! force all but 1st value to the max
          if(x(iCase)>xMin) then
            y(iCase)=yMax
          else
            y(iCase)=ySum-real(nCases-1,8)*yMax
          end if
      ! else iExpo==-1; retain incoming or assigned curve's shape
        end if

      end do
      end ! subroutine NonlinearlyMap
!-----

      subroutine MonthNonlinearlyMap( &
                                    x,xMin,xMax,y,yMin,yMax,ySum,nCases)
      use end_routine, only: end_program, er_message
      use debugtrace
    ! for feasibility, we require yMin<(ySum/nCases)<yMax
      integer :: file_trace_mlnmap=0
      LOGICAL (KIND=1) :: Feasible,yDecreasing,L1_FALSE=.false.
      LOGICAL (KIND=1) :: MappingFailed
      integer (KIND=2) :: nCases,iCase,jCase,pCase,iExpo,nxUnique,nz0
      integer (KIND=2) :: nz1,nHalfCycles,iHalfCycle,modulus,OrgOrder(:)
      real (KIND=8) :: x(*),y(*),yMin,yMax,ySum,xMin,xMax,PrevSumPower
      real (KIND=8) :: ThisSumPower,TargSumPower,ErrorSumPowr
      real (KIND=8) :: PrevExponent,ThisExponent,NextExponent,xSpan
      real (KIND=8) :: ySpan,z(:),xDiff,pDiff,LoopGain,ErrToler
      real (KIND=8) :: LimExponent
      real (kind=8), parameter :: underflow=2.225074D-308

!
      parameter(ErrToler=0.000001d0, &
        LimExponent=999.9d0) ! 35.0d0 before 20040228
      allocatable :: z,OrgOrder
      if(file_trace_mlnmap==0) then
        file_trace_mlnmap=open_trace("MonthNonlinearlyMap.trace", rq_wh_mlnmap)
        call write_trace_message(file_trace_mlnmap, &
            "Tracing MonthNonLinearlyMap routine.")
      end if
      

      
      
      
      
! END DECLARATIONS

    ! Using a transformation form of
    !   zi=(yi-yMin)/(yMax-yMin)=[(xi-xMin)/(xMax-xMin)]**b,
    ! use Newton's Method to converge on exponent b which makes
    !   sum(yi,over i on [1,nCases])=ySum, or equivalently
    !   sum(yi-yMin,over i on [1,nCases])=ySum-nCases*yMin, or
    !   sum(zi,over i on [1,nCases])=(ySum-nCases*yMin)/(yMax-yMin),
    ! subject (by construction) to
    !   xMin=>yMin and
    !   xMax=>yMax
    ! and where the sequence in array x may be non-monotonic (even cyclic)
      MappingFailed = .false.
      if(allocated(z)) deallocate(z,OrgOrder)
      allocate(z(nCases))
      allocate(OrgOrder(nCases))
      do iCase=1,nCases
        OrgOrder(iCase)=iCase ! before sorting
      end do
      call Indexed_SortR8_Ascending(x,OrgOrder,nCases,L1_FALSE)
      nxUnique=1
      nHalfCycles=1
      pCase=OrgOrder(1)
      pDiff=0.0d0
      do jCase=2,nCases ! count distinct values
        iCase=OrgOrder(jCase)
        if(x(iCase).ne.x(pCase)) nxUnique=nxUnique+1
        xDiff=x(jCase)-x(jCase-1) ! difference in input-ordered pairs
        if(xDiff*pDiff<0.0d0) nHalfCycles=nHalfCycles+1
        
        
        pCase=iCase
        if(dabs(xDiff)>0.0d0) then 
            pDiff=xDiff
        end if
        
      end do
      xSpan=xMax-xMin
      ySpan=yMax-yMin
      if(ySpan<0.0d0) then
        er_message='Output range negative: MonthNonlinearlyMap SIID351'
        call end_program(er_message)
      endif

    ! begin with an exponent whose effects are simple
      PrevExponent=1.0d0
      PrevSumPower=0.0d0
      nz0=0
      nz1=0
      if(xSpan>0.0d0) then
        do iCase=1,nCases
          z(iCase)=(x(iCase)-xMin)/xSpan
          PrevSumPower=PrevSumPower+z(iCase) ! for linear redistribution
        end do

      end if
      if(ySpan>0.0d0) then ! after 20040227
      ! then either xMin==xMax or array x has only 2 ...
      ! ... distinct values, both of which conditions preclude the ...
      ! ... exponentiation of the above z from affecting ThisSumPower;
      ! ... as a subterfuge, make z a linear interpolation on [0,1]
        if((nxUnique<=2) &
          .or.((nxUnique<nHalfCycles*3).and.(nHalfCycles==nCases/12))) &
          then
          yDecreasing=(y(1)>yMin)
          modulus=nCases
          if(nHalfCycles>1) modulus=nCases/nHalfCycles
          PrevSumPower=0.0d0
          do iCase=1,nCases
            if((nxUnique>2).and.(x(iCase).ne.xMin) &
                           .and.(x(iCase).ne.xMax)) then
            continue ! with preassigned z(iCase)=(x(iCase)-xMin)/xSpan
            elseif(yDecreasing) then
            z(iCase)=real(mod(nCases-iCase,modulus),8)/real(modulus-1,8)
            else
          z(iCase)=real(mod(iCase-int(1,2),modulus),8)/real(modulus-1,8)
            end if
            iHalfCycle=1+(iCase-1)/modulus ! 1 for iCase on [1,modulus]
            if(z(iCase)<0.001d0) nz0=nz0+1 ! exclude invariant terms
            if(z(iCase)>0.999d0) nz1=nz1+1 ! exclude invariant terms
            if(mod(iHalfCycle,2)==0) then
                z(iCase)=1.0d0-z(iCase) ! for 2nd half of cycle
            end if
            PrevSumPower=PrevSumPower+z(iCase) ! linear redistribution
          end do
        end if
        ! to be distributed on [1,nCases]
        TargSumPower=(ySum-yMin*real(nCases,8))/ySpan 
        Feasible=.true.
        
        if(TargSumPower>PrevSumPower) then 
          ! leaving nz0 at 0 allows at most ...
          Feasible=(TargSumPower<real(nCases-nz0,8)) ! ... (nCases-nz0) at 1
          if(.not.Feasible) then ! leaving nz0 values at 0,
          ! force all non-z0 variables to a revised maximum
            nz1=nCases-nz0
            yMax=(ySum-yMin*real(nz0,8))/real(nz1,8)
            write(4,'(f9.4,a)') yMax,' Infeasible MNLM yMax relaxed.'
          end if
        else ! TargSumPower<=PrevSumPower
          Feasible=(TargSumPower>real(nz1,8))
          if(.not.Feasible) then ! leaving nz1 values at 1,
          ! force all non-z1 variables to a revised minimum
            nz0=nCases-nz1
            yMin=(ySum-yMax*real(nz1,8))/real(nz0,8)
            write(4,'(f9.4,a)') yMin,' Infeasible MNLM yMin relaxed.'
          end if
        end if
        if(.not.Feasible) then
          ! TODO: consider whether a STEP decoration (add , -1 to the DO line) 
          ! should be added to this DO loop instead of reversing the values into 
          ! the OrgOrder array.
          do jCase=1,nCases ! in descending order
            iCase=OrgOrder(jCase)
            if(jCase<=nz1) then
              y(iCase)=yMax
            else
              y(iCase)=yMin
            end if
          end do
      call write_trace_real8s(file_trace_mlnmap, "1. X", x(2:ncases))
      call write_trace_real8(file_trace_mlnmap, "1. xMin", xMin)
      call write_trace_real8(file_trace_mlnmap, "1. xMax", xMax)
      call write_trace_real8s(file_trace_mlnmap, "1. Y", y(2:ncases))
      call write_trace_real8(file_trace_mlnmap, "1. ySum", ySum)
      call write_trace_int2(file_trace_mlnmap, "1. nCases", nCases)
      
          return
        end if

      end if
      if((TargSumPower<ErrToler).or.(ySpan<=0.0d0)) then
        iExpo=-1 ! indicating loop not entered
      else
      ! perturb PrevExponent slightly to get an initial slope
        ThisExponent=1.01d0
        do iExpo=1,32 ! limit # of iterations
          ThisSumPower=0.0d0
          do iCase=1,nCases

            if(z(iCase)>0.0d0) then 
               !  avoid crashing at 0**(ThisExpo<0)
               ThisSumPower=ThisSumPower+z(iCase)**ThisExponent
            end if
          end do
          ErrorSumPowr=ThisSumPower-TargSumPower

          if(dabs(ErrorSumPowr)<ErrToler*TargSumPower) then
            exit ! criterion met
          end if
          if(dabs(ThisSumPower-PrevSumPower)<ErrToler*PrevSumPower) then
!            stop "MonthNonLinearlyMap failed; check inputs."
            write(4,'(4i4,3f9.4,2f13.6,a)') nHalfCycles,nCases,nz0,nz1, &
              yMin,yMax,ySpan,ySum,TargSumPower,' SumPow target'
            write(4,'(a)') ' MonthNonLinearlyMap failed; check inputs.'
            MappingFailed = .true.
            exit ! preclude div 0 attempt below
          end if
          if(ThisExponent*PrevExponent<0.0d0) then ! damp oscillations
            LoopGain=0.5d0
          else
            LoopGain=1.0d0
          end if
          NextExponent=ThisExponent-ErrorSumPowr*LoopGain* &
            (ThisExponent-PrevExponent)/ &
            (ThisSumPower-PrevSumPower)
          if(ThisExponent*NextExponent<0.0d0) then
             !  condition added 20040311
            NextExponent=NextExponent/16.0d0 ! ... to preclude divergence
          end if
          PrevExponent=ThisExponent
          PrevSumPower=ThisSumPower
          ThisExponent=NextExponent
          if(dabs(ThisExponent)>=LimExponent) exit
        end do
      end if

      do iCase=1,nCases

        if(MappingFailed) then
          y(iCase) = x(iCase)
        elseif(iExpo>0) then
          if(z(iCase)>0.0d0) then ! avoid crashing at 0**(ThisExpo<0)
            y(iCase)=yMin+ySpan*z(iCase)**ThisExponent
          else
            y(iCase)=yMin
          end if
        elseif(iExpo==0) then ! force all but 1st value to the max
          if(x(iCase)>xMin) then
            y(iCase)=yMax
          else
            y(iCase)=ySum-real(nCases-1,8)*yMax
          end if
        end if
      end do
      call write_trace_real8s(file_trace_mlnmap, "2. X", x(2:ncases))
      call write_trace_real8(file_trace_mlnmap, "2. xMin", xMin)
      call write_trace_real8(file_trace_mlnmap, "2. xMax", xMax)
      call write_trace_real8s(file_trace_mlnmap, "2. Y", y(2:ncases))
      call write_trace_real8(file_trace_mlnmap, "2. ySum", ySum)
      call write_trace_int2(file_trace_mlnmap, "2. nCases", nCases)
      end ! subroutine MonthNonlinearlyMap
!----
!**********************************************************************
      subroutine SquareUpPattern(x,xMin,xMax,y,yMin,yMax,ySum,nCases)
       use end_routine
       implicit none
    ! for feasibility, we require yMin<(ySum/nCases)<yMax
      LOGICAL (KIND=1) :: L1_FALSE=.false.
      integer (KIND=2) :: nCases,iCase,jCase,pCase,nxUnique,nyAtMax
      integer (KIND=2) :: jIntermediate,iIntermediate,OrgOrder(:)
      real (KIND=8) :: x(*),y(*),yMin,yMax,ySum,xMin,xMax,ThisySum
      allocatable :: OrgOrder
!
! END DECLARATIONS
!
    ! Move the nearest intermediate values to either extreme to make
    !   sum(yi,over i on [1,nCases])=ySum, or equivalently
    !   sum(yi-yMin,over i on [1,nCases])=ySum-nCases*yMin
    ! subject (by construction) to
    !   xMin=>yMin and
    !   xMax=>yMax
    ! and where the sequence in array x may be non-monotonic (even cyclic)
      if((xMax<=xMin).or.(yMax<=yMin)) then
        return ! no transformation is possible
      end if
      if(allocated(OrgOrder)) deallocate(OrgOrder)
      allocate(OrgOrder(nCases))
      do iCase=1,nCases
        OrgOrder(iCase)=iCase ! before sorting
      end do
      call Indexed_SortR8_Ascending(x,OrgOrder,nCases,L1_FALSE)
       ! truncated to an integer
      nyAtMax=(ySum-yMin*real(nCases,8))/(yMax-yMin)
      ! only y at this rank will not be at yMin or yMax
      jIntermediate=nyAtMax+1 
      iIntermediate=OrgOrder(jIntermediate)
      nxUnique=1
      pCase=OrgOrder(1)
      y(pCase)=yMax
      do jCase=2,nCases ! count distinct values
        iCase=OrgOrder(jCase)
        if(x(iCase).ne.x(pCase)) nxUnique=nxUnique+1
        if(jCase<=nyAtMax) then
          y(iCase)=yMax
        else
          y(iCase)=yMin
        end if

        pCase=iCase
      end do
      deallocate(OrgOrder)
      if(nxUnique<=2) then
      ! use exponential transformation of interpolates
        call MonthNonlinearlyMap(x,xMin,xMax,y,yMin,yMax,ySum,nCases)
        return
      end if
      ThisySum=yMin*real(nCases,8)+(yMax-yMin)*real(nyAtMax,8)
      y(iIntermediate)=y(iIntermediate)-(ThisySum-ySum)

      end ! subroutine SquareUpPattern


      subroutine Indexed_SortR8_Ascending(a,Ofs,nItems,Ascending)
      use end_routine
      implicit none
!     sorts nItems items into Ascending order(Ofs array); from EvaluInc.fpp
      integer (KIND=2) :: nItems,i,j,k,Gap,Ofs(*),Hold
      LOGICAL (KIND=1) :: Ascending ! .false. => Descending order
      real (KIND=8) :: a(*) ! the only difference from Indexed_Sort_Ascending
!
      Gap=nItems/2
      do while(Gap>0)
        do i=Gap+1,nItems
          j=i-Gap
          do while(j>0)
            k=j+Gap
            if((      Ascending .and.(a(Ofs(j))<=a(Ofs(k)))).or. &
               ((.not.Ascending).and.(a(Ofs(j))>=a(Ofs(k))))) then
              j=0 ! break the while loop (assign j=-1 for 0-based arrays)
            else ! interchange the index values

              Hold=Ofs(j)
              Ofs(j)=Ofs(k)
              Ofs(k)=Hold
            end if
            j=j-Gap
          end do
        end do
        Gap=Gap/2
      end do
      end ! subroutine Indexed_SortR8_Ascending


!
!        PROGRAM TO READ MULTI-TAB INFORMATION ON WEATHER DEMAND
!                 AND CONVERT TO BINARY FORMAT
!                       COPYRIGHT (C) 2001
!      ALL RIGHTS RESERVED M.S. GERBER & ASSOCIATES, INC.
!

!
      SUBROUTINE WEATHER_DEMAND_OBJECT
      use end_routine, only: end_program, er_message
      use SpinDriftLib
      USE PROD_ARRAYS_DIMENSIONS
      USE SIZECOM
      USE iostatmsg
      use mod_base_year
      implicit none
      LOGICAL (KIND=1) :: SAVE_WD_FILE_EXISTS=.FALSE.,R_WD_FILE_EXISTS
      LOGICAL (KIND=1) :: R_WD_FILE_USED,SAVE_WD_FILE_USED=.FALSE.
      INTEGER (KIND=2) :: INUNIT,IREC,DELETE,LRECL=229,R_LRECL
      INTEGER (KIND=2) :: SAVE_TRANS_LOAD_RECORDS=0,R_TRANS_LOAD_RECORDS
      INTEGER (KIND=2) :: TEMP_YEAR,R_NUM_OF_TRANS_CLASSES
      INTEGER (KIND=2) :: NUM_OF_OL_TRANS_CLASSES=0
      INTEGER (KIND=2) :: R_MAX_TRANS_CLASS_NUM
      INTEGER (KIND=2) :: MAX_OL_TRANS_CLASS_ID_NUM=0
      INTEGER (KIND=2) :: R_NUM_OF_ASSET_CLASSES
      INTEGER (KIND=2) :: NUM_OF_OL_ASSET_CLASSES=0
      INTEGER (KIND=2) :: R_MAX_ASSET_CLASS_NUM
      INTEGER (KIND=2) :: MAX_OL_ASSET_CLASS_ID_NUM=0
      INTEGER (KIND=2) :: R_NUM_OF_CUST_CLASSES,NUM_OF_OL_CUST_CLASSES=0
      INTEGER (KIND=2) :: R_MAX_CUST_CLASS_NUM
      INTEGER (KIND=2) :: MAX_OL_CUST_CLASS_ID_NUM=0
      INTEGER (KIND=2) :: NUM_OF_BC_TRANS_CLASSES=0
      INTEGER (KIND=2) :: MAX_BC_TRANS_CLASS_ID_NUM=0
      INTEGER (KIND=2) :: NUM_OF_BC_ASSET_CLASSES=0
      INTEGER (KIND=2) :: MAX_BC_ASSET_CLASS_ID_NUM=0
      INTEGER (KIND=2) :: NUM_OF_BC_CUST_CLASSES=0
      INTEGER (KIND=2) :: MAX_BC_CUST_CLASS_ID_NUM=0
      INTEGER (KIND=2) :: TEMP_TRANS_CLASS_POINTER(:)
      INTEGER (KIND=2) :: TEMP_ASSET_CLASS_POINTER(:)
      INTEGER (KIND=2) :: TEMP_CUST_CLASS_POINTER(:),TRANSACTION_GROUP
      INTEGER (KIND=2) :: CUSTOMER_GROUP
      INTEGER (KIND=4) :: IOS,IOS_BASE
      ALLOCATABLE :: TEMP_TRANS_CLASS_POINTER,TEMP_ASSET_CLASS_POINTER, &
                  TEMP_CUST_CLASS_POINTER
      CHARACTER (LEN=5) :: WEATHER_DEMAND_FILE,OVERLAY_FAMILY_NAME
      CHARACTER (LEN=5) :: BSYRLOAD
      CHARACTER (LEN=256) :: FILE_NAME
      CHARACTER (LEN=256) :: OUTPUT_DIRECTORY
      CHARACTER (LEN=256) :: BASE_FILE_DIRECTORY
      CHARACTER (LEN=152) :: MESSAGE
      LOGICAL (KIND=4) :: FILE_EXISTS
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER (LEN=1024) :: RECLN
! DECLARATION FOR SYSTEM FORECAST VARIABLES
      INTEGER (KIND=2) :: UNIT_NUM=10
!
! SIMULATION VARIABLES
!
      CHARACTER (LEN=1) :: RECORD_IS_ACTIVE ! C 1
      CHARACTER (LEN=20) :: FORECAST_SOURCE ! C 20
      CHARACTER (LEN=32) :: MARKET_AREA_COMPANY_NAME ! C 32
      CHARACTER (LEN=5) :: MARKET_AREA_COMPANY_ID,REFERENCE_LOAD_NAME ! C 5
      INTEGER (KIND=2) :: REFERENCE_LOAD_NUMBER ! I 2
      INTEGER (KIND=4) :: DATE_OF_FORECAST_ACTUAL_DATA ! I 4
      CHARACTER (LEN=9) :: DAY_OF_WEEK ! C 9
      REAL (KIND=4) :: HOUR_OF_DAY(24) ! 24x R 4
      REAL (KIND=4) :: ASSET_CLASS_ID,ASSET_CLASS_REV_ALLOC_VECTOR
      CHARACTER (LEN=50) :: COMMENT ! C 50
!
      CHARACTER (LEN=17) :: FILE_TYPE='Weather Demand   '
      CHARACTER (LEN=2) :: WEATHER_DEMAND_OL='BC',R_WEATHER_DEMAND_OL
      LOGICAL (KIND=1) :: LAHEY_LF95
      CHARACTER (LEN=30) :: SCREEN_OUTPUT
!
      INTEGER (KIND=2) :: STUDY_BASE_YEAR
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

      ENTRY WEATHER_DEMAND_MAKEBIN

      STUDY_BASE_YEAR = get_base_year()
      FILE_NAME = trim(BASE_FILE_DIRECTORY())// &
                            "WDB"//trim(WEATHER_DEMAND_FILE())//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
!
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT=trim(FILE_TYPE)//'-'//WEATHER_DEMAND_FILE()
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_LOCATE_WRITE(16,30,WEATHER_DEMAND_FILE(), &
                                                         ALL_VERSIONS,0)
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
         ALLOCATE(   TEMP_TRANS_CLASS_POINTER(0:1023), &
                     TEMP_ASSET_CLASS_POINTER(0:1023), &
                     TEMP_CUST_CLASS_POINTER(0:1023))
         TEMP_TRANS_CLASS_POINTER = 0
         TEMP_ASSET_CLASS_POINTER = 0
         TEMP_CUST_CLASS_POINTER = 0
!
         SAVE_WD_FILE_EXISTS = .TRUE.
!

      
         OPEN(10,FILE=FILE_NAME) ! wdb file
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCWDYFC.BIN", &
                            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
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

               IF(RECLN(1:1) == '7' .OR. IOS /= 0) THEN ! END OF TABLE

                  EXIT ! LEAVE LOOP (GO TO NEXT TABLE OR EXIT ROUTINE)

               ENDIF
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,ERR=200) DELETE, &
                           RECORD_IS_ACTIVE, & !  C 1
                           FORECAST_SOURCE, & !  C 20
                           MARKET_AREA_COMPANY_NAME, & !  C 32
                           MARKET_AREA_COMPANY_ID, & !  C 5
                           REFERENCE_LOAD_NAME, & !  C 5
                           REFERENCE_LOAD_NUMBER, & !  I 2
                           DATE_OF_FORECAST_ACTUAL_DATA, & !  I 4
                           DAY_OF_WEEK, & !  C 9
                           HOUR_OF_DAY, & !  24x R 4
                           COMMENT, & !  C 50
                           TRANSACTION_GROUP

               IREC = IREC + 1
!

               IF(TEMP_TRANS_CLASS_POINTER(TRANSACTION_GROUP) == 0) THEN
                  NUM_OF_BC_TRANS_CLASSES = NUM_OF_BC_TRANS_CLASSES + 1
                  MAX_BC_TRANS_CLASS_ID_NUM = MAX( &
                            MAX_BC_TRANS_CLASS_ID_NUM,TRANSACTION_GROUP)
                  TEMP_TRANS_CLASS_POINTER(TRANSACTION_GROUP) = 1
               ENDIF
           IF(TEMP_ASSET_CLASS_POINTER(int(ASSET_CLASS_ID,2)) == 0) THEN
                  NUM_OF_BC_ASSET_CLASSES = NUM_OF_BC_ASSET_CLASSES + 1
                  MAX_BC_ASSET_CLASS_ID_NUM = MAX( &
                        MAX_BC_ASSET_CLASS_ID_NUM,int(ASSET_CLASS_ID,2))
                  TEMP_ASSET_CLASS_POINTER(int(ASSET_CLASS_ID,2)) = 1
           ENDIF
               IF(TEMP_CUST_CLASS_POINTER(CUSTOMER_GROUP) == 0) THEN
                  NUM_OF_BC_CUST_CLASSES = NUM_OF_BC_CUST_CLASSES + 1
                  MAX_BC_CUST_CLASS_ID_NUM = MAX( &
                            MAX_BC_CUST_CLASS_ID_NUM,CUSTOMER_GROUP)
                  TEMP_CUST_CLASS_POINTER(CUSTOMER_GROUP) = 1
               ENDIF
!
               WRITE(11,REC=IREC) DELETE, &
                           RECORD_IS_ACTIVE, & !  C 1
                           FORECAST_SOURCE, & !  C 20
                           MARKET_AREA_COMPANY_NAME, & !  C 32
                           MARKET_AREA_COMPANY_ID, & !  C 5
                           REFERENCE_LOAD_NAME, & !  C 5
                           REFERENCE_LOAD_NUMBER, & !  I 2
                           DATE_OF_FORECAST_ACTUAL_DATA, & !  I 4
                           DAY_OF_WEEK, & !  C 9
                           HOUR_OF_DAY, & !  24x R 4
                           COMMENT, & !  C 50
                           TRANSACTION_GROUP
            ENDDO ! READ RECORDS
            IF(IOS /= 0) EXIT
         ENDDO ! TABLES
         CLOSE(10)
      ELSE IF(INDEX(WEATHER_DEMAND_FILE(),'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "wh_objt:1119")
      ELSE
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCWDYFC.BIN", &
                            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)

         SAVE_WD_FILE_EXISTS = .FALSE.
!
      ENDIF

      CLOSE(11)
      IF(ALLOCATED(TEMP_TRANS_CLASS_POINTER)) &
            DEALLOCATE(    TEMP_TRANS_CLASS_POINTER, &
                           TEMP_ASSET_CLASS_POINTER, &
                           TEMP_CUST_CLASS_POINTER)
 !
      SAVE_TRANS_LOAD_RECORDS = IREC
 !
      RETURN

  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from wh_objt SIID351'
      call end_program(er_message)

!
!          ROUTINE TO CREATE OVERLAY FILES
!          COPYRIGHT (C) 1984-88  M.S. GERBER & ASSOCIATES, INC.
!          COPYRIGHT (C) 1991-92  M.S. GERBER & ASSOCIATES, INC.
!

!

      ENTRY WEATHER_DEMAND_MAKEOVL(OVERLAY_FAMILY_NAME)

      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      FILE_NAME=trim(OUTPUT_DIRECTORY())//"WDO"// &
                                     trim(OVERLAY_FAMILY_NAME)//".DAT"

      OPEN(10,FILE=FILE_NAME) ! Wdo file
      READ(10,*) DELETE
      INUNIT = 12
      IF(WEATHER_DEMAND_OL == 'BC') THEN
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCWDYFC.BIN", &
                                             ACCESS="DIRECT",RECL=LRECL)
         INUNIT = 11
      ENDIF
!     INQUIRE(UNIT=12,OPENED=FILE_EXISTS)
      FILE_NAME = trim(OUTPUT_DIRECTORY())//"OLWDYFC.BIN"
      OPEN(12,FILE=FILE_NAME,ACCESS="DIRECT", &
                                 STATUS="UNKNOWN",RECL=LRECL,IOSTAT=IOS)
      IF(IOS /= 0) THEN
         CALL iostatmsg_unit(IOS,MESSAGE)
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
      ALLOCATE(TEMP_TRANS_CLASS_POINTER(0:1023), &
               TEMP_ASSET_CLASS_POINTER(0:1023), &
               TEMP_CUST_CLASS_POINTER(0:1023))
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
            READ(INUNIT,REC=IREC,IOSTAT=IOS_BASE) &
                              DELETE, &
                              RECORD_IS_ACTIVE, & !  C 1
                              FORECAST_SOURCE, & !  C 20
                              MARKET_AREA_COMPANY_NAME, & !  C 32
                              MARKET_AREA_COMPANY_ID, & !  C 5
                              REFERENCE_LOAD_NAME, & !  C 5
                              REFERENCE_LOAD_NUMBER, & !  I 2
                              DATE_OF_FORECAST_ACTUAL_DATA, & !  I 4
                              DAY_OF_WEEK, & !  C 9
                              HOUR_OF_DAY, & !  24x R 4
                              COMMENT, & !  C 50
                              TRANSACTION_GROUP
            IF(IOS_BASE /= 0) EXIT
            IF(IOS == 0) THEN
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,ERR=200) DELETE, &
                           RECORD_IS_ACTIVE, & !  C 1
                           FORECAST_SOURCE, & !  C 20
                           MARKET_AREA_COMPANY_NAME, & !  C 32
                           MARKET_AREA_COMPANY_ID, & !  C 5
                           REFERENCE_LOAD_NAME, & !  C 5
                           REFERENCE_LOAD_NUMBER, & !  I 2
                           DATE_OF_FORECAST_ACTUAL_DATA, & !  I 4
                           DAY_OF_WEEK, & !  C 9
                           HOUR_OF_DAY, & !  24x R 4
                           COMMENT, & !  C 50
                           TRANSACTION_GROUP
            ENDIF
!
!
            IF(TEMP_TRANS_CLASS_POINTER(TRANSACTION_GROUP) == 0) THEN
               NUM_OF_OL_TRANS_CLASSES = NUM_OF_OL_TRANS_CLASSES + 1
               MAX_OL_TRANS_CLASS_ID_NUM = MAX( &
                            MAX_OL_TRANS_CLASS_ID_NUM,TRANSACTION_GROUP)
               TEMP_TRANS_CLASS_POINTER(TRANSACTION_GROUP) = 1
            ENDIF
           IF(TEMP_ASSET_CLASS_POINTER(int(ASSET_CLASS_ID,2)) == 0) THEN
               NUM_OF_OL_ASSET_CLASSES = NUM_OF_OL_ASSET_CLASSES + 1
               MAX_OL_ASSET_CLASS_ID_NUM = MAX( &
                        MAX_OL_ASSET_CLASS_ID_NUM,int(ASSET_CLASS_ID,2))
               TEMP_ASSET_CLASS_POINTER(int(ASSET_CLASS_ID,2)) = 1
           ENDIF
            IF(TEMP_CUST_CLASS_POINTER(CUSTOMER_GROUP) == 0) THEN
               NUM_OF_OL_CUST_CLASSES = NUM_OF_OL_CUST_CLASSES + 1
               MAX_OL_CUST_CLASS_ID_NUM = MAX( &
                                MAX_OL_CUST_CLASS_ID_NUM,CUSTOMER_GROUP)
               TEMP_CUST_CLASS_POINTER(CUSTOMER_GROUP) = 1
            ENDIF
!
            WRITE(12,REC=IREC) DELETE, &
                               RECORD_IS_ACTIVE, & !  C 1
                               FORECAST_SOURCE, & !  C 20
                               MARKET_AREA_COMPANY_NAME, & !  C 32
                               MARKET_AREA_COMPANY_ID, & !  C 5
                               REFERENCE_LOAD_NAME, & !  C 5
                               REFERENCE_LOAD_NUMBER, & !  I 2
                               DATE_OF_FORECAST_ACTUAL_DATA, & !  I 4
                               DAY_OF_WEEK, & !  C 9
                               HOUR_OF_DAY, & !  24x R 4
                               COMMENT, & !  C 50
                               TRANSACTION_GROUP
         ENDDO ! RECORDS IN TABLES
         IF(IOS_BASE /= 0) EXIT
      ENDDO ! END TABLES
      CLOSE(10)
      CLOSE(12)
      IF(WEATHER_DEMAND_OL == 'BC') CLOSE(11)
      WEATHER_DEMAND_OL = 'OL'
      IF(ALLOCATED(TEMP_TRANS_CLASS_POINTER)) &
            DEALLOCATE(    TEMP_TRANS_CLASS_POINTER, &
                           TEMP_ASSET_CLASS_POINTER, &
                           TEMP_CUST_CLASS_POINTER)
      RETURN
!
!

      ENTRY RESET_WEATHER_DEMAND_OL

         WEATHER_DEMAND_OL = 'BC'
         SAVE_WD_FILE_USED = .FALSE.
      RETURN
!

      ENTRY RETURN_WEATHER_DEMAND_OL(R_WEATHER_DEMAND_OL,R_LRECL)

         R_WEATHER_DEMAND_OL = WEATHER_DEMAND_OL
         R_LRECL = LRECL
      RETURN

      ENTRY DOES_WEATHER_DEMAND_FILE_EXIST(R_WD_FILE_EXISTS)

         R_WD_FILE_EXISTS = SAVE_WD_FILE_EXISTS
      RETURN

      ENTRY GET_WEATHER_DEMAND_RECORDS(R_TRANS_LOAD_RECORDS)

         R_TRANS_LOAD_RECORDS = SAVE_TRANS_LOAD_RECORDS
      RETURN

      ENTRY WD_FILE_USED_THIS_ENDPOINT(R_WD_FILE_USED)

         R_WD_FILE_USED = SAVE_WD_FILE_USED
         SAVE_WD_FILE_USED = .TRUE.
      RETURN

      ENTRY OPEN_WEATHER_DEMAND_FILE

         OPEN(UNIT_NUM, &
              FILE=trim(OUTPUT_DIRECTORY())//WEATHER_DEMAND_OL// &
              "WDYFC.BIN",ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      RETURN
!

      ENTRY CLOSE_WEATHER_DEMAND_FILE

         CLOSE(UNIT_NUM)
      RETURN

      ENTRY RETURN_WD_GROUP_INFO(R_NUM_OF_TRANS_CLASSES, &
                                 R_MAX_TRANS_CLASS_NUM, &
                                 R_NUM_OF_ASSET_CLASSES, &
                                 R_MAX_ASSET_CLASS_NUM, &
                                 R_NUM_OF_CUST_CLASSES, &
                                 R_MAX_CUST_CLASS_NUM)

         IF(WEATHER_DEMAND_OL == 'OL') THEN
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
      FUNCTION MANAGE_WEATHER_DEMAND_FORECASTS()
      use end_routine, only: end_program, er_message
!

!
!
! LOCAL DATA LIST
!
      USE SIZECOM
      use SpinDriftLib
      USE PROD_ARRAYS_DIMENSIONS
      use globecom
      use mod_base_year
      SAVE

      LOGICAL (KIND=1) :: SAVE_WEATHER_DEMAND_STATUS=.FALSE.
      LOGICAL (KIND=1) :: MANAGE_WEATHER_DEMAND_FORECASTS,GET_WD_LOAD
      LOGICAL (KIND=1) :: DOES_WD_FILE_EXIST
      INTEGER (KIND=2) :: YR,MAKER_TABLES=0,DELETE,R_MONTH,MAKER_MONTHS
      INTEGER (KIND=2) :: R_YEAR,R_LOAD_UNIT,WK,LOCAL_WEEKS,LOCAL_YEARS
      INTEGER (KIND=2) :: NUM_SCEN_VAR,IREC
!
!
      CHARACTER (LEN=1) :: RECORD_IS_ACTIVE ! C 1
      CHARACTER (LEN=20) :: FORECAST_SOURCE ! C 20
      CHARACTER (LEN=32) :: MARKET_AREA_COMPANY_NAME ! C 32
      CHARACTER (LEN=5) :: MARKET_AREA_COMPANY_ID,REFERENCE_LOAD_NAME ! C 5
      INTEGER (KIND=2) :: J,REFERENCE_LOAD_NUMBER ! I 2
      INTEGER (KIND=2) :: TRANSACTION_GROUP,WEATHER_DEMAND_RECORDS
      INTEGER (KIND=2) :: I_HOUR,LOCAL_MONTH,LOCAL_DAY,LOCAL_YEAR
      INTEGER (KIND=2) :: MARKET_AREA_COMPANY_ID_NUM
      INTEGER (KIND=2) :: MARKET_AREA_LOOKUP,R_ID_NUM,R_DAY
      INTEGER (KIND=4) :: DATE_OF_FORECAST_ACTUAL_DATA,R_DAILY_I4(24)
      CHARACTER (LEN=9) :: DAY_OF_WEEK ! C 9
      REAL (KIND=4) :: HOUR_OF_DAY(24),WD_LOAD(:,:) ! RECORDS X 24 x R 4
      ALLOCATABLE :: WD_LOAD
      CHARACTER (LEN=50) :: COMMENT ! C 50
!
!
! INPUT DATA LIST
!
      INTEGER (KIND=4) :: VALUES_2_ZERO

      INTEGER (KIND=2) :: SCENARIO_YEAR,TABLE,SCENARIO_INDEX
      INTEGER (KIND=2) :: LOCAL_MAX_NUM_MARKET_AREAS=0
      INTEGER (KIND=2) :: LOCAL_MAX_HOURS=24,LOCAL_MAX_DAYS=31
      INTEGER (KIND=2) :: LOCAL_MAX_MONTHS=12,LOCAL_MAX_YEARS=12
      ! UP'ED FOR DOUG 051310 ! UP'ED FOR SCOTT 02/26/08 ! UP'ED FOR SCOTT 01/08/04 
      INTEGER (KIND=2) :: MIN_YEAR=9999,MAX_YEAR=0
      REAL (KIND=4) :: R_MONTHLY_SLOPE,R_MONTHLY_INTERCEPT
      CHARACTER (LEN=20) :: HYDRO_VARIABLES
      INTEGER (KIND=2) :: WD_INDEX(:,:,:,:) ! MARKET_ID, LOCAL_MONTH, LOCAL_DAY, LOCAL_YEAR
      ALLOCATABLE :: WD_INDEX
!
!      SAVE WD_LOAD,WD_INDEX
!
! END DATA DECLARATIONS
!

!
         CALL DOES_WEATHER_DEMAND_FILE_EXIST(DOES_WD_FILE_EXIST)
         IF(DOES_WD_FILE_EXIST) THEN
            CALL OPEN_WEATHER_DEMAND_FILE
!
            CALL GET_WEATHER_DEMAND_RECORDS(WEATHER_DEMAND_RECORDS)
!
            LOCAL_WEEKS = 52
            NUM_SCEN_VAR = 3

            IF( ALLOCATED(WD_LOAD) ) then
                      DEALLOCATE(WD_LOAD)
            end if
            ALLOCATE( &
                  WD_LOAD(LOCAL_MAX_HOURS,WEATHER_DEMAND_RECORDS))
! 031108
            WD_LOAD = -999.0 ! 0.

!
!
!     DETERMINE LOCAL_MAX_NUM_MARKET_AREAS, MIN_YEAR, MAX_YEAR
!
            DO IREC = 1, WEATHER_DEMAND_RECORDS
               READ(10,REC=IREC) DELETE, &
                           RECORD_IS_ACTIVE, & !  C 1
                           FORECAST_SOURCE, & !  C 20
                           MARKET_AREA_COMPANY_NAME, & !  C 32
                           MARKET_AREA_COMPANY_ID, & !  C 5
                           REFERENCE_LOAD_NAME, & !  C 5
                           REFERENCE_LOAD_NUMBER, & !  I 2
                           DATE_OF_FORECAST_ACTUAL_DATA, & !  I 4
                           DAY_OF_WEEK, & !  C 9
                           HOUR_OF_DAY, & !  24x R 4
                           COMMENT, & !  C 50
                           TRANSACTION_GROUP

               IF(RECORD_IS_ACTIVE == 'F') CYCLE
!
!
               LOCAL_MONTH = DATE_OF_FORECAST_ACTUAL_DATA/10000
               LOCAL_DAY = (DATE_OF_FORECAST_ACTUAL_DATA - &
                           LOCAL_MONTH*10000) / 100
!
               LOCAL_YEAR = DATE_OF_FORECAST_ACTUAL_DATA - &
                      LOCAL_MONTH * 10000 - LOCAL_DAY * 100
!
!     CALENDAR CORRECT YEAR
!
               IF(LOCAL_YEAR <= 50) THEN
                     LOCAL_YEAR = LOCAL_YEAR + 2000 - get_base_year()
                  ELSE
                     LOCAL_YEAR = LOCAL_YEAR + 1900 - get_base_year()
               ENDIF

               IF(LOCAL_YEAR > MAX_YEAR) MAX_YEAR = LOCAL_YEAR
               IF(LOCAL_YEAR < MIN_YEAR) MIN_YEAR = LOCAL_YEAR

               MARKET_AREA_COMPANY_ID_NUM = &
                              MARKET_AREA_LOOKUP(MARKET_AREA_COMPANY_ID)
               LOCAL_MAX_NUM_MARKET_AREAS = &
                           MAX(LOCAL_MAX_NUM_MARKET_AREAS, &
                                    MARKET_AREA_COMPANY_ID_NUM+int(1,2))
!
            ENDDO ! RECORDS
!
            IF( ALLOCATED(WD_INDEX) ) &
                      DEALLOCATE(WD_INDEX)
            IF(MAX_YEAR <= 0 .OR. LOCAL_MAX_NUM_MARKET_AREAS <= 0) THEN
            ENDIF
            ALLOCATE( &
                  WD_INDEX(0:LOCAL_MAX_NUM_MARKET_AREAS-1, &
                     LOCAL_MAX_MONTHS, &
                     LOCAL_MAX_DAYS, &
                     LOCAL_MAX_YEARS))


            WD_INDEX = 0

            J = 0
            DO IREC = 1, WEATHER_DEMAND_RECORDS
               READ(10,REC=IREC) DELETE, &
                           RECORD_IS_ACTIVE, & !  C 1
                           FORECAST_SOURCE, & !  C 20
                           MARKET_AREA_COMPANY_NAME, & !  C 32
                           MARKET_AREA_COMPANY_ID, & !  C 5
                           REFERENCE_LOAD_NAME, & !  C 5
                           REFERENCE_LOAD_NUMBER, & !  I 2
                           DATE_OF_FORECAST_ACTUAL_DATA, & !  I 4
                           DAY_OF_WEEK, & !  C 9
                           HOUR_OF_DAY, & !  24x R 4
                           COMMENT, & !  C 50
                           TRANSACTION_GROUP
!
               IF(RECORD_IS_ACTIVE == 'F' .OR. &
                                          RECORD_IS_ACTIVE == 'N') CYCLE
               J = J + 1
!
               DO I_HOUR = 1, 24
                  WD_LOAD(I_HOUR,J) = HOUR_OF_DAY(I_HOUR)
               ENDDO
!
               LOCAL_MONTH = DATE_OF_FORECAST_ACTUAL_DATA/10000
               LOCAL_DAY = (DATE_OF_FORECAST_ACTUAL_DATA - &
                            LOCAL_MONTH*10000) / 100
               LOCAL_YEAR = DATE_OF_FORECAST_ACTUAL_DATA - &
                      LOCAL_MONTH * 10000 - LOCAL_DAY * 100
!
!     CALENDAR CORRECT YEAR
!
               IF(LOCAL_YEAR <= 50) THEN
                     LOCAL_YEAR = LOCAL_YEAR + 2000 - get_base_year()
                  ELSE
                     LOCAL_YEAR = LOCAL_YEAR + 1900 - get_base_year()
               ENDIF

               MARKET_AREA_COMPANY_ID_NUM = &
                  MARKET_AREA_LOOKUP(MARKET_AREA_COMPANY_ID)
               IF(MARKET_AREA_COMPANY_ID_NUM < 0 .OR. &
                        MARKET_AREA_COMPANY_ID_NUM > &
                                 LOCAL_MAX_NUM_MARKET_AREAS-1) THEN
                  WRITE(4,*) 'BAD MARKET AREA ID IN THE SHORT-TERM'
                  WRITE(4,*) 'FORECAST MODEL',MARKET_AREA_COMPANY_ID_NUM
                  WRITE(4,*) 'MARKET AREA',MARKET_AREA_COMPANY_NAME
                  er_message='Stop requested from wh_objt SIID354'
                  call end_program(er_message)
               ENDIF
               IF(LOCAL_DAY > LOCAL_MAX_DAYS .OR. LOCAL_DAY < 1) THEN
                  WRITE(4,*) 'BAD DAY INDEX IN THE SHORT-TERM'
                  WRITE(4,*) 'FORECAST MODEL',LOCAL_DAY
                  WRITE(4,*) 'MARKET AREA',MARKET_AREA_COMPANY_NAME
                  er_message='Stop requested from wh_objt SIID355'
                  call end_program(er_message)
               ENDIF
               IF(LOCAL_MONTH > LOCAL_MAX_MONTHS .OR. &
                                                   LOCAL_MONTH < 1) THEN
                  WRITE(4,*) 'BAD MONTH INDEX IN THE SHORT-TERM'
                  WRITE(4,*) 'FORECAST MODEL',LOCAL_MONTH
                  WRITE(4,*) 'MARKET AREA',MARKET_AREA_COMPANY_NAME
                  er_message='Stop requested from wh_objt SIID356'
                  call end_program(er_message)
               ENDIF
               IF(LOCAL_YEAR > LOCAL_MAX_YEARS .OR. LOCAL_YEAR < 1) THEN
                  WRITE(4,*) 'BAD YEAR INDEX IN THE SHORT-TERM'
                  WRITE(4,*) 'FORECAST MODEL',LOCAL_YEAR
                  WRITE(4,*) 'MARKET AREA',MARKET_AREA_COMPANY_NAME
                  er_message='Stop requested from wh_objt SIID357'
                  call end_program(er_message)
               ENDIF
               WD_INDEX(MARKET_AREA_COMPANY_ID_NUM,LOCAL_MONTH, &
                        LOCAL_DAY,LOCAL_YEAR) = J

!
            ENDDO ! RECORDS
            CALL CLOSE_WEATHER_DEMAND_FILE
            MANAGE_WEATHER_DEMAND_FORECASTS = .TRUE.
         ENDIF
!
      RETURN
!

!
      ENTRY GET_WD_LOAD(R_ID_NUM,R_YEAR,R_MONTH,R_DAY,R_DAILY_I4, &
                         R_MONTHLY_SLOPE,R_MONTHLY_INTERCEPT)
!


         GET_WD_LOAD = .FALSE.
         IF( (R_YEAR >= MIN_YEAR .AND. R_YEAR <= MAX_YEAR) .AND. &
            R_MONTH <= LOCAL_MAX_MONTHS .AND. &
            R_DAY <= LOCAL_MAX_DAYS .AND. &
            R_ID_NUM <= LOCAL_MAX_NUM_MARKET_AREAS-1) THEN


            J = WD_INDEX(R_ID_NUM,R_MONTH,R_DAY,R_YEAR)

            IF(J > 0 .AND. WD_LOAD(1,J) > -998.0) THEN
                  R_DAILY_I4(1) = INT(WD_LOAD(1,J))
                  R_DAILY_I4(2) = INT(WD_LOAD(2,J))
                  R_DAILY_I4(3) = INT(WD_LOAD(3,J))
                  R_DAILY_I4(4) = INT(WD_LOAD(4,J))
                  R_DAILY_I4(5) = INT(WD_LOAD(5,J))
                  R_DAILY_I4(6) = INT(WD_LOAD(6,J))
                  R_DAILY_I4(7) = INT(WD_LOAD(7,J))
                  R_DAILY_I4(8) = INT(WD_LOAD(8,J))
                  R_DAILY_I4(9) = INT(WD_LOAD(9,J))
                  R_DAILY_I4(10) = INT(WD_LOAD(10,J))
                  R_DAILY_I4(11) = INT(WD_LOAD(11,J))
                  R_DAILY_I4(12) = INT(WD_LOAD(12,J))
                  R_DAILY_I4(13) = INT(WD_LOAD(13,J))
                  R_DAILY_I4(14) = INT(WD_LOAD(14,J))
                  R_DAILY_I4(15) = INT(WD_LOAD(15,J))
                  R_DAILY_I4(16) = INT(WD_LOAD(16,J))
                  R_DAILY_I4(17) = INT(WD_LOAD(17,J))
                  R_DAILY_I4(18) = INT(WD_LOAD(18,J))
                  R_DAILY_I4(19) = INT(WD_LOAD(19,J))
                  R_DAILY_I4(20) = INT(WD_LOAD(20,J))
                  R_DAILY_I4(21) = INT(WD_LOAD(21,J))
                  R_DAILY_I4(22) = INT(WD_LOAD(22,J))
                  R_DAILY_I4(23) = INT(WD_LOAD(23,J))
                  R_DAILY_I4(24) = INT(WD_LOAD(24,J))
                  R_MONTHLY_SLOPE = 1.0
                  R_MONTHLY_INTERCEPT = 0.0
                  GET_WD_LOAD = .TRUE.
            ELSE
                  GET_WD_LOAD = .FALSE.
            ENDIF
         ENDIF
      RETURN
!
      END

!
!     TODO: convert to dictionary lookup.
      FUNCTION STATE_NAME_LOOKUP(R_STATE_NAME_NUM)
      use end_routine
      implicit none

      CHARACTER (LEN=20) :: STATE_NAME_LOOKUP
      INTEGER (KIND=2) :: R_STATE_NAME_NUM
         SELECT CASE(R_STATE_NAME_NUM)
            CASE (1)
               STATE_NAME_LOOKUP = 'Alabama'
            CASE (2)
               STATE_NAME_LOOKUP = 'Alaska'
            CASE (3)
               STATE_NAME_LOOKUP = 'Arizona'
            CASE (4)
               STATE_NAME_LOOKUP = 'Arkansas'
            CASE (5)
               STATE_NAME_LOOKUP = 'California'
            CASE (6)
               STATE_NAME_LOOKUP = 'Colorado'
            CASE (7)
               STATE_NAME_LOOKUP = 'Connecticut'
            CASE (8)
               STATE_NAME_LOOKUP = 'Delaware'
            CASE (9)
               STATE_NAME_LOOKUP = 'District of Columbia'
            CASE (10)
               STATE_NAME_LOOKUP = 'Florida'
            CASE (11)
               STATE_NAME_LOOKUP = 'Georgia'
            CASE (12)
               STATE_NAME_LOOKUP = 'Hawaii'
            CASE (13)
               STATE_NAME_LOOKUP = 'Idaho'
            CASE (14)
               STATE_NAME_LOOKUP = 'Illinois'
            CASE (15)
               STATE_NAME_LOOKUP = 'Indiana'
            CASE (16)
               STATE_NAME_LOOKUP = 'Iowa'
            CASE (17)
               STATE_NAME_LOOKUP = 'Kansas'
            CASE (18)
               STATE_NAME_LOOKUP = 'Kentucky'
            CASE (19)
               STATE_NAME_LOOKUP = 'Louisiana'
            CASE (20)
               STATE_NAME_LOOKUP = 'Maine'
            CASE (21)
               STATE_NAME_LOOKUP = 'Maryland'
            CASE (22)
               STATE_NAME_LOOKUP = 'Massachusetts'
            CASE (23)
               STATE_NAME_LOOKUP = 'Michigan'
            CASE (24)
               STATE_NAME_LOOKUP = 'Minnesota'
            CASE (25)
               STATE_NAME_LOOKUP = 'Mississippi'
            CASE (26)
               STATE_NAME_LOOKUP = 'Missouri'
            CASE (27)
               STATE_NAME_LOOKUP = 'Montana'
            CASE (28)
               STATE_NAME_LOOKUP = 'Nebraska'
            CASE (29)
               STATE_NAME_LOOKUP = 'Nevada'
            CASE (30)
               STATE_NAME_LOOKUP = 'New Hampshire'
            CASE (31)
               STATE_NAME_LOOKUP = 'New Jersey'
            CASE (32)
               STATE_NAME_LOOKUP = 'New Mexico'
            CASE (33)
               STATE_NAME_LOOKUP = 'New York'
            CASE (34)
               STATE_NAME_LOOKUP = 'North Carolina'
            CASE (35)
               STATE_NAME_LOOKUP = 'North Dakota'
            CASE (36)
               STATE_NAME_LOOKUP = 'Ohio'
            CASE (37)
               STATE_NAME_LOOKUP = 'Oklahoma'
            CASE (38)
               STATE_NAME_LOOKUP = 'Oregon'
            CASE (39)
               STATE_NAME_LOOKUP = 'Pennsylvania'
            CASE (40)
               STATE_NAME_LOOKUP = 'Rhode Island'
            CASE (41)
               STATE_NAME_LOOKUP = 'South Carolina'
            CASE (42)
               STATE_NAME_LOOKUP = 'South Dakota'
            CASE (43)
               STATE_NAME_LOOKUP = 'Tennessee'
            CASE (44)
               STATE_NAME_LOOKUP = 'Texas'
            CASE (45)
               STATE_NAME_LOOKUP = 'Utah'
            CASE (46)
               STATE_NAME_LOOKUP = 'Vermont'
            CASE (47)
               STATE_NAME_LOOKUP = 'Virginia'
            CASE (48)
               STATE_NAME_LOOKUP = 'Washington'
            CASE (49)
               STATE_NAME_LOOKUP = 'West Virginia'
            CASE (50)
               STATE_NAME_LOOKUP = 'Wisconsin'
            CASE (51)
               STATE_NAME_LOOKUP = 'Wyoming'
            CASE (52)
               STATE_NAME_LOOKUP = 'Alberta'
            CASE (53)
               STATE_NAME_LOOKUP = 'British Columbia'
            CASE (54)
               STATE_NAME_LOOKUP = 'Manitoba'
            CASE (55)
               STATE_NAME_LOOKUP = 'New Brunswick'
            CASE (56)
               STATE_NAME_LOOKUP = 'Newfoundland'
            CASE (57)
               STATE_NAME_LOOKUP = 'Nova Scotia'
            CASE (58)
               STATE_NAME_LOOKUP = 'Ontario'
            CASE (59)
               STATE_NAME_LOOKUP = 'Prince Edward'
            CASE (60)
               STATE_NAME_LOOKUP = 'Quebec'
            CASE (61)
               STATE_NAME_LOOKUP = 'Saskatchewan'
          END SELECT
       RETURN
       END

      ! TODO: Make state_id_lookups data-driven
      FUNCTION STATE_ID_LOOKUP(STATE_ID_NUM)
      use end_routine
      implicit none
      INTEGER (KIND=2) :: STATE_ID_LOOKUP,CHECK_NEW_STATE_IDS
      CHARACTER (LEN=6) :: STATE_ID_NUM

         ! TODO: Make lookup
         ! TODO: Make this data-driven
         if(trim(STATE_ID_NUM)=="") then
            STATE_ID_NUM="" ! Debugstop
         end if
         SELECT CASE(TRIM(STATE_ID_NUM))
            CASE ('AL')
               STATE_ID_LOOKUP = 1
            CASE ('AK')
               STATE_ID_LOOKUP = 2
            CASE ('AZ')
               STATE_ID_LOOKUP = 3
            CASE ('AR')
               STATE_ID_LOOKUP = 4
            CASE ('CA')
               STATE_ID_LOOKUP = 5
            CASE ('CO')
               STATE_ID_LOOKUP = 6
            CASE ('CT')
               STATE_ID_LOOKUP = 7
            CASE ('DE')
               STATE_ID_LOOKUP = 8
            CASE ('DC')
               STATE_ID_LOOKUP = 9
            CASE ('FL')
               STATE_ID_LOOKUP = 10
            CASE ('GA')
               STATE_ID_LOOKUP = 11
            CASE ('HI')
               STATE_ID_LOOKUP = 12
            CASE ('ID')
               STATE_ID_LOOKUP = 13
            CASE ('IL')
               STATE_ID_LOOKUP = 14
            CASE ('IN')
               STATE_ID_LOOKUP = 15
            CASE ('IA')
               STATE_ID_LOOKUP = 16
            CASE ('KS')
               STATE_ID_LOOKUP = 17
            CASE ('KY')
               STATE_ID_LOOKUP = 18
            CASE ('LA')
               STATE_ID_LOOKUP = 19
            CASE ('ME')
               STATE_ID_LOOKUP = 20
            CASE ('MD')
               STATE_ID_LOOKUP = 21
            CASE ('MA')
               STATE_ID_LOOKUP = 22
            CASE ('MI')
               STATE_ID_LOOKUP = 23
            CASE ('MN')
               STATE_ID_LOOKUP = 24
            CASE ('MS')
               STATE_ID_LOOKUP = 25
            CASE ('MO')
               STATE_ID_LOOKUP = 26
            CASE ('MT')
               STATE_ID_LOOKUP = 27
            CASE ('NE')
               STATE_ID_LOOKUP = 28
            CASE ('NV')
               STATE_ID_LOOKUP = 29
            CASE ('NH')
               STATE_ID_LOOKUP = 30
            CASE ('NJ')
               STATE_ID_LOOKUP = 31
            CASE ('NM')
               STATE_ID_LOOKUP = 32
            CASE ('NY')
               STATE_ID_LOOKUP = 33
            CASE ('NC')
               STATE_ID_LOOKUP = 34
            CASE ('ND')
               STATE_ID_LOOKUP = 35
            CASE ('OH')
               STATE_ID_LOOKUP = 36
            CASE ('OK')
               STATE_ID_LOOKUP = 37
            CASE ('OR')
               STATE_ID_LOOKUP = 38
            CASE ('PA')
               STATE_ID_LOOKUP = 39
            CASE ('RI')
               STATE_ID_LOOKUP = 40
            CASE ('SC')
               STATE_ID_LOOKUP = 41
            CASE ('SD')
               STATE_ID_LOOKUP = 42
            CASE ('TN')
               STATE_ID_LOOKUP = 43
            CASE ('TX')
               STATE_ID_LOOKUP = 44
            CASE ('UT')
               STATE_ID_LOOKUP = 45
            CASE ('VT')
               STATE_ID_LOOKUP = 46
            CASE ('VA')
               STATE_ID_LOOKUP = 47
            CASE ('WA')
               STATE_ID_LOOKUP = 48
            CASE ('WV')
               STATE_ID_LOOKUP = 49
            CASE ('WI')
               STATE_ID_LOOKUP = 50
            CASE ('WY')
               STATE_ID_LOOKUP = 51
            CASE ('AB')
               STATE_ID_LOOKUP = 52
            CASE ('BC')
               STATE_ID_LOOKUP = 53
            CASE ('MB')
               STATE_ID_LOOKUP = 54
            CASE ('NB')
               STATE_ID_LOOKUP = 55
            CASE ('NL')
               STATE_ID_LOOKUP = 56
            CASE ('NS')
               STATE_ID_LOOKUP = 57
            CASE ('ON')
               STATE_ID_LOOKUP = 58
            CASE ('PE')
               STATE_ID_LOOKUP = 59
            CASE ('QC')
               STATE_ID_LOOKUP = 60
            CASE ('SK')
               STATE_ID_LOOKUP = 61
            CASE ('BCN')
               STATE_ID_LOOKUP = 62
            CASE ('BCS')
               STATE_ID_LOOKUP = 63
            CASE ('CAM')
               STATE_ID_LOOKUP = 64
            CASE ('CHS')
               STATE_ID_LOOKUP = 65
            CASE ('CHI')
               STATE_ID_LOOKUP = 66
            CASE ('COA')
               STATE_ID_LOOKUP = 67
            CASE ('COL')
               STATE_ID_LOOKUP = 68
            CASE ('DGO')
               STATE_ID_LOOKUP = 69
            CASE ('GTO')
               STATE_ID_LOOKUP = 70
            CASE ('GRO')
               STATE_ID_LOOKUP = 71
            CASE ('HGO')
               STATE_ID_LOOKUP = 72
            CASE ('JAL')
               STATE_ID_LOOKUP = 73
            CASE ('MEX')
               STATE_ID_LOOKUP = 74
            CASE ('MIC')
               STATE_ID_LOOKUP = 75
            CASE ('NAY')
               STATE_ID_LOOKUP = 76
            CASE ('NLN')
               STATE_ID_LOOKUP = 77
            CASE ('OAX')
               STATE_ID_LOOKUP = 78
            CASE ('PUE')
               STATE_ID_LOOKUP = 79
            CASE ('QRO')
               STATE_ID_LOOKUP = 80
            CASE ('QTR')
               STATE_ID_LOOKUP = 81
            CASE ('SLP')
               STATE_ID_LOOKUP = 82
            CASE ('SIN')
               STATE_ID_LOOKUP = 83
            CASE ('AB_52')
               STATE_ID_LOOKUP = 84
            CASE ('AB_53')
               STATE_ID_LOOKUP = 85
            CASE ('AL_45')
               STATE_ID_LOOKUP = 86
            CASE ('AL_46')
               STATE_ID_LOOKUP = 87
            CASE ('AR_43')
               STATE_ID_LOOKUP = 88
            CASE ('AR_50')
               STATE_ID_LOOKUP = 89
            CASE ('AZ_54')
               STATE_ID_LOOKUP = 90
            CASE ('AZ_65')
               STATE_ID_LOOKUP = 91
            CASE ('AZ_72')
               STATE_ID_LOOKUP = 92
            CASE ('AZ_73')
               STATE_ID_LOOKUP = 93
            CASE ('AZ_79')
               STATE_ID_LOOKUP = 94
            CASE ('BC_52')
               STATE_ID_LOOKUP = 95
            CASE ('BC_55')
               STATE_ID_LOOKUP = 96
            CASE ('BCN_68')
               STATE_ID_LOOKUP = 97
            CASE ('BCN_79')
               STATE_ID_LOOKUP = 98
            CASE ('CA_56')
               STATE_ID_LOOKUP = 99
            CASE ('CA_57')
               STATE_ID_LOOKUP = 100
            CASE ('CA_58')
               STATE_ID_LOOKUP = 101
            CASE ('CA_59')
               STATE_ID_LOOKUP = 102
            CASE ('CA_65')
               STATE_ID_LOOKUP = 103
            CASE ('CA_66')
               STATE_ID_LOOKUP = 104
            CASE ('CA_70')
               STATE_ID_LOOKUP = 105
            CASE ('CA_73')
               STATE_ID_LOOKUP = 106
            CASE ('CHI_68')
               STATE_ID_LOOKUP = 107
            CASE ('CO_61')
               STATE_ID_LOOKUP = 108
            CASE ('CO_62')
               STATE_ID_LOOKUP = 109
            CASE ('CT_86')
               STATE_ID_LOOKUP = 110
            CASE ('CT_87')
               STATE_ID_LOOKUP = 111
            CASE ('CT_88')
               STATE_ID_LOOKUP = 112
            CASE ('DC_16')
               STATE_ID_LOOKUP = 113
            CASE ('DE_15')
               STATE_ID_LOOKUP = 114
            CASE ('FL_14')
               STATE_ID_LOOKUP = 115
            CASE ('FL_45')
               STATE_ID_LOOKUP = 116
            CASE ('GA_45')
               STATE_ID_LOOKUP = 117
            CASE ('IA_17')
               STATE_ID_LOOKUP = 118
            CASE ('IA_21')
               STATE_ID_LOOKUP = 119
            CASE ('IA_23')
               STATE_ID_LOOKUP = 120
            CASE ('IA_25')
               STATE_ID_LOOKUP = 121
            CASE ('ID_63')
               STATE_ID_LOOKUP = 122
            CASE ('ID_64')
               STATE_ID_LOOKUP = 123
            CASE ('ID_71')
               STATE_ID_LOOKUP = 124
            CASE ('IL_18')
               STATE_ID_LOOKUP = 125
            CASE ('IL_19')
               STATE_ID_LOOKUP = 126
            CASE ('IL_21')
               STATE_ID_LOOKUP = 127
            CASE ('IL_46')
               STATE_ID_LOOKUP = 128
            CASE ('IN_02')
               STATE_ID_LOOKUP = 129
            CASE ('IN_03')
               STATE_ID_LOOKUP = 130
            CASE ('IN_17')
               STATE_ID_LOOKUP = 131
            CASE ('IN_18')
               STATE_ID_LOOKUP = 132
            CASE ('KS_51')
               STATE_ID_LOOKUP = 133
            CASE ('KY_02')
               STATE_ID_LOOKUP = 134
            CASE ('KY_03')
               STATE_ID_LOOKUP = 135
            CASE ('KY_05')
               STATE_ID_LOOKUP = 136
            CASE ('KY_46')
               STATE_ID_LOOKUP = 137
            CASE ('LA_43')
               STATE_ID_LOOKUP = 138
            CASE ('LA_49')
               STATE_ID_LOOKUP = 139
            CASE ('LA_50')
               STATE_ID_LOOKUP = 140
            CASE ('MA_86')
               STATE_ID_LOOKUP = 141
            CASE ('MA_87')
               STATE_ID_LOOKUP = 142
            CASE ('MA_89')
               STATE_ID_LOOKUP = 143
            CASE ('MB_22')
               STATE_ID_LOOKUP = 144
            CASE ('MD_01')
               STATE_ID_LOOKUP = 145
            CASE ('MD_15')
               STATE_ID_LOOKUP = 146
            CASE ('MD_16')
               STATE_ID_LOOKUP = 147
            CASE ('ME_27')
               STATE_ID_LOOKUP = 148
            CASE ('ME_85')
               STATE_ID_LOOKUP = 149
            CASE ('MI_02')
               STATE_ID_LOOKUP = 150
            CASE ('MI_06')
               STATE_ID_LOOKUP = 151
            CASE ('MI_20')
               STATE_ID_LOOKUP = 152
            CASE ('MN_17')
               STATE_ID_LOOKUP = 153
            CASE ('MN_23')
               STATE_ID_LOOKUP = 154
            CASE ('MN_25')
               STATE_ID_LOOKUP = 155
            CASE ('MO_19')
               STATE_ID_LOOKUP = 156
            CASE ('MO_42')
               STATE_ID_LOOKUP = 157
            CASE ('MO_50')
               STATE_ID_LOOKUP = 158
            CASE ('MO_51')
               STATE_ID_LOOKUP = 159
            CASE ('MS_43')
               STATE_ID_LOOKUP = 160
            CASE ('MS_45')
               STATE_ID_LOOKUP = 161
            CASE ('MS_46')
               STATE_ID_LOOKUP = 162
            CASE ('MT_25')
               STATE_ID_LOOKUP = 163
            CASE ('MT_67')
               STATE_ID_LOOKUP = 164
            CASE ('NB_27')
               STATE_ID_LOOKUP = 165
            CASE ('NC_44')
               STATE_ID_LOOKUP = 166
            CASE ('NC_47')
               STATE_ID_LOOKUP = 167
            CASE ('ND_23')
               STATE_ID_LOOKUP = 168
            CASE ('ND_25')
               STATE_ID_LOOKUP = 169
            CASE ('NE_24')
               STATE_ID_LOOKUP = 170
            CASE ('NE_75')
               STATE_ID_LOOKUP = 171
            CASE ('NH_87')
               STATE_ID_LOOKUP = 172
            CASE ('NJ_15')
               STATE_ID_LOOKUP = 173
            CASE ('NJ_38')
               STATE_ID_LOOKUP = 174
            CASE ('NM_50')
               STATE_ID_LOOKUP = 175
            CASE ('NM_61')
               STATE_ID_LOOKUP = 176
            CASE ('NM_69')
               STATE_ID_LOOKUP = 177
            CASE ('NS_27')
               STATE_ID_LOOKUP = 178
            CASE ('NV_57')
               STATE_ID_LOOKUP = 179
            CASE ('NV_70')
               STATE_ID_LOOKUP = 180
            CASE ('NV_73')
               STATE_ID_LOOKUP = 181
            CASE ('NY_31')
               STATE_ID_LOOKUP = 182
            CASE ('NY_32')
               STATE_ID_LOOKUP = 183
            CASE ('NY_33')
               STATE_ID_LOOKUP = 184
            CASE ('NY_34')
               STATE_ID_LOOKUP = 185
            CASE ('NY_38')
               STATE_ID_LOOKUP = 186
            CASE ('NY_39')
               STATE_ID_LOOKUP = 187
            CASE ('NY_95')
               STATE_ID_LOOKUP = 188
            CASE ('NY_96')
               STATE_ID_LOOKUP = 189
            CASE ('OH_01')
               STATE_ID_LOOKUP = 190
            CASE ('OH_02')
               STATE_ID_LOOKUP = 191
            CASE ('OH_03')
               STATE_ID_LOOKUP = 192
            CASE ('OH_04')
               STATE_ID_LOOKUP = 193
            CASE ('OK_42')
               STATE_ID_LOOKUP = 194
            CASE ('OK_50')
               STATE_ID_LOOKUP = 195
            CASE ('OK_78')
               STATE_ID_LOOKUP = 196
            CASE ('ON_80')
               STATE_ID_LOOKUP = 197
            CASE ('ON_81')
               STATE_ID_LOOKUP = 198
            CASE ('ON_82')
               STATE_ID_LOOKUP = 199
            CASE ('ON_83')
               STATE_ID_LOOKUP = 200
            CASE ('OR_60')
               STATE_ID_LOOKUP = 201
            CASE ('OR_71')
               STATE_ID_LOOKUP = 202
            CASE ('PA_01')
               STATE_ID_LOOKUP = 203
            CASE ('PA_04')
               STATE_ID_LOOKUP = 204
            CASE ('PA_15')
               STATE_ID_LOOKUP = 205
            CASE ('PA_16')
               STATE_ID_LOOKUP = 206
            CASE ('PA_95')
               STATE_ID_LOOKUP = 207
            CASE ('PE_27')
               STATE_ID_LOOKUP = 208
            CASE ('QC_41')
               STATE_ID_LOOKUP = 209
            CASE ('RI_87')
               STATE_ID_LOOKUP = 210
            CASE ('SC_44')
               STATE_ID_LOOKUP = 211
            CASE ('SD_23')
               STATE_ID_LOOKUP = 212
            CASE ('SD_25')
               STATE_ID_LOOKUP = 213
            CASE ('SD_75')
               STATE_ID_LOOKUP = 214
            CASE ('SK_26')
               STATE_ID_LOOKUP = 215
            CASE ('TN_46')
               STATE_ID_LOOKUP = 216
            CASE ('TX_07')
               STATE_ID_LOOKUP = 217
            CASE ('TX_08')
               STATE_ID_LOOKUP = 218
            CASE ('TX_09')
               STATE_ID_LOOKUP = 219
            CASE ('TX_10')
               STATE_ID_LOOKUP = 220
            CASE ('TX_11')
               STATE_ID_LOOKUP = 221
            CASE ('TX_12')
               STATE_ID_LOOKUP = 222
            CASE ('TX_13')
               STATE_ID_LOOKUP = 223
            CASE ('TX_43')
               STATE_ID_LOOKUP = 224
            CASE ('TX_50')
               STATE_ID_LOOKUP = 225
            CASE ('TX_69')
               STATE_ID_LOOKUP = 226
            CASE ('TX_76')
               STATE_ID_LOOKUP = 227
            CASE ('TX_77')
               STATE_ID_LOOKUP = 228
            CASE ('UT_74')
               STATE_ID_LOOKUP = 229
            CASE ('VA_02')
               STATE_ID_LOOKUP = 230
            CASE ('VA_15')
               STATE_ID_LOOKUP = 231
            CASE ('VA_16')
               STATE_ID_LOOKUP = 232
            CASE ('VA_47')
               STATE_ID_LOOKUP = 233
            CASE ('VT_86')
               STATE_ID_LOOKUP = 234
            CASE ('VT_87')
               STATE_ID_LOOKUP = 235
            CASE ('WA_71')
               STATE_ID_LOOKUP = 236
            CASE ('WI_20')
               STATE_ID_LOOKUP = 237
            CASE ('WI_23')
               STATE_ID_LOOKUP = 238
            CASE ('WV_01')
               STATE_ID_LOOKUP = 239
            CASE ('WV_02')
               STATE_ID_LOOKUP = 240
            CASE ('WV_47')
               STATE_ID_LOOKUP = 241
            CASE ('WY_25')
               STATE_ID_LOOKUP = 242
            CASE ('WY_63')
               STATE_ID_LOOKUP = 243
            CASE ('WY_74')
               STATE_ID_LOOKUP = 244
            CASE ('WY_75')
               STATE_ID_LOOKUP = 245
            CASE ('AB_98')
               STATE_ID_LOOKUP = 246
            CASE ('AZ_94')
               STATE_ID_LOOKUP = 247
            CASE ('AZ_99')
               STATE_ID_LOOKUP = 248
            CASE ('BC_98')
               STATE_ID_LOOKUP = 249
            CASE ('CA_99')
               STATE_ID_LOOKUP = 250
            CASE ('ID_29')
               STATE_ID_LOOKUP = 251
            CASE ('KS_61')
               STATE_ID_LOOKUP = 252
            CASE ('NY_40')
               STATE_ID_LOOKUP = 253
            CASE ('NY_97')
               STATE_ID_LOOKUP = 254
            CASE ('OK_08')
               STATE_ID_LOOKUP = 255
            CASE ('ON_40')
               STATE_ID_LOOKUP = 256
            CASE ('WY_29')
               STATE_ID_LOOKUP = 257
            CASE ('AR_42')
               STATE_ID_LOOKUP = 258
            CASE ('BCN_58')
               STATE_ID_LOOKUP = 259
            CASE ('BCS_68')
               STATE_ID_LOOKUP = 260
            CASE ('CA_71')
               STATE_ID_LOOKUP = 261
            CASE ('CA_76')
               STATE_ID_LOOKUP = 262
            CASE ('CA_77')
               STATE_ID_LOOKUP = 263
            CASE ('CA_94')
               STATE_ID_LOOKUP = 264
            CASE ('DC_12')
               STATE_ID_LOOKUP = 265
            CASE ('KY_19')
               STATE_ID_LOOKUP = 266
            CASE ('KY_91')
               STATE_ID_LOOKUP = 267
            CASE ('MD_12')
               STATE_ID_LOOKUP = 268
            CASE ('MN_21')
               STATE_ID_LOOKUP = 269
            CASE ('MN_90')
               STATE_ID_LOOKUP = 270
            CASE ('MT_90')
               STATE_ID_LOOKUP = 271
            CASE ('ND_90')
               STATE_ID_LOOKUP = 272
            CASE ('OH_91')
               STATE_ID_LOOKUP = 273
            CASE ('PA_02')
               STATE_ID_LOOKUP = 274
            CASE ('PA_09')
               STATE_ID_LOOKUP = 275
            CASE ('PA_13')
               STATE_ID_LOOKUP = 276
            CASE ('SD_90')
               STATE_ID_LOOKUP = 277
            CASE ('VA_12')
               STATE_ID_LOOKUP = 278
            CASE ('WY_61')
               STATE_ID_LOOKUP = 279
            CASE('GA_44')
               STATE_ID_LOOKUP = 280
            CASE('GA_46')
               STATE_ID_LOOKUP = 281
            CASE('IA_19')
               STATE_ID_LOOKUP = 282
            CASE('ID_74')
               STATE_ID_LOOKUP = 283
            CASE('KY_04')
               STATE_ID_LOOKUP = 284
            CASE('MD_13')
               STATE_ID_LOOKUP = 285
            CASE('MT_71')
               STATE_ID_LOOKUP = 286
            CASE('NC_46')
               STATE_ID_LOOKUP = 287
            CASE('ND_67')
               STATE_ID_LOOKUP = 288
            CASE('NH_86')
               STATE_ID_LOOKUP = 289
            CASE('NL_41')
               STATE_ID_LOOKUP = 290
            CASE('OH_05')
               STATE_ID_LOOKUP = 292
            CASE('OR_29')
               STATE_ID_LOOKUP = 293
            CASE('PA_97')
               STATE_ID_LOOKUP = 294
            CASE('SC_45')
               STATE_ID_LOOKUP = 295
            CASE('UT_29')
               STATE_ID_LOOKUP = 296
            CASE('WV_03')
               STATE_ID_LOOKUP = 297
            CASE('MO_16')
               STATE_ID_LOOKUP = 298
            CASE('IL_16')
               STATE_ID_LOOKUP = 299
            CASE('MA_84')
               STATE_ID_LOOKUP = 300
            CASE('NH_89')
               STATE_ID_LOOKUP = 301
            CASE('RI_89')
               STATE_ID_LOOKUP = 302
            CASE('CT_89')
               STATE_ID_LOOKUP = 303
            CASE('MS_48')
               STATE_ID_LOOKUP = 304
            CASE DEFAULT
               STATE_ID_LOOKUP = CHECK_NEW_STATE_IDS(STATE_ID_NUM)
            END SELECT
      RETURN
      END FUNCTION

      FUNCTION CHECK_NEW_STATE_IDS(STATE_ID_NUM) RESULT(STATE_ID)
      use end_routine
      USE PROD_ARRAYS_DIMENSIONS
      CHARACTER (LEN=6) :: STATE_ID_NUM
      CHARACTER (LEN=6), SAVE :: StateAdds(305:MAX_STATE_LOOKUP_IDS)
      INTEGER (KIND=2),SAVE :: ActiveStateIDs=304
      INTEGER (KIND=2) :: I,STATE_ID
         IF(TRIM(STATE_ID_NUM) /= " " .AND. &
                                      SCAN(STATE_ID_NUM,'#/') == 0) THEN
            IF(ActiveStateIDs == 304) StateAdds(305:) = " "
            DO I = 305, ActiveStateIDs
               IF(TRIM(STATE_ID_NUM) == TRIM(StateAdds(I))) EXIT
            ENDDO
            IF(I > ActiveStateIDs) THEN
               ActiveStateIDs = ActiveStateIDs + 1
               StateAdds(ActiveStateIDs) = STATE_ID_NUM
            ENDIF
            STATE_ID = I
         ELSE
            STATE_ID = -1
         ENDIF
      END FUNCTION

!
      FUNCTION STATE_2_GAS_REGION_LOOKUP(R_STATE_ID_NUM)
      use end_routine
      implicit none

      INTEGER (KIND=2) :: STATE_2_GAS_REGION_LOOKUP
      CHARACTER (LEN=6) :: R_STATE_ID_NUM
!
! THIS WILL PROBABLY NEED TO BE AN EXTERNALLY DEFINED TABLE.
!
! 082207.
!
         SELECT CASE(trim(R_STATE_ID_NUM))
            CASE ('AK')
               STATE_2_GAS_REGION_LOOKUP = 1
            CASE ('AL')
               STATE_2_GAS_REGION_LOOKUP = 2
            CASE ('AR')
               STATE_2_GAS_REGION_LOOKUP = 3
            CASE ('AZ')
               STATE_2_GAS_REGION_LOOKUP = 4
            CASE ('CA')
               STATE_2_GAS_REGION_LOOKUP = 5
            CASE ('CO')
               STATE_2_GAS_REGION_LOOKUP = 6
            CASE ('CT')
               STATE_2_GAS_REGION_LOOKUP = 7
            CASE ('DC')
               STATE_2_GAS_REGION_LOOKUP = 8
            CASE ('DE')
               STATE_2_GAS_REGION_LOOKUP = 9
            CASE ('FL')
               STATE_2_GAS_REGION_LOOKUP = 10
            CASE ('GA')
               STATE_2_GAS_REGION_LOOKUP = 11
            CASE ('HI')
               STATE_2_GAS_REGION_LOOKUP = 12
            CASE ('ID')
               STATE_2_GAS_REGION_LOOKUP = 14
            CASE ('IL')
               STATE_2_GAS_REGION_LOOKUP = 15
            CASE ('IN')
               STATE_2_GAS_REGION_LOOKUP = 16
            CASE ('IA')
               STATE_2_GAS_REGION_LOOKUP = 13
            CASE ('KS')
               STATE_2_GAS_REGION_LOOKUP = 17
            CASE ('KY')
               STATE_2_GAS_REGION_LOOKUP = 18
            CASE ('LA')
               STATE_2_GAS_REGION_LOOKUP = 19
            CASE ('ME')
               STATE_2_GAS_REGION_LOOKUP = 22
            CASE ('MD')
               STATE_2_GAS_REGION_LOOKUP = 21
            CASE ('MA')
               STATE_2_GAS_REGION_LOOKUP = 20
            CASE ('MI')
               STATE_2_GAS_REGION_LOOKUP = 23
            CASE ('MN')
               STATE_2_GAS_REGION_LOOKUP = 24
            CASE ('MS')
               STATE_2_GAS_REGION_LOOKUP = 26
            CASE ('MO')
               STATE_2_GAS_REGION_LOOKUP = 25
            CASE ('MT')
               STATE_2_GAS_REGION_LOOKUP = 27
            CASE ('NE')
               STATE_2_GAS_REGION_LOOKUP = 30
            CASE ('NV')
               STATE_2_GAS_REGION_LOOKUP = 34
            CASE ('NH')
               STATE_2_GAS_REGION_LOOKUP = 31
            CASE ('NJ')
               STATE_2_GAS_REGION_LOOKUP = 32
            CASE ('NM')
               STATE_2_GAS_REGION_LOOKUP = 33
            CASE ('NY')
               STATE_2_GAS_REGION_LOOKUP = 35
            CASE ('NC')
               STATE_2_GAS_REGION_LOOKUP = 28
            CASE ('ND')
               STATE_2_GAS_REGION_LOOKUP = 29
            CASE ('OH')
               STATE_2_GAS_REGION_LOOKUP = 36
            CASE ('OK')
               STATE_2_GAS_REGION_LOOKUP = 37
            CASE ('OR')
               STATE_2_GAS_REGION_LOOKUP = 38
            CASE ('PA')
               STATE_2_GAS_REGION_LOOKUP = 39
            CASE ('RI')
               STATE_2_GAS_REGION_LOOKUP = 40
            CASE ('SC')
               STATE_2_GAS_REGION_LOOKUP = 41
            CASE ('SD')
               STATE_2_GAS_REGION_LOOKUP = 42
            CASE ('TN')
               STATE_2_GAS_REGION_LOOKUP = 43
            CASE ('TX')
               STATE_2_GAS_REGION_LOOKUP = 44
            CASE ('UT')
               STATE_2_GAS_REGION_LOOKUP = 45
            CASE ('VT')
               STATE_2_GAS_REGION_LOOKUP = 47
            CASE ('VA')
               STATE_2_GAS_REGION_LOOKUP = 46
            CASE ('WA')
               STATE_2_GAS_REGION_LOOKUP = 48
            CASE ('WV')
               STATE_2_GAS_REGION_LOOKUP = 50
            CASE ('WI')
               STATE_2_GAS_REGION_LOOKUP = 49
            CASE ('WY')
               STATE_2_GAS_REGION_LOOKUP = 51
            CASE ('AB')
               STATE_2_GAS_REGION_LOOKUP = 61
            CASE ('BC')
               STATE_2_GAS_REGION_LOOKUP = 62
            CASE ('MB')
               STATE_2_GAS_REGION_LOOKUP = 63
            CASE ('NB')
               STATE_2_GAS_REGION_LOOKUP = 64
            CASE ('NL')
               STATE_2_GAS_REGION_LOOKUP = 65
            CASE ('NS')
               STATE_2_GAS_REGION_LOOKUP = 66
            CASE ('ON')
               STATE_2_GAS_REGION_LOOKUP = 67
            CASE ('PE')
               STATE_2_GAS_REGION_LOOKUP = 68
            CASE ('QC')
               STATE_2_GAS_REGION_LOOKUP = 69
            CASE ('SK')
               STATE_2_GAS_REGION_LOOKUP = 70
            CASE ('BCN')
               STATE_2_GAS_REGION_LOOKUP = 71
            CASE ('BCS')
               STATE_2_GAS_REGION_LOOKUP = 63
            CASE ('CAM')
               STATE_2_GAS_REGION_LOOKUP = 64
            CASE ('CHS')
               STATE_2_GAS_REGION_LOOKUP = 65
            CASE ('CHI')
               STATE_2_GAS_REGION_LOOKUP = 66
            CASE ('COA')
               STATE_2_GAS_REGION_LOOKUP = 67
            CASE ('COL')
               STATE_2_GAS_REGION_LOOKUP = 68
            CASE ('DGO')
               STATE_2_GAS_REGION_LOOKUP = 69
            CASE ('GTO')
               STATE_2_GAS_REGION_LOOKUP = 70
            CASE ('GRO')
               STATE_2_GAS_REGION_LOOKUP = 71
            CASE ('HGO')
               STATE_2_GAS_REGION_LOOKUP = 72
            CASE ('JAL')
               STATE_2_GAS_REGION_LOOKUP = 73
            CASE ('MEX')
               STATE_2_GAS_REGION_LOOKUP = 74
            CASE ('MIC')
               STATE_2_GAS_REGION_LOOKUP = 75
            CASE ('NAY')
               STATE_2_GAS_REGION_LOOKUP = 76
            CASE ('NLN')
               STATE_2_GAS_REGION_LOOKUP = 77
            CASE ('OAX')
               STATE_2_GAS_REGION_LOOKUP = 78
            CASE ('PUE')
               STATE_2_GAS_REGION_LOOKUP = 79
            CASE ('QRO')
               STATE_2_GAS_REGION_LOOKUP = 80
            CASE ('QTR')
               STATE_2_GAS_REGION_LOOKUP = 81
            CASE ('SLP')
               STATE_2_GAS_REGION_LOOKUP = 82
            CASE ('SIN')
               STATE_2_GAS_REGION_LOOKUP = 83
            CASE ('AB_52')
               STATE_2_GAS_REGION_LOOKUP = 303
            CASE ('AB_53')
               STATE_2_GAS_REGION_LOOKUP = 303
            CASE ('AL_45')
               STATE_2_GAS_REGION_LOOKUP = 289
            CASE ('AL_46')
               STATE_2_GAS_REGION_LOOKUP = 289
            CASE ('AR_43')
               STATE_2_GAS_REGION_LOOKUP = 88
            CASE ('AR_50')
               STATE_2_GAS_REGION_LOOKUP = 89
            CASE ('AZ_54')
               STATE_2_GAS_REGION_LOOKUP = 90
            CASE ('AZ_65')
               STATE_2_GAS_REGION_LOOKUP = 91
            CASE ('AZ_72')
               STATE_2_GAS_REGION_LOOKUP = 92
            CASE ('AZ_73')
               STATE_2_GAS_REGION_LOOKUP = 93
            CASE ('AZ_79')
               STATE_2_GAS_REGION_LOOKUP = 94
            CASE ('BC_52')
               STATE_2_GAS_REGION_LOOKUP = 95
            CASE ('BC_55')
               STATE_2_GAS_REGION_LOOKUP = 96
            CASE ('BCN_68')
               STATE_2_GAS_REGION_LOOKUP = 97
            CASE ('BCN_79')
               STATE_2_GAS_REGION_LOOKUP = 98
            CASE ('CA_56')
               STATE_2_GAS_REGION_LOOKUP = 300
            CASE ('CA_57')
               STATE_2_GAS_REGION_LOOKUP = 302
            CASE ('CA_58')
               STATE_2_GAS_REGION_LOOKUP = 302
            CASE ('CA_59')
               STATE_2_GAS_REGION_LOOKUP = 302
            CASE ('CA_65')
               STATE_2_GAS_REGION_LOOKUP = 302
            CASE ('CA_66')
               STATE_2_GAS_REGION_LOOKUP = 302
            CASE ('CA_70')
               STATE_2_GAS_REGION_LOOKUP = 300
            CASE ('CA_73')
! 101709. REASSIGNED FOR TOPO TO CA_99 PER BURESH
               STATE_2_GAS_REGION_LOOKUP = 300
!               STATE_2_GAS_REGION_LOOKUP = 106
            CASE ('CHI_68')
               STATE_2_GAS_REGION_LOOKUP = 107
            CASE ('CO_61')
               STATE_2_GAS_REGION_LOOKUP = 297
            CASE ('CO_62')
               STATE_2_GAS_REGION_LOOKUP = 297
            CASE ('CT_86')
               STATE_2_GAS_REGION_LOOKUP = 281
            CASE ('CT_87')
               STATE_2_GAS_REGION_LOOKUP = 281
            CASE ('CT_88')
               STATE_2_GAS_REGION_LOOKUP = 281
            CASE ('DC_16')
               STATE_2_GAS_REGION_LOOKUP = 285
            CASE ('DE_15')
               STATE_2_GAS_REGION_LOOKUP = 284
            CASE ('FL_14')
               STATE_2_GAS_REGION_LOOKUP = 115
            CASE ('FL_45')
               STATE_2_GAS_REGION_LOOKUP = 116
            CASE ('GA_45')
               STATE_2_GAS_REGION_LOOKUP = 117
            CASE ('IA_17')
               STATE_2_GAS_REGION_LOOKUP = 294
            CASE ('IA_21')
               STATE_2_GAS_REGION_LOOKUP = 294
            CASE ('IA_23')
               STATE_2_GAS_REGION_LOOKUP = 294
            CASE ('IA_25')
               STATE_2_GAS_REGION_LOOKUP = 294
            CASE ('ID_63')
               STATE_2_GAS_REGION_LOOKUP = 298
            CASE ('ID_64')
               STATE_2_GAS_REGION_LOOKUP = 298
            CASE ('ID_71')
               STATE_2_GAS_REGION_LOOKUP = 298
            CASE ('IL_18')
               STATE_2_GAS_REGION_LOOKUP = 293
            CASE ('IL_19')
               STATE_2_GAS_REGION_LOOKUP = 293
            CASE ('IL_21')
               STATE_2_GAS_REGION_LOOKUP = 293
            CASE ('IL_46')
               STATE_2_GAS_REGION_LOOKUP = 293
            CASE ('IN_02')
               STATE_2_GAS_REGION_LOOKUP = 129
            CASE ('IN_03')
               STATE_2_GAS_REGION_LOOKUP = 130
            CASE ('IN_17')
               STATE_2_GAS_REGION_LOOKUP = 131
            CASE ('IN_18')
               STATE_2_GAS_REGION_LOOKUP = 132
            CASE ('KS_51')
               STATE_2_GAS_REGION_LOOKUP = 296
            CASE ('KY_02')
               STATE_2_GAS_REGION_LOOKUP = 287
            CASE ('KY_03')
               STATE_2_GAS_REGION_LOOKUP = 287
            CASE ('KY_05')
               STATE_2_GAS_REGION_LOOKUP = 287
            CASE ('KY_46')
               STATE_2_GAS_REGION_LOOKUP = 287
            CASE ('LA_43')
               STATE_2_GAS_REGION_LOOKUP = 138
            CASE ('LA_49')
               STATE_2_GAS_REGION_LOOKUP = 139
            CASE ('LA_50')
               STATE_2_GAS_REGION_LOOKUP = 140
            CASE ('MA_86')
               STATE_2_GAS_REGION_LOOKUP = 281
            CASE ('MA_87')
               STATE_2_GAS_REGION_LOOKUP = 281
            CASE ('MA_89')
               STATE_2_GAS_REGION_LOOKUP = 281
            CASE ('MB_22')
               STATE_2_GAS_REGION_LOOKUP = 144
            CASE ('MD_01')
               STATE_2_GAS_REGION_LOOKUP = 285
            CASE ('MD_15')
               STATE_2_GAS_REGION_LOOKUP = 285
            CASE ('MD_16')
               STATE_2_GAS_REGION_LOOKUP = 285
            CASE ('ME_27')
               STATE_2_GAS_REGION_LOOKUP = 280
            CASE ('ME_85')
               STATE_2_GAS_REGION_LOOKUP = 280
            CASE ('MI_02')
               STATE_2_GAS_REGION_LOOKUP = 150
            CASE ('MI_06')
               STATE_2_GAS_REGION_LOOKUP = 151
            CASE ('MI_20')
               STATE_2_GAS_REGION_LOOKUP = 152
            CASE ('MN_17')
               STATE_2_GAS_REGION_LOOKUP = 294
            CASE ('MN_23')
               STATE_2_GAS_REGION_LOOKUP = 294
            CASE ('MN_25')
               STATE_2_GAS_REGION_LOOKUP = 294
            CASE ('MO_19')
               STATE_2_GAS_REGION_LOOKUP = 156
            CASE ('MO_42')
               STATE_2_GAS_REGION_LOOKUP = 157
            CASE ('MO_50')
               STATE_2_GAS_REGION_LOOKUP = 158
            CASE ('MO_51')
               STATE_2_GAS_REGION_LOOKUP = 159
            CASE ('MS_43')
               STATE_2_GAS_REGION_LOOKUP = 289
            CASE ('MS_45')
               STATE_2_GAS_REGION_LOOKUP = 289
            CASE ('MS_46')
               STATE_2_GAS_REGION_LOOKUP = 289
            CASE ('MT_25')
               STATE_2_GAS_REGION_LOOKUP = 295
            CASE ('MT_67')
               STATE_2_GAS_REGION_LOOKUP = 295
            CASE ('NB_27')
               STATE_2_GAS_REGION_LOOKUP = 304
            CASE ('NC_44')
               STATE_2_GAS_REGION_LOOKUP = 288
            CASE ('NC_47')
               STATE_2_GAS_REGION_LOOKUP = 288
            CASE ('ND_23')
               STATE_2_GAS_REGION_LOOKUP = 295
            CASE ('ND_25')
               STATE_2_GAS_REGION_LOOKUP = 295
            CASE ('NE_24')
               STATE_2_GAS_REGION_LOOKUP = 170
            CASE ('NE_75')
               STATE_2_GAS_REGION_LOOKUP = 171
            CASE ('NH_87')
               STATE_2_GAS_REGION_LOOKUP = 280
            CASE ('NJ_15')
               STATE_2_GAS_REGION_LOOKUP = 284
            CASE ('NJ_38')
               STATE_2_GAS_REGION_LOOKUP = 284
            CASE ('NM_50')
               STATE_2_GAS_REGION_LOOKUP = 292
            CASE ('NM_61')
               STATE_2_GAS_REGION_LOOKUP = 292
            CASE ('NM_69')
               STATE_2_GAS_REGION_LOOKUP = 292
            CASE ('NS_27')
               STATE_2_GAS_REGION_LOOKUP = 304
            CASE ('NV_57')
               STATE_2_GAS_REGION_LOOKUP = 179
            CASE ('NV_70')
               STATE_2_GAS_REGION_LOOKUP = 299
            CASE ('NV_73')
               STATE_2_GAS_REGION_LOOKUP = 301
            CASE ('NY_31')
               STATE_2_GAS_REGION_LOOKUP = 282
            CASE ('NY_32')
               STATE_2_GAS_REGION_LOOKUP = 282
            CASE ('NY_33')
               STATE_2_GAS_REGION_LOOKUP = 282
            CASE ('NY_34')
               STATE_2_GAS_REGION_LOOKUP = 282 ! FOR 091309 TOPO
            CASE ('NY_38')
               STATE_2_GAS_REGION_LOOKUP = 283
            CASE ('NY_39')
               STATE_2_GAS_REGION_LOOKUP = 282 ! FOR 091309 TOPO
            CASE ('NY_95')
               STATE_2_GAS_REGION_LOOKUP = 282
            CASE ('NY_96')
               STATE_2_GAS_REGION_LOOKUP = 282 ! FOR 091309 TOPO
            CASE ('OH_01')
               STATE_2_GAS_REGION_LOOKUP = 286
            CASE ('OH_02')
               STATE_2_GAS_REGION_LOOKUP = 286
            CASE ('OH_03')
               STATE_2_GAS_REGION_LOOKUP = 286
            CASE ('OH_04')
               STATE_2_GAS_REGION_LOOKUP = 286
            CASE ('OK_42')
               STATE_2_GAS_REGION_LOOKUP = 296
            CASE ('OK_50')
               STATE_2_GAS_REGION_LOOKUP = 296
            CASE ('OK_78')
               STATE_2_GAS_REGION_LOOKUP = 296
            CASE ('ON_80')
               STATE_2_GAS_REGION_LOOKUP = 197
            CASE ('ON_81')
               STATE_2_GAS_REGION_LOOKUP = 198
            CASE ('ON_82')
               STATE_2_GAS_REGION_LOOKUP = 199
            CASE ('ON_83')
               STATE_2_GAS_REGION_LOOKUP = 200
            CASE ('OR_60')
               STATE_2_GAS_REGION_LOOKUP = 298
            CASE ('OR_71')
               STATE_2_GAS_REGION_LOOKUP = 298
            CASE ('PA_01')
               STATE_2_GAS_REGION_LOOKUP = 203
            CASE ('PA_04')
               STATE_2_GAS_REGION_LOOKUP = 204
            CASE ('PA_15')
               STATE_2_GAS_REGION_LOOKUP = 205
            CASE ('PA_16')
               STATE_2_GAS_REGION_LOOKUP = 206
            CASE ('PA_95')
               STATE_2_GAS_REGION_LOOKUP = 207
            CASE ('PE_27')
               STATE_2_GAS_REGION_LOOKUP = 208
            CASE ('QC_41')
               STATE_2_GAS_REGION_LOOKUP = 209
            CASE ('RI_87')
               STATE_2_GAS_REGION_LOOKUP = 281
            CASE ('SC_44')
               STATE_2_GAS_REGION_LOOKUP = 288
            CASE ('SD_23')
               STATE_2_GAS_REGION_LOOKUP = 295
            CASE ('SD_25')
               STATE_2_GAS_REGION_LOOKUP = 295
            CASE ('SD_75')
               STATE_2_GAS_REGION_LOOKUP = 295
            CASE ('SK_26')
               STATE_2_GAS_REGION_LOOKUP = 303
            CASE ('TN_46')
               STATE_2_GAS_REGION_LOOKUP = 287
            CASE ('TX_07')
               STATE_2_GAS_REGION_LOOKUP = 291
            CASE ('TX_08')
               STATE_2_GAS_REGION_LOOKUP = 290
            CASE ('TX_09')
               STATE_2_GAS_REGION_LOOKUP = 219
            CASE ('TX_10')
               STATE_2_GAS_REGION_LOOKUP = 291
            CASE ('TX_11')
               STATE_2_GAS_REGION_LOOKUP = 292
            CASE ('TX_12')
               STATE_2_GAS_REGION_LOOKUP = 222
            CASE ('TX_13')
               STATE_2_GAS_REGION_LOOKUP = 223
            CASE ('TX_43')
               STATE_2_GAS_REGION_LOOKUP = 291 ! for topo. 101609
            CASE ('TX_50')
               STATE_2_GAS_REGION_LOOKUP = 290
            CASE ('TX_69')
               STATE_2_GAS_REGION_LOOKUP = 292 ! for topo. 101609
            CASE ('TX_76')
               STATE_2_GAS_REGION_LOOKUP = 227
            CASE ('TX_77')
               STATE_2_GAS_REGION_LOOKUP = 228
            CASE ('UT_74')
               STATE_2_GAS_REGION_LOOKUP = 297
            CASE ('VA_02')
               STATE_2_GAS_REGION_LOOKUP = 285
            CASE ('VA_15')
               STATE_2_GAS_REGION_LOOKUP = 285
            CASE ('VA_16')
               STATE_2_GAS_REGION_LOOKUP = 285
            CASE ('VA_47')
               STATE_2_GAS_REGION_LOOKUP = 285
            CASE ('VT_86')
               STATE_2_GAS_REGION_LOOKUP = 280
            CASE ('VT_87')
               STATE_2_GAS_REGION_LOOKUP = 280
            CASE ('WA_71')
               STATE_2_GAS_REGION_LOOKUP = 298
            CASE ('WI_20')
               STATE_2_GAS_REGION_LOOKUP = 293
            CASE ('WI_23')
               STATE_2_GAS_REGION_LOOKUP = 293
            CASE ('WV_01')
               STATE_2_GAS_REGION_LOOKUP = 286
            CASE ('WV_02')
               STATE_2_GAS_REGION_LOOKUP = 286
            CASE ('WV_47')
               STATE_2_GAS_REGION_LOOKUP = 286
            CASE ('WY_25')
               STATE_2_GAS_REGION_LOOKUP = 297
            CASE ('WY_63')
               STATE_2_GAS_REGION_LOOKUP = 297
            CASE ('WY_74')
               STATE_2_GAS_REGION_LOOKUP = 297
            CASE ('WY_75')
               STATE_2_GAS_REGION_LOOKUP = 297
            CASE ('AB_98')
               STATE_2_GAS_REGION_LOOKUP = 303
            CASE ('AZ_94')
               STATE_2_GAS_REGION_LOOKUP = 247
            CASE ('AZ_99')
               STATE_2_GAS_REGION_LOOKUP = 248
            CASE ('BC_98')
               STATE_2_GAS_REGION_LOOKUP = 249
            CASE ('CA_99')
               STATE_2_GAS_REGION_LOOKUP = 250
            CASE ('ID_29')
               STATE_2_GAS_REGION_LOOKUP = 298
            CASE ('KS_61')
               STATE_2_GAS_REGION_LOOKUP = 296
            CASE ('NY_40')
               STATE_2_GAS_REGION_LOOKUP = 282
            CASE ('NY_97')
               STATE_2_GAS_REGION_LOOKUP = 282 ! FOR 091309 TOPO
            CASE ('OK_08')
               STATE_2_GAS_REGION_LOOKUP = 296
            CASE ('ON_40')
               STATE_2_GAS_REGION_LOOKUP = 256
            CASE ('WY_29')
               STATE_2_GAS_REGION_LOOKUP = 297
            CASE ('AR_42')
               STATE_2_GAS_REGION_LOOKUP = 258
            CASE ('BCN_58')
               STATE_2_GAS_REGION_LOOKUP = 259
            CASE ('BCS_68')
               STATE_2_GAS_REGION_LOOKUP = 260
            CASE ('CA_71')
               STATE_2_GAS_REGION_LOOKUP = 300
            CASE ('CA_76')
               STATE_2_GAS_REGION_LOOKUP = 300
            CASE ('CA_77')
               STATE_2_GAS_REGION_LOOKUP = 300
            CASE ('CA_94')
               STATE_2_GAS_REGION_LOOKUP = 302
            CASE ('DC_12')
               STATE_2_GAS_REGION_LOOKUP = 285
            CASE ('KY_19')
               STATE_2_GAS_REGION_LOOKUP = 287
            CASE ('KY_91')
               STATE_2_GAS_REGION_LOOKUP = 287
            CASE ('MD_12')
               STATE_2_GAS_REGION_LOOKUP = 285
            CASE ('MN_21')
               STATE_2_GAS_REGION_LOOKUP = 294
            CASE ('MN_90')
               STATE_2_GAS_REGION_LOOKUP = 294
            CASE ('MT_90')
               STATE_2_GAS_REGION_LOOKUP = 295
            CASE ('ND_90')
               STATE_2_GAS_REGION_LOOKUP = 295
            CASE ('OH_91')
               STATE_2_GAS_REGION_LOOKUP = 286
            CASE ('PA_02')
               STATE_2_GAS_REGION_LOOKUP = 274
            CASE ('PA_09')
               STATE_2_GAS_REGION_LOOKUP = 275
            CASE ('PA_13')
               STATE_2_GAS_REGION_LOOKUP = 276
            CASE ('SD_90')
               STATE_2_GAS_REGION_LOOKUP = 295
            CASE ('VA_12')
               STATE_2_GAS_REGION_LOOKUP = 285
            CASE ('WY_61')
               STATE_2_GAS_REGION_LOOKUP = 297
            CASE('MENHVT')
               STATE_2_GAS_REGION_LOOKUP = 280
            CASE('MARICT')
               STATE_2_GAS_REGION_LOOKUP = 281
            CASE('NY STA')
               STATE_2_GAS_REGION_LOOKUP = 282
            CASE('NY CIT')
               STATE_2_GAS_REGION_LOOKUP = 283
            CASE('NJ DE')
               STATE_2_GAS_REGION_LOOKUP = 284
            CASE('MDDCVA')
               STATE_2_GAS_REGION_LOOKUP = 285
!            CASE('PA')
!               STATE_2_GAS_REGION_LOOKUP = 279
            CASE('OH WV')
               STATE_2_GAS_REGION_LOOKUP = 286
            CASE('KY TN')
               STATE_2_GAS_REGION_LOOKUP = 287
            CASE('NC SC')
               STATE_2_GAS_REGION_LOOKUP = 288
!            CASE('GA')
!               STATE_2_GAS_REGION_LOOKUP = 279
            CASE('AL MS')
               STATE_2_GAS_REGION_LOOKUP = 289
!            CASE('FL')
!               STATE_2_GAS_REGION_LOOKUP = 279
!            CASE('LA')
!               STATE_2_GAS_REGION_LOOKUP = 279
            CASE('TX N')
               STATE_2_GAS_REGION_LOOKUP = 290
            CASE('TX S')
               STATE_2_GAS_REGION_LOOKUP = 291
            CASE('TXW NM')
               STATE_2_GAS_REGION_LOOKUP = 292
!            CASE('AZ')
!               STATE_2_GAS_REGION_LOOKUP = 279
!            CASE('AR')
!               STATE_2_GAS_REGION_LOOKUP = 279
!            CASE('MO')
!               STATE_2_GAS_REGION_LOOKUP = 279
!            CASE('IN')
!               STATE_2_GAS_REGION_LOOKUP = 279
!            CASE('MI')
!               STATE_2_GAS_REGION_LOOKUP = 279
            CASE('IL WI')
               STATE_2_GAS_REGION_LOOKUP = 293
            CASE('IA MN')
               STATE_2_GAS_REGION_LOOKUP = 294
            CASE('MTNDSD')
               STATE_2_GAS_REGION_LOOKUP = 295
!            CASE('NE')
!               STATE_2_GAS_REGION_LOOKUP = 279
            CASE('KS OK')
               STATE_2_GAS_REGION_LOOKUP = 296
            CASE('COWYUT')
               STATE_2_GAS_REGION_LOOKUP = 297
            CASE('WAORID')
               STATE_2_GAS_REGION_LOOKUP = 298
            CASE('NV N')
               STATE_2_GAS_REGION_LOOKUP = 299
            CASE('CA N')
               STATE_2_GAS_REGION_LOOKUP = 300
            CASE('NV S')
               STATE_2_GAS_REGION_LOOKUP = 301
            CASE('CA S')
               STATE_2_GAS_REGION_LOOKUP = 302
            CASE('AB SK')
               STATE_2_GAS_REGION_LOOKUP = 303
!            CASE('BC')
!               STATE_2_GAS_REGION_LOOKUP = 279
!            CASE('MN')
!               STATE_2_GAS_REGION_LOOKUP = 279
            CASE('NB NS')
               STATE_2_GAS_REGION_LOOKUP = 304
!            CASE('ON')
!               STATE_2_GAS_REGION_LOOKUP = 279
!            CASE('QB')
!               STATE_2_GAS_REGION_LOOKUP = 279
            CASE('MX STX')
               STATE_2_GAS_REGION_LOOKUP = 279
            CASE('MX WTX')
               STATE_2_GAS_REGION_LOOKUP = 279
            CASE('MX AZ')
               STATE_2_GAS_REGION_LOOKUP = 279
            CASE('MX CA')
               STATE_2_GAS_REGION_LOOKUP = 279
            CASE DEFAULT
               STATE_2_GAS_REGION_LOOKUP = -1
            END SELECT
      RETURN
      END

!
      FUNCTION MARKET_AREA_LOOKUP(R_MARKET_AREA_COMPANY_ID_NUM)
      use end_routine
      implicit none
      INTEGER (KIND=2) :: MARKET_AREA_LOOKUP
      CHARACTER (LEN=5) :: R_MARKET_AREA_COMPANY_ID_NUM
      CHARACTER (LEN=3) :: STR_NUM,MARKET_AREA
         MARKET_AREA = R_MARKET_AREA_COMPANY_ID_NUM(1:2)
         MARKET_AREA_LOOKUP = -1
         IF(MARKET_AREA == 'MK' .OR. MARKET_AREA == 'ZN') THEN
            STR_NUM = R_MARKET_AREA_COMPANY_ID_NUM(3:)
            READ(STR_NUM,'(I3)') MARKET_AREA_LOOKUP
            IF(MARKET_AREA_LOOKUP < 0 .OR. MARKET_AREA_LOOKUP > 599) &
                                                 MARKET_AREA_LOOKUP = -1
         ENDIF
!         RETURN
      RETURN
      END
! 020107. NEW VARIABLES FOR AOD

!
!        PROGRAM TO READ MULTI-TAB INFORMATION ON MARKET GROUPS
!                 AND CONVERT TO BINARY FORMAT
!                       COPYRIGHT (C) 2001
!      ALL RIGHTS RESERVED M.S. GERBER & ASSOCIATES, INC.
!

!
      SUBROUTINE MK_OBJECT
      use end_routine, only: end_program, er_message
      use spindriftlib
      use prod_arrays_dimensions
      use sizecom
      use iostatmsg
      use miscmod
      use logging
      use flusher
      use mk_fileio
      use wh_shared_data
      implicit none
      integer :: mkunit_read, mkunit_write
      LOGICAL (KIND=1) :: SAVE_MK_FILE_EXISTS=.FALSE.
      LOGICAL :: R_MK_FILE_EXISTS
      INTEGER (KIND=2) :: UNIT_NUM=10,INUNIT,IREC,DELETE
      INTEGER (KIND=2) :: SAVE_MARKET_GROUPS_TABLES=0
      INTEGER (KIND=2) :: R_MARKET_GROUPS_TABLES
      INTEGER (KIND=2) :: SAVE_MARKET_GROUPS_RECORDS=0
      INTEGER (KIND=2) :: R_MARKET_GROUPS_RECORDS,TRANSACTION_GROUP_ID
      INTEGER :: IOS
      
      character (len=2) :: r_mkroup_ol_pass
      CHARACTER (LEN=5) :: MARKET_GROUPS_FILE,OVERLAY_FAMILY_NAME
      CHARACTER (LEN=3) :: HOURLY_PRICE_NAME
      CHARACTER (LEN=256) :: FILE_NAME,FILE_NAME_OVL
      CHARACTER (LEN=256) :: OUTPUT_DIRECTORY
      CHARACTER (LEN=256) :: BASE_FILE_DIRECTORY
      CHARACTER (LEN=152) :: MESSAGE
      LOGICAL (KIND=4) :: FILE_EXISTS
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER (LEN=1024) :: RECLN
      logical :: beenhere=.false.

!
! SIMULATION VARIABLES
!
      CHARACTER (LEN=1) :: MKT_AREA_ACTIVE_loc,REPORT_THIS_GROUP
      CHARACTER (LEN=1) :: MARKET_LOADS_ACTIVE,REPORT_MARKET_LOADS
      CHARACTER (LEN=1) :: MARKET_THERMAL_ACTIVE,REPORT_MARKET_THERMAL
      CHARACTER (LEN=1) :: MARKET_HYDRO_ACTIVE,REPORT_MARKET_HYDRO
      CHARACTER (LEN=1) :: MARKET_CONTRACTS_ACTIVE
      CHARACTER (LEN=1) :: REPORT_MARKET_CONTRACTS
      CHARACTER (LEN=6) :: PORTFL_MKT_AREA_ABBREV
      CHARACTER (LEN=6) :: PORTFOLIO_MARKET_AREA_ID
      CHARACTER (LEN=20) :: PORTFL_MKT_AREA_NAME_LOC
!      CHARACTER*50 COMMENT
!
! FILE MANAGEMENT VARIABLES
!
      CHARACTER (LEN=17) :: FILE_TYPE='Market Group     '
      CHARACTER (LEN=2) :: R_MKROUP_OL
      LOGICAL (KIND=1) :: LAHEY_LF95
      CHARACTER (LEN=30) :: SCREEN_OUTPUT
      character (len=255) :: fname
!
!
!
!

!
!          ROUTINE TO CONVERT FROM ASCII TO DIRECT-ACESS BINARY
!          COPYRIGHT (C) 1983-98  M.S. GERBER & ASSOCIATES, INC.
!

!
! CONVERT THE MARKET GROUPS FILE
!
!

      ENTRY MK_MAKEBIN


      FILE_NAME = trim(BASE_FILE_DIRECTORY())// &
                            "MKB"//trim(MARKET_GROUPS_FILE())//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//MARKET_GROUPS_FILE()
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_LOCATE_WRITE(16,30,MARKET_GROUPS_FILE(), &
                                                         ALL_VERSIONS,0)
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         ENDIF
!
         SAVE_MK_FILE_EXISTS = .TRUE.
         
         OPEN(10,FILE=FILE_NAME) ! MkB/mgf/.dat ^^
         
         file_name=trim(OUTPUT_DIRECTORY())//"BCMKROUP.BIN"
         
         mkunit_write=open_mk_file_write(r_mkroup_ol, kroup_lrecl)
         
!
!
         SAVE_MARKET_GROUPS_TABLES = 0
         MKT_AREA_ACTIVE_loc = 'T'
         REPORT_THIS_GROUP = 'T'
         MARKET_LOADS_ACTIVE = 'T'
         REPORT_MARKET_LOADS = 'T'
         MARKET_THERMAL_ACTIVE = 'T'
         REPORT_MARKET_THERMAL = 'T'
         MARKET_HYDRO_ACTIVE = 'T'
         REPORT_MARKET_HYDRO = 'T'
         MARKET_CONTRACTS_ACTIVE = 'T'
         REPORT_MARKET_CONTRACTS = 'T'
!
         IREC = 0
         READ(10,*) DELETE ! MkB/mgf/.dat
         DO ! TABLES
            DO ! GROUP-BASED RECORDS
               READ(10,1000,IOSTAT=IOS) RECLN

               IF(IOS /= 0) then
                EXIT ! END OF FILE
               end if

               IF(RECLN(1:1) == '7') THEN 
                  beenhere=.false.
                  EXIT ! GO TO NEXT TABLE
               ENDIF

               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,ERR=200)   DELETE, &
                                       PORTFL_MKT_AREA_NAME_LOC, &
                                       PORTFL_MKT_AREA_ABBREV, &
                                       PORTFOLIO_MARKET_AREA_ID, &
                                       MKT_AREA_ACTIVE_loc, &
                                       REPORT_THIS_GROUP, &
                                       TRANSACTION_GROUP_ID, &
                                       MARKET_LOADS_ACTIVE, &
                                       REPORT_MARKET_LOADS, &
                                       MARKET_THERMAL_ACTIVE, &
                                       REPORT_MARKET_THERMAL, &
                                       MARKET_HYDRO_ACTIVE, &
                                       REPORT_MARKET_HYDRO, &
                                       MARKET_CONTRACTS_ACTIVE, &
                                       REPORT_MARKET_CONTRACTS
!
               IREC = IREC + 1
               ! wh_objt:0001
               ! Called 282 times (current)
               ! read is at wh_objt:0003
               
               
               WRITE(mkunit_write,REC=IREC)  DELETE, &
                 PORTFL_MKT_AREA_NAME_LOC, &
                 PORTFL_MKT_AREA_ABBREV, &
                 PORTFOLIO_MARKET_AREA_ID, &
                 MKT_AREA_ACTIVE_loc, &
                 REPORT_THIS_GROUP, &
                 TRANSACTION_GROUP_ID, &
                 MARKET_LOADS_ACTIVE, &
                 REPORT_MARKET_LOADS, &
                 MARKET_THERMAL_ACTIVE, &
                 REPORT_MARKET_THERMAL, & 
                 MARKET_HYDRO_ACTIVE, &
                 REPORT_MARKET_HYDRO, &
                 MARKET_CONTRACTS_ACTIVE, &
                 REPORT_MARKET_CONTRACTS


            ENDDO ! MARKET GROUPS
            SAVE_MARKET_GROUPS_TABLES = SAVE_MARKET_GROUPS_TABLES + 1
            IF(IOS /= 0) then
                EXIT
            end if
         ENDDO ! READ TABLES
         CLOSE(10)
      ELSE IF(INDEX(MARKET_GROUPS_FILE(),'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "wh_objt:1118")
      ELSE
         mkunit_write=open_mk_file_write(r_mkroup_ol, kroup_lrecl)
         ! TODO:  Remove local variable and create set subroutine
         SAVE_MK_FILE_EXISTS = .FALSE.
!
      ENDIF
      SAVE_MARKET_GROUPS_RECORDS = IREC

      CLOSE(mkunit_write)
      RETURN

!
!          ROUTINE TO CREATE OVERLAY FILES
!          COPYRIGHT (C) 1984-88  M.S. GERBER & ASSOCIATES, INC.
!          COPYRIGHT (C) 1991-92  M.S. GERBER & ASSOCIATES, INC.
!

!
! OVERLAY THE SYSTEM-FORECAST FILE

      ENTRY MK_MAKEOVL(OVERLAY_FAMILY_NAME)

      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      FILE_NAME_OVL=trim(OUTPUT_DIRECTORY())//"MKO"// &
                                     trim(OVERLAY_FAMILY_NAME)//".DAT"

      OPEN(10,FILE=FILE_NAME_OVL) ! MKO
      READ(10,*) DELETE
      INUNIT = 12
      IF(MKROUP_OL == 'BC') THEN
         inunit=open_mk_file_read(r_mkroup_ol, kroup_lrecl)

         
      ENDIF

      FILE_NAME = trim(OUTPUT_DIRECTORY())//"OLMKROUP.BIN"
      mkunit_write=open_mk_file_write(r_mkroup_ol, kroup_lrecl)
      
!      OPEN(12,FILE=FILE_NAME,ACCESS="DIRECT", &
!                                 STATUS="UNKNOWN",RECL=kroup_lrecl,IOSTAT=IOS)
      IF(IOS /= 0) THEN
         CALL iostatmsg_unit(IOS,MESSAGE)
         WRITE(4,*) trim(MESSAGE)
         er_message='in MK_MAKEOVL see WARNINGS messages'
         call end_program(er_message)
      ENDIF
      IREC = 0
      DELETE = 1
!
      READ(10,1000,IOSTAT=IOS) RECLN
      DO WHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE
!
         READ(10,1000,IOSTAT=IOS) RECLN
!
      ENDDO
!
      DO
         IREC = IREC + 1
         READ(INUNIT,REC=IREC,IOSTAT=IOS)  DELETE, &
                                       PORTFL_MKT_AREA_NAME_LOC, &
                                       PORTFL_MKT_AREA_ABBREV, &
                                       PORTFOLIO_MARKET_AREA_ID, &
                                       MKT_AREA_ACTIVE_loc, &
                                       REPORT_THIS_GROUP, &
                                       TRANSACTION_GROUP_ID, &
                                       MARKET_LOADS_ACTIVE, &
                                       REPORT_MARKET_LOADS, &
                                       MARKET_THERMAL_ACTIVE, &
                                       REPORT_MARKET_THERMAL, &
                                       MARKET_HYDRO_ACTIVE, &
                                       REPORT_MARKET_HYDRO, &
                                       MARKET_CONTRACTS_ACTIVE, &
                                       REPORT_MARKET_CONTRACTS
         IF(IOS /= 0) EXIT

         RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
         READ(RECLN,*,ERR=200)         DELETE, &
                                       PORTFL_MKT_AREA_NAME_LOC, &
                                       PORTFL_MKT_AREA_ABBREV, &
                                       PORTFOLIO_MARKET_AREA_ID, &
                                       MKT_AREA_ACTIVE_loc, &
                                       REPORT_THIS_GROUP, &
                                       TRANSACTION_GROUP_ID, &
                                       MARKET_LOADS_ACTIVE, &
                                       REPORT_MARKET_LOADS, &
                                       MARKET_THERMAL_ACTIVE, &
                                       REPORT_MARKET_THERMAL, &
                                       MARKET_HYDRO_ACTIVE, &
                                       REPORT_MARKET_HYDRO, &
                                       MARKET_CONTRACTS_ACTIVE, &
                                       REPORT_MARKET_CONTRACTS
!
         READ(10,1000,IOSTAT=IOS) RECLN
         DO WHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE
!
            READ(10,1000,IOSTAT=IOS) RECLN
!
         ENDDO

! TODO:  Verify and remove this comment.
!  This is another write with the same arguments.
! It was never called during same calculation using
! June '22 executable.
         file_name=get_filename_from_unit(int(mkunit_write))
         if(index(file_name, "BCMKROUP")>0) then
            file_name=file_name ! Debugstop
         end if
         WRITE(mkunit_write,REC=IREC) DELETE, &
                                       PORTFL_MKT_AREA_NAME_LOC, &
                                       PORTFL_MKT_AREA_ABBREV, &
                                       PORTFOLIO_MARKET_AREA_ID, &
                                       MKT_AREA_ACTIVE_loc, &
                                       REPORT_THIS_GROUP, &
                                       TRANSACTION_GROUP_ID, &
                                       MARKET_LOADS_ACTIVE, &
                                       REPORT_MARKET_LOADS, &
                                       MARKET_THERMAL_ACTIVE, &
                                       REPORT_MARKET_THERMAL, &
                                       MARKET_HYDRO_ACTIVE, &
                                       REPORT_MARKET_HYDRO, &
                                       MARKET_CONTRACTS_ACTIVE, &
                                       REPORT_MARKET_CONTRACTS
      ENDDO
      IF(IREC /= SAVE_MARKET_GROUPS_RECORDS) THEN
         WRITE(4,*) "MARKET GROUP OVERLAY DIFFERENT LENGTH"
         WRITE(4,*) "THAN THE BASE FILE. OVERLAY MUST BE THE SAME"
         WRITE(4,*) "LENGTH. ",FILE_NAME_OVL
         WRITE(4,*) "BASE RECORDS ",SAVE_MARKET_GROUPS_RECORDS
         WRITE(4,*) "OVERLAY RECORDS ",IREC
      ENDIF
      CLOSE(10)
      call close_mk_file_write()
      IF(MKROUP_OL == 'BC') then
        close(inunit)
      end if
      MKROUP_OL = 'OL'
      RETURN
!

      ENTRY RESET_MKROUP_OL

         MKROUP_OL = 'BC'
      RETURN
!

      ENTRY RETURN_MKROUP_OL(R_MKROUP_OL_pass)

         R_MKROUP_OL_pass = mkroup_ol
      RETURN

      ENTRY DOES_MK_FILE_EXIST(R_MK_FILE_EXISTS)

         R_MK_FILE_EXISTS = SAVE_MK_FILE_EXISTS
      RETURN

      ENTRY GET_MARKET_GROUPS_TABLES(R_MARKET_GROUPS_TABLES)

         R_MARKET_GROUPS_TABLES = SAVE_MARKET_GROUPS_TABLES
      RETURN

      ENTRY GET_MARKET_GROUPS_RECORDS(R_MARKET_GROUPS_RECORDS)

         R_MARKET_GROUPS_RECORDS = SAVE_MARKET_GROUPS_RECORDS
      RETURN



  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from wh_objt SIID358'
      call end_program(er_message)
!
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END





! 6/8/01. CALL BEFORE RESOURCES OR CUSTOMERS ARE READ.

!
      FUNCTION READ_MARKET_GROUPS_DATA()
      use irec_endpoint_control
      use grx_planning_routines
      use spindriftlib
      use prod_arrays_dimensions
      use sizecom
      use globecom
      use wh_shared_data
      use mk_fileio
      use wh_shared_data
      use end_routine

      SAVE
      CHARACTER (LEN=2) :: R_MKROUP_OL
      LOGICAL (KIND=1) :: SAVE_MK_FILE_EXISTS=.FALSE.
      INTEGER (KIND=4) :: VALUES_2_ZERO
      INTEGER (KIND=2) :: DELETE,CURRENT_RECORD,MARKET_GROUP
      INTEGER (KIND=2) :: MARKET_GROUPS_RECORDS,R_MARKET_GROUP
      INTEGER (KIND=2) :: MAX_MARKET_GROUPS=599 ! CHANGED TO 0:599 041304
      INTEGER (KIND=2) :: SAVE_MARKET_GROUPS_RECORDS=0,GET_MARKET_GROUPS
      integer (kind=2) :: tgid
      INTEGER (KIND=2) :: TEMP_I,I,HG
      LOGICAL (KIND=1) :: READ_MARKET_GROUPS_DATA
      LOGICAL (KIND=1) :: MARKET_GROUP_ACTIVE_SWITCH
      LOGICAL (KIND=1) :: GET_MARKET_SPINNING_CAPACITY
      LOGICAL (KIND=1) :: GET_MARKET_RAMP_RATES
      LOGICAL (KIND=1) :: HYDRO_AGGREGATION=.FALSE.
      REAL (KIND=4) :: R_MARKET_AREA_INDEX,SCENARIO_NUMBER
      REAL (KIND=4) :: GET_SCENARIO_LOAD_SHAPE
      LOGICAL (KIND=4) :: MK_FILE_EXISTS


      CHARACTER (LEN=3) :: R_GET_HOURLY_PRICE_NAME
      CHARACTER (LEN=20) :: R_GET_MARKETACTION_GROUP_NAME
      CHARACTER (LEN=35) :: GET_GROUP_NAME
      LOGICAL (KIND=1) :: GET_TF_HOURLY_PRICE_NAME,GET_TF_GROUP_NAME
      LOGICAL (KIND=1) :: GET_TF_MARKETACTION_GROUP_NAME
      LOGICAL (KIND=1) :: GET_TF_MARKET_GROUP_NOX_SEASON
      LOGICAL (KIND=1) :: YES_PORT_MARKET_AREAS_REPORT
      LOGICAL (KIND=1) :: PORT_MARKET_AREAS_REPORT
      LOGICAL (KIND=1) :: PORT_MARKET_AREAS_REPORT_OPEN
!
! SIMULATION VARIABLES
!
!
! SIMULATION VARIABLES
!
      LOGICAL (KIND=1) :: IN_ACTIVE_MARKET_AREA
      LOGICAL (KIND=1) :: IN_ACTIVE_THERMAL_MARKET_AREA
      LOGICAL (KIND=1) :: IN_ACTIVE_HYDRO_MARKET_AREA
      LOGICAL (KIND=1) :: IN_ACTIVE_LOADS_MARKET_AREA
      LOGICAL (KIND=1) :: IN_ACTIVE_CONTR_MARKET_AREA
      LOGICAL (KIND=1) :: REPORT_MARKET_AREA
      LOGICAL (KIND=1) :: GET_PORTFOLIO_MARKET_AREA_NAME
      LOGICAL (KIND=1) :: GET_PORT_MARKET_AREA_ABBREV
      LOGICAL (KIND=1) :: MOD_TRANS_GROUP_ACTIVE_SWITCH
      INTEGER (KIND=2) :: USE_MARKET_AREA(0:599)
      INTEGER (KIND=2) :: MARKET_AREA_POSITION(0:599)
      INTEGER (KIND=2) :: MARKET_AREA_LOOKUP,LOCAL_INDEX,TG_POSITION
      INTEGER (KIND=2) :: GET_TRANS_GROUP_POSITION
      integer :: ios
      CHARACTER (LEN=1) :: MARKET_AREA_ACTIVE(:)
      CHARACTER (LEN=1) :: REPORT_THIS_GROUP(0:599)
      CHARACTER (LEN=1) :: TEMP_REPORT_THIS_GROUP,QUOTE='"'
      CHARACTER (LEN=1) :: MARKET_LOADS_ACTIVE(0:599)
      CHARACTER (LEN=1) :: REPORT_MARKET_LOADS(0:599)
      CHARACTER (LEN=1) :: MARKET_THERMAL_ACTIVE(0:599)
      CHARACTER (LEN=1) :: REPORT_MARKET_THERMAL(0:599)
      CHARACTER (LEN=1) :: MARKET_HYDRO_ACTIVE(0:599)
      CHARACTER (LEN=1) :: REPORT_MARKET_HYDRO(0:599)
      CHARACTER (LEN=1) :: MARKET_CONTRACTS_ACTIVE(0:599)
      CHARACTER (LEN=1) :: REPORT_MARKET_CONTRACTS(0:599)
      CHARACTER (LEN=1) :: TEMP_MARKET_LOADS_ACTIVE
      CHARACTER (LEN=1) :: TEMP_REPORT_MARKET_LOADS
      CHARACTER (LEN=1) :: TEMP_MARKET_THERMAL_ACTIVE
      CHARACTER (LEN=1) :: TEMP_REPORT_MARKET_THERMAL
      CHARACTER (LEN=1) :: TEMP_MARKET_HYDRO_ACTIVE
      CHARACTER (LEN=1) :: TEMP_REPORT_MARKET_HYDRO
      CHARACTER (LEN=1) :: TEMP_MARKET_CONTRACTS_ACTIVE
      CHARACTER (LEN=1) :: TEMP_REPORT_MARKET_CONTRACTS
      CHARACTER (LEN=5) :: LOCAL_NAME,GET_SCENAME
      CHARACTER (LEN=6) :: PORTFOLIO_MARKET_AREA_ABBREV(:)
      CHARACTER (LEN=6) :: PORTFOLIO_MARKET_AREA_ID(:)
      CHARACTER (LEN=6) :: R_BASECASE_MARKET_AREA_ID
      CHARACTER (LEN=6) :: R_PORTFOLIO_MARKET_AREA_ABBREV
      CHARACTER (LEN=64) :: FILE_NAME
      CHARACTER (LEN=20) :: PORTFOLIO_MARKET_AREA_NAME(:)
      CHARACTER (LEN=20) :: R_PORTFOLIO_MARKET_AREA_NAME
      CHARACTER (LEN=1024) :: PW_REC
      
      ALLOCATABLE :: PORTFOLIO_MARKET_AREA_NAME
      ALLOCATABLE :: PORTFOLIO_MARKET_AREA_ABBREV
      ALLOCATABLE :: PORTFOLIO_MARKET_AREA_ID,MARKET_AREA_ACTIVE
      logical :: file_is_open
      integer :: mkunit_read, mkunit_write
      character :: chars8(8)


         READ_MARKET_GROUPS_DATA = .FALSE.
         SAVE_MK_FILE_EXISTS = .FALSE.
         

         USE_MARKET_AREA = 1 ! DEFAULT IS "UP"
         
         MARKET_AREA_POSITION = 0
         
!        TODO:  convert from subroutine (ENTRY) to function;
!        replace local variable MK_FILE_EXISTS with call.
         CALL DOES_MK_FILE_EXIST(MK_FILE_EXISTS)
         IF(.NOT. MK_FILE_EXISTS) then
            RETURN
         end if

         CALL GET_MARKET_GROUPS_RECORDS(MARKET_GROUPS_RECORDS)

         SAVE_MARKET_GROUPS_RECORDS = MARKET_GROUPS_RECORDS

!
!         CALL open_mk_file_read
         mkunit_read=open_mk_file_read(r_mkroup_ol, kroup_lrecl)
         
         IF(ALLOCATED(PORTFOLIO_MARKET_AREA_NAME)) &
                  DEALLOCATE( &
                     PORTFOLIO_MARKET_AREA_NAME, &
                     PORTFOLIO_MARKET_AREA_ABBREV, &
                     PORTFOLIO_MARKET_AREA_ID, &
                     MARKET_AREA_ACTIVE)
         ALLOCATE( &
                     PORTFOLIO_MARKET_AREA_NAME(MAX_MARKET_GROUPS), &
                     PORTFOLIO_MARKET_AREA_ABBREV(MAX_MARKET_GROUPS), &
                     PORTFOLIO_MARKET_AREA_ID(MAX_MARKET_GROUPS), &
                     MARKET_AREA_ACTIVE(MAX_MARKET_GROUPS))
!
         PORT_MARKET_AREAS_REPORT = YES_PORT_MARKET_AREAS_REPORT()
!
         MARKET_THERMAL_ACTIVE = 'T'

         FILE_NAME=get_filename_from_unit(mkunit_read) ! for debugging
         if(MARKET_GROUPS_RECORDS>0) then
          inquire(unit=mkunit_read, opened=file_is_open)
          if(.not. file_is_open) then
            call end_program("wh_objt:0004 - Attempt was " // &
            "made to read a unit which is not open.")
          end if
         end if
         
         DO CURRENT_RECORD = 1, MARKET_GROUPS_RECORDS

         ! write is at wh_objt:0001
!          wh_objt:0003

            READ(mkunit_read,REC=CURRENT_RECORD) DELETE, &
               PORTFOLIO_MARKET_AREA_NAME(CURRENT_RECORD), &
               PORTFOLIO_MARKET_AREA_ABBREV(CURRENT_RECORD), &
               PORTFOLIO_MARKET_AREA_ID(CURRENT_RECORD), &
               MARKET_AREA_ACTIVE(CURRENT_RECORD), &
               TEMP_REPORT_THIS_GROUP, &
               tgid, &
               TEMP_MARKET_LOADS_ACTIVE, &
               TEMP_REPORT_MARKET_LOADS, &
               TEMP_MARKET_THERMAL_ACTIVE, &
               TEMP_REPORT_MARKET_THERMAL, &
               TEMP_MARKET_HYDRO_ACTIVE, &
               TEMP_REPORT_MARKET_HYDRO, &
               TEMP_MARKET_CONTRACTS_ACTIVE, &
               TEMP_REPORT_MARKET_CONTRACTS
            

                  

            IF(.NOT. &
              MOD_TRANS_GROUP_ACTIVE_SWITCH(tgid)) then
                CYCLE
            end if
            LOCAL_NAME = &
                         PORTFOLIO_MARKET_AREA_ID(CURRENT_RECORD)
            LOCAL_INDEX = MARKET_AREA_LOOKUP(LOCAL_NAME)
            IF(LOCAL_INDEX >= 0 .AND. LOCAL_INDEX < 600) THEN
               MARKET_AREA_POSITION(LOCAL_INDEX) = CURRENT_RECORD
            ENDIF
            IF(LOCAL_INDEX >= 0 .AND. LOCAL_INDEX < 600) THEN
               IF(MARKET_AREA_ACTIVE(CURRENT_RECORD) == 'F') THEN
                  USE_MARKET_AREA(LOCAL_INDEX) = 0
               ENDIF
               REPORT_THIS_GROUP(LOCAL_INDEX) = TEMP_REPORT_THIS_GROUP
!
               MARKET_LOADS_ACTIVE(LOCAL_INDEX) = &
                                                TEMP_MARKET_LOADS_ACTIVE
               REPORT_MARKET_LOADS(LOCAL_INDEX) = &
                                                TEMP_REPORT_MARKET_LOADS
               MARKET_THERMAL_ACTIVE(LOCAL_INDEX) = &
                                              TEMP_MARKET_THERMAL_ACTIVE
               REPORT_MARKET_THERMAL(LOCAL_INDEX) = &
                                              TEMP_REPORT_MARKET_THERMAL
               MARKET_HYDRO_ACTIVE(LOCAL_INDEX) = &
                                                TEMP_MARKET_HYDRO_ACTIVE
               REPORT_MARKET_HYDRO(LOCAL_INDEX) = &
                                                TEMP_REPORT_MARKET_HYDRO
               MARKET_CONTRACTS_ACTIVE(LOCAL_INDEX) = &
                                            TEMP_MARKET_CONTRACTS_ACTIVE
               REPORT_MARKET_CONTRACTS(LOCAL_INDEX) = &
                                            TEMP_REPORT_MARKET_CONTRACTS
!
            ELSE
               WRITE(4,*) "BAD MARKET AREA ID IN THE MARKET GROUPS FILE"
               WRITE(4,*) "MARKET AREA ID = ",LOCAL_NAME
            ENDIF
            IF( MARKET_AREA_ACTIVE(CURRENT_RECORD) /= 'F') THEN

               IF(PORT_MARKET_AREAS_REPORT) THEN
                  IF(.NOT. PORT_MARKET_AREAS_REPORT_OPEN) THEN
                     PORT_MARKET_AREAS_REPORT_OPEN = .TRUE.
                     FILE_NAME = 'PMB'//TRIM(GET_SCENAME())//'.CSV'
                     OPEN(4446,FILE=FILE_NAME)
                     WRITE(4446,4445) &
                        'ENDPOINT,'// &
                        'MARKET_AREA_ID,'// &
                        'MARKET_AREA_NAME,'// &
                        'MARKET_AREA_ABBREV,'// &
                        'TRANS_GROUP_ID,'// &
                        'TRANS_GROUP_NAME,'// &
                        'LOAD_SHAPE_YEAR,'
!
                     SCENARIO_NUMBER = &
                              GET_SCENARIO_LOAD_SHAPE(INT(1,2),INT(1,2))
!
                  ENDIF
                  PW_REC = ' '
!                  WRITE(PW_REC,4447)
                  TG_POSITION = MAX(INT(1,2), &
                         GET_TRANS_GROUP_POSITION(tgid))
                  WRITE(4446,4447) &
                     PRT_ENDPOINT(),',', &
                     FLOAT(LOCAL_INDEX),',', &
                     QUOTE, &
                        PORTFOLIO_MARKET_AREA_NAME(CURRENT_RECORD), &
                           QUOTE,',', &
                     QUOTE, &
                        PORTFOLIO_MARKET_AREA_ABBREV(CURRENT_RECORD), &
                           QUOTE,',', &
                     FLOAT(tgid),',', &
                     QUOTE, &
                        GET_GROUP_NAME(TG_POSITION), &
                           QUOTE,',', &
                     SCENARIO_NUMBER,','
!

               ENDIF
            ENDIF

         ENDDO
         IF(PORT_MARKET_AREAS_REPORT) THEN
            CLOSE(4446)
         ENDIF
!
         CALL CLOSE_MK_FILE_read
         SAVE_MK_FILE_EXISTS = .TRUE.
         READ_MARKET_GROUPS_DATA = .TRUE.
      RETURN
4445  FORMAT(1X,A)
4447  FORMAT (           1X,F5.0,   A, &
                         1X,F5.0,   A, &
                         1X,A,A,A,A, &
                         1X,A,A,A,A, &
                         1X,F5.0,   A, &
                         1X,A,A,A,A, &
                         1X,F6.0,   A)

      ENTRY GET_PORTFOLIO_MARKET_AREA_NAME(R_PORTFOLIO_MARKET_AREA_NAME, &
                                           R_MARKET_AREA_INDEX)

         IF(SAVE_MK_FILE_EXISTS) THEN

            LOCAL_INDEX = INT(R_MARKET_AREA_INDEX)
            IF(LOCAL_INDEX >= 0 .AND. LOCAL_INDEX < 600) THEN
               GET_PORTFOLIO_MARKET_AREA_NAME = .TRUE.
! NOTE: DOUBLE LOCAL INDEX
               LOCAL_INDEX = MARKET_AREA_POSITION(LOCAL_INDEX)
               R_PORTFOLIO_MARKET_AREA_NAME = &
                             PORTFOLIO_MARKET_AREA_NAME(LOCAL_INDEX)
            ELSE
               GET_PORTFOLIO_MARKET_AREA_NAME = .FALSE.
               R_PORTFOLIO_MARKET_AREA_NAME =  'PORTF NAME NOT FOUND'
            ENDIF
         ELSE
            GET_PORTFOLIO_MARKET_AREA_NAME = .FALSE.
            R_PORTFOLIO_MARKET_AREA_NAME =  'PORTF NAME NOT FOUND'
         ENDIF
      RETURN

      ENTRY GET_PORT_MARKET_AREA_ABBREV( &
                                    R_PORTFOLIO_MARKET_AREA_ABBREV, &
                                    R_MARKET_AREA_INDEX)

         IF(SAVE_MK_FILE_EXISTS) THEN
            LOCAL_INDEX = INT(R_MARKET_AREA_INDEX)
            IF(LOCAL_INDEX >= 0 .AND. LOCAL_INDEX < 600) THEN
               GET_PORT_MARKET_AREA_ABBREV = .TRUE.
! NOTE: DOUBLE LOCAL INDEX
               LOCAL_INDEX = MARKET_AREA_POSITION(LOCAL_INDEX)
               R_PORTFOLIO_MARKET_AREA_ABBREV = &
                             PORTFOLIO_MARKET_AREA_ABBREV(LOCAL_INDEX)
            ELSE
               GET_PORT_MARKET_AREA_ABBREV = .FALSE.
               R_PORTFOLIO_MARKET_AREA_ABBREV =  'NONE  '
            ENDIF
         ELSE
            GET_PORT_MARKET_AREA_ABBREV = .FALSE.
            R_PORTFOLIO_MARKET_AREA_ABBREV =  'NONE  '
         ENDIF
      RETURN

      ENTRY IN_ACTIVE_MARKET_AREA(R_BASECASE_MARKET_AREA_ID)

         IF(SAVE_MK_FILE_EXISTS) THEN
            LOCAL_NAME = R_BASECASE_MARKET_AREA_ID
            LOCAL_INDEX = MARKET_AREA_LOOKUP(LOCAL_NAME)
            IF(LOCAL_INDEX >= 0 .AND. LOCAL_INDEX < 600) THEN
               IN_ACTIVE_MARKET_AREA = &
                                       USE_MARKET_AREA(LOCAL_INDEX) == 1
            ELSE
               IN_ACTIVE_MARKET_AREA = .TRUE.
            ENDIF
         ELSE
            IN_ACTIVE_MARKET_AREA = .TRUE.
         ENDIF
      RETURN

      ENTRY IN_ACTIVE_THERMAL_MARKET_AREA(R_BASECASE_MARKET_AREA_ID)

         IF(SAVE_MK_FILE_EXISTS) THEN
            LOCAL_NAME = R_BASECASE_MARKET_AREA_ID
            LOCAL_INDEX = MARKET_AREA_LOOKUP(LOCAL_NAME)
            IF(LOCAL_INDEX >= 0 .AND. LOCAL_INDEX < 600) THEN
               IN_ACTIVE_THERMAL_MARKET_AREA = &
                              USE_MARKET_AREA(LOCAL_INDEX) == 1 .AND. &
                        MARKET_THERMAL_ACTIVE(LOCAL_INDEX) == 'T'
            ELSE
               IN_ACTIVE_THERMAL_MARKET_AREA = .TRUE.
            ENDIF
         ELSE
            IN_ACTIVE_THERMAL_MARKET_AREA = .TRUE.
         ENDIF
      RETURN

      ENTRY IN_ACTIVE_HYDRO_MARKET_AREA(R_BASECASE_MARKET_AREA_ID)

         IF(SAVE_MK_FILE_EXISTS) THEN
            LOCAL_NAME = R_BASECASE_MARKET_AREA_ID
            LOCAL_INDEX = MARKET_AREA_LOOKUP(LOCAL_NAME)
            IF(LOCAL_INDEX >= 0 .AND. LOCAL_INDEX < 600) THEN
               IN_ACTIVE_HYDRO_MARKET_AREA = &
                              USE_MARKET_AREA(LOCAL_INDEX) == 1 .AND. &
                          MARKET_HYDRO_ACTIVE(LOCAL_INDEX) == 'T'
            ELSE
               IN_ACTIVE_HYDRO_MARKET_AREA = .TRUE.
            ENDIF
         ELSE
            IN_ACTIVE_HYDRO_MARKET_AREA = .TRUE.
         ENDIF
      RETURN

      ENTRY IN_ACTIVE_LOADS_MARKET_AREA(R_BASECASE_MARKET_AREA_ID)

         IF(SAVE_MK_FILE_EXISTS) THEN
            LOCAL_NAME = R_BASECASE_MARKET_AREA_ID
            LOCAL_INDEX = MARKET_AREA_LOOKUP(LOCAL_NAME)
            IF(LOCAL_INDEX >= 0 .AND. LOCAL_INDEX < 600) THEN
               IN_ACTIVE_LOADS_MARKET_AREA = &
                              USE_MARKET_AREA(LOCAL_INDEX) == 1 .AND. &
                          MARKET_LOADS_ACTIVE(LOCAL_INDEX) == 'T'
            ELSE
               IN_ACTIVE_LOADS_MARKET_AREA = .TRUE.
            ENDIF
         ELSE
            IN_ACTIVE_LOADS_MARKET_AREA = .TRUE.
         ENDIF
      RETURN

      ENTRY IN_ACTIVE_CONTR_MARKET_AREA(R_BASECASE_MARKET_AREA_ID)

         IF(SAVE_MK_FILE_EXISTS) THEN
            LOCAL_NAME = R_BASECASE_MARKET_AREA_ID
            LOCAL_INDEX = MARKET_AREA_LOOKUP(LOCAL_NAME)
            IF(LOCAL_INDEX >= 0 .AND. LOCAL_INDEX < 600) THEN
               IN_ACTIVE_CONTR_MARKET_AREA = &
                              USE_MARKET_AREA(LOCAL_INDEX) == 1 .AND. &
                          MARKET_CONTRACTS_ACTIVE(LOCAL_INDEX) == 'T'
            ELSE
               IN_ACTIVE_CONTR_MARKET_AREA = .TRUE.
            ENDIF
         ELSE
            IN_ACTIVE_CONTR_MARKET_AREA = .TRUE.
         ENDIF
      RETURN

      ENTRY REPORT_MARKET_AREA(R_BASECASE_MARKET_AREA_ID)

         IF(SAVE_MK_FILE_EXISTS) THEN
            LOCAL_NAME = R_BASECASE_MARKET_AREA_ID
            LOCAL_INDEX = MARKET_AREA_LOOKUP(LOCAL_NAME)
            IF(LOCAL_INDEX >= 0 .AND. LOCAL_INDEX < 600) THEN
               REPORT_MARKET_AREA = &
                        USE_MARKET_AREA(LOCAL_INDEX) == 1 .AND. &
                                   REPORT_THIS_GROUP(LOCAL_INDEX) /= 'F'
            ELSE
               REPORT_MARKET_AREA = .TRUE.
            ENDIF
         ELSE
            REPORT_MARKET_AREA = .TRUE.
         ENDIF
      RETURN
      END

!
      FUNCTION SUM_24_I4(R_DAILY_LOADS_I4)
       use end_routine
       implicit none

      REAL (KIND=4) :: SUM_24_I4
      INTEGER (KIND=4) :: R_DAILY_LOADS_I4(24)
         SUM_24_I4 = &
                              FLOAT( &
               R_DAILY_LOADS_I4(1) + &
               R_DAILY_LOADS_I4(2) + &
               R_DAILY_LOADS_I4(3) + &
               R_DAILY_LOADS_I4(4) + &
               R_DAILY_LOADS_I4(5) + &
               R_DAILY_LOADS_I4(6) + &
               R_DAILY_LOADS_I4(7) + &
               R_DAILY_LOADS_I4(8) + &
               R_DAILY_LOADS_I4(9) + &
               R_DAILY_LOADS_I4(10) + &
               R_DAILY_LOADS_I4(11) + &
               R_DAILY_LOADS_I4(12) + &
               R_DAILY_LOADS_I4(13) + &
               R_DAILY_LOADS_I4(14) + &
               R_DAILY_LOADS_I4(15) + &
               R_DAILY_LOADS_I4(16) + &
               R_DAILY_LOADS_I4(17) + &
               R_DAILY_LOADS_I4(18) + &
               R_DAILY_LOADS_I4(19) + &
               R_DAILY_LOADS_I4(20) + &
               R_DAILY_LOADS_I4(21) + &
               R_DAILY_LOADS_I4(22) + &
               R_DAILY_LOADS_I4(23) + &
               R_DAILY_LOADS_I4(24))
      RETURN
      END

!
      FUNCTION PEAK_24_I4(R_DAILY_LOADS_I4)
      use end_routine
      implicit none

      REAL (KIND=4) :: PEAK_24_I4
      INTEGER (KIND=4) :: R_DAILY_LOADS_I4(24)
         PEAK_24_I4 =  MAX( &
          R_DAILY_LOADS_I4(1),R_DAILY_LOADS_I4(2),R_DAILY_LOADS_I4(3), &
          R_DAILY_LOADS_I4(4),R_DAILY_LOADS_I4(5),R_DAILY_LOADS_I4(6), &
          R_DAILY_LOADS_I4(7),R_DAILY_LOADS_I4(8),R_DAILY_LOADS_I4(9), &
          R_DAILY_LOADS_I4(10),R_DAILY_LOADS_I4(11), &
          R_DAILY_LOADS_I4(12),R_DAILY_LOADS_I4(13), &
          R_DAILY_LOADS_I4(14),R_DAILY_LOADS_I4(15), &
          R_DAILY_LOADS_I4(16),R_DAILY_LOADS_I4(17), &
          R_DAILY_LOADS_I4(18),R_DAILY_LOADS_I4(19), &
          R_DAILY_LOADS_I4(20),R_DAILY_LOADS_I4(21), &
          R_DAILY_LOADS_I4(22),R_DAILY_LOADS_I4(23), &
                               R_DAILY_LOADS_I4(24))
      RETURN
      END

!
      FUNCTION PEAK_24_R4(R_DAILY_LOADS_R4)
      use end_routine
      implicit none
      REAL (KIND=4) :: PEAK_24_R4,R_DAILY_LOADS_R4(24)
!
         PEAK_24_R4 =  MAX( &
          R_DAILY_LOADS_R4(1),R_DAILY_LOADS_R4(2),R_DAILY_LOADS_R4(3), &
          R_DAILY_LOADS_R4(4),R_DAILY_LOADS_R4(5),R_DAILY_LOADS_R4(6), &
          R_DAILY_LOADS_R4(7),R_DAILY_LOADS_R4(8),R_DAILY_LOADS_R4(9), &
          R_DAILY_LOADS_R4(10),R_DAILY_LOADS_R4(11), &
          R_DAILY_LOADS_R4(12),R_DAILY_LOADS_R4(13), &
          R_DAILY_LOADS_R4(14),R_DAILY_LOADS_R4(15), &
          R_DAILY_LOADS_R4(16),R_DAILY_LOADS_R4(17), &
          R_DAILY_LOADS_R4(18),R_DAILY_LOADS_R4(19), &
          R_DAILY_LOADS_R4(20),R_DAILY_LOADS_R4(21), &
          R_DAILY_LOADS_R4(22),R_DAILY_LOADS_R4(23), &
          R_DAILY_LOADS_R4(24))
      RETURN
      END

!
      FUNCTION BASE_24_I4(R_DAILY_LOADS_I4)
      use end_routine
      implicit none
      REAL (KIND=4) :: BASE_24_I4
      INTEGER (KIND=4) :: R_DAILY_LOADS_I4(24)
         BASE_24_I4 =  MIN( &
         R_DAILY_LOADS_I4(1),&
         R_DAILY_LOADS_I4(2), &
         R_DAILY_LOADS_I4(3), &
         R_DAILY_LOADS_I4(4), &
         R_DAILY_LOADS_I4(5), &
         R_DAILY_LOADS_I4(6), &
         R_DAILY_LOADS_I4(7), &
         R_DAILY_LOADS_I4(8),&
         R_DAILY_LOADS_I4(9), &
         R_DAILY_LOADS_I4(10), &
         R_DAILY_LOADS_I4(11), &
         R_DAILY_LOADS_I4(12), &
         R_DAILY_LOADS_I4(13), &
         R_DAILY_LOADS_I4(14), &
         R_DAILY_LOADS_I4(15), &
         R_DAILY_LOADS_I4(16), &
         R_DAILY_LOADS_I4(17), &
         R_DAILY_LOADS_I4(18), &
         R_DAILY_LOADS_I4(19), &
         R_DAILY_LOADS_I4(20), &
         R_DAILY_LOADS_I4(21), &
         R_DAILY_LOADS_I4(22), &
         R_DAILY_LOADS_I4(23), &
         R_DAILY_LOADS_I4(24))
      RETURN
      END

!
      FUNCTION SUM_24_R4(R_DAILY_LOADS_R4)
!

      REAL (KIND=4) :: SUM_24_R4
      REAL (KIND=4) :: R_DAILY_LOADS_R4(24)
         SUM_24_R4 = &
          R_DAILY_LOADS_R4(1) + &
          R_DAILY_LOADS_R4(2) + &
          R_DAILY_LOADS_R4(3) + &
          R_DAILY_LOADS_R4(4) + &
          R_DAILY_LOADS_R4(5) + &
          R_DAILY_LOADS_R4(6) + &
          R_DAILY_LOADS_R4(7) + &
          R_DAILY_LOADS_R4(8) + &
          R_DAILY_LOADS_R4(9) + &
          R_DAILY_LOADS_R4(10) + &
          R_DAILY_LOADS_R4(11) + &
          R_DAILY_LOADS_R4(12) + &
          R_DAILY_LOADS_R4(13) + &
          R_DAILY_LOADS_R4(14) + &
          R_DAILY_LOADS_R4(15) + &
          R_DAILY_LOADS_R4(16) + &
          R_DAILY_LOADS_R4(17) + &
          R_DAILY_LOADS_R4(18) + &
          R_DAILY_LOADS_R4(19) + &
          R_DAILY_LOADS_R4(20) + &
          R_DAILY_LOADS_R4(21) + &
          R_DAILY_LOADS_R4(22) + &
          R_DAILY_LOADS_R4(23) + &
          R_DAILY_LOADS_R4(24)
      RETURN
      END

!
      FUNCTION BASE_24_R4(R_DAILY_LOADS_R4)
      use end_routine
      implicit none
      REAL (KIND=4) :: BASE_24_R4
      REAL (KIND=4) :: R_DAILY_LOADS_R4(24)
         BASE_24_R4 =  MIN( &
          R_DAILY_LOADS_R4(1),R_DAILY_LOADS_R4(2),R_DAILY_LOADS_R4(3), &
          R_DAILY_LOADS_R4(4),R_DAILY_LOADS_R4(5),R_DAILY_LOADS_R4(6), &
          R_DAILY_LOADS_R4(7),R_DAILY_LOADS_R4(8),R_DAILY_LOADS_R4(9), &
          R_DAILY_LOADS_R4(10),R_DAILY_LOADS_R4(11), &
          R_DAILY_LOADS_R4(12),R_DAILY_LOADS_R4(13), &
          R_DAILY_LOADS_R4(14),R_DAILY_LOADS_R4(15), &
          R_DAILY_LOADS_R4(16),R_DAILY_LOADS_R4(17), &
          R_DAILY_LOADS_R4(18),R_DAILY_LOADS_R4(19), &
          R_DAILY_LOADS_R4(20),R_DAILY_LOADS_R4(21), &
          R_DAILY_LOADS_R4(22),R_DAILY_LOADS_R4(23), &
          R_DAILY_LOADS_R4(24))
      RETURN
      END

      RECURSIVE FUNCTION GET_SCARCITY_INFO(R_TG, &
                                 R_FIRST_CAP, &
                                 R_FIRST_PERCENT, &
                                 R_SECOND_CAP, &
                                 R_SECOND_PERCENT, &
                                 R_THIRD_CAP, &
                                 R_THIRD_PERCENT, &
                                 R_MONTH, &
                                 R_YEAR, &
                                 R_CAPACITY_ADDER, &
                                 R_ADDITIONAL_VALUE, &
                                 R_ADDITIONAL_PERCENT)
      use end_routine
      implicit none
      LOGICAL (KIND=1) :: GET_SCARCITY_INFO,STORE_TG_SCARCITY_INFO
      INTEGER (KIND=2) :: R_TG,R_MONTH,R_YEAR,TRANS_GROUPS_RECORDS,I,J
      INTEGER (KIND=2) :: TRANS_GROUP,TEMP_I,GET_TRANS_GROUP_POSITION
      REAL :: ESCALATED_MONTHLY_VALUE
      REAL :: R_ADDITIONAL_VALUE(7),R_ADDITIONAL_PERCENT(7),R_FIRST_CAP, &
           R_FIRST_PERCENT, &
           R_SECOND_CAP, &
           R_SECOND_PERCENT, &
           R_THIRD_CAP, &
           R_THIRD_PERCENT, &
           R_CAPACITY_ADDER, &
           R_FIRST_CAPACITY_VALUE(0:TRANS_GROUPS_RECORDS), &
           R_FIRST_CAPACITY_PERCENT(0:TRANS_GROUPS_RECORDS), &
           R_SECOND_CAPACITY_VALUE(0:TRANS_GROUPS_RECORDS), &
           R_SECOND_CAPACITY_PERCENT(0:TRANS_GROUPS_RECORDS), &
           R_THIRD_CAPACITY_VALUE(0:TRANS_GROUPS_RECORDS), &
           R_THIRD_CAPACITY_PERCENT(0:TRANS_GROUPS_RECORDS), &
           R_CAPACITY_ADDER_ARRAY(TRANS_GROUPS_RECORDS), &
!      +     R_CAPACITY_ADDER_ARRAY(0:TRANS_GROUPS_RECORDS),
           R_ADDITIONAL_CAPACITY_VALUE(0:TRANS_GROUPS_RECORDS,7), &
           R_ADDITIONAL_CAPACITY_PERCENT(0:TRANS_GROUPS_RECORDS,7), &
           GLOBAL_SCARCITY, &
           GET_GLOBAL_SCARCITY

      REAL FIRST_CAPACITY_VALUE(:), &
           FIRST_CAPACITY_PERCENT(:), &
           SECOND_CAPACITY_VALUE(:), &
           SECOND_CAPACITY_PERCENT(:), &
           THIRD_CAPACITY_VALUE(:), &
           THIRD_CAPACITY_PERCENT(:), &
           CAPACITY_ADDER(:), &
           ADDITIONAL_CAPACITY_VALUE(:,:), &
           ADDITIONAL_CAPACITY_PERCENT(:,:)
      ALLOCATABLE ::  FIRST_CAPACITY_VALUE, &
                      FIRST_CAPACITY_PERCENT, &
                      SECOND_CAPACITY_VALUE, &
                      SECOND_CAPACITY_PERCENT, &
                      THIRD_CAPACITY_VALUE, &
                      THIRD_CAPACITY_PERCENT, &
                      CAPACITY_ADDER, &
                      ADDITIONAL_CAPACITY_VALUE, &
                      ADDITIONAL_CAPACITY_PERCENT
      SAVE FIRST_CAPACITY_VALUE, &
           FIRST_CAPACITY_PERCENT, &
           SECOND_CAPACITY_VALUE, &
           SECOND_CAPACITY_PERCENT, &
           THIRD_CAPACITY_VALUE, &
           THIRD_CAPACITY_PERCENT, &
           CAPACITY_ADDER, &
           ADDITIONAL_CAPACITY_VALUE, &
           ADDITIONAL_CAPACITY_PERCENT
!
         GET_SCARCITY_INFO = .TRUE.
         IF( R_TG < 0 .OR. .NOT. ALLOCATED(FIRST_CAPACITY_VALUE)) THEN
            R_FIRST_CAP = 0.
            R_FIRST_PERCENT = 50.
            R_SECOND_CAP = 0.
            R_SECOND_PERCENT = 80.
            R_THIRD_CAP = 0.
            R_THIRD_PERCENT = 100.
            R_CAPACITY_ADDER = 0.
            RETURN
         ELSEIF(R_TG == 0) THEN
!
! MOVED 5/8/00. return 9/7/01 msg
!
            GLOBAL_SCARCITY = GET_GLOBAL_SCARCITY( &
            FIRST_CAPACITY_VALUE(0),&
            SECOND_CAPACITY_VALUE(0), &
            THIRD_CAPACITY_VALUE(0), &
            FIRST_CAPACITY_PERCENT(0), &
            SECOND_CAPACITY_PERCENT(0), &
            THIRD_CAPACITY_PERCENT(0), &
            ADDITIONAL_CAPACITY_VALUE(0,1), &
            ADDITIONAL_CAPACITY_VALUE(0,2), &
            ADDITIONAL_CAPACITY_VALUE(0,3), &
            ADDITIONAL_CAPACITY_VALUE(0,4), &
            ADDITIONAL_CAPACITY_VALUE(0,5), &
            ADDITIONAL_CAPACITY_VALUE(0,6), &
            ADDITIONAL_CAPACITY_VALUE(0,7), &
            ADDITIONAL_CAPACITY_PERCENT(0,1), &
            ADDITIONAL_CAPACITY_PERCENT(0,2), &
            ADDITIONAL_CAPACITY_PERCENT(0,3), &
            ADDITIONAL_CAPACITY_PERCENT(0,4), &
            ADDITIONAL_CAPACITY_PERCENT(0,5), &
            ADDITIONAL_CAPACITY_PERCENT(0,6), &
            ADDITIONAL_CAPACITY_PERCENT(0,7))

         ENDIF

            IF(R_TG == 0) THEN
               TRANS_GROUP = 0
            ELSE
! TEST POINT FOR MODEL BOMB
               TRANS_GROUP = GET_TRANS_GROUP_POSITION(R_TG)

            ENDIF

            IF(FIRST_CAPACITY_VALUE(TRANS_GROUP) < 0.) THEN
               TEMP_I = INT(ABS(FIRST_CAPACITY_VALUE(TRANS_GROUP)))
               R_FIRST_CAP = ESCALATED_MONTHLY_VALUE(R_FIRST_CAP, &
               TEMP_I,R_YEAR,R_MONTH,INT(1,2))
            ELSE
               R_FIRST_CAP = FIRST_CAPACITY_VALUE(TRANS_GROUP)
            ENDIF
            IF(SECOND_CAPACITY_VALUE(TRANS_GROUP) < 0.) THEN
               TEMP_I = INT(ABS(SECOND_CAPACITY_VALUE(TRANS_GROUP)))
               R_SECOND_CAP = ESCALATED_MONTHLY_VALUE(R_SECOND_CAP, &
               TEMP_I,R_YEAR,R_MONTH,INT(1,2))
            ELSE
               R_SECOND_CAP = SECOND_CAPACITY_VALUE(TRANS_GROUP)
            ENDIF
            IF(THIRD_CAPACITY_VALUE(TRANS_GROUP) < 0.) THEN
               TEMP_I = INT(ABS(THIRD_CAPACITY_VALUE(TRANS_GROUP)))
               R_THIRD_CAP = ESCALATED_MONTHLY_VALUE(R_THIRD_CAP, &
               TEMP_I,R_YEAR,R_MONTH,INT(1,2))
            ELSE
               R_THIRD_CAP = THIRD_CAPACITY_VALUE(TRANS_GROUP)
            ENDIF
!
!            R_FIRST_PERCENT = FIRST_CAPACITY_PERCENT(TRANS_GROUP)
            IF(FIRST_CAPACITY_PERCENT(TRANS_GROUP) < 0.) THEN
               TEMP_I = INT(ABS(FIRST_CAPACITY_PERCENT(TRANS_GROUP)))
               R_FIRST_PERCENT = &
                              ESCALATED_MONTHLY_VALUE(R_FIRST_PERCENT, &
                              TEMP_I,R_YEAR,R_MONTH,INT(1,2))
            ELSE
               R_FIRST_PERCENT = FIRST_CAPACITY_PERCENT(TRANS_GROUP)
            ENDIF
!            R_SECOND_CAP = SECOND_CAPACITY_VALUE(TRANS_GROUP)
!            R_SECOND_PERCENT = SECOND_CAPACITY_PERCENT(TRANS_GROUP)
            IF(SECOND_CAPACITY_PERCENT(TRANS_GROUP) < 0.) THEN
               TEMP_I = INT(ABS(SECOND_CAPACITY_PERCENT(TRANS_GROUP)))
               R_SECOND_PERCENT = &
                              ESCALATED_MONTHLY_VALUE(R_SECOND_PERCENT, &
                              TEMP_I,R_YEAR,R_MONTH,INT(1,2))
            ELSE
               R_SECOND_PERCENT = SECOND_CAPACITY_PERCENT(TRANS_GROUP)
            ENDIF
!            R_THIRD_CAP = THIRD_CAPACITY_VALUE(TRANS_GROUP)
            IF(THIRD_CAPACITY_PERCENT(TRANS_GROUP) < 0.) THEN
               TEMP_I = INT(ABS(THIRD_CAPACITY_PERCENT(TRANS_GROUP)))
               R_THIRD_PERCENT = &
                              ESCALATED_MONTHLY_VALUE(R_THIRD_PERCENT, &
                              TEMP_I,R_YEAR,R_MONTH,INT(1,2))
            ELSE
               R_THIRD_PERCENT = THIRD_CAPACITY_PERCENT(TRANS_GROUP)
            ENDIF
!
!
!
            DO I = 1, 7
               IF(ADDITIONAL_CAPACITY_VALUE(TRANS_GROUP,I) < 0.) THEN
                  TEMP_I = INT(ABS( &
                            ADDITIONAL_CAPACITY_VALUE(TRANS_GROUP,I)))
                  R_ADDITIONAL_VALUE(I) = &
                        ESCALATED_MONTHLY_VALUE(R_ADDITIONAL_VALUE(I), &
                        TEMP_I,R_YEAR,R_MONTH,INT(1,2))
               ELSE
                  R_ADDITIONAL_VALUE(I) = &
                              ADDITIONAL_CAPACITY_VALUE(TRANS_GROUP,I)
               ENDIF
            ENDDO
            DO I = 1, 7
               IF(ADDITIONAL_CAPACITY_PERCENT(TRANS_GROUP,I) < 0.) THEN
                  TEMP_I = INT(ABS( &
                            ADDITIONAL_CAPACITY_PERCENT(TRANS_GROUP,I)))
                  R_ADDITIONAL_PERCENT(I) = &
                        ESCALATED_MONTHLY_VALUE(R_ADDITIONAL_PERCENT(I), &
                        TEMP_I,R_YEAR,R_MONTH,INT(1,2))
               ELSE
                  R_ADDITIONAL_PERCENT(I) = &
                              ADDITIONAL_CAPACITY_PERCENT(TRANS_GROUP,I)
               ENDIF
            ENDDO
!            R_THIRD_PERCENT = THIRD_CAPACITY_PERCENT(TRANS_GROUP)
            IF(TRANS_GROUP == 0) THEN
               R_CAPACITY_ADDER = 0.
            ELSEIF(CAPACITY_ADDER(TRANS_GROUP) < 0.) THEN
               TEMP_I = INT(ABS(CAPACITY_ADDER(TRANS_GROUP)))
               R_CAPACITY_ADDER = &
                              ESCALATED_MONTHLY_VALUE(R_CAPACITY_ADDER, &
                              TEMP_I,R_YEAR,R_MONTH,INT(1,2))
            ELSE
               R_CAPACITY_ADDER = CAPACITY_ADDER(TRANS_GROUP)
            ENDIF
!         ENDIF
!
      RETURN

      ENTRY STORE_TG_SCARCITY_INFO(TRANS_GROUPS_RECORDS, &
                                R_FIRST_CAPACITY_VALUE, &
                                R_FIRST_CAPACITY_PERCENT, &
                                R_SECOND_CAPACITY_VALUE, &
                                R_SECOND_CAPACITY_PERCENT, &
                                R_THIRD_CAPACITY_VALUE, &
                                R_THIRD_CAPACITY_PERCENT, &
                                R_CAPACITY_ADDER_ARRAY, &
                                R_ADDITIONAL_CAPACITY_VALUE, &
                                R_ADDITIONAL_CAPACITY_PERCENT)

      IF(ALLOCATED(CAPACITY_ADDER)) &
                                 DEALLOCATE(FIRST_CAPACITY_VALUE, &
                                            FIRST_CAPACITY_PERCENT, &
                                            SECOND_CAPACITY_VALUE, &
                                            SECOND_CAPACITY_PERCENT, &
                                            THIRD_CAPACITY_VALUE, &
                                            THIRD_CAPACITY_PERCENT, &
                                            CAPACITY_ADDER, &
                                            ADDITIONAL_CAPACITY_VALUE, &
                                            ADDITIONAL_CAPACITY_PERCENT)
      ALLOCATE(CAPACITY_ADDER(0:TRANS_GROUPS_RECORDS), &
               FIRST_CAPACITY_VALUE(0:TRANS_GROUPS_RECORDS), &
               FIRST_CAPACITY_PERCENT(0:TRANS_GROUPS_RECORDS), &
               SECOND_CAPACITY_VALUE(0:TRANS_GROUPS_RECORDS), &
               SECOND_CAPACITY_PERCENT(0:TRANS_GROUPS_RECORDS), &
               THIRD_CAPACITY_VALUE(0:TRANS_GROUPS_RECORDS), &
               THIRD_CAPACITY_PERCENT(0:TRANS_GROUPS_RECORDS), &
               ADDITIONAL_CAPACITY_VALUE(0:TRANS_GROUPS_RECORDS,7), &
               ADDITIONAL_CAPACITY_PERCENT(0:TRANS_GROUPS_RECORDS,7))
!
      DO I = 0, TRANS_GROUPS_RECORDS
         FIRST_CAPACITY_VALUE(I) = R_FIRST_CAPACITY_VALUE(I)
         FIRST_CAPACITY_PERCENT(I) = R_FIRST_CAPACITY_PERCENT(I)
         SECOND_CAPACITY_VALUE(I) = R_SECOND_CAPACITY_VALUE(I)
         SECOND_CAPACITY_PERCENT(I) = R_SECOND_CAPACITY_PERCENT(I)
         THIRD_CAPACITY_VALUE(I) = R_THIRD_CAPACITY_VALUE(I)
         THIRD_CAPACITY_PERCENT(I) = R_THIRD_CAPACITY_PERCENT(I)
         IF(I > 0) THEN
            CAPACITY_ADDER(I) = R_CAPACITY_ADDER_ARRAY(I)
         ENDIF
         DO J = 1, 7
            ADDITIONAL_CAPACITY_VALUE(I,J) = &
                                        R_ADDITIONAL_CAPACITY_VALUE(I,J)
! 8/16/00. GAT. HOW DID THIS EVER WORK ???
!            ADDITIONAL_CAPACITY_PERCENT(I,J) =
!     +                                 ADDITIONAL_CAPACITY_PERCENT(I,J)
            ADDITIONAL_CAPACITY_PERCENT(I,J) = &
                                      R_ADDITIONAL_CAPACITY_PERCENT(I,J)
         ENDDO
      ENDDO
      STORE_TG_SCARCITY_INFO = .TRUE.
      RETURN
      END

!
!                  ROUTINE TO CREATE A REGIONAL OUTAGES FILE

      SUBROUTINE REGIONAL_OUTAGES_OBJECT
      use end_routine, only: end_program, er_message
      use filename_tracker
      use grx_planning_routines
      use spindriftlib
      use prod_arrays_dimensions
      use sizecom
      use mod_base_year

      INTEGER (KIND=2) :: REGIONAL_YEAR,TF_YEAR,TEMP_YEAR,year_loc2
      INTEGER (KIND=2) :: STUDY_BASE_YEAR=0
      CHARACTER (LEN=1) :: RECORD_IS_ACTIVE,TIME_FRAME

      REAL (KIND=4) :: MONTHLY_VALUES(12),ANNUAL_VALUE

      CHARACTER (LEN=20) :: REGIONAL_VARIABLE
      INTEGER (KIND=2) :: DELETE,INUNIT,IREC,LRECL=81
      INTEGER :: IOS
      INTEGER (KIND=2) :: UNIT_NUM=10,OUTAGES_TABLES_IN_FILE=0
      INTEGER (KIND=2) :: R_OUTAGES_TABLES,TRANSACTION_GROUP
      CHARACTER (LEN=5) :: BASE_FILE_NAME,OVERLAY_FAMILY_NAME
      CHARACTER (LEN=5) :: REGIONAL_OUTAGES_FILE
      CHARACTER (LEN=256) :: FILE_NAME
      CHARACTER (LEN=256) :: BASE_FILE_DIRECTORY
      CHARACTER (LEN=256) :: DATA_DRIVE,OUTPUT_DIRECTORY
      LOGICAL (KIND=4) :: FILE_EXISTS,REG_OUTAGES_FILE_EXISTS=.FALSE.
      LOGICAL (KIND=4) :: R_REG_OUTAGES_FILE_EXISTS
!
!
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER (LEN=1024) :: RECLN
! DECLARATION FOR DAY TYPE DETERMINANTS
      CHARACTER (LEN=16) :: FILE_TYPE='REGIONAL OUTAGES  '
      CHARACTER (LEN=2) :: REGIONAL_OUTAGES_OL='BC'
      LOGICAL (KIND=1) :: LAHEY_LF95
      CHARACTER (LEN=30) :: SCREEN_OUTPUT

! CONVERT THE DAY_TYPE FILE

      ENTRY REGIONAL_OUTAGES_MAKEBIN

      STUDY_BASE_YEAR = get_base_year()
      TF_YEAR = 0

      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME = get_rob_filename(base_file_name)
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      REG_OUTAGES_FILE_EXISTS = FILE_EXISTS
      OUTAGES_TABLES_IN_FILE = 0
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_LOCATE_WRITE(16,30,BASE_FILE_NAME,ALL_VERSIONS,0)
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         ENDIF

         OPEN(10,FILE=FILE_NAME) ! rob
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCRGOUT.BIN", &
                            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)

         IREC = 0
         READ(10,*) DELETE
         DO ! TABLES
            TF_YEAR = 1
            DO ! YEAR-BASED RECORDS
               READ(10,1000,IOSTAT=IOS) RECLN
!
               IF(RECLN(1:1) == '7' .OR. IOS /= 0) THEN ! END OF TABLE ! EXIT AT BOTTOM OF IF
                  IF(TF_YEAR <= AVAIL_DATA_YEARS) THEN
                     DO TF_YEAR = TF_YEAR, AVAIL_DATA_YEARS
                        IREC = IREC + 1
                        WRITE(11,REC=IREC) DELETE, &
                           TF_YEAR+STUDY_BASE_YEAR, & !  INT2
                           RECORD_IS_ACTIVE, &
                           TRANSACTION_GROUP, &
                           REGIONAL_VARIABLE, &
                           TIME_FRAME, &
                           ANNUAL_VALUE, &
                           MONTHLY_VALUES
                     ENDDO
                  ENDIF ! DETECTED NEW TABLE OR END OF FILE
!
                  EXIT ! LEAVE LOOP (GO TO NEXT TABLE OR EXIT ROUTINE)
!
               ENDIF
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,ERR=200) DELETE, &
                           REGIONAL_YEAR, &
                           RECORD_IS_ACTIVE, &
                           TRANSACTION_GROUP, &
                           REGIONAL_VARIABLE, &
                           TIME_FRAME, &
                           ANNUAL_VALUE, &
                           MONTHLY_VALUES
!
               IREC = IREC + 1
!
               READ(RECLN,*,ERR=200) DELETE,year_loc2
               TEMP_YEAR = MIN(AVAIL_DATA_YEARS,year_loc2 - STUDY_BASE_YEAR)
               IF(TEMP_YEAR > TF_YEAR) THEN ! FILL IN MISSING YEARS
                  IF(IREC == 1) THEN
                     WRITE(4,*) "The first year of the first table in"
                     WRITE(4,*) "the REGIONAL OUTAGES file is ",year_loc2
                     WRITE(4,*) "while the base year set in project"
                     WRITE(4,*) "information is ",STUDY_BASE_YEAR
                     WRITE(4,*) "First forecast year in the Transact"
                     WRITE(4,*) "Forecast file first year must be one"
                     WRITE(4,*) "year greater than the base year."
                     WRITE(4,*) " "

                  ENDIF
                  DO TF_YEAR = TF_YEAR, TEMP_YEAR - 1
                     WRITE(11,REC=IREC) DELETE, &
                           TF_YEAR+STUDY_BASE_YEAR, & !  INT2
                           RECORD_IS_ACTIVE, &
                           TRANSACTION_GROUP, &
                           REGIONAL_VARIABLE, &
                           TIME_FRAME, &
                           ANNUAL_VALUE, &
                           MONTHLY_VALUES
                     IREC = IREC + 1
                  ENDDO

               ENDIF

               WRITE(11,REC=IREC) DELETE, &
                           TF_YEAR+STUDY_BASE_YEAR, & !  INT2
                           RECORD_IS_ACTIVE, &
                           TRANSACTION_GROUP, &
                           REGIONAL_VARIABLE, &
                           TIME_FRAME, &
                           ANNUAL_VALUE, &
                           MONTHLY_VALUES
               TF_YEAR = TF_YEAR + 1
            ENDDO ! LOAD GROUP
            OUTAGES_TABLES_IN_FILE = OUTAGES_TABLES_IN_FILE + 1
            IF(IOS /= 0) EXIT
         ENDDO ! READ TABLES
         CLOSE(10)
         CLOSE(11)
!
      ELSE IF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "wh_objt:1117")
      ENDIF
      RETURN
! OVERLAY THE DAY TYPE FILE

      ENTRY REGIONAL_OUTAGES_MAKEOVL(OVERLAY_FAMILY_NAME)

      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME = get_roo_filename(data_drive, overlay_family_name)
      OPEN(10,FILE=FILE_NAME) ! Roo
      READ(10,*) DELETE
      INUNIT = 12
      IF(REGIONAL_OUTAGES_OL == 'BC') THEN
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCRGOUT.BIN", &
                                             ACCESS="DIRECT",RECL=LRECL)
         INUNIT = 11
      ENDIF
      OPEN(12,FILE=trim(DATA_DRIVE)//"OLRGOUT.BIN",ACCESS="DIRECT", &
                                            STATUS="UNKNOWN",RECL=LRECL)
      IREC = 0
!
      TF_YEAR = 0
      READ(10,1000,IOSTAT=IOS) RECLN
      DO WHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE
!
         READ(10,1000,IOSTAT=IOS) RECLN
!
      ENDDO
!
      READ(RECLN,*,ERR=200) DELETE,TF_YEAR
!
      DO ! TABLES AND YEARS, COUNTING IS DONE INSIDE
         IREC = IREC + 1
         READ(INUNIT,REC=IREC,IOSTAT=IOS) &
                           DELETE, &
                           REGIONAL_YEAR, &
                           RECORD_IS_ACTIVE, &
                           TRANSACTION_GROUP, &
                           REGIONAL_VARIABLE, &
                           TIME_FRAME, &
                           ANNUAL_VALUE, &
                           MONTHLY_VALUES
         IF(IOS /= 0) EXIT ! END OF BINARY FILE
!
         IF(REGIONAL_YEAR == TF_YEAR) THEN
            RECLN = trim(RECLN)//',,,,,,,,,,,,,'
            READ(RECLN,*,ERR=200) &
                           DELETE, &
                           REGIONAL_YEAR, &
                           RECORD_IS_ACTIVE, &
                           TRANSACTION_GROUP, &
                           REGIONAL_VARIABLE, &
                           TIME_FRAME, &
                           ANNUAL_VALUE, &
                           MONTHLY_VALUES
            READ(10,1000,IOSTAT=IOS) RECLN
!
            DO WHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE
!
               READ(10,1000,IOSTAT=IOS) RECLN
!
            ENDDO
            READ(RECLN,*,ERR=200) DELETE,TF_YEAR
         ENDIF
!
         WRITE(12,REC=IREC)  DELETE, &
                           REGIONAL_YEAR, &
                           RECORD_IS_ACTIVE, &
                           TRANSACTION_GROUP, &
                           REGIONAL_VARIABLE, &
                           TIME_FRAME, &
                           ANNUAL_VALUE, &
                           MONTHLY_VALUES
      ENDDO
      CLOSE(10)
      CLOSE(12)

      IF(REGIONAL_OUTAGES_OL == 'BC') CLOSE(11)
      REGIONAL_OUTAGES_OL = 'OL'
      RETURN

  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from wh_objt SIID359'
      call end_program(er_message)

  300 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      CALL MG_LOCATE_WRITE(21,0, &
                  'Error reading the Regional Outage record. Look for'// &
                                          ' a "," in a character name.', &
                           ALL_VERSIONS,1)
      er_message='stop requested from wh_objt SIID360'
      call end_program(er_message)


      ENTRY RESET_REGIONAL_OUTAGES_OL

         REGIONAL_OUTAGES_OL = 'BC'
      RETURN


      ENTRY OPEN_REGIONAL_OUTAGES_FILE

         OPEN(UNIT_NUM, &
               FILE=trim(OUTPUT_DIRECTORY())//REGIONAL_OUTAGES_OL// &
               "RGOUT.BIN",ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      RETURN


      ENTRY CLOSE_REGIONAL_OUTAGES_FILE

         CLOSE(UNIT_NUM)
      RETURN

      ENTRY DOES_REG_OUTAGES_FILE_EXIST(R_REG_OUTAGES_FILE_EXISTS)

         R_REG_OUTAGES_FILE_EXISTS = REG_OUTAGES_FILE_EXISTS
      RETURN

      ENTRY GET_OUTAGES_TABLES(R_OUTAGES_TABLES)

         R_OUTAGES_TABLES = OUTAGES_TABLES_IN_FILE
      RETURN
!
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END
!
!        PROGRAM TO READ MULTI-TAB INFORMATION ON GLOBAL REGIONALS
!        FOR FORWARD PRICE DEVELOPMENT AND CONVERT TO BINARY FORMAT
!                       COPYRIGHT (C) 1998
!           ALL RIGHTS RESERVED M.S. GERBER & ASSOCIATES, INC.
!  READ FOR EACH ENDPOINT
!
!
      FUNCTION READ_REGIONAL_OUTAGES_DATA()

      USE SIZECOM
      use SpinDriftLib
      USE PROD_ARRAYS_DIMENSIONS
      use end_routine
      implicit none
      SAVE
      LOGICAL (KIND=1) :: READ_REGIONAL_OUTAGES_DATA
      LOGICAL (KIND=1) :: SAVE_REGIONAL_OUTAGES_STATUS=.FALSE.
      LOGICAL (KIND=1) :: REGIONAL_OUTAGES_ACTIVE
      LOGICAL (KIND=4) :: REG_OUTAGES_FILE_EXISTS
      INTEGER (KIND=2) :: YR,OUTAGES_TABLES=0,DELETE,TRANSACTION_GROUP
      INTEGER (KIND=2) :: R_MONTH,OUTAGES_MONTHS,R_YEAR,R_TG,MO
      INTEGER (KIND=2) :: LOCAL_MONTHS,LOCAL_YEARS,NUM_SCEN_VAR,IREC
      INTEGER (KIND=2) :: UPPER_TRANS_GROUP,GET_NUMBER_OF_ACTIVE_GROUPS
      INTEGER (KIND=2) :: MAX_REGIONAL_INDEX=6,TG
      INTEGER (KIND=2) :: GET_TRANS_GROUP_POSITION
      INTEGER (KIND=2) :: FUEL_INDEX,R_FUEL_INDEX
! INPUT DATA LIST
      INTEGER (KIND=4) :: VALUES_2_ZERO

      INTEGER (KIND=2) :: REGIONAL_YEAR,TABLE,REGIONAL_INDEX,LOAD_N
      CHARACTER (LEN=1) :: RECORD_IS_ACTIVE,TEMP_TIME_FRAME
      CHARACTER (LEN=20) :: REGIONAL_VARIABLE
      REAL (KIND=4) :: ANNUAL_VALUE,MONTHLY_VALUES(12)
!     +                  GET_MONTHLY_REGIONAL_OUTAGE,
      REAL (KIND=4) :: SCEN_OUTAGES_VARIABLE(:,:,:,:)
      ALLOCATABLE :: SCEN_OUTAGES_VARIABLE
!
!
! END DATA DECLARATIONS

         READ_REGIONAL_OUTAGES_DATA = .FALSE.
!
         CALL DOES_REG_OUTAGES_FILE_EXIST(REG_OUTAGES_FILE_EXISTS)
         IF(REG_OUTAGES_FILE_EXISTS) THEN

            CALL OPEN_REGIONAL_OUTAGES_FILE
!
            CALL GET_OUTAGES_TABLES(OUTAGES_TABLES)
            OUTAGES_MONTHS = 360
            LOCAL_MONTHS = 12
            LOCAL_YEARS = AVAIL_DATA_YEARS

            UPPER_TRANS_GROUP = GET_NUMBER_OF_ACTIVE_GROUPS()

            NUM_SCEN_VAR = 18
            IF(OUTAGES_MONTHS <= 0) THEN
            ENDIF
            IF( ALLOCATED(SCEN_OUTAGES_VARIABLE) ) &
                      DEALLOCATE(SCEN_OUTAGES_VARIABLE)
            ALLOCATE( &
                  SCEN_OUTAGES_VARIABLE(  MAX_REGIONAL_INDEX, &
                                          UPPER_TRANS_GROUP, &
                                          LOCAL_MONTHS, &
                                          LOCAL_YEARS))
!
!

            SCEN_OUTAGES_VARIABLE = 1.

            DO TABLE = 1, OUTAGES_TABLES
               DO YR = 1, AVAIL_DATA_YEARS
                  IREC = (TABLE-1)*AVAIL_DATA_YEARS + YR
                  READ(10,REC=IREC) DELETE, &
                           REGIONAL_YEAR, &
                           RECORD_IS_ACTIVE, &
                           TRANSACTION_GROUP, &
                           REGIONAL_VARIABLE, & !  TRANSACTION GROUP NUMBER
                           TEMP_TIME_FRAME, &
                           ANNUAL_VALUE, &
                           MONTHLY_VALUES
!
                  IF(RECORD_IS_ACTIVE == 'F' .OR. &
                                          RECORD_IS_ACTIVE == 'N') CYCLE
!
                  SELECT CASE(trim(REGIONAL_VARIABLE))
                     CASE ('Coal')
                        FUEL_INDEX = 1
                     CASE ('Gas')
                        FUEL_INDEX = 2
                     CASE ('Oil')
                        FUEL_INDEX = 3
                     CASE ('Uranium')
                        FUEL_INDEX = 4
                     CASE ('Hydro')
                        FUEL_INDEX = 5
                     CASE ('Other')
                        FUEL_INDEX = 6
                     CASE (' ')
                        FUEL_INDEX = 6
                  END SELECT

                  TG = GET_TRANS_GROUP_POSITION(TRANSACTION_GROUP)

                  REGIONAL_INDEX = FUEL_INDEX*TG

                  IF (REGIONAL_INDEX <= 0) CYCLE
!
                  DO MO = 1, 12
                     IF(TEMP_TIME_FRAME == 'A') THEN
                        SCEN_OUTAGES_VARIABLE(FUEL_INDEX,TG,MO,YR) = &
                           SCEN_OUTAGES_VARIABLE(FUEL_INDEX,TG,MO,YR) * &
                                                            ANNUAL_VALUE
                     ELSEIF(TEMP_TIME_FRAME == 'M') THEN
                        SCEN_OUTAGES_VARIABLE(FUEL_INDEX,TG,MO,YR) = &
                           SCEN_OUTAGES_VARIABLE(FUEL_INDEX,TG,MO,YR) * &
                                                      MONTHLY_VALUES(MO)
                     ENDIF
                  ENDDO ! MONTHS
               ENDDO ! RECORDS (YEARS)
            ENDDO ! TABLES
!
            READ_REGIONAL_OUTAGES_DATA = .TRUE.
            CALL CLOSE_REGIONAL_OUTAGES_FILE

         ENDIF

         SAVE_REGIONAL_OUTAGES_STATUS = READ_REGIONAL_OUTAGES_DATA
      RETURN

      ENTRY REGIONAL_OUTAGES_ACTIVE()

         REGIONAL_OUTAGES_ACTIVE = SAVE_REGIONAL_OUTAGES_STATUS
      RETURN


      END

!                ROUTINE TO CONVERT USER_DAY FILE
!
!                           COPYRIGHT (C) 1997
!                        M.S. GERBER & ASSOCIATES, INC.
!                           ALL RIGHTS RESERVED

      SUBROUTINE USER_DAY_OBJECT
      use end_routine, only: end_program, er_message
      use filename_tracker

      USE SIZECOM
      use SpinDriftLib
      USE PROD_ARRAYS_DIMENSIONS
      use program_state
      use debugtrace
      SAVE
      
!!! RECORD LENGTH UPDATED WITH VARIABLE CHANGES
      INTEGER (KIND=2) :: DELETE,INUNIT,LRECL=147
      INTEGER (KIND=4) :: IREC,R_MAX_USER_DAYS,R_USER_DAY_PRODUCTS
      INTEGER :: IOS
!
      INTEGER (KIND=2) :: UNIT_NUM=10,HOURS_PER_DAY,USER_DAYS_IN_MONTH
      INTEGER (KIND=2) :: BIN_TABLES_IN_FILE=0,MONTHS_PER_YEAR,DAY
      PARAMETER ( HOURS_PER_DAY=24,USER_DAYS_IN_MONTH=7, &
                  MONTHS_PER_YEAR=12)
      INTEGER (KIND=2) :: FILE_ID,DAY_TYPE_ID,START_MONTH,END_MONTH      !INTEGER*2
      REAL (KIND=4) :: TABLE_VALUE(HOURS_PER_DAY)
      CHARACTER (LEN=5) :: BASE_FILE_NAME,OVERLAY_FAMILY_NAME
      CHARACTER (LEN=5) :: USER_DAY_FILE
      CHARACTER (LEN=1) :: RECORD_ACTIVE
      CHARACTER (LEN=20) :: USER_DAY_NAME
      CHARACTER (LEN=256) :: FILE_NAME
      CHARACTER (LEN=256) :: BASE_FILE_DIRECTORY
      CHARACTER (LEN=256) :: DATA_DRIVE,OUTPUT_DIRECTORY
      LOGICAL (KIND=4) :: FILE_EXISTS,R_USER_DAY_FILE_EXISTS
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER (LEN=1024) :: RECLN
! DECLARATION FOR DAY TYPE DETERMINANTS
      INTEGER (KIND=4), PARAMETER :: MAX_FILES=10
      CHARACTER (LEN=5) :: FILE_BASE_NAMES(0:MAX_FILES-1)
      CHARACTER (LEN=2) :: FILE_CODES(0:MAX_FILES-1)= &
                  (/'UD','D1','D2','D3','D4','D5','D6','D7','D8','D9'/)
      CHARACTER (LEN=2) :: FILE_CODE
      CHARACTER (LEN=6) :: FILE_BINARY_NAMES(0:MAX_FILES-1)=(/ &
                         'USRDY0','USRDY1','USRDY2','USRDY3','USRDY4', &
                         'USRDY5','USRDY6','USRDY7','USRDY8','USRDY9'/), &
                          BINARY_FILE_NAME
      LOGICAL , DIMENSION(0:MAX_FILES-1) :: ACTIVE_BASE_FILES=.FALSE.
      LOGICAL , DIMENSION(0:MAX_FILES-1) :: ACTIVE_OVERLAY_FILES=.FALSE.
      LOGICAL :: USER_DAY_FILE_EXISTS=.FALSE.
      CHARACTER (LEN=16) :: FILE_TYPE='User Day Type   '
      CHARACTER (LEN=2),DIMENSION(0:MAX_FILES-1) :: USER_DAY_OL='BC'
! 032712. CHANGED FOR BURESH.
      INTEGER (KIND=4),DIMENSION(0:MAX_FILES-1):: SAVE_MAX_USER_DAYS=0
      INTEGER (KIND=4),DIMENSION(0:MAX_FILES-1):: SAVE_USER_DAY_PRODUCTS &
                                                           =0
      INTEGER (KIND=4) :: FILE_NO
      CHARACTER (LEN=30) :: SCREEN_OUTPUT
      CHARACTER (LEN=1) :: CAPEX_DAY_TYPE
      integer :: file_trace_udm=0 ! user_day_makebin

! CONVERT THE USER_DAY FILE

      ENTRY USER_DAY_MAKEBIN
      if(file_trace_udm/=0) then
        file_trace_udm=open_trace("user_day_makebin.trace", rq_udm)
      end if
      
      DATA_DRIVE = OUTPUT_DIRECTORY()
      BASE_FILE_NAME = USER_DAY_FILE(FILE_BASE_NAMES)
      DO FILE_ID = 0, MAX_FILES-1
         BASE_FILE_NAME = FILE_BASE_NAMES(FILE_ID)
         IF(INDEX(BASE_FILE_NAME,'NONE') /= 0) CYCLE
         FILE_CODE = FILE_CODES(FILE_ID)
         FILE_NAME = TRIM(BASE_FILE_DIRECTORY())//FILE_CODE// &
                                      "B"//TRIM(BASE_FILE_NAME)//".DAT"
         if(index(file_name, "BCMK")>0) then
            ! Stop here for bcmkroup.bin
            ! file_name=file_name ! Debugstop
         end if
         call write_trace_string(file_trace_udm, "filename", file_name)
         
         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
         ACTIVE_BASE_FILES(FILE_ID) = FILE_EXISTS
         USER_DAY_FILE_EXISTS = FILE_EXISTS .OR. USER_DAY_FILE_EXISTS
         IF(FILE_EXISTS) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
            BINARY_FILE_NAME = FILE_BINARY_NAMES(FILE_ID)

            

            OPEN(10,FILE=FILE_NAME) !/file_code/b/bfn
            READ(10,*) DELETE
            OPEN(11,FILE=TRIM(DATA_DRIVE)//"BC"// &
                         BINARY_FILE_NAME//".BIN",ACCESS="DIRECT", &
                                            STATUS="UNKNOWN",RECL=LRECL)
            IREC = 1

             TABLE_VALUE = 0.
            BIN_TABLES_IN_FILE = 1

            DO
               READ(10,1000,IOSTAT=IOS) RECLN
               IF(IOS /= 0) EXIT
               IF(RECLN(1:1) == '7') THEN ! BEGINNING OF TABLE
                  BIN_TABLES_IN_FILE = BIN_TABLES_IN_FILE + 1
                  CYCLE  ! ADD 3/13/10
               ENDIF
               DO DAY = 1, USER_DAYS_IN_MONTH
! IF ADDED BECAUSE THERE IS VALID INFO IN RECLN FROM READ ABOVE 3/13/10
                  call write_trace_int2(file_trace_udm, "DAY", DAY)

                  IF(DAY /= 1) then
                    READ(10,1000,IOSTAT=IOS) RECLN
                    call write_trace_string(file_trace_udm, "RECLN", RECLN)
                  end if
                  IF(IOS /= 0) then
                  call write_trace_message(file_trace_udm, "IOS/0")
                    EXIT
                  end if

                  RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
                  READ(RECLN,*,ERR=200) DELETE, &
                    USER_DAY_NAME, &
                         TABLE_VALUE, &
                    RECORD_ACTIVE, & ! CHAR*1
                    DAY_TYPE_ID,   & ! INTEGER*2
                    START_MONTH,   & ! INTEGER*2
                    END_MONTH,      & ! INTEGER*2  29
                    CAPEX_DAY_TYPE  ! 30
          if(file_trace_udm/=BAD_TRACE_HANDLE) then
              write(file_trace_udm, *) linefeed, &
              "2. read", linefeed, &
               "USER_DAY_NAME ", USER_DAY_NAME, linefeed, &
               "TABLE_VALUE ", TABLE_VALUE, linefeed, &
               "RECORD_ACTIVE ", RECORD_ACTIVE, linefeed, & 
               "DAY_TYPE_ID ", DAY_TYPE_ID,  linefeed, & 
               "START_MONTH ", START_MONTH, linefeed,  & 
               "END_MONTH ", END_MONTH,  linefeed,    & 
               "CAPEX_DAY_TYPE ", CAPEX_DAY_TYPE, linefeed, &
               
               linefeed, "Writing", linefeed, &
               "USER_DAY_NAME ", USER_DAY_NAME, linefeed, &
               "TABLE_VALUE ", TABLE_VALUE, linefeed, &
               "RECORD_ACTIVE ", RECORD_ACTIVE, linefeed,  & ! CHAR*1
               "DAY_TYPE_ID ", DAY_TYPE_ID, linefeed,    & ! INTEGER*2
               "START_MONTH ", START_MONTH, linefeed,    & ! INTEGER*2
               "END_MONTH ", END_MONTH, linefeed,  &      !INTEGER*2
               "IREC ", IREC, linefeed
               
              

          end if

          
                  WRITE(11,REC=IREC) DELETE, &
                    USER_DAY_NAME, &
                         TABLE_VALUE, &
                    RECORD_ACTIVE, & ! CHAR*1
                    DAY_TYPE_ID,   & ! INTEGER*2
                    START_MONTH,   & ! INTEGER*2
                    END_MONTH      !INTEGER*2
                  IREC = IREC + 1
               ENDDO ! DAYS PER TABLE
            ENDDO ! TABLE
            CLOSE(10)
            CLOSE(11)
         ELSE IF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
            CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "wh_objt:1116")
         ENDIF

         SAVE_USER_DAY_PRODUCTS(FILE_ID) = BIN_TABLES_IN_FILE
         call write_trace_int2(file_trace_udm, "FILE_ID", file_id)
         call write_trace_int2(file_trace_udm, "BIN_TABLES_IN_FILE", &
            BIN_TABLES_IN_FILE)
            

         SAVE_MAX_USER_DAYS(FILE_ID) = IREC - 1
      ENDDO
!
      RETURN



! OVERLAY THE DAY TYPE FILE

      ENTRY USER_DAY_MAKEOVL(OVERLAY_FAMILY_NAME,FILE_NO)

      SCREEN_OUTPUT = TRIM(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
      CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      DATA_DRIVE = OUTPUT_DIRECTORY()
       FILE_NAME = get_o_filename(trim(data_drive), &
        trim(file_codes(file_no)), trim(overlay_family_name))

      OPEN(10,FILE=FILE_NAME) ! o filename
      READ(10,*) DELETE
      INUNIT = 12
      BINARY_FILE_NAME = FILE_BINARY_NAMES(FILE_NO)
      IF(USER_DAY_OL(FILE_NO) == 'BC') THEN
         OPEN(11,FILE=TRIM(DATA_DRIVE)//"BC"//BINARY_FILE_NAME// &
                                      ".BIN",ACCESS="DIRECT",RECL=LRECL)
         INUNIT = 11
      ENDIF
      OPEN(12,FILE=TRIM(DATA_DRIVE)//"OL"//BINARY_FILE_NAME// &
                     ".BIN",ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      IREC = 0
      BIN_TABLES_IN_FILE = 1
      DO
         ! UNLESS A TABLE MARKER THIS IS THE DAY 1 RECORD
         READ(10,1000,IOSTAT=IOS) RECLN  
         IF(IOS /= 0) then
            EXIT
         end if
         IF(RECLN(1:1) == '7') THEN ! BEGINNING OF TABLE
            BIN_TABLES_IN_FILE = BIN_TABLES_IN_FILE + 1
            CYCLE
         ENDIF
         DO DAY = 1, USER_DAYS_IN_MONTH
            IREC = IREC + 1
            READ(INUNIT,REC=IREC,IOSTAT=IOS) DELETE, &
                 USER_DAY_NAME, &
                    TABLE_VALUE, &
                 RECORD_ACTIVE, & ! CHAR*1
                 DAY_TYPE_ID,   & ! INTEGER*2
                 START_MONTH,   & ! INTEGER*2
                 END_MONTH      !INTEGER*2
            IF(IOS /= 0) EXIT
            IF(DAY /= 1) READ(10,1000,IOSTAT=IOS) RECLN
            IF(IOS == 0) THEN
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,ERR=300) DELETE, &
                 USER_DAY_NAME, &
                    TABLE_VALUE, &
                 RECORD_ACTIVE, & ! CHAR*1
                 DAY_TYPE_ID,   & ! INTEGER*2
                 START_MONTH,   & ! INTEGER*2
                 END_MONTH,     & ! INTEGER*2
                 CAPEX_DAY_TYPE  ! 30
            ENDIF
            WRITE(12,REC=IREC) DELETE, &
                 USER_DAY_NAME, &
                    TABLE_VALUE, &
                 RECORD_ACTIVE, & ! CHAR*1
                 DAY_TYPE_ID,   & ! INTEGER*2
                 START_MONTH,   & ! INTEGER*2
                 END_MONTH      !INTEGER*2
         ENDDO
      ENDDO
      CLOSE(10)
      CLOSE(12)
      IF(USER_DAY_OL(FILE_NO) == 'BC') CLOSE(11)
      USER_DAY_OL(FILE_NO) = 'OL'

      IF(IREC /= SAVE_MAX_USER_DAYS(FILE_NO) .OR. &
             SAVE_USER_DAY_PRODUCTS(FILE_NO) /= BIN_TABLES_IN_FILE) THEN
         WRITE(4,*) "USER DAY TYPE OVERLAY COUNT INCONSISTENT WITH BASE"
      ENDIF
!
      RETURN

  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from wh_objt SIID361'
      call end_program(er_message)

  300 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      CALL MG_LOCATE_WRITE(21,0, &
                     'Error reading the User Daytype record. Look for'// &
                                          ' a "," in a character name.', &
                           ALL_VERSIONS,1)
      er_message='stop requested from wh_objt SIID362'
      call end_program(er_message)
!

      ENTRY RESET_USER_DAY_OL

         USER_DAY_OL = 'BC'
      RETURN
!

      ENTRY OPEN_USER_DAY_FILE(FILE_NO,R_USER_DAY_FILE_EXISTS)

         R_USER_DAY_FILE_EXISTS= ACTIVE_BASE_FILES(FILE_NO)
         IF(R_USER_DAY_FILE_EXISTS) &
           OPEN(UNIT_NUM,FILE=TRIM(OUTPUT_DIRECTORY())// &
               USER_DAY_OL(FILE_NO)//FILE_BINARY_NAMES(FILE_NO)//".BIN", &
                            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      RETURN
!

      ENTRY CLOSE_USER_DAY_FILE

         CLOSE(UNIT_NUM)
      RETURN

      ENTRY DOES_USER_DAY_FILE_EXIST(R_USER_DAY_FILE_EXISTS)

         R_USER_DAY_FILE_EXISTS = USER_DAY_FILE_EXISTS
      RETURN

      ENTRY GET_MAX_USER_DAYS(R_MAX_USER_DAYS,R_USER_DAY_PRODUCTS)

         R_MAX_USER_DAYS = MAXVAL(SAVE_MAX_USER_DAYS)
         R_USER_DAY_PRODUCTS = SUM(SAVE_USER_DAY_PRODUCTS)
      RETURN


      ENTRY GET_USER_PRODUCTS_IN_FILE(FILE_NO,R_USER_DAY_PRODUCTS)

         R_USER_DAY_PRODUCTS = SAVE_USER_DAY_PRODUCTS(FILE_NO)
      RETURN

 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END

      FUNCTION READ_USER_DAY_DATA()
      use end_routine
      use SpinDriftLib
      USE PROD_ARRAYS_DIMENSIONS
      use rangechecker
      use debugtrace
      use program_state
      SAVE
      integer :: file_trace_gud=0 ! get_user_day
      integer :: file_trace_rud=0 ! This routine
      integer :: file_trace_mcfu=0 ! MONTHLY_CF_USER_DATA
      CHARACTER (LEN=1) :: RECORD_ACTIVE
      CHARACTER (LEN=20) :: USER_DAY_NAME
      INTEGER (KIND=4) :: VALUES_2_ZERO,CURRENT_RECORD
      INTEGER (KIND=4) :: PRODUCT_INDEX(4096,12),MAX_USER_DAYS
      INTEGER (KIND=4) :: USER_PRODUCTS,FILE_PRODUCTS,USER_DAY_PRODUCTS
      INTEGER (KIND=2) :: PRODUCT
      INTEGER (KIND=2) :: DELETE,HOURS_PER_DAY,USER_DAYS_IN_MONTH
      INTEGER (KIND=2) :: MONTHS_PER_YEAR,MONTH
      integer (kind=2) :: HOUR,R_HOUR
      integer (kind=2) :: USER_DAY
      INTEGER (KIND=2) :: R_DAY_OF_WEEK,R_MONTH,LAST_USER_DAY=1
      INTEGER (KIND=2) :: GET_LAST_USER_DAY,DAY_TYPE_ID,START_MONTH   !INTEGER*2
      INTEGER (KIND=2) :: END_MONTH,R_DAY_TYPE_ID,R_MAX_USER_ID
      INTEGER (KIND=2) :: R_MONTHLY_USER_CF_COUNTER(*),R_CF,MO,I,J,DAY
      INTEGER (KIND=2) :: GET_DAY_OF_WEEK_4,NUMBER_OF_DAYS(7)
      INTEGER (KIND=2) :: DAY_OF_WEEK
      INTEGER (KIND=2) :: DAYS_PER_MONTH(12)=(/31,28,31,30,31,30,31,31, &
                                                           30,31,30,31/)
      PARAMETER ( HOURS_PER_DAY = 24,USER_DAYS_IN_MONTH=7, &
                  MONTHS_PER_YEAR=12)
      INTEGER (KIND=2) :: LOCAL_MONTH,nHrAtMin,HoursAtNonzeroMin
      REAL (KIND=4) :: USER_DAY_DATA(:,:,:,:),CF_USER_DAY_DATA(:,:,:)
      REAL (KIND=4) :: SAVE_USER_DAY_DATA(:,:,:,:)
      REAL (KIND=4) :: SUM_USER_DAY_DATA(:,:) ! 072309.
      REAL (KIND=4) :: GET_SUM_USER_DAY_DATA,GET_USER_DAY
      REAL (KIND=4) :: GET_USER_HOUR_IN_DAY,R_ONE_DAY_DATA(24)
      REAL (KIND=4) :: ONE_DAY_DATA(24),R_HOURS_FOR_PRODUCT
      REAL (KIND=4) :: R_MONTHLY_USER_CF(*),TARGET_CF,TARGET_MINIMUM
      REAL (KIND=4) :: WEEKLY_REFERENCE_LOAD(168),WEEKLY_REFERENCE_BASE
      REAL (KIND=4) :: WEEKLY_NONZERO_BASE,WEEKLY_REFERENCE_PEAK
      REAL (KIND=4) :: WEEKLY_REFERENCE_ENERGY,WEEKLY_LOAD(168)
      REAL (KIND=4) :: MINIMUM_MW,AVERAGE_MW,MAXIMUM_MW,UPPER_CF
      REAL (KIND=4) :: LOWER_CF,SLOPE,INTERCEPT,ENERGY,Tolerance=1e-6
      REAL (KIND=4) :: MaxFeasCF,TEMP_R,DAY_ENERGY(7),MULT
      REAL (KIND=4) :: MAX_LOAD_SCALAR
      ALLOCATABLE :: USER_DAY_DATA,SAVE_USER_DAY_DATA,SUM_USER_DAY_DATA, &
                     CF_USER_DAY_DATA
      LOGICAL (KIND=1) :: READ_USER_DAY_DATA,MONTHLY_CF_USER_DATA
      LOGICAL (KIND=1) :: MONTHLY_USER_DATA_RESET,WEEKLY_SHAPE_FLAT
      LOGICAL (KIND=4) :: USER_DAY_FILE_EXISTS,LOCAL_DAY_FILE_EXISTS
      INTEGER (KIND=4) :: FILE_NO


!
! END DATA DECLARATIONS

        if(file_trace_rud==0) then
            file_trace_rud=open_trace("read_user_day_data.trace",rq_rud)
        end if
        
         CALL DOES_USER_DAY_FILE_EXIST(USER_DAY_FILE_EXISTS)
         call write_trace_bool(file_trace_rud, "USER_DAY_FILE_EXISTS", &
            USER_DAY_FILE_EXISTS)
            
         IF(.NOT. USER_DAY_FILE_EXISTS) RETURN
!
         call write_trace_int4(file_trace_rud, "1. MAX_USER_DAYS", &
            MAX_USER_DAYS)
         call write_trace_int4(file_trace_rud, "1. USER_DAY_PRODUCTS", &
            USER_DAY_PRODUCTS)
            
         CALL GET_MAX_USER_DAYS(MAX_USER_DAYS,USER_DAY_PRODUCTS)
         call write_trace_int4(file_trace_rud, "2. MAX_USER_DAYS", &
            MAX_USER_DAYS)
         call write_trace_int4(file_trace_rud, "2. USER_DAY_PRODUCTS", &
            USER_DAY_PRODUCTS)
!
         IF(ALLOCATED(USER_DAY_DATA)) DEALLOCATE(USER_DAY_DATA, &
                                                 SAVE_USER_DAY_DATA, &
                                                 SUM_USER_DAY_DATA)
         ALLOCATE (USER_DAY_DATA(HOURS_PER_DAY,USER_DAYS_IN_MONTH, &
                                     MONTHS_PER_YEAR,USER_DAY_PRODUCTS), &
                 SAVE_USER_DAY_DATA(HOURS_PER_DAY,USER_DAYS_IN_MONTH, &
                                     MONTHS_PER_YEAR,USER_DAY_PRODUCTS), &
                 SUM_USER_DAY_DATA(0:MONTHS_PER_YEAR,4096))
!
         READ_USER_DAY_DATA = .FALSE.
!
         VALUES_2_ZERO = INT(HOURS_PER_DAY* &
                              USER_DAYS_IN_MONTH* &
                             MONTHS_PER_YEAR* &
                             USER_DAY_PRODUCTS)
!
         SAVE_USER_DAY_DATA = 0.
         SUM_USER_DAY_DATA = 0.
         PRODUCT_INDEX = 0
         USER_PRODUCTS = 0
!
 
         DO FILE_NO = 0, 9 !
            call write_trace_int4(file_trace_rud, "FILE_NO", FILE_NO)
            
            CALL OPEN_USER_DAY_FILE(FILE_NO,LOCAL_DAY_FILE_EXISTS)
            call write_trace_bool(file_trace_rud, "LDFE", &
                LOCAL_DAY_FILE_EXISTS)
                
            IF(.NOT. LOCAL_DAY_FILE_EXISTS) then
                call write_trace_message(file_trace_gud, "1. cycle")
                CYCLE
            end if
            
            CURRENT_RECORD = 0
            CALL GET_USER_PRODUCTS_IN_FILE(FILE_NO,USER_DAY_PRODUCTS)
            call write_trace_int4(file_trace_rud, "USER_DAY_PRODUCTS", &
                USER_DAY_PRODUCTS)
                
            DO FILE_PRODUCTS = 1, USER_DAY_PRODUCTS
               USER_PRODUCTS = USER_PRODUCTS + 1
               call write_trace_int4(file_trace_rud, "USER_PRODUCTS", &
                USER_PRODUCTS)
                
               DO USER_DAY = 1, 7
                  call write_trace_int2(file_trace_rud, "USER_DAY", &
                    user_day)

                  CURRENT_RECORD = CURRENT_RECORD + 1
                  call write_trace_int4(file_trace_rud, "CURRENT_RECORD", &
                    CURRENT_RECORD)
                    
                  READ(10,REC=CURRENT_RECORD) DELETE, &
                    USER_DAY_NAME, &
                           (ONE_DAY_DATA(HOUR),HOUR = 1, HOURS_PER_DAY), &
                    RECORD_ACTIVE, & ! CHAR*1
                    DAY_TYPE_ID,   & ! INTEGER*2
                    START_MONTH,   & ! INTEGER*2
                    END_MONTH      !INTEGER*2

                  if(file_trace_rud/=BAD_TRACE_HANDLE) then
                    write(file_trace_rud, *) " READ... ", &
                    "USER_DAY_NAME ", USER_DAY_NAME, linefeed,  &
                    "ONE_DAY_DATA(HOUR) ", linefeed, &
                    (ONE_DAY_DATA(HOUR),HOUR=1,HOURS_PER_DAY), linefeed, &
                    "RECORD_ACTIVE ", RECORD_ACTIVE, linefeed, & 
                    "DAY_TYPE_ID ", DAY_TYPE_ID, linefeed,  & 
                    "START_MONTH ", START_MONTH,  linefeed, & 
                    "END_MONTH ", END_MONTH, linefeed
                  end if
                call write_trace_string(file_trace_rud, "RECORD_ACTIVE", &
                    RECORD_ACTIVE)
                    

              call write_trace_string(file_trace_rud, "RECORD_ACTIVE",  &
                    RECORD_ACTIVE)
                  IF(RECORD_ACTIVE == 'F') then
                    call write_trace_message(file_trace_rud, "cycle")
                    CYCLE
                  end if
                  
                  DO MO = START_MONTH, END_MONTH
                     IF(CURRENT_RECORD == 9593) THEN
                        CURRENT_RECORD = CURRENT_RECORD
                     ENDIF
                     IF(MO < 1 .OR. MO > 12 .OR. DAY_TYPE_ID < 1 .OR. &
                           DAY_TYPE_ID > 4096) THEN
                        WRITE(4,*) "RECORD = ",CURRENT_RECORD
                        WRITE(4,*) "USER DAY NAME = ",USER_DAY_NAME
                        WRITE(4,*) "RECORD ACTIVE = ",RECORD_ACTIVE
                        WRITE(4,*) "START MONTH = ",START_MONTH
                        WRITE(4,*) "END MONTH = ",END_MONTH
                        WRITE(4,*) "MONTH = ",MO
                        WRITE(4,*) "DAY TYPE ID = ",DAY_TYPE_ID
                        er_message='Stop requested from wh_objt SIID364'
                        call end_program(er_message)
                     ENDIF
                     PRODUCT_INDEX(DAY_TYPE_ID,MO) = USER_PRODUCTS
                     call write_trace_int2(file_trace_rud, "h/d", &
                        HOURS_PER_DAY)
                     DO HOUR = 1, HOURS_PER_DAY
                        call write_trace_int2(file_trace_rud, "HOUR", &
                            hour)
                            
                        SAVE_USER_DAY_DATA(HOUR,USER_DAY,MO, &
                                                        USER_PRODUCTS) = &
                                                      ONE_DAY_DATA(HOUR)
                        SUM_USER_DAY_DATA(MO,DAY_TYPE_ID) = &
                                 SUM_USER_DAY_DATA(MO,DAY_TYPE_ID) + &
                                                      ONE_DAY_DATA(HOUR)
                        SUM_USER_DAY_DATA(0,DAY_TYPE_ID) = &
                                 SUM_USER_DAY_DATA(0,DAY_TYPE_ID) + &
                                                      ONE_DAY_DATA(HOUR)
                     ENDDO
                  ENDDO
               ENDDO
            ENDDO
            CALL CLOSE_USER_DAY_FILE
         ENDDO
         READ_USER_DAY_DATA = .TRUE.
      RETURN

      ENTRY MONTHLY_USER_DATA_RESET

         IF(ALLOCATED(SAVE_USER_DAY_DATA)) THEN
            USER_DAY_DATA = SAVE_USER_DAY_DATA
            MONTHLY_USER_DATA_RESET = .TRUE.
         ELSE
            MONTHLY_USER_DATA_RESET = .FALSE.
         ENDIF
      RETURN
! ***********************************************************************
      ENTRY MONTHLY_CF_USER_DATA(R_MONTH, &
                                 R_MAX_USER_ID, &
                                 R_MONTHLY_USER_CF_COUNTER, &
                                 R_MONTHLY_USER_CF)
      if(file_trace_mcfu==0) then
      file_trace_mcfu=open_trace("monthly_cf_user_data.trace", rq_mcfu)
      end if
      
! ***********************************************************************
! 052312.THE PROBLEM IS ONE PATTERN TO MANY
!        RESOURCES WANTING TO USE THE
!        PATTERN WITH DIFFERENT CF'S
         MONTHLY_CF_USER_DATA = .TRUE.
!
! 082012. SHOULD REPLACE WITH AN ANNUAL CALL NUMBER_OF_DAYS(MONTH,DAY)
!
         NUMBER_OF_DAYS = 0
         DO DAY = 1, DAYS_PER_MONTH(R_MONTH)
            call write_trace_int2(file_trace_mcfu, "1. DAY", DAY)
            
            DAY_OF_WEEK = MAX(int(1,2),MIN(int(7,2), &
                                        GET_DAY_OF_WEEK_4(R_MONTH,DAY)))
            NUMBER_OF_DAYS(DAY_OF_WEEK) = &
                                         NUMBER_OF_DAYS(DAY_OF_WEEK) + 1
         ENDDO
!
         IF(ALLOCATED(CF_USER_DAY_DATA)) DEALLOCATE(CF_USER_DAY_DATA)
         ALLOCATE(CF_USER_DAY_DATA(24,7,R_MAX_USER_ID))
         CF_USER_DAY_DATA = 0.0
!
         DO I  = 1, R_MAX_USER_ID
            DAY_TYPE_ID = R_MONTHLY_USER_CF_COUNTER(I)
            
            PRODUCT = PRODUCT_INDEX(DAY_TYPE_ID,R_MONTH)
            IF(PRODUCT == 0) CYCLE
! 052412. THIS WHOLE SECTION SHOULD BE PRE-CALCULATED AND PARAMETERS SAVED
            HOUR = 24
            DAY = 0
            WEEKLY_REFERENCE_PEAK = -999999999.
            WEEKLY_REFERENCE_BASE =  999999999.
            WEEKLY_NONZERO_BASE = 999999999.
            WEEKLY_REFERENCE_ENERGY = 0.0
            nHrAtMin=0
            HoursAtNonzeroMin = 0
!
            DAY_ENERGY = 0.0
!
            MAX_LOAD_SCALAR = -999999999.
            DO J = 1, 168
               IF(HOUR == 24) THEN
                  HOUR = 1
                  DAY = DAY + 1
               ELSE
                  HOUR = HOUR + 1
               ENDIF
               MAX_LOAD_SCALAR = MAX(MAX_LOAD_SCALAR, &
                              USER_DAY_DATA(HOUR,DAY,R_MONTH,PRODUCT))
            ENDDO
            IF(MAX_LOAD_SCALAR > 0.00001)THEN
               MAX_LOAD_SCALAR = 1.0/MAX_LOAD_SCALAR
            ELSE
               MAX_LOAD_SCALAR = 1.0
            ENDIF
!
            HOUR = 24
            DAY = 0
!
            DO J = 1, 168
               IF(HOUR == 24) THEN
                  HOUR = 1
                  DAY = DAY + 1
               ELSE
                  HOUR = HOUR + 1
               ENDIF
               WEEKLY_REFERENCE_LOAD(J) = MAX_LOAD_SCALAR * &
                              USER_DAY_DATA(HOUR,DAY,R_MONTH,PRODUCT)
               WEEKLY_REFERENCE_PEAK = MAX(WEEKLY_REFERENCE_PEAK, &
                                             WEEKLY_REFERENCE_LOAD(J))
               IF(WEEKLY_REFERENCE_LOAD(J) < WEEKLY_REFERENCE_BASE) THEN
                  WEEKLY_REFERENCE_BASE = WEEKLY_REFERENCE_LOAD(J)
                  nHrAtMin=1
               ELSE
                  if(abs(WEEKLY_REFERENCE_LOAD(J) - &
                           WEEKLY_REFERENCE_BASE) < &
                           max(Tolerance, &
                                  WEEKLY_REFERENCE_BASE*Tolerance)) then
                     nHrAtMin = nHrAtMin+1
                  endif
               ENDIF
               IF(WEEKLY_REFERENCE_LOAD(J) < WEEKLY_NONZERO_BASE .AND. &
                              ABS(WEEKLY_REFERENCE_LOAD(J)) > .001) THEN
                  WEEKLY_NONZERO_BASE = WEEKLY_REFERENCE_LOAD(J)
                  HoursAtNonzeroMin = 1
               ELSE
                  HoursAtNonzeroMin = HoursAtNonzeroMin + 1
               ENDIF
               WEEKLY_REFERENCE_ENERGY = WEEKLY_REFERENCE_ENERGY + &
                                             WEEKLY_REFERENCE_LOAD(J)
               DAY_ENERGY(DAY) = DAY_ENERGY(DAY) + &
                                             WEEKLY_REFERENCE_LOAD(J)
            ENDDO
            IF(R_MONTH == 9) THEN
               R_MONTH = R_MONTH
            ENDIF
            MaxFeasCF=(FLOAT(168-nHrAtMin)*WEEKLY_REFERENCE_PEAK + &
                           WEEKLY_REFERENCE_BASE*FLOAT(nHrAtMin))/ &
                                                           FLOAT(168)
            TARGET_CF = MIN(R_MONTHLY_USER_CF(I),MaxFeasCF*.99)
            IF(nHrAtMin > 0) THEN
               TARGET_MINIMUM = 1.01 * &
                  (R_MONTHLY_USER_CF(I) * FLOAT(168) - &
                       FLOAT(168-nHrAtMin)*WEEKLY_REFERENCE_PEAK)/ &
                                                       FLOAT(nHrAtMin)
            ENDIF
            IF(MaxFeasCF < R_MONTHLY_USER_CF(I)) THEN
               MaxFeasCF = MaxFeasCF
            ENDIF
            MINIMUM_MW = WEEKLY_REFERENCE_BASE
            AVERAGE_MW = WEEKLY_REFERENCE_ENERGY/168.
            MAXIMUM_MW = WEEKLY_REFERENCE_PEAK
! 070812.
            WEEKLY_SHAPE_FLAT = .FALSE.
            IF(MAXIMUM_MW > 0.0) THEN
               TEMP_R = (MAXIMUM_MW - WEEKLY_NONZERO_BASE)/MAXIMUM_MW
               IF( TEMP_R < .01) THEN
                  WEEKLY_SHAPE_FLAT = .TRUE.
               ENDIF
            ENDIF
!
            UPPER_CF = ((MAXIMUM_MW - AVERAGE_MW)/3.0 + AVERAGE_MW)/ &
                                                         MAXIMUM_MW
            LOWER_CF = (AVERAGE_MW - (AVERAGE_MW - MINIMUM_MW)/3.0)/ &
                                                         MAXIMUM_MW
!
            IF(R_MONTHLY_USER_CF(I) > UPPER_CF .AND. &
                                                   UPPER_CF < 1.0 ) THEN
               SLOPE =  (MAXIMUM_MW - MINIMUM_MW) / (1.0 - UPPER_CF)
               INTERCEPT =  MAXIMUM_MW - SLOPE ! * 1.0

               MINIMUM_MW = MAX(INTERCEPT + SLOPE * &
                                    R_MONTHLY_USER_CF(I),TARGET_MINIMUM)
            ELSEIF(R_MONTHLY_USER_CF(I) < LOWER_CF .AND. &
                                                    LOWER_CF > 0.0) THEN
               SLOPE = (MINIMUM_MW - 0.0) / (LOWER_CF - 0.0)

               MINIMUM_MW = SLOPE * R_MONTHLY_USER_CF(I)
            ENDIF

            TARGET_CF = MIN(R_MONTHLY_USER_CF(I),MaxFeasCF*.99)

            IF(WEEKLY_REFERENCE_ENERGY > 0.1) THEN
               MULT = 0.0
               DO DAY = 1, 7
                  MULT = MULT + &
                              DAY_ENERGY(DAY)*FLOAT(NUMBER_OF_DAYS(DAY))
               ENDDO
               MULT = 7.0 * MULT / FLOAT(DAYS_PER_MONTH(R_MONTH))
               IF(MULT > 0.001) THEN
                  MULT = WEEKLY_REFERENCE_ENERGY/MULT
               ELSE
                  MULT = 1.0
               ENDIF
            ELSE
               MULT = 1.0
            ENDIF

            IF(R_MONTHLY_USER_CF(I) < MaxFeasCF) THEN
               ENERGY = MAXIMUM_MW*TARGET_CF*168. *MULT
            ELSE
               ENERGY = MAXIMUM_MW*TARGET_CF*168.
            ENDIF


            IF(ENERGY > 0.1) THEN
               IF(WEEKLY_SHAPE_FLAT .AND. HoursAtNonzeroMin > 0) THEN
                  IF(MAXIMUM_MW > 0.1) THEN
                     TEMP_R = TARGET_CF * 168.0 / &
                               (MAXIMUM_MW * FLOAT(HoursAtNonzeroMin))
                     DO J = 1, 168
                        WEEKLY_LOAD(J) = &
                                 WEEKLY_REFERENCE_LOAD(J) * TEMP_R
                     ENDDO
                  ELSE
                     WEEKLY_LOAD = 0.0
                  ENDIF
               ELSE
                  call NonlinearlyMap_R4( &
                        WEEKLY_REFERENCE_LOAD, & !  array source of mapping ...
                        WEEKLY_REFERENCE_BASE, & !  min of week
                        WEEKLY_REFERENCE_PEAK, & !  max of week
                        WEEKLY_LOAD,         & !  array destination of mapping ...
                        MINIMUM_MW,          & !
                        MAXIMUM_MW,          & !
                        ENERGY,INT(168,2))    ! ... for all 168 hours of the week
               ENDIF
            ELSE ! 070912.
                WEEKLY_LOAD = 0.0
            ENDIF
            HOUR = 24
            DAY = 0
            WEEKLY_REFERENCE_ENERGY = 0.0
            DO J = 1, 168
               IF(HOUR == 24) THEN
                  HOUR = 1
                  DAY = DAY + 1
               ELSE
                  HOUR = HOUR + 1
               ENDIF
!               USER_DAY_DATA(HOUR,DAY,R_MONTH,PRODUCT) = WEEKLY_LOAD(J)
               CF_USER_DAY_DATA(HOUR,DAY,I) = WEEKLY_LOAD(J)
               WEEKLY_REFERENCE_ENERGY = WEEKLY_REFERENCE_ENERGY + &
                                                         WEEKLY_LOAD(J)
            ENDDO
         ENDDO ! DAY_TYPE_IDS
      RETURN

      ENTRY GET_SUM_USER_DAY_DATA(R_MONTH,R_DAY_TYPE_ID)

!
! 052312. NEED TO ACCOMODATE CAPACITY FACTORS.
!
         GET_SUM_USER_DAY_DATA = 0.
!
         IF(.NOT. USER_DAY_FILE_EXISTS) RETURN
         GET_SUM_USER_DAY_DATA = &
                                SUM_USER_DAY_DATA(R_MONTH,R_DAY_TYPE_ID)
      RETURN

      ENTRY GET_USER_DAY(R_DAY_OF_WEEK,R_MONTH,R_DAY_TYPE_ID, &
                                        R_ONE_DAY_DATA, &
                                          R_CF)

            
        if(file_trace_gud==0) then
            file_trace_gud=open_trace("get_user_day.trace", rq_gud)
        end if
        call write_trace_int4s(file_trace_gud, "PRODUCT_INDEX array", &
            PRODUCT_INDEX(R_DAY_TYPE_ID,1:12))

        
        if(trim(gud_called_from)=="") then
            STOP "gud_called_from"
        end if
        call write_trace_string(file_trace_gud, "called from", &
            gud_called_from)
            
        gud_called_from=""
        if(file_trace_gud/=BAD_TRACE_HANDLE) then
            write(file_trace_gud,*) " Arguments: ", &
            "R_DAY_OF_WEEK ", R_DAY_OF_WEEK,"R_MONTH ", R_MONTH, &
            "R_DAY_TYPE_ID ", R_DAY_TYPE_ID, &
            "R_ONE_DAY_DATA ", R_ONE_DAY_DATA, &
            "R_CF ", R_CF
        end if
        
        call check_bounds_i4_2s(product_index, "wh_objt:0019", &
                 "product_index", int(R_DAY_TYPE_ID,2), int(R_MONTH,2))
!
         GET_USER_DAY = 0.
!
         
         IF(.NOT. USER_DAY_FILE_EXISTS) RETURN
!
         IF(R_DAY_OF_WEEK > 7) THEN
            R_DAY_OF_WEEK = 7
         ELSEIF(R_DAY_OF_WEEK < 1) THEN
            R_DAY_OF_WEEK = 1
         ENDIF
         IF(R_CF > 0) THEN
            GET_USER_DAY = 0
         call write_trace_real4(file_trace_gud, "1. GUD", GET_USER_DAY)
            DO I = 1, 24
               R_ONE_DAY_DATA(I) = &
                                  CF_USER_DAY_DATA(I,R_DAY_OF_WEEK,R_CF)
               IF( ABS(R_ONE_DAY_DATA(I)) > 0.001) &
                                         GET_USER_DAY = GET_USER_DAY + 1
       call write_trace_real4(file_trace_gud, "2. GUD", GET_USER_DAY)
            ENDDO
         ELSE
            PRODUCT = PRODUCT_INDEX(R_DAY_TYPE_ID,R_MONTH)
!
            IF(PRODUCT == 0) then
                call write_trace_message(file_trace_gud, "1. return")
                RETURN
            end if
!
            R_ONE_DAY_DATA(1) = &
                          USER_DAY_DATA(1,R_DAY_OF_WEEK,R_MONTH,PRODUCT)
            R_ONE_DAY_DATA(2) = &
                          USER_DAY_DATA(2,R_DAY_OF_WEEK,R_MONTH,PRODUCT)
            R_ONE_DAY_DATA(3) = &
                          USER_DAY_DATA(3,R_DAY_OF_WEEK,R_MONTH,PRODUCT)
            R_ONE_DAY_DATA(4) = &
                          USER_DAY_DATA(4,R_DAY_OF_WEEK,R_MONTH,PRODUCT)
            R_ONE_DAY_DATA(5) = &
                          USER_DAY_DATA(5,R_DAY_OF_WEEK,R_MONTH,PRODUCT)
            R_ONE_DAY_DATA(6) = &
                          USER_DAY_DATA(6,R_DAY_OF_WEEK,R_MONTH,PRODUCT)
            R_ONE_DAY_DATA(7) = &
                          USER_DAY_DATA(7,R_DAY_OF_WEEK,R_MONTH,PRODUCT)
            R_ONE_DAY_DATA(8) = &
                          USER_DAY_DATA(8,R_DAY_OF_WEEK,R_MONTH,PRODUCT)
            R_ONE_DAY_DATA(9) = &
                         USER_DAY_DATA(9,R_DAY_OF_WEEK,R_MONTH,PRODUCT)
            R_ONE_DAY_DATA(10) = &
                         USER_DAY_DATA(10,R_DAY_OF_WEEK,R_MONTH,PRODUCT)
            R_ONE_DAY_DATA(11) = &
                         USER_DAY_DATA(11,R_DAY_OF_WEEK,R_MONTH,PRODUCT)
            R_ONE_DAY_DATA(12) = &
                         USER_DAY_DATA(12,R_DAY_OF_WEEK,R_MONTH,PRODUCT)
            R_ONE_DAY_DATA(13) = &
                         USER_DAY_DATA(13,R_DAY_OF_WEEK,R_MONTH,PRODUCT)
            R_ONE_DAY_DATA(14) = &
                         USER_DAY_DATA(14,R_DAY_OF_WEEK,R_MONTH,PRODUCT)
            R_ONE_DAY_DATA(15) = &
                         USER_DAY_DATA(15,R_DAY_OF_WEEK,R_MONTH,PRODUCT)
            R_ONE_DAY_DATA(16) = &
                         USER_DAY_DATA(16,R_DAY_OF_WEEK,R_MONTH,PRODUCT)
            R_ONE_DAY_DATA(17) = &
                         USER_DAY_DATA(17,R_DAY_OF_WEEK,R_MONTH,PRODUCT)
            R_ONE_DAY_DATA(18) = &
                         USER_DAY_DATA(18,R_DAY_OF_WEEK,R_MONTH,PRODUCT)
            R_ONE_DAY_DATA(19) = &
                         USER_DAY_DATA(19,R_DAY_OF_WEEK,R_MONTH,PRODUCT)
            R_ONE_DAY_DATA(20) = &
                         USER_DAY_DATA(20,R_DAY_OF_WEEK,R_MONTH,PRODUCT)
            R_ONE_DAY_DATA(21) = &
                         USER_DAY_DATA(21,R_DAY_OF_WEEK,R_MONTH,PRODUCT)
            R_ONE_DAY_DATA(22) = &
                         USER_DAY_DATA(22,R_DAY_OF_WEEK,R_MONTH,PRODUCT)
            R_ONE_DAY_DATA(23) = &
                         USER_DAY_DATA(23,R_DAY_OF_WEEK,R_MONTH,PRODUCT)
            R_ONE_DAY_DATA(24) = &
                         USER_DAY_DATA(24,R_DAY_OF_WEEK,R_MONTH,PRODUCT)
                         
            call write_trace_real4s(file_trace_gud, "one_day", &
                r_one_day_data(1:24))
!
            GET_USER_DAY = 0
        call write_trace_real4(file_trace_gud, "3. GUD", GET_USER_DAY)
!
            IF( ABS(R_ONE_DAY_DATA(1)) > 0.001) &
                                         GET_USER_DAY = GET_USER_DAY + 1
            IF( ABS(R_ONE_DAY_DATA(2)) > 0.001) &
                                         GET_USER_DAY = GET_USER_DAY + 1
            IF( ABS(R_ONE_DAY_DATA(3)) > 0.001) &
                                         GET_USER_DAY = GET_USER_DAY + 1
            IF( ABS(R_ONE_DAY_DATA(4)) > 0.001) &
                                         GET_USER_DAY = GET_USER_DAY + 1
            IF( ABS(R_ONE_DAY_DATA(5)) > 0.001) &
                                         GET_USER_DAY = GET_USER_DAY + 1
            IF( ABS(R_ONE_DAY_DATA(6)) > 0.001) &
                                         GET_USER_DAY = GET_USER_DAY + 1
            IF( ABS(R_ONE_DAY_DATA(7)) > 0.001) &
                                         GET_USER_DAY = GET_USER_DAY + 1
            IF( ABS(R_ONE_DAY_DATA(8)) > 0.001) &
                                         GET_USER_DAY = GET_USER_DAY + 1
            IF( ABS(R_ONE_DAY_DATA(9)) > 0.001) &
                                         GET_USER_DAY = GET_USER_DAY + 1
            IF( ABS(R_ONE_DAY_DATA(10)) > 0.001) &
                                         GET_USER_DAY = GET_USER_DAY + 1
            IF( ABS(R_ONE_DAY_DATA(11)) > 0.001) &
                                         GET_USER_DAY = GET_USER_DAY + 1
            IF( ABS(R_ONE_DAY_DATA(12)) > 0.001) &
                                         GET_USER_DAY = GET_USER_DAY + 1
            IF( ABS(R_ONE_DAY_DATA(13)) > 0.001) &
                                         GET_USER_DAY = GET_USER_DAY + 1
            IF( ABS(R_ONE_DAY_DATA(14)) > 0.001) &
                                         GET_USER_DAY = GET_USER_DAY + 1
            IF( ABS(R_ONE_DAY_DATA(15)) > 0.001) &
                                         GET_USER_DAY = GET_USER_DAY + 1
            IF( ABS(R_ONE_DAY_DATA(16)) > 0.001) &
                                         GET_USER_DAY = GET_USER_DAY + 1
            IF( ABS(R_ONE_DAY_DATA(17)) > 0.001) &
                                         GET_USER_DAY = GET_USER_DAY + 1
            IF( ABS(R_ONE_DAY_DATA(18)) > 0.001) &
                                         GET_USER_DAY = GET_USER_DAY + 1
            IF( ABS(R_ONE_DAY_DATA(19)) > 0.001) &
                                         GET_USER_DAY = GET_USER_DAY + 1
            IF( ABS(R_ONE_DAY_DATA(20)) > 0.001) &
                                         GET_USER_DAY = GET_USER_DAY + 1
            IF( ABS(R_ONE_DAY_DATA(21)) > 0.001) &
                                         GET_USER_DAY = GET_USER_DAY + 1
            IF( ABS(R_ONE_DAY_DATA(22)) > 0.001) &
                                         GET_USER_DAY = GET_USER_DAY + 1
            IF( ABS(R_ONE_DAY_DATA(23)) > 0.001) &
                                         GET_USER_DAY = GET_USER_DAY + 1
            IF( ABS(R_ONE_DAY_DATA(24)) > 0.001) &
                                         GET_USER_DAY = GET_USER_DAY + 1
         ENDIF

       call write_trace_real4(file_trace_gud, "4. GUD", GET_USER_DAY)
      RETURN

      ENTRY GET_USER_HOUR_IN_DAY(R_HOUR,R_DAY_OF_WEEK,R_MONTH, &
                                        R_DAY_TYPE_ID,R_CF)

!
         GET_USER_HOUR_IN_DAY = 0.
!
         IF(.NOT. USER_DAY_FILE_EXISTS) then
            RETURN
         end if
!
         IF(R_DAY_OF_WEEK > 7) THEN
            R_DAY_OF_WEEK = 7
         ELSEIF(R_DAY_OF_WEEK < 1) THEN
            R_DAY_OF_WEEK = 1
         ENDIF
         IF(R_CF > 0) THEN
            GET_USER_HOUR_IN_DAY = &
                             CF_USER_DAY_DATA(R_HOUR,R_DAY_OF_WEEK,R_CF)
         ELSE
            if (r_month>12) then
                call end_program("wh_objt:0020 - r_month of " // &
                    trim(itos(int(r_month))) // " invalid.")
            end if

            PRODUCT = PRODUCT_INDEX(R_DAY_TYPE_ID,R_MONTH)
!
            IF(PRODUCT == 0) RETURN
!
!
            GET_USER_HOUR_IN_DAY = &
                     USER_DAY_DATA(R_HOUR,R_DAY_OF_WEEK,R_MONTH,PRODUCT)
         ENDIF
!
      RETURN
      END

!                  ROUTINE TO CREATE A REGIONAL PARAMETER FILE

      SUBROUTINE REGIONAL_PARAMETER_OBJECT
      use end_routine, only: end_program, er_message
      use filename_tracker
      use mod_base_year
      use SpinDriftLib
      USE PROD_ARRAYS_DIMENSIONS
      USE SIZECOM
      INTEGER (KIND=2) :: REGIONAL_YEAR,TF_YEAR,TEMP_YEAR,YEAR
      INTEGER (KIND=2) :: STUDY_BASE_YEAR=0
      CHARACTER (LEN=1) :: RECORD_IS_ACTIVE,TIME_FRAME

      REAL (KIND=4) :: MONTHLY_VALUES(12),ANNUAL_VALUE

      CHARACTER (LEN=20) :: REGIONAL_VARIABLE
      INTEGER (KIND=2) :: DELETE,INUNIT,IREC,LRECL=81
      INTEGER :: IOS
      INTEGER (KIND=2) :: UNIT_NUM=10,PARAMETER_TABLES_IN_FILE=0
      INTEGER (KIND=2) :: R_PARAMETER_TABLES,TRANSACTION_GROUP
      CHARACTER (LEN=5) :: BASE_FILE_NAME,OVERLAY_FAMILY_NAME
      CHARACTER (LEN=5) :: REGIONAL_PARAMETER_FILE
      CHARACTER (LEN=256) :: FILE_NAME
      CHARACTER (LEN=256) :: BASE_FILE_DIRECTORY
      CHARACTER (LEN=256) :: DATA_DRIVE,OUTPUT_DIRECTORY
      LOGICAL (KIND=4) :: FILE_EXISTS,REG_PARAMETER_FILE_EXISTS=.FALSE.
      LOGICAL (KIND=4) :: R_REG_PARAMETER_FILE_EXISTS
!
!
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER (LEN=1024) :: RECLN
! DECLARATION FOR DAY TYPE DETERMINANTS
      CHARACTER (LEN=16) :: FILE_TYPE='REGIONAL PARAM  '
      CHARACTER (LEN=2) :: REGIONAL_PARAMETER_OL='BC'
      LOGICAL (KIND=1) :: LAHEY_LF95
      CHARACTER (LEN=30) :: SCREEN_OUTPUT

! CONVERT THE DAY_TYPE FILE

      ENTRY REGIONAL_PARAMETER_MAKEBIN

      STUDY_BASE_YEAR = get_base_year()
      TF_YEAR = 0
!      BASE_FILE_NAME = REGIONAL_PARAMETER_FILE()
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME = get_rmb_filename(base_file_name)
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      REG_PARAMETER_FILE_EXISTS = FILE_EXISTS
      PARAMETER_TABLES_IN_FILE = 0
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_LOCATE_WRITE(16,30,BASE_FILE_NAME,ALL_VERSIONS,0)
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         ENDIF

         OPEN(10,FILE=FILE_NAME) ! rmb
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCRPARM.BIN", &
                            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
!
!
         IREC = 0
         READ(10,*) DELETE
         DO ! TABLES
            TF_YEAR = 1
            DO ! YEAR-BASED RECORDS
               READ(10,1000,IOSTAT=IOS) RECLN

               IF(RECLN(1:1) == '7' .OR. IOS /= 0) THEN 
                  IF(TF_YEAR <= AVAIL_DATA_YEARS) THEN
                     DO TF_YEAR = TF_YEAR, AVAIL_DATA_YEARS
                        IREC = IREC + 1
                        WRITE(11,REC=IREC) DELETE, &
                           TF_YEAR+STUDY_BASE_YEAR, & !  INT2
                           RECORD_IS_ACTIVE, &
                           TRANSACTION_GROUP, &
                           REGIONAL_VARIABLE, &
                           TIME_FRAME, &
                           ANNUAL_VALUE, &
                           MONTHLY_VALUES
                     ENDDO
                  ENDIF ! DETECTED NEW TABLE OR END OF FILE

                  EXIT ! LEAVE LOOP (GO TO NEXT TABLE OR EXIT ROUTINE)
!
               ENDIF
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,ERR=200) DELETE, &
                           REGIONAL_YEAR, &
                           RECORD_IS_ACTIVE, &
                           TRANSACTION_GROUP, &
                           REGIONAL_VARIABLE, &
                           TIME_FRAME, &
                           ANNUAL_VALUE, &
                           MONTHLY_VALUES
!
               IREC = IREC + 1
!
               READ(RECLN,*,ERR=200) DELETE,YEAR
               TEMP_YEAR = MIN(AVAIL_DATA_YEARS,YEAR - STUDY_BASE_YEAR)
               IF(TEMP_YEAR > TF_YEAR) THEN ! FILL IN MISSING YEARS
                  IF(IREC == 1) THEN
                     WRITE(4,*) "The first year of the first table in"
                     WRITE(4,*) "the REGIONAL PARAMETER file is ",YEAR
                     WRITE(4,*) "while the base year set in project"
                     WRITE(4,*) "information is ",STUDY_BASE_YEAR
                     WRITE(4,*) "First forecast year in the Transact"
                     WRITE(4,*) "Forecast file first year must be one"
                     WRITE(4,*) "year greater than the base year."
                     WRITE(4,*) " "

                  ENDIF
                  DO TF_YEAR = TF_YEAR, TEMP_YEAR - 1
                     WRITE(11,REC=IREC) DELETE, &
                           TF_YEAR+STUDY_BASE_YEAR, & !  INT2
                           RECORD_IS_ACTIVE, &
                           TRANSACTION_GROUP, &
                           REGIONAL_VARIABLE, &
                           TIME_FRAME, &
                           ANNUAL_VALUE, &
                           MONTHLY_VALUES
                     IREC = IREC + 1
                  ENDDO

               ENDIF

               WRITE(11,REC=IREC) DELETE, &
                           TF_YEAR+STUDY_BASE_YEAR, & !  INT2
                           RECORD_IS_ACTIVE, &
                           TRANSACTION_GROUP, &
                           REGIONAL_VARIABLE, &
                           TIME_FRAME, &
                           ANNUAL_VALUE, &
                           MONTHLY_VALUES
               TF_YEAR = TF_YEAR + 1
            ENDDO ! LOAD GROUP
            PARAMETER_TABLES_IN_FILE = PARAMETER_TABLES_IN_FILE + 1
            IF(IOS /= 0) EXIT
         ENDDO ! READ TABLES
         CLOSE(10)
         CLOSE(11)
!
      ELSE IF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "wh_objt:1115")
      ENDIF
      RETURN
! OVERLAY THE REGIONAL PARAMETER FILE

      ENTRY REGIONAL_PARAMETER_MAKEOVL(OVERLAY_FAMILY_NAME)

      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME = get_rmo_filename(data_drive, overlay_family_name)
      OPEN(10,FILE=FILE_NAME) ! rmo
      READ(10,*) DELETE
      INUNIT = 12
      IF(REGIONAL_PARAMETER_OL == 'BC') THEN
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCRPARM.BIN", &
                                             ACCESS="DIRECT",RECL=LRECL)
         INUNIT = 11
      ENDIF
      OPEN(12,FILE=trim(DATA_DRIVE)//"OLRPARM.BIN",ACCESS="DIRECT", &
                                            STATUS="UNKNOWN",RECL=LRECL)
      IREC = 0
!
      TF_YEAR = 0
      READ(10,1000,IOSTAT=IOS) RECLN
      DO WHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE
!
         READ(10,1000,IOSTAT=IOS) RECLN
!
      ENDDO
!
      READ(RECLN,*,ERR=200) DELETE,TF_YEAR
!
      DO ! TABLES AND YEARS, COUNTING IS DONE INSIDE
         IREC = IREC + 1
         READ(INUNIT,REC=IREC,IOSTAT=IOS) &
                           DELETE, &
                           REGIONAL_YEAR, &
                           RECORD_IS_ACTIVE, &
                           TRANSACTION_GROUP, &
                           REGIONAL_VARIABLE, &
                           TIME_FRAME, &
                           ANNUAL_VALUE, &
                           MONTHLY_VALUES
         IF(IOS /= 0) EXIT ! END OF BINARY FILE
!
         IF(REGIONAL_YEAR == TF_YEAR) THEN
            RECLN = trim(RECLN)//',,,,,,,,,,,,,'
            READ(RECLN,*,ERR=200) &
                           DELETE, &
                           REGIONAL_YEAR, &
                           RECORD_IS_ACTIVE, &
                           TRANSACTION_GROUP, &
                           REGIONAL_VARIABLE, &
                           TIME_FRAME, &
                           ANNUAL_VALUE, &
                           MONTHLY_VALUES
            READ(10,1000,IOSTAT=IOS) RECLN
!
            DO WHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE
!
               READ(10,1000,IOSTAT=IOS) RECLN
!
            ENDDO
            READ(RECLN,*,ERR=200) DELETE,TF_YEAR
         ENDIF
!
         WRITE(12,REC=IREC)  DELETE, &
                           REGIONAL_YEAR, &
                           RECORD_IS_ACTIVE, &
                           TRANSACTION_GROUP, &
                           REGIONAL_VARIABLE, &
                           TIME_FRAME, &
                           ANNUAL_VALUE, &
                           MONTHLY_VALUES
      ENDDO
      CLOSE(10)
      CLOSE(12)

      IF(REGIONAL_PARAMETER_OL == 'BC') CLOSE(11)
      REGIONAL_PARAMETER_OL = 'OL'
      RETURN

  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from wh_objt SIID364'
      call end_program(er_message)

  300 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      CALL MG_LOCATE_WRITE(21,0, &
               'Error reading the Regional Parameter record. Look for'// &
                                          ' a "," in a character name.', &
                           ALL_VERSIONS,1)
      er_message='stop requested from wh_objt SIID365'
      call end_program(er_message)


      ENTRY RESET_REGIONAL_PARAMETER_OL

         REGIONAL_PARAMETER_OL = 'BC'
      RETURN


      ENTRY OPEN_REGIONAL_PARAMETER_FILE

         OPEN(UNIT_NUM, &
               FILE=trim(OUTPUT_DIRECTORY())//REGIONAL_PARAMETER_OL// &
               "RPARM.BIN",ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      RETURN


      ENTRY CLOSE_REGIONAL_PARAMETER_FILE

         CLOSE(UNIT_NUM)
      RETURN

      ENTRY DOES_REG_PARAMETER_FILE_EXIST(R_REG_PARAMETER_FILE_EXISTS)

         R_REG_PARAMETER_FILE_EXISTS = REG_PARAMETER_FILE_EXISTS
      RETURN

      ENTRY GET_PARAMETER_TABLES(R_PARAMETER_TABLES)

         R_PARAMETER_TABLES = PARAMETER_TABLES_IN_FILE
      RETURN
!
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END
!
! ANNUAL MIN MAX ROUTINE
!

      subroutine PickHourlyStrikes
      use SpinDriftLib
      USE PROD_ARRAYS_DIMENSIONS
      use end_routine
      implicit none
      integer (KIND=2) :: HourStruck(8784),i,i2,iHr,iDa,iMo,cYr,nDaCMo
      integer (KIND=2) :: HrInYr,SAVE_HrInYr,GainRank,SuccHr,iBiU,nBiU
      integer (KIND=2) :: iUnit,nUnits,BlocksThisUnit,MaxBlocksPerUnit
      integer (KIND=2) :: GcuBlock,GcuDa,GcuMo,ThisMMHHH,DaysInNLYMo(12)
      integer (KIND=2) :: MMDD(8784),ContigHour(8784)
      integer (KIND=2) :: SeqnHrAtMoHr(12744),MinStrikesPerYear
      integer (KIND=2) :: MaxStrikesPerYear,SystemMaxStrikesPerYear
      integer (KIND=2) :: OperInMo(12),StrikeMMHHH(:,:,:),NumStrikes(:)
      integer (KIND=2) :: TG_USING_PRICE_DISTN
      data DaysInNLYMo/31,28,31,30,31,30,31,31,30,31,30,31/ ! non-leap years
      LOGICAL (KIND=1) :: Mandatory,PredStruck,SuccStruck
      LOGICAL (KIND=1) :: R_AREA_PRICE,AREA_PRICE=.FALSE.
      LOGICAL (KIND=4) :: CountStartsOnly
      real (KIND=4) :: AggregateCost,MktPrice,StrikeValue,OptionValue
      real (KIND=4) :: GcuCap(24),BlockMargin(2),BestMargin
      real (KIND=4) :: CostCriterion(2),ContigPrices(8784)
      real (KIND=4) :: ContigGain(8784) ! ==ContigPrices-OperCost
      real (KIND=4) :: PricesHDM(24,31,12) ! $/MWh
      real (KIND=4) :: R_ANNUAL_PRICES(*),BlockCap(2,12),OperCost(2,12) ! varies by month
      real (KIND=4) :: HrlyCapUsed(:,:,:,:,:),TOTAL_PRICE_SCALAR
      real (KIND=4) :: MONTHLY_TOTAL_PRICE_SCALAR
      real (KIND=4) :: TOTAL_ANNUAL_ENERGY(0:12)
      real (KIND=4) :: GET_HOURLY_SCEN_ELECT_MULT
      real (KIND=4) :: GET_HRLY_TG_SCEN_ELECT_MULT
      real (KIND=4) :: SCENARIO_ELECTRIC_MULT,AREA_PRICE_MULT
      real (KIND=4) :: GET_SCENARIO_ELECTRIC_PRICE,MARKET_PRICE_SCALAR
      real (KIND=4) :: GET_MARKET_PRICE_SCALAR,GET_VAR
      allocatable :: StrikeMMHHH,NumStrikes,HrlyCapUsed
      save
! PRICE FILE DECLARATIONS
      INTEGER (KIND=2) :: R_YEAR,R_TRANS_GROUP,TRANS_GROUP=0,R_END_POINT
      INTEGER (KIND=2) :: LOCAL_UNIT_NO,USER_I,USER_IREC,ALINE_LOAD_DATA
      INTEGER (KIND=2) :: TIMZON,TEMPER,DELTMP,LOCAL_DAY_WEEK
      INTEGER (KIND=2) :: LOCAL_MONTH,LOCAL_DAY,USER_YEAR,I2_ZERO
      INTEGER (KIND=2) :: R_MONTH,HOUR_IN_MONTH
      REAL (KIND=4) :: R_ANNUAL_MARKET_PRICES(8800)
      CHARACTER (LEN=2) :: LOAD_FILE_CHAR_EXT
      CHARACTER (LEN=5) :: MARKET_PRICE_NAME
      CHARACTER (LEN=8) :: EEICODE
      LOGICAL (KIND=4) :: FILE_EXISTS
      CHARACTER (LEN=35) :: R_MULTI_AREA_NAME
      CHARACTER (LEN=256) ::  FILE_NAME,R_FILE_NAME,PRB_FILE_DIRECTORY
      LOGICAL (KIND=1) :: L1_FALSE,GET_TF_GROUP_NAME,VOID_LOGICAL
      LOGICAL (KIND=1) :: GET_ONE_TG_PRICE_MULT
      PARAMETER (L1_FALSE=.FALSE.,I2_ZERO=0)
! END DATA DECLARATIONS

      ENTRY PUT_ANNUAL_MARKET_PRICES(R_ANNUAL_MARKET_PRICES,cYr)

      do iMo=1,12
        nDaCMo=DaysInNLYMo(iMo)
        if((iMo==2).and.(mod(cYr,int(4,2))==0)) then
            nDaCMo=29
        end if
        do iDa=1,nDaCMo
          i=iMo*100+iDa
          do iHr=1,24
            SAVE_HrInYr=SAVE_HrInYr+1

            MMDD        (SAVE_HrInYr)=i
            ContigPrices(SAVE_HrInYr) = &
                                     R_ANNUAL_MARKET_PRICES(SAVE_HrInYr)
          end do
        end do
      end do
!     sorting is done for each unit, after 20020701
!     call Indexed_Sort_Ascending(ContigPrices,ContigHour,
!    +  HrInYr,L1_FALSE) ! for descending order
      RETURN

      ENTRY READ_USER_MARKET_ANNUAL(R_YEAR, &
                                    cYr, &
                                    R_TRANS_GROUP, &
                                    R_FILE_NAME, &
                                    R_END_POINT, &
                                    R_AREA_PRICE)

!
!
      LOCAL_UNIT_NO = 2813 + R_TRANS_GROUP


      FILE_NAME = R_FILE_NAME
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      IF(.NOT. FILE_EXISTS) THEN
!
! FLAG THAT THERE ARE NO PRICES FOR THIS SERIES.
!
         WRITE(4,*) "THERE IS NO MARKET PRICE FILE NAMED", &
                                                               FILE_NAME
         WRITE(4,*) "RUNNING MARKET PRICE MODE FOR AREA", &
                      GET_TF_GROUP_NAME(R_TRANS_GROUP,R_MULTI_AREA_NAME)
         RETURN
      ENDIF
!
      AREA_PRICE = R_AREA_PRICE
      TRANS_GROUP = R_TRANS_GROUP
!
      OPEN(UNIT=LOCAL_UNIT_NO, &
                     FILE=FILE_NAME,FORM='UNFORMATTED',RECL=118, &
                                      ACCESS="DIRECT",STATUS="UNKNOWN")
      USER_I = 1
      SAVE_HrInYr=0
      MARKET_PRICE_SCALAR = GET_MARKET_PRICE_SCALAR()
      IF(MARKET_PRICE_SCALAR < 0.) THEN
         MARKET_PRICE_SCALAR = GET_VAR(MARKET_PRICE_SCALAR,R_YEAR, &
                                                 'MARKET PRICE SCALAR ')
      ENDIF
      do iMo=1,12
         nDaCMo=DaysInNLYMo(iMo)
         ThisMMHHH=iMo*1000
         SCENARIO_ELECTRIC_MULT = &
                             GET_SCENARIO_ELECTRIC_PRICE(R_YEAR,iMo)
!
         VOID_LOGICAL = GET_ONE_TG_PRICE_MULT( &
                                       AREA_PRICE_MULT, &
                                       TG_USING_PRICE_DISTN, &
                                       TRANS_GROUP, &
                                       iMo, &
                                       R_YEAR)
!         if((iMo==2).and.(mod(cYr,4)==0)) nDaCMo=29
         IF(AREA_PRICE) THEN
            IF(TG_USING_PRICE_DISTN == 1) THEN
               MONTHLY_TOTAL_PRICE_SCALAR = &
                                   AREA_PRICE_MULT * MARKET_PRICE_SCALAR
            ELSE
               MONTHLY_TOTAL_PRICE_SCALAR = &
                            SCENARIO_ELECTRIC_MULT * MARKET_PRICE_SCALAR
            ENDIF
         ELSE
            MONTHLY_TOTAL_PRICE_SCALAR = &
                            SCENARIO_ELECTRIC_MULT * MARKET_PRICE_SCALAR
         ENDIF
!        if((iMo==2).and.(mod(cYr,4)==0)) nDaCMo=29
        do iDa=1,nDaCMo
!
!
! 11/1/99.
!
            USER_IREC = ALINE_LOAD_DATA(USER_I,iMo) + iDa - 1
!
            READ(LOCAL_UNIT_NO,REC=USER_IREC) &
                               LOCAL_MONTH,LOCAL_DAY,USER_YEAR, &
                               EEICODE,LOCAL_DAY_WEEK, &
                               TIMZON,TEMPER, &
                               DELTMP, &
                      (PricesHDM(iHr,iDa,iMo),iHR=1,24)
            USER_IREC = USER_IREC + 1
!
! ALWAYS ADD LEAP YEAR INTO LDE FILE
!
            IF(iDa == 28 .AND. iMo == 2) THEN
               READ(LOCAL_UNIT_NO,REC=USER_IREC) &
                                LOCAL_MONTH,LOCAL_DAY,USER_YEAR, &
                                EEICODE,LOCAL_DAY_WEEK, &
                                TIMZON,TEMPER, &
                                DELTMP, &
                      (PricesHDM(iHr,29,iMo),iHR=1,24)
!     +                (PricesHDM(iHr,iDa,iMo),iHR=1,24)
               USER_IREC = USER_IREC + 1
            ENDIF
!
            i=iMo*100+iDa
            do iHr=1,24
!
! 12/21/04.
!
               HOUR_IN_MONTH = (iDa-1)*24 + iHr
               TOTAL_PRICE_SCALAR = MONTHLY_TOTAL_PRICE_SCALAR * &
                        GET_HOURLY_SCEN_ELECT_MULT(HOUR_IN_MONTH,iMo) * &
                        GET_HRLY_TG_SCEN_ELECT_MULT(HOUR_IN_MONTH, &
                                                      iMo,R_TRANS_GROUP)
!
               SAVE_HrInYr=SAVE_HrInYr+1
!              R_ANNUAL_PRICES(SAVE_HrInYr) = PricesHDM(iHr,iDa,iMo) *
!    +                                                TOTAL_PRICE_SCALAR
!
!               ContigPrices(HrInYr) = R_ANNUAL_MARKET_PRICES(HrInYr)
               ContigPrices(SAVE_HrInYr) = PricesHDM(iHr,iDa,iMo) * &
                                                      TOTAL_PRICE_SCALAR
               ThisMMHHH=ThisMMHHH+1
               SeqnHrAtMoHr(ThisMMHHH)=SAVE_HrInYr
               MMDD (SAVE_HrInYr)=i
            end do
!
        end do
      end do

      CLOSE(LOCAL_UNIT_NO)

      RETURN

      entry GET_ANNUAL_PRICES(cYr,R_ANNUAL_PRICES,R_YEAR)


      SAVE_HrInYr=0

      do iMo = 1, 12



         do iDa=1,nDaCMo
            do iHr=1,24

               SAVE_HrInYr=SAVE_HrInYr+1
               R_ANNUAL_PRICES(SAVE_HrInYr) = ContigPrices(SAVE_HrInYr)

            end do
         end do
      enddo
      return ! entry SortPricesForYear

      entry SortPricesForYear(cYr)
      SAVE_HrInYr=0
      do iMo=1,12
        nDaCMo=DaysInNLYMo(iMo)
        if((iMo==2).and.(mod(cYr,int(4,2))==0)) then
            nDaCMo=29
        end if
        do iDa=1,nDaCMo
          i=iMo*100+iDa
          do iHr=1,24
            SAVE_HrInYr=SAVE_HrInYr+1
            ContigPrices(SAVE_HrInYr)=PricesHDM(iHr,iDa,iMo)
            MMDD        (SAVE_HrInYr)=i
          end do
        end do
      end do

      return ! entry SortPricesForYear

      entry PickStrikesForUnit(iUnit,BlocksThisUnit, &
        BlockCap,OperCost,MinStrikesPerYear,MaxStrikesPerYear,OperInMo, &
        CountStartsOnly) ! after 20031030, this (if .true.) implies count
      ! contiguous strike-hours for this unit as one start, and interpret
      ! Min/MaxStrikesPerYear as Min/MaxStartsPerYear


      TOTAL_ANNUAL_ENERGY = 0.
      HrInYr = SAVE_HrInYr
      do iHr=1,SAVE_HrInYr
        iMo=MMDD(iHr)/100
        HourStruck(iHr)=0 ! => .false.; used only if CountStartsOnly
        ContigHour(iHr)=iHr ! refresh indices before sorting
        ContigGain(iHr)=ContigPrices(iHr)-OperCost(1,iMo)
      ! note that use of Block#1 costs above affects only the ordering
      ! of the most-profitable hours, not the decision to strike below;
! for down months, exclude iHr without disturbing ContigPrices or HrInYr
        if(OperInMo(iMo)==0) ContigGain(iHr)=-9999.9
      end do
      call Indexed_Sort_Ascending(ContigGain,ContigHour, &
        HrInYr,L1_FALSE) ! for descending order of gain using only Block#1
      i=0
      do GainRank=1,SAVE_HrInYr
        HrInYr=ContigHour(GainRank)! as ordered by decreasing ContigGain
        MktPrice=ContigPrices(HrInYr)
        iMo=MMDD(HrInYr)/100
        iDa=MMDD(HrInYr)-100*iMo
        iHr=mod (HrInYr-1,24)+1
        ThisMMHHH=iMo*1000+(iDa-1)*24+iHr
        if(OperInMo(iMo)==0) then
            cycle ! user mandated more strikes than were economic
        end if
        i=i+1
        if(i>MaxStrikesPerYear) then
            exit ! consistently with i># Strikes or Starts
        end if
        CostCriterion(2)=0.0 ! useful only for display during debugging
        if((BlocksThisUnit<2).or. &
          (BlockCap(BlocksThisUnit,iMo)<=0.0)) then ! unit is single-block
          CostCriterion(1)=OperCost(1,iMo)
          nBiU=1
        else
!20031031 AggregateCost=(OperCost(1,iMo)*BlockCap(1,iUnit)+
!    +                   OperCost(2,iMo)*BlockCap(2,iUnit))/
!    +                  (BlockCap(1,iUnit    )+BlockCap(2,iUnit))
          AggregateCost=CostCriterion(2) ! use block 2's marg cost per GAT 20031031
          BlockMargin(1)= (MktPrice -OperCost(1,iMo))*BlockCap(1,iMo)
          BlockMargin(2)= (MktPrice - OperCost(2,iMo)) * &
                                   (BlockCap(1,iMo)+BlockCap(2,iMo))

          BestMargin = max(BlockMargin(1),BlockMargin(2))
          if(BestMargin > 0.) then
            if(BlockMargin(1) > BlockMargin(2)) then
          ! decision to strike is successive, first for Block1 then for Block2
               nBiU=2
            else ! (OperCost(1,iMo)>=OperCost(2,iMo))
!20031031 ! decision to strike is based on the capacity-weighted average cost
          ! decision to strike is based on block 2's marg cost per GAT 20031031
!            CostCriterion(1)=AggregateCost
               nBiU=-1 ! indicating Blocks 1 & 2 are combined
            end if
          endif
        end if
        Mandatory=(i<=MinStrikesPerYear) ! even if non-economic
        if(Mandatory.or.(BestMargin > 0.)) then
        ! strike on option to call iUnit
          StrikeMMHHH(1,i,iUnit)=ThisMMHHH
          HrlyCapUsed(iHr,iDa,iMo,1,iUnit)=BlockCap(1,iMo)

          if(   (nBiU<0)) then ! decision is on aggregate

            StrikeMMHHH(2,i,iUnit)=ThisMMHHH
            HrlyCapUsed(iHr,iDa,iMo,2,iUnit)=BlockCap(2,iMo)
          ! the second block is also used in the strike;
          ! use of second block does not increment NumStrikes for iUnit
          end if
!        elseif((nBiU==2).and.(AggregateCost<MktPrice)) then
!        ! strike on both blocks of iUnit as if an aggregate
!          StrikeMMHHH(1,i,iUnit)=ThisMMHHH
!          StrikeMMHHH(2,i,iUnit)=ThisMMHHH
!          HrlyCapUsed(iHr,iDa,iMo,1,iUnit)=BlockCap(1,iMo)
!          HrlyCapUsed(iHr,iDa,iMo,2,iUnit)=BlockCap(2,iMo)
        else
          exit ! quit early since strike is neither economic nor mandatory;
               ! prices further down the sorted list do not get any better
        end if
!
        TOTAL_ANNUAL_ENERGY(iMO) = &
                  TOTAL_ANNUAL_ENERGY(iMO) + &
                           HrlyCapUsed(iHr,iDa,iMo,1,iUnit) + &
                                        HrlyCapUsed(iHr,iDa,iMo,2,iUnit)
        TOTAL_ANNUAL_ENERGY(0) = &
                  TOTAL_ANNUAL_ENERGY(0) + &
                           HrlyCapUsed(iHr,iDa,iMo,1,iUnit) + &
                                        HrlyCapUsed(iHr,iDa,iMo,2,iUnit)
!
        if(CountStartsOnly) then
! do not count towards Starts if preceding or succeeding HourStruck>0
          HourStruck(HrInYr)=1 ! => .true.
          PredStruck=.false.
          SuccStruck=.false.
          if(HrInYr>1          ) PredStruck=(HourStruck(HrInYr-1)>0)
          if(HrInYr<SAVE_HrInYr) SuccStruck=(HourStruck(HrInYr+1)>0)

          if(PredStruck.AND.SuccStruck) then
            i=i-2 ! retain former count
          elseif(PredStruck.or.SuccStruck) then
            i=i-1 ! retain former count
          endif
        ! if(PredStruck) then retain the existing StrikeMMHHH array
          if(SuccStruck) then
! retain the MMHHH at the beginning of the run of contiguous hours
            SuccHr=HrInYr+1 ! hour struck earlier in the i-loop
            do i2=i,1,-1 ! search the list of prior runs for SuccHr
              if(SeqnHrAtMoHr(StrikeMMHHH(1,i2,iUnit))==SuccHr) then

                if(nBiU/=1) then
                    StrikeMMHHH(2,i2,iUnit)=ThisMMHHH
                end if
                StrikeMMHHH(1,i2,iUnit)=ThisMMHHH
                exit ! only one instance indexed i2 need be matched
              end if
            end do
          end if
        end if

      end do ! GainRank
      NumStrikes(iUnit)=i-1
      return ! entry PickStrikesForUnit

      entry GetStrikeStateForUnit(iUnit,BlocksThisUnit, &
        BlockCap,OperCost,OptionValue)

!     BlockCap,OperCost needed only for OptionValue computation
      OptionValue=0.0 ! annual value of option due to this iUnit
      do i=1,NumStrikes(iUnit)
        do iBiU=1,BlocksThisUnit
          ThisMMHHH=StrikeMMHHH(iBiU,1,iUnit)
          if(ThisMMHHH==0) exit ! possible only for iBiU>1
          if(iBiU==1) then ! higher iBiU-indexed MMHHH is either same or 0
            iMo=ThisMMHHH/1000
            iHr=ThisMMHHH-1000*iMo ! here hour in month on [1,744]
            iDa=(iHr+23)/24
            iHr=iHr-24*(iDa-1)
          end if
          StrikeValue=BlockCap(iBiU,iMo)*( &
            PricesHDM(iHr,iDa,iMo)-OperCost(iBiU,iMo))
          OptionValue=OptionValue+StrikeValue
!         write(StdOut,'(6i3,3f6.2,2f8.2,a)')
!    +      iUnit,i,iBiU,iMo,iDa,iHr,
!    +      BlockCap(iBiU,iMo),
!    +      OperCost(iBiU,iMo),
!    +      PricesHDM(iHr,iDa,iMo),
!    +      StrikeValue,OptionValue,' GSS'
        end do
      end do
      return ! entry GetStrikeStateForUnit

      entry GetCapUsedForBlock(iUnit,GcuBlock,GcuMo,GcuDa,GcuCap)

      do iHr=1,24 ! use cmove() if available under LF95's SpinLib.MON
        GcuCap(iHr)=HrlyCapUsed(iHr,GcuDa,GcuMo,GcuBlock,iUnit)
      end do
      return ! entry GetCapUsedForBlock
!

      entry PickHrStrInitArrays(nUnits, &
        MaxBlocksPerUnit, &
        SystemMaxStrikesPerYear)

!     following arrays are used only internally to PickHourlyStrikes
      if(allocated(StrikeMMHHH)) deallocate( &
        StrikeMMHHH,NumStrikes,HrlyCapUsed)
      allocate( &
        StrikeMMHHH(MaxBlocksPerUnit,SystemMaxStrikesPerYear,nUnits), &
        NumStrikes(nUnits), &
        HrlyCapUsed(24,31,12,MaxBlocksPerUnit,nUnits))
!#ifdef UsingF90
!      StrikeMMHHH=I2_ZERO
!      NumStrikes=I2_ZERO
!      HrlyCapUsed=0.0e0
!#else
      StrikeMMHHH = 0
      NumStrikes = 0
      HrlyCapUsed = 0.
!#endif
      return ! entry PickHrStrInitArrays
!
      end ! subroutine PickHourlyStrikes

      subroutine annual_thermal_call() ! useful only for examples of calls
      use end_routine
      implicit none
      real (KIND=4) :: MoBias,OptionValue,BlockCap(2,12,2) ! allow this to vary by month per GAT?
      real (KIND=4) :: OperCost(2,12,2)
      integer (KIND=2) :: iMo,iUnit,nUnits=2,MaxBlocksPerUnit=2 ! <=2 per GAT
      integer (KIND=2) :: SystemMaxStrikesPerYear=32
      integer (KIND=2) :: MinStrikesPerYear(2)=(4,0)
      integer (KIND=2) :: MaxStrikesPerYear(2)=(16,3)
      integer (KIND=2),DIMENSION(12,2) :: OperInMo=reshape((/0,0,0,0,1, &
                       1,1,1,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1/),(/12,2/))
!
      call PickHrStrInitArrays(nUnits,MaxBlocksPerUnit, &
        SystemMaxStrikesPerYear)
!
      do iMo=1,12 ! generate values for testing
        MoBias=0.1*float(iMo)
        OperCost(1,iMo,1)=27.5+MoBias ! Unit 1
        OperCost(2,iMo,1)=31.6+MoBias
        OperCost(1,iMo,2)=29.3+MoBias ! Unit 2
        OperCost(2,iMo,2)=27.9+MoBias ! less than for block 1
        BlockCap(1,iMo,1)=75.0 ! Unit 1
        BlockCap(2,iMo,1)=25.0
        BlockCap(1,iMo,2)=30.0 ! Unit 2
        BlockCap(2,iMo,2)=20.0
      end do
!
!     need to fill out the PricesHDM and ContigPrices arrays:
!     call READ_USER_MARKET_ANNUAL(R_YEAR, ...
!     call PUT_ANNUAL_MARKET_PRICES(R_ANNUAL_MARKET_PRICES,cYr)
!
      do iUnit=1,nUnits
        call PickStrikesForUnit(iUnit,2_2, &
          BlockCap(1,1,iUnit), &
          OperCost(1,1,iUnit), &
          MinStrikesPerYear(iUnit), &
          MaxStrikesPerYear(iUnit), &
          OperInMo(1,iUnit),.true.)
      end do
      do iUnit=1,nUnits
        call GetStrikeStateForUnit(iUnit,2_2, &
          BlockCap(1,1,iUnit), &
          OperCost(1,1,iUnit),OptionValue)
      end do
      end ! subroutine annual_thermal_call
!

      subroutine WeeklyPumpStorage
      use end_routine
      implicit none
      LOGICAL (KIND=1) :: GPSameHour,GPShoulder,TgtAttained
      LOGICAL (KIND=1) :: L1_FALSE=.false.
      integer (KIND=2) :: iHr,iGP,PrtDetail=0 ! 2 to get LDC before and after PS mods
      integer (KIND=2) :: nFullCapHr,DescOrder(168),GPHour(0:1)
      integer (KIND=2) :: GPRank(0:1)
      real (KIND=4) :: Nearly1=0.999999 ! relative threshold for completion
      real (KIND=4) :: R_PUMPING_CAP_MW,R_MIN_GEN_CAP_MW
      real (KIND=4) :: R_MAX_GEN_CAP_MW,R_AVG_GEN_CAP_MW
      real (KIND=4) :: R_PUMPING_STORAGE_EFFICIENCY
      real (KIND=4) :: R_WEEKLY_LOAD_ORG(*) ! 24x7 hourly detail
      real (KIND=4) :: R_WEEKLY_LOAD_MOD(*) ! 24x7 hourly detail
      real (KIND=4) :: IncrHrCurLoad(0:1),FracHrCurLoad(0:1)
      real (KIND=4) :: TimeRatio(0:1),GPLoad(0:1),CapyMW(0:1)
      real (KIND=4) :: CeffMW(0:1),HrsUse(0:1),EnergyRatio
      real (KIND=4) :: TimeConstraint,OriginalLoad,ShoulderLoad
!    +   NominalCap,
!    +   ReducedCap,
      real (KIND=4) :: SumGenMWH,TgtGenMWH
      return ! subroutine OnlyDeclarations

!
      entry OptimizeWeeksPumpedStorage( &
         R_PUMPING_CAP_MW, & !  cannot be inferred from efficiency
         R_MIN_GEN_CAP_MW, & !  due to a RoR component or min-flow reqmt?
         R_MAX_GEN_CAP_MW, &
         R_AVG_GEN_CAP_MW, &
         R_PUMPING_STORAGE_EFFICIENCY, &
         R_WEEKLY_LOAD_ORG, & !  24x7 detail indexed on [1,168]
         R_WEEKLY_LOAD_MOD) ! 24x7 detail indexed on [1,168]

!
!     This routine assumes that for the purpose of the optimization,
!     market prices and pumping costs are monotonically related to loads.
!     Lacking other information on time-of-day operational constraints,
!     it assumes that the generation hours and pumping hours can be
!     anywhere in the week, i.e., that reservoir capacity is not binding.
!     Its only output is the array of loads modified by the PS operation.
!     If it is assumed that the PS operation affects the market prices and
!     costs by its operation, then the operation can continue in shoulder
!     hours at reduced capacity until pumping and generation hours collide.
!     AGT 20020823

    ! first account for the possibility of non-zero minimum
    ! required generation
      EnergyRatio=1.0/R_PUMPING_STORAGE_EFFICIENCY ! PmpMWH/GenMWH
      CapyMW(0)=R_MAX_GEN_CAP_MW-R_MIN_GEN_CAP_MW
      CapyMW(1)=R_PUMPING_CAP_MW
    ! energy-balance constraint:  PmpMW=EnergyRatio*GenMWH
    !   CapyMW(1)*HrsUse(1)=EnergyRatio*CapyMW(0)*HrsUse(0)
      TimeRatio(0)=CapyMW(1)/(EnergyRatio*CapyMW(0))
      TimeRatio(1)=EnergyRatio*CapyMW(0)/CapyMW(1)
      TgtGenMWH=168.0*(R_AVG_GEN_CAP_MW-R_MIN_GEN_CAP_MW)
      do iHr=1,168
        R_WEEKLY_LOAD_MOD(iHr)=R_WEEKLY_LOAD_ORG(iHr)-R_MIN_GEN_CAP_MW
        DescOrder(iHr)=iHr
      end do
      call Indexed_Sort_Ascending(R_WEEKLY_LOAD_MOD,DescOrder, &
        int(168,2),L1_FALSE) ! for descending order
    ! scan the sorted array to determine the number
    ! of full-capacity gen hours
      do nFullCapHr=1,168
        SumGenMWH=CapyMW(0)*float(nFullCapHr)
        ShoulderLoad=R_WEEKLY_LOAD_MOD(DescOrder(nFullCapHr))-CapyMW(0)
        do iHr=nFullCapHr+1,168
          OriginalLoad=R_WEEKLY_LOAD_MOD(DescOrder(iHr))
          if(OriginalLoad<=ShoulderLoad) exit
          SumGenMWH=SumGenMWH+amin1(CapyMW(0),OriginalLoad-ShoulderLoad)
        end do
!        if(PrtDetail>1) write(*,'(2i4,2f6.1,2f8.1)')
!     +    nFullCapHr,iHr,OriginalLoad,ShoulderLoad,SumGenMWH,TgtGenMWH
        if(iHr==nFullCapHr+1) exit ! no loads lie above the shoulder
        if(SumGenMWH>TgtGenMWH) exit
      end do
      nFullCapHr=nFullCapHr-1
!     if(nFullCapHr>nint(168.0*TimeRatio(0)/(TimeRatio(0)+TimeRatio(1))))
      if(nFullCapHr>nint(168.0/(1.0+TimeRatio(1)/TimeRatio(0)))) &
         nFullCapHr=nint(168.0/(1.0+TimeRatio(1)/TimeRatio(0)))
      if(nFullCapHr>0) then
        ShoulderLoad=R_WEEKLY_LOAD_MOD(DescOrder(nFullCapHr))-CapyMW(0)
      else
      ! in the unlikely case of relatively level load or small PS facility,
      ! find the ShoulderLoad which meets the TgtGenMWH
        ShoulderLoad=R_WEEKLY_LOAD_MOD(DescOrder(1))
        SumGenMWH=0.0
        do iHr=1,168
          OriginalLoad=ShoulderLoad
          ShoulderLoad=R_WEEKLY_LOAD_MOD(DescOrder(iHr+1))
          SumGenMWH=SumGenMWH+(OriginalLoad-ShoulderLoad)*float(iHr)
!          if(PrtDetail>1) write(*,'(2i4,2f6.1,2f8.1)')
!     +      nFullCapHr,iHr,OriginalLoad,ShoulderLoad,SumGenMWH,TgtGenMWH
          if(SumGenMWH>TgtGenMWH) exit
        end do
!       ShoulderLoad=ShoulderLoad+(OriginalLoad-ShoulderLoad)*
!    +    (SumGenMWH-TgtGenMWH)/((OriginalLoad-ShoulderLoad)*float(iHr))
        ShoulderLoad=ShoulderLoad+ &
          (SumGenMWH-TgtGenMWH)/float(iHr) ! equivalent to above
!        if(PrtDetail>1) write(*,'(2i4,2f6.1,2f8.1)')
!     +    nFullCapHr,iHr,OriginalLoad,ShoulderLoad,SumGenMWH,TgtGenMWH
      end if
    ! walk in from both ends of the sorted array, accumulating energy until
    ! the pumping and generation collide (unlikely), the generating capacity
    ! is exceeded, or the target energy is met
      SumGenMWH=0.0
      GPRank(0)=1
      GPRank(1)=168
!     NominalCap=0.0
      do iGP=0,1 ! 0 for generation, 1 for pumping
        GPHour(iGP)=DescOrder(GPRank(iGP))
        GPLoad(iGP)=R_WEEKLY_LOAD_MOD(GPHour(iGP))- &
          CapyMW(iGP)*float(1-2*iGP)
        FracHrCurLoad(iGP)=0.0
        HrsUse(iGP)=0.0
        CeffMW(iGP)=CapyMW(iGP)
!       NominalCap=NominalCap+CapyMW(iGP)
      end do
    ! first two checks below are unlikely to be binding in real-life cases:
      GPSameHour=(GPRank(0)>=GPRank(1)) ! cannot pump and gen in same hour
      GPShoulder=(GPLoad(0)<=GPLoad(1)) ! load difference is too small for full-cap operation
      TgtAttained=(SumGenMWH>=TgtGenMWH) ! target generation attained

      do

        if(GPSameHour.or.TgtAttained) exit
        do iGP=0,1
          if(FracHrCurLoad(iGP)>Nearly1) then
          ! incr/decrement rank of current hour of generation/pumping, respectively
            GPRank(iGP)=GPRank(iGP)+1-2*iGP ! i.e., decrement for iGP==1
            GPHour(iGP)=DescOrder(GPRank(iGP))
            GPLoad(iGP)=R_WEEKLY_LOAD_MOD(GPHour(iGP))- &
              CapyMW(iGP)*float(1-2*iGP)
            FracHrCurLoad(iGP)=0.0
            GPSameHour=(GPRank(0)>=GPRank(1))
!           GPShoulder=(GPLoad(0)<=GPLoad(1))
          end if
        ! probe an increment during current hour
          IncrHrCurLoad(iGP)=amin1(1.0-FracHrCurLoad(iGP), &
                   TimeRatio(iGP)*(1.0-FracHrCurLoad(mod(iGP+1,2))))
        end do ! this loop must complete before the next is begun
        GPShoulder=(GPRank(0)>nFullCapHr)
!       if(GPSameHour.or.GPShoulder) exit ! reduced-cap operation is not allowed
        if(GPSameHour) exit
        if(GPShoulder) then
        ! prorate generation and pump capacities to levelize shoulder loads
!         ReducedCap=GPLoad(0)-GPLoad(1)+NominalCap
!         CeffMW(0)=CapyMW(0)*ReducedCap/NominalCap
!         CeffMW(1)=CapyMW(1)*ReducedCap/NominalCap
          CeffMW(0)=amax1(0.001,GPLoad(0)+CapyMW(0)-ShoulderLoad)
          CeffMW(1)=CapyMW(1)*CeffMW(0)/CapyMW(0)
      ! else retain use of full nominal capacities
        end if
        IncrHrCurLoad(0)=amin1(IncrHrCurLoad(0), &
          (TgtGenMWH-SumGenMWH)/CeffMW(0))
      ! IncrHrCurLoad here is the maximum increment in use in current hour
        do iGP=0,1
          TimeConstraint= &
            IncrHrCurLoad(mod(iGP+1,2))*TimeRatio(iGP)
          if(IncrHrCurLoad(iGP)>TimeConstraint) &
             IncrHrCurLoad(iGP)=TimeConstraint
          FracHrCurLoad(iGP)= &
          FracHrCurLoad(iGP)+ &
          IncrHrCurLoad(iGP) ! at most 1.0
          HrsUse(iGP)= &
          HrsUse(iGP)+IncrHrCurLoad(iGP)
          R_WEEKLY_LOAD_MOD(GPHour(iGP))= &
          R_WEEKLY_LOAD_MOD(GPHour(iGP))-CeffMW(iGP)*float(1-2*iGP)* &
          IncrHrCurLoad(iGP)
        end do ! iGP
        SumGenMWH=SumGenMWH+CeffMW(0)*IncrHrCurLoad(0)
        TgtAttained=(SumGenMWH>Nearly1*TgtGenMWH)

      end do

      return ! entry OptimizeWeeksPumpedStorage
!
      end ! subroutine OnlyDeclarations

      subroutine CallWeeklyPumpStorge ! useful only to test OptimizeWeeksPumpedStorage
      use end_routine
      implicit none
      integer (KIND=2) :: iDa,iHr,iHiW
      real (KIND=4) :: DayBias,R_WEEKLY_LOAD_ORG(168)
      real (KIND=4) :: R_WEEKLY_LOAD_MOD(168)
!
    ! generate a plausible load pattern for testing
      do iDa=1,7 ! for Monday-Sunday
        if(iDa<=5) then
          DayBias=10.0*float(iDa)
        elseif(iDa==6) then
          DayBias= 5.0
        else ! iDa==7
          DayBias= 0.0
        end if
        do iHr=1,24
          iHiW=24*(iDa-1)+iHr
          R_WEEKLY_LOAD_MOD(iHiW)=0.0
          R_WEEKLY_LOAD_ORG(iHiW)=DayBias+200.0*(2.0- &
            sin(6.28*float(iHr-4)/24.0))
        end do
      end do
      call OptimizeWeeksPumpedStorage( &
         93.0,0.0,108.0,5.0,0.75, &
         R_WEEKLY_LOAD_ORG, & !  24x7 hourly detail begins at (1)
         R_WEEKLY_LOAD_MOD) ! 24x7 hourly detail begins at (1)

      end

      FUNCTION GET_DEPTH_OF_MARKET_QUANTITY( &
                                           R_MAX_OUTAGE_BLOCKS, &
                                           R_DISPATCH_COST, &
                                           R_DISPATCH_MW, &
                                           R_SYSTEM_LOAD, &
                                           R_QUANTITY, & !  THIS IS AFTER NATIVE LOAD
                                           R_UNIT_CAPACITY, &
                                           R_HOUR, &
                                           R_DAY, &
                                           R_TG, &
                                           R_DEPTH_QUANTITY, &
                                           R_DEPTH_PRICE, &
                                           R_LOCAL_BALANCE, &
                                           R_SYSTEM_BALANCE, &
                                           R_MIN_DEPTH_MARKET, &
                                           R_MAX_DEPTH_MARKET, &
                                           R_DEPTH_MARGINAL_COST, &
                                           R_DEPTH_RESOURCES, &
                                           R_CALENDAR_DAY_OF_WEEK)

      use SpinDriftLib
      USE PROD_ARRAYS_DIMENSIONS
      use end_routine
      implicit none
      SAVE
      LOGICAL (KIND=1) :: GET_DEPTH_OF_MARKET_QUANTITY
      LOGICAL (KIND=1) :: APPLY_ENERGY_PRODUCT
      LOGICAL (KIND=1) :: WRITE_DEPTH_MARGINAL_COST
      INTEGER (KIND=2) :: R_HOUR,R_TG,MAX_INTERVALS=5,INTERVAL,I,J,K,L
      INTEGER (KIND=2) :: MARGINAL_UNIT_INDEX,HOUR,TOP_INTERVAL
      INTEGER (KIND=2) :: BOTTOM_INTERVAL,R_MAX_OUTAGE_BLOCKS
      INTEGER (KIND=2) :: R_CALENDAR_DAY_OF_WEEK,LAMBDA_INTERVAL
      REAL (KIND=4) :: R_UNIT_PRICE,R_QUANTITY,UPPER_PERCENT,TEMP_CAP
      REAL (KIND=4) :: R_DISPATCH_COST(*),R_DISPATCH_MW(*)
      REAL (KIND=4) :: R_SYSTEM_LOAD,LOCAL_PRICE(2),LOCAL_QUANTITY(2)
      REAL (KIND=4) :: R_UNIT_CAPACITY,SLOPE,INTERCEPT
      REAL (KIND=4) :: R_DEPTH_QUANTITY,R_DEPTH_PRICE
      REAL (KIND=4) :: R_LOCAL_BALANCE,R_SYSTEM_BALANCE
      REAL (KIND=4) :: R_DEPTH_0_PRICE(800),TEMP_PRICE
      REAL (KIND=4) :: R_MIN_DEPTH_MARKET,R_MAX_DEPTH_MARKET
      REAL (KIND=4) :: MAX_DEPTH_PROFIT,LOCAL_DEPTH_PROFIT
      REAL (KIND=4) :: LOCAL_DEPTH_QUANTITY,LOCAL_DEPTH_PRICE
      REAL (KIND=4) :: LOCAL_NET_LOAD,R_DEPTH_MARGINAL_COST
      REAL (KIND=4) :: R_DEPTH_RESOURCES,TEMP_R,R_THIS_YEAR
      REAL (KIND=4) :: TARGET_CAPACITY,DMD_COEFF_A,DMD_COEFF_B
! TEMPORARY DEMO DATA
      REAL (KIND=4) :: DEMO_PRICE(5)
      REAL (KIND=4) :: DEMO_QUANTITY(5)=(/-500.,-250.,0.,250.,500./)
      LOGICAL (KIND=1) :: YES_DEPTH_OF_MARKET,DEPTH_OF_MARKET
      LOGICAL (KIND=1) :: MONTHLY_DEPTH_OF_MARKET_DB
      LOGICAL (KIND=1) :: FILE_NEVER_OPENED=.TRUE.
      LOGICAL (KIND=1) :: LAHEY_LF95
      LOGICAL (KIND=1) :: YES_REPORT_NAMES,DEPTH_MARKET_REPORTS_ACTIVE
      LOGICAL (KIND=1) :: GET_DMD_COEFF,TEMP_L
      CHARACTER (LEN=5) :: DEPTH_MARKET_FILE_NAME
      INTEGER (KIND=2) :: R_END_POINT,R_YEAR,R_MONTH,DAY,R_DAY
      INTEGER (KIND=2) :: R_DAYS_IN_MONTH,DEPTH_MARKET_INTERVALS
      INTEGER (KIND=2) :: DEPTH_TARGET_GROUP,DEPTH_MARKET_UNIT
      INTEGER (KIND=2) :: TRANS_MWH_UNIT,HR,ZERO_PRICE_INTERVAL
      REAL (KIND=4) :: DEPTH_MARKET_INTERVAL_SIZE
      LOGICAL (KIND=1) :: C_DEPTH_MARGIN_REPORT
      LOGICAL (KIND=1) :: C_DEPTH_MARGIN_NOT_OPEN=.TRUE.
      INTEGER (KIND=2) :: C_DEPTH_MARGIN_UNIT,C_DEPTH_MARGIN_RPT_HEADER
      INTEGER (KIND=2) :: C_DEPTH_LAMBDA_UNIT,GY_HOURLY_HEADER
      INTEGER (KIND=4) :: C_DEPTH_MARGIN_REC,C_DEPTH_LAMBDA_REC
      CHARACTER (LEN=20) :: R_TRANS_GROUP_NAME
      INTEGER (KIND=2) :: NUM_PRODUCTS
      PARAMETER ( NUM_PRODUCTS=15)                             ! TMS 20041129 NUM_PRODUCTS increased by 2 to 15
      CHARACTER (LEN=20) :: LOCAL_PRODUCT_TYPE(NUM_PRODUCTS)= &
                                           (/'5x16                ', &
                                            '6x16                ', &
                                            '7x16                ',   & !  TMS added 20041129 for TVA
                                            '5x8                 ', &
                                            '6x8                 ', &
                                            '7x8                 ', &
                                            'Wrap                ', &
                                            'Western Wrap        ', &
                                            '7x24                ',   & !  Eventually add 6x24 and 5x25 for Completeness
                                            '2x24                ',   & !  Eventually add Sax24 and Sux24 for Completness
                                            'Sax16               ', &
                                            'Sux16               ', &
                                            'Wkx16               ', &
                                            'Wkx8                ',   & !  TMS added 20041129 for TVA
                                            'Super Peak          '/)   ! Eventually add Sax8 and Sux8 for Completness
      INTEGER (KIND=4) :: DMG_TARGET_RECORD,TIG_TARGET_RECORD
      REAL (KIND=4) :: LOCAL_END_POINT,LOCAL_YEAR,LOCAL_INTERVAL
      REAL (KIND=4) :: LOCAL_DAY,DEPTH_PRICE(:,:,:),DEPTH_TRANS(:,:)
      REAL (KIND=4) :: DEPTH_MONTH_MARGINAL_COST(:,:)
      REAL (KIND=4) :: DEPTH_LAMBDA(:,:),DEPTH_MC_BY_HOUR(:,:,:)
      REAL (KIND=4) :: DEPTH_MONTH_PRICE(:,:),DEPTH_MONTH_LAMBDA(:,:)
      REAL (KIND=4) :: PRODUCT_HOURS(:,:),DEPTH_QUANTITY(:)
      REAL (KIND=4) :: TOP_INTERVAL_MC(24,31)
      CHARACTER (LEN=4) :: LOCAL_DEPTH_MARKET_CODE=".DMD"
      CHARACTER (LEN=4) :: LOCAL_TRANS_MWH_CODE=".TID"
      CHARACTER (LEN=5) :: GET_SCENAME
      CHARACTER (LEN=9) :: LOCAL_MONTH_NAME
      CHARACTER (LEN=20) :: LOCAL_MULTI_AREA_NAME,SAVE_MULTI_AREA_NAME
      CHARACTER (LEN=20) :: GET_GROUP_NAME
      CHARACTER (LEN=256) :: DMG_FILE_NAME,GET_RESULTS_DIRECTORY
      CHARACTER (LEN=256) :: TIG_FILE_NAME
      LOGICAL (KIND=4) :: DMG_FILE_EXISTS,TIG_FILE_EXISTS
      ALLOCATABLE :: DEPTH_PRICE,DEPTH_QUANTITY,DEPTH_TRANS
      ALLOCATABLE :: DEPTH_MONTH_MARGINAL_COST,DEPTH_MC_BY_HOUR
      ALLOCATABLE :: DEPTH_LAMBDA,DEPTH_MONTH_PRICE,DEPTH_MONTH_LAMBDA
      ALLOCATABLE :: PRODUCT_HOURS
!
! R_UNIT_PRICE = MARGINAL COST OF THE CURRENT UNIT BLOCK
! R_UNIT_CAPACITY = CAPACITY OF THE CURRENT UNIT BLOCK
! R_QUANTITY = GENERATION USED - NATIVE DEMAND
!
! STRATEGY: THE INCOMING R_UNIT IS MOVING UP THE DISPATCH ORDER.
!           INSIDE THIS ROUTINE WE KNOW THE MARGINAL COST BETWEEN
!           THE LOWER AND UPPER DEPTH OF MARKET BOUNDS.
!           FOR EACH UNIT, WE TEST ITS POSITION WITHIN THE SYSTEM
!           BUY/SELL.  WE THEN SEE IF ITS POSITION ACROSS THE
!           DEPTH OF MARKET CURVE. IF SO, WE HAVE A DEPTH OF MARKET
!           SOLUTION.  IF NOT, WE KEEP ON LOOKING UNTIL WE ARE AT
!           THE TOP END OF THE DEPTH OF MARKET RANGE.

! END OF DATA DECLARATIONS

         GET_DEPTH_OF_MARKET_QUANTITY = .FALSE.

         DEMO_QUANTITY(1) = - 500.
         DEMO_QUANTITY(2) = - 250.
         DEMO_QUANTITY(3) =     5.
         DEMO_QUANTITY(4) =   250.
         DEMO_QUANTITY(5) =   500.
!
         MAX_INTERVALS = DEPTH_MARKET_INTERVALS
!
         IF( .NOT. ALLOCATED(DEPTH_PRICE)) RETURN

! TEST FOR OUTSIDE THE INTERVAL.
! WOULD I USE THESE IF NEVER FOUND? PROBABLY.
! TO CAPTURE RAMP-UP AND RAMP-DOWN ISSUES
         DO INTERVAL = MAX_INTERVALS, 1, -1
            IF(DEPTH_QUANTITY(INTERVAL) > R_MAX_DEPTH_MARKET) CYCLE
            TOP_INTERVAL = MAX(Int(2,2),MIN(INTERVAL,MAX_INTERVALS))
            EXIT
         ENDDO
         DO INTERVAL = 1, MAX_INTERVALS
            IF(DEPTH_QUANTITY(INTERVAL) < R_MIN_DEPTH_MARKET) CYCLE
            BOTTOM_INTERVAL = MIN(INTERVAL,TOP_INTERVAL-int(1,2))
            EXIT
         ENDDO

! DEPTH_QUANTITY(MAX_INTERVALS) : BUY MAX FROM THE MARKET
! DEPTH_PRICE(MAX_INTERVALS) :    MAX MARKET PRICE
! DEPTH_QUANTITY(1) : SELL MAX TO THE MARKET
! DEPTH_PRICE(1):    MIN MARKET PRICE



!BUY CONIDITION

         TOP_INTERVAL = MAX_INTERVALS
         BOTTOM_INTERVAL = 1
!
         INTERVAL = ZERO_PRICE_INTERVAL
!
         IF(R_LOCAL_BALANCE + DEPTH_QUANTITY(TOP_INTERVAL) < 0.) THEN
!
!              INSUFFICIENT NATIVE RESOURCES AVAILABLE TO MEET DEMAND
!
            LOCAL_DEPTH_QUANTITY = DEPTH_QUANTITY(TOP_INTERVAL)
            LOCAL_DEPTH_PRICE = &
                                  DEPTH_PRICE(TOP_INTERVAL,R_HOUR,R_DAY)
!
!
!              STOP SEARCH AND BUY THE MAX AT THE MAX PRICE
!
            GET_DEPTH_OF_MARKET_QUANTITY = .TRUE.
         ENDIF

!
         LOCAL_NET_LOAD = - R_SYSTEM_LOAD - DEPTH_TRANS(R_HOUR,R_DAY)
!
         DO I = 2 , R_MAX_OUTAGE_BLOCKS
!
            IF(GET_DEPTH_OF_MARKET_QUANTITY) EXIT
!
            LOCAL_QUANTITY(1) = R_DISPATCH_MW(I-1) + LOCAL_NET_LOAD
            LOCAL_QUANTITY(2) = R_DISPATCH_MW(I) + LOCAL_NET_LOAD
!
            IF(LOCAL_QUANTITY(1) < 0. .AND. LOCAL_QUANTITY(2) > 0.) THEN
               R_MAX_OUTAGE_BLOCKS = R_MAX_OUTAGE_BLOCKS
            ENDIF
!
!
            IF(LOCAL_QUANTITY(2) + &
                                DEPTH_QUANTITY(TOP_INTERVAL) < 0.) THEN
!
! BUY CONDITION
!
!
!              CURRENT DISPATCH LESS NATIVE LOAD (NEED) IS ABOVE
!              THE TOP END OF THE BUY RANGE
!
               LOCAL_DEPTH_QUANTITY = DEPTH_QUANTITY(TOP_INTERVAL)
               LOCAL_DEPTH_PRICE = &
                                  DEPTH_PRICE(TOP_INTERVAL,R_HOUR,R_DAY)
!
            ELSEIF(LOCAL_QUANTITY(1) + &
                            DEPTH_QUANTITY(BOTTOM_INTERVAL) > 0. .AND. &
                     R_DISPATCH_COST(I) < &
                         DEPTH_PRICE(BOTTOM_INTERVAL,R_HOUR,R_DAY)) THEN
!
! SELL AND TERMINATE CONDITION
! MODIFIED 01/30/03 TO INC UNIT VS DEPTH PRICE CHECK.
!
!           CURRENT DISPATCH ABOVE UPPER END OF DEPTH OF MARKET CURVE
!
               LOCAL_DEPTH_QUANTITY = DEPTH_QUANTITY(BOTTOM_INTERVAL)
               LOCAL_DEPTH_PRICE = &
                               DEPTH_PRICE(BOTTOM_INTERVAL,R_HOUR,R_DAY)
!
!           STOP SEARCH AND SELL THE MAX AT THE MIN PRICE
!
               GET_DEPTH_OF_MARKET_QUANTITY = .TRUE.

            ELSE

               DO INTERVAL = BOTTOM_INTERVAL+1, TOP_INTERVAL
                  IF(DEPTH_PRICE(INTERVAL-1,R_HOUR,R_DAY) <= &
              R_DISPATCH_COST(MIN(I+int(1,2),R_MAX_OUTAGE_BLOCKS)) .AND. &
              DEPTH_PRICE(INTERVAL,R_HOUR,R_DAY) >= &
                                              R_DISPATCH_COST(I)) THEN

! IS THE UNIT CONTAINED WITHIN THE INTERVAL?
!
! TRY TO TAKE THE UNIT (LOCAL_QUANTITY(2)) FIRST.
!
!
                     IF( (DEPTH_QUANTITY(INTERVAL-1) <= &
                                    -LOCAL_QUANTITY(1) .AND. &
                              -LOCAL_QUANTITY(1) <= &
                                         DEPTH_QUANTITY(INTERVAL)) .OR. &
                        (DEPTH_QUANTITY(INTERVAL-1) <= &
                                    -LOCAL_QUANTITY(2) .AND. &
                              -LOCAL_QUANTITY(2) <= &
                                        DEPTH_QUANTITY(INTERVAL)) ) THEN
                        IF(DEPTH_QUANTITY(INTERVAL-1) <= &
                                    -LOCAL_QUANTITY(2) .AND. &
                              -LOCAL_QUANTITY(2) <= &
                                        DEPTH_QUANTITY(INTERVAL)) THEN
                           TEMP_CAP = - R_DISPATCH_MW(I) + R_SYSTEM_LOAD
!                           TEMP_CAP = -LOCAL_QUANTITY(2)
                        ELSE
                           TEMP_CAP = - R_DISPATCH_MW(I-1) + &
                                                           R_SYSTEM_LOAD
!                           TEMP_CAP = -LOCAL_QUANTITY(1)
                        ENDIF
!
                        SLOPE = DEPTH_QUANTITY(INTERVAL) - &
                                              DEPTH_QUANTITY(INTERVAL-1)
                        IF(SLOPE > 0.) THEN
                           SLOPE = &
                              (DEPTH_PRICE(INTERVAL-1,R_HOUR,R_DAY) - &
                               DEPTH_PRICE(INTERVAL,R_HOUR,R_DAY))/SLOPE
                        ELSE
                           SLOPE = 0.
                        ENDIF
! 01/29/03. CHANGED LOCAL_QUANTITY(1) TO TEMP_CAP
                        TEMP_PRICE = &
                           DEPTH_PRICE(INTERVAL-1,R_HOUR,R_DAY) + &
                           SLOPE* &
                                 (DEPTH_QUANTITY(INTERVAL-1) - TEMP_CAP)
!
                        LOCAL_DEPTH_QUANTITY = TEMP_CAP
                        LOCAL_DEPTH_PRICE = TEMP_PRICE
                        GET_DEPTH_OF_MARKET_QUANTITY = .TRUE.
                        EXIT




                     ENDIF ! QUANTITY CHECK
                  ELSEIF(DEPTH_PRICE(INTERVAL-1,R_HOUR,R_DAY) > &
                     R_DISPATCH_COST(I) .AND. &
                     DEPTH_PRICE(INTERVAL,R_HOUR,R_DAY) < &
                   R_DISPATCH_COST(MIN(I+int(1,2),R_MAX_OUTAGE_BLOCKS))) &
                                                                    THEN
!
                     LOCAL_DEPTH_QUANTITY = DEPTH_QUANTITY(INTERVAL-1)
                     LOCAL_DEPTH_PRICE = &
                                    DEPTH_PRICE(INTERVAL-1,R_HOUR,R_DAY)
                     GET_DEPTH_OF_MARKET_QUANTITY = .TRUE.
                     EXIT
                  ENDIF ! PRICE CHECK
               ENDDO
!
            ENDIF
!
!
            R_DEPTH_MARGINAL_COST = R_DISPATCH_COST(I-1)
            R_DEPTH_RESOURCES = R_DISPATCH_MW(I)
         ENDDO ! UNIT BLOCK LOOP COUNTER
!
         R_DEPTH_QUANTITY = LOCAL_DEPTH_QUANTITY
         R_DEPTH_PRICE = LOCAL_DEPTH_PRICE
!
! 04/08/03. DEVELOP MARGINAL COSTS AROUND I
!

         L = 2
         DO I = 1, DEPTH_MARKET_INTERVALS
!
! SEARCH FOR THE UNIT THAT FALLS WITHIN THE INTERVAL (K INDEX)
!

               TARGET_CAPACITY = R_SYSTEM_LOAD + &
                           DEPTH_TRANS(R_HOUR,R_DAY) + DEPTH_QUANTITY(I)

!
            DO K = L, R_MAX_OUTAGE_BLOCKS
               IF(R_DISPATCH_MW(K) < TARGET_CAPACITY) CYCLE
               EXIT
            ENDDO
! 02/02/04.
            L = MAX(int(1,2),MIN(K-int(1,2),R_MAX_OUTAGE_BLOCKS))
!

!
            LOCAL_DEPTH_PRICE = DEPTH_PRICE(I,R_HOUR,R_DAY)
!
            IF(DMD_COEFF_A > -9990.) THEN
               LOCAL_DEPTH_PRICE = LOCAL_DEPTH_PRICE* &
                           (DMD_COEFF_A + DMD_COEFF_B*DEPTH_QUANTITY(I))
            ENDIF
            TEMP_R = MIN(LOCAL_DEPTH_PRICE,R_DISPATCH_COST(L))
            DEPTH_MC_BY_HOUR(I,R_HOUR,R_DAY) = TEMP_R
!
            DO J = 1, NUM_PRODUCTS
               IF(APPLY_ENERGY_PRODUCT(R_HOUR, &
                                    R_CALENDAR_DAY_OF_WEEK, &
                                    LOCAL_PRODUCT_TYPE(J))) THEN
!
! 12/08/03. FOR JONES. TO CAPTURE SYSTEM LAMBDA FOR THIS REPORT.
!

                     DEPTH_MONTH_MARGINAL_COST(J,I) = &
                        DEPTH_MONTH_MARGINAL_COST(J,I) + TEMP_R

                  PRODUCT_HOURS(J,I) = PRODUCT_HOURS(J,I) + 1.
               ENDIF
            ENDDO ! PRODUCTS
!
            IF(I == DEPTH_MARKET_INTERVALS) THEN
               TOP_INTERVAL_MC(R_HOUR,R_DAY) = R_DISPATCH_COST(L)
            ENDIF
         ENDDO ! INTERVALS
         LAMBDA_INTERVAL = ZERO_PRICE_INTERVAL
         IF(R_DEPTH_QUANTITY <= 0.) THEN
            DEPTH_LAMBDA(R_HOUR,R_DAY) = &
                      DEPTH_MC_BY_HOUR(ZERO_PRICE_INTERVAL,R_HOUR,R_DAY)
         ELSEIF(R_DEPTH_QUANTITY > &
                            DEPTH_QUANTITY(DEPTH_MARKET_INTERVALS)) THEN
            DEPTH_LAMBDA(R_HOUR,R_DAY) = &
                   DEPTH_MC_BY_HOUR(DEPTH_MARKET_INTERVALS,R_HOUR,R_DAY)
         ELSE ! LINEARLY INTERPOLATE BETWEEN POINTS
!
            DO I = ZERO_PRICE_INTERVAL+1, DEPTH_MARKET_INTERVALS
!
               IF(R_DEPTH_QUANTITY > DEPTH_QUANTITY(I)) CYCLE
!
               IF(R_DEPTH_QUANTITY == DEPTH_QUANTITY(I)) THEN
                  DEPTH_LAMBDA(R_HOUR,R_DAY) = &
                                        DEPTH_MC_BY_HOUR(I,R_HOUR,R_DAY)
               ELSE
                  TEMP_R = (R_DEPTH_QUANTITY - DEPTH_QUANTITY(I-1))/ &
                               (DEPTH_QUANTITY(I)-DEPTH_QUANTITY(I-1))
                  DEPTH_LAMBDA(R_HOUR,R_DAY) = &
                        TEMP_R*DEPTH_MC_BY_HOUR(I,R_HOUR,R_DAY) + &
                         (1.-TEMP_R)* DEPTH_MC_BY_HOUR(I-1,R_HOUR,R_DAY)
               ENDIF
               LAMBDA_INTERVAL = I
               EXIT
            END DO
!
         ENDIF

         IF(R_DEPTH_QUANTITY <= DEPTH_QUANTITY(1)) THEN
            INTERVAL = 1
         ELSEIF(R_DEPTH_QUANTITY >= &
                            DEPTH_QUANTITY(DEPTH_MARKET_INTERVALS)) THEN
            INTERVAL = DEPTH_MARKET_INTERVALS
         ELSE
            DO I = 2, DEPTH_MARKET_INTERVALS
               IF(R_DEPTH_QUANTITY > DEPTH_QUANTITY(I)) CYCLE
               INTERVAL = I
               EXIT
            ENDDO
         ENDIF
         DO INTERVAL = 1, DEPTH_MARKET_INTERVALS
            DO J = 1, NUM_PRODUCTS
               IF(APPLY_ENERGY_PRODUCT(R_HOUR, &
                                    R_CALENDAR_DAY_OF_WEEK, &
                                    LOCAL_PRODUCT_TYPE(J))) THEN

                     DEPTH_MONTH_LAMBDA(J,INTERVAL) = &
                           DEPTH_MONTH_LAMBDA(J,INTERVAL)+ &
                                              DEPTH_LAMBDA(R_HOUR,R_DAY)

               ENDIF
!
!
            ENDDO
         ENDDO
!
!
      RETURN

      ENTRY WRITE_DEPTH_MARGINAL_COST(R_END_POINT, &
                                      R_THIS_YEAR, &
                                      R_MONTH, &
                                      R_TRANS_GROUP_NAME, &
                                      R_DAYS_IN_MONTH)

         C_DEPTH_MARGIN_REPORT = .TRUE.
         IF(C_DEPTH_MARGIN_REPORT) THEN
!
            IF(C_DEPTH_MARGIN_NOT_OPEN) THEN
               C_DEPTH_MARGIN_NOT_OPEN = .FALSE.
               C_DEPTH_MARGIN_UNIT = &
                           C_DEPTH_MARGIN_RPT_HEADER(C_DEPTH_MARGIN_REC, &
                                                           NUM_PRODUCTS)
               C_DEPTH_LAMBDA_UNIT = &
                           GY_HOURLY_HEADER(C_DEPTH_LAMBDA_REC)
            ENDIF
!
            DO I = 1, DEPTH_MARKET_INTERVALS
               DO J = 1, NUM_PRODUCTS
!
                  IF(PRODUCT_HOURS(J,I) > 0.) THEN
                     DEPTH_MONTH_MARGINAL_COST(J,I) = &
                                    DEPTH_MONTH_MARGINAL_COST(J,I)/ &
                                                      PRODUCT_HOURS(J,I)
                     DEPTH_MONTH_PRICE(J,I) = &
                                    DEPTH_MONTH_PRICE(J,I)/ &
                                                      PRODUCT_HOURS(J,I)
                     DEPTH_MONTH_LAMBDA(J,I) = &
                                    DEPTH_MONTH_LAMBDA(J,I)/ &
                                                      PRODUCT_HOURS(J,I)
                  ENDIF
               ENDDO

               WRITE(C_DEPTH_MARGIN_UNIT,REC=C_DEPTH_MARGIN_REC) &
                              FLOAT(R_END_POINT), &
                              R_THIS_YEAR, &
                              LOCAL_MONTH_NAME, &
                              R_TRANS_GROUP_NAME(1:14)//' MCOST', &
                              DEPTH_QUANTITY(I), &
                              (DEPTH_MONTH_MARGINAL_COST(J,I), &
                                                       J=1,NUM_PRODUCTS)
!
               C_DEPTH_MARGIN_REC = C_DEPTH_MARGIN_REC + 1
               WRITE(C_DEPTH_MARGIN_UNIT,REC=C_DEPTH_MARGIN_REC) &
                              FLOAT(R_END_POINT), &
                              R_THIS_YEAR, &
                              LOCAL_MONTH_NAME, &
                              R_TRANS_GROUP_NAME(1:14)//'Lambda', &
                              DEPTH_QUANTITY(I), &
                              (DEPTH_MONTH_LAMBDA(J,I), &
                                                       J=1,NUM_PRODUCTS)
!
               C_DEPTH_MARGIN_REC = C_DEPTH_MARGIN_REC + 1
!
               WRITE(C_DEPTH_MARGIN_UNIT,REC=C_DEPTH_MARGIN_REC) &
                              FLOAT(R_END_POINT), &
                              R_THIS_YEAR, &
                              LOCAL_MONTH_NAME, &
                              'Hours Product Depth ', &
                              DEPTH_QUANTITY(I), &
                              (PRODUCT_HOURS(J,I),J=1,NUM_PRODUCTS)
!
               C_DEPTH_MARGIN_REC = C_DEPTH_MARGIN_REC + 1
            ENDDO

            DO DAY = 1, R_DAYS_IN_MONTH
!
               WRITE(C_DEPTH_LAMBDA_UNIT,REC=C_DEPTH_LAMBDA_REC) &
                              FLOAT(R_END_POINT), &
                              R_THIS_YEAR, &
                              LOCAL_MONTH_NAME, &
                              FLOAT(DAY), &
                              (DEPTH_LAMBDA(HOUR,DAY),HOUR=1,24)
                  C_DEPTH_LAMBDA_REC = C_DEPTH_LAMBDA_REC + 1
            END DO

         ENDIF
         WRITE_DEPTH_MARGINAL_COST = .TRUE.
      RETURN

      ENTRY MONTHLY_DEPTH_OF_MARKET_DB( &
                        R_END_POINT,R_YEAR,R_MONTH,R_TG,R_DAYS_IN_MONTH, &
                        R_DEPTH_0_PRICE)

!
         TEMP_L = GET_DMD_COEFF(DMD_COEFF_A,DMD_COEFF_B)
!
         MAX_DEPTH_PROFIT = -999999999.
         LOCAL_DEPTH_PROFIT = MAX_DEPTH_PROFIT - 1.
!
         YES_DEPTH_OF_MARKET = &
                             DEPTH_OF_MARKET(DEPTH_MARKET_INTERVALS, &
                                             DEPTH_MARKET_INTERVAL_SIZE, &
                                             DEPTH_TARGET_GROUP)
         YES_REPORT_NAMES = &
                     DEPTH_MARKET_REPORTS_ACTIVE(DEPTH_MARKET_FILE_NAME)
         DMG_FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                          trim(DEPTH_MARKET_FILE_NAME)// &
                                                 LOCAL_DEPTH_MARKET_CODE
         INQUIRE(FILE=DMG_FILE_NAME,EXIST=DMG_FILE_EXISTS)
!
         TIG_FILE_NAME = trim(GET_RESULTS_DIRECTORY())//"MSG"// &
                             trim(DEPTH_MARKET_FILE_NAME)// &
                                                    LOCAL_TRANS_MWH_CODE
         INQUIRE(FILE=TIG_FILE_NAME,EXIST=TIG_FILE_EXISTS)
!
         IF(.NOT. DMG_FILE_EXISTS .OR. &
!      +         .NOT. TIG_FILE_EXISTS .OR.
                       .NOT. YES_DEPTH_OF_MARKET) THEN
!     +                        DEPTH_TARGET_GROUP /= R_TG) THEN
            MONTHLY_DEPTH_OF_MARKET_DB = .FALSE.
         ELSE
!
            R_DEPTH_0_PRICE = 0.
!
            DEPTH_MARKET_UNIT = 11
            TRANS_MWH_UNIT = 12
!
            IF(FILE_NEVER_OPENED) THEN
               FILE_NEVER_OPENED = .FALSE.
               SAVE_MULTI_AREA_NAME = GET_GROUP_NAME(DEPTH_TARGET_GROUP)
               IF(LAHEY_LF95()) THEN
                  DMG_TARGET_RECORD = 9
                  TIG_TARGET_RECORD = 8
               ELSE
                  DMG_TARGET_RECORD = 8
                  TIG_TARGET_RECORD = 7
               ENDIF
            ENDIF
!
            OPEN(UNIT=DEPTH_MARKET_UNIT, &
                     FILE=DMG_FILE_NAME,FORM='UNFORMATTED',RECL=121, &
                                           ACCESS="DIRECT",STATUS="OLD")
!
            IF(ALLOCATED(DEPTH_PRICE)) &
                      DEALLOCATE(DEPTH_PRICE,DEPTH_QUANTITY,DEPTH_TRANS, &
                                    DEPTH_MONTH_MARGINAL_COST, &
                                    DEPTH_MC_BY_HOUR, &
                                    DEPTH_LAMBDA, &
                                    DEPTH_MONTH_PRICE, &
                                    DEPTH_MONTH_LAMBDA, &
                                    PRODUCT_HOURS)
            ALLOCATE( DEPTH_PRICE(DEPTH_MARKET_INTERVALS, &
                                                    24,R_DAYS_IN_MONTH), &
                       DEPTH_QUANTITY(DEPTH_MARKET_INTERVALS), &
                       DEPTH_TRANS(24,R_DAYS_IN_MONTH), &
                       DEPTH_MONTH_MARGINAL_COST(NUM_PRODUCTS, &
                                                DEPTH_MARKET_INTERVALS), &
                       DEPTH_MC_BY_HOUR(DEPTH_MARKET_INTERVALS, &
                                                    24,R_DAYS_IN_MONTH), &
                       DEPTH_LAMBDA(24,R_DAYS_IN_MONTH), &
                       DEPTH_MONTH_PRICE(NUM_PRODUCTS, &
                                                DEPTH_MARKET_INTERVALS), &
                       DEPTH_MONTH_LAMBDA(NUM_PRODUCTS, &
                                                DEPTH_MARKET_INTERVALS), &
                       PRODUCT_HOURS(NUM_PRODUCTS, &
                                                DEPTH_MARKET_INTERVALS))
!
            TOP_INTERVAL_MC = 0.
!
            DEPTH_PRICE = 0.
            DEPTH_MC_BY_HOUR = 0.
            DEPTH_LAMBDA = 0.
            DEPTH_QUANTITY = 0.
            DEPTH_TRANS = 0.
            DEPTH_MONTH_MARGINAL_COST = 0.
            DEPTH_MONTH_PRICE = 0.
            DEPTH_MONTH_LAMBDA = 0.
            PRODUCT_HOURS = 0.
!
            ZERO_PRICE_INTERVAL = INT(DEPTH_MARKET_INTERVALS/2) +1

            DO DAY = 1, R_DAYS_IN_MONTH
               DO I = 1, DEPTH_MARKET_INTERVALS

                  READ(DEPTH_MARKET_UNIT,REC=DMG_TARGET_RECORD) &
                        LOCAL_END_POINT, &
                        LOCAL_YEAR, &
                        LOCAL_MONTH_NAME, &
                        LOCAL_DAY, &
                        DEPTH_QUANTITY(I), &
                        (DEPTH_PRICE(I,HR,DAY),HR=1,24)
                  DMG_TARGET_RECORD = DMG_TARGET_RECORD + 1
!
                  IF(LOCAL_MONTH_NAME == 'February ' .AND. &
                                                  LOCAL_DAY == 29) THEN
                     DMG_TARGET_RECORD = DMG_TARGET_RECORD + &
                                              DEPTH_MARKET_INTERVALS - 1
                     READ(DEPTH_MARKET_UNIT,REC=DMG_TARGET_RECORD) &
                        LOCAL_END_POINT, &
                        LOCAL_YEAR, &
                        LOCAL_MONTH_NAME, &
                        LOCAL_DAY, &
                        DEPTH_QUANTITY(I), &
                        (DEPTH_PRICE(I,HR,DAY),HR=1,24)
                     DMG_TARGET_RECORD = DMG_TARGET_RECORD + 1
                     CYCLE
                  ENDIF

                  IF(I == ZERO_PRICE_INTERVAL) THEN
                     DO HR = 1, 24
                        HOUR = (DAY-1)*24 + HR
                        R_DEPTH_0_PRICE(HOUR) = &
                                 DEPTH_PRICE(ZERO_PRICE_INTERVAL,HR,DAY)
                     ENDDO
                  ENDIF
               ENDDO
            ENDDO
! TIG IS NOT NECESSARY
            IF(TIG_FILE_EXISTS) THEN
               OPEN(UNIT=TRANS_MWH_UNIT, &
                     FILE=TIG_FILE_NAME,FORM='UNFORMATTED',RECL=137, &
                                           ACCESS="DIRECT",STATUS="OLD")
               DO DAY = 1, R_DAYS_IN_MONTH
               ! TODO:  Fix so that only needed indices are iterated.
               ! Do this by fixing the need to prevent infinite loops
               ! in this manner.
                  DO J = 1, 100 ! LIMIT ITERATIONS SO WE DON'T HAVE INFINATE LOOP
                     READ(TRANS_MWH_UNIT,REC=TIG_TARGET_RECORD) &
                        LOCAL_END_POINT, &
                        LOCAL_YEAR, &
                        LOCAL_MONTH_NAME, &
                        LOCAL_DAY, &
                        LOCAL_MULTI_AREA_NAME, &
                        (DEPTH_TRANS(HR,DAY),HR=1,24)
                     TIG_TARGET_RECORD = TIG_TARGET_RECORD + 1
                     IF(LOCAL_MULTI_AREA_NAME /= &
                                             SAVE_MULTI_AREA_NAME) CYCLE
                     EXIT
                  ENDDO
               ENDDO
               CLOSE(TRANS_MWH_UNIT)
            ENDIF
            MONTHLY_DEPTH_OF_MARKET_DB = .TRUE.
            CLOSE(DEPTH_MARKET_UNIT)
         ENDIF
!
      RETURN
      END

!
!                  ROUTINE TO CREATE A FUEL DERIVATIVE FILE
!
!                           COPYRIGHT (C) 2002
!                        M.S. GERBER & ASSOCIATES, INC.
!                           ALL RIGHTS RESERVED
!

!
      SUBROUTINE FUEL_DERIVATIVES_OBJECT
      use end_routine, only: end_program, er_message
      use filename_tracker
      USE SIZECOM
      use SpinDriftLib
      USE PROD_ARRAYS_DIMENSIONS
      SAVE

      INTEGER (KIND=2) :: DELETE,INUNIT,IREC,LRECL=214
      INTEGER :: IOS
      INTEGER (KIND=2) ::  UNIT_NUM=10
      CHARACTER (LEN=5) :: BASE_FILE_NAME,OVERLAY_FAMILY_NAME
      CHARACTER (LEN=5) :: FUEL_DERIVATIVES_FILE
      CHARACTER (LEN=256) :: FILE_NAME
      CHARACTER (LEN=256) :: BASE_FILE_DIRECTORY
      CHARACTER (LEN=256) :: DATA_DRIVE,OUTPUT_DIRECTORY
      LOGICAL (KIND=4) :: FILE_EXISTS,R_FUEL_DERIVATIVES_FILE_EXISTS
      LOGICAL (KIND=4) :: FUEL_DERIVATIVES_FILE_EXISTS=.FALSE.
!
      CHARACTER (LEN=50) :: COMMENT
      CHARACTER (LEN=20) :: TRANSACTION_NAME,COUNTERPARTY_NAME
      CHARACTER (LEN=20) :: TRANSACTION_TYPE,TRANSACTION_DESCRIPTION
      CHARACTER (LEN=3) :: COUNTERPARTY_BOND_RATING
      CHARACTER (LEN=1) :: PRODUCT_ACTIVE,REPORT_DERIVATIVE
      CHARACTER (LEN=1) :: COST_ASSIGNMENT,EXPENSE_COLLECTION,PRICE_TYPE
      CHARACTER (LEN=1) :: SELL_EXCESS_PHYSICAL_FUEL,FUEL_TYPE
      CHARACTER (LEN=1) :: TRANSPORTATION_BASIS_TYPE
      CHARACTER (LEN=1) :: THERMAL_UNIT_FUEL_LINK
      INTEGER (KIND=2) :: ASSET_CLASS_ID,ASSET_ALLOCATION_VECTOR
      INTEGER (KIND=2) :: THERMAL_UNIT_LINK,DOLLARS_PER_MMBTU_ESC_VEC
      INTEGER (KIND=2) :: DOLLARS_PER_DEAL_ESC_VECTOR
      INTEGER (KIND=2) :: BASE_TRANSACTIONS_IN_FILE=0
      INTEGER (KIND=2) :: R_BASE_TRANSACTIONS
      INTEGER (KIND=2) :: OVERLAY_TRANSACTIONS_IN_FILE=0
      INTEGER (KIND=2) :: MAX_BASE_TRANS_GROUP_NUM=0
      INTEGER (KIND=2) :: MAX_OVL_TRANS_GROUP_NUM=0
      INTEGER (KIND=2) :: R_MAX_TRANS_GROUP_NUM
!
      INTEGER (KIND=2) :: R_NUM_OF_TRANS_CLASSES
      INTEGER (KIND=2) :: NUM_OF_OL_TRANS_CLASSES=0
      INTEGER (KIND=2) :: R_MAX_TRANS_CLASS_NUM
      INTEGER (KIND=2) :: MAX_OL_TRANS_CLASS_ID_NUM=0
      INTEGER (KIND=2) :: R_NUM_OF_ASSET_CLASSES
      INTEGER (KIND=2) :: NUM_OF_OL_ASSET_CLASSES=0
      INTEGER (KIND=2) :: R_MAX_ASSET_CLASS_NUM
      INTEGER (KIND=2) :: MAX_OL_ASSET_CLASS_ID_NUM=0
      INTEGER (KIND=2) :: R_NUM_OF_CUST_CLASSES
      INTEGER (KIND=2) :: NUM_OF_OL_CUST_CLASSES=0
      INTEGER (KIND=2) :: R_MAX_CUST_CLASS_NUM
      INTEGER (KIND=2) :: MAX_OL_CUST_CLASS_ID_NUM=0
      INTEGER (KIND=2) :: NUM_OF_BC_TRANS_CLASSES=0
      INTEGER (KIND=2) :: MAX_BC_TRANS_CLASS_ID_NUM=0
      INTEGER (KIND=2) :: NUM_OF_BC_ASSET_CLASSES=0
      INTEGER (KIND=2) :: MAX_BC_ASSET_CLASS_ID_NUM=0
      INTEGER (KIND=2) :: NUM_OF_BC_CUST_CLASSES=0
      INTEGER (KIND=2) :: MAX_BC_CUST_CLASS_ID_NUM=0
      INTEGER (KIND=2) :: TEMP_TRANS_CLASS_POINTER(:)
      INTEGER (KIND=2) :: TEMP_ASSET_CLASS_POINTER(:)
      INTEGER (KIND=2) :: TEMP_CUST_CLASS_POINTER(:)
      ALLOCATABLE :: TEMP_TRANS_CLASS_POINTER,TEMP_ASSET_CLASS_POINTER, &
                  TEMP_CUST_CLASS_POINTER
      INTEGER (KIND=4) :: CONTRACT_DATE,CONTRACT_BEGIN_DATE
      INTEGER (KIND=4) :: CONTRACT_END_DATE
      REAL (KIND=4) :: MARKET_PRICE,SPOT_TRANSPORTATION_BASIS
      REAL (KIND=4) :: SPOT_DELIVERY_ADDER,MONTHLY_QUANTITY
      REAL (KIND=4) :: MONTHLY_MULTIPLIER,FIXED_PRICE,DELIVERY_ADDER
      REAL (KIND=4) :: STORAGE_FEE,PREMIMUM_DOLLARS_PER_MMBTU
      REAL (KIND=4) :: PREMIMUM_DOLLARS_PER_DEAL,TRANSPORTATION_BASIS
      REAL (KIND=4) :: PHYSICAL_SALE_FEE
!
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER (LEN=1024) :: RECLN
! DECLARATION FOR DAY TYPE DETERMINANTS
      CHARACTER (LEN=16) :: FILE_TYPE='Fuel Derivatives'
      CHARACTER (LEN=2) :: FUEL_DERIVATIVES_OL='BC'
      LOGICAL (KIND=1) :: LAHEY_LF95
      CHARACTER (LEN=30) :: SCREEN_OUTPUT

! CONVERT THE DAY_TYPE FILE

      ENTRY FUEL_DERIVATIVES_MAKEBIN

      BASE_FILE_NAME = FUEL_DERIVATIVES_FILE()
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME = get_gpb_filename(base_file_name)
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      FUEL_DERIVATIVES_FILE_EXISTS = FILE_EXISTS
      IF(FILE_EXISTS) THEN
!
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_LOCATE_WRITE(16,30,BASE_FILE_NAME,ALL_VERSIONS,0)
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         ENDIF
         NUM_OF_BC_TRANS_CLASSES = 0
         MAX_BC_TRANS_CLASS_ID_NUM = 0
         NUM_OF_BC_ASSET_CLASSES = 0
         MAX_BC_ASSET_CLASS_ID_NUM = 0
         NUM_OF_BC_CUST_CLASSES = 0
         MAX_BC_CUST_CLASS_ID_NUM = 0
!
         ALLOCATE(   TEMP_TRANS_CLASS_POINTER(0:1023), &
                     TEMP_ASSET_CLASS_POINTER(0:1023), &
                     TEMP_CUST_CLASS_POINTER(0:1023))
         TEMP_TRANS_CLASS_POINTER = 0
         TEMP_ASSET_CLASS_POINTER = 0
         TEMP_CUST_CLASS_POINTER = 0

         OPEN(10,FILE=FILE_NAME) ! gpb
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCGPROD.BIN", &
                            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         READ(10,*) DELETE

         IREC = 0
         PRODUCT_ACTIVE = 'T'
         REPORT_DERIVATIVE = 'T'
         MARKET_PRICE = 0.
         SPOT_TRANSPORTATION_BASIS = 0.
         SPOT_DELIVERY_ADDER = 0.
         THERMAL_UNIT_LINK = 0
         MONTHLY_MULTIPLIER = 1.
         FIXED_PRICE = 0.
         DELIVERY_ADDER = 0.
         TRANSPORTATION_BASIS = 0.

         DO

            READ(10,1000,IOSTAT=IOS) RECLN
            IF(IOS /= 0) EXIT
            IF(RECLN(1:1) == '7') CYCLE
            RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
            READ(RECLN,*,ERR=200) DELETE, &
            TRANSACTION_NAME,               & !  1
              PRODUCT_ACTIVE, &
              REPORT_DERIVATIVE, &
              COUNTERPARTY_NAME,        & !  4
              COUNTERPARTY_BOND_RATING, &
              TRANSACTION_TYPE,               & !  6
              TRANSACTION_DESCRIPTION, &
              CONTRACT_DATE, &
              CONTRACT_BEGIN_DATE, & !  9
              CONTRACT_END_DATE, &
              MARKET_PRICE, &
              SPOT_TRANSPORTATION_BASIS,  & !  12
              SPOT_DELIVERY_ADDER, &
              COST_ASSIGNMENT, &
              EXPENSE_COLLECTION,    & !  15
              ASSET_CLASS_ID, &
              ASSET_ALLOCATION_VECTOR, &
              THERMAL_UNIT_LINK,  & !  18
              MONTHLY_QUANTITY, &
              MONTHLY_MULTIPLIER, &
              PRICE_TYPE,            & !  21
              FIXED_PRICE, &
              DELIVERY_ADDER, &
              SELL_EXCESS_PHYSICAL_FUEL,      & !  24
              STORAGE_FEE, &
              PREMIMUM_DOLLARS_PER_MMBTU,                    & !  26
              DOLLARS_PER_MMBTU_ESC_VEC, &
              PREMIMUM_DOLLARS_PER_DEAL,       & !  28
              DOLLARS_PER_DEAL_ESC_VECTOR, &
              COMMENT, &
              FUEL_TYPE,             & !  31
              TRANSPORTATION_BASIS, &
              TRANSPORTATION_BASIS_TYPE,            & !  33
              PHYSICAL_SALE_FEE, &
              THERMAL_UNIT_FUEL_LINK
            IREC = IREC + 1

            IF(TEMP_ASSET_CLASS_POINTER(ASSET_CLASS_ID) == 0) THEN
               NUM_OF_BC_ASSET_CLASSES = NUM_OF_BC_ASSET_CLASSES + 1
               MAX_BC_ASSET_CLASS_ID_NUM = MAX( &
                            MAX_BC_ASSET_CLASS_ID_NUM,ASSET_CLASS_ID)
               TEMP_ASSET_CLASS_POINTER(ASSET_CLASS_ID) = 1
            ENDIF

            WRITE(11,REC=IREC) DELETE, &
            TRANSACTION_NAME, &
            PRODUCT_ACTIVE,  & !  2
                  REPORT_DERIVATIVE, &
                  COUNTERPARTY_NAME,                  & !  4
                  COUNTERPARTY_BOND_RATING, &
                  TRANSACTION_TYPE,            & !  6
                  TRANSACTION_DESCRIPTION, &
                  CONTRACT_DATE,                & !  8
                  CONTRACT_BEGIN_DATE, &
                  CONTRACT_END_DATE, &
                  MARKET_PRICE,   & !  11
                  SPOT_TRANSPORTATION_BASIS, &
                  SPOT_DELIVERY_ADDER,        & !  13
                  COST_ASSIGNMENT, &
                  EXPENSE_COLLECTION, &
                  ASSET_CLASS_ID,    & !  16
                  ASSET_ALLOCATION_VECTOR, &
                  THERMAL_UNIT_LINK,            & !  18
                  MONTHLY_QUANTITY, &
                  MONTHLY_MULTIPLIER, &
                  PRICE_TYPE,       & !  21
                  FIXED_PRICE, &
                  DELIVERY_ADDER, &
                  SELL_EXCESS_PHYSICAL_FUEL, & !  24
                  STORAGE_FEE, &
                  PREMIMUM_DOLLARS_PER_MMBTU,               & !  26
                  DOLLARS_PER_MMBTU_ESC_VEC, &
                  PREMIMUM_DOLLARS_PER_DEAL,  & !  28
                  DOLLARS_PER_DEAL_ESC_VECTOR, &
                  COMMENT, &
                  FUEL_TYPE,        & !  31
                  TRANSPORTATION_BASIS, &
                  TRANSPORTATION_BASIS_TYPE,       & !  33
                  PHYSICAL_SALE_FEE, &
                  THERMAL_UNIT_FUEL_LINK

         ENDDO ! RECORDS
         CLOSE(10)
         CLOSE(11)
!
         BASE_TRANSACTIONS_IN_FILE = IREC
!
      ELSE IF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "wh_objt:1114")
      ENDIF
      IF(ALLOCATED(TEMP_TRANS_CLASS_POINTER)) &
            DEALLOCATE(    TEMP_TRANS_CLASS_POINTER, &
                           TEMP_ASSET_CLASS_POINTER, &
                           TEMP_CUST_CLASS_POINTER)
      RETURN
! OVERLAY THE DAY TYPE FILE

      ENTRY FUEL_DERIVATIVES_MAKEOVL(OVERLAY_FAMILY_NAME)

!
      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME = get_gpo_filename(data_drive, overlay_family_name)
      OPEN(10,FILE=FILE_NAME) ! gpo
      READ(10,*) DELETE
      INUNIT = 12
      IF(FUEL_DERIVATIVES_OL == 'BC') THEN
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCGPROD.BIN", &
                                                        ACCESS="DIRECT", &
                                                             RECL=LRECL)
         INUNIT = 11
      ENDIF
      OPEN(12,FILE=trim(DATA_DRIVE)//"OLGPROD.BIN",ACCESS="DIRECT", &
                                            STATUS="UNKNOWN",RECL=LRECL)
      IREC = 0
!
      READ(10,1000,IOSTAT=IOS) RECLN
      DO WHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE
!
         READ(10,1000,IOSTAT=IOS) RECLN
      ENDDO

      NUM_OF_OL_TRANS_CLASSES = 0
      MAX_OL_TRANS_CLASS_ID_NUM = 0
      NUM_OF_OL_ASSET_CLASSES = 0
      MAX_OL_ASSET_CLASS_ID_NUM = 0
      NUM_OF_OL_CUST_CLASSES = 0
      MAX_OL_CUST_CLASS_ID_NUM = 0
!
      ALLOCATE(   TEMP_TRANS_CLASS_POINTER(0:1023), &
                     TEMP_ASSET_CLASS_POINTER(0:1023), &
                     TEMP_CUST_CLASS_POINTER(0:1023))
      TEMP_TRANS_CLASS_POINTER = 0
      TEMP_ASSET_CLASS_POINTER = 0
      TEMP_CUST_CLASS_POINTER = 0
!
      DO

         IF(IOS /= 0) EXIT
         IREC = IREC + 1
         READ(INUNIT,REC=IREC,IOSTAT=IOS) DELETE, &
            TRANSACTION_NAME,      & !  1
           PRODUCT_ACTIVE, &
            REPORT_DERIVATIVE,  &
            COUNTERPARTY_NAME, & !  4
            COUNTERPARTY_BOND_RATING, &
            TRANSACTION_TYPE, &
            TRANSACTION_DESCRIPTION, &
            CONTRACT_DATE, &
            CONTRACT_BEGIN_DATE,  & !  9
            CONTRACT_END_DATE, &
            MARKET_PRICE, &
            SPOT_TRANSPORTATION_BASIS, &!  12
            SPOT_DELIVERY_ADDER, &
            COST_ASSIGNMENT, &
            EXPENSE_COLLECTION, & !  15
            ASSET_CLASS_ID, &
            ASSET_ALLOCATION_VECTOR, &
            THERMAL_UNIT_LINK,& !  18
            MONTHLY_QUANTITY, &
            MONTHLY_MULTIPLIER, &
            PRICE_TYPE, &
            FIXED_PRICE,   & !  22
            DELIVERY_ADDER, &
            SELL_EXCESS_PHYSICAL_FUEL, &
            STORAGE_FEE, & !  25
            PREMIMUM_DOLLARS_PER_MMBTU, &
            DOLLARS_PER_MMBTU_ESC_VEC, & !  27
            PREMIMUM_DOLLARS_PER_DEAL, &
            DOLLARS_PER_DEAL_ESC_VECTOR, & !  29
            COMMENT, &
            FUEL_TYPE, &
            TRANSPORTATION_BASIS, & !  32
            TRANSPORTATION_BASIS_TYPE, &
            PHYSICAL_SALE_FEE, & ! 34
           THERMAL_UNIT_FUEL_LINK
         IF(IOS /= 0) EXIT

            RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
            READ(RECLN,*,ERR=300) DELETE, &
            TRANSACTION_NAME,              & !  1
              PRODUCT_ACTIVE,REPORT_DERIVATIVE, &
              COUNTERPARTY_NAME,       & !  4
              COUNTERPARTY_BOND_RATING, &
              TRANSACTION_TYPE,                & !  6
              TRANSACTION_DESCRIPTION, &
              CONTRACT_DATE, &
              CONTRACT_BEGIN_DATE, & !  9
              CONTRACT_END_DATE, &
              MARKET_PRICE, &
              SPOT_TRANSPORTATION_BASIS, & !  12
              SPOT_DELIVERY_ADDER, &
              COST_ASSIGNMENT, &
              EXPENSE_COLLECTION,   & !  15
              ASSET_CLASS_ID, &
              ASSET_ALLOCATION_VECTOR, &
              THERMAL_UNIT_LINK, & !  18
              MONTHLY_QUANTITY, &
              MONTHLY_MULTIPLIER, &
              PRICE_TYPE,           & !  21
              FIXED_PRICE, &
              DELIVERY_ADDER, &
              SELL_EXCESS_PHYSICAL_FUEL,     & !  24
              STORAGE_FEE, &
              PREMIMUM_DOLLARS_PER_MMBTU,                   & !  26
              DOLLARS_PER_MMBTU_ESC_VEC, &
              PREMIMUM_DOLLARS_PER_DEAL,      & !  28
              DOLLARS_PER_DEAL_ESC_VECTOR, &
              COMMENT, &
              FUEL_TYPE,            & !  31
              TRANSPORTATION_BASIS, &
              TRANSPORTATION_BASIS_TYPE,           & !  33
              PHYSICAL_SALE_FEE, &
              THERMAL_UNIT_FUEL_LINK

         READ(10,1000,IOSTAT=IOS) RECLN
         DO WHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE
!
            READ(10,1000,IOSTAT=IOS) RECLN
         ENDDO

         IF(TEMP_ASSET_CLASS_POINTER(ASSET_CLASS_ID) == 0) THEN
            NUM_OF_OL_ASSET_CLASSES = NUM_OF_OL_ASSET_CLASSES + 1
            MAX_OL_ASSET_CLASS_ID_NUM = MAX( &
                            MAX_OL_ASSET_CLASS_ID_NUM,ASSET_CLASS_ID)
            TEMP_ASSET_CLASS_POINTER(ASSET_CLASS_ID) = 1
         ENDIF

         WRITE(12,REC=IREC) DELETE, &
         TRANSACTION_NAME, &
         PRODUCT_ACTIVE,     & !  2
           REPORT_DERIVATIVE, &
           COUNTERPARTY_NAME, &
           COUNTERPARTY_BOND_RATING, & !  5
           TRANSACTION_TYPE, &
           TRANSACTION_DESCRIPTION, &
           CONTRACT_DATE,      & !  8
           CONTRACT_BEGIN_DATE, &
           CONTRACT_END_DATE, &
           MARKET_PRICE,          & !  11
           SPOT_TRANSPORTATION_BASIS, &
           SPOT_DELIVERY_ADDER,               & !  13
           COST_ASSIGNMENT, &
           EXPENSE_COLLECTION, &
           ASSET_CLASS_ID,           & !  16
           ASSET_ALLOCATION_VECTOR, &
           THERMAL_UNIT_LINK, &
           MONTHLY_QUANTITY,  & !  19
           MONTHLY_MULTIPLIER, &
           PRICE_TYPE, &
           FIXED_PRICE, &
           DELIVERY_ADDER,    & !  23
           SELL_EXCESS_PHYSICAL_FUEL, &
           STORAGE_FEE,                       & !  25
           PREMIMUM_DOLLARS_PER_MMBTU, &
           DOLLARS_PER_MMBTU_ESC_VEC,        & !  27
           PREMIMUM_DOLLARS_PER_DEAL, &
           DOLLARS_PER_DEAL_ESC_VECTOR,       & !  29
           COMMENT, &
           FUEL_TYPE, &
           TRANSPORTATION_BASIS,                      & !  32
           TRANSPORTATION_BASIS_TYPE, &
           PHYSICAL_SALE_FEE,                 & !  34
           THERMAL_UNIT_FUEL_LINK

      ENDDO
      CLOSE(10)
      CLOSE(12)
!
      OVERLAY_TRANSACTIONS_IN_FILE = IREC
!
      IF(FUEL_DERIVATIVES_OL == 'BC') CLOSE(11)
      FUEL_DERIVATIVES_OL = 'OL'
!
      IF(ALLOCATED(TEMP_TRANS_CLASS_POINTER)) &
            DEALLOCATE(    TEMP_TRANS_CLASS_POINTER, &
                           TEMP_ASSET_CLASS_POINTER, &
                           TEMP_CUST_CLASS_POINTER)
!
      RETURN
!

  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from wh_objt SIID366'
      call end_program(er_message)

  300 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      CALL MG_LOCATE_WRITE(21,0, &
                  'Error reading the Fuel Derivative record. Look for'// &
                                          ' a "," in a character name.', &
                           ALL_VERSIONS,1)
      er_message='stop requested from wh_objt SIID367'
      call end_program(er_message)
!

      ENTRY RESET_FUEL_DERIVATIVES_OL

         MAX_OVL_TRANS_GROUP_NUM = 0
         FUEL_DERIVATIVES_OL = 'BC'
      RETURN
!

      ENTRY OPEN_FUEL_DERIVATIVES_FILE

         OPEN(UNIT_NUM, &
               FILE=trim(OUTPUT_DIRECTORY())//FUEL_DERIVATIVES_OL// &
                "GPROD.BIN",ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      RETURN
!

      ENTRY CLOSE_FUEL_DERIVATIVES_FILE

         CLOSE(UNIT_NUM)
      RETURN

      ENTRY DOES_FUEL_DERIVATVE_FILE_EXIST( &
                           R_FUEL_DERIVATIVES_FILE_EXISTS)

         R_FUEL_DERIVATIVES_FILE_EXISTS = FUEL_DERIVATIVES_FILE_EXISTS
      RETURN

      ENTRY GET_BASE_FUEL_DERIVATIVES(R_BASE_TRANSACTIONS, &
                                       R_MAX_TRANS_GROUP_NUM)

         IF(FUEL_DERIVATIVES_OL == 'OL') THEN
            R_BASE_TRANSACTIONS = OVERLAY_TRANSACTIONS_IN_FILE
            R_MAX_TRANS_GROUP_NUM = MAX_OVL_TRANS_GROUP_NUM
         ELSE
            R_BASE_TRANSACTIONS = BASE_TRANSACTIONS_IN_FILE
            R_MAX_TRANS_GROUP_NUM = MAX_BASE_TRANS_GROUP_NUM
         ENDIF
      RETURN

      ENTRY RETURN_FD_GROUP_INFO(R_NUM_OF_TRANS_CLASSES, &
                                 R_MAX_TRANS_CLASS_NUM, &
                                 R_NUM_OF_ASSET_CLASSES, &
                                 R_MAX_ASSET_CLASS_NUM, &
                                 R_NUM_OF_CUST_CLASSES, &
                                 R_MAX_CUST_CLASS_NUM)

         IF(FUEL_DERIVATIVES_OL == 'OL') THEN
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
!                ROUTINE TO READ FUEL DERIVATIVES FILE
!
!                         COPYRIGHT (C) 2003
!                    M.S. GERBER & ASSOCIATES, INC.
!                        ALL RIGHTS RESERVED
!

!

!
      RECURSIVE FUNCTION READ_FUEL_DERIVATIVES_DATA()
!

!
! LOCAL DATA LIST
!
      USE IREC_ENDPOINT_CONTROL
      use grx_planning_routines
      USE SIZECOM
      use SpinDriftLib
      USE PROD_ARRAYS_DIMENSIONS
      use globecom
      use mthnmcom
      use mod_base_year
      use nunits_shared
      use end_routine
      implicit none
      SAVE

!
      LOGICAL (KIND=1) :: READ_FUEL_DERIVATIVES_DATA
      LOGICAL (KIND=1) :: SAVE_FUEL_DERIVATIVES_STATUS=.FALSE.
      LOGICAL (KIND=1) :: RUN_TRANSACT=.FALSE.,YES_RUN_TRANSACT
      LOGICAL (KIND=1) :: DAILY_FORWARD_FUEL_DERIVATIVES
      LOGICAL (KIND=1) :: END_MONTH_FUEL_DERIVATIVES
      LOGICAL (KIND=1) :: BEGIN_MONTH_FUEL_DERIVATIVES
      LOGICAL (KIND=1) :: ANNUAL_FUEL_DERIVATIVES
      LOGICAL (KIND=1) :: REDUCE_HOURLY_FUEL_DERIVATIVES
      LOGICAL (KIND=1) :: L_DUMMY,GET_ESC_NRG_GAS_PRICE
      LOGICAL (KIND=1) :: FUEL_DERIVATIVES_FILE_STATUS
      LOGICAL (KIND=1) :: UNIT_HAS_FUEL_DERIVATIVE
      LOGICAL (KIND=1) :: RETURN_FUEL_DERIV_VARIABLES
      LOGICAL (KIND=1) :: HOURLY_IN_THE_MONEY
      INTEGER (KIND=2) :: RET_MNTHLY_FUEL_DERIV_CASH_VARS
      INTEGER (KIND=2) :: RET_MLY_FUEL_DERIV_INC_VARS,MO
      INTEGER (KIND=2) :: FI,TRANSACT_ANLST_RSLTS_AVAIL_STG
!
      LOGICAL (KIND=4) :: FUEL_DERIVATIVES_FILE_EXISTS
!
      INTEGER (KIND=2) :: IREC,TRANS,DELETE,R_TRANS,MARGINAL_FUEL_DERIV
      INTEGER (KIND=2) :: I,NUM_TRANSACTIONS=0
      INTEGER (KIND=2) :: SAVE_ACTIVE_TRANSACTIONS=0
      INTEGER (KIND=2) :: MONTHLY_ACTIVE_TRANSACTIONS=0
      INTEGER (KIND=2) :: ANNUAL_ACTIVE_TRANSACTIONS=0
      INTEGER (KIND=2) :: NUM_OF_TRANS_CLASSES,MAX_TRANS_GROUP_NUM
      INTEGER (KIND=2) :: NUM_OF_ASSET_CLASSES,MAX_ASSET_CLASS_NUM
      INTEGER (KIND=2) :: NUM_OF_CUST_CLASSES,MAX_CUST_CLASS_NUM
      INTEGER (KIND=2) :: MAX_TRANS_GROUPS,MAX_ASSET_GROUPS
      INTEGER (KIND=2) :: MAX_CLASS_GROUPS,MAX_ASSET_CLASS_GROUPS
      INTEGER (KIND=2) :: R_CLASS,ASSET_CLASS,MONTH
!    +               GET_NUMBER_OF_ACTIVE_GROUPS,
      INTEGER (KIND=2) :: BEGIN_MONTH,BEGIN_YEAR,END_MONTH,END_YEAR,AC
      INTEGER (KIND=2) :: TG,GET_TRANS_GROUP_POSITION
      INTEGER (KIND=2) :: YEARS_BEG_DAY_IN_MO(:,:)
      INTEGER (KIND=2) :: YEARS_END_DAY_IN_MO(:,:),NUM_INTER_PUTS(:)
      INTEGER (KIND=2) :: NUM_INTER_CALLS(:),FORWARD_POSITION(:,:)
      INTEGER (KIND=2) :: CALL_POSITION(:,:),MONTH_CALL_POSITION(:,:)
      INTEGER (KIND=2) :: ANNUAL_CALL_POSITION(:,:),PUT_POSITION(:,:)
      INTEGER (KIND=2) :: MONTH_PUT_POSITION(:,:)
      INTEGER (KIND=2) :: ANNUAL_PUT_POSITION(:,:)
      INTEGER (KIND=2) :: STORAGE_POSITION(:,:),INTER_PUT_POSITION(:,:)
      INTEGER (KIND=2) :: INTER_CALL_POSITION(:,:),BEGIN_DAY(:)
      INTEGER (KIND=2) :: BEGIN_DAY_IN_MONTH(:),BEGIN_EP(:),END_DAY(:)
      INTEGER (KIND=2) :: END_DAY_IN_MONTH(:),END_EP(:),NUM_FORWARDS(:)
      INTEGER (KIND=2) :: NUM_CALLS(:),NUM_MONTH_CALLS(:)
      INTEGER (KIND=2) :: NUM_ANNUAL_CALLS(:),NUM_PUTS(:),NUM_STORAGE(:)
      INTEGER (KIND=2) :: NUM_MONTH_PUTS(:),NUM_ANNUAL_PUTS(:)
      INTEGER (KIND=2) :: MONTHLY_STRIKES(:,:),MONTHLY_PRODUCT_MONTHS(:)
      INTEGER (KIND=2) :: ANNUAL_UNIT_TRANS_INDEX(:,:),T_COUNT(:)
      INTEGER (KIND=2) :: ASSET_CLASS_GROUPS_INDEX(:),DERIVATIVE_TYPE(:)
      INTEGER (KIND=2) :: ACTIVE_IN_MONTH(:),ACTIVE_IN_YEAR(:)
      INTEGER (KIND=2) :: ACTIVE_IN_YEAR_INDEX(:)
      INTEGER (KIND=2) :: UNIT_2_FUEL_TRANS(0:MAX_CL_UNITS)
      INTEGER (KIND=2) :: R_UNIT,R_UNIT_ID,TEMP_I,L_NUNITS
      INTEGER (KIND=4) :: BEGIN_DATE,END_DATE,VALUES_2_ZERO
      REAL (KIND=8) :: R_MMBTUS,MONTHLY_UNIT_FUEL_DERIV_HEAT

! FUEL DERIVATIVE INPUT DATA LIST
      CHARACTER (LEN=50) :: COMMENT
      CHARACTER (LEN=20) :: TRANSACTION_NAME(:),COUNTERPARTY_NAME(:)
      CHARACTER (LEN=20) :: TRANSACTION_TYPE(:)
      CHARACTER (LEN=20) :: TRANSACTION_DESCRIPTION(:)
      CHARACTER (LEN=3) :: COUNTERPARTY_BOND_RATING(:)
      CHARACTER (LEN=1) :: PRODUCT_ACTIVE,REPORT_DERIVATIVE(:)
      CHARACTER (LEN=1) :: COST_ASSIGNMENT(:),EXPENSE_COLLECTION(:)
      CHARACTER (LEN=1) :: PRICE_TYPE(:),SELL_EXCESS_PHYSICAL_FUEL(:)
      CHARACTER (LEN=1) :: FUEL_TYPE(:),TRANSPORTATION_BASIS_TYPE(:)
      CHARACTER (LEN=1) :: THERMAL_UNIT_FUEL_LINK(:)

      INTEGER (KIND=2) :: ASSET_CLASS_ID(:),ASSET_ALLOCATION_VECTOR(:)
      INTEGER (KIND=2) :: THERMAL_UNIT_LINK(:)
      INTEGER (KIND=2) :: DOLLARS_PER_MMBTU_ESC_VEC(:)
      INTEGER (KIND=2) :: DOLLARS_PER_DEAL_ESC_VECTOR(:)
      INTEGER (KIND=2) :: BASE_TRANSACTIONS_IN_FILE=0
      INTEGER (KIND=2) :: OVERLAY_TRANSACTIONS_IN_FILE=0
      INTEGER (KIND=2) :: MAX_BASE_TRANS_GROUP_NUM=0
      INTEGER (KIND=2) :: MAX_OVL_TRANS_GROUP_NUM=0
      INTEGER (KIND=2) :: R_YEAR,R_MONTH,R_DAY,R_HOUR,TEST_DATE
      INTEGER (KIND=2) :: LOCAL_YR,FT
!
      INTEGER (KIND=4) :: CONTRACT_DATE(:),CONTRACT_BEGIN_DATE(:)
      INTEGER (KIND=2) :: CONTRACT_END_DATE(:)
      REAL (KIND=4) :: ESCALATED_MONTHLY_VALUE,GET_VAR,MARKET_PRICE(:)
      REAL (KIND=4) :: HOURLY_PRICE(:) ! DELIVERED
      REAL (KIND=4) :: SPOT_PRICE(:) ! BASIS
      REAL (KIND=4) :: HOURLY_SPOT_PRICE(:),HOURLY_COST(:)
      REAL (KIND=4) :: HOURLY_TRANS_HEAT(:,:)
      REAL (KIND=4) :: ANNUAL_FUEL_REPORT(:,:)
      REAL (KIND=4) :: SPOT_TRANSPORTATION_BASIS(:)
      REAL (KIND=4) :: SPOT_DELIVERY_ADDER(:),MONTHLY_QUANTITY(:)
      REAL (KIND=4) :: MONTHLY_MULTIPLIER(:),FIXED_PRICE(:)
      REAL (KIND=4) :: DELIVERY_ADDER(:),STORAGE_FEE(:)
      REAL (KIND=4) :: MONTHLY_STORAGE_FEE(:)
      REAL (KIND=4) :: PREMIMUM_DOLLARS_PER_MMBTU(:)
      REAL (KIND=4) :: PREMIMUM_DOLLARS_PER_DEAL(:)
      REAL (KIND=4) :: TRANSPORTATION_BASIS(:),PHYSICAL_SALE_FEE(:)
      REAL (KIND=4) :: MONTHLY_PHYSICAL_SALE_FEE(:),HOURLY_QUANTITY(:)
      REAL (KIND=4) :: LAST_MONTH_STORED_FUEL(:)
      REAL (KIND=4) :: MONTH_AVE_COST_OF_INV(:)
      REAL (KIND=4) :: MONTH_BEGINNING_FUEL(:)
      REAL (KIND=4) :: MONTH_FUEL_DERIV_BENEFIT(:),MONTH_FUEL_ADDED(:)
      REAL (KIND=4) :: MONTH_TOTAL_FUEL_BURNED(:)
      REAL (KIND=4) :: MONTH_SPOT_FUEL_BURNED(:)
      REAL (KIND=4) :: MONTH_OPTION_PREMIUM_COST(:)
      REAL (KIND=4) :: MONTH_COST_OF_FUEL_ADDED(:)
      REAL (KIND=4) :: MONTH_COST_OF_SPOT_FUEL(:)
      REAL (KIND=4) :: ANNUAL_INTERRUPTIBLE_CAPACITY(:)
      REAL (KIND=4) :: ANNUAL_STORAGE_CAPACITY(:)
      REAL (KIND=4) :: MONTHLY_BURN_COST(:,:),MONTHLY_BURN(:,:)
      REAL (KIND=4) :: MONTHLY_BURN_REVENUE(:,:)
      REAL (KIND=4) :: MONTHLY_AC_REVENUE(:,:,:)
      REAL (KIND=4) :: MONTHLY_AC_REVENUE_ENERGY(:,:,:)
      REAL (KIND=4) :: MONTHLY_AC_EXPENSE(:,:,:)
      REAL (KIND=4) :: MONTHLY_AC_EXPENSE_ENERGY(:,:,:)
      REAL (KIND=4) :: MONTHLY_TRANSACTION_COST(:,:)
      REAL (KIND=4) :: MONTHLY_TRANSACTION_REVENUE(:,:)
      REAL (KIND=4) :: MONTHLY_UNIT_BURN(:,:),FUEL_SCEN_MULT(5)
      REAL (KIND=4) :: GET_SCENARIO_COAL_PRICES,GET_SCENARIO_GAS_PRICES
      REAL (KIND=4) :: GET_SCENARIO_OIL_PRICES
      REAL (KIND=4) :: GET_SCENARIO_URANIUM_PRICES
      REAL (KIND=4) :: AVE_COST_OF_INVENTORY,TOTAL_FUEL_BEGINNING_MONTH
      REAL (KIND=4) :: TOTAL_FUEL_REMOVED,NOT_BURNED_FUEL_EXPENSE
      REAL (KIND=4) :: BURN_EXPENSE,NOT_BURNED_FUEL_REVENUE,BURN_REVENUE
      ALLOCATABLE :: TRANSACTION_NAME,COUNTERPARTY_NAME
      ALLOCATABLE :: TRANSACTION_DESCRIPTION,COUNTERPARTY_BOND_RATING
      ALLOCATABLE :: REPORT_DERIVATIVE,TRANSACTION_TYPE,NUM_INTER_PUTS
      ALLOCATABLE :: NUM_INTER_CALLS,FORWARD_POSITION,CALL_POSITION
      ALLOCATABLE :: MONTH_CALL_POSITION,ANNUAL_CALL_POSITION
      ALLOCATABLE :: PUT_POSITION,MONTH_PUT_POSITION
      ALLOCATABLE :: ANNUAL_PUT_POSITION,STORAGE_POSITION
      ALLOCATABLE :: INTER_PUT_POSITION,INTER_CALL_POSITION
      ALLOCATABLE :: DERIVATIVE_TYPE,COST_ASSIGNMENT,EXPENSE_COLLECTION
      ALLOCATABLE :: PRICE_TYPE,SELL_EXCESS_PHYSICAL_FUEL,FUEL_TYPE
      ALLOCATABLE :: TRANSPORTATION_BASIS_TYPE
      ALLOCATABLE :: THERMAL_UNIT_FUEL_LINK,ASSET_CLASS_ID
      ALLOCATABLE :: ASSET_ALLOCATION_VECTOR,THERMAL_UNIT_LINK
      ALLOCATABLE :: DOLLARS_PER_MMBTU_ESC_VEC
      ALLOCATABLE :: DOLLARS_PER_DEAL_ESC_VECTOR,CONTRACT_DATE
      ALLOCATABLE :: CONTRACT_BEGIN_DATE,CONTRACT_END_DATE,MARKET_PRICE
      ALLOCATABLE :: HOURLY_PRICE,SPOT_PRICE,HOURLY_SPOT_PRICE
      ALLOCATABLE :: HOURLY_COST,HOURLY_TRANS_HEAT,ANNUAL_FUEL_REPORT
      ALLOCATABLE :: SPOT_TRANSPORTATION_BASIS,SPOT_DELIVERY_ADDER
      ALLOCATABLE :: MONTHLY_QUANTITY,MONTHLY_MULTIPLIER,FIXED_PRICE
      ALLOCATABLE :: DELIVERY_ADDER,STORAGE_FEE,MONTHLY_STORAGE_FEE
      ALLOCATABLE :: PREMIMUM_DOLLARS_PER_MMBTU
      ALLOCATABLE :: PREMIMUM_DOLLARS_PER_DEAL,TRANSPORTATION_BASIS
      ALLOCATABLE :: PHYSICAL_SALE_FEE,MONTHLY_PHYSICAL_SALE_FEE
      ALLOCATABLE :: HOURLY_QUANTITY,LAST_MONTH_STORED_FUEL
      ALLOCATABLE :: MONTH_AVE_COST_OF_INV,MONTH_BEGINNING_FUEL
      ALLOCATABLE :: MONTH_FUEL_DERIV_BENEFIT,MONTH_FUEL_ADDED
      ALLOCATABLE :: MONTH_TOTAL_FUEL_BURNED,MONTH_SPOT_FUEL_BURNED
      ALLOCATABLE :: MONTH_OPTION_PREMIUM_COST,MONTH_COST_OF_FUEL_ADDED
      ALLOCATABLE :: MONTH_COST_OF_SPOT_FUEL
      ALLOCATABLE :: ANNUAL_INTERRUPTIBLE_CAPACITY
      ALLOCATABLE :: ANNUAL_STORAGE_CAPACITY,MONTHLY_BURN_COST
      ALLOCATABLE :: MONTHLY_BURN,MONTHLY_BURN_REVENUE
      ALLOCATABLE :: MONTHLY_AC_REVENUE,MONTHLY_AC_REVENUE_ENERGY
      ALLOCATABLE :: MONTHLY_AC_EXPENSE,MONTHLY_AC_EXPENSE_ENERGY
      ALLOCATABLE :: MONTHLY_TRANSACTION_COST
      ALLOCATABLE :: MONTHLY_TRANSACTION_REVENUE,MONTHLY_UNIT_BURN
      ALLOCATABLE :: BEGIN_DAY,BEGIN_DAY_IN_MONTH,BEGIN_EP,END_DAY
      ALLOCATABLE :: END_DAY_IN_MONTH,END_EP,NUM_FORWARDS,NUM_CALLS
      ALLOCATABLE :: NUM_MONTH_CALLS,NUM_ANNUAL_CALLS,NUM_PUTS
      ALLOCATABLE :: NUM_STORAGE,NUM_MONTH_PUTS,NUM_ANNUAL_PUTS
      ALLOCATABLE :: MONTHLY_STRIKES,ANNUAL_UNIT_TRANS_INDEX
      ALLOCATABLE :: T_COUNT,MONTHLY_PRODUCT_MONTHS
      ALLOCATABLE :: ASSET_CLASS_GROUPS_INDEX,ACTIVE_IN_MONTH
      ALLOCATABLE :: ACTIVE_IN_YEAR,ACTIVE_IN_YEAR_INDEX
      ALLOCATABLE :: YEARS_BEG_DAY_IN_MO,YEARS_END_DAY_IN_MO
      REAL (KIND=4) :: R_HEAT,LOCAL_HEAT,GENP_R_VALUE,GENP_ACCUM
      REAL (KIND=4) :: LOCAL_PRICE,DERIV_FUEL_USED,SPOT_FUEL_USED
      REAL (KIND=4) :: R_MARKET_PRICE,TEMP_R,TEMP_COST,R_VECTOR
      REAL (KIND=4) :: R_FUEL_DERIVATIVE_PRICE,PHY_DERIV_VAR_REVENUE
      REAL (KIND=4) :: PHY_DERIV_FIX_REVENUE,PHY_DERIV_VAR_EXPENSE
      REAL (KIND=4) :: PHY_DERIV_FIX_EXPENSE,FIN_DERIV_VAR_REVENUE
      REAL (KIND=4) :: FIN_DERIV_FIX_REVENUE,FIN_DERIV_VAR_EXPENSE
      REAL (KIND=4) :: FIN_DERIV_FIX_EXPENSE,PHY_DERIV_REVENUE_ENERGY
      REAL (KIND=4) :: PHY_DERIV_EXPENSE_ENERGY,FIN_DERIV_REVENUE_ENERGY
      REAL (KIND=4) :: FIN_DERIV_EXPENSE_ENERGY
!
      CHARACTER (LEN=1) :: LOCAL_ASSIGNMENT(2)=(/'O','P'/)
      REAL (KIND=4) :: INC_MONTH_VARS(0:12,1:*)
      REAL (KIND=4) :: CASH_MONTH_VARS(0:12,1:*)
!
! 04/16/03. DETAILED REPORT
!
      LOGICAL (KIND=1) :: MONTH_DERIV_REPORT_NOT_OPEN=.TRUE.
      LOGICAL (KIND=1) :: MONTHLY_FUEL_DERIV_REPORT
      LOGICAL (KIND=1) :: YES_MONTHLY_FUEL_DERIV_REPORT
      INTEGER (KIND=2) :: MONTHLY_FUEL_DERIV_RPT_HEADER
      INTEGER (KIND=2) :: MONTHLY_FUEL_DERIV_UNIT=0
      INTEGER (KIND=2) :: FUEL_REPORT_VARIABLE_NUMBER=18
      INTEGER :: MONTHLY_FUEL_DERIV_REC
      REAL (KIND=4) :: FUEL_SOLD,REVENUE_OF_FUEL_SOLD,COST_OF_FUEL_SOLD
      REAL (KIND=4) :: FUEL_STORED,COST_OF_FUEL_STORED
      REAL (KIND=4) :: REVENUE_OF_FUEL_STORED

      CHARACTER (LEN=9) :: CL_MONTH_NAME(14)=( &
                               /'January  ','February ', &
                                'March    ','April    ', &
                                'May      ','June     ', &
                                'July     ','August   ', &
                                'September','October  ', &
                                'November ','December ', &
                                'Annual   ','Fiscal Yr'/)
!
! END DATA DECLARATIONS
!
!
         READ_FUEL_DERIVATIVES_DATA = .FALSE.
!
         CALL DOES_FUEL_DERIVATVE_FILE_EXIST( &
                                           FUEL_DERIVATIVES_FILE_EXISTS)
         IF(FUEL_DERIVATIVES_FILE_EXISTS) THEN
            CALL OPEN_FUEL_DERIVATIVES_FILE
!
            CALL GET_BASE_FUEL_DERIVATIVES(NUM_TRANSACTIONS, &
                                                    MAX_TRANS_GROUP_NUM)
            CALL RETURN_FD_GROUP_INFO( &
                                       NUM_OF_TRANS_CLASSES, &
                                       MAX_TRANS_GROUP_NUM, &
                                       NUM_OF_ASSET_CLASSES, &
                                       MAX_ASSET_CLASS_NUM, &
                                       NUM_OF_CUST_CLASSES, &
                                       MAX_CUST_CLASS_NUM)
            MAX_ASSET_GROUPS = MAX_ASSET_CLASS_NUM
            IF(NUM_TRANSACTIONS == 0) THEN
               WRITE(4,*) "In the fuel derivative file, there were"
               WRITE(4,*) "no active records found.  Please"
               WRITE(4,*) "verify the fuel derivatives and check any"
               WRITE(4,*) "Fuel Derivative overlays."
!
               RETURN
!
            ENDIF
! 02/26/03. TRANS_CLASSES CURRENTLY HAS NO DEFINITION.
            NUM_OF_TRANS_CLASSES = MAX(int(1,2),NUM_OF_TRANS_CLASSES)
!
!  Fuel Derivative Allocation
            IF( ALLOCATED(TRANSACTION_NAME) ) THEN
                 DEALLOCATE( TRANSACTION_NAME,COUNTERPARTY_NAME, &
                 TRANSACTION_DESCRIPTION,COUNTERPARTY_BOND_RATING, &
                 REPORT_DERIVATIVE,TRANSACTION_TYPE,DERIVATIVE_TYPE, &
                 COST_ASSIGNMENT,EXPENSE_COLLECTION,PRICE_TYPE, &
                 SELL_EXCESS_PHYSICAL_FUEL,FUEL_TYPE, &
                 TRANSPORTATION_BASIS_TYPE,THERMAL_UNIT_FUEL_LINK, &
                 ASSET_CLASS_ID,ASSET_ALLOCATION_VECTOR, &
                 THERMAL_UNIT_LINK,DOLLARS_PER_MMBTU_ESC_VEC, &
                 DOLLARS_PER_DEAL_ESC_VECTOR, &
                 CONTRACT_DATE,CONTRACT_BEGIN_DATE,CONTRACT_END_DATE, &
                 MARKET_PRICE,HOURLY_PRICE,SPOT_PRICE, &
                 HOURLY_SPOT_PRICE,HOURLY_COST,HOURLY_TRANS_HEAT, &
                 ANNUAL_FUEL_REPORT,SPOT_TRANSPORTATION_BASIS, &
                 SPOT_DELIVERY_ADDER,MONTHLY_QUANTITY, &
                 MONTHLY_MULTIPLIER,FIXED_PRICE,DELIVERY_ADDER, &
                 STORAGE_FEE,MONTHLY_STORAGE_FEE, &
                 PREMIMUM_DOLLARS_PER_MMBTU,PREMIMUM_DOLLARS_PER_DEAL, &
                 TRANSPORTATION_BASIS,PHYSICAL_SALE_FEE, &
                 MONTHLY_PHYSICAL_SALE_FEE,HOURLY_QUANTITY )
                 DEALLOCATE ( LAST_MONTH_STORED_FUEL, &
                 MONTH_AVE_COST_OF_INV,MONTH_BEGINNING_FUEL, &
                 MONTH_FUEL_DERIV_BENEFIT,MONTH_FUEL_ADDED, &
                 MONTH_TOTAL_FUEL_BURNED,MONTH_SPOT_FUEL_BURNED, &
                 MONTH_OPTION_PREMIUM_COST,MONTH_COST_OF_FUEL_ADDED, &
                 MONTH_COST_OF_SPOT_FUEL,NUM_INTER_PUTS,NUM_INTER_CALLS, &
                 FORWARD_POSITION,CALL_POSITION,MONTH_CALL_POSITION, &
                 ANNUAL_CALL_POSITION,PUT_POSITION,MONTH_PUT_POSITION, &
                 ANNUAL_PUT_POSITION,STORAGE_POSITION, &
                 INTER_PUT_POSITION,INTER_CALL_POSITION, &
                 ANNUAL_INTERRUPTIBLE_CAPACITY,ANNUAL_STORAGE_CAPACITY, &
                 BEGIN_DAY,BEGIN_DAY_IN_MONTH,BEGIN_EP,END_DAY, &
                 END_DAY_IN_MONTH,END_EP,NUM_FORWARDS,NUM_CALLS, &
                 NUM_MONTH_CALLS,NUM_ANNUAL_CALLS,NUM_PUTS,NUM_STORAGE, &
                 NUM_MONTH_PUTS,NUM_ANNUAL_PUTS, &
                 ASSET_CLASS_GROUPS_INDEX,ACTIVE_IN_MONTH, &
                 ACTIVE_IN_YEAR,ACTIVE_IN_YEAR_INDEX, &
                 YEARS_BEG_DAY_IN_MO,YEARS_END_DAY_IN_MO)
            ENDIF
            ALLOCATE(TRANSACTION_NAME(NUM_TRANSACTIONS), &
                     COUNTERPARTY_NAME(NUM_TRANSACTIONS), &
                     TRANSACTION_DESCRIPTION(NUM_TRANSACTIONS), &
                     COUNTERPARTY_BOND_RATING(NUM_TRANSACTIONS), &
                     REPORT_DERIVATIVE(NUM_TRANSACTIONS), &
                     TRANSACTION_TYPE(NUM_TRANSACTIONS), &
                     DERIVATIVE_TYPE(NUM_TRANSACTIONS), &
                     COST_ASSIGNMENT(NUM_TRANSACTIONS), &
                     EXPENSE_COLLECTION(NUM_TRANSACTIONS), &
                     PRICE_TYPE(NUM_TRANSACTIONS), &
                     SELL_EXCESS_PHYSICAL_FUEL(NUM_TRANSACTIONS), &
                     FUEL_TYPE(NUM_TRANSACTIONS), &
                     TRANSPORTATION_BASIS_TYPE(NUM_TRANSACTIONS), &
                     THERMAL_UNIT_FUEL_LINK(NUM_TRANSACTIONS), &
                     ASSET_CLASS_ID(NUM_TRANSACTIONS), &
                     ASSET_ALLOCATION_VECTOR(NUM_TRANSACTIONS), &
                     THERMAL_UNIT_LINK(NUM_TRANSACTIONS), &
                     DOLLARS_PER_MMBTU_ESC_VEC(NUM_TRANSACTIONS), &
                     DOLLARS_PER_DEAL_ESC_VECTOR(NUM_TRANSACTIONS), &
                     CONTRACT_DATE(NUM_TRANSACTIONS))
            ALLOCATE(CONTRACT_BEGIN_DATE(NUM_TRANSACTIONS), &
                     CONTRACT_END_DATE(NUM_TRANSACTIONS), &
                     MARKET_PRICE(NUM_TRANSACTIONS), &
                     HOURLY_PRICE(NUM_TRANSACTIONS), &
                     SPOT_PRICE(NUM_TRANSACTIONS), &
                     HOURLY_SPOT_PRICE(NUM_TRANSACTIONS), &
                     HOURLY_COST(NUM_TRANSACTIONS), &
                     HOURLY_TRANS_HEAT(800,NUM_TRANSACTIONS), &
                     ANNUAL_FUEL_REPORT(NUM_TRANSACTIONS, &
                                           FUEL_REPORT_VARIABLE_NUMBER), &
                     SPOT_TRANSPORTATION_BASIS(NUM_TRANSACTIONS), &
                     SPOT_DELIVERY_ADDER(NUM_TRANSACTIONS), &
                     MONTHLY_QUANTITY(NUM_TRANSACTIONS), &
                     MONTHLY_MULTIPLIER(NUM_TRANSACTIONS), &
                     FIXED_PRICE(NUM_TRANSACTIONS), &
                     DELIVERY_ADDER(NUM_TRANSACTIONS), &
                     STORAGE_FEE(NUM_TRANSACTIONS), &
                     MONTHLY_STORAGE_FEE(NUM_TRANSACTIONS), &
                     PREMIMUM_DOLLARS_PER_MMBTU(NUM_TRANSACTIONS), &
                     PREMIMUM_DOLLARS_PER_DEAL(NUM_TRANSACTIONS))
            ALLOCATE(TRANSPORTATION_BASIS(NUM_TRANSACTIONS), &
                     PHYSICAL_SALE_FEE(NUM_TRANSACTIONS), &
                     MONTHLY_PHYSICAL_SALE_FEE(NUM_TRANSACTIONS), &
                     HOURLY_QUANTITY(NUM_TRANSACTIONS), &
                     LAST_MONTH_STORED_FUEL(NUM_TRANSACTIONS), &
                     MONTH_AVE_COST_OF_INV(NUM_TRANSACTIONS), &
                     MONTH_BEGINNING_FUEL(NUM_TRANSACTIONS), &
                     MONTH_FUEL_DERIV_BENEFIT(NUM_TRANSACTIONS), &
                     MONTH_FUEL_ADDED(NUM_TRANSACTIONS), &
                     MONTH_TOTAL_FUEL_BURNED(NUM_TRANSACTIONS), &
                     MONTH_SPOT_FUEL_BURNED(NUM_TRANSACTIONS), &
                     MONTH_OPTION_PREMIUM_COST(NUM_TRANSACTIONS), &
                     MONTH_COST_OF_FUEL_ADDED(NUM_TRANSACTIONS), &
                     MONTH_COST_OF_SPOT_FUEL(NUM_TRANSACTIONS), &
                     NUM_INTER_PUTS(NUM_OF_TRANS_CLASSES), &
                     NUM_INTER_CALLS(NUM_OF_TRANS_CLASSES), &
                     FORWARD_POSITION(NUM_TRANSACTIONS, &
                                                 NUM_OF_TRANS_CLASSES), &
                     CALL_POSITION(NUM_TRANSACTIONS, &
                                                 NUM_OF_TRANS_CLASSES))
            ALLOCATE(MONTH_CALL_POSITION(NUM_TRANSACTIONS, &
                                                 NUM_OF_TRANS_CLASSES), &
                     ANNUAL_CALL_POSITION(NUM_TRANSACTIONS, &
                                                 NUM_OF_TRANS_CLASSES), &
                     PUT_POSITION(NUM_TRANSACTIONS, &
                                                 NUM_OF_TRANS_CLASSES), &
                     MONTH_PUT_POSITION(NUM_TRANSACTIONS, &
                                                 NUM_OF_TRANS_CLASSES), &
                     ANNUAL_PUT_POSITION(NUM_TRANSACTIONS, &
                                                 NUM_OF_TRANS_CLASSES), &
                     STORAGE_POSITION(NUM_TRANSACTIONS, &
                                                 NUM_OF_TRANS_CLASSES), &
                     INTER_PUT_POSITION(NUM_TRANSACTIONS, &
                                                 NUM_OF_TRANS_CLASSES), &
                     INTER_CALL_POSITION(NUM_TRANSACTIONS, &
                                                 NUM_OF_TRANS_CLASSES), &
                     ANNUAL_INTERRUPTIBLE_CAPACITY( &
                                                  NUM_OF_TRANS_CLASSES), &
                     ANNUAL_STORAGE_CAPACITY(NUM_OF_TRANS_CLASSES), &
                     BEGIN_DAY(NUM_TRANSACTIONS))
            ALLOCATE(BEGIN_DAY_IN_MONTH(NUM_TRANSACTIONS), &
                     BEGIN_EP(NUM_TRANSACTIONS), &
                     END_DAY(NUM_TRANSACTIONS), &
                     END_DAY_IN_MONTH(NUM_TRANSACTIONS), &
                     END_EP(NUM_TRANSACTIONS), &
                     NUM_FORWARDS(NUM_TRANSACTIONS), &
                     NUM_CALLS(NUM_TRANSACTIONS), &
                     NUM_MONTH_CALLS(NUM_TRANSACTIONS), &
                     NUM_ANNUAL_CALLS(NUM_TRANSACTIONS), &
                     NUM_PUTS(NUM_TRANSACTIONS), &
                     NUM_STORAGE(NUM_TRANSACTIONS), &
                     NUM_MONTH_PUTS(NUM_TRANSACTIONS), &
                     NUM_ANNUAL_PUTS(NUM_TRANSACTIONS), &
                     ASSET_CLASS_GROUPS_INDEX(0:1024), & !  MAX ASSET CLASS SIZE
                     YEARS_BEG_DAY_IN_MO(NUM_TRANSACTIONS,0:12), &
                     YEARS_END_DAY_IN_MO(NUM_TRANSACTIONS,0:12), &
                     ACTIVE_IN_MONTH(NUM_TRANSACTIONS), &
                     ACTIVE_IN_YEAR(NUM_TRANSACTIONS), &
                     ACTIVE_IN_YEAR_INDEX(NUM_TRANSACTIONS))
!
            UNIT_2_FUEL_TRANS = 0
            ASSET_CLASS_GROUPS_INDEX = 0
!
! Fuel Derivative Final Read
!
            TRANS = 1
            MAX_ASSET_CLASS_GROUPS = 0
            DO IREC = 1, NUM_TRANSACTIONS
               READ(10,REC=IREC) DELETE, &
               TRANSACTION_NAME(TRANS), &
               PRODUCT_ACTIVE, &
               REPORT_DERIVATIVE(TRANS), &
               COUNTERPARTY_NAME(TRANS), &
               COUNTERPARTY_BOND_RATING(TRANS), &
               TRANSACTION_TYPE(TRANS), &
               TRANSACTION_DESCRIPTION(TRANS), &
               CONTRACT_DATE(TRANS), &
               BEGIN_DATE, &
               END_DATE,               & !  10
               MARKET_PRICE(TRANS), &
               SPOT_TRANSPORTATION_BASIS(TRANS),   & !  12
               SPOT_DELIVERY_ADDER(TRANS), &
               COST_ASSIGNMENT(TRANS),      & !  14
               EXPENSE_COLLECTION(TRANS), &
               ASSET_CLASS_ID(TRANS),        & !  16
               ASSET_ALLOCATION_VECTOR(TRANS),        & !  17
               THERMAL_UNIT_LINK(TRANS), &
               MONTHLY_QUANTITY(TRANS),       & !  19
               MONTHLY_MULTIPLIER(TRANS), &
               PRICE_TYPE(TRANS),            & !  21
               FIXED_PRICE(TRANS), &
               DELIVERY_ADDER(TRANS),               & !  23
               SELL_EXCESS_PHYSICAL_FUEL(TRANS), &
               STORAGE_FEE(TRANS),    & !  25
               PREMIMUM_DOLLARS_PER_MMBTU(TRANS),                      & !  26
               DOLLARS_PER_MMBTU_ESC_VEC(TRANS),                       & !  27
               PREMIMUM_DOLLARS_PER_DEAL(TRANS),                       & !  28
               DOLLARS_PER_DEAL_ESC_VECTOR(TRANS), &
               COMMENT,             & !  30
               FUEL_TYPE(TRANS), &
               TRANSPORTATION_BASIS(TRANS),           & !  32
               TRANSPORTATION_BASIS_TYPE(TRANS),                       & !  33
               PHYSICAL_SALE_FEE(TRANS), &
               THERMAL_UNIT_FUEL_LINK(TRANS)
!
               IF(PRODUCT_ACTIVE == 'F') CYCLE
!
! DOUBLE INDEX
!
               TG = 1 ! TRANSACTION_GROUP(TRANS)

               BEGIN_MONTH = INT(BEGIN_DATE/10000)
               BEGIN_DAY(TRANS) = INT(BEGIN_DATE/100) - &
                                                  BEGIN_MONTH*100
               BEGIN_YEAR = INT(BEGIN_DATE - &
                                       BEGIN_MONTH*10000 - &
                                                   BEGIN_DAY(TRANS)*100)
               IF(BEGIN_YEAR > 60) THEN
                  BEGIN_EP(TRANS) = BEGIN_YEAR*100 + BEGIN_MONTH
               ELSE
                  BEGIN_EP(TRANS) = BEGIN_YEAR*100 + 10000 + BEGIN_MONTH
               ENDIF
!
               END_MONTH = INT(END_DATE/10000)
               END_DAY(TRANS) = INT(END_DATE/100) - &
                                                  END_MONTH*100
               END_YEAR = INT(END_DATE - &
                                       END_MONTH*10000 - &
                                                   END_DAY(TRANS)*100)
               IF(END_YEAR > 60) THEN
                  END_EP(TRANS) = END_YEAR*100 + END_MONTH
               ELSE
                  END_EP(TRANS) = END_YEAR*100 + 10000 + END_MONTH
               ENDIF
!
               AC = ASSET_CLASS_ID(TRANS)
!
               IF(ASSET_CLASS_GROUPS_INDEX(AC) == 0) THEN
                  MAX_ASSET_CLASS_GROUPS = MAX_ASSET_CLASS_GROUPS + 1
                  ASSET_CLASS_GROUPS_INDEX(AC) = MAX_ASSET_CLASS_GROUPS
               ENDIF
! 02/27/03.
! THERMAL RESOURCE LINK
! CAN ONLY HAVE ONE FUEL DERIVATIVE FOR EACH RESOURCE, OTHERWISE LOSE
! THE THERMAL_UNIT_LINK(TRANS) INFORMATION.
! HOWEVER, CAN HAVE MANY UNITS TIED TO A FUEL DERIVATIVE.
!
               IF(THERMAL_UNIT_LINK(TRANS) > 0 .AND. &
                         THERMAL_UNIT_LINK(TRANS) <= MAX_CL_UNITS) THEN
                  UNIT_2_FUEL_TRANS(THERMAL_UNIT_LINK(TRANS)) = TRANS
               ELSE ! HANDLE A VECTOR OF UNITS
                  R_VECTOR = ABS(THERMAL_UNIT_LINK(TRANS))
                  DO I = 1, 30
                     TEMP_R = &
                             GET_VAR(R_VECTOR,I,TRANSACTION_NAME(TRANS))
                     IF(TEMP_R <= 0.) EXIT
                     TEMP_I = INT(TEMP_R,2)
                     UNIT_2_FUEL_TRANS(TEMP_I) = TRANS
                  ENDDO
               ENDIF
!
               IF(TRANSACTION_TYPE(TRANS) == &
                                            'Physical Forward    ') THEN
                  DERIVATIVE_TYPE(TRANS) = 1
               ELSEIF(TRANSACTION_TYPE(TRANS) == 'Physical Call') THEN
                  DERIVATIVE_TYPE(TRANS) = 2
               ELSEIF(TRANSACTION_TYPE(TRANS) == 'Physical Put') THEN
                  DERIVATIVE_TYPE(TRANS) = 3
               ELSEIF(TRANSACTION_TYPE(TRANS) == &
                                           'Financial Forward   ') THEN
                  DERIVATIVE_TYPE(TRANS) = 4
               ELSEIF(TRANSACTION_TYPE(TRANS) == 'Financial Call') THEN
                  DERIVATIVE_TYPE(TRANS) = 5
               ELSEIF(TRANSACTION_TYPE(TRANS) == 'Financial Put') THEN
                  DERIVATIVE_TYPE(TRANS) = 6
               ELSEIF(TRANSACTION_TYPE(TRANS) == &
                                            'Fixed for Floating S') THEN
                  DERIVATIVE_TYPE(TRANS) = 7
               ELSEIF(TRANSACTION_TYPE(TRANS) == &
                                            'Floating for Fixed S') THEN
                  DERIVATIVE_TYPE(TRANS) = 8
               ENDIF
!
!
               TRANS = TRANS + 1
            ENDDO
!
! MONTH-TO-MONTH INVENTORY VARIABLE
!
            LAST_MONTH_STORED_FUEL = 0.
            MONTH_AVE_COST_OF_INV = 0.
!
            READ_FUEL_DERIVATIVES_DATA = .TRUE.
!
            CALL CLOSE_FUEL_DERIVATIVES_FILE
!
         ENDIF
!
         SAVE_ACTIVE_TRANSACTIONS = TRANS - 1
         SAVE_FUEL_DERIVATIVES_STATUS = READ_FUEL_DERIVATIVES_DATA                !
!
      RETURN

      ENTRY ANNUAL_FUEL_DERIVATIVES()

         MONTHLY_ACTIVE_TRANSACTIONS = 0
         ANNUAL_ACTIVE_TRANSACTIONS = 0
         RUN_TRANSACT = YES_RUN_TRANSACT()
         IF(.NOT. SAVE_FUEL_DERIVATIVES_STATUS .OR. &
                                              .NOT. RUN_TRANSACT) RETURN
         ACTIVE_IN_YEAR_INDEX = 0
         ACTIVE_IN_YEAR = 0
         L_NUNITS = MAX(int(999,2),GET_NUNITS()) ! 061808. CHANGED FOR KCPL
         IF( ALLOCATED(MONTHLY_BURN_COST) ) &
            DEALLOCATE( &
                     MONTHLY_BURN_COST, &
                     MONTHLY_UNIT_BURN, &
                     MONTHLY_BURN, &
                     MONTHLY_BURN_REVENUE, &
                     MONTHLY_AC_REVENUE, &
                     MONTHLY_AC_REVENUE_ENERGY, &
                     MONTHLY_AC_EXPENSE, &
                     MONTHLY_AC_EXPENSE_ENERGY, &
                     MONTHLY_PRODUCT_MONTHS, &
                     MONTHLY_STRIKES, &
                     ANNUAL_UNIT_TRANS_INDEX, &
                     T_COUNT, &
                     MONTHLY_TRANSACTION_COST, &
                     MONTHLY_TRANSACTION_REVENUE )
         ALLOCATE( &
                    MONTHLY_BURN_COST(NUM_TRANSACTIONS,0:12), &
                    MONTHLY_UNIT_BURN(NUM_TRANSACTIONS,L_NUNITS), &
                    MONTHLY_BURN(NUM_TRANSACTIONS,0:12), &
                    MONTHLY_BURN_REVENUE(NUM_TRANSACTIONS,0:12), &
                    MONTHLY_AC_REVENUE(0:MAX_ASSET_CLASS_GROUPS,4,0:12), &
                    MONTHLY_AC_REVENUE_ENERGY( &
                                       0:MAX_ASSET_CLASS_GROUPS,4,0:12), &
                    MONTHLY_AC_EXPENSE(0:MAX_ASSET_CLASS_GROUPS,4,0:12), &
                    MONTHLY_AC_EXPENSE_ENERGY( &
                                       0:MAX_ASSET_CLASS_GROUPS,4,0:12), &
                    MONTHLY_PRODUCT_MONTHS(NUM_TRANSACTIONS), &
                    MONTHLY_STRIKES(NUM_TRANSACTIONS,0:12), &
                    ANNUAL_UNIT_TRANS_INDEX(NUM_TRANSACTIONS,L_NUNITS), &
                    T_COUNT(L_NUNITS), &
                    MONTHLY_TRANSACTION_COST(NUM_TRANSACTIONS,0:12), &
                    MONTHLY_TRANSACTION_REVENUE(NUM_TRANSACTIONS,0:12))
!
         MONTHLY_BURN_COST = 0.
         MONTHLY_BURN = 0.
         MONTHLY_PRODUCT_MONTHS = 0
         MONTHLY_STRIKES = 0
         MONTHLY_BURN_REVENUE = 0.
         MONTHLY_AC_REVENUE = 0.
         MONTHLY_AC_REVENUE_ENERGY = 0.
         MONTHLY_AC_EXPENSE = 0.
         MONTHLY_AC_EXPENSE_ENERGY = 0.
         MONTHLY_TRANSACTION_COST = 0.
         MONTHLY_TRANSACTION_REVENUE = 0.

         ANNUAL_INTERRUPTIBLE_CAPACITY = 0.
         ANNUAL_STORAGE_CAPACITY = 0.

         NUM_ANNUAL_CALLS = 0
         NUM_ANNUAL_PUTS = 0

         ANNUAL_FUEL_REPORT = 0.

         MONTHLY_FUEL_DERIV_REPORT = YES_MONTHLY_FUEL_DERIV_REPORT()

         ANNUAL_FUEL_DERIVATIVES = .TRUE.
      RETURN

      ENTRY BEGIN_MONTH_FUEL_DERIVATIVES(R_MONTH,R_YEAR)


! COUNT THE DERIVATIVES BY TYPE

         BEGIN_MONTH_FUEL_DERIVATIVES = .FALSE.

         MONTHLY_ACTIVE_TRANSACTIONS = 0
!
         IF( .NOT. SAVE_FUEL_DERIVATIVES_STATUS) RETURN

         NUM_FORWARDS = 0
         NUM_CALLS = 0
         NUM_MONTH_CALLS = 0
         NUM_PUTS = 0
         NUM_STORAGE = 0
         NUM_MONTH_PUTS = 0
         NUM_INTER_PUTS = 0
         NUM_INTER_CALLS = 0

         FORWARD_POSITION = 0
         PUT_POSITION = 0
         MONTH_PUT_POSITION = 0
         STORAGE_POSITION = 0
         INTER_PUT_POSITION = 0
         INTER_CALL_POSITION = 0
         CALL_POSITION = 0
         MONTH_CALL_POSITION = 0
         HOURLY_PRICE = 0.
         SPOT_PRICE = 0.
         HOURLY_SPOT_PRICE = 0.
         HOURLY_COST = 0.
         HOURLY_TRANS_HEAT = 0.

         MONTHLY_UNIT_BURN = 0.
         MONTH_BEGINNING_FUEL = 0.
         MONTH_FUEL_DERIV_BENEFIT = 0.
         MONTH_FUEL_ADDED = 0.
         MONTH_TOTAL_FUEL_BURNED = 0.
         MONTH_SPOT_FUEL_BURNED = 0.
         MONTH_OPTION_PREMIUM_COST = 0.
         MONTH_COST_OF_FUEL_ADDED = 0.
         MONTH_COST_OF_SPOT_FUEL = 0.

         IF(.NOT. RUN_TRANSACT) RETURN

         TEST_DATE = 100*(R_YEAR - 1900) + R_MONTH
         LOCAL_YR = R_YEAR - get_base_year()

         FUEL_SCEN_MULT(1) =  GET_SCENARIO_COAL_PRICES(LOCAL_YR,R_MONTH)
         FUEL_SCEN_MULT(2) =  GET_SCENARIO_GAS_PRICES(LOCAL_YR,R_MONTH)
         FUEL_SCEN_MULT(3) =  GET_SCENARIO_OIL_PRICES(LOCAL_YR,R_MONTH)
         FUEL_SCEN_MULT(4) = &
                           GET_SCENARIO_URANIUM_PRICES(LOCAL_YR,R_MONTH)
         FUEL_SCEN_MULT(5) = 1.0
!
         ANNUAL_UNIT_TRANS_INDEX = 0
         T_COUNT = 0
!
! 040706. 042406. MOVED INSIDE MONTH.
!
         DO TRANS = 1, SAVE_ACTIVE_TRANSACTIONS
            IF(THERMAL_UNIT_FUEL_LINK(TRANS) /= 'U') CYCLE
            IF(TEST_DATE > END_EP(TRANS) .OR. &
                                      TEST_DATE < BEGIN_EP(TRANS)) CYCLE
            TEMP_I  = THERMAL_UNIT_LINK(TRANS)
            IF(TEMP_I == 0) THEN
               CYCLE
            ELSEIF( TEMP_I > 0 .AND. &
                         TEMP_I <= L_NUNITS) THEN
               T_COUNT(TEMP_I) = T_COUNT(TEMP_I) + 1
               ANNUAL_UNIT_TRANS_INDEX(TEMP_I,T_COUNT(TEMP_I)) = &
                                                                   TRANS

            ELSE ! HANDLE A VECTOR OF UNITS
               R_VECTOR = ABS(THERMAL_UNIT_LINK(TRANS))
               DO I = 1, 30
                  TEMP_R = &
                             GET_VAR(R_VECTOR,I,TRANSACTION_NAME(TRANS))
                  TEMP_I = INT(TEMP_R,2)
                  IF(TEMP_I <= 0.) EXIT
                  T_COUNT(TEMP_I) = T_COUNT(TEMP_I) + 1
                  ANNUAL_UNIT_TRANS_INDEX(TEMP_I,T_COUNT(TEMP_I)) = &
                                                                   TRANS

               ENDDO
            ENDIF
         END DO
!
         DO TRANS = 1, SAVE_ACTIVE_TRANSACTIONS
            IF(TEST_DATE > END_EP(TRANS) .OR. &
                                      TEST_DATE < BEGIN_EP(TRANS)) CYCLE
!
!
            MONTHLY_PRODUCT_MONTHS(TRANS) = &
                                      MONTHLY_PRODUCT_MONTHS(TRANS) + 1.
!
            IF(STORAGE_FEE(TRANS) > 0.) THEN
               MONTHLY_STORAGE_FEE(TRANS) = STORAGE_FEE(TRANS)
            ELSE
               MONTHLY_STORAGE_FEE(TRANS) = &
                 ESCALATED_MONTHLY_VALUE(ABS(STORAGE_FEE(TRANS)), &
                          INT(ABS(STORAGE_FEE(TRANS)),2),LOCAL_YR, &
                                                       R_MONTH,INT(1,2))
            ENDIF
!
            IF(PHYSICAL_SALE_FEE(TRANS) > 0.) THEN
               MONTHLY_PHYSICAL_SALE_FEE(TRANS) = &
                                                PHYSICAL_SALE_FEE(TRANS)
            ELSE
               MONTHLY_PHYSICAL_SALE_FEE(TRANS) = &
                 ESCALATED_MONTHLY_VALUE(ABS(PHYSICAL_SALE_FEE(TRANS)), &
                          INT(ABS(PHYSICAL_SALE_FEE(TRANS)),2),LOCAL_YR, &
                                                       R_MONTH,INT(1,2))
            ENDIF
!
            IF(MONTHLY_MULTIPLIER(TRANS) < 0.) THEN
               HOURLY_QUANTITY(TRANS) = &
                 ESCALATED_MONTHLY_VALUE(ABS(MONTHLY_MULTIPLIER(TRANS)), &
                         INT(ABS(MONTHLY_MULTIPLIER(TRANS)),2),LOCAL_YR, &
                                                       R_MONTH,INT(1,2))
            ELSE
               HOURLY_QUANTITY(TRANS) = MONTHLY_MULTIPLIER(TRANS)
            ENDIF
! 010201.
            IF(MONTHLY_QUANTITY(TRANS) < 0.) THEN
               HOURLY_QUANTITY(TRANS) = HOURLY_QUANTITY(TRANS) * &
                        GET_VAR(MONTHLY_QUANTITY(TRANS), &
                                                R_YEAR-get_base_year(), &
                                                TRANSACTION_NAME(TRANS))
            ELSE
               HOURLY_QUANTITY(TRANS) = HOURLY_QUANTITY(TRANS) * &
                                              MONTHLY_QUANTITY(TRANS)
            ENDIF
!
            TEMP_R = 0.
            CALL GET_DELIVERED_FUEL_COST( &
                                 MARKET_PRICE(TRANS),                   & !  11
                                 SPOT_TRANSPORTATION_BASIS(TRANS),      & !  12
                                 SPOT_DELIVERY_ADDER(TRANS),            & !  13
                                 TEMP_R, &
                                 HOURLY_PRICE(TRANS), &
                                 R_MONTH,LOCAL_YR)
! 05/22/03. FOR BURESH
            CALL GET_A_FUEL_PRICE( &
                                 MARKET_PRICE(TRANS),                   & !  11
                                 SPOT_PRICE(TRANS), &
                                 R_MONTH,LOCAL_YR)
!
            IF(PRICE_TYPE(TRANS) == 'F') THEN
               TEMP_COST = FIXED_PRICE(TRANS)
            ELSE
               TEMP_COST = MARKET_PRICE(TRANS)
            ENDIF
!
            CALL GET_DELIVERED_FUEL_COST( &
                                 TEMP_COST,                        & !  11
                                 TRANSPORTATION_BASIS(TRANS),      & !  12
                                 DELIVERY_ADDER(TRANS),            & !  13
                                 TEMP_R, &
                                 HOURLY_COST(TRANS), &
                                 R_MONTH,LOCAL_YR)
!
            IF(    FUEL_TYPE(TRANS) == 'C') THEN
               FT = 1
            ELSEIF(FUEL_TYPE(TRANS) == 'G') THEN
               FT = 2
            ELSEIF(FUEL_TYPE(TRANS) == 'O') THEN
               FT = 3
            ELSEIF(FUEL_TYPE(TRANS) == 'N' .OR. &
                                          FUEL_TYPE(TRANS) == 'U') THEN
               FT = 4
            ELSE
               FT = 5
            ENDIF
!
            HOURLY_PRICE(TRANS) = &
                                HOURLY_PRICE(TRANS) * FUEL_SCEN_MULT(FT)
            HOURLY_SPOT_PRICE(TRANS) = &
                                  SPOT_PRICE(TRANS) * FUEL_SCEN_MULT(FT)
            IF(PRICE_TYPE(TRANS) /= 'F') THEN
               HOURLY_COST(TRANS) = &
                                 HOURLY_COST(TRANS) * FUEL_SCEN_MULT(FT)
            ENDIF
!
! 06/11/03. PAY FOR THE PREMIUM REGARDLESS OF STRIKE.
!
            MONTH_OPTION_PREMIUM_COST(TRANS) = &
                       PREMIMUM_DOLLARS_PER_MMBTU(TRANS) * &
                             HOURLY_QUANTITY(TRANS) + &
                                        PREMIMUM_DOLLARS_PER_DEAL(TRANS)
!
! 05/27/03. MOVED FROM HOURLY
!
            HOURLY_IN_THE_MONEY = .TRUE.
!
            IF(DERIVATIVE_TYPE(TRANS) == 2 .OR. &
                                       DERIVATIVE_TYPE(TRANS) == 5) THEN
               IF(HOURLY_PRICE(TRANS) <= HOURLY_COST(TRANS)) THEN
                  HOURLY_IN_THE_MONEY = .FALSE.
                  HOURLY_QUANTITY(TRANS) = 0.
               ENDIF
            ELSEIF(DERIVATIVE_TYPE(TRANS) == 3 .OR. &
                                       DERIVATIVE_TYPE(TRANS) == 6) THEN
               IF(HOURLY_PRICE(TRANS) > HOURLY_COST(TRANS)) THEN
                  HOURLY_IN_THE_MONEY = .FALSE.
                  HOURLY_QUANTITY(TRANS) = 0.
               ENDIF
            ENDIF
!
            MONTH_FUEL_ADDED(TRANS) = MONTH_FUEL_ADDED(TRANS) + &
                                                  HOURLY_QUANTITY(TRANS)
!
            HOURLY_QUANTITY(TRANS) = HOURLY_QUANTITY(TRANS) + &
                                           LAST_MONTH_STORED_FUEL(TRANS)
            MONTH_BEGINNING_FUEL(TRANS) = &
                           LAST_MONTH_STORED_FUEL(TRANS)
            LAST_MONTH_STORED_FUEL(TRANS) = 0. 

            MONTH_COST_OF_FUEL_ADDED(TRANS) = &
                              HOURLY_COST(TRANS)*MONTH_FUEL_ADDED(TRANS)

            MONTH_AVE_COST_OF_INV(TRANS) = &
                             MONTH_AVE_COST_OF_INV(TRANS) + &
                                         MONTH_COST_OF_FUEL_ADDED(TRANS)
!
!
! ASSUMES THAT NO FUEL MEANS NOT ACTIVE.
!
            IF(HOURLY_QUANTITY(TRANS) <= 0.) CYCLE

! ACTIVE IN THE MONTH. ALL NON-ENERGY COSTS ARE CALLED TRANSACTION COSTS.

            MONTHLY_ACTIVE_TRANSACTIONS = &
                                         MONTHLY_ACTIVE_TRANSACTIONS + 1
            ACTIVE_IN_MONTH(MONTHLY_ACTIVE_TRANSACTIONS) = TRANS
            IF(ACTIVE_IN_YEAR_INDEX(TRANS) == 0) THEN
               ACTIVE_IN_YEAR_INDEX(TRANS) = 1
               ANNUAL_ACTIVE_TRANSACTIONS = &
                                          ANNUAL_ACTIVE_TRANSACTIONS + 1
               ACTIVE_IN_YEAR(ANNUAL_ACTIVE_TRANSACTIONS) = TRANS
            ENDIF
!
            IF(TEST_DATE > BEGIN_EP(TRANS)) THEN
               BEGIN_DAY_IN_MONTH(TRANS) = 1
            ELSE
               BEGIN_DAY_IN_MONTH(TRANS) = BEGIN_DAY(TRANS)
            ENDIF
            IF(TEST_DATE < END_EP(TRANS)) THEN
               END_DAY_IN_MONTH(TRANS) = 31
            ELSE
               END_DAY_IN_MONTH(TRANS) = END_DAY(TRANS)
            ENDIF
            YEARS_BEG_DAY_IN_MO(TRANS,R_MONTH)= &
               BEGIN_DAY_IN_MONTH(TRANS)
            YEARS_END_DAY_IN_MO(TRANS,R_MONTH)= &
               END_DAY_IN_MONTH(TRANS)
! NOTE DOUBLE INDEX
            TG = 1 ! TRANSACTION_GROUP(TRANS)

            IF(DERIVATIVE_TYPE(TRANS) == 1 .OR. &
                                       DERIVATIVE_TYPE(TRANS) == 4) THEN
               NUM_FORWARDS(TG) = NUM_FORWARDS(TG) + 1
               FORWARD_POSITION(NUM_FORWARDS(TG),TG) = TRANS
            ELSEIF(DERIVATIVE_TYPE(TRANS) == 2 .OR. &
                                       DERIVATIVE_TYPE(TRANS) == 5) THEN
!

                  NUM_MONTH_CALLS(TG) = NUM_MONTH_CALLS(TG) + 1
                  MONTH_CALL_POSITION(NUM_MONTH_CALLS(TG),TG) = TRANS
!
            ELSEIF(DERIVATIVE_TYPE(TRANS) == 3 .OR. &
                                       DERIVATIVE_TYPE(TRANS) == 6) THEN
!

                  NUM_MONTH_PUTS(TG) = NUM_MONTH_PUTS(TG) + 1
                  MONTH_PUT_POSITION(NUM_MONTH_PUTS(TG),TG) = TRANS
!
            ELSEIF( DERIVATIVE_TYPE(TRANS) == 7) THEN
               NUM_INTER_CALLS(TG) = NUM_INTER_CALLS(TG) + 1
               INTER_CALL_POSITION(NUM_INTER_CALLS(TG),TG) = TRANS

            ELSEIF( DERIVATIVE_TYPE(TRANS) == 8) THEN
               NUM_INTER_PUTS(TG) = NUM_INTER_PUTS(TG) + 1
               INTER_PUT_POSITION(NUM_INTER_PUTS(TG),TG) = TRANS

            ENDIF
!
         ENDDO
!
         BEGIN_MONTH_FUEL_DERIVATIVES = .TRUE.

      RETURN
!

!
      ENTRY GET_ESC_NRG_GAS_PRICE(R_YEAR,R_MONTH,R_TRANS,GENP_R_VALUE)
    ! assumes it's okay to overwrite array MONTHLY_2ND_ENERGY_PRICE
!

      GET_ESC_NRG_GAS_PRICE = .FALSE.
!
! 02/28/03. THIS PRICING SECTION NEEDS TO BE WRITTEN TO
!           INCORPORATE ALL CHANGES TO

      TEMP_R = 0.
      CALL GET_DELIVERED_FUEL_COST( &
                                 MARKET_PRICE(R_TRANS),                   & !  11
                                 SPOT_TRANSPORTATION_BASIS(R_TRANS),      & !  12
                                 SPOT_DELIVERY_ADDER(R_TRANS),            & !  13
                                 TEMP_R, &
                                 GENP_ACCUM, &
                                 R_MONTH,R_YEAR)

      GENP_R_VALUE=GENP_ACCUM ! + MONTHLY_2ND_ENERGY_PRICE(R_TRANS)
      GET_ESC_NRG_GAS_PRICE=.TRUE.
      RETURN ! ENTRY GET_ESC_NRG_GAS_PRICE

      ENTRY END_MONTH_FUEL_DERIVATIVES(R_MONTH,R_YEAR)

!
! CALCULATE THE MONTHLY COSTS OF STORAGE AND REVENUES OF SELLING
!
         END_MONTH_FUEL_DERIVATIVES = .FALSE.
         IF( .NOT. SAVE_FUEL_DERIVATIVES_STATUS .OR. &
                                              .NOT. RUN_TRANSACT) RETURN
!
         TEST_DATE = 100*(R_YEAR - 1900) + R_MONTH
!         LOCAL_YR = R_YEAR - get_base_year()
         MONTH = R_MONTH
         DO TRANS = 1, SAVE_ACTIVE_TRANSACTIONS
            IF(TEST_DATE > END_EP(TRANS) .OR. &
                                      TEST_DATE < BEGIN_EP(TRANS)) CYCLE
!
            ASSET_CLASS = ASSET_CLASS_ID(TRANS)
            ASSET_CLASS = ASSET_CLASS_GROUPS_INDEX(ASSET_CLASS)
!

            FUEL_SOLD = 0.
            REVENUE_OF_FUEL_SOLD = 0.
            COST_OF_FUEL_SOLD = 0.
            FUEL_STORED = 0.
            COST_OF_FUEL_STORED = 0.
            REVENUE_OF_FUEL_STORED = 0.
!
! TO TRAP FOR DOUG'S PROBLEM
!
            IF(ASSET_CLASS < 0 .OR. &
                              ASSET_CLASS > MAX_ASSET_CLASS_GROUPS) THEN
               WRITE(4,*) "IN FUEL DERIVATIVES, AND FUEL DERIVATIVE"
               WRITE(4,*) TRANSACTION_NAME(TRANS)," IS OUTSIDE OF THE"
               WRITE(4,*) "VALID BOUNDS. ASSET_CLASS = ",ASSET_CLASS
               CYCLE
            ENDIF
!
            TOTAL_FUEL_BEGINNING_MONTH = MONTH_BEGINNING_FUEL(TRANS) + &
                                                 MONTH_FUEL_ADDED(TRANS)
            IF(DERIVATIVE_TYPE(TRANS) < 4) THEN
               FI = 2 ! PHYSICAL FIXED INDEX
            ELSEIF(DERIVATIVE_TYPE(TRANS) < 7) THEN
               FI = 4 ! FINANCIAL FIXED INDEX
            ELSE
               FI = 2 ! PHYSICAL AS DEFAULT FOR NOW
            ENDIF
!
! 01/04/06.
!
            IF(COST_ASSIGNMENT(TRANS) == 'P') THEN
               IF(SELL_EXCESS_PHYSICAL_FUEL(TRANS) == 'Y') THEN
! FIXED PHYSICAL REVENUE
                  FUEL_SOLD = HOURLY_QUANTITY(TRANS)
                  REVENUE_OF_FUEL_SOLD = &
                     HOURLY_QUANTITY(TRANS) * &
                          (HOURLY_SPOT_PRICE(TRANS) - &
                                       MONTHLY_PHYSICAL_SALE_FEE(TRANS))
                  MONTHLY_AC_REVENUE(ASSET_CLASS,FI,MONTH) = &
                     MONTHLY_AC_REVENUE(ASSET_CLASS,FI,MONTH) + &
                                                    REVENUE_OF_FUEL_SOLD
                  MONTHLY_AC_REVENUE(ASSET_CLASS,FI,0) = &
                     MONTHLY_AC_REVENUE(ASSET_CLASS,FI,0) + &
                                                    REVENUE_OF_FUEL_SOLD
                  MONTHLY_AC_REVENUE_ENERGY(ASSET_CLASS,FI,MONTH) = &
                     MONTHLY_AC_REVENUE_ENERGY(ASSET_CLASS,FI,MONTH) + &
                                                               FUEL_SOLD
                  MONTHLY_AC_REVENUE_ENERGY(ASSET_CLASS,FI,0) = &
                     MONTHLY_AC_REVENUE_ENERGY(ASSET_CLASS,FI,0) + &
                                                               FUEL_SOLD

!
                  AVE_COST_OF_INVENTORY = 0.
                  BURN_EXPENSE = HOURLY_COST(TRANS) * &
                                             MONTHLY_BURN(TRANS,R_MONTH)
               ELSE
! FIXED PHYSICAL EXPENSE
                  FUEL_STORED = HOURLY_QUANTITY(TRANS)
                  COST_OF_FUEL_STORED = &
                     HOURLY_QUANTITY(TRANS) * MONTHLY_STORAGE_FEE(TRANS)
                  MONTHLY_AC_EXPENSE(ASSET_CLASS,FI,MONTH) = &
                     MONTHLY_AC_EXPENSE(ASSET_CLASS,FI,MONTH) + &
                                                     COST_OF_FUEL_STORED
                  MONTHLY_AC_EXPENSE(ASSET_CLASS,FI,0) = &
                     MONTHLY_AC_EXPENSE(ASSET_CLASS,FI,0) + &
                                                     COST_OF_FUEL_STORED
!
! ONLY TRACKING BURN AT THIS TIME.
!
!
                  LAST_MONTH_STORED_FUEL(TRANS) = FUEL_STORED

!
                  MONTHLY_AC_EXPENSE_ENERGY(ASSET_CLASS,FI,MONTH) = &
                     MONTHLY_AC_EXPENSE_ENERGY(ASSET_CLASS,FI,MONTH) + &
                                                             FUEL_STORED
                  MONTHLY_AC_EXPENSE_ENERGY(ASSET_CLASS,FI,0) = &
                     MONTHLY_AC_EXPENSE_ENERGY(ASSET_CLASS,FI,0) + &
                                                             FUEL_STORED

!
                  IF(TOTAL_FUEL_BEGINNING_MONTH > 0.) THEN
                     AVE_COST_OF_INVENTORY = &
                                          MONTH_AVE_COST_OF_INV(TRANS)/ &
                                              TOTAL_FUEL_BEGINNING_MONTH
                  ELSE
                     AVE_COST_OF_INVENTORY = 0.
                  ENDIF
                  BURN_EXPENSE = AVE_COST_OF_INVENTORY * &
                                           MONTHLY_BURN(TRANS,R_MONTH)
               ENDIF ! SELL EXCESS FUEL
!
! 04/14/03. VAR PHYSICAL EXPENSE. BUY THE FUEL AT THE BEGINNING OF THE MONTH
! 04/16/03. TAKEN OUT BECAUSE FUEL EXPENSE FLOWS THROUGH THE UNIT
!
!
               TOTAL_FUEL_REMOVED = MONTHLY_BURN(TRANS,R_MONTH) + &
                                                               FUEL_SOLD
               NOT_BURNED_FUEL_EXPENSE = MAX(0., &
                         MONTH_COST_OF_FUEL_ADDED(TRANS) - BURN_EXPENSE)

! 05/23/03. OUT PER GERBER.
!           IF IN, THEN EXPENSES FOR THE DERIVATIVE ARE CAPTURED IN
!           THIS ROUTINE AS AGAIN (IE DOUBLE-COUNTED) WITH CLA_OBJT.FOR
! 05/29/03. IN PER BURESH. ONLY FOR PHYSICALS.
!
!
               IF(DERIVATIVE_TYPE(TRANS) < 4) THEN
                  MONTHLY_AC_EXPENSE(ASSET_CLASS,1,MONTH) = &
                     MONTHLY_AC_EXPENSE(ASSET_CLASS,1,MONTH) + &
                                         NOT_BURNED_FUEL_EXPENSE
                  MONTHLY_AC_EXPENSE(ASSET_CLASS,1,0) = &
                     MONTHLY_AC_EXPENSE(ASSET_CLASS,1,0) + &
                                         NOT_BURNED_FUEL_EXPENSE
                  MONTHLY_AC_EXPENSE(ASSET_CLASS,2,MONTH) = &
                     MONTHLY_AC_EXPENSE(ASSET_CLASS,2,MONTH) + &
                                        MONTH_OPTION_PREMIUM_COST(TRANS)
                  MONTHLY_AC_EXPENSE(ASSET_CLASS,2,0) = &
                     MONTHLY_AC_EXPENSE(ASSET_CLASS,2,0) + &
                                        MONTH_OPTION_PREMIUM_COST(TRANS)
               ELSEIF(DERIVATIVE_TYPE(TRANS) < 7) THEN
                  MONTHLY_AC_EXPENSE(ASSET_CLASS,3,MONTH) = &
                     MONTHLY_AC_EXPENSE(ASSET_CLASS,3,MONTH) + &
                                         NOT_BURNED_FUEL_EXPENSE
                  MONTHLY_AC_EXPENSE(ASSET_CLASS,3,0) = &
                     MONTHLY_AC_EXPENSE(ASSET_CLASS,3,0) + &
                                         NOT_BURNED_FUEL_EXPENSE
                  MONTHLY_AC_EXPENSE(ASSET_CLASS,4,MONTH) = &
                     MONTHLY_AC_EXPENSE(ASSET_CLASS,4,MONTH) + &
                                        MONTH_OPTION_PREMIUM_COST(TRANS)
                     MONTHLY_AC_EXPENSE(ASSET_CLASS,4,0) = &
                  MONTHLY_AC_EXPENSE(ASSET_CLASS,4,0) + &
                                        MONTH_OPTION_PREMIUM_COST(TRANS)
               ENDIF
!
            ELSE ! ! SALE !!!
!
               FUEL_SOLD = 0.
               COST_OF_FUEL_STORED = 0.
               FUEL_STORED = 0.
               REVENUE_OF_FUEL_STORED = 0.
!
               IF(SELL_EXCESS_PHYSICAL_FUEL(TRANS) == 'Y') THEN
! FIXED PHYSICAL COST
                  FUEL_SOLD = HOURLY_QUANTITY(TRANS)
                  COST_OF_FUEL_SOLD = &
                     HOURLY_QUANTITY(TRANS) * &
                          (HOURLY_SPOT_PRICE(TRANS) - &
                                       MONTHLY_PHYSICAL_SALE_FEE(TRANS))
                  MONTHLY_AC_EXPENSE(ASSET_CLASS,FI,MONTH) = &
                     MONTHLY_AC_EXPENSE(ASSET_CLASS,FI,MONTH) + &
                                                    COST_OF_FUEL_SOLD
                  MONTHLY_AC_EXPENSE(ASSET_CLASS,FI,0) = &
                     MONTHLY_AC_EXPENSE(ASSET_CLASS,FI,0) + &
                                                    COST_OF_FUEL_SOLD
                  MONTHLY_AC_EXPENSE_ENERGY(ASSET_CLASS,FI,MONTH) = &
                     MONTHLY_AC_EXPENSE_ENERGY(ASSET_CLASS,FI,MONTH) + &
                                                               FUEL_SOLD
                  MONTHLY_AC_EXPENSE_ENERGY(ASSET_CLASS,FI,0) = &
                     MONTHLY_AC_EXPENSE_ENERGY(ASSET_CLASS,FI,0) + &
                                                               FUEL_SOLD

!
                  AVE_COST_OF_INVENTORY = 0.
                  BURN_REVENUE = HOURLY_COST(TRANS) * &
                                             MONTHLY_BURN(TRANS,R_MONTH)
               ELSE
! FIXED PHYSICAL REVENUE
                  FUEL_STORED = HOURLY_QUANTITY(TRANS)
                  REVENUE_OF_FUEL_STORED = &
                     HOURLY_QUANTITY(TRANS) * MONTHLY_STORAGE_FEE(TRANS)
                  MONTHLY_AC_REVENUE(ASSET_CLASS,FI,MONTH) = &
                     MONTHLY_AC_REVENUE(ASSET_CLASS,FI,MONTH) + &
                                                  REVENUE_OF_FUEL_STORED
                  MONTHLY_AC_REVENUE(ASSET_CLASS,FI,0) = &
                     MONTHLY_AC_REVENUE(ASSET_CLASS,FI,0) + &
                                                  REVENUE_OF_FUEL_STORED
!
! ONLY TRACKING BURN AT THIS TIME.
!

!
! 04/14/03. NEED TO STORE FOR NEXT MONTH.
!
                  LAST_MONTH_STORED_FUEL(TRANS) = FUEL_STORED
!     +                         LAST_MONTH_STORED_FUEL(TRANS) +
!
                  MONTHLY_AC_REVENUE_ENERGY(ASSET_CLASS,FI,MONTH) = &
                     MONTHLY_AC_REVENUE_ENERGY(ASSET_CLASS,FI,MONTH) + &
                                                             FUEL_STORED
                  MONTHLY_AC_REVENUE_ENERGY(ASSET_CLASS,FI,0) = &
                     MONTHLY_AC_REVENUE_ENERGY(ASSET_CLASS,FI,0) + &
                                                             FUEL_STORED

!
                  IF(TOTAL_FUEL_BEGINNING_MONTH > 0.) THEN
                     AVE_COST_OF_INVENTORY = &
                                          MONTH_AVE_COST_OF_INV(TRANS)/ &
                                              TOTAL_FUEL_BEGINNING_MONTH
                  ELSE
                     AVE_COST_OF_INVENTORY = 0.
                  ENDIF
                  BURN_REVENUE = AVE_COST_OF_INVENTORY * &
                                           MONTHLY_BURN(TRANS,R_MONTH)
               ENDIF ! SELL EXCESS FUEL
!
! 04/14/03. VAR PHYSICAL EXPENSE. BUY THE FUEL AT THE BEGINNING OF THE MONTH
! 04/16/03. TAKEN OUT BECAUSE FUEL EXPENSE FLOWS THROUGH THE UNIT
!

               TOTAL_FUEL_REMOVED = MONTHLY_BURN(TRANS,R_MONTH) + &
                                                               FUEL_SOLD
               NOT_BURNED_FUEL_REVENUE = MAX(0., &
                         MONTH_COST_OF_FUEL_ADDED(TRANS) - BURN_REVENUE)
!            IF(TOTAL_FUEL_BEGINNING_MONTH > .0001) THEN
!
! 05/23/03. OUT PER GERBER.
!           IF IN, THEN EXPENSES FOR THE DERIVATIVE ARE CAPTURED IN
!           THIS ROUTINE AS AGAIN (IE DOUBLE-COUNTED) WITH CLA_OBJT.FOR
! 05/29/03. IN PER BURESH. ONLY FOR PHYSICALS.
!
!
               IF(DERIVATIVE_TYPE(TRANS) < 4) THEN
                  MONTHLY_AC_REVENUE(ASSET_CLASS,1,MONTH) = &
                     MONTHLY_AC_REVENUE(ASSET_CLASS,1,MONTH) + &
                                         NOT_BURNED_FUEL_REVENUE
                  MONTHLY_AC_REVENUE(ASSET_CLASS,1,0) = &
                     MONTHLY_AC_REVENUE(ASSET_CLASS,1,0) + &
                                         NOT_BURNED_FUEL_REVENUE
                  MONTHLY_AC_REVENUE(ASSET_CLASS,2,MONTH) = &
                     MONTHLY_AC_REVENUE(ASSET_CLASS,2,MONTH) + &
                                        MONTH_OPTION_PREMIUM_COST(TRANS)
                  MONTHLY_AC_REVENUE(ASSET_CLASS,2,0) = &
                     MONTHLY_AC_REVENUE(ASSET_CLASS,2,0) + &
                                        MONTH_OPTION_PREMIUM_COST(TRANS)
               ELSEIF(DERIVATIVE_TYPE(TRANS) < 7) THEN
                  MONTHLY_AC_REVENUE(ASSET_CLASS,3,MONTH) = &
                     MONTHLY_AC_REVENUE(ASSET_CLASS,3,MONTH) + &
                                         NOT_BURNED_FUEL_REVENUE
                  MONTHLY_AC_REVENUE(ASSET_CLASS,3,0) = &
                     MONTHLY_AC_REVENUE(ASSET_CLASS,3,0) + &
                                         NOT_BURNED_FUEL_REVENUE
                  MONTHLY_AC_REVENUE(ASSET_CLASS,4,MONTH) = &
                     MONTHLY_AC_REVENUE(ASSET_CLASS,4,MONTH) + &
                                        MONTH_OPTION_PREMIUM_COST(TRANS)
                     MONTHLY_AC_REVENUE(ASSET_CLASS,4,0) = &
                  MONTHLY_AC_REVENUE(ASSET_CLASS,4,0) + &
                                        MONTH_OPTION_PREMIUM_COST(TRANS)
               ENDIF
            ENDIF ! PURCHASE OR SALE
!
            IF(MONTHLY_FUEL_DERIV_REPORT) THEN
!
               IF(MONTH_DERIV_REPORT_NOT_OPEN) THEN
                  MONTH_DERIV_REPORT_NOT_OPEN = .FALSE.
                  MONTHLY_FUEL_DERIV_UNIT = &
                           MONTHLY_FUEL_DERIV_RPT_HEADER( &
                                         MONTHLY_FUEL_DERIV_REC, &
                                            FUEL_REPORT_VARIABLE_NUMBER)
               ENDIF
! 05/22/03.
               ANNUAL_FUEL_REPORT(TRANS,1) = &
                                ANNUAL_FUEL_REPORT(TRANS,1) + &
                                             MONTH_BEGINNING_FUEL(TRANS)
               ANNUAL_FUEL_REPORT(TRANS,2) = &
                                ANNUAL_FUEL_REPORT(TRANS,2) + &
                                                 MONTH_FUEL_ADDED(TRANS)
               ANNUAL_FUEL_REPORT(TRANS,3) = &
                                ANNUAL_FUEL_REPORT(TRANS,3) + &
                                         MONTH_COST_OF_FUEL_ADDED(TRANS)
               ANNUAL_FUEL_REPORT(TRANS,4) = &
                                ANNUAL_FUEL_REPORT(TRANS,4) + &
                                          MONTH_COST_OF_SPOT_FUEL(TRANS)
               ANNUAL_FUEL_REPORT(TRANS,5) = &
                                ANNUAL_FUEL_REPORT(TRANS,5) + &
                                           MONTHLY_BURN(TRANS,R_MONTH)
               ANNUAL_FUEL_REPORT(TRANS,6) = &
                                ANNUAL_FUEL_REPORT(TRANS,6) + &
                                           MONTH_SPOT_FUEL_BURNED(TRANS)
               ANNUAL_FUEL_REPORT(TRANS,7) = &
                                ANNUAL_FUEL_REPORT(TRANS,7) + &
                                          MONTH_TOTAL_FUEL_BURNED(TRANS)
               ANNUAL_FUEL_REPORT(TRANS,8) = &
                                ANNUAL_FUEL_REPORT(TRANS,8) + &
                                                               FUEL_SOLD
               ANNUAL_FUEL_REPORT(TRANS,9) = &
                                ANNUAL_FUEL_REPORT(TRANS,9) + &
                                                    REVENUE_OF_FUEL_SOLD
               ANNUAL_FUEL_REPORT(TRANS,10) = &
                                ANNUAL_FUEL_REPORT(TRANS,10) + &
                                                             FUEL_STORED
               ANNUAL_FUEL_REPORT(TRANS,11) = &
                                ANNUAL_FUEL_REPORT(TRANS,11) + &
                                                     COST_OF_FUEL_STORED
               ANNUAL_FUEL_REPORT(TRANS,12) = &
                                ANNUAL_FUEL_REPORT(TRANS,12) + &
                                        MONTH_OPTION_PREMIUM_COST(TRANS)
               ANNUAL_FUEL_REPORT(TRANS,13) = &
                                ANNUAL_FUEL_REPORT(TRANS,13) + &
                                                HOURLY_SPOT_PRICE(TRANS)
               ANNUAL_FUEL_REPORT(TRANS,14) = &
                                ANNUAL_FUEL_REPORT(TRANS,14) + &
                                                     HOURLY_PRICE(TRANS)
               ANNUAL_FUEL_REPORT(TRANS,15) = &
                                ANNUAL_FUEL_REPORT(TRANS,15) + &
                                                      HOURLY_COST(TRANS)
               ANNUAL_FUEL_REPORT(TRANS,17) = &
                                ANNUAL_FUEL_REPORT(TRANS,17) + &
                                                            BURN_EXPENSE
               ! TODO: Determine what this refers to.
               ANNUAL_FUEL_REPORT(TRANS,18) = &
                                ANNUAL_FUEL_REPORT(TRANS,18) + &
                                                 NOT_BURNED_FUEL_EXPENSE
!
!
               WRITE(MONTHLY_FUEL_DERIV_UNIT, &
                REC=MONTHLY_FUEL_DERIV_REC) &
                PRT_ENDPOINT(), &
                FLOAT(R_YEAR), &
                CL_MONTH_NAME(R_MONTH),          & !
                TRANSACTION_NAME(TRANS), &
                MONTH_BEGINNING_FUEL(TRANS),     & !  FUEL ADDED (?)
                MONTH_FUEL_ADDED(TRANS), &
                MONTH_COST_OF_FUEL_ADDED(TRANS), & !  BEGINNING FUEL (?)  
                MONTH_COST_OF_SPOT_FUEL(TRANS),    & !  SPOT MARKET FUEL COST
                MONTHLY_BURN(TRANS,R_MONTH),     & !  DERIVATIVE FUEL BURN
                MONTH_SPOT_FUEL_BURNED(TRANS), & 
                MONTH_TOTAL_FUEL_BURNED(TRANS), &
                FUEL_SOLD,                       & 
                REVENUE_OF_FUEL_SOLD, &
                FUEL_STORED, &
                COST_OF_FUEL_STORED,             & 
                MONTH_OPTION_PREMIUM_COST(TRANS), & 
                HOURLY_SPOT_PRICE(TRANS), &
                HOURLY_PRICE(TRANS), &
                HOURLY_COST(TRANS), &
                AVE_COST_OF_INVENTORY, &
                BURN_EXPENSE, &
                NOT_BURNED_FUEL_EXPENSE
!
               MONTHLY_FUEL_DERIV_REC = MONTHLY_FUEL_DERIV_REC + 1
!
            ENDIF
! END OF MONTH CALC
            MONTH_AVE_COST_OF_INV(TRANS) = &
                             MONTH_AVE_COST_OF_INV(TRANS) - &
                                         AVE_COST_OF_INVENTORY * &
                                                      TOTAL_FUEL_REMOVED
!
         ENDDO
!
         IF(MONTH == 12) THEN
            DO TRANS = 1, SAVE_ACTIVE_TRANSACTIONS
               IF(MONTHLY_PRODUCT_MONTHS(TRANS) > 0) THEN

                  ANNUAL_FUEL_REPORT(TRANS,10) = &
                           ANNUAL_FUEL_REPORT(TRANS,1) + &
                                 ANNUAL_FUEL_REPORT(TRANS,2) - &
                                          ANNUAL_FUEL_REPORT(TRANS,5)
                  ANNUAL_FUEL_REPORT(TRANS,13) = &
                          ANNUAL_FUEL_REPORT(TRANS,13)/ &
                                    FLOAT(MONTHLY_PRODUCT_MONTHS(TRANS))
                  ANNUAL_FUEL_REPORT(TRANS,14) = &
                          ANNUAL_FUEL_REPORT(TRANS,14)/ &
                                    FLOAT(MONTHLY_PRODUCT_MONTHS(TRANS))
                  ANNUAL_FUEL_REPORT(TRANS,15) = &
                          ANNUAL_FUEL_REPORT(TRANS,15)/ &
                                    FLOAT(MONTHLY_PRODUCT_MONTHS(TRANS))
                  ANNUAL_FUEL_REPORT(TRANS,16) = AVE_COST_OF_INVENTORY
               ELSE
               ENDIF
               IF(MONTHLY_FUEL_DERIV_REPORT .AND. &
                                    ANNUAL_ACTIVE_TRANSACTIONS > 0) THEN

                  WRITE(MONTHLY_FUEL_DERIV_UNIT, &
                                             REC=MONTHLY_FUEL_DERIV_REC) &
                              PRT_ENDPOINT(),                & !
                              FLOAT(R_YEAR),                 & !
                              CL_MONTH_NAME(13),          & !
                              TRANSACTION_NAME(TRANS), &
                              (ANNUAL_FUEL_REPORT(TRANS,I),I=1, &
                                            FUEL_REPORT_VARIABLE_NUMBER)
!

                  MONTHLY_FUEL_DERIV_REC = MONTHLY_FUEL_DERIV_REC + 1
               ENDIF
            ENDDO
         ENDIF
!
         END_MONTH_FUEL_DERIVATIVES = .TRUE.
      RETURN

      ENTRY RETURN_FUEL_DERIV_VARIABLES( R_CLASS, &
                                         R_MONTH, &
                                         PHY_DERIV_VAR_REVENUE, &
                                         PHY_DERIV_FIX_REVENUE, &
                                         PHY_DERIV_VAR_EXPENSE, &
                                         PHY_DERIV_FIX_EXPENSE, &
                                         FIN_DERIV_VAR_REVENUE, &
                                         FIN_DERIV_FIX_REVENUE, &
                                         FIN_DERIV_VAR_EXPENSE, &
                                         FIN_DERIV_FIX_EXPENSE, &
                                         PHY_DERIV_REVENUE_ENERGY, &
                                         PHY_DERIV_EXPENSE_ENERGY, &
                                         FIN_DERIV_REVENUE_ENERGY, &
                                         FIN_DERIV_EXPENSE_ENERGY)

!
         RETURN_FUEL_DERIV_VARIABLES = .FALSE.
!
         PHY_DERIV_VAR_REVENUE = 0.
         PHY_DERIV_FIX_REVENUE = 0.
         PHY_DERIV_VAR_EXPENSE = 0.
         PHY_DERIV_FIX_EXPENSE= 0.
         PHY_DERIV_REVENUE_ENERGY = 0.
         PHY_DERIV_EXPENSE_ENERGY = 0.
         FIN_DERIV_VAR_REVENUE = 0.
         FIN_DERIV_FIX_REVENUE = 0.
         FIN_DERIV_VAR_EXPENSE = 0.
         FIN_DERIV_FIX_EXPENSE = 0.
         FIN_DERIV_REVENUE_ENERGY = 0.
         FIN_DERIV_EXPENSE_ENERGY = 0.
         IF( .NOT. SAVE_FUEL_DERIVATIVES_STATUS .OR. &
                                              .NOT. RUN_TRANSACT) RETURN
!
         IF(R_CLASS-1 <= MAX_ASSET_GROUPS .AND. R_CLASS-1 >= 0) THEN
            ASSET_CLASS = ASSET_CLASS_GROUPS_INDEX(R_CLASS-1)
!
!
!
            IF(ASSET_CLASS >= 0) THEN
!
               MONTH = R_MONTH
               MONTH = TRANSACT_ANLST_RSLTS_AVAIL_STG()
               IF(MONTH >= 13) RETURN
!
               RETURN_FUEL_DERIV_VARIABLES = .TRUE.
!
               PHY_DERIV_VAR_REVENUE = .000001 * &
                           SUM(MONTHLY_AC_REVENUE(ASSET_CLASS,1,MONTH:))
               PHY_DERIV_FIX_REVENUE = .000001 * &
                           SUM(MONTHLY_AC_REVENUE(ASSET_CLASS,2,MONTH:))
               PHY_DERIV_VAR_EXPENSE = .000001 * &
                           SUM(MONTHLY_AC_EXPENSE(ASSET_CLASS,1,MONTH:))
               PHY_DERIV_FIX_EXPENSE= .000001 * &
                           SUM(MONTHLY_AC_EXPENSE(ASSET_CLASS,2,MONTH:))
!
               PHY_DERIV_REVENUE_ENERGY = .001 * &
                    SUM(MONTHLY_AC_REVENUE_ENERGY(ASSET_CLASS,2,MONTH:))
               PHY_DERIV_EXPENSE_ENERGY = .001 * &
                    SUM(MONTHLY_AC_EXPENSE_ENERGY(ASSET_CLASS,2,MONTH:))
!
!
               FIN_DERIV_VAR_REVENUE = .000001 * &
                           SUM(MONTHLY_AC_REVENUE(ASSET_CLASS,3,MONTH:))
               FIN_DERIV_FIX_REVENUE = .000001 * &
                           SUM(MONTHLY_AC_REVENUE(ASSET_CLASS,4,MONTH:))
               FIN_DERIV_VAR_EXPENSE = .000001 * &
                           SUM(MONTHLY_AC_EXPENSE(ASSET_CLASS,3,MONTH:))
               FIN_DERIV_FIX_EXPENSE = .000001 * &
                           SUM(MONTHLY_AC_EXPENSE(ASSET_CLASS,4,MONTH:))
!
               FIN_DERIV_REVENUE_ENERGY = .001 * &
                    SUM(MONTHLY_AC_REVENUE_ENERGY(ASSET_CLASS,3,MONTH:))
               FIN_DERIV_EXPENSE_ENERGY = .001 * &
                    SUM(MONTHLY_AC_EXPENSE_ENERGY(ASSET_CLASS,3,MONTH:))
            ENDIF
         ENDIF
      RETURN

      ENTRY RET_MLY_FUEL_DERIV_INC_VARS(R_CLASS, &
                                                  INC_MONTH_VARS)

!
         RET_MLY_FUEL_DERIV_INC_VARS = 1
!
         IF( .NOT. SAVE_FUEL_DERIVATIVES_STATUS .OR. &
                                              .NOT. RUN_TRANSACT) RETURN
         IF(R_CLASS-1 <= MAX_ASSET_GROUPS .AND. R_CLASS-1 >= 0) THEN
            ASSET_CLASS = ASSET_CLASS_GROUPS_INDEX(R_CLASS-1)
!
            IF(ASSET_CLASS >= 0) THEN
               MONTH = TRANSACT_ANLST_RSLTS_AVAIL_STG()
               IF(MONTH >= 13) RETURN
!
               DO MO = MONTH, 12
!
                  INC_MONTH_VARS(MO,PhysicalFuelDerivRevVar) = &
                       INC_MONTH_VARS(MO,PhysicalFuelDerivRevVar) &
                       + MONTHLY_AC_REVENUE(ASSET_CLASS,1,MO)*.000001
!
                  INC_MONTH_VARS(MO,PhysicalFuelDerivRevFix) = &
                       INC_MONTH_VARS(MO,PhysicalFuelDerivRevFix) &
                       + MONTHLY_AC_REVENUE(ASSET_CLASS,2,MO)*.000001
!
                  INC_MONTH_VARS(MO,PhysicalFuelDerivExpVar) = &
                       INC_MONTH_VARS(MO,PhysicalFuelDerivExpVar) &
                       + MONTHLY_AC_EXPENSE(ASSET_CLASS,1,MO)*.000001
!
                  INC_MONTH_VARS(MO,PhysicalFuelDerivExpFix) = &
                       INC_MONTH_VARS(MO,PhysicalFuelDerivExpFix) &
                       + MONTHLY_AC_EXPENSE(ASSET_CLASS,2,MO)*.000001
!
                  INC_MONTH_VARS(MO,FinancialFuelDerivRevVar) = &
                       INC_MONTH_VARS(MO,FinancialFuelDerivRevVar) &
                       + MONTHLY_AC_REVENUE(ASSET_CLASS,3,MO)*.000001
!
                  INC_MONTH_VARS(MO,FinancialFuelDerivRevFix) = &
                       INC_MONTH_VARS(MO,FinancialFuelDerivRevFix) &
                       + MONTHLY_AC_REVENUE(ASSET_CLASS,4,MO)*.000001
!
                  INC_MONTH_VARS(MO,FinancialFuelDerivExpVar) = &
                       INC_MONTH_VARS(MO,FinancialFuelDerivExpVar) &
                       + MONTHLY_AC_EXPENSE(ASSET_CLASS,3,MO)*.000001
!
                  INC_MONTH_VARS(MO,FinancialFuelDerivExpFix) = &
                       INC_MONTH_VARS(MO,FinancialFuelDerivExpFix) &
                       + MONTHLY_AC_EXPENSE(ASSET_CLASS,4,MO)*.000001
!
               ENDDO
            ENDIF
         ENDIF
      RETURN

      ENTRY RET_MNTHLY_FUEL_DERIV_CASH_VARS(R_CLASS,CASH_MONTH_VARS)

         RET_MNTHLY_FUEL_DERIV_CASH_VARS = 1
!
         IF( .NOT. SAVE_FUEL_DERIVATIVES_STATUS .OR. &
                                              .NOT. RUN_TRANSACT) then
             RETURN
         end if
         IF(R_CLASS-1 <= MAX_ASSET_GROUPS .AND. R_CLASS-1 >= 0) THEN
            ASSET_CLASS = ASSET_CLASS_GROUPS_INDEX(R_CLASS-1)
!
            IF(ASSET_CLASS >= 0) THEN
               MONTH = TRANSACT_ANLST_RSLTS_AVAIL_STG()
               IF(MONTH >= 13) RETURN
!
               DO MO = MONTH, 12
                  CASH_MONTH_VARS(MO,cash_phys_fuel_deriv_rev_var) = &
                    CASH_MONTH_VARS(MO,cash_phys_fuel_deriv_rev_var) &
                    + MONTHLY_AC_REVENUE(ASSET_CLASS,1,MO)*.000001
                  CASH_MONTH_VARS(MO,CashPhysicalFuelDerivRevFix) = &
                    CASH_MONTH_VARS(MO,CashPhysicalFuelDerivRevFix) &
                    + MONTHLY_AC_REVENUE(ASSET_CLASS,2,MO)*.000001
                  CASH_MONTH_VARS(MO,CashPhysicalFuelDerivExpVar) = &
                    CASH_MONTH_VARS(MO,CashPhysicalFuelDerivExpVar) &
                    + MONTHLY_AC_EXPENSE(ASSET_CLASS,1,MO)*.000001
                  CASH_MONTH_VARS(MO,CashPhysicalFuelDerivExpFix) = &
                    CASH_MONTH_VARS(MO,CashPhysicalFuelDerivExpFix) &
                    + MONTHLY_AC_EXPENSE(ASSET_CLASS,2,MO)*.000001
                  CASH_MONTH_VARS(MO,cash_fin_fuel_deriv_rev_var)= &
                   CASH_MONTH_VARS(MO,cash_fin_fuel_deriv_rev_var) &
                   + MONTHLY_AC_REVENUE(ASSET_CLASS,3,MO)*.000001
                  CASH_MONTH_VARS(MO,CashFinancialFuelDerivRevFix)= &
                   CASH_MONTH_VARS(MO,CashFinancialFuelDerivRevFix) &
                   + MONTHLY_AC_REVENUE(ASSET_CLASS,4,MO)*.000001
                  CASH_MONTH_VARS(MO,CashFinancialFuelDerivExpVar)= &
                   CASH_MONTH_VARS(MO,CashFinancialFuelDerivExpVar) &
                   + MONTHLY_AC_EXPENSE(ASSET_CLASS,3,MO)*.000001
                  CASH_MONTH_VARS(MO,CashFinancialFuelDerivExpFix)= &
                   CASH_MONTH_VARS(MO,CashFinancialFuelDerivExpFix) &
                   + MONTHLY_AC_EXPENSE(ASSET_CLASS,4,MO)*.000001
               ENDDO
            ENDIF
         ENDIF
      RETURN

      ENTRY FUEL_DERIVATIVES_FILE_STATUS()

!
! THERE ARE ACTIVE DERIVATIVES IN THE MONTH.
!
         FUEL_DERIVATIVES_FILE_STATUS = MONTHLY_ACTIVE_TRANSACTIONS > 0
!
      RETURN

      ENTRY DAILY_FORWARD_FUEL_DERIVATIVES(R_DAY,R_MONTH)

         DAILY_FORWARD_FUEL_DERIVATIVES = .FALSE.
         DAILY_FORWARD_FUEL_DERIVATIVES = .TRUE.
      RETURN

      ENTRY UNIT_HAS_FUEL_DERIVATIVE(R_UNIT_ID)

! ALTERED 042506 PER HANSON.
         UNIT_HAS_FUEL_DERIVATIVE = .FALSE.
         IF(.NOT. SAVE_FUEL_DERIVATIVES_STATUS .OR. &
                                              .NOT. RUN_TRANSACT) RETURN
         IF(R_UNIT_ID > 0 .AND. R_UNIT_ID < L_NUNITS) THEN
            IF(T_COUNT(R_UNIT_ID) > 0) THEN
               UNIT_HAS_FUEL_DERIVATIVE = .TRUE.
            ENDIF
         ENDIF

      RETURN

      ENTRY MONTHLY_UNIT_FUEL_DERIV_HEAT(R_UNIT_ID, &
                                         R_MMBTUS, &
                                         R_FUEL_DERIVATIVE_PRICE, &
                                         R_MONTH)

! ALTERED 042506 PER HANSON.
! NEED COMPLETE REWRITE.

         R_FUEL_DERIVATIVE_PRICE = 0.
         MONTHLY_UNIT_FUEL_DERIV_HEAT = 0.0
         I = 1
         DO
            IF(I > T_COUNT(R_UNIT_ID)) EXIT
            TRANS = ANNUAL_UNIT_TRANS_INDEX(R_UNIT_ID,I)
            R_FUEL_DERIVATIVE_PRICE = R_FUEL_DERIVATIVE_PRICE + &
                  HOURLY_COST(TRANS)* MONTHLY_UNIT_BURN(TRANS,R_UNIT_ID)
            MONTHLY_UNIT_FUEL_DERIV_HEAT = &
                        MONTHLY_UNIT_FUEL_DERIV_HEAT + &
                                DBLE(MONTHLY_UNIT_BURN(TRANS,R_UNIT_ID))
            IF(MONTHLY_UNIT_FUEL_DERIV_HEAT > R_MMBTUS) THEN
               WRITE(4,*) 'MMBTUS FOR A RESOURCE FUEL DERIVATIVES'
               WRITE(4,*) 'EXCEEDS THE TOTAL MMBTUS FOR THIS RESOURCE'
               WRITE(4,*) 'DERIVATIVE NAME=',TRANSACTION_NAME(TRANS)
            ENDIF
            I = I + 1
         ENDDO
         IF(MONTHLY_UNIT_FUEL_DERIV_HEAT > .000001) THEN
            R_FUEL_DERIVATIVE_PRICE = R_FUEL_DERIVATIVE_PRICE/ &
                                            MONTHLY_UNIT_FUEL_DERIV_HEAT
         ENDIF
      RETURN

      ENTRY REDUCE_HOURLY_FUEL_DERIVATIVES(R_HOUR, &
                                           R_UNIT_ID, &
                                           R_HEAT, &
                                           R_MONTH, &
                                           R_MARKET_PRICE, &
                                           R_YEAR)

         REDUCE_HOURLY_FUEL_DERIVATIVES = .FALSE.
         IF( .NOT. SAVE_FUEL_DERIVATIVES_STATUS) RETURN
         IF(R_UNIT_ID <= 0 .OR. R_HEAT <= 0.00001) RETURN

! 041906. ALLOW FOR A UNIT TO HAVE MANY FUEL DERIVATIVES.

         I = 1

         TEST_DATE = 100*(R_YEAR - 1900) + R_MONTH

         LOCAL_HEAT = R_HEAT

         TEMP_R = 0.

! DOES FUEL DERIVATIVE BURN EXCEED LOCAL_HEAT. IF SO, TO WHICH DERIV DO WE ASSIGN THE SPOT?
!
         DO

            IF(I > T_COUNT(R_UNIT_ID)) EXIT
            TRANS = ANNUAL_UNIT_TRANS_INDEX(R_UNIT_ID,I)
            IF(TRANS <= 0) EXIT

            IF(TRANS > 0) THEN

               IF(DERIVATIVE_TYPE(TRANS) == 1 .OR. &
                           DERIVATIVE_TYPE(TRANS) == 4 .OR. &
                                               HOURLY_IN_THE_MONEY) THEN
                  TEMP_R = TEMP_R + HOURLY_QUANTITY(TRANS)
                  MARGINAL_FUEL_DERIV = TRANS
               ENDIF
            ENDIF
            IF(TEMP_R > LOCAL_HEAT*.001) THEN

               EXIT
            ENDIF
            I = I + 1
         ENDDO

         I = 1

         DO
!
            IF(I > T_COUNT(R_UNIT_ID)) EXIT
            TRANS = ANNUAL_UNIT_TRANS_INDEX(R_UNIT_ID,I)
            IF(TRANS <= 0) EXIT

            IF(TRANS > 0 .AND. LOCAL_HEAT > 0.00001 .AND. &
                   (HOURLY_QUANTITY(TRANS) > .00001 .OR. &
                                      TRANS == MARGINAL_FUEL_DERIV).AND. &
                       TEST_DATE <= END_EP(TRANS) .AND. &
                                      TEST_DATE >= BEGIN_EP(TRANS)) THEN
               HOURLY_IN_THE_MONEY = .TRUE.

               HOURLY_TRANS_HEAT(R_HOUR,TRANS) = &
                            HOURLY_TRANS_HEAT(R_HOUR,TRANS) + LOCAL_HEAT
!
               IF(DERIVATIVE_TYPE(TRANS) == 1 .OR. &
                           DERIVATIVE_TYPE(TRANS) == 4 .OR. &
                                               HOURLY_IN_THE_MONEY) THEN
!                 NOTE UNITS ON LOCAL_HEAT
                  DERIV_FUEL_USED = &
                     MAX(0.,MIN(HOURLY_QUANTITY(TRANS),LOCAL_HEAT*.001)) 
!
                  IF(TRANS == MARGINAL_FUEL_DERIV) THEN
                     MONTH_SPOT_FUEL_BURNED(TRANS) = &
                           MONTH_SPOT_FUEL_BURNED(TRANS) + &
                         MAX(0.,LOCAL_HEAT - DERIV_FUEL_USED*1000.)*.001
                     MONTH_COST_OF_SPOT_FUEL(TRANS) = &
                                 MONTH_COST_OF_SPOT_FUEL(TRANS) + &
                         HOURLY_PRICE(TRANS)* &
                         MAX(0.,LOCAL_HEAT - DERIV_FUEL_USED*1000.)*.001
                     MONTH_TOTAL_FUEL_BURNED(TRANS) = &
                        MONTH_TOTAL_FUEL_BURNED(TRANS) + LOCAL_HEAT*.001
                  ELSE ! NOT THE MARGINAL FUEL
                     MONTH_TOTAL_FUEL_BURNED(TRANS) = &
                        MONTH_TOTAL_FUEL_BURNED(TRANS) + DERIV_FUEL_USED
                  ENDIF



                  LOCAL_HEAT = LOCAL_HEAT - DERIV_FUEL_USED*1000.
!
!
                  HOURLY_QUANTITY(TRANS) = &
                                HOURLY_QUANTITY(TRANS) - DERIV_FUEL_USED
                  MONTHLY_UNIT_BURN(TRANS,R_UNIT_ID) = &
                                 MONTHLY_UNIT_BURN(TRANS,R_UNIT_ID) + &
                                                         DERIV_FUEL_USED
                  MONTHLY_BURN(TRANS,R_MONTH) = &
                                         MONTHLY_BURN(TRANS,R_MONTH) + &
                                                         DERIV_FUEL_USED
!
! REVENUE AND COST ARE FROM THE PERSPECTIVE OF BUYING A PRODUCT
!
                  MONTHLY_BURN_REVENUE(TRANS,R_MONTH) = &
                        MONTHLY_BURN_REVENUE(TRANS,R_MONTH) + &
                                   HOURLY_PRICE(TRANS) * DERIV_FUEL_USED
!
                  IF(PRICE_TYPE(TRANS) == 'F') THEN
                     MONTHLY_BURN_COST(TRANS,R_MONTH) = &
                        MONTHLY_BURN_COST(TRANS,R_MONTH) + &
                                    HOURLY_COST(TRANS) * DERIV_FUEL_USED
                  ELSE
                     MONTHLY_BURN_COST(TRANS,R_MONTH) = &
                          MONTHLY_BURN_COST(TRANS,R_MONTH) + &
                                   HOURLY_PRICE(TRANS) * DERIV_FUEL_USED
                  ENDIF

                  REDUCE_HOURLY_FUEL_DERIVATIVES = .TRUE.
               ENDIF
            ENDIF
            I = I + 1
         ENDDO ! I COUNTER
!
!
      RETURN
      END!
!
!
!

      RECURSIVE SUBROUTINE CREATE_LONG_PATHS_LOADINGS( &
                                          R_SELLER,R_BUYER, &
                                          R_CURRENT_PATH_MW, &
                                          R_RECURSIVE_COUNT)
      use end_routine
      implicit none
      INTEGER (KIND=2) :: J,K,L1,L2,R_SELLER,R_BUYER,PATH
      INTEGER (KIND=2) :: GET_HOUR_PATHS_INDEX
      INTEGER (KIND=2) :: GET_HOUR_PATH_FOR_LONG_PATH
      INTEGER (KIND=2) :: GET_HOUR_WHEEL_PATH,R_RECURSIVE_COUNT
      REAL (KIND=4) ::  R_CURRENT_PATH_MW,CURRENT_PATH_MW,TEMP_R
      REAL (KIND=4) ::  CHANGE_THIS_PATH_SEGMENT
!
! END DATA DECLARATIONS
!
         PATH = GET_HOUR_PATHS_INDEX(R_SELLER,R_BUYER)
         IF(GET_HOUR_PATH_FOR_LONG_PATH(PATH) > 0) THEN
!
            J = GET_HOUR_PATH_FOR_LONG_PATH(PATH)
!
            K = 1
            L1 = GET_HOUR_WHEEL_PATH(J,K)
            K = K + 1
!
! 01/22/03. THIS NEEDS TO BE RECURSIVE.
!
            DO WHILE(K <= 10 .AND. GET_HOUR_WHEEL_PATH(J,K) > 0)
               L2 = GET_HOUR_WHEEL_PATH(J,K)
!
               PATH = GET_HOUR_PATHS_INDEX(L1,L2)
               IF(GET_HOUR_PATH_FOR_LONG_PATH(PATH) > 0) THEN
                  CALL CREATE_LONG_PATHS_LOADINGS( &
                                          L1,L2, &
                                          CURRENT_PATH_MW, &
                                          R_RECURSIVE_COUNT)
                  R_RECURSIVE_COUNT = R_RECURSIVE_COUNT + 1
                  IF(R_RECURSIVE_COUNT > 10) THEN
                     R_RECURSIVE_COUNT = R_RECURSIVE_COUNT
                  ENDIF
               ENDIF
!
               TEMP_R = &
                       CHANGE_THIS_PATH_SEGMENT(L1,L2,R_CURRENT_PATH_MW)

               K = K + 1
               L1 = L2
            ENDDO
         ENDIF
      RETURN
      END
      !
!

!
!           ROUTINE TO CREATE A REGIONAL PARAMETER AND OUTAGE FILE
!
!                           COPYRIGHT (C) 2003
!                        M.S. GERBER & ASSOCIATES, INC.
!                           ALL RIGHTS RESERVED
!

!
      SUBROUTINE REG_OUT_PARAM_OBJECT
      use end_routine, only: end_program, er_message
      use logging
      use SpinDriftLib
      USE PROD_ARRAYS_DIMENSIONS
      USE SIZECOM
      use mod_base_year
      implicit none
      INTEGER (KIND=2) :: SCENARIO_YEAR,TEMP_SCENARIO_YEAR,TF_YEAR
      INTEGER (KIND=2) :: TEMP_YEAR,YEAR,STUDY_BASE_YEAR=0
      INTEGER (KIND=2) :: TRANSACTION_GROUP,TEMP_TRANSACTION_GROUP
      CHARACTER (LEN=1) :: RECORD_IS_ACTIVE,TIME_FRAME
      CHARACTER (LEN=1) :: TEMP_RECORD_IS_ACTIVE

      REAL (KIND=4) :: MONTHLY_VALUES(12),ANNUAL_VALUE

      CHARACTER (LEN=40) :: SCENARIO_VARIABLE,TEMP_SCENARIO_VARIABLE
      INTEGER (KIND=2) :: DELETE,INUNIT,IREC,LRECL=100,TEMP_DELETE
      INTEGER (KIND=2) :: IREC_OFFSET
      INTEGER :: IOS,IOS_BASE
      INTEGER (KIND=2) :: UNIT_NUM=10,R_MAKER_TABLES
      INTEGER (KIND=2) :: HOURLY_REFERENCE_NUMBER,FIRST_IREC,LAST_IREC
      CHARACTER (LEN=5) :: BASE_FILE_NAME,OVERLAY_FAMILY_NAME
      CHARACTER (LEN=5) :: REG_OUT_PARAM_FILE
      CHARACTER (LEN=256) :: FILE_NAME
      CHARACTER (LEN=256) :: BASE_FILE_DIRECTORY
      CHARACTER (LEN=256) :: DATA_DRIVE,OUTPUT_DIRECTORY
      LOGICAL (KIND=4) :: FILE_EXISTS,REG_OUT_PARAM_FILE_EXISTS=.FALSE.
      LOGICAL (KIND=4) :: R_REG_OUT_PARAM_FILE_EXISTS
!
!
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER (LEN=1024) :: RECLN
! DECLARATION FOR DAY TYPE DETERMINANTS
      CHARACTER (LEN=16) :: FILE_TYPE='Regional Param  '
      CHARACTER (LEN=2) :: REG_OUT_PARAM_OL='BC'
!
! MULTI-FILE VARIABLES
!
      INTEGER :: FILE_NUMBER,FILE_ID
      INTEGER (KIND=2) :: R_FILE_NUMBER
      INTEGER :: MAX_RM_FILES
      INTEGER (KIND=2) :: BC_MASTR_REC,CUR_REC,UNIT_NO
      INTEGER (KIND=2) :: OL_MASTR_REC
      PARAMETER (MAX_RM_FILES=5)
      INTEGER (KIND=2) :: MAKER_TABLES_IN_FILE(0:MAX_RM_FILES-1)
      CHARACTER (LEN=2), DIMENSION(0:4) :: RM_MULTI_OL_CODES='BC'
      CHARACTER (LEN=5) :: RM_FILE_BASE_NAMES(0:MAX_RM_FILES-1)
      CHARACTER (LEN=5) :: VOID_CHR
      CHARACTER (LEN=5) :: OVERLAY_FAMILY_NAMES(0:MAX_RM_FILES-1)
      CHARACTER (LEN=2) :: RM_FILE_CODES(0:MAX_RM_FILES-1)=(/ &
                                      'RM','R1','R2','R3','RO'/)
      CHARACTER (LEN=2) :: FILE_CODE
      CHARACTER (LEN=6) :: RM_FILE_BINARY_NAMES(0:MAX_RM_FILES-1)= &
                   (/'ROPFM','ROPF1','ROPF2','ROPF3','ROPF4'/)
      CHARACTER (LEN=6) :: BINARY_FILE_NAME
      LOGICAL,DIMENSION(0:MAX_RM_FILES-1):: ACTIVE_BASE_RM_FILES=.FALSE.
      LOGICAL, DIMENSION(0:MAX_RM_FILES-1) :: ACTIVE_OVERLAY_RM_FILES= &
       .FALSE.
      LOGICAL (KIND=1) :: OVERLAY_NAME_ACTIVE(0:MAX_RM_FILES-1)
      LOGICAL (KIND=1) :: RM_OVERLAY_MASTR_FILE_OPEN=.FALSE.
      LOGICAL (KIND=1) :: CHECK_4_SCENARIO_YEAR=.FALSE.
      LOGICAL (KIND=1) :: RECORD_BASED_OVERLAYS=.TRUE.
      LOGICAL (KIND=1) :: RECORD_BASED_STOCHASTIC_OLS
      LOGICAL (KIND=1) :: LAHEY_LF95
      CHARACTER*30 SCREEN_OUTPUT
! CONVERT THE DAY_TYPE FILE

      ENTRY REG_OUT_PARAM_MAKEBIN

!
      VOID_CHR = REG_OUT_PARAM_FILE(RM_FILE_BASE_NAMES)
!
      STUDY_BASE_YEAR = get_base_year()
      TF_YEAR = 0
      DATA_DRIVE = OUTPUT_DIRECTORY()
      IF(.NOT. LAHEY_LF95()) &
              CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
      MAKER_TABLES_IN_FILE = 0
      DO FILE_ID = 0, MAX_RM_FILES-1
         ACTIVE_BASE_RM_FILES(FILE_ID) = .FALSE.
         BASE_FILE_NAME = RM_FILE_BASE_NAMES(FILE_ID)
         IF(INDEX(BASE_FILE_NAME,'NONE') /= 0) CYCLE
         FILE_CODE = RM_FILE_CODES(FILE_ID)
         BINARY_FILE_NAME = RM_FILE_BINARY_NAMES(FILE_ID)
!
         FILE_NAME = trim(BASE_FILE_DIRECTORY())//FILE_CODE// &
                                     "B"//trim(BASE_FILE_NAME)//".DAT"
         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
         IF(FILE_EXISTS) THEN
!
            IF(LAHEY_LF95()) THEN
               SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
               CALL MG_CLEAR_LINE_WRITE(16,30,34,trim(SCREEN_OUTPUT), &
                                                         ALL_VERSIONS,0)
            ELSE
               CALL MG_CLEAR_LINE_WRITE(16,30,34,trim(BASE_FILE_NAME), &
                                                         ALL_VERSIONS,0)
            ENDIF
            ACTIVE_BASE_RM_FILES(FILE_ID) = .TRUE.
!
            REG_OUT_PARAM_FILE_EXISTS = FILE_EXISTS
      if(index(file_name, "BCMK")>0) then
      
        ! bcmkroup.bin read file being opened
        file_name=file_name
        call write_log_entry("wh_objt:0007", "Opening " // &
        trim(file_name) // " for read.")
      end if
            OPEN(10,FILE=FILE_NAME) ! File_type + "-" + bfn
            
       file_name=trim(OUTPUT_DIRECTORY())// &
                           "BC"//trim(BINARY_FILE_NAME)//".BIN"
                           
       if(index(FILE_NAME, "BCMK")>0) then
      
        ! Stop here for bcmkroup.bin
        file_name=file_name
      end if
      
            OPEN(11,FILE=file_name, &
               ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)

      
            IREC = 0
!
            READ(10,*) DELETE
            DO ! TABLES
               TF_YEAR = 1
               DO ! YEAR-BASED RECORDS
                  READ(10,1000,IOSTAT=IOS) RECLN
!
                  IF(RECLN(1:1) == '7' .OR. IOS /= 0) THEN
                     IF(TF_YEAR <= AVAIL_DATA_YEARS) THEN
                        DO TF_YEAR = TF_YEAR, AVAIL_DATA_YEARS
                           IREC = IREC + 1
                           WRITE(11,REC=IREC) DELETE, &
                              TF_YEAR+STUDY_BASE_YEAR, & !  INT2
                              RECORD_IS_ACTIVE, &
                              TRANSACTION_GROUP, &
                              SCENARIO_VARIABLE, &
                              TIME_FRAME, &
                              ANNUAL_VALUE, &
                              MONTHLY_VALUES
                        ENDDO
                     ENDIF ! DETECTED NEW TABLE OR END OF FILE
!
                     EXIT ! LEAVE LOOP (GO TO NEXT TABLE OR EXIT ROUTINE)
                  ELSE
!
!
! 02/04/03. ROUTINE FOR BURESH. TO TRAP FOR MULTIPLE VARIABLES WITHIN ONE TAB.
!
                     READ(RECLN,*,ERR=200) &
                           TEMP_DELETE, &
                           TEMP_SCENARIO_YEAR, &
                           TEMP_RECORD_IS_ACTIVE, &
                           TEMP_TRANSACTION_GROUP, &
                           TEMP_SCENARIO_VARIABLE
                     IF(TF_YEAR > 1 .AND. &
                              TEMP_SCENARIO_VARIABLE /= &
                                                 SCENARIO_VARIABLE) THEN
                        IF(TF_YEAR <= AVAIL_DATA_YEARS) THEN
                           DO TF_YEAR = TF_YEAR, AVAIL_DATA_YEARS
                              IREC = IREC + 1
                              WRITE(11,REC=IREC) DELETE, &
                                 TF_YEAR+STUDY_BASE_YEAR, & !  INT2
                                 RECORD_IS_ACTIVE, &
                                 TRANSACTION_GROUP, &
                                 SCENARIO_VARIABLE, &
                                 TIME_FRAME, &
                                 ANNUAL_VALUE, &
                                 MONTHLY_VALUES
                           ENDDO
                        ENDIF ! DETECTED NEW TABLE OR END OF FILE
!
                        TF_YEAR = 1 ! SO THAT IT PROCESSES THE FIRST RECORD OF NEXT VARIABLE
                        MAKER_TABLES_IN_FILE(FILE_ID) = &
                                       MAKER_TABLES_IN_FILE(FILE_ID) + 1

                     ENDIF
                  ENDIF
                  RECLN = &
                       trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
                  READ(RECLN,*,ERR=200) DELETE, &
                           SCENARIO_YEAR, &
                           RECORD_IS_ACTIVE, &
                           TRANSACTION_GROUP, &
                           SCENARIO_VARIABLE, &
                           TIME_FRAME, &
                           ANNUAL_VALUE, &
                           MONTHLY_VALUES
!
                  IREC = IREC + 1
!
                  READ(RECLN,*,ERR=200) DELETE,YEAR
                  TEMP_YEAR = &
                            MIN(AVAIL_DATA_YEARS,YEAR - STUDY_BASE_YEAR)
                  IF(TEMP_YEAR > TF_YEAR) THEN ! FILL IN MISSING YEARS
                     IF(IREC == 1) THEN
                        WRITE(4,*) &
                                  "The first year of the first table in"
                        WRITE(4,*) "the Scenario Maker file is ",YEAR
                        WRITE(4,*) "while the base year set in project"
                        WRITE(4,*) "information is ",STUDY_BASE_YEAR
                        WRITE(4,*) &
                                   "First forecast year in the Transact"
                        WRITE(4,*) &
                                  "Forecast file first year must be one"
                        WRITE(4,*) "year greater than the base year."
                        WRITE(4,*) '*** line 7022 TF_OBJT.FOR ***'

                     ENDIF
                     DO TF_YEAR = TF_YEAR, TEMP_YEAR - 1
                        WRITE(11,REC=IREC) DELETE, &
                           TF_YEAR+STUDY_BASE_YEAR, & !  INT2
                           RECORD_IS_ACTIVE, &
                           TRANSACTION_GROUP, &
                           SCENARIO_VARIABLE, &
                           TIME_FRAME, &
                           ANNUAL_VALUE, &
                           MONTHLY_VALUES
                        IREC = IREC + 1
                     ENDDO

                  ENDIF

                  WRITE(11,REC=IREC) DELETE, &
                           TF_YEAR+STUDY_BASE_YEAR, & !  INT2
                           RECORD_IS_ACTIVE, &
                           TRANSACTION_GROUP, &
                           SCENARIO_VARIABLE, &
                           TIME_FRAME, &
                           ANNUAL_VALUE, &
                           MONTHLY_VALUES
                  TF_YEAR = TF_YEAR + 1
               ENDDO ! LOAD GROUP
               MAKER_TABLES_IN_FILE(FILE_ID) = &
                                       MAKER_TABLES_IN_FILE(FILE_ID) + 1
               IF(IOS /= 0) EXIT
            ENDDO ! READ TABLES
            CLOSE(10)
            CLOSE(11)
!
         ELSE IF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
            CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "wh_objt:1113")
         ENDIF
      ENDDO ! FILE TYPES
      RETURN
! OVERLAY THE DAY TYPE FILE

      ENTRY REG_OUT_PARAM_MAKEOVL(OVERLAY_FAMILY_NAME,FILE_NUMBER)

!
      IF(.NOT. ACTIVE_BASE_RM_FILES(FILE_NUMBER)) RETURN
      FILE_CODE = RM_FILE_CODES(FILE_NUMBER)
      BINARY_FILE_NAME = RM_FILE_BINARY_NAMES(FILE_NUMBER)
!
      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_CLEAR_LINE_WRITE(17,9,36,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      FILE_NAME = trim(OUTPUT_DIRECTORY())//FILE_CODE//"O"// &
                                     trim(OVERLAY_FAMILY_NAME)//".DAT"
      OPEN(10,FILE=FILE_NAME) ! O file
      READ(10,*) DELETE ! FC+0+ofn+dat
      INUNIT = 12
      IF(RM_MULTI_OL_CODES(FILE_NUMBER) == 'BC') THEN
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BC"// &
                            trim(BINARY_FILE_NAME)//".BIN", &
                            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         INUNIT = 11
      ENDIF
      OPEN(12,FILE=trim(OUTPUT_DIRECTORY())//"OL"// &
                            trim(BINARY_FILE_NAME)//".BIN", &
                            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      IREC = 0

      TF_YEAR = 0
      READ(10,1000,IOSTAT=IOS) RECLN
      DO WHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE
!
         READ(10,1000,IOSTAT=IOS) RECLN
!
      ENDDO
!
      READ(RECLN,*,ERR=200) DELETE, &
                            TF_YEAR
!
      IREC = 0

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
            READ(INUNIT,REC=IREC,IOSTAT=IOS_BASE) &
                           DELETE, &
                           SCENARIO_YEAR, &
                           RECORD_IS_ACTIVE, &
                           TRANSACTION_GROUP, &
                           SCENARIO_VARIABLE, &
                           TIME_FRAME, &
                           ANNUAL_VALUE, &
                           MONTHLY_VALUES
            IF(IOS_BASE /= 0) EXIT ! END OF BINARY FILE
!
! DOES THE YEAR IN THE BINARY RECORD MATCH THE YEAR IN THE 
! OVERLAY DAT FILE?
!
            IF(TF_YEAR < SCENARIO_YEAR .AND. &
                           CHECK_4_SCENARIO_YEAR .AND. &
                                       .NOT. RECORD_BASED_OVERLAYS) THEN
!
               DO WHILE(TF_YEAR < SCENARIO_YEAR .AND. &
                                             DELETE /= 7 .AND. IOS == 0)
                  READ(10,1000,IOSTAT=IOS) RECLN
                  READ(RECLN,*,ERR=200) DELETE, &
                                        TF_YEAR
                  IREC_OFFSET = IREC_OFFSET + 1
               ENDDO
!
            ENDIF
!
            IF(SCENARIO_YEAR == TF_YEAR .OR. RECORD_BASED_OVERLAYS) THEN
               RECLN = trim(RECLN)//',,,,,,,,,,,,,'
               READ(RECLN,*,ERR=200) &
                           DELETE, &
                           TF_YEAR, & !  SCENARIO_YEAR, 10/4/02
                           RECORD_IS_ACTIVE, &
                           TRANSACTION_GROUP, &
                           SCENARIO_VARIABLE, &
                           TIME_FRAME, &
                           ANNUAL_VALUE, &
                           MONTHLY_VALUES
               READ(10,1000,IOSTAT=IOS) RECLN
!
               DO WHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE
!
                  READ(10,1000,IOSTAT=IOS) RECLN
!
               ENDDO
               READ(RECLN,*,ERR=200) DELETE, &
                                     TF_YEAR
            ENDIF
!
            WRITE(12,REC=IREC)  DELETE, &
                           SCENARIO_YEAR, &
                           RECORD_IS_ACTIVE, &
                           TRANSACTION_GROUP, &
                           SCENARIO_VARIABLE, &
                           TIME_FRAME, &
                           ANNUAL_VALUE, &
                           MONTHLY_VALUES
!
! TAKES CARE OF BINARY FILE GOING TO THE NEXT TABLE
!
            IF(SCENARIO_YEAR - STUDY_BASE_YEAR + IREC_OFFSET >= &
                                                  AVAIL_DATA_YEARS) THEN
!               IREC = IREC + IREC_OFFSET
               CHECK_4_SCENARIO_YEAR = .FALSE.
! 011310.
               IF(TF_YEAR > SCENARIO_YEAR) THEN
                  DO WHILE(TF_YEAR > SCENARIO_YEAR)
                     READ(10,1000,IOSTAT=IOS) RECLN
                     IF(IOS /= 0) EXIT
                     READ(RECLN,*,ERR=200) DELETE, &
                                        TF_YEAR
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
!      OVERLAY_TRANSACTIONS_IN_FILE = IREC
!
      IF(RM_MULTI_OL_CODES(FILE_NUMBER) == 'BC') CLOSE(11)
      RM_MULTI_OL_CODES(FILE_NUMBER) = 'OL'
      RETURN
!

  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from wh_objt SIID368'
      call end_program(er_message)

  300 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      CALL MG_LOCATE_WRITE(21,0, &
               'Error reading the Regional Parameter record. Look for'// &
                                          ' a "," in a character name.', &
                           ALL_VERSIONS,1)
      er_message='stop requested from wh_objt SIID369'
      call end_program(er_message)
!

      ENTRY RESET_REG_OUT_PARAM_OL

         DO FILE_ID = 0, MAX_RM_FILES-1
            RM_MULTI_OL_CODES(FILE_ID) = 'BC'

         ENDDO
      RETURN
!

      ENTRY OPEN_REG_OUT_PARAM_FILE(R_FILE_NUMBER)

         OPEN(UNIT_NUM, &
            FILE=trim(OUTPUT_DIRECTORY())// &
                           RM_MULTI_OL_CODES(R_FILE_NUMBER)// &
                trim(RM_FILE_BINARY_NAMES(R_FILE_NUMBER))//'.BIN', &
                            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      RETURN
!

      ENTRY CLOSE_REG_OUT_PARAM_FILE

         CLOSE(UNIT_NUM)
      RETURN

      ENTRY DOES_REG_OUT_PARAM_FILE_EXIST(R_REG_OUT_PARAM_FILE_EXISTS)

         R_REG_OUT_PARAM_FILE_EXISTS = REG_OUT_PARAM_FILE_EXISTS
      RETURN

      ENTRY IS_ACTIVE_BASE_RM_FILES(R_REG_OUT_PARAM_FILE_EXISTS, &
                                                          R_FILE_NUMBER)

         R_REG_OUT_PARAM_FILE_EXISTS = &
                                     ACTIVE_BASE_RM_FILES(R_FILE_NUMBER)
      RETURN


      ENTRY GET_ROPF_TABLES(R_MAKER_TABLES,R_FILE_NUMBER)

         R_MAKER_TABLES = MAKER_TABLES_IN_FILE(R_FILE_NUMBER)
      RETURN
!
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END
!
!

!
!
!        PROGRAM TO READ MULTI-TAB INFORMATION ON GLOBAL REGIONALS
!        FOR FORWARD PRICE DEVELOPMENT AND CONVERT TO BINARY FORMAT
!                       COPYRIGHT (C) 1998
!           ALL RIGHTS RESERVED M.S. GERBER & ASSOCIATES, INC.
!

!
!
!  READ FOR EACH ENDPOINT
!
!
      FUNCTION READ_REGIONAL_PARAMS_DATA()
      USE IREC_ENDPOINT_CONTROL
      use grx_planning_routines
      USE SIZECOM
      use SpinDriftLib
      USE PROD_ARRAYS_DIMENSIONS
      use globecom
      use mod_base_year
      use end_routine
      implicit none
      SAVE

      LOGICAL (KIND=1) :: READ_REGIONAL_PARAMS_DATA
      LOGICAL (KIND=1) :: SAVE_REGIONAL_PARAMS_STATUS=.FALSE.
      LOGICAL (KIND=1) :: REGIONAL_PARAMS_ACTIVE
      LOGICAL (KIND=1) :: IS_REGIONAL_HYDRO_PARAM_ACTIVE=.FALSE.
      LOGICAL (KIND=1) :: REGIONAL_VARIABLE_ACTIVE(:)
      LOGICAL (KIND=1) :: ACTIVE_TRANS_GROUP(:)
      LOGICAL (KIND=1) :: REGIONAL_HYDRO_PARAM_ACTIVE
      LOGICAL (KIND=4) :: REG_PARAMS_FILE_EXISTS
      INTEGER (KIND=2) :: YR,PARAMS_TABLES=0,DELETE,TRANSACTION_GROUP
      INTEGER (KIND=2) :: R_MONTH,PARAMS_MONTHS,R_YEAR,R_TG,R_FUEL_INDEX
      INTEGER (KIND=2) :: MO,I,LOCAL_MONTHS,LOCAL_YEARS,NUM_SCEN_VAR
      INTEGER (KIND=2) :: IREC,UPPER_TRANS_GROUP
      INTEGER (KIND=2) :: GET_NUMBER_OF_ACTIVE_GROUPS
      INTEGER (KIND=2) :: MAX_REGIONAL_INDEX=13
      INTEGER (KIND=2) :: TG,GET_TRANS_GROUP_POSITION
      INTEGER (KIND=2) :: GET_TRANS_GROUP_INDEX,FUEL_INDEX,FILE_ID
      INTEGER (KIND=2) :: GET_REGIONAL_SCENARIO_INDEX
! INPUT DATA LIST
      LOGICAL (KIND=4) :: VALUES_2_ZERO
      INTEGER (KIND=2) :: REGIONAL_YEAR,TABLE,REGIONAL_INDEX,LOAD_N
      CHARACTER (LEN=1) :: RECORD_IS_ACTIVE,TEMP_TIME_FRAME
      CHARACTER (LEN=40) :: REGIONAL_VARIABLE
      CHARACTER (LEN=40) :: REGIONAL_VARIABLE_NAME(:)
      CHARACTER(LEN=25) :: SCENARIO_VARIABLE_NAME(:)
      REAL (KIND=4) :: ANNUAL_VALUE,MONTHLY_VALUES(12)
      REAL (KIND=4) :: GET_PARAM_ELECTRIC_DEMAND,GET_PARAM_PEAK
      REAL (KIND=4) :: GET_PARAM_ENERGY,GET_PARAM_HYDRO
      REAL (KIND=4) :: GET_MONTHLY_REGIONAL_OUTAGE
      REAL (KIND=4) :: GET_MONTHLY_REGIONAL_PARAM
      REAL (KIND=4) :: SCEN_PARAMS_VARIABLE(:,:,:,:)
      ALLOCATABLE :: SCEN_PARAMS_VARIABLE,REGIONAL_VARIABLE_ACTIVE
      ALLOCATABLE :: REGIONAL_VARIABLE_NAME,SCENARIO_VARIABLE_NAME
      ALLOCATABLE :: ACTIVE_TRANS_GROUP
!
! 04/18/03. DETAILED REPORT FOR TORNADO CHARTS
!
      LOGICAL (KIND=1) :: REGIONAL_MAKER_REPORT_NOT_OPEN=.TRUE.
      LOGICAL (KIND=1) :: REGIONAL_MAKER_REPORT
      LOGICAL (KIND=1) :: YES_MONTHLY_SCEN_MAKER_REPORT
      INTEGER (KIND=2) :: REGIONAL_MAKER_RPT_HEADER
      INTEGER (KIND=2) :: REGIONAL_MAKER_UNIT=0,THIS_YEAR
      INTEGER :: REGIONAL_MAKER_REC
      REAL (KIND=4) :: AVERAGE_MULTIPLIER,STDDEV_OF_MULTIPLIER
      REAL (KIND=4) :: STDDEV_OF_MULT_ERROR,VOLATILITY
      
!
!
! END DATA DECLARATIONS
!
!
!
!
         READ_REGIONAL_PARAMS_DATA = .FALSE.
!
         CALL DOES_REG_OUT_PARAM_FILE_EXIST(REG_PARAMS_FILE_EXISTS)

         IF(REG_PARAMS_FILE_EXISTS) THEN
!
!
            PARAMS_MONTHS = 360
            LOCAL_MONTHS = 12
            LOCAL_YEARS = AVAIL_DATA_YEARS
!
            REGIONAL_MAKER_REPORT = YES_MONTHLY_SCEN_MAKER_REPORT()
!
            UPPER_TRANS_GROUP = GET_NUMBER_OF_ACTIVE_GROUPS()
!
            NUM_SCEN_VAR = 18
            IF( ALLOCATED(SCEN_PARAMS_VARIABLE) ) &
                      DEALLOCATE(SCEN_PARAMS_VARIABLE)
            ALLOCATE( &
                  SCEN_PARAMS_VARIABLE(  MAX_REGIONAL_INDEX, &
                                          UPPER_TRANS_GROUP, &
                                          LOCAL_MONTHS, &
                                          LOCAL_YEARS))
!
!
!
            SCEN_PARAMS_VARIABLE = 1.
!
            IF(ALLOCATED(REGIONAL_VARIABLE_ACTIVE)) &
                                    DEALLOCATE(REGIONAL_VARIABLE_ACTIVE, &
                                                 REGIONAL_VARIABLE_NAME, &
                                                 ACTIVE_TRANS_GROUP, &
                                                 SCENARIO_VARIABLE_NAME)
            ALLOCATE(REGIONAL_VARIABLE_ACTIVE(NUM_SCEN_VAR), &
                                   REGIONAL_VARIABLE_NAME(NUM_SCEN_VAR), &
                                   SCENARIO_VARIABLE_NAME(NUM_SCEN_VAR), &
                                  ACTIVE_TRANS_GROUP(UPPER_TRANS_GROUP))
!
            DO TABLE = 1, NUM_SCEN_VAR
               REGIONAL_VARIABLE_ACTIVE(TABLE) = .FALSE.
            ENDDO
!
            DO TG = 1, UPPER_TRANS_GROUP
               ACTIVE_TRANS_GROUP(TG) = .FALSE.
            ENDDO
!
            DO FILE_ID = 0, 4
!
               CALL IS_ACTIVE_BASE_RM_FILES(REG_PARAMS_FILE_EXISTS, &
                                                                FILE_ID)
               IF(.NOT. REG_PARAMS_FILE_EXISTS) CYCLE
               CALL OPEN_REG_OUT_PARAM_FILE(FILE_ID)
!
               CALL GET_ROPF_TABLES(PARAMS_TABLES,FILE_ID)
!
               DO TABLE = 1, PARAMS_TABLES
                  DO YR = 1, AVAIL_DATA_YEARS
                     IREC = (TABLE-1)*AVAIL_DATA_YEARS + YR
                     READ(10,REC=IREC) DELETE, &
                           REGIONAL_YEAR, &
                           RECORD_IS_ACTIVE, &
                           TRANSACTION_GROUP, &
                           REGIONAL_VARIABLE, & !  TRANSACTION GROUP NUMBER
                           TEMP_TIME_FRAME, &
                           ANNUAL_VALUE, &
                           MONTHLY_VALUES
!
                     IF(RECORD_IS_ACTIVE == 'F' .OR. &
                                          RECORD_IS_ACTIVE == 'N') CYCLE
! GAS BASIS REPLACES OTHER 1. 8/26/02. GAT.
!
! 041012. CREATED EXTERNAL FUNCTION
!
                     FUEL_INDEX = &
                          GET_REGIONAL_SCENARIO_INDEX(REGIONAL_VARIABLE)
                     IF(FUEL_INDEX == 9) THEN
                        IS_REGIONAL_HYDRO_PARAM_ACTIVE = .TRUE.
                     ENDIF
!
                     TG = GET_TRANS_GROUP_POSITION(TRANSACTION_GROUP)
                     IF (TG <= 0) CYCLE
!
                     REGIONAL_VARIABLE_ACTIVE(FUEL_INDEX) = .TRUE.
                     REGIONAL_VARIABLE_NAME(FUEL_INDEX) = &
                                                       REGIONAL_VARIABLE
                     SCENARIO_VARIABLE_NAME(FUEL_INDEX) = &
                                                       REGIONAL_VARIABLE
                     ACTIVE_TRANS_GROUP(TG) = .TRUE.
!
                     DO MO = 1, 12
                        IF(TEMP_TIME_FRAME == 'A') THEN
                           SCEN_PARAMS_VARIABLE(FUEL_INDEX,TG,MO,YR) = &
                             SCEN_PARAMS_VARIABLE(FUEL_INDEX,TG,MO,YR) * &
                                                            ANNUAL_VALUE
                        ELSEIF(TEMP_TIME_FRAME == 'M') THEN
                           SCEN_PARAMS_VARIABLE(FUEL_INDEX,TG,MO,YR) = &
                             SCEN_PARAMS_VARIABLE(FUEL_INDEX,TG,MO,YR) * &
                                                      MONTHLY_VALUES(MO)
                        ENDIF
                     ENDDO ! MONTHS
                  ENDDO ! RECORDS (YEARS)
               ENDDO ! TABLES
            ENDDO ! FILES
!
            READ_REGIONAL_PARAMS_DATA = .TRUE.
            CALL CLOSE_REG_OUT_PARAM_FILE

            IF(REGIONAL_MAKER_REPORT) THEN
!
               IF(REGIONAL_MAKER_REPORT_NOT_OPEN) THEN
                  REGIONAL_MAKER_REPORT_NOT_OPEN = .FALSE.
                  REGIONAL_MAKER_UNIT = &
                           REGIONAL_MAKER_RPT_HEADER(REGIONAL_MAKER_REC)
               ENDIF
!
               DO YR = 1, AVAIL_DATA_YEARS
                  THIS_YEAR = YR + get_base_year()
                  DO REGIONAL_INDEX = 1, NUM_SCEN_VAR
!
                     IF(.NOT. &
                         REGIONAL_VARIABLE_ACTIVE(REGIONAL_INDEX)) CYCLE
!
                     DO TG = 1, UPPER_TRANS_GROUP
!

                        IF(.NOT. ACTIVE_TRANS_GROUP(TG)) then
                            CYCLE
                        end if
                        
                        
                        
                        

                        TRANSACTION_GROUP = &
                            GET_TRANS_GROUP_INDEX(TG)





                        AVERAGE_MULTIPLIER = .08333333*( &
                           SCEN_PARAMS_VARIABLE(REGIONAL_INDEX,TG,1,YR)+ &
                           SCEN_PARAMS_VARIABLE(REGIONAL_INDEX,TG,2,YR)+ &
                           SCEN_PARAMS_VARIABLE(REGIONAL_INDEX,TG,3,YR)+ &
                           SCEN_PARAMS_VARIABLE(REGIONAL_INDEX,TG,4,YR)+ &
                           SCEN_PARAMS_VARIABLE(REGIONAL_INDEX,TG,5,YR)+ &
                           SCEN_PARAMS_VARIABLE(REGIONAL_INDEX,TG,6,YR)+ &
                           SCEN_PARAMS_VARIABLE(REGIONAL_INDEX,TG,7,YR)+ &
                           SCEN_PARAMS_VARIABLE(REGIONAL_INDEX,TG,8,YR)+ &
                           SCEN_PARAMS_VARIABLE(REGIONAL_INDEX,TG,9,YR)+ &
                          SCEN_PARAMS_VARIABLE(REGIONAL_INDEX,TG,10,YR)+ &
                          SCEN_PARAMS_VARIABLE(REGIONAL_INDEX,TG,11,YR)+ &
                          SCEN_PARAMS_VARIABLE(REGIONAL_INDEX,TG,12,YR))
                        STDDEV_OF_MULTIPLIER = 2.0
                        STDDEV_OF_MULT_ERROR = 3.0
                        VOLATILITY = 4.0
!
                        WRITE(REGIONAL_MAKER_UNIT, &
                                                 REC=REGIONAL_MAKER_REC) &
                              PRT_ENDPOINT(), &
                              FLOAT(THIS_YEAR), &
                              'Product  ', &
                              SCENARIO_VARIABLE_NAME(REGIONAL_INDEX), &
                              FLOAT(TRANSACTION_GROUP), &
                              (SCEN_PARAMS_VARIABLE( &
                                      REGIONAL_INDEX,TG,MO,YR),MO=1,12), &
                              AVERAGE_MULTIPLIER, &
                              STDDEV_OF_MULTIPLIER, &
                              STDDEV_OF_MULT_ERROR, &
                              VOLATILITY
                        REGIONAL_MAKER_REC = REGIONAL_MAKER_REC + 1
                     ENDDO
                  ENDDO
               ENDDO
!
            ENDIF
         ENDIF
!
         SAVE_REGIONAL_PARAMS_STATUS = READ_REGIONAL_PARAMS_DATA
      RETURN

      ENTRY REGIONAL_PARAMS_ACTIVE()

         REGIONAL_PARAMS_ACTIVE = SAVE_REGIONAL_PARAMS_STATUS
      RETURN

      ENTRY GET_MONTHLY_REGIONAL_PARAM( &
                                       R_YEAR,R_MONTH,R_TG,R_FUEL_INDEX)

!
         TG = GET_TRANS_GROUP_POSITION(R_TG)
!
         IF(SAVE_REGIONAL_PARAMS_STATUS .AND. &
                  R_FUEL_INDEX > 0 .AND. &
                      R_FUEL_INDEX <= MAX_REGIONAL_INDEX .AND. &
                             TG > 0 .AND. TG <= UPPER_TRANS_GROUP ) THEN
!            REGIONAL_INDEX = TG*R_FUEL_INDEX
            GET_MONTHLY_REGIONAL_PARAM = &
                    SCEN_PARAMS_VARIABLE(R_FUEL_INDEX,TG,R_MONTH, &
                                           MIN(R_YEAR,AVAIL_DATA_YEARS))

         ELSE
            GET_MONTHLY_REGIONAL_PARAM = 1.0
         ENDIF
      RETURN

      ENTRY GET_PARAM_ELECTRIC_DEMAND(R_YEAR,R_MONTH,R_TG) ! DONE.

!
         TG = R_TG ! 11/27/02. ALREADY INDEXED. FOUND BY PAC.
!
         IF(SAVE_REGIONAL_PARAMS_STATUS .AND. &
                             TG > 0 .AND. TG <= UPPER_TRANS_GROUP ) THEN
            FUEL_INDEX = 8

            GET_PARAM_ELECTRIC_DEMAND = &
                    SCEN_PARAMS_VARIABLE(FUEL_INDEX,TG,R_MONTH, &
                                           MIN(R_YEAR,AVAIL_DATA_YEARS))
         ELSE
            GET_PARAM_ELECTRIC_DEMAND = 1.0
         ENDIF
      RETURN

      ENTRY GET_PARAM_PEAK(R_YEAR,R_MONTH,R_TG) ! DONE.

!

         TG = R_TG ! 11/27/02. ALREADY INDEXED. FOUND BY PAC.
!
         IF(SAVE_REGIONAL_PARAMS_STATUS .AND. &
                             TG > 0 .AND. TG <= UPPER_TRANS_GROUP ) THEN
            FUEL_INDEX = 6

            GET_PARAM_PEAK = &
                    SCEN_PARAMS_VARIABLE(FUEL_INDEX,TG,R_MONTH, &
                                           MIN(R_YEAR,AVAIL_DATA_YEARS))
         ELSE
            GET_PARAM_PEAK = 1.0
         ENDIF
      RETURN

      ENTRY GET_PARAM_ENERGY(R_YEAR,R_MONTH,R_TG) ! DONE. ADDED 5/15/00.

!

         TG = R_TG ! 11/27/02. ALREADY INDEXED. FOUND BY PAC.
!
         IF(SAVE_REGIONAL_PARAMS_STATUS .AND. &
                             TG > 0 .AND. TG <= UPPER_TRANS_GROUP ) THEN
            FUEL_INDEX = 7

            GET_PARAM_ENERGY = &
                    SCEN_PARAMS_VARIABLE(FUEL_INDEX,TG,R_MONTH, &
                                           MIN(R_YEAR,AVAIL_DATA_YEARS))
         ELSE
            GET_PARAM_ENERGY = 1.0
         ENDIF
      RETURN

      ENTRY REGIONAL_HYDRO_PARAM_ACTIVE()

         REGIONAL_HYDRO_PARAM_ACTIVE = IS_REGIONAL_HYDRO_PARAM_ACTIVE
      RETURN

      ENTRY GET_PARAM_HYDRO(R_YEAR,R_MONTH,R_TG) ! DONE.

!
         TG = GET_TRANS_GROUP_POSITION(R_TG)
!
         IF(SAVE_REGIONAL_PARAMS_STATUS .AND. &
                             TG > 0 .AND. TG <= UPPER_TRANS_GROUP ) THEN
            FUEL_INDEX = 9

            GET_PARAM_HYDRO = &
                    SCEN_PARAMS_VARIABLE(FUEL_INDEX,TG,R_MONTH, &
                                           MIN(R_YEAR,AVAIL_DATA_YEARS))
            IF(GET_PARAM_HYDRO < .99 .OR. GET_PARAM_HYDRO > 1.01) THEN
               TG = TG
            ENDIF
         ELSE
            GET_PARAM_HYDRO = 1.0
         ENDIF
      RETURN

      ENTRY GET_MONTHLY_REGIONAL_OUTAGE( &
                                       R_YEAR,R_MONTH,R_TG,R_FUEL_INDEX)

!
         TG = GET_TRANS_GROUP_POSITION(R_TG)
!
         IF(SAVE_REGIONAL_PARAMS_STATUS .AND. &
                  R_FUEL_INDEX > 0 .AND. &
                      R_FUEL_INDEX <= MAX_REGIONAL_INDEX .AND. &
                             TG > 0 .AND. TG <= UPPER_TRANS_GROUP ) THEN

            GET_MONTHLY_REGIONAL_OUTAGE = &
                   SCEN_PARAMS_VARIABLE(R_FUEL_INDEX,TG,R_MONTH, &
                                           MIN(R_YEAR,AVAIL_DATA_YEARS))

         ELSE
            GET_MONTHLY_REGIONAL_OUTAGE = 1.0
         ENDIF
      RETURN
      END
!

      FUNCTION GET_REGIONAL_SCENARIO_INDEX(REGIONAL_VARIABLE)
      use end_routine
      implicit none
      CHARACTER (LEN=*) :: REGIONAL_VARIABLE
      INTEGER (KIND=2) :: FUEL_INDEX,GET_REGIONAL_SCENARIO_INDEX
      FUEL_INDEX = -99
      SELECT CASE(trim(REGIONAL_VARIABLE))
         CASE ('Coal Availability')
            FUEL_INDEX = 1
         CASE ('Coal')
            FUEL_INDEX = 1
         CASE ('Gas Availability')
            FUEL_INDEX = 2
         CASE ('Gas')
            FUEL_INDEX = 2
         CASE ('Oil Availability')
            FUEL_INDEX = 3
         CASE ('Oil')
            FUEL_INDEX = 3
         CASE ('Nuclear Availability')
            FUEL_INDEX = 4
         CASE ('Nuclear')
            FUEL_INDEX = 4
         CASE ('Other Availability')
            FUEL_INDEX = 5
         CASE ('Other')
            FUEL_INDEX = 5
         CASE ('Peak')
            FUEL_INDEX = 6
         CASE ('Energy')
            FUEL_INDEX = 7
         CASE ('Demand')
            FUEL_INDEX = 8
         CASE ('Hydro Output')
            FUEL_INDEX = 9
!            IS_REGIONAL_HYDRO_PARAM_ACTIVE = .TRUE.
         CASE ('Gas Basis')
            FUEL_INDEX = 10
         CASE ('Other 1')
            FUEL_INDEX = 11
         CASE (' ')
            FUEL_INDEX = 11
         CASE ('Wind Availability')
            FUEL_INDEX = 12
         CASE ('Solar Availability')
            FUEL_INDEX = 13
      END SELECT
      GET_REGIONAL_SCENARIO_INDEX = FUEL_INDEX
      RETURN
      END
! 020107. ADDED AOD VALUATION TYPE SWITCH

!
!          OBJECT TO CONVERT DATA FILES TO DIRECT ACESS BINARY
!         COPYRIGHT (C) 1983, 84, 85, 04 M.S. GERBER & ASSOCIATES, INC.
!

!
      SUBROUTINE LH_GLOBAL_OBJECT
      use end_routine, only: end_program, er_message
!
      use SpinDriftLib
      USE PROD_ARRAYS_DIMENSIONS
      USE LH_GLOBAL_VARIABLES
      USE SIZECOM
      use globecom

      INTEGER (KIND=2) :: IREC,INUNIT,DELETE,LRECL=65
      INTEGER :: UNIT_NO
      INTEGER (KIND=4) :: IOS
      LOGICAL (KIND=4) :: LH_GLOBAL_FILE_EXISTS=.FALSE.
      LOGICAL (KIND=1) :: R_LH_GLOBAL_FILE_ACTIVE
      CHARACTER (LEN=5) :: LH_GLOBAL_FILE,OVERLAY_FAMILY_NAME
      CHARACTER (LEN=8) :: PRB_NUMBER_OF_DIGITS
      CHARACTER (LEN=256) :: FILE_NAME
      CHARACTER (LEN=256) :: BASE_FILE_DIRECTORY,OUTPUT_DIRECTORY
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER (LEN=1024) :: RECLN
! DECLARATION FOR LOCALS
      CHARACTER (LEN=15) :: FILE_TYPE='LH Global    '
      CHARACTER (LEN=2) :: CAPACITY_LH_GLOBAL_OL='BC'
      LOGICAL (KIND=1) :: LAHEY_LF95
      CHARACTER (LEN=30) :: SCREEN_OUTPUT
!
      CHARACTER (LEN=1) :: USE_ANNUAL_PARAMETERS,RECORD_IS_ACTIVE      ! CHAR*1
      CHARACTER (LEN=1) :: STOCHASTIC_OVERLAYS,HOURLY_MARKET_PRICES    ! CHAR*1
      CHARACTER (LEN=1) :: MID_TERM_PEAK_UNCER,AOD_VALUATION_TYPE
      CHARACTER (LEN=1) :: AOD_UNIT_REPORTING
      INTEGER (KIND=2) :: STATES_PER_VARIABLE    ! INT*2
      INTEGER (KIND=4) :: RANDOM_NUMBER_SEED      ! INT*4
      CHARACTER (LEN=9) :: INTERCONNECTION,TIME_ZONE               ! CHAR*9
      INTEGER (KIND=2) ::  PRB_SEQUENCE_NUMBER=1
!

      ENTRY LH_GLOBAL_MAKEBIN

!
      FILE_NAME = trim(BASE_FILE_DIRECTORY())// &
                                "SGB"//trim(LH_GLOBAL_FILE())//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=LH_GLOBAL_FILE_EXISTS)
      IF(LH_GLOBAL_FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//LH_GLOBAL_FILE()
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
            CALL MG_LOCATE_WRITE(16,30,LH_GLOBAL_FILE(),ALL_VERSIONS,0)
         ENDIF
!
! INITILIZE REGIONAL PLANNING PARAMETERS PER DOUG
!
! INITIALIZE VALUES
!
         MID_TERM_PEAK_UNCER = 'I'
         AOD_VALUATION_TYPE = 'C'
         AOD_UNIT_REPORTING = 'A'
         PRB_SEQUENCE_NUMBER = 1
         PRB_NUMBER_OF_DIGITS = '2-Digit' ! OLD FORMAT
!
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCLH_GLOBAL.BIN", &
                            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         OPEN(10,FILE=FILE_NAME) ! SGB
         IREC = 1
         READ(10,*) DELETE
         DO
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(IOS /=0) EXIT
            RECLN = trim(RECLN)// &
                              ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
!
            IF(RECLN(1:1) == '7' .OR. IOS /= 0) CYCLE
!
!
            READ(RECLN,*,ERR=300)   DELETE, &
                                    RECORD_IS_ACTIVE,       & !  CHAR*1
                                    STATES_PER_VARIABLE,    & !  INT*2
                                    RANDOM_NUMBER_SEED,     & !  INT*4
                                    USE_ANNUAL_PARAMETERS,  & !  CHAR*1
                                    INTERCONNECTION,        & !  CHAR*9
                                    TIME_ZONE,              & !  CHAR*9
                                    STOCHASTIC_OVERLAYS,    & !  CHAR*1
                                    HOURLY_MARKET_PRICES,   & !  CHAR*1
                                    MID_TERM_PEAK_UNCER, &
                                    PRB_SEQUENCE_NUMBER, &
                                    PRB_NUMBER_OF_DIGITS, &
                                    SAVE_SCENARIO_MRX_PLANS, &
                                    MRX_EXPANSION_PLAN_FILE_CODE, &
                                    MRX_EXPANSION_PLAN_FILE_NAME, &
                                    MRX_SEQUENCE_NUMBER, &
                                    AOD_VALUATION_TYPE, &
                                    AOD_UNIT_REPORTING
            WRITE(11,REC=IREC) &
                                    RECORD_IS_ACTIVE,       & !  CHAR*1
                                    STATES_PER_VARIABLE,    & !  INT*2
                                    RANDOM_NUMBER_SEED,     & !  INT*4
                                    USE_ANNUAL_PARAMETERS,  & !  CHAR*1
                                    INTERCONNECTION,        & !  CHAR*9
                                    TIME_ZONE,              & !  CHAR*9
                                    STOCHASTIC_OVERLAYS,    & !  CHAR*1
                                    HOURLY_MARKET_PRICES,   & !  CHAR*1
                                    MID_TERM_PEAK_UNCER, &
                                    PRB_SEQUENCE_NUMBER, &
                                    PRB_NUMBER_OF_DIGITS, &
                                    SAVE_SCENARIO_MRX_PLANS, &
                                    MRX_EXPANSION_PLAN_FILE_CODE, &
                                    MRX_EXPANSION_PLAN_FILE_NAME, &
                                    MRX_SEQUENCE_NUMBER, &
                                    AOD_VALUATION_TYPE, &
                                    AOD_UNIT_REPORTING
                 IREC = IREC + 1
!            IF(IREC .GT. LRECL) EXIT
         ENDDO
         CLOSE(10)
!        ENDFILE(11)
         CLOSE(11)
      ELSE IF(INDEX(LH_GLOBAL_FILE(),'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "wh_objt:1112") 
      ENDIF
      RETURN

! OVERLAY THE TARGET CAPACITY-LH_GLOBAL FILE
      ENTRY LH_GLOBAL_MAKEOVL(OVERLAY_FAMILY_NAME)

!
      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      FILE_NAME = trim(OUTPUT_DIRECTORY())//"SGO"// &
                                     trim(OVERLAY_FAMILY_NAME)//".DAT"
      OPEN(10,FILE=FILE_NAME) ! SGO
      READ(10,*) DELETE
      INUNIT = 12
      IF(CAPACITY_LH_GLOBAL_OL == 'BC') THEN
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCLH_GLOBAL.BIN", &
                                             ACCESS="DIRECT",RECL=LRECL)
         INUNIT = 11
      ENDIF
      OPEN(12,FILE=trim(OUTPUT_DIRECTORY())//"OLLH_GLOBAL.BIN", &
                            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      IREC = 0
      DO
         IREC = IREC + 1
         READ(INUNIT,REC=IREC,IOSTAT=IOS) &
                                    RECORD_IS_ACTIVE,       & !  CHAR*1
                                    STATES_PER_VARIABLE,    & !  INT*2
                                    RANDOM_NUMBER_SEED,     & !  INT*4
                                    USE_ANNUAL_PARAMETERS,  & !  CHAR*1
                                    INTERCONNECTION,        & !  CHAR*9
                                    TIME_ZONE,              & !  CHAR*9
                                    STOCHASTIC_OVERLAYS,    & !  CHAR*1
                                    HOURLY_MARKET_PRICES,   & !  CHAR*1
                                    MID_TERM_PEAK_UNCER, &
                                    PRB_SEQUENCE_NUMBER, &
                                    PRB_NUMBER_OF_DIGITS, &
                                    SAVE_SCENARIO_MRX_PLANS, &
                                    MRX_EXPANSION_PLAN_FILE_CODE, &
                                    MRX_EXPANSION_PLAN_FILE_NAME, &
                                    MRX_SEQUENCE_NUMBER, &
                                    AOD_VALUATION_TYPE, &
                                    AOD_UNIT_REPORTING
         IF(IOS /= 0) EXIT
         DO
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(RECLN(1:1) == '7') CYCLE
            EXIT
         ENDDO
         IF(IOS == 0) THEN
            RECLN = trim(RECLN)//',,,,,,,,,,,,,'
            READ(RECLN,*,ERR=300)   DELETE, &
                                    RECORD_IS_ACTIVE,       & !  CHAR*1
                                    STATES_PER_VARIABLE,    & !  INT*2
                                    RANDOM_NUMBER_SEED,     & !  INT*4
                                    USE_ANNUAL_PARAMETERS,  & !  CHAR*1
                                    INTERCONNECTION,        & !  CHAR*9
                                    TIME_ZONE,              & !  CHAR*9
                                    STOCHASTIC_OVERLAYS,    & !  CHAR*1
                                    HOURLY_MARKET_PRICES,   & !  CHAR*1
                                    MID_TERM_PEAK_UNCER, &
                                    PRB_SEQUENCE_NUMBER, &
                                    PRB_NUMBER_OF_DIGITS, &
                                    SAVE_SCENARIO_MRX_PLANS, &
                                    MRX_EXPANSION_PLAN_FILE_CODE, &
                                    MRX_EXPANSION_PLAN_FILE_NAME, &
                                    MRX_SEQUENCE_NUMBER, &
                                    AOD_VALUATION_TYPE, &
                                    AOD_UNIT_REPORTING
         ENDIF
         WRITE(12,REC=IREC) &
                                    RECORD_IS_ACTIVE,       & !  CHAR*1
                                    STATES_PER_VARIABLE,    & !  INT*2
                                    RANDOM_NUMBER_SEED,     & !  INT*4
                                    USE_ANNUAL_PARAMETERS,  & !  CHAR*1
                                    INTERCONNECTION,        & !  CHAR*9
                                    TIME_ZONE,              & !  CHAR*9
                                    STOCHASTIC_OVERLAYS,    & !  CHAR*1
                                    HOURLY_MARKET_PRICES,   & !  CHAR*1
                                    MID_TERM_PEAK_UNCER, &
                                    PRB_SEQUENCE_NUMBER, &
                                    PRB_NUMBER_OF_DIGITS, &
                                    SAVE_SCENARIO_MRX_PLANS, &
                                    MRX_EXPANSION_PLAN_FILE_CODE, &
                                    MRX_EXPANSION_PLAN_FILE_NAME, &
                                    MRX_SEQUENCE_NUMBER, &
                                    AOD_VALUATION_TYPE, &
                                    AOD_UNIT_REPORTING
      ENDDO
      CLOSE(10)
      CLOSE(12)
      IF(CAPACITY_LH_GLOBAL_OL == 'BC') CLOSE(11)
      CAPACITY_LH_GLOBAL_OL = 'OL'
      RETURN

  300 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from wh_objt SIID370'
      call end_program(er_message)
!

      ENTRY RESET_LH_GLOBAL

         CAPACITY_LH_GLOBAL_OL = 'BC'
      RETURN
!

      ENTRY OPEN_CAPACITY_LH_GLOBAL_FILE(UNIT_NO, &
                                                R_LH_GLOBAL_FILE_ACTIVE)

         IF(LH_GLOBAL_FILE_EXISTS) THEN
            OPEN(UNIT_NO,FILE=trim(OUTPUT_DIRECTORY())// &
                                 CAPACITY_LH_GLOBAL_OL//"LH_GLOBAL.BIN", &
                                             ACCESS="DIRECT",RECL=LRECL)
         ELSE
            WRITE(4,'(1X,3A,I4,A)') '** WARNING ** Capacity LH_GLOBAL ', &
            'file did not exist.  Expansion planning was not done for ', &
            'end point',gc_end_point,'.'
         ENDIF
         R_LH_GLOBAL_FILE_ACTIVE = LH_GLOBAL_FILE_EXISTS
      RETURN
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END

!
! LH_GLOBAL FUNCTIONS
!

      FUNCTION READ_LH_GLOBAL_FILE()
      use end_routine, only: end_program, er_message
      use SpinDriftLib
      USE PROD_ARRAYS_DIMENSIONS
      USE LH_GLOBAL_VARIABLES
      use mod_base_year
      USE SIZECOM
      use globecom
      SAVE
!
      LOGICAL (KIND=1) :: READ_LH_GLOBAL_FILE,LH_GLOBAL_FILE_ACTIVE
      LOGICAL (KIND=1) :: RECORD_BASED_STOCHASTIC_OLS
      LOGICAL (KIND=1) :: YES_MID_TERM_PEAK_UNCER,YES_AOD_VALUATION_TYPE
      LOGICAL (KIND=1) :: YES_TWO_PRB_DIGITS,YES_AOD_UNIT_REPORTING
      INTEGER (KIND=2) :: GET_PRB_SEQUENCE_NUMBER
      CHARACTER (LEN=8) :: PRB_NUMBER_OF_DIGITS
      INTEGER (KIND=2) :: K,R_YEAR,GET_MARKET_PRICE_YEAR
      INTEGER (KIND=4) :: IOS
!
      CHARACTER (LEN=1) :: USE_ANNUAL_PARAMETERS,RECORD_IS_ACTIVE       ! CHAR*1
      CHARACTER (LEN=1) :: STOCHASTIC_OVERLAYS='R'    ! CHAR*1
      CHARACTER (LEN=1) :: HOURLY_MARKET_PRICES='C'   ! CHAR*1
      CHARACTER (LEN=1) :: MID_TERM_PEAK_UNCER,AOD_VALUATION_TYPE='C'
      CHARACTER (LEN=1) :: AOD_UNIT_REPORTING='A'
      INTEGER (KIND=2) :: STATES_PER_VARIABLE    ! INT*2
      INTEGER (KIND=4) :: RANDOM_NUMBER_SEED      ! INT*4
      CHARACTER (LEN=9) :: INTERCONNECTION,TIME_ZONE               ! CHAR*9
      INTEGER (KIND=2) ::  PRB_SEQUENCE_NUMBER=1
!
! END DATA DECLARATIONS
!
!
         CALL OPEN_CAPACITY_LH_GLOBAL_FILE(10,LH_GLOBAL_FILE_ACTIVE)
         READ_LH_GLOBAL_FILE = .FALSE.
         IF(LH_GLOBAL_FILE_ACTIVE) THEN
!
            K = 1
!            DO
               READ(10,REC=K,IOSTAT=IOS) &
                                    RECORD_IS_ACTIVE,       & !  CHAR*1
                                    STATES_PER_VARIABLE,    & !  INT*2
                                    RANDOM_NUMBER_SEED,     & !  INT*4
                                    USE_ANNUAL_PARAMETERS,  & !  CHAR*1
                                    INTERCONNECTION,        & !  CHAR*9
                                    TIME_ZONE,              & !  CHAR*9
                                    STOCHASTIC_OVERLAYS,    & !  CHAR*1
                                    HOURLY_MARKET_PRICES,   & !  CHAR*1
                                    MID_TERM_PEAK_UNCER, &
                                    PRB_SEQUENCE_NUMBER, &
                                    PRB_NUMBER_OF_DIGITS, &
                                    SAVE_SCENARIO_MRX_PLANS, &
                                    MRX_EXPANSION_PLAN_FILE_CODE, &
                                    MRX_EXPANSION_PLAN_FILE_NAME, &
                                    MRX_SEQUENCE_NUMBER, &
                                    AOD_VALUATION_TYPE, &
                                    AOD_UNIT_REPORTING

!               IF(IOS /= 0) EXIT
               IF(IOS /= 0) THEN
                  WRITE(4,*) 'PROBLEM READING SCENARIO SWITCHES FILE'
                  er_message='Stop requested from wh_objt SIID372'
                  call end_program(er_message)
               ENDIF
               K = K + 1
               READ_LH_GLOBAL_FILE = .TRUE.
!            ENDDO
!
            CLOSE(10)
         ENDIF
      RETURN

      ENTRY YES_TWO_PRB_DIGITS

         IF(LH_GLOBAL_FILE_ACTIVE) THEN
            YES_TWO_PRB_DIGITS = PRB_NUMBER_OF_DIGITS == '2-Digit '
         ELSE
            YES_TWO_PRB_DIGITS = .TRUE.
         ENDIF
      RETURN

      ENTRY GET_PRB_SEQUENCE_NUMBER

         IF(LH_GLOBAL_FILE_ACTIVE) THEN
            GET_PRB_SEQUENCE_NUMBER = PRB_SEQUENCE_NUMBER
         ELSE
            GET_PRB_SEQUENCE_NUMBER = 1
         ENDIF
      RETURN

      ENTRY GET_MARKET_PRICE_YEAR(R_YEAR)

         IF(HOURLY_MARKET_PRICES == 'C' .AND. &
                                       .NOT. LH_GLOBAL_FILE_ACTIVE) THEN
            GET_MARKET_PRICE_YEAR = MIN(R_YEAR,AVAIL_DATA_YEARS)
         ELSEIF(HOURLY_MARKET_PRICES == 'Y') THEN ! ASSUME YEAR-BASED
            GET_MARKET_PRICE_YEAR=get_base_year()+MIN(R_YEAR,AVAIL_DATA_YEARS)
            IF(get_base_year()+R_YEAR > 1999) THEN
               GET_MARKET_PRICE_YEAR = GET_MARKET_PRICE_YEAR-2000
            ELSE
               GET_MARKET_PRICE_YEAR = GET_MARKET_PRICE_YEAR-1900
            ENDIF
         ELSE ! ASSUME CHRONO WITH FIRST SIMULATION YEAR = 1
            GET_MARKET_PRICE_YEAR = MIN(R_YEAR,AVAIL_DATA_YEARS)
         ENDIF
      RETURN

      ENTRY RECORD_BASED_STOCHASTIC_OLS()

         RECORD_BASED_STOCHASTIC_OLS =  STOCHASTIC_OVERLAYS == 'R'
      RETURN

      ENTRY YES_MID_TERM_PEAK_UNCER()

         YES_MID_TERM_PEAK_UNCER =  MID_TERM_PEAK_UNCER /= 'E'
      RETURN

      ENTRY YES_AOD_VALUATION_TYPE

         YES_AOD_VALUATION_TYPE =  AOD_VALUATION_TYPE == 'C'

      RETURN

      ENTRY YES_AOD_UNIT_REPORTING

         YES_AOD_UNIT_REPORTING = AOD_UNIT_REPORTING == 'L'
      RETURN
      END

!
      SUBROUTINE FIND_PRICE_AND_QUANT(R_HR, &
                                      R_MONTH, &
                                      R_TEMP_TRANSACTION, &
                                      R_TEMP_INTERP_TRANSACTION, &
                                      R_TRANSACTION_COMPLETED, &
                                      R_BUYER_DB,R_SELLER_DB, &
                                      R_BUYERS_COST, &
                                      R_SELLERS_COST, &
                                      R_CONVERGENCE_PATH, &
                                      R_SELLERS_LOAD, &
                                      R_BUYERS_LOAD, &
                                      R_SELLER_SCARCITY_COST, &
                                      R_BUYER_SCARCITY_COST, &
                                      R_SELLER_SCARCITY_MULT, &
                                      R_BUYER_SCARCITY_MULT,R_L_M, &
                                      R_TIE_PLUS_MARKET_SCARCITY, &
                                      R_TOTAL_DELIVERED_COST, &
                                      R_SEARCHES_WITHIN_TRANSACTIONS, &
                                      R_TWH, &
                                      R_HOURLY_DUMP_USED)
      use end_routine, only: end_program, er_message
!

!
! TWO EQUATIONS, TWO UNKNOWNS

!
      LOGICAL (KIND=1) :: SEARCH_SUCCESSFUL,R_TRANSACTION_COMPLETED
      INTEGER (KIND=2) :: R_HR,R_MONTH,R_BUYER_DB,R_SELLER_DB,R_L_M
      INTEGER (KIND=2) :: R_TWH
      INTEGER (KIND=4) :: R_SEARCHES_WITHIN_TRANSACTIONS
      INTEGER (KIND=4) :: MAX_SEARCHES_WITHIN_TRANS=50


      REAL (KIND=4) :: R_BUYERS_COST,R_SELLERS_COST,R_TEMP_TRANSACTION
      REAL (KIND=4) :: R_CONVERGENCE_PATH,R_SELLER_SCARCITY_COST
      REAL (KIND=4) :: R_BUYER_SCARCITY_COST,R_SELLER_SCARCITY_MULT
      REAL (KIND=4) :: R_BUYER_SCARCITY_MULT,R_TIE_PLUS_MARKET_SCARCITY
      REAL (KIND=4) :: R_TOTAL_DELIVERED_COST,R_HOURLY_DUMP_USED
      REAL (KIND=8) :: R_TEMP_INTERP_TRANSACTION,DENOM
      REAL (KIND=8) :: GET_MARGINAL_COST_AT_MARKET,r8_X_TEMP,r8_Y_TEMP
      REAL (KIND=8) :: SELLER_SLOPE,SELLER_INTERCEPT
      REAL (KIND=8) :: BUYER_SLOPE,BUYER_INTERCEPT
      REAL (KIND=8) :: R_SELLERS_LOAD,R_BUYERS_LOAD
      REAL (KIND=8) :: SELLER_PRICE_AT_MAX_TRANS
      REAL (KIND=8) :: SELLER_PRICE_AT_MIN_TRANS
      REAL (KIND=8) :: BUYER_PRICE_AT_MAX_TRANS
      REAL (KIND=8) :: BUYER_PRICE_AT_MIN_TRANS
      REAL (KIND=8) :: SELLER_PRICE_AT_TEST_TRANS
      REAL (KIND=8) :: BUYER_PRICE_AT_TEST_TRANS
      REAL (KIND=8) :: QUANT_AT_MAX_TRANS,QUANT_AT_MIN_TRANS
!
! END DATA DECLARATIONS
!
         IF(R_TRANSACTION_COMPLETED) RETURN
!
         QUANT_AT_MAX_TRANS = R_TEMP_INTERP_TRANSACTION
         QUANT_AT_MIN_TRANS = 0.
!
         R_SEARCHES_WITHIN_TRANSACTIONS = 0D0
!
         SEARCH_SUCCESSFUL = .FALSE.
!
         DO WHILE(R_BUYER_DB > 0 .AND. .NOT. SEARCH_SUCCESSFUL .AND. &
                        R_SEARCHES_WITHIN_TRANSACTIONS < &
                                              MAX_SEARCHES_WITHIN_TRANS)
!
!
            SELLER_PRICE_AT_MAX_TRANS = &
                           GET_MARGINAL_COST_AT_MARKET( &
                                  R_SELLERS_LOAD+QUANT_AT_MAX_TRANS, &
                                    R_SELLER_DB, &
                                R_SELLER_SCARCITY_COST,R_L_M, &
                                R_SELLER_SCARCITY_MULT) + &
                                          R_TIE_PLUS_MARKET_SCARCITY
            SELLER_PRICE_AT_MIN_TRANS = &
                           GET_MARGINAL_COST_AT_MARKET( &
                                  R_SELLERS_LOAD+QUANT_AT_MIN_TRANS, &
                                    R_SELLER_DB, &
                                R_SELLER_SCARCITY_COST,R_L_M, &
                                R_SELLER_SCARCITY_MULT) + &
                                          R_TIE_PLUS_MARKET_SCARCITY
            BUYER_PRICE_AT_MAX_TRANS = &
                              GET_MARGINAL_COST_AT_MARKET( &
                              R_BUYERS_LOAD-QUANT_AT_MAX_TRANS, &
                              R_BUYER_DB, &
                              R_BUYER_SCARCITY_COST,R_L_M, &
                              R_BUYER_SCARCITY_MULT)
            BUYER_PRICE_AT_MIN_TRANS = &
                              GET_MARGINAL_COST_AT_MARKET( &
                              R_BUYERS_LOAD-QUANT_AT_MIN_TRANS, &
                              R_BUYER_DB, &
                              R_BUYER_SCARCITY_COST,R_L_M, &
                              R_BUYER_SCARCITY_MULT)
!
            DENOM = QUANT_AT_MAX_TRANS - QUANT_AT_MIN_TRANS
!
            IF(DENOM == 0.d0) THEN
               WRITE(4,*)'*** line 6351 TRANSOBJ.FOR ***'
               WRITE(4,*) &
                                        "IRREGULAR COST CURVES DETECTED"
               er_message='See WARNING MESSAGES -wh_objt.for-2'
               call end_program(er_message)
            ENDIF
!
            SELLER_SLOPE = &
                              (SELLER_PRICE_AT_MAX_TRANS - &
                                      SELLER_PRICE_AT_MIN_TRANS) / DENOM ! unused
            BUYER_SLOPE = &
                              (BUYER_PRICE_AT_MAX_TRANS - &
                                       BUYER_PRICE_AT_MIN_TRANS) / DENOM ! unused
!
            SELLER_INTERCEPT = SELLER_PRICE_AT_MIN_TRANS
                        BUYER_INTERCEPT = BUYER_PRICE_AT_MIN_TRANS
!
            IF(SELLER_SLOPE == BUYER_SLOPE) THEN
               WRITE(4,*) "Slopes identical",SELLER_SLOPE
                           SELLER_SLOPE = BUYER_SLOPE + .1d0
            ENDIF
!
            r8_X_TEMP = &
                              (BUYER_INTERCEPT - SELLER_INTERCEPT)/ &
                                            (SELLER_SLOPE - BUYER_SLOPE)
!
            r8_Y_TEMP = SELLER_INTERCEPT + SELLER_SLOPE * &
                                                               r8_X_TEMP
!
!  EVERYTHING ABOVE WORKS OFF OF DENOM, OR THE ORIGIN OF THE INTERSECTION
!  AT THE QUANT_AT_MIN_TRANS.
!
!                                   Previous min value
            r8_X_TEMP = r8_X_TEMP + QUANT_AT_MIN_TRANS
!
            R_SEARCHES_WITHIN_TRANSACTIONS = &
                                      R_SEARCHES_WITHIN_TRANSACTIONS + 1
!
            IF(R_SEARCHES_WITHIN_TRANSACTIONS >= &
                                  MAX_SEARCHES_WITHIN_TRANS) THEN
!               R_TRANSACTION_COMPLETED = .TRUE.
               IF(R_TEMP_TRANSACTION > 0. .AND. &
                                    R_SELLERS_COST < R_BUYERS_COST) THEN
!
                  SEARCH_SUCCESSFUL = .TRUE.
                  R_CONVERGENCE_PATH = 35.
               ENDIF
            ELSEIF(r8_Y_TEMP > &
                                  SELLER_PRICE_AT_MIN_TRANS .AND. &
                                       SELLER_PRICE_AT_MAX_TRANS > &
                                          BUYER_PRICE_AT_MAX_TRANS) THEN
!
               SELLER_PRICE_AT_TEST_TRANS = &
                           GET_MARGINAL_COST_AT_MARKET( &
                                 R_SELLERS_LOAD+r8_X_TEMP, &
                                 R_SELLER_DB, &
                                 R_SELLER_SCARCITY_COST,R_L_M, &
                                 R_SELLER_SCARCITY_MULT) + &
                                          R_TIE_PLUS_MARKET_SCARCITY
               BUYER_PRICE_AT_TEST_TRANS = &
                           GET_MARGINAL_COST_AT_MARKET( &
                                 R_BUYERS_LOAD-r8_X_TEMP, &
                                 R_BUYER_DB, &
                                 R_BUYER_SCARCITY_COST,R_L_M, &
                                 R_BUYER_SCARCITY_MULT)
!
               IF(ABS(BUYER_PRICE_AT_TEST_TRANS - &
                                 SELLER_PRICE_AT_TEST_TRANS) < .05 .AND. &
                            BUYER_PRICE_AT_TEST_TRANS - &
                            SELLER_PRICE_AT_TEST_TRANS > -.005) THEN
                  R_BUYERS_COST = BUYER_PRICE_AT_TEST_TRANS
                  R_SELLERS_COST = &
                                    SELLER_PRICE_AT_TEST_TRANS - &
                                          R_TIE_PLUS_MARKET_SCARCITY
                  R_TEMP_TRANSACTION = r8_X_TEMP

                  SEARCH_SUCCESSFUL = .TRUE.
                  R_CONVERGENCE_PATH = 32.
               ELSEIF(BUYER_PRICE_AT_TEST_TRANS > &
                                        SELLER_PRICE_AT_TEST_TRANS) THEN
! MAKE X THE LOWER BOUND AND RETEST
                  QUANT_AT_MIN_TRANS = r8_X_TEMP
! FEASIBLE SOLUTION ADDRESSES PAC ISSUE: BUYER COST < DELIVERED COST
                  R_BUYERS_COST = BUYER_PRICE_AT_TEST_TRANS
                  R_SELLERS_COST = &
                                    SELLER_PRICE_AT_TEST_TRANS - &
                                          R_TIE_PLUS_MARKET_SCARCITY
                  R_TEMP_TRANSACTION = r8_X_TEMP
               ELSEIF(BUYER_PRICE_AT_TEST_TRANS < &
                                        SELLER_PRICE_AT_TEST_TRANS) THEN
! MAKE X THE UPPER BOUND AND RETEST
                  QUANT_AT_MAX_TRANS = r8_X_TEMP
               ENDIF
            ELSEIF( SELLER_PRICE_AT_MAX_TRANS - &
                                  BUYER_PRICE_AT_MAX_TRANS < 0.010) THEN
               R_TEMP_TRANSACTION = QUANT_AT_MAX_TRANS
               R_BUYERS_COST = BUYER_PRICE_AT_MAX_TRANS
               R_SELLERS_COST = SELLER_PRICE_AT_MAX_TRANS - &
                                          R_TIE_PLUS_MARKET_SCARCITY

               SEARCH_SUCCESSFUL = .TRUE.
               R_CONVERGENCE_PATH = 33.
            ELSEIF(r8_Y_TEMP <= &
                                         SELLER_PRICE_AT_MIN_TRANS) THEN
               R_BUYERS_COST = BUYER_PRICE_AT_MIN_TRANS
               R_SELLERS_COST = SELLER_PRICE_AT_MIN_TRANS - &
                                          R_TIE_PLUS_MARKET_SCARCITY
               R_TEMP_TRANSACTION = QUANT_AT_MIN_TRANS

               SEARCH_SUCCESSFUL = .TRUE.
               R_CONVERGENCE_PATH = 34.
            ENDIF
         ENDDO
!
         IF(SEARCH_SUCCESSFUL) THEN
!
            IF(R_CONVERGENCE_PATH /= 33) THEN

               R_BUYERS_COST = BUYER_PRICE_AT_MIN_TRANS
               R_SELLERS_COST = SELLER_PRICE_AT_MIN_TRANS - &
                                          R_TIE_PLUS_MARKET_SCARCITY

            ENDIF
!
            R_TOTAL_DELIVERED_COST = &
                                       R_SELLERS_COST + &
                                            R_TIE_PLUS_MARKET_SCARCITY
            IF( R_TOTAL_DELIVERED_COST - &
                                                R_BUYERS_COST > 0.2)THEN
               WRITE(4,*) 'DELIVERED COST EXCEEDS BUYERS'
               WRITE(4,*) 'INTERAL COST. MONTH= ', &
                                             R_MONTH,' HOUR= ',R_HR

               SEARCH_SUCCESSFUL = .FALSE.
               R_SEARCHES_WITHIN_TRANSACTIONS = 0
            ENDIF
            R_TRANSACTION_COMPLETED = SEARCH_SUCCESSFUL

         ELSE
            R_SEARCHES_WITHIN_TRANSACTIONS = 0
         ENDIF
!
      RETURN
      END
! 013107. FOR AOD.
! CALLED FROM TOP OF CL_UNITS_READ

      FUNCTION AOD_THERMAL_UNIT_VALUE_LIST(R_COMP_LIST)
      use end_routine
      implicit none
         SAVE
         CHARACTER (LEN=1) :: R_REPORT_THIS_UNIT
         LOGICAL (KIND=1) :: AOD_THERMAL_UNIT_VALUE_LIST
         LOGICAL (KIND=1) :: AOD_THERMAL_LIST_COMPARE
         LOGICAL (KIND=1) :: YES_AOD_VALUATION_TYPE,R_COMP_LIST
         LOGICAL (KIND=1) :: VALUATION_TYPE_COMP
         LOGICAL (KIND=1) :: AOD_UNIT_REPORTING,YES_AOD_UNIT_REPORTING
         LOGICAL (KIND=4) :: LIST_FILE_EXISTS
         INTEGER (KIND=2) :: I,ID_LIST_INDEX(10000),NO_OF_IDS
         INTEGER (KIND=4) :: IOS
         INTEGER (KIND=8) :: R_IN_ID_NO,ID_NO_IN_LIST(10000),TEMP_ID
         CHARACTER (LEN=256) :: FILE_NAME,OUTPUT_DIRECTORY
         CHARACTER (LEN=1024) :: RECLN,A
!
         AOD_THERMAL_UNIT_VALUE_LIST = .FALSE.
         R_COMP_LIST = .FALSE.
!
         VALUATION_TYPE_COMP = YES_AOD_VALUATION_TYPE()
         AOD_UNIT_REPORTING = YES_AOD_UNIT_REPORTING()
!
         I = 0
         NO_OF_IDS = 0
         ID_NO_IN_LIST = 0
         ID_LIST_INDEX = 0
         FILE_NAME = trim(OUTPUT_DIRECTORY())//"IDSOFINT.DAT"
         INQUIRE(FILE=FILE_NAME,EXIST=LIST_FILE_EXISTS)
         IF(LIST_FILE_EXISTS) THEN
            OPEN(11,FILE=FILE_NAME)
            DO
               I = I + 1
               READ(11,1000,IOSTAT=IOS) RECLN
               IF(IOS /= 0) EXIT
               READ(RECLN,*) TEMP_ID
               ID_NO_IN_LIST(I) = TEMP_ID
               IF(VALUATION_TYPE_COMP) THEN
                  IF(AOD_UNIT_REPORTING) THEN
                     R_COMP_LIST = .TRUE.
                  ENDIF
               ELSE
                  AOD_THERMAL_UNIT_VALUE_LIST = .TRUE.
               ENDIF
            END DO
            CLOSE(11)
            NO_OF_IDS = I - 1

         ENDIF
         RETURN
 1000 FORMAT(A)
 1010 FORMAT(I12,A)
 ! CALLED FROM THE UNITS READ LOOP. CYCLE ON UNIT IF NOT TRUE.

         ENTRY AOD_THERMAL_LIST_COMPARE(R_IN_ID_NO)

            AOD_THERMAL_LIST_COMPARE = .FALSE.
            DO I = 1, NO_OF_IDS
               IF(ID_NO_IN_LIST(I) /= R_IN_ID_NO) CYCLE
               AOD_THERMAL_LIST_COMPARE = .TRUE.
               EXIT
            END DO
         RETURN
      END

!
!        PROGRAM TO READ MULTI-TAB INFORMATION ON RPS
!                 AND CONVERT TO BINARY FORMAT
!                       COPYRIGHT (C) 2009
!                  ALL RIGHTS RESERVED VENTYX
!

!
      SUBROUTINE RPS_REQUIREMENTS_OBJECT
      use end_routine, only: end_program, er_message
      use SpinDriftLib
      USE PROD_ARRAYS_DIMENSIONS
      USE SIZECOM
      USE iostatmsg
      use mod_base_year
      implicit none
      
      INTEGER (KIND=1) :: FORECAST_GROWTH_YEARS=AVAIL_DATA_YEARS
      LOGICAL (KIND=1) :: SAVE_RR_FILE_EXISTS=.FALSE.,R_RR_FILE_EXISTS
      LOGICAL (KIND=1) :: R_RR_FILE_USED,SAVE_RR_FILE_USED=.FALSE.
      CHARACTER (LEN=20) :: RPS_REQUIREMENT_TYPE
      CHARACTER (LEN=1) :: EXTENSION_PERIOD_GROWTH_SWITCH='X'
      CHARACTER (LEN=1) :: TABLE_ACTIVE
      INTEGER (KIND=2) :: INUNIT,IREC,DELETE,LRECL=270, R_LRECL
      INTEGER (KIND=2) :: SAVE_TRANS_LOAD_TABLES=0,R_TRANS_LOAD_TABLES
      INTEGER (KIND=2) :: TEMP_YEAR
      INTEGER (KIND=4) :: IOS
      CHARACTER (LEN=5) :: RPS_REQUIREMENTS_FILE,OVERLAY_FAMILY_NAME
      CHARACTER (LEN=5) :: BSYRLOAD,REFERENCE_LOAD_NAME
      CHARACTER (LEN=256) :: FILE_NAME
      CHARACTER (LEN=256) :: OUTPUT_DIRECTORY
      CHARACTER (LEN=256) :: BASE_FILE_DIRECTORY
      CHARACTER (LEN=152) :: MESSAGE
      LOGICAL (KIND=4) :: FILE_EXISTS
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER (LEN=1024) :: RECLN
! DECLARATION FOR SYSTEM FORECAST VARIABLES
      INTEGER (KIND=2) :: YEAR,UNIT_NUM=10
!
! SIMULATION VARIABLES
!
      REAL (KIND=4) :: RPS_STATE_PROVINCE(56) ! ASSUME MAXIMUM RPS GROUPS = 56
      CHARACTER (LEN=17) :: FILE_TYPE='RPS Requirements '
      CHARACTER (LEN=2) :: RPS_REQUIREMENTS_OL='BC'
      CHARACTER (LEN=2) :: R_RPS_REQUIREMENTS_OL
      LOGICAL (KIND=1) :: LAHEY_LF95
      CHARACTER (LEN=30) :: SCREEN_OUTPUT
!
      INTEGER (KIND=2) :: RR_YEAR,STUDY_BASE_YEAR
!
      SAVE STUDY_BASE_YEAR
!

!
!          ROUTINE TO CONVERT METAFILE FILES TO DIRECT-ACESS BINARY
!          COPYRIGHT (C) 1983, 84, 85  M.S. GERBER & ASSOCIATES, INC.
!

!
! CONVERT THE SYSTEM-FORECAST FILE
!
!

      ENTRY RPS_REQUIREMENTS_MAKEBIN

      STUDY_BASE_YEAR = get_base_year()
      RR_YEAR = 0
      FILE_NAME = trim(BASE_FILE_DIRECTORY())// &
                          "RRB"//trim(RPS_REQUIREMENTS_FILE())//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      IF(EXTENSION_PERIOD_GROWTH_SWITCH == 'F') &
                                FORECAST_GROWTH_YEARS = AVAIL_DATA_YEARS
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = &
                         trim(FILE_TYPE)//'-'//RPS_REQUIREMENTS_FILE()
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_LOCATE_WRITE(16,30,RPS_REQUIREMENTS_FILE(), &
                                                         ALL_VERSIONS,0)
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         ENDIF
!
!
!
         SAVE_RR_FILE_EXISTS = .TRUE.
!
         OPEN(10,FILE=FILE_NAME) ! RRB
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCRPSREQ.BIN", &
                            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
!
         RPS_REQUIREMENT_TYPE = "Total_RPS"
!
         IREC = 0
         READ(10,*) DELETE ! RRB
         DO ! TABLES
            RR_YEAR = 1
            DO ! YEAR-BASED RECORDS
               READ(10,1000,IOSTAT=IOS) RECLN
!
!               IF(IOS /= 0) EXIT ! END OF FILE
!
               IF(RECLN(1:1) == '7' .OR. IOS /= 0) THEN ! END OF TABLE ! EXIT AT BOTTOM OF IF
                  IF(RR_YEAR <= AVAIL_DATA_YEARS+1) THEN
                     DO RR_YEAR = RR_YEAR, AVAIL_DATA_YEARS+1
                        IREC = IREC + 1
                        WRITE(11,REC=IREC) DELETE, &
                           RR_YEAR+STUDY_BASE_YEAR, & !  I 2
                           RPS_REQUIREMENT_TYPE, & !  C 20
                           RPS_STATE_PROVINCE    ! R 4 X 60
                     ENDDO
                  ENDIF ! DETECTED NEW TABLE OR END OF FILE
!
                  EXIT ! LEAVE LOOP (GO TO NEXT TABLE OR EXIT ROUTINE)
!
               ENDIF
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,ERR=200) DELETE, &
                           YEAR, & !  INT2
                           RPS_REQUIREMENT_TYPE, & !  C 20
                           RPS_STATE_PROVINCE    ! R 4 X 60
!
               IREC = IREC + 1
!
               READ(RECLN,*,ERR=200) DELETE,YEAR
               TEMP_YEAR = MIN(AVAIL_DATA_YEARS+int(1,2), &
                                                 YEAR - STUDY_BASE_YEAR)
!
!
               IF(TEMP_YEAR > RR_YEAR) THEN ! FILL IN MISSING YEARS
                  IF(IREC == 1) THEN
                     WRITE(4,*) "The first year of the first table in"
                     WRITE(4,*) "the Transact Forecast file is ",YEAR
                     WRITE(4,*) "RRile the base year set in project"
                     WRITE(4,*) "information is ",STUDY_BASE_YEAR
                     WRITE(4,*) "First forecast year in the Transact"
                     WRITE(4,*) "Forecast file first year must be one"
                     WRITE(4,*) "year greater than the base year."
                     WRITE(4,*) " "
                     er_message='See WARNINGS MESSAGES file.'
                     call end_program(er_message)
                  ENDIF
                  DO RR_YEAR = RR_YEAR, TEMP_YEAR - 1
                     WRITE(11,REC=IREC) DELETE, &
                           RR_YEAR+STUDY_BASE_YEAR, & !  I 2
                           RPS_REQUIREMENT_TYPE, & !  C 20
                           RPS_STATE_PROVINCE    ! R 4 X 60
                     IREC = IREC + 1
                  ENDDO


               ENDIF

               WRITE(11,REC=IREC) DELETE, &
                           RR_YEAR+STUDY_BASE_YEAR, & !  I 2
                           RPS_REQUIREMENT_TYPE, & !  C 20
                           RPS_STATE_PROVINCE    ! R 4 X 60
               RR_YEAR = RR_YEAR + 1
            ENDDO ! LOAD GROUP
            SAVE_TRANS_LOAD_TABLES = SAVE_TRANS_LOAD_TABLES + 1
            IF(IOS /= 0) EXIT
         ENDDO ! READ TABLES
         CLOSE(10)
      ELSE IF(INDEX(RPS_REQUIREMENTS_FILE(),'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "wh_objt:1111")
      ELSE
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCRPSREQ.BIN", &
                            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
!
         SAVE_RR_FILE_EXISTS = .FALSE.
!
      ENDIF
!      ENDFILE(11)
      CLOSE(11)
      RETURN

  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from wh_objt SIID374'
      call end_program(er_message)

!
!          ROUTINE TO CREATE OVERLAY FILES
!          COPYRIGHT (C) 1984-88  M.S. GERBER & ASSOCIATES, INC.
!          COPYRIGHT (C) 1991-92  M.S. GERBER & ASSOCIATES, INC.
!

!

      ENTRY RPS_REQUIREMENTS_MAKEOVL(OVERLAY_FAMILY_NAME)

      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      FILE_NAME=trim(OUTPUT_DIRECTORY())//"RRO"// &
                                     trim(OVERLAY_FAMILY_NAME)//".DAT"
      OPEN(10,FILE=FILE_NAME) ! RRO
      READ(10,*) DELETE
      RR_YEAR = 0
      INUNIT = 12
      IF(RPS_REQUIREMENTS_OL == 'BC') THEN
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCRPSREQ.BIN", &
                                             ACCESS="DIRECT",RECL=LRECL)
         INUNIT = 11
      ENDIF
      FILE_NAME = trim(OUTPUT_DIRECTORY())//"OLRPSREQ.BIN"
      OPEN(12,FILE=FILE_NAME,ACCESS="DIRECT", &
                                 STATUS="UNKNOWN",RECL=LRECL,IOSTAT=IOS)
      IF(IOS /= 0) THEN
         CALL iostatmsg_unit(IOS,MESSAGE)
         WRITE(4,*) trim(MESSAGE)
         er_message='in RPS_REQUIREMENTS_MAKEOVL see WARNINGS messages'
         call end_program(er_message)
      ENDIF
      IREC = 0
      DELETE = 1
      READ(10,1000,IOSTAT=IOS) RECLN
!
      DO WHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE
!
         READ(10,1000,IOSTAT=IOS) RECLN
!
      ENDDO
      READ(RECLN,*,ERR=200) DELETE,RR_YEAR
!
!
!
      DO ! TABLES AND YEARS, COUNTING IS DONE INSIDE
         IREC = IREC + 1
         READ(INUNIT,REC=IREC,IOSTAT=IOS) &
                           DELETE, &
                           YEAR, & !  INT2
                           RPS_REQUIREMENT_TYPE, & !  C 20
                           RPS_STATE_PROVINCE    ! R 4 X 60
         IF(IOS /= 0) EXIT ! END OF BINARY FILE
!
         IF(YEAR == RR_YEAR) THEN
            RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
            READ(RECLN,*,ERR=200) &
                           DELETE, &
                           RR_YEAR, & !  INT2
                           RPS_REQUIREMENT_TYPE, & !  C 20
                           RPS_STATE_PROVINCE    ! R 4 X 60
            READ(10,1000,IOSTAT=IOS) RECLN
!
            DO WHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE
!
               READ(10,1000,IOSTAT=IOS) RECLN
!
            ENDDO
            READ(RECLN,*,ERR=200) DELETE,RR_YEAR
         ENDIF
!
         WRITE(12,REC=IREC)  DELETE, &
                           YEAR, & !  INT2
                           RPS_REQUIREMENT_TYPE, & !  C 20
                           RPS_STATE_PROVINCE    ! R 4 X 60
      ENDDO
      CLOSE(10)
      CLOSE(12)
      IF(RPS_REQUIREMENTS_OL == 'BC') CLOSE(11)
      RPS_REQUIREMENTS_OL = 'OL'
      RETURN
!
!

      ENTRY RESET_RPS_REQUIREMENTS_OL

         RPS_REQUIREMENTS_OL = 'BC'
         SAVE_RR_FILE_USED = .FALSE.
      RETURN
!

      ENTRY RETURN_RPS_REQUIREMENTS_OL(R_RPS_REQUIREMENTS_OL,R_LRECL)

         R_RPS_REQUIREMENTS_OL = RPS_REQUIREMENTS_OL
         R_LRECL = LRECL
      RETURN

      ENTRY DOES_RPS_REQUIREMNTS_FILE_EXIST(R_RR_FILE_EXISTS)

         R_RR_FILE_EXISTS = SAVE_RR_FILE_EXISTS
      RETURN

      ENTRY GET_RPS_REQUIREMENTS_TABLES(R_TRANS_LOAD_TABLES)

         R_TRANS_LOAD_TABLES = SAVE_TRANS_LOAD_TABLES
      RETURN



      ENTRY OPEN_RPS_REQUIREMENTS_FILE

         OPEN(UNIT_NUM, &
              FILE=trim(OUTPUT_DIRECTORY())//RPS_REQUIREMENTS_OL// &
              "RPSREQ.BIN",ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      RETURN


      ENTRY CLOSE_RPS_REQUIREMENTS_FILE

         CLOSE(UNIT_NUM)
      RETURN
!
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END


!
!        PROGRAM TO MANAGE WEEKLY HYDRO FORECASTS, REVENUES AND COSTS
!           BY TRANSACTION GROUP ASSET CLASS AND CUSTOMER GROUP
!
!                       COPYRIGHT (C) 2009
!          ALL RIGHTS RESERVED M.S. GERBER & ASSOCIATES, INC.

      FUNCTION MANAGE_RPS_REQUIREMENTS()

      USE SIZECOM
      use SpinDriftLib
      USE PROD_ARRAYS_DIMENSIONS
      use globecom
      use end_routine
      implicit none
      SAVE

      LOGICAL (KIND=1) :: READ_RPS_REQUIREMENTS_DATA
      LOGICAL (KIND=1) :: SAVE_RPS_REQUIREMENTS_STATUS=.FALSE.
      LOGICAL (KIND=1) :: MANAGE_RPS_REQUIREMENTS
      LOGICAL (KIND=1) :: GET_RPS_REQUIREMENTS_FORECASTS
      LOGICAL (KIND=1) :: GET_STATE_RPS_REQS

      LOGICAL (KIND=1) :: DOES_RR_FILE_EXIST
      INTEGER (KIND=2) :: YR,MAKER_TABLES=0,DELETE,LOCAL_YEARS
      INTEGER (KIND=2) :: NUM_SCEN_VAR,IREC,NUM_RPS_REGIONS
      INTEGER (KIND=2) :: MAX_RPS_REGION_NUM,I,J,K,RPS_STATE_INDEX(:)
      INTEGER (KIND=2) :: R_STATE,R_YR
!
      CHARACTER (LEN=20) :: RPS_REQUIREMENT_TYPE
      INTEGER (KIND=2), PARAMETER :: NUM_RPS_TYPES=11 ! 050617 10 ! 122616 9
!
! INPUT DATA LIST
!
      INTEGER (KIND=4) :: VALUES_2_ZERO
!
      INTEGER (KIND=2) :: SCENARIO_YEAR,TABLE,SCENARIO_INDEX
!
      REAL (KIND=4) :: RPS_STATE_PROVINCE(56)
      REAL (KIND=4) :: RPS_REQUIREMENTS_VARIABLE(:,:,:)
      REAL (KIND=4) :: R_REQS(NUM_RPS_TYPES)
      ALLOCATABLE :: RPS_REQUIREMENTS_VARIABLE,RPS_STATE_INDEX
      LOGICAL (KIND=1) :: LAHEY_LF95
!
! END DATA DECLARATIONS
!
!
!
!
         READ_RPS_REQUIREMENTS_DATA = .FALSE.
!
         CALL DOES_RPS_REQUIREMNTS_FILE_EXIST(DOES_RR_FILE_EXIST)
         IF(DOES_RR_FILE_EXIST) THEN
            CALL OPEN_RPS_REQUIREMENTS_FILE
!
            CALL GET_RPS_REQUIREMENTS_TABLES(MAKER_TABLES)
!
            LOCAL_YEARS = 30
            NUM_RPS_REGIONS = 56
            MAX_RPS_REGION_NUM = 62
!
            IF( ALLOCATED(RPS_REQUIREMENTS_VARIABLE) ) &
                                  DEALLOCATE(RPS_REQUIREMENTS_VARIABLE, &
                                             RPS_STATE_INDEX)
            ALLOCATE( &
                  RPS_REQUIREMENTS_VARIABLE(NUM_RPS_TYPES, &
                                            MAX_RPS_REGION_NUM, &
                                            LOCAL_YEARS), &
                  RPS_STATE_INDEX(NUM_RPS_REGIONS))

            RPS_REQUIREMENTS_VARIABLE = 0.
!
            DO TABLE = 1, MAKER_TABLES
               RPS_STATE_INDEX = 1
! NOTE: K IS RECORD COUNTER (INCLUDING INDEX), YR IS YEAR COUNTER
               DO K = 1, AVAIL_DATA_YEARS+1
                  YR = K-1
                  IREC = (TABLE-1)*(AVAIL_DATA_YEARS+1) + K

                  READ(10,REC=IREC) DELETE, &
                           SCENARIO_YEAR, &
                           RPS_REQUIREMENT_TYPE, & !  C 20
                           RPS_STATE_PROVINCE    ! R 4 X 60
!
                  SCENARIO_INDEX = -99
                  SELECT CASE(trim(RPS_REQUIREMENT_TYPE))
                     CASE ('TOTAL_RPS')
                        SCENARIO_INDEX = 1
                     CASE ('WND_RPS')
                        SCENARIO_INDEX = 2
                     CASE ('SL_RPS')
                        SCENARIO_INDEX = 3
                     CASE ('BIO_RPS')
                        SCENARIO_INDEX = 4
                     CASE ('LFG_RPS')
                        SCENARIO_INDEX = 5
                     CASE ('GEO_RPS')
                        SCENARIO_INDEX = 6
                     CASE ('HY_RPS')
                        SCENARIO_INDEX = 7
                     CASE ('BA_RPS')
                        SCENARIO_INDEX = 8
                     CASE ('REC_Allowed')
                        SCENARIO_INDEX = 9
                     CASE ('Region')
                        SCENARIO_INDEX = 10
                     CASE ('Index')
                        SCENARIO_INDEX = 11
                  END SELECT
                  IF(SCENARIO_INDEX == -99) THEN
! NOT A VALID INDEX.
                  ELSEIF(SCENARIO_INDEX == 11) THEN
                     DO I = 1, NUM_RPS_REGIONS
                        RPS_STATE_INDEX(I) = RPS_STATE_PROVINCE(I)
                     ENDDO ! RPS REGIONS
                  ELSE
                     DO I = 1, NUM_RPS_REGIONS
                        J = RPS_STATE_INDEX(I)
                        RPS_REQUIREMENTS_VARIABLE(SCENARIO_INDEX, &
                                                              J,YR) = &
                                                   RPS_STATE_PROVINCE(I)
                     ENDDO ! RPS REGIONS
                  ENDIF
               ENDDO ! YEARS
!
            ENDDO ! TABLES
            READ_RPS_REQUIREMENTS_DATA = .TRUE.
            CALL CLOSE_RPS_REQUIREMENTS_FILE
         ENDIF

         SAVE_RPS_REQUIREMENTS_STATUS = READ_RPS_REQUIREMENTS_DATA
         MANAGE_RPS_REQUIREMENTS = .TRUE.
      RETURN

      ENTRY GET_STATE_RPS_REQS(R_STATE,R_YR,R_REQS)

         GET_STATE_RPS_REQS = .FALSE.
         R_REQS = 0.0
         IF(ALLOCATED(RPS_REQUIREMENTS_VARIABLE)) THEN
            IF(R_STATE > 0 .AND. R_STATE <= NUM_RPS_REGIONS) THEN
               YR = MIN(AVAIL_DATA_YEARS,R_YR)
               GET_STATE_RPS_REQS = .TRUE.
               R_REQS(1:10) = RPS_REQUIREMENTS_VARIABLE(1:10,R_STATE,YR)
            ENDIF
         ENDIF
      RETURN

      END
