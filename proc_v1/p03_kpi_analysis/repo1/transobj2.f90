
!     TRANSOBJ2.FOR
!     Copyright(c)  2000

!     Created: 8/5/2003 10:21:06 AM
!     Author : MARK S GERBER
!     Last change: msg 2/14/2022 6:50:34 PM
!            MSG 6/5/2016 11:42:03 AM
!            MSG 6/5/2016 11:32:55 AM





!
!                ROUTINE TO CONVERT TRANSACT TIE FILE
!
!                             COPYRIGHT (C) 1996
!                        M.S. GERBER & ASSOCIATES, INC.
!                             ALL RIGHTS RESERVED
!

!
! 011607. INITIALIZE GENERATION REPORTING GROUP
!
      SUBROUTINE TIE_OBJECT
      use end_routine
      use filename_tracker
      use month_extra
      use prod_arrays_dimensions
      use readenergyproductsdata
      use sizecom
      use spindriftlib
      use production
      implicit none

      SAVE

!!! RECORD LENGTH UPDATED WITH VARIABLE CHANGES
      INTEGER(kind=2) :: DELETE,INUNIT,IREC,LRECL=117,DAY
      INTEGER :: IOS
!
      INTEGER(kind=2) :: UNIT_NUM=10,GENCO
      integer (kind=2), PARAMETER :: MAX_GENCO = 25
      real (kind=4) :: &
              TABLE_VALUE(MAX_GENCO)
      CHARACTER(len=5) :: BASE_FILE_NAME,OVERLAY_FAMILY_NAME, &
                  TRANSACT_TIE_FILE
      CHARACTER(len=14) :: COMPANY_NAME
      CHARACTER(len=256) :: FILE_NAME
      CHARACTER(len=256) :: BASE_FILE_DIRECTORY
      CHARACTER(len=256) :: DATA_DRIVE,OUTPUT_DIRECTORY
      LOGICAL(kind=4) :: FILE_EXISTS,TIE_FILE_EXISTS=.FALSE., &
                  R_TIE_FILE_EXISTS
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER(len=1024) :: RECLN
! DECLARATION FOR TRANSACT TIE DETERMINANTS
      CHARACTER(len=16) :: FILE_TYPE='TRANSACT Ties   '
      CHARACTER(len=2) :: TRTIE_OL='BC'
      LOGICAL(kind=1) :: LAHEY_LF95
      CHARACTER(len=30) :: SCREEN_OUTPUT

! CONVERT THE TIE FILE

      ENTRY TIE_MAKEBIN

      BASE_FILE_NAME = TRANSACT_TIE_FILE()
      DATA_DRIVE = OUTPUT_DIRECTORY()

      FILE_NAME = get_ft_filename("TTB", base_file_name)
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      TIE_FILE_EXISTS = FILE_EXISTS
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_LOCATE_WRITE(16,30,BASE_FILE_NAME,ALL_VERSIONS,0)
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         ENDIF
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCTRTIE.BIN",ACCESS="DIRECT", &
                                            STATUS="UNKNOWN",RECL=LRECL)
         IREC = 1
!
!
           TABLE_VALUE = 0.
!
         DO
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(IOS /= 0) EXIT
            DO GENCO = 1, MAX_GENCO
               READ(10,1000,IOSTAT=IOS) RECLN
               IF(IOS /= 0) EXIT
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,ERR=200) DELETE, &
                 COMPANY_NAME, &
                    TABLE_VALUE
               WRITE(11,REC=IREC) DELETE, &
                    TABLE_VALUE
               IREC = IREC + 1
            ENDDO ! MAX_GENCO
         ENDDO ! TABLES
         CLOSE(10)
         CLOSE(11)
      ELSE IF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "transobj2:1112")
      ENDIF
      RETURN



! OVERLAY THE TIE FILE

      ENTRY TIE_MAKEOVL(OVERLAY_FAMILY_NAME)

!
      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME = get_tto_filename(data_drive, overlay_family_name)
      OPEN(10,FILE=FILE_NAME)

      INUNIT = 12
      IF(TRTIE_OL == 'BC') THEN
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCTRTIE.BIN",ACCESS="DIRECT", &
                                                             RECL=LRECL)
         INUNIT = 11
      ENDIF
      OPEN(12,FILE=trim(DATA_DRIVE)//"OLTRTIE.BIN",ACCESS="DIRECT", &
                                            STATUS="UNKNOWN",RECL=LRECL)
      IREC = 0
      DO
         READ(10,1000,IOSTAT=IOS) RECLN
         IF(IOS /= 0) EXIT
         DO GENCO = 1, MAX_GENCO
            IREC = IREC + 1
            READ(INUNIT,REC=IREC,IOSTAT=IOS) DELETE, &
                    TABLE_VALUE
            IF(IOS /= 0) EXIT
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(IOS == 0) THEN
               RECLN = trim(RECLN)//',,,,,,,,,,,,,'
               READ(RECLN,*,ERR=300) DELETE, &
                  COMPANY_NAME, &
                    TABLE_VALUE
            ENDIF
            WRITE(12,REC=IREC) DELETE, &
                    TABLE_VALUE
         ENDDO
      ENDDO
      CLOSE(10)
      CLOSE(12)
      IF(TRTIE_OL == 'BC') CLOSE(11)
      TRTIE_OL = 'OL'
      RETURN

  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from TRANSOBJ2 SIID313'
      call end_program(er_message)

  300 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      CALL MG_LOCATE_WRITE(21,0, &
                 'Error reading the Transmission Tie record. Look for'// &
                                          ' a "," in a character name.', &
                           ALL_VERSIONS,1)
      er_message='stop requested from TRANSOBJ2 SIID314'
      call end_program(er_message)
!

      ENTRY RESET_TRTIE_OL

         TRTIE_OL = 'BC'
      RETURN
!

      ENTRY OPEN_TRTIE_FILE

         OPEN(UNIT_NUM,FILE=trim(OUTPUT_DIRECTORY())//TRTIE_OL// &
                "TRTIE.BIN",ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      RETURN
!

      ENTRY CLOSE_TRTIE_FILE

         CLOSE(UNIT_NUM)
      RETURN

      ENTRY DOES_TRTIE_FILE_EXIST(R_TIE_FILE_EXISTS)

         R_TIE_FILE_EXISTS = TIE_FILE_EXISTS
      RETURN
!
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END

      FUNCTION  ALINE_LOAD_DATA(UTILITY,R_MONTH)
      use end_routine
      implicit none
      INTEGER(kind=2) :: ALINE_LOAD_DATA,UTILITY,R_MONTH

      LOGICAL(kind=1) :: LAHEY_LF95
      INTEGER(kind=2) :: LOAD_START_REC(13)= &
                      (/1,32,61,92,122,153,183,214,245,275,306,336,367/)
!
!         MONTH = MAP_MONTH(UTILITY,R_MONTH)
         IF(LAHEY_LF95()) THEN
            ALINE_LOAD_DATA = LOAD_START_REC(R_MONTH) + 1
         ELSE
            ALINE_LOAD_DATA = LOAD_START_REC(R_MONTH)
         ENDIF
      RETURN

      END function aline_load_data

      FUNCTION READ_TRANSACT_TIE_DATA()
      use end_routine, only: end_program, er_message
      use GRX_PLANNING_ROUTINES
	  use production

!
      use spindriftlib
      use prod_arrays_dimensions
      USE IREC_ENDPOINT_CONTROL
      USE SIZECOM
      use globecom
      SAVE

!
      INTEGER(kind=2) :: DELETE, &
                TRANS_GROUP,CURRENT_RECORD, &
                G0,G1,MAX_TIE_GROUPS, &
                R_BUYER,R_SELLER,R_MONTH,R_HOUR_IN_DAY, &
                R_MAX_TRANS_GROUPS,DAILY_HOURS=24, &
                R_TRANS_GROUP,RPT_HR,R_YEAR,R_DAY
      CHARACTER(len=20) :: R_TRANS_NAME
      INTEGER(kind=4) :: VALUES_2_ZERO
      PARAMETER (MAX_TIE_GROUPS=25)
      LOGICAL(kind=1) :: READ_TRANSACT_TIE_DATA, &
                  INIT_HOURLY_TIE,RECORD_TRANSACTION_TO_TIE, &
                  UPDATE_MONTHLY_TIE_COST, &
                  MONTHLY_TIE_FLOW_EXISTS=.FALSE., &
                  MONTHLY_TIE_CHARGE_EXISTS=.FALSE., &
                  INIT_DAILY_TIE_REVENUES,WRITE_DAILY_TIE_REVENUES
      LOGICAL(kind=4) :: TIE_FILE_EXISTS
      REAL(kind=4) :: &
              SAVE_TIE_LIMIT(MAX_TIE_GROUPS,MAX_TIE_GROUPS), & !  FROM / TO
              SAVE_TIE_CHARGE(MAX_TIE_GROUPS,MAX_TIE_GROUPS), &
              SAVE_TIE_SPREAD(MAX_TIE_GROUPS,MAX_TIE_GROUPS), &
              SAVE_TIE_FLOW(MAX_TIE_GROUPS,MAX_TIE_GROUPS, &
                                                       MAX_TIE_GROUPS), &
              HOURLY_TIE_AVAILABLE(MAX_TIE_GROUPS,MAX_TIE_GROUPS), &
              HOURLY_TIE_FLOW(MAX_TIE_GROUPS,MAX_TIE_GROUPS, &
                                                       MAX_TIE_GROUPS), &
              ANNUAL_DELIVERED_TIE_COST(MAX_TIE_GROUPS, &
                                                       MAX_TIE_GROUPS), &
              MONTHLY_DELIVERED_TIE_COST(MAX_TIE_GROUPS, &
                                                       MAX_TIE_GROUPS)
      REAL(kind=4) :: &
              TIE_CHARGE,TIE_SPREAD, &
              DELIVERED_TIE_COST,HOURLY_TIE_LIMIT, &
              SEASONAL_TIE_LIMIT, &
              MONTHLY_TRANS_REVENUE(MAX_TIE_GROUPS,MAX_TIE_GROUPS), &
              MONTHLY_TRANS_COST(MAX_TIE_GROUPS,MAX_TIE_GROUPS), &
              R_QUANTITY,GET_VAR, &
              TEMP_TIE_FLOW,TEMP_TIE_CHARGE, &
              QUANTITY_TO_TRANS_GROUP, &
              MONTHLY_TRANS_QUANTITY(MAX_TIE_GROUPS,MAX_TIE_GROUPS), &
              TRANS_COST
!
! DETAILED REPORT OVERHEAD
!
      LOGICAL(kind=1) :: TRMS_REPORT_NOT_OPEN=.TRUE., &
                   TRMS_REPORT_ACTIVE=.TRUE.,TRANS_TRANSMISSION_REPORTS
      INTEGER(kind=2) :: LAST_SEASON=0,PRODUCTION_PERIODS, &
                  TRANS_TRM_RPT_HEADER,TRANS_TRM_UNIT, &
                  CURRENT_MONTH
      REAL(kind=4) :: HOURLY_TRANS_REVENUE(:,:)
      ALLOCATABLE :: HOURLY_TRANS_REVENUE
!

      CHARACTER(len=9) :: CL_MONTH_NAME(13),MONTH_NAME*20
      INTEGER :: TRANS_TRM_REC

! END DATA DECLARATIONS
         SAVE_TIE_LIMIT = 99999.
         SAVE_TIE_CHARGE = 0.
         SAVE_TIE_SPREAD = 0.
         ANNUAL_DELIVERED_TIE_COST = 0.

         SAVE_TIE_FLOW = 0.

         CALL DOES_TRTIE_FILE_EXIST(TIE_FILE_EXISTS)
         IF(.NOT. TIE_FILE_EXISTS) RETURN
         CALL OPEN_TRTIE_FILE
         CURRENT_RECORD = 0

         DO    TRANS_GROUP = 1, MAX_TIE_GROUPS
            CURRENT_RECORD = CURRENT_RECORD + 1
            READ(10,REC=CURRENT_RECORD) DELETE, &
                (SAVE_TIE_LIMIT(G0,TRANS_GROUP),G0 = 1, MAX_TIE_GROUPS)
         ENDDO
!
         DO    TRANS_GROUP = 1, MAX_TIE_GROUPS
            CURRENT_RECORD = CURRENT_RECORD + 1
            READ(10,REC=CURRENT_RECORD) DELETE, &
               (SAVE_TIE_CHARGE(G0,TRANS_GROUP),G0 = 1, MAX_TIE_GROUPS)
         ENDDO
!
         DO    TRANS_GROUP = 1, MAX_TIE_GROUPS
            CURRENT_RECORD = CURRENT_RECORD + 1
            READ(10,REC=CURRENT_RECORD) DELETE, &
               (SAVE_TIE_SPREAD(G0,TRANS_GROUP),G0 = 1, MAX_TIE_GROUPS)
         ENDDO
!
         DO    G1 = 1, MAX_TIE_GROUPS ! TABLE (FROM)
            DO    TRANS_GROUP = 1, MAX_TIE_GROUPS ! COLUMN (TO)
               CURRENT_RECORD = CURRENT_RECORD + 1
               READ(10,REC=CURRENT_RECORD) DELETE, &
                  (SAVE_TIE_FLOW(G1,G0,TRANS_GROUP), &
                                                G0 = 1, MAX_TIE_GROUPS) ! ROWS
!
               DO G0 = 1, MAX_TIE_GROUPS
!
                  IF(SAVE_TIE_FLOW(G1,G0,TRANS_GROUP) < 0.) &
                                        MONTHLY_TIE_FLOW_EXISTS = .TRUE.
!
                  IF(SAVE_TIE_CHARGE(G1,TRANS_GROUP) < 0.) &
                                      MONTHLY_TIE_CHARGE_EXISTS = .TRUE.
!
                  ANNUAL_DELIVERED_TIE_COST(G1,TRANS_GROUP) = &
                           ANNUAL_DELIVERED_TIE_COST(G1,TRANS_GROUP) + &
                                  SAVE_TIE_FLOW(G1,G0,TRANS_GROUP) * &
                                         SAVE_TIE_CHARGE(G1,TRANS_GROUP)
               ENDDO
!
            ENDDO
         ENDDO
         CALL CLOSE_TRTIE_FILE
         READ_TRANSACT_TIE_DATA = .TRUE.
      RETURN

      ENTRY UPDATE_MONTHLY_TIE_COST(R_MONTH)

!
         TRMS_REPORT_ACTIVE = TRANS_TRANSMISSION_REPORTS()
!
         IF(TRMS_REPORT_NOT_OPEN .AND. &
                             TRMS_REPORT_ACTIVE .AND. &
                                               .NOT. TESTING_PLAN ) THEN
            TRMS_REPORT_NOT_OPEN = .FALSE.
            TRANS_TRM_UNIT = TRANS_TRM_RPT_HEADER(TRANS_TRM_REC)
            LAST_SEASON = get_PRODUCTION_PERIODS_in()
            DO CURRENT_MONTH = 1, LAST_SEASON
               CL_MONTH_NAME(CURRENT_MONTH) = MONTH_NAME(CURRENT_MONTH)
            ENDDO
            CL_MONTH_NAME(LAST_SEASON+1) = 'Annual'

         ENDIF
         IF(ALLOCATED(HOURLY_TRANS_REVENUE)) &
                                        DEALLOCATE(HOURLY_TRANS_REVENUE)
         ALLOCATE(HOURLY_TRANS_REVENUE(MAX_TIE_GROUPS,DAILY_HOURS))
!
         UPDATE_MONTHLY_TIE_COST = .FALSE.
         MONTHLY_TRANS_REVENUE = 0.
         MONTHLY_TRANS_COST = 0.
         MONTHLY_TRANS_QUANTITY = 0.
         IF(MONTHLY_TIE_FLOW_EXISTS .OR. MONTHLY_TIE_CHARGE_EXISTS) THEN
            MONTHLY_DELIVERED_TIE_COST = 0.
            UPDATE_MONTHLY_TIE_COST = .TRUE.
            DO    G1 = 1, MAX_TIE_GROUPS
               DO    TRANS_GROUP = 1, MAX_TIE_GROUPS
                  DO G0 = 1, MAX_TIE_GROUPS
!
                     IF(SAVE_TIE_FLOW(G1,G0,TRANS_GROUP) >= 0. .AND. &
                                      SAVE_TIE_CHARGE(G1,G0) >= 0.) THEN
!
                        MONTHLY_DELIVERED_TIE_COST(G1,G0) = &
                                        ANNUAL_DELIVERED_TIE_COST(G1,G0)
                     ELSE
                        IF(SAVE_TIE_FLOW(G1,G0,TRANS_GROUP) < 0.) THEN
                           TEMP_TIE_FLOW = &
                               GET_VAR(SAVE_TIE_FLOW(G1,G0,TRANS_GROUP), &
                                              R_MONTH,'Tie Flow Vector')
                        ELSE
                           TEMP_TIE_FLOW = &
                                        SAVE_TIE_FLOW(G1,G0,TRANS_GROUP)
                        ENDIF
                        IF(SAVE_TIE_CHARGE(G1,G0) < 0.) THEN
                           TEMP_TIE_CHARGE = &
                                 GET_VAR(SAVE_TIE_CHARGE(G1,G0),R_MONTH, &
                                                    'Tie Charge Vector')
                        ELSE
                           TEMP_TIE_CHARGE = SAVE_TIE_CHARGE(G1,G0)
                        ENDIF
                        MONTHLY_DELIVERED_TIE_COST(G1,G0) = &
                           MONTHLY_DELIVERED_TIE_COST(G1,G0) + &
                                         TEMP_TIE_FLOW * TEMP_TIE_CHARGE
                     ENDIF
                  ENDDO
!
               ENDDO
            ENDDO
         ELSE
             MONTHLY_DELIVERED_TIE_COST = ANNUAL_DELIVERED_TIE_COST

         ENDIF
      RETURN

      ENTRY INIT_DAILY_TIE_REVENUES()

!
         HOURLY_TRANS_REVENUE = 0.
         INIT_DAILY_TIE_REVENUES = .TRUE.
!
      RETURN

      ENTRY WRITE_DAILY_TIE_REVENUES(R_YEAR,R_MONTH,R_TRANS_GROUP, &
                                     R_DAY,R_TRANS_NAME)

!
         WRITE_DAILY_TIE_REVENUES = .FALSE.
!
         IF(TRMS_REPORT_ACTIVE) THEN
            WRITE(TRANS_TRM_UNIT,REC=TRANS_TRM_REC) &
               PRT_ENDPOINT(), &
               FLOAT(R_YEAR), &
               CL_MONTH_NAME(R_MONTH), &
               FLOAT(R_DAY), &
               R_TRANS_NAME, &
               (HOURLY_TRANS_REVENUE(R_TRANS_GROUP,RPT_HR), &
                                                   RPT_HR=1,DAILY_HOURS)
            TRANS_TRM_REC = TRANS_TRM_REC + 1
            WRITE_DAILY_TIE_REVENUES = .TRUE.
         ENDIF
!
      RETURN

      ENTRY TIE_CHARGE(R_SELLER,R_BUYER)

         IF(R_SELLER <= MAX_TIE_GROUPS .AND. &
                                     R_BUYER < MAX_TIE_GROUPS) THEN
            TIE_CHARGE = SAVE_TIE_CHARGE(R_SELLER,R_BUYER)
         ELSE
            TIE_CHARGE = 0.
         ENDIF
      RETURN

      ENTRY TIE_SPREAD(R_SELLER,R_BUYER)

         IF(R_SELLER <= 0 .OR. R_BUYER <= 0) THEN
            WRITE(4,*) '*** line 8968 TRANSOBJ.FOR ***'
            WRITE(4,*) "MARKET CLEARING ROUTINE DID NOT FIND"
            WRITE(4,*) "A POSSIBLE TRANSACTION"
            er_message='See WARNING MESSAGES -TRANSOBJ2.FOR-1'
            call end_program(er_message)
         ELSE
            IF(R_SELLER <= MAX_TIE_GROUPS .AND. &
                                         R_BUYER <= MAX_TIE_GROUPS) THEN
               TIE_SPREAD = SAVE_TIE_SPREAD(R_SELLER,R_BUYER)
            ELSE
               TIE_SPREAD = 0.
            ENDIF
         ENDIF
      RETURN
!

      ENTRY DELIVERED_TIE_COST(R_SELLER,R_BUYER)

!
         IF(R_SELLER <= MAX_TIE_GROUPS .AND. &
                                         R_BUYER <= MAX_TIE_GROUPS) THEN
            DELIVERED_TIE_COST = &
                      MONTHLY_DELIVERED_TIE_COST(R_SELLER,R_BUYER)
         ELSE
            DELIVERED_TIE_COST = 0.
         ENDIF
!
      RETURN
!

      ENTRY INIT_HOURLY_TIE

!
         HOURLY_TIE_AVAILABLE = SAVE_TIE_LIMIT
         HOURLY_TIE_FLOW = SAVE_TIE_FLOW

         INIT_HOURLY_TIE = .TRUE.
      RETURN

      ENTRY HOURLY_TIE_LIMIT(R_SELLER,R_BUYER)

!
         IF(R_SELLER <= 0 .OR. R_SELLER > MAX_TIE_GROUPS .OR. &
                       R_BUYER <= 0. .OR. R_BUYER > MAX_TIE_GROUPS) THEN
            HOURLY_TIE_LIMIT = 99999.
         ELSE
            HOURLY_TIE_LIMIT = HOURLY_TIE_AVAILABLE(R_SELLER,R_BUYER)
         ENDIF
      RETURN

      ENTRY SEASONAL_TIE_LIMIT(R_SELLER,R_BUYER)

         IF(R_SELLER <= 0 .OR. R_SELLER > MAX_TIE_GROUPS .OR. &
                       R_BUYER <= 0. .OR. R_BUYER > MAX_TIE_GROUPS) THEN
            SEASONAL_TIE_LIMIT = 99999.
         ELSE
            SEASONAL_TIE_LIMIT = SAVE_TIE_LIMIT(R_SELLER,R_BUYER)
         ENDIF
      RETURN

      ENTRY RECORD_TRANSACTION_TO_TIE(R_SELLER,R_BUYER,R_QUANTITY, &
                                                          R_HOUR_IN_DAY)

!
         IF(R_QUANTITY <= 0.) RETURN
!
         HOURLY_TIE_AVAILABLE(R_SELLER,R_BUYER) = MAX(0., &
                    HOURLY_TIE_AVAILABLE(R_SELLER,R_BUYER) - R_QUANTITY)
         DO    TRANS_GROUP = 1, MAX_TIE_GROUPS
            QUANTITY_TO_TRANS_GROUP = R_QUANTITY * &
                           SAVE_TIE_FLOW(R_SELLER,TRANS_GROUP,R_BUYER)
            TRANS_COST = SAVE_TIE_CHARGE(R_SELLER,TRANS_GROUP) * &
                                                 QUANTITY_TO_TRANS_GROUP
            MONTHLY_TRANS_REVENUE(R_SELLER,TRANS_GROUP) = &
                MONTHLY_TRANS_REVENUE(R_SELLER,TRANS_GROUP) + TRANS_COST
            MONTHLY_TRANS_QUANTITY(R_SELLER,TRANS_GROUP) = &
                     MONTHLY_TRANS_QUANTITY(R_SELLER,TRANS_GROUP) + &
                                                 QUANTITY_TO_TRANS_GROUP
            MONTHLY_TRANS_COST(TRANS_GROUP,R_SELLER) = &
                   MONTHLY_TRANS_COST(TRANS_GROUP,R_SELLER) + TRANS_COST
!
            HOURLY_TRANS_REVENUE(TRANS_GROUP,R_HOUR_IN_DAY) = &
                       HOURLY_TRANS_REVENUE(TRANS_GROUP,R_HOUR_IN_DAY) + &
                                                              TRANS_COST
            HOURLY_TRANS_REVENUE(R_SELLER,R_HOUR_IN_DAY) = &
                          HOURLY_TRANS_REVENUE(R_SELLER,R_HOUR_IN_DAY) - &
                                                              TRANS_COST
!
         ENDDO
         RECORD_TRANSACTION_TO_TIE = .TRUE.
      RETURN
      END


!                ROUTINE TO CONVERT DAY_TYPE FILE
!
!                           COPYRIGHT (C) 1997
!                        M.S. GERBER & ASSOCIATES, INC.
!                           ALL RIGHTS RESERVED
!

!
      SUBROUTINE DAY_TYPE_OBJECT
      use end_routine, only: end_program, er_message
      use filename_tracker
      use grx_planning_routines
      USE SIZECOM
      use spindriftlib
      use prod_arrays_dimensions
      implicit none
      SAVE
!!! RECORD LENGTH UPDATED WITH VARIABLE CHANGES
      INTEGER(kind=2) :: DELETE,INUNIT,IREC,LRECL=100
      INTEGER :: IOS
!
      INTEGER(kind=2) :: UNIT_NUM=10,HOURS_PER_DAY, &
                  DAY_TYPES_IN_MONTH,BIN_TABLES_IN_FILE=0, &
                  MONTHS_PER_YEAR,DAY
      PARAMETER ( HOURS_PER_DAY = 24,DAY_TYPES_IN_MONTH=7, &
                  MONTHS_PER_YEAR=12)
      INTEGER(kind=2) :: TABLE_VALUE(HOURS_PER_DAY)
      CHARACTER(len=5) :: BASE_FILE_NAME,OVERLAY_FAMILY_NAME, &
                  DAY_TYPE_FILE
      CHARACTER(len=4) :: DAY_TYPE_NAME
      CHARACTER(len=256) :: FILE_NAME
      CHARACTER(len=256) :: BASE_FILE_DIRECTORY
      CHARACTER(len=256) :: DATA_DRIVE,OUTPUT_DIRECTORY
      LOGICAL(kind=4) :: FILE_EXISTS,DAY_TYPE_FILE_EXISTS=.FALSE., &
                  R_DAY_TYPE_FILE_EXISTS
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER(len=1024) :: RECLN
! DECLARATION FOR DAY TYPE DETERMINANTS
      CHARACTER(len=16) :: FILE_TYPE='Day Type        '
      CHARACTER(len=2) :: DAY_TYPE_OL='BC'
      LOGICAL(kind=1) :: LAHEY_LF95
      CHARACTER(len=30) :: SCREEN_OUTPUT

! CONVERT THE DAY_TYPE FILE

      ENTRY DAY_TYPE_MAKEBIN

      BASE_FILE_NAME = DAY_TYPE_FILE()
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME = get_dtb_filename(base_file_name)
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      DAY_TYPE_FILE_EXISTS = FILE_EXISTS
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_LOCATE_WRITE(16,30,BASE_FILE_NAME,ALL_VERSIONS,0)
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         ENDIF
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCDYTYP.BIN",ACCESS="DIRECT", &
                                            STATUS="UNKNOWN",RECL=LRECL)
         IREC = 1
!
!
           TABLE_VALUE = 0
          BIN_TABLES_IN_FILE = 0
!
         DO
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(IOS /= 0) EXIT
            BIN_TABLES_IN_FILE = BIN_TABLES_IN_FILE + 1
            DO DAY = 1, DAY_TYPES_IN_MONTH
               READ(10,1000,IOSTAT=IOS) RECLN
               IF(IOS /= 0) EXIT
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,ERR=200) DELETE, &
                 DAY_TYPE_NAME, &
                    TABLE_VALUE
               WRITE(11,REC=IREC) DELETE, &
                    TABLE_VALUE
               IREC = IREC + 1
            ENDDO ! HOURS_PER_DAY
         ENDDO ! TABLE
         CLOSE(10)
         CLOSE(11)
      ELSE IF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "transobj2:11111")
      ENDIF
      RETURN



! OVERLAY THE DAY TYPE FILE

      ENTRY DAY_TYPE_MAKEOVL(OVERLAY_FAMILY_NAME)

!
      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME = get_dto_filename(data_drive, overlay_family_name)
      OPEN(10,FILE=FILE_NAME)
      READ(10,*) DELETE
      INUNIT = 12
      IF(DAY_TYPE_OL == 'BC') THEN
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCDYTYP.BIN",ACCESS="DIRECT", &
                                                             RECL=LRECL)
         INUNIT = 11
      ENDIF
      OPEN(12,FILE=trim(DATA_DRIVE)//"OLDYTYP.BIN",ACCESS="DIRECT", &
                                            STATUS="UNKNOWN",RECL=LRECL)
      IREC = 0
      DO
         READ(10,1000,IOSTAT=IOS) RECLN
         IF(IOS /= 0) EXIT
         DO DAY = 1, DAY_TYPES_IN_MONTH
            IREC = IREC + 1
            READ(INUNIT,REC=IREC,IOSTAT=IOS) DELETE, &
                    TABLE_VALUE
            IF(IOS /= 0) EXIT
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(IOS == 0) THEN
               RECLN = trim(RECLN)//',,,,,,,,,,,,,'
               READ(RECLN,*,ERR=300) DELETE, &
                 DAY_TYPE_NAME, &
                    TABLE_VALUE
            ENDIF
            WRITE(12,REC=IREC) DELETE, &
                    TABLE_VALUE
         ENDDO
      ENDDO
      CLOSE(10)
      CLOSE(12)
      IF(DAY_TYPE_OL == 'BC') CLOSE(11)
      DAY_TYPE_OL = 'OL'
      RETURN

  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from TRANSOBJ2 SIID316'
      call end_program(er_message)

  300 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      CALL MG_LOCATE_WRITE(21,0, &
                         'Error reading the Day Type record. Look for'// &
                                          ' a "," in a character name.', &
                           ALL_VERSIONS,1)
      er_message='stop requested from TRANSOBJ2 SIID317'
      call end_program(er_message)
!

      ENTRY RESET_DAY_TYPE_OL

         DAY_TYPE_OL = 'BC'
      RETURN
!

      ENTRY OPEN_DAY_TYPE_FILE

         OPEN(UNIT_NUM,FILE=trim(OUTPUT_DIRECTORY())//DAY_TYPE_OL// &
                "DYTYP.BIN",ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      RETURN
!

      ENTRY CLOSE_DAY_TYPE_FILE

         CLOSE(UNIT_NUM)
      RETURN

      ENTRY DOES_DAY_TYPE_FILE_EXIST(R_DAY_TYPE_FILE_EXISTS)

         R_DAY_TYPE_FILE_EXISTS = DAY_TYPE_FILE_EXISTS
      RETURN
!
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END

      FUNCTION READ_DAY_TYPE_DATA()
      use end_routine
      use spindriftlib
      use prod_arrays_dimensions
      use debugtrace
      SAVE
      


      
      INTEGER(kind=4) :: VALUES_2_ZERO
      INTEGER(kind=2) :: DELETE,HOURS_PER_DAY,DAY_TYPES_IN_MONTH, &
                  MONTHS_PER_YEAR,CURRENT_RECORD,MONTH,HOUR,DAY_TYPE, &
                  GET_DAY_TYPE,R_HOUR,R_DAY_OF_WEEK,R_MONTH, &
                  LAST_DAY_TYPE=1,GET_LAST_DAY_TYPE
      PARAMETER ( HOURS_PER_DAY = 24,DAY_TYPES_IN_MONTH=7, &
                  MONTHS_PER_YEAR=12)
      INTEGER(kind=2) :: DAY_TYPE_DATA(HOURS_PER_DAY,DAY_TYPES_IN_MONTH, &
                                                        MONTHS_PER_YEAR)
      LOGICAL(kind=1) :: READ_DAY_TYPE_DATA
      LOGICAL(kind=4) :: DAY_TYPE_FILE_EXISTS
!      SAVE      DAY_TYPE_DATA
!
! END DATA DECLARATIONS
!
         READ_DAY_TYPE_DATA = .FALSE.
!
         VALUES_2_ZERO = INT(HOURS_PER_DAY*DAY_TYPES_IN_MONTH* &
                                                        MONTHS_PER_YEAR)
!
         DAY_TYPE_DATA = 1
!
         CALL DOES_DAY_TYPE_FILE_EXIST(DAY_TYPE_FILE_EXISTS)
         IF(.NOT. DAY_TYPE_FILE_EXISTS) RETURN
         CALL OPEN_DAY_TYPE_FILE
         CURRENT_RECORD = 0
!
         DO MONTH = 1, MONTHS_PER_YEAR
            DO DAY_TYPE = 1, DAY_TYPES_IN_MONTH
               CURRENT_RECORD = CURRENT_RECORD + 1
               READ(10,REC=CURRENT_RECORD) DELETE, &
                           (DAY_TYPE_DATA(HOUR,DAY_TYPE,MONTH), &
                                                HOUR = 1, HOURS_PER_DAY)
               DO HOUR = 1, HOURS_PER_DAY
                  LAST_DAY_TYPE = MAX(LAST_DAY_TYPE, &
                                     DAY_TYPE_DATA(HOUR,DAY_TYPE,MONTH))
               ENDDO
            ENDDO
         ENDDO
         CALL CLOSE_DAY_TYPE_FILE
         READ_DAY_TYPE_DATA = .TRUE.
      RETURN

      ENTRY GET_LAST_DAY_TYPE()

         GET_LAST_DAY_TYPE = LAST_DAY_TYPE
      RETURN

      ENTRY GET_DAY_TYPE(R_HOUR,R_DAY_OF_WEEK,R_MONTH)

         IF(R_DAY_OF_WEEK > 7) THEN
            R_DAY_OF_WEEK = 7
         ELSEIF(R_DAY_OF_WEEK < 1) THEN
            R_DAY_OF_WEEK = 1
         ENDIF
         GET_DAY_TYPE = DAY_TYPE_DATA(R_HOUR,R_DAY_OF_WEEK,R_MONTH)
      RETURN
      END

!                  ROUTINE TO CREATE AN ENERGY PRODUCTS FILE
!                          RENAMED POWER DERIVATIVES

!                           COPYRIGHT (C) 1997-2003
!                        M.S. GERBER & ASSOCIATES, INC.
!                           ALL RIGHTS RESERVED

      SUBROUTINE ENERGY_PRODUCTS_OBJECT
      use end_routine
      use filename_tracker
      use rps_index_translate_mod


      USE SIZECOM
      use spindriftlib
      use prod_arrays_dimensions
      implicit none
      SAVE
      INTEGER(kind=2) :: DELETE,INUNIT,IREC,LRECL=1048
      INTEGER :: IOS
      INTEGER(kind=2) :: UNIT_NUM=10
      CHARACTER(len=1) :: WVPA_RATE_TRACKER,WVPA_MEM_TRACKER
      CHARACTER(len=5) :: BASE_FILE_NAME,OVERLAY_FAMILY_NAME, &
                  ENERGY_PRODUCTS_FILE, &
                  REFERENCE_LOAD_NAME
      CHARACTER(len=256) :: FILE_NAME
      CHARACTER(len=256) :: BASE_FILE_DIRECTORY
      CHARACTER(len=256) :: DATA_DRIVE,OUTPUT_DIRECTORY
      LOGICAL(kind=4) :: FILE_EXISTS, &
                  ENERGY_PRODUCTS_FILE_EXISTS=.FALSE., &
                  R_ENERGY_PRODUCTS_FILE_EXISTS
!
      INTEGER(kind=8) :: DERIV_PARENT_ID
      CHARACTER(len=36) :: DERIV_GUID
      CHARACTER(len=50) :: COMMENT
      CHARACTER(len=20) :: TRANSACTION_NAME,COUNTERPARTY_NAME, &
                     TRANSACTION_TYPE, &
                     PRODUCT_TYPE, &
                     DISTINGUISHING
! 042609
      INTEGER(kind=2) :: &
                     Tax_Credit_Begin_Date, & !  I 2
                     Tax_Credit_End_Date, & !  90 I 2
                     GRX_ID, &
                     REFERENCE_LOAD_NUMBER, &
                     EMISS_MARKET_DATA_PTR, & !  128 I 2
                     GRX_BUILD_VECTOR ! 126
      INTEGER(kind=4) :: &
                     Holding_Company_ID, & !  95 I 4
                     External_Unit_ID, & !  I 4
                     EV_ID_HYBRID_BATTERY ! 131
      REAL(kind=4) :: &
                     Production_Tax_Credit, & !  R 4
                     RPS_Contribution, & !  R 4
                     Percentage_Owned ! R 4
      CHARACTER(len=6) :: &
                     State_TG_Key, & !  S 6
                     EIA_Plant_ID, & !  S 6
                     DERIV_ZONE_ID
      CHARACTER(len=10) :: &
                     Fuel_Category ! 100 S 10
      CHARACTER(len=12) :: &
                     Owner_ID     ! S 12
      CHARACTER(len=20) :: &
                     New_Build_Unit_Status, & !  S 20
                     Latin_Hypercube_Dist ! S 20
      CHARACTER(len=25) :: &
                     NERC_Subregion, & !  S 25
                     State_Province ! S 25
!
      CHARACTER(len=1) :: &
                     TRANSACTION_CLASS, &
                     EXPENSE_ASSIGNMENT, &
                     EXPENSE_COLLECTION, &
                     PRICE_TYPE, &
                     OPTION_POSITION, &
                     PRODUCT_ACTIVE, &
                     STRIKE_FREQUENCY, &
                     REPORT_PRODUCT, &
                     UNIT_CONTINGENCY, &
!     +               FUEL_TYPE, ! C* &
                     FUEL_PRICE_TYPE, & !  C*1
                     TRANSPORTATION_BASIS_TYPE, & !  C*1
                     DERIV_AGGREGATED_UNIT
      CHARACTER(len=2) :: UNIT_TYPE,UNIT_TYPE_CATEGORY
      CHARACTER(len=3) :: COUNTERPARTY_BOND_RATING
      INTEGER(kind=4) :: TRANSACTION_ID
      INTEGER(kind=2) :: TRANSACTION_GROUP, &
                     REPORTING_GENERATION_GROUP,ASSET_CLASS_ID, &
                     ASSET_ALLOCATION_VECTOR, &
                     DOLLAR_KW_DAY_ESC, &
                     DOLLAR_KW_MONTH_ESC, &
                     DOLLAR_KW_YEAR_ESC, &
                     DOLLAR_MONTH_ESC, &
                     DOLLAR_DEAL_ESC, &
                     MAXIMUM_STRIKES, &
                     BILLING_LAG, &
                     MINIMUM_STRIKES, &
                     UNIT_CONTINGENT_LINK, &
                     USER_DAY_TYPES_ID, &
                     BASE_TRANSACTIONS_IN_FILE=0, &
                     R_TRANSACTIONS_IN_FILES, &
                     MAX_TRANS_GROUP_NUM=0, &
                     R_MAX_TRANS_GROUP_NUM
!
      INTEGER(kind=2) :: R_NUM_OF_TRANS_CLASSES, &
                     R_MAX_TRANS_CLASS_NUM, &
                     R_NUM_OF_ASSET_CLASSES, &
                     R_MAX_ASSET_CLASS_NUM, &
                     NUM_OF_TRANS_CLASSES=0, &
                     MAX_TRANS_CLASS_ID_NUM=0, &
                     NUM_OF_ASSET_CLASSES=0, &
                     MAX_ASSET_CLASS_ID_NUM=0
      INTEGER (KIND=2), ALLOCATABLE :: TRANS_CLASS_POINTER(:), &
                                       ASSET_CLASS_POINTER(:)
      INTEGER(kind=4) :: BEGIN_DATE,END_DATE, &
                     CONTRACT_DATE
      REAL(kind=4) :: QUANTITY_OF_PRODUCT, &
                     ENERGY_PRICE, &
                     ENERGY_PRICE_MULTIPLIER, &
                     MAX_QUANTITY_OF_PRODUCT, &
                     SECOND_ENERGY_PRICE, &
                     DOLLAR_KW_DAY, &
                     DOLLAR_KW_MONTH, &
                     DOLLAR_KW_YEAR, &
                     DOLLAR_MONTH, &
                     DOLLAR_DEAL
      REAL(kind=4) :: DOLLAR_MWH, &
                     ENERGY_MULTIPLIER, &
                     HEAT_RATE_FOR_SPREAD, &
                     PUMPING_CAPACITY, &
                     PUMPING_STORAGE_EFFICIENCY, &
                     DAILY_PUMPING_MULT, &
                     FUEL_PRICE, & !  R*4
                     TRANSPORTATION_BASIS, & !  R*4
                     DELIVERY_ADDER, & !  R*4
                     USER_CF, &
                     RenewableEnergyPercent
!
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER(len=2048) :: RECLN
! DECLARATION FOR DAY TYPE DETERMINANTS
      CHARACTER(len=16) :: FILE_TYPE='Energy Products '
      LOGICAL(kind=1) :: LAHEY_LF95
      CHARACTER(len=30) :: SCREEN_OUTPUT
! MULTI-FILE ADDITION 7/27/04 MSG
      INTEGER (KIND=2) :: FILE_ID,R_MAX_NUM_OF_FILES,R_FILE_ID
      INTEGER (KIND=2), PARAMETER :: MAX_PRODUCTS_FILES=8
      CHARACTER (LEN=2) :: FILE_CODES(0:MAX_PRODUCTS_FILES-1)= &
                            (/'TP','P0','P1','P2','P3','P4','P5','P6'/)
      CHARACTER (LEN=13) :: BINARY_FILE_NAMES(0:MAX_PRODUCTS_FILES-1)= &
                                             (/'ENRG_PRODS_TP', &
                                              'ENRG_PRODS_P0', &
                                              'ENRG_PRODS_P1', &
                                              'ENRG_PRODS_P2', &
                                              'ENRG_PRODS_P3', &
                                              'ENRG_PRODS_P4', &
                                              'ENRG_PRODS_P5', &
                                              'ENRG_PRODS_P6'/)
      CHARACTER (LEN=5) :: BASE_FILE_NAMES(0:MAX_PRODUCTS_FILES-1)
      CHARACTER(LEN=2)::ENERGY_PRODUCTS_OL(0:MAX_PRODUCTS_FILES-1)= &
                                                                 'BC'
      LOGICAL :: ACTIVE_BASE_FILES(0:MAX_PRODUCTS_FILES-1)=.FALSE., &
              ACTIVE_OVERLAY_FILES(0:MAX_PRODUCTS_FILES-1)=.FALSE., &
              FILES_ARE_ACTIVE=.FALSE.
      INTEGER (KIND=4) :: TRANSACTIONS_IN_FILE(0:MAX_PRODUCTS_FILES-1)= &
                                                                      0, &
                  OVERLAY_TRANSACTIONS_IN_FILE(0:MAX_PRODUCTS_FILES-1)= &
                                                                       0
      CHARACTER*(*) OVERLAY_FILE_NAMES(0:*)
      LOGICAL (KIND=1) :: OVERLAY_FILE_EXISTS(0:*),R_ACTIVE_FILE
! VARIABLES FOR REPORTING
      REAL(kind=4) :: CAPACITY_PLANNING_FACTOR
      CHARACTER(len=6) :: UNIT_CATEGORY,FUEL_TYPE
!
      CHARACTER(len=1) :: UNIT_CONTING_MARKET_SUPPLEMENT
!
! ADDED 090706.
!
! 13+64+12 = 89
!
         CHARACTER (LEN=1)  :: IS_AN_EXCHANGE_AGREEMENT
         CHARACTER (LEN=10) :: EXCHANGE_TYPE
         INTEGER (KIND=2)   :: JOINT_TRANSACTION_ID ! 13
         CHARACTER (LEN=3)  :: RESERVES_INCLUDED  ! Yes, No
         CHARACTER (LEN=5) :: DELIVERY_ZONE  ! East, West
         REAL (KIND=4) :: ANNUAL_ENERGY_PLANNING_FACTOR
         REAL (KIND=4) :: PLANNING_FACTOR, &
                          ENERGY_RETURN_LATER_IN_DAY, &
                          ENERGY_RETURN_MW, &
                          MINIMUM_ENERGY_LIMIT_MW(3), &
                          MAXIMUM_ENERGY_LIMIT_MW(3), &
                          UnitConstructionCostUSDKW, &
                          ConstructionCostEscalation, &
                          Year1CapitalRecoveryRate, &
                          Year1RecoveryRateEscalation, &
                          BookandServiceLife ! 16*4
         INTEGER (KIND=2)   :: FirstYearAvailable, &
                               LastYearAvailable, &
                               MinimumAnnualUnits, &
                               MaximumAnnualUnits, &
                               MaximumCumulativeUnits, &
                               ContinuousBuildFirstYear ! 6*2
!      MODULE LMP_InputVariables *** from LMPModules.F95
         CHARACTER (LEN=2) :: MarketHubID
         CHARACTER (LEN=5) :: CalCongestion,LMPActive
         CHARACTER (LEN=3) :: LMPScenarioNum
         CHARACTER (LEN=22) :: IncExpenseAssignment
         REAL (KIND=4) :: LMPPeakSlope,LMPPeakConst,LMPOffPeakSlope, &
                          LMPOffPeakConst,DailyStorageDischargeLimit

         INTEGER (KIND=2) :: RPS_PROGRAM_NUMBER_ord
         CHARACTER (LEN=22) :: IncRevenueAssignment
         INTEGER (KIND=4) :: CapMarketPointer
         CHARACTER (LEN=5) :: CapMarketType
         CHARACTER (LEN=7) :: CapMarketMonth
         CHARACTER (LEN=17) :: CapExpenseCollection
         REAL (KIND=4) :: CapAdjustmentFactor,CapPlanningFactor
! CONVERT THE DAY_TYPE FILE

      ENTRY ENERGY_PRODUCTS_MAKEBIN

      BASE_FILE_NAME = ENERGY_PRODUCTS_FILE(BASE_FILE_NAMES)
      DATA_DRIVE = OUTPUT_DIRECTORY()

      ALLOCATE(TRANS_CLASS_POINTER(0:1023), &
               ASSET_CLASS_POINTER(0:1023))
      TRANS_CLASS_POINTER = 0
      ASSET_CLASS_POINTER = 0
!
      DO FILE_ID = 0, MAX_PRODUCTS_FILES-1
         BASE_FILE_NAME = BASE_FILE_NAMES(FILE_ID)
         FILE_NAME = trim(BASE_FILE_DIRECTORY())// &
                                     FILE_CODES(FILE_ID)//"B"// &
                                          trim(BASE_FILE_NAME)//".DAT"
         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
         ENERGY_PRODUCTS_FILE_EXISTS = ENERGY_PRODUCTS_FILE_EXISTS .OR. &
                                       FILE_EXISTS
         ACTIVE_BASE_FILES(FILE_ID) = FILE_EXISTS
         IF(FILE_EXISTS) THEN
!
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
!
            OPEN(10,FILE=FILE_NAME)
            OPEN(11,FILE=trim(DATA_DRIVE)//"BC"// &
                          trim(BINARY_FILE_NAMES(FILE_ID))//".BIN", &
                            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
            READ(10,*) DELETE
!
            IREC = 0
            PRODUCT_ACTIVE = 'T'
            STRIKE_FREQUENCY = 'D'
            MAXIMUM_STRIKES = 9999
            REPORTING_GENERATION_GROUP = 0
            BILLING_LAG = 0
            MINIMUM_STRIKES = 0
            REPORT_PRODUCT = 'T'
            UNIT_CONTINGENCY = 'N'
            ENERGY_MULTIPLIER = 1.0
            HEAT_RATE_FOR_SPREAD = 0.
            USER_DAY_TYPES_ID = 0
            PUMPING_CAPACITY = 0.
            PUMPING_STORAGE_EFFICIENCY = 0.
            DAILY_PUMPING_MULT = 1.
!
            FUEL_TYPE = 'G'
            FUEL_PRICE_TYPE = 'F'
            FUEL_PRICE = 0.0
            TRANSPORTATION_BASIS_TYPE = 'F'
            TRANSPORTATION_BASIS = 0.0
            DELIVERY_ADDER = 0.0
!
            COUNTERPARTY_BOND_RATING = 'ZZZ'
            DOLLAR_MWH = 0.0
            WVPA_RATE_TRACKER = 'N'
            WVPA_MEM_TRACKER = 'M'
!
            ENERGY_PRICE_MULTIPLIER = 1.0
            MAX_QUANTITY_OF_PRODUCT = 999999.0
!
            CAPACITY_PLANNING_FACTOR = 0.0
            UNIT_CATEGORY = 'NA'
            UNIT_CONTING_MARKET_SUPPLEMENT = 'F'
            Tax_Credit_Begin_Date = 0
            Tax_Credit_End_Date = 0
            External_Unit_ID = 0
            Production_Tax_Credit = 0.0
            RPS_Contribution = 0.0
            Percentage_Owned = 100.0
            GRX_ID = 0
            EMISS_MARKET_DATA_PTR = -9999
            State_TG_Key = '      '
            DERIV_GUID = 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
            DERIV_PARENT_ID = 123456789
            DERIV_AGGREGATED_UNIT = 'F'
            DERIV_ZONE_ID = 'ZZZZZZ'
            REFERENCE_LOAD_NAME = 'NONE'
            REFERENCE_LOAD_NUMBER = 0
            GRX_BUILD_VECTOR = 0
            EV_ID_HYBRID_BATTERY = 0
            RenewableEnergyPercent = 0.0
            DailyStorageDischargeLimit = 999999.

!
            DO
!
               READ(10,1000,IOSTAT=IOS) RECLN
               IF(IOS /= 0) EXIT
               IF(RECLN(1:1) == '7') CYCLE
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,ERR=200) DELETE, &
                        TRANSACTION_NAME,COUNTERPARTY_NAME, &
                        TRANSACTION_ID,TRANSACTION_GROUP, &
                        TRANSACTION_TYPE,TRANSACTION_CLASS, &
                        PRODUCT_TYPE, &
                        REPORTING_GENERATION_GROUP, &
                        BEGIN_DATE,END_DATE, &
                        EXPENSE_ASSIGNMENT, &
                        EXPENSE_COLLECTION, &
                        ASSET_CLASS_ID, &
                        ASSET_ALLOCATION_VECTOR, &
                        QUANTITY_OF_PRODUCT, &
                        ENERGY_PRICE, &
                        PRICE_TYPE, &
                        SECOND_ENERGY_PRICE, &
                        DOLLAR_KW_DAY, &
                        DOLLAR_KW_DAY_ESC, &
                        DOLLAR_KW_MONTH, &
                        DOLLAR_KW_MONTH_ESC, &
                        DOLLAR_KW_YEAR, &
                        DOLLAR_KW_YEAR_ESC, &
                        DOLLAR_MONTH, &
                        DOLLAR_MONTH_ESC, &
                        DOLLAR_DEAL, &
                        DOLLAR_DEAL_ESC, &
                        COMMENT, &
                        CONTRACT_DATE, &
                        OPTION_POSITION, &
                        DISTINGUISHING, &
                        PRODUCT_ACTIVE, & !  33
                        STRIKE_FREQUENCY, &
                        MAXIMUM_STRIKES, &
                        BILLING_LAG, &
                        MINIMUM_STRIKES, &
                        REPORT_PRODUCT, & !  38
                        UNIT_CONTINGENCY, &
                        UNIT_CONTINGENT_LINK, &
                        ENERGY_MULTIPLIER, &
                        HEAT_RATE_FOR_SPREAD, &
                        USER_DAY_TYPES_ID, &
                        PUMPING_CAPACITY, &
                        PUMPING_STORAGE_EFFICIENCY, &
                        DAILY_PUMPING_MULT, &
                        FUEL_TYPE, & !  C*1
                        FUEL_PRICE_TYPE, & !  C*1
                        FUEL_PRICE, & !  R*4
                        TRANSPORTATION_BASIS_TYPE, & !  C*1
                        TRANSPORTATION_BASIS, & !  R*4
                        DELIVERY_ADDER, & !  R*4
                        COUNTERPARTY_BOND_RATING, &
                        DOLLAR_MWH, &
                        WVPA_RATE_TRACKER, &
                        ENERGY_PRICE_MULTIPLIER, &
                        MAX_QUANTITY_OF_PRODUCT, &
                        WVPA_MEM_TRACKER, &
                        CAPACITY_PLANNING_FACTOR, &
                        UNIT_CATEGORY, &
                        IS_AN_EXCHANGE_AGREEMENT, &
                        JOINT_TRANSACTION_ID, &
                        EXCHANGE_TYPE, &
                        ANNUAL_ENERGY_PLANNING_FACTOR, & !  64
                        DELIVERY_ZONE,    & !  65 changed from 59
                        ENERGY_RETURN_LATER_IN_DAY, & !  66
                        ENERGY_RETURN_MW,  & !  67
                        MINIMUM_ENERGY_LIMIT_MW, & !  68-70
                        MAXIMUM_ENERGY_LIMIT_MW, & !  71-73
                        RESERVES_INCLUDED,  & !  74 changed from 60
                        UnitConstructionCostUSDKW, &
                        ConstructionCostEscalation, &
                        Year1CapitalRecoveryRate, &
                        Year1RecoveryRateEscalation, &
                        BookandServiceLife, &
                        FirstYearAvailable, &
                        LastYearAvailable, &
                        MinimumAnnualUnits, &
                        MaximumAnnualUnits, &
                        MaximumCumulativeUnits, &
                        ContinuousBuildFirstYear,  & !  85
                        UNIT_CONTING_MARKET_SUPPLEMENT, &
                        New_Build_Unit_Status, &
                        Production_Tax_Credit, &
                        Tax_Credit_Begin_Date, &
                        Tax_Credit_End_Date, & !  90
                        Latin_Hypercube_Dist, &
                        State_TG_Key, &
                        EIA_Plant_ID, &
                        Owner_ID, &
                        Holding_Company_ID, & !  95
                        RPS_Contribution, &
                        NERC_Subregion, &
                        State_Province, &
                        External_Unit_ID, &
                        Fuel_Category, & !  100
                        Percentage_Owned, &
                        GRX_ID, &
                        DERIV_GUID, &
                        DERIV_PARENT_ID, &
                        DERIV_AGGREGATED_UNIT, &
                        DERIV_ZONE_ID, & !  106
                        USER_CF, & !  107
                        CapMarketPointer, &
                        IncRevenueAssignment, &
                        CapExpenseCollection, & !  110
                        CapAdjustmentFactor, &
                        CapPlanningFactor, &
                        LMPActive, &
                        MarketHubID, &
                        LMPPeakSlope,   & !  115
                        LMPPeakConst, &
                        LMPOffPeakSlope, &
                        LMPOffPeakConst, &
                        LMPScenarioNum, &
                        CalCongestion,  & !  120
                        IncExpenseAssignment, &
                        CapMarketType, &
                        CapMarketMonth, &
                        REFERENCE_LOAD_NAME, &
                        REFERENCE_LOAD_NUMBER, &
                        GRX_BUILD_VECTOR, &
                        UNIT_TYPE, &
                        UNIT_TYPE_CATEGORY, &
                        RPS_PROGRAM_NUMBER_ord, &
                        DailyStorageDischargeLimit, &
                        EV_ID_HYBRID_BATTERY, &
                        RenewableEnergyPercent
               IREC = IREC + 1

               IF(TRANS_CLASS_POINTER(TRANSACTION_GROUP) == 0) THEN
                  NUM_OF_TRANS_CLASSES = NUM_OF_TRANS_CLASSES + 1
                  MAX_TRANS_CLASS_ID_NUM = MAX( &
                               MAX_TRANS_CLASS_ID_NUM,TRANSACTION_GROUP)
                  TRANS_CLASS_POINTER(TRANSACTION_GROUP) = 1
               ENDIF
               IF(ASSET_CLASS_POINTER(ASSET_CLASS_ID) == 0) THEN
                  NUM_OF_ASSET_CLASSES = NUM_OF_ASSET_CLASSES + 1
                  MAX_ASSET_CLASS_ID_NUM = MAX( &
                                  MAX_ASSET_CLASS_ID_NUM,ASSET_CLASS_ID)
                  ASSET_CLASS_POINTER(ASSET_CLASS_ID) = 1
               ENDIF
!
               WRITE(11,REC=IREC) DELETE, &
                        TRANSACTION_NAME,COUNTERPARTY_NAME, &
                        TRANSACTION_ID,TRANSACTION_GROUP, &
                        TRANSACTION_TYPE,TRANSACTION_CLASS, &
                        PRODUCT_TYPE, &
                        REPORTING_GENERATION_GROUP, &
                        BEGIN_DATE,END_DATE, &
                        EXPENSE_ASSIGNMENT, &
                        EXPENSE_COLLECTION, &
                        ASSET_CLASS_ID, &
                        ASSET_ALLOCATION_VECTOR, &
                        QUANTITY_OF_PRODUCT, &
                        ENERGY_PRICE, &
                        PRICE_TYPE, &
                        SECOND_ENERGY_PRICE, &
                        DOLLAR_KW_DAY, &
                        DOLLAR_KW_DAY_ESC, &
                        DOLLAR_KW_MONTH, &
                        DOLLAR_KW_MONTH_ESC, &
                        DOLLAR_KW_YEAR, &
                        DOLLAR_KW_YEAR_ESC, &
                        DOLLAR_MONTH, &
                        DOLLAR_MONTH_ESC, &
                        DOLLAR_DEAL, &
                        DOLLAR_DEAL_ESC, &
                        COMMENT, &
                        CONTRACT_DATE, &
                        OPTION_POSITION, &
                        DISTINGUISHING, &
                        PRODUCT_ACTIVE, &
                        STRIKE_FREQUENCY, &
                        MAXIMUM_STRIKES, &
                        BILLING_LAG, &
                        MINIMUM_STRIKES, &
                        REPORT_PRODUCT, & !  38
                        UNIT_CONTINGENCY, &
                        UNIT_CONTINGENT_LINK, &
                        ENERGY_MULTIPLIER, &
                        HEAT_RATE_FOR_SPREAD, &
                        USER_DAY_TYPES_ID, &
                        PUMPING_CAPACITY, &
                        PUMPING_STORAGE_EFFICIENCY, &
                        DAILY_PUMPING_MULT, &
                        FUEL_TYPE, & !  C*1
                        FUEL_PRICE_TYPE, & !  C*1
                        FUEL_PRICE, & !  R*4
                        TRANSPORTATION_BASIS_TYPE, & !  C*1
                        TRANSPORTATION_BASIS, & !  R*4
                        DELIVERY_ADDER, & !  R*4
                        COUNTERPARTY_BOND_RATING, &
                        DOLLAR_MWH, &
                        WVPA_RATE_TRACKER, &
                        ENERGY_PRICE_MULTIPLIER, &
                        MAX_QUANTITY_OF_PRODUCT, &
                        WVPA_MEM_TRACKER, &
                        CAPACITY_PLANNING_FACTOR, &
                        UNIT_CATEGORY, &
                        IS_AN_EXCHANGE_AGREEMENT, &
                        JOINT_TRANSACTION_ID, &
                        EXCHANGE_TYPE, &
                        ANNUAL_ENERGY_PLANNING_FACTOR, & !  64
                        DELIVERY_ZONE,    & !  65 changed from 59
                        ENERGY_RETURN_LATER_IN_DAY, & !  66
                        ENERGY_RETURN_MW,  & !  67
                        MINIMUM_ENERGY_LIMIT_MW, & !  68-70
                        MAXIMUM_ENERGY_LIMIT_MW, & !  71-73
                        RESERVES_INCLUDED,  & !  74 changed from 60
                        UnitConstructionCostUSDKW, &
                        ConstructionCostEscalation, &
                        Year1CapitalRecoveryRate, &
                        Year1RecoveryRateEscalation, &
                        BookandServiceLife, &
                        FirstYearAvailable, &
                        LastYearAvailable, &
                        MinimumAnnualUnits, &
                        MaximumAnnualUnits, &
                        MaximumCumulativeUnits, &
                        ContinuousBuildFirstYear,  & !  85
                        UNIT_CONTING_MARKET_SUPPLEMENT, &
                        New_Build_Unit_Status, &
                        Production_Tax_Credit, &
                        Tax_Credit_Begin_Date, &
                        Tax_Credit_End_Date, & !  90
                        Latin_Hypercube_Dist, &
                        State_TG_Key, &
                        EIA_Plant_ID, &
                        Owner_ID, &
                        Holding_Company_ID, & !  95
                        RPS_Contribution, &
                        NERC_Subregion, &
                        State_Province, &
                        External_Unit_ID, &
                        Fuel_Category, & !  100
                        Percentage_Owned, &
                        GRX_ID, &
                        DERIV_GUID, &
                        DERIV_PARENT_ID, &
                        DERIV_AGGREGATED_UNIT, &
                        DERIV_ZONE_ID, &
                        USER_CF, &
                        CapMarketPointer, &
                        IncRevenueAssignment, &
                        CapExpenseCollection, & !  110
                        CapAdjustmentFactor, &
                        CapPlanningFactor, &
                        LMPActive, &
                        MarketHubID, &
                        LMPPeakSlope,   & !  115
                        LMPPeakConst, &
                        LMPOffPeakSlope, &
                        LMPOffPeakConst, &
                        LMPScenarioNum, &
                        CalCongestion,  & !  120
                        IncExpenseAssignment, &
                        CapMarketType, &
                        CapMarketMonth, &
                        REFERENCE_LOAD_NAME, &
                        REFERENCE_LOAD_NUMBER, &
                        GRX_BUILD_VECTOR, &
                        UNIT_TYPE, &
                        UNIT_TYPE_CATEGORY, &
                        RPS_PROGRAM_NUMBER_ord, &
                        DailyStorageDischargeLimit, &
                        EV_ID_HYBRID_BATTERY, &
                        RenewableEnergyPercent
               MAX_TRANS_GROUP_NUM = MAX(MAX_TRANS_GROUP_NUM, &
                                                      TRANSACTION_GROUP)
            ENDDO ! RECORDS
            CLOSE(10)
            CLOSE(11)
!
            TRANSACTIONS_IN_FILE(FILE_ID) = IREC
!
         ELSE IF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
            CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "transobj2:11110")
         ENDIF
      ENDDO ! FILES
      RETURN
! OVERLAY THE DAY TYPE FILE

      ENTRY ENERGY_PRODUCTS_MAKEOVL(OVERLAY_FILE_NAMES, &
                                    OVERLAY_FILE_EXISTS)

!
      DATA_DRIVE = OUTPUT_DIRECTORY()
      OVERLAY_TRANSACTIONS_IN_FILE = TRANSACTIONS_IN_FILE
      DO FILE_ID = 0, MAX_PRODUCTS_FILES-1
         IF(.NOT. ACTIVE_BASE_FILES(FILE_ID)) CYCLE
         IF(.NOT. OVERLAY_FILE_EXISTS(FILE_ID)) CYCLE
         OVERLAY_FAMILY_NAME = OVERLAY_FILE_NAMES(FILE_ID)
         SCREEN_OUTPUT = TRIM(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)

         file_name=get_o_filename(data_drive, file_codes(file_id), &
           OVERLAY_FAMILY_NAME)
         OPEN(10,FILE=FILE_NAME)
         READ(10,*) DELETE
         INUNIT = 12
         IF(ENERGY_PRODUCTS_OL(FILE_ID) == 'BC') THEN
            OPEN(11,FILE=trim(DATA_DRIVE)//"BC"// &
                          trim(BINARY_FILE_NAMES(FILE_ID))//".BIN", &
                                                        ACCESS="DIRECT", &
                                                             RECL=LRECL)
            INUNIT = 11
         ENDIF
         OPEN(12,FILE=trim(DATA_DRIVE)//"OL"// &
                             trim(BINARY_FILE_NAMES(FILE_ID))//".BIN", &
                            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         IREC = 0
!
         READ(10,1000,IOSTAT=IOS) RECLN
         DOWHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE
!
            READ(10,1000,IOSTAT=IOS) RECLN
         ENDDO
!
         DO
            IF(IOS /= 0) EXIT
            IREC = IREC + 1
            READ(INUNIT,REC=IREC,IOSTAT=IOS) DELETE, &
                        TRANSACTION_NAME,COUNTERPARTY_NAME, &
                        TRANSACTION_ID,TRANSACTION_GROUP, &
                        TRANSACTION_TYPE,TRANSACTION_CLASS, &
                        PRODUCT_TYPE, &
                        REPORTING_GENERATION_GROUP, &
                        BEGIN_DATE,END_DATE, &
                        EXPENSE_ASSIGNMENT, &
                        EXPENSE_COLLECTION, &
                        ASSET_CLASS_ID, &
                        ASSET_ALLOCATION_VECTOR, &
                        QUANTITY_OF_PRODUCT, &
                        ENERGY_PRICE, &
                        PRICE_TYPE, &
                        SECOND_ENERGY_PRICE, &
                        DOLLAR_KW_DAY, &
                        DOLLAR_KW_DAY_ESC, &
                        DOLLAR_KW_MONTH, &
                        DOLLAR_KW_MONTH_ESC, &
                        DOLLAR_KW_YEAR, &
                        DOLLAR_KW_YEAR_ESC, &
                        DOLLAR_MONTH, &
                        DOLLAR_MONTH_ESC, &
                        DOLLAR_DEAL, &
                        DOLLAR_DEAL_ESC, &
                        COMMENT, &
                        CONTRACT_DATE, &
                        OPTION_POSITION, &
                        DISTINGUISHING, &
                        PRODUCT_ACTIVE, &
                        STRIKE_FREQUENCY, &
                        MAXIMUM_STRIKES, &
                        BILLING_LAG, &
                        MINIMUM_STRIKES, &
                        REPORT_PRODUCT, & !  38
                        UNIT_CONTINGENCY, &
                        UNIT_CONTINGENT_LINK, &
                        ENERGY_MULTIPLIER, &
                        HEAT_RATE_FOR_SPREAD, &
                        USER_DAY_TYPES_ID, &
                        PUMPING_CAPACITY, &
                        PUMPING_STORAGE_EFFICIENCY, &
                        DAILY_PUMPING_MULT, &
                        FUEL_TYPE, & !  C*1
                        FUEL_PRICE_TYPE, & !  C*1
                        FUEL_PRICE, & !  R*4
                        TRANSPORTATION_BASIS_TYPE, & !  C*1
                        TRANSPORTATION_BASIS, & !  R*4
                        DELIVERY_ADDER, & !  R*4
                        COUNTERPARTY_BOND_RATING, &
                        DOLLAR_MWH, &
                        WVPA_RATE_TRACKER, &
                        ENERGY_PRICE_MULTIPLIER, &
                        MAX_QUANTITY_OF_PRODUCT, &
                        WVPA_MEM_TRACKER, &
                        CAPACITY_PLANNING_FACTOR, &
                        UNIT_CATEGORY, &
                        IS_AN_EXCHANGE_AGREEMENT, &
                        JOINT_TRANSACTION_ID, &
                        EXCHANGE_TYPE, &
                        ANNUAL_ENERGY_PLANNING_FACTOR, & !  64
                        DELIVERY_ZONE,    & !  65 changed from 59
                        ENERGY_RETURN_LATER_IN_DAY, & !  66
                        ENERGY_RETURN_MW,  & !  67
                        MINIMUM_ENERGY_LIMIT_MW, & !  68-70
                        MAXIMUM_ENERGY_LIMIT_MW, & !  71-73
                        RESERVES_INCLUDED,  & !  74 changed from 60
                        UnitConstructionCostUSDKW, &
                        ConstructionCostEscalation, &
                        Year1CapitalRecoveryRate, &
                        Year1RecoveryRateEscalation, &
                        BookandServiceLife, &
                        FirstYearAvailable, &
                        LastYearAvailable, &
                        MinimumAnnualUnits, &
                        MaximumAnnualUnits, &
                        MaximumCumulativeUnits, &
                        ContinuousBuildFirstYear,  & !  85
                        UNIT_CONTING_MARKET_SUPPLEMENT, &
                        New_Build_Unit_Status, &
                        Production_Tax_Credit, &
                        Tax_Credit_Begin_Date, &
                        Tax_Credit_End_Date, & !  90
                        Latin_Hypercube_Dist, &
                        State_TG_Key, &
                        EIA_Plant_ID, &
                        Owner_ID, &
                        Holding_Company_ID, & !  95
                        RPS_Contribution, &
                        NERC_Subregion, &
                        State_Province, &
                        External_Unit_ID, &
                        Fuel_Category, & !  100
                        Percentage_Owned, &
                        GRX_ID, &
                        DERIV_GUID, &
                        DERIV_PARENT_ID, &
                        DERIV_AGGREGATED_UNIT, &
                        DERIV_ZONE_ID, &
                        USER_CF, &
                        CapMarketPointer, &
                        IncRevenueAssignment, &
                        CapExpenseCollection, & !  110
                        CapAdjustmentFactor, &
                        CapPlanningFactor, &
                        LMPActive, &
                        MarketHubID, &
                        LMPPeakSlope,   & !  115
                        LMPPeakConst, &
                        LMPOffPeakSlope, &
                        LMPOffPeakConst, &
                        LMPScenarioNum, &
                        CalCongestion,  & !  120
                        IncExpenseAssignment, &
                        CapMarketType, &
                        CapMarketMonth, &
                        REFERENCE_LOAD_NAME, &
                        REFERENCE_LOAD_NUMBER, &
                        GRX_BUILD_VECTOR, &
                        UNIT_TYPE, &
                        UNIT_TYPE_CATEGORY, &
                        RPS_PROGRAM_NUMBER_ord, &
                        DailyStorageDischargeLimit, &
                        EV_ID_HYBRID_BATTERY, &
                        RenewableEnergyPercent
            IF(IOS /= 0) EXIT
!         READ(10,1000,IOSTAT=IOS) RECLN
!         IF(IOS == 0) THEN
               RECLN = TRIM(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,ERR=300) DELETE, &
                     TRANSACTION_NAME,COUNTERPARTY_NAME, &
                     TRANSACTION_ID,TRANSACTION_GROUP, &
                     TRANSACTION_TYPE,TRANSACTION_CLASS, &
                     PRODUCT_TYPE, &
                     REPORTING_GENERATION_GROUP, &
                     BEGIN_DATE,END_DATE, &
                     EXPENSE_ASSIGNMENT, &
                     EXPENSE_COLLECTION, &
                     ASSET_CLASS_ID, &
                     ASSET_ALLOCATION_VECTOR, &
                     QUANTITY_OF_PRODUCT, &
                     ENERGY_PRICE, &
                     PRICE_TYPE, &
                     SECOND_ENERGY_PRICE, &
                     DOLLAR_KW_DAY, &
                     DOLLAR_KW_DAY_ESC, &
                     DOLLAR_KW_MONTH, &
                     DOLLAR_KW_MONTH_ESC, &
                     DOLLAR_KW_YEAR, &
                     DOLLAR_KW_YEAR_ESC, &
                     DOLLAR_MONTH, &
                     DOLLAR_MONTH_ESC, &
                     DOLLAR_DEAL, &
                     DOLLAR_DEAL_ESC, &
                     COMMENT, &
                     CONTRACT_DATE, &
                     OPTION_POSITION, &
                     DISTINGUISHING, &
                     PRODUCT_ACTIVE, &
                     STRIKE_FREQUENCY, &
                     MAXIMUM_STRIKES, &
                     BILLING_LAG, &
                     MINIMUM_STRIKES, &
                     REPORT_PRODUCT, & !  38
                     UNIT_CONTINGENCY, &
                     UNIT_CONTINGENT_LINK, &
                     ENERGY_MULTIPLIER, &
                     HEAT_RATE_FOR_SPREAD, &
                     USER_DAY_TYPES_ID, &
                     PUMPING_CAPACITY, &
                     PUMPING_STORAGE_EFFICIENCY, &
                     DAILY_PUMPING_MULT, &
                     FUEL_TYPE, & !  C*1
                     FUEL_PRICE_TYPE, & !  C*1
                     FUEL_PRICE, & !  R*4
                     TRANSPORTATION_BASIS_TYPE, & !  C*1
                     TRANSPORTATION_BASIS, & !  R*4
                     DELIVERY_ADDER, & !  R*4
                     COUNTERPARTY_BOND_RATING, &
                     DOLLAR_MWH, &
                     WVPA_RATE_TRACKER, &
                     ENERGY_PRICE_MULTIPLIER, &
                     MAX_QUANTITY_OF_PRODUCT, &
                     WVPA_MEM_TRACKER, &
                     CAPACITY_PLANNING_FACTOR, &
                        UNIT_CATEGORY, &
                        IS_AN_EXCHANGE_AGREEMENT, &
                        JOINT_TRANSACTION_ID, &
                        EXCHANGE_TYPE, &
                        ANNUAL_ENERGY_PLANNING_FACTOR, & !  64
                        DELIVERY_ZONE,    & !  65 changed from 59
                        ENERGY_RETURN_LATER_IN_DAY, & !  66
                        ENERGY_RETURN_MW,  & !  67
                        MINIMUM_ENERGY_LIMIT_MW, & !  68-70
                        MAXIMUM_ENERGY_LIMIT_MW, & !  71-73
                        RESERVES_INCLUDED,  & !  74 changed from 60
                        UnitConstructionCostUSDKW, &
                        ConstructionCostEscalation, &
                        Year1CapitalRecoveryRate, &
                        Year1RecoveryRateEscalation, &
                        BookandServiceLife, &
                        FirstYearAvailable, &
                        LastYearAvailable, &
                        MinimumAnnualUnits, &
                        MaximumAnnualUnits, &
                        MaximumCumulativeUnits, &
                        ContinuousBuildFirstYear,  & !  85
                        UNIT_CONTING_MARKET_SUPPLEMENT, &
                        New_Build_Unit_Status, &
                        Production_Tax_Credit, &
                        Tax_Credit_Begin_Date, &
                        Tax_Credit_End_Date, & !  90
                        Latin_Hypercube_Dist, &
                        State_TG_Key, &
                        EIA_Plant_ID, &
                        Owner_ID, &
                        Holding_Company_ID, & !  95
                        RPS_Contribution, &
                        NERC_Subregion, &
                        State_Province, &
                        External_Unit_ID, &
                        Fuel_Category, & !  100
                        Percentage_Owned, &
                        GRX_ID, &
                        DERIV_GUID, &
                        DERIV_PARENT_ID, &
                        DERIV_AGGREGATED_UNIT, &
                        DERIV_ZONE_ID, &
                        USER_CF, &
                        CapMarketPointer, &
                        IncRevenueAssignment, &
                        CapExpenseCollection, & !  110
                        CapAdjustmentFactor, &
                        CapPlanningFactor, &
                        LMPActive, &
                        MarketHubID, &
                        LMPPeakSlope,   & !  115
                        LMPPeakConst, &
                        LMPOffPeakSlope, &
                        LMPOffPeakConst, &
                        LMPScenarioNum, &
                        CalCongestion,  & !  120
                        IncExpenseAssignment, &
                        CapMarketType, &
                        CapMarketMonth, &
                        REFERENCE_LOAD_NAME, &
                        REFERENCE_LOAD_NUMBER, &
                        GRX_BUILD_VECTOR, &
                        UNIT_TYPE, &
                        UNIT_TYPE_CATEGORY, &
                        RPS_PROGRAM_NUMBER_ord, &
                        DailyStorageDischargeLimit, &
                        EV_ID_HYBRID_BATTERY, &
                        RenewableEnergyPercent
            IF(IOS /= 0) EXIT
!         READ(10,1000,IOSTAT=IOS) RECLN
!         IF(IOS == 0) THEN
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,ERR=300) DELETE, &
                     TRANSACTION_NAME,COUNTERPARTY_NAME, &
                     TRANSACTION_ID,TRANSACTION_GROUP, &
                     TRANSACTION_TYPE,TRANSACTION_CLASS, &
                     PRODUCT_TYPE, &
                     REPORTING_GENERATION_GROUP, &
                     BEGIN_DATE,END_DATE, &
                     EXPENSE_ASSIGNMENT, &
                     EXPENSE_COLLECTION, &
                     ASSET_CLASS_ID, &
                     ASSET_ALLOCATION_VECTOR, &
                     QUANTITY_OF_PRODUCT, &
                     ENERGY_PRICE, &
                     PRICE_TYPE, &
                     SECOND_ENERGY_PRICE, &
                     DOLLAR_KW_DAY, &
                     DOLLAR_KW_DAY_ESC, &
                     DOLLAR_KW_MONTH, &
                     DOLLAR_KW_MONTH_ESC, &
                     DOLLAR_KW_YEAR, &
                     DOLLAR_KW_YEAR_ESC, &
                     DOLLAR_MONTH, &
                     DOLLAR_MONTH_ESC, &
                     DOLLAR_DEAL, &
                     DOLLAR_DEAL_ESC, &
                     COMMENT, &
                     CONTRACT_DATE, &
                     OPTION_POSITION, &
                     DISTINGUISHING, &
                     PRODUCT_ACTIVE, &
                     STRIKE_FREQUENCY, &
                     MAXIMUM_STRIKES, &
                     BILLING_LAG, &
                     MINIMUM_STRIKES, &
                     REPORT_PRODUCT, & !  38
                     UNIT_CONTINGENCY, &
                     UNIT_CONTINGENT_LINK, &
                     ENERGY_MULTIPLIER, &
                     HEAT_RATE_FOR_SPREAD, &
                     USER_DAY_TYPES_ID, &
                     PUMPING_CAPACITY, &
                     PUMPING_STORAGE_EFFICIENCY, &
                     DAILY_PUMPING_MULT, &
                     FUEL_TYPE, & !  C*1
                     FUEL_PRICE_TYPE, & !  C*1
                     FUEL_PRICE, & !  R*4
                     TRANSPORTATION_BASIS_TYPE, & !  C*1
                     TRANSPORTATION_BASIS, & !  R*4
                     DELIVERY_ADDER, & !  R*4
                     COUNTERPARTY_BOND_RATING, &
                     DOLLAR_MWH, &
                     WVPA_RATE_TRACKER, &
                     ENERGY_PRICE_MULTIPLIER, &
                     MAX_QUANTITY_OF_PRODUCT, &
                     WVPA_MEM_TRACKER, &
                     CAPACITY_PLANNING_FACTOR, &
                        UNIT_CATEGORY, &
                        IS_AN_EXCHANGE_AGREEMENT, &
                        JOINT_TRANSACTION_ID, &
                        EXCHANGE_TYPE, &
                        ANNUAL_ENERGY_PLANNING_FACTOR, & !  64
                        DELIVERY_ZONE,    & !  65 changed from 59
                        ENERGY_RETURN_LATER_IN_DAY, & !  66
                        ENERGY_RETURN_MW,  & !  67
                        MINIMUM_ENERGY_LIMIT_MW, & !  68-70
                        MAXIMUM_ENERGY_LIMIT_MW, & !  71-73
                        RESERVES_INCLUDED,  & !  74 changed from 60
                        UnitConstructionCostUSDKW, &
                        ConstructionCostEscalation, &
                        Year1CapitalRecoveryRate, &
                        Year1RecoveryRateEscalation, &
                        BookandServiceLife, &
                        FirstYearAvailable, &
                        LastYearAvailable, &
                        MinimumAnnualUnits, &
                        MaximumAnnualUnits, &
                        MaximumCumulativeUnits, &
                        ContinuousBuildFirstYear,  & !  85
                        UNIT_CONTING_MARKET_SUPPLEMENT, &
                        New_Build_Unit_Status, &
                        Production_Tax_Credit, &
                        Tax_Credit_Begin_Date, &
                        Tax_Credit_End_Date, & !  90
                        Latin_Hypercube_Dist, &
                        State_TG_Key, &
                        EIA_Plant_ID, &
                        Owner_ID, &
                        Holding_Company_ID, & !  95
                        RPS_Contribution, &
                        NERC_Subregion, &
                        State_Province, &
                        External_Unit_ID, &
                        Fuel_Category, & !  100
                        Percentage_Owned, &
                        GRX_ID, &
                        DERIV_GUID, &
                        DERIV_PARENT_ID, &
                        DERIV_AGGREGATED_UNIT, &
                        DERIV_ZONE_ID, &
                        USER_CF, &
                        CapMarketPointer, &
                        IncRevenueAssignment, &
                        CapExpenseCollection, & !  110
                        CapAdjustmentFactor, &
                        CapPlanningFactor, &
                        LMPActive, &
                        MarketHubID, &
                        LMPPeakSlope,   & !  115
                        LMPPeakConst, &
                        LMPOffPeakSlope, &
                        LMPOffPeakConst, &
                        LMPScenarioNum, &
                        CalCongestion,  & !  120
                        IncExpenseAssignment, &
                        CapMarketType, &
                        CapMarketMonth, &
                        REFERENCE_LOAD_NAME, &
                        REFERENCE_LOAD_NUMBER, &
                        GRX_BUILD_VECTOR, &
                        UNIT_TYPE, &
                        UNIT_TYPE_CATEGORY, &
                        RPS_PROGRAM_NUMBER_ord, &
                        DailyStorageDischargeLimit, &
                        EV_ID_HYBRID_BATTERY, &
                        RenewableEnergyPercent
!         ENDIF
!
            READ(10,1000,IOSTAT=IOS) RECLN
            DOWHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE
!
               READ(10,1000,IOSTAT=IOS) RECLN
            ENDDO
!
            IF(TRANS_CLASS_POINTER(TRANSACTION_GROUP) == 0) THEN
               NUM_OF_TRANS_CLASSES = NUM_OF_TRANS_CLASSES + 1
               MAX_TRANS_CLASS_ID_NUM = MAX( &
                            MAX_TRANS_CLASS_ID_NUM,TRANSACTION_GROUP)
               TRANS_CLASS_POINTER(TRANSACTION_GROUP) = 1
            ENDIF
            IF(ASSET_CLASS_POINTER(ASSET_CLASS_ID) == 0) THEN
               NUM_OF_ASSET_CLASSES = NUM_OF_ASSET_CLASSES + 1
               MAX_ASSET_CLASS_ID_NUM = MAX( &
                                  MAX_ASSET_CLASS_ID_NUM,ASSET_CLASS_ID)
               ASSET_CLASS_POINTER(ASSET_CLASS_ID) = 1
            ENDIF
!
            WRITE(12,REC=IREC) DELETE, &
                     TRANSACTION_NAME,COUNTERPARTY_NAME, &
                     TRANSACTION_ID,TRANSACTION_GROUP, &
                     TRANSACTION_TYPE,TRANSACTION_CLASS, &
                     PRODUCT_TYPE, &
                     REPORTING_GENERATION_GROUP, &
                     BEGIN_DATE,END_DATE, &
                     EXPENSE_ASSIGNMENT, &
                     EXPENSE_COLLECTION, &
                     ASSET_CLASS_ID, &
                     ASSET_ALLOCATION_VECTOR, &
                     QUANTITY_OF_PRODUCT, &
                     ENERGY_PRICE, &
                     PRICE_TYPE, &
                     SECOND_ENERGY_PRICE, &
                     DOLLAR_KW_DAY, &
                     DOLLAR_KW_DAY_ESC, &
                     DOLLAR_KW_MONTH, &
                     DOLLAR_KW_MONTH_ESC, &
                     DOLLAR_KW_YEAR, &
                     DOLLAR_KW_YEAR_ESC, &
                     DOLLAR_MONTH, &
                     DOLLAR_MONTH_ESC, &
                     DOLLAR_DEAL, &
                     DOLLAR_DEAL_ESC, &
                     COMMENT, &
                     CONTRACT_DATE, &
                     OPTION_POSITION, &
                     DISTINGUISHING, &
                     PRODUCT_ACTIVE, &
                     STRIKE_FREQUENCY, &
                     MAXIMUM_STRIKES, &
                     BILLING_LAG, &
                     MINIMUM_STRIKES, &
                     REPORT_PRODUCT, & !  38
                     UNIT_CONTINGENCY, &
                     UNIT_CONTINGENT_LINK, &
                     ENERGY_MULTIPLIER, &
                     HEAT_RATE_FOR_SPREAD, &
                     USER_DAY_TYPES_ID, &
                     PUMPING_CAPACITY, &
                     PUMPING_STORAGE_EFFICIENCY, &
                     DAILY_PUMPING_MULT, &
                     FUEL_TYPE, & !  C*1
                     FUEL_PRICE_TYPE, & !  C*1
                     FUEL_PRICE, & !  R*4
                     TRANSPORTATION_BASIS_TYPE, & !  C*1
                     TRANSPORTATION_BASIS, & !  R*4
                     DELIVERY_ADDER, & !  R*4
                     COUNTERPARTY_BOND_RATING, &
                     DOLLAR_MWH, &
                     WVPA_RATE_TRACKER, &
                     ENERGY_PRICE_MULTIPLIER, &
                     MAX_QUANTITY_OF_PRODUCT, &
                     WVPA_MEM_TRACKER, &
                     CAPACITY_PLANNING_FACTOR, &
                        UNIT_CATEGORY, &
                        IS_AN_EXCHANGE_AGREEMENT, &
                        JOINT_TRANSACTION_ID, &
                        EXCHANGE_TYPE, &
                        ANNUAL_ENERGY_PLANNING_FACTOR, & !  64
                        DELIVERY_ZONE,    & !  65 changed from 59
                        ENERGY_RETURN_LATER_IN_DAY, & !  66
                        ENERGY_RETURN_MW,  & !  67
                        MINIMUM_ENERGY_LIMIT_MW, & !  68-70
                        MAXIMUM_ENERGY_LIMIT_MW, & !  71-73
                        RESERVES_INCLUDED,  & !  74 changed from 60
                        UnitConstructionCostUSDKW, &
                        ConstructionCostEscalation, &
                        Year1CapitalRecoveryRate, &
                        Year1RecoveryRateEscalation, &
                        BookandServiceLife, &
                        FirstYearAvailable, &
                        LastYearAvailable, &
                        MinimumAnnualUnits, &
                        MaximumAnnualUnits, &
                        MaximumCumulativeUnits, &
                        ContinuousBuildFirstYear,  & !  85
                        UNIT_CONTING_MARKET_SUPPLEMENT, &
                        New_Build_Unit_Status, &
                        Production_Tax_Credit, &
                        Tax_Credit_Begin_Date, &
                        Tax_Credit_End_Date, & !  90
                        Latin_Hypercube_Dist, &
                        State_TG_Key, &
                        EIA_Plant_ID, &
                        Owner_ID, &
                        Holding_Company_ID, & !  95
                        RPS_Contribution, &
                        NERC_Subregion, &
                        State_Province, &
                        External_Unit_ID, &
                        Fuel_Category, & !  100
                        Percentage_Owned, &
                        GRX_ID, &
                        DERIV_GUID, &
                        DERIV_PARENT_ID, &
                        DERIV_AGGREGATED_UNIT, &
                        DERIV_ZONE_ID, &
                        USER_CF, &
                        CapMarketPointer, &
                        IncRevenueAssignment, &
                        CapExpenseCollection, & !  110
                        CapAdjustmentFactor, &
                        CapPlanningFactor, &
                        LMPActive, &
                        MarketHubID, &
                        LMPPeakSlope,   & !  115
                        LMPPeakConst, &
                        LMPOffPeakSlope, &
                        LMPOffPeakConst, &
                        LMPScenarioNum, &
                        CalCongestion,  & !  120
                        IncExpenseAssignment, &
                        CapMarketType, &
                        CapMarketMonth, &
                        REFERENCE_LOAD_NAME, &
                        REFERENCE_LOAD_NUMBER, &
                        GRX_BUILD_VECTOR, &
                        UNIT_TYPE, &
                        UNIT_TYPE_CATEGORY, &
                       RPS_PROGRAM_NUMBER_ord, &
                        DailyStorageDischargeLimit, &
                        EV_ID_HYBRID_BATTERY, &
                        RenewableEnergyPercent
            MAX_TRANS_GROUP_NUM = MAX(MAX_TRANS_GROUP_NUM, &
                                                      TRANSACTION_GROUP)
         ENDDO
         CLOSE(10)
         CLOSE(12)
!
         OVERLAY_TRANSACTIONS_IN_FILE(FILE_ID) = IREC
!
         IF(ENERGY_PRODUCTS_OL(FILE_ID) == 'BC') CLOSE(11)
         ENERGY_PRODUCTS_OL(FILE_ID) = 'OL'
!
      ENDDO ! FILES
!
      RETURN
!
  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from TRANSOBJ2 SIID318'
      call end_program(er_message)
  300 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      CALL MG_LOCATE_WRITE(21,0, &
                  'Error reading the Energy Products record. Look for'// &
                                          ' a "," in a character name.', &
                           ALL_VERSIONS,1)
      er_message='stop requested from TRANSOBJ2 SIID319'
      call end_program(er_message)
!

      ENTRY RESET_ENERGY_PRODUCTS_OL

         ENERGY_PRODUCTS_OL = 'BC'
      RETURN
!

      ENTRY OPEN_ENERGY_PRODUCTS_FILE(R_FILE_ID,R_ACTIVE_FILE)

         R_ACTIVE_FILE = ACTIVE_BASE_FILES(R_FILE_ID)
         IF(ACTIVE_BASE_FILES(R_FILE_ID)) THEN
            OPEN(UNIT_NUM, &
               FILE=TRIM(OUTPUT_DIRECTORY())// &
                    ENERGY_PRODUCTS_OL(R_FILE_ID)// &
                             TRIM(BINARY_FILE_NAMES(R_FILE_ID))//".BIN", &
               ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         ENDIF
      RETURN
!

      ENTRY CLOSE_ENERGY_PRODUCTS_FILE

         CLOSE(UNIT_NUM)
      RETURN

      ENTRY DOES_ENER_PRODUCTS_FILE_EXIST(R_MAX_NUM_OF_FILES, &
                                          R_ENERGY_PRODUCTS_FILE_EXISTS)

         R_ENERGY_PRODUCTS_FILE_EXISTS = ENERGY_PRODUCTS_FILE_EXISTS
         R_MAX_NUM_OF_FILES = MAX_PRODUCTS_FILES
      RETURN

      ENTRY GET_BASE_TRANSACTIONS(R_TRANSACTIONS_IN_FILES, &
                                  R_MAX_TRANS_GROUP_NUM)

         R_MAX_TRANS_GROUP_NUM = MAX_TRANS_GROUP_NUM
         R_TRANSACTIONS_IN_FILES = MAX(SUM(TRANSACTIONS_IN_FILE), &
                                      SUM(OVERLAY_TRANSACTIONS_IN_FILE))
      RETURN

      ENTRY RETURN_EP_GROUP_INFO(R_NUM_OF_TRANS_CLASSES, &
                                 R_MAX_TRANS_CLASS_NUM, &
                                 R_NUM_OF_ASSET_CLASSES, &
                                 R_MAX_ASSET_CLASS_NUM)

         R_NUM_OF_TRANS_CLASSES = NUM_OF_TRANS_CLASSES
         R_MAX_TRANS_CLASS_NUM = MAX_TRANS_CLASS_ID_NUM
         R_NUM_OF_ASSET_CLASSES = NUM_OF_ASSET_CLASSES
         R_MAX_ASSET_CLASS_NUM = MAX_ASSET_CLASS_ID_NUM
      RETURN

 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END
!                ROUTINE TO READ ENERGY PRODUCTS FILE

!                         COPYRIGHT (C) 1997
!                    M.S. GERBER & ASSOCIATES, INC.
!                        ALL RIGHTS RESERVED





      RECURSIVE FUNCTION READ_ENERGY_PRODUCTS_DATA()
      
      use end_routine, only: end_program, er_message
      use logging
      use capacity_arrays
      use grx_planning_routines
      use rptreccontrol
      use cla_objt_arrays
      use rps_index_translate_mod
      use arrayallocationinterface
      use hourlyproductsdata
      use readenergyproductsdata
      use prim_mover_idx
      use rps_data
      use dr_booth_modules
      use sizecom
      use globecom
      use month_extra
      use prod_arrays_dimensions
      use spindriftlib
      use mod_base_year
      use tf_objt_interfaces
      use irec_endpoint_control
      use mthnmcom
      use end_routine
      use transobj2_static
      use rangechecker
      use program_state
      use filemod
      
      use debugtrace
      use energy_trace

      SAVE
      
      
      

      integer :: file_trace_ass=0
!     CALCULATE_MONTHLY_TRANS_GROUP
      integer (kind=2) :: ilocyear 
      integer (kind=4) :: dow_pass ! Pass r_day_of_week
      integer (kind=2) :: month_pass ! access violation
      integer (kind=2) :: gud_pass ! Access violation
      
      
      integer :: file_trace_cxdo=0 !CX_DailyOperAnPS2
      integer :: times_cmtg_entered=0
      LOGICAL(kind=1) :: READ_ENERGY_PRODUCTS_DATA, &
                     MULTI_YEAR_ENERGY_PRODUCTS, &
                     GET_MONTHLY_TRANS_VARIABLES, &
                     MONTHLY_ACTIVE_ENERGY_PRODUCTS, &
                     GET_MONTHLY_CT_GROUP_REPORT, &
                     CALCULATE_MONTHLY_TRANS_GROUP, &
                     GET_TRANS_RPS_SUM, &
                     GET_TRANS_GRX_RPS_SUM, &
                     UPDATE_SEASON_FOR_CT_RPT, &
                     SAVE_ENERGY_PRODUCTS_STATUS=.FALSE., &
                     SAVE_MONTHLY_PRODUCTS_STATUS=.FALSE., &
                     IS_TRANS_RESOURCE_ACTIVE, &
                     APPLY_ENERGY_PRODUCT, &
                     APPLY_5X16,APPLY_6X16,APPLY_7X24,APPLY_5X8, &
                     APPLY_WRAP,APPLY_2X24, &
                     YES_RUN_TRANSACT, &
                     RUN_TRANSACT, &
                     MIX_RATIOS_ACTIVE, &
                     DAILY_CALL_PUT_CAPACITY, &
                     CX_ANNUAL_CONTRACT, &
                     ALL_CONTINGENT_MUST_BE_UP=.FALSE., &
                     TEST_CONTINGENCY, &
                     ANNUAL_ENERGY_PRODUCTS, &
                     RECORD_EP_2_AC, &
                     PUT_MONTHLY_EP_COST_REV, &
                     TEMP_L, &
                     MONTHLY_CF_USER_DATA, &
                     R_YES_REPORT_PRODUCT, &
                     YES_FISCAL_REPORTING, &
                     POWER_DERIV_REV_EXP_BY_CLASS, &
                     FISCAL_ONLY, &
                     R_ECITY_USED, &
                     YES_CONTINGENT_MARKET_CAPACITY, &
                     TRANS_ACTIVE_IN_ENDPOINT(:), &
                     R_TRANS_NOT_ACTIVE, &
                     RENEWABLE_OPTION_CHECK, &
                     EFFICIENCY_OPTION_CHECK, &
                     GRX_SAVE_DERIV_PLAN_VALUES, &
                     GET_DERIV_ZONE_ID
      LOGICAL(kind=4) :: ENERGY_PRODUCTS_FILE_EXISTS, &
                     R_SAVE_VALUES
      LOGICAL(kind=1) :: SAVE_VALUES, &
                GET_DAILY_PRODUTION_DATA, &
                UPDATE_TRANS_FIXED_COSTS, &
                ACTIVE_GRX_RPS_VECTOR, &
                PUT_RPS_ENRG_CAP, &
                GET_TRANS_NAME, &
                GET_GRX_TRANS_MONTHLY_STORAGE
      INTEGER(kind=2) :: YR,CURRENT_RECORD,NUM_TRANSACTIONS=0,TRANS, &
                     DELETE,GET_NUM_SCENARIO_TRANSACTIONS, &
                     GET_TRANS_FOR_GRX_ID, &
                     GET_TRANS_GROUP_FOR_TRANS, &
                     SAVE_ACTIVE_TRANSACTIONS=0, &
                     SAVE_NUM_PROPOSED_UNITS=0, &
                     R_HOUR_IN_DAY,R_DAY_IN_WEEK,R_MONTH, &
                     DAY_IN_WEEK,DAY_IN_MONTH, &
                     R_HOUR, &
                     R_YEAR,R_TRANS_GROUP,TRANS_GROUP, &
                     MONTHLY_ACTIVE_TRANSACTIONS=0, &
                     ANNUAL_ACTIVE_TRANSACTIONS=0, &
                     FISCAL_ACTIVE_TRANSACTIONS=0, &
                     GET_NUM_FISCAL_TRANSACTIONS, &
                     GET_NUM_ANNUAL_TRANSACTIONS, &
                     GET_NUM_SAVE_ACTIVE_TRANSACT, &
                     GET_NUM_MONTHLY_TRANSACTIONS, &
                     LOCAL_ACTIVE_TRANSACTIONS, &
                     BEGIN_MONTH,BEGIN_YEAR, &
                     END_MONTH,END_YEAR, &
                     TEST_DATE=0,IND, &
                     BEGIN_TEST_DATE=0, &
                     END_TEST_DATE=0, &
                     R_DATE1,R_DATE2, &
                     THIS_YEAR, &
                     R_DAY_IN_MONTH, &
                     MAX_TRANS_GROUP_NUM, &
                     GET_TRANS_GROUP_POSITION, &
                     MAX_TRANS_E_P, &
                     TG,AC, &
                     NUM_OF_TRANS_CLASSES=0, &
                     MAX_TRANS_CLASS_NUM, &
                     NUM_OF_ASSET_CLASSES, &
                     MAX_ASSET_CLASS_NUM, &
                     MAX_ASSET_GROUPS=1, &
                     MAX_TRANS_GROUPS=256, &
                     MAX_ASSET_CLASS_GROUPS, &
                     IREC,MO, &
                     PRODUCT, &
                     R_TRANS_NUM, &
                     I,K,M,N, &
                     HOUR_IN_MONTH, &
                     TEMP_I2, &
                     LOCAL_YR, &
                     DATA_BASES,GET_DATA_BASE_FOR_UNIT, &
                     DAY_TYPE,GET_DAY_TYPE, &
                     GET_TRANS_GROUP_INDEX, &
                     GET_DATA_BASE_FOR_TRANS, &
                     GET_TRANS_UNIT_TO_BLOCK, &
                     FISCAL_SEASON_RESET, &
                     R_SUM_ANNUAL, &
                     PEAK_MONTH,LOCAL_PEAK_MONTH,LOCAL_YEAR, &
                     TRANSACT_ANLST_RSLTS_AVAIL_STG, &
                     TEMP_STRIKES, &
                     TEMP_HOURS, &
                     SAVE_MONTH=0, &
                     R_GRX_ID, &
                     MONTHLY_USER_CF_COUNTER(:), &
                     MONTHLY_CF_TRANS(:), &
                     MAX_USER_CF, &
                     R_VECTOR
      CHARACTER(len=36) :: DERIV_GUID
      CHARACTER(len=2) ::UNIT_TYPE,UNIT_TYPE_CATEGORY
      INTEGER(kind=8) :: DERIV_PARENT_ID
      REAL(kind=4) :: MONTHLY_CT_GROUP_REPORT(:,:,:), &
                     ENERGY_PRODUCTS_PLANNING_ADD, &
                     R_LOCAL_YEAR
      ALLOCATABLE :: MONTHLY_CT_GROUP_REPORT
      INTEGER(kind=2) :: MAX_MONTHLY_GROUPS, &
                     MAX_MONTHLY_GROUP_VARIABLES, &
                     CT_GRP, &
                     CT Capacity, &
                     fixed_om, &
                     Generation, &
                     variable_om, &
                     sulfur_o2, &
                     CT Equivalent Capacity, &
                     GRX_BUILD_VECTOR(:), &
                     GRX_BUILD_VECTOR_INDEX(32000)

      PARAMETER( &
                     CT Capacity =  1, &
                     fixed_om =     2, &
                     Generation =   3, &
                     variable_om =  6, &
                     sulfur_o2 =    7, &
                     CT Equivalent Capacity=16)

      integer :: bad_index_value, max_index_value
      INTEGER(kind=2) :: R_MAX_MONTHLY_GROUPS, &
                R_MAX_MONTHLY_GROUP_VARIABLES, &
                R_GROUP, &
                GET_TRANS_RPS_USER_PATTERN, &
                CUMULATIVE_HOURS, &
                DAILY_STRIKES, &
                TYPE_OF_DERIV, &
                GET_DERIV_PRIM_MOVER, &
                GET_DERIV_PRIM_MOVER_INDEX, &
                START_HOUR, &
                DAY_IN_YEAR
      REAL(kind=4) :: R_MONTHLY_GROUP_REPORT(0:12,0:99,17)

      integer (kind=2), PARAMETER :: NUM_PRODUCTS=8
      REAL(kind=4) :: HOURLY_FORWARD_CONTRACT_ENERGY, &
                     DERIV_CAPACITY_PLANNING, &
                     DERIV_CAPACITY_PLANNING_BY_TG, &
                     DERIV_CAPACITY_PLANNING_BY_ALL, &
                     DERIV_NEW_CAP_PLAN_BY_ALL, &
                     GET_VAR,TEMP_R4, &
                     R_ON_LINE_DATE, &
                     R_OFF_LINE_DATE, &
                     R_UNIT_ID, &
                     R_ASSET_CLASS, &
                     R_DAILY_CAPACITY(24), &
                     DAILY_CAPACITY(24), &
                     R_LOCAL_PRICE(800), &
                     R_ANNUAL_PRICE(8784), &
                     R_ANNUAL_USAGE(8784), &
                     LAST_HOUR_QUANT, &
                     CURRENT_HOUR_QUANT, &
                     LAST_HOUR_PRICE, &
                     CURRENT_HOUR_PRICE, &
                     R_ANNUAL_STRIKE_PRICE(8784), &
                     HOUR_USAGE(24)
        REAL*4 PRODUCT_PATTERN(24,NUM_PRODUCTS) &
                 /0.,0.,0.,0.,0.,0.,1.,1., &
                  1.,1.,1.,1.,1.,1.,1.,1., &
                  1.,1.,1.,1.,1.,1.,0.,0., &! 5X16 &
                  0.,0.,0.,0.,0.,0.,1.,1., &
                  1.,1.,1.,1.,1.,1.,1.,1., &
                  1.,1.,1.,1.,1.,1.,0.,0., & ! 6X16 &
                  1.,1.,1.,1.,1.,1.,1.,1., &
                  1.,1.,1.,1.,1.,1.,1.,1., &
                  1.,1.,1.,1.,1.,1.,1.,1., & ! 7X24 &
                  1.,1.,1.,1.,1.,1.,1.,0., &
                  0.,0.,0.,0.,0.,0.,0.,0., &
                  0.,0.,0.,0.,0.,0.,0.,1., & ! 5X8 &
                  1.,1.,1.,1.,1.,1.,0.,0., &
                  0.,0.,0.,0.,0.,0.,0.,0., &
                  0.,0.,0.,0.,0.,0.,1.,1., & ! Wrap &
                  0.,0.,0.,0.,0.,0.,0.,0., &
                  0.,0.,0.,0.,0.,0.,0.,0., &
                  0.,0.,0.,0.,0.,0.,0.,0., & ! USER_DEFINED &
                  1.,1.,1.,1.,1.,1.,1.,1., &
                  1.,1.,1.,1.,1.,1.,1.,1., &
                  1.,1.,1.,1.,1.,1.,1.,1., & ! 2x24  &
                  0.,0.,0.,0.,0.,0.,0.,0., &
                  0.,0.,0.,0.,0.,0.,0.,0., &
                  0.,0.,0.,0.,0.,0.,0.,0./ ! Hourly &


      REAL(kind=4) :: R_DAILY_PRICE(24), &
                     DAILY_PRICE(24), &
                     CALL_OR_PUT, &
                     LONG_SHORT, &
                     HOURS_FOR_PRODUCT(NUM_PRODUCTS)= &
                      (/16.,16.,24.,8.,12.6,24.,24.,24./), &
                     DAILY_OPTION_VALUE, &
                     DAILY_OPTION_REVENUE, &
                     HOURLY_CONTINGENT_MARKET_CAP, &
                     R_HOURLY_IMPORT_CAP, &
                     GET_DAILY_OPTION_VALUE, &
                     GET_HOURLY_OPTION_VALUE, &
                     GET_HOUR_DAY_OPTION_VALUE, &
                     DAILY_ENERGY, &
                     DAILY_VARIABLE_COST, &
                     DAILY_ENERGY_REVENUE(365), &
                     R_TRANS_ENRG, &
                     R_TRANS_CAP, &
                     R_TRANS_VAR_EXP, &
                     R_TRANS_VAR_MWH, &
                     R_TRANS_FIX_EXP, &
                     TRANS_CAP, &
                     TRANS_ENRG, &
                     TRANS_VAR_EXP, &
                     TRANS_FIX_EXP, &
                     CALC_MONTHLY_DERIVATIVE_FIXED, &
                     CALC_GRX_FIX_COST_PER_UNIT, &
                     LOCAL_CAPACITY, &
                     R_CAPACITY, &
                     R_ANNUAL_HOURS, &
                     R_ANNUAL_STRIKES, &
                     R_ANNUAL_ENERGY, &
                     R_ANNUAL_CHARGE, &
                     R_ANNUAL_ENERGY_REVENUE, &
                     R_ANNUAL_VARIABLE_COST, &
                     R_ANNUAL_HYBRID_MARKET_ENERGY, &
                     MARKET_DISCHARGE_LIMIT, &
                     R_ANNUAL_HYBRID_MW_PUMP(8784), &
                     R_ANNUAL_HYBRID_MW_GEN(8784), &
                     R_ANNUAL_HYBRID_HR_MARGIN(8784), &
                     R_MARKET_MW_USAGE(8784), &
                     R_CONSTRAINED_MW_USAGE(8784), &
                     R_ANNUAL_HYBRID_PROFIT(8784), &
                     R_ANNUAL_EMISSION_COST, &
                     R_ANNUAL_FIXED_COST, &
                     MONTHLY_OP_HOURS(0:12), &
                     MONTHLY_OP_STRIKES(0:12), &
                     MONTHLY_OP_ENERGY(0:12), &
                     MONTHLY_OP_ENERGY_REVENUE(0:12), &
                     MONTHLY_OP_VARIABLE_COST(0:12), &
                     MONTHLY_OP_FIXED_COST(0:12), &
                     R_ENERGY, &
                     R_TRANS_REV, &
                     R_TRANS_REV_MWH, &
                     R_TRANS_CHARGING_ENERGY, &
                     R_TRANS_HOURS, &
                     R_PRODUCT_HOURS, &
                     R_TRANS_STRIKES, &
                     R_HOUR_PRICE, &
                     TEMP_PRICE, &
                     TEMP_PRICE_24(24), &
                     MONTH_COST, &
                     MONTH_REVENUE, &
                     ESCALATED_MONTHLY_VALUE, &
                     SELL_OR_BUY, &
                     TEMP_QUANTITY, &
                     SUM_QUANTITY, &
                     RESOURCE_MW, &
                     GET_BLOCK_OUTAGE_CAPACITY, &
                     TEMP_R1,TEMP_R2, &
                     GET_USER_DAY, &
                     CAP_MW, &
                     USER_CF(:), &
                     MONTHLY_USER_CF(:), &
                     R_VALUES_30(30), &
                     TEMP_ENRG, &
                     TEMP_CAP, &
                     GET_Holding_Company_ID
      CHARACTER(len=1) :: LOCAL_ASSIGNMENT(2)=(/'O','P'/)
      character(len=20) :: lpt ! local copy of local_product_type value
      CHARACTER(len=20) :: LOCAL_PRODUCT_TYPE(NUM_PRODUCTS)= &
                                          (/'5x16                ', &
                                           '6x16                ', &
                                           '7x24                ', &
                                           '5x8                 ', &
                                           'Wrap                ', &
                                           'User                ', &
                                           '2x24                ', &
                                           'Hourly              '/)
! INPUT DATA LIST
      CHARACTER(len=20) :: TRANSACTION_NAME(:), &
                     COUNTERPARTY_NAME(:), &
                     TRANSACTION_TYPE(:), &
                     PRODUCT_TYPE(:), &
                     COMMENT*50, & !  THROWN AWAY
                     DISTINGUISHING(:)
      CHARACTER*(*) R_GET_MONTHLY_TRANS_VARIABLES
      CHARACTER(len=1) :: &
                     TRANSACTION_CLASS(:), &
                     EXPENSE_ASSIGNMENT(:), &
                     EXPENSE_COLLECTION(:), &
                     PRICE_TYPE(:), &
                     OPTION_POSITION(:), &
                     STRIKE_FREQUENCY(:), &
                     REPORT_PRODUCT(:), &
                     UNIT_CONTINGENCY(:), &
                     FUEL_PRICE_TYPE(:), &
                     TRANSPORTATION_BASIS_TYPE(:), &
                     WVPA_RATE_TRACKER(:), &
                     WVPA_MEM_TRACKER(:), &
                     UNIT_CONTING_MARKET_SUPPLEMENT(:), &
                     DERIV_AGGREGATED_UNIT
      CHARACTER(len=6) :: UNIT_CATEGORY(:), &
                  DERIV_ZONE_ID(:), &
                  TRANS_ZONE_ID

! BEGIN 042609
      INTEGER(kind=2) :: &
                     Tax_Credit_Begin_Date(:), & !  I 2
                     Tax_Credit_End_Date(:), & !  90 I 2
                     State_TG_Index(:), &
                     State_Index(:), &
                     GET_TRANS_STATE_INDEX, &
                     GRX_ID(:), &
                     GRX_ID_INDEX(:), &
                     RI, &
                     PROPOSED_RESOURCE_INDEX(:), &
                     PROPOSED_RESOURCE_POSITION(:), &
                     GET_DERIV_PROP_RESOURCE_POS, &
                     ST_TG,STATE_ID_LOOKUP,PM, &
                     R_ST_TG, &
                     LHS_INDEX(:), &
                     GET_TRANS_RPS_PROG_NUMBER, &
                     GET_REGIONAL_SCENARIO_INDEX
      INTEGER(kind=4) :: &
                     Holding_Company_ID(:), & !  95 I 4
                     External_Unit_ID(:) ! I 4
      REAL(kind=4) :: &
                     Production_Tax_Credit(:), & !  R 4
                     RPS_Contribution(:), & !  R 4
                     Percentage_Owned(:), & !  R 4
                     GRX_RPS_CAPACITY(:,:), &
                     SAVE_GRX_RPS_CAPACITY(:,:), &
                     GRX_STORAGE_PATTERN(:,:), &
                     GRX_INDEP_PATTERN(:,:), &
                     SAVE_GRX_STORAGE_PATTERN(:,:), &
                     GET_GRX_TRANS_HOURLY_STORAGE, &
                     R_RESOURCE_RPS_VARS(17), &
                     GET_MONTHLY_REGIONAL_PARAM, &
                     GET_TRANS_CAP_PLAN_FAC, &
                     GET_TRANS_RPS_PERCENT, &
                     LOCAL_RPS_CONTRIB_PERCENT

      CHARACTER(len=6) :: &
                     State_TG_Key, & !  S 6
                     TEMP_CHAR6, &
                     EIA_Plant_ID ! S 6
      CHARACTER(len=10) :: &
                     Fuel_Category(:) ! 100 S 10
      CHARACTER(len=12) :: &
                     Owner_ID     ! S 12
      CHARACTER(len=20) :: &
                     New_Build_Unit_Status, & !  S 20
                     Latin_Hypercube_Dist ! S 20
      CHARACTER(len=25) :: &
                     NERC_Subregion, & !  S 25
                     State_Province ! S 25
! END 042609
!
! ADDED 090706.
!
         CHARACTER (LEN=1)  :: IS_AN_EXCHANGE_AGREEMENT
         CHARACTER (LEN=10) :: EXCHANGE_TYPE
         INTEGER (KIND=2)   :: JOINT_TRANSACTION_ID
         CHARACTER (LEN=3)  :: RESERVES_INCLUDED  ! Yes, No
         CHARACTER (LEN=5) :: DELIVERY_ZONE  ! East, West
         REAL (KIND=4) :: ANNUAL_ENERGY_PLANNING_FACTOR
         REAL (KIND=4) :: PLANNING_FACTOR, &
                          ENERGY_RETURN_LATER_IN_DAY, &
                          ENERGY_RETURN_MW, &
                          MINIMUM_ENERGY_LIMIT_MW(3), &
                          MAXIMUM_ENERGY_LIMIT_MW(3), &
                          UnitConstructionCostUSDKW, &
                          ConstructionCostEscalation, &
                          Year1CapitalRecoveryRate, &
                          Year1RecoveryRateEscalation, &
                          BookandServiceLife ! 16*4
         INTEGER (KIND=2)   :: FirstYearAvailable, &
                               LastYearAvailable, &
                               MinimumAnnualUnits, &
                               MaximumAnnualUnits, &
                               MaximumCumulativeUnits, &
                               ContinuousBuildFirstYear ! 6*2
! UNIT CONTINGENT VARIABLES
      INTEGER(kind=2) :: LOCAL_UNIT_ID, &
                  TEMP_UNIT_NUMBER, &
                  TOTAL_CONTINGENT_UNITS, &
                  TOTAL_CONTINGENT_UP_UNITS, &
                  TOTAL_CONTINGENT_FOR_UNITS, &
                  TOTAL_CONTINGENT_MOR_UNITS, &
                  NUMBER_OF_UNITS_FOR_ID, &
                  GET_RESOURCE_ID_TO_UNIT, &
                  OUTAGE_TYPE, &
                  HOUR, &
                  R_TG, &
                  GET_P_DERIV_TRACKER_INDEX, &
                  GET_P_DERIV_MEM_TRK_INDEX, &
                  WVPA_TRACKING_TYPE, &
                  WVPA_MEM_TRACKING_TYPE, &
                  DERIV_PRIMARY_MOVER(:), &
                  LOCAL_TG, &
                  R_PM, &
                  R_FT, &
                  FT, &
                  GET_TRANS_EMISS_MARKET_PTR
      REAL(kind=4) :: &
                  GET_BLOCK_OUTAGE_4_HOUR, &
                  GET_BLOCK_AVAIL_4_HOUR, &
                  UNIT_AVAIL, &
                  DAILY_MW(0:24), &
                  HOURLY_ENERGY_PRICE(24), &
                  R_MONTHLY_AVAIL(0:800), &
                  MONTHLY_CONTINGENT_CAPACITY(:), &
                  GET_MONTHLY_CONTINGENT_CAP, &
                  CAPACITY_PLANNING_FACTOR(:), &
                  ANNUAL_INTERRUPTIBLE_CAPACITY(:), &
                  ANNU_PLANNING_INTRPT_CAPACITY(:), &
                  ANNUAL_STORAGE_CAPACITY(:), &
                  GET_ANNUAL_INTER_CAPACITY, & !  FOR CAPACITY PLANNING PURPOSES
                  FUEL_PRICE(:), &
                  TRANSPORTATION_BASIS(:), &
                  DELIVERY_ADDER(:), &
                  DOLLAR_MWH(:), &
                  LocalDailyDischargeLimit
       real(kind=4) :: pass_24(24)
       

      CHARACTER(len=2) :: TYPE_CHAR(0:12)= &
      (/'  ','FO','PC','PP','FA','FC','FP','IC','IP','DS','LC','LP', &
                                                                 'MP'/), &
                     PLANNING_METHOD,CAPACITY_PLANNING_METHOD
      CHARACTER(len=3) :: COUNTERPARTY_BOND_RATING(:)
      CHARACTER(len=6) :: FUEL_TYPE(:)
      INTEGER(kind=4) :: TRANSACTION_ID(:)
      integer (kind=2), allocatable :: rps_program_numbers(:)
      INTEGER(kind=2) :: &
                     TRANSACTION_GROUP(:), &
                     REPORTING_GENERATION_GROUP(:), &
                     DOLLAR_KW_DAY_ESC(:), &
                     DOLLAR_KW_MONTH_ESC(:), &
                     DOLLAR_KW_YEAR_ESC(:), &
                     DOLLAR_MONTH_ESC(:), &
                     DOLLAR_DEAL_ESC(:), &
                     BEGIN_DAY_IN_MONTH(:), &
                     END_DAY_IN_MONTH(:), &
                     ACTIVE_IN_MONTH(:), &
                     TRANS_TO_ACTIVE_MONTH(:), &
                     ACTIVE_IN_YEAR(:), &
                     ACTIVE_IN_YEAR_INDEX(:), &
                     TRANS_E_P_INDEX(:), &
                     TRANS_EP_2_TRANS_GROUPS(:), &
                     TRANS_EP_2_TG(:), &
                     MAXIMUM_STRIKES(:), &
                     BILLING_LAG(:), &
                     MINIMUM_STRIKES(:), &
                     MONTHLY_STRIKES(:,:), &
                     STRIKES_AVAILABLE(:), &
                     STRIKES_REQUIRED(:), &
                     ASSET_CLASS_GROUPS_INDEX(:), &
                     ASSET_2_TRANS_INDEX(:,:), &
                     NUMBER_ASSET_2_TRANS(:), &
                     FIRST_AC_TG(:), &
                     DERIVATIVE_TYPE(:), &
                     PRODUCT_INDEX(:), &
                     NUM_FORWARDS(:), &
                     NUM_CALLS(:), &
                     NUM_MONTH_CALLS(:), &
                     NUM_ANNUAL_CALLS(:), &
                     NUM_PUTS(:), &
                     NUM_STORAGE(:), &
                     ANN_NUM_STOR(:), &
                     SCEN_NUM_STOR(:), &
                     ANN_STOR_POS(:,:), &
                     SCEN_STOR_POS(:,:), &
                     NUM_LF_CALLS(:), &
                     NUM_LF_PUTS(:), &
                     NUM_MONTH_PUTS(:), &
                     NUM_ANNUAL_PUTS(:), &
                     GET_NUM_ANNUAL_DERIVATIVES, &
                     NUM_INTER_PUTS(:), &
                     NUM_INTER_CALLS(:), &
                     FORWARD_POSITION(:,:), &
                     CALL_POSITION(:,:), &
                     MONTH_CALL_POSITION(:,:), &
                     ANNUAL_CALL_POSITION(:,:), &
                     PUT_POSITION(:,:), &
                     MONTH_PUT_POSITION(:,:), &
                     ANNUAL_PUT_POSITION(:,:), &
                     STORAGE_POSITION(:,:), &
                     BATTERY_POSITION(:,:), &
                     LF_CALL_POSITION(:,:), &
                     LF_PUT_POSITION(:,:), &
                     INTER_PUT_POSITION(:,:), &
                     INTER_CALL_POSITION(:,:), &
                     MONTHLY_PRODUCT_HOURS(:,:), &
                     MONTHLY_PRODUCT_DAYS(:,:), &
                     MONTHLY_PRODUCT_MONTHS(:), &
                     FISCAL_TRANS_HOURS(:), &
                     FISCAL_PRODUCT_HOURS(:), &
                     FISCAL_PRODUCT_DAYS(:), &
                     FISCAL_PRODUCT_MONTHS(:), &
                     FISCAL_STRIKES(:), &
                     FISCAL_ACTIVE_IN_YEAR_INDEX(:), &
                     FISCAL_ACTIVE_IN_YEAR(:), &
                     UNIT_CONTINGENT_LINK(:), &
                     USER_DAY_TYPES_ID(:), &
                     YEARS_BEG_DAY_IN_MO(:,:), &
                     YEARS_END_DAY_IN_MO(:,:), &
                     GET_BELONGS_TO_GROUP, & ! External
                     TOTAL_ANNUAL_CALLS, &
                     TOTAL_ANNUAL_PUTS, &
                     GET_REPORTING_GENERATION_GROUP, &
                     LOCAL_BEGIN_EP, &
                     LOCAL_END_EP
      INTEGER(kind=4) :: BEGIN_DATE, &
                     END_DATE, &
                     CONTRACT_DATE(:)
      REAL(kind=4) :: QUANTITY_OF_PRODUCT(:), &
                     ANNUAL_CAPACITY(:,:,:), &
                     CONTINGENT_CAPACITY(:,:), &
                     MONTHLY_2ND_ENERGY_PRICE(:), &
                     SECOND_ENERGY_PRICE(:), &
                     DOLLAR_KW_DAY(:), &
                     DOLLAR_KW_MONTH(:), &
                     DOLLAR_KW_YEAR(:), &
                     GRX_DOLLAR_KW_YEAR(:), &
                     GRX_DOLLAR_KW_MONTH(:), &
                     DOLLAR_MONTH(:), &
                     DOLLAR_DEAL(:), &
                     MONTHLY_ENR_FOR_REV(:,:), &
                     MONTHLY_ENR_FOR_EXP(:,:), &
                     MONTHLY_CAPACITY(:,:), &
                     MONTHLY_AC_REVENUE(:,:,:), &
                     MONTHLY_AC_REVENUE_ENERGY(:,:,:), &
                     MONTHLY_AC_EXPENSE(:,:,:), &
                     MONTHLY_AC_EXPENSE_ENERGY(:,:,:), &
                     MONTHLY_TRANSACTION_COST(:,:), &
                     MONTHLY_TRANSACTION_REVENUE(:,:), &
                     FISCAL_ENERGY_COST(:), &
                     FISCAL_ENERGY(:), &
                     FISCAL_CAPACITY(:), &
                     FISCAL_ENERGY_REVENUE(:), &
                     FISCAL_TRANSACTION_COST(:), &
                     FISCAL_TRANSACTION_REVENUE(:), &
                     ENERGY_MULTIPLIER(:), &
                     HEAT_RATE_FOR_SPREAD(:), &
                     MAX_QUANTITY_OF_PRODUCT(:), &
                     SAVED_QUANT_OF_PRODUCT(:), &
                     GET_TRANS_ITER_CAP_PERCENT

      ALLOCATABLE :: &
                     TRANSACTION_NAME,COUNTERPARTY_NAME, &
                     MONTHLY_USER_CF, &
                     MONTHLY_USER_CF_COUNTER, &
                     MONTHLY_CF_TRANS, &
                     USER_CF, &
                     TRANSACTION_ID,TRANSACTION_GROUP, &
                     TRANSACTION_TYPE,TRANSACTION_CLASS, &
                     DERIVATIVE_TYPE, &
                     PRODUCT_INDEX, &
                     PRODUCT_TYPE, &
                     OPTION_POSITION, &
                     STRIKE_FREQUENCY, &
                     REPORT_PRODUCT, &
                     UNIT_CONTINGENCY, &
                     UNIT_CONTINGENT_LINK, &
                     ENERGY_MULTIPLIER, &
                     TRANS_ACTIVE_IN_ENDPOINT, &
                     HEAT_RATE_FOR_SPREAD, &
                     USER_DAY_TYPES_ID, &
                     FUEL_TYPE, &
                     FUEL_PRICE_TYPE, &
                     FUEL_PRICE, &
                     TRANSPORTATION_BASIS_TYPE, &
                     TRANSPORTATION_BASIS, &
                     DELIVERY_ADDER, &
                     COUNTERPARTY_BOND_RATING, &
                     DOLLAR_MWH, &
                     WVPA_RATE_TRACKER, &
                     WVPA_MEM_TRACKER, &
                     UNIT_CONTING_MARKET_SUPPLEMENT, &
                     Tax_Credit_Begin_Date, &
                     Tax_Credit_End_Date, &
                     LHS_INDEX, &
                     External_Unit_ID, &
                     Holding_Company_ID, &
                     Production_Tax_Credit, &
                     RPS_Contribution, &
                     Percentage_Owned, &
                     Fuel_Category, &
                     GRX_ID, &
                     GRX_ID_INDEX, &
                     State_TG_Index, &
                     State_Index, &
                     PROPOSED_RESOURCE_INDEX, &
                     PROPOSED_RESOURCE_POSITION, &
                     GRX_RPS_CAPACITY, &
                     SAVE_GRX_RPS_CAPACITY, &
                     GRX_STORAGE_PATTERN, &
                     GRX_INDEP_PATTERN, &
                     SAVE_GRX_STORAGE_PATTERN, &
                     DERIV_PRIMARY_MOVER, &
                     CAPACITY_PLANNING_FACTOR, &
                     UNIT_CATEGORY, &
                     DERIV_ZONE_ID, &
                     MAX_QUANTITY_OF_PRODUCT, &
                     ASSET_CLASS_GROUPS_INDEX, &
                     ASSET_2_TRANS_INDEX, &
                     NUMBER_ASSET_2_TRANS, &
                     FIRST_AC_TG, &
                     NUM_FORWARDS, &
                     MONTHLY_CONTINGENT_CAPACITY, &
                     ANNUAL_INTERRUPTIBLE_CAPACITY, &
                     ANNU_PLANNING_INTRPT_CAPACITY, &
                     ANNUAL_STORAGE_CAPACITY, &
                     NUM_CALLS, &
                     NUM_MONTH_CALLS, &
                     NUM_ANNUAL_CALLS, &
                     NUM_PUTS, &
                     NUM_STORAGE, &
                     ANN_NUM_STOR, &
                     SCEN_NUM_STOR, &
                     ANN_STOR_POS, &
                     SCEN_STOR_POS, &
                     NUM_LF_CALLS, &
                     NUM_LF_PUTS, &
                     NUM_MONTH_PUTS, &
                     NUM_ANNUAL_PUTS, &
                     NUM_INTER_PUTS, &
                     NUM_INTER_CALLS, &
                     FORWARD_POSITION, &
                     CALL_POSITION, &
                     MONTH_CALL_POSITION, &
                     ANNUAL_CALL_POSITION, &
                     PUT_POSITION, &
                     MONTH_PUT_POSITION, &
                     ANNUAL_PUT_POSITION, &
                     STORAGE_POSITION, &
                     BATTERY_POSITION, &
                     LF_CALL_POSITION, &
                     LF_PUT_POSITION, &
                     INTER_PUT_POSITION, &
                     INTER_CALL_POSITION, &
                     MONTHLY_PRODUCT_HOURS, &
                     MONTHLY_PRODUCT_DAYS, &
                     MONTHLY_PRODUCT_MONTHS, &
                     CONTRACT_DATE, &
                     DISTINGUISHING, &
                     REPORTING_GENERATION_GROUP, &
                     EXPENSE_ASSIGNMENT, &
                     EXPENSE_COLLECTION, &
                     QUANTITY_OF_PRODUCT, &
                     SAVED_QUANT_OF_PRODUCT, &
                     CONTINGENT_CAPACITY, &
                     MONTHLY_2ND_ENERGY_PRICE, &
                     PRICE_TYPE, &
                     SECOND_ENERGY_PRICE, &
                     MAXIMUM_STRIKES, &
                     BILLING_LAG, &
                     MINIMUM_STRIKES, &
                     DOLLAR_KW_DAY, &
                     DOLLAR_KW_DAY_ESC, &
                     DOLLAR_KW_MONTH, &
                     DOLLAR_KW_MONTH_ESC, &
                     DOLLAR_KW_YEAR, &
                     GRX_DOLLAR_KW_YEAR, &
                     GRX_DOLLAR_KW_MONTH, &
                     DOLLAR_KW_YEAR_ESC, &
                     DOLLAR_MONTH, &
                     DOLLAR_MONTH_ESC, &
                     DOLLAR_DEAL, &
                     DOLLAR_DEAL_ESC, &
                     BEGIN_DAY_IN_MONTH, &
                     END_DAY_IN_MONTH, &
                     ACTIVE_IN_MONTH, &
                     TRANS_TO_ACTIVE_MONTH, &
                     ACTIVE_IN_YEAR, &
                     ACTIVE_IN_YEAR_INDEX, &
                     MONTHLY_ENR_FOR_REV, &
                     MONTHLY_ENR_FOR_EXP, &
                     MONTHLY_CAPACITY, &
                     MONTHLY_STRIKES, &
                     STRIKES_AVAILABLE, &
                     STRIKES_REQUIRED, &
                     MONTHLY_AC_REVENUE, &
                     MONTHLY_AC_REVENUE_ENERGY, &
                     MONTHLY_AC_EXPENSE, &
                     MONTHLY_AC_EXPENSE_ENERGY, &
                     MONTHLY_TRANSACTION_COST, &
                     MONTHLY_TRANSACTION_REVENUE, &
                     FISCAL_ENERGY_COST, &
                     FISCAL_ENERGY, &
                     FISCAL_CAPACITY, &
                     FISCAL_ENERGY_REVENUE, &
                     FISCAL_TRANS_HOURS, &
                     FISCAL_PRODUCT_HOURS, &
                     FISCAL_PRODUCT_DAYS, &
                     FISCAL_PRODUCT_MONTHS, &
                     FISCAL_STRIKES, &
                     FISCAL_ACTIVE_IN_YEAR_INDEX, &
                     FISCAL_ACTIVE_IN_YEAR, &
                     FISCAL_TRANSACTION_COST, &
                     FISCAL_TRANSACTION_REVENUE, &
                     TRANS_E_P_INDEX, &
                     TRANS_EP_2_TRANS_GROUPS, &
                     TRANS_EP_2_TG, &
                     YEARS_BEG_DAY_IN_MO, &
                     YEARS_END_DAY_IN_MO, &
                     ANNUAL_CAPACITY, &
                     GRX_BUILD_VECTOR
!
! 12/15/01. ASSET CLASS VARIABLES.
!
      INTEGER(kind=2) :: RETURN_ANNUL_DERIV_VARIABLES, &
                RET_MNTHLY_DERIV_INC_VARIABLES, &
                RET_MNTHLY_DERIV_CASH_VARIABLES, &
                R_CLASS,ASSET_CLASS,MONTH,DAYS_IN_EACH_MONTH, &
                NUM_STORAGE_UNITS, &
                DAYS_YEAR_TO_DATE,R_HOUR_IN_MONTH

      REAL(kind=4) :: PHY_DERIV_VAR_REVENUE, &
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
                FIN_DERIV_EXPENSE_ENERGY, &
                R_TOTAL_REV, &
                R_TOTAL_EXP
!
      INTEGER :: VALUES_2_ZERO
!
! INTERRUPTIBLE
!
      INTEGER(kind=2) :: SEVEN_I2=7
      REAL(kind=4) :: R_HOURLY_PRICE ! Argument
      real (kind=4) :: &
                 R_WHOLESALE_DEMAND, &
                  R_WHOLESALE_PURCHASE, &
                  REMAINING_WHOLESALE_DEMAND, &
                  REMAINING_WHOLESALE_PURCHASE, &
                  R_HOURLY_CAPACITY, &
                  HOURLY_OPTION_VALUE, &
                  HOURLY_OPTION_REVENUE, &
                  HOURLY_INTERRUPTIBLE_CAPACITY, &
                  HOURLY_LF_CAPACITY, &
                  GET_ANNUAL_CALL_PUT_CAPACITY, &
                  GET_USER_HOUR_IN_DAY, &
                  GET_HOURLY_PRODUTION_DATA, &
                  USER_DAY_MULT, &
                  CO2_NEW_TRANS_DATA, &
                  CO2_RATE_COST_PER_MWH, &
                  R_CO2_COST_PER_MWH, &
                  ZERO_R4=0.0
      
      REAL (kind=4) :: INC_MONTH_VARS(0:12,1:*)
      REAL (kind=4) :: CASH_MONTH_VARS(0:12,1:*)
! ADDITION FOR MULTI-PRODUCTS FILES 7/27/04 MSG
      INTEGER (KIND=2) :: MAX_ENRG_PRODUCT_FILES, &
                          FILE_ID
      INTEGER (KIND=4) :: NUM_TRANSACTIONS_IN_FILE,IOS
      LOGICAL (KIND=1) :: FILE_ACTIVE
!

         LOGICAL(kind=1) :: MONTHLY_CALL_PUT_CAPACITY
         INTEGER(kind=2) :: HOURS_STRUCK,R_DAYS_IN_MONTH, &
            DOW_PROD_HOURS(8,6),J,DAY,HOUR_IN_DAY,LOCAL_BEGIN_DAY, &
            LOCAL_END_DAY,DAYS_IN_MONTH, &
            CALENDAR_DAY_OF_WEEK,GET_DAY_OF_WEEK_4, &
            MONTH_HOURS_STRUCK(12)
         REAL(kind=4) :: STRIKE_VALUE, &
            R_MONTHLY_PRICE(*), & !  hourly detail
            LOCAL_PRICE(800), &
            LOCAL_ANN_PRICE(8784), &
            LOCAL_HYBRID_MW_CHARGE(8784), &
            LOCAL_HYBRID_MW_DISCHARGE(8784), &
            LOCAL_HYBRID_HR_MARGIN(8784), &
            CONSTRAIN_HYBRID_MARGIN(8784), &
            CONSTRAIN_HYBRID_MW_USAGE(8784), &
            MARKET_HYBRID_MW_USAGE(8784), &
            HYBRID_MARKET_MW_USAGE(8784), &
            LOCAL_HYBRID_PROFIT(8784), &
            LOCAL_ANNUAL_ENERGY, &
            LOCAL_ANNUAL_CHARGE, &
            LOCAL_ANNUAL_ENERGY_REVENUE, &
            LOCAL_ANNUAL_VOM_COST, &
            R_MONTHLY_CAPACITY(0:800), & !  hourly detail
            IND_HRLY_CAPACITY(744),    & !  hourly detail
            DOW_PROD_APPLIES(24,8,6)
!
!        DECLARATIONS ADDED 20020304 FOR USE IN SAVING BEST STRIKE-VALUES:
         INTEGER(kind=2) :: LIM_NBV
         PARAMETER(LIM_NBV=1000) ! subject to change
         LOGICAL(kind=1) :: & !  STRIKES_LIMITED,
            R_ENLIST_THIS_OPT_VALU, &
            R_SORT_BEST_STRIKES, &
            R_GET_ANNUAL_BEST_STRIKES
         INTEGER(kind=2) :: &
            IBV,I_INF_BV,I_SUP_BV,N_BEST_VALUES,R_DIM,R_HID,R_CALL_PUT, &
            STRIKES_LISTED(2), & ! (1:2) for calls & puts, respectively
            ORG_ORDER(LIM_NBV), &
            BEST_VAL_HR(LIM_NBV,2), &
            BEST_VAL_ID(LIM_NBV,2), &
            BEST_VAL_MO(LIM_NBV,2), &
            B_MO_VAL_HR(0:12,LIM_NBV,2), &
            B_MO_VAL_ID(0:12,LIM_NBV,2), &
            B_MO_VAL_SL(0:12,2), &
            B_MO_VAL_SE(0:12,2)
         REAL(kind=4) :: INF_BV,SUP_BV,R_OPT_VALU, &
            BEST_VALUES(LIM_NBV,2), &
            BEST_MO_VAL(0:12,LIM_NBV,2), &
            HOURLY_VALUE(24),GET_HOURLY_OPTION_VALUE_BY_HOUR
!
    !    declarations added 20020607 for entry DailyOperMoPumpedStorage
         INTEGER(kind=2) :: PrtDetail, &
           InfHrAvail(0:1)=(8,23), &
           SupHrAvail(0:1)=(22,7), &
           nPreMidnHr=2 ! 25-InfHrAvail(1)
         parameter(PrtDetail=0)
         INTEGER(kind=2) :: jHr,iHr,iDa,nDa,iGP, &
           Rank(0:1), &
           nHrAvail(0:1), &
           GroupHr(0:1), &
           ChronHr(0:1), &
           GpHrRanked(24,0:1)
         REAL(kind=4) :: PmpStoEff,LimHrsGen,TimeConstraint, &
           PmpStoVOM, &
           GpCapyProrated,GpHourPrice, &
           CapyMW(0:1), &
           nHrs(0:1), &
           nHrsSlack(0:1), &
           IncrHrCurCost(0:1), &
           FracHrCurCost(0:1), &
           LimHrsUse(0:1), &
           TimeRatio(0:1), &
           CurHrCost(24,0:1)
        LOGICAL(kind=1) :: Economical,Feasible,DailyOperMoPumpedStorage, &
                                       CX_DailyOperAnPumpedStorage, &
                                       CX_DailyOperAnPS2, &
                                       CX_DailyOperAnPS3, &
                                       EXISTING_STORAGE_N_HYBRID, &
                                       ANNUAL_STORAGE_SORT, &
                                       CX_DailyStorePattern, &
                                       CX_DailyStorePat2, &
                                       WRITE_HOURLY_STORAGE_REPORT, &
                                       CX_DailyStorePat3, &
                                       DailyOperMoPSTransC, &
                                       CONTRACT_IS_STORAGE, &
                                       use_charge_mwh=.true., &
                                       HYBRID_ACTIVE
!
         
         LOGICAL(kind=1) :: YES_HOURLY_GRX_STORAGE_REPORT, &
                     HOURLY_STORAGE_NOT_OPEN=.TRUE.
         INTEGER(kind=2) :: HOURLY_STORAGE_REPORT_HEADER, &
                     HOURLY_STORAGE_NO=0, &
                     HOURLY_STORAGE_VARIABLES, &
                     RPT_HR,END_HOUR,CURRENT_YEAR, &
                     GET_GRX_INDEP_POINTER
!     +               GET_TRANS_GROUP_INDEX
         INTEGER(kind=4) :: HOURLY_STORAGE_REC
         CHARACTER(len=9) :: CL_MONTH_NAME(14)= &
                               (/'January  ','February ', &
                                'March    ','April    ', &
                                'May      ','June     ', &
                                'July     ','August   ', &
                                'September','October  ', &
                                'November ','December ', &
                                'Annual   ','Fiscal   '/)
         CHARACTER(len=35) :: GET_GROUP_NAME
!        DECLARATIONS ADDED 20020723 FOR USE IN ANNUAL_CALL_PUT_CAPACITY:
         LOGICAL(kind=1) :: ANNUAL_CALL_PUT_CAPACITY,L_DUMMY, &
            SAVE_ANNUAL_PRODUCTS_STATUS=.FALSE.,TEMP_L1
         INTEGER(kind=2) :: R_CALENDAR_YEAR,R_DAYS_IN_YEAR,HOUR_IN_YEAR, &
         iMo,jMo,nDaCMo,DaysInNLYMo(12),HOURS_IN_MONTH,HOUR_END_OF_MONTH
         DATA DaysInNLYMo/31,28,31,30,31,30,31,31,30,31,30,31/ ! non-leap years
         REAL(kind=4) :: ONE_DAY_PRICE(24),MARGIN(144), &
                   HIGH_CAP(12),LOW_CAP(12),HOUR_STORAGE,TEST_CAP, &
                   PATTERN(24),TOTAL_STORAGE(24),STOR_LIM, &
                   DailyPumpingLimitMWH, &
                   PATTERN_MARGIN(24)
        INTEGER(kind=2) :: HIGH_HOUR(12),LOW_HOUR(12),MARGIN_ORDER(144), &
                   TOTAL_COMBO,HIGH_HR_MARGIN(144),LOW_HR_MARGIN(144), &
                   ANNUAL_ORDER(8784),PRICE_ORDER(24),L,CHARGE(24)
         REAL(kind=4) :: &
            ANNUAL_PRICE(8784), & !  hourly detail
            IND_ANNL_HRLY_CAP(8784),   & !  hourly detail
            ANNUAL_AVAIL(0:8784),    & !  hourly detail
            CUR_MO_ENERGY_PRICE(12), &
            CUR_QUANTITY, &
            CUR_MONTH_QUANTITY(12), &
            SUM_QTY_COST, &
            C_HOURS, &
            GET_DAILY_PUMPING_LIMIT
!

         LOGICAL(kind=1) :: R_GET_ESC_NRG_PRICE
         INTEGER(kind=2) :: R_TRANS,R_START_YEAR,R_EP_YEARS,HR
         REAL(kind=4) :: GENP_R_VALUE,GENP_ACCUM
!
         REAL (KIND=4) :: LMP_Price, &
                          LMP_HourlyEnergyPrice(24), &
                          LMP_CongestionInMonth, &
                          LMP_CongestionByMonth(12), &
                          PLACE_HOLDER107, &
                          LMPCongestionPrice
         CHARACTER (LEN=22) :: IncExpenseAssignment,IncRevenueAssignment
         INTEGER (KIND=2) :: INCOME_STATEMENT_POSITION
         CHARACTER (LEN=2) :: LMPMarketHubID
         CHARACTER (LEN=3) :: LMPScenarioNum
         CHARACTER (LEN=15) :: LMP_TempPriceFileNames
         CHARACTER (LEN=5) :: LMPActive,LMPCalc
         CHARACTER (LEN=20) :: ScenarioLoadShape &
                                                ='Hourly              '
         LOGICAL (KIND=1) :: LMP_IS_ACTIVE
         CHARACTER (LEN=5) :: ProductHourlyRefName
         INTEGER (KIND=2) :: ProductHourlyRefNumber, &
                             StoreHourlyProductFiles, &
                             ResetActiveHourlyProducts, &
                             HYBRID_BATTERY_COUNT=0, &
                             BATTERY_COUNT=0, &
                             HYBRID_INDEP_COUNT=0
!
         CHARACTER (LEN=5), ALLOCATABLE :: DerivCapMarketType(:)
         CHARACTER (LEN=7), ALLOCATABLE :: DerivCapMarketMonth(:)
         INTEGER (KIND=4), ALLOCATABLE :: DerivCapMarketPointer(:), &
                                          EV_ID_HYBRID_BATTERY(:)
         REAL (KIND=4), ALLOCATABLE :: DerivLMPPeakSlope(:), &
                                       DerivLMPPeakConst(:), &
             DerivLMPOffPeakSlope(:),DerivLMPOffPeakConst(:), &
             DerivCapAdjustmentFactor(:),DerivCapPlanningFactor(:)
         REAL (KIND=4), ALLOCATABLE :: DerivLMPMoPeakSlope(:,:), &
                                 DerivLMPMoPeakConst(:,:), &
               DerivLMPMoOffPeakSlope(:,:),DerivLMPMoOffPeakConst(:,:)
         LOGICAL (KIND=4), ALLOCATABLE :: DerivCalCongestion(:)
         INTEGER (KIND=4) :: ActiveDirvs
         REAL (KIND=4), ALLOCATABLE :: &
                   DerivIncExpense(:,:),DerivIncRevenue(:,:), &
                   DerivPeakPeriodLMPrice(:,:),DerivOffPeakLMPrice(:,:), &
                   DerivCongestionCost(:,:),DerivTransGroupPrice(:,:)
         CHARACTER (LEN=17), ALLOCATABLE :: DerivCapExpenseCollection(:)
         INTEGER (KIND=2), ALLOCATABLE :: DerivIncExpensePtr(:), &
                                          DerivIncRevenuePtr(:), &
                                          ASSET_CLASS_ID(:), &
                                          ASSET_ALLOCATION_VECTOR(:)
         LOGICAL (KIND=4), ALLOCATABLE :: DerivLMPActive(:)
         CHARACTER (LEN=15), ALLOCATABLE :: LMPDerivPriceFileNames(:)
         INTEGER (KIND=2), ALLOCATABLE :: LMPDerivPriceFilePointer(:)
!         REAL (KIND=4), ALLOCATABLE ::
!     +                           LMPDerivAnnualHourlyInputPrices(:,:,:)
         INTEGER (KIND=4) :: LMPDerivActivePriceFiles
         integer :: ubound_tmp
         logical :: rep_failed, tg_element_failed, ft_element_failed, &
           yr_element_failed
         integer ::  tg_ubound, ft_index, ft_ubound, &
           yr_ubound
           
           
           

           
           integer :: file_trace_aep=0
           integer :: file_trace_cxat=0 ! CX_ANNUAL_CONTRACT
           integer :: cxat_count=0
           integer :: aep_count=0
           
           

           
         
         CHARACTER(len=32) :: bad_vector_name
         character (len=255) :: baddim, badidxval
         

         real (kind=4) :: R_HOUR_ACTIVE(24)


! END DATA DECLARATIONS

          
         READ_ENERGY_PRODUCTS_DATA = .FALSE.

         CALL DOES_ENER_PRODUCTS_FILE_EXIST(MAX_ENRG_PRODUCT_FILES, &
                                            ENERGY_PRODUCTS_FILE_EXISTS)
         IF(ENERGY_PRODUCTS_FILE_EXISTS) THEN
            CALL GET_BASE_TRANSACTIONS(NUM_TRANSACTIONS, &
                                       MAX_TRANS_GROUP_NUM)
            CALL RETURN_EP_GROUP_INFO(NUM_OF_TRANS_CLASSES, &
                                      MAX_TRANS_GROUP_NUM, &
                                      NUM_OF_ASSET_CLASSES, &
                                      MAX_ASSET_CLASS_NUM)

            MAX_ASSET_GROUPS = MAX_ASSET_CLASS_NUM
            NUM_OF_TRANS_CLASSES = GET_NUMBER_OF_ACTIVE_GROUPS()
            IF(NUM_TRANSACTIONS == 0) THEN
               WRITE(4,*) "In the energy products file, there were"
               WRITE(4,*) "no active records found.  Please file"
               WRITE(4,*) "verify the energy products and check any"
               WRITE(4,*) "Energy Products overlays."
            ENDIF
            IF( ALLOCATED(TRANSACTION_NAME) ) &
                 DEALLOCATE( &
                     TRANSACTION_NAME,COUNTERPARTY_NAME, &
                     MONTHLY_USER_CF, &
                     MONTHLY_USER_CF_COUNTER, &
                     MONTHLY_CF_TRANS, &
                     USER_CF, &
                     TRANSACTION_ID,TRANSACTION_GROUP, &
                     TRANSACTION_TYPE,TRANSACTION_CLASS, &
                     DERIVATIVE_TYPE, &
                     PRODUCT_INDEX, &
                     PRODUCT_TYPE, &
                     OPTION_POSITION, &
                     PRODUCT_ACTIVE, &
                     STRIKE_FREQUENCY, &
                     REPORT_PRODUCT, &
                     UNIT_CONTINGENCY, &
                     UNIT_CONTINGENT_LINK, &
                     ENERGY_MULTIPLIER, &
                     TRANS_ACTIVE_IN_ENDPOINT, &
                     HEAT_RATE_FOR_SPREAD, &
                     USER_DAY_TYPES_ID, &
                     PUMPING_CAPACITY, &
                     PUMPING_STORAGE_EFFICIENCY, &
                     DAILY_PUMPING_MULT, &
                     FUEL_TYPE, &
                     FUEL_PRICE_TYPE, &
                     FUEL_PRICE, &
                     TRANSPORTATION_BASIS_TYPE, &
                     TRANSPORTATION_BASIS, &
                     DELIVERY_ADDER, &
                     COUNTERPARTY_BOND_RATING, &
                     DOLLAR_MWH, &
                     WVPA_RATE_TRACKER, &
                     WVPA_MEM_TRACKER, &
                     UNIT_CONTING_MARKET_SUPPLEMENT, &
                     Tax_Credit_Begin_Date, &
                     Tax_Credit_End_Date, &
                     LHS_INDEX, &
                     External_Unit_ID, &
                     Holding_Company_ID, &
                     Production_Tax_Credit, &
                     RPS_Contribution, &
                     Percentage_Owned, &
                     Fuel_Category, &
                     GRX_ID, &
                     GRX_ID_INDEX, &
                     State_TG_Index, &
                     State_Index, &
                     PROPOSED_RESOURCE_INDEX, &
                     PROPOSED_RESOURCE_POSITION, &
                     GRX_RPS_CAPACITY, &
                     SAVE_GRX_RPS_CAPACITY, &
                     GRX_STORAGE_PATTERN, &
                     GRX_INDEP_PATTERN, &
                     SAVE_GRX_STORAGE_PATTERN, &
                     DERIV_PRIMARY_MOVER, &
                     CAPACITY_PLANNING_FACTOR, &
                     UNIT_CATEGORY, &
                     DERIV_ZONE_ID, &
                     ENERGY_PRICE_MULTIPLIER, &
                     MAX_QUANTITY_OF_PRODUCT, &
                     MONTHLY_ENERGY_MULT, &
                     ASSET_CLASS_GROUPS_INDEX, &
                     ASSET_2_TRANS_INDEX, &
                     NUMBER_ASSET_2_TRANS, &
                     FIRST_AC_TG, &
                     NUM_FORWARDS, &
                     MONTHLY_CONTINGENT_CAPACITY, &
                     ANNUAL_INTERRUPTIBLE_CAPACITY, &
                     ANNU_PLANNING_INTRPT_CAPACITY, &
                     ANNUAL_STORAGE_CAPACITY, &
                     NUM_CALLS, &
                     NUM_MONTH_CALLS, &
                     NUM_ANNUAL_CALLS, &
                     ANNUAL_CAPACITY, &
                     NUM_PUTS, &
                     NUM_STORAGE, &
                     ANN_NUM_STOR, &
                     SCEN_NUM_STOR, &
                     ANN_STOR_POS, &
                     SCEN_STOR_POS, &
                     NUM_LF_CALLS, &
                     NUM_LF_PUTS, &
                     NUM_MONTH_PUTS, &
                     NUM_ANNUAL_PUTS, &
                     NUM_INTER_PUTS, &
                     NUM_INTER_CALLS, &
                     FORWARD_POSITION, &
                     CALL_POSITION, &
                     MONTH_CALL_POSITION, &
                     ANNUAL_CALL_POSITION, &
                     PUT_POSITION, &
                     MONTH_PUT_POSITION, &
                     ANNUAL_PUT_POSITION, &
                     STORAGE_POSITION, &
                     BATTERY_POSITION, &
                     LF_CALL_POSITION, &
                     LF_PUT_POSITION, &
                     INTER_PUT_POSITION, &
                     INTER_CALL_POSITION, &
                     CONTRACT_DATE, &
                     DISTINGUISHING, &
                     REPORTING_GENERATION_GROUP, &
                     EXPENSE_ASSIGNMENT, &
                     EXPENSE_COLLECTION, &
                     ASSET_CLASS_ID, &
                     ASSET_ALLOCATION_VECTOR, &
                     QUANTITY_OF_PRODUCT, &
                     PROPOSED_QUANT_OF_PRODUCT, &
                     SAVED_QUANT_OF_PRODUCT, &
                     HOURLY_QUANTITY, &
                     ENERGY_PRICE, &
                     MONTHLY_ENERGY_PRICE, &
                     CONTINGENT_CAPACITY, &
                     MONTHLY_2ND_ENERGY_PRICE, &
                     PRICE_TYPE, &
                     SECOND_ENERGY_PRICE, &
                     MAXIMUM_STRIKES, &
                     BILLING_LAG, &
                     MINIMUM_STRIKES, &
                     DOLLAR_KW_DAY, &
                     DOLLAR_KW_DAY_ESC, &
                     DOLLAR_KW_MONTH, &
                     DOLLAR_KW_MONTH_ESC, &
                     DOLLAR_KW_YEAR, &
                     GRX_DOLLAR_KW_YEAR, &
                     GRX_DOLLAR_KW_MONTH, &
                     DOLLAR_KW_YEAR_ESC, &
                     DOLLAR_MONTH, &
                     DOLLAR_MONTH_ESC, &
                     DOLLAR_DEAL, &
                     DOLLAR_DEAL_ESC, &
                     BEGIN_DAY, &
                     BEGIN_DAY_IN_MONTH, &
                     BEGIN_EP, &
                     END_DAY, &
                     END_DAY_IN_MONTH, &
                     END_EP, &
                     ACTIVE_IN_MONTH, &
                     TRANS_TO_ACTIVE_MONTH, &
                     ACTIVE_IN_YEAR, &
                     ACTIVE_IN_YEAR_INDEX, &
                     TRANS_E_P_INDEX, &
                     TRANS_EP_2_TRANS_GROUPS, &
                     TRANS_EP_2_TG, &
                     YEARS_BEG_DAY_IN_MO, &
                     YEARS_END_DAY_IN_MO, &
                     rps_program_numbers, &
                     DailyStorageDischargeLimit, &
                     EV_ID_HYBRID_BATTERY, &
                     HYBRID_BATTERY, &
                     BATTERY, &
                     ANNUAL_HYBRID_BATTERY, &
                     ANNUAL_HYBRID_INDEX, &
                     HYBRID_INDEX, &
                     RenewableEnergyPercent)
            ALLOCATE(TRANSACTION_NAME(NUM_TRANSACTIONS))
            ALLOCATE(MONTHLY_USER_CF(NUM_TRANSACTIONS))
            ALLOCATE(MONTHLY_USER_CF_COUNTER(NUM_TRANSACTIONS))
            ALLOCATE(MONTHLY_CF_TRANS(NUM_TRANSACTIONS))
            ALLOCATE(USER_CF(NUM_TRANSACTIONS))
            ALLOCATE(COUNTERPARTY_NAME(NUM_TRANSACTIONS))
            ALLOCATE(TRANSACTION_ID(NUM_TRANSACTIONS))
            ALLOCATE(TRANSACTION_GROUP(NUM_TRANSACTIONS))
            ALLOCATE(TRANSACTION_TYPE(NUM_TRANSACTIONS))
            ALLOCATE(DERIVATIVE_TYPE(NUM_TRANSACTIONS))
            ALLOCATE(PRODUCT_INDEX(NUM_TRANSACTIONS))
            ALLOCATE(TRANSACTION_CLASS(NUM_TRANSACTIONS))
            ALLOCATE(PRODUCT_TYPE(NUM_TRANSACTIONS))
            ALLOCATE(OPTION_POSITION(NUM_TRANSACTIONS))
            ALLOCATE(PRODUCT_ACTIVE(NUM_TRANSACTIONS))
            ALLOCATE(STRIKE_FREQUENCY(NUM_TRANSACTIONS))
            ALLOCATE(REPORT_PRODUCT(NUM_TRANSACTIONS))
            ALLOCATE(UNIT_CONTINGENCY(NUM_TRANSACTIONS))
            ALLOCATE(UNIT_CONTINGENT_LINK(NUM_TRANSACTIONS))
            ALLOCATE(ENERGY_MULTIPLIER(NUM_TRANSACTIONS))
            ALLOCATE(TRANS_ACTIVE_IN_ENDPOINT(NUM_TRANSACTIONS))
            ALLOCATE(HEAT_RATE_FOR_SPREAD(NUM_TRANSACTIONS))
            ALLOCATE(USER_DAY_TYPES_ID(NUM_TRANSACTIONS))
            ALLOCATE(PUMPING_CAPACITY(NUM_TRANSACTIONS))
            ALLOCATE(PUMPING_STORAGE_EFFICIENCY(NUM_TRANSACTIONS))
            ALLOCATE(DAILY_PUMPING_MULT(NUM_TRANSACTIONS))
            ALLOCATE(FUEL_TYPE(NUM_TRANSACTIONS))
            ALLOCATE(FUEL_PRICE_TYPE(NUM_TRANSACTIONS))
            ALLOCATE(FUEL_PRICE(NUM_TRANSACTIONS))
            ALLOCATE(TRANSPORTATION_BASIS_TYPE(NUM_TRANSACTIONS))
            ALLOCATE(TRANSPORTATION_BASIS(NUM_TRANSACTIONS))
            ALLOCATE(DELIVERY_ADDER(NUM_TRANSACTIONS))
            ALLOCATE(COUNTERPARTY_BOND_RATING(NUM_TRANSACTIONS))
            ALLOCATE(DOLLAR_MWH(NUM_TRANSACTIONS))
            ALLOCATE(WVPA_RATE_TRACKER(NUM_TRANSACTIONS))
            ALLOCATE(WVPA_MEM_TRACKER(NUM_TRANSACTIONS), &
                     UNIT_CONTING_MARKET_SUPPLEMENT(NUM_TRANSACTIONS), &
                     DERIV_PRIMARY_MOVER(NUM_TRANSACTIONS), &
                     CAPACITY_PLANNING_FACTOR(NUM_TRANSACTIONS), &
                     UNIT_CATEGORY(NUM_TRANSACTIONS), &
                     DERIV_ZONE_ID(NUM_TRANSACTIONS))
            ALLOCATE(Tax_Credit_Begin_Date(NUM_TRANSACTIONS), &
                     Tax_Credit_End_Date(NUM_TRANSACTIONS), &
                     LHS_INDEX(NUM_TRANSACTIONS), &
                     External_Unit_ID(NUM_TRANSACTIONS), &
                     Holding_Company_ID(NUM_TRANSACTIONS), &
                     Production_Tax_Credit(NUM_TRANSACTIONS), &
                     RPS_Contribution(NUM_TRANSACTIONS), &
                     Percentage_Owned(NUM_TRANSACTIONS), &
                     Fuel_Category(NUM_TRANSACTIONS), &
                     GRX_ID(NUM_TRANSACTIONS), &
                     GRX_ID_INDEX(10000), &
                     State_TG_Index(NUM_TRANSACTIONS), &
                     State_Index(NUM_TRANSACTIONS), &
                     PROPOSED_RESOURCE_INDEX(NUM_TRANSACTIONS), &
                     PROPOSED_RESOURCE_POSITION(NUM_TRANSACTIONS))

            ALLOCATE(GRX_RPS_CAPACITY(NUM_TRANSACTIONS,30))
            ALLOCATE(SAVE_GRX_RPS_CAPACITY(NUM_TRANSACTIONS,30))
            ALLOCATE(GRX_STORAGE_PATTERN(NUM_OF_TRANS_CLASSES,8760))
            ALLOCATE(GRX_INDEP_PATTERN(NUM_OF_TRANS_CLASSES,8760))
            ALLOCATE(SAVE_GRX_STORAGE_PATTERN &
                                         (NUM_OF_TRANS_CLASSES,8760))
            ALLOCATE(ENERGY_PRICE_MULTIPLIER(NUM_TRANSACTIONS))
            ALLOCATE(MAX_QUANTITY_OF_PRODUCT(NUM_TRANSACTIONS))
            ALLOCATE(MONTHLY_ENERGY_MULT(NUM_TRANSACTIONS))
            ALLOCATE(CONTRACT_DATE(NUM_TRANSACTIONS))
            ALLOCATE(DISTINGUISHING(NUM_TRANSACTIONS))
            ALLOCATE(REPORTING_GENERATION_GROUP(NUM_TRANSACTIONS))
            ALLOCATE(EXPENSE_ASSIGNMENT(NUM_TRANSACTIONS))
            ALLOCATE(EXPENSE_COLLECTION(NUM_TRANSACTIONS))
            ALLOCATE(ASSET_CLASS_ID(NUM_TRANSACTIONS))
            ALLOCATE(ASSET_ALLOCATION_VECTOR(NUM_TRANSACTIONS))
            ALLOCATE(QUANTITY_OF_PRODUCT(NUM_TRANSACTIONS))
            ALLOCATE(PROPOSED_QUANT_OF_PRODUCT(NUM_TRANSACTIONS))
            ALLOCATE(SAVED_QUANT_OF_PRODUCT(NUM_TRANSACTIONS))
            ALLOCATE(HOURLY_QUANTITY(NUM_TRANSACTIONS))
            ALLOCATE(ENERGY_PRICE(NUM_TRANSACTIONS))
            ALLOCATE(MONTHLY_ENERGY_PRICE(NUM_TRANSACTIONS))
            ALLOCATE(CONTINGENT_CAPACITY(24,NUM_TRANSACTIONS))
            ALLOCATE(MONTHLY_2ND_ENERGY_PRICE(NUM_TRANSACTIONS))
            ALLOCATE(PRICE_TYPE(NUM_TRANSACTIONS))
            ALLOCATE(SECOND_ENERGY_PRICE(NUM_TRANSACTIONS))
            ALLOCATE(MAXIMUM_STRIKES(NUM_TRANSACTIONS))
            ALLOCATE(BILLING_LAG(NUM_TRANSACTIONS))
            ALLOCATE(MINIMUM_STRIKES(NUM_TRANSACTIONS))
            ALLOCATE(DOLLAR_KW_DAY(NUM_TRANSACTIONS))
            ALLOCATE(DOLLAR_KW_DAY_ESC(NUM_TRANSACTIONS))
            ALLOCATE(DOLLAR_KW_MONTH(NUM_TRANSACTIONS))
            ALLOCATE(DOLLAR_KW_MONTH_ESC(NUM_TRANSACTIONS))
            ALLOCATE(DOLLAR_KW_YEAR(NUM_TRANSACTIONS))
            ALLOCATE(GRX_DOLLAR_KW_YEAR(NUM_TRANSACTIONS))
            ALLOCATE(GRX_DOLLAR_KW_MONTH(NUM_TRANSACTIONS))
            ALLOCATE(DOLLAR_KW_YEAR_ESC(NUM_TRANSACTIONS))
            ALLOCATE(DOLLAR_MONTH(NUM_TRANSACTIONS))
            ALLOCATE(DOLLAR_MONTH_ESC(NUM_TRANSACTIONS))
            ALLOCATE(DOLLAR_DEAL(NUM_TRANSACTIONS))
            ALLOCATE(DOLLAR_DEAL_ESC(NUM_TRANSACTIONS))
            ALLOCATE(BEGIN_DAY(NUM_TRANSACTIONS))
            ALLOCATE(BEGIN_DAY_IN_MONTH(NUM_TRANSACTIONS))
            ALLOCATE(BEGIN_EP(NUM_TRANSACTIONS))
            ALLOCATE(END_DAY(NUM_TRANSACTIONS))
            ALLOCATE(END_DAY_IN_MONTH(NUM_TRANSACTIONS))
            ALLOCATE(END_EP(NUM_TRANSACTIONS))
            ALLOCATE(ACTIVE_IN_MONTH(NUM_TRANSACTIONS))
            ALLOCATE(TRANS_TO_ACTIVE_MONTH(NUM_TRANSACTIONS))
            ALLOCATE(ACTIVE_IN_YEAR(NUM_TRANSACTIONS))
            ALLOCATE(ACTIVE_IN_YEAR_INDEX(NUM_TRANSACTIONS))
            ALLOCATE(ASSET_CLASS_GROUPS_INDEX(0:MAX_ASSET_GROUPS))
            ALLOCATE(ASSET_2_TRANS_INDEX(0:MAX_ASSET_GROUPS, &
                                                      MAX_TRANS_GROUPS))
            ALLOCATE(NUMBER_ASSET_2_TRANS(MAX_TRANS_GROUPS))
            ALLOCATE(FIRST_AC_TG(0:MAX_ASSET_GROUPS))
            ALLOCATE(NUM_FORWARDS(NUM_OF_TRANS_CLASSES))
            ALLOCATE(MONTHLY_CONTINGENT_CAPACITY(NUM_OF_TRANS_CLASSES))
            ALLOCATE(ANNUAL_INTERRUPTIBLE_CAPACITY( &
                                                 NUM_OF_TRANS_CLASSES))
            ALLOCATE(ANNU_PLANNING_INTRPT_CAPACITY( &
                                                 NUM_OF_TRANS_CLASSES))
            ALLOCATE(ANNUAL_STORAGE_CAPACITY(NUM_OF_TRANS_CLASSES))
            ALLOCATE(NUM_CALLS(NUM_OF_TRANS_CLASSES))
            ALLOCATE(NUM_MONTH_CALLS(NUM_OF_TRANS_CLASSES))
            ALLOCATE(NUM_ANNUAL_CALLS(NUM_OF_TRANS_CLASSES))
            ALLOCATE(ANNUAL_CAPACITY(744,12,NUM_OF_TRANS_CLASSES))
            ALLOCATE(NUM_PUTS(NUM_OF_TRANS_CLASSES))
            ALLOCATE(NUM_STORAGE(NUM_OF_TRANS_CLASSES))
            ALLOCATE(ANN_NUM_STOR(NUM_OF_TRANS_CLASSES))
            ALLOCATE(SCEN_NUM_STOR(NUM_OF_TRANS_CLASSES))
            ALLOCATE(ANN_STOR_POS(NUM_TRANSACTIONS, &
                                                 NUM_OF_TRANS_CLASSES))
            ALLOCATE(SCEN_STOR_POS(NUM_TRANSACTIONS, &
                                                 NUM_OF_TRANS_CLASSES))
            ALLOCATE(NUM_LF_CALLS(NUM_OF_TRANS_CLASSES))
            ALLOCATE(NUM_LF_PUTS(NUM_OF_TRANS_CLASSES))
            ALLOCATE(NUM_MONTH_PUTS(NUM_OF_TRANS_CLASSES))
            ALLOCATE(NUM_ANNUAL_PUTS(NUM_OF_TRANS_CLASSES))
            ALLOCATE(NUM_INTER_PUTS(NUM_OF_TRANS_CLASSES))
            ALLOCATE(NUM_INTER_CALLS(NUM_OF_TRANS_CLASSES))
            ALLOCATE(FORWARD_POSITION(NUM_TRANSACTIONS, &
                                                 NUM_OF_TRANS_CLASSES))
            ALLOCATE(CALL_POSITION(NUM_TRANSACTIONS, &
                                                 NUM_OF_TRANS_CLASSES))
            ALLOCATE(MONTH_CALL_POSITION(NUM_TRANSACTIONS, &
                                                 NUM_OF_TRANS_CLASSES))
            ALLOCATE(ANNUAL_CALL_POSITION(NUM_TRANSACTIONS, &
                                                 NUM_OF_TRANS_CLASSES))
            ALLOCATE(PUT_POSITION(NUM_TRANSACTIONS, &
                                                 NUM_OF_TRANS_CLASSES))
            ALLOCATE(MONTH_PUT_POSITION(NUM_TRANSACTIONS, &
                                                 NUM_OF_TRANS_CLASSES))
            ALLOCATE(ANNUAL_PUT_POSITION(NUM_TRANSACTIONS, &
                                                 NUM_OF_TRANS_CLASSES))
            ALLOCATE(STORAGE_POSITION(NUM_TRANSACTIONS, &
                                                 NUM_OF_TRANS_CLASSES))
            ALLOCATE(BATTERY_POSITION(NUM_TRANSACTIONS, &
                                                 NUM_OF_TRANS_CLASSES))
            ALLOCATE(LF_CALL_POSITION(NUM_TRANSACTIONS, &
                                                 NUM_OF_TRANS_CLASSES))
            ALLOCATE(LF_PUT_POSITION(NUM_TRANSACTIONS, &
                                                 NUM_OF_TRANS_CLASSES))
            ALLOCATE(INTER_PUT_POSITION(NUM_TRANSACTIONS, &
                                                 NUM_OF_TRANS_CLASSES))
            ALLOCATE(INTER_CALL_POSITION(NUM_TRANSACTIONS, &
                                                 NUM_OF_TRANS_CLASSES))
            ALLOCATE(TRANS_E_P_INDEX(MAX_TRANS_GROUP_NUM))
            ALLOCATE(TRANS_EP_2_TRANS_GROUPS(MAX_TRANS_GROUP_NUM))
            ALLOCATE(TRANS_EP_2_TG(MAX_TRANS_GROUP_NUM))
            ALLOCATE(YEARS_BEG_DAY_IN_MO(NUM_TRANSACTIONS,0:12))
            ALLOCATE(YEARS_END_DAY_IN_MO(NUM_TRANSACTIONS,0:12))
! 072119.
            CALL AllocateArray(HourlyProductFilePtr,NUM_TRANSACTIONS)
            HourlyProductFilePtr = -1
            CALL AllocateArray(RefUsed,NUM_TRANSACTIONS)
            RefUsed = ""
            CALL AllocateArray(HourlyProdFileNames,NUM_TRANSACTIONS)
            HourlyProdFileNames =""
            CALL AllocateArray(HourlyProdRefNumber, &
                                       1,INT(NUM_TRANSACTIONS,4),1,31)
            HourlyProdRefNumber = -1
            CALL AllocateArray(HourlyProdPtrToCFLoc,NUM_TRANSACTIONS)
            HourlyProdPtrToCFLoc = -1
            CALL AllocateArray(HourlyProdPtrCFLocToProd, &
                                                    NUM_TRANSACTIONS)
            ALLOCATE(RPS_PROGRAM_NUMBERs(NUM_TRANSACTIONS), &
                     DailyStorageDischargeLimit(NUM_TRANSACTIONS), &
                     EV_ID_HYBRID_BATTERY(NUM_TRANSACTIONS), &
                     HYBRID_BATTERY(NUM_TRANSACTIONS), &
                     BATTERY(NUM_TRANSACTIONS), &
                     ANNUAL_HYBRID_BATTERY(NUM_TRANSACTIONS), &
                     ANNUAL_HYBRID_INDEX(10000), &
                     RenewableEnergyPercent(NUM_TRANSACTIONS), &
                     HYBRID_INDEX(10000))

         ActiveDirvs =INT(NUM_TRANSACTIONS,4)

!         CALL AllocateArray(REFERENCE_LOAD_NAME,ActiveDirvs)
!         CALL AllocateArray(REFERENCE_LOAD_NUMBER,ActiveDirvs)
         CALL AllocateArray(GRX_BUILD_VECTOR,ActiveDirvs)

         CALL AllocateArray(DerivCapMarketMonth,ActiveDirvs)
         CALL AllocateArray(DerivCapMarketPointer,ActiveDirvs)
         CALL AllocateArray(DerivIncRevenuePtr,ActiveDirvs)
         CALL AllocateArray(DerivCapExpenseCollection,ActiveDirvs)
         CALL AllocateArray(DerivCapAdjustmentFactor,ActiveDirvs)
         CALL AllocateArray(DerivCapPlanningFactor,ActiveDirvs)
         CALL AllocateArray(DerivLMPActive,ActiveDirvs)
         CALL AllocateArray(DerivLMPPeakSlope,ActiveDirvs)
         CALL AllocateArray(DerivLMPPeakConst,ActiveDirvs)
         CALL AllocateArray(DerivLMPOffPeakSlope,ActiveDirvs)
         CALL AllocateArray(DerivLMPOffPeakConst,ActiveDirvs)
         CALL AllocateArray(DerivCalCongestion,ActiveDirvs)
         CALL AllocateArray(DerivIncExpensePtr,ActiveDirvs)
         CALL AllocateArray(DerivCapMarketType,ActiveDirvs)
         CALL AllocateArray(LMPDerivPriceFileNames,ActiveDirvs)
         CALL AllocateArray(LMPDerivPriceFilePointer,ActiveDirvs)
         CALL AllocateArray(DerivIncExpense,0,12,1,ActiveDirvs)
         CALL AllocateArray(DerivIncRevenue,0,12,1,ActiveDirvs)
         CALL AllocateArray(DerivPeakPeriodLMPrice,0,12,1,ActiveDirvs)
         CALL AllocateArray(DerivOffPeakLMPrice,0,12,1,ActiveDirvs)
         CALL AllocateArray(DerivLMPMoPeakSlope,1,12,1,ActiveDirvs)
         CALL AllocateArray(DerivLMPMoPeakConst,1,12,1,ActiveDirvs)
         CALL AllocateArray(DerivLMPMoOffPeakSlope,1,12,1,ActiveDirvs)
         CALL AllocateArray(DerivLMPMoOffPeakConst,1,12,1,ActiveDirvs)
         LMPDerivPriceFileNames = " "
         LMPDerivPriceFilePointer = 0
         LMPDerivActivePriceFiles = 0
         DerivLMPMoPeakSlope = 1.
         DerivLMPMoPeakConst = 0.
         DerivLMPMoOffPeakSlope = 1.
         DerivLMPMoOffPeakConst = 0.
         DerivCalCongestion = .FALSE.
         DerivLMPActive = .FALSE.

         TEMP_I2 = ResetActiveHourlyProducts()

            MONTHLY_ENERGY_PRICE = 0.
            MONTHLY_ENERGY_MULT = 0.

            CONTINGENT_CAPACITY = 0.
            PRODUCT_ACTIVE = 'X'

            BEGIN_EP = 0
            END_EP = 0

            DERIVATIVE_TYPE = 0
            PRODUCT_INDEX = 0
            TRANS_E_P_INDEX = 0
            TRANS_EP_2_TRANS_GROUPS = 0
            TRANS_EP_2_TG = 0

            DOLLAR_KW_YEAR = 0.0
            GRX_DOLLAR_KW_YEAR = 0.0
            GRX_DOLLAR_KW_MONTH = 0.0
            DOLLAR_KW_MONTH = 0.0

            ASSET_CLASS_GROUPS_INDEX = 0

            ! WHICH ASSET CLASSES BELONG TO A TRANSACTION GROUP
            ASSET_2_TRANS_INDEX = 0
            ! HOW MANY ASSET CLASSES BELONG TO THE TRANSACTION GROUP
            NUMBER_ASSET_2_TRANS = 0
            FIRST_AC_TG = 0

            ENERGY_PRICE_MULTIPLIER = 0.
            MAX_QUANTITY_OF_PRODUCT = 999999.
            PROPOSED_QUANT_OF_PRODUCT = -999999.
            SAVED_QUANT_OF_PRODUCT = 0.

            FISCAL_ACTIVE_TRANSACTIONS = 0

            MAX_TRANS_E_P = 0
            MAX_ASSET_CLASS_GROUPS = 0

            DERIV_PRIMARY_MOVER = 0

            Tax_Credit_Begin_Date = 0
            Tax_Credit_End_Date = 0
            LHS_INDEX = 0
            External_Unit_ID = 0
            Holding_Company_ID = 0
            DERIV_ZONE_ID = '      '
            Production_Tax_Credit = 0.0
            RPS_Contribution = 0.0
            Percentage_Owned = 100.0
            Fuel_Category = '          '
            GRX_ID = 0
            GRX_ID_INDEX = 0
            State_TG_Index = 0
            State_Index = 0
            RPS_TRANS_DB = 0.0
            GRX_RPS_TRANS_DB = 0.0
            GRX_RPS_CAPACITY = 0.0
            SAVE_GRX_RPS_CAPACITY = 0.0
            GRX_STORAGE_PATTERN = 0.0
            GRX_INDEP_PATTERN = 0.0
            SAVE_GRX_STORAGE_PATTERN = 0.0
            PROPOSED_RESOURCE_INDEX = 0
            PROPOSED_RESOURCE_POSITION = 0
            USER_CF = 0
            GRX_BUILD_VECTOR_INDEX = 0
            DailyStorageDischargeLimit = 0.0
            RenewableEnergyPercent = 0.0
            EV_ID_HYBRID_BATTERY = 0
            HYBRID_BATTERY = 0
            BATTERY_COUNT = 0
            SCEN_NUM_STOR = 0
            SCEN_STOR_POS = 0
            BATTERY = 0
            BATTERY_POSITION = 0
            HYBRID_INDEX = 0
            HYBRID_BATTERY_COUNT = 0
            HYBRID_INDEP_COUNT = 0

            TRANS = 1
            RI = 0
            DO FILE_ID = 0, MAX_ENRG_PRODUCT_FILES-1
               CALL OPEN_ENERGY_PRODUCTS_FILE(FILE_ID,FILE_ACTIVE)
               IF(.NOT. FILE_ACTIVE) CYCLE
               IREC = 1
               DO
                  READ(10,REC=IREC,IOSTAT=IOS) DELETE, &
                     TRANSACTION_NAME(TRANS), &
                     COUNTERPARTY_NAME(TRANS), &
                     TRANSACTION_ID(TRANS), &
                     TRANSACTION_GROUP(TRANS), &
                     TRANSACTION_TYPE(TRANS), &
                     TRANSACTION_CLASS(TRANS), &
                     PRODUCT_TYPE(TRANS), &
                     REPORTING_GENERATION_GROUP(TRANS), &
                     BEGIN_DATE, &
                     END_DATE, &
                     EXPENSE_ASSIGNMENT(TRANS), &
                     EXPENSE_COLLECTION(TRANS), &
                     ASSET_CLASS_ID(TRANS), &
                     ASSET_ALLOCATION_VECTOR(TRANS), &
                     QUANTITY_OF_PRODUCT(TRANS), &
                     ENERGY_PRICE(TRANS), &
                     PRICE_TYPE(TRANS), &
                     SECOND_ENERGY_PRICE(TRANS), &
                     DOLLAR_KW_DAY(TRANS), &
                     DOLLAR_KW_DAY_ESC(TRANS), &
                     DOLLAR_KW_MONTH(TRANS), &
                     DOLLAR_KW_MONTH_ESC(TRANS), &
                     DOLLAR_KW_YEAR(TRANS), &
                     DOLLAR_KW_YEAR_ESC(TRANS), &
                     DOLLAR_MONTH(TRANS), &
                     DOLLAR_MONTH_ESC(TRANS), &
                     DOLLAR_DEAL(TRANS), &
                     DOLLAR_DEAL_ESC(TRANS), &
                     COMMENT, &
                     CONTRACT_DATE(TRANS), &
                     OPTION_POSITION(TRANS), &
                     DISTINGUISHING(TRANS), &
                     PRODUCT_ACTIVE(TRANS), &
                     STRIKE_FREQUENCY(TRANS), &
                     MAXIMUM_STRIKES(TRANS), &
                     BILLING_LAG(TRANS), &
                     MINIMUM_STRIKES(TRANS), &
                     REPORT_PRODUCT(TRANS), &
                     UNIT_CONTINGENCY(TRANS), &
                     UNIT_CONTINGENT_LINK(TRANS), &
                     ENERGY_MULTIPLIER(TRANS), &
                     HEAT_RATE_FOR_SPREAD(TRANS), &
                     USER_DAY_TYPES_ID(TRANS), &
                     PUMPING_CAPACITY(TRANS), &
                     PUMPING_STORAGE_EFFICIENCY(TRANS), &
                     DAILY_PUMPING_MULT(TRANS), &
                     FUEL_TYPE(TRANS), &
                     FUEL_PRICE_TYPE(TRANS), &
                     FUEL_PRICE(TRANS), &
                     TRANSPORTATION_BASIS_TYPE(TRANS), &
                     TRANSPORTATION_BASIS(TRANS), &
                     DELIVERY_ADDER(TRANS), &
                     COUNTERPARTY_BOND_RATING(TRANS), &
                     DOLLAR_MWH(TRANS), &
                     WVPA_RATE_TRACKER(TRANS), &
                     ENERGY_PRICE_MULTIPLIER(TRANS), &
                     MAX_QUANTITY_OF_PRODUCT(TRANS), &
                     WVPA_MEM_TRACKER(TRANS), &
                     CAPACITY_PLANNING_FACTOR(TRANS), &
                     UNIT_CATEGORY(TRANS), & !  60 PRIM MOVER
                     IS_AN_EXCHANGE_AGREEMENT, &
                     JOINT_TRANSACTION_ID, &
                     EXCHANGE_TYPE, &
                     ANNUAL_ENERGY_PLANNING_FACTOR, & !  64
                     DELIVERY_ZONE,    & !  65 changed from 59
                     ENERGY_RETURN_LATER_IN_DAY, & !  66
                     ENERGY_RETURN_MW,  & !  67
                     MINIMUM_ENERGY_LIMIT_MW, & !  68-70
                     MAXIMUM_ENERGY_LIMIT_MW, & !  71-73
                     RESERVES_INCLUDED,  & !  74 changed from 60
                     UnitConstructionCostUSDKW, &
                     ConstructionCostEscalation, &
                     Year1CapitalRecoveryRate, &
                     Year1RecoveryRateEscalation, &
                     BookandServiceLife, &
                     FirstYearAvailable, &
                     LastYearAvailable, &
                     MinimumAnnualUnits, &
                     MaximumAnnualUnits, &
                     MaximumCumulativeUnits, &
                     ContinuousBuildFirstYear,  & !  85
                     UNIT_CONTING_MARKET_SUPPLEMENT(TRANS), &
                     New_Build_Unit_Status, &
                     Production_Tax_Credit(TRANS), &
                     Tax_Credit_Begin_Date(TRANS), &
                     Tax_Credit_End_Date(TRANS), & !  90
                     Latin_Hypercube_Dist, &
                     State_TG_Key, &
                     EIA_Plant_ID, &
                     Owner_ID, &
                     Holding_Company_ID(TRANS), & !  95
                     RPS_Contribution(TRANS), &
                     NERC_Subregion, &
                     State_Province, &
                     External_Unit_ID(TRANS), &
                     Fuel_Category(TRANS), & !  100
                     Percentage_Owned(TRANS), &
                     GRX_ID(TRANS), &
                     DERIV_GUID, &
                     DERIV_PARENT_ID, &
                     DERIV_AGGREGATED_UNIT, &
                     DERIV_ZONE_ID(TRANS), &
                     USER_CF(TRANS), &
                     DerivCapMarketPointer(TRANS), &
                     IncRevenueAssignment, &
                     DerivCapExpenseCollection(TRANS), & !  110
                     DerivCapAdjustmentFactor(TRANS), &
                     DerivCapPlanningFactor(TRANS), &
                     LMPActive, &
                     LMPMarketHubID, &
                     DerivLMPPeakSlope(TRANS),   & !  115
                     DerivLMPPeakConst(TRANS), &
                     DerivLMPOffPeakSlope(TRANS), &
                     DerivLMPOffPeakConst(TRANS), &
                     LMPScenarioNum, &
                     LMPCalc,  & !  120
                     IncExpenseAssignment, &
                     DerivCapMarketType(TRANS), &
                     DerivCapMarketMonth(TRANS), &
                     ProductHourlyRefName, &
                     ProductHourlyRefNumber, & !  125
                     GRX_BUILD_VECTOR(TRANS), &
                     UNIT_TYPE, &
                     UNIT_TYPE_CATEGORY, &
                     rps_program_numbers(TRANS), &
                     DailyStorageDischargeLimit(TRANS), &
                     EV_ID_HYBRID_BATTERY(TRANS), &
                     RenewableEnergyPercent(TRANS)
                  if(irec == 200) then
                    irec=irec ! debugstop
                  end if
                  IF(IOS /= 0) EXIT

                  IREC = IREC + 1
                  TRANSACTION_GROUP(TRANS) = &
                          GET_BELONGS_TO_GROUP(TRANSACTION_GROUP(TRANS))

                  IF (PRODUCT_ACTIVE(TRANS) /= 'T' .AND. &
                           PRODUCT_ACTIVE(TRANS) /= 'P' .AND. &
                                     PRODUCT_ACTIVE(TRANS) /= 'E') CYCLE

! DOUBLE INDEX

                  TG = TRANSACTION_GROUP(TRANS)
                  TG = GET_TRANS_GROUP_POSITION(TG)
                  IF(TG == 0) then
                    CYCLE
                  end if
! Hourly Product File is tracked
                  IF(INDEX(TRIM(PRODUCT_TYPE(TRANS)),'Hourly') /= 0)THEN
                     HourlyProductFilePtr(TRANS) = &
                         StoreHourlyProductFiles(ProductHourlyRefName, &
                                                 ProductHourlyRefNumber, &
                                                 ScenarioLoadShape(1:2))
                  ENDIF

                  RenewableEnergyPercent(TRANS) = &
                                    RenewableEnergyPercent(TRANS) * 0.01

                  IF(EV_ID_HYBRID_BATTERY(TRANS) > 0) THEN
                    HYBRID_BATTERY_COUNT = HYBRID_BATTERY_COUNT + 1
                    HYBRID_BATTERY(TRANS) = HYBRID_BATTERY_COUNT
                  ENDIF

                  GRX_BUILD_VECTOR(TRANS) = ABS(GRX_BUILD_VECTOR(TRANS))
                  IF(PRODUCT_ACTIVE(TRANS) == 'P') THEN
                     RI = RI + 1
                     PROPOSED_RESOURCE_INDEX(RI) =  TRANS
                     PROPOSED_RESOURCE_POSITION(TRANS) = RI
!                     BEGIN_DATE = 123154
                     PROPOSED_QUANT_OF_PRODUCT(TRANS) = &
                                              QUANTITY_OF_PRODUCT(TRANS)
                     SAVED_QUANT_OF_PRODUCT(TRANS) = &
                                              QUANTITY_OF_PRODUCT(TRANS)
                     QUANTITY_OF_PRODUCT(TRANS) = 0.0
!
                     IF(GRX_ID(TRANS) <= 10000) THEN
                        IF(GRX_ID_INDEX(GRX_ID(TRANS)) > 0) THEN
                           WRITE(4,*) 'MORE THAN ONE PROPOSED'
                           WRITE(4,*) 'DERIVATIVE HAS THE SAME'
                           WRITE(4,*) 'RESOURCE ID',GRX_ID(TRANS)
                           WRITE(4,*) 'ONLY THE FIRST OCCURANCE'
                           WRITE(4,*) 'WILL BE USED'
                        ELSE
                           GRX_ID_INDEX(GRX_ID(TRANS)) = TRANS
                        ENDIF
                        IF( GRX_BUILD_VECTOR(TRANS) > 0 .AND. &
                                   GRX_BUILD_VECTOR(TRANS) < 32000) THEN
                           GRX_BUILD_VECTOR_INDEX( &
                                        GRX_BUILD_VECTOR(TRANS)) = TRANS
                        ENDIF
                     ENDIF
                     GRX_DOLLAR_KW_YEAR(TRANS) = DOLLAR_KW_YEAR(TRANS)
                     GRX_DOLLAR_KW_MONTH(TRANS) = DOLLAR_KW_MONTH(TRANS)
! ALTER CAPACITY?
! ALTER ON-LINE DATE?
                  ENDIF


                  PUMPING_STORAGE_EFFICIENCY(TRANS) = &
                                  PUMPING_STORAGE_EFFICIENCY(TRANS)/100.

                  HEAT_RATE_FOR_SPREAD(TRANS) = &
                                       HEAT_RATE_FOR_SPREAD(TRANS)/1000.

! E.G. BEGIN_DATE = 062398
                  BEGIN_MONTH = INT(BEGIN_DATE/10000)
                  BEGIN_DAY(TRANS) = INT(BEGIN_DATE/100) - &
                                                  BEGIN_MONTH*100
                  BEGIN_YEAR = INT(BEGIN_DATE - &
                                       BEGIN_MONTH*10000 - &
                                                   BEGIN_DAY(TRANS)*100)
! 032219. TEST.
                  IF(BEGIN_YEAR > 80) THEN
                     BEGIN_EP(TRANS) = BEGIN_YEAR*100 + BEGIN_MONTH
                  ELSE
                     BEGIN_EP(TRANS)=BEGIN_YEAR*100+10000 + BEGIN_MONTH
                  ENDIF

                  END_MONTH = INT(END_DATE/10000)
                  END_DAY(TRANS) = INT(END_DATE/100) - &
                                                  END_MONTH*100
                  END_YEAR = INT(END_DATE - &
                                       END_MONTH*10000 - &
                                                   END_DAY(TRANS)*100)
                  IF(END_YEAR > 80) THEN
                     END_EP(TRANS) = END_YEAR*100 + END_MONTH
                  ELSE
                     END_EP(TRANS) = END_YEAR*100 + 10000 + END_MONTH
                  ENDIF

                  AC = ASSET_CLASS_ID(TRANS)



                  IF(ASSET_CLASS_GROUPS_INDEX(AC) == 0) THEN
                     MAX_ASSET_CLASS_GROUPS = MAX_ASSET_CLASS_GROUPS + 1
                     ASSET_CLASS_GROUPS_INDEX(AC)=MAX_ASSET_CLASS_GROUPS
                  ENDIF
!

                  IF(TRANSACTION_TYPE(TRANS) == &
                                          'Physical Transaction' .OR. &
                     TRANSACTION_TYPE(TRANS) == &
                                           'Forward            '  ) THEN
                     DERIVATIVE_TYPE(TRANS) = 1
                  ELSEIF(TRANSACTION_TYPE(TRANS) == 'Physical Call' .OR. &
                         TRANSACTION_TYPE(TRANS) == 'Call         ')THEN
                     DERIVATIVE_TYPE(TRANS) = 2
                  ELSEIF(TRANSACTION_TYPE(TRANS) == 'Physical Put' .OR. &
                         TRANSACTION_TYPE(TRANS) == 'Put          ')THEN
                     DERIVATIVE_TYPE(TRANS) = 3
                  ELSEIF(TRANSACTION_TYPE(TRANS) == &
                                            'Financial Transactio') THEN
                     DERIVATIVE_TYPE(TRANS) = 4
                  ELSEIF(TRANSACTION_TYPE(TRANS)=='Financial Call') THEN
                     DERIVATIVE_TYPE(TRANS) = 5
                  ELSEIF(TRANSACTION_TYPE(TRANS)=='Financial Put') THEN
                     DERIVATIVE_TYPE(TRANS) = 6
                  ELSEIF(TRANSACTION_TYPE(TRANS) == &
                                              'Interruptible Call') THEN
                     DERIVATIVE_TYPE(TRANS) = 7
                  ELSEIF(TRANSACTION_TYPE(TRANS) == &
                                               'Interruptible Put') THEN
                     DERIVATIVE_TYPE(TRANS) = 8
                  ELSEIF(TRANSACTION_TYPE(TRANS) == &
                                               'Daily Storage    ') THEN
                     DERIVATIVE_TYPE(TRANS) = 9
                     SCEN_NUM_STOR(TG) = SCEN_NUM_STOR(TG) + 1

                     SCEN_STOR_POS(SCEN_NUM_STOR(TG),TG) = TRANS
                  ELSEIF(TRANSACTION_TYPE(TRANS) == &
                                               'Load Follow Call ') THEN
                     DERIVATIVE_TYPE(TRANS) = 10
                  ELSEIF(TRANSACTION_TYPE(TRANS) == &
                                               'Load Follow Put  ') THEN
                     DERIVATIVE_TYPE(TRANS) = 11
                  ELSEIF(TRANSACTION_TYPE(TRANS) == &
                                               'Market Physical  ') THEN
                     DERIVATIVE_TYPE(TRANS) = 12
                  ENDIF
!
                  IF(PRODUCT_TYPE(TRANS) == LOCAL_PRODUCT_TYPE(1)) THEN
                     PRODUCT_INDEX(TRANS) = 1
                  ELSEIF(PRODUCT_TYPE(TRANS)==LOCAL_PRODUCT_TYPE(2))THEN
                     PRODUCT_INDEX(TRANS) = 2
                  ELSEIF(PRODUCT_TYPE(TRANS)==LOCAL_PRODUCT_TYPE(3))THEN
                     PRODUCT_INDEX(TRANS) = 3
                  ELSEIF(PRODUCT_TYPE(TRANS)==LOCAL_PRODUCT_TYPE(4))THEN
                     PRODUCT_INDEX(TRANS) = 4
! DUMPS UNKNOWN PRODUCTS INTO TYPE 1
                  ELSEIF(PRODUCT_TYPE(TRANS)==LOCAL_PRODUCT_TYPE(5))THEN
                     PRODUCT_INDEX(TRANS) = 5
                  ELSEIF(PRODUCT_TYPE(TRANS)==LOCAL_PRODUCT_TYPE(6))THEN
                     PRODUCT_INDEX(TRANS) = 6
                  ELSEIF(PRODUCT_TYPE(TRANS)==LOCAL_PRODUCT_TYPE(8))THEN
                     PRODUCT_INDEX(TRANS) = 8
                  ELSE
                     PRODUCT_INDEX(TRANS) = 1
                  ENDIF
!
                  TEMP_CHAR6 = State_TG_Key(1:2)

                  
                  ST_TG = STATE_ID_LOOKUP(TEMP_CHAR6)
                  State_Index(TRANS) = ST_TG
                  State_TG_Index(TRANS) = ST_TG
                  ST_TG = STATE_ID_LOOKUP(State_TG_Key)
                  State_TG_Index(TRANS) = ST_TG
! NEED TO VERIFY THIS
                  Tax_Credit_Begin_Date(TRANS) = &
                                  10000 + Tax_Credit_Begin_Date(TRANS)
                  Tax_Credit_End_Date(TRANS) = &
                                  10000 + Tax_Credit_End_Date(TRANS)
! 041012.
                  LHS_INDEX(TRANS) = &
                       GET_REGIONAL_SCENARIO_INDEX(Latin_Hypercube_Dist)
!
                  TRANS = TRANS + 1
               ENDDO !END PRODUCTS LOOP
!
               CALL CLOSE_ENERGY_PRODUCTS_FILE
            ENDDO ! END FILE LOOP
!
            READ_ENERGY_PRODUCTS_DATA = .TRUE.
            IF( ALLOCATED(FISCAL_ENERGY_COST) ) &
               DEALLOCATE( &
                     FISCAL_ENERGY_COST, &
                     FISCAL_ENERGY, &
                     FISCAL_CAPACITY, &
                     FISCAL_ENERGY_REVENUE, &
                     FISCAL_TRANS_HOURS, &
                     FISCAL_PRODUCT_HOURS, &
                     FISCAL_PRODUCT_DAYS, &
                     FISCAL_PRODUCT_MONTHS, &
                     FISCAL_STRIKES, &
                     FISCAL_ACTIVE_IN_YEAR_INDEX, &
                     FISCAL_ACTIVE_IN_YEAR, &
                     FISCAL_TRANSACTION_COST, &
                     FISCAL_TRANSACTION_REVENUE )
            ALLOCATE(FISCAL_ENERGY_COST(NUM_TRANSACTIONS))
            ALLOCATE(FISCAL_ENERGY(NUM_TRANSACTIONS))
            ALLOCATE(FISCAL_CAPACITY(NUM_TRANSACTIONS))
            ALLOCATE(FISCAL_ENERGY_REVENUE(NUM_TRANSACTIONS))
            ALLOCATE(FISCAL_TRANS_HOURS(NUM_TRANSACTIONS))
            ALLOCATE(FISCAL_PRODUCT_HOURS(NUM_TRANSACTIONS))
            ALLOCATE(FISCAL_PRODUCT_DAYS(NUM_TRANSACTIONS))
            ALLOCATE(FISCAL_PRODUCT_MONTHS(NUM_TRANSACTIONS))
            ALLOCATE(FISCAL_STRIKES(NUM_TRANSACTIONS))
            ALLOCATE(FISCAL_ACTIVE_IN_YEAR_INDEX(NUM_TRANSACTIONS))
            ALLOCATE(FISCAL_ACTIVE_IN_YEAR(NUM_TRANSACTIONS))
            ALLOCATE(FISCAL_TRANSACTION_COST(NUM_TRANSACTIONS))
            ALLOCATE(FISCAL_TRANSACTION_REVENUE(NUM_TRANSACTIONS))
            FISCAL_ENERGY_COST = 0.
            FISCAL_ENERGY = 0.
            FISCAL_CAPACITY = 0.
            FISCAL_TRANS_HOURS = 0
            FISCAL_PRODUCT_HOURS = 0
            FISCAL_PRODUCT_DAYS = 0
            FISCAL_PRODUCT_MONTHS = 0
            FISCAL_STRIKES = 0
            FISCAL_ENERGY_REVENUE = 0.
            FISCAL_TRANSACTION_COST = 0.
            FISCAL_TRANSACTION_REVENUE = 0.
            FISCAL_ACTIVE_IN_YEAR_INDEX = 0
            FISCAL_ACTIVE_IN_YEAR = 0
         ENDIF
         SAVE_NUM_PROPOSED_UNITS = RI
         SAVE_ACTIVE_TRANSACTIONS = TRANS - 1
         SAVE_ENERGY_PRODUCTS_STATUS = READ_ENERGY_PRODUCTS_DATA

! 082221. 090521. NEGATIVE VALUE FOR INDEPENDENT.

         IF(HYBRID_BATTERY_COUNT > 0) THEN
            ! If there are hybrid batteries, run loop.
            DO I = 1, SAVE_ACTIVE_TRANSACTIONS
! If no battery is assigned at this location, continue to next item
               IF(EV_ID_HYBRID_BATTERY(I) < 1) CYCLE
               DO J = 1, SAVE_ACTIVE_TRANSACTIONS
                  IF(EV_ID_HYBRID_BATTERY(J) > -1) CYCLE
                  ! If hybrid battery index is assigned at this location
                  ! and that value is the inverse of the value found in the
                  ! outer loop, increment the independent counter,
                  ! point the hybrid battery index of J to I
                  ! and point the hybrid_index for I, to J.
                  IF(EV_ID_HYBRID_BATTERY(J) < 0 .AND. &
                       -EV_ID_HYBRID_BATTERY(J) == &
                                           EV_ID_HYBRID_BATTERY(I)) THEN
                     HYBRID_INDEP_COUNT = HYBRID_INDEP_COUNT + 1
                     HYBRID_INDEX(HYBRID_BATTERY(I)) = J
                     HYBRID_BATTERY(J) = -HYBRID_BATTERY(I)
                  ENDIF
                  IF(HYBRID_INDEP_COUNT == HYBRID_BATTERY_COUNT) EXIT
               ENDDO
               IF(HYBRID_INDEP_COUNT == HYBRID_BATTERY_COUNT) EXIT
            ENDDO
         ENDIF
         IF(HYBRID_INDEP_COUNT /= HYBRID_BATTERY_COUNT) THEN
            WRITE(9,*) 'MISMATCH IN NUMBER OF HYBRID PROJECTS'
            WRITE(9,*) 'NUMBER OF BATTERY HYBRIDS = ', &
                                                    HYBRID_BATTERY_COUNT
            WRITE(9,*) 'NUMBER OF INDEPENDENT HYBRIDS = ', &
                                                    HYBRID_INDEP_COUNT
            er_message='Stop requested from TRANSOBJ2 SIID321'
            call end_program(er_message)
         ENDIF

      RETURN



! CALLED ONCE PER gc_end_point

      ENTRY MULTI_YEAR_ENERGY_PRODUCTS()



         RUN_TRANSACT = YES_RUN_TRANSACT()
         PLANNING_METHOD = CAPACITY_PLANNING_METHOD()
         IF(PLANNING_METHOD == 'MI' .OR. PLANNING_METHOD == 'SO' .OR. &
              PLANNING_METHOD == 'RE' .OR. PLANNING_METHOD == 'PR') THEN
            MIX_RATIOS_ACTIVE = .TRUE.
         ELSE
            MIX_RATIOS_ACTIVE = .FALSE.
         ENDIF
       if(get_globecom_study_period()==0) then
        call end_program("transobj2:0011 - get_globecom_study_period() is zero.")
       end if
       
       call allocate_dev_tg_cap(int(NUM_OF_TRANS_CLASSES), &
       int(get_globecom_study_period()))
!
         DEV_TG_CAP = 0.
         DEV_NEW_TG_CAP = 0.
         IF(.NOT. SAVE_ENERGY_PRODUCTS_STATUS .OR. &
                     .NOT. (RUN_TRANSACT .OR. MIX_RATIOS_ACTIVE)) then
             RETURN
         end if
         TRANS_ACTIVE_IN_ENDPOINT = .FALSE. ! 121214 MOVED.
         
         call check_bounds_r4_3s(dev_tg_cap, &
              "transobj2:0015", "dev_tg_cap", num_fuel_categories, &
                GET_NUMBER_OF_ACTIVE_GROUPS(), &
                    get_globecom_study_period())

         DO LOCAL_YEAR = 1, get_globecom_study_period()

            THIS_YEAR = LOCAL_YEAR + get_BASE_YEAR()
            ilocyear=LOCAL_YEAR
            LOCAL_PEAK_MONTH = PEAK_MONTH(ilocyear)
            TEST_DATE = 100*(THIS_YEAR-1900) + LOCAL_PEAK_MONTH

            DO TRANS = 1, SAVE_ACTIVE_TRANSACTIONS
               IF(TEST_DATE > END_EP(TRANS) .OR. &
                          TEST_DATE < BEGIN_EP(TRANS)) CYCLE

               IF(ENERGY_MULTIPLIER(TRANS) < 0.) THEN
                  TEMP_R1 = ABS(ENERGY_MULTIPLIER(TRANS))
                  TEMP_I2 = INT(ABS(ENERGY_MULTIPLIER(TRANS)),2)
                  CAP_MW = &
                     ESCALATED_MONTHLY_VALUE(TEMP_R1,TEMP_I2,LOCAL_YEAR, &
                                              LOCAL_PEAK_MONTH,INT(1,2))
               ELSE
                  CAP_MW = ENERGY_MULTIPLIER(TRANS)
               ENDIF
! 010201.
               IF(QUANTITY_OF_PRODUCT(TRANS) < 0.) THEN
                  get_var_called_from="transobj2:4036"
                  CAP_MW = CAP_MW * &
                        GET_VAR(QUANTITY_OF_PRODUCT(TRANS), &
                                                LOCAL_YEAR, &
                                                TRANSACTION_NAME(TRANS))
               ELSE
                  CAP_MW = CAP_MW * QUANTITY_OF_PRODUCT(TRANS)
               ENDIF
               TRANS_ACTIVE_IN_ENDPOINT(TRANS) = .TRUE.
               IF(CAPACITY_PLANNING_FACTOR(TRANS) < 0.) THEN
                  get_var_called_from="transobj2:4224"
                  CAP_MW = CAP_MW * &
                        GET_VAR(CAPACITY_PLANNING_FACTOR(TRANS), &
                                                LOCAL_YEAR, &
                                                TRANSACTION_NAME(TRANS))
               ELSE
                  CAP_MW = CAP_MW *CAPACITY_PLANNING_FACTOR(TRANS)
               ENDIF
! 092706
               IF(EXPENSE_ASSIGNMENT(TRANS) == LOCAL_ASSIGNMENT(1)) THEN
                  CAP_MW = -CAP_MW
               ENDIF

               TG = TRANSACTION_GROUP(TRANS)
               TG = GET_TRANS_GROUP_POSITION(TG)
               FT = FUEL_TYPE_2_PRIM_MOVER(FUEL_TYPE(TRANS))
               get_primary_mover_index_called_from="transobj2:0027"
               PM = PRIMARY_MOVER_INDEX_DB(UNIT_CATEGORY(TRANS))
               IF(TG < 1 .OR. TG > NUM_OF_TRANS_CLASSES) THEN
                  WRITE(4,*) "BAD TRANSACTION GROUP ID IN DERIV FILE", &
                                                 TRANSACTION_NAME(TRANS)
               ENDIF
! 121506.
               IF(ABS(CAP_MW) > .01) THEN
                  TRANS_ACTIVE_IN_ENDPOINT(TRANS) = .TRUE.
               ENDIF
!
               ! Edge case? TODO: Describe (unable to find where FT set
               ! to 8 in codebase, so it's probably a read).
               IF(FT == 8 .AND. PM == 17) then
                   FT = 12 ! DG
               end if
               IF(FT < 1 .OR. FT > 12) THEN
! 122020. UNDEFINED.
               ELSEIF(FT /= 10) THEN

                  DEV_TG_CAP(FT,TG,LOCAL_YEAR) = &
                                   DEV_TG_CAP(FT,TG,LOCAL_YEAR) + CAP_MW
                  DEV_TG_CAP(0,TG,LOCAL_YEAR) = &
                                    DEV_TG_CAP(0,TG,LOCAL_YEAR) + CAP_MW
                  DEV_TG_CAP(FT,0,LOCAL_YEAR) = &
                                    DEV_TG_CAP(FT,0,LOCAL_YEAR) + CAP_MW
                  DEV_TG_CAP(0,0,LOCAL_YEAR) = &
                                     DEV_TG_CAP(0,0,LOCAL_YEAR) + CAP_MW

               ELSEIF(FT == 10) THEN ! 022708. DSM AND EES
                  IF(DERIVATIVE_TYPE(TRANS) /= 7) THEN

                     DEV_TG_CAP(FT,TG,LOCAL_YEAR) = &
                                   DEV_TG_CAP(FT,TG,LOCAL_YEAR) + CAP_MW
                     DEV_TG_CAP(0,TG,LOCAL_YEAR) = &
                                    DEV_TG_CAP(0,TG,LOCAL_YEAR) + CAP_MW
                     DEV_TG_CAP(0,0,LOCAL_YEAR) = &
                                     DEV_TG_CAP(0,0,LOCAL_YEAR) + CAP_MW
                  ENDIF

               ENDIF
            ENDDO

! ADD CAPACITY_PLANNING_FACTOR

         ENDDO



      RETURN

      ENTRY GET_DERIV_PROP_RESOURCE_POS(R_TRANS)

         IF(ALLOCATED(PROPOSED_RESOURCE_POSITION)) THEN
            GET_DERIV_PROP_RESOURCE_POS = &
                                     PROPOSED_RESOURCE_POSITION(R_TRANS)
         ELSE
            GET_DERIV_PROP_RESOURCE_POS = 0
         ENDIF
      RETURN


      ENTRY GET_TRANS_RPS_PERCENT(R_TRANS,R_YEAR)

! 011919. CHANGED FOR TIME-SERIES ON VARIABLE
         IF(RPS_Contribution(R_TRANS) < -0.0001) THEN
            GET_TRANS_RPS_PERCENT = &
                  0.01*ESCALATED_MONTHLY_VALUE(GET_TRANS_RPS_PERCENT, &
                          ABS(INT(RPS_Contribution(R_TRANS),2)), &
                                               R_YEAR,INT(1,2),INT(1,2))
         ELSE
            GET_TRANS_RPS_PERCENT = RPS_Contribution(R_TRANS)*0.01
         ENDIF
      RETURN

      ENTRY GET_TRANS_RPS_PROG_NUMBER(R_TRANS)

         GET_TRANS_RPS_PROG_NUMBER = RPS_PROGRAM_NUMBERs(R_TRANS)
      RETURN

      ENTRY GET_TRANS_CAP_PLAN_FAC(R_TRANS,R_YEAR)

         IF(CAPACITY_PLANNING_FACTOR(R_TRANS) < 0.) THEN
            get_var_called_from="transobj2:4143"



            GET_TRANS_CAP_PLAN_FAC = &
                        GET_VAR(CAPACITY_PLANNING_FACTOR(R_TRANS), &
                                              R_YEAR, &
                                              TRANSACTION_NAME(R_TRANS))
         ELSE
            GET_TRANS_CAP_PLAN_FAC = CAPACITY_PLANNING_FACTOR(R_TRANS)
         ENDIF
      RETURN

      ENTRY GET_TRANS_STATE_INDEX(R_TRANS)

         GET_TRANS_STATE_INDEX = State_Index(R_TRANS)
      RETURN

      ENTRY RENEWABLE_OPTION_CHECK(R_GRX_ID)

         RENEWABLE_OPTION_CHECK = .FALSE.
         IF(ALLOCATED(GRX_ID_INDEX)) THEN
            IF(R_GRX_ID > 0 .AND. R_GRX_ID <= 10000) THEN
               TEMP_I2 = GRX_ID_INDEX(R_GRX_ID)
               IF(TEMP_I2 > 0) THEN
                  IF(Fuel_Category(TEMP_I2) == 'Renewable ' .OR. &
                            Fuel_Category(TEMP_I2) == 'MWH       ' .OR. &
                            Fuel_Category(TEMP_I2) == 'Other     ') THEN
                     RENEWABLE_OPTION_CHECK = .TRUE.
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      RETURN

      ENTRY EFFICIENCY_OPTION_CHECK(R_GRX_ID)

         EFFICIENCY_OPTION_CHECK = .FALSE.
         IF(ALLOCATED(GRX_ID_INDEX)) THEN
            IF(R_GRX_ID > 0 .AND. R_GRX_ID <= 10000) THEN
               TEMP_I2 = GRX_ID_INDEX(R_GRX_ID)
               IF(TEMP_I2 > 0) THEN
                  IF(Fuel_Category(TEMP_I2) == 'Efficiency') THEN
                     EFFICIENCY_OPTION_CHECK = .TRUE.
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      RETURN

      ENTRY GET_TRANS_FOR_GRX_ID(R_GRX_ID)

         IF(ALLOCATED(GRX_ID_INDEX)) THEN
            GET_TRANS_FOR_GRX_ID = GRX_ID_INDEX(R_GRX_ID)
         ELSE
            GET_TRANS_FOR_GRX_ID = 1
         ENDIF
      RETURN

      ENTRY GET_TRANS_GROUP_FOR_TRANS(R_TRANS)

         IF(ALLOCATED(GRX_ID_INDEX)) THEN
            IF(R_TRANS <= SAVE_ACTIVE_TRANSACTIONS) THEN
               GET_TRANS_GROUP_FOR_TRANS = TRANSACTION_GROUP(R_TRANS)
            ELSE
               GET_TRANS_GROUP_FOR_TRANS = -9999
            ENDIF
         ELSE
            GET_TRANS_GROUP_FOR_TRANS = 1
         ENDIF
      RETURN

      ENTRY GRX_SAVE_DERIV_PLAN_VALUES(R_SAVE_VALUES,R_YEAR)


         IF(R_SAVE_VALUES) THEN
            DO RI = 1, SAVE_NUM_PROPOSED_UNITS
               TRANS = PROPOSED_RESOURCE_INDEX(RI)
               SAVED_QUANT_OF_PRODUCT(TRANS) = &
                                        QUANTITY_OF_PRODUCT(TRANS)
! 041117. MOVED INSIDE LOOP.
               SAVE_GRX_RPS_CAPACITY(TRANS,:) = &
                                               GRX_RPS_CAPACITY(TRANS,:)
            ENDDO
            SAVED_DEV_NEW_TG_CAP = DEV_NEW_TG_CAP






         ELSE
            I = 1
            I = 2
            I = 3
            DEV_NEW_TG_CAP = SAVED_DEV_NEW_TG_CAP

            I = 4
            I = 5
            I = 6
            DO RI = 1, SAVE_NUM_PROPOSED_UNITS
               TRANS = PROPOSED_RESOURCE_INDEX(RI)
               QUANTITY_OF_PRODUCT(TRANS) = &
                                        SAVED_QUANT_OF_PRODUCT(TRANS)


               GRX_RPS_CAPACITY(TRANS,:) = &
                                          SAVE_GRX_RPS_CAPACITY(TRANS,:)
!
            ENDDO
         ENDIF
         GRX_SAVE_DERIV_PLAN_VALUES = .TRUE.
      RETURN

      ENTRY GET_TRANS_ITER_CAP_PERCENT(R_TRANS)

         IF(PROPOSED_RESOURCE_POSITION(R_TRANS) <= 0) THEN
            GET_TRANS_ITER_CAP_PERCENT = 1.0
         ELSE
            IF(QUANTITY_OF_PRODUCT(R_TRANS) > 0.001) THEN
               GET_TRANS_ITER_CAP_PERCENT = &
                          MIN(SAVED_QUANT_OF_PRODUCT(R_TRANS), &
                                    QUANTITY_OF_PRODUCT(R_TRANS))/ &
                                        QUANTITY_OF_PRODUCT(R_TRANS)
            ELSE
               GET_TRANS_ITER_CAP_PERCENT = 0.0
            ENDIF
         ENDIF
      RETURN

      ENTRY ACTIVE_GRX_RPS_VECTOR(R_VECTOR,R_VALUES_30)

         ACTIVE_GRX_RPS_VECTOR = .FALSE.
         IF(GRX_BUILD_VECTOR_INDEX(R_VECTOR) > 0) THEN
               ACTIVE_GRX_RPS_VECTOR = .TRUE.
               TRANS = GRX_BUILD_VECTOR_INDEX(R_VECTOR)
               R_VALUES_30(:) = GRX_RPS_CAPACITY(TRANS,:)
         ENDIF
      RETURN

! SINGLE DERIVATIVE ADD.

      ENTRY ENERGY_PRODUCTS_PLANNING_ADD(R_GRX_ID, &
                                         R_START_YEAR, &
                                         R_EP_YEARS)

        ENERGY_PRODUCTS_PLANNING_ADD = 0.0
        RUN_TRANSACT = YES_RUN_TRANSACT()

         IF(.NOT. SAVE_ENERGY_PRODUCTS_STATUS .OR. &
                   .NOT. RUN_TRANSACT) RETURN

         TRANS = GRX_ID_INDEX(R_GRX_ID)
         TRANS_ACTIVE_IN_ENDPOINT(TRANS) = .FALSE. ! 121214 MOVED.

         TEST_DATE = 100*(THIS_YEAR-1900) + 1
         BEGIN_EP(TRANS) = MIN(BEGIN_EP(TRANS),TEST_DATE)

         TEST_DATE = 100*(THIS_YEAR-1900) + 12
         END_EP(TRANS) = MAX(END_EP(TRANS),TEST_DATE)
         PRODUCT_ACTIVE(TRANS) = 'T'
! 072609.
         BEGIN_YEAR = MAX(INT(1,2), R_START_YEAR - get_BASE_YEAR())
         END_YEAR = MIN(get_globecom_study_period(),R_START_YEAR - &
                                     get_BASE_YEAR() + R_EP_YEARS)
                                     
         call check_bounds_r4_3s(DEV_NEW_TG_CAP, &
            "transobj2:0017", "DEV_NEW_TG_CAP", &
              num_fuel_categories, GET_NUMBER_OF_ACTIVE_GROUPS(), &
              END_YEAR)
              
         DO LOCAL_YEAR = BEGIN_YEAR, END_YEAR
!
            THIS_YEAR = LOCAL_YEAR + get_BASE_YEAR()
            LOCAL_PEAK_MONTH = PEAK_MONTH(LOCAL_YEAR)
            TEST_DATE = 100*(THIS_YEAR-1900) + LOCAL_PEAK_MONTH

               IF(TEST_DATE > END_EP(TRANS) .OR. &
                          TEST_DATE < BEGIN_EP(TRANS)) CYCLE

               IF(ENERGY_MULTIPLIER(TRANS) < 0.) THEN
                  TEMP_R1 = ABS(ENERGY_MULTIPLIER(TRANS))
                  TEMP_I2 = INT(ABS(ENERGY_MULTIPLIER(TRANS)),2)
                  CAP_MW = &
                     ESCALATED_MONTHLY_VALUE(TEMP_R1,TEMP_I2,LOCAL_YEAR, &
                                              LOCAL_PEAK_MONTH,INT(1,2))
               ELSE
                  CAP_MW = ENERGY_MULTIPLIER(TRANS)
               ENDIF
! 010201.
               IF(PROPOSED_QUANT_OF_PRODUCT(TRANS) < 0.) THEN
                  CAP_MW = CAP_MW * &
                        GET_VAR(PROPOSED_QUANT_OF_PRODUCT(TRANS), &
                                                LOCAL_YEAR, &
                                                TRANSACTION_NAME(TRANS))
               ELSE
                  CAP_MW = CAP_MW * PROPOSED_QUANT_OF_PRODUCT(TRANS)
               ENDIF
               IF(LOCAL_YEAR == R_START_YEAR - get_BASE_YEAR()) THEN
                  ENERGY_PRODUCTS_PLANNING_ADD = CAP_MW
               ENDIF
               TRANS_ACTIVE_IN_ENDPOINT(TRANS) = .TRUE.
!
! 072709. CAPACITY BEFORE CAPACITY PLANNING FACTOR
!
               GRX_RPS_CAPACITY(TRANS,LOCAL_YEAR) = &
                             GRX_RPS_CAPACITY(TRANS,LOCAL_YEAR) + CAP_MW
!
               IF(CAPACITY_PLANNING_FACTOR(TRANS) < 0.) THEN
                  get_var_called_from="transobj2:4352"
                  CAP_MW = CAP_MW * &
                        GET_VAR(CAPACITY_PLANNING_FACTOR(TRANS), &
                                                LOCAL_YEAR, &
                                                TRANSACTION_NAME(TRANS))
               ELSE
                  CAP_MW = CAP_MW *CAPACITY_PLANNING_FACTOR(TRANS)
               ENDIF
! 092706
               IF(EXPENSE_ASSIGNMENT(TRANS) == LOCAL_ASSIGNMENT(1)) THEN
                  CAP_MW = -CAP_MW
               ENDIF
!
               TG = TRANSACTION_GROUP(TRANS)
               TG = GET_TRANS_GROUP_POSITION(TG)
               FT = FUEL_TYPE_2_PRIM_MOVER(FUEL_TYPE(TRANS))
               get_primary_mover_index_called_from="transobj2:0029"
               PM = PRIMARY_MOVER_INDEX_DB(UNIT_CATEGORY(TRANS))

               IF(TG < 1 .OR. TG > NUM_OF_TRANS_CLASSES) THEN
                  WRITE(4,*) "BAD TRANSACTION GROUP ID IN DERIV FILE", &
                                                 TRANSACTION_NAME(TRANS)
               ENDIF
! 121506.
               IF(ABS(CAP_MW) > .01) THEN
                  TRANS_ACTIVE_IN_ENDPOINT(TRANS) = .TRUE.
               ENDIF
!
               IF(FT == 8 .AND. PM == 17) THEN
               FT = 12 ! 112820. DG
               endif

               ! FT=10 is DSM of Efficiency 022708. DSM AND EES
               IF(FT == 10) THEN
                  IF(DERIVATIVE_TYPE(TRANS) /= 7) THEN

                     DEV_NEW_TG_CAP(FT,TG,LOCAL_YEAR) = &
                               DEV_NEW_TG_CAP(FT,TG,LOCAL_YEAR) + CAP_MW
                     DEV_NEW_TG_CAP(0,TG,LOCAL_YEAR) = &
                               DEV_NEW_TG_CAP(0,TG,LOCAL_YEAR) + CAP_MW
                     DEV_NEW_TG_CAP(FT,0,LOCAL_YEAR) = &
                               DEV_NEW_TG_CAP(FT,0,LOCAL_YEAR) + CAP_MW
                     DEV_NEW_TG_CAP(0,0,LOCAL_YEAR) = &
                               DEV_NEW_TG_CAP(0,0,LOCAL_YEAR) + CAP_MW

                  ENDIF

! FT 1-6 are fossil fuels; 7=bio; 8=solar; 9=wind; 11=geo thermal;
! 12=land fill
! FT 12 = DG
!
               ELSEIF (FT >= 1 .AND. FT <= 12) THEN

                  DEV_NEW_TG_CAP(FT,TG,LOCAL_YEAR) = &
                               DEV_NEW_TG_CAP(FT,TG,LOCAL_YEAR) + CAP_MW
                  DEV_NEW_TG_CAP(0,TG,LOCAL_YEAR) = &
                               DEV_NEW_TG_CAP(0,TG,LOCAL_YEAR) + CAP_MW
                  DEV_NEW_TG_CAP(FT,0,LOCAL_YEAR) = &
                               DEV_NEW_TG_CAP(FT,0,LOCAL_YEAR) + CAP_MW
                  DEV_NEW_TG_CAP(0,0,LOCAL_YEAR) = &
                               DEV_NEW_TG_CAP(0,0,LOCAL_YEAR) + CAP_MW
               ENDIF

! ADD CAPACITY_PLANNING_FACTOR

         ENDDO ! YEAR LOOP

! 052815. THIS IS A PIVOTAL ASSUMPTION. NEED TO CHECK THAT IT WORKS.

         QUANTITY_OF_PRODUCT(TRANS) = QUANTITY_OF_PRODUCT(TRANS) + &
                                        PROPOSED_QUANT_OF_PRODUCT(TRANS)
      RETURN

      ENTRY GET_DERIV_PRIM_MOVER(R_TRANS)

         GET_DERIV_PRIM_MOVER = &
                  FUEL_TYPE_2_PRIM_MOVER(FUEL_TYPE(R_TRANS))
      RETURN

      ENTRY GET_DERIV_PRIM_MOVER_INDEX(R_TRANS)
         get_primary_mover_index_called_from="transobj2:0030"
         GET_DERIV_PRIM_MOVER_INDEX = &
                 PRIMARY_MOVER_INDEX_DB(UNIT_CATEGORY(R_TRANS))
      RETURN

      ENTRY GET_DERIV_ZONE_ID(R_TRANS,TRANS_ZONE_ID)

         
         GET_DERIV_ZONE_ID = .TRUE.
         
         ubound_tmp=ubound(deriv_zone_id, 1)

         if(ubound_tmp>=r_trans) then
            TRANS_ZONE_ID = DERIV_ZONE_ID(R_TRANS)
         else
            trans_zone_id=" " ! Reproduce legacy behavior without 
                              ! indexing beyond array bounds 
                              !(legacy always exceeds bounds).
         ! The legacy (June '22) source allowed access into
         ! deriv_zone_id beyond its upper bound (which is 7
         ! during this debugging session). This went undiscovered
         ! because:
            ! 1) Range-checking was not performed.
            ! 2) Coincidentally, what was returned from that invalid
               ! location was an empty string.
         
         ! Assumption here is that if an invalid index is passed,
         ! an empty string should be returned. Although that
         ! reproduces the behavior from the legacy binary,
         ! the original intention is unknown.

         endif

      RETURN

      ENTRY GET_Holding_Company_ID(R_TRANS)

         GET_Holding_Company_ID = FLOAT(Holding_Company_ID(R_TRANS))
      RETURN


      ENTRY DERIV_CAPACITY_PLANNING_BY_ALL(R_FT,R_TG,R_YEAR)



        tg_element_failed=.false.
        yr_element_failed=.false.
        ft_element_failed=.false.

         bad_vector_name=""
         ft_ubound=ubound(dev_tg_cap,dim=1)
         tg_ubound=ubound(dev_tg_cap,dim=2)
         yr_ubound=ubound(dev_tg_cap, dim=3)
         ft_index=r_ft


         if(ft_ubound<R_FT) then
            rep_failed=.true.
            ft_element_failed=.true.
            bad_vector_name="R_FT"
            bad_index_value=R_FT
            max_index_value=ft_ubound
        endif

        if(tg_ubound<R_TG) then
            rep_failed=.true.
            tg_element_failed=.true.
            bad_vector_name="R_TG"
            bad_index_value=R_TG
            max_index_value=tg_ubound
        endif

        if(yr_ubound<R_YEAR) then
            rep_failed=.true.
            yr_element_failed=.true.
            bad_vector_name="R_YEAR"
        endif

         if(rep_failed) then

            log_message="transobj2:0002 " // trim(er_message) // &
       "Bounds of dev_tg_cap exceeded. Failed DIMension: " // &
       trim(bad_vector_name) // "=" //  trim(itos(bad_index_value)) &
       // ", " // "Max is " // trim(itos(max_index_value)) // &
       " for that array."

          call write_log_entry("transobj2:2", log_message)
          er_message=trim(log_message) ! Write error last
          er_message=trim(er_message) // " See log for details."

          log_message= &
          "tg_element_failed? " //trim(bl_2str(tg_element_failed))// &
             ", tg=" // trim(int_to_string(int(tg))) // &
             ", tg_ubound=" // trim(int_to_string(int(tg_ubound)))
          call write_log_entry("transobj2:3", log_message)

        call write_log_entry("transobj2:3", log_message)

        log_message="ft_element_failed? "// &
          trim(bl_2str(ft_element_failed)) // &
             ", ft=" //trim(int_to_string(int(ft))) // &
             ", ft_ubound="//trim(int_to_string(int(ft_ubound)))

        call write_log_entry("transobj2:4", trim(log_message))


        log_message="yr_element_failed? "// &
        trim(bl_2str(yr_element_failed)) // &
             ", yr="//trim(int_to_string(int(globecom_YEAR)))// &
             ", yr_ubound="// &
             trim(int_to_string(int(yr_ubound)))

            call write_log_entry("transobj2:5", log_message)

            call end_program(er_message)
         endif


         RUN_TRANSACT = YES_RUN_TRANSACT()
         IF(.NOT. SAVE_ENERGY_PRODUCTS_STATUS .OR. &
                                              .NOT. RUN_TRANSACT) THEN
            DERIV_CAPACITY_PLANNING_BY_ALL =  0.
         ELSE
            IF(R_FT == 6) THEN ! OTHER
               DERIV_CAPACITY_PLANNING_BY_ALL = &
                                       DEV_TG_CAP(6,R_TG, &
                                           MIN(R_YEAR,get_globecom_study_period())) + &
                                       DEV_TG_CAP(7,R_TG, &
                                           MIN(R_YEAR,get_globecom_study_period())) + &
                                       DEV_TG_CAP(10,R_TG, &
                                           MIN(R_YEAR,get_globecom_study_period()))

            ELSEIF(R_FT == 7) THEN ! DG
               DERIV_CAPACITY_PLANNING_BY_ALL = &
                                       DEV_TG_CAP(12,R_TG, &
                                           MIN(R_YEAR,get_globecom_study_period()))
            ELSEIF(R_FT == 10) THEN ! BATTERY
               DERIV_CAPACITY_PLANNING_BY_ALL = &
                                       DEV_TG_CAP(11,R_TG, &
                                           MIN(R_YEAR,get_globecom_study_period()))
            ELSE
               DERIV_CAPACITY_PLANNING_BY_ALL = DEV_TG_CAP(R_FT,R_TG, &
                                               MIN(R_YEAR,get_globecom_study_period()))
            ENDIF
         ENDIF
      RETURN

!
      ENTRY DERIV_NEW_CAP_PLAN_BY_ALL(R_FT,R_TG,R_YEAR)
!

!
         RUN_TRANSACT = YES_RUN_TRANSACT()
         IF(.NOT. SAVE_ENERGY_PRODUCTS_STATUS .OR. &
                                              .NOT. RUN_TRANSACT) THEN
            DERIV_NEW_CAP_PLAN_BY_ALL =  0.
         ELSE
! 112820. USING 12 FOR DG.
! 122020. REMOVED 7 AND 10 FROM FT = 6.
! 060815. MODIFIED TO HANDLE SIX FUEL CATEGORIES WHEN 12 ARE DEFINED.
              call check_bounds_r4_3s(DEV_NEW_TG_CAP, &
               "transobj2:0018", "DEV_NEW_TG_CAP", &
                int(10,2), r_TG, get_globecom_study_period())
            IF(R_FT == 6) THEN

               DERIV_NEW_CAP_PLAN_BY_ALL = &
                                       DEV_NEW_TG_CAP(6,R_TG, &
                                           MIN(R_YEAR,get_globecom_study_period())) + &
                                       DEV_NEW_TG_CAP(7,R_TG, &
                                           MIN(R_YEAR,get_globecom_study_period())) + &
                                       DEV_NEW_TG_CAP(10,R_TG, &
                                           MIN(R_YEAR,get_globecom_study_period()))

            ELSEIF(R_FT == 7) THEN ! DG
               DERIV_NEW_CAP_PLAN_BY_ALL = DEV_NEW_TG_CAP(12,R_TG, &
                                               MIN(R_YEAR,get_globecom_study_period()))
            ELSEIF(R_FT == 10) THEN ! BATTERY
               DERIV_NEW_CAP_PLAN_BY_ALL = DEV_NEW_TG_CAP(11,R_TG, &
                                               MIN(R_YEAR,get_globecom_study_period()))
            ELSE
               DERIV_NEW_CAP_PLAN_BY_ALL = DEV_NEW_TG_CAP(R_FT,R_TG, &
                                               MIN(R_YEAR,get_globecom_study_period()))

               if(deriv_new_cap_plan_by_all .ne. 0) then
                   log_message="deriv_new_cap_plan_by_all is " // &
         "returning a nonzero result for FT=" // &
          trim(itos(int(FT))) // &
         ", TG=" // trim(itos(int(TG))) // ", Year=" // &
          trim(itos(int(r_year)))
                call write_log_entry("transobj2:8", log_message)
                call flush_log_entry()

               endif

            ENDIF
        ENDIF
      RETURN


      ENTRY DERIV_CAPACITY_PLANNING_BY_TG(R_TG,R_YEAR)



         RUN_TRANSACT = YES_RUN_TRANSACT()
         IF(.NOT. SAVE_ENERGY_PRODUCTS_STATUS .OR. &
                                              .NOT. RUN_TRANSACT) THEN
            DERIV_CAPACITY_PLANNING_BY_TG = 0.
         ELSE
            DERIV_CAPACITY_PLANNING_BY_TG = DEV_TG_CAP(0,R_TG, &
                                               MIN(R_YEAR,get_globecom_study_period()))
         ENDIF
      RETURN

!
      ENTRY DERIV_CAPACITY_PLANNING(R_YEAR)
!

!
         RUN_TRANSACT = YES_RUN_TRANSACT()
         IF(.NOT. SAVE_ENERGY_PRODUCTS_STATUS .OR. &
                       .NOT. (RUN_TRANSACT .OR. MIX_RATIOS_ACTIVE)) THEN
            DERIV_CAPACITY_PLANNING =  0.
         ELSE
            DERIV_CAPACITY_PLANNING =  DEV_TG_CAP(0,0, &
                                          MIN(R_YEAR,get_globecom_study_period())) + &
                                       DEV_NEW_TG_CAP(0,0, &
                                          MIN(R_YEAR,get_globecom_study_period()))
         ENDIF
      RETURN

      ENTRY GET_TRANS_RPS_USER_PATTERN(R_TRANS)

         IF(PRODUCT_TYPE(R_TRANS) == LOCAL_PRODUCT_TYPE(6)) THEN
            GET_TRANS_RPS_USER_PATTERN = USER_DAY_TYPES_ID(R_TRANS)
         ELSE
            GET_TRANS_RPS_USER_PATTERN = 0
         ENDIF
      RETURN


! CALLED FROM PROCOST. 2/1/99. GAT.

      ENTRY ANNUAL_ENERGY_PRODUCTS(R_YEAR)
         annual_energy_products_call_count= &
            annual_energy_products_call_count+1
         MONTHLY_ACTIVE_TRANSACTIONS = 0
         ANNUAL_ACTIVE_TRANSACTIONS = 0
         RUN_TRANSACT = YES_RUN_TRANSACT()

         
         if(file_trace_aep==0) then
             
             ! File not explicitly closed.
             file_trace_aep=open_trace("annual_energy_products.trace", &
                rq_to21_aep)
         end if
         call write_trace_int2(file_trace_aep, "year", R_year)
         
         IF(.NOT. SAVE_ENERGY_PRODUCTS_STATUS .OR. &
                                              .NOT. RUN_TRANSACT) RETURN
         ACTIVE_IN_YEAR_INDEX = 0
         ACTIVE_IN_YEAR = 0
         IF( ALLOCATED(MONTHLY_ENERGY_COST) ) &
            DEALLOCATE( &
                     MONTHLY_ENERGY_COST, &
                     MONTHLY_ENERGY, &
                     MONTHLY_CHARGE, &
                     MONTHLY_ENR_FOR_REV, &
                     MONTHLY_ENR_FOR_EXP, &
                     MONTHLY_CAPACITY, &
                     MONTHLY_ENERGY_REVENUE, &
                     MONTHLY_AC_REVENUE, &
                     MONTHLY_AC_REVENUE_ENERGY, &
                     MONTHLY_AC_EXPENSE, &
                     MONTHLY_AC_EXPENSE_ENERGY, &
                     MONTHLY_TRANS_HOURS, &
                     MONTHLY_PRODUCT_HOURS, &
                     MONTHLY_PRODUCT_DAYS, &
                     MONTHLY_PRODUCT_MONTHS, &
                     MONTHLY_STRIKES, &
                     MONTHLY_TRANSACTION_COST, &
                     MONTHLY_TRANSACTION_REVENUE)
         ALLOCATE(MONTHLY_ENERGY_COST(NUM_TRANSACTIONS,0:12))
         ALLOCATE(MONTHLY_ENERGY(NUM_TRANSACTIONS,0:12))
         ALLOCATE(MONTHLY_CHARGE(NUM_TRANSACTIONS,0:12))
         ALLOCATE(MONTHLY_ENR_FOR_REV(NUM_TRANSACTIONS,0:12))
         ALLOCATE(MONTHLY_ENR_FOR_EXP(NUM_TRANSACTIONS,0:12))
         ALLOCATE(MONTHLY_CAPACITY(NUM_TRANSACTIONS,0:12))
         ALLOCATE(MONTHLY_ENERGY_REVENUE(NUM_TRANSACTIONS,0:12))
         ALLOCATE(MONTHLY_AC_REVENUE(0:MAX_ASSET_CLASS_GROUPS,4,0:12))
         ALLOCATE(MONTHLY_AC_REVENUE_ENERGY( &
                                       0:MAX_ASSET_CLASS_GROUPS,4,0:12))
         ALLOCATE(MONTHLY_AC_EXPENSE(0:MAX_ASSET_CLASS_GROUPS,4,0:12))
         ALLOCATE(MONTHLY_AC_EXPENSE_ENERGY( &
                                       0:MAX_ASSET_CLASS_GROUPS,4,0:12))
         ALLOCATE(MONTHLY_TRANS_HOURS(NUM_TRANSACTIONS,0:12))
         ALLOCATE(MONTHLY_PRODUCT_HOURS(NUM_TRANSACTIONS,0:12))
         ALLOCATE(MONTHLY_PRODUCT_DAYS(NUM_TRANSACTIONS,0:12))
         ALLOCATE(MONTHLY_PRODUCT_MONTHS(NUM_TRANSACTIONS))
         ALLOCATE(MONTHLY_STRIKES(NUM_TRANSACTIONS,0:12))
         ALLOCATE(MONTHLY_TRANSACTION_COST(NUM_TRANSACTIONS,0:12))
         ALLOCATE(MONTHLY_TRANSACTION_REVENUE(NUM_TRANSACTIONS,0:12))
!
         CALL AllocateArray(DerivCongestionCost,1, &
                                         INT(NUM_TRANSACTIONS,4),0,12)
         DerivCongestionCost = 0.
!
         MONTHLY_ENERGY_COST = 0.
         MONTHLY_ENERGY = 0.
         MONTHLY_CHARGE = 0.
         MONTHLY_ENR_FOR_REV = 0.
         MONTHLY_ENR_FOR_EXP = 0.
         MONTHLY_CAPACITY = 0.
         MONTHLY_TRANS_HOURS = 0
         MONTHLY_PRODUCT_HOURS = 0
         MONTHLY_PRODUCT_DAYS = 0
         MONTHLY_PRODUCT_MONTHS = 0
         MONTHLY_STRIKES = 0
         MONTHLY_ENERGY_REVENUE = 0.
         MONTHLY_AC_REVENUE = 0.
         MONTHLY_AC_REVENUE_ENERGY = 0.
         MONTHLY_AC_EXPENSE = 0.
         MONTHLY_AC_EXPENSE_ENERGY = 0.
         MONTHLY_TRANSACTION_COST = 0.
         MONTHLY_TRANSACTION_REVENUE = 0.

         ANNUAL_INTERRUPTIBLE_CAPACITY = 0.
         ANNU_PLANNING_INTRPT_CAPACITY = 0.
         ANNUAL_STORAGE_CAPACITY = 0.

         NUM_ANNUAL_CALLS = 0
         ANNUAL_CAPACITY = 0.
         NUM_ANNUAL_PUTS = 0
         ANNUAL_CALL_POSITION = 0
         ANNUAL_PUT_POSITION = 0

         RPS_TRANS_DB = 0.0
         GRX_RPS_TRANS_DB = 0.0
         GRX_STORAGE_PATTERN = 0.0
         GRX_INDEP_PATTERN = 0.0
         ANNUAL_HYBRID_BATTERY = 0
         ANNUAL_HYBRID_INDEX = 0
         ANN_NUM_STOR = 0
         ANN_STOR_POS = 0

! 10/14/02. FOR ANNUAL PRODUCTS

         THIS_YEAR = globecom_YEAR + get_BASE_YEAR()
         BEGIN_TEST_DATE = 100*(THIS_YEAR - 1900) + 1
         END_TEST_DATE = 100*(THIS_YEAR - 1900) + 12
         SAVE_ANNUAL_PRODUCTS_STATUS = .FALSE.
         TOTAL_ANNUAL_CALLS = 0
         TOTAL_ANNUAL_PUTS = 0
         LOCAL_PEAK_MONTH = PEAK_MONTH(R_YEAR)

         if(file_trace_aep/=BAD_TRACE_HANDLE) then
         
            write(file_trace_aep, *) "SAVE_ACTIVE_TRANSACTIONS ", &
            "SAVE_ACTIVE_TRANSACTIONS ", SAVE_ACTIVE_TRANSACTIONS, &
            "THIS_YEAR ", THIS_YEAR, &
            "BEGIN_TEST_DATE ", BEGIN_TEST_DATE, &
            "END_TEST_DATE ", END_TEST_DATE, &
          "SAVE_ANNUAL_PRODUCTS_STATUS ", SAVE_ANNUAL_PRODUCTS_STATUS, &
            "PEAK MONTH ", LOCAL_PEAK_MONTH
         end if
         
         DO TRANS = 1, SAVE_ACTIVE_TRANSACTIONS
         
            IF(BEGIN_TEST_DATE > END_EP(TRANS) .OR. &
                        END_TEST_DATE < BEGIN_EP(TRANS)) then
             call write_trace_int(file_trace_aep, "calls",  &
            annual_energy_products_call_count)
     
                call write_trace_message(file_trace_aep, "1. CYCLE")
                CYCLE
            end if

            SAVE_ANNUAL_PRODUCTS_STATUS = .TRUE.
!
! NOTE DOUBLE INDEX
!
            TG = TRANSACTION_GROUP(TRANS)
            TG = GET_TRANS_GROUP_POSITION(TG)
!
            IF(STRIKE_FREQUENCY(TRANS) == 'A') THEN
               IF(DERIVATIVE_TYPE(TRANS) == 2 .OR. &
                                       DERIVATIVE_TYPE(TRANS) == 5) THEN
!
                  NUM_ANNUAL_CALLS(TG) = NUM_ANNUAL_CALLS(TG) + 1
                  TOTAL_ANNUAL_CALLS = TOTAL_ANNUAL_CALLS + 1
                  ANNUAL_CALL_POSITION(NUM_ANNUAL_CALLS(TG),TG) = TRANS
!
               ELSEIF(DERIVATIVE_TYPE(TRANS) == 3 .OR. &
                                       DERIVATIVE_TYPE(TRANS) == 6) THEN
!
                  NUM_ANNUAL_PUTS(TG) = NUM_ANNUAL_PUTS(TG) + 1
                  TOTAL_ANNUAL_PUTS = TOTAL_ANNUAL_PUTS + 1
                  ANNUAL_PUT_POSITION(NUM_ANNUAL_PUTS(TG),TG) = TRANS
               ENDIF
               DO iMo = 1,12
                  IF(TEST_DATE > BEGIN_EP(TRANS)) THEN
                     YEARS_BEG_DAY_IN_MO(TRANS,iMo) = 1
                  ELSE
                     YEARS_BEG_DAY_IN_MO(TRANS,iMo) = BEGIN_DAY(TRANS)
                  ENDIF
                  IF(TEST_DATE < END_EP(TRANS)) THEN
                     YEARS_END_DAY_IN_MO(TRANS,iMo) = 31
                  ELSE
                     YEARS_END_DAY_IN_MO(TRANS,iMo) = END_DAY(TRANS)
                  ENDIF
               ENDDO
            ELSEIF( DERIVATIVE_TYPE(TRANS) == 7 .OR. &
                                       DERIVATIVE_TYPE(TRANS) == 8) THEN
               TEST_DATE = 100*(R_YEAR+get_BASE_YEAR() - 1900) + &
                                                        LOCAL_PEAK_MONTH

               IF(TEST_DATE <= END_EP(TRANS) .AND. &
                                      TEST_DATE >= BEGIN_EP(TRANS)) THEN

                  IF(ENERGY_MULTIPLIER(TRANS) < 0.) THEN
                     TEMP_R1 = ABS(ENERGY_MULTIPLIER(TRANS))
                     TEMP_I2 = INT(ABS(ENERGY_MULTIPLIER(TRANS)),2)
                     HOURLY_QUANTITY(TRANS) = &
                        ESCALATED_MONTHLY_VALUE( &
                                               TEMP_R1,TEMP_I2,R_YEAR, &
                                             LOCAL_PEAK_MONTH,INT(1,2))
                  ELSE
                     HOURLY_QUANTITY(TRANS) = ENERGY_MULTIPLIER(TRANS)
                  ENDIF
! 010201.

       if(file_trace_aep/=BAD_TRACE_HANDLE) then
         write(file_trace_aep,*) "annual_energy_products_call_count ", &
            annual_energy_products_call_count
                  write(file_trace_aep,*) "TRANS ", TRANS

                  write(file_trace_aep,*) "R_YEAR ", R_YEAR
       end if

                  
                  IF(QUANTITY_OF_PRODUCT(TRANS) < 0.) THEN
      
      if(file_trace_aep/=BAD_TRACE_HANDLE) then

                  write(file_trace_aep,*) "TRANSACTION_NAME(TRANS) ", &
               TRANSACTION_NAME(TRANS)
       end if
      
       

            get_var_called_from="transobj2:4863"
            
            
                     HOURLY_QUANTITY(TRANS) = HOURLY_QUANTITY(TRANS) * &
                        GET_VAR(QUANTITY_OF_PRODUCT(TRANS), &
                                                R_YEAR, &
                                                TRANSACTION_NAME(TRANS))
                  ELSE
                     HOURLY_QUANTITY(TRANS) = HOURLY_QUANTITY(TRANS) * &
                                              QUANTITY_OF_PRODUCT(TRANS)
                  ENDIF
! 041012.
                  TEMP_I2 = 1 ! ARBITRARY
!
                  HOURLY_QUANTITY(TRANS) = &
                      HOURLY_QUANTITY(TRANS) * &
                           GET_MONTHLY_REGIONAL_PARAM( &
                                     R_YEAR,TEMP_I2, &
                                          TRANSACTION_GROUP(TRANS), &
                                                       LHS_INDEX(TRANS))

                  IF( DERIVATIVE_TYPE(TRANS) == 7) THEN
                     ANNUAL_INTERRUPTIBLE_CAPACITY(TG) = &
                        ANNUAL_INTERRUPTIBLE_CAPACITY(TG) + &
                                                  HOURLY_QUANTITY(TRANS)
                     ANNU_PLANNING_INTRPT_CAPACITY(TG) = &
                           ANNU_PLANNING_INTRPT_CAPACITY(TG) + &
                                CAPACITY_PLANNING_FACTOR(TRANS)* &
                                                  HOURLY_QUANTITY(TRANS)
!                     ENDIF
                  ELSEIF( DERIVATIVE_TYPE(TRANS) == 8) THEN
!               NUM_INTER_PUTS(TG) = NUM_INTER_PUTS(TG) + 1
!               INTER_PUT_POSITION(NUM_INTER_PUTS(TG),TG) = TRANS
!                     IF(R_MONTH == LOCAL_PEAK_MONTH) THEN
                     ANNUAL_INTERRUPTIBLE_CAPACITY(TG) = &
                           ANNUAL_INTERRUPTIBLE_CAPACITY(TG) - &
                                                  HOURLY_QUANTITY(TRANS)
                     ANNU_PLANNING_INTRPT_CAPACITY(TG) = &
                           ANNU_PLANNING_INTRPT_CAPACITY(TG) - &
                                 CAPACITY_PLANNING_FACTOR(TRANS) * &
                                                  HOURLY_QUANTITY(TRANS)
!                     ENDIF
                  ENDIF
               ENDIF
            ELSEIF(DERIVATIVE_TYPE(TRANS) == 9) THEN
               ANN_NUM_STOR(TG) = ANN_NUM_STOR(TG) + 1
               ANN_STOR_POS(ANN_NUM_STOR(TG),TG) = TRANS
!
! 021422. ADDED SO THAT INDEPENDENT CAPACITY IS COUNTED FOR PLANNING.
               INDEP_POINTER = HYBRID_BATTERY(TRANS)
               IF(INDEP_POINTER > 0) THEN
                     INDEP_POINTER = HYBRID_INDEX(INDEP_POINTER)
               ENDIF
               DO I = 1, 2
                  IF(I == 1) THEN
                     IND = TRANS
                  ELSE
                     IF(INDEP_POINTER == 0) THEN
                        CYCLE
                     ELSE
                        IND = INDEP_POINTER
                     ENDIF
                  ENDIF
                  IF(ENERGY_MULTIPLIER(IND) < 0.) THEN
                     TEMP_R1 = ABS(ENERGY_MULTIPLIER(IND))
                     TEMP_I2 = INT(ABS(ENERGY_MULTIPLIER(IND)),2)
                     HOURLY_QUANTITY(IND) = &
                        ESCALATED_MONTHLY_VALUE( &
                                               TEMP_R1,TEMP_I2,R_YEAR, &
                                             LOCAL_PEAK_MONTH,INT(1,2))
                  ELSE
                     HOURLY_QUANTITY(IND) = ENERGY_MULTIPLIER(IND)
                  ENDIF
! 010201.
                  IF(QUANTITY_OF_PRODUCT(IND) < 0.) THEN
                     get_var_called_from="transobj2:4935"

                     HOURLY_QUANTITY(IND) = HOURLY_QUANTITY(IND) * &
                        GET_VAR(QUANTITY_OF_PRODUCT(IND), &
                                                R_YEAR, &
                                                TRANSACTION_NAME(IND))
                  ELSE
                     HOURLY_QUANTITY(IND) = HOURLY_QUANTITY(IND) * &
                                              QUANTITY_OF_PRODUCT(IND)
                  ENDIF
               ENDDO

            ENDIF ! ANNUAL DERIVATIVE (EITHER STRIKE FREQ OR INTERRUPT)
!
         ENDDO ! TRANSACTIONS
!
         ANNUAL_ENERGY_PRODUCTS = .TRUE.
!
      RETURN
!

!
      ENTRY GET_ESC_NRG_PRICE(R_YEAR,R_MONTH,R_TRANS,GENP_R_VALUE) &
                                             RESULT(R_GET_ESC_NRG_PRICE)
    ! assumes it's okay to overwrite array MONTHLY_2ND_ENERGY_PRICE
!

!      GET_ESC_NRG_PRICE=.FALSE.
      R_GET_ESC_NRG_PRICE=.FALSE.
      IF(PRICE_TYPE(R_TRANS) /= 'U') THEN
         IF(ENERGY_PRICE(R_TRANS) < 0.) THEN
            TEMP_R4 = ABS(ENERGY_PRICE(R_TRANS))
            TEMP_I2 = INT(ABS(ENERGY_PRICE(R_TRANS)),2)
            GENP_ACCUM= &
               ESCALATED_MONTHLY_VALUE(TEMP_R4, &
                                       TEMP_I2, &
                                       R_YEAR-get_BASE_YEAR(), &
                                       R_MONTH,INT(1,2))
         ELSE
            GENP_ACCUM=ENERGY_PRICE(R_TRANS)
         ENDIF
      ELSE ! 10/29/02.
!
         TEMP_I2 = INT(ABS(ENERGY_PRICE(R_TRANS)),2)
!
         TEMP_I2 = GET_RESOURCE_ID_TO_UNIT(TEMP_I2,I)
         if(temp_i2<=1) then
            call end_program("picadisp:0002 - " // &
             "temp_i2 invalid before calling get_mrx_delivery_cost.")
         end if         
         TEMP_L1 = .TRUE.
         
         CALL GET_MRX_DELIVERY_COST(      TEMP_I2, &
                                          GENP_ACCUM, &
                                          R_MONTH, &
                                       R_YEAR-get_BASE_YEAR(), &
                                          TEMP_L, &
                                          TEMP_L1)
!         CALL GET_TOTAL_DELIVERY_COST(GENP_ACCUM,TEMP_I2)
      ENDIF
! 11/24/03.
      GENP_ACCUM = GENP_ACCUM * MONTHLY_ENERGY_MULT(R_TRANS)
!
      IF(SECOND_ENERGY_PRICE(R_TRANS) < 0.) THEN
!
         TEMP_I2 = INT(ABS(SECOND_ENERGY_PRICE(R_TRANS)),2)
!
         MONTHLY_2ND_ENERGY_PRICE(R_TRANS)= &
              ESCALATED_MONTHLY_VALUE(ABS(SECOND_ENERGY_PRICE(R_TRANS)), &
                  TEMP_I2, &
                  R_YEAR-get_BASE_YEAR(),R_MONTH,INT(1,2))
      ELSE
         MONTHLY_2ND_ENERGY_PRICE(R_TRANS)=SECOND_ENERGY_PRICE(R_TRANS)
      ENDIF
!
      IF(PRICE_TYPE(R_TRANS) == 'G' .OR. &
                                       PRICE_TYPE(R_TRANS) == 'U') THEN
         IF(HEAT_RATE_FOR_SPREAD(R_TRANS) < 0.) THEN
            TEMP_I2 = INT(ABS(HEAT_RATE_FOR_SPREAD(R_TRANS)),2)
            GENP_ACCUM=GENP_ACCUM* &
            ESCALATED_MONTHLY_VALUE(ABS(HEAT_RATE_FOR_SPREAD(R_TRANS)), &
               TEMP_I2, &
               R_YEAR-get_BASE_YEAR(),R_MONTH,INT(1,2))
         ELSE
            GENP_ACCUM=GENP_ACCUM*HEAT_RATE_FOR_SPREAD(R_TRANS)
         ENDIF
      ENDIF
! NOTE: THE SIMPLE SUM OF FIRST AND SECOND PRICE. 1/5/2.
      GENP_R_VALUE=GENP_ACCUM + MONTHLY_2ND_ENERGY_PRICE(R_TRANS)
!      GET_ESC_NRG_PRICE=.TRUE.
      R_GET_ESC_NRG_PRICE=.TRUE.
      RETURN ! ENTRY GET_ESC_NRG_PRICE
!

!
!
! CALLED FROM PROCOST. 2/1/99. GAT.
!
      ENTRY MONTHLY_ACTIVE_ENERGY_PRODUCTS(R_MONTH,R_YEAR)
!
!

!
         MONTHLY_ACTIVE_ENERGY_PRODUCTS = .FALSE.
!
         IF( .NOT. SAVE_ENERGY_PRODUCTS_STATUS) RETURN
!
         NUM_FORWARDS = 0
         NUM_CALLS = 0
         NUM_MONTH_CALLS = 0
         NUM_PUTS = 0
         NUM_STORAGE = 0
         NUM_LF_CALLS = 0
         NUM_LF_PUTS = 0
         NUM_MONTH_PUTS = 0
         NUM_INTER_PUTS = 0
         NUM_INTER_CALLS = 0
!
         FORWARD_POSITION = 0
         PUT_POSITION = 0
         MONTH_PUT_POSITION = 0
         STORAGE_POSITION = 0
         LF_CALL_POSITION = 0
         LF_PUT_POSITION = 0
         INTER_PUT_POSITION = 0
         INTER_CALL_POSITION = 0
         CALL_POSITION = 0
         MONTH_CALL_POSITION = 0
         MONTHLY_CONTINGENT_CAPACITY = 0.
         YES_CONTINGENT_MARKET_CAPACITY = .FALSE.
         MONTHLY_USER_CF = 0.0
         MONTHLY_USER_CF_COUNTER = 0
         MONTHLY_CF_TRANS = 0
         MAX_USER_CF = 0
         TRANS_TO_ACTIVE_MONTH = 0

         ! R_MONTH=/
         HOURS_IN_MONTH = DaysInNLYMo(R_MONTH)*24
         ! hours_in_month=/
         call check_hours_in_month(hours_in_month, "transobj2:0012")
         IF(HOURS_IN_MONTH>744) then
            call end_program( "transobj2:026 - HOURS_IN_MONTH=" // &
           trim(itos(int(hours_in_month))))
         end if
         IF(.NOT. RUN_TRANSACT) RETURN

! FISCAL YEAR INITIALIZATION

         YES_FISCAL_REPORTING = &
                              IS_FISCAL_YEAR_ACTIVE(FISCAL_SEASON_RESET, &
                                                            FISCAL_ONLY)

         IF( .NOT. YES_FISCAL_REPORTING .OR. &
               (YES_FISCAL_REPORTING .AND. &
                                   R_MONTH == FISCAL_SEASON_RESET)) THEN
!
            FISCAL_ACTIVE_IN_YEAR_INDEX = 0
            FISCAL_ACTIVE_IN_YEAR = 0
!
            FISCAL_ACTIVE_TRANSACTIONS = 0
!
            FISCAL_ENERGY_COST = 0.
            FISCAL_ENERGY = 0.
            FISCAL_CAPACITY = 0.
            FISCAL_TRANS_HOURS = 0
            FISCAL_PRODUCT_HOURS = 0
            FISCAL_PRODUCT_DAYS = 0
            FISCAL_PRODUCT_MONTHS = 0
            FISCAL_STRIKES = 0
            FISCAL_ENERGY_REVENUE = 0.
            FISCAL_TRANSACTION_COST = 0.
            FISCAL_TRANSACTION_REVENUE = 0.
!         ELSEIF( .NOT. YES_FISCAL_REPORTING) THEN
!            FISCAL_ACTIVE_TRANSACTIONS = 0
         ENDIF
!
         IF( ALLOCATED(STRIKES_AVAILABLE) ) &
            DEALLOCATE(STRIKES_AVAILABLE,STRIKES_REQUIRED)
         ALLOCATE(STRIKES_AVAILABLE(NUM_TRANSACTIONS))
         ALLOCATE(STRIKES_REQUIRED(NUM_TRANSACTIONS))
         STRIKES_AVAILABLE = 0
         STRIKES_REQUIRED = 0
!
!
!         LOCAL_PEAK_MONTH = PEAK_MONTH(R_YEAR)
!
         MONTHLY_ACTIVE_TRANSACTIONS = 0
         TEST_DATE = 100*(R_YEAR - 1900) + R_MONTH
         LOCAL_YR = R_YEAR - get_BASE_YEAR()
         DO TRANS = 1, SAVE_ACTIVE_TRANSACTIONS
            IF(TEST_DATE > END_EP(TRANS) .OR. &
                                      TEST_DATE < BEGIN_EP(TRANS)) CYCLE
            IF(ENERGY_MULTIPLIER(TRANS) < 0.) THEN
               TEMP_R1 = ABS(ENERGY_MULTIPLIER(TRANS))
               TEMP_I2 = INT(ABS(ENERGY_MULTIPLIER(TRANS)),2)
               HOURLY_QUANTITY(TRANS) = &
                 ESCALATED_MONTHLY_VALUE(TEMP_R1,TEMP_I2,LOCAL_YR, &
                                                   R_MONTH,INT(1,2))
            ELSE
               HOURLY_QUANTITY(TRANS) = ENERGY_MULTIPLIER(TRANS)
            ENDIF
! 010201.
            IF(QUANTITY_OF_PRODUCT(TRANS) < 0.) THEN
               get_var_called_from="transobj2:5138"
               HOURLY_QUANTITY(TRANS) = HOURLY_QUANTITY(TRANS) * &
                        GET_VAR(QUANTITY_OF_PRODUCT(TRANS), &
                                                R_YEAR-get_BASE_YEAR(), &
                                                TRANSACTION_NAME(TRANS))
            ELSE
               HOURLY_QUANTITY(TRANS) = HOURLY_QUANTITY(TRANS) * &
                                              QUANTITY_OF_PRODUCT(TRANS)
            ENDIF

            IF(ENERGY_PRICE_MULTIPLIER(TRANS) < 0.) THEN
!
               TEMP_R1 = 1.0
               TEMP_I2 = INT(ABS(ENERGY_PRICE_MULTIPLIER(TRANS)),2)
               MONTHLY_ENERGY_MULT(TRANS) = &
                 ESCALATED_MONTHLY_VALUE(TEMP_R1,TEMP_I2,LOCAL_YR, &
                                                   R_MONTH,INT(1,2))
!
            ELSE
!
               MONTHLY_ENERGY_MULT(TRANS) = &
                                          ENERGY_PRICE_MULTIPLIER(TRANS)
!
            ENDIF
            IF(ABS(USER_CF(TRANS)) > 0.0001 .AND. &
                            ABS(USER_DAY_TYPES_ID(TRANS)) > 0.1) THEN
               MAX_USER_CF = MAX_USER_CF + 1
               MONTHLY_USER_CF_COUNTER(MAX_USER_CF) = &
                                               USER_DAY_TYPES_ID(TRANS)
               MONTHLY_CF_TRANS(TRANS) = MAX_USER_CF
               IF(USER_CF(TRANS) < 0.0) THEN
!
                  TEMP_R1 = 1.0
                  TEMP_I2 = INT(ABS(USER_CF(TRANS)),2)
                  MONTHLY_USER_CF(MAX_USER_CF) = &
                     ESCALATED_MONTHLY_VALUE(TEMP_R1,TEMP_I2,LOCAL_YR, &
                                                R_MONTH,INT(1,2))/100.
!
               ELSE
!
                  MONTHLY_USER_CF(MAX_USER_CF) = USER_CF(TRANS)/100.
!
               ENDIF
            ENDIF
!
            L_DUMMY=GET_ESC_NRG_PRICE(R_YEAR,R_MONTH,TRANS, &
                                            MONTHLY_ENERGY_PRICE(TRANS))
!
            IF(HOURLY_QUANTITY(TRANS) <= 0.) CYCLE
! 041012.
            TG = TRANSACTION_GROUP(TRANS)
            HOURLY_QUANTITY(TRANS) = &
                        HOURLY_QUANTITY(TRANS) * &
                             GET_MONTHLY_REGIONAL_PARAM( &
                                     R_YEAR-get_BASE_YEAR(), &
                                            R_MONTH,TG,LHS_INDEX(TRANS))
!
!
! ACTIVE IN THE MONTH. ALL NON-ENERGY COSTS ARE CALLED TRANSACTION
! COSTS.
            MONTHLY_ACTIVE_TRANSACTIONS = &
                                         MONTHLY_ACTIVE_TRANSACTIONS + 1
            ACTIVE_IN_MONTH(MONTHLY_ACTIVE_TRANSACTIONS) = TRANS
            TRANS_TO_ACTIVE_MONTH(TRANS) = MONTHLY_ACTIVE_TRANSACTIONS
            IF(ACTIVE_IN_YEAR_INDEX(TRANS) == 0) THEN
               ACTIVE_IN_YEAR_INDEX(TRANS) = 1
               ANNUAL_ACTIVE_TRANSACTIONS = &
                                          ANNUAL_ACTIVE_TRANSACTIONS + 1
               ACTIVE_IN_YEAR(ANNUAL_ACTIVE_TRANSACTIONS) = TRANS
            ENDIF
!
! 10/03/02.
!
            IF(YES_FISCAL_REPORTING) THEN
               IF(FISCAL_ACTIVE_IN_YEAR_INDEX(TRANS) == 0) THEN
                  FISCAL_ACTIVE_IN_YEAR_INDEX(TRANS) = 1
                  FISCAL_ACTIVE_TRANSACTIONS = &
                                          FISCAL_ACTIVE_TRANSACTIONS + 1
                  FISCAL_ACTIVE_IN_YEAR(FISCAL_ACTIVE_TRANSACTIONS) = &
                                                                   TRANS
               ENDIF
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
            TG = TRANSACTION_GROUP(TRANS)
            TG = GET_TRANS_GROUP_POSITION(TG)
!
            IF(UNIT_CONTING_MARKET_SUPPLEMENT(TRANS) == 'T') THEN
               YES_CONTINGENT_MARKET_CAPACITY = .TRUE.
            ENDIF
!
            IF(UNIT_CONTINGENCY(TRANS) /= 'N') THEN
               MONTHLY_CONTINGENT_CAPACITY(TG) = &
                        MONTHLY_CONTINGENT_CAPACITY(TG) + &
                                                  HOURLY_QUANTITY(TRANS)
            ENDIF
!
            STRIKES_AVAILABLE(TRANS) = MAXIMUM_STRIKES(TRANS)
            STRIKES_REQUIRED(TRANS) = MINIMUM_STRIKES(TRANS)
            IF(DERIVATIVE_TYPE(TRANS) == 1 .OR. &
                         DERIVATIVE_TYPE(TRANS) == 4 .OR. &
                                      DERIVATIVE_TYPE(TRANS) == 12) THEN
               NUM_FORWARDS(TG) = NUM_FORWARDS(TG) + 1
               FORWARD_POSITION(NUM_FORWARDS(TG),TG) = TRANS
            ELSEIF(DERIVATIVE_TYPE(TRANS) == 2 .OR. &
                                       DERIVATIVE_TYPE(TRANS) == 5) THEN
!
               IF(STRIKE_FREQUENCY(TRANS) == 'M') THEN
                  NUM_MONTH_CALLS(TG) = NUM_MONTH_CALLS(TG) + 1
                  MONTH_CALL_POSITION(NUM_MONTH_CALLS(TG),TG) = TRANS
               ELSEIF(STRIKE_FREQUENCY(TRANS) /= 'A') THEN
                  NUM_CALLS(TG) = NUM_CALLS(TG) + 1
                  CALL_POSITION(NUM_CALLS(TG),TG) = TRANS
               ENDIF
!
            ELSEIF(DERIVATIVE_TYPE(TRANS) == 3 .OR. &
                                       DERIVATIVE_TYPE(TRANS) == 6) THEN
!
               IF(STRIKE_FREQUENCY(TRANS) == 'M') THEN
                  NUM_MONTH_PUTS(TG) = NUM_MONTH_PUTS(TG) + 1
                  MONTH_PUT_POSITION(NUM_MONTH_PUTS(TG),TG) = TRANS
               ELSEIF(STRIKE_FREQUENCY(TRANS) /= 'A') THEN
                  NUM_PUTS(TG) = NUM_PUTS(TG) + 1
                  PUT_POSITION(NUM_PUTS(TG),TG) = TRANS
               ENDIF

            ELSEIF( DERIVATIVE_TYPE(TRANS) == 7) THEN
               NUM_INTER_CALLS(TG) = NUM_INTER_CALLS(TG) + 1
               INTER_CALL_POSITION(NUM_INTER_CALLS(TG),TG) = TRANS

            ELSEIF( DERIVATIVE_TYPE(TRANS) == 8) THEN
               NUM_INTER_PUTS(TG) = NUM_INTER_PUTS(TG) + 1
               INTER_PUT_POSITION(NUM_INTER_PUTS(TG),TG) = TRANS

            ELSEIF( DERIVATIVE_TYPE(TRANS) == 9) THEN
                  NUM_STORAGE(TG) = NUM_STORAGE(TG) + 1
                  STORAGE_POSITION(NUM_STORAGE(TG),TG) = TRANS
            ELSEIF( DERIVATIVE_TYPE(TRANS) == 10) THEN
                  NUM_LF_CALLS(TG) = NUM_LF_CALLS(TG) + 1
                  LF_CALL_POSITION(NUM_LF_CALLS(TG),TG) = TRANS
            ELSEIF( DERIVATIVE_TYPE(TRANS) == 11) THEN
                  NUM_LF_PUTS(TG) = NUM_LF_PUTS(TG) + 1
                  LF_PUT_POSITION(NUM_LF_PUTS(TG),TG) = TRANS
            ENDIF
!
         ENDDO
         IF(MAX_USER_CF > 0) THEN
! 051312    CALL USER DAY TYPES, CREATE ARRAY OF CF'S, INITIALIZE
!           TO THE BASE PATTERN, THEN PULL IN THE VECTOR OF CF'S
!           AND THE INDICES TO REMEMBER THE ID'S. THIS WAY, ONLY THE
!           SET OF CF TRANS ARE CREATED ONCE AND REFERENCED MANY TIMES
!           TO MAINTAIN SPEED. THEN PASS THE INDEX VALUE IN ON THE
!           DAILY AND HOURLY ARRAYS.
            TEMP_L = MONTHLY_CF_USER_DATA(R_MONTH, &
                                          MAX_USER_CF, &
                                          MONTHLY_USER_CF_COUNTER, &
                                          MONTHLY_USER_CF)
         ENDIF
         MONTHLY_ACTIVE_ENERGY_PRODUCTS = .TRUE.
         SAVE_MONTHLY_PRODUCTS_STATUS = MONTHLY_ACTIVE_ENERGY_PRODUCTS
      RETURN

      ENTRY UPDATE_SEASON_FOR_CT_RPT(R_MONTH)
         SAVE_MONTH = R_MONTH

         IF(SAVE_MONTH == 1) THEN

            IF(ALLOCATED(MONTHLY_CT_GROUP_REPORT)) &
                                     DEALLOCATE(MONTHLY_CT_GROUP_REPORT)
            MAX_MONTHLY_GROUPS = 99
            MAX_MONTHLY_GROUP_VARIABLES = 16
            ALLOCATE(MONTHLY_CT_GROUP_REPORT(0:12,0:MAX_MONTHLY_GROUPS, &
                                           MAX_MONTHLY_GROUP_VARIABLES))
            MONTHLY_CT_GROUP_REPORT = 0.
!
         ENDIF

         UPDATE_SEASON_FOR_CT_RPT = .TRUE.
!
      RETURN

      ENTRY CALCULATE_MONTHLY_TRANS_GROUP(R_MONTH,R_YEAR)

! 012408. MOVED FROM GET_MONTHLY_TRANS_VARIABLES.
         times_cmtg_entered=times_cmtg_entered+1
         IF(ALLOCATED(MONTHLY_TRANS_HOURS) .AND. RUN_TRANSACT) THEN
            TEST_DATE = 100*(R_YEAR +get_BASE_YEAR() - 1900) + R_MONTH
            LOCAL_YR = R_YEAR
            DO TRANS = 1, SAVE_ACTIVE_TRANSACTIONS
               IF(ANNUAL_HYBRID_BATTERY(TRANS) > 0 .AND. &
                        MONTHLY_TRANS_HOURS(TRANS,R_MONTH) < 1) THEN
! DOUBLE INDEX.
                  J = ANNUAL_HYBRID_BATTERY(TRANS)
                  J = ANNUAL_HYBRID_INDEX(J)
                  IF(J > 0) THEN
                     MONTHLY_CAPACITY(TRANS,R_MONTH) = &
                              MONTHLY_CAPACITY(TRANS,R_MONTH) + &
                                                      HOURLY_QUANTITY(J)
                  ENDIF
               ENDIF
               IF(TEST_DATE > END_EP(TRANS) .OR. &
                                      TEST_DATE < BEGIN_EP(TRANS)) CYCLE
               SELL_OR_BUY =  1.

               IF (MONTHLY_TRANS_HOURS(TRANS,R_MONTH) > 0. .OR. &
                                       DERIVATIVE_TYPE(TRANS) == 9) THEN
!
!
                  MONTHLY_CAPACITY(TRANS,R_MONTH) = &
                                             HOURLY_QUANTITY(TRANS)
!
                  IF(ANNUAL_HYBRID_BATTERY(TRANS) > 0) THEN
! DOUBLE INDEX.
                     J = ANNUAL_HYBRID_BATTERY(TRANS)
                     J = ANNUAL_HYBRID_INDEX(J)
                     IF(J > 0) THEN
                        MONTHLY_CAPACITY(TRANS,R_MONTH) = &
                              MONTHLY_CAPACITY(TRANS,R_MONTH) + &
                                                      HOURLY_QUANTITY(J)
                     ENDIF
                  ENDIF
               ELSE
                  MONTHLY_CAPACITY(TRANS,R_MONTH) = 0.0
               ENDIF
!
               TRANS_CAP = MONTHLY_CAPACITY(TRANS,R_MONTH)* &
                                                            SELL_OR_BUY
               TRANS_VAR_EXP = MONTHLY_ENERGY_COST(TRANS,R_MONTH) * &
                                                            SELL_OR_BUY
               TRANS_FIX_EXP = &
                   MONTHLY_TRANSACTION_COST(TRANS,R_MONTH) * SELL_OR_BUY
               TRANS_ENRG = MONTHLY_ENERGY(TRANS,R_MONTH)*SELL_OR_BUY
               CT_GRP = REPORTING_GENERATION_GROUP(TRANS)
               IF(CT_GRP > 0 .AND. CT_GRP <= 100) THEN
                  MONTHLY_CT_GROUP_REPORT(R_MONTH,CT_GRP,CT Capacity) = &
                     MONTHLY_CT_GROUP_REPORT(R_MONTH,CT_GRP,CT Capacity) &
                                                           + TRANS_CAP
                  MONTHLY_CT_GROUP_REPORT(R_MONTH, &
                                        CT_GRP,CT Equivalent Capacity) = &
                     MONTHLY_CT_GROUP_REPORT(R_MONTH, &
                                        CT_GRP,CT Equivalent Capacity) + &
                                                             TRANS_CAP
!
                  MONTHLY_CT_GROUP_REPORT(R_MONTH,CT_GRP,Generation) = &
                     MONTHLY_CT_GROUP_REPORT(R_MONTH,CT_GRP,Generation) &
                                                          + TRANS_ENRG
                  MONTHLY_CT_GROUP_REPORT(R_MONTH,CT_GRP,variable_om) = &
                     MONTHLY_CT_GROUP_REPORT(R_MONTH,CT_GRP,variable_om) &
                                                       + TRANS_VAR_EXP
                  MONTHLY_CT_GROUP_REPORT(R_MONTH,CT_GRP,fixed_om) = &
                     MONTHLY_CT_GROUP_REPORT(R_MONTH,CT_GRP,fixed_om) + &
                                                         TRANS_FIX_EXP
               ENDIF
!
               get_primary_mover_index_called_from="transobj2:0031"
               PM = PRIMARY_MOVER_INDEX_DB(UNIT_CATEGORY(TRANS))
               ST_TG = State_Index(TRANS)
! 121618.
               LOCAL_RPS_CONTRIB_PERCENT = RPS_Contribution(TRANS)
               IF(LOCAL_RPS_CONTRIB_PERCENT < -0.01) THEN
                  LOCAL_RPS_CONTRIB_PERCENT = &
                     ESCALATED_MONTHLY_VALUE(LOCAL_RPS_CONTRIB_PERCENT, &
                          ABS(INT(RPS_Contribution(TRANS),2)), &
                                               R_YEAR,INT(1,2),INT(1,2))
               ENDIF
               TEMP_ENRG = LOCAL_RPS_CONTRIB_PERCENT * &
                                               TRANS_ENRG * 0.01

               TEMP_CAP = LOCAL_RPS_CONTRIB_PERCENT * TRANS_CAP * 0.01
               TEMP_L1 = PUT_RPS_ENRG_CAP(RPS_PROGRAM_NUMBERs(TRANS),PM, &
                                          TEMP_ENRG,TEMP_CAP)
! RPS
               baddim=" "
               badidxval=" "
       ! merge3issue - look at st_tg due to new dims
               IF(ST_TG > 0 .AND. PM /= 13) THEN
                  TEMP_R4 = RPS_Contribution(TRANS)*TRANS_CAP * 0.01
                  
                  dim1ub=ubound(rps_trans_db, 1)
                  if(dim1ub<1) then
                    baddim="1"
                    badidxval="1"
                  end if
                  
                  dim2ub=ubound(rps_trans_db, 2)
                  if(dim2ub<st_tg) then
                    baddim="2"
                    badidxval=trim(itos(int(st_tg)))
                  end if
                  dim3ub=ubound(rps_trans_db, 3)
                  if(dim3ub<pm) then
                    baddim="3"
                    badidxval=trim(itos(int(pm)))
                  end if
                  dim4ub=ubound(rps_trans_db, 4)
                  if(dim4ub<r_month) then
                    baddim="4"
                    badidxval=trim(itos(int(r_month)))
                  end if
                   if((trim(baddim)/="") .and. (trim(baddim)/="3")) then
                    er_message="transobj2:0013 - attempt was made " // &
                    "to index into element " // trim(baddim) // " " // &
                    "of the rps_trans_db array. Actual array size: " // &
                    "(" // trim(itos(int(dim1ub))) // "," // &
                     trim(itos(int(dim2ub))) // "," // &
                    trim(itos(int(dim3ub))) // "," // &
                     trim(itos(int(dim4ub))) // "); " // &
                    "Requested elements: (1," // &
                    trim(itos(int(st_tg))) // ", " // &
                    trim(itos(int(pm))) // ", " // &
                    trim(itos(int(r_month))) // ")"
                    
                    call end_program(er_message)
                   else if(trim(baddim)/="") then
       call end_program("transobj2:0014 - pm of " // &
       trim(itos(int(pm))) // " is invalid because it " // &
       "exceeds bound " // trim(itos(int(dim3ub))))
                   end if
                  
                  RPS_TRANS_DB(1,ST_TG,PM,R_MONTH) = &
                            RPS_TRANS_DB(1,ST_TG,PM,R_MONTH) + TEMP_R4
                  RPS_TRANS_DB(1,ST_TG,PM,0) = &
                            RPS_TRANS_DB(1,ST_TG,PM,0) + TEMP_R4
! GWH
                  TEMP_R4 = RPS_Contribution(TRANS) * &
                                               TRANS_ENRG * 0.01 * 0.001
                  RPS_TRANS_DB(2,ST_TG,PM,R_MONTH) = &
                           RPS_TRANS_DB(2,ST_TG,PM,R_MONTH) + TEMP_R4
                  RPS_TRANS_DB(2,ST_TG,PM,0) = &
                            RPS_TRANS_DB(2,ST_TG,PM,0) + TEMP_R4
                  IF(TEST_DATE <= Tax_Credit_End_Date(TRANS) .AND. &
                         TEST_DATE >= Tax_Credit_Begin_Date(TRANS)) THEN
                     RPS_TRANS_DB(3,ST_TG,PM,R_MONTH) = &
                           RPS_TRANS_DB(3,ST_TG,PM,R_MONTH) + TEMP_R4 * &
                                            Production_Tax_Credit(TRANS)
                     RPS_TRANS_DB(3,ST_TG,PM,0) = &
                            RPS_TRANS_DB(3,ST_TG,PM,0) + TEMP_R4 * &
                                            Production_Tax_Credit(TRANS)
                  ENDIF
               ENDIF
!
               ST_TG = State_TG_Index(TRANS)
               IF(ST_TG > 0 .AND. PM /= 13) THEN
                  TEMP_R4 = RPS_Contribution(TRANS)*TRANS_CAP * 0.01
                  RPS_TRANS_DB(1,ST_TG,PM,R_MONTH) = &
                            RPS_TRANS_DB(1,ST_TG,PM,R_MONTH) + TEMP_R4
                  RPS_TRANS_DB(1,ST_TG,PM,0) = &
                            RPS_TRANS_DB(1,ST_TG,PM,0) + TEMP_R4
! GWH
                  TEMP_R4 = RPS_Contribution(TRANS) * &
                                               TRANS_ENRG * 0.01 * 0.001
                  RPS_TRANS_DB(2,ST_TG,PM,R_MONTH) = &
                           RPS_TRANS_DB(2,ST_TG,PM,R_MONTH) + TEMP_R4
                  RPS_TRANS_DB(2,ST_TG,PM,0) = &
                            RPS_TRANS_DB(2,ST_TG,PM,0) + TEMP_R4
                  IF(TEST_DATE <= Tax_Credit_End_Date(TRANS) .AND. &
                         TEST_DATE >= Tax_Credit_Begin_Date(TRANS)) THEN
                     RPS_TRANS_DB(3,ST_TG,PM,R_MONTH) = &
                           RPS_TRANS_DB(3,ST_TG,PM,R_MONTH) + TEMP_R4 * &
                                            Production_Tax_Credit(TRANS)
                     RPS_TRANS_DB(3,ST_TG,PM,0) = &
                            RPS_TRANS_DB(3,ST_TG,PM,0) + TEMP_R4 * &
                                            Production_Tax_Credit(TRANS)
                  ENDIF
               ENDIF
! GRX_RPS

               IF(PROPOSED_RESOURCE_POSITION(TRANS) > 0 .AND. &
                                                          PM /= 16) THEN
                  IF(QUANTITY_OF_PRODUCT(TRANS) > 0.001) THEN
                     TRANS_CAP = TRANS_CAP * &
                          MIN(SAVED_QUANT_OF_PRODUCT(TRANS), &
                                    QUANTITY_OF_PRODUCT(TRANS)) / &
                                           QUANTITY_OF_PRODUCT(TRANS)
                     TRANS_ENRG = TRANS_ENRG * &
                          MIN(SAVED_QUANT_OF_PRODUCT(TRANS), &
                                    QUANTITY_OF_PRODUCT(TRANS)) / &
                                           QUANTITY_OF_PRODUCT(TRANS)
                  ELSE
                     TRANS_CAP = 0.0
                     TRANS_ENRG = 0.0
                  ENDIF
               ENDIF
               ST_TG = State_Index(TRANS)
               IF(ST_TG > 0 .AND. PM /= 13) THEN
                  TEMP_R4 = RPS_Contribution(TRANS)*TRANS_CAP * 0.01
                  GRX_RPS_TRANS_DB(1,ST_TG,PM,R_MONTH) = &
                          GRX_RPS_TRANS_DB(1,ST_TG,PM,R_MONTH) + TEMP_R4
                  GRX_RPS_TRANS_DB(1,ST_TG,PM,0) = &
                            GRX_RPS_TRANS_DB(1,ST_TG,PM,0) + TEMP_R4
! GWH
                  TEMP_R4 = RPS_Contribution(TRANS) * &
                                               TRANS_ENRG * 0.01 * 0.001
                  GRX_RPS_TRANS_DB(2,ST_TG,PM,R_MONTH) = &
                          GRX_RPS_TRANS_DB(2,ST_TG,PM,R_MONTH) + TEMP_R4
                  GRX_RPS_TRANS_DB(2,ST_TG,PM,0) = &
                            GRX_RPS_TRANS_DB(2,ST_TG,PM,0) + TEMP_R4
                  IF(TEST_DATE <= Tax_Credit_End_Date(TRANS) .AND. &
                         TEST_DATE >= Tax_Credit_Begin_Date(TRANS)) THEN
                     GRX_RPS_TRANS_DB(3,ST_TG,PM,R_MONTH) = &
                        GRX_RPS_TRANS_DB(3,ST_TG,PM,R_MONTH) + TEMP_R4 * &
                                            Production_Tax_Credit(TRANS)
                     GRX_RPS_TRANS_DB(3,ST_TG,PM,0) = &
                            GRX_RPS_TRANS_DB(3,ST_TG,PM,0) + TEMP_R4 * &
                                            Production_Tax_Credit(TRANS)
                  ENDIF
               ENDIF
!
               ST_TG = State_TG_Index(TRANS)
               IF(ST_TG > 0 .AND. PM /= 13) THEN
                  TEMP_R4 = RPS_Contribution(TRANS)*TRANS_CAP * 0.01
                  GRX_RPS_TRANS_DB(1,ST_TG,PM,R_MONTH) = &
                          GRX_RPS_TRANS_DB(1,ST_TG,PM,R_MONTH) + TEMP_R4
                  GRX_RPS_TRANS_DB(1,ST_TG,PM,0) = &
                            GRX_RPS_TRANS_DB(1,ST_TG,PM,0) + TEMP_R4
! GWH
                  TEMP_R4 = RPS_Contribution(TRANS) * &
                                               TRANS_ENRG * 0.01 * 0.001
                  GRX_RPS_TRANS_DB(2,ST_TG,PM,R_MONTH) = &
                          GRX_RPS_TRANS_DB(2,ST_TG,PM,R_MONTH) + TEMP_R4
                  GRX_RPS_TRANS_DB(2,ST_TG,PM,0) = &
                            GRX_RPS_TRANS_DB(2,ST_TG,PM,0) + TEMP_R4
                  IF(TEST_DATE <= Tax_Credit_End_Date(TRANS) .AND. &
                         TEST_DATE >= Tax_Credit_Begin_Date(TRANS)) THEN
                     GRX_RPS_TRANS_DB(3,ST_TG,PM,R_MONTH) = &
                        GRX_RPS_TRANS_DB(3,ST_TG,PM,R_MONTH) + TEMP_R4 * &
                                            Production_Tax_Credit(TRANS)
                     GRX_RPS_TRANS_DB(3,ST_TG,PM,0) = &
                            GRX_RPS_TRANS_DB(3,ST_TG,PM,0) + TEMP_R4 * &
                                            Production_Tax_Credit(TRANS)
                  ENDIF
               ENDIF
!
            ENDDO ! TRANSACTIONS
            CALCULATE_MONTHLY_TRANS_GROUP = .TRUE.
         ELSE
            CALCULATE_MONTHLY_TRANS_GROUP = .FALSE.
         ENDIF
      RETURN

      ENTRY GET_TRANS_RPS_SUM(R_MONTH,R_ST_TG,R_RESOURCE_RPS_VARS)


            GET_TRANS_RPS_SUM = .TRUE.
            DO PM = 2, 8 ! 8 FOR STORAGE
               I = RPS_INDEX_TRANSLATE(PM)
               IF(R_ST_TG > 0 .AND. R_ST_TG < 258 .AND. I > 0 .AND. &
                                                            I < 17) THEN
                  R_RESOURCE_RPS_VARS(PM) = R_RESOURCE_RPS_VARS(PM) + &
                                      RPS_TRANS_DB(2,R_ST_TG,I,R_MONTH)
                  R_RESOURCE_RPS_VARS(PM+8) = &
                               R_RESOURCE_RPS_VARS(PM+8) + &
                                      RPS_TRANS_DB(1,R_ST_TG,I,R_MONTH)
! TOTAL
                  R_RESOURCE_RPS_VARS(1) = R_RESOURCE_RPS_VARS(1) + &
                                      RPS_TRANS_DB(2,R_ST_TG,I,R_MONTH)
                  R_RESOURCE_RPS_VARS(9) = R_RESOURCE_RPS_VARS(9) + &
                                      RPS_TRANS_DB(1,R_ST_TG,I,R_MONTH)
               ENDIF
            END DO
!         ENDIF
      RETURN

      ENTRY GET_TRANS_GRX_RPS_SUM(R_MONTH,R_ST_TG,R_RESOURCE_RPS_VARS)

            GET_TRANS_GRX_RPS_SUM = .TRUE.
            DO PM = 2, 8
               I = RPS_INDEX_TRANSLATE(PM)
               IF(R_ST_TG > 0 .AND. R_ST_TG < 258 .AND. I > 0 .AND. &
                                                            I < 17) THEN
                  R_RESOURCE_RPS_VARS(PM) = R_RESOURCE_RPS_VARS(PM) + &
                                   GRX_RPS_TRANS_DB(2,R_ST_TG,I,R_MONTH)
                  R_RESOURCE_RPS_VARS(PM+8) = &
                               R_RESOURCE_RPS_VARS(PM+8) + &
                                   GRX_RPS_TRANS_DB(1,R_ST_TG,I,R_MONTH)
! TOTAL
                  R_RESOURCE_RPS_VARS(1) = R_RESOURCE_RPS_VARS(1) + &
                                   GRX_RPS_TRANS_DB(2,R_ST_TG,I,R_MONTH)
                  R_RESOURCE_RPS_VARS(9) = R_RESOURCE_RPS_VARS(9) + &
                                   GRX_RPS_TRANS_DB(1,R_ST_TG,I,R_MONTH)
               ENDIF
            END DO
!         ENDIF
      RETURN

      ENTRY GET_MONTHLY_CT_GROUP_REPORT(R_MONTH,R_GROUP, &
                                        R_MONTHLY_GROUP_REPORT)


         R_MONTHLY_GROUP_REPORT(SAVE_MONTH,R_GROUP,CT Capacity) = &
                R_MONTHLY_GROUP_REPORT(SAVE_MONTH,R_GROUP,CT Capacity) + &
                 MONTHLY_CT_GROUP_REPORT(SAVE_MONTH,R_GROUP,CT Capacity)
! ADDED 5/9/99. FOR BURESH.
         R_MONTHLY_GROUP_REPORT(SAVE_MONTH, &
                                       R_GROUP,CT Equivalent Capacity) = &
                R_MONTHLY_GROUP_REPORT(SAVE_MONTH, &
                                       R_GROUP,CT Equivalent Capacity) + &
                MONTHLY_CT_GROUP_REPORT(SAVE_MONTH, &
                                         R_GROUP,CT Equivalent Capacity)
!
         R_MONTHLY_GROUP_REPORT(SAVE_MONTH,R_GROUP,Generation) = &
                R_MONTHLY_GROUP_REPORT(SAVE_MONTH,R_GROUP,Generation) + &
                 MONTHLY_CT_GROUP_REPORT(SAVE_MONTH,R_GROUP,Generation)
         R_MONTHLY_GROUP_REPORT(SAVE_MONTH,R_GROUP,variable_om) = &
                R_MONTHLY_GROUP_REPORT(SAVE_MONTH,R_GROUP,variable_om) + &
                 MONTHLY_CT_GROUP_REPORT(SAVE_MONTH,R_GROUP,variable_om)
         R_MONTHLY_GROUP_REPORT(SAVE_MONTH,R_GROUP,fixed_om) = &
                 R_MONTHLY_GROUP_REPORT(SAVE_MONTH,R_GROUP,fixed_om) + &
                    MONTHLY_CT_GROUP_REPORT(SAVE_MONTH,R_GROUP,fixed_om)
         R_MONTHLY_GROUP_REPORT(SAVE_MONTH,R_GROUP,sulfur_o2) = &
                 R_MONTHLY_GROUP_REPORT(SAVE_MONTH,R_GROUP,sulfur_o2) + &
                   MONTHLY_CT_GROUP_REPORT(SAVE_MONTH,R_GROUP,sulfur_o2)
!
         GET_MONTHLY_CT_GROUP_REPORT = .TRUE.
      RETURN

!
      ENTRY GET_ANNUAL_INTER_CAPACITY(R_TG)
!

!
! 072507. Used only for reserve margin calculations.
!
         IF(ALLOCATED(ANNU_PLANNING_INTRPT_CAPACITY) .AND. &
                                                      RUN_TRANSACT) THEN
            GET_ANNUAL_INTER_CAPACITY = &
                      ANNU_PLANNING_INTRPT_CAPACITY(R_TG) + &
                         DEV_TG_CAP(10,R_TG,MIN(globecom_YEAR,get_globecom_study_period()))

         ELSE
            GET_ANNUAL_INTER_CAPACITY = 0.
         ENDIF
!
      RETURN
!

!
      ENTRY GET_MONTHLY_CONTINGENT_CAP(R_TG)
!

!
         IF(ALLOCATED(MONTHLY_CONTINGENT_CAPACITY)) THEN
            GET_MONTHLY_CONTINGENT_CAP = &
                                       MONTHLY_CONTINGENT_CAPACITY(R_TG)
         ELSE
            GET_MONTHLY_CONTINGENT_CAP = 0.
         ENDIF
!
      RETURN

!
      ENTRY GET_NUM_MONTHLY_TRANSACTIONS()
!

!
         GET_NUM_MONTHLY_TRANSACTIONS = MONTHLY_ACTIVE_TRANSACTIONS
!
      RETURN
!

!
      ENTRY GET_NUM_ANNUAL_TRANSACTIONS()
!

!
         GET_NUM_ANNUAL_TRANSACTIONS = ANNUAL_ACTIVE_TRANSACTIONS

!
      RETURN

!
      ENTRY IS_TRANS_RESOURCE_ACTIVE(R_TRANS,R_DATE1,R_DATE2)
!

         IF(R_DATE1 > END_EP(R_TRANS) .OR. &
                                       R_DATE2 < BEGIN_EP(R_TRANS)) THEN
            IS_TRANS_RESOURCE_ACTIVE = .FALSE.
         ELSE
            IS_TRANS_RESOURCE_ACTIVE = .TRUE.
         ENDIF
      RETURN

!
      ENTRY GET_REPORTING_GENERATION_GROUP(R_TRANS)
!

         GET_REPORTING_GENERATION_GROUP = &
                                     REPORTING_GENERATION_GROUP(R_TRANS)
      RETURN

!
      ENTRY GET_NUM_SAVE_ACTIVE_TRANSACT()
!

!
         GET_NUM_SAVE_ACTIVE_TRANSACT = SAVE_ACTIVE_TRANSACTIONS

!
      RETURN

!
      ENTRY GET_NUM_FISCAL_TRANSACTIONS()
!

!
         GET_NUM_FISCAL_TRANSACTIONS = FISCAL_ACTIVE_TRANSACTIONS

!
      RETURN

!
      ENTRY GET_NUM_SCENARIO_TRANSACTIONS()
!

!
         GET_NUM_SCENARIO_TRANSACTIONS = NUM_TRANSACTIONS

!
      RETURN

      ENTRY POWER_DERIV_REV_EXP_BY_CLASS( R_CLASS, &
                                          R_MONTH, &
                                          R_TOTAL_REV, &
                                          R_TOTAL_EXP)

         POWER_DERIV_REV_EXP_BY_CLASS = .TRUE.
         IF(.NOT. SAVE_MONTHLY_PRODUCTS_STATUS .OR. &
                                              .NOT. RUN_TRANSACT) RETURN
!
         IF(R_CLASS-1 <= MAX_ASSET_GROUPS .AND. R_CLASS-1 >= 0) THEN
            ASSET_CLASS = ASSET_CLASS_GROUPS_INDEX(R_CLASS-1)
!
            IF(ASSET_CLASS >= 0) THEN
!
               MONTH = R_MONTH
               R_TOTAL_REV = .000001* &
                     (MONTHLY_AC_REVENUE(ASSET_CLASS,1,MONTH) + &
                      MONTHLY_AC_REVENUE(ASSET_CLASS,2,MONTH) + &
                      MONTHLY_AC_REVENUE(ASSET_CLASS,3,MONTH) + &
                      MONTHLY_AC_REVENUE(ASSET_CLASS,4,MONTH))
               R_TOTAL_EXP = .000001* &
                     (MONTHLY_AC_EXPENSE(ASSET_CLASS,1,MONTH) + &
                      MONTHLY_AC_EXPENSE(ASSET_CLASS,2,MONTH) + &
                      MONTHLY_AC_EXPENSE(ASSET_CLASS,3,MONTH) + &
                      MONTHLY_AC_EXPENSE(ASSET_CLASS,4,MONTH))
            ENDIF
         ENDIF
      RETURN

      ENTRY RETURN_ANNUL_DERIV_VARIABLES(R_CLASS, &
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
!         CALL DOES_TF_FILE_EXIST(TF_FILE_EXISTS)
!
         RETURN_ANNUL_DERIV_VARIABLES = 1
         IF(.NOT. SAVE_MONTHLY_PRODUCTS_STATUS .OR. &
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
               PHY_DERIV_VAR_REVENUE = .000001 * &
                         SUM(MONTHLY_AC_REVENUE(ASSET_CLASS,1,MONTH:12))
               PHY_DERIV_FIX_REVENUE = .000001 * &
                         SUM(MONTHLY_AC_REVENUE(ASSET_CLASS,2,MONTH:12))
               PHY_DERIV_VAR_EXPENSE = .000001 * &
                         SUM(MONTHLY_AC_EXPENSE(ASSET_CLASS,1,MONTH:12))
               PHY_DERIV_FIX_EXPENSE = .000001 * &
                         SUM(MONTHLY_AC_EXPENSE(ASSET_CLASS,2,MONTH:12))
!
               PHY_DERIV_REVENUE_ENERGY = .001 * &
                  SUM(MONTHLY_AC_REVENUE_ENERGY(ASSET_CLASS,1,MONTH:12))
               PHY_DERIV_EXPENSE_ENERGY = .001 * &
                  SUM(MONTHLY_AC_EXPENSE_ENERGY(ASSET_CLASS,1,MONTH:12))
!
!
               FIN_DERIV_VAR_REVENUE = .000001 * &
                         SUM(MONTHLY_AC_REVENUE(ASSET_CLASS,3,MONTH:12))
               FIN_DERIV_FIX_REVENUE = .000001 * &
                         SUM(MONTHLY_AC_REVENUE(ASSET_CLASS,4,MONTH:12))
               FIN_DERIV_VAR_EXPENSE = .000001 * &
                         SUM(MONTHLY_AC_EXPENSE(ASSET_CLASS,3,MONTH:12))
               FIN_DERIV_FIX_EXPENSE = .000001 * &
                         SUM(MONTHLY_AC_EXPENSE(ASSET_CLASS,4,MONTH:12))
!
               FIN_DERIV_REVENUE_ENERGY = .001 * &
                  SUM(MONTHLY_AC_REVENUE_ENERGY(ASSET_CLASS,3,MONTH:12))
               FIN_DERIV_EXPENSE_ENERGY = .001 * &
                  SUM(MONTHLY_AC_EXPENSE_ENERGY(ASSET_CLASS,3,MONTH:12))

            ENDIF
         ENDIF
      RETURN

      ENTRY RET_MNTHLY_DERIV_INC_VARIABLES(R_CLASS, &
                                                  INC_MONTH_VARS)

!
         RET_MNTHLY_DERIV_INC_VARIABLES = 1
         IF(.NOT. SAVE_MONTHLY_PRODUCTS_STATUS .OR. &
                                              .NOT. RUN_TRANSACT) RETURN
!
         IF(R_CLASS-1 <= MAX_ASSET_GROUPS .AND. R_CLASS-1 >= 0) THEN
            ASSET_CLASS = ASSET_CLASS_GROUPS_INDEX(R_CLASS-1)
!
            IF(ASSET_CLASS >= 0) THEN
!
               MONTH = TRANSACT_ANLST_RSLTS_AVAIL_STG()
               IF(MONTH >= 13) RETURN
               DO MO = MONTH, 12
!
                  INC_MONTH_VARS(MO,PhysicalRevenueVariable) = &
                       INC_MONTH_VARS(MO,PhysicalRevenueVariable) &
                       + MONTHLY_AC_REVENUE(ASSET_CLASS,1,MO)*.000001
!
                  INC_MONTH_VARS(MO,PhysicalRevenueFixed) = &
                       INC_MONTH_VARS(MO,PhysicalRevenueFixed) &
                       + MONTHLY_AC_REVENUE(ASSET_CLASS,2,MO)*.000001
!
                  INC_MONTH_VARS(MO,PhysicalExpenseVariable) = &
                       INC_MONTH_VARS(MO,PhysicalExpenseVariable) &
                       + MONTHLY_AC_EXPENSE(ASSET_CLASS,1,MO)*.000001
!
                  INC_MONTH_VARS(MO,PhysicalExpenseFixed) = &
                       INC_MONTH_VARS(MO,PhysicalExpenseFixed) &
                       + MONTHLY_AC_EXPENSE(ASSET_CLASS,2,MO)*.000001
!
                  INC_MONTH_VARS(MO,FinancialRevenueVariable) = &
                       INC_MONTH_VARS(MO,FinancialRevenueVariable) &
                       + MONTHLY_AC_REVENUE(ASSET_CLASS,3,MO)*.000001
!
                  INC_MONTH_VARS(MO,FinancialRevenueFixed) = &
                       INC_MONTH_VARS(MO,FinancialRevenueFixed) &
                       + MONTHLY_AC_REVENUE(ASSET_CLASS,4,MO)*.000001
!
                  INC_MONTH_VARS(MO,FinancialExpenseVariable) = &
                       INC_MONTH_VARS(MO,FinancialExpenseVariable) &
                       + MONTHLY_AC_EXPENSE(ASSET_CLASS,3,MO)*.000001
!
                  INC_MONTH_VARS(MO,FinancialExpenseFixed) = &
                       INC_MONTH_VARS(MO,FinancialExpenseFixed) &
                       + MONTHLY_AC_EXPENSE(ASSET_CLASS,4,MO)*.000001
!
               ENDDO
            ENDIF
         ENDIF
      RETURN

      ENTRY RET_MNTHLY_DERIV_CASH_VARIABLES(R_CLASS, &
                                                CASH_MONTH_VARS)

!
         RET_MNTHLY_DERIV_CASH_VARIABLES = 1
         IF(.NOT. SAVE_MONTHLY_PRODUCTS_STATUS .OR. &
                                              .NOT. RUN_TRANSACT) RETURN
!
         IF(R_CLASS-1 <= MAX_ASSET_GROUPS .AND. R_CLASS-1 >= 0) THEN
            ASSET_CLASS = ASSET_CLASS_GROUPS_INDEX(R_CLASS-1)
!
            IF(ASSET_CLASS >= 0) THEN
!
               MONTH = TRANSACT_ANLST_RSLTS_AVAIL_STG()
               IF(MONTH >= 13) RETURN
               DO MO = MONTH, 12
                  CASH_MONTH_VARS(MO,CashPhysicalRevenueVariable) = &
                      CASH_MONTH_VARS(MO,CashPhysicalRevenueVariable) &
                       + MONTHLY_AC_REVENUE(ASSET_CLASS,1,MO)*.000001
                  CASH_MONTH_VARS(MO,CashPhysicalRevenueFixed) = &
                         CASH_MONTH_VARS(MO,CashPhysicalRevenueFixed) &
                         + MONTHLY_AC_REVENUE(ASSET_CLASS,2,MO)*.000001
                  CASH_MONTH_VARS(MO,CashPhysicalExpenseVariable) = &
                      CASH_MONTH_VARS(MO,CashPhysicalExpenseVariable) &
                      + MONTHLY_AC_EXPENSE(ASSET_CLASS,1,MO)*.000001
                  CASH_MONTH_VARS(MO,CashPhysicalExpenseFixed) = &
                         CASH_MONTH_VARS(MO,CashPhysicalExpenseFixed) &
                         + MONTHLY_AC_EXPENSE(ASSET_CLASS,2,MO)*.000001
                  CASH_MONTH_VARS(MO,cash_financl_rev_variable) = &
                     CASH_MONTH_VARS(MO,cash_financl_rev_variable) &
                     + MONTHLY_AC_REVENUE(ASSET_CLASS,3,MO)*.000001
                  CASH_MONTH_VARS(MO,CashFinancialRevenueFixed) = &
                        CASH_MONTH_VARS(MO,CashFinancialRevenueFixed) &
                        + MONTHLY_AC_REVENUE(ASSET_CLASS,4,MO)*.000001
                  CASH_MONTH_VARS(MO,CashFinancialExpenseVariable) = &
                     CASH_MONTH_VARS(MO,CashFinancialExpenseVariable) &
                     + MONTHLY_AC_EXPENSE(ASSET_CLASS,3,MO)*.000001
                  CASH_MONTH_VARS(MO,CashFinancialExpenseFixed) = &
                        CASH_MONTH_VARS(MO,CashFinancialExpenseFixed) &
                        + MONTHLY_AC_EXPENSE(ASSET_CLASS,4,MO)*.000001
!
               ENDDO
            ENDIF
         ENDIF
      RETURN


      ENTRY GET_TRANS_NAME(R_TRANS_NUM,R_GET_MONTHLY_TRANS_VARIABLES)

         R_GET_MONTHLY_TRANS_VARIABLES = &
                     TRANSACTION_NAME(R_TRANS_NUM)(1:5)//' '// &
                     COUNTERPARTY_NAME(R_TRANS_NUM)(1:13)//' '// &
                           TYPE_CHAR(DERIVATIVE_TYPE(R_TRANS_NUM))
         GET_TRANS_NAME = .TRUE.
      RETURN

!
      ENTRY GET_MONTHLY_TRANS_VARIABLES( &
                        R_TRANS_NUM, &
                        R_TRANS_ENRG, &
                        R_TRANS_CAP, &
                        R_TRANS_VAR_EXP, &
                        R_TRANS_VAR_MWH, &
                        R_TRANS_FIX_EXP, &
                        R_TRANS_REV, &
                        R_TRANS_REV_MWH, &
                        R_TRANS_HOURS, &
                        R_PRODUCT_HOURS, &
                        R_TRANS_STRIKES, &
                        R_MONTH, &
                        R_GET_MONTHLY_TRANS_VARIABLES, &
                        R_SUM_ANNUAL, & !  INT*2 FOR MONTHLY/ANNUAL/FISCAL
                        R_YES_REPORT_PRODUCT, & !  LOGICAL
                        LOCAL_TG, &
                        R_PM, &
                        R_FT, &
                        R_ON_LINE_DATE, &
                        R_OFF_LINE_DATE, &
                        R_UNIT_ID, &
                        R_ASSET_CLASS, &
                        R_TRANS_NOT_ACTIVE, &
                        R_LOCAL_YEAR, &
                        R_TRANS_CHARGING_ENERGY)
!

!
!
!
         GET_MONTHLY_TRANS_VARIABLES = .FALSE.
         IF(.NOT. SAVE_ENERGY_PRODUCTS_STATUS) RETURN
         GET_MONTHLY_TRANS_VARIABLES = .TRUE.
!
! 121806. BIG CHANGE. PULL DOWN EACH TRANS FOR DENSE REPORTING.
!
         TRANS = R_TRANS_NUM
!
         IF (PRODUCT_ACTIVE(TRANS) /= 'T' .AND. &
                                    PRODUCT_ACTIVE(TRANS) /= 'E') THEN
            R_TRANS_NOT_ACTIVE = .TRUE.
            RETURN
         ELSE
            R_TRANS_NOT_ACTIVE = .FALSE.
         ENDIF
! 042208. MOVED UP

         IF(ABS(HYBRID_BATTERY(TRANS)) > 0 ) THEN
            R_GET_MONTHLY_TRANS_VARIABLES = 'H '// &
                     TRANSACTION_NAME(TRANS)(1:5)//' '// &
                     COUNTERPARTY_NAME(TRANS)(1:11)//' '// &
                                       TYPE_CHAR(DERIVATIVE_TYPE(TRANS))
         ELSE
            R_GET_MONTHLY_TRANS_VARIABLES = &
                     TRANSACTION_NAME(TRANS)(1:5)//' '// &
                     COUNTERPARTY_NAME(TRANS)(1:13)//' '// &
                                       TYPE_CHAR(DERIVATIVE_TYPE(TRANS))
         ENDIF
         IF(.NOT. RUN_TRANSACT) RETURN
!
         IF(R_SUM_ANNUAL > 0) THEN ! MONTHLY SUM FROM THE CLREPORT FILE
!
!
            MONTHLY_ENERGY(TRANS,0) = MONTHLY_ENERGY(TRANS,0) + &
                                           MONTHLY_ENERGY(TRANS,R_MONTH)
            MONTHLY_CHARGE(TRANS,0) = MONTHLY_CHARGE(TRANS,0) + &
                                           MONTHLY_CHARGE(TRANS,R_MONTH)
            MONTHLY_ENERGY_COST(TRANS,0) = &
                        MONTHLY_ENERGY_COST(TRANS,0) + &
                                      MONTHLY_ENERGY_COST(TRANS,R_MONTH)
            MONTHLY_ENERGY_REVENUE(TRANS,0) = &
                        MONTHLY_ENERGY_REVENUE(TRANS,0) + &
                                   MONTHLY_ENERGY_REVENUE(TRANS,R_MONTH)
            MONTHLY_STRIKES(TRANS,0) = &
                        MONTHLY_STRIKES(TRANS,0) + &
                                   MONTHLY_STRIKES(TRANS,R_MONTH)
            MONTHLY_TRANS_HOURS(TRANS,0) = &
                        MONTHLY_TRANS_HOURS(TRANS,0) + &
                                   MONTHLY_TRANS_HOURS(TRANS,R_MONTH)
            MONTHLY_PRODUCT_HOURS(TRANS,0) = &
                        MONTHLY_PRODUCT_HOURS(TRANS,0) + &
                                   MONTHLY_PRODUCT_HOURS(TRANS,R_MONTH)
! 11/19/02. MOVED MONTH_PRODUCT_DAYS CALC
            MONTHLY_PRODUCT_DAYS(TRANS,0) = &
                        MONTHLY_PRODUCT_DAYS(TRANS,0) + &
                                   MONTHLY_PRODUCT_DAYS(TRANS,R_MONTH)

            MONTHLY_CAPACITY(TRANS,0) = &
                            MONTHLY_CAPACITY(TRANS,0) + &
                                      MONTHLY_CAPACITY(TRANS,R_MONTH)

!
! 10/02/02. FISCAL YEAR REPORTING
!
            FISCAL_ENERGY(TRANS) = FISCAL_ENERGY(TRANS) + &
                                           MONTHLY_ENERGY(TRANS,R_MONTH)
            FISCAL_ENERGY_COST(TRANS) = &
                        FISCAL_ENERGY_COST(TRANS) + &
                                      MONTHLY_ENERGY_COST(TRANS,R_MONTH)
            FISCAL_ENERGY_REVENUE(TRANS) = &
                        FISCAL_ENERGY_REVENUE(TRANS) + &
                                   MONTHLY_ENERGY_REVENUE(TRANS,R_MONTH)
            FISCAL_STRIKES(TRANS) = &
                        FISCAL_STRIKES(TRANS) + &
                                   MONTHLY_STRIKES(TRANS,R_MONTH)
            FISCAL_TRANS_HOURS(TRANS) = &
                        FISCAL_TRANS_HOURS(TRANS) + &
                                   MONTHLY_TRANS_HOURS(TRANS,R_MONTH)
            FISCAL_PRODUCT_HOURS(TRANS) = &
                        FISCAL_PRODUCT_HOURS(TRANS) + &
                                   MONTHLY_PRODUCT_HOURS(TRANS,R_MONTH)

            FISCAL_PRODUCT_DAYS(TRANS) = &
                        FISCAL_PRODUCT_DAYS(TRANS) + &
                                   MONTHLY_PRODUCT_DAYS(TRANS,R_MONTH)

               FISCAL_CAPACITY(TRANS) = &
                            FISCAL_CAPACITY(TRANS) + &
                                      MONTHLY_CAPACITY(TRANS,R_MONTH)

         ENDIF

!
! CHECK FOR WVPA POWER COST REPORT AND DERIVATIVE NOT ACTIVE
!
!         IF(REPORT_PRODUCT(TRANS) == 'T') THEN
!
! 121806.
!
         TEST_DATE = 100*(INT(R_LOCAL_YEAR,2) - 1900) + R_MONTH
         LOCAL_YR = INT(R_LOCAL_YEAR,2) - get_BASE_YEAR()
         IF(ANNUAL_HYBRID_BATTERY(TRANS) > 0) THEN
! DOUBLE INDEX.
            J = ANNUAL_HYBRID_BATTERY(TRANS)
            J = ANNUAL_HYBRID_INDEX(J)
            IF(J > 0) THEN
               LOCAL_BEGIN_EP = MIN(BEGIN_EP(TRANS),BEGIN_EP(J))
               LOCAL_END_EP = MAX(END_EP(TRANS),END_EP(J))
            ELSE
               LOCAL_BEGIN_EP = BEGIN_EP(TRANS)
               LOCAL_END_EP = END_EP(TRANS)
            ENDIF
         ELSE
            LOCAL_BEGIN_EP = BEGIN_EP(TRANS)
            LOCAL_END_EP = END_EP(TRANS)
         ENDIF
         IF(R_MONTH > 0 .AND. R_SUM_ANNUAL >= 0 .AND. &
               (TEST_DATE > LOCAL_END_EP .OR. &
                                      TEST_DATE < LOCAL_BEGIN_EP)) THEN

            R_TRANS_ENRG = 0.
            R_TRANS_CAP = 0.
            R_TRANS_VAR_EXP = 0.
            R_TRANS_VAR_MWH = 0.
            R_TRANS_FIX_EXP = 0.
            R_TRANS_REV = 0.
            R_TRANS_STRIKES = 0.
            R_TRANS_REV_MWH = 0.
            R_TRANS_HOURS = 0.
            R_PRODUCT_HOURS = 0.
            R_TRANS_CHARGING_ENERGY = 0.0
! 10/02/02.
         ELSEIF(R_SUM_ANNUAL == -1) THEN

               SELL_OR_BUY =  1.

            R_TRANS_ENRG = FISCAL_ENERGY(TRANS)*SELL_OR_BUY
            R_TRANS_CAP = FISCAL_CAPACITY(TRANS)* SELL_OR_BUY
            R_TRANS_VAR_EXP = FISCAL_ENERGY_COST(TRANS) * &
                                                            SELL_OR_BUY
            IF(ABS(R_TRANS_ENRG) > .1) THEN
               R_TRANS_VAR_MWH = ABS(R_TRANS_VAR_EXP/R_TRANS_ENRG)
            ELSE
               R_TRANS_VAR_MWH = MONTHLY_ENERGY_PRICE(TRANS)
            ENDIF
            R_TRANS_FIX_EXP = &
                            FISCAL_TRANSACTION_COST(TRANS) * SELL_OR_BUY
            R_TRANS_REV = FISCAL_ENERGY_REVENUE(TRANS)*SELL_OR_BUY
            R_TRANS_STRIKES = FLOAT(FISCAL_STRIKES(TRANS))
            IF(FISCAL_ENERGY(TRANS) /= 0.) THEN
               R_TRANS_REV_MWH = &
                           ABS(FISCAL_ENERGY_REVENUE(TRANS)/ &
                                                   FISCAL_ENERGY(TRANS))
            ELSE
               R_TRANS_REV_MWH = 0.
            ENDIF
            R_TRANS_HOURS = FLOAT(FISCAL_TRANS_HOURS(TRANS))
            R_PRODUCT_HOURS = &
                             FLOAT(FISCAL_PRODUCT_HOURS(TRANS))
         ELSE
!
! 3/13/01. GAT.
! 10/06/03. BIG CHANGE.
!
!            IF(EXPENSE_ASSIGNMENT(TRANS) == LOCAL_ASSIGNMENT(1)) THEN
!               SELL_OR_BUY = -1.
!            ELSE
               SELL_OR_BUY =  1.
!            ENDIF
!
            R_TRANS_ENRG = MONTHLY_ENERGY(TRANS,R_MONTH)*SELL_OR_BUY
            R_TRANS_CHARGING_ENERGY = -MONTHLY_CHARGE(TRANS,R_MONTH)* &
                                                            SELL_OR_BUY
            R_TRANS_CAP = MONTHLY_CAPACITY(TRANS,R_MONTH)* &
                                                            SELL_OR_BUY
            R_TRANS_VAR_EXP = MONTHLY_ENERGY_COST(TRANS,R_MONTH) * &
                                                            SELL_OR_BUY
            IF(ABS(R_TRANS_ENRG) > .1) THEN
               R_TRANS_VAR_MWH = ABS(R_TRANS_VAR_EXP/R_TRANS_ENRG)
            ELSE
               R_TRANS_VAR_MWH = MONTHLY_ENERGY_PRICE(TRANS)
            ENDIF
            R_TRANS_FIX_EXP = &
                   MONTHLY_TRANSACTION_COST(TRANS,R_MONTH) * SELL_OR_BUY
            R_TRANS_REV = MONTHLY_ENERGY_REVENUE(TRANS,R_MONTH) * &
                                                             SELL_OR_BUY
            R_TRANS_STRIKES = FLOAT(MONTHLY_STRIKES(TRANS,R_MONTH))
            IF(MONTHLY_ENERGY(TRANS,R_MONTH) /= 0.) THEN
               R_TRANS_REV_MWH = &
                           ABS(MONTHLY_ENERGY_REVENUE(TRANS,R_MONTH)/ &
                                          MONTHLY_ENERGY(TRANS,R_MONTH))
            ELSE
               R_TRANS_REV_MWH = 0.
            ENDIF
            R_TRANS_HOURS = FLOAT(MONTHLY_TRANS_HOURS(TRANS,R_MONTH))
            R_PRODUCT_HOURS = &
                             FLOAT(MONTHLY_PRODUCT_HOURS(TRANS,R_MONTH))
         ENDIF
!         ENDIF
!
         R_YES_REPORT_PRODUCT = REPORT_PRODUCT(TRANS) == 'T'
!
         LOCAL_TG = TRANSACTION_GROUP(TRANS)
         get_primary_mover_index_called_from="transobj2:0028"
         R_PM = PRIMARY_MOVER_INDEX_DB(UNIT_CATEGORY(TRANS))
         R_FT = FUEL_TYPE_2_PRIM_MOVER(FUEL_TYPE(TRANS))
!
!         IF(BEGIN_EP(TRANS) > 10000) THEN
         IF(BEGIN_EP(TRANS) > 8012) THEN
            R_ON_LINE_DATE = BEGIN_EP(TRANS) + 190000.
         ELSE
            R_ON_LINE_DATE = BEGIN_EP(TRANS) + 180000.
         ENDIF
!         IF(END_EP(TRANS) > 10000) THEN
         IF(END_EP(TRANS) > 8012) THEN
            R_OFF_LINE_DATE = END_EP(TRANS) + 190000.
         ELSE
            R_OFF_LINE_DATE = END_EP(TRANS) + 180000.
         ENDIF
         R_UNIT_ID = TRANSACTION_ID(TRANS)
         R_ASSET_CLASS = ASSET_CLASS_ID(TRANS)
!      IF(EL_GRP /= -99) THEN
!

      RETURN
!

      ENTRY CALC_GRX_FIX_COST_PER_UNIT(R_TRANS_NUM, &
                                       R_YEAR, &
                                       R_MONTH, &
                                       R_CAPACITY, &
                                       R_ENERGY)

            TRANS = R_TRANS_NUM
            TRANS_FIX_EXP = &
               ESCALATED_MONTHLY_VALUE(DOLLAR_MONTH(TRANS), &
                       DOLLAR_MONTH_ESC(TRANS),R_YEAR,R_MONTH,INT(1,2))

            TRANS_FIX_EXP = TRANS_FIX_EXP + &
                         GRX_DOLLAR_KW_MONTH(TRANS) * R_CAPACITY * 1000.
!     +                                  HOURLY_QUANTITY(TRANS) * 1000.
            IF(HOURS_FOR_PRODUCT(PRODUCT_INDEX(TRANS)) > 0.) THEN
               MONTHLY_PRODUCT_DAYS(TRANS,R_MONTH) = &
                     MONTHLY_PRODUCT_HOURS(TRANS,R_MONTH)/ &
                                 HOURS_FOR_PRODUCT(PRODUCT_INDEX(TRANS))
            ELSE
               MONTHLY_PRODUCT_DAYS(TRANS,R_MONTH) = 0.
            ENDIF
!
            TRANS_FIX_EXP = TRANS_FIX_EXP + &
               ESCALATED_MONTHLY_VALUE(DOLLAR_KW_DAY(TRANS), &
                     DOLLAR_KW_DAY_ESC(TRANS),R_YEAR,R_MONTH,INT(1,2)) * &
                           R_CAPACITY * &
                             MONTHLY_PRODUCT_DAYS(TRANS,R_MONTH) * 1000.
            IF(R_MONTH == 1 .OR. BEGIN_EP(TRANS) == TEST_DATE) THEN

               TRANS_FIX_EXP = TRANS_FIX_EXP + &
                        GRX_DOLLAR_KW_YEAR(TRANS) * &
                                        R_CAPACITY * 1000.
               IF(BEGIN_EP(TRANS) == TEST_DATE) THEN
                  TRANS_FIX_EXP = TRANS_FIX_EXP + &
                     ESCALATED_MONTHLY_VALUE(DOLLAR_DEAL(TRANS), &
                         DOLLAR_DEAL_ESC(TRANS),R_YEAR,R_MONTH,INT(1,2))
               ENDIF
            ENDIF
!
            IF(DOLLAR_MWH(TRANS) < 0.) THEN
               TRANS_FIX_EXP = TRANS_FIX_EXP + &
                  ESCALATED_MONTHLY_VALUE(DOLLAR_MWH(TRANS), &
                    ABS(INT(DOLLAR_MWH(TRANS),2)), &
                                           R_YEAR,R_MONTH,INT(1,2)) * &
                                           R_ENERGY
            ELSE
               TRANS_FIX_EXP = TRANS_FIX_EXP + DOLLAR_MWH(TRANS) * &
                                               R_ENERGY
            ENDIF
            CALC_GRX_FIX_COST_PER_UNIT = TRANS_FIX_EXP
      RETURN

      ENTRY UPDATE_TRANS_FIXED_COSTS(R_YEAR)

         UPDATE_TRANS_FIXED_COSTS = .FALSE.
         IF(SAVE_NUM_PROPOSED_UNITS > 0) THEN
            UPDATE_TRANS_FIXED_COSTS = .TRUE.
            DO RI = 1, SAVE_NUM_PROPOSED_UNITS
               TRANS = PROPOSED_RESOURCE_INDEX(RI)
               GRX_DOLLAR_KW_MONTH(TRANS) = &
                  ESCALATED_MONTHLY_VALUE(DOLLAR_KW_MONTH(TRANS), &
                   DOLLAR_KW_MONTH_ESC(TRANS),R_YEAR,INT(1,2),INT(1,2))
               GRX_DOLLAR_KW_YEAR(TRANS) = &
                     ESCALATED_MONTHLY_VALUE(GRX_DOLLAR_KW_YEAR(TRANS), &
                     DOLLAR_KW_YEAR_ESC(TRANS),R_YEAR,INT(1,2),INT(1,2))
            ENDDO
         ENDIF
      RETURN
!

!
      ENTRY CALC_MONTHLY_DERIVATIVE_FIXED(R_MONTH)
!

!
!
!
! 11/12/00. FIXED COST LOGIC. NEED TO ADD $/MWH FOR THE PRODUCT
!
! 071216. ALTERED TO CORRECT FOM UNDER GRX AND FOR UNITS STARTING AFTER
!         FIRST FORECAST MONTH.
!
!         DO IND = 1, MONTHLY_ACTIVE_TRANSACTIONS
!            TRANS = ACTIVE_IN_MONTH(IND)
         DO TRANS = 1, SAVE_ACTIVE_TRANSACTIONS
! 082821
!            IF(ABS(ANNUAL_HYBRID_BATTERY(TRANS)) > 0) CYCLE
! 120121. ADDED TEST_DATE. REFERENCED BELOW.
            TEST_DATE = 100*(get_BASE_YEAR() + globecom_YEAR - 1900) + R_MONTH
            HYBRID_ACTIVE = .FALSE.
            IF(ANNUAL_HYBRID_BATTERY(TRANS) > 0) THEN
! DOUBLE INDEX.
               J = ANNUAL_HYBRID_BATTERY(TRANS)
               J = ANNUAL_HYBRID_INDEX(J)
               IF(J > 0) THEN
                  LOCAL_BEGIN_EP = MIN(BEGIN_EP(TRANS),BEGIN_EP(J))
                  LOCAL_END_EP = MAX(END_EP(TRANS),END_EP(J))
                  IF(TEST_DATE <= LOCAL_END_EP .AND. &
                                      TEST_DATE >= LOCAL_BEGIN_EP) THEN
                     HYBRID_ACTIVE = .TRUE.
                  ENDIF
               ENDIF
            ENDIF
!
            IF(GRX_ITERATIONS == 0) THEN
               DOLLAR_KW_MONTH(TRANS) = &
                  ESCALATED_MONTHLY_VALUE(DOLLAR_KW_MONTH(TRANS), &
                     DOLLAR_KW_MONTH_ESC(TRANS),globecom_YEAR,R_MONTH,INT(1,2))
               IF(R_MONTH == 1 .OR. BEGIN_EP(TRANS) == TEST_DATE) THEN
                  DOLLAR_KW_YEAR(TRANS) = &
                     ESCALATED_MONTHLY_VALUE(DOLLAR_KW_YEAR(TRANS), &
                     DOLLAR_KW_YEAR_ESC(TRANS),globecom_YEAR,R_MONTH,INT(1,2))
               ENDIF
            ENDIF
            IND = TRANS_TO_ACTIVE_MONTH(TRANS)
            IF(IND < 1 .AND. .NOT. HYBRID_ACTIVE) CYCLE
!
            TRANS_FIX_EXP = &
               ESCALATED_MONTHLY_VALUE(DOLLAR_MONTH(TRANS), &
                         DOLLAR_MONTH_ESC(TRANS),globecom_YEAR,R_MONTH,INT(1,2))
!
! 061708. CHANGED FOR KATHY.
! 080117. TOOK OUT B/C IT WAS DOUBLE CALCULATING $/KW MONTH
!

            TRANS_FIX_EXP = TRANS_FIX_EXP + &
                        DOLLAR_KW_MONTH(TRANS) * &
                                        HOURLY_QUANTITY(TRANS) * 1000.

            IF(HOURS_FOR_PRODUCT(PRODUCT_INDEX(TRANS)) > 0.) THEN
               MONTHLY_PRODUCT_DAYS(TRANS,R_MONTH) = &
                     MONTHLY_PRODUCT_HOURS(TRANS,R_MONTH)/ &
                                 HOURS_FOR_PRODUCT(PRODUCT_INDEX(TRANS))
            ELSE
               MONTHLY_PRODUCT_DAYS(TRANS,R_MONTH) = 0.
            ENDIF
!
            TRANS_FIX_EXP = TRANS_FIX_EXP + &
               ESCALATED_MONTHLY_VALUE(DOLLAR_KW_DAY(TRANS), &
                   DOLLAR_KW_DAY_ESC(TRANS),globecom_YEAR,R_MONTH,INT(1,2)) * &
                           HOURLY_QUANTITY(TRANS) * &
                             MONTHLY_PRODUCT_DAYS(TRANS,R_MONTH) * 1000.
!
! SAVED IN MONTHLY STUFF
!

            IF(R_MONTH == 1 .OR. BEGIN_EP(TRANS) == TEST_DATE) THEN
!

!

               TRANS_FIX_EXP = TRANS_FIX_EXP + &
                        DOLLAR_KW_YEAR(TRANS) * &
                                        HOURLY_QUANTITY(TRANS) * 1000.

               IF(BEGIN_EP(TRANS) == TEST_DATE) THEN
                  TRANS_FIX_EXP = TRANS_FIX_EXP + &
                     ESCALATED_MONTHLY_VALUE(DOLLAR_DEAL(TRANS), &
                          DOLLAR_DEAL_ESC(TRANS),globecom_YEAR,R_MONTH,INT(1,2))
               ENDIF
            ENDIF
!
            IF(DOLLAR_MWH(TRANS) < 0.) THEN
               TRANS_FIX_EXP = TRANS_FIX_EXP + &
                  ESCALATED_MONTHLY_VALUE(DOLLAR_MWH(TRANS), &
              ABS(INT(DOLLAR_MWH(TRANS),2)),globecom_YEAR,R_MONTH,INT(1,2)) * &
                                           MONTHLY_ENERGY(TRANS,R_MONTH)
            ELSE
               TRANS_FIX_EXP = TRANS_FIX_EXP + DOLLAR_MWH(TRANS) * &
                                           MONTHLY_ENERGY(TRANS,R_MONTH)
            ENDIF
!
! 10/07/03.
!
            IF(EXPENSE_ASSIGNMENT(TRANS) == LOCAL_ASSIGNMENT(1)) THEN
               TRANS_FIX_EXP = -TRANS_FIX_EXP
            ENDIF
!
            MONTHLY_TRANSACTION_COST(TRANS,R_MONTH) = TRANS_FIX_EXP
            MONTHLY_TRANSACTION_COST(TRANS,0) = &
                    MONTHLY_TRANSACTION_COST(TRANS,0) + &
                                MONTHLY_TRANSACTION_COST(TRANS,R_MONTH)
            FISCAL_TRANSACTION_COST(TRANS) = &
                    FISCAL_TRANSACTION_COST(TRANS) + &
                                MONTHLY_TRANSACTION_COST(TRANS,R_MONTH)
!
! 12/15/01. ASSET CLASS ASSIGNMENTS
!
            AC = ASSET_CLASS_GROUPS_INDEX(ASSET_CLASS_ID(TRANS))
!
! 10/06/03. BIG CHANGE: NOW HANDLED IN EACH ROUTINE
            ! TODO: Remove code in IF block
            IF(1 == 2) THEN
!
!
               IF(DERIVATIVE_TYPE(TRANS) < 4) THEN ! PHYSICAL
                  MONTHLY_AC_REVENUE(AC,1,R_MONTH) = &
                                MONTHLY_AC_REVENUE(AC,1,R_MONTH) + &
                                      MONTHLY_ENERGY_COST(TRANS,R_MONTH)
                  MONTHLY_AC_EXPENSE_ENERGY(AC,1,R_MONTH) = &
                               MONTHLY_AC_EXPENSE_ENERGY(AC,1,R_MONTH) + &
                                           MONTHLY_ENERGY(TRANS,R_MONTH)
                  MONTHLY_AC_REVENUE(AC,2,R_MONTH) = &
                                MONTHLY_AC_REVENUE(AC,2,R_MONTH) + &
                                 MONTHLY_TRANSACTION_COST(TRANS,R_MONTH)
!
                  MONTHLY_AC_REVENUE(AC,1,0) = &
                                MONTHLY_AC_REVENUE(AC,1,0) + &
                                      MONTHLY_ENERGY_COST(TRANS,R_MONTH)
                  MONTHLY_AC_EXPENSE_ENERGY(AC,1,0) = &
                               MONTHLY_AC_EXPENSE_ENERGY(AC,1,0) + &
                                           MONTHLY_ENERGY(TRANS,R_MONTH)
                  MONTHLY_AC_REVENUE(AC,2,0) = &
                                MONTHLY_AC_REVENUE(AC,2,0) + &
                                 MONTHLY_TRANSACTION_COST(TRANS,R_MONTH)
               ELSE
                  MONTHLY_AC_EXPENSE(AC,3,R_MONTH) = &
                                MONTHLY_AC_EXPENSE(AC,3,R_MONTH) + &
                                   MONTHLY_ENERGY_REVENUE(TRANS,R_MONTH)
!
                  MONTHLY_AC_REVENUE(AC,3,R_MONTH) = &
                                MONTHLY_AC_REVENUE(AC,3,R_MONTH) + &
                                      MONTHLY_ENERGY_COST(TRANS,R_MONTH)
                  MONTHLY_AC_EXPENSE_ENERGY(AC,3,R_MONTH) = &
                              MONTHLY_AC_EXPENSE_ENERGY(AC,3,R_MONTH) + &
                                           MONTHLY_ENERGY(TRANS,R_MONTH)
!
                  MONTHLY_AC_REVENUE(AC,4,R_MONTH) = &
                                MONTHLY_AC_REVENUE(AC,4,R_MONTH) + &
                                 MONTHLY_TRANSACTION_COST(TRANS,R_MONTH)
!
                  MONTHLY_AC_EXPENSE(AC,3,0) = &
                                MONTHLY_AC_EXPENSE(AC,3,0) + &
                                   MONTHLY_ENERGY_REVENUE(TRANS,R_MONTH)
                  MONTHLY_AC_REVENUE(AC,3,0) = &
                                MONTHLY_AC_REVENUE(AC,3,0) + &
                                      MONTHLY_ENERGY_COST(TRANS,R_MONTH)
!
                  MONTHLY_AC_EXPENSE_ENERGY(AC,3,0) = &
                              MONTHLY_AC_EXPENSE_ENERGY(AC,3,0) + &
                                           MONTHLY_ENERGY(TRANS,R_MONTH)
                  MONTHLY_AC_REVENUE(AC,4,0) = &
                                MONTHLY_AC_REVENUE(AC,4,0) + &
                                 MONTHLY_TRANSACTION_COST(TRANS,R_MONTH)
               ENDIF
            ELSE ! SELL

               IF(DERIVATIVE_TYPE(TRANS) < 4 .OR. &
                          DERIVATIVE_TYPE(TRANS) > 6) THEN
!
!
                  MONTHLY_AC_EXPENSE(AC,1,R_MONTH) = &
                                MONTHLY_AC_EXPENSE(AC,1,R_MONTH) + &
                                      MONTHLY_ENERGY_COST(TRANS,R_MONTH)
                  MONTHLY_AC_EXPENSE(AC,2,R_MONTH) = &
                                MONTHLY_AC_EXPENSE(AC,2,R_MONTH) + &
                                 MONTHLY_TRANSACTION_COST(TRANS,R_MONTH)
                  MONTHLY_AC_EXPENSE(AC,1,0) = &
                                MONTHLY_AC_EXPENSE(AC,1,0) + &
                                      MONTHLY_ENERGY_COST(TRANS,R_MONTH)
                  MONTHLY_AC_EXPENSE(AC,2,0) = &
                                MONTHLY_AC_EXPENSE(AC,2,0) + &
                                 MONTHLY_TRANSACTION_COST(TRANS,R_MONTH)
                  MONTHLY_AC_EXPENSE_ENERGY(AC,1,R_MONTH) = &
                               MONTHLY_AC_EXPENSE_ENERGY(AC,1,R_MONTH) + &
                                   MONTHLY_ENR_FOR_EXP(TRANS,R_MONTH)
                  MONTHLY_AC_EXPENSE_ENERGY(AC,1,0) = &
                                MONTHLY_AC_EXPENSE_ENERGY(AC,1,0) + &
                                   MONTHLY_ENR_FOR_EXP(TRANS,R_MONTH)
! 010122. REVENUE:
                  IF(DERIVATIVE_TYPE(TRANS) /= 9) THEN ! STORAGE/HYBRID
                     MONTHLY_AC_REVENUE(AC,1,R_MONTH) = &
                                MONTHLY_AC_REVENUE(AC,1,R_MONTH) + &
                                   MONTHLY_ENERGY_REVENUE(TRANS,R_MONTH)
!
                     MONTHLY_AC_REVENUE(AC,1,0) = &
                                MONTHLY_AC_REVENUE(AC,1,0) + &
                                   MONTHLY_ENERGY_REVENUE(TRANS,R_MONTH)
                     MONTHLY_AC_REVENUE_ENERGY(AC,1,R_MONTH) = &
                               MONTHLY_AC_REVENUE_ENERGY(AC,1,R_MONTH) + &
                                   MONTHLY_ENR_FOR_REV(TRANS,R_MONTH)
                     MONTHLY_AC_REVENUE_ENERGY(AC,1,0) = &
                                MONTHLY_AC_REVENUE_ENERGY(AC,1,0) + &
                                   MONTHLY_ENR_FOR_REV(TRANS,R_MONTH)
                  ENDIF
               ELSE
                  MONTHLY_AC_REVENUE(AC,3,R_MONTH) = &
                                MONTHLY_AC_REVENUE(AC,3,R_MONTH) + &
                                   MONTHLY_ENERGY_REVENUE(TRANS,R_MONTH)
                  MONTHLY_AC_REVENUE_ENERGY(AC,3,R_MONTH) = &
                               MONTHLY_AC_REVENUE_ENERGY(AC,3,R_MONTH) + &
                                   MONTHLY_ENR_FOR_REV(TRANS,R_MONTH)
!
                  MONTHLY_AC_EXPENSE_ENERGY(AC,3,R_MONTH) = &
                               MONTHLY_AC_EXPENSE_ENERGY(AC,3,R_MONTH) + &
                                   MONTHLY_ENR_FOR_EXP(TRANS,R_MONTH)
                  MONTHLY_AC_EXPENSE_ENERGY(AC,3,0) = &
                                MONTHLY_AC_EXPENSE_ENERGY(AC,3,0) + &
                                   MONTHLY_ENR_FOR_EXP(TRANS,R_MONTH)
!
                  MONTHLY_AC_EXPENSE(AC,3,R_MONTH) = &
                                MONTHLY_AC_EXPENSE(AC,3,R_MONTH) + &
                                      MONTHLY_ENERGY_COST(TRANS,R_MONTH)
                  MONTHLY_AC_EXPENSE(AC,4,R_MONTH) = &
                                MONTHLY_AC_EXPENSE(AC,4,R_MONTH) + &
                                 MONTHLY_TRANSACTION_COST(TRANS,R_MONTH)
!
                  MONTHLY_AC_REVENUE(AC,3,0) = &
                                MONTHLY_AC_REVENUE(AC,3,0) + &
                                   MONTHLY_ENERGY_REVENUE(TRANS,R_MONTH)
                  MONTHLY_AC_REVENUE_ENERGY(AC,3,0) = &
                                MONTHLY_AC_REVENUE_ENERGY(AC,3,0) + &
                                   MONTHLY_ENR_FOR_REV(TRANS,R_MONTH)
                  MONTHLY_AC_EXPENSE(AC,3,0) = &
                                MONTHLY_AC_EXPENSE(AC,3,0) + &
                                      MONTHLY_ENERGY_COST(TRANS,R_MONTH)
                  MONTHLY_AC_EXPENSE(AC,4,0) = &
                                MONTHLY_AC_EXPENSE(AC,4,0) + &
                                 MONTHLY_TRANSACTION_COST(TRANS,R_MONTH)
               ENDIF
            ENDIF

         ENDDO

         CALC_MONTHLY_DERIVATIVE_FIXED = TRANS_FIX_EXP
      RETURN

      ENTRY GET_P_DERIV_TRACKER_INDEX(R_TRANS)


         GET_P_DERIV_TRACKER_INDEX = &
                         WVPA_TRACKING_TYPE(WVPA_RATE_TRACKER(R_TRANS))

      RETURN

      ENTRY GET_P_DERIV_MEM_TRK_INDEX(R_TRANS)

!
         GET_P_DERIV_MEM_TRK_INDEX = &
                       WVPA_MEM_TRACKING_TYPE(WVPA_MEM_TRACKER(R_TRANS))
!
      RETURN

!
      ENTRY HOURLY_FORWARD_CONTRACT_ENERGY(R_HOUR_IN_DAY,R_DAY_IN_WEEK, &
                                           R_DAY_IN_MONTH,R_TRANS_GROUP, &
                                           R_HOUR_PRICE, &
                                           R_MONTH)
!

!
!
! COUNT ACTIVE TRANSACTIONS BY PRODUCT AND TRANSACTION GROUP
! INDEX EACH TRANSACTION BY PRODUCT AND TRANSACTION GROUP
! CREATE NEW VARIABLES IN THE MODEL TO TRACK EXPENSES AND REVENUES
! CREATE AS MANY PARALLELS TO THE MONTHLY CL TRANS UNIT REPORT
! CREATE A LINK WITH THE MONTHLY CL TRANS UNIT REPORT
! CREATE A LINK WITH ASSET ANALYST
! CREATE AN ABILITY TO DO DAILY CALLS AND PUTS
!
!
         HOURLY_FORWARD_CONTRACT_ENERGY = 0.
!
         IF(.NOT. SAVE_MONTHLY_PRODUCTS_STATUS) RETURN
!
         IF(NUM_FORWARDS(R_TRANS_GROUP) == 0) RETURN
!
         APPLY_5X16 = APPLY_ENERGY_PRODUCT(R_HOUR_IN_DAY,R_DAY_IN_WEEK, &
                                                  LOCAL_PRODUCT_TYPE(1))
         APPLY_6X16 = APPLY_ENERGY_PRODUCT(R_HOUR_IN_DAY,R_DAY_IN_WEEK, &
                                                  LOCAL_PRODUCT_TYPE(2))
         APPLY_7X24 = APPLY_ENERGY_PRODUCT(R_HOUR_IN_DAY,R_DAY_IN_WEEK, &
                                                  LOCAL_PRODUCT_TYPE(3))
         APPLY_5X8  = APPLY_ENERGY_PRODUCT(R_HOUR_IN_DAY,R_DAY_IN_WEEK, &
                                                  LOCAL_PRODUCT_TYPE(4))
         APPLY_WRAP  = APPLY_ENERGY_PRODUCT(R_HOUR_IN_DAY,R_DAY_IN_WEEK, &
                                                  LOCAL_PRODUCT_TYPE(5))
         APPLY_2X24  = APPLY_ENERGY_PRODUCT(R_HOUR_IN_DAY,R_DAY_IN_WEEK, &
                                                  LOCAL_PRODUCT_TYPE(7))
!
! ALTERED 10/20/00. GAT.
!
         LOCAL_ACTIVE_TRANSACTIONS = NUM_FORWARDS(R_TRANS_GROUP)
!
         DO TRANS = 1, LOCAL_ACTIVE_TRANSACTIONS
!         DO TRANS = 1, MONTHLY_ACTIVE_TRANSACTIONS
            IND = FORWARD_POSITION(TRANS,R_TRANS_GROUP)
            

! 082121. ADDED HYBRID CHECK.
!
            IF(R_DAY_IN_MONTH < BEGIN_DAY_IN_MONTH(IND) .OR. &
                   R_DAY_IN_MONTH > END_DAY_IN_MONTH(IND) .OR. &
                          ABS(HYBRID_BATTERY(IND)) > 0) then
                CYCLE ! 012322.
            end if

            IF(   (PRODUCT_TYPE(IND) == LOCAL_PRODUCT_TYPE(1) .AND. &
                                                        APPLY_5X16) .OR. &
                  (PRODUCT_TYPE(IND) == LOCAL_PRODUCT_TYPE(2) .AND. &
                                                        APPLY_6X16) .OR. &
                  (PRODUCT_TYPE(IND) == LOCAL_PRODUCT_TYPE(3) .AND. &
                                                        APPLY_7X24) .OR. &
                  (PRODUCT_TYPE(IND) == LOCAL_PRODUCT_TYPE(4) .AND. &
                                                        APPLY_5X8)  .OR. &
                  (PRODUCT_TYPE(IND) == LOCAL_PRODUCT_TYPE(5) .AND. &
                                                       APPLY_WRAP)  .OR. &
                     PRODUCT_TYPE(IND) == LOCAL_PRODUCT_TYPE(6)     .OR. &
                        PRODUCT_TYPE(IND) == LOCAL_PRODUCT_TYPE(8)) THEN
                        


               IF(PRODUCT_TYPE(IND) == LOCAL_PRODUCT_TYPE(6) .AND. &
                                        USER_DAY_TYPES_ID(IND) > 0) THEN
                  USER_DAY_MULT = GET_USER_HOUR_IN_DAY(R_HOUR_IN_DAY, &
                                      R_DAY_IN_WEEK, &
                                      R_MONTH, &
                                      USER_DAY_TYPES_ID(IND), &
                                      MONTHLY_CF_TRANS(IND))
               ELSEIF(PRODUCT_TYPE(IND) == LOCAL_PRODUCT_TYPE(8)) THEN
                  HOUR_IN_MONTH = 24*(R_DAY_IN_MONTH-1) + R_HOUR_IN_DAY
                  USER_DAY_MULT = GET_HOURLY_PRODUTION_DATA(IND, &
                                              R_MONTH, &
                                              HOUR_IN_MONTH, &
                                              HourlyProdPtrToCFLoc(IND))
               ELSE
                  USER_DAY_MULT = 1.0
               ENDIF
!
               MONTHLY_PRODUCT_HOURS(IND,R_MONTH) = &
                                  MONTHLY_PRODUCT_HOURS(IND,R_MONTH) + 1
               MONTHLY_TRANS_HOURS(IND,R_MONTH) = &
                                    MONTHLY_TRANS_HOURS(IND,R_MONTH) + 1
!
               MONTHLY_STRIKES(IND,R_MONTH) = 1
!
               TEMP_QUANTITY = HOURLY_QUANTITY(IND)
               DAILY_MW = 0.
!
               LONG_SHORT = 1.0
!
               IF(EXPENSE_ASSIGNMENT(IND) == LOCAL_ASSIGNMENT(2)) THEN
                  LONG_SHORT = -LONG_SHORT
               ENDIF
!
               HOUR_IN_MONTH = (R_DAY_IN_MONTH-1) * 24 + &
                                                           R_HOUR_IN_DAY
!
               IF(UNIT_CONTINGENCY(IND) /= 'N') THEN
!
                  HOUR = R_HOUR_IN_DAY
                  HOUR_IN_MONTH = (R_DAY_IN_MONTH-1) * 24 + HOUR
!
                  UNIT_AVAIL = 0.0
                  RESOURCE_MW = 0.
                  LOCAL_UNIT_ID = UNIT_CONTINGENT_LINK(IND)
                  IF(LOCAL_UNIT_ID < 0) THEN
                     ALL_CONTINGENT_MUST_BE_UP = &
                                 ABS(HOURLY_QUANTITY(IND) - &
                                    MAX_QUANTITY_OF_PRODUCT(IND)) < .001
                     TOTAL_CONTINGENT_UNITS = 1
                     TOTAL_CONTINGENT_UP_UNITS = 0
                     TOTAL_CONTINGENT_FOR_UNITS = 0
                     TOTAL_CONTINGENT_MOR_UNITS = 0
                     LOCAL_UNIT_ID = &
                           GET_VAR(FLOAT(UNIT_CONTINGENT_LINK(IND)), &
                                         TOTAL_CONTINGENT_UNITS, &
                                                 'MultiUnit Contingent')
                     DOWHILE(LOCAL_UNIT_ID > 0 .AND. &
                                           TOTAL_CONTINGENT_UNITS <= 30)
                        TEMP_UNIT_NUMBER = &
                                 GET_RESOURCE_ID_TO_UNIT(LOCAL_UNIT_ID, &
                                 NUMBER_OF_UNITS_FOR_ID) ! FIRST OCCURRENCE
!
                        TEMP_R4 = &
                             GET_BLOCK_AVAIL_4_HOUR(TEMP_UNIT_NUMBER, &
                                                         INT(1,2), &
                                                      HOUR_IN_MONTH) + &
                             GET_BLOCK_AVAIL_4_HOUR(TEMP_UNIT_NUMBER, &
                                                         INT(2,2), &
                                                      HOUR_IN_MONTH)
                        UNIT_AVAIL = UNIT_AVAIL + TEMP_R4
!
                        CALL GET_MWBLOK_FOR_UNIT( &
                                       TEMP_UNIT_NUMBER,TEMP_R1,TEMP_R2)
!
                        RESOURCE_MW = RESOURCE_MW + TEMP_R1 + TEMP_R2
                        TOTAL_CONTINGENT_UNITS = &
                                              TOTAL_CONTINGENT_UNITS + 1
!
                        OUTAGE_TYPE = &
                              GET_BLOCK_OUTAGE_4_HOUR(TEMP_UNIT_NUMBER, &
                                                         INT(1,2), &
                                                         HOUR_IN_MONTH)
!
                        IF(OUTAGE_TYPE == 1) THEN
                           TOTAL_CONTINGENT_MOR_UNITS = &
                                          TOTAL_CONTINGENT_MOR_UNITS + 1
                        ELSEIF(OUTAGE_TYPE == 2) THEN
                           TOTAL_CONTINGENT_FOR_UNITS = &
                                          TOTAL_CONTINGENT_FOR_UNITS + 1
                        ELSE
                           TOTAL_CONTINGENT_UP_UNITS = &
                                           TOTAL_CONTINGENT_UP_UNITS + 1
                        ENDIF
!
                        LOCAL_UNIT_ID = &
                              GET_VAR(FLOAT(UNIT_CONTINGENT_LINK(IND)), &
                                         TOTAL_CONTINGENT_UNITS, &
                                                 'MultiUnit Contingent')
                     ENDDO

                     TOTAL_CONTINGENT_UNITS = &
                                              TOTAL_CONTINGENT_UNITS - 1
! 090904 TESTING
                     TEST_CONTINGENCY = .TRUE.
                     IF(ALL_CONTINGENT_MUST_BE_UP .AND. &
                                TOTAL_CONTINGENT_UNITS /= &
                                         TOTAL_CONTINGENT_UP_UNITS) THEN
                        IF(UNIT_CONTINGENCY(IND) == 'U') THEN
                           TEST_CONTINGENCY = .FALSE.
                           DAILY_MW(HOUR) = 0.
                        ELSEIF(UNIT_CONTINGENCY(IND) == 'M' .AND. &
                                    TOTAL_CONTINGENT_UNITS /= &
                                      TOTAL_CONTINGENT_UP_UNITS + &
                                        TOTAL_CONTINGENT_FOR_UNITS) THEN
                           TEST_CONTINGENCY = .FALSE.
                           DAILY_MW(HOUR) = 0.
                        ELSEIF(UNIT_CONTINGENCY(IND) == 'F' .AND. &
                                    TOTAL_CONTINGENT_UNITS /= &
                                      TOTAL_CONTINGENT_UP_UNITS + &
                                        TOTAL_CONTINGENT_MOR_UNITS) THEN
                           TEST_CONTINGENCY = .FALSE.
                           DAILY_MW(HOUR) = 0.
                        ENDIF
                     ENDIF
!
                     IF(UNIT_CONTING_MARKET_SUPPLEMENT(IND) == 'T') THEN
                           CONTINGENT_CAPACITY(HOUR,IND) = &
                                 CONTINGENT_CAPACITY(HOUR,IND) + &
                                       MAX(0.0,RESOURCE_MW - UNIT_AVAIL)
                     ELSEIF(TEST_CONTINGENCY) THEN
                        IF(UNIT_CONTINGENCY(IND) == 'R' .OR. &
                                UNIT_CONTINGENCY(IND) == 'A' .OR. &
                                      UNIT_CONTINGENCY(IND) == 'B') THEN
                           IF(UNIT_CONTINGENCY(IND) == 'R') THEN
                              DAILY_MW(HOUR) = &
                                 MIN(MAX_QUANTITY_OF_PRODUCT(IND), &
                                       TEMP_QUANTITY * &
                                         (TOTAL_CONTINGENT_MOR_UNITS + &
                                            TOTAL_CONTINGENT_FOR_UNITS))
                           ELSEIF(UNIT_CONTINGENCY(IND) == 'A') THEN
                              DAILY_MW(HOUR) = &
                                 MIN(MAX_QUANTITY_OF_PRODUCT(IND), &
                                       TEMP_QUANTITY * &
                                             TOTAL_CONTINGENT_MOR_UNITS)
                           ELSEIF(UNIT_CONTINGENCY(IND) == 'B') THEN
                              DAILY_MW(HOUR) = &
                                 MIN(MAX_QUANTITY_OF_PRODUCT(IND), &
                                       TEMP_QUANTITY * &
                                             TOTAL_CONTINGENT_FOR_UNITS)
                           ENDIF
                        ELSE
                           IF( UNIT_CONTINGENCY(IND) == 'U') THEN
                              DAILY_MW(HOUR) = &
                                 MIN(MAX_QUANTITY_OF_PRODUCT(IND), &
                                       TEMP_QUANTITY * &
                                         TOTAL_CONTINGENT_UP_UNITS)
                           ELSEIF( & !  OUTAGE_TYPE /= 1 .AND.
                                      UNIT_CONTINGENCY(IND) == 'M') THEN
                              DAILY_MW(HOUR) = &
                                 MIN(MAX_QUANTITY_OF_PRODUCT(IND), &
                                       TEMP_QUANTITY * &
                                         (TOTAL_CONTINGENT_UNITS - &
                                            TOTAL_CONTINGENT_MOR_UNITS))
! 091106.
                              CONTINGENT_CAPACITY(HOUR,IND) = &
                                 MIN(MAX_QUANTITY_OF_PRODUCT(IND), &
                                       TEMP_QUANTITY * &
                                             TOTAL_CONTINGENT_MOR_UNITS)
                           ELSEIF( & !  OUTAGE_TYPE /= 2 .AND.
                                      UNIT_CONTINGENCY(IND) == 'F') THEN
                              DAILY_MW(HOUR) = &
                                 MIN(MAX_QUANTITY_OF_PRODUCT(IND), &
                                       TEMP_QUANTITY * &
                                         (TOTAL_CONTINGENT_UNITS - &
                                            TOTAL_CONTINGENT_FOR_UNITS))
! 091106.
                              CONTINGENT_CAPACITY(HOUR,IND) = &
                                 MIN(MAX_QUANTITY_OF_PRODUCT(IND), &
                                       TEMP_QUANTITY * &
                                             TOTAL_CONTINGENT_FOR_UNITS)
                           ENDIF
                        ENDIF
                     ENDIF ! TEST_CONTINGENCY
                  ELSE
                     TEMP_UNIT_NUMBER = &
                                 GET_RESOURCE_ID_TO_UNIT(LOCAL_UNIT_ID, &
                               NUMBER_OF_UNITS_FOR_ID) ! FIRST OCCURRENCE
                     UNIT_AVAIL = UNIT_AVAIL + &
                             GET_BLOCK_AVAIL_4_HOUR(TEMP_UNIT_NUMBER, &
                                                         INT(1,2), &
                                                      HOUR_IN_MONTH) + &
                             GET_BLOCK_AVAIL_4_HOUR(TEMP_UNIT_NUMBER, &
                                                         INT(2,2), &
                                                      HOUR_IN_MONTH)
                     CALL GET_MWBLOK_FOR_UNIT( &
                                       TEMP_UNIT_NUMBER,TEMP_R1,TEMP_R2)
                     RESOURCE_MW = TEMP_R1 + TEMP_R2
!
                     IF(RESOURCE_MW > 0.) THEN
                        UNIT_AVAIL = UNIT_AVAIL/RESOURCE_MW
                        IF(UNIT_AVAIL > 1.0) THEN
                           WRITE(4,*) "PROBLEM IN UNIT CONTINGENT"
                        ENDIF
!                     IF(UNIT_AVAIL > 0.) THEN
!                        UNIT_AVAIL = 1.0
                     ELSE
                        UNIT_AVAIL = 0.0
                     ENDIF
!
                     OUTAGE_TYPE = &
                           GET_BLOCK_OUTAGE_4_HOUR(TEMP_UNIT_NUMBER, &
                                                         INT(1,2), &
                                                         HOUR_IN_MONTH)
! 11/03/03. NEW CASES FOR REPLACEMENT LOGIC.
                     IF(UNIT_CONTINGENCY(IND) == 'R' .OR. &
                                UNIT_CONTINGENCY(IND) == 'A' .OR. &
                                      UNIT_CONTINGENCY(IND) == 'B') THEN
                        IF( UNIT_CONTINGENCY(IND) == 'R' .OR. &
                               ( (OUTAGE_TYPE == 1 .AND. &
                                      UNIT_CONTINGENCY(IND) == 'A') .OR. &
                                 (OUTAGE_TYPE == 2 .AND. &
                                   UNIT_CONTINGENCY(IND) == 'B') )) THEN
                           DAILY_MW(HOUR) = &
                                       TEMP_QUANTITY * (1. - UNIT_AVAIL)
                        ELSE
                           DAILY_MW(HOUR) = 0.
                        ENDIF
                     ELSE
!
! 090804 MAJOR RE-WRITE
!
                        IF( UNIT_CONTINGENCY(IND) == 'U') THEN
                           DAILY_MW(HOUR) = TEMP_QUANTITY*UNIT_AVAIL
                        ELSEIF((OUTAGE_TYPE /= 1 .AND. &
                                      UNIT_CONTINGENCY(IND) == 'M') .OR. &
                              (OUTAGE_TYPE /= 2 .AND. &
                                   UNIT_CONTINGENCY(IND) == 'F') ) THEN
!                           DAILY_MW(HOUR) = TEMP_QUANTITY*UNIT_AVAIL
                           DAILY_MW(HOUR) = TEMP_QUANTITY
                        ENDIF
                     ENDIF
                  ENDIF ! SINGLE OR MULTIPLE CONTINGENT

                  TEMP_QUANTITY = DAILY_MW(HOUR)

               ENDIF ! UNIT CONTINGENT LINK

! 052204
               TEMP_QUANTITY = TEMP_QUANTITY * &
                                              USER_DAY_MULT * LONG_SHORT

! 7/17//02. PER KATHY ANDERSON. APPEARS FINANCIALS CREDITED AGAINST LOAD
!
! 110306. FOR BURESH.
!
               IF(DERIVATIVE_TYPE(IND) == 12) THEN
                  HOURLY_FORWARD_CONTRACT_ENERGY = &
                     HOURLY_FORWARD_CONTRACT_ENERGY + &
                                                    TEMP_QUANTITY
                  TEMP_R1 = ABS(TEMP_QUANTITY)
               ELSEIF(DERIVATIVE_TYPE(IND) < 4) THEN ! PHYSICAL
                  HOURLY_FORWARD_CONTRACT_ENERGY = &
                     HOURLY_FORWARD_CONTRACT_ENERGY + &
                                                    TEMP_QUANTITY
                  TEMP_R1 = 0.
               ELSE
                  TEMP_R1 = ABS(TEMP_QUANTITY)
               ENDIF
! 03/13/01
! 10/07/03.
!
               MONTHLY_ENERGY(IND,R_MONTH) = &
                             MONTHLY_ENERGY(IND,R_MONTH) - TEMP_QUANTITY

               IF(PRICE_TYPE(IND) == 'C') THEN
                  TEMP_R4 = &
                             MIN(MONTHLY_ENERGY_PRICE(IND),R_HOUR_PRICE)
               ELSEIF(PRICE_TYPE(IND) == 'I') THEN
! 11/25/03.
!               IF(PRICE_TYPE(IND) == 'I') THEN
                  TEMP_R4 = R_HOUR_PRICE * MONTHLY_ENERGY_MULT(IND)
                  IF(HOUR_IN_MONTH == 1) THEN
                     MONTHLY_ENERGY_PRICE(IND) = TEMP_R4
                  ELSEIF(HOUR_IN_MONTH == HOURS_IN_MONTH) THEN
                     MONTHLY_ENERGY_PRICE(IND) = &
                                 (MONTHLY_ENERGY_PRICE(IND) + TEMP_R4)/ &
                                                   FLOAT(HOURS_IN_MONTH)
                  ELSE
                     MONTHLY_ENERGY_PRICE(IND) = &
                                     MONTHLY_ENERGY_PRICE(IND) + TEMP_R4
                  ENDIF
               ELSE
                  TEMP_R4 = MONTHLY_ENERGY_PRICE(IND)
               ENDIF
!
               IF(EXPENSE_ASSIGNMENT(IND) == LOCAL_ASSIGNMENT(1)) THEN
                  MONTHLY_ENERGY_REVENUE(IND,R_MONTH) = &
                                   MONTHLY_ENERGY_REVENUE(IND,R_MONTH) + &
                          TEMP_R4 * ABS(TEMP_QUANTITY)

                  MONTHLY_ENERGY_COST(IND,R_MONTH) = &
                        MONTHLY_ENERGY_COST(IND,R_MONTH) + &
                                     R_HOUR_PRICE * TEMP_R1
                  MONTHLY_ENR_FOR_REV(IND,R_MONTH) = &
                          MONTHLY_ENR_FOR_REV(IND,R_MONTH) + &
                                                      ABS(TEMP_QUANTITY)
               ELSE
                  MONTHLY_ENERGY_REVENUE(IND,R_MONTH) = &
                                   MONTHLY_ENERGY_REVENUE(IND,R_MONTH) + &
                                     R_HOUR_PRICE * TEMP_R1
                  MONTHLY_ENERGY_COST(IND,R_MONTH) = &
                        MONTHLY_ENERGY_COST(IND,R_MONTH) + &
                          TEMP_R4 * ABS(TEMP_QUANTITY)

                  MONTHLY_ENR_FOR_EXP(IND,R_MONTH) = &
                          MONTHLY_ENR_FOR_EXP(IND,R_MONTH) + &
                                                      ABS(TEMP_QUANTITY)
               ENDIF
!
! BUY/SELL POSITION HANDLED ON MONTHLY BASIS
            ENDIF ! VALID PRODUCT FOR THE HOUR
         ENDDO
      RETURN

!
      ENTRY DAILY_CALL_PUT_CAPACITY(R_DAY_IN_WEEK, &
                                    R_DAY_IN_MONTH, &
                                    R_TRANS_GROUP, &
                                    R_DAILY_PRICE, &
                                    R_DAILY_CAPACITY, &
                                    R_MONTH, &
                                    R_MONTHLY_AVAIL)
!

!
!
! COUNT ACTIVE TRANSACTIONS BY PRODUCT AND TRANSACTION GROUP
! INDEX EACH TRANSACTION BY PRODUCT AND TRANSACTION GROUP
! CREATE NEW VARIABLES IN THE MODEL TO TRACK EXPENSES AND REVENUES
! CREATE AS MANY PARALLELS TO THE MONTHLY CL TRANS UNIT REPORT
! CREATE A LINK WITH THE MONTHLY CL TRANS UNIT REPORT
! CREATE A LINK WITH ASSET ANALYST
! CREATE AN ABILITY TO DO DAILY CALLS AND PUTS
!
!
         DAILY_CALL_PUT_CAPACITY = .FALSE.
!
!
         R_DAILY_CAPACITY = 0.
!
!
         IF(.NOT. SAVE_MONTHLY_PRODUCTS_STATUS) RETURN
!
!
!         IF(MONTHLY_ACTIVE_TRANSACTIONS == 0) RETURN
         CONTINGENT_CAPACITY = 0.
         IF(NUM_CALLS(R_TRANS_GROUP) + &
                                    NUM_PUTS(R_TRANS_GROUP) <= 0) RETURN
!
         APPLY_5X16 = APPLY_ENERGY_PRODUCT(INT(12,2),R_DAY_IN_WEEK, &
                                                  LOCAL_PRODUCT_TYPE(1))
         APPLY_6X16 = APPLY_ENERGY_PRODUCT(INT(12,2),R_DAY_IN_WEEK, &
                                                  LOCAL_PRODUCT_TYPE(2))
         APPLY_7X24 = APPLY_ENERGY_PRODUCT(INT(12,2),R_DAY_IN_WEEK, &
                                                  LOCAL_PRODUCT_TYPE(3))
         APPLY_5X8  = APPLY_ENERGY_PRODUCT(INT(1,2), R_DAY_IN_WEEK, &
                                                  LOCAL_PRODUCT_TYPE(4))
         APPLY_WRAP  = APPLY_ENERGY_PRODUCT(INT(1,2),R_DAY_IN_WEEK, &
                                                  LOCAL_PRODUCT_TYPE(5))
         APPLY_2X24  = APPLY_ENERGY_PRODUCT(INT(1,2),R_DAY_IN_WEEK, &
                                                  LOCAL_PRODUCT_TYPE(7))
!
! DAILY CALL LOGIC
!
!
!
         DO I = 1, 2
            N_BEST_VALUES=0
            IF(I == 1) THEN ! CALLS
               LOCAL_ACTIVE_TRANSACTIONS = NUM_CALLS(R_TRANS_GROUP)
               CALL_OR_PUT = 1.
            ELSE ! PUTS
               LOCAL_ACTIVE_TRANSACTIONS = NUM_PUTS(R_TRANS_GROUP)
               CALL_OR_PUT = -1.
            ENDIF
            DO TRANS = 1, LOCAL_ACTIVE_TRANSACTIONS
               IF(I == 1) THEN
                  IND = CALL_POSITION(TRANS,R_TRANS_GROUP)
               ELSE
                  IND = PUT_POSITION(TRANS,R_TRANS_GROUP)
               ENDIF
!
               IF(PRICE_TYPE(IND) == 'I') THEN
                  MONTHLY_ENERGY_PRICE(IND) = 0.
                  DO J = 1, 24
                     HOURLY_ENERGY_PRICE(J) = R_DAILY_PRICE(J) * &
                                              MONTHLY_ENERGY_MULT(IND)
                     MONTHLY_ENERGY_PRICE(IND) = &
                                    MONTHLY_ENERGY_PRICE(IND) + &
                                             HOURLY_ENERGY_PRICE(J)
                     IF(J == 24) THEN
                        MONTHLY_ENERGY_PRICE(IND) = &
                                    MONTHLY_ENERGY_PRICE(IND)/24.
                     ENDIF
                  ENDDO
               ELSE
                  HOURLY_ENERGY_PRICE = MONTHLY_ENERGY_PRICE(IND)
               ENDIF
!
               IF(R_DAY_IN_MONTH < BEGIN_DAY_IN_MONTH(IND) .OR. &
                         R_DAY_IN_MONTH > END_DAY_IN_MONTH(IND)) CYCLE
!
               IF( PRODUCT_TYPE(IND) == LOCAL_PRODUCT_TYPE(1) .AND. &
                                                        APPLY_5X16) THEN
                  PRODUCT = 1
!                  HOURS_FOR_PRODUCT = 16
               ELSEIF(PRODUCT_TYPE(IND) == LOCAL_PRODUCT_TYPE(2) .AND. &
                                                        APPLY_6X16) THEN
                  PRODUCT = 2
!                  HOURS_FOR_PRODUCT = 16
               ELSEIF(PRODUCT_TYPE(IND) == LOCAL_PRODUCT_TYPE(3) .AND. &
                                                        APPLY_7X24) THEN
                  PRODUCT = 3
!                  HOURS_FOR_PRODUCT = 24
               ELSEIF(PRODUCT_TYPE(IND) == LOCAL_PRODUCT_TYPE(4) .AND. &
                                                        APPLY_5X8) THEN
                  PRODUCT = 4
!                  HOURS_FOR_PRODUCT = 8
               ELSEIF(PRODUCT_TYPE(IND) == LOCAL_PRODUCT_TYPE(5) .AND. &
                                                        APPLY_WRAP) THEN
                  IF(APPLY_2X24) THEN
                     PRODUCT = 7
                  ELSE
                     PRODUCT = 5
                  ENDIF
               ELSEIF(PRODUCT_TYPE(IND) == LOCAL_PRODUCT_TYPE(6)) THEN
                  PRODUCT = 6
                  month_pass=r_month
                  gud_called_from="read_energy_products_data"
                  HOURS_FOR_PRODUCT(PRODUCT) = &
                                 GET_USER_DAY(R_DAY_IN_WEEK, &
                                        R_MONTH, &
                                        USER_DAY_TYPES_ID(IND), &
                                        PRODUCT_PATTERN(1,PRODUCT), &
                                        MONTHLY_CF_TRANS(IND))
                  r_month=month_pass
               ELSE
                  CYCLE
               ENDIF
!
!
!
               MONTHLY_PRODUCT_HOURS(IND,R_MONTH) = &
                        MONTHLY_PRODUCT_HOURS(IND,R_MONTH) + &
                                       INT(HOURS_FOR_PRODUCT(PRODUCT),2)
!
               IF(STRIKES_AVAILABLE(IND) <= 0) CYCLE

               TEMP_QUANTITY = HOURLY_QUANTITY(IND)
               DAILY_MW = TEMP_QUANTITY
!
               LONG_SHORT = 1.0
!
               IF(EXPENSE_ASSIGNMENT(IND) == LOCAL_ASSIGNMENT(1)) THEN
                  LONG_SHORT = -LONG_SHORT
               ENDIF
!
               IF(UNIT_CONTINGENCY(IND) /= 'N') THEN
                  DO HOUR = 1, 24
                     HOUR_IN_MONTH = (R_DAY_IN_MONTH-1) * 24 + HOUR
!
                     UNIT_AVAIL = 0.0
                     RESOURCE_MW = 0.
                     LOCAL_UNIT_ID = UNIT_CONTINGENT_LINK(IND)
                     IF(LOCAL_UNIT_ID < 0) THEN
                        ALL_CONTINGENT_MUST_BE_UP = &
                                 ABS(HOURLY_QUANTITY(IND) - &
                                    MAX_QUANTITY_OF_PRODUCT(IND)) < .001
                        TOTAL_CONTINGENT_UNITS = 1
                        TOTAL_CONTINGENT_UP_UNITS = 0
                        TOTAL_CONTINGENT_FOR_UNITS = 0
                        TOTAL_CONTINGENT_MOR_UNITS = 0
                        LOCAL_UNIT_ID = &
                           GET_VAR(FLOAT(UNIT_CONTINGENT_LINK(IND)), &
                                         TOTAL_CONTINGENT_UNITS, &
                                                 'MultiUnit Contingent')
                        DOWHILE(LOCAL_UNIT_ID > 0 .AND. &
                                           TOTAL_CONTINGENT_UNITS <= 30)
                           TEMP_UNIT_NUMBER = &
                                 GET_RESOURCE_ID_TO_UNIT(LOCAL_UNIT_ID, &
                                                 NUMBER_OF_UNITS_FOR_ID)

!
                           TEMP_R4 = &
                             GET_BLOCK_AVAIL_4_HOUR(TEMP_UNIT_NUMBER, &
                                                         INT(1,2), &
                                                      HOUR_IN_MONTH) + &
                             GET_BLOCK_AVAIL_4_HOUR(TEMP_UNIT_NUMBER, &
                                                         INT(2,2), &
                                                      HOUR_IN_MONTH)
                           UNIT_AVAIL = UNIT_AVAIL + TEMP_R4
!
                           CALL GET_MWBLOK_FOR_UNIT( &
                                       TEMP_UNIT_NUMBER,TEMP_R1,TEMP_R2)
!
                           RESOURCE_MW = RESOURCE_MW + TEMP_R1 + TEMP_R2
                           TOTAL_CONTINGENT_UNITS = &
                                              TOTAL_CONTINGENT_UNITS + 1
!
                           OUTAGE_TYPE = &
                              GET_BLOCK_OUTAGE_4_HOUR(TEMP_UNIT_NUMBER, &
                                                         INT(1,2), &
                                                         HOUR_IN_MONTH)
!
                           IF(OUTAGE_TYPE == 1) THEN
                              TOTAL_CONTINGENT_MOR_UNITS = &
                                          TOTAL_CONTINGENT_MOR_UNITS + 1
                           ELSEIF(OUTAGE_TYPE == 2) THEN
                              TOTAL_CONTINGENT_FOR_UNITS = &
                                          TOTAL_CONTINGENT_FOR_UNITS + 1
                           ELSE
                              TOTAL_CONTINGENT_UP_UNITS = &
                                           TOTAL_CONTINGENT_UP_UNITS + 1
                           ENDIF
!
                           LOCAL_UNIT_ID = &
                              GET_VAR(FLOAT(UNIT_CONTINGENT_LINK(IND)), &
                                         TOTAL_CONTINGENT_UNITS, &
                                                 'MultiUnit Contingent')
                        ENDDO
! 11/25/03. DETERMINE TEMP_QUANTITY AND UNIT_AVAIL FOR THE MULTI
! CONTINGENT CASE
!
                        TOTAL_CONTINGENT_UNITS = &
                                              TOTAL_CONTINGENT_UNITS - 1
! 090904 TESTING
                        TEST_CONTINGENCY = .TRUE.
                        IF(ALL_CONTINGENT_MUST_BE_UP .AND. &
                                TOTAL_CONTINGENT_UNITS /= &
                                         TOTAL_CONTINGENT_UP_UNITS) THEN
                           IF(UNIT_CONTINGENCY(IND) == 'U') THEN
                              TEST_CONTINGENCY = .FALSE.
                              DAILY_MW(HOUR) = 0.
                           ELSEIF(UNIT_CONTINGENCY(IND) == 'M' .AND. &
                                    TOTAL_CONTINGENT_UNITS /= &
                                      TOTAL_CONTINGENT_UP_UNITS + &
                                        TOTAL_CONTINGENT_FOR_UNITS) THEN
                              TEST_CONTINGENCY = .FALSE.
                              DAILY_MW(HOUR) = 0.
                           ELSEIF(UNIT_CONTINGENCY(IND) == 'F' .AND. &
                                    TOTAL_CONTINGENT_UNITS /= &
                                      TOTAL_CONTINGENT_UP_UNITS + &
                                        TOTAL_CONTINGENT_MOR_UNITS) THEN
                              TEST_CONTINGENCY = .FALSE.
                              DAILY_MW(HOUR) = 0.
                           ENDIF
                        ENDIF
!
                        IF(UNIT_CONTING_MARKET_SUPPLEMENT(IND) == &
                                                               'T') THEN

                              CONTINGENT_CAPACITY(HOUR,IND) = &
                                 CONTINGENT_CAPACITY(HOUR,IND) + &
                                       MAX(0.0,RESOURCE_MW - UNIT_AVAIL)

                        ELSEIF(TEST_CONTINGENCY) THEN

                           IF(UNIT_CONTINGENCY(IND) == 'R' .OR. &
                                UNIT_CONTINGENCY(IND) == 'A' .OR. &
                                      UNIT_CONTINGENCY(IND) == 'B') THEN
                              IF(UNIT_CONTINGENCY(IND) == 'R') THEN
                                 DAILY_MW(HOUR) = &
                                    MIN(MAX_QUANTITY_OF_PRODUCT(IND), &
                                       TEMP_QUANTITY * &
                                         (TOTAL_CONTINGENT_MOR_UNITS + &
                                            TOTAL_CONTINGENT_FOR_UNITS))
                              ELSEIF(UNIT_CONTINGENCY(IND) == 'A') THEN
                                 DAILY_MW(HOUR) = &
                                    MIN(MAX_QUANTITY_OF_PRODUCT(IND), &
                                       TEMP_QUANTITY * &
                                             TOTAL_CONTINGENT_MOR_UNITS)
                              ELSEIF(UNIT_CONTINGENCY(IND) == 'B') THEN
                                 DAILY_MW(HOUR) = &
                                    MIN(MAX_QUANTITY_OF_PRODUCT(IND), &
                                       TEMP_QUANTITY * &
                                             TOTAL_CONTINGENT_FOR_UNITS)
                              ENDIF
                           ELSE
!
! 083006. ALTERED CODE FOR CONTINGENCIES.
!
                              IF( UNIT_CONTINGENCY(IND) == 'U') THEN
                                 DAILY_MW(HOUR) = &
                                    MIN(MAX_QUANTITY_OF_PRODUCT(IND), &
                                       TEMP_QUANTITY * &
                                         TOTAL_CONTINGENT_UP_UNITS)
                              ELSEIF( & !  OUTAGE_TYPE /= 1 .AND.
                                      UNIT_CONTINGENCY(IND) == 'M') THEN
                                 DAILY_MW(HOUR) = &
                                    MIN(MAX_QUANTITY_OF_PRODUCT(IND), &
                                       TEMP_QUANTITY * &
                                        (TOTAL_CONTINGENT_UNITS - &
                                            TOTAL_CONTINGENT_MOR_UNITS))
! 091106.
                                 CONTINGENT_CAPACITY(HOUR,IND) = &
                                    MIN(MAX_QUANTITY_OF_PRODUCT(IND), &
                                       TEMP_QUANTITY * &
                                             TOTAL_CONTINGENT_MOR_UNITS)
                              ELSEIF(  & !  OUTAGE_TYPE /= 2 .AND.
                                      UNIT_CONTINGENCY(IND) == 'F') THEN
                                 DAILY_MW(HOUR) = &
                                    MIN(MAX_QUANTITY_OF_PRODUCT(IND), &
                                       TEMP_QUANTITY * &
                                        (TOTAL_CONTINGENT_UNITS - &
                                            TOTAL_CONTINGENT_FOR_UNITS))
! 091106.
                                 CONTINGENT_CAPACITY(HOUR,IND) = &
                                    MIN(MAX_QUANTITY_OF_PRODUCT(IND), &
                                       TEMP_QUANTITY * &
                                             TOTAL_CONTINGENT_FOR_UNITS)
                              ENDIF
                           ENDIF
                        ENDIF ! TEST CONTINGENCY
                     ELSE
                        TEMP_UNIT_NUMBER = &
                                 GET_RESOURCE_ID_TO_UNIT(LOCAL_UNIT_ID, &
                           NUMBER_OF_UNITS_FOR_ID) ! FIRST OCCURRENCE
                        UNIT_AVAIL = UNIT_AVAIL + &
                             GET_BLOCK_AVAIL_4_HOUR(TEMP_UNIT_NUMBER, &
                                                         INT(1,2), &
                                                      HOUR_IN_MONTH) + &
                             GET_BLOCK_AVAIL_4_HOUR(TEMP_UNIT_NUMBER, &
                                                         INT(2,2), &
                                                      HOUR_IN_MONTH)
                        CALL GET_MWBLOK_FOR_UNIT( &
                                       TEMP_UNIT_NUMBER,TEMP_R1,TEMP_R2)
                        RESOURCE_MW = TEMP_R1 + TEMP_R2
!
                        IF(RESOURCE_MW > 0.) THEN
                           UNIT_AVAIL = UNIT_AVAIL/RESOURCE_MW
                           IF(UNIT_AVAIL > 1.0) THEN
                              WRITE(4,*) "PROBLEM IN UNIT CONTINGENT"
                           ENDIF
!                     IF(UNIT_AVAIL > 0.) THEN
!                        UNIT_AVAIL = 1.0
                        ELSE
                           UNIT_AVAIL = 0.0
                        ENDIF
!
                        OUTAGE_TYPE = &
                           GET_BLOCK_OUTAGE_4_HOUR(TEMP_UNIT_NUMBER, &
                                                         INT(1,2), &
                                                         HOUR_IN_MONTH)
! 11/03/03. NEW CASES FOR REPLACEMENT LOGIC.
                        IF(UNIT_CONTINGENCY(IND) == 'R' .OR. &
                                UNIT_CONTINGENCY(IND) == 'A' .OR. &
                                      UNIT_CONTINGENCY(IND) == 'B') THEN
                           IF( UNIT_CONTINGENCY(IND) == 'R' .OR. &
                               ( (OUTAGE_TYPE == 1 .AND. &
                                      UNIT_CONTINGENCY(IND) == 'A') .OR. &
                                 (OUTAGE_TYPE == 2 .AND. &
                                   UNIT_CONTINGENCY(IND) == 'B') )) THEN
                              DAILY_MW(HOUR) = &
                                       TEMP_QUANTITY * (1. - UNIT_AVAIL)
                           ELSE
                              DAILY_MW(HOUR) = 0.
                           ENDIF
                        ELSE
!
! 090904 MAJOR RE-WRITE.
!
                           IF( UNIT_CONTINGENCY(IND) == 'U') THEN
                              DAILY_MW(HOUR) = TEMP_QUANTITY*UNIT_AVAIL
                           ELSEIF( (OUTAGE_TYPE == 1 .AND. &
                                      UNIT_CONTINGENCY(IND) == 'M') .OR. &
                                    (OUTAGE_TYPE == 2 .AND. &
                                    UNIT_CONTINGENCY(IND) == 'F') ) THEN
                              DAILY_MW(HOUR) = TEMP_QUANTITY*UNIT_AVAIL
                           ENDIF
                        ENDIF
                     ENDIF ! SINGLE OR MULTIPLE CONTINGENT
!
                     R_MONTHLY_AVAIL(HOUR_IN_MONTH) = &
                                       R_MONTHLY_AVAIL(HOUR_IN_MONTH) + &
                                             DAILY_MW(HOUR) * LONG_SHORT
                  ENDDO ! HOUR
               ELSE
                  DO HOUR = 1, 24
                     HOUR_IN_MONTH = (R_DAY_IN_MONTH-1) * 24 + HOUR
                     R_MONTHLY_AVAIL(HOUR_IN_MONTH+HOUR-1) = &
                                R_MONTHLY_AVAIL(HOUR_IN_MONTH+HOUR-1) + &
                                             DAILY_MW(HOUR) * LONG_SHORT
                  ENDDO
               ENDIF ! CONTINGENT OPTION
!
               IF(STRIKE_FREQUENCY(IND) == 'H') THEN ! ASSUMES HOURLY
                  DAILY_OPTION_VALUE = &
                              GET_HOURLY_OPTION_VALUE( &
                                             R_DAILY_PRICE, &
                                             DAILY_MW, &
                                             PRODUCT_PATTERN(1,PRODUCT), &
                                             CALL_OR_PUT, &
                                             HOURLY_ENERGY_PRICE, &
                                             TEMP_STRIKES)
              call write_trace_real4(file_trace_cxat, "1. DOV", &
                DAILY_OPTION_VALUE)
                
                  TEMP_HOURS = TEMP_STRIKES
!
               ELSE ! ASSUMES DAILY
                  DAILY_OPTION_VALUE = &
                              GET_DAILY_OPTION_VALUE( &
                                             R_DAILY_PRICE, &
                                             DAILY_MW, &
                                             PRODUCT_PATTERN(1,PRODUCT), &
                                             CALL_OR_PUT, &
                                             HOURLY_ENERGY_PRICE)
              call write_trace_real4(file_trace_cxat, "2. DOV", &
                DAILY_OPTION_VALUE)

                  TEMP_STRIKES = 1
                  TEMP_HOURS = INT(HOURS_FOR_PRODUCT(PRODUCT),2)
               ENDIF

! IF VALUE NON-POSITIVE, ITS NO GOOD.  CYCLE.
               IF(DAILY_OPTION_VALUE <= 0.) then
                call write_trace_message(file_trace_cxat, "1. Cycle")
                CYCLE
               end if

               DAILY_CALL_PUT_CAPACITY = .TRUE.
               SELL_OR_BUY = 1.
               LONG_SHORT = 1.0

! ITS GOOD.  ADD IT TO R_DAILY_CAPACITY.
!
               MONTHLY_TRANS_HOURS(IND,R_MONTH) = &
                                 MONTHLY_TRANS_HOURS(IND,R_MONTH) + &
                                                              TEMP_HOURS
!     +                                  INT2(HOURS_FOR_PRODUCT(PRODUCT))
               MONTHLY_STRIKES(IND,R_MONTH) = &
                             MONTHLY_STRIKES(IND,R_MONTH) + TEMP_STRIKES
!    +                                  MONTHLY_STRIKES(IND,R_MONTH) + 1
!
               STRIKES_AVAILABLE(IND) = STRIKES_AVAILABLE(IND) - &
                                                            TEMP_STRIKES
! BUY/SELL POSITION ACCUMULATED MONTHLY.
               SELL_OR_BUY = 1.0
!
! 12/21/01. IF SELLING A DERIVATIVE PRODUCT, THEN THE BUYER IS PAYING
! FOR THE OPTIONALITY.
! FOR INSTANCE, SELLING A CALL OPTION MEANS THAT WHEN IT IS CALLED,
! YOU MUST SERVE THE LOAD.


! 5/4/02. ALTERED DEFINITION BECAUSE ENERGY LOOKED WRONG FOR USER
! DAY TYPES
               DAILY_MW(0) = &
                       DAILY_MW(1) * PRODUCT_PATTERN(1,PRODUCT) + &
                       DAILY_MW(2) * PRODUCT_PATTERN(2,PRODUCT) + &
                       DAILY_MW(3) * PRODUCT_PATTERN(3,PRODUCT) + &
                       DAILY_MW(4) * PRODUCT_PATTERN(4,PRODUCT) + &
                       DAILY_MW(5) * PRODUCT_PATTERN(5,PRODUCT) + &
                       DAILY_MW(6) * PRODUCT_PATTERN(6,PRODUCT) + &
                       DAILY_MW(7) * PRODUCT_PATTERN(7,PRODUCT) + &
                       DAILY_MW(8) * PRODUCT_PATTERN(8,PRODUCT) + &
                       DAILY_MW(9) * PRODUCT_PATTERN(9,PRODUCT) + &
                       DAILY_MW(10) * PRODUCT_PATTERN(10,PRODUCT) + &
                       DAILY_MW(11) * PRODUCT_PATTERN(11,PRODUCT) + &
                       DAILY_MW(12) * PRODUCT_PATTERN(12,PRODUCT) + &
                       DAILY_MW(13) * PRODUCT_PATTERN(13,PRODUCT) + &
                       DAILY_MW(14) * PRODUCT_PATTERN(14,PRODUCT) + &
                       DAILY_MW(15) * PRODUCT_PATTERN(15,PRODUCT) + &
                       DAILY_MW(16) * PRODUCT_PATTERN(16,PRODUCT) + &
                       DAILY_MW(17) * PRODUCT_PATTERN(17,PRODUCT) + &
                       DAILY_MW(18) * PRODUCT_PATTERN(18,PRODUCT) + &
                       DAILY_MW(19) * PRODUCT_PATTERN(19,PRODUCT) + &
                       DAILY_MW(20) * PRODUCT_PATTERN(20,PRODUCT) + &
                       DAILY_MW(21) * PRODUCT_PATTERN(21,PRODUCT) + &
                       DAILY_MW(22) * PRODUCT_PATTERN(22,PRODUCT) + &
                       DAILY_MW(23) * PRODUCT_PATTERN(23,PRODUCT) + &
                       DAILY_MW(24) * PRODUCT_PATTERN(24,PRODUCT)
!
! 4/19/02. DON'T KNOW WHY THIS WAS TURNED-OFF.
!
               IF(EXPENSE_ASSIGNMENT(IND) == LOCAL_ASSIGNMENT(1)) THEN
                  LONG_SHORT = -LONG_SHORT
               ENDIF
!
! 10/06/03.
!
               LONG_SHORT = LONG_SHORT*CALL_OR_PUT
!
               MONTHLY_ENERGY(IND,R_MONTH) = &
                   MONTHLY_ENERGY(IND,R_MONTH) + DAILY_MW(0)*LONG_SHORT

               TEMP_PRICE_24 = 0.
               IF(DERIVATIVE_TYPE(IND) < 4 .OR. &
                                      DERIVATIVE_TYPE(IND) == 12) THEN ! PHYSICAL
                  R_DAILY_CAPACITY(1) = &
                        R_DAILY_CAPACITY(1) + LONG_SHORT* &
                       DAILY_MW(1) * PRODUCT_PATTERN(1,PRODUCT)
                  R_DAILY_CAPACITY(2) = &
                        R_DAILY_CAPACITY(2) + LONG_SHORT* &
                       DAILY_MW(2) * PRODUCT_PATTERN(2,PRODUCT)
                  R_DAILY_CAPACITY(3) = &
                        R_DAILY_CAPACITY(3) + LONG_SHORT* &
                       DAILY_MW(3) * PRODUCT_PATTERN(3,PRODUCT)
                  R_DAILY_CAPACITY(4) = &
                        R_DAILY_CAPACITY(4) + LONG_SHORT* &
                       DAILY_MW(4) * PRODUCT_PATTERN(4,PRODUCT)
                  R_DAILY_CAPACITY(5) = &
                        R_DAILY_CAPACITY(5) + LONG_SHORT* &
                       DAILY_MW(5) * PRODUCT_PATTERN(5,PRODUCT)
                  R_DAILY_CAPACITY(6) = &
                        R_DAILY_CAPACITY(6) + LONG_SHORT* &
                       DAILY_MW(6) * PRODUCT_PATTERN(6,PRODUCT)
                  R_DAILY_CAPACITY(7) = &
                        R_DAILY_CAPACITY(7) + LONG_SHORT* &
                       DAILY_MW(7) * PRODUCT_PATTERN(7,PRODUCT)
                  R_DAILY_CAPACITY(8) = &
                        R_DAILY_CAPACITY(8) + LONG_SHORT* &
                       DAILY_MW(8) * PRODUCT_PATTERN(8,PRODUCT)
                  R_DAILY_CAPACITY(9) = &
                        R_DAILY_CAPACITY(9) + LONG_SHORT* &
                       DAILY_MW(9) * PRODUCT_PATTERN(9,PRODUCT)
                  R_DAILY_CAPACITY(10) = &
                        R_DAILY_CAPACITY(10) +LONG_SHORT* &
                      DAILY_MW(10) * PRODUCT_PATTERN(10,PRODUCT)
                  R_DAILY_CAPACITY(11) = &
                        R_DAILY_CAPACITY(11) +LONG_SHORT* &
                      DAILY_MW(11) * PRODUCT_PATTERN(11,PRODUCT)
                  R_DAILY_CAPACITY(12) = &
                        R_DAILY_CAPACITY(12) +LONG_SHORT* &
                      DAILY_MW(12) * PRODUCT_PATTERN(12,PRODUCT)
                  R_DAILY_CAPACITY(13) = &
                        R_DAILY_CAPACITY(13) +LONG_SHORT* &
                      DAILY_MW(13) * PRODUCT_PATTERN(13,PRODUCT)
                  R_DAILY_CAPACITY(14) = &
                        R_DAILY_CAPACITY(14) +LONG_SHORT* &
                      DAILY_MW(14) * PRODUCT_PATTERN(14,PRODUCT)
                  R_DAILY_CAPACITY(15) = &
                        R_DAILY_CAPACITY(15) +LONG_SHORT* &
                      DAILY_MW(15) * PRODUCT_PATTERN(15,PRODUCT)
                  R_DAILY_CAPACITY(16) = &
                        R_DAILY_CAPACITY(16) +LONG_SHORT* &
                      DAILY_MW(16) * PRODUCT_PATTERN(16,PRODUCT)
                  R_DAILY_CAPACITY(17) = &
                        R_DAILY_CAPACITY(17) +LONG_SHORT* &
                      DAILY_MW(17) * PRODUCT_PATTERN(17,PRODUCT)
                  R_DAILY_CAPACITY(18) = &
                        R_DAILY_CAPACITY(18) +LONG_SHORT* &
                      DAILY_MW(18) * PRODUCT_PATTERN(18,PRODUCT)
                  R_DAILY_CAPACITY(19) = &
                        R_DAILY_CAPACITY(19) +LONG_SHORT* &
                      DAILY_MW(19) * PRODUCT_PATTERN(19,PRODUCT)
                  R_DAILY_CAPACITY(20) = &
                        R_DAILY_CAPACITY(20) +LONG_SHORT* &
                      DAILY_MW(20) * PRODUCT_PATTERN(20,PRODUCT)
                  R_DAILY_CAPACITY(21) = &
                        R_DAILY_CAPACITY(21) +LONG_SHORT* &
                      DAILY_MW(21) * PRODUCT_PATTERN(21,PRODUCT)
                  R_DAILY_CAPACITY(22) = &
                        R_DAILY_CAPACITY(22) +LONG_SHORT* &
                      DAILY_MW(22) * PRODUCT_PATTERN(22,PRODUCT)
                  R_DAILY_CAPACITY(23) = &
                        R_DAILY_CAPACITY(23) +LONG_SHORT* &
                      DAILY_MW(23) * PRODUCT_PATTERN(23,PRODUCT)
                  R_DAILY_CAPACITY(24) = &
                        R_DAILY_CAPACITY(24) +LONG_SHORT* &
                      DAILY_MW(24) * PRODUCT_PATTERN(24,PRODUCT)
!
                  DAILY_OPTION_REVENUE = 0.
               ENDIF
               IF(DERIVATIVE_TYPE(IND) > 3) THEN
!
                  SELL_OR_BUY = 1.0
!
                  IF(STRIKE_FREQUENCY(IND) == 'H') THEN ! ASSUMES HOURLY
                     DAILY_OPTION_REVENUE = &
                              GET_HOURLY_OPTION_VALUE( &
                                             R_DAILY_PRICE, &
                                             DAILY_MW, &
                                             PRODUCT_PATTERN(1,PRODUCT), &
                                             SELL_OR_BUY, &
                                             TEMP_PRICE_24, &
                                             TEMP_STRIKES)
!
                  ELSE ! ASSUMES DAILY
                     DAILY_OPTION_REVENUE = &
                              GET_DAILY_OPTION_VALUE( &
                                             R_DAILY_PRICE, &
                                             DAILY_MW, &
                                             PRODUCT_PATTERN(1,PRODUCT), &
                                             SELL_OR_BUY, &
                                             TEMP_PRICE_24)
                  ENDIF
               ENDIF
! LONG_SHORT AND CALL_PUT ALREADY CALCULATED
!

               IF(LONG_SHORT > 0.) THEN
                  MONTHLY_ENERGY_COST(IND,R_MONTH) = &
                     MONTHLY_ENERGY_COST(IND,R_MONTH) + &
                                 MONTHLY_ENERGY_PRICE(IND) * DAILY_MW(0)
                  MONTHLY_ENERGY_REVENUE(IND,R_MONTH) = &
                      MONTHLY_ENERGY_REVENUE(IND,R_MONTH) + &
                                                    DAILY_OPTION_REVENUE
                  MONTHLY_ENR_FOR_EXP(IND,R_MONTH) = &
                          MONTHLY_ENR_FOR_EXP(IND,R_MONTH) + DAILY_MW(0)
               ELSE
                  MONTHLY_ENERGY_COST(IND,R_MONTH) = &
                     MONTHLY_ENERGY_COST(IND,R_MONTH) + &
                                                    DAILY_OPTION_REVENUE
                  MONTHLY_ENERGY_REVENUE(IND,R_MONTH) = &
                      MONTHLY_ENERGY_REVENUE(IND,R_MONTH) + &
                                 MONTHLY_ENERGY_PRICE(IND) * DAILY_MW(0)
                  MONTHLY_ENR_FOR_REV(IND,R_MONTH) = &
                          MONTHLY_ENR_FOR_REV(IND,R_MONTH) + DAILY_MW(0)
               ENDIF
!
            ENDDO ! TRANSACTIONS

            STRIKES_LISTED(I)=N_BEST_VALUES
         ENDDO ! CALL OR PUT
      RETURN ! ENTRY DAILY_CALL_PUT_CAPACITY

!
!
! 041615. HOW MANY CAN I ELIMINATE? HOW MANY DO I NEED TO ADD?
!
      ENTRY CX_ANNUAL_CONTRACT(     R_TRANS, &
                                    R_ANNUAL_PRICE, &
                                    R_ANNUAL_HOURS, &
                                    R_ANNUAL_STRIKES, &
                                    R_ANNUAL_ENERGY, &
                                    R_ANNUAL_ENERGY_REVENUE, &
                                    R_ANNUAL_VARIABLE_COST, &
                                    R_ANNUAL_EMISSION_COST, &
                                    R_ANNUAL_USAGE, &
                                    R_ANNUAL_STRIKE_PRICE, &
                                    R_CO2_COST_PER_MWH)
       
!
!
! 042714. FIGURE-OUT TIME DIMENSIONS INTERNALLY. ASSUME MONTHLY CALL.
!

       
       if(file_trace_cxat==0) then
        file_trace_cxat=open_trace("cxat.trace", rq_cxat)
       end if
       if(trim(cxac_called_from)=="") then
        stop "cxac_called_from not set"
       end if
       call write_trace_string(file_trace_cxat, "cxat called from", &
        cxac_called_from)
       cxac_called_from=""
        
       if(cxat_count>5000) then
        file_trace_cxat=BAD_TRACE_HANDLE
       else
        cxat_count=cxat_count+1
       end if
       call write_trace_real4s_vert(file_trace_cxat, "R_ANNUAL_PRICE 1a", &
        R_ANNUAL_PRICE)
        
       if(file_trace_cxat/=BAD_TRACE_HANDLE) then
        write(file_trace_cxat,*) "Other args: ",  &
           "R_TRANS ", R_TRANS, linefeed, &
           "R_ANNUAL_HOURS ", R_ANNUAL_HOURS, linefeed, &
           "R_ANNUAL_STRIKES ", R_ANNUAL_STRIKES, linefeed, &
           "R_ANNUAL_ENERGY ", R_ANNUAL_ENERGY, linefeed, &
           "3. R_ANNUAL_ENERGY_REVENUE ", R_ANNUAL_ENERGY_REVENUE, linefeed, &
           "R_ANNUAL_VARIABLE_COST ", R_ANNUAL_VARIABLE_COST, linefeed, &
           "R_ANNUAL_EMISSION_COST ", R_ANNUAL_EMISSION_COST, linefeed, &
           "R_ANNUAL_USAGE ", R_ANNUAL_USAGE, linefeed, &
           "R_ANNUAL_STRIKE_PRICE ", R_ANNUAL_STRIKE_PRICE, linefeed, &
           "R_CO2_COST_PER_MWH ", R_CO2_COST_PER_MWH, linefeed
       end if
       

      CX_ANNUAL_CONTRACT = .FALSE.

      call write_trace_bool1(file_trace_cxat, "sMPS",  &
        SAVE_MONTHLY_PRODUCTS_STATUS)
        
      IF(.NOT. SAVE_MONTHLY_PRODUCTS_STATUS) RETURN
!
      IND = R_TRANS
! 052615. QUANTITY OF PRODUCT IS SET TO ZERO AND THEN CONTROLS THE
!         NUMBER OF INSTANCES OF A PARTICULAR ADDITION.
!         NEED TO MAKE SURE THAT IT GETS RESET WITH EACH ITERATION.
      LOCAL_CAPACITY = PROPOSED_QUANT_OF_PRODUCT(IND)
! 052415. ANNUAL PARAMETERS.
      R_ANNUAL_HOURS = 0.0 ! HOURS WHEN ENERGY > 0
      R_ANNUAL_STRIKES = 0.0
      R_ANNUAL_ENERGY = 0.0
      R_ANNUAL_ENERGY_REVENUE = 0.0
      R_ANNUAL_VARIABLE_COST = 0.0
      R_ANNUAL_EMISSION_COST = 0.0
      R_ANNUAL_USAGE = 0.0
      R_ANNUAL_STRIKE_PRICE = 0.0

      MONTHLY_OP_HOURS = 0.0
      MONTHLY_OP_STRIKES = 0.0
      MONTHLY_OP_ENERGY = 0.0
      MONTHLY_OP_ENERGY_REVENUE = 0.0
      MONTHLY_OP_VARIABLE_COST = 0.0
      MONTHLY_OP_FIXED_COST = 0.0
      DAILY_ENERGY_REVENUE = 0.0
      DAY_IN_YEAR = 0
!
      INDEP_ENERGY_COST = 0.0
      INDEP_ENERGY_REVENUE = 0.0
      INDEP_ENR_FOR_EXP = 0.0
      INDEP_ENR_FOR_REV = 0.0
!
      TEMP_PRICE_24 = 0. ! PURPOSELY ZERO TO CALCULATE REVENUE
!
      DO MONTH = 1, 12
         MO = MONTH
         CALL GET_CUM_HOURS_N_HOURS_IN_MONTH(MO, &
                                             HOURS_IN_MONTH, &
                                             CUMULATIVE_HOURS)

! 012422. ASSUMES ALL HOURS RATHER THAN HOURS OF STRIKE.
!
         MONTHLY_TRANS_HOURS(IND,MONTH) = HOURS_IN_MONTH
!
         TEST_DATE = 100*(globecom_YEAR+get_BASE_YEAR()- 1900) + MONTH
         
         IF(TEST_DATE > END_EP(IND) .OR. &
            TEST_DATE < BEGIN_EP(IND)) then

            RETURN
         end if

         IF(TEST_DATE > BEGIN_EP(IND)) THEN
            BEGIN_DAY_IN_MONTH(IND) = 1
         ELSE
            BEGIN_DAY_IN_MONTH(IND) = BEGIN_DAY(IND)
         ENDIF
         IF(TEST_DATE < END_EP(IND)) THEN
            END_DAY_IN_MONTH(IND) = 31
         ELSE
            END_DAY_IN_MONTH(IND) = END_DAY(IND)
         ENDIF
!
         CALL_OR_PUT = 1.
         IF(DERIVATIVE_TYPE(IND) == 3 .OR. &
                              DERIVATIVE_TYPE(IND) == 6 .OR. &
                              DERIVATIVE_TYPE(IND) == 8 .OR. &
                              DERIVATIVE_TYPE(IND) == 11 .OR. &
                                      DERIVATIVE_TYPE(IND) == 14) THEN
            CALL_OR_PUT = -1.
         ENDIF
!
! TIME LOOPING HERE: SET FOR DAY IN WEEK.
!
!
! 041615. ADDING HOUR, MONTH, YEAR TO DAILY.
!
! ENERGY PRICE
!
         if(file_trace_cxat/=BAD_TRACE_HANDLE) then
            write(file_trace_cxat, *) &
                "YEAR ", globecom_YEAR+get_BASE_YEAR(), &
                "MO ", MO,"IND ", IND, &
                "MEP ", MONTHLY_ENERGY_PRICE(IND)
         end if
         
         L_DUMMY=GET_ESC_NRG_PRICE(globecom_YEAR+get_BASE_YEAR(),MO,IND, &
                                      MONTHLY_ENERGY_PRICE(IND))

         IF(ENERGY_PRICE_MULTIPLIER(IND) < 0.) THEN
            call write_trace_message(file_trace_cxat, "EPM is zero.")
            
!
            TEMP_R1 = 1.0
            TEMP_I2 = INT(ABS(ENERGY_PRICE_MULTIPLIER(IND)),2)
            MONTHLY_ENERGY_MULT(IND) = &
                 ESCALATED_MONTHLY_VALUE(TEMP_R1,TEMP_I2,globecom_YEAR, &
                                                   MO,INT(1,2))
!
         ELSE
!
            MONTHLY_ENERGY_MULT(IND) = ENERGY_PRICE_MULTIPLIER(IND)
!
         ENDIF
! CAPACITY
       call write_trace_int2(file_trace_cxat, "IND", IND)
       call write_trace_real4(file_trace_cxat, "E-M", &
        ENERGY_MULTIPLIER(IND))
         IF(ENERGY_MULTIPLIER(IND) < 0.) THEN
            TEMP_R1 = ABS(ENERGY_MULTIPLIER(IND))
            TEMP_I2 = INT(ABS(ENERGY_MULTIPLIER(IND)),2)
! 021422. TEMP_QUANTITY INSTEAD OF HOURLY_QUANTITY(IND).
            TEMP_QUANTITY = &
                 ESCALATED_MONTHLY_VALUE(TEMP_R1,TEMP_I2,globecom_YEAR, &
                                                   MO,INT(1,2))
         ELSE
            TEMP_QUANTITY = ENERGY_MULTIPLIER(IND)
         ENDIF
! 081521. TO ACCOMMODATE HYBRID
       call write_trace_int2(file_trace_cxat, "IND", IND)
       call write_trace_int2(file_trace_cxat, "prop. res. pos.", &
        PROPOSED_RESOURCE_POSITION(IND))
        
         IF(PROPOSED_RESOURCE_POSITION(IND) == 0) THEN
! 082221. 082421.
            IF(QUANTITY_OF_PRODUCT(IND) < 0.) THEN
               TEMP_QUANTITY = TEMP_QUANTITY * &
                        GET_VAR(QUANTITY_OF_PRODUCT(IND), &
                                                globecom_YEAR, &
                                                TRANSACTION_NAME(IND))
            ELSE
               TEMP_QUANTITY = TEMP_QUANTITY * &
                                               QUANTITY_OF_PRODUCT(IND)
            ENDIF
         ELSEIF(LOCAL_CAPACITY < 0.) THEN
            TEMP_QUANTITY = TEMP_QUANTITY * &
                        GET_VAR(LOCAL_CAPACITY, &
                                                globecom_YEAR, &
                                                TRANSACTION_NAME(IND))
         ELSE
            TEMP_QUANTITY = TEMP_QUANTITY * LOCAL_CAPACITY
         ENDIF
         call write_trace_int2(file_trace_cxat, "IND", IND)
         call write_trace_int2(file_trace_cxat, "deriv", &
            DERIVATIVE_TYPE(IND))
         IF(DERIVATIVE_TYPE(IND) < 4 .OR. &
                   DERIVATIVE_TYPE(IND) == 12) THEN ! PHYSICAL
            TYPE_OF_DERIV = 1
         ELSEIF(STRIKE_FREQUENCY(IND) == 'H') THEN ! ASSUMES HOURLY
            TYPE_OF_DERIV = 2
         ELSE ! ASSUME DAILY FOR NOW
            TYPE_OF_DERIV = 3
         ENDIF
         DAYS_IN_MONTH = HOURS_IN_MONTH/24
         DO DAY = 1, DAYS_IN_MONTH
            if (cxat_count<10000) then
                cxat_count=cxat_count+1
            else
                file_trace_cxat=BAD_TRACE_HANDLE
            end if
            
            DAY_IN_WEEK = GET_DAY_OF_WEEK_4(MO,DAY)
            DAY_IN_YEAR = DAY_IN_YEAR + 1
!
            J = CUMULATIVE_HOURS + (DAY-1)*24 ! + 1
            DAILY_PRICE(1:24) = R_ANNUAL_PRICE(J:J+23)
            call write_trace_real4s(file_trace_cxat, "DAILY_PRICE", &
                DAILY_PRICE)
                
            
!
            APPLY_5X16 = APPLY_ENERGY_PRODUCT(INT(12,2),DAY_IN_WEEK, &
                                                  LOCAL_PRODUCT_TYPE(1))
            APPLY_6X16 = APPLY_ENERGY_PRODUCT(INT(12,2),DAY_IN_WEEK, &
                                                  LOCAL_PRODUCT_TYPE(2))
            APPLY_7X24 = APPLY_ENERGY_PRODUCT(INT(12,2),DAY_IN_WEEK, &
                                                  LOCAL_PRODUCT_TYPE(3))
            APPLY_5X8  = APPLY_ENERGY_PRODUCT(INT(1,2), DAY_IN_WEEK, &
                                                  LOCAL_PRODUCT_TYPE(4))
            APPLY_WRAP  = APPLY_ENERGY_PRODUCT(INT(1,2),DAY_IN_WEEK, &
                                                  LOCAL_PRODUCT_TYPE(5))
            APPLY_2X24  = APPLY_ENERGY_PRODUCT(INT(1,2),DAY_IN_WEEK, &
                                                  LOCAL_PRODUCT_TYPE(7))

            N_BEST_VALUES=0
            call write_trace_int2(file_trace_cxat, "IND", IND)
            call write_trace_string(file_trace_cxat, "PRICE_TYPE", &
                PRICE_TYPE(IND))
                
            IF(PRICE_TYPE(IND) == 'I') THEN
               MONTHLY_ENERGY_PRICE(IND) = 0.
               DO J = 1, 24
                  HOUR = CUMULATIVE_HOURS + (DAY-1)*24 + J - 1 ! 081116.
                  HOURLY_ENERGY_PRICE(J) = R_ANNUAL_PRICE(HOUR) * &
                                                MONTHLY_ENERGY_MULT(IND)
!               MONTHLY_ENERGY_PRICE(IND) =
               ENDDO
            ELSE
               HOURLY_ENERGY_PRICE = MONTHLY_ENERGY_PRICE(IND) + &
                                                      R_CO2_COST_PER_MWH
            ENDIF
!
!
            IF( PRODUCT_TYPE(IND) == LOCAL_PRODUCT_TYPE(1) .AND. &
                                                        APPLY_5X16) THEN
               PRODUCT = 1
            ELSEIF(PRODUCT_TYPE(IND) == LOCAL_PRODUCT_TYPE(2) .AND. &
                                                        APPLY_6X16) THEN
               PRODUCT = 2
            ELSEIF(PRODUCT_TYPE(IND) == LOCAL_PRODUCT_TYPE(3) .AND. &
                                                        APPLY_7X24) THEN
               PRODUCT = 3
            ELSEIF(PRODUCT_TYPE(IND) == LOCAL_PRODUCT_TYPE(4) .AND. &
                                                        APPLY_5X8) THEN
               PRODUCT = 4
            ELSEIF(PRODUCT_TYPE(IND) == LOCAL_PRODUCT_TYPE(5) .AND. &
                                                        APPLY_WRAP) THEN
               IF(APPLY_2X24) THEN
                  PRODUCT = 7
               ELSE
                  PRODUCT = 5
               ENDIF
            ELSEIF(PRODUCT_TYPE(IND) == LOCAL_PRODUCT_TYPE(6)) THEN
               PRODUCT = 6

              if(mo>12) then
                call end_program("transobj2:0032 - invalid month.")
              end if
              call write_trace_int2(file_trace_cxat, "NUM_PRODUCTS", &
                NUM_PRODUCTS)
                
              month_pass=mo
              dow_pass=day_in_week
              gud_called_from="2. read_energy_products_data"
              gud_pass=GET_USER_DAY(dow_pass, &
                                 month_pass, &
                                        USER_DAY_TYPES_ID(IND), &
                                        PRODUCT_PATTERN(1,PRODUCT), &
                                        MONTHLY_CF_TRANS(IND))
                                        
               HOURS_FOR_PRODUCT(PRODUCT) = gud_pass
               mo=month_pass
               DAY_IN_WEEK=dow_pass
! CONTEMPLATE OTHER STORED PATTERNS.
!
            ELSEIF(PRODUCT_TYPE(IND) == LOCAL_PRODUCT_TYPE(8)) THEN
               PRODUCT = 8
               TEMP_L = GET_DAILY_PRODUTION_DATA( &
                                             IND, &
                                             DAY, &
                                             MO, &
                                             HourlyProdPtrToCFLoc(IND), &
                                             PRODUCT_PATTERN(1,PRODUCT))
            ENDIF


            DAILY_MW = TEMP_QUANTITY
!
            LONG_SHORT = 1.0
!
            IF(EXPENSE_ASSIGNMENT(IND) == LOCAL_ASSIGNMENT(1)) THEN
               LONG_SHORT = -LONG_SHORT
            ENDIF
!
       call write_trace_int2(file_trace_cxat, "IND", IND)
       call write_trace_string(file_trace_cxat,  "UNIT_CONTINGENCY", &
        UNIT_CONTINGENCY(IND))
        
            IF(UNIT_CONTINGENCY(IND) /= 'N') THEN
               DO HOUR = 1, 24
                  HOUR_IN_MONTH = (DAY-1) * 24 + HOUR
                  UNIT_AVAIL = 0.0
                  RESOURCE_MW = 0.
                  LOCAL_UNIT_ID = UNIT_CONTINGENT_LINK(IND)
                  IF(LOCAL_UNIT_ID < 0) THEN
                     ALL_CONTINGENT_MUST_BE_UP = &
                                 ABS(HOURLY_QUANTITY(IND) - &
                                    MAX_QUANTITY_OF_PRODUCT(IND)) < .001
                     TOTAL_CONTINGENT_UNITS = 1
                     TOTAL_CONTINGENT_UP_UNITS = 0
                     TOTAL_CONTINGENT_FOR_UNITS = 0
                     TOTAL_CONTINGENT_MOR_UNITS = 0
                     LOCAL_UNIT_ID = &
                           GET_VAR(FLOAT(UNIT_CONTINGENT_LINK(IND)), &
                                         TOTAL_CONTINGENT_UNITS, &
                                                 'MultiUnit Contingent')
                     DOWHILE(LOCAL_UNIT_ID > 0 .AND. &
                                           TOTAL_CONTINGENT_UNITS <= 30)
                        TEMP_UNIT_NUMBER = &
                                 GET_RESOURCE_ID_TO_UNIT(LOCAL_UNIT_ID, &
                             NUMBER_OF_UNITS_FOR_ID) ! FIRST OCCURRENCE

!
                        TEMP_R4 = &
                             GET_BLOCK_AVAIL_4_HOUR(TEMP_UNIT_NUMBER, &
                                                         INT(1,2), &
                                                      HOUR_IN_MONTH) + &
                             GET_BLOCK_AVAIL_4_HOUR(TEMP_UNIT_NUMBER, &
                                                         INT(2,2), &
                                                      HOUR_IN_MONTH)
                        UNIT_AVAIL = UNIT_AVAIL + TEMP_R4
!
                        CALL GET_MWBLOK_FOR_UNIT( &
                                       TEMP_UNIT_NUMBER,TEMP_R1,TEMP_R2)
!
                        RESOURCE_MW = RESOURCE_MW + TEMP_R1 + TEMP_R2
                        TOTAL_CONTINGENT_UNITS = &
                                              TOTAL_CONTINGENT_UNITS + 1
!
                        OUTAGE_TYPE = &
                              GET_BLOCK_OUTAGE_4_HOUR(TEMP_UNIT_NUMBER, &
                                                         INT(1,2), &
                                                         HOUR_IN_MONTH)
!
                        IF(OUTAGE_TYPE == 1) THEN
                           TOTAL_CONTINGENT_MOR_UNITS = &
                                          TOTAL_CONTINGENT_MOR_UNITS + 1
                        ELSEIF(OUTAGE_TYPE == 2) THEN
                           TOTAL_CONTINGENT_FOR_UNITS = &
                                          TOTAL_CONTINGENT_FOR_UNITS + 1
                        ELSE
                           TOTAL_CONTINGENT_UP_UNITS = &
                                           TOTAL_CONTINGENT_UP_UNITS + 1
                        ENDIF
!
                        LOCAL_UNIT_ID = &
                              GET_VAR(FLOAT(UNIT_CONTINGENT_LINK(IND)), &
                                         TOTAL_CONTINGENT_UNITS, &
                                                 'MultiUnit Contingent')
                     ENDDO
! 11/25/03. DETERMINE TEMP_QUANTITY AND UNIT_AVAIL FOR THE MULTI CONTINGENT CASE
!
                     TOTAL_CONTINGENT_UNITS = &
                                              TOTAL_CONTINGENT_UNITS - 1
! 090904 TESTING
                     TEST_CONTINGENCY = .TRUE.
                     IF(ALL_CONTINGENT_MUST_BE_UP .AND. &
                                TOTAL_CONTINGENT_UNITS /= &
                                         TOTAL_CONTINGENT_UP_UNITS) THEN
                        IF(UNIT_CONTINGENCY(IND) == 'U') THEN
                           TEST_CONTINGENCY = .FALSE.
                           DAILY_MW(HOUR) = 0.
                        ELSEIF(UNIT_CONTINGENCY(IND) == 'M' .AND. &
                                    TOTAL_CONTINGENT_UNITS /= &
                                      TOTAL_CONTINGENT_UP_UNITS + &
                                        TOTAL_CONTINGENT_FOR_UNITS) THEN
                           TEST_CONTINGENCY = .FALSE.
                           DAILY_MW(HOUR) = 0.
                        ELSEIF(UNIT_CONTINGENCY(IND) == 'F' .AND. &
                                    TOTAL_CONTINGENT_UNITS /= &
                                      TOTAL_CONTINGENT_UP_UNITS + &
                                        TOTAL_CONTINGENT_MOR_UNITS) THEN
                           TEST_CONTINGENCY = .FALSE.
                           DAILY_MW(HOUR) = 0.
                        ENDIF
                     ENDIF
!
                     IF(UNIT_CONTING_MARKET_SUPPLEMENT(IND) == &
                                                               'T') THEN

                           CONTINGENT_CAPACITY(HOUR,IND) = &
                                 CONTINGENT_CAPACITY(HOUR,IND) + &
                                       MAX(0.0,RESOURCE_MW - UNIT_AVAIL)

                     ELSEIF(TEST_CONTINGENCY) THEN
!                        IF(TEST_CONTINGENCY) THEN
                        IF(UNIT_CONTINGENCY(IND) == 'R' .OR. &
                                UNIT_CONTINGENCY(IND) == 'A' .OR. &
                                      UNIT_CONTINGENCY(IND) == 'B') THEN
                           IF(UNIT_CONTINGENCY(IND) == 'R') THEN
                              DAILY_MW(HOUR) = &
                                    MIN(MAX_QUANTITY_OF_PRODUCT(IND), &
                                       TEMP_QUANTITY * &
                                         (TOTAL_CONTINGENT_MOR_UNITS + &
                                            TOTAL_CONTINGENT_FOR_UNITS))
                           ELSEIF(UNIT_CONTINGENCY(IND) == 'A') THEN
                              DAILY_MW(HOUR) = &
                                    MIN(MAX_QUANTITY_OF_PRODUCT(IND), &
                                       TEMP_QUANTITY * &
                                             TOTAL_CONTINGENT_MOR_UNITS)
                           ELSEIF(UNIT_CONTINGENCY(IND) == 'B') THEN
                              DAILY_MW(HOUR) = &
                                    MIN(MAX_QUANTITY_OF_PRODUCT(IND), &
                                       TEMP_QUANTITY * &
                                             TOTAL_CONTINGENT_FOR_UNITS)
                           ENDIF
                        ELSE
!
! 083006. ALTERED CODE FOR CONTINGENCIES.
!
                           IF( UNIT_CONTINGENCY(IND) == 'U') THEN
                              DAILY_MW(HOUR) = &
                                    MIN(MAX_QUANTITY_OF_PRODUCT(IND), &
                                       TEMP_QUANTITY * &
                                         TOTAL_CONTINGENT_UP_UNITS)
                           ELSEIF( & !  OUTAGE_TYPE /= 1 .AND.
                                      UNIT_CONTINGENCY(IND) == 'M') THEN
                              DAILY_MW(HOUR) = &
                                    MIN(MAX_QUANTITY_OF_PRODUCT(IND), &
                                       TEMP_QUANTITY * &
                                        (TOTAL_CONTINGENT_UNITS - &
                                            TOTAL_CONTINGENT_MOR_UNITS))
! 091106.
                              CONTINGENT_CAPACITY(HOUR,IND) = &
                                    MIN(MAX_QUANTITY_OF_PRODUCT(IND), &
                                       TEMP_QUANTITY * &
                                             TOTAL_CONTINGENT_MOR_UNITS)
                           ELSEIF(  & !  OUTAGE_TYPE /= 2 .AND.
                                      UNIT_CONTINGENCY(IND) == 'F') THEN
                              DAILY_MW(HOUR) = &
                                    MIN(MAX_QUANTITY_OF_PRODUCT(IND), &
                                       TEMP_QUANTITY * &
                                        (TOTAL_CONTINGENT_UNITS - &
                                            TOTAL_CONTINGENT_FOR_UNITS))
! 091106.
                              CONTINGENT_CAPACITY(HOUR,IND) = &
                                    MIN(MAX_QUANTITY_OF_PRODUCT(IND), &
                                       TEMP_QUANTITY * &
                                             TOTAL_CONTINGENT_FOR_UNITS)
                           ENDIF
                        ENDIF
                     ENDIF ! TEST CONTINGENCY
                  ELSE
                     TEMP_UNIT_NUMBER = &
                                 GET_RESOURCE_ID_TO_UNIT(LOCAL_UNIT_ID, &
                               NUMBER_OF_UNITS_FOR_ID) ! FIRST OCCURRENCE
                     UNIT_AVAIL = UNIT_AVAIL + &
                             GET_BLOCK_AVAIL_4_HOUR(TEMP_UNIT_NUMBER, &
                                                         INT(1,2), &
                                                      HOUR_IN_MONTH) + &
                             GET_BLOCK_AVAIL_4_HOUR(TEMP_UNIT_NUMBER, &
                                                         INT(2,2), &
                                                      HOUR_IN_MONTH)
                     CALL GET_MWBLOK_FOR_UNIT( &
                                       TEMP_UNIT_NUMBER,TEMP_R1,TEMP_R2)
                     RESOURCE_MW = TEMP_R1 + TEMP_R2
!
                     IF(RESOURCE_MW > 0.) THEN
                        UNIT_AVAIL = UNIT_AVAIL/RESOURCE_MW
                        IF(UNIT_AVAIL > 1.0) THEN
                           WRITE(4,*) "PROBLEM IN UNIT CONTINGENT"
                        ENDIF

                     ELSE
                        UNIT_AVAIL = 0.0
                     ENDIF
!
                     OUTAGE_TYPE = &
                           GET_BLOCK_OUTAGE_4_HOUR(TEMP_UNIT_NUMBER, &
                                                         INT(1,2), &
                                                         HOUR_IN_MONTH)
! 11/03/03. NEW CASES FOR REPLACEMENT LOGIC.
                     IF(UNIT_CONTINGENCY(IND) == 'R' .OR. &
                                UNIT_CONTINGENCY(IND) == 'A' .OR. &
                                      UNIT_CONTINGENCY(IND) == 'B') THEN
                        IF( UNIT_CONTINGENCY(IND) == 'R' .OR. &
                               ( (OUTAGE_TYPE == 1 .AND. &
                                      UNIT_CONTINGENCY(IND) == 'A') .OR. &
                                 (OUTAGE_TYPE == 2 .AND. &
                                   UNIT_CONTINGENCY(IND) == 'B') )) THEN
                           DAILY_MW(HOUR) = &
                                       TEMP_QUANTITY * (1. - UNIT_AVAIL)
                        ELSE
                           DAILY_MW(HOUR) = 0.
                        ENDIF
                     ELSE
!
! 090904 MAJOR RE-WRITE.
!
                        IF( UNIT_CONTINGENCY(IND) == 'U') THEN
                           DAILY_MW(HOUR) = TEMP_QUANTITY*UNIT_AVAIL
                        ELSEIF( (OUTAGE_TYPE == 1 .AND. &
                                      UNIT_CONTINGENCY(IND) == 'M') .OR. &
                                    (OUTAGE_TYPE == 2 .AND. &
                                    UNIT_CONTINGENCY(IND) == 'F') ) THEN
                           DAILY_MW(HOUR) = TEMP_QUANTITY*UNIT_AVAIL
                        ENDIF
                     ENDIF
                  ENDIF ! SINGLE OR MULTIPLE CONTINGENT

               ENDDO ! HOUR

      ENDIF ! CONTINGENT OPTION
!
      R_HOUR_ACTIVE= PRODUCT_PATTERN(1,PRODUCT)
           call write_trace_real4s(file_trace_cxat, "R_HOUR_ACTIVE", &
            R_HOUR_ACTIVE)
            
                LMP_HourlyEnergyPrice = DAILY_PRICE ! 100312 R_HOUR_PRICE
       call write_daily_energy_trace("transobj2:A001", daily_energy)
       if(file_trace_cxat/=BAD_TRACE_HANDLE) then
          write(file_trace_cxat, *) "Calling GET_HOUR_DAY_OPTION_VALUE ", &
              "DAY IN YEAR", DAY_IN_YEAR, &
              "TYPE_OF_DERIV ", TYPE_OF_DERIV, &
              "LMP_HourlyEnergyPrice ", LMP_HourlyEnergyPrice, &
              "DAILY_MW ", DAILY_MW, &
              "PRODUCT_PATTERN ", PRODUCT_PATTERN(1,PRODUCT), &
              "CALL_OR_PUT ", CALL_OR_PUT, &
              "HOURLY_ENERGY_PRICE ", HOURLY_ENERGY_PRICE, &
              "DAILY_STRIKES ", DAILY_STRIKES, &
              "DAILY_ENERGY ", DAILY_ENERGY, &
              "DAILY_VARIABLE_COST ", DAILY_VARIABLE_COST, &
              "DAILY_ENERGY_REVENUE ", DAILY_ENERGY_REVENUE(DAY_IN_YEAR), &
              "HOUR_USAGE ", HOUR_USAGE, &
              "cxat_count ", cxat_count
       end if
           r_hour_active=PRODUCT_PATTERN(1,PRODUCT)
         call write_trace_real4s(file_trace_cxat, "2. R_HOUR_ACTIVE", &
            R_HOUR_ACTIVE)
        if(cxat_count==1100) then
            cxat_count=cxat_count ! Debugstop
            if(file_trace_cxat /= BAD_TRACE_HANDLE) then
           write(file_trace_cxat,*) crlf, "PRODUCT_PATTERN ", &
            PRODUCT_PATTERN, crlf
            end if
        end if
        
        
        
            
            DAILY_OPTION_VALUE = &
                  GET_HOUR_DAY_OPTION_VALUE( &
                                    TYPE_OF_DERIV, &
                                    LMP_HourlyEnergyPrice, & 
                                    DAILY_MW, &
                                    PRODUCT_PATTERN(1,PRODUCT), &
                                    CALL_OR_PUT, &
                                    HOURLY_ENERGY_PRICE, &
                                    DAILY_STRIKES, &
                                    DAILY_ENERGY, &
                                    DAILY_VARIABLE_COST, &
                                    DAILY_ENERGY_REVENUE(DAY_IN_YEAR), &
                                    HOUR_USAGE)
                                    
       if (file_trace_cxat/=BAD_TRACE_HANDLE) then
        write(file_trace_cxat,*) "< get_hour_day_option_value ", &
        "PRODUCT ", PRODUCT, &
         "TYPE_OF_DERIV ", TYPE_OF_DERIV, &
         "hourly price ", LMP_HourlyEnergyPrice, & 
         "DAILY_MW ", DAILY_MW, &
         "PRODUCT_PATTERN ", PRODUCT_PATTERN(1,PRODUCT), &
         "CALL_OR_PUT ", CALL_OR_PUT, &
         "HOURLY_ENERGY_PRICE ", HOURLY_ENERGY_PRICE, &
         "DAILY_STRIKES ", DAILY_STRIKES, &
         "DAILY_ENERGY ", DAILY_ENERGY, &
         "DAILY_VARIABLE_COST ", DAILY_VARIABLE_COST, &
         "DAILY_ENERGY_REVENUE ", DAILY_ENERGY_REVENUE(DAY_IN_YEAR), &
         "HOUR_USAGE ", HOUR_USAGE
       end if

              call write_trace_real4(file_trace_cxat, "3. DOV", &
                DAILY_OPTION_VALUE)

        call write_daily_energy_trace("transobj2:A002", daily_energy)
            START_HOUR = CUMULATIVE_HOURS + (DAY-1)*24  ! + 1
            R_ANNUAL_USAGE(START_HOUR:START_HOUR+23) = HOUR_USAGE
            R_ANNUAL_STRIKE_PRICE(START_HOUR:START_HOUR+23) = &
                                                     HOURLY_ENERGY_PRICE
            MONTHLY_OP_HOURS(MONTH) = MONTHLY_OP_HOURS(MONTH) + &
                                                   DAILY_STRIKES
            call write_trace_real4(file_trace_cxat, "Daily energy", &
                DAILY_ENERGY)
                
            MONTHLY_OP_ENERGY(MONTH) = MONTHLY_OP_ENERGY(MONTH) + &
                                                   DAILY_ENERGY
         call write_trace_real4(file_trace_cxat, "1. MOH", &
            MONTHLY_OP_ENERGY(MONTH))

            MONTHLY_OP_ENERGY_REVENUE(MONTH) = &
                     MONTHLY_OP_ENERGY_REVENUE(MONTH) + &
                                       DAILY_ENERGY_REVENUE(DAY_IN_YEAR)
            MONTHLY_OP_VARIABLE_COST(MONTH) = &
                     MONTHLY_OP_VARIABLE_COST(MONTH) + &
                                                     DAILY_VARIABLE_COST
            IF(STRIKE_FREQUENCY(IND) == 'H') THEN ! ASSUMES HOURLY
               DAILY_OPTION_VALUE = &
                              GET_HOURLY_OPTION_VALUE( &
                                             LMP_HourlyEnergyPrice, &
                                             DAILY_MW, &
                                             PRODUCT_PATTERN(1,PRODUCT), &
                                             CALL_OR_PUT, &
                                             HOURLY_ENERGY_PRICE, &
                                             TEMP_STRIKES)
              call write_trace_real4(file_trace_cxat, "4. DOV", &
                DAILY_OPTION_VALUE)
               TEMP_HOURS = TEMP_STRIKES
!
            ELSE ! ASSUMES DAILY
               DAILY_OPTION_VALUE = &
                              GET_DAILY_OPTION_VALUE( &
                                             LMP_HourlyEnergyPrice, &
                                             DAILY_MW, &
                                             PRODUCT_PATTERN(1,PRODUCT), &
                                             CALL_OR_PUT, &
                                             HOURLY_ENERGY_PRICE)
              call write_trace_real4(file_trace_cxat, "5. DOV", &
                DAILY_OPTION_VALUE)
               TEMP_STRIKES = 1
               TEMP_HOURS = INT(HOURS_FOR_PRODUCT(PRODUCT),2)
            ENDIF
!
! IF VALUE NON-POSITIVE, ITS NO GOOD.  CYCLE.
!
! 052515. CAN'T DO THIS BECAUSE OF PHYSICAL.


            CX_ANNUAL_CONTRACT = .TRUE.
            SELL_OR_BUY = 1.
            LONG_SHORT = 1.0

            DAILY_MW(0) =DOT_PRODUCT(DAILY_MW(1:24), &
                                          PRODUCT_PATTERN(1:24,PRODUCT))

! 4/19/02. DON'T KNOW WHY THIS WAS TURNED-OFF.

            IF(EXPENSE_ASSIGNMENT(IND) == LOCAL_ASSIGNMENT(1)) THEN
               LONG_SHORT = -LONG_SHORT
            ENDIF

! 10/06/03.

            LONG_SHORT = LONG_SHORT*CALL_OR_PUT

            IF(DERIVATIVE_TYPE(IND) < 4 .OR. &
                   DERIVATIVE_TYPE(IND) == 12) THEN ! PHYSICAL
               DAILY_CAPACITY = 0
               DO Hr = 1, 24
                  DAILY_CAPACITY(Hr) = &
                       LONG_SHORT* &
                       DAILY_MW(Hr) * PRODUCT_PATTERN(Hr,PRODUCT)
               ENDDO
!
!               DAILY_OPTION_REVENUE = 0.
            ENDIF
! 052415. ALWAYS ACCUMULATE REVENUE.
!
               SELL_OR_BUY = 1.0
!
               IF(STRIKE_FREQUENCY(IND) == 'H') THEN ! ASSUMES HOURLY
                    DAILY_OPTION_REVENUE = &
                              GET_HOURLY_OPTION_VALUE( &
                               LMP_HourlyEnergyPrice, & 
                               DAILY_MW, &
                               PRODUCT_PATTERN(1,PRODUCT), &
                               SELL_OR_BUY, &
                               TEMP_PRICE_24, &
                               TEMP_STRIKES)
!
                  IF(DerivLMPActive(IND) .AND. &
                                           DerivCalCongestion(IND)) THEN
                     DO HR = 1, 24
                        IF(DAILY_MW(HR) > 0.) THEN
                           DerivCongestionCost(IND,MO) = &
                                        DerivCongestionCost(IND,MO) &
                                        + (R_DAILY_PRICE(HR) &
                                           - LMP_HourlyEnergyPrice(HR))* &
                                           DAILY_MW(HR)
                        ENDIF
                     ENDDO
                  ENDIF
               ELSE ! ASSUMES DAILY
                  DAILY_OPTION_REVENUE = &
                              GET_DAILY_OPTION_VALUE( &
                                   LMP_HourlyEnergyPrice, & 
                                   DAILY_MW, &
                                   PRODUCT_PATTERN(1,PRODUCT), &
                                   SELL_OR_BUY, &
                                   TEMP_PRICE_24)
                  IF(DerivLMPActive(IND) .AND. &
                                           DerivCalCongestion(IND)) THEN
                     DO HR = 1, 24
                        IF(PRODUCT_PATTERN(HR,PRODUCT) > 0) THEN
                           DerivCongestionCost(IND,MO) = &
                                        DerivCongestionCost(IND,MO) &
                                        + (R_DAILY_PRICE(HR) &
                                           - LMP_HourlyEnergyPrice(HR))* &
                                           DAILY_MW(HR)
                        ENDIF
                     ENDDO
                  ENDIF
               ENDIF
!            ENDIF
!
! 012722. TO MAKE REVENUE ASSUMPTION CONSISTENT.
!
            IF(DERIVATIVE_TYPE(IND) < 4) THEN ! PHYSICAL
                  TEMP_R1 = 0.
            ELSE
                  TEMP_R1 = 1.
            ENDIF

!
! 082121. FOR INDEP PORTION OF HYBRID
!
               IF(LONG_SHORT > 0.) THEN
                  INDEP_ENERGY_COST(MONTH) = &
                     INDEP_ENERGY_COST(MONTH) + &
                                 MONTHLY_ENERGY_PRICE(IND) * DAILY_MW(0)
                  INDEP_ENERGY_REVENUE(MONTH) = &
                      INDEP_ENERGY_REVENUE(MONTH) + &
                                          TEMP_R1 * DAILY_OPTION_REVENUE
!     +                                             DAILY_OPTION_REVENUE
                  INDEP_ENR_FOR_EXP(MONTH) = &
                          INDEP_ENR_FOR_EXP(MONTH) + DAILY_MW(0)
               ELSE
                  INDEP_ENERGY_COST(MONTH) = &
                     INDEP_ENERGY_COST(MONTH) + &
                                                    DAILY_OPTION_REVENUE
                  INDEP_ENERGY_REVENUE(MONTH) = &
                      INDEP_ENERGY_REVENUE(MONTH) + &
                                 MONTHLY_ENERGY_PRICE(IND) * DAILY_MW(0)
                  INDEP_ENR_FOR_REV(MONTH) = &
                          INDEP_ENR_FOR_REV(MONTH) + DAILY_MW(0)
               ENDIF
!            ENDDO ! TRANSACTIONS
         ENDDO ! DAY_IN_MONTH
! 052415.
!         MONTHLY_OP_FIXED_COST(MONTH) =
!
         R_ANNUAL_HOURS = R_ANNUAL_HOURS + MONTHLY_OP_HOURS(MONTH)
         call write_trace_real4(file_trace_cxat, "annual hours", &
            R_ANNUAL_HOURS)
            
         call write_trace_int2(file_trace_cxat, "MONTH", MONTH)
         call write_trace_real4(file_trace_cxat, "MOO", &
            MONTHLY_OP_ENERGY(MONTH))
         R_ANNUAL_ENERGY = R_ANNUAL_ENERGY + MONTHLY_OP_ENERGY(MONTH)
         call write_trace_real4(file_trace_cxat, "R_ANNUAL_ENERGY", &
            R_ANNUAL_ENERGY)
            
         R_ANNUAL_ENERGY_REVENUE = R_ANNUAL_ENERGY_REVENUE + &
                                     MONTHLY_OP_ENERGY_REVENUE(MONTH)
         call write_trace_real4(file_trace_cxat, &
            "4. R_ANNUAL_ENERGY_REVENUE", R_ANNUAL_ENERGY_REVENUE)
            
         R_ANNUAL_VARIABLE_COST = R_ANNUAL_VARIABLE_COST + &
                                      MONTHLY_OP_VARIABLE_COST(MONTH)

      ENDDO ! MONTH
      R_ANNUAL_EMISSION_COST = R_ANNUAL_ENERGY * R_CO2_COST_PER_MWH
      R_ANNUAL_VARIABLE_COST = R_ANNUAL_VARIABLE_COST - &
                                                  R_ANNUAL_EMISSION_COST
!


      RETURN ! CX_ANNUAL_CONTRACT
!
      ENTRY HOURLY_CONTINGENT_MARKET_CAP( &
                                           R_HOUR_IN_DAY, &
                                           R_DAY_IN_WEEK, &
                                           R_DAY_IN_MONTH, &
                                           R_TRANS_GROUP, &
                                           R_HOUR_PRICE, &
                                           R_MONTH, &
                                           R_HOURLY_IMPORT_CAP)
!

!
         HOURLY_CONTINGENT_MARKET_CAP = 0.
         IF(YES_CONTINGENT_MARKET_CAPACITY .AND. &
                                      R_HOURLY_IMPORT_CAP > .0001) THEN
            DO IND = 1, NUM_TRANSACTIONS
               IF(UNIT_CONTING_MARKET_SUPPLEMENT(IND) /= 'T') CYCLE
               IF(CONTINGENT_CAPACITY(R_HOUR_IN_DAY,IND) <= .01 .OR. &
                    (R_HOUR_PRICE < MONTHLY_ENERGY_PRICE(IND) .AND. &
                                    R_HOURLY_IMPORT_CAP > .01 .AND. &
                    CONTINGENT_CAPACITY(R_HOUR_IN_DAY,IND) > .01) ) THEN
                  HOURLY_CONTINGENT_MARKET_CAP = &
                        HOURLY_CONTINGENT_MARKET_CAP + &
                             MIN(R_HOURLY_IMPORT_CAP, &
                                                   HOURLY_QUANTITY(IND))

                  TEMP_QUANTITY = HOURLY_CONTINGENT_MARKET_CAP

! ALL THE CODE BELOW IS TAKEN FROM THE HOURLY OPTION / FORWARD \
! ROUTINE ABOVE.
                  LONG_SHORT = 1.0
!
                  IF(EXPENSE_ASSIGNMENT(IND) == &
                                               LOCAL_ASSIGNMENT(2)) THEN
                     LONG_SHORT = -LONG_SHORT
                  ENDIF

                  IF(PRODUCT_TYPE(IND) == LOCAL_PRODUCT_TYPE(6)) THEN
                     USER_DAY_MULT = &
                               GET_USER_HOUR_IN_DAY(R_HOUR_IN_DAY, &
                                      R_DAY_IN_WEEK, &
                                      R_MONTH, &
                                      USER_DAY_TYPES_ID(IND), &
                                      MONTHLY_CF_TRANS(IND))
                  ELSE
                     USER_DAY_MULT = 1.0
                  ENDIF
!
! 052204
                  TEMP_QUANTITY = TEMP_QUANTITY * &
                                              USER_DAY_MULT * LONG_SHORT
!
                  MONTHLY_ENERGY(IND,R_MONTH) = &
                             MONTHLY_ENERGY(IND,R_MONTH) - TEMP_QUANTITY

                  IF(PRICE_TYPE(IND) == 'I') THEN
! 11/25/03.
                     TEMP_R4 = R_HOUR_PRICE * MONTHLY_ENERGY_MULT(IND)
                     IF(HOUR_IN_MONTH == 1) THEN
                        MONTHLY_ENERGY_PRICE(IND) = TEMP_R4
                     ELSEIF(HOUR_IN_MONTH == HOURS_IN_MONTH) THEN
                        MONTHLY_ENERGY_PRICE(IND) = &
                                 (MONTHLY_ENERGY_PRICE(IND) + TEMP_R4)/ &
                                                   FLOAT(HOURS_IN_MONTH)
                     ELSE
                        MONTHLY_ENERGY_PRICE(IND) = &
                                     MONTHLY_ENERGY_PRICE(IND) + TEMP_R4
                     ENDIF
                  ELSE
                     TEMP_R4 = MONTHLY_ENERGY_PRICE(IND)
                  ENDIF
!
                  IF(EXPENSE_ASSIGNMENT(IND) == &
                                               LOCAL_ASSIGNMENT(1)) THEN
                     MONTHLY_ENERGY_REVENUE(IND,R_MONTH) = &
                                   MONTHLY_ENERGY_REVENUE(IND,R_MONTH) + &
                          TEMP_R4 * ABS(TEMP_QUANTITY)

                     MONTHLY_ENERGY_COST(IND,R_MONTH) = &
                        MONTHLY_ENERGY_COST(IND,R_MONTH) + &
                                     R_HOUR_PRICE * TEMP_R1
                     MONTHLY_ENR_FOR_REV(IND,R_MONTH) = &
                          MONTHLY_ENR_FOR_REV(IND,R_MONTH) + &
                                                      ABS(TEMP_QUANTITY)
                  ELSE
                     MONTHLY_ENERGY_REVENUE(IND,R_MONTH) = &
                                   MONTHLY_ENERGY_REVENUE(IND,R_MONTH) + &
                                     R_HOUR_PRICE * TEMP_R1
                     MONTHLY_ENERGY_COST(IND,R_MONTH) = &
                        MONTHLY_ENERGY_COST(IND,R_MONTH) + &
                          TEMP_R4 * ABS(TEMP_QUANTITY)

                     MONTHLY_ENR_FOR_EXP(IND,R_MONTH) = &
                          MONTHLY_ENR_FOR_EXP(IND,R_MONTH) + &
                                                      ABS(TEMP_QUANTITY)
                  ENDIF
               ENDIF ! VALID MARKET CONTINGENCY
            END DO ! TRANS COUNTER
         ENDIF ! CONTINGENCY ACTIVE
      RETURN

      ENTRY MONTHLY_CALL_PUT_CAPACITY( &
         R_TRANS_GROUP, &
         R_MONTHLY_PRICE,    & !  24x31 hourly detail
         R_MONTHLY_CAPACITY, & !  24x31 hourly detail begins at (1); (0:800)
         R_MONTH, &
         R_DAYS_IN_MONTH, &
         R_MONTHLY_AVAIL)
!

!
         MONTHLY_CALL_PUT_CAPACITY = .FALSE.
         IF(.NOT. SAVE_MONTHLY_PRODUCTS_STATUS) RETURN
         IF(NUM_MONTH_CALLS(R_TRANS_GROUP) + &
                NUM_MONTH_PUTS(R_TRANS_GROUP) <= 0) then
            RETURN
         end if
!
         DOW_PROD_HOURS = 0
         DOW_PROD_APPLIES = 0.
         DO CALENDAR_DAY_OF_WEEK=1,8
         ! prepare the hourly array of product applicability
            DO HOUR_IN_DAY=1,24
               DO PRODUCT=1,5 ! pattern for product==6 varies by IND
                  IF(.TRUE..EQV.APPLY_ENERGY_PRODUCT(HOUR_IN_DAY, &
                     CALENDAR_DAY_OF_WEEK,LOCAL_PRODUCT_TYPE(PRODUCT))) &
                  THEN
                     DOW_PROD_APPLIES(HOUR_IN_DAY, &
                                    CALENDAR_DAY_OF_WEEK,PRODUCT)= 1.0
                     DOW_PROD_HOURS(CALENDAR_DAY_OF_WEEK,PRODUCT)= &
                     DOW_PROD_HOURS(CALENDAR_DAY_OF_WEEK,PRODUCT)+1
                  ENDIF
               END DO
            END DO
         END DO
!
         R_MONTHLY_CAPACITY = 0.
         TEMP_PRICE_24 = 0. ! strike threshold, constant for the month
! MONTHLY CALL LOGIC
         DO I = 1, 2
            N_BEST_VALUES=0
            IF(I == 1) THEN ! CALLS
               LOCAL_ACTIVE_TRANSACTIONS = &
                                          NUM_MONTH_CALLS(R_TRANS_GROUP)
               CALL_OR_PUT = 1.
            ELSE ! PUTS
               LOCAL_ACTIVE_TRANSACTIONS = NUM_MONTH_PUTS(R_TRANS_GROUP)
               CALL_OR_PUT = -1.
            ENDIF
            DO TRANS = 1, LOCAL_ACTIVE_TRANSACTIONS
               IF(I == 1) THEN
                  IND = MONTH_CALL_POSITION(TRANS,R_TRANS_GROUP)
               ELSE
                  IND = MONTH_PUT_POSITION(TRANS,R_TRANS_GROUP)
               ENDIF
!
               HOURLY_ENERGY_PRICE = MONTHLY_ENERGY_PRICE(IND)
!
               IF(STRIKES_AVAILABLE(IND) <= 0) CYCLE


               TEMP_L=(DERIVATIVE_TYPE(IND) < 4)
               DO PRODUCT=1,6 ! find index of applicable product
                  IF(PRODUCT_TYPE(IND)==LOCAL_PRODUCT_TYPE(PRODUCT)) then
					EXIT
				end if
               END DO
               IF(PRODUCT>6) CYCLE
               IF(PRODUCT==6) THEN ! PRODUCT_PATTERN may vary with IND
                  DOW_PROD_APPLIES(:,:,6) = 0.
                  DOW_PROD_HOURS(:,6)= 0
                  DO CALENDAR_DAY_OF_WEEK=1,8
                  ! prepare the hourly array of product applicability
                     month_pass=r_month
                     gud_called_from="3. read_energy_products_data"
                     TEMP_R1 = GET_USER_DAY(CALENDAR_DAY_OF_WEEK, &
                        month_pass, &
                        USER_DAY_TYPES_ID(IND), &
                        PRODUCT_PATTERN(1,PRODUCT), &
                        MONTHLY_CF_TRANS(IND))
                     DO HOUR_IN_DAY=1,24
                        IF(NINT(PRODUCT_PATTERN(HOUR_IN_DAY,PRODUCT))>0) &
                           THEN
                           DOW_PROD_APPLIES(HOUR_IN_DAY, &
                                      CALENDAR_DAY_OF_WEEK,PRODUCT)= 1.0
                          DOW_PROD_HOURS(CALENDAR_DAY_OF_WEEK,PRODUCT)= &
                          DOW_PROD_HOURS(CALENDAR_DAY_OF_WEEK,PRODUCT)+1
                        ENDIF
                     END DO
                  END DO
               END IF
               MONTHLY_ENERGY_REVENUE(IND,R_MONTH) = 0.0
               SELL_OR_BUY= 1.0
               LONG_SHORT = 1.0
!
! 12/21/01. IF SELLING A DERIVATIVE PRODUCT, THEN THE BUYER IS PAYING
! FOR THE OPTIONALITY.
! FOR INSTANCE, SELLING A CALL OPTION MEANS THAT WHEN IT IS CALLED,
! YOU MUST SERVE THE LOAD.

               IF(EXPENSE_ASSIGNMENT(IND) == LOCAL_ASSIGNMENT(1)) &
                  LONG_SHORT = -LONG_SHORT

               HOURS_STRUCK=0
               STRIKE_VALUE=0.0
               SUM_QUANTITY=0.0 ! sum of DAILY_MW across hours and days
               IF(TEMP_L) IND_HRLY_CAPACITY = 0.

               LOCAL_BEGIN_DAY = BEGIN_DAY_IN_MONTH(IND)
               LOCAL_END_DAY = MIN(R_DAYS_IN_MONTH, &
                                                  END_DAY_IN_MONTH(IND))

! CALCULATE DAILY OPTION VALUE

               DO DAY = LOCAL_BEGIN_DAY, LOCAL_END_DAY

! DAILY UNIT CONTINGENT LOGIC
!
! 10/24/01.
                  TEMP_QUANTITY = HOURLY_QUANTITY(IND)
                  DAILY_MW = TEMP_QUANTITY
!
                  IF(UNIT_CONTINGENCY(IND) /= 'N') THEN
!
                     DAILY_MW = 0.
!
                     DO HOUR = 1, 24
                        HOUR_IN_MONTH = (DAY-1)*24+HOUR
!
                        UNIT_AVAIL = 0.0
                        RESOURCE_MW = 0.
                        LOCAL_UNIT_ID = UNIT_CONTINGENT_LINK(IND)
                        IF(LOCAL_UNIT_ID < 0) THEN
                           TEMP_I2 = 1
                           LOCAL_UNIT_ID = &
                              GET_VAR(FLOAT(UNIT_CONTINGENT_LINK(IND)), &
                                         TEMP_I2,'MultiUnit Contingent')
                           DOWHILE(LOCAL_UNIT_ID > 0 .AND. &
                                                          TEMP_I2 <= 30)
                              TEMP_UNIT_NUMBER = &
                                 GET_RESOURCE_ID_TO_UNIT(LOCAL_UNIT_ID, &
                                                 NUMBER_OF_UNITS_FOR_ID)
                              UNIT_AVAIL = UNIT_AVAIL + &
                              GET_BLOCK_AVAIL_4_HOUR(TEMP_UNIT_NUMBER, &
                                                         INT(1,2), &
                                                      HOUR_IN_MONTH) + &
                              GET_BLOCK_AVAIL_4_HOUR(TEMP_UNIT_NUMBER, &
                                                         INT(2,2), &
                                                      HOUR_IN_MONTH)
                              CALL GET_MWBLOK_FOR_UNIT( &
                                       TEMP_UNIT_NUMBER,TEMP_R1,TEMP_R2)
!
                              RESOURCE_MW = &
                                         RESOURCE_MW + TEMP_R1 + TEMP_R2
                              TEMP_I2 = TEMP_I2 + 1
                              LOCAL_UNIT_ID = &
                                 GET_VAR( &
                                       FLOAT(UNIT_CONTINGENT_LINK(IND)), &
                                         TEMP_I2,'MultiUnit Contingent')
                           ENDDO
                        ELSE
                           TEMP_UNIT_NUMBER = &
                                 GET_RESOURCE_ID_TO_UNIT(LOCAL_UNIT_ID, &
                             NUMBER_OF_UNITS_FOR_ID) ! FIRST OCCURRENCE
                           UNIT_AVAIL = UNIT_AVAIL + &
                             GET_BLOCK_AVAIL_4_HOUR(TEMP_UNIT_NUMBER, &
                                                         INT(1,2), &
                                                      HOUR_IN_MONTH) + &
                             GET_BLOCK_AVAIL_4_HOUR(TEMP_UNIT_NUMBER, &
                                                         INT(2,2), &
                                                      HOUR_IN_MONTH)
                           CALL GET_MWBLOK_FOR_UNIT( &
                                       TEMP_UNIT_NUMBER,TEMP_R1,TEMP_R2)
                           RESOURCE_MW = TEMP_R1 + TEMP_R2
                        ENDIF
!
                        IF(RESOURCE_MW > 0.) THEN
                           UNIT_AVAIL = UNIT_AVAIL/RESOURCE_MW
                           IF(UNIT_AVAIL > 1.0) THEN
                              WRITE(4,*) "PROBLEM IN UNIT CONTINGENT"
                           ENDIF
                        ELSE
                           UNIT_AVAIL = 0.0
                        ENDIF
! 11/03/03. NEW CASES FOR REPLACEMENT LOGIC.
                        OUTAGE_TYPE = NINT(GET_BLOCK_OUTAGE_4_HOUR( &
                               TEMP_UNIT_NUMBER,INT(1,2),HOUR_IN_MONTH))
!     +                          TEMP_UNIT_NUMBER,INT2(2),HOUR_IN_MONTH))
                        IF(UNIT_CONTINGENCY(IND) == 'R' .OR. &
                                UNIT_CONTINGENCY(IND) == 'A' .OR. &
                                      UNIT_CONTINGENCY(IND) == 'B') THEN
                           IF( UNIT_CONTINGENCY(IND) == 'R' .OR. &
                               ( (OUTAGE_TYPE == 1 .AND. &
                                      UNIT_CONTINGENCY(IND) == 'A') .OR. &
                                 (OUTAGE_TYPE == 2 .AND. &
                                   UNIT_CONTINGENCY(IND) == 'B') )) THEN
                              DAILY_MW(HOUR) = &
                                       TEMP_QUANTITY * (1. - UNIT_AVAIL)
                           ENDIF
                        ELSE

                           IF( UNIT_CONTINGENCY(IND) == 'U' .OR. &
                               ( (OUTAGE_TYPE == 1 .AND. &
                                      UNIT_CONTINGENCY(IND) == 'M') .OR. &
                                 (OUTAGE_TYPE == 2 .AND. &
                                   UNIT_CONTINGENCY(IND) == 'F') )) THEN
                              DAILY_MW(HOUR) = &
                                              TEMP_QUANTITY * UNIT_AVAIL
                           ENDIF
                        ENDIF
                        R_MONTHLY_AVAIL(HOUR_IN_MONTH) = &
                        R_MONTHLY_AVAIL(HOUR_IN_MONTH) + &
                                             DAILY_MW(HOUR) * LONG_SHORT
                     ENDDO ! HOUR
                  ELSE
                     DO HOUR = 1, 24
                        HOUR_IN_MONTH = (DAY-1)*24+HOUR
                        R_MONTHLY_AVAIL(HOUR_IN_MONTH) = &
                        R_MONTHLY_AVAIL(HOUR_IN_MONTH) + &
                                             DAILY_MW(HOUR) * LONG_SHORT
                     ENDDO
                  ENDIF ! CONTINGENT OPTION
!
                  HOUR_IN_MONTH=1+(DAY-1)*24

                  CALENDAR_DAY_OF_WEEK = GET_DAY_OF_WEEK_4(R_MONTH,DAY)
                  HOURS_STRUCK=HOURS_STRUCK+ &
                     DOW_PROD_HOURS(CALENDAR_DAY_OF_WEEK,PRODUCT)
!                    ! ^^ varies by DAY
                  STRIKE_VALUE = & !  accumulate regardless of sign
                  STRIKE_VALUE+GET_DAILY_OPTION_VALUE( &
                     R_MONTHLY_PRICE(HOUR_IN_MONTH), &
                     DAILY_MW, &
                     DOW_PROD_APPLIES(1,CALENDAR_DAY_OF_WEEK,PRODUCT), &
!     +               SELL_OR_BUY & !      +               SELL_OR_BUY,
                     CALL_OR_PUT, & !  ALTERED 100206.
                     HOURLY_ENERGY_PRICE)

                  DO J = 1, 24
                     SUM_QUANTITY = SUM_QUANTITY + DAILY_MW(J) * &
                        DOW_PROD_APPLIES(J,CALENDAR_DAY_OF_WEEK,PRODUCT)
                  END DO
                  ! use .ASM version for speed
                  IF(TEMP_L) CALL AugmentVectByProductOf(DAILY_MW(1), &
                     DOW_PROD_APPLIES(1,CALENDAR_DAY_OF_WEEK,PRODUCT), &
                     IND_HRLY_CAPACITY(HOUR_IN_MONTH), &
                     NINT(LONG_SHORT*CALL_OR_PUT),24) ! ALTERED 100206.
               END DO ! DAY
!
               IF((STRIKE_VALUE<=0.0).AND.((STRIKES_REQUIRED(IND)==0) &
                  .OR.(N_BEST_VALUES>=LIM_NBV))) then
                    CYCLE ! reject the option
               end if
!              failing the last condition above allows negative STRIKE_VALUEs
!              to be saved to satisfy 'minimum-take' requirements

                  L_DUMMY=ENLIST_THIS_OPT_VALU(INT(0,2),INT(0,2), &
                                                           STRIKE_VALUE)
!
               MONTHLY_PRODUCT_HOURS(IND,R_MONTH) = &
                       MONTHLY_PRODUCT_HOURS(IND,R_MONTH) + HOURS_STRUCK
               MONTHLY_TRANS_HOURS(IND,R_MONTH) = &
                         MONTHLY_TRANS_HOURS(IND,R_MONTH) + HOURS_STRUCK
               MONTHLY_STRIKES(IND,R_MONTH) = &
                                        MONTHLY_STRIKES(IND,R_MONTH) + 1
               STRIKES_AVAILABLE(IND) = STRIKES_AVAILABLE(IND) - 1
               STRIKES_REQUIRED(IND) = STRIKES_REQUIRED(IND) - 1

! 10/13/03. TO BE CONSISTENT WITH CHANGES MADE TO DAILY AND HOURLY ROUTINES.

               LONG_SHORT = LONG_SHORT * CALL_OR_PUT
               MONTHLY_ENERGY(IND,R_MONTH) = &
                             MONTHLY_ENERGY(IND,R_MONTH) + &
                                            LONG_SHORT*ABS(SUM_QUANTITY)
               MONTHLY_ENERGY_COST(IND,R_MONTH) = &
                        MONTHLY_ENERGY_COST(IND,R_MONTH) + &
                               MONTHLY_ENERGY_PRICE(IND) * &
                                            LONG_SHORT*ABS(SUM_QUANTITY)

               IF(TEMP_L) THEN ! PHYSICAL
                  HOUR_IN_MONTH=0
                  ! TODO:  Create augment_accums_by_ihc routine.
                  ! augment accums by IND_HRLY_CAPACITY
                  DO DAY=1, R_DAYS_IN_MONTH
                     DO HOUR_IN_DAY=1,24
                        HOUR_IN_MONTH=HOUR_IN_MONTH+1
                        R_MONTHLY_CAPACITY(HOUR_IN_MONTH) = &
                        R_MONTHLY_CAPACITY(HOUR_IN_MONTH) + &
                        IND_HRLY_CAPACITY (HOUR_IN_MONTH)
                     END DO
                  END DO
               !  MONTHLY_ENERGY_REVENUE(IND,R_MONTH) remains 0
               ELSE
                  DO DAY = 1, R_DAYS_IN_MONTH
                     HOUR_IN_MONTH=1+(DAY-1)*24

                     CALENDAR_DAY_OF_WEEK = GET_DAY_OF_WEEK_4(R_MONTH, &
                                                                    DAY)
                     MONTHLY_ENERGY_REVENUE(IND,R_MONTH) = &
                     MONTHLY_ENERGY_REVENUE(IND,R_MONTH) + &
                        GET_DAILY_OPTION_VALUE( &
                           R_MONTHLY_PRICE(HOUR_IN_MONTH), &
                           DAILY_MW, &
                           DOW_PROD_APPLIES( &
                              1,CALENDAR_DAY_OF_WEEK,PRODUCT), &
                           LONG_SHORT, &
                           TEMP_PRICE_24)
                  ENDDO
               END IF
            END DO ! TRANS
            STRIKES_LISTED(I)=N_BEST_VALUES
            B_MO_VAL_SL(R_MONTH,I)=N_BEST_VALUES
            IF(N_BEST_VALUES>0) L_DUMMY=SORT_BEST_STRIKES(I)
            ! save month's sorted best values for annual review
            DO IBV=1,N_BEST_VALUES
               J=ORG_ORDER(IBV)
               BEST_MO_VAL(R_MONTH,IBV,I)=BEST_VALUES(J,I)
               B_MO_VAL_ID(R_MONTH,IBV,I)=BEST_VAL_ID(J,I)
               B_MO_VAL_HR(R_MONTH,IBV,I)=BEST_VAL_HR(J,I)
            END DO
         END DO ! I-loop over calls, puts
         MONTHLY_CALL_PUT_CAPACITY=.TRUE.
      RETURN ! ENTRY MONTHLY_CALL_PUT_CAPACITY

      ENTRY GET_NUM_ANNUAL_DERIVATIVES(R_TRANS_GROUP)


         IF(.NOT. SAVE_ANNUAL_PRODUCTS_STATUS .OR. &
                  R_TRANS_GROUP > NUM_OF_TRANS_CLASSES) THEN
            GET_NUM_ANNUAL_DERIVATIVES = 0
         ELSE
            GET_NUM_ANNUAL_DERIVATIVES = &
                        NUM_ANNUAL_CALLS(R_TRANS_GROUP) + &
                                         NUM_ANNUAL_PUTS (R_TRANS_GROUP)
         ENDIF
      RETURN
!

      ENTRY GET_ANNUAL_CALL_PUT_CAPACITY(R_HOUR,R_MONTH,R_TRANS_GROUP)

         IF(ALLOCATED(ANNUAL_CAPACITY)) THEN
            GET_ANNUAL_CALL_PUT_CAPACITY = &
                           ANNUAL_CAPACITY(R_HOUR,R_MONTH,R_TRANS_GROUP)
         ELSE
            GET_ANNUAL_CALL_PUT_CAPACITY = 0.
         ENDIF
      RETURN

!
      ENTRY ANNUAL_CALL_PUT_CAPACITY(R_CALENDAR_YEAR,R_TRANS_GROUP)
!
!     +   R_TRANS_GROUP,
!     +   R_MONTHLY_CAPACITY, ! 24x31x12 hourly detail begins at (1); (0:8784)
!     +   R_MONTHLY_AVAIL)    ! 24x31x12 hourly detail begins at (1); (0:8784)
!     +   R_ANNUAL_PRICE,    ! 24x31x12 hourly detail ! CALL EXTERNALLY
!     +   R_DAYS_IN_YEAR,
!

!
         ANNUAL_CALL_PUT_CAPACITY = .FALSE.
         IF(.NOT. SAVE_ANNUAL_PRODUCTS_STATUS) RETURN
         K = R_TRANS_GROUP
         IF(NUM_ANNUAL_CALLS(K) + &
                             NUM_ANNUAL_PUTS (K) <= 0) RETURN

         CALL GET_ANNUAL_PRICES(R_CALENDAR_YEAR,ANNUAL_PRICE,globecom_YEAR)

         R_DAYS_IN_YEAR=365
         IF(MOD(R_CALENDAR_YEAR,INT(4,2))==0) R_DAYS_IN_YEAR=366
         DOW_PROD_HOURS = 0
         DOW_PROD_APPLIES = 0.
         DO CALENDAR_DAY_OF_WEEK=1,8
         ! prepare the hourly array of product applicability
            DO HOUR_IN_DAY=1,24
               DO PRODUCT=1,5 ! pattern for product==6 varies by IND
                  IF(.TRUE..EQV.APPLY_ENERGY_PRODUCT(HOUR_IN_DAY, &
                     CALENDAR_DAY_OF_WEEK,LOCAL_PRODUCT_TYPE(PRODUCT))) &
                  THEN
                     DOW_PROD_APPLIES(HOUR_IN_DAY, &
                                    CALENDAR_DAY_OF_WEEK,PRODUCT)= 1.0
                     DOW_PROD_HOURS(CALENDAR_DAY_OF_WEEK,PRODUCT)= &
                     DOW_PROD_HOURS(CALENDAR_DAY_OF_WEEK,PRODUCT)+1
                  ENDIF
               END DO
            END DO
         END DO
!
! MOVED INTO ANNUAL INITIALIZATION ROUTINE
!
         TEMP_PRICE_24=0. ! strike threshold, constant for the year
!
!
!         DO K = 1, NUM_OF_TRANS_CLASSES
!
!
!            IF(NUM_ANNUAL_CALLS(K) +
!     +                       NUM_ANNUAL_PUTS (K) <= 0) CYCLE
! ANNUAL CALL LOGIC
            DO I = 1, 2
               N_BEST_VALUES=0
               IF(I == 1) THEN ! CALLS
                  LOCAL_ACTIVE_TRANSACTIONS = &
                                         NUM_ANNUAL_CALLS(K)
                  CALL_OR_PUT = 1.
               ELSE ! PUTS
                  LOCAL_ACTIVE_TRANSACTIONS = &
                                          NUM_ANNUAL_PUTS(K)
                  CALL_OR_PUT = -1.
               ENDIF
               DO TRANS = 1, LOCAL_ACTIVE_TRANSACTIONS
                  IF(I == 1) THEN
                     IND = ANNUAL_CALL_POSITION(TRANS,K)
                  ELSE
                     IND = ANNUAL_PUT_POSITION(TRANS,K)
                  ENDIF
                  IF(STRIKES_AVAILABLE(IND) <= 0) CYCLE

             ! AGT would duplicate code from DAILY_CALL_PUT_CAPACITY
             !if applicable to monthly
             ! IF(UNIT_CONTINGENCY(IND) /= 'N') THEN
             ! ... code as in ENTRY DAILY_CALL_PUT_CAPACITY ...
             ! ENDIF ! CONTINGENT OPTION
                  TEMP_L=(DERIVATIVE_TYPE(IND) < 4)
                  DO PRODUCT=1,6 ! find index of applicable product
           IF(PRODUCT_TYPE(IND) == LOCAL_PRODUCT_TYPE(PRODUCT))then
            EXIT
           end if
                  END DO
                  IF(PRODUCT>6) CYCLE
                  IF(PRODUCT==6) THEN
                     DOW_PROD_HOURS(:,6) = 0
                     DOW_PROD_APPLIES(:,:,6) = 0.
                     DO CALENDAR_DAY_OF_WEEK=1,8
                     month_pass=imo
                   gud_called_from="4. read_energy_products_data"
                  ! prepare the hourly array of product applicability
                        TEMP_R1 = GET_USER_DAY(CALENDAR_DAY_OF_WEEK, &
                           month_pass, &
                           USER_DAY_TYPES_ID(IND), &
                           PRODUCT_PATTERN(1,PRODUCT), &
                           MONTHLY_CF_TRANS(IND))
                           imo=month_pass
                        DO HOUR_IN_DAY=1,24
                           IF(NINT(PRODUCT_PATTERN( &
                                           HOUR_IN_DAY,PRODUCT))>0) THEN
                              DOW_PROD_APPLIES(HOUR_IN_DAY, &
                                      CALENDAR_DAY_OF_WEEK,PRODUCT)= 1.0
                              DOW_PROD_HOURS( &
                                          CALENDAR_DAY_OF_WEEK,PRODUCT)= &
                                 DOW_PROD_HOURS( &
                                         CALENDAR_DAY_OF_WEEK,PRODUCT)+1
                           ENDIF
                        END DO
                     END DO
                  END IF
                  ! AGT to GAT:  this is dubious
                  MONTHLY_ENERGY_REVENUE(IND,0) = 0.0
                  SELL_OR_BUY= 1.0
                  LONG_SHORT = 1.0

! 12/21/01. IF SELLING A DERIVATIVE PRODUCT, THEN THE BUYER IS PAYING
! FOR THE OPTIONALITY. FOR INSTANCE, SELLING A CALL OPTION MEANS THAT
! WHEN IT IS CALLED, YOU MUST SERVE THE LOAD.

                  IF(EXPENSE_ASSIGNMENT(IND) == LOCAL_ASSIGNMENT(1)) &
                    SELL_OR_BUY = - SELL_OR_BUY

                  HOURS_STRUCK=0
                  STRIKE_VALUE=0.0
                  SUM_QUANTITY=0.0 ! sum of DAILY_MW across hours and days
                  SUM_QTY_COST=0.0
                  CUR_MONTH_QUANTITY = 0.
                  CUR_MO_ENERGY_PRICE = 0.
                  MONTH_HOURS_STRUCK = 0
                   IND_ANNL_HRLY_CAP = 0.
!
! AGT to GAT:  calls below to
! GET_BLOCK_AVAIL_4_HOUR(TEMP_UNIT_NUMBER,INT2(1),HOUR_IN_MONTH) and
! GET_BLOCK_OUTAGE_4_HOUR(TEMP_UNIT_NUMBER,INT2(2),HOUR_IN_MONTH)
! are evidently month-specific, need to be replaced by something like
! GET_BLOCK_AVAIL_4_HRinYR(TEMP_UNIT_NUMBER,INT2(1),HOUR_IN_YEAR) and
! GET_BLOCK_OUTAGE_4_HRinYR(TEMP_UNIT_NUMBER,INT2(2),HOUR_IN_YEAR)
                  HOUR_IN_YEAR=0
                  do iMo=1,12
                     nDaCMo=DaysInNLYMo(iMo)
!                     if((iMo==2).and.(R_DAYS_IN_YEAR==366)) nDaCMo=29
                     LOCAL_BEGIN_DAY = &
                              MAX(INT(1,2),YEARS_BEG_DAY_IN_MO(IND,iMo))
                     LOCAL_END_DAY = &
                                MIN(nDaCMo,YEARS_END_DAY_IN_MO(IND,iMo))
                     HOUR_IN_MONTH=24*(LOCAL_BEGIN_DAY-1)
                     L_DUMMY= &
                            GET_ESC_NRG_PRICE(R_CALENDAR_YEAR,iMO,IND, &
                                               CUR_MO_ENERGY_PRICE(iMo))
                     CUR_QUANTITY=0.0

                     HOURLY_ENERGY_PRICE = CUR_MO_ENERGY_PRICE(iMo)

! CALCULATE ANNUAL OPTION VALUE

                     DO DAY = LOCAL_BEGIN_DAY, LOCAL_END_DAY

! DAILY UNIT CONTINGENT LOGIC

! 10/24/01.
                        TEMP_QUANTITY = HOURLY_QUANTITY(IND)
                        DAILY_MW = TEMP_QUANTITY

                        DO HOUR = 1, 24
                           HOUR_IN_MONTH=HOUR_IN_MONTH+1
                           IF(UNIT_CONTINGENCY(IND) /= 'N') THEN
                              UNIT_AVAIL = 0.0
                              RESOURCE_MW = 0.
                              LOCAL_UNIT_ID = UNIT_CONTINGENT_LINK(IND)
                              IF(LOCAL_UNIT_ID < 0) THEN
                                 TEMP_I2 = 1
                                 LOCAL_UNIT_ID = &
                               GET_VAR(FLOAT(UNIT_CONTINGENT_LINK(IND)), &
                                         TEMP_I2,'MultiUnit Contingent')
                                 DO WHILE(LOCAL_UNIT_ID > 0 .AND. &
                                                          TEMP_I2 <= 30)
                                    ! FIRST OCCURRENCE
                                    TEMP_UNIT_NUMBER = &
                                  GET_RESOURCE_ID_TO_UNIT(LOCAL_UNIT_ID, &
                                                 NUMBER_OF_UNITS_FOR_ID)

                                    CALL GET_MWBLOK_FOR_UNIT( &
                                       TEMP_UNIT_NUMBER,TEMP_R1,TEMP_R2)
!
                                    RESOURCE_MW = &
                                         RESOURCE_MW + TEMP_R1 + TEMP_R2
                                    TEMP_I2 = TEMP_I2 + 1
                                    LOCAL_UNIT_ID = &
                                      GET_VAR( &
                                       FLOAT(UNIT_CONTINGENT_LINK(IND)), &
                                         TEMP_I2,'MultiUnit Contingent')
                                 END DO
                              ELSE
                              ! FIRST OCCURENCE
                                 TEMP_UNIT_NUMBER = &
                                  GET_RESOURCE_ID_TO_UNIT(LOCAL_UNIT_ID, &
                                                 NUMBER_OF_UNITS_FOR_ID)

                                 CALL GET_MWBLOK_FOR_UNIT( &
                                       TEMP_UNIT_NUMBER,TEMP_R1,TEMP_R2)
                                 RESOURCE_MW = TEMP_R1 + TEMP_R2
                              ENDIF
!
                              IF(RESOURCE_MW > 0.) THEN
                                 UNIT_AVAIL = UNIT_AVAIL/RESOURCE_MW
                                 IF(UNIT_AVAIL > 1.0) THEN
                                         WRITE(4,*) &
                                            "PROBLEM IN UNIT CONTINGENT"
                                 ENDIF
                              ELSE
                                 UNIT_AVAIL = 0.0
                              ENDIF
!
                              IF(UNIT_CONTINGENCY(IND) == 'R') THEN
                                 DAILY_MW(HOUR) = &
                                       TEMP_QUANTITY * (1. - UNIT_AVAIL)
                              ELSE
                                 IF( UNIT_CONTINGENCY(IND) == 'U' .OR. &
                                    ( (OUTAGE_TYPE == 1 .AND. &
                                      UNIT_CONTINGENCY(IND) == 'M') .OR. &
                                    (OUTAGE_TYPE == 2 .AND. &
                                   UNIT_CONTINGENCY(IND) == 'F') )) THEN
                                    DAILY_MW(HOUR) = &
                                              TEMP_QUANTITY * UNIT_AVAIL
                                 ENDIF
                              ENDIF
                           ENDIF ! CONTINGENT OPTION
                           ANNUAL_AVAIL(HOUR_IN_YEAR+HOUR_IN_MONTH) = &
                             ANNUAL_AVAIL(HOUR_IN_YEAR+HOUR_IN_MONTH) + &
                                            DAILY_MW(HOUR) * SELL_OR_BUY
                        ENDDO ! HOUR
                        ! index of first hour this DAY
                        HOUR_IN_MONTH=HOUR_IN_MONTH-23
                        CALENDAR_DAY_OF_WEEK=GET_DAY_OF_WEEK_4(iMo,DAY)
                        HOURS_STRUCK=HOURS_STRUCK+ &
                            DOW_PROD_HOURS(CALENDAR_DAY_OF_WEEK,PRODUCT)
                                                      ! varies by DAY
                        MONTH_HOURS_STRUCK(iMo)=MONTH_HOURS_STRUCK(iMo)+ &
                            DOW_PROD_HOURS(CALENDAR_DAY_OF_WEEK,PRODUCT)
                             ! varies by DAY
                        STRIKE_VALUE = & !  accumulate regardless of sign
                           STRIKE_VALUE+GET_DAILY_OPTION_VALUE( &
                           ANNUAL_PRICE(HOUR_IN_YEAR+HOUR_IN_MONTH), &
                           DAILY_MW, &
                           DOW_PROD_APPLIES(1, &
                                          CALENDAR_DAY_OF_WEEK,PRODUCT), &
                           CALL_OR_PUT, &

                           HOURLY_ENERGY_PRICE)

! 01/03/05. ALWAYS INVOKE.
                        ! use .ASM version for speed
                        CALL AugmentVectByProductOf(DAILY_MW(1), &
                        DOW_PROD_APPLIES(1, &
                                          CALENDAR_DAY_OF_WEEK,PRODUCT), &
                        IND_ANNL_HRLY_CAP(HOUR_IN_YEAR+HOUR_IN_MONTH), &
                        NINT(CALL_OR_PUT*SELL_OR_BUY),24) ! BIG CHANGE
                        DO J = 0, 23
                           CUR_QUANTITY = CUR_QUANTITY + &
                              IND_ANNL_HRLY_CAP( &
                                          HOUR_IN_YEAR+HOUR_IN_MONTH+J)
                        ENDDO

                        ! index of last hour this DAY
                        HOUR_IN_MONTH=HOUR_IN_MONTH+23
                     END DO ! DAYS
                     SUM_QUANTITY=SUM_QUANTITY+CUR_QUANTITY
                     CUR_MONTH_QUANTITY(iMo) = CUR_MONTH_QUANTITY(iMo) + &
                                                            CUR_QUANTITY
                     SUM_QTY_COST=SUM_QTY_COST+CUR_QUANTITY* &
                                                CUR_MO_ENERGY_PRICE(iMo)
                     HOUR_IN_YEAR=HOUR_IN_YEAR+24*nDaCMo
                  END DO ! iMo having accumulated HOURS_STRUCK,SUM_QUANTITY
!
                  IF((STRIKE_VALUE<=0.0).AND.((STRIKES_REQUIRED(IND)==0) &
                                    .OR.(N_BEST_VALUES>=LIM_NBV))) then
                    CYCLE ! reject the option
                  end if

                  L_DUMMY=ENLIST_THIS_OPT_VALU(INT(0,2),INT(0,2), &
                                                           STRIKE_VALUE)

                  DO iMo = 1,12
                     MONTHLY_PRODUCT_HOURS(IND,iMo) = &
                           MONTHLY_PRODUCT_HOURS(IND,iMo) + &
                                                 MONTH_HOURS_STRUCK(iMo)
                     MONTHLY_TRANS_HOURS(IND,iMo) = &
                             MONTHLY_TRANS_HOURS(IND,iMo) + &
                                                 MONTH_HOURS_STRUCK(iMo)
                     MONTHLY_STRIKES(IND,iMo) = &
                                            MONTHLY_STRIKES(IND,iMo) + 1
                     MONTHLY_ENERGY(IND,iMo) = &
                                  MONTHLY_ENERGY(IND,iMo) + &
                                     CUR_MONTH_QUANTITY(iMo)*SELL_OR_BUY
                     MONTHLY_ENERGY_COST(IND,iMo) = &
                       MONTHLY_ENERGY_COST(IND,iMo) + &
                        CUR_MONTH_QUANTITY(iMo)* &
                                    CUR_MO_ENERGY_PRICE(iMo)*SELL_OR_BUY
                  ENDDO

!

                  STRIKES_AVAILABLE(IND) = STRIKES_AVAILABLE(IND) - 1
                  STRIKES_REQUIRED(IND) = STRIKES_REQUIRED(IND) - 1

                  IF(TEMP_L) THEN ! PHYSICAL
                     HOUR_IN_YEAR=0
                     DO iMo = 1, 12
                        nDaCMo=DaysInNLYMo(iMo)
                        LOCAL_BEGIN_DAY = &
                              MAX(INT(1,2),YEARS_BEG_DAY_IN_MO(IND,iMo))
                        LOCAL_END_DAY = &
                                MIN(nDaCMo,YEARS_END_DAY_IN_MO(IND,iMo))
                        DO DAY = LOCAL_BEGIN_DAY, LOCAL_END_DAY
                           HOUR_IN_MONTH=24*(DAY-1)
                           DO iHr=1,24 ! augment accums by IND_ANNL_HRLY_CAP
                              M = iHr + HOUR_IN_MONTH
                              N = M + HOUR_IN_YEAR
                              ANNUAL_CAPACITY(M,iMo,R_TRANS_GROUP) = &
                                ANNUAL_CAPACITY(M,iMo,R_TRANS_GROUP) + &
                                                    IND_ANNL_HRLY_CAP(N)
                           ENDDO
                        ENDDO
                        HOUR_IN_YEAR=HOUR_IN_YEAR+24*nDaCMo
                     END DO
               !  MONTHLY_ENERGY_REVENUE(IND,0) remains 0
                  ELSE
                     iHr=1
                     DO iMo=1,12
                        nDaCMo=DaysInNLYMo(iMo)
                        DO DAY=1,nDaCMo
                         CALENDAR_DAY_OF_WEEK=GET_DAY_OF_WEEK_4(iMo,DAY)
                           MONTHLY_ENERGY_REVENUE(IND,iMo) = &
                                       MONTHLY_ENERGY_REVENUE(IND,iMo) + &
                           GET_DAILY_OPTION_VALUE( &
                              ANNUAL_PRICE(iHr), &
                              DAILY_MW, &
                              DOW_PROD_APPLIES( &
                                 1,CALENDAR_DAY_OF_WEEK,PRODUCT), &
                              SELL_OR_BUY, &
                              TEMP_PRICE_24)

                           iHr=iHr+24
                        END DO ! DAY
                     END DO ! iMo
                  END IF ! PHY OR FIN
               END DO ! TRANS
               STRIKES_LISTED(I)=N_BEST_VALUES
               B_MO_VAL_SL(0,I)=N_BEST_VALUES
               IF(N_BEST_VALUES>0) L_DUMMY=SORT_BEST_STRIKES(I)
               DO IBV=1,N_BEST_VALUES
                ! save year's sorted best values for review
                  J=ORG_ORDER(IBV)
                  BEST_MO_VAL(0,IBV,I)=BEST_VALUES(J,I)
                  B_MO_VAL_ID(0,IBV,I)=BEST_VAL_ID(J,I)
                  B_MO_VAL_HR(0,IBV,I)=BEST_VAL_HR(J,I)
               END DO
            END DO ! I-loop over calls, puts
!         ENDDO ! LOOP FOR TRANSACTION GROUPS
         ANNUAL_CALL_PUT_CAPACITY=.TRUE.
      RETURN ! ENTRY ANNUAL_CALL_PUT_CAPACITY
!

!
      ENTRY ENLIST_THIS_OPT_VALU(R_DIM,R_HID,R_OPT_VALU) &
                                          RESULT(R_ENLIST_THIS_OPT_VALU)

      ! build and maintain a list of the best values for this month
         N_BEST_VALUES=MIN(N_BEST_VALUES+INT(1,2),LIM_NBV)
         IF(N_BEST_VALUES<LIM_NBV) THEN
         ! store the strike value in a list of best values
            BEST_VALUES(N_BEST_VALUES,I)=R_OPT_VALU
            BEST_VAL_HR(N_BEST_VALUES,I)=R_DIM*24+R_HID
            BEST_VAL_ID(N_BEST_VALUES,I)=TRANS ! allows recovery of IND
         ELSE ! conditionally replace the smallest of BEST_VALUES
            I_INF_BV=1
            INF_BV=BEST_VALUES(I_INF_BV,I)
            DO IBV=2,LIM_NBV
               IF(INF_BV>BEST_VALUES(IBV,I)) THEN
               ! retain index and value of the smaller
                  I_INF_BV=IBV
                  INF_BV=BEST_VALUES(I_INF_BV,I)
               ENDIF
            END DO
            IF(INF_BV<R_OPT_VALU) THEN ! replace value & index
               BEST_VALUES(I_INF_BV,I)=R_OPT_VALU
               BEST_VAL_HR(I_INF_BV,I)=R_DIM*24+R_HID
               BEST_VAL_ID(I_INF_BV,I)=TRANS ! allows recovery of IND
            ENDIF
         ENDIF
         R_ENLIST_THIS_OPT_VALU=.TRUE.
      RETURN ! ENTRY ENLIST_THIS_OPT_VALU
!



      ! 1,2 for calls & puts, respectively
      ENTRY SORT_BEST_STRIKES(R_CALL_PUT) &
          RESULT(R_SORT_BEST_STRIKES)

!     sorts N_BEST_VALUES of BEST_VALUES into descending order
         R_SORT_BEST_STRIKES=.FALSE.
         N_BEST_VALUES=STRIKES_LISTED(R_CALL_PUT)
         IF(N_BEST_VALUES<=0) RETURN
         DO J=1,N_BEST_VALUES
            ORG_ORDER(J)=J ! before sorting
         END DO
         TEMP_L1 = .FALSE.
         CALL INDEXED_SORT_ASCENDING(BEST_VALUES(1,R_CALL_PUT), &
            ORG_ORDER,N_BEST_VALUES,TEMP_L1)

         R_SORT_BEST_STRIKES=.TRUE.
      RETURN ! ENTRY SORT_BEST_STRIKES


      !  CALL_PUT arg: 1,2 for calls & puts, respectively
      ENTRY GET_ANNUAL_BEST_STRIKES(R_CALL_PUT) &
                         RESULT(R_GET_ANNUAL_BEST_STRIKES)

!     having sorted monthly N_BEST_VALUES of BEST_VALUES into
!      descending order,
!     pick the best of these across months for the annual list
      R_GET_ANNUAL_BEST_STRIKES=.FALSE.
      DO MO=1,12
         ! index of strikes eligible for annual list
         B_MO_VAL_SE(MO,R_CALL_PUT)=1
      END DO
      B_MO_VAL_SE(0,R_CALL_PUT)=0 ! count of values on annual list
      DO
         SUP_BV=-1.0E-18
         I_SUP_BV=0
         DO MO=1,12
            N_BEST_VALUES=B_MO_VAL_SL(MO,R_CALL_PUT)
            IBV          =B_MO_VAL_SE(MO,R_CALL_PUT)
            IF(IBV<=N_BEST_VALUES) THEN
               IF(SUP_BV<BEST_MO_VAL(MO,IBV,R_CALL_PUT)) THEN
                  SUP_BV=BEST_MO_VAL(MO,IBV,R_CALL_PUT)
                  I_SUP_BV=MO
               END IF
            END IF
         END DO
         MO=I_SUP_BV
         IF(I_SUP_BV==0) EXIT ! all months' values are exhausted
         IBV=B_MO_VAL_SE(MO,R_CALL_PUT)
         ! increment count of items on annual list
         J  =B_MO_VAL_SE( 0,R_CALL_PUT)+1
         ! increment index of strikes eligible for MO
         B_MO_VAL_SE(MO,R_CALL_PUT)=IBV+1
         ! save count of items on annual list
         B_MO_VAL_SE( 0,R_CALL_PUT)=J
         BEST_MO_VAL( 0,J,R_CALL_PUT)=SUP_BV
         B_MO_VAL_ID( 0,J,R_CALL_PUT)=B_MO_VAL_ID(MO,IBV,R_CALL_PUT)
         B_MO_VAL_HR( 0,J,R_CALL_PUT)=B_MO_VAL_HR(MO,IBV,R_CALL_PUT)
         BEST_VAL_MO(   J,R_CALL_PUT)=MO
         IF(J>=LIM_NBV) EXIT
      END DO
      N_BEST_VALUES=B_MO_VAL_SE(0,R_CALL_PUT)

      R_GET_ANNUAL_BEST_STRIKES=.TRUE.
      RETURN ! ENTRY GET_ANNUAL_BEST_STRIKES

      ENTRY GET_DAILY_PUMPING_LIMIT(R_GRX_ID)

         GET_DAILY_PUMPING_LIMIT = 0.0
         IF(R_GRX_ID > 0 .AND. R_GRX_ID <= 10000) THEN
            TRANS = GRX_ID_INDEX(R_GRX_ID)
            IF(TRANS > 0) THEN
               IF(DERIVATIVE_TYPE(TRANS) == 9) THEN
                  GET_DAILY_PUMPING_LIMIT = DAILY_PUMPING_MULT(TRANS)
!                     WRITE(4,*) 'BATTERY ',TRANSACTION_NAME(TRANS),
!     +                  ' INDEX ',TRANS,
!     +                  ' CHARGE CAP ',GET_DAILY_PUMPING_MULT
               ENDIF
            ENDIF
         ENDIF
      RETURN
!

!
      entry DailyOperMoPumpedStorage( &
         R_TRANS_GROUP, &
         !  24x31 hourly detail
         R_MONTHLY_PRICE,    &
         !  24x31 hourly detail begins at (1); (0:800)
         R_MONTHLY_CAPACITY, &
         R_MONTH, &
         R_DAYS_IN_MONTH, &
         R_MONTHLY_AVAIL)

!     LOOP OVER MULTIPLE OCCURRENCES FOR A GIVEN R_TRANS_GROUP

      IF(.NOT. SAVE_MONTHLY_PRODUCTS_STATUS) RETURN
      LOCAL_ACTIVE_TRANSACTIONS = NUM_STORAGE(R_TRANS_GROUP)
      IF(LOCAL_ACTIVE_TRANSACTIONS == 0) RETURN

      DO iDA = 1, R_DAYS_IN_MONTH
         HOUR_IN_MONTH=(iDa-1)*24
         LOCAL_PRICE(HOUR_IN_MONTH+1) = R_MONTHLY_PRICE(HOUR_IN_MONTH+1)
         LOCAL_PRICE(HOUR_IN_MONTH+2) = R_MONTHLY_PRICE(HOUR_IN_MONTH+2)
         LOCAL_PRICE(HOUR_IN_MONTH+3) = R_MONTHLY_PRICE(HOUR_IN_MONTH+3)
         LOCAL_PRICE(HOUR_IN_MONTH+4) = R_MONTHLY_PRICE(HOUR_IN_MONTH+4)
         LOCAL_PRICE(HOUR_IN_MONTH+5) = R_MONTHLY_PRICE(HOUR_IN_MONTH+5)
         LOCAL_PRICE(HOUR_IN_MONTH+6) = R_MONTHLY_PRICE(HOUR_IN_MONTH+6)
         LOCAL_PRICE(HOUR_IN_MONTH+7) = R_MONTHLY_PRICE(HOUR_IN_MONTH+7)
         LOCAL_PRICE(HOUR_IN_MONTH+8) = R_MONTHLY_PRICE(HOUR_IN_MONTH+8)
         LOCAL_PRICE(HOUR_IN_MONTH+9) = &
                                        R_MONTHLY_PRICE(HOUR_IN_MONTH+9)
         LOCAL_PRICE(HOUR_IN_MONTH+10) = &
                                       R_MONTHLY_PRICE(HOUR_IN_MONTH+10)
         LOCAL_PRICE(HOUR_IN_MONTH+11) = &
                                       R_MONTHLY_PRICE(HOUR_IN_MONTH+11)
         LOCAL_PRICE(HOUR_IN_MONTH+12) = &
                                       R_MONTHLY_PRICE(HOUR_IN_MONTH+12)
         LOCAL_PRICE(HOUR_IN_MONTH+13) = &
                                       R_MONTHLY_PRICE(HOUR_IN_MONTH+13)
         LOCAL_PRICE(HOUR_IN_MONTH+14) = &
                                       R_MONTHLY_PRICE(HOUR_IN_MONTH+14)
         LOCAL_PRICE(HOUR_IN_MONTH+15) = &
                                       R_MONTHLY_PRICE(HOUR_IN_MONTH+15)
         LOCAL_PRICE(HOUR_IN_MONTH+16) = &
                                       R_MONTHLY_PRICE(HOUR_IN_MONTH+16)
         LOCAL_PRICE(HOUR_IN_MONTH+17) = &
                                       R_MONTHLY_PRICE(HOUR_IN_MONTH+17)
         LOCAL_PRICE(HOUR_IN_MONTH+18) = &
                                       R_MONTHLY_PRICE(HOUR_IN_MONTH+18)
         LOCAL_PRICE(HOUR_IN_MONTH+19) = &
                                       R_MONTHLY_PRICE(HOUR_IN_MONTH+19)
         LOCAL_PRICE(HOUR_IN_MONTH+20) = &
                                       R_MONTHLY_PRICE(HOUR_IN_MONTH+20)
         LOCAL_PRICE(HOUR_IN_MONTH+21) = &
                                       R_MONTHLY_PRICE(HOUR_IN_MONTH+21)
         LOCAL_PRICE(HOUR_IN_MONTH+22) = &
                                       R_MONTHLY_PRICE(HOUR_IN_MONTH+22)
         LOCAL_PRICE(HOUR_IN_MONTH+23) = &
                                       R_MONTHLY_PRICE(HOUR_IN_MONTH+23)
         LOCAL_PRICE(HOUR_IN_MONTH+24) = &
                                       R_MONTHLY_PRICE(HOUR_IN_MONTH+24)
      ENDDO
!
    ! shift prior day's late-hours prices to same hours in current day
      do iHr=InfHrAvail(1),24
      !  save the nPreMidnHr values of the final day
      !(as if prior to the 1st)
         HOUR_IN_MONTH=(R_DAYS_IN_MONTH-1)*24+iHr
         TEMP_R1=LOCAL_PRICE(HOUR_IN_MONTH)
         LOCAL_PRICE(HOUR_IN_MONTH) = TEMP_R1
         do iDa=2,R_DAYS_IN_MONTH
            HOUR_IN_MONTH=(iDa-1)*24+iHr
            LOCAL_PRICE(HOUR_IN_MONTH)= &
                                       LOCAL_PRICE(HOUR_IN_MONTH-24)
!            R_MONTHLY_PRICE(HOUR_IN_MONTH)=
!     +      R_MONTHLY_PRICE(HOUR_IN_MONTH-24)
         end do

         LOCAL_PRICE(iHr)=TEMP_R1

      end do ! iHR
!
      DO TRANS = 1, LOCAL_ACTIVE_TRANSACTIONS
         IND = STORAGE_POSITION(TRANS,R_TRANS_GROUP)
         ! COUNTS GENERATION HOURS, INCLUDING FRACTIONAL HOURS
         TEMP_R1 = 0.
         CapyMW(0) = HOURLY_QUANTITY(IND)
         CapyMW(1) = PUMPING_CAPACITY(IND)
         PmpStoEff = PUMPING_STORAGE_EFFICIENCY(IND)
         LimHrsGen = DAILY_PUMPING_MULT(IND)
         PmpStoVOM = MONTHLY_ENERGY_PRICE(IND)
!
       ! time balance:
       !    PmpStoEff*CapyMW(1)*LimHrsUse(1)=CapyMW(0)*LimHrsUse(0)
       ! 0=>generation, 1=>pumping energy required
         LimHrsUse(0)=LimHrsGen
         LimHrsUse(1)=(CapyMW(0)*LimHrsUse(0))/(PmpStoEff*CapyMW(1))
         TimeRatio(0)=LimHrsUse(0)/LimHrsUse(1)
         TimeRatio(1)=LimHrsUse(1)/LimHrsUse(0)
         nHrAvail(0)=SupHrAvail(0)-InfHrAvail(0)+1
         nHrAvail(1)=SupHrAvail(1)+nPreMidnHr
         do iDa=1,R_DAYS_IN_MONTH
            Rank(0)=0
            Rank(1)=0
            do jHr=InfHrAvail(1),SupHrAvail(0)+24
                ! place pre-midnight hours first
               iHr=1+mod(jHr-1,24)

               iGP=1
               if((InfHrAvail(0)<=iHr).and.(iHr<=SupHrAvail(0))) iGP=0
               ! count of hours available for generation/pumping
               Rank(iGP)=Rank(iGP)+1
               GpHrRanked(Rank(iGP),iGP)=Rank(iGP)
               HOUR_IN_MONTH = (iDa-1) * 24 + iHr

               if(iGP == 1) then
                  CurHrCost(Rank(iGP),iGP) = LOCAL_PRICE(HOUR_IN_MONTH) &
                    + PmpStoVOM ! added 20080924 per GAT request
               else
                  CurHrCost(Rank(iGP),iGP) = LOCAL_PRICE(HOUR_IN_MONTH)
               endif
            end do

            do iGP=0,1 ! 0=>generation, 1=>pumping energy required
               TEMP_L1 = iGP == 1
               call INDEXED_SORT_ASCENDING( &
                  CurHrCost(1,iGP),GpHrRanked(1,iGP), &
                  nHrAvail(iGP),TEMP_L1)

               nHrs(iGP)=0.0

               FracHrCurCost(iGP)=0.0
               Rank(iGP)=1
               GroupHr(iGP)=GpHrRanked(Rank(iGP),iGP)
            end do
            if(PrtDetail>1) then

               write(4,'(2a)') &
                  ' Da G R HG HP CostG CostP nGHS nPHS InGH', &
                  ' InPH FrGH FrPH #GHr #PHr GMWH PMWH'
               write(4,'(2a)') &
                  ' -- - - -- -- ----- ----- ---- ---- ----', &
                  ' ---- ---- ---- ---- ---- ---- ----'
            end if
            do
               Economical=CurHrCost(GroupHr(1),1)< &
                  PmpStoEff*CurHrCost(GroupHr(0),0)
               Feasible=.true.
               do iGP=0,1
                  nHrsSlack(iGP)=LimHrsUse(iGP)-nHrs(iGP)
                  Feasible=Feasible .and. (nHrsSlack(iGP)>0.0)
               end do
               if(.not.(Economical.and.Feasible)) then

                  exit
               end if
               do iGP=0,1
                  ! increment in hours at current costs
                  IncrHrCurCost(iGP)= &
                     amin1(1.0-FracHrCurCost(iGP), &
                     amin1(1.0,nHrsSlack(iGP)))
                  if(iGP==0) then
                     ChronHr(iGP)=GroupHr(iGP)+InfHrAvail(0)-1
                  else
                     if(GroupHr(iGP)>nPreMidnHr) then
                     ! early-morning hours
                        ChronHr(iGP)=GroupHr(iGP)-nPreMidnHr
                     else
                        ! pre-midnight hours
                        ChronHr(iGP)=GroupHr(iGP)+InfHrAvail(1)-1
                     end if
                  end if
               end do ! this loop must complete before the next is begun

               do iGP=0,1
                  TimeConstraint = &
                     IncrHrCurCost(mod(iGP+1,2))*TimeRatio(iGP)
                  if(IncrHrCurCost(iGP)>TimeConstraint) &
                     IncrHrCurCost(iGP)=TimeConstraint
                  FracHrCurCost(iGP) = &
                     FracHrCurCost(iGP)+IncrHrCurCost(iGP) ! at most 1.0

                  nHrs(iGP)=nHrs(iGP)+IncrHrCurCost(iGP)
                  GpCapyProrated=CapyMW(iGP)*IncrHrCurCost(iGP)
                  GpHourPrice=CurHrCost(GroupHr(iGP),iGP)

                  HOUR_IN_MONTH = (iDa-1) * 24 + ChronHr(iGP)
                  IF(iGP == 1) THEN ! for pumping energy

                     GpCapyProrated = -GpCapyProrated
                  ELSE

                     TEMP_R1 = TEMP_R1 + IncrHrCurCost(iGP)

                     MONTHLY_ENERGY(IND,R_MONTH) = &
                        MONTHLY_ENERGY(IND,R_MONTH) + GpCapyProrated
                  ENDIF
                  R_MONTHLY_CAPACITY(HOUR_IN_MONTH) = &
                  R_MONTHLY_CAPACITY(HOUR_IN_MONTH) + GpCapyProrated
               end do ! iGP

               do iGP=0,1
                  if(FracHrCurCost(iGP)>0.999999) then
                     ! advance to next hour of possible gen/pmp
                     Rank(iGP)=Rank(iGP)+1
                     GroupHr(iGP)=GpHrRanked(Rank(iGP),iGP)
                     FracHrCurCost(iGP)=0.0
                  end if
               end do
            end do ! while (Economical.and.Feasible)

         end do ! iDa loop

                                           ! generation only
         MONTHLY_TRANS_HOURS(IND,R_MONTH) = NINT(TEMP_R1)
         MONTHLY_PRODUCT_HOURS(IND,R_MONTH) = &
         MONTHLY_PRODUCT_HOURS(IND,R_MONTH) + R_DAYS_IN_MONTH * &
            nint(LimHrsUse(0)+LimHrsUse(1)) ! not always 15
      END DO ! TRANSACTIONS

    ! shift each day's late-hours capacity to same hours in prior day
    ! of month
      do iHr=InfHrAvail(1),24
      !  save the nPreMidnHr values of the first day
!        HOUR_IN_MONTH=(iDa-1)*24+iHr ! (iDa==1) => HOUR_IN_MONTH=iHr
         HOUR_IN_MONTH=iHr
         TEMP_R1=R_MONTHLY_CAPACITY(iHr)
         do iDa=2,R_DAYS_IN_MONTH
             HOUR_IN_MONTH=HOUR_IN_MONTH+24
             R_MONTHLY_CAPACITY(HOUR_IN_MONTH-24)= &
             R_MONTHLY_CAPACITY(HOUR_IN_MONTH)
         end do
      !  move the first day's nPreMidnHr values to iDa==R_DAYS_IN_MONTH
         HOUR_IN_MONTH=HOUR_IN_MONTH+24
         R_MONTHLY_CAPACITY(HOUR_IN_MONTH)=TEMP_R1
      end do ! iHr
      DailyOperMoPumpedStorage = .TRUE.
      return ! entry DailyOperMoPumpedStorage

      ENTRY CONTRACT_IS_STORAGE(R_TRANS)

         CONTRACT_IS_STORAGE =   DERIVATIVE_TYPE(R_TRANS) == 9
      RETURN



      entry CX_DailyOperAnPS2( &
                                          R_TRANS, &
                                          R_ANNUAL_PRICE, & !  8784
                                          R_ANNUAL_USAGE, & !  8784
                                          R_ANNUAL_ENERGY, &
                                          R_ANNUAL_CHARGE, &
                                          R_ANNUAL_ENERGY_REVENUE, &
                                          R_ANNUAL_VARIABLE_COST, &
                                          R_ANNUAL_HYBRID_MW_PUMP, &
                                          R_ANNUAL_HYBRID_MW_GEN, &
                                          R_ANNUAL_HYBRID_HR_MARGIN, &
                                          R_ANNUAL_HYBRID_PROFIT)
      if(file_trace_cxdo==0) then
        file_trace_cxdo=open_trace("cx_daily_operAnPS2.trace", rq_cxdo)
        
      end if
      if (trim(cx_called_from)=="") then
        stop "cx_called_from not set."
      end if
      call write_trace_string(file_trace_cxdo, "Called from", &
        cx_called_from)
      

      IF(.NOT. SAVE_MONTHLY_PRODUCTS_STATUS) RETURN
      IF(LOCAL_ACTIVE_TRANSACTIONS == 0) RETURN
      call write_trace_real4s(file_trace_cxdo, "annual price", &
        R_ANNUAL_PRICE(1:8784))
        
      LOCAL_ANN_PRICE(1:8784) = R_ANNUAL_PRICE(1:8784) ! 8784 HOURS
      R_ANNUAL_ENERGY = 0.0
     call write_trace_message(file_trace_cxdo, "ANNUAL ENERGY to zero")
      R_ANNUAL_CHARGE = 0.0
      R_ANNUAL_ENERGY_REVENUE = 0.0
      R_ANNUAL_VARIABLE_COST = 0.0
      R_ANNUAL_HYBRID_PROFIT = 0.0
      IND = R_TRANS ! SINGLE STORAGE EVALUATION
      MONTH = 0
      DAY_IN_MONTH = 0
      nDaCMo = 0
         do iDa=1, 365
            IF(iDa > nDaCMo) THEN
               DAY_IN_MONTH = 1
               MONTH = MONTH + 1
               nDaCMo = nDaCMo + DAYS_IN_EACH_MONTH(MONTH)
!
               IF(ENERGY_PRICE_MULTIPLIER(IND) < 0.) THEN
!
                  TEMP_R1 = 1.0
                  TEMP_I2 = INT(ABS(ENERGY_PRICE_MULTIPLIER(IND)),2)
                  MONTHLY_ENERGY_MULT(IND) = &
                  ESCALATED_MONTHLY_VALUE(TEMP_R1,TEMP_I2,globecom_YEAR, &
                                                       MONTH,INT(1,2))
!
               ELSE
!
                  MONTHLY_ENERGY_MULT(IND) = &
                                          ENERGY_PRICE_MULTIPLIER(IND)
!
               ENDIF
               IF(ENERGY_PRICE(IND) < 0.0) THEN
                  TEMP_R4 = ABS(ENERGY_PRICE(IND))
                  TEMP_I2 = INT(ABS(ENERGY_PRICE(IND)),2)
                  MONTHLY_ENERGY_PRICE(IND) = &
                     ESCALATED_MONTHLY_VALUE(TEMP_R4, &
                                       TEMP_I2, &
                                       globecom_YEAR, &
                                       MONTH,INT(1,2))
               ELSE
                  MONTHLY_ENERGY_PRICE(IND) = ENERGY_PRICE(IND)
               ENDIF
               MONTHLY_ENERGY_PRICE(IND) = &
                               MONTHLY_ENERGY_PRICE(IND) * &
                                          MONTHLY_ENERGY_MULT(IND)
            ELSE
               DAY_IN_MONTH = DAY_IN_MONTH + 1
            ENDIF

! 051017. NEW STORAGE ALGORITHM THAT ALLOWS ANY HOUR TO PUMP OR GEN.
!         ALSO TRACKS HOURLY STORAGE MWH/HR AS A LIMIT.
!         ONE DAY CYCLING FROM 11 PM TO 10 PM

            HOUR_IN_YEAR=(iDa-1)*24+1

! 121319. REDUCE AVAILABLE PUMPING CAPACITY TO MIN OF PUMPING CAPACITY
!         OR THE HYBRID GENERATION

!            HIGH_CAP = PROPOSED_QUANT_OF_PRODUCT(IND)
! 011220.
            HOUR_END_OF_MONTH = nDaCMo*24 - 2

            DO I = 0, 23
               IF(DAY_IN_MONTH == 1 .AND. I < 2) THEN
                  ONE_DAY_PRICE(I+1) = &
                                    LOCAL_ANN_PRICE(HOUR_END_OF_MONTH+I)
               ELSE
                  ONE_DAY_PRICE(I+1) = LOCAL_ANN_PRICE(HOUR_IN_YEAR+I-2)
               ENDIF
!               ONE_DAY_PRICE(I+1) = LOCAL_ANN_PRICE(HOUR_IN_YEAR+I)
               PRICE_ORDER(I+1) = I + 1
            ENDDO
            call INDEXED_SORT_ASCENDING( &
                  ONE_DAY_PRICE,PRICE_ORDER, &
                  I,TEMP_L1) ! I = 24

            DO I = 1, 12
               HIGH_HOUR(I) = PRICE_ORDER(I)
               LOW_HOUR(I) = PRICE_ORDER(25-I)
               CHARGE(HIGH_HOUR(I)) = 0
               CHARGE(LOW_HOUR(I)) = 1
               IF(DAY_IN_MONTH == 1 .AND. LOW_HOUR(I) < 3) THEN
                  LOW_CAP(I) = MIN(PUMPING_CAPACITY(IND), &
                    R_ANNUAL_HYBRID_MW_PUMP( &
                                       HOUR_END_OF_MONTH+LOW_HOUR(I)-1))
               ELSE
                  LOW_CAP(I) = MIN(PUMPING_CAPACITY(IND), &
                    R_ANNUAL_HYBRID_MW_PUMP(HOUR_IN_YEAR+LOW_HOUR(I)-3))
               ENDIF
               IF(DAY_IN_MONTH == 1 .AND. HIGH_HOUR(I) < 3) THEN
                  HIGH_CAP(I) = MIN(PROPOSED_QUANT_OF_PRODUCT(IND), &
                    R_ANNUAL_HYBRID_MW_GEN( &
                                       HOUR_END_OF_MONTH+LOW_HOUR(I)-1))
               ELSE
                  HIGH_CAP(I) = MIN(PROPOSED_QUANT_OF_PRODUCT(IND), &
                    R_ANNUAL_HYBRID_MW_GEN(HOUR_IN_YEAR+HIGH_HOUR(I)-3))
               ENDIF
            ENDDO
            K = 0
            MARGIN = -99999.0
            MARGIN_ORDER = 0
            DO I = 1, 12
               DO J = 1, 12
                  IF(LOW_HOUR(J) > HIGH_HOUR(I) .OR. &
                        (ONE_DAY_PRICE(LOW_HOUR(J)) + &
                                         MONTHLY_ENERGY_PRICE(IND))/ &
                               PUMPING_STORAGE_EFFICIENCY(IND) > &
                                      ONE_DAY_PRICE(HIGH_HOUR(I))) CYCLE
                  K = K + 1

! 062017.
                  MARGIN(K) = ONE_DAY_PRICE(HIGH_HOUR(I)) - &
                            (ONE_DAY_PRICE(LOW_HOUR(J)) + &
                                              MONTHLY_ENERGY_PRICE(IND))
                  MARGIN_ORDER(K) = K
                  HIGH_HR_MARGIN(K)= I
                  LOW_HR_MARGIN(K)= J
               ENDDO
               TOTAL_COMBO = K
            ENDDO
            call INDEXED_SORT_ASCENDING( &
                  MARGIN,MARGIN_ORDER, &
                  TOTAL_COMBO,TEMP_L1) ! K IS THE LAST VALID DISCHARGE/CHARGE PAIR

            HOUR_STORAGE = 0.0
            IF(PUMPING_STORAGE_EFFICIENCY(IND) <= 0.0) THEN
               WRITE(4,*) 'ZERO STORAGE EFFICIENCY'
               WRITE(4,*) 'STORAGE RESOURCE NAME ', &
                         TRIM(TRANSACTION_NAME(IND))," ", &
                                            TRIM(COUNTERPARTY_NAME(IND))
               er_message='Stop requested from TRANSOBJ2 SIID324'
               call end_program(er_message)
            ENDIF
            TOTAL_STORAGE = 0.0
            PATTERN = 0.0
            PATTERN_MARGIN = 0.0
!
! 06-26-2021. REVISED: 07-24-2021 PER DIANE.
!
            LocalDailyDischargeLimit = &
                     DailyStorageDischargeLimit(IND) * &
                                       PROPOSED_QUANT_OF_PRODUCT(IND)
!
            DO L = 1, TOTAL_COMBO
               K = MARGIN_ORDER(L)
               I = HIGH_HR_MARGIN(K)
               J = LOW_HR_MARGIN(K)
               IF(HIGH_CAP(I) > 0.0001 .AND. &
                                               LOW_CAP(J) > 0.0001) THEN
                  TEST_CAP = MIN(HIGH_CAP(I), &
                             LOW_CAP(J)*PUMPING_STORAGE_EFFICIENCY(IND), &
                                               LocalDailyDischargeLimit)

! TEST FOR STORAGE CAP
!
                  DO M = LOW_HOUR(J), HIGH_HOUR(I)-1
                     TEST_CAP = MIN(TEST_CAP, &
                         (DAILY_PUMPING_MULT(IND) + TOTAL_STORAGE(M)) * &
                                       PUMPING_STORAGE_EFFICIENCY(IND) )

                  ENDDO
                  IF(TEST_CAP <= 0.0) CYCLE
                  LocalDailyDischargeLimit = &
                            MAX(0.0,LocalDailyDischargeLimit - TEST_CAP)
                  DO M = LOW_HOUR(J), HIGH_HOUR(I)-1
                     TOTAL_STORAGE(M) = TOTAL_STORAGE(M) - TEST_CAP/ &
                                         PUMPING_STORAGE_EFFICIENCY(IND)
                  ENDDO
!
                  HIGH_CAP(I) = HIGH_CAP(I) - TEST_CAP
                  LOW_CAP(J) = LOW_CAP(J) - TEST_CAP/ &
                                         PUMPING_STORAGE_EFFICIENCY(IND)
                  PATTERN(HIGH_HOUR(I)) = PATTERN(HIGH_HOUR(I)) + &
                                                                TEST_CAP
                  PATTERN(LOW_HOUR(J)) = PATTERN(LOW_HOUR(J)) - &
                                                    TEST_CAP/ &
                                         PUMPING_STORAGE_EFFICIENCY(IND)
                  PATTERN_MARGIN(HIGH_HOUR(I)) = &
                                    PATTERN_MARGIN(HIGH_HOUR(I)) + &
                                                    TEST_CAP * MARGIN(K)
               ENDIF
            ENDDO
            DO I = 0, 23
               IF(DAY_IN_MONTH == 1 .AND. I < 2) THEN

                  TEMP_I2 = HOUR_END_OF_MONTH+I
               ELSE

                  TEMP_I2 = HOUR_IN_YEAR+I-2
               ENDIF
               R_ANNUAL_USAGE(TEMP_I2) = PATTERN(I+1)
               IF(PATTERN(I+1) > 0.0) THEN
                  R_ANNUAL_HYBRID_HR_MARGIN(TEMP_I2) = &
                                      PATTERN_MARGIN(I+1) / PATTERN(I+1)
               ENDIF
               IF(CHARGE(I+1) == 0) THEN
                  IF(PATTERN(I+1) < 0) THEN
                     TEMP_I2 = TEMP_I2
                  ENDIF
                  
                  R_ANNUAL_ENERGY = R_ANNUAL_ENERGY + &
                                                 R_ANNUAL_USAGE(TEMP_I2)
      call write_trace_real4(file_trace_cxdo, "ANN. ENRG", &
        R_ANNUAL_ENERGY)
        
                  R_ANNUAL_ENERGY_REVENUE = R_ANNUAL_ENERGY_REVENUE + &
                                              LOCAL_ANN_PRICE(TEMP_I2) * &
                                                 R_ANNUAL_USAGE(TEMP_I2)
         call write_trace_real4(file_trace_cxdo, &
            "2. R_ANNUAL_ENERGY_REVENUE", R_ANNUAL_ENERGY_REVENUE)
                  R_ANNUAL_HYBRID_MW_GEN(TEMP_I2) = &
                                            R_ANNUAL_USAGE(TEMP_I2)
                  R_ANNUAL_HYBRID_PROFIT(TEMP_I2) = &
                                            R_ANNUAL_USAGE(TEMP_I2) * &
                                                LOCAL_ANN_PRICE(TEMP_I2)
               ELSE
! R_ANNUAL_VARIABLE_COST = HYBRID LOST REVENUES
                  R_ANNUAL_VARIABLE_COST = R_ANNUAL_VARIABLE_COST &
                           - (LOCAL_ANN_PRICE(TEMP_I2) + &
                                       MONTHLY_ENERGY_PRICE(IND)) * &
                                                 R_ANNUAL_USAGE(TEMP_I2)
                  R_ANNUAL_CHARGE = R_ANNUAL_CHARGE + &
                                                 R_ANNUAL_USAGE(TEMP_I2)
               ENDIF
            ENDDO
         ENDDO ! DAY
      RETURN

      ENTRY ANNUAL_STORAGE_SORT(R_TRANS, &
                                R_ANNUAL_HYBRID_MARKET_ENERGY, &
                                R_MARKET_MW_USAGE, &
                                R_CONSTRAINED_MW_USAGE, &
                                R_ANNUAL_HYBRID_HR_MARGIN, &
                                R_ANNUAL_USAGE)
         if(file_trace_ass==0) then
           file_trace_ass=open_trace("annual_storage_sort.trace", rq_ass)
         end if
         if(file_trace_ass/=BAD_TRACE_HANDLE) then
            write(file_trace_ass,*) "Arguments: ", &
              "R_TRANS ", R_TRANS, &
        "R_ANNUAL_HYBRID_MARKET_ENERGY ", R_ANNUAL_HYBRID_MARKET_ENERGY, &
        "R_MARKET_MW_USAGE ", R_MARKET_MW_USAGE, &
        "R_CONSTRAINED_MW_USAGE ", R_CONSTRAINED_MW_USAGE, &
        "R_ANNUAL_HYBRID_HR_MARGIN ", R_ANNUAL_HYBRID_HR_MARGIN, &
        "R_ANNUAL_USAGE ", R_ANNUAL_USAGE
        
         end if
         
         ANNUAL_STORAGE_SORT = .TRUE.
         R_ANNUAL_USAGE = 0.0
         J = 8784
         TEMP_L1 = .FALSE.
         DO I = 1, J
            ANNUAL_ORDER(I) = I
         ENDDO
         call INDEXED_SORT_ASCENDING( &
                  R_ANNUAL_HYBRID_HR_MARGIN, &
                  ANNUAL_ORDER, &
                  J, &
                  TEMP_L1)

! 030720. INPUT MAXIMUM ENERGY AVAILABLE TO CHARGE FROM MARKET.
!
         MARKET_DISCHARGE_LIMIT = R_ANNUAL_HYBRID_MARKET_ENERGY * &
                                     PUMPING_STORAGE_EFFICIENCY(R_TRANS)
!
         TEMP_R4 = 0.0 ! ACCUMULATE TO HOURLY LIMIT.

         DO I = 1, J
            K = ANNUAL_ORDER(I)

            TEST_CAP = MIN(MARKET_DISCHARGE_LIMIT - TEMP_R4, &
                         R_MARKET_MW_USAGE(K)-R_CONSTRAINED_MW_USAGE(K))
            TEMP_R4 = TEMP_R4 + TEST_CAP
            R_ANNUAL_USAGE(K) = TEST_CAP
            IF(ABS(MARKET_DISCHARGE_LIMIT - TEMP_R4) < 0.01) EXIT

         ENDDO
         TEMP_R4 = TEMP_R4
      RETURN


      entry CX_DailyOperAnPumpedStorage( &
                                          R_TRANS, &
                                          R_ANNUAL_PRICE, & !  8784
                                          R_ANNUAL_USAGE, & !  8784
                                          R_ANNUAL_ENERGY, &
                                          R_ANNUAL_ENERGY_REVENUE, &
                                          R_ANNUAL_VARIABLE_COST)


!     LOOP OVER MULTIPLE OCCURRENCES FOR A GIVEN R_TRANS_GROUP

      IF(.NOT. SAVE_MONTHLY_PRODUCTS_STATUS) RETURN

      IF(LOCAL_ACTIVE_TRANSACTIONS == 0) RETURN


! !!! NEED TO DO DAY SHIFT FOR ALL 365 DAYS !!!
! !!! NEED TO DO LOCAL HOUR SHIFT WITHOUT WIPING OUT VALUES =>
!                      !!! PUT INTO TEMP VALUES TO AVOID WIPE-OUT !!!

      LOCAL_ANN_PRICE(1:8784) = R_ANNUAL_PRICE(1:8784) ! 8784 HOURS
      R_ANNUAL_ENERGY = 0.0
      R_ANNUAL_ENERGY_REVENUE = 0.0
      R_ANNUAL_VARIABLE_COST = 0.0

    ! shift prior day's late-hours prices to same hours in current day

      do iHr=InfHrAvail(1),24 ! HOURS 23 AND 24

!  save the nPreMidnHr values of the final day (as if prior to the 1st)

         HOUR_IN_YEAR = (365-1)*24+iHr
         TEMP_R1=LOCAL_PRICE(HOUR_IN_YEAR)
         LOCAL_ANN_PRICE(HOUR_IN_YEAR) = TEMP_R1

         LAST_HOUR_PRICE = LOCAL_ANN_PRICE(iHr)
         do iDa=2, 365
            HOUR_IN_YEAR=(iDa-1)*24+iHr
            CURRENT_HOUR_PRICE = LOCAL_ANN_PRICE(HOUR_IN_YEAR)
            LOCAL_ANN_PRICE(HOUR_IN_YEAR)= LAST_HOUR_PRICE
            LAST_HOUR_PRICE = CURRENT_HOUR_PRICE

         end do
      !  move the last day's nPreMidnHr values to iDa==1
         LOCAL_ANN_PRICE(iHr)=TEMP_R1
      end do ! iHR

         IND = R_TRANS ! SINGLE STORAGE EVALUATION
         ! COUNTS GENERATION HOURS, INCLUDING FRACTIONAL HOURS
         TEMP_R1 = 0.

          ! TO MANAGE ITERATIONS
         CapyMW(0) = PROPOSED_QUANT_OF_PRODUCT(IND)

         CapyMW(1) = PUMPING_CAPACITY(IND)
         PmpStoEff = PUMPING_STORAGE_EFFICIENCY(IND)
         LimHrsGen = DAILY_PUMPING_MULT(IND)

         PmpStoVOM = 0.0 ! ASSUME ZERO FOR NOW.

! time balance:  PmpStoEff*CapyMW(1)*LimHrsUse(1)=CapyMW(0)*LimHrsUse(0)
! 0=>generation, 1=>pumping energy required
! 050617. if charge mwh then transform
            if(CapyMW(0) > 0.000001 .and. use_charge_mwh) then
               LimHrsUse(0)= LimHrsGen * PmpStoEff /CapyMW(0)
            else
               LimHrsUse(0)=LimHrsGen
            endif


         LimHrsUse(1)=(CapyMW(0)*LimHrsUse(0))/(PmpStoEff*CapyMW(1))
         TimeRatio(0)=LimHrsUse(0)/LimHrsUse(1)
         TimeRatio(1)=LimHrsUse(1)/LimHrsUse(0)
         nHrAvail(0)=SupHrAvail(0)-InfHrAvail(0)+1
         nHrAvail(1)=SupHrAvail(1)+nPreMidnHr

         MONTH = 0
         nDaCMo = 0
         do iDa=1, 365
            IF(iDa > nDaCMo) THEN
               MONTH = MONTH + 1
               nDaCMo = nDaCMo + DAYS_IN_EACH_MONTH(MONTH)
!
               IF(ENERGY_PRICE_MULTIPLIER(IND) < 0.) THEN
!
                  TEMP_R1 = 1.0
                  TEMP_I2 = INT(ABS(ENERGY_PRICE_MULTIPLIER(IND)),2)
                  MONTHLY_ENERGY_MULT(IND) = &
                  ESCALATED_MONTHLY_VALUE(TEMP_R1,TEMP_I2,globecom_YEAR, &
                                                       MONTH,INT(1,2))
!
               ELSE
!
                  MONTHLY_ENERGY_MULT(IND) = &
                                          ENERGY_PRICE_MULTIPLIER(IND)
!
               ENDIF
               IF(ENERGY_PRICE(IND) < 0.0) THEN
                  TEMP_R4 = ABS(ENERGY_PRICE(IND))
                  TEMP_I2 = INT(ABS(ENERGY_PRICE(IND)),2)
                  MONTHLY_ENERGY_PRICE(IND) = &
                     ESCALATED_MONTHLY_VALUE(TEMP_R4, &
                                       TEMP_I2, &
                                       globecom_YEAR, &
                                       MONTH,INT(1,2))
               ELSE
                  MONTHLY_ENERGY_PRICE(IND) = ENERGY_PRICE(IND)
               ENDIF
               MONTHLY_ENERGY_PRICE(IND) = &
                               MONTHLY_ENERGY_PRICE(IND) * &
                                          MONTHLY_ENERGY_MULT(IND)
            ENDIF
!
            Rank(0)=0
            Rank(1)=0
            ! place pre-midnight hours first
            do jHr=InfHrAvail(1),SupHrAvail(0)+24
               iHr=1+mod(jHr-1,24)

               iGP=1
               if((InfHrAvail(0)<=iHr).and.(iHr<=SupHrAvail(0))) iGP=0
               ! count of hours available for generation/pumping
               Rank(iGP)=Rank(iGP)+1
               GpHrRanked(Rank(iGP),iGP)=Rank(iGP)
!               HOUR_IN_MONTH = (iDa-1) * 24 + iHr
               HOUR_IN_YEAR = (iDa-1) * 24 + iHR
               if(iGP == 1) then
                  CurHrCost(Rank(iGP),iGP) = &
                                           LOCAL_ANN_PRICE(HOUR_IN_YEAR) &

                    + PmpStoVOM ! added 20080924 per GAT request
               else
                  CurHrCost(Rank(iGP),iGP) = &
                                           LOCAL_ANN_PRICE(HOUR_IN_YEAR)
!                  CurHrCost(Rank(iGP),iGP) = LOCAL_PRICE(HOUR_IN_MONTH)
               endif
            end do

            do iGP=0,1 ! 0=>generation, 1=>pumping energy required
               TEMP_L1 = iGP == 1
               call INDEXED_SORT_ASCENDING( &
                  CurHrCost(1,iGP),GpHrRanked(1,iGP), &
                  nHrAvail(iGP),TEMP_L1)

               nHrs(iGP)=0.0

               FracHrCurCost(iGP)=0.0
               Rank(iGP)=1
               GroupHr(iGP)=GpHrRanked(Rank(iGP),iGP)
            end do
            if(PrtDetail>1) then

               write(4,'(2a)') &
                  ' Da G R HG HP CostG CostP nGHS nPHS InGH', &
                  ' InPH FrGH FrPH #GHr #PHr GMWH PMWH'
               write(4,'(2a)') &
                  ' -- - - -- -- ----- ----- ---- ---- ----', &
                  ' ---- ---- ---- ---- ---- ---- ----'
            end if
            do
               Economical=CurHrCost(GroupHr(1),1)< &
                  PmpStoEff*CurHrCost(GroupHr(0),0)
               Feasible=.true.
               do iGP=0,1
                  nHrsSlack(iGP)=LimHrsUse(iGP)-nHrs(iGP)
                  Feasible=Feasible .and. (nHrsSlack(iGP)>0.0)
               end do
               if(.not.(Economical.and.Feasible)) then

                  exit
               end if
               do iGP=0,1
                  IncrHrCurCost(iGP)= & !  increment in hours at current costs
                     amin1(1.0-FracHrCurCost(iGP), &
                     amin1(1.0,nHrsSlack(iGP)))
                  if(iGP==0) then
                     ChronHr(iGP)=GroupHr(iGP)+InfHrAvail(0)-1
                  else
                     if(GroupHr(iGP)>nPreMidnHr) then
                        ! early-morning hours
                        ChronHr(iGP)=GroupHr(iGP)-nPreMidnHr
                     else
                         ! pre-midnight hours
                        ChronHr(iGP)=GroupHr(iGP)+InfHrAvail(1)-1
                     end if
                  end if
               end do

               do iGP=0,1
                  TimeConstraint = &
                     IncrHrCurCost(mod(iGP+1,2))*TimeRatio(iGP)
                  if(IncrHrCurCost(iGP)>TimeConstraint) &
                     IncrHrCurCost(iGP)=TimeConstraint
                  FracHrCurCost(iGP) = &
                     FracHrCurCost(iGP)+IncrHrCurCost(iGP) ! at most 1.0
!                 MoFrHrUsed(ChronHr(iGP),iDa,iGP)=FracHrCurCost(iGP)
                  nHrs(iGP)=nHrs(iGP)+IncrHrCurCost(iGP)
                  GpCapyProrated=CapyMW(iGP)*IncrHrCurCost(iGP)
                  GpHourPrice=CurHrCost(GroupHr(iGP),iGP)
!                 EnergyMWH(iGP) = EnergyMWH(iGP)+GpCapyProrated
!                 CostAccum(iGP) = CostAccum(iGP)+GpCapyProrated
!    +               *GpHourPrice

                  HOUR_IN_YEAR = (iDa-1) * 24 + ChronHr(iGP)
                  IF(iGP == 1) THEN ! for pumping energy
! 050517. ADDED MONTHLY ENERGY PRICE BACK IN.
                     R_ANNUAL_VARIABLE_COST = R_ANNUAL_VARIABLE_COST + &
                                 GpCapyProrated * GpHourPrice

                     GpCapyProrated = -GpCapyProrated
                  ELSE
                     R_ANNUAL_ENERGY = R_ANNUAL_ENERGY + GpCapyProrated
                     R_ANNUAL_ENERGY_REVENUE = R_ANNUAL_ENERGY_REVENUE + &
                                              GpCapyProrated*GpHourPrice

                  ENDIF
                  R_ANNUAL_USAGE(HOUR_IN_YEAR) = &
                           R_ANNUAL_USAGE(HOUR_IN_YEAR) + GpCapyProrated

               end do ! iGP

               do iGP=0,1
                  if(FracHrCurCost(iGP)>0.999999) then
                  ! advance to next hour of possible gen/pmp
                     Rank(iGP)=Rank(iGP)+1
                     GroupHr(iGP)=GpHrRanked(Rank(iGP),iGP)
                     FracHrCurCost(iGP)=0.0
                  end if
               end do
            end do ! while (Economical.and.Feasible)

         end do ! iDa loop
!
! REMOVE MONTHLY BECAUSE PLANNING ROUTINE.
!



    ! shift each day's late-hours capacity to same hours in prior
    ! day of month
      do iHr=InfHrAvail(1),24
      !  save the nPreMidnHr values of the first day
         HOUR_IN_YEAR=iHr

         TEMP_R1 = R_ANNUAL_USAGE(iHr)
         LAST_HOUR_QUANT = R_ANNUAL_USAGE(8736+iHr)

         do iDa=365, 2, -1
            HOUR_IN_YEAR=(iDa-1)*24+iHr
            CURRENT_HOUR_QUANT = R_ANNUAL_USAGE(HOUR_IN_YEAR-24)
            R_ANNUAL_USAGE(HOUR_IN_YEAR-24)= LAST_HOUR_QUANT
            LAST_HOUR_QUANT = CURRENT_HOUR_QUANT

         end do
      !  move the first day's nPreMidnHr values to iDa==R_DAYS_IN_MONTH

         HOUR_IN_YEAR = 8736+iHr
         R_ANNUAL_USAGE(HOUR_IN_YEAR) = TEMP_R1
      end do ! iHr
      CX_DailyOperAnPumpedStorage = .TRUE.
      return ! entry CX_DailyOperAnPumpedStorage
!

!
      entry CX_DailyStorePattern()

!

!
!     041617
!
!     CALCULATE DAILY STORAGE PATTERN (GEN AND PUMP) BY TRANS BY HOUR
!
      GRX_STORAGE_PATTERN = 0.0 ! 042217. I BELIEVE THIS IS CORRECT.
      IF(.NOT. SAVE_MONTHLY_PRODUCTS_STATUS) RETURN
      DO TRANS_GROUP = 1, NUM_OF_TRANS_CLASSES
         LOCAL_ACTIVE_TRANSACTIONS = NUM_STORAGE(TRANS_GROUP)
         IF(LOCAL_ACTIVE_TRANSACTIONS == 0) CYCLE
!
!
         CALL CX_TRANS_ANNUAL_USER_MARKET(TRANS_GROUP,LOCAL_ANN_PRICE)
         LOCAL_STORAGE_PATTERN = 0.0

!
    ! shift prior day's late-hours prices to same hours in current day
!
         do iHr=InfHrAvail(1),24 ! HOURS 23 AND 24

      !  save the nPreMidnHr values of the final day (as if
      ! prior to the 1st)

            HOUR_IN_YEAR = (365-1)*24+iHr
            TEMP_R1=LOCAL_PRICE(HOUR_IN_YEAR)
            LOCAL_ANN_PRICE(HOUR_IN_YEAR) = TEMP_R1

            LAST_HOUR_PRICE = LOCAL_ANN_PRICE(iHr)
            do iDa=2, 365
               HOUR_IN_YEAR=(iDa-1)*24+iHr
               CURRENT_HOUR_PRICE = LOCAL_ANN_PRICE(HOUR_IN_YEAR)
               LOCAL_ANN_PRICE(HOUR_IN_YEAR)= LAST_HOUR_PRICE
               LAST_HOUR_PRICE = CURRENT_HOUR_PRICE
            end do
      !     move the last day's nPreMidnHr values to iDa==1
            LOCAL_ANN_PRICE(iHr)=TEMP_R1
         end do ! iHR

         DO TRANS = 1, LOCAL_ACTIVE_TRANSACTIONS
            IND = STORAGE_POSITION(TRANS,TRANS_GROUP)

! ASSUME TOTAL CAPACITY OF STORAGE. NOTE: THIS APPLIED TO NEW
! AND EXISTING DYNAMIC STORAGE

            IF(PROPOSED_QUANT_OF_PRODUCT(IND) > 0) THEN
               NUM_STORAGE_UNITS = INT(HOURLY_QUANTITY(IND)/ &
                                       PROPOSED_QUANT_OF_PRODUCT(IND),2)
            ELSE
! PROBLEM.
               NUM_STORAGE_UNITS = 1
            ENDIF
            CapyMW(0) = HOURLY_QUANTITY(IND) ! * NUM_STORAGE_UNITS

            CapyMW(1) = PUMPING_CAPACITY(IND) * NUM_STORAGE_UNITS
            PmpStoEff = PUMPING_STORAGE_EFFICIENCY(IND)
            LimHrsGen = DAILY_PUMPING_MULT(IND) ! * NUM_STORAGE_UNITS

            PmpStoVOM = 0.0 ! ASSUME ZERO FOR NOW.

! time balance:  PmpStoEff*CapyMW(1)*LimHrsUse(1)=CapyMW(0)*LimHrsUse(0)
! 0=>generation, 1=>pumping energy required
! 050617. if charge mwh then transform
            if(CapyMW(0) > 0.000001 .and. use_charge_mwh) then
               LimHrsUse(0)= LimHrsGen * NUM_STORAGE_UNITS * &
                                                    PmpStoEff /CapyMW(0)
            else
               LimHrsUse(0)=LimHrsGen
            endif

            LimHrsUse(1)=(CapyMW(0)*LimHrsUse(0))/(PmpStoEff*CapyMW(1))
            TimeRatio(0)=LimHrsUse(0)/LimHrsUse(1)
            TimeRatio(1)=LimHrsUse(1)/LimHrsUse(0)
            nHrAvail(0)=SupHrAvail(0)-InfHrAvail(0)+1
            nHrAvail(1)=SupHrAvail(1)+nPreMidnHr

            MONTH = 0
            nDaCMo = 0
            do iDa=1, 365
               IF(iDa > nDaCMo) THEN
                  MONTH = MONTH + 1
                  nDaCMo = nDaCMo + DAYS_IN_EACH_MONTH(MONTH)
                  ! COUNTS GENERATION HOURS, INCLUDING FRACTIONAL HOURS
                  C_HOURS = 0.
                  IF(ENERGY_PRICE_MULTIPLIER(IND) < 0.) THEN

                     TEMP_R4 = 1.0
                     TEMP_I2 = INT(ABS(ENERGY_PRICE_MULTIPLIER(IND)),2)
                     MONTHLY_ENERGY_MULT(IND) = &
                     ESCALATED_MONTHLY_VALUE(TEMP_R4,TEMP_I2,globecom_YEAR, &
                                                       MONTH,INT(1,2))

                  ELSE

                     MONTHLY_ENERGY_MULT(IND) = &
                                          ENERGY_PRICE_MULTIPLIER(IND)

                  ENDIF
                  IF(ENERGY_PRICE(IND) < 0.0) THEN
                     TEMP_R4 = ABS(ENERGY_PRICE(IND))
                     TEMP_I2 = INT(ABS(ENERGY_PRICE(IND)),2)
                     MONTHLY_ENERGY_PRICE(IND) = &
                        ESCALATED_MONTHLY_VALUE(TEMP_R4, &
                                       TEMP_I2, &
                                       globecom_YEAR, &
                                       MONTH,INT(1,2))
                  ELSE
                     MONTHLY_ENERGY_PRICE(IND) = ENERGY_PRICE(IND)
                  ENDIF
                  MONTHLY_ENERGY_PRICE(IND) = &
                               MONTHLY_ENERGY_PRICE(IND) * &
                                          MONTHLY_ENERGY_MULT(IND)
               ENDIF
               Rank(0)=0
               Rank(1)=0
               ! place pre-midnight hours first
               do jHr=InfHrAvail(1),SupHrAvail(0)+24
                  iHr=1+mod(jHr-1,24)

                  iGP=1
                  if((InfHrAvail(0)<=iHr).and.(iHr<=SupHrAvail(0))) then
                                                                   iGP=0
                  end if
                  ! count of hours available for generation/pumping
                  Rank(iGP)=Rank(iGP)+1
                  GpHrRanked(Rank(iGP),iGP)=Rank(iGP)

                  HOUR_IN_YEAR = (iDa-1) * 24 + iHR
                  if(iGP == 1) then
                     CurHrCost(Rank(iGP),iGP) = &
                                           LOCAL_ANN_PRICE(HOUR_IN_YEAR) &
                     + PmpStoVOM ! added 20080924 per GAT request
                  else
                     CurHrCost(Rank(iGP),iGP) = &
                                           LOCAL_ANN_PRICE(HOUR_IN_YEAR)
                  endif
               end do

               do iGP=0,1 ! 0=>generation, 1=>pumping energy required
                  TEMP_L1 = iGP == 1
                  call INDEXED_SORT_ASCENDING( &
                     CurHrCost(1,iGP),GpHrRanked(1,iGP), &
                     nHrAvail(iGP),TEMP_L1)

                  nHrs(iGP)=0.0

                  FracHrCurCost(iGP)=0.0
                  Rank(iGP)=1
                  GroupHr(iGP)=GpHrRanked(Rank(iGP),iGP)
               end do
               if(PrtDetail>1) then

                  write(4,'(2a)') &
                     ' Da G R HG HP CostG CostP nGHS nPHS InGH', &
                     ' InPH FrGH FrPH #GHr #PHr GMWH PMWH'
                  write(4,'(2a)') &
                     ' -- - - -- -- ----- ----- ---- ---- ----', &
                     ' ---- ---- ---- ---- ---- ---- ----'
               end if
               do
                  Economical=CurHrCost(GroupHr(1),1)< &
                        PmpStoEff*CurHrCost(GroupHr(0),0)
                  Feasible=.true.
                  do iGP=0,1
                     nHrsSlack(iGP)=LimHrsUse(iGP)-nHrs(iGP)
                     Feasible=Feasible .and. (nHrsSlack(iGP)>0.0)
                  end do
                  if(.not.(Economical.and.Feasible)) then

                     exit
                  end if
                  do iGP=0,1
                     ! increment in hours at current costs
                     IncrHrCurCost(iGP)= &
                        amin1(1.0-FracHrCurCost(iGP), &
                        amin1(1.0,nHrsSlack(iGP)))
                     if(iGP==0) then
                        ChronHr(iGP)=GroupHr(iGP)+InfHrAvail(0)-1
                     else
                        if(GroupHr(iGP)>nPreMidnHr) then
                           ! early-morning hours
                           ChronHr(iGP)=GroupHr(iGP)-nPreMidnHr
                        else
                           ! pre-midnight hours
                           ChronHr(iGP)=GroupHr(iGP)+InfHrAvail(1)-1
                        end if
                     end if
                  end do ! this loop must complete before the next is begun

                  do iGP=0,1
                     TimeConstraint = &
                        IncrHrCurCost(mod(iGP+1,2))*TimeRatio(iGP)
                     if(IncrHrCurCost(iGP)>TimeConstraint) &
                                       IncrHrCurCost(iGP)=TimeConstraint
                     FracHrCurCost(iGP) = &
                     FracHrCurCost(iGP)+IncrHrCurCost(iGP) ! at most 1.0

                     nHrs(iGP)=nHrs(iGP)+IncrHrCurCost(iGP)
                     GpCapyProrated=CapyMW(iGP)*IncrHrCurCost(iGP)
                     GpHourPrice=CurHrCost(GroupHr(iGP),iGP)

                     HOUR_IN_YEAR = (iDa-1) * 24 + ChronHr(iGP)
                     IF(iGP == 1) THEN ! for pumping energy

                        MONTHLY_ENERGY_COST(IND,MONTH) = &
                        MONTHLY_ENERGY_COST(IND,MONTH) + GpCapyProrated &
                                                           * GpHourPrice

                        GpCapyProrated = -GpCapyProrated
                     ELSE

                        MONTHLY_ENERGY_REVENUE(IND,MONTH) = &
                        MONTHLY_ENERGY_REVENUE(IND,MONTH) + &
                                              GpCapyProrated*GpHourPrice
                        C_HOURS = C_HOURS + IncrHrCurCost(iGP)

                        MONTHLY_ENERGY(IND,MONTH) = &
                           MONTHLY_ENERGY(IND,MONTH) + GpCapyProrated
                     ENDIF

                     LOCAL_STORAGE_PATTERN(HOUR_IN_YEAR) = &
                           LOCAL_STORAGE_PATTERN(HOUR_IN_YEAR) &
                                                        + GpCapyProrated

                  end do ! iGP

                  do iGP=0,1
                     if(FracHrCurCost(iGP)>0.999999) then
                        ! advance to next hour of possible gen/pmp
                        Rank(iGP)=Rank(iGP)+1
                        GroupHr(iGP)=GpHrRanked(Rank(iGP),iGP)
                        FracHrCurCost(iGP)=0.0
                     end if
                  end do
               end do ! while (Economical.and.Feasible)

               IF(iDa == nDaCMo) THEN
                  ! generation only
                  MONTHLY_TRANS_HOURS(IND,MONTH) = NINT(C_HOURS)
                  MONTHLY_PRODUCT_HOURS(IND,MONTH) = &
                     MONTHLY_PRODUCT_HOURS(IND,MONTH) + &
                        DAYS_IN_EACH_MONTH(MONTH) * &
                        nint(LimHrsUse(0)+LimHrsUse(1)) ! not always 15
               ENDIF
            end do ! iDa loop
!
! KEEP MONTHLY BECAUSE TRANS PRICING OPERATIONAL ROUTINE.


!       shift each day's late-hours capacity to same hours in prior
            do iHr=InfHrAvail(1),24
      !        save the nPreMidnHr values of the first day
               HOUR_IN_YEAR=iHr
               TEMP_R1 = LOCAL_STORAGE_PATTERN(iHr)


               LAST_HOUR_QUANT = LOCAL_STORAGE_PATTERN(8736+iHr)
               do iDa=365, 2, -1
                  HOUR_IN_YEAR=(iDa-1)*24+iHr

                  CURRENT_HOUR_QUANT = &
                                  LOCAL_STORAGE_PATTERN(HOUR_IN_YEAR-24)

                  LOCAL_STORAGE_PATTERN(HOUR_IN_YEAR-24)= &
                                                         LAST_HOUR_QUANT
                  LAST_HOUR_QUANT = CURRENT_HOUR_QUANT
               end do
      !        move the first day's nPreMidnHr values to iDa==R_DAYS_IN_MONTH
               HOUR_IN_YEAR = 8736+iHr

               LOCAL_STORAGE_PATTERN(HOUR_IN_YEAR) = TEMP_R1
            end do ! iHr
            GRX_STORAGE_PATTERN(TRANS_GROUP,:) = &
                       GRX_STORAGE_PATTERN(TRANS_GROUP,:) + &
                                                LOCAL_STORAGE_PATTERN(:)
         END DO ! TRANSACTIONS
      END DO ! TRANS_GROUP
      CX_DailyStorePattern = .TRUE.
      return ! entry CX_DailyStorePattern
!

      ENTRY WRITE_HOURLY_STORAGE_REPORT()

         IF(.NOT. YES_HOURLY_GRX_STORAGE_REPORT()) RETURN
         IF(HOURLY_STORAGE_NOT_OPEN) THEN
            HOURLY_STORAGE_NOT_OPEN = .FALSE.
            HOURLY_STORAGE_VARIABLES = 24
            HOURLY_STORAGE_NO = &
                         HOURLY_STORAGE_REPORT_HEADER( &
                                           HOURLY_STORAGE_REC, &
                                           HOURLY_STORAGE_VARIABLES)
         ENDIF
         DO TRANS = 1, NUM_OF_TRANS_CLASSES
            IF(GET_TRANS_GROUP_INDEX(TRANS) < 1) CYCLE
            CURRENT_YEAR = get_BASE_YEAR() + globecom_YEAR
            MONTH = 0
            nDaCMo = 0
            do iDa=1, 365
               IF(iDa > nDaCMo) THEN
                  DAY_IN_MONTH = 1
                  MONTH = MONTH + 1
                  nDaCMo = nDaCMo + DAYS_IN_EACH_MONTH(MONTH)
               ELSE
                  DAY_IN_MONTH = DAY_IN_MONTH + 1
               ENDIF
               START_HOUR = (iDa-1)*24 + 1
               END_HOUR = START_HOUR + 23
               HOURLY_STORAGE_REC = RPTREC(HOURLY_STORAGE_NO)
               WRITE(HOURLY_STORAGE_NO,REC=HOURLY_STORAGE_REC) &
                           PRT_ENDPOINT(), &
                           FLOAT(CURRENT_YEAR), &
                           CL_MONTH_NAME(MONTH), &
                           FLOAT(DAY_IN_MONTH), &
                           GET_GROUP_NAME(TRANS), &
                           (GRX_STORAGE_PATTERN(TRANS,RPT_HR), &
                                             RPT_HR=START_HOUR,END_HOUR)
               HOURLY_STORAGE_REC = HOURLY_STORAGE_REC + 1
            enddo ! DAY IN YEAR
         ENDDO ! TRANS
      RETURN

!
      entry CX_DailyStorePat2()
!

!
!     041617
!
!     CALCULATE DAILY STORAGE PATTERN (GEN AND PUMP) BY TRANS BY HOUR
!
      GRX_STORAGE_PATTERN = 0.0 ! 042217. I BELIEVE THIS IS CORRECT.
      IF(.NOT. SAVE_MONTHLY_PRODUCTS_STATUS) RETURN
      DO TRANS_GROUP = 1, NUM_OF_TRANS_CLASSES
         LOCAL_ACTIVE_TRANSACTIONS = NUM_STORAGE(TRANS_GROUP)
         IF(LOCAL_ACTIVE_TRANSACTIONS == 0) CYCLE
!
!
         CALL CX_TRANS_ANNUAL_USER_MARKET(TRANS_GROUP,LOCAL_ANN_PRICE)
         LOCAL_STORAGE_PATTERN = 0.0
!
         DO TRANS = 1, LOCAL_ACTIVE_TRANSACTIONS
            IND = STORAGE_POSITION(TRANS,TRANS_GROUP)

! ASSUME TOTAL CAPACITY OF STORAGE. NOTE: THIS APPLIED TO NEW AND
! EXISTING DYNAMIC STORAGE

            IF(PROPOSED_QUANT_OF_PRODUCT(IND) > 0) THEN
               NUM_STORAGE_UNITS = INT(HOURLY_QUANTITY(IND)/ &
                                       PROPOSED_QUANT_OF_PRODUCT(IND),2)
            ELSE
! PROBLEM.
               NUM_STORAGE_UNITS = 1
            ENDIF

            MONTH = 0
            DAY_IN_MONTH = 0
            nDaCMo = 0
            do iDa=1, 365
               IF(iDa > nDaCMo) THEN
                  DAY_IN_MONTH = 1
                  MONTH = MONTH + 1
                  nDaCMo = nDaCMo + DAYS_IN_EACH_MONTH(MONTH)
                  C_HOURS = 0

                  IF(ENERGY_PRICE_MULTIPLIER(IND) < 0.) THEN

                     TEMP_R1 = 1.0
                     TEMP_I2 = INT(ABS(ENERGY_PRICE_MULTIPLIER(IND)),2)
                     MONTHLY_ENERGY_MULT(IND) = &
                  ESCALATED_MONTHLY_VALUE(TEMP_R1,TEMP_I2,globecom_YEAR, &
                                                       MONTH,INT(1,2))

                  ELSE

                     MONTHLY_ENERGY_MULT(IND) = &
                                          ENERGY_PRICE_MULTIPLIER(IND)

                  ENDIF
                  IF(ENERGY_PRICE(IND) < 0.0) THEN
                     TEMP_R4 = ABS(ENERGY_PRICE(IND))
                     TEMP_I2 = INT(ABS(ENERGY_PRICE(IND)),2)
                     MONTHLY_ENERGY_PRICE(IND) = &
                        ESCALATED_MONTHLY_VALUE(TEMP_R4, &
                                       TEMP_I2, &
                                       globecom_YEAR, &
                                       MONTH,INT(1,2))
                  ELSE
                     MONTHLY_ENERGY_PRICE(IND) = ENERGY_PRICE(IND)
                  ENDIF
                  MONTHLY_ENERGY_PRICE(IND) = &
                               MONTHLY_ENERGY_PRICE(IND) * &
                                          MONTHLY_ENERGY_MULT(IND)
               ELSE
                  DAY_IN_MONTH = DAY_IN_MONTH + 1
               ENDIF
!
! 051017.   NEW STORAGE ALGORITHM THAT ALLOWS ANY HOUR TO PUMP OR GEN.
!           ALSO TRACKS HOURLY STORAGE MWH/HR AS A LIMIT.
!           ONE DAY CYCLING FROM 11 PM TO 10 PM


               HOUR_IN_YEAR=(iDa-1)*24+1
               DO I = 0, 23
                  IF(DAY_IN_MONTH == 1 .AND. I < 2) THEN
                     HOUR_END_OF_MONTH = nDaCMo*24 + I - 1
!                     HOUR_END_OF_MONTH = nDaCMo*24 + I - 2
                     ONE_DAY_PRICE(I+1) = &
                                  LOCAL_ANN_PRICE(HOUR_END_OF_MONTH+I)
                  ELSE
                     ONE_DAY_PRICE(I+1) = &
                                      LOCAL_ANN_PRICE(HOUR_IN_YEAR+I-2)
                  ENDIF
                  PRICE_ORDER(I+1) = I + 1
               ENDDO
               call INDEXED_SORT_ASCENDING( &
                  ONE_DAY_PRICE,PRICE_ORDER, &
                  I,TEMP_L1) ! I = 24

               DO I = 1, 12
                  HIGH_HOUR(I) = PRICE_ORDER(I)
                  LOW_HOUR(I) = PRICE_ORDER(25-I)
                  CHARGE(HIGH_HOUR(I)) = 0
                  CHARGE(LOW_HOUR(I)) = 1
               ENDDO
               K = 0
               MARGIN = -99999.0
               MARGIN_ORDER = 0
               DO I = 1, 12
                  DO J = 1, 12
                     IF(LOW_HOUR(J) > HIGH_HOUR(I) .OR. &
                        (ONE_DAY_PRICE(LOW_HOUR(J)) + &
                                         MONTHLY_ENERGY_PRICE(IND))/ &
                               PUMPING_STORAGE_EFFICIENCY(IND) > &
                                      ONE_DAY_PRICE(HIGH_HOUR(I))) CYCLE
                     K = K + 1

! 062017.
                     MARGIN(K) = ONE_DAY_PRICE(HIGH_HOUR(I)) - &
                            (ONE_DAY_PRICE(LOW_HOUR(J)) + &
                                              MONTHLY_ENERGY_PRICE(IND))
                     MARGIN_ORDER(K) = K
                     HIGH_HR_MARGIN(K)= I
                     LOW_HR_MARGIN(K)= J
                  ENDDO
                  TOTAL_COMBO = K
               ENDDO
               call INDEXED_SORT_ASCENDING( &
                  MARGIN,MARGIN_ORDER, &
      ! K IS THE LAST VALID DISCHARGE/CHARGE PAIR
                  TOTAL_COMBO,TEMP_L1)

               HIGH_CAP = HOURLY_QUANTITY(IND)
               LOW_CAP = PUMPING_CAPACITY(IND) * NUM_STORAGE_UNITS
               STOR_LIM = DAILY_PUMPING_MULT(IND) * NUM_STORAGE_UNITS
               HOUR_STORAGE = 0.0
               IF(PUMPING_STORAGE_EFFICIENCY(IND) <= 0.0) THEN
                  WRITE(4,*) 'ZERO STORAGE EFFICIENCY',IND
                  WRITE(4,*) 'STORAGE RESOURCE NAME ', &
                         TRIM(TRANSACTION_NAME(IND))," ", &
                                            TRIM(COUNTERPARTY_NAME(IND))
                  er_message='Stop requested from TRANSOBJ2 SIID325'
                  call end_program(er_message)
               ELSEIF(HOURLY_QUANTITY(IND) <= 0.0) THEN
                  WRITE(4,*) 'ZERO DISCHARGE CAPACITY'
                  er_message='Stop requested from TRANSOBJ2 SIID326'
                  call end_program(er_message)
               ENDIF
               TOTAL_STORAGE = 0.0
               PATTERN = 0
               DO L = 1, TOTAL_COMBO
                  K = MARGIN_ORDER(L)
                  I = HIGH_HR_MARGIN(K)
                  J = LOW_HR_MARGIN(K)
                  IF(HIGH_CAP(I) > 0.0001 .AND. &
                                               LOW_CAP(J) > 0.0001) THEN
                     TEST_CAP = MIN(HIGH_CAP(I), &
                             LOW_CAP(J)*PUMPING_STORAGE_EFFICIENCY(IND))
!
! TEST FOR STORAGE CAP
!
                     DO M = LOW_HOUR(J), HIGH_HOUR(I)-1
                        TEST_CAP = MIN(TEST_CAP, &
                            (STOR_LIM + TOTAL_STORAGE(M)) * &
                                       PUMPING_STORAGE_EFFICIENCY(IND) )

                     ENDDO
                     IF(TEST_CAP <= 0.0) then
                        CYCLE
                     endif
                     DO M = LOW_HOUR(J), HIGH_HOUR(I)-1
                        TOTAL_STORAGE(M) = TOTAL_STORAGE(M) - TEST_CAP/ &
                                         PUMPING_STORAGE_EFFICIENCY(IND)
                     ENDDO
!
                     HIGH_CAP(I) = HIGH_CAP(I) - TEST_CAP
                     LOW_CAP(J) = LOW_CAP(J) - TEST_CAP/ &
                                         PUMPING_STORAGE_EFFICIENCY(IND)
                     PATTERN(HIGH_HOUR(I)) = PATTERN(HIGH_HOUR(I)) + &
                                                                TEST_CAP
                     PATTERN(LOW_HOUR(J)) = PATTERN(LOW_HOUR(J)) - &
                                                    TEST_CAP/ &
                                         PUMPING_STORAGE_EFFICIENCY(IND)
                  ENDIF
               ENDDO
               IF(globecom_YEAR > 2 .AND. MONTH == 7 .AND. &
                                                DAY_IN_MONTH == 27) THEN
                  I = I
               ENDIF
               DO I = 0, 23
                  IF(DAY_IN_MONTH == 1 .AND. I < 2) THEN
                     HOUR_END_OF_MONTH = nDaCMo*24 + I - 1
                     TEMP_I2 = HOUR_END_OF_MONTH
                  ELSE
                     TEMP_I2 = HOUR_IN_YEAR+I-2
                  ENDIF
                  LOCAL_STORAGE_PATTERN(TEMP_I2) = PATTERN(I+1)
                  IF(CHARGE(I+1) == 0) THEN
                     IF(PATTERN(I+1) < 0) THEN
                        TEMP_I2 = TEMP_I2
                     ENDIF
                     IF(LOCAL_STORAGE_PATTERN(TEMP_I2) > 0.0) THEN
                        C_HOURS = C_HOURS + &
                                  LOCAL_STORAGE_PATTERN(TEMP_I2)/ &
                                                    HOURLY_QUANTITY(IND)
                     ENDIF
                     MONTHLY_ENERGY(IND,MONTH) = &
                           MONTHLY_ENERGY(IND,MONTH) + &
                                          LOCAL_STORAGE_PATTERN(TEMP_I2)
                     MONTHLY_ENERGY_REVENUE(IND,MONTH) = &
                           MONTHLY_ENERGY_REVENUE(IND,MONTH) + &
                                              LOCAL_ANN_PRICE(TEMP_I2) * &
                                          LOCAL_STORAGE_PATTERN(TEMP_I2)
                  ELSE
                     MONTHLY_ENERGY_COST(IND,MONTH) = &
                         MONTHLY_ENERGY_COST(IND,MONTH) &
                              - (LOCAL_ANN_PRICE(TEMP_I2) + &
                                           MONTHLY_ENERGY_PRICE(IND)) * &
                                          LOCAL_STORAGE_PATTERN(TEMP_I2)
                  ENDIF
               ENDDO
               IF(iDa == nDaCMo) THEN
                  ! generation only
                  MONTHLY_TRANS_HOURS(IND,MONTH) = NINT(C_HOURS)

               ENDIF
            ENDDO ! DAY
!
! KEEP MONTHLY BECAUSE TRANS PRICING OPERATIONAL ROUTINE.
!
!
            GRX_STORAGE_PATTERN(TRANS_GROUP,:) = &
                       GRX_STORAGE_PATTERN(TRANS_GROUP,:) + &
                                                LOCAL_STORAGE_PATTERN(:)
         END DO ! TRANSACTIONS
      END DO ! TRANS_GROUP
      CX_DailyStorePat2 = .TRUE.
      return ! entry CX_DailyStorePat2

!
      entry CX_DailyStorePat3()
!

!
      GRX_STORAGE_PATTERN = 0.0
      IF(.NOT. SAVE_MONTHLY_PRODUCTS_STATUS) RETURN
      DO TRANS_GROUP = 1, NUM_OF_TRANS_CLASSES
!
! 090221. CAN'T USE NUM_STORAGE B/C IT IS MONTHLY.
!
!         LOCAL_ACTIVE_TRANSACTIONS = NUM_STORAGE(TRANS_GROUP)
!         LOCAL_ACTIVE_TRANSACTIONS = ANN_NUM_STOR(TRANS_GROUP)
         LOCAL_ACTIVE_TRANSACTIONS = SCEN_NUM_STOR(TRANS_GROUP)
         IF(LOCAL_ACTIVE_TRANSACTIONS == 0) CYCLE
!
!
         CALL CX_TRANS_ANNUAL_USER_MARKET(TRANS_GROUP,LOCAL_ANN_PRICE)
!         LOCAL_STORAGE_PATTERN = 0.0
!
         DO TRANS = 1, LOCAL_ACTIVE_TRANSACTIONS
            IND = SCEN_STOR_POS(TRANS,TRANS_GROUP)
!            IND = STORAGE_POSITION(TRANS,TRANS_GROUP)
!
! ASSUME TOTAL CAPACITY OF STORAGE. NOTE: THIS APPLIED TO NEW
! AND EXISTING DYNAMIC STORAGE

! 021322. TRAP FOR EXPANSION RESOURCES.
!
            IF(PRODUCT_ACTIVE(IND) == 'P') THEN
               IF(ABS(HOURLY_QUANTITY(IND)) < 0.001) THEN
                  CYCLE
               ELSE
                  IND = IND
               ENDIF
            ENDIF

            LOCAL_STORAGE_PATTERN = 0.0 ! MOVED INSIDE LOOP.
            LOCAL_HYBRID_MW_DISCHARGE = 999999.0
            LOCAL_HYBRID_HR_MARGIN = 0.0
            LOCAL_HYBRID_MW_CHARGE = 999999.0

            TEMP_L = EXISTING_STORAGE_N_HYBRID( &
                                        IND, & !  TRANS
                                        LOCAL_ANN_PRICE, & !  8784
                                        LOCAL_STORAGE_PATTERN, & !  8760 ! 8784 ?
                                        LOCAL_ANNUAL_ENERGY, &
                                        LOCAL_ANNUAL_CHARGE, &
                                        LOCAL_ANNUAL_ENERGY_REVENUE, &
                                        LOCAL_ANNUAL_VOM_COST, &
                                        LOCAL_INDEP_PATTERN) ! 8760 ! 8784 ?

            GRX_STORAGE_PATTERN(TRANS_GROUP,:) = &
                       GRX_STORAGE_PATTERN(TRANS_GROUP,:) + &
                                                LOCAL_STORAGE_PATTERN(:)
            GRX_INDEP_PATTERN(TRANS_GROUP,:) = &
                       GRX_INDEP_PATTERN(TRANS_GROUP,:) + &
                                                  LOCAL_INDEP_PATTERN(:)
         END DO ! TRANSACTIONS
      END DO ! TRANS_GROUP
      CX_DailyStorePat3 = .TRUE.
      return ! entry CX_DailyStorePat3

      ENTRY GET_GRX_INDEP_POINTER(R_TRANS)

         IND = GRX_ID_INDEX(R_TRANS)
         IF(IND > 0) THEN
            GET_GRX_INDEP_POINTER = MAX(INT(0,2),-HYBRID_BATTERY(IND))
            IF(GET_GRX_INDEP_POINTER > 0) THEN
               GET_GRX_INDEP_POINTER = &
                                     HYBRID_INDEX(GET_GRX_INDEP_POINTER)
               GET_GRX_INDEP_POINTER = GRX_ID(GET_GRX_INDEP_POINTER)
            ENDIF
         ELSE
            GET_GRX_INDEP_POINTER = 0
         ENDIF
      RETURN

!
!      entry DailyOperMoPumpedStorage(
      entry DailyOperMoPSTransC( &
         R_TRANS_GROUP, &
         R_MONTHLY_PRICE,    & !  24x31 hourly detail
         R_MONTHLY_CAPACITY, & !  24x31 hourly detail begins at (1); (0:800)
         R_MONTH, &
         R_DAYS_IN_MONTH, &
         R_MONTHLY_AVAIL)
!

!
!     072917
!
!     CALCULATE DAILY STORAGE PATTERN (GEN AND PUMP) BY TRANS BY HOUR
!
!      GRX_STORAGE_PATTERN = 0.0 ! 042217. I BELIEVE THIS IS CORRECT.
      IF(.NOT. SAVE_MONTHLY_PRODUCTS_STATUS) RETURN
!      DO TRANS_GROUP = 1, NUM_OF_TRANS_CLASSES
      TRANS_GROUP = R_TRANS_GROUP
         LOCAL_ACTIVE_TRANSACTIONS = NUM_STORAGE(TRANS_GROUP)
         IF(LOCAL_ACTIVE_TRANSACTIONS == 0) then
            RETURN
         end if

         LOCAL_PRICE(1:800) = R_MONTHLY_PRICE(1:800)
!
!         LOCAL_STORAGE_PATTERN = 0.0
         R_MONTHLY_CAPACITY = 0.0
!
         DO TRANS = 1, LOCAL_ACTIVE_TRANSACTIONS
            IND = STORAGE_POSITION(TRANS,TRANS_GROUP)

! ASSUME TOTAL CAPACITY OF STORAGE. NOTE: THIS APPLIED TO NEW AND
! EXISTING DYNAMIC STORAGE

            IF(PROPOSED_QUANT_OF_PRODUCT(IND) > 0) THEN
               NUM_STORAGE_UNITS = INT(HOURLY_QUANTITY(IND)/ &
                                       PROPOSED_QUANT_OF_PRODUCT(IND),2)
            ELSE
               NUM_STORAGE_UNITS = 1
            ENDIF

            MONTH = R_MONTH
            DAY_IN_MONTH = 0
            nDaCMo = R_DAYS_IN_MONTH
            do iDa=1, R_DAYS_IN_MONTH
               IF(iDa == 1) THEN
                  DAY_IN_MONTH = 1

                  C_HOURS = 0

                  IF(ENERGY_PRICE_MULTIPLIER(IND) < 0.) THEN

                     TEMP_R1 = 1.0
                     TEMP_I2 = INT(ABS(ENERGY_PRICE_MULTIPLIER(IND)),2)
                     MONTHLY_ENERGY_MULT(IND) = &
                ESCALATED_MONTHLY_VALUE(TEMP_R1,TEMP_I2,globecom_YEAR, &
                                                       MONTH,INT(1,2))

                  ELSE

                     MONTHLY_ENERGY_MULT(IND) = &
                                          ENERGY_PRICE_MULTIPLIER(IND)

                  ENDIF
                  IF(ENERGY_PRICE(IND) < 0.0) THEN
                     TEMP_R4 = ABS(ENERGY_PRICE(IND))
                     TEMP_I2 = INT(ABS(ENERGY_PRICE(IND)),2)
                     MONTHLY_ENERGY_PRICE(IND) = &
                        ESCALATED_MONTHLY_VALUE(TEMP_R4, &
                                       TEMP_I2, &
                                       globecom_YEAR, &
                                       MONTH,INT(1,2))
                  ELSE
                     MONTHLY_ENERGY_PRICE(IND) = ENERGY_PRICE(IND)
                  ENDIF
                  MONTHLY_ENERGY_PRICE(IND) = &
                               MONTHLY_ENERGY_PRICE(IND) * &
                                          MONTHLY_ENERGY_MULT(IND)
               ELSE
                  DAY_IN_MONTH = DAY_IN_MONTH + 1
               ENDIF

! 051017.   NEW STORAGE ALGORITHM THAT ALLOWS ANY HOUR TO PUMP OR GEN.
!           ALSO TRACKS HOURLY STORAGE MWH/HR AS A LIMIT.
!           ONE DAY CYCLING FROM 11 PM TO 10 PM

               HOUR_IN_YEAR=(iDa-1)*24+1
               DO I = 0, 23
                  IF(DAY_IN_MONTH == 1 .AND. I < 2) THEN
                     HOUR_END_OF_MONTH = nDaCMo*24 + I - 1

                     ONE_DAY_PRICE(I+1) = &
                                      LOCAL_PRICE(HOUR_END_OF_MONTH)
!     +                            LOCAL_ANN_PRICE(HOUR_END_OF_MONTH+I)
                  ELSE
                     ONE_DAY_PRICE(I+1) = &
                                          LOCAL_PRICE(HOUR_IN_YEAR+I-2)
!     +                                LOCAL_ANN_PRICE(HOUR_IN_YEAR+I-2)
                  ENDIF
                  PRICE_ORDER(I+1) = I + 1
               ENDDO
               call INDEXED_SORT_ASCENDING( &
                  ONE_DAY_PRICE,PRICE_ORDER, &
                  I,TEMP_L1) ! I = 24
!
               DO I = 1, 12
                  HIGH_HOUR(I) = PRICE_ORDER(I)
                  LOW_HOUR(I) = PRICE_ORDER(25-I)
                  CHARGE(HIGH_HOUR(I)) = 0
                  CHARGE(LOW_HOUR(I)) = 1
               ENDDO
               K = 0
               MARGIN = -99999.0
               MARGIN_ORDER = 0
               DO I = 1, 12
                  DO J = 1, 12
                     IF(LOW_HOUR(J) > HIGH_HOUR(I) .OR. &
                        (ONE_DAY_PRICE(LOW_HOUR(J)) + &
                                         MONTHLY_ENERGY_PRICE(IND))/ &
                               PUMPING_STORAGE_EFFICIENCY(IND) > &
                                      ONE_DAY_PRICE(HIGH_HOUR(I))) CYCLE

                     K = K + 1

                     MARGIN(K) = ONE_DAY_PRICE(HIGH_HOUR(I)) - &
                            (ONE_DAY_PRICE(LOW_HOUR(J)) + &
                                              MONTHLY_ENERGY_PRICE(IND))
                     MARGIN_ORDER(K) = K
                     HIGH_HR_MARGIN(K)= I
                     LOW_HR_MARGIN(K)= J
                  ENDDO
                  TOTAL_COMBO = K
               ENDDO
               call INDEXED_SORT_ASCENDING( &
                  MARGIN,MARGIN_ORDER, &
                  ! K IS THE LAST VALID DISCHARGE/CHARGE PAIR
                  TOTAL_COMBO,TEMP_L1)

               HIGH_CAP = HOURLY_QUANTITY(IND)
               LOW_CAP = PUMPING_CAPACITY(IND) * NUM_STORAGE_UNITS
               DailyPumpingLimitMWH = DAILY_PUMPING_MULT(IND)* &
                                                   PUMPING_CAPACITY(IND)
               STOR_LIM = DailyPumpingLimitMWH * NUM_STORAGE_UNITS
               HOUR_STORAGE = 0.0
               IF(PUMPING_STORAGE_EFFICIENCY(IND) <= 0.0) THEN
                  WRITE(4,*) 'ZERO STORAGE EFFICIENCY',IND
                  WRITE(4,*) 'STORAGE RESOURCE NAME ', &
                         TRIM(TRANSACTION_NAME(IND))," ", &
                                            TRIM(COUNTERPARTY_NAME(IND))
                  er_message='Stop requested from TRANSOBJ2 SIID327'
                  call end_program(er_message)
               ELSEIF(HOURLY_QUANTITY(IND) <= 0.0) THEN
                  WRITE(4,*) 'ZERO DISCHARGE CAPACITY'
                  er_message='Stop requested from TRANSOBJ2 SIID328'
                  call end_program(er_message)
               ENDIF
               TOTAL_STORAGE = 0.0
               PATTERN = 0
               DO L = 1, TOTAL_COMBO
                  K = MARGIN_ORDER(L)
                  I = HIGH_HR_MARGIN(K)
                  J = LOW_HR_MARGIN(K)
                  IF(HIGH_CAP(I) > 0.0001 .AND. &
                                               LOW_CAP(J) > 0.0001) THEN
                     TEST_CAP = MIN(HIGH_CAP(I), &
                             LOW_CAP(J)*PUMPING_STORAGE_EFFICIENCY(IND))
!
! TEST FOR STORAGE CAP
!
                     DO M = LOW_HOUR(J), HIGH_HOUR(I)-1
                        TEST_CAP = MIN(TEST_CAP, &
                            (STOR_LIM + TOTAL_STORAGE(M)) * &
                                       PUMPING_STORAGE_EFFICIENCY(IND) )
!                        TEST_CAP = MIN(TEST_CAP,
!     +                      STOR_LIM + TOTAL_STORAGE(M))
                     ENDDO
                     IF(TEST_CAP <= 0.0) CYCLE
                     DO M = LOW_HOUR(J), HIGH_HOUR(I)-1
                        TOTAL_STORAGE(M) = TOTAL_STORAGE(M) - TEST_CAP/ &
                                         PUMPING_STORAGE_EFFICIENCY(IND)
                     ENDDO
!
                     HIGH_CAP(I) = HIGH_CAP(I) - TEST_CAP
                     LOW_CAP(J) = LOW_CAP(J) - TEST_CAP/ &
                                         PUMPING_STORAGE_EFFICIENCY(IND)
                     PATTERN(HIGH_HOUR(I)) = PATTERN(HIGH_HOUR(I)) + &
                                                                TEST_CAP
                     PATTERN(LOW_HOUR(J)) = PATTERN(LOW_HOUR(J)) - &
                                                    TEST_CAP/ &
                                         PUMPING_STORAGE_EFFICIENCY(IND)
                  ENDIF
               ENDDO
               IF(globecom_YEAR > 2 .AND. MONTH == 7 .AND. &
                                                DAY_IN_MONTH == 27) THEN
                  I = I
               ENDIF
               DO I = 0, 23
                  IF(DAY_IN_MONTH == 1 .AND. I < 2) THEN
                     HOUR_END_OF_MONTH = nDaCMo*24 + I - 1
                     TEMP_I2 = HOUR_END_OF_MONTH
                  ELSE
                     TEMP_I2 = HOUR_IN_YEAR+I-2
                  ENDIF
                  R_MONTHLY_CAPACITY(TEMP_I2) = PATTERN(I+1)
                  IF(CHARGE(I+1) == 0) THEN
                     IF(PATTERN(I+1) < 0) THEN
                        TEMP_I2 = TEMP_I2
                     ENDIF
                     IF(R_MONTHLY_CAPACITY(TEMP_I2) > 0.0) THEN
                        C_HOURS = C_HOURS + &
                                  R_MONTHLY_CAPACITY(TEMP_I2)/ &
                                                    HOURLY_QUANTITY(IND)
                     ENDIF
                     MONTHLY_ENERGY(IND,MONTH) = &
                           MONTHLY_ENERGY(IND,MONTH) + &
                                          R_MONTHLY_CAPACITY(TEMP_I2)
                     MONTHLY_ENERGY_REVENUE(IND,MONTH) = &
                           MONTHLY_ENERGY_REVENUE(IND,MONTH) + &
                                               LOCAL_PRICE(TEMP_I2) * &
                                          R_MONTHLY_CAPACITY(TEMP_I2)
                  ELSE
                     MONTHLY_ENERGY_COST(IND,MONTH) = &
                         MONTHLY_ENERGY_COST(IND,MONTH) &
                              - (LOCAL_PRICE(TEMP_I2) + &
                                           MONTHLY_ENERGY_PRICE(IND)) * &
                                          R_MONTHLY_CAPACITY(TEMP_I2)
                  ENDIF
               ENDDO
               IF(iDa == nDaCMo) THEN
                  ! generation only
                  MONTHLY_TRANS_HOURS(IND,MONTH) = NINT(C_HOURS)

               ENDIF
            ENDDO ! DAY

! KEEP MONTHLY BECAUSE TRANS PRICING OPERATIONAL ROUTINE.


         END DO ! TRANSACTIONS

      DailyOperMoPSTransC = .TRUE.
      return !

      ENTRY GET_GRX_TRANS_HOURLY_STORAGE( R_TRANS_GROUP, &
                                          R_HOUR_IN_MONTH, &
                                          R_MONTH)

         IF(.NOT. SAVE_MONTHLY_PRODUCTS_STATUS) RETURN
         HOUR_IN_YEAR = DAYS_YEAR_TO_DATE(R_MONTH)*24 + R_HOUR_IN_MONTH

         GET_GRX_TRANS_HOURLY_STORAGE = &
                       GRX_STORAGE_PATTERN(R_TRANS_GROUP,HOUR_IN_YEAR) + &
                       GRX_INDEP_PATTERN(R_TRANS_GROUP,HOUR_IN_YEAR)
      RETURN
!

!
      ENTRY GET_GRX_TRANS_MONTHLY_STORAGE(R_TRANS_GROUP, &
                                          R_MONTH, &
                                          R_MONTHLY_CAPACITY)
!

         HOURS_IN_MONTH = DaysInNLYMo(R_MONTH)*24
         DO I = 1, HOURS_IN_MONTH
            HOUR_IN_YEAR = DAYS_YEAR_TO_DATE(R_MONTH)*24 + I
            R_MONTHLY_CAPACITY(I) = &
                      GRX_STORAGE_PATTERN(R_TRANS_GROUP,HOUR_IN_YEAR) + &
                      GRX_INDEP_PATTERN(R_TRANS_GROUP,HOUR_IN_YEAR)
         ENDDO
         GET_GRX_TRANS_MONTHLY_STORAGE = .TRUE.
      RETURN

!
      ENTRY HOURLY_INTERRUPTIBLE_CAPACITY( &
                                       R_HOUR_IN_DAY, &
                                       R_HOURLY_PRICE, &
                                       R_DAY_IN_WEEK, &
                                       R_DAY_IN_MONTH, &
                                       R_TRANS_GROUP, &
                                       R_MONTH)
               if(r_month>12) then
                call end_program("transobj2:0021 - r_month value of " // &
                    trim(itos(int(r_month))) // " is invalid.")
               end if

         HOURLY_INTERRUPTIBLE_CAPACITY = 0.
!
         IF(.NOT. SAVE_MONTHLY_PRODUCTS_STATUS) RETURN
!
!
         IF(NUM_INTER_CALLS(R_TRANS_GROUP) + &
                              NUM_INTER_PUTS(R_TRANS_GROUP) <= 0) RETURN
!
         APPLY_5X16 = APPLY_ENERGY_PRODUCT(INT(12,2),R_DAY_IN_WEEK, &
                                                  LOCAL_PRODUCT_TYPE(1))
         APPLY_6X16 = APPLY_ENERGY_PRODUCT(INT(12,2),R_DAY_IN_WEEK, &
                                                  LOCAL_PRODUCT_TYPE(2))
         APPLY_7X24 = APPLY_ENERGY_PRODUCT(INT(12,2),R_DAY_IN_WEEK, &
                                                  LOCAL_PRODUCT_TYPE(3))
         APPLY_5X8  = APPLY_ENERGY_PRODUCT(INT(1,2), R_DAY_IN_WEEK, &
                                                  LOCAL_PRODUCT_TYPE(4))
         APPLY_WRAP  = APPLY_ENERGY_PRODUCT(R_HOUR_IN_DAY,R_DAY_IN_WEEK, &
                                                  LOCAL_PRODUCT_TYPE(5))
         APPLY_2X24  = APPLY_ENERGY_PRODUCT(INT(1,2),R_DAY_IN_WEEK, &
                                                  LOCAL_PRODUCT_TYPE(7))
!
! HOURLY CALL LOGIC
!
! NEED A TRAP FOR HOURLY INTERRUPTIBLE
!
!
         DO I = 1, 2
            IF(I == 1) THEN ! CALLS
               LOCAL_ACTIVE_TRANSACTIONS = &
                                          NUM_INTER_CALLS(R_TRANS_GROUP)
               CALL_OR_PUT = 1.
            ELSE ! PUTS
               LOCAL_ACTIVE_TRANSACTIONS = NUM_INTER_PUTS(R_TRANS_GROUP)
               CALL_OR_PUT = -1.
            ENDIF
            DO TRANS = 1, LOCAL_ACTIVE_TRANSACTIONS
               IF(I == 1) THEN
                  IND = INTER_CALL_POSITION(TRANS,R_TRANS_GROUP)
               ELSE
                  IND = INTER_PUT_POSITION(TRANS,R_TRANS_GROUP)
               ENDIF
               if(r_month>12) then
                call end_program("transobj2:0019 - r_month value of " // &
                    trim(itos(int(r_month))) // " is invalid.")
               end if
               
!               USER_DAY_MULT = 1.0
               IF(R_DAY_IN_MONTH < BEGIN_DAY_IN_MONTH(IND) .OR. &
                         R_DAY_IN_MONTH > END_DAY_IN_MONTH(IND)) CYCLE
!
               IF( PRODUCT_TYPE(IND) == LOCAL_PRODUCT_TYPE(1) .AND. &
                                                        APPLY_5X16) THEN
                  PRODUCT = 1
               ELSEIF(PRODUCT_TYPE(IND) == LOCAL_PRODUCT_TYPE(2) .AND. &
                                                        APPLY_6X16) THEN
                  PRODUCT = 2
               ELSEIF(PRODUCT_TYPE(IND) == LOCAL_PRODUCT_TYPE(3) .AND. &
                                                        APPLY_7X24) THEN
                  PRODUCT = 3
               ELSEIF(PRODUCT_TYPE(IND) == LOCAL_PRODUCT_TYPE(4) .AND. &
                                                        APPLY_5X8) THEN
                  PRODUCT = 4
               ELSEIF(PRODUCT_TYPE(IND) == LOCAL_PRODUCT_TYPE(5) .AND. &
                                                        APPLY_WRAP) THEN
                  IF(APPLY_2X24) THEN
                     PRODUCT = 7
                  ELSE
                     PRODUCT = 5
                  ENDIF
!                  PRODUCT = 5
                ELSE
                    if(r_month>12) then
                        call end_program("transobj2:0022 - " // &
                        "r_month value of " // trim(itos(int(r_month))) // &
                        " is invalid.")
                    end if 
                    
                    
                    


                    


                                     
                    IF(PRODUCT_TYPE(IND) == LOCAL_PRODUCT_TYPE(6) .AND. &
                    GET_USER_HOUR_IN_DAY(r_hour_in_day, &
                       DAY_IN_WEEK,R_MONTH, &
               USER_DAY_TYPES_ID(IND), MONTHLY_CF_TRANS(IND)) > 0.) THEN
                  PRODUCT = 6
! 052204
                  USER_DAY_MULT = GET_USER_HOUR_IN_DAY(R_HOUR_IN_DAY, &
                                      R_DAY_IN_WEEK, &
                                      R_MONTH, &
                                      USER_DAY_TYPES_ID(IND), &
                                      MONTHLY_CF_TRANS(IND))
               ELSE

                  CYCLE
           end if
               ENDIF
               
           
!
!
!
!
               MONTHLY_PRODUCT_HOURS(IND,R_MONTH) = &
                        MONTHLY_PRODUCT_HOURS(IND,R_MONTH) + 1
!     +                                  INT2(HOURS_FOR_PRODUCT(PRODUCT))
!
               IF(STRIKES_AVAILABLE(IND) <= 0) CYCLE
! 10/24/01.
               TEMP_QUANTITY = HOURLY_QUANTITY(IND)
! 052204
!               TEMP_QUANTITY = TEMP_QUANTITY * USER_DAY_MULT
! 100908.
!
               IF(PRODUCT /= 6) THEN
                  TEMP_QUANTITY = TEMP_QUANTITY * &
                                  PRODUCT_PATTERN(R_HOUR_IN_DAY,PRODUCT)
               ELSE
                  TEMP_QUANTITY = TEMP_QUANTITY * USER_DAY_MULT
               ENDIF
!
! NO CONTINGENCY LOGIC
!
!
           if(allocated(MONTHLY_ENERGY_PRICE)) then
               HOURLY_OPTION_VALUE = &
                     CALL_OR_PUT * &
                          (R_HOURLY_PRICE - MONTHLY_ENERGY_PRICE(IND)) * &
                          TEMP_QUANTITY
           else
            call end_program("transobj2:0010 - the " // &
      "MONTHLY_ENERGY_PRICE array is not allocated.")
           end if
! IF VALUE NON-POSITIVE, ITS NO GOOD.  CYCLE.
!
               IF(HOURLY_OPTION_VALUE <= 0.) CYCLE

               SELL_OR_BUY = 1.
               LONG_SHORT = 1.0
!
! ITS GOOD.  ADD IT TO R_DAILY_CAPACITY.

               MONTHLY_TRANS_HOURS(IND,R_MONTH) = &
                                    MONTHLY_TRANS_HOURS(IND,R_MONTH) + 1
               MONTHLY_STRIKES(IND,R_MONTH) = &
                                        MONTHLY_STRIKES(IND,R_MONTH) + 1

               STRIKES_AVAILABLE(IND) = STRIKES_AVAILABLE(IND) - 1
! BUY/SELL POSITION ACCUMULATED MONTHLY.
               SELL_OR_BUY = 1.0

! 12/21/01. IF SELLING A DERIVATIVE PRODUCT, THEN THE BUYER IS PAYING
! FOR THE OPTIONALITY.
! FOR INSTANCE, SELLING A CALL OPTION MEANS THAT WHEN IT IS CALLED,
! YOU MUST SERVE THE LOAD.

! THIS IS CONSISTENT WITH THE FORWARD SALE.  A SALE INCREASES
! DEMAND ON THE SYSTEM.

! 4/19/02. CHANGED TO LOCAL_ASSIGNMENT(1)

               IF(EXPENSE_ASSIGNMENT(IND) == LOCAL_ASSIGNMENT(1)) THEN
                  LONG_SHORT = -LONG_SHORT
               ENDIF
!
!
               MONTHLY_ENERGY(IND,R_MONTH) = &
                   MONTHLY_ENERGY(IND,R_MONTH) + TEMP_QUANTITY
               MONTHLY_ENERGY_COST(IND,R_MONTH) = &
                  MONTHLY_ENERGY_COST(IND,R_MONTH) + &
                               MONTHLY_ENERGY_PRICE(IND) * TEMP_QUANTITY
!
               TEMP_PRICE_24 = 0.
!              IF(DERIVATIVE_TYPE(IND) < 4) THEN ! PHYSICAL
!
!ASSUMES ALL INTERRUPTIBLES ARE PHYSICAL
!
               HOURLY_INTERRUPTIBLE_CAPACITY = &
                              HOURLY_INTERRUPTIBLE_CAPACITY + &
                                                TEMP_QUANTITY*LONG_SHORT
!
               HOURLY_OPTION_REVENUE = 0.

! SELL_OR_BUY ALREADY CALCULATED
!

            ENDDO ! TRANSACTIONS
         ENDDO ! CALL OR PUT
      RETURN

!
      ENTRY HOURLY_LF_CAPACITY( &
                                       R_HOUR_IN_DAY, &
                                       R_HOURLY_PRICE, &
                                       R_DAY_IN_WEEK, &
                                       R_DAY_IN_MONTH, &
                                       R_TRANS_GROUP, &
                                       R_MONTH, &
                                       R_WHOLESALE_DEMAND, &
                                       R_WHOLESALE_PURCHASE, &
                                       R_ECITY_USED)
!

!
!
         HOURLY_LF_CAPACITY = 0.
!
         IF(.NOT. SAVE_MONTHLY_PRODUCTS_STATUS) RETURN
!
         IF(NUM_LF_CALLS(R_TRANS_GROUP) + &
                              NUM_LF_PUTS(R_TRANS_GROUP) <= 0) RETURN
!
         APPLY_5X16 = APPLY_ENERGY_PRODUCT(INT(12,2),R_DAY_IN_WEEK, &
                                                  LOCAL_PRODUCT_TYPE(1))
         APPLY_6X16 = APPLY_ENERGY_PRODUCT(INT(12,2),R_DAY_IN_WEEK, &
                                                  LOCAL_PRODUCT_TYPE(2))
         APPLY_7X24 = APPLY_ENERGY_PRODUCT(INT(12,2),R_DAY_IN_WEEK, &
                                                  LOCAL_PRODUCT_TYPE(3))
         APPLY_5X8  = APPLY_ENERGY_PRODUCT(INT(1,2), R_DAY_IN_WEEK, &
                                                  LOCAL_PRODUCT_TYPE(4))
         APPLY_WRAP  = APPLY_ENERGY_PRODUCT(R_HOUR_IN_DAY,R_DAY_IN_WEEK, &
                                                  LOCAL_PRODUCT_TYPE(5))
         APPLY_2X24  = APPLY_ENERGY_PRODUCT(INT(1,2),R_DAY_IN_WEEK, &
                                                  LOCAL_PRODUCT_TYPE(7))
!
! HOURLY CALL LOGIC
!
! NEED A TRAP FOR HOURLY LFRUPTIBLE
!
         REMAINING_WHOLESALE_DEMAND = R_WHOLESALE_DEMAND
         REMAINING_WHOLESALE_PURCHASE = R_WHOLESALE_PURCHASE
!
!
         DO I = 1, 2
            IF(I == 1) THEN ! CALLS
               LOCAL_ACTIVE_TRANSACTIONS = &
                                          NUM_LF_CALLS(R_TRANS_GROUP)
               CALL_OR_PUT = 1.
            ELSE ! PUTS
               LOCAL_ACTIVE_TRANSACTIONS = NUM_LF_PUTS(R_TRANS_GROUP)
               CALL_OR_PUT = -1.
            ENDIF
            DO TRANS = 1, LOCAL_ACTIVE_TRANSACTIONS
               IF(I == 1) THEN
                  IND = LF_CALL_POSITION(TRANS,R_TRANS_GROUP)
               ELSE
                  IND = LF_PUT_POSITION(TRANS,R_TRANS_GROUP)
               ENDIF
!
               IF(UNIT_CONTINGENCY(IND) == 'R' .AND. R_ECITY_USED) THEN
                  CYCLE
               ELSEIF(UNIT_CONTINGENCY(IND) == 'U' .AND. &
                                                .NOT. R_ECITY_USED) THEN
                  CYCLE
               ENDIF
!
               LONG_SHORT = 1.0
! 013108. LOCAL_ASSIGNMENT(1) IMPLIES AN OFF SYSTEM SALE
               IF(EXPENSE_ASSIGNMENT(IND) == LOCAL_ASSIGNMENT(1)) THEN
                  LONG_SHORT = -LONG_SHORT
               ENDIF

              IF(R_DAY_IN_MONTH < BEGIN_DAY_IN_MONTH(IND) .OR. &
                         R_DAY_IN_MONTH > END_DAY_IN_MONTH(IND)) CYCLE

               IF( PRODUCT_TYPE(IND) == LOCAL_PRODUCT_TYPE(1) .AND. &
                                                        APPLY_5X16) THEN
                  PRODUCT = 1
               ELSEIF(PRODUCT_TYPE(IND) == LOCAL_PRODUCT_TYPE(2) .AND. &
                                                        APPLY_6X16) THEN
                  PRODUCT = 2
               ELSEIF(PRODUCT_TYPE(IND) == LOCAL_PRODUCT_TYPE(3) .AND. &
                                                        APPLY_7X24) THEN
                  PRODUCT = 3
               ELSEIF(PRODUCT_TYPE(IND) == LOCAL_PRODUCT_TYPE(4) .AND. &
                                                        APPLY_5X8) THEN
                  PRODUCT = 4
               ELSEIF(PRODUCT_TYPE(IND) == LOCAL_PRODUCT_TYPE(5) .AND. &
                                                        APPLY_WRAP) THEN
                  IF(APPLY_2X24) THEN
                     PRODUCT = 7
                  ELSE
                     PRODUCT = 5
                  ENDIF
               ELSEIF(PRODUCT_TYPE(IND) == LOCAL_PRODUCT_TYPE(6) .AND. &
                     GET_USER_HOUR_IN_DAY(R_HOUR_IN_DAY, &
                                      R_DAY_IN_WEEK, &
                                      R_MONTH, &
                                      USER_DAY_TYPES_ID(IND), &
                                      MONTHLY_CF_TRANS(IND)) > 0.) THEN
                  PRODUCT = 6
! 052204
                  USER_DAY_MULT = GET_USER_HOUR_IN_DAY(R_HOUR_IN_DAY, &
                                      R_DAY_IN_WEEK, &
                                      R_MONTH, &
                                      USER_DAY_TYPES_ID(IND), &
                                      MONTHLY_CF_TRANS(IND))
               ELSE

                  CYCLE

               ENDIF

               MONTHLY_PRODUCT_HOURS(IND,R_MONTH) = &
                        MONTHLY_PRODUCT_HOURS(IND,R_MONTH) + 1

               IF(STRIKES_AVAILABLE(IND) <= 0) CYCLE


               IF(CALL_OR_PUT*LONG_SHORT > 0) THEN
                  TEMP_QUANTITY = MIN( HOURLY_QUANTITY(IND), &
                                    REMAINING_WHOLESALE_DEMAND)
               ELSE
                  TEMP_QUANTITY = MIN( HOURLY_QUANTITY(IND), &
                                    REMAINING_WHOLESALE_PURCHASE)
               ENDIF

               IF(PRICE_TYPE(IND) == 'I') THEN


                  MONTHLY_ENERGY_PRICE(IND) = &
                               R_HOURLY_PRICE * MONTHLY_ENERGY_MULT(IND)

               ENDIF

               IF(PRODUCT /= 6) THEN
                  TEMP_QUANTITY = TEMP_QUANTITY * &
                                  PRODUCT_PATTERN(R_HOUR_IN_DAY,PRODUCT)
               ELSE
                  TEMP_QUANTITY = TEMP_QUANTITY * USER_DAY_MULT
               ENDIF

! IS IT IN THE MONEY?

               HOURLY_OPTION_VALUE = &
                     CALL_OR_PUT * &
                          (R_HOURLY_PRICE - MONTHLY_ENERGY_PRICE(IND)) * &
                          TEMP_QUANTITY



               IF(TEMP_QUANTITY <= 0.01) then
                CYCLE
               end if

! ITS GOOD.  ADD IT TO R_DAILY_CAPACITY.

               MONTHLY_TRANS_HOURS(IND,R_MONTH) = &
                                    MONTHLY_TRANS_HOURS(IND,R_MONTH) + 1
               MONTHLY_STRIKES(IND,R_MONTH) = &
                                        MONTHLY_STRIKES(IND,R_MONTH) + 1
               STRIKES_AVAILABLE(IND) = STRIKES_AVAILABLE(IND) - 1
! BUY/SELL POSITION ACCUMULATED MONTHLY.
!
! 12/21/01. IF SELLING A DERIVATIVE PRODUCT, THEN THE BUYER IS PAYING
! FOR THE OPTIONALITY.
! FOR INSTANCE, SELLING A CALL OPTION MEANS THAT WHEN IT IS CALLED, !
! YOU MUST SERVE THE LOAD.

! THIS IS CONSISTENT WITH THE FORWARD SALE.  A SALE INCREASES DEMAND
! ON THE SYSTEM.

! 4/19/02. CHANGED TO LOCAL_ASSIGNMENT(1)
!

               LONG_SHORT = LONG_SHORT*CALL_OR_PUT
!
               TEMP_QUANTITY = TEMP_QUANTITY*LONG_SHORT
!
! ALWAYS POSITIVE?
!
               MONTHLY_ENERGY(IND,R_MONTH) = &
                   MONTHLY_ENERGY(IND,R_MONTH) + TEMP_QUANTITY

               TEMP_PRICE_24 = 0.
!
!ASSUMES ALL LF ARE PHYSICAL

               HOURLY_OPTION_REVENUE = 0.
!
               IF(DERIVATIVE_TYPE(IND) < 12) THEN ! PHYSICAL

!
!
! TRANSACTION
!
                  HOURLY_LF_CAPACITY = &
                                      HOURLY_LF_CAPACITY + TEMP_QUANTITY
! 013108. THIS LOOKS LIKE IT HAS BEEN MULTIPLIED TOO MANY TIMES.
               IF(CALL_OR_PUT*LONG_SHORT > 0) THEN
                  REMAINING_WHOLESALE_DEMAND = &
                         REMAINING_WHOLESALE_DEMAND - ABS(TEMP_QUANTITY)
               ELSE
                  REMAINING_WHOLESALE_PURCHASE = &
                       REMAINING_WHOLESALE_PURCHASE - ABS(TEMP_QUANTITY)
               ENDIF
!
! 02/09/04.

                  SELL_OR_BUY = 1.0
!
!                  IF(STRIKE_FREQUENCY(IND) == 'H') THEN ! ASSUMES HOURLY
                  DAILY_OPTION_REVENUE = &
                     CALL_OR_PUT * &
                          PRODUCT_PATTERN(R_HOUR_IN_DAY,PRODUCT) * &
                          (R_HOURLY_PRICE) * &
                          TEMP_QUANTITY

               ENDIF
!
! 10/06/03.
!
! 012508. REVERSED AND CHANGED THE SIGN OF TEMP_QUANTITY FOR KATHY
!
               IF(LONG_SHORT < 0.) THEN
                  MONTHLY_ENERGY_COST(IND,R_MONTH) = &
                     MONTHLY_ENERGY_COST(IND,R_MONTH) - &
                               MONTHLY_ENERGY_PRICE(IND) * TEMP_QUANTITY

                  MONTHLY_ENR_FOR_EXP(IND,R_MONTH) = &
                        MONTHLY_ENR_FOR_EXP(IND,R_MONTH) - TEMP_QUANTITY
               ELSE

                  MONTHLY_ENERGY_REVENUE(IND,R_MONTH) = &
                      MONTHLY_ENERGY_REVENUE(IND,R_MONTH) - &
                               MONTHLY_ENERGY_PRICE(IND) * TEMP_QUANTITY
                  MONTHLY_ENR_FOR_REV(IND,R_MONTH) = &
                        MONTHLY_ENR_FOR_REV(IND,R_MONTH) - TEMP_QUANTITY
               ENDIF
!
! SELL_OR_BUY ALREADY CALCULATED
!
            ENDDO ! TRANSACTIONS
         ENDDO ! CALL OR PUT
      RETURN
!
      END

      FUNCTION GET_DAILY_OPTION_VALUE( &
                                    R_24_HOUR_PRICE, &
                                    R_STRIKE_QUANTITY, &
                                    R_HOUR_ACTIVE, &
                                    R_CALL_OR_PUT, &
                                    R_STRIKE_PRICE)
      use end_routine
      REAL(kind=4) :: GET_DAILY_OPTION_VALUE, &
             R_24_HOUR_PRICE(24), &
             R_HOUR_ACTIVE(24), & !  SOMEBODY HAS DEFINED THE PRODUCT
             R_STRIKE_PRICE(24), & !  11/25/05. MADE HOURLY
             R_CALL_OR_PUT, &
             R_STRIKE_QUANTITY(0:24)
!
! END DATA DEFINITIONS
!
         GET_DAILY_OPTION_VALUE = &
!     +      R_STRIKE_QUANTITY  &
            R_CALL_OR_PUT * ( &
            R_STRIKE_QUANTITY(1) * &
           (R_24_HOUR_PRICE(1) - R_STRIKE_PRICE(1)) * &
                  R_HOUR_ACTIVE(1)  + &
            R_STRIKE_QUANTITY(2) * &
           (R_24_HOUR_PRICE(2) - R_STRIKE_PRICE(2)) * &
                  R_HOUR_ACTIVE(2)  + &
            R_STRIKE_QUANTITY(3) * &
           (R_24_HOUR_PRICE(3) - R_STRIKE_PRICE(3)) * &
                  R_HOUR_ACTIVE(3)  + &
            R_STRIKE_QUANTITY(4) * &
           (R_24_HOUR_PRICE(4) - R_STRIKE_PRICE(4)) * &
                  R_HOUR_ACTIVE(4)  + &
            R_STRIKE_QUANTITY(5) * &
           (R_24_HOUR_PRICE(5) - R_STRIKE_PRICE(5)) * &
                  R_HOUR_ACTIVE(5)  + &
            R_STRIKE_QUANTITY(6) * &
           (R_24_HOUR_PRICE(6) - R_STRIKE_PRICE(6)) * &
                  R_HOUR_ACTIVE(6)  + &
            R_STRIKE_QUANTITY(7) * &
           (R_24_HOUR_PRICE(7) - R_STRIKE_PRICE(7)) * &
                  R_HOUR_ACTIVE(7)  + &
            R_STRIKE_QUANTITY(8) * &
           (R_24_HOUR_PRICE(8) - R_STRIKE_PRICE(8)) * &
                  R_HOUR_ACTIVE(8)  + &
            R_STRIKE_QUANTITY(9) * &
           (R_24_HOUR_PRICE(9) - R_STRIKE_PRICE(9)) * &
                  R_HOUR_ACTIVE(9)  + &
            R_STRIKE_QUANTITY(10) * &
           (R_24_HOUR_PRICE(10) - R_STRIKE_PRICE(10)) * &
                  R_HOUR_ACTIVE(10)  + &
            R_STRIKE_QUANTITY(11) * &
           (R_24_HOUR_PRICE(11) - R_STRIKE_PRICE(11)) * &
                  R_HOUR_ACTIVE(11)  + &
            R_STRIKE_QUANTITY(12) * &
           (R_24_HOUR_PRICE(12) - R_STRIKE_PRICE(12)) * &
                  R_HOUR_ACTIVE(12)  + &
            R_STRIKE_QUANTITY(13) * &
           (R_24_HOUR_PRICE(13) - R_STRIKE_PRICE(13)) * &
                  R_HOUR_ACTIVE(13)  + &
            R_STRIKE_QUANTITY(14) * &
           (R_24_HOUR_PRICE(14) - R_STRIKE_PRICE(14)) * &
                  R_HOUR_ACTIVE(14)  + &
            R_STRIKE_QUANTITY(15) * &
           (R_24_HOUR_PRICE(15) - R_STRIKE_PRICE(15)) * &
                  R_HOUR_ACTIVE(15)  + &
            R_STRIKE_QUANTITY(16) * &
           (R_24_HOUR_PRICE(16) - R_STRIKE_PRICE(16)) * &
                  R_HOUR_ACTIVE(16)  + &
            R_STRIKE_QUANTITY(17) * &
           (R_24_HOUR_PRICE(17) - R_STRIKE_PRICE(17)) * &
                  R_HOUR_ACTIVE(17)  + &
            R_STRIKE_QUANTITY(18) * &
           (R_24_HOUR_PRICE(18) - R_STRIKE_PRICE(18)) * &
                  R_HOUR_ACTIVE(18)  + &
            R_STRIKE_QUANTITY(19) * &
           (R_24_HOUR_PRICE(19) - R_STRIKE_PRICE(19)) * &
                  R_HOUR_ACTIVE(19)  + &
            R_STRIKE_QUANTITY(20) * &
           (R_24_HOUR_PRICE(20) - R_STRIKE_PRICE(20)) * &
                  R_HOUR_ACTIVE(20)  + &
            R_STRIKE_QUANTITY(21) * &
           (R_24_HOUR_PRICE(21) - R_STRIKE_PRICE(21)) * &
                  R_HOUR_ACTIVE(21)  + &
            R_STRIKE_QUANTITY(22) * &
           (R_24_HOUR_PRICE(22) - R_STRIKE_PRICE(22)) * &
                  R_HOUR_ACTIVE(22)  + &
            R_STRIKE_QUANTITY(23) * &
           (R_24_HOUR_PRICE(23) - R_STRIKE_PRICE(23)) * &
                  R_HOUR_ACTIVE(23)  + &
            R_STRIKE_QUANTITY(24) * &
           (R_24_HOUR_PRICE(24) - R_STRIKE_PRICE(24)) * &
                  R_HOUR_ACTIVE(24)  )
      RETURN
      END function get_daily_option_value

      subroutine INDEXED_SORT_ASCENDING(a,Ofs,nItems,Ascending)

!     sorts nItems items into Ascending order(Ofs array)
      INTEGER(kind=2) :: nItems,i,j,k,Gap,Ofs(*),Hold
      LOGICAL(kind=1) :: Ascending ! .false. => Descending order
      REAL(kind=4) :: a(*)

      Gap=nItems/2
      do while(Gap>0)
        do i=Gap+1,nItems
          j=i-Gap
          do while(j>0)
            k=j+Gap
            if((Ascending .and.(a(Ofs(j))<=a(Ofs(k)))).or. &
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
      RETURN
      end ! subroutine IndexedSortAscending

      FUNCTION GET_HOURLY_OPTION_VALUE( &
                                    R_24_HOUR_PRICE, &
                                    R_STRIKE_QUANTITY, &
                                    R_HOUR_ACTIVE, &
                                    R_CALL_OR_PUT, &
                                    R_STRIKE_PRICE, &
                                    R_TEMP_STRIKES)

      INTEGER(kind=2) :: I,R_TEMP_STRIKES
      REAL(kind=4) :: GET_HOURLY_OPTION_VALUE, &
             TEMP_VALUE, &
             R_24_HOUR_PRICE(24), &
             R_HOUR_ACTIVE(24), & !  SOMEBODY HAS DEFINED THE PRODUCT
             R_STRIKE_PRICE(24), & !  11/25/03. MADE HOURLY FROM CONSTANT
             R_CALL_OR_PUT, &
             R_STRIKE_QUANTITY(0:24)
!
! END DATA DEFINITIONS
!
         GET_HOURLY_OPTION_VALUE = 0.
         R_TEMP_STRIKES = 0
         DO I = 1, 24
            TEMP_VALUE = &
               R_STRIKE_QUANTITY(I) * R_CALL_OR_PUT * &
               (R_24_HOUR_PRICE(I) - R_STRIKE_PRICE(I))*R_HOUR_ACTIVE(I)
            IF(TEMP_VALUE > 0.) THEN
               GET_HOURLY_OPTION_VALUE = GET_HOURLY_OPTION_VALUE + &
                                                              TEMP_VALUE
               R_TEMP_STRIKES = R_TEMP_STRIKES + 1
            ELSE
               R_STRIKE_QUANTITY(I) = 0.
            ENDIF
         ENDDO
      RETURN
      END
!
!
      FUNCTION APPLY_ENERGY_PRODUCT(R_HOUR_IN_DAY,R_DAY_IN_WEEK, &
                                                         R_PRODUCT_TYPE)
      use end_routine, only: end_program, er_message
!

!
      LOGICAL(kind=1) :: APPLY_ENERGY_PRODUCT,ON_PEAK,SUPER_PEAK
      INTEGER(kind=2) :: R_HOUR_IN_DAY,R_DAY_IN_WEEK
      CHARACTER(len=20) :: R_PRODUCT_TYPE
!
! END DATA DECLARATIONS
!
! BRING-IN TIME ZONE FOR THE REGION
!
         APPLY_ENERGY_PRODUCT = .FALSE.
!
         IF(R_HOUR_IN_DAY > 6 .AND. R_HOUR_IN_DAY < 23) THEN
            ON_PEAK = .TRUE.
         ELSE
            ON_PEAK = .FALSE.
         ENDIF
         IF(R_HOUR_IN_DAY > 12 .AND. R_HOUR_IN_DAY < 21) THEN
            SUPER_PEAK = .TRUE.
         ELSE
            SUPER_PEAK = .FALSE.
         ENDIF
         IF(    trim(R_PRODUCT_TYPE) == '5x16') THEN ! EASTERN MARKET
            IF(R_DAY_IN_WEEK > 0 .AND. R_DAY_IN_WEEK < 6 .AND. &
                                                           ON_PEAK) THEN
               APPLY_ENERGY_PRODUCT = .TRUE.
            ENDIF
         ELSEIF(trim(R_PRODUCT_TYPE) == '6x16') THEN ! WSCC MARKET
            IF(R_DAY_IN_WEEK > 0 .AND. R_DAY_IN_WEEK < 7 .AND. &
                                                           ON_PEAK) THEN
               APPLY_ENERGY_PRODUCT = .TRUE.
            ENDIF
         ELSEIF(trim(R_PRODUCT_TYPE) == '7x24') THEN ! ALL HOURS
            APPLY_ENERGY_PRODUCT = .TRUE.
         ELSEIF(trim(R_PRODUCT_TYPE) == '5x8') THEN ! OFFPEAK
            IF(R_DAY_IN_WEEK > 0 .AND. R_DAY_IN_WEEK < 6 .AND. &
                                                     .NOT. ON_PEAK) THEN
               APPLY_ENERGY_PRODUCT = .TRUE.
            ENDIF
         ELSEIF(trim(R_PRODUCT_TYPE) == 'Wrap') THEN ! (5x8) + (2x24)
            IF( (R_DAY_IN_WEEK > 0 .AND. R_DAY_IN_WEEK < 6 .AND. &
                                                    .NOT. ON_PEAK) .OR. &
                                                 R_DAY_IN_WEEK > 5) THEN
               APPLY_ENERGY_PRODUCT = .TRUE.
            ENDIF
         ELSEIF(trim(R_PRODUCT_TYPE) == 'Western Wrap') THEN
            IF( (R_DAY_IN_WEEK > 0 .AND. R_DAY_IN_WEEK < 7 .AND. &
                                                    .NOT. ON_PEAK) .OR. &
                                                 R_DAY_IN_WEEK > 6) THEN
               APPLY_ENERGY_PRODUCT = .TRUE.
            ENDIF
         ELSEIF(trim(R_PRODUCT_TYPE) == '2x24') THEN !  (2x24)
            IF( (R_DAY_IN_WEEK > 5 .AND. R_DAY_IN_WEEK <= 8)) THEN
               ! 5/6/02: INCLUDES HOLIDAYS
               APPLY_ENERGY_PRODUCT = .TRUE.
            ENDIF
         ELSEIF(trim(R_PRODUCT_TYPE) == 'Sax16') THEN !
            IF( (R_DAY_IN_WEEK == 6) .AND. ON_PEAK) THEN
               APPLY_ENERGY_PRODUCT = .TRUE.
            ENDIF
         ELSEIF(trim(R_PRODUCT_TYPE) == 'Sux16') THEN !
            IF( (R_DAY_IN_WEEK == 7) .AND. ON_PEAK) THEN
               APPLY_ENERGY_PRODUCT = .TRUE.
            ENDIF
         ELSEIF(trim(R_PRODUCT_TYPE) == '6x8') THEN !
            IF( R_DAY_IN_WEEK < 7 .AND. .NOT. ON_PEAK) THEN
               APPLY_ENERGY_PRODUCT = .TRUE.
            ENDIF
         ELSEIF(trim(R_PRODUCT_TYPE) == '7x8') THEN !
            IF( .NOT. ON_PEAK) THEN
               APPLY_ENERGY_PRODUCT = .TRUE.
            ENDIF
         ELSEIF(trim(R_PRODUCT_TYPE) == 'Wkx16') THEN !
            IF( (R_DAY_IN_WEEK > 5 .AND. R_DAY_IN_WEEK < 8) .AND. &
                                                     ON_PEAK) THEN
               APPLY_ENERGY_PRODUCT = .TRUE.
            ENDIF
         ELSEIF(trim(R_PRODUCT_TYPE) == 'Mox16') THEN !
            IF( (R_DAY_IN_WEEK == 1) .AND. ON_PEAK) THEN
               APPLY_ENERGY_PRODUCT = .TRUE.
            ENDIF
         ELSEIF(trim(R_PRODUCT_TYPE) == 'Mox8') THEN !
            IF( (R_DAY_IN_WEEK == 1) .AND. .NOT. ON_PEAK) THEN
               APPLY_ENERGY_PRODUCT = .TRUE.
            ENDIF
         ELSEIF(trim(R_PRODUCT_TYPE) == 'Tux16') THEN !
            IF( (R_DAY_IN_WEEK == 2) .AND. ON_PEAK) THEN
               APPLY_ENERGY_PRODUCT = .TRUE.
            ENDIF
         ELSEIF(trim(R_PRODUCT_TYPE) == 'Tux8') THEN !
            IF( (R_DAY_IN_WEEK == 2) .AND. .NOT. ON_PEAK) THEN
               APPLY_ENERGY_PRODUCT = .TRUE.
            ENDIF
         ELSEIF(trim(R_PRODUCT_TYPE) == 'Wex16') THEN !
            IF( (R_DAY_IN_WEEK == 3) .AND. ON_PEAK) THEN
               APPLY_ENERGY_PRODUCT = .TRUE.
            ENDIF
         ELSEIF(trim(R_PRODUCT_TYPE) == 'Wex8') THEN !
            IF( (R_DAY_IN_WEEK == 3) .AND. .NOT. ON_PEAK) THEN
               APPLY_ENERGY_PRODUCT = .TRUE.
            ENDIF
         ELSEIF(trim(R_PRODUCT_TYPE) == 'Thx16') THEN !
            IF( (R_DAY_IN_WEEK == 4) .AND. ON_PEAK) THEN
               APPLY_ENERGY_PRODUCT = .TRUE.
            ENDIF
         ELSEIF(trim(R_PRODUCT_TYPE) == 'Thx8') THEN !
            IF( (R_DAY_IN_WEEK == 4) .AND. .NOT. ON_PEAK) THEN
               APPLY_ENERGY_PRODUCT = .TRUE.
            ENDIF
         ELSEIF(trim(R_PRODUCT_TYPE) == 'Frx16') THEN !
            IF( (R_DAY_IN_WEEK == 5) .AND. ON_PEAK) THEN
               APPLY_ENERGY_PRODUCT = .TRUE.
            ENDIF
         ELSEIF(trim(R_PRODUCT_TYPE) == 'Frx8') THEN !
            IF( (R_DAY_IN_WEEK == 5) .AND. .NOT. ON_PEAK) THEN
               APPLY_ENERGY_PRODUCT = .TRUE.
            ENDIF
         ELSEIF(trim(R_PRODUCT_TYPE) == 'Super Peak') THEN !
            IF( (R_DAY_IN_WEEK > 0 .AND. R_DAY_IN_WEEK < 7) .AND. &
                                                       SUPER_PEAK ) THEN
               APPLY_ENERGY_PRODUCT = .TRUE.
            ENDIF
         ELSEIF(trim(R_PRODUCT_TYPE) == '7x16') THEN
            IF( ON_PEAK) THEN
               APPLY_ENERGY_PRODUCT = .TRUE.
            ENDIF
         ELSEIF(trim(R_PRODUCT_TYPE) == 'Wkx8') THEN
            IF( (R_DAY_IN_WEEK > 5 .AND. R_DAY_IN_WEEK < 8) .AND. &
                                                     .NOT. ON_PEAK) THEN
               APPLY_ENERGY_PRODUCT = .TRUE.
            ENDIF
         ELSE
            WRITE(4,*) '*** line 11854 TRANSOBJ.FOR ***'
            WRITE(4,*) "Undentified Product"
            er_message='See WARNING MESSAGES -TRANSOBJ2.FOR-2'
            call end_program(er_message)
         ENDIF
      RETURN
      END function APPLY_ENERGY_PRODUCT

! for speed-critical uses, disable the next two subroutines here and
! link in the .OBJ files from the corresponding assembly-language code

      SUBROUTINE AccumVector(X,ACCUM,N)
      REAL(kind=4) :: X(:),ACCUM
      INTEGER(kind=4) :: N,I
      DO I=N,1,-1
        ACCUM=ACCUM+X(I) ! caller is responsible for initializing ACCUM
      END DO
      END

      SUBROUTINE AugmentVectByProductOf(MULTCAND,MULPLIER,VECTOR,SIGN,N)
      REAL(kind=4) :: MULTCAND(*),MULPLIER(*),VECTOR(*)
      INTEGER(kind=4) :: SIGN,N,I
      IF(SIGN==1) THEN
        DO I=N,1,-1
          VECTOR(I)=VECTOR(I)+MULTCAND(I)*MULPLIER(I)
        END DO
      ELSE ! assume SIGN==-1
        DO I=N,1,-1
          VECTOR(I)=VECTOR(I)-MULTCAND(I)*MULPLIER(I)
        END DO
      END IF
      END
!
!

!
!                ROUTINE TO CONVERT MARKET_PRICE FILE
!
!                           COPYRIGHT (C) 1997
!                        M.S. GERBER & ASSOCIATES, INC.
!                           ALL RIGHTS RESERVED
!

!
!
! 8/7/97. GAT. INITIALLY USED TO REMEMBER MARKET PRICE FILE BASE AND
!              OVERLAY NAMES.
!
      SUBROUTINE MARKET_PRICE_OBJECT
!
      use spindriftlib
      use prod_arrays_dimensions
      USE SIZECOM
      CHARACTER(len=5) :: BASE_FILE_NAME='NONE ',OVERLAY_FAMILY_NAME, &
                  MARKET_PRICE_FILE,SAVE_FILE_NAME,R_MARKET_PRICE_NAME
      CHARACTER(len=4) :: MARKET_PRICE_NAME
      CHARACTER(len=256) :: FILE_NAME
      CHARACTER(len=256) :: BASE_FILE_DIRECTORY
      CHARACTER(len=256)::DATA_DRIVE,OUTPUT_DIRECTORY,PRB_FILE_DIRECTORY
      LOGICAL(kind=4) :: FILE_EXISTS
! DECLARATION FOR DAY TYPE DETERMINANTS
      CHARACTER(len=16) :: FILE_TYPE='Market Prices   '
      LOGICAL(kind=1) :: LAHEY_LF95
      CHARACTER(len=30) :: SCREEN_OUTPUT
      SAVE SAVE_FILE_NAME

! CONVERT THE MARKET_PRICE FILE

      ENTRY MARKET_PRICE_MAKEBIN

      BASE_FILE_NAME = MARKET_PRICE_FILE()
! CHANGE 4/24/02.
      DATA_DRIVE = PRB_FILE_DIRECTORY()
      FILE_NAME = trim(DATA_DRIVE)// &
                                   "LDE"//trim(BASE_FILE_NAME)//'.BIN'
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      SAVE_FILE_NAME = BASE_FILE_NAME
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_LOCATE_WRITE(16,30,BASE_FILE_NAME,ALL_VERSIONS,0)
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         ENDIF
         SAVE_FILE_NAME = BASE_FILE_NAME
      ELSEIF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "transobj2:1119")
      ENDIF
      RETURN



! OVERLAY THE DAY TYPE FILE

      ENTRY MARKET_PRICE_MAKEOVL(OVERLAY_FAMILY_NAME)


      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME = trim(DATA_DRIVE)// &
                              "LDE"//trim(OVERLAY_FAMILY_NAME)//'.BIN'
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      IF(FILE_EXISTS) THEN
         SAVE_FILE_NAME = OVERLAY_FAMILY_NAME

      ENDIF
      RETURN

      ENTRY RESET_MARKET_PRICES

         SAVE_FILE_NAME = BASE_FILE_NAME
      RETURN

      ENTRY GET_MARKET_PRICE_NAME(R_MARKET_PRICE_NAME)

         R_MARKET_PRICE_NAME = SAVE_FILE_NAME
      RETURN
!
 1010 FORMAT('&',A)
!
      END
!
!

!
!                ROUTINE TO CONVERT NEW_MARKET_PRICE FILE
!
!                           COPYRIGHT (C) 1997
!                        M.S. GERBER & ASSOCIATES, INC.
!                           ALL RIGHTS RESERVED
!

!
!
! 8/7/97. GAT. INITIALLY USED TO REMEMBER MARKET PRICE FILE BASE AND
!              OVERLAY NAMES.
!
      SUBROUTINE NEW_MARKET_PRICE_OBJECT
!
      use spindriftlib
      use prod_arrays_dimensions
      USE SIZECOM
      CHARACTER(len=5) :: BASE_FILE_NAME='NONE ',OVERLAY_FAMILY_NAME, &
                  NEW_MARKET_PRICE_FILE,SAVE_FILE_NAME, &
                  R_NEW_MARKET_PRICE_NAME
      CHARACTER(len=4) :: NEW_MARKET_PRICE_NAME
      CHARACTER(len=256) :: FILE_NAME
      CHARACTER(len=256) :: BASE_FILE_DIRECTORY,PRB_FILE_DIRECTORY
      CHARACTER(len=256) :: DATA_DRIVE,OUTPUT_DIRECTORY
      LOGICAL(kind=4) :: FILE_EXISTS
! DECLARATION FOR DAY TYPE DETERMINANTS
      CHARACTER(len=16) :: FILE_TYPE='Market Prices   '
      LOGICAL(kind=1) :: LAHEY_LF95
      CHARACTER(len=30) :: SCREEN_OUTPUT
      SAVE SAVE_FILE_NAME

! CONVERT THE NEW_MARKET_PRICE FILE

      ENTRY NEW_MARKET_PRICE_MAKEBIN

      BASE_FILE_NAME = NEW_MARKET_PRICE_FILE()
      DATA_DRIVE = PRB_FILE_DIRECTORY()
      FILE_NAME = trim(DATA_DRIVE)// &
                                   "PRB"//trim(BASE_FILE_NAME)//'.PIN'
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      IF(LAHEY_LF95() .AND. FILE_EXISTS) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSEIF(FILE_EXISTS) THEN
         CALL MG_LOCATE_WRITE(16,30,BASE_FILE_NAME,ALL_VERSIONS,0)
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
      ENDIF
      SAVE_FILE_NAME = BASE_FILE_NAME

      RETURN
!
! OVERLAY THE DAY TYPE FILE

      ENTRY NEW_MARKET_PRICE_MAKEOVL(OVERLAY_FAMILY_NAME)

!
      DATA_DRIVE = PRB_FILE_DIRECTORY()
      FILE_NAME = trim(DATA_DRIVE)// &
                              "PRB"//trim(OVERLAY_FAMILY_NAME)//'.PIN'
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)

         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
            CALL LOCATE(10,51)
         ENDIF
         SAVE_FILE_NAME = OVERLAY_FAMILY_NAME

      RETURN

      ENTRY RESET_NEW_MARKET_PRICES

         SAVE_FILE_NAME = BASE_FILE_NAME
      RETURN

      ENTRY GET_NEW_MARKET_PRICE_NAME(R_NEW_MARKET_PRICE_NAME)

         R_NEW_MARKET_PRICE_NAME = SAVE_FILE_NAME
      RETURN
!
 1010 FORMAT('&',A)
!
      END

!
!                ROUTINE TO CONVERT ELECT_HOUR_MULT FILE
!
!                           COPYRIGHT (C) 2003
!                        M.S. GERBER & ASSOCIATES, INC.
!                           ALL RIGHTS RESERVED
!

!
!
! 8/7/97. GAT. INITIALLY USED TO REMEMBER MARKET PRICE FILE BASE AND
!              OVERLAY NAMES.
!
      SUBROUTINE ELECT_HOUR_MULT_OBJECT
!
      use spindriftlib
      use prod_arrays_dimensions
      USE SIZECOM
      CHARACTER(len=5) :: BASE_FILE_NAME='NONE ',OVERLAY_FAMILY_NAME, &
                  ELECT_HOUR_MULT_FILE,SAVE_FILE_NAME, &
                  R_ELECT_HOUR_MULT_NAME
      CHARACTER(len=4) :: ELECT_HOUR_MULT_NAME
      CHARACTER(len=256) :: FILE_NAME
      CHARACTER(len=256) :: BASE_FILE_DIRECTORY,SHB_FILE_DIRECTORY
      CHARACTER(len=256) :: DATA_DRIVE,OUTPUT_DIRECTORY
      LOGICAL(kind=4) :: FILE_EXISTS
! DECLARATION FOR DAY TYPE DETERMINANTS
      CHARACTER(len=16) :: FILE_TYPE='Elect Hour Mult '
      LOGICAL(kind=1) :: LAHEY_LF95
      CHARACTER(len=30) :: SCREEN_OUTPUT
      SAVE SAVE_FILE_NAME

! CONVERT THE ELECT_HOUR_MULT FILE

      ENTRY ELECT_HOUR_MULT_MAKEBIN

      BASE_FILE_NAME = ELECT_HOUR_MULT_FILE()
      DATA_DRIVE = SHB_FILE_DIRECTORY()
      FILE_NAME = trim(DATA_DRIVE)// &
                                   "EHB"//trim(BASE_FILE_NAME)//'.S01'
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      IF(LAHEY_LF95() .AND. FILE_EXISTS) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSEIF(FILE_EXISTS) THEN
         CALL MG_LOCATE_WRITE(16,30,BASE_FILE_NAME,ALL_VERSIONS,0)
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
      ENDIF
      SAVE_FILE_NAME = BASE_FILE_NAME
      IF(.NOT. FILE_EXISTS .AND. INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "transobj2:1118")
      ENDIF
      RETURN
!
! OVERLAY THE DAY TYPE FILE

      ENTRY EH_MAKEOVL(OVERLAY_FAMILY_NAME)

!
      DATA_DRIVE = SHB_FILE_DIRECTORY()
      FILE_NAME = trim(DATA_DRIVE)// &
                              "EHB"//trim(OVERLAY_FAMILY_NAME)//'.S01'
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
            CALL LOCATE(10,51)
         ENDIF
         SAVE_FILE_NAME = OVERLAY_FAMILY_NAME

      ENDIF
      RETURN

      ENTRY RESET_ELECT_HOUR_MULT

         SAVE_FILE_NAME = BASE_FILE_NAME
      RETURN

      ENTRY GET_ELECT_HOUR_MULT_NAME(R_ELECT_HOUR_MULT_NAME)

         R_ELECT_HOUR_MULT_NAME = SAVE_FILE_NAME
      RETURN
!
 1010 FORMAT('&',A)
!
      END

!
      RECURSIVE FUNCTION MANAGE_ELECT_HOUR_MULT()
!

      SAVE
      INTEGER(kind=2) :: R_HOUR,R_MONTH,MONTH,R_YEAR,R_END_POINT, &
                DA,DAYS_IN_MONTH,I,IREC, &
                ALINE_LOAD_DATA,HR,START_HR,END_HR
      INTEGER(kind=2) :: TIMZON,TEMPER,DELTMP, &
                LDE_YEAR,LDE_DAY,LDE_MONTH, &
                DAY_WEEK
      REAL(kind=4) ::HOURLY_SCEN_ELECT_MULT(744,12), &
               GET_HOURLY_SCEN_ELECT_MULT
      LOGICAL(kind=1) :: ANN_HOURLY_SCEN_ELECT_MULT, &
               MANAGE_ELECT_HOUR_MULT
      LOGICAL(kind=4) :: FILE_EXISTS
      CHARACTER(len=2) :: LOAD_FILE_CHAR_EXT
      CHARACTER(len=5) :: ELECT_HOUR_MULT_NAME
      CHARACTER(len=8) :: EEICODE
      CHARACTER(len=256) :: FILE_NAME
      CHARACTER(len=256) :: BASE_FILE_DIRECTORY,SHB_FILE_DIRECTORY
!
! END DATA DECLARATIONS
!
         MANAGE_ELECT_HOUR_MULT = .TRUE.
      RETURN

      ENTRY ANN_HOURLY_SCEN_ELECT_MULT(R_YEAR)

         ANN_HOURLY_SCEN_ELECT_MULT = .FALSE.
         CALL GET_ELECT_HOUR_MULT_NAME(ELECT_HOUR_MULT_NAME)
         HOURLY_SCEN_ELECT_MULT = 1.
         IF(INDEX(ELECT_HOUR_MULT_NAME,'NONE') == 0) THEN
            FILE_NAME = trim(SHB_FILE_DIRECTORY())//"EHB"// &
                                    trim(ELECT_HOUR_MULT_NAME)//".S"// &
                                              LOAD_FILE_CHAR_EXT(R_YEAR)
            INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
            IF(FILE_EXISTS) THEN
               ANN_HOURLY_SCEN_ELECT_MULT = .TRUE.
               OPEN(UNIT=2801,FILE=FILE_NAME,RECL=118, &
                                           ACCESS="DIRECT",STATUS="OLD")
!
! ASSUMES 369 RECORD CONVENTION
!
               DO MONTH = 1, 12
                  DAYS_IN_MONTH = MONTH+1
                  DAYS_IN_MONTH = ALINE_LOAD_DATA(I,DAYS_IN_MONTH) - &
                                                ALINE_LOAD_DATA(I,MONTH)
                  I = 1
                  IREC = ALINE_LOAD_DATA(I,MONTH)
                  DO DA = 1, DAYS_IN_MONTH
                     START_HR = (DA-1)*24+1
                     END_HR = START_HR + 23
                     READ(2801,REC=IREC) &
                                  LDE_MONTH,LDE_DAY,LDE_YEAR, &
                                  EEICODE,DAY_WEEK, &
                                  TIMZON,TEMPER, &
                                  DELTMP, &
                              (HOURLY_SCEN_ELECT_MULT(HR,MONTH), &
                                                     HR=START_HR,END_HR)
                     IREC = IREC + 1
                  ENDDO
               ENDDO
               CLOSE(2801)
            ENDIF ! FILE EXISTS
         ENDIF ! NAME IN OVERLAY OR BASE FILE
      RETURN

      ENTRY GET_HOURLY_SCEN_ELECT_MULT(R_HOUR,R_MONTH)

         GET_HOURLY_SCEN_ELECT_MULT = HOURLY_SCEN_ELECT_MULT( &
                                                         R_HOUR,R_MONTH)
      RETURN
      END

!
      RECURSIVE FUNCTION MANAGE_ELECT_HOUR_TG_MULT()
!

      SAVE
      INTEGER(kind=2) :: R_HOUR,R_MONTH,MONTH,R_YEAR,R_END_POINT, &
                DA,DAYS_IN_MONTH,I,IREC,R_TG,TG, &
                ALINE_LOAD_DATA,HR,START_HR,END_HR
      INTEGER(kind=2) :: TIMZON,TEMPER,DELTMP, &
                LDE_YEAR,LDE_DAY,LDE_MONTH, &
                DAY_WEEK
      INTEGER :: VALUES_2_ZERO
      REAL(kind=4) :: HRLY_TG_SCEN_ELECT_MULT(:,:,:), &
               GET_HRLY_TG_SCEN_ELECT_MULT
      ALLOCATABLE :: HRLY_TG_SCEN_ELECT_MULT
      LOGICAL(kind=1) :: ANN_HRLY_TG_SCEN_ELECT_MULT, &
               MANAGE_ELECT_HOUR_TG_MULT, &
               TEMP_L, &
               GET_ST_LHS_FOR_PRICES
      CHARACTER(len=2) :: ST_LHS_FOR_PRICES, &
                  LOAD_FILE_CHAR_EXT
      CHARACTER(len=3) :: SCEN_FILE_CHAR_EXT
      LOGICAL(kind=4) :: FILE_EXISTS
      CHARACTER(len=8) :: EEICODE
      CHARACTER(len=256) :: FILE_NAME
      CHARACTER(len=256) :: BASE_FILE_DIRECTORY,SHB_FILE_DIRECTORY
!
! END DATA DECLARATIONS
!
         MANAGE_ELECT_HOUR_TG_MULT = .TRUE.
      RETURN

      ! R_TG HERE MEANS NUMBER OF TG
      ENTRY ANN_HRLY_TG_SCEN_ELECT_MULT(R_YEAR,R_TG,R_END_POINT)

         ANN_HRLY_TG_SCEN_ELECT_MULT = .FALSE.
         IF(ALLOCATED(HRLY_TG_SCEN_ELECT_MULT)) &
                                     DEALLOCATE(HRLY_TG_SCEN_ELECT_MULT)
         ALLOCATE(HRLY_TG_SCEN_ELECT_MULT(744,12,R_TG))
!
         HRLY_TG_SCEN_ELECT_MULT = 1.0
         DO TG = 1, R_TG
            TEMP_L = GET_ST_LHS_FOR_PRICES(ST_LHS_FOR_PRICES,TG)
!
            FILE_NAME = trim(SHB_FILE_DIRECTORY())// &
                                    "EHB"// &
                                    trim(ST_LHS_FOR_PRICES)// &
                                    SCEN_FILE_CHAR_EXT(R_END_POINT)// &
                                    ".S"// &
                                    LOAD_FILE_CHAR_EXT(R_YEAR)
            INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
            IF(FILE_EXISTS) THEN
               ANN_HRLY_TG_SCEN_ELECT_MULT = .TRUE.
               OPEN(UNIT=2801,FILE=FILE_NAME,RECL=118, &
                                           ACCESS="DIRECT",STATUS="OLD")
!
! ASSUMES 369 RECORD CONVENTION
!
               DO MONTH = 1, 12
                  DAYS_IN_MONTH = MONTH+1
                  DAYS_IN_MONTH = ALINE_LOAD_DATA(I,DAYS_IN_MONTH) - &
                                                ALINE_LOAD_DATA(I,MONTH)
                  I = 1
                  IREC = ALINE_LOAD_DATA(I,MONTH)
                  DO DA = 1, DAYS_IN_MONTH
                     START_HR = (DA-1)*24+1
                     END_HR = START_HR + 23
                     READ(2801,REC=IREC) &
                                  LDE_MONTH,LDE_DAY,LDE_YEAR, &
                                  EEICODE,DAY_WEEK, &
                                  TIMZON,TEMPER, &
                                  DELTMP, &
                              (HRLY_TG_SCEN_ELECT_MULT(HR,MONTH,TG), &
                                                     HR=START_HR,END_HR)
                     IREC = IREC + 1
                  ENDDO
               ENDDO
               CLOSE(2801)
            ENDIF ! FILE EXISTS
         ENDDO ! TRANSACTION GROUPS
      RETURN

      ENTRY GET_HRLY_TG_SCEN_ELECT_MULT(R_HOUR,R_MONTH,R_TG)

         GET_HRLY_TG_SCEN_ELECT_MULT = HRLY_TG_SCEN_ELECT_MULT( &
                                                    R_HOUR,R_MONTH,R_TG)
      RETURN
      END

!
      RECURSIVE FUNCTION MANAGE_GAS_HOUR_MULT()
!

      SAVE
      INTEGER(kind=2) :: R_HOUR,R_MONTH,MONTH,R_YEAR,R_END_POINT, &
                DA,DAYS_IN_MONTH,I,IREC, &
                ALINE_LOAD_DATA,HR,START_HR,END_HR
      INTEGER(kind=2) :: TIMZON,TEMPER,DELTMP, &
                LDE_YEAR,LDE_DAY,LDE_MONTH, &
                DAY_WEEK
      REAL(kind=4) :: HOURLY_SCEN_GAS_MULT(744,12), &
               GET_HOURLY_SCEN_GAS_MULT
      LOGICAL(kind=1) :: ANN_HOURLY_SCEN_GAS_MULT, &
               MANAGE_GAS_HOUR_MULT
      LOGICAL(kind=4) :: FILE_EXISTS
      CHARACTER(len=2) :: LOAD_FILE_CHAR_EXT
      CHARACTER(len=5) :: GAS_HOUR_MULT_NAME
      CHARACTER(len=8) :: EEICODE
      CHARACTER(len=256) :: FILE_NAME
      CHARACTER(len=256) :: BASE_FILE_DIRECTORY,SHB_FILE_DIRECTORY
!
! END DATA DECLARATIONS
!
         MANAGE_GAS_HOUR_MULT = .TRUE.
      RETURN

      ENTRY ANN_HOURLY_SCEN_GAS_MULT(R_YEAR)

         ANN_HOURLY_SCEN_GAS_MULT = .FALSE.
         CALL GET_GAS_HOUR_MULT_NAME(GAS_HOUR_MULT_NAME)
         HOURLY_SCEN_GAS_MULT = 1.
         IF(INDEX(GAS_HOUR_MULT_NAME,'NONE') == 0) THEN
            FILE_NAME = trim(SHB_FILE_DIRECTORY())//"GHB"// &
                                    trim(GAS_HOUR_MULT_NAME)//".S"// &
                                              LOAD_FILE_CHAR_EXT(R_YEAR)
            INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
            IF(FILE_EXISTS) THEN
               ANN_HOURLY_SCEN_GAS_MULT = .TRUE.
               OPEN(UNIT=2801,FILE=FILE_NAME,RECL=118, &
                                           ACCESS="DIRECT",STATUS="OLD")
!
! ASSUMES 369 RECORD CONVENTION
!
               DO MONTH = 1, 12
                  DAYS_IN_MONTH = MONTH+1
                  DAYS_IN_MONTH = ALINE_LOAD_DATA(I,DAYS_IN_MONTH) - &
                                                ALINE_LOAD_DATA(I,MONTH)
                  I = 1
                  IREC = ALINE_LOAD_DATA(I,MONTH)
                  DO DA = 1, DAYS_IN_MONTH
                     START_HR = (DA-1)*24+1
                     END_HR = START_HR + 23
                     READ(2801,REC=IREC) &
                                  LDE_MONTH,LDE_DAY,LDE_YEAR, &
                                  EEICODE,DAY_WEEK, &
                                  TIMZON,TEMPER, &
                                  DELTMP, &
                              (HOURLY_SCEN_GAS_MULT(HR,MONTH), &
                                                     HR=START_HR,END_HR)
                     IREC = IREC + 1
                  ENDDO
               ENDDO
               CLOSE(2801)
            ENDIF ! FILE EXISTS
         ENDIF ! NAME IN OVERLAY OR BASE FILE
      RETURN

      ENTRY GET_HOURLY_SCEN_GAS_MULT(R_HOUR,R_MONTH)

         GET_HOURLY_SCEN_GAS_MULT = HOURLY_SCEN_GAS_MULT( &
                                                         R_HOUR,R_MONTH)
      RETURN
      END

!
      RECURSIVE FUNCTION MANAGE_OIL_HOUR_MULT()
!

      SAVE
      INTEGER(kind=2) :: R_HOUR,R_MONTH,MONTH,R_YEAR,R_END_POINT, &
                DA,DAYS_IN_MONTH,I,IREC, &
                ALINE_LOAD_DATA,HR,START_HR,END_HR
      INTEGER(kind=2) :: TIMZON,TEMPER,DELTMP, &
                LDE_YEAR,LDE_DAY,LDE_MONTH, &
                DAY_WEEK
      REAL(kind=4) :: HOURLY_SCEN_OIL_MULT(744,12), &
               GET_HOURLY_SCEN_OIL_MULT
      LOGICAL(kind=1) :: ANN_HOURLY_SCEN_OIL_MULT, &
               MANAGE_OIL_HOUR_MULT
      LOGICAL(kind=4) :: FILE_EXISTS
      CHARACTER(len=2) :: LOAD_FILE_CHAR_EXT
      CHARACTER(len=5) :: OIL_HOUR_MULT_NAME
      CHARACTER(len=8) :: EEICODE
      CHARACTER(len=256) :: FILE_NAME
      CHARACTER(len=256) :: BASE_FILE_DIRECTORY,SHB_FILE_DIRECTORY
!
! END DATA DECLARATIONS
!
         MANAGE_OIL_HOUR_MULT = .TRUE.
      RETURN

      ENTRY ANN_HOURLY_SCEN_OIL_MULT(R_YEAR)

         ANN_HOURLY_SCEN_OIL_MULT = .FALSE.
         CALL GET_OIL_HOUR_MULT_NAME(OIL_HOUR_MULT_NAME)
         HOURLY_SCEN_OIL_MULT = 1.
         IF(INDEX(OIL_HOUR_MULT_NAME,'NONE') == 0) THEN
            FILE_NAME = trim(SHB_FILE_DIRECTORY())//"OHB"// &
                                    trim(OIL_HOUR_MULT_NAME)//".S"// &
                                              LOAD_FILE_CHAR_EXT(R_YEAR)
            INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
            IF(FILE_EXISTS) THEN
               ANN_HOURLY_SCEN_OIL_MULT = .TRUE.
               OPEN(UNIT=2801,FILE=FILE_NAME,RECL=118, &
                                           ACCESS="DIRECT",STATUS="OLD")
!
! ASSUMES 369 RECORD CONVENTION
!
               DO MONTH = 1, 12
                  DAYS_IN_MONTH = MONTH+1
                  DAYS_IN_MONTH = ALINE_LOAD_DATA(I,DAYS_IN_MONTH) - &
                                                ALINE_LOAD_DATA(I,MONTH)
                  I = 1
                  IREC = ALINE_LOAD_DATA(I,MONTH)
                  DO DA = 1, DAYS_IN_MONTH
                     START_HR = (DA-1)*24+1
                     END_HR = START_HR + 23
                     READ(2801,REC=IREC) &
                                  LDE_MONTH,LDE_DAY,LDE_YEAR, &
                                  EEICODE,DAY_WEEK, &
                                  TIMZON,TEMPER, &
                                  DELTMP, &
                              (HOURLY_SCEN_OIL_MULT(HR,MONTH), &
                                                     HR=START_HR,END_HR)
                     IREC = IREC + 1
                  ENDDO
               ENDDO
               CLOSE(2801)
            ENDIF ! FILE EXISTS
         ENDIF ! NAME IN OVERLAY OR BASE FILE
      RETURN

      ENTRY GET_HOURLY_SCEN_OIL_MULT(R_HOUR,R_MONTH)

         GET_HOURLY_SCEN_OIL_MULT = HOURLY_SCEN_OIL_MULT( &
                                                         R_HOUR,R_MONTH)
      RETURN
      END

!
!                ROUTINE TO CONVERT GAS_HOUR_MULT FILE
!
!                           COPYRIGHT (C) 2003
!                        M.S. GERBER & ASSOCIATES, INC.
!                           ALL RIGHTS RESERVED
!

!
!
! 8/7/97. GAT. INITIALLY USED TO REMEMBER MARKET PRICE FILE BASE AND
!              OVERLAY NAMES.
!
      SUBROUTINE GAS_HOUR_MULT_OBJECT
!
      use spindriftlib
      use prod_arrays_dimensions
      USE SIZECOM
      CHARACTER(len=5) :: BASE_FILE_NAME='NONE ',OVERLAY_FAMILY_NAME, &
                  GAS_HOUR_MULT_FILE,SAVE_FILE_NAME, &
                  R_GAS_HOUR_MULT_NAME
      CHARACTER(len=4) :: GAS_HOUR_MULT_NAME
      CHARACTER(len=256) :: FILE_NAME
      CHARACTER(len=256) :: BASE_FILE_DIRECTORY,SHB_FILE_DIRECTORY
      CHARACTER(len=256) :: DATA_DRIVE,OUTPUT_DIRECTORY
      LOGICAL(kind=4) :: FILE_EXISTS
! DECLARATION FOR DAY TYPE DETERMINANTS
      CHARACTER(len=16) :: FILE_TYPE='Gas Hour Mult   '
      LOGICAL(kind=1) :: LAHEY_LF95
      CHARACTER(len=30) :: SCREEN_OUTPUT
      SAVE SAVE_FILE_NAME

! CONVERT THE GAS_HOUR_MULT FILE

      ENTRY GAS_HOUR_MULT_MAKEBIN

      BASE_FILE_NAME = GAS_HOUR_MULT_FILE()
      DATA_DRIVE = SHB_FILE_DIRECTORY()
      FILE_NAME = trim(DATA_DRIVE)// &
                                   "GHB"//trim(BASE_FILE_NAME)//'.S01'
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      IF(LAHEY_LF95() .AND. FILE_EXISTS) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSEIF(FILE_EXISTS) THEN
         CALL MG_LOCATE_WRITE(16,30,BASE_FILE_NAME,ALL_VERSIONS,0)
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
      ENDIF
      SAVE_FILE_NAME = BASE_FILE_NAME
      IF(.NOT. FILE_EXISTS .AND. INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "transobj2:1117")
      ENDIF
      RETURN
!
! OVERLAY THE DAY TYPE FILE

      ENTRY GH_MAKEOVL(OVERLAY_FAMILY_NAME)

!
      DATA_DRIVE = SHB_FILE_DIRECTORY()
      FILE_NAME = trim(DATA_DRIVE)// &
                              "GHB"//trim(OVERLAY_FAMILY_NAME)//'.S01'
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
            CALL LOCATE(10,51)
         ENDIF
         SAVE_FILE_NAME = OVERLAY_FAMILY_NAME

      ENDIF
      RETURN

      ENTRY RESET_GAS_HOUR_MULT

         SAVE_FILE_NAME = BASE_FILE_NAME
      RETURN

      ENTRY GET_GAS_HOUR_MULT_NAME(R_GAS_HOUR_MULT_NAME)

         R_GAS_HOUR_MULT_NAME = SAVE_FILE_NAME
      RETURN
!
 1010 FORMAT('&',A)
!
      END
!

!
!                ROUTINE TO CONVERT OIL_HOUR_MULT FILE
!
!                           COPYRIGHT (C) 2003
!                        M.S. GERBER & ASSOCIATES, INC.
!                           ALL RIGHTS RESERVED
!

!
!
! 8/7/97. GAT. INITIALLY USED TO REMEMBER MARKET PRICE FILE BASE AND
!              OVERLAY NAMES.
!
      SUBROUTINE OIL_HOUR_MULT_OBJECT
!
      use spindriftlib
      use prod_arrays_dimensions
      USE SIZECOM
      CHARACTER(len=5) :: BASE_FILE_NAME='NONE ',OVERLAY_FAMILY_NAME, &
                  OIL_HOUR_MULT_FILE,SAVE_FILE_NAME, &
                  R_OIL_HOUR_MULT_NAME
      CHARACTER(len=4) :: OIL_HOUR_MULT_NAME
      CHARACTER(len=256) :: FILE_NAME
      CHARACTER(len=256) :: BASE_FILE_DIRECTORY,SHB_FILE_DIRECTORY
      CHARACTER(len=256) :: DATA_DRIVE,OUTPUT_DIRECTORY
      LOGICAL(kind=4) :: FILE_EXISTS
! DECLARATION FOR DAY TYPE DETERMINANTS
      CHARACTER(len=16) :: FILE_TYPE='OIL Hour Mult   '
      LOGICAL(kind=1) :: LAHEY_LF95
      CHARACTER(len=30) :: SCREEN_OUTPUT
      SAVE SAVE_FILE_NAME

! CONVERT THE OIL_HOUR_MULT FILE

      ENTRY OIL_HOUR_MULT_MAKEBIN

      BASE_FILE_NAME = OIL_HOUR_MULT_FILE()
      DATA_DRIVE = SHB_FILE_DIRECTORY()
      FILE_NAME = trim(DATA_DRIVE)// &
                                   "OHB"//trim(BASE_FILE_NAME)//'.S01'
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      IF(LAHEY_LF95() .AND. FILE_EXISTS) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSEIF(FILE_EXISTS) THEN
         CALL MG_LOCATE_WRITE(16,30,BASE_FILE_NAME,ALL_VERSIONS,0)
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
      ENDIF
      SAVE_FILE_NAME = BASE_FILE_NAME
      IF(.NOT. FILE_EXISTS .AND. INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "transobj2:1116")
      ENDIF
      RETURN
!
! OVERLAY THE DAY TYPE FILE

      ENTRY OH_MAKEOVL(OVERLAY_FAMILY_NAME)

!
      DATA_DRIVE = SHB_FILE_DIRECTORY()
      FILE_NAME = trim(DATA_DRIVE)// &
                              "OHB"//trim(OVERLAY_FAMILY_NAME)//'.S01'
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
            CALL LOCATE(10,51)
         ENDIF
         SAVE_FILE_NAME = OVERLAY_FAMILY_NAME

      ENDIF
      RETURN

      ENTRY RESET_OIL_HOUR_MULT

         SAVE_FILE_NAME = BASE_FILE_NAME
      RETURN

      ENTRY GET_OIL_HOUR_MULT_NAME(R_OIL_HOUR_MULT_NAME)

         R_OIL_HOUR_MULT_NAME = SAVE_FILE_NAME
      RETURN
!
 1010 FORMAT('&',A)
!
      END
!
!

!
!                ROUTINE TO CONVERT SCENARIO_HOURLY FILE
!
!                           COPYRIGHT (C) 1999
!                        M.S. GERBER & ASSOCIATES, INC.
!                           ALL RIGHTS RESERVED
!

!
!
! CREATED 11/7/99. GAT.
!
      SUBROUTINE SCENARIO_HOURLY_OBJECT
!
      use spindriftlib
      use prod_arrays_dimensions
      USE SIZECOM
      CHARACTER(len=5) :: BASE_FILE_NAME='NONE ',OVERLAY_FAMILY_NAME, &
                  SCENARIO_HOURLY_FILE,SAVE_FILE_NAME, &
                  R_SCENARIO_HOURLY_NAME
      CHARACTER(len=4) :: SCENARIO_HOURLY_NAME
      CHARACTER(len=256) :: FILE_NAME
      CHARACTER(len=256) :: SHB_FILE_DIRECTORY
      CHARACTER(len=256) :: DATA_DRIVE,OUTPUT_DIRECTORY
      LOGICAL(kind=4) :: FILE_EXISTS
! DECLARATION FOR DAY TYPE DETERMINANTS
      CHARACTER(len=16) :: FILE_TYPE='Scenario Hourly '
      LOGICAL(kind=1) :: LAHEY_LF95
      CHARACTER(len=30) :: SCREEN_OUTPUT
      SAVE SAVE_FILE_NAME

! CONVERT THE SCENARIO_HOURLY FILE

      ENTRY SCENARIO_HOURLY_MAKEBIN

      BASE_FILE_NAME = SCENARIO_HOURLY_FILE()
      DATA_DRIVE = SHB_FILE_DIRECTORY()
      FILE_NAME = trim(DATA_DRIVE)// &
                                   "SHB"//trim(BASE_FILE_NAME)//'.SIN'
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
!

      IF(LAHEY_LF95() .AND. FILE_EXISTS) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSEIF(FILE_EXISTS) THEN
         CALL MG_LOCATE_WRITE(16,30,BASE_FILE_NAME,ALL_VERSIONS,0)
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
      ENDIF
      SAVE_FILE_NAME = BASE_FILE_NAME
      IF(.NOT. FILE_EXISTS .AND. INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "transobj2:1115")
      ENDIF
      RETURN
!
! OVERLAY THE SCENARIO_HOURLY FILE

      ENTRY SCENARIO_HOURLY_MAKEOVL(OVERLAY_FAMILY_NAME)

!
      DATA_DRIVE = SHB_FILE_DIRECTORY()
      FILE_NAME = trim(DATA_DRIVE)// &
                              "SHB"//trim(OVERLAY_FAMILY_NAME)//'.SIN'
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
            CALL LOCATE(10,51)
         ENDIF
         SAVE_FILE_NAME = OVERLAY_FAMILY_NAME

      ENDIF
      RETURN

      ENTRY RESET_SCENARIO_HOURLY

         SAVE_FILE_NAME = BASE_FILE_NAME
      RETURN

      ENTRY GET_SCENARIO_HOURLY_NAME(R_SCENARIO_HOURLY_NAME)

         R_SCENARIO_HOURLY_NAME = SAVE_FILE_NAME
      RETURN
!
 1010 FORMAT('&',A)
!
      END

!
!                ROUTINE TO CONVERT REFERENCE_LOAD FILE
!
!                           COPYRIGHT (C) 1997
!                        M.S. GERBER & ASSOCIATES, INC.
!                           ALL RIGHTS RESERVED
!

!
!
! 8/7/97. GAT. INITIALLY USED TO REMEMBER MARKET PRICE FILE BASE AND
!              OVERLAY NAMES.
!
      SUBROUTINE REFERENCE_LOAD_OBJECT
!
      use spindriftlib
      use prod_arrays_dimensions
      USE SIZECOM
      CHARACTER(len=5) :: BASE_FILE_NAME='NONE ',OVERLAY_FAMILY_NAME, &
                  REFERENCE_LOAD_FILE,SAVE_FILE_NAME, &
                  R_REFERENCE_LOAD_NAME,TEMP_CHR5,PUT_REFERENCE_LOAD
      CHARACTER(len=4) :: REFERENCE_LOAD_NAME
      CHARACTER(len=256) :: FILE_NAME,SAVE_FILE_DIR
      CHARACTER(len=256) :: BASE_FILE_DIRECTORY,LDE_FILE_DIRECTORY
      CHARACTER(len=256) :: DATA_DRIVE,OUTPUT_DIRECTORY
      LOGICAL(kind=4) :: FILE_EXISTS
! DECLARATION FOR DAY TYPE DETERMINANTS
      CHARACTER(len=16) :: FILE_TYPE='Reference Load  '
      LOGICAL(kind=1) :: LAHEY_LF95
      CHARACTER(len=30) :: SCREEN_OUTPUT
      SAVE SAVE_FILE_NAME,SAVE_FILE_DIR

! CONVERT THE REFERENCE_LOAD FILE

      ENTRY REFERENCE_LOAD_MAKEBIN

      BASE_FILE_NAME = REFERENCE_LOAD_FILE()
      FILE_NAME = trim(BASE_FILE_DIRECTORY())// &
                                   "LDE"//trim(BASE_FILE_NAME)//'.BIN'
      SAVE_FILE_DIR = BASE_FILE_DIRECTORY()
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      IF(.NOT. FILE_EXISTS) THEN
         FILE_NAME = trim(LDE_FILE_DIRECTORY())// &
                                   "LDE"//trim(BASE_FILE_NAME)//'.BIN'
         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
         SAVE_FILE_DIR = LDE_FILE_DIRECTORY()
      ENDIF
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_LOCATE_WRITE(16,30,BASE_FILE_NAME,ALL_VERSIONS,0)
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         ENDIF
         SAVE_FILE_NAME = BASE_FILE_NAME
      ELSEIF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "transobj2:1113")
      ENDIF
      TEMP_CHR5 = PUT_REFERENCE_LOAD(SAVE_FILE_NAME,SAVE_FILE_DIR)
      RETURN
!
! OVERLAY THE DAY TYPE FILE

      ENTRY REFERENCE_LOAD_MAKEOVL(OVERLAY_FAMILY_NAME)

!
!
!
      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      FILE_NAME = trim(OUTPUT_DIRECTORY())// &
                              "LDE"//trim(OVERLAY_FAMILY_NAME)//'.BIN'
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      IF(FILE_EXISTS) THEN
         TEMP_CHR5 = PUT_REFERENCE_LOAD(OVERLAY_FAMILY_NAME, &
                                                     OUTPUT_DIRECTORY())
      ELSE
         FILE_NAME = trim(LDE_FILE_DIRECTORY())// &
                                   "LDE"//trim(BASE_FILE_NAME)//'.BIN'
         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
         IF(FILE_EXISTS) &
             TEMP_CHR5 = PUT_REFERENCE_LOAD(OVERLAY_FAMILY_NAME, &
                                                   LDE_FILE_DIRECTORY())
      ENDIF

      RETURN

      ENTRY RESET_REFERENCE_LOAD

         TEMP_CHR5 = PUT_REFERENCE_LOAD(SAVE_FILE_NAME,SAVE_FILE_DIR)
      RETURN
!
 1010 FORMAT('&',A)
!
      END
!
!
!

!
      FUNCTION BSYRLOAD() ! 1/16/98. GAT. FOR JONES AT TVA.
!

      CHARACTER(len=5) :: BSYRLOAD,SAVE_REFERENCE_LOAD, &
                  PUT_REFERENCE_LOAD,R_BASE_FILE_NAME
      CHARACTER(len=256) :: R_FULL_FILE_NAME,SAVE_FULL_FILE_NAME, &
                    REFERENCE_LOAD_FULL_NAME
      SAVE SAVE_REFERENCE_LOAD,SAVE_FULL_FILE_NAME
!
!
!
         BSYRLOAD = SAVE_REFERENCE_LOAD
      RETURN

      ENTRY PUT_REFERENCE_LOAD(R_BASE_FILE_NAME,R_FULL_FILE_NAME)

         SAVE_REFERENCE_LOAD = R_BASE_FILE_NAME
         SAVE_FULL_FILE_NAME = R_FULL_FILE_NAME
         PUT_REFERENCE_LOAD = SAVE_REFERENCE_LOAD
      RETURN

      ENTRY REFERENCE_LOAD_FULL_NAME()

         REFERENCE_LOAD_FULL_NAME = SAVE_FULL_FILE_NAME
      RETURN
      END
!

!
!     ROUTINE TO CALCULATE THE MARKET PRICE PROBABILITY
!                 CURVE ON AN ODD SPACED GRID
!                  FOR EACH TRANSACTION GROUP
!

!
!
! NEED TO BE CAREFUL ABOUT PASSING MARKET_PRICE INTO THIS DATABASE
!
      SUBROUTINE USER_MARKET_PRICE_PROB(R_TRANS_GROUP, &
                                    HOURS,HOURS_INCREMENT, &
                                    MARKET_PRICE,SUM_MARKET_PRICES, &
                                    MAX_MARKET_PRICE,MIN_MARKET_PRICE, &
                                    R_ISEAS,R_TRANS_GROUP_NAME)
      use end_routine, only: end_program, er_message
      use endpoint
      use globecom
      use grx_planning_routines
      use irec_endpoint_control
      use mod_base_year
      use prod_arrays_dimensions
      use production
      use sizecom
      use spindriftlib

      SAVE

      INTEGER(kind=2) :: PRICE_POINTS,MAX_POINTS
      PARAMETER (PRICE_POINTS=79)
      INTEGER(kind=2) :: I,HR,IMAX,HOURS,HOURS_INCREMENT,IPEAK,COUNT, &
                  INTERVALS,COUNTER,POINTS_IN_CURVE,R_ISEAS
      INTEGER(kind=2) :: IPNT,STORE_IPNT,HALF_POINT,START,END, &
                  R_HIGHEST_INTERVAL,MX_INTERVAL,R_TRANS_GROUP, &
                  TG, &
                  R_MAX_TRANS_GROUPS
      CHARACTER*(*) :: R_TRANS_GROUP_NAME
      REAL :: PEAK,BASE,OBS(:,:),DELTA_PROB,AREA, &
           MIN_LPROB,OBSERVATIONS
      REAL :: LPROB(:,:), &
           DX, &
           LODDUR(:,:,:), &
           CUM_HOURS(:,:,:), &
           CUM_REVENUE(:,:,:), &
           MARKET_PRICE(*), &
           AVE_REVENUE, &
           MAX_MARKET_PRICE, &
           MIN_MARKET_PRICE,LOAD_VAL,PEAK_DX,BASE_DX, &
           BASE_ADJUSTMENT,INTERVAL_HOURS, &
           R_CF,R_AVE_REVENUE,TOP_CAP_PERCENT, &
           TOP_AVE_REVENUE,BOTTOM_AVE_REVENUE,TOTAL_PERIOD_HOURS, &
           TOTAL_RESOURCE_HOURS, &
! FROM MR & !  FROM MRX
           R_PRICES(0:*),R_INTERVALS(0:*), &
           R_CUM_REVENUES(0:*), &
           MX_INCREMENT, &
           MRX_VOLATILITY_MULT, &
           GET_MRX_VOLATILITY_MULT, &
           MX_MIN_PRICE, &
           MX_MAX_PRICE, &
           MX_SLOPE, &
           MX_INTERCEPT, &
           PRICE_MULT

      REAL(kind=8) :: SUM_MARKET_PRICES,DEMAND, &
               ENERGY(:,:),ALPHA,PRECISN
!
      ALLOCATABLE :: OBS, &
                     LPROB, &
                     LODDUR, &
                     CUM_HOURS, &
                     CUM_REVENUE, &
                     ENERGY
!
      REAL(kind=4) :: TEMP_INTERVALS,TEMP_VALUE
      LOGICAL(kind=1) :: SET_POINTS,POINT_NOT_FOUND
      INTEGER(kind=2) :: SET_CURRENT_IPNT
!
      LOGICAL(kind=1) :: MARKET_DURATION_NOT_OPEN=.TRUE., &
                  YES_MARKET_DURATION,MARKET_DURATION_REPORT
      INTEGER(kind=2) :: USER_DURATION_NO,USER_DURATION_HEADER, &
                  CURRENT_MONTH, &
                  LAST_SEASON=0,PRODUCTION_PERIODS,CURRENT_YEAR
      CHARACTER(len=9) :: CL_MONTH_NAME(13)
      CHARACTER(len=20) :: MONTH_NAME,R_UNIT_NAME
!
      REAL(kind=4) :: BREAK,DX_1,DX_2, &
             SLOPE,EP_MAX,EP_AVE,PD_MAX,PD_AVE,INTERCEPT
      INTEGER(kind=2) :: INTERVALS_1,INTERVALS_2, &
                  PRICE_POINTS_1,PRICE_POINTS_2
      LOGICAL(kind=1) :: TWO_CURVES
      INTEGER :: USER_DURATION_REC

      YES_MARKET_DURATION = MARKET_DURATION_REPORT()
!
      IF(MARKET_DURATION_NOT_OPEN .AND. &
                        YES_MARKET_DURATION .AND. &
                                               .NOT. TESTING_PLAN ) THEN
         MARKET_DURATION_NOT_OPEN = .FALSE.
         USER_DURATION_NO = USER_DURATION_HEADER(USER_DURATION_REC)
         LAST_SEASON = get_PRODUCTION_PERIODS_in()
         DO CURRENT_MONTH = 1, LAST_SEASON
            CL_MONTH_NAME(CURRENT_MONTH) = MONTH_NAME(CURRENT_MONTH)
         ENDDO
         CL_MONTH_NAME(LAST_SEASON+1) = 'Annual'
      ENDIF
!
      TG = R_TRANS_GROUP
!
      CURRENT_YEAR = get_BASE_YEAR() + globecom_YEAR
!
!
      BASE_ADJUSTMENT = -999.
      INTERVALS = PRICE_POINTS/2 + 1
!
! 7/17/00. CREATING GREATER DENSITY FROM MIN_MARKET_PRICE TO USD100/MWH
!
      BREAK = 100.
      IF(MAX_MARKET_PRICE > BREAK .AND. MIN_MARKET_PRICE < BREAK) THEN
         INTERVALS_1 = 25
         INTERVALS_2 = 15
         PRICE_POINTS_1 = INTERVALS_1 * 2 - 1
         PRICE_POINTS_2 = INTERVALS_2 * 2
         TWO_CURVES = .TRUE.
         DX_1 = MAX(( &
             BREAK-MIN_MARKET_PRICE)/FLOAT(INTERVALS_1-1),0.01)
         DX_2 = MAX(( &
             MAX_MARKET_PRICE-BREAK)/FLOAT(INTERVALS_2),0.01)
      ELSE
         TWO_CURVES = .FALSE.
         DX = MAX(( &
             MAX_MARKET_PRICE-MIN_MARKET_PRICE)/FLOAT(INTERVALS-1),0.01)
      ENDIF
      POINTS_IN_CURVE = PRICE_POINTS
!
      PEAK = MAX_MARKET_PRICE
      BASE = MIN_MARKET_PRICE
!
! 12/16/04. TOOK OUT INITIALIZATION
!
      DO I = 1, PRICE_POINTS

         ENERGY(I,TG)= 0.D0
         OBS(I,TG) = 0.
         LPROB(I,TG) = 0.
      ENDDO
      LPROB(1,TG) = 1.
!
      LODDUR(1,TG,R_ISEAS) = BASE
      COUNT = 1
      IF(TWO_CURVES) THEN
         DOWHILE (LODDUR(COUNT,TG,R_ISEAS) < BREAK .AND. &
                                           COUNT + 2 <= PRICE_POINTS_1 )
            COUNT = COUNT + 2
            LODDUR(COUNT,TG,R_ISEAS) = LODDUR(COUNT-2,TG,R_ISEAS) + DX_1
         ENDDO
! NOTE PRICE_POINTS NOT PRICE_POINTS_2
         DOWHILE (LODDUR(COUNT,TG,R_ISEAS) < PEAK .AND. &
                                           COUNT + 2 <= PRICE_POINTS )
            COUNT = COUNT + 2
            LODDUR(COUNT,TG,R_ISEAS) = LODDUR(COUNT-2,TG,R_ISEAS) + DX_2
         ENDDO
      ELSE
         DOWHILE (LODDUR(COUNT,TG,R_ISEAS) < PEAK .AND. &
                                             COUNT + 2 <= PRICE_POINTS )
            COUNT = COUNT + 2
            LODDUR(COUNT,TG,R_ISEAS) = LODDUR(COUNT-2,TG,R_ISEAS) + DX
         ENDDO
      ENDIF
!
      IF(COUNT > PRICE_POINTS) THEN
         WRITE(4,*) "In the Market Duration Curve Report"
         WRITE(4,*) "the model detected an abnormal price series."
         WRITE(4,*) "Model reassigned the distribution of prices."
         COUNT = PRICE_POINTS
      ENDIF
!
      IF(LODDUR(COUNT,TG,R_ISEAS) < PEAK) THEN
         TEMP_VALUE = LODDUR(COUNT,TG,R_ISEAS) - PEAK
      ENDIF
      IF(COUNT < 3) THEN
         WRITE(4,*) "Market prices appear to be flat."
         WRITE(4,*) "Please check that Multi-area simulation"
         WRITE(4,*) "has at least two market areas."

         RETURN ! NOT THE BEST WAY TO HANDLE THIS.
      ENDIF
!
! 7/17/00.
!
      IF(TWO_CURVES) THEN
         IF(PEAK - LODDUR(COUNT-2,TG,R_ISEAS) < &
                                           .0001*DX_2) COUNT = COUNT - 2
         LODDUR(COUNT,TG,R_ISEAS) = PEAK
         POINTS_IN_CURVE = COUNT
         IMAX = COUNT
         INTERVALS = COUNT/2 + 1
         IPEAK = INTERVALS - 1
         DO I = COUNT + 1, PRICE_POINTS
            LODDUR(I,TG,R_ISEAS) = LODDUR(I-1,TG,R_ISEAS) + DX_2
         ENDDO
!
!     PLACES MARKET PRICES INTO PRICE_POINTS/2 - 1 INTERVALS
!
!      DO HR = HOURS,HOURS+HOURS_INCREMENT-1
         DO HR = 1, HOURS_INCREMENT
            IF(MARKET_PRICE(HR) .GE. BASE) THEN
               IF(MARKET_PRICE(HR) <= BREAK) THEN
                  I = AINT((MARKET_PRICE(HR) +.0001 - BASE)/DX_1) + 1
!                  IF(I.GT.IPEAK) I = IPEAK
               ELSE
                  I = AINT((MARKET_PRICE(HR) +.0001 - BREAK)/DX_2) + &
                                                            INTERVALS_1
!                  IF(I.GT.IPEAK) I = IPEAK
               ENDIF
               IF(MARKET_PRICE(HR) > 1000000000.00) THEN
                  WRITE(4,*) 'FOR TRANSACTION GROUP',R_TRANS_GROUP_NAME
                  WRITE(4,*) 'IN MONTH',R_ISEAS
                  WRITE(4,*) 'IN YEAR',CURRENT_YEAR
                  WRITE(4,*) 'THE MARKET PRICES EXCEED UPPER BOUNDS'
                  WRITE(4,*) 'TG ',TG
                  WRITE(4,*) 'I ',I
                  WRITE(4,*) 'HR ',HR
                  WRITE(4,*) 'MARKET PRICE',MARKET_PRICE(HR)
                  WRITE(4,*) 'DELTA PRICES',DX_2,DX_1
                  WRITE(4,*) 'MINIMUM PRICES',BASE,BREAK
                  er_message='Stop requested from TRANSOBJ2 SIID330'
                  call end_program(er_message)
               ENDIF
               IF(I > PRICE_POINTS .OR. I < 1 .OR. TG < 1) THEN
                  WRITE(4,*) 'FOR TRANSACTION GROUP',R_TRANS_GROUP_NAME
                  WRITE(4,*) 'IN MONTH',R_ISEAS
                  WRITE(4,*) 'IN YEAR',CURRENT_YEAR
                  WRITE(4,*) 'THE MARKET PRICE CURVES WERE FLAT'
                  WRITE(4,*) 'TG ',TG
                  WRITE(4,*) 'I ',I
                  WRITE(4,*) 'HR ',HR
                  WRITE(4,*) 'MARKET PRICE',MARKET_PRICE(HR)
                  WRITE(4,*) 'DELTA PRICES',DX_2,DX_1
                  WRITE(4,*) 'MINIMUM PRICES',BASE,BREAK
                  er_message='Stop requested from TRANSOBJ2 SIID331'
                  call end_program(er_message)
               ENDIF
               OBS(I,TG) = OBS(I,TG) + 1.
               ENERGY(I,TG) = ENERGY(I,TG) + MARKET_PRICE(HR)
            ENDIF
         ENDDO
      ELSE
         IF(PEAK - LODDUR(COUNT-2,TG,R_ISEAS) < &
                                             .0001*DX) COUNT = COUNT - 2
         LODDUR(COUNT,TG,R_ISEAS) = PEAK
         POINTS_IN_CURVE = COUNT
         IMAX = COUNT
         INTERVALS = COUNT/2 + 1
         IPEAK = INTERVALS - 1
         DO I = COUNT + 1, PRICE_POINTS
            LODDUR(I,TG,R_ISEAS) = LODDUR(I-1,TG,R_ISEAS) + DX
         ENDDO
!
!     PLACES MARKET PRICES INTO PRICE_POINTS/2 - 1 INTERVALS
!
!      DO HR = HOURS,HOURS+HOURS_INCREMENT-1
         DO HR = 1, HOURS_INCREMENT
            IF(MARKET_PRICE(HR) .GE. BASE) THEN
               I = AINT((MARKET_PRICE(HR) +.0001 - BASE)/DX) + 1
               IF(I.GT.IPEAK) I = IPEAK
               OBS(I,TG) = OBS(I,TG) + 1.
               ENERGY(I,TG) = ENERGY(I,TG) + MARKET_PRICE(HR)
            ENDIF
         ENDDO
      ENDIF
!
!
      MIN_LPROB = 1./FLOAT(HOURS_INCREMENT)
      OBSERVATIONS = 0.
      DO I = 2 , INTERVALS
         COUNT = 2*(I) - 2
         OBSERVATIONS = OBSERVATIONS + OBS(I-1,TG)
         DELTA_PROB = 1. - OBSERVATIONS/FLOAT(HOURS_INCREMENT)
!
!
!
         IF(OBS(I-1,TG) .LE. 0.) THEN
            LPROB(COUNT+1,TG) = LPROB(COUNT-1,TG)
            LPROB(COUNT,TG) = LPROB(COUNT-1,TG)
            LODDUR(COUNT,TG,R_ISEAS) = &
                      (LODDUR(COUNT+1,TG,R_ISEAS) + &
                                          LODDUR(COUNT-1,TG,R_ISEAS))/2.
         ELSE
            LODDUR(COUNT,TG,R_ISEAS) = ENERGY(I-1,TG)/OBS(I-1,TG)
            LPROB(COUNT+1,TG) = DELTA_PROB
            IF(LPROB(COUNT+1,TG) < MIN_LPROB) LPROB(COUNT+1,TG) = 0.
            AREA = (ENERGY(I-1,TG) - &
                                LODDUR(COUNT-1,TG,R_ISEAS)*OBS(I-1,TG))/ &
               FLOAT(HOURS_INCREMENT) + &
               LPROB(COUNT+1,TG)*(LODDUR(COUNT+1,TG,R_ISEAS) - &
                                             LODDUR(COUNT-1,TG,R_ISEAS))
            LPROB(COUNT,TG) = (2*AREA - &
               (LPROB(COUNT-1,TG)* &
                                 (LODDUR(COUNT,TG,R_ISEAS) - &
                                           LODDUR(COUNT-1,TG,R_ISEAS)) + &
               LPROB(COUNT+1,TG)* &
                             (LODDUR(COUNT+1,TG,R_ISEAS) - &
                                         LODDUR(COUNT,TG,R_ISEAS)))  ) / &
               (LODDUR(COUNT+1,TG,R_ISEAS)-LODDUR(COUNT-1,TG,R_ISEAS))
!
            IF( ENERGY(I-1,TG)/(OBS(I-1,TG) * &
                           LODDUR(COUNT+1,TG,R_ISEAS)) &
                                        > .999999 ) THEN
               LODDUR(COUNT,TG,R_ISEAS) = LODDUR(COUNT+1,TG,R_ISEAS)
               IF(LODDUR(COUNT+1,TG,R_ISEAS) < 10000) THEN
                  LODDUR(COUNT+1,TG,R_ISEAS) = &
                                       LODDUR(COUNT+1,TG,R_ISEAS) + .001
               ELSE
                  LODDUR(COUNT+1,TG,R_ISEAS) = &
                                        LODDUR(COUNT+1,TG,R_ISEAS) + .01
               ENDIF
            ENDIF
            IF(LPROB(COUNT,TG) .GT. LPROB(COUNT-1,TG)) THEN
               LPROB(COUNT,TG) = LPROB(COUNT-1,TG)
            ELSEIF(LPROB(COUNT,TG) .LT. LPROB(COUNT+1,TG) ) THEN
               LPROB(COUNT,TG) = LPROB(COUNT+1,TG)
            ENDIF
         ENDIF
      ENDDO
!
!     THIS SECTION TAKES THE ROUNDING ERROR FROM THE
!     PREVIOUS ALGORITHM (SINGLE PRECISION CALC.) AND
!     DISTRIBUTES IT EVENLY ACROSS IPNT-1 POINTS.
!
      COUNTER = 0.
   50 CALL INTEG8(DEMAND,LODDUR(1,TG,R_ISEAS),LPROB(1,TG),IMAX, &
                  HOURS_INCREMENT,LODDUR(1,TG,R_ISEAS))
      IF(BASE_ADJUSTMENT == -999.) THEN
         ALPHA = LODDUR(2,TG,R_ISEAS) * FLOAT(HOURS_INCREMENT)
         ALPHA = (SUM_MARKET_PRICES-ALPHA)/(DEMAND-ALPHA)
      ELSE
         ALPHA = 1.
      ENDIF
      DO I = 2,IMAX
         LPROB(I,TG) = LPROB(I,TG)*ALPHA
      ENDDO
      LPROB(IMAX,TG) = 0.
      CALL INTEG8(DEMAND,LODDUR(1,TG,R_ISEAS),LPROB(1,TG),IMAX, &
         HOURS_INCREMENT,LODDUR(1,TG,R_ISEAS))
      PRECISN = 1.
      IF(SUM_MARKET_PRICES .GT. DEMAND) THEN
         PRECISN = DEMAND/SUM_MARKET_PRICES
      ELSE
         PRECISN = SUM_MARKET_PRICES/DEMAND
      ENDIF
      COUNTER = COUNTER + 1
      IF(PRECISN .LT. .999999 .AND. COUNTER .LT. 3) GOTO 50
!
      I = POINTS_IN_CURVE
      DOWHILE (LPROB(I,TG) == 0.)
         I = I - 1
      ENDDO
      POINTS_IN_CURVE = I + 1
      DX = (LODDUR(POINTS_IN_CURVE,TG,R_ISEAS)-BASE)/ &
                                                FLOAT(POINTS_IN_CURVE-1)
      DX = MAX(DX,0.01)
!
! 051410. START CHANGES TO IMPROVE ACCURACY.
!
      DO I = PRICE_POINTS-1, 1, -1
         INTERVAL_HOURS = (LPROB(I,TG)-LPROB(I+1,TG))*HOURS_INCREMENT
         CUM_HOURS(I,TG,R_ISEAS) = CUM_HOURS(I+1,TG,R_ISEAS) + &
                                                          INTERVAL_HOURS
         CUM_REVENUE(I,TG,R_ISEAS) = CUM_REVENUE(I+1,TG,R_ISEAS) + &
                                   LODDUR(I,TG,R_ISEAS) * INTERVAL_HOURS
      ENDDO
! LINEAR TRANSFORM OF ENERGY PRODUCTS AVERAGE REVENUE TO PRICE DURATION
! PERHAPS HIGHEST PRICE AND ALL HOUR AVERAGE
      EP_MAX = MAX_MARKET_PRICE
      EP_AVE = SUM_MARKET_PRICES / FLOAT(HOURS_INCREMENT)
      I = PRICE_POINTS - 1
      IF(CUM_HOURS(I,TG,R_ISEAS) > 0.) THEN
         AVE_REVENUE = CUM_REVENUE(I,TG,R_ISEAS) / &
                                                 CUM_HOURS(I,TG,R_ISEAS)
      ELSE
         AVE_REVENUE = 0.
      ENDIF
!
      PD_MAX = AVE_REVENUE
      I = 1
      IF(CUM_HOURS(I,TG,R_ISEAS) > 0.) THEN
         AVE_REVENUE = CUM_REVENUE(I,TG,R_ISEAS) / &
                                                 CUM_HOURS(I,TG,R_ISEAS)
      ELSE
         AVE_REVENUE = 0.
      ENDIF
      PD_AVE = AVE_REVENUE
!
      IF(ABS(PD_MAX - PD_AVE) > 0.0001) THEN
         SLOPE = (EP_MAX - EP_AVE) / (PD_MAX - PD_AVE)
      ELSE
         SLOPE = 1.0
      ENDIF
      INTERCEPT =  EP_AVE - SLOPE * PD_AVE
!
      DO I = PRICE_POINTS-1, 1, -1
         INTERVAL_HOURS = (LPROB(I,TG)-LPROB(I+1,TG))*HOURS_INCREMENT

         IF(CUM_HOURS(I,TG,R_ISEAS) > 0.) THEN
            AVE_REVENUE = CUM_REVENUE(I,TG,R_ISEAS) / &
                                                 CUM_HOURS(I,TG,R_ISEAS)
         ELSE
            AVE_REVENUE = 0.
         ENDIF
! TRANSFORM...
         AVE_REVENUE = INTERCEPT + SLOPE * AVE_REVENUE
         CUM_REVENUE(I,TG,R_ISEAS) = AVE_REVENUE * &
                                                 CUM_HOURS(I,TG,R_ISEAS)
         IF(INTERVAL_HOURS > 0.0001) THEN
            LODDUR(I,TG,R_ISEAS) = (CUM_REVENUE(I,TG,R_ISEAS) - &
                                       CUM_REVENUE(I+1,TG,R_ISEAS))/ &
                                                          INTERVAL_HOURS
         ELSE
            LODDUR(I,TG,R_ISEAS) = LODDUR(I+1,TG,R_ISEAS)
         ENDIF


         IF(YES_MARKET_DURATION) THEN
            IF(CUM_HOURS(I,TG,R_ISEAS) > 0.) THEN
               AVE_REVENUE = CUM_REVENUE(I,TG,R_ISEAS) / &
                                                 CUM_HOURS(I,TG,R_ISEAS)
            ELSE
               AVE_REVENUE = 0.
            ENDIF
            WRITE(USER_DURATION_NO,REC=USER_DURATION_REC) &
               PRT_ENDPOINT(), &
               FLOAT(CURRENT_YEAR), &
               CL_MONTH_NAME(R_ISEAS), &
               R_TRANS_GROUP_NAME, &
               FLOAT(I), &
               CUM_HOURS(I,TG,R_ISEAS), &
               LODDUR(I,TG,R_ISEAS), &
               CUM_REVENUE(I,TG,R_ISEAS), &
               AVE_REVENUE
            USER_DURATION_REC = USER_DURATION_REC + 1
         ENDIF
!
      ENDDO
!
      RETURN

      ENTRY INIT_USER_MARKET_PRICE_PROB(R_MAX_TRANS_GROUPS)

         IF(R_MAX_TRANS_GROUPS < 1) RETURN
         IF(ALLOCATED(OBS)) &
               DEALLOCATE( &
                     OBS, &
                     LPROB, &
                     LODDUR, &
                     CUM_HOURS, &
                     CUM_REVENUE, &
                     ENERGY)
         ALLOCATE(OBS(PRICE_POINTS,R_MAX_TRANS_GROUPS)) ! DON'T NEED MONTHLY
         ALLOCATE(LPROB(PRICE_POINTS,R_MAX_TRANS_GROUPS)) ! DON'T NEED MONTHLY
         ALLOCATE(LODDUR(PRICE_POINTS,R_MAX_TRANS_GROUPS,13)) !  DONE.
         ALLOCATE(CUM_HOURS(PRICE_POINTS,R_MAX_TRANS_GROUPS,13)) ! DONE.
         ALLOCATE(CUM_REVENUE(PRICE_POINTS,R_MAX_TRANS_GROUPS,13)) ! NEED MONTHLY
         ALLOCATE(ENERGY(PRICE_POINTS,R_MAX_TRANS_GROUPS)) ! DON'T NEED MONTHLY
!
         CUM_HOURS = 0.
         CUM_REVENUE = 0.
         LODDUR = 0.
!
      RETURN
!

      ENTRY MX_USER_PRICE_CURVE( R_TRANS_GROUP, &
                                 R_HIGHEST_INTERVAL,R_INTERVALS, &
                                 R_CUM_REVENUES,R_PRICES, &
                                 R_ISEAS)

!
! ASSUMES THAT THERE IS A ZEROTH INTERVAL.
! R_POINTS DOES NOT INC THIS INTERVAL.
!
! DO I WANT PRICES OR REVENUES?
!
         IF(R_HIGHEST_INTERVAL < 1) THEN
            RETURN
         ELSEIF(R_HIGHEST_INTERVAL == PRICE_POINTS) THEN
            IF(R_TRANS_GROUP == 0) THEN
               MRX_VOLATILITY_MULT = 1.0
            ELSE
               MRX_VOLATILITY_MULT = &
                        GET_MRX_VOLATILITY_MULT(R_TRANS_GROUP)
            ENDIF
!
            MX_MAX_PRICE = LODDUR(PRICE_POINTS,R_TRANS_GROUP,R_ISEAS)
            MX_MIN_PRICE = LODDUR(1,R_TRANS_GROUP,R_ISEAS)
!
            IF(MX_MIN_PRICE == MX_MAX_PRICE) THEN
               MX_SLOPE = 1.0
               MX_INTERCEPT = 0.
            ELSE
               MX_SLOPE = &
                  ( MRX_VOLATILITY_MULT*MX_MAX_PRICE - MX_MIN_PRICE ) / &
                                           (MX_MAX_PRICE - MX_MIN_PRICE)
               MX_INTERCEPT = MX_MIN_PRICE * ( 1. - MX_SLOPE)
            ENDIF
!
!

            DO I = PRICE_POINTS, 1, -1
               R_INTERVALS(I) = CUM_HOURS(I,R_TRANS_GROUP,R_ISEAS)
!               R_PRICES(I) = LODDUR(I,R_TRANS_GROUP)
               R_PRICES(I) = &
                       (MX_INTERCEPT + MX_SLOPE * &
                                        LODDUR(I,R_TRANS_GROUP,R_ISEAS))
               IF(LODDUR(I,R_TRANS_GROUP,R_ISEAS) > 0.) THEN
                  PRICE_MULT = R_PRICES(I) / &
                                         LODDUR(I,R_TRANS_GROUP,R_ISEAS)
               ELSE
                  PRICE_MULT = 1.0
               ENDIF
!               R_CUM_REVENUES(I) = CUM_REVENUE(I,R_TRANS_GROUP)
               R_CUM_REVENUES(I) = &
                         PRICE_MULT*CUM_REVENUE(I,R_TRANS_GROUP,R_ISEAS)
            ENDDO
            RETURN
         ENDIF
!
         IF(R_TRANS_GROUP == 0) THEN
            MRX_VOLATILITY_MULT = 1.0
         ELSE
            MRX_VOLATILITY_MULT = &
                        GET_MRX_VOLATILITY_MULT(R_TRANS_GROUP)
         ENDIF
!
         MX_MAX_PRICE = LODDUR(PRICE_POINTS,R_TRANS_GROUP,R_ISEAS)
         MX_MIN_PRICE = LODDUR(PRICE_POINTS,R_TRANS_GROUP,R_ISEAS)
!
         R_INTERVALS(R_HIGHEST_INTERVAL) = &
                           CUM_HOURS(PRICE_POINTS,R_TRANS_GROUP,R_ISEAS)
         R_CUM_REVENUES(R_HIGHEST_INTERVAL) = &
                         CUM_REVENUE(PRICE_POINTS,R_TRANS_GROUP,R_ISEAS)
         R_PRICES(R_HIGHEST_INTERVAL) = &
                              LODDUR(PRICE_POINTS,R_TRANS_GROUP,R_ISEAS)
         R_INTERVALS(0) = CUM_HOURS(1,R_TRANS_GROUP,R_ISEAS)
         R_CUM_REVENUES(0) = CUM_REVENUE(1,R_TRANS_GROUP,R_ISEAS)
         R_PRICES(0) = LODDUR(1,R_TRANS_GROUP,R_ISEAS)
!
! FIND THE CUM_REVENUE/CUM_HOURS INTERVAL THAT CONTAINS MX_HOURS
!
         MX_INCREMENT = CUM_HOURS(1,R_TRANS_GROUP,R_ISEAS)/ &
                                                      R_HIGHEST_INTERVAL
         MX_INTERVAL = 1
!
         R_INTERVALS(MX_INTERVAL) = R_INTERVALS(MX_INTERVAL-1) - &
                                                            MX_INCREMENT
!
         IF(MX_MIN_PRICE == MX_MAX_PRICE) THEN
            MX_SLOPE = 1.0
            MX_INTERCEPT = 0.
         ELSE
            MX_SLOPE = &
              ( MRX_VOLATILITY_MULT*MX_MAX_PRICE - MX_MIN_PRICE ) / &
                                           (MX_MAX_PRICE - MX_MIN_PRICE)
            MX_INTERCEPT = MX_MIN_PRICE * ( 1. - MX_SLOPE)
         ENDIF
!
         DO I = 2, PRICE_POINTS
!
            IF(CUM_HOURS(I,R_TRANS_GROUP,R_ISEAS) &
                                      >= R_INTERVALS(MX_INTERVAL)) CYCLE
!
! FIND THE PRICING.
!
!
            DOWHILE (R_INTERVALS(MX_INTERVAL) > &
                                     CUM_HOURS(I,R_TRANS_GROUP,R_ISEAS))
!
               IF(R_INTERVALS(MX_INTERVAL) - &
                                 R_INTERVALS(MX_INTERVAL-1) /= 0.0) THEN
                  TOP_CAP_PERCENT = &
                     (R_INTERVALS(MX_INTERVAL) - &
                                 CUM_HOURS(I-1,R_TRANS_GROUP,R_ISEAS)) / &
                           (CUM_HOURS(I,R_TRANS_GROUP,R_ISEAS) - &
                                   CUM_HOURS(I-1,R_TRANS_GROUP,R_ISEAS))
               ELSE
                  TOP_CAP_PERCENT = 0.0
               ENDIF
!
!
!
               R_PRICES(MX_INTERVAL) = &
                        TOP_CAP_PERCENT * &
                     (MX_INTERCEPT + &
                             MX_SLOPE*LODDUR(I,R_TRANS_GROUP,R_ISEAS)) + &
                                 (1.-TOP_CAP_PERCENT) * &
                     (MX_INTERCEPT + &
                             MX_SLOPE*LODDUR(I-1,R_TRANS_GROUP,R_ISEAS))
               R_CUM_REVENUES(MX_INTERVAL) = &
                       TOP_CAP_PERCENT * &
                     (MX_INTERCEPT + &
                        MX_SLOPE*CUM_REVENUE(I,R_TRANS_GROUP,R_ISEAS)) + &
                                 (1.-TOP_CAP_PERCENT) * &
                     (MX_INTERCEPT + &
                        MX_SLOPE*CUM_REVENUE(I-1,R_TRANS_GROUP,R_ISEAS))
!
               MX_INTERVAL = MX_INTERVAL + 1
               R_INTERVALS(MX_INTERVAL) = R_INTERVALS(MX_INTERVAL-1) - &
                                                            MX_INCREMENT
!
            ENDDO ! MULTIPLE INTERVALS BETWEEN TWO PRICE POINTS
!

!
         ENDDO ! SEARCHING FOR PRICE_POINTS CROSSOVER
!
      RETURN
      END

!
!     ROUTINE TO CALCULATE THE MARKET PRICE PROBABILITY
!                 CURVE ON AN ODD SPACED GRID
!

!
      SUBROUTINE MARKET_PRICE_PROB(HOURS,HOURS_INCREMENT, &
                                    MARKET_PRICE,SUM_MARKET_PRICES, &
                                    MAX_MARKET_PRICE,MIN_MARKET_PRICE, &
                                    R_ISEAS)
!
      use mod_base_year
      use production
      use spindriftlib
      use prod_arrays_dimensions
      use irec_endpoint_control
      use globecom
      use endpoint
      use grx_planning_routines
      use rptreccontrol
      use sizecom
      SAVE

      INTEGER(kind=2) :: PRICE_POINTS,MAX_POINTS
      PARAMETER (PRICE_POINTS=79)
      INTEGER(kind=2) :: I,HR,IMAX,HOURS,HOURS_INCREMENT,IPEAK,COUNT, &
                  INTERVALS,COUNTER,POINTS_IN_CURVE,ISEAS,R_ISEAS
      INTEGER(kind=2) :: IPNT,STORE_IPNT,HALF_POINT,START,END, &
                  R_HIGHEST_INTERVAL,MX_INTERVAL
      REAL :: PEAK,BASE,OBS(PRICE_POINTS,13),DELTA_PROB,AREA, &
           MIN_LPROB,OBSERVATIONS
      REAL :: LPROB(PRICE_POINTS,13), &
           DX, &
           LODDUR(PRICE_POINTS,13), &
           CUM_HOURS(PRICE_POINTS,13), &
           CUM_REVENUE(PRICE_POINTS,13), &
           MARKET_PRICE(8800), &
           AVE_REVENUE, &
           MAX_MARKET_PRICE, &
           MIN_MARKET_PRICE,LOAD_VAL,PEAK_DX,BASE_DX, &
           BASE_ADJUSTMENT,INTERVAL_HOURS, &
           R_CF,R_AVE_REVENUE,TOP_CAP_PERCENT, &
           TOP_AVE_REVENUE,BOTTOM_AVE_REVENUE,TOTAL_PERIOD_HOURS, &
           TOTAL_RESOURCE_HOURS,R_PRICES(0:*),R_INTERVALS(0:*), &
           R_CUM_REVENUES(0:*), &
           MX_INCREMENT
      REAL(kind=8) :: SUM_MARKET_PRICES,DEMAND, &
               ENERGY(PRICE_POINTS,13),ALPHA,PRECISN
!
      REAL(kind=4) :: TEMP_INTERVALS,TEMP_VALUE
      LOGICAL(kind=1) :: SET_POINTS,POINT_NOT_FOUND
      INTEGER(kind=2) :: SET_CURRENT_IPNT
!
      LOGICAL(kind=1) :: MARKET_DURATION_NOT_OPEN=.TRUE., &
                  YES_MARKET_DURATION,MARKET_DURATION_REPORT
      INTEGER(kind=2) :: MARKET_DURATION_NO,MARKET_DURATION_HEADER, &
                  CURRENT_MONTH, &
                  LAST_SEASON=0,PRODUCTION_PERIODS,CURRENT_YEAR
      CHARACTER(len=9) :: CL_MONTH_NAME(13)
      CHARACTER(len=20) :: MONTH_NAME,R_UNIT_NAME
      INTEGER :: MARKET_DURATION_REC
!
! SAVE ARRAY 8/15/01 MSG
!
!      SAVE LODDUR,LPROB,CUM_HOURS,CUM_REVENUE
!
!      SAVE MARKET_DURATION_NO,MARKET_DURATION_REC
!
!     END OF DATA DECLARATIONS
!
!
!
      YES_MARKET_DURATION = MARKET_DURATION_REPORT()
!
      IF(MARKET_DURATION_NOT_OPEN .AND. &
                        YES_MARKET_DURATION .AND. &
                                               .NOT. TESTING_PLAN ) THEN
         MARKET_DURATION_NOT_OPEN = .FALSE.
         MARKET_DURATION_NO = MARKET_DURATION_HEADER( &
                                                    MARKET_DURATION_REC)
         LAST_SEASON = get_PRODUCTION_PERIODS_in()
         DO CURRENT_MONTH = 1, LAST_SEASON
            CL_MONTH_NAME(CURRENT_MONTH) = MONTH_NAME(CURRENT_MONTH)
         ENDDO
         CL_MONTH_NAME(LAST_SEASON+1) = 'Annual'
      ENDIF
!
      CURRENT_YEAR = get_BASE_YEAR() + globecom_YEAR
!
!
      BASE_ADJUSTMENT = -999.
      INTERVALS = PRICE_POINTS/2 + 1
      DX = MAX((MAX_MARKET_PRICE-MIN_MARKET_PRICE)/FLOAT(INTERVALS-1), &
                                                                   0.01)
      POINTS_IN_CURVE = PRICE_POINTS
!
      PEAK = MAX_MARKET_PRICE
      BASE = MIN_MARKET_PRICE
!
      DO I = 1, PRICE_POINTS
         CUM_HOURS(I,R_ISEAS) = 0.
         CUM_REVENUE(I,R_ISEAS) = 0.
         ENERGY(I,R_ISEAS)= 0.
         OBS(I,R_ISEAS) = 0.
         LPROB(I,R_ISEAS) = 0.
      ENDDO
      LPROB(1,R_ISEAS) = 1.
!
      LODDUR(1,R_ISEAS) = BASE
      COUNT = 1
      DOWHILE (LODDUR(COUNT,R_ISEAS) < PEAK .AND. &
                                             COUNT + 2 <= PRICE_POINTS )
         COUNT = COUNT + 2
         LODDUR(COUNT,R_ISEAS) = LODDUR(COUNT-2,R_ISEAS) + DX
      ENDDO
!
      IF(COUNT > PRICE_POINTS) THEN
         WRITE(4,*) "In the Market Duration Curve Report"
         WRITE(4,*) "the model detected an abnormal price series."
         WRITE(4,*) "Model reassigned the distribution of prices."
         COUNT = PRICE_POINTS
      ENDIF
!
      IF(LODDUR(COUNT,R_ISEAS) < PEAK) THEN
         TEMP_VALUE = LODDUR(COUNT,R_ISEAS) - PEAK
      ENDIF
      IF(COUNT < 3) THEN
         WRITE(4,*) "Market prices appear to be flat."
         WRITE(4,*) "Please check that Multi-area simulation"
         WRITE(4,*) "has at least two market areas."

         RETURN ! NOT THE BEST WAY TO HANDLE THIS.
      ENDIF
      IF(PEAK - LODDUR(COUNT-2,R_ISEAS) < .0001*DX) COUNT = COUNT - 2
      LODDUR(COUNT,R_ISEAS) = PEAK
      POINTS_IN_CURVE = COUNT
      IMAX = COUNT
      INTERVALS = COUNT/2 + 1
      IPEAK = INTERVALS - 1
      DO I = COUNT + 1, PRICE_POINTS
         LODDUR(I,R_ISEAS) = LODDUR(I-1,R_ISEAS) + DX
      ENDDO
!
!     PLACES MARKET PRICES INTO PRICE_POINTS/2 - 1 INTERVALS
!
      DO HR = HOURS,HOURS+HOURS_INCREMENT-1
         IF(MARKET_PRICE(HR) .GE. BASE) THEN
            I = AINT((MARKET_PRICE(HR) +.0001 - BASE)/DX) + 1
            IF(I.GT.IPEAK) I = IPEAK
            OBS(I,R_ISEAS) = OBS(I,R_ISEAS) + 1.
            ENERGY(I,R_ISEAS) = ENERGY(I,R_ISEAS) + MARKET_PRICE(HR)
         ENDIF
      ENDDO
!
!
      MIN_LPROB = 1./FLOAT(HOURS_INCREMENT)
      OBSERVATIONS = 0.
      DO I = 2 , INTERVALS
         COUNT = 2*(I) - 2
         OBSERVATIONS = OBSERVATIONS + OBS(I-1,R_ISEAS)
         DELTA_PROB = 1. - OBSERVATIONS/FLOAT(HOURS_INCREMENT)
!
!
!
         IF(OBS(I-1,R_ISEAS) .LE. 0.) THEN
            LPROB(COUNT+1,R_ISEAS) = LPROB(COUNT-1,R_ISEAS)
            LPROB(COUNT,R_ISEAS) = LPROB(COUNT-1,R_ISEAS)
            LODDUR(COUNT,R_ISEAS) = (LODDUR(COUNT+1,R_ISEAS)+ &
                                             LODDUR(COUNT-1,R_ISEAS))/2.
         ELSE
            LODDUR(COUNT,R_ISEAS) = ENERGY(I-1,R_ISEAS)/OBS(I-1,R_ISEAS)
            LPROB(COUNT+1,R_ISEAS) = DELTA_PROB
            IF(LPROB(COUNT+1,R_ISEAS) < MIN_LPROB) &
                                             LPROB(COUNT+1,R_ISEAS) = 0.
            AREA = (ENERGY(I-1,R_ISEAS)-LODDUR(COUNT-1,R_ISEAS)* &
                                                      OBS(I-1,R_ISEAS))/ &
               FLOAT(HOURS_INCREMENT) + &
               LPROB(COUNT+1,R_ISEAS)*(LODDUR(COUNT+1,R_ISEAS) - &
                                                LODDUR(COUNT-1,R_ISEAS))
            LPROB(COUNT,R_ISEAS) = (2*AREA - &
               (LPROB(COUNT-1,R_ISEAS)*(LODDUR(COUNT,R_ISEAS)- &
                                              LODDUR(COUNT-1,R_ISEAS)) + &
               LPROB(COUNT+1,R_ISEAS)*(LODDUR(COUNT+1,R_ISEAS)- &
                                            LODDUR(COUNT,R_ISEAS)))  ) / &
               (LODDUR(COUNT+1,R_ISEAS)-LODDUR(COUNT-1,R_ISEAS))
!
            IF( ENERGY(I-1,R_ISEAS)/(OBS(I-1,R_ISEAS) * &
                                                LODDUR(COUNT+1,R_ISEAS)) &
                                        > .999999 ) THEN
               LODDUR(COUNT,R_ISEAS) = LODDUR(COUNT+1,R_ISEAS)
               IF(LODDUR(COUNT+1,R_ISEAS) < 10000) THEN
                  LODDUR(COUNT+1,R_ISEAS) = &
                                          LODDUR(COUNT+1,R_ISEAS) + .001
               ELSE
                  LODDUR(COUNT+1,R_ISEAS) = &
                                           LODDUR(COUNT+1,R_ISEAS) + .01
               ENDIF
            ENDIF
            IF(LPROB(COUNT,R_ISEAS) .GT. LPROB(COUNT-1,R_ISEAS)) THEN
               LPROB(COUNT,R_ISEAS) = LPROB(COUNT-1,R_ISEAS)
            ELSEIF(LPROB(COUNT,R_ISEAS) .LT. &
                                           LPROB(COUNT+1,R_ISEAS) ) THEN
               LPROB(COUNT,R_ISEAS) = LPROB(COUNT+1,R_ISEAS)
            ENDIF
         ENDIF
      ENDDO
!
!     THIS SECTION TAKES THE ROUNDING ERROR FROM THE
!     PREVIOUS ALGORITHM (SINGLE PRECISION CALC.) AND
!     DISTRIBUTES IT EVENLY ACROSS IPNT-1 POINTS.
!
      COUNTER = 0.
   50 CALL INTEG8(DEMAND,LODDUR(1,R_ISEAS),LPROB(1,R_ISEAS),IMAX, &
                  HOURS_INCREMENT,LODDUR(1,R_ISEAS))
      IF(BASE_ADJUSTMENT == -999.) THEN
         ALPHA = LODDUR(2,R_ISEAS) * FLOAT(HOURS_INCREMENT)
         ALPHA = (SUM_MARKET_PRICES-ALPHA)/(DEMAND-ALPHA)
      ELSE
         ALPHA = 1.
      ENDIF
      DO I = 2,IMAX
         LPROB(I,R_ISEAS) = LPROB(I,R_ISEAS)*ALPHA
      ENDDO
      LPROB(IMAX,R_ISEAS) = 0.
      CALL INTEG8(DEMAND,LODDUR(1,R_ISEAS),LPROB(1,R_ISEAS),IMAX, &
         HOURS_INCREMENT,LODDUR(1,R_ISEAS))
      PRECISN = 1.
      IF(SUM_MARKET_PRICES .GT. DEMAND) THEN
         PRECISN = DEMAND/SUM_MARKET_PRICES
      ELSE
         PRECISN = SUM_MARKET_PRICES/DEMAND
      ENDIF
      COUNTER = COUNTER + 1
      IF(PRECISN .LT. .999999 .AND. COUNTER .LT. 3) GOTO 50
!
!     WRITE THE RESULTS TO BE READ BY PROCOST
!
      HOURS = HOURS + HOURS_INCREMENT
      I = POINTS_IN_CURVE
      DOWHILE (LPROB(I,R_ISEAS) == 0.)
         I = I - 1
      ENDDO
      POINTS_IN_CURVE = I + 1
      DX = (LODDUR(POINTS_IN_CURVE,R_ISEAS)-BASE)/ &
                                                FLOAT(POINTS_IN_CURVE-1)
      DX = MAX(DX,0.01)

! ASSUME LAST POINT HAS ZERO REVENUE
!
      IF(YES_MARKET_DURATION) THEN
         IF(CUM_HOURS(I,R_ISEAS) > 0.) THEN
            AVE_REVENUE = CUM_REVENUE(PRICE_POINTS,R_ISEAS) / &
                                         CUM_HOURS(PRICE_POINTS,R_ISEAS)
         ELSE
            AVE_REVENUE = 0.
         ENDIF
         WRITE(MARKET_DURATION_NO,REC=MARKET_DURATION_REC) &
               PRT_ENDPOINT(), &
               FLOAT(CURRENT_YEAR), &
               CL_MONTH_NAME(R_ISEAS), &
               FLOAT(PRICE_POINTS), &
               CUM_HOURS(PRICE_POINTS,R_ISEAS), &
               LODDUR(PRICE_POINTS,R_ISEAS), &
               CUM_REVENUE(PRICE_POINTS,R_ISEAS), &
               AVE_REVENUE
         MARKET_DURATION_REC = MARKET_DURATION_REC + 1
      ENDIF
      DO I = PRICE_POINTS-1, 1, -1
         INTERVAL_HOURS = &
                   (LPROB(I,R_ISEAS)-LPROB(I+1,R_ISEAS))*HOURS_INCREMENT
         CUM_HOURS(I,R_ISEAS) = CUM_HOURS(I+1,R_ISEAS) + INTERVAL_HOURS
         CUM_REVENUE(I,R_ISEAS) = CUM_REVENUE(I+1,R_ISEAS) + &
                                      LODDUR(I,R_ISEAS) * INTERVAL_HOURS
!
         IF(YES_MARKET_DURATION) THEN
            IF(CUM_HOURS(I,R_ISEAS) > 0.) THEN
               AVE_REVENUE = CUM_REVENUE(I,R_ISEAS) / &
                                                    CUM_HOURS(I,R_ISEAS)
            ELSE
               AVE_REVENUE = 0.
            ENDIF
            WRITE(MARKET_DURATION_NO,REC=MARKET_DURATION_REC) &
               PRT_ENDPOINT(), &
               FLOAT(CURRENT_YEAR), &
               CL_MONTH_NAME(R_ISEAS), &
               FLOAT(I), &
               CUM_HOURS(I,R_ISEAS), &
               LODDUR(I,R_ISEAS), &
               CUM_REVENUE(I,R_ISEAS), &
               AVE_REVENUE
            MARKET_DURATION_REC = MARKET_DURATION_REC + 1
         ENDIF
!
      ENDDO
!
      RETURN
!

      ENTRY MX_PRICE_CURVE(R_HIGHEST_INTERVAL,R_INTERVALS, &
                           R_CUM_REVENUES,R_PRICES,R_ISEAS)

!
! ASSUMES THAT THERE IS A ZEROTH INTERVAL.
! R_POINTS DOES NOT INCLUDE THIS INTERVAL.
!
! DO I WANT PRICES OR REVENUES?
!
         IF(R_HIGHEST_INTERVAL < 1) THEN
            RETURN
         ELSEIF(R_HIGHEST_INTERVAL == PRICE_POINTS) THEN

            DO I = PRICE_POINTS, 1, -1
               R_INTERVALS(I) = CUM_HOURS(I,R_ISEAS)
               R_PRICES(I) = LODDUR(I,R_ISEAS)
               R_CUM_REVENUES(I) = CUM_REVENUE(I,R_ISEAS)

            ENDDO
            RETURN
         ENDIF
!
         R_INTERVALS(R_HIGHEST_INTERVAL) = &
                                         CUM_HOURS(PRICE_POINTS,R_ISEAS)
         R_CUM_REVENUES(R_HIGHEST_INTERVAL) = &
                                       CUM_REVENUE(PRICE_POINTS,R_ISEAS)
         R_PRICES(R_HIGHEST_INTERVAL) = LODDUR(PRICE_POINTS,R_ISEAS)
         R_INTERVALS(0) = CUM_HOURS(1,R_ISEAS)
         R_CUM_REVENUES(0) = CUM_REVENUE(1,R_ISEAS)
         R_PRICES(0) = LODDUR(1,R_ISEAS)
!
! FIND THE CUM_REVENUE/CUM_HOURS INTERVAL THAT CONTAINS MX_HOURS
!
         MX_INCREMENT = CUM_HOURS(1,R_ISEAS) / R_HIGHEST_INTERVAL
         MX_INTERVAL = 1
!
         R_INTERVALS(MX_INTERVAL) = R_INTERVALS(MX_INTERVAL-1) - &
                                                            MX_INCREMENT
!
         DO I = 2, PRICE_POINTS
!
            IF(CUM_HOURS(I,R_ISEAS) >= R_INTERVALS(MX_INTERVAL)) CYCLE
!
! FIND THE PRICING.
!
!
            DOWHILE (R_INTERVALS(MX_INTERVAL) > CUM_HOURS(I,R_ISEAS))
!
               IF(R_INTERVALS(MX_INTERVAL) - &
                                 R_INTERVALS(MX_INTERVAL-1) /= 0.0) THEN
                  TOP_CAP_PERCENT = &
                     (R_INTERVALS(MX_INTERVAL) - &
                                               CUM_HOURS(I-1,R_ISEAS)) / &
                         (CUM_HOURS(I,R_ISEAS) - CUM_HOURS(I-1,R_ISEAS))
               ELSE
                  TOP_CAP_PERCENT = 0.0
               ENDIF
!
               R_PRICES(MX_INTERVAL) = &
                        TOP_CAP_PERCENT * LODDUR(I,R_ISEAS) + &
                              (1.-TOP_CAP_PERCENT) * LODDUR(I-1,R_ISEAS)
               R_CUM_REVENUES(MX_INTERVAL) = &
                        TOP_CAP_PERCENT * CUM_REVENUE(I,R_ISEAS) + &
                         (1.-TOP_CAP_PERCENT) * CUM_REVENUE(I-1,R_ISEAS)
!
               MX_INTERVAL = MX_INTERVAL + 1
               R_INTERVALS(MX_INTERVAL) = R_INTERVALS(MX_INTERVAL-1) - &
                                                            MX_INCREMENT
!
            ENDDO ! MULTIPLE INTERVALS BETWEEN TWO PRICE POINTS
!

!
         ENDDO ! SEARCHING FOR PRICE_POINTS CROSSOVER
!
      RETURN

      ENTRY CF_2_ANNUAL_REVENUE(R_CF,R_AVE_REVENUE,R_UNIT_NAME,R_ISEAS)

!
         MAX_POINTS = MAX(1,PRICE_POINTS - 1)
!
         TOTAL_PERIOD_HOURS = CUM_HOURS(1,R_ISEAS)
!
         IF(TOTAL_PERIOD_HOURS <= 0.0 .OR. &
                                       TOTAL_PERIOD_HOURS >= 8785.) THEN

            R_AVE_REVENUE = 0.0
!
            RETURN
!
         ENDIF
!
         IF(R_CF > 100.0 .OR. R_CF < 0.0) THEN
            WRITE(4,*) "Hydro resource ",R_UNIT_NAME
            WRITE(4,*) " has a capacity factor"
            WRITE(4,*) "of ",R_CF," which is outside of valid range"
            WRITE(4,*) "for asset revenue allocation purposes."
            IF(R_CF > 100.0) THEN
               WRITE(4,*) "Revenue assumes 100% Capacity Factor."
               TOTAL_RESOURCE_HOURS = TOTAL_PERIOD_HOURS
            ELSE
               WRITE(4,*) "Revenue assumes 0% Capacity Factor."
               TOTAL_RESOURCE_HOURS = 0.
            ENDIF
         ELSE
            TOTAL_RESOURCE_HOURS = R_CF * TOTAL_PERIOD_HOURS / 100.
         ENDIF
         IF(R_CF == 0.0) THEN
            R_AVE_REVENUE = CUM_REVENUE(MAX_POINTS,R_ISEAS) / &
                                           CUM_HOURS(MAX_POINTS,R_ISEAS)
         ELSEIF(R_CF == 100.0) THEN
            R_AVE_REVENUE = &
                           CUM_REVENUE(1,R_ISEAS) / CUM_HOURS(1,R_ISEAS)
         ELSE ! SEARCH FOR THE CF INTERVAL
!
            START = 1
            END = MAX_POINTS
            POINT_NOT_FOUND = .TRUE.
!
            DOWHILE (POINT_NOT_FOUND)
               HALF_POINT = START + (END-START)/2
               IF(HALF_POINT == START) THEN
                  POINT_NOT_FOUND = .FALSE.
                  EXIT
               ENDIF
               IF(TOTAL_RESOURCE_HOURS < &
                                     CUM_HOURS(HALF_POINT,R_ISEAS)) THEN
                  START = HALF_POINT
               ELSEIF(TOTAL_RESOURCE_HOURS > &
                                     CUM_HOURS(HALF_POINT,R_ISEAS)) THEN
                  END = HALF_POINT
               ELSE
                  POINT_NOT_FOUND = .FALSE.
                  EXIT
               ENDIF
            ENDDO
            IF(CUM_HOURS(HALF_POINT+1,R_ISEAS) - &
                              CUM_HOURS(HALF_POINT,R_ISEAS) /= 0.0) THEN
               TOP_CAP_PERCENT = &
                  (TOTAL_RESOURCE_HOURS - &
                                        CUM_HOURS(HALF_POINT,R_ISEAS)) / &
                  (CUM_HOURS(HALF_POINT+1,R_ISEAS) - &
                                          CUM_HOURS(HALF_POINT,R_ISEAS))
            ELSE
               TOP_CAP_PERCENT = 1.0
            ENDIF
            BOTTOM_AVE_REVENUE = CUM_REVENUE(HALF_POINT,R_ISEAS)/ &
                                           CUM_HOURS(HALF_POINT,R_ISEAS)
!            TOP_AVE_REVENUE = CUM_REVENUE(HALF_POINT+1,R_ISEAS)/
!     +                                   CUM_HOURS(HALF_POINT+1,R_ISEAS)
            IF(CUM_HOURS(HALF_POINT+1,R_ISEAS) <= 0.01) THEN
               WRITE(4,*) "Price curve has flat interval for pricing"
               WRITE(4,*) "Hydro units"
               TOP_AVE_REVENUE = BOTTOM_AVE_REVENUE
            ELSE
               TOP_AVE_REVENUE = CUM_REVENUE(HALF_POINT+1,R_ISEAS)/ &
                                         CUM_HOURS(HALF_POINT+1,R_ISEAS)
            ENDIF
            R_AVE_REVENUE = (1.-TOP_CAP_PERCENT) * BOTTOM_AVE_REVENUE + &
                                       TOP_CAP_PERCENT * TOP_AVE_REVENUE
         ENDIF
      RETURN
      END

!
!                  ROUTINE TO CONVERT TIE PATHS FILE
!
!                          COPYRIGHT (C) 1998
!                     M.S. GERBER & ASSOCIATES, INC.
!                          ALL RIGHTS RESERVED
!

!
      SUBROUTINE PATH_OBJECT
      use end_routine, only: end_program, er_message
      use filename_tracker
!
      use spindriftlib
      use prod_arrays_dimensions
      USE SIZECOM
      INTEGER(kind=2) :: DELETE,INUNIT,IREC,LRECL=146
      INTEGER :: IOS
!
      INTEGER(kind=2) :: UNIT_NUM=10,MAX_WHEELS, &
                  SELLER_TRANSACTION_GROUP, &
                  BUYER_TRANSACTION_GROUP, &
                  GRX_ID
      PARAMETER   (MAX_WHEELS = 10)
      REAL(kind=4) :: PATH_PERCENT,PATH_WHEEL_RATE,PATH_SPREAD, &
                  PATH_KV_RATING, &
                  PATH_INDUCTANCE, &
                  PATH_SPREAD_MULT, &
                  PATH_WHEEL_MULT, &
                  PATH_SPREAD_OFF_MULT, &
                  PATH_WHEEL_OFF_MULT, &
                  MARKET_PRICE_DELTA, &
                  PEAK_PRICE_DELTA_MULT, &
                  OFF_PEAK_PRICE_DELTA_MULT
      INTEGER(kind=2) :: WHEEL_PATH(MAX_WHEELS), &
                  SAVE_NUMBER_OF_PATHS=0,R_NUMBER_OF_PATHS,R_WHEELS, &
                  PATH_OWNER
      INTEGER(kind=2) :: TRANS_LINE_INDEX
      CHARACTER(len=5) :: BASE_FILE_NAME,OVERLAY_FAMILY_NAME, &
                  TRANS_PATHS_FILE
      CHARACTER(len=30) :: SELLER_NAME,BUYER_NAME
      CHARACTER(len=256) :: FILE_NAME
      CHARACTER(len=256) :: BASE_FILE_DIRECTORY
      CHARACTER(len=256) :: DATA_DRIVE,OUTPUT_DIRECTORY
      CHARACTER(len=1) :: PATH_ACTIVE
      CHARACTER(len=2) :: MARKET_PRICE_ID
      CHARACTER(len=3) :: SCENARIO_NUM
      LOGICAL (kind=4) :: FILE_EXISTS
      logical (kind=2) :: PATH_FILE_EXISTS=.FALSE.
      logical (kind=2) :: R_PATH_FILE_EXISTS
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER(len=1024) :: RECLN
! DECLARATION FOR TRANSACT PATH DETERMINANTS
      CHARACTER(len=16) :: FILE_TYPE='TRANSACT Paths  '
      CHARACTER(len=2) :: TRPATH_OL='BC'
      LOGICAL(kind=1) :: LAHEY_LF95
      CHARACTER(len=30) :: SCREEN_OUTPUT

! CONVERT THE PATH FILE
!
!
!

      ENTRY PATH_MAKEBIN

      BASE_FILE_NAME = TRANS_PATHS_FILE()
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME = get_ipb_filename(BASE_FILE_NAME)
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      PATH_FILE_EXISTS = FILE_EXISTS
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_LOCATE_WRITE(16,30,BASE_FILE_NAME,ALL_VERSIONS,0)
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         ENDIF
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=trim(DATA_DRIVE)// &
                            "BCTRPATH.BIN",ACCESS="DIRECT", &
                                            STATUS="UNKNOWN",RECL=LRECL)
         IREC = 1
!
!
!
         PATH_PERCENT = 100.
         PATH_WHEEL_RATE = 0.
         PATH_SPREAD = 0.
         PATH_OWNER = 0
         PATH_KV_RATING = 250
         PATH_INDUCTANCE = 20.
         PATH_WHEEL_MULT = 1.0
         PATH_SPREAD_MULT = 1.0
!
         PATH_WHEEL_OFF_MULT = 1.0
         PATH_SPREAD_OFF_MULT = 1.0
!
         MARKET_PRICE_ID = '00'
         SCENARIO_NUM = '001'
         MARKET_PRICE_DELTA = 0.
         PEAK_PRICE_DELTA_MULT = 1.0
         OFF_PEAK_PRICE_DELTA_MULT = 1.0
         TRANS_LINE_INDEX = 1
         GRX_ID = 0
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
            WHEEL_PATH = 0
!
            READ(RECLN,*,ERR=200) DELETE, &
                  SELLER_NAME, &
                  SELLER_TRANSACTION_GROUP, &
                     BUYER_NAME, &
                  BUYER_TRANSACTION_GROUP, &
                  PATH_ACTIVE, &
                  PATH_PERCENT, &
                  WHEEL_PATH, &
                  PATH_WHEEL_RATE, &
                  PATH_SPREAD, &
                  PATH_OWNER, &
                  PATH_KV_RATING, &
                  PATH_INDUCTANCE, &
                  PATH_WHEEL_MULT, &
                  PATH_SPREAD_MULT, &
                  PATH_SPREAD_OFF_MULT, &
                  PATH_WHEEL_OFF_MULT, &
                  MARKET_PRICE_ID, &
                  SCENARIO_NUM, &
                  MARKET_PRICE_DELTA, &
                  PEAK_PRICE_DELTA_MULT, &
                  OFF_PEAK_PRICE_DELTA_MULT, &
                  TRANS_LINE_INDEX, &
                  GRX_ID
            WRITE(11,REC=IREC) DELETE, &
                  SELLER_NAME, &
                  SELLER_TRANSACTION_GROUP, &
                     BUYER_NAME, &
                  BUYER_TRANSACTION_GROUP, &
                  PATH_ACTIVE, &
                  PATH_PERCENT, &
                  WHEEL_PATH, &
                  PATH_WHEEL_RATE, &
                  PATH_SPREAD, &
                  PATH_OWNER, &
                  PATH_KV_RATING, &
                  PATH_INDUCTANCE, &
                  PATH_WHEEL_MULT, &
                  PATH_SPREAD_MULT, &
                  PATH_SPREAD_OFF_MULT, &
                  PATH_WHEEL_OFF_MULT, &
                  MARKET_PRICE_ID, &
                  SCENARIO_NUM, &
                  MARKET_PRICE_DELTA, &
                  PEAK_PRICE_DELTA_MULT, &
                  OFF_PEAK_PRICE_DELTA_MULT, &
                  TRANS_LINE_INDEX, &
                  GRX_ID
            IREC = IREC + 1
         ENDDO
         CLOSE(10)
         CLOSE(11)
      ELSE IF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "transobj2:1112")
      ENDIF
!
      SAVE_NUMBER_OF_PATHS = MAX(0,IREC-1)
!
      RETURN



! OVERLAY THE PATH FILE

      ENTRY PATH_MAKEOVL(OVERLAY_FAMILY_NAME)

!
      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME = get_ipo_filename(data_drive, overlay_family_name)
      OPEN(10,FILE=FILE_NAME)
      READ(10,*) DELETE
      INUNIT = 12
      IF(TRPATH_OL == 'BC') THEN
         OPEN(11,FILE=trim(DATA_DRIVE)// &
                                         "BCTRPATH.BIN",ACCESS="DIRECT", &
                                                             RECL=LRECL)
         INUNIT = 11
      ENDIF
      OPEN(12,FILE=trim(DATA_DRIVE)//"OLTRPATH.BIN",ACCESS="DIRECT", &
                                            STATUS="UNKNOWN",RECL=LRECL)
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
         READ(INUNIT,REC=IREC,IOSTAT=IOS) DELETE, &
                  SELLER_NAME, &
                  SELLER_TRANSACTION_GROUP, &
                     BUYER_NAME, &
                  BUYER_TRANSACTION_GROUP, &
                  PATH_ACTIVE, &
                  PATH_PERCENT, &
                  WHEEL_PATH, &
                  PATH_WHEEL_RATE, &
                  PATH_SPREAD, &
                  PATH_OWNER, &
                  PATH_KV_RATING, &
                  PATH_INDUCTANCE, &
                  PATH_WHEEL_MULT, &
                  PATH_SPREAD_MULT, &
                  PATH_SPREAD_OFF_MULT, &
                  PATH_WHEEL_OFF_MULT, &
                  MARKET_PRICE_ID, &
                  SCENARIO_NUM, &
                  MARKET_PRICE_DELTA, &
                  PEAK_PRICE_DELTA_MULT, &
                  OFF_PEAK_PRICE_DELTA_MULT, &
                  TRANS_LINE_INDEX, &
                  GRX_ID
         IF(IOS /= 0) then
            EXIT
         end if

         RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
         READ(RECLN,*,ERR=300) DELETE, &
                  SELLER_NAME, &
                  SELLER_TRANSACTION_GROUP, &
                     BUYER_NAME, &
                  BUYER_TRANSACTION_GROUP, &
                  PATH_ACTIVE, &
                  PATH_PERCENT, &
                  WHEEL_PATH, &
                  PATH_WHEEL_RATE, &
                  PATH_SPREAD, &
                  PATH_OWNER, &
                  PATH_KV_RATING, &
                  PATH_INDUCTANCE, &
                  PATH_WHEEL_MULT, &
                  PATH_SPREAD_MULT, &
                  PATH_SPREAD_OFF_MULT, &
                  PATH_WHEEL_OFF_MULT, &
                  MARKET_PRICE_ID, &
                  SCENARIO_NUM, &
                  MARKET_PRICE_DELTA, &
                  PEAK_PRICE_DELTA_MULT, &
                  OFF_PEAK_PRICE_DELTA_MULT, &
                  TRANS_LINE_INDEX, &
                  GRX_ID
!         ENDIF
!
         READ(10,1000,IOSTAT=IOS) RECLN
         DOWHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE
!
            READ(10,1000,IOSTAT=IOS) RECLN
!
         ENDDO
         WRITE(12,REC=IREC) DELETE, &
                  SELLER_NAME, &
                  SELLER_TRANSACTION_GROUP, &
                     BUYER_NAME, &
                  BUYER_TRANSACTION_GROUP, &
                  PATH_ACTIVE, &
                  PATH_PERCENT, &
                  WHEEL_PATH, &
                  PATH_WHEEL_RATE, &
                  PATH_SPREAD, &
                  PATH_OWNER, &
                  PATH_KV_RATING, &
                  PATH_INDUCTANCE, &
                  PATH_WHEEL_MULT, &
                  PATH_SPREAD_MULT, &
                  PATH_SPREAD_OFF_MULT, &
                  PATH_WHEEL_OFF_MULT, &
                  MARKET_PRICE_ID, &
                  SCENARIO_NUM, &
                  MARKET_PRICE_DELTA, &
                  PEAK_PRICE_DELTA_MULT, &
                  OFF_PEAK_PRICE_DELTA_MULT, &
                  TRANS_LINE_INDEX, &
                  GRX_ID
!
      ENDDO
      CLOSE(10)
      CLOSE(12)
      IF(TRPATH_OL == 'BC') CLOSE(11)
      TRPATH_OL = 'OL'
      RETURN

  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from TRANSOBJ2 SIID331'
      call end_program(er_message)

  300 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      CALL MG_LOCATE_WRITE(21,0, &
               'Error reading the Transmission Paths record. Look for'// &
                                          ' a "," in a character name.', &
                           ALL_VERSIONS,1)
      er_message='stop requested from TRANSOBJ2 SIID332'
      call end_program(er_message)
!

      ENTRY RESET_TRPATH_OL

         TRPATH_OL = 'BC'
      RETURN
!

      ENTRY OPEN_TRPATH_FILE

         OPEN(UNIT_NUM,FILE=trim(OUTPUT_DIRECTORY())//TRPATH_OL// &
               "TRPATH.BIN",ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      RETURN
!

      ENTRY CLOSE_TRPATH_FILE

         CLOSE(UNIT_NUM)
      RETURN

      ENTRY DOES_TRPATH_FILE_EXIST(R_PATH_FILE_EXISTS)

         R_PATH_FILE_EXISTS = PATH_FILE_EXISTS
      RETURN

      ENTRY GET_MAX_WHEELS_AND_PATHS(R_WHEELS,R_NUMBER_OF_PATHS)

         R_WHEELS = MAX_WHEELS
         R_NUMBER_OF_PATHS = SAVE_NUMBER_OF_PATHS
      RETURN
!
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END

!
!
!     MAINTAINS LIST OF FROM/TO PATHS AS WELL AS MW LOADING ON PATHS
!     AND TIES
!
!
      RECURSIVE FUNCTION READ_TRANS_PATH_DATA()
      use end_routine, only: end_program, er_message
      use RptRecControl
      use spindriftlib
      use prod_arrays_dimensions
      USE IREC_ENDPOINT_CONTROL
      USE SIZECOM
      use miscmod
      SAVE
      LOGICAL(kind=1) :: SAVE_PATH_FILE_EXISTS=.FALSE., &
                  REDUCE_PATH_CAPACITY,HOUR_PATH_ACTIVE, &
                  THIS_IS_A_LONG_PATH, &
                  SET_HOUR_LONG_PATH_PARAMS, &
                  MULTI_AREA_PRICING_FOR_MONTH, &
                  MULTI_AREA_BUY_PRICE_FOR_MONTH, &
                  GET_SCENARIO_PRICE_NUM, &
                  TEMP_L, &
                  TRANSMISSION_OPTION_CHECK
      INTEGER(kind=4) :: VALUES_2_ZERO
      INTEGER(kind=2) :: DELETE,I,J,K,CURRENT_RECORD,TRANS_GROUP, &
                  TEMP_I2, &
                  R_NUM_OF_PRICING_GROUPS, &
                  GET_TRANS_PATH,TRANS_PATH_RECORDS,R_TRANS_GROUP, &
                  SAVE_TRANS_PATH_RECORDS=0,MAX_WHEELS=0,MAX_PATHS, &
                  MAX_IN_PATHS, &
                  GET_MAX_IN_PATHS, &
                  LOCAL_MAX_WHEELS, &
                  ACTIVE_PATH_NUMBER=0,SELLER_ID,BUYER_ID, &
                  LOCAL_PATH_NUMBER, &
                  BEG_FO_HR_TL, &
                  END_FO_HR_TL, &
                  CURRENT_SELLER_PATH, &
                  CURRENT_BUYER_PATH, &
                  R_LP, &
                  L_LP, &
                  L1,L2, &
                  SAVE_MAX_PATHS=0, &
                  NUM_TRANS_GROUPS=0,GET_NUMBER_OF_ACTIVE_GROUPS, &
                  MAX_TRANS_GROUP_NUMBER=0,GET_MAX_TRANS_GROUP_NUMBER, &
                  GET_TRANS_GROUP_INDEX,TRANS_ID,TEMP_ID,CURRENT_TIE, &
                  LAST_TIE,R_HOUR,CURRENT_PATH, &
                  R_MONTH,R_YEAR, &
                  MAX_PATHS_PER_TIE=100,MAX_PATHS_PER_PAIR, &
                  TEMP_PATH,TEMP_WHEEL,TEMP_TIES_PER_PAIR, &
                  NUMBER_OF_TG,TRANSACTION_GROUPS_W_PATHS, &
                  PATH_FIRST_WHEELS,PATH, &
                  R_TIME_OF_DAY, &
                  LONG_PATHS_PER_HOUR=0, &
                  MAX_HOURLY_LONG_PATHS=9, & !  12/23/02. GAT.
                  SELLER_TRACK_COUNTER, &
                  SELLER_TRACK_NUMBER, &
                  NUM_SELLER_MARKETS(75), &
                  SELLER_MARKET_INDEX(75,75), & !  THIS NEED TO BE ALLOCATABLE
                  NUM_BUYER_MARKETS(75), &
                  BUYER_MARKET_INDEX(75,75), & 
                  ! ^^  THIS NEED TO BE ALLOCATABLE
                  SELL_BEG_FO_HR_TL(:), &
                  SELL_END_FO_HR_TL(:), &
                  BUY_BEG_FO_HR_TL(:), &
                  BUY_END_FO_HR_TL(:), &
                  R_GRX_ID,MAX_GRX_ID=0, &
                  GET_TRANSMISSION_FOR_GRX_ID, &
                  GET_TG_FOR_TRANSMISSION_BUY, &
                  GET_TG_FOR_TRANSMISSION_SELL
      INTEGER(kind=2) :: R_SELLER_PATH,R_BUYER_PATH
      INTEGER(kind=2), INTENT(IN) :: R_SELLER,R_BUYER
      PARAMETER   (MAX_PATHS_PER_PAIR = 75) ! 091604. CHANGED FROM 12
      INTEGER(kind=2) :: NUMBER_OF_WHEELS(MAX_PATHS_PER_PAIR), &
                  GET_HOUR_PATHS_INDEX, &
                  GET_HOUR_PATH_FOR_LONG_PATH, &
                  GET_HOUR_WHEEL_PATH, &
                  R_PATH, &
                  R_DEPTH, &
                  RECURSIVE_COUNT, &
                  GET_MARKET_PRICE_YEAR
      REAL(kind=4) :: GET_SEASON_CONSTRAINT_LIMIT, &
                  R_CAPACITY,CURRENT_PATH_MW, &
                  TEMP_CAPACITY, &
                  HOUR_TIE_LIMIT, &
                  PATH_WHEELING_CHARGE,PATH_SPREAD_RATE, &
                  LOCAL_WHEELING_CHARGE,LOCAL_PRICE_DELTA, &
                  LOCAL_DELTA_MULT, &
                  PATH_PRICE_DELTA, &
                  GET_PRICING_GROUP_SELL_PRICE, &
                  GET_PRICING_GROUP_BUY_PRICE, &
                  R_PRICING_GROUP_QUANT, &
                  GET_SCENARIO_TRANSMISSION_AVAIL, &
                  SCENARIO_TRANSMISSION_AVAIL, &
                  LOCAL_WHEEL_MULT, &
                  LOCAL_SPREAD_MULT, &
                  ESCALATED_MONTHLY_VALUE, &
                  CONSTRAINT_MULT, &
                  GET_TRANS_LINE_MULT, &
                  GET_CONSTRAINT_MULT, &
                  TEMP_R, &
                  SELLER_TRACK_MW(1000), &
                  CHANGE_THIS_PATH_SEGMENT, &
                  MULTI_AREA_MONTH_PRICE(:,:), &
                  MULTI_AREA_BUY_MONTH_PRICE(:,:), &
                  TRANSMISSION_PLANNING_ADD

      REAL(kind=8) :: & !  REAL(kind=4 before 20030429
                  HOUR_PATH_MW(:,:), &
                  HOUR_PATH_LIMIT(:,:), &
                  SAVE_HOUR_PATH_LIMIT(:,:), &
                  GET_HOUR_PATH_MW
      LOGICAL(kind=1) :: READ_TRANS_PATH_DATA,TRANS_GROUP_ACTIVE_SWITCH, &
                  UPDATE_TRANS_PATH_DATA, &
                  INIT_HOUR_PATH_LIMIT,TRANS_PATH_DATA_READ=.TRUE., &
                  GET_TRANS_PATH_PARAMS, &
                  DETAILED_CONSTRAINTS_ACTIVE=.FALSE., &
                  ARE_DETAILED_CONSTRAINTS_ACTIVE, &
                  RUN_TRANSACT, &
                  YES_RUN_TRANSACT
      LOGICAL(kind=2) :: PATH_FILE_EXISTS
      CHARACTER(len=1) :: PATH_ACTIVE
      CHARACTER(len=30) :: BUYER_NAME,SELLER_NAME
!
      CHARACTER(len=2) :: MARKET_PRICE_ID(:)
      CHARACTER(len=3) :: SCENARIO_NUM(:),SCENARIO_REFERENCE_NUM
!
      LOGICAL(kind=1) :: SET_HOUR_LONG_PATH_WI_PATHS, &
                SET_HOUR_LONG_CAP_TWH

      INTEGER(kind=2) :: PATHS_LONG_PATHS=0,L_BUYER,L_SELLER
      REAL(kind=4) :: L_CAPACITY
!
! SIMULATION VARIABLES
!
      INTEGER(kind=2) :: &
                  SELLER_TRANSACTION_GROUP(:), &
                  LONG_PATH_PATH(:), &
                  ACTIVE_PATH_FOR_LONG_PATH(:), &
                  BELONGS_TO_A_LONG_PATH(:), &
                  BUYER_TRANSACTION_GROUP(:), &
                  WHEEL_PATH(:,:), &
                  PATHS_PER_PAIR(:,:), &
                  LONG_PATH_FOR_PAIR(:,:), &
                  HOUR_LONG_PATH_FOR_PAIR(:,:), &
                  HOUR_PATHS_PER_PAIR(:,:), &
                  HOUR_PATH_FOR_LONG_PATH(:), &
                  HOUR_WHEEL_PATH(:,:), &
                  PATHS_INDEX(:,:,:), &
                  HOUR_PATHS_INDEX(:,:,:), &
                  TIES_PER_PAIR(:,:), &
                  TIE_INDEX(:,:,:), &
                  TIE_WHEEL_INDEX(:,:,:), &
                  TRANS_GROUP_POSITION(:), &
                  PATH_OWNER(:), &
                  TG_USED_IN_PATH(:), &
                  REVERSE_PATH(:,:)
      REAL(kind=4) :: PATH_PERCENT(:), &
                  SEASON_PATH_LIMIT(:,:), &
                  HOUR_TIE_LOADING(:,:), &
                  DAILY_TIE_LOADING(:,:), &
                  DAILY_TIE_CONSTRAINT(:,:), &
                  PATH_WHEEL_RATE(:), &
                  PATH_SPREAD(:), &
                  PATH_KV_RATING(:), &
                  PATH_INDUCTANCE(:), &
                  PATH_WHEEL_MULT(:), &
                  PATH_SPREAD_MULT(:), &
                  PATH_SPREAD_OFF_MULT(:), &
                  PATH_WHEEL_OFF_MULT(:), &
                  MARKET_PRICE_DELTA(:), &
                  TRANS_LINE_CONSTRAINT(:), &
                  PEAK_PRICE_DELTA_MULT(:), &
                  OFF_PEAK_PRICE_DELTA_MULT(:), &
                  GET_SEASON_PATH_LIMIT, &
                  GET_TRANS_LINE_CONSTRAINT, &
                  GET_TRANS_DERATE, &
                  TRANS_LINE_FOR
      INTEGER(kind=2) :: TRANS_LINE_INDEX(:),GRX_ID(:), &
                         GRX_ID_INDEX(10000)
      CHARACTER(len=65) :: LONG_PATH_NAME(:)
      CHARACTER(len=20) :: PATH_NAME(:), &
                  R_PRICING_GROUP_NAME
      ALLOCATABLE :: &
                  SELLER_TRANSACTION_GROUP, &
                  LONG_PATH_PATH, &
                  ACTIVE_PATH_FOR_LONG_PATH, &
                  BELONGS_TO_A_LONG_PATH, &
                  BUYER_TRANSACTION_GROUP, &
                  PATH_PERCENT, &
                  WHEEL_PATH, &
                  PATHS_PER_PAIR, &
                  LONG_PATH_FOR_PAIR, &
                  HOUR_LONG_PATH_FOR_PAIR, &
                  HOUR_PATHS_PER_PAIR, &
                  HOUR_PATH_FOR_LONG_PATH, &
                  HOUR_WHEEL_PATH, &
                  PATHS_INDEX, &
                  HOUR_PATHS_INDEX, &
                  TIES_PER_PAIR, &
                  TIE_INDEX, &
                  TIE_WHEEL_INDEX, &
                  SEASON_PATH_LIMIT, &
                  HOUR_PATH_LIMIT, &
                  HOUR_PATH_MW, &
                  SAVE_HOUR_PATH_LIMIT, &
                  HOUR_TIE_LOADING, &
                  DAILY_TIE_LOADING, &
                  DAILY_TIE_CONSTRAINT, &
                  PATH_NAME, &
                  LONG_PATH_NAME, &
                  TRANS_GROUP_POSITION, &
                  PATH_WHEEL_RATE, &
                  PATH_SPREAD, &
                  PATH_OWNER, &
                  PATH_KV_RATING, &
                  PATH_INDUCTANCE, &
                  PATH_WHEEL_MULT, &
                  PATH_SPREAD_MULT, &
                  PATH_SPREAD_OFF_MULT, &
                  PATH_WHEEL_OFF_MULT, &
                  MARKET_PRICE_ID, &
                  SCENARIO_NUM, &
                  MARKET_PRICE_DELTA, &
                  TRANS_LINE_CONSTRAINT, &
                  PEAK_PRICE_DELTA_MULT, &
                  OFF_PEAK_PRICE_DELTA_MULT, &
                  TRANS_LINE_INDEX, &
                  GRX_ID, &
                  TG_USED_IN_PATH, &
                  REVERSE_PATH, &
                  MULTI_AREA_MONTH_PRICE, &
                  MULTI_AREA_BUY_MONTH_PRICE, &
                  SELL_BEG_FO_HR_TL, &
                  SELL_END_FO_HR_TL, &
                  BUY_BEG_FO_HR_TL, &
                  BUY_END_FO_HR_TL
!
      CHARACTER(len=2) :: LOAD_FILE_CHAR_EXT
      CHARACTER(len=256) :: FILE_NAME, &
                    PRB_FILE_DIRECTORY
      CHARACTER(len=8) :: EEICODE
      INTEGER(kind=2) :: TIMZON,TEMPER,DELTMP,DAY, &
                CURRENT_HR,DA,DAYS_IN_MONTH,LDE_YEAR, &
                MARKET_DAY_OF_WEEK,CALENDAR_DAY_OF_WEEK, &
                LDE_DAY,LDE_MONTH,DAY_WEEK,R_HOURS_IN_MONTH, &
                ALINE_LOAD_DATA,IREC,HR
      REAL(kind=4) :: LOCAL_MARKET_PRICE(24)
!
      LOGICAL(kind=4) :: FILE_EXISTS
      INTEGER(kind=2) :: iLine,iNode,jNode,PathID
      REAL(kind=4) :: Impedance,LineVoltageLevel,LinePowerLimit
!
! 010303.
!
         INTEGER(kind=2) :: R_END_POINT,R_DAY
         CHARACTER(len=9) :: R_MONTH_NAME
         LOGICAL(kind=1) :: HOURLY_FLOW_SUMMARY_REPORT, &
                     HOURLY_FLOW_REPORT_NOT_OPEN=.TRUE., &
                     DAILY_PATHS_REPORTS, &
                     YES_WRITE_DAILY_PATHS_REPORTS, &
                     HOURLY_PATHS_REPORTS, &
                     WRITE_DAILY_PATHS_REPORTS=.FALSE., &
                     R_STEP_4
         INTEGER(kind=2) :: HOURLY_FLOW_REPORT_VARIABLES, &
                     HOURLY_FLOW_ANALYST_NO=0, &
                     HOURLY_FLOW_CONSTRAINT_NO=0, &
                     HOURLY_FLOW_ANALYST_HEADER, &
                     HOURLY_FLOW_CONSTRAINT_HEADER
         INTEGER(kind=4) :: HOURLY_FLOW_ANALYST_REC, &
                     HOURLY_FLOW_CONSTRAINT_REC

         LOGICAL(kind=1) :: YES_REGIONAL_AREA_PRICE, &
                     YES_TWO_PRB_DIGITS
         CHARACTER(len=5) :: MARKET_PRICE_NAME
         CHARACTER(len=3) :: GET_HOURLY_PRICE_NAME


         READ_TRANS_PATH_DATA = .FALSE.
         SAVE_PATH_FILE_EXISTS = .FALSE.
!
!
         CALL DOES_TRPATH_FILE_EXIST(PATH_FILE_EXISTS)
!
         IF(.NOT. PATH_FILE_EXISTS) RETURN
!
!
         WRITE_DAILY_PATHS_REPORTS = YES_WRITE_DAILY_PATHS_REPORTS()
!
         CALL GET_MAX_WHEELS_AND_PATHS(MAX_WHEELS,MAX_IN_PATHS)
!
!         MAX_HOURLY_LONG_PATHS = 0
         MAX_PATHS = MAX_IN_PATHS + MAX_HOURLY_LONG_PATHS 
         SAVE_MAX_PATHS = MAX_PATHS
!
         CALL OPEN_TRPATH_FILE
!
         NUM_TRANS_GROUPS = GET_NUMBER_OF_ACTIVE_GROUPS()
         MAX_TRANS_GROUP_NUMBER = GET_MAX_TRANS_GROUP_NUMBER()
!
         NUMBER_OF_TG = 0
!
         IF(ALLOCATED(TRANS_GROUP_POSITION)) &
                                        DEALLOCATE(TRANS_GROUP_POSITION)
         ALLOCATE(TRANS_GROUP_POSITION(-1:MAX_TRANS_GROUP_NUMBER))
!
           TRANS_GROUP_POSITION = 0
           NUMBER_OF_WHEELS = 0
!
         DO J = 1, NUM_TRANS_GROUPS
             
             TRANS_ID = GET_TRANS_GROUP_INDEX(j)
             TRANS_GROUP_POSITION(TRANS_ID) = J
         ENDDO
!
         TRANS_GROUP_POSITION(-1) = -1
!
         IF(ALLOCATED(WHEEL_PATH)) DEALLOCATE( &
                                          SELLER_TRANSACTION_GROUP, &
                                          LONG_PATH_PATH, &
                                          ACTIVE_PATH_FOR_LONG_PATH, &
                                          BELONGS_TO_A_LONG_PATH, &
                                          BUYER_TRANSACTION_GROUP, &
                                          PATH_PERCENT, &
                                          WHEEL_PATH, &
                                          PATHS_PER_PAIR, &
                                          LONG_PATH_FOR_PAIR, &
                                          HOUR_LONG_PATH_FOR_PAIR, &
                                          HOUR_PATHS_PER_PAIR, &
                                          HOUR_PATH_FOR_LONG_PATH, &
                                          HOUR_WHEEL_PATH)
         IF(ALLOCATED(PATHS_INDEX)) DEALLOCATE( &
                                          PATHS_INDEX, &
                                          HOUR_PATHS_INDEX, &
                                          TIES_PER_PAIR, &
                                          TIE_INDEX, &
                                          TIE_WHEEL_INDEX, &
                                          SEASON_PATH_LIMIT, &
                                          HOUR_PATH_LIMIT, &
                                          HOUR_PATH_MW, &
                                          SAVE_HOUR_PATH_LIMIT, &
                                          HOUR_TIE_LOADING, &
                                          DAILY_TIE_LOADING, &
                                          DAILY_TIE_CONSTRAINT, &
                                          PATH_NAME, &
                                          LONG_PATH_NAME, &
                                          PATH_WHEEL_RATE, &
                                          PATH_SPREAD, &
                                          PATH_OWNER, &
                                          PATH_KV_RATING, &
                                          PATH_INDUCTANCE, &
                                          PATH_WHEEL_MULT, &
                                          PATH_SPREAD_MULT, &
                                          PATH_SPREAD_OFF_MULT, &
                                          PATH_WHEEL_OFF_MULT, &
                                          MARKET_PRICE_ID, &
                                          SCENARIO_NUM, &
                                          MARKET_PRICE_DELTA, &
                                          TRANS_LINE_CONSTRAINT, &
                                          PEAK_PRICE_DELTA_MULT, &
                                          OFF_PEAK_PRICE_DELTA_MULT, &
                                          TRANS_LINE_INDEX, &
                                          GRX_ID, &
                                          TG_USED_IN_PATH, &
                                          REVERSE_PATH)
         ALLOCATE(SELLER_TRANSACTION_GROUP(MAX_PATHS))
         ALLOCATE(LONG_PATH_PATH(MAX_PATHS))
         ALLOCATE(ACTIVE_PATH_FOR_LONG_PATH(MAX_PATHS))
         ALLOCATE(BELONGS_TO_A_LONG_PATH(MAX_PATHS))
         ALLOCATE(BUYER_TRANSACTION_GROUP(MAX_PATHS))
         ALLOCATE(PATH_PERCENT(MAX_PATHS))
         ALLOCATE(WHEEL_PATH(MAX_PATHS,MAX_WHEELS))
         ALLOCATE(PATHS_PER_PAIR(-1:NUM_TRANS_GROUPS, &
                                                   -1:NUM_TRANS_GROUPS))
         ALLOCATE(LONG_PATH_FOR_PAIR(-1:NUM_TRANS_GROUPS, &
                                                   -1:NUM_TRANS_GROUPS))
         ALLOCATE(HOUR_LONG_PATH_FOR_PAIR(-1:NUM_TRANS_GROUPS, &
                                                   -1:NUM_TRANS_GROUPS))
         ALLOCATE(HOUR_PATHS_PER_PAIR(-1:NUM_TRANS_GROUPS, &
                                                   -1:NUM_TRANS_GROUPS))
         ALLOCATE(HOUR_PATH_FOR_LONG_PATH(MAX_PATHS))
         ALLOCATE(HOUR_WHEEL_PATH(MAX(INT(1,2),MAX_HOURLY_LONG_PATHS), &
                                                     MAX_PATHS_PER_TIE))
         ALLOCATE(PATHS_INDEX(-1:NUM_TRANS_GROUPS,-1:NUM_TRANS_GROUPS, &
                                                    MAX_PATHS_PER_PAIR))
         ALLOCATE(HOUR_PATHS_INDEX( &
                                -1:NUM_TRANS_GROUPS,-1:NUM_TRANS_GROUPS, &
                                                    MAX_PATHS_PER_PAIR))
         ALLOCATE(TIES_PER_PAIR(-1:NUM_TRANS_GROUPS, &
                                                   -1:NUM_TRANS_GROUPS))
         ALLOCATE(TIE_INDEX(-1:NUM_TRANS_GROUPS,-1:NUM_TRANS_GROUPS, &
                                                     MAX_PATHS_PER_TIE))
         ALLOCATE(TIE_WHEEL_INDEX(-1:NUM_TRANS_GROUPS, &
                                             -1:NUM_TRANS_GROUPS, &
                                                     MAX_PATHS_PER_TIE))
         ALLOCATE(SEASON_PATH_LIMIT(-1:NUM_TRANS_GROUPS, &
                                                   -1:NUM_TRANS_GROUPS))
         ALLOCATE(HOUR_PATH_LIMIT(-1:NUM_TRANS_GROUPS, &
                                                   -1:NUM_TRANS_GROUPS))
         ALLOCATE(HOUR_PATH_MW(-1:NUM_TRANS_GROUPS, &
                                                   -1:NUM_TRANS_GROUPS))
         ALLOCATE(SAVE_HOUR_PATH_LIMIT(-1:NUM_TRANS_GROUPS, &
                                                   -1:NUM_TRANS_GROUPS))
         ALLOCATE(HOUR_TIE_LOADING(-1:NUM_TRANS_GROUPS, &
                                                   -1:NUM_TRANS_GROUPS))
         ALLOCATE(DAILY_TIE_LOADING(MAX_PATHS,24))
         ALLOCATE(DAILY_TIE_CONSTRAINT(MAX_PATHS,24))
         ALLOCATE(PATH_NAME(MAX_PATHS))
         ALLOCATE(LONG_PATH_NAME(MAX_PATHS))
         ALLOCATE(PATH_WHEEL_RATE(MAX_PATHS))
         ALLOCATE(PATH_SPREAD(MAX_PATHS))
         ALLOCATE(PATH_OWNER(MAX_PATHS))
         ALLOCATE(PATH_KV_RATING(MAX_PATHS))
         ALLOCATE(PATH_INDUCTANCE(MAX_PATHS))
         ALLOCATE(PATH_WHEEL_MULT(MAX_PATHS))
         ALLOCATE(PATH_SPREAD_MULT(MAX_PATHS))
         ALLOCATE(PATH_SPREAD_OFF_MULT(MAX_PATHS))
         ALLOCATE(PATH_WHEEL_OFF_MULT(MAX_PATHS))
         ALLOCATE(MARKET_PRICE_ID(MAX_PATHS))
         ALLOCATE(SCENARIO_NUM(MAX_PATHS))
         ALLOCATE(MARKET_PRICE_DELTA(MAX_PATHS))
         ALLOCATE(TRANS_LINE_CONSTRAINT(MAX_PATHS))
         ALLOCATE(PEAK_PRICE_DELTA_MULT(MAX_PATHS))
         ALLOCATE(OFF_PEAK_PRICE_DELTA_MULT(MAX_PATHS))
         ALLOCATE(TRANS_LINE_INDEX(MAX_PATHS))
         ALLOCATE(GRX_ID(MAX_PATHS))
         ALLOCATE(TG_USED_IN_PATH(-1:MAX_TRANS_GROUP_NUMBER))
         ALLOCATE(REVERSE_PATH(-1:NUM_TRANS_GROUPS, &
                                                   -1:NUM_TRANS_GROUPS))
! FOR LGE
           SEASON_PATH_LIMIT = 999999.
         GRX_ID_INDEX = 0
!
         PATH_WHEEL_RATE = 0.
         PATH_SPREAD = 0.
         PATH_WHEEL_MULT = 1.
         PATH_SPREAD_MULT = 1.
         PATH_SPREAD_OFF_MULT = 1.
         PATH_WHEEL_OFF_MULT = 1.
!
           PATHS_PER_PAIR = 0
         LONG_PATH_FOR_PAIR = 0
           TIES_PER_PAIR = 0
           REVERSE_PATH = 0
!
         ACTIVE_PATH_FOR_LONG_PATH = 0
         BELONGS_TO_A_LONG_PATH = 0
!
           TIE_INDEX = 0
           TIE_WHEEL_INDEX = 0
!
           TG_USED_IN_PATH = 0
!
           PATHS_INDEX = 0
           HOUR_PATHS_INDEX = 0
!
         NUM_SELLER_MARKETS = 0
         SELLER_MARKET_INDEX = 0
         NUM_BUYER_MARKETS = 0
         BUYER_MARKET_INDEX = 0
         MAX_GRX_ID = 0

         PATHS_LONG_PATHS = 0

         ACTIVE_PATH_NUMBER = 1

         DO CURRENT_RECORD = 1, MAX_IN_PATHS
            READ(10,REC=CURRENT_RECORD) DELETE, &
                  SELLER_NAME, &
                  SELLER_TRANSACTION_GROUP(ACTIVE_PATH_NUMBER), &
                     BUYER_NAME, &
                  BUYER_TRANSACTION_GROUP(ACTIVE_PATH_NUMBER), &
                  PATH_ACTIVE, &
                  PATH_PERCENT(ACTIVE_PATH_NUMBER), &
                  (WHEEL_PATH(ACTIVE_PATH_NUMBER,J),J=1,MAX_WHEELS), &
                  PATH_WHEEL_RATE(ACTIVE_PATH_NUMBER), &
                  PATH_SPREAD(ACTIVE_PATH_NUMBER), &
                  PATH_OWNER(ACTIVE_PATH_NUMBER), &
                  PATH_KV_RATING(ACTIVE_PATH_NUMBER), &
                  PATH_INDUCTANCE(ACTIVE_PATH_NUMBER), &
                  PATH_WHEEL_MULT(ACTIVE_PATH_NUMBER), &
                  PATH_SPREAD_MULT(ACTIVE_PATH_NUMBER), &
                  PATH_SPREAD_OFF_MULT(ACTIVE_PATH_NUMBER), &
                  PATH_WHEEL_OFF_MULT(ACTIVE_PATH_NUMBER), &
                  MARKET_PRICE_ID(ACTIVE_PATH_NUMBER), &
                  SCENARIO_NUM(ACTIVE_PATH_NUMBER), &
                  MARKET_PRICE_DELTA(ACTIVE_PATH_NUMBER), &
                  PEAK_PRICE_DELTA_MULT(ACTIVE_PATH_NUMBER), &
                  OFF_PEAK_PRICE_DELTA_MULT(ACTIVE_PATH_NUMBER), &
                  TRANS_LINE_INDEX(ACTIVE_PATH_NUMBER), &
                  GRX_ID(ACTIVE_PATH_NUMBER)

!
            SELLER_ID = SELLER_TRANSACTION_GROUP(ACTIVE_PATH_NUMBER)
            BUYER_ID = BUYER_TRANSACTION_GROUP(ACTIVE_PATH_NUMBER)
!
! 03/04/04. MODIFIED FOR MULTI-MARKET
!
! 12/09/17.
!
            IF(PATH_ACTIVE == 'E') THEN
               IF(GRX_ID(ACTIVE_PATH_NUMBER) <= 1) THEN
                  WRITE(4,*) "Bad GRX ID in Transmission Paths ", &
                                       GRX_ID(ACTIVE_PATH_NUMBER)
                  WRITE(4,*) "For a path designated as Expansion"
                  WRITE(4,*) "Seller ",SELLER_NAME
                  WRITE(4,*) "Buyer ",BUYER_NAME
                  WRITE(4,*) "Path Ignored"
                  CYCLE
               ELSE
                  PATH_ACTIVE = 'T'
                  GRX_ID_INDEX(GRX_ID(ACTIVE_PATH_NUMBER)) = &
                                                      ACTIVE_PATH_NUMBER
                  MAX_GRX_ID = &
                              MAX(MAX_GRX_ID,GRX_ID(ACTIVE_PATH_NUMBER))
               ENDIF
            ENDIF
!
            IF(PATH_ACTIVE == 'T' .OR. PATH_ACTIVE == 'F') THEN
               IF(PATH_ACTIVE == 'F' .OR. &
                  (.NOT. TRANS_GROUP_ACTIVE_SWITCH(SELLER_ID) &
                       .AND. SELLER_ID /= 0 .AND. SELLER_ID /= -1)  .OR. &
                  (.NOT. TRANS_GROUP_ACTIVE_SWITCH(BUYER_ID) &
                          .AND. BUYER_ID /= 0 .AND. BUYER_ID /= -1) .OR. &
                             (SELLER_ID == 0 .AND. BUYER_ID == 0)) CYCLE
            ELSE
               IF(PATH_ACTIVE == 'B') THEN
                  IF(SELLER_ID == 0 .OR. &
                      .NOT. TRANS_GROUP_ACTIVE_SWITCH(SELLER_ID) ) CYCLE
               ELSEIF(PATH_ACTIVE == 'S') THEN
                  IF(BUYER_ID == 0 .OR. &
                       .NOT. TRANS_GROUP_ACTIVE_SWITCH(BUYER_ID) ) CYCLE
               ELSE ! 'I'
                  IF(BUYER_ID == 0 .OR. SELLER_ID == 0) CYCLE
               ENDIF
            ENDIF

            PATH_PERCENT(ACTIVE_PATH_NUMBER) = &
                                 PATH_PERCENT(ACTIVE_PATH_NUMBER) / 100.

            IF(TG_USED_IN_PATH(SELLER_ID) == 0) THEN
               TG_USED_IN_PATH(SELLER_ID) = 1
               NUMBER_OF_TG = NUMBER_OF_TG + 1
            ENDIF
            IF(TG_USED_IN_PATH(BUYER_ID) == 0) THEN
               TG_USED_IN_PATH(BUYER_ID) = 1
               NUMBER_OF_TG = NUMBER_OF_TG + 1
            ENDIF
!
            PATH_NAME(ACTIVE_PATH_NUMBER) = SELLER_NAME(1:9)//'  '// &
                                                         BUYER_NAME(1:9)
            LONG_PATH_NAME(ACTIVE_PATH_NUMBER) = TRIM(SELLER_NAME)// &
                                                ' => '//TRIM(BUYER_NAME)

!
! COMPRESS THE ID'S
!
            SELLER_ID = TRANS_GROUP_POSITION(SELLER_ID)
            BUYER_ID = TRANS_GROUP_POSITION(BUYER_ID)
!


            IF( (PATH_ACTIVE == 'S' .OR. PATH_ACTIVE == 'I') &
                                                .AND. BUYER_ID > 0) THEN
               NUM_SELLER_MARKETS(BUYER_ID) = &
                                        NUM_SELLER_MARKETS(BUYER_ID) + 1
               SELLER_MARKET_INDEX( &
                                NUM_SELLER_MARKETS(BUYER_ID),BUYER_ID) = &
                                                      ACTIVE_PATH_NUMBER
            ENDIF

            IF( (PATH_ACTIVE == 'B' .OR. PATH_ACTIVE == 'I') &
                                               .AND. SELLER_ID > 0) THEN
               NUM_BUYER_MARKETS(SELLER_ID) = &
                                        NUM_BUYER_MARKETS(SELLER_ID) + 1
               BUYER_MARKET_INDEX( &
                               NUM_BUYER_MARKETS(SELLER_ID),SELLER_ID) = &
                                                      ACTIVE_PATH_NUMBER
            ENDIF

            THIS_IS_A_LONG_PATH = .FALSE.

            IF(WHEEL_PATH(ACTIVE_PATH_NUMBER,3) > 0) THEN
               IF(PATHS_LONG_PATHS < MAX_HOURLY_LONG_PATHS) THEN
                  PATHS_LONG_PATHS = PATHS_LONG_PATHS + 1
                  LONG_PATH_PATH(PATHS_LONG_PATHS) = ACTIVE_PATH_NUMBER
                  ACTIVE_PATH_FOR_LONG_PATH(ACTIVE_PATH_NUMBER) = &
                                                        PATHS_LONG_PATHS
                  LONG_PATH_FOR_PAIR(SELLER_ID,BUYER_ID) = &
                                                      ACTIVE_PATH_NUMBER
                  DO J = 2, MAX_WHEELS
                     IF(WHEEL_PATH(ACTIVE_PATH_NUMBER,J) == 0) EXIT
                     SELLER_ID = TRANS_GROUP_POSITION( &
                                     WHEEL_PATH(ACTIVE_PATH_NUMBER,J-1))
                     BUYER_ID = TRANS_GROUP_POSITION( &
                                       WHEEL_PATH(ACTIVE_PATH_NUMBER,J))
                     LONG_PATH_FOR_PAIR(SELLER_ID,BUYER_ID) = &
                                                     -ACTIVE_PATH_NUMBER
                  ENDDO
                  ! FOR THE NEXT PATH
                  ACTIVE_PATH_NUMBER = ACTIVE_PATH_NUMBER + 1 
               ENDIF
!
               CYCLE ! DON'T CALCULATE ALL THE OTHER STUFF.
            ENDIF
!
            IF(WHEEL_PATH(ACTIVE_PATH_NUMBER,1) /= &
                      SELLER_TRANSACTION_GROUP(ACTIVE_PATH_NUMBER)) THEN
               WRITE(4,*) "Originating Wheel Position"
               WRITE(4,*) "must be the Seller."
               WRITE(4,*) "Seller name = ",SELLER_NAME
               WRITE(4,*) "Buyer name =",BUYER_NAME
               WRITE(4,*) '*** line 13877 TRANSOBJ.FOR ***'
               er_message='See WARNING MESSAGES -TRANSOBJ2.FOR-3'
               call end_program(er_message)
            ENDIF
!
! IDENTIFY REVERSE PATH. THIS ASSUMES ALL PATHS ARE 100%.
!
            IF(REVERSE_PATH(SELLER_ID,BUYER_ID) == 0) THEN !
               REVERSE_PATH(SELLER_ID,BUYER_ID) = ACTIVE_PATH_NUMBER
            ELSE
               REVERSE_PATH(BUYER_ID,SELLER_ID) = &
                                        REVERSE_PATH(SELLER_ID,BUYER_ID)
               REVERSE_PATH(SELLER_ID,BUYER_ID) = ACTIVE_PATH_NUMBER
            ENDIF

            DO J = 1, MAX_WHEELS
               TEMP_ID  = WHEEL_PATH(ACTIVE_PATH_NUMBER,J)
               IF(TEMP_ID <= 0 .AND. .NOT. &
                       ((J==1 .AND. SELLER_ID==0) .OR. &
                        (J==2 .AND. BUYER_ID==0)  .OR. &
                        (J==1 .AND. SELLER_ID==-1) .OR. &
                        (J==2 .AND. BUYER_ID== -1) ) ) THEN
                  NUMBER_OF_WHEELS(J) = NUMBER_OF_WHEELS(J) + 1
                  EXIT ! NO MORE WHEELS. EXIT CONDITION.
               ELSEIF(TEMP_ID < = MAX_TRANS_GROUP_NUMBER) THEN
!
!                 REDEFINE WHEEL_PATH TO BE THE TRANSACTION GROUP POSITION
!
                  CURRENT_TIE = TRANS_GROUP_POSITION(TEMP_ID)
                  WHEEL_PATH(ACTIVE_PATH_NUMBER,J) = CURRENT_TIE
!
               ELSE
                  WRITE(4,*) "Wheel Position ", J
                  WRITE(4,*) "Seller name = ",SELLER_NAME
                  WRITE(4,*) "Buyer name = ",BUYER_NAME
                  WRITE(4,*) "is outside defined Transaction Groups"
                  WRITE(4,*) "and will be ignored."
                  CYCLE
               ENDIF
!
               IF( J == 1) THEN
                  LAST_TIE = SELLER_ID
               ELSE

                  TIES_PER_PAIR(CURRENT_TIE,LAST_TIE) = &
                               TIES_PER_PAIR(CURRENT_TIE,LAST_TIE) + 1
                  TIE_INDEX(CURRENT_TIE,LAST_TIE, &
                           TIES_PER_PAIR(CURRENT_TIE,LAST_TIE)) = &
                                                      ACTIVE_PATH_NUMBER
                  TIE_WHEEL_INDEX(CURRENT_TIE,LAST_TIE, &
                                TIES_PER_PAIR(CURRENT_TIE,LAST_TIE)) = J
               ENDIF
!
               LAST_TIE = CURRENT_TIE
            ENDDO
!
            PATHS_PER_PAIR(SELLER_ID,BUYER_ID) = &
                                  PATHS_PER_PAIR(SELLER_ID,BUYER_ID) + 1
            PATHS_INDEX(SELLER_ID,BUYER_ID, &
                           PATHS_PER_PAIR(SELLER_ID,BUYER_ID)) = &
                                                      ACTIVE_PATH_NUMBER
            ! FOR THE NEXT PATH
            ACTIVE_PATH_NUMBER = ACTIVE_PATH_NUMBER + 1
!
         ENDDO
         ACTIVE_PATH_NUMBER = ACTIVE_PATH_NUMBER - 1
!
         CALL CLOSE_TRPATH_FILE
!
         DETAILED_CONSTRAINTS_ACTIVE = ARE_DETAILED_CONSTRAINTS_ACTIVE()
!
         SAVE_PATH_FILE_EXISTS = .TRUE.
         READ_TRANS_PATH_DATA = .TRUE.
!
      RETURN

! SINGLE DERIVATIVE ADD.
!
      ENTRY TRANSMISSION_PLANNING_ADD(R_GRX_ID)
!     +                                R_START_YEAR,
!     +                                   R_EP_YEARS)
!
!

!
        TRANSMISSION_PLANNING_ADD = 0.0
        RUN_TRANSACT = YES_RUN_TRANSACT()
        IF(.NOT. PATH_FILE_EXISTS .OR. .NOT. RUN_TRANSACT) RETURN
!
! 010818.
!
!  ADD TO EXISTING EXPANSION CAPACITY. NEED A RESET ARRAY.
!  RESET AS NEEDED FOR MULTIPLE ITERATIONS
!  REVERSE FLOW
!  WHAT ABOUT: FT, PM, EXPENSE OR REVENUE
!
      RETURN

      ENTRY GET_TG_FOR_TRANSMISSION_SELL(R_GRX_ID)

         IF(R_GRX_ID > 0 .AND. R_GRX_ID <= MAX_GRX_ID) THEN
            GET_TG_FOR_TRANSMISSION_SELL = &
                                      SELLER_TRANSACTION_GROUP(R_GRX_ID)
         ELSE
            GET_TG_FOR_TRANSMISSION_SELL = -9999
         ENDIF
      RETURN

      ENTRY GET_TG_FOR_TRANSMISSION_BUY(R_GRX_ID)

         IF(R_GRX_ID > 0 .AND. R_GRX_ID <= MAX_GRX_ID) THEN
            GET_TG_FOR_TRANSMISSION_BUY = &
                                       BUYER_TRANSACTION_GROUP(R_GRX_ID)
         ELSE
            GET_TG_FOR_TRANSMISSION_BUY = -9999
         ENDIF
      RETURN

      ENTRY GET_TRANSMISSION_FOR_GRX_ID(R_GRX_ID)

         GET_TRANSMISSION_FOR_GRX_ID = GRX_ID_INDEX(R_GRX_ID)
      RETURN

      ENTRY TRANSMISSION_OPTION_CHECK(R_GRX_ID)

         TRANSMISSION_OPTION_CHECK = .FALSE.
         IF(R_GRX_ID > 0 .AND. R_GRX_ID <= 10000) THEN
            TEMP_I2 = GRX_ID_INDEX(R_GRX_ID)
            IF(TEMP_I2 > 0) THEN
               TRANSMISSION_OPTION_CHECK = .TRUE.
            ENDIF
         ENDIF
      RETURN

      ENTRY MULTI_AREA_PRICING_FOR_MONTH(R_HOURS_IN_MONTH, &
                                         R_MONTH,R_YEAR,R_BUYER, &
                                         R_NUM_OF_PRICING_GROUPS)

         MULTI_AREA_PRICING_FOR_MONTH = .TRUE.
         TEMP_I2 = NUM_SELLER_MARKETS(R_BUYER)
!
         R_NUM_OF_PRICING_GROUPS = TEMP_I2
!
         IF(TEMP_I2 == 0) RETURN
!
         DAYS_IN_MONTH = R_HOURS_IN_MONTH/24
!
         IF(ALLOCATED(MULTI_AREA_MONTH_PRICE)) &
                                      DEALLOCATE(MULTI_AREA_MONTH_PRICE, &
                                                 SELL_BEG_FO_HR_TL, &
                                                 SELL_END_FO_HR_TL)
!     +                                           BUY_BEG_FO_HR_TL,
!     +                                           BUY_END_FO_HR_TL)
         ALLOCATE(MULTI_AREA_MONTH_PRICE(R_HOURS_IN_MONTH,TEMP_I2))
         ALLOCATE(SELL_BEG_FO_HR_TL(ACTIVE_PATH_NUMBER))
         ALLOCATE(SELL_END_FO_HR_TL(ACTIVE_PATH_NUMBER))
!     +                            BUY_BEG_FO_HR_TL(ACTIVE_PATH_NUMBER),
!     +                            BUY_END_FO_HR_TL(ACTIVE_PATH_NUMBER))
         MULTI_AREA_MONTH_PRICE = 0. ! 12/22/03.
!
         SELL_BEG_FO_HR_TL = 0
         SELL_END_FO_HR_TL = 0
!         BUY_BEG_FO_HR_TL = 0
!         BUY_END_FO_HR_TL = 0
!
         TEMP_L = GET_SCENARIO_PRICE_NUM(R_YEAR, &
                                                SCENARIO_REFERENCE_NUM)
!
         DO I = 1, TEMP_I2
!
            LOCAL_PATH_NUMBER = SELLER_MARKET_INDEX(I,R_BUYER)
            TRANS_LINE_CONSTRAINT(LOCAL_PATH_NUMBER) = &
                  GET_TRANS_LINE_CONSTRAINT( &
                       TRANS_LINE_INDEX(LOCAL_PATH_NUMBER), &
                                    TRANS_LINE_FOR,R_MONTH,R_YEAR)
            IF(TRANS_LINE_FOR > .001) THEN
               CALL GET_FO_HRS_TLINE(LOCAL_PATH_NUMBER, &
                                     R_YEAR, & !    R_YEAR_OR_SCEN,
                                     R_MONTH, &
                                     DAYS_IN_MONTH, &
                                     TRANS_LINE_FOR, &
                                   SELL_BEG_FO_HR_TL(LOCAL_PATH_NUMBER), &
                                   SELL_END_FO_HR_TL(LOCAL_PATH_NUMBER))
            ENDIF
!
            IF(TEMP_L) THEN
               IF(YES_REGIONAL_AREA_PRICE()) THEN
!                  CALL GET_NEW_MARKET_PRICE_NAME(MARKET_PRICE_NAME)
                  MARKET_PRICE_NAME = GET_HOURLY_PRICE_NAME(R_BUYER)
                  IF(YES_TWO_PRB_DIGITS()) THEN
                    MARKET_PRICE_NAME = trim(MARKET_PRICE_NAME(1:3))// &
                           trim(SCENARIO_REFERENCE_NUM(1:2))
                  ELSE
                    MARKET_PRICE_NAME = trim(MARKET_PRICE_NAME(1:2))// &
                           trim(SCENARIO_REFERENCE_NUM(1:3))
                  ENDIF
               ELSE
                  MARKET_PRICE_NAME = &
                     trim(MARKET_PRICE_ID(LOCAL_PATH_NUMBER))// &
                                          trim(SCENARIO_REFERENCE_NUM)
               ENDIF
               FILE_NAME = trim(PRB_FILE_DIRECTORY())//"PRB"// &
                     trim(MARKET_PRICE_NAME)// &
                     ".P"//LOAD_FILE_CHAR_EXT( &
                                          GET_MARKET_PRICE_YEAR(R_YEAR))
               INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
               IF(.NOT. FILE_EXISTS) THEN
                  FILE_NAME = trim(PRB_FILE_DIRECTORY())//"PRB"// &
                     trim(MARKET_PRICE_NAME)// &
                      ".P"//LOAD_FILE_CHAR_EXT( &
                                          GET_MARKET_PRICE_YEAR(R_YEAR))
                  INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
               ENDIF
            ELSE
               IF(YES_REGIONAL_AREA_PRICE()) THEN
!                  CALL GET_NEW_MARKET_PRICE_NAME(MARKET_PRICE_NAME)
                  MARKET_PRICE_NAME = GET_HOURLY_PRICE_NAME(R_BUYER)
                  IF(YES_TWO_PRB_DIGITS()) THEN
                    MARKET_PRICE_NAME = trim(MARKET_PRICE_NAME(1:3))// &
                           trim(SCENARIO_NUM(LOCAL_PATH_NUMBER)(1:2))
                  ELSE
                    MARKET_PRICE_NAME = trim(MARKET_PRICE_NAME(1:2))// &
                           trim(SCENARIO_NUM(LOCAL_PATH_NUMBER)(1:3))
                  ENDIF
               ELSE
                  MARKET_PRICE_NAME = &
                     trim(MARKET_PRICE_ID(LOCAL_PATH_NUMBER))// &
                                 trim(SCENARIO_NUM(LOCAL_PATH_NUMBER))
               ENDIF
               FILE_NAME = trim(PRB_FILE_DIRECTORY())//"PRB"// &
                     trim(MARKET_PRICE_NAME)// &
                     ".P"//LOAD_FILE_CHAR_EXT( &
                                          GET_MARKET_PRICE_YEAR(R_YEAR))
               INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
            ENDIF
            IF(.NOT. FILE_EXISTS) CYCLE
!
! READ THE PRB
!
            OPEN(UNIT=2801,FILE=FILE_NAME,RECL=118, &
                                           ACCESS="DIRECT",STATUS="OLD")
            J = 1
            IREC = ALINE_LOAD_DATA(J,R_MONTH)
!            DAYS_IN_MONTH = R_HOURS_IN_MONTH/24
!
            CURRENT_HR = 0
            DO DA = 1, DAYS_IN_MONTH
               READ(2801,REC=IREC) LDE_MONTH,LDE_DAY,LDE_YEAR, &
                                     EEICODE,DAY_WEEK, &
                                     TIMZON,TEMPER, &
                                     DELTMP, &
                                     (LOCAL_MARKET_PRICE(HR),HR=1,24)
               IREC = IREC + 1
               DO HR = 1, 24
                  CURRENT_HR = CURRENT_HR + 1
                  MULTI_AREA_MONTH_PRICE(CURRENT_HR,I) = &
                                                  LOCAL_MARKET_PRICE(HR)
               ENDDO
            ENDDO
            CLOSE(2801)
         END DO

      RETURN

      ENTRY MULTI_AREA_BUY_PRICE_FOR_MONTH( &
                                         R_HOURS_IN_MONTH, &
                                         R_MONTH,R_YEAR,R_BUYER, &
                                         R_NUM_OF_PRICING_GROUPS)

         MULTI_AREA_BUY_PRICE_FOR_MONTH = .TRUE.
         TEMP_I2 = NUM_BUYER_MARKETS(R_BUYER)
!
         R_NUM_OF_PRICING_GROUPS = TEMP_I2
!
         IF(TEMP_I2 == 0) RETURN
!
         DAYS_IN_MONTH = R_HOURS_IN_MONTH/24
!
         IF(ALLOCATED(MULTI_AREA_BUY_MONTH_PRICE)) &
                                      DEALLOCATE( &
                                            MULTI_AREA_BUY_MONTH_PRICE, &
                                            BUY_BEG_FO_HR_TL, &
                                            BUY_END_FO_HR_TL)
         ALLOCATE(MULTI_AREA_BUY_MONTH_PRICE(R_HOURS_IN_MONTH,TEMP_I2))
         ALLOCATE(BUY_BEG_FO_HR_TL(ACTIVE_PATH_NUMBER))
         ALLOCATE(BUY_END_FO_HR_TL(ACTIVE_PATH_NUMBER))
         MULTI_AREA_BUY_MONTH_PRICE = 0. ! 12/22/03.
!
         BUY_BEG_FO_HR_TL = 0
         BUY_END_FO_HR_TL = 0
         TEMP_L = GET_SCENARIO_PRICE_NUM(R_YEAR, &
                                                SCENARIO_REFERENCE_NUM)
!
         DO I = 1, TEMP_I2
!
            LOCAL_PATH_NUMBER = BUYER_MARKET_INDEX(I,R_BUYER)
            TRANS_LINE_CONSTRAINT(LOCAL_PATH_NUMBER) = &
                  GET_TRANS_LINE_CONSTRAINT( &
                       TRANS_LINE_INDEX(LOCAL_PATH_NUMBER), &
                               TRANS_LINE_FOR,R_MONTH,R_YEAR)
            IF(TRANS_LINE_FOR > .001) THEN
               CALL GET_FO_HRS_TLINE(LOCAL_PATH_NUMBER, &
                                     R_YEAR, & !    R_YEAR_OR_SCEN,
                                     R_MONTH, &
                                     DAYS_IN_MONTH, &
                                     TRANS_LINE_FOR, &
                                   BUY_BEG_FO_HR_TL(LOCAL_PATH_NUMBER), &
                                   BUY_END_FO_HR_TL(LOCAL_PATH_NUMBER))
            ENDIF
!
            IF(TEMP_L) THEN
               IF(YES_REGIONAL_AREA_PRICE()) THEN
!                  CALL GET_NEW_MARKET_PRICE_NAME(MARKET_PRICE_NAME)
                  MARKET_PRICE_NAME = GET_HOURLY_PRICE_NAME(R_BUYER)
                  IF(YES_TWO_PRB_DIGITS()) THEN
                    MARKET_PRICE_NAME = trim(MARKET_PRICE_NAME(1:3))// &
                           trim(SCENARIO_REFERENCE_NUM(1:2))
                  ELSE
                    MARKET_PRICE_NAME = trim(MARKET_PRICE_NAME(1:2))// &
                           trim(SCENARIO_REFERENCE_NUM(1:3))
                  ENDIF
               ELSE
                  MARKET_PRICE_NAME = &
                     trim(MARKET_PRICE_ID(LOCAL_PATH_NUMBER))// &
                                          trim(SCENARIO_REFERENCE_NUM)
               ENDIF
               FILE_NAME = trim(PRB_FILE_DIRECTORY())//"PRB"// &
                     trim(MARKET_PRICE_NAME)// &
                     ".P"//LOAD_FILE_CHAR_EXT( &
                                          GET_MARKET_PRICE_YEAR(R_YEAR))
               INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
               IF(.NOT. FILE_EXISTS) THEN
                  FILE_NAME = trim(PRB_FILE_DIRECTORY())//"PRB"// &
                     trim(MARKET_PRICE_NAME)// &
                      ".P"//LOAD_FILE_CHAR_EXT( &
                                          GET_MARKET_PRICE_YEAR(R_YEAR))
                  INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
               ENDIF
            ELSE
               IF(YES_REGIONAL_AREA_PRICE()) THEN
!                  CALL GET_NEW_MARKET_PRICE_NAME(MARKET_PRICE_NAME)
                  MARKET_PRICE_NAME = GET_HOURLY_PRICE_NAME(R_BUYER)
                  IF(YES_TWO_PRB_DIGITS()) THEN
                    MARKET_PRICE_NAME = trim(MARKET_PRICE_NAME(1:3))// &
                           trim(SCENARIO_NUM(LOCAL_PATH_NUMBER)(1:2))
                  ELSE
                    MARKET_PRICE_NAME = trim(MARKET_PRICE_NAME(1:2))// &
                           trim(SCENARIO_NUM(LOCAL_PATH_NUMBER)(1:3))
                  ENDIF
               ELSE
                  MARKET_PRICE_NAME = &
                     trim(MARKET_PRICE_ID(LOCAL_PATH_NUMBER))// &
                                 trim(SCENARIO_NUM(LOCAL_PATH_NUMBER))
               ENDIF
               FILE_NAME = trim(PRB_FILE_DIRECTORY())//"PRB"// &
                     trim(MARKET_PRICE_NAME)// &
                     ".P"//LOAD_FILE_CHAR_EXT( &
                                          GET_MARKET_PRICE_YEAR(R_YEAR))
               INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
            ENDIF
            IF(.NOT. FILE_EXISTS) CYCLE
!
! READ THE PRB
!
            OPEN(UNIT=2801,FILE=FILE_NAME,RECL=118, &
                                           ACCESS="DIRECT",STATUS="OLD")
            J = 1
            IREC = ALINE_LOAD_DATA(J,R_MONTH)
!            DAYS_IN_MONTH = R_HOURS_IN_MONTH/24
!
            CURRENT_HR = 0
            DO DA = 1, DAYS_IN_MONTH
               READ(2801,REC=IREC) LDE_MONTH,LDE_DAY,LDE_YEAR, &
                                     EEICODE,DAY_WEEK, &
                                     TIMZON,TEMPER, &
                                     DELTMP, &
                                     (LOCAL_MARKET_PRICE(HR),HR=1,24)
               IREC = IREC + 1
               DO HR = 1, 24
                  CURRENT_HR = CURRENT_HR + 1
                  MULTI_AREA_BUY_MONTH_PRICE(CURRENT_HR,I) = &
                                                  LOCAL_MARKET_PRICE(HR)
               ENDDO
            ENDDO
            CLOSE(2801)
         END DO
!
! 01/14/04. FOR THE BUYER SIDE
!
         DO I = 1, TEMP_I2
!
            LOCAL_PATH_NUMBER = BUYER_MARKET_INDEX(I,R_BUYER)
            TRANS_LINE_CONSTRAINT(LOCAL_PATH_NUMBER) = &
                  GET_TRANS_LINE_CONSTRAINT( &
                                   TRANS_LINE_INDEX(LOCAL_PATH_NUMBER), &
                                 TRANS_LINE_FOR,R_MONTH,R_YEAR)
            IF(TRANS_LINE_FOR > .001) THEN
               CALL GET_FO_HRS_TLINE(LOCAL_PATH_NUMBER, &
                                     R_YEAR, & !    R_YEAR_OR_SCEN,
                                     R_MONTH, &
                                     DAYS_IN_MONTH, &
                                     TRANS_LINE_FOR, &
                                    BUY_BEG_FO_HR_TL(LOCAL_PATH_NUMBER), &
                                    BUY_END_FO_HR_TL(LOCAL_PATH_NUMBER))
            ENDIF
!
         ENDDO
      RETURN

      ENTRY GET_PRICING_GROUP_SELL_PRICE(R_HOUR, &
                                    R_BUYER, &
                                    R_PATH, &
                                    R_TIME_OF_DAY, &
                                    R_PRICING_GROUP_QUANT, &
                                    R_PRICING_GROUP_NAME, &
                                    R_MONTH, &
                                    R_YEAR)

         CURRENT_PATH = SELLER_MARKET_INDEX(R_PATH,R_BUYER)
!
! CALCULATE A DELIVERED PRICE, INCLUDING WHEELING_RATE,
!                                        MARKET_PRICE_DELTA,
!                                        PEAK/OFF-PEAK PRICE MULT
!
         IF(R_TIME_OF_DAY /= 1) THEN ! 1.0 = PEAK
            LOCAL_WHEEL_MULT = PATH_WHEEL_OFF_MULT(CURRENT_PATH)
            LOCAL_DELTA_MULT = OFF_PEAK_PRICE_DELTA_MULT(CURRENT_PATH)
         ELSE
            LOCAL_WHEEL_MULT = PATH_WHEEL_MULT(CURRENT_PATH)
            LOCAL_DELTA_MULT = PEAK_PRICE_DELTA_MULT(CURRENT_PATH)
         ENDIF
! 021306
         IF(LOCAL_WHEEL_MULT < 0.0) THEN
            LOCAL_WHEEL_MULT = &
                        ESCALATED_MONTHLY_VALUE( &
                                    ABS(LOCAL_WHEEL_MULT), &
                                    INT(ABS(LOCAL_WHEEL_MULT),2), &
                                                R_YEAR,R_MONTH,INT(1,2))
         ENDIF
         IF(LOCAL_DELTA_MULT < 0.0) THEN
            LOCAL_DELTA_MULT = &
                        ESCALATED_MONTHLY_VALUE( &
                                    ABS(LOCAL_DELTA_MULT), &
                                    INT(ABS(LOCAL_DELTA_MULT),2), &
                                                R_YEAR,R_MONTH,INT(1,2))
         ENDIF
!
         LOCAL_WHEELING_CHARGE = PATH_WHEEL_RATE(CURRENT_PATH) * &
                                                        LOCAL_WHEEL_MULT
         LOCAL_PRICE_DELTA = MARKET_PRICE_DELTA(CURRENT_PATH)* &
                                                        LOCAL_DELTA_MULT
         GET_PRICING_GROUP_SELL_PRICE = &
                  MULTI_AREA_MONTH_PRICE(R_HOUR,R_PATH) + &
                                     LOCAL_WHEELING_CHARGE + &
                                         LOCAL_PRICE_DELTA

!
         R_PRICING_GROUP_NAME = PATH_NAME(CURRENT_PATH)
!
!         SELLER_ID = SELLER_TRANSACTION_GROUP(CURRENT_PATH)
!         R_PRICING_GROUP_QUANT = TRANS_LINE_CONSTRAINT(CURRENT_PATH)
!
! 052712. COMPLETE REWRITE OF THIS SECTION.
!
         IF(DETAILED_CONSTRAINTS_ACTIVE) THEN
               CONSTRAINT_MULT = &
                    GET_TRANS_LINE_MULT( &
                                 TRANS_LINE_INDEX(CURRENT_PATH), &
                                 R_TIME_OF_DAY, &
                                 R_MONTH, &
                                 R_YEAR)
         ELSE
            CONSTRAINT_MULT = 1.0
         ENDIF
!
         TEMP_R = TRANS_LINE_CONSTRAINT(CURRENT_PATH)* CONSTRAINT_MULT
!
         IF(R_HOUR >= SELL_BEG_FO_HR_TL(CURRENT_PATH) .AND. &
                         R_HOUR <= SELL_END_FO_HR_TL(CURRENT_PATH)) THEN

!            R_PRICING_GROUP_QUANT = 0.
            R_PRICING_GROUP_QUANT = GET_TRANS_DERATE(CURRENT_PATH, &
                                                     TEMP_R, &
                                                     R_MONTH, &
                                                     R_YEAR)
         ELSE
            R_PRICING_GROUP_QUANT = TEMP_R
         ENDIF
!
      RETURN

      ENTRY GET_PRICING_GROUP_BUY_PRICE(R_HOUR, &
                                    R_BUYER, &
                                    R_PATH, &
                                    R_TIME_OF_DAY, &
                                    R_PRICING_GROUP_QUANT, &
                                    R_PRICING_GROUP_NAME, &
                                    R_MONTH, &
                                    R_YEAR)

!         CURRENT_PATH = BUYER_MARKET_INDEX(R_PATH,R_BUYER)
!
! 12/24/03. TO TEST THE MARKET INTERFACE
!
         CURRENT_PATH = BUYER_MARKET_INDEX(R_PATH,R_BUYER)
!
! CALCULATE A DELIVERED PRICE, INCLUDING WHEELING_RATE,
!                                        MARKET_PRICE_DELTA,
!                                        PEAK/OFF-PEAK PRICE MULT
!
         IF(R_TIME_OF_DAY /= 1) THEN ! 1.0 = PEAK
            LOCAL_WHEEL_MULT = PATH_WHEEL_OFF_MULT(CURRENT_PATH)
            LOCAL_DELTA_MULT = OFF_PEAK_PRICE_DELTA_MULT(CURRENT_PATH)
         ELSE
            LOCAL_WHEEL_MULT = PATH_WHEEL_MULT(CURRENT_PATH)
            LOCAL_DELTA_MULT = PEAK_PRICE_DELTA_MULT(CURRENT_PATH)
         ENDIF
! 021306
         IF(LOCAL_WHEEL_MULT < 0.0) THEN
            LOCAL_WHEEL_MULT = &
                        ESCALATED_MONTHLY_VALUE( &
                                    ABS(LOCAL_WHEEL_MULT), &
                                    INT(ABS(LOCAL_WHEEL_MULT),2), &
                                                R_YEAR,R_MONTH,INT(1,2))
         ENDIF
         IF(LOCAL_DELTA_MULT < 0.0) THEN
            LOCAL_DELTA_MULT = &
                        ESCALATED_MONTHLY_VALUE( &
                                    ABS(LOCAL_DELTA_MULT), &
                                    INT(ABS(LOCAL_DELTA_MULT),2), &
                                                R_YEAR,R_MONTH,INT(1,2))
         ENDIF
!
         LOCAL_WHEELING_CHARGE = PATH_WHEEL_RATE(CURRENT_PATH) * &
                                                        LOCAL_WHEEL_MULT
         LOCAL_PRICE_DELTA = MARKET_PRICE_DELTA(CURRENT_PATH)* &
                                                        LOCAL_DELTA_MULT
         GET_PRICING_GROUP_BUY_PRICE = &
                  MULTI_AREA_BUY_MONTH_PRICE(R_HOUR,R_PATH) + &
                                     LOCAL_WHEELING_CHARGE + &
                                         LOCAL_PRICE_DELTA

!
         R_PRICING_GROUP_NAME = PATH_NAME(CURRENT_PATH)
!
!         SELLER_ID = SELLER_TRANSACTION_GROUP(CURRENT_PATH)
         IF(R_HOUR >= BUY_BEG_FO_HR_TL(CURRENT_PATH) .AND. &
                           R_HOUR <=BUY_END_FO_HR_TL(CURRENT_PATH)) THEN

            R_PRICING_GROUP_QUANT = 0.
         ELSE
            IF(DETAILED_CONSTRAINTS_ACTIVE) THEN
               CONSTRAINT_MULT = &
                    GET_TRANS_LINE_MULT( &
                                 TRANS_LINE_INDEX(CURRENT_PATH), &
                                 R_TIME_OF_DAY, &
                                 R_MONTH, &
                                 R_YEAR)
            ELSE
               CONSTRAINT_MULT = 1.0
            ENDIF
            R_PRICING_GROUP_QUANT = &
                          TRANS_LINE_CONSTRAINT(CURRENT_PATH)* &
                                                         CONSTRAINT_MULT
         ENDIF
!
      RETURN

! CALLED MONTHLY FROM SIMULATE_MULTI_PARTY EVERY MONTH AFTER FIRST MONTH
!
      ENTRY UPDATE_TRANS_PATH_DATA(R_MONTH,R_YEAR)
!

!
         UPDATE_TRANS_PATH_DATA = .FALSE.
         IF(.NOT. PATH_FILE_EXISTS) RETURN
         UPDATE_TRANS_PATH_DATA = .TRUE.
         if(r_year<0) then
            call end_program("transobj2:0009 - r_year of" //  &
       trim(itos(int(r_year))) // " is invalid.")
      
         end if
!
         SCENARIO_TRANSMISSION_AVAIL = &
                         GET_SCENARIO_TRANSMISSION_AVAIL(R_YEAR,R_MONTH)
!
         VALUES_2_ZERO = INT((NUM_TRANS_GROUPS+2)* &
                                                  (NUM_TRANS_GROUPS+2))
           SEASON_PATH_LIMIT = 999999.
         DO PATH = 1, ACTIVE_PATH_NUMBER
!
            SELLER_ID = SELLER_TRANSACTION_GROUP(PATH)
            BUYER_ID = BUYER_TRANSACTION_GROUP(PATH)
! COMPRESS THE ID'S
            SELLER_ID = TRANS_GROUP_POSITION(SELLER_ID)
            BUYER_ID = TRANS_GROUP_POSITION(BUYER_ID)
!
            DO J = 1, MAX_WHEELS
!               TEMP_ID  = WHEEL_PATH(PATH,J)
!
!
               CURRENT_TIE = WHEEL_PATH(PATH,J)
               IF(  ACTIVE_PATH_FOR_LONG_PATH(PATH) > 0 .OR. &
                           (CURRENT_TIE == 0 .AND. .NOT. &
                                       ((J==1 .AND. SELLER_ID==0) .OR. &
                                      (J==2 .AND. BUYER_ID==0)))  ) then
                    EXIT
                end if

               IF( J == 1) THEN
                  LAST_TIE = SELLER_ID
               ELSE
                  ! FROM THE CONSTRAINT FILE
                  SEASON_PATH_LIMIT(SELLER_ID,BUYER_ID) = & 
                      MIN(SEASON_PATH_LIMIT(SELLER_ID,BUYER_ID), &
                              GET_SEASON_CONSTRAINT_LIMIT( &
                                                LAST_TIE,CURRENT_TIE)) * &
                                             SCENARIO_TRANSMISSION_AVAIL

               ENDIF
!
               LAST_TIE = CURRENT_TIE
            ENDDO
!
         ENDDO
      RETURN

      entry GET_TRANS_PATH_PARAMS(iLine,iNode,jNode,Impedance, &
         LineVoltageLevel,LinePowerLimit)


      GET_TRANS_PATH_PARAMS=.false.
      IF(.NOT. TRANS_PATH_DATA_READ) then
        RETURN ! force caller to use defaults
      end if
      PathID=iLine+1 ! caller uses 0-based indices
      do SELLER_ID=1,NUM_TRANS_GROUPS
         do BUYER_ID=1,NUM_TRANS_GROUPS ! assume PATHS_PER_PAIR==1
           if(PATHS_INDEX(SELLER_ID,BUYER_ID,1)==PathID) exit
         end do
         if(PATHS_INDEX(SELLER_ID,BUYER_ID,1)==PathID) exit
      end do
      if(PATHS_INDEX(SELLER_ID,BUYER_ID,1)/=PathID) then
         call end_program("transobj2:0026 - unmatched PathID " // &
         "in GET_TRANS_PATH_PARAMS")
      end if
      iNode=SELLER_ID-1
      jNode=BUYER_ID -1
      LinePowerLimit = SEASON_PATH_LIMIT(SELLER_ID,BUYER_ID)
      LineVoltageLevel = PATH_KV_RATING(PathID)
      Impedance        = PATH_INDUCTANCE(PathID)
      GET_TRANS_PATH_PARAMS=.true.
      return



! CALLED FROM SIMULATE_MULTI_PARTY AND MARKET_PARTY_TRANSACTION 
! EVERY HOUR
      ENTRY INIT_HOUR_PATH_LIMIT(R_TIME_OF_DAY,R_MONTH,R_YEAR)

         IF(.NOT. SAVE_PATH_FILE_EXISTS) RETURN

           HOUR_PATHS_INDEX = 0

         SELLER_TRACK_COUNTER = 0
         SELLER_TRACK_NUMBER = 13
         SELLER_TRACK_MW = 0.

         DO I = -1, NUM_TRANS_GROUPS
            DO J = -1, NUM_TRANS_GROUPS
               IF(DETAILED_CONSTRAINTS_ACTIVE) THEN
                  CONSTRAINT_MULT = &
                    GET_CONSTRAINT_MULT(I,J, &
                                           R_TIME_OF_DAY,R_MONTH,R_YEAR)
               ELSE
                  CONSTRAINT_MULT = 1.0
               ENDIF
               HOUR_PATH_LIMIT(I,J) = dble(SEASON_PATH_LIMIT(I,J) * &
                                                        CONSTRAINT_MULT)
               SAVE_HOUR_PATH_LIMIT(I,J) = HOUR_PATH_LIMIT(I,J)
               HOUR_PATHS_PER_PAIR(I,J) = PATHS_PER_PAIR(I,J)
!
               HOUR_PATHS_INDEX(I,J,1) = PATHS_INDEX(I,J,1)
               HOUR_PATHS_INDEX(I,J,2) = PATHS_INDEX(I,J,2)
               HOUR_PATHS_INDEX(I,J,3) = PATHS_INDEX(I,J,3)
               HOUR_PATHS_INDEX(I,J,4) = PATHS_INDEX(I,J,4)
               HOUR_PATHS_INDEX(I,J,5) = PATHS_INDEX(I,J,5)
               HOUR_PATHS_INDEX(I,J,6) = PATHS_INDEX(I,J,6)
               HOUR_PATHS_INDEX(I,J,7) = PATHS_INDEX(I,J,7)
               HOUR_PATHS_INDEX(I,J,8) = PATHS_INDEX(I,J,8)
               HOUR_PATHS_INDEX(I,J,9) = PATHS_INDEX(I,J,9)
               HOUR_PATHS_INDEX(I,J,10) = PATHS_INDEX(I,J,10)

            ENDDO
         ENDDO
! 01/02/03.
         HOUR_PATH_FOR_LONG_PATH = 0
         HOUR_WHEEL_PATH = 0
         HOUR_LONG_PATH_FOR_PAIR = 0

           HOUR_TIE_LOADING = 0.
           HOUR_PATH_MW = 0.0d0

         LONG_PATHS_PER_HOUR = 0

         INIT_HOUR_PATH_LIMIT = .TRUE.
      RETURN

      ! NUMBER OF PATHS WITH AVAILABLE CAPACITY
      ENTRY HOUR_PATH_ACTIVE(R_SELLER,R_BUYER) 

         IF(HOUR_PATHS_PER_PAIR(R_SELLER,R_BUYER) == 0) THEN
            HOUR_PATH_ACTIVE = .FALSE.
         ELSEIF(HOUR_PATH_LIMIT(R_SELLER,R_BUYER) <= 0.d0) THEN

!         NEED TO CHECK AGAINST HOUR PATH AND TIE CONSTRAINT

            HOUR_PATH_ACTIVE = .FALSE.
         ELSE
            HOUR_PATH_ACTIVE = .TRUE.
         ENDIF
      RETURN

      ENTRY PATH_FIRST_WHEELS()

         IF(.NOT. SAVE_PATH_FILE_EXISTS) THEN
            PATH_FIRST_WHEELS = -999
         ELSE
            PATH_FIRST_WHEELS = NUMBER_OF_WHEELS(1)
         ENDIF
      RETURN

      ENTRY TRANSACTION_GROUPS_W_PATHS()

         IF(.NOT. SAVE_PATH_FILE_EXISTS) THEN
            TRANSACTION_GROUPS_W_PATHS = -999
         ELSE
            TRANSACTION_GROUPS_W_PATHS = NUMBER_OF_TG
         ENDIF
      RETURN

      ENTRY PATH_WHEELING_CHARGE(R_SELLER,R_BUYER, &
                                           R_TIME_OF_DAY,R_MONTH,R_YEAR)

         IF(.NOT. SAVE_PATH_FILE_EXISTS) THEN
            PATH_WHEELING_CHARGE = 0.
         ELSE
            MAX_PATHS = HOUR_PATHS_PER_PAIR(R_SELLER,R_BUYER)
            IF(MAX_PATHS == 1) THEN
               CURRENT_PATH = HOUR_PATHS_INDEX(R_SELLER,R_BUYER,1)
               IF(R_TIME_OF_DAY /= 1) THEN ! 1.0 = PEAK
!                  LOCAL_WHEEL_MULT = 1.0
                  LOCAL_WHEEL_MULT = PATH_WHEEL_OFF_MULT(CURRENT_PATH)
                  IF(LOCAL_WHEEL_MULT < 0.0) THEN
                     LOCAL_WHEEL_MULT = &
                        ESCALATED_MONTHLY_VALUE( &
                                    ABS(LOCAL_WHEEL_MULT), &
                                    INT(ABS(LOCAL_WHEEL_MULT),2), &
                                                R_YEAR,R_MONTH,INT(1,2))
                  ENDIF
               ELSE
                  LOCAL_WHEEL_MULT = PATH_WHEEL_MULT(CURRENT_PATH)
                  IF(LOCAL_WHEEL_MULT < 0.0) THEN
                     LOCAL_WHEEL_MULT = &
                        ESCALATED_MONTHLY_VALUE( &
                                    ABS(LOCAL_WHEEL_MULT), &
                                    INT(ABS(LOCAL_WHEEL_MULT),2), &
                                                R_YEAR,R_MONTH,INT(1,2))
                  ENDIF
               ENDIF
               PATH_WHEELING_CHARGE = PATH_WHEEL_RATE(CURRENT_PATH) * &
                                                        LOCAL_WHEEL_MULT

            ELSEIF(MAX_PATHS <= 0) THEN
               PATH_WHEELING_CHARGE = 0.
            ELSE
! 071109. BROUGHT INITIALIZATION BACK IN ON PATH_WHEELING_CHARGE.
               PATH_WHEELING_CHARGE = 0.
!               WRITE(SCREEN_MESSAGES,*) 'INVALID MAX_PATHS',MAX_PATHS
!               CALL MG_LOCATE_WRITE(12,26,
!     +                           trim(SCREEN_MESSAGES),ALL_VERSIONS,2)
               DO I = 1, MAX_PATHS
!
                  CURRENT_PATH = HOUR_PATHS_INDEX(R_SELLER,R_BUYER,I)
!
                  IF(R_TIME_OF_DAY /= 1) THEN ! 1.0 = PEAK
!                     LOCAL_WHEEL_MULT = 1.0
                     LOCAL_WHEEL_MULT = &
                                       PATH_WHEEL_OFF_MULT(CURRENT_PATH)
                     IF(LOCAL_WHEEL_MULT < 0.0) THEN
                        LOCAL_WHEEL_MULT = &
                           ESCALATED_MONTHLY_VALUE( &
                                    ABS(LOCAL_WHEEL_MULT), &
                                    INT(ABS(LOCAL_WHEEL_MULT),2), &
                                                R_YEAR,R_MONTH,INT(1,2))
                     ENDIF
                  ELSE
                     LOCAL_WHEEL_MULT = PATH_WHEEL_MULT(CURRENT_PATH)
                     IF(LOCAL_WHEEL_MULT < 0.0) THEN
                        LOCAL_WHEEL_MULT = &
                           ESCALATED_MONTHLY_VALUE( &
                                    ABS(LOCAL_WHEEL_MULT), &
                                    INT(ABS(LOCAL_WHEEL_MULT),2), &
                                                R_YEAR,R_MONTH,INT(1,2))
                     ENDIF
                  ENDIF

                  PATH_WHEELING_CHARGE = PATH_WHEELING_CHARGE + &
                           PATH_WHEEL_RATE(CURRENT_PATH) * &
                                    LOCAL_WHEEL_MULT * &
                                              PATH_PERCENT(CURRENT_PATH)
               ENDDO
            ENDIF
         ENDIF
      RETURN

      ENTRY PATH_SPREAD_RATE(R_SELLER,R_BUYER, &
           R_TIME_OF_DAY,R_MONTH,R_YEAR) ! NUMBER OF PATHS WITH AVAILABLE CAPACITY

         IF(.NOT. SAVE_PATH_FILE_EXISTS) THEN
            PATH_SPREAD_RATE = 0.
         ELSE
            MAX_PATHS = HOUR_PATHS_PER_PAIR(R_SELLER,R_BUYER)
            IF(MAX_PATHS == 1) THEN
               CURRENT_PATH = HOUR_PATHS_INDEX(R_SELLER,R_BUYER,1)
!               PATH_SPREAD_RATE = PATH_SPREAD(CURRENT_PATH)
               IF(R_TIME_OF_DAY /= 1) THEN ! 1.0 = PEAK
!                  LOCAL_SPREAD_MULT = 1.0
                  LOCAL_SPREAD_MULT = PATH_SPREAD_OFF_MULT(CURRENT_PATH)
                  IF(LOCAL_SPREAD_MULT < 0.0) THEN
                     LOCAL_SPREAD_MULT = &
                        ESCALATED_MONTHLY_VALUE( &
                                    ABS(LOCAL_SPREAD_MULT), &
                                    INT(ABS(LOCAL_SPREAD_MULT),2), &
                                                R_YEAR,R_MONTH,INT(1,2))
                  ENDIF
               ELSE
                  LOCAL_SPREAD_MULT = PATH_SPREAD_MULT(CURRENT_PATH)
                  IF(LOCAL_SPREAD_MULT < 0.0) THEN
                     LOCAL_SPREAD_MULT = &
                        ESCALATED_MONTHLY_VALUE( &
                                    ABS(LOCAL_SPREAD_MULT), &
                                    INT(ABS(LOCAL_SPREAD_MULT),2), &
                                                R_YEAR,R_MONTH,INT(1,2))
                  ENDIF
               ENDIF
               PATH_SPREAD_RATE = PATH_SPREAD(CURRENT_PATH) * &
                                                       LOCAL_SPREAD_MULT
            ELSEIF(MAX_PATHS <= 0) THEN
               PATH_SPREAD_RATE = 0.
            ELSE
               PATH_SPREAD_RATE = 0.
               DO I = 1, MAX_PATHS
                  PATH_SPREAD_RATE = PATH_SPREAD_RATE + &
                           PATH_SPREAD(CURRENT_PATH) * &
                                              PATH_PERCENT(CURRENT_PATH)
               ENDDO
            ENDIF
         ENDIF
      RETURN

      ENTRY HOUR_TIE_LIMIT(R_SELLER,R_BUYER)

         IF(.NOT. SAVE_PATH_FILE_EXISTS) THEN
            HOUR_TIE_LIMIT = 999999.
         ELSE

            HOUR_TIE_LIMIT = sngl(HOUR_PATH_LIMIT(R_SELLER,R_BUYER))

         ENDIF
      RETURN

      ENTRY SET_HOUR_LONG_PATH_WI_PATHS(R_TIME_OF_DAY,R_MONTH,R_YEAR)

!
! 10/22/02. CURRENTLY ONLY SET FOR L_SELLER > L_LP > L_BUYER.
! ASSUMES THAT L_SELLER > L_LP AND L_LP > L_BUYER
!
         DO J = 1, PATHS_LONG_PATHS
!
            IF(LONG_PATHS_PER_HOUR < MAX_HOURLY_LONG_PATHS) THEN
               LONG_PATHS_PER_HOUR = LONG_PATHS_PER_HOUR + 1
!
               LOCAL_PATH_NUMBER = LONG_PATH_PATH(J)
               L_SELLER = TRANS_GROUP_POSITION( &
                            SELLER_TRANSACTION_GROUP(LOCAL_PATH_NUMBER))
!               L_LP = TRANS_GROUP_POSITION(WHEEL_PATH(K,2))
               L_BUYER = TRANS_GROUP_POSITION( &
                             BUYER_TRANSACTION_GROUP(LOCAL_PATH_NUMBER))
               HOUR_PATHS_PER_PAIR(L_SELLER,L_BUYER) = 1
!
               HOUR_PATHS_INDEX(L_SELLER,L_BUYER,1) = LOCAL_PATH_NUMBER
!
               K = 1
               L1 = TRANS_GROUP_POSITION( &
                                        WHEEL_PATH(LOCAL_PATH_NUMBER,K))
!
               PATH_PERCENT(LOCAL_PATH_NUMBER) = 1.0
!
               PATH_SPREAD(LOCAL_PATH_NUMBER) = 0.
               PATH_WHEEL_RATE(LOCAL_PATH_NUMBER) = 0.
!
               PATH_WHEEL_MULT(LOCAL_PATH_NUMBER) = 1.0
               PATH_WHEEL_OFF_MULT(LOCAL_PATH_NUMBER) = 1.0
!
               K = K + 1
               DOWHILE(K <= 10 .AND. &
                                    WHEEL_PATH(LOCAL_PATH_NUMBER,K) > 0)
                  L2 = TRANS_GROUP_POSITION( &
                                        WHEEL_PATH(LOCAL_PATH_NUMBER,K))

                  CURRENT_SELLER_PATH = &
                                   HOUR_PATHS_INDEX(L1,L2,1)
                  PATH_SPREAD(LOCAL_PATH_NUMBER) = &
                           PATH_SPREAD(LOCAL_PATH_NUMBER) + &
                                        PATH_SPREAD(CURRENT_SELLER_PATH)
!
                  IF(R_TIME_OF_DAY /= 1) THEN ! 1.0 = PEAK
                     LOCAL_WHEEL_MULT = &
                                PATH_WHEEL_OFF_MULT(CURRENT_SELLER_PATH)
                     IF(LOCAL_WHEEL_MULT < 0.0) THEN
                        LOCAL_WHEEL_MULT = &
                           ESCALATED_MONTHLY_VALUE( &
                                    ABS(LOCAL_WHEEL_MULT), &
                                    INT(ABS(LOCAL_WHEEL_MULT),2), &
                                                R_YEAR,R_MONTH,INT(1,2))
                     ENDIF
                  ELSE
                     LOCAL_WHEEL_MULT = &
                                    PATH_WHEEL_MULT(CURRENT_SELLER_PATH)
                     IF(LOCAL_WHEEL_MULT < 0.0) THEN
                        LOCAL_WHEEL_MULT = &
                           ESCALATED_MONTHLY_VALUE( &
                                    ABS(LOCAL_WHEEL_MULT), &
                                    INT(ABS(LOCAL_WHEEL_MULT),2), &
                                                R_YEAR,R_MONTH,INT(1,2))
                     ENDIF
                  ENDIF
                  PATH_WHEEL_RATE(LOCAL_PATH_NUMBER) = &
                           PATH_WHEEL_RATE(LOCAL_PATH_NUMBER) + &
                                  PATH_WHEEL_RATE(CURRENT_SELLER_PATH) * &
                                                        LOCAL_WHEEL_MULT
                  K = K + 1
                  L1 = L2
!


               ENDDO
            ENDIF ! LONG PATHS AVAILABLE
         ENDDO
         SET_HOUR_LONG_PATH_WI_PATHS = .TRUE.
      RETURN

      ENTRY SET_HOUR_LONG_CAP_TWH(R_SELLER,R_BUYER)

!
! 10/24/02.
!
!
         LOCAL_PATH_NUMBER = LONG_PATH_FOR_PAIR(R_SELLER,R_BUYER)
         IF(LOCAL_PATH_NUMBER > 0) THEN
!
            K = 1
            L1 = TRANS_GROUP_POSITION(WHEEL_PATH(LOCAL_PATH_NUMBER,K))
            K = K + 1
            DOWHILE(K <= 10 .AND. WHEEL_PATH(LOCAL_PATH_NUMBER,K) > 0)
               L2 = TRANS_GROUP_POSITION( &
                                        WHEEL_PATH(LOCAL_PATH_NUMBER,K))
               HOUR_PATH_LIMIT(R_SELLER,R_BUYER) = &
                        MIN(HOUR_PATH_LIMIT(R_SELLER,R_BUYER), &
                                                 HOUR_PATH_LIMIT(L1,L2))
               K = K + 1
               L1 = L2
            ENDDO
!
            SET_HOUR_LONG_CAP_TWH = .TRUE.
         ELSE
            SET_HOUR_LONG_CAP_TWH = .FALSE.
         ENDIF
      RETURN

      ENTRY SET_HOUR_LONG_PATH_PARAMS(R_CAPACITY,R_SELLER,R_BUYER,R_LP)

         HOUR_PATH_LIMIT(R_SELLER,R_BUYER) = R_CAPACITY
!         SET_HOUR_LONG_PATH_PARAMS = HOUR_PATH_LIMIT(R_SELLER,R_BUYER)
!
! UNEXPECTED RESULTS???
!
         IF(LONG_PATHS_PER_HOUR < MAX_HOURLY_LONG_PATHS) THEN
            LONG_PATHS_PER_HOUR = LONG_PATHS_PER_HOUR + 1
            LOCAL_PATH_NUMBER = ACTIVE_PATH_NUMBER + LONG_PATHS_PER_HOUR
            HOUR_PATHS_PER_PAIR(R_SELLER,R_BUYER) = 1

            HOUR_PATHS_INDEX(R_SELLER,R_BUYER,1) = LOCAL_PATH_NUMBER

            CURRENT_SELLER_PATH = HOUR_PATHS_INDEX(R_SELLER,R_LP,1)
            CURRENT_BUYER_PATH = HOUR_PATHS_INDEX(R_LP,R_BUYER,1)
!
            IF(CURRENT_SELLER_PATH > 0. .AND. &
                                    CURRENT_BUYER_PATH > 0. .AND. &
                           MAX_HOURLY_LONG_PATHS <= SAVE_MAX_PATHS .AND. &
                        CURRENT_SELLER_PATH <= SAVE_MAX_PATHS .AND. &
                              CURRENT_BUYER_PATH <= SAVE_MAX_PATHS) THEN
               PATH_SPREAD(LOCAL_PATH_NUMBER) = &
                              PATH_SPREAD(CURRENT_SELLER_PATH) + &
                                         PATH_SPREAD(CURRENT_BUYER_PATH)
               PATH_WHEEL_RATE(LOCAL_PATH_NUMBER) = &
                              PATH_WHEEL_RATE(CURRENT_SELLER_PATH) + &
                                     PATH_WHEEL_RATE(CURRENT_BUYER_PATH)
!
               PATH_PERCENT(LOCAL_PATH_NUMBER) = 1.0
!
! CODE I NEED TO "REVERSE" FROM REDUCE PATH CAPACITY, THEN SAY IF
! ACTIVE_PATH_FOR_LONG_PATH(PATH) > ACTIVE_PATH_NUMBER THEN 
! GET THIS STUFF
! FOR THESE LONG PATHS, USE THE TG POSITION'S NOT THE INPUT FILE VALUES
!
               HOUR_PATH_FOR_LONG_PATH(LOCAL_PATH_NUMBER) = &
                                                     LONG_PATHS_PER_HOUR
               K = 1
               L1 = R_SELLER
               HOUR_WHEEL_PATH(LONG_PATHS_PER_HOUR,K) = R_SELLER
!
               K = K + 1
               HOUR_WHEEL_PATH(LONG_PATHS_PER_HOUR,K) = R_LP
               HOUR_LONG_PATH_FOR_PAIR(R_SELLER,R_LP) = &
                                                     LONG_PATHS_PER_HOUR
               K = K + 1
               HOUR_WHEEL_PATH(LONG_PATHS_PER_HOUR,K) = R_BUYER
               HOUR_LONG_PATH_FOR_PAIR(R_LP,R_BUYER) = &
                                                     LONG_PATHS_PER_HOUR
!
! I NEED A RECURSIVE ROUTINE HERE FOR LONG PATHS
!
!               DOWHILE(K <= 10)
!                  IF(WHEEL_PATH(PATH,K) > 0) THEN
!                  ELSE
!                     WHEEL_PATH(LOCAL_PATH_NUMBER,K) = L2
!                  ENDIF
!
!                  K = K + 1
!                  L1 = L2
!               ENDDO
               SET_HOUR_LONG_PATH_PARAMS = .TRUE.
            ELSE
               SET_HOUR_LONG_PATH_PARAMS = .FALSE.
               MAX_PATHS = MAX_PATHS
            ENDIF

         ENDIF
      RETURN

!      ENTRY REDUCE_PATH_CAPACITY(R_HOUR,R_MONTH,
!     +                           R_SELLER,R_BUYER,R_CAPACITY)
      ENTRY REDUCE_PATH_CAPACITY( &
                                          R_CAPACITY, &
                                          R_SELLER,R_BUYER, &
                                          R_TIME_OF_DAY,R_MONTH,R_YEAR)

!
         IF(.NOT. SAVE_PATH_FILE_EXISTS) RETURN
!
         REDUCE_PATH_CAPACITY = .FALSE.

            TEMP_CAPACITY = R_CAPACITY
!
         IF(HOUR_PATH_LIMIT(R_SELLER,R_BUYER)-TEMP_CAPACITY >-.5d0) THEN
            MAX_PATHS = HOUR_PATHS_PER_PAIR(R_SELLER,R_BUYER)
            IF(MAX_PATHS > 0) THEN
              DO I = 1, MAX_PATHS
!
               PATH = HOUR_PATHS_INDEX(R_SELLER,R_BUYER,I)
!
               CURRENT_PATH = HOUR_PATHS_INDEX(R_SELLER,R_BUYER,I)
               CURRENT_PATH_MW = &
                              TEMP_CAPACITY * PATH_PERCENT(CURRENT_PATH)
!

               TEMP_R = MIN(dble(CURRENT_PATH_MW), &
                                      HOUR_PATH_LIMIT(R_SELLER,R_BUYER))
               HOUR_PATH_LIMIT(R_SELLER,R_BUYER) = &
                              HOUR_PATH_LIMIT(R_SELLER,R_BUYER) - TEMP_R
!
! NET THE LINE LOADING (E.G. THIS SELL FREES-UP A BUY IN PREVIOUS
! DIRECTION)
!
               HOUR_TIE_LOADING(R_SELLER,R_BUYER) = &
                              HOUR_TIE_LOADING(R_SELLER,R_BUYER)  + &
                                                                  TEMP_R
!
! 10/24/02. LOOKING IN THE REVERSE DIRECTION.
!
               IF(HOUR_TIE_LOADING(R_BUYER,R_SELLER) > 0.) THEN
                  TEMP_R = MIN(CURRENT_PATH_MW, &
                                HOUR_TIE_LOADING(R_BUYER,R_SELLER))
                  HOUR_PATH_LIMIT(R_BUYER,R_SELLER) = &
                       MIN(HOUR_PATH_LIMIT(R_BUYER,R_SELLER) + TEMP_R, &
                              dble(SEASON_PATH_LIMIT(R_BUYER,R_SELLER)))
                  HOUR_TIE_LOADING(R_BUYER,R_SELLER) = &
                             HOUR_TIE_LOADING(R_BUYER,R_SELLER) - TEMP_R
               ENDIF
!
! 10/23/02. PATHS WITHIN PATHS FILE LOGIC
!
! 11/22/02. IN SEARCH OF
!
               IF(PATH > SAVE_MAX_PATHS) THEN
                  WRITE(4,*) "LONG PATH EXCEEDS MAX PATHS", &
                                             R_TIME_OF_DAY, &
                                             R_MONTH, &
                                             R_YEAR
                  CYCLE
               ENDIF
! 01/02/03
               IF(HOUR_PATH_FOR_LONG_PATH(PATH) > 0) THEN

                  J = HOUR_PATH_FOR_LONG_PATH(PATH)
!
                  K = 1
                  L1 = HOUR_WHEEL_PATH(J,K)
                  K = K + 1
!
! 01/22/03. THIS NEEDS TO BE RECURSIVE.
!
                  DOWHILE(K <= 10 .AND. HOUR_WHEEL_PATH(J,K) > 0)
                     L2 = HOUR_WHEEL_PATH(J,K)
!
                     HOUR_PATH_LIMIT(L1,L2) = &
                        HOUR_PATH_LIMIT(L1,L2) - CURRENT_PATH_MW
!
                     HOUR_PATH_MW(L1,L2) = &
                               HOUR_PATH_MW(L1,L2) + CURRENT_PATH_MW
!
! 10/24/02. LOOKING IN THE REVERSE DIRECTION.
!
                     IF(HOUR_TIE_LOADING(L2,L1) > 0.) THEN
                        TEMP_R = MIN(CURRENT_PATH_MW, &
                                HOUR_TIE_LOADING(L2,L1))
                        HOUR_PATH_LIMIT(L2,L1) = &
                            MIN(HOUR_PATH_LIMIT(L2,L1) + TEMP_R, &
                                  dble(SEASON_PATH_LIMIT(L2,L1)))
                        HOUR_TIE_LOADING(L2,L1) = &
                             HOUR_TIE_LOADING(L2,L1) - TEMP_R
                     ENDIF
                     K = K + 1
                     L1 = L2
                  ENDDO
               ELSEIF(HOUR_LONG_PATH_FOR_PAIR(R_SELLER,R_BUYER) > &
                                                                 0) THEN
                  J = HOUR_LONG_PATH_FOR_PAIR(R_SELLER,R_BUYER)
                  K = 1
                  L1 = HOUR_WHEEL_PATH(J,K)
                  K = K + 2
                  L2 = HOUR_WHEEL_PATH(J,K)
!                  HOUR_PATH_LIMIT(L1,L2) =
!     +                  HOUR_PATH_LIMIT(L1,L2) - CURRENT_PATH_MW
                  HOUR_PATH_LIMIT(L1,L2) = &
                      MAX(0.d0,HOUR_PATH_LIMIT(L1,L2) - CURRENT_PATH_MW)
!
                  HOUR_PATH_MW(R_SELLER,R_BUYER) = &
                        HOUR_PATH_MW(R_SELLER,R_BUYER) + CURRENT_PATH_MW
                  HOUR_PATH_MW(L1,L2) = &
                           MIN(HOUR_PATH_MW(L1,L2)+CURRENT_PATH_MW, &
                                            SAVE_HOUR_PATH_LIMIT(L1,L2))
!
! LOOK IN THE REVERSE DIRECTION.
!
                  IF(HOUR_TIE_LOADING(L2,L1) > 0.) THEN
                     TEMP_R = MIN(CURRENT_PATH_MW, &
                                HOUR_TIE_LOADING(L2,L1))
                     HOUR_PATH_LIMIT(L2,L1) = &
                            MIN(HOUR_PATH_LIMIT(L2,L1) + TEMP_R, &
                                 dble(SEASON_PATH_LIMIT(L2,L1)))
                     HOUR_TIE_LOADING(L2,L1) = &
                             HOUR_TIE_LOADING(L2,L1) - TEMP_R
                  ENDIF
               ELSEIF(ACTIVE_PATH_FOR_LONG_PATH(PATH) > 0) THEN ! ASSUME LONG PATH

                  K = 1
                  L1 = TRANS_GROUP_POSITION(WHEEL_PATH(PATH,K))

                  K = K + 1
                  DOWHILE(K <= 10 .AND. WHEEL_PATH(PATH,K) > 0)
                     L2 = TRANS_GROUP_POSITION(WHEEL_PATH(PATH,K))

                     HOUR_PATH_LIMIT(L1,L2) = &
                        HOUR_PATH_LIMIT(L1,L2) - CURRENT_PATH_MW

                     HOUR_PATH_MW(L1,L2) = &
                               HOUR_PATH_MW(L1,L2) + CURRENT_PATH_MW

! 10/24/02. LOOKING IN THE REVERSE DIRECTION.
                     IF(HOUR_TIE_LOADING(L2,L1) > 0.) THEN
                        TEMP_R = MIN(CURRENT_PATH_MW, &
                                HOUR_TIE_LOADING(L2,L1))
                        HOUR_PATH_LIMIT(L2,L1) = &
                            MIN(HOUR_PATH_LIMIT(L2,L1) + TEMP_R, &
                                 dble(SEASON_PATH_LIMIT(L2,L1)))
                        HOUR_TIE_LOADING(L2,L1) = &
                             HOUR_TIE_LOADING(L2,L1) - TEMP_R
                     ENDIF
                     K = K + 1
                     L1 = L2
                  ENDDO


               ELSEIF(LONG_PATH_FOR_PAIR(R_SELLER,R_BUYER) < 0) THEN
                     LOCAL_PATH_NUMBER = &
                               ABS(LONG_PATH_FOR_PAIR(R_SELLER,R_BUYER))
                  K = 1
                  L1 = &
                   TRANS_GROUP_POSITION(WHEEL_PATH(LOCAL_PATH_NUMBER,K))
                  DOWHILE(K < 10)
                     K = K + 1
                     IF(WHEEL_PATH(LOCAL_PATH_NUMBER,K) == 0) EXIT
                     L2 = TRANS_GROUP_POSITION( &
                                        WHEEL_PATH(LOCAL_PATH_NUMBER,K))
                  ENDDO
                  HOUR_PATH_LIMIT(L1,L2) = &
                      MAX(0.d0,HOUR_PATH_LIMIT(L1,L2) - CURRENT_PATH_MW)
!
                  HOUR_PATH_MW(R_SELLER,R_BUYER) = &
                        HOUR_PATH_MW(R_SELLER,R_BUYER) + CURRENT_PATH_MW

! LOOK IN THE REVERSE DIRECTION.
                  IF(HOUR_TIE_LOADING(L2,L1) > 0.) THEN
                     TEMP_R = MIN(CURRENT_PATH_MW, &
                                HOUR_TIE_LOADING(L2,L1))
                     HOUR_PATH_LIMIT(L2,L1) = &
                            MIN(HOUR_PATH_LIMIT(L2,L1) + TEMP_R, &
                                 dble(SEASON_PATH_LIMIT(L2,L1)))
                     HOUR_TIE_LOADING(L2,L1) = &
                             HOUR_TIE_LOADING(L2,L1) - TEMP_R
                  ENDIF
               ELSE
!
                  HOUR_PATH_MW(R_SELLER,R_BUYER) = &
                        HOUR_PATH_MW(R_SELLER,R_BUYER) + CURRENT_PATH_MW
               ENDIF

              ENDDO
            ELSE ! ASSUME LONG PATH
               HOUR_PATH_LIMIT(R_SELLER,R_BUYER) = &
                     HOUR_PATH_LIMIT(R_SELLER,R_BUYER) - R_CAPACITY
            ENDIF
            REDUCE_PATH_CAPACITY = .TRUE.
         ELSE
            WRITE(4,*) "Hourly transaction capacity limit exceeded"
            WRITE(4,*) "in hour ",R_TIME_OF_DAY
            WRITE(4,*) "and month ",R_MONTH

         ENDIF
         IF(HOUR_PATH_LIMIT(R_SELLER,R_BUYER) < 0.0) THEN
            HOUR_PATH_LIMIT(R_SELLER,R_BUYER)  = 0.0
         ENDIF
         IF(R_SELLER == SELLER_TRACK_NUMBER) THEN
            IF(SELLER_TRACK_COUNTER < 1000) THEN
               SELLER_TRACK_COUNTER = SELLER_TRACK_COUNTER + 1
               SELLER_TRACK_MW(SELLER_TRACK_COUNTER) = CURRENT_PATH_MW
            ENDIF
         ENDIF
      RETURN

      ENTRY GET_MAX_IN_PATHS()

         GET_MAX_IN_PATHS = ACTIVE_PATH_NUMBER
      RETURN

      ENTRY GET_SEASON_PATH_LIMIT(R_SELLER,R_BUYER)

         IF(ALLOCATED(SEASON_PATH_LIMIT)) THEN
            GET_SEASON_PATH_LIMIT = SEASON_PATH_LIMIT(R_SELLER,R_BUYER)
         ELSE
            GET_SEASON_PATH_LIMIT = 999999.
         ENDIF
      RETURN

      ENTRY GET_HOUR_PATH_MW(R_PATH,R_SELLER_PATH,R_BUYER_PATH)

         IF(R_PATH <= ACTIVE_PATH_NUMBER .AND. &
                                         WHEEL_PATH(R_PATH,3) == 0) THEN
            R_SELLER_PATH = WHEEL_PATH(R_PATH,1)
            R_BUYER_PATH = WHEEL_PATH(R_PATH,2)
            GET_HOUR_PATH_MW = MAX(0.d0, &
                              HOUR_PATH_MW(R_SELLER_PATH,R_BUYER_PATH)- &
                               HOUR_PATH_MW(R_BUYER_PATH,R_SELLER_PATH))
         ELSE
            R_SELLER_PATH = 0
            R_BUYER_PATH = 0
            GET_HOUR_PATH_MW = 0.d0
         ENDIF
      RETURN

      ENTRY GET_HOUR_PATHS_INDEX(R_SELLER,R_BUYER)

         IF(R_SELLER > 0 .AND. R_SELLER <= NUM_TRANS_GROUPS .AND. &
                     R_BUYER > 0 .AND. R_BUYER <= NUM_TRANS_GROUPS) THEN
            GET_HOUR_PATHS_INDEX = HOUR_PATHS_INDEX(R_SELLER,R_BUYER,1)
         ELSE
            GET_HOUR_PATHS_INDEX = 0
         ENDIF
      RETURN

      ENTRY GET_HOUR_PATH_FOR_LONG_PATH(R_PATH)

         GET_HOUR_PATH_FOR_LONG_PATH = HOUR_PATH_FOR_LONG_PATH(R_PATH)
      RETURN

      ENTRY GET_HOUR_WHEEL_PATH(R_PATH,R_DEPTH)

         GET_HOUR_WHEEL_PATH = HOUR_WHEEL_PATH(R_PATH,R_DEPTH)
      RETURN

      ENTRY CHANGE_THIS_PATH_SEGMENT(R_SELLER,R_BUYER,R_CAPACITY)

         HOUR_PATH_LIMIT(R_SELLER,R_BUYER) = &
                          HOUR_PATH_LIMIT(R_SELLER,R_BUYER) - R_CAPACITY
!
         HOUR_PATH_MW(R_SELLER,R_BUYER) = &
                             HOUR_PATH_MW(R_SELLER,R_BUYER) + R_CAPACITY
         CHANGE_THIS_PATH_SEGMENT = R_CAPACITY
      RETURN

      ENTRY HOURLY_PATHS_REPORTS(R_HOUR,R_TIME_OF_DAY,R_MONTH,R_YEAR)

         IF(.NOT. SAVE_PATH_FILE_EXISTS .OR. &
                                 .NOT. WRITE_DAILY_PATHS_REPORTS) RETURN
         IF(R_HOUR == 1) THEN
            DAILY_TIE_LOADING = 0.
            DAILY_TIE_CONSTRAINT = 0.
         ENDIF
         DO I = 1, ACTIVE_PATH_NUMBER
            IF(WHEEL_PATH(I,3) > 0) CYCLE
            L1 = WHEEL_PATH(I,1)
            L2 = WHEEL_PATH(I,2)

            DAILY_TIE_CONSTRAINT(I,R_HOUR) = SAVE_HOUR_PATH_LIMIT(L1,L2)
! 062304 FOR BURESH
            DAILY_TIE_LOADING(I,R_HOUR) = &
                  MIN(DAILY_TIE_CONSTRAINT(I,R_HOUR), &
            REAL(MAX(0.d0,HOUR_PATH_MW(L1,L2)-HOUR_PATH_MW(L2,L1))))

         ENDDO
         HOURLY_PATHS_REPORTS = .TRUE.
      RETURN
!

      ENTRY DAILY_PATHS_REPORTS(R_END_POINT,R_YEAR,R_DAY,R_MONTH_NAME, &
                                R_STEP_4)

!
         IF(.NOT. SAVE_PATH_FILE_EXISTS .OR. &
                                 .NOT. WRITE_DAILY_PATHS_REPORTS) RETURN
!
!
         IF(HOURLY_FLOW_REPORT_NOT_OPEN) THEN
            HOURLY_FLOW_REPORT_NOT_OPEN = .FALSE.
            HOURLY_FLOW_REPORT_VARIABLES = 24
            HOURLY_FLOW_ANALYST_NO = &
                         HOURLY_FLOW_ANALYST_HEADER( &
                                           HOURLY_FLOW_REPORT_VARIABLES, &
                                           HOURLY_FLOW_ANALYST_REC)
            HOURLY_FLOW_CONSTRAINT_NO = &
                         HOURLY_FLOW_CONSTRAINT_HEADER( &
                                           HOURLY_FLOW_REPORT_VARIABLES, &
                                           HOURLY_FLOW_CONSTRAINT_REC)
         ENDIF
!
         DO I = 1, ACTIVE_PATH_NUMBER
            IF(WHEEL_PATH(I,3) > 0) CYCLE
!
             IF(R_STEP_4) THEN
               DO J = 1, 6
                  K = (J-1)*4 + 1
                  DAILY_TIE_LOADING(I,K+1) =  DAILY_TIE_LOADING(I,K)
                  DAILY_TIE_LOADING(I,K+2) =  DAILY_TIE_LOADING(I,K)
                  DAILY_TIE_LOADING(I,K+3) =  DAILY_TIE_LOADING(I,K)
!
                  DAILY_TIE_CONSTRAINT(I,K+1)=DAILY_TIE_CONSTRAINT(I,K)
                  DAILY_TIE_CONSTRAINT(I,K+2)=DAILY_TIE_CONSTRAINT(I,K)
                  DAILY_TIE_CONSTRAINT(I,K+3)=DAILY_TIE_CONSTRAINT(I,K)
               ENDDO
             ENDIF
!
             HOURLY_FLOW_ANALYST_REC = RPTREC(HOURLY_FLOW_ANALYST_NO)
             WRITE(HOURLY_FLOW_ANALYST_NO,REC=HOURLY_FLOW_ANALYST_REC) &
                              FLOAT(R_END_POINT), &
                              FLOAT(R_YEAR), &
                              R_MONTH_NAME, &
                              FLOAT(R_DAY), &
                              LONG_PATH_NAME(I), &
                              (DAILY_TIE_LOADING(I,J),J=1,24)
             HOURLY_FLOW_ANALYST_REC = HOURLY_FLOW_ANALYST_REC + 1
!
             HOURLY_FLOW_CONSTRAINT_REC = &
                                       RPTREC(HOURLY_FLOW_CONSTRAINT_NO)
             WRITE(HOURLY_FLOW_CONSTRAINT_NO, &
                                         REC=HOURLY_FLOW_CONSTRAINT_REC) &
                              FLOAT(R_END_POINT), &
                              FLOAT(R_YEAR), &
                              R_MONTH_NAME, &
                              FLOAT(R_DAY), &
                              LONG_PATH_NAME(I), &
                              (DAILY_TIE_CONSTRAINT(I,J),J=1,24)
             HOURLY_FLOW_CONSTRAINT_REC = HOURLY_FLOW_CONSTRAINT_REC + 1
         ENDDO
         DAILY_PATHS_REPORTS = .TRUE.
      RETURN
      END

!
!                  ROUTINE TO CONVERT TIE CONSTRAINTS FILE
!
!                          COPYRIGHT (C) 1998
!                     M.S. GERBER & ASSOCIATES, INC.
!                          ALL RIGHTS RESERVED
!

!
      SUBROUTINE CONSTRAINT_OBJECT
      use end_routine, only: end_program, er_message
      use filename_tracker
!
      use spindriftlib
      use prod_arrays_dimensions
      USE SIZECOM
      INTEGER(kind=2) :: DELETE,INUNIT,IREC,LRECL=100 ! 052712 KCPL
      INTEGER :: IOS
!
      INTEGER(kind=2) :: UNIT_NUM=10
      CHARACTER(len=8) :: CONSTRAINT_ID*20,SOURCE
      CHARACTER(len=1) :: CONSTRAINT_ACTIVE,SEASON, &
                  TRANS_DERATE,TRANS_DERATE_DIST
      REAL(kind=4) :: MW_LIMIT, &
                  PEAK_MULT, &
                  OFF_PEAK_MULT, &
                  FORCED_OUTAGE_RATE, &
                  CapTransMultiplier, &
                  FORCED_OUTAGE_DERATE_MW
      INTEGER(kind=2) :: TRANS_LINE_INDEX
      INTEGER(kind=2) :: FROM_AREA(10),TO_AREA(10)
      INTEGER(kind=2) :: SAVE_NUMBER_OF_CONSTRAINTS=0, &
                  SAVE_MAX_TRANS_LINE_INDEX=1, &
                  R_MAX_TRANS_LINE_INDEX, &
                  R_NUMBER_OF_CONSTRAINTS
      CHARACTER(len=5) :: BASE_FILE_NAME,OVERLAY_FAMILY_NAME, &
                  TRANS_CONSTRAINT_FILE
      CHARACTER(len=256) :: FILE_NAME
      CHARACTER(len=256) :: BASE_FILE_DIRECTORY
      CHARACTER(len=256) :: DATA_DRIVE,OUTPUT_DIRECTORY
      LOGICAL(kind=4) :: FILE_EXISTS,CONSTRAINT_FILE_EXISTS=.FALSE., &
                   R_CONSTRAINT_FILE_EXISTS
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER(len=1024) :: RECLN
! DECLARATION FOR TRANSACT CONSTRAINT DETERMINANTS
      CHARACTER(len=23) :: FILE_TYPE='TRANSACT Tie Constraint'
      CHARACTER(len=2) :: TRCON_OL='BC'
      LOGICAL(kind=1) :: LAHEY_LF95
      CHARACTER(len=30) :: SCREEN_OUTPUT


! CONVERT THE CONSTRAINT FILE
      ENTRY CONSTRAINT_MAKEBIN

      BASE_FILE_NAME = TRANS_CONSTRAINT_FILE()
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME = get_tnb_filename(base_file_name)
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      CONSTRAINT_FILE_EXISTS = FILE_EXISTS
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_LOCATE_WRITE(16,30,BASE_FILE_NAME,ALL_VERSIONS,0)
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         ENDIF
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=trim(DATA_DRIVE)// &
                            "BCTRCON.BIN",ACCESS="DIRECT", &
                                            STATUS="UNKNOWN",RECL=LRECL)
         IREC = 1
         SAVE_MAX_TRANS_LINE_INDEX = 1

         READ(10,1000,IOSTAT=IOS) RECLN

         DO
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(IOS /= 0) EXIT
            READ(RECLN,*,ERR=200) DELETE

            IF(DELETE == 7) CYCLE ! NEW TABLE

            RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'

            PEAK_MULT = 1.0
            OFF_PEAK_MULT = 1.0
            FORCED_OUTAGE_RATE = 0.0
            TRANS_LINE_INDEX = 0
            CapTransMultiplier = 1.0
            TRANS_DERATE = 'F'
            TRANS_DERATE_DIST = 'F'
            FORCED_OUTAGE_DERATE_MW = 0.0
!
            READ(RECLN,*,ERR=200) DELETE, & !  1
                  CONSTRAINT_ID, & !  20
                  CONSTRAINT_ACTIVE, & !  1
                  MW_LIMIT, & !  4
                  SOURCE, & !  8
                  FROM_AREA(1), & !  2 x 20
                  TO_AREA(1), &
                  FROM_AREA(2), &
                  TO_AREA(2), &
                  FROM_AREA(3), &
                  TO_AREA(3), &
                  FROM_AREA(4), &
                  TO_AREA(4), &
                  FROM_AREA(5), &
                  TO_AREA(5), &
                  FROM_AREA(6), &
                  TO_AREA(6), &
                  FROM_AREA(7), &
                  TO_AREA(7), &
                  FROM_AREA(8), &
                  TO_AREA(8), &
                  FROM_AREA(9), &
                  TO_AREA(9), &
                  FROM_AREA(10), &
                  TO_AREA(10), &
                  SEASON, &
                  PEAK_MULT, &
                  OFF_PEAK_MULT, &
                  FORCED_OUTAGE_RATE, &
                  TRANS_LINE_INDEX, &
                  CapTransMultiplier, &
                  TRANS_DERATE, &
                  TRANS_DERATE_DIST, &
                  FORCED_OUTAGE_DERATE_MW
! NOTE REFORMATTING FOR THE BINARY FILE.
            WRITE(11,REC=IREC) DELETE, &
                  CONSTRAINT_ID, &
                  CONSTRAINT_ACTIVE, &
                  MW_LIMIT, &
                  SOURCE, &
                  FROM_AREA, &
                  TO_AREA, &
                  SEASON, &
                  PEAK_MULT, &
                  OFF_PEAK_MULT, &
                  FORCED_OUTAGE_RATE, &
                  TRANS_LINE_INDEX, &
                  CapTransMultiplier, &
                  TRANS_DERATE, &
                  TRANS_DERATE_DIST, &
                  FORCED_OUTAGE_DERATE_MW
! 02/23/05
                  SAVE_MAX_TRANS_LINE_INDEX = &
                      MAX(SAVE_MAX_TRANS_LINE_INDEX,TRANS_LINE_INDEX)
!
            IREC = IREC + 1
         ENDDO
         CLOSE(10)
         CLOSE(11)
      ELSE IF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "transobj2:1111")
      ENDIF
!
      SAVE_NUMBER_OF_CONSTRAINTS = MAX(0,IREC-1)
!
      RETURN



! OVERLAY THE CONSTRAINT FILE

      ENTRY CONSTRAINT_MAKEOVL(OVERLAY_FAMILY_NAME)

!
      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME = get_tno_filename(data_drive, overlay_family_name)

      OPEN(10,FILE=FILE_NAME)
      READ(10,*) DELETE
      INUNIT = 12
      IF(TRCON_OL == 'BC') THEN
         OPEN(11,FILE=trim(DATA_DRIVE)// &
                                         "BCTRCON.BIN",ACCESS="DIRECT", &
                                                             RECL=LRECL)
         INUNIT = 11
      ENDIF
      OPEN(12,FILE=trim(DATA_DRIVE)//"OLTRCON.BIN",ACCESS="DIRECT", &
                                            STATUS="UNKNOWN",RECL=LRECL)
      IREC = 0
!
      READ(10,1000,IOSTAT=IOS) RECLN
      DOWHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE
!
         READ(10,1000,IOSTAT=IOS) RECLN
!
      ENDDO
      DO
!
         IREC = IREC + 1
         READ(INUNIT,REC=IREC,IOSTAT=IOS) DELETE, &
                  CONSTRAINT_ID, &
                  CONSTRAINT_ACTIVE, &
                  MW_LIMIT, &
                  SOURCE, &
                  FROM_AREA, &
                  TO_AREA, &
                  SEASON, &
                  PEAK_MULT, &
                  OFF_PEAK_MULT, &
                  FORCED_OUTAGE_RATE, &
                  TRANS_LINE_INDEX, &
                  CapTransMultiplier, &
                  TRANS_DERATE, &
                  TRANS_DERATE_DIST, &
                  FORCED_OUTAGE_DERATE_MW
         IF(IOS /= 0) EXIT
!         READ(10,1000,IOSTAT=IOS) RECLN
         RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
         READ(RECLN,*,ERR=300) DELETE, &
                  CONSTRAINT_ID, & !  8
                  CONSTRAINT_ACTIVE, & !  1
                  MW_LIMIT, & !  4
                  SOURCE, & !  8
                  FROM_AREA(1), & !  2 x 20
                  TO_AREA(1), &
                  FROM_AREA(2), &
                  TO_AREA(2), &
                  FROM_AREA(3), &
                  TO_AREA(3), &
                  FROM_AREA(4), &
                  TO_AREA(4), &
                  FROM_AREA(5), &
                  TO_AREA(5), &
                  FROM_AREA(6), &
                  TO_AREA(6), &
                  FROM_AREA(7), &
                  TO_AREA(7), &
                  FROM_AREA(8), &
                  TO_AREA(8), &
                  FROM_AREA(9), &
                  TO_AREA(9), &
                  FROM_AREA(10), &
                  TO_AREA(10), &
                  SEASON, &
                  PEAK_MULT, &
                  OFF_PEAK_MULT, &
                  FORCED_OUTAGE_RATE, &
                  TRANS_LINE_INDEX, &
                  CapTransMultiplier, &
                  TRANS_DERATE, &
                  TRANS_DERATE_DIST, &
                  FORCED_OUTAGE_DERATE_MW
!
         READ(10,1000,IOSTAT=IOS) RECLN
         DOWHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE
!
            READ(10,1000,IOSTAT=IOS) RECLN
!
         ENDDO
         WRITE(12,REC=IREC) DELETE, &
                  CONSTRAINT_ID, &
                  CONSTRAINT_ACTIVE, &
                  MW_LIMIT, &
                  SOURCE, &
                  FROM_AREA, &
                  TO_AREA, &
                  SEASON, &
                  PEAK_MULT, &
                  OFF_PEAK_MULT, &
                  FORCED_OUTAGE_RATE, &
                  TRANS_LINE_INDEX, &
                  CapTransMultiplier, &
                  TRANS_DERATE, &
                  TRANS_DERATE_DIST, &
                  FORCED_OUTAGE_DERATE_MW
! 02/23/05
         SAVE_MAX_TRANS_LINE_INDEX = &
                      MAX(SAVE_MAX_TRANS_LINE_INDEX,TRANS_LINE_INDEX)
!
!
      ENDDO
      CLOSE(10)
      CLOSE(12)
      IF(TRCON_OL == 'BC') CLOSE(11)
      TRCON_OL = 'OL'
      RETURN

  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from TRANSOBJ2 SIID334'
      call end_program(er_message)

  300 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      CALL MG_LOCATE_WRITE(21,0, &
         'Error reading the Transmission Constraints record. Look for'// &
                                          ' a "," in a character name.', &
                           ALL_VERSIONS,1)
      er_message='stop requested from TRANSOBJ2 SIID335'
      call end_program(er_message)
!

      ENTRY RESET_TRCON_OL

         TRCON_OL = 'BC'
      RETURN
!

      ENTRY OPEN_TRCON_FILE

         OPEN(UNIT_NUM,FILE=trim(OUTPUT_DIRECTORY())//TRCON_OL// &
               "TRCON.BIN",ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      RETURN
!

      ENTRY CLOSE_TRCON_FILE

         CLOSE(UNIT_NUM)
      RETURN

      ENTRY DOES_TRCON_FILE_EXIST(R_CONSTRAINT_FILE_EXISTS)

         R_CONSTRAINT_FILE_EXISTS = CONSTRAINT_FILE_EXISTS
      RETURN

      ENTRY GET_MAX_TRANS_CONSTRAINTS(R_NUMBER_OF_CONSTRAINTS, &
                                                 R_MAX_TRANS_LINE_INDEX)

         R_NUMBER_OF_CONSTRAINTS = SAVE_NUMBER_OF_CONSTRAINTS
         ! 02/23/05
         R_MAX_TRANS_LINE_INDEX = SAVE_MAX_TRANS_LINE_INDEX
!

      RETURN
!
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END
!
!
!
!

!
!
!     MAINTAINS LIST OF GROUP CONSTRAINTS BETWEEN TRANSACTION GROUPS
!
!
      FUNCTION READ_TRANS_CONSTRAINT_DATA()
      use end_routine, only: end_program, er_message
!
!

!
      use spindriftlib
      use prod_arrays_dimensions
      SAVE
      LOGICAL(kind=1) :: SAVE_CONSTRAINT_FILE_EXISTS=.FALSE., &
                  REDUCE_CONSTRAINT_CAPACITY
      INTEGER(kind=2) :: DELETE,I,J,K,CURRENT_RECORD,TRANS_GROUP, &
                  GET_TRANS_CONSTRAINT,TRANS_CONSTRAINT_RECORDS, &
                  R_TRANS_GROUP, &
                  SAVE_TRANS_CONSTRAINT_RECORDS=0,MAX_WHEELS=1, &
                  MAX_CONSTRAINTS, &
                  MAX_TRANS_LINE_INDEX, &
                  ACTIVE_CONSTRAINT_NUMBER=0,SELLER_ID,BUYER_ID, &
                  NUM_TRANS_GROUPS=0,GET_NUMBER_OF_ACTIVE_GROUPS, &
                  MAX_TRANS_GROUP_NUMBER=0,GET_MAX_TRANS_GROUP_NUMBER, &
                  GET_TRANS_GROUP_INDEX,TRANS_ID,TEMP_ID,CURRENT_TIE=1, &
                  LAST_TIE,R_SELLER,R_BUYER,R_HOUR,CURRENT_CONSTRAINT, &
                  R_MONTH, &
                  MAX_CONSTRAINTS_PER_TIE=100, &
                  MAX_CONSTRAINTS_PER_PAIR=75, & !  091604. PREVIOUSLY 12
                  MAX_CONSTRAINT_AREAS=10, &
                  SEASONS_PER_YEAR=2, & !  DATA AVAILABLE FROM BASECASE = 2/YEAR
                  TEMP_CONSTRAINT,TEMP_WHEEL,TEMP_TIES_PER_PAIR, &
                  SAVE_SEASON=0,PUT_SEASON_FOR_CONSTRAINTS, &
                  R_YEAR,R_TIME_OF_DAY

      REAL(kind=4)::SEASONAL_TIE_LIMIT,R_CAPACITY,CURRENT_CONSTRAINT_MW, &
                  GET_HOUR_TIE_LOADING,GET_SEASON_CONSTRAINT_LIMIT, &
                  CONSTRAINT_MULT, &
                  GET_CONSTRAINT_MULT, &
                  GET_TRANS_LINE_MULT, &
                  ESCALATED_MONTHLY_VALUE, &
                  RET_SNG_VALUE_ESCALATION_VECTOR, &
                  GET_VAR, &
                  TempCapTransMultiplier, &
                  GetCapTransMultiplier, &
                  CapTransMultiplier(:,:)


      LOGICAL(kind=1) :: READ_TRANS_CONSTRAINT_DATA, &
                  TRANS_GROUP_ACTIVE_SWITCH,INIT_HOUR_CONSTRAINT_LIMIT, &
                  CONSTRAINT_HAS_ACTIVE_GROUPS, &
                  DETAILED_CONSTRAINTS_ACTIVE=.FALSE., &
                  ARE_DETAILED_CONSTRAINTS_ACTIVE
      LOGICAL(kind=4) :: CONSTRAINT_FILE_EXISTS

      CHARACTER(len=20) :: BUYER_NAME,SELLER_NAME
!
! SIMULATION VARIABLES
!
      CHARACTER(len=8) :: CONSTRAINT_ID(:)*20, &
                  SOURCE
      CHARACTER(len=1) :: CONSTRAINT_ACTIVE, &
                  SEASON(:), &
                  TRANS_DERATE(:), &
                  TRANS_DERATE_DIST(:)
      REAL(kind=4) :: MW_LIMIT(:), &
                  PEAK_MULT(:), &
                  OFF_PEAK_MULT(:), &
                  FORCED_OUTAGE_RATE(:), &
                  FORCED_OUTAGE_DERATE_MW(:), &
                  GET_TRANS_LINE_CONSTRAINT, &
                  GET_TRANS_DERATE, &
                  GET_SCENARIO_TRANS_DERATE, &
                  R_TRANS_LINE_FOR

      INTEGER(kind=2) :: TRANS_LINE_INDEX, &
                  R_TRANS_LINE_INDEX, &
                  TEMP_I2, &
                  TRANS_LINE_POSITION(:)
      INTEGER(kind=2) :: FROM_AREA(:,:),TO_AREA(:,:)
      ALLOCATABLE :: &
                  CONSTRAINT_ID, &
                  SEASON, &
                  MW_LIMIT, &
                  PEAK_MULT, &
                  OFF_PEAK_MULT, &
                  FORCED_OUTAGE_RATE, &
                  FORCED_OUTAGE_DERATE_MW, &
                  TRANS_LINE_POSITION, &
                  FROM_AREA, &
                  TO_AREA
!
      INTEGER(kind=2) :: &
                  CONSTRAINTS_PER_PAIR(:,:), &
       ! 1 = SUMMER, 2 = WINTER, 3 = ALL, 4 = NONE
                  LOCAL_SEASON(:), &
                  CONSTRAINTS_INDEX(:,:,:), &
                  TIES_PER_PAIR(:,:), &
                  TIE_INDEX(:,:,:), &
                  TIE_WHEEL_INDEX(:,:,:), &
                  TRANS_GROUP_POSITION(:)
      REAL(kind=4) :: CONSTRAINT_PERCENT(:), &
                  SEASON_CONSTRAINT_LIMIT(:,:,:), &
                  HOUR_CONSTRAINT_LIMIT(:,:,:), &
                  HOUR_TIE_LOADING(:,:)
      ALLOCATABLE :: &
                  CONSTRAINT_PERCENT, &
                  CapTransMultiplier, &
                  TRANS_DERATE, &
                  TRANS_DERATE_DIST, &
                  CONSTRAINTS_PER_PAIR, &
                  LOCAL_SEASON, &
                  CONSTRAINTS_INDEX, &
                  TIES_PER_PAIR, &
                  TIE_INDEX, &
                  TIE_WHEEL_INDEX, &
                  SEASON_CONSTRAINT_LIMIT, &
                  HOUR_CONSTRAINT_LIMIT, &
                  HOUR_TIE_LOADING, &
                  TRANS_GROUP_POSITION
      REAL :: REAL_ARGUMENT
      INTEGER(kind=2) :: INT2_ARGUMENT


         READ_TRANS_CONSTRAINT_DATA = .FALSE.
         SAVE_CONSTRAINT_FILE_EXISTS = .FALSE.

         CALL DOES_TRCON_FILE_EXIST(CONSTRAINT_FILE_EXISTS)

         IF(.NOT. CONSTRAINT_FILE_EXISTS) RETURN

         CALL GET_MAX_TRANS_CONSTRAINTS(MAX_CONSTRAINTS, &
                                        MAX_TRANS_LINE_INDEX)
!
         CALL OPEN_TRCON_FILE
!
         NUM_TRANS_GROUPS = GET_NUMBER_OF_ACTIVE_GROUPS()
         MAX_TRANS_GROUP_NUMBER = GET_MAX_TRANS_GROUP_NUMBER()
!
         IF(ALLOCATED(TRANS_GROUP_POSITION)) &
                                        DEALLOCATE(TRANS_GROUP_POSITION)
         ALLOCATE(TRANS_GROUP_POSITION(-1:MAX_TRANS_GROUP_NUMBER))
!
           TRANS_GROUP_POSITION = 0
!
         DO J = 1, NUM_TRANS_GROUPS
             
             TRANS_ID = GET_TRANS_GROUP_INDEX(j)
             TRANS_GROUP_POSITION(TRANS_ID) = J
         ENDDO
         TRANS_GROUP_POSITION(-1) = -1
!
         IF(ALLOCATED(CONSTRAINT_ID)) DEALLOCATE( &
                                          CONSTRAINT_ID, &
                                          SEASON, &
                                          MW_LIMIT, &
                                          FROM_AREA, &
                                          TO_AREA, &
                                          PEAK_MULT, &
                                          OFF_PEAK_MULT)
         IF(ALLOCATED(FORCED_OUTAGE_RATE)) DEALLOCATE( &
                                          FORCED_OUTAGE_RATE, &
                                          TRANS_LINE_POSITION, &
                                          FORCED_OUTAGE_DERATE_MW)
         ALLOCATE(CONSTRAINT_ID(MAX_CONSTRAINTS))
         ALLOCATE(SEASON(MAX_CONSTRAINTS))
         ALLOCATE(MW_LIMIT(MAX_CONSTRAINTS))
         ALLOCATE(FROM_AREA(MAX_CONSTRAINTS,MAX_CONSTRAINT_AREAS))
         ALLOCATE(TO_AREA(MAX_CONSTRAINTS,MAX_CONSTRAINT_AREAS))
         ALLOCATE(PEAK_MULT(MAX_CONSTRAINTS))
         ALLOCATE(OFF_PEAK_MULT(MAX_CONSTRAINTS))
         ALLOCATE(FORCED_OUTAGE_RATE(MAX_CONSTRAINTS))
         ALLOCATE(FORCED_OUTAGE_DERATE_MW(MAX_CONSTRAINTS))
         ALLOCATE(TRANS_LINE_POSITION(0:MAX_TRANS_LINE_INDEX))
!
         IF(ALLOCATED(CONSTRAINT_PERCENT)) &
               DEALLOCATE(CONSTRAINT_PERCENT)
         IF(ALLOCATED(CapTransMultiplier)) &
               DEALLOCATE(CapTransMultiplier, &
                  TRANS_DERATE, &
                  TRANS_DERATE_DIST)
         IF(ALLOCATED(CONSTRAINTS_PER_PAIR)) &
                DEALLOCATE(CONSTRAINTS_PER_PAIR)
         IF(ALLOCATED(LOCAL_SEASON)) &
                DEALLOCATE(LOCAL_SEASON)
         IF(ALLOCATED(CONSTRAINTS_INDEX)) &
                DEALLOCATE(CONSTRAINTS_INDEX)
         IF(ALLOCATED(TIES_PER_PAIR)) &
                DEALLOCATE(TIES_PER_PAIR)
         IF(ALLOCATED(TIE_INDEX)) &
                DEALLOCATE(TIE_INDEX)
         IF(ALLOCATED(TIE_WHEEL_INDEX)) &
                DEALLOCATE(TIE_WHEEL_INDEX)
         IF(ALLOCATED(SEASON_CONSTRAINT_LIMIT)) &
                DEALLOCATE(SEASON_CONSTRAINT_LIMIT)
         IF(ALLOCATED(HOUR_CONSTRAINT_LIMIT)) &
                DEALLOCATE(HOUR_CONSTRAINT_LIMIT)
         IF(ALLOCATED(HOUR_TIE_LOADING)) &
                DEALLOCATE(HOUR_TIE_LOADING)
         ALLOCATE(CONSTRAINT_PERCENT(MAX_CONSTRAINTS))
         ALLOCATE(CapTransMultiplier(0:NUM_TRANS_GROUPS, &
                                                    0:NUM_TRANS_GROUPS), &
                  TRANS_DERATE(MAX_CONSTRAINTS), &
                  TRANS_DERATE_DIST(MAX_CONSTRAINTS))
         ALLOCATE(CONSTRAINTS_PER_PAIR( &
                               -1:NUM_TRANS_GROUPS,-1:NUM_TRANS_GROUPS))
         ALLOCATE(LOCAL_SEASON(MAX_CONSTRAINTS))
         ALLOCATE(CONSTRAINTS_INDEX( &
                                -1:NUM_TRANS_GROUPS,-1:NUM_TRANS_GROUPS, &
                                              MAX_CONSTRAINTS_PER_PAIR))
         ALLOCATE(TIES_PER_PAIR(-1:NUM_TRANS_GROUPS, &
                                                   -1:NUM_TRANS_GROUPS))
         ALLOCATE(TIE_INDEX(-1:NUM_TRANS_GROUPS,-1:NUM_TRANS_GROUPS, &
                                               MAX_CONSTRAINTS_PER_TIE))
         ALLOCATE(TIE_WHEEL_INDEX(-1:NUM_TRANS_GROUPS, &
                                             -1:NUM_TRANS_GROUPS, &
                                               MAX_CONSTRAINTS_PER_TIE))
         ALLOCATE(SEASON_CONSTRAINT_LIMIT( &
                              -1:NUM_TRANS_GROUPS, &
                                  -1:NUM_TRANS_GROUPS,SEASONS_PER_YEAR))
         ALLOCATE(HOUR_CONSTRAINT_LIMIT( &
                              -1:NUM_TRANS_GROUPS, &
                                  -1:NUM_TRANS_GROUPS,SEASONS_PER_YEAR))
         ALLOCATE(HOUR_TIE_LOADING(-1:NUM_TRANS_GROUPS, &
                                                   -1:NUM_TRANS_GROUPS))
!
           CONSTRAINTS_PER_PAIR = 0
         LOCAL_SEASON = 0
           CONSTRAINTS_INDEX = 0
           TIES_PER_PAIR = 0
           TIE_INDEX = 0
           TIE_WHEEL_INDEX = 0
           SEASON_CONSTRAINT_LIMIT = 999999.
         CapTransMultiplier = 1
         TRANS_DERATE = 'F'
         TRANS_DERATE_DIST = 'F'
!
         FORCED_OUTAGE_RATE = 0.
         FORCED_OUTAGE_DERATE_MW = 0.0
         TRANS_LINE_POSITION = 0
!

         ACTIVE_CONSTRAINT_NUMBER = 1
         DETAILED_CONSTRAINTS_ACTIVE = .FALSE.
!
         DO CURRENT_RECORD = 1, MAX_CONSTRAINTS
            READ(10,REC=CURRENT_RECORD) DELETE, &
                  CONSTRAINT_ID(ACTIVE_CONSTRAINT_NUMBER), &
                  CONSTRAINT_ACTIVE, &
                  MW_LIMIT(ACTIVE_CONSTRAINT_NUMBER), &
                  SOURCE, &
                  (FROM_AREA(ACTIVE_CONSTRAINT_NUMBER,J), &
                           J = 1,MAX_CONSTRAINT_AREAS), &
                  (TO_AREA(ACTIVE_CONSTRAINT_NUMBER,J), &
                           J = 1,MAX_CONSTRAINT_AREAS), &
                  SEASON(ACTIVE_CONSTRAINT_NUMBER), &
                  PEAK_MULT(ACTIVE_CONSTRAINT_NUMBER), &
                  OFF_PEAK_MULT(ACTIVE_CONSTRAINT_NUMBER), &
                  FORCED_OUTAGE_RATE(ACTIVE_CONSTRAINT_NUMBER), &
                  TRANS_LINE_INDEX, &
                  TempCapTransMultiplier, &
                  TRANS_DERATE(ACTIVE_CONSTRAINT_NUMBER), &
                  TRANS_DERATE_DIST(ACTIVE_CONSTRAINT_NUMBER), &
                  FORCED_OUTAGE_DERATE_MW(ACTIVE_CONSTRAINT_NUMBER)
!
            IF(PEAK_MULT(ACTIVE_CONSTRAINT_NUMBER) /= 1.0 .OR. &
                    OFF_PEAK_MULT(ACTIVE_CONSTRAINT_NUMBER) /= 1.0) THEN
               DETAILED_CONSTRAINTS_ACTIVE = .TRUE.
            ENDIF
!
! ELIMINATE UNNEEDED CONSTRAINTS
!
            IF(CONSTRAINT_ACTIVE == 'F' .OR. &
                    MW_LIMIT(ACTIVE_CONSTRAINT_NUMBER) > 1000000.) CYCLE
!
            TRANS_LINE_POSITION(TRANS_LINE_INDEX) = &
                                                ACTIVE_CONSTRAINT_NUMBER
!
            CONSTRAINT_HAS_ACTIVE_GROUPS = .FALSE.
            DO J = 1, MAX_CONSTRAINT_AREAS

               IF(FROM_AREA(ACTIVE_CONSTRAINT_NUMBER,J) == 0 .AND. &
                          TO_AREA(ACTIVE_CONSTRAINT_NUMBER,J) == 0) EXIT
               SELLER_ID = FROM_AREA(ACTIVE_CONSTRAINT_NUMBER,J)
               BUYER_ID = TO_AREA(ACTIVE_CONSTRAINT_NUMBER,J)
               IF( (.NOT. TRANS_GROUP_ACTIVE_SWITCH(SELLER_ID) &
                           .AND. SELLER_ID /= 0 &
                                             .AND. SELLER_ID /= -1) .OR. &
                   (.NOT. TRANS_GROUP_ACTIVE_SWITCH(BUYER_ID) &
                           .AND. BUYER_ID /= 0 .AND. &
                                                 BUYER_ID /= -1) ) CYCLE
!
! COMPRESS THE ID'S
!
               SELLER_ID = TRANS_GROUP_POSITION(SELLER_ID)
               BUYER_ID = TRANS_GROUP_POSITION(BUYER_ID)
! 070410. DOES THIS IMPLY THAT IT BELONGS IN THE PATHS FILE?
               IF(J == 1) THEN
                  CapTransMultiplier(SELLER_ID,BUYER_ID) = &
                                                  TempCapTransMultiplier
               ENDIF
!
               CONSTRAINTS_PER_PAIR(SELLER_ID,BUYER_ID) = &
                            CONSTRAINTS_PER_PAIR(SELLER_ID,BUYER_ID) + 1
               CONSTRAINTS_INDEX(SELLER_ID,BUYER_ID, &
                           CONSTRAINTS_PER_PAIR(SELLER_ID,BUYER_ID)) = &
                                                ACTIVE_CONSTRAINT_NUMBER
!
               CONSTRAINT_HAS_ACTIVE_GROUPS = .TRUE.
!
! MODIFIED 10/12/98. GAT.
!
               IF(SEASON(ACTIVE_CONSTRAINT_NUMBER) == 'A' .OR. &
                           SEASON(ACTIVE_CONSTRAINT_NUMBER) == ' ') THEN
                  SEASON_CONSTRAINT_LIMIT(SELLER_ID,BUYER_ID,1) = &
                      MIN(SEASON_CONSTRAINT_LIMIT(SELLER_ID,BUYER_ID,1), &
                                     MW_LIMIT(ACTIVE_CONSTRAINT_NUMBER))
                  SEASON_CONSTRAINT_LIMIT(SELLER_ID,BUYER_ID,2) = &
                      MIN(SEASON_CONSTRAINT_LIMIT(SELLER_ID,BUYER_ID,2), &
                                     MW_LIMIT(ACTIVE_CONSTRAINT_NUMBER))
                  LOCAL_SEASON(ACTIVE_CONSTRAINT_NUMBER) = 3
               ELSEIF(SEASON(ACTIVE_CONSTRAINT_NUMBER) == 'S') THEN
                  SEASON_CONSTRAINT_LIMIT(SELLER_ID,BUYER_ID,1) = &
                      MIN(SEASON_CONSTRAINT_LIMIT(SELLER_ID,BUYER_ID,1), &
                                     MW_LIMIT(ACTIVE_CONSTRAINT_NUMBER))
                  LOCAL_SEASON(ACTIVE_CONSTRAINT_NUMBER) = 1
               ELSEIF(SEASON(ACTIVE_CONSTRAINT_NUMBER) == 'W') THEN
                  SEASON_CONSTRAINT_LIMIT(SELLER_ID,BUYER_ID,2) = &
                      MIN(SEASON_CONSTRAINT_LIMIT(SELLER_ID,BUYER_ID,2), &
                                     MW_LIMIT(ACTIVE_CONSTRAINT_NUMBER))
                  LOCAL_SEASON(ACTIVE_CONSTRAINT_NUMBER) = 2
               ELSE
                  WRITE(4,*) "UNDEFINED SEASON IN THE TRANSMISSION"
                  WRITE(4,*) "CONSTRAINTS FILE.  CONSTRAINT NAME = "
                  WRITE(4,*) CONSTRAINT_ID(ACTIVE_CONSTRAINT_NUMBER)
                  WRITE(4,*) '*** line 14891 TRANSOBJ.FOR ***'
                  er_message='See WARNING MESSAGES -TRANSOBJ2.FOR-4'
                  call end_program(er_message)
               ENDIF
!
            ENDDO
            IF(.NOT. CONSTRAINT_HAS_ACTIVE_GROUPS) CYCLE
            ! FOR THE NEXT CONSTRAINT
            ACTIVE_CONSTRAINT_NUMBER = ACTIVE_CONSTRAINT_NUMBER + 1 
!
         ENDDO
         ACTIVE_CONSTRAINT_NUMBER = ACTIVE_CONSTRAINT_NUMBER - 1
!
         CALL CLOSE_TRCON_FILE
!
         SAVE_CONSTRAINT_FILE_EXISTS = .TRUE.
         READ_TRANS_CONSTRAINT_DATA = .TRUE.
!
      RETURN

      ENTRY GetCapTransMultiplier(R_SELLER,R_BUYER,R_YEAR)

         IF (ALLOCATED(CapTransMultiplier)) THEN
            GetCapTransMultiplier = CapTransMultiplier(R_SELLER,R_BUYER)
            IF(GetCapTransMultiplier < -0.0001) THEN
               GetCapTransMultiplier = &
                  GET_VAR(GetCapTransMultiplier,R_YEAR,'Trans Cap Mult')
            ENDIF
         ELSE
            GetCapTransMultiplier = 1.0
         ENDIF
      RETURN

      ENTRY GET_TRANS_DERATE(R_TRANS_LINE_INDEX, &
                             R_CAPACITY, &
                             R_MONTH, &
                             R_YEAR)

         IF (ALLOCATED(TRANS_LINE_POSITION)) THEN
            TEMP_I2 = TRANS_LINE_POSITION(R_TRANS_LINE_INDEX)
            IF(TRANS_DERATE(TEMP_I2) == 'T') THEN
               IF(ABS(FORCED_OUTAGE_DERATE_MW(TEMP_I2)) > 0.000001) THEN
                  IF(FORCED_OUTAGE_DERATE_MW(TEMP_I2) > 0.000001) THEN
                     GET_TRANS_DERATE = FORCED_OUTAGE_DERATE_MW(TEMP_I2)
                  ELSE
                     GET_TRANS_DERATE = &
                           ESCALATED_MONTHLY_VALUE( &
                              ABS(FORCED_OUTAGE_DERATE_MW(TEMP_I2)), &
                              INT(FORCED_OUTAGE_DERATE_MW(TEMP_I2),2), &
                                                R_YEAR,R_MONTH,INT(1,2))
                  ENDIF
               ELSE
                  GET_TRANS_DERATE = 0.0
               ENDIF
               IF(TRANS_DERATE_DIST(TEMP_I2) == 'T') THEN
                  GET_TRANS_DERATE = GET_TRANS_DERATE * &
                               GET_SCENARIO_TRANS_DERATE(R_YEAR,R_MONTH)
               ENDIF
               GET_TRANS_DERATE = MAX(0.0,R_CAPACITY - GET_TRANS_DERATE)
            ELSE
               GET_TRANS_DERATE = 0.0
            ENDIF
         ELSE
            GET_TRANS_DERATE = 0.0
         ENDIF
      RETURN

      ENTRY GET_TRANS_LINE_CONSTRAINT(R_TRANS_LINE_INDEX, &
                                   R_TRANS_LINE_FOR,R_MONTH,R_YEAR)

         IF (ALLOCATED(TRANS_LINE_POSITION)) THEN
            TEMP_I2 = TRANS_LINE_POSITION(R_TRANS_LINE_INDEX)
            IF(TEMP_I2 > 0 .AND. TEMP_I2 <= MAX_CONSTRAINTS) THEN
               GET_TRANS_LINE_CONSTRAINT = MW_LIMIT(TEMP_I2)
               IF(FORCED_OUTAGE_RATE(TEMP_I2) < -0.00001) THEN
                  R_TRANS_LINE_FOR = &
                           ESCALATED_MONTHLY_VALUE( &
                              ABS(FORCED_OUTAGE_RATE(TEMP_I2)), &
                              INT(FORCED_OUTAGE_RATE(TEMP_I2),2), &
                                         R_YEAR,R_MONTH,INT(1,2))/100.
               ELSE
                  R_TRANS_LINE_FOR = FORCED_OUTAGE_RATE(TEMP_I2)/100.
               ENDIF
            ELSE
               GET_TRANS_LINE_CONSTRAINT = 999999.
               R_TRANS_LINE_FOR = 0.
            ENDIF
         ELSE
            GET_TRANS_LINE_CONSTRAINT = 999999.
            R_TRANS_LINE_FOR = 0.
         ENDIF
      RETURN

      ENTRY GET_CONSTRAINT_MULT(R_SELLER,R_BUYER, &
                                          R_TIME_OF_DAY,R_MONTH,R_YEAR)

         GET_CONSTRAINT_MULT = 1.0
! 08/29/03.
         IF(.NOT. ALLOCATED(PEAK_MULT)) RETURN
!
         DO J = 1, CONSTRAINTS_PER_PAIR(R_SELLER,R_BUYER)
            ACTIVE_CONSTRAINT_NUMBER = &
                     CONSTRAINTS_INDEX(R_SELLER,R_BUYER,J)
!
            IF(ACTIVE_CONSTRAINT_NUMBER <= 0) CYCLE
            IF(LOCAL_SEASON(ACTIVE_CONSTRAINT_NUMBER) /= &
                                                 SAVE_SEASON .AND. &
                      LOCAL_SEASON(ACTIVE_CONSTRAINT_NUMBER) /= 3) CYCLE
!
            IF(R_TIME_OF_DAY /= 1) THEN ! 1.0 = PEAK
               GET_CONSTRAINT_MULT = &
                                 OFF_PEAK_MULT(ACTIVE_CONSTRAINT_NUMBER)
            ELSE
               GET_CONSTRAINT_MULT = &
                                     PEAK_MULT(ACTIVE_CONSTRAINT_NUMBER)
            ENDIF
            IF(GET_CONSTRAINT_MULT < 0.0) THEN
               REAL_ARGUMENT = ABS(GET_CONSTRAINT_MULT)
               INT2_ARGUMENT = INT(ABS(GET_CONSTRAINT_MULT),2)
               GET_CONSTRAINT_MULT = &
                    RET_SNG_VALUE_ESCALATION_VECTOR( &
                                                          INT2_ARGUMENT, &
                                                          R_YEAR, &
                                                          R_MONTH)

            ENDIF
            EXIT
         ENDDO
      RETURN

      ENTRY GET_TRANS_LINE_MULT(R_TRANS_LINE_INDEX, &
                                          R_TIME_OF_DAY,R_MONTH,R_YEAR)

         GET_TRANS_LINE_MULT = 1.0
! 08/29/03.
         IF(.NOT. ALLOCATED(PEAK_MULT)) RETURN
!
!         DO J = 1, CONSTRAINTS_PER_PAIR(R_SELLER,R_BUYER)
            ACTIVE_CONSTRAINT_NUMBER = &
                                 TRANS_LINE_POSITION(R_TRANS_LINE_INDEX)
!     +               CONSTRAINTS_INDEX(R_SELLER,R_BUYER,J)
!
            IF(ACTIVE_CONSTRAINT_NUMBER <= 0 .OR. &
                      ACTIVE_CONSTRAINT_NUMBER > MAX_CONSTRAINTS) RETURN
            IF(LOCAL_SEASON(ACTIVE_CONSTRAINT_NUMBER) /= &
                                                 SAVE_SEASON .AND. &
                     LOCAL_SEASON(ACTIVE_CONSTRAINT_NUMBER) /= 3) RETURN
!
            IF(R_TIME_OF_DAY /= 1) THEN ! 1.0 = PEAK
               GET_TRANS_LINE_MULT = &
                                 OFF_PEAK_MULT(ACTIVE_CONSTRAINT_NUMBER)
            ELSE
               GET_TRANS_LINE_MULT = &
                                     PEAK_MULT(ACTIVE_CONSTRAINT_NUMBER)
            ENDIF
            IF(GET_TRANS_LINE_MULT < 0.0) THEN
               REAL_ARGUMENT = ABS(GET_TRANS_LINE_MULT)
               INT2_ARGUMENT = INT(ABS(GET_TRANS_LINE_MULT),2)
               GET_TRANS_LINE_MULT = &
                    RET_SNG_VALUE_ESCALATION_VECTOR( &
                                                          INT2_ARGUMENT, &
                                                          R_YEAR, &
                                                          R_MONTH)

          ENDIF

      RETURN

      ENTRY ARE_DETAILED_CONSTRAINTS_ACTIVE()

         ARE_DETAILED_CONSTRAINTS_ACTIVE = DETAILED_CONSTRAINTS_ACTIVE
      RETURN

      ENTRY PUT_SEASON_FOR_CONSTRAINTS(R_MONTH)

         IF(R_MONTH > 3 .AND. R_MONTH < 10) THEN
            SAVE_SEASON = 1
         ELSE
            SAVE_SEASON = 2
         ENDIF
         PUT_SEASON_FOR_CONSTRAINTS = SAVE_SEASON
      RETURN

      ENTRY GET_SEASON_CONSTRAINT_LIMIT(R_SELLER,R_BUYER)

!         SELLER_ID = TRANS_GROUP_POSITION(R_SELLER)
!         BUYER_ID = TRANS_GROUP_POSITION(R_BUYER)
         IF(ALLOCATED(SEASON_CONSTRAINT_LIMIT)) THEN
            GET_SEASON_CONSTRAINT_LIMIT = &
                   SEASON_CONSTRAINT_LIMIT(R_SELLER,R_BUYER,SAVE_SEASON)
         ELSE
            GET_SEASON_CONSTRAINT_LIMIT = 999999.
         ENDIF
      RETURN

      ! CALLED FROM SIMULATE_MULTI_PARTY EVERY HOUR
      ENTRY INIT_HOUR_CONSTRAINT_LIMIT() 


         IF(.NOT. SAVE_CONSTRAINT_FILE_EXISTS) RETURN


         ! same structure loops not needed
         HOUR_CONSTRAINT_LIMIT = SEASON_CONSTRAINT_LIMIT
           HOUR_TIE_LOADING = 0.
         INIT_HOUR_CONSTRAINT_LIMIT = .TRUE.
      RETURN

      ENTRY GET_HOUR_TIE_LOADING(R_HOUR,R_MONTH,R_SELLER,R_BUYER)

         GET_HOUR_TIE_LOADING = HOUR_TIE_LOADING(R_SELLER,R_BUYER)
      RETURN

      ENTRY REDUCE_CONSTRAINT_CAPACITY(R_HOUR,R_MONTH, &
                                 R_SELLER,R_BUYER,R_CAPACITY)


         IF(.NOT. SAVE_CONSTRAINT_FILE_EXISTS) RETURN

         REDUCE_CONSTRAINT_CAPACITY = .FALSE.
         IF(R_CAPACITY <= &
               HOUR_CONSTRAINT_LIMIT(R_SELLER,R_BUYER,SAVE_SEASON)) THEN
            MAX_CONSTRAINTS = CONSTRAINTS_PER_PAIR(R_SELLER,R_BUYER)
            DO I = 1, MAX_CONSTRAINTS

               CURRENT_CONSTRAINT = &
                                   CONSTRAINTS_INDEX(R_SELLER,R_BUYER,I)

               CURRENT_CONSTRAINT_MW = R_CAPACITY * &
                             CONSTRAINT_PERCENT(CURRENT_CONSTRAINT)/100.

               HOUR_CONSTRAINT_LIMIT(R_SELLER,R_BUYER,SAVE_SEASON) = &
                   HOUR_CONSTRAINT_LIMIT(R_SELLER,R_BUYER,SAVE_SEASON) - &
                                                   CURRENT_CONSTRAINT_MW

               IF(HOUR_CONSTRAINT_LIMIT(R_SELLER,R_BUYER,SAVE_SEASON) &
                                                              < 0.) THEN
                  WRITE(4,*) "NEGATIVE CONSTRAINT ENCOUNTERED"
                  WRITE(4,*) "CONSTRAINT NAME =", &
                                       CONSTRAINT_ID(CURRENT_CONSTRAINT)
                  WRITE(4,*) '*** line 15018 TRANSOBJ.FOR ***'
                  er_message='See WARNING MESSAGES -TRANSOBJ2.FOR-5'
                  call end_program(er_message)
               ENDIF

               J = 1
               DO J = 2, MAX_WHEELS

                  TEMP_TIES_PER_PAIR = &
                                     TIES_PER_PAIR(CURRENT_TIE,LAST_TIE)

! NEED TO CHECK THE NET LINE LOADING (E.G. LAST_TIE,CURRENT_TIE)

                  HOUR_TIE_LOADING(CURRENT_TIE,LAST_TIE) = &
                              HOUR_TIE_LOADING(CURRENT_TIE,LAST_TIE)  + &
                                                   CURRENT_CONSTRAINT_MW
!
                  DO K = 1, TEMP_TIES_PER_PAIR
                     TEMP_CONSTRAINT = TIE_INDEX(CURRENT_TIE,LAST_TIE,K)
                     TEMP_WHEEL = &
                                 TIE_WHEEL_INDEX(CURRENT_TIE,LAST_TIE,K)
                  ENDDO
                  LAST_TIE = CURRENT_TIE
               ENDDO
            ENDDO
            REDUCE_CONSTRAINT_CAPACITY = .TRUE.
         ELSE
            WRITE(4,*) "Hourly transaction capacity limit exceeded"
            WRITE(4,*) "in hour ",R_HOUR
            WRITE(4,*) "and month ",R_MONTH

         ENDIF
      RETURN
      END

      FUNCTION CX_ALTER_MX_CHRONO_PRICES( &
                                    R_TRANS_CAPACITY, &
                                    R_ANNUAL_RESOURCE_USAGE, &
                                    R_ANNUAL_STRIKE_PRICE, &
                                    R_ANNUAL_TRANS_PRICES, &
                                    R_PEAK)

      LOGICAL(kind=1) :: CX_ALTER_MX_CHRONO_PRICES
      INTEGER(kind=2) :: HOUR
      REAL(kind=4) :: R_TRANS_CAPACITY, &
               R_ANNUAL_RESOURCE_USAGE(8784), &
               R_ANNUAL_TRANS_PRICES(8784), &
               R_ANNUAL_STRIKE_PRICE(8784), &
               R_PEAK, &
               NEW_PRICE, &
               IMPACT, &
               PERCENT_CHANGE_QUANTITY, &
               PRICE_DELTA(8784), &
               ELASTICITY_RECIP(8784), &
               MIN_PRICE, &
               MAX_PRICE, &
               MIN_PRICE_DELTA, &
               MAX_PRICE_DELTA, &
               PRICE_DIFF, &
               RESOURCE_ELASTICITY, &
               PRICE_REDUCTION(8760), &
               AVE_PRICE_REDUCTION
! END DATA DECLARATIONS.
         MIN_PRICE = 999999.0
         MAX_PRICE = -999999.0
         MIN_PRICE_DELTA = 999999.0
         MAX_PRICE_DELTA = -999999.0
         AVE_PRICE_REDUCTION = 0.0
         CX_ALTER_MX_CHRONO_PRICES = .FALSE.
! 080616. INCORPORATED ORIGINAL ALTER PRICE ALGORITHM TO GET SIMILAR
!         PRICE RESPONSE FOR SAME RESOURCE CHOSEN.
         IF(R_PEAK > 0.00001) THEN
            DO HOUR = 1, 8760 ! CLEAR UP NUMBER OF HOURS
               PRICE_DIFF =  R_ANNUAL_TRANS_PRICES(HOUR) - &
                                             R_ANNUAL_STRIKE_PRICE(HOUR)
! ASSUMES RESOURCE
               IF(PRICE_DIFF < .00001) then
                CYCLE
               end if

               RESOURCE_ELASTICITY = &
                      MIN(1.,12.*R_ANNUAL_RESOURCE_USAGE(HOUR) / &
                                                         MAX(1.,R_PEAK))
               PRICE_REDUCTION(HOUR) = PRICE_DIFF * RESOURCE_ELASTICITY
               AVE_PRICE_REDUCTION = &
                             AVE_PRICE_REDUCTION + PRICE_REDUCTION(HOUR)
               R_ANNUAL_TRANS_PRICES(HOUR) = &
                     R_ANNUAL_TRANS_PRICES(HOUR) - PRICE_REDUCTION(HOUR)
            ENDDO
            AVE_PRICE_REDUCTION = AVE_PRICE_REDUCTION / 8760.0
         ENDIF

      RETURN
      END

!
      FUNCTION CX_DailyOperAnPS3( &
                                          R_TRANS, &
                                          R_ANNUAL_PRICE, & !  8784
                                          R_ANNUAL_USAGE, & !  8784
                                          R_ANNUAL_ENERGY, &
                                          R_ANNUAL_CHARGE, &
                                          R_ANNUAL_ENERGY_REVENUE, &
                                          R_ANNUAL_VARIABLE_COST, &
                                          R_ANNUAL_HYBRID_MW_PUMP, &
                                          R_ANNUAL_HYBRID_MW_GEN, &
                                          R_ANNUAL_HYBRID_HR_MARGIN, &
                                          R_ANNUAL_HYBRID_PROFIT)
!

      USE ReadEnergyProductsData
      use globecom
      use mod_base_year
      use program_state
      use debugtrace
      integer :: file_trace_cxp3=0
      type TLastCxpValues ! Prevent trace writes when not changed
          real (kind=4) :: LastLocalAnnPrice=0.
          real(kind=4) :: LastAnnualUsage=0.
          integer(kind=2) :: LastTempI2=0
          real(kind=4) :: Last1AER=0.
      end type TLastCxpValues
      type(tLastCxpValues), save :: LastCxpValues
      
      LOGICAL(kind=1) :: TEMP_L1,CX_DailyOperAnPS3,CX_ANNUAL_CONTRACT
      INTEGER(kind=2) :: I,J,K,L,M, &
                R_TRANS,IND,DAYS_IN_EACH_MONTH, &
                MONTH,DAY_IN_MONTH,nDaCMo,iDa, &
                HOUR_END_OF_MONTH,HOUR_IN_YEAR, &
                GET_TRANS_FOR_GRX_ID
      REAL(kind=4) :: ONE_DAY_PRICE(24),MARGIN(144), &
                   HIGH_CAP(12),LOW_CAP(12),HOUR_STORAGE,TEST_CAP, &
                   PATTERN(24),TOTAL_STORAGE(24),STOR_LIM, &
                   PATTERN_MARGIN(24), &
                   LocalDailyDischargeLimit, &
                   DailyPumpingLimitMWH, &
                   NUM_STORAGE_UNITS, &
                   TEMP_DISCHARGE,TEMP_CHARGE_CAP, &
                   C_HOURS, &
                   INDEP_MARKET_HOURS, &
                   INDEP_ANNUAL_STRIKES, &
                   INDEP_ANNUAL_ENERGY, &
                   INDEP_ANNUAL_ENERGY_REVENUE, &
                   INDEP_ANNUAL_VOM_COST, &
                   INDEP_ANNUAL_EMIS_COST, &
                   INDEP_CHRONO_MW_USAGE(1:8784), &
                   INDEP_CHRONO_STRIKE_PRICE(1:8784), &
                   INDEP_EMISSIONS_PER_MWH
      INTEGER(kind=2) :: HIGH_HOUR(12),LOW_HOUR(12),MARGIN_ORDER(144), &
                   TOTAL_COMBO,HIGH_HR_MARGIN(144),LOW_HR_MARGIN(144), &
                   ANNUAL_ORDER(8784),PRICE_ORDER(24),CHARGE(24), &
                   TEMP_I2,TEST_DATE,TEST_DATE2
      REAL(kind=4) :: ESCALATED_MONTHLY_VALUE,TEMP_R1,TEMP_R4,GET_VAR, &
                     R_ANNUAL_PRICE(8784), &
                     LOCAL_ANN_PRICE(8784), &
                     R_ANNUAL_USAGE(8784), &
                     R_ANNUAL_HOURS, &
                     R_ANNUAL_ENERGY, &
                     R_ANNUAL_CHARGE, &
                     R_ANNUAL_ENERGY_REVENUE, &
                     R_ANNUAL_VARIABLE_COST, &
                     R_ANNUAL_HYBRID_MW_PUMP(8784), &
                     R_ANNUAL_HYBRID_MW_GEN(8784), &
                     R_ANNUAL_HYBRID_HR_MARGIN(8784), &
                     R_ANNUAL_HYBRID_PROFIT(8784)
!
! END DATA DECLARATION.
!
!      IF(.NOT. SAVE_MONTHLY_PRODUCTS_STATUS) RETURN
!      IF(LOCAL_ACTIVE_TRANSACTIONS == 0) RETURN
!
!
      if(file_trace_cxp3==0) then
        file_trace_cxp3=open_trace("cx_dailyoperanps3.trace", rq_cxp3)
      end if
      
      LOCAL_ANN_PRICE(1:8784) = R_ANNUAL_PRICE(1:8784) ! 8784 HOURS
      R_ANNUAL_ENERGY = 0.0
      R_ANNUAL_CHARGE = 0.0
      R_ANNUAL_ENERGY_REVENUE = 0.0
      R_ANNUAL_VARIABLE_COST = 0.0
      R_ANNUAL_HYBRID_PROFIT = 0.0
      IND = R_TRANS ! SINGLE STORAGE EVALUATION



! ASSUME TOTAL CAPACITY OF STORAGE. NOTE: THIS APPLIED TO NEW AND 
! EXISTING DYNAMIC STORAGE
! AND EXISTING DYNAMIC STORAGE

      IF(PROPOSED_QUANT_OF_PRODUCT(IND) > 0) THEN
         NUM_STORAGE_UNITS = INT(HOURLY_QUANTITY(IND)/ &
                                       PROPOSED_QUANT_OF_PRODUCT(IND),2)
      ELSE
! PROBLEM.
         NUM_STORAGE_UNITS = 1
      ENDIF
      STORAGE_TRANS_HOURS = 0
      STORAGE_ENERGY = 0.0
      STORAGE_CHARGE = 0.0
      STORAGE_ENERGY_COST = 0.0
      STORAGE_ENERGY_REVENUE = 0.0
! 012422.
      TEST_DATE = 100*(get_BASE_YEAR() + globecom_YEAR - 1900) + 1
      TEST_DATE2 = 100*(get_BASE_YEAR() + globecom_YEAR - 1900) + 12
      IF(TEST_DATE > END_EP(IND) .OR. &
                                      TEST_DATE2 < BEGIN_EP(IND)) RETURN
      MONTH = 0
      DAY_IN_MONTH = 0
      nDaCMo = 0
      IF(PRODUCT_ACTIVE(IND) == 'P') THEN
         TEMP_DISCHARGE = PROPOSED_QUANT_OF_PRODUCT(IND)
      ELSE
         TEMP_DISCHARGE = HOURLY_QUANTITY(IND)
      ENDIF
      TEMP_CHARGE_CAP = PUMPING_CAPACITY(IND)
      IF(TEMP_CHARGE_CAP < -0.0001) THEN
         get_var_called_from="transobj2:17308"
         TEMP_CHARGE_CAP = &
                  GET_VAR(TEMP_CHARGE_CAP,globecom_YEAR,'PumpingCapacity')
      ENDIF
      DailyPumpingLimitMWH = DAILY_PUMPING_MULT(IND) * TEMP_CHARGE_CAP
! 090421
      IF(DailyPumpingLimitMWH < 0.001) RETURN
      TEMP_CHARGE_CAP = NUM_STORAGE_UNITS * TEMP_CHARGE_CAP
         do iDa=1, 365
            IF(iDa > nDaCMo) THEN
               DAY_IN_MONTH = 1
               C_HOURS = 0
               MONTH = MONTH + 1
               TEST_DATE = 100*(get_BASE_YEAR() + globecom_YEAR - 1900) + MONTH
               nDaCMo = nDaCMo + DAYS_IN_EACH_MONTH(MONTH)
!
               IF(ENERGY_PRICE_MULTIPLIER(IND) < 0.) THEN
!
                  TEMP_R1 = 1.0
                  TEMP_I2 = INT(ABS(ENERGY_PRICE_MULTIPLIER(IND)),2)
                  MONTHLY_ENERGY_MULT(IND) = &
                     ESCALATED_MONTHLY_VALUE(TEMP_R1,TEMP_I2,globecom_YEAR, &
                                                       MONTH,INT(1,2))
!
               ELSE
!
                  MONTHLY_ENERGY_MULT(IND) = &
                                          ENERGY_PRICE_MULTIPLIER(IND)
!
               ENDIF
               IF(ENERGY_PRICE(IND) < 0.0) THEN
                  TEMP_R4 = ABS(ENERGY_PRICE(IND))
                  TEMP_I2 = INT(ABS(ENERGY_PRICE(IND)),2)
                  MONTHLY_ENERGY_PRICE(IND) = &
                     ESCALATED_MONTHLY_VALUE(TEMP_R4, &
                                       TEMP_I2, &
                                       globecom_YEAR, &
                                       MONTH,INT(1,2))
               ELSE
                  MONTHLY_ENERGY_PRICE(IND) = ENERGY_PRICE(IND)
               ENDIF
               MONTHLY_ENERGY_PRICE(IND) = &
                               MONTHLY_ENERGY_PRICE(IND) * &
                                          MONTHLY_ENERGY_MULT(IND)
            ELSE
               DAY_IN_MONTH = DAY_IN_MONTH + 1
            ENDIF
            IF(TEST_DATE > END_EP(IND) .OR. &
                                         TEST_DATE < BEGIN_EP(IND)) THEN
               CYCLE
            ENDIF
            IF( (TEST_DATE == BEGIN_EP(IND) .AND. &
                           BEGIN_DAY(IND) > DAY_IN_MONTH) .OR. &
                    (TEST_DATE == END_EP(IND) .AND. &
                             END_DAY(IND) <= DAY_IN_MONTH)) THEN
               CYCLE
            ENDIF
!
! 051017. NEW STORAGE ALGORITHM THAT ALLOWS ANY HOUR TO PUMP OR GEN.
!         ALSO TRACKS HOURLY STORAGE MWH/HR AS A LIMIT.
!         ONE DAY CYCLING FROM 11 PM TO 10 PM
!
!
            HOUR_IN_YEAR=(iDa-1)*24+1
!
! 121319. REDUCE AVAILABLE PUMPING CAPACITY TO MIN OF PUMPING CAPACITY
!         OR THE HYBRID GENERATION
!
!            HIGH_CAP = PROPOSED_QUANT_OF_PRODUCT(IND)
! 011220.
            HOUR_END_OF_MONTH = nDaCMo*24 - 2
!            HOUR_END_OF_MONTH = HOUR_IN_YEAR + nDaCMo*24 - 2
            DO I = 0, 23
               IF(DAY_IN_MONTH == 1 .AND. I < 2) THEN
                  ONE_DAY_PRICE(I+1) = &
                                    LOCAL_ANN_PRICE(HOUR_END_OF_MONTH+I)
               ELSE
                  ONE_DAY_PRICE(I+1) = LOCAL_ANN_PRICE(HOUR_IN_YEAR+I-2)
               ENDIF
!               ONE_DAY_PRICE(I+1) = LOCAL_ANN_PRICE(HOUR_IN_YEAR+I)
               PRICE_ORDER(I+1) = I + 1
            ENDDO
            call INDEXED_SORT_ASCENDING( &
                  ONE_DAY_PRICE,PRICE_ORDER, &
                  I,TEMP_L1) ! I = 24

            DO I = 1, 12
               HIGH_HOUR(I) = PRICE_ORDER(I)
               LOW_HOUR(I) = PRICE_ORDER(25-I)
               CHARGE(HIGH_HOUR(I)) = 0
               CHARGE(LOW_HOUR(I)) = 1
               IF(DAY_IN_MONTH == 1 .AND. LOW_HOUR(I) < 3) THEN
                  LOW_CAP(I) = MIN(TEMP_CHARGE_CAP, &
                    R_ANNUAL_HYBRID_MW_PUMP( &
                                       HOUR_END_OF_MONTH+LOW_HOUR(I)-1))
               ELSE
                  LOW_CAP(I) = MIN(TEMP_CHARGE_CAP, &

                    R_ANNUAL_HYBRID_MW_PUMP(HOUR_IN_YEAR+LOW_HOUR(I)-3))
               ENDIF
               IF(DAY_IN_MONTH == 1 .AND. HIGH_HOUR(I) < 3) THEN
                  HIGH_CAP(I) = MIN(TEMP_DISCHARGE, &

                    R_ANNUAL_HYBRID_MW_GEN( &
                                       HOUR_END_OF_MONTH+LOW_HOUR(I)-1))
               ELSE
                  HIGH_CAP(I) = MIN(TEMP_DISCHARGE, &

                    R_ANNUAL_HYBRID_MW_GEN(HOUR_IN_YEAR+HIGH_HOUR(I)-3))
               ENDIF
            ENDDO
            K = 0
            MARGIN = -99999.0
            MARGIN_ORDER = 0
            DO I = 1, 12
               DO J = 1, 12
                  IF(LOW_HOUR(J) > HIGH_HOUR(I) .OR. &
                        (ONE_DAY_PRICE(LOW_HOUR(J)) + &
                                         MONTHLY_ENERGY_PRICE(IND))/ &
                               PUMPING_STORAGE_EFFICIENCY(IND) > &
                                      ONE_DAY_PRICE(HIGH_HOUR(I))) then
                        CYCLE
                  end if
                  K = K + 1

! 062017.
                  MARGIN(K) = ONE_DAY_PRICE(HIGH_HOUR(I)) - &
                            (ONE_DAY_PRICE(LOW_HOUR(J)) + &
                                              MONTHLY_ENERGY_PRICE(IND))
                  MARGIN_ORDER(K) = K
                  HIGH_HR_MARGIN(K)= I
                  LOW_HR_MARGIN(K)= J
               ENDDO
               TOTAL_COMBO = K
            ENDDO
            call INDEXED_SORT_ASCENDING( &
                  MARGIN,MARGIN_ORDER, &
          ! K IS THE LAST VALID DISCHARGE/CHARGE PAIR
                  TOTAL_COMBO,TEMP_L1)

            HOUR_STORAGE = 0.0

            TOTAL_STORAGE = 0.0
            PATTERN = 0.0
            PATTERN_MARGIN = 0.0
!
! 06-26-2021. REVISED: 07-24-2021 PER DIANE.

            LocalDailyDischargeLimit = &
                     DailyStorageDischargeLimit(IND) * TEMP_DISCHARGE

            DO L = 1, TOTAL_COMBO
               K = MARGIN_ORDER(L)
               I = HIGH_HR_MARGIN(K)
               J = LOW_HR_MARGIN(K)
               IF(HIGH_CAP(I) > 0.0001 .AND. &
                                               LOW_CAP(J) > 0.0001) THEN
                  TEST_CAP = MIN(HIGH_CAP(I), &
                             LOW_CAP(J)*PUMPING_STORAGE_EFFICIENCY(IND), &
                                               LocalDailyDischargeLimit)

! TEST FOR STORAGE CAP
!
                  DO M = LOW_HOUR(J), HIGH_HOUR(I)-1
                     TEST_CAP = MIN(TEST_CAP, &
                         (DailyPumpingLimitMWH + TOTAL_STORAGE(M)) * &
                                       PUMPING_STORAGE_EFFICIENCY(IND) )

                  ENDDO
                  IF(TEST_CAP <= 0.0) then
                    CYCLE
                  end if
                  LocalDailyDischargeLimit = &
                            MAX(0.0,LocalDailyDischargeLimit - TEST_CAP)
                  DO M = LOW_HOUR(J), HIGH_HOUR(I)-1
                     TOTAL_STORAGE(M) = TOTAL_STORAGE(M) - TEST_CAP/ &
                                         PUMPING_STORAGE_EFFICIENCY(IND)
                  ENDDO
!
                  HIGH_CAP(I) = HIGH_CAP(I) - TEST_CAP
                  LOW_CAP(J) = LOW_CAP(J) - TEST_CAP/ &
                                         PUMPING_STORAGE_EFFICIENCY(IND)
                  PATTERN(HIGH_HOUR(I)) = PATTERN(HIGH_HOUR(I)) + &
                                                                TEST_CAP
                  PATTERN(LOW_HOUR(J)) = PATTERN(LOW_HOUR(J)) - &
                                                    TEST_CAP/ &
                                         PUMPING_STORAGE_EFFICIENCY(IND)
                  PATTERN_MARGIN(HIGH_HOUR(I)) = &
                                    PATTERN_MARGIN(HIGH_HOUR(I)) + &
                                                    TEST_CAP * MARGIN(K)
               ENDIF
            ENDDO
            DO I = 0, 23
               IF(DAY_IN_MONTH == 1 .AND. I < 2) THEN
!                  R_ANNUAL_USAGE(HOUR_END_OF_MONTH+I) = PATTERN(I+1)
                  TEMP_I2 = HOUR_END_OF_MONTH+I
               ELSE
!                  R_ANNUAL_USAGE(HOUR_IN_YEAR+I-2) = PATTERN(I+1)
                  TEMP_I2 = HOUR_IN_YEAR+I-2
               ENDIF
               R_ANNUAL_USAGE(TEMP_I2) = PATTERN(I+1)
               IF(PATTERN(I+1) > 0.0) THEN
                  R_ANNUAL_HYBRID_HR_MARGIN(TEMP_I2) = &
                                      PATTERN_MARGIN(I+1) / PATTERN(I+1)
               ENDIF
               call write_trace_int2(file_trace_cxp3, "CHARGE", &
                CHARGE(I+1))

               IF(CHARGE(I+1) == 0) THEN
                  IF(PATTERN(I+1) < 0) THEN
                     TEMP_I2 = TEMP_I2
                  ENDIF
      

          if(LastCxpValues%LastLocalAnnPrice/=LOCAL_ANN_PRICE(TEMP_I2)) then
              LastCxpValues%LastLocalAnnPrice=LOCAL_ANN_PRICE(TEMP_I2)
              call write_trace_real4(file_trace_cxp3, "LOCAL_ANN_PRICE", &
                  LOCAL_ANN_PRICE(TEMP_I2))
              
          end if
          if(LastCxpValues%LastAnnualUsage/=R_ANNUAL_USAGE(TEMP_I2)) then
               LastCxpValues%LastAnnualUsage=R_ANNUAL_USAGE(TEMP_I2)
               call write_trace_real4(file_trace_cxp3, "R_ANNUAL_USAGE", &
                    R_ANNUAL_USAGE(TEMP_I2))
          end if

        if(LastCxpValues%LastTempI2/=Temp_I2) then
             LastCxpValues%LastTempI2=Temp_I2
             call write_trace_int2(file_trace_cxp3, "TEMP_I2", TEMP_I2)
        end if
                  R_ANNUAL_ENERGY = R_ANNUAL_ENERGY + &
                                                 R_ANNUAL_USAGE(TEMP_I2)
                  R_ANNUAL_ENERGY_REVENUE = R_ANNUAL_ENERGY_REVENUE + &
                                              LOCAL_ANN_PRICE(TEMP_I2) * &
                                                 R_ANNUAL_USAGE(TEMP_I2)
        if(LastCxpValues%Last1AER/=R_ANNUAL_ENERGY_REVENUE) then
            LastCxpValues%Last1AER=R_ANNUAL_ENERGY_REVENUE
                  call write_trace_real4(file_trace_cxp3, &
                    "1. ANNUAL_ENERGY_REVENUE", R_ANNUAL_ENERGY_REVENUE)
        end if
                    
                  R_ANNUAL_HYBRID_MW_GEN(TEMP_I2) = &
                                            R_ANNUAL_USAGE(TEMP_I2)
                  R_ANNUAL_HYBRID_PROFIT(TEMP_I2) = &
                                            R_ANNUAL_USAGE(TEMP_I2) * &
                                                LOCAL_ANN_PRICE(TEMP_I2)
!
                  IF(R_ANNUAL_USAGE(TEMP_I2) > 0.0) THEN
                     C_HOURS = C_HOURS + &
                                  R_ANNUAL_USAGE(TEMP_I2)/ &
                                                    TEMP_DISCHARGE
                  ENDIF
                  STORAGE_ENERGY(MONTH) = &
                           STORAGE_ENERGY(MONTH) + &
                                                 R_ANNUAL_USAGE(TEMP_I2)
! 010122. ENERGY COST TO REDUCE REVENUE.
                  STORAGE_ENERGY_REVENUE(MONTH) = &
                           STORAGE_ENERGY_REVENUE(MONTH) + &
                                      (LOCAL_ANN_PRICE(TEMP_I2) - &
                                           MONTHLY_ENERGY_PRICE(IND)) * &
                                                 R_ANNUAL_USAGE(TEMP_I2)
               ELSE
! R_ANNUAL_VARIABLE_COST = HYBRID LOST REVENUES
                  R_ANNUAL_VARIABLE_COST = R_ANNUAL_VARIABLE_COST &
                           - LOCAL_ANN_PRICE(TEMP_I2) * &


                                                 R_ANNUAL_USAGE(TEMP_I2)
                  R_ANNUAL_CHARGE = R_ANNUAL_CHARGE + &
                                                 R_ANNUAL_USAGE(TEMP_I2)
                  STORAGE_CHARGE(MONTH) = STORAGE_CHARGE(MONTH) + &
                                                 R_ANNUAL_USAGE(TEMP_I2)
                  STORAGE_ENERGY_COST(MONTH) = &
                         STORAGE_ENERGY_COST(MONTH) &
                           - LOCAL_ANN_PRICE(TEMP_I2) * &

                                                 R_ANNUAL_USAGE(TEMP_I2)
               ENDIF
               IF(iDa == nDaCMo) THEN
                  ! generation only
                  STORAGE_TRANS_HOURS(MONTH) = NINT(C_HOURS)
               ENDIF

            ENDDO
         ENDDO ! DAY
         CX_DailyOperAnPS3 = .TRUE.
      RETURN
      END




!     ExistingStorageAndHybrid.for
!     Copyright(c)  2000

!     Created: 8/8/2021 12:53:10 PM
!     Author : Greg Turk
!     Last change: msg 8/15/2021 2:28:19 PM


      FUNCTION EXISTING_STORAGE_N_HYBRID( R_TRANS, &
                                          R_ANNUAL_PRICE, & !  8784
                                          R_ANNUAL_USAGE, & !  8784
                                          R_ANNUAL_ENERGY, &
                                          R_ANNUAL_CHARGE, &
                                          R_ANNUAL_ENERGY_REVENUE, &
                                          R_ANNUAL_VARIABLE_COST, &
                                          R_INDEP_CHRONO_MW_USAGE)


      USE ReadEnergyProductsData
      use globecom
      use program_state
      use debugtrace
      
      integer :: file_trace_esh=0 ! existing_storage_n_hybrid routine trace
      LOGICAL(kind=1) :: TEMP_L,EXISTING_STORAGE_N_HYBRID, &
                CX_ANNUAL_CONTRACT, &
                ANNUAL_STORAGE_SORT,CX_DailyOperAnPS3
      INTEGER(kind=2) :: IND,R_TRANS,HYBRID_PROD_POINTER, &
                    R_HYBRID_VALUE
      REAL(kind=4) :: LOCAL_ANN_PRICE(1:8784), &
                     R_ANNUAL_PRICE(1:8784), &
                     R_ANNUAL_USAGE(1:8760), &
                     R_ANNUAL_HOURS, &
                     R_ANNUAL_ENERGY, &
                     R_ANNUAL_CHARGE, &
                     R_ANNUAL_ENERGY_REVENUE, &
                     R_ANNUAL_VARIABLE_COST, &
                     R_ANNUAL_HYBRID_MW_PUMP(1:8784), &
                     R_ANNUAL_HYBRID_MW_GEN(1:8784), &
                     R_ANNUAL_HYBRID_HR_MARGIN(1:8784), &
                     R_ANNUAL_HYBRID_PROFIT(1:8784), &
                     INDEP_MARKET_HOURS, &
                     INDEP_ANNUAL_STRIKES, &
                     INDEP_ANNUAL_ENERGY, &
                     INDEP_ANNUAL_ENERGY_REVENUE, &
                     INDEP_ANNUAL_VOM_COST, &
                     INDEP_ANNUAL_EMIS_COST, &
                     INDEP_CHRONO_MW_USAGE(1:8784), &
                     R_INDEP_CHRONO_MW_USAGE(1:8760), &
                     INDEP_CHRONO_STRIKE_PRICE(1:8784), &
                     INDEP_EMISSIONS_PER_MWH, &
                     LOCAL_HYBRID_MW_DISCHARGE(1:8784), &
                     LOCAL_HYBRID_HR_MARGIN(1:8784), &
                     LOCAL_HYBRID_MW_CHARGE(1:8784), &
                     LOCAL_CHRONO_MW_USAGE(1:8784), &
                     CONSTRAIN_HYBRID_MARGIN(1:8784), &
                     CONSTRAIN_HYBRID_MW_USAGE(1:8784), &

                     LOCAL_HYBRID_PROFIT(1:8784), &
                     HYBRID_MARKET_ENERGY_LIM, &
                     HYBRID_MARKET_ANNUAL_REVENUE, &
                     HYBRID_MARKET_VOM_COST, &
                     HYBRID_MARKET_ANNUAL_ENERGY, &
                     HYBRID_MARKET_ANNUAL_CHARGE, &
                     MARKET_HYBRID_MW_USAGE(1:8784), &
                     HYBRID_MARKET_MW_USAGE(1:8784), &
                     CONSTRAIN_ANNUAL_ENERGY, &
                     CONSTRAIN_ANNUAL_CHARGE, &
                     CONSTRAIN_ANNUAL_REVENUE, &
                     CONSTRAIN_VOM_COST, &
                     ADD_HYBRID_MW_CHARGE(1:8784) ! NOTUSED
! END DATA DECLARATIONS.

      if(file_trace_esh==0) then
        file_trace_esh=open_trace("existing_storage_n_hybrid.trace", &
            rq_esh)
      end if
      if(file_trace_esh/=BAD_TRACE_HANDLE) then
        write(file_trace_esh,*) "args: ", &
        "R_TRANS ", R_TRANS, &
        "R_ANNUAL_PRICE ", R_ANNUAL_PRICE, & 
        "R_ANNUAL_USAGE ", R_ANNUAL_USAGE, & 
        "R_ANNUAL_ENERGY ", R_ANNUAL_ENERGY, &
        "R_ANNUAL_CHARGE ", R_ANNUAL_CHARGE, &
        "1. R_ANNUAL_ENERGY_REVENUE ", R_ANNUAL_ENERGY_REVENUE, &
        "R_ANNUAL_VARIABLE_COST ", R_ANNUAL_VARIABLE_COST, &
        "R_INDEP_CHRONO_MW_USAGE ", R_INDEP_CHRONO_MW_USAGE
      end if
      
      
      IND = R_TRANS
      LOCAL_ANN_PRICE(1:8784) = R_ANNUAL_PRICE(1:8784)
      R_ANNUAL_ENERGY = 0.0
      R_ANNUAL_CHARGE = 0.0
      R_ANNUAL_ENERGY_REVENUE = 0.0
      R_ANNUAL_VARIABLE_COST = 0.0
      R_ANNUAL_HYBRID_PROFIT = 0.0
      R_INDEP_CHRONO_MW_USAGE = 0.0
!
      INDEP_CHRONO_MW_USAGE = 0.0
      INDEP_EMISSIONS_PER_MWH = 0.0
      INDEP_ANNUAL_ENERGY = 0.0
      INDEP_ANNUAL_ENERGY_REVENUE = 0.0

      INDEP_POINTER = HYBRID_BATTERY(IND)
      IF( INDEP_POINTER > 0) THEN

         INDEP_POINTER = HYBRID_INDEX(INDEP_POINTER)
         cxac_called_from="ESNH_001"
         call write_trace_message(file_trace_esh, "calling cxat")
         if(file_trace_esh/=BAD_TRACE_HANDLE) then
            write(file_trace_esh, *) "Args>", &
         "INDEP_POINTER ", INDEP_POINTER, & 
         "LOCAL_ANN_PRICE ", LOCAL_ANN_PRICE, &
         "INDEP_MARKET_HOURS ", INDEP_MARKET_HOURS, &
         "INDEP_ANNUAL_STRIKES ", INDEP_ANNUAL_STRIKES, &
         "INDEP_ANNUAL_ENERGY ", INDEP_ANNUAL_ENERGY, &
         "INDEP_ANNUAL_ENERGY_REVENUE ", INDEP_ANNUAL_ENERGY_REVENUE, &
         "INDEP_ANNUAL_VOM_COST ", INDEP_ANNUAL_VOM_COST, &
         "INDEP_ANNUAL_EMIS_COST ", INDEP_ANNUAL_EMIS_COST, &
         "INDEP_CHRONO_MW_USAGE ", INDEP_CHRONO_MW_USAGE, & 
         "INDEP_CHRONO_STRIKE_PRICE ", INDEP_CHRONO_STRIKE_PRICE, & 
         "INDEP_EMISSIONS_PER_MWH ", INDEP_EMISSIONS_PER_MWH
         end if
         cxac_called_from="ESNH_001"
         TEMP_L  = CX_ANNUAL_CONTRACT(  INDEP_POINTER, &
                                        LOCAL_ANN_PRICE, &
                                        INDEP_MARKET_HOURS, &
                                        INDEP_ANNUAL_STRIKES, &
                                        INDEP_ANNUAL_ENERGY, &
                                        INDEP_ANNUAL_ENERGY_REVENUE, &
                                        INDEP_ANNUAL_VOM_COST, &
                                        INDEP_ANNUAL_EMIS_COST, &
                                        INDEP_CHRONO_MW_USAGE, & ! (1:8784)
                                        INDEP_CHRONO_STRIKE_PRICE, & ! (1:8784)
                                        INDEP_EMISSIONS_PER_MWH)


            LOCAL_CHRONO_MW_USAGE = INDEP_CHRONO_MW_USAGE
            R_INDEP_CHRONO_MW_USAGE(1:8760) = &
                                           INDEP_CHRONO_MW_USAGE(1:8760)

! 012322. CHANGED INDEX FROM IND TO INDEP_POINTER.

            MONTHLY_ENERGY(INDEP_POINTER,1:12) = INDEP_ENR_FOR_EXP(1:12)
            MONTHLY_ENERGY_COST(INDEP_POINTER,1:12) = &
                                                 INDEP_ENERGY_COST(1:12)
! 120121. 010122-BACK-IN.
            MONTHLY_ENERGY_REVENUE(INDEP_POINTER,1:12) = &
                                              INDEP_ENERGY_REVENUE(1:12)

            LOCAL_HYBRID_MW_DISCHARGE = 999999.0
            LOCAL_HYBRID_HR_MARGIN = 0.0
            LOCAL_HYBRID_MW_CHARGE = LOCAL_CHRONO_MW_USAGE
            HYBRID_PROD_POINTER = IND
            CONSTRAIN_VOM_COST = 0.0
            CONSTRAIN_ANNUAL_REVENUE = 0.0
            CONSTRAIN_ANNUAL_ENERGY = 0.0
            CONSTRAIN_ANNUAL_CHARGE = 0.0
            TEMP_L = CX_DailyOperAnPS3( &
                                        HYBRID_PROD_POINTER, & !  TRANS
                                        LOCAL_ANN_PRICE, & !  8784, TG
                                        LOCAL_CHRONO_MW_USAGE, & ! 8784
                                        CONSTRAIN_ANNUAL_ENERGY, &
                                        CONSTRAIN_ANNUAL_CHARGE, &
                                        CONSTRAIN_ANNUAL_REVENUE, &
                                        CONSTRAIN_VOM_COST, &
                                        LOCAL_HYBRID_MW_CHARGE, &
                                        LOCAL_HYBRID_MW_DISCHARGE, &
                                        LOCAL_HYBRID_HR_MARGIN, &
                                        LOCAL_HYBRID_PROFIT)

! CONSTRAINTED. STORAGE_CHARGE IS NEGATIVE.
!
            MONTHLY_ENERGY(IND,1:12) = MONTHLY_ENERGY(IND,1:12) + &
                 STORAGE_ENERGY(1:12) 
            MONTHLY_CHARGE(IND,1:12) = STORAGE_CHARGE(1:12)


! 120121. 010122-BACK-IN.!
            MONTHLY_ENERGY_REVENUE(IND,1:12) = &
                  MONTHLY_ENERGY_REVENUE(IND,1:12) + &
                               STORAGE_ENERGY_REVENUE(1:12) - &
                                               STORAGE_ENERGY_COST(1:12)

            CONSTRAIN_HYBRID_MARGIN = LOCAL_HYBRID_HR_MARGIN
            CONSTRAIN_HYBRID_MW_USAGE = LOCAL_CHRONO_MW_USAGE


! 030720.
            IF(RenewableEnergyPercent(IND) > 0.0001) THEN
               HYBRID_MARKET_ENERGY_LIM = &
                           - CONSTRAIN_ANNUAL_CHARGE * &
                                (1.0/RenewableEnergyPercent(IND) - 1.0)
            ELSE
               HYBRID_MARKET_ENERGY_LIM = 999999.0
            ENDIF
            IF(HYBRID_MARKET_ENERGY_LIM > 0.0001) THEN

! UNCONSTRAINED CHARGE, UNCONSTRAINED DISCHARGE.

               LOCAL_HYBRID_MW_DISCHARGE = 999999.0
               LOCAL_HYBRID_HR_MARGIN = 0.0
               LOCAL_HYBRID_MW_CHARGE = 999999.0
!                       ANNUAL_EMIS_COST = 0.0
               TEMP_L = CX_DailyOperAnPS3( &
                                        HYBRID_PROD_POINTER, & !  TRANS
                                        LOCAL_ANN_PRICE, & !  8784, TG
                                        LOCAL_CHRONO_MW_USAGE, & ! 8784
                                        HYBRID_MARKET_ANNUAL_ENERGY, &
                                        HYBRID_MARKET_ANNUAL_CHARGE, &
                                        HYBRID_MARKET_ANNUAL_REVENUE, &
                                        HYBRID_MARKET_VOM_COST, &
                                        LOCAL_HYBRID_MW_CHARGE, &
                                        LOCAL_HYBRID_MW_DISCHARGE, &
                                        LOCAL_HYBRID_HR_MARGIN, &
                                        LOCAL_HYBRID_PROFIT)
               HYBRID_MARKET_MW_USAGE = LOCAL_CHRONO_MW_USAGE

! FIND MOST PROFITABLE STORAGE DISCHARGE ENERGY GIVEN RENEWABLE 
! ENERGY PERCENT
               LOCAL_HYBRID_HR_MARGIN = 0.0
               ADD_HYBRID_MW_CHARGE = 999999.0

               TEMP_L = ANNUAL_STORAGE_SORT( &
                                        HYBRID_PROD_POINTER, &
                                        HYBRID_MARKET_ENERGY_LIM, &
                                        HYBRID_MARKET_MW_USAGE, &
                                        CONSTRAIN_HYBRID_MW_USAGE, &
                                        CONSTRAIN_HYBRID_MARGIN, &
                                        LOCAL_CHRONO_MW_USAGE)
!
! UNCONSTRAINED CHARGE, CONSTRAINED DISCHARGE. MARKET HYBRID.
!
               LOCAL_HYBRID_MW_DISCHARGE = LOCAL_CHRONO_MW_USAGE
               LOCAL_HYBRID_MW_CHARGE = 999999.0

               TEMP_L = CX_DailyOperAnPS3( &
                                        HYBRID_PROD_POINTER, & !  TRANS
                                        LOCAL_ANN_PRICE, & !  8784, TG
                                        LOCAL_CHRONO_MW_USAGE, & !  8784
                                        HYBRID_MARKET_ANNUAL_ENERGY, &
                                        HYBRID_MARKET_ANNUAL_CHARGE, &
                                        HYBRID_MARKET_ANNUAL_REVENUE, &
                                        HYBRID_MARKET_VOM_COST, &
                                        LOCAL_HYBRID_MW_CHARGE, &
                                        LOCAL_HYBRID_MW_DISCHARGE, &
                                        LOCAL_HYBRID_HR_MARGIN, &
                                        LOCAL_HYBRID_PROFIT)

               MARKET_HYBRID_MW_USAGE = LOCAL_CHRONO_MW_USAGE

               CONSTRAIN_HYBRID_MW_USAGE = &
                                       CONSTRAIN_HYBRID_MW_USAGE + &
                                                  MARKET_HYBRID_MW_USAGE

               MONTHLY_ENERGY(IND,1:12) = MONTHLY_ENERGY(IND,1:12) + &
                             STORAGE_ENERGY(1:12) 
               MONTHLY_CHARGE(IND,1:12) = STORAGE_CHARGE(1:12)
!
               MONTHLY_ENERGY_COST(IND,1:12) = &
                  MONTHLY_ENERGY_COST(IND,1:12) + &
                                               STORAGE_ENERGY_COST(1:12)
! 120121. 010122-BACK-IN.
               MONTHLY_ENERGY_REVENUE(IND,1:12) = &
                           MONTHLY_ENERGY_REVENUE(IND,1:12) + &
                                            STORAGE_ENERGY_REVENUE(1:12)
!
            ELSE
               HYBRID_MARKET_ANNUAL_ENERGY = 0.0
               HYBRID_MARKET_ANNUAL_CHARGE = 0.0
               HYBRID_MARKET_ANNUAL_REVENUE = 0.0
               HYBRID_MARKET_VOM_COST = 0.0
            ENDIF ! HYBRID MARKET ENERGY > 0
!
            MONTHLY_TRANS_HOURS(IND,1:12) = STORAGE_TRANS_HOURS(1:12)
!
! COMBINED HOURLY ENERGY.
! 012422. REMOVED COMBINED HOURLY.
!
            LOCAL_CHRONO_MW_USAGE =  & !  INDEP_CHRONO_MW_USAGE +
                                               CONSTRAIN_HYBRID_MW_USAGE
! 121621.
            R_ANNUAL_USAGE(1:8760) = LOCAL_CHRONO_MW_USAGE(1:8760)
! 011920. PUT BACK INTO DEPENDENT RESOURCE FOR ENERGY EVAL PURPOSES
            R_ANNUAL_ENERGY = INDEP_ANNUAL_ENERGY + &
                               CONSTRAIN_ANNUAL_ENERGY + &
                                    CONSTRAIN_ANNUAL_CHARGE + &
                                         HYBRID_MARKET_ANNUAL_ENERGY

! CONSTRAIN VOM ELIMINATES PORTION OF INDEP ENERGY REVENUE.
            R_ANNUAL_ENERGY_REVENUE = &
                                     INDEP_ANNUAL_ENERGY_REVENUE + &
                                       CONSTRAIN_ANNUAL_REVENUE - &
                                       CONSTRAIN_VOM_COST + &
                                       HYBRID_MARKET_ANNUAL_REVENUE
            R_ANNUAL_VARIABLE_COST = INDEP_ANNUAL_VOM_COST + &
                                                  HYBRID_MARKET_VOM_COST

      ELSE ! STAND-ALONE STORAGE

         LOCAL_CHRONO_MW_USAGE = 0.0 ! 011022.

         LOCAL_HYBRID_MW_DISCHARGE = 999999.0
         LOCAL_HYBRID_HR_MARGIN = 0.0
         LOCAL_HYBRID_MW_CHARGE = 999999.0
         TEMP_L = CX_DailyOperAnPS3( &
                                        IND, & !  TRANS
                                        LOCAL_ANN_PRICE, & !  8784, TG
                                        LOCAL_CHRONO_MW_USAGE, & !  8784
                                        HYBRID_MARKET_ANNUAL_ENERGY, &
                                        HYBRID_MARKET_ANNUAL_CHARGE, &
                                        HYBRID_MARKET_ANNUAL_REVENUE, &
                                        HYBRID_MARKET_VOM_COST, &
                                        LOCAL_HYBRID_MW_CHARGE, &
                                        LOCAL_HYBRID_MW_DISCHARGE, &
                                        LOCAL_HYBRID_HR_MARGIN, &
                                        LOCAL_HYBRID_PROFIT)
!         HYBRID_MARKET_MW_USAGE = LOCAL_CHRONO_MW_USAGE
!
         R_ANNUAL_USAGE(1:8760) = LOCAL_CHRONO_MW_USAGE(1:8760)
         R_ANNUAL_ENERGY = HYBRID_MARKET_ANNUAL_ENERGY
         R_ANNUAL_CHARGE = HYBRID_MARKET_ANNUAL_CHARGE
         R_ANNUAL_ENERGY_REVENUE = HYBRID_MARKET_ANNUAL_REVENUE
         R_ANNUAL_VARIABLE_COST = HYBRID_MARKET_VOM_COST
!
!
         MONTHLY_TRANS_HOURS(IND,1:12) = STORAGE_TRANS_HOURS(1:12)
         MONTHLY_ENERGY(IND,1:12) = STORAGE_ENERGY(1:12)
         MONTHLY_CHARGE(IND,1:12) = STORAGE_CHARGE(1:12)
         MONTHLY_ENERGY_COST(IND,1:12) = STORAGE_ENERGY_COST(1:12)
! 120121. 010122-BACK-IN.
         MONTHLY_ENERGY_REVENUE(IND,1:12) = STORAGE_ENERGY_REVENUE(1:12)
      ENDIF ! INDEP RESOURCE
!
      EXISTING_STORAGE_N_HYBRID = .TRUE.
      RETURN
      END
!
!
