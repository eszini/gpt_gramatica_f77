
!     WVPARTRK.FOR
!     Copyright(c)  2000
!
!     Created: 1/29/2004 8:58:55 AM
!     Author : MARK S GERBER
!     Last change: MSG 1/10/2010 2:53:17 PM



      SUBROUTINE WVPA_RATE_STRUCTURE_OBJECT

!

      USE spindriftlib
      USE prod_arrays_dimensions
      USE SIZECOM

      INTEGER(kind=2) :: NUMBER_OF_BC_CLASSES=0, &
                MAX_BC_CLASS_ID_NUM=0
      INTEGER(kind=2) :: NUMBER_OF_OL_CLASSES=0, &
                MAX_OL_CLASS_ID_NUM=0
      INTEGER(kind=2) :: INUNIT,IREC,DELETE,LRECL=1024, &
                ASSET_CLASS_NUM
      INTEGER(kind=4) :: IOS,IOS_BASE
      CHARACTER(len=5) :: OVERLAY_FAMILY_NAME
      CHARACTER(len=30) :: COMMENT
      CHARACTER(len=256) :: FILE_NAME
      CHARACTER(len=256) :: BASE_FILE_DIRECTORY
      CHARACTER(len=256) :: OUTPUT_DIRECTORY,DATA_DRIVE
      LOGICAL(kind=4) :: FILE_EXISTS,FILE_OPEN
      CHARACTER(len=5) :: BASE_FILE_NAME, &
                  WVPA_RATE_STRUCTURES_FILE
      CHARACTER(len=1) :: USE_TRANSACT_FILE
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER(len=1024) :: RECLN
! DECLARATION FOR CLASS FORECASTS
      INTEGER(kind=2) :: YEAR,DATA_RECORDS_IN_TABLE
      CHARACTER(len=22) :: FILE_TYPE  ! leading space needed
      INTEGER(kind=2) :: BC_TABLE_NUMBER,OL_TABLE_NUMBER
      INTEGER(kind=2) :: R_NUMBER_OF_TABLES
      SAVE BC_TABLE_NUMBER,OL_TABLE_NUMBER
      CHARACTER(len=2) :: CLASS_SALES_OL='BC'
      REAL(kind=4) :: ENERGY_LOSS_FACTOR, &
             DEMAND_LOSS_FACTOR
      CHARACTER(len=1) :: PLACE_HOLDER, & !  25
                  DEMAND_ADJUSTMENT_METHOD ! 39
      CHARACTER(len=32) :: RATE_NAME
      INTEGER(kind=4) :: RATE_CODE
      CHARACTER(len=10) :: RATE_TRACKER, &
                   RATE_OPTION
      CHARACTER(len=10) :: RATE_QUALIFIER
      CHARACTER(len=25) :: TRANS_DIST
      CHARACTER(len=3) :: FUEL_TRACKER_USE, & !  76
                  NON_FUEL_TRACKER_USE ! 77
      REAL(kind=4) :: ENRG_DEMD_TRACKER_SCR_PRICES(60), & !  16 - 75
             NON_FUEL_BASE_RATE(12) ! 78-89
!
!
      CHARACTER(len=1) :: ACCOUNT_ACTIVE
      LOGICAL(kind=1) :: LAHEY_LF95
      CHARACTER(len=30) :: SCREEN_OUTPUT
      INTEGER(kind=2) :: WVPA_SALES_UNIT, &
                NUM_RATE_TABLES
      REAL :: BASE_FUEL_RATE
      CHARACTER(len=3) :: CHANGE_BASE_FUEL_AFTER_MO
!

      ENTRY WVPA_RATE_STRUCTURES_MAKEBIN

!
      BASE_FILE_NAME = WVPA_RATE_STRUCTURES_FILE()
      FILE_NAME = trim(BASE_FILE_DIRECTORY())// &
                                   "WRB"//trim(BASE_FILE_NAME)//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      DATA_DRIVE = OUTPUT_DIRECTORY()
      IF(FILE_EXISTS) THEN
         FILE_TYPE = ' WVPA Rate Structures' ! leading space needed
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_LOCATE_WRITE(16,30,BASE_FILE_NAME,ALL_VERSIONS,0)
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         ENDIF
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCWVRATE.BIN", &
                            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         IREC = 0
         BC_TABLE_NUMBER = 0
!
         READ(10,*,IOSTAT=IOS) DELETE
         DO
            BC_TABLE_NUMBER = BC_TABLE_NUMBER + 1
            BASE_FUEL_RATE = 0.
            CHANGE_BASE_FUEL_AFTER_MO = 'Dec'
            DO
               READ(10,'(A)',IOSTAT=IOS) RECLN
               IF(IOS /= 0) EXIT
               IF(RECLN(1:1) == '7') EXIT
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
                                      ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
                                      ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
                                      ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
!
               READ(RECLN,*) DELETE,YEAR,RATE_NAME, &
                             RATE_CODE, &
                             RATE_TRACKER, &
                             RATE_OPTION, &
                             RATE_QUALIFIER, &
                             TRANS_DIST, &
                             ACCOUNT_ACTIVE, &
                             BASE_FUEL_RATE, & !  9
                             CHANGE_BASE_FUEL_AFTER_MO, & !  10
                             PLACE_HOLDER, & !  11
                             PLACE_HOLDER, & !  12
                             PLACE_HOLDER, & !  13
                             PLACE_HOLDER, & !  14
                             COMMENT,      & !  15
                             ENRG_DEMD_TRACKER_SCR_PRICES, & !  16 - 75
                             FUEL_TRACKER_USE, & !  76
                             NON_FUEL_TRACKER_USE, & !  77
                             NON_FUEL_BASE_RATE ! 78-89
!
! WRITE RECORD
!
               IREC = IREC + 1
               WRITE(11,REC=IREC) DELETE,YEAR,RATE_NAME, &
                                  RATE_CODE, &
                                  RATE_TRACKER, &
                                  RATE_OPTION, &
                                  RATE_QUALIFIER, &
                                  TRANS_DIST, &
                                  ACCOUNT_ACTIVE, &
                                  BASE_FUEL_RATE, & !  9
                                  CHANGE_BASE_FUEL_AFTER_MO, & !  10
                                  COMMENT,      & !  15
                                  ENRG_DEMD_TRACKER_SCR_PRICES, & !  16 - 75
                                  FUEL_TRACKER_USE, & !  76
                                  NON_FUEL_TRACKER_USE, & !  77
                                  NON_FUEL_BASE_RATE ! 78-89
               DATA_RECORDS_IN_TABLE = DATA_RECORDS_IN_TABLE + 1
            ENDDO
            IF(DATA_RECORDS_IN_TABLE < AVAIL_DATA_YEARS) THEN
               DO WHILE (DATA_RECORDS_IN_TABLE < AVAIL_DATA_YEARS)
                  IREC = IREC + 1
                  WRITE(11,REC=IREC) DELETE,YEAR,RATE_NAME, &
                                     RATE_CODE, &
                                     RATE_TRACKER, &
                                     RATE_OPTION, &
                                     RATE_QUALIFIER, &
                                     TRANS_DIST, &
                                     ACCOUNT_ACTIVE, &
                                     BASE_FUEL_RATE, & !  9
                                     CHANGE_BASE_FUEL_AFTER_MO, & !  10
                                     COMMENT,      & !  15
                                     ENRG_DEMD_TRACKER_SCR_PRICES, & !  16 - 75
                                     FUEL_TRACKER_USE, & !  76
                                     NON_FUEL_TRACKER_USE, & !  77
                                     NON_FUEL_BASE_RATE ! 78-89
                  DATA_RECORDS_IN_TABLE = DATA_RECORDS_IN_TABLE + 1
               ENDDO
            ENDIF
            IF(IOS /= 0) EXIT
         ENDDO
         CLOSE(11)
         CLOSE(10)
      ENDIF
      RETURN

      ENTRY OPEN_WVPA_RATE_STRUCTURE_FILE(WVPA_SALES_UNIT, &
                                           NUM_RATE_TABLES)

         OPEN(WVPA_SALES_UNIT,FILE=trim(DATA_DRIVE)//"BCWVRATE.BIN", &
                            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         NUM_RATE_TABLES = BC_TABLE_NUMBER
      RETURN
      END

      FUNCTION WVPA_PROCESS_RATE_STRUCTURES(RATE_YEAR)

!
      USE spindriftlib
      USE prod_arrays_dimensions
      USE SIZECOM
      SAVE
      CHARACTER(len=30) :: COMMENT
      LOGICAL(kind=1) :: WVPA_PROCESS_RATE_STRUCTURES,RETURN_WVPA_RATES, &
                WVPA_UPDATE_FUEL_ADJ_RATES, &
                WVPA_UPDATE_NON_FUEL_ADJ_RATES, &
                WVPA_UPDATE_PSCR_ADJ_RATES
      REAL :: R_POWER_RATE, &
           R_RECONCILLATION_RATE, &
           R_NEW_FUEL_ADJ_RATE, &
           R_NEW_NON_FUEL_ADJ_RATE
      CHARACTER(len=1) :: ACCOUNT_ACTIVE
      INTEGER(kind=2) :: YEAR,DELETE,MO,R_MO
      INTEGER :: IOS
      INTEGER(kind=2) :: RATE_YEAR, &
                WVPA_SALES_UNIT, &
                NUM_RATE_TABLES,I, &
                CHANGE_MO, &
                GET_MONTH_NUMBER
      PARAMETER (WVPA_SALES_UNIT=10)
      CHARACTER(len=32) :: RATE_NAME(:)
      INTEGER(kind=4) :: RATE_CODE(:),IREC,R_RATE_CODE
      CHARACTER(len=10) :: RATE_TRACKER(:), &
                   RATE_OPTION(:)
      CHARACTER(len=10) :: RATE_QUALIFIER(:)
      CHARACTER(len=25) :: TRANS_DIST(:)
      CHARACTER(len=3) :: FUEL_TRACKER_USE(:), & !  76
                  NON_FUEL_TRACKER_USE(:), & !  77
                  CHANGE_BASE_FUEL_AFTER_MO ! 10
      REAL(kind=4) :: ENRG_RATES(:,:), &
             DEMAND_RATES(:,:), &
             FUEL_RATES(:,:), &
             NONFUEL_RATES(:,:), &
             SCR_RATES(:,:), &
             NON_FUEL_BASE_RATE(:,:), & !  78-89
             BASE_FUEL_RATE ! 9
      REAL, ALLOCATABLE :: FUEL_RATE_IN_ENRG_RATE(:,:)
      INTEGER(kind=4) :: RATE_CODE_LIST(:)
      ALLOCATABLE :: ENRG_RATES, &
                     DEMAND_RATES, &
                     FUEL_RATES, &
                     NONFUEL_RATES, &
                     SCR_RATES, &
                     NON_FUEL_BASE_RATE, & !  78-89
                     RATE_CODE_LIST, &
                     RATE_NAME, &
                     RATE_CODE, &
                     RATE_TRACKER, &
                     RATE_OPTION, &
                     RATE_QUALIFIER, &
                     TRANS_DIST, &
                     FUEL_TRACKER_USE, & !  76
                     NON_FUEL_TRACKER_USE ! 77
      REAL(kind=4) :: R_ENRG_RATES(12), &
             R_DEMAND_RATES(12), &
             R_FUEL_RATES(12), &
             R_NONFUEL_RATE(12), &
             R_SCR_RATES(12), &
             R_NON_FUEL_BASE_RATE(12), &
             R_FUEL_RATE_IN_ENRG_RATE(12)
      REAL :: SAVE_NEW_PSCR_ADJ_RATE
      REAL :: R_NEW_PSCR_ADJ_RATE
!
         WVPA_PROCESS_RATE_STRUCTURES = .FALSE.
         IF(RATE_YEAR == 1) SAVE_NEW_PSCR_ADJ_RATE = 0.
         CALL OPEN_WVPA_RATE_STRUCTURE_FILE(WVPA_SALES_UNIT, &
                                            NUM_RATE_TABLES)
         IF(NUM_RATE_TABLES <= 0) RETURN
!
         IF(ALLOCATED(RATE_CODE_LIST)) DEALLOCATE(RATE_CODE_LIST, &
                  ENRG_RATES, &
                  DEMAND_RATES, &
                  FUEL_RATES, &
                  NONFUEL_RATES, &
                  SCR_RATES, &
                  NON_FUEL_BASE_RATE, &
                  RATE_NAME, &
                  RATE_CODE, &
                  RATE_TRACKER, &
                  RATE_OPTION, &
                  RATE_QUALIFIER, &
                  TRANS_DIST, &
                  FUEL_TRACKER_USE, &
                  FUEL_RATE_IN_ENRG_RATE, &
                  NON_FUEL_TRACKER_USE)
         ALLOCATE(RATE_CODE_LIST(0:NUM_RATE_TABLES), &
                  RATE_NAME(0:NUM_RATE_TABLES), &
                  RATE_CODE(0:NUM_RATE_TABLES), &
                  RATE_TRACKER(0:NUM_RATE_TABLES), &
                  RATE_OPTION(0:NUM_RATE_TABLES), &
                  RATE_QUALIFIER(0:NUM_RATE_TABLES), &
                  TRANS_DIST(0:NUM_RATE_TABLES), &
                  FUEL_TRACKER_USE(0:NUM_RATE_TABLES), &
                  NON_FUEL_TRACKER_USE(0:NUM_RATE_TABLES), &
                  ENRG_RATES(12,0:NUM_RATE_TABLES), &
                  DEMAND_RATES(12,0:NUM_RATE_TABLES), &
                  FUEL_RATE_IN_ENRG_RATE(12,0:NUM_RATE_TABLES), &
                  FUEL_RATES(12,0:NUM_RATE_TABLES), &
                  NONFUEL_RATES(12,0:NUM_RATE_TABLES), &
                  SCR_RATES(12,0:NUM_RATE_TABLES), &
                  NON_FUEL_BASE_RATE(12,0:NUM_RATE_TABLES))
!
         FUEL_RATE_IN_ENRG_RATE = 0.
         WVPA_PROCESS_RATE_STRUCTURES = .TRUE.
         RATE_CODE_LIST = -99
         IREC = RATE_YEAR - AVAIL_DATA_YEARS
         DO I = 1, NUM_RATE_TABLES
            IREC = IREC + AVAIL_DATA_YEARS
            READ(WVPA_SALES_UNIT,REC=IREC,IOSTAT=IOS) DELETE,YEAR, &
                                           RATE_NAME(I), &
                                           RATE_CODE(I), &
                                           RATE_TRACKER(I), &
                                           RATE_OPTION(I), &
                                           RATE_QUALIFIER(I), &
                                           TRANS_DIST(I), &
                                           ACCOUNT_ACTIVE, &
                                           BASE_FUEL_RATE, & !  9
                                           CHANGE_BASE_FUEL_AFTER_MO, & !  10
                                           COMMENT,      & !  15
                                          (ENRG_RATES(MO,I),MO=1,12), &
                                          (DEMAND_RATES(MO,I),MO=1,12), &
                                          (FUEL_RATES(MO,I),MO=1,12), &
                                          (NONFUEL_RATES(MO,I),MO=1,12), &
                                          (SCR_RATES(MO,I),MO=1,12), &
                                           FUEL_TRACKER_USE(I), & !  76
                                           NON_FUEL_TRACKER_USE(I), & !  77
                                          (NON_FUEL_BASE_RATE(MO,I), &
                                                                MO=1,12)
            IF(IOS /= 0) EXIT
            IF(DELETE >= 8 .OR. ACCOUNT_ACTIVE == 'N') CYCLE
            RATE_CODE_LIST(I) = RATE_CODE(I)
!
            FUEL_RATE_IN_ENRG_RATE(:,I) = BASE_FUEL_RATE
            IF(INDEX(CHANGE_BASE_FUEL_AFTER_MO,'Dec') == 0 .AND. &
                                 RATE_YEAR + 1 <= AVAIL_DATA_YEARS) THEN
               CHANGE_MO = GET_MONTH_NUMBER(CHANGE_BASE_FUEL_AFTER_MO)+1
               READ(WVPA_SALES_UNIT,REC=IREC+1,IOSTAT=IOS) DELETE,YEAR, &
                                           RATE_NAME(I), &
                                           RATE_CODE(I), &
                                           RATE_TRACKER(I), &
                                           RATE_OPTION(I), &
                                           RATE_QUALIFIER(I), &
                                           TRANS_DIST(I), &
                                           ACCOUNT_ACTIVE, &
                                           BASE_FUEL_RATE ! 9

               FUEL_RATE_IN_ENRG_RATE(CHANGE_MO:12,I) = BASE_FUEL_RATE
           ENDIF
         ENDDO
         CLOSE(WVPA_SALES_UNIT)
      RETURN

      ENTRY RETURN_WVPA_RATES(R_RATE_CODE, &
                              R_ENRG_RATES, &
                              R_DEMAND_RATES, &
                              R_FUEL_RATES, &
                              R_NONFUEL_RATE, &
                              R_SCR_RATES, &
                              R_NON_FUEL_BASE_RATE, &
                              R_FUEL_RATE_IN_ENRG_RATE)

!
         RETURN_WVPA_RATES = .FALSE.
         DO I = 1, NUM_RATE_TABLES
            IF(RATE_CODE_LIST(I) == R_RATE_CODE) THEN
               R_ENRG_RATES(:) = ENRG_RATES(:,I)
               R_DEMAND_RATES(:) = DEMAND_RATES(:,I)
               R_NONFUEL_RATE(:) = NONFUEL_RATES(:,I)
               R_SCR_RATES(:) = SCR_RATES(:,I)
               R_NON_FUEL_BASE_RATE(:) = NON_FUEL_BASE_RATE(:,I)
               R_FUEL_RATE_IN_ENRG_RATE(:)=FUEL_RATE_IN_ENRG_RATE(:,I)
               RETURN_WVPA_RATES = .TRUE.
                  R_FUEL_RATES(:) = FUEL_RATES(:,I)
               EXIT
            ENDIF
         ENDDO
      RETURN

      ENTRY WVPA_UPDATE_FUEL_ADJ_RATES(R_MO,R_NEW_FUEL_ADJ_RATE)

!
         DO I = 1, NUM_RATE_TABLES

            IF(RATE_CODE(I) == 142021) CYCLE
            IF(INDEX(FUEL_TRACKER_USE(I),'Cal') /= 0) THEN
               CHANGE_MO = 0
            ELSE
               CHANGE_MO = GET_MONTH_NUMBER(FUEL_TRACKER_USE(I))
            ENDIF
            DO MO = R_MO, R_MO+2
               IF(MO > CHANGE_MO) THEN
                  FUEL_RATES(MO,I) = R_NEW_FUEL_ADJ_RATE &
                                     - FUEL_RATE_IN_ENRG_RATE(MO,I)
               ENDIF
            ENDDO
         ENDDO
         WVPA_UPDATE_FUEL_ADJ_RATES = .TRUE.
      RETURN

      ENTRY WVPA_UPDATE_NON_FUEL_ADJ_RATES(R_MO,R_NEW_NON_FUEL_ADJ_RATE)

!
         DO I = 1, NUM_RATE_TABLES
            IF(RATE_CODE(I) == 142021 .OR. &
              (RATE_CODE(I) > 160000 .AND. RATE_CODE(I) < 170000)) CYCLE

            IF(INDEX(NON_FUEL_TRACKER_USE(I),'Cal') /= 0) THEN
               CHANGE_MO = 0
            ELSE
               CHANGE_MO = GET_MONTH_NUMBER(NON_FUEL_TRACKER_USE(I))
            ENDIF
            DO MO = R_MO, R_MO+2
               IF(MO > CHANGE_MO) THEN
                  NONFUEL_RATES(MO,I) = R_NEW_NON_FUEL_ADJ_RATE &
                                        - NON_FUEL_BASE_RATE(MO,I)
               ENDIF
            ENDDO
         ENDDO
         WVPA_UPDATE_NON_FUEL_ADJ_RATES = .TRUE.
      RETURN

      ENTRY WVPA_UPDATE_PSCR_ADJ_RATES(R_NEW_PSCR_ADJ_RATE)


         WVPA_UPDATE_PSCR_ADJ_RATES = .TRUE.
         return
         DO I = 1, NUM_RATE_TABLES
            IF(RATE_CODE(I) < 200000 .OR. RATE_CODE(I) >= 300000) CYCLE
            IF(INDEX(FUEL_TRACKER_USE(I),'Cal') /= 0) THEN
               FUEL_RATES(:,I) = R_NEW_PSCR_ADJ_RATE
            ELSE
               CHANGE_MO = GET_MONTH_NUMBER(FUEL_TRACKER_USE(I))
               IF(CHANGE_MO <12) &
                        FUEL_RATES(CHANGE_MO+1:,I) = R_NEW_PSCR_ADJ_RATE
            ENDIF
         ENDDO
      RETURN
      END

      SUBROUTINE WVPA_COOP_SALES_FORECAST

!
      USE spindriftlib
      USE prod_arrays_dimensions
      USE SIZECOM
      INTEGER(kind=2) :: NUMBER_OF_BC_CLASSES=0, &
                MAX_BC_CLASS_ID_NUM=0
      INTEGER(kind=2) :: NUMBER_OF_OL_CLASSES=0, &
                MAX_OL_CLASS_ID_NUM=0
      INTEGER(kind=2) :: INUNIT,IREC,DELETE,LRECL=1024, &
                ASSET_CLASS_NUM
      INTEGER(kind=4) :: IOS,IOS_BASE
      CHARACTER(len=1) :: DEMAND_PRICING_SWITCH
      CHARACTER(len=5) :: OVERLAY_FAMILY_NAME
      CHARACTER(len=30) :: COMMENT
      CHARACTER(len=4) :: ENERGY_ACTUAL_MONTH,  & !  76
                  CAPACITY_ACTUAL_MONTH ! 77
      CHARACTER(len=256) :: FILE_NAME
      CHARACTER(len=256) :: BASE_FILE_DIRECTORY
      CHARACTER(len=256) :: OUTPUT_DIRECTORY,DATA_DRIVE
      LOGICAL(kind=4) :: FILE_EXISTS,FILE_OPEN
      CHARACTER(len=5) :: BASE_FILE_NAME,WVPA_COOP_REV_FORECAST_FILE, &
                  WVPA_RATE_STRUCTURES_FILE
      CHARACTER(len=1) :: USE_TRANSACT_FILE
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER(len=1024) :: RECLN
! DECLARATION FOR CLASS FORECASTS
      INTEGER(kind=2) :: YEAR,DATA_RECORDS_IN_TABLE, &
                   ASSET_CLASS_ALLOCATION_VECTOR
      CHARACTER(len=32) :: CLASS_NAME,ENERGY_ADJUSTMENT_METHOD*1, &
                      RATE_NAME
      CHARACTER(len=22) :: FILE_TYPE  ! leading space needed
      INTEGER(kind=2) :: SAVE_SALES_UNIT=0, &
                SALES_UNIT
      INTEGER(kind=2) :: BC_TABLE_NUMBER=0,OL_TABLE_NUMBER=0
      INTEGER(kind=2) :: R_NUMBER_OF_TABLES
      CHARACTER(len=2) :: CLASS_SALES_OL='BC'
      CHARACTER(len=1) :: CASH_TREATMENT
      CHARACTER(len=30) :: LAG_PERIOD, &
                   CASH_RECEIVABLE_PAYABLE
      REAL(kind=4) :: ENERGY_LOSS_FACTOR, &
             DEMAND_LOSS_FACTOR
      CHARACTER(len=1) :: PLACE_HOLDER, & !  25
                  DEMAND_ADJUSTMENT_METHOD ! 39
      REAL(kind=4) :: ENERGY_ADJUSTMENTS(12), & !  27-38
             DEMAND_ADJUSTMENTS(12), & !  40-51
             NUMBER_OF_CUSTOMERS(12), & ! 52-63
             CUSTOMER_REVENUE_RATE(12) ! 64-75
      INTEGER(kind=4) :: RATE_CODE
      CHARACTER(len=10) :: RATE_TRACKER, &
                   RATE_OPTION
      CHARACTER(len=10) :: RATE_QUALIFIER
      CHARACTER(len=25) :: TRANS_DIST
!
!
      CHARACTER(len=40) :: REVENUE_CLASSIFICATION
      INTEGER(kind=2) :: TRANSACT_FORECAST_GROUP, &
                TRANSACT_CUSTOMER_GROUP
      INTEGER(kind=2) :: BC_ASSET_CLASS_POINTER(:), &
                OL_ASSET_CLASS_POINTER(:), &
                TEMP_ASSET_CLASS_POINTER(:)
      ALLOCATABLE :: BC_ASSET_CLASS_POINTER, &
                     OL_ASSET_CLASS_POINTER, &
                     TEMP_ASSET_CLASS_POINTER
      SAVE BC_ASSET_CLASS_POINTER,OL_ASSET_CLASS_POINTER
      INTEGER(kind=2) :: R_NUM_OF_CLASSES,R_MAX_CLASS_NUM
      INTEGER(kind=2) :: nPOSITIONS, &
                WVPA_COMPANY_ID
      INTEGER(kind=2), DIMENSION(nPOSITIONS) :: R_CLASS_POINTERS
!      INTEGER(kind=2, POINTER, DIMENSION(:) :: ValueArray
      CHARACTER(len=1) :: ACCOUNT_ACTIVE
      LOGICAL(kind=1) :: LAHEY_LF95
      CHARACTER(len=30) :: SCREEN_OUTPUT
!

!
!          ROUTINE TO CONVERT METAFILE FILES TO DIRECT-ACESS BINARY
!          COPYRIGHT (C) 1983, 84, 85  M.S. GERBER & ASSOCIATES, INC.
!

!
! CONVERT THE CLASS-FORECAST-DATA FILES

      ENTRY WVPA_SALES_MAKEBIN

!
      BASE_FILE_NAME = WVPA_COOP_REV_FORECAST_FILE()
      FILE_NAME = trim(BASE_FILE_DIRECTORY())// &
                                   "RCB"//trim(BASE_FILE_NAME)//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      DATA_DRIVE = OUTPUT_DIRECTORY()
      IF(FILE_EXISTS) THEN
         FILE_TYPE = ' WVPA Revenue Forecast' ! leading space needed
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_LOCATE_WRITE(16,30,BASE_FILE_NAME,ALL_VERSIONS,0)
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         ENDIF
         ALLOCATE(TEMP_ASSET_CLASS_POINTER(1024))
         TEMP_ASSET_CLASS_POINTER = 0
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCWVSALE.BIN", &
                            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         IREC = 0
         BC_TABLE_NUMBER = 0
!
         READ(10,*,IOSTAT=IOS) DELETE
         DO
            DO
               READ(10,'(A)',IOSTAT=IOS) RECLN
               IF(IOS /= 0) EXIT
               IF(RECLN(1:1) == '7') EXIT
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
                                    ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
                                    ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
                                    ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*) DELETE,YEAR,CLASS_NAME,RATE_CODE, &
                RATE_TRACKER,RATE_OPTION,ACCOUNT_ACTIVE, &
                ASSET_CLASS_NUM,ASSET_CLASS_ALLOCATION_VECTOR, &
                REVENUE_CLASSIFICATION,CASH_TREATMENT,LAG_PERIOD, &
                CASH_RECEIVABLE_PAYABLE,COMMENT,USE_TRANSACT_FILE, &
                TRANSACT_FORECAST_GROUP, TRANSACT_CUSTOMER_GROUP, & !  15,16
                ENERGY_LOSS_FACTOR,DEMAND_LOSS_FACTOR, &
                DEMAND_PRICING_SWITCH, WVPA_COMPANY_ID, & !  19,20
                PLACE_HOLDER, PLACE_HOLDER, & !  21,22
                PLACE_HOLDER, PLACE_HOLDER, & !  23,24
                PLACE_HOLDER, & !  25
                ENERGY_ADJUSTMENT_METHOD, & !  26
                ENERGY_ADJUSTMENTS, & !  27-38
                DEMAND_ADJUSTMENT_METHOD, & !  39
                DEMAND_ADJUSTMENTS, & !  40-51
                NUMBER_OF_CUSTOMERS, & ! 52-63
                CUSTOMER_REVENUE_RATE, & !  64-75
                ENERGY_ACTUAL_MONTH,  & !  76
                CAPACITY_ACTUAL_MONTH ! 77
!


               IF(.NOT. (DELETE >= 8 .OR. ACCOUNT_ACTIVE == 'N')) THEN
                  CALL SET_ASSET_CLASSES(ASSET_CLASS_NUM, &
                                         NUMBER_OF_BC_CLASSES, &
                                          MAX_BC_CLASS_ID_NUM, &
                                         TEMP_ASSET_CLASS_POINTER)
               ENDIF
!
! WRITE RECORD
!
               IREC = IREC + 1
               WRITE(11,REC=IREC) DELETE,YEAR,CLASS_NAME,RATE_CODE, &
                RATE_TRACKER,RATE_OPTION,ACCOUNT_ACTIVE, &
                ASSET_CLASS_NUM,ASSET_CLASS_ALLOCATION_VECTOR, &
                REVENUE_CLASSIFICATION,CASH_TREATMENT, LAG_PERIOD, &
                CASH_RECEIVABLE_PAYABLE,COMMENT,USE_TRANSACT_FILE, &
                TRANSACT_FORECAST_GROUP, TRANSACT_CUSTOMER_GROUP, & !  15,16
                ENERGY_LOSS_FACTOR,DEMAND_LOSS_FACTOR, &
                DEMAND_PRICING_SWITCH, & !  19
                ENERGY_ADJUSTMENT_METHOD, & !  26
                ENERGY_ADJUSTMENTS, & !  27-38
                DEMAND_ADJUSTMENT_METHOD, & !  39
                DEMAND_ADJUSTMENTS, & !  40-51
                NUMBER_OF_CUSTOMERS, & ! 52-63
                CUSTOMER_REVENUE_RATE, & !  64-75
                ENERGY_ACTUAL_MONTH,  & !  76
                CAPACITY_ACTUAL_MONTH ! 77
               DATA_RECORDS_IN_TABLE = DATA_RECORDS_IN_TABLE + 1
            ENDDO
            IF(DATA_RECORDS_IN_TABLE < AVAIL_DATA_YEARS) THEN
               DO WHILE (DATA_RECORDS_IN_TABLE < AVAIL_DATA_YEARS)
                  IREC = IREC + 1
                  WRITE(11,REC=IREC) DELETE,YEAR,CLASS_NAME,RATE_CODE, &
                   RATE_TRACKER,RATE_OPTION,ACCOUNT_ACTIVE, &
                   ASSET_CLASS_NUM,ASSET_CLASS_ALLOCATION_VECTOR, &
                   REVENUE_CLASSIFICATION,CASH_TREATMENT,LAG_PERIOD, &
                   CASH_RECEIVABLE_PAYABLE,COMMENT,USE_TRANSACT_FILE, &
                   TRANSACT_FORECAST_GROUP,TRANSACT_CUSTOMER_GROUP, & !  15,16
                   ENERGY_LOSS_FACTOR,DEMAND_LOSS_FACTOR, &
                   DEMAND_PRICING_SWITCH,ENERGY_ADJUSTMENT_METHOD, & !  19,26
                   ENERGY_ADJUSTMENTS, & !  27-38
                   DEMAND_ADJUSTMENT_METHOD, & !  39
                   DEMAND_ADJUSTMENTS, & !  40-51
                   NUMBER_OF_CUSTOMERS, & ! 52-63
                   CUSTOMER_REVENUE_RATE, & !  64-75
                   ENERGY_ACTUAL_MONTH,  & !  76
                   CAPACITY_ACTUAL_MONTH ! 77
                  DATA_RECORDS_IN_TABLE = DATA_RECORDS_IN_TABLE + 1
               ENDDO
            ENDIF
            IF(IOS /= 0) EXIT
         ENDDO
         BC_TABLE_NUMBER = IREC/30
         CLOSE(11)
         CLOSE(10)
         IF(MAX_BC_CLASS_ID_NUM > 0) THEN
            ALLOCATE(BC_ASSET_CLASS_POINTER(MAX_BC_CLASS_ID_NUM))
            BC_ASSET_CLASS_POINTER = TEMP_ASSET_CLASS_POINTER
         ENDIF
         DEALLOCATE(TEMP_ASSET_CLASS_POINTER)
      ENDIF
      RETURN

      ENTRY RETURN_WVPA_COOP_TABLE_NUM(R_NUMBER_OF_TABLES)

         IF(CLASS_SALES_OL == 'OL') THEN
            R_NUMBER_OF_TABLES = OL_TABLE_NUMBER
         ELSE
            R_NUMBER_OF_TABLES = BC_TABLE_NUMBER
         ENDIF
      RETURN

      ENTRY RETURN_NUM_WVPA_COOPS(R_NUM_OF_CLASSES, &
                                  R_MAX_CLASS_NUM)

         IF(CLASS_SALES_OL == 'OL') THEN
            R_NUM_OF_CLASSES = NUMBER_OF_OL_CLASSES
            R_MAX_CLASS_NUM = MAX_OL_CLASS_ID_NUM
         ELSE
            R_NUM_OF_CLASSES = NUMBER_OF_BC_CLASSES
            R_MAX_CLASS_NUM = MAX_BC_CLASS_ID_NUM
         ENDIF
      RETURN

      ENTRY RETURN_WVPA_COOP_POINTERS(R_CLASS_POINTERS,nPOSITIONS)

         IF(CLASS_SALES_OL == 'OL') THEN
!            R_CLASS_POINTERS(:) = OL_ASSET_CLASS_POINTER(:)
         ELSE
            R_CLASS_POINTERS = BC_ASSET_CLASS_POINTER
         ENDIF
      RETURN

      ENTRY OPEN_WVPA_COOP_SALES_FILE(SALES_UNIT)

!
         IF(CLASS_SALES_OL == 'OL') THEN
            OPEN(SALES_UNIT,FILE=trim(DATA_DRIVE)//"OLWVSALE.BIN", &
                            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         ELSE
            OPEN(SALES_UNIT,FILE=trim(DATA_DRIVE)//"BCWVSALE.BIN", &
                            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         ENDIF
         SAVE_SALES_UNIT = SALES_UNIT
      RETURN

      ENTRY CLOSE_WVPA_COOP_SALES_FILE()

         CLOSE(SAVE_SALES_UNIT,IOSTAT=IOS)
      RETURN
      END

      FUNCTION WVPA_RATES_TRACKER_OBJECT()
      USE IREC_ENDPOINT_CONTROL
      use grx_planning_routines
      use mod_base_year
      implicit none

      LOGICAL(kind=1) :: WVPA_RATES_TRACKER_OBJECT, &
                WVPA_STORE_TRACKER_INFO,WVPA_RATE_TRACKER_REPORT, &
                WVPA_INIT_TRACKER_BALANCES, &
                WVPA_STORE_TRACKER_SALES_INFO, &
                WVPA_UPDATE_FUEL_ADJ_RATES, &
                VOID_LOGICAL, &
                WVPA_SET_FUEL_TRACKER, &
                WVPA_UPDATE_NON_FUEL_ADJ_RATES, &
                WVPA_UPDATE_PSCR_ADJ_RATES, &
                WVPA_INIT_TRACKER_DATABASE, &
                WVPA_TRACKER_DATABASE_INFO, &
                WVPA_TRACKER_DATABASE_FROM_DD, &
                WVPA_MNTHLY_MEMBER_ACCRUED_REVS, &
                WVPA_MEMBER_ACCRUED_REVS
      REAL WVPA_QUARTERLY_SYSTEM_RATES, &
           WVPA_IN_FUEL_ADJUSTMENT_CAL, &
           WVPA_IN_FUEL_TRACKER_CALC, &
           WVPA_NON_FUEL_TRACKER_CALC, &
           WVPA_NON_FUEL_ADJUSTMENT_CAL, &
           WVPA_PSCR_ADJUSTMENT_CAL, &
           WVPA_PSCR_ADJUSTMENT_CAL_MO, &
           WVPA_PSCR_TRACKER_CALUCLATION
      INTEGER(kind=2) :: GET_MONTH_NUMBER
      REAL, SAVE :: QRT_RECONCILIATION_ADJ(0:12), &
                    QRT_NONFUEL_RECONCILIATION_ADJ(0:12), &
                    QRT_PSCR_RECONCILIATION_ADJ(0:12), &
                    PSCR_PRIOR_YEAR_RECONC_AMOUNT(0:12)
      REAL :: QRT_FUEL_COLLECTION_AMOUNTS, &
        QRT_PSRC_COLLECTION_AMOUNTS, &
           IN_QRT_SALES,NEW_FUEL_ADJ_RATE,IL_BASE_FUEL_RATE, &
           IN_BASE_FUEL_RATE,MI_BASE_FUEL_RATE,IL_FUEL_ADJ_RATE, & !  28
           MI_FUEL_ADJ_RATE,SEVEN_MNTH_PR_QRT_FUEL_RUN_SUM, &
           SEVEN_MONTH_PRIOR_QRT_FUEL_SUM, &
           SVN_MNTH_PR_QRT_NON_FL_RUN_SUM, &
           SEVEN_MNTH_PR_QRT_NON_FUEL_SUM, &
           MEMBER_SALES_FL_QRT_RUNNING_SUM,MEMBER_SALES_FUEL_QRT_SUM, &
           QRT_MOVING_COLLECTION_RATE, &
           QRT_MOVING_NONFUEL_COLL_RATE, &
           QRT_NON_FUEL_COLLECTION_AMOUNTS, &
           NEW_NON_FUEL_ADJ_RATE, &
           MBR_SALES_NONFL_QRT_RUNNING_SUM, &
           MEMBER_SALES_NONFUEL_QRT_SUM, &
           NON_FUEL_TRACKER_SALES_IN, &
           NEW_PSCR_ADJ_RATE,QRT_PSCR_COLLECTION_AMOUNTS, &
           PSCR_COLLECTION_AMOUNTS(0:12), &
           PSCR_OVER_UNDER_COLLECTION, &
           PSCR_ANUAL_ACTUAL_FUEL_COST
      REAL(kind=4), DIMENSION(0:12) :: R_MONTHLY_ENERGY
      REAL(kind=4) :: R_MONTHLY_BASE_FUEL_DOLLARS, &
             R_MONTHLY_FUEL_ADJ_DOLLARS, &
             R_MONTHLY_NONFUEL_DOLLARS, &
             R_MONTHLY_BASE_NONFUEL_DOLLARS, &
             R_MONTHLY_PSCR_DOLLARS
      CHARACTER*(*) R_RATE_TRACKER
      INTEGER R_RATE_CODE
      REAL(kind=4), SAVE, DIMENSION(0:12) :: FUEL_TRACKER_SALES, &
                                       NONFUEL_TRACKER_SALES
      REAL(kind=4), SAVE, DIMENSION(0:12) :: IN_SALES, &
                                       IN_FUEL_ADJ_DOLLARS, &
                                       IN_NONFUEL_ADJ_DOLLARS, &
                                       IN_BASE_NONFUEL_DOLLARS, &
                                       IN_BASE_FUEL_DOLLARS, &
                                       BASE_FUEL_RATE, &
                                       BASE_NONFUEL_RATE
      REAL(kind=4), SAVE, DIMENSION(0:12) :: IL_SALES, &
                                       IL_FUEL_ADJ_DOLLARS, &
                                       IL_NONFUEL_ADJ_DOLLARS, &
                                       IL_BASE_FUEL_DOLLARS, &
                                       IL_BASE_NONFUEL_DOLLARS
      REAL(kind=4), SAVE, DIMENSION(0:12) :: MI_SALES, &
                                       MI_FUEL_ADJ_DOLLARS, &
                                       MI_BASE_FUEL_DOLLARS, &
                                       MI_NONFUEL_ADJ_DOLLARS, &
                                       MI_BASE_NONFUEL_DOLLARS
      REAL(kind=4), SAVE, DIMENSION(0:12) :: NON_MEMBER_SALES, &
                                       NON_MEMBER_FUEL_SALES, &
                                       NON_MEMBER_BASE_FUEL_DOLLARS, &
                                       NON_MEMBER_FUEL_ADJ_DOLLARS, &
                                       NON_MEMBER_NONFUEL_ADJ_DOLLARS, &
                                       NON_MEMBER_BASE_NONFUEL_DOLLARS
      REAL(kind=4), SAVE, DIMENSION(0:12) :: TOTAL_MEMBER_SALES, &
                                       TOTAL_MEMBER_FUEL_ADJ_DOLLARS, &
                                       TOTAL_MEMBER_BASE_FUEL_DOLLARS, &
                                       TOTAL_MEMBER_NONFL_ADJ_DOLLARS, &
                                       TOTAL_MEMBER_BASE_NONFL_DOLLARS
      LOGICAL(kind=1) :: WVPA_RATE_TRACKER_PRT_OPEN = .FALSE.
      INTEGER, SAVE :: WVPA_TRACKER_REC
      INTEGER(kind=2), SAVE :: WVPA_TRACKER_UNIT
      INTEGER(kind=2) :: WVPA_RATE_TRACKER_HEADR,R_RUN_YEAR
      CHARACTER(len=6) :: SHORT_MONTH_NAMES
      REAL :: IN_FUEL_ADJ_RATE,THE_RATIO_OF_A_TO_B, &
           IN_ACTUAL_FUEL_COST(0:12), &
           IL_ACTUAL_FUEL_COST(0:12), &
           MI_ACTUAL_FUEL_COST(0:12), &
           IN_ACTUAL_NON_FUEL_COST(0:12), &
           IL_ACTUAL_NON_FUEL_COST(0:12), &
           MI_ACTUAL_NON_FUEL_COST(0:12), &
           PSCR_ACTUAL_FUEL_COST(0:12)
      INTEGER(kind=2) :: MO,MONTH,endpoint,CURRENT_YEAR,DELETE,R_MO, &
                         START_QRT
      CHARACTER(len=256) :: FILE_NAME
      CHARACTER(len=256) :: BASE_FILE_DIRECTORY
      CHARACTER(len=256) :: OUTPUT_DIRECTORY
      LOGICAL(kind=4) :: FILE_EXISTS,FILE_OPEN
      CHARACTER(len=5) :: BASE_FILE_NAME,WVPA_ADJUSTMENT_TRACKER_FILE
      REAL, SAVE :: STARTING_BASE_FUEL_RATE, &
                    STARTING_NON_FUEL_RATE, &
                    FUEL_COLLECTION_AMOUNTS(0:12), &
                    NON_FUEL_COLLECTION_AMOUNTS(0:12), &
                    BEG_UNCOLLECTED_PSCR_BALANCE, &             ! 40
                    BEG_UNCOLLECTED_FUEL_BALANCE, &
                    BEG_UNCOLLECTED_NONFL_BALANCE, &
                    RPT_FUEL_COLLECTION_AMOUNTS(0:12), &
                    RPT_NON_FUEL_COLLECTION_AMOUNTS(0:12)
      REAL, SAVE :: INCOME_FUEL_STATEMENT_EFFECT(0:12),  & !  10
           INCOME_NON_FL_STATEMENT_EFFECT(0:12),  & !  10
           INCOME_PSCR_STATEMENT_EFFECT(0:12), &
           CLOSING_UNCOLLECTED_FL_BALANCE,  & !  11
           CLOS_UNCOLLECTED_NONFL_BALANCE,  & !  11
           CLOS_UNCOLLECTED_PSCR_BALANCE, &
           UNCOLLECTED_FUEL_BALANCE, &
           UNCOLLECTED_NON_FUEL_BALANCE, &
           UNCOLLECTED_PSCR_BALANCE
!      CHARACTER(len=3, SAVE ::
!     +            FIRST_SCR_RATE_CHANGE_MONTH,
!     +            SECOND_SCR_RATE_CHANGE_MONTH
      CHARACTER(len=3) :: MICHIGAN_CASH_PAYMENT_MONTH_STR
      INTEGER(kind=2), SAVE :: MICHIGAN_CASH_PAYMENT_MONTH
!      INTEGER(kind=2, SAVE ::
!     +          FIRST_SCR_RATE_CHANGE_YEAR,
!     +          SECOND_SCR_RATE_CHANGE_YEAR
      REAL, SAVE :: WVPA_PURCHASES_FUEL_COST(0:12,0:5), &
           WVPA_GENERATION_FUEL_COST(0:12,0:5), &
           WVPA_SERVICES_FUEL_COST(0:12,0:5), &
           WVPA_TOTAL_FUEL_COST(0:12), &
           WVPA_DEMAND_FUEL_COST(0:12,0:5), &
           WVPA_DERIVATIVE_FUEL_COST(0:12,0:5), &
           WVPA_ACTUAL_FUEL_COST(0:12,0:5), &
           WVPA_ACTUAL_NON_FUEL_COST(0:12,0:5), &
           TEMP_FUEL_COLLECTION_AMOUNTS(12), &
           TEMP_NON_FUEL_COLL_AMOUNTS(12), &
           QUARTERLY_RUNNING_TOTAL_FL_COST(0:12), & !  18
           QUARTERLY_RUNNING_SALES_MWH(0:12), & !  19
           AVERAGE_QUARTERLY_FUEL_RATE(0:12), &
           AVERAGE_QUARTERLY_NON_FUEL_RATE(0:12), &
           QRTRLY_RUNNING_TOTAL_NONFL_COST(0:12), &
           QUARTERLY_FLAT_POWER_RATE(0:12), &
           QUARTERLY_POWER_RATE_ADJ, &
           WVPA_TOTAL_WHOLESALE_COST(0:12), &
           WVPA_WHOLESALE_FUEL_COST(0:12), &
           WVPA_WHOLESALE_NONFUEL_COST(0:12)
      REAL, SAVE :: &
           WVPA_WHOLESALE_REVENUE(0:12), &
           QUARTERLY_NONFUEL_RATE_ADJ, &
           WVPA_TOTAL_PSCR_COST(0:12), &
           QRTRLY_RUNNING_TOTAL_PSCR_COST(0:12), &
           AVERAGE_QUARTERLY_PSCR_RATE(0:12), &
           QUARTERLY_FLAT_PSCR_RATE(0:12), &
           WVPA_PSCR_AJD_2_FUEL_COST(0:12)
      REAL, SAVE :: WVPA_PURCHASES_NON_FUEL_COST(0:12,0:5), &
           WVPA_GENERATION_NON_FUEL_COST(0:12,0:5), &
           WVPA_SERVICES_NON_FUEL_COST(0:12,0:5), &
           WVPA_DERIVATIVE_NON_FUEL_COST(0:12,0:5), &
           WVPA_TOTAL_NON_FUEL_COST(0:12), &
           WVPA_DEMAND_NON_FUEL_COST(0:12,0:5), &
           QUARTERLY_FLAT_NON_FUEL_RATE(0:12), &
           WVPA_OTHER_NON_FUEL_COST(0:12), &
           WVPA_OTHER_PSCR_COST(0:12),                  & !  108
           WVPA_OTHER_FUEL_COST(0:12),                  & !  109
           WVPA_TOTAL_MI_FUEL_COST(0:12), &
           WVPA_TOTAL_MI_NON_FUEL_COST(0:12)
      REAL IN_BASE_NON_FUEL_RATE, &
           IN_FUEL_NON_ADJ_RATE, &
           IL_BASE_NON_FUEL_RATE, &
           IL_FUEL_NON_ADJ_RATE, &
           MI_BASE_NON_FUEL_RATE, &
           MI_FUEL_NON_ADJ_RATE, &
           PSCR_BASE_FUEL_RATE, &
           PSCR_BASE_FUEL_ADJ_RATE, &
           PSCR_RECONCILATION_AMOUNT
!
      CHARACTER(len=10) :: DUMMY,COMMENT
      INTEGER(kind=2) :: I
      INTEGER(kind=2), PARAMETER :: Purchases=1, &
                              Generation=2, &
                              Derivative=3, &
                              Services=4, &
                              FuelCost=1, &
                              NonFuelCost=2, &
                              DemandCost=3, &
                              PSCROnly=1, &
                              BothIndianaPSCR=2, &
                              SDI=3, &
                              IndianaOnly=4
!
      INTEGER(kind=2) :: WVPA_TRACKING_TYPE,TRACKING_TYPE,END_YR, &
                R_FINANCIAL_SIMULATION_YEARS
      CHARACTER*(*) R_WVPA_TRACKING_TYPE, DESC
      REAL, SAVE, ALLOCATABLE :: WVPA_TRACKED_EXPENSES(:,:,:), &
                                 WVPA_TRACKED_DEFERRED_DEBITS(:,:,:)
      REAL, INTENT(IN) :: R_MIDAS_MONTHLY_EXPENSES_BOOKED(12,5)
      CHARACTER(len=1) :: R_DEFERRAL_TYPE
      REAL R_DEFERRED_DEBITS_AMORT(0:12,0:R_FINANCIAL_SIMULATION_YEARS)
      INTEGER (KIND=2) :: BASE_DATE,HISTORICAL_PRODUCTION_DATE
      REAL (KIND=4) :: R_ACCRUED_REVENUES, &
                       R_MONTHLY_ACCRUED_REVENUES(0:12)
!
         IN_SALES = 0.
         NONFUEL_TRACKER_SALES = 0.
         FUEL_TRACKER_SALES = 0.
         IN_FUEL_ADJ_DOLLARS = 0.
         IN_NONFUEL_ADJ_DOLLARS = 0.
         IN_BASE_NONFUEL_DOLLARS = 0.
         IN_BASE_FUEL_DOLLARS = 0.
         IL_SALES = 0.
         IL_FUEL_ADJ_DOLLARS = 0.
         IL_NONFUEL_ADJ_DOLLARS = 0.
         IL_BASE_NONFUEL_DOLLARS = 0.
         IL_BASE_FUEL_DOLLARS = 0.
         MI_SALES = 0.
         MI_FUEL_ADJ_DOLLARS = 0.
         MI_NONFUEL_ADJ_DOLLARS = 0.
         MI_BASE_NONFUEL_DOLLARS = 0.
         MI_BASE_FUEL_DOLLARS = 0.
         NON_MEMBER_SALES = 0.
         NON_MEMBER_FUEL_ADJ_DOLLARS = 0.
         NON_MEMBER_NONFUEL_ADJ_DOLLARS = 0.
         NON_MEMBER_BASE_NONFUEL_DOLLARS = 0.
         NON_MEMBER_BASE_FUEL_DOLLARS = 0.
         TOTAL_MEMBER_SALES = 0.

! OPEN REPORT HEADERS

         IF(.NOT. WVPA_RATE_TRACKER_PRT_OPEN) THEN
            WVPA_TRACKER_UNIT=WVPA_RATE_TRACKER_HEADR(WVPA_TRACKER_REC)
            WVPA_RATE_TRACKER_PRT_OPEN = .TRUE.
         ENDIF
         WVPA_RATES_TRACKER_OBJECT = .TRUE.
      RETURN

      ENTRY WVPA_SET_FUEL_TRACKER()

         IN_FUEL_ADJ_DOLLARS = 0.
         IN_NONFUEL_ADJ_DOLLARS = 0.
         IN_BASE_NONFUEL_DOLLARS = 0.
         IN_BASE_FUEL_DOLLARS = 0.
         IL_FUEL_ADJ_DOLLARS = 0.
         IL_NONFUEL_ADJ_DOLLARS = 0.
         IL_BASE_NONFUEL_DOLLARS = 0.
         IL_BASE_FUEL_DOLLARS = 0.
         MI_FUEL_ADJ_DOLLARS = 0.
         MI_NONFUEL_ADJ_DOLLARS = 0.
         MI_BASE_NONFUEL_DOLLARS = 0.
         MI_BASE_FUEL_DOLLARS = 0.
         NON_MEMBER_FUEL_ADJ_DOLLARS = 0.
         NON_MEMBER_NONFUEL_ADJ_DOLLARS = 0.
         NON_MEMBER_BASE_NONFUEL_DOLLARS = 0.
         NON_MEMBER_BASE_FUEL_DOLLARS = 0.
         CALL WVPA_INIT_TRACKER_DATE_BASE()
         WVPA_SET_FUEL_TRACKER = .TRUE.
      RETURN

      ENTRY WVPA_INIT_TRACKER_BALANCES()

         BASE_FILE_NAME = WVPA_ADJUSTMENT_TRACKER_FILE()
         FILE_NAME = trim(BASE_FILE_DIRECTORY())// &
                                   "RTB"//trim(BASE_FILE_NAME)//".DAT"
         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
         IF(FILE_EXISTS) THEN
            OPEN(10,FILE=FILE_NAME)
            READ(10,*) DELETE
            READ(10,*) DELETE, &
                       BEG_UNCOLLECTED_FUEL_BALANCE, &
                       BEG_UNCOLLECTED_NONFL_BALANCE, &
                       (DUMMY, I=3,4), &
                       COMMENT, &
                       TEMP_FUEL_COLLECTION_AMOUNTS, &
                       TEMP_NON_FUEL_COLL_AMOUNTS, &
                       BEG_UNCOLLECTED_PSCR_BALANCE, & !  40
                       MICHIGAN_CASH_PAYMENT_MONTH_STR
            CLOSE(10)
            FUEL_COLLECTION_AMOUNTS(1:7) = &
                                      TEMP_FUEL_COLLECTION_AMOUNTS(6:12)
            NON_FUEL_COLLECTION_AMOUNTS(1:7) = &
                                  TEMP_NON_FUEL_COLL_AMOUNTS(6:12)
            MICHIGAN_CASH_PAYMENT_MONTH = &
                       GET_MONTH_NUMBER(MICHIGAN_CASH_PAYMENT_MONTH_STR)
         ELSE
         ENDIF
         WVPA_INIT_TRACKER_BALANCES = .TRUE.
      RETURN

      ENTRY WVPA_INIT_TRACKER_DATABASE

         IF(ALLOCATED(WVPA_TRACKED_EXPENSES)) &
                                DEALLOCATE(WVPA_TRACKED_EXPENSES, &
                                           WVPA_TRACKED_DEFERRED_DEBITS)
         ALLOCATE(WVPA_TRACKED_EXPENSES(0:12,30,0:4), &
                  WVPA_TRACKED_DEFERRED_DEBITS(0:12,30,0:4))
         WVPA_TRACKED_EXPENSES = 0.
         WVPA_TRACKED_DEFERRED_DEBITS = 0.
         WVPA_INIT_TRACKER_DATABASE = .TRUE.
      RETURN

      ENTRY WVPA_TRACKER_DATABASE_INFO(DESC, &
                                       R_WVPA_TRACKING_TYPE, &
                                       R_FINANCIAL_SIMULATION_YEARS, &
                                       R_MIDAS_MONTHLY_EXPENSES_BOOKED)

!
         TRACKING_TYPE = WVPA_TRACKING_TYPE(R_WVPA_TRACKING_TYPE)
         IF(TRACKING_TYPE == 5) RETURN
         END_YR = MIN(INT(5,2),R_FINANCIAL_SIMULATION_YEARS)
         WVPA_TRACKED_EXPENSES(1:,:,TRACKING_TYPE) = &
                      WVPA_TRACKED_EXPENSES(1:12,1:END_YR,TRACKING_TYPE) &
                      + R_MIDAS_MONTHLY_EXPENSES_BOOKED(1:12,1:END_YR)
         WVPA_TRACKER_DATABASE_INFO = .TRUE.
      RETURN

      ENTRY WVPA_TRACKER_DATABASE_FROM_DD(DESC, &
                                          R_DEFERRAL_TYPE, &
                                          R_WVPA_TRACKING_TYPE, &
                                          R_FINANCIAL_SIMULATION_YEARS, &
                                          R_DEFERRED_DEBITS_AMORT)

!
         TRACKING_TYPE = WVPA_TRACKING_TYPE(R_WVPA_TRACKING_TYPE)
         IF(TRACKING_TYPE == 5) RETURN
         END_YR = MAX(INT(1,2),R_FINANCIAL_SIMULATION_YEARS)
         IF(R_DEFERRAL_TYPE == 'C') THEN
            WVPA_TRACKED_DEFERRED_DEBITS(1:12,1:END_YR,TRACKING_TYPE) = &
               WVPA_TRACKED_DEFERRED_DEBITS(1:12,1:END_YR,TRACKING_TYPE) &
               - R_DEFERRED_DEBITS_AMORT(1:12,1:END_YR)
         ELSE
            WVPA_TRACKED_DEFERRED_DEBITS(1:12,1:END_YR,TRACKING_TYPE) = &
               WVPA_TRACKED_DEFERRED_DEBITS(1:12,1:END_YR,TRACKING_TYPE) &
                      + R_DEFERRED_DEBITS_AMORT(1:12,1:END_YR)
         ENDIF
         WVPA_TRACKER_DATABASE_FROM_DD = .TRUE.
      RETURN


      ENTRY WVPA_STORE_TRACKER_SALES_INFO(R_RATE_TRACKER, &
                                          R_RATE_CODE, &
                                          R_MONTHLY_ENERGY)

         IF(INDEX(R_RATE_TRACKER,'IN') /= 0) THEN
            IN_SALES = IN_SALES + R_MONTHLY_ENERGY
            TOTAL_MEMBER_SALES = TOTAL_MEMBER_SALES + R_MONTHLY_ENERGY
            FUEL_TRACKER_SALES = FUEL_TRACKER_SALES + R_MONTHLY_ENERGY
            IF(R_RATE_CODE < 160000) THEN
               NONFUEL_TRACKER_SALES = NONFUEL_TRACKER_SALES &
                                      + R_MONTHLY_ENERGY
            ENDIF
         ELSEIF(INDEX(R_RATE_TRACKER,'IL') /= 0)THEN
            IL_SALES = IL_SALES + R_MONTHLY_ENERGY
            TOTAL_MEMBER_SALES = TOTAL_MEMBER_SALES + R_MONTHLY_ENERGY
            FUEL_TRACKER_SALES = FUEL_TRACKER_SALES + R_MONTHLY_ENERGY
            NONFUEL_TRACKER_SALES = NONFUEL_TRACKER_SALES &
                                    + R_MONTHLY_ENERGY
         ELSEIF(INDEX(R_RATE_TRACKER,'MI') /= 0) THEN
            MI_SALES = MI_SALES + R_MONTHLY_ENERGY
            TOTAL_MEMBER_SALES = TOTAL_MEMBER_SALES + R_MONTHLY_ENERGY
            FUEL_TRACKER_SALES = FUEL_TRACKER_SALES + R_MONTHLY_ENERGY
            NONFUEL_TRACKER_SALES = NONFUEL_TRACKER_SALES &
                                    + R_MONTHLY_ENERGY
         ELSE
            NON_MEMBER_SALES = NON_MEMBER_SALES + R_MONTHLY_ENERGY
            IF(R_RATE_CODE == 901021) THEN
               NONFUEL_TRACKER_SALES = NONFUEL_TRACKER_SALES &
                                       + R_MONTHLY_ENERGY
               FUEL_TRACKER_SALES = FUEL_TRACKER_SALES &
                                    + R_MONTHLY_ENERGY
               NON_MEMBER_FUEL_SALES = NON_MEMBER_FUEL_SALES &
                                       + R_MONTHLY_ENERGY
            ENDIF
         ENDIF
      RETURN

      ENTRY WVPA_QUARTERLY_SYSTEM_RATES(R_RUN_YEAR)

!
!  GET_MM_DATABASE RETURNS THE
!   0:5  Type: Purchase, Generation, Derivative, Services, Actuals
!    0:3  Cost: Fuel, Non-Fuel, Demand
!   0:4 Tracker PSCROnly=1, BothIndianaPSCR=2, SDI=3, IndianaOnly=4
!
         BASE_DATE = (get_base_year() + R_RUN_YEAR - 1900) * 100
         WVPA_PURCHASES_FUEL_COST = 0.
         WVPA_GENERATION_FUEL_COST = 0.
         WVPA_SERVICES_FUEL_COST = 0.
         WVPA_DEMAND_FUEL_COST = 0.
         WVPA_DERIVATIVE_FUEL_COST = 0.
         WVPA_ACTUAL_FUEL_COST = 0.

! The DemandCost were removed 1/28/04 based on report from Accumen

         CALL GET_MM_DATABASE(Purchases,FuelCost, &
                                               WVPA_PURCHASES_FUEL_COST)

         CALL GET_MM_DATABASE(Generation,FuelCost, &
                                              WVPA_GENERATION_FUEL_COST)
         CALL GET_MM_DATABASE(Services,FuelCost, &
                                                WVPA_SERVICES_FUEL_COST)

         CALL GET_MM_DATABASE(Derivative,FuelCost, &
                                              WVPA_DERIVATIVE_FUEL_COST)
         WVPA_DERIVATIVE_FUEL_COST = 1000.*WVPA_DERIVATIVE_FUEL_COST
         CALL GET_ACTUAL_DATABASE(FuelCost,WVPA_ACTUAL_FUEL_COST)

         TOTAL_MEMBER_SALES = IN_SALES &
               + IL_SALES &
               + MI_SALES &
               + NON_MEMBER_FUEL_SALES
         CALL WVPA_WHOLESALE_REV_COST_TRAMS(WVPA_TOTAL_WHOLESALE_COST, &
                 WVPA_WHOLESALE_FUEL_COST, & 
                 WVPA_WHOLESALE_NONFUEL_COST, &
                 WVPA_WHOLESALE_REVENUE) 
         WVPA_PSCR_AJD_2_FUEL_COST(:) = 0.
         CALL WVPA_ACTUAL_WHOLESALE_TRANS(R_RUN_YEAR, &
              WVPA_WHOLESALE_FUEL_COST, & 
              WVPA_WHOLESALE_NONFUEL_COST, &
              WVPA_WHOLESALE_REVENUE, & 
              WVPA_PSCR_AJD_2_FUEL_COST)
         WVPA_WHOLESALE_FUEL_COST(0)=SUM(WVPA_WHOLESALE_FUEL_COST(1:))
         WVPA_WHOLESALE_NONFUEL_COST(0) = &
                                    SUM(WVPA_WHOLESALE_NONFUEL_COST(1:))
         WVPA_TOTAL_WHOLESALE_COST(:) = WVPA_WHOLESALE_FUEL_COST(:) &
                                        + WVPA_WHOLESALE_NONFUEL_COST(:)
!
         WVPA_PURCHASES_FUEL_COST(0,:) = &
                               SUM(WVPA_PURCHASES_FUEL_COST(1:,:),DIM=1)
         WVPA_GENERATION_FUEL_COST(0,:) = &
                              SUM(WVPA_GENERATION_FUEL_COST(1:,:),DIM=1)
         WVPA_SERVICES_FUEL_COST(0,:) = &
                                SUM(WVPA_SERVICES_FUEL_COST(1:,:),DIM=1)
         WVPA_DERIVATIVE_FUEL_COST(0,:) = &
                              SUM(WVPA_DERIVATIVE_FUEL_COST(1:,:),DIM=1)
         WVPA_WHOLESALE_FUEL_COST(0) = SUM(WVPA_WHOLESALE_FUEL_COST(1:))
!         WVPA_OTHER_FUEL_COST = 0.
         WVPA_OTHER_FUEL_COST(1:) = &
           WVPA_TRACKED_DEFERRED_DEBITS(1:,R_RUN_YEAR,BothIndianaPSCR) &
           + WVPA_TRACKED_DEFERRED_DEBITS(1:,R_RUN_YEAR,IndianaOnly)
!         DO MO = 1, 12
!            IF(BASE_DATE + MO >= HISTORICAL_PRODUCTION_DATE()) EXIT
!            WVPA_OTHER_FUEL_COST(MO) = 0.
!         ENDDO
         WVPA_OTHER_FUEL_COST(0) = SUM(WVPA_OTHER_FUEL_COST(1:))
         WVPA_TOTAL_FUEL_COST = WVPA_PURCHASES_FUEL_COST(:,IndianaOnly) &
                        + WVPA_PURCHASES_FUEL_COST(:,BothIndianaPSCR) &
                        + WVPA_GENERATION_FUEL_COST(:,IndianaOnly) &
                        + WVPA_GENERATION_FUEL_COST(:,BothIndianaPSCR) &
                        + WVPA_SERVICES_FUEL_COST(:,IndianaOnly) &
                        + WVPA_SERVICES_FUEL_COST(:,BothIndianaPSCR) &
                        + WVPA_DERIVATIVE_FUEL_COST(:,IndianaOnly) &
                        + WVPA_DERIVATIVE_FUEL_COST(:,BothIndianaPSCR) &
                        + WVPA_ACTUAL_FUEL_COST(:,IndianaOnly) &
                        + WVPA_ACTUAL_FUEL_COST(:,BothIndianaPSCR) &
                        + WVPA_OTHER_FUEL_COST(:) &
                        - WVPA_WHOLESALE_FUEL_COST(:)
!
! NON_FUEL SECTION
!
         WVPA_PURCHASES_NON_FUEL_COST = 0.
         WVPA_GENERATION_NON_FUEL_COST = 0.
         WVPA_SERVICES_NON_FUEL_COST = 0.
         WVPA_DEMAND_NON_FUEL_COST = 0.
         WVPA_DERIVATIVE_NON_FUEL_COST = 0.
         WVPA_ACTUAL_NON_FUEL_COST = 0.
!
         CALL GET_MM_DATABASE(Purchases,NonFuelCost, &
                                           WVPA_PURCHASES_NON_FUEL_COST)
         CALL GET_MM_DATABASE(Purchases,DemandCost, &
                                           WVPA_PURCHASES_NON_FUEL_COST)
         CALL GET_MM_DATABASE(Generation,NonFuelCost, &
                                          WVPA_GENERATION_NON_FUEL_COST)
         CALL GET_MM_DATABASE(Generation,DemandCost, &
                                          WVPA_GENERATION_NON_FUEL_COST)
         CALL GET_MM_DATABASE(Services,NonFuelCost, &
                                            WVPA_SERVICES_NON_FUEL_COST)
         CALL GET_MM_DATABASE(Services,DemandCost, &
                                            WVPA_SERVICES_NON_FUEL_COST)
!         CALL GET_MM_DATABASE(Services,NonFuelCost,
!     +                                      WVPA_SERVICES_NON_FUEL_COST)
         CALL GET_MM_DATABASE(Derivative,NonFuelCost, &
                                          WVPA_DERIVATIVE_NON_FUEL_COST)
         CALL GET_MM_DATABASE(Derivative,DemandCost, &
                                          WVPA_DERIVATIVE_NON_FUEL_COST)
         CALL GET_ACTUAL_DATABASE(NonFuelCost, &
                                              WVPA_ACTUAL_NON_FUEL_COST)
         CALL GET_ACTUAL_DATABASE(DemandCost,WVPA_ACTUAL_NON_FUEL_COST)
         WVPA_OTHER_NON_FUEL_COST(1:) = &
                  WVPA_TRACKED_EXPENSES(1:,R_RUN_YEAR,BothIndianaPSCR) &
                  + WVPA_TRACKED_EXPENSES(1:,R_RUN_YEAR,IndianaOnly)

         WVPA_OTHER_NON_FUEL_COST(0) = SUM(WVPA_OTHER_NON_FUEL_COST(1:))
!
         WVPA_PURCHASES_NON_FUEL_COST(0,:) = &
                           SUM(WVPA_PURCHASES_NON_FUEL_COST(1:,:),DIM=1)
         WVPA_GENERATION_NON_FUEL_COST(0,:) = &
                          SUM(WVPA_GENERATION_NON_FUEL_COST(1:,:),DIM=1)
         WVPA_SERVICES_NON_FUEL_COST(0,:) = &
                            SUM(WVPA_SERVICES_NON_FUEL_COST(1:,:),DIM=1)
         WVPA_DEMAND_NON_FUEL_COST(0,:) = &
                              SUM(WVPA_DEMAND_NON_FUEL_COST(1:,:),DIM=1)
         WVPA_DERIVATIVE_NON_FUEL_COST(0,:) = &
                          SUM(WVPA_DERIVATIVE_NON_FUEL_COST(1:,:),DIM=1)
!
         WVPA_TOTAL_NON_FUEL_COST(:) = &
                    WVPA_PURCHASES_NON_FUEL_COST(:,IndianaOnly) &
                    + WVPA_PURCHASES_NON_FUEL_COST(:,BothIndianaPSCR) &
                    + WVPA_GENERATION_NON_FUEL_COST(:,IndianaOnly) &
                    + WVPA_GENERATION_NON_FUEL_COST(:,BothIndianaPSCR) &
                    + WVPA_SERVICES_NON_FUEL_COST(:,IndianaOnly) &
                    + WVPA_SERVICES_NON_FUEL_COST(:,BothIndianaPSCR) &
                    + WVPA_DEMAND_NON_FUEL_COST(:,IndianaOnly) &
                    + WVPA_DEMAND_NON_FUEL_COST(:,BothIndianaPSCR) &
                    + WVPA_DERIVATIVE_NON_FUEL_COST(:,IndianaOnly) &
                    + WVPA_DERIVATIVE_NON_FUEL_COST(:,BothIndianaPSCR) &
                    + WVPA_OTHER_NON_FUEL_COST(:) &
                    + WVPA_ACTUAL_NON_FUEL_COST(:,IndianaOnly) &
                    + WVPA_ACTUAL_NON_FUEL_COST(:,BothIndianaPSCR) &
                    - WVPA_WHOLESALE_NONFUEL_COST(:)

!
! MICHIGAN SECTION
!
         WVPA_OTHER_PSCR_COST(1:) = &
                  WVPA_TRACKED_EXPENSES(1:,R_RUN_YEAR,BothIndianaPSCR) &
                  + WVPA_TRACKED_EXPENSES(1:,R_RUN_YEAR,PSCROnly)
         DO MO = 1, 12
            IF(BASE_DATE + MO >= HISTORICAL_PRODUCTION_DATE()) EXIT
            WVPA_OTHER_PSCR_COST(MO) = 0.
         ENDDO
         WVPA_OTHER_PSCR_COST(0) = SUM(WVPA_OTHER_PSCR_COST(1:))
!
         WVPA_TOTAL_MI_FUEL_COST = &
                   WVPA_PURCHASES_FUEL_COST(:,PSCROnly) &
                   + WVPA_GENERATION_FUEL_COST(:,PSCROnly) &
                   + WVPA_SERVICES_FUEL_COST(:,PSCROnly) &
                   + WVPA_DERIVATIVE_FUEL_COST(:,PSCROnly) &
                   + WVPA_ACTUAL_FUEL_COST(:,PSCROnly)
!     +             - WVPA_PSCR_AJD_2_FUEL_COST(:)
         WVPA_TOTAL_MI_NON_FUEL_COST = &
                   + WVPA_PURCHASES_NON_FUEL_COST(:,PSCROnly) &
                   + WVPA_SERVICES_NON_FUEL_COST(:,PSCROnly) &
                   + WVPA_DERIVATIVE_NON_FUEL_COST(:,PSCROnly) &
                   + WVPA_DEMAND_NON_FUEL_COST(:,PSCROnly) &
                   + WVPA_ACTUAL_NON_FUEL_COST(:,PSCROnly)
!     +             + WVPA_OTHER_PSCR_COST                  ! 108
!     +             - WVPA_WHOLESALE_REVENUE ! removed 1/29/04
         WVPA_TOTAL_PSCR_COST = &
                   WVPA_PURCHASES_FUEL_COST(:,PSCROnly) &
                   +WVPA_PURCHASES_FUEL_COST(:,BothIndianaPSCR) &
                   + WVPA_GENERATION_FUEL_COST(:,PSCROnly) &
                   + WVPA_GENERATION_FUEL_COST(:,BothIndianaPSCR) &
                   + WVPA_SERVICES_FUEL_COST(:,PSCROnly) &
                   + WVPA_SERVICES_FUEL_COST(:,BothIndianaPSCR) &
                   + WVPA_DERIVATIVE_FUEL_COST(:,PSCROnly) &
                   + WVPA_DERIVATIVE_FUEL_COST(:,BothIndianaPSCR) &
                   + WVPA_ACTUAL_FUEL_COST(:,PSCROnly) &
                   + WVPA_ACTUAL_FUEL_COST(:,BothIndianaPSCR) &
                   - WVPA_PSCR_AJD_2_FUEL_COST(:) &
                   + WVPA_PURCHASES_NON_FUEL_COST(:,PSCROnly) &
                   + WVPA_PURCHASES_NON_FUEL_COST(:,BothIndianaPSCR) &
                   + WVPA_SERVICES_NON_FUEL_COST(:,PSCROnly) &
                   + WVPA_SERVICES_NON_FUEL_COST(:,BothIndianaPSCR) &
                   + WVPA_DERIVATIVE_NON_FUEL_COST(:,PSCROnly) &
                   + WVPA_DERIVATIVE_NON_FUEL_COST(:,BothIndianaPSCR) &
                   + WVPA_DEMAND_NON_FUEL_COST(:,PSCROnly) &
                   + WVPA_DEMAND_NON_FUEL_COST(:,BothIndianaPSCR) &
                   + WVPA_ACTUAL_NON_FUEL_COST(:,PSCROnly) &
                   + WVPA_ACTUAL_NON_FUEL_COST(:,BothIndianaPSCR) &
                   + WVPA_OTHER_PSCR_COST                  & !  108
                   - WVPA_WHOLESALE_REVENUE ! removed 1/29/04

! 5/4/04 MODIFICATION TO PUT MI IN THE REGULAR TRACKER REPORT

         WVPA_TOTAL_NON_FUEL_COST(:) = WVPA_TOTAL_NON_FUEL_COST(:) &
                                       + WVPA_TOTAL_MI_NON_FUEL_COST(:)
         WVPA_TOTAL_FUEL_COST(:) = WVPA_TOTAL_FUEL_COST(:) &
                                   + WVPA_TOTAL_MI_FUEL_COST(:)

         DO MONTH = 1, 13
            IF(MOD(MONTH,INT(3,2)) == 1) START_QRT = MONTH
            MO = MONTH
            IF(MONTH == 13) THEN
               MO = 0
               QUARTERLY_RUNNING_SALES_MWH(0) = TOTAL_MEMBER_SALES(0)
               QUARTERLY_RUNNING_TOTAL_FL_COST(0) = &
                                                 WVPA_TOTAL_FUEL_COST(0)
               AVERAGE_QUARTERLY_FUEL_RATE(0) = WVPA_TOTAL_FUEL_COST(0)/ &
                                                 TOTAL_MEMBER_SALES(0)
!
! NON-FUEL VARIABLES
!
               QRTRLY_RUNNING_TOTAL_NONFL_COST(0) = &
                                             WVPA_TOTAL_NON_FUEL_COST(0)
               AVERAGE_QUARTERLY_NON_FUEL_RATE(0) = &
                                            WVPA_TOTAL_NON_FUEL_COST(0)/ &
                                               TOTAL_MEMBER_SALES(0)
!
! PSCR
!
               QRTRLY_RUNNING_TOTAL_PSCR_COST(0) = &
                                                 WVPA_TOTAL_PSCR_COST(0)
               AVERAGE_QUARTERLY_PSCR_RATE(0) = WVPA_TOTAL_PSCR_COST(0)/ &
                                                 TOTAL_MEMBER_SALES(0)

            ELSE
               QUARTERLY_RUNNING_SALES_MWH(MO) = &
                                   SUM(TOTAL_MEMBER_SALES(START_QRT:MO))
               QUARTERLY_RUNNING_TOTAL_FL_COST(MO) = &
                                 SUM(WVPA_TOTAL_FUEL_COST(START_QRT:MO))
               AVERAGE_QUARTERLY_FUEL_RATE(MO) = &
                               SUM(WVPA_TOTAL_FUEL_COST(START_QRT:MO))/ &
                                   SUM(TOTAL_MEMBER_SALES(START_QRT:MO))
!
! NON-FUEL VARIABLES
!
               QRTRLY_RUNNING_TOTAL_NONFL_COST(MO) = &
                             SUM(WVPA_TOTAL_NON_FUEL_COST(START_QRT:MO))
               AVERAGE_QUARTERLY_NON_FUEL_RATE(MO) = &
                              QRTRLY_RUNNING_TOTAL_NONFL_COST(MO)/ &
                                  QUARTERLY_RUNNING_SALES_MWH(MO)
!
! PSCR
!
               QRTRLY_RUNNING_TOTAL_PSCR_COST(MO) = &
                           SUM(WVPA_TOTAL_PSCR_COST(START_QRT:MO))
               AVERAGE_QUARTERLY_PSCR_RATE(MO) = &
                                  QRTRLY_RUNNING_TOTAL_PSCR_COST(MO)/ &
                                      QUARTERLY_RUNNING_SALES_MWH(MO)
            ENDIF
         ENDDO
         AVERAGE_QUARTERLY_FUEL_RATE = 1000000. * &
                                             AVERAGE_QUARTERLY_FUEL_RATE
         QUARTERLY_FLAT_POWER_RATE(0) = &
                                    SUM(WVPA_TOTAL_FUEL_COST(1:12))/ &
                                           SUM(TOTAL_MEMBER_SALES(1:12))
         QUARTERLY_FLAT_POWER_RATE(1:3) = &
                                       SUM(WVPA_TOTAL_FUEL_COST(1:3))/ &
                                            SUM(TOTAL_MEMBER_SALES(1:3))
         QUARTERLY_FLAT_POWER_RATE(4:6) = &
                                       SUM(WVPA_TOTAL_FUEL_COST(4:6))/ &
                                            SUM(TOTAL_MEMBER_SALES(4:6))
         QUARTERLY_FLAT_POWER_RATE(7:9) = &
                                       SUM(WVPA_TOTAL_FUEL_COST(7:9))/ &
                                            SUM(TOTAL_MEMBER_SALES(7:9))
         QUARTERLY_FLAT_POWER_RATE(10:12) = &
                                      SUM(WVPA_TOTAL_FUEL_COST(10:12))/ &
                                          SUM(TOTAL_MEMBER_SALES(10:12))
!
! NON_FUEL ADJ
!
         AVERAGE_QUARTERLY_NON_FUEL_RATE = 1000000. * &
                                         AVERAGE_QUARTERLY_NON_FUEL_RATE
         QUARTERLY_FLAT_NON_FUEL_RATE(0) = &
                                    SUM(WVPA_TOTAL_NON_FUEL_COST(1:12))/ &
                                        SUM(TOTAL_MEMBER_SALES(1:12))
         QUARTERLY_FLAT_NON_FUEL_RATE(1:3) = &
                                     SUM(WVPA_TOTAL_NON_FUEL_COST(1:3))/ &
                                         SUM(TOTAL_MEMBER_SALES(1:3))
         QUARTERLY_FLAT_NON_FUEL_RATE(4:6) = &
                                     SUM(WVPA_TOTAL_NON_FUEL_COST(4:6))/ &
                                         SUM(TOTAL_MEMBER_SALES(4:6))
         QUARTERLY_FLAT_NON_FUEL_RATE(7:9) = &
                                     SUM(WVPA_TOTAL_NON_FUEL_COST(7:9))/ &
                                         SUM(TOTAL_MEMBER_SALES(7:9))
         QUARTERLY_FLAT_NON_FUEL_RATE(10:12) = &
                                   SUM(WVPA_TOTAL_NON_FUEL_COST(10:12))/ &
                                       SUM(TOTAL_MEMBER_SALES(10:12))
!
! PSCR
!
         AVERAGE_QUARTERLY_PSCR_RATE = 1000000. * &
                                         AVERAGE_QUARTERLY_PSCR_RATE
         QUARTERLY_FLAT_PSCR_RATE(0) = &
                                    SUM(WVPA_TOTAL_PSCR_COST(1:12))/ &
                                        SUM(TOTAL_MEMBER_SALES(1:12))
         QUARTERLY_FLAT_PSCR_RATE(1:3) = &
                                     SUM(WVPA_TOTAL_PSCR_COST(1:3))/ &
                                         SUM(TOTAL_MEMBER_SALES(1:3))
         QUARTERLY_FLAT_PSCR_RATE(4:6) = &
                                     SUM(WVPA_TOTAL_PSCR_COST(4:6))/ &
                                         SUM(TOTAL_MEMBER_SALES(4:6))
         QUARTERLY_FLAT_PSCR_RATE(7:9) = &
                                     SUM(WVPA_TOTAL_PSCR_COST(7:9))/ &
                                         SUM(TOTAL_MEMBER_SALES(7:9))
         QUARTERLY_FLAT_PSCR_RATE(10:12) = &
                                   SUM(WVPA_TOTAL_PSCR_COST(10:12))/ &
                                       SUM(TOTAL_MEMBER_SALES(10:12))
!
!
!
         WVPA_QUARTERLY_SYSTEM_RATES = QUARTERLY_FLAT_POWER_RATE(0)
         QUARTERLY_FLAT_POWER_RATE = 1000000.*QUARTERLY_FLAT_POWER_RATE
         QUARTERLY_FLAT_NON_FUEL_RATE = 1000000. * &
                                            QUARTERLY_FLAT_NON_FUEL_RATE
         QUARTERLY_FLAT_PSCR_RATE =  1000000. * &
                                                QUARTERLY_FLAT_PSCR_RATE
      RETURN

      ENTRY WVPA_IN_FUEL_ADJUSTMENT_CAL(R_MO)

         QRT_FUEL_COLLECTION_AMOUNTS = &
                               SUM(FUEL_COLLECTION_AMOUNTS(R_MO:R_MO+2))
         IN_QRT_SALES = SUM(IN_SALES(R_MO:R_MO+2)) &
                        + SUM(IL_SALES(R_MO:R_MO+2)) &
                        + SUM(MI_SALES(R_MO:R_MO+2))
         QRT_RECONCILIATION_ADJ(R_MO:R_MO+2) = &
                                 1000000. * QRT_FUEL_COLLECTION_AMOUNTS/ &
                                                  IN_QRT_SALES
         WVPA_IN_FUEL_ADJUSTMENT_CAL = QRT_RECONCILIATION_ADJ(R_MO)

         NEW_FUEL_ADJ_RATE = (QUARTERLY_FLAT_POWER_RATE(R_MO) &
                               + QRT_RECONCILIATION_ADJ(R_MO))/1000.

      RETURN

      ENTRY WVPA_IN_FUEL_TRACKER_CALC(R_MO)

         MO = R_MO
         IF(MO == 1) UNCOLLECTED_FUEL_BALANCE = &
                                      BEG_UNCOLLECTED_FUEL_BALANCE
       IN_ACTUAL_FUEL_COST(MO) =(IN_SALES(MO)/TOTAL_MEMBER_SALES(MO))* &
                                          WVPA_TOTAL_FUEL_COST(MO)
       IL_ACTUAL_FUEL_COST(MO) =(IL_SALES(MO)/TOTAL_MEMBER_SALES(MO))* &
                                         WVPA_TOTAL_FUEL_COST(MO)
       MI_ACTUAL_FUEL_COST(MO) =(MI_SALES(MO)/TOTAL_MEMBER_SALES(MO))* &
                                         WVPA_TOTAL_FUEL_COST(MO)
         INCOME_FUEL_STATEMENT_EFFECT(MO) =  IN_ACTUAL_FUEL_COST(MO) &
                                            - IN_BASE_FUEL_DOLLARS(MO) &
                                            - IN_FUEL_ADJ_DOLLARS(MO) &
                                            + IL_ACTUAL_FUEL_COST(MO) &
                                            - IL_BASE_FUEL_DOLLARS(MO) &
                                            - IL_FUEL_ADJ_DOLLARS(MO) &
                                            + MI_ACTUAL_FUEL_COST(MO) &
                                            - MI_BASE_FUEL_DOLLARS(MO) &
                                            - MI_FUEL_ADJ_DOLLARS(MO)
         CLOSING_UNCOLLECTED_FL_BALANCE = UNCOLLECTED_FUEL_BALANCE &
                                      + INCOME_FUEL_STATEMENT_EFFECT(MO)
         UNCOLLECTED_FUEL_BALANCE = CLOSING_UNCOLLECTED_FL_BALANCE
         RPT_FUEL_COLLECTION_AMOUNTS(MO) = FUEL_COLLECTION_AMOUNTS(MO)
         IF(MO >=  6) THEN
            FUEL_COLLECTION_AMOUNTS(MO-5) = &
                                      INCOME_FUEL_STATEMENT_EFFECT(MO) &
                                      + FUEL_COLLECTION_AMOUNTS(MO)
         ELSEIF(MO < 6 .AND. MO > 0) THEN
            FUEL_COLLECTION_AMOUNTS(MO+7) = &
                                      INCOME_FUEL_STATEMENT_EFFECT(MO) &
                                      + FUEL_COLLECTION_AMOUNTS(MO)
         ENDIF
         WVPA_IN_FUEL_TRACKER_CALC = &
                                        INCOME_FUEL_STATEMENT_EFFECT(MO)
      RETURN

      ENTRY WVPA_NON_FUEL_ADJUSTMENT_CAL(R_MO)

         QRT_NON_FUEL_COLLECTION_AMOUNTS = &
                           SUM(NON_FUEL_COLLECTION_AMOUNTS(R_MO:R_MO+2))
         IN_QRT_SALES = SUM(NONFUEL_TRACKER_SALES(R_MO:R_MO+2))
         QRT_NONFUEL_RECONCILIATION_ADJ(R_MO:R_MO+2) = &
                           1000000. * QRT_NON_FUEL_COLLECTION_AMOUNTS/ &
                                                  IN_QRT_SALES
         WVPA_NON_FUEL_ADJUSTMENT_CAL = &
                                    QRT_NONFUEL_RECONCILIATION_ADJ(R_MO)

         NEW_NON_FUEL_ADJ_RATE = (QUARTERLY_FLAT_NON_FUEL_RATE(R_MO) &
                          + QRT_NONFUEL_RECONCILIATION_ADJ(R_MO))/1000.

      RETURN

      ENTRY WVPA_NON_FUEL_TRACKER_CALC(R_MO)

         MO = R_MO
         IF(MO == 1) UNCOLLECTED_NON_FUEL_BALANCE = &
                                  BEG_UNCOLLECTED_NONFL_BALANCE
         NON_FUEL_TRACKER_SALES_IN = NONFUEL_TRACKER_SALES(MO) &
                                     - IL_SALES(MO) &
                                     - MI_SALES(MO)
         IN_ACTUAL_NON_FUEL_COST(MO) = WVPA_TOTAL_NON_FUEL_COST(MO) * &
                      (NON_FUEL_TRACKER_SALES_IN/TOTAL_MEMBER_SALES(MO))
         IL_ACTUAL_NON_FUEL_COST(MO) = WVPA_TOTAL_NON_FUEL_COST(MO) * &
                                     IL_SALES(MO)/TOTAL_MEMBER_SALES(MO)
         MI_ACTUAL_NON_FUEL_COST(MO) = WVPA_TOTAL_NON_FUEL_COST(MO) * &
                                     MI_SALES(MO)/TOTAL_MEMBER_SALES(MO)
         INCOME_NON_FL_STATEMENT_EFFECT(MO) = &
                                        IN_ACTUAL_NON_FUEL_COST(MO) &
                                        - IN_BASE_NONFUEL_DOLLARS(MO) &
                                        - IN_NONFUEL_ADJ_DOLLARS(MO) &
                                        + IL_ACTUAL_NON_FUEL_COST(MO) &
                                        - IL_BASE_NONFUEL_DOLLARS(MO) &
                                        - IL_NONFUEL_ADJ_DOLLARS(MO) &
                                        + MI_ACTUAL_NON_FUEL_COST(MO) &
                                        - MI_BASE_NONFUEL_DOLLARS(MO) &
                                        - MI_NONFUEL_ADJ_DOLLARS(MO)
         CLOS_UNCOLLECTED_NONFL_BALANCE = &
                                  UNCOLLECTED_NON_FUEL_BALANCE &
                                  + INCOME_NON_FL_STATEMENT_EFFECT(MO)
         UNCOLLECTED_NON_FUEL_BALANCE = &
                                    CLOS_UNCOLLECTED_NONFL_BALANCE
         RPT_NON_FUEL_COLLECTION_AMOUNTS(MO) = &
                                         NON_FUEL_COLLECTION_AMOUNTS(MO)
         IF(MO >=  6) THEN
            NON_FUEL_COLLECTION_AMOUNTS(MO-5) = &
                                    INCOME_NON_FL_STATEMENT_EFFECT(MO) &
                                    + NON_FUEL_COLLECTION_AMOUNTS(MO)
         ELSEIF(MO < 6 .AND. MO > 0) THEN
            NON_FUEL_COLLECTION_AMOUNTS(MO+7) = &
                                    INCOME_NON_FL_STATEMENT_EFFECT(MO) &
                                    + NON_FUEL_COLLECTION_AMOUNTS(MO)
         ENDIF
         WVPA_NON_FUEL_TRACKER_CALC = &
                                    INCOME_NON_FL_STATEMENT_EFFECT(MO)
      RETURN

      ENTRY WVPA_PSCR_ADJUSTMENT_CAL

         MI_BASE_FUEL_DOLLARS(0) = SUM(MI_BASE_FUEL_DOLLARS(1:))
         MI_SALES(0) = SUM(MI_SALES(1:))
         return
         PSCR_ANUAL_ACTUAL_FUEL_COST = WVPA_TOTAL_PSCR_COST(0)* &
                                   (MI_SALES(0)/TOTAL_MEMBER_SALES(0))
         PSCR_OVER_UNDER_COLLECTION = PSCR_ANUAL_ACTUAL_FUEL_COST &
                                      - MI_BASE_FUEL_DOLLARS(0)
         NEW_PSCR_ADJ_RATE = 1000. * PSCR_OVER_UNDER_COLLECTION/ &
                                            MI_SALES(0)
         WVPA_PSCR_ADJUSTMENT_CAL = NEW_PSCR_ADJ_RATE

!         VOID_LOGICAL = WVPA_UPDATE_PSCR_ADJ_RATES(NEW_PSCR_ADJ_RATE)
      RETURN

      ENTRY WVPA_PSCR_TRACKER_CALUCLATION

         WVPA_PSCR_TRACKER_CALUCLATION = 0.
         return
         DO MO = 1, 12
            IF(MO == 1) THEN
               UNCOLLECTED_PSCR_BALANCE = &
                                      BEG_UNCOLLECTED_PSCR_BALANCE
               PSCR_RECONCILATION_AMOUNT = &
                                     -BEG_UNCOLLECTED_PSCR_BALANCE
            ENDIF
            IF(MO == MICHIGAN_CASH_PAYMENT_MONTH) THEN
               PSCR_PRIOR_YEAR_RECONC_AMOUNT(MO) = &
                                               PSCR_RECONCILATION_AMOUNT
            ELSE
               PSCR_PRIOR_YEAR_RECONC_AMOUNT(MO) = 0.
            ENDIF
            PSCR_ACTUAL_FUEL_COST(MO) = WVPA_TOTAL_PSCR_COST(MO) * &
                                   (MI_SALES(MO)/TOTAL_MEMBER_SALES(MO))
            INCOME_PSCR_STATEMENT_EFFECT(MO) = &
                             PSCR_ACTUAL_FUEL_COST(MO) &
                             - MI_BASE_FUEL_DOLLARS(MO) &
                             - MI_FUEL_ADJ_DOLLARS(MO)
            CLOS_UNCOLLECTED_PSCR_BALANCE = &
                             UNCOLLECTED_PSCR_BALANCE &
                             + INCOME_PSCR_STATEMENT_EFFECT(MO) &
                             + PSCR_PRIOR_YEAR_RECONC_AMOUNT(MO)
            UNCOLLECTED_PSCR_BALANCE = &
                                        CLOS_UNCOLLECTED_PSCR_BALANCE
            PSCR_COLLECTION_AMOUNTS(MO) = &
                             INCOME_PSCR_STATEMENT_EFFECT(MO) &
                             + PSCR_PRIOR_YEAR_RECONC_AMOUNT(MO)
         ENDDO
         WVPA_PSCR_TRACKER_CALUCLATION = PSCR_COLLECTION_AMOUNTS(1)
      RETURN

      ENTRY WVPA_STORE_TRACKER_INFO(R_MO, &
                                    R_RATE_TRACKER, &
                                    R_RATE_CODE, &
                                    R_MONTHLY_BASE_FUEL_DOLLARS, &
                                    R_MONTHLY_FUEL_ADJ_DOLLARS, &
                                    R_MONTHLY_NONFUEL_DOLLARS, &
                                    R_MONTHLY_BASE_NONFUEL_DOLLARS, &
                                    R_MONTHLY_PSCR_DOLLARS)

!
         IF(INDEX(R_RATE_TRACKER,'IN') /= 0) THEN
            IN_BASE_FUEL_DOLLARS(R_MO) = IN_BASE_FUEL_DOLLARS(R_MO) &
                                         + R_MONTHLY_BASE_FUEL_DOLLARS
            IN_FUEL_ADJ_DOLLARS(R_MO) = IN_FUEL_ADJ_DOLLARS(R_MO) &
                                        + R_MONTHLY_FUEL_ADJ_DOLLARS
            IN_BASE_NONFUEL_DOLLARS(R_MO) =IN_BASE_NONFUEL_DOLLARS(R_MO) &
                                        + R_MONTHLY_BASE_NONFUEL_DOLLARS
            IF(R_RATE_CODE < 160000) THEN
               IN_NONFUEL_ADJ_DOLLARS(R_MO) = &
                                            IN_NONFUEL_ADJ_DOLLARS(R_MO) &
                                            + R_MONTHLY_NONFUEL_DOLLARS
            ENDIF
         ELSEIF(INDEX(R_RATE_TRACKER,'IL') /= 0)THEN
            IL_BASE_FUEL_DOLLARS(R_MO) = IL_BASE_FUEL_DOLLARS(R_MO) &
                                         + R_MONTHLY_BASE_FUEL_DOLLARS
            IL_FUEL_ADJ_DOLLARS(R_MO) = IL_FUEL_ADJ_DOLLARS(R_MO) &
                                        + R_MONTHLY_FUEL_ADJ_DOLLARS
            IL_NONFUEL_ADJ_DOLLARS(R_MO) = IL_NONFUEL_ADJ_DOLLARS(R_MO) &
                                           + R_MONTHLY_NONFUEL_DOLLARS
            IL_BASE_NONFUEL_DOLLARS(R_MO)=IL_BASE_NONFUEL_DOLLARS(R_MO) &
                                        + R_MONTHLY_BASE_NONFUEL_DOLLARS
         ELSEIF(INDEX(R_RATE_TRACKER,'MI') /= 0) THEN
            MI_BASE_FUEL_DOLLARS(R_MO) = MI_BASE_FUEL_DOLLARS (R_MO) &
                                         + R_MONTHLY_BASE_FUEL_DOLLARS
            MI_FUEL_ADJ_DOLLARS(R_MO) = MI_FUEL_ADJ_DOLLARS(R_MO) &
                                        + R_MONTHLY_FUEL_ADJ_DOLLARS
            MI_NONFUEL_ADJ_DOLLARS(R_MO) = MI_NONFUEL_ADJ_DOLLARS(R_MO) &
                                           + R_MONTHLY_NONFUEL_DOLLARS
            MI_BASE_NONFUEL_DOLLARS(R_MO)=MI_BASE_NONFUEL_DOLLARS(R_MO) &
                                        + R_MONTHLY_BASE_NONFUEL_DOLLARS
         ELSE
            NON_MEMBER_BASE_FUEL_DOLLARS(R_MO) = &
                                      NON_MEMBER_BASE_FUEL_DOLLARS(R_MO) &
                                      + R_MONTHLY_BASE_FUEL_DOLLARS
            NON_MEMBER_FUEL_ADJ_DOLLARS(R_MO) = &
                                       NON_MEMBER_FUEL_ADJ_DOLLARS(R_MO) &
                                       + R_MONTHLY_FUEL_ADJ_DOLLARS
            NON_MEMBER_NONFUEL_ADJ_DOLLARS(R_MO) = &
                                    NON_MEMBER_NONFUEL_ADJ_DOLLARS(R_MO) &
                                    + R_MONTHLY_NONFUEL_DOLLARS
            NON_MEMBER_BASE_NONFUEL_DOLLARS(R_MO) = &
                                   NON_MEMBER_BASE_NONFUEL_DOLLARS(R_MO) &
                                   + R_MONTHLY_BASE_NONFUEL_DOLLARS
            IF(R_RATE_CODE == 901021) THEN
               IN_BASE_FUEL_DOLLARS(R_MO) = IN_BASE_FUEL_DOLLARS(R_MO) &
                                           + R_MONTHLY_BASE_FUEL_DOLLARS
               IN_FUEL_ADJ_DOLLARS(R_MO) = IN_FUEL_ADJ_DOLLARS(R_MO) &
                                           + R_MONTHLY_FUEL_ADJ_DOLLARS
               IN_NONFUEL_ADJ_DOLLARS(R_MO) = &
                                            IN_NONFUEL_ADJ_DOLLARS(R_MO) &
                                            + R_MONTHLY_NONFUEL_DOLLARS
               IN_BASE_NONFUEL_DOLLARS(R_MO) = &
                                        IN_BASE_NONFUEL_DOLLARS(R_MO) &
                                        + R_MONTHLY_BASE_NONFUEL_DOLLARS
            ENDIF
         ENDIF
      RETURN

      ENTRY WVPA_RATE_TRACKER_REPORT(endpoint, &
                                     CURRENT_YEAR)

!
         UNCOLLECTED_FUEL_BALANCE = BEG_UNCOLLECTED_FUEL_BALANCE
         RPT_FUEL_COLLECTION_AMOUNTS(0) = &
                                    SUM(RPT_FUEL_COLLECTION_AMOUNTS(1:))
         UNCOLLECTED_NON_FUEL_BALANCE = &
                                  BEG_UNCOLLECTED_NONFL_BALANCE
         RPT_NON_FUEL_COLLECTION_AMOUNTS(0) = &
                                SUM(RPT_NON_FUEL_COLLECTION_AMOUNTS(1:))
         UNCOLLECTED_PSCR_BALANCE = BEG_UNCOLLECTED_PSCR_BALANCE
         PSCR_PRIOR_YEAR_RECONC_AMOUNT(0) = &
                          SUM(PSCR_PRIOR_YEAR_RECONC_AMOUNT(1:))
         INCOME_PSCR_STATEMENT_EFFECT(0) = &
                                   SUM(INCOME_PSCR_STATEMENT_EFFECT(1:))
!
         DO MONTH = 1, 13
            IF(MOD(MONTH,INT(3,2)) == 1) START_QRT = MONTH
            MO = MONTH
            IF(MONTH == 13) THEN
               MO = 0
               IN_ACTUAL_FUEL_COST(0) = SUM(IN_ACTUAL_FUEL_COST(1:))
               IN_BASE_FUEL_DOLLARS(0) = SUM(IN_BASE_FUEL_DOLLARS(1:))
               IN_FUEL_ADJ_DOLLARS(0) = SUM(IN_FUEL_ADJ_DOLLARS(1:))

               IL_ACTUAL_FUEL_COST(0) = SUM(IL_ACTUAL_FUEL_COST(1:))
               IL_BASE_FUEL_DOLLARS(0) = SUM(IL_BASE_FUEL_DOLLARS(1:))
               IL_FUEL_ADJ_DOLLARS(0) = SUM(IL_FUEL_ADJ_DOLLARS(1:))

               MI_ACTUAL_FUEL_COST(0) = SUM(MI_ACTUAL_FUEL_COST(1:))
               MI_BASE_FUEL_DOLLARS(0) =  SUM(MI_BASE_FUEL_DOLLARS(1:))
               MI_FUEL_ADJ_DOLLARS(0) = SUM(MI_FUEL_ADJ_DOLLARS(1:))

               INCOME_FUEL_STATEMENT_EFFECT(0) = &
                                   SUM(INCOME_FUEL_STATEMENT_EFFECT(1:))
               UNCOLLECTED_FUEL_BALANCE = &
                                      BEG_UNCOLLECTED_FUEL_BALANCE
               SEVEN_MNTH_PR_QRT_FUEL_RUN_SUM = &
                                          RPT_FUEL_COLLECTION_AMOUNTS(0)
               SEVEN_MONTH_PRIOR_QRT_FUEL_SUM = &
                                          RPT_FUEL_COLLECTION_AMOUNTS(0)
               MEMBER_SALES_FL_QRT_RUNNING_SUM = IN_SALES(0) &
                                                   + IL_SALES(0) &
                                                   + MI_SALES(0)
               MEMBER_SALES_FUEL_QRT_SUM = IN_SALES(0) &
                                           + IL_SALES(0) &
                                           + MI_SALES(0)
!
! NON-FUEL VALUES
!
               UNCOLLECTED_NON_FUEL_BALANCE = &
                                  BEG_UNCOLLECTED_NONFL_BALANCE
               IN_NONFUEL_ADJ_DOLLARS(0) = &
                                         SUM(IN_NONFUEL_ADJ_DOLLARS(1:))
               IN_BASE_NONFUEL_DOLLARS(0) = &
                                        SUM(IN_BASE_NONFUEL_DOLLARS(1:))
               IL_NONFUEL_ADJ_DOLLARS(0) = &
                                         SUM(IL_NONFUEL_ADJ_DOLLARS(1:))
               IL_BASE_NONFUEL_DOLLARS(0) = &
                                        SUM(IL_BASE_NONFUEL_DOLLARS(1:))
               IN_ACTUAL_NON_FUEL_COST(0) = &
                                        SUM(IN_ACTUAL_NON_FUEL_COST(1:))
               IL_ACTUAL_NON_FUEL_COST(0) = &
                                        SUM(IL_ACTUAL_NON_FUEL_COST(1:))
               MI_ACTUAL_NON_FUEL_COST(0) = &
                                        SUM(MI_ACTUAL_NON_FUEL_COST(1:))
               MI_NONFUEL_ADJ_DOLLARS(0) = &
                                         SUM(MI_NONFUEL_ADJ_DOLLARS(1:))
               MI_BASE_NONFUEL_DOLLARS(0) = &
                                        SUM(MI_BASE_NONFUEL_DOLLARS(1:))
               INCOME_NON_FL_STATEMENT_EFFECT(0) = &
                               SUM(INCOME_NON_FL_STATEMENT_EFFECT(1:))
               SVN_MNTH_PR_QRT_NON_FL_RUN_SUM = &
                                      RPT_NON_FUEL_COLLECTION_AMOUNTS(0)
               SEVEN_MNTH_PR_QRT_NON_FUEL_SUM = &
                                      RPT_NON_FUEL_COLLECTION_AMOUNTS(0)
               MBR_SALES_NONFL_QRT_RUNNING_SUM = &
                                                NONFUEL_TRACKER_SALES(0)
               MEMBER_SALES_NONFUEL_QRT_SUM = NONFUEL_TRACKER_SALES(0)
!
! PSCR
!
               UNCOLLECTED_PSCR_BALANCE = &
                                      BEG_UNCOLLECTED_PSCR_BALANCE
               MI_BASE_FUEL_DOLLARS(0) =  SUM(MI_BASE_FUEL_DOLLARS(1:))
               MI_FUEL_ADJ_DOLLARS(0) = SUM(MI_FUEL_ADJ_DOLLARS(1:))
               MI_NONFUEL_ADJ_DOLLARS(0) = &
                                         SUM(MI_NONFUEL_ADJ_DOLLARS(1:))
               MI_BASE_NONFUEL_DOLLARS(0) = &
                                        SUM(MI_BASE_NONFUEL_DOLLARS(1:))
               PSCR_ACTUAL_FUEL_COST(0) = SUM(PSCR_ACTUAL_FUEL_COST(1:))

            ELSE
               SEVEN_MNTH_PR_QRT_FUEL_RUN_SUM = &
                          SUM(RPT_FUEL_COLLECTION_AMOUNTS(START_QRT:MO))
               SEVEN_MONTH_PRIOR_QRT_FUEL_SUM = &
                 SUM(RPT_FUEL_COLLECTION_AMOUNTS(START_QRT:START_QRT+2))
               MEMBER_SALES_FL_QRT_RUNNING_SUM = &
                                           SUM(IN_SALES(START_QRT:MO)) &
                                           + SUM(IL_SALES(START_QRT:MO)) &
                                           + SUM(MI_SALES(START_QRT:MO))
               MEMBER_SALES_FUEL_QRT_SUM = &
                                  SUM(IN_SALES(START_QRT:START_QRT+2)) &
                                  + SUM(IL_SALES(START_QRT:START_QRT+2)) &
                                  + SUM(MI_SALES(START_QRT:START_QRT+2))
               SVN_MNTH_PR_QRT_NON_FL_RUN_SUM = &
                      SUM(RPT_NON_FUEL_COLLECTION_AMOUNTS(START_QRT:MO))
               SEVEN_MNTH_PR_QRT_NON_FUEL_SUM = SUM( &
                 RPT_NON_FUEL_COLLECTION_AMOUNTS(START_QRT:START_QRT+2))
               MBR_SALES_NONFL_QRT_RUNNING_SUM = &
                                SUM(NONFUEL_TRACKER_SALES(START_QRT:MO))
               MEMBER_SALES_NONFUEL_QRT_SUM = &
                       SUM(NONFUEL_TRACKER_SALES(START_QRT:START_QRT+2))
            ENDIF
!
            IN_BASE_FUEL_RATE = 1000000. * THE_RATIO_OF_A_TO_B( &
                                  IN_BASE_FUEL_DOLLARS(MO),IN_SALES(MO))
            IN_FUEL_ADJ_RATE = 1000000. * THE_RATIO_OF_A_TO_B( &
                                                IN_FUEL_ADJ_DOLLARS(MO), &
                                                      IN_SALES(MO))
            IL_BASE_FUEL_RATE = 1000000. * THE_RATIO_OF_A_TO_B( &
                                  IL_BASE_FUEL_DOLLARS(MO),IL_SALES(MO))
            IL_FUEL_ADJ_RATE = 1000000. * THE_RATIO_OF_A_TO_B( &
                                                IL_FUEL_ADJ_DOLLARS(MO), &
                                                      IL_SALES(MO))
            MI_BASE_FUEL_RATE = 1000000. * THE_RATIO_OF_A_TO_B( &
                                  MI_BASE_FUEL_DOLLARS(MO),MI_SALES(MO))
            MI_FUEL_ADJ_RATE = 1000000. * THE_RATIO_OF_A_TO_B( &
                                                MI_FUEL_ADJ_DOLLARS(MO), &
                                                       MI_SALES(MO))
            CLOSING_UNCOLLECTED_FL_BALANCE = UNCOLLECTED_FUEL_BALANCE &
                                      + INCOME_FUEL_STATEMENT_EFFECT(MO)
            BASE_FUEL_RATE(MO) = 1000000.*(IN_BASE_FUEL_DOLLARS(MO) &
                                           + IL_BASE_FUEL_DOLLARS(MO) &
                                           + MI_BASE_FUEL_DOLLARS(MO))/ &
                             (IN_SALES(MO) + IL_SALES(MO)+ MI_SALES(MO))
            QUARTERLY_POWER_RATE_ADJ = QUARTERLY_FLAT_POWER_RATE(MO) & !  22
                                       - BASE_FUEL_RATE(MO)
            IN_FUEL_ADJ_RATE = 1000000. * THE_RATIO_OF_A_TO_B( &
                                                IN_FUEL_ADJ_DOLLARS(MO), &
                                                      IN_SALES(MO))

            QRT_MOVING_COLLECTION_RATE = 1000000. * THE_RATIO_OF_A_TO_B( &
                                     SEVEN_MNTH_PR_QRT_FUEL_RUN_SUM, &
                                      MEMBER_SALES_FL_QRT_RUNNING_SUM)
            IF(MO == 0) QRT_RECONCILIATION_ADJ(MO) = &
                                              QRT_MOVING_COLLECTION_RATE
!
! NON-FUEL REPORT VARAIBLES
!
            NON_FUEL_TRACKER_SALES_IN = NONFUEL_TRACKER_SALES(MO) &
                                        - IL_SALES(MO) &
                                        - MI_SALES(MO)
            CLOS_UNCOLLECTED_NONFL_BALANCE = &
                                            UNCOLLECTED_NON_FUEL_BALANCE &
                                  + INCOME_NON_FL_STATEMENT_EFFECT(MO)
            BASE_NONFUEL_RATE(MO)=1000000.*(IN_BASE_NONFUEL_DOLLARS(MO) &
                                   + IL_BASE_NONFUEL_DOLLARS(MO) &
                                   + MI_BASE_NONFUEL_DOLLARS(MO))/ &
                                            NONFUEL_TRACKER_SALES(MO)
            QUARTERLY_NONFUEL_RATE_ADJ=QUARTERLY_FLAT_NON_FUEL_RATE(MO) & !  22
                                       - BASE_NONFUEL_RATE(MO)
            IN_BASE_NON_FUEL_RATE = 1000000. * &
                        THE_RATIO_OF_A_TO_B(IN_BASE_NONFUEL_DOLLARS(MO), &
                                              NON_FUEL_TRACKER_SALES_IN)
            IN_FUEL_NON_ADJ_RATE = 1000000. * THE_RATIO_OF_A_TO_B( &
                                             IN_NONFUEL_ADJ_DOLLARS(MO), &
                                              NON_FUEL_TRACKER_SALES_IN)
            IL_BASE_NON_FUEL_RATE = 1000000. * THE_RATIO_OF_A_TO_B( &
                               IL_BASE_NONFUEL_DOLLARS(MO),IL_SALES(MO))
            IL_FUEL_NON_ADJ_RATE = 1000000. * THE_RATIO_OF_A_TO_B( &
                                             IL_NONFUEL_ADJ_DOLLARS(MO), &
                                                           IL_SALES(MO))
            MI_BASE_NON_FUEL_RATE = 1000000. * THE_RATIO_OF_A_TO_B( &
                               MI_BASE_NONFUEL_DOLLARS(MO),MI_SALES(MO))
            MI_FUEL_NON_ADJ_RATE = 1000000. * THE_RATIO_OF_A_TO_B( &
                                             MI_NONFUEL_ADJ_DOLLARS(MO), &
                                                           MI_SALES(MO))
            QRT_MOVING_NONFUEL_COLL_RATE = 1000000. * &
                     THE_RATIO_OF_A_TO_B( &
                                 SVN_MNTH_PR_QRT_NON_FL_RUN_SUM, &
                                   MBR_SALES_NONFL_QRT_RUNNING_SUM)
            IF(MO == 0) QRT_NONFUEL_RECONCILIATION_ADJ(MO) = &
                                      QRT_MOVING_NONFUEL_COLL_RATE
!
! PSCR RATES
!
            CLOS_UNCOLLECTED_PSCR_BALANCE = &
                             UNCOLLECTED_PSCR_BALANCE &
                             + INCOME_PSCR_STATEMENT_EFFECT(MO) &
                             + PSCR_PRIOR_YEAR_RECONC_AMOUNT(MO)
            PSCR_BASE_FUEL_RATE = 1000000. * &
              THE_RATIO_OF_A_TO_B(MI_BASE_FUEL_DOLLARS(MO),MI_SALES(MO))
            PSCR_BASE_FUEL_ADJ_RATE = 1000000. * &
               THE_RATIO_OF_A_TO_B(MI_FUEL_ADJ_DOLLARS(MO),MI_SALES(MO))
            NEW_PSCR_ADJ_RATE = 1000000.*WVPA_TOTAL_PSCR_COST(MO)/ &
                                                TOTAL_MEMBER_SALES(MO) &
                                - PSCR_BASE_FUEL_RATE
!
            WRITE(WVPA_TRACKER_UNIT,REC=WVPA_TRACKER_REC) &
                        PRT_ENDPOINT(), &
                        FLOAT(CURRENT_YEAR), &
                        SHORT_MONTH_NAMES(MO), &
                        0., &
                    IN_SALES(MO) + IL_SALES(MO)+ MI_SALES(MO),  & !  1
                        IN_BASE_FUEL_RATE, &
                        IN_FUEL_ADJ_RATE, &
                        IN_BASE_FUEL_RATE &
                               + IN_FUEL_ADJ_RATE, &
                        IN_BASE_FUEL_DOLLARS(MO) &
                               + IN_FUEL_ADJ_DOLLARS(MO), & !  5
                        WVPA_TOTAL_FUEL_COST(MO),  & !  6
                        TOTAL_MEMBER_SALES(MO),  & !  7
                        IN_ACTUAL_FUEL_COST(MO), &
                        UNCOLLECTED_FUEL_BALANCE, &
                        INCOME_FUEL_STATEMENT_EFFECT(MO),  & !  10
                        CLOSING_UNCOLLECTED_FL_BALANCE,  & !  11
                        WVPA_PURCHASES_FUEL_COST(MO, &
                                                 IndianaOnly) &
                              + WVPA_PURCHASES_FUEL_COST(MO, &
                                           BothIndianaPSCR), & !  12
                        WVPA_GENERATION_FUEL_COST(MO, &
                                                 IndianaOnly) &
                              + WVPA_GENERATION_FUEL_COST(MO, &
                                           BothIndianaPSCR), & !  13
                        WVPA_SERVICES_FUEL_COST(MO, &
                                                 IndianaOnly) &
                              + WVPA_SERVICES_FUEL_COST(MO, &
                                           BothIndianaPSCR), & !  14
                        WVPA_DEMAND_FUEL_COST(MO, &
                                                 IndianaOnly) &
                              + WVPA_DEMAND_FUEL_COST(MO, &
                                           BothIndianaPSCR), & !  15
                        RPT_FUEL_COLLECTION_AMOUNTS(MO),       & !  16
                        INCOME_FUEL_STATEMENT_EFFECT(MO) &
                          + RPT_FUEL_COLLECTION_AMOUNTS(MO), & !  17
                        QUARTERLY_RUNNING_TOTAL_FL_COST(MO), & !  18
                        QUARTERLY_RUNNING_SALES_MWH(MO), & !  19
                        AVERAGE_QUARTERLY_FUEL_RATE(MO), &
                        QUARTERLY_POWER_RATE_ADJ, &
                        QUARTERLY_FLAT_POWER_RATE(MO), & !  22
                        IN_BASE_FUEL_DOLLARS(MO),  & !  23
                        IN_FUEL_ADJ_DOLLARS(MO), & !  24
                        IN_SALES(MO),  & !  25
                        IL_SALES(MO),  & !  26
                        IL_BASE_FUEL_RATE, &
                        IL_FUEL_ADJ_RATE, & !  28
                        IL_BASE_FUEL_RATE &
                          + IL_FUEL_ADJ_RATE, & !  29
                        IL_BASE_FUEL_DOLLARS(MO) &
                          + IL_FUEL_ADJ_DOLLARS(MO), & !  30
                        IN_BASE_FUEL_DOLLARS(MO) &
                          + IN_FUEL_ADJ_DOLLARS(MO) &
                          + IL_BASE_FUEL_DOLLARS(MO) &
                          + IL_FUEL_ADJ_DOLLARS(MO) &
                          + MI_BASE_FUEL_DOLLARS(MO) &
                          + MI_FUEL_ADJ_DOLLARS(MO),  & !  31
                        WVPA_WHOLESALE_FUEL_COST(MO), &
                        BASE_FUEL_RATE(MO),             & !  33
                        IL_BASE_FUEL_DOLLARS(MO),        & !  34
                        QRT_RECONCILIATION_ADJ(MO),      & !  35
                        SEVEN_MNTH_PR_QRT_FUEL_RUN_SUM, &
                        SEVEN_MONTH_PRIOR_QRT_FUEL_SUM, &
                        MEMBER_SALES_FL_QRT_RUNNING_SUM,    & !  38
                        MEMBER_SALES_FUEL_QRT_SUM,              & !  39
                        QRT_MOVING_COLLECTION_RATE,      & !  40
                        QUARTERLY_POWER_RATE_ADJ &
                            + QRT_RECONCILIATION_ADJ(MO),  & !  41
                        IL_ACTUAL_FUEL_COST(MO),           & !  42
                        IN_ACTUAL_FUEL_COST(MO) &
                            + IL_ACTUAL_FUEL_COST(MO) &
                            + MI_ACTUAL_FUEL_COST(MO),       & !  43
                        IL_FUEL_ADJ_DOLLARS(MO), &             ! 44
!
! WVPA NON_FUEL REPORT VARIABLES
!
                        AVERAGE_QUARTERLY_NON_FUEL_RATE(MO),  & !  45
                        QUARTERLY_FLAT_NON_FUEL_RATE(MO),     & !  46
                        SVN_MNTH_PR_QRT_NON_FL_RUN_SUM, &
                        SEVEN_MNTH_PR_QRT_NON_FUEL_SUM, &    ! 48
                        WVPA_TOTAL_NON_FUEL_COST(MO), &
                        WVPA_PURCHASES_NON_FUEL_COST(MO, &
                                                  IndianaOnly) &
                             + WVPA_PURCHASES_NON_FUEL_COST(MO, &
                                            BothIndianaPSCR), & !  50
                        WVPA_GENERATION_NON_FUEL_COST(MO, &
                                                  IndianaOnly) &
                            + WVPA_GENERATION_NON_FUEL_COST(MO, &
                                            BothIndianaPSCR), & !  51
                        WVPA_SERVICES_NON_FUEL_COST(MO, &
                                                  IndianaOnly) &
                              + WVPA_SERVICES_NON_FUEL_COST(MO, &
                                            BothIndianaPSCR), & !  52
                        WVPA_DEMAND_NON_FUEL_COST(MO, &
                                                  IndianaOnly) &
                                + WVPA_DEMAND_NON_FUEL_COST(MO, &
                                            BothIndianaPSCR),   & !  53
                        WVPA_WHOLESALE_NONFUEL_COST(MO),       & !  54
                        IN_NONFUEL_ADJ_DOLLARS(MO),            & !  55
                        IN_BASE_NONFUEL_DOLLARS(MO),           & !  56
                        IN_NONFUEL_ADJ_DOLLARS(MO) &
                           + IN_BASE_NONFUEL_DOLLARS(MO),      & !  57
                        IL_NONFUEL_ADJ_DOLLARS(MO),            & !  58
                        IL_BASE_NONFUEL_DOLLARS(MO),           & !  59
                        IL_NONFUEL_ADJ_DOLLARS(MO) &
                           + IL_BASE_NONFUEL_DOLLARS(MO),      & !  60
                        IN_NONFUEL_ADJ_DOLLARS(MO) &
                           + IN_BASE_NONFUEL_DOLLARS(MO) &
                           + IL_NONFUEL_ADJ_DOLLARS(MO) &
                           + IL_BASE_NONFUEL_DOLLARS(MO) &
                           + MI_NONFUEL_ADJ_DOLLARS(MO) &
                           + MI_BASE_NONFUEL_DOLLARS(MO),      & !  61
                        RPT_NON_FUEL_COLLECTION_AMOUNTS(MO),   & !  62
                        IN_ACTUAL_NON_FUEL_COST(MO),           & !  63
                        IL_ACTUAL_NON_FUEL_COST(MO),           & !  64
                        IN_ACTUAL_NON_FUEL_COST(MO) &
                           + IL_ACTUAL_NON_FUEL_COST(MO) &
                           + MI_ACTUAL_NON_FUEL_COST(MO),      & !  65
                        INCOME_NON_FL_STATEMENT_EFFECT(MO),  & !  66
                        UNCOLLECTED_NON_FUEL_BALANCE,          & !  67
                        CLOS_UNCOLLECTED_NONFL_BALANCE,  & !  68
                        INCOME_NON_FL_STATEMENT_EFFECT(MO) &
                          + RPT_NON_FUEL_COLLECTION_AMOUNTS(MO), & ! 69
                        IN_BASE_NON_FUEL_RATE,                 & !  70
                        IN_FUEL_NON_ADJ_RATE,                  & !  71
                        IN_BASE_NON_FUEL_RATE &
                           + IN_FUEL_NON_ADJ_RATE,             & !  72
                        IL_BASE_NON_FUEL_RATE,                 & !  73
                        IL_FUEL_NON_ADJ_RATE,                  & !  74
                        IL_BASE_NON_FUEL_RATE &
                           + IL_FUEL_NON_ADJ_RATE,             & !  75
                     QRTRLY_RUNNING_TOTAL_NONFL_COST(MO), & !  76
                     QUARTERLY_NONFUEL_RATE_ADJ,               & !  77
                     BASE_NONFUEL_RATE(MO),                    & !  78
                     QRT_MOVING_NONFUEL_COLL_RATE,       & !  79
                     QRT_NONFUEL_RECONCILIATION_ADJ(MO),       & !  80
                     QUARTERLY_NONFUEL_RATE_ADJ &
                         + QRT_NONFUEL_RECONCILIATION_ADJ(MO), & !  81
                     NONFUEL_TRACKER_SALES(MO), &
                     MBR_SALES_NONFL_QRT_RUNNING_SUM,     & !  83
                     MEMBER_SALES_NONFUEL_QRT_SUM,             & !  84
                     NON_FUEL_TRACKER_SALES_IN, &                ! 85
!
! PSCR
!
                 WVPA_TOTAL_PSCR_COST(MO),                 & !  86
                 AVERAGE_QUARTERLY_PSCR_RATE(MO),          & !  87
                 QRTRLY_RUNNING_TOTAL_PSCR_COST(MO),    & !  88
                 MI_SALES(MO),                             & !  89
                 MI_BASE_FUEL_DOLLARS(MO),                 & !  90
                 MI_FUEL_ADJ_DOLLARS(MO),                  & !  91
                 MI_NONFUEL_ADJ_DOLLARS(MO),               & !  92
                 MI_BASE_NONFUEL_DOLLARS(MO),              & !  93
                 PSCR_BASE_FUEL_RATE,                      & !  94
                 PSCR_BASE_FUEL_ADJ_RATE,                  & !  95
                 PSCR_BASE_FUEL_RATE &
                    + PSCR_BASE_FUEL_ADJ_RATE,             & !  96
                 WVPA_WHOLESALE_REVENUE(MO),               & !  97
                 MI_BASE_FUEL_DOLLARS(MO) &
                    + MI_FUEL_ADJ_DOLLARS(MO),             & !  98
                 PSCR_ACTUAL_FUEL_COST(MO),                & !  99
                PSCR_PRIOR_YEAR_RECONC_AMOUNT(MO), & !  100
                INCOME_PSCR_STATEMENT_EFFECT(MO),          & !  101
                UNCOLLECTED_PSCR_BALANCE,                  & !  102
                CLOS_UNCOLLECTED_PSCR_BALANCE,          & !  103
                PSCR_COLLECTION_AMOUNTS(MO),               & !  104
                1000000.*WVPA_TOTAL_PSCR_COST(MO)/ &
                                   TOTAL_MEMBER_SALES(MO), & !  105
                NEW_PSCR_ADJ_RATE,                         & !  106
                WVPA_OTHER_NON_FUEL_COST(MO),              & !  107
                WVPA_OTHER_PSCR_COST(MO),                  & !  108
                WVPA_OTHER_FUEL_COST(MO),                  & !  109
                WVPA_PURCHASES_FUEL_COST(MO,PSCROnly) &
                + WVPA_PURCHASES_FUEL_COST(MO,BothIndianaPSCR),   & !  110
                WVPA_GENERATION_FUEL_COST(MO,PSCROnly) &
                + WVPA_GENERATION_FUEL_COST(MO,BothIndianaPSCR),   & !  111
                WVPA_SERVICES_FUEL_COST(MO,PSCROnly) &
                + WVPA_SERVICES_FUEL_COST(MO,BothIndianaPSCR),     & !  112
                WVPA_PURCHASES_NON_FUEL_COST(MO,PSCROnly) &
                +WVPA_PURCHASES_NON_FUEL_COST(MO,BothIndianaPSCR), & !  113
                WVPA_SERVICES_NON_FUEL_COST(MO,PSCROnly) &
                + WVPA_SERVICES_NON_FUEL_COST(MO,BothIndianaPSCR), & !  114
                WVPA_DEMAND_NON_FUEL_COST(MO,PSCROnly) &
                + WVPA_DEMAND_NON_FUEL_COST(MO,BothIndianaPSCR),   & !  115
                WVPA_DERIVATIVE_NON_FUEL_COST(MO,BothIndianaPSCR)  & !
                + WVPA_DERIVATIVE_NON_FUEL_COST(MO,PSCROnly),       & !  116
                WVPA_DERIVATIVE_FUEL_COST(MO,IndianaOnly) &
                + WVPA_DERIVATIVE_FUEL_COST(MO,BothIndianaPSCR),   & !  117
                WVPA_DERIVATIVE_NON_FUEL_COST(MO,BothIndianaPSCR) &
                + WVPA_DERIVATIVE_NON_FUEL_COST(MO,IndianaOnly),    & !  118
                WVPA_DERIVATIVE_FUEL_COST(MO,PSCROnly) &
                + WVPA_DERIVATIVE_FUEL_COST(MO,BothIndianaPSCR),   & !  119
                WVPA_ACTUAL_FUEL_COST(MO,IndianaOnly) &
                + WVPA_ACTUAL_FUEL_COST(MO,BothIndianaPSCR),       & !  120
                WVPA_ACTUAL_NON_FUEL_COST(MO,IndianaOnly) &
                + WVPA_ACTUAL_NON_FUEL_COST(MO,BothIndianaPSCR),   & !  121
                WVPA_ACTUAL_NON_FUEL_COST(MO,PSCROnly) &
                + WVPA_ACTUAL_NON_FUEL_COST(MO,BothIndianaPSCR),   & !  122
                WVPA_ACTUAL_FUEL_COST(MO,PSCROnly) &
                + WVPA_ACTUAL_FUEL_COST(MO,BothIndianaPSCR)         & !  123
                - WVPA_PSCR_AJD_2_FUEL_COST(MO), &
                MI_BASE_FUEL_RATE,                                  & !  124
                MI_FUEL_ADJ_RATE,                                   & !  125
                MI_BASE_FUEL_RATE + MI_FUEL_ADJ_RATE,               & !  126                    ! 125
                MI_BASE_NON_FUEL_RATE,                              & !  127
                MI_FUEL_NON_ADJ_RATE,                               & !  128
                MI_BASE_NON_FUEL_RATE + MI_FUEL_NON_ADJ_RATE,       & !  129                    ! 125
                MI_NONFUEL_ADJ_DOLLARS(MO) &
                + MI_BASE_NONFUEL_DOLLARS(MO),                      & !  130
                MI_ACTUAL_NON_FUEL_COST(MO)                         ! 131
!
            WVPA_TRACKER_REC = WVPA_TRACKER_REC + 1
            UNCOLLECTED_FUEL_BALANCE = CLOSING_UNCOLLECTED_FL_BALANCE
            UNCOLLECTED_NON_FUEL_BALANCE = &
                                    CLOS_UNCOLLECTED_NONFL_BALANCE
            UNCOLLECTED_PSCR_BALANCE = CLOS_UNCOLLECTED_PSCR_BALANCE
         ENDDO
         BEG_UNCOLLECTED_FUEL_BALANCE = &
                                        CLOSING_UNCOLLECTED_FL_BALANCE
         BEG_UNCOLLECTED_NONFL_BALANCE = &
                                    CLOS_UNCOLLECTED_NONFL_BALANCE
         BEG_UNCOLLECTED_PSCR_BALANCE = &
                                        CLOS_UNCOLLECTED_PSCR_BALANCE
!         NEW_PSCR_ADJ_RATE = NEW_PSCR_ADJ_RATE/1000.
!         VOID_LOGICAL = WVPA_UPDATE_PSCR_ADJ_RATES(NEW_PSCR_ADJ_RATE)
!         CALL WVPA_ZERO_ACTUAL_DATA_BASE()
      RETURN

      ENTRY WVPA_MEMBER_ACCRUED_REVS(R_ACCRUED_REVENUES)

         RETURN
         R_ACCRUED_REVENUES = R_ACCRUED_REVENUES &
                              + INCOME_FUEL_STATEMENT_EFFECT(0)         & !  10
                              + INCOME_NON_FL_STATEMENT_EFFECT(0)     ! 66

         WVPA_MEMBER_ACCRUED_REVS = .TRUE.
      RETURN

      ENTRY WVPA_MNTHLY_MEMBER_ACCRUED_REVS(R_MONTHLY_ACCRUED_REVENUES)

         RETURN
         R_MONTHLY_ACCRUED_REVENUES(:) = R_MONTHLY_ACCRUED_REVENUES(:) &
                              + INCOME_FUEL_STATEMENT_EFFECT(:)         & !  10
                              + INCOME_NON_FL_STATEMENT_EFFECT(:)     ! 66

         WVPA_MNTHLY_MEMBER_ACCRUED_REVS = .TRUE.
      RETURN
      END

      FUNCTION WVPA_TRACKING_TYPE(R_WVPA_TRACKING_TYPE)
      use mod_base_year
      implicit none
         INTEGER(kind=2) :: WVPA_TRACKING_TYPE
         CHARACTER*(*) R_WVPA_TRACKING_TYPE
         CHARACTER(len=1) :: CHECK_VALUE
!
         CHECK_VALUE = R_WVPA_TRACKING_TYPE
         SELECT CASE(CHECK_VALUE)
!
            CASE ('P') ! PSCR Only
               WVPA_TRACKING_TYPE = 1
            CASE ('B') ! Both IN & PSCR
               WVPA_TRACKING_TYPE = 2
            CASE ('S') ! SDI
               WVPA_TRACKING_TYPE = 3
            CASE ('I') ! Indiana Only
               WVPA_TRACKING_TYPE = 4
            CASE DEFAULT ! CASE ('N')
               WVPA_TRACKING_TYPE = 5
         END SELECT
      RETURN
      END

      FUNCTION WVPA_RESOURCE_TRACKING_TYPE( &
                                          R_WVPA_RESOURCE_TRACKING_TYPE)

!
         INTEGER(kind=2) :: WVPA_RESOURCE_TRACKING_TYPE
         CHARACTER(len=*) R_WVPA_RESOURCE_TRACKING_TYPE
         CHARACTER(len=1) :: CHECK_VALUE
!
         CHECK_VALUE = R_WVPA_RESOURCE_TRACKING_TYPE
         SELECT CASE(CHECK_VALUE)
!
            CASE ('L') ! LONG-TERM
               WVPA_RESOURCE_TRACKING_TYPE = 1
            CASE ('S') ! SHORT-TERM
               WVPA_RESOURCE_TRACKING_TYPE = 2
            CASE ('T') ! TRANSMISSION
               WVPA_RESOURCE_TRACKING_TYPE = 3
            CASE DEFAULT ! CASE ('N')
               WVPA_RESOURCE_TRACKING_TYPE = 4
         END SELECT
      RETURN
      END

      FUNCTION WVPA_FUEL_TRACKING_TYPE(R_WVPA_FUEL_TRACKING_TYPE)

!
         INTEGER(kind=2) :: WVPA_FUEL_TRACKING_TYPE
         CHARACTER*(*) R_WVPA_FUEL_TRACKING_TYPE
         CHARACTER(len=1) :: CHECK_VALUE
!
         CHECK_VALUE = R_WVPA_FUEL_TRACKING_TYPE
         SELECT CASE(CHECK_VALUE)
!
            CASE ('I') ! INTERNAL
               WVPA_FUEL_TRACKING_TYPE = 1
            CASE ('P') ! PURCHASE & INTERCHANGE
               WVPA_FUEL_TRACKING_TYPE = 2
            CASE ('F') ! FIRM
               WVPA_FUEL_TRACKING_TYPE = 3
            CASE DEFAULT ! CASE ('N')
               WVPA_FUEL_TRACKING_TYPE = 4
         END SELECT
      RETURN
      END

      FUNCTION WVPA_MEM_TRACKING_TYPE(R_WVPA_MEM_TRACKING_TYPE)

!
         INTEGER(kind=2) :: WVPA_MEM_TRACKING_TYPE
         CHARACTER*(*) R_WVPA_MEM_TRACKING_TYPE
         CHARACTER(len=1) :: CHECK_VALUE
!
         CHECK_VALUE = R_WVPA_MEM_TRACKING_TYPE
         SELECT CASE(CHECK_VALUE)
!
            CASE ('M') ! MEMBER
               WVPA_MEM_TRACKING_TYPE = 1
            CASE ('N') ! NON-MEMBER
               WVPA_MEM_TRACKING_TYPE = 2
            CASE DEFAULT !
               WVPA_MEM_TRACKING_TYPE = 0 ! CHANGED FORM 1 1/19/05 MSG
         END SELECT
      RETURN
      END

      SUBROUTINE WVPA_PRC_ACTUAL_PURCHASED_POWER()
        use mod_base_year
        use mthnmcom

      INTEGER(kind=2) :: R_MO,DELETE,GET_MONTH_NUMBER,REC_LOC, &
                R_BASE_DATE,R_YR,MO1,MO_ADDER
      INTEGER :: IOS,I,ACTIVE_RECS,R_SERVICE_ID_NUMBER
      REAL :: R_SEASONAL_ENERGY, &
           R_SEASONAL_CAPACITY, &
           R_SEASONAL_ENERGY_CHARGE, &
           R_SEASONAL_CAPACITY_CHARGE, &
           R_RPT_ENERGY_CHARGE, &
           R_RPT_CAPACITY_CHARGE, &
           R_SEASONAL_FUEL_CHARGE, &
           R_RPT_FUEL_CHARGE, &
           R_ANNU_OFF_SYS_SALES_RECEIVABLE, &
           R_ANNU_OFF_SYS_SALES_PAYABLE
      CHARACTER(len=256) :: FILE_NAME
      CHARACTER(len=1024) :: RECLN
      CHARACTER (LEN=256) :: WVPA_ACTUAL_PURCHASES_FILE
      LOGICAL, SAVE :: FILE_EXISTS
      CHARACTER (LEN=50) :: VARIABLES(15)
      CHARACTER (LEN=4) :: LAST_MONTH_STR
      CHARACTER (LEN=20) :: TRACKER_TAG_STR, &
                            RESOUCE_TYPE
      REAL (KIND=4), ALLOCATABLE, DIMENSION(:,:,:),SAVE :: ACTUAL_VALUES
      REAL (KIND=4), ALLOCATABLE, DIMENSION(:,:,:), SAVE :: &
                                                    TOTAL_MONTHLY_VALUES
      REAL (KIND=4),SAVE:: INCOME_MONTHLY_VALUES(12,4), &
                          FORECAST_MBR_PURCHASE_PWR_COST(0:12), &
                          FORECAST_NONMBR_PUR_PWR_COST(0:12), &
                          FORECAST_MEMBER_SERVICES_COST(0:12), &
                          FORECAST_NONMBR_SERVICES_COST(0:12)
      INTEGER (KIND=4), ALLOCATABLE, DIMENSION(:,:), SAVE :: ID_NUMBER
      INTEGER (KIND=4), SAVE :: MIN_ID_NUM, &
                                MAX_ID_NUM
      INTEGER (KIND=2), SAVE :: RUN_YEAR,SAVE_BASE_DATE
      LOGICAL (KIND=1), SAVE :: DONT_PASS_TO_ASSET_CLASS
      INTEGER (KIND=2) :: HISTORICAL_PRODUCTION_DATE,LAST_MONTH
      INTEGER (KIND=4) :: TEMP_ID_NUMBER
      REAL (KIND=4) :: TEMP_ACTUAL_VALUES(7)
      INTEGER(kind=2), PARAMETER :: Fuel=1, &
                              NonFuel=2, &
                              FixedCost=3, &
                              MemberPurchases=4
      INTEGER(kind=2) :: R_COST_TYPE,TRACKER_TAG,WVPA_TRACKING_TYPE
      REAL(kind=4) :: R_ACTUAL_VALUES(0:12,0:5),TOTAL_COSTS, &
             REVENUES_FROM_NON_MEMBER_SALES(0:12), &
             NON_MEMBER_POWER_COST(0:12), &
             R_MONTHLY_REVENUE(0:12), &
             R_MONTHLY_SALES_EXPENSE(0:12), &
             R_MONTHLY_SALES_MARGIN(0:12), &
             R_INCOME_VARIABLES(0:12,*), &
             R_CASH_VARIABLES(0:12,*)
      REAL (KIND=4) :: R_SECONDARY_SALES_REVENUES, &
                       R_WVPA_NON_MEMBER_COST_OF_POWER, &
                       R_WVPA_MEMBER_COST_OF_POWER, &
                       R_PURCHASE_POWER_COSTS, &
                       WVPA_MBER_COST_OF_SERVICES, &
                       WVPA_NONMBR_COST_OF_SERVICES
      INTEGER(kind=2) :: BASE_DATE,R_RUN_YEAR,MO
      REAL (KIND=4) ::  WVPA_WHOLESALE_FUEL_COST(0:12), &
                        WVPA_WHOLESALE_NONFUEL_COST(0:12), &
                        WVPA_WHOLESALE_REVENUE(0:12), &
                        WVPA_PSCR_AJD_2_FUEL_COST(0:12)
      CHARACTER(len=20) :: CASH_LAG_PATTERN
      CHARACTER(len=30) :: LAGGED_PATTERN
      REAL (KIND=4) :: CASH_LAG_PERCENTAGES(12), &
                       CASH_FROM_NON_MEMBER_SALES(24), &
                       CASH_NON_MEMBER_POWER_COST(24), &
                       CASH_DISTRIBUTION(12), &
                       R_ACTUAL_CASH_VALUES(12,6), &
                       OFF_SYSTEM_SALES_ACT_REC(0:12), &
                       OFF_SYSTEM_SALES_ACT_PAY(0:12), &
                       INTERNAL_PWR_COSTS_PAYABLE_PAY(0:12), &
                       R_OFF_SYSTEM_SALES_RECEIVABLE(0:12), &
                       R_OFF_SYSTEM_SALES_PAYABLE(0:12), &
                       R_INT_POWER_COSTS_PAYABLE_PAY(0:12)
      REAL (KIND=4), SAVE :: TOTAL_MONTHLY_CASH(24,5)=0., &
                             CASH_MONTHLY_VALUES(24,5)=0.
!
         FILE_NAME = WVPA_ACTUAL_PURCHASES_FILE()
         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
         IF(FILE_EXISTS) THEN
            IF(ALLOCATED(ACTUAL_VALUES)) DEALLOCATE(ACTUAL_VALUES, &
                                                   TOTAL_MONTHLY_VALUES, &
                                                   ID_NUMBER)
            ALLOCATE(ACTUAL_VALUES(12,7,200), &
                     TOTAL_MONTHLY_VALUES(12,0:5,3), &
                     ID_NUMBER(12,200))
            OPEN(10,FILE=FILE_NAME)
            READ(10,*) DELETE
            ACTUAL_VALUES = 0.
            LAST_MONTH = -1
            ID_NUMBER = 0
            TOTAL_MONTHLY_VALUES = 0.
            REVENUES_FROM_NON_MEMBER_SALES = 0.
            NON_MEMBER_POWER_COST = 0.
            CASH_FROM_NON_MEMBER_SALES = 0.
            INCOME_MONTHLY_VALUES = 0.
            MIN_ID_NUM = 999999
            MAX_ID_NUM = 0
            DO
               I = 1
               DO
                  READ(10,'(A)',IOSTAT=IOS) RECLN
                  IF(IOS /=0) EXIT
                  IF(RECLN(1:1) == '7') EXIT
                  RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,' &
                                       //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,' &
                                       //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
                  VARIABLES = ' '
                  TEMP_ACTUAL_VALUES = 0.
                  READ(RECLN,*,IOSTAT=IOS) DELETE,VARIABLES(1), &
                                           TEMP_ACTUAL_VALUES, &
                                           TEMP_ID_NUMBER, & !  9
                                           VARIABLES(2),  & !  Active
                                           LAST_MONTH_STR, &
                                           VARIABLES(3:4), &
                                           TRACKER_TAG_STR, &
                                           VARIABLES(6:8), &
                                           RESOUCE_TYPE,  & !  18
                                           TOTAL_COSTS, &
                                           CASH_LAG_PATTERN
                  IF(IOS /= 0) EXIT
                  IF(INDEX(VARIABLES(2),'Not Active') /= 0) CYCLE
                  IF(TEMP_ID_NUMBER == 100087) CYCLE  ! ENRON AMORTIZATON
                  CASH_LAG_PATTERN="0,100"
                  IF(TEMP_ID_NUMBER == 100032)CASH_LAG_PATTERN="0,0,100"
                  LAGGED_PATTERN=trim(CASH_LAG_PATTERN)//',,,,,,,,,,,'
                  CASH_LAG_PERCENTAGES = 0.
                  READ(LAGGED_PATTERN,*) CASH_LAG_PERCENTAGES
                  CASH_LAG_PERCENTAGES(:) = CASH_LAG_PERCENTAGES(:)/100.
                  LAST_MONTH = GET_MONTH_NUMBER(LAST_MONTH_STR)
                  ID_NUMBER(LAST_MONTH,I) = TEMP_ID_NUMBER
                  IF(TEMP_ID_NUMBER == 100088) THEN  ! NON-MEMBER REVENUES
                     TEMP_ACTUAL_VALUES(5) = -TEMP_ACTUAL_VALUES(5)
                     REVENUES_FROM_NON_MEMBER_SALES(LAST_MONTH) = &
                              REVENUES_FROM_NON_MEMBER_SALES(LAST_MONTH) &
                              + TEMP_ACTUAL_VALUES(5)
                     CASH_DISTRIBUTION(:) = CASH_LAG_PERCENTAGES(:) * &
                                            TEMP_ACTUAL_VALUES(5)
                     CASH_FROM_NON_MEMBER_SALES(LAST_MONTH: &
                                                        LAST_MONTH+11) = &
                              CASH_FROM_NON_MEMBER_SALES(LAST_MONTH: &
                                                          LAST_MONTH+11) &
                              + CASH_DISTRIBUTION(1:12)
                     CYCLE
                  ELSEIF(TEMP_ID_NUMBER == 100089) THEN 
                  ! NON-MEMBER PRODUCTION EXPENSE OF NON-MEMBER REVENUES
                     NON_MEMBER_POWER_COST(LAST_MONTH) = &
                                            SUM(TEMP_ACTUAL_VALUES(3:7))
                     CASH_DISTRIBUTION(:) = CASH_LAG_PERCENTAGES(:) * &
                                            SUM(TEMP_ACTUAL_VALUES(3:7))
                     CASH_NON_MEMBER_POWER_COST(LAST_MONTH: &
                                                        LAST_MONTH+11) = &
                            CASH_NON_MEMBER_POWER_COST(LAST_MONTH: &
                                                          LAST_MONTH+11) &
                            + CASH_DISTRIBUTION(1:12)
                     CYCLE
                  ENDIF
!
! TRACKER ITEMS
!
                  ACTUAL_VALUES(LAST_MONTH,:,I)=TEMP_ACTUAL_VALUES(:)
                  TRACKER_TAG = WVPA_TRACKING_TYPE(TRACKER_TAG_STR)
                  TOTAL_MONTHLY_VALUES(LAST_MONTH,TRACKER_TAG,Fuel) = &
                       TOTAL_MONTHLY_VALUES(LAST_MONTH,TRACKER_TAG,Fuel) &
                       + TEMP_ACTUAL_VALUES(5)
                  IF(TEMP_ID_NUMBER == 100074) TRACKER_TAG = 4
                  TOTAL_MONTHLY_VALUES(LAST_MONTH,TRACKER_TAG,NonFuel) = &
                                TOTAL_MONTHLY_VALUES(LAST_MONTH, &
                                                    TRACKER_TAG,NonFuel) &
                                + TEMP_ACTUAL_VALUES(4) &
                                + SUM(TEMP_ACTUAL_VALUES(6:7))
                  TOTAL_MONTHLY_VALUES(LAST_MONTH, &
                                               TRACKER_TAG,FixedCost) = &
                             TOTAL_MONTHLY_VALUES(LAST_MONTH, &
                                                 TRACKER_TAG,FixedCost) &
                             + TEMP_ACTUAL_VALUES(3)
                  I = I + 1
                  IF(TEMP_ID_NUMBER == 100123) then
                    CYCLE ! BY PASS SDI
                  end if
                  
                  IF(TEMP_ID_NUMBER == 100124) then
                    CYCLE ! BY PASS SDI  TRANSMISSION
                  end if
!
! INCOME AND CASH IMPACTS
!
                  IF(RESOUCE_TYPE(1:1) == 'I') THEN
                     INCOME_MONTHLY_VALUES(LAST_MONTH,Fuel) = &
                                  INCOME_MONTHLY_VALUES(LAST_MONTH,Fuel) &
                                  + TEMP_ACTUAL_VALUES(5)
                     INCOME_MONTHLY_VALUES(LAST_MONTH,NonFuel) = &
                               INCOME_MONTHLY_VALUES(LAST_MONTH,NonFuel) &
                               + TEMP_ACTUAL_VALUES(4) &
                               + SUM(TEMP_ACTUAL_VALUES(6:7))
                     INCOME_MONTHLY_VALUES(LAST_MONTH,FixedCost) = &
                            INCOME_MONTHLY_VALUES(LAST_MONTH,FixedCost) &
                            + TEMP_ACTUAL_VALUES(3)
!
! CASH PIECES
!
                     CASH_DISTRIBUTION(:) = CASH_LAG_PERCENTAGES(:) * &
                                            TEMP_ACTUAL_VALUES(5)
                     CASH_MONTHLY_VALUES(LAST_MONTH:LAST_MONTH+11,Fuel)= &
                           CASH_MONTHLY_VALUES(LAST_MONTH:LAST_MONTH+11, &
                                                                   Fuel) &
                           + CASH_DISTRIBUTION(1:12)
!
                     CASH_DISTRIBUTION = CASH_LAG_PERCENTAGES * &
                               (TEMP_ACTUAL_VALUES(4) &
                                       +  SUM(TEMP_ACTUAL_VALUES(6:7)))
                     CASH_MONTHLY_VALUES(LAST_MONTH:LAST_MONTH+11, &
                                                              NonFuel) = &
                           CASH_MONTHLY_VALUES(LAST_MONTH:LAST_MONTH+11, &
                                                                NonFuel) &
                           + CASH_DISTRIBUTION(1:12)
!
                     CASH_DISTRIBUTION(:) = CASH_LAG_PERCENTAGES(:) * &
                                            TEMP_ACTUAL_VALUES(3)
                     CASH_MONTHLY_VALUES(LAST_MONTH:LAST_MONTH+11, &
                                                            FixedCost)= &
                           CASH_MONTHLY_VALUES(LAST_MONTH:LAST_MONTH+11, &
                                                             FixedCost) &
                           + CASH_DISTRIBUTION(1:12)
!
                  ELSE
                     INCOME_MONTHLY_VALUES(LAST_MONTH,MemberPurchases)= &
                      INCOME_MONTHLY_VALUES(LAST_MONTH,MemberPurchases) &
                      + SUM(TEMP_ACTUAL_VALUES(3:7))
                     CASH_DISTRIBUTION(:) = CASH_LAG_PERCENTAGES(:) * &
                                            SUM(TEMP_ACTUAL_VALUES(3:7))
                     CASH_MONTHLY_VALUES(LAST_MONTH:LAST_MONTH+11, &
                                                     MemberPurchases) = &
                           CASH_MONTHLY_VALUES(LAST_MONTH:LAST_MONTH+11, &
                                                       MemberPurchases) &
                           + CASH_DISTRIBUTION(1:12)
                  ENDIF
               ENDDO
               IF(IOS /= 0) then
                    EXIT
               end if
            ENDDO
            NON_MEMBER_POWER_COST = NON_MEMBER_POWER_COST/1000000.
            NON_MEMBER_POWER_COST(0) = SUM(NON_MEMBER_POWER_COST(1:))
            REVENUES_FROM_NON_MEMBER_SALES = &
                                 REVENUES_FROM_NON_MEMBER_SALES/1000000.
            REVENUES_FROM_NON_MEMBER_SALES(0) = &
                                 SUM(REVENUES_FROM_NON_MEMBER_SALES(1:))
            TOTAL_MONTHLY_CASH = TOTAL_MONTHLY_CASH/1000000.
            CASH_NON_MEMBER_POWER_COST = &
                                     CASH_NON_MEMBER_POWER_COST/1000000.
            CASH_FROM_NON_MEMBER_SALES = &
                                     CASH_FROM_NON_MEMBER_SALES/1000000.
            INCOME_MONTHLY_VALUES = INCOME_MONTHLY_VALUES/1000000.
            CASH_MONTHLY_VALUES = CASH_MONTHLY_VALUES/1000000.
            OFF_SYSTEM_SALES_ACT_REC(1:12) = &
                                    REVENUES_FROM_NON_MEMBER_SALES(1:12) &
                                    - CASH_FROM_NON_MEMBER_SALES(1:12)
            OFF_SYSTEM_SALES_ACT_PAY(1:12) = &
                          NON_MEMBER_POWER_COST(1:12) &
                          - CASH_NON_MEMBER_POWER_COST(1:12) &
                          + INCOME_MONTHLY_VALUES(1:12,MemberPurchases) &
                          - CASH_MONTHLY_VALUES(1:12,MemberPurchases)
            INTERNAL_PWR_COSTS_PAYABLE_PAY(1:12) = &
                          + SUM(INCOME_MONTHLY_VALUES(1:12,1:3),DIM=2)
!     +                    - SUM(CASH_MONTHLY_VALUES(1:12,1:3),DIM=2)
         ENDIF
         ACTIVE_RECS = I - 1
!                  MIN_ID_NUM = MIN(MIN_ID_NUM,TEMP_ID_NUMBER)
!                  MAX_ID_NUM = MAX(MAX_ID_NUM,TEMP_ID_NUMBER)
!         IF(ALLOCATED(ID_POINTER)) DEALLOCATE(ID_POINTER)
!         ALLOCATE(ID_POINTER(MIN_ID_NUM:MAX_ID_NUM))
!         ID_POINTER = -999
!         DO J = 1, I-1
!            ID_POINTER(ID_NUMBER(I)) = I
!         ENDDO
      RETURN

      ENTRY WVPA_OFF_SYSTEM_SALES_ACCRUALS(R_RUN_YEAR, &
                                     R_OFF_SYSTEM_SALES_RECEIVABLE, &
                                     R_OFF_SYSTEM_SALES_PAYABLE, &
                                     R_INT_POWER_COSTS_PAYABLE_PAY)

!
         BASE_DATE = (get_base_year() + R_RUN_YEAR - 1900) * 100
         R_OFF_SYSTEM_SALES_RECEIVABLE = 0.
         R_OFF_SYSTEM_SALES_PAYABLE = 0.
         R_INT_POWER_COSTS_PAYABLE_PAY = 0.
         IF(R_RUN_YEAR ==1) THEN
            DO MO = 1, 12
               IF(BASE_DATE + MO >= HISTORICAL_PRODUCTION_DATE()) EXIT
               R_OFF_SYSTEM_SALES_RECEIVABLE(MO) = &
                                            OFF_SYSTEM_SALES_ACT_REC(MO)
               R_OFF_SYSTEM_SALES_PAYABLE(MO) = &
                                            OFF_SYSTEM_SALES_ACT_PAY(MO)
               R_INT_POWER_COSTS_PAYABLE_PAY(MO) = &
                                    INTERNAL_PWR_COSTS_PAYABLE_PAY(MO)
            ENDDO
         ENDIF
         CALL GET_WVPA_MONTHLY_PAYABLES(R_RUN_YEAR, &
                                     R_OFF_SYSTEM_SALES_RECEIVABLE, &
                                     R_OFF_SYSTEM_SALES_PAYABLE, &
                                     R_INT_POWER_COSTS_PAYABLE_PAY)
         R_OFF_SYSTEM_SALES_RECEIVABLE(0) = &
                                SUM(R_OFF_SYSTEM_SALES_RECEIVABLE(1:12))
         R_OFF_SYSTEM_SALES_PAYABLE(0) = &
                                   SUM(R_OFF_SYSTEM_SALES_PAYABLE(1:12))
         R_INT_POWER_COSTS_PAYABLE_PAY(0) = &
                           SUM(R_INT_POWER_COSTS_PAYABLE_PAY(1:12))
      RETURN

      ENTRY WVPA_ANNU_OFF_SYS_SALES_ACCS(R_RUN_YEAR, &
                                   R_ANNU_OFF_SYS_SALES_RECEIVABLE, &
                                   R_ANNU_OFF_SYS_SALES_PAYABLE)

         BASE_DATE = (get_base_year() + R_RUN_YEAR - 1900) * 100
         DO MO = 1, 12
            IF(BASE_DATE + MO >= HISTORICAL_PRODUCTION_DATE()) EXIT
            R_ANNU_OFF_SYS_SALES_RECEIVABLE = &
                                    R_ANNU_OFF_SYS_SALES_RECEIVABLE &
                                    + OFF_SYSTEM_SALES_ACT_REC(MO)
            R_ANNU_OFF_SYS_SALES_PAYABLE = &
                                  R_ANNU_OFF_SYS_SALES_PAYABLE &
                                  + OFF_SYSTEM_SALES_ACT_PAY(MO) &
                                  + INTERNAL_PWR_COSTS_PAYABLE_PAY(MO)
         ENDDO
      RETURN

      ENTRY WVPA_STORE_CURRENT_DATE(R_YR,R_BASE_DATE)

         SAVE_BASE_DATE  = (R_BASE_DATE + R_YR -1900) * 100
         RUN_YEAR = R_YR
!        DO MO = 1, 12
!           IF(SAVE_BASE_DATE + MO >= HISTORICAL_PRODUCTION_DATE()) EXIT
!        ENDDO
      RETURN

      ENTRY WVPA_ZERO_ACTUAL_DATA_BASE

         IF(ALLOCATED(TOTAL_MONTHLY_VALUES))TOTAL_MONTHLY_VALUES = 0.
         REVENUES_FROM_NON_MEMBER_SALES = 0.
         NON_MEMBER_POWER_COST = 0.
         CASH_FROM_NON_MEMBER_SALES = 0.
         INCOME_MONTHLY_VALUES = 0.
         INTERNAL_PWR_COSTS_PAYABLE_PAY = 0.
      RETURN

      ENTRY GET_ACTUAL_DATABASE(R_COST_TYPE,R_ACTUAL_VALUES)

         IF(ALLOCATED(TOTAL_MONTHLY_VALUES)) THEN
            R_ACTUAL_VALUES(1:,:) = R_ACTUAL_VALUES(1:,:) &
                               + TOTAL_MONTHLY_VALUES(1:,:,R_COST_TYPE)/ &
                                                1000000.
            R_ACTUAL_VALUES(0,:) = SUM(R_ACTUAL_VALUES(1:12,:),DIM=1)
            TOTAL_MONTHLY_VALUES(:,:,R_COST_TYPE) = 0.
         ENDIF
      RETURN

      ENTRY WVPA_GET_ACTUAL_CASH_DATABASE(R_ACTUAL_CASH_VALUES)

         IF(RUN_YEAR >= 2) THEN
            MO_ADDER = 12
         ELSE
            MO_ADDER = 0
         ENDIF
         DO MO = 1, 12
            IF(SAVE_BASE_DATE + MO >= HISTORICAL_PRODUCTION_DATE()) EXIT
            MO1 = MO + MO_ADDER
            R_ACTUAL_CASH_VALUES(MO,1:4) = CASH_MONTHLY_VALUES(MO1,1:4)
            R_ACTUAL_CASH_VALUES(MO,5) = CASH_FROM_NON_MEMBER_SALES(MO1)
            R_ACTUAL_CASH_VALUES(MO,6) = CASH_NON_MEMBER_POWER_COST(MO1)
        ENDDO
!        CALL WVPA_CASH_PAYMENTS_RECEIPTS(RUN_YEAR,R_ACTUAL_CASH_VALUES)
      RETURN

      ENTRY GET_ACTUAL_SALES_INFO(R_MONTHLY_REVENUE, &
                                  R_MONTHLY_SALES_EXPENSE)

         R_MONTHLY_REVENUE(:) = REVENUES_FROM_NON_MEMBER_SALES(:)
         R_MONTHLY_SALES_EXPENSE(:) = NON_MEMBER_POWER_COST(:)
      RETURN

      ENTRY WVPA_NON_MEMBER_SALES(R_RUN_YEAR, &
                                  R_SECONDARY_SALES_REVENUES, &
                                  R_WVPA_NON_MEMBER_COST_OF_POWER, &
                                  R_WVPA_MEMBER_COST_OF_POWER, &
                                  R_PURCHASE_POWER_COSTS, &
                                  WVPA_MBER_COST_OF_SERVICES, &
                                  WVPA_NONMBR_COST_OF_SERVICES)


         CALL GET_MM_MEMBER_DATABASE(INT(1,2),1_2,  & !  MEMBER
                                    FORECAST_MBR_PURCHASE_PWR_COST)
         CALL GET_MM_MEMBER_DATABASE(INT(2,2),1_2,  & !  NON MEMBER
                                FORECAST_NONMBR_PUR_PWR_COST)
!
! SERVICE TRANSACTIONS
!
         CALL GET_MM_MEMBER_DATABASE(INT(1,2),4_2,  & !  MEMBER
                                    FORECAST_MEMBER_SERVICES_COST)
         CALL GET_MM_MEMBER_DATABASE(INT(2,2),4_2,  & !  NON MEMBER
                                FORECAST_NONMBR_SERVICES_COST)

         DO MO = 1, 12
            IF(SAVE_BASE_DATE + MO >= HISTORICAL_PRODUCTION_DATE()) THEN ! EXIT
               R_WVPA_NON_MEMBER_COST_OF_POWER = &
                           R_WVPA_NON_MEMBER_COST_OF_POWER &
                           + FORECAST_NONMBR_PUR_PWR_COST(MO)
               R_WVPA_MEMBER_COST_OF_POWER = R_WVPA_MEMBER_COST_OF_POWER &
                               + FORECAST_MBR_PURCHASE_PWR_COST(MO)
               WVPA_MBER_COST_OF_SERVICES = &
                                     WVPA_MBER_COST_OF_SERVICES &
                                     + FORECAST_MEMBER_SERVICES_COST(MO)
               WVPA_NONMBR_COST_OF_SERVICES = &
                                 WVPA_NONMBR_COST_OF_SERVICES &
                                 + FORECAST_NONMBR_SERVICES_COST(MO)
            ELSE
               R_SECONDARY_SALES_REVENUES = R_SECONDARY_SALES_REVENUES &
                                    + REVENUES_FROM_NON_MEMBER_SALES(MO)
               R_WVPA_NON_MEMBER_COST_OF_POWER = &
                                         R_WVPA_NON_MEMBER_COST_OF_POWER &
                                         + NON_MEMBER_POWER_COST(MO)
               R_WVPA_MEMBER_COST_OF_POWER = R_WVPA_MEMBER_COST_OF_POWER &
                            + INCOME_MONTHLY_VALUES(MO,MemberPurchases)
               R_PURCHASE_POWER_COSTS = R_PURCHASE_POWER_COSTS &
                            + NON_MEMBER_POWER_COST(MO) &
                            + INCOME_MONTHLY_VALUES(MO,MemberPurchases)
            ENDIF
         ENDDO
      RETURN

      ENTRY WVPA_MONTHLY_NON_MEMBER_SALES(R_INCOME_VARIABLES, &
                                          R_CASH_VARIABLES)

         IF(RUN_YEAR >= 2) THEN
            MO_ADDER = 12
         ELSE
            MO_ADDER = 0
         ENDIF
         DO MO = 1, 12
            IF(SAVE_BASE_DATE + MO >= HISTORICAL_PRODUCTION_DATE()) THEN ! EXIT
               R_INCOME_VARIABLES(MO, &
                                   WVPA_pwr_cost_4_non_mbrs) = &
                     R_INCOME_VARIABLES(MO, &
                                     WVPA_pwr_cost_4_non_mbrs) &
                     + FORECAST_NONMBR_PUR_PWR_COST(MO)

               R_INCOME_VARIABLES(MO,wvpa_pwr_cost_4_mbrs) = &
                   R_INCOME_VARIABLES(MO,wvpa_pwr_cost_4_mbrs) &
                   + FORECAST_MBR_PURCHASE_PWR_COST(MO)
               R_INCOME_VARIABLES(MO,WVPA_Member_Cost_of_Services) = &
                     R_INCOME_VARIABLES(MO,WVPA_Member_Cost_of_Services) &
                     + FORECAST_MEMBER_SERVICES_COST(MO)
               R_INCOME_VARIABLES(MO,wvpa_non_mbr_svcs_cost) = &
                 R_INCOME_VARIABLES(MO,wvpa_non_mbr_svcs_cost) &
                 + FORECAST_NONMBR_SERVICES_COST(MO)
            ELSE
               R_INCOME_VARIABLES(MO,WVPA_NonMember_Power_Sales) = &
                       R_INCOME_VARIABLES(MO,WVPA_NonMember_Power_Sales) &
                       + REVENUES_FROM_NON_MEMBER_SALES(MO)
               R_INCOME_VARIABLES(MO, &
                                   WVPA_pwr_cost_4_non_mbrs) = &
                     R_INCOME_VARIABLES(MO, &
                                     WVPA_pwr_cost_4_non_mbrs) &
                     + NON_MEMBER_POWER_COST(MO)
               R_INCOME_VARIABLES(MO,monthly_purchased_power) = &
                          R_INCOME_VARIABLES(MO,monthly_purchased_power) &
                          + NON_MEMBER_POWER_COST(MO) &
                          + INCOME_MONTHLY_VALUES(MO,MemberPurchases)

               R_INCOME_VARIABLES(MO,wvpa_pwr_cost_4_mbrs) = &
                   R_INCOME_VARIABLES(MO,wvpa_pwr_cost_4_mbrs) &
                   + INCOME_MONTHLY_VALUES(MO,MemberPurchases)
               R_INCOME_VARIABLES(MO,Monthly_Fossil_Fuel) = &
                              R_INCOME_VARIABLES(MO,Monthly_Fossil_Fuel) &
                              + INCOME_MONTHLY_VALUES(MO,Fuel)
               R_INCOME_VARIABLES(MO,monthly_variable_oandm) = &
                           R_INCOME_VARIABLES(MO,monthly_variable_oandm) &
                           + INCOME_MONTHLY_VALUES(MO,NonFuel)
               R_INCOME_VARIABLES(MO,monthly_fixed_oandm) = &
                              R_INCOME_VARIABLES(MO,monthly_fixed_oandm) &
                              + INCOME_MONTHLY_VALUES(MO,FixedCost)
!
! CASH
!

               R_CASH_VARIABLES(MO,wvpa_cash_nonmbr_pwr_cost) = &
                  R_CASH_VARIABLES(MO,wvpa_cash_nonmbr_pwr_cost) &
                  + CASH_NON_MEMBER_POWER_COST(MO+MO_ADDER)
               R_CASH_VARIABLES(MO, &
                                 wvpa_cash_fm_nonmbr_pwr_sales) = &
                     R_CASH_VARIABLES(MO, &
                                   wvpa_cash_fm_nonmbr_pwr_sales) &
                     + CASH_FROM_NON_MEMBER_SALES(MO+MO_ADDER)
               R_CASH_VARIABLES(MO,WVPA_Cash_Member_Cost_of_Power) = &
                   R_CASH_VARIABLES(MO,WVPA_Cash_Member_Cost_of_Power) &
                   + CASH_MONTHLY_VALUES(MO+MO_ADDER,MemberPurchases)
            ENDIF
         ENDDO
!
        CALL WVPA_CASH_PAYMENTS_RECEIPTS(RUN_YEAR,R_CASH_VARIABLES)
!
         R_INCOME_VARIABLES(0,WVPA_NonMember_Power_Sales) = &
                  SUM(R_INCOME_VARIABLES(1:,WVPA_NonMember_Power_Sales))
         R_INCOME_VARIABLES(0,WVPA_pwr_cost_4_non_mbrs) = &
                     SUM(R_INCOME_VARIABLES(1:, &
                                    WVPA_pwr_cost_4_non_mbrs))

         R_INCOME_VARIABLES(0,wvpa_pwr_cost_4_mbrs) = &
                     SUM(R_INCOME_VARIABLES(1:, &
                                        wvpa_pwr_cost_4_mbrs))
         R_INCOME_VARIABLES(0,Monthly_Fossil_Fuel) = &
                         SUM(R_INCOME_VARIABLES(1:,Monthly_Fossil_Fuel))
         R_INCOME_VARIABLES(0,monthly_variable_oandm) = &
                      SUM(R_INCOME_VARIABLES(1:,monthly_variable_oandm))
         R_INCOME_VARIABLES(0,monthly_fixed_oandm) = &
                         SUM(R_INCOME_VARIABLES(1:,monthly_fixed_oandm))
! CASH
         R_CASH_VARIABLES(0,WVPA_Cash_Member_Cost_of_Power) = &
                SUM(R_CASH_VARIABLES(1:,WVPA_Cash_Member_Cost_of_Power))
         R_CASH_VARIABLES(0,wvpa_cash_nonmbr_pwr_cost) = &
             SUM(R_CASH_VARIABLES(1:,wvpa_cash_nonmbr_pwr_cost))
         R_CASH_VARIABLES(0,Cash_Fossil_Fuel) = &
                              SUM(R_CASH_VARIABLES(1:,Cash_Fossil_Fuel))
         R_CASH_VARIABLES(0,Cash_Variable_OandM) = &
                           SUM(R_CASH_VARIABLES(1:,Cash_Variable_OandM))
         R_CASH_VARIABLES(0,Cash_Fixed_OandM) = &
                              SUM(R_CASH_VARIABLES(1:,Cash_Fixed_OandM))
      RETURN

      ENTRY GET_ACTUAL_SALES_MARGINS(R_MONTHLY_SALES_MARGIN)

         R_MONTHLY_SALES_MARGIN(:) = REVENUES_FROM_NON_MEMBER_SALES(:) &
                                     - NON_MEMBER_POWER_COST(:)
      RETURN

      ENTRY WVPA_ACTUAL_WHOLESALE_TRANS(R_RUN_YEAR, &
                                        WVPA_WHOLESALE_FUEL_COST, &
                                        WVPA_WHOLESALE_NONFUEL_COST, &
                                        WVPA_WHOLESALE_REVENUE, &
                                        WVPA_PSCR_AJD_2_FUEL_COST)

         BASE_DATE = (get_base_year() + R_RUN_YEAR - 1900) * 100
         DO MO = 1, 12
            IF(BASE_DATE + MO >= HISTORICAL_PRODUCTION_DATE()) EXIT
            WVPA_WHOLESALE_FUEL_COST(MO)= 0.
            WVPA_WHOLESALE_NONFUEL_COST(MO) = 0.
            WVPA_WHOLESALE_REVENUE(MO) = WVPA_WHOLESALE_FUEL_COST(MO)
            WVPA_PSCR_AJD_2_FUEL_COST(MO) = &
                                      REVENUES_FROM_NON_MEMBER_SALES(MO) &
                                      - NON_MEMBER_POWER_COST(MO)
         ENDDO
      RETURN
      END SUBROUTINE

      SUBROUTINE WVPA_SPECIAL_REPORT_VARIABLES(R_CLASS, &
                                               R_VARIABLES)

      use namescom
      REAL (KIND=4), PARAMETER :: NOT_AVAIL=-999999.
      INTEGER (KIND=2) :: R_CLASS,MO
      REAL (KIND=4) ::  ENERGY_SALES_BY_CLASS(0:12,LAST_INCOME_LINE), &
                        REVENUES_BY_CLASS(0:12,LAST_INCOME_LINE), &
                        REVENUES_W_SDI(0:12), &
                        REVENUES_WO_SDI(0:12), &
                        R_W_SDI, &
                        R_WO_SDI, &
                        R_W_SDI_MONTHLY(0:12), &
                        R_WO_SDI_MONTHLY(0:12), &
                        R_MEMBER_ENRGY_SOLD(0:12), &
                        R_SDI_ENRGY_SOLD(0:12), &
                        WVPA_MEMBER_ENERGY_SALES, & !  751
                        WVPA_SDI_ENERGY_SALES, & !  752
                        MEMBER_MARGIN, & !  753
                        NON_MEMBER_MARGIN, & !  754
                        R_VARIABLES(0:*),  & !  ANNUAL VARIABLES
                        R_MONTHLY_VARIABLES(0:12,*), & !  MONTHLY VARIABLES
                        INTEREST_RETIREMENTS, &
                        LTD_INTEREST_LESS_AFUDC
      REAL (KIND=4), SAVE :: WVPA_Member_Rates_without_SDI(0:12), &
                             WVPA_Member_Rates_with_SDI(0:12), &
                             ENERGY_SOLD_TO_SDI(0:12), &
                             ENERGY_SOLD_WO_SDI(0:12), &
                             ENERGY_SOLD_W_SDI(0:12)
!
            CALL GET_ENERGY_SALES_BY_CLASS(R_CLASS, &
                                           REVENUES_BY_CLASS, &
                                           ENERGY_SALES_BY_CLASS)
            REVENUES_W_SDI(0) = R_VARIABLES(0) &
                                - R_VARIABLES(660) ! Member Accurred
            REVENUES_WO_SDI(0) = REVENUES_W_SDI(0) &
                                 - R_VARIABLES(330) ! Commercial
!
            ENERGY_SOLD_WO_SDI(:) = ENERGY_SALES_BY_CLASS(:,BaseRates) &
                                  + ENERGY_SALES_BY_CLASS(:,Residential) &
                                  + ENERGY_SALES_BY_CLASS(:,Industrial) &
                                  + ENERGY_SALES_BY_CLASS(:,Government) &
                                  + ENERGY_SALES_BY_CLASS(:,Lighting)
            ENERGY_SOLD_TO_SDI(:) = ENERGY_SALES_BY_CLASS(:,Commercial)
            ENERGY_SOLD_TO_SDI(0) = SUM(ENERGY_SOLD_TO_SDI(1:12))
            ENERGY_SOLD_WO_SDI(0) = SUM(ENERGY_SOLD_WO_SDI(1:12))
            ENERGY_SOLD_W_SDI(:) = ENERGY_SOLD_WO_SDI(:) &
                                   + ENERGY_SOLD_TO_SDI(:)
            DO MO = 0, 0
               IF(ENERGY_SOLD_WO_SDI(MO) /= 0.) THEN
                  WVPA_Member_Rates_without_SDI(MO) = 1000000. * &
                              REVENUES_WO_SDI(MO)/ENERGY_SOLD_WO_SDI(MO)
               ELSE
                  WVPA_Member_Rates_without_SDI(MO) = 0.
               ENDIF
               IF(ENERGY_SOLD_W_SDI(MO) /= 0.) THEN
                  WVPA_Member_Rates_with_SDI(MO) = 1000000. * &
                                REVENUES_W_SDI(MO)/ENERGY_SOLD_W_SDI(MO)
               ELSE
                  WVPA_Member_Rates_with_SDI(MO) = 0.
               ENDIF
            ENDDO
!            R_W_SDI = WVPA_Member_Rates_with_SDI(0)
!            R_WO_SDI = WVPA_Member_Rates_without_SDI(0)
!            WVPA_MEMBER_ENERGY_SALES = ENERGY_SOLD_WO_SDI(0)/1000.! 751
!            WVPA_SDI_ENERGY_SALES =  ENERGY_SOLD_TO_SDI(0)/1000.
      RETURN

      ENTRY WVPA_ANNUAL_RISK_VALUES(R_W_SDI,R_WO_SDI, &
                                    WVPA_MEMBER_ENERGY_SALES, & !  751
                                    WVPA_SDI_ENERGY_SALES) ! 752

         R_W_SDI = WVPA_Member_Rates_with_SDI(0)
         R_WO_SDI = WVPA_Member_Rates_without_SDI(0)
         WVPA_MEMBER_ENERGY_SALES = ENERGY_SOLD_WO_SDI(0)/1000.! 751
         WVPA_SDI_ENERGY_SALES =  ENERGY_SOLD_TO_SDI(0)/1000.
      RETURN

      ENTRY WVPA_MONTHLY_RISK_VALUES(R_MONTHLY_VARIABLES)

!
! RESULT VARIABLES FOR RISK ANALYSIS
         ! Total Base Revenues
         REVENUES_W_SDI(:) = R_MONTHLY_VARIABLES(:,29) &
                          - R_MONTHLY_VARIABLES(:,248)! Member Accrued
         REVENUES_WO_SDI(:) = REVENUES_W_SDI(:) &
                  - R_MONTHLY_VARIABLES(:,12)! SDI/Commercial Revenues

         REVENUES_WO_SDI(0) = SUM(REVENUES_WO_SDI(1:12))
         REVENUES_W_SDI(0) = SUM(REVENUES_W_SDI(1:12))
         DO MO = 0, 12
            IF(ENERGY_SOLD_WO_SDI(MO) /= 0.) THEN
               WVPA_Member_Rates_without_SDI(MO) = 1000000. * &
                           REVENUES_WO_SDI(MO)/ENERGY_SOLD_WO_SDI(MO)
            ELSE
               WVPA_Member_Rates_without_SDI(MO) = 0.
            ENDIF
            IF(ENERGY_SOLD_W_SDI(MO) /= 0.) THEN
               WVPA_Member_Rates_with_SDI(MO) = 1000000. * &
                             REVENUES_W_SDI(MO)/ENERGY_SOLD_W_SDI(MO)
            ELSE
               WVPA_Member_Rates_with_SDI(MO) = 0.
            ENDIF
         ENDDO
!
         R_MONTHLY_VARIABLES(:,265) = WVPA_Member_Rates_with_SDI
         R_MONTHLY_VARIABLES(:,266) = WVPA_Member_Rates_without_SDI
         R_MONTHLY_VARIABLES(:,267) = ENERGY_SOLD_WO_SDI/1000.
         R_MONTHLY_VARIABLES(:,268) = ENERGY_SOLD_TO_SDI/1000.
         &  !  NON-MEMBER REVENUES
         R_MONTHLY_VARIABLES(:,270) = R_MONTHLY_VARIABLES(:,3)      &
         & !  NON-MEMBER COST OF POWER
           - R_MONTHLY_VARIABLES(:,255)  & 
         & ! NON-MEMBER SERVICES COSTS
           - R_MONTHLY_VARIABLES(:,261)  
         &                            !  NET INCOME
         R_MONTHLY_VARIABLES(:,269) = R_MONTHLY_VARIABLES(:,156) & 
         &                            ! NON-MEMBER MARGIN
                                      - R_MONTHLY_VARIABLES(:,270)     
      RETURN

      ENTRY WVPA_ANNUAL_MARGINS(R_VARIABLES)

         NON_MEMBER_MARGIN = R_VARIABLES(2) &
                             - R_VARIABLES(735) &
                             - R_VARIABLES(750)
         MEMBER_MARGIN = R_VARIABLES(49) &
                          - NON_MEMBER_MARGIN
         R_VARIABLES(753) = MEMBER_MARGIN
         R_VARIABLES(754) = NON_MEMBER_MARGIN
!
! debt coverage (net income + dep expense + LTD Interest)/
!                 (LTD Interest Paid + Debt Retirements)
         ! LTD_INTEREST_CASH_PAYMENTS
         INTEREST_RETIREMENTS = R_VARIABLES(387)  &
          + R_VARIABLES(119) ! LONG_TERM_DEBT_RETIREMENTS
         IF(INTEREST_RETIREMENTS /= 0.) THEN
            R_VARIABLES(756) = R_VARIABLES(45)       & !  NET_PROFIT_OR_LOSS
                               + R_VARIABLES(293)    & !  LTD_INTEREST_AND_AMORTIZATION
                               + R_VARIABLES(17)     & !  BOOK_DEPRECIATION
                               - R_VARIABLES(41)     ! AFUDC_BORROWED
            R_VARIABLES(756) = R_VARIABLES(756)/INTEREST_RETIREMENTS
         ELSE
            R_VARIABLES(756) = NOT_AVAIL
         ENDIF
!
! TIER Calculation

         !  LTD BOOKED INTEREST WITH AMORT
         LTD_INTEREST_LESS_AFUDC = R_VARIABLES(293)   & 
                                   - R_VARIABLES(41)  ! afudc borrowed
         IF(LTD_INTEREST_LESS_AFUDC /= 0.) THEN
!                             NET_PROFIT_OR_LOSS         
            R_VARIABLES(757)=(LTD_INTEREST_LESS_AFUDC+R_VARIABLES(45))/  &
                                      LTD_INTEREST_LESS_AFUDC
         ELSE
            R_VARIABLES(757) = NOT_AVAIL
         ENDIF
      RETURN
      END SUBROUTINE

      SUBROUTINE WVPA_TRACKER_DATA_BASE()

!
      REAL (KIND=4), SAVE :: MM_DATABASE(0:12, &
                                         0:5,  & ! Type: Purchase, Generation, Derivative, Services, Actuals
                                         0:5,  & ! Tracker: PSCROnly=1, BothIndianaPSCR=2, SDI=3, IndianaOnly=4
                                         0:3)  !Cost: Fuel, Non-Fuel, Demand
      REAL (KIND=4), SAVE :: MM_MEMBER_DATABASE(0:12, &
                                         0:5,  & ! Type: Purchase, Generation, Derivative, Services, Actuals
                                         0:2,  & ! Tracker: Member=1, Non-member=2
                                         0:3)  !Cost: Fuel, Non-Fuel, Demand
      INTEGER (KIND=2) :: R_TYPE,R_COST_TYPE,TRK_LOCATION,ISEAS, &
                          TRK_MEM_LOC
      REAL (KIND=4) :: R_MM_DB_BY_SEASON(0:12,0:5), &
                       R_MM_MEM_DB_BY_SEASON(0:12), &
                       R_FUEL_COST,R_NON_FUEL_COST,R_FIXED_COST, &
                       TEMP_MM_MEM_DB_BY_SEASON(0:12,0:3)
      INTEGER (KIND=2) :: R_SERVICE_TYPE

      INTEGER(kind=2), PARAMETER :: Purchases=1, &
                              Generation=2, &
                              Derivative=3, &
                              Services=4, &
                              FuelCost=1, &
                              NonFuelCost=2, &
                              DemandCost=3, &
                              PSCROnly=1, &
                              BothIndianaPSCR=2, &
                              SDI=3, &
                              IndianaOnly=4

      ENTRY WVPA_INIT_TRACKER_DATE_BASE()

         MM_DATABASE = 0.
         MM_MEMBER_DATABASE = 0.
      RETURN

      ENTRY WVPA_STORE_CL_TRACKER_DATE_BASE(ISEAS, &
                                            R_COST_TYPE, &
                                            TRK_LOCATION, &
                                            R_FUEL_COST, &
                                            R_NON_FUEL_COST, &
                                            R_FIXED_COST, &
                                            TRK_MEM_LOC)


            MM_DATABASE(ISEAS,R_COST_TYPE,TRK_LOCATION,1) = &
                           MM_DATABASE(ISEAS,R_COST_TYPE,TRK_LOCATION,1) &
                           + R_FUEL_COST/1000.
            MM_DATABASE(ISEAS,R_COST_TYPE,TRK_LOCATION,2) = &
                           MM_DATABASE(ISEAS,R_COST_TYPE,TRK_LOCATION,2) &
                           + R_NON_FUEL_COST/1000.
            MM_DATABASE(ISEAS,R_COST_TYPE,TRK_LOCATION,3) = &
                           MM_DATABASE(ISEAS,R_COST_TYPE,TRK_LOCATION,3) &
                           + R_FIXED_COST/1000.

            MM_MEMBER_DATABASE(ISEAS,R_COST_TYPE,TRK_MEM_LOC,1) = &
                    MM_MEMBER_DATABASE(ISEAS,R_COST_TYPE,TRK_MEM_LOC,1) &
                           + R_FUEL_COST/1000.
            MM_MEMBER_DATABASE(ISEAS,R_COST_TYPE,TRK_MEM_LOC,2) = &
                    MM_MEMBER_DATABASE(ISEAS,R_COST_TYPE,TRK_MEM_LOC,2) &
                           + R_NON_FUEL_COST/1000.
            MM_MEMBER_DATABASE(ISEAS,R_COST_TYPE,TRK_MEM_LOC,3) = &
                    MM_MEMBER_DATABASE(ISEAS,R_COST_TYPE,TRK_MEM_LOC,3) &
                           + R_FIXED_COST/1000.
      RETURN

      ENTRY GET_MM_DATABASE(R_TYPE, &
                            R_COST_TYPE, &
                            R_MM_DB_BY_SEASON)

!
         R_MM_DB_BY_SEASON(1:,:) = R_MM_DB_BY_SEASON(1:,:) &
                            + MM_DATABASE(1:,R_TYPE,:,R_COST_TYPE)/1000.
      RETURN

      ENTRY GET_MM_MEMBER_DATABASE(R_TYPE,R_SERVICE_TYPE, &
                                   R_MM_MEM_DB_BY_SEASON)

!
!  NOTE THAT
!
!      REAL (KIND=4), SAVE :: MM_MEMBER_DATABASE(0:12,
!     +                                   0:5,  !Type: Purchase, Generation, Derivative, Services, Actuals
!     +                                   0:2,  !Tracker: Member=1, Non-member=2
!     +                                   0:3)  !Cost: Fuel, Non-Fuel, Demand
         TEMP_MM_MEM_DB_BY_SEASON(1:,:) = &
                          MM_MEMBER_DATABASE(1:,R_SERVICE_TYPE,R_TYPE,:)

         R_MM_MEM_DB_BY_SEASON(1:) = &
                             SUM(TEMP_MM_MEM_DB_BY_SEASON(1:,1:3),DIM=2)

         R_MM_MEM_DB_BY_SEASON(0) = SUM(R_MM_MEM_DB_BY_SEASON(1:))
         R_MM_MEM_DB_BY_SEASON = R_MM_MEM_DB_BY_SEASON/1000.
      RETURN
      END SUBROUTINE

