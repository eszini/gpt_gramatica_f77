
!     Msgmmrev.for
!     Copyright(c) M.S. Gerber & Associates 2000
!
!     Created: 5/16/2003 11:54:14 AM
!     Author : MARK S GERBER



      SUBROUTINE CLASS_SALES_OBJECT
      use end_routine, only: end_program, er_message
!
      use spindriftlib
      use prod_arrays_dimensions
      USE SIZECOM

      INTEGER (KIND=2) :: NUMBER_OF_BC_CLASSES=0,MAX_BC_CLASS_ID_NUM=0
      INTEGER (KIND=2) :: NUMBER_OF_OL_CLASSES=0,MAX_OL_CLASS_ID_NUM=0
      INTEGER (KIND=2) :: INUNIT,IREC,DELETE,LRECL=1042,ASSET_CLASS_NUM
      INTEGER (KIND=4) :: IOS,IOS_BASE
      CHARACTER (len=5) :: OVERLAY_FAMILY_NAME
      CHARACTER (len=10) :: STATE_OR_PROVINCE
      CHARACTER (len=30) :: COMMENT
      CHARACTER (len=256) :: FILE_NAME
      CHARACTER (len=256) :: BASE_FILE_DIRECTORY
      CHARACTER (len=256) :: OUTPUT_DIRECTORY,DATA_DRIVE
      LOGICAL (KIND=4) :: FILE_EXISTS,FILE_OPEN
      CHARACTER (len=5) :: BASE_FILE_NAME,CLASS_SALES_BASE_FILE
      REAL (KIND=4) :: FILE_VALUES(41) ! READS VARIABLES 4 THROUGH 44
      CHARACTER (LEN=1) :: INTRA_COMPANY_TRANSACTION,USE_TRANSACT_FILE
      REAL :: ENERGY_ADJ_RATE(12)    ! 114-125
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER (LEN=1024) :: RECLN
! DECLARATION FOR CLASS FORECASTS
      INTEGER (KIND=2) :: YEAR,DATA_RECORDS_IN_TABLE
      CHARACTER (LEN=32) :: CLASS_NAME,ENERGY_UNITS*3,TEMP_CLASS_NAME

      ! leading space needed
      CHARACTER (LEN=16) :: FILE_TYPE='Revenue Forecast'
      INTEGER (KIND=2) :: SAVE_CLASS_SALES_UNIT,CLASS_SALES_UNIT
      INTEGER (KIND=2) :: BC_TABLE_NUMBER,OL_TABLE_NUMBER
      INTEGER (KIND=2) :: R_NUMBER_OF_TABLES
      SAVE BC_TABLE_NUMBER,OL_TABLE_NUMBER
      CHARACTER (LEN=2) :: CLASS_SALES_OL='BC'
      REAL (KIND=4) :: MONTHLY_PRICE_INFORMATION(36)
      CHARACTER (LEN=1) :: CASH_TREATMENT
      CHARACTER (len=30) :: LAG_PERIOD,CASH_RECEIVABLE_PAYABLE
      REAL (KIND=4) :: ENERGY_LOSS_FACTOR, &
             DEMAND_LOSS_FACTOR, &
             STATE_RPS_ALLOCATION, &
             STATE_RPS_EXEMPTION
!
      CHARACTER (LEN=1) :: DEMAND_PRICING_SWITCH
      CHARACTER (LEN=1) :: OUC_CLASSIFICATIONS
      CHARACTER (len=30) :: OUC_TAX_TREATMENT
      REAL (KIND=4) :: OUC_BASE_EL_STAB_ADDER, &
             OUC_FUEL_STAB_ADDER, &
             OUC_CUST_RETENTION_ADDER, &
             OUC_BASE_WATER_STAB_ADDER, &
             OUC_PLACEHOLDER_FUND_A, &
             OUC_PLACEHOLDER_FUND_B
!
! INTRA-COMPANY DATA
!
      INTEGER (KIND=2) :: INTRA_ASSET_CLASS_ID,INTRA_ASSET_CLASS_ALLOCATION
      CHARACTER (LEN=40) :: INTRA_EXPENSE_CLASSIFICATION, &
                   INTRA_EXPENSE_COLLECTION, &
                   REVENUE_CLASSIFICATION, &
                   CPL_JURISDICTION, &
                   EXPENSE_CLASSIFICATION, &
                   EXPENSE_COLLECTION, &
                   FE_COMPETITIVE_CLASSIFICATION ! 112
      CHARACTER (LEN=20) :: COMMODITY_LHC_DISTRIBUTION, &
                   DEMAND_LHC_DISTRIBUTION, & ! 105
                   CUSTOMER_LHC_DISTRIBUTION  ! 106
      CHARACTER (LEN=1) :: ACCOUNT_TYPE ! 107
      INTEGER (KIND=2) :: TRANSACT_FORECAST_GROUP, & ! 110
                          TRANSACT_CUSTOMER_GROUP ! 111
      INTEGER (KIND=2) :: BC_ASSET_CLASS_POINTER(:), &
                OL_ASSET_CLASS_POINTER(:), &
                TEMP_ASSET_CLASS_POINTER(:)
      ALLOCATABLE :: BC_ASSET_CLASS_POINTER, &
                     OL_ASSET_CLASS_POINTER, &
                     TEMP_ASSET_CLASS_POINTER
      SAVE BC_ASSET_CLASS_POINTER,OL_ASSET_CLASS_POINTER
      INTEGER (KIND=2) :: R_NUM_OF_CLASSES,R_MAX_CLASS_NUM
      CHARACTER (LEN=1) :: ACCOUNT_ACTIVE
      LOGICAL (KIND=1) :: LAHEY_LF95
      CHARACTER (len=30) :: SCREEN_OUTPUT
      INTEGER (KIND=2) :: R_CLASS_POINTERS(R_MAX_CLASS_NUM)
      INTEGER :: RECORDS_IN_OVERLAY_TABLE
      LOGICAL (KIND=1) :: READ_OVERLAY_INPUT
!

!
!          ROUTINE TO CONVERT METAFILE FILES TO DIRECT-ACESS BINARY
!          COPYRIGHT (C) 1983, 84, 85  M.S. GERBER & ASSOCIATES, INC.
!

!
! CONVERT THE CLASS-FORECAST-DATA FILES

      ENTRY CLASS_SALES_MAKEBIN

!
      BASE_FILE_NAME = CLASS_SALES_BASE_FILE()
      FILE_NAME = trim(BASE_FILE_DIRECTORY())// &
                                   "RCB"//trim(BASE_FILE_NAME)//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      DATA_DRIVE = OUTPUT_DIRECTORY()
      IF(FILE_EXISTS) THEN
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
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCCSALES.BIN", &
                           ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         IREC = 0
         BC_TABLE_NUMBER = 0
!
         READ(10,*,IOSTAT=IOS) DELETE
         DO
            FILE_VALUES = 0.
            INTRA_COMPANY_TRANSACTION = 'N'
            ENERGY_UNITS = 'GWh'
            CASH_TREATMENT = 'B'
            LAG_PERIOD = '100'
            CPL_JURISDICTION = 'Wholesale'
            DATA_RECORDS_IN_TABLE = 0
            OUC_CLASSIFICATIONS = 'I'
            OUC_TAX_TREATMENT = 'I'
            OUC_BASE_EL_STAB_ADDER = 0.
            OUC_FUEL_STAB_ADDER = 0.
            OUC_CUST_RETENTION_ADDER = 0.
            OUC_BASE_WATER_STAB_ADDER = 0.
            OUC_PLACEHOLDER_FUND_A = 0.
            OUC_PLACEHOLDER_FUND_B = 0.
            ENERGY_LOSS_FACTOR = 0.
            DEMAND_LOSS_FACTOR = 0.
            EXPENSE_COLLECTION = 'ATL Expense'
            DEMAND_PRICING_SWITCH = 'A'
            ACCOUNT_ACTIVE = 'A'
            COMMODITY_LHC_DISTRIBUTION = 'Not Active'
            DEMAND_LHC_DISTRIBUTION = 'Not Active' ! 105
            CUSTOMER_LHC_DISTRIBUTION = 'Not Active' ! 106
            ACCOUNT_TYPE = 'R' ! 107
            EXPENSE_CLASSIFICATION = " "     ! 108
            EXPENSE_COLLECTION = " "   ! 109
            TRANSACT_FORECAST_GROUP = 0  ! 110
            TRANSACT_CUSTOMER_GROUP = 0 ! 111
            FE_COMPETITIVE_CLASSIFICATION = " " ! 112
            USE_TRANSACT_FILE ='N'
            MONTHLY_PRICE_INFORMATION = 0.
            ENERGY_ADJ_RATE = 0.    ! 114-125
            STATE_OR_PROVINCE = 'AL_45'
            STATE_RPS_ALLOCATION = 100.0
            STATE_RPS_EXEMPTION = 0.0
            BC_TABLE_NUMBER = BC_TABLE_NUMBER + 1
            DO
               CASH_RECEIVABLE_PAYABLE = "0."
               READ(10,1000,IOSTAT=IOS) RECLN
               IF(IOS /= 0) EXIT
               IF(RECLN(1:1) == '7') EXIT
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
                                     ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
                                     ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
                                     ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,ERR=200) DELETE,YEAR,CLASS_NAME,COMMENT,FILE_VALUES,INTRA_COMPANY_TRANSACTION,INTRA_ASSET_CLASS_ID, &
                                  INTRA_ASSET_CLASS_ALLOCATION,INTRA_EXPENSE_CLASSIFICATION,INTRA_EXPENSE_COLLECTION, &
                                  REVENUE_CLASSIFICATION,ENERGY_UNITS,MONTHLY_PRICE_INFORMATION,CASH_TREATMENT,LAG_PERIOD, &
                                  CASH_RECEIVABLE_PAYABLE,CPL_JURISDICTION, & ! 91 FE competitive classes
                                  OUC_CLASSIFICATIONS,OUC_TAX_TREATMENT,OUC_BASE_EL_STAB_ADDER,OUC_FUEL_STAB_ADDER, &
                                  OUC_CUST_RETENTION_ADDER,OUC_BASE_WATER_STAB_ADDER,OUC_PLACEHOLDER_FUND_A, &
                                  OUC_PLACEHOLDER_FUND_B,ENERGY_LOSS_FACTOR,DEMAND_LOSS_FACTOR,DEMAND_PRICING_SWITCH, &
                                  ACCOUNT_ACTIVE,COMMODITY_LHC_DISTRIBUTION, &  ! 104
                                  DEMAND_LHC_DISTRIBUTION, &                    ! 105
                                  CUSTOMER_LHC_DISTRIBUTION, &                  ! 106
                                  ACCOUNT_TYPE, &                               ! 107
                                  EXPENSE_CLASSIFICATION, &                     ! 108
                                  EXPENSE_COLLECTION, &                         ! 109
                                  TRANSACT_FORECAST_GROUP, &                    ! 110
                                  TRANSACT_CUSTOMER_GROUP, &                    ! 111
                                  FE_COMPETITIVE_CLASSIFICATION, &              ! 112
                                  USE_TRANSACT_FILE,ENERGY_ADJ_RATE, &          ! 114-125
                                  STATE_OR_PROVINCE,STATE_RPS_ALLOCATION,STATE_RPS_EXEMPTION  ! 128


               ASSET_CLASS_NUM = INT(FILE_VALUES(4),2)
!
! TRACK ASSET CLASS INFO
!
             IF(.NOT. (DELETE >= 8 .OR. ACCOUNT_ACTIVE == 'N')) THEN
                CALL SET_ASSET_CLASSES(ASSET_CLASS_NUM, &
                                               NUMBER_OF_BC_CLASSES, &
                                               MAX_BC_CLASS_ID_NUM, &
                                               TEMP_ASSET_CLASS_POINTER)
               IF(INDEX(INTRA_COMPANY_TRANSACTION,'Y') /= 0) &
                         CALL SET_ASSET_CLASSES(INTRA_ASSET_CLASS_ID, &
                                               NUMBER_OF_BC_CLASSES, &
                                               MAX_BC_CLASS_ID_NUM, &
                                               TEMP_ASSET_CLASS_POINTER)
            ENDIF
!
! WRITE RECORD
!
               IREC = IREC + 1
               WRITE(11,REC=IREC) DELETE,YEAR,CLASS_NAME, &
                                  FILE_VALUES, &
                                  INTRA_COMPANY_TRANSACTION, &
                                  INTRA_ASSET_CLASS_ID, &
                                  INTRA_ASSET_CLASS_ALLOCATION, &
                                  INTRA_EXPENSE_CLASSIFICATION, &
                                  INTRA_EXPENSE_COLLECTION, &
                                  REVENUE_CLASSIFICATION, &
                                  ENERGY_UNITS, &
                                  MONTHLY_PRICE_INFORMATION, &
                                  CASH_TREATMENT, &
                                  LAG_PERIOD, &
                                  CASH_RECEIVABLE_PAYABLE, &
                                  CPL_JURISDICTION, &
                                  OUC_CLASSIFICATIONS, &
                                  OUC_TAX_TREATMENT, &
                                  OUC_BASE_EL_STAB_ADDER, &
                                  OUC_FUEL_STAB_ADDER, &
                                  OUC_CUST_RETENTION_ADDER, &
                                  OUC_BASE_WATER_STAB_ADDER, &
                                  OUC_PLACEHOLDER_FUND_A, &
                                  OUC_PLACEHOLDER_FUND_B, &
                                  ENERGY_LOSS_FACTOR, &
                                  DEMAND_LOSS_FACTOR, &
                                  DEMAND_PRICING_SWITCH, &
                                  ACCOUNT_ACTIVE, &
                                  COMMODITY_LHC_DISTRIBUTION, &
                                  DEMAND_LHC_DISTRIBUTION, & !  105
                                  CUSTOMER_LHC_DISTRIBUTION, & !  106
                                  ACCOUNT_TYPE, & !  107
                                  EXPENSE_CLASSIFICATION, & !  108
                                  EXPENSE_COLLECTION, & !  109
                                  TRANSACT_FORECAST_GROUP, & !  110
                                  TRANSACT_CUSTOMER_GROUP, & !  111
                                  FE_COMPETITIVE_CLASSIFICATION, & !  112
                                  USE_TRANSACT_FILE, &
                                  ENERGY_ADJ_RATE,    & !  114-125
                                  STATE_OR_PROVINCE, &
                                  STATE_RPS_ALLOCATION, &
                                  STATE_RPS_EXEMPTION  ! 128
               DATA_RECORDS_IN_TABLE = DATA_RECORDS_IN_TABLE + 1
            ENDDO
            IF(DATA_RECORDS_IN_TABLE < AVAIL_DATA_YEARS) THEN
               DO WHILE (DATA_RECORDS_IN_TABLE < AVAIL_DATA_YEARS)
                  IREC = IREC + 1
                  WRITE(11,REC=IREC) DELETE,YEAR,CLASS_NAME,FILE_VALUES
                  DATA_RECORDS_IN_TABLE = DATA_RECORDS_IN_TABLE + 1
               ENDDO
            ENDIF
            IF(IOS /= 0) EXIT
         ENDDO
         CLOSE(11)
         IF(MAX_BC_CLASS_ID_NUM > 0) THEN
            ALLOCATE(BC_ASSET_CLASS_POINTER(MAX_BC_CLASS_ID_NUM))
     
            BC_ASSET_CLASS_POINTER(:) = &
                TEMP_ASSET_CLASS_POINTER(1:MAX_BC_CLASS_ID_NUM)
         ENDIF
         DEALLOCATE(TEMP_ASSET_CLASS_POINTER)
      ELSE IF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "msgmmrev:1117")
      ENDIF
      RETURN

      ENTRY CLASS_SALES_MAKEOVL(OVERLAY_FAMILY_NAME)

         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
            CALL LOCATE(10,51)
         ENDIF
         DATA_DRIVE = OUTPUT_DIRECTORY()
         FILE_NAME = trim(DATA_DRIVE)// &
                             "RCO"//trim(OVERLAY_FAMILY_NAME)//".DAT"
         OPEN(10,FILE=FILE_NAME)
         READ(10,*) DELETE
         INUNIT = 12
         IF(CLASS_SALES_OL == 'BC') THEN
            OPEN(11,FILE=trim(DATA_DRIVE)//"BCCSALES.BIN", &
                                           ACCESS="DIRECT",RECL=LRECL)
            INUNIT = 11
         ENDIF
         OPEN(12,FILE=trim(DATA_DRIVE)//"OLCSALES.BIN", &
                          ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         ALLOCATE(TEMP_ASSET_CLASS_POINTER(1024))
         TEMP_ASSET_CLASS_POINTER = 0
         IREC = 0
         OL_TABLE_NUMBER = 0
         NUMBER_OF_OL_CLASSES = 0
         MAX_OL_CLASS_ID_NUM = 0
         DO
            OL_TABLE_NUMBER = OL_TABLE_NUMBER + 1
            RECORDS_IN_OVERLAY_TABLE = 0
            READ_OVERLAY_INPUT = .TRUE.
            DO
               IF(READ_OVERLAY_INPUT) READ(10,1000,IOSTAT=IOS) RECLN
               IF(RECLN(1:1) == '7' .AND. READ_OVERLAY_INPUT) THEN
                  IF(RECORDS_IN_OVERLAY_TABLE == AVAIL_DATA_YEARS) EXIT
                  READ_OVERLAY_INPUT = .FALSE.
               ENDIF
               IREC = IREC + 1
               READ(INUNIT,REC=IREC,IOSTAT=IOS_BASE) DELETE,YEAR,CLASS_NAME,FILE_VALUES,INTRA_COMPANY_TRANSACTION, &
                                          INTRA_ASSET_CLASS_ID,INTRA_ASSET_CLASS_ALLOCATION,INTRA_EXPENSE_CLASSIFICATION, &
                                          INTRA_EXPENSE_COLLECTION,REVENUE_CLASSIFICATION,ENERGY_UNITS,MONTHLY_PRICE_INFORMATION, &
                                          CASH_TREATMENT,LAG_PERIOD,CASH_RECEIVABLE_PAYABLE,CPL_JURISDICTION,OUC_CLASSIFICATIONS, &
                                          OUC_TAX_TREATMENT,OUC_BASE_EL_STAB_ADDER,OUC_FUEL_STAB_ADDER,OUC_CUST_RETENTION_ADDER, &
                                          OUC_BASE_WATER_STAB_ADDER,OUC_PLACEHOLDER_FUND_A,OUC_PLACEHOLDER_FUND_B, &
                                          ENERGY_LOSS_FACTOR,DEMAND_LOSS_FACTOR,DEMAND_PRICING_SWITCH,ACCOUNT_ACTIVE, &
                                          COMMODITY_LHC_DISTRIBUTION,DEMAND_LHC_DISTRIBUTION, &                       ! 105
                                          CUSTOMER_LHC_DISTRIBUTION, &                                                ! 106
                                          ACCOUNT_TYPE, &                                                             ! 107
                                          EXPENSE_CLASSIFICATION, &                                                   ! 108
                                          EXPENSE_COLLECTION, &                                                       ! 109
                                          TRANSACT_FORECAST_GROUP, &                                                  ! 110
                                          TRANSACT_CUSTOMER_GROUP, &                                                  ! 111
                                          FE_COMPETITIVE_CLASSIFICATION, &                                            ! 112
                                          USE_TRANSACT_FILE,ENERGY_ADJ_RATE, &                                        ! 114-125
                                          STATE_OR_PROVINCE,STATE_RPS_ALLOCATION,STATE_RPS_EXEMPTION                  ! 128
               IF(IOS_BASE /= 0) EXIT
!
!
!
               IF(IOS == 0 .AND. READ_OVERLAY_INPUT) THEN
                  RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
                                       ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
                                       ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
                                       ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
                  READ(RECLN,*,ERR=200) DELETE,YEAR,TEMP_CLASS_NAME,COMMENT,FILE_VALUES,INTRA_COMPANY_TRANSACTION, &
                                  INTRA_ASSET_CLASS_ID,INTRA_ASSET_CLASS_ALLOCATION,INTRA_EXPENSE_CLASSIFICATION, &
                                  INTRA_EXPENSE_COLLECTION,REVENUE_CLASSIFICATION,ENERGY_UNITS,MONTHLY_PRICE_INFORMATION, &
                                  CASH_TREATMENT,LAG_PERIOD,CASH_RECEIVABLE_PAYABLE,CPL_JURISDICTION,OUC_CLASSIFICATIONS, &
                                  OUC_TAX_TREATMENT,OUC_BASE_EL_STAB_ADDER,OUC_FUEL_STAB_ADDER,OUC_CUST_RETENTION_ADDER, &
                                  OUC_BASE_WATER_STAB_ADDER,OUC_PLACEHOLDER_FUND_A,OUC_PLACEHOLDER_FUND_B,ENERGY_LOSS_FACTOR, &
                                  DEMAND_LOSS_FACTOR,DEMAND_PRICING_SWITCH,ACCOUNT_ACTIVE,COMMODITY_LHC_DISTRIBUTION, &
                                  DEMAND_LHC_DISTRIBUTION, &            ! 105
                                  CUSTOMER_LHC_DISTRIBUTION, &          ! 106
                                  ACCOUNT_TYPE, &                       ! 107
                                  EXPENSE_CLASSIFICATION, &             ! 108
                                  EXPENSE_COLLECTION, &                 ! 109
                                  TRANSACT_FORECAST_GROUP, &            ! 110
                                  TRANSACT_CUSTOMER_GROUP, &            ! 111
                                  FE_COMPETITIVE_CLASSIFICATION, &      ! 112
                                  USE_TRANSACT_FILE,ENERGY_ADJ_RATE, &  ! 114-125
                                  STATE_OR_PROVINCE,STATE_RPS_ALLOCATION,STATE_RPS_EXEMPTION  ! 128
               ENDIF
!
! TRACK ASSET CLASS INFO
!
               ASSET_CLASS_NUM = INT(FILE_VALUES(4),2)
               IF(.NOT. (DELETE >= 8 .OR. ACCOUNT_ACTIVE == 'N')) THEN
                   CALL SET_ASSET_CLASSES(ASSET_CLASS_NUM, &
                                                NUMBER_OF_OL_CLASSES, &
                                                MAX_OL_CLASS_ID_NUM, &
                                                TEMP_ASSET_CLASS_POINTER)
                  IF(INDEX(INTRA_COMPANY_TRANSACTION,'Y') /= 0) &
                         CALL SET_ASSET_CLASSES(INTRA_ASSET_CLASS_ID, &
                                              NUMBER_OF_OL_CLASSES, &
                                              MAX_OL_CLASS_ID_NUM, &
                                              TEMP_ASSET_CLASS_POINTER)
               ENDIF
!
! WRITE RECORD
!
               RECORDS_IN_OVERLAY_TABLE = RECORDS_IN_OVERLAY_TABLE + 1
               WRITE(12,REC=IREC) DELETE,YEAR,CLASS_NAME,FILE_VALUES,INTRA_COMPANY_TRANSACTION,INTRA_ASSET_CLASS_ID, &
                                  INTRA_ASSET_CLASS_ALLOCATION,INTRA_EXPENSE_CLASSIFICATION,INTRA_EXPENSE_COLLECTION, &
                                  REVENUE_CLASSIFICATION,ENERGY_UNITS,MONTHLY_PRICE_INFORMATION,CASH_TREATMENT,LAG_PERIOD, &
                                  CASH_RECEIVABLE_PAYABLE,CPL_JURISDICTION,OUC_CLASSIFICATIONS,OUC_TAX_TREATMENT, &
                                  OUC_BASE_EL_STAB_ADDER,OUC_FUEL_STAB_ADDER,OUC_CUST_RETENTION_ADDER,OUC_BASE_WATER_STAB_ADDER, &
                                  OUC_PLACEHOLDER_FUND_A,OUC_PLACEHOLDER_FUND_B,ENERGY_LOSS_FACTOR,DEMAND_LOSS_FACTOR, &
                                  DEMAND_PRICING_SWITCH,ACCOUNT_ACTIVE,COMMODITY_LHC_DISTRIBUTION,DEMAND_LHC_DISTRIBUTION, & ! 105
                                  CUSTOMER_LHC_DISTRIBUTION, &                                                             ! 106
                                  ACCOUNT_TYPE, &                                                                          ! 107
                                  EXPENSE_CLASSIFICATION, &                                                                ! 108
                                  EXPENSE_COLLECTION, &                                                                    ! 109
                                  TRANSACT_FORECAST_GROUP, &                                                               ! 110
                                  TRANSACT_CUSTOMER_GROUP, &                                                               ! 111
                                  FE_COMPETITIVE_CLASSIFICATION, &                                                         ! 112
                                  USE_TRANSACT_FILE,ENERGY_ADJ_RATE, &                                                 ! 114-125
                                  STATE_OR_PROVINCE,STATE_RPS_ALLOCATION,STATE_RPS_EXEMPTION                               ! 128
               IF(RECORDS_IN_OVERLAY_TABLE >= AVAIL_DATA_YEARS) EXIT
            ENDDO ! read loop
            IF(IOS_BASE /= 0) EXIT
         ENDDO
         CLOSE(10)
         CLOSE(12)
         IF(CLASS_SALES_OL == 'BC') CLOSE(11)
         CLASS_SALES_OL= 'OL'
         IF(ALLOCATED(OL_ASSET_CLASS_POINTER)) then
            DEALLOCATE(OL_ASSET_CLASS_POINTER)
         end if
         IF(MAX_OL_CLASS_ID_NUM > 0) THEN
            ALLOCATE(OL_ASSET_CLASS_POINTER(MAX_OL_CLASS_ID_NUM))
            OL_ASSET_CLASS_POINTER(:) = &
                TEMP_ASSET_CLASS_POINTER(1:MAX_OL_CLASS_ID_NUM)
         ENDIF
         DEALLOCATE(TEMP_ASSET_CLASS_POINTER)
      RETURN
  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from Msgmmrev SIID232'
      call end_program(er_message)


      ENTRY RESET_CLASS_SALES_OL

         CLASS_SALES_OL = 'BC'
      RETURN


      ENTRY OPEN_CLASS_SALES_FILE(CLASS_SALES_UNIT)

         SAVE_CLASS_SALES_UNIT = CLASS_SALES_UNIT
         INQUIRE(UNIT=CLASS_SALES_UNIT,OPENED=FILE_OPEN)
         IF(FILE_OPEN) CLOSE(CLASS_SALES_UNIT)
         OPEN(CLASS_SALES_UNIT,FILE=trim(OUTPUT_DIRECTORY())// &
            CLASS_SALES_OL//"CSALES.BIN",ACCESS="DIRECT",RECL=LRECL)
      RETURN
!

      ENTRY CLOSE_CLASS_SALES_FILE

         CLOSE(SAVE_CLASS_SALES_UNIT)
      RETURN


      ENTRY RETURN_CLASS_SALES_TABLE_NUM(R_NUMBER_OF_TABLES)

         IF(CLASS_SALES_OL == 'OL') THEN
            R_NUMBER_OF_TABLES = OL_TABLE_NUMBER
         ELSE
            R_NUMBER_OF_TABLES = BC_TABLE_NUMBER
         ENDIF
      RETURN

      ENTRY RETURN_NUM_CLASS_SALES_CLASSES(R_NUM_OF_CLASSES, &
                                           R_MAX_CLASS_NUM)

         IF(CLASS_SALES_OL == 'OL') THEN
            R_NUM_OF_CLASSES = NUMBER_OF_OL_CLASSES
            R_MAX_CLASS_NUM = MAX_OL_CLASS_ID_NUM
         ELSE
            R_NUM_OF_CLASSES = NUMBER_OF_BC_CLASSES
            R_MAX_CLASS_NUM = MAX_BC_CLASS_ID_NUM
         ENDIF
      RETURN

      ENTRY RETURN_CLASS_SALES_POINTERS(R_CLASS_POINTERS, &
                                        R_MAX_CLASS_NUM)

         IF(CLASS_SALES_OL == 'OL') THEN
            R_CLASS_POINTERS(1:R_MAX_CLASS_NUM) = &
                OL_ASSET_CLASS_POINTER(1:MAX_OL_CLASS_ID_NUM)
         ELSE
            R_CLASS_POINTERS(1:R_MAX_CLASS_NUM) = &
                BC_ASSET_CLASS_POINTER(1:MAX_BC_CLASS_ID_NUM)
         ENDIF
      RETURN
 1000 FORMAT(A)
! 1010 FORMAT('&',A)
      END

!
!  READS SALES TABLES AND ALLOCATES TO CLASS
!

      RECURSIVE SUBROUTINE INIT_SALES_REVENUE_BY_CLASS

!
      use spindriftlib
      use prod_arrays_dimensions
      USE IREC_ENDPOINT_CONTROL
      use filename_tracker
      use rptreccontrol
      use grx_planning_routines
      USE SIZECOM
      use globecom
      use class_run_switchesc
      use class_run_switchesl1
      use class_run_switchesl4
      use mthnmcom
      use namescom
      use forecast
      use mod_base_year
      use prod_arrays_dimensions
      SAVE


!
      INTEGER (KIND=2) :: ASSET_CLASS_POINTER_loc(:),MAX_ASSET_CLASS_NUM, &
        NUM_OF_ASSET_CLASSES,R_MONTH
      ALLOCATABLE :: ASSET_CLASS_POINTER_loc
      INTEGER (KIND=2) :: INTRA_CLASS,LAG_MO,MO1
      PARAMETER (INTRA_CLASS=-1)
      INTEGER (KIND=4) :: VALUES_TO_ZERO
      INTEGER (KIND=2) :: MO,I,IREC,R_YEAR,R_ST_TG
      INTEGER (KIND=4) :: IOS
      LOGICAL (KIND=1) :: FirstEnergyActive,FirstEnergy, &
               CPL_IS_ACTIVE,CPL_ACTIVE,WVPA, &
               YES_RPS_CALC
      CHARACTER (LEN=32) :: RATE_CLASS_NAME(:),INTRA_COMPANY_TRANSACTION*1
      REAL (KIND=4) :: INTRA_COMPANY_REVENUE
      REAL (KIND=4) :: R_TOTAL_BASE_REVENUE,TOTAL_BASE_REVENUE
      REAL (KIND=4) :: R_CLASS_REVENUES(0:*),ALLOCATED_REVENUES, &
        TWO_YR_CASH_DISTRIBUTION(24)
!
      REAL (KIND=4) :: CLASS_ALLOCATION_ID(:), &
             CLASS_ALLOCATION_VECTOR(:), &
             CLASS_ENERGY(:,:), &
             CLASS_DEMAND(:,:), &
             CLASS_CUSTOMERS(:,:), &
             CPL_JURISDICTIONAL_INFOR(:,:,:), &
             STATE_RPS_DB(:,:,:), &
             R_STATE_RPS_VARS(13)
!
      ALLOCATABLE :: CLASS_ALLOCATION_ID, &
                     CLASS_ALLOCATION_VECTOR, &
                     RATE_CLASS_NAME, &
                     CLASS_ENERGY, &
                     CLASS_DEMAND, &
                     CLASS_CUSTOMERS, &
                     CPL_JURISDICTIONAL_INFOR, &
                     STATE_RPS_DB
      REAL (KIND=4) :: ASSET_CLASS_LIST(:)
      REAL (KIND=4) :: ASSET_ALLOCATION_LIST(:),ASSET_ALLOCATOR
      ALLOCATABLE :: ASSET_CLASS_LIST,ASSET_ALLOCATION_LIST
      REAL (KIND=4) :: MARKET_ENERGY_PRICE, &
           MARKET_DEMAND_PRICE,MARKET_CUSTOMER_PRICE, &
           ENERGY_PRICE(0:12),ENERGY(12), &
           DEMAND_PRICE(0:12),DEMAND(12), &
           CUSTOMER_PRICE(0:12),CUSTOMERS(12), &
           MONTHLY_DOLLARS(0:12),MONTHLY_ENERGY(0:12), &
           TRANS_ENERGY(0:12), &
           TRANS_PEAK(0:12), &
           TRANS_CUSTOMERS(0:12), &
           TEMP_TRANS_ENERGY(0:12), &
           TEMP_TRANS_PEAK(0:12), &
           TEMP_TRANS_CUSTOMERS(0:12)
      REAL (KIND=4) :: MONTHLY_ENERGY_DOLLARS(0:12), &
             MONTHLY_CUSTOMER_DOLLARS(0:12), &
             MONTHLY_DEMAND_DOLLARS(0:12), &
             BILLING_DEMAND(0:12), &
             R_MONTHLY_VALUES(0:12)
      LOGICAL (KIND=1) :: GET_TG_CG_DATA,DATA_FOUND,TEMP_DATA_FOUND, &
         TEMP_L,GET_STATE_RPS_REQS
      REAL (KIND=4) :: ANNUAL_PEAK_DEMAND,ANNUAL_ENERGY
      INTEGER (KIND=2) :: FILE_TABLES
      INTEGER (KIND=2) :: ASSET_CLASS_loc,CLASS_POINTER,SALES_UNIT=3458,DELETE, &
        ASSET_ALLOCATION_VECTOR
      CHARACTER (LEN=1) :: DUMMY_TYPE
      INTEGER (KIND=2) :: INTRA_ASSET_CLASS_ID,INTRA_ASSET_CLASS_ALLOCATION
      CHARACTER (LEN=40) :: INTRA_EXPENSE_CLASSIFICATION, &
                   INTRA_EXPENSE_COLLECTION, &
                   REVENUE_CLASSIFICATION, &
                   CPL_JURISDICTION, &
                   EXPENSE_CLASSIFICATION, &
                   EXPENSE_COLLECTION, &
                   FE_COMPETITIVE_CLASSIFICATION ! 112
      CHARACTER (LEN=20) :: COMMODITY_LHC_DISTRIBUTION, &
                            DEMAND_LHC_DISTRIBUTION, & ! 105
                            CUSTOMER_LHC_DISTRIBUTION ! 106
      CHARACTER (len=10) :: STATE_OR_PROVINCE
      CHARACTER (LEN=1) :: ACCOUNT_TYPE, & ! 107
                           USE_TRANSACT_FILE ! 113
      CHARACTER (LEN=6) :: TEMP_CHAR_6
      REAL :: ENERGY_ADJ_RATE(12), &      ! 114-125
              STATE_RPS_ALLOCATION, &
              STATE_RPS_EXEMPTION, &  ! 128
              RPS_REQS(8), &
              TEMP_R
      INTEGER (KIND=2) :: TRANSACT_FORECAST_GROUP, & ! 110
                          TRANSACT_CUSTOMER_GROUP, & ! 111
                          ST,STATE_ID_LOOKUP, &
                          RPS,ST_TG
      CHARACTER (LEN=3) :: ENERGY_UNITS
      REAL (KIND=4) :: MONTHLY_PRICE_INFORMATION(12,3)
      CHARACTER (LEN=1) :: CASH_TREATMENT, &
                           R_CASH_TREATMENT
!
      CHARACTER (LEN=1) :: OUC_CLASSIFICATIONS
      CHARACTER (len=30) :: OUC_TAX_TREATMENT, &
                            LAG_PERIOD, &
                            CASH_RECEIVABLE_PAYABLE
      REAL (KIND=4) :: OUC_BASE_EL_STAB_ADDER, &
                       OUC_FUEL_STAB_ADDER, &
                       OUC_CUST_RETENTION_ADDER, &
                       OUC_BASE_WATER_STAB_ADDER, &
                       OUC_PLACEHOLDER_FUND_A, &
                       OUC_PLACEHOLDER_FUND_B
      REAL (KIND=4) :: REVENUES_MONTHLY(:,:,:),EXPENSES_MONTHLY(:,:,:), &
                       CASH_AMOUNTS_RECEIVABLE(:,:,:), &
                       CASH_AMOUNTS_PAYABLE(:,:,:), &
                       CASH_CARRY_OVER_RECEIVABLE(:,:,:), &
                       CASH_CARRY_OVER_PAYABLE(:,:,:), &
                       ENERGY_MONTHLY(:,:,:)
      ALLOCATABLE :: REVENUES_MONTHLY,EXPENSES_MONTHLY, &
                     CASH_AMOUNTS_RECEIVABLE, &
                     CASH_AMOUNTS_PAYABLE, &
                     CASH_CARRY_OVER_RECEIVABLE, &
                     CASH_CARRY_OVER_PAYABLE, &
                     ENERGY_MONTHLY
      INTEGER (KIND=4), ALLOCATABLE :: ASSET_CLASS_CUSTOMERS(:,:)
      REAL (KIND=4), ALLOCATABLE :: ASSET_CLASS_ENERGY(:,:)
      REAL (KIND=4) :: DUMMY_CUSTOMERS(0:12)= 12*0.
      INTEGER (KIND=2) :: RUN_YEAR
      INTEGER (KIND=2) :: EXTYPE,REV_TYPE
      REAL (KIND=4) :: OPREV_TAX_RATE
      INTEGER (KIND=2) :: TEMP_YR
      REAL (KIND=4) :: LAG_BY_MONTH(12),ANNUAL_CASH_TOTAL, &
                       TOTAL_BOOKED_AMOUNT,TOTAL_CASH_AMOUNT, &
                       ACCRUED_CASH_BY_MONTH(12)

!
! REVENUE SECTION
!
      INTEGER (KIND=2) :: R_CLASS
      CHARACTER (LEN=*) :: R_REVENUE_CLASSIFICATION, &
                           R_EXPENSE_CLASSIFICATION,R_EXPENSE_COLLECTION
      REAL (KIND=4) :: R_ADJUSTMENT_CLAUSE_REVENUES, &
                       R_BASE_RATES_REVENUES, &
                       R_SECONDARY_SALES_REVENUES, &
                       R_OTHER_REVENUES, &
                       R_BTL_REVENUES, &
                       R_GAS_REVENUES, &
                       R_CAT_REVENUES, &
                       R_RESIDENTIAL_MWH, &
                       R_COMMERCIAL_MWH, &
                       R_INDUSTRIAL_MWH, &
                       R_PUBLIC_STREET_HIGHWAY_MWH, &
                       R_OTHER_PUBLIC_MWH, &
                       R_WHOLESALE_MWH, &
                       R_OTHER_MWH, &
                       R_GAS_ADJUSTMENT_CLAUSE_REVENUE, &
                       R_COMPETITIVE_SALES_REVENUE, &
                       FE_Competitive_Unit_Sales, &
                       FE_Intra_Company_Utility_Sales
      REAL (KIND=4) :: R_COMPETITIVE_SALES(9), &
             R_COMPETITIVE_SALES_QUANT(9), &
             R_COMPETITIVE_LOSS(9)
      REAL (KIND=4) :: R_FUEXP,R_PREXP,R_OPEXP,R_MNEXP,R_OTHER1,R_OTHER2, &
           R_OTHER3,R_NFOWN,R_NFLEASE, &
           R_DSM_EXPENSE,R_DSM_REBATE,R_BTL_EXPENSE, &
           R_ATL_LEASE_EXP,R_BTL_LEASE_EXP,R_SERVICE_TRANSACTIONS
      REAL (KIND=4) :: EXPENSE_ELIM,REVENUE_ELIM
      LOGICAL (KIND=1) :: CLASS_LINKED_2_PARENT
      INTEGER (KIND=2) :: SC_FUEL_FACTOR_ADJ_MONTH=4, &
                SC_FUEL_FACTOR_CAL_MONTH=3, &
                NC_FUEL_FACTOR_ADJ_MONTH=10, &
                NC_FUEL_FACTOR_CAL_MONTH=3
      REAL (KIND=4) :: SC_PURCHASE_POWER_ALLOCATION(10), &
             NC_PURCHASE_POWER_ALLOCATION(10), &
             CPL_OFF_SYSTEM_FUEL_RATES(12,7), &
             SC_FUEL_FACTOR_INPUT(12,7), &
             NC_FUEL_FACTOR_INPUT(12,7), &
             NC_EMF_FACTOR_INPUT(12,7)
      CHARACTER (LEN=1) :: OFF_SYSTEM_RATES_ACTIVE(7)
!
! SC AND NC FUEL ACCOUNTING
!
      INTEGER (KIND=2) :: REPORTING_UNIT,CPL_DEFERRED_FUEL_RPT_HEADER
      LOGICAL (KIND=1) :: REPORT_HEADER_OPEN=.FALSE.
      REAL (KIND=4) :: SC_CUMULATIVE_FUEL_COST, &
             SC_CUMULATIVE_ENERGY_SALES, &
             SC_CUMULATIVE_JURIS_SALES, &
             SC_BASE_FUEL_FACTOR, &
             SC_DEFERRED_FUEL_BALANCE
      REAL (KIND=4) :: NC_CUMULATIVE_FUEL_COST, &
             NC_CUMULATIVE_ENERGY_SALES, &
             NC_CUMULATIVE_JURIS_SALES, &
             NC_BASE_FUEL_FACTOR, &
             NC_EMF_FACTOR , &
             NC_DEFERRED_FUEL_BALANCE, &
             NC_EMF_FUEL_BALANCE
      INTEGER (KIND=2) :: CPL_DEFERRED_FUEL_RPT_CLASS_ID, &
                NC_DEFERRED_FUEL_RPT_CLASS_ID, &
                SC_DEFERRED_FUEL_RPT_CLASS_ID
      CHARACTER (LEN=1) :: USE_BUDGET_FORECAST_IN(7)
! ADDITIONAL SECTION
      REAL (KIND=4) :: ENERGY_LOSS_FACTOR, &
             DEMAND_LOSS_FACTOR, &
             SYSTEM_PEAK_WINTER_NC, &
             SYSTEM_PEAK_WINTER_SC, &
             SYSTEM_PEAK_WINTER, &
             SYSTEM_PEAK_SUMMER_NC, &
             SYSTEM_PEAK_SUMMER_SC, &
             SYSTEM_PEAK_SUMMER, &
             SYSTEM_NON_PEAK_NC, &
             SYSTEM_NON_PEAK_SC, &
             SYSTEM_NON_PEAK_NC_SC
      REAL (KIND=4) :: SYSTEM_PEAK_WHOLE_12SUM, &
             SYSTEM_PEAK_12SUM, &
             SYSTEM_ENERGY_SALES_NC, &
             SYSTEM_ENERGY_SALES_SC, &
             SYSTEM_ENERGY_SALES_WHOLE, &
             SYSTEM_ENERGY_SALES, &
             TEMP_PEAK, &
             CPL_NC_PEAK=0.,CPL_SC_PEAK=0., &
             SYSTEM_PEAK_WINTER_WHOLE, &   ! CA ADDED 2.25.99
             SYSTEM_PEAK_WINTER_NCEMPA, &
             SYSTEM_PEAK_WINTER_NCEMC, &
             SYSTEM_PEAK_SUMMER_WHOLE, &
             SYSTEM_PEAK_SUMMER_NCEMPA, &
             SYSTEM_PEAK_SUMMER_NCEMC, &
             SYSTEM_ENERGY_SALES_NCEMPA, &
             SYSTEM_ENERGY_SALES_NCEMC, &
             NCP_MULTIPLIER (4), &
             SPLIT_FACTOR (4)

      CHARACTER (LEN=1) :: DEMAND_PRICING_SWITCH, &
        ACCOUNT_ACTIVE
      INTEGER (KIND=2) :: SALES_CLASSIFICATION,CPL_SALES_CLASS, &
                NC_SALES_CLASS=1,SC_SALES_CLASS=2, &
                WHOLE_SALES_CLASS=3,NCEMPA_SALES_CLASS=5, &
                NCEMC_SALES_CLASS=6, &
                PEAK_MONTH,A
      INTEGER (KIND=2) :: INCOME_STATEMENT_POSITION
      REAL (KIND=4) :: SC_FUEL_REV_CASH_JAN, &
                       NC_FUEL_REV_CASH_JAN, &
                       BU_FUEL_REV_CASH_JAN
      INTEGER (KIND=2) :: SC_CALCULATION_START_YR,NC_CALCULATION_START_YR
      CHARACTER (LEN=1) :: DEF_CLASS_TYPE
      REAL (KIND=4) :: NC_CALC_BASE_FUEL_FACTOR,NC_CALC_EMF_FACTOR,SC_CALC_BASE_FUEL_FACTOR
      LOGICAL (KIND=1) :: CPL_DEFERRED_FUEL_NOT_ACTIVE
!
      INTEGER (KIND=2) :: ALLOCATION_VECTOR
      REAL (KIND=4) :: ALLOCATION_VALUE(AVAIL_DATA_YEARS)
      LOGICAL (KIND=1) :: VOID_LOGICAL,RETURN_ASSET_CLASS_LISTS
      REAL (KIND=4) :: PA_TOTAL_RETAINED, &
                       PA_SEPA_CAPACITY, &
                       PA_FIRM_PURCHASE_CAPACITY, &
                       EMC_SEPA_CAPACITY
!
      INTEGER (KIND=2) :: R_RUN_YEAR
      CHARACTER (len=30) :: R_LAG_PERIOD,R_CASH_RECEIVABLE_PAYABLE
      CHARACTER (LEN=40) :: LAGGED_PATTERN
      REAL (KIND=4) :: R_MONTHLY_REVENUES(0:12),R_ASSET_ALLOCATOR,R_MONTHLY_ENERGY(0:12),R_CLASS_CUSTOMERS(0:12)
      REAL (KIND=4) :: MONTHLY_ALLOCATED_REVENUES(0:12),MONTHLY_ALLOCATED_ENERGY
      REAL (KIND=4) :: CURRENT_AMOUNT,LAGGED_AMOUNT
      REAL (KIND=4) :: R_REVENUE_ELIM
      REAL (KIND=4) :: R_MONTHLY_EXPENSES(0:12),MONTHLY_ALLOCATED_EXPENSES(0:12)
      LOGICAL (KIND=1) :: EXPENSE_TYPE_IS_CASH
      REAL (KIND=4) :: R_EXPENSES_ELIM
      INTEGER (KIND=2) :: PERIOD
      REAL (KIND=4) :: R_RESIDENTIAL_REVENUES, &
                       R_COMMERCIAL_REVENUES, &
                       R_INDUSTRIAL_REVENUES, &
                       R_LIGHTING_REVENUES, &
                       R_BULK_POWER_REVENUES, &
                       R_CAPACITY_SALES, &
                       R_GOVERNMENT_SALES, &
                       R_ADJ_EXP
!
      REAL (KIND=4) :: R_MON_CHANGE_ACCTS_RECEIVABLE(0:12),R_MON_CHANGE_ACCTS_PAYABLE(0:12)
      INTEGER (KIND=2) :: DATA_POS,J
!
      REAL (KIND=4) :: R_CHANGE_IN_ACCOUNTS_RECEIVABLE, &
        R_CHANGE_IN_ACCOUNTS_PAYABLE
!
      CHARACTER (len=5) :: CPL_DEFERRED_FUEL_FILE
      CHARACTER (len=256) :: FILE_NAME
      CHARACTER (len=256) :: BASE_FILE_DIRECTORY
      CHARACTER (LEN=5000) :: RECLN,DUMMY_CHR*20
      INTEGER (KIND=2) :: SKIP,YR,CLASS_POS
      CHARACTER (LEN=1) :: R_CLASS_TYPE(0:*)
      INTEGER (KIND=2) :: R_MASTER_CLASS_LIST(0:*),RC_REVENUE_FORECAST_HEADER
!
      REAL (KIND=4) :: CPL_GEN_COST(0:12), &
                       CPL_NUC_COST(0:12), &
                       CPL_PA_ENTITLE(0:12), &
                       CPL_PUR_POWER(0:12), &
                       CPL_OFF_SYS_INC_COST(0:12), &
                       CPL_PA_MWH_COST(0:12), &
                       CPL_OFF_SYS_ENRG(0:12), &
                       CPL_OFF_SYS_ENRG_RATE(0:12), &
                       NC_NEW_BASE_FUEL_FACTOR, &
                       NC_NEW_EMF_FACTOR
      REAL (KIND=4) :: CPL_NCEMPA_MWH(0:12),CPL_NCEMC_MWH(0:12)
      INTEGER (KIND=2) :: OTHER2_FORECAST
      REAL (KIND=4) :: ZERO_POS
      PARAMETER(ZERO_POS=0.,OTHER2_FORECAST=5)
      CHARACTER (LEN=6) :: SHORT_MONTH_NAMES
      INTEGER (KIND=2) :: CURRENT_YEAR
      REAL (KIND=4) :: SC_PUR_POWER_ALLOCATION, &
                       NC_PUR_POWER_ALLOCATION, &
                       TOTAL_OFF_SYSTEM_ENRG, &
                       TOTAL_OFF_SYSTEM_COST, &
                       OFF_SYSTEM_ENRG_RATE
      REAL (KIND=4) :: BUDGET_GEN_COST(0:12),BUDGET_PUR_POWER(0:12)
      REAL (KIND=4) :: ANNUAL_CPL_GEN_COST,ANNUAL_CPL_PUR_POWER
      REAL (KIND=4) :: ENRG_RATIO,P_POWER_RATIO
!
      REAL (KIND=4) :: R_CPL_FUEL_REVENUES,R_DEFERRED_FUEL_EXPENSE
!
      REAL (KIND=4) :: R_CPL_MONTHLY_FUEL_REVENUES(0:12), &
                       R_MONTHLY_DEFERRED_FUEL_EXPENSE(0:12), &
                       R_MONTHLY_CASH_FUEL_REVENUES(0:12), &
                       R_FUEL_REVENUE_RECEIVABLE
      INTEGER (KIND=2) :: SCENARIO_INDEX,GET_SCENARIO_INDEX
      REAL (KIND=4) :: GET_SCENARIO_BY_INDEX
      CHARACTER (LEN=40) :: EXT_RATE_CLASS_NAME
      CHARACTER (LEN=8) :: ACCOUNT_TYPE_NAME
      CHARACTER (LEN=20) :: VARIABLE_TYPE_NAME(0:10)
      REAL (KIND=4) :: VARIABLE_VALUE(0:13,0:10)
      LOGICAL (KIND=1) :: RC_RPT_HEADER_NOT_OPEN=.TRUE.
      INTEGER (KIND=4) :: NEXT_REC=0
      INTEGER (KIND=2) :: RC_UNIT_NO=0
      REAL (KIND=4) :: THE_RATIO_OF_A_TO_B

!
! END DATA DECLARATIONS
!
         IF(WVPA()) RETURN
         NCP_MULTIPLIER(1) = 1.1049
         NCP_MULTIPLIER(2) = 1.1438
         NCP_MULTIPLIER(3) = 1.0912
         NCP_MULTIPLIER(4) = 1.0578

         SPLIT_FACTOR (1) =  0.8730
         SPLIT_FACTOR (2) =  0.8341
         SPLIT_FACTOR (3) =  0.7654
         SPLIT_FACTOR (4) =  0.9345

         MAX_ASSET_CLASS_NUM = -99
         CALL RETURN_CLASS_SALES_TABLE_NUM(FILE_TABLES)
         IF(FILE_TABLES > 0) THEN
            IF(ALLOCATED(ASSET_CLASS_LIST)) &
       DEALLOCATE(ASSET_CLASS_LIST, &
                                               ASSET_ALLOCATION_LIST, &
                                               RATE_CLASS_NAME, &
                                               CLASS_ALLOCATION_ID, &
                                               CLASS_ALLOCATION_VECTOR, &
                                               CLASS_ENERGY, &
                                               CLASS_DEMAND, &
                                               CLASS_CUSTOMERS, &
                                               CPL_JURISDICTIONAL_INFOR, &
                                               STATE_RPS_DB)
            ALLOCATE(ASSET_CLASS_LIST(AVAIL_DATA_YEARS), &
                                ASSET_ALLOCATION_LIST(AVAIL_DATA_YEARS), &
                                RATE_CLASS_NAME(FILE_TABLES), &
                                CLASS_ALLOCATION_ID(FILE_TABLES), &
                                CLASS_ALLOCATION_VECTOR(FILE_TABLES), &
                                CLASS_ENERGY(0:12,FILE_TABLES), &
                                CLASS_DEMAND(0:12,FILE_TABLES), &
                                CLASS_CUSTOMERS(0:12,FILE_TABLES), &
                                CPL_JURISDICTIONAL_INFOR(0:12, &
                                CPL_JURISDICTIONS,CPL_FUEL_VARS), &
                                STATE_RPS_DB(12,600,0:12))
!
            CLASS_ENERGY = 0.
            CLASS_DEMAND = 0.
            CLASS_CUSTOMERS = 0.
            CPL_JURISDICTIONAL_INFOR = 0.
            STATE_RPS_DB = 0.0
!
! ASSET CLASS EXPENSE AND REVENUE INFORMATION
!
            CALL RETURN_NUM_CLASS_SALES_CLASSES(NUM_OF_ASSET_CLASSES, &
                                                MAX_ASSET_CLASS_NUM)
                        IF(ALLOCATED(ASSET_CLASS_POINTER_loc)) then
                            DEALLOCATE(ASSET_CLASS_POINTER_loc)
                        end if
            IF(MAX_ASSET_CLASS_NUM > 0) THEN
               ALLOCATE(ASSET_CLASS_POINTER_loc(MAX_ASSET_CLASS_NUM))
             CALL RETURN_CLASS_SALES_POINTERS(ASSET_CLASS_POINTER_loc, &
                                                MAX_ASSET_CLASS_NUM)
            ENDIF
            CALL REVENUE_FORECAST_DATABASE(NUM_OF_ASSET_CLASSES, &
                                           MAX_ASSET_CLASS_NUM, &
     +                                     ASSET_CLASS_POINTER_loc)
!
! REVENUE SECTION
!
           IF(ALLOCATED(REVENUES_MONTHLY)) DEALLOCATE(REVENUES_MONTHLY, &
                                             EXPENSES_MONTHLY, &
                                             CASH_AMOUNTS_RECEIVABLE, &
                                             CASH_AMOUNTS_PAYABLE, &
                                             CASH_CARRY_OVER_RECEIVABLE, &
                                             CASH_CARRY_OVER_PAYABLE, &
                                             ENERGY_MONTHLY)
            ALLOCATE(REVENUES_MONTHLY(0:12,-1:NUM_OF_ASSET_CLASSES, &
                LAST_INCOME_LINE), &
                     EXPENSES_MONTHLY(0:12,-1:NUM_OF_ASSET_CLASSES, &
                        LAST_EXPENSE_ITEM), &
                     CASH_AMOUNTS_RECEIVABLE(0:12,-1:NUM_OF_ASSET_CLASSES,LAST_INCOME_LINE), &
                     CASH_AMOUNTS_PAYABLE(0:12,-1:NUM_OF_ASSET_CLASSES,LAST_EXPENSE_ITEM), &
                     CASH_CARRY_OVER_RECEIVABLE(1:12,-1:NUM_OF_ASSET_CLASSES,LAST_INCOME_LINE), &
                     CASH_CARRY_OVER_PAYABLE(1:12,-1:NUM_OF_ASSET_CLASSES,LAST_EXPENSE_ITEM), &
                     ENERGY_MONTHLY(0:12,-1:NUM_OF_ASSET_CLASSES,LAST_INCOME_LINE))
            IF(ALLOCATED(ASSET_CLASS_CUSTOMERS)) then
                DEALLOCATE(ASSET_CLASS_CUSTOMERS)
            end if
            ALLOCATE(ASSET_CLASS_CUSTOMERS(0:12,-1:NUM_OF_ASSET_CLASSES))

            IF(ALLOCATED(ASSET_CLASS_ENERGY)) then
                DEALLOCATE(ASSET_CLASS_ENERGY)
            end if
            ALLOCATE(ASSET_CLASS_ENERGY(0:12,-1:NUM_OF_ASSET_CLASSES))
            ASSET_CLASS_CUSTOMERS = 0.
            ASSET_CLASS_ENERGY = 0.


            REVENUES_MONTHLY = 0.
            CASH_AMOUNTS_RECEIVABLE = 0.
            ENERGY_MONTHLY = 0.

            CASH_CARRY_OVER_RECEIVABLE = 0.
            EXPENSES_MONTHLY = 0.
            CASH_AMOUNTS_PAYABLE = 0.
            CASH_CARRY_OVER_PAYABLE = 0.

            CALL OPEN_CLASS_SALES_FILE(SALES_UNIT)
            FirstEnergyActive = FirstEnergy()
            CPL_IS_ACTIVE = CPL_ACTIVE()
         ENDIF
      RETURN

      ENTRY CALCULATE_CLASS_SALES_REVENUES(R_YEAR,R_CLASS_REVENUES, &
                                           INTRA_COMPANY_REVENUE)


         IF(WVPA()) RETURN
         INTRA_COMPANY_REVENUE = 0.
         IF(FILE_TABLES == 0) RETURN
         RUN_YEAR = MIN(AVAIL_DATA_YEARS,R_YEAR)

! set up report

         IF(RC_RPT_HEADER_NOT_OPEN) THEN
            RC_RPT_HEADER_NOT_OPEN = .FALSE.
            RC_UNIT_NO = RC_REVENUE_FORECAST_HEADER(INT(20,2),NEXT_REC)
         ENDIF
!
! ZERO ARRAYS
!
!
         ASSET_CLASS_CUSTOMERS = 0.
         ASSET_CLASS_ENERGY = 0.
         REVENUES_MONTHLY = 0.
         CASH_AMOUNTS_RECEIVABLE = 0.
         ENERGY_MONTHLY = 0.
         EXPENSES_MONTHLY = 0.
         CASH_AMOUNTS_PAYABLE = 0.
         CPL_JURISDICTIONAL_INFOR = 0.
         STATE_RPS_DB = 0.
!
         IF(RUN_YEAR /= 1) THEN
            DO ASSET_CLASS_loc  = -1, NUM_OF_ASSET_CLASSES
               DO REV_TYPE = 1, LAST_INCOME_LINE
                  DO MO = 1, 12
                     CASH_AMOUNTS_RECEIVABLE(MO,ASSET_CLASS_loc,REV_TYPE) = &
                        CASH_CARRY_OVER_RECEIVABLE(MO, &
                            ASSET_CLASS_loc,REV_TYPE)
                     CASH_CARRY_OVER_RECEIVABLE(MO, &
                        ASSET_CLASS_loc,REV_TYPE) = 0.
                  ENDDO
               ENDDO

               DO EXTYPE = 1, LAST_EXPENSE_ITEM
                  DO MO = 1, 12
                     CASH_AMOUNTS_PAYABLE(MO,ASSET_CLASS_loc,EXTYPE) = &
                        CASH_CARRY_OVER_PAYABLE(MO,ASSET_CLASS_loc,EXTYPE)
                    CASH_CARRY_OVER_PAYABLE(MO,ASSET_CLASS_loc,EXTYPE) = 0.
                  ENDDO
               ENDDO
            ENDDO
         ENDIF
!
! READ SALES FILE
!
         IREC = RUN_YEAR - AVAIL_DATA_YEARS
         DO I = 1, FILE_TABLES
            IREC = IREC + AVAIL_DATA_YEARS
            READ(SALES_UNIT,REC=IREC,IOSTAT=IOS) DELETE,TEMP_YR,RATE_CLASS_NAME(I),MARKET_ENERGY_PRICE,MARKET_DEMAND_PRICE, &
                          MARKET_CUSTOMER_PRICE,CLASS_ALLOCATION_ID(I),CLASS_ALLOCATION_VECTOR(I), &
                          (ENERGY(MO),DEMAND(MO),CUSTOMERS(MO),MO=1,12),INTRA_COMPANY_TRANSACTION,INTRA_ASSET_CLASS_ID, &
                          INTRA_ASSET_CLASS_ALLOCATION,INTRA_EXPENSE_CLASSIFICATION,INTRA_EXPENSE_COLLECTION, &
                          REVENUE_CLASSIFICATION,ENERGY_UNITS,MONTHLY_PRICE_INFORMATION,CASH_TREATMENT,LAG_PERIOD, &
                          CASH_RECEIVABLE_PAYABLE,CPL_JURISDICTION,OUC_CLASSIFICATIONS,OUC_TAX_TREATMENT,OUC_BASE_EL_STAB_ADDER, &
                          OUC_FUEL_STAB_ADDER,OUC_CUST_RETENTION_ADDER,OUC_BASE_WATER_STAB_ADDER,OUC_PLACEHOLDER_FUND_A, &
                          OUC_PLACEHOLDER_FUND_B,ENERGY_LOSS_FACTOR,DEMAND_LOSS_FACTOR,DEMAND_PRICING_SWITCH,ACCOUNT_ACTIVE, &
                          COMMODITY_LHC_DISTRIBUTION,DEMAND_LHC_DISTRIBUTION, &           ! 105
                          CUSTOMER_LHC_DISTRIBUTION, &                                    ! 106
                          ACCOUNT_TYPE, &                                                 ! 107
                          EXPENSE_CLASSIFICATION, &                                       ! 108
                          EXPENSE_COLLECTION, &                                           ! 109
                          TRANSACT_FORECAST_GROUP, &                                      ! 110
                          TRANSACT_CUSTOMER_GROUP, &                                      ! 111
                          FE_COMPETITIVE_CLASSIFICATION, &                                ! 112
                          USE_TRANSACT_FILE, &                                            ! 113
                          ENERGY_ADJ_RATE, &                                              ! 114-125
                          STATE_OR_PROVINCE,STATE_RPS_ALLOCATION,STATE_RPS_EXEMPTION      ! 128
            IF(IOS /= 0) EXIT
            IF(DELETE >= 8 .OR. ACCOUNT_ACTIVE == 'N') CYCLE
!

            DO MO = 1, 12
               ENERGY_PRICE(MO) =  MONTHLY_PRICE_INFORMATION(MO,1) &
                + MARKET_ENERGY_PRICE + ENERGY_ADJ_RATE(MO)    ! 114-125
               DEMAND_PRICE(MO) =  MONTHLY_PRICE_INFORMATION(MO,2) &
                + MARKET_DEMAND_PRICE
               CUSTOMER_PRICE(MO) =  MONTHLY_PRICE_INFORMATION(MO,3) &
                + MARKET_CUSTOMER_PRICE
            ENDDO
            SALES_CLASSIFICATION = CPL_SALES_CLASS(CPL_JURISDICTION)
            DATA_FOUND = .FALSE.
            IF(ACCOUNT_TYPE == 'C') then
                REVENUE_CLASSIFICATION = &
                    FE_COMPETITIVE_CLASSIFICATION
            end if
            ANNUAL_PEAK_DEMAND = 0.
            ANNUAL_ENERGY = 0.
            IF(USE_TRANSACT_FILE == 'Y') THEN
               TRANS_ENERGY = 0.
               TRANS_PEAK = 0.
               TRANS_CUSTOMERS = 0.
               IF(TRANSACT_CUSTOMER_GROUP < 0) THEN
                  ASSET_CLASS_LIST = -99.
                  CALL GET_ASSET_VAR(ABS(TRANSACT_CUSTOMER_GROUP), &
                                         DUMMY_TYPE, &
                                         ASSET_CLASS_LIST)
                  CLASS_POINTER = 1
                  DO
                     TRANSACT_CUSTOMER_GROUP = ASSET_CLASS_LIST(CLASS_POINTER)
                     TEMP_DATA_FOUND = &
                                GET_TG_CG_DATA(TRANSACT_FORECAST_GROUP, &
                                               TRANSACT_CUSTOMER_GROUP, &
                                               TEMP_TRANS_ENERGY, &
                                               TEMP_TRANS_PEAK, &
                                               TEMP_TRANS_CUSTOMERS)
                     DATA_FOUND = DATA_FOUND .OR. TEMP_DATA_FOUND
                     IF(TEMP_DATA_FOUND) THEN
                        DO MO = 1, 12
                           TRANS_ENERGY(MO) = TRANS_ENERGY(MO) + TEMP_TRANS_ENERGY(MO)
                           TRANS_PEAK(MO) = TRANS_PEAK(MO) + TEMP_TRANS_PEAK(MO)
                           TRANS_CUSTOMERS(MO) = TRANS_CUSTOMERS(MO) + TEMP_TRANS_CUSTOMERS(MO)
                        ENDDO
                     ENDIF
                     CLASS_POINTER = CLASS_POINTER + 1
                     IF(CLASS_POINTER > AVAIL_DATA_YEARS) EXIT
                     IF(ASSET_CLASS_LIST(CLASS_POINTER) == 0. .OR. &
                        ASSET_CLASS_LIST(CLASS_POINTER) == -99.) then
                            EXIT
                     end if
                  ENDDO
               ELSE
                  DATA_FOUND = GET_TG_CG_DATA(TRANSACT_FORECAST_GROUP, &
                                              TRANSACT_CUSTOMER_GROUP, &
                                              TRANS_ENERGY, &
                                              TRANS_PEAK, &
                                              TRANS_CUSTOMERS)
                  IF(.NOT. DATA_FOUND) THEN
                     TRANS_ENERGY = 1.
                     TRANS_PEAK = 1.
                     TRANS_CUSTOMERS = 1.
                  ENDIF
               ENDIF
!               DO MO = 1, 12
                 CLASS_ENERGY(1:,I) = TRANS_ENERGY(1:)*ENERGY(1:)/1000.
                  ANNUAL_ENERGY = SUM(CLASS_ENERGY(1:,I))
                  CLASS_DEMAND(1:,I) = TRANS_PEAK(1:) * DEMAND(1:)
                  ANNUAL_PEAK_DEMAND = MAXVAL(CLASS_DEMAND(1:,I))
                  CLASS_CUSTOMERS(1:,I) = TRANS_CUSTOMERS(1:) &
                    * CUSTOMERS(1:)

            ELSE
               DO MO = 1, 12
                  IF(ENERGY(MO) < 1. .AND. RUN_YEAR > 1 .AND. &
                    ENERGY(MO) /= 0.) THEN
                       CLASS_ENERGY(MO,I) = CLASS_ENERGY(MO,I) * &
                        (1.+ENERGY(MO))
                  ELSE
                     IF(INDEX(ENERGY_UNITS,'MWh') /= 0) THEN
                        CLASS_ENERGY(MO,I) = ENERGY(MO)/1000.
                     ELSE
                        CLASS_ENERGY(MO,I) = ENERGY(MO)
                     ENDIF
                  ENDIF
                  ANNUAL_ENERGY = ANNUAL_ENERGY + CLASS_ENERGY(MO,I)
                  IF(DEMAND(MO) < 1. .AND. RUN_YEAR > 1) THEN
                     CLASS_DEMAND(MO,I) = CLASS_DEMAND(MO,I) * (1.+DEMAND(MO))
                  ELSE
                     CLASS_DEMAND(MO,I) = DEMAND(MO)
                  ENDIF
                  ANNUAL_PEAK_DEMAND = &
                    MAX(ANNUAL_PEAK_DEMAND,CLASS_DEMAND(MO,I))
                  IF(CUSTOMERS(MO) < 1. .AND. RUN_YEAR > 1) THEN
                     CLASS_CUSTOMERS(MO,I) = CLASS_CUSTOMERS(MO,I) * &
                        (1.+CUSTOMERS(MO))
                  ELSE
                     CLASS_CUSTOMERS(MO,I) = CUSTOMERS(MO)
                  ENDIF
                  CPL_JURISDICTIONAL_INFOR(MO,SALES_CLASSIFICATION, &
                                                               cpl_mwh) = &
                        CPL_JURISDICTIONAL_INFOR(MO,SALES_CLASSIFICATION, &
                                                               cpl_mwh) + &
                                                       CLASS_ENERGY(MO,I)
                  CPL_JURISDICTIONAL_INFOR(MO,SALES_CLASSIFICATION, &
                                                               cpl_dmd) = &
                        CPL_JURISDICTIONAL_INFOR(MO,SALES_CLASSIFICATION, &
                                                               cpl_dmd) + &
                                                       CLASS_DEMAND(MO,I)
                  CPL_JURISDICTIONAL_INFOR(MO,SALES_CLASSIFICATION, &
                                                         cpl_customers) = &
                        CPL_JURISDICTIONAL_INFOR(MO,SALES_CLASSIFICATION, &
                                                         cpl_customers) + &
                                                    CLASS_CUSTOMERS(MO,I)
               ENDDO
            ENDIF
            CLASS_ENERGY(0,I) = ANNUAL_ENERGY
            CLASS_DEMAND(0,I) = ANNUAL_PEAK_DEMAND
!
! MODIFY QUANTIES USING LATIN HYPERCUBE
!
           IF(INDEX(COMMODITY_LHC_DISTRIBUTION,"Not Active") == 0) THEN
               ANNUAL_ENERGY = 0.
               SCENARIO_INDEX = &
                GET_SCENARIO_INDEX(COMMODITY_LHC_DISTRIBUTION)
               DO MO = 1, 12
                  CLASS_ENERGY(MO,I) = CLASS_ENERGY(MO,I) &
                    * GET_SCENARIO_BY_INDEX(RUN_YEAR,MO,SCENARIO_INDEX)
                  ANNUAL_ENERGY = ANNUAL_ENERGY + CLASS_ENERGY(MO,I)
               END DO
               CLASS_ENERGY(0,I) = ANNUAL_ENERGY
            ENDIF
            IF(INDEX(DEMAND_LHC_DISTRIBUTION,"Not Active") == 0) THEN
               ANNUAL_PEAK_DEMAND = 0.
               SCENARIO_INDEX = GET_SCENARIO_INDEX(DEMAND_LHC_DISTRIBUTION)
               DO MO = 1, 12
                  CLASS_DEMAND(MO,I) = CLASS_DEMAND(MO,I) &
                    * GET_SCENARIO_BY_INDEX(RUN_YEAR,MO,SCENARIO_INDEX)
                  ANNUAL_PEAK_DEMAND = MAX(ANNUAL_PEAK_DEMAND, &
                      CLASS_DEMAND(MO,I))
               END DO
               CLASS_DEMAND(0,I) = ANNUAL_PEAK_DEMAND
            ENDIF
            IF(INDEX(CUSTOMER_LHC_DISTRIBUTION,"Not Active") == 0) THEN
               SCENARIO_INDEX = GET_SCENARIO_INDEX(CUSTOMER_LHC_DISTRIBUTION)
               DO MO = 1, 12
                  CLASS_CUSTOMERS(MO,I) = CLASS_CUSTOMERS(MO,I) &
                    * GET_SCENARIO_BY_INDEX(RUN_YEAR,MO,SCENARIO_INDEX)
               END DO
            ENDIF
! END LHC MODIFICATIONS
           CPL_JURISDICTIONAL_INFOR(0,SALES_CLASSIFICATION,cpl_mwh) = &
                     CPL_JURISDICTIONAL_INFOR(0,SALES_CLASSIFICATION, &
                                                             cpl_mwh) &
                                                      + ANNUAL_ENERGY

            MONTHLY_DOLLARS(0) = 0.
            MONTHLY_ENERGY(0) = 0.
            MONTHLY_ENERGY_DOLLARS(0) = 0.
            MONTHLY_CUSTOMER_DOLLARS(0) = 0.
            MONTHLY_DEMAND_DOLLARS(0) = 0.
            BILLING_DEMAND(0) = 0.
            CLASS_DEMAND(0,I) = 0.
            CLASS_CUSTOMERS(0,I) = 0.
!
            DO MO = 1, 12

               IF(ENERGY_LOSS_FACTOR < 100.) THEN
                  MONTHLY_ENERGY(MO) = CLASS_ENERGY(MO,I) * (1.-ENERGY_LOSS_FACTOR/100.)
               ELSE
                  MONTHLY_ENERGY(MO) = 0.
               ENDIF
               IF (DEMAND_LOSS_FACTOR < 100.) THEN
                  CLASS_DEMAND(MO,I) = CLASS_DEMAND(MO,I) * (1. - DEMAND_LOSS_FACTOR/100.)
               ENDIF
               MONTHLY_ENERGY(0) = MONTHLY_ENERGY(0)+MONTHLY_ENERGY(MO)
               MONTHLY_ENERGY_DOLLARS(MO) = ENERGY_PRICE(MO)*MONTHLY_ENERGY(MO)/1000.
               MONTHLY_CUSTOMER_DOLLARS(MO) = CUSTOMER_PRICE(MO)*CLASS_CUSTOMERS(MO,I)/1000000.
               IF(DEMAND_PRICING_SWITCH == 'M') THEN
                  MONTHLY_DEMAND_DOLLARS(MO) = + DEMAND_PRICE(MO)*CLASS_DEMAND(MO,I)/1000.
                  BILLING_DEMAND(MO) = CLASS_DEMAND(MO,I)
               ELSE
                  MONTHLY_DEMAND_DOLLARS(MO) = + DEMAND_PRICE(MO)*ANNUAL_PEAK_DEMAND/1000.
                  BILLING_DEMAND(MO) = ANNUAL_PEAK_DEMAND
               ENDIF
               MONTHLY_DOLLARS(MO) = MONTHLY_ENERGY_DOLLARS(MO) + MONTHLY_DEMAND_DOLLARS(MO) + MONTHLY_CUSTOMER_DOLLARS(MO)
               MONTHLY_DOLLARS(0) = MONTHLY_DOLLARS(0) + MONTHLY_DOLLARS(MO)
               MONTHLY_ENERGY_DOLLARS(0) = MONTHLY_ENERGY_DOLLARS(0) + MONTHLY_ENERGY_DOLLARS(MO)
               MONTHLY_DEMAND_DOLLARS(0) = MONTHLY_DEMAND_DOLLARS(0) + MONTHLY_DEMAND_DOLLARS(MO)
              MONTHLY_CUSTOMER_DOLLARS(0) = MONTHLY_CUSTOMER_DOLLARS(0) + MONTHLY_CUSTOMER_DOLLARS(MO)
               BILLING_DEMAND(0) = BILLING_DEMAND(0) + BILLING_DEMAND(MO)
               CLASS_DEMAND(0,I) = CLASS_DEMAND(0,I) + CLASS_DEMAND(MO,I)
               CLASS_CUSTOMERS(0,I) = CLASS_CUSTOMERS(0,I) + CLASS_CUSTOMERS(MO,I)
!
            ENDDO
!           REVENUES(I) = MONTHLY_DOLLARS(0)
            EXPENSE_ELIM = 0.

            IF(INTRA_COMPANY_TRANSACTION == 'Y') THEN
               ASSET_CLASS_loc = INTRA_ASSET_CLASS_ID
               ASSET_ALLOCATION_VECTOR = INTRA_ASSET_CLASS_ALLOCATION
!
               VOID_LOGICAL=RETURN_ASSET_CLASS_LISTS(ASSET_CLASS_loc, &
                                               ASSET_CLASS_LIST, &
                                               ASSET_ALLOCATION_VECTOR, &
                                               ASSET_ALLOCATION_LIST)
!
               CLASS_POINTER = 1
               DO
                  ASSET_CLASS_loc = ASSET_CLASS_LIST(CLASS_POINTER)
                  CALL CHECK_IF_CLASS_DEFINED(ASSET_CLASS_loc)
                  ASSET_CLASS_loc = ASSET_CLASS_loc + 1
                  IF(ASSET_CLASS_loc > 0) then
                    ASSET_CLASS_loc = ASSET_CLASS_POINTER_loc(ASSET_CLASS_loc)
                  end if
                 ASSET_ALLOCATOR = ASSET_ALLOCATION_LIST(CLASS_POINTER)
                  IF(ASSET_ALLOCATOR < 0.) THEN
                     ALLOCATION_VECTOR = ABS(ASSET_ALLOCATOR)
                     CALL GET_ASSET_VAR(ALLOCATION_VECTOR, &
                        DUMMY_TYPE, ALLOCATION_VALUE)
                     ASSET_ALLOCATOR = ALLOCATION_VALUE(RUN_YEAR)
                  ENDIF
!
                  ASSET_ALLOCATOR = ASSET_ALLOCATOR/100.
                  IF(ASSET_CLASS_loc <= MAX_ASSET_CLASS_NUM) THEN
                     CALL CL_SALES_EXP_ALLOCATION(ASSET_CLASS_loc, &
                                           MONTHLY_DOLLARS, &
                                           ASSET_ALLOCATOR, &
                                           INTRA_EXPENSE_CLASSIFICATION, &
                                           INTRA_EXPENSE_COLLECTION, &
                                           RUN_YEAR, &
                                           CASH_TREATMENT, &
                                           LAG_PERIOD, &
                                           CASH_RECEIVABLE_PAYABLE)

                      EXPENSE_ELIM = EXPENSE_ELIM + ASSET_ALLOCATOR * &
                        MONTHLY_DOLLARS(0)
                  ENDIF
                  CLASS_POINTER = CLASS_POINTER + 1
                  IF(CLASS_POINTER > AVAIL_DATA_YEARS) EXIT
                  IF(ASSET_CLASS_LIST(CLASS_POINTER) == 0. .OR. &
                    ASSET_CLASS_LIST(CLASS_POINTER) == -99.) then
                        EXIT
                  end if
               ENDDO
               IF(EXPENSE_ELIM /= 0.) THEN

                  ASSET_ALLOCATOR = 1.0
                  CALL CL_SALES_EXP_ALLOCATION(INTRA_CLASS, &
                                           MONTHLY_DOLLARS, &
                                           ASSET_ALLOCATOR, &
                                           INTRA_EXPENSE_CLASSIFICATION, &
                                           INTRA_EXPENSE_COLLECTION, &
                                           RUN_YEAR, &
                                           CASH_TREATMENT, &
                                           LAG_PERIOD, &
                                           CASH_RECEIVABLE_PAYABLE)
               ENDIF
            ENDIF
!
            ASSET_CLASS_loc = CLASS_ALLOCATION_ID(I)
            ASSET_ALLOCATION_VECTOR = CLASS_ALLOCATION_VECTOR(I)
!
            VOID_LOGICAL=RETURN_ASSET_CLASS_LISTS(ASSET_CLASS_loc, &
                                               ASSET_CLASS_LIST, &
                                               ASSET_ALLOCATION_VECTOR, &
                                               ASSET_ALLOCATION_LIST)
            REVENUE_ELIM = 0.
            CLASS_POINTER = 1
            DO
               ASSET_CLASS_loc = ASSET_CLASS_LIST(CLASS_POINTER)
               CALL CHECK_IF_CLASS_DEFINED(ASSET_CLASS_loc)
               ASSET_CLASS_loc = ASSET_CLASS_loc + 1
               IF(ASSET_CLASS_loc > 0) ASSET_CLASS_loc = &
                ASSET_CLASS_POINTER_loc(ASSET_CLASS_loc)

               ASSET_ALLOCATOR = ASSET_ALLOCATION_LIST(CLASS_POINTER)
               IF(ASSET_ALLOCATOR < 0.) THEN
                  ALLOCATION_VECTOR = ABS(ASSET_ALLOCATOR)
                  CALL GET_ASSET_VAR(ALLOCATION_VECTOR,&
                    DUMMY_TYPE,ALLOCATION_VALUE)
                  ASSET_ALLOCATOR = ALLOCATION_VALUE(RUN_YEAR)
               ENDIF
!
               ASSET_ALLOCATOR = ASSET_ALLOCATOR/100.
!
               IF(ASSET_CLASS_loc <= MAX_ASSET_CLASS_NUM) THEN
                  IF(ACCOUNT_TYPE == 'E') THEN
                     CALL CL_SALES_EXP_ALLOCATION(ASSET_CLASS_loc, &
                                           MONTHLY_DOLLARS, &
                                           ASSET_ALLOCATOR, &
                                           EXPENSE_CLASSIFICATION, &
                                           EXPENSE_COLLECTION, &
                                           RUN_YEAR, &
                                           CASH_TREATMENT, &
                                           LAG_PERIOD, &
                                           CASH_RECEIVABLE_PAYABLE)
                  ELSE
                     CALL CL_SALES_REV_ALLOCATION(ASSET_CLASS_loc, &
                                               MONTHLY_DOLLARS, &
                                               MONTHLY_ENERGY, &
                                               ASSET_ALLOCATOR, &
                                               REVENUE_CLASSIFICATION, &
                                               RUN_YEAR, &
                                               CASH_TREATMENT, &
                                               LAG_PERIOD, &
                                               CASH_RECEIVABLE_PAYABLE, &
                                               CLASS_CUSTOMERS(:,I))
!
                     ALLOCATED_REVENUES = ASSET_ALLOCATOR * &
                        MONTHLY_DOLLARS(0)
                     R_CLASS_REVENUES(ASSET_CLASS_loc) = ALLOCATED_REVENUES &
                         + R_CLASS_REVENUES(ASSET_CLASS_loc)
                     R_CLASS_REVENUES(0) = R_CLASS_REVENUES(0) &
                         + ALLOCATED_REVENUES
                     CALL CLASS_LINKED_TO_PARENT(ASSET_CLASS_loc, &
                        CLASS_LINKED_2_PARENT)
                     IF(.NOT. CLASS_LINKED_2_PARENT) REVENUE_ELIM = &
                        REVENUE_ELIM + ALLOCATED_REVENUES
                  ENDIF
               ENDIF
               CLASS_POINTER = CLASS_POINTER + 1
               IF(CLASS_POINTER > AVAIL_DATA_YEARS) EXIT
               IF(ASSET_CLASS_LIST(CLASS_POINTER) == 0. .OR. &
                ASSET_CLASS_LIST(CLASS_POINTER) == -99.) then
                    EXIT
               end if
            ENDDO
            IF(INTRA_COMPANY_TRANSACTION == 'Y') THEN
               INTRA_COMPANY_REVENUE = INTRA_COMPANY_REVENUE + REVENUE_ELIM

               ASSET_ALLOCATOR = 1.
               CALL CL_SALES_REV_ALLOCATION(INTRA_CLASS, &
                                            MONTHLY_DOLLARS, &
                                            MONTHLY_ENERGY, &
                                            ASSET_ALLOCATOR, &
                                            REVENUE_CLASSIFICATION, &
                                            RUN_YEAR, &
                                            CASH_TREATMENT, &
                                            LAG_PERIOD, &
                                            CASH_RECEIVABLE_PAYABLE, &
                                            DUMMY_CUSTOMERS) ! THIS IS NUM CUSTOMERS
            ENDIF
!
! WRITE OUTPUT
!
            IF(.TRUE.) THEN
               VARIABLE_TYPE_NAME(0) = "Energy Dollars"
               VARIABLE_TYPE_NAME(1) = "Demand Dollars"
               VARIABLE_TYPE_NAME(2) = "Customer Dollars"
               VARIABLE_TYPE_NAME(3) = "Total Dollars"
               VARIABLE_TYPE_NAME(4) = "Energy Rate"
               VARIABLE_TYPE_NAME(5) = "Demand Rate"
               VARIABLE_TYPE_NAME(6) = "Customer Rate"
               VARIABLE_TYPE_NAME(7) = "Energy Amount"
               VARIABLE_TYPE_NAME(8) = "Demand"
               VARIABLE_TYPE_NAME(9) = "Customers"
               VARIABLE_TYPE_NAME(10) = "Billing Demand"
               ACCOUNT_TYPE_NAME = "Revenues"
               IF(ACCOUNT_TYPE == 'E') ACCOUNT_TYPE_NAME = "Expenses"
               EXT_RATE_CLASS_NAME = RATE_CLASS_NAME(I)
               DO MO = 0, 12
                  VARIABLE_VALUE(MO,0) = MONTHLY_ENERGY_DOLLARS(MO)
                  VARIABLE_VALUE(MO,1) = MONTHLY_DEMAND_DOLLARS(MO)
                  VARIABLE_VALUE(MO,2) = MONTHLY_CUSTOMER_DOLLARS(MO)
                  VARIABLE_VALUE(MO,3) = MONTHLY_DOLLARS(MO)
                  VARIABLE_VALUE(MO,7) = MONTHLY_ENERGY(MO)/1000.
                  IF(MO == 0) THEN
                     VARIABLE_VALUE(MO,4) = THE_RATIO_OF_A_TO_B( &
                        MONTHLY_ENERGY_DOLLARS(MO),VARIABLE_VALUE(MO,7))
                     VARIABLE_VALUE(MO,5) = 1000*THE_RATIO_OF_A_TO_B( &
                        MONTHLY_DEMAND_DOLLARS(MO),BILLING_DEMAND(MO))
                     VARIABLE_VALUE(MO,6) = THE_RATIO_OF_A_TO_B(&
                        MONTHLY_CUSTOMER_DOLLARS(MO),CLASS_CUSTOMERS(MO,I))
                     VARIABLE_VALUE(MO,8) = CLASS_DEMAND(MO,I)/12.
                     VARIABLE_VALUE(MO,9) = CLASS_CUSTOMERS(MO,I)/12.
                     VARIABLE_VALUE(MO,10) = BILLING_DEMAND(MO)/12.
                  ELSE
                     VARIABLE_VALUE(MO,4) = ENERGY_PRICE(MO)
                     VARIABLE_VALUE(MO,5) = DEMAND_PRICE(MO)
                     VARIABLE_VALUE(MO,6) = CUSTOMER_PRICE(MO)
                     VARIABLE_VALUE(MO,8) = CLASS_DEMAND(MO,I)
                     VARIABLE_VALUE(MO,9) = CLASS_CUSTOMERS(MO,I)
                     VARIABLE_VALUE(MO,10) = BILLING_DEMAND(MO)
                  ENDIF
!
                  WRITE(RC_UNIT_NO,REC=NEXT_REC) PRT_ENDPOINT(), &
                                         FLOAT(get_BASE_YEAR()+R_YEAR), &
                                         ACCOUNT_TYPE_NAME, &
                                         EXT_RATE_CLASS_NAME, &
                                         SHORT_MONTH_NAMES(MO), &
                                        (VARIABLE_VALUE(MO,J),J=0,10)
                  NEXT_REC = NEXT_REC + 1
               ENDDO
            ENDIF
         ENDDO ! READ RECORDS LOOP
!
! SUM EACH CASH VARIABLE TO AN ANNUAL AMOUNT
!
         DO ASSET_CLASS_loc = -1, NUM_OF_ASSET_CLASSES
            DO REV_TYPE = 1, LAST_INCOME_LINE
               CASH_AMOUNTS_RECEIVABLE(0,ASSET_CLASS_loc,REV_TYPE) = 0.
               DO MO = 1, 12
                  CASH_AMOUNTS_RECEIVABLE(0,ASSET_CLASS_loc,REV_TYPE) = &
                     CASH_AMOUNTS_RECEIVABLE(0,ASSET_CLASS_loc,REV_TYPE) &
                     + CASH_AMOUNTS_RECEIVABLE(MO,ASSET_CLASS_loc,REV_TYPE)
               ENDDO
            ENDDO

            DO EXTYPE = 1, LAST_EXPENSE_ITEM
               CASH_AMOUNTS_PAYABLE(0,ASSET_CLASS_loc,EXTYPE) = 0.
               DO MO = 1, 12
                  CASH_AMOUNTS_PAYABLE(0,ASSET_CLASS_loc,EXTYPE) = &
                           CASH_AMOUNTS_PAYABLE(0,ASSET_CLASS_loc,EXTYPE) &
                         + CASH_AMOUNTS_PAYABLE(MO,ASSET_CLASS_loc,EXTYPE)
               ENDDO
            ENDDO
         ENDDO
!
! TRANSFER TO COMMON CODE
!
         CALL SAVE_REVENUE_DATA_FORECASTS(NUM_OF_ASSET_CLASSES, &
                                          REVENUES_MONTHLY, &
                                          CASH_AMOUNTS_RECEIVABLE, &
                                          ENERGY_MONTHLY)
         CALL SAVE_EXPENSE_DATA_FORECASTS(NUM_OF_ASSET_CLASSES, &
                                          EXPENSES_MONTHLY, &
                                          CASH_AMOUNTS_PAYABLE)
      RETURN


      ENTRY GET_CPL_JURIS_ALLOCATION_DATA(R_YEAR)

!
!
         IF(FILE_TABLES == 0) RETURN
         RUN_YEAR = MIN(AVAIL_DATA_YEARS,R_YEAR)
!
! ZERO ARRAYS
!
         CPL_JURISDICTIONAL_INFOR = 0.
!
         CPL_NC_PEAK = 0.
         CPL_SC_PEAK = 0.
!
!
! READ SALES FILE
!
         IREC = RUN_YEAR - AVAIL_DATA_YEARS
         DO I = 1, FILE_TABLES
            IREC = IREC + AVAIL_DATA_YEARS
            READ(SALES_UNIT,REC=IREC,IOSTAT=IOS) DELETE,TEMP_YR,&
                RATE_CLASS_NAME(I),MARKET_ENERGY_PRICE,MARKET_DEMAND_PRICE, &
                          MARKET_CUSTOMER_PRICE,CLASS_ALLOCATION_ID(I), &
                              CLASS_ALLOCATION_VECTOR(I), &
                         (ENERGY(MO),DEMAND(MO),CUSTOMERS(MO),MO=1,12),&
                             INTRA_COMPANY_TRANSACTION,INTRA_ASSET_CLASS_ID, &
                          INTRA_ASSET_CLASS_ALLOCATION, &
                       INTRA_EXPENSE_CLASSIFICATION,INTRA_EXPENSE_COLLECTION, &
                          REVENUE_CLASSIFICATION,ENERGY_UNITS, &
                          MONTHLY_PRICE_INFORMATION, &
                          CASH_TREATMENT, &
                          LAG_PERIOD, &
                          CASH_RECEIVABLE_PAYABLE, &
                          CPL_JURISDICTION, &
                          OUC_CLASSIFICATIONS, &
                          OUC_TAX_TREATMENT, &
                          OUC_BASE_EL_STAB_ADDER, &
                          OUC_FUEL_STAB_ADDER, &
                          OUC_CUST_RETENTION_ADDER, &
                          OUC_BASE_WATER_STAB_ADDER, &
                          OUC_PLACEHOLDER_FUND_A, &
                          OUC_PLACEHOLDER_FUND_B, &
                          ENERGY_LOSS_FACTOR, &
                          DEMAND_LOSS_FACTOR
            IF(IOS /= 0) EXIT
            IF(DELETE >= 8) CYCLE
!
            SALES_CLASSIFICATION = CPL_SALES_CLASS(CPL_JURISDICTION)
            ANNUAL_PEAK_DEMAND = 0.
            ANNUAL_ENERGY = 0.
            TEMP_PEAK = 0.
            DO MO = 1, 12
               IF(ENERGY(MO) < 1. .AND. RUN_YEAR > 1) THEN
                  CLASS_ENERGY(MO,I)=CLASS_ENERGY(MO,I)*(1.+ENERGY(MO))
               ELSE
                  IF(INDEX(ENERGY_UNITS,'MWh') /= 0) THEN
                     CLASS_ENERGY(MO,I) = ENERGY(MO)/1000.
                  ELSE
                     CLASS_ENERGY(MO,I) = ENERGY(MO)
                  ENDIF
               ENDIF
               IF(ENERGY_LOSS_FACTOR < 100.0) THEN
                  CLASS_ENERGY(MO,I) = CLASS_ENERGY(MO,I) / &
                    (1. - ENERGY_LOSS_FACTOR/100.)
               ENDIF
               IF(DEMAND(MO) < 1. .AND. RUN_YEAR > 1) THEN
                  CLASS_DEMAND(MO,I)=CLASS_DEMAND(MO,I)*(1.+DEMAND(MO))
               ELSE
                  CLASS_DEMAND(MO,I) = DEMAND(MO)
               ENDIF

               !CA 3.8.99
               IF (DEMAND_LOSS_FACTOR < 100.) THEN
                  CLASS_DEMAND(MO,I) = CLASS_DEMAND(MO,I) * &
                    (1. + DEMAND_LOSS_FACTOR/100.)
               ENDIF
               ANNUAL_ENERGY = ANNUAL_ENERGY + CLASS_ENERGY(MO,I)
               ANNUAL_PEAK_DEMAND = MAX(ANNUAL_PEAK_DEMAND,CLASS_DEMAND(MO,I))
                IF(CUSTOMERS(MO) < 1. .AND. RUN_YEAR > 1) THEN
                  CLASS_CUSTOMERS(MO,I) = CLASS_CUSTOMERS(MO,I) * &
                    (1.+CUSTOMERS(MO))
               ELSE

                  CLASS_CUSTOMERS(MO,I) = CUSTOMERS(MO)
               ENDIF

               CPL_JURISDICTIONAL_INFOR(MO,SALES_CLASSIFICATION, &
                                                           cpl_mwh) = &
                    CPL_JURISDICTIONAL_INFOR(MO,SALES_CLASSIFICATION, &
                                                           cpl_mwh) + &
                                                   CLASS_ENERGY(MO,I)
               CPL_JURISDICTIONAL_INFOR(MO,SALES_CLASSIFICATION, &
                                                           cpl_dmd) = &
                    CPL_JURISDICTIONAL_INFOR(MO,SALES_CLASSIFICATION, &
                                                           cpl_dmd) + &
                                                   CLASS_DEMAND(MO,I)

               CPL_JURISDICTIONAL_INFOR(MO,SALES_CLASSIFICATION, &
                                                      cpl_customers) = &
                     CPL_JURISDICTIONAL_INFOR(MO,SALES_CLASSIFICATION, &
                                                      cpl_customers) + &
                                                 CLASS_CUSTOMERS(MO,I)

            ! CA 3.8.98

            TEMP_PEAK = CLASS_DEMAND(7,I)



            ENDDO

           CPL_JURISDICTIONAL_INFOR(0,SALES_CLASSIFICATION,cpl_mwh) = &
                     CPL_JURISDICTIONAL_INFOR(0,SALES_CLASSIFICATION, &
                                                           cpl_mwh) + &
                                                        ANNUAL_ENERGY


            REV_TYPE = INCOME_STATEMENT_POSITION(REVENUE_CLASSIFICATION)
            IF(REV_TYPE == 11) THEN
               A = 1
            ELSEIF(REV_TYPE == 12) THEN
               A = 2
            ELSEIF(REV_TYPE == 13) THEN
               A = 3
            ELSEIF(REV_TYPE == 18) THEN
               A = 4
            ELSE
               A = 0
            ENDIF
            IF (SALES_CLASSIFICATION == 1 .AND. A/=0) THEN
               CPL_NC_PEAK = CPL_NC_PEAK + TEMP_PEAK * NCP_MULTIPLIER (A)
!
            ELSEIF (SALES_CLASSIFICATION == 2 .AND. A/=0) THEN
               CPL_SC_PEAK = CPL_SC_PEAK + TEMP_PEAK * NCP_MULTIPLIER (A)
!

            ENDIF


         ENDDO ! READ RECORDS LOOP
      RETURN
!

      ENTRY CPL_JURIS_PEAK_N_SALES( SYSTEM_PEAK_SUMMER_NC, &
        SYSTEM_PEAK_SUMMER_SC,SYSTEM_PEAK_SUMMER_WHOLE, &
        SYSTEM_PEAK_SUMMER_NCEMPA,SYSTEM_PEAK_SUMMER_NCEMC, &
            SYSTEM_PEAK_SUMMER, &
                                    SYSTEM_PEAK_WINTER_NC, &
                                    SYSTEM_PEAK_WINTER_SC, &
                                    SYSTEM_PEAK_WINTER_WHOLE, &
                                    SYSTEM_PEAK_WINTER_NCEMPA, &
                                    SYSTEM_PEAK_WINTER_NCEMC, &
                                    SYSTEM_PEAK_WINTER, &
                                    SYSTEM_NON_PEAK_NC_SC, &
                                    SYSTEM_NON_PEAK_NC, &
                                    SYSTEM_NON_PEAK_SC, &
                                    SYSTEM_PEAK_WHOLE_12SUM, &
                                    SYSTEM_PEAK_12SUM, &
                                    SYSTEM_ENERGY_SALES_NC, &
                                    SYSTEM_ENERGY_SALES_SC, &
                                    SYSTEM_ENERGY_SALES_WHOLE, &
                                    SYSTEM_ENERGY_SALES_NCEMPA, &
                                    SYSTEM_ENERGY_SALES_NCEMC, &
                                    SYSTEM_ENERGY_SALES)

! TRY TO FIGURE OUT HOW TO GET THE VALUE FROM DEMAND PEAK



      IF(WVPA()) RETURN
      SYSTEM_PEAK_SUMMER_NC =  0.
      SYSTEM_PEAK_SUMMER_SC =  0.
      SYSTEM_PEAK_SUMMER_WHOLE  = 0.
      SYSTEM_PEAK_SUMMER_NCEMPA = 0.
      SYSTEM_PEAK_SUMMER_NCEMC  = 0.
      SYSTEM_PEAK_SUMMER = 0.
      PEAK_MONTH = 1
      DO MO = 4, 9
         TEMP_PEAK = 0.
         DO SALES_CLASSIFICATION = 1,7

            TEMP_PEAK = TEMP_PEAK + &
                CPL_JURISDICTIONAL_INFOR(MO,SALES_CLASSIFICATION,cpl_dmd)

         ENDDO
          IF (TEMP_PEAK > SYSTEM_PEAK_SUMMER) THEN
            SYSTEM_PEAK_SUMMER = TEMP_PEAK
            PEAK_MONTH = MO
         ENDIF
      ENDDO

      SYSTEM_PEAK_SUMMER_NC = CPL_JURISDICTIONAL_INFOR( &
        PEAK_MONTH,NC_SALES_CLASS,cpl_dmd)
      SYSTEM_PEAK_SUMMER_SC = CPL_JURISDICTIONAL_INFOR( &
        PEAK_MONTH,SC_SALES_CLASS,cpl_dmd)

!     CA ADDED 2/25/99

      SYSTEM_PEAK_SUMMER_WHOLE = &
        CPL_JURISDICTIONAL_INFOR(PEAK_MONTH,WHOLE_SALES_CLASS,cpl_dmd)

      SYSTEM_PEAK_SUMMER_NCEMPA = &
        CPL_JURISDICTIONAL_INFOR(PEAK_MONTH,NCEMPA_SALES_CLASS,cpl_dmd)

!

      SYSTEM_PEAK_SUMMER_NCEMC = &
        CPL_JURISDICTIONAL_INFOR(PEAK_MONTH,NCEMC_SALES_CLASS,cpl_dmd)


!     2/28/99. GAT.

      CALL GET_TOTAL_RETAINED(PA_TOTAL_RETAINED)
      CALL GET_PA_FIRM_N_SEPA(PA_FIRM_PURCHASE_CAPACITY,PA_SEPA_CAPACITY)
      CALL GET_EMC_SEPA_CAPACITY(EMC_SEPA_CAPACITY)

      SYSTEM_PEAK_SUMMER_NCEMPA = SYSTEM_PEAK_SUMMER_NCEMPA - &
                                       PA_TOTAL_RETAINED - &
                                       PA_SEPA_CAPACITY - &
                                       PA_FIRM_PURCHASE_CAPACITY
      SYSTEM_PEAK_SUMMER_NCEMC = SYSTEM_PEAK_SUMMER_NCEMC - &
                                       EMC_SEPA_CAPACITY
      SYSTEM_PEAK_SUMMER = &
               SYSTEM_PEAK_SUMMER_NC + &
               SYSTEM_PEAK_SUMMER_SC + &
               SYSTEM_PEAK_SUMMER_WHOLE + &
               SYSTEM_PEAK_SUMMER_NCEMPA + &
               SYSTEM_PEAK_SUMMER_NCEMC


      SYSTEM_PEAK_WINTER_NC =  0.
      SYSTEM_PEAK_WINTER_SC = 0.
      SYSTEM_PEAK_WINTER_WHOLE = 0.
      SYSTEM_PEAK_WINTER_NCEMPA = 0.
      SYSTEM_PEAK_WINTER_NCEMC = 0.
      SYSTEM_PEAK_WINTER = 0.
      PEAK_MONTH = 1
      DO MO = 1, 12
         IF(MO > 3 .AND. MO < 10) CYCLE
         TEMP_PEAK = 0.
         DO SALES_CLASSIFICATION = 1,7

            TEMP_PEAK = TEMP_PEAK + &
                CPL_JURISDICTIONAL_INFOR(MO,SALES_CLASSIFICATION,cpl_dmd)
            IF(TEMP_PEAK > SYSTEM_PEAK_WINTER) THEN
               SYSTEM_PEAK_WINTER = TEMP_PEAK
               PEAK_MONTH = MO
            ENDIF

         ENDDO
      ENDDO
      SYSTEM_PEAK_WINTER_NC = &
        CPL_JURISDICTIONAL_INFOR(PEAK_MONTH,NC_SALES_CLASS,cpl_dmd)

      SYSTEM_PEAK_WINTER_SC = CPL_JURISDICTIONAL_INFOR &
        (PEAK_MONTH,SC_SALES_CLASS,cpl_dmd)

      SYSTEM_PEAK_WINTER_WHOLE = &
        CPL_JURISDICTIONAL_INFOR(PEAK_MONTH,WHOLE_SALES_CLASS,cpl_dmd)

      SYSTEM_PEAK_WINTER_NCEMPA = &
        CPL_JURISDICTIONAL_INFOR(PEAK_MONTH,NCEMPA_SALES_CLASS,cpl_dmd)

      SYSTEM_PEAK_WINTER_NCEMC = &
        CPL_JURISDICTIONAL_INFOR(PEAK_MONTH,NCEMC_SALES_CLASS,cpl_dmd)

      SYSTEM_PEAK_WINTER_NCEMPA = SYSTEM_PEAK_WINTER_NCEMPA - &
                                       PA_TOTAL_RETAINED - &
                                       PA_SEPA_CAPACITY - &
                                       PA_FIRM_PURCHASE_CAPACITY
      SYSTEM_PEAK_WINTER_NCEMC = SYSTEM_PEAK_WINTER_NCEMC - EMC_SEPA_CAPACITY
      SYSTEM_PEAK_WINTER = &
               SYSTEM_PEAK_WINTER_NC + &
               SYSTEM_PEAK_WINTER_SC + &
               SYSTEM_PEAK_WINTER_WHOLE + &
               SYSTEM_PEAK_WINTER_NCEMPA + &
               SYSTEM_PEAK_WINTER_NCEMC


! go to

      SYSTEM_NON_PEAK_NC_SC = 0.
      SYSTEM_NON_PEAK_NC = 0.
      SYSTEM_NON_PEAK_SC = 0.
      PEAK_MONTH = 1
      TEMP_PEAK = 0.
      SYSTEM_NON_PEAK_NC = 0.
      SYSTEM_NON_PEAK_SC = 0.


         SYSTEM_NON_PEAK_NC =  CPL_NC_PEAK
         SYSTEM_NON_PEAK_SC =  CPL_SC_PEAK
         SYSTEM_NON_PEAK_NC_SC = SYSTEM_NON_PEAK_NC + SYSTEM_NON_PEAK_SC
!
!
!
      SYSTEM_ENERGY_SALES_NC = 0.
      SYSTEM_ENERGY_SALES_SC = 0.
      SYSTEM_ENERGY_SALES_WHOLE = 0.
      SYSTEM_ENERGY_SALES_NCEMPA = 0.
      SYSTEM_ENERGY_SALES_NCEMC = 0.
      SYSTEM_ENERGY_SALES = 0.
!
      MO = 0
      SYSTEM_ENERGY_SALES_NC = SYSTEM_ENERGY_SALES_NC + &
        1000.*CPL_JURISDICTIONAL_INFOR(MO,NC_SALES_CLASS,cpl_mwh)
      SYSTEM_ENERGY_SALES_SC = SYSTEM_ENERGY_SALES_SC + &
        1000.*CPL_JURISDICTIONAL_INFOR(MO,SC_SALES_CLASS,cpl_mwh)
      SYSTEM_ENERGY_SALES_WHOLE = SYSTEM_ENERGY_SALES_WHOLE +  &
        1000.* CPL_JURISDICTIONAL_INFOR(MO,WHOLE_SALES_CLASS,cpl_mwh)
       SYSTEM_ENERGY_SALES_NCEMPA = SYSTEM_ENERGY_SALES_NCEMPA + &
        1000.* CPL_JURISDICTIONAL_INFOR(MO,NCEMPA_SALES_CLASS,cpl_mwh)
       SYSTEM_ENERGY_SALES_NCEMC = SYSTEM_ENERGY_SALES_NCEMC + &
        1000.* CPL_JURISDICTIONAL_INFOR(MO,NCEMC_SALES_CLASS,cpl_mwh)


! GAT. 2/28/99.


      SYSTEM_ENERGY_SALES = SYSTEM_ENERGY_SALES + &
                              SYSTEM_ENERGY_SALES_NC + &
                              SYSTEM_ENERGY_SALES_SC + &
                              SYSTEM_ENERGY_SALES_WHOLE + &
                              SYSTEM_ENERGY_SALES_NCEMPA + &
                              SYSTEM_ENERGY_SALES_NCEMC


!  CPL_JURISDICTIONAL_INFOR(MO,SALES_CLASSIFICATION,cpl_mwh)
      SYSTEM_PEAK_12SUM = 0.
      SYSTEM_PEAK_WHOLE_12SUM = 0.

      DO MO = 1, 12
!
         SYSTEM_PEAK_WHOLE_12SUM = SYSTEM_PEAK_WHOLE_12SUM + &
            CPL_JURISDICTIONAL_INFOR(MO,WHOLE_SALES_CLASS,cpl_dmd)
!
         DO SALES_CLASSIFICATION = 1,6

            IF( SALES_CLASSIFICATION == 4) CYCLE
            SYSTEM_PEAK_12SUM = SYSTEM_PEAK_12SUM + &
                CPL_JURISDICTIONAL_INFOR(MO,SALES_CLASSIFICATION,cpl_dmd)

         ENDDO
            SYSTEM_PEAK_12SUM = SYSTEM_PEAK_12SUM - &
                                          PA_TOTAL_RETAINED - &
                                          PA_SEPA_CAPACITY - &
                                          PA_FIRM_PURCHASE_CAPACITY
      ENDDO
!
!


      RETURN

      ENTRY IMPA_CUSTOMERS_SALES(R_CLASS,R_CLASS_CUSTOMERS, &
                                 R_MONTHLY_ENERGY)

         R_CLASS_CUSTOMERS(:) = 0.
         R_MONTHLY_ENERGY(:) = 0.
         IF(R_CLASS <= MAX_ASSET_CLASS_NUM .AND. MAX_ASSET_CLASS_NUM > 0) THEN
            IF(R_CLASS <=0) THEN
               ASSET_CLASS_loc = R_CLASS
            ELSE
               ASSET_CLASS_loc = ASSET_CLASS_POINTER_loc(R_CLASS)
            ENDIF
            IF(ASSET_CLASS_loc > 0 .OR. R_CLASS <= 0) THEN
               DO MO = 0,12
                  R_CLASS_CUSTOMERS(MO) = SUM(ASSET_CLASS_CUSTOMERS(MO,:))
                  R_MONTHLY_ENERGY(MO) = SUM(ASSET_CLASS_ENERGY(MO,:))
               ENDDO
            ENDIF
         ENDIF
      RETURN

      ENTRY CL_SALES_REV_ALLOCATION(R_CLASS, &
                                    R_MONTHLY_REVENUES, &
                                    R_MONTHLY_ENERGY, &
                                    R_ASSET_ALLOCATOR, &
                                    R_REVENUE_CLASSIFICATION, &
                                    R_RUN_YEAR, &
                                    R_CASH_TREATMENT, &
                                    R_LAG_PERIOD, &
                                    R_CASH_RECEIVABLE_PAYABLE, &
                                    R_CLASS_CUSTOMERS)

!
         REV_TYPE = INCOME_STATEMENT_POSITION(R_REVENUE_CLASSIFICATION)
         IF(REV_TYPE >= 1 .AND. REV_TYPE <= LAST_INCOME_LINE) THEN
            DO MO = 0, 12
               MONTHLY_ALLOCATED_REVENUES(MO) = &
                R_MONTHLY_REVENUES(MO)*R_ASSET_ALLOCATOR
               REVENUES_MONTHLY(MO,R_CLASS,REV_TYPE) = &
                MONTHLY_ALLOCATED_REVENUES(MO) + REVENUES_MONTHLY(MO,R_CLASS,REV_TYPE)
!
               MONTHLY_ALLOCATED_ENERGY = &
                R_MONTHLY_ENERGY(MO)*R_ASSET_ALLOCATOR
               ENERGY_MONTHLY(MO,R_CLASS,REV_TYPE) = &
                MONTHLY_ALLOCATED_ENERGY + ENERGY_MONTHLY(MO,R_CLASS,REV_TYPE)
               ASSET_CLASS_ENERGY(MO,R_CLASS) = &
                ASSET_CLASS_ENERGY(MO,R_CLASS) + MONTHLY_ALLOCATED_ENERGY
               ASSET_CLASS_CUSTOMERS(MO,R_CLASS) = &
                ASSET_CLASS_CUSTOMERS(MO,R_CLASS) + &
                R_CLASS_CUSTOMERS(MO)*R_ASSET_ALLOCATOR
            ENDDO
            ACCRUED_CASH_BY_MONTH = 0.
            IF(R_RUN_YEAR == 1) THEN
               LAGGED_PATTERN = trim(R_CASH_RECEIVABLE_PAYABLE)// &
               ',,,,,,,,,,,,,,,,,,,,'
               READ(LAGGED_PATTERN,*) ACCRUED_CASH_BY_MONTH
               DO MO = 1, 12

                  CASH_AMOUNTS_RECEIVABLE(MO,R_CLASS,REV_TYPE) = &
                       CASH_AMOUNTS_RECEIVABLE(MO,R_CLASS,REV_TYPE) &
                       + R_ASSET_ALLOCATOR * ACCRUED_CASH_BY_MONTH(MO)
                  CASH_AMOUNTS_RECEIVABLE(0,R_CLASS,REV_TYPE) = &
                      CASH_AMOUNTS_RECEIVABLE(0,R_CLASS,REV_TYPE) &
                      + R_ASSET_ALLOCATOR * ACCRUED_CASH_BY_MONTH(MO)
               ENDDO
            ENDIF
            LAG_BY_MONTH = 0.
            IF(R_CASH_TREATMENT == 'B') THEN
               LAG_BY_MONTH(1) = 100.
            ELSE ! IF(R_CASH_TREATMENT == 'L') THEN
               LAGGED_PATTERN = trim(R_LAG_PERIOD)//',,,,,,,,,,,,,,,,,,,,'
               READ(LAGGED_PATTERN,*) LAG_BY_MONTH
              IF(LAG_BY_MONTH(1) <= 1. .AND. LAG_BY_MONTH(2) == 0.)THEN
                  LAG_BY_MONTH(1) = 100.*(1. - MIN(1.,LAG_BY_MONTH(1)))
                  LAG_BY_MONTH(2) = 100. - LAG_BY_MONTH(1)
               ENDIF
            ENDIF
            IF(R_CASH_TREATMENT /= 'N') THEN
               TWO_YR_CASH_DISTRIBUTION = 0.
               DO MO = 1, 12
                  LAG_MO = 1
                  DO MO1 = MO, MIN(MO + 11,24)
                     TWO_YR_CASH_DISTRIBUTION(MO1) = &
                                       TWO_YR_CASH_DISTRIBUTION(MO1) &
                                      + MONTHLY_ALLOCATED_REVENUES(MO) &
                                           * LAG_BY_MONTH(LAG_MO)/100.
                     LAG_MO = LAG_MO + 1
                  ENDDO
               ENDDO
!
               DO MO = 1, 12
                  CASH_AMOUNTS_RECEIVABLE(MO,R_CLASS,REV_TYPE) = &
                          CASH_AMOUNTS_RECEIVABLE(MO,R_CLASS,REV_TYPE) &
                            + TWO_YR_CASH_DISTRIBUTION(MO)
                  CASH_AMOUNTS_RECEIVABLE(0,R_CLASS,REV_TYPE) = &
                           CASH_AMOUNTS_RECEIVABLE(0,R_CLASS,REV_TYPE) &
                            + TWO_YR_CASH_DISTRIBUTION(MO)
               ENDDO
! STORE END OF YEAR CARRY OVER
               DO MO = 1, 12
                  CASH_CARRY_OVER_RECEIVABLE(MO,R_CLASS,REV_TYPE) = &
                       CASH_CARRY_OVER_RECEIVABLE(MO,R_CLASS,REV_TYPE) &
                         + TWO_YR_CASH_DISTRIBUTION(MO+12)
               ENDDO
            ENDIF
         ENDIF
      RETURN

      ENTRY CL_SALES_REV_ELIMINATION(R_CLASS, &
                                     R_REVENUE_ELIM, &
                                     R_REVENUE_CLASSIFICATION)

!
         REV_TYPE = INCOME_STATEMENT_POSITION(R_REVENUE_CLASSIFICATION)
         IF(REV_TYPE >= 1 .AND. REV_TYPE <= LAST_INCOME_LINE) THEN
            REVENUES_MONTHLY(0,R_CLASS,REV_TYPE) = R_REVENUE_ELIM + &
                REVENUES_MONTHLY(0,R_CLASS,REV_TYPE)
         ENDIF
      RETURN

      ENTRY CL_SALES_EXP_ALLOCATION(R_CLASS, &
                                    R_MONTHLY_EXPENSES, &
                                    R_ASSET_ALLOCATOR, &
                                    R_EXPENSE_CLASSIFICATION, &
                                    R_EXPENSE_COLLECTION, &
                                    R_RUN_YEAR, &
                                    R_CASH_TREATMENT, &
                                    R_LAG_PERIOD, &
                                    R_CASH_RECEIVABLE_PAYABLE)

!
         EXTYPE = INCOME_STATEMENT_POSITION(R_EXPENSE_CLASSIFICATION)
         IF(EXTYPE < 9 .OR. EXTYPE > LAST_EXPENSE_ITEM) RETURN
!
         IF(INDEX(R_EXPENSE_COLLECTION,'BTL') /= 0) THEN
            IF(EXPENSE_TYPE_IS_CASH(EXTYPE)) EXTYPE = BTLExpenses
            IF(EXTYPE == Amortization) EXTYPE = BTLAmortization
         ENDIF
!
         DO MO = 0, 12
            MONTHLY_ALLOCATED_EXPENSES(MO) = &
            R_MONTHLY_EXPENSES(MO)*R_ASSET_ALLOCATOR
            EXPENSES_MONTHLY(MO,R_CLASS,EXTYPE) = &
                                   MONTHLY_ALLOCATED_EXPENSES(MO) &
                                  + EXPENSES_MONTHLY(MO,R_CLASS,EXTYPE)
         ENDDO
!
         ACCRUED_CASH_BY_MONTH = 0.
         IF(R_RUN_YEAR == 1) THEN
            LAGGED_PATTERN = trim(R_CASH_RECEIVABLE_PAYABLE)// &
                ',,,,,,,,,,,,,,,,,,,,'
            READ(LAGGED_PATTERN,*) ACCRUED_CASH_BY_MONTH
            DO MO = 1, 12
               CASH_AMOUNTS_PAYABLE(1,R_CLASS,EXTYPE) = &
                       CASH_AMOUNTS_PAYABLE(1,R_CLASS,EXTYPE) &
                       + R_ASSET_ALLOCATOR * ACCRUED_CASH_BY_MONTH(MO)
               CASH_AMOUNTS_PAYABLE(0,R_CLASS,EXTYPE) = &
                      CASH_AMOUNTS_PAYABLE(0,R_CLASS,EXTYPE) &
                      + R_ASSET_ALLOCATOR * ACCRUED_CASH_BY_MONTH(MO)
            ENDDO
         ENDIF


         LAG_BY_MONTH = 0.
         IF(R_CASH_TREATMENT == 'B') THEN
            LAG_BY_MONTH(1) = 100.
         ELSE ! IF(R_CASH_TREATMENT == 'L') THEN
            LAGGED_PATTERN = trim(R_LAG_PERIOD)//',,,,,,,,,,,,,,,,,,,,'
            READ(LAGGED_PATTERN,*) LAG_BY_MONTH
            IF(LAG_BY_MONTH(1) <= 1. .AND. LAG_BY_MONTH(2) == 0.)THEN
               LAG_BY_MONTH(1) = 100.*(1. - MIN(1.,LAG_BY_MONTH(1)))
               LAG_BY_MONTH(2) = 100. - LAG_BY_MONTH(1)
            ENDIF
         ENDIF
         IF(R_CASH_TREATMENT /= 'N') THEN
            TWO_YR_CASH_DISTRIBUTION = 0.
            DO MO = 1, 12
               LAG_MO = 1
               DO MO1 = MO, MIN(MO + 11,24)
                  TWO_YR_CASH_DISTRIBUTION(MO1) = &
                                        TWO_YR_CASH_DISTRIBUTION(MO1) &
                                       + MONTHLY_ALLOCATED_EXPENSES(MO) &
                                            * LAG_BY_MONTH(LAG_MO)/100.
                  LAG_MO = LAG_MO + 1
               ENDDO
            ENDDO
!
            DO MO = 1, 12
               CASH_AMOUNTS_PAYABLE(MO,R_CLASS,EXTYPE) = &
                            CASH_AMOUNTS_RECEIVABLE(MO,R_CLASS,EXTYPE) &
                            + TWO_YR_CASH_DISTRIBUTION(MO)
               CASH_AMOUNTS_PAYABLE(0,R_CLASS,EXTYPE) = &
                             CASH_AMOUNTS_RECEIVABLE(0,R_CLASS,EXTYPE) &
                            + TWO_YR_CASH_DISTRIBUTION(MO)
            ENDDO
! STORE END OF YEAR CARRY OVER
            DO MO = 1, 12
               CASH_CARRY_OVER_PAYABLE(MO,R_CLASS,EXTYPE) = &
                            CASH_CARRY_OVER_PAYABLE(MO,R_CLASS,EXTYPE) &
                            + TWO_YR_CASH_DISTRIBUTION(MO+12)
            ENDDO
         ENDIF
      RETURN

      ENTRY CL_SALES_EXP_ELIMINATION(R_CLASS, &
                                     R_EXPENSES_ELIM, &
                                     R_EXPENSE_CLASSIFICATION, &
                                     R_EXPENSE_COLLECTION)

!
         EXTYPE = INCOME_STATEMENT_POSITION(R_EXPENSE_CLASSIFICATION)
         IF(EXTYPE < 9 .OR. EXTYPE > LAST_EXPENSE_ITEM) RETURN
!

         IF(INDEX(R_EXPENSE_COLLECTION,'BTL') /= 0) THEN
            IF(EXPENSE_TYPE_IS_CASH(EXTYPE)) EXTYPE = BTLExpenses
            IF(EXTYPE == Amortization) EXTYPE = BTLAmortization
         ENDIF
         EXPENSES_MONTHLY(0,R_CLASS,EXTYPE) = R_EXPENSES_ELIM + &
            EXPENSES_MONTHLY(0,R_CLASS,EXTYPE)
      RETURN

      ENTRY CPL_INT_DEFERRED_FUEL_ACCNTING(R_MASTER_CLASS_LIST, &
                                           R_CLASS_TYPE)


         IF(WVPA()) RETURN
         FILE_NAME = get_dfb_filename_II()

         SC_CALCULATION_START_YR = 1990
         NC_CALCULATION_START_YR = 1990
         SC_CUMULATIVE_FUEL_COST = 0.
         SC_CUMULATIVE_ENERGY_SALES = 0.
         SC_CUMULATIVE_JURIS_SALES = 0.
         DO YR = 1, 7
            USE_BUDGET_FORECAST_IN(YR) = 'Y'
         ENDDO

         SC_FUEL_FACTOR_INPUT = 0.
         NC_FUEL_FACTOR_INPUT = 0.
         NC_EMF_FACTOR_INPUT = 0.
         OPEN(10,FILE=FILE_NAME)

         CPL_DEFERRED_FUEL_NOT_ACTIVE = .FALSE.
         READ(10,*,IOSTAT=IOS) DELETE
         IF(IOS /= 0) THEN
            CPL_DEFERRED_FUEL_NOT_ACTIVE = .TRUE.
            RETURN
         ENDIF
!
         READ(10,'(A)',IOSTAT=IOS) RECLN
         IF(IOS == 0) THEN
            RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,' &
                                //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,' &
                                //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,' &
                                //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,' &
                                //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,' &
                                //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,' &
                                //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,' &
                                //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,' &
                                //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,' &
                                //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,' &
                                //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,' &
                                //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,' &
                                //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,' &
                                //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,' &
                                //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
            READ(RECLN,*) DELETE, &
                CPL_DEFERRED_FUEL_RPT_CLASS_ID, &
                SC_DEFERRED_FUEL_RPT_CLASS_ID,SC_CUMULATIVE_JURIS_SALES, &
        SC_CALC_BASE_FUEL_FACTOR,SC_DEFERRED_FUEL_BALANCE, &
            NC_DEFERRED_FUEL_RPT_CLASS_ID,NC_CUMULATIVE_FUEL_COST, &
                          NC_CUMULATIVE_ENERGY_SALES, &
                          NC_CUMULATIVE_JURIS_SALES,NC_CALC_BASE_FUEL_FACTOR, &
                          NC_CALC_EMF_FACTOR, &
                          NC_DEFERRED_FUEL_BALANCE,NC_EMF_FUEL_BALANCE, &
                          SC_FUEL_REV_CASH_JAN,NC_FUEL_REV_CASH_JAN, &
                          DUMMY_CHR, & ! 16
                          SC_CALCULATION_START_YR, &
                          NC_CALCULATION_START_YR, &
                          SC_CUMULATIVE_FUEL_COST, &
                          SC_CUMULATIVE_ENERGY_SALES,DUMMY_CHR,  &  ! 21
                          (SC_PURCHASE_POWER_ALLOCATION(YR),YR=1,10), &
                          (NC_PURCHASE_POWER_ALLOCATION(YR),YR=1,10), &
                          USE_BUDGET_FORECAST_IN, &
                          DUMMY_CHR, & ! 49
                          CPL_OFF_SYSTEM_FUEL_RATES, &
                          SC_FUEL_FACTOR_INPUT, &
                          NC_FUEL_FACTOR_INPUT, &
                          NC_EMF_FACTOR_INPUT, &
                          OFF_SYSTEM_RATES_ACTIVE
!
         ELSE
            SC_CUMULATIVE_FUEL_COST = 0.
            SC_CUMULATIVE_ENERGY_SALES = 0.
            SC_CUMULATIVE_JURIS_SALES = 0.
            SC_CUMULATIVE_JURIS_SALES = 25195.161-641.207
            SC_BASE_FUEL_FACTOR = 11.22
            SC_DEFERRED_FUEL_BALANCE = 2.201
!
            NC_CUMULATIVE_FUEL_COST = 472.046-55.404
            NC_CUMULATIVE_ENERGY_SALES = 38788.269-4616.338
            NC_CUMULATIVE_JURIS_SALES = 27616.699-3062.745
            NC_BASE_FUEL_FACTOR = 10.79
            NC_EMF_FACTOR = .36
            NC_DEFERRED_FUEL_BALANCE = 24.144
            NC_EMF_FUEL_BALANCE = -15.486
            CPL_DEFERRED_FUEL_RPT_CLASS_ID = 1 ! 8
            NC_DEFERRED_FUEL_RPT_CLASS_ID = 14
            SC_DEFERRED_FUEL_RPT_CLASS_ID = 18
            SC_FUEL_REV_CASH_JAN = 0.
            NC_FUEL_REV_CASH_JAN = 0.
            SC_CALCULATION_START_YR = 1990
            NC_CALCULATION_START_YR = 1990
         ENDIF
         CLOSE(10)
         SC_CALCULATION_START_YR = 1990
         NC_CALCULATION_START_YR = 1990
         BU_FUEL_REV_CASH_JAN = SC_FUEL_REV_CASH_JAN &
            + NC_FUEL_REV_CASH_JAN
         IF(.NOT. REPORT_HEADER_OPEN) THEN
            REPORTING_UNIT = CPL_DEFERRED_FUEL_RPT_HEADER()
            REPORT_HEADER_OPEN = .TRUE.
         ENDIF
        CLASS_POS=R_MASTER_CLASS_LIST(CPL_DEFERRED_FUEL_RPT_CLASS_ID+1)
         DEF_CLASS_TYPE = R_CLASS_TYPE(CLASS_POS)

      RETURN

      ENTRY CPL_DEFERRED_FUEL_ACCOUNTING(R_YEAR)


         IF(WVPA()) RETURN
         IF(CPL_DEFERRED_FUEL_NOT_ACTIVE) RETURN
!
! SC FUEL CLAUSE IS FORWARD THE APPROXIMATION IS CUURENT YEAR
!
         CALL READ_CLASS_RUN_SWITCHES(CPL_DEFERRED_FUEL_RPT_CLASS_ID+1, &
                                      R_YEAR,DEF_CLASS_TYPE)
         CPL_JURISDICTIONAL_INFOR(0,sc_fuel_clause, &
                                          cpl_cumulative_fuel_cost) = &
                                                SC_CUMULATIVE_FUEL_COST
         CPL_JURISDICTIONAL_INFOR(0,sc_fuel_clause, &
                                                cpl_cum_enrg_sales) = &
                                             SC_CUMULATIVE_ENERGY_SALES
         CPL_JURISDICTIONAL_INFOR(0,sc_fuel_clause, &
                                               cpl_cum_juris_sales) = &
                                               SC_CUMULATIVE_JURIS_SALES
!
         SC_PUR_POWER_ALLOCATION = &
            SC_PURCHASE_POWER_ALLOCATION(MIN(R_YEAR,INT(10,2)))/100.
         NC_PUR_POWER_ALLOCATION = &
            NC_PURCHASE_POWER_ALLOCATION(MIN(R_YEAR,INT(10,2)))/100.
         CURRENT_YEAR = get_BASE_YEAR() + R_YEAR
         TOTAL_OFF_SYSTEM_ENRG = 0.
         TOTAL_OFF_SYSTEM_COST = 0.
         DO MO = 1, 12
            CALL CPL_SC_RETAIL(MO,CPL_GEN_COST(MO), &
                               CPL_NUC_COST(MO), &
                               CPL_PA_ENTITLE(MO), &
                               CPL_PUR_POWER(MO), &
                               CPL_PA_MWH_COST(MO), &
                               CPL_NCEMPA_MWH(MO), &
                               CPL_OFF_SYS_ENRG(MO), &
                               CPL_OFF_SYS_ENRG_RATE(MO))
            IF(OFF_SYSTEM_RATES_ACTIVE(MIN(R_YEAR,INT(7,2))) == 'Y')THEN
               OFF_SYSTEM_ENRG_RATE = CPL_OFF_SYSTEM_FUEL_RATES(MO,MIN(R_YEAR,INT(7,2)))
            ELSE
               OFF_SYSTEM_ENRG_RATE = CPL_OFF_SYS_ENRG_RATE(MO)
            ENDIF
            CPL_OFF_SYS_INC_COST(MO) = OFF_SYSTEM_ENRG_RATE * &
                CPL_OFF_SYS_ENRG(MO)
            TOTAL_OFF_SYSTEM_ENRG = TOTAL_OFF_SYSTEM_ENRG + &
                CPL_OFF_SYS_ENRG(MO)
            TOTAL_OFF_SYSTEM_COST = TOTAL_OFF_SYSTEM_COST + &
                CPL_OFF_SYS_INC_COST(MO)
         ENDDO
!
! SUBSTITUTE THE FUEL AND PURCHASE POWER INFORMATION FROM THE EXPENSE FILE
!
         IF(USE_BUDGET_FORECAST_IN(MIN(INT(7,2),R_YEAR)) == 'Y') THEN


            IF(MONTHLY_MIDAS_ACTIVE .AND. &
                R_YEAR <= LAST_AVAILABLE_MONTHLY_YEAR) THEN
               CALL BUDGET_FUEL_PURCHASES(R_YEAR, &
                                          BUDGET_GEN_COST, &
                                          BUDGET_PUR_POWER)
               DO MO = 0, 12
                  CPL_GEN_COST(MO) = 1000000. * BUDGET_GEN_COST(MO)
                  CPL_PUR_POWER(MO) = 1000000. * BUDGET_PUR_POWER(MO)
               ENDDO
            ELSE
               CALL BUDGET_FUEL_PURCHASES(R_YEAR, &
                                          BUDGET_GEN_COST(0), &
                                          BUDGET_PUR_POWER(0))
               ANNUAL_CPL_GEN_COST = 1000000. * BUDGET_GEN_COST(0)
               ANNUAL_CPL_PUR_POWER = 1000000. * BUDGET_PUR_POWER(0)
               IF(CPL_GEN_COST(0) /= 0.) THEN
                  ENRG_RATIO = ANNUAL_CPL_GEN_COST/CPL_GEN_COST(0)
               ELSE
                  ENRG_RATIO = 1./12.
               ENDIF
               IF(CPL_PUR_POWER(0) /= 0.) THEN
                  P_POWER_RATIO = ANNUAL_CPL_PUR_POWER/CPL_PUR_POWER(0)
               ELSE
                  P_POWER_RATIO = 1./12.
               ENDIF
               CPL_GEN_COST(0) = ANNUAL_CPL_GEN_COST
               CPL_PUR_POWER(0) = ANNUAL_CPL_PUR_POWER
               DO MO = 1, 12
                  CPL_GEN_COST(MO) = ENRG_RATIO * CPL_GEN_COST(MO)
                  CPL_PUR_POWER(MO) = P_POWER_RATIO * CPL_PUR_POWER(MO)
               ENDDO
            ENDIF
         ENDIF

         IF(TOTAL_OFF_SYSTEM_ENRG /= 0.) THEN
            CPL_OFF_SYS_ENRG_RATE(0) = TOTAL_OFF_SYSTEM_COST/ &
                TOTAL_OFF_SYSTEM_ENRG
         ELSE
            CPL_OFF_SYS_ENRG_RATE(0) = 0.
         ENDIF
         DO MO = 0, 12
            CALL RETURN_CLASS_MONTHLY_SALES(MO,OTHER2_FORECAST, &
                CPL_NCEMC_MWH(MO))
!
! SC FUEL CALCULATIONS
!
            CPL_JURISDICTIONAL_INFOR(MO,sc_fuel_clause,cpl_mwh) = &
                   CPL_JURISDICTIONAL_INFOR(MO,sc_retail,cpl_mwh) &
                 + CPL_JURISDICTIONAL_INFOR(MO,nc_retail,cpl_mwh) &
                 + CPL_JURISDICTIONAL_INFOR(MO,Wholesale,cpl_mwh) &
                 + CPL_NCEMC_MWH(MO)/1000.
            CPL_JURISDICTIONAL_INFOR(MO,sc_fuel_clause,cpl_fuel_cost) = &
                                         (CPL_GEN_COST(MO) &
                                        + CPL_NUC_COST(MO) &
                                     + CPL_PUR_POWER(MO) * &
                                   SC_PUR_POWER_ALLOCATION &
                                - CPL_OFF_SYS_INC_COST(MO) &
                              - CPL_PA_MWH_COST(MO))/1000000.
            IF(MO > 0) THEN
               SC_CUMULATIVE_FUEL_COST = SC_CUMULATIVE_FUEL_COST &
                        + CPL_JURISDICTIONAL_INFOR(MO,sc_fuel_clause, &
                                                         cpl_fuel_cost)
               SC_CUMULATIVE_ENERGY_SALES = SC_CUMULATIVE_ENERGY_SALES + &
                  CPL_JURISDICTIONAL_INFOR(MO,sc_fuel_clause,cpl_mwh)
               SC_CUMULATIVE_JURIS_SALES = SC_CUMULATIVE_JURIS_SALES + &
                  CPL_JURISDICTIONAL_INFOR(MO,sc_retail,cpl_mwh)/1000.
            ENDIF
             CPL_JURISDICTIONAL_INFOR(MO,sc_fuel_clause, &
                                         cpl_avg_fuel_cost) = 1000.* &
             CPL_JURISDICTIONAL_INFOR(MO,sc_fuel_clause,cpl_fuel_cost)/ &
             CPL_JURISDICTIONAL_INFOR(MO,sc_fuel_clause,cpl_mwh)
            CPL_JURISDICTIONAL_INFOR(MO,sc_fuel_clause, &
                                        cpl_jurisdictional_fuel_cost) = &
                            CPL_JURISDICTIONAL_INFOR(MO,sc_fuel_clause, &
                                                   cpl_avg_fuel_cost) * &
               CPL_JURISDICTIONAL_INFOR(MO,sc_retail,cpl_mwh)/1000.
!
! NC FUEL CALCULATIONS
!
            CPL_JURISDICTIONAL_INFOR(MO,nc_fuel_clause,cpl_mwh) = &
                     CPL_JURISDICTIONAL_INFOR(MO,sc_retail,cpl_mwh) + &
                     CPL_JURISDICTIONAL_INFOR(MO,nc_retail,cpl_mwh) + &
                     CPL_JURISDICTIONAL_INFOR(MO,Wholesale,cpl_mwh) + &
                      (CPL_NCEMPA_MWH(MO)+CPL_NCEMC_MWH(MO))/1000.
            CPL_JURISDICTIONAL_INFOR(MO,nc_fuel_clause,cpl_fuel_cost) = &
                                                   (CPL_GEN_COST(MO) &
                                                  + CPL_NUC_COST(MO) &
                                               + CPL_PUR_POWER(MO) * &
                                             NC_PUR_POWER_ALLOCATION &
                                  - CPL_OFF_SYS_INC_COST(MO))/1000000.
            CPL_JURISDICTIONAL_INFOR(MO,nc_fuel_clause, &
                                         cpl_avg_fuel_cost) = 1000.* &
          CPL_JURISDICTIONAL_INFOR(MO,nc_fuel_clause,cpl_fuel_cost)/ &
                 CPL_JURISDICTIONAL_INFOR(MO,nc_fuel_clause,cpl_mwh)

            CPL_JURISDICTIONAL_INFOR(MO,nc_fuel_clause, &
                                      cpl_jurisdictional_fuel_cost) = &
                          CPL_JURISDICTIONAL_INFOR(MO,nc_fuel_clause, &
                                                 cpl_avg_fuel_cost) * &
                  CPL_JURISDICTIONAL_INFOR(MO,nc_retail,cpl_mwh)/1000.
         ENDDO
!
! NC DEFERRED ACCOUNTING
!
         DO MO = 1, 12
            IF(CURRENT_YEAR >= NC_CALCULATION_START_YR .AND. &
                MO == NC_FUEL_FACTOR_ADJ_MONTH) THEN
               NC_CALC_BASE_FUEL_FACTOR = NC_NEW_BASE_FUEL_FACTOR
               NC_CALC_EMF_FACTOR = NC_NEW_EMF_FACTOR
            ENDIF
            NC_BASE_FUEL_FACTOR = NC_CALC_BASE_FUEL_FACTOR
            NC_EMF_FACTOR = NC_CALC_EMF_FACTOR
            IF(R_YEAR <= 7) THEN
               IF(NC_FUEL_FACTOR_INPUT(MO,R_YEAR) /= 0.) THEN
                  NC_BASE_FUEL_FACTOR = NC_FUEL_FACTOR_INPUT(MO,R_YEAR)
               ENDIF
               IF(NC_EMF_FACTOR_INPUT(MO,R_YEAR) /= 0.) THEN
                  NC_EMF_FACTOR = NC_EMF_FACTOR_INPUT(MO,R_YEAR)
               ENDIF
            ENDIF
            CPL_JURISDICTIONAL_INFOR(MO,nc_fuel_clause, &
                cpl_base_fuel_factor) = NC_BASE_FUEL_FACTOR
                       CPL_JURISDICTIONAL_INFOR(MO,nc_fuel_clause, &
                    cpl_jurisdictional_rev) = NC_BASE_FUEL_FACTOR* &
                CPL_JURISDICTIONAL_INFOR(MO,nc_retail,cpl_mwh)/1000.
            CPL_JURISDICTIONAL_INFOR(MO,nc_fuel_clause,cpl_emf_factor) = NC_EMF_FACTOR
            CPL_JURISDICTIONAL_INFOR(MO,nc_fuel_clause, &
                                    cpl_emf_revenues) = NC_EMF_FACTOR* &
                 CPL_JURISDICTIONAL_INFOR(MO,nc_retail,cpl_mwh)/1000.
            CPL_JURISDICTIONAL_INFOR(MO,nc_fuel_clause, &
                                       cpl_jurisdictional_fuel_cost) = &
                CPL_JURISDICTIONAL_INFOR(MO,nc_retail,cpl_mwh)* &
                CPL_JURISDICTIONAL_INFOR(MO,nc_fuel_clause, &
                                           cpl_avg_fuel_cost)/1000.
!
! CURRENT MONTH RECOVERY JURS REVENUES - FUEL EXPENSE
!
            CPL_JURISDICTIONAL_INFOR(MO,nc_fuel_clause, &
                                           cpl_deferral_recovery) = &
                  CPL_JURISDICTIONAL_INFOR(MO,nc_fuel_clause, &
                                          cpl_jurisdictional_rev) - &
                  CPL_JURISDICTIONAL_INFOR(MO,nc_fuel_clause, &
                                          cpl_jurisdictional_fuel_cost)
!
! EMF NEXT CASE ACCOUNTING
!
            CPL_JURISDICTIONAL_INFOR(MO,nc_fuel_clause, &
                                           cpl_emf_startbal) = &
                                                    NC_EMF_FUEL_BALANCE
            NC_EMF_FUEL_BALANCE = NC_EMF_FUEL_BALANCE + &
                  CPL_JURISDICTIONAL_INFOR(MO,nc_fuel_clause, &
                                                 cpl_deferral_recovery)
            CPL_JURISDICTIONAL_INFOR(MO,nc_fuel_clause, &
                                              cpl_emf_endbal) = &
                                                   NC_EMF_FUEL_BALANCE
!
! DEFERRED FUEL SUMMARY
!
            CPL_JURISDICTIONAL_INFOR(MO,nc_fuel_clause, &
                                               cpl_startbal) = &
                                               NC_DEFERRED_FUEL_BALANCE
            NC_DEFERRED_FUEL_BALANCE = NC_DEFERRED_FUEL_BALANCE + &
                      CPL_JURISDICTIONAL_INFOR(MO,nc_fuel_clause, &
                                              cpl_deferral_recovery) + &
                  CPL_JURISDICTIONAL_INFOR(MO,nc_fuel_clause, &
                                                       cpl_emf_revenues)
            CPL_JURISDICTIONAL_INFOR(MO,nc_fuel_clause, &
                                                cpl_ending_bal) = &
                                               NC_DEFERRED_FUEL_BALANCE
!
! CUMULATIVE FUEL, SALES AND JURISDICTION SALES
!
            NC_CUMULATIVE_FUEL_COST = NC_CUMULATIVE_FUEL_COST + &
               CPL_JURISDICTIONAL_INFOR(MO,nc_fuel_clause,cpl_fuel_cost)
            CPL_JURISDICTIONAL_INFOR(MO,nc_fuel_clause, &
                                            cpl_cumulative_fuel_cost) = &
                                                NC_CUMULATIVE_FUEL_COST
!
            NC_CUMULATIVE_ENERGY_SALES = NC_CUMULATIVE_ENERGY_SALES + &
                  CPL_JURISDICTIONAL_INFOR(MO,nc_fuel_clause,cpl_mwh)
            CPL_JURISDICTIONAL_INFOR(MO,nc_fuel_clause, &
                                         cpl_cum_enrg_sales) = &
                                             NC_CUMULATIVE_ENERGY_SALES
!
           NC_CUMULATIVE_JURIS_SALES = NC_CUMULATIVE_JURIS_SALES + &
                       CPL_JURISDICTIONAL_INFOR(MO,nc_retail,cpl_mwh)
            CPL_JURISDICTIONAL_INFOR(MO,nc_fuel_clause, &
                                  cpl_cum_juris_sales) = &
                                              NC_CUMULATIVE_JURIS_SALES
!
            IF(CURRENT_YEAR >= NC_CALCULATION_START_YR &
                .AND. MO == NC_FUEL_FACTOR_CAL_MONTH) THEN
              NC_NEW_BASE_FUEL_FACTOR = 1000.*NC_CUMULATIVE_FUEL_COST/ &
                NC_CUMULATIVE_ENERGY_SALES
               NC_NEW_EMF_FACTOR = -1000.*NC_EMF_FUEL_BALANCE/ &
                NC_CUMULATIVE_JURIS_SALES
               NC_CUMULATIVE_FUEL_COST = 0.
               NC_CUMULATIVE_ENERGY_SALES = 0.
               NC_CUMULATIVE_JURIS_SALES = 0.
!              NC_EMF_FUEL_BALANCE = 0.
            ENDIF
         ENDDO

!
! SC DEFERRED ACCOUNTING




         DO MO = 1, 12
            IF(CURRENT_YEAR >= SC_CALCULATION_START_YR .AND. &
                MO == SC_FUEL_FACTOR_ADJ_MONTH) THEN
              SC_CALC_BASE_FUEL_FACTOR = 1000.*SC_CUMULATIVE_FUEL_COST/ &
                                           SC_CUMULATIVE_ENERGY_SALES - &
                     SC_DEFERRED_FUEL_BALANCE/SC_CUMULATIVE_JURIS_SALES
               SC_CUMULATIVE_FUEL_COST = 0.
               SC_CUMULATIVE_ENERGY_SALES = 0.
               SC_CUMULATIVE_JURIS_SALES = 0.
            ENDIF
            SC_BASE_FUEL_FACTOR = SC_CALC_BASE_FUEL_FACTOR
            IF(R_YEAR <= 7) THEN
               IF(SC_FUEL_FACTOR_INPUT(MO,R_YEAR) /= 0.) THEN
                  SC_BASE_FUEL_FACTOR = SC_FUEL_FACTOR_INPUT(MO,R_YEAR)
               ENDIF
            ENDIF
            CPL_JURISDICTIONAL_INFOR(MO,sc_fuel_clause, &
                cpl_base_fuel_factor) = SC_BASE_FUEL_FACTOR
            CPL_JURISDICTIONAL_INFOR(MO,sc_fuel_clause, &
                     cpl_jurisdictional_rev) = SC_BASE_FUEL_FACTOR* &
                 CPL_JURISDICTIONAL_INFOR(MO,sc_retail,cpl_mwh)/1000.
            CPL_JURISDICTIONAL_INFOR(MO,sc_fuel_clause, &
                                        cpl_jurisdictional_fuel_cost) = &
                 CPL_JURISDICTIONAL_INFOR(MO,sc_retail,cpl_mwh)* &
                 CPL_JURISDICTIONAL_INFOR(MO,sc_fuel_clause, &
                                           cpl_avg_fuel_cost)/1000.
            CPL_JURISDICTIONAL_INFOR(MO,sc_fuel_clause, &
                                               cpl_startbal) = &
                                               SC_DEFERRED_FUEL_BALANCE
            CPL_JURISDICTIONAL_INFOR(MO,sc_fuel_clause, &
                                               cpl_deferral_recovery) = &
                  CPL_JURISDICTIONAL_INFOR(MO,sc_fuel_clause, &
                                          cpl_jurisdictional_rev) - &
                  CPL_JURISDICTIONAL_INFOR(MO,sc_fuel_clause, &
                                          cpl_jurisdictional_fuel_cost)
            SC_DEFERRED_FUEL_BALANCE = SC_DEFERRED_FUEL_BALANCE + &
                  CPL_JURISDICTIONAL_INFOR(MO,sc_fuel_clause, &
                                                 cpl_deferral_recovery)
            CPL_JURISDICTIONAL_INFOR(MO,sc_fuel_clause, &
                                                cpl_ending_bal) = &
                                               SC_DEFERRED_FUEL_BALANCE
            CPL_JURISDICTIONAL_INFOR(MO,sc_fuel_clause, &
                                            cpl_cumulative_fuel_cost) = &
              CPL_JURISDICTIONAL_INFOR(MO-1,sc_fuel_clause, &
                                            cpl_cumulative_fuel_cost) + &
              CPL_JURISDICTIONAL_INFOR(MO,sc_fuel_clause,cpl_fuel_cost)
            CPL_JURISDICTIONAL_INFOR(MO,sc_fuel_clause, &
                                         cpl_cum_enrg_sales) = &
                   CPL_JURISDICTIONAL_INFOR(MO-1,sc_fuel_clause, &
                                         cpl_cum_enrg_sales) + &
                  CPL_JURISDICTIONAL_INFOR(MO,sc_fuel_clause,cpl_mwh)
            CPL_JURISDICTIONAL_INFOR(MO,sc_fuel_clause, &
                                  cpl_cum_juris_sales) = &
                        CPL_JURISDICTIONAL_INFOR(MO-1,sc_fuel_clause, &
                                  cpl_cum_juris_sales) + &
                       CPL_JURISDICTIONAL_INFOR(MO,sc_retail,cpl_mwh)
         ENDDO
!
! WRITE REPORT
!
         CPL_JURISDICTIONAL_INFOR(0,nc_fuel_clause, &
            cpl_jurisdictional_rev) = 0.
         CPL_JURISDICTIONAL_INFOR(0,nc_fuel_clause, &
            cpl_emf_revenues)=0.
         CPL_JURISDICTIONAL_INFOR(0,nc_fuel_clause, &
            cpl_deferral_recovery) = 0.
         CPL_JURISDICTIONAL_INFOR(0,sc_fuel_clause, &
            cpl_jurisdictional_rev) = 0.
         CPL_JURISDICTIONAL_INFOR(0,sc_fuel_clause, &
            cpl_deferral_recovery) = 0.
!
         DO MO = 1, 12
!
            CPL_JURISDICTIONAL_INFOR(MO, sc_fuel_clause, &
                cpl_jurisdiction_sales) = &
                    CPL_JURISDICTIONAL_INFOR(MO,sc_retail,cpl_mwh)
            CPL_JURISDICTIONAL_INFOR(MO,nc_fuel_clause, &
                cpl_jurisdiction_sales) = &
                    CPL_JURISDICTIONAL_INFOR( &
                    MO,nc_retail,cpl_mwh)
!
            WRITE(REPORTING_UNIT) &
                PRT_ENDPOINT(), &
                    FLOAT(get_BASE_YEAR()+globecom_YEAR), &
                SHORT_MONTH_NAMES(MO),ZERO_POS, &
                      (CPL_JURISDICTIONAL_INFOR(MO,sc_fuel_clause,I) &
                        ,I=1,CPL_FUEL_VARS), &
                      (CPL_JURISDICTIONAL_INFOR(MO,nc_fuel_clause,I) &
                        ,I=1,CPL_FUEL_VARS), &
                       CPL_GEN_COST(MO)/1000000., &
                       CPL_PA_ENTITLE(MO)/1000000., &
                       CPL_PUR_POWER(MO)/1000000., &
                       CPL_OFF_SYS_INC_COST(MO)/1000000., &
                       CPL_PA_MWH_COST(MO)/1000000., &
                       CPL_NCEMPA_MWH(MO)/1000., &
                       CPL_NCEMC_MWH(MO)/1000., &
                       CPL_JURISDICTIONAL_INFOR(MO,Wholesale,cpl_mwh), &
                       CPL_PUR_POWER(MO)/1000000. * &
                        SC_PUR_POWER_ALLOCATION, &
                       CPL_PUR_POWER(MO)/1000000. &
                         * NC_PUR_POWER_ALLOCATION, &
                       CPL_OFF_SYS_ENRG(MO)/1000., &
                       CPL_OFF_SYS_ENRG_RATE(MO), &
                       CPL_OFF_SYSTEM_FUEL_RATES(MO,MIN(R_YEAR,INT(7,2))), &
                       CPL_NUC_COST(MO)/1000000., &
                      (CPL_GEN_COST(MO) + CPL_NUC_COST(MO))/1000000.
!
            CPL_JURISDICTIONAL_INFOR(0,nc_fuel_clause, &
                                           cpl_jurisdictional_rev) = &
                    CPL_JURISDICTIONAL_INFOR(0,nc_fuel_clause, &
                                             cpl_jurisdictional_rev) &
                    + CPL_JURISDICTIONAL_INFOR(MO,nc_fuel_clause, &
                                             cpl_jurisdictional_rev)
!
            CPL_JURISDICTIONAL_INFOR(0,nc_fuel_clause,cpl_emf_revenues)= &
                  CPL_JURISDICTIONAL_INFOR(0,nc_fuel_clause, &
                                                       cpl_emf_revenues) &
                  + CPL_JURISDICTIONAL_INFOR(MO,nc_fuel_clause, &
                                                       cpl_emf_revenues)
!
            CPL_JURISDICTIONAL_INFOR(0,sc_fuel_clause, &
                                           cpl_jurisdictional_rev) = &
                  CPL_JURISDICTIONAL_INFOR(0,sc_fuel_clause, &
                                             cpl_jurisdictional_rev) &
                  + CPL_JURISDICTIONAL_INFOR(MO,sc_fuel_clause, &
                                             cpl_jurisdictional_rev)
!
            CPL_JURISDICTIONAL_INFOR(0,nc_fuel_clause, &
                                                cpl_deferral_recovery) = &
                  CPL_JURISDICTIONAL_INFOR(0,nc_fuel_clause, &
                                                  cpl_deferral_recovery) &
                  + CPL_JURISDICTIONAL_INFOR(MO,nc_fuel_clause, &
                                                  cpl_deferral_recovery)
!
            CPL_JURISDICTIONAL_INFOR(0,sc_fuel_clause, &
                                                cpl_deferral_recovery) = &
                  CPL_JURISDICTIONAL_INFOR(0,sc_fuel_clause, &
                                                  cpl_deferral_recovery) &
                  + CPL_JURISDICTIONAL_INFOR(MO,sc_fuel_clause, &
                                                  cpl_deferral_recovery)
!
         ENDDO
      RETURN

      ENTRY CPL_ACCOUNTING_4_DEFERRED_FUEL(R_CLASS, &
                                           R_CPL_FUEL_REVENUES, &
                                           R_DEFERRED_FUEL_EXPENSE)

!
         IF(WVPA()) RETURN
         R_CPL_FUEL_REVENUES = 0.
         R_DEFERRED_FUEL_EXPENSE = 0.
         IF(CPL_DEFERRED_FUEL_NOT_ACTIVE) RETURN
!
         IF(R_CLASS-1 == CPL_DEFERRED_FUEL_RPT_CLASS_ID) THEN ! = 1
            R_CPL_FUEL_REVENUES = &
                  CPL_JURISDICTIONAL_INFOR(0,nc_fuel_clause, &
                                             cpl_jurisdictional_rev) &
                  + CPL_JURISDICTIONAL_INFOR(0,nc_fuel_clause, &
                                                       cpl_emf_revenues) &
                  + CPL_JURISDICTIONAL_INFOR(0,sc_fuel_clause, &
                                             cpl_jurisdictional_rev)
            R_DEFERRED_FUEL_EXPENSE = &
                    CPL_JURISDICTIONAL_INFOR(0,nc_fuel_clause, &
                                                  cpl_deferral_recovery) &
                  + CPL_JURISDICTIONAL_INFOR(0,nc_fuel_clause, &
                                                       cpl_emf_revenues) &
                  + CPL_JURISDICTIONAL_INFOR(0,sc_fuel_clause, &
                                                  cpl_deferral_recovery)
         ELSEIF(R_CLASS-1 == NC_DEFERRED_FUEL_RPT_CLASS_ID) THEN ! = 14
            R_CPL_FUEL_REVENUES = &
                  CPL_JURISDICTIONAL_INFOR(0,nc_fuel_clause, &
                                             cpl_jurisdictional_rev) &
                  + CPL_JURISDICTIONAL_INFOR(0,nc_fuel_clause, &
                                                       cpl_emf_revenues)
            R_DEFERRED_FUEL_EXPENSE = &
                  CPL_JURISDICTIONAL_INFOR(0,nc_fuel_clause, &
                                                  cpl_deferral_recovery) &
                  + CPL_JURISDICTIONAL_INFOR(0,nc_fuel_clause, &
                                                       cpl_emf_revenues)
         ELSEIF(R_CLASS-1 == SC_DEFERRED_FUEL_RPT_CLASS_ID) THEN ! = 18
            R_CPL_FUEL_REVENUES = &
                    CPL_JURISDICTIONAL_INFOR(0,sc_fuel_clause, &
                                             cpl_jurisdictional_rev)
            R_DEFERRED_FUEL_EXPENSE = &
                    CPL_JURISDICTIONAL_INFOR(0,sc_fuel_clause, &
                                                  cpl_deferral_recovery)
         ENDIF
      RETURN

      ENTRY CPL_MONTHLY_DEFERRED_FUEL(R_CLASS, &
                                      R_CPL_MONTHLY_FUEL_REVENUES, &
                                      R_MONTHLY_DEFERRED_FUEL_EXPENSE, &
                                      R_MONTHLY_CASH_FUEL_REVENUES, &
                                      R_FUEL_REVENUE_RECEIVABLE)

!
         IF(WVPA()) RETURN
         IF(R_CLASS-1 == CPL_DEFERRED_FUEL_RPT_CLASS_ID) THEN ! = 1
            R_MONTHLY_CASH_FUEL_REVENUES(1) = BU_FUEL_REV_CASH_JAN
            R_MONTHLY_CASH_FUEL_REVENUES(0) = BU_FUEL_REV_CASH_JAN
            DO MO = 0, 12
               R_CPL_MONTHLY_FUEL_REVENUES(MO) = &
                  CPL_JURISDICTIONAL_INFOR(MO,nc_fuel_clause, &
                                             cpl_jurisdictional_rev) &
                  + CPL_JURISDICTIONAL_INFOR(MO,nc_fuel_clause, &
                                                       cpl_emf_revenues) &
                  + CPL_JURISDICTIONAL_INFOR(MO,sc_fuel_clause, &
                                             cpl_jurisdictional_rev)
               R_MONTHLY_DEFERRED_FUEL_EXPENSE(MO) = &
                  CPL_JURISDICTIONAL_INFOR(MO,nc_fuel_clause, &
                                                  cpl_deferral_recovery) &
                  + CPL_JURISDICTIONAL_INFOR(MO,nc_fuel_clause, &
                                                       cpl_emf_revenues) &
                  + CPL_JURISDICTIONAL_INFOR(MO,sc_fuel_clause, &
                                                  cpl_deferral_recovery)
               IF(MO > 1) THEN
                  R_MONTHLY_CASH_FUEL_REVENUES(MO) = &
                    R_CPL_MONTHLY_FUEL_REVENUES(MO-1)
                  R_MONTHLY_CASH_FUEL_REVENUES(0) = &
                    R_MONTHLY_CASH_FUEL_REVENUES(0) + &
                        R_MONTHLY_CASH_FUEL_REVENUES(MO)
               ENDIF
            ENDDO
            BU_FUEL_REV_CASH_JAN = R_CPL_MONTHLY_FUEL_REVENUES(12)
            R_FUEL_REVENUE_RECEIVABLE = R_CPL_MONTHLY_FUEL_REVENUES(0) - &
                R_MONTHLY_CASH_FUEL_REVENUES(0)
          ELSEIF(R_CLASS-1 == NC_DEFERRED_FUEL_RPT_CLASS_ID) THEN ! = 14
            R_MONTHLY_CASH_FUEL_REVENUES(1) = NC_FUEL_REV_CASH_JAN
            R_MONTHLY_CASH_FUEL_REVENUES(0) = NC_FUEL_REV_CASH_JAN
            DO MO = 0, 12
               R_CPL_MONTHLY_FUEL_REVENUES(MO) = &
                     CPL_JURISDICTIONAL_INFOR(MO,nc_fuel_clause, &
                                             cpl_jurisdictional_rev) &
                     + CPL_JURISDICTIONAL_INFOR(MO,nc_fuel_clause, &
                                                       cpl_emf_revenues)
               R_MONTHLY_DEFERRED_FUEL_EXPENSE(MO) = &
                     CPL_JURISDICTIONAL_INFOR(MO,nc_fuel_clause, &
                                                  cpl_deferral_recovery) &
                     + CPL_JURISDICTIONAL_INFOR(MO,nc_fuel_clause, &
                                                       cpl_emf_revenues)
               IF(MO > 1) THEN
                  R_MONTHLY_CASH_FUEL_REVENUES(MO) = &
                    R_CPL_MONTHLY_FUEL_REVENUES(MO-1)
                  R_MONTHLY_CASH_FUEL_REVENUES(0) = &
                                      R_MONTHLY_CASH_FUEL_REVENUES(0) &
                                      + R_MONTHLY_CASH_FUEL_REVENUES(MO)
               ENDIF
            ENDDO
            NC_FUEL_REV_CASH_JAN = R_CPL_MONTHLY_FUEL_REVENUES(12)
         ELSEIF(R_CLASS-1 == SC_DEFERRED_FUEL_RPT_CLASS_ID) THEN ! = 18
            R_MONTHLY_CASH_FUEL_REVENUES(1) = SC_FUEL_REV_CASH_JAN
            R_MONTHLY_CASH_FUEL_REVENUES(0) = SC_FUEL_REV_CASH_JAN
            DO MO = 0, 12
               R_CPL_MONTHLY_FUEL_REVENUES(MO) = &
                    CPL_JURISDICTIONAL_INFOR(MO,sc_fuel_clause, &
                                             cpl_jurisdictional_rev)
               R_MONTHLY_DEFERRED_FUEL_EXPENSE(MO) = &
                    CPL_JURISDICTIONAL_INFOR(MO,sc_fuel_clause, &
                                                  cpl_deferral_recovery)
               IF(MO > 1) THEN
                  R_MONTHLY_CASH_FUEL_REVENUES(MO) = &
                                       R_CPL_MONTHLY_FUEL_REVENUES(MO-1)
                  R_MONTHLY_CASH_FUEL_REVENUES(0) = &
                                      R_MONTHLY_CASH_FUEL_REVENUES(0) &
                                      + R_MONTHLY_CASH_FUEL_REVENUES(MO)
               ENDIF
            ENDDO
            SC_FUEL_REV_CASH_JAN = R_CPL_MONTHLY_FUEL_REVENUES(12)
         ELSE
            DO MO = 0, 12
               R_CPL_MONTHLY_FUEL_REVENUES(MO) = 0.
               R_MONTHLY_DEFERRED_FUEL_EXPENSE(MO) = 0.
               R_MONTHLY_CASH_FUEL_REVENUES(MO) = 0.
            ENDDO
         ENDIF
         R_FUEL_REVENUE_RECEIVABLE = R_CPL_MONTHLY_FUEL_REVENUES(0) &
            - R_MONTHLY_CASH_FUEL_REVENUES(0)
      RETURN

      ENTRY RETURN_FE_COMPETITIVE_SALE_INFO(R_CLASS, &
                                            R_COMPETITIVE_SALES, &
                                            R_COMPETITIVE_SALES_QUANT, &
                                            R_COMPETITIVE_LOSS)

         IF(WVPA()) RETURN
         IF(R_CLASS <= MAX_ASSET_CLASS_NUM .AND. MAX_ASSET_CLASS_NUM > 0) THEN
            IF(R_CLASS <=0) THEN
               ASSET_CLASS_loc = R_CLASS
            ELSE
               ASSET_CLASS_loc = ASSET_CLASS_POINTER_loc(R_CLASS)
            ENDIF
            IF(ASSET_CLASS_loc > 0 .OR. R_CLASS <= 0) THEN
               PERIOD = 0
               DO REV_TYPE = 22, 30 ! FE Competitive sales
                  R_COMPETITIVE_SALES(REV_TYPE-21) = &
                         R_COMPETITIVE_SALES(REV_TYPE-21) &
                         + REVENUES_MONTHLY(PERIOD,ASSET_CLASS_loc,REV_TYPE)
                  R_COMPETITIVE_SALES_QUANT(REV_TYPE-21) = &
                     R_COMPETITIVE_SALES_QUANT(REV_TYPE-21) &
                     + ENERGY_MONTHLY(PERIOD,ASSET_CLASS_loc,REV_TYPE)/1000.
               ENDDO
            ENDIF
         ENDIF
         RETURN

      ENTRY GET_MARKET_GENERATED_REVEUNES(R_TOTAL_BASE_REVENUE)

         R_TOTAL_BASE_REVENUE = 0. !TOTAL_BASE_REVENUE
      RETURN
      END

      FUNCTION CPL_SALES_CLASS(R_CPL_JURISDICTION)

!
      use mthnmcom

      INTEGER (KIND=2) :: SALES_CLASSIFICATION,CPL_SALES_CLASS
      CHARACTER*(*) R_CPL_JURISDICTION
!
         SELECT CASE (trim(R_CPL_JURISDICTION))
!
         CASE ('NC Retail')
               CPL_SALES_CLASS = nc_retail
         CASE ('SC Retail')
               CPL_SALES_CLASS = sc_retail
         CASE ('Wholesale')
               CPL_SALES_CLASS = Wholesale
         CASE ('Other')
               CPL_SALES_CLASS = cpl_other
         CASE ('NCEMPA')
               CPL_SALES_CLASS = NCEMPA
         CASE ('NCEMC')
               CPL_SALES_CLASS = NCEMC
         CASE DEFAULT
            CPL_SALES_CLASS = cpl_other
         END SELECT
      RETURN
      END

!**
!**            FILE :CPLMAINS.FOR
!**            DATE : DECEMBER 16, 1998
!**            SOURCE FILE FOR NORTH CAROLINA POWER LINE
!**            (ALLOCATION SECTION)
!**
!**                     COPYRIGHT (C) 1998
!**               M.S. GERBER & ASSOCIATES, INC.
!**                     ALL RIGHTS RESERVED

      SUBROUTINE CPL_JURIS_CALCULATIONS(R_YEAR,REPORT_ALLOWED)

      USE IREC_ENDPOINT_CONTROL
      use grx_planning_routines
      use globecom
      use mod_base_year

      INTEGER (KIND=2) :: R_YEAR,VARIABLE_NUMBER
      LOGICAL (KIND=4) :: REPORT_ALLOWED
      REAL (KIND=4) :: &
                  COMBINED_SYSTEM_PEAK, &
                  JANUARY_MULTIPLIER, &
                  NCEMPA_RETAINED_CAPACITY, &
                  LOAD_FACTOR, &
                  HOURS_IN_YEAR, &
                  JURIS_PEAK_SUMMER_NC, &
                  SYSTEM_PEAK_SUMMER, &
                  JURIS_PEAK_WINTER_NC, &
                  SYSTEM_PEAK_WINTER, &
                  JURIS_ENERGY_SALES_NC, &
                  SYSTEM_ENERGY_SALES, &
                  JURIS_NCP_NC, &
                  SYSTEM_NCP_NC_SC, &
                  JURIS_PEAK_SUMMER_SC, &
                  JURIS_PEAK_WINTER_SC, &
                  JURIS_ENERGY_SALES_SC, &
                  JURIS_NCP_SC
      REAL (KIND=4) :: &
                  JURIS_PEAK_WHOLE, &
                  SYSTEM_PEAK_WHOLE, &
                  JURIS_ENERGY_SALES_WHOLE, &
                  SEPA_MW_NET_WHOLE, &
                  DEMAND_FACTOR_NC, &
                  ENERGY_FACTOR_NC, &
                  PROD_DEMAND_ALLOC_NC(30), &
                  R_PROD_DEMAND_ALLOC_NC, &
                  ENERGY_ALLOC_NC(30), &
                  R_ENERGY_ALLOC_NC, &
                  TRANS_DEMAND_ALLOC_NC(30), &    !
                  R_TRANS_DEMAND_ALLOC_NC, &
                  DISTRI_DEMAND_ALLOC_NC(30), &
                  R_DISTRI_DEMAND_ALLOC_NC, &
                  PROD_DEMAND_ALLOC_SC(30), &
                  R_PROD_DEMAND_ALLOC_SC
      REAL (KIND=4) :: &
                  ENERGY_ALLOC_SC(30), &
                  R_ENERGY_ALLOC_SC, &
                  TRANS_DEMAND_ALLOC_SC(30), &
                  R_TRANS_DEMAND_ALLOC_SC, &
                  DISTRI_DEMAND_ALLOC_SC(30), &
                  R_DISTRI_DEMAND_ALLOC_SC, &
                  PROD_DEMAND_ALLOC_WHOLE(30), &
                  R_PROD_DEMAND_ALLOC_WHOLE, &
                  ENERGY_ALLOC_WHOLE(30), &
                  R_ENERGY_ALLOC_WHOLE, &
                  TRANS_DEMAND_ALLOC_WHOLE(30), &
                  R_TRANS_DEMAND_ALLOC_WHOLE, &
                  PROD_DEMAND_ALLOC_NC_C(30), &
                  R_PROD_DEMAND_ALLOC_NC_C, &
                  ENERGY_ALLOC_NC_C(30), &
                  R_ENERGY_ALLOC_NC_C, &
                  TRANS_DEMAND_ALLOC_NC_C(30) !
      REAL (KIND=4) :: &
                  R_TRANS_DEMAND_ALLOC_NC_C, &
                  DISTRI_DEMAND_ALLOC_NC_C(30), &
                  R_DISTRI_DEMAND_ALLOC_NC_C, &
                  PROD_DEMAND_ALLOC_SC_A(30), &
                  R_PROD_DEMAND_ALLOC_SC_A, &
                  ENERGY_ALLOC_SC_A(30), &
                  R_ENERGY_ALLOC_SC_A, &
                  TRANS_DEMAND_ALLOC_SC_A(30), &
                  R_TRANS_DEMAND_ALLOC_SC_A, &
                  DISTRI_DEMAND_ALLOC_SC_A(30), &
                  R_DISTRI_DEMAND_ALLOC_SC_A, &
                  DEMAND_FACTOR_SC_A, &
                  ENERGY_FACTOR_SC_A, &
                  JURIS_PEAK_SUMMER_WHOLE, &
                  JURIS_PEAK_SUMMER_NCEMPA, &
                  JURIS_PEAK_SUMMER_NCEMC, &
                  JURIS_PEAK_WINTER_WHOLE
      REAL (KIND=4) :: &
                  JURIS_PEAK_WINTER_NCEMPA, &
                  JURIS_PEAK_WINTER_NCEMC, &
                  JURIS_ENERGY_SALES_NCEMPA, &
                  JURIS_ENERGY_SALES_NCEMC

      SAVE  &
                     PROD_DEMAND_ALLOC_NC, &
                     ENERGY_ALLOC_NC, &
                     TRANS_DEMAND_ALLOC_NC, &
                     DISTRI_DEMAND_ALLOC_NC, &
                     PROD_DEMAND_ALLOC_SC, &
                     ENERGY_ALLOC_SC, &
                     TRANS_DEMAND_ALLOC_SC, &
                     DISTRI_DEMAND_ALLOC_SC, &
                     PROD_DEMAND_ALLOC_WHOLE, &
                     ENERGY_ALLOC_WHOLE, &
                     TRANS_DEMAND_ALLOC_WHOLE, &
                     PROD_DEMAND_ALLOC_NC_C, &
                     ENERGY_ALLOC_NC_C, &
                     TRANS_DEMAND_ALLOC_NC_C, &
                     DISTRI_DEMAND_ALLOC_NC_C, &
                     PROD_DEMAND_ALLOC_SC_A, &
                     ENERGY_ALLOC_SC_A, &
                     TRANS_DEMAND_ALLOC_SC_A, &
                     DISTRI_DEMAND_ALLOC_SC_A


! DETAILED REPORT OVERHEAD
      LOGICAL (KIND=1) :: JURIS_REPORT_NOT_OPEN=.TRUE.,WVPA, &
        DYNAMIC_JURIS_ALLOC_ACTIVE,DYNAMIC_JURIS_ALLOC
      INTEGER (KIND=2) :: DYNAMIC_JURIS_RPT_HEADER,DYNAMIC_JURIS_UNIT=0
      INTEGER :: DYNAMIC_JURIS_REC
      SAVE DYNAMIC_JURIS_REC
!
      INTEGER (KIND=2) :: YR,R_VECTOR
      REAL (KIND=4) :: R_DATA(30)
      LOGICAL (KIND=1) :: CPL_ACTIVE
!
! END DATA DECLARATION
!
         IF(.NOT. CPL_ACTIVE()) RETURN
         DYNAMIC_JURIS_ALLOC_ACTIVE = DYNAMIC_JURIS_ALLOC()
!
         IF(DYNAMIC_JURIS_ALLOC_ACTIVE .AND. JURIS_REPORT_NOT_OPEN) THEN
            JURIS_REPORT_NOT_OPEN = .FALSE.
            VARIABLE_NUMBER = 48 !28 + 10 (MARK'S) + 9+1(GREG'S)
            DYNAMIC_JURIS_UNIT=DYNAMIC_JURIS_RPT_HEADER(VARIABLE_NUMBER, &
                DYNAMIC_JURIS_REC)
         ENDIF
         CALL CPL_JURIS_PEAK_N_SALES ( &
                                    JURIS_PEAK_SUMMER_NC,&
                    JURIS_PEAK_SUMMER_SC,JURIS_PEAK_SUMMER_WHOLE, &
                                    JURIS_PEAK_SUMMER_NCEMPA, &
                              JURIS_PEAK_SUMMER_NCEMC,SYSTEM_PEAK_SUMMER, &
                                    JURIS_PEAK_WINTER_NC, &
                                    JURIS_PEAK_WINTER_SC, &
                                    JURIS_PEAK_WINTER_WHOLE, &
                                    JURIS_PEAK_WINTER_NCEMPA, &
                                    JURIS_PEAK_WINTER_NCEMC, &
                                    SYSTEM_PEAK_WINTER, &
                                    SYSTEM_NCP_NC_SC, &
                                    JURIS_NCP_NC, &
                                    JURIS_NCP_SC, &
                                    JURIS_PEAK_WHOLE, &
                                    SYSTEM_PEAK_WHOLE, &
                                    JURIS_ENERGY_SALES_NC, &
                                    JURIS_ENERGY_SALES_SC, &
                                    JURIS_ENERGY_SALES_WHOLE, &
                                    JURIS_ENERGY_SALES_NCEMPA, &
                                    JURIS_ENERGY_SALES_NCEMC, &
                                    SYSTEM_ENERGY_SALES)



         SEPA_MW_NET_WHOLE = 1064.0

         CALL LEAP_YEAR_CALCULATION ( R_YEAR,HOURS_IN_YEAR)


         CALL  LOAD_FACTOR_CALCU (  SYSTEM_ENERGY_SALES, &
                                    HOURS_IN_YEAR, &
                                    SYSTEM_PEAK_SUMMER, &
                                    SYSTEM_PEAK_WINTER)

         CALL  GET_LOAD_FACTOR_CALCU (LOAD_FACTOR)


! Average  Method for SC
         CALL AVERAGE_ALLOC_FACTOR (     LOAD_FACTOR, &
                                    JURIS_PEAK_SUMMER_NC, &
                                    SYSTEM_PEAK_SUMMER, &
                                    JURIS_PEAK_WINTER_NC, &
                                    SYSTEM_PEAK_WINTER, &
                                    JURIS_ENERGY_SALES_NC, &
                                    SYSTEM_ENERGY_SALES, &
                                    JURIS_NCP_NC, &
                                    SYSTEM_NCP_NC_SC)

         CALL GET_AVERAGE_ALLOC_FACTOR(  DEMAND_FACTOR_NC, &
                                    ENERGY_FACTOR_NC, &
                                    PROD_DEMAND_ALLOC_NC(R_YEAR), &
                                    ENERGY_ALLOC_NC(R_YEAR), &
                                    TRANS_DEMAND_ALLOC_NC(R_YEAR), &
                                    DISTRI_DEMAND_ALLOC_NC(R_YEAR))

! Average Method for SC
         CALL AVERAGE_ALLOC_FACTOR (     LOAD_FACTOR, &
                                    JURIS_PEAK_SUMMER_SC, &
                                    SYSTEM_PEAK_SUMMER, &
                                    JURIS_PEAK_WINTER_SC, &
                                    SYSTEM_PEAK_WINTER, &
                                    JURIS_ENERGY_SALES_SC, &
                                    SYSTEM_ENERGY_SALES, &
                                    JURIS_NCP_SC, &
                                    SYSTEM_NCP_NC_SC)

         CALL GET_AVERAGE_ALLOC_FACTOR(  DEMAND_FACTOR_SC_A, &
                                    ENERGY_FACTOR_SC_A, &
                                    PROD_DEMAND_ALLOC_SC_A(R_YEAR), &
                                    ENERGY_ALLOC_SC_A(R_YEAR), &
                                    TRANS_DEMAND_ALLOC_SC_A(R_YEAR), &
                                    DISTRI_DEMAND_ALLOC_SC_A(R_YEAR))

! Single Coincident Method for NC
         CALL CO_IN_ALLOC_FACTOR (  JURIS_PEAK_SUMMER_NC, &
                                    SYSTEM_PEAK_SUMMER, &
                                    JURIS_ENERGY_SALES_NC, &
                                    SYSTEM_ENERGY_SALES, &
                                    JURIS_NCP_NC, &
                                    SYSTEM_NCP_NC_SC)

!

         CALL GET_CO_IN_ALLOC_FACTOR(  PROD_DEMAND_ALLOC_NC_C(R_YEAR), &
                                    ENERGY_ALLOC_NC_C(R_YEAR), &
                                    TRANS_DEMAND_ALLOC_NC_C(R_YEAR), &
                                    DISTRI_DEMAND_ALLOC_NC_C(R_YEAR))

! Single Coincident Method for SC
         CALL CO_IN_ALLOC_FACTOR (  JURIS_PEAK_SUMMER_SC, &
                                    SYSTEM_PEAK_SUMMER, &
                                    JURIS_ENERGY_SALES_SC, &
                                    SYSTEM_ENERGY_SALES, &
                                    JURIS_NCP_SC, &
                                    SYSTEM_NCP_NC_SC)

         CALL GET_CO_IN_ALLOC_FACTOR(  PROD_DEMAND_ALLOC_SC(R_YEAR), &
                                    ENERGY_ALLOC_SC(R_YEAR), &
                                    TRANS_DEMAND_ALLOC_SC(R_YEAR), &
                                    DISTRI_DEMAND_ALLOC_SC(R_YEAR))

         CALL WHOLE_ALLOC_FACTOR (  JURIS_PEAK_WHOLE, &
                                    SYSTEM_PEAK_WHOLE, &
                                    JURIS_ENERGY_SALES_WHOLE, &
                                    SYSTEM_ENERGY_SALES, &
                                    SEPA_MW_NET_WHOLE)


         CALL GET_WHOLE_ALLOC_FACTOR(  PROD_DEMAND_ALLOC_WHOLE(R_YEAR), &
                                       ENERGY_ALLOC_WHOLE(R_YEAR), &
                                       TRANS_DEMAND_ALLOC_WHOLE(R_YEAR))


         IF(DYNAMIC_JURIS_ALLOC_ACTIVE .AND. REPORT_ALLOWED) then
             WRITE(DYNAMIC_JURIS_UNIT,REC=DYNAMIC_JURIS_REC) &
                  PRT_ENDPOINT(), &
                  FLOAT(R_YEAR+get_BASE_YEAR()),JURIS_PEAK_SUMMER_NC, &     !0
                  SYSTEM_PEAK_SUMMER,JURIS_PEAK_WINTER_NC, &
                  SYSTEM_PEAK_WINTER,JURIS_ENERGY_SALES_NC/1000., &
                  SYSTEM_ENERGY_SALES/1000., &  ! 5
                  JURIS_NCP_NC,SYSTEM_NCP_NC_SC,JURIS_PEAK_SUMMER_SC, &
                  JURIS_ENERGY_SALES_SC/1000.,JURIS_NCP_SC, &               ! 10
                  JURIS_PEAK_WHOLE,SYSTEM_PEAK_WHOLE, &
                  JURIS_ENERGY_SALES_WHOLE/1000., &
                  SEPA_MW_NET_WHOLE,DEMAND_FACTOR_NC*100., & ! 15
                  ENERGY_FACTOR_NC*100., &
                  PROD_DEMAND_ALLOC_NC(R_YEAR)*100., &
                  ENERGY_ALLOC_NC(R_YEAR)*100., &
                  TRANS_DEMAND_ALLOC_NC(R_YEAR)*100., &
                  DISTRI_DEMAND_ALLOC_NC(R_YEAR)*100., &                                    ! 20
                  PROD_DEMAND_ALLOC_SC(R_YEAR)*100., &
                  ENERGY_ALLOC_SC(R_YEAR)*100., &
                  TRANS_DEMAND_ALLOC_SC(R_YEAR)*100., &
                  DISTRI_DEMAND_ALLOC_SC(R_YEAR)*100., &
                  PROD_DEMAND_ALLOC_WHOLE(R_YEAR)*100., &
                  ENERGY_ALLOC_WHOLE(R_YEAR)*100., &
                  TRANS_DEMAND_ALLOC_WHOLE(R_YEAR)*100, &
                  PROD_DEMAND_ALLOC_NC_C(R_YEAR)*100., &                                  ! 28
                  ENERGY_ALLOC_NC_C(R_YEAR)*100., &
                  TRANS_DEMAND_ALLOC_NC_C(R_YEAR)*100., &
                  DISTRI_DEMAND_ALLOC_NC_C(R_YEAR)*100., &
                  PROD_DEMAND_ALLOC_SC_A(R_YEAR)*100., &
                  ENERGY_ALLOC_SC_A(R_YEAR)*100., &
                  TRANS_DEMAND_ALLOC_SC_A(R_YEAR)*100., &
                  DISTRI_DEMAND_ALLOC_SC_A(R_YEAR)*100., & ! 35
                  JURIS_PEAK_SUMMER_WHOLE,JURIS_PEAK_SUMMER_NCEMPA, & ! 37
                  JURIS_PEAK_SUMMER_NCEMC,JURIS_PEAK_WINTER_SC, &
                  JURIS_PEAK_WINTER_WHOLE,JURIS_PEAK_WINTER_NCEMPA, &             ! 41
                  JURIS_PEAK_WINTER_NCEMC,JURIS_ENERGY_SALES_NCEMPA/1000., &
                  JURIS_ENERGY_SALES_NCEMC/1000.,LOAD_FACTOR*100., &   !45
                  DEMAND_FACTOR_SC_A*100.,ENERGY_FACTOR_SC_A*100.
            end if
            DYNAMIC_JURIS_REC = DYNAMIC_JURIS_REC + 1


      RETURN

      ENTRY GET_CPL_JURIS_N_SALES (R_YEAR, &
                     R_PROD_DEMAND_ALLOC_NC, &
                     R_ENERGY_ALLOC_NC, &
                     R_TRANS_DEMAND_ALLOC_NC, &
                     R_DISTRI_DEMAND_ALLOC_NC, &
                     R_PROD_DEMAND_ALLOC_SC, &
                     R_ENERGY_ALLOC_SC, &
                     R_TRANS_DEMAND_ALLOC_SC, &
                     R_DISTRI_DEMAND_ALLOC_SC, &
                     R_PROD_DEMAND_ALLOC_WHOLE, &
                     R_ENERGY_ALLOC_WHOLE, &
                     R_TRANS_DEMAND_ALLOC_WHOLE, &
                     R_PROD_DEMAND_ALLOC_NC_C, &
                     R_ENERGY_ALLOC_NC_C, &
                     R_TRANS_DEMAND_ALLOC_NC_C, &
                     R_DISTRI_DEMAND_ALLOC_NC_C, &
                     R_PROD_DEMAND_ALLOC_SC_A, &
                     R_ENERGY_ALLOC_SC_A, &
                     R_TRANS_DEMAND_ALLOC_SC_A, &
                     R_DISTRI_DEMAND_ALLOC_SC_A )

         R_PROD_DEMAND_ALLOC_NC   = PROD_DEMAND_ALLOC_NC (R_YEAR)
         R_ENERGY_ALLOC_NC        = ENERGY_ALLOC_NC(R_YEAR)
         R_TRANS_DEMAND_ALLOC_NC  = TRANS_DEMAND_ALLOC_NC(R_YEAR)
         R_DISTRI_DEMAND_ALLOC_NC = DISTRI_DEMAND_ALLOC_NC(R_YEAR)

!

         R_PROD_DEMAND_ALLOC_SC  = PROD_DEMAND_ALLOC_SC(R_YEAR)
         R_ENERGY_ALLOC_SC        = ENERGY_ALLOC_SC(R_YEAR)
         R_TRANS_DEMAND_ALLOC_SC  = TRANS_DEMAND_ALLOC_SC(R_YEAR)
         R_DISTRI_DEMAND_ALLOC_SC = DISTRI_DEMAND_ALLOC_SC (R_YEAR)

         R_PROD_DEMAND_ALLOC_WHOLE  = PROD_DEMAND_ALLOC_WHOLE(R_YEAR)
         R_ENERGY_ALLOC_WHOLE       = ENERGY_ALLOC_WHOLE (R_YEAR)
         R_TRANS_DEMAND_ALLOC_WHOLE = TRANS_DEMAND_ALLOC_WHOLE (R_YEAR)

         R_PROD_DEMAND_ALLOC_NC_C   = PROD_DEMAND_ALLOC_NC_C (R_YEAR)
         R_ENERGY_ALLOC_NC_C        = ENERGY_ALLOC_NC_C(R_YEAR)
         R_TRANS_DEMAND_ALLOC_NC_C  = TRANS_DEMAND_ALLOC_NC_C(R_YEAR)
         R_DISTRI_DEMAND_ALLOC_NC_C = DISTRI_DEMAND_ALLOC_NC_C (R_YEAR)

         R_PROD_DEMAND_ALLOC_SC_A   = PROD_DEMAND_ALLOC_SC_A(R_YEAR)
         R_ENERGY_ALLOC_SC_A        = ENERGY_ALLOC_SC_A(R_YEAR)
         R_TRANS_DEMAND_ALLOC_SC_A  = TRANS_DEMAND_ALLOC_SC_A(R_YEAR)
         R_DISTRI_DEMAND_ALLOC_SC_A = DISTRI_DEMAND_ALLOC_SC_A (R_YEAR)

      RETURN

      ENTRY RETURN_CPL_ALLOCATION_VECTOR(R_VECTOR,R_DATA)

!
! NC ALLOCATION VALUES
!
         IF(WVPA()) RETURN
         IF(R_VECTOR == 1) THEN
            DO YR = 1, 30
               R_DATA(YR) = 100.*PROD_DEMAND_ALLOC_NC(YR)
            ENDDO
         ENDIF
         IF(R_VECTOR == 2) THEN
            DO YR = 1, 30
               R_DATA(YR) = 100.*ENERGY_ALLOC_NC(YR)
            ENDDO
         ENDIF
         IF(R_VECTOR == 3) THEN
            DO YR = 1, 30
               R_DATA(YR) = 100.*TRANS_DEMAND_ALLOC_NC(YR)
            ENDDO
         ENDIF
         IF(R_VECTOR == 4) THEN
            DO YR = 1, 30
               R_DATA(YR) = 100.*DISTRI_DEMAND_ALLOC_NC(YR)
            ENDDO
         ENDIF

! SC ALLOCATION VECTORS

         IF(R_VECTOR == 6) THEN
            DO YR = 1, 30
               R_DATA(YR) = 100.*PROD_DEMAND_ALLOC_SC(YR)
            ENDDO
         ENDIF
         IF(R_VECTOR == 7) THEN
            DO YR = 1, 30
               R_DATA(YR) = 100.*ENERGY_ALLOC_SC(YR)
            ENDDO
         ENDIF
         IF(R_VECTOR == 8) THEN
            DO YR = 1, 30
               R_DATA(YR) = 100.*TRANS_DEMAND_ALLOC_SC(YR)
            ENDDO
         ENDIF
         IF(R_VECTOR == 9) THEN
            DO YR = 1, 30
               R_DATA(YR) = 100.*DISTRI_DEMAND_ALLOC_SC(YR)
            ENDDO
         ENDIF

! FERC ALLOCATION VECTORS

         IF(R_VECTOR == 11) THEN
            DO YR = 1, 30
               R_DATA(YR) = 100.*PROD_DEMAND_ALLOC_WHOLE(YR)
            ENDDO
         ENDIF
         IF(R_VECTOR == 12) THEN
            DO YR = 1, 30
               R_DATA(YR) = 100.*ENERGY_ALLOC_WHOLE(YR)
            ENDDO
         ENDIF
         IF(R_VECTOR == 13) THEN
            DO YR = 1, 30
               R_DATA(YR) = 100.*TRANS_DEMAND_ALLOC_WHOLE(YR)
            ENDDO
         ENDIF
      RETURN
      END

!!
!!
!!      routine NORTH CAROLINA ALLOCATION FACTOR
!!
!!      CALCULATE THE ALLOCATION FACTOR FOR NC AND SC
!!           !!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      WITH !!!!!! AVERAGE METHOD !!!!!!
!!           !!!!!!!!!!!!!!!!!!!!!!!!!!!!

      SUBROUTINE AVERAGE_ALLOC_FACTOR( LOAD_FACTOR_NC, &
                                    JURIS_PEAK_SUMMER_NC, &
                                    SYSTEM_PEAK_SUMMER_NC, &
                                    JURIS_PEAK_WINTER_NC, &
                                    SYSTEM_PEAK_WINTER_NC, &
                                    JURIS_ENERGY_SALES_NC, &
                                    SYSTEM_ENERGY_SALES_NC, &
                                    JURIS_NCP_NC, &
                                    SYSTEM_NCP_NC_SC)

      REAL (KIND=4) :: &
               LOAD_FACTOR_NC, &
               JURIS_PEAK_SUMMER_NC, &
               SYSTEM_PEAK_SUMMER_NC, &
               JURIS_PEAK_WINTER_NC, &
               SYSTEM_PEAK_WINTER_NC, &
               JURIS_ENERGY_SALES_NC, &
               SYSTEM_ENERGY_SALES_NC, &
               JURIS_NCP_NC, &
               SYSTEM_NCP_NC_SC, &
               DEMAND_FACTOR_NC, &
               ENERGY_FACTOR_NC
      REAL (KIND=4) :: &
               PROD_DEMAND_ALLOC_NC, &
               ENERGY_ALLOC_NC, &
               TRANSMISSION_ALLOC_NC, &
               DISTRI_DEMAND_ALLOC_NC, &
               R_DEMAND_FACTOR_NC, &
               R_ENERGY_FACTOR_NC, &
               R_PROD_DEMAND_ALLOC_NC, &
               R_ENERGY_ALLOC_NC, &
               R_TRANSMISSION_ALLOC_NC, &
               R_DISTRI_DEMAND_ALLOC_NC

      SAVE &
               DEMAND_FACTOR_NC, &
               ENERGY_FACTOR_NC, &
               PROD_DEMAND_ALLOC_NC, &
               ENERGY_ALLOC_NC, &
               TRANSMISSION_ALLOC_NC, &
               DISTRI_DEMAND_ALLOC_NC

!
! END DATA DECLARATIONS
!
         DEMAND_FACTOR_NC = (1.0 - LOAD_FACTOR_NC) * &
                    (((JURIS_PEAK_SUMMER_NC / SYSTEM_PEAK_SUMMER_NC) + &
                 (JURIS_PEAK_WINTER_NC / SYSTEM_PEAK_WINTER_NC)) / 2.0)

         ENERGY_FACTOR_NC = JURIS_ENERGY_SALES_NC / &
            SYSTEM_ENERGY_SALES_NC * LOAD_FACTOR_NC

         PROD_DEMAND_ALLOC_NC = DEMAND_FACTOR_NC + ENERGY_FACTOR_NC

         ENERGY_ALLOC_NC = JURIS_ENERGY_SALES_NC / SYSTEM_ENERGY_SALES_NC

         TRANSMISSION_ALLOC_NC = &
        (JURIS_PEAK_SUMMER_NC + JURIS_PEAK_WINTER_NC) / &
        (SYSTEM_PEAK_SUMMER_NC + SYSTEM_PEAK_WINTER_NC)

         IF(SYSTEM_NCP_NC_SC == 0.) THEN
            DISTRI_DEMAND_ALLOC_NC = 0.
         ELSE
            DISTRI_DEMAND_ALLOC_NC = JURIS_NCP_NC / SYSTEM_NCP_NC_SC
         ENDIF

      RETURN


      ENTRY GET_AVERAGE_ALLOC_FACTOR( R_DEMAND_FACTOR_NC, &
                                R_ENERGY_FACTOR_NC, &
                                R_PROD_DEMAND_ALLOC_NC, &
                                R_ENERGY_ALLOC_NC, &
                                R_TRANSMISSION_ALLOC_NC, &
                                R_DISTRI_DEMAND_ALLOC_NC)


         R_DEMAND_FACTOR_NC         = DEMAND_FACTOR_NC
         R_ENERGY_FACTOR_NC         = ENERGY_FACTOR_NC
         R_PROD_DEMAND_ALLOC_NC     = PROD_DEMAND_ALLOC_NC
         R_ENERGY_ALLOC_NC          = ENERGY_ALLOC_NC
         R_TRANSMISSION_ALLOC_NC    = TRANSMISSION_ALLOC_NC
         R_DISTRI_DEMAND_ALLOC_NC   = DISTRI_DEMAND_ALLOC_NC

       RETURN

      END


!!
!!
!!      ROUTINE SC AND NC ALLOCATION FACTOR
!!
!!      CALCULATE ALLOCATION FACTOR FOR SC
!!      WITH SINGLE COINCIDENT METHOD
!!
       SUBROUTINE CO_IN_ALLOC_FACTOR(  JURIS_PEAK_SUMMER_SC, &
                                       SYSTEM_PEAK_SUMMER_SC, &
                                       JURIS_ENERGY_SALES_SC, &
                                       SYSTEM_ENERGY_SALES_SC, &
                                       JURIS_NCP_SC, &
                                       SYSTEM_NCP_NC_SC)

      LOGICAL (KIND=1) :: WVPA
      REAL (KIND=4) ::   JURIS_PEAK_SUMMER_SC, &
               SYSTEM_PEAK_SUMMER_SC, &
               JURIS_ENERGY_SALES_SC, &
               SYSTEM_ENERGY_SALES_SC, &
               JURIS_NCP_SC, &
               SYSTEM_NCP_NC_SC, &
               PROD_DEMAND_ALLOC_SC, &
               ENERGY_ALLOC_SC, &
               TRANS_DEMAND_ALLOC_SC, &
               DISTRI_DEMAND_ALLOC_SC, &
               R_PROD_DEMAND_ALLOC_SC, &
               R_ENERGY_ALLOC_SC, &
               R_TRANS_DEMAND_ALLOC_SC, &
               R_DISTRI_DEMAND_ALLOC_SC

      SAVE &
               PROD_DEMAND_ALLOC_SC, &
               ENERGY_ALLOC_SC, &
               TRANS_DEMAND_ALLOC_SC, &
               DISTRI_DEMAND_ALLOC_SC

!
! END DATA DECLARATIONS
!

         IF(WVPA()) RETURN
         PROD_DEMAND_ALLOC_SC = 0.
         ENERGY_ALLOC_SC = 0.
         TRANS_DEMAND_ALLOC_SC = 0.
         DISTRI_DEMAND_ALLOC_SC = 0.
         IF(SYSTEM_PEAK_SUMMER_SC /= 0.) then
            PROD_DEMAND_ALLOC_SC = JURIS_PEAK_SUMMER_SC/ SYSTEM_PEAK_SUMMER_SC
         end if

         IF(SYSTEM_ENERGY_SALES_SC /= 0.) then
            ENERGY_ALLOC_SC = JURIS_ENERGY_SALES_SC/ SYSTEM_ENERGY_SALES_SC
         end if

         IF(SYSTEM_PEAK_SUMMER_SC /= 0.) then
            TRANS_DEMAND_ALLOC_SC = JURIS_PEAK_SUMMER_SC/ SYSTEM_PEAK_SUMMER_SC
         end if


         IF(SYSTEM_NCP_NC_SC /= 0.) then
            DISTRI_DEMAND_ALLOC_SC = JURIS_NCP_SC/SYSTEM_NCP_NC_SC
         end if


      RETURN


      ENTRY GET_CO_IN_ALLOC_FACTOR( R_PROD_DEMAND_ALLOC_SC, &
                                 R_ENERGY_ALLOC_SC, &
                                 R_TRANS_DEMAND_ALLOC_SC, &
                                 R_DISTRI_DEMAND_ALLOC_SC)



         IF(WVPA()) RETURN
         R_PROD_DEMAND_ALLOC_SC   = PROD_DEMAND_ALLOC_SC
         R_ENERGY_ALLOC_SC        = ENERGY_ALLOC_SC
         R_TRANS_DEMAND_ALLOC_SC  = TRANS_DEMAND_ALLOC_SC
         R_DISTRI_DEMAND_ALLOC_SC = DISTRI_DEMAND_ALLOC_SC
      RETURN


      END


!!
!!
!!      ROUTINE WHOLESALE ALLOCATION FACTOR
!!
!!      CALCULATE ALLOCATION FACTOR FOR CAROLINA
!!      WHOLESALE WITH 12 CP METHOD
!!
      SUBROUTINE  WHOLE_ALLOC_FACTOR(JURIS_PEAK_WHOLE, &
                                       SYSTEM_PEAK_WHOLE, &
                                       JURIS_ENERGY_SALES_WHOLE, &
                                       SYSTEM_ENERGY_SALES_WHOLE, &
                                       SEPA_MW_NET_WHOLE)

      REAL (KIND=4) :: &
               JURIS_PEAK_WHOLE, &
               SYSTEM_PEAK_WHOLE, &
               JURIS_ENERGY_SALES_WHOLE, &
               SYSTEM_ENERGY_SALES_WHOLE, &
               SEPA_MW_NET_WHOLE, &
               PROD_DEMAND_ALLOC_WHOLE, &
               ENERGY_ALLOC_WHOLE, &
               TRANS_DEMAND_ALLOC_WHOLE, &
               R_PROD_DEMAND_ALLOC_WHOLE, &
               R_ENERGY_ALLOC_WHOLE, &
               R_TRANS_DEMAND_ALLOC_WHOLE


      SAVE &
               PROD_DEMAND_ALLOC_WHOLE, &
               ENERGY_ALLOC_WHOLE, &
               TRANS_DEMAND_ALLOC_WHOLE

!
! END DATA DECLARATIONS
!

         PROD_DEMAND_ALLOC_WHOLE = JURIS_PEAK_WHOLE / SYSTEM_PEAK_WHOLE

         ENERGY_ALLOC_WHOLE = JURIS_ENERGY_SALES_WHOLE / SYSTEM_ENERGY_SALES_WHOLE

         TRANS_DEMAND_ALLOC_WHOLE = JURIS_PEAK_WHOLE / &
            (SYSTEM_PEAK_WHOLE + SEPA_MW_NET_WHOLE)


      RETURN


      ENTRY GET_WHOLE_ALLOC_FACTOR( R_PROD_DEMAND_ALLOC_WHOLE, &
                                    R_ENERGY_ALLOC_WHOLE, &
                                    R_TRANS_DEMAND_ALLOC_WHOLE)


         R_PROD_DEMAND_ALLOC_WHOLE  = PROD_DEMAND_ALLOC_WHOLE
         R_ENERGY_ALLOC_WHOLE       = ENERGY_ALLOC_WHOLE
         R_TRANS_DEMAND_ALLOC_WHOLE = TRANS_DEMAND_ALLOC_WHOLE

      RETURN
      END

      !! Calculate load factor
      SUBROUTINE LOAD_FACTOR_CALCU (SYSTEM_ENERGY_SALES, &
                                    HOURS_IN_YEAR, &
                                    SYSTEM_PEAK_SUMMER, &
                                    SYSTEM_PEAK_WINTER )
      implicit none

      REAL (KIND=4) :: &
               SYSTEM_ENERGY_SALES, &
               HOURS_IN_YEAR, &
               SYSTEM_PEAK_SUMMER, &
               SYSTEM_PEAK_WINTER, &
               LOAD_FACTOR, &
               R_LOAD_FACTOR

      SAVE LOAD_FACTOR
!
! END DATA DECLARATIONS
!

         LOAD_FACTOR = &
                SYSTEM_ENERGY_SALES / HOURS_IN_YEAR  / &
               ((SYSTEM_PEAK_SUMMER +  SYSTEM_PEAK_WINTER) /2)


       RETURN


      ENTRY GET_LOAD_FACTOR_CALCU (R_LOAD_FACTOR)



         R_LOAD_FACTOR      = LOAD_FACTOR

      RETURN
      END



      SUBROUTINE MANAGE_ASSET_ALLOCATIONS(REPORT_ALLOWED)
      use spindriftlib
      use prod_arrays_dimensions
      USE SIZECOM
      use globecom

         INTEGER (KIND=2) :: LOCAL_YEAR,END_YEAR
         LOGICAL (KIND=1) :: CPL_IS_ACTIVE,CPL_ACTIVE
         LOGICAL (KIND=4) :: REPORT_ALLOWED
!
! END DATA DECLARATIONS
!
         IF(.NOT. CPL_ACTIVE()) RETURN
!
         CALL INIT_SALES_REVENUE_BY_CLASS
!
         END_YEAR = MIN(get_globecom_study_period(),AVAIL_DATA_YEARS)
!
         DO LOCAL_YEAR = 1, END_YEAR
            CALL READ_CPL_DATA(LOCAL_YEAR)
            CALL GET_CPL_JURIS_ALLOCATION_DATA(LOCAL_YEAR)
            CALL CPL_JURIS_CALCULATIONS(LOCAL_YEAR,REPORT_ALLOWED)
         ENDDO
!
         CALL CLOSE_CLASS_SALES_FILE
!
      RETURN
      END




      SUBROUTINE LEAP_YEAR_CALCULATION (R_YEAR,R_HOURS_IN_YEAR)

      use globecom
      use mod_base_year


      INTEGER (KIND=2) :: CURRENT_YEAR, &
                R_YEAR, &
                LEAP_YEAR, &
                TEMP_LEAP_YEAR
      REAL (KIND=4) ::    HOURS_IN_YEAR, &
                R_HOURS_IN_YEAR

      SAVE HOURS_IN_YEAR
      !
! END DATA DECLARATIONS

!
      CURRENT_YEAR = get_BASE_YEAR() + R_YEAR

!
      TEMP_LEAP_YEAR = MOD (CURRENT_YEAR,INT(100,2))
      LEAP_YEAR = MOD (TEMP_LEAP_YEAR,INT(4,2))

         IF(LEAP_YEAR == 0) THEN
            HOURS_IN_YEAR = 8784.
         ELSE
            HOURS_IN_YEAR = 8760.
         ENDIF



         R_HOURS_IN_YEAR = HOURS_IN_YEAR

      RETURN
       END

!
      SUBROUTINE RPS_STATE_DEMAND_OBJECT
      use end_routine, only: end_program, er_message
!

!
      use spindriftlib
      use prod_arrays_dimensions
      USE SIZECOM

      INTEGER (KIND=2) :: INUNIT,IREC,DELETE,LRECL=1042
      INTEGER (KIND=4) :: IOS,IOS_BASE
      CHARACTER (len=5) :: OVERLAY_FAMILY_NAME
      CHARACTER (len=10) :: STATE_OR_PROVINCE
      CHARACTER (len=256) :: FILE_NAME
      CHARACTER (len=256) :: BASE_FILE_DIRECTORY
      CHARACTER (len=256) :: OUTPUT_DIRECTORY,DATA_DRIVE
      LOGICAL (KIND=4) :: FILE_EXISTS,FILE_OPEN
      CHARACTER (len=5) :: BASE_FILE_NAME,RPS_STATE_DEMAND_FILE
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER (LEN=1024) :: RECLN
! DECLARATION FOR CLASS FORECASTS
      INTEGER (KIND=2) :: YEAR,DATA_RECORDS_IN_TABLE
      CHARACTER (LEN=32) :: CLASS_NAME,ENERGY_UNITS*3,TEMP_CLASS_NAME
      CHARACTER (LEN=16) :: FILE_TYPE='RPS State Demand' ! leading space needed
      INTEGER (KIND=2) :: SAVE_CLASS_SALES_UNIT,CLASS_SALES_UNIT
      INTEGER (KIND=2) :: BC_TABLE_NUMBER,OL_TABLE_NUMBER
      INTEGER (KIND=2) :: R_NUMBER_OF_TABLES
      SAVE BC_TABLE_NUMBER,OL_TABLE_NUMBER
      CHARACTER (LEN=2) :: CLASS_SALES_OL='BC'
      REAL (KIND=4) :: ENERGY_LOSS_FACTOR, &
             DEMAND_LOSS_FACTOR, &
             STATE_RPS_ALLOCATION, &
             STATE_RPS_EXEMPTION
!
!
! INTRA-COMPANY DATA
!
      INTEGER (KIND=2) :: TRANSACT_FORECAST_GROUP, & ! 110
                          TRANSACT_CUSTOMER_GROUP ! 111
      INTEGER (KIND=2) :: R_NUM_OF_CLASSES
      CHARACTER (LEN=1) :: ACCOUNT_ACTIVE
      LOGICAL (KIND=1) :: LAHEY_LF95
      CHARACTER (len=30) :: SCREEN_OUTPUT
      INTEGER :: RECORDS_IN_OVERLAY_TABLE
      LOGICAL (KIND=1) :: READ_OVERLAY_INPUT
!

!
!          ROUTINE TO CONVERT METAFILE FILES TO DIRECT-ACESS BINARY
!          COPYRIGHT (C) 1983, 84, 85  M.S. GERBER & ASSOCIATES, INC.
!

!
! CONVERT THE CLASS-FORECAST-DATA FILES

      ENTRY RPS_STATE_DEMAND_MAKEBIN

!
      BASE_FILE_NAME = RPS_STATE_DEMAND_FILE()
      FILE_NAME = trim(BASE_FILE_DIRECTORY())// &
        "RPB"//trim(BASE_FILE_NAME)//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      DATA_DRIVE = OUTPUT_DIRECTORY()
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_LOCATE_WRITE(16,30,BASE_FILE_NAME,ALL_VERSIONS,0)
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         ENDIF
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCRPS_ST.BIN", &
            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         IREC = 0
         BC_TABLE_NUMBER = 0
!
         READ(10,*,IOSTAT=IOS) DELETE
         DO
            ENERGY_LOSS_FACTOR = 0.
            DEMAND_LOSS_FACTOR = 0.
            TRANSACT_FORECAST_GROUP = 0
            TRANSACT_CUSTOMER_GROUP = 0
            STATE_OR_PROVINCE = 'AL_45'
            STATE_RPS_ALLOCATION = 100.0
            STATE_RPS_EXEMPTION = 0.0
            BC_TABLE_NUMBER = BC_TABLE_NUMBER + 1
            DO
               READ(10,1000,IOSTAT=IOS) RECLN
               IF(IOS /= 0) EXIT
               IF(RECLN(1:1) == '7') EXIT
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
                                    ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
                                    ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
                                    ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,ERR=200) &
                                 DELETE, &
                                 YEAR, &
                                 CLASS_NAME, &
                                 ACCOUNT_ACTIVE, &
                                 TRANSACT_FORECAST_GROUP, &
                                 TRANSACT_CUSTOMER_GROUP, &
                                 ENERGY_LOSS_FACTOR, &
                                 DEMAND_LOSS_FACTOR, &
                                 STATE_OR_PROVINCE, &
                                 STATE_RPS_ALLOCATION, &
                                 STATE_RPS_EXEMPTION


!
! WRITE RECORD
!
               IREC = IREC + 1
               WRITE(11,REC=IREC) &
                                 DELETE, &
                                 YEAR, &
                                 CLASS_NAME, &
                                 ACCOUNT_ACTIVE, &
                                 TRANSACT_FORECAST_GROUP, &
                                 TRANSACT_CUSTOMER_GROUP, &
                                 ENERGY_LOSS_FACTOR, &
                                 DEMAND_LOSS_FACTOR, &
                                 STATE_OR_PROVINCE, &
                                 STATE_RPS_ALLOCATION, &
                                 STATE_RPS_EXEMPTION
               DATA_RECORDS_IN_TABLE = DATA_RECORDS_IN_TABLE + 1
            ENDDO
            IF(IOS /= 0) EXIT
         ENDDO
         CLOSE(11)
      ELSE IF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "msgmmrev:1116")
      ENDIF
      RETURN

      ENTRY RPS_STATE_DEMAND_MAKEOVL(OVERLAY_FAMILY_NAME)

         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
            CALL LOCATE(10,51)
         ENDIF
         DATA_DRIVE = OUTPUT_DIRECTORY()
         FILE_NAME = trim(DATA_DRIVE) &
            //"RPO"//trim(OVERLAY_FAMILY_NAME)//".DAT"
         OPEN(10,FILE=FILE_NAME)
         READ(10,*) DELETE
         INUNIT = 12
         IF(CLASS_SALES_OL == 'BC') THEN
            OPEN(11,FILE=trim(DATA_DRIVE)//"BCRPS_ST.BIN", &
                ACCESS="DIRECT",RECL=LRECL)
            INUNIT = 11
         ENDIF
         OPEN(12,FILE=trim(DATA_DRIVE)//"OLRPS_ST.BIN", &
            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         IREC = 0
         OL_TABLE_NUMBER = 0
         DO
            OL_TABLE_NUMBER = OL_TABLE_NUMBER + 1
            RECORDS_IN_OVERLAY_TABLE = 0
            READ_OVERLAY_INPUT = .TRUE.
            DO
               IF(READ_OVERLAY_INPUT) READ(10,1000,IOSTAT=IOS) RECLN
               IF(RECLN(1:1) == '7' .AND. READ_OVERLAY_INPUT) THEN
                  IF(RECORDS_IN_OVERLAY_TABLE == AVAIL_DATA_YEARS) EXIT
                  READ_OVERLAY_INPUT = .FALSE.
               ENDIF
               IREC = IREC + 1
               READ(INUNIT,REC=IREC,IOSTAT=IOS_BASE) &
                                 DELETE, &
                                 YEAR, &
                                 CLASS_NAME, &
                                 ACCOUNT_ACTIVE, &
                                 TRANSACT_FORECAST_GROUP, &
                                 TRANSACT_CUSTOMER_GROUP, &
                                 ENERGY_LOSS_FACTOR, &
                                 DEMAND_LOSS_FACTOR, &
                                 STATE_OR_PROVINCE, &
                                 STATE_RPS_ALLOCATION, &
                                 STATE_RPS_EXEMPTION
               IF(IOS_BASE /= 0) EXIT
!
!
!
               IF(IOS == 0 .AND. READ_OVERLAY_INPUT) THEN
                  RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
                                      ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
                                      ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
                                      ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
                  READ(RECLN,*,ERR=200) &
                                 DELETE, &
                                 YEAR, &
                                 CLASS_NAME, &
                                 ACCOUNT_ACTIVE, &
                                 TRANSACT_FORECAST_GROUP, &
                                 TRANSACT_CUSTOMER_GROUP, &
                                 ENERGY_LOSS_FACTOR, &
                                 DEMAND_LOSS_FACTOR, &
                                 STATE_OR_PROVINCE, &
                                 STATE_RPS_ALLOCATION, &
                                 STATE_RPS_EXEMPTION
               ENDIF
!
! WRITE RECORD
!
               RECORDS_IN_OVERLAY_TABLE = RECORDS_IN_OVERLAY_TABLE + 1
               WRITE(12,REC=IREC) &
                                 DELETE, &
                                 YEAR, &
                                 CLASS_NAME, &
                                 ACCOUNT_ACTIVE, &
                                 TRANSACT_FORECAST_GROUP, &
                                 TRANSACT_CUSTOMER_GROUP, &
                                 ENERGY_LOSS_FACTOR, &
                                 DEMAND_LOSS_FACTOR, &
                                 STATE_OR_PROVINCE, &
                                 STATE_RPS_ALLOCATION, &
                                 STATE_RPS_EXEMPTION
               IF(RECORDS_IN_OVERLAY_TABLE >= AVAIL_DATA_YEARS) EXIT
            ENDDO
            IF(IOS_BASE /= 0) EXIT
         ENDDO
         CLOSE(10)
         CLOSE(12)
         IF(CLASS_SALES_OL == 'BC') CLOSE(11)
         CLASS_SALES_OL= 'OL'
      RETURN
  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from Msgmmrev SIID233'
      call end_program(er_message)

      ENTRY RETURN_RPS_DEMAND_TABLE_NUM(R_NUMBER_OF_TABLES)

         IF(CLASS_SALES_OL == 'OL') THEN
            R_NUMBER_OF_TABLES = OL_TABLE_NUMBER
         ELSE
            R_NUMBER_OF_TABLES = BC_TABLE_NUMBER
         ENDIF
      RETURN


      ENTRY RESET_RPS_STATE_DEMAND_OL


         CLASS_SALES_OL = 'BC'
      RETURN


      ENTRY OPEN_RPS_STATE_FILE(CLASS_SALES_UNIT)

         SAVE_CLASS_SALES_UNIT = CLASS_SALES_UNIT
         INQUIRE(UNIT=CLASS_SALES_UNIT,OPENED=FILE_OPEN)
         IF(FILE_OPEN) CLOSE(CLASS_SALES_UNIT)
         OPEN(CLASS_SALES_UNIT,FILE=trim(OUTPUT_DIRECTORY())// &
            CLASS_SALES_OL//"RPS_ST.BIN",ACCESS="DIRECT",RECL=LRECL)
      RETURN


      ENTRY CLOSE_RPS_STATE_FILE

         CLOSE(SAVE_CLASS_SALES_UNIT)

 1000 FORMAT(A)
! 1010 FORMAT('&',A)
      END


!  READS SALES TABLES AND ALLOCATES TO CLASS


      RECURSIVE SUBROUTINE MANAGE_RPS_STATE_DEMAND

      use dr_booth_modules
      use enrglimit
      use globecom
      use grx_planning_routines
      use irec_endpoint_control
      use mod_base_year
      use prod_arrays_dimensions
      use rptreccontrol
      use sizecom
      use spindriftlib
      use debugtrace
      use program_state
      save

!
      integer :: file_trace_mrsd=0
      INTEGER (KIND=2) :: R_MONTH
      INTEGER (KIND=4) :: VALUES_TO_ZERO
      INTEGER (KIND=2) :: MO,I,IREC,R_YEAR, &
        R_ST_TG,DELETE,SALES_UNIT
      INTEGER (KIND=4) :: IOS
      CHARACTER (LEN=2) :: GREEN_MRX_METHOD
      
      
      CHARACTER (LEN=32) :: RATE_CLASS_NAME
      INTEGER (KIND=2) :: STATE_RPS_VAR_NUM=13, & ! 052817
                          RPS_CATEGORIES=7, &     ! CHANGED TO 7 ON 052817
                          RPS_USER_DEFINED_REGION(0:61), &
                          RPS_REGION_COUNT, &
                          R_NUM_REGIONS, &
                          R_STATE_REGION(0:61), &
                          R_REGION_POSITION(0:99), &
                          RPS_REGION_INDEX(0:99), &
                          RPS_REGION_POSITION(0:99), &
                          RPS_STATE_IN_REGION(99), &
                          TEMP_I2
      REAL (KIND=4) :: R_STATE_RPS_VARS(13), &
                       R_STATE_SURPLUS_VARS(0:7), &
                       STATE_RPS_DB(:,:,:), &
                       RPS_SURPLUS(:,:), &
                       ANNUAL_SURPLUS_OR_DEFICIT(:,:,:), &
                       RPS_REC_PERCENT_ALLOWED(61), &
                       ANN_RPS_INTRA_STATE_SURPLUS(0:7,0:61), &
                       ANN_RPS_INTRA_REGION_SURPLUS(:,:)
      ALLOCATABLE :: STATE_RPS_DB, &
                     RPS_SURPLUS, &
                     ANNUAL_SURPLUS_OR_DEFICIT, &
                     ANN_RPS_INTRA_REGION_SURPLUS
      REAL (KIND=4) :: &
           TRANS_ENERGY(0:12), &
           TRANS_PEAK(0:12), &
           TRANS_CUSTOMERS(0:12), &
           TEMP_TRANS_ENERGY(0:12), &
           TEMP_TRANS_PEAK(0:12), &
           TEMP_TRANS_CUSTOMERS(0:12)
      LOGICAL (KIND=1) :: GET_TG_CG_DATA,DATA_FOUND,TEMP_DATA_FOUND, &
        TEMP_L,GET_STATE_RPS_REQS
      REAL (KIND=4) :: ANNUAL_PEAK_DEMAND,ANNUAL_ENERGY
      INTEGER (KIND=2) :: FILE_TABLES
      CHARACTER (len=10) :: STATE_OR_PROVINCE
      CHARACTER (LEN=1) :: ACCOUNT_TYPE ! 107
      CHARACTER (LEN=6) :: TEMP_CHAR_6
      REAL :: STATE_RPS_ALLOCATION, &
              STATE_RPS_EXEMPTION, &  ! 128
              RPS_REQS(11), &         ! 050617 ! 122616.
              TEMP_R
      INTEGER (KIND=2) :: TRANSACT_FORECAST_GROUP, &   ! 110
                          TRANSACT_CUSTOMER_GROUP, &   ! 111
                          ST,STATE_ID_LOOKUP, &
                          RPS,ST_TG,REGION
      CHARACTER (LEN=3) :: ENERGY_UNITS
      INTEGER (KIND=2) :: RUN_YEAR
! ADDITIONAL SECTION
      REAL (KIND=4) :: ENERGY_LOSS_FACTOR, &
                       DEMAND_LOSS_FACTOR, &
                       TEMP_PEAK

      CHARACTER (LEN=1) :: ACCOUNT_ACTIVE
      INTEGER (KIND=2) :: R_RUN_YEAR

      INTEGER (KIND=2) :: DATA_POS,J

      CHARACTER (len=256) :: FILE_NAME
      CHARACTER (len=256) :: BASE_FILE_DIRECTORY
      CHARACTER (LEN=5000) :: RECLN,DUMMY_CHR*20
      INTEGER (KIND=2) :: SKIP,YR,CLASS_POS

      INTEGER (KIND=4) :: NEXT_REC=0
      INTEGER (KIND=2) :: RC_UNIT_NO=0
      REAL (KIND=4) :: THE_RATIO_OF_A_TO_B
      CHARACTER (LEN=9) :: CL_MONTH_NAME(0:12)= &
                               (/'Annual   ', &
                                'January  ','February ', &
                                'March    ','April    ', &
                                'May      ','June     ', &
                                'July     ','August   ', &
                                'September','October  ', &
                                'November ','December '/)
      LOGICAL (KIND=1) :: STATE_RPS_REPORT_ACTIVE, &
                          STATE_RPS_REPORT_NOT_OPEN=.TRUE., &
                          TEMP_L1,GET_TRANS_RPS_SUM, &
                          GET_TRANS_GRX_RPS_SUM, &
                          TRANSACT_FUEL_REPORT, &
                          YES_OLD_RPS_PROGRAMS_REPORT
      INTEGER (KIND=2) :: STATE_RPS_NO,STATE_RPS_RPT_HEADER, &
                          NUM_RESOURCE_RPS_VARS=16 ! 052817
      INTEGER (KIND=4) :: STATE_RPS_REC
      REAL (KIND=4) :: RESOURCE_RPS_VARS(16), &   ! 052817
                       TOTAL_RES_REQ_RPS_VARS(16) ! 052817
      CHARACTER (LEN=20) :: TEMP_RPS_NAME,STATE_NAME_LOOKUP
!
! END DATA DECLARATIONS
!
         if(file_trace_mrsd==0) then
            file_trace_mrsd=open_trace("MANAGE_RPS_STATE_DEMAND.trace", &
                rq_mrsd)
            call write_trace_message(file_trace_mrsd, &
                "MANAGE_RPS_STATE_DEMAND trace file - including " // &
                "ANNUAL_RPS_STATE_DEMAND")
                
         end if
         CALL RETURN_RPS_DEMAND_TABLE_NUM(FILE_TABLES)
         IF(FILE_TABLES > 0) THEN
            IF(ALLOCATED(STATE_RPS_DB)) &
                                   DEALLOCATE(STATE_RPS_DB, &
                                              RPS_SURPLUS, &
                                              ANNUAL_SURPLUS_OR_DEFICIT)
            ALLOCATE(STATE_RPS_DB(13,600,0:12), &
                     RPS_SURPLUS(0:7,0:61), &
                     ANNUAL_SURPLUS_OR_DEFICIT(2,61,0:7))

            RPS_REC_PERCENT_ALLOWED = 100.0
            RPS_USER_DEFINED_REGION = 0
            RPS_REGION_COUNT = 0
            RPS_REGION_INDEX = 0
            RPS_STATE_IN_REGION = 0
            RPS_REGION_POSITION = 0
!            ANN_RPS_INTRA_STATE_SURPLUS = 0.0
         ENDIF
      RETURN

      ENTRY ANNUAL_RPS_STATE_DEMAND(R_YEAR)
         call write_trace_message(file_trace_mrsd, &
            "ANNUAL_RPS_STATE_DEMAND")
         call write_trace_int2(file_trace_mrsd, "R_YEAR", R_YEAR)
         call write_trace_int2(file_trace_mrsd, "FILE_TABLES", &
            FILE_TABLES)
         IF(FILE_TABLES == 0) RETURN
         RUN_YEAR = MIN(AVAIL_DATA_YEARS,R_YEAR)
         call write_trace_int2(file_trace_mrsd, "RUN_YEAR", RUN_YEAR)
!
! ZERO ARRAYS
!
!
         STATE_RPS_DB = 0.0
         TOTAL_RES_REQ_RPS_VARS = 0.0
!
!
! READ SALES FILE
!
         CALL OPEN_RPS_STATE_FILE(SALES_UNIT)
         IREC = RUN_YEAR - AVAIL_DATA_YEARS
         DO I = 1, FILE_TABLES
            IREC = IREC + AVAIL_DATA_YEARS
            READ(SALES_UNIT,REC=IREC,IOSTAT=IOS) &
                                 DELETE, &
                                 LOCAL_YEAR, &
                                 RATE_CLASS_NAME, &
                                 ACCOUNT_ACTIVE, &
                                 TRANSACT_FORECAST_GROUP, &
                                 TRANSACT_CUSTOMER_GROUP, &
                                 ENERGY_LOSS_FACTOR, &
                                 DEMAND_LOSS_FACTOR, &
                                 STATE_OR_PROVINCE, &
                                 STATE_RPS_ALLOCATION, &
                                 STATE_RPS_EXEMPTION
            IF(IOS /= 0) EXIT
            IF(DELETE >= 8 .OR. ACCOUNT_ACTIVE == 'N') CYCLE
            TRANS_ENERGY = 0.
            TRANS_PEAK = 0.
            TRANS_CUSTOMERS = 0.
            DATA_FOUND = GET_TG_CG_DATA(TRANSACT_FORECAST_GROUP, &
                                              TRANSACT_CUSTOMER_GROUP, &
                                              TRANS_ENERGY, &
                                              TRANS_PEAK, &
                                              TRANS_CUSTOMERS)
!
            IF(.NOT. DATA_FOUND) CYCLE
            TEMP_CHAR_6 = STATE_OR_PROVINCE(1:2)
            ST = STATE_ID_LOOKUP(TEMP_CHAR_6)
            TEMP_L = GET_STATE_RPS_REQS(ST,R_YEAR,RPS_REQS)
            RPS_REC_PERCENT_ALLOWED(ST) = RPS_REQS(9)*0.01
            RPS_USER_DEFINED_REGION(ST) = INT(RPS_REQS(10),2)
            IF( RPS_USER_DEFINED_REGION(ST) > 99 .OR. &
                RPS_USER_DEFINED_REGION(ST) < 0) THEN
               RPS_USER_DEFINED_REGION(ST) = 0
            ENDIF
            TEMP_I2 = RPS_USER_DEFINED_REGION(ST)
            IF(TEMP_I2 > 0) THEN
               IF(RPS_REGION_POSITION(TEMP_I2) == 0) THEN
                  RPS_REGION_COUNT = RPS_REGION_COUNT + 1
                  RPS_REGION_POSITION(TEMP_I2) = RPS_REGION_COUNT
                  RPS_REGION_INDEX(RPS_REGION_COUNT) = TEMP_I2
                  RPS_STATE_IN_REGION(RPS_REGION_COUNT) = ST
               ENDIF
            ENDIF
            TEMP_CHAR_6 = STATE_OR_PROVINCE
            ST_TG = STATE_ID_LOOKUP(TEMP_CHAR_6)
            IF(STATE_RPS_EXEMPTION > 0.001) THEN
               STATE_RPS_EXEMPTION = (100.0 - STATE_RPS_EXEMPTION)/100.0
            ELSE
               STATE_RPS_EXEMPTION = 1.00
            ENDIF
!
            DO MO = 1, 12
! RPS
               IF(TEMP_L) THEN
                  TEMP_R = TRANS_ENERGY(MO) * 0.001 * &
                    STATE_RPS_ALLOCATION * 0.01
                  STATE_RPS_DB(1,ST,MO) = STATE_RPS_DB(1,ST,MO) + TEMP_R
                  STATE_RPS_DB(1,ST,0) = STATE_RPS_DB(1,ST,0) + TEMP_R
!
! LOSS PERCENT (3) CALCULATED AT REPORT TIME
!
                  STATE_RPS_DB(2,ST,MO) = STATE_RPS_DB(2,ST,MO) + &
                     TEMP_R * ENERGY_LOSS_FACTOR/100.
                  STATE_RPS_DB(2,ST,0) = STATE_RPS_DB(2,ST,0) + &
                     TEMP_R* ENERGY_LOSS_FACTOR/100.
! RETAIL DEMAND: GENERATION - LOSSES
                  TEMP_R = TEMP_R  * (1. - ENERGY_LOSS_FACTOR/100.)
                  STATE_RPS_DB(4,ST,MO) = STATE_RPS_DB(4,ST,MO) + TEMP_R
                  STATE_RPS_DB(4,ST,0) = STATE_RPS_DB(4,ST,0) + TEMP_R
                  STATE_RPS_DB(5,ST,MO) = RPS_REQS(1)
                  STATE_RPS_DB(5,ST,0) = RPS_REQS(1)
 !
                  DO RPS = 6, 13
                     STATE_RPS_DB(RPS,ST,MO) = STATE_RPS_DB(RPS,ST,MO) + &
                         TEMP_R * RPS_REQS(RPS-5) * 0.01 * STATE_RPS_EXEMPTION
                     STATE_RPS_DB(RPS,ST,0) = STATE_RPS_DB(RPS,ST,0) +&
                         TEMP_R * RPS_REQS(RPS-5) * 0.01 * STATE_RPS_EXEMPTION
                  END DO

               ENDIF
!
! ACCUMULATE FOR EACH ST_TG
!
               IF(TEMP_L .AND. ST_TG > 0) THEN
                  TEMP_R = TRANS_ENERGY(MO) * 0.001 * STATE_RPS_ALLOCATION * 0.01
                  STATE_RPS_DB(1,ST_TG,MO) = STATE_RPS_DB(1,ST_TG,MO) + TEMP_R
                  STATE_RPS_DB(1,ST_TG,0) = STATE_RPS_DB(1,ST_TG,0) + TEMP_R
!
! LOSS PERCENT (3) CALCULATED AT REPORT TIME
!
                  STATE_RPS_DB(2,ST_TG,MO) = STATE_RPS_DB(2,ST_TG,MO) + &
                    TEMP_R * ENERGY_LOSS_FACTOR/100.
                  STATE_RPS_DB(2,ST_TG,0) = STATE_RPS_DB(2,ST_TG,0) + &
                    TEMP_R* ENERGY_LOSS_FACTOR/100.
! RETAIL DEMAND
                  TEMP_R = TEMP_R  * (1.-ENERGY_LOSS_FACTOR/100.)
                  STATE_RPS_DB(4,ST_TG,MO) = STATE_RPS_DB(4,ST_TG,MO) + TEMP_R
                  STATE_RPS_DB(4,ST_TG,0) = STATE_RPS_DB(4,ST_TG,0) + TEMP_R
! NOTE: REDEFINE STATE_RPS_EXEMPTION
!                  STATE_RPS_EXEMPTION = (1.-STATE_RPS_EXEMPTION*0.01)
                  STATE_RPS_DB(5,ST_TG,MO) = RPS_REQS(1)
                  STATE_RPS_DB(5,ST_TG,0) = RPS_REQS(1)
                  DO RPS = 6, 13
                     STATE_RPS_DB(RPS,ST_TG,MO) = &
                           STATE_RPS_DB(RPS,ST_TG,MO) + &
                                   TEMP_R * RPS_REQS(RPS-5) * 0.01 * &
                                                    STATE_RPS_EXEMPTION
                     STATE_RPS_DB(RPS,ST_TG,0) = &
                           STATE_RPS_DB(RPS,ST_TG,0) + &
                                    TEMP_R * RPS_REQS(RPS-5) * 0.01 * &
                                                     STATE_RPS_EXEMPTION
                  END DO

               ENDIF
            ENDDO ! MONTH
         ENDDO ! FILE TABLES
         CALL CLOSE_RPS_STATE_FILE
! 090616. FOR GRX OR MRX.
         IF (ALLOCATED(STATE_RPS_DB)) THEN
            RPS_SURPLUS = 0.0
            ANN_RPS_INTRA_STATE_SURPLUS = 0.0
            ANNUAL_SURPLUS_OR_DEFICIT = 0.0
            IF(GREEN_MRX_METHOD() == 'GX') THEN
               IF(ALLOCATED(ANN_RPS_INTRA_REGION_SURPLUS)) then
                   DEALLOCATE(ANN_RPS_INTRA_REGION_SURPLUS)
               end if
               ALLOCATE(ANN_RPS_INTRA_REGION_SURPLUS(0:6,0:RPS_REGION_COUNT))
               ANN_RPS_INTRA_REGION_SURPLUS = 0.0
               DO ST = 1, 61
                  REGION = RPS_USER_DEFINED_REGION(ST)
                  REGION = RPS_REGION_POSITION(REGION)
                  DO MO = 1, 12
                     IF(STATE_RPS_DB(1,ST,MO) < 0.01) CYCLE
                     RESOURCE_RPS_VARS = 0.0
                     IF(GRX_ITERATIONS == 0) THEN
                        TEMP_L1 = GET_TRANS_RPS_SUM(MO,ST,RESOURCE_RPS_VARS)
                     ELSE
                        TEMP_L1 = GET_TRANS_GRX_RPS_SUM(MO,ST,RESOURCE_RPS_VARS)
                     ENDIF
                     CALL GET_HYDRO_RPS_SUM(MO,ST,RESOURCE_RPS_VARS)
                     CALL GET_THERMAL_RPS_SUM(MO,ST,RESOURCE_RPS_VARS)
                     DO J = 0, 7
                        TOTAL_RES_REQ_RPS_VARS(J+1) = &
                                     TOTAL_RES_REQ_RPS_VARS(J+1) + &
                                                  RESOURCE_RPS_VARS(J+1)
                        TOTAL_RES_REQ_RPS_VARS(J+8) = &
                                     TOTAL_RES_REQ_RPS_VARS(J+8) + &
                                                 STATE_RPS_DB(J+6,ST,MO)
                        RPS_SURPLUS(J,ST) = RPS_SURPLUS(J,ST) + &
                                       RESOURCE_RPS_VARS(J+1) - &
                                            STATE_RPS_DB(J+6,ST,MO)
                        RPS_SURPLUS(J,0) = RPS_SURPLUS(J,0) + &
                                       RESOURCE_RPS_VARS(J+1) - &
                                            STATE_RPS_DB(J+6,ST,MO)
                        IF(RPS_SURPLUS(J,ST) > 0.0) THEN ! SURPLUS
                           ANNUAL_SURPLUS_OR_DEFICIT(1,ST,J) = RPS_SURPLUS(J,ST)
                        ELSE
                           ANNUAL_SURPLUS_OR_DEFICIT(2,ST,J) = & ! DEFICIT
                                                      -RPS_SURPLUS(J,ST)
                        ENDIF
                     END DO ! RENEW TYPE
                  ENDDO ! MONTH
                  IF(RPS_REC_PERCENT_ALLOWED(ST) <= 0.999) THEN

! IS DEFICIT GREATER THAN THE INTERNAL RPS REQUIREMENT?
                     IF(R_YEAR == 4) THEN
                        R_YEAR = R_YEAR
                     ENDIF
                     DO J = 0, 7
                        TEMP_R = STATE_RPS_DB(J+6,ST,0) + RPS_SURPLUS(J,ST)
                        IF( TEMP_R < &
                           STATE_RPS_DB(J+6,ST,0) * &
                              (1.0 - RPS_REC_PERCENT_ALLOWED(ST)) ) THEN
                           ANN_RPS_INTRA_STATE_SURPLUS(J,ST) = &
                              TEMP_R - STATE_RPS_DB(J+6,ST,0) * &
                                     (1.0 - RPS_REC_PERCENT_ALLOWED(ST))
                           ANN_RPS_INTRA_REGION_SURPLUS(J,REGION) = &
                                ANN_RPS_INTRA_REGION_SURPLUS(J,REGION) + &
                                   TEMP_R - STATE_RPS_DB(J+6,ST,0) * &
                                     (1.0 - RPS_REC_PERCENT_ALLOWED(ST))

                        ENDIF ! RESOURCE IS LESS THAN ST REQUIREMENT
                     ENDDO ! J
                  ENDIF ! LESS THAN 100% ALLOWED
               ENDDO ! STATES / PROVINCES
            ENDIF ! STATE REPORT OR GRX
         ENDIF ! RPS VARS ARE ALLOCATED
! END RPS
      RETURN

      ENTRY WRITE_STATE_RPS_REPORT(R_YEAR,R_MONTH)

         IF (ALLOCATED(STATE_RPS_DB)) THEN
            RPS_SURPLUS = 0.0
            ANNUAL_SURPLUS_OR_DEFICIT = 0.0
!
            STATE_RPS_REPORT_ACTIVE = YES_OLD_RPS_PROGRAMS_REPORT()
            IF( STATE_RPS_REPORT_ACTIVE .AND. &
                STATE_RPS_REPORT_NOT_OPEN) THEN
               TEMP_I2 = STATE_RPS_VAR_NUM + &
                         NUM_RESOURCE_RPS_VARS + &
                         RPS_CATEGORIES + 1
               STATE_RPS_NO = STATE_RPS_RPT_HEADER(TEMP_I2,STATE_RPS_REC)
               STATE_RPS_REPORT_NOT_OPEN = .FALSE.
            ENDIF
            IF(STATE_RPS_REPORT_ACTIVE .OR. GREEN_MRX_METHOD() == 'GX') THEN
               DO ST = 1, 61
                  IF(STATE_RPS_DB(1,ST,R_MONTH) < 0.01) CYCLE
                  RESOURCE_RPS_VARS = 0.0
                  TEMP_RPS_NAME = STATE_NAME_LOOKUP(ST)
                  IF(STATE_RPS_DB(1,ST,R_MONTH) > 0.001) THEN
                     STATE_RPS_DB(3,ST,R_MONTH) = &
                        100.0 *(1. - STATE_RPS_DB(4,ST,R_MONTH)/ &
                                        STATE_RPS_DB(1,ST,R_MONTH))
                  ENDIF
                  TEMP_L1 = GET_TRANS_RPS_SUM(R_MONTH,ST,RESOURCE_RPS_VARS)
                  CALL GET_HYDRO_RPS_SUM(R_MONTH,ST,RESOURCE_RPS_VARS)
                  CALL GET_THERMAL_RPS_SUM(R_MONTH,ST,RESOURCE_RPS_VARS)
                  IF(R_MONTH == 0) THEN
                     DO J = 9, 16
                        RESOURCE_RPS_VARS(J) = RESOURCE_RPS_VARS(J) / 12.0
                     ENDDO
                  ENDIF
                  DO J = 0, 7
                     RPS_SURPLUS(J,ST) = RESOURCE_RPS_VARS(J+1) - &
                        STATE_RPS_DB(J+6,ST,R_MONTH)
                     RPS_SURPLUS(J,0) = RPS_SURPLUS(J,0) + RPS_SURPLUS(J,ST)
                     IF(RPS_SURPLUS(J,ST) > 0.0) THEN ! SURPLUS
                        ANNUAL_SURPLUS_OR_DEFICIT(1,ST,J) = RPS_SURPLUS(J,ST)
                     ELSE
                        ANNUAL_SURPLUS_OR_DEFICIT(2,ST,J) = & ! DEFICIT
                                                      -RPS_SURPLUS(J,ST)
                     ENDIF
                  END DO
                  IF(STATE_RPS_REPORT_ACTIVE) THEN
                     STATE_RPS_REC = RPTREC(STATE_RPS_NO)
                     WRITE(STATE_RPS_NO,REC=STATE_RPS_REC) &
                        PRT_ENDPOINT(), &
                        FLOAT(R_YEAR+get_BASE_YEAR()), &
                        CL_MONTH_NAME(R_MONTH), &
                        TEMP_RPS_NAME, &
                        (STATE_RPS_DB(I,ST,R_MONTH), &
                            I=1,STATE_RPS_VAR_NUM), &
                        RESOURCE_RPS_VARS, &
                        (RPS_SURPLUS(J,ST), J=0,RPS_CATEGORIES)
                     STATE_RPS_REC = STATE_RPS_REC + 1
                  ENDIF ! STATE REPORT ACTIVE
               ENDDO ! STATES / PROVINCES
            ENDIF ! STATE REPORT OR GRX
         ENDIF ! RPS VARS ARE ALLOCATED
      RETURN

      ENTRY GET_ANNUAL_STATE_SURPLUS_DATA(R_ST_TG,R_STATE_SURPLUS_VARS)

         R_STATE_SURPLUS_VARS = 0.0
         IF (ALLOCATED(RPS_SURPLUS)) THEN
            IF(R_ST_TG == 0) THEN
               R_STATE_SURPLUS_VARS(:) = RPS_SURPLUS(:,R_ST_TG)
            ELSEIF(R_ST_TG < 0) THEN
               R_STATE_SURPLUS_VARS(:) = &
                ANN_RPS_INTRA_REGION_SURPLUS(:,-R_ST_TG)
            ELSE
               R_STATE_SURPLUS_VARS(:) = &
                ANN_RPS_INTRA_STATE_SURPLUS(:,R_ST_TG)
            ENDIF
         END IF
      RETURN

      ENTRY GET_ANNUAL_REGION_DATA(R_NUM_REGIONS, &
                                   R_STATE_REGION, &
                                   R_REGION_POSITION)

         R_NUM_REGIONS = RPS_REGION_COUNT
         R_STATE_REGION(0:61) = RPS_USER_DEFINED_REGION(0:61)
         R_REGION_POSITION(0:99) = RPS_REGION_POSITION(0:99)
      RETURN

      ENTRY GET_STATE_RPS_DATA(R_MONTH,R_ST_TG,R_STATE_RPS_VARS)

         R_STATE_RPS_VARS = 0.0
         IF (ALLOCATED(STATE_RPS_DB)) THEN
            IF(STATE_RPS_DB(1,R_ST_TG,R_MONTH) > 0.001) THEN
               STATE_RPS_DB(3,R_ST_TG,R_MONTH) = &
                  100.0 *(1. - STATE_RPS_DB(4,R_ST_TG,R_MONTH)/ &
                                        STATE_RPS_DB(1,R_ST_TG,R_MONTH))
            ENDIF
            R_STATE_RPS_VARS(:) = STATE_RPS_DB(:,R_ST_TG,R_MONTH)
         END IF
      RETURN
      END

!
      SUBROUTINE RETROFIT_OBJECT
      use end_routine, only: end_program, er_message
!

!
      use spindriftlib
      use prod_arrays_dimensions
      USE SIZECOM

      INTEGER (KIND=2) :: INUNIT,IREC,DELETE,LRECL=160
      INTEGER (KIND=4) :: IOS,IOS_BASE
      CHARACTER (len=5) :: OVERLAY_FAMILY_NAME
      CHARACTER (len=10) :: STATE_OR_PROVINCE
      CHARACTER (len=256) :: FILE_NAME
      CHARACTER (len=256) :: BASE_FILE_DIRECTORY
      CHARACTER (len=256) :: OUTPUT_DIRECTORY,DATA_DRIVE
      LOGICAL (KIND=4) :: FILE_EXISTS,FILE_OPEN
      CHARACTER (len=5) :: BASE_FILE_NAME,THERMAL_RETROFIT_FILE
      CHARACTER (LEN=6) :: UNIT_ID
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER (LEN=1024) :: RECLN
! DECLARATION FOR CLASS FORECASTS
      INTEGER (KIND=2) :: YEAR,DATA_RECORDS_IN_TABLE
      CHARACTER (LEN=32) :: CLASS_NAME,ENERGY_UNITS*3,TEMP_CLASS_NAME
      CHARACTER (LEN=16) :: FILE_TYPE='Thermal Retrofit     ' ! leading space needed
      INTEGER (KIND=2) :: SAVE_CLASS_SALES_UNIT=0,CLASS_SALES_UNIT
      INTEGER (KIND=2) :: BC_RECORD_NUMBER=0,OL_RECORD_NUMBER=0
      INTEGER (KIND=2) :: R_NUMBER_OF_RECORDS, &
                R_MAX_PROJECT_ID, &
                MAX_PROJECT_ID=0
      INTEGER (KIND=4) :: EV_ID
      SAVE BC_RECORD_NUMBER,OL_RECORD_NUMBER
      CHARACTER (LEN=2) :: CLASS_SALES_OL='BC'
! DATA FOR THERMAL VARIABLES
      CHARACTER (len=30) :: Project_Name ! C*30
      CHARACTER (LEN=1) :: Project_Active ! C*1
      CHARACTER (LEN=12) :: Project_Type ! C*12
      INTEGER (KIND=2) :: Project_ID, &    ! I*2
                   First_Year_Available, & ! I*2
                   Last_Year_Available, &  ! I*2
                   Capital_Cost_Esc, &     ! I*2
                   Var_OM_Esc, &           ! I*2
                   Fixed_OM_Esc            ! I*2
      REAL (KIND=4) :: &
                   Learning_Curve, &       ! R*4
                   Capital_Cost_CCS, &     ! R*4
                   Capital_Cost_Trans, &   ! R*4
                   CRR_On_Project, &       ! R*4
                   Var_OM_Adder, &         ! R*4
                   Var_Storage_OM_Adder, & ! R*4
                   Fixed_OM_Capture_Adder, & ! R*4
                   Fixed_OM_Trans, &       ! R*4
                   SO2_Control_Percent, &  ! R*4
                   NOx_Control_Percent, &  ! R*4
                   CO2_Control_Percent, &  ! R*4
                   HG_Control_Percent, &   ! R*4
                   Other_Control_Percent, & ! R*4
                   Replacement_Fuel, &     ! R*4
                   Ave_Heat_Rate_Mult, &   ! R*4
                   Forced_Outage_Adder, &  ! R*4
                   Dispatch_Adder, &       ! R*4
                   Min_Capacity_Change, &  ! R*4
                   Max_Capacity_Change     ! R*4
! END RETROFIT DATA
!
      CHARACTER (LEN=1) :: ACCOUNT_ACTIVE
      LOGICAL (KIND=1) :: LAHEY_LF95
      CHARACTER (len=30) :: SCREEN_OUTPUT
      INTEGER :: RECORDS_IN_OVERLAY_TABLE
      LOGICAL (KIND=1) :: READ_OVERLAY_INPUT
!

!
!          ROUTINE TO CONVERT METAFILE FILES TO DIRECT-ACESS BINARY
!          COPYRIGHT (C) 1983, 84, 85  M.S. GERBER & ASSOCIATES, INC.
!

!
! CONVERT THE CLASS-FORECAST-DATA FILES

      ENTRY THERMAL_RETROFIT_MAKEBIN

!
      BASE_FILE_NAME = THERMAL_RETROFIT_FILE()
      FILE_NAME = trim(BASE_FILE_DIRECTORY())// &
        "RTB"//trim(BASE_FILE_NAME)//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      DATA_DRIVE = OUTPUT_DIRECTORY()
      BC_RECORD_NUMBER = 0
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_LOCATE_WRITE(16,30,BASE_FILE_NAME,ALL_VERSIONS,0)
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         ENDIF
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCRETROFIT.BIN", &
            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         IREC = 0
         Project_Active = 'F'
         Project_Type = 'CCS'
         First_Year_Available = 0
         Last_Year_Available = 0
         Learning_Curve = 0
         Capital_Cost_CCS = 0
         Capital_Cost_Trans = 0
         Capital_Cost_Esc = 0
         CRR_On_Project = 0
         Var_OM_Adder = 0
         Var_Storage_OM_Adder = 0
         Var_OM_Esc = 0
         Fixed_OM_Capture_Adder = 0
         Fixed_OM_Trans = 0
         Fixed_OM_Esc = 0
         SO2_Control_Percent = 0
         NOx_Control_Percent = 0
         CO2_Control_Percent = 0
         HG_Control_Percent = 0
         Other_Control_Percent = 0
         Replacement_Fuel = 0
         Ave_Heat_Rate_Mult = 0
         Forced_Outage_Adder = 0
         Dispatch_Adder = 0
         Min_Capacity_Change = 0
         Max_Capacity_Change = 0
         MAX_PROJECT_ID = 0
!
         READ(10,*,IOSTAT=IOS) DELETE
         DO
            DO
               READ(10,1000,IOSTAT=IOS) RECLN
               IF(IOS /= 0) EXIT
               IF(RECLN(1:1) == '7') EXIT
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
                                      ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
                                      ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
                                      ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,ERR=200) &
                DELETE,Project_Name, & ! C*30
                                 Project_Active,Project_Type, &
                                 Project_ID,First_Year_Available, & ! C*1 - C*12 - I*2 - I*2
                                 Last_Year_Available,Learning_Curve, &
                                 Capital_Cost_CCS,Capital_Cost_Trans, & ! I*2 - R*4 - R*4 - R*4
                                 Capital_Cost_Esc,CRR_On_Project,Var_OM_Adder,Var_Storage_OM_Adder, &      ! I*2 - R*4 - R*4 - R*4
                                 Var_OM_Esc, &                 ! I*2
                                 Fixed_OM_Capture_Adder, &     ! R*4
                                 Fixed_OM_Trans, &             ! R*4
                                 Fixed_OM_Esc, &               ! I*2
                                 SO2_Control_Percent, &        ! R*4
                                 NOx_Control_Percent, &        ! R*4
                                 CO2_Control_Percent, &        ! R*4
                                 HG_Control_Percent, &         ! R*4
                                 Other_Control_Percent, &       ! R*4
                                 Replacement_Fuel, &             ! R*4
                                 Ave_Heat_Rate_Mult, &         ! R*4
                                 Forced_Outage_Adder, &        ! R*4
                                 Dispatch_Adder, &            ! R*4
                                 Min_Capacity_Change, &         ! R*4
                                 Max_Capacity_Change, &     ! R*4
                                 UNIT_ID,EV_ID
!
! WRITE RECORD
!
               IREC = IREC + 1
               MAX_PROJECT_ID = MAX(Project_ID,MAX_PROJECT_ID)
               WRITE(11,REC=IREC)DELETE,Project_Name,Project_Active,Project_Type,Project_ID, &
                                 First_Year_Available,Last_Year_Available,Learning_Curve,Capital_Cost_CCS, & ! I*2 - I*2 - R*4 - R*4
                                 Capital_Cost_Trans,Capital_Cost_Esc,CRR_On_Project,Var_OM_Adder, &
                                 Var_Storage_OM_Adder, & ! R*4
                                 Var_OM_Esc, & ! I*2
                                 Fixed_OM_Capture_Adder, & ! R*4
                                 Fixed_OM_Trans, & ! R*4
                                 Fixed_OM_Esc, & ! I*2
                                 SO2_Control_Percent, & ! R*4
                                 NOx_Control_Percent, & ! R*4
                                 CO2_Control_Percent, & ! R*4
                                 HG_Control_Percent, & ! R*4
                                 Other_Control_Percent, & ! R*4
                                 Replacement_Fuel, & ! R*4
                                 Ave_Heat_Rate_Mult, & ! R*4
                                 Forced_Outage_Adder, & ! R*4
                                 Dispatch_Adder, & ! R*4
                                 Min_Capacity_Change, & ! R*4
                                 Max_Capacity_Change, & ! R*4
                                 UNIT_ID,EV_ID
               DATA_RECORDS_IN_TABLE = DATA_RECORDS_IN_TABLE + 1
            ENDDO
            IF(IOS /= 0) EXIT
         ENDDO
         BC_RECORD_NUMBER = DATA_RECORDS_IN_TABLE
         CLOSE(11)
      ELSE IF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "msgmmrev:1115")
      ENDIF
      RETURN

      ENTRY THERMAL_RETROFIT_MAKEOVL(OVERLAY_FAMILY_NAME)

         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
            CALL LOCATE(10,51)
         ENDIF
         DATA_DRIVE = OUTPUT_DIRECTORY()
         FILE_NAME = trim(DATA_DRIVE)// &
             "RTO"//trim(OVERLAY_FAMILY_NAME)//".DAT"
         OPEN(10,FILE=FILE_NAME)
         READ(10,*) DELETE
         INUNIT = 12
         IF(CLASS_SALES_OL == 'BC') THEN
            OPEN(11,FILE=trim(DATA_DRIVE)//"BCRETROFIT.BIN", &
                ACCESS="DIRECT",RECL=LRECL)
            INUNIT = 11
         ENDIF
         OPEN(12,FILE=trim(DATA_DRIVE)//"OLRETROFIT.BIN", &
            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         IREC = 0
         OL_RECORD_NUMBER = 0
!
         RECORDS_IN_OVERLAY_TABLE = 0

            DO
!
               READ(10,1000,IOSTAT=IOS) RECLN
               IF(RECLN(1:1) >= '7') then
                CYCLE
               end if
!
               IREC = IREC + 1
               READ(INUNIT,REC=IREC,IOSTAT=IOS_BASE) &
                DELETE,Project_Name,Project_Active,Project_Type, &
                                 Project_ID,First_Year_Available,Last_Year_Available, &
                                    Learning_Curve, &
                  Capital_Cost_CCS,Capital_Cost_Trans,Capital_Cost_Esc, &
                  CRR_On_Project, &
                                 Var_OM_Adder,Var_Storage_OM_Adder, &
                                 Var_OM_Esc, &             ! I*2
                                 Fixed_OM_Capture_Adder, & ! R*4
                                 Fixed_OM_Trans, &         ! R*4
                                 Fixed_OM_Esc, &           ! I*2
                                 SO2_Control_Percent, &    ! R*4
                                 NOx_Control_Percent, &    ! R*4
                                 CO2_Control_Percent, &    ! R*4
                                 HG_Control_Percent, &     ! R*4
                                 Other_Control_Percent, &  ! R*4
                                 Replacement_Fuel, &       ! R*4
                                 Ave_Heat_Rate_Mult, &     ! R*4
                                 Forced_Outage_Adder, &    ! R*4
                                 Dispatch_Adder, &         ! R*4
                                 Min_Capacity_Change, &    ! R*4
                                 Max_Capacity_Change, &    ! R*4
                                 UNIT_ID,EV_ID
               IF(IOS_BASE /= 0) EXIT
!
!
               IF(IOS == 0) THEN
                  RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
                                      ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
                                      ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
                                      ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
                  READ(RECLN,*,ERR=200) &
                  DELETE,Project_Name,Project_Active,Project_Type, &
                  Project_ID, &
                  First_Year_Available,Last_Year_Available, &
                  Learning_Curve,Capital_Cost_CCS, &
                                Capital_Cost_Trans, &
                                Capital_Cost_Esc, &
                                CRR_On_Project,Var_OM_Adder, &
                                Var_Storage_OM_Adder, & ! R*4
                                Var_OM_Esc, & ! I*2
                                Fixed_OM_Capture_Adder, & ! R*4
                                Fixed_OM_Trans, & ! R*4
                                Fixed_OM_Esc, & ! I*2
                                SO2_Control_Percent, & ! R*4
                                NOx_Control_Percent, & ! R*4
                                CO2_Control_Percent, & ! R*4
                                HG_Control_Percent, & ! R*4
                                Other_Control_Percent, & ! R*4
                                Replacement_Fuel, & ! R*4
                                Ave_Heat_Rate_Mult, & ! R*4
                                Forced_Outage_Adder, & ! R*4
                                Dispatch_Adder, & ! R*4
                                Min_Capacity_Change, & ! R*4
                                Max_Capacity_Change, & ! R*4
                                UNIT_ID,EV_ID
               ENDIF
!
! WRITE RECORD
!
               RECORDS_IN_OVERLAY_TABLE = RECORDS_IN_OVERLAY_TABLE + 1
               MAX_PROJECT_ID = MAX(Project_ID,MAX_PROJECT_ID)
               WRITE(12,REC=IREC) &
                                 DELETE,Project_Name,Project_Active, &
                                 Project_Type,Project_ID, &
                                 First_Year_Available,Last_Year_Available,Learning_Curve,Capital_Cost_CCS, & ! I*2 - I*2 - R*4 - R*4
                                 Capital_Cost_Trans,Capital_Cost_Esc, &
                                 CRR_On_Project,Var_OM_Adder, &
                                 Var_Storage_OM_Adder,Var_OM_Esc, & ! R*4 - I*2
                                 Fixed_OM_Capture_Adder, & ! R*4
                                 Fixed_OM_Trans, &         ! R*4
                                 Fixed_OM_Esc, &           ! I*2
                                 SO2_Control_Percent, &    ! R*4
                                 NOx_Control_Percent, &    ! R*4
                                 CO2_Control_Percent, &    ! R*4
                                 HG_Control_Percent, &     ! R*4
                                 Other_Control_Percent, &  ! R*4
                                 Replacement_Fuel, &       ! R*4
                                 Ave_Heat_Rate_Mult, &     ! R*4
                                 Forced_Outage_Adder, &    ! R*4
                                 Dispatch_Adder, &         ! R*4
                                 Min_Capacity_Change, &    ! R*4
                                 Max_Capacity_Change, &    ! R*4
                                 UNIT_ID,EV_ID
!
            ENDDO
!
         OL_RECORD_NUMBER = RECORDS_IN_OVERLAY_TABLE
         CLOSE(10)
         CLOSE(12)
         IF(CLASS_SALES_OL == 'BC') CLOSE(11)
         CLASS_SALES_OL= 'OL'
      RETURN
  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from Msgmmrev SIID234'
      call end_program(er_message)

      ENTRY RETURN_RETROFIT_RECORD_NUM(R_NUMBER_OF_RECORDS, &
                                       R_MAX_PROJECT_ID)

         IF(CLASS_SALES_OL == 'OL') THEN
            R_NUMBER_OF_RECORDS = OL_RECORD_NUMBER
         ELSE
            R_NUMBER_OF_RECORDS = BC_RECORD_NUMBER
         ENDIF
         R_MAX_PROJECT_ID = MAX_PROJECT_ID
      RETURN
!

      ENTRY RESET_THERMAL_RETROFIT_OL


         CLASS_SALES_OL = 'BC'
      RETURN
!

      ENTRY OPEN_THERMAL_RETROFIT_FILE(CLASS_SALES_UNIT)

         SAVE_CLASS_SALES_UNIT = CLASS_SALES_UNIT
         INQUIRE(UNIT=CLASS_SALES_UNIT,OPENED=FILE_OPEN)
         IF(FILE_OPEN) CLOSE(CLASS_SALES_UNIT)
         OPEN(CLASS_SALES_UNIT,FILE=trim(OUTPUT_DIRECTORY())// &
            CLASS_SALES_OL//"RETROFIT.BIN",ACCESS="DIRECT",RECL=LRECL)
      RETURN
!

      ENTRY CLOSE_THERMAL_RETROFIT_FILE

         CLOSE(SAVE_CLASS_SALES_UNIT)
      RETURN

 1000 FORMAT(A)

      END


!  READS SALES TABLES AND ALLOCATES TO CLASS


      RECURSIVE SUBROUTINE MANAGE_THERMAL_RETROFIT
      use end_routine, only: end_program, er_message
      use mod_base_year
      use spindriftlib
      use prod_arrays_dimensions
      USE SIZECOM
      use globecom
      SAVE

!
      LOGICAL (KIND=4) :: VECTOR_TYPE_ESCAL,RETROFIT_INFO_ACTIVE, &
                          SET_RETROFIT_LOGIC_OFF,R_TEMP_L1
      REAL (KIND=4) :: ESCALATED_VECTOR_OR_RETURN_VAL
      INTEGER (KIND=2) :: Retro_Project_First_Year_Avail, &
                          Retro_Project_Last_Year_Avail, &
                          R_Retro_Project_Frst_Year_Avail, &
                          R_Retro_Project_Last_Year_Avail
      LOGICAL (KIND=1) :: PROJECT_ACTIVE(:)
      INTEGER (KIND=4) :: VALUES_TO_ZERO
      INTEGER (KIND=2) :: MO,I,IREC,R_YEAR, &
                          R_ST_TG,DELETE, &
                          SALES_UNIT=10,LOCAL_YEAR, &
                          R_RETROFIT_ID, &
                          MAX_PROJECT_ID
      INTEGER (KIND=4) :: IOS,EV_ID
      REAL (KIND=4) ::  R_CO2, &
                        R_MW, &
                        R_MWH, &
                        R_GROSS_MARGIN, &
                        R_COST_PER_TON, &
                        TOTAL_CO2_COST, &
                        R_TOTAL_CO2_REDUCTION
!
      INTEGER (KIND=2) :: RUN_YEAR,Project
! ADDITIONAL SECTION
      INTEGER (KIND=2) :: DATA_POS,J
!
      CHARACTER (LEN=6) :: UNIT_ID
      CHARACTER (len=256) :: FILE_NAME
      CHARACTER (len=256) :: BASE_FILE_DIRECTORY
      CHARACTER (LEN=5000) :: RECLN,DUMMY_CHR*20
!
      INTEGER (KIND=4) :: NEXT_REC=0
      INTEGER (KIND=2) :: RC_UNIT_NO=0
      INTEGER (KIND=2) :: RECORD_NUM, &
                          SAVE_RECORD_NUM=0, &
                          RETROFIT_POSITION(:), &
                          R_MONTH
! DATA FOR THERMAL RETROFIT VARIABLES
      CHARACTER (len=30) :: Project_Name(:) ! C*30
      CHARACTER (LEN=1) :: Project_Active_CHR ! C*1
      CHARACTER (LEN=12) :: Project_Type(:) ! C*12
      INTEGER (KIND=2) :: Project_ID(:), &     ! I*2
                          First_Year_Available(:), &  ! I*2
                          Last_Year_Available(:), &  ! I*2
                          Capital_Cost_Esc(:), &  ! I*2
                          Var_OM_Esc(:), &  ! I*2
                          Fixed_OM_Esc(:) ! I*2
      REAL (KIND=4) :: &
                   Learning_Curve(:), &        ! R*4
                   Capital_Cost_CCS(:), &      ! R*4
                   Capital_Cost_Trans(:), &    ! R*4
                   CRR_On_Project(:), &        ! R*4
                   Var_OM_Adder(:), &          ! R*4
                   Var_Storage_OM_Adder(:), &  ! R*4
                   Fixed_OM_Capture_Adder(:), & ! R*4
                   Fixed_OM_Trans(:), &        ! R*4
                   SO2_Control_Percent(:), &   ! R*4
                   NOx_Control_Percent(:), &   ! R*4
                   CO2_Control_Percent(:), &   ! R*4
                   HG_Control_Percent(:), &    ! R*4
                   Other_Control_Percent(:), & ! R*4
                   Replacement_Fuel(:), &      ! R*4
                   Ave_Heat_Rate_Mult(:), &    ! R*4
                   Forced_Outage_Adder(:), &   ! R*4
                   Dispatch_Adder(:)           ! R*4
      REAL (KIND=4) :: &
                   Min_Capacity_Change(:), &   ! R*4
                   Max_Capacity_Change(:), &   ! R*4
                   R_Min_Capacity_Change, &
                   R_Max_Capacity_Change, &
                   R_Ave_Heat_Rate_Mult, &
                   R_CO2_Control_Percent, &
                   R_VOM_Adder, &
                   R_FOM_Adder, &
                   ESCALATED_MONTHLY_VALUE, &
                   VOM_ESC, &
                   FOM_ESC
      ALLOCATABLE ::             Project_Name,Project_Active, &
                                 Project_Type,Project_ID, &
                                 First_Year_Available,Last_Year_Available, &
                                 Learning_Curve,Capital_Cost_CCS, &
                                 Capital_Cost_Trans,Capital_Cost_Esc, &
                                 CRR_On_Project,Var_OM_Adder, &
                                 Var_Storage_OM_Adder,Var_OM_Esc, &
                                 Fixed_OM_Capture_Adder,Fixed_OM_Trans, &
                                 Fixed_OM_Esc,SO2_Control_Percent, &
                                 NOx_Control_Percent,CO2_Control_Percent, &
                                 HG_Control_Percent, &    ! R*4
                                 Other_Control_Percent, & ! R*4
                                 Replacement_Fuel, &      ! R*4
                                 Ave_Heat_Rate_Mult, &    ! R*4
                                 Forced_Outage_Adder, &   ! R*4
                                 Dispatch_Adder, &        ! R*4
                                 Min_Capacity_Change, &   ! R*4
                                 Max_Capacity_Change, &   ! R*4
                                 RETROFIT_POSITION
! END RETROFIT DATA
!
! END DATA DECLARATIONS
!
         Retro_Project_First_Year_Avail = 3100
         Retro_Project_Last_Year_Avail = 0
         CALL RETURN_RETROFIT_RECORD_NUM(RECORD_NUM,MAX_PROJECT_ID)
         SAVE_RECORD_NUM = RECORD_NUM

         IF(RECORD_NUM > 0) THEN
            IF(ALLOCATED(Project_Active)) &
               DEALLOCATE(Project_Name,Project_Active, &
               Project_Type,Project_ID, &
                                 First_Year_Available,Last_Year_Available,Learning_Curve,Capital_Cost_CCS, & ! I*2 - I*2 - R*4 - R*4
                                 Capital_Cost_Trans,Capital_Cost_Esc, & 
                                 CRR_On_Project,Var_OM_Adder, & 
                                 Var_Storage_OM_Adder,Var_OM_Esc, & 
                                 Fixed_OM_Capture_Adder,Fixed_OM_Trans, & 
                                 Fixed_OM_Esc, &                    ! I*2
                                 SO2_Control_Percent, &             ! R*4
                                 NOx_Control_Percent, &             ! R*4
                                 CO2_Control_Percent, &             ! R*4
                                 HG_Control_Percent, &              ! R*4
                                 Other_Control_Percent, &           ! R*4
                                 Replacement_Fuel, &                ! R*4
                                 Ave_Heat_Rate_Mult, &              ! R*4
                                 Forced_Outage_Adder, &             ! R*4
                                 Dispatch_Adder, &                  ! R*4
                                 Min_Capacity_Change, &             ! R*4
                                 Max_Capacity_Change,RETROFIT_POSITION) ! R*4
            ALLOCATE( &
                                 Project_Name(RECORD_NUM), &
                                 Project_Active(RECORD_NUM), &
              Project_Type(RECORD_NUM),Project_ID(RECORD_NUM), &
              First_Year_Available(RECORD_NUM),Last_Year_Available(RECORD_NUM), &
              Learning_Curve(RECORD_NUM),Capital_Cost_CCS(RECORD_NUM), & 
              Capital_Cost_Trans(RECORD_NUM),Capital_Cost_Esc(RECORD_NUM), &
                                 CRR_On_Project(RECORD_NUM), &
                                    Var_OM_Adder(RECORD_NUM), &  
                                    Var_Storage_OM_Adder(RECORD_NUM), &
                                    Var_OM_Esc(RECORD_NUM), &
                                    Fixed_OM_Capture_Adder(RECORD_NUM), &
                                    Fixed_OM_Trans(RECORD_NUM), &
                                 Fixed_OM_Esc(RECORD_NUM),SO2_Control_Percent(RECORD_NUM), &
                                 NOx_Control_Percent(RECORD_NUM), &
                                 CO2_Control_Percent(RECORD_NUM), &
                                 HG_Control_Percent(RECORD_NUM), &    ! R*4
                                 Other_Control_Percent(RECORD_NUM), & ! R*4
                                 Replacement_Fuel(RECORD_NUM), &      ! R*4
                                 Ave_Heat_Rate_Mult(RECORD_NUM), &    ! R*4
                                 Forced_Outage_Adder(RECORD_NUM), &   ! R*4
                                 Dispatch_Adder(RECORD_NUM), &        ! R*4
                                 Min_Capacity_Change(RECORD_NUM), &   ! R*4
                                 Max_Capacity_Change(RECORD_NUM), &
                                 RETROFIT_POSITION(MAX_PROJECT_ID) )  ! R*4
            PROJECT_ACTIVE = .FALSE.
            RETROFIT_POSITION = 0
         ELSE
            RETURN
         ENDIF

! READ RETROFIT FILE

         CALL OPEN_THERMAL_RETROFIT_FILE(SALES_UNIT)

         IREC = 0

         DO IREC = 1, SAVE_RECORD_NUM

            READ(SALES_UNIT,REC=IREC,IOSTAT=IOS) &
              DELETE,Project_Name(IREC),Project_Active_CHR, &
              Project_Type(IREC),Project_ID(IREC), & 
              First_Year_Available(IREC),Last_Year_Available(IREC), & 
              Learning_Curve(IREC),Capital_Cost_CCS(IREC), & 
              Capital_Cost_Trans(IREC),Capital_Cost_Esc(IREC), & 
              CRR_On_Project(IREC),Var_OM_Adder(IREC), &
              Var_Storage_OM_Adder(IREC),Var_OM_Esc(IREC), & 
              Fixed_OM_Capture_Adder(IREC),Fixed_OM_Trans(IREC), &
              Fixed_OM_Esc(IREC),SO2_Control_Percent(IREC), & 
              NOx_Control_Percent(IREC),CO2_Control_Percent(IREC), & 
              HG_Control_Percent(IREC), &       
              Other_Control_Percent(IREC), &    
              Replacement_Fuel(IREC), &         
              Ave_Heat_Rate_Mult(IREC), &       
              Forced_Outage_Adder(IREC), &      
              Dispatch_Adder(IREC), &           
              Min_Capacity_Change(IREC), &      
              Max_Capacity_Change(IREC), &      
              UNIT_ID,EV_ID
            IF(IOS /= 0) EXIT
            IF(DELETE >= 8) CYCLE
            IF(Project_Active_CHR /= 'F') THEN
               Project_Active(IREC) = .TRUE.
               Retro_Project_First_Year_Avail = &
                min(Retro_Project_First_Year_Avail, &
                
               First_Year_Available(IREC))
                Retro_Project_Last_Year_Avail = &
                max(Retro_Project_Last_Year_Avail, &
                Last_Year_Available(IREC))
            ENDIF
            IF(Project_ID(IREC) > 0) THEN
               RETROFIT_POSITION(Project_ID(IREC)) = IREC
!               MAX_PROJECT_ID = MAX(Project_ID(IREC),MAX_PROJECT_ID)
            ENDIF
         ENDDO ! PROJECTS
         CALL CLOSE_THERMAL_RETROFIT_FILE
! END RETROFIT
      RETURN

      ENTRY GET_RETRO_AVAIL_PERIOD(R_Retro_Project_Frst_Year_Avail, &
                                   R_Retro_Project_Last_Year_Avail)
         R_Retro_Project_Frst_Year_Avail=Retro_Project_First_Year_Avail
         R_Retro_Project_Last_Year_Avail = Retro_Project_Last_Year_Avail
      RETURN

      ENTRY  RETRO_PROJECT_AVAILABLE(R_RETROFIT_ID,R_YEAR, &
                                    R_TEMP_L1)
         R_TEMP_L1 = .FALSE.
         IF(ALLOCATED(Project_Active) .AND. R_RETROFIT_ID > 0 .AND. &
                                   R_RETROFIT_ID <= MAX_PROJECT_ID) THEN
            Project = RETROFIT_POSITION(R_RETROFIT_ID)
            IF(Project > 0 .AND. Project <= SAVE_RECORD_NUM) THEN
               LOCAL_YEAR = R_YEAR + get_BASE_YEAR()
               R_TEMP_L1 = First_Year_Available(Project) <= LOCAL_YEAR &
                        .AND. Last_Year_Available(Project) >= LOCAL_YEAR
            ENDIF
         ENDIF
      RETURN

      ENTRY ANNUAL_RETROFIT_PROJECT(R_RETROFIT_ID, &
                                    R_YEAR, &
                                    R_CO2, &
                                    R_MW, &
                                    R_MWH, &
                                    R_GROSS_MARGIN, &
                                    R_COST_PER_TON, &
                                    R_TOTAL_CO2_REDUCTION)

         R_COST_PER_TON = 999999999.0
         IF(ALLOCATED(Project_Active) .AND. R_RETROFIT_ID > 0 .AND. &
                                   R_RETROFIT_ID <= MAX_PROJECT_ID) THEN
            Project = RETROFIT_POSITION(R_RETROFIT_ID)
            IF(Project > 0 .AND. Project <= SAVE_RECORD_NUM) THEN
               LOCAL_YEAR = R_YEAR + get_BASE_YEAR()
               IF(First_Year_Available(Project) <= LOCAL_YEAR .AND. &
                        Last_Year_Available(Project) >= LOCAL_YEAR) THEN
                  TOTAL_CO2_COST = Var_OM_Adder(Project) * R_MWH + &
                         Fixed_OM_Capture_Adder(Project) * R_MW * 1000.0
                  R_TOTAL_CO2_REDUCTION = R_CO2 * &
                                  CO2_Control_Percent(Project) * 0.01
! 020510. ADDED.
                  IF(R_TOTAL_CO2_REDUCTION > 0.000001) THEN
                     R_COST_PER_TON = TOTAL_CO2_COST / R_TOTAL_CO2_REDUCTION
                  ELSE
                     R_COST_PER_TON = 999999.00
                  ENDIF
               ENDIF
            ELSE
               WRITE(4,*) 'ATTEMPT TO USE A NON-EXISTENT RETROFIT'
               WRITE(4,*) 'PROJECT WITH ID = ',R_RETROFIT_ID
               WRITE(4,*) 'RETROFIT POSITION = ',Project
               WRITE(4,*) 'SAVE_RECORD_NUM = ',SAVE_RECORD_NUM
               WRITE(4,*) 'MAX_PROJECT_ID = ',MAX_PROJECT_ID
               er_message='stop requested from Msgmmrev SIID235'
               call end_program(er_message)
            ENDIF
         ENDIF
      RETURN

      ENTRY RETURN_RETROFIT_PROJECT_IMPACTS(R_RETROFIT_ID, &
                                            R_Min_Capacity_Change, &
                                            R_Max_Capacity_Change, &
                                            R_Ave_Heat_Rate_Mult, &
                                            R_CO2_Control_Percent)

         IF(ALLOCATED(Project_Active) .AND. R_RETROFIT_ID > 0 .AND. &
                R_RETROFIT_ID <= MAX_PROJECT_ID) THEN           
            R_Min_Capacity_Change = Min_Capacity_Change(R_RETROFIT_ID)
            R_Max_Capacity_Change = Max_Capacity_Change(R_RETROFIT_ID)
            R_Ave_Heat_Rate_Mult = Ave_Heat_Rate_Mult(R_RETROFIT_ID)
            R_CO2_Control_Percent = CO2_Control_Percent(R_RETROFIT_ID)
         ELSE
            R_Min_Capacity_Change = 0.0
            R_Max_Capacity_Change = 0.0
            R_Ave_Heat_Rate_Mult = 0.0
            R_CO2_Control_Percent = 0.0
         ENDIF
      RETURN

      ENTRY RETROFIT_VOM_ADDER(R_RETROFIT_ID,R_VOM_Adder,R_YEAR,R_MONTH)

         IF(ALLOCATED(Project_Active) .AND. R_RETROFIT_ID > 0 .AND. &
             R_RETROFIT_ID <= MAX_PROJECT_ID) THEN
            VOM_ESC = 1.0
            IF(Var_OM_Esc(R_RETROFIT_ID) > 0) THEN
               VOM_ESC = &
                    ESCALATED_VECTOR_OR_RETURN_VAL(VECTOR_TYPE_ESCAL, &
                                             Var_OM_Esc(R_RETROFIT_ID), &
                                             R_YEAR, &
                                             R_MONTH, &
                                             1_2)
            ENDIF
            IF(VECTOR_TYPE_ESCAL) THEN
               R_VOM_Adder = Var_OM_Adder(R_RETROFIT_ID) * VOM_ESC
            ELSE
               R_VOM_Adder = VOM_ESC
            ENDIF
         ELSE
            R_VOM_Adder = 0.0
         ENDIF
      RETURN

      ENTRY RETROFIT_FOM_ADDER(R_RETROFIT_ID,R_FOM_Adder,R_YEAR,R_MONTH)

         IF(ALLOCATED(Project_Active) .AND. R_RETROFIT_ID > 0 .AND. &
                                   R_RETROFIT_ID <= MAX_PROJECT_ID) THEN
            FOM_ESC = 1.0
            IF(Fixed_OM_Esc(R_RETROFIT_ID) > 0) THEN
               FOM_ESC = &
                    ESCALATED_VECTOR_OR_RETURN_VAL(VECTOR_TYPE_ESCAL, &
                                            Fixed_OM_Esc(R_RETROFIT_ID), &
                                            R_YEAR, &
                                            R_MONTH, &
                                            1_2)
            ENDIF
            IF(VECTOR_TYPE_ESCAL) THEN
               R_FOM_Adder = Fixed_OM_Capture_Adder(R_RETROFIT_ID) * FOM_ESC
            ELSE
               R_FOM_Adder = FOM_ESC
            ENDIF
         ELSE
            R_FOM_Adder = 0.0
         ENDIF
      RETURN

      ENTRY RETROFIT_HEATRATE_IMPACT(R_RETROFIT_ID,R_Ave_Heat_Rate_Mult)

         IF(ALLOCATED(Project_Active) .AND. R_RETROFIT_ID > 0 .AND. &
                                   R_RETROFIT_ID <= MAX_PROJECT_ID) THEN
            R_Ave_Heat_Rate_Mult = Ave_Heat_Rate_Mult(R_RETROFIT_ID)
         ELSE
            R_Ave_Heat_Rate_Mult = 0.0
         ENDIF
      RETURN

      ENTRY RETROFIT_MINCAP_IMPACT(R_RETROFIT_ID,R_Min_Capacity_Change)

         IF(ALLOCATED(Project_Active) .AND. R_RETROFIT_ID > 0 .AND. &
                                   R_RETROFIT_ID <= MAX_PROJECT_ID) THEN
            R_Min_Capacity_Change = Min_Capacity_Change(R_RETROFIT_ID)
         ELSE
            R_Min_Capacity_Change = 0.0
         ENDIF
      RETURN

      ENTRY RETROFIT_MAXCAP_IMPACT(R_RETROFIT_ID,R_Max_Capacity_Change)

         IF(ALLOCATED(Project_Active) .AND. R_RETROFIT_ID > 0 .AND. &
                                   R_RETROFIT_ID <= MAX_PROJECT_ID) THEN
            R_Max_Capacity_Change = Max_Capacity_Change(R_RETROFIT_ID)
         ELSE
            R_Max_Capacity_Change = 0.0
         ENDIF
      RETURN

      ENTRY RETROFIT_CO2_IMPACT(R_RETROFIT_ID,R_CO2_Control_Percent)

         IF(ALLOCATED(Project_Active) .AND. R_RETROFIT_ID > 0 .AND. &
                                   R_RETROFIT_ID <= MAX_PROJECT_ID) THEN
            R_CO2_Control_Percent = CO2_Control_Percent(R_RETROFIT_ID)
         ELSE
            R_CO2_Control_Percent = 0.0
         ENDIF
      RETURN

      ENTRY RETROFIT_VOM_FOM_ADDERS(R_RETROFIT_ID,R_VOM_Adder, &
                                                            R_FOM_Adder)

         R_VOM_Adder = 0.
         R_FOM_Adder = 0.
         IF(ALLOCATED(Project_Active) .AND. R_RETROFIT_ID > 0 .AND. &
                                   R_RETROFIT_ID <= MAX_PROJECT_ID) THEN
            R_VOM_Adder = Var_OM_Adder(R_RETROFIT_ID)
            R_FOM_Adder = Fixed_OM_Capture_Adder(R_RETROFIT_ID)
         ENDIF
      RETURN
      END

!
!                  ROUTINE TO CONVERT TRAN_EXPS FILE
!
!                          COPYRIGHT (C) 2009
!                                 VENTYX
!                          ALL RIGHTS RESERVED
!

!
! 070906. CREATED.
! 070906. MAY NEED TO MAKE THIS A MULTI-FILE SPECIFICATION.
!
      SUBROUTINE TRAN_EXP_OBJECT
      use end_routine, only: end_program, er_message
      use filename_tracker
!
      use spindriftlib
      use prod_arrays_dimensions
      USE SIZECOM

      INTEGER (KIND=2) :: UNIT_NUM=10
      INTEGER (KIND=2) :: INUNIT,IREC,LRECL=328 ! ADDED 20 VARIABLES ON 3/18/07.
!      INTEGER IOS
!
      INTEGER (KIND=2) ::   SAVE_NUMBER_OF_TRAN_EXPS=0, &
                  R_NUMBER_OF_TRAN_EXPS,R_WHEELS, &
                  TRAN_EXP_OWNER
      INTEGER (KIND=2) ::   TRANS_LINE_INDEX
      CHARACTER (len=5) :: BASE_FILE_NAME,OVERLAY_FAMILY_NAME
      CHARACTER (LEN=256) :: TRANS_EXP_FILE
      CHARACTER (len=256) :: FILE_NAME
      CHARACTER (LEN=255) :: SAVE_BASE_FILE_NAME,R_TEMP_NAME
      CHARACTER (len=256) :: BASE_FILE_DIRECTORY
      CHARACTER (len=256) :: DATA_DRIVE,OUTPUT_DIRECTORY
      CHARACTER (LEN=1) ::  TRAN_EXP_ACTIVE
      LOGICAL (KIND=4) :: FILE_EXISTS=.FALSE., &
                     TRAN_EXP_FILE_EXISTS=.FALSE.,R_TRAN_EXP_FILE_EXISTS
! DECLARATION FOR DBREAD COMMON BLOCK
!      CHARACTER*1024 RECLN
! DECLARATION FOR TRANSACT TRAN_EXP DETERMINANTS
      CHARACTER (LEN=16) :: FILE_TYPE='TRANSMISSION EXPAND  '
      CHARACTER (LEN=2) ::  TRAN_EXP_OL='BC'
      LOGICAL (KIND=1) :: LAHEY_LF95
      CHARACTER (len=30) :: SCREEN_OUTPUT

! 070906.
      INTEGER (KIND=2) :: DELETE,YR,ZONE,POINTR, &
                          PRC_MULTI_ZONE_TIE_LIMIT_FILES,I, &
                          TieID
      INTEGER (KIND=4) :: IOS
      CHARACTER (LEN=4096) RECLN
      REAL (KIND=4) :: ExpansionCost, &
                       ExpansionCostEscalation, &
                       MaximumExpansion, &
                       ExpansionCCRF, &
                       PipelineID, &
                       PipelineCapacity, &
                       PipelineDistance, &
                       CONGESTION_COST_ESC, &
                       OUTAGE_RATES, &
                       LINE_LOSSES_A_TO_B, &
                       LINE_LOSSES_B_TO_A, &
                       ZONE_A_TO_B_PEAK_TIES, &
                       ZONE__A_TO_B_OFF_PEAK_TIES, &
                       ZONE__A_TO_B_WHEELING_RATES, &
                       ZONE_B_TO_A_PEAK_TIES, &
                       ZONE__B_TO_A_OFF_PEAK_TIES, &
                       ZONE__B_TO_A_WHEELING_RATES
      REAL (KIND=4) :: &
                       ESCALATION_RATE, &
                       ZONE_A_TO_B_PLANNING_FACTOR, &
                       ZONE_B_TO_A_PLANNING_FACTOR, &
                       CONGESTION_PERCENT(10), &
                       CONGESTION_COST(10)
      INTEGER (KIND=2) :: ZONE_A_GROUP_ID, &
                          ZONE_B_GROUP_ID, &
                          FIND_TRANS_GROUP_ID
      CHARACTER (LEN=31) :: ZONE_A,ZONE_B, &
                            COMMENT, &
                            ZONE_A_FULL_NAME, &
                            ZONE_B_FULL_NAME
!
      CHARACTER (LEN=3) :: ActiveConstraint
      CHARACTER (LEN=50) :: TieDescription
      SAVE SAVE_BASE_FILE_NAME
! CONVERT THE TRAN_EXP FILE
!
!
!

      ENTRY TRAN_EXP_MAKEBIN

      BASE_FILE_NAME = TRANS_EXP_FILE ()
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME = get_atb_filename(base_file_name)
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      TRAN_EXP_FILE_EXISTS = FILE_EXISTS
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
         OPEN(11,FILE=trim(DATA_DRIVE)// &
         "BCTRAN_EXP.BIN",ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         IREC = 1

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

            READ(RECLN,*,ERR=200) DELETE, &
            TieDescription,ActiveConstraint,ExpansionCost, &
            ExpansionCostEscalation, &
            LINE_LOSSES_A_TO_B,OUTAGE_RATES,ZONE_A,ZONE_A_TO_B_PEAK_TIES, &
            ZONE__A_TO_B_OFF_PEAK_TIES,ZONE__A_TO_B_WHEELING_RATES, &
                                  ZONE_B,ZONE_B_TO_A_PEAK_TIES,ZONE__B_TO_A_OFF_PEAK_TIES,ZONE__B_TO_A_WHEELING_RATES, &
                                  TieID,COMMENT, &
                                  ZONE_A_GROUP_ID, &
                                  ZONE_B_GROUP_ID, &
                                  ZONE_A_TO_B_PLANNING_FACTOR, &
                                  ZONE_B_TO_A_PLANNING_FACTOR, &
                                  LINE_LOSSES_B_TO_A, &
                                  CONGESTION_PERCENT, &
                                  CONGESTION_COST, &
                                  PipelineDistance, &
                                  MaximumExpansion,&
                                  ExpansionCCRF, &
                                  PipelineID, &
                                  PipelineCapacity, &
                                  CONGESTION_COST_ESC
            WRITE(11,REC=IREC) DELETE, &
            TieDescription,ActiveConstraint,ExpansionCost, &
            ExpansionCostEscalation, &
                               LINE_LOSSES_A_TO_B,OUTAGE_RATES, &
             ZONE_A,ZONE_A_TO_B_PEAK_TIES,ZONE__A_TO_B_OFF_PEAK_TIES, &
             ZONE__A_TO_B_WHEELING_RATES, &
             ZONE_B,ZONE_B_TO_A_PEAK_TIES,ZONE__B_TO_A_OFF_PEAK_TIES, &
             ZONE__B_TO_A_WHEELING_RATES, &
                               TieID, &
                               COMMENT, &
                               ZONE_A_GROUP_ID, &
                               ZONE_B_GROUP_ID, &
                               ZONE_A_TO_B_PLANNING_FACTOR, &
                               ZONE_B_TO_A_PLANNING_FACTOR, &
                               LINE_LOSSES_B_TO_A, &
                               CONGESTION_PERCENT, &
                               CONGESTION_COST, &
                               PipelineDistance, &
                               MaximumExpansion, &
                               ExpansionCCRF, &
                               PipelineID, &
                               PipelineCapacity, &
                               CONGESTION_COST_ESC
            IREC = IREC + 1
         ENDDO
         CLOSE(10)
         CLOSE(11)
      ELSE IF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "msgmmrev:1114")
      ENDIF
!
      SAVE_NUMBER_OF_TRAN_EXPS = MAX(0,IREC-1)
!
      RETURN



! OVERLAY THE TRAN_EXP FILE

      ENTRY TRAN_EXP_MAKEOVL(OVERLAY_FAMILY_NAME)


      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME = get_ato_filename(DATA_DRIVE, overlay_family_name)
      OPEN(10,FILE=FILE_NAME)
      READ(10,*) DELETE
      INUNIT = 12
      IF(TRAN_EXP_OL == 'BC') THEN
         OPEN(11,FILE=trim(DATA_DRIVE)// &
         "BCTRAN_EXP.BIN",ACCESS="DIRECT",RECL=LRECL)
         INUNIT = 11
      ENDIF
      OPEN(12,FILE=trim(DATA_DRIVE)// &
      "OLTRAN_EXP.BIN",ACCESS="DIRECT",  STATUS="UNKNOWN",RECL=LRECL)
      IREC = 0
!
      READ(10,1000,IOSTAT=IOS) RECLN
      DO WHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE
!
         READ(10,1000,IOSTAT=IOS) RECLN
!
      END DO
!
      DO
!
         IREC = IREC + 1
         READ(INUNIT,REC=IREC,IOSTAT=IOS) DELETE, &
         TieDescription,ActiveConstraint,ExpansionCost, &
             ExpansionCostEscalation, &
             LINE_LOSSES_A_TO_B,OUTAGE_RATES, &
             ZONE_A,ZONE_A_TO_B_PEAK_TIES,ZONE__A_TO_B_OFF_PEAK_TIES, &
             ZONE__A_TO_B_WHEELING_RATES,ZONE_B,ZONE_B_TO_A_PEAK_TIES, &
             ZONE__B_TO_A_OFF_PEAK_TIES,ZONE__B_TO_A_WHEELING_RATES, &
             TieID, &
             COMMENT, &
             ZONE_A_GROUP_ID, &
             ZONE_B_GROUP_ID, &
             ZONE_A_TO_B_PLANNING_FACTOR, &
             ZONE_B_TO_A_PLANNING_FACTOR, &
             LINE_LOSSES_B_TO_A, &
             CONGESTION_PERCENT, &
             CONGESTION_COST, &
             PipelineDistance, &
             MaximumExpansion, &
             ExpansionCCRF, &
             PipelineID, &
             PipelineCapacity, &
             CONGESTION_COST_ESC
         IF(IOS /= 0) EXIT
!
         RECLN = trim(RECLN)//',,,,,,,,,,,,,'
         READ(RECLN,*,ERR=300) DELETE, &
         TieDescription,ActiveConstraint,ExpansionCost,ExpansionCostEscalation, &
             LINE_LOSSES_A_TO_B,OUTAGE_RATES, &
             ZONE_A,ZONE_A_TO_B_PEAK_TIES, &
             ZONE__A_TO_B_OFF_PEAK_TIES,ZONE__A_TO_B_WHEELING_RATES, &
             ZONE_B,ZONE_B_TO_A_PEAK_TIES,ZONE__B_TO_A_OFF_PEAK_TIES, &
             ZONE__B_TO_A_WHEELING_RATES, &
             TieID, &
             COMMENT, &
             ZONE_A_GROUP_ID, &
             ZONE_B_GROUP_ID, &
             ZONE_A_TO_B_PLANNING_FACTOR, &
             ZONE_B_TO_A_PLANNING_FACTOR, &
             LINE_LOSSES_B_TO_A, &
             CONGESTION_PERCENT, &
             CONGESTION_COST, &
             PipelineDistance, &
             MaximumExpansion, &
             ExpansionCCRF, &
             PipelineID, &
             PipelineCapacity, &
             CONGESTION_COST_ESC
!         ENDIF
!
         READ(10,1000,IOSTAT=IOS) RECLN
         DO WHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE
!
            READ(10,1000,IOSTAT=IOS) RECLN
!
         ENDDO
         WRITE(12,REC=IREC) DELETE,TieDescription,ActiveConstraint,ExpansionCost,ExpansionCostEscalation, &
                                     LINE_LOSSES_A_TO_B,OUTAGE_RATES, &
             ZONE_A,ZONE_A_TO_B_PEAK_TIES,ZONE__A_TO_B_OFF_PEAK_TIES, &
                ZONE__A_TO_B_WHEELING_RATES, &
                                     ZONE_B,ZONE_B_TO_A_PEAK_TIES,ZONE__B_TO_A_OFF_PEAK_TIES,ZONE__B_TO_A_WHEELING_RATES, &
                                     TieID, &
                                     COMMENT, &
                                     ZONE_A_GROUP_ID, &
                                     ZONE_B_GROUP_ID, &
                                     ZONE_A_TO_B_PLANNING_FACTOR, &
                                     ZONE_B_TO_A_PLANNING_FACTOR, &
                                     LINE_LOSSES_B_TO_A, &
                                     CONGESTION_PERCENT, &
                                     CONGESTION_COST, &
                                     PipelineDistance, &
                                     MaximumExpansion, &
                                     ExpansionCCRF, &
                                     PipelineID, &
                                     PipelineCapacity, &
                                     CONGESTION_COST_ESC
!
      ENDDO
      CLOSE(10)
      CLOSE(12)
      IF(TRAN_EXP_OL == 'BC') CLOSE(11)
      TRAN_EXP_OL = 'OL'
      RETURN

  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from Msgmmrev SIID236'
      call end_program(er_message)

  300 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      CALL MG_LOCATE_WRITE(21,0, &
        'Error reading the Trans Exp record. Look for'// &
        ' a "," in a character name.', &
                           ALL_VERSIONS,1)
      er_message='stop requested from Msgmmrev SIID237'
      call end_program(er_message)
!

      ENTRY RESET_TRAN_EXP_OL

         TRAN_EXP_OL = 'BC'
      RETURN
!

      ENTRY OPEN_TRAN_EXP_FILE

         OPEN(UNIT_NUM,FILE=trim(OUTPUT_DIRECTORY())//TRAN_EXP_OL// &
            "TRAN_EXP.BIN",ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      RETURN
!

      ENTRY CLOSE_TRAN_EXP_FILE

         CLOSE(UNIT_NUM)
      RETURN

      ENTRY DOES_TRAN_EXP_FILE_EXIST(R_TRAN_EXP_FILE_EXISTS)

         R_TRAN_EXP_FILE_EXISTS = TRAN_EXP_FILE_EXISTS
      RETURN

      ENTRY GET_MAX_TRAN_EXPS(R_NUMBER_OF_TRAN_EXPS)

         R_NUMBER_OF_TRAN_EXPS = SAVE_NUMBER_OF_TRAN_EXPS
      RETURN

 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END

!
!
!     MAINTAINS LIST OF FROM/TO TRAN_EXPS AS WELL AS MW LOADING ON TRAN_EXPS
!     AND TIES
!
!
      RECURSIVE FUNCTION MANAGE_TRAN_EXP_FORECASTS()
!
!


      use spindriftlib
      use prod_arrays_dimensions
      USE SIZECOM

      SAVE
      LOGICAL (KIND=1) :: SAVE_TRAN_EXP_FILE_EXISTS=.FALSE., &
                  REDUCE_TRAN_EXP_CAPACITY, &
                  HOUR_TRAN_EXP_ACTIVE, &
                  THIS_IS_A_LONG_TRAN_EXP, &
                  SET_HOUR_LONG_TRAN_EXP_PARAMS, &
                  MULTI_AREA_PRICING_FOR_MONTH, &
                  MULTI_AREA_BUY_PRICE_FOR_MONTH, &
                  GET_SCENARIO_PRICE_NUM, &
                  TEMP_L, &
                  ANNUAL_TRAN_EXP_FORECAST, &
                  MONTHLY_TRAN_EXP_INIT, &
                  MONTHLY_TRAN_EXP_FORECAST
      INTEGER (KIND=4) ::   VALUES_2_ZERO
      INTEGER (KIND=2) ::   I,J,K,CURRENT_RECORD,TRANS_GROUP, &
                  TEMP_I2,Z, &
                  R_NUM_OF_PRICING_GROUPS, &
                  GET_TRAN_EXP,TRAN_EXP_RECORDS, &
                  R_TRANS_GROUP, &
                  SAVE_TRAN_EXP_RECORDS=0, &
                  MAX_WHEELS=0,MAX_TRAN_EXPS, &
                  MAX_IN_TRAN_EXPS, &
                  MAX_OUT_TRAN_EXPS, &
                  GET_MAX_IN_TRAN_EXPS, &
                  LOCAL_MAX_WHEELS, &
                  ACTIVE_LINK=0, &
                  GET_ACTIVE_LINK_NUMBER, &
                  GET_TRAN_EXP_ID, &
                  LOCAL_TRAN_EXP_NUMBER, &
                  BEG_FO_HR_TL, &
                  END_FO_HR_TL
      INTEGER (KIND=2) :: &
                  CURRENT_SELLER_TRAN_EXP, &
                  CURRENT_BUYER_TRAN_EXP, &
                  R_LP, &
                  L_LP, &
                  L1,L2, &
                  SAVE_MAX_TRAN_EXPS=0, &
                  R_MONTH,R_YEAR
      INTEGER (KIND=2) :: R_SELLER,R_BUYER
      INTEGER (KIND=2) :: GET_HOUR_TRAN_EXPS_INDEX, &
                  GET_HR_TRAN_EXP_4_LONG_TRAN_EXP, &
                  GET_HOUR_WHEEL_TRAN_EXP, &
                  R_TRAN_EXP, &
                  R_DEPTH, &
                  RECURSIVE_COUNT
!
      integer, allocatable ::  J_PATH(:), & ! BIG ARRAY, VALUE IS ALSO BIG.
                  PATH(:), & ! INDEX IS VERY BIG. VALUE OF PATH IS SMALL
                  PARENT(:), & ! VERY BIG ARRAY, VALUE IS ALSO BIG.
                  NODE(:), &
                  I_PATH(:), &
                  I_PARENT(:), & ! VERY BIG ARRAY, VALUE IS ALSO BIG.
                  NUMBER_OF_PATHS_FOR_I(:), &
                  PATH_FOR_J_TO_K(:,:)  ! HIGHLY INDEXED ARRAY: COU
                  
      INTEGER :: TOTAL_PATHS_FOR_I, &
                  TOTAL_PATHS_FOR_SYSTEM, &
                  GET_TOTAL_PATHS_FOR_SYSTEM, &
                  R_PATH, NT,  &
                  GET_PATH_FOR_J_TO_K, &
                  GET_PARENT_OF_LEAF, &
                  GET_PARENT_PATH_OF_LEAF, &
                  GET_LEAF_OF_PATH, &
                  LAST_NODE_SUM, &
                  NUMBER_OF_J,NUMBER_OF_K,M,N,P,Q,LAST_Q, &
                  GET_NUMBER_OF_SUPPLY_PATHS, &
                  NUMBER_OF_DEMAND_PATHS
      INTEGER (KIND=4), allocatable :: &
                  K_PATH(:), &   ! BIG
                  SET_OF_J(:), & ! BIG
                  SET_OF_K(:)    ! BIG
      integer (kind=2), allocatable :: MAX_C_FOR_(:), &
                          MIN_C_FOR_(:)

      INTEGER (kind=2) :: L,NUM,LAST_NUM, &
                          R_A,R_B,R_C, &
                          ACTIVE_NODES, &
                          GET_NODE_OF_PATH, &
                          GET_NUMBER_OF_GAS_ACTIVE_GROUPS, &
                          GET_MAX_C_FOR_

      REAL (KIND=4) :: GET_SEASON_CONSTRAINT_LIMIT, &
                       R_CAPACITY,CURRENT_TRAN_EXP_MW, &
!
                       TEMP_CAPACITY, &
                       HOUR_TIE_LIMIT, &
                       TRAN_EXP_WHEELING_CHARGE,TRAN_EXP_SPREAD_RATE, &
                       LOCAL_WHEELING_CHARGE,LOCAL_PRICE_DELTA, &
                       LOCAL_DELTA_MULT, &
                       TRAN_EXP_PRICE_DELTA, &
                       GET_PRICING_GROUP_SELL_PRICE, &
                       GET_PRICING_GROUP_BUY_PRICE, &
                       R_PRICING_GROUP_QUANT, &
                       GET_SCENARIO_TRANSMISSION_AVAIL, &
                       SCENARIO_TRANSMISSION_AVAIL, &
                       LOCAL_WHEEL_MULT, &
                       LOCAL_SPREAD_MULT, &
                       ESCALATED_MONTHLY_VALUE, &
                       REAL4_ONE=1.
      REAL (KIND=4) :: &
                       CONSTRAINT_MULT, &
                       GET_TRANS_LINE_MULT, &
                       GET_CONSTRAINT_MULT, &
                       TEMP_R, &
                       R_CONGEST_VECTOR(10), & ! NUMBER OF CONGESTION POINTS
                       GET_GAS_TRANSPORT_RATE, &
                       GET_GAS_TRANSPORT_QUANT, &
                       GET_TRAN_EXP_QUANT, &
                       GET_GAS_TRANSPORT_LOSSES, &
                       GET_GAS_TRANS_CONGEST_COST, &
                       R_CF, &
                       R_LOCAL_QUANT, &
                       R_LINK_CAP_USED, &
                       R_LINK_CAP, &
                       AVAIL_GAS_GIVEN_LINK_CONGEST, &
                       MAX_CAP, &
                       R_CONGESTION_QUANT

      LOGICAL (KIND=1) ::   MANAGE_TRAN_EXP_FORECASTS, &
                  TRANS_GROUP_ACTIVE_SWITCH, &
                  UPDATE_TRAN_EXP_DATA, &
                  GET_GAS_TRANSPORT_CONGESTION, &
                  INIT_HOUR_TRAN_EXP_LIMIT, &
                  TRAN_EXP_DATA_READ=.TRUE., &
                  GET_TRAN_EXP_PARAMS, &
                  DETAILED_CONSTRAINTS_ACTIVE=.FALSE., &
                  ARE_DETAILED_CONSTRAINTS_ACTIVE, &
                  GAS_DEMAND_GROUP_ACTIVE, &
                 GAS_SUPPLY_GROUP_ACTIVE, &
                  REDUNDANT_PATH, &
                  TEST_TRAN_EXP_CF
      logical (kind=1), allocatable :: ACTIVE_DEMAND_NODE(:), &
                  ACTIVE_SUPPLY_NODE(:)
      LOGICAL (KIND=4) ::   TRAN_EXP_FILE_EXISTS
      CHARACTER (LEN=1) :: TRAN_EXP_ACTIVE
!
!
      LOGICAL (KIND=1) :: SET_HRLONGTRANEXP_WI_TRANEXPS, &
                SET_HOUR_LONG_CAP_TWH

      INTEGER (KIND=2) :: TRAN_EXPS_LONG_TRAN_EXPS=0,L_BUYER,L_SELLER
      REAL (KIND=4) ::    L_CAPACITY
!
      CHARACTER (LEN=2) ::   LOAD_FILE_CHAR_EXT
      CHARACTER (len=256) :: FILE_NAME,PRB_FILE_DIRECTORY
      CHARACTER (LEN=8) :: EEICODE
      INTEGER (KIND=2) :: TIMZON,TEMPER,DELTMP,DAY, &
                CURRENT_HR,DA,DAYS_IN_MONTH,LDE_YEAR, &
                MARKET_DAY_OF_WEEK,CALENDAR_DAY_OF_WEEK, &
                LDE_DAY,LDE_MONTH,DAY_WEEK,R_HOURS_IN_MONTH, &
                ALINE_LOAD_DATA,IREC,HR, &
                NUM_TRANS_GROUPS, &
                MAX_GAS_GROUP_NUMBER, &
                MAX_DEPTH=30, &
                GET_NUMBER_OF_ACTIVE_GROUPS, &
                GET_MAX_GAS_GROUP_NUMBER, &
                GET_TRANS_GROUP_INDEX, &
                NUMBER_OF_TG, &
                TRANS_ID, &
                TEMP_K, &
                TRANS_GROUP_POSITION(:), &
                iPort(:), &
                SNamePos(:), &
                GET_LINK_ALPHA_ORDER
      REAL (KIND=4) ::    LOCAL_MARKET_PRICE(24)
!
      LOGICAL (KIND=4) :: FILE_EXISTS
      INTEGER (KIND=2) :: iLine,iNode,jNode,TRAN_EXPID
      REAL (KIND=4) :: Impedance,LineVoltageLevel,LinePowerLimit
!
! 010303.
!
         INTEGER (KIND=2) :: R_END_POINT,R_DAY
         CHARACTER (LEN=9) :: R_MONTH_NAME
         LOGICAL (KIND=1) ::   HOURLY_FLOW_SUMMARY_REPORT, &
                     HOURLY_FLOW_REPORT_NOT_OPEN=.TRUE., &
                     DAILY_TRAN_EXPS_REPORTS, &
                     YES_WRITE_DAILY_TRAN_EXPS_RPTS, &
                     HOURLY_TRAN_EXPS_REPORTS, &
                     WRITE_DAILY_TRAN_EXPS_REPORTS=.FALSE., &
                     GAS_GROUP_ACTIVE_SWITCH, &
                     GET_LINK_NAME
         logical (kind=1), allocatable :: LINK_PAIR(:,:)

      integer (kind=2), allocatable :: TieID(:), &
            LINK_INDEX(:,:), &
            I_TO_K_PATHS(:,:), &
            J_TO_K_PATHS(:,:), &
            VLI(:,:), & ! VALID LINK INDEX: J,K
            TieID_INDEX(:,:)


      INTEGER (KIND=2) :: DELETE,YR,ZONE,POINTR, &
                          A_ID,B_ID, &
                          VLI_COUNT, &
                          GET_NUMBER_OF_J_TO_K_PATHS, &
                          MAX_I_TO_K_PATHS=100, &
                          MAX_J_TO_K_PATHS=500, & ! BIG: NEEDS TO BE BIGGER?
                          GET_GAS_GROUP_POSITION, &
                          GET_TieID_INDEX
      INTEGER (KIND=4) :: IOS
      CHARACTER (LEN=4096) :: RECLN
      real (kind=4), allocatable :: ExpansionCost(:), &
                       ExpansionCostEscalation(:), &
                       LINE_LOSSES_A_TO_B(:), &
                       LINE_LOSSES_B_TO_A(:), &
                       CONGESTION_PERCENT(:,:), &
                       CONGESTION_COST(:,:), &
                       MaximumExpansion(:), &
                       CONGESTION_COST_ESC(:), &
                       CONGESTION_COST_NOMINAL(:), &
                       MONTHLY_CONGEST_PERCENT(:,:), &
                       MONTHLY_CONGEST_COST(:,:), &
                       ExpansionCCRF(:), &
                       ZONE_A_TO_B_PEAK_TIES(:)
                       
      REAL (KIND=4) :: MONTHLY_DATA(12), &
                       OUTAGE_RATES, &
                       PipelineDistance, &
                       PipelineID, &
                       PipelineCapacity
                       
      real (kind=4), allocatable :: &
                       ZONE__A_TO_B_OFF_PEAK_TIES(:), &
                       ZONE__A_TO_B_WHEELING_RATES(:), &
                       ZONE__B_TO_A_OFF_PEAK_TIES(:), &
                       ZONE__B_TO_A_WHEELING_RATES(:), &
                       ZONE_B_TO_A_PEAK_TIES(:), &
                       ESCALATION_RATE(:)

      REAL (KIND=4) :: &
                       ZONE_A_TO_B_PLANNING_FACTOR, &
                       ZONE_B_TO_A_PLANNING_FACTOR, &
                       GET_TRANS_CONGEST_COST, &
                       R_UTIL
                       
      INTEGER (KIND=2), allocatable :: ZONE_A_GROUP_ID(:), &
                          ZONE_B_GROUP_ID(:)
                          
      integer (kind=2) :: FIND_TRANS_GROUP_ID, &
                          DEPTH, &
                          R_LINK_GROUP, &
                          GET_MAX_LINK_ID, &
                          MAX_LINK_ID=0
      CHARACTER (LEN=31), allocatable :: ZONE_A(:), &
                            ZONE_B(:)
                            
      character (len=31) :: COMMENT, &
                            ZONE_A_FULL_NAME, &
                            ZONE_B_FULL_NAME
!
      CHARACTER (LEN=3) :: ActiveConstraint
      CHARACTER (LEN=50), allocatable :: TieDescription(:)
      character (len=50) :: R_GET_TIE_NAME
      CHARACTER (LEN=256) :: PATH_STRING,LINK_STRING
      ALLOCATABLE :: TRANS_GROUP_POSITION, &
        iPort,SNamePos
                     
!
! END DATA DECLARATIONS
!
!
         MANAGE_TRAN_EXP_FORECASTS = .FALSE.
         SAVE_TRAN_EXP_FILE_EXISTS = .FALSE.
!
!
         CALL DOES_TRAN_EXP_FILE_EXIST(TRAN_EXP_FILE_EXISTS)
!
         IF(.NOT. TRAN_EXP_FILE_EXISTS) RETURN
!

         CALL GET_MAX_TRAN_EXPS(MAX_IN_TRAN_EXPS)
! 061207. SO THAT EACH LINK MAY BE BI-DIRECTIONAL.

         MAX_OUT_TRAN_EXPS = 2 * MAX_IN_TRAN_EXPS
!

         SAVE_MAX_TRAN_EXPS = MAX_IN_TRAN_EXPS
!
         CALL OPEN_TRAN_EXP_FILE
!

!
! THIS IS FROM THE GAS_NODES FILE
         MAX_GAS_GROUP_NUMBER = GET_MAX_GAS_GROUP_NUMBER()
!
         NUMBER_OF_TG = 0
!
         IF(ALLOCATED(TRANS_GROUP_POSITION)) &
              DEALLOCATE(TRANS_GROUP_POSITION, &
                     iPort, &
                     SNamePos, &
                     TieID, &
                     TieID_INDEX, &
                     ExpansionCost, &
                     ExpansionCostEscalation, &
                     MAX_C_FOR_, &
                     MIN_C_FOR_, &
                     LINE_LOSSES_A_TO_B, &
                     LINE_LOSSES_B_TO_A, &
                     CONGESTION_PERCENT, &
                     CONGESTION_COST, &
                     MaximumExpansion, &
                     ExpansionCCRF, &
                     CONGESTION_COST_ESC, &
                     CONGESTION_COST_NOMINAL, &
                     MONTHLY_CONGEST_PERCENT, &
                     MONTHLY_CONGEST_COST, &
                     ZONE_A_TO_B_PEAK_TIES, &
                     ZONE__A_TO_B_OFF_PEAK_TIES, &
                     ZONE__A_TO_B_WHEELING_RATES, &
                     ZONE_B_TO_A_PEAK_TIES, &
                     ZONE__B_TO_A_OFF_PEAK_TIES, &
                     ZONE__B_TO_A_WHEELING_RATES, &
                     ESCALATION_RATE, &
                     ZONE_A_GROUP_ID, &
                     ZONE_B_GROUP_ID, &
                     ZONE_A, &
                     ZONE_B, &
                     TieDescription, &
                     LINK_PAIR, &
                     LINK_INDEX, &
                     I_TO_K_PATHS, &
                     J_TO_K_PATHS, &
                     PATH_FOR_J_TO_K, &
                     VLI, &
                     ACTIVE_DEMAND_NODE, &
                     ACTIVE_SUPPLY_NODE)
         ALLOCATE(TRANS_GROUP_POSITION(-1:MAX_GAS_GROUP_NUMBER), &
                     TieID(MAX_OUT_TRAN_EXPS), &
                     TieID_INDEX(MAX_GAS_GROUP_NUMBER,MAX_GAS_GROUP_NUMBER), &
                     ExpansionCost(MAX_OUT_TRAN_EXPS), &
                     ExpansionCostEscalation(MAX_OUT_TRAN_EXPS), &
                     MAX_C_FOR_(MAX_OUT_TRAN_EXPS), &
                     MIN_C_FOR_(0:MAX_OUT_TRAN_EXPS), &
                     LINE_LOSSES_A_TO_B(MAX_OUT_TRAN_EXPS), &
                     LINE_LOSSES_B_TO_A(MAX_OUT_TRAN_EXPS), &
                     CONGESTION_PERCENT(MAX_OUT_TRAN_EXPS,10), &
                     CONGESTION_COST(MAX_OUT_TRAN_EXPS,10), &
                     MaximumExpansion(MAX_OUT_TRAN_EXPS), &
                     ExpansionCCRF(MAX_OUT_TRAN_EXPS), &
                     CONGESTION_COST_ESC(MAX_OUT_TRAN_EXPS), &
                     CONGESTION_COST_NOMINAL(MAX_OUT_TRAN_EXPS), &
                     MONTHLY_CONGEST_PERCENT(MAX_OUT_TRAN_EXPS,10), &
                     MONTHLY_CONGEST_COST(MAX_OUT_TRAN_EXPS,10), &
                     ZONE_A_TO_B_PEAK_TIES(MAX_OUT_TRAN_EXPS), &
                     ZONE__A_TO_B_OFF_PEAK_TIES(MAX_OUT_TRAN_EXPS), &
                     ZONE__A_TO_B_WHEELING_RATES(MAX_OUT_TRAN_EXPS), &
                     ZONE_B_TO_A_PEAK_TIES(MAX_OUT_TRAN_EXPS), &
                     ZONE__B_TO_A_OFF_PEAK_TIES(MAX_OUT_TRAN_EXPS), &
                     ZONE__B_TO_A_WHEELING_RATES(MAX_OUT_TRAN_EXPS), &
                     ESCALATION_RATE(MAX_OUT_TRAN_EXPS), &
                     ZONE_A_GROUP_ID(MAX_OUT_TRAN_EXPS), &
                     ZONE_B_GROUP_ID(MAX_OUT_TRAN_EXPS), &
                     ZONE_A(MAX_OUT_TRAN_EXPS), &
                     ZONE_B(MAX_OUT_TRAN_EXPS), &
                     TieDescription(MAX_OUT_TRAN_EXPS), &
                     iPort(MAX_OUT_TRAN_EXPS), &
                     SNamePos(MAX_OUT_TRAN_EXPS), &
                     LINK_PAIR(MAX_GAS_GROUP_NUMBER,MAX_GAS_GROUP_NUMBER), & ! NEED TO KNOW THE MAX VALUE
                     LINK_INDEX(MAX_GAS_GROUP_NUMBER,MAX_GAS_GROUP_NUMBER), & ! NEED TO KNOW THE MAX VALUE
                     I_TO_K_PATHS(MAX_GAS_GROUP_NUMBER,MAX_GAS_GROUP_NUMBER), &
                     J_TO_K_PATHS(MAX_GAS_GROUP_NUMBER,MAX_GAS_GROUP_NUMBER), &
                     PATH_FOR_J_TO_K(MAX_J_TO_K_PATHS,MAX_OUT_TRAN_EXPS), & ! MAX COUNT FOR J,K BY MAX TOTAL LINKS INCL. RESERVE
                     VLI(MAX_GAS_GROUP_NUMBER,MAX_GAS_GROUP_NUMBER), &
                     ACTIVE_DEMAND_NODE(MAX_GAS_GROUP_NUMBER), &
                     ACTIVE_SUPPLY_NODE(MAX_GAS_GROUP_NUMBER))
!
           TRANS_GROUP_POSITION = 0
!
! CHANGE TO GAS.
!


         TRANS_GROUP_POSITION(-1) = -1

         LINK_PAIR = .FALSE.
         LINK_INDEX = 0
         TieID_INDEX = 0

         ACTIVE_LINK = 1

         MAX_C_FOR_ = 0
         MIN_C_FOR_ = 0

         VLI_COUNT = 0
         VLI = 0

         iPort = 0
         SNamePos = 0
         TieDescription = 'ZZZZZZZZZZZZZ'
         CONGESTION_COST_NOMINAL = 1.0
! 061207. EACH BIDIRECTIONAL BECOMES TWO LINKS.
         DO CURRENT_RECORD = 1, MAX_IN_TRAN_EXPS
            READ(10,REC=CURRENT_RECORD) DELETE, &
                TieDescription(ACTIVE_LINK),ActiveConstraint, &
                        ExpansionCost(ACTIVE_LINK), &
                        ExpansionCostEscalation(ACTIVE_LINK), &
                        LINE_LOSSES_A_TO_B(ACTIVE_LINK),OUTAGE_RATES, &
                        ZONE_A(ACTIVE_LINK),ZONE_A_TO_B_PEAK_TIES(ACTIVE_LINK), &
                        ZONE__A_TO_B_OFF_PEAK_TIES(ACTIVE_LINK), &
                        ZONE__A_TO_B_WHEELING_RATES(ACTIVE_LINK), &
                        ZONE_B(ACTIVE_LINK),ZONE_B_TO_A_PEAK_TIES(ACTIVE_LINK), &
                        ZONE__B_TO_A_OFF_PEAK_TIES(ACTIVE_LINK), &
                        ZONE__B_TO_A_WHEELING_RATES(ACTIVE_LINK), &
                        TieID(ACTIVE_LINK),COMMENT, &
                        ZONE_A_GROUP_ID(ACTIVE_LINK), &
                        ZONE_B_GROUP_ID(ACTIVE_LINK), &
                        ZONE_A_TO_B_PLANNING_FACTOR, &
                        ZONE_B_TO_A_PLANNING_FACTOR, &
                        LINE_LOSSES_B_TO_A(ACTIVE_LINK), &
                        (CONGESTION_PERCENT(ACTIVE_LINK,I),I=1,10), &
                        (CONGESTION_COST(ACTIVE_LINK,I),I=1,10), &
                        PipelineDistance, &
                        MaximumExpansion(ACTIVE_LINK), &
                        ExpansionCCRF(ACTIVE_LINK), &
                        PipelineID, &
                        PipelineCapacity, &
                        CONGESTION_COST_ESC(ACTIVE_LINK)
!
            A_ID = ZONE_A_GROUP_ID(ACTIVE_LINK)
            B_ID = ZONE_B_GROUP_ID(ACTIVE_LINK)
!
            MAX_LINK_ID = MAX(MAX_LINK_ID,TieID(ACTIVE_LINK))

            DO I = 1, 10
               IF(ABS(CONGESTION_COST(ACTIVE_LINK,I)) < .0001) EXIT
               MAX_C_FOR_(ACTIVE_LINK) = I
!
! ALLOW FOR CONGESTION COST TO START AT THE FIRST INTERVAL.
! OTHERWISE ZERO UNTIL THE FIRST INTERVAL.
!
               IF(I == 1 .AND. &
               ABS(CONGESTION_PERCENT(ACTIVE_LINK,I)) < .0001) THEN
                  MIN_C_FOR_(ACTIVE_LINK) = 1
               ENDIF
            END DO
!
            IF(ZONE_A_TO_B_PEAK_TIES(ACTIVE_LINK) > 0. .AND. &
             LINE_LOSSES_A_TO_B(ACTIVE_LINK) < 99.999) THEN
               LINK_PAIR(A_ID,B_ID) = .TRUE.
               LINK_INDEX(A_ID,B_ID) = ACTIVE_LINK
!
               VLI_COUNT = VLI_COUNT + 1
               VLI(A_ID,B_ID) = VLI_COUNT
!
            ENDIF
         ENDDO
!
         CALL CLOSE_TRAN_EXP_FILE()
!
         call IndexedSortAlphaOrder(MAX_OUT_TRAN_EXPS, &
            iPort,SNamePos,TieDescription)
!
         ACTIVE_LINK = ACTIVE_LINK - 1
!
         SAVE_TRAN_EXP_FILE_EXISTS = .TRUE.
         MANAGE_TRAN_EXP_FORECASTS = .TRUE.
!
      RETURN
      END

!
!

      RECURSIVE FUNCTION MANAGE_RPS_SUPPLY_CURVES()
      use spindriftlib
      use prod_arrays_dimensions
      USE SIZECOM
      
      SAVE
      
      LOGICAL (KIND=1) :: MANAGE_RPS_SUPPLY_CURVES, &
                INIT_ANNUAL_RPS_SUPPLY_CURVES, &
                UPDATE_RPS_SUPPLY_CURVES, &
                GET_REC_PRICE, &
                SAVE_RPS_SUPPLY_CURVES, &
                REC_PRICING_AVAILABLE, &
                BUILD_RPS_SUPPLY_CURVES, &
                ASCENDING
      INTEGER (KIND=2) :: MAX_STATES=61, &
                MAX_TECH=6, &
                MAX_RESOURCES_PER_TYPE, &
                R_RESOURCE_NUM, &
                I,J, &
                R_RESOURCE_TYPE(*), &
                NUM_RPS_RESOURCES, &
                RESOURCES_PER_TYPE(:), &
                RPS_RESOURCES_INDEX(:), &
                RPS_RESOURCES_POSITION(:), &
                R_RESOURCE_STATE(*), &
                ST, &
                R_RPS_OPTION(*), &
                R_RPS_RESOURCE_COUNT
      REAL (KIND=4) :: RPS_SUPPLY_CURVES(:,:,:), & ! ST, TECH, MAX RESOURCES
             SYSTEM_RPS_SUPPLY_CURVES(:), &
             R_RESOURCE_MWHS(*), &
             R_RESOURCE_REV_PER_MWH(*), &
             RPS_NET_REV(:), &
             RPS_MWHS(:)
      CHARACTER (LEN=20) :: R_OPTION_NAME(*),RPS_OPTION_NAME(:)
      ALLOCATABLE :: RPS_SUPPLY_CURVES, &
                     RESOURCES_PER_TYPE, &
                     SYSTEM_RPS_SUPPLY_CURVES, &
                     RPS_RESOURCES_INDEX, &
                     RPS_RESOURCES_POSITION, &
                     RPS_NET_REV, &
                     RPS_MWHS, &
                     RPS_OPTION_NAME
! END DATA DECLARATIONS
      MANAGE_RPS_SUPPLY_CURVES = .TRUE.
      RETURN

      ENTRY INIT_ANNUAL_RPS_SUPPLY_CURVES(R_RESOURCE_NUM, &
                                          R_RESOURCE_STATE, &
                                          R_RESOURCE_TYPE, &
                                          R_RPS_OPTION, &
                                          R_RPS_RESOURCE_COUNT, &
                                          R_OPTION_NAME)

         INIT_ANNUAL_RPS_SUPPLY_CURVES = .TRUE.
         SAVE_RPS_SUPPLY_CURVES = .FALSE.
         IF(R_RPS_RESOURCE_COUNT <= 0) RETURN
         MAX_RESOURCES_PER_TYPE = 1
         NUM_RPS_RESOURCES = 0
         IF(ALLOCATED(RESOURCES_PER_TYPE)) then
             DEALLOCATE(RESOURCES_PER_TYPE,RPS_RESOURCES_INDEX)
         end if
         ALLOCATE(RESOURCES_PER_TYPE(MAX_STATES), &
            RPS_RESOURCES_INDEX(R_RESOURCE_NUM))
         RESOURCES_PER_TYPE = 0
! SET UP ARRAYS TO CREATE SUPPLY CURVES
         DO I = 1, R_RESOURCE_NUM
            IF(R_RPS_OPTION(I) < 1) CYCLE
            NUM_RPS_RESOURCES = NUM_RPS_RESOURCES + 1
            RPS_RESOURCES_INDEX(NUM_RPS_RESOURCES) = I
            ST = R_RESOURCE_STATE(I)
            IF(ST < MAX_STATES .AND. ST > 0)THEN
               RESOURCES_PER_TYPE(ST) = RESOURCES_PER_TYPE(ST) + 1
               RESOURCES_PER_TYPE = MAX(RESOURCES_PER_TYPE, &
                RESOURCES_PER_TYPE(ST))
            ELSE
               WRITE(4,*) "STATE NOT DEFINED IN RPS EXPANSION",ST
            ENDIF
         END DO
         IF(NUM_RPS_RESOURCES < 1) RETURN
         SAVE_RPS_SUPPLY_CURVES = .TRUE.
         IF(ALLOCATED(RPS_SUPPLY_CURVES)) &
                            DEALLOCATE(RPS_SUPPLY_CURVES, &
                                       SYSTEM_RPS_SUPPLY_CURVES)
         ALLOCATE(RPS_SUPPLY_CURVES( &
                            MAX_STATES,MAX_TECH,MAX_RESOURCES_PER_TYPE), &
                  SYSTEM_RPS_SUPPLY_CURVES(NUM_RPS_RESOURCES))
         IF(ALLOCATED(RPS_NET_REV)) DEALLOCATE( RPS_NET_REV, &
                                             RPS_MWHS, &
                                             RPS_RESOURCES_POSITION, &
                                             RPS_OPTION_NAME)
         ALLOCATE(RPS_NET_REV(NUM_RPS_RESOURCES), &
                  RPS_MWHS(NUM_RPS_RESOURCES), &
                  RPS_RESOURCES_POSITION(NUM_RPS_RESOURCES), &
                  RPS_OPTION_NAME(NUM_RPS_RESOURCES))
         RPS_SUPPLY_CURVES = 0.0
         SYSTEM_RPS_SUPPLY_CURVES = 0.0
         RPS_NET_REV = 0.0
         RPS_MWHS = 0.0
         RPS_OPTION_NAME = '                    '
         DO I = 1, NUM_RPS_RESOURCES
            J = RPS_RESOURCES_INDEX(I)
            RPS_OPTION_NAME(I) = R_OPTION_NAME(J)
         ENDDO
      RETURN

      ENTRY BUILD_RPS_SUPPLY_CURVES(R_RESOURCE_MWHS, &
                                    R_RESOURCE_REV_PER_MWH)

! BUILD CURVES BY ST TECH INCLUDING FOR NA
! SORT CURVES BY ST TECH INCLUDING FOR NA
! IDENTIFY THE TOTAL SURPLUS AND DEFICIT MW OF THE SYSTEM
! IDENTIFY THE MARGINAL PRICE AT THAT POINT
! REPORT RPS SUPPLY CURVES
!
         BUILD_RPS_SUPPLY_CURVES = .FALSE.
!
         IF(.NOT. SAVE_RPS_SUPPLY_CURVES) RETURN
!
         DO I = 1, NUM_RPS_RESOURCES
            J = RPS_RESOURCES_INDEX(I)
            RPS_NET_REV(I) = R_RESOURCE_REV_PER_MWH(J)
            RPS_MWHS(I) = R_RESOURCE_MWHS(J)
            RPS_RESOURCES_POSITION(I) = I
         END DO
!
         CALL INDEXED_SORT_ASCENDING(RPS_NET_REV, &
                                     RPS_RESOURCES_POSITION, &
                                     NUM_RPS_RESOURCES, &
                                     ASCENDING)
!
         BUILD_RPS_SUPPLY_CURVES = .TRUE.
!
      RETURN

      ENTRY REC_PRICING_AVAILABLE

         REC_PRICING_AVAILABLE = SAVE_RPS_SUPPLY_CURVES
      RETURN
      END

!
!                  ROUTINE TO CONVERT CO2_PARAM FILE
!
!                          COPYRIGHT (C) 2009
!                                 VENTYX
!                          ALL RIGHTS RESERVED
!

!
! 070906. CREATED.
! 070906. MAY NEED TO MAKE THIS A MULTI-FILE SPECIFICATION.
!
      SUBROUTINE CO2_PARAM_OBJECT
      use end_routine, only: end_program, er_message
      use filename_tracker
!
      use spindriftlib
      use prod_arrays_dimensions
      USE SIZECOM
      character (len=2) :: green_mrx_result
      INTEGER (KIND=2) ::   UNIT_NUM=10
      INTEGER (KIND=2) :: INUNIT,IREC,LRECL=300
      INTEGER (KIND=2) ::   SAVE_NUMBER_OF_CO2_PARAM=0, &
                  R_NUMBER_OF_CO2_PARAM
      INTEGER (KIND=2) ::   TRANS_LINE_INDEX
      CHARACTER (len=5) :: BASE_FILE_NAME,OVERLAY_FAMILY_NAME
      CHARACTER (LEN=256) :: CO2_PARAM_FILE
      CHARACTER (len=256) :: FILE_NAME
      CHARACTER (LEN=255) :: SAVE_BASE_FILE_NAME,R_TEMP_NAME
      CHARACTER (len=256) :: BASE_FILE_DIRECTORY
      CHARACTER (len=256) :: DATA_DRIVE,OUTPUT_DIRECTORY
      CHARACTER (LEN=1) ::  CO2_PARAM_ACTIVE
      LOGICAL (KIND=4) :: FILE_EXISTS=.FALSE., &
                  CO2_PARAM_FILE_EXISTS=.FALSE.,R_CO2_PARAM_FILE_EXISTS
! DECLARATION FOR TRANSACT CO2_PARAM DETERMINANTS
      CHARACTER (LEN=21) :: FILE_TYPE='CO2 Offset Markets   '
      CHARACTER (LEN=2) :: CO2_PARAM_OL='BC'
      LOGICAL (KIND=1) :: LAHEY_LF95
      CHARACTER (len=30) :: SCREEN_OUTPUT
!
      INTEGER (KIND=2) :: DELETE,YR
      INTEGER (KIND=4) :: IOS
      CHARACTER (LEN=4096) :: RECLN
      REAL (KIND=4) :: &
                       CO2_Emission_Cap, &
                       Residential_Emissions, &
                       Commercial_Emission, &
                       Industrial_Emissions, &
                       Transportation_Emissions, &
                       Dom_Min_Off_Sets_To_Buy, &
                       Dom_Max_Off_Sets_To_Buy, &
                       Int_Min_Off_Sets_To_Buy, &
                       Int_Max_Off_Sets_To_Buy, &
                       Non_Elect_Min_Reductions, &
                       Non_Elect_Max_Reductions, &
                       CO2_Banking, &
                       Dom_1_A_Coefficient, &
                       Dom_1_B_Coefficient, &
                       Dom_Price_Break
      REAL (KIND=4) :: &
                       Dom_2_A_Coefficient, &
                       Dom_2_B_Coefficient, &
                       Int_1_A_Coefficient, &
                       Int_1_B_Coefficient, &
                       Int_Price_Break, &
                       Int_2_A_Coefficient, &
                       Int_2_B_Coefficient, &
                       Non_El_1_A_Coefficient, &
                       Non_El_1_B_Coefficient, &
                       Non_El_Price_Break, &
                       Non_El_2_A_Coefficient, &
                       Non_El_2_B_Coefficient, &
                       CO2_Price_Forecast, &
                       CO2_Max_Price_Cap, &
                       CO2_Min_Price
      INTEGER (KIND=2) :: CO2_Min_GRX_Iterations, &
                          CO2_Max_GRX_Iterations, &
                          CO2_GRX_Iter_After_Converg
      CHARACTER (LEN=15) :: CO2_Price_Evaluation_Switch
      CHARACTER (LEN=10) :: CO2_Market_Available_Switch
      CHARACTER (LEN=12) :: &
                       Dom_Price_Equation_1_Type, &
                       Dom_Price_Equation_2_Type, &
                       Int_Price_Equation_1_Type, &
                       Int_Price_Equation_2_Type, &
                       Non_El_Price_Equation_1_Type, &
                       Non_El_Price_Equation_2_Type
      CHARACTER (LEN=50) :: COMMENT
      SAVE SAVE_BASE_FILE_NAME
      CHARACTER (LEN=2) :: CAPACITY_PLANNING_METHOD,GREEN_MRX_METHOD
      character (len=2) :: cap_method_result
! CONVERT THE CO2_PARAM FILE
!
!
!

      ENTRY CO2_PARAM_MAKEBIN

      BASE_FILE_NAME = CO2_PARAM_FILE ()
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME = get_20b_filename(base_file_name)
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      CO2_PARAM_FILE_EXISTS = FILE_EXISTS
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
         OPEN(11,FILE=trim(DATA_DRIVE)// &
            "BCCO2_PARAM.BIN",ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         CO2_Max_Price_Cap = 999.
         CO2_Min_Price = 0.
         CO2_Min_GRX_Iterations = 1
         CO2_Max_GRX_Iterations = 10
         CO2_GRX_Iter_After_Converg = 0
         CO2_Market_Available_Switch = 'Active'
         IREC = 1
!
         READ(10,1000,IOSTAT=IOS) RECLN
!
         DO
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(IOS /= 0) EXIT
            READ(RECLN,*,ERR=200) DELETE
!
            IF(DELETE == 7) THEN
               CO2_Max_Price_Cap = 999.
               CO2_Min_Price = 0.
               CO2_Min_GRX_Iterations = 1
               CO2_Max_GRX_Iterations = 10
               CO2_GRX_Iter_After_Converg = 0
               CYCLE ! NEW TABLE
            ENDIF
!
            RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
!
!
            READ(RECLN,*,ERR=200)   DELETE, &
            YR,CO2_Emission_Cap,Residential_Emissions, &
            Commercial_Emission, &
            Industrial_Emissions,Transportation_Emissions, &
            Dom_Min_Off_Sets_To_Buy, &
              Dom_Max_Off_Sets_To_Buy,Int_Min_Off_Sets_To_Buy, &
              Int_Max_Off_Sets_To_Buy, &
              Non_Elect_Min_Reductions,Non_Elect_Max_Reductions, &
              CO2_Banking, &
              Dom_Price_Equation_1_Type,Dom_1_A_Coefficient, &
              Dom_1_B_Coefficient, &
              Dom_Price_Break,Dom_Price_Equation_2_Type, &
              Dom_2_A_Coefficient, &
              Dom_2_B_Coefficient,Int_Price_Equation_1_Type, &
              Int_1_A_Coefficient, &
              Int_1_B_Coefficient,Int_Price_Break, &
              Int_Price_Equation_2_Type, &
              Int_2_A_Coefficient,Int_2_B_Coefficient, &
              Non_El_Price_Equation_1_Type,Non_El_1_A_Coefficient, &
              Non_El_1_B_Coefficient, &
              Non_El_Price_Break,Non_El_Price_Equation_2_Type, &
              Non_El_2_A_Coefficient, &
              Non_El_2_B_Coefficient,COMMENT, &
              CO2_Price_Forecast, &
              CO2_Price_Evaluation_Switch, &
              CO2_Max_Price_Cap, &
              CO2_Min_Price, &
              CO2_Min_GRX_Iterations, &
              CO2_Max_GRX_Iterations, &
             CO2_GRX_Iter_After_Converg, &
              CO2_Market_Available_Switch
            WRITE(11,REC=IREC) DELETE,YR,CO2_Emission_Cap, &
       Residential_Emissions,Commercial_Emission,Industrial_Emissions, &
              Transportation_Emissions,Dom_Min_Off_Sets_To_Buy, &
              Dom_Max_Off_Sets_To_Buy, &
              Int_Min_Off_Sets_To_Buy,Int_Max_Off_Sets_To_Buy, &
              Non_Elect_Min_Reductions, &
              Non_Elect_Max_Reductions, &
              CO2_Banking,Dom_Price_Equation_1_Type, &
              Dom_1_A_Coefficient,Dom_1_B_Coefficient,Dom_Price_Break, &
              Dom_Price_Equation_2_Type, &
              Dom_2_A_Coefficient,Dom_2_B_Coefficient, &
              Int_Price_Equation_1_Type, &
              Int_1_A_Coefficient,Int_1_B_Coefficient, &
              Int_Price_Break,Int_Price_Equation_2_Type, &
              Int_2_A_Coefficient,Int_2_B_Coefficient, &
              Non_El_Price_Equation_1_Type,Non_El_1_A_Coefficient, &
              Non_El_1_B_Coefficient, &
              Non_El_Price_Break,Non_El_Price_Equation_2_Type, &
              Non_El_2_A_Coefficient, &
              Non_El_2_B_Coefficient,COMMENT, &
              CO2_Price_Forecast, &
              CO2_Price_Evaluation_Switch, &
              CO2_Max_Price_Cap, &
              CO2_Min_Price, &
              CO2_Min_GRX_Iterations, &
              CO2_Max_GRX_Iterations, &
             CO2_GRX_Iter_After_Converg, &
              CO2_Market_Available_Switch
            IREC = IREC + 1
         ENDDO
         CLOSE(10)
         CLOSE(11)
      ELSE
         cap_method_result=CAPACITY_PLANNING_METHOD()
         green_mrx_result=GREEN_MRX_METHOD()
         
         IF(cap_method_result == 'MX'  &
         .AND. green_mrx_result == 'GX') THEN
            er_message='For GRX planning a CO2 ' &
            //'Offsets Market file is required.'
            CALL MG_LOCATE_WRITE(20,0,trim(er_message),ALL_VERSIONS,1)
            er_message="msgmmrev:1113 - " // trim(er_message)
            call end_program(er_message)
            ! This won't get called, but is left for documentation purposes.
            CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "msgmmrev:1113")
         ENDIF

         IF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
            CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "msgmmrev:1112")
         ENDIF
      ENDIF
!
      SAVE_NUMBER_OF_CO2_PARAM = MAX(0,IREC-1)
!
      RETURN



! OVERLAY THE CO2_PARAM FILE

      ENTRY CO2_PARAM_MAKEOVL(OVERLAY_FAMILY_NAME)


      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME = get_2OO_filename(data_drive, overlay_family_name)
      OPEN(10,FILE=FILE_NAME)
      READ(10,*) DELETE
      INUNIT = 12
      IF(CO2_PARAM_OL == 'BC') THEN
         OPEN(11,FILE=trim(DATA_DRIVE)// &
            "BCCO2_PARAM.BIN",ACCESS="DIRECT",RECL=LRECL)
         INUNIT = 11
      ENDIF
      OPEN(12,FILE=trim(DATA_DRIVE)// &
        "OLCO2_PARAM.BIN",ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      IREC = 0
!
      READ(10,1000,IOSTAT=IOS) RECLN
      DO WHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE
!
         READ(10,1000,IOSTAT=IOS) RECLN
!
      ENDDO
!
      DO
!
         IREC = IREC + 1
         READ(INUNIT,REC=IREC,IOSTAT=IOS) DELETE, &
            YR,CO2_Emission_Cap,Residential_Emissions,Commercial_Emission, &
                                    Industrial_Emissions,Transportation_Emissions,Dom_Min_Off_Sets_To_Buy, &
                                    Dom_Max_Off_Sets_To_Buy,Int_Min_Off_Sets_To_Buy,Int_Max_Off_Sets_To_Buy, &
                                    Non_Elect_Min_Reductions,Non_Elect_Max_Reductions,CO2_Banking, &
                                    Dom_Price_Equation_1_Type,Dom_1_A_Coefficient,Dom_1_B_Coefficient, &
                                    Dom_Price_Break,Dom_Price_Equation_2_Type,Dom_2_A_Coefficient,Dom_2_B_Coefficient, &
                                    Int_Price_Equation_1_Type, &
                    Int_1_A_Coefficient,Int_1_B_Coefficient,Int_Price_Break, &
                                    Int_Price_Equation_2_Type,Int_2_A_Coefficient,Int_2_B_Coefficient, &
                                    Non_El_Price_Equation_1_Type,Non_El_1_A_Coefficient,Non_El_1_B_Coefficient, &
                                    Non_El_Price_Break,Non_El_Price_Equation_2_Type,Non_El_2_A_Coefficient, &
                                    Non_El_2_B_Coefficient,COMMENT, &
                                    CO2_Price_Forecast, &
                                    CO2_Price_Evaluation_Switch, &
                                    CO2_Max_Price_Cap, &
                                    CO2_Min_Price, &
                                    CO2_Min_GRX_Iterations, &
                                    CO2_Max_GRX_Iterations, &
                                   CO2_GRX_Iter_After_Converg, &
                                    CO2_Market_Available_Switch
         IF(IOS /= 0) EXIT

         RECLN = trim(RECLN)//',,,,,,,,,,,,,'
         READ(RECLN,*,ERR=300) DELETE,YR,CO2_Emission_Cap, &
            Residential_Emissions,Commercial_Emission, &
            Industrial_Emissions, &
             Transportation_Emissions,Dom_Min_Off_Sets_To_Buy, &
             Dom_Max_Off_Sets_To_Buy, &
             Int_Min_Off_Sets_To_Buy,Int_Max_Off_Sets_To_Buy, &
             Non_Elect_Min_Reductions,Non_Elect_Max_Reductions, &
             CO2_Banking, &
             Dom_Price_Equation_1_Type,Dom_1_A_Coefficient, &
             Dom_1_B_Coefficient, &
             Dom_Price_Break,Dom_Price_Equation_2_Type, &
             Dom_2_A_Coefficient,Dom_2_B_Coefficient, &
             Int_Price_Equation_1_Type,Int_1_A_Coefficient, &
             Int_1_B_Coefficient, &
             Int_Price_Break,Int_Price_Equation_2_Type, &
             Int_2_A_Coefficient,Int_2_B_Coefficient, &
             Non_El_Price_Equation_1_Type,Non_El_1_A_Coefficient, &
             Non_El_1_B_Coefficient, &
             Non_El_Price_Break,Non_El_Price_Equation_2_Type, &
             Non_El_2_A_Coefficient, &
             Non_El_2_B_Coefficient, &
             COMMENT, &
             CO2_Price_Forecast, &
             CO2_Price_Evaluation_Switch, &
             CO2_Max_Price_Cap, &
             CO2_Min_Price, &
             CO2_Min_GRX_Iterations, &
             CO2_Max_GRX_Iterations, &
            CO2_GRX_Iter_After_Converg, &
             CO2_Market_Available_Switch
!         ENDIF
!
         READ(10,1000,IOSTAT=IOS) RECLN
         DO WHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE
!
            READ(10,1000,IOSTAT=IOS) RECLN
!
         ENDDO
         WRITE(12,REC=IREC) DELETE,YR,CO2_Emission_Cap,Residential_Emissions,Commercial_Emission,Industrial_Emissions, &
                                    Transportation_Emissions,Dom_Min_Off_Sets_To_Buy,Dom_Max_Off_Sets_To_Buy, &
                                    Int_Min_Off_Sets_To_Buy,Int_Max_Off_Sets_To_Buy, &
                                    Non_Elect_Min_Reductions,Non_Elect_Max_Reductions,CO2_Banking, &
                                    Dom_Price_Equation_1_Type,Dom_1_A_Coefficient,Dom_1_B_Coefficient, &
                                    Dom_Price_Break,Dom_Price_Equation_2_Type,Dom_2_A_Coefficient,Dom_2_B_Coefficient, &
                                    Int_Price_Equation_1_Type,Int_1_A_Coefficient,Int_1_B_Coefficient, &
                                    Int_Price_Break,Int_Price_Equation_2_Type,Int_2_A_Coefficient,Int_2_B_Coefficient, &
                                    Non_El_Price_Equation_1_Type,Non_El_1_A_Coefficient,Non_El_1_B_Coefficient, &
                                    Non_El_Price_Break,Non_El_Price_Equation_2_Type,Non_El_2_A_Coefficient, &
                                    Non_El_2_B_Coefficient,COMMENT, &
                                    CO2_Price_Forecast, &
                                    CO2_Price_Evaluation_Switch, &
                                    CO2_Max_Price_Cap, &
                                    CO2_Min_Price, &
                                    CO2_Min_GRX_Iterations, &
                                    CO2_Max_GRX_Iterations, &
                                   CO2_GRX_Iter_After_Converg, &
                                    CO2_Market_Available_Switch
!
      ENDDO
      CLOSE(10)
      CLOSE(12)
      IF(CO2_PARAM_OL == 'BC') CLOSE(11)
      CO2_PARAM_OL = 'OL'
      RETURN

  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from Msgmmrev SIID238'
      call end_program(er_message)

  300 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      CALL MG_LOCATE_WRITE(21,0,'Error reading the Trans Exp record. Look for'// &
                                          ' a "," in a character name.', &
                           ALL_VERSIONS,1)
      er_message='stop requested from Msgmmrev SIID239'
      call end_program(er_message)


      ENTRY RESET_CO2_PARAM_OL

         CO2_PARAM_OL = 'BC'
      RETURN


      ENTRY OPEN_CO2_PARAM_FILE

         OPEN(UNIT_NUM,FILE=trim(OUTPUT_DIRECTORY())//CO2_PARAM_OL// &
           "CO2_PARAM.BIN",ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      RETURN


      ENTRY CLOSE_CO2_PARAM_FILE

         CLOSE(UNIT_NUM)
      RETURN

      ENTRY DOES_CO2_PARAM_FILE_EXIST(R_CO2_PARAM_FILE_EXISTS)

         R_CO2_PARAM_FILE_EXISTS = CO2_PARAM_FILE_EXISTS
      RETURN

      ENTRY GET_MAX_CO2_PARAM(R_NUMBER_OF_CO2_PARAM)

         R_NUMBER_OF_CO2_PARAM = SAVE_NUMBER_OF_CO2_PARAM
      RETURN

 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END

!
!
!     MAINTAINS LIST OF FROM/TO CO2_PARAM AS WELL AS MW LOADING ON CO2_PARAM
!     AND TIES
!
!
      FUNCTION MANAGE_CO2_PARAM_FORECASTS()
!
!


      USE CO2_CAP_N_TRADE
      use spindriftlib
      use prod_arrays_dimensions
      USE SIZECOM

      SAVE
      LOGICAL (KIND=1) ::   SAVE_CO2_PARAM_FILE_EXISTS=.FALSE., &
                  MANAGE_CO2_PARAM_FORECASTS
      INTEGER (KIND=2) :: &
                        YR, &
                        SAVE_MAX_CO2_PARAM, &
                        MAX_IN_CO2_PARAM, &
                        CURRENT_RECORD, &
                        DELETE, &
                        I
      CHARACTER (LEN=50) :: COMMENT
      CHARACTER (LEN=15) :: CO2_Price_Evaluation_Switch
      CHARACTER (LEN=10) :: CO2_Market_Available_Switch
!
! END DATA DECLARATIONS
!
!
         MANAGE_CO2_PARAM_FORECASTS = .FALSE.
         SAVE_CO2_PARAM_FILE_EXISTS = .FALSE.
         CO2_BANK_BALANCE = 0.
!
!
         CALL DOES_CO2_PARAM_FILE_EXIST(CO2_PARAM_FILE_EXISTS)
!
         IF(.NOT. CO2_PARAM_FILE_EXISTS) RETURN
         CALL GET_MAX_CO2_PARAM(MAX_IN_CO2_PARAM)
         SAVE_MAX_CO2_PARAM = MAX_IN_CO2_PARAM
!
         CALL OPEN_CO2_PARAM_FILE
         READ(10,REC=1) DELETE,YR1_DATA
         READ(10,REC=MAX_IN_CO2_PARAM) DELETE,YR2_DATA
         IF(.NOT. ALLOCATED(CO2_Emission_Cap)) THEN
            ALLOCATE(CO2_Emission_Cap(YR1_DATA:YR2_DATA), &
            Residential_Emissions(YR1_DATA:YR2_DATA), &
                     Commercial_Emission(YR1_DATA:YR2_DATA), &
                     Industrial_Emissions(YR1_DATA:YR2_DATA), &
                     Transportation_Emissions(YR1_DATA:YR2_DATA))
                     
           allocate(Dom_Min_Off_Sets_To_Buy(YR1_DATA:YR2_DATA), &
                     Dom_Max_Off_Sets_To_Buy(YR1_DATA:YR2_DATA), &
                     Int_Min_Off_Sets_To_Buy(YR1_DATA:YR2_DATA), &
                     Int_Max_Off_Sets_To_Buy(YR1_DATA:YR2_DATA), &
                     Non_Elect_Min_Reductions(YR1_DATA:YR2_DATA), &
                     Non_Elect_Max_Reductions(YR1_DATA:YR2_DATA), &
                     CO2_Banking(YR1_DATA:YR2_DATA), &
                     Dom_Price_Equation_1_Type(YR1_DATA:YR2_DATA), &
                     Dom_1_A_Coefficient(YR1_DATA:YR2_DATA), &
                     Dom_1_B_Coefficient(YR1_DATA:YR2_DATA), &
                     Dom_Price_Break(YR1_DATA:YR2_DATA), &
                     Dom_Price_Equation_2_Type(YR1_DATA:YR2_DATA), &
                     Dom_2_A_Coefficient(YR1_DATA:YR2_DATA), &
                     Dom_2_B_Coefficient(YR1_DATA:YR2_DATA), &
                     Int_Price_Equation_1_Type(YR1_DATA:YR2_DATA), &
                     Int_1_A_Coefficient(YR1_DATA:YR2_DATA), &
                     Int_1_B_Coefficient(YR1_DATA:YR2_DATA), &
                     Int_Price_Break(YR1_DATA:YR2_DATA), &
                     Int_Price_Equation_2_Type(YR1_DATA:YR2_DATA), &
                     Int_2_A_Coefficient(YR1_DATA:YR2_DATA), &
                     Int_2_B_Coefficient(YR1_DATA:YR2_DATA), &
                     Non_El_Price_Equation_1_Type(YR1_DATA:YR2_DATA), &
                     Non_El_1_A_Coefficient(YR1_DATA:YR2_DATA), &
                     Non_El_1_B_Coefficient(YR1_DATA:YR2_DATA), &
                     Non_El_Price_Break(YR1_DATA:YR2_DATA), &
                     Non_El_Price_Equation_2_Type(YR1_DATA:YR2_DATA), &
                     Non_El_2_A_Coefficient(YR1_DATA:YR2_DATA), &
                     Non_El_2_B_Coefficient(YR1_DATA:YR2_DATA), &
                     CO2_Price_Forecast(YR1_DATA:YR2_DATA), &
                     CO2_Price_Evaluation(YR1_DATA:YR2_DATA), &
                     CO2_Max_Price_Cap(YR1_DATA:YR2_DATA), &
                     CO2_Min_Price(YR1_DATA:YR2_DATA), &
                     CO2_Min_GRX_Iterations(YR1_DATA:YR2_DATA), &
                     CO2_Max_GRX_Iterations(YR1_DATA:YR2_DATA), &
                     CO2_Market_Available(YR1_DATA:YR2_DATA), &
               CO2_GRX_Iter_After_Converg(YR1_DATA:YR2_DATA), &
                     POKE_CURRENT_CO2_DISPATCH_COST(YR1_DATA:YR2_DATA))
         ENDIF

         I = 1
         DO CURRENT_RECORD = 1, MAX_IN_CO2_PARAM
            READ(10,REC=CURRENT_RECORD) DELETE, &
            YR,CO2_Emission_Cap(YR),Residential_Emissions(YR), &
                Commercial_Emission(YR),Industrial_Emissions(YR), &
                Transportation_Emissions(YR), &
                Dom_Min_Off_Sets_To_Buy(YR), &
                Dom_Max_Off_Sets_To_Buy(YR), &
                Int_Min_Off_Sets_To_Buy(YR), &
                Int_Max_Off_Sets_To_Buy(YR), &
                Non_Elect_Min_Reductions(YR), &
                Non_Elect_Max_Reductions(YR), &
                CO2_Banking(YR), &
                Dom_Price_Equation_1_Type(YR),Dom_1_A_Coefficient(YR), &
                Dom_1_B_Coefficient(YR), &
                Dom_Price_Break(YR),Dom_Price_Equation_2_Type(YR), &
                Dom_2_A_Coefficient(YR),Dom_2_B_Coefficient(YR), &
                Int_Price_Equation_1_Type(YR),Int_1_A_Coefficient(YR), &
                Int_1_B_Coefficient(YR), &
                Int_Price_Break(YR),Int_Price_Equation_2_Type(YR), &
                Int_2_A_Coefficient(YR), &
                Int_2_B_Coefficient(YR), &
                Non_El_Price_Equation_1_Type(YR), &
                Non_El_1_A_Coefficient(YR),Non_El_1_B_Coefficient(YR), &
                Non_El_Price_Break(YR),Non_El_Price_Equation_2_Type(YR), &
                Non_El_2_A_Coefficient(YR), &
                Non_El_2_B_Coefficient(YR),COMMENT, &
                CO2_Price_Forecast(YR),CO2_Price_Evaluation_Switch, &
                CO2_Max_Price_Cap(YR), &
                CO2_Min_Price(YR),CO2_Min_GRX_Iterations(YR), &
                CO2_Max_GRX_Iterations(YR), &
                CO2_GRX_Iter_After_Converg(YR), &
                CO2_Market_Available_Switch
            CO2_Price_Evaluation(YR) = &
                           CO2_Price_Evaluation_Switch(1:1)=='T' .OR. &
                                   CO2_Price_Evaluation_Switch(1:1)=='P'
            CO2_Market_Available(YR) = &
                CO2_Market_Available_Switch(1:1)=='A'
            POKE_CURRENT_CO2_DISPATCH_COST(YR) = INDEX(CO2_Price_Evaluation_Switch,'REGION') == 0
            IF(.not. (CO2_Min_GRX_Iterations(YR) == 0 .AND. CO2_Max_GRX_Iterations(YR) == 0)) THEN
               CO2_Min_GRX_Iterations(YR) = MAX(INT(1,2),CO2_Min_GRX_Iterations(YR))
               CO2_Max_GRX_Iterations(YR)= &
                           MIN(INT(20,2),MAX(CO2_Max_GRX_Iterations(YR), &
                                            CO2_Min_GRX_Iterations(YR)))
            ENDIF
            I = I + 1
         ENDDO
!
         CALL CLOSE_CO2_PARAM_FILE()
!
         SAVE_CO2_PARAM_FILE_EXISTS = .TRUE.
         MANAGE_CO2_PARAM_FORECASTS = .TRUE.
!
      RETURN
      END
!
!      Program_Name C50
!      Program_Number I4
!      Program_Active C3
!      Initial_Bank R4
!      Maximum_Bank R4
!      Alt_Compliance_Price R4
!      Annual_Requirement(30) R4x30
!      Comment C50
!      Total Record Length = 239

!
!                  ROUTINE TO CONVERT RPS_PROGRAM FILE
!
!                          COPYRIGHT (C) 2018
!                                   ABB
!                          ALL RIGHTS RESERVED
!

!
! 102818. CREATED.
!
      SUBROUTINE RPS_PROGRAM_OBJECT
      use end_routine, only: end_program, er_message
      use filename_tracker
      use mmrev_decs
      use end_routine
      use spindriftlib
      use prod_arrays_dimensions
      USE SIZECOM

      CHARACTER (len=256) :: alt_filename
      INTEGER (KIND=2) :: UNIT_NUM=10
      INTEGER (KIND=2) :: INUNIT,IREC,LRECL=245
!
      INTEGER (KIND=2) :: SAVE_NUMBER_OF_RPS_PROGRAMS=0, &
                  R_NUMBER_OF_RPS_PROGRAMS
      CHARACTER (len=5) :: BASE_FILE_NAME,OVERLAY_FAMILY_NAME
      CHARACTER (LEN=5) :: RPS_PROGRAM_FILE
      CHARACTER (len=256) :: FILE_NAME
      CHARACTER (LEN=255) :: SAVE_BASE_FILE_NAME,R_TEMP_NAME
      CHARACTER (len=256) :: BASE_FILE_DIRECTORY
      CHARACTER (len=256) :: DATA_DRIVE,OUTPUT_DIRECTORY
      CHARACTER (LEN=1) ::  RPS_PROGRAM_ACTIVE
      LOGICAL (KIND=4) :: FILE_EXISTS=.FALSE.
      LOGICAL (KIND=4) :: R_RPS_PROGRAM_FILE_EXISTS
! DECLARATION FOR TRANSACT RPS_PROGRAM DETERMINANTS
      CHARACTER (LEN=16) :: FILE_TYPE='RPS PROGRAM          '
      CHARACTER (LEN=2) ::  RPS_PROGRAM_OL='BC'
      LOGICAL (KIND=1) :: LAHEY_LF95
      CHARACTER (len=30) :: SCREEN_OUTPUT

! 070906.
      INTEGER (KIND=2) :: DELETE,YR
      INTEGER (KIND=4) :: IOS
      integer :: lt
      character (len=1024) :: tfn_part
      CHARACTER (LEN=4096) RECLN
      REAL (KIND=4) ::  Initial_Bank, &
                        Maximum_Bank, &
                        Alt_Compliance_Price, &
                        Annual_Requirement(30), &
                        REC_Price
      INTEGER (KIND=4) :: Program_Number
      CHARACTER (LEN=3) :: Program_Active
      CHARACTER (LEN=50) :: Comment,Program_Name
      SAVE SAVE_BASE_FILE_NAME


!
! CONVERT THE RPS_PROGRAM FILE
!
!
!

      ENTRY RPS_PROGRAM_MAKEBIN

      BASE_FILE_NAME = RPS_PROGRAM_FILE()
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME = get_rqb_filename(base_file_name)


      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      RPS_PROGRAM_FILE_EXISTS = FILE_EXISTS
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
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCRPS_PROGRAM.BIN",ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         IREC = 1
!
!        Initialize values
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
            READ(RECLN,*,ERR=200) DELETE, &
                                    Program_Name, &
                                    Program_Number, &
                                    Program_Active, &
                                    Initial_Bank, &
                                    Maximum_Bank, &
                                    Alt_Compliance_Price, &
                                    Annual_Requirement, &
                                    Comment, &
                                    REC_Price
            WRITE(11,REC=IREC) DELETE, &
                                    Program_Name, &
                                    Program_Number, &
                                    Program_Active, &
                                    Initial_Bank, &
                                    Maximum_Bank, &
                                    Alt_Compliance_Price, &
                                    Annual_Requirement, &
                                    Comment, &
                                    REC_Price
            IREC = IREC + 1
         ENDDO
         CLOSE(10)
         CLOSE(11)
      ELSE IF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "msgmmrev:1111")
      ENDIF
!
      SAVE_NUMBER_OF_RPS_PROGRAMS = MAX(0,IREC-1)
!
      RETURN



! OVERLAY THE RPS_PROGRAM FILE

      ENTRY RPS_PROGRAM_MAKEOVL(OVERLAY_FAMILY_NAME)

!
      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME = get_rqo_filename(data_drive, overlay_family_name)
      OPEN(10,FILE=FILE_NAME)
      READ(10,*) DELETE
      INUNIT = 12
      IF(RPS_PROGRAM_OL == 'BC') THEN
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCRPS_PROGRAM.BIN",ACCESS="DIRECT",RECL=LRECL)
         INUNIT = 11
      ENDIF
      OPEN(12,FILE=trim(DATA_DRIVE)//"OLRPS_PROGRAM.BIN",ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      IREC = 0
!
      READ(10,1000,IOSTAT=IOS) RECLN
      DO WHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE
!
         READ(10,1000,IOSTAT=IOS) RECLN
!
      ENDDO
!
      DO
!
         IREC = IREC + 1
         READ(INUNIT,REC=IREC,IOSTAT=IOS) DELETE, &
                                    Program_Name, &
                                    Program_Number, &
                                    Program_Active, &
                                    Initial_Bank, &
                                    Maximum_Bank, &
                                    Alt_Compliance_Price, &
                                    Annual_Requirement, &
                                    Comment, &
                                    REC_Price
         IF(IOS /= 0) EXIT
!
         RECLN = trim(RECLN)//',,,,,,,,,,,,,'
         READ(RECLN,*,ERR=300) DELETE, &
                                    Program_Name, &
                                    Program_Number, &
                                    Program_Active, &
                                    Initial_Bank, &
                                    Maximum_Bank, &
                                    Alt_Compliance_Price, &
                                    Annual_Requirement, &
                                    Comment, &
                                    REC_Price
!         ENDIF
!
         READ(10,1000,IOSTAT=IOS) RECLN
         DO WHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE
!
            READ(10,1000,IOSTAT=IOS) RECLN
!
         ENDDO
         WRITE(12,REC=IREC) DELETE, &
                                    Program_Name, &
                                    Program_Number, &
                                    Program_Active, &
                                    Initial_Bank, &
                                    Maximum_Bank, &
                                    Alt_Compliance_Price, &
                                    Annual_Requirement, &
                                    Comment, &
                                    REC_Price
!
      ENDDO
      CLOSE(10)
      CLOSE(12)
      IF(RPS_PROGRAM_OL == 'BC') CLOSE(11)
      RPS_PROGRAM_OL = 'OL'
      RETURN
!
  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from Msgmmrev SIID240'
      call end_program(er_message)
  300 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      CALL MG_LOCATE_WRITE(21,0, &
                      'Error reading the RPS Program record. Look for'// &
                                          ' a "," in a character name.', &
                           ALL_VERSIONS,1)
      er_message='stop requested from Msgmmrev SIID241'
      call end_program(er_message)
!

      ENTRY RESET_RPS_PROGRAM_OL

         RPS_PROGRAM_OL = 'BC'
      RETURN
!

      ENTRY OPEN_RPS_PROGRAM_FILE

         OPEN(UNIT_NUM,FILE=trim(OUTPUT_DIRECTORY())//RPS_PROGRAM_OL// &
            "RPS_PROGRAM.BIN", &
          ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      RETURN
!

      ENTRY CLOSE_RPS_PROGRAM_FILE

         CLOSE(UNIT_NUM)
      RETURN



      ENTRY GET_MAX_RPS_PROGRAMS(R_NUMBER_OF_RPS_PROGRAMS)

         R_NUMBER_OF_RPS_PROGRAMS = SAVE_NUMBER_OF_RPS_PROGRAMS
      RETURN

 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END

!     MAINTAINS DATA FOR RPS PROGRAMS.

      RECURSIVE FUNCTION MANAGE_RPS_PROGRAM_FORECASTS()
      use capacity_arrays
      use end_routine
      use endpoint
      use grx_planning_routines
      use irec_endpoint_control
      use mmrev_decs
      use mod_base_year
      use prod_arrays_dimensions
      use rptreccontrol
      use sizecom
      use spindriftlib
      
      implicit none
      SAVE

      LOGICAL (KIND=1) :: SAVE_RPS_PROGRAM_FILE_ACTIVE=.FALSE., &
                  MANAGE_RPS_PROGRAM_FORECASTS, &
                  PUT_RPS_ENRG_CAP, &
                  RPS_PROGRAMS_ANNUAL_CALC, &
                  STOP_RPS, &
                  YES_RPS_PROGRAMS_REPORT, &
                  RPS_PROGRAMS_REPORT_NOT_OPEN=.TRUE., &
                  IS_IN_RPS_PROGRAM, &
                  RPS_PROGRAMS_ANNUAL_INIT, &
                  RESET_MRX_RPS_TEST_REQ, &
                  GRX_SAVE_RPS_PROGRAM_VALUES, &
                  GET_RPS_NAME, &
                  R_CURVE
      INTEGER (KIND=2) :: CURRENT_RECORD, &
                  DELETE, &
                  GET_RPS_PROGRAM,RPS_PROGRAM_RECORDS, &
                  R_TRANS_GROUP, &
                  NUM_RPS_PROGRAMS, &
                  SAVE_NUM_RPS_PROGRAMS=0, &
                  GET_MAX_IN_RPS_PROGRAMS, &
                  ACTIVE_RPS=0, &
                  NUMBER_OF_RPS_PROGRAMS, &
                  MAX_PROGRAM_NUMBER=0, &
                  PROGRAM_INDEX(:), &
                  PROGRAM_POSITION(:), &
                  GET_RPS_PROGRAM_POSITION, &
                  R_RPS_NO, &
                  R_RPS_COUNT, &
                  R_PM, &
                  R_INDEX, &
                  I
      INTEGER (KIND=2) :: &
                  J, &
                  COUNT_RPS, &
                  R_YEAR, &
                  R_ENDPOINT, &
                  RPS_PROGRAMS_VAR_NUM, &
                  RPS_PROGRAMS_NO=0, &
                  RPS_PROGRAMS_RPT_HEADER, &
                  R_RPS_POINTER, &
                  TEMP_I2
      INTEGER (KIND=4) :: RPS_PROGRAMS_REC
      REAL (KIND=4) ::  Initial_Bank(:), &
                        Maximum_Bank(:), &
                        Alt_Compliance_Price(:), &
                        Annual_Requirement(:,:), &
                        REC_Price(:), &
                        LOCAL_YEAR, &
                        LAST_YEAR_BANK(:), &
                        R_ENRG, &
                        R_CAP, &
                        ACP_GEN, &
                        LOCAL_MAXIMUM_BANK, &
                        LOCAL_REC_PRICE(:), &
                        MRX_GEN_WEIGHTED_REC_PRICE(:), &
                        R_GEN_WEIGHTED_REC_PRICE, &
                        MRX_REC_PRICE(:), &
                        LOCAL_ALT_PRICE(:), &
                        R_ALT_PRICE
      REAL (KIND=4) ::  &
                        LOCAL_ALT_PAYMENT, &
                        LOCAL_REC_REVENUE, &
                        LOCAL_STARTING_BALANCE, &
                        LOCAL_ENDING_BALANCE(:), &
                        NEXT_ITER_REQUIREMENT(:), &
                        MRX_TEST_REQUIREMENT(:), &
                        GET_ANNUAL_VECTOR_VALUE, &
                        ESCALATED_MONTHLY_VALUE, &
                        GET_LOCAL_ENDING_BALANCE, &
                        GET_RPS_REV_BY_OPTION, &
                        PUT_RPS_PRICE_BY_OPTION, &
                        PUT_RPS_PRICE_BY_PROGRAM, &
                        TEMP_R4, &
                        GET_QUALIFYING_GEN_DB, &
                        GET_ANNUAL_RPS_PROG_REQ, &
                        GET_RPS_ALT_COMPLIANCE_PRICE
      INTEGER (KIND=4) :: Program_Number(:)
      integer :: ios
      LOGICAL (KIND=4) :: R_SAVE_VALUES
      CHARACTER (LEN=3) :: Program_Active(:)
      CHARACTER (LEN=50) :: Comment,Program_Name(:),R_RPS_NAME
      ALLOCATABLE :: Initial_Bank, &
                     Maximum_Bank, &
                     Alt_Compliance_Price, &
                     Annual_Requirement, &
                     REC_Price, &
                     Program_Number, &
                     Program_Active, &
                     Program_Name, &
                     PROGRAM_INDEX, &
                     PROGRAM_POSITION, &
                     LAST_YEAR_BANK, &
                     LOCAL_ENDING_BALANCE, &
                     LOCAL_ALT_PRICE, &
                     LOCAL_REC_PRICE, &
                     MRX_GEN_WEIGHTED_REC_PRICE, &
                     MRX_REC_PRICE, &
                     NEXT_ITER_REQUIREMENT, &
                     MRX_TEST_REQUIREMENT
!
!
! END DATA DECLARATIONS
!
!
         MANAGE_RPS_PROGRAM_FORECASTS = .FALSE.
         SAVE_RPS_PROGRAM_FILE_ACTIVE = .FALSE.
         NUM_RPS_PROGRAMS = 0


!
!

!
         IF(.NOT. RPS_PROGRAM_FILE_EXISTS) RETURN
!
         CALL GET_MAX_RPS_PROGRAMS(NUM_RPS_PROGRAMS)
!
         SAVE_NUM_RPS_PROGRAMS = NUM_RPS_PROGRAMS
!
         IF(NUM_RPS_PROGRAMS < 1) RETURN
!
         CALL OPEN_RPS_PROGRAM_FILE
!
         IF(ALLOCATED(Initial_Bank)) &
              DEALLOCATE(Initial_Bank, &
                        Maximum_Bank, &
                        Alt_Compliance_Price, &
                        Annual_Requirement, &
                        REC_Price, &
                        Program_Number, &
                        Program_Active, &
                        Program_Name, &
                        PROGRAM_INDEX, &
                        LAST_YEAR_BANK, &
                        LOCAL_ENDING_BALANCE, &
                        LOCAL_ALT_PRICE, &
                        LOCAL_REC_PRICE, &
                        MRX_GEN_WEIGHTED_REC_PRICE, &
                        MRX_REC_PRICE, &
                        NEXT_ITER_REQUIREMENT, &
                        MRX_TEST_REQUIREMENT)
         ALLOCATE(      Initial_Bank(NUM_RPS_PROGRAMS), &
                        Maximum_Bank(NUM_RPS_PROGRAMS), &
                        Alt_Compliance_Price(NUM_RPS_PROGRAMS), &
                        Annual_Requirement(NUM_RPS_PROGRAMS,30), &
                        REC_Price(NUM_RPS_PROGRAMS), &
                        Program_Number(NUM_RPS_PROGRAMS), &
                        Program_Active(NUM_RPS_PROGRAMS), &
                        Program_Name(NUM_RPS_PROGRAMS), &
                        PROGRAM_INDEX(NUM_RPS_PROGRAMS), &
                        LAST_YEAR_BANK(NUM_RPS_PROGRAMS), &
                        LOCAL_ENDING_BALANCE(NUM_RPS_PROGRAMS), &
                        LOCAL_ALT_PRICE(NUM_RPS_PROGRAMS), &
                        LOCAL_REC_PRICE(NUM_RPS_PROGRAMS), &
                        MRX_GEN_WEIGHTED_REC_PRICE(NUM_RPS_PROGRAMS), &
                        MRX_REC_PRICE(NUM_RPS_PROGRAMS), &
                        NEXT_ITER_REQUIREMENT(NUM_RPS_PROGRAMS), &
                        MRX_TEST_REQUIREMENT(NUM_RPS_PROGRAMS))
!
         ACTIVE_RPS = 1
         MAX_PROGRAM_NUMBER = 0
         PROGRAM_INDEX = 0
         DO CURRENT_RECORD = 1, NUM_RPS_PROGRAMS
            READ(10,REC=CURRENT_RECORD) DELETE, &
                        Program_Name(ACTIVE_RPS), &
                        Program_Number(ACTIVE_RPS), &
                        Program_Active(ACTIVE_RPS), &
                        Initial_Bank(ACTIVE_RPS), &
                        Maximum_Bank(ACTIVE_RPS), &
                        Alt_Compliance_Price(ACTIVE_RPS), &
                        (Annual_Requirement(ACTIVE_RPS,I),I=1,30), &
                        COMMENT, &
                        REC_Price(ACTIVE_RPS)
            IF(Program_Active(ACTIVE_RPS) /= 'Yes') CYCLE

            MAX_PROGRAM_NUMBER = MAX(INT(MAX_PROGRAM_NUMBER,2),INT(Program_Number(ACTIVE_RPS),2))
            LAST_YEAR_BANK(ACTIVE_RPS) = Initial_Bank(ACTIVE_RPS)
            ACTIVE_RPS = ACTIVE_RPS + 1
         ENDDO
         ACTIVE_RPS = ACTIVE_RPS - 1
         IF(ACTIVE_RPS > 0) THEN

! 080722. ADDED OW AND HB (18,19)
!  Arrays below - description:
! First element (active_rps):
! Second element:  Prime Mover
! Third element: Mode - capacity or energy
! 080722. ADDED OW AND HB (18,19)
! 030124: JTR added H2 and CS.
! TODO:  Add qualifying_gen_db, grx_qual_gen_db, save_grx_qual_gen_db
!   arrays to capacity_arrays module


            if(allocated(program_position)) then
                deallocate(program_position)
            endif

            ALLOCATE(PROGRAM_POSITION(MAX_PROGRAM_NUMBER), stat=ios)
            call check_alloc("msgmmrev:0001", "PROGRAM_POSITION", ios) 
            call allocate_grx_db_arrays(ACTIVE_RPS)

            PROGRAM_POSITION = 0
            DO CURRENT_RECORD = 1, ACTIVE_RPS
               PROGRAM_POSITION(Program_Number(CURRENT_RECORD)) = CURRENT_RECORD
            ENDDO
!            QUALIFYING_GEN_DB = 0.0
            GRX_QUAL_GEN_DB = 0.0
            SAVE_GRX_QUAL_GEN_DB = 0.0
            MANAGE_RPS_PROGRAM_FORECASTS = .TRUE.
            SAVE_RPS_PROGRAM_FILE_ACTIVE = .TRUE.
         ENDIF
!
         CALL CLOSE_RPS_PROGRAM_FILE()
!
!
      RETURN

      ENTRY GET_RPS_NAME(R_RPS_NAME,R_RPS_NO)

         IF(R_RPS_NO > MAX_PROGRAM_NUMBER .OR. R_RPS_NO < 1) THEN
            R_RPS_NAME = '  '
            GET_RPS_NAME = .FALSE.
         ELSE
            R_RPS_NAME = Program_Name(R_RPS_NO)
            GET_RPS_NAME = .TRUE.
         ENDIF
      RETURN

      ENTRY GET_RPS_PROGRAM_POSITION(R_RPS_NO,R_RPS_COUNT)

         IF(R_RPS_NO < 0) THEN ! ASSUME A VECTOR
! 091820. DEPENDS ON COUNT.
            I = R_RPS_COUNT ! ONLY GRABS THEN FIRST VALUE
            COUNT_RPS = INT(GET_ANNUAL_VECTOR_VALUE(R_RPS_NO,I),2)
! 062721
         ELSEIF(R_RPS_NO > 0 .AND. R_RPS_COUNT > 1) THEN
            COUNT_RPS = 0
         ELSE
            COUNT_RPS = R_RPS_NO
         ENDIF
         IF(COUNT_RPS > MAX_PROGRAM_NUMBER .OR. COUNT_RPS < 1) THEN
            GET_RPS_PROGRAM_POSITION = 0
         ELSE
            GET_RPS_PROGRAM_POSITION = PROGRAM_POSITION(COUNT_RPS)
         ENDIF
      RETURN

      ENTRY RPS_PROGRAMS_ANNUAL_INIT()

         IF(ACTIVE_RPS > 0) THEN
            QUALIFYING_GEN_DB = 0.0
            RPS_PROGRAMS_ANNUAL_INIT = .TRUE.
         ELSE
            RPS_PROGRAMS_ANNUAL_INIT = .FALSE.
         ENDIF
      RETURN

      ENTRY NUMBER_OF_RPS_PROGRAMS

         IF(SAVE_NUM_RPS_PROGRAMS > 0) THEN
            NUMBER_OF_RPS_PROGRAMS = ACTIVE_RPS
         ELSE
            NUMBER_OF_RPS_PROGRAMS = 0
         ENDIF
      RETURN

      ENTRY GET_QUALIFYING_GEN_DB(R_RPS_NO,R_PM,R_INDEX)

         IF(R_RPS_NO /= 0 .AND. R_PM /= 13 .AND. SAVE_RPS_PROGRAM_FILE_ACTIVE) THEN
            GET_QUALIFYING_GEN_DB = QUALIFYING_GEN_DB(R_RPS_NO,R_PM,R_INDEX)
         ELSE
            GET_QUALIFYING_GEN_DB = 0.0
         ENDIF
      RETURN

      ENTRY PUT_RPS_ENRG_CAP(R_RPS_NO,R_PM,R_ENRG,R_CAP)

         IF(R_RPS_NO /= 0 .AND. R_PM /= 13 .AND. SAVE_RPS_PROGRAM_FILE_ACTIVE) THEN
            I = 1
            STOP_RPS = .FALSE.
            DO
               IF(STOP_RPS) EXIT
               IF(R_RPS_NO > 0) THEN
                  STOP_RPS = .TRUE.
                  COUNT_RPS = R_RPS_NO
               ELSE
                  COUNT_RPS = INT(GET_ANNUAL_VECTOR_VALUE(R_RPS_NO,I),2)
!
               ENDIF
               IF(COUNT_RPS > 0 .AND. COUNT_RPS <= MAX_PROGRAM_NUMBER) THEN
! DOUBLE INDEX
                  COUNT_RPS = PROGRAM_POSITION(COUNT_RPS)
                  IF(COUNT_RPS > 0 .AND. COUNT_RPS <= ACTIVE_RPS) THEN

                     QUALIFYING_GEN_DB(COUNT_RPS,R_PM,1) = QUALIFYING_GEN_DB(COUNT_RPS,R_PM,1) + R_ENRG
                     QUALIFYING_GEN_DB(COUNT_RPS,R_PM,2) = QUALIFYING_GEN_DB(COUNT_RPS,R_PM,2) + R_CAP
!
                     QUALIFYING_GEN_DB(COUNT_RPS,0,1) = QUALIFYING_GEN_DB(COUNT_RPS,0,1) + R_ENRG
                     QUALIFYING_GEN_DB(COUNT_RPS,0,2) = QUALIFYING_GEN_DB(COUNT_RPS,0,2) + R_CAP
                     IF(I < 30) THEN
                        I = I + 1
                     ELSE
                        STOP_RPS = .TRUE.
                     ENDIF
                  ELSE
                     STOP_RPS = .TRUE.
                  ENDIF
               ELSE
                  STOP_RPS = .TRUE.
               ENDIF
            ENDDO
            PUT_RPS_ENRG_CAP = .TRUE.
         ELSE
            PUT_RPS_ENRG_CAP = .FALSE.
         ENDIF
      RETURN

      ENTRY GET_RPS_REV_BY_OPTION(R_RPS_NO,R_ENRG,R_YEAR)

         GET_RPS_REV_BY_OPTION = 0.0
         IF(R_RPS_NO /= 0 .AND. SAVE_RPS_PROGRAM_FILE_ACTIVE) THEN
            I = 1
            STOP_RPS = .FALSE.
            DO
               IF(STOP_RPS) EXIT
               IF(R_RPS_NO > 0) THEN
                  STOP_RPS = .TRUE.
                  COUNT_RPS = R_RPS_NO
               ELSE
                  COUNT_RPS = INT(GET_ANNUAL_VECTOR_VALUE(R_RPS_NO,I),2)

               ENDIF
               IF(COUNT_RPS > 0 .AND. COUNT_RPS <= MAX_PROGRAM_NUMBER) THEN
! DOUBLE INDEX
                  COUNT_RPS = PROGRAM_POSITION(COUNT_RPS)
                  IF(COUNT_RPS > 0 .AND. COUNT_RPS <= ACTIVE_RPS) THEN
                     TEMP_R4 = Annual_Requirement(COUNT_RPS,R_YEAR) - &
                                (MRX_TEST_REQUIREMENT(COUNT_RPS)+ &
                                    QUALIFYING_GEN_DB(COUNT_RPS,0,1) - &
                                    SAVE_GRX_QUAL_GEN_DB(COUNT_RPS,0,1))
                     TEMP_R4 = MAX(0.0,MIN(TEMP_R4,R_ENRG))
                     IF(TEMP_R4 > 0.0001) THEN
                        GET_RPS_REV_BY_OPTION = GET_RPS_REV_BY_OPTION + TEMP_R4 * LOCAL_ALT_PRICE(COUNT_RPS)
                        MRX_TEST_REQUIREMENT(COUNT_RPS) = MRX_TEST_REQUIREMENT(COUNT_RPS) + TEMP_R4
                     ENDIF
                     IF(I < 30) THEN
                        I = I + 1
                     ELSE
                        STOP_RPS = .TRUE.
                     ENDIF
                  ELSE
                     STOP_RPS = .TRUE.
                  ENDIF
               ELSE
                  STOP_RPS = .TRUE.
               ENDIF
            ENDDO
            GET_RPS_REV_BY_OPTION = GET_RPS_REV_BY_OPTION * 0.000001
         ENDIF
      RETURN

      ENTRY PUT_RPS_PRICE_BY_PROGRAM(R_RPS_NO,R_ALT_PRICE, &
                                               R_GEN_WEIGHTED_REC_PRICE)

         PUT_RPS_PRICE_BY_PROGRAM = 0.0
         IF(R_RPS_NO /= 0 .AND. SAVE_RPS_PROGRAM_FILE_ACTIVE) THEN
            MRX_GEN_WEIGHTED_REC_PRICE(R_RPS_NO) = R_GEN_WEIGHTED_REC_PRICE
            MRX_REC_PRICE(R_RPS_NO) = R_ALT_PRICE
            PUT_RPS_PRICE_BY_PROGRAM = R_ALT_PRICE
         ENDIF
      RETURN

      ENTRY PUT_RPS_PRICE_BY_OPTION(R_RPS_NO,R_ALT_PRICE, &
                                    R_ENRG,R_CAP,R_PM,R_YEAR,R_CURVE, &
                                    R_GEN_WEIGHTED_REC_PRICE)

         PUT_RPS_PRICE_BY_OPTION = 0.0
         IF(R_RPS_NO /= 0 .AND. SAVE_RPS_PROGRAM_FILE_ACTIVE) THEN
            I = 1
            STOP_RPS = .FALSE.
            DO
               IF(STOP_RPS) EXIT
               IF(R_RPS_NO > 0) THEN
                  STOP_RPS = .TRUE.
                  COUNT_RPS = R_RPS_NO
               ELSE
                  COUNT_RPS = INT(GET_ANNUAL_VECTOR_VALUE(R_RPS_NO,I),2)

               ENDIF
               IF(COUNT_RPS > 0 .AND. COUNT_RPS <= MAX_PROGRAM_NUMBER) THEN
! DOUBLE INDEX
                  COUNT_RPS = PROGRAM_POSITION(COUNT_RPS)
                  IF(COUNT_RPS > 0 .AND. COUNT_RPS <= ACTIVE_RPS) THEN
! NEED TO ACCOUNT FOR MULTIPLE MARKETS CONTRIBUTING TO THE REC PRICE
                     GRX_QUAL_GEN_DB(COUNT_RPS,0,1) = GRX_QUAL_GEN_DB(COUNT_RPS,0,1) + R_ENRG
                     GRX_QUAL_GEN_DB(COUNT_RPS,R_PM,1) = GRX_QUAL_GEN_DB(COUNT_RPS,R_PM,1) + R_ENRG
                     GRX_QUAL_GEN_DB(COUNT_RPS,R_PM,2) = GRX_QUAL_GEN_DB(COUNT_RPS,R_PM,2) + R_CAP
                     GRX_QUAL_GEN_DB(COUNT_RPS,0,2) = GRX_QUAL_GEN_DB(COUNT_RPS,0,2) + R_CAP
                     TEMP_R4 = QUALIFYING_GEN_DB(COUNT_RPS,0,1) + &
                                 GRX_QUAL_GEN_DB(COUNT_RPS,0,1) - &
                                    SAVE_GRX_QUAL_GEN_DB(COUNT_RPS,0,1)
                     MRX_GEN_WEIGHTED_REC_PRICE(COUNT_RPS) = R_GEN_WEIGHTED_REC_PRICE
! 082120
                     MRX_REC_PRICE(COUNT_RPS) = R_ALT_PRICE

                     PUT_RPS_PRICE_BY_OPTION = MRX_REC_PRICE(COUNT_RPS)
                     IF(I < 30) THEN
                        I = I + 1
                     ELSE
                        STOP_RPS = .TRUE.
                     ENDIF
                  ELSE
                     STOP_RPS = .TRUE.
                  ENDIF
               ELSE
                  STOP_RPS = .TRUE.
               ENDIF
            ENDDO
         ENDIF
      RETURN

      ENTRY RESET_MRX_RPS_TEST_REQ()

         RESET_MRX_RPS_TEST_REQ = .FALSE.
         IF(.NOT. SAVE_RPS_PROGRAM_FILE_ACTIVE .OR. ACTIVE_RPS < 1) RETURN
         MRX_TEST_REQUIREMENT = 0.0 ! -1.0 * NEXT_ITER_REQUIREMENT
         MRX_REC_PRICE = -99999.0
      RETURN

      ENTRY GET_LOCAL_ENDING_BALANCE(R_YEAR,R_RPS_NO,R_ALT_PRICE)

         GET_LOCAL_ENDING_BALANCE = 0.
         R_ALT_PRICE = 0.
         IF(.NOT. SAVE_RPS_PROGRAM_FILE_ACTIVE .OR. R_RPS_NO < 1) RETURN
! 080920.
         GET_LOCAL_ENDING_BALANCE = &
                           QUALIFYING_GEN_DB(R_RPS_NO,0,1) - &
                                 SAVE_GRX_QUAL_GEN_DB(R_RPS_NO,0,1) - &
                                     Annual_Requirement(R_RPS_NO,R_YEAR)

         R_ALT_PRICE = LOCAL_ALT_PRICE(R_RPS_NO)
      RETURN

      ENTRY IS_IN_RPS_PROGRAM(R_RPS_COUNT,R_RPS_NO)

         IS_IN_RPS_PROGRAM = .FALSE.
! TEST FOR VECTOR
         I = 1
         STOP_RPS = .FALSE.
         DO
            IF(STOP_RPS) EXIT
            IF(R_RPS_NO > 0) THEN
               STOP_RPS = .TRUE.
               COUNT_RPS = R_RPS_NO
            ELSE
               COUNT_RPS = INT(GET_ANNUAL_VECTOR_VALUE(R_RPS_NO,I),2)

            ENDIF
            IF(COUNT_RPS > 0 .AND. COUNT_RPS <= MAX_PROGRAM_NUMBER) THEN
! DOUBLE INDEX
               COUNT_RPS = PROGRAM_POSITION(COUNT_RPS)
               IF(COUNT_RPS > 0 .AND. COUNT_RPS <= ACTIVE_RPS) THEN

                  IF(I < 30) THEN
                     I = I + 1
                  ELSE
                     STOP_RPS = .TRUE.
                  ENDIF ! 30 VALUE LIMIT
               ELSE
                  STOP_RPS = .TRUE.
               ENDIF ! BOUNDS OFF
            ELSE
               STOP_RPS = .TRUE.
            ENDIF ! ALSO BOUNDS OFF
            IF(COUNT_RPS == R_RPS_COUNT) THEN
               IS_IN_RPS_PROGRAM = .TRUE.
            ENDIF
         ENDDO ! VECTOR
      RETURN

      ENTRY GRX_SAVE_RPS_PROGRAM_VALUES(R_SAVE_VALUES)
         ! r_save_values=asdf/.true.
         if ( allocated(save_grx_qual_gen_db)) THEN
             IF(R_SAVE_VALUES) THEN
                SAVE_GRX_QUAL_GEN_DB = 0.0
             ELSE
                SAVE_GRX_QUAL_GEN_DB = GRX_QUAL_GEN_DB
             ENDIF
         endif

         if(allocated(grx_qual_gen_db)) then
            GRX_QUAL_GEN_DB = 0.0
         endif

         GRX_SAVE_RPS_PROGRAM_VALUES = .TRUE.
      RETURN

      ENTRY GET_ANNUAL_RPS_PROG_REQ(R_RPS_NO,R_YEAR)

! ALREADY USING POSITION
         IF(R_RPS_NO > MAX_PROGRAM_NUMBER .OR. R_RPS_NO < 1) THEN
            GET_ANNUAL_RPS_PROG_REQ = 0
         ELSE
            GET_ANNUAL_RPS_PROG_REQ = Annual_Requirement(R_RPS_NO,R_YEAR)
         ENDIF
      RETURN

      ENTRY GET_RPS_ALT_COMPLIANCE_PRICE(R_RPS_NO,R_YEAR)

         IF(R_RPS_NO > ACTIVE_RPS .OR. R_RPS_NO < 1) THEN
            GET_RPS_ALT_COMPLIANCE_PRICE = 999999.
         ELSE
            I = R_RPS_NO
            IF(Alt_Compliance_Price(I) < -0.01) THEN
               GET_RPS_ALT_COMPLIANCE_PRICE = &
                     ESCALATED_MONTHLY_VALUE(LOCAL_ALT_PRICE(I), &
                               ABS(INT(Alt_Compliance_Price(I),2)), &
                                               R_YEAR,INT(1,2),INT(1,2))
            ELSE
               GET_RPS_ALT_COMPLIANCE_PRICE = Alt_Compliance_Price(I)
            ENDIF
         ENDIF
      RETURN

      ENTRY RPS_PROGRAMS_ANNUAL_CALC(R_YEAR)

         RPS_PROGRAMS_ANNUAL_CALC = .FALSE.
         IF(.NOT. SAVE_RPS_PROGRAM_FILE_ACTIVE) RETURN
!
         IF(YES_RPS_PROGRAMS_REPORT() .AND. RPS_PROGRAMS_REPORT_NOT_OPEN) THEN
            RPS_PROGRAMS_VAR_NUM = 27 ! 9
            RPS_PROGRAMS_NO = RPS_PROGRAMS_RPT_HEADER( &
                                                   RPS_PROGRAMS_VAR_NUM, &
                                                   RPS_PROGRAMS_REC)
            RPS_PROGRAMS_REPORT_NOT_OPEN = .FALSE.
         ENDIF
!
         LOCAL_YEAR = FLOAT(R_YEAR + get_BASE_YEAR())
!
         DO I = 1, ACTIVE_RPS
            LOCAL_MAXIMUM_BANK = Maximum_Bank(I)
            IF(Maximum_Bank(I) < -0.01) THEN
               LOCAL_MAXIMUM_BANK = &
                     ESCALATED_MONTHLY_VALUE(LOCAL_MAXIMUM_BANK, &
                                         ABS(INT(Maximum_Bank(I),2)), &
                                               R_YEAR,INT(1,2),INT(1,2))
            ENDIF
            LOCAL_REC_PRICE(I) = REC_Price(I)
            IF(REC_Price(I) < -0.01) THEN
               LOCAL_REC_PRICE(I) = &
                     ESCALATED_MONTHLY_VALUE(LOCAL_REC_PRICE(I), &
                                         ABS(INT(REC_Price(I),2)), &
                                               R_YEAR,INT(1,2),INT(1,2))
            ENDIF
            LOCAL_ALT_PRICE(I) = Alt_Compliance_Price(I)
            IF(Alt_Compliance_Price(I) < -0.01) THEN
               LOCAL_ALT_PRICE(I) = &
                     ESCALATED_MONTHLY_VALUE(LOCAL_ALT_PRICE(I), &
                               ABS(INT(Alt_Compliance_Price(I),2)), &
                                               R_YEAR,INT(1,2),INT(1,2))
            ENDIF
            IF(R_YEAR == 1) THEN
               LOCAL_STARTING_BALANCE = Initial_Bank(I)
            ELSE
! NEED CARRY-FORWARD BALANCE
               LOCAL_STARTING_BALANCE = 0.0
            ENDIF
            IF(LOCAL_ALT_PRICE(I) < 0.00001) THEN
               ACP_GEN = 0.0
            ELSE ! LOCAL_ALT_PRICE(I) > 0.0
               ACP_GEN = MAX(0.0,Annual_Requirement(I,R_YEAR) - &
                                      LOCAL_STARTING_BALANCE - &
                                              QUALIFYING_GEN_DB(I,0,1))
               IF(MRX_REC_PRICE(I) > -1.0) THEN ! MRX PRICE
                  LOCAL_REC_PRICE(I) = MRX_REC_PRICE(I)
               ELSEIF(ACP_GEN > 0.00001) THEN ! DEFICIT
                  LOCAL_REC_PRICE(I) = LOCAL_ALT_PRICE(I)
               ELSE ! SURPLUS
                  LOCAL_REC_PRICE(I) = 0.0
               ENDIF
            ENDIF
            NEXT_ITER_REQUIREMENT(I) = &
                   LOCAL_STARTING_BALANCE + &
                             QUALIFYING_GEN_DB(I,0,1) - &    ! + ACP_GEN -
                                           Annual_Requirement(I,R_YEAR)
            LOCAL_ENDING_BALANCE(I) = &
               MAX(0.0, &
                     LOCAL_STARTING_BALANCE + &
                           QUALIFYING_GEN_DB(I,0,1) + ACP_GEN - &
                                           Annual_Requirement(I,R_YEAR))
            LOCAL_ALT_PAYMENT = LOCAL_ALT_PRICE(I) * ACP_GEN
            LOCAL_REC_REVENUE = LOCAL_REC_PRICE(I) * QUALIFYING_GEN_DB(I,0,1)
            IF(YES_RPS_PROGRAMS_REPORT()) THEN
               RPS_PROGRAMS_REC = RPTREC(RPS_PROGRAMS_NO)
               WRITE(RPS_PROGRAMS_NO,REC=RPS_PROGRAMS_REC) &
                     PRT_ENDPOINT(), &
                     LOCAL_YEAR, &
                     Program_Name(I), &
                     Annual_Requirement(I,R_YEAR), &
                     (QUALIFYING_GEN_DB(I,J,1),J=0,17), &
                     ACP_GEN, &
                     LOCAL_REC_PRICE(I), &
                     LOCAL_ALT_PRICE(I), &
                     LOCAL_ALT_PAYMENT, &
                     LOCAL_REC_REVENUE, &
                     LOCAL_STARTING_BALANCE, &
                     LOCAL_ENDING_BALANCE(I), &
                     MRX_GEN_WEIGHTED_REC_PRICE(I)
               RPS_PROGRAMS_REC = RPS_PROGRAMS_REC + 1
            ENDIF
         ENDDO
!
         RPS_PROGRAMS_ANNUAL_CALC = .TRUE.
      RETURN
      END
!
!

