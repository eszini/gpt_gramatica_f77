
      RECURSIVE SUBROUTINE FA_OBJT
      use end_routine, only: end_program, er_message
      use grx_planning_routines
      USE IREC_ENDPOINT_CONTROL
      use SpinDriftLib
      USE PROD_ARRAYS_DIMENSIONS
      USE SIZECOM
      use globecom
      use mod_base_year


 ! NOTE THAT ACTUAL FILES IS +1
      INTEGER (KIND=2), PARAMETER :: FUTURE_ASSETS_FILES=1
      CHARACTER (LEN=256) :: FA_FILE_BASE_NAMES(0:FUTURE_ASSETS_FILES)
      CHARACTER (LEN=2) :: FA_FILE_CODES(0:FUTURE_ASSETS_FILES) &
      	=(/'FA','FX'/)
      CHARACTER (LEN=10) :: FA_FILE_BINARY_NAMES &
      	(0:FUTURE_ASSETS_FILES)	=(/'FUAST     ','FACAPEX   '/)
      LOGICAL (KIND=4), SAVE ::FUTURE_ASSET_FILE_EXISTS &
      	(0:FUTURE_ASSETS_FILES)
      CHARACTER (LEN=40) :: R_CAP_HYPERCUBE
      INTEGER (KIND=2) :: GET_SCENARIO_INDEX,SCENARIO_INDEX,HYPER_YR
      REAL (KIND=4) :: GET_SCENARIO_BY_INDEX
      CHARACTER (LEN=2) :: FUASTOL(0:FUTURE_ASSETS_FILES)=(/'BC','BC'/)
      INTEGER (KIND=4) :: FILE_NUMBER,FILE_ID
      INTEGER (KIND=2) :: ASSET_CLASS_NUM,ASSET_CLASS_VECTOR
      INTEGER (KIND=2) :: R_NUM_OF_CLASSES,R_MAX_CLASS_NUM
!
      INTEGER (KIND=2) :: NUM_OF_OL_ASSET_CLASSES=0, &
      	MAX_OL_CLASS_ID_NUM=0
      INTEGER (KIND=2) :: NUM_OF_BC_ASSET_CLASSES=0, &
      	MAX_BC_CLASS_ID_NUM=0
      INTEGER (KIND=2) :: BC_FA_CLASS_POINTER(:), &
      	OL_FA_CLASS_POINTER(:),TEMP_ASSET_CLASS_POINTER(:)
      ALLOCATABLE :: BC_FA_CLASS_POINTER, &
      	OL_FA_CLASS_POINTER,TEMP_ASSET_CLASS_POINTER
      CHARACTER (LEN=2) :: CAPACITY_PLANNING_METHOD
      INTEGER (KIND=2) :: R_ASSET_CLASS,R_ASSET_CLASS_VECTOR
!
      INTEGER (KIND=2) :: IREC,AFDC_CAP_VECTOR,UNIT_NO, &
                RECORDS_IN_BASE_FILE,LAST_UNIT_NO_OPENED, &
                R_LAST_RECORD,RECORDS_IN_ORG_BASE_FILE, &
                FLEX_RPT_UNIT,FLEX_PLANNING_ACTION_HEADER, &
                SAVE_FN_RECORDS_IN_BASE_FILE=0,G_RECORDS_IN_BASE_FILE
      INTEGER :: IOS,IOS_BASE,FLEX_RPT_REC
      INTEGER (KIND=2) :: R_CURRENT_YEAR,R_OPTION_TIMES_DELAYED
      INTEGER (KIND=2) :: DELETE,INUNIT,LRECL=1024,REC_LEN,R_REC
      INTEGER :: ACCTNO
      LOGICAL (KIND=1) :: R_LOGICAL_1,THERE_ARE_NEW_ASSESTS, &
                R_ABANDON_IT, &
                FROM_ADD_NEXT_UNIT_FINANCIAL, &
                FLEX_PLANNING_RPT_NOT_OPEN=.TRUE.
      CHARACTER (LEN=1) :: FUASTFIL,VOID_CHR
      CHARACTER (LEN=5) :: OVERLAY_FAMILY_NAME
      CHARACTER (LEN=30) :: DESC,OPTION_NAME*38,DELAYED_STR*12,TEMP_DESC
      CHARACTER (LEN=30) :: COMMENT
      CHARACTER (LEN=15) :: LEFT_JUSTIFY_I2_IN_STR
      INTEGER (KIND=2) :: TOTAL_UNITS_ADDED=0
      CHARACTER (LEN=256) :: FILE_NAME
      CHARACTER (LEN=256) :: BASE_FILE_DIRECTORY,OUTPUT_DIRECTORY
      CHARACTER (LEN=2) :: RESOURCE_TYPE
      REAL :: TXNORM_OUT,TXNORM,SALVAGE_VALUE,REAL_LEAD_TIME, &
        TAX_BASIS_ADJUSTOR
      CHARACTER (LEN=40) :: LAG_STR,CASH_STR
      REAL :: R_PERCENT_COMPLETED
      INTEGER (KIND=2) :: CONSTRUCTION_YEARS_COMPLETED,R_REC_POSITION
      INTEGER (KIND=2) :: R_ON_LINE_YR
      LOGICAL (KIND=4) :: FILE_EXISTS,FILE_OPENED
      LOGICAL (KIND=1) :: FIRST_FINANCIAL_ADDITION=.TRUE.
      LOGICAL (KIND=1) :: FUTURE_ASSET_FILE_OPEN=.FALSE.
      LOGICAL (KIND=1) :: MODEL_ADDED_ASSET_FILE_OPEN=.FALSE.
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER (LEN=1024) :: RECLN
! DECLARATION FOR /FUTURE ASSETS FILE/
      INTEGER (KIND=2) :: AFDCSW,ABYEAR,ABMETH,FIRSTYR, &
      	CONSTRUCTION_ESCALATION_VECTOR,AFDCPERIODS,SERVICEMO
      REAL (KIND=4) :: CONSTRUCT_ESCALAT_VECTOR_RATE
! ADD_FINANCIAL_RECORD VARIABLES
      INTEGER (KIND=2) :: OPERATION_LIFE,LEAD_TIME, &
      	INVESTMENT_DATA_POINTER,ESCALATION_VECTOR, &
      	ON_LINE_MO,R_ON_LINE_MO
      REAL :: CONSTRUCTION_COSTS, &
      BOKLFX,R_CONSTRUCTION_COSTS,RR_CONSTRUCTION_COSTS
      INTEGER (KIND=2) :: AI_REMAINING_LIFE, &
      SET_AI_CL_REMAINING_LIFE,SET_AI_EL_REMAINING_LIFE
!     ADDED MIDAS GOLD 7/24/91
      INTEGER (KIND=2) :: CASH_VECT,PLANT_VECT,PROJECTS_IN_PROCESS
      REAL :: ADRLIFE,CONSTRUCTION_PERIOD,PV_PLANT_COST,REGULATORY_ALLOCATOR
      REAL :: DBRATE,CWIP1,CWIPRB,AFUDC1,AFDCB1,BOKLF,TAXLF, &
      ABTAX,WOYRS,OHRATE,TAXEXP,PLANT(15),CASH(15)
      CHARACTER (LEN=21) :: FILE_TYPE='Future Asset Accounts'
      CHARACTER (LEN=4) :: DEPMET
      CHARACTER (LEN=1) :: ACCOUNT_ACTIVE,ABACCT,DATA_TYPE, &
      TAKE_BONUS_TAX_DEPRECIATION
      LOGICAL (KIND=1) :: R_FUASTOL
!
      INTEGER (KIND=2) :: ON_LINE_YR,IOFFSET,IEND,I,DELAY_PERIODS,YR
      REAL :: CEP,CE,ESCALATION_RATES,CURRENT_VALUE,TEMP_CWIP, &
           ESCALATION_MULTIPLIER,TEMP_HOLDING_ARRAY,CASH_TO_DATE, &
           DELAY_CHARGE_RATE,DELAY_CHARGE
      REAL :: ACCEL_CHARGE, &
              ACCELERATE_FIXED_CHARGE, &
              ACCELERATE_CHARGE_PERCENT, &
              DELAY_FIXED_CHARGE, &
              DELAY_CHARGE_PERCENT, &
              CANCEL_CHARGE, &
              ABANDONMENT_FIXED_CHARGE, &
              ABANDONMENT_CHARGE_PERCENT, &
              PERCENT_CASH_EARNS_ITC, &  !81
              PROJECT_ITC_RATE          !82
      REAL :: TEMP_REAL,LAST_YEAR_CASH,CASH_ADJUSTMENT
      ALLOCATABLE :: CEP(:),CE(:),ESCALATION_RATES(:),TEMP_HOLDING_ARRAY(:)
      LOGICAL (KIND=1) :: VOID_LOG,RETURN_A_VECTOR_FOR_ALL_YEARS
      INTEGER (KIND=2) :: PROJECTS_ACCEPTED
!
      LOGICAL (KIND=1) :: LAHEY_LF95
      CHARACTER (LEN=30) :: SCREEN_OUTPUT
      INTEGER (KIND=2) :: OPTIONS_UNIT_NUM
      LOGICAL (KIND=1) :: OPTIONS_FINANCIAL_FILE_EXISTS
      CHARACTER (LEN=1) :: BOOK_DEP_METHOD
      SAVE BC_FA_CLASS_POINTER,OL_FA_CLASS_POINTER
      SAVE RECORDS_IN_BASE_FILE,IREC,PROJECTS_IN_PROCESS, &
      	LAST_UNIT_NO_OPENED,RECORDS_IN_ORG_BASE_FILE, &
           THERE_ARE_NEW_ASSESTS,FLEX_RPT_UNIT, &
           PROJECTS_ACCEPTED,FLEX_RPT_REC
      SAVE OPTIONS_FINANCIAL_FILE_EXISTS
      CHARACTER (LEN=20) :: TAB_NAME
!
! WVPA DATA ITEMS
!
      INTEGER (KIND=2) :: PROJECT_COMPLETION_MO, &
      	PROJECT_COMPLETION_YR,WVPA_COMPANY_ID
      INTEGER :: WVPA_SUB_ACCOUNT_NUMBER,WVPA_BUDGET_ITEM_NUM
      CHARACTER (LEN=15) :: WVPA_DEPARTMENT_HEAD
      REAL :: OM_CAPITIALIZED_CASH,OM_CAPITIALIZED_NON_CASH
      CHARACTER (LEN=10) :: WVPA_WORK_ORDER_NUMBER
      CHARACTER (LEN=40) :: OVN_CAPITAL_COST_STOCASTIC
      LOGICAL (KIND=4) :: R_FILE_EXISTS

!
!          ROUTINE TO CONVERT METAFILE FILES TO DIRECT ACESS BINARY
!          COPYRIGHT (C) 1983, 84, 85  M.S. GERBER & ASSOCIATES, INC.
!

!
! CONVERT THE FUTURE-ASSET-COMMITTED FILE
      ENTRY FA_MAKEBIN
!
      VOID_CHR = FUASTFIL(FA_FILE_BASE_NAMES)
      ALLOCATE(TEMP_ASSET_CLASS_POINTER(1024))
      TEMP_ASSET_CLASS_POINTER = 0
      FUTURE_ASSET_FILE_EXISTS = .FALSE.
      DO FILE_ID = 0, FUTURE_ASSETS_FILES
         FILE_NAME = TRIM(BASE_FILE_DIRECTORY())// &
         	TRIM(FA_FILE_CODES(FILE_ID))//"B"// &
         	TRIM(FA_FILE_BASE_NAMES(FILE_ID))//".DAT"
         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
         IREC = 0
         IF(.NOT. FILE_EXISTS) CYCLE
!
!
!
         SCREEN_OUTPUT = TRIM(FILE_TYPE)//'-'//TRIM(FA_FILE_BASE_NAMES(FILE_ID))
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=TRIM(OUTPUT_DIRECTORY())//"BC"// &
                            TRIM(FA_FILE_BINARY_NAMES(FILE_ID))//".BIN", &
                            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         READ(10,*) DELETE
         DO
            OHRATE = 0.
            TAXEXP = 0.
            AFDC_CAP_VECTOR = 0  
            PLANT_VECT = 0
            CASH_VECT = 0
            SALVAGE_VALUE = 0
            ABACCT = 'N'
            TXNORM = -99999
            ASSET_CLASS_NUM = 0 
            ASSET_CLASS_VECTOR = 0
            BOOK_DEP_METHOD = 'R' !emaining
            TAX_BASIS_ADJUSTOR = 100.
            LAG_STR = "100"
            CASH_STR = "0"
            DBRATE = 150.
            REGULATORY_ALLOCATOR = 100.
            ACCOUNT_ACTIVE = 'A'
            SERVICEMO = 7
            TAKE_BONUS_TAX_DEPRECIATION = 'N'
            WVPA_SUB_ACCOUNT_NUMBER = 0
            WVPA_DEPARTMENT_HEAD = 'WVPA'
            TAB_NAME = 'WVPA'
            PROJECT_COMPLETION_MO = 12
            PROJECT_COMPLETION_YR = 2050
            PV_PLANT_COST = 0.
            OM_CAPITIALIZED_CASH = 0.
            OM_CAPITIALIZED_NON_CASH = 0.
            CONSTRUCT_ESCALAT_VECTOR_RATE = 0.
            WVPA_WORK_ORDER_NUMBER = 'XXXX'
            WVPA_BUDGET_ITEM_NUM = 0
            DEPMET = " "
            ADRLIFE = 10.
            FIRSTYR = 0
            AFUDC1 = 0.
            BOKLF = 99.
            TAXLF = 99.
            DATA_TYPE = 'D'
            CWIPRB = 0.
            AFDCB1 = 0.
            AFDCSW = 2
            CONSTRUCTION_PERIOD = 3
            AFDCPERIODS = 0
            ABYEAR =  2100
            ABMETH = 0
            ABTAX = 0
            WOYRS = 99
            WVPA_COMPANY_ID = 1
            OVN_CAPITAL_COST_STOCASTIC = "Not Active"
            PERCENT_CASH_EARNS_ITC =0.   !81
            PROJECT_ITC_RATE = 0.          !82
!
            DO
               READ(10,1000,IOSTAT=IOS) RECLN
               IF(IOS /= 0 .OR. TRIM(RECLN) == ' ') EXIT
               IF(RECLN(1:1) == '7') THEN
                  READ(RECLN,*) DELETE,TAB_NAME
                  WVPA_DEPARTMENT_HEAD = TAB_NAME
                  EXIT
               ENDIF
               PLANT = -999999.
               CASH = -999999.
               RECLN = TRIM(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,ERR=200) DELETE,ACCTNO,DATA_TYPE,BOKLF, &
                  TAXLF,ADRLIFE,DEPMET,DBRATE,CWIP1,CWIPRB,AFUDC1, &
                  AFDCB1,AFDCSW,CONSTRUCTION_PERIOD,AFDCPERIODS,ABYEAR, &
                  ABMETH,TAKE_BONUS_TAX_DEPRECIATION, &
                  ABTAX,WOYRS,CASH_VECT,SERVICEMO,FIRSTYR, &
                  ACCOUNT_ACTIVE,OHRATE,TAXEXP,REGULATORY_ALLOCATOR, &
                  PV_PLANT_COST,CONSTRUCT_ESCALAT_VECTOR_RATE, &
                  PLANT,CASH,DESC,COMMENT,AFDC_CAP_VECTOR,PLANT_VECT, &
                  TXNORM,SALVAGE_VALUE, &
                  ASSET_CLASS_NUM,ASSET_CLASS_VECTOR,BOOK_DEP_METHOD, &
                  TAX_BASIS_ADJUSTOR,LAG_STR, &   ! 69 
                  CASH_STR, &  ! 70
                  WVPA_SUB_ACCOUNT_NUMBER,WVPA_DEPARTMENT_HEAD, & ! 72
                  PROJECT_COMPLETION_MO,PROJECT_COMPLETION_YR, & ! 74
                  WVPA_WORK_ORDER_NUMBER,WVPA_BUDGET_ITEM_NUM, &
                  OM_CAPITIALIZED_CASH, &  ! 77
                  OM_CAPITIALIZED_NON_CASH, & ! 78
                  WVPA_COMPANY_ID,OVN_CAPITAL_COST_STOCASTIC, & ! 80
                  PERCENT_CASH_EARNS_ITC, &  !81
                  PROJECT_ITC_RATE          !82
!
               IF(TXNORM == -99999) THEN
                  TXNORM_OUT = BOKLF
               ELSE
                  TXNORM_OUT = TXNORM
               ENDIF
               IF(ACCOUNT_ACTIVE /= 'N') ACCOUNT_ACTIVE = 'A'
             IF(.NOT. (DELETE >= 8 .OR. ACCOUNT_ACTIVE == 'N' )) THEN
                CALL SET_ASSET_CLASSES(ASSET_CLASS_NUM, &
                                        NUM_OF_BC_ASSET_CLASSES, &
                                        MAX_BC_CLASS_ID_NUM, &
                                        TEMP_ASSET_CLASS_POINTER)
               ENDIF
               IREC = IREC + 1
!
! RIPPLE DOWN PLANT AND CASH VALUES
!
               IF(PLANT(1) == -999999.) PLANT(1) = 0.
               DO I = 2, 15
                  IF(PLANT(I) == -999999.) PLANT(I) = PLANT(I-1)
               ENDDO
               IF(CASH(1) == -999999.) CASH(1) = 0.
               DO I = 2, 15
                  IF(CASH(I) == -999999.) CASH(I) = CASH(I-1)
               ENDDO
!
               WRITE(11,REC=IREC) DELETE,ACCTNO,DATA_TYPE,BOKLF, &
                     TAXLF,ADRLIFE,DEPMET,DBRATE,CWIP1,CWIPRB,AFUDC1, &
                     AFDCB1,AFDCSW,CONSTRUCTION_PERIOD,AFDCPERIODS,ABYEAR, &
                     ABMETH,ABACCT,ABTAX,WOYRS,CASH_VECT,SERVICEMO,FIRSTYR, &
                     ACCOUNT_ACTIVE,OHRATE,TAXEXP,REGULATORY_ALLOCATOR, &
                     PV_PLANT_COST,CONSTRUCT_ESCALAT_VECTOR_RATE, &
                     PLANT,CASH,DESC,AFDC_CAP_VECTOR,PLANT_VECT, &
                     TXNORM_OUT,SALVAGE_VALUE, &
                     ASSET_CLASS_NUM,ASSET_CLASS_VECTOR, &
                     BOOK_DEP_METHOD,TAX_BASIS_ADJUSTOR, &
                     LAG_STR,CASH_STR,TAKE_BONUS_TAX_DEPRECIATION, &
                     WVPA_SUB_ACCOUNT_NUMBER,WVPA_DEPARTMENT_HEAD, &
                     PROJECT_COMPLETION_MO,PROJECT_COMPLETION_YR, &
                     WVPA_WORK_ORDER_NUMBER,WVPA_BUDGET_ITEM_NUM, &
                     OM_CAPITIALIZED_CASH, &
                     OM_CAPITIALIZED_NON_CASH, &
                     WVPA_COMPANY_ID, &
                     OVN_CAPITAL_COST_STOCASTIC, &
                     PERCENT_CASH_EARNS_ITC, &  !81
                     PROJECT_ITC_RATE          !82
   

            ENDDO
            IF(IREC > 0) FUTURE_ASSET_FILE_EXISTS(FILE_ID) = .TRUE.
            IF(IOS /= 0) EXIT
         ENDDO
         CLOSE(10)

         CLOSE(11)
      ENDDO ! FA FILES
      IF(MAX_BC_CLASS_ID_NUM > 0) THEN
         ALLOCATE(BC_FA_CLASS_POINTER(MAX_BC_CLASS_ID_NUM))
         BC_FA_CLASS_POINTER = TEMP_ASSET_CLASS_POINTER(1:MAX_BC_CLASS_ID_NUM)
      ENDIF
      DEALLOCATE(TEMP_ASSET_CLASS_POINTER)

      RECORDS_IN_BASE_FILE = IREC
      RECORDS_IN_ORG_BASE_FILE = IREC
      IREC = 0
      RETURN

!          ROUTINE TO CREATE OVERLAY FILES
!          COPYRIGHT (C) 1984-88  M.S. GERBER & ASSOCIATES, INC.
!          COPYRIGHT (C) 1991-92  M.S. GERBER & ASSOCIATES, INC.
!

!
! OVERLAY THE FUTURE-ASSET-COMMITTED FILE

      ENTRY FA_MAKEOVL(OVERLAY_FAMILY_NAME,FILE_NUMBER)

      IF(.NOT. FUTURE_ASSET_FILE_EXISTS(FILE_NUMBER)) RETURN
      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = TRIM(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      FILE_NAME=TRIM(OUTPUT_DIRECTORY())// &
                       TRIM(FA_FILE_CODES(FILE_NUMBER))//"O"// &
                                     TRIM(OVERLAY_FAMILY_NAME)//".DAT"
      OPEN(10,FILE=FILE_NAME)
      READ(10,*) DELETE
      INUNIT = 12
      IF(FUASTOL(FILE_NUMBER) == 'BC') THEN
         OPEN(11,FILE=TRIM(OUTPUT_DIRECTORY())//"BC"// &
                    TRIM(FA_FILE_BINARY_NAMES(FILE_NUMBER))//".BIN", &
                            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         INUNIT = 11
      ENDIF
      OPEN(12,FILE=TRIM(OUTPUT_DIRECTORY())//"OL"// &
                    TRIM(FA_FILE_BINARY_NAMES(FILE_NUMBER))//".BIN", &
                            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      ALLOCATE(TEMP_ASSET_CLASS_POINTER(1024))
      TEMP_ASSET_CLASS_POINTER = 0
      NUM_OF_OL_ASSET_CLASSES = 0
      MAX_OL_CLASS_ID_NUM = 0
      IREC = 0
      DO
         DO
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(RECLN(1:1) == '7') EXIT
            IREC = IREC + 1
            READ(INUNIT,REC=IREC,IOSTAT=IOS_BASE) DELETE,ACCTNO, &
                 DATA_TYPE,BOKLF,TAXLF,ADRLIFE,DEPMET,DBRATE,CWIP1, &
                 CWIPRB,AFUDC1,AFDCB1,AFDCSW,CONSTRUCTION_PERIOD, &
                 AFDCPERIODS,ABYEAR,ABMETH,ABACCT,ABTAX,WOYRS,CASH_VECT, &
                 SERVICEMO,FIRSTYR,ACCOUNT_ACTIVE,OHRATE,TAXEXP, &
                 REGULATORY_ALLOCATOR,PV_PLANT_COST, &
                 CONSTRUCT_ESCALAT_VECTOR_RATE,PLANT,CASH, &
                 DESC,AFDC_CAP_VECTOR,PLANT_VECT,TXNORM,SALVAGE_VALUE, &
                 ASSET_CLASS_NUM,ASSET_CLASS_VECTOR, &
                 BOOK_DEP_METHOD,TAX_BASIS_ADJUSTOR,LAG_STR,CASH_STR, &
                 TAKE_BONUS_TAX_DEPRECIATION,WVPA_SUB_ACCOUNT_NUMBER, &
                 WVPA_DEPARTMENT_HEAD,PROJECT_COMPLETION_MO, &
                 PROJECT_COMPLETION_YR,WVPA_WORK_ORDER_NUMBER, &
                 WVPA_BUDGET_ITEM_NUM, &
                 OM_CAPITIALIZED_CASH, &
                 OM_CAPITIALIZED_NON_CASH, &
                 WVPA_COMPANY_ID, &
                 OVN_CAPITAL_COST_STOCASTIC, &
                 PERCENT_CASH_EARNS_ITC, &  !81
                 PROJECT_ITC_RATE          !82
     
            IF(IOS_BASE /= 0) THEN
               IREC = IREC - 1
               EXIT
            ENDIF
            IF(IOS == 0) THEN
               RECLN = TRIM(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,ERR=200) DELETE,ACCTNO,DATA_TYPE,BOKLF, &
                    TAXLF,ADRLIFE,DEPMET,DBRATE,CWIP1,CWIPRB,AFUDC1, &
                    AFDCB1,AFDCSW,CONSTRUCTION_PERIOD,AFDCPERIODS,ABYEAR, &
                    ABMETH,TAKE_BONUS_TAX_DEPRECIATION, &
                    ABTAX,WOYRS,CASH_VECT,SERVICEMO,FIRSTYR, &
                    ACCOUNT_ACTIVE,OHRATE,TAXEXP,REGULATORY_ALLOCATOR, &
                    PV_PLANT_COST,CONSTRUCT_ESCALAT_VECTOR_RATE, &
                    PLANT, &
                    CASH,TEMP_DESC,COMMENT,AFDC_CAP_VECTOR,PLANT_VECT, &
                    TXNORM,SALVAGE_VALUE, &
                    ASSET_CLASS_NUM,ASSET_CLASS_VECTOR, &
                    BOOK_DEP_METHOD,TAX_BASIS_ADJUSTOR, &
                    LAG_STR,CASH_STR,WVPA_SUB_ACCOUNT_NUMBER, &
                    WVPA_DEPARTMENT_HEAD,PROJECT_COMPLETION_MO, &
                    PROJECT_COMPLETION_YR,WVPA_WORK_ORDER_NUMBER, &
                    WVPA_BUDGET_ITEM_NUM,OM_CAPITIALIZED_CASH, &
                    OM_CAPITIALIZED_NON_CASH,WVPA_COMPANY_ID, &
                    OVN_CAPITAL_COST_STOCASTIC, &
                    PERCENT_CASH_EARNS_ITC, &  !81
                    PROJECT_ITC_RATE          !82

            ENDIF
            IF(.NOT. (DELETE >= 8 .OR. ACCOUNT_ACTIVE == 'N' )) THEN
               CALL SET_ASSET_CLASSES(ASSET_CLASS_NUM, &
                                      NUM_OF_OL_ASSET_CLASSES, &
                                      MAX_OL_CLASS_ID_NUM, &
                                      TEMP_ASSET_CLASS_POINTER)
            ENDIF
            WRITE(12,REC=IREC) DELETE,ACCTNO,DATA_TYPE,BOKLF,TAXLF, &
                  ADRLIFE,DEPMET,DBRATE,CWIP1,CWIPRB,AFUDC1,AFDCB1,AFDCSW, &
                  CONSTRUCTION_PERIOD,AFDCPERIODS,ABYEAR,ABMETH,ABACCT, &
                  ABTAX,WOYRS,CASH_VECT,SERVICEMO,FIRSTYR, &
                  ACCOUNT_ACTIVE,OHRATE, &
                  TAXEXP,REGULATORY_ALLOCATOR,PV_PLANT_COST, &
                  CONSTRUCT_ESCALAT_VECTOR_RATE,PLANT,CASH, &
                  DESC,AFDC_CAP_VECTOR,PLANT_VECT,TXNORM,SALVAGE_VALUE, &
                  ASSET_CLASS_NUM,ASSET_CLASS_VECTOR, &
                  BOOK_DEP_METHOD,TAX_BASIS_ADJUSTOR, &
                  LAG_STR,CASH_STR,TAKE_BONUS_TAX_DEPRECIATION, &
                  WVPA_SUB_ACCOUNT_NUMBER,WVPA_DEPARTMENT_HEAD, &
                  PROJECT_COMPLETION_MO,PROJECT_COMPLETION_YR, &
                  WVPA_WORK_ORDER_NUMBER,WVPA_BUDGET_ITEM_NUM, &
                  OM_CAPITIALIZED_CASH, &
                  OM_CAPITIALIZED_NON_CASH, &
                  WVPA_COMPANY_ID, &
                  OVN_CAPITAL_COST_STOCASTIC, &
                  PERCENT_CASH_EARNS_ITC, &  !81
                  PROJECT_ITC_RATE          !82
         ENDDO
         IF(IOS_BASE /= 0) EXIT
      ENDDO
      IREC = 0
      CLOSE(10)
      CLOSE(12)
      IF(FUASTOL(FILE_NUMBER) == 'BC') CLOSE(11)
      FUASTOL(FILE_NUMBER)= 'OL'
!
      IF(ALLOCATED(OL_FA_CLASS_POINTER)) DEALLOCATE(OL_FA_CLASS_POINTER)
      IF(MAX_OL_CLASS_ID_NUM > 0) THEN
         ALLOCATE(OL_FA_CLASS_POINTER(MAX_OL_CLASS_ID_NUM))
         OL_FA_CLASS_POINTER = &
         	TEMP_ASSET_CLASS_POINTER(1:MAX_OL_CLASS_ID_NUM)
      ENDIF
      DEALLOCATE(TEMP_ASSET_CLASS_POINTER)
      RETURN

  200 CALL MG_LOCATE_WRITE(20,0,TRIM(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from msgmmfa SIID206'
      call end_program(er_message)
!


      ENTRY RESET_FUASTOL

         TOTAL_UNITS_ADDED = 0
         FUASTOL = 'BC'
      RETURN

      ENTRY GET_FUAST_OL(R_FUASTOL)

         R_FUASTOL = .FALSE.
         DO FILE_ID = 0, FUTURE_ASSETS_FILES
            IF(FUASTOL(FILE_ID) == "OL") THEN
               R_FUASTOL = .TRUE.
               EXIT
            ENDIF
         ENDDO
      RETURN

      ENTRY OPEN_FA_OUT_FILE(UNIT_NO)

         FILE_NAME = TRIM(OUTPUT_DIRECTORY())//'BCFA_AST.BIN'
         DO FILE_ID = 0, FUTURE_ASSETS_FILES
            IF(FUASTOL(FILE_ID) == 'OL') THEN
               FILE_NAME = TRIM(OUTPUT_DIRECTORY())//'OLFA_AST.BIN'
               EXIT
            ENDIF
         ENDDO
         INQUIRE(FILE=FILE_NAME,OPENED=FILE_OPENED)
         IF(.NOT. FILE_OPENED) then
         	OPEN(UNIT_NO,FILE=FILE_NAME,ACCESS='DIRECT',RECL=256)
         end if
      RETURN

      ENTRY OPEN_NEW_ASSETS_OUT_FILE(UNIT_NO)


         FILE_NAME = TRIM(OUTPUT_DIRECTORY())//'BCFUASST.BIN'
         DO FILE_ID = 0, FUTURE_ASSETS_FILES
            IF(FUASTOL(FILE_ID) == 'OL') THEN
               FILE_NAME = TRIM(OUTPUT_DIRECTORY())//'OLFUASST.BIN'
               EXIT
            ENDIF
         ENDDO
         INQUIRE(FILE=FILE_NAME,OPENED=FILE_OPENED)
         IF(.NOT. FILE_OPENED) then
         	OPEN(UNIT_NO,FILE=FILE_NAME,ACCESS='DIRECT',RECL=256)
         end if
      RETURN

      ENTRY OPEN_FUTURE_ASSET_FILE(UNIT_NO,FILE_NUMBER,R_FILE_EXISTS)

         R_FILE_EXISTS = FUTURE_ASSET_FILE_EXISTS(FILE_NUMBER)
         IF(.NOT. FUTURE_ASSET_FILE_EXISTS(FILE_NUMBER)) RETURN
         IF(.NOT. FUTURE_ASSET_FILE_OPEN) THEN
            OPEN(UNIT_NO,FILE=TRIM(OUTPUT_DIRECTORY())// &
                     FUASTOL(FILE_NUMBER)// &
                     TRIM(FA_FILE_BINARY_NAMES(FILE_NUMBER))//".BIN", &
                     ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
            LAST_UNIT_NO_OPENED = UNIT_NO
            FUTURE_ASSET_FILE_OPEN = .TRUE.
         ENDIF
      RETURN

      ENTRY CLOSE_FUTURE_ASSET_FILE

         IF(FUTURE_ASSET_FILE_OPEN) THEN
!         
            CLOSE(LAST_UNIT_NO_OPENED)
            FUTURE_ASSET_FILE_OPEN = .FALSE.
         ENDIF

      RETURN

      ENTRY OPEN_FA_BASE_CASE_FILE(UNIT_NO)

         OPEN(UNIT_NO,FILE=TRIM(OUTPUT_DIRECTORY())// &
         	'BC_FUAST.BIN',FORM='UNFORMATTED')
      RETURN


      ENTRY OPEN_MODEL_ADDED_ASSET_FILE(UNIT_NO)

         IF(.NOT. MODEL_ADDED_ASSET_FILE_OPEN) THEN
            FILE_NAME = TRIM(OUTPUT_DIRECTORY())//"MSGFUAST.BIN"
            INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
            IF(FILE_EXISTS) CALL ERASE(FILE_NAME)
            OPEN(UNIT_NO,FILE=FILE_NAME,ACCESS="DIRECT", &
            	STATUS="UNKNOWN",RECL=1500)
            LAST_UNIT_NO_OPENED = UNIT_NO
            MODEL_ADDED_ASSET_FILE_OPEN = .TRUE.
         ENDIF
      RETURN

      ENTRY CLOSE_MODEL_ADDED_ASSET_FILE

         IF(MODEL_ADDED_ASSET_FILE_OPEN) THEN
            CLOSE(LAST_UNIT_NO_OPENED)
            MODEL_ADDED_ASSET_FILE_OPEN = .FALSE.
         ENDIF
         FIRST_FINANCIAL_ADDITION = .TRUE.
      RETURN

      ENTRY ADD_NEW_UNIT_FINANCIAL(R_ON_LINE_YR,R_ON_LINE_MO, &
                                   OPERATION_LIFE, &
                                   LEAD_TIME, &
                                   INVESTMENT_DATA_POINTER, &
                                   ESCALATION_VECTOR, &
                                   RR_CONSTRUCTION_COSTS, &
                                   RESOURCE_TYPE, &
                                   R_ASSET_CLASS, &
                                   R_ASSET_CLASS_VECTOR, &
                                   R_CAP_HYPERCUBE)
      FROM_ADD_NEXT_UNIT_FINANCIAL = .FALSE.
      GOTO 10

      ENTRY ADD_NEXT_UNIT_FINANCIAL(R_ON_LINE_YR,R_ON_LINE_MO, &
                                   OPERATION_LIFE, &
                                   LEAD_TIME, &
                                   INVESTMENT_DATA_POINTER, &
                                   ESCALATION_VECTOR, &
                                   RR_CONSTRUCTION_COSTS, &
                                   RESOURCE_TYPE, &
                                   R_ASSET_CLASS, &
                                   R_ASSET_CLASS_VECTOR, &
                                   R_CAP_HYPERCUBE, &
                                   R_PERCENT_COMPLETED, &
                                   R_REC_POSITION, &
                                   R_CURRENT_YEAR)

!
         FROM_ADD_NEXT_UNIT_FINANCIAL = .TRUE.
   10    IF(FIRST_FINANCIAL_ADDITION) THEN
            FIRST_FINANCIAL_ADDITION = .FALSE.
            OPTIONS_UNIT_NUM = 12
            CALL OPEN_FINANCIAL_OPTIONS(OPTIONS_UNIT_NUM, &
            	G_RECORDS_IN_BASE_FILE)
            OPTIONS_FINANCIAL_FILE_EXISTS = OPTIONS_UNIT_NUM == 12
            IF(OPTIONS_FINANCIAL_FILE_EXISTS) THEN
               CALL OPEN_MODEL_ADDED_ASSET_FILE(INT(11,2))
               SAVE_FN_RECORDS_IN_BASE_FILE = G_RECORDS_IN_BASE_FILE
            ENDIF
         ENDIF
         IF(.NOT. OPTIONS_FINANCIAL_FILE_EXISTS) RETURN
!
         IF(INVESTMENT_DATA_POINTER > SAVE_FN_RECORDS_IN_BASE_FILE) THEN
            WRITE(4,*) "In the Capacity Options File, a reference"
            WRITE(4,*) "was made to Expansion Plant Financial"
            WRITE(4,*) "record ",INVESTMENT_DATA_POINTER, &
                                 " which is greater"
            WRITE(4,*) "than the number of records in the"
            WRITE(4,*) "Expansion Plant Financial file."
            WRITE(4,*) '*** line 475 MSGMMFA.FOR ***'
            er_message='See WARNING MESSAGES -msgmmfa.for-1'
            call end_program(er_message)
         ENDIF
!
         READ(12,REC=INVESTMENT_DATA_POINTER) DELETE,ACCTNO,TAXLF, &
                           ADRLIFE,DEPMET,DBRATE,CWIPRB,AFDCSW, &
                           AFDCPERIODS,CASH_VECT,OHRATE, &
                           REGULATORY_ALLOCATOR,PLANT,CASH, &
                           BOKLF,DESC,TXNORM,SALVAGE_VALUE, &
                           ACCELERATE_FIXED_CHARGE, &
                           ACCELERATE_CHARGE_PERCENT, &
                           DELAY_FIXED_CHARGE,DELAY_CHARGE_PERCENT, &
                           ABMETH,ABTAX,WOYRS,ABANDONMENT_FIXED_CHARGE, &
                           ABANDONMENT_CHARGE_PERCENT, &
                           TAKE_BONUS_TAX_DEPRECIATION, &
                           WVPA_SUB_ACCOUNT_NUMBER,WVPA_DEPARTMENT_HEAD, &
                           PROJECT_COMPLETION_MO,PROJECT_COMPLETION_YR, &
                           WVPA_WORK_ORDER_NUMBER,WVPA_BUDGET_ITEM_NUM, &
                           OM_CAPITIALIZED_CASH, &
                           OM_CAPITIALIZED_NON_CASH, &
                           WVPA_COMPANY_ID, &
                           OVN_CAPITAL_COST_STOCASTIC, &
                           PERCENT_CASH_EARNS_ITC, &  !81
                           PROJECT_ITC_RATE          !82

         THERE_ARE_NEW_ASSESTS = .TRUE.
!
! NEED TO PASS THE A&I STUFF TO THE CL OR EL OBJECT
!
         BOKLFX = BOKLF
         IF(RESOURCE_TYPE == 'EL') THEN
            IF(BOKLF < 0.) THEN
               AI_REMAINING_LIFE = &
               	SET_AI_EL_REMAINING_LIFE(OPERATION_LIFE)
            ELSE
              AI_REMAINING_LIFE = &
              	SET_AI_EL_REMAINING_LIFE(INT(BOKLF,2))
            ENDIF
         ELSE
            IF(BOKLF < 0.) THEN
               AI_REMAINING_LIFE = &
               	SET_AI_CL_REMAINING_LIFE(OPERATION_LIFE)
            ELSE
              AI_REMAINING_LIFE = &
              	SET_AI_CL_REMAINING_LIFE(INT(BOKLF,2))
            ENDIF
         ENDIF
         IF(BOKLF < -10000.) THEN
            BOKLFX = FLOAT(OPERATION_LIFE)
         ENDIF
         IF(TXNORM < -10000.) THEN
            IF(BOKLF < -10000.) THEN
               TXNORM = FLOAT(OPERATION_LIFE)
            ELSE
               TXNORM = BOKLF
            ENDIF
         ENDIF
!
! SET-UP CASH FLOW AND PLANT ENTERING SERVICE VECTORS
!
         ALLOCATE(CEP(MAX_FINANCIAL_SIMULATION_YEARS))
         ALLOCATE(CE(MAX_FINANCIAL_SIMULATION_YEARS))
         ALLOCATE(ESCALATION_RATES(AVAIL_DATA_YEARS))
         CEP = 0.
         CE = 0.
!
         FIRSTYR = MAX(R_ON_LINE_YR - LEAD_TIME,int(0,2))
         IF(CAPACITY_PLANNING_METHOD() == 'NE') THEN
            OPTION_NAME = LEFT_JUSTIFY_I2_IN_STR(FIRSTYR)
         ELSEIF(CAPACITY_PLANNING_METHOD() == 'PR') THEN
            DO I = 1, 15
               IF(PLANT(I) == 0.) CYCLE
               FIRSTYR = R_ON_LINE_YR - I + 1
               EXIT
            ENDDO
         ELSE
            OPTION_NAME = LEFT_JUSTIFY_I2_IN_STR(R_ON_LINE_YR)
         ENDIF
         TOTAL_UNITS_ADDED = TOTAL_UNITS_ADDED + 1
         OPTION_NAME = TRIM(OPTION_NAME)//' '//TRIM(DESC)
         OPTION_NAME = TRIM(OPTION_NAME)//' ' &
         	//LEFT_JUSTIFY_I2_IN_STR(TOTAL_UNITS_ADDED)

         ABYEAR = 0
         ABACCT = 'N'
         IOFFSET = MIN(MAX_FINANCIAL_SIMULATION_YEARS, &
         	MAX(FIRSTYR - get_BASE_YEAR(),int(1,2)))
         IEND = MAX(int(0,2),MIN(int(15,2), &
         	MAX_FINANCIAL_SIMULATION_YEARS-IOFFSET))
!
! NEED TO HAVE A MEANS TO HAVE A PLANT UNDER CONSTRUCTION
!
         R_CONSTRUCTION_COSTS = RR_CONSTRUCTION_COSTS
         IF(INDEX(R_CAP_HYPERCUBE,"Not Active") == 0) THEN
            SCENARIO_INDEX = GET_SCENARIO_INDEX(R_CAP_HYPERCUBE)
            IF(SCENARIO_INDEX > 0) THEN
               HYPER_YR =  MAX(FIRSTYR - get_BASE_YEAR(),int(1,2))
               R_CONSTRUCTION_COSTS = RR_CONSTRUCTION_COSTS &
               	* GET_SCENARIO_BY_INDEX(HYPER_YR,1_2,SCENARIO_INDEX)
            ENDIF
         ENDIF
         
         DO I = 1, IEND
            CE(I+IOFFSET) = CASH(I) * R_CONSTRUCTION_COSTS/100.
            CEP(I+IOFFSET) = PLANT(I)/100.
         ENDDO
         VOID_LOG = RETURN_A_VECTOR_FOR_ALL_YEARS(ESCALATION_RATES, &
                                                  ESCALATION_VECTOR)
         IF(VOID_LOG) THEN
            WRITE(4,*) "The capacity options file referenced"
            WRITE(4,*) "escalation vector",ESCALATION_VECTOR," which"
            WRITE(4,*) "is a value escalation vector. Construction"
            WRITE(4,*) "cost escalation vectors cannot be values."
            WRITE(4,*) '*** line 569 MSGMMFA.FOR ***'
            er_message='See WARNING MESSAGES -msgmmfa.for-2'
            call end_program(er_message)
         ENDIF
         CURRENT_VALUE = 1.
         TEMP_CWIP = 0.
         DO I = 2, MAX_FINANCIAL_SIMULATION_YEARS
            IF(I <= AVAIL_DATA_YEARS + 1) THEN
               ESCALATION_MULTIPLIER = ESCALATION_RATES(I-1)
            ENDIF
            CURRENT_VALUE = CURRENT_VALUE * ESCALATION_MULTIPLIER
            CE(I)  = CURRENT_VALUE * CE(I)
            IF(I-1 == IOFFSET) THEN
               ACCELERATE_FIXED_CHARGE = ACCELERATE_FIXED_CHARGE * &
               	CURRENT_VALUE
               DELAY_FIXED_CHARGE = DELAY_FIXED_CHARGE * CURRENT_VALUE
               ABANDONMENT_FIXED_CHARGE = ABANDONMENT_FIXED_CHARGE * &
               	CURRENT_VALUE
            ENDIF
!

         ENDDO
!
! FOR A REASON I DON'T REMEMBER THE AFUDC SWITCH ON NEW UNITS WAS SET TO 
! 2.  THIS IS WRONG ON 11/15/95 I CHANGED IT TO SET IT TO 2 IF THE SWITCH IS
! NOT EQUAL TO 0 MSG)
!
         IF(AFDCSW /= 0) AFDCSW = 2
!
         CONSTRUCTION_YEARS_COMPLETED = 1
         IREC = IREC + 1
         WRITE(11,REC=IREC,IOSTAT=IOS) DELETE,CE,CEP,OPTION_NAME,ACCTNO, &
                                    BOKLFX,TAXLF,ADRLIFE,DEPMET,DBRATE, &
                                    CWIPRB,AFDCSW,FLOAT(LEAD_TIME), &
                                    AFDCPERIODS, &
                                    ABYEAR,ABMETH,ABACCT,ABTAX,WOYRS, &
                                    R_ON_LINE_MO,R_ON_LINE_YR,FIRSTYR, &
                                    OHRATE,REGULATORY_ALLOCATOR, &
                                    R_CONSTRUCTION_COSTS, &
                                    ESCALATION_VECTOR, &
                                    TXNORM,SALVAGE_VALUE, &
                                    CONSTRUCTION_YEARS_COMPLETED, &
                                    CASH,ACCELERATE_FIXED_CHARGE, &
                                    ACCELERATE_CHARGE_PERCENT, &
                                    DELAY_FIXED_CHARGE, &
                                    DELAY_CHARGE_PERCENT, &
                                    ABANDONMENT_FIXED_CHARGE, &
                                    ABANDONMENT_CHARGE_PERCENT, &
                                    TAKE_BONUS_TAX_DEPRECIATION, &
                                    R_ASSET_CLASS,R_ASSET_CLASS_VECTOR
!
         IF(FROM_ADD_NEXT_UNIT_FINANCIAL) THEN
            R_PERCENT_COMPLETED = CASH(1)
            R_REC_POSITION = IREC
            IF(FLEX_PLANNING_RPT_NOT_OPEN) THEN
               FLEX_RPT_UNIT = FLEX_PLANNING_ACTION_HEADER(FLEX_RPT_REC)
               FLEX_PLANNING_RPT_NOT_OPEN = .FALSE.
            ENDIF
            WRITE(FLEX_RPT_UNIT,REC=FLEX_RPT_REC) PRT_ENDPOINT(), &
                               OPTION_NAME,'Committed   ', &
                               FLOAT(R_CURRENT_YEAR),FLOAT(R_ON_LINE_YR)
            FLEX_RPT_REC = FLEX_RPT_REC + 1
         ENDIF
         DEALLOCATE(CEP,CE,ESCALATION_RATES)
         RETURN
!

      ENTRY ADJUST_NEW_UNIT_FINANCIAL(R_REC,R_ON_LINE_YR, &
                                      R_PERCENT_COMPLETED,R_ABANDON_IT, &
                                      R_CURRENT_YEAR, &
                                      R_OPTION_TIMES_DELAYED)

         ALLOCATE(CEP(MAX_FINANCIAL_SIMULATION_YEARS))
         ALLOCATE(CE(MAX_FINANCIAL_SIMULATION_YEARS))
         READ(11,REC=R_REC) DELETE,CE,CEP,OPTION_NAME,ACCTNO,BOKLFX, &
                            TAXLF,ADRLIFE,DEPMET,DBRATE, &
                            CWIPRB,AFDCSW,REAL_LEAD_TIME, &
                            AFDCPERIODS, &
                            ABYEAR,ABMETH,ABACCT,ABTAX,WOYRS, &
                            ON_LINE_MO,ON_LINE_YR,FIRSTYR, &
                            OHRATE,REGULATORY_ALLOCATOR, &
                            CONSTRUCTION_COSTS, &
                            CONSTRUCTION_ESCALATION_VECTOR, &
                            TXNORM,SALVAGE_VALUE, &
                            CONSTRUCTION_YEARS_COMPLETED, &
                            CASH, &
                            ACCELERATE_FIXED_CHARGE, &
                            ACCELERATE_CHARGE_PERCENT, &
                            DELAY_FIXED_CHARGE, &
                            DELAY_CHARGE_PERCENT, &
                            ABANDONMENT_FIXED_CHARGE, &
                            ABANDONMENT_CHARGE_PERCENT, &
                            TAKE_BONUS_TAX_DEPRECIATION, &
                            ASSET_CLASS_NUM,ASSET_CLASS_VECTOR
!
         IF(ON_LINE_YR == 2300) RETURN
         ALLOCATE(ESCALATION_RATES(AVAIL_DATA_YEARS))
         VOID_LOG = RETURN_A_VECTOR_FOR_ALL_YEARS(ESCALATION_RATES, &
                                         CONSTRUCTION_ESCALATION_VECTOR)
         IF(globecom_YEAR < AVAIL_DATA_YEARS) THEN
            ESCALATION_MULTIPLIER = ESCALATION_RATES(globecom_YEAR)
         ELSE
            ESCALATION_MULTIPLIER = ESCALATION_RATES(AVAIL_DATA_YEARS)
         ENDIF
         ACCELERATE_FIXED_CHARGE = ACCELERATE_FIXED_CHARGE * &
         	ESCALATION_MULTIPLIER
         DELAY_FIXED_CHARGE = DELAY_FIXED_CHARGE * ESCALATION_MULTIPLIER
         ABANDONMENT_FIXED_CHARGE = ABANDONMENT_FIXED_CHARGE * &
         	ESCALATION_MULTIPLIER
         IF(R_ON_LINE_YR == ON_LINE_YR .OR. R_ABANDON_IT) THEN
            IF(R_ABANDON_IT) THEN
               ABYEAR = globecom_YEAR + get_BASE_YEAR()
               R_ON_LINE_YR = 2300
               CANCEL_CHARGE = ABANDONMENT_FIXED_CHARGE + &
               	ABANDONMENT_CHARGE_PERCENT * CASH_TO_DATE/100.
               CE(globecom_YEAR+1) = CANCEL_CHARGE
               DO YR = globecom_YEAR+2,MAX_FINANCIAL_SIMULATION_YEARS
                  CE(YR) = 0.
               ENDDO
            ELSE
               IF(CONSTRUCTION_YEARS_COMPLETED < 15) THEN
                  CONSTRUCTION_YEARS_COMPLETED = 1 + &
                  	CONSTRUCTION_YEARS_COMPLETED
                  CASH(1) = CASH(1) + CASH(CONSTRUCTION_YEARS_COMPLETED)
               ELSE
                  CASH(1) = 100.
               ENDIF
            ENDIF
         ELSE
            ALLOCATE(TEMP_HOLDING_ARRAY(MAX_FINANCIAL_SIMULATION_YEARS))
            CASH_TO_DATE = CE(1)
            IF(R_ON_LINE_YR < ON_LINE_YR) THEN
               TEMP_REAL = 0.
               LAST_YEAR_CASH = CE(ON_LINE_YR-get_BASE_YEAR())
               ACCEL_CHARGE=ACCELERATE_CHARGE_PERCENT/100.*CASH_TO_DATE + &
                ACCELERATE_FIXED_CHARGE
               CASH_ADJUSTMENT = (LAST_YEAR_CASH+ACCEL_CHARGE)/ &
               	FLOAT(R_ON_LINE_YR - globecom_YEAR - get_BASE_YEAR())            
               DO YR = globecom_YEAR+1, R_ON_LINE_YR - get_BASE_YEAR()
                  CE(YR) = CE(YR) + CASH_ADJUSTMENT 
                  IF(CONSTRUCTION_YEARS_COMPLETED < 15) THEN
                     CONSTRUCTION_YEARS_COMPLETED = 1 + &
                     	CONSTRUCTION_YEARS_COMPLETED
                     CASH(1)=CASH(1)+CASH(CONSTRUCTION_YEARS_COMPLETED)
                  ELSE
                     CASH(1) = 100.
                  ENDIF
               ENDDO
               DO YR = R_ON_LINE_YR-get_BASE_YEAR()+1, &
               	MAX_FINANCIAL_SIMULATION_YEARS-1
                  CURRENT_VALUE = CE(YR+1)
                  IF(YR < AVAIL_DATA_YEARS) THEN
                     ESCALATION_MULTIPLIER = ESCALATION_RATES(YR)
                  ELSE
                     ESCALATION_MULTIPLIER = &
                     	ESCALATION_RATES(AVAIL_DATA_YEARS)
                  ENDIF
                  CURRENT_VALUE = CURRENT_VALUE / ESCALATION_MULTIPLIER
                  CE(YR) = CURRENT_VALUE
               ENDDO
               CE(MAX_FINANCIAL_SIMULATION_YEARS) = 0.
!
! ACCELERATE THE IN-SERVICE VECTOR
!
               CEP(R_ON_LINE_YR-get_BASE_YEAR())=CEP(R_ON_LINE_YR- &
               	get_BASE_YEAR()) + &
                 CEP(R_ON_LINE_YR-get_BASE_YEAR()+1)  
                 
               DO YR = R_ON_LINE_YR-get_BASE_YEAR()+1, &
                MAX_FINANCIAL_SIMULATION_YEARS-1
                  CEP(YR) = CEP(YR+1)
               ENDDO
               CEP(MAX_FINANCIAL_SIMULATION_YEARS) = 0.
            ELSEIF(R_ON_LINE_YR > ON_LINE_YR) THEN
               DELAY_CHARGE_RATE = DELAY_CHARGE_PERCENT/100.
               DELAY_PERIODS = R_ON_LINE_YR - ON_LINE_YR
               TEMP_HOLDING_ARRAY = CE
               CE(1:globecom_YEAR) = TEMP_HOLDING_ARRAY(1:globecom_YEAR)
               ESCALATION_MULTIPLIER = 1
               DO YR = globecom_year+1,globecom_year+DELAY_PERIODS
                  CURRENT_VALUE = TEMP_HOLDING_ARRAY(YR)
                  IF(YR <= AVAIL_DATA_YEARS) THEN
                    ESCALATION_MULTIPLIER = ESCALATION_RATES(YR)
                  ELSE
                    ESCALATION_MULTIPLIER = ESCALATION_RATES(AVAIL_DATA_YEARS)
                  ENDIF
                  CURRENT_VALUE = CURRENT_VALUE * ESCALATION_MULTIPLIER
                  DELAY_CHARGE = DELAY_CHARGE_RATE * CASH_TO_DATE + &
                  	DELAY_FIXED_CHARGE
                  CE(YR) = DELAY_CHARGE
                  CASH_TO_DATE = CASH_TO_DATE + DELAY_CHARGE
               ENDDO
               DO YR=globecom_YEAR+DELAY_PERIODS+1,MAX_FINANCIAL_SIMULATION_YEARS
                  CURRENT_VALUE = TEMP_HOLDING_ARRAY(YR-DELAY_PERIODS)
                  DO I = YR, YR+DELAY_PERIODS-1
                     IF(I <= AVAIL_DATA_YEARS + 1) THEN
                        ESCALATION_MULTIPLIER = ESCALATION_RATES(I-1)
                     ELSE
                        ESCALATION_MULTIPLIER = &
                        	ESCALATION_RATES(AVAIL_DATA_YEARS)
                     ENDIF
                     CURRENT_VALUE=CURRENT_VALUE * ESCALATION_MULTIPLIER
                  ENDDO
                  CE(YR) = CURRENT_VALUE
               ENDDO
!
! DELAY THE IN-SERVICE VECTOR
!
               TEMP_HOLDING_ARRAY = CEP
               DO YR = 1, globecom_year
                  CEP(YR) = TEMP_HOLDING_ARRAY(YR)
               ENDDO
               DO YR = globecom_YEAR+1,globecom_YEAR+1+DELAY_PERIODS
                  CEP(YR) = 0.
               ENDDO
               DO YR=globecom_YEAR+DELAY_PERIODS+2,MAX_FINANCIAL_SIMULATION_YEARS
                  CEP(YR) = TEMP_HOLDING_ARRAY(YR-DELAY_PERIODS)
               ENDDO
            ENDIF
            DEALLOCATE(TEMP_HOLDING_ARRAY)
         ENDIF
         WRITE(11,REC=R_REC) DELETE,CE,CEP,OPTION_NAME,ACCTNO,BOKLFX, &
                                      TAXLF,ADRLIFE,DEPMET,DBRATE, &
                                      CWIPRB,AFDCSW,REAL_LEAD_TIME, &
                                      AFDCPERIODS, &
                                      ABYEAR,ABMETH,ABACCT,ABTAX,WOYRS, &
                                      ON_LINE_MO,R_ON_LINE_YR,FIRSTYR, &
                                      OHRATE,REGULATORY_ALLOCATOR, &
                                      CONSTRUCTION_COSTS, &
                                      CONSTRUCTION_ESCALATION_VECTOR, &
                                      TXNORM,SALVAGE_VALUE, &
                                      CONSTRUCTION_YEARS_COMPLETED, &
                                      CASH, &
                                      ACCELERATE_FIXED_CHARGE, &
                                      ACCELERATE_CHARGE_PERCENT, &
                                      DELAY_FIXED_CHARGE, &
                                      DELAY_CHARGE_PERCENT, &
                                      ABANDONMENT_FIXED_CHARGE, &
                                      ABANDONMENT_CHARGE_PERCENT, &
                                      TAKE_BONUS_TAX_DEPRECIATION, &
                                      ASSET_CLASS_NUM,ASSET_CLASS_VECTOR
         IF(R_ABANDON_IT) THEN
            WRITE(FLEX_RPT_UNIT,REC=FLEX_RPT_REC) PRT_ENDPOINT(), &
                               OPTION_NAME,'Abandoned   ', &
                               FLOAT(R_CURRENT_YEAR),FLOAT(R_ON_LINE_YR)
            FLEX_RPT_REC = FLEX_RPT_REC + 1
         ELSEIF(R_CURRENT_YEAR == R_ON_LINE_YR) THEN
            WRITE(FLEX_RPT_UNIT,REC=FLEX_RPT_REC) PRT_ENDPOINT(), &
                               OPTION_NAME,'Finished    ', &
                               FLOAT(R_CURRENT_YEAR),FLOAT(R_ON_LINE_YR)
            FLEX_RPT_REC = FLEX_RPT_REC + 1
         ELSEIF(R_ON_LINE_YR < ON_LINE_YR) THEN
            WRITE(FLEX_RPT_UNIT,REC=FLEX_RPT_REC) PRT_ENDPOINT(), &
                               OPTION_NAME,'Accelerated ', &
                               FLOAT(R_CURRENT_YEAR),FLOAT(R_ON_LINE_YR)
            FLEX_RPT_REC = FLEX_RPT_REC + 1
         ELSEIF(R_ON_LINE_YR > ON_LINE_YR) THEN
            DELAYED_STR = 'Delay # '// &
                LEFT_JUSTIFY_I2_IN_STR(R_OPTION_TIMES_DELAYED)
            WRITE(FLEX_RPT_UNIT,REC=FLEX_RPT_REC) PRT_ENDPOINT(), &
                               OPTION_NAME,DELAYED_STR, &
                               FLOAT(R_CURRENT_YEAR),FLOAT(R_ON_LINE_YR)
            FLEX_RPT_REC = FLEX_RPT_REC + 1
         ENDIF
!
         R_PERCENT_COMPLETED = CASH(1)
         DEALLOCATE(CEP,CE,ESCALATION_RATES)
      RETURN
      ENTRY SET_FIRST_FINANCIAL_ADD_TRUE
         FIRST_FINANCIAL_ADDITION = .TRUE.
      RETURN
      ENTRY INC_PROJECTS_IN_PROCESS_RECS
         PROJECTS_IN_PROCESS = PROJECTS_IN_PROCESS + 1
      RETURN
      ENTRY INC_PROJECTS_ACCEPTED_RECS
         PROJECTS_ACCEPTED = PROJECTS_ACCEPTED + 1
      RETURN
      ENTRY LAST_PROJECTS_IN_PROCESS_RECORD(R_LAST_RECORD)
         R_LAST_RECORD = IREC
      RETURN
      ENTRY RESET_PROJECTS_IN_PROCESS_RECS
         PROJECTS_IN_PROCESS = 0
         PROJECTS_ACCEPTED = 0
         IREC = 0
      RETURN
      ENTRY RESET_AN_DECOMP_FINAN_OPTIONS
         IREC = PROJECTS_ACCEPTED
      RETURN
      ENTRY INCREMENT_FA_RECORDS
         RECORDS_IN_BASE_FILE = RECORDS_IN_BASE_FILE + 1
      RETURN
      ENTRY LAST_FUTURE_ASSET_RECORD(R_LAST_RECORD)
         R_LAST_RECORD = RECORDS_IN_ORG_BASE_FILE
      RETURN
      ENTRY RESET_FA_BASE_RECORDS
         RECORDS_IN_BASE_FILE = RECORDS_IN_ORG_BASE_FILE
         IREC = RECORDS_IN_ORG_BASE_FILE
      RETURN
      ENTRY WHAT_INVESTMENTS_ARE_THESE(R_LOGICAL_1)
         R_LOGICAL_1 = THERE_ARE_NEW_ASSESTS
      RETURN
      ENTRY SET_NEW_ASSETS_ACTIVE
         THERE_ARE_NEW_ASSESTS = .TRUE.
      RETURN
      ENTRY SET_NEW_ASSETS_INACTIVE
         THERE_ARE_NEW_ASSESTS = .FALSE.
      RETURN
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END

!                                                                      
!          SUBROUTINE TO CALCULATE FUTURE ASSET VALUE                  
!          COPYRIGHT (C) 1983 M.S. GERBER & ASSOCIATES, INC.           
!                                                                      

!
      SUBROUTINE FUASST(SAVE_BASE_CASE,PROCESSING_NEW_ADDITIONS)
!
! NOTE: 6/30/94 AFDCRT,AFDCBR,INTEREST_CAP_RATE ARE ONLY PASSED TO
! AFUDC
! CURRENT_INTEREST_CAP_RATE IS USED IN THIS ROUTINE
!
      USE IREC_ENDPOINT_CONTROL
      USE DRILLING_REPT_PARAMETERS
      use mod_base_year
      use grx_planning_routines
      use SpinDriftLib
      USE PROD_ARRAYS_DIMENSIONS
      USE SIZECOM
      use globecom
      use iostatmsg
      use namescom
      use msgmmfa_cash
!
      LOGICAL (KIND=4) :: SAVE_BASE_CASE,PROCESSING_NEW_ADDITIONS
      CHARACTER (LEN=15) :: LEFT_JUSTIFY_I2_IN_STR
      INTEGER (KIND=2) :: FINANCIAL_SIMULATION_YEARS,VOID_INT2, &
      	WRITE_DRILLING_RPT
      LOGICAL (KIND=1) :: MONTHLY_CASH_VALUES_EXIST, &
      	MONTHLY_PLANT_IN_SERVICE_EXIST
!
      REAL :: ACCELERATE_FIXED_CHARGE, &
           ACCELERATE_CHARGE_PERCENT, &
           DELAY_FIXED_CHARGE, &
           DELAY_CHARGE_PERCENT, &
           ABANDONMENT_FIXED_CHARGE, &
           ABANDONMENT_CHARGE_PERCENT
      INTEGER (KIND=2) :: FA_RPT_UNIT=0,FUTURE_ASSET_REPT_HEADER,MO_START
      INTEGER :: FA_RPT_REC
      SAVE FA_RPT_REC
      LOGICAL (KIND=1) :: FUTURE_ASSET_RPT_NOT_OPEN=.TRUE., &
      	USE_PLANT_PERCENTAGE
      REAL :: FLOAT_END_PT,REPORTING_YEAR,PROJECT_SUM
!
      REAL :: AFDCRT,AFDCBR,INTEREST_CAP_RATE,CURRENT_INTEREST_CAP_RATE, &
      	PROPERTY_ESCALATION,AFUDC_NF_RATE
      COMMON/AFUDC_STUFF/AFDCRT(MAX_FINANCIAL_SIMULATION_YEARS), &
              AFDCBR(MAX_FINANCIAL_SIMULATION_YEARS), &
              INTEREST_CAP_RATE(MAX_FINANCIAL_SIMULATION_YEARS), &
              PROPERTY_ESCALATION(MAX_FINANCIAL_SIMULATION_YEARS), &
              CURRENT_INTEREST_CAP_RATE(MAX_FINANCIAL_SIMULATION_YEARS), &
              AFUDC_NF_RATE(MAX_FINANCIAL_SIMULATION_YEARS)
!
      LOGICAL (KIND=1) :: MIDAS_ADDED_INVESTMENT
      INTEGER (KIND=2) :: START,END,LAST_REPORTING_YEAR,LAST_FA_RECORD, &
      	ON_LINE_YR,CONSTRUCTION_YEARS_COMPLETED
      CHARACTER (LEN=1) :: UTILITY_TYPE,DUMMY_TYPE,BOOK_DEP_METHOD, &
      	TAKE_BONUS_TAX_DEPRECIATION,TAX_BOOK_DEP_METHOD
      CHARACTER (LEN=30) :: DESC,RPT_TITLE(15),RECORD_TYPE*6
      CHARACTER (LEN=34) :: WORKORDER_NAME,TEMP_WORKORDER_NAME,INDEXING_STR*4
!
      CHARACTER (LEN=10) :: PERIOD_NAME,TEMP_STR
      CHARACTER (LEN=38) :: OPTION_NAME
      LOGICAL (KIND=1) :: CAP_PLANNING_METHOD_ANNDECOMP
      INTEGER (KIND=2) :: I,J,IREC,IEND,DELETE,TACCTS,IOFFSET, &
                SERVICEMO,AFDCPERIODS,CONSTRUCTION_ESCALATION_VECTOR, &
                AFDC_CAP_VECTOR,CASH_VECT,PLANT_VECT,IVEC, &
                LAST_YEAR,FIRST_YEARS_VEC(AVAIL_DATA_YEARS), &
                START_YEARS,FIRSTYR_VEC, &
                ACCOUNTS_REPORTED, &
                ASSETS_IN_BASE_FILE, &
                MO
      REAL (KIND=4) :: CONSTRUCT_ESCALAT_VECTOR_RATE
      REAL :: NRTXWO,RATIO,TAXWO,BOKWO,BOKBL,BOKDAL, &
           AFCBL,AFCWO,AFCDAL, &
           ADRLIFE,DBRT,CONSTRUCTION_PERIOD,ITC_RATE, &
           PV_PLANT_COST, &
           TEMP_CWIP, &
           TAX_NORMALIZATION_PEROID,SALVAGE_VALUE,TAX_BASIS_ADJUSTOR
      CHARACTER (LEN=40) :: LAG_STR,CASH_STR,READ_STR*50
      REAL (KIND=4) :: CASH_LAG_PATTERN(12),CASH_PAYMENTS(12)
      REAL :: CEPTXBOK(:),TOTAL_BKDPTX(:),CURRENT_INTEREST_CAP(:), &
           PCAPINRST(:),TAXDPALT(:),CAPINRST(:), &
           TXPREFDEP(:),BOOK_VALUE(:),ACE_BOOK_DEP(:), &
           TAX_VALUE_BEGINNING_90(:),DUMMY(:)
      ALLOCATABLE :: CEPTXBOK,TOTAL_BKDPTX,CURRENT_INTEREST_CAP, &
                     PCAPINRST,TAXDPALT,CAPINRST,TXPREFDEP, &
                     BOOK_VALUE,ACE_BOOK_DEP,TAX_VALUE_BEGINNING_90, &
                     DUMMY
      REAL :: VECTOR_DATA_CASH(:), &
           VECTOR_DATA_PLNT(:), &
           VECTOR_DATA(:)
      ALLOCATABLE :: VECTOR_DATA_CASH,VECTOR_DATA_PLNT,VECTOR_DATA
      LOGICAL (KIND=1) :: FUTURE_ASSET_REPORT,FUTURE_ASSETS_REPORT, &
      	REPORT_ALL_ACCOUNTS,HALF_YEAR_BOOK_DEP
! ADDED 4/13/92 MIDAS GOLD FOR REAL PROPERTY VALUES
      LOGICAL (KIND=1) :: REAL_PROPERTY_TAX_VALUES,CAL_REAL_PROPERTY_TAX_VALUES
!
!     TYPE DECLARATION FOR /LCARRY/
      REAL :: WODFTX,AFDC1B,AFDC2B,AFDCDF
! TYPE DECLARATION FOR /WKARRY/
!     TAX ITEMS
           REAL :: TXDEF,TAXDP,CEPTX,ITCDF,ITCDP,TAXEXP,TXDEFC,ITCPAY
!     AFDC ITEMS
           REAL :: CE,CEP,AFDC1,AFDC2,CWIP,RBCWIP
!     BOOK ITEMS
           REAL :: BOKDP,AFCDP
! ABANDONMENT ITEMS
       REAL :: DDB,RBDDB,AFDPAJ,AJAFDC,EXEXP,AMORTE,AFCEXP,BKDPTX,BKDPAJ
       REAL :: GPV_PROPERTY_TAX,NPV_PROPERTY_TAX
      COMMON/FA_WKARRY/ TXDEF(MAX_FINANCIAL_SIMULATION_YEARS), &
      	TAXDP(MAX_FINANCIAL_SIMULATION_YEARS), &
             CEPTX(MAX_FINANCIAL_SIMULATION_YEARS), &
             ITCDF(MAX_FINANCIAL_SIMULATION_YEARS), &
             ITCDP(MAX_FINANCIAL_SIMULATION_YEARS), &
             TAXEXP(MAX_FINANCIAL_SIMULATION_YEARS), &
             TXDEFC(MAX_FINANCIAL_SIMULATION_YEARS), &
             CE(MAX_FINANCIAL_SIMULATION_YEARS), &
             CEP(MAX_FINANCIAL_SIMULATION_YEARS), &
             AFDC1(MAX_FINANCIAL_SIMULATION_YEARS), &
             AFDC2(MAX_FINANCIAL_SIMULATION_YEARS), &
             CWIP(MAX_FINANCIAL_SIMULATION_YEARS), &
             RBCWIP(MAX_FINANCIAL_SIMULATION_YEARS), &
             BOKDP(MAX_FINANCIAL_SIMULATION_YEARS), &
             AFCDP(MAX_FINANCIAL_SIMULATION_YEARS), &
             DDB(MAX_FINANCIAL_SIMULATION_YEARS), &
             RBDDB(MAX_FINANCIAL_SIMULATION_YEARS), &
             AFDPAJ(MAX_FINANCIAL_SIMULATION_YEARS), &
             AJAFDC(MAX_FINANCIAL_SIMULATION_YEARS), &
             EXEXP(MAX_FINANCIAL_SIMULATION_YEARS), &
             AMORTE(MAX_FINANCIAL_SIMULATION_YEARS), &
             BKDPTX(MAX_FINANCIAL_SIMULATION_YEARS), &
             ITCPAY(MAX_FINANCIAL_SIMULATION_YEARS), &
             AFCEXP(MAX_FINANCIAL_SIMULATION_YEARS), &
             BKDPAJ(MAX_FINANCIAL_SIMULATION_YEARS), &
             WODFTX(MAX_FINANCIAL_SIMULATION_YEARS), &
             AFDC1B(MAX_FINANCIAL_SIMULATION_YEARS), &
             AFDC2B(MAX_FINANCIAL_SIMULATION_YEARS), &
             AFDCDF(MAX_FINANCIAL_SIMULATION_YEARS), &
             GPV_PROPERTY_TAX(MAX_FINANCIAL_SIMULATION_YEARS), &
             NPV_PROPERTY_TAX(MAX_FINANCIAL_SIMULATION_YEARS)
! TYPE DECLARATION FOR /FAINPT/
      CHARACTER (len=1) :: DPBAIS,ABACCT,ACCOUNT_ACTIVE
      CHARACTER (LEN=4) :: DEPMET
      INTEGER (KIND=2) :: AFDCSW,ABYEAR,ABMETH,FIRSTYR, &
      	ASSET_CLASS_NUM,ASSET_CLASS_VECTOR
      INTEGER :: ACCTNO
      INTEGER :: IOS
      REAL BOKLF,TAXLF,DBRATE,CWIP1,CWIPRB,AFUDC1,AFDCB1, &
           AFDCFC,ABTAX,WOYRS,PCITC,OHRATE,REGULATORY_ALLOCATOR, &
           CASH(15),PLANT(15), &
           AFUDC_IN_CWIP(MAX_FINANCIAL_SIMULATION_YEARS)
      INTEGER (KIND=2) :: BASEYEAR
      integer :: timeshere=0
!
!
      CHARACTER (LEN=2) :: CAPACITY_PLANNING_METHOD,TEMP_CHR
      LOGICAL (KIND=1) :: NEXT_DECISION_CAPACITY_PLANNING
      INTEGER (KIND=2) :: REPORTING_YEARS
      SAVE ASSETS_IN_BASE_FILE
      LOGICAL (KIND=1) :: REPORT_HEADER_OPEN=.FALSE.
      INTEGER (KIND=2) :: FA_REPORTING_UNIT,FUTURE_ASSET_RPT_HEADER
      INTEGER :: FA_REPORTING_REC
      SAVE FA_REPORTING_UNIT,FA_REPORTING_REC
!
!
! STANDARD VARIABLES FOR GETTING MONTHLY VECTORS
!
      CHARACTER (LEN=1) :: CASH_MONTHLY_DATA_UNITS(LAST_AVAILABLE_MONTHLY_YEAR)
      CHARACTER (LEN=1) :: PLANT_MONTHLY_DATA_UNITS(LAST_AVAILABLE_MONTHLY_YEAR)
      REAL (KIND=4) :: MONTHLY_VECTOR_DATA(12,LAST_AVAILABLE_MONTHLY_YEAR)
!     
      INTEGER (KIND=2) :: CASH_MONTH_ENDING(LAST_AVAILABLE_MONTHLY_YEAR)
      INTEGER (KIND=2) :: YR
      INTEGER (KIND=2):: PLANT_MONTH_ENDING(LAST_AVAILABLE_MONTHLY_YEAR)
      CHARACTER (LEN=1) :: DATA_TYPE,VECTOR_TYPE*20, &
      	CASH_VECTOR_DATA_TYPE,PLANT_VECTOR_DATA_TYPE
      CHARACTER (LEN=1) :: DRILLING_REPRT_LEVEL, &
      FINANCIAL_DRILLING,ASSET_REPORTING_LEVEL
      SAVE ASSET_REPORTING_LEVEL
      CHARACTER (LEN=40) :: ACCOUNT_NAME
      CHARACTER (LEN=30) :: DRILLING_NAME
      LOGICAL (KIND=1) :: ANNUAL_INFO_ACTIVE,FIRST_PASS_NEW_ADDITIONS
      SAVE FIRST_PASS_NEW_ADDITIONS
! 
!
      REAL (KIND=4) :: CASH_EXPENDITURES(:,:)
      ALLOCATABLE :: CASH_EXPENDITURES
      REAL (KIND=4) :: BOOK_EXPEN(:,:),PLANT_2_SERVICE(:,:), &
             MONTHLY_AFUDC_ON_CASH(:,:), &
             MONTHLY_AFUDC_ON_PLANT(:,:), &
             MONTHLY_CWIP(:,:), &
             MONTHLY_AFUDC_IN_CWIP(:,:), &
             MONTHLY_CWIP_IN_RATEBASE(:,:), &
             MONTHLY_CAPITALIZED_INTEREST(:,:), &
             MONTHLY_CURRENT_INTEREST(:,:), &
             MONTHLY_INTEREST_TO_TAX_VALUE(:,:)
      REAL (KIND=4) :: MONTHLY_CURRENT_INTEREST_CAP(:,:), &
             MONTHLY_TAX_VALUE_OF_ASSET(:,:), &
             BONUS_DEP_MONTHLY_2001(:,:), &
             MONTHLY_TAX_BOOK_DEP(:,:), &
             DUMMY_MONTHLY_ARRAY(:,:), &
             MONTHLY_TAX_DEP_PREFERENCE(:,:), &
             MONTHLY_TAX_DEPRECIATION_ALT(:,:), &
             MONTHLY_DEFERRED_TAX_BASIS(:,:), &
             MONTHLY_ACE_BOOK_DEP(:,:), &
             MONTHLY_TAX_EXPENSE(:,:), &
             MONTHLY_TAX_DEPRECIATION(:,:), &
             MONTHLY_BOOK_DEP(:,:), &
             CUM_BOOK_DEP(:,:), &
             GPV(:,:), &
             NPV(:,:), &
             TEMP_PLANT_2_SERVICE(:)

      real, allocatable :: PERCENT_PLANT_2_SERVICE(:,:)
      ALLOCATABLE :: BOOK_EXPEN,PLANT_2_SERVICE, &
                     MONTHLY_AFUDC_ON_CASH,MONTHLY_AFUDC_ON_PLANT, &
                     MONTHLY_CWIP,MONTHLY_AFUDC_IN_CWIP, &
                     MONTHLY_CWIP_IN_RATEBASE, &
                     MONTHLY_CAPITALIZED_INTEREST, &
                     MONTHLY_CURRENT_INTEREST, &
                     MONTHLY_INTEREST_TO_TAX_VALUE, &
                     MONTHLY_TAX_VALUE_OF_ASSET, &
                     BONUS_DEP_MONTHLY_2001,MONTHLY_TAX_BOOK_DEP, &
                     DUMMY_MONTHLY_ARRAY,MONTHLY_TAX_DEP_PREFERENCE, &
                     MONTHLY_TAX_DEPRECIATION_ALT, &
                     MONTHLY_TAX_DEPRECIATION, &
                     MONTHLY_DEFERRED_TAX_BASIS, &
                     MONTHLY_ACE_BOOK_DEP, &
                     MONTHLY_TAX_EXPENSE, &
                     MONTHLY_CURRENT_INTEREST_CAP, &
                     MONTHLY_BOOK_DEP,CUM_BOOK_DEP,GPV, &
                     NPV, &
                     TEMP_PLANT_2_SERVICE
      CHARACTER (LEN=1) :: RIPPLE_ZERO
      PARAMETER(RIPPLE_ZERO='Z')
      REAL (KIND=4) :: TEMP_SUM,PLANT_CAP
      LOGICAL (KIND=1) :: VOID_LOGICAL,SET_DRILLING_DATA_CASH_TRUE
      INTEGER (KIND=4) :: VALUES_2_ZERO
      REAL (KIND=4) :: PERIOD,PERIOD_AMOUNT
      LOGICAL (KIND=1) :: CASH_NOT_ZERO,BOOK_DEP_NOT_ZERO
      LOGICAL (KIND=1) :: LF95,LAHEY_LF95
      CHARACTER (LEN=6) :: SHORT_MONTH_NAMES
      INTEGER (KIND=2) :: LAST_REPORTING_MO
      CHARACTER (LEN=1) :: MONTHLY_DETAIL='M',ANNUAL_DETAIL='A'
      REAL (KIND=4) :: CWIP_OPENING_BAL
      INTEGER (KIND=2) :: PROJECT_COMPLETION_MO, &
      	PROJECT_COMPLETION_YR,WVPA_COMPANY_ID
      INTEGER (KIND=4) :: WVPA_SUB_ACCOUNT_NUMBER,WVPA_BUDGET_ITEM_NUM
      CHARACTER (LEN=10) :: WVPA_WORK_ORDER_NUMBER
      REAL :: OM_CAPITIALIZED_CASH,OM_CAPITIALIZED_NON_CASH
      CHARACTER (LEN=15) :: WVPA_DEPARTMENT_HEAD
      INTEGER :: WVPA_WORKORDER_CONTRL_REC,WVPA_WORKORDER_EXPENSES_REC
      INTEGER (KIND=2) :: WVPA_WORKORDER_CONTRL_UNIT, &
      	WVPA_WORKORDER_EXPENSES_UNIT
      LOGICAL (KIND=1) :: WVPA_CONTROL_FILE_OPEN=.FALSE.,WVPA, &
                          WVPA_WO_EXPENSES_FILE_OPEN=.FALSE., &
                          WVPA_WO_SUMMARY_FILE_OPEN=.FALSE.
      INTEGER (KIND=2) :: WVPA_WORKORDER_CONTROL_RPT, &
                          WVPA_WORKORDER_EXPENSES_RPT, &
                          WVPA_WORKORDER_SUMMARY_RPT
!
      REAL (KIND=4) :: EXPENDITURES_ProjToDate, &
                       RemainingExpenditures, &
                       YearToDate, &
                       AFUDC_CHECK_MARK, &
                       COMPLETION_CHECK_MARK
      INTEGER (KIND=4) :: WVPA_WORKORDER_SUMMARY_RPRT_REC
      INTEGER (KIND=2) :: WVPA_WORKORDER_SUMARY_RPRT_UNIT
      INTEGER (KIND=4) :: FILE_ID,AVAIL_FILES
      CHARACTER (LEN=40) :: OVN_CAPITAL_COST_STOCASTIC
      INTEGER (KIND=2) :: SCENARIO_INDEX,GET_SCENARIO_INDEX
      REAL (KIND=4) :: GET_SCENARIO_BY_INDEX
      INTEGER (KIND=2) :: STO_PTR,LAST_DATA_INPUT_YR
      CHARACTER (LEN=256) :: ERR_MSG
      LOGICAL (KIND=4) :: FILE_EXISTS
!
      REPORTING_YEARS = get_globecom_study_period() + get_extension_period() + 1
      LAST_DATA_INPUT_YR = get_BASE_YEAR() + get_globecom_study_period() + get_extension_period()
      timeshere=timeshere+1
      LF95 = LAHEY_LF95()
      FIRSTYR_VEC = 0
      STO_PTR = 0
      IF(LF95) THEN
         IF(WVPA()) THEN
            WRITE(SCREEN_MESSAGES,"(A)") "Work Orders"
         ELSE
            WRITE(SCREEN_MESSAGES,"(A)") "Future Asset Accounts"
         ENDIF
         CALL MG_LOCATE_WRITE(18,70,TRIM(SCREEN_MESSAGES),3,2)
      ENDIF
      FINANCIAL_SIMULATION_YEARS = MAX_FINANCIAL_SIMULATION_YEARS
      CAL_REAL_PROPERTY_TAX_VALUES = REAL_PROPERTY_TAX_VALUES()
      TEMP_CHR = CAPACITY_PLANNING_METHOD()
      NEXT_DECISION_CAPACITY_PLANNING = TEMP_CHR =='NE'
      CAP_PLANNING_METHOD_ANNDECOMP = TEMP_CHR == 'AN'
      BASEYEAR = get_BASE_YEAR()
      RPT_TITLE(1) = "Cash"
      RPT_TITLE(2) = "AFDC on Cash"
      RPT_TITLE(3) = "CWIP Balance"
      RPT_TITLE(4) = "Plant Entering Service"
      RPT_TITLE(5) = "AFUDC Capitialized with Plant"
      RPT_TITLE(6) = "Total Book Depreciation"
      RPT_TITLE(7) = "Deferred Tax Basis"
      RPT_TITLE(8) = "Tax Depreciation"
      RPT_TITLE(9) = "SL Tax Depreciation"
      RPT_TITLE(10) = "Tax Expenses"
      RPT_TITLE(11) = "ACE Depreciation"
      RPT_TITLE(12) = "Capitalized Interest"
      RPT_TITLE(13) = "Imputed Interest"
      RPT_TITLE(14) = "GPV Property Tax Basis"
      RPT_TITLE(15) = "NPV Property Tax Basis"

      ACCOUNTS_REPORTED = 0
!
      ALLOCATE(CEPTXBOK(FINANCIAL_SIMULATION_YEARS))
      ALLOCATE(TOTAL_BKDPTX(FINANCIAL_SIMULATION_YEARS))
      ALLOCATE(CURRENT_INTEREST_CAP(FINANCIAL_SIMULATION_YEARS))
      ALLOCATE(PCAPINRST(FINANCIAL_SIMULATION_YEARS))
      ALLOCATE(TAXDPALT(FINANCIAL_SIMULATION_YEARS))
      ALLOCATE(CAPINRST(FINANCIAL_SIMULATION_YEARS))
      ALLOCATE(TXPREFDEP(FINANCIAL_SIMULATION_YEARS))
      ALLOCATE(BOOK_VALUE(FINANCIAL_SIMULATION_YEARS))
      ALLOCATE(ACE_BOOK_DEP(FINANCIAL_SIMULATION_YEARS))
      ALLOCATE(TAX_VALUE_BEGINNING_90(FINANCIAL_SIMULATION_YEARS))
      ALLOCATE(DUMMY(FINANCIAL_SIMULATION_YEARS))
      AFDCDF = 0.
      TOTAL_BKDPTX = 0.
!
! MONTHLY VARIABLES
!
      ALLOCATE(BOOK_EXPEN(0:12,0:FINANCIAL_SIMULATION_YEARS))

      if(allocated(cash_om_adder)) then
        deallocate(cash_om_adder)
      end if
      ALLOCATE(CASH_OM_ADDER(0:12,0:FINANCIAL_SIMULATION_YEARS))
      if(allocated(non_cash_om_adder)) then
        deallocate(non_cash_om_adder)
      end if
      ALLOCATE(NON_CASH_OM_ADDER(0:12,0:FINANCIAL_SIMULATION_YEARS))
      if(allocated(PLANT_2_SERVICE)) then
        deallocate(PLANT_2_SERVICE)
      end if
      ALLOCATE(PLANT_2_SERVICE(0:12,0:FINANCIAL_SIMULATION_YEARS))
      
      if(allocated(PERCENT_PLANT_2_SERVICE)) then
        deallocate(PERCENT_PLANT_2_SERVICE)
      end if
      
      ALLOCATE(PERCENT_PLANT_2_SERVICE(0:12,0:FINANCIAL_SIMULATION_YEARS))
      if(allocated(MONTHLY_AFUDC_ON_CASH)) then
        deallocate(MONTHLY_AFUDC_ON_CASH)
      end if
      ALLOCATE(MONTHLY_AFUDC_ON_CASH(0:12,0:FINANCIAL_SIMULATION_YEARS))
      
      if(allocated(MONTHLY_AFUDC_ON_PLANT)) then
        deallocate(MONTHLY_AFUDC_ON_PLANT)
      end if
      ALLOCATE(MONTHLY_AFUDC_ON_PLANT(0:12,0:FINANCIAL_SIMULATION_YEARS))
      if(allocated(MONTHLY_CWIP)) then
        deallocate(MONTHLY_CWIP)
      end if
      ALLOCATE(MONTHLY_CWIP(0:12,0:FINANCIAL_SIMULATION_YEARS))
      if(allocated(MONTHLY_AFUDC_IN_CWIP)) then
        deallocate(MONTHLY_AFUDC_IN_CWIP)
      end if
      ALLOCATE(MONTHLY_AFUDC_IN_CWIP(0:12,0:FINANCIAL_SIMULATION_YEARS))
      if(allocated(MONTHLY_CWIP_IN_RATEBASE)) then
        deallocate(MONTHLY_CWIP_IN_RATEBASE)
      end if
      ALLOCATE(MONTHLY_CWIP_IN_RATEBASE(0:12,0:FINANCIAL_SIMULATION_YEARS))
      if(allocated(MONTHLY_CAPITALIZED_INTEREST)) then
        deallocate(MONTHLY_CAPITALIZED_INTEREST)
      end if
      ALLOCATE(MONTHLY_CAPITALIZED_INTEREST(0:12,0:FINANCIAL_SIMULATION_YEARS))
      if(allocated(MONTHLY_CURRENT_INTEREST)) then
        deallocate(MONTHLY_CURRENT_INTEREST)
      end if
      ALLOCATE(MONTHLY_CURRENT_INTEREST(0:12,0:FINANCIAL_SIMULATION_YEARS))
      if(allocated(CASH_EXPENDITURES)) then
        deallocate(CASH_EXPENDITURES)
      end if
      ALLOCATE(CASH_EXPENDITURES(0:12,0:FINANCIAL_SIMULATION_YEARS))
      if(allocated(MONTHLY_INTEREST_TO_TAX_VALUE)) then
        deallocate(MONTHLY_INTEREST_TO_TAX_VALUE)
      end if
      ALLOCATE(MONTHLY_INTEREST_TO_TAX_VALUE(0:12,0:FINANCIAL_SIMULATION_YEARS))
      if(allocated(MONTHLY_TAX_VALUE_OF_ASSET)) then
        deallocate(MONTHLY_TAX_VALUE_OF_ASSET)
      end if
      ALLOCATE(MONTHLY_TAX_VALUE_OF_ASSET(0:12,0:FINANCIAL_SIMULATION_YEARS))
      if(allocated(BONUS_DEP_MONTHLY_2001)) then
        deallocate(BONUS_DEP_MONTHLY_2001)
      end if
      ALLOCATE(BONUS_DEP_MONTHLY_2001(0:12,0:FINANCIAL_SIMULATION_YEARS))
      if(allocated(MONTHLY_TAX_DEP_PREFERENCE)) then
        deallocate(MONTHLY_TAX_DEP_PREFERENCE)
      end if
      ALLOCATE(MONTHLY_TAX_DEP_PREFERENCE(0:12,0:FINANCIAL_SIMULATION_YEARS))
      if(allocated(MONTHLY_TAX_DEPRECIATION_ALT)) then
        deallocate(MONTHLY_TAX_DEPRECIATION_ALT)
      end if
      ALLOCATE(MONTHLY_TAX_DEPRECIATION_ALT(0:12,0:FINANCIAL_SIMULATION_YEARS))
      if(allocated(MONTHLY_TAX_BOOK_DEP)) then
        deallocate(MONTHLY_TAX_BOOK_DEP)
      end if
      ALLOCATE(MONTHLY_TAX_BOOK_DEP(0:12,0:FINANCIAL_SIMULATION_YEARS))
      if(allocated(MONTHLY_ACE_BOOK_DEP)) then
        deallocate(MONTHLY_ACE_BOOK_DEP)
      end if
      ALLOCATE(MONTHLY_ACE_BOOK_DEP(0:12,0:FINANCIAL_SIMULATION_YEARS))
      if(allocated(DUMMY_MONTHLY_ARRAY)) then
        deallocate(DUMMY_MONTHLY_ARRAY)
      end if
      ALLOCATE(DUMMY_MONTHLY_ARRAY(0:12,0:FINANCIAL_SIMULATION_YEARS))
      if(allocated(MONTHLY_TAX_DEPRECIATION)) then
        deallocate(MONTHLY_TAX_DEPRECIATION)
      end if
      ALLOCATE(MONTHLY_TAX_DEPRECIATION(0:12,0:FINANCIAL_SIMULATION_YEARS))
      
      if(allocated(MONTHLY_DEFERRED_TAX_BASIS)) then
        deallocate(MONTHLY_DEFERRED_TAX_BASIS)
      end if
      ALLOCATE(MONTHLY_DEFERRED_TAX_BASIS(0:12,0:FINANCIAL_SIMULATION_YEARS))
      
      if(allocated(MONTHLY_TAX_EXPENSE)) then
        deallocate(MONTHLY_TAX_EXPENSE)
      end if
      
      ALLOCATE(MONTHLY_TAX_EXPENSE(0:12,0:FINANCIAL_SIMULATION_YEARS))
      
      if(allocated(MONTHLY_CURRENT_INTEREST_CAP)) then
        deallocate(MONTHLY_CURRENT_INTEREST_CAP)
      end if
      ALLOCATE(MONTHLY_CURRENT_INTEREST_CAP(0:12,0:FINANCIAL_SIMULATION_YEARS))
      if(allocated(MONTHLY_BOOK_DEP)) then
        deallocate(MONTHLY_BOOK_DEP)
      end if
      ALLOCATE(MONTHLY_BOOK_DEP(0:12,0:FINANCIAL_SIMULATION_YEARS))
      
      if(allocated(CUM_BOOK_DEP)) then
        deallocate(CUM_BOOK_DEP)
      end if
      ALLOCATE(CUM_BOOK_DEP(0:12,0:FINANCIAL_SIMULATION_YEARS))
      if(allocated(GPV)) then
        deallocate(GPV)
      end if
      ALLOCATE(GPV(0:12,0:FINANCIAL_SIMULATION_YEARS))
      if(allocated(NPV)) then
        deallocate(NPV)
      end if
      ALLOCATE(NPV(0:12,0:FINANCIAL_SIMULATION_YEARS))
      if(allocated(TEMP_PLANT_2_SERVICE)) then
        deallocate(TEMP_PLANT_2_SERVICE)
      end if
      ALLOCATE(TEMP_PLANT_2_SERVICE(0:FINANCIAL_SIMULATION_YEARS))
!
! READ A DATABASE RECORD
!
      FUTURE_ASSET_REPORT = FUTURE_ASSETS_REPORT(REPORT_ALL_ACCOUNTS) &
      	 .AND.  .NOT. SAVE_BASE_CASE
      REPORT_ALL_ACCOUNTS = REPORT_ALL_ACCOUNTS .AND. FUTURE_ASSET_REPORT
      IF(FUTURE_ASSET_REPORT) THEN
         DRILLING_REPRT_LEVEL = FINANCIAL_DRILLING()
!
      ENDIF
      IF(FUTURE_ASSET_REPORT .AND. .NOT. REPORT_HEADER_OPEN) THEN
         IF(DRILLING_REPRT_LEVEL == 'M' .AND. MONTHLY_MIDAS_ACTIVE) THEN
            FA_REPORTING_UNIT=FUTURE_ASSET_RPT_HEADER(FA_REPORTING_REC, &
            	MONTHLY_DETAIL)
            ASSET_REPORTING_LEVEL = 'M'
         ELSE
            FA_REPORTING_UNIT=FUTURE_ASSET_RPT_HEADER(FA_REPORTING_REC, &
            	ANNUAL_DETAIL)
            ASSET_REPORTING_LEVEL = 'A'
         ENDIF
         REPORT_HEADER_OPEN = .TRUE.
      ENDIF
      IF(.NOT. WVPA_CONTROL_FILE_OPEN .AND.  &
      	FUTURE_ASSET_REPORT .AND. WVPA()) THEN
         WVPA_WORKORDER_CONTRL_UNIT =  &
         	WVPA_WORKORDER_CONTROL_RPT(WVPA_WORKORDER_CONTRL_REC)
         WVPA_CONTROL_FILE_OPEN = .TRUE. 
      ENDIF
      IF(.NOT. WVPA_WO_EXPENSES_FILE_OPEN .AND. &
      	FUTURE_ASSET_REPORT .AND. WVPA()) THEN
         WVPA_WORKORDER_EXPENSES_UNIT =  &
         	WVPA_WORKORDER_EXPENSES_RPT(WVPA_WORKORDER_EXPENSES_REC)
         WVPA_WO_EXPENSES_FILE_OPEN = .TRUE.
      ENDIF
      IF(.NOT. WVPA_WO_SUMMARY_FILE_OPEN .AND. &
      	FUTURE_ASSET_REPORT .AND. WVPA()) THEN
         WVPA_WORKORDER_SUMARY_RPRT_UNIT = &
         	WVPA_WORKORDER_SUMMARY_RPT(WVPA_WORKORDER_SUMMARY_RPRT_REC)
         WVPA_WO_SUMMARY_FILE_OPEN = .TRUE.
      ENDIF
!
      ALLOCATE(VECTOR_DATA_CASH(AVAIL_DATA_YEARS))
      ALLOCATE(VECTOR_DATA_PLNT(AVAIL_DATA_YEARS))
      ALLOCATE(VECTOR_DATA(AVAIL_DATA_YEARS))
!
      CALL FA_STZERO
      IF(SAVE_BASE_CASE) FIRST_PASS_NEW_ADDITIONS = .TRUE.
      MIDAS_ADDED_INVESTMENT = .FALSE.
      IF(PROCESSING_NEW_ADDITIONS) THEN
         IF(FIRST_PASS_NEW_ADDITIONS) THEN
            CALL SET_UP_MODEL_ASSET_ARRAYS
            FIRST_PASS_NEW_ADDITIONS = .FALSE.
         END IF
         CALL INIT_MODEL_ASSET_ARRAYS
         CALL WHAT_INVESTMENTS_ARE_THESE(MIDAS_ADDED_INVESTMENT) 
         IF(MIDAS_ADDED_INVESTMENT) THEN
            CALL OPEN_MODEL_ADDED_ASSET_FILE(INT(11,2))
            CALL LAST_PROJECTS_IN_PROCESS_RECORD(LAST_FA_RECORD)
         ELSE
            LAST_FA_RECORD = 0
         ENDIF
         TACCTS = ASSETS_IN_BASE_FILE
         AVAIL_FILES = 0
      ELSE
         CALL SET_UP_FA_ARRAYS
         CALL INIT_FA_ARRAYS
!
         LAST_FA_RECORD = 32000
         AVAIL_FILES = 1
!
         TACCTS = 0
         IF(MONTHLY_MIDAS_ACTIVE) THEN
            CALL MONTHLY_FUTURE_ASSETS_OBJECT
         ENDIF
      ENDIF
!
!
      DO FILE_ID = 0, AVAIL_FILES
         IF(.NOT. PROCESSING_NEW_ADDITIONS) THEN
            CALL OPEN_FUTURE_ASSET_FILE(INT(11,2),FILE_ID,FILE_EXISTS)
            IF(.NOT. FILE_EXISTS) CYCLE
         ENDIF
       IREC = 0
       DO WHILE (IREC < LAST_FA_RECORD)
         IREC = IREC + 1
         CEP = 0.
         CE = 0.
         TEMP_PLANT_2_SERVICE = 0.
         MONTHLY_BOOK_DEP = 0.
         PLANT_2_SERVICE = 0.
         PERCENT_PLANT_2_SERVICE = 0.
         MONTHLY_AFUDC_ON_CASH = 0.
         MONTHLY_AFUDC_ON_PLANT = 0.
         MONTHLY_CWIP = 0.
         MONTHLY_AFUDC_IN_CWIP = 0.
         MONTHLY_CWIP_IN_RATEBASE = 0.
         MONTHLY_CAPITALIZED_INTEREST = 0.
         MONTHLY_CURRENT_INTEREST = 0.
         MONTHLY_INTEREST_TO_TAX_VALUE = 0.
         MONTHLY_TAX_VALUE_OF_ASSET = 0.
         BONUS_DEP_MONTHLY_2001 = 0.
         MONTHLY_TAX_BOOK_DEP = 0.
         DUMMY_MONTHLY_ARRAY = 0.
         MONTHLY_TAX_DEP_PREFERENCE = 0.
         MONTHLY_TAX_DEPRECIATION_ALT = 0.
         MONTHLY_ACE_BOOK_DEP = 0.
         MONTHLY_DEFERRED_TAX_BASIS = 0.
         MONTHLY_TAX_DEPRECIATION = 0.
         MONTHLY_TAX_EXPENSE = 0.
         MONTHLY_CURRENT_INTEREST_CAP = 0.
         CASH_EXPENDITURES = 0.
         MONTHLY_BOOK_DEP = 0.
         CUM_BOOK_DEP = 0.
         GPV = 0.
         NPV = 0.
         BOOK_EXPEN = 0.
         CASH_OM_ADDER = 0.
         NON_CASH_OM_ADDER = 0.
         MONTHLY_CASH_VALUES_EXIST = .FALSE.
         MONTHLY_PLANT_IN_SERVICE_EXIST = .FALSE.
         DBRATE = 0.
         OHRATE = 0.
         PCITC  = 0.
         ITC_RATE = 0.
         ABTAX  = 0.
!
         IF(PROCESSING_NEW_ADDITIONS) THEN
            READ(11,REC=IREC,IOSTAT=IOS) DELETE,CE,CEP, &
                             OPTION_NAME,ACCTNO,BOKLF, &
                             TAXLF,ADRLIFE,DEPMET,DBRATE, &
                             CWIPRB,AFDCSW, &
                             CONSTRUCTION_PERIOD,AFDCPERIODS, &
                             ABYEAR,ABMETH,ABACCT,ABTAX,WOYRS, &
                             SERVICEMO,ON_LINE_YR,FIRSTYR_VEC, &
                             OHRATE,REGULATORY_ALLOCATOR,PV_PLANT_COST, &
                             CONSTRUCTION_ESCALATION_VECTOR, &
                             TAX_NORMALIZATION_PEROID,SALVAGE_VALUE, &
                             CONSTRUCTION_YEARS_COMPLETED, &
                             CASH,ACCELERATE_FIXED_CHARGE, &
                             ACCELERATE_CHARGE_PERCENT, &
                             DELAY_FIXED_CHARGE, &
                             DELAY_CHARGE_PERCENT, &
                             ABANDONMENT_FIXED_CHARGE, &
                             ABANDONMENT_CHARGE_PERCENT, &
                             TAKE_BONUS_TAX_DEPRECIATION, &
                             ASSET_CLASS_NUM,ASSET_CLASS_VECTOR
            IF(IOS /= 0) EXIT
            IF(NEXT_DECISION_CAPACITY_PLANNING) THEN
               IF(FIRSTYR_VEC > BASEYEAR + globecom_YEAR) then
                CYCLE
               end if
               CE(1) = CE(1) + CE(1+globecom_YEAR)
               WRITE(11,REC=IREC) DELETE,CE,CEP, &
                                OPTION_NAME,ACCTNO,BOKLF, &
                                TAXLF,ADRLIFE,DEPMET,DBRATE, &
                                CWIPRB,AFDCSW,CONSTRUCTION_PERIOD, &
                                AFDCPERIODS, &
                                ABYEAR,ABMETH,ABACCT,ABTAX,WOYRS, &
                                SERVICEMO,ON_LINE_YR,FIRSTYR_VEC, &
                                OHRATE,REGULATORY_ALLOCATOR, &
                                PV_PLANT_COST, &
                                CONSTRUCTION_ESCALATION_VECTOR, &
                                TAX_NORMALIZATION_PEROID,SALVAGE_VALUE, &
                                CONSTRUCTION_YEARS_COMPLETED,CASH, &
                                ACCELERATE_FIXED_CHARGE, &
                                ACCELERATE_CHARGE_PERCENT, &
                                DELAY_FIXED_CHARGE,DELAY_CHARGE_PERCENT, &
                                ABANDONMENT_FIXED_CHARGE, &
                                ABANDONMENT_CHARGE_PERCENT, &
                                TAKE_BONUS_TAX_DEPRECIATION, &
                                ASSET_CLASS_NUM, &
                                ASSET_CLASS_VECTOR
            ENDIF
            TACCTS = TACCTS + 1
            FIRST_YEARS_VEC(1) = FIRSTYR_VEC
            IF(.NOT. CAP_PLANNING_METHOD_ANNDECOMP .OR. SAVE_BASE_CASE) THEN
               IF(LF95) THEN
                  WRITE(SCREEN_MESSAGES,"(I4,A)")TACCTS,"-"//OPTION_NAME 
                  CALL MG_LOCATE_WRITE(18,70,TRIM(SCREEN_MESSAGES),3,0)
               ELSE
                  WRITE(SCREEN_MESSAGES,"(I4)") TACCTS
                  CALL MG_LOCATE_WRITE(18,70,TRIM(SCREEN_MESSAGES), &
                  	ALL_VERSIONS,0)
               ENDIF
            ENDIF
            IF(DELETE > 7) CYCLE  !GOTO 10
            DATA_TYPE = 'D' ! 'R'
            ACCOUNT_ACTIVE = 'A'
            LAST_YEAR = 1
            CWIP1 = 0.
            AFUDC1 = 0.
            AFDCB1 = 0.
            TAXEXP(1) = 0.
            CASH_VECT = 0
            AFDC_CAP_VECTOR = 0
            PLANT_VECT = 0
            TEMP_CWIP = 0.
            BOOK_DEP_METHOD = 'R' !emaining
            TAX_BASIS_ADJUSTOR = 100.
            LAG_STR = '100'
            CASH_STR = '0'
            REGULATORY_ALLOCATOR = 100.
            HALF_YEAR_BOOK_DEP = SERVICEMO < 0
            SERVICEMO = ABS(SERVICEMO)
            DO I = 2, MAX_FINANCIAL_SIMULATION_YEARS 
               TEMP_CWIP  = TEMP_CWIP + CE(I)
               CEP(I) = MIN(CEP(I),1.) * TEMP_CWIP
               TEMP_CWIP = TEMP_CWIP - CEP(I)
               BOOK_EXPEN(0,I-1) = CE(I)
               PLANT_2_SERVICE(0,I-1) = CEP(I)
            ENDDO
         ELSE
            CASH = 0.
            PLANT = 0.
            CE = 0.
            CEP = 0.
            READ(11,REC=IREC,IOSTAT=IOS) DELETE,ACCTNO,DATA_TYPE,BOKLF, &
                  TAXLF,ADRLIFE,DEPMET,DBRATE,CWIP1,CWIPRB,AFUDC1, &
                  AFDCB1,AFDCSW,CONSTRUCTION_PERIOD,AFDCPERIODS, &
                  ABYEAR,ABMETH,ABACCT,ABTAX,WOYRS,CASH_VECT,SERVICEMO, &
                  FIRSTYR_VEC,ACCOUNT_ACTIVE,OHRATE,TAXEXP(1), &
                  REGULATORY_ALLOCATOR,PV_PLANT_COST, &
                  CONSTRUCT_ESCALAT_VECTOR_RATE,PLANT,CASH,DESC, &
                  AFDC_CAP_VECTOR,PLANT_VECT, &
                  TAX_NORMALIZATION_PEROID,SALVAGE_VALUE, &
                  ASSET_CLASS_NUM,ASSET_CLASS_VECTOR, &
                  BOOK_DEP_METHOD,TAX_BASIS_ADJUSTOR,LAG_STR,CASH_STR, &
                  TAKE_BONUS_TAX_DEPRECIATION, &
                  WVPA_SUB_ACCOUNT_NUMBER,WVPA_DEPARTMENT_HEAD, &
                  PROJECT_COMPLETION_MO,PROJECT_COMPLETION_YR, &
                  WVPA_WORK_ORDER_NUMBER, &
                  WVPA_BUDGET_ITEM_NUM, &
                  OM_CAPITIALIZED_CASH,OM_CAPITIALIZED_NON_CASH, &
                  WVPA_COMPANY_ID, &     ! 79
                  OVN_CAPITAL_COST_STOCASTIC, & ! 80
                  PCITC,ITC_RATE
            IF(IOS /= 0) THEN
               CALL iostatmsg_unit(IOS,ERR_MSG)
               EXIT !RECORD LOOP
            ENDIF
            IF(DELETE > 7 .OR. ACCOUNT_ACTIVE == 'N') CYCLE

            IF(FIRSTYR_VEC > LAST_DATA_INPUT_YR) CYCLE
            OPTION_NAME = DESC
            TACCTS = TACCTS + 1
            HALF_YEAR_BOOK_DEP = SERVICEMO < 0
            SERVICEMO = ABS(SERVICEMO)
            PROJECT_COMPLETION_MO = SERVICEMO 
            IF(.NOT. CAP_PLANNING_METHOD_ANNDECOMP .OR. &
            	SAVE_BASE_CASE) THEN
               IF(LF95 .AND. TACCTS > 0) THEN
                  WRITE(SCREEN_MESSAGES,"(I4,A)") TACCTS,"-"//DESC 
                  CALL MG_LOCATE_WRITE(18,70,TRIM(SCREEN_MESSAGES),3,0)
               ELSEIF(.NOT. LF95 .AND. TACCTS > 0) THEN
                  WRITE(SCREEN_MESSAGES,"(I4)") TACCTS
                  CALL MG_LOCATE_WRITE(18,70,TRIM(SCREEN_MESSAGES), &
                  	ALL_VERSIONS,0)
               ENDIF
            ENDIF
!
            IF(ABYEAR > gc_last_study_year) ABYEAR = 3050
            LAST_YEAR = 1
            FIRST_YEARS_VEC(1) = FIRSTYR_VEC
            FIRSTYR = FIRSTYR_VEC
            IOFFSET = MIN(MAX_FINANCIAL_SIMULATION_YEARS,MAX(FIRSTYR - &
            	BASEYEAR,INT(1,2)))
            IF(IOFFSET >= MAX_FINANCIAL_SIMULATION_YEARS) CYCLE
            IEND = MAX(INT(0,2),MIN(INT(15,2), &
            	MAX_FINANCIAL_SIMULATION_YEARS-IOFFSET))
            IEND = MAX(INT(0,2),MIN(INT(30,2), &
            	MAX_FINANCIAL_SIMULATION_YEARS-IOFFSET))
            BOOK_EXPEN = 0.
            IF(CASH_VECT /= 0) THEN
               IVEC = ABS(CASH_VECT)
!
!
               CALL GET_MONTHLY_ANNUAL_VALUES(IVEC, &
                                              CASH_VECTOR_DATA_TYPE, &
                                              VECTOR_TYPE, &
                                              VECTOR_DATA, &
                                              MONTHLY_VECTOR_DATA(1,1), &
                                              CASH_MONTHLY_DATA_UNITS, &
                                              CASH_MONTH_ENDING)
               CALL IS_THERE_MONTHLY_INFORMATION(MONTHLY_CASH_VALUES_EXIST)
               CALL MOVE_ANNUAL_INPUT(BOOK_EXPEN, &
                                      VECTOR_DATA, &
                                      IOFFSET, &
                                      FINANCIAL_SIMULATION_YEARS)
            ELSE
               VECTOR_DATA = 0.
               VECTOR_DATA(1:15) = CASH(1:15)
               CALL MOVE_ANNUAL_INPUT(BOOK_EXPEN, &
                                      VECTOR_DATA, &
                                      IOFFSET, &
                                      FINANCIAL_SIMULATION_YEARS) ! IEND)
            ENDIF
            IF(DATA_TYPE == 'P' .OR. DATA_TYPE == 'B') THEN
!
!
! 110806. FILE_ID == 1 IS CURRENTLY FX OR TH CAPEX GENERATED FILE.
!         THERE APPEARS TO BE A VALID VALUE PASSED FOR ESCALATION RATE.
!
!     
                  CONSTRUCT_ESCALAT_VECTOR_RATE = &
                  	-CONSTRUCT_ESCALAT_VECTOR_RATE

! MODIFY QUANTIES USING LATIN HYPERCUBE

               IF(INDEX(OVN_CAPITAL_COST_STOCASTIC,"Not Active") == 0) THEN
                  SCENARIO_INDEX = GET_SCENARIO_INDEX(OVN_CAPITAL_COST_STOCASTIC)
                  IF(SCENARIO_INDEX <= 0) EXIT
                  MO = 1
                  DO STO_PTR = 1, 29
                     IF(BOOK_EXPEN(0,STO_PTR) /= 0.) EXIT
                  ENDDO
                  PV_PLANT_COST = PV_PLANT_COST &
                  	* GET_SCENARIO_BY_INDEX(STO_PTR,MO,SCENARIO_INDEX)
               ENDIF
! END LHC MODIFICATIONS
               CALL S_CURVE_2_DOLLARS_VECTOR_RATE(PV_PLANT_COST, &
                                    CONSTRUCT_ESCALAT_VECTOR_RATE, &
                                    BOOK_EXPEN, &
                                    MAX_FINANCIAL_SIMULATION_YEARS)
            ENDIF
!
! NOW HAVE ANNUAL CASH AMOUNT IS MILLIONS OF DOLLARS
! NEED TO DISTRIBUTE AMOUNT MONTHS
!
!
            IF(CASH_VECT /= 0 .AND. (MONTHLY_MIDAS_ACTIVE .OR. &
            	 MONTHLY_CASH_VALUES_EXIST)) THEN
!
               CALL RIPPLE_MOVE_MONTHLY_DATA(BOOK_EXPEN, &
                                             MONTHLY_VECTOR_DATA, &
                                             IOFFSET, &
                                             FINANCIAL_SIMULATION_YEARS, &
                                             RIPPLE_ZERO)
               IF(CASH_MONTHLY_DATA_UNITS(1) == 'D' .AND.  & !CHECK FOR NOT ZERO
                          (DATA_TYPE == 'P' .OR. DATA_TYPE == 'B')) THEN
                  BOOK_EXPEN(0,IOFFSET) = SUM(BOOK_EXPEN(1:,IOFFSET))
                  IF(BOOK_EXPEN(0,IOFFSET) == 0.) THEN
                     CASH_MONTHLY_DATA_UNITS(1) = 'A'
                  ELSE
                     CASH_MONTHLY_DATA_UNITS(1) = 'N'
                  ENDIF
               ENDIF
               J = 1
               PROJECT_SUM = CWIP1
               DO YR = IOFFSET, IOFFSET+4
                  IF(CASH_MONTHLY_DATA_UNITS(YR-IOFFSET+1) /= 'D') THEN
                     IF(WVPA()) THEN
                        CALL MONTHLY_BOOK_VALUES_4_YEAR( &
                                             BOOK_EXPEN(0,YR), &
                                             CASH_MONTHLY_DATA_UNITS(J), &
                                             CASH_MONTH_ENDING(J))
                     ELSE
                        CYCLE
                     ENDIF
                  ENDIF
!
                  J = J + 1
                  TEMP_SUM = SUM(BOOK_EXPEN(1:,YR))
                  PROJECT_SUM = PROJECT_SUM + TEMP_SUM
!
! WVPA BUGET CONTROL REPORT
!
                  IF(WVPA() .AND. FUTURE_ASSET_REPORT) THEN
!
                     WORKORDER_NAME = DESC
                     IF(WVPA()) THEN
                        DRILLING_ACCOUNT_NAME = &
                           WVPA_DEPART_EXP_REPORT_NAME(DESC, &
                                                 ACCTNO, &
                                                 WVPA_BUDGET_ITEM_NUM, &
                                                 WVPA_WORK_ORDER_NUMBER)
                     ELSE
                        DRILLING_ACCOUNT_NAME = OPTION_NAME
                        WRITE(TEMP_STR,'(I4)') IREC
                        DRILLING_ACCOUNT_NAME = &
                        	DRILLING_ACCOUNT_NAME &
                        		(1:DrillingAccountNameWidth-4)//TEMP_STR
                     ENDIF
                     WRITE(INDEXING_STR,'(I4)') get_BASE_YEAR()+YR
!
                     WRITE(WVPA_WORKORDER_EXPENSES_UNIT, &
                                   REC=WVPA_WORKORDER_EXPENSES_REC) &
                                        PRT_ENDPOINT(), &
                                        WVPA_DEPARTMENT_HEAD, &
                                        FLOAT(get_BASE_YEAR()+YR), &
                                        DRILLING_ACCOUNT_NAME, & 
                                        FLOAT(PROJECT_COMPLETION_MO), &
                                        FLOAT(PROJECT_COMPLETION_YR), &
                                        FLOAT(get_BASE_YEAR()+YR), &
                                        BOOK_EXPEN(0,YR), &
                                        TEMP_SUM, &
                                        BOOK_EXPEN(0,YR) - TEMP_SUM, &
                                       (BOOK_EXPEN(MO,YR),MO=1,12)
     
                     WVPA_WORKORDER_EXPENSES_REC = WVPA_WORKORDER_EXPENSES_REC + 1
                  ENDIF
                  BOOK_EXPEN(0,YR) = TEMP_SUM
               ENDDO
 
!
! WVPA WORKORDER CONTROL REPORT
!
                  IF(WVPA() .AND. FUTURE_ASSET_REPORT) THEN
                     PROJECT_SUM = SUM(BOOK_EXPEN(0,IOFFSET+5:)) + PROJECT_SUM
                     WRITE(INDEXING_STR,'(I4)') PROJECT_COMPLETION_YR
!
!
                     DRILLING_ACCOUNT_NAME = &
                           WVPA_WORK_ORDER_REPORT_NAME(DESC, &
                                                 ACCTNO, &
                                                 WVPA_BUDGET_ITEM_NUM, &
                                                 WVPA_WORK_ORDER_NUMBER)
                     WRITE(WVPA_WORKORDER_CONTRL_UNIT, &
                                   REC = WVPA_WORKORDER_CONTRL_REC) &
                                        PRT_ENDPOINT(), &
                                        WVPA_DEPARTMENT_HEAD, &
!
                                        DRILLING_ACCOUNT_NAME, &
                                        FLOAT(PROJECT_COMPLETION_MO), &
                                        FLOAT(PROJECT_COMPLETION_YR), &
!
                                        PV_PLANT_COST, &
                                        PROJECT_SUM, &
                                        PV_PLANT_COST - PROJECT_SUM 

                     WVPA_WORKORDER_CONTRL_REC = WVPA_WORKORDER_CONTRL_REC + 1
                     IF(ABS(PV_PLANT_COST-PROJECT_SUM) > .0005) THEN
                        WRITE(WVPA_WORKORDER_CONTRL_UNIT, &
                                   REC=WVPA_WORKORDER_CONTRL_REC) &
                                      PRT_ENDPOINT(), &
                                      'UnBalanced     ', &
!
                                      DRILLING_ACCOUNT_NAME, & ! SAME AS ABOVE
                                      FLOAT(PROJECT_COMPLETION_MO), &
                                      FLOAT(PROJECT_COMPLETION_YR), &
!
                                      PV_PLANT_COST, &
                                      PROJECT_SUM, &
                                      PV_PLANT_COST - PROJECT_SUM
!
                        WVPA_WORKORDER_CONTRL_REC = WVPA_WORKORDER_CONTRL_REC + 1
                     ENDIF
                  ENDIF
!
            ENDIF
!
! CAPITIALIZED O&M ADDERS
!
            CASH_OM_ADDER = OM_CAPITIALIZED_CASH * BOOK_EXPEN
            NON_CASH_OM_ADDER = OM_CAPITIALIZED_NON_CASH * BOOK_EXPEN
!
! SET-UP PLANT VALUES
!
            PLANT_2_SERVICE = 0.
            IF(PLANT_VECT /= 0) THEN
               IVEC = ABS(PLANT_VECT)
!
!
! RULE IF DATA_TYPE IS NOT D THEN MONTHLY PES PATTERNS ARE NOT ALLOWED
!
               CALL GET_MONTHLY_ANNUAL_VALUES(IVEC, &
                                              PLANT_VECTOR_DATA_TYPE, &
                                              VECTOR_TYPE, &
                                              VECTOR_DATA, &
                                              MONTHLY_VECTOR_DATA(1,1), &
                                              PLANT_MONTHLY_DATA_UNITS, &
                                              PLANT_MONTH_ENDING)
               CALL IS_THERE_MONTHLY_INFORMATION( &
               	MONTHLY_PLANT_IN_SERVICE_EXIST)
!
               IF(MONTHLY_MIDAS_ACTIVE .OR. &
               	MONTHLY_PLANT_IN_SERVICE_EXIST) THEN
                     CALL RIPPLE_MONTHLY_DATA(VECTOR_DATA, &
                     	MONTHLY_VECTOR_DATA)
                     CALL MOVE_ANNUAL_MONTHLY_INPUT(PLANT_2_SERVICE, &
                                             VECTOR_DATA, &
                                             MONTHLY_VECTOR_DATA, &
                                             IOFFSET, &
                                             FINANCIAL_SIMULATION_YEARS, &
                                             SERVICEMO)
!
!
               ELSE
                  CALL MOVE_ANNUAL_INPUT(PLANT_2_SERVICE, &
                                         VECTOR_DATA, &
                                         IOFFSET, &
                                         FINANCIAL_SIMULATION_YEARS)
!
               ENDIF
            ELSEIF(WVPA()) THEN
               DATA_TYPE = 'R'
               MO_START = SERVICEMO
               MONTHLY_PLANT_IN_SERVICE_EXIST =  .TRUE.
               DO YR = IOFFSET, FINANCIAL_SIMULATION_YEARS
                  IF(get_BASE_YEAR() + YR < PROJECT_COMPLETION_YR) CYCLE
                  PLANT_2_SERVICE(0,YR) = 100.
                  DO MO = MO_START, 12 
                     PLANT_2_SERVICE(MO,YR) = 100.
                  ENDDO
                  MO_START = 1
               ENDDO
            ELSE
               VECTOR_DATA = 0.
               VECTOR_DATA(1:15) = PLANT(1:15)
               CALL MOVE_ANNUAL_INPUT(PLANT_2_SERVICE, &
                                      VECTOR_DATA, &
                                      IOFFSET, &
                                      FINANCIAL_SIMULATION_YEARS)
            ENDIF
         ENDIF ! PROCESSING_NEW_ADDITIONS VS FA INVESTMENTS
!
         CASH_LAG_PATTERN = 0.
         CASH_PAYMENTS = 0.
         IF(MONTHLY_MIDAS_ACTIVE) THEN
! PROCESS THE LAG INFORMATION
            READ_STR = TRIM(LAG_STR)//',,,,,,,,,,,,,,,,,,,,,,,,,'
            READ(READ_STR,*) CASH_LAG_PATTERN
            CALL CHECK_CASH_LAG_PATTERN(CASH_LAG_PATTERN)
            READ_STR = TRIM(CASH_STR)//',,,,,,,,,,,,,,,,,,,,,,,,,'
            READ(READ_STR,*) CASH_PAYMENTS
         ENDIF
         DBRATE = DBRATE/100.
         OHRATE = OHRATE/100.
         PCITC  = PCITC/100.
         ITC_RATE = ITC_RATE/100.
         ABTAX  = ABTAX/100.
         AFDCFC = CONSTRUCTION_PERIOD/2.
         REGULATORY_ALLOCATOR = REGULATORY_ALLOCATOR/100.
         TAX_BASIS_ADJUSTOR = TAX_BASIS_ADJUSTOR/100.
         DPBAIS = 'F'    ! REMOVED FROM MIDAS GOLD ON 7/24/91
         IF(WVPA()) THEN
            DRILLING_ACCOUNT_NAME = &
                     WVPA_DEPART_EXP_REPORT_NAME(DESC, &
                                                 ACCTNO, &
                                                 WVPA_BUDGET_ITEM_NUM, &
                                                 WVPA_WORK_ORDER_NUMBER)
         ELSE
            DRILLING_ACCOUNT_NAME = OPTION_NAME
            WRITE(TEMP_STR,'(I4)') IREC
            DRILLING_ACCOUNT_NAME = DRILLING_ACCOUNT_NAME &
            	(1:DrillingAccountNameWidth-4)//TEMP_STR
         ENDIF
!
         DO START_YEARS = 1, LAST_YEAR
            FIRSTYR = FIRST_YEARS_VEC(START_YEARS)
            IF(PROCESSING_NEW_ADDITIONS) THEN
               TEMP_CWIP = 0.
               DO YR = 1, MAX_FINANCIAL_SIMULATION_YEARS 
                  TEMP_CWIP  = TEMP_CWIP + BOOK_EXPEN(0,YR)
          TEMP_PLANT_2_SERVICE(YR) = MIN(PLANT_2_SERVICE(0,YR),TEMP_CWIP)
                  TEMP_CWIP = TEMP_CWIP - TEMP_PLANT_2_SERVICE(YR)
               ENDDO
            ELSE
!
               IF(DATA_TYPE == 'P' .AND. AFDCSW /= 0) AFDCSW = 2
               IF((PLANT_VECT == 0 .AND. .NOT. WVPA()) .OR. &
                         .NOT. MONTHLY_MIDAS_ACTIVE .OR. &
                              .NOT. MONTHLY_PLANT_IN_SERVICE_EXIST) THEN
                  TEMP_CWIP = CWIP1
                  DO YR = 1, MAX_FINANCIAL_SIMULATION_YEARS 
                     TEMP_CWIP  = TEMP_CWIP + BOOK_EXPEN(0,YR)
                     IF(INDEX('RPB',DATA_TYPE) /= 0) THEN
                        TEMP_PLANT_2_SERVICE(YR) = TEMP_CWIP/100. * &
                        	MIN(PLANT_2_SERVICE(0,YR),100.)
                     ELSEIF(DATA_TYPE == 'C') THEN
                        TEMP_PLANT_2_SERVICE(YR) = &
                              MIN(PLANT_2_SERVICE(0,YR)* &
                                         BOOK_EXPEN(0,YR)/100, &
                                                      MAX(TEMP_CWIP,0.))
                     ELSE
                        TEMP_PLANT_2_SERVICE(YR) = &
                                    MIN(PLANT_2_SERVICE(0,YR), &
                                                      MAX(TEMP_CWIP,0.))
                     ENDIF
                     TEMP_CWIP = TEMP_CWIP - TEMP_PLANT_2_SERVICE(YR)
                  ENDDO
               ENDIF
            ENDIF
!
! MOVE PLANT TO MONTHLY VIEW
!
            IF(PLANT_VECT /= 0 .AND. (MONTHLY_MIDAS_ACTIVE .OR. &
            	MONTHLY_PLANT_IN_SERVICE_EXIST)) THEN
               IF(DATA_TYPE == 'D') THEN
                  CALL TREND_MONTHLY_INPUT(PLANT_2_SERVICE, &
                                        PLANT_MONTHLY_DATA_UNITS, &
                                        PLANT_MONTH_ENDING, &
                                        IOFFSET, &
                                        SERVICEMO, &
                                        MAX_FINANCIAL_SIMULATION_YEARS)
               ENDIF
            ELSEIF(.NOT. WVPA()) THEN 
               DO YR = 1, FINANCIAL_SIMULATION_YEARS 
                  PLANT_2_SERVICE(MIN(INT(12,2),SERVICEMO),YR) = &
                  	PLANT_2_SERVICE(0,YR)
               ENDDO
            ENDIF
!
! MOVE CASH TO MONTHLY VIEW
!
!
            IF(CASH_VECT /= 0 .AND. (MONTHLY_MIDAS_ACTIVE .OR. &
            	MONTHLY_CASH_VALUES_EXIST)) THEN
               IF(WVPA()) THEN
                  CALL WVPA_TREND_MONTHLY_EXPEN_INPUT(BOOK_EXPEN, &
                                         CASH_MONTHLY_DATA_UNITS, &
                                         CASH_MONTH_ENDING, &
                                         IOFFSET, &
                                         MAX_FINANCIAL_SIMULATION_YEARS)
               ELSE
                  IF(INDEX('RPB',DATA_TYPE) /= 0) THEN
                     CALL WVPA_TREND_MONTHLY_EXPEN_INPUT(BOOK_EXPEN, &
                                         CASH_MONTHLY_DATA_UNITS, &
                                         CASH_MONTH_ENDING, &
                                         IOFFSET, &
                                         MAX_FINANCIAL_SIMULATION_YEARS)

                  ELSE
                     CALL TREND_MONTHLY_EXPEN_INPUT(BOOK_EXPEN, &
                                         CASH_MONTHLY_DATA_UNITS, &
                                         CASH_MONTH_ENDING, &
                                         IOFFSET, &
                                         MAX_FINANCIAL_SIMULATION_YEARS, &
                                         SERVICEMO, &
                                         CWIP1, &
                                         PLANT_2_SERVICE)
                  ENDIF
               ENDIF
            ELSE
               TEMP_CWIP = CWIP1
               IF(PLANT_VECT == 0 .OR. .NOT. MONTHLY_MIDAS_ACTIVE .OR. &
               	   .NOT. MONTHLY_CASH_VALUES_EXIST) THEN
                  DO YR = 1, MAX_FINANCIAL_SIMULATION_YEARS
                     IF(TEMP_PLANT_2_SERVICE(YR) /= 0.) THEN
                        IF(TEMP_CWIP > TEMP_PLANT_2_SERVICE(YR)) THEN
                           DO MO = 1, 12
                              BOOK_EXPEN(MO,YR) = BOOK_EXPEN(0,YR)/12.
                           ENDDO
                        ELSE
                           IF(SERVICEMO == 1) THEN
                              BOOK_EXPEN(1,YR) = BOOK_EXPEN(0,YR)
!
                              PERIOD = 12.
                           ELSEIF(SERVICEMO == 13) THEN
                              DO MO = 1, 12
                     BOOK_EXPEN(MO,YR) = BOOK_EXPEN(0,YR)/12.
                              ENDDO
                           ELSE
                              PERIOD = FLOAT(SERVICEMO)
                              PERIOD_AMOUNT = TEMP_PLANT_2_SERVICE(YR) &
                              	-TEMP_CWIP
                              DO MO = 1, SERVICEMO 
                                 BOOK_EXPEN(MO,YR)=PERIOD_AMOUNT/PERIOD
                              ENDDO
                              PERIOD = FLOAT(12-SERVICEMO)
                              PERIOD_AMOUNT = BOOK_EXPEN(0,YR) - &
                              	 (TEMP_PLANT_2_SERVICE(YR)-TEMP_CWIP)
                              IF(PERIOD_AMOUNT<.00001) PERIOD_AMOUNT=0.
                              DO MO = SERVICEMO+1, 12 
                                 BOOK_EXPEN(MO,YR)=PERIOD_AMOUNT/PERIOD
                             ENDDO
                           ENDIF
                        ENDIF
                     ELSEIF(BOOK_EXPEN(0,YR) /= 0.) THEN
                        DO MO = 1, 12
                           BOOK_EXPEN(MO,YR) = BOOK_EXPEN(0,YR)/12.
                        ENDDO
                     ENDIF
                     TEMP_CWIP = TEMP_CWIP + BOOK_EXPEN(0,YR) - &
                     	TEMP_PLANT_2_SERVICE(YR)
                     IF(TEMP_CWIP < .00001) TEMP_CWIP = 0.
     
                  ENDDO
               ELSE

               ENDIF
            ENDIF

            USE_PLANT_PERCENTAGE = .FALSE.
            IF(PROCESSING_NEW_ADDITIONS) THEN
            ELSE
               TEMP_CWIP = CWIP1
               IF(MONTHLY_MIDAS_ACTIVE .OR. &
               	MONTHLY_PLANT_IN_SERVICE_EXIST) THEN
                  DO YR = 1, MAX_FINANCIAL_SIMULATION_YEARS

                     DO MO = 1, 12
                        TEMP_CWIP  = TEMP_CWIP + BOOK_EXPEN(MO,YR)
                        IF(INDEX('RPB',DATA_TYPE) /= 0) THEN
                           PERCENT_PLANT_2_SERVICE(MO,YR) = &
                           	MIN(PLANT_2_SERVICE(MO,YR),100.)
                           PLANT_2_SERVICE(MO,YR) = TEMP_CWIP/100. * &
                           	MIN(PLANT_2_SERVICE(MO,YR),100.)
                        ELSEIF(DATA_TYPE == 'C') THEN
                           PLANT_2_SERVICE(MO,YR) = &
                                    MIN(PLANT_2_SERVICE(MO,YR)* &
                                        BOOK_EXPEN(MO,YR)/100, &
                                                      MAX(TEMP_CWIP,0.))
                        ELSE
                           PLANT_2_SERVICE(MO,YR) = &
                                   MIN(PLANT_2_SERVICE(MO,YR), &
                                                      MAX(TEMP_CWIP,0.))
                        ENDIF
                        IF(ABS(PLANT_2_SERVICE(MO,YR)) < .00001) &
                        	PLANT_2_SERVICE(MO,YR) = 0. 
                        TEMP_CWIP = TEMP_CWIP - PLANT_2_SERVICE(MO,YR)
!
                     ENDDO
                     PLANT_2_SERVICE(0,YR)=SUM(PLANT_2_SERVICE(1:12,YR))
                  ENDDO
                  USE_PLANT_PERCENTAGE = INDEX('RPB',DATA_TYPE) /= 0
               ELSE
                  DO YR = 1, MAX_FINANCIAL_SIMULATION_YEARS 
                     TEMP_CWIP  = TEMP_CWIP + BOOK_EXPEN(0,YR)
                     IF(INDEX('RPB',DATA_TYPE) /= 0) THEN
                        PLANT_2_SERVICE(0,YR) = TEMP_CWIP/100. * &
                        	MIN(PLANT_2_SERVICE(0,YR),100.)
                     ELSEIF(DATA_TYPE == 'C') THEN
                        PLANT_2_SERVICE(0,YR)=MIN(PLANT_2_SERVICE(0,YR)* &
                                                  BOOK_EXPEN(0,YR)/100, &
                                              MAX(TEMP_CWIP,0.))
                     ELSE
                        PLANT_2_SERVICE(0,YR) = &
                                           MIN(PLANT_2_SERVICE(0,YR), &
                                           MAX(TEMP_CWIP,0.))
                     ENDIF
                     IF(ABS(PLANT_2_SERVICE(0,YR)) < .00001) then
                        PLANT_2_SERVICE(0,YR) = 0. 
                     end if
                     TEMP_CWIP = TEMP_CWIP - PLANT_2_SERVICE(0,YR)
                     PLANT_2_SERVICE(MIN(INT(12,2),SERVICEMO),YR) = &
                     	PLANT_2_SERVICE(0,YR)
                  ENDDO
               ENDIF
            ENDIF
!
! PASS MONTHLY BACK TO OLD ANNUAL VARIABLES
!
            DO I = 1, FINANCIAL_SIMULATION_YEARS
               CE(I) = BOOK_EXPEN(0,I-1)
               CEP(I) = PLANT_2_SERVICE(0,I-1)
            ENDDO
!
! AFUDC
!
            MONTHLY_AFUDC_ON_CASH = 0.
            MONTHLY_AFUDC_ON_PLANT = 0.
!
            CALL AFUDC_MONTHLY(AFUDC1,CWIPRB,AFDCB1,AFDCFC,CWIP1, &
                               AFDCSW,AFDCPERIODS,CE,CEP, &
                               AFDC1,AFDC2,CWIP,RBCWIP, &
                               AFDC1B,AFDC2B,SERVICEMO,CAPINRST, &
                               DEPMET,PCAPINRST,AFDC_CAP_VECTOR, &
                               FIRSTYR,CURRENT_INTEREST_CAP,'FA', &
                               MAX_FINANCIAL_SIMULATION_YEARS,ADRLIFE, &
                               AFUDC_IN_CWIP,BOOK_EXPEN, &
                               PLANT_2_SERVICE, &
                               MONTHLY_AFUDC_ON_CASH, &
                               MONTHLY_AFUDC_ON_PLANT, &
                               MONTHLY_CWIP, &
                               MONTHLY_AFUDC_IN_CWIP, &
                               MONTHLY_CWIP_IN_RATEBASE, &
                               MONTHLY_CAPITALIZED_INTEREST, &
                               MONTHLY_CURRENT_INTEREST, &
                               MONTHLY_INTEREST_TO_TAX_VALUE, &
                               MONTHLY_CURRENT_INTEREST_CAP, &
                               USE_PLANT_PERCENTAGE, &
                               PERCENT_PLANT_2_SERVICE)
!
! CALCULATE BOOK DEPRECIATION FOR AFDC
!
            IF(MONTHLY_MIDAS_ACTIVE) THEN
               CALL BOOK_DEPRECIATION(BOKLF, &
                                      MONTHLY_BOOK_DEP, &
                                      PLANT_2_SERVICE, &
                                      MONTHLY_AFUDC_ON_PLANT, &
                                      SALVAGE_VALUE, &
                                      BOOK_DEP_METHOD, &
                                      FINANCIAL_SIMULATION_YEARS, &
                                      HALF_YEAR_BOOK_DEP) 
               DO J = 1, FINANCIAL_SIMULATION_YEARS
                  BOKDP(J) = MONTHLY_BOOK_DEP(0,J-1)
               ENDDO
            ELSE
               IF(BOKLF < 0.) THEN
                  DO J = 1, FINANCIAL_SIMULATION_YEARS
                     BOOK_VALUE(J) = CEP(J) + AFDC2(J)
                     AFCDP(J) = 0.
                  ENDDO
                  CALL DEPBOK(BOKLF,BOKDP,BOOK_VALUE,1.0,SERVICEMO, &
                              FIRSTYR,SALVAGE_VALUE, &
                              FINANCIAL_SIMULATION_YEARS, &
                              HALF_YEAR_BOOK_DEP)
               ELSE
                  CALL DEPBOK(BOKLF,AFCDP,AFDC2,1.0,SERVICEMO, &
                              FIRSTYR,0., &  ! SALVAGE_VALUE, 
                              FINANCIAL_SIMULATION_YEARS, &
                              HALF_YEAR_BOOK_DEP)
!
! RECOMPUTE BOOK DEPRECIATION THAT INCLUDES SALVAGE VALUE
!
                  CALL DEPBOK(BOKLF,BOKDP,CEP,1.0,SERVICEMO, &
                              FIRSTYR,SALVAGE_VALUE, &
                              FINANCIAL_SIMULATION_YEARS, &
                              HALF_YEAR_BOOK_DEP)
               ENDIF
            ENDIF
!
            IF(INDEX(DEPMET,'CCA') /= 0) THEN
               DO J = 1, FINANCIAL_SIMULATION_YEARS
                  ITCDF(J) = 0.
                  ITCDP(J) = 0.
                  ITCPAY(J) = 0.
                  CEPTXBOK(J) = 0.
                  CEPTX(J) = 0.
               ENDDO
               IF(INDEX(DEPMET,'CCA4') /= 0) THEN
                  CALL FAST_TAX_RATE_34(CEP,CEPTX,CE,OHRATE,TAXEXP,CWIP, &
                                        CAPINRST,CEPTXBOK,TAXDP, &
                                        FINANCIAL_SIMULATION_YEARS)
               ELSE
                  CALL CCA_TAX_DEP(TAXLF,CEPTX,CE,OHRATE,TAXEXP, &
                             PCAPINRST,DBRATE,TAXDP,CAPINRST(1),FIRSTYR, &
                             CEP,DEPMET,FINANCIAL_SIMULATION_YEARS)
               ENDIF
               DO J = 1, FINANCIAL_SIMULATION_YEARS
                  TAX_VALUE_BEGINNING_90(J) = CEPTX(J)
               ENDDO
               DO J = 1, MAX(1,1989-BASEYEAR)
                  TAX_VALUE_BEGINNING_90(J) = 0.
               ENDDO
            ELSE
!
! CALCULATE TAX VALUE AND ITCs
!
               CALL TAXVALUE(TAXLF,DEPMET,CEP,CEPTX,CE,ITCDF, &
                     ITCDP,DPBAIS,OHRATE,TAXEXP,PCITC,ITCPAY,CWIP, &
                     CAPINRST,CEPTXBOK,ITC_RATE, &
                     CURRENT_INTEREST_CAP,FINANCIAL_SIMULATION_YEARS, &
                     TAX_BASIS_ADJUSTOR, &
                     BOOK_EXPEN, &
                     PLANT_2_SERVICE, &
                     MONTHLY_TAX_VALUE_OF_ASSET, &
                     MONTHLY_TAX_EXPENSE, &
                     MONTHLY_CWIP, &
                     MONTHLY_INTEREST_TO_TAX_VALUE, &
                     MONTHLY_CURRENT_INTEREST_CAP)
!
! CALCULATE TAX DEPRECIATION ON PLANT
!
               CALL DEPTAX(TAXLF,DEPMET,DBRATE,TAXDP,CEPTX, &
                           TAX_VALUE_BEGINNING_90, &
                           FINANCIAL_SIMULATION_YEARS, &
                           TAKE_BONUS_TAX_DEPRECIATION, &
                           SERVICEMO, &
                           MONTHLY_TAX_VALUE_OF_ASSET, &
                           MONTHLY_TAX_DEPRECIATION, &
                           BONUS_DEP_MONTHLY_2001)
            ENDIF
!
! CALCULATE TAX ITEMS FOR 1986 TAX REFORM ACT
!
            IF(DEPMET /= 'ACRS') THEN
!
! CALCULATE TAX DEPRECIATION PREFERANCE ON PLANT
!
               CALL DEPTAX(TAXLF,DEPMET,DBRATE,TAXDPALT,CEPTX,DUMMY, &
                           FINANCIAL_SIMULATION_YEARS, &
                           TAKE_BONUS_TAX_DEPRECIATION, &
                           SERVICEMO, &
                           MONTHLY_TAX_VALUE_OF_ASSET, &
                           MONTHLY_TAX_DEPRECIATION_ALT, &
                           BONUS_DEP_MONTHLY_2001)
               DBRT = 1.5
               CALL DEPTAX(ADRLIFE,DEPMET,DBRT,TXPREFDEP,CEPTX,DUMMY, &
                           FINANCIAL_SIMULATION_YEARS, &
                           TAKE_BONUS_TAX_DEPRECIATION, &
                           SERVICEMO, &
                           MONTHLY_TAX_VALUE_OF_ASSET, &
                           MONTHLY_TAX_DEP_PREFERENCE, &
                           BONUS_DEP_MONTHLY_2001)
               TXPREFDEP(1:) = TAXDPALT(1:) - TXPREFDEP(1:)
            ELSE
               TXPREFDEP = 0.
            ENDIF
!
! CALCULATE ACE_BOOK_DEP FOR THE AMT TAX CALCULATION
!
            CALL ACE_DEP(ADRLIFE,ACE_BOOK_DEP, &
                         TAX_VALUE_BEGINNING_90, &
                         FINANCIAL_SIMULATION_YEARS, &
                         MONTHLY_TAX_VALUE_OF_ASSET, &
                         MONTHLY_ACE_BOOK_DEP)
!
! CALCULATE BOOK DEPRECIATION FOR DEFFERED TAXES
!
            IF(TAX_NORMALIZATION_PEROID == 0.) THEN
               BKDPTX = 0. 
               TXDEF = 0.
               TXDEFC = 0.
            ELSE
               HALF_YEAR_BOOK_DEP = .TRUE.
               DO I = 2, FINANCIAL_SIMULATION_YEARS
                  BKDPTX(I) = 0.
                  IF(CEP(I) /= 0.) THEN
                     RATIO = CEPTXBOK(I)/CEP(I)
                     CALL DEPBOK(TAX_NORMALIZATION_PEROID,BKDPTX, &
                                 CEPTXBOK,RATIO,SERVICEMO,FIRSTYR,0., &
                                 FINANCIAL_SIMULATION_YEARS, &
                                 HALF_YEAR_BOOK_DEP)
                     EXIT
                  ENDIF
               ENDDO
!
! CALCULATE THE BASIS FOR DEFFERED TAXES
!
               CALL DEFTAX(TAXDP,TAXEXP,BKDPTX,TXDEF,TXDEFC,PCAPINRST, &
                           CURRENT_INTEREST_CAP_RATE, &
                           FINANCIAL_SIMULATION_YEARS)
!
!
!
               IF(MONTHLY_MIDAS_ACTIVE) THEN
                  TAX_BOOK_DEP_METHOD = BOOK_DEP_METHOD
                  IF(BOOK_DEP_METHOD == 'R') TAX_BOOK_DEP_METHOD = 'G'
                  CALL BOOK_DEPRECIATION(TAX_NORMALIZATION_PEROID, &
                                      MONTHLY_TAX_BOOK_DEP, &
                                      MONTHLY_TAX_VALUE_OF_ASSET, &
                                      DUMMY_MONTHLY_ARRAY, &
                                      0., &
                                      TAX_BOOK_DEP_METHOD, &
                                      FINANCIAL_SIMULATION_YEARS, &
                                      HALF_YEAR_BOOK_DEP)
                  DO YR = 1, FINANCIAL_SIMULATION_YEARS-1
                     DO MO = 0, 12 
                        MONTHLY_DEFERRED_TAX_BASIS(MO,YR) = &
                                MONTHLY_TAX_DEPRECIATION(MO,YR) &
                                - MONTHLY_TAX_BOOK_DEP(MO,YR) &
                                + MONTHLY_TAX_EXPENSE(MO,YR) &
                               - MONTHLY_CAPITALIZED_INTEREST(MO,YR) &
                           * (1.-CURRENT_INTEREST_CAP_RATE(YR+1))
                    
                     ENDDO
                  ENDDO
!    
               ENDIF
            ENDIF
!
! CALCULATE REAL PROPERTY TAX VALUE IF BEING USED

            CALL REAL_PROPERTY_VALUES(SERVICEMO,PROPERTY_ESCALATION, &
            	FINANCIAL_SIMULATION_YEARS)

! CALCULATE THE CHANGES FROM AN ABANDONMENT

            IF(ABMETH > 0) THEN
               CALL FAABND(ABYEAR,ABMETH,ABTAX,TAXWO,BOKBL,BOKDAL,BOKWO, &
                           AFCBL,AFCDAL,AFCWO,NRTXWO,PCAPINRST, &
                           TXPREFDEP,ABACCT,ACE_BOOK_DEP, &
                           AFUDC_IN_CWIP, &
                           FINANCIAL_SIMULATION_YEARS)
               CALL WRITE_OFF(ABYEAR,ABMETH,WOYRS,DDB,RBDDB,AFCEXP, &
                              AJAFDC,EXEXP,AMORTE,BOKBL,BOKDAL,BOKWO, &
                              AFCBL,AFCDAL,AFCWO,ABACCT,WODFTX,NRTXWO, &
                              FINANCIAL_SIMULATION_YEARS)
            ENDIF
            IF(PROCESSING_NEW_ADDITIONS) THEN
               CALL MODEL_ASSET_TOTALS(ABMETH,ABYEAR,TAXWO,PCAPINRST, &
                                       TXPREFDEP,REGULATORY_ALLOCATOR, &
                                       ABTAX,ACE_BOOK_DEP, &
                                       AFUDC_IN_CWIP, &
                                       ASSET_CLASS_NUM, &
                                       ASSET_CLASS_VECTOR)
            ELSE
               CALL FA_TOTALS(ABMETH,ABYEAR,TAXWO,PCAPINRST,TXPREFDEP, &
                              REGULATORY_ALLOCATOR,ABTAX,ACE_BOOK_DEP, &
                              AFUDC_IN_CWIP, &
                              ASSET_CLASS_NUM,ASSET_CLASS_VECTOR)
            ENDIF
            IF(MONTHLY_MIDAS_ACTIVE) THEN
               CALL MONTHLY_TOTAL_FUTURE_ASSETS(ASSET_CLASS_NUM, &
                                                ASSET_CLASS_VECTOR, &
                                                BOOK_EXPEN, &
                                                CASH_LAG_PATTERN, &
                                                CASH_PAYMENTS, &
                                                MONTHLY_AFUDC_ON_CASH, &
                                                MONTHLY_AFUDC_ON_PLANT, &
                                                PLANT_2_SERVICE, &
                                                MONTHLY_BOOK_DEP, &
                                                CASH_EXPENDITURES, &
                                                AFDCBR(2), &
                                                CWIP1+AFUDC1, &
                                                REGULATORY_ALLOCATOR, &
                                                MONTHLY_CWIP, &
                                                MONTHLY_CWIP_IN_RATEBASE, &
                                                CUM_BOOK_DEP, &
                                                GPV, &
                                                MONTHLY_AFUDC_IN_CWIP)
            ENDIF
            IF(NEXT_DECISION_CAPACITY_PLANNING .AND. &
            	 PROCESSING_NEW_ADDITIONS) THEN
               IF(FIRSTYR == globecom_YEAR+get_BASE_YEAR() .OR. &
                                  ABYEAR == globecom_YEAR+get_BASE_YEAR() .OR. &
              ON_LINE_YR == globecom_YEAR+get_BASE_YEAR()) THEN
                  IF(FUTURE_ASSET_RPT_NOT_OPEN) THEN
                     FA_RPT_UNIT = FUTURE_ASSET_REPT_HEADER(FA_RPT_REC)
                     FUTURE_ASSET_RPT_NOT_OPEN = .FALSE.
                  ENDIF
                  FLOAT_END_PT = PRT_ENDPOINT()
                  REPORTING_YEAR = FLOAT(globecom_YEAR+get_BASE_YEAR())
                  RECORD_TYPE = 'Commit'
                  IF(ABYEAR == globecom_YEAR+get_BASE_YEAR() .OR. &
                  	ON_LINE_YR == globecom_YEAR+get_BASE_YEAR()) then
                        RECORD_TYPE = 'Finish'
                  end if
                  END = 31
!
                  TOTAL_BKDPTX(1) = TOTAL_BKDPTX(1) + BKDPTX(1)
                  DO I = 2, END
                     WRITE(FA_RPT_UNIT,REC=FA_RPT_REC) FLOAT_END_PT, &
                                     RECORD_TYPE,OPTION_NAME, &
                                     FLOAT(I+get_BASE_YEAR()-1), &
                                     CE(I),AFDC1(I),CWIP(I),CEP(I), &
                                     AFDC2(I),BOKDP(I)+AFCDP(I), &
                                     TXDEF(I),TAXDP(I),BKDPTX(I), &
                                     TAXEXP(I),ACE_BOOK_DEP(I), &
                                     PCAPINRST(I), &
                                     PCAPINRST(I) * &
                                     (1.-CURRENT_INTEREST_CAP_RATE(I)), &
                                     AFUDC_IN_CWIP(I), &
                                     CWIP(I)+AFUDC_IN_CWIP(I), &
                                     AFDC2(I)  + CEP(I)*(1.-SALVAGE_VALUE/100.), &
                                     ITCDP(I)
                     FA_RPT_REC = FA_RPT_REC + 1
                     TOTAL_BKDPTX(I) = TOTAL_BKDPTX(I) + BKDPTX(I)
                  ENDDO
               ENDIF
            ELSEIF(FUTURE_ASSET_REPORT .AND. DELETE > 1 .OR. &
            	REPORT_ALL_ACCOUNTS) THEN
               ACCOUNTS_REPORTED = ACCOUNTS_REPORTED + 1
               TOTAL_BKDPTX(1) = TOTAL_BKDPTX(1) + &
               REGULATORY_ALLOCATOR * BKDPTX(1)
!
! ADD DRILLING INFORMATION
!
!     +                 BOOK_EXPEN,
!     +                 PLANT_2_SERVICE,
!     +                 MONTHLY_AFUDC_ON_PLANT,
!     +                 MONTHLY_CWIP_IN_RATEBASE)
               ANNUAL_INFO_ACTIVE = DRILLING_REPRT_LEVEL == 'A'
               CASH_NOT_ZERO = CWIP1 /= 0.
               CUM_BOOK_DEP(0,1) = 0.
               GPV(0,1) = 0.
               DO YR = 1, FINANCIAL_SIMULATION_YEARS
                  IF(YR > 1) THEN
                     CUM_BOOK_DEP(0,YR) = CUM_BOOK_DEP(12,YR-1)
                     GPV(0,YR) = GPV(12,YR-1)
                  ENDIF
                  DO MO = 1, 12
                     CUM_BOOK_DEP(MO,YR) = CUM_BOOK_DEP(MO-1,YR) &
                     	+ REGULATORY_ALLOCATOR * MONTHLY_BOOK_DEP(MO,YR)
                     GPV(MO,YR) = GPV(MO-1,YR) &
                                  + REGULATORY_ALLOCATOR * &
                                  (PLANT_2_SERVICE(MO,YR) &
                                  + MONTHLY_AFUDC_ON_PLANT(MO,YR))
                  ENDDO
               ENDDO
               BOOK_DEP_NOT_ZERO = .FALSE.
               DO YR = 1, FINANCIAL_SIMULATION_YEARS ! REPORTING_YEARS-1
                  DO I = 0, 12
                     MONTHLY_AFUDC_ON_CASH(I,YR) = REGULATORY_ALLOCATOR * &
                     	MONTHLY_AFUDC_ON_CASH(I,YR)
                     MONTHLY_BOOK_DEP(I,YR) = REGULATORY_ALLOCATOR * &
                     	MONTHLY_BOOK_DEP(I,YR)
                     MONTHLY_CAPITALIZED_INTEREST(I,YR) =  &
                                    REGULATORY_ALLOCATOR &
                                    * MONTHLY_CAPITALIZED_INTEREST(I,YR)
                     MONTHLY_CURRENT_INTEREST(I,YR) = &
                                        REGULATORY_ALLOCATOR &
                                        * MONTHLY_CURRENT_INTEREST(I,YR)
                     CASH_EXPENDITURES(I,YR) = REGULATORY_ALLOCATOR * &
                     	CASH_EXPENDITURES(I,YR)
                     PLANT_2_SERVICE(I,YR) = REGULATORY_ALLOCATOR * &
                     	PLANT_2_SERVICE(I,YR)
                     MONTHLY_AFUDC_ON_PLANT(I,YR) = &
                                           REGULATORY_ALLOCATOR * &
                                           MONTHLY_AFUDC_ON_PLANT(I,YR)
                     MONTHLY_CWIP(I,YR) = REGULATORY_ALLOCATOR * &
                     	MONTHLY_CWIP(I,YR)
                     NPV(I,YR) = GPV(I,YR) - CUM_BOOK_DEP(I,YR)
                     BOOK_EXPEN(I,YR) = REGULATORY_ALLOCATOR * BOOK_EXPEN(I,YR)
                  ENDDO
                  CASH_NOT_ZERO = CASH_NOT_ZERO .OR. &
                  	CASH_EXPENDITURES(0,YR) /= 0.
                  BOOK_DEP_NOT_ZERO = BOOK_DEP_NOT_ZERO .OR. &
                  	MONTHLY_BOOK_DEP(0,YR) /= 0.
               ENDDO
               IF(WVPA()) THEN
!
! WVPA WORKORDER CONTROL REPORT
!
                  IF(FUTURE_ASSET_REPORT) THEN
                     EXPENDITURES_ProjToDate = CWIP1
                     AFUDC_CHECK_MARK = 1.
                     IF(AFDCSW == 2) AFUDC_CHECK_MARK = 2.
                     DRILLING_ACCOUNT_NAME = &
                           WVPA_WORK_ORDER_REPORT_NAME(DESC, &
                                                ACCTNO, &
                                                WVPA_BUDGET_ITEM_NUM, &
                                                WVPA_WORK_ORDER_NUMBER)
                     DO YR = IOFFSET, IOFFSET+4
                        YearToDate = 0.
                        DO MO = 1, 12
                           EXPENDITURES_ProjToDate = BOOK_EXPEN(MO,YR) &
                           + EXPENDITURES_ProjToDate
                           YearToDate = YearToDate + BOOK_EXPEN(MO,YR)
                           RemainingExpenditures = PV_PLANT_COST - &
                           EXPENDITURES_ProjToDate
                           COMPLETION_CHECK_MARK = 1.
                           IF(EXPENDITURES_ProjToDate >= &
                           	.90*PV_PLANT_COST) COMPLETION_CHECK_MARK=2.
!
                           WRITE(WVPA_WORKORDER_SUMARY_RPRT_UNIT, &
                               REC = WVPA_WORKORDER_SUMMARY_RPRT_REC) &
                                        PRT_ENDPOINT(), &
                                        FLOAT(get_BASE_YEAR()+YR), &
                                        WVPA_DEPARTMENT_HEAD, &
                                        DRILLING_ACCOUNT_NAME, &
                                        SHORT_MONTH_NAMES(MO), &
                                        FLOAT(PROJECT_COMPLETION_MO), &   ! 0
                                        FLOAT(PROJECT_COMPLETION_YR), &
                                        PV_PLANT_COST, &
                                        PROJECT_SUM, &                  ! 3
                                        EXPENDITURES_ProjToDate, &      ! 4
                                        RemainingExpenditures, &        ! 5
                                        YearToDate, &                   ! 6
                                        AFUDC_CHECK_MARK, &             ! 7
                                        COMPLETION_CHECK_MARK, &        ! 8
                                        BOOK_EXPEN(MO,YR), &            ! 9
                                        MONTHLY_CWIP(0,YR)              ! 10
!
                           WVPA_WORKORDER_SUMMARY_RPRT_REC = 1 + &
                           	WVPA_WORKORDER_SUMMARY_RPRT_REC

                         ENDDO
                      ENDDO
                  ENDIF
                  CALL WVPA_FA_PLANT_DATA_BASE(ACCTNO, &
                                               WVPA_COMPANY_ID, &
                                               GPV, &
                                               CUM_BOOK_DEP, &
                                               NPV, &
                                               MONTHLY_BOOK_DEP, & 
                                               PLANT_2_SERVICE, &
                                               CASH_EXPENDITURES, &
                                               MONTHLY_CWIP)
                  FIRSTYR = MAX(INT(0,2),FIRSTYR-get_BASE_YEAR())
                  PROJECT_COMPLETION_YR = MAX(INT(0,2), &
                  	PROJECT_COMPLETION_YR-get_BASE_YEAR())
                  CALL WVPA_CONSTRUCTION_DATA_BASE(ACCTNO, &
                                                  WVPA_COMPANY_ID, &
                                                  WORKORDER_NAME, &
                                                  FIRSTYR, &
                                                  PROJECT_COMPLETION_YR, &
                                                  GPV, &
                                                  CUM_BOOK_DEP, &
                                                  NPV, &
                                                  MONTHLY_BOOK_DEP, &
                                                  PLANT_2_SERVICE, &
                                                  CASH_EXPENDITURES, &
                                                  MONTHLY_CWIP, &
                                                  MONTHLY_AFUDC_ON_CASH)
               ENDIF
             IF(FINANCIAL_DRILLING() /= 'O') THEN  
               DO YR = 1, REPORTING_YEARS-1
                  IF(MONTHLY_AFUDC_ON_CASH(0,YR) /= 0.) THEN
                     DRILLING_NAME = 'Total AFUDC'
                     VOID_INT2 = WRITE_DRILLING_RPT(DRILLING_NAME, &
                                                 DRILLING_ACCOUNT_NAME, &
                                                 MONTHLY_AFUDC_ON_CASH, &
                                                 INCOME_REPORT_ITEM, &
                                                 ANNUAL_INFO_ACTIVE)
                     EXIT
                  ENDIF
               ENDDO
!
               IF(BOOK_DEP_NOT_ZERO) THEN
                  DRILLING_NAME = 'Book Depreciation'
                  VOID_INT2 = WRITE_DRILLING_RPT(DRILLING_NAME, &
                                                 DRILLING_ACCOUNT_NAME, &
                                                 MONTHLY_BOOK_DEP, &
                                                 INCOME_REPORT_ITEM, &
                                                 ANNUAL_INFO_ACTIVE)
                  VOID_INT2 = WRITE_DRILLING_RPT(DRILLING_NAME, &
                                                 DRILLING_ACCOUNT_NAME, &
                                                 MONTHLY_BOOK_DEP, &
                                                 BALANCE_SHEET_ITEM, &
                                                 ANNUAL_INFO_ACTIVE)
                  DRILLING_NAME = 'Accumulated Depreciation'
                  VOID_INT2 = WRITE_DRILLING_RPT(DRILLING_NAME, &
                                                 DRILLING_ACCOUNT_NAME, &
                                                 CUM_BOOK_DEP, &
                                                 BALANCE_SHEET_ITEM, &
                                                 ANNUAL_INFO_ACTIVE)
               ENDIF
               DO YR = 1, REPORTING_YEARS-1
                  IF(PLANT_2_SERVICE(0,YR) /= 0. .OR. &
                  	MONTHLY_AFUDC_ON_PLANT(0,YR) /= 0.) THEN
                     DRILLING_NAME = 'Plant Capitialized'
                     VOID_INT2 = WRITE_DRILLING_RPT(DRILLING_NAME, &
                                                  DRILLING_ACCOUNT_NAME, &
                                                  PLANT_2_SERVICE, &
                                                  BALANCE_SHEET_ITEM, &
                                                  ANNUAL_INFO_ACTIVE)
                     DRILLING_NAME = 'AFUDC Capitialized'
                     VOID_INT2 = WRITE_DRILLING_RPT(DRILLING_NAME, &
                                                 DRILLING_ACCOUNT_NAME, &
                                                 MONTHLY_AFUDC_ON_PLANT, &
                                                 BALANCE_SHEET_ITEM, &
                                                 ANNUAL_INFO_ACTIVE)
                     DRILLING_NAME = 'GPV'
                     VOID_INT2 = WRITE_DRILLING_RPT(DRILLING_NAME, &
                                                  DRILLING_ACCOUNT_NAME, &
                                                  GPV, &
                                                  BALANCE_SHEET_ITEM, &
                                                  ANNUAL_INFO_ACTIVE)
                     DRILLING_NAME = 'NPV'
                     VOID_INT2 = WRITE_DRILLING_RPT(DRILLING_NAME, &
                                                  DRILLING_ACCOUNT_NAME, &
                                                  NPV, &
                                                  BALANCE_SHEET_ITEM, &
                                                  ANNUAL_INFO_ACTIVE)
                     EXIT
                  ENDIF
               ENDDO
               IF(CASH_NOT_ZERO) THEN
!     
                  DRILLING_NAME = 'Cash Construction'
                  IF(ANNUAL_INFO_ACTIVE) THEN
                     VOID_INT2 = WRITE_DRILLING_RPT(DRILLING_NAME, &
                                                 DRILLING_ACCOUNT_NAME, &
                                                 BOOK_EXPEN, &
                                                 CASH_REPORT_ITEM, &
                                                 ANNUAL_INFO_ACTIVE)
                  ELSE
                     VOID_INT2 = WRITE_DRILLING_RPT(DRILLING_NAME, &
                                                 DRILLING_ACCOUNT_NAME, &
                                                 CASH_EXPENDITURES, &
                                                 CASH_REPORT_ITEM, &
                                                 ANNUAL_INFO_ACTIVE)
                  ENDIF
                  DRILLING_NAME = 'CWIP Balance'
                  VOID_INT2 = WRITE_DRILLING_RPT(DRILLING_NAME, &
                                                 DRILLING_ACCOUNT_NAME, &
                                                 MONTHLY_CWIP, &
                                                 BALANCE_SHEET_ITEM, &
                                                 ANNUAL_INFO_ACTIVE)
                  DRILLING_NAME = 'Book Construction'
                  VOID_INT2 = WRITE_DRILLING_RPT(DRILLING_NAME, &
                                                 DRILLING_ACCOUNT_NAME, &
                                                 BOOK_EXPEN, &
                                                 BALANCE_SHEET_ITEM, &
                                                 ANNUAL_INFO_ACTIVE)
               ENDIF
             ENDIF
!
! STANDARD ACCOUNT REPORTING
!
               IF(WVPA()) DRILLING_ACCOUNT_NAME = &
               	WVPA_WORK_ORDER_NUMBER(1:8)//DRILLING_ACCOUNT_NAME
               IF(ASSET_REPORTING_LEVEL == 'M' .AND.  &
               	MONTHLY_MIDAS_ACTIVE) THEN
                  LAST_REPORTING_MO = 12
                  CWIP_OPENING_BAL = MONTHLY_CWIP(12,0)
                  DO YR = 1, REPORTING_YEARS
                     I = YR+1
                     WRITE(FA_REPORTING_UNIT,REC=FA_REPORTING_REC) &
                 PRT_ENDPOINT(), DRILLING_ACCOUNT_NAME, &
                 FLOAT(YR+get_BASE_YEAR()),'AnRslt/EOY', &
                 REGULATORY_ALLOCATOR * CE(I),REGULATORY_ALLOCATOR * &
                 AFDC1(I),REGULATORY_ALLOCATOR * CWIP(I), &
                 REGULATORY_ALLOCATOR * CEP(I), &
                 REGULATORY_ALLOCATOR * &
                 AFDC2(I), &
                 REGULATORY_ALLOCATOR * &
                 	 (BOKDP(I)+AFCDP(I)), &
                 REGULATORY_ALLOCATOR * TXDEF(I), &
                 REGULATORY_ALLOCATOR * TAXDP(I), &
                 REGULATORY_ALLOCATOR * BKDPTX(I), &
                 REGULATORY_ALLOCATOR * TAXEXP(I), &
                 REGULATORY_ALLOCATOR * &
                 	ACE_BOOK_DEP(I), &                 ! 10
                 REGULATORY_ALLOCATOR*PCAPINRST(I), &
                 REGULATORY_ALLOCATOR*PCAPINRST(I) * &
                 (1.-CURRENT_INTEREST_CAP_RATE(I)), &
                 REGULATORY_ALLOCATOR * AFUDC_IN_CWIP(I), &
                 REGULATORY_ALLOCATOR * (CWIP(I)+AFUDC_IN_CWIP(I)), &
                 REGULATORY_ALLOCATOR * CEPTX(I), &  ! 15
                 REGULATORY_ALLOCATOR * (CEP(I)* &
                 (1.-SALVAGE_VALUE/100.) + AFDC2(I)), &
                 REGULATORY_ALLOCATOR * CEP(I)*SALVAGE_VALUE/100., &
                 GPV(12,YR), &                             ! 18
                 CUM_BOOK_DEP(12,YR), &
                 GPV(12,YR) - CUM_BOOK_DEP(12,YR), &          ! 20 
                 CASH_EXPENDITURES(0,YR),MONTHLY_CWIP(12,YR), &
                 REGULATORY_ALLOCATOR * BONUS_DEP_MONTHLY_2001(0,YR)  ! 23
                     FA_REPORTING_REC = FA_REPORTING_REC + 1
                     DO MO = 0, LAST_REPORTING_MO
                        IF(MO == 0) THEN
                           PERIOD_NAME = SHORT_MONTH_NAMES(MO)//'/BOY'
                        ELSE
                           PERIOD_NAME = SHORT_MONTH_NAMES(MO)
                        ENDIF
                        WRITE(FA_REPORTING_UNIT,REC=FA_REPORTING_REC) &
                              PRT_ENDPOINT(), &
                              DRILLING_ACCOUNT_NAME, &
                              FLOAT(YR+get_BASE_YEAR()), &
                              PERIOD_NAME,BOOK_EXPEN(MO,YR), &
                              MONTHLY_AFUDC_ON_CASH(MO,YR), &
                              MONTHLY_CWIP(MO,YR),PLANT_2_SERVICE(MO,YR), &
                              MONTHLY_AFUDC_ON_PLANT(MO,YR), &
                              MONTHLY_BOOK_DEP(MO,YR), &  
!
! ANNUAL TAX STUFF
!
          REGULATORY_ALLOCATOR * MONTHLY_DEFERRED_TAX_BASIS(MO,YR), &
          REGULATORY_ALLOCATOR * MONTHLY_TAX_DEPRECIATION(MO,YR), &
          REGULATORY_ALLOCATOR * MONTHLY_TAX_BOOK_DEP(MO,YR), &
          MONTHLY_TAX_EXPENSE(MO,YR), &
	   
          REGULATORY_ALLOCATOR * MONTHLY_ACE_BOOK_DEP(MO,YR), &
          MONTHLY_CAPITALIZED_INTEREST(MO,YR), & ! 10
          MONTHLY_CURRENT_INTEREST(MO,YR), &
          REGULATORY_ALLOCATOR * &
          MONTHLY_AFUDC_IN_CWIP(MO,YR), &
          REGULATORY_ALLOCATOR * MONTHLY_AFUDC_IN_CWIP(MO,YR) + &
          MONTHLY_CWIP(MO,YR), &
          REGULATORY_ALLOCATOR * MONTHLY_TAX_VALUE_OF_ASSET(MO,YR), &
          PLANT_2_SERVICE(MO,YR) * (1.-SALVAGE_VALUE/100.) + &
          MONTHLY_AFUDC_ON_PLANT(MO,YR), &            ! 15
          PLANT_2_SERVICE(MO,YR) * SALVAGE_VALUE/100., &
!
! NEW VARIABLES
!
                              GPV(MO,YR),  &   ! 18
                              CUM_BOOK_DEP(MO,YR), &
                              GPV(MO,YR) - CUM_BOOK_DEP(MO,YR),  &
                              CASH_EXPENDITURES(MO,YR), &   
                              CWIP_OPENING_BAL, &          ! 22
                              REGULATORY_ALLOCATOR * &
							  BONUS_DEP_MONTHLY_2001(MO,YR)   ! 23
 
                        CWIP_OPENING_BAL = MONTHLY_CWIP(MO,YR)
                        FA_REPORTING_REC = FA_REPORTING_REC + 1
                     ENDDO
                  ENDDO
               ELSE
                  DO I = 1, REPORTING_YEARS
                     WRITE(FA_REPORTING_UNIT,REC=FA_REPORTING_REC) &
            PRT_ENDPOINT(),DRILLING_ACCOUNT_NAME,FLOAT &
				(I+get_BASE_YEAR()-1), &
            REGULATORY_ALLOCATOR * CE(I),REGULATORY_ALLOCATOR * &
				AFDC1(I), &
            REGULATORY_ALLOCATOR * CWIP(I),REGULATORY_ALLOCATOR * &
				CEP(I), &
            REGULATORY_ALLOCATOR * AFDC2(I), &
            REGULATORY_ALLOCATOR * (BOKDP(I)+AFCDP(I)), &
            REGULATORY_ALLOCATOR * TXDEF(I), &
            REGULATORY_ALLOCATOR * TAXDP(I), &
            REGULATORY_ALLOCATOR * BKDPTX(I), &
            REGULATORY_ALLOCATOR * TAXEXP(I), &
            REGULATORY_ALLOCATOR * ACE_BOOK_DEP(I), &
            REGULATORY_ALLOCATOR*PCAPINRST(I), &
            REGULATORY_ALLOCATOR*PCAPINRST(I) * &
				(1.-CURRENT_INTEREST_CAP_RATE(I)), &
            REGULATORY_ALLOCATOR * AFUDC_IN_CWIP(I), &
            REGULATORY_ALLOCATOR * (CWIP(I)+AFUDC_IN_CWIP(I)), &
            REGULATORY_ALLOCATOR * CEPTX(I), &  ! 15
            REGULATORY_ALLOCATOR * (CEP(I)*(1.-SALVAGE_VALUE/100.) + &
				AFDC2(I)), &
            REGULATORY_ALLOCATOR * CEP(I)*SALVAGE_VALUE/100., & 
            REGULATORY_ALLOCATOR * ITCDP(I), &
            REGULATORY_ALLOCATOR * &
            BONUS_DEP_MONTHLY_2001(0,I-1) ! 19
                     FA_REPORTING_REC = FA_REPORTING_REC + 1
                     TOTAL_BKDPTX(I) = TOTAL_BKDPTX(I) + &
						REGULATORY_ALLOCATOR * BKDPTX(I)
                  ENDDO
               ENDIF
            ENDIF
         ENDDO ! START_YEARS
        ENDDO ! NUMBER OF RECORDS
        IF(.NOT. PROCESSING_NEW_ADDITIONS) THEN
            CALL CLOSE_FUTURE_ASSET_FILE
        ENDIF
      ENDDO ! END ACTIVE FUTURE ASSET FILES
!
!     WRITE THE CUMULATED RESULTS TO THE ASSET FILE
!
      IF(LF95 .AND. TACCTS > 0 .AND. .NOT. PROCESSING_NEW_ADDITIONS)THEN
         IF(WVPA()) THEN
            WRITE(SCREEN_MESSAGES,"(I4,A)") TACCTS,"-Work Orders" 
         ELSE
            WRITE(SCREEN_MESSAGES,'(I4,A)')TACCTS,'-Future Asset Accounts'
         ENDIF
         CALL MG_LOCATE_WRITE(0,0,TRIM(SCREEN_MESSAGES),ALL_VERSIONS,1)
      ENDIF
      IF(PROCESSING_NEW_ADDITIONS) THEN
         FUTURE_ASSET_REPORT = FUTURE_ASSET_REPORT .AND. MIDAS_ADDED_INVESTMENT
         IF(MIDAS_ADDED_INVESTMENT) THEN
            CALL CLOSE_MODEL_ADDED_ASSET_FILE
         ENDIF
         IF(FUTURE_ASSET_REPORT) &
                 CALL SAVE_MODEL_ASSET_TOTALS(FUTURE_ASSET_REPORT, &
                                              FA_REPORTING_UNIT, &
                                              CURRENT_INTEREST_CAP_RATE, &
                                              FA_REPORTING_REC)
      ELSE
         FUTURE_ASSET_REPORT = FUTURE_ASSET_REPORT .AND. ACCOUNTS_REPORTED /= 1
! 
         ASSETS_IN_BASE_FILE = TACCTS
         ANNUAL_INFO_ACTIVE = FINANCIAL_DRILLING() == 'A'
         CALL SAVE_FUTURE_ASSET_TOTALS(SAVE_BASE_CASE, &
                                       FUTURE_ASSET_REPORT, &
                                       FA_REPORTING_UNIT, &
                                       CURRENT_INTEREST_CAP_RATE, &
                                       FA_REPORTING_REC, &
                                       ANNUAL_INFO_ACTIVE)
         IF(MONTHLY_MIDAS_ACTIVE) THEN
            IF(SAVE_BASE_CASE) CALL SAVE_MONTHLY_FA_VALUES()
         ENDIF
      ENDIF
      DEALLOCATE(VECTOR_DATA_CASH, &
                 VECTOR_DATA_PLNT, &
                 VECTOR_DATA)
      DEALLOCATE(CEPTXBOK,TOTAL_BKDPTX,CURRENT_INTEREST_CAP, &
                 PCAPINRST,TAXDPALT,CAPINRST,TXPREFDEP, &
                 BOOK_VALUE,ACE_BOOK_DEP,TAX_VALUE_BEGINNING_90, &
                 DUMMY)
      DEALLOCATE(BOOK_EXPEN,PLANT_2_SERVICE, &
                 MONTHLY_AFUDC_ON_CASH, &
                 MONTHLY_AFUDC_ON_PLANT, &
                 MONTHLY_CWIP, &
                 MONTHLY_AFUDC_IN_CWIP, &
                 MONTHLY_BOOK_DEP, &
                 CUM_BOOK_DEP, &
                 GPV, &
                 NPV, &
                 TEMP_PLANT_2_SERVICE, &
                 CASH_EXPENDITURES, &
                 MONTHLY_CWIP_IN_RATEBASE, &
                 MONTHLY_CAPITALIZED_INTEREST, &
                 MONTHLY_CURRENT_INTEREST)
      RETURN
 1000 FORMAT(1X,A30,12F8.2)
 1010 FORMAT(27X,'Year ',12(2X,I4,2X))
      END

!                                                                      *
!          SUBROUTINE TO CALCULATE FUTURE ASSET VALUE                  *
!          COPYRIGHT (C) 1983 M.S. GERBER & ASSOCIATES, INC.           *
!                                                                      *

!
      RECURSIVE SUBROUTINE SET_UP_FA_ARRAYS
!
      USE IREC_ENDPOINT_CONTROL
      USE DRILLING_REPT_PARAMETERS
      use grx_planning_routines
      USE SIZECOM
      use SpinDriftLib
      USE PROD_ARRAYS_DIMENSIONS
      use globecom
      use mod_base_year
      SAVE

!
      INTEGER (KIND=2) :: R_YR,R_CLASS,MO
      LOGICAL (KIND=1) :: R_CLASS_EXISTS,LGandE
      REAL (KIND=4) :: ALLOCATION_VALUE(0:AVAIL_DATA_YEARS)
      REAL :: R_CLASS_GPV,R_CLASS_CUMULATIVE_DEPRECIATION
      REAL :: FA_CASH,FA_CAPITIALIZED,FA_TAX_DEP, &
           FA_BOOK_DEP,FA_AFDC_CASH, &
           FA_AFDC_CAPITIALIZED,FA_CWIP,FA_ITC_CREDIT, &
           FA_CWIP_IN_RB,FA_CURRENT_TAX_EXPENSES, &
           FA_CAPITIALIZED_INTEREST, &
           FA_PROPERTY_TAX_GPV,FA_PROPERTY_TAX_NPV, &
           FA_AMORTIZATION_OF_DEF_DEBIT, &
           FA_EXTRAORDINARY_EXPENSE, &
           FA_NET_DEFERRED_DEBIT_BAL, &
           FA_DEF_DEBIT_IN_RB, &
           FA_DEF_TAX_B4_TAX_RATES, & ! TTXDEF
           FA_SL_TAX_DEP, & ! TBKDPT(I)
           FA_TAX_PREFERENCE_DEP, & ! TTXPREFDEP(I)
           FA_ACE_BOOK_DEP, & ! TOTAL_ACE_BOOK_DEP(I)
      ! write-off adjustment to cumulative book depreciaton
           FA_WO_ADJUSTMENT_2_CUM_BOOK_DEP, &
      ! write_off adj to cumulative afdc dep.
           FA_WO_ADJUSTMENT_2_CUM_AFDC_DEP, &
           FA_AFDC_BORROWED, & ! TAFDCB(I)
           FA_WO_ADJUSTMENT_2_CUM_AFDC, & ! TAFCAJ(I)
           FA_WO_ADJUSTMENT_2_CUM_AFEXP, &  ! TAFEXP(I)
           FA_WO_ADJUSTMENT_2_CUM_DEF_TAX, & ! TWODFT(I)
           FA_WO_ADJUSTMENT_2_CUM_AFDCF  ! TAFDCF(I)
!
      REAL :: GPV_ADDITIONS,DEP_ADDITIONS,AFUDC_IN_CWIP(*)
      LOGICAL (KIND=4) :: SAVE_BASE_CASE
      CHARACTER (LEN=1) :: DUMMY_TYPE
      INTEGER (KIND=2) :: MAX_ASSET_CLASS_NUM, &
      	ASSET_CLASS,ASSET_CLASS_VECTOR,CLASS,R_ASSET_CLASS
      REAL :: ASSET_CLASS_LIST(AVAIL_DATA_YEARS), &
      	ASSET_ALLOCATION_LIST(AVAIL_DATA_YEARS),ALLOCATOR,TAXWO
      INTEGER (KIND=2) :: ASSET_CLASS_POINTER(:),CLASS_POINTER
      ALLOCATABLE :: ASSET_CLASS_POINTER
      INTEGER (KIND=2) :: NUM_OF_FA_CLASSES=0
      INTEGER (KIND=4) :: VALUES_TO_ZERO
      INTEGER (KIND=2) :: FINANCIAL_SIMULATION_YEARS
      INTEGER (KIND=2) :: I,ABMETH,ABYEAR
      REAL :: PCAPINRST(*),TXPREFDEP(*),ACE_BOOK_DEP(*), &
      	REGULATORY_ALLOCATOR,ABTAX,CUMNPV,CUM_DD_AMORT
!     TYPE DECLARATION FOR /WKARRY/
!        TAX ITEMS
           REAL :: TXDEF,TAXDP,CEPTX,ITCDF,ITCDP,TAXEXP,TXDEFC,ITCPAY
!        AFDC ITEMS
           REAL :: CE,CEP,AFDC1,AFDC2,CWIP,RBCWIP
!        BOOK ITEMS
           REAL :: BOKDP,AFCDP
!        ABANDONMENT ITEMS
          REAL :: DDB,RBDDB,AFDPAJ,AJAFDC,EXEXP,AMORTE,AFCEXP,BKDPTX, &
          	BKDPAJ,WODFTX,AFDC1B,AFDC2B,AFDCDF
          REAL :: GPV_PROPERTY_TAX,NPV_PROPERTY_TAX
      COMMON /FA_WKARRY/ TXDEF(MAX_FINANCIAL_SIMULATION_YEARS), &
      	TAXDP(MAX_FINANCIAL_SIMULATION_YEARS), &
             CEPTX(MAX_FINANCIAL_SIMULATION_YEARS), &
             ITCDF(MAX_FINANCIAL_SIMULATION_YEARS), &
             ITCDP(MAX_FINANCIAL_SIMULATION_YEARS), &
             	TAXEXP(MAX_FINANCIAL_SIMULATION_YEARS), &
             TXDEFC(MAX_FINANCIAL_SIMULATION_YEARS), &
             CE(MAX_FINANCIAL_SIMULATION_YEARS), &
             CEP(MAX_FINANCIAL_SIMULATION_YEARS), &
             AFDC1(MAX_FINANCIAL_SIMULATION_YEARS), &
             AFDC2(MAX_FINANCIAL_SIMULATION_YEARS), &
             CWIP(MAX_FINANCIAL_SIMULATION_YEARS), &
             RBCWIP(MAX_FINANCIAL_SIMULATION_YEARS), &
             BOKDP(MAX_FINANCIAL_SIMULATION_YEARS), &
             AFCDP(MAX_FINANCIAL_SIMULATION_YEARS), &
             DDB(MAX_FINANCIAL_SIMULATION_YEARS), &
             RBDDB(MAX_FINANCIAL_SIMULATION_YEARS), &
             AFDPAJ(MAX_FINANCIAL_SIMULATION_YEARS), &
             AJAFDC(MAX_FINANCIAL_SIMULATION_YEARS), &
             EXEXP(MAX_FINANCIAL_SIMULATION_YEARS), &
             AMORTE(MAX_FINANCIAL_SIMULATION_YEARS), &
             BKDPTX(MAX_FINANCIAL_SIMULATION_YEARS), &
             ITCPAY(MAX_FINANCIAL_SIMULATION_YEARS), &
             AFCEXP(MAX_FINANCIAL_SIMULATION_YEARS), &
             BKDPAJ(MAX_FINANCIAL_SIMULATION_YEARS), &
             WODFTX(MAX_FINANCIAL_SIMULATION_YEARS), &
             AFDC1B(MAX_FINANCIAL_SIMULATION_YEARS), &
             AFDC2B(MAX_FINANCIAL_SIMULATION_YEARS), &
             AFDCDF(MAX_FINANCIAL_SIMULATION_YEARS), &
             GPV_PROPERTY_TAX(MAX_FINANCIAL_SIMULATION_YEARS), &
             NPV_PROPERTY_TAX(MAX_FINANCIAL_SIMULATION_YEARS)
!
      REAL :: TCE(:,:),TCEP(:,:),TTAXDP(:,:),TBOKDP(:,:),TAFCDP(:,:), &
           TAFDC1(:,:),TAFDC2(:,:),TCWIP(:,:),TRBCWP(:,:),TITCDP(:,:), &
           TITCDF(:,:),TTXEXP(:,:),TTXDEF(:,:),TDDB(:,:),TRBDDB(:,:), &
           TAMRTE(:,:),TEXEXP(:,:),TAFDPA(:,:),TCEPTX(:,:),TAFCAJ(:,:), &
           TBKDPT(:,:),TITCPY(:,:),TAFEXP(:,:),TBKDPA(:,:), &
           TPCAPINRST(:,:),TTXPREFDEP(:,:),TOTAL_ACE_BOOK_DEP(:,:), &
           TAFDCB(:,:),TWODFT(:,:),TAFDCF(:,:), &
           PROPERTY_TAX_GPV(:,:),PROPERTY_TAX_NPV(:,:)
      REAL :: CLASS_GPV(:,:),CLASS_CUMULATIVE_DEPRECIATION(:,:), &
           CLASS_AFUDC_IN_CWIP(:,:)
      REAL :: RB_NPV(:),RB_TAXDP(:),RB_BOKDP(:),RB_PCAPINRST(:), &
           RBCWIP_AFDC_METH2(:),RB_TXEXP(:),RB_TXDEF(:), &
           RB_TXPREFDEP(:),RB_AMRTE(:),RB_ITCPY(:),RB_ITC(:), &
           RB_AFDC1(:)
      ALLOCATABLE :: TCE,TCEP,TTAXDP,TBOKDP,TAFCDP, &
                     TAFDC1,TAFDC2,TCWIP,TRBCWP,TITCDP, &
                     TITCDF,TTXEXP,TTXDEF,TDDB,TRBDDB, &
                     TAMRTE,TEXEXP,TAFDPA,TCEPTX,TAFCAJ, &
                     TBKDPT,TITCPY,TAFEXP,TBKDPA, &
                     TPCAPINRST,TTXPREFDEP,TOTAL_ACE_BOOK_DEP, &
                     TAFDCB,TWODFT,TAFDCF,RB_NPV, &
                     RB_TAXDP,RB_BOKDP,RB_PCAPINRST, &
                     RBCWIP_AFDC_METH2,RB_TXEXP,RB_TXDEF, &
                     RB_TXPREFDEP,RB_AMRTE,RB_ITCPY,RB_ITC, &
                     RB_AFDC1,PROPERTY_TAX_GPV,PROPERTY_TAX_NPV, &
                     CLASS_GPV,CLASS_CUMULATIVE_DEPRECIATION, &
                     CLASS_AFUDC_IN_CWIP
!
      LOGICAL (KIND=1) :: FUTURE_ASSET_REPORT
      CHARACTER (LEN=40) :: OUT_OPTION_NAME
      INTEGER (KIND=2) :: FA_REPORTING_UNIT,DEACTIVE_YR, &
       ASSET_CLASS_ID,MO_DEACT
      INTEGER :: FA_REPORTING_REC
      REAL (KIND=4) :: CURRENT_INTEREST_CAP_RATE(*)
      LOGICAL (KIND=1) :: VOID_LOGICAL,RETURN_ASSET_CLASS_LISTS
      INTEGER (KIND=2) :: ALLOCATION_VECTOR,J
      CHARACTER (LEN=6) :: SHORT_MONTH_NAMES 
      LOGICAL (KIND=1) :: ANNUAL_INFO_ACTIVE
!      
         FINANCIAL_SIMULATION_YEARS = get_globecom_study_period() + get_extension_period() +1
!
         CALL RETURN_INITIALIZATION_CLASSES(NUM_OF_FA_CLASSES, &
                                            MAX_ASSET_CLASS_NUM)
         IF(ALLOCATED(ASSET_CLASS_POINTER)) DEALLOCATE(ASSET_CLASS_POINTER)
         IF(MAX_ASSET_CLASS_NUM > 0) THEN
            ALLOCATE(ASSET_CLASS_POINTER(MAX_ASSET_CLASS_NUM))
            CALL RETURN_INITIALIZATION_POINTER(ASSET_CLASS_POINTER)
         ENDIF
!
         IF(ALLOCATED(TCE)) &
                    DEALLOCATE(TCE,TCEP,TTAXDP,TBOKDP,TAFCDP, &
                             TAFDC1,TAFDC2,TCWIP,TRBCWP,TITCDP, &
                             TITCDF,TTXEXP,TTXDEF,TDDB,TRBDDB, &
                             TAMRTE,TEXEXP,TAFDPA,TCEPTX,TAFCAJ, &
                             TBKDPT,TITCPY,TAFEXP,TBKDPA, &
                             TPCAPINRST,TTXPREFDEP,TOTAL_ACE_BOOK_DEP, &
                             TAFDCB,TWODFT,TAFDCF,RB_NPV, &
                             RB_TAXDP,RB_BOKDP,RB_PCAPINRST, &
                             RBCWIP_AFDC_METH2,RB_TXEXP,RB_TXDEF, &
                             RB_TXPREFDEP,RB_AMRTE,RB_ITCPY,RB_ITC, &
                             RB_AFDC1,PROPERTY_TAX_GPV,PROPERTY_TAX_NPV, &
                             CLASS_GPV,CLASS_CUMULATIVE_DEPRECIATION, &
                             CLASS_AFUDC_IN_CWIP)
!
         ALLOCATE(TCE(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(TCEP(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(CLASS_GPV(FINANCIAL_SIMULATION_YEARS, &
         	0:NUM_OF_FA_CLASSES))
         ALLOCATE(CLASS_CUMULATIVE_DEPRECIATION( &
         	FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(CLASS_AFUDC_IN_CWIP(FINANCIAL_SIMULATION_YEARS, &
         	0:NUM_OF_FA_CLASSES))
         ALLOCATE(TTAXDP(FINANCIAL_SIMULATION_YEARS, &
         	0:NUM_OF_FA_CLASSES))
         ALLOCATE(TBOKDP(FINANCIAL_SIMULATION_YEARS, &
         	0:NUM_OF_FA_CLASSES))
         ALLOCATE(TAFCDP(FINANCIAL_SIMULATION_YEARS, &
         	0:NUM_OF_FA_CLASSES))
         ALLOCATE(TAFDC1(FINANCIAL_SIMULATION_YEARS, &
         	0:NUM_OF_FA_CLASSES))
         ALLOCATE(TAFDC2(FINANCIAL_SIMULATION_YEARS, &
         	0:NUM_OF_FA_CLASSES))
         ALLOCATE(TCWIP(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(TRBCWP(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(TITCDP(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(TITCDF(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(TTXEXP(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(TTXDEF(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(TDDB(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(TRBDDB(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(TAMRTE(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(TEXEXP(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(TAFDPA(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(TCEPTX(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(TAFCAJ(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(TBKDPT(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(TITCPY(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(TAFEXP(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(TBKDPA(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(TPCAPINRST(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(TTXPREFDEP(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(TOTAL_ACE_BOOK_DEP( &
            FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(TAFDCB(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(TWODFT(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(TAFDCF(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(PROPERTY_TAX_GPV( &
            FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(PROPERTY_TAX_NPV( &
            FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
!
         ALLOCATE(RB_NPV(FINANCIAL_SIMULATION_YEARS))
         ALLOCATE(RB_TAXDP(FINANCIAL_SIMULATION_YEARS))
         ALLOCATE(RB_BOKDP(FINANCIAL_SIMULATION_YEARS))
         ALLOCATE(RB_PCAPINRST(FINANCIAL_SIMULATION_YEARS))
         ALLOCATE(RBCWIP_AFDC_METH2(FINANCIAL_SIMULATION_YEARS))
         ALLOCATE(RB_TXEXP(FINANCIAL_SIMULATION_YEARS))
         ALLOCATE(RB_TXDEF(FINANCIAL_SIMULATION_YEARS))
         ALLOCATE(RB_TXPREFDEP(FINANCIAL_SIMULATION_YEARS))
         ALLOCATE(RB_AMRTE(FINANCIAL_SIMULATION_YEARS))
         ALLOCATE(RB_ITCPY(FINANCIAL_SIMULATION_YEARS))
         ALLOCATE(RB_ITC(FINANCIAL_SIMULATION_YEARS))
         ALLOCATE(RB_AFDC1(FINANCIAL_SIMULATION_YEARS))
!
      RETURN

      ENTRY INIT_FA_ARRAYS

!
         CLASS_GPV = 0.
         CLASS_CUMULATIVE_DEPRECIATION = 0.
         CLASS_AFUDC_IN_CWIP = 0.
         TCE = 0.
         TCEP = 0.
         TTAXDP = 0.
         TBOKDP = 0.
         TAFCDP = 0.
         TAFDC1 = 0.
         TAFDC2 = 0.
         TCWIP = 0.
         TRBCWP = 0.
         TITCDP = 0.
         TITCDF = 0.
         TTXEXP = 0.
         TTXDEF = 0.
         TDDB = 0.
         TRBDDB = 0.
         TAMRTE = 0.
         TEXEXP = 0.
         TAFDPA = 0.
         TCEPTX = 0.
         TAFCAJ = 0.
         TBKDPT = 0.
         TITCPY = 0.
         TAFEXP = 0.
         TBKDPA = 0.
         TPCAPINRST = 0.
         TTXPREFDEP = 0.
         TOTAL_ACE_BOOK_DEP = 0.
         TAFDCB = 0.
         TWODFT = 0.
         TAFDCF = 0.
         PROPERTY_TAX_GPV = 0.
         PROPERTY_TAX_NPV = 0.
!
         RB_NPV = 0.
         RB_TAXDP = 0.
         RB_BOKDP = 0.
         RB_PCAPINRST = 0.
         RBCWIP_AFDC_METH2 = 0.
         RB_TXEXP = 0.
         RB_TXDEF = 0.
         RB_TXPREFDEP = 0.
         RB_AMRTE = 0.
         RB_ITCPY = 0.
         RB_ITC = 0.
         RB_AFDC1 = 0.
!                 
      RETURN
!

      ENTRY FA_TOTALS(ABMETH,ABYEAR,TAXWO,PCAPINRST,TXPREFDEP, &
                      REGULATORY_ALLOCATOR,ABTAX,ACE_BOOK_DEP, &
                      AFUDC_IN_CWIP, &
                      R_ASSET_CLASS,ASSET_CLASS_VECTOR)

!
! ALLOCATE TO TOTAL COMPANY AND ASSET CLASSES
!
         VOID_LOGICAL=RETURN_ASSET_CLASS_LISTS(R_ASSET_CLASS, &
                                               ASSET_CLASS_LIST, &
                                               ASSET_CLASS_VECTOR, &
                                               ASSET_ALLOCATION_LIST)
!
         CLASS_POINTER = 1
         DO
            CLASS = ASSET_CLASS_LIST(CLASS_POINTER)
            CALL CHECK_IF_CLASS_DEFINED(CLASS)
            CLASS = CLASS + 1
            ASSET_CLASS_ID = CLASS
            IF(CLASS > 0) CLASS = ASSET_CLASS_POINTER(CLASS)
            IF(ASSET_ALLOCATION_LIST(CLASS_POINTER) < 0.) THEN
               ALLOCATION_VECTOR =  &
               	ABS(ASSET_ALLOCATION_LIST(CLASS_POINTER))
               CALL GET_ASSET_VAR(ALLOCATION_VECTOR, &
               	DUMMY_TYPE,ALLOCATION_VALUE(1))
               CALL RETURN_BASE_YEAR_VECTOR_VALUES(ALLOCATION_VALUE(0))
            ELSE
               ALLOCATOR = ASSET_ALLOCATION_LIST(CLASS_POINTER)
!                        
               DO I = 0, AVAIL_DATA_YEARS
                  ALLOCATION_VALUE(I) = ALLOCATOR
               ENDDO
            ENDIF
!
            IF(REGULATORY_ALLOCATOR /= 1.) THEN
               DO I = 0, AVAIL_DATA_YEARS
                  ALLOCATION_VALUE(I) = REGULATORY_ALLOCATOR * ALLOCATION_VALUE(I)
               ENDDO
            ENDIF
            IF(ABMETH > 0) THEN
               I = MAX(2,ABYEAR - get_BASE_YEAR() + 1)
               J = MIN(INT(I-1,2),AVAIL_DATA_YEARS)
               TTXEXP(I,CLASS) = TTXEXP(I,CLASS) + TAXWO * &
               	ALLOCATION_VALUE(J)/100.
            ENDIF
!
            GPV_ADDITIONS = 0.
            DEP_ADDITIONS = 0.
            CUM_DD_AMORT = 0.
            
            ! ALLOCATION 0 IS USED TWICE
            ALLOCATOR = ALLOCATION_VALUE(0)/100. 
            CALL CLASS_DEACTIVATE_IN_YR(ASSET_CLASS_ID,DEACTIVE_YR,MO_DEACT)
            DO I = 1, FINANCIAL_SIMULATION_YEARS
!
!        CUMMULATE TOTAL COMPANY
!
               IF(I > DEACTIVE_YR + 1) EXIT
               IF(I == DEACTIVE_YR + 1) THEN
                  CLASS_GPV(I,CLASS) = 0. ! -CLASS_GPV(I-1,CLASS)
                  CLASS_CUMULATIVE_DEPRECIATION(I,CLASS) = 0.
!
                  CLASS_AFUDC_IN_CWIP(I,CLASS) = 0.
!
                  TCWIP(I,CLASS)  = 0. !
                  TRBCWP(I,CLASS) = 0. !
                  EXIT
               ENDIF
               IF(I > 1) THEN
                  IF(LGandE()) THEN
                     GPV_ADDITIONS = GPV_ADDITIONS + (CEP(I) + AFDC2(I))
           CLASS_GPV(I,CLASS) = CLASS_GPV(I,CLASS) + ALLOCATOR * GPV_ADDITIONS
!
                     DEP_ADDITIONS = DEP_ADDITIONS + BOKDP(I) + AFCDP(I)
                     CLASS_CUMULATIVE_DEPRECIATION(I,CLASS) = &
                                  CLASS_CUMULATIVE_DEPRECIATION(I,CLASS) &
                                  + ALLOCATOR * DEP_ADDITIONS
                  ELSE
                  GPV_ADDITIONS = GPV_ADDITIONS + ALLOCATOR * (CEP(I) + AFDC2(I))
                     CLASS_GPV(I,CLASS) = CLASS_GPV(I,CLASS) + GPV_ADDITIONS
              DEP_ADDITIONS = DEP_ADDITIONS + ALLOCATOR * (BOKDP(I) + AFCDP(I))
                     CLASS_CUMULATIVE_DEPRECIATION(I,CLASS) = &
                                  CLASS_CUMULATIVE_DEPRECIATION(I,CLASS) &
                                  + DEP_ADDITIONS
                  ENDIF
               ENDIF
               CLASS_AFUDC_IN_CWIP(I,CLASS)=ALLOCATOR*AFUDC_IN_CWIP(I) + &
               	CLASS_AFUDC_IN_CWIP(I,CLASS)
               TCE(I,CLASS)    = TCE(I,CLASS)    + CE(I) * ALLOCATOR
               TCEP(I,CLASS)   = TCEP(I,CLASS)   + CEP(I) * ALLOCATOR
               TCEPTX(I,CLASS) = TCEPTX(I,CLASS) + CEPTX(I) * ALLOCATOR
               TTAXDP(I,CLASS) = TTAXDP(I,CLASS) + TAXDP(I) * ALLOCATOR
               TBKDPT(I,CLASS) = TBKDPT(I,CLASS) + BKDPTX(I) * ALLOCATOR
               TBOKDP(I,CLASS) = TBOKDP(I,CLASS) + BOKDP(I) * ALLOCATOR
               TAFCDP(I,CLASS) = TAFCDP(I,CLASS) + AFCDP(I) * ALLOCATOR
               TAFDC1(I,CLASS) = TAFDC1(I,CLASS) + AFDC1(I) * ALLOCATOR
               TAFDCB(I,CLASS) = TAFDCB(I,CLASS) + AFDC1B(I) * ALLOCATOR
               TAFDCF(I,CLASS) = TAFDCF(I,CLASS) + AFDCDF(I) * ALLOCATOR
               TAFDC2(I,CLASS) = TAFDC2(I,CLASS) + AFDC2(I) * ALLOCATOR
               TCWIP(I,CLASS)  = TCWIP(I,CLASS)  + CWIP(I) * ALLOCATOR
               TRBCWP(I,CLASS) = TRBCWP(I,CLASS) + RBCWIP(I) * ALLOCATOR
               TITCDP(I,CLASS) = TITCDP(I,CLASS) + ITCDP(I) * ALLOCATOR
               TITCDF(I,CLASS) = TITCDF(I,CLASS) + ITCDF(I) * ALLOCATOR
               TTXEXP(I,CLASS) = TTXEXP(I,CLASS) + TAXEXP(I) * ALLOCATOR
               TTXDEF(I,CLASS) = TTXDEF(I,CLASS) + TXDEF(I) * ALLOCATOR
               TPCAPINRST(I,CLASS) = TPCAPINRST(I,CLASS) + &
               	PCAPINRST(I) * ALLOCATOR
               TTXPREFDEP(I,CLASS) = TTXPREFDEP(I,CLASS) + &
               	TXPREFDEP(I) * ALLOCATOR
               TOTAL_ACE_BOOK_DEP(I,CLASS)=TOTAL_ACE_BOOK_DEP(I,CLASS) + &
               	ACE_BOOK_DEP(I) * ALLOCATOR
               PROPERTY_TAX_GPV(I,CLASS) = PROPERTY_TAX_GPV(I,CLASS) + &
               	GPV_PROPERTY_TAX(I) * ALLOCATOR
               PROPERTY_TAX_NPV(I,CLASS) = PROPERTY_TAX_NPV(I,CLASS) + &
               	NPV_PROPERTY_TAX(I) * ALLOCATOR
               IF(ABMETH > 0) THEN
                  CUM_DD_AMORT = CUM_DD_AMORT + AMORTE(I)
                  TDDB(I,CLASS)   = TDDB(I,CLASS) + &
                  	(DDB(I) - CUM_DD_AMORT) * ALLOCATOR
                  TRBDDB(I,CLASS) = TRBDDB(I,CLASS) + RBDDB(I)*ALLOCATOR
                  TAMRTE(I,CLASS) = TAMRTE(I,CLASS)+AMORTE(I)*ALLOCATOR
                  TEXEXP(I,CLASS) = TEXEXP(I,CLASS) + EXEXP(I)*ALLOCATOR
                  TAFEXP(I,CLASS) = TAFEXP(I,CLASS)+AFCEXP(I)*ALLOCATOR
                  TAFDPA(I,CLASS) = TAFDPA(I,CLASS)+AFDPAJ(I)*ALLOCATOR
                  TBKDPA(I,CLASS) = TBKDPA(I,CLASS)+BKDPAJ(I)*ALLOCATOR
                  TAFCAJ(I,CLASS) = TAFCAJ(I,CLASS)+AJAFDC(I)*ALLOCATOR
                  TITCPY(I,CLASS) = TITCPY(I,CLASS)+ITCPAY(I)*ALLOCATOR
                  TWODFT(I,CLASS) = TWODFT(I,CLASS)+WODFTX(I)*ALLOCATOR
               ENDIF
               IF(I <= AVAIL_DATA_YEARS) ALLOCATOR=ALLOCATION_VALUE(I)/100.
            ENDDO
            CLASS_POINTER = CLASS_POINTER + 1
            IF(CLASS_POINTER > AVAIL_DATA_YEARS) EXIT
            IF(ASSET_CLASS_LIST(CLASS_POINTER) == 0. .OR. &
            	ASSET_CLASS_LIST(CLASS_POINTER) == -99.) then
            		EXIT
            	end if
         ENDDO    
!              
         IF(REGULATORY_ALLOCATOR /= 0.) THEN
            CUMNPV = 0.
            IF(ABMETH > 0) THEN
               I = ABYEAR - get_BASE_YEAR() + 1
               IF(ABMETH /= 6) RB_TXEXP(I) = RB_TXEXP(I) + TAXWO*ABTAX * &
               	REGULATORY_ALLOCATOR
            ENDIF
            DO I = 1, FINANCIAL_SIMULATION_YEARS
               CUMNPV = CUMNPV +(CEP(I)+AFDC2(I)-BOKDP(I)-AFCDP(I)) * &
               	REGULATORY_ALLOCATOR
               RB_NPV(I)   = RB_NPV(I) + CUMNPV
               RBCWIP_AFDC_METH2(I) = RBCWIP_AFDC_METH2(I) + (CWIP(I) + &
               	AFUDC_IN_CWIP(I)) * REGULATORY_ALLOCATOR
               RB_TAXDP(I) = RB_TAXDP(I) + TAXDP(I) * REGULATORY_ALLOCATOR
               RB_AFDC1(I) = RB_AFDC1(I) + AFDC1(I) * REGULATORY_ALLOCATOR
               RB_BOKDP(I) = RB_BOKDP(I) + (BOKDP(I) + AFCDP(I)) * &
               	REGULATORY_ALLOCATOR
               RB_TXPREFDEP(I) = RB_TXPREFDEP(I) + TXPREFDEP(I) * &
               	REGULATORY_ALLOCATOR
               RB_PCAPINRST(I) = RB_PCAPINRST(I) + PCAPINRST(I) * &
               	REGULATORY_ALLOCATOR
               RB_TXEXP(I) = RB_TXEXP(I) + TAXEXP(I) * &
               	REGULATORY_ALLOCATOR
               RB_TXDEF(I) = RB_TXDEF(I) + TXDEF(I) * &
               	REGULATORY_ALLOCATOR
               RB_ITC(I)   = RB_ITC(I) + (ITCDP(I) + ITCDF(I)) * &
               	REGULATORY_ALLOCATOR
               IF(ABMETH > 0) THEN
                  RB_AMRTE(I) = RB_AMRTE(I) + AMORTE(I) * REGULATORY_ALLOCATOR
                  RB_ITCPY(I) = RB_ITCPY(I) + ITCPAY(I) * REGULATORY_ALLOCATOR
               ENDIF
            ENDDO
         ENDIF
      RETURN

      ENTRY SAVE_FUTURE_ASSET_TOTALS(SAVE_BASE_CASE,FUTURE_ASSET_REPORT, &
                                     FA_REPORTING_UNIT, &
                                     CURRENT_INTEREST_CAP_RATE, &
                                     FA_REPORTING_REC, &
                                     ANNUAL_INFO_ACTIVE)

!
! WRITE THE FUASSET FILE 
!
      DO CLASS = 1, NUM_OF_FA_CLASSES
         DO I = 1, FINANCIAL_SIMULATION_YEARS
            CLASS_GPV(I,0) = CLASS_GPV(I,0) + CLASS_GPV(I,CLASS) 
            CLASS_CUMULATIVE_DEPRECIATION(I,0) = &
                                  CLASS_CUMULATIVE_DEPRECIATION(I,0) + &
                                  CLASS_CUMULATIVE_DEPRECIATION(I,CLASS)
            CLASS_AFUDC_IN_CWIP(I,0) = CLASS_AFUDC_IN_CWIP(I,0)+ &
                                            CLASS_AFUDC_IN_CWIP(I,CLASS)
            TCE(I,0)    = TCE(I,0)    + TCE(I,CLASS)
            TCEP(I,0)   = TCEP(I,0)   + TCEP(I,CLASS)
            TCEPTX(I,0) = TCEPTX(I,0) + TCEPTX(I,CLASS)
            TTAXDP(I,0) = TTAXDP(I,0) + TTAXDP(I,CLASS)
            TBKDPT(I,0) = TBKDPT(I,0) + TBKDPT(I,CLASS)
            TBOKDP(I,0) = TBOKDP(I,0) + TBOKDP(I,CLASS)
            TAFCDP(I,0) = TAFCDP(I,0) + TAFCDP(I,CLASS)
            TAFDC1(I,0) = TAFDC1(I,0) + TAFDC1(I,CLASS)
            TAFDCB(I,0) = TAFDCB(I,0) + TAFDCB(I,CLASS)
            TAFDCF(I,0) = TAFDCF(I,0) + TAFDCF(I,CLASS)
            TAFDC2(I,0) = TAFDC2(I,0) + TAFDC2(I,CLASS)
            TCWIP(I,0)  = TCWIP(I,0)  + TCWIP(I,CLASS)
            TRBCWP(I,0) = TRBCWP(I,0) + TRBCWP(I,CLASS)
            TITCDP(I,0) = TITCDP(I,0) + TITCDP(I,CLASS)
            TITCDF(I,0) = TITCDF(I,0) + TITCDF(I,CLASS)
            TTXEXP(I,0) = TTXEXP(I,0) + TTXEXP(I,CLASS)
            TTXDEF(I,0) = TTXDEF(I,0) + TTXDEF(I,CLASS)
            TDDB(I,0)   = TDDB(I,0)   + TDDB(I,CLASS)
            TRBDDB(I,0) = TRBDDB(I,0) + TRBDDB(I,CLASS)
            TAMRTE(I,0) = TAMRTE(I,0) + TAMRTE(I,CLASS)
            TEXEXP(I,0) = TEXEXP(I,0) + TEXEXP(I,CLASS)
            TAFEXP(I,0) = TAFEXP(I,0) + TAFEXP(I,CLASS)
            TAFDPA(I,0) = TAFDPA(I,0) + TAFDPA(I,CLASS)
            TBKDPA(I,0) = TBKDPA(I,0) + TBKDPA(I,CLASS)
            TAFCAJ(I,0) = TAFCAJ(I,0) + TAFCAJ(I,CLASS)
            TITCPY(I,0) = TITCPY(I,0) + TITCPY(I,CLASS)
            TWODFT(I,0) = TWODFT(I,0) + TWODFT(I,CLASS)
!
            TPCAPINRST(I,0) = TPCAPINRST(I,0) + TPCAPINRST(I,CLASS)
            TTXPREFDEP(I,0) = TTXPREFDEP(I,0) + TTXPREFDEP(I,CLASS)
            TOTAL_ACE_BOOK_DEP(I,0)=TOTAL_ACE_BOOK_DEP(I,0) + &
                TOTAL_ACE_BOOK_DEP(I,CLASS)
            PROPERTY_TAX_GPV(I,0) = PROPERTY_TAX_GPV(I,0) + &
                PROPERTY_TAX_GPV(I,CLASS)
            PROPERTY_TAX_NPV(I,0) = PROPERTY_TAX_NPV(I,0) + &
                PROPERTY_TAX_NPV(I,CLASS)
         ENDDO
      ENDDO
!
! WRITE SUMMARY REPORT
!
      IF(FUTURE_ASSET_REPORT) THEN
         DRILLING_ACCOUNT_NAME = "Total for Future Assets"
         IF(.NOT. ANNUAL_INFO_ACTIVE) THEN
            DO I = 1, FINANCIAL_SIMULATION_YEARS
               EXIT
               DO MO = 0, 12
                  WRITE(FA_REPORTING_UNIT,REC=FA_REPORTING_REC) &
                                   PRT_ENDPOINT(), &
                                   DRILLING_ACCOUNT_NAME, &
                                   FLOAT(I+get_BASE_YEAR()-1), &
                                   SHORT_MONTH_NAMES(MO), &
                                   TCE(I,MO),TAFDC1(I,MO), &
                                   TCWIP(I,MO),TCEP(I,MO), &
                                   TAFDC2(I,MO), &
                                   TBOKDP(I,MO)+TAFCDP(I,MO), &
                                   TTXDEF(I,MO),TTAXDP(I,MO), &
                                   TBKDPT(I,MO), &
                                   TTXEXP(I,MO), &
                                   TOTAL_ACE_BOOK_DEP(I,MO), &
                                   TPCAPINRST(I,MO), &
                                   TPCAPINRST(I,MO) * &
                                   (1.-CURRENT_INTEREST_CAP_RATE(I)), &
                                   CLASS_AFUDC_IN_CWIP(I,MO), &
                                   TCWIP(I,MO)+CLASS_AFUDC_IN_CWIP(I,MO)
                  FA_REPORTING_REC = FA_REPORTING_REC + 1
               ENDDO
            ENDDO
         ELSE
            DO I = 1, FINANCIAL_SIMULATION_YEARS
               WRITE(FA_REPORTING_UNIT,REC=FA_REPORTING_REC) &
                                     PRT_ENDPOINT(), &
                                     DRILLING_ACCOUNT_NAME, &
                                     FLOAT(I+get_BASE_YEAR()-1), &
                                     TCE(I,0),TAFDC1(I,0), &
                                     TCWIP(I,0),TCEP(I,0), &
                                     TAFDC2(I,0), &
                                     TBOKDP(I,0)+TAFCDP(I,0), & 
                                     TTXDEF(I,0),TTAXDP(I,0), &
                                     TBKDPT(I,0), &
                                     TTXEXP(I,0), &
                                     TOTAL_ACE_BOOK_DEP(I,0), &
                                     TPCAPINRST(I,0), &
                                     TPCAPINRST(I,0) * &
                                     (1.-CURRENT_INTEREST_CAP_RATE(I)), &
                                     CLASS_AFUDC_IN_CWIP(I,0), &
                                     TCWIP(I,0)+CLASS_AFUDC_IN_CWIP(I,0)
               FA_REPORTING_REC = FA_REPORTING_REC + 1
            ENDDO
         ENDIF
      ENDIF
      CALL OPEN_FA_OUT_FILE(INT(10,2))
      DO I = 1, FINANCIAL_SIMULATION_YEARS
         WRITE(10,REC=I) TCE(I,0),TCEP(I,0),TTAXDP(I,0),TBOKDP(I,0), &
            TAFCDP(I,0),TAFDC1(I,0),TAFDC2(I,0),TCWIP(I,0),TRBCWP(I,0), &
            TITCDP(I,0),TITCDF(I,0),TTXEXP(I,0),TTXDEF(I,0), &
! WRITE-OFF VARIABLES
            TBKDPA(I,0),TDDB(I,0),TRBDDB(I,0),TAMRTE(I,0), &
            TEXEXP(I,0),TAFDPA(I,0),TAFDCB(I,0),TAFCAJ(I,0), &
            TITCPY(I,0),TAFEXP(I,0),TWODFT(I,0),TAFDCF(I,0), &
! 
            TBKDPT(I,0),TPCAPINRST(I,0),TTXPREFDEP(I,0), &
            RB_NPV(I),RB_TAXDP(I),RB_BOKDP(I),RB_PCAPINRST(I), &
            RBCWIP_AFDC_METH2(I), &
            RB_TXEXP(I),RB_TXDEF(I),RB_TXPREFDEP(I), &
            RB_AMRTE(I),RB_ITCPY(I),RB_ITC(I),RB_AFDC1(I), &
            TOTAL_ACE_BOOK_DEP(I,0), &
            PROPERTY_TAX_GPV(I,0),PROPERTY_TAX_NPV(I,0)
      ENDDO
      CLOSE(10)
      IF(SAVE_BASE_CASE) THEN
         CALL OPEN_FA_BASE_CASE_FILE(INT(10,2))
         WRITE(10) CLASS_GPV,CLASS_CUMULATIVE_DEPRECIATION, &
                   CLASS_AFUDC_IN_CWIP, &
                   TCE,TCEP,TTAXDP,TBOKDP, &
                   TAFCDP,TAFDC1,TAFDC2,TCWIP,TRBCWP, &
                   TITCDP,TITCDF,TTXEXP,TTXDEF, &
! WRITE-OFF VARIABLES
                   TBKDPA,TDDB,TRBDDB,TAMRTE,TEXEXP,TAFDPA, &
                   TAFDCB,TAFCAJ,TITCPY,TAFEXP,TWODFT, &
                   TAFDCF,TBKDPT,TPCAPINRST,TTXPREFDEP, &
! RB ITEMS
                   RB_NPV,RB_TAXDP,RB_BOKDP,RB_PCAPINRST, &
                   RBCWIP_AFDC_METH2, &
                   RB_TXEXP,RB_TXDEF,RB_TXPREFDEP, &
                   RB_AMRTE,RB_ITCPY,RB_ITC,RB_AFDC1, &
                   TOTAL_ACE_BOOK_DEP, &
                   PROPERTY_TAX_GPV,PROPERTY_TAX_NPV
         CLOSE(10)
      ENDIF
      RETURN

      ENTRY READ_FA_BASE_CASE

!
         CALL SET_UP_FA_ARRAYS
         CALL OPEN_FA_BASE_CASE_FILE(INT(10,2))
         READ(10) CLASS_GPV,CLASS_CUMULATIVE_DEPRECIATION, &
                  CLASS_AFUDC_IN_CWIP, &
                  TCE,TCEP,TTAXDP,TBOKDP, &
                  TAFCDP,TAFDC1,TAFDC2,TCWIP,TRBCWP, &
                  TITCDP,TITCDF,TTXEXP,TTXDEF,TBKDPA, &
                  TDDB,TRBDDB,TAMRTE,TEXEXP,TAFDPA, &
                  TAFDCB,TAFCAJ,TITCPY,TAFEXP,TWODFT, &
                  TAFDCF,TBKDPT,TPCAPINRST,TTXPREFDEP, &
                  RB_NPV,RB_TAXDP,RB_BOKDP,RB_PCAPINRST, &
                  RBCWIP_AFDC_METH2, &
                  RB_TXEXP,RB_TXDEF,RB_TXPREFDEP, &
                  RB_AMRTE,RB_ITCPY,RB_ITC,RB_AFDC1, &
                  TOTAL_ACE_BOOK_DEP, &
                  PROPERTY_TAX_GPV,PROPERTY_TAX_NPV
         CLOSE(10)
         IF(MONTHLY_MIDAS_ACTIVE) THEN
            CALL RETRIVE_MONTHLY_FA_VALUES
         ENDIF
      RETURN

      ENTRY FUTURE_ASSET_BY_INFO(R_CLASS,FA_CWIP,FA_CWIP_IN_RB)

         FA_CWIP = 0.
         FA_CWIP_IN_RB = 0.
         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN 
               FA_CWIP = TCWIP(1,ASSET_CLASS) +  &
				CLASS_AFUDC_IN_CWIP(1,ASSET_CLASS)
               FA_CWIP_IN_RB = TRBCWP(1,ASSET_CLASS)
            ENDIF
         ENDIF
      RETURN

      ENTRY FUTURE_ASSET_INFO(R_YR,R_CLASS,R_CLASS_EXISTS, &
		R_CLASS_GPV,R_CLASS_CUMULATIVE_DEPRECIATION, &
           FA_CASH,FA_CAPITIALIZED,FA_TAX_DEP,FA_BOOK_DEP, &
		   FA_AFDC_CASH,FA_AFDC_CAPITIALIZED,FA_CWIP, &
           FA_CWIP_IN_RB,FA_CURRENT_TAX_EXPENSES, &
		   FA_CAPITIALIZED_INTEREST, &
           FA_PROPERTY_TAX_GPV,FA_PROPERTY_TAX_NPV, &
           FA_AMORTIZATION_OF_DEF_DEBIT, &
           FA_EXTRAORDINARY_EXPENSE, &
           FA_NET_DEFERRED_DEBIT_BAL, &
           FA_DEF_DEBIT_IN_RB, &
           FA_DEF_TAX_B4_TAX_RATES, & 
           FA_SL_TAX_DEP, &           
           FA_TAX_PREFERENCE_DEP, &   
           FA_ACE_BOOK_DEP, &         
! write-off adjustment to cumulative book depreciaton
           FA_WO_ADJUSTMENT_2_CUM_BOOK_DEP, & 
!  write_off adj to cumulative afdc dep.
           FA_WO_ADJUSTMENT_2_CUM_AFDC_DEP, & 
           FA_AFDC_BORROWED, & 
           FA_WO_ADJUSTMENT_2_CUM_AFDC, & 
           FA_WO_ADJUSTMENT_2_CUM_AFEXP, & 
           FA_WO_ADJUSTMENT_2_CUM_DEF_TAX, &
           FA_WO_ADJUSTMENT_2_CUM_AFDCF, & 
           FA_ITC_CREDIT)

!
         R_CLASS_EXISTS = .FALSE.
         R_CLASS_CUMULATIVE_DEPRECIATION = 0.
         FA_CASH = 0.
         FA_CAPITIALIZED = 0.
         FA_AFDC_CASH = 0.
         FA_AFDC_CAPITIALIZED = 0.
         FA_CURRENT_TAX_EXPENSES = 0.
         FA_PROPERTY_TAX_GPV = 0.
         FA_PROPERTY_TAX_NPV = 0.
         FA_EXTRAORDINARY_EXPENSE = 0.
         FA_NET_DEFERRED_DEBIT_BAL = 0.
         FA_SL_TAX_DEP = 0. ! TBKDPT(I)
         FA_WO_ADJUSTMENT_2_CUM_BOOK_DEP = 0.
         FA_WO_ADJUSTMENT_2_CUM_AFDC_DEP = 0.
         FA_AFDC_BORROWED = 0. ! TAFDCB(I)
         FA_WO_ADJUSTMENT_2_CUM_AFDC = 0. ! TAFCAJ(I)
         FA_WO_ADJUSTMENT_2_CUM_AFEXP = 0.  ! TAFEXP(I)
         FA_WO_ADJUSTMENT_2_CUM_DEF_TAX = 0. ! TWODFT(I)
         FA_WO_ADJUSTMENT_2_CUM_AFDCF = 0.  ! TAFDCF(I)
         FA_ITC_CREDIT = 0.
!
         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN 
               R_CLASS_GPV = R_CLASS_GPV + CLASS_GPV(R_YR,ASSET_CLASS)
               R_CLASS_CUMULATIVE_DEPRECIATION = &
               	CLASS_CUMULATIVE_DEPRECIATION(R_YR,ASSET_CLASS)
               FA_CASH = TCE(R_YR,ASSET_CLASS)
               FA_CAPITIALIZED = TCEP(R_YR,ASSET_CLASS)
               FA_TAX_DEP = FA_TAX_DEP + TTAXDP(R_YR,ASSET_CLASS)
               FA_BOOK_DEP = FA_BOOK_DEP + TBOKDP(R_YR,ASSET_CLASS) + &
               	TAFCDP(R_YR,ASSET_CLASS)
               FA_AFDC_CASH = TAFDC1(R_YR,ASSET_CLASS)
               FA_AFDC_CAPITIALIZED =TAFDC2(R_YR,ASSET_CLASS) 
               FA_CWIP = FA_CWIP + TCWIP(R_YR,ASSET_CLASS) + &
               	CLASS_AFUDC_IN_CWIP(R_YR,ASSET_CLASS)
               FA_CWIP_IN_RB = FA_CWIP_IN_RB + TRBCWP(R_YR,ASSET_CLASS)
               FA_CURRENT_TAX_EXPENSES = TTXEXP(R_YR,ASSET_CLASS)
               FA_CAPITIALIZED_INTEREST = FA_CAPITIALIZED_INTEREST + &
               	TPCAPINRST(R_YR,ASSET_CLASS)
               FA_PROPERTY_TAX_GPV = PROPERTY_TAX_GPV(R_YR,ASSET_CLASS)
               FA_PROPERTY_TAX_NPV = PROPERTY_TAX_NPV(R_YR,ASSET_CLASS)
               FA_AMORTIZATION_OF_DEF_DEBIT = &
                                         FA_AMORTIZATION_OF_DEF_DEBIT +  &
                                         TAMRTE(R_YR,ASSET_CLASS)
               FA_EXTRAORDINARY_EXPENSE = TEXEXP(R_YR,ASSET_CLASS)
               FA_NET_DEFERRED_DEBIT_BAL = TDDB(R_YR,ASSET_CLASS)
               FA_DEF_DEBIT_IN_RB = FA_DEF_DEBIT_IN_RB + &
                                                TRBDDB(R_YR,ASSET_CLASS)
               FA_DEF_TAX_B4_TAX_RATES = FA_DEF_TAX_B4_TAX_RATES + &
               	TTXDEF(R_YR,ASSET_CLASS)
               FA_SL_TAX_DEP = TBKDPT(R_YR,ASSET_CLASS)
               FA_TAX_PREFERENCE_DEP = FA_TAX_PREFERENCE_DEP + &
               	TTXPREFDEP(R_YR,ASSET_CLASS)
               FA_ACE_BOOK_DEP = FA_ACE_BOOK_DEP + &
               	TOTAL_ACE_BOOK_DEP(R_YR,ASSET_CLASS)
               FA_WO_ADJUSTMENT_2_CUM_BOOK_DEP= &
               	TBKDPA(R_YR,ASSET_CLASS) 
               FA_WO_ADJUSTMENT_2_CUM_AFDC_DEP= &
               	TAFDPA(R_YR,ASSET_CLASS) 
               FA_AFDC_BORROWED = TAFDCB(R_YR,ASSET_CLASS)
               FA_WO_ADJUSTMENT_2_CUM_AFDC = TAFCAJ(R_YR,ASSET_CLASS)
               FA_WO_ADJUSTMENT_2_CUM_AFEXP = TAFEXP(R_YR,ASSET_CLASS)
               FA_WO_ADJUSTMENT_2_CUM_DEF_TAX = TWODFT(R_YR,ASSET_CLASS)
               FA_WO_ADJUSTMENT_2_CUM_AFDCF = TAFDCF(R_YR,ASSET_CLASS)
               FA_ITC_CREDIT = TITCDP(R_YR,ASSET_CLASS)
            ENDIF
         ENDIF
      RETURN
      END

!*                                                                     *
!*                      REAL_PROPERTY_VALUES                           *
!*                                                                     *
!*          COPYRIGHT (C) 1992 M.S. GERBER & ASSOCIATES, INC.          *
!*                       ALL RIGHTS RESERVED                           *
!*                                                                     *

!*                                                                     *
!*    PURPOSE:                                                         *
!*       CALCULATES THE ESCALATING GROSS PLANT AND NPV OF NEW PLANT    *
!*                                                                     *

!
      SUBROUTINE REAL_PROPERTY_VALUES(SERVICEMO,PROPERTY_ESCALATION, &
      	FINANCIAL_SIMULATION_YEARS)
!
      use SpinDriftLib
      USE PROD_ARRAYS_DIMENSIONS
      USE SIZECOM
      use globecom
      use mod_base_year

      REAL :: PROPERTY_ESCALATION(*)
      REAL :: BOOK_DEP(MAX_FINANCIAL_SIMULATION_YEARS), &
      	NPV,SL_DEP,DEP,GPV,ESCALATOR
      INTEGER (KIND=2) :: SERVICEMO,FINANCIAL_SIMULATION_YEARS
      INTEGER (KIND=2) :: J,LAST_YEAR
      INTEGER (KIND=2) :: I,ABMETH,ABYEAR
      CHARACTER (LEN=1) :: ABACCT
      REAL (KIND=4) :: AFUDC_IN_CWIP(*)
      REAL :: NRTXWO,BOKBL,BOKWO,BOKDAL,AFCBL,AFCWO,AFCDAL
      REAL :: PCAPINRST(*),TXPREFDEP(*),ACE_BOOK_DEP(*),ABTAX
      REAL :: CCE,CCEP,CCEPTX,CTAXDP,CTXEXP,CITCDP,CITCDF, &
      	CBKDPT,CBOKDP,CAFDC1,CAFDC2,CAFDCF,CAFCDP,CRBCWP,TEMP1,TEMP2,TEMP3
      REAL :: TAXWO
!     TYPE DECLARATION FOR /WKARRY/
!        TAX ITEMS
           REAL :: TXDEF,TAXDP,CEPTX,ITCDF,ITCDP,TAXEXP,TXDEFC,ITCPAY
!        AFDC ITEMS
           REAL :: CE,CEP,AFDC1,AFDC2,CWIP,RBCWIP
!        BOOK ITEMS
           REAL :: BOKDP,AFCDP
!        ABANDONMENT ITEMS
          REAL :: DDB,RBDDB,AFDPAJ,AJAFDC,EXEXP,AMORTE,AFCEXP,BKDPTX, &
          	BKDPAJ,WODFTX,AFDC1B,AFDC2B,AFDCDF
          REAL :: GPV_PROPERTY_TAX,NPV_PROPERTY_TAX
      COMMON /FA_WKARRY/ TXDEF(MAX_FINANCIAL_SIMULATION_YEARS), &
      TAXDP(MAX_FINANCIAL_SIMULATION_YEARS), &
             CEPTX(MAX_FINANCIAL_SIMULATION_YEARS), &
             ITCDF(MAX_FINANCIAL_SIMULATION_YEARS), &
             	ITCDP(MAX_FINANCIAL_SIMULATION_YEARS), &
             TAXEXP(MAX_FINANCIAL_SIMULATION_YEARS), &
             	TXDEFC(MAX_FINANCIAL_SIMULATION_YEARS), &
             CE(MAX_FINANCIAL_SIMULATION_YEARS), &
             CEP(MAX_FINANCIAL_SIMULATION_YEARS), &
             AFDC1(MAX_FINANCIAL_SIMULATION_YEARS), &
             AFDC2(MAX_FINANCIAL_SIMULATION_YEARS), &
             CWIP(MAX_FINANCIAL_SIMULATION_YEARS), &
             RBCWIP(MAX_FINANCIAL_SIMULATION_YEARS), &
             BOKDP(MAX_FINANCIAL_SIMULATION_YEARS), &
             AFCDP(MAX_FINANCIAL_SIMULATION_YEARS), &
             DDB(MAX_FINANCIAL_SIMULATION_YEARS), &
             RBDDB(MAX_FINANCIAL_SIMULATION_YEARS), &
             AFDPAJ(MAX_FINANCIAL_SIMULATION_YEARS), &
             AJAFDC(MAX_FINANCIAL_SIMULATION_YEARS), &
             EXEXP(MAX_FINANCIAL_SIMULATION_YEARS), &
             AMORTE(MAX_FINANCIAL_SIMULATION_YEARS), &
             BKDPTX(MAX_FINANCIAL_SIMULATION_YEARS), &
             ITCPAY(MAX_FINANCIAL_SIMULATION_YEARS), &
             AFCEXP(MAX_FINANCIAL_SIMULATION_YEARS), &
             BKDPAJ(MAX_FINANCIAL_SIMULATION_YEARS), &
             WODFTX(MAX_FINANCIAL_SIMULATION_YEARS), &
             AFDC1B(MAX_FINANCIAL_SIMULATION_YEARS), &
             AFDC2B(MAX_FINANCIAL_SIMULATION_YEARS), &
             AFDCDF(MAX_FINANCIAL_SIMULATION_YEARS), &
             GPV_PROPERTY_TAX(MAX_FINANCIAL_SIMULATION_YEARS), &
             NPV_PROPERTY_TAX(MAX_FINANCIAL_SIMULATION_YEARS)
!
      DO I = 2, FINANCIAL_SIMULATION_YEARS
         BOOK_DEP(I) = BOKDP(I) + AFCDP(I)
      ENDDO
      
      DO I = 2, FINANCIAL_SIMULATION_YEARS
         IF((CEP(I)+AFDC2(I)) /= 0.) THEN
            GPV = CEP(I) + AFDC2(I)
            GPV_PROPERTY_TAX(I) = GPV
            NPV = GPV - BOOK_DEP(I)
            NPV_PROPERTY_TAX(I) = NPV
            SL_DEP = FLOAT(13-SERVICEMO) * BOOK_DEP(I)/12.
            IF(I < FINANCIAL_SIMULATION_YEARS) THEN
               IF(CEP(I+1)+AFDC2(I+1) == 0.) THEN
                  SL_DEP = BOOK_DEP(I+1)
               ENDIF
            ENDIF
            BOOK_DEP(I) = 0.
            ESCALATOR = 1.
            DO J = I + 1, FINANCIAL_SIMULATION_YEARS
               ESCALATOR = ESCALATOR * (1. + PROPERTY_ESCALATION(J))
               GPV_PROPERTY_TAX(J) = GPV*ESCALATOR
               DEP = MIN(NPV,SL_DEP,BOOK_DEP(J))
               NPV = MAX(NPV - DEP,0.)
               BOOK_DEP(J) = MAX(BOOK_DEP(J) - DEP,0.)
               NPV_PROPERTY_TAX(J) = NPV*ESCALATOR
            ENDDO
         ENDIF
      ENDDO
      RETURN

!*                                                                     *
!*                           F A A B N D                               *
!*                                                                     *
!*          COPYRIGHT (C) 1982    M.S. GERBER & ASSOCIATES, INC.       *
!*                         ALL RIGHTS RESERVED                         *
!*                                                                     *

!
      ENTRY FAABND(ABYEAR,ABMETH,ABTAX,TAXWO,BOKBL,BOKDAL,BOKWO, &
                   AFCBL,AFCDAL,AFCWO,NRTXWO, &
                   PCAPINRST,TXPREFDEP,ABACCT,ACE_BOOK_DEP, &
                   AFUDC_IN_CWIP, &
                   FINANCIAL_SIMULATION_YEARS)
!
      LAST_YEAR = ABYEAR - get_BASE_YEAR()
      IF (LAST_YEAR >= AVAIL_DATA_YEARS+1 .OR. LAST_YEAR < 1) THEN
         ABMETH = 0
         RETURN
      ENDIF
      IF(ABTAX == 0.) ABMETH = 6
      CCE    = 0.
      CCEP   = 0.
      CTAXDP = 0.
      CBKDPT = 0.
      CBOKDP = 0.
      CAFCDP = 0.
      CAFDC1 = 0.
      CAFDC2 = 0.
      CRBCWP = 0.
      CCEPTX = 0.
      CTXEXP = 0.
      CITCDP = 0.
      CITCDF = 0.
      CAFDCF = 0.
!
!        THEN CUMMULATE THE ACCOUNTS
!
      DO J = 1, LAST_YEAR
         BKDPAJ(J) = 0.
         AFDPAJ(J) = 0.
         CCE    = CCE    + CE(J)
         CCEP   = CCEP   + CEP(J)
         CCEPTX = CCEPTX + CEPTX(J)
         CTAXDP = CTAXDP + TAXDP(J)
         CBKDPT = CBKDPT + BKDPTX(J)
         CBOKDP = CBOKDP + BOKDP(J)
         CAFCDP = CAFCDP + AFCDP(J)
         CAFDC1 = CAFDC1 + AFDC1(J)
         CAFDCF = CAFDCF + AFDCDF(J)
         CAFDC2 = CAFDC2 + AFDC2(J)
         CRBCWP = CRBCWP + RBCWIP(J)
         CTXEXP = CTXEXP + TAXEXP(J)
         CITCDP = CITCDP + ITCDP(J)
         CITCDF = CITCDF + ITCDF(J)
      ENDDO
!
!
!     THEN ABANDONMENT ITEMS ARE COMPUTED
!
      LAST_YEAR = LAST_YEAR + 1
      TEMP1 = CE(LAST_YEAR)
      TEMP2 = AFDC1(LAST_YEAR)
      TEMP3 = AFDC1B(LAST_YEAR)
      DO J = LAST_YEAR, FINANCIAL_SIMULATION_YEARS
         BKDPAJ(J) = 0.
         AFDPAJ(J) = 0.
         CE(J)     = 0.
         CEP(J)    = 0.
         CEPTX(J)  = 0.
         TAXDP(J)  = 0.
         BKDPTX(J) = 0.
         BOKDP(J)  = 0.
         AFCDP(J)  = 0.
         AFDC1(J)  = 0.
         AFDCDF(J)  = 0.
         AFDC1B(J)  = 0.
         AFDC2(J)  = 0.
         CWIP(J)   = 0.
         RBCWIP(J) = 0.
         ITCDF(J)  = 0.
         ITCDP(J)  = 0.
         TAXEXP(J) = 0.
         TXDEF(J)  = 0.
         TXDEFC(J) = 0.
         PCAPINRST(J) = 0.
         TXPREFDEP(J) = 0.
         ACE_BOOK_DEP(J) = 0.
         AFUDC_IN_CWIP(J) = 0.
      ENDDO
!
!     ADJUSTMENTS ARE MADE TO THE PLANT PARAMETERS
!     TO REFLECT THE WRITE-OFF AMOUNTS
!
      CE(LAST_YEAR)     = TEMP1
      CEP(LAST_YEAR)    = -CCEP
      AFDC1(LAST_YEAR)  = TEMP2
      AFDC1B(LAST_YEAR) = TEMP3
      AFDCDF(LAST_YEAR) = -CAFDCF
      AFDC2(LAST_YEAR)  = -CAFDC2
      BKDPAJ(LAST_YEAR) = CBOKDP
      AFDPAJ(LAST_YEAR) = CAFCDP
      ITCPAY(LAST_YEAR) = CITCDP + CITCDF
!
!    WRITE OFF EFFECTS ARE COMPUTED IN NEXT SEVEN LINES:
!
!     TOTAL TAX DEDUCTION OF THE WO
      TAXWO  = CCE + CE(LAST_YEAR) + CWIP(1) - (CTAXDP+CTXEXP)
      IF(ABACCT == 'F') THEN
         TXDEF(LAST_YEAR)  = -TXDEFC(LAST_YEAR-1)
      ELSE
         TXDEF(LAST_YEAR)  = -(1.-ABTAX) * TXDEFC(LAST_YEAR-1)
      ENDIF
!     AMOUNT OF TAX DEDUCTION THAT GOES TO RATE PAYERS

      NRTXWO = TAXWO * ABTAX
!     TOTAL BOOK WO OF BRICKS AND MORTAR
      BOKBL  = (CCE + CE(LAST_YEAR) + CWIP(1) - CBOKDP)
!     BOOK WO BELOW THE LINE
      BOKDAL = BOKBL * (1.-ABTAX)
!     BOOK WO ABOVE THE LINE
      BOKWO  = BOKBL - BOKDAL
!     TOTAL AFUDC WO
      AFCBL  = (CAFDC1 + AFDC1(LAST_YEAR) - CAFCDP)
!     AFUDC WO BELOW THE LINE
      AFCDAL = AFCBL * (1.-ABTAX)
!     AFUDC WO ABOVE THE LINE
      AFCWO  = AFCBL - AFCDAL
      RETURN
      END

!*                                                                     *
!*                        T A X V A L U E                              *
!*                                                                     *
!*          COPYRIGHT (C) 1986 M.S. GERBER & ASSOCIATES, INC.          *
!*                       ALL RIGHTS RESERVED                           *
!*                                                                     *

!*                                                                     *
!*    PURPOSE:                                                         *
!*       TAXVALUE CALCULATES THE TAX VALUE OF THE ASSET.               *
!*                                                                     *

!
      SUBROUTINE TAXVALUE(TAXLF,DEPMET,CEP,CEPTX,CE,ITCDF, &
                          ITCDP,DPBAIS,OHRATE,TAXEXP,PCITC,ITCPAY,CWIP, &
                          CAPINRST,CEPTXBOK,ITC_RATE, &
                          CURRENT_INTEREST_CAP, &
                          FINANCIAL_SIMULATION_YEARS, &
                          TAX_BASIS_ADJUSTOR, &
                          BOOK_EXPEN, &
                          PLANT_2_SERVICE, &
                          MONTHLY_TAX_VALUE_OF_ASSET, &
                          MONTHLY_TAX_EXPENSE, &
                          MONTHLY_CWIP, &
                          MONTHLY_INTEREST_TO_TAX_VALUE, &
                          MONTHLY_CURRENT_INTEREST_CAP)
!
!     INTERNAL VARIABLES
      use mod_base_year
      implicit none
      INTEGER (KIND=2) :: ENDYEAR,STARTYR,FINANCIAL_SIMULATION_YEARS
      INTEGER (KIND=2) :: YR,MO
      REAL :: TAXLF,TAXBAL,OHRATE,TAXAMT,TAX_BASIS_ADJUSTOR
      REAL :: ITCBAL,ITCAMT,CAPINRST(FINANCIAL_SIMULATION_YEARS), &
      CEPTXBOK(FINANCIAL_SIMULATION_YEARS),RTITC,RTITCN
!     EXTERNAL VARIABLES
      CHARACTER (LEN=1) :: DPBAIS
      CHARACTER (LEN=4) :: DEPMET
      REAL :: CEP(FINANCIAL_SIMULATION_YEARS), &
           CEPTX(FINANCIAL_SIMULATION_YEARS), &
           CE(FINANCIAL_SIMULATION_YEARS), &
           ITCDF(FINANCIAL_SIMULATION_YEARS), &
           ITCDP(FINANCIAL_SIMULATION_YEARS), &
           TAXEXP(FINANCIAL_SIMULATION_YEARS),PCITC, &
           ITCPAY(FINANCIAL_SIMULATION_YEARS), &
           CWIP(FINANCIAL_SIMULATION_YEARS),ITC_RATE, &
           CURRENT_INTEREST_CAP(FINANCIAL_SIMULATION_YEARS) 
!     DECLARATION FOR WORLD VARIABLES FOR FUTURE ASSETS
      INTEGER (KIND=2) :: BASEYEAR
!      
      REAL (KIND=4) :: BOOK_EXPEN(0:12,0:FINANCIAL_SIMULATION_YEARS), &
             PLANT_2_SERVICE(0:12,0:FINANCIAL_SIMULATION_YEARS), &
             MONTHLY_TAX_VALUE_OF_ASSET(0:12,0:FINANCIAL_SIMULATION_YEARS), &
             MONTHLY_TAX_EXPENSE(0:12,0:FINANCIAL_SIMULATION_YEARS), &
             MONTHLY_CWIP(0:12,0:FINANCIAL_SIMULATION_YEARS), &
             MONTHLY_INTEREST_TO_TAX_VALUE(0:12,0:FINANCIAL_SIMULATION_YEARS), &
             MONTHLY_CURRENT_INTEREST_CAP(0:12,0:FINANCIAL_SIMULATION_YEARS)
!
      BASEYEAR = get_BASE_YEAR()
      TAXBAL = TAXEXP(1)
      CEPTX(1:FINANCIAL_SIMULATION_YEARS) = 0.
      ITCDF(1:FINANCIAL_SIMULATION_YEARS) = 0.
      ITCDP(1:FINANCIAL_SIMULATION_YEARS) = 0.
      TAXEXP(1:FINANCIAL_SIMULATION_YEARS) = 0.
      ITCPAY(1:FINANCIAL_SIMULATION_YEARS) = 0.
      CEPTXBOK(1:FINANCIAL_SIMULATION_YEARS) = 0.
      TAXEXP(1) = TAXBAL
      STARTYR = 2
      RTITC = 0
      IF(TAXLF >= 3. .AND. (DEPMET == 'ACRS' .OR. DEPMET == 'MACR')) &
      	RTITC = ITC_RATE * PCITC
      IF(RTITC > 0.) THEN
         ITCDP(1) = RTITC * (CWIP(1) - TAXEXP(1))
         ITCBAL = ITCDP(1)/2.
         IF(DPBAIS == 'F') ITCBAL = 0.
         DO YR = STARTYR, FINANCIAL_SIMULATION_YEARS
            TAXEXP(YR) = OHRATE * CE(YR)
            TAXBAL    = TAXBAL + TAXEXP(YR)
            ITCDP(YR)  = RTITC * (CE(YR) - TAXEXP(YR))
            IF(BASEYEAR+YR-1 > 1987)ITCBAL = ITCBAL + .650*ITCDP(YR) 
            DO MO = 0, 12 
               MONTHLY_TAX_EXPENSE(MO,YR-1) = OHRATE*BOOK_EXPEN(MO,YR-1)
!
            ENDDO
            IF(CWIP(YR-1) + CE(YR) /= 0. .AND. CEP(YR) /= 0.) THEN
               TAXAMT = CEP(YR)/(CWIP(YR-1) + CE(YR)) * TAXBAL
               ITCAMT = CEP(YR) / (CWIP(YR-1) + CE(YR)) * ITCBAL
               CEPTX(YR)  = TAX_BASIS_ADJUSTOR * (CEP(YR)-TAXAMT-ITCAMT) + &
               	CAPINRST(YR)
               CEPTXBOK(YR) = TAX_BASIS_ADJUSTOR * (CEP(YR)- ITCAMT) + &
               	CURRENT_INTEREST_CAP(YR)
!
               TAXBAL = TAXBAL - TAXAMT
               ITCBAL = ITCBAL - ITCAMT
               DO MO = 0, 12 
                  IF(PLANT_2_SERVICE(MO,YR-1) /= 0.) THEN
                     MONTHLY_TAX_VALUE_OF_ASSET(MO,YR-1) = &
                     	CEPTX(YR)/CEP(YR)* PLANT_2_SERVICE(MO,YR-1)
                  ENDIF
               ENDDO
            ENDIF
         ENDDO
      ELSE
      DO YR = STARTYR, FINANCIAL_SIMULATION_YEARS
         TAXEXP(YR) = OHRATE * CE(YR)
         TAXBAL    = TAXBAL + TAXEXP(YR)
         DO MO = 0, 12 
            MONTHLY_TAX_EXPENSE(MO,YR-1) = OHRATE * BOOK_EXPEN(MO,YR-1)
         ENDDO
         IF(CWIP(YR-1) + CE(YR) /= 0. .AND. CEP(YR) /= 0.) THEN
            TAXAMT = CEP(YR)/(CWIP(YR-1) + CE(YR)) * TAXBAL
            CEPTX(YR)  = TAX_BASIS_ADJUSTOR * &
            	(CEP(YR) -  TAXAMT) + CAPINRST(YR)
            CEPTXBOK(YR) = TAX_BASIS_ADJUSTOR * &
            	CEP(YR) + CURRENT_INTEREST_CAP(YR)
!
            TAXBAL    = TAXBAL - TAXAMT
            DO MO = 0, 12 
               IF(PLANT_2_SERVICE(MO,YR-1) /= 0.) THEN
                  MONTHLY_TAX_VALUE_OF_ASSET(MO,YR-1)=&
                  CEPTX(YR)/CEP(YR) * PLANT_2_SERVICE(MO,YR-1)
               ENDIF
            ENDDO
         ENDIF
      ENDDO
      ENDIF ! ITC ACTIVE
      RETURN
      END

!*                                                                     *
!*                          D E P T A X                                *
!*                                                                     *
!*          COPYRIGHT (C) 1982 M.S. GERBER & ASSOCIATES, INC.          *
!*                       ALL RIGHTS RESERVED                           *
!*                                                                     *

!*                                                                     *
!*    PURPOSE:                                                         *
!*       DEPTAX CALCULATES THE ANNUAL DEPRECIATION FOR FEDERAL TAXES   *
!*       BASED ON THE ACCELERATED COST RECOVERY SYSTEM (ACRS) AS       *
!*       SPECIFIED IN THE ECONOMIC RECOVERY TAX ACT (ERTA) OF 1981, OR *
!*       THE ASSET DEPRECIATION RANGE (ADR), OR STRAIGHT LINE.         *
!*       THE MEHTOD USED IS SPECIFIED FOR EACH ACCOUNT                 *
!*                                                                     *

!
      SUBROUTINE DEPTAX(TAXLF,DEPMET,R_DBRATE,TAXDP,CEPTX_TEMP, &
                        TAX_VALUE_BEGINNING_90, &
                        FINANCIAL_SIMULATION_YEARS, &
                        TAKE_BONUS_TAX_DEPRECIATION, &
                        IN_SERVICE_MONTH, &
                        R_MONTHLY_TAX_VALUE_OF_ASSET, &
                        MONTHLY_TAX_DEPRECIATION, &
                        BONUS_DEP_MONTHLY_2001)
!
!     INTERNAL VARIABLES
      use spindriftlib
      use prod_arrays_dimensions
      use mod_base_year
      
      implicit none
      
      LOGICAL (KIND=1) :: NO_SWITCH,DUMP_AT_TAX_LIFE
      INTEGER (KIND=2) :: YREND,SUMYD,J,L,LIFECK,LIFE,IYEAR,IYR,M
      INTEGER (KIND=2) :: FINANCIAL_SIMULATION_YEARS
      REAL :: TAXLF,DBRATE,TAXDEP,TAXDPC,GPVF,DEPDB,DEPSL,R_DBRATE
      REAL :: DEPPCT(20,6),SL_OFFSET
      REAL :: NPVFT,BONUS_DEP_4_MO,TAX_VALUE_4_MO
!     EXTERNAL VARIABLES
      CHARACTER (LEN=4) :: DEPMET
      REAL :: TAXDP(FINANCIAL_SIMULATION_YEARS), &
           CEPTX(105), &
           BONUS_TAX_DEP(105), &
           CEPTX_TEMP(FINANCIAL_SIMULATION_YEARS), &
           TAX_VALUE_BEGINNING_90(FINANCIAL_SIMULATION_YEARS)
      REAL :: DEP_AMOUNT
      INTEGER (KIND=2) :: BASEYEAR,IN_SERVICE_MONTH,MO
      CHARACTER (LEN=1) :: TAKE_BONUS_TAX_DEPRECIATION
      REAL (kind=4) :: &
        R_MONTHLY_TAX_VALUE_OF_ASSET(0:12,0:FINANCIAL_SIMULATION_YEARS), &
             MONTHLY_TAX_DEPRECIATION(0:12,0:FINANCIAL_SIMULATION_YEARS)
      REAL (KIND=4) :: MONTHLY_TAX_VALUE_OF_ASSET(0:12,0:101),BONUS_DEP_2001
      REAL (KIND=4) :: &
        BONUS_DEP_MONTHLY_2001(0:12,0:FINANCIAL_SIMULATION_YEARS)
!
      DATA DEPPCT/.25,.38,.37,17*0. &
      	,.15,.22,3*.21,15*0.,.08,.14, &
      	.12,3*.10,4*.09,10*0.,.05,.10, &
      	.09,.08,2*.07,9*.06,5*0.,20*0.,20*0./
!
      BASEYEAR = get_BASE_YEAR()
      DO J = 1, FINANCIAL_SIMULATION_YEARS
         TAXDP(J) = 0.
         CEPTX(J) = CEPTX_TEMP(J)
         TAX_VALUE_BEGINNING_90(J) = CEPTX(J)
         DO MO = 0, 12
            MONTHLY_TAX_VALUE_OF_ASSET(MO,J-1) = &
            	R_MONTHLY_TAX_VALUE_OF_ASSET(MO,J-1) 
         ENDDO
      ENDDO
!
      DO L = 1, MAX(1,1989-BASEYEAR)
         TAX_VALUE_BEGINNING_90(L) = 0.
      ENDDO
!
!     IF THE TAX LIFE IS GREATER THAN 98 YEARS, IT IS ASSUMED
!     THAT NO TAX DEPRECIATION WILL OCURR
!
      IF( TAXLF > 98.0 ) RETURN
      IF(TAXLF <= 1.0) THEN
           DO L = 2, FINANCIAL_SIMULATION_YEARS
            TAXDP(L) = CEPTX(L)
            IF(BASEYEAR+L-1 <= 1989) TAX_VALUE_BEGINNING_90(L) = 0.
            DO MO = 0, 12 
               MONTHLY_TAX_DEPRECIATION(MO,L-1) = &
               	MONTHLY_TAX_VALUE_OF_ASSET(MO,L-1)
              
            ENDDO
         ENDDO
         RETURN
       ENDIF
!
      BONUS_DEP_MONTHLY_2001 = 0.
      IF(TAKE_BONUS_TAX_DEPRECIATION == 'Y') THEN
         DO J = 2, FINANCIAL_SIMULATION_YEARS
!
            BONUS_TAX_DEP(J) = 0.
            IYEAR = BASEYEAR + J - 1
            IF(IYEAR > 2012) EXIT
            IF(IYEAR < 2001) CYCLE
! 
            IF(IYEAR == 2001 .AND. CEPTX(J) == 0.) CYCLE
            IF(IYEAR == 2001) THEN
               BONUS_DEP_2001 = 0.
               DO MO = 9, 12 
                  IF(MONTHLY_TAX_VALUE_OF_ASSET(MO,J-1) /= 0.) THEN
                     
                     BONUS_DEP_2001 = BONUS_DEP_2001 + &
                     	 .30 * MONTHLY_TAX_VALUE_OF_ASSET(MO,J-1)
                     BONUS_DEP_MONTHLY_2001(MO,J-1) = + &
                     	 .30 * MONTHLY_TAX_VALUE_OF_ASSET(MO,J-1)
                     MONTHLY_TAX_VALUE_OF_ASSET(MO,J-1) = &
                     	0.70 * MONTHLY_TAX_VALUE_OF_ASSET(MO,J-1)
                  ENDIF
               ENDDO
               BONUS_TAX_DEP(J) = BONUS_DEP_2001
               BONUS_DEP_MONTHLY_2001(0,J-1) = BONUS_DEP_2001 
               CEPTX(J) = CEPTX(J) - BONUS_TAX_DEP(J)  
            ELSEIF(CEPTX(J) /= 0.) THEN
               IF(IYEAR == 2002) THEN 
                  BONUS_TAX_DEP(J) = .30 * CEPTX(J)
                  BONUS_DEP_MONTHLY_2001(0,J-1) = BONUS_TAX_DEP(J) 
                  CEPTX(J) = CEPTX(J) - BONUS_TAX_DEP(J)  
                  BONUS_DEP_2001 = 0.
                  DO MO = 0, 12 
                     IF(MONTHLY_TAX_VALUE_OF_ASSET(MO,J-1) /= 0.) THEN
                        BONUS_DEP_MONTHLY_2001(MO,J-1) =  &
                        	+ .30 * MONTHLY_TAX_VALUE_OF_ASSET(MO,J-1)
                        MONTHLY_TAX_VALUE_OF_ASSET(MO,J-1) &
                        	= 0.70 * MONTHLY_TAX_VALUE_OF_ASSET(MO,J-1)
                     ENDIF
                  ENDDO
               ELSEIF(IYEAR == 2003) THEN
                  BONUS_TAX_DEP(J) = 0.
                  BONUS_DEP_2001 = 0.
                  DO MO = 1, 12 
                     IF(MONTHLY_TAX_VALUE_OF_ASSET(MO,J-1) /= 0.) THEN
                        IF(MO < 5) THEN
                           BONUS_DEP_4_MO = &
                           	.30 * MONTHLY_TAX_VALUE_OF_ASSET(MO,J-1)
                           TAX_VALUE_4_MO = &
                           	0.70 * MONTHLY_TAX_VALUE_OF_ASSET(MO,J-1)
                        ELSE
                           BONUS_DEP_4_MO = &
                           	.50 * MONTHLY_TAX_VALUE_OF_ASSET(MO,J-1)
                           TAX_VALUE_4_MO = &
                           	0.50 * MONTHLY_TAX_VALUE_OF_ASSET(MO,J-1)
                        ENDIF
                        BONUS_DEP_MONTHLY_2001(MO,J-1) = BONUS_DEP_4_MO &
                        	 + BONUS_DEP_MONTHLY_2001(MO,J-1)
                        BONUS_TAX_DEP(J) = BONUS_TAX_DEP(J) &
                        	 + BONUS_DEP_4_MO
                        MONTHLY_TAX_VALUE_OF_ASSET(MO,J-1) = &
                        	TAX_VALUE_4_MO + MONTHLY_TAX_VALUE_OF_ASSET(MO,J-1)
                     ENDIF
                  ENDDO
                  BONUS_DEP_MONTHLY_2001(0,J-1) = BONUS_TAX_DEP(J) 
                  CEPTX(J) = CEPTX(J) - BONUS_TAX_DEP(J)  
                  MONTHLY_TAX_VALUE_OF_ASSET(0,J-1) = CEPTX(J)
               ELSEIF(IYEAR < 2010 .OR. IYEAR == 2012) THEN
                  BONUS_TAX_DEP(J) = .50 * CEPTX(J)
                  BONUS_DEP_MONTHLY_2001(0,J-1) = BONUS_TAX_DEP(J) 
                  CEPTX(J) = CEPTX(J) - BONUS_TAX_DEP(J)  
                  BONUS_DEP_2001 = 0.
                  DO MO = 0, 12 
                     IF(MONTHLY_TAX_VALUE_OF_ASSET(MO,J-1) /= 0.) THEN
                        BONUS_DEP_MONTHLY_2001(MO,J-1) = &
                        	 + .50 * MONTHLY_TAX_VALUE_OF_ASSET(MO,J-1)
                        MONTHLY_TAX_VALUE_OF_ASSET(MO,J-1) &
                        	 = 0.50 * MONTHLY_TAX_VALUE_OF_ASSET(MO,J-1)
                     ENDIF
                  ENDDO
               ELSEIF(IYEAR == 2010 .OR. IYEAR == 2011) THEN
                  IF(IYEAR == 2010 .AND. IN_SERVICE_MONTH < 9) THEN
                     BONUS_TAX_DEP(J) = .50 * CEPTX(J)
                     BONUS_DEP_MONTHLY_2001(0,J-1) = BONUS_TAX_DEP(J) 
                     CEPTX(J) = CEPTX(J) - BONUS_TAX_DEP(J)  
                     BONUS_DEP_2001 = 0.
                     DO MO = 0, 12 
                        IF(MONTHLY_TAX_VALUE_OF_ASSET(MO,J-1) /= 0.)THEN
                           BONUS_DEP_MONTHLY_2001(MO,J-1) = &
                           	 + .50 * MONTHLY_TAX_VALUE_OF_ASSET(MO,J-1)
                           MONTHLY_TAX_VALUE_OF_ASSET(MO,J-1) = &
                           	0.50 * MONTHLY_TAX_VALUE_OF_ASSET(MO,J-1)
                        ENDIF
                     ENDDO
                  ELSE
                     BONUS_TAX_DEP(J) = 1.00 * CEPTX(J)
                     CEPTX(J) = 0.
                     BONUS_DEP_2001 = 0.
                     BONUS_DEP_MONTHLY_2001(0,J-1) = BONUS_TAX_DEP(J) 
                     MONTHLY_TAX_VALUE_OF_ASSET(:,J-1) = 0.
                     BONUS_DEP_MONTHLY_2001(IN_SERVICE_MONTH,J-1) = &
                     	BONUS_TAX_DEP(J)
                        
                  ENDIF
               ENDIF 
            ENDIF
         ENDDO
      ENDIF
!

      DBRATE = R_DBRATE
!
! 
      IF(INDEX(DEPMET,'ACRS') /= 0) THEN
         LIFECK = INT(TAXLF + 0.001)
         LIFE = LIFECK/5 + 1
         IF(LIFE > 4) LIFE = 4
         IF(LIFECK > 15) LIFECK = 15
!
         DO J = 2, FINANCIAL_SIMULATION_YEARS
!
            IF(CEPTX(J) /= 0.) THEN   
               IYEAR = BASEYEAR + J - 1
               IF(IYEAR < 1981) THEN
                  DBRATE = 2.
                  DEPMET = 'DB  '
                  EXIT ! GOTO 40
               ENDIF
!
               YREND = J + LIFECK - 1
               IF(YREND > FINANCIAL_SIMULATION_YEARS) then
                YREND = FINANCIAL_SIMULATION_YEARS
               end if
               NPVFT = CEPTX(J)
               DO L = J, YREND
                  IYR = L - J + 1
                  TAXDEP = CEPTX(J) * DEPPCT(IYR,LIFE)
                  TAXDP(L) = TAXDP(L) + TAXDEP
                  CALL DISTRIBUTE_MONTHLY_TAX_DEP(TAXDEP,J,L, &
                        MONTHLY_TAX_VALUE_OF_ASSET, &
                        MONTHLY_TAX_DEPRECIATION)
                  NPVFT = NPVFT - TAXDEP
                  IF((BASEYEAR + L - 1) == 1989) THEN
                     TAX_VALUE_BEGINNING_90(J) = NPVFT
                  ENDIF
               ENDDO
            ENDIF
         ENDDO
! 
      ENDIF
!
      IF(INDEX(DEPMET,'DB') /= 0 .OR. INDEX(DEPMET,'ADR') /= 0 .OR. &
               INDEX(DEPMET,'DBNS') /= 0 .OR. &
               INDEX(DEPMET,'DBND') /= 0 .OR. &
                                       INDEX(DEPMET,'MACR') /= 0) THEN
         SL_OFFSET = 0.
         IF(INDEX(DEPMET,'MACR') /= 0) THEN
            SL_OFFSET = .5
            DBRATE = 1.5
         ENDIF
         NO_SWITCH = INDEX(DEPMET,'DBNS') /= 0 .OR. INDEX(DEPMET,'DBND') /= 0
         DUMP_AT_TAX_LIFE = INDEX(DEPMET,'DBND') /= 0
         DO J = 2, FINANCIAL_SIMULATION_YEARS
            IF(CEPTX(J) /= 0.) THEN
               NPVFT = CEPTX(J) / 2.
               TAXDPC = 0.
               GPVF = CEPTX(J)
               DO L = J, FINANCIAL_SIMULATION_YEARS
                  DEPDB = DBRATE * NPVFT/TAXLF
                  IF(TAXLF - FLOAT(L - J) + SL_OFFSET > 0) THEN
                     DEPSL = NPVFT/(TAXLF - FLOAT(L - J) + SL_OFFSET)
                  ELSEIF(DUMP_AT_TAX_LIFE) THEN
                     TAXDP(L) = TAXDP(L) + NPVFT
                     CALL DISTRIBUTE_MONTHLY_TAX_DEP(NPVFT,J,L, &
                                             MONTHLY_TAX_VALUE_OF_ASSET, &
                                             MONTHLY_TAX_DEPRECIATION)
                     EXIT
                  ELSE
                     DEPSL = DEPDB
                  ENDIF
                  IF(NO_SWITCH .OR. ABS(DEPDB) >= ABS(DEPSL)) THEN
!                    THEN TAKE THE DB VALUE FOR THE EXPENSE
                     TAXDP(L) = TAXDP(L) + DEPDB
                     TAXDPC =  TAXDPC + DEPDB
                     NPVFT = GPVF - TAXDPC
                     IF((BASEYEAR + L - 1) == 1989) THEN
                        TAX_VALUE_BEGINNING_90(J) = NPVFT
                     ENDIF
                     CALL DISTRIBUTE_MONTHLY_TAX_DEP(DEPDB,J,L, &
                                             MONTHLY_TAX_VALUE_OF_ASSET, &
                                             MONTHLY_TAX_DEPRECIATION)
!                 ELSE TAKE THE STRAIGHT LINE UNTIL NPV IS ZERO
                  ELSE
                     DO M = L, FINANCIAL_SIMULATION_YEARS
                        IF(ABS(NPVFT) < ABS(DEPSL)) THEN
                           TAXDP(M) = TAXDP(M) + NPVFT
                           CALL DISTRIBUTE_MONTHLY_TAX_DEP(NPVFT,J,M, &
                                             MONTHLY_TAX_VALUE_OF_ASSET, &
                                             MONTHLY_TAX_DEPRECIATION)
                           EXIT !GOTO 85
                        ENDIF
                        TAXDP(M) = TAXDP(M) + DEPSL
                        NPVFT = NPVFT - DEPSL
                        IF(ABS(NPVFT) < .0001) NPVFT = 0.
                        IF((BASEYEAR + M - 1) == 1989) THEN
                           TAX_VALUE_BEGINNING_90(J) = NPVFT
                        ENDIF
                        CALL DISTRIBUTE_MONTHLY_TAX_DEP(DEPSL,J,M, &
                                             MONTHLY_TAX_VALUE_OF_ASSET, &
                                             MONTHLY_TAX_DEPRECIATION)
                     ENDDO
                     EXIT ! GOTO 85
                  ENDIF
               ENDDO
            ENDIF
         ENDDO !85    CONTINUE
! 
      ENDIF
!
! 
      IF(INDEX(DEPMET,'SL') /= 0) THEN
         DO J = 2, FINANCIAL_SIMULATION_YEARS
            IF(CEPTX(J) /= 0.) THEN
               DEPSL = CEPTX(J) / TAXLF
               TAXDP(J) = DEPSL/2. + TAXDP(J) 
               CALL DISTRIBUTE_MONTHLY_TAX_DEP(TAXDP(J),J,J, &
                                             MONTHLY_TAX_VALUE_OF_ASSET, &
                                             MONTHLY_TAX_DEPRECIATION)
               NPVFT = CEPTX(J) - TAXDP(J)
               M = J + 1
               IF (M > FINANCIAL_SIMULATION_YEARS) CYCLE
               DO L = M, FINANCIAL_SIMULATION_YEARS
                  IF((BASEYEAR + L) == 1989) THEN
                     TAX_VALUE_BEGINNING_90(J) = NPVFT
                  ENDIF
                  IF(NPVFT < DEPSL) THEN
                     TAXDP(L) = TAXDP(L) + NPVFT
                     CALL DISTRIBUTE_MONTHLY_TAX_DEP(NPVFT,J,L, &
                                             MONTHLY_TAX_VALUE_OF_ASSET, &
                                             MONTHLY_TAX_DEPRECIATION)
                     EXIT
                  ELSE
                     TAXDP(L) = TAXDP(L) + DEPSL
                     CALL DISTRIBUTE_MONTHLY_TAX_DEP(DEPSL,J,L, &
                                             MONTHLY_TAX_VALUE_OF_ASSET, &
                                             MONTHLY_TAX_DEPRECIATION)
                     NPVFT = NPVFT - DEPSL
                     IF(NPVFT < .0001) NPVFT = 0.
                  ENDIF
               ENDDO
            ENDIF    
         ENDDO
! 
      ENDIF
!
!     DEP METH IS SYD
!
! 
      IF(INDEX(DEPMET,'SYD') /= 0) THEN
         LIFECK = INT(TAXLF + 0.001)
         SUMYD = LIFECK * (LIFECK + 1) / 2
         DO J = 2, FINANCIAL_SIMULATION_YEARS
            IF(CEPTX(J) /= 0.) THEN
               NPVFT = CEPTX(J)
               YREND = J + LIFECK - 1
               IF(YREND > FINANCIAL_SIMULATION_YEARS) &
               	YREND = FINANCIAL_SIMULATION_YEARS
               DO L = J, YREND
                  IYR = TAXLF  - (L - J)
                  DEP_AMOUNT =  CEPTX(J)*FLOAT(IYR)/FLOAT(SUMYD)
                  TAXDP(L) = TAXDP(L) + DEP_AMOUNT
                  CALL DISTRIBUTE_MONTHLY_TAX_DEP(DEP_AMOUNT,J,L, &
                                             MONTHLY_TAX_VALUE_OF_ASSET, &
                                             MONTHLY_TAX_DEPRECIATION)
                  NPVFT = NPVFT - DEP_AMOUNT
                  IF(NPVFT < .0001) NPVFT = 0.
                  IF((BASEYEAR + L - 1) == 1989) THEN
                     TAX_VALUE_BEGINNING_90(J) = NPVFT
                  ENDIF
               ENDDO
            ENDIF
         ENDDO
      ENDIF
!
! DISTRIBUTE THE ANNUAL TAX DEP ACROSS MONTHLYS
!      
      IF(TAKE_BONUS_TAX_DEPRECIATION == 'Y') THEN
         DO J = 2, FINANCIAL_SIMULATION_YEARS
!
            IYEAR = BASEYEAR + J - 1
            IF(IYEAR < 2001) CYCLE
            IF(IYEAR > 2012) EXIT
            IF(BONUS_TAX_DEP(J) /= 0.) THEN 
               TAXDP(J) = TAXDP(J) + BONUS_TAX_DEP(J)
               DO MO = 0, 12 
                  MONTHLY_TAX_DEPRECIATION(MO,J-1) = &
                                        MONTHLY_TAX_DEPRECIATION(MO,J-1) &
                                        + BONUS_DEP_MONTHLY_2001(MO,J-1)
               ENDDO
            ENDIF
         ENDDO
      ENDIF
      RETURN
      END

!*                                                                     *
!*                          D E F T A X                                *
!*                                                                     *
!*          COPYRIGHT (C) 1982 M.S. GERBER & ASSOCIATES, INC.          *
!*                       ALL RIGHTS RESERVED                           *
!*                                                                     *

!*                                                                     *
!*    PURPOSE:                                                         *
!*       DEFTAX CALCULATES THE DEFFERRED TAX ITEMS                     *
!*                                                                     *

!
      SUBROUTINE DEFTAX(TAXDP,TAXEXP,BKDPTX,TXDEF,TXDEFC,PCAPINRST, &
                        CURRENT_INTEREST_CAP_RATE, &
                        FINANCIAL_SIMULATION_YEARS)
!
      use SpinDriftLib
      USE PROD_ARRAYS_DIMENSIONS
      USE SIZECOM
      INTEGER (KIND=2) :: I,FINANCIAL_SIMULATION_YEARS
!     EXTERNAL VARIABLES
      REAL :: TAXDP(FINANCIAL_SIMULATION_YEARS), &
              TAXEXP(FINANCIAL_SIMULATION_YEARS), &
              BKDPTX(FINANCIAL_SIMULATION_YEARS), &
              TXDEF(FINANCIAL_SIMULATION_YEARS), &
              TXDEFC(FINANCIAL_SIMULATION_YEARS), &
              PCAPINRST(FINANCIAL_SIMULATION_YEARS), &
              CURRENT_INTEREST_CAP_RATE(FINANCIAL_SIMULATION_YEARS)
!
!     CALCULATE DEFERRED TAXES
!
      TXDEF(1) = TAXDP(1) - BKDPTX(1) + TAXEXP(1) - PCAPINRST(1) * &
      	 (1.-CURRENT_INTEREST_CAP_RATE(1))
      IF(ABS(TXDEF(1)) < 0.00001) TXDEF(1) = 0.
      TXDEFC(1) = TXDEF(1)
      DO I = 2, FINANCIAL_SIMULATION_YEARS
         TXDEF(I) = TAXDP(I) - BKDPTX(I) + TAXEXP(I) - PCAPINRST(I) * &
         	(1.-CURRENT_INTEREST_CAP_RATE(I))
         IF(ABS(TXDEF(I)) < 0.00001) TXDEF(I) = 0.
!
         TXDEFC(I) = TXDEFC(I-1) + TXDEF(I)
         IF(ABS(TXDEFC(I)) < 0.00001) TXDEFC(I) = 0.
      ENDDO
      RETURN
      END

!*                                                                     *
!*                           DEP_ACE                                   *
!*                                                                     *
!*          COPYRIGHT (C) 1990   M.S. GERBER & ASSOCIATES, INC         *
!*                       ALL RIGHTS RESERVED                           *
!*  TAKEN FROM FIN ON JUNE 5, 1991 BY M. GERBER FOR USE IN MIDAS GOLD  *

!                                                                      *
!     PURPOSE:                                                         *
!        CALCULATES THE ACE DEPRECIATION FOR FUTURE ASSETS             *
!                                                                      *

!
      SUBROUTINE ACE_DEP(ADRLIFE,ACE_BOOK_DEP,TAX_VALUE_BEGINNING_90, &
                         FINANCIAL_SIMULATION_YEARS, &
                         MONTHLY_TAX_VALUE_OF_ASSET, &
                         MONTHLY_ACE_BOOK_DEP)
!
      use SpinDriftLib
      USE PROD_ARRAYS_DIMENSIONS
      USE SIZECOM
!
      INTEGER (KIND=2) :: J,L,FINANCIAL_SIMULATION_YEARS
      REAL ADRLIFE,ACE_BOOK_DEP(FINANCIAL_SIMULATION_YEARS), &
           TAX_VALUE_BEGINNING_90(FINANCIAL_SIMULATION_YEARS), &
           REMAINING_LIFE,DEPSL,FIRST_YR_DEP,NPVFB, &
           MONTHLY_TAX_VALUE_OF_ASSET(0:12,0:*), &
           MONTHLY_ACE_BOOK_DEP(0:12,0:*)
!
      ACE_BOOK_DEP = 0.
      IF(ADRLIFE <= 1) THEN
         DO J = 2, FINANCIAL_SIMULATION_YEARS
            ACE_BOOK_DEP(J) = TAX_VALUE_BEGINNING_90(J)
            CALL DISTRIBUTE_MONTHLY_TAX_DEP(ACE_BOOK_DEP(J),J,J, &
                                            MONTHLY_TAX_VALUE_OF_ASSET, &
                                            MONTHLY_ACE_BOOK_DEP)
         ENDDO
      ELSE
!
! DEP ON REMAINING LIFE 
!
         DO J = 2, FINANCIAL_SIMULATION_YEARS
            IF(TAX_VALUE_BEGINNING_90(J) /= 0.) THEN
               REMAINING_LIFE = ADRLIFE
               DEPSL = TAX_VALUE_BEGINNING_90(J)/REMAINING_LIFE
               FIRST_YR_DEP = DEPSL/2.
               ACE_BOOK_DEP(J) = ACE_BOOK_DEP(J) + FIRST_YR_DEP
               NPVFB =  TAX_VALUE_BEGINNING_90(J) - FIRST_YR_DEP
               CALL DISTRIBUTE_MONTHLY_TAX_DEP(FIRST_YR_DEP,J,J, &
                                             MONTHLY_TAX_VALUE_OF_ASSET, &
                                             MONTHLY_ACE_BOOK_DEP)
               DO L = J+1, FINANCIAL_SIMULATION_YEARS
                  IF(ABS(NPVFB) < ABS(DEPSL)) THEN
                     ACE_BOOK_DEP(L) = ACE_BOOK_DEP(L) + NPVFB
                     CALL DISTRIBUTE_MONTHLY_TAX_DEP(NPVFB,J,L, &
                                             MONTHLY_TAX_VALUE_OF_ASSET, &
                                             MONTHLY_ACE_BOOK_DEP)
                     EXIT
                  ELSE
                     ACE_BOOK_DEP(L) = ACE_BOOK_DEP(L) + DEPSL
                     CALL DISTRIBUTE_MONTHLY_TAX_DEP(DEPSL,J,L, &
                                             MONTHLY_TAX_VALUE_OF_ASSET, &
                                             MONTHLY_ACE_BOOK_DEP)
                  ENDIF
                  NPVFB = NPVFB - DEPSL
               ENDDO
            ENDIF
         ENDDO
      ENDIF
      RETURN
      END

!*                                                                     *
!*                        CCA_TAX_DEP                                  *
!*                                                                     *
!*          COPYRIGHT (C) 1992 M.S. GERBER & ASSOCIATES, INC.          *
!*                       ALL RIGHTS RESERVED                           *
!*                                                                     *

!*                                                                     *
!*    PURPOSE:                                                         *
!*       CALCULATES THE ANNAUL TAX VALUE OF THE ASSET AND CANADIAN     *
!*       CAPITAL COST ALLOCATION, CCA                                  *
!*                                                                     *

!
      SUBROUTINE CCA_TAX_DEP(TAXLF,CEPTX,CE,OHRATE,TAXEXP, &
                  PCAPINRST,DBRATE,TAXDP,BEGINNING_INTEREST_CAP,FIRSTYR, &
                  CEP,DEPMET,FINANCIAL_SIMULATION_YEARS)
!
      use SpinDriftLib
      use prod_arrays_dimensions
      use mod_base_year
      use sizecom
      use globecom

!
!     INTERNAL VARIABLES
      CHARACTER (len=4) :: DEPMET
      INTEGER (KIND=2) :: FIRSTYR,FINANCIAL_SIMULATION_YEARS
      INTEGER (KIND=2) :: I
      REAL :: TAXLF,OHRATE,DBRATE
      REAL :: PCAPINRST(FINANCIAL_SIMULATION_YEARS), &
      	BEGINNING_INTEREST_CAP,DEP_RATE
!     EXTERNAL VARIABLES
      REAL :: CEPTX(FINANCIAL_SIMULATION_YEARS), &
           CE(FINANCIAL_SIMULATION_YEARS), &
           TAXEXP(FINANCIAL_SIMULATION_YEARS), &
           TAXDP(FINANCIAL_SIMULATION_YEARS), &
           CEP(FINANCIAL_SIMULATION_YEARS)
      REAL :: CE_TAX(MAX_FINANCIAL_SIMULATION_YEARS), &
           ROLLING_START(MAX_FINANCIAL_SIMULATION_YEARS), &
           LONG_TERM_PROJECT(MAX_FINANCIAL_SIMULATION_YEARS)
!      
      REAL :: TOTAL_ROLLING_START,LTP_TEST,RS_TEST, &
      	TOTAL_LONG_TERM_PROJECT,TOTAL_CASH_INTEREST
      INTEGER (KIND=2) :: YR
!
      PCAPINRST(1) = BEGINNING_INTEREST_CAP
      ROLLING_START(1) = 0.
      LONG_TERM_PROJECT(1) = 0.
      DO I = 2, FINANCIAL_SIMULATION_YEARS
         ROLLING_START(I) = 0.
         LONG_TERM_PROJECT(I) = 0.
         TAXEXP(I) = CE(I) * OHRATE
         CE_TAX(I) = CE(I) - TAXEXP(I)
      ENDDO
      TOTAL_CASH_INTEREST = CE_TAX(1) + PCAPINRST(1)
      I = 2
      TOTAL_ROLLING_START = 0.
      DO WHILE ((CEP(I) == 0.) .AND. (I <= FINANCIAL_SIMULATION_YEARS))
         YR = get_BASE_YEAR() + I - 1 - FIRSTYR
         IF(YR >= 2) THEN
            IF(YR >= 4) THEN
               ROLLING_START(I) = MAX(0., CE_TAX(I-2) + PCAPINRST(I-2) - &
               	CE_TAX(I-4) + PCAPINRST(I-4))
            ELSEIF (YR >= 2) THEN
               ROLLING_START(I) = MAX(0., CE_TAX(I-2) + PCAPINRST(I-2))
            ELSE
               ROLLING_START(I) = 0.
            ENDIF
            TOTAL_ROLLING_START = TOTAL_ROLLING_START + ROLLING_START(I)
         ELSE
            ROLLING_START(I) = 0.
         ENDIF
         TOTAL_CASH_INTEREST = TOTAL_CASH_INTEREST + CE_TAX(I) + PCAPINRST(I)
         I = I + 1
      ENDDO
      IF(I <= FINANCIAL_SIMULATION_YEARS) THEN
         ROLLING_START(I) = TOTAL_CASH_INTEREST + CE_TAX(I) + &
         	PCAPINRST(I) - TOTAL_ROLLING_START
         I = I + 1
         DO WHILE (I <= FINANCIAL_SIMULATION_YEARS)
            ROLLING_START(I) = CE_TAX(I) + PCAPINRST(I)
            I = I + 1
         ENDDO
      ENDIF
!
      IF(INDEX(DEPMET,'CCAL') /= 0 .OR. &
      	INDEX(DEPMET,'CCAP') /= 0) THEN
         I = 1
         TOTAL_LONG_TERM_PROJECT = 0.
         TOTAL_CASH_INTEREST = 0.
         LTP_TEST = 0.
         RS_TEST = 0.
         DO WHILE (CEP(I) == 0. .AND. I <= FINANCIAL_SIMULATION_YEARS)
            IF(get_BASE_YEAR() + I - 1 - FIRSTYR >= 2) then
            	RS_TEST = RS_TEST + CE_TAX(I) + PCAPINRST(I) - ROLLING_START(I)
            end if
            IF(get_BASE_YEAR() + I - 1 - FIRSTYR > 3) THEN
               LONG_TERM_PROJECT(I) = &
               	MAX(0., MIN(RS_TEST,LTP_TEST-LONG_TERM_PROJECT(I-1)))
            ELSE
               LONG_TERM_PROJECT(I) = ROLLING_START(I)
            ENDIF
            TOTAL_LONG_TERM_PROJECT = TOTAL_LONG_TERM_PROJECT + &
            	LONG_TERM_PROJECT(I)
            IF(get_BASE_YEAR() + I - 1 >= FIRSTYR) LTP_TEST = LTP_TEST + &
            	CE_TAX(I) + PCAPINRST(I) - LONG_TERM_PROJECT(I)
            TOTAL_CASH_INTEREST = TOTAL_CASH_INTEREST + CE_TAX(I) + &
	            PCAPINRST(I)
            I = I + 1
         ENDDO
         IF(INDEX(DEPMET,'CCAP') /= 0) THEN
            IF(TOTAL_LONG_TERM_PROJECT > TOTAL_ROLLING_START) THEN
               DEPMET = 'CCAL'
            ELSE
               DEPMET = 'CCAR'
            ENDIF
         ENDIF
         IF(INDEX(DEPMET,'CCAL') /= 0) THEN
            IF(I <= FINANCIAL_SIMULATION_YEARS) THEN
               LONG_TERM_PROJECT(I) = TOTAL_CASH_INTEREST + CE_TAX(I) + &
               	PCAPINRST(I) - TOTAL_LONG_TERM_PROJECT
               I = I + 1
               DO WHILE (I <= FINANCIAL_SIMULATION_YEARS)
                  LONG_TERM_PROJECT(I) = CE_TAX(I) + PCAPINRST(I)
                  I = I + 1
               ENDDO
            ENDIF
         ENDIF
      ENDIF
!
      TAXDP(1) = 0.
      IF(TAXLF > 1.) THEN
         DEP_RATE = DBRATE/TAXLF
      ELSE
         DEP_RATE = 1.
      ENDIF
      IF(INDEX(DEPMET,'CCAL') /= 0) THEN
         CEPTX(1) = LONG_TERM_PROJECT(1)
         DO I = 2, FINANCIAL_SIMULATION_YEARS
            CEPTX(I) = CEPTX(I-1)+LONG_TERM_PROJECT(I)-TAXDP(I-1)
            TAXDP(I) = CEPTX(I) * DEP_RATE
         ENDDO
      ELSE
         CEPTX(1) = ROLLING_START(1)
         DO I = 2, FINANCIAL_SIMULATION_YEARS
            CEPTX(I) = CEPTX(I-1)+ROLLING_START(I)-TAXDP(I-1)
            TAXDP(I) = CEPTX(I) * DEP_RATE
         ENDDO
      ENDIF
      RETURN
      END

!*                                                                     *
!*                        FAST_TAX_RATE_34                             *
!*                                                                     *
!*          COPYRIGHT (C) 1992 M.S. GERBER & ASSOCIATES, INC.          *
!*                       ALL RIGHTS RESERVED                           *
!*                                                                     *

!*                                                                     *
!*    PURPOSE:                                                         *
!*       CALCULATES THE TAC VALUE AND THE TAX DEP FOR CANADIAN         *
!*       FAST TAX RATE 34                                              *
!*                                                                     *

!
      SUBROUTINE FAST_TAX_RATE_34(CEP,CEPTX,CE,OHRATE,TAXEXP,CWIP, &
                                  CAPINRST,CEPTXBOK,TAXDP, &
                                  FINANCIAL_SIMULATION_YEARS)
!
      use SpinDriftLib
      use prod_arrays_dimensions

      INTEGER (KIND=2) :: J,L,IYR,FINANCIAL_SIMULATION_YEARS
      REAL :: TAXBAL,OHRATE,TAXAMT,NPVFT,DEPPCT(3),TAXDEP
      REAL :: CAPINRST(FINANCIAL_SIMULATION_YEARS), &
           CEPTXBOK(FINANCIAL_SIMULATION_YEARS), &
           CEP(FINANCIAL_SIMULATION_YEARS), &
           CEPTX(FINANCIAL_SIMULATION_YEARS), &
           CE(FINANCIAL_SIMULATION_YEARS), &
           CWIP(FINANCIAL_SIMULATION_YEARS), &
           TAXEXP(FINANCIAL_SIMULATION_YEARS), &
           TAXDP(FINANCIAL_SIMULATION_YEARS)
!      
! FAST TAX RATE 34 25,50,25
!
      DEPPCT(1) = .25
      DEPPCT(2) = .50
      DEPPCT(3) = .25
      TAXBAL = TAXEXP(1)
      TAXDP = 0.
      DO J = 2, FINANCIAL_SIMULATION_YEARS
         TAXEXP(J) = OHRATE * CE(J)
         TAXBAL    = TAXBAL + TAXEXP(J)
         IF(CWIP(J-1) + CE(J) /= 0. .AND. CEP(J) /= 0.) THEN
            TAXAMT    = CEP(J) / (CWIP(J-1) + CE(J)) * TAXBAL
            CEPTX(J)  = CEP(J) -  TAXAMT + CAPINRST(J)
            CEPTXBOK(J) = CEP(J)
            TAXBAL    = TAXBAL - TAXAMT
            NPVFT = CEPTX(J)
            DO L = J, MIN(J+INT(2,2),FINANCIAL_SIMULATION_YEARS)
               IYR = L - J + 1
               TAXDEP = CEPTX(J) * DEPPCT(IYR)
               TAXDP(L) = TAXDP(L) + TAXDEP
               NPVFT = NPVFT - TAXDEP
            ENDDO
         ENDIF
      ENDDO
      RETURN
      END

!*                                                                     *
!*                           D E P B O K                               *
!*                                                                     *
!*          COPYRIGHT (c) 1982   M.S. GERBER & ASSOCIATES, INC         *
!*                       ALL RIGHTS RESERVED                           *
!*                                                                     *
!*                       MIDAS GOLD ADDITIONS                          *
!*          COPYRIGHT (c) 1991   M.S. GERBER & ASSOCIATES, INC         *
!*                       ALL RIGHTS RESERVED                           *

!                                                                      *
!     PURPOSE:                                                         *
!        DEPBOK CALCULATES THE BOOK DEPRECIATION FOR FUTURE            *
!        PLANT ENTERING SERVICE USING STRAIGHT LINE WITH               *
!        HALF YEAR CONVENTION                                          *
!                                                                      *

!
      SUBROUTINE DEPBOK(R_BOKLF,BOKDP,CEP,RATIO,SERVICEMO,FIRSTYR, &
                        SALVAGE_VALUE_IN,FINANCIAL_SIMULATION_YEARS, &
                        HALF_YEAR_BOOK_DEP)
!
      use spindriftlib
      use prod_arrays_dimensions
      use sizecom
      use globecom
      use mod_base_year
      implicit none
      
      LOGICAL (KIND=1) :: HALF_YEAR_BOOK_DEP
      INTEGER (KIND=2) :: FINANCIAL_SIMULATION_YEARS
      CHARACTER (LEN=1) :: DATA_TYPE
      INTEGER (KIND=2) :: I,J,L,M,IVEC,SERVICEMO
      INTEGER (KIND=2) :: FIRSTYR,IOFFSET,MAX_DEP_YEARS
      INTEGER (KIND=2) :: RL_END_YEAR_INDEX,RL_END_MONTH
      REAL :: RATIO,DEPSL,VECTOR_DATA(AVAIL_DATA_YEARS), &
      	SALVAGE_VALUE,SALVAGE_VALUE_IN
!     INTERNAL VARIABLES
      REAL :: NPVFB,R_BOKLF
!     EXTERNAL VARIABLES
      REAL :: BOKLF,BOKDP(FINANCIAL_SIMULATION_YEARS), &
      	CEP(FINANCIAL_SIMULATION_YEARS), &
           DEPVEC(MAX_FINANCIAL_SIMULATION_YEARS-1),FIRST_YEAR_DEP
      INTEGER (KIND=2) :: BASE_YEAR_REF
      LOGICAL (KIND=1) :: REMAINING_LIFE_DEP
!
      REAL (KIND=4) :: MONTHLY_BOOK_DEP(0:12,0:FINANCIAL_SIMULATION_YEARS), &
                       PLANT_2_SERVICE(0:12,0:*), &
                       MONTHLY_AFUDC_ON_PLANT(0:12,0:*)
      INTEGER (KIND=2) :: DEP_MONTHS,MO_START,MO2,YR2,MO,YR
      CHARACTER (LEN=1) :: BOOK_DEP_METHOD,VECTOR_TYPE*20
      REAL (KIND=4) :: NPV,MONTHLY_DEP_AMOUNT,BOOK_LIFE,ANNUAL_BOOK_DEP
      LOGICAL (KIND=1) :: GROUP_DEPRECIATION,ACCOUNT_COMPLETE
      LOGICAL (KIND=1) :: BOOK_DEP_FINISHED
      INTEGER (KIND=2) :: START_YR
!
! MONTHLY DEP VECTOR VARIABLES
!
      INTEGER (KIND=2) :: VECTOR
      REAL (KIND=4) :: MONTHLY_BOOK_DEP_VECTOR(12,LAST_AVAILABLE_MONTHLY_YEAR)
      REAL (KIND=4) :: ANNUAL_BOOK_DEP_30(AVAIL_DATA_YEARS),TREND_NORM
      CHARACTER (LEN=1) :: MONTHLY_DATA_UNITS(LAST_AVAILABLE_MONTHLY_YEAR)
      INTEGER (KIND=2) :: MONTH_ENDING(LAST_AVAILABLE_MONTHLY_YEAR)
      INTEGER (KIND=2) :: YR1
!
!     IF THE BOOK LIFE IS LESS THAN ONE YEAR THE ASSUMPTION IS THAT
!     THE TOTAL AMOUNT OF CEP AND AFDC2 ARE TO BE EXPENSED IN ONE
!     YEAR
!
      BASE_YEAR_REF = get_BASE_YEAR()
      REMAINING_LIFE_DEP = R_BOKLF > BASE_YEAR_REF
      DEPVEC = 0.
      IF(REMAINING_LIFE_DEP) THEN
         RL_END_YEAR_INDEX = R_BOKLF
         RL_END_MONTH=NINT((R_BOKLF-FLOAT(RL_END_YEAR_INDEX))*100.)
         IF(RL_END_MONTH == 0) RL_END_MONTH = 12
         RL_END_YEAR_INDEX = RL_END_YEAR_INDEX - BASE_YEAR_REF ! + 1
         BOKLF = MAX(1.,R_BOKLF - BASE_YEAR_REF - 1.)
      ELSE
         BOKLF = R_BOKLF
      ENDIF
!
      SALVAGE_VALUE = 1. - SALVAGE_VALUE_IN/100.
      IF(BOKLF <= 1.0 .AND. BOKLF >= 0.0) THEN
         BOKDP(1) = 0.
         DO J = 2, FINANCIAL_SIMULATION_YEARS
            BOKDP(J) = CEP(J) * SALVAGE_VALUE
         ENDDO
         RETURN
      ENDIF
!
!     INITIALIZE VARIABLES
!
      BOKDP = 0.0
      DATA_TYPE = 'X'
!
!     THE USER SPECIFIED DEP VALUES ARE USED
!
      IF(BOKLF < 0.) THEN
         IVEC = ABS(BOKLF) + .001
         CALL GET_ASSET_VAR(IVEC,DATA_TYPE,VECTOR_DATA)
         IOFFSET = MIN(FINANCIAL_SIMULATION_YEARS, &
         	MAX(FIRSTYR - BASE_YEAR_REF, INT(1,2)))
         DO I = 1, IOFFSET
            DEPVEC(I) = 0.
         ENDDO
         IOFFSET = IOFFSET - 1
         I = 1
         DO WHILE ((I+IOFFSET) <= FINANCIAL_SIMULATION_YEARS .AND. &
         	(I <= AVAIL_DATA_YEARS))                
            DEPVEC(I+IOFFSET) = VECTOR_DATA(I)
            I = I + 1
         ENDDO
      ENDIF
!
!     IF THE BOOK LIFE IS GREATER THAN 98 YEARS DEPRECIATION IS NOT
!     TAKEN
!
      IF((BOKLF > 1.0  .AND. BOKLF <= 98.0) .OR. &
      	(BOKLF < 0. .AND. INDEX('YPR',DATA_TYPE) /= 0)) THEN
!
!     CALCULATE BOOK DEPRECIATION ON BRICKS AND MORTAR
!
         MAX_DEP_YEARS = FINANCIAL_SIMULATION_YEARS
         DO J = 2, MAX_DEP_YEARS
            IF(CEP(J) /= 0.0) THEN
               NPVFB = CEP(J) * SALVAGE_VALUE
               IF(REMAINING_LIFE_DEP) THEN
                     IF(J > RL_END_YEAR_INDEX) THEN
                        BOKLF = FLOAT(13-SERVICEMO)/12.
                     ELSE
                        IF(HALF_YEAR_BOOK_DEP) THEN
                           DEP_MONTHS = (RL_END_YEAR_INDEX - J)*12. &
                                         + RL_END_MONTH &
                                         + 6
                        ELSE
                           DEP_MONTHS = (RL_END_YEAR_INDEX - J)*12. &
                                         + RL_END_MONTH &
                                         + FLOAT(13-SERVICEMO)
                        ENDIF
                        DEPSL = NPVFB/DEP_MONTHS*12.
                     ENDIF
               ELSEIF(BOKLF > 0.) THEN
                  DEPSL = NPVFB/BOKLF
               ELSE
                  IF(DEPVEC(J-1) > 0.) THEN
                     IF(DATA_TYPE == 'Y') THEN
                        IF(DEPVEC(J-1) > BASE_YEAR_REF) THEN
                           DEPSL = NPVFB/(DEPVEC(J-1)-BASE_YEAR_REF)
                        ELSE
                           DEPSL = NPVFB/DEPVEC(J-1)
                        ENDIF
                     ENDIF
                     IF(DATA_TYPE == 'P') DEPSL=NPVFB*DEPVEC(J-1)/100.
                     IF(DATA_TYPE == 'R') DEPSL = NPVFB * DEPVEC(J-1)
                  ELSE
                     DEPSL = NPVFB
                  ENDIF
               ENDIF
               IF(BOKLF >= 0. .AND. BOKLF < 1.) THEN
                 FIRST_YEAR_DEP = DEPSL
               ELSE
                  IF(HALF_YEAR_BOOK_DEP) THEN
                     FIRST_YEAR_DEP = 0.5*DEPSL
                  ELSE
                     FIRST_YEAR_DEP = FLOAT(13-SERVICEMO)*DEPSL/12.
                  ENDIF
               ENDIF
               BOKDP(J) = BOKDP(J) + FIRST_YEAR_DEP
               NPVFB = NPVFB - FIRST_YEAR_DEP
               M = J + 1
               IF(M > MAX_DEP_YEARS) CYCLE
               DO L = M, MAX_DEP_YEARS
                  IF(ABS(NPVFB) < ABS(DEPSL)) THEN
                     BOKDP(L) = BOKDP(L) + NPVFB
                     EXIT
                  ELSE
                     BOKDP(L) = BOKDP(L) + DEPSL
                     NPVFB = NPVFB - DEPSL
                  ENDIF
               ENDDO
            ENDIF

         ENDDO
      ELSEIF(BOKLF < 0.) THEN
         NPVFB = 0.0
!        THEN USE THE DOLLAR AMOUNT VECTORS
         DO I = 2, FINANCIAL_SIMULATION_YEARS
            NPVFB = NPVFB + CEP(I) * SALVAGE_VALUE
            BOKDP(I) = RATIO * DEPVEC(I-1)
            NPVFB = NPVFB - BOKDP(I)
         ENDDO
      ENDIF
      RETURN

      ENTRY BOOK_DEPRECIATION(BOOK_LIFE, &
                              MONTHLY_BOOK_DEP, &
                              PLANT_2_SERVICE, &
                              MONTHLY_AFUDC_ON_PLANT, &
                              SALVAGE_VALUE_IN, &
                              BOOK_DEP_METHOD, &
                              FINANCIAL_SIMULATION_YEARS, &
                              HALF_YEAR_BOOK_DEP)

!
         GROUP_DEPRECIATION = .FALSE.
         MONTHLY_BOOK_DEP = 0.
         BASE_YEAR_REF = get_BASE_YEAR()
         IF(BOOK_DEP_METHOD == 'R') THEN
            IF(BOOK_LIFE > BASE_YEAR_REF) THEN
               RL_END_YEAR_INDEX = BOOK_LIFE
               RL_END_MONTH = NINT((BOOK_LIFE-FLOAT(RL_END_YEAR_INDEX))*100.)
               IF(RL_END_MONTH == 0) RL_END_MONTH = 12
               RL_END_YEAR_INDEX = RL_END_YEAR_INDEX - get_BASE_YEAR()
            ELSE
               RL_END_YEAR_INDEX = 1
               RL_END_MONTH = 12
            ENDIF
         ELSE
            DEP_MONTHS = MAX(INT(1,2),INT(BOOK_LIFE*12.,2))
         ENDIF
!
         DATA_TYPE = 'X'
!
         IF(BOOK_LIFE > 99. .AND. BOOK_LIFE < 1990) RETURN
         IF(BOOK_LIFE >= 0.) THEN
            SALVAGE_VALUE = 1. - SALVAGE_VALUE_IN/100.
!
            DO YR = 1, FINANCIAL_SIMULATION_YEARS
               IF(PLANT_2_SERVICE(0,YR) == 0.) CYCLE
               DO MO = 1, 12
                  ACCOUNT_COMPLETE = .FALSE.
                  IF(PLANT_2_SERVICE(MO,YR) == 0.) CYCLE
                  IF(BOOK_DEP_METHOD == 'R') THEN
                     IF(YR > RL_END_YEAR_INDEX) THEN
                        DEP_MONTHS = FLOAT(13-MO)
                     ELSE
                        DEP_MONTHS = (RL_END_YEAR_INDEX - YR-1)*12. &
                                      + RL_END_MONTH &
                                      + FLOAT(13-MO)
                     ENDIF
                  ENDIF
                  NPV = SALVAGE_VALUE * PLANT_2_SERVICE(MO,YR) + &
                  	MONTHLY_AFUDC_ON_PLANT(MO,YR)
                  MONTHLY_DEP_AMOUNT = NPV/DEP_MONTHS
                  IF(HALF_YEAR_BOOK_DEP) THEN
!
                     IF(ABS(NPV) < ABS(6.*MONTHLY_DEP_AMOUNT)) THEN
                        FIRST_YEAR_DEP = NPV
                     ELSE
                        FIRST_YEAR_DEP = 6.*MONTHLY_DEP_AMOUNT
                     ENDIF
                     
                     NPV = NPV - FIRST_YEAR_DEP
                     FIRST_YEAR_DEP = FIRST_YEAR_DEP/FLOAT(13-MO)
                     DO MO2 = MO, 12
                        MONTHLY_BOOK_DEP(MO2,YR) = MONTHLY_BOOK_DEP(MO2,YR) + &
                        	FIRST_YEAR_DEP
                     ENDDO
                     MO_START = 1
                     START_YR = YR + 1
                  ELSE
                     MONTHLY_BOOK_DEP(MO,YR) = MONTHLY_BOOK_DEP(MO,YR) + &
                     	MONTHLY_DEP_AMOUNT ! /2.
                     NPV = NPV - MONTHLY_DEP_AMOUNT ! /2.
                     MO_START = MO + 1
                     START_YR = YR
                  ENDIF
                  DO YR2 = START_YR, FINANCIAL_SIMULATION_YEARS
                     DO MO2 = MO_START, 12
                        IF(GROUP_DEPRECIATION) THEN
                           MONTHLY_BOOK_DEP(MO2,YR2)=MONTHLY_DEP_AMOUNT+  &
                           	MONTHLY_BOOK_DEP(MO2,YR2)
                        ELSEIF(ABS(NPV) <= ABS(MONTHLY_DEP_AMOUNT)) THEN
                           MONTHLY_BOOK_DEP(MO2,YR2) = NPV + &
                           	MONTHLY_BOOK_DEP(MO2,YR2)
                           ACCOUNT_COMPLETE = .TRUE.
                           EXIT
                        ELSE
                           MONTHLY_BOOK_DEP(MO2,YR2)=MONTHLY_DEP_AMOUNT+ &
                           	MONTHLY_BOOK_DEP(MO2,YR2)
                           NPV = NPV - MONTHLY_DEP_AMOUNT
                        ENDIF
                     ENDDO
                     MO_START = 1
                     IF(.NOT. GROUP_DEPRECIATION .AND. ACCOUNT_COMPLETE) then
                     	EXIT
                     end if
                  ENDDO
               ENDDO
            ENDDO
!
         ELSE
            VECTOR = ABS(BOOK_LIFE)
!
            CALL GET_MONTHLY_ANNUAL_VALUES(VECTOR, &
                                           DATA_TYPE, &
                                           VECTOR_TYPE, &
                                           ANNUAL_BOOK_DEP_30, &
                                           MONTHLY_BOOK_DEP_VECTOR, &
                                           MONTHLY_DATA_UNITS, &
                                           MONTH_ENDING)
            CALL RIPPLE_MONTHLY_DATA(ANNUAL_BOOK_DEP_30,MONTHLY_BOOK_DEP_VECTOR)
!
            CALL MONTHLY_BOOK_VALUES_IN_DOLLARS(ANNUAL_BOOK_DEP_30, &
                                                MONTHLY_BOOK_DEP_VECTOR, &
                                                MONTHLY_DATA_UNITS, &
                                                MONTH_ENDING)
!
            TREND_NORM = 0.
            BOOK_DEP_FINISHED = .FALSE.
            IF(DATA_TYPE == 'D') THEN
!
                  yr1 = 1
                  DO globecom_YEAR = YR1, FINANCIAL_SIMULATION_YEARS
                     YR = globecom_YEAR - YR1 + 1
                     DO MO = 1, 12 
                        IF(YR <= LAST_AVAILABLE_MONTHLY_YEAR) THEN
                           DEPSL = MONTHLY_BOOK_DEP_VECTOR(MO,YR)
                           IF(YR == LAST_AVAILABLE_MONTHLY_YEAR) THEN
                              TREND_NORM = TREND_NORM + &
                              	MONTHLY_BOOK_DEP_VECTOR(MO,YR)
                           ENDIF
                        ELSEIF(YR <= AVAIL_DATA_YEARS) THEN
                           IF(TREND_NORM == 0.) THEN
                              DEPSL = ANNUAL_BOOK_DEP_30(YR)/12.
                           ELSE
                              DEPSL = ANNUAL_BOOK_DEP_30(YR) * &
                                   MONTHLY_BOOK_DEP_VECTOR(MO, &
                                           LAST_AVAILABLE_MONTHLY_YEAR)/ &
                                                    TREND_NORM
                           ENDIF
                        ELSEIF(YR > AVAIL_DATA_YEARS) THEN
                           IF(TREND_NORM == 0.) THEN
                              DEPSL = ANNUAL_BOOK_DEP_30(AVAIL_DATA_YEARS)/12.
                           ELSE
                              DEPSL = &
                                ANNUAL_BOOK_DEP_30(AVAIL_DATA_YEARS)* &
                                     MONTHLY_BOOK_DEP_VECTOR(MO, &
                                           LAST_AVAILABLE_MONTHLY_YEAR)/ &
                                                    TREND_NORM
                           ENDIF
                        ENDIF
                        MONTHLY_BOOK_DEP(MO,globecom_YEAR) = DEPSL
                     ENDDO
                  ENDDO
!
!
            ELSE
            ENDIF
         ENDIF
!
         DO YR = 1, FINANCIAL_SIMULATION_YEARS
            MONTHLY_BOOK_DEP(0,YR) = SUM(MONTHLY_BOOK_DEP(1:12,YR))
         ENDDO
!
      RETURN
      END

      SUBROUTINE FA_STZERO

!
      use SpinDriftLib
      USE PROD_ARRAYS_DIMENSIONS
      USE SIZECOM
!
      REAL :: WORK1
      COMMON /FA_WKARRY/ WORK1(31*MAX_FINANCIAL_SIMULATION_YEARS)
!
      WORK1 = 0.
      RETURN
      END

!                                                                      *
!          SUBROUTINE TO CALCULATE FUTURE ASSET VALUE                  *
!          COPYRIGHT (c) 1983 M.S. GERBER & ASSOCIATES, INC.           *
!                                                                      *

!
      SUBROUTINE SET_UP_MODEL_ASSET_ARRAYS
!
      USE IREC_ENDPOINT_CONTROL
      USE DRILLING_REPT_PARAMETERS
      use mod_base_year
      use grx_planning_routines
      use SpinDriftLib
      USE PROD_ARRAYS_DIMENSIONS
      use sizecom
      use globecom

!
      REAL :: GPV_ADDITIONS,DEP_ADDITIONS, &
      	AFUDC_IN_CWIP(MAX_FINANCIAL_SIMULATION_YEARS)
      INTEGER (KIND=2) :: R_YR,R_CLASS
      LOGICAL (KIND=1) :: R_CLASS_EXISTS,FIRST_PASS_IS_ACTIVE
      REAL :: R_CLASS_GPV,R_CLASS_CUMULATIVE_DEPRECIATION
      REAL :: FA_CASH,FA_CAPITIALIZED,FA_TAX_DEP, &
           FA_BOOK_DEP,FA_AFDC_CASH, &
           FA_AFDC_CAPITIALIZED,FA_CWIP, &
           FA_CWIP_IN_RB,FA_CURRENT_TAX_EXPENSES, &
           FA_CAPITIALIZED_INTEREST, &
           FA_PROPERTY_TAX_GPV,FA_PROPERTY_TAX_NPV, &
           FA_AMORTIZATION_OF_DEF_DEBIT, &
           FA_EXTRAORDINARY_EXPENSE, &
           FA_NET_DEFERRED_DEBIT_BAL, &
           FA_DEF_DEBIT_IN_RB, &
           FA_DEF_TAX_B4_TAX_RATES, & 
           FA_SL_TAX_DEP, & ! TBKDPT(I)
           FA_TAX_PREFERENCE_DEP, & 
           FA_ACE_BOOK_DEP, & 
           FA_WO_ADJUSTMENT_2_CUM_BOOK_DEP, &
           FA_WO_ADJUSTMENT_2_CUM_AFDC_DEP, &
           FA_AFDC_BORROWED, & 
           FA_WO_ADJUSTMENT_2_CUM_AFDC, & 
           FA_WO_ADJUSTMENT_2_CUM_AFEXP, &
           FA_WO_ADJUSTMENT_2_CUM_DEF_TAX, &
           FA_WO_ADJUSTMENT_2_CUM_AFDCF  
!
      CHARACTER (LEN=1) :: DUMMY_TYPE
      INTEGER (KIND=2) :: MAX_ASSET_CLASS_NUM,ASSET_CLASS,ASSET_CLASS_VECTOR, &
      	CLASS,R_ASSET_CLASS
      SAVE MAX_ASSET_CLASS_NUM
      REAL :: ASSET_CLASS_LIST(AVAIL_DATA_YEARS), &
      	ASSET_ALLOCATION_LIST(AVAIL_DATA_YEARS),ALLOCATOR,TAXWO
      INTEGER (KIND=2) :: ASSET_CLASS_POINTER(:),CLASS_POINTER
      ALLOCATABLE :: ASSET_CLASS_POINTER
      SAVE ASSET_CLASS_POINTER
      REAL CUMULATIVE_CASH_ADJUSTMENT(MAX_FINANCIAL_SIMULATION_YEARS)
      SAVE CUMULATIVE_CASH_ADJUSTMENT
      CHARACTER (LEN=2) :: CAPACITY_PLANNING_METHOD
      INTEGER (KIND=2) :: NUM_OF_FA_CLASSES=0
      INTEGER (KIND=4) :: VALUES_TO_ZERO
      INTEGER (KIND=2) :: FINANCIAL_SIMULATION_YEARS
      SAVE FINANCIAL_SIMULATION_YEARS
      INTEGER (KIND=2) :: I,ABMETH,ABYEAR
      REAL :: PCAPINRST(*),TXPREFDEP(*),ACE_BOOK_DEP(*), &
      	REGULATORY_ALLOCATOR,ABTAX,CUMNPV,CUM_DD_AMORT
!     TYPE DECLARATION FOR /WKARRY/
!        TAX ITEMS
           REAL :: TXDEF,TAXDP,CEPTX,ITCDF,ITCDP,TAXEXP,TXDEFC,ITCPAY
!        AFDC ITEMS
           REAL :: CE,CEP,AFDC1,AFDC2,CWIP,RBCWIP
!        BOOK ITEMS
           REAL :: BOKDP,AFCDP
!        ABANDONMENT ITEMS
          REAL :: DDB,RBDDB,AFDPAJ,AJAFDC,EXEXP,AMORTE,AFCEXP,BKDPTX, &
               BKDPAJ,WODFTX,AFDC1B,AFDC2B,AFDCDF
          REAL :: GPV_PROPERTY_TAX, &
               NPV_PROPERTY_TAX
      COMMON /FA_WKARRY/ TXDEF(MAX_FINANCIAL_SIMULATION_YEARS), &
      TAXDP(MAX_FINANCIAL_SIMULATION_YEARS), &
             CEPTX(MAX_FINANCIAL_SIMULATION_YEARS), &
             ITCDF(MAX_FINANCIAL_SIMULATION_YEARS), &
             ITCDP(MAX_FINANCIAL_SIMULATION_YEARS), &
             TAXEXP(MAX_FINANCIAL_SIMULATION_YEARS), &
             TXDEFC(MAX_FINANCIAL_SIMULATION_YEARS), &
             CE(MAX_FINANCIAL_SIMULATION_YEARS), &
             CEP(MAX_FINANCIAL_SIMULATION_YEARS), &
             AFDC1(MAX_FINANCIAL_SIMULATION_YEARS), &
             AFDC2(MAX_FINANCIAL_SIMULATION_YEARS), &
             CWIP(MAX_FINANCIAL_SIMULATION_YEARS), &
             RBCWIP(MAX_FINANCIAL_SIMULATION_YEARS), &
             BOKDP(MAX_FINANCIAL_SIMULATION_YEARS), &
             AFCDP(MAX_FINANCIAL_SIMULATION_YEARS), &
             DDB(MAX_FINANCIAL_SIMULATION_YEARS), &
             RBDDB(MAX_FINANCIAL_SIMULATION_YEARS), &
             AFDPAJ(MAX_FINANCIAL_SIMULATION_YEARS), &
             AJAFDC(MAX_FINANCIAL_SIMULATION_YEARS), &
             EXEXP(MAX_FINANCIAL_SIMULATION_YEARS), &
             AMORTE(MAX_FINANCIAL_SIMULATION_YEARS), &
             BKDPTX(MAX_FINANCIAL_SIMULATION_YEARS), &
             ITCPAY(MAX_FINANCIAL_SIMULATION_YEARS), &
             AFCEXP(MAX_FINANCIAL_SIMULATION_YEARS), &
             BKDPAJ(MAX_FINANCIAL_SIMULATION_YEARS), &
             WODFTX(MAX_FINANCIAL_SIMULATION_YEARS), &
             AFDC1B(MAX_FINANCIAL_SIMULATION_YEARS), &
             AFDC2B(MAX_FINANCIAL_SIMULATION_YEARS), &
             AFDCDF(MAX_FINANCIAL_SIMULATION_YEARS), &
             GPV_PROPERTY_TAX(MAX_FINANCIAL_SIMULATION_YEARS), &
             NPV_PROPERTY_TAX(MAX_FINANCIAL_SIMULATION_YEARS)
!
      REAL :: TCE(:,:),TCEP(:,:),TTAXDP(:,:),TBOKDP(:,:),TAFCDP(:,:), &
           TAFDC1(:,:),TAFDC2(:,:),TCWIP(:,:),TRBCWP(:,:),TITCDP(:,:), &
           TITCDF(:,:),TTXEXP(:,:),TTXDEF(:,:),TDDB(:,:),TRBDDB(:,:), &
           TAMRTE(:,:),TEXEXP(:,:),TAFDPA(:,:),TCEPTX(:,:),TAFCAJ(:,:), &
           TBKDPT(:,:),TITCPY(:,:),TAFEXP(:,:),TBKDPA(:,:), &
           TPCAPINRST(:,:),TTXPREFDEP(:,:),TOTAL_ACE_BOOK_DEP(:,:), &
           TAFDCB(:,:),TWODFT(:,:),TAFDCF(:,:), &
           PROPERTY_TAX_GPV(:,:),PROPERTY_TAX_NPV(:,:)
      REAL :: CLASS_GPV(:,:),CLASS_CUMULATIVE_DEPRECIATION(:,:), &
      CLASS_AFUDC_IN_CWIP(:,:)
      REAL :: RB_NPV(:),RB_TAXDP(:),RB_BOKDP(:),RB_PCAPINRST(:), &
           RBCWIP_AFDC_METH2(:),RB_TXEXP(:),RB_TXDEF(:), &
           RB_TXPREFDEP(:),RB_AMRTE(:),RB_ITCPY(:),RB_ITC(:), &
           RB_AFDC1(:)
      ALLOCATABLE :: TCE,TCEP,TTAXDP,TBOKDP,TAFCDP, &
                     TAFDC1,TAFDC2,TCWIP,TRBCWP,TITCDP, &
                     TITCDF,TTXEXP,TTXDEF,TDDB,TRBDDB, &
                     TAMRTE,TEXEXP,TAFDPA,TCEPTX,TAFCAJ, &
                     TBKDPT,TITCPY,TAFEXP,TBKDPA, &
                     TPCAPINRST,TTXPREFDEP,TOTAL_ACE_BOOK_DEP, &
                     TAFDCB,TWODFT,TAFDCF,RB_NPV, &
                     RB_TAXDP,RB_BOKDP,RB_PCAPINRST, &
                     RBCWIP_AFDC_METH2,RB_TXEXP,RB_TXDEF, &
                     RB_TXPREFDEP,RB_AMRTE,RB_ITCPY,RB_ITC, &
                     RB_AFDC1,PROPERTY_TAX_GPV,PROPERTY_TAX_NPV, &
                     CLASS_GPV,CLASS_CUMULATIVE_DEPRECIATION, &
                     CLASS_AFUDC_IN_CWIP
      SAVE TCE,TCEP,TTAXDP,TBOKDP,TAFCDP, &
           TAFDC1,TAFDC2,TCWIP,TRBCWP,TITCDP, &
           TITCDF,TTXEXP,TTXDEF,TDDB,TRBDDB, &
           TAMRTE,TEXEXP,TAFDPA,TCEPTX,TAFCAJ, &
           TBKDPT,TITCPY,TAFEXP,TBKDPA, &
           TPCAPINRST,TTXPREFDEP,TOTAL_ACE_BOOK_DEP, &
           TAFDCB,TWODFT,TAFDCF,RB_NPV, &
           RB_TAXDP,RB_BOKDP,RB_PCAPINRST, &
           RBCWIP_AFDC_METH2,RB_TXEXP,RB_TXDEF, &
           RB_TXPREFDEP,RB_AMRTE,RB_ITCPY,RB_ITC, &
           RB_AFDC1,PROPERTY_TAX_GPV,PROPERTY_TAX_NPV, &
           CLASS_GPV,CLASS_CUMULATIVE_DEPRECIATION, &
           CLASS_AFUDC_IN_CWIP
      LOGICAL (KIND=1) :: FUTURE_ASSET_REPORT
      CHARACTER (LEN=40) :: OUT_OPTION_NAME
      INTEGER (KIND=2) :: FA_REPORTING_UNIT
      INTEGER :: FA_REPORTING_REC
      REAL (KIND=4) :: CURRENT_INTEREST_CAP_RATE(*)
      REAL (KIND=4) :: ALLOCATION_VALUE(0:AVAIL_DATA_YEARS)
!
      LOGICAL (KIND=1) :: VOID_LOGICAL,RETURN_ASSET_CLASS_LISTS,LGandE
      INTEGER (KIND=2) :: ALLOCATION_VECTOR
!
         FINANCIAL_SIMULATION_YEARS = get_globecom_study_period() + get_extension_period() +1
         CALL RETURN_NUM_OF_OPTIONS_CLASSES(NUM_OF_FA_CLASSES,MAX_ASSET_CLASS_NUM)
         IF(ALLOCATED(ASSET_CLASS_POINTER)) then
         	DEALLOCATE(ASSET_CLASS_POINTER)
         end if
         IF(MAX_ASSET_CLASS_NUM > 0) THEN
            ALLOCATE(ASSET_CLASS_POINTER(MAX_ASSET_CLASS_NUM))
            CALL RETURN_OPTIONS_CLASS_POINTER(ASSET_CLASS_POINTER)
         ENDIF
!      return
!
         IF(ALLOCATED(TCE)) &
                    DEALLOCATE(TCE,TCEP,TTAXDP,TBOKDP,TAFCDP, &
                             TAFDC1,TAFDC2,TCWIP,TRBCWP,TITCDP, &
                             TITCDF,TTXEXP,TTXDEF,TDDB,TRBDDB, &
                             TAMRTE,TEXEXP,TAFDPA,TCEPTX,TAFCAJ, &
                             TBKDPT,TITCPY,TAFEXP,TBKDPA, &
                             TPCAPINRST,TTXPREFDEP,TOTAL_ACE_BOOK_DEP, &
                             TAFDCB,TWODFT,TAFDCF,RB_NPV, &
                             RB_TAXDP,RB_BOKDP,RB_PCAPINRST, &
                             RBCWIP_AFDC_METH2,RB_TXEXP,RB_TXDEF, &
                             RB_TXPREFDEP,RB_AMRTE,RB_ITCPY,RB_ITC, &
                             RB_AFDC1,PROPERTY_TAX_GPV,PROPERTY_TAX_NPV, &
                             CLASS_GPV,CLASS_CUMULATIVE_DEPRECIATION, &
                             CLASS_AFUDC_IN_CWIP)
!
         ALLOCATE(TCE(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(TCEP(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(CLASS_GPV(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(CLASS_CUMULATIVE_DEPRECIATION( &
            FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(CLASS_AFUDC_IN_CWIP( &
            FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(TTAXDP(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(TBOKDP(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(TAFCDP(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(TAFDC1(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(TAFDC2(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(TCWIP(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(TRBCWP(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(TITCDP(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(TITCDF(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(TTXEXP(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(TTXDEF(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(TDDB(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(TRBDDB(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(TAMRTE(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(TEXEXP(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(TAFDPA(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(TCEPTX(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(TAFCAJ(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(TBKDPT(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(TITCPY(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(TAFEXP(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(TBKDPA(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(TPCAPINRST(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(TTXPREFDEP(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE( &
            TOTAL_ACE_BOOK_DEP(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(TAFDCB(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(TWODFT(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE(TAFDCF(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE( &
            PROPERTY_TAX_GPV(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
         ALLOCATE( &
            PROPERTY_TAX_NPV(FINANCIAL_SIMULATION_YEARS,0:NUM_OF_FA_CLASSES))
!
         ALLOCATE(RB_NPV(FINANCIAL_SIMULATION_YEARS), &
                  RB_TAXDP(FINANCIAL_SIMULATION_YEARS), &
                  RB_BOKDP(FINANCIAL_SIMULATION_YEARS), &
                  RB_PCAPINRST(FINANCIAL_SIMULATION_YEARS), &
                  RBCWIP_AFDC_METH2(FINANCIAL_SIMULATION_YEARS), &
                  RB_TXEXP(FINANCIAL_SIMULATION_YEARS), &
                  RB_TXDEF(FINANCIAL_SIMULATION_YEARS), &
                  RB_TXPREFDEP(FINANCIAL_SIMULATION_YEARS), &
                  RB_AMRTE(FINANCIAL_SIMULATION_YEARS), &
                  RB_ITCPY(FINANCIAL_SIMULATION_YEARS), &
                  RB_ITC(FINANCIAL_SIMULATION_YEARS), &
                  RB_AFDC1(FINANCIAL_SIMULATION_YEARS))
!
      RETURN

      ENTRY INIT_MODEL_ASSET_ARRAYS


         CLASS_GPV = 0.
         CLASS_CUMULATIVE_DEPRECIATION = 0.
         CLASS_AFUDC_IN_CWIP = 0.
         TCE = 0.
         TCEP = 0.
         TTAXDP = 0.
         TBOKDP = 0.
         TAFCDP = 0.
         TAFDC1 = 0.
         TAFDC2 = 0.
         TCWIP = 0.
         TRBCWP = 0.
         TITCDP = 0.
         TITCDF = 0.
         TTXEXP = 0.
         TTXDEF = 0.
         TDDB = 0.
         TRBDDB = 0.
         TAMRTE = 0.
         TEXEXP = 0.
         TAFDPA = 0.
         TCEPTX = 0.
         TAFCAJ = 0.
         TBKDPT = 0.
         TITCPY = 0.
         TAFEXP = 0.
         TBKDPA = 0.
         TPCAPINRST = 0.
         TTXPREFDEP = 0.
         TOTAL_ACE_BOOK_DEP = 0.
         TAFDCB = 0.
         TWODFT = 0.
         TAFDCF = 0.
         PROPERTY_TAX_GPV = 0.
         PROPERTY_TAX_NPV = 0.
!
         RB_NPV = 0.
         RB_TAXDP = 0.
         RB_BOKDP = 0.
         RB_PCAPINRST = 0.
         RBCWIP_AFDC_METH2 = 0.
         RB_TXEXP = 0.
         RB_TXDEF = 0.
         RB_TXPREFDEP = 0.
         RB_AMRTE = 0.
         RB_ITCPY = 0.
         RB_ITC = 0.
         RB_AFDC1 = 0.
         CUMULATIVE_CASH_ADJUSTMENT = 0.
!                 
      RETURN
!

      ENTRY MODEL_ASSET_TOTALS(ABMETH,ABYEAR,TAXWO,PCAPINRST,TXPREFDEP, &
                               REGULATORY_ALLOCATOR,ABTAX,ACE_BOOK_DEP, &
                               AFUDC_IN_CWIP, &
                               R_ASSET_CLASS,ASSET_CLASS_VECTOR)

!
! ALLOCATE TO TOTAL COMPANY AND ASSET CLASSES
         VOID_LOGICAL=RETURN_ASSET_CLASS_LISTS(R_ASSET_CLASS, &
                                               ASSET_CLASS_LIST, &
                                               ASSET_CLASS_VECTOR, &
                                               ASSET_ALLOCATION_LIST)
!
         CLASS_POINTER = 1
         DO
            CLASS = ASSET_CLASS_LIST(CLASS_POINTER)
            CALL CHECK_IF_CLASS_DEFINED(CLASS)
            CLASS = CLASS + 1
            IF(CLASS > 0) CLASS = ASSET_CLASS_POINTER(CLASS)
            ALLOCATOR = ASSET_ALLOCATION_LIST(CLASS_POINTER)/100.
!
            IF(ASSET_ALLOCATION_LIST(CLASS_POINTER) < 0.) THEN
               ALLOCATION_VECTOR = ABS(ASSET_ALLOCATION_LIST(CLASS_POINTER))
               CALL GET_ASSET_VAR(ALLOCATION_VECTOR, &
               	DUMMY_TYPE,ALLOCATION_VALUE(1))
               CALL RETURN_BASE_YEAR_VECTOR_VALUES(ALLOCATION_VALUE(0))
            ELSE
               ALLOCATOR = ASSET_ALLOCATION_LIST(CLASS_POINTER)
!                        
               DO I = 0, AVAIL_DATA_YEARS
                  ALLOCATION_VALUE(I) = ALLOCATOR
               ENDDO
            ENDIF
!
            IF(ABMETH > 0) THEN
               I = ABYEAR - get_BASE_YEAR() + 1
               ALLOCATOR = &
               	ALLOCATION_VALUE(MIN(AVAIL_DATA_YEARS,INT(I-1,2)))/100.
               TTXEXP(I,CLASS) = TTXEXP(I,CLASS) + TAXWO * ALLOCATOR
            ENDIF
            GPV_ADDITIONS = 0.
            DEP_ADDITIONS = 0.
            CUM_DD_AMORT = 0.
            ALLOCATOR = ALLOCATION_VALUE(0)/100.
            DO I = 1, FINANCIAL_SIMULATION_YEARS
!
!        CUMMULATE TOTAL COMPANY
!
               IF(I > 1) THEN
                  IF(LGandE()) THEN
                     GPV_ADDITIONS = GPV_ADDITIONS + CEP(I) + AFDC2(I)
                     CLASS_GPV(I,CLASS) = ALLOCATOR * GPV_ADDITIONS
!
                  ELSE
                     GPV_ADDITIONS = GPV_ADDITIONS &
                     	+ ALLOCATOR * (CEP(I) + AFDC2(I))
                     CLASS_GPV(I,CLASS) = CLASS_GPV(I,CLASS) + GPV_ADDITIONS
                  ENDIF 
                  DEP_ADDITIONS = DEP_ADDITIONS + ALLOCATOR * (BOKDP(I) + &
                  AFCDP(I))
                  CLASS_CUMULATIVE_DEPRECIATION(I,CLASS) = &
                  CLASS_CUMULATIVE_DEPRECIATION(I,CLASS)+DEP_ADDITIONS
                  CLASS_AFUDC_IN_CWIP(I,CLASS) = ALLOCATOR * &
                  AFUDC_IN_CWIP(I) + CLASS_AFUDC_IN_CWIP(I,CLASS)
               ENDIF
               TCE(I,CLASS)    = TCE(I,CLASS)    + CE(I) * ALLOCATOR
               TCEP(I,CLASS)   = TCEP(I,CLASS)   + CEP(I) * ALLOCATOR
               TCEPTX(I,CLASS) = TCEPTX(I,CLASS) + CEPTX(I) * ALLOCATOR
               TTAXDP(I,CLASS) = TTAXDP(I,CLASS) + TAXDP(I) * ALLOCATOR
               TBKDPT(I,CLASS) = TBKDPT(I,CLASS) + BKDPTX(I) * ALLOCATOR
               TBOKDP(I,CLASS) = TBOKDP(I,CLASS) + BOKDP(I) * ALLOCATOR
               TAFCDP(I,CLASS) = TAFCDP(I,CLASS) + AFCDP(I) * ALLOCATOR
               TAFDC1(I,CLASS) = TAFDC1(I,CLASS) + AFDC1(I) * ALLOCATOR
               TAFDCB(I,CLASS) = TAFDCB(I,CLASS) + AFDC1B(I) * ALLOCATOR
               TAFDCF(I,CLASS) = TAFDCF(I,CLASS) + AFDCDF(I) * ALLOCATOR
               TAFDC2(I,CLASS) = TAFDC2(I,CLASS) + AFDC2(I) * ALLOCATOR
               TCWIP(I,CLASS)  = TCWIP(I,CLASS)  + CWIP(I) * ALLOCATOR
               TRBCWP(I,CLASS) = TRBCWP(I,CLASS) + RBCWIP(I) * ALLOCATOR
               TITCDP(I,CLASS) = TITCDP(I,CLASS) + ITCDP(I) * ALLOCATOR
               TITCDF(I,CLASS) = TITCDF(I,CLASS) + ITCDF(I) * ALLOCATOR
               TTXEXP(I,CLASS) = TTXEXP(I,CLASS) + TAXEXP(I) * ALLOCATOR
               TTXDEF(I,CLASS) = TTXDEF(I,CLASS) + TXDEF(I) * ALLOCATOR
               TPCAPINRST(I,CLASS) = TPCAPINRST(I,CLASS) + PCAPINRST(I) * &
               	ALLOCATOR
               TTXPREFDEP(I,CLASS) = TTXPREFDEP(I,CLASS) + TXPREFDEP(I) * &
               	ALLOCATOR
               TOTAL_ACE_BOOK_DEP(I,CLASS)=TOTAL_ACE_BOOK_DEP(I,CLASS) + &
               	ACE_BOOK_DEP(I) * ALLOCATOR
               PROPERTY_TAX_GPV(I,CLASS) = PROPERTY_TAX_GPV(I,CLASS) + &
               	GPV_PROPERTY_TAX(I) * ALLOCATOR
               PROPERTY_TAX_NPV(I,CLASS) = PROPERTY_TAX_NPV(I,CLASS) + &
               	NPV_PROPERTY_TAX(I) * ALLOCATOR
               IF(ABMETH > 0) THEN
                  CUM_DD_AMORT = CUM_DD_AMORT + AMORTE(I)
                  TDDB(I,CLASS)   = TDDB(I,CLASS) + &
                  	 (DDB(I) - CUM_DD_AMORT) * ALLOCATOR
!
                  TRBDDB(I,CLASS) = TRBDDB(I,CLASS) + RBDDB(I)*ALLOCATOR
                  TAMRTE(I,CLASS) = TAMRTE(I,CLASS)+AMORTE(I)*ALLOCATOR
                  TEXEXP(I,CLASS) = TEXEXP(I,CLASS) + EXEXP(I)*ALLOCATOR
                  TAFEXP(I,CLASS) = TAFEXP(I,CLASS)+AFCEXP(I)*ALLOCATOR
                  TAFDPA(I,CLASS) = TAFDPA(I,CLASS)+AFDPAJ(I)*ALLOCATOR
                  TBKDPA(I,CLASS) = TBKDPA(I,CLASS)+BKDPAJ(I)*ALLOCATOR
                  TAFCAJ(I,CLASS) = TAFCAJ(I,CLASS)+AJAFDC(I)*ALLOCATOR
                  TITCPY(I,CLASS) = TITCPY(I,CLASS)+ITCPAY(I)*ALLOCATOR
                  TWODFT(I,CLASS) = TWODFT(I,CLASS)+WODFTX(I)*ALLOCATOR
               ENDIF
               IF(I <= AVAIL_DATA_YEARS) ALLOCATOR = ALLOCATION_VALUE(I)/100.
            ENDDO
            CLASS_POINTER = CLASS_POINTER + 1
            IF(CLASS_POINTER > AVAIL_DATA_YEARS) EXIT
            IF(ASSET_CLASS_LIST(CLASS_POINTER) == 0. .OR. &
            	ASSET_CLASS_LIST(CLASS_POINTER) == -99.) EXIT
         ENDDO    
!              
         IF(REGULATORY_ALLOCATOR /= 0.) THEN
            CUMNPV = 0.
            IF(ABMETH > 0) THEN
               I = ABYEAR - get_BASE_YEAR() + 1
               IF(ABMETH /= 6) RB_TXEXP(I) = RB_TXEXP(I) + TAXWO*ABTAX * &
               	REGULATORY_ALLOCATOR
            ENDIF
            DO I = 1, FINANCIAL_SIMULATION_YEARS
               CUMNPV = CUMNPV +(CEP(I)+AFDC2(I)-BOKDP(I)-AFCDP(I)) * &
               	REGULATORY_ALLOCATOR
               RB_NPV(I)   = RB_NPV(I)   + CUMNPV
               RBCWIP_AFDC_METH2(I) = RBCWIP_AFDC_METH2(I) + (CWIP(I) + &
               	AFUDC_IN_CWIP(I)) * REGULATORY_ALLOCATOR
               RB_TAXDP(I) = RB_TAXDP(I) + TAXDP(I)  * REGULATORY_ALLOCATOR
               RB_AFDC1(I) = RB_AFDC1(I) + AFDC1(I)  * REGULATORY_ALLOCATOR
               RB_BOKDP(I) = RB_BOKDP(I) + (BOKDP(I) + AFCDP(I)) * &
               	REGULATORY_ALLOCATOR
               RB_TXPREFDEP(I) = RB_TXPREFDEP(I) + TXPREFDEP(I) * &
               	REGULATORY_ALLOCATOR
               RB_PCAPINRST(I) = RB_PCAPINRST(I) + PCAPINRST(I) * &
               	REGULATORY_ALLOCATOR
               RB_TXEXP(I) = RB_TXEXP(I) + TAXEXP(I) * REGULATORY_ALLOCATOR
               RB_TXDEF(I) = RB_TXDEF(I) + TXDEF(I)  * REGULATORY_ALLOCATOR
               RB_ITC(I)   = RB_ITC(I) + (ITCDP(I) + ITCDF(I)) * &
               REGULATORY_ALLOCATOR
               IF(ABMETH > 0) THEN
                  RB_AMRTE(I) = RB_AMRTE(I) + AMORTE(I) * REGULATORY_ALLOCATOR
                  RB_ITCPY(I) = RB_ITCPY(I) + ITCPAY(I) * REGULATORY_ALLOCATOR
               ENDIF
            ENDDO
         ENDIF
      RETURN

      ENTRY SAVE_MODEL_ASSET_TOTALS(FUTURE_ASSET_REPORT, &
                                    FA_REPORTING_UNIT, &
                                    CURRENT_INTEREST_CAP_RATE, &
                                    FA_REPORTING_REC)

!
! WRITE THE FUASSET FILE 
!
      DO CLASS = 1, NUM_OF_FA_CLASSES
         DO I = 1, FINANCIAL_SIMULATION_YEARS
            CLASS_GPV(I,0) = CLASS_GPV(I,0) + CLASS_GPV(I,CLASS) 
            CLASS_CUMULATIVE_DEPRECIATION(I,0) = &
            	CLASS_CUMULATIVE_DEPRECIATION(I,0) + &
            		CLASS_CUMULATIVE_DEPRECIATION(I,CLASS) 
            CLASS_AFUDC_IN_CWIP(I,0) = CLASS_AFUDC_IN_CWIP(I,0)+ &
            	CLASS_AFUDC_IN_CWIP(I,CLASS)
            TCE(I,0)    = TCE(I,0)    + TCE(I,CLASS)
            TCEP(I,0)   = TCEP(I,0)   + TCEP(I,CLASS)
            TCEPTX(I,0) = TCEPTX(I,0) + TCEPTX(I,CLASS)
            TTAXDP(I,0) = TTAXDP(I,0) + TTAXDP(I,CLASS)
            TBKDPT(I,0) = TBKDPT(I,0) + TBKDPT(I,CLASS)
            TBOKDP(I,0) = TBOKDP(I,0) + TBOKDP(I,CLASS)
            TAFCDP(I,0) = TAFCDP(I,0) + TAFCDP(I,CLASS)
            TAFDC1(I,0) = TAFDC1(I,0) + TAFDC1(I,CLASS)
            TAFDCB(I,0) = TAFDCB(I,0) + TAFDCB(I,CLASS)
            TAFDCF(I,0) = TAFDCF(I,0) + TAFDCF(I,CLASS)
            TAFDC2(I,0) = TAFDC2(I,0) + TAFDC2(I,CLASS)
            TCWIP(I,0)  = TCWIP(I,0)  + TCWIP(I,CLASS)
            TRBCWP(I,0) = TRBCWP(I,0) + TRBCWP(I,CLASS)
            TITCDP(I,0) = TITCDP(I,0) + TITCDP(I,CLASS)
            TITCDF(I,0) = TITCDF(I,0) + TITCDF(I,CLASS)
            TTXEXP(I,0) = TTXEXP(I,0) + TTXEXP(I,CLASS)
            TTXDEF(I,0) = TTXDEF(I,0) + TTXDEF(I,CLASS)
            TDDB(I,0)   = TDDB(I,0)   + TDDB(I,CLASS)
            TRBDDB(I,0) = TRBDDB(I,0) + TRBDDB(I,CLASS)
            TAMRTE(I,0) = TAMRTE(I,0) + TAMRTE(I,CLASS)
            TEXEXP(I,0) = TEXEXP(I,0) + TEXEXP(I,CLASS)
            TAFEXP(I,0) = TAFEXP(I,0) + TAFEXP(I,CLASS)
            TAFDPA(I,0) = TAFDPA(I,0) + TAFDPA(I,CLASS)
            TBKDPA(I,0) = TBKDPA(I,0) + TBKDPA(I,CLASS)
            TAFCAJ(I,0) = TAFCAJ(I,0) + TAFCAJ(I,CLASS)
            TITCPY(I,0) = TITCPY(I,0) + TITCPY(I,CLASS)
            TWODFT(I,0) = TWODFT(I,0) + TWODFT(I,CLASS)
!
            TPCAPINRST(I,0) = TPCAPINRST(I,0) + TPCAPINRST(I,CLASS)
            TTXPREFDEP(I,0) = TTXPREFDEP(I,0) + TTXPREFDEP(I,CLASS)
            TOTAL_ACE_BOOK_DEP(I,0)=TOTAL_ACE_BOOK_DEP(I,0) + &
            	TOTAL_ACE_BOOK_DEP(I,CLASS)
            PROPERTY_TAX_GPV(I,0) = PROPERTY_TAX_GPV(I,0) + &
            	PROPERTY_TAX_GPV(I,CLASS)
            PROPERTY_TAX_NPV(I,0) = PROPERTY_TAX_NPV(I,0) + &
            	PROPERTY_TAX_NPV(I,CLASS)
         ENDDO
      ENDDO
!
!
! WRITE SUMMARY REPORT
!
      IF(FUTURE_ASSET_REPORT) THEN
         DRILLING_ACCOUNT_NAME = "Model Added Assets-Total"
         DO I = 1, FINANCIAL_SIMULATION_YEARS
            WRITE(FA_REPORTING_UNIT,REC=FA_REPORTING_REC) &
                                     PRT_ENDPOINT(), &
                                     DRILLING_ACCOUNT_NAME, &
                                     FLOAT(I+get_BASE_YEAR()-1), &
                                     TCE(I,0),TAFDC1(I,0), &
                                     TCWIP(I,0),TCEP(I,0), &
                                     TAFDC2(I,0), &
                                     TBOKDP(I,0)+TAFCDP(I,0), &
                                     TTXDEF(I,0),TTAXDP(I,0), &
                                     TBOKDP(I,0), &
                                     TTXEXP(I,0), &
                                     TOTAL_ACE_BOOK_DEP(I,0), &
                                     TPCAPINRST(I,0), &
                                     TPCAPINRST(I,0) * &
                                     (1.-CURRENT_INTEREST_CAP_RATE(I)), &
                                     CLASS_AFUDC_IN_CWIP(I,0), &
                                     TCWIP(I,0)+CLASS_AFUDC_IN_CWIP(I,0)
            FA_REPORTING_REC = FA_REPORTING_REC + 1
        ENDDO
      ENDIF
!
!
      CUMULATIVE_CASH_ADJUSTMENT = 0.
      IF(CAPACITY_PLANNING_METHOD() == 'PR' .AND. &
      	FIRST_PASS_IS_ACTIVE()) THEN
         DO I = 2, FINANCIAL_SIMULATION_YEARS
            CUMULATIVE_CASH_ADJUSTMENT(I) = TCEP(I,0) - TCE(I,0)
         ENDDO
      ENDIF
      CALL OPEN_NEW_ASSETS_OUT_FILE(INT(10,2))
      DO I = 1, FINANCIAL_SIMULATION_YEARS
         WRITE(10,REC=I) TCE(I,0),TCEP(I,0),TTAXDP(I,0),TBOKDP(I,0), &
            TAFCDP(I,0),TAFDC1(I,0),TAFDC2(I,0),TCWIP(I,0),TRBCWP(I,0), &
            TITCDP(I,0),TITCDF(I,0),TTXEXP(I,0),TTXDEF(I,0),TBKDPA(I,0), &
            TDDB(I,0),TRBDDB(I,0),TAMRTE(I,0),TEXEXP(I,0),TAFDPA(I,0), &
            TAFDCB(I,0),TAFCAJ(I,0),TITCPY(I,0),TAFEXP(I,0),TWODFT(I,0), &
            TAFDCF(I,0),TBKDPT(I,0),TPCAPINRST(I,0),TTXPREFDEP(I,0), &
            RB_NPV(I),RB_TAXDP(I),RB_BOKDP(I),RB_PCAPINRST(I), &
            RBCWIP_AFDC_METH2(I), &
            RB_TXEXP(I),RB_TXDEF(I),RB_TXPREFDEP(I), &
            RB_AMRTE(I),RB_ITCPY(I),RB_ITC(I),RB_AFDC1(I), &
            TOTAL_ACE_BOOK_DEP(I,0), &
            PROPERTY_TAX_GPV(I,0),PROPERTY_TAX_NPV(I,0), &
            CUMULATIVE_CASH_ADJUSTMENT(I)
      ENDDO
      CLOSE(10)
      RETURN

      ENTRY MODEL_ASSET_INFO(R_YR,R_CLASS,R_CLASS_EXISTS, &
      	R_CLASS_GPV,R_CLASS_CUMULATIVE_DEPRECIATION, &
          FA_CASH,FA_CAPITIALIZED,FA_TAX_DEP,FA_BOOK_DEP, &
		  FA_AFDC_CASH,FA_AFDC_CAPITIALIZED,FA_CWIP, &
          FA_CWIP_IN_RB,FA_CURRENT_TAX_EXPENSES, &
		  FA_CAPITIALIZED_INTEREST, &
          FA_PROPERTY_TAX_GPV,FA_PROPERTY_TAX_NPV, &
		  FA_AMORTIZATION_OF_DEF_DEBIT, &
		  FA_EXTRAORDINARY_EXPENSE, &
          FA_NET_DEFERRED_DEBIT_BAL,FA_DEF_DEBIT_IN_RB, &
          FA_DEF_TAX_B4_TAX_RATES, &            
          FA_SL_TAX_DEP, &                      
          FA_TAX_PREFERENCE_DEP, &              
          FA_ACE_BOOK_DEP, &                    
          FA_WO_ADJUSTMENT_2_CUM_BOOK_DEP, &    
          FA_WO_ADJUSTMENT_2_CUM_AFDC_DEP, &    
          FA_AFDC_BORROWED, &                   
          FA_WO_ADJUSTMENT_2_CUM_AFDC, &        
          FA_WO_ADJUSTMENT_2_CUM_AFEXP, &       
          FA_WO_ADJUSTMENT_2_CUM_DEF_TAX, &     
          FA_WO_ADJUSTMENT_2_CUM_AFDCF)         
		
!
         R_CLASS_EXISTS = .FALSE.
!
         IF(R_CLASS <= MAX_ASSET_CLASS_NUM .AND. ALLOCATED(CLASS_GPV)) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN 
               R_CLASS_GPV = R_CLASS_GPV + CLASS_GPV(R_YR,ASSET_CLASS)
               R_CLASS_CUMULATIVE_DEPRECIATION = &
                         R_CLASS_CUMULATIVE_DEPRECIATION + &
                         CLASS_CUMULATIVE_DEPRECIATION(R_YR,ASSET_CLASS)
               FA_CASH = FA_CASH + TCE(R_YR,ASSET_CLASS)
               FA_CAPITIALIZED = FA_CAPITIALIZED +TCEP(R_YR,ASSET_CLASS)
               FA_TAX_DEP = FA_TAX_DEP + TTAXDP(R_YR,ASSET_CLASS)
               FA_BOOK_DEP = FA_BOOK_DEP + TBOKDP(R_YR,ASSET_CLASS) + &
               	TAFCDP(R_YR,ASSET_CLASS)
               FA_AFDC_CASH = FA_AFDC_CASH + TAFDC1(R_YR,ASSET_CLASS)
               FA_AFDC_CAPITIALIZED =FA_AFDC_CAPITIALIZED + &
               	TAFDC2(R_YR,ASSET_CLASS) 
               FA_CWIP = FA_CWIP + TCWIP(R_YR,ASSET_CLASS) + &
               	CLASS_AFUDC_IN_CWIP(R_YR,ASSET_CLASS)
               FA_CWIP_IN_RB = FA_CWIP_IN_RB + TRBCWP(R_YR,ASSET_CLASS)
               FA_CURRENT_TAX_EXPENSES = FA_CURRENT_TAX_EXPENSES + &
               	TTXEXP(R_YR,ASSET_CLASS)
               FA_CAPITIALIZED_INTEREST = FA_CAPITIALIZED_INTEREST + &
               	TPCAPINRST(R_YR,ASSET_CLASS)
               FA_PROPERTY_TAX_GPV = FA_PROPERTY_TAX_GPV + &
               	PROPERTY_TAX_GPV(R_YR,ASSET_CLASS)
               FA_PROPERTY_TAX_NPV = FA_PROPERTY_TAX_NPV +  &
               	PROPERTY_TAX_NPV(R_YR,ASSET_CLASS)
               FA_AMORTIZATION_OF_DEF_DEBIT = TAMRTE(R_YR,ASSET_CLASS) + &
                                            FA_AMORTIZATION_OF_DEF_DEBIT
               FA_EXTRAORDINARY_EXPENSE = FA_EXTRAORDINARY_EXPENSE + &
               	TEXEXP(R_YR,ASSET_CLASS)
               FA_NET_DEFERRED_DEBIT_BAL = FA_NET_DEFERRED_DEBIT_BAL + &
               	TDDB(R_YR,ASSET_CLASS)
               FA_DEF_DEBIT_IN_RB = FA_DEF_DEBIT_IN_RB + &
               	TRBDDB(R_YR,ASSET_CLASS)
               FA_DEF_TAX_B4_TAX_RATES = FA_DEF_TAX_B4_TAX_RATES + &
               	TTXDEF(R_YR,ASSET_CLASS)
               FA_SL_TAX_DEP = FA_SL_TAX_DEP + TBKDPT(R_YR,ASSET_CLASS)
               FA_TAX_PREFERENCE_DEP = FA_TAX_PREFERENCE_DEP + &
               	TTXPREFDEP(R_YR,ASSET_CLASS)
               FA_ACE_BOOK_DEP = FA_ACE_BOOK_DEP + &
               	TOTAL_ACE_BOOK_DEP(R_YR,ASSET_CLASS)
               FA_WO_ADJUSTMENT_2_CUM_BOOK_DEP=TBKDPA(R_YR,ASSET_CLASS)+ &
                                         FA_WO_ADJUSTMENT_2_CUM_BOOK_DEP
               FA_WO_ADJUSTMENT_2_CUM_AFDC_DEP=TAFDPA(R_YR,ASSET_CLASS)+ &
                                         FA_WO_ADJUSTMENT_2_CUM_AFDC_DEP
               FA_AFDC_BORROWED = FA_AFDC_BORROWED + TAFDCB(R_YR,ASSET_CLASS)
               FA_WO_ADJUSTMENT_2_CUM_AFDC = TAFCAJ(R_YR,ASSET_CLASS) + &
               	FA_WO_ADJUSTMENT_2_CUM_AFDC
               FA_WO_ADJUSTMENT_2_CUM_AFEXP = TAFEXP(R_YR,ASSET_CLASS) + &
               	FA_WO_ADJUSTMENT_2_CUM_AFEXP
               FA_WO_ADJUSTMENT_2_CUM_DEF_TAX=TWODFT(R_YR,ASSET_CLASS) + &
               	FA_WO_ADJUSTMENT_2_CUM_DEF_TAX
               FA_WO_ADJUSTMENT_2_CUM_AFDCF = TAFDCF(R_YR,ASSET_CLASS) + &
               	FA_WO_ADJUSTMENT_2_CUM_AFDCF
            ENDIF                    
         ENDIF
      RETURN
      END

      SUBROUTINE TREND_MONTHLY_INPUT(OUTPUT_MONTHLY, &
                                     MONTHLY_DATA_UNITS, &
                                     MONTH_ENDING, &
                                     OFFSET, &
                                     INSERVICE_MO, &
                                     CAL_YEARS)

!
      use mod_base_year
      use SpinDriftLib
      USE PROD_ARRAYS_DIMENSIONS
      USE SIZECOM
      use globecom
      implicit none


      INTEGER (KIND=2) :: OFFSET,CAL_YEARS
      REAL (KIND=4) :: ANNUAL_DATA(*)
      REAL (KIND=4) :: MONTHLY_DATA(12,*),ACTIVE_TREND(0:12)
      REAL (KIND=4) ::OUTPUT_MONTHLY(0:12,0:*),PLANT_2_SERVICE(0:12,0:*)
      REAL (KIND=4) :: CWIP_BAL_BY,QUICK_CASH,CWIP_BAL
      CHARACTER (LEN=1) :: MONTHLY_DATA_UNITS(LAST_AVAILABLE_MONTHLY_YEAR)
      INTEGER (KIND=2) :: MONTH_ENDING(LAST_AVAILABLE_MONTHLY_YEAR)
      INTEGER (KIND=2) :: YR,MO,J,DUMMY_MONTH_ENDING,INSERVICE_MO
      CHARACTER (LEN=1) :: TREND
      PARAMETER(TREND='T') 
      CHARACTER (LEN=1) :: RIPPLE_TYPE,AVERAGE,ZERO,NORMALIZE
      PARAMETER(AVERAGE='A',ZERO='Z',NORMALIZE='N')
      LOGICAL (KIND=1) :: ACTIVE_TREND_FOUND
!
         J = 1
         DUMMY_MONTH_ENDING = 0 
         DO YR = OFFSET, CAL_YEARS
            IF(J <= LAST_AVAILABLE_MONTHLY_YEAR) THEN
               CALL MONTHLY_BOOK_VALUES_4_YEAR(OUTPUT_MONTHLY(0,YR), &
                                               MONTHLY_DATA_UNITS(J), &
                                               MONTH_ENDING(J))
            ELSE
               OUTPUT_MONTHLY(INSERVICE_MO,YR) = 1
               CALL MONTHLY_BOOK_VALUES_4_YEAR(OUTPUT_MONTHLY(0,YR), &
                                               NORMALIZE, &
                                               DUMMY_MONTH_ENDING)
            ENDIF
            J = J + 1
         ENDDO
      RETURN

      ENTRY TREND_MONTHLY_EXPEN_INPUT(OUTPUT_MONTHLY, &
                                           MONTHLY_DATA_UNITS, &
                                           MONTH_ENDING, &
                                           OFFSET, &
                                           CAL_YEARS, &
                                           INSERVICE_MO, &
                                           CWIP_BAL_BY, &
                                           PLANT_2_SERVICE)

!
         J = 1
         DUMMY_MONTH_ENDING = 0 
         CWIP_BAL = CWIP_BAL_BY 
!
         DO YR = OFFSET, CAL_YEARS
            IF(J <= LAST_AVAILABLE_MONTHLY_YEAR) THEN
               CALL MONTHLY_BOOK_VALUES_4_YEAR(OUTPUT_MONTHLY(0,YR), &
                                               MONTHLY_DATA_UNITS(J), &
                                               MONTH_ENDING(J))
            ELSE
               IF(PLANT_2_SERVICE(0,YR) == 0. .OR. &
                CWIP_BAL >= PLANT_2_SERVICE(0,YR)) THEN
                  CALL MONTHLY_BOOK_VALUES_4_YEAR(OUTPUT_MONTHLY(0,YR), &
                                                  TREND, &
                                                  DUMMY_MONTH_ENDING)
               ELSE
                  QUICK_CASH = PLANT_2_SERVICE(0,YR) - CWIP_BAL
                  OUTPUT_MONTHLY(0,YR) = OUTPUT_MONTHLY(0,YR) - QUICK_CASH 
                  IF(ABS(OUTPUT_MONTHLY(0,YR)) > .01) THEN
                     CALL MONTHLY_BOOK_VALUES_4_YEAR( &
                                                   OUTPUT_MONTHLY(0,YR), &
                                                   TREND, &
                                                   DUMMY_MONTH_ENDING)
                  ENDIF
                  OUTPUT_MONTHLY(0,YR) = OUTPUT_MONTHLY(0,YR) + QUICK_CASH 
                  DO MO = 1, INSERVICE_MO               
                     OUTPUT_MONTHLY(MO,YR) = &
                     	OUTPUT_MONTHLY(MO,YR) + QUICK_CASH/FLOAT(INSERVICE_MO) 
                  ENDDO
               ENDIF
            ENDIF               
            CWIP_BAL = CWIP_BAL &
                       + OUTPUT_MONTHLY(0,YR) &
                       - PLANT_2_SERVICE(0,YR)
            J = J + 1
         ENDDO
      RETURN

      ENTRY WVPA_TREND_MONTHLY_EXPEN_INPUT(OUTPUT_MONTHLY, &
                                           MONTHLY_DATA_UNITS, &
                                           MONTH_ENDING, &
                                           OFFSET, &
                                           CAL_YEARS)

!
         J = 1
         DUMMY_MONTH_ENDING = 0 
!
         DO YR = OFFSET, CAL_YEARS
            IF(J <= LAST_AVAILABLE_MONTHLY_YEAR) THEN
               CALL MONTHLY_BOOK_VALUES_4_YEAR(OUTPUT_MONTHLY(0,YR), &
                                               MONTHLY_DATA_UNITS(J), &
                                               MONTH_ENDING(J))
            ELSE
               CALL MONTHLY_BOOK_VALUES_4_YEAR(OUTPUT_MONTHLY(0,YR), &
                                               TREND, &
                                               DUMMY_MONTH_ENDING)
            ENDIF               
            J = J + 1
         ENDDO
      RETURN

      ENTRY MOVE_ANNUAL_MONTHLY_INPUT(OUTPUT_MONTHLY, &
                                      ANNUAL_DATA, &
                                      MONTHLY_DATA, &
                                      OFFSET, &
                                      CAL_YEARS, &
                                      INSERVICE_MO)

!
         J = 1
         ACTIVE_TREND_FOUND = .FALSE.
         DO YR = OFFSET, CAL_YEARS
            IF(J <= AVAIL_DATA_YEARS) THEN
               OUTPUT_MONTHLY(0,YR) = ANNUAL_DATA(J)
            ELSE
               OUTPUT_MONTHLY(0,YR) = ANNUAL_DATA(AVAIL_DATA_YEARS)
            ENDIF

            IF(J <= LAST_AVAILABLE_MONTHLY_YEAR) THEN

                OUTPUT_MONTHLY(1:12,YR) = MONTHLY_DATA(1:12,J)
                IF(OUTPUT_MONTHLY(0,YR) /= 0.) THEN
                   ACTIVE_TREND(:) = OUTPUT_MONTHLY(:,YR)
                   ACTIVE_TREND_FOUND = .TRUE.
                ENDIF

            ELSE
               IF(ACTIVE_TREND_FOUND) THEN
                  OUTPUT_MONTHLY(1:,YR) = OUTPUT_MONTHLY(0,YR)* &
                  	ACTIVE_TREND(1:)/ACTIVE_TREND(0)
               ELSE
                  OUTPUT_MONTHLY(INSERVICE_MO,YR) = OUTPUT_MONTHLY(0,YR)
               ENDIF
            ENDIF
            J = J + 1
         ENDDO
      RETURN

      ENTRY TREND_ANNUAL_INPUT(OUTPUT_MONTHLY, &
                               ANNUAL_DATA, &
                               OFFSET, &
                               CAL_YEARS)

!
         J = 1
         DO YR = OFFSET, CAL_YEARS
            IF(J <= AVAIL_DATA_YEARS) THEN
               OUTPUT_MONTHLY(0,YR) = ANNUAL_DATA(J)
            ELSE
               OUTPUT_MONTHLY(0,YR) = ANNUAL_DATA(AVAIL_DATA_YEARS)
            ENDIF
!
            DO MO = 1, 12 
               OUTPUT_MONTHLY(MO,YR) = OUTPUT_MONTHLY(0,YR)/12.
            ENDDO
            J = J + 1
         ENDDO
      RETURN

      ENTRY MOVE_ANNUAL_INPUT(OUTPUT_MONTHLY, &
                               ANNUAL_DATA, &
                               OFFSET, &
                               CAL_YEARS)

!
         J = 1
         DO YR = OFFSET, CAL_YEARS
            IF(J <= AVAIL_DATA_YEARS) THEN
               OUTPUT_MONTHLY(0,YR) = ANNUAL_DATA(J)
            ELSE
               OUTPUT_MONTHLY(0,YR) = ANNUAL_DATA(AVAIL_DATA_YEARS)
            ENDIF
            J = J + 1
         ENDDO
      RETURN

      ENTRY RIPPLE_MOVE_MONTHLY_DATA(OUTPUT_MONTHLY, &
                                     MONTHLY_DATA, &
                                     OFFSET, &
                                     CAL_YEARS, &
                                     RIPPLE_TYPE)

!
         J = 1
         DO YR = OFFSET, CAL_YEARS
            IF(J > LAST_AVAILABLE_MONTHLY_YEAR) EXIT
            IF(MONTHLY_DATA(1,J) == -999999.) THEN
               IF(RIPPLE_TYPE == ZERO) THEN
                  OUTPUT_MONTHLY(1,YR) = 0.
               ELSEIF(RIPPLE_TYPE == AVERAGE) THEN
                  OUTPUT_MONTHLY(1,YR) = OUTPUT_MONTHLY(0,YR)/12.
               ENDIF
            ELSE
               OUTPUT_MONTHLY(1,YR) = MONTHLY_DATA(1,J)
            ENDIF
            DO MO = 2, 12
               IF(MONTHLY_DATA(MO,J)==-999999.) THEN
                  OUTPUT_MONTHLY(MO,YR) = OUTPUT_MONTHLY(MO-1,YR)
               ELSE
                  OUTPUT_MONTHLY(MO,YR) = MONTHLY_DATA(MO,J)
               ENDIF
            ENDDO
            J = J + 1
         ENDDO
       
      RETURN
      END

      RECURSIVE SUBROUTINE MONTHLY_FUTURE_ASSETS_OBJECT()

!
      use SpinDriftLib
      USE PROD_ARRAYS_DIMENSIONS
      USE SIZECOM
      use mthnmcom
      implicit none
      SAVE

      REAL (KIND=4) :: TOTAL_MONTHLY_FA_VALUES(:,:,:,:)
      INTEGER (KIND=2) :: ASSET_CLASS_POINTER(:)
      ALLOCATABLE :: TOTAL_MONTHLY_FA_VALUES,ASSET_CLASS_POINTER
      INTEGER (KIND=2) :: FINANCIAL_SIMULATION_YEARS,RUN_YEARS,EXTENSION_YEARS
      INTEGER (KIND=4) :: VALUES_2_SET
      INTEGER (KIND=2) :: MAX_ASSET_CLASS_NUM,NUM_OF_CLASSES
!
      LOGICAL (KIND=1) :: MONTHLY_ACTIVE
!

      INTEGER (KIND=2) :: R_ASSET_CLASS_VECTOR,R_ASSET_CLASS, &
        ASSET_CLASS_ID,DEACTIVE_YR,MO_DEACT
      REAL (KIND=4) :: BY_CWIP_BALANCE,REGULATORY_ALLOCATOR
      REAL (KIND=4) :: R_BOOK_EXPEN(0:12,0:*), &
             R_MONTHLY_AFUDC_ON_CASH(0:12,0:*), &
             R_MONTHLY_AFUDC_ON_PLANT(0:12,0:*), &
             R_MONTHLY_BOOK_DEP(0:12,0:*), &
             R_PLANT_2_SERVICE(0:12,0:*), &
             R_CASH_EXPENDITURES(0:12,0:*), &
             R_MONTHLY_CWIP(0:12,0:*), &
             R_MONTHLY_CWIP_IN_RATEBASE(0:12,0:*), &
             R_CUM_BOOK_DEP(0:12,0:*), &
             R_GPV(0:12,0:*), &
             R_MONTHLY_AFUDC_IN_CWIP(0:12,0:*), &
             AFUDC_BORROWED_RATES(*)
      REAL (KIND=4) :: R_CASH_LAG_PATTERN(12),R_CASH_PAYMENTS(12)
      REAL (KIND=4) :: ASSET_CLASS_LIST(30)
      REAL (KIND=4) :: ASSET_ALLOCATION_LIST(30),ALLOCATOR
      INTEGER (KIND=2) :: CLASS,CLASS_POINTER,MO,YR ! ,AVAIL_DATA_YEARS/30/
      CHARACTER (LEN=1) :: DUMMY_TYPE
      REAL (KIND=4) :: ALLOCATION_VALUE(0:AVAIL_DATA_YEARS)
      INTEGER (KIND=2) :: ALLOCATION_VECTOR
      REAL (KIND=4) :: MONTHLY_CWIP(0:12),MONTHLY_GPV(0:12)
      REAL (KIND=4) :: MONTH_VARS(0:12,1:*)
      INTEGER (KIND=2) :: R_YR
      LOGICAL (KIND=1) :: VOID_LOGICAL,RETURN_ASSET_CLASS_LISTS
!
!
         MONTHLY_ACTIVE = .FALSE.
         IF(ALLOCATED(TOTAL_MONTHLY_FA_VALUES))  &
         	DEALLOCATE(TOTAL_MONTHLY_FA_VALUES,ASSET_CLASS_POINTER)
!
         FINANCIAL_SIMULATION_YEARS= RUN_YEARS() + EXTENSION_YEARS()
         CALL RETURN_INITIALIZATION_CLASSES(NUM_OF_CLASSES,MAX_ASSET_CLASS_NUM)
         IF(MAX_ASSET_CLASS_NUM > 0) THEN
            ALLOCATE(ASSET_CLASS_POINTER(MAX_ASSET_CLASS_NUM))
            ALLOCATE(TOTAL_MONTHLY_FA_VALUES(0:12, &
                                           0:FINANCIAL_SIMULATION_YEARS, &
                                           0:NUM_OF_CLASSES, &
                                           MAX_FA_VARIALBES))
!
            TOTAL_MONTHLY_FA_VALUES = 0.
            CALL RETURN_INITIALIZATION_POINTER(ASSET_CLASS_POINTER)
            MONTHLY_ACTIVE = .TRUE.
         ENDIF
         
      RETURN

      ENTRY SAVE_MONTHLY_FA_VALUES()

         OPEN(10,FILE='BC_FAMON.BIN',FORM='UNFORMATTED')
         WRITE(10) TOTAL_MONTHLY_FA_VALUES
         CLOSE(10) 
      RETURN

      ENTRY RETRIVE_MONTHLY_FA_VALUES()

         CALL MONTHLY_FUTURE_ASSETS_OBJECT
         OPEN(10,FILE='BC_FAMON.BIN',FORM='UNFORMATTED')
         READ(10) TOTAL_MONTHLY_FA_VALUES
         CLOSE(10) 
      RETURN

      ENTRY MONTHLY_TOTAL_FUTURE_ASSETS(R_ASSET_CLASS, &
                                        R_ASSET_CLASS_VECTOR, &
                                        R_BOOK_EXPEN, &
                                        R_CASH_LAG_PATTERN, &
                                        R_CASH_PAYMENTS, &
                                        R_MONTHLY_AFUDC_ON_CASH, &
                                        R_MONTHLY_AFUDC_ON_PLANT, &
                                        R_PLANT_2_SERVICE, &
                                        R_MONTHLY_BOOK_DEP, &
                                        R_CASH_EXPENDITURES, &
                                        AFUDC_BORROWED_RATES, &
                                        BY_CWIP_BALANCE, &
                                        REGULATORY_ALLOCATOR, &
                                        R_MONTHLY_CWIP, &
                                        R_MONTHLY_CWIP_IN_RATEBASE, &
                                        R_CUM_BOOK_DEP, &
                                        R_GPV, &
                                        R_MONTHLY_AFUDC_IN_CWIP)

!
         IF(.NOT. MONTHLY_ACTIVE) RETURN
!         CALL CINITD(R_CASH_EXPENDITURES,
!     +                       INT(13*(FINANCIAL_SIMULATION_YEARS+1)),0.)
         CALL LAG_CASH_USING_PATTERN_ALL_YRS(FINANCIAL_SIMULATION_YEARS, &
                                             R_BOOK_EXPEN, &
                                             R_CASH_LAG_PATTERN, &
                                             R_CASH_PAYMENTS, &
                                             R_CASH_EXPENDITURES)
!
         VOID_LOGICAL=RETURN_ASSET_CLASS_LISTS(R_ASSET_CLASS, &
                                               ASSET_CLASS_LIST, &
                                               R_ASSET_CLASS_VECTOR, &
                                               ASSET_ALLOCATION_LIST)
!
!
         CLASS_POINTER = 1
         DO
            CLASS = ASSET_CLASS_LIST(CLASS_POINTER)
            CALL CHECK_IF_CLASS_DEFINED(CLASS)
            CLASS = CLASS + 1
            ASSET_CLASS_ID = CLASS
            IF(CLASS > 0) CLASS = ASSET_CLASS_POINTER(CLASS)
!
            IF(ASSET_ALLOCATION_LIST(CLASS_POINTER) < 0.) THEN
               ALLOCATION_VECTOR = ABS(ASSET_ALLOCATION_LIST(CLASS_POINTER))
               CALL GET_ASSET_VAR( &
                ALLOCATION_VECTOR,DUMMY_TYPE,ALLOCATION_VALUE(1))
               CALL RETURN_BASE_YEAR_VECTOR_VALUES(ALLOCATION_VALUE(0))
            ELSE
               ALLOCATOR = ASSET_ALLOCATION_LIST(CLASS_POINTER)
!                     
               DO YR = 0, AVAIL_DATA_YEARS
                  ALLOCATION_VALUE(YR) = ALLOCATOR 
               ENDDO
            ENDIF
            IF(REGULATORY_ALLOCATOR /= 1.) THEN
               DO YR = 0, AVAIL_DATA_YEARS
                  ALLOCATION_VALUE(YR) = REGULATORY_ALLOCATOR * &
                  	ALLOCATION_VALUE(YR)
               ENDDO
            ENDIF
!
!
!
            MONTHLY_CWIP(12) = BY_CWIP_BALANCE*ALLOCATION_VALUE(0)/100.
            MONTHLY_GPV(12) = 0.
            CALL CLASS_DEACTIVATE_IN_YR(ASSET_CLASS_ID,DEACTIVE_YR,MO_DEACT)
            DO YR = 1, FINANCIAL_SIMULATION_YEARS
               IF(YR > DEACTIVE_YR + 1) EXIT
               IF(YR <= AVAIL_DATA_YEARS) ALLOCATOR = ALLOCATION_VALUE(YR)/100.
               MONTHLY_CWIP(0) = MONTHLY_CWIP(12)
               MONTHLY_GPV(0) = MONTHLY_GPV(12)
               DO MO = 0, 12
                  IF(YR == DEACTIVE_YR + 1 .AND. MO >= MO_DEACT) ALLOCATOR = 0.
                  IF(YR == DEACTIVE_YR + 1 .AND. MO == MO_DEACT) THEN
                     TOTAL_MONTHLY_FA_VALUES(MO,YR,CLASS,fa_cwip_balance) = 0.
                     TOTAL_MONTHLY_FA_VALUES(MO,YR,CLASS,fa_gpv_balance) = 0.
                  ENDIF
                  TOTAL_MONTHLY_FA_VALUES(MO,YR,CLASS, &
                                       book_construction_expenditures) = &
                         TOTAL_MONTHLY_FA_VALUES(MO,YR,CLASS, &
                                         book_construction_expenditures) &
                         + ALLOCATOR * R_BOOK_EXPEN(MO,YR)
                  TOTAL_MONTHLY_FA_VALUES(MO,YR,CLASS, &
                                       cash_construction_expenditures) = &
                         TOTAL_MONTHLY_FA_VALUES(MO,YR,CLASS, &
                                         cash_construction_expenditures) &
                         + ALLOCATOR * R_CASH_EXPENDITURES(MO,YR)
                  TOTAL_MONTHLY_FA_VALUES(MO,YR,CLASS,afudc_on_cash) = &
                      TOTAL_MONTHLY_FA_VALUES(MO,YR,CLASS,afudc_on_cash) &
                      + ALLOCATOR * R_MONTHLY_AFUDC_ON_CASH(MO,YR)
                  TOTAL_MONTHLY_FA_VALUES(MO,YR,CLASS,afudc_on_plant) = &
                     TOTAL_MONTHLY_FA_VALUES(MO,YR,CLASS,afudc_on_plant) &
                     + ALLOCATOR * R_MONTHLY_AFUDC_ON_PLANT(MO,YR)
                  TOTAL_MONTHLY_FA_VALUES(MO,YR,CLASS,afudc_borrowed_on_cash) = &
                TOTAL_MONTHLY_FA_VALUES(MO,YR,CLASS,afudc_borrowed_on_cash) &
                         + ALLOCATOR * AFUDC_BORROWED_RATES(YR) * &
                         	R_MONTHLY_AFUDC_ON_CASH(MO,YR)
                  TOTAL_MONTHLY_FA_VALUES(MO,YR,CLASS, &
                                              book_dep_on_plant_afudc) = &
                         TOTAL_MONTHLY_FA_VALUES(MO,YR,CLASS, &
                                                book_dep_on_plant_afudc) &
                         + ALLOCATOR * R_MONTHLY_BOOK_DEP(MO,YR)
                  TOTAL_MONTHLY_FA_VALUES(MO,YR,CLASS,plant_2_service) = &
                         TOTAL_MONTHLY_FA_VALUES(MO,YR,CLASS,plant_2_service) &
                         + ALLOCATOR * R_PLANT_2_SERVICE(MO,YR)
                  IF(MO > 0) THEN
                     MONTHLY_CWIP(MO) = MONTHLY_CWIP(MO-1) &
                       + ALLOCATOR * (R_BOOK_EXPEN(MO,YR) &
                                      + R_MONTHLY_AFUDC_ON_CASH(MO,YR) &
                                      - R_PLANT_2_SERVICE(MO,YR) &
                                      - R_MONTHLY_AFUDC_ON_PLANT(MO,YR))
                     MONTHLY_GPV(MO) = MONTHLY_GPV(MO-1) + ALLOCATOR * &
                     	(R_PLANT_2_SERVICE(MO,YR) &
                                      + R_MONTHLY_AFUDC_ON_PLANT(MO,YR))
                  ENDIF
                  TOTAL_MONTHLY_FA_VALUES(MO,YR,CLASS,fa_cwip_balance) = &
                    TOTAL_MONTHLY_FA_VALUES(MO,YR,CLASS,fa_cwip_balance) &
                    + ALLOCATOR * (R_MONTHLY_CWIP(MO,YR) &
                                   + R_MONTHLY_AFUDC_IN_CWIP(MO,YR))
!
                  TOTAL_MONTHLY_FA_VALUES(MO,YR,CLASS,fa_gpv_balance) = &
                     TOTAL_MONTHLY_FA_VALUES( &
                        MO,YR,CLASS,fa_gpv_balance) + MONTHLY_GPV(MO)
               ENDDO
            ENDDO
            CLASS_POINTER = CLASS_POINTER + 1
            IF(CLASS_POINTER > AVAIL_DATA_YEARS) EXIT
            IF(ASSET_CLASS_LIST(CLASS_POINTER) == 0. .OR. &
            	ASSET_CLASS_LIST(CLASS_POINTER) == -99.) EXIT
         ENDDO
      RETURN

      ENTRY MONTHLY_FA_CASH_RESULTS(R_YR,R_ASSET_CLASS,MONTH_VARS)

!
!
         IF(R_ASSET_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_ASSET_CLASS ==0) THEN
               CLASS = 0
            ELSE
               CLASS = ASSET_CLASS_POINTER(R_ASSET_CLASS)
            ENDIF
            IF(CLASS > 0 .OR. R_ASSET_CLASS == 0) THEN
               DO MO = 0, 12
                  MONTH_VARS(MO,cash_plant_construction) = &
                  	MONTH_VARS(MO,cash_plant_construction) + &
                     TOTAL_MONTHLY_FA_VALUES(MO,R_YR,CLASS, &
                     	cash_construction_expenditures)
               ENDDO
            ENDIF
         ENDIF
      RETURN

      ENTRY MONTHLY_FA_INCOME_RESULTS(R_YR,R_ASSET_CLASS,MONTH_VARS)

!
!
         IF(R_ASSET_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_ASSET_CLASS ==0) THEN
               CLASS = 0
            ELSE
               CLASS = ASSET_CLASS_POINTER(R_ASSET_CLASS)
            ENDIF
            IF(CLASS > 0 .OR. R_ASSET_CLASS == 0) THEN
               DO MO = 0, 12
                  MONTH_VARS(MO,Monthly_AFUDC_Equity) = &
                                   MONTH_VARS(MO,Monthly_AFUDC_Equity) + &
                                TOTAL_MONTHLY_FA_VALUES(MO,R_YR,CLASS, &
                                                        afudc_on_cash) - &
                                TOTAL_MONTHLY_FA_VALUES(MO,R_YR,CLASS, &
                                                 afudc_borrowed_on_cash)
                  MONTH_VARS(MO,Monthly_AFUDC_Borrowed) = &
                                 MONTH_VARS(MO,Monthly_AFUDC_Borrowed) + &
                                TOTAL_MONTHLY_FA_VALUES(MO,R_YR,CLASS, &
                                                 afudc_borrowed_on_cash)
                  MONTH_VARS(MO,Monthly_AFUDC_Total) = &
                                    MONTH_VARS(MO,Monthly_AFUDC_Total) + &
                                TOTAL_MONTHLY_FA_VALUES(MO,R_YR,CLASS, &
                                                          afudc_on_cash)
                  MONTH_VARS(MO,Monthly_Book_Depreciation) = &
                              MONTH_VARS(MO,Monthly_Book_Depreciation) + &
                              TOTAL_MONTHLY_FA_VALUES(MO,R_YR,CLASS, &
                                                book_dep_on_plant_afudc)
               ENDDO
            ENDIF
         ENDIF
      RETURN

      ENTRY MONTHLY_FUTURE_ASSET_BS_INFO(R_YR,R_ASSET_CLASS,MONTH_VARS)

!
!
         IF(R_ASSET_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_ASSET_CLASS ==0) THEN
               CLASS = 0
            ELSE
               CLASS = ASSET_CLASS_POINTER(R_ASSET_CLASS)
            ENDIF
            IF(CLASS > 0 .OR. R_ASSET_CLASS == 0) THEN
               DO MO = 0, 12
                  MONTH_VARS(MO,mtly_cwip) = &
                              TOTAL_MONTHLY_FA_VALUES(MO,R_YR,CLASS, &
                                                        fa_cwip_balance)
                  MONTH_VARS(MO,monthly_gross_plant_in_svc) = &
                           MONTH_VARS(MO,monthly_gross_plant_in_svc) &
                           + TOTAL_MONTHLY_FA_VALUES(MO,R_YR,CLASS, &
                                                         fa_gpv_balance)
                  MONTH_VARS(MO,monthly_total_utility_plant) = &
                           MONTH_VARS(MO,monthly_gross_plant_in_svc) &
                           + MONTH_VARS(MO,mtly_cwip)
                  MONTH_VARS(MO,monthly_afudc_capitalized) = &
                     TOTAL_MONTHLY_FA_VALUES(MO,R_YR,CLASS, &
                                                         afudc_on_plant)
                  MONTH_VARS(MO,monthly_plant_capitalized) = &
                     TOTAL_MONTHLY_FA_VALUES(MO,R_YR,CLASS, &
                                                        plant_2_service)
               ENDDO
            ENDIF
         ENDIF
      RETURN
      END

      SUBROUTINE CHECK_CASH_LAG_PATTERN(CASH_LAG_PATTERN)

!
      REAL (KIND=4) :: CASH_LAG_PATTERN(12),PATTERN_TOTAL
      INTEGER (KIND=2) :: MONTH,MO
         PATTERN_TOTAL = 0.
         DO MONTH = 12, 1, -1
            IF(CASH_LAG_PATTERN(MONTH) > 0.) THEN
               DO MO = 1, MONTH
                  PATTERN_TOTAL=PATTERN_TOTAL+CASH_LAG_PATTERN(MO)
                  CASH_LAG_PATTERN(MO) = CASH_LAG_PATTERN(MO)/100.
               ENDDO
               IF(PATTERN_TOTAL /= 100.) THEN
                  IF(MONTH == 12) THEN
                     CASH_LAG_PATTERN(12) = CASH_LAG_PATTERN(12) + &
                     	(100.-PATTERN_TOTAL)/100.
                  ELSE
                     CASH_LAG_PATTERN(MONTH+1) = &
                     	(100.-PATTERN_TOTAL)/100.
                  ENDIF
               ENDIF
               EXIT
            ENDIF
         ENDDO
         IF(PATTERN_TOTAL == 0.) CASH_LAG_PATTERN(1) = 1.
      RETURN
      END

      SUBROUTINE S_CURVE_2_DOLLARS(PV_PLANT_COST, &
                                   ESCALATION_VECTOR, & 
                                   BOOK_CASH, &  !   CE,
                                   SIMULATION_PERIOD)

!
      use prod_arrays_dimensions
      REAL (KIND=4) :: PV_PLANT_COST,BOOK_CASH(0:12,0:*)
      INTEGER (KIND=2) :: ESCALATION_VECTOR,SIMULATION_PERIOD
      REAL (KIND=4) :: FV_PLANT_COST,CURRENT_VALUE
      REAL :: ESCALATION_RATES(AVAIL_DATA_YEARS),ESCALATION_MULTIPLIER
      INTEGER (KIND=2) :: YR
      LOGICAL (KIND=1) :: VOID_LOG,RETURN_A_VECTOR_FOR_ALL_YEARS
!
         FV_PLANT_COST = PV_PLANT_COST
!
! NEED TO HAVE A MEANS TO HAVE A PLANT UNDER CONSTRUCTION
!
!
         DO YR = 1, SIMULATION_PERIOD
            BOOK_CASH(0,YR)  = BOOK_CASH(0,YR) * FV_PLANT_COST/100.
         ENDDO
         VOID_LOG = RETURN_A_VECTOR_FOR_ALL_YEARS(ESCALATION_RATES, &
         	ESCALATION_VECTOR)
         CURRENT_VALUE = 1.
         DO YR = 1, SIMULATION_PERIOD
            IF(YR <= AVAIL_DATA_YEARS) THEN
               ESCALATION_MULTIPLIER = ESCALATION_RATES(YR)
            ENDIF
            CURRENT_VALUE=CURRENT_VALUE * ESCALATION_MULTIPLIER
            BOOK_CASH(0,YR)  = CURRENT_VALUE * BOOK_CASH(0,YR)
         ENDDO

      RETURN
      END

      SUBROUTINE S_CURVE_2_DOLLARS_VECTOR_RATE(PV_PLANT_COST, &
                                               ESCALATION_VECTOR_RATE, &
                                               BOOK_CASH, & !    CE,
                                               SIMULATION_PERIOD)

!
      use prod_arrays_dimensions
      REAL (KIND=4) :: PV_PLANT_COST,BOOK_CASH(0:12,0:*)
      INTEGER (KIND = 2) :: ESCALATION_VECTOR,SIMULATION_PERIOD
      REAL (KIND=4) ::  ESCALATION_VECTOR_RATE
      REAL (KIND = 4) :: FV_PLANT_COST,CURRENT_VALUE
      REAL (KIND = 4) :: ESCALATION_RATES(AVAIL_DATA_YEARS),&
      	ESCALATION_MULTIPLIER
      INTEGER (KIND = 2) :: YR
      LOGICAL (KIND = 1) :: VOID_LOG,RETURN_A_VECTOR_FOR_ALL_YEARS
!
         FV_PLANT_COST = PV_PLANT_COST
!
! NEED TO HAVE A MEANS TO HAVE A PLANT UNDER CONSTRUCTION
!
!
         DO YR = 1, SIMULATION_PERIOD
            BOOK_CASH(0,YR)  = BOOK_CASH(0,YR) * FV_PLANT_COST/100.
         ENDDO
         IF(ESCALATION_VECTOR_RATE < 0.) THEN
            ESCALATION_VECTOR = ABS(ESCALATION_VECTOR_RATE)
            VOID_LOG = RETURN_A_VECTOR_FOR_ALL_YEARS(ESCALATION_RATES, &
            	ESCALATION_VECTOR)
         ELSE
            ESCALATION_RATES = 1. + ESCALATION_VECTOR_RATE/100.
         ENDIF
         CURRENT_VALUE = 1.
         DO YR = 1, SIMULATION_PERIOD
            IF(YR <= AVAIL_DATA_YEARS) THEN
               ESCALATION_MULTIPLIER = ESCALATION_RATES(YR)
            ENDIF
            CURRENT_VALUE = CURRENT_VALUE * ESCALATION_MULTIPLIER
            BOOK_CASH(0,YR)  = CURRENT_VALUE * BOOK_CASH(0,YR)
         ENDDO
      RETURN
      END

      SUBROUTINE MONTHLY_BOOK_VALUES_4_YEAR(MONTHLY_VALUES, &
                                            MONTHLY_DATA_UNITS, &
                                            MONTH_ENDING)

!
      use globecom

      INTEGER (KIND=2) :: YR
      REAL (KIND=4) :: MONTHLY_VALUES(0:12),AMOUNT
      CHARACTER (LEN=1) :: MONTHLY_DATA_UNITS
      INTEGER (KIND=2) :: MO,MONTH_END
      INTEGER (KIND=2) :: MONTH_ENDING
      REAL (KIND=4) :: UNDISTRIBUTED_BALANCE
      CHARACTER (LEN=1) :: DOLLARS,PERCENT,NORMALIZE,AVERAGE,TREND,LAST_MONTH
      PARAMETER (DOLLARS='D',PERCENT='P',NORMALIZE='N', &
      	AVERAGE='A',TREND='T',LAST_MONTH='L')
      REAL (KIND=4) :: LAST_YEARS_VALUES(0:12)
      SAVE LAST_YEARS_VALUES
!
         YR = 0
         IF(MONTHLY_DATA_UNITS == TREND) THEN
            AMOUNT = LAST_YEARS_VALUES(YR)
            IF(AMOUNT /= 0.) THEN
               AMOUNT = MONTHLY_VALUES(YR)/AMOUNT
               DO MO = 1, 12 
                  MONTHLY_VALUES(MO) = AMOUNT * LAST_YEARS_VALUES(MO)
               ENDDO
            ELSE
               AMOUNT = MONTHLY_VALUES(YR)/12.
               DO MO = 1, 12 
                  MONTHLY_VALUES(MO) = AMOUNT
               ENDDO
            ENDIF
         ELSE
            IF(MONTHLY_DATA_UNITS == DOLLARS) THEN
               AMOUNT = 0.
               DO MO = 1, 12
                  AMOUNT = AMOUNT + MONTHLY_VALUES(MO)
               ENDDO
               MONTHLY_VALUES(YR) = AMOUNT
            ENDIF
            IF(MONTHLY_DATA_UNITS == PERCENT) THEN
               AMOUNT = MONTHLY_VALUES(YR)/100.
               MONTHLY_VALUES(YR) = 0.
               DO MO = 1, 12
                  MONTHLY_VALUES(MO) = AMOUNT * MONTHLY_VALUES(MO)
                  MONTHLY_VALUES(YR) = MONTHLY_VALUES(YR) + MONTHLY_VALUES(MO)
               ENDDO
            ENDIF
            IF(MONTHLY_DATA_UNITS == AVERAGE) THEN
               AMOUNT = MONTHLY_VALUES(YR)/12.
               DO MO = 1, 12
                  MONTHLY_VALUES(MO) = AMOUNT
               ENDDO
            ENDIF
            IF(MONTHLY_DATA_UNITS == LAST_MONTH) THEN
               MONTH_END = MONTH_ENDING
               AMOUNT = 0.
               DO MO = 1, MONTH_END
                  AMOUNT = AMOUNT + MONTHLY_VALUES(MO)
               ENDDO
               IF(MONTH_END < 12) THEN
                  UNDISTRIBUTED_BALANCE = MONTHLY_VALUES(YR) - AMOUNT
                  AMOUNT = 0.
                  DO MO = MONTH_END+1, 12 
                     AMOUNT = AMOUNT + MONTHLY_VALUES(MO)
                  ENDDO
                  IF(AMOUNT /= 0.) THEN
                     AMOUNT = UNDISTRIBUTED_BALANCE/AMOUNT
                     DO MO = MONTH_END+1, 12 
                        MONTHLY_VALUES(MO) = AMOUNT *MONTHLY_VALUES(MO)
                     ENDDO
                  ELSE
                     AMOUNT = UNDISTRIBUTED_BALANCE/(12-MONTH_END)
                     DO MO = MONTH_END+1, 12 
                        MONTHLY_VALUES(MO) = AMOUNT
                     ENDDO
                  ENDIF
               ELSE
                  MONTHLY_VALUES(YR) = AMOUNT
               ENDIF
            ENDIF
            IF(MONTHLY_DATA_UNITS == NORMALIZE) THEN
               AMOUNT = 0.
               DO MO = 1, 12
                  AMOUNT = AMOUNT + MONTHLY_VALUES(MO)
               ENDDO
               IF(AMOUNT /= 0.) THEN
                  AMOUNT = MONTHLY_VALUES(YR)/AMOUNT
                  DO MO = 1, 12 
                     MONTHLY_VALUES(MO) = AMOUNT * MONTHLY_VALUES(MO)
                  ENDDO
               ELSE
                  AMOUNT = MONTHLY_VALUES(YR)/12.
                  DO MO = 1, 12 
                     MONTHLY_VALUES(MO) = AMOUNT
                  ENDDO
               ENDIF
            ENDIF
         ENDIF
!
! SAVE CURRENT PATTERN FOR TREND ANALYSIS
!
!     
            LAST_YEARS_VALUES = MONTHLY_VALUES
!     
      RETURN
      END      

      SUBROUTINE DISTRIBUTE_MONTHLY_TAX_DEP(TAX_DEP, &
                                            IN_SERVICE_YR, &
                                            DISTRIBUTION_YR, &
                                            MONTHLY_TAX_VALUE_OF_ASSET, &
                                            MONTHLY_TAX_DEPRECIATION)

!
      REAL (KIND=4) :: TAX_DEP
      REAL (KIND=4) :: MONTHLY_TAX_VALUE_OF_ASSET(0:12,0:*), &
      	MONTHLY_TAX_DEPRECIATION(0:12,0:*),TAX_ALLOCATOR,TAX_DEP_2_ALLOCATE
      INTEGER (KIND=2) :: IN_SERVICE_YR,DISTRIBUTION_YR,YR,MO,MOP1
!
         YR = DISTRIBUTION_YR - 1
         IF(DISTRIBUTION_YR == IN_SERVICE_YR .AND. &
         	MONTHLY_TAX_VALUE_OF_ASSET(0,YR) /= 0.) THEN
            TAX_ALLOCATOR = TAX_DEP/MONTHLY_TAX_VALUE_OF_ASSET(0,YR)  
            DO MO = 1, 12 
               IF(MONTHLY_TAX_VALUE_OF_ASSET(MO,YR) /= 0.) THEN
                  TAX_DEP_2_ALLOCATE = TAX_ALLOCATOR / &
                    FLOAT(13-MO) * MONTHLY_TAX_VALUE_OF_ASSET(MO,YR)
     
                  DO MOP1 = MO, 12 
                     MONTHLY_TAX_DEPRECIATION(MOP1,YR) = &
                                       MONTHLY_TAX_DEPRECIATION(MOP1,YR) &
                                       + TAX_DEP_2_ALLOCATE
                  ENDDO
               ENDIF    
            ENDDO
         ELSE
            DO MO = 1, 12 
               MONTHLY_TAX_DEPRECIATION(MO,YR) = TAX_DEP/12. + &
               	MONTHLY_TAX_DEPRECIATION(MO,YR)
            ENDDO
         ENDIF           
         MONTHLY_TAX_DEPRECIATION(0,YR) = MONTHLY_TAX_DEPRECIATION(0,YR) + &
         	TAX_DEP
      RETURN
      END
