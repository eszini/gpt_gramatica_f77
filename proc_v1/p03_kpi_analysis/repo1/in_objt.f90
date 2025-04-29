!     Last change: MSG 9/7/2011 11:52:50 AM

!     IN_OBJT.FOR
!     Created: 10/16/02 3:25:38 PM
!     Author : msg
!     Last change: MSG 10/22/02 1:56:11 PM


      SUBROUTINE IN_OBJECT
      use end_routine, only: end_program, er_message
      use filename_tracker
      use mod_base_year
      use SpinDriftLib
      use prod_arrays_dimensions
      use kepcocom
      use sizecom

      INTEGER (KIND=2) :: INUNIT,DELETE,IREC,I
      INTEGER (KIND=4) :: IOS,IOS_BASE
      LOGICAL (KIND=4) :: FILE_EXISTS,UNIT_10_OPEN
      LOGICAL (KIND=1) :: INIT_FILE_ACTIVE=.FALSE.,R_INIT_FILE_ACTIVE
      CHARACTER (LEN=28) :: COMPANY_NAME,CLASS_NAME*30,CLASS_TYPE*1
      CHARACTER (LEN=28) :: TEMP_CLASS_NAME*30,CLASS_TYPE_STR*15
      CHARACTER (LEN=5) :: BASE_FILE_NAME,OVERLAY_FAMILY_NAME,INITFIL
      CHARACTER (LEN=50) :: COMMENT
      CHARACTER (LEN=256) :: FILE_NAME
      CHARACTER (LEN=256) :: BASE_FILE_DIRECTORY
      CHARACTER (LEN=256) :: DATA_DRIVE,OUTPUT_DIRECTORY
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER (LEN=1024) :: RECLN
      character (len=1024) :: rcln
! DECLARATION FOR REAL VALUES
      INTEGER (KIND=2) :: YEAR,CLASS_NUM,LINKED_CLASS_ID
      INTEGER (KIND=2) :: MOVE_TO_CLASS_ID
      INTEGER (KIND=2) :: INIT_VALUES,LRECL,LAST_DATA_NUMBER
      INTEGER (KIND=2) :: VARIABLES_NOT_IN_LAST_NUMBER
      PARAMETER(LAST_DATA_NUMBER = 58,VARIABLES_NOT_IN_LAST_NUMBER=6)

      PARAMETER(INIT_VALUES=LAST_DATA_NUMBER-52, LRECL=1024) ! INCLS. INIT_PLANNING_PEAK

      REAL :: VALUES43_45(3),VALUES_27(27),VALUES_11(11)
      REAL :: VALUES47_51(5),VALUES53_58(6)
      REAL :: ALL_VALUES(LAST_DATA_NUMBER-VARIABLES_NOT_IN_LAST_NUMBER) 
      EQUIVALENCE (ALL_VALUES(1),VALUES_27(1)), &
          (ALL_VALUES(28),VALUES_11(1)),(ALL_VALUES(39),VALUES43_45(1)), &
                  (ALL_VALUES(42),VALUES47_51(1)),(ALL_VALUES(47),VALUES53_58(1))
      character (len=20) :: CURRENCY_DENOMINATION
      CHARACTER (LEN=1) :: REPORT_CLASS
      CHARACTER (LEN=1) :: FED_TAX_EXEMPT,ACCOUNT_ACTIVE
! DECLARATION FOR LOCALS
      CHARACTER (LEN=14) :: FILE_TYPE='Initialization'
      CHARACTER (LEN=19) :: MUNI_FILE_TYPE='Municipal Init File'

      REAL :: TOTAL_RESERVE_BALANCE,FIXED_ASSETS,SYSTEM_AVERAGE_RATE
      REAL :: SPECIAL_RESERVE_BALANCE,LAST_YEAR_SALES_REVENUE
      REAL :: CALAVERAS_RESERVE_BALANCE,ACCOUNTS_RECEIVABLE
      REAL :: ACCOUNTS_PAYABLE,STD_INTEREST_PAYMENT,FUEL_INVENTORY
      REAL :: DEFERRED_FUEL_BALANCE,ACQUISITION_PREMIUM
      REAL :: OUC_BOND_SINKING_FUND_BAL,OUC_DEBT_SERV_RESV_BAL
      REAL :: OUC_RENEWAL_REPLACEMENT_BAL,OUC_OP_CONSTRUCT_FUND_BAL
      REAL :: OUC_SP_CONSTRUCT_FUND_BAL,OUC_SR_CAPTLZED_INT_FD_BAL
      REAL :: OUC_JR_CAPTLZED_INT_FD_BAL,OUC_MINI_BOND_PRIN_SKFUND_BAL
      REAL :: OUC_MINI_BOND_INT_SKFUND_BAL,OUC_LIABILITY_REDUCTION_BAL
      REAL :: OUC_FUEL_INVENTORY_BAL,OUC_MATERIALS_INVENTORY_BAL
      REAL :: OUC_PREPAID_EXP_BAL,OUC_BASE_EL_STAB_FUND_BAL
      REAL :: OUC_BASE_WATER_STAB_FUND_BAL,OUC_FUEL_STAB_FUND_BAL
      REAL :: OUC_CUST_RETENTION_FUND_BAL,OUC_TTL_UNRESTR_CUR_LIAB_BAL
      REAL :: OUC_CASH_POOLED_INVEST_BAL,OUC_OTHER_LIAB_AND_CRED_BAL
      REAL :: OUC_MINI_BOND_DEBT_BAL,OUC_OTHER_CURRENT_ASSETS_BAL
      REAL :: OUC_BOY_BOND_PRINCIPL_DUE,FASB_87_INTANGIBLE_ASSETS
      REAL :: OTHER_COMPREHENSIVE_INC_BALANCE,OptRevenueTaxBasis

!
      INTEGER (KIND=2) :: STD_INTEREST_VECTOR,LTD_INTEREST_VECTOR
      INTEGER (KIND=2) :: PS_DIVIDENDS_VECTOR,STI_INTEREST_VECTOR
      INTEGER (KIND=2) :: LTI_INTEREST_VECTOR
      INTEGER (KIND=2) :: CUSTOMER_DEPOSIT_INTREST_VECTOR
      INTEGER (KIND=2) :: DECOM_FUND_INTEREST_VECTOR
      INTEGER (KIND=2) :: RETIREMENT_FUND_INTEREST_VECTOR
      INTEGER (KIND=2) :: CURRENCY_CONVERSION_VECTOR,COMPANY_ID  ! 105
!
      REAL :: MUNI_INIT_VALUES(5)
      CHARACTER (LEN=2) :: INITOL='BC'
      INTEGER (KIND=2) :: NUM_OF_OL_INIT_CLASSES=0
      INTEGER (KIND=2) :: MAX_OL_INIT_CLASS_ID_NUM=0
      INTEGER (KIND=2) :: NUM_OF_BC_INIT_CLASSES=0
      INTEGER (KIND=2) :: MAX_BC_INIT_CLASS_ID_NUM=0
      INTEGER (KIND=2) :: R_NUM_OF_INIT_CLASSES,R_MAX_INIT_CLASS_NUM
      INTEGER (KIND=2) :: R_INIT_CLASS_POINTERS(*)
      CHARACTER (LEN=1), DIMENSION(1024) :: BC_CLASS_TYPE='X'
      CHARACTER (LEN=1), DIMENSION(1024) :: OL_CLASS_TYPE='X'
      INTEGER (KIND=2) :: BC_INIT_CLASS_POINTER(:)
      INTEGER (KIND=2) :: OL_INIT_CLASS_POINTER(:)
      INTEGER (KIND=2) :: BC_CLASS_LINKAGE(:),OL_CLASS_LINKAGE(:)
      INTEGER (KIND=2) :: TEMP_ASSET_CLASS_POINTER(:)
      INTEGER (KIND=2) :: SBU_LINKED_COUNTER(:)
      ALLOCATABLE :: BC_INIT_CLASS_POINTER,OL_INIT_CLASS_POINTER
      ALLOCATABLE :: BC_CLASS_LINKAGE,OL_CLASS_LINKAGE
      ALLOCATABLE :: TEMP_ASSET_CLASS_POINTER,SBU_LINKED_COUNTER
      SAVE BC_INIT_CLASS_POINTER,OL_INIT_CLASS_POINTER,BC_CLASS_LINKAGE,OL_CLASS_LINKAGE
      INTEGER (KIND=2) :: R_ASSET_CLASS,R_LINKED_CLASS,POINTR
      INTEGER (KIND=2) :: BC_MAX_LINKED_SBU,OL_MAX_LINKED_SBU
      INTEGER (KIND=2) :: R_MAX_LINKED_SBU
      SAVE BC_MAX_LINKED_SBU,OL_MAX_LINKED_SBU
      INTEGER (KIND=2) :: BC_MASTER_DEFINED_CLASS_IDS(:)
      INTEGER (KIND=2) :: BC_MAX_DEFINED_ID,BC_PARENT_CLASS_ID_NUM
      ALLOCATABLE :: BC_MASTER_DEFINED_CLASS_IDS
      SAVE BC_MASTER_DEFINED_CLASS_IDS,BC_MAX_DEFINED_ID,BC_PARENT_CLASS_ID_NUM
      INTEGER (KIND=2) :: OL_MASTER_DEFINED_CLASS_IDS(:)
      INTEGER (KIND=2) :: OL_MAX_DEFINED_ID
      INTEGER (KIND=2) :: OL_PARENT_CLASS_ID_NUM,DEACTIVATE_IN_YEAR
      INTEGER (KIND=2) :: ACTIVATE_IN_YEAR
      ALLOCATABLE :: OL_MASTER_DEFINED_CLASS_IDS
      SAVE OL_MASTER_DEFINED_CLASS_IDS,OL_MAX_DEFINED_ID,OL_PARENT_CLASS_ID_NUM
      INTEGER (KIND=2) :: R_PARENT_CLASS_ID,PARENT_CLASS_ID_NUM
      SAVE PARENT_CLASS_ID_NUM
      INTEGER (KIND=2) :: OL_ELIMINAITON_CLASS_ID_NUM=-30000
      INTEGER (KIND=2) :: BC_ELIMINAITON_CLASS_ID_NUM=-30000
      INTEGER (KIND=2) :: R_ELIMINAITON_CLASS_ID_NUM
      LOGICAL (KIND=1) :: DUPLICATE_ID_FOUND
      INTEGER (KIND=2) :: DUPLICATE_CLASS_ID_CHECK(0:1024)
      integer :: reclen_length
      CHARACTER (LEN=1) :: COMPANY_CODE,PLANT_ID,UNIT_ID
      LOGICAL (KIND=1) :: LAHEY_LF95
      CHARACTER (LEN=30) :: SCREEN_OUTPUT

!
!          SUBROUTINE TO CONVERT METAFILE FILES TO DIRECT ACESS BINARY
!          COPYRIGHT (C) 1983, 84, 85  M.S. GERBER & ASSOCIATES, INC.
!

!
! CONVERT THE INITIALIZATION FILE

      ENTRY IN_MAKEBIN

         DATA_DRIVE = OUTPUT_DIRECTORY()
         BASE_FILE_NAME = INITFIL()
         FILE_NAME = trim(BASE_FILE_DIRECTORY())// &
             "INB"//trim(BASE_FILE_NAME)//".DAT"
         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCINITAL.BIN", &
             ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)

         DELETE = 1

         LINKED_CLASS_ID = 0
         CLASS_NUM = 0
         CLASS_TYPE = 'P'
         CLASS_NAME = COMPANY_NAME()
         ALL_VALUES = -.000001
         VALUES_11(8) = -9999. ! TRAP FOR BLANK VALUE
         VALUES_11(9) = -9999.
         VALUES_11(10) = -9999.
         ALL_VALUES(51) = 1.
         ALL_VALUES(52) = -9999.
         BC_MAX_LINKED_SBU = 0
         BC_MAX_DEFINED_ID = 0
         BC_PARENT_CLASS_ID_NUM = 0
         PARENT_CLASS_ID_NUM = 1
         DUPLICATE_ID_FOUND = .FALSE.
         INIT_FILE_ACTIVE = FILE_EXISTS
         IF(FILE_EXISTS) THEN
            IF(LAHEY_LF95()) THEN
               SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
               CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
            ELSE
               CALL MG_LOCATE_WRITE(16,30,BASE_FILE_NAME,ALL_VERSIONS,0)
               CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
            ENDIF
            ALLOCATE(TEMP_ASSET_CLASS_POINTER(1024), &
                SBU_LINKED_COUNTER(1024),BC_CLASS_LINKAGE(0:1024), &
                    BC_MASTER_DEFINED_CLASS_IDS(0:1024))
            TEMP_ASSET_CLASS_POINTER = 0
            SBU_LINKED_COUNTER = 0
            BC_CLASS_LINKAGE = -99
            BC_MASTER_DEFINED_CLASS_IDS = -99
            DUPLICATE_CLASS_ID_CHECK = -99

            OPEN(10,FILE=FILE_NAME)
            READ(10,*,IOSTAT=IOS) DELETE
            IREC = 0
            DO
               MOVE_TO_CLASS_ID = -99
               ACCOUNTS_RECEIVABLE = 0.
               ACCOUNTS_PAYABLE = 0.
               STD_INTEREST_PAYMENT = 0.
               FUEL_INVENTORY = 0.
               DEFERRED_FUEL_BALANCE = 0.
               ACQUISITION_PREMIUM = 0.
               REPORT_CLASS = 'X'
               FED_TAX_EXEMPT = 'X'
               DEACTIVATE_IN_YEAR = 2200
               ACTIVATE_IN_YEAR = 1995
               OUC_BOND_SINKING_FUND_BAL = 0
               OUC_DEBT_SERV_RESV_BAL = 0
               OUC_RENEWAL_REPLACEMENT_BAL = 0
               OUC_OP_CONSTRUCT_FUND_BAL = 0
               OUC_SP_CONSTRUCT_FUND_BAL = 0
               OUC_SR_CAPTLZED_INT_FD_BAL = 0
               OUC_JR_CAPTLZED_INT_FD_BAL = 0
               OUC_MINI_BOND_PRIN_SKFUND_BAL = 0
               OUC_MINI_BOND_INT_SKFUND_BAL = 0
               OUC_LIABILITY_REDUCTION_BAL = 0
               OUC_FUEL_INVENTORY_BAL = 0
               OUC_MATERIALS_INVENTORY_BAL = 0
               OUC_PREPAID_EXP_BAL = 0
               OUC_BASE_EL_STAB_FUND_BAL = 0
               OUC_BASE_WATER_STAB_FUND_BAL = 0
               OUC_FUEL_STAB_FUND_BAL = 0
               OUC_CUST_RETENTION_FUND_BAL = 0
               OUC_TTL_UNRESTR_CUR_LIAB_BAL = 0
               OUC_CASH_POOLED_INVEST_BAL = 0
               OUC_OTHER_LIAB_AND_CRED_BAL = 0
               OUC_MINI_BOND_DEBT_BAL = 0
               OUC_OTHER_CURRENT_ASSETS_BAL = 0
               OUC_BOY_BOND_PRINCIPL_DUE = 0.
               ACCOUNT_ACTIVE = 'A'
               COMPANY_CODE = " "
               PLANT_ID = " "
               UNIT_ID = " "
               STD_INTEREST_VECTOR = 0.            !93
               LTD_INTEREST_VECTOR = 0.
               PS_DIVIDENDS_VECTOR = 0.
               STI_INTEREST_VECTOR = 0.
               LTI_INTEREST_VECTOR = 0.
               CUSTOMER_DEPOSIT_INTREST_VECTOR = 0.
               DECOM_FUND_INTEREST_VECTOR = 0.
               RETIREMENT_FUND_INTEREST_VECTOR = 0.
               CURRENCY_CONVERSION_VECTOR = 0.
               FASB_87_INTANGIBLE_ASSETS = 0.
               OTHER_COMPREHENSIVE_INC_BALANCE = 0.
               OptRevenueTaxBasis = 0.
               COMPANY_ID = -99                       ! 105

               DO
                  READ(10,1000,IOSTAT=IOS) RECLN
                  
                  rcln=trim(recln) ! Debugger can't see recln
                  IF(IOS /=0) then
                    EXIT
                  end if
                  IF(RECLN(1:1) == '7') then
                    EXIT
                  end if
                  RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,' &
                      //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
                  CURRENCY_DENOMINATION = 'Dollars'
                  reclen_length=len_trim(recln)
                  
                  READ(RECLN,*,IOSTAT=IOS) DELETE,VALUES_27,COMMENT, &
                  YEAR,VALUES_11,CLASS_NAME,CLASS_NUM,VALUES43_45, &
                  CLASS_TYPE_STR, &
                  VALUES47_51,LINKED_CLASS_ID,VALUES53_58, &
                  CURRENCY_DENOMINATION,REPORT_CLASS,FED_TAX_EXEMPT, &
                  DEACTIVATE_IN_YEAR, &
                  ACTIVATE_IN_YEAR,ACCOUNTS_RECEIVABLE,ACCOUNTS_PAYABLE, &
                  COMPANY_CODE,PLANT_ID,UNIT_ID,STD_INTEREST_PAYMENT, &
                  FUEL_INVENTORY,DEFERRED_FUEL_BALANCE,MOVE_TO_CLASS_ID, &
                  OUC_BOND_SINKING_FUND_BAL,OUC_DEBT_SERV_RESV_BAL, &
                  OUC_RENEWAL_REPLACEMENT_BAL,OUC_OP_CONSTRUCT_FUND_BAL, &
                  OUC_SP_CONSTRUCT_FUND_BAL,OUC_SR_CAPTLZED_INT_FD_BAL, &
                  OUC_JR_CAPTLZED_INT_FD_BAL,OUC_MINI_BOND_PRIN_SKFUND_BAL, &
                  OUC_MINI_BOND_INT_SKFUND_BAL, &
                  OUC_LIABILITY_REDUCTION_BAL, OUC_FUEL_INVENTORY_BAL, &
                  OUC_MATERIALS_INVENTORY_BAL, &
                  OUC_PREPAID_EXP_BAL,OUC_BASE_EL_STAB_FUND_BAL, &
                  ACCOUNT_ACTIVE,ACQUISITION_PREMIUM, &
                  OUC_BASE_WATER_STAB_FUND_BAL, &
                  OUC_FUEL_STAB_FUND_BAL, OUC_CUST_RETENTION_FUND_BAL, &
                  OUC_TTL_UNRESTR_CUR_LIAB_BAL,STD_INTEREST_VECTOR, &
                  LTD_INTEREST_VECTOR,PS_DIVIDENDS_VECTOR,STI_INTEREST_VECTOR, &
                  LTI_INTEREST_VECTOR,CUSTOMER_DEPOSIT_INTREST_VECTOR, &
                  DECOM_FUND_INTEREST_VECTOR,RETIREMENT_FUND_INTEREST_VECTOR, &
                  CURRENCY_CONVERSION_VECTOR,FASB_87_INTANGIBLE_ASSETS, &
                  OTHER_COMPREHENSIVE_INC_BALANCE,OptRevenueTaxBasis, &
                  COMPANY_ID                       ! 105

                  CLASS_TYPE = CLASS_TYPE_STR(1:1)
!
                  IF(IOS /= 0) THEN
                      CALL MG_LOCATE_WRITE(20,0,trim(RECLN),&
                        ALL_VERSIONS,1)
                        er_message='stop requested from In_objt SIID183'
                        call end_program(er_message)
                  ENDIF
                  IF(INDEX(REPORT_CLASS,'Y') == 0 .AND.  &
                      INDEX(REPORT_CLASS,'N') == 0) REPORT_CLASS = 'Y'
                  IF(INDEX(FED_TAX_EXEMPT,'Y') == 0 .AND. &
                      INDEX(FED_TAX_EXEMPT,'N') == 0) FED_TAX_EXEMPT = 'N'
                  IF(DELETE >= 8 .OR. ACCOUNT_ACTIVE == 'N' .OR. &
                  CLASS_TYPE == 'N') THEN
                     IF(DUPLICATE_CLASS_ID_CHECK(CLASS_NUM) == -99) THEN
                        IF(CLASS_NUM+1 > 0) THEN
                           IF(TEMP_ASSET_CLASS_POINTER(CLASS_NUM+1) == 0) THEN
                              NUM_OF_BC_INIT_CLASSES = &
                                  NUM_OF_BC_INIT_CLASSES + 1
                              TEMP_ASSET_CLASS_POINTER(CLASS_NUM+1) = &
                                  NUM_OF_BC_INIT_CLASSES
                        MAX_BC_INIT_CLASS_ID_NUM=MAX(CLASS_NUM+int(1,2), &
                        MAX_BC_INIT_CLASS_ID_NUM)
                           ENDIF
                        ENDIF
                     endif
                  ELSE
                     IF(DUPLICATE_CLASS_ID_CHECK(CLASS_NUM) == -99) THEN
                        IF(CLASS_TYPE /=  'N') THEN
                           DUPLICATE_CLASS_ID_CHECK(CLASS_NUM) = 1
                        ENDIF
                     ELSEIF(CLASS_TYPE /=  'N') THEN
                        WRITE(4,*) 'Duplicate Class ID found for', &
                            CLASS_NUM,' ',CLASS_NAME
                        DUPLICATE_ID_FOUND = .TRUE.
                     ENDIF
                     IF(CLASS_TYPE /= 'E') THEN !  .AND. CLASS_TYPE /= 'C') THEN
                        IF(CLASS_NUM+1 > 0) THEN
                           IF(TEMP_ASSET_CLASS_POINTER(CLASS_NUM+1) == 0) THEN
                              NUM_OF_BC_INIT_CLASSES = NUM_OF_BC_INIT_CLASSES + 1
                              TEMP_ASSET_CLASS_POINTER(CLASS_NUM+1) = &
                                  NUM_OF_BC_INIT_CLASSES
                        MAX_BC_INIT_CLASS_ID_NUM= &
                        MAX(CLASS_NUM+int(1,2),MAX_BC_INIT_CLASS_ID_NUM)
                           ENDIF
                        ENDIF

                        IF(CLASS_TYPE == 'P') THEN

                           PARENT_CLASS_ID_NUM = CLASS_NUM ! + 1
                        ELSE
                            IF(LINKED_CLASS_ID+1 > 0) THEN
                              BC_CLASS_LINKAGE(NUM_OF_BC_INIT_CLASSES) = &
                                  LINKED_CLASS_ID
                              BC_CLASS_TYPE(NUM_OF_BC_INIT_CLASSES) = CLASS_TYPE
                              SBU_LINKED_COUNTER(LINKED_CLASS_ID+1) = 1+ &
                              SBU_LINKED_COUNTER(LINKED_CLASS_ID+1)
                              BC_MAX_LINKED_SBU = MAX(BC_MAX_LINKED_SBU, &
                              SBU_LINKED_COUNTER(LINKED_CLASS_ID+1))
                           ENDIF
                        ENDIF
                     ELSEIF(CLASS_TYPE == 'E') THEN
                        IF(CLASS_NUM+1 > 0) THEN
                           IF(TEMP_ASSET_CLASS_POINTER(CLASS_NUM+1) == 0) THEN
                              NUM_OF_BC_INIT_CLASSES = NUM_OF_BC_INIT_CLASSES + 1
                              TEMP_ASSET_CLASS_POINTER(CLASS_NUM+1) = &
                                  NUM_OF_BC_INIT_CLASSES
                        MAX_BC_INIT_CLASS_ID_NUM=MAX(CLASS_NUM+int(1,2), &
                            MAX_BC_INIT_CLASS_ID_NUM)
                           ENDIF
                        ENDIF
                     ENDIF
!
! ESTABLISH A LIST OF DEFINED CLASSES
!
                        BC_MAX_DEFINED_ID = &
                            MAX(BC_MAX_DEFINED_ID,CLASS_NUM)
                        BC_MASTER_DEFINED_CLASS_IDS(CLASS_NUM)=CLASS_NUM
                        IF(CLASS_TYPE == 'P') THEN
                           BC_PARENT_CLASS_ID_NUM = CLASS_NUM
                        ENDIF
!                     ENDIF
                     IF(CLASS_TYPE == 'E') BC_ELIMINAITON_CLASS_ID_NUM = &
                         CLASS_NUM
                  ENDIF
                  IREC = IREC + 1
                  WRITE(11,REC=IREC) DELETE,CLASS_NAME, &
                      CLASS_NUM,CLASS_TYPE,&
                      CLASS_TYPE_STR,&
                      LINKED_CLASS_ID,REPORT_CLASS, &
                    MOVE_TO_CLASS_ID,ACCOUNT_ACTIVE,COMPANY_ID, &
                    ALL_VALUES,CURRENCY_DENOMINATION,FED_TAX_EXEMPT, &
          DEACTIVATE_IN_YEAR,ACTIVATE_IN_YEAR,ACCOUNTS_RECEIVABLE, &
          ACCOUNTS_PAYABLE, &
          STD_INTEREST_PAYMENT,FUEL_INVENTORY,DEFERRED_FUEL_BALANCE, &
          OUC_BOND_SINKING_FUND_BAL, &
          OUC_DEBT_SERV_RESV_BAL,OUC_RENEWAL_REPLACEMENT_BAL, &
          OUC_OP_CONSTRUCT_FUND_BAL, &
          OUC_SP_CONSTRUCT_FUND_BAL,OUC_SR_CAPTLZED_INT_FD_BAL, &
          OUC_JR_CAPTLZED_INT_FD_BAL, &
          OUC_MINI_BOND_PRIN_SKFUND_BAL,OUC_MINI_BOND_INT_SKFUND_BAL, &
          OUC_LIABILITY_REDUCTION_BAL, &
          OUC_FUEL_INVENTORY_BAL,OUC_MATERIALS_INVENTORY_BAL, &
          OUC_PREPAID_EXP_BAL, &
          OUC_BASE_EL_STAB_FUND_BAL,ACQUISITION_PREMIUM, &
          OUC_BASE_WATER_STAB_FUND_BAL, &
          OUC_FUEL_STAB_FUND_BAL,OUC_CUST_RETENTION_FUND_BAL, &
          OUC_TTL_UNRESTR_CUR_LIAB_BAL, &
          STD_INTEREST_VECTOR,LTD_INTEREST_VECTOR,PS_DIVIDENDS_VECTOR, &
          STI_INTEREST_VECTOR, &
          LTI_INTEREST_VECTOR,CUSTOMER_DEPOSIT_INTREST_VECTOR, &
          DECOM_FUND_INTEREST_VECTOR, &
          RETIREMENT_FUND_INTEREST_VECTOR,CURRENCY_CONVERSION_VECTOR, &
          FASB_87_INTANGIBLE_ASSETS, &
          OTHER_COMPREHENSIVE_INC_BALANCE,OptRevenueTaxBasis
!

               ENDDO
               IF(IOS /= 0) EXIT
            ENDDO
            CLOSE(10)
            IF(MAX_BC_INIT_CLASS_ID_NUM > 0) THEN
               ALLOCATE(BC_INIT_CLASS_POINTER(MAX_BC_INIT_CLASS_ID_NUM))
               BC_INIT_CLASS_POINTER = &
                   TEMP_ASSET_CLASS_POINTER(1:MAX_BC_INIT_CLASS_ID_NUM)
            ENDIF
            DEALLOCATE(TEMP_ASSET_CLASS_POINTER,SBU_LINKED_COUNTER)
         ELSE
            WRITE(11,REC=1) DELETE,CLASS_NAME,CLASS_NUM,CLASS_TYPE, &
            CLASS_TYPE_STR,LINKED_CLASS_ID,REPORT_CLASS,MOVE_TO_CLASS_ID, &
              ACCOUNT_ACTIVE,COMPANY_ID,ALL_VALUES,CURRENCY_DENOMINATION, &
              FED_TAX_EXEMPT,DEACTIVATE_IN_YEAR, &
              ACTIVATE_IN_YEAR,ACCOUNTS_RECEIVABLE,ACCOUNTS_PAYABLE, &
              STD_INTEREST_PAYMENT,FUEL_INVENTORY, &
              DEFERRED_FUEL_BALANCE,OUC_BOND_SINKING_FUND_BAL, &
              OUC_DEBT_SERV_RESV_BAL, &
              OUC_RENEWAL_REPLACEMENT_BAL,OUC_OP_CONSTRUCT_FUND_BAL, &
              OUC_SP_CONSTRUCT_FUND_BAL, &
              OUC_SR_CAPTLZED_INT_FD_BAL,OUC_JR_CAPTLZED_INT_FD_BAL, &
              OUC_MINI_BOND_PRIN_SKFUND_BAL, &
              OUC_MINI_BOND_INT_SKFUND_BAL,OUC_LIABILITY_REDUCTION_BAL, &
              OUC_FUEL_INVENTORY_BAL, &
              OUC_MATERIALS_INVENTORY_BAL,OUC_PREPAID_EXP_BAL, &
              OUC_BASE_EL_STAB_FUND_BAL, &
              ACQUISITION_PREMIUM,OUC_BASE_WATER_STAB_FUND_BAL, &
              OUC_FUEL_STAB_FUND_BAL, &
              OUC_CUST_RETENTION_FUND_BAL,OUC_TTL_UNRESTR_CUR_LIAB_BAL, &
              STD_INTEREST_VECTOR, &
              LTD_INTEREST_VECTOR,PS_DIVIDENDS_VECTOR, &
              STI_INTEREST_VECTOR,LTI_INTEREST_VECTOR, &
              CUSTOMER_DEPOSIT_INTREST_VECTOR, &
              DECOM_FUND_INTEREST_VECTOR,RETIREMENT_FUND_INTEREST_VECTOR, &
              CURRENCY_CONVERSION_VECTOR,FASB_87_INTANGIBLE_ASSETS, &
              OTHER_COMPREHENSIVE_INC_BALANCE, &
              OptRevenueTaxBasis
!

         ENDIF
         CLOSE(11)

      RETURN

!
! OVERLAY THE INITIALIZATION FILE
!

      ENTRY IN_MAKEOVL(OVERLAY_FAMILY_NAME)

         DATA_DRIVE = OUTPUT_DIRECTORY()
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
            CALL LOCATE(10,51)
         ENDIF
         FILE_NAME = trim(DATA_DRIVE)//"INO"//trim(OVERLAY_FAMILY_NAME)//".DAT"
         OPEN(10,FILE=FILE_NAME)
         READ(10,*) DELETE
         INUNIT = 12
         IF(INITOL == 'BC') THEN
            OPEN(11,FILE=trim(DATA_DRIVE)//"BCINITAL.BIN", &
                ACCESS="DIRECT",RECL=LRECL)
            INUNIT = 11
         ENDIF
         OPEN(12,FILE=trim(DATA_DRIVE)//"OLINITAL.BIN", &
             ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         NUM_OF_OL_INIT_CLASSES = 0
         MAX_OL_INIT_CLASS_ID_NUM = 0
         OL_MAX_DEFINED_ID = 0
         OL_ELIMINAITON_CLASS_ID_NUM = -30000
         IF(ALLOCATED(OL_INIT_CLASS_POINTER)) then
             DEALLOCATE(OL_INIT_CLASS_POINTER)
         end if
         IF(ALLOCATED(OL_CLASS_LINKAGE)) then
             DEALLOCATE(OL_CLASS_LINKAGE)
         end if
         IF(ALLOCATED(OL_MASTER_DEFINED_CLASS_IDS)) then
             DEALLOCATE(OL_MASTER_DEFINED_CLASS_IDS)
         end if

         ALLOCATE(TEMP_ASSET_CLASS_POINTER(1024), &
             SBU_LINKED_COUNTER(1024),OL_CLASS_LINKAGE(0:1024), &
                 OL_MASTER_DEFINED_CLASS_IDS(0:1024))
         TEMP_ASSET_CLASS_POINTER = 0
         SBU_LINKED_COUNTER = 0
         OL_MASTER_DEFINED_CLASS_IDS = -99
         OL_CLASS_LINKAGE = -99
         DUPLICATE_CLASS_ID_CHECK = -99
         DUPLICATE_ID_FOUND = .FALSE.
         OL_MAX_LINKED_SBU = 0
         IREC = 0
!
         DO
            DO
               READ(10,1000,IOSTAT=IOS) RECLN
               IF(RECLN(1:1) == '7') EXIT
               IREC = IREC + 1
               READ(INUNIT,REC=IREC,IOSTAT=IOS_BASE) &
                DELETE,CLASS_NAME,CLASS_NUM,CLASS_TYPE,CLASS_TYPE_STR, &
                LINKED_CLASS_ID,REPORT_CLASS, &
                MOVE_TO_CLASS_ID,ACCOUNT_ACTIVE,COMPANY_ID,ALL_VALUES, &
                CURRENCY_DENOMINATION,FED_TAX_EXEMPT, &
                DEACTIVATE_IN_YEAR,ACTIVATE_IN_YEAR, &
                ACCOUNTS_RECEIVABLE,ACCOUNTS_PAYABLE, &
                STD_INTEREST_PAYMENT, &
                FUEL_INVENTORY,DEFERRED_FUEL_BALANCE, &
                OUC_BOND_SINKING_FUND_BAL,OUC_DEBT_SERV_RESV_BAL, &
                OUC_RENEWAL_REPLACEMENT_BAL,OUC_OP_CONSTRUCT_FUND_BAL, &
                OUC_SP_CONSTRUCT_FUND_BAL, &
                OUC_SR_CAPTLZED_INT_FD_BAL,OUC_JR_CAPTLZED_INT_FD_BAL, &
                OUC_MINI_BOND_PRIN_SKFUND_BAL, &
                OUC_MINI_BOND_INT_SKFUND_BAL, &
                OUC_LIABILITY_REDUCTION_BAL,OUC_FUEL_INVENTORY_BAL, &
                OUC_MATERIALS_INVENTORY_BAL,OUC_PREPAID_EXP_BAL, &
                OUC_BASE_EL_STAB_FUND_BAL,ACQUISITION_PREMIUM, &
                OUC_BASE_WATER_STAB_FUND_BAL,OUC_FUEL_STAB_FUND_BAL, &
                OUC_CUST_RETENTION_FUND_BAL, &
                OUC_TTL_UNRESTR_CUR_LIAB_BAL,STD_INTEREST_VECTOR, &
                LTD_INTEREST_VECTOR,PS_DIVIDENDS_VECTOR, &
                STI_INTEREST_VECTOR,LTI_INTEREST_VECTOR, &
                CUSTOMER_DEPOSIT_INTREST_VECTOR, &
                DECOM_FUND_INTEREST_VECTOR, &
                RETIREMENT_FUND_INTEREST_VECTOR, &
                CURRENCY_CONVERSION_VECTOR, &
                FASB_87_INTANGIBLE_ASSETS, &
                OTHER_COMPREHENSIVE_INC_BALANCE,OptRevenueTaxBasis
!

               IF(IOS_BASE /= 0) EXIT
               IF(IOS == 0) THEN
                  RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
                  READ(RECLN,*,ERR=200) DELETE,VALUES_27,COMMENT,YEAR, &
                  VALUES_11,TEMP_CLASS_NAME,CLASS_NUM,VALUES43_45, &
                   CLASS_TYPE_STR,VALUES47_51,LINKED_CLASS_ID, &
                   VALUES53_58,CURRENCY_DENOMINATION,REPORT_CLASS, &
                   FED_TAX_EXEMPT,DEACTIVATE_IN_YEAR,ACTIVATE_IN_YEAR, &
                   ACCOUNTS_RECEIVABLE,ACCOUNTS_PAYABLE, &
                   COMPANY_CODE,PLANT_ID,UNIT_ID,STD_INTEREST_PAYMENT, &
                   FUEL_INVENTORY,DEFERRED_FUEL_BALANCE, &
                   MOVE_TO_CLASS_ID,OUC_BOND_SINKING_FUND_BAL, &
                   OUC_DEBT_SERV_RESV_BAL,OUC_RENEWAL_REPLACEMENT_BAL, &
                   OUC_OP_CONSTRUCT_FUND_BAL,OUC_SP_CONSTRUCT_FUND_BAL, &
                   OUC_SR_CAPTLZED_INT_FD_BAL, &
                   OUC_JR_CAPTLZED_INT_FD_BAL, &
                   OUC_MINI_BOND_PRIN_SKFUND_BAL, &
                   OUC_MINI_BOND_INT_SKFUND_BAL, &
                   OUC_LIABILITY_REDUCTION_BAL, &
                   OUC_FUEL_INVENTORY_BAL, &
                   OUC_MATERIALS_INVENTORY_BAL, &
                   OUC_PREPAID_EXP_BAL, &
                   OUC_BASE_EL_STAB_FUND_BAL, &
                   ACCOUNT_ACTIVE, &
                   ACQUISITION_PREMIUM,  &
                   OUC_BASE_WATER_STAB_FUND_BAL, &
                   OUC_FUEL_STAB_FUND_BAL, &
                   OUC_CUST_RETENTION_FUND_BAL,  &
                   OUC_TTL_UNRESTR_CUR_LIAB_BAL, &
                   STD_INTEREST_VECTOR, &
                   LTD_INTEREST_VECTOR, &
                   PS_DIVIDENDS_VECTOR, &
                   STI_INTEREST_VECTOR, &
                   LTI_INTEREST_VECTOR, &
                   CUSTOMER_DEPOSIT_INTREST_VECTOR,  &
                   DECOM_FUND_INTEREST_VECTOR, &
                   RETIREMENT_FUND_INTEREST_VECTOR, &
                   CURRENCY_CONVERSION_VECTOR, &
                   FASB_87_INTANGIBLE_ASSETS, &
                   OTHER_COMPREHENSIVE_INC_BALANCE, &
                   OptRevenueTaxBasis, &
                   COMPANY_ID     ! 105
!
                  CLASS_TYPE = CLASS_TYPE_STR(1:1)
               ENDIF
               IF(DELETE >= 8 .OR. ACCOUNT_ACTIVE == 'N' .OR. CLASS_TYPE == 'N') THEN
                  IF(DUPLICATE_CLASS_ID_CHECK(CLASS_NUM) == -99) THEN
                     IF(CLASS_NUM+1 > 0) THEN
                        IF(TEMP_ASSET_CLASS_POINTER(CLASS_NUM+1)==0)THEN
                           NUM_OF_OL_INIT_CLASSES = NUM_OF_OL_INIT_CLASSES + 1
                           TEMP_ASSET_CLASS_POINTER(CLASS_NUM+1) = &
                            NUM_OF_OL_INIT_CLASSES
                      MAX_OL_INIT_CLASS_ID_NUM = MAX(CLASS_NUM+int(1,2), &
                        MAX_OL_INIT_CLASS_ID_NUM)
                        ENDIF
                     ENDIF
                  ENDIF
               else
                  IF(DUPLICATE_CLASS_ID_CHECK(CLASS_NUM) == -99) THEN
                     IF(CLASS_TYPE /=  'N') THEN
                        DUPLICATE_CLASS_ID_CHECK(CLASS_NUM) = 1
                     ENDIF
                  ELSEIF(CLASS_TYPE /=  'N') THEN
                     WRITE(4,*) 'Duplicate Class ID found for', &
                         CLASS_NUM,' ',CLASS_NAME
                     DUPLICATE_ID_FOUND = .TRUE.
                  ENDIF
                  IF(CLASS_TYPE /= 'E') THEN
                     IF(CLASS_NUM+1 > 0) THEN
                        IF(TEMP_ASSET_CLASS_POINTER(CLASS_NUM+1) == 0) THEN
                           NUM_OF_OL_INIT_CLASSES = NUM_OF_OL_INIT_CLASSES + 1
                           TEMP_ASSET_CLASS_POINTER(CLASS_NUM+1) = NUM_OF_OL_INIT_CLASSES
                      MAX_OL_INIT_CLASS_ID_NUM = MAX(CLASS_NUM+int(1,2),  &
                        MAX_OL_INIT_CLASS_ID_NUM)
                        ENDIF
                     ENDIF

                     IF(CLASS_TYPE == 'P') THEN

                        PARENT_CLASS_ID_NUM = CLASS_NUM ! + 1
                     ELSE

                        IF(LINKED_CLASS_ID+1 > 0) THEN
              OL_CLASS_LINKAGE(NUM_OF_OL_INIT_CLASSES) = LINKED_CLASS_ID
                           OL_CLASS_TYPE(NUM_OF_OL_INIT_CLASSES) = CLASS_TYPE
                           SBU_LINKED_COUNTER(LINKED_CLASS_ID+1) = 1 +  &
                            SBU_LINKED_COUNTER(LINKED_CLASS_ID+1)
                           OL_MAX_LINKED_SBU = MAX(OL_MAX_LINKED_SBU, &
                            SBU_LINKED_COUNTER(LINKED_CLASS_ID+1))
                        ENDIF
                     ENDIF
                  ELSEIF(CLASS_TYPE == 'E') THEN
                     IF(CLASS_NUM+1 > 0) THEN
                        IF(TEMP_ASSET_CLASS_POINTER(CLASS_NUM+1) == 0) THEN
                           NUM_OF_OL_INIT_CLASSES = NUM_OF_OL_INIT_CLASSES + 1
                           TEMP_ASSET_CLASS_POINTER(CLASS_NUM+1) = &
                            NUM_OF_OL_INIT_CLASSES
                      MAX_OL_INIT_CLASS_ID_NUM = MAX(CLASS_NUM+int(1,2), &
                        MAX_OL_INIT_CLASS_ID_NUM)
                        ENDIF
                     ENDIF
                  ENDIF
!
! ESTABLISH A LIST OF DEFINED CLASSES
!
                     OL_MAX_DEFINED_ID=MAX(OL_MAX_DEFINED_ID,CLASS_NUM)
                     OL_MASTER_DEFINED_CLASS_IDS(CLASS_NUM) = CLASS_NUM
                     IF(CLASS_TYPE == 'P') THEN
                        OL_PARENT_CLASS_ID_NUM = CLASS_NUM
                     ENDIF

                  IF(CLASS_TYPE == 'E') then 
                    OL_ELIMINAITON_CLASS_ID_NUM = CLASS_NUM
                  end if
               ENDIF
               WRITE(12,REC=IREC) DELETE, &
                    CLASS_NAME, &
                    CLASS_NUM, &
                    CLASS_TYPE, &
                    CLASS_TYPE_STR, &
                    LINKED_CLASS_ID, &
                    REPORT_CLASS, &
                    MOVE_TO_CLASS_ID, &
                    ACCOUNT_ACTIVE, &
                    COMPANY_ID, &
                    ALL_VALUES, &
                    CURRENCY_DENOMINATION, &
                    FED_TAX_EXEMPT, &
                    DEACTIVATE_IN_YEAR, &
                    ACTIVATE_IN_YEAR, &
                    ACCOUNTS_RECEIVABLE, &
                    ACCOUNTS_PAYABLE, &
                    STD_INTEREST_PAYMENT, &
                    FUEL_INVENTORY, &
                    DEFERRED_FUEL_BALANCE, &
                    OUC_BOND_SINKING_FUND_BAL, &
                    OUC_DEBT_SERV_RESV_BAL, &
                    OUC_RENEWAL_REPLACEMENT_BAL, &
                    OUC_OP_CONSTRUCT_FUND_BAL, &
                    OUC_SP_CONSTRUCT_FUND_BAL, &
                    OUC_SR_CAPTLZED_INT_FD_BAL, &
                    OUC_JR_CAPTLZED_INT_FD_BAL, &
                    OUC_MINI_BOND_PRIN_SKFUND_BAL, &
                    OUC_MINI_BOND_INT_SKFUND_BAL, &
                    OUC_LIABILITY_REDUCTION_BAL, &
                    OUC_FUEL_INVENTORY_BAL, &
                    OUC_MATERIALS_INVENTORY_BAL, &
                    OUC_PREPAID_EXP_BAL, &
                    OUC_BASE_EL_STAB_FUND_BAL, &
                    ACQUISITION_PREMIUM, &
                    OUC_BASE_WATER_STAB_FUND_BAL, &
                    OUC_FUEL_STAB_FUND_BAL, &
                    OUC_CUST_RETENTION_FUND_BAL, &
                    OUC_TTL_UNRESTR_CUR_LIAB_BAL, &
                    STD_INTEREST_VECTOR, &
                    LTD_INTEREST_VECTOR, &
                    PS_DIVIDENDS_VECTOR, &
                    STI_INTEREST_VECTOR, &
                    LTI_INTEREST_VECTOR, &
                    CUSTOMER_DEPOSIT_INTREST_VECTOR, &
                    DECOM_FUND_INTEREST_VECTOR, &
                    RETIREMENT_FUND_INTEREST_VECTOR, &
                    CURRENCY_CONVERSION_VECTOR, &
                    FASB_87_INTANGIBLE_ASSETS, &
                    OTHER_COMPREHENSIVE_INC_BALANCE, &
                    OptRevenueTaxBasis
!

            ENDDO
            IF(IOS_BASE /= 0) EXIT
         ENDDO
         CLOSE(10)
         CLOSE(12)
         IF(MAX_OL_INIT_CLASS_ID_NUM > 0) THEN
            ALLOCATE(OL_INIT_CLASS_POINTER(MAX_OL_INIT_CLASS_ID_NUM))
            OL_INIT_CLASS_POINTER = &
                TEMP_ASSET_CLASS_POINTER(1:MAX_OL_INIT_CLASS_ID_NUM)
         ENDIF
         DEALLOCATE(TEMP_ASSET_CLASS_POINTER,SBU_LINKED_COUNTER)
         IF(INITOL == 'BC') CLOSE(11)
         INITOL = 'OL'

      RETURN
!

      ENTRY MUNI_IN_MAKEBIN

         CALL MG_LOCATE_WRITE(16,30,INITFIL(),ALL_VERSIONS,0)
         CALL MG_CLEAR_LINE_WRITE(17,9,36,MUNI_FILE_TYPE,ALL_VERSIONS,0)
         FILE_NAME = get_inb_filename()
         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCINITAL.BIN", &
            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         DELETE= 1
         YEAR = get_base_year()
         TOTAL_RESERVE_BALANCE = 0.
         FIXED_ASSETS = 0.
         SYSTEM_AVERAGE_RATE = 0.
         SPECIAL_RESERVE_BALANCE = 0.
         LAST_YEAR_SALES_REVENUE = 0.
         CALAVERAS_RESERVE_BALANCE = 0.
         WRITE(11,REC=1) DELETE,YEAR,TOTAL_RESERVE_BALANCE,FIXED_ASSETS, &
            SYSTEM_AVERAGE_RATE,SPECIAL_RESERVE_BALANCE, &
             LAST_YEAR_SALES_REVENUE,CALAVERAS_RESERVE_BALANCE
         IF(FILE_EXISTS) THEN
                OPEN(10,FILE=FILE_NAME)
            READ(10,*) DELETE
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(IOS == 0) THEN
                RECLN = trim(RECLN)//',,,,,,,,,,,,,,'
                READ(RECLN,*,ERR=200) DELETE,YEAR,TOTAL_RESERVE_BALANCE, &
                FIXED_ASSETS,SYSTEM_AVERAGE_RATE,SPECIAL_RESERVE_BALANCE, &
                     LAST_YEAR_SALES_REVENUE,CALAVERAS_RESERVE_BALANCE
                ENDIF
            CLOSE(10)
         ENDIF
         WRITE(11,REC=1) DELETE,YEAR,TOTAL_RESERVE_BALANCE,FIXED_ASSETS, &
            SYSTEM_AVERAGE_RATE,SPECIAL_RESERVE_BALANCE, &
             LAST_YEAR_SALES_REVENUE,CALAVERAS_RESERVE_BALANCE
         CLOSE(11)

      RETURN

! MUNICIPAL OVERLAY

       ENTRY MUNI_IN_MAKEOVL(OVERLAY_FAMILY_NAME)
       DATA_DRIVE = OUTPUT_DIRECTORY()
         CALL MG_CLEAR_LINE_WRITE(17,9,36,MUNI_FILE_TYPE,ALL_VERSIONS,0)
         FILE_NAME = get_ino_filename(data_drive, overlay_family_name)

         OPEN(10,FILE=FILE_NAME)
         READ(10,*) DELETE
         INUNIT = 12
         IF(INITOL == 'BC') THEN
            OPEN(11,FILE=trim(DATA_DRIVE)//"BCINITAL.BIN",ACCESS="DIRECT" &
                ,STATUS="UNKNOWN",RECL=LRECL)
            INUNIT = 11
         ENDIF
         OPEN(12,FILE=DATA_DRIVE//"OLINITAL.BIN",ACCESS="DIRECT", &
            STATUS="UNKNOWN",RECL=LRECL)
         READ(INUNIT,REC=1) DELETE,YEAR,MUNI_INIT_VALUES
         READ(10,1000,IOSTAT=IOS) RECLN
         IF(IOS == 0) THEN
            RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
            READ(RECLN,*,ERR=200) DELETE,YEAR,MUNI_INIT_VALUES
            WRITE(12,REC=1) DELETE,YEAR,MUNI_INIT_VALUES
         ENDIF
         CLOSE(10)
         CLOSE(12)
         IF(INITOL == 'BC') CLOSE(11)
         INITOL = 'OL'
      RETURN
!
  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from In_objt SIID182'
      call end_program(er_message)
!

      ENTRY RESET_INITOL

         INITOL = 'BC'
      RETURN

      ENTRY GET_MAX_LINKED_SBUS(R_MAX_LINKED_SBU,R_MAX_INIT_CLASS_NUM)

         IF(INITOL == 'OL') THEN
            R_MAX_INIT_CLASS_NUM = MAX_OL_INIT_CLASS_ID_NUM
            R_MAX_LINKED_SBU = OL_MAX_LINKED_SBU
         ELSE
            R_MAX_INIT_CLASS_NUM = MAX_BC_INIT_CLASS_ID_NUM
            R_MAX_LINKED_SBU = BC_MAX_LINKED_SBU
         ENDIF
      RETURN

      ENTRY OPEN_INITIZATION_FILE

         INQUIRE(UNIT=10,OPENED=UNIT_10_OPEN)
         OPEN(10,FILE=trim(OUTPUT_DIRECTORY())//INITOL// &
            "INITAL.BIN",ACCESS="DIRECT",RECL=LRECL)
      RETURN

      ENTRY CLOSE_INIIZATION_FILE

         CLOSE(10)
      RETURN

      ENTRY RETURN_INITIALIZATION_CLASSES(R_NUM_OF_INIT_CLASSES, &
        R_MAX_INIT_CLASS_NUM)

         IF(INITOL == 'OL') THEN
            R_NUM_OF_INIT_CLASSES = NUM_OF_OL_INIT_CLASSES
            R_MAX_INIT_CLASS_NUM = MAX_OL_INIT_CLASS_ID_NUM
         ELSE
            R_NUM_OF_INIT_CLASSES = NUM_OF_BC_INIT_CLASSES
            R_MAX_INIT_CLASS_NUM = MAX_BC_INIT_CLASS_ID_NUM
         ENDIF
      RETURN

      ENTRY RETURN_INITIALIZATION_POINTER(R_INIT_CLASS_POINTERS)

         IF(INITOL == 'OL') THEN

               R_INIT_CLASS_POINTERS(1:MAX_OL_INIT_CLASS_ID_NUM) = &
                OL_INIT_CLASS_POINTER
         ELSE

               R_INIT_CLASS_POINTERS(1:MAX_BC_INIT_CLASS_ID_NUM) = &
                BC_INIT_CLASS_POINTER 
         ENDIF
      RETURN

      ENTRY GET_LINKED_CLASS(R_ASSET_CLASS,R_LINKED_CLASS)

         R_LINKED_CLASS = -99.
         IF(.NOT.((INITOL == 'OL' .AND. R_ASSET_CLASS == &
            OL_ELIMINAITON_CLASS_ID_NUM) .OR. (INITOL == 'BC' .AND. &
                   R_ASSET_CLASS == BC_ELIMINAITON_CLASS_ID_NUM))) THEN
!
            IF(INITOL == 'OL' .AND. R_ASSET_CLASS+1 <=  &
                MAX_OL_INIT_CLASS_ID_NUM .AND. &
                    ALLOCATED(OL_INIT_CLASS_POINTER)) THEN
               POINTR = OL_INIT_CLASS_POINTER(R_ASSET_CLASS+1)
               R_LINKED_CLASS = OL_CLASS_LINKAGE(POINTR)
            ELSEIF(INITOL == 'BC' .AND. R_ASSET_CLASS+1 <= &
                MAX_BC_INIT_CLASS_ID_NUM .AND.  &
                ALLOCATED(BC_INIT_CLASS_POINTER)) THEN
               POINTR = BC_INIT_CLASS_POINTER(R_ASSET_CLASS+1)
               R_LINKED_CLASS = BC_CLASS_LINKAGE(POINTR)
            ENDIF
            IF(R_LINKED_CLASS == R_ASSET_CLASS) R_LINKED_CLASS = -99
         ENDIF
      RETURN

      ENTRY GET_DEBT_LINKED_CLASS(R_ASSET_CLASS,R_LINKED_CLASS)

         R_LINKED_CLASS = -99.
         IF(INITOL == 'OL' .AND. R_ASSET_CLASS+1 <= &
            MAX_OL_INIT_CLASS_ID_NUM .AND. &
            ALLOCATED(OL_INIT_CLASS_POINTER)) THEN
                IF(R_ASSET_CLASS == OL_ELIMINAITON_CLASS_ID_NUM) THEN
                    RETURN
                end IF
            POINTR = OL_INIT_CLASS_POINTER(R_ASSET_CLASS+1)
            R_LINKED_CLASS = OL_CLASS_LINKAGE(POINTR)
            CLASS_TYPE = OL_CLASS_TYPE(POINTR)
         ELSEIF(INITOL == 'BC' .AND. R_ASSET_CLASS+1 <= &
            MAX_BC_INIT_CLASS_ID_NUM .AND. &
                ALLOCATED(BC_INIT_CLASS_POINTER)) THEN
            IF(R_ASSET_CLASS == BC_ELIMINAITON_CLASS_ID_NUM) RETURN
            POINTR = BC_INIT_CLASS_POINTER(R_ASSET_CLASS+1)
            R_LINKED_CLASS = BC_CLASS_LINKAGE(POINTR)
            CLASS_TYPE = BC_CLASS_TYPE(POINTR)
         ENDIF
         IF(R_LINKED_CLASS == R_ASSET_CLASS .OR. CLASS_TYPE == 'S' .OR. &
            CLASS_TYPE == 'N') R_LINKED_CLASS = -99
      RETURN

      ENTRY CHECK_IF_CLASS_DEFINED(R_ASSET_CLASS)

         IF(INITOL == 'OL') THEN
            IF(R_ASSET_CLASS >= 0 .AND. &
                ALLOCATED(OL_MASTER_DEFINED_CLASS_IDS) .AND. &
                    R_ASSET_CLASS <= OL_MAX_DEFINED_ID) THEN
               R_ASSET_CLASS=OL_MASTER_DEFINED_CLASS_IDS(R_ASSET_CLASS)
               IF(R_ASSET_CLASS == -99 .OR. REALLY_KEPCO) R_ASSET_CLASS &
                = OL_PARENT_CLASS_ID_NUM
            ELSE
               R_ASSET_CLASS = OL_PARENT_CLASS_ID_NUM
            ENDIF
         ELSE
            IF(R_ASSET_CLASS >= 0 .AND. &
                ALLOCATED(BC_MASTER_DEFINED_CLASS_IDS) .AND. &
                    R_ASSET_CLASS <= BC_MAX_DEFINED_ID) THEN
               R_ASSET_CLASS=BC_MASTER_DEFINED_CLASS_IDS(R_ASSET_CLASS)
               IF(R_ASSET_CLASS == -99 .OR. REALLY_KEPCO) R_ASSET_CLASS = &
                BC_PARENT_CLASS_ID_NUM
            ELSE
               R_ASSET_CLASS = BC_PARENT_CLASS_ID_NUM
            ENDIF
         ENDIF
      RETURN

      ENTRY PARENT_CLASS_ID(R_PARENT_CLASS_ID)

         R_PARENT_CLASS_ID = PARENT_CLASS_ID_NUM
      RETURN

      ENTRY PARENT_CLASS_ID_LOCATION(R_PARENT_CLASS_ID)

         IF(INITOL == 'OL') THEN
            R_PARENT_CLASS_ID = OL_INIT_CLASS_POINTER(PARENT_CLASS_ID_NUM+1)
         ELSE
            R_PARENT_CLASS_ID = BC_INIT_CLASS_POINTER(PARENT_CLASS_ID_NUM+1)
         ENDIF
      RETURN

      ENTRY ELIMINATION_CLASS_ID(R_ELIMINAITON_CLASS_ID_NUM)

         IF(INITOL == 'OL') THEN
            R_ELIMINAITON_CLASS_ID_NUM = &
 !                  +1 TO BE CONSISTENT WITH CLASS POSITIONS
                OL_ELIMINAITON_CLASS_ID_NUM+1 
         ELSE
            R_ELIMINAITON_CLASS_ID_NUM = BC_ELIMINAITON_CLASS_ID_NUM+1
         ENDIF
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END

      FUNCTION INIT_FILE_IS_ACTIVE()
      use dsex_obj_interfaces
      implicit none
      LOGICAL (KIND=1) :: INIT_FILE_IS_ACTIVE
      LOGICAL (KIND=4) :: FILE_EXISTS
      CHARACTER (LEN=256) :: FILE_NAME,BASE_FILE_DIRECTORY
      CHARACTER (LEN=5) :: INITFIL

         FILE_NAME = trim(BASE_FILE_DIRECTORY())//"INB"// &
            trim(INITFIL())//".DAT"
         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
         INIT_FILE_IS_ACTIVE = (FILE_EXISTS .AND. &
            .NOT. SP_CAPEX_ACTIVE())
      RETURN
      END
