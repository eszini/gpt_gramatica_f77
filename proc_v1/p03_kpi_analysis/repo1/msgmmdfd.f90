
!     msgmmdfd.for
!     Copyright(c)  2000
!
!     Created: 7/6/2003 3:36:13 PM
!     Author : MARK S GERBER
!     Last change: MSG 1/10/2010 2:59:53 PM


! Last change: MSG 7/1/2003 12:46:31 PM

      SUBROUTINE DD_OBJECT

!
      use spindriftlib
      use prod_arrays_dimensions
      USE SIZECOM
      use end_routine
      use logging
      use string
      implicit none
      
      INTEGER(kind=2) :: DELETE,IREC,INUNIT,LRECL=136,I
      INTEGER :: ACCTNO
      INTEGER(kind=4) :: IOS,IOS_BASE
      integer :: iostatus
      INTEGER(kind=2) :: NUMBER_OF_BC_DEBIT_CLASSES=0, &
      	MAX_BC_DEBIT_CLASS_ID_NUM=0
      INTEGER(kind=2) :: NUMBER_OF_OL_DEBIT_CLASSES=0, &
      	MAX_OL_DEBIT_CLASS_ID_NUM=0
      INTEGER(kind=2) :: UNIT_NUM=10,ASSET_CLASS_NUM,ASSET_CLASS_VECTOR
      INTEGER(kind=2) :: R_NUM_OF_DEBIT_CLASSES,R_MAX_DEBIT_CLASS_NUM, &
      	R_DEBIT_CLASS_POINTERS(*)
      INTEGER(kind=2) :: R_UNIT_NUM,YEAR_DEBIT_BOOKED,MONTH_DEBIT_BOOKED
      CHARACTER(len=5) :: BASE_FILE_NAME,OVERLAY_FAMILY_NAME,DEBITFIL
      CHARACTER(len=256) :: FILE_NAME
      CHARACTER(len=256) :: BASE_FILE_DIRECTORY,OUTPUT_DIRECTORY
      CHARACTER(len=256) :: DATA_DRIVE
      CHARACTER(len=20) :: WVPA_TRACKING_TYPE
      CHARACTER(len=35) :: DESC,TEMP_DESC
      CHARACTER(len=1) :: TAX_TIMING_CLASSIFICATION, &
      	ACCOUNT_ACTIVE,DEFERRAL_TYPE
      CHARACTER(len=50) :: COMMENT
      LOGICAL(kind=4) :: FILE_EXISTS
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER(len=1024) :: RECLN
! DECLARATION FOR /DEBIT FILE/
      CHARACTER(len=1) :: INCOME_TREATMENT
      REAL(kind=4) :: AMORT_DELAY_PERIOD
      REAL(kind=4) :: DEFERRED_TAX_ALLOCATION, &
      	CURRENT_TAX_DEDUCTION_PERCENT
      REAL(kind=4) :: DEBAMT,CAMORT,WOYRS
      CHARACTER(len=23) :: FILE_TYPE='Deferred Debit Accounts'
      CHARACTER(len=1) :: METHOD
      CHARACTER(len=2) :: DEBITOL='BC',R_DEBITOL
      CHARACTER(len=8) :: BALANCE_SHEET_TREATMENT
      INTEGER(kind=2) :: DEBIT_BC_ASSET_CLASS_POINTER(:), &
                         DEBIT_OL_ASSET_CLASS_POINTER(:), &
                         TEMP_ASSET_CLASS_POINTER(:)

      ALLOCATABLE :: DEBIT_OL_ASSET_CLASS_POINTER, &
                     DEBIT_BC_ASSET_CLASS_POINTER, &
                     TEMP_ASSET_CLASS_POINTER
      LOGICAL(kind=1) :: LAHEY_LF95
      CHARACTER(len=30) :: SCREEN_OUTPUT
      SAVE DEBIT_BC_ASSET_CLASS_POINTER, &
           DEBIT_OL_ASSET_CLASS_POINTER


!
!          ROUTINE TO CONVERT METAFILE FILES TO DIRECT-ACESS BINARY
!          COPYRIGHT (C) 1983, 84, 85  M.S. GERBER & ASSOCIATES, INC.
!

!
! CONVERT THE DEBIT FILE

      ENTRY DD_MAKEBIN

      BASE_FILE_NAME = DEBITFIL()
      FILE_NAME = trim(BASE_FILE_DIRECTORY())//"DDB"// &
      	trim(BASE_FILE_NAME)//".DAT"
      DATA_DRIVE = OUTPUT_DIRECTORY()
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
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
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCDEBIT.BIN",ACCESS="DIRECT", &
         	STATUS="UNKNOWN",RECL=LRECL)
         IREC = 0
         READ(10,*) DELETE
         DO
            ASSET_CLASS_NUM = 0
            ASSET_CLASS_VECTOR = 0
            INCOME_TREATMENT = "A"
            AMORT_DELAY_PERIOD = 0.
            DEFERRED_TAX_ALLOCATION = 0.
            CURRENT_TAX_DEDUCTION_PERCENT = 0.
            YEAR_DEBIT_BOOKED = 1990
            MONTH_DEBIT_BOOKED = 7
            TAX_TIMING_CLASSIFICATION = 'T'
            BALANCE_SHEET_TREATMENT = 'Deferred'
            ACCOUNT_ACTIVE = 'A'
            DEFERRAL_TYPE = 'D'
            WVPA_TRACKING_TYPE = 'Not Tracked'
            DO
               READ(10,'(A)',IOSTAT=IOS) RECLN
               IF(IOS /=0) EXIT
               IF(RECLN(1:1) == '7') EXIT
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,ERR=200) DELETE,ACCTNO,DEBAMT,CAMORT,WOYRS, &
                                    METHOD,DESC,COMMENT, &
                                    ASSET_CLASS_NUM,ASSET_CLASS_VECTOR, &
                                    INCOME_TREATMENT,AMORT_DELAY_PERIOD, &
                                    DEFERRED_TAX_ALLOCATION, &
                                    YEAR_DEBIT_BOOKED, &
                                    TAX_TIMING_CLASSIFICATION, &
                                    CURRENT_TAX_DEDUCTION_PERCENT, &
                                    MONTH_DEBIT_BOOKED, &
                                    BALANCE_SHEET_TREATMENT, &
                                    ACCOUNT_ACTIVE, &
                                    DEFERRAL_TYPE, &
                                    WVPA_TRACKING_TYPE
!
             IF(.NOT. (DELETE >= 8 .OR. ACCOUNT_ACTIVE == 'N')) &
                     CALL SET_ASSET_CLASSES(ASSET_CLASS_NUM, &
                                            NUMBER_OF_BC_DEBIT_CLASSES, &
                                            MAX_BC_DEBIT_CLASS_ID_NUM, &
                                            TEMP_ASSET_CLASS_POINTER)
               IREC = IREC + 1
               WRITE(11,REC=IREC)DELETE,DESC,ACCTNO,DEBAMT,CAMORT,WOYRS, &
                                 METHOD,ASSET_CLASS_NUM, &
                                 ASSET_CLASS_VECTOR, &
                                 INCOME_TREATMENT,AMORT_DELAY_PERIOD, &
                                 DEFERRED_TAX_ALLOCATION, &
                                 YEAR_DEBIT_BOOKED, &
                                 TAX_TIMING_CLASSIFICATION, &
                                 CURRENT_TAX_DEDUCTION_PERCENT, &
                                 MONTH_DEBIT_BOOKED, &
                                 BALANCE_SHEET_TREATMENT, &
                                 ACCOUNT_ACTIVE, &
                                 DEFERRAL_TYPE, &
                                 WVPA_TRACKING_TYPE 
            ENDDO
            IF(IOS /= 0) EXIT
         ENDDO
         CLOSE(10)
!
         CLOSE(11)
         IF(MAX_BC_DEBIT_CLASS_ID_NUM > 0) THEN
            ALLOCATE(DEBIT_BC_ASSET_CLASS_POINTER(MAX_BC_DEBIT_CLASS_ID_NUM))
            DO I = 1, MAX_BC_DEBIT_CLASS_ID_NUM
               DEBIT_BC_ASSET_CLASS_POINTER(I) = TEMP_ASSET_CLASS_POINTER(I)
            ENDDO
! 
         ENDIF
         DEALLOCATE(TEMP_ASSET_CLASS_POINTER)
      ELSE IF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "msgmmdfd:1111")
      ENDIF
      RETURN


! ROUTINE TO CREATE OVERLAY FILES
! OVERLAY THE DEBIT FILE

      ENTRY DD_MAKEOVL(OVERLAY_FAMILY_NAME)

      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME=trim(DATA_DRIVE)//"DDO"//trim(OVERLAY_FAMILY_NAME)//".DAT"
      OPEN(10,FILE=FILE_NAME)
      READ(10,*) DELETE
      INUNIT = 12
      IF(DEBITOL == 'BC') THEN
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCDEBIT.BIN",ACCESS="DIRECT",RECL=LRECL)
         INUNIT = 11
      ENDIF
      OPEN(12,FILE=trim(DATA_DRIVE)//"OLDEBIT.BIN",ACCESS="DIRECT", &
      	STATUS="UNKNOWN",RECL=LRECL)
      ALLOCATE(TEMP_ASSET_CLASS_POINTER(1024))
      TEMP_ASSET_CLASS_POINTER = 0
      NUMBER_OF_OL_DEBIT_CLASSES = 0
      MAX_OL_DEBIT_CLASS_ID_NUM = 0
      IREC = 0
      DO
         DO
            READ(10,'(A)',IOSTAT=IOS) RECLN
            IF(RECLN(1:1) == '7') EXIT
            IREC = IREC + 1
            READ(INUNIT,REC=IREC,IOSTAT=IOS_BASE) DELETE,DESC, &
                                         ACCTNO,DEBAMT, &
                                         CAMORT,WOYRS,METHOD, &
                                         ASSET_CLASS_NUM, &
                                         ASSET_CLASS_VECTOR, &
                                         INCOME_TREATMENT, &
                                         AMORT_DELAY_PERIOD, &
                                         DEFERRED_TAX_ALLOCATION, &
                                         YEAR_DEBIT_BOOKED, &
                                         TAX_TIMING_CLASSIFICATION, &
                                         CURRENT_TAX_DEDUCTION_PERCENT, &
                                         MONTH_DEBIT_BOOKED, &
                                         BALANCE_SHEET_TREATMENT, &
                                         ACCOUNT_ACTIVE, &
                                         DEFERRAL_TYPE, &
                                         WVPA_TRACKING_TYPE
            IF(IOS_BASE /= 0) EXIT
            IF(IOS == 0) THEN
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,ERR=200) DELETE,ACCTNO,DEBAMT,CAMORT, &
                                     WOYRS,METHOD,TEMP_DESC,COMMENT, &
                                     ASSET_CLASS_NUM,ASSET_CLASS_VECTOR, &
                                     INCOME_TREATMENT,AMORT_DELAY_PERIOD, &
                                     DEFERRED_TAX_ALLOCATION, &
                                     YEAR_DEBIT_BOOKED, &
                                     TAX_TIMING_CLASSIFICATION, &
                                     CURRENT_TAX_DEDUCTION_PERCENT, &
                                     MONTH_DEBIT_BOOKED, &
                                     BALANCE_SHEET_TREATMENT, &
                                     ACCOUNT_ACTIVE, &
                                     DEFERRAL_TYPE, &
                                     WVPA_TRACKING_TYPE
            ENDIF
            IF(.NOT. (DELETE >= 8 .OR. ACCOUNT_ACTIVE == 'N')) &
                     CALL SET_ASSET_CLASSES(ASSET_CLASS_NUM, &
                                            NUMBER_OF_OL_DEBIT_CLASSES, &
                                            MAX_OL_DEBIT_CLASS_ID_NUM, &
                                            TEMP_ASSET_CLASS_POINTER)
            WRITE(12,REC=IREC) DELETE,DESC,ACCTNO,DEBAMT, &
                               CAMORT,WOYRS,METHOD, &
                               ASSET_CLASS_NUM,ASSET_CLASS_VECTOR, &
                               INCOME_TREATMENT,AMORT_DELAY_PERIOD, &
                               DEFERRED_TAX_ALLOCATION, &
                               YEAR_DEBIT_BOOKED, &
                               TAX_TIMING_CLASSIFICATION, &
                               CURRENT_TAX_DEDUCTION_PERCENT, &
                               MONTH_DEBIT_BOOKED, &
                               BALANCE_SHEET_TREATMENT, &
                               ACCOUNT_ACTIVE, &
                               DEFERRAL_TYPE, &
                               WVPA_TRACKING_TYPE
         ENDDO
         IF(IOS_BASE /= 0) EXIT
      ENDDO
      CLOSE(10)
      CLOSE(12)
      IF(DEBITOL == 'BC') CLOSE(11)
      DEBITOL = 'OL'
      IF(ALLOCATED(DEBIT_OL_ASSET_CLASS_POINTER)) then
      	DEALLOCATE(DEBIT_OL_ASSET_CLASS_POINTER)
      end if
      IF(MAX_OL_DEBIT_CLASS_ID_NUM > 0) THEN
         ALLOCATE(DEBIT_OL_ASSET_CLASS_POINTER &
         	(MAX_OL_DEBIT_CLASS_ID_NUM))
         DO I = 1, MAX_OL_DEBIT_CLASS_ID_NUM
            DEBIT_OL_ASSET_CLASS_POINTER(I)=TEMP_ASSET_CLASS_POINTER(I)
         ENDDO

      ENDIF
      DEALLOCATE(TEMP_ASSET_CLASS_POINTER)
      RETURN

  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      call end_program("msgmmdfd:0004") ! Unknown reason


      ENTRY RESET_DEBITOL

         DEBITOL = 'BC'
      RETURN


      ENTRY GET_DEBIT_OL(R_DEBITOL)

         R_DEBITOL = DEBITOL
      RETURN


      ENTRY OPEN_DD_BASE_CASE_FILE(R_UNIT_NUM)
         file_name=trim(OUTPUT_DIRECTORY())// &
         	'BC_DEBIT.BIN'
          call write_log_entry("msgmmdfd:0002", "Opening " // &
            "unit " // trim(itos(int(r_unit_num))) // " for " // &
         trim(file_name) // ".")
         OPEN(R_UNIT_NUM,FILE=file_name,FORM='UNFORMATTED', &
         iostat=iostatus)
       
         if(iostatus/=0) then
            er_message="msgmmdfd:0001 - IOS=" // trim(itos(iostatus)) // &
            " when opening " // trim (file_name) 
            call end_program(er_message)
            
         end if
                           
         call write_log_entry("msgmmdfd:0003", "Opened " // &
            trim(file_name) // ".")
            

      RETURN

      ENTRY OPEN_DD_OUT_FILE(R_UNIT_NUM)

         OPEN(R_UNIT_NUM,FILE=trim(OUTPUT_DIRECTORY())//DEBITOL// &
         	'DD_AST.BIN',ACCESS='DIRECT',RECL=32)
      RETURN
!

      ENTRY OPEN_DEFERRED_DEBIT_FILE(R_UNIT_NUM)

         OPEN(R_UNIT_NUM,FILE=trim(OUTPUT_DIRECTORY())//DEBITOL// &
                "DEBIT.BIN",ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         UNIT_NUM = R_UNIT_NUM
      RETURN
!

      ENTRY CLOSE_DEFERRED_DEBIT_FILE

         CLOSE(UNIT_NUM)
      RETURN
!

      ENTRY RETURN_NUM_OF_DEBIT_CLASSES(R_NUM_OF_DEBIT_CLASSES, &
                                        R_MAX_DEBIT_CLASS_NUM)

      IF(DEBITOL == 'OL') THEN
         R_NUM_OF_DEBIT_CLASSES = NUMBER_OF_OL_DEBIT_CLASSES
         R_MAX_DEBIT_CLASS_NUM = MAX_OL_DEBIT_CLASS_ID_NUM
      ELSE         
         R_NUM_OF_DEBIT_CLASSES = NUMBER_OF_BC_DEBIT_CLASSES
         R_MAX_DEBIT_CLASS_NUM = MAX_BC_DEBIT_CLASS_ID_NUM
      ENDIF
      RETURN

      ENTRY RETURN_DEBIT_CLASS_POINTER(R_DEBIT_CLASS_POINTERS)

      IF(DEBITOL == 'OL') THEN
         DO I = 1, MAX_OL_DEBIT_CLASS_ID_NUM
            R_DEBIT_CLASS_POINTERS(I) = DEBIT_OL_ASSET_CLASS_POINTER(I)
         ENDDO

      ELSE
         DO I = 1, MAX_BC_DEBIT_CLASS_ID_NUM
            R_DEBIT_CLASS_POINTERS(I) = DEBIT_BC_ASSET_CLASS_POINTER(I)
         ENDDO

      ENDIF
      RETURN
 1000 FORMAT('&',A)
      END

!*                                                                     *
!*                         DEFERRED DEBITS                             *
!*                                                                     *
!*                     COPYRIGHT (C) 1982, 1994, 1999                  *
!*                     M.S. GERBER & ASSOCIATES, INC                   *
!*                         ALL RIGHTS RESERVED                         *
!*                                                                     *

!*                                                                     *
!*    PURPOSE:                                                         *
!*                                                                     *
!*       DEFDEB CALCULATES THE ANNUAL AMORTIZATION EXPENSE OF          *
!*       DEFERRED DEBITS AND THE YEAR END BALANCE FOR DEFERRED         *
!*       DEBITS.                                                       *
!*                                                                     *

!
      RECURSIVE SUBROUTINE DEFERRED_DEBITS(SAVE_BASE_CASE)
!
      use irec_endpoint_control
      use grx_planning_routines
      use sizecom
      use globecom
      use spindriftlib
      use prod_arrays_dimensions
      use mthnmcom
      use namescom
      use endpoint
	  use globecom
	  use namescom
      use mod_base_year
	  implicit none
      SAVE     
!
      LOGICAL(kind=1) :: SAVE_BASE_CASE,LGandE
      CHARACTER :: ABOVE
      PARAMETER(ABOVE = 'A')
      REAL(kind=4) :: GROSS_DD,AMORT
      REAL(kind=4) :: LGE_GROSS_DD,LGE_NET_DD
      INTEGER(kind=2) :: T,IREC,TACCTS,DELETE,YR,MO, &
      	TACCTS_CR,TACCTS_DR,ACTIVE_ACCOUNTS
      INTEGER(kind=4) :: IOS
!     DECLARATION FOR /FAINPT/
      REAL(kind=4) :: NET_DD,WOYRS,CASH_ADDITONS
      REAL(kind=4) :: INT_DEBAMT,INT_CAMORT
      REAL(kind=4) :: VECTOR_VALUES(AVAIL_DATA_YEARS), &
      	DEFERRED_ADDITONS(AVAIL_DATA_YEARS)
      INTEGER(kind=2) :: FINANCIAL_SIMULATION_YEARS
      INTEGER :: ACCTNO
      INTEGER(kind=2) :: NUM_OF_ASSET_CLASSES,MAX_ASSET_CLASS_NUM
      CHARACTER(len=1) :: METHOD,DUMMY_TYPE,AMORT_DATA_TYPE
!
      INTEGER(kind=2)::R_YR,ASSET_CLASS_POINTER(:),CLASS_POINTER,YR_BAL
      INTEGER(kind=2) :: R_CLASS,YEAR_DEBIT_BOOKED,MONTH_DEBIT_BOOKED
      CHARACTER(len=1)::TAX_TIMING_CLASSIFICATION,DESC*35,INDEX_NAME*34, &
      	ACCOUNT_ACTIVE,DEFERRAL_TYPE
      CHARACTER(len=8) :: BALANCE_SHEET_TREATMENT,TEMP_STR
      CHARACTER(len=20) :: WVPA_TRACKING_TYPE
      LOGICAL(kind=1) :: R_CLASS_EXISTS,CPL_IS_ACTIVE
      INTEGER(kind=2) :: RUN_YEARS,EXTENSION_YEARS, &
      	ASSET_CLASS_VECTOR,ASSET_CLASS,BOOKED_YEAR
      REAL(kind=4) :: ASSET_CLASS_LIST(:)
      REAL(kind=4) :: ASSET_ALLOCATION_LIST(:),ASSET_ALLOCATOR, &
                      ANN_AMORT(:,:),ANN_CASH_ADDITIONS(:,:), &
                      MONTHLY_AMORT_AMOUNT(:,:)
      REAL(kind=4) :: R_TDDB,R_TDDRBB,R_TAMRTE,R_TNDB,R_TCAMRT,R_RB_DD_AMRTE
      REAL(kind=4) :: R_INTEREST_AMORTIZATION, &
                      R_ATL_AMORTIZATION, &
                      R_BTL_AMORTIZATION, &
                      R_ATL_DEF_TAX, &
                      R_BTL_DEF_TAX, &
                      R_UNAMORTIZED_INTEREST_BALANCE, &
                      UNAMORTIZED_ISSUE_EXP_BALANCE, &
                      R_DEFERRED_CASH_ADDITIONS, &
                      R_ATL_CURRENT_TAX_EXPENSE, &
                      R_BTL_CURRENT_TAX_EXPENSE, &
                      R_VARIABLE_OM_AMORT, &
                      R_OTHER_OM_AMORT, & 
                      R_PURCHASE_POWER_AMORT, &
                      R_DEF_CREDITS_IN_RB, &
                      R_OTHER_DEF_CREDITS, &
                      R_RegulatoryLiabilites, &
                      R_GainOnReaquiredDebt, &
                      R_AssetSalesDeferredGain
      INTEGER :: BAL_SHEET_OPTIONS,BAL_SHEET_POS,B_POS
      PARAMETER (BAL_SHEET_OPTIONS=7)
      INTEGER :: UNIQUE_ACCT_TYPE
      PARAMETER (UNIQUE_ACCT_TYPE=2) ! DEBITS AND CREDITS
! DEBIT BALANCE SHEET OPTIONS
      INTEGER (KIND=4), PARAMETER :: Total_Deferred_Debits=0, &
                                     Total_Deferred_Credits=0, &
                 Other_Deferred_Debits=1,Other_Deferred_Credit=1, &
                 Goodwill=2,Regulatory_Liabilites=2, &
                 RegulatoryAssets=3,Gain_on_Reaquired_Debt=3, &
                 FASB109=4,Asset_Sales_Deferred_Gain=4, &
                 FASB133=5, &
                 Unamortized_InterestBalance=6, &
                 Unamortized_Issue_Expen_Balance=7
! CREDIT BALANCE SHEET OPTIONS
      INTEGER :: ACCT_TYPE,DEBIT,CREDIT,Dr,Cr
      PARAMETER (DEBIT=1,CREDIT=2,Dr=1,Cr=2)
!
      REAL :: ATL_SUB_AMORT_VALUE(BAL_SHEET_OPTIONS)
      REAL(kind=4) :: TDDB_wide(:,:,:,:,:),TDDRBB_wide(:,:,:,:), &
        TAMRTE_wide(:,:,:,:), &
           TNDB_wide(:,:,:,:,:),TCAMRT_wide(:,:,:,:), &
           RB_DD_AMRTE_wide(:,:,:,:), &
           INTRST_AMORT_wide(:,:,:,:), &
!     
           BTL_AMORTIZATION_wide(:,:,:,:), &
           ATL_AMORTIZATION_wide(:,:,:,:), &
           ATL_AMORT_SUB_ITEMS_wide(:,:,:,:,:), &
           ATL_DEF_TAX_wide(:,:,:,:), &
           BTL_DEF_TAX_wide(:,:,:,:), &
           DEFERRED_CASH_ADDITIONS_wide(:,:,:,:), &
           VARIABLE_OM_AMORT_wide(:,:,:,:), &
           OTHER_OM_AMORT_wide(:,:,:,:), &
           ATL_CURRENT_TAX_EXPENSE_wide(:,:,:,:), &
           BTL_CURRENT_TAX_EXPENSE_wide(:,:,:,:), &
           TAX_TIMING_DIFFERENCES_wide(:,:,:,:,:), &
           PURCHASE_POWER_AMORT_wide(:,:,:,:)
      ALLOCATABLE :: TDDB_wide,TDDRBB_wide,TAMRTE_wide,TNDB_wide,TCAMRT_wide,RB_DD_AMRTE_wide, &
                     ASSET_CLASS_POINTER, &
                     ASSET_CLASS_LIST, &
                     ASSET_ALLOCATION_LIST, &
                     INTRST_AMORT_wide, &
                     BTL_AMORTIZATION_wide, &
                     ATL_AMORTIZATION_wide, &
                     ATL_AMORT_SUB_ITEMS_wide, &
                     ATL_DEF_TAX_wide, &
                     BTL_DEF_TAX_wide, &
                     DEFERRED_CASH_ADDITIONS_wide, &
                     ANN_AMORT, &
                     MONTHLY_AMORT_AMOUNT, &
                     VARIABLE_OM_AMORT_wide, &
                     OTHER_OM_AMORT_wide, &
                     ATL_CURRENT_TAX_EXPENSE_wide, &
                     BTL_CURRENT_TAX_EXPENSE_wide, &
                     ANN_CASH_ADDITIONS, &
                     TAX_TIMING_DIFFERENCES_wide, &
                     PURCHASE_POWER_AMORT_wide
      INTEGER(kind=2) :: START_AMORT_FULL_YR,I
      CHARACTER(len=1)::INCOME_TREATMENT,O_and_M,OTHER_OM,PURCHASE_POWER
      REAL(kind=4) :: AMORT_DELAY_PERIOD
      REAL(kind=4) :: DEFERRED_TAX_ALLOCATION, &
      	CURRENT_TAX_DEDUCTION_PERCENT
      LOGICAL(kind=1) :: IN_RATEBASE
      CHARACTER(len=1) :: BTL,INTEREST
      PARAMETER (BTL='B',INTEREST='I',O_and_M='V',OTHER_OM='O',PURCHASE_POWER='P')
      CHARACTER(len=1) :: Temporary,Permanent
      PARAMETER(Temporary='T',Permanent='P')
      REAL(kind=4) :: MONTHLY_VALUES(12,LAST_AVAILABLE_MONTHLY_YEAR)
      CHARACTER(len=1)::MONTHLY_DATA_UNITS(LAST_AVAILABLE_MONTHLY_YEAR)
      CHARACTER(len=4) :: MIDAS_LAST_MONTH(LAST_AVAILABLE_MONTHLY_YEAR)
      INTEGER(kind=2) :: MONTH_ENDING(LAST_AVAILABLE_MONTHLY_YEAR)
      CHARACTER(len=1) :: DATA_TYPE,VECTOR_TYPE*20
!
!
! OPEN DETAIL REPORT FILE
!
      REAL(kind=4) :: CLOSING_NET_DD,OPENING_NET_DD, &
                      ATL_AMORTIZATION_VALUE, &
                      INTEREST_AMORTIZATION_VALUE, &
                      BTL_AMORTIZATION_VALUE, &
                      RATEBASE_VALUE, &
                      ATL_DEFERRED_TAX_BASIS, &
                      BTL_DEFERRED_TAX_BASIS
      LOGICAL(kind=1) :: REPORTS_ACTIVE,DEBIT_REPORT, &
      	REPORT_ALL_ACCOUNTS,REPORT_HEADER_OPEN=.FALSE.
      INTEGER(kind=2) :: REPORTING_UNIT,DEBIT_FILE_RPT_HEADER, &
      	ASSET_CLASS_ID,DEACTIVE_YR
      CHARACTER(len=15) :: LEFT_JUSTIFY_I2_IN_STR
      INTEGER :: REPORTING_REC
      REAL(kind=4) :: INCOME_VARIABLES(0:12,*), &
                      CASH_VARIABLES(0:12,*), &
                      BALANCE_VARIABLES(0:12,*), &
                      TAX_VARIABLES(0:12,*)
      INTEGER(kind=2) :: DELAY_MONTHS,DELAY_YEARS,START_AMORT_MONTH
      LOGICAL(kind=1) :: TAKE_FULL_MONTH
      INTEGER(kind=2) :: YR2,MO2,WO_MONTHS
      REAL(kind=4) :: HALF_MONTH_AMORTIZATION,MONTHLY_AMORTIZATION_LOC, &
      	CURRENT_MONTH_AMORTIZATION,ANNUAL_AMOUNT
      REAL(kind=4) :: ATL_TAX_EXPENSE, &
                      BTL_TAX_EXPENSE, &
                      INTEREST_OPENING_NET_DD, &
                      INTEREST_CLOSING_NET_DD, &
                      DEBIT_OPENING_NET_DD, &
                      DEBIT_CLOSING_NET_DD, &
                      VARIABLE_OM_AMORT_VALUE, &
                      OTHER_OM_AMORT_VALUE, &
                      PURCHASE_POWER_AMORTIZATION
      LOGICAL(kind=1) :: VOID_LOGICAL,RETURN_ASSET_CLASS_LISTS
      REAL(kind=4) :: ALLOCATION_VALUE(0:AVAIL_DATA_YEARS,0:12), &
                      ANNUAL_VECTOR_VALUES(0:AVAIL_DATA_YEARS), &
                      VECTOR_MONTHLY_DATA(12,LAST_AVAILABLE_MONTHLY_YEAR)
      REAL(kind=4) :: TREND_NORM
      INTEGER(kind=2) :: ALLOCATION_VECTOR
      INTEGER(kind=2) :: START_MO,MO_DEACT
      INTEGER(kind=2) :: MO_BAL
      REAL(kind=4) :: NET_DD_OPENING, &
                      NET_DD_CLOSING, &
                      CHANGE_DUE_2_AMORT, &
                      R_OTHER_DD_BALANCE, &
                      R_GOODWILL, &
                      R_CLASS_REGULATORY_ASSETS, &
                      R_FASB_109, &
                      R_FASB_133, &
                      R_OTHER_DD_AMORT, &
                      R_GOODWILL_AMORT, &
                      R_CLASS_REGULATORY_ASSETS_AMORT, &
                      R_FASB_109_AMORT, &
                      R_FASB_133_AMORT 
      LOGICAL(kind=1) :: LF95,LAHEY_LF95
      LOGICAL(kind=1) :: WVPA,WVPA_TRACKER_DATABASE_FROM_DD
      REAL (KIND=4), ALLOCATABLE :: NET_CHANGE_IN_DB(:,:,:,:,:)
      REAL (KIND=4) BAL_SHEET_NET_CHANGE

      LF95 = LAHEY_LF95()
      REPORTS_ACTIVE = DEBIT_REPORT(REPORT_ALL_ACCOUNTS) .AND. &
      	.NOT. SAVE_BASE_CASE
      REPORT_ALL_ACCOUNTS = REPORT_ALL_ACCOUNTS .AND. REPORTS_ACTIVE

!     DEFERRED DEBITS SECTION

      FINANCIAL_SIMULATION_YEARS=MAX(6,get_globecom_study_period()+ &
        get_EXTENSION_PERIOD()+1)
      CALL SET_UP_DD_ARRAYS
!
      ALLOCATE(ASSET_CLASS_LIST(AVAIL_DATA_YEARS), &
               ASSET_ALLOCATION_LIST(AVAIL_DATA_YEARS), &
               ANN_AMORT(0:12,0:FINANCIAL_SIMULATION_YEARS), &
               ANN_CASH_ADDITIONS(0:12,0:FINANCIAL_SIMULATION_YEARS), &
               MONTHLY_AMORT_AMOUNT(0:12,0:FINANCIAL_SIMULATION_YEARS))
      CALL OPEN_DEFERRED_DEBIT_FILE(INT(11,2))
      TACCTS = 0
      TACCTS_CR = 0
      TACCTS_DR = 0
      IREC = 0
      IF(LF95) THEN
         WRITE(SCREEN_MESSAGES,"(A)") "Debit/Credit Accounts "
         CALL MG_LOCATE_WRITE(18,70,trim(SCREEN_MESSAGES),3,2)
      ENDIF
      DO WHILE (NUM_OF_ASSET_CLASSES > 0)
         IREC = IREC + 1
         READ(11,REC=IREC,IOSTAT=IOS) DELETE,DESC,ACCTNO,INT_DEBAMT, &
                                      INT_CAMORT,WOYRS,METHOD, &
                                      ASSET_CLASS, &
                                      ASSET_CLASS_VECTOR, &
                                      INCOME_TREATMENT, &
                                      AMORT_DELAY_PERIOD, &
                                      DEFERRED_TAX_ALLOCATION, &
                                      YEAR_DEBIT_BOOKED, &
                                      TAX_TIMING_CLASSIFICATION, &
                                      CURRENT_TAX_DEDUCTION_PERCENT, &
                                      MONTH_DEBIT_BOOKED, &
                                      TEMP_STR, &
                                      ACCOUNT_ACTIVE, &
                                      DEFERRAL_TYPE, &
                                      WVPA_TRACKING_TYPE
!
         IF(IOS /= 0) EXIT
         IF(DELETE > 7 .OR. ACCOUNT_ACTIVE == 'N') CYCLE
         TACCTS = TACCTS + 1
         IF(LF95) THEN
            WRITE(SCREEN_MESSAGES,"(I4,A)") TACCTS,"-"//DESC
            CALL MG_LOCATE_WRITE(16,70,trim(SCREEN_MESSAGES),3,0)
         ELSE
            WRITE(SCREEN_MESSAGES,"(I4)") TACCTS
            CALL MG_LOCATE_WRITE(16,70,trim(SCREEN_MESSAGES),ALL_VERSIONS,0)
         ENDIF
         IF(DEFERRAL_TYPE == 'C') THEN
            ACCT_TYPE = CREDIT
            TACCTS_CR = TACCTS_CR + 1
         ELSE
            ACCT_TYPE = DEBIT
            TACCTS_DR = TACCTS_DR + 1
         ENDIF
         CALL UPC(TEMP_STR,BALANCE_SHEET_TREATMENT)
         IF(YEAR_DEBIT_BOOKED < 0) THEN
            BOOKED_YEAR = YEAR_DEBIT_BOOKED
         ELSE
            BOOKED_YEAR = MAX(INT(0,2),YEAR_DEBIT_BOOKED-get_base_year())
         ENDIF
         IF(BOOKED_YEAR > FINANCIAL_SIMULATION_YEARS) CYCLE
!
         IF(MONTH_DEBIT_BOOKED == 0 .OR. MONTH_DEBIT_BOOKED > 12) then
         	MONTH_DEBIT_BOOKED = 7
         end if
!
         IF(AMORT_DELAY_PERIOD >= 1900) THEN
            AMORT_DELAY_PERIOD = MAX(0., &
            	AMORT_DELAY_PERIOD-get_base_year()-BOOKED_YEAR,REAL(BOOKED_YEAR))
         ELSE
            AMORT_DELAY_PERIOD = MAX(0.,AMORT_DELAY_PERIOD)
         ENDIF
!
         DEFERRED_TAX_ALLOCATION = DEFERRED_TAX_ALLOCATION/100.
         CURRENT_TAX_DEDUCTION_PERCENT = CURRENT_TAX_DEDUCTION_PERCENT/100.
         IN_RATEBASE = METHOD == 'A' .OR. METHOD == 'R'
!
         NET_DD = INT_DEBAMT - INT_CAMORT
         ANN_AMORT(0,0) = INT_CAMORT
!
! LOAD THE CASH ADDITIONS
!
         ANN_CASH_ADDITIONS = 0.
         IF(YEAR_DEBIT_BOOKED < 0) THEN ! GET VECTOR OF CASH ADDITIONS
           CALL GET_MONTHLY_ANNUAL_VALUES(ABS(INT(YEAR_DEBIT_BOOKED,2)), &
                                           DATA_TYPE, &
                                           VECTOR_TYPE, &
                                           DEFERRED_ADDITONS, &
                                           MONTHLY_VALUES(1,1), &
                                           MONTHLY_DATA_UNITS, &
                                           MONTH_ENDING)
            IF(MONTHLY_MIDAS_ACTIVE) THEN
            
               CALL RIPPLE_MONTHLY_DATA(DEFERRED_ADDITONS, &
                                        MONTHLY_VALUES)
!
               CALL MONTHLY_BOOK_VALUES_IN_DOLLARS(DEFERRED_ADDITONS, &
                                                   MONTHLY_VALUES, &
                                                   MONTHLY_DATA_UNITS, &
                                                   MONTH_ENDING) 

               CALL TRANSFER_VECTOR_VALUES_2(ANN_CASH_ADDITIONS, &
                                             DEFERRED_ADDITONS, &
                                             MONTHLY_VALUES, &
                                             FINANCIAL_SIMULATION_YEARS)
            ELSE
               DO YR = 1, FINANCIAL_SIMULATION_YEARS
                  IF(YR > AVAIL_DATA_YEARS) THEN
                     ANN_CASH_ADDITIONS(0,YR) = &
                     	DEFERRED_ADDITONS(AVAIL_DATA_YEARS)
                  ELSE
                     ANN_CASH_ADDITIONS(0,YR) = DEFERRED_ADDITONS(YR)
                  ENDIF
                  ANN_CASH_ADDITIONS(MONTH_DEBIT_BOOKED,YR) = &
                  	ANN_CASH_ADDITIONS(0,YR)
               ENDDO
            ENDIF
            ANN_CASH_ADDITIONS(0,0) = INT_DEBAMT - INT_CAMORT
            ANN_CASH_ADDITIONS(12,0) = INT_DEBAMT - INT_CAMORT
            NET_DD = INT_DEBAMT - INT_CAMORT
            ANN_AMORT(0,0) = INT_CAMORT
         ELSE
            IF(BOOKED_YEAR == 0) THEN
               ANN_CASH_ADDITIONS(0,0) = INT_DEBAMT - INT_CAMORT
               ANN_CASH_ADDITIONS(12,0) = INT_DEBAMT - INT_CAMORT
               NET_DD = INT_DEBAMT - INT_CAMORT
               ANN_AMORT(0,0) = INT_CAMORT
            ELSE
               ANN_CASH_ADDITIONS(MONTH_DEBIT_BOOKED,BOOKED_YEAR) = INT_DEBAMT
               ANN_CASH_ADDITIONS(0,BOOKED_YEAR) = INT_DEBAMT
               NET_DD = 0.
               ANN_AMORT(0,0) = 0.
               INT_DEBAMT = 0.
               INT_CAMORT = 0.
            ENDIF
         ENDIF
!
! CALCULATE AMORTIZATION OR GET IT FROM A VECTOR
!
         ANN_AMORT = 0.
         IF(WOYRS < 0.) THEN
            CALL GET_MONTHLY_ANNUAL_VALUES(ABS(INT(WOYRS,2)), &
                                           AMORT_DATA_TYPE, &
                                           VECTOR_TYPE, &
                                           VECTOR_VALUES, &
                                           MONTHLY_VALUES(1,1), &
                                           MONTHLY_DATA_UNITS, &
                                           MONTH_ENDING)
            IF(MONTHLY_MIDAS_ACTIVE) THEN
            
               CALL RIPPLE_MONTHLY_DATA(VECTOR_VALUES, &
                                        MONTHLY_VALUES)
!
               CALL MONTHLY_BOOK_VALUES_IN_DOLLARS(VECTOR_VALUES, & 
                                                   MONTHLY_VALUES, &
                                                   MONTHLY_DATA_UNITS, &
                                                   MONTH_ENDING)

               CALL TRANSFER_VECTOR_VALUES_2(ANN_AMORT, &
                                             VECTOR_VALUES, &
                                             MONTHLY_VALUES, &
                                             FINANCIAL_SIMULATION_YEARS)
               IF(AMORT_DATA_TYPE == 'Y') THEN
                  CALL RETURN_BASE_YEAR_VECTOR_VALUES(ANN_AMORT(12,0))
               ENDIF
            ELSE
               DO YR = 1, FINANCIAL_SIMULATION_YEARS
                  IF(YR > AVAIL_DATA_YEARS) THEN
                     ANN_AMORT(0,YR) = VECTOR_VALUES(AVAIL_DATA_YEARS)
                  ELSE
                     ANN_AMORT(0,YR) = VECTOR_VALUES(YR)
                  ENDIF
                  IF(AMORT_DATA_TYPE == 'Y') THEN
                     AMORT = ANN_AMORT(0,YR)
                  ELSE
                     AMORT = ANN_AMORT(0,YR)/12.
                  ENDIF
                  DO MO = 1, 12
                     ANN_AMORT(MO,YR) = AMORT
                  ENDDO
               ENDDO
            ENDIF
         ELSE
            AMORT_DATA_TYPE = 'Y' !ears
            VECTOR_VALUES = WOYRS
            ANN_AMORT = WOYRS
         ENDIF
         ANN_AMORT(0,0) = INT_CAMORT
!
         IF(AMORT_DATA_TYPE == 'Y' .AND. &
         	AMORT_DELAY_PERIOD  <= FINANCIAL_SIMULATION_YEARS) THEN
            MONTHLY_AMORT_AMOUNT = 0.
            DO YR = 0, FINANCIAL_SIMULATION_YEARS
               IF(ANN_CASH_ADDITIONS(0,YR) == 0.) CYCLE
               DO MO = 1, 12
                  IF(ANN_CASH_ADDITIONS(MO,YR) == 0.) CYCLE
                  WOYRS = ANN_AMORT(MO,YR)
                  IF(WOYRS >= 99.) CYCLE
                  WO_MONTHS = MAX(INT(1,2),INT(WOYRS*12.,2))
                  
                  NET_DD = ANN_CASH_ADDITIONS(MO,YR)
                  DELAY_MONTHS = 12. * AMORT_DELAY_PERIOD
                  DELAY_YEARS = DELAY_MONTHS/12
                  IF(YR == 0) THEN
                     START_AMORT_MONTH=1+DELAY_MONTHS-12*DELAY_YEARS
                     TAKE_FULL_MONTH = .TRUE.
                  ELSE
                     START_AMORT_MONTH = MO+DELAY_MONTHS-12*DELAY_YEARS
                     TAKE_FULL_MONTH = .FALSE.
                     IF(.NOT. MONTHLY_MIDAS_ACTIVE) TAKE_FULL_MONTH = .TRUE.
                  ENDIF
          START_AMORT_FULL_YR=MAX(INT(1,2),YR+INT(AMORT_DELAY_PERIOD,2))
                  IF(START_AMORT_MONTH > 12) THEN
                     START_AMORT_FULL_YR = START_AMORT_FULL_YR + 1
                     START_AMORT_MONTH = START_AMORT_MONTH - 12
                  ENDIF
                     MONTHLY_AMORTIZATION_LOC = &
                     	ANN_CASH_ADDITIONS(MO,YR)/WO_MONTHS
                  NET_DD = ANN_CASH_ADDITIONS(MO,YR)
                  DO YR2 = START_AMORT_FULL_YR,FINANCIAL_SIMULATION_YEARS
                     ANNUAL_AMOUNT = 0
                     DO MO2 = START_AMORT_MONTH, 12
                        IF(TAKE_FULL_MONTH) THEN
                           CURRENT_MONTH_AMORTIZATION = &
                                 SIGN(MIN(ABS(NET_DD), &
                                     ABS(MONTHLY_AMORTIZATION_LOC)), &
                                     MONTHLY_AMORTIZATION_LOC)
                           MONTHLY_AMORT_AMOUNT(MO2,YR2) = &
                                           MONTHLY_AMORT_AMOUNT(MO2,YR2) &
                                           + CURRENT_MONTH_AMORTIZATION
                        ELSE
                           CURRENT_MONTH_AMORTIZATION = &
                                 SIGN(MIN(ABS(NET_DD), &
                                      ABS(MONTHLY_AMORTIZATION_LOC)/2.), &
                                      MONTHLY_AMORTIZATION_LOC) 
                           MONTHLY_AMORT_AMOUNT(MO2,YR2) = &
                                           MONTHLY_AMORT_AMOUNT(MO2,YR2) &
                                           + CURRENT_MONTH_AMORTIZATION
                           TAKE_FULL_MONTH = .TRUE.
                        ENDIF
                        ANNUAL_AMOUNT = ANNUAL_AMOUNT + &
                        	CURRENT_MONTH_AMORTIZATION
                        NET_DD = NET_DD - CURRENT_MONTH_AMORTIZATION
                        IF(ABS(NET_DD) < .00001) EXIT
                     ENDDO
                     START_AMORT_MONTH = 1
                     MONTHLY_AMORT_AMOUNT(0,YR2) = ANNUAL_AMOUNT + &
                     	 MONTHLY_AMORT_AMOUNT(0,YR2)
                     IF(ABS(NET_DD) < .00001) EXIT
                  ENDDO
               ENDDO
            ENDDO
            ANN_AMORT = MONTHLY_AMORT_AMOUNT ! ARRAYS
         ENDIF
!
! DETERMING BALANCE SHEET POSITION
!
         IF(INCOME_TREATMENT == INTEREST .AND. &
         	trim(BALANCE_SHEET_TREATMENT) == ' ') THEN
            BALANCE_SHEET_TREATMENT = 'UNAMORT'
         ENDIF
         IF(ACCT_TYPE == CREDIT) THEN
            BAL_SHEET_POS = Other_Deferred_Credit
            IF(INDEX(BALANCE_SHEET_TREATMENT,'REGULATO') /= 0) &
                                   BAL_SHEET_POS = Regulatory_Liabilites
            IF(INDEX(BALANCE_SHEET_TREATMENT,'GAIN ON') /= 0) &
                                  BAL_SHEET_POS = Gain_on_Reaquired_Debt
            IF(INDEX(BALANCE_SHEET_TREATMENT,'ASSET SA') /= 0) &
                               BAL_SHEET_POS = Asset_Sales_Deferred_Gain
         ELSE
            BAL_SHEET_POS = Other_Deferred_Debits
            IF(INDEX(BALANCE_SHEET_TREATMENT,'GOODWILL') /= 0) &
                                                BAL_SHEET_POS = Goodwill
            IF(INDEX(BALANCE_SHEET_TREATMENT,'REGULA') /= 0) &
                                       BAL_SHEET_POS = RegulatoryAssets
            IF(INDEX(BALANCE_SHEET_TREATMENT,'109') /= 0) &
                                                BAL_SHEET_POS = FASB109
            IF(INDEX(BALANCE_SHEET_TREATMENT,'133') /= 0) &
                                                BAL_SHEET_POS = FASB133
            IF(INDEX(BALANCE_SHEET_TREATMENT,'UNAMORT') /= 0) &
                            BAL_SHEET_POS = Unamortized_InterestBalance
            IF(INDEX(BALANCE_SHEET_TREATMENT,'ISSUE') /= 0) &
                       BAL_SHEET_POS = Unamortized_Issue_Expen_Balance
         ENDIF 
!
! WRITE ASSET RESULTS
!
         IF(REPORTS_ACTIVE .AND. (DELETE > 1 .OR. REPORT_ALL_ACCOUNTS)) THEN
            IF(REPORTS_ACTIVE .AND. .NOT. REPORT_HEADER_OPEN) THEN
               REPORTING_UNIT = DEBIT_FILE_RPT_HEADER(REPORTING_REC)
               REPORT_HEADER_OPEN = .TRUE.
            ENDIF
            INDEX_NAME = DESC//LEFT_JUSTIFY_I2_IN_STR(IREC)
            OPENING_NET_DD = INT_DEBAMT - INT_CAMORT
            DO YR = 1, FINANCIAL_SIMULATION_YEARS
               CLOSING_NET_DD = OPENING_NET_DD &
                                + ANN_CASH_ADDITIONS(0,YR) &
                                - ANN_AMORT(0,YR) 
               ATL_AMORTIZATION_VALUE = 0.
               INTEREST_AMORTIZATION_VALUE = 0.
               BTL_AMORTIZATION_VALUE = 0.
               RATEBASE_VALUE = 0.
               ATL_DEFERRED_TAX_BASIS = 0.
               BTL_DEFERRED_TAX_BASIS = 0.
   
               ATL_SUB_AMORT_VALUE = 0.

               IF(INCOME_TREATMENT == BTL) THEN
                  BTL_AMORTIZATION_VALUE = ANN_AMORT(0,YR)
                  BTL_DEFERRED_TAX_BASIS = DEFERRED_TAX_ALLOCATION * &
                              (ANN_CASH_ADDITIONS(0,YR)-ANN_AMORT(0,YR)) &
                              * (1.-CURRENT_TAX_DEDUCTION_PERCENT)
                  BTL_TAX_EXPENSE = (1.-CURRENT_TAX_DEDUCTION_PERCENT) &
                                             * ANN_CASH_ADDITIONS(0,YR) &
                         + CURRENT_TAX_DEDUCTION_PERCENT*ANN_AMORT(0,YR) 
               ELSE
                  ATL_DEFERRED_TAX_BASIS = DEFERRED_TAX_ALLOCATION * &
                              (ANN_CASH_ADDITIONS(0,YR)-ANN_AMORT(0,YR)) &
                               * (1.-CURRENT_TAX_DEDUCTION_PERCENT)
                  ATL_TAX_EXPENSE = (1.-CURRENT_TAX_DEDUCTION_PERCENT) &
                                              * ANN_CASH_ADDITIONS(0,YR) &
                         + CURRENT_TAX_DEDUCTION_PERCENT*ANN_AMORT(0,YR) 
               ENDIF
               IF(INCOME_TREATMENT == INTEREST) THEN
                  INTEREST_AMORTIZATION_VALUE = ANN_AMORT(0,YR)
               ELSEIF(INCOME_TREATMENT == O_and_M) THEN
                  VARIABLE_OM_AMORT_VALUE = ANN_AMORT(0,YR)
               ELSEIF(INCOME_TREATMENT == OTHER_OM) THEN
                  OTHER_OM_AMORT_VALUE = ANN_AMORT(0,YR)
               ELSEIF(INCOME_TREATMENT == PURCHASE_POWER) THEN
                  PURCHASE_POWER_AMORTIZATION = ANN_AMORT(0,YR)
               ELSEIF(INCOME_TREATMENT /= BTL) THEN
                  ATL_AMORTIZATION_VALUE = ANN_AMORT(0,YR)
               ENDIF
               ATL_SUB_AMORT_VALUE(BAL_SHEET_POS) = ANN_AMORT(0,YR)

                  INTEREST_OPENING_NET_DD = 0.
                  INTEREST_CLOSING_NET_DD = 0.
                  DEBIT_OPENING_NET_DD = OPENING_NET_DD
                  DEBIT_CLOSING_NET_DD = CLOSING_NET_DD

               IF(IN_RATEBASE) THEN
                  RATEBASE_VALUE = (OPENING_NET_DD+CLOSING_NET_DD)/2.
               ENDIF

               WRITE(REPORTING_UNIT,REC=REPORTING_REC) PRT_ENDPOINT(), &
                                     INDEX_NAME, &
                                     FLOAT(YR+get_base_year()), &
                                     DEBIT_OPENING_NET_DD, &
                                     ANN_CASH_ADDITIONS(0,YR), &
                                     ATL_AMORTIZATION_VALUE, &
                                     INTEREST_AMORTIZATION_VALUE, &
                                     BTL_AMORTIZATION_VALUE, &
                                     DEBIT_CLOSING_NET_DD, &
                                     RATEBASE_VALUE, &
                                     ATL_DEFERRED_TAX_BASIS, &
                                     BTL_DEFERRED_TAX_BASIS, &
                                     ATL_TAX_EXPENSE, &
                                     BTL_TAX_EXPENSE, &
                                     VARIABLE_OM_AMORT_VALUE, &
                                     INTEREST_OPENING_NET_DD, &
                                     INTEREST_CLOSING_NET_DD, &
                                     OTHER_OM_AMORT_VALUE, &
                                     PURCHASE_POWER_AMORTIZATION, &
                                     ATL_SUB_AMORT_VALUE 
               REPORTING_REC = REPORTING_REC + 1
               OPENING_NET_DD = CLOSING_NET_DD
            ENDDO
         ENDIF
!
! ALLOCATE RESULTS TO CLASSES
!
         IF(WVPA() .AND. .NOT. SAVE_BASE_CASE) THEN
            VOID_LOGICAL = WVPA_TRACKER_DATABASE_FROM_DD(DESC, &
                                             DEFERRAL_TYPE, &
                                             WVPA_TRACKING_TYPE, &
                                             FINANCIAL_SIMULATION_YEARS, &
                                             ANN_AMORT)
         ENDIF
         VOID_LOGICAL = RETURN_ASSET_CLASS_LISTS(ASSET_CLASS, &
                                                 ASSET_CLASS_LIST, &
                                                 ASSET_CLASS_VECTOR, &
                                                 ASSET_ALLOCATION_LIST)
         CLASS_POINTER = 1
         DO
            ASSET_CLASS = ASSET_CLASS_LIST(CLASS_POINTER)
            CALL CHECK_IF_CLASS_DEFINED(ASSET_CLASS)
            ASSET_CLASS = ASSET_CLASS + 1
            ASSET_CLASS_ID = ASSET_CLASS

            IF(ASSET_CLASS > 0) ASSET_CLASS = ASSET_CLASS_POINTER(ASSET_CLASS)
!
            IF(ASSET_ALLOCATION_LIST(CLASS_POINTER) < 0.) THEN
               ALLOCATION_VECTOR = ABS(ASSET_ALLOCATION_LIST(CLASS_POINTER))
               IF(MONTHLY_MIDAS_ACTIVE) THEN
                  CALL GET_MONTHLY_ANNUAL_VALUES(ALLOCATION_VECTOR, &
                                               DATA_TYPE, &
                                               VECTOR_TYPE, &
                                               ANNUAL_VECTOR_VALUES(1), &
                                               VECTOR_MONTHLY_DATA(1,1), &
                                               MONTHLY_DATA_UNITS, &
                                               MONTH_ENDING) 
                  CALL RETURN_BASE_YEAR_VECTOR_VALUES(ANNUAL_VECTOR_VALUES(0))
            
                  CALL RIPPLE_MONTHLY_DATA(ANNUAL_VECTOR_VALUES(1), &
                                           VECTOR_MONTHLY_DATA)

! LOAD BASE YEAR ALLOCATION                  

                  ASSET_ALLOCATOR = ANNUAL_VECTOR_VALUES(0)/100.
                  DO MO = 0, 12
                     ALLOCATION_VALUE(0,MO) = ASSET_ALLOCATOR 
                  ENDDO
!
                  DO YR = 1, AVAIL_DATA_YEARS
                     ASSET_ALLOCATOR = ANNUAL_VECTOR_VALUES(YR)/100.
                     DO MO = 0, 12
                        IF(MO == 0) THEN
                           ALLOCATION_VALUE(YR,MO) =  &
                           	ANNUAL_VECTOR_VALUES(YR)/100.
                        ELSEIF(YR > LAST_AVAILABLE_MONTHLY_YEAR) THEN
                           ALLOCATION_VALUE(YR,MO) = &
                           	ANNUAL_VECTOR_VALUES(YR)/100.
                        ELSE
                           ALLOCATION_VALUE(YR,MO) = &
                           	VECTOR_MONTHLY_DATA(MO,YR)/100.
                        ENDIF
                     ENDDO
                  ENDDO
               ELSE
                  CALL GET_ASSET_VAR(ALLOCATION_VECTOR, &
                                     DUMMY_TYPE,ANNUAL_VECTOR_VALUES(1))
                  CALL RETURN_BASE_YEAR_VECTOR_VALUES( &
                                                ANNUAL_VECTOR_VALUES(0))
!                     
                  DO YR = 0, AVAIL_DATA_YEARS
                     ASSET_ALLOCATOR = ANNUAL_VECTOR_VALUES(YR)/100.
                     DO MO = 0, 12
                        ALLOCATION_VALUE(YR,MO) = ASSET_ALLOCATOR 
                     ENDDO
                  ENDDO
               ENDIF
            ELSE
                  
               ASSET_ALLOCATOR=ASSET_ALLOCATION_LIST(CLASS_POINTER)/100.
               DO MO = 0, 12
                  DO YR = 0, AVAIL_DATA_YEARS
                     ALLOCATION_VALUE(YR,MO) = ASSET_ALLOCATOR
                  ENDDO
               ENDDO
            ENDIF

            GROSS_DD = 0. ! INT_DEBAMT * ASSET_ALLOCATOR
            NET_DD = 0.
            ASSET_ALLOCATOR = ALLOCATION_VALUE(0,12)
            GROSS_DD = INT_DEBAMT * ASSET_ALLOCATOR
            NET_DD = (INT_DEBAMT - INT_CAMORT) * ASSET_ALLOCATOR
            LGE_GROSS_DD = INT_DEBAMT 
            LGE_NET_DD = INT_DEBAMT - INT_CAMORT

               TDDB_wide(12,0,ASSET_CLASS,BAL_SHEET_POS,ACCT_TYPE) = &
                         TDDB_wide(12,0,ASSET_CLASS,BAL_SHEET_POS,ACCT_TYPE) &
                         + GROSS_DD
               TDDB_wide(12,0,ASSET_CLASS,Total_Deferred_Debits,ACCT_TYPE) = &
                 TDDB_wide(12,0,ASSET_CLASS,Total_Deferred_Debits,ACCT_TYPE) &
                 + GROSS_DD
               TNDB_wide(12,0,ASSET_CLASS,BAL_SHEET_POS,ACCT_TYPE) = &
                          TNDB_wide(12,0,ASSET_CLASS,BAL_SHEET_POS,ACCT_TYPE) &
                          + NET_DD
               TNDB_wide(12,0,ASSET_CLASS,Total_Deferred_Debits,ACCT_TYPE) = &
                  TNDB_wide(12,0,ASSET_CLASS,Total_Deferred_Debits,ACCT_TYPE) &
                  + NET_DD
               TNDB_wide(0,1,ASSET_CLASS,BAL_SHEET_POS,ACCT_TYPE) = &
                          TNDB_wide(0,1,ASSET_CLASS,BAL_SHEET_POS,ACCT_TYPE) &
                          + NET_DD
               TNDB_wide(0,1,ASSET_CLASS,Total_Deferred_Debits,ACCT_TYPE) = &
                  TNDB_wide(0,1,ASSET_CLASS,Total_Deferred_Debits,ACCT_TYPE) &
                  + NET_DD
!            ENDIF
            IF(IN_RATEBASE) THEN
               TDDRBB_wide(12,0,ASSET_CLASS,ACCT_TYPE) = &
               	TDDRBB_wide(12,0,ASSET_CLASS,ACCT_TYPE) + NET_DD
               RB_DD_AMRTE_wide(12,0,ASSET_CLASS,ACCT_TYPE) = AMORT + &
               	 RB_DD_AMRTE_wide(12,0,ASSET_CLASS,ACCT_TYPE)
            ENDIF
!
            START_MO = 12
            CALL CLASS_DEACTIVATE_IN_YR(ASSET_CLASS_ID, &
                                        DEACTIVE_YR,MO_DEACT)
            DO YR = 1, FINANCIAL_SIMULATION_YEARS
               IF(YR > DEACTIVE_YR + 1) EXIT
               IF(YR == DEACTIVE_YR + 1) THEN
                  TDDB_wide(0,YR+1,ASSET_CLASS,BAL_SHEET_POS,ACCT_TYPE) = 0.
!    
                  TNDB_wide(0,YR+1,ASSET_CLASS,BAL_SHEET_POS,ACCT_TYPE) = 0.
!    
                  TDDRBB_wide(0,YR+1,ASSET_CLASS,ACCT_TYPE) = 0.
!    
               ENDIF
               TDDB_wide(0,YR,ASSET_CLASS,BAL_SHEET_POS,ACCT_TYPE) = &
                       TDDB_wide(12,YR-1,ASSET_CLASS,BAL_SHEET_POS,ACCT_TYPE)
               TNDB_wide(0,YR,ASSET_CLASS,BAL_SHEET_POS,ACCT_TYPE) = &
                       TNDB_wide(12,YR-1,ASSET_CLASS,BAL_SHEET_POS,ACCT_TYPE)
               IF(IN_RATEBASE) THEN
                  TDDRBB_wide(0,YR,ASSET_CLASS,ACCT_TYPE) = &
                  	TDDRBB_wide(12,YR-1,ASSET_CLASS,ACCT_TYPE)
               ENDIF
               DO MO = 1, 12
                  
                  ASSET_ALLOCATOR = ALLOCATION_VALUE(MIN(YR,AVAIL_DATA_YEARS),MO)
                  IF(YR == DEACTIVE_YR + 1 .AND. MO == MO_DEACT) THEN
                    TDDB_wide(MO,YR,ASSET_CLASS,BAL_SHEET_POS,ACCT_TYPE) = 0.

                     TNDB_wide(MO,YR,ASSET_CLASS,BAL_SHEET_POS,ACCT_TYPE) = 0.

                     TDDRBB_wide(MO,YR,ASSET_CLASS,ACCT_TYPE) = 0.

                     ASSET_ALLOCATOR = 0.
                     GROSS_DD = 0.
                     NET_DD = 0.
                     CYCLE
                  ENDIF
                  CASH_ADDITONS = ANN_CASH_ADDITIONS(MO,YR) * ASSET_ALLOCATOR
                  AMORT = ANN_AMORT(MO,YR) * ASSET_ALLOCATOR 
                  IF(LGandE()) THEN
                     LGE_GROSS_DD = LGE_GROSS_DD + ANN_CASH_ADDITIONS(MO,YR)
                     GROSS_DD = ASSET_ALLOCATOR * LGE_GROSS_DD
                     LGE_NET_DD = LGE_NET_DD &
                                  + ANN_CASH_ADDITIONS(MO,YR) &
                                  - ANN_AMORT(MO,YR)
                     BAL_SHEET_NET_CHANGE = ASSET_ALLOCATOR * LGE_NET_DD - NET_DD
                     NET_DD = ASSET_ALLOCATOR * LGE_NET_DD
                  ELSE
                     GROSS_DD = GROSS_DD + CASH_ADDITONS
                     NET_DD = NET_DD + CASH_ADDITONS - AMORT
                     BAL_SHEET_NET_CHANGE = CASH_ADDITONS - AMORT
                  ENDIF
!
                  DEFERRED_CASH_ADDITIONS_wide(MO,YR,ASSET_CLASS,ACCT_TYPE) = &
                            CASH_ADDITONS &
                            + DEFERRED_CASH_ADDITIONS_wide(MO,YR,ASSET_CLASS, &
                                                              ACCT_TYPE)
!
                  TDDB_wide(MO,YR,ASSET_CLASS,BAL_SHEET_POS,ACCT_TYPE) = &
                                   TDDB_wide(MO,YR,ASSET_CLASS,BAL_SHEET_POS, &
                                                              ACCT_TYPE) &
                                   + GROSS_DD
                  TDDB_wide(MO,YR,ASSET_CLASS,Total_Deferred_Debits,ACCT_TYPE) = &
                           TDDB_wide(MO,YR,ASSET_CLASS,Total_Deferred_Debits, &
                                                              ACCT_TYPE) &
                           + GROSS_DD
                  IF(INCOME_TREATMENT == BTL) THEN
                     BTL_AMORTIZATION_wide(MO,YR,ASSET_CLASS,ACCT_TYPE) = &
                                     BTL_AMORTIZATION_wide(MO,YR,ASSET_CLASS, &
                                                              ACCT_TYPE) &
                                     + AMORT
                     BTL_DEF_TAX_wide(MO,YR,ASSET_CLASS,ACCT_TYPE) = &
                       BTL_DEF_TAX_wide(MO,YR,ASSET_CLASS,ACCT_TYPE) &
                       + (CASH_ADDITONS-AMORT) * DEFERRED_TAX_ALLOCATION 
                     BTL_CURRENT_TAX_EXPENSE_wide(MO,YR,ASSET_CLASS,ACCT_TYPE) = &
                     BTL_CURRENT_TAX_EXPENSE_wide(MO,YR,ASSET_CLASS,ACCT_TYPE) &
                     + (1.-CURRENT_TAX_DEDUCTION_PERCENT)*CASH_ADDITONS &
                     + CURRENT_TAX_DEDUCTION_PERCENT * AMORT
                  ELSE
                     ATL_CURRENT_TAX_EXPENSE_wide(MO,YR,ASSET_CLASS,ACCT_TYPE) = &
                     ATL_CURRENT_TAX_EXPENSE_wide(MO,YR,ASSET_CLASS,ACCT_TYPE) &
                     + (1.-CURRENT_TAX_DEDUCTION_PERCENT)*CASH_ADDITONS &
                     + CURRENT_TAX_DEDUCTION_PERCENT * AMORT 
                     ATL_DEF_TAX_wide(MO,YR,ASSET_CLASS,ACCT_TYPE) = &
                     ATL_DEF_TAX_wide(MO,YR,ASSET_CLASS,ACCT_TYPE) &
                     + (CASH_ADDITONS-AMORT) * DEFERRED_TAX_ALLOCATION &
                                    * (1.-CURRENT_TAX_DEDUCTION_PERCENT)
                     IF(INCOME_TREATMENT == INTEREST) THEN
                        INTRST_AMORT_wide(MO,YR,ASSET_CLASS, &
                        ACCT_TYPE) = &
                        INTRST_AMORT_wide(MO,YR,ASSET_CLASS, &
                        	ACCT_TYPE) + AMORT
!
                     ELSEIF(INCOME_TREATMENT == O_and_M) THEN
                        VARIABLE_OM_AMORT_wide(MO,YR,ASSET_CLASS,ACCT_TYPE) = &
                        VARIABLE_OM_AMORT_wide(MO,YR,ASSET_CLASS,ACCT_TYPE) &
                        + AMORT
                     ELSEIF(INCOME_TREATMENT == OTHER_OM) THEN
                        OTHER_OM_AMORT_wide(MO,YR,ASSET_CLASS,ACCT_TYPE) = &
                            OTHER_OM_AMORT_wide(MO,YR,ASSET_CLASS,ACCT_TYPE) &
                            + AMORT
                     ELSEIF(INCOME_TREATMENT == PURCHASE_POWER) THEN
                        PURCHASE_POWER_AMORT_wide(MO,YR,ASSET_CLASS,ACCT_TYPE) = &
                        PURCHASE_POWER_AMORT_wide(MO,YR,ASSET_CLASS,ACCT_TYPE) + AMORT
                     ELSE   
                        ATL_AMORTIZATION_wide(MO,YR,ASSET_CLASS,ACCT_TYPE) = &
                        ATL_AMORTIZATION_wide(MO,YR,ASSET_CLASS,ACCT_TYPE) + &
                        	 AMORT
                        ATL_AMORT_SUB_ITEMS_wide(MO,YR, &
                        	ASSET_CLASS,BAL_SHEET_POS,ACCT_TYPE) = &
                             + ATL_AMORT_SUB_ITEMS_wide(MO,YR, &
                             	ASSET_CLASS,BAL_SHEET_POS,ACCT_TYPE) &
                             + AMORT
                     ENDIF
                  ENDIF
                  IF(TAX_TIMING_CLASSIFICATION == Temporary) THEN
                     IF(INCOME_TREATMENT == BTL) THEN
                        TAX_TIMING_DIFFERENCES_wide(YR,ASSET_CLASS,2,2, &
                        	ACCT_TYPE) = &
                        TAX_TIMING_DIFFERENCES_wide(YR,ASSET_CLASS,2,2, &
                        	ACCT_TYPE) + AMORT
                     ELSE
                        TAX_TIMING_DIFFERENCES_wide(YR,ASSET_CLASS,2,1,ACCT_TYPE) = &
                        TAX_TIMING_DIFFERENCES_wide(YR,ASSET_CLASS,2,1,ACCT_TYPE) &
                        	 + AMORT
                     ENDIF
                  ELSEIF(TAX_TIMING_CLASSIFICATION == Permanent) THEN
                     IF(INCOME_TREATMENT == BTL) THEN
                        TAX_TIMING_DIFFERENCES_wide(YR,ASSET_CLASS,1,2,ACCT_TYPE) = &
                        TAX_TIMING_DIFFERENCES_wide(YR,ASSET_CLASS,1,2,ACCT_TYPE) + &
                        	 AMORT
                     ELSE
                     TAX_TIMING_DIFFERENCES_wide(YR,ASSET_CLASS,1,1,ACCT_TYPE) = &
                     TAX_TIMING_DIFFERENCES_wide(YR,ASSET_CLASS,1,1,ACCT_TYPE) + &
                     	 AMORT
                     ENDIF
                  ENDIF
                  TAMRTE_wide(MO,YR,ASSET_CLASS,ACCT_TYPE) = &
                                    TAMRTE_wide(MO,YR,ASSET_CLASS,ACCT_TYPE) &
                                    + AMORT
                  TNDB_wide(MO,YR,ASSET_CLASS,BAL_SHEET_POS,ACCT_TYPE) = &
                        TNDB_wide(MO,YR,ASSET_CLASS,BAL_SHEET_POS,ACCT_TYPE) &
                        + NET_DD
                  NET_CHANGE_IN_DB(MO,YR,ASSET_CLASS,BAL_SHEET_POS,ACCT_TYPE) = &
                  NET_CHANGE_IN_DB(MO,YR,ASSET_CLASS,BAL_SHEET_POS,ACCT_TYPE) &
                   + BAL_SHEET_NET_CHANGE
                  TNDB_wide(MO,YR,ASSET_CLASS,Total_Deferred_Debits,ACCT_TYPE) = &
                          TNDB_wide(MO,YR,ASSET_CLASS,Total_Deferred_Debits,ACCT_TYPE) &
                          + NET_DD
                  IF(IN_RATEBASE) THEN
                     TDDRBB_wide(MO,YR,ASSET_CLASS,ACCT_TYPE) = &
                                     TDDRBB_wide(MO,YR,ASSET_CLASS,ACCT_TYPE) &
                                     + NET_DD
                     RB_DD_AMRTE_wide(MO,YR,ASSET_CLASS,ACCT_TYPE) = AMORT &
                              + RB_DD_AMRTE_wide(MO,YR,ASSET_CLASS,ACCT_TYPE)
                  ENDIF
               ENDDO
            ENDDO
            CLASS_POINTER = CLASS_POINTER + 1
            IF(CLASS_POINTER > AVAIL_DATA_YEARS) EXIT
            IF(ASSET_CLASS_LIST(CLASS_POINTER) == -99.) EXIT
         ENDDO

      ENDDO
      CALL CLOSE_DEFERRED_DEBIT_FILE
      IF(LF95 .AND. TACCTS > 0) THEN
         WRITE(SCREEN_MESSAGES,"(I4,A)") TACCTS,"-Debit/Credit Accounts"
         CALL MG_LOCATE_WRITE(16,70,trim(SCREEN_MESSAGES),ALL_VERSIONS,1)
      ENDIF
      DEALLOCATE(ASSET_CLASS_LIST,ASSET_ALLOCATION_LIST,ANN_AMORT, &
                 ANN_CASH_ADDITIONS,MONTHLY_AMORT_AMOUNT)

!
! TOTAL ANNUAL AMOUNTS
!
      ACTIVE_ACCOUNTS = TACCTS_DR + TACCTS_CR
      DO ACCT_TYPE = 1, 2
       IF(ACCT_TYPE == 1 .AND. TACCTS_DR == 0) CYCLE
       IF(ACCT_TYPE == 2 .AND. TACCTS_CR == 0) CYCLE
       DO ASSET_CLASS = 1, NUM_OF_ASSET_CLASSES
         DO YR = 1, FINANCIAL_SIMULATION_YEARS
            DO MO = 1, 12
               DEFERRED_CASH_ADDITIONS_wide(0,YR,ASSET_CLASS,ACCT_TYPE) = &
                            DEFERRED_CASH_ADDITIONS_wide(0,YR,ASSET_CLASS, &
                            ACCT_TYPE) &
                            + DEFERRED_CASH_ADDITIONS_wide(MO,YR,ASSET_CLASS, &
                            	ACCT_TYPE)
               TAMRTE_wide(0,YR,ASSET_CLASS,ACCT_TYPE) = & 
                                   TAMRTE_wide(0,YR,ASSET_CLASS,ACCT_TYPE) &
                                   + TAMRTE_wide(MO,YR,ASSET_CLASS,ACCT_TYPE)
               RB_DD_AMRTE_wide(0,YR,ASSET_CLASS,ACCT_TYPE) = &
                                        RB_DD_AMRTE_wide(0,YR,ASSET_CLASS, &
                                        ACCT_TYPE) &
                                        + RB_DD_AMRTE_wide(MO,YR,ASSET_CLASS, &
                                        ACCT_TYPE)
               ATL_AMORTIZATION_wide(0,YR,ASSET_CLASS,ACCT_TYPE) = &
                                   ATL_AMORTIZATION_wide(0,YR,ASSET_CLASS, &
                                   	ACCT_TYPE) &
                                   + ATL_AMORTIZATION_wide(MO,YR,ASSET_CLASS, &
                                   ACCT_TYPE)
               ATL_DEF_TAX_wide(0,YR,ASSET_CLASS,ACCT_TYPE) = &
                                        ATL_DEF_TAX_wide(0,YR,ASSET_CLASS, &
                                        ACCT_TYPE) &
                                        + ATL_DEF_TAX_wide(MO,YR,ASSET_CLASS, &
                                        ACCT_TYPE)
               BTL_AMORTIZATION_wide(0,YR,ASSET_CLASS,ACCT_TYPE) = &
                                   BTL_AMORTIZATION_wide(0,YR,ASSET_CLASS, &
                                   ACCT_TYPE) &
                                   + BTL_AMORTIZATION_wide(MO,YR,ASSET_CLASS, &
                                   ACCT_TYPE)
               BTL_DEF_TAX_wide(0,YR,ASSET_CLASS,ACCT_TYPE) = &
                                        BTL_DEF_TAX_wide(0,YR,ASSET_CLASS, &
                                        ACCT_TYPE) &
                                        + BTL_DEF_TAX_wide(MO,YR,ASSET_CLASS, &
                                        ACCT_TYPE)
               INTRST_AMORT_wide(0,YR,ASSET_CLASS,ACCT_TYPE) = &
                              INTRST_AMORT_wide(0,YR,ASSET_CLASS, &
                              ACCT_TYPE) &
                              + INTRST_AMORT_wide(MO,YR,ASSET_CLASS, &
                              ACCT_TYPE)
               VARIABLE_OM_AMORT_wide(0,YR,ASSET_CLASS,ACCT_TYPE) = &
                                  VARIABLE_OM_AMORT_wide(0,YR,ASSET_CLASS, &
                                  ACCT_TYPE) &
                                  + VARIABLE_OM_AMORT_wide(MO,YR,ASSET_CLASS, &
                                  ACCT_TYPE)
               OTHER_OM_AMORT_wide(0,YR,ASSET_CLASS,ACCT_TYPE) = &
                                     OTHER_OM_AMORT_wide(0,YR,ASSET_CLASS, &
                                     ACCT_TYPE) &
                                     + OTHER_OM_AMORT_wide(MO,YR,ASSET_CLASS, &
                                     ACCT_TYPE)
               PURCHASE_POWER_AMORT_wide(0,YR,ASSET_CLASS,ACCT_TYPE) = &
                               PURCHASE_POWER_AMORT_wide(0,YR,ASSET_CLASS, &
                               ACCT_TYPE) &
                               + PURCHASE_POWER_AMORT_wide(MO,YR,ASSET_CLASS, &
                               ACCT_TYPE)
               ATL_CURRENT_TAX_EXPENSE_wide(0,YR,ASSET_CLASS,ACCT_TYPE) = &
                            ATL_CURRENT_TAX_EXPENSE_wide(0,YR,ASSET_CLASS, &
                            ACCT_TYPE) &
                            + ATL_CURRENT_TAX_EXPENSE_wide(MO,YR,ASSET_CLASS, &
                            ACCT_TYPE)
               BTL_CURRENT_TAX_EXPENSE_wide(0,YR,ASSET_CLASS,ACCT_TYPE) = &
                            BTL_CURRENT_TAX_EXPENSE_wide(0,YR,ASSET_CLASS, &
                            ACCT_TYPE) &
                            + BTL_CURRENT_TAX_EXPENSE_wide(MO,YR,ASSET_CLASS, &
                            ACCT_TYPE)
               DO B_POS = 1, BAL_SHEET_OPTIONS
                  ATL_AMORT_SUB_ITEMS_wide(0,YR,ASSET_CLASS,B_POS,ACCT_TYPE)= &
                           ATL_AMORT_SUB_ITEMS_wide(MO,YR,ASSET_CLASS,B_POS, &
                                                              ACCT_TYPE) &
                           + ATL_AMORT_SUB_ITEMS_wide(0,YR,ASSET_CLASS,B_POS, &
                                                              ACCT_TYPE)
               ENDDO
            ENDDO
         ENDDO
       ENDDO
      ENDDO

!
!     TOTAL COMPANY
!
      DO ACCT_TYPE = 1, 2
       IF(ACCT_TYPE == 1 .AND. TACCTS_DR == 0) CYCLE
       IF(ACCT_TYPE == 2 .AND. TACCTS_CR == 0) CYCLE
       DO ASSET_CLASS = 1, NUM_OF_ASSET_CLASSES
         DO YR = 0, FINANCIAL_SIMULATION_YEARS
            DO MO = 0, 12 !1, 12
               TDDRBB_wide(MO,YR,0,ACCT_TYPE) = TDDRBB_wide(MO,YR,0,ACCT_TYPE) &
                                 + TDDRBB_wide(MO,YR,ASSET_CLASS,ACCT_TYPE)
               TAMRTE_wide(MO,YR,0,ACCT_TYPE) = TAMRTE_wide(MO,YR,0,ACCT_TYPE) &
                                 + TAMRTE_wide(MO,YR,ASSET_CLASS,ACCT_TYPE)
               DEFERRED_CASH_ADDITIONS_wide(MO,YR,0,ACCT_TYPE) = &
                    DEFERRED_CASH_ADDITIONS_wide(MO,YR,ASSET_CLASS,ACCT_TYPE) &
                    + DEFERRED_CASH_ADDITIONS_wide(MO,YR,0,ACCT_TYPE)
               RB_DD_AMRTE_wide(MO,YR,0,ACCT_TYPE) = &
                              RB_DD_AMRTE_wide(MO,YR,0,ACCT_TYPE) &
                              + RB_DD_AMRTE_wide(MO,YR,ASSET_CLASS,ACCT_TYPE)
               ATL_AMORTIZATION_wide(MO,YR,0,ACCT_TYPE) = &
                         ATL_AMORTIZATION_wide(MO,YR,0,ACCT_TYPE) &
                         + ATL_AMORTIZATION_wide(MO,YR,ASSET_CLASS,ACCT_TYPE) 
               ATL_DEF_TAX_wide(MO,YR,0,ACCT_TYPE) = &
                              ATL_DEF_TAX_wide(MO,YR,0,ACCT_TYPE) &
                              + ATL_DEF_TAX_wide(MO,YR,ASSET_CLASS,ACCT_TYPE) 
               BTL_AMORTIZATION_wide(MO,YR,0,ACCT_TYPE) = &
                         BTL_AMORTIZATION_wide(MO,YR,0,ACCT_TYPE) &
                         + BTL_AMORTIZATION_wide(MO,YR,ASSET_CLASS,ACCT_TYPE)
               BTL_DEF_TAX_wide(MO,YR,0,ACCT_TYPE) = &
                              BTL_DEF_TAX_wide(MO,YR,0,ACCT_TYPE) &
                              + BTL_DEF_TAX_wide(MO,YR,ASSET_CLASS,ACCT_TYPE)
               INTRST_AMORT_wide(MO,YR,0,ACCT_TYPE) = &
                    INTRST_AMORT_wide(MO,YR,0,ACCT_TYPE) &
                    + INTRST_AMORT_wide(MO,YR,ASSET_CLASS,ACCT_TYPE) 
!
               VARIABLE_OM_AMORT_wide(MO,YR,0,ACCT_TYPE) = &
                        VARIABLE_OM_AMORT_wide(MO,YR,0,ACCT_TYPE) &
                        + VARIABLE_OM_AMORT_wide(MO,YR,ASSET_CLASS,ACCT_TYPE)
               OTHER_OM_AMORT_wide(MO,YR,0,ACCT_TYPE) = &
                           OTHER_OM_AMORT_wide(MO,YR,0,ACCT_TYPE) &
                           + OTHER_OM_AMORT_wide(MO,YR,ASSET_CLASS,ACCT_TYPE)
               PURCHASE_POWER_AMORT_wide(MO,YR,0,ACCT_TYPE) = &
                     PURCHASE_POWER_AMORT_wide(MO,YR,0,ACCT_TYPE) &
                     + PURCHASE_POWER_AMORT_wide(MO,YR,ASSET_CLASS,ACCT_TYPE)
               ATL_CURRENT_TAX_EXPENSE_wide(MO,YR,0,ACCT_TYPE) = &
                  ATL_CURRENT_TAX_EXPENSE_wide(MO,YR,0,ACCT_TYPE) &
                  + ATL_CURRENT_TAX_EXPENSE_wide(MO,YR,ASSET_CLASS,ACCT_TYPE)
               BTL_CURRENT_TAX_EXPENSE_wide(MO,YR,0,ACCT_TYPE) = &
                  BTL_CURRENT_TAX_EXPENSE_wide(MO,YR,0,ACCT_TYPE) &
                  + BTL_CURRENT_TAX_EXPENSE_wide(MO,YR,ASSET_CLASS,ACCT_TYPE)
               DO B_POS = 1,BAL_SHEET_OPTIONS 
                  ATL_AMORT_SUB_ITEMS_wide(MO,YR,0,B_POS,ACCT_TYPE) = &
                          ATL_AMORT_SUB_ITEMS_wide(MO,YR,ASSET_CLASS,B_POS,ACCT_TYPE) &
                          + ATL_AMORT_SUB_ITEMS_wide(MO,YR,0,B_POS,ACCT_TYPE)
                  TDDB_wide(MO,YR,0,BAL_SHEET_POS,ACCT_TYPE) = &
                               TDDB_wide(MO,YR,0,B_POS,ACCT_TYPE) &
                               + TDDB_wide(MO,YR,ASSET_CLASS,B_POS,ACCT_TYPE)
                  TDDB_wide(MO,YR,0,Total_Deferred_Debits,ACCT_TYPE) = &
                           TDDB_wide(MO,YR,0,Total_Deferred_Debits,ACCT_TYPE) &
                           + TDDB_wide(MO,YR,ASSET_CLASS,B_POS,ACCT_TYPE)
                  TNDB_wide(MO,YR,0,BAL_SHEET_POS,ACCT_TYPE) = &
                               TNDB_wide(MO,YR,0,B_POS,ACCT_TYPE) &
                               + TNDB_wide(MO,YR,ASSET_CLASS,B_POS,ACCT_TYPE)
                  TNDB_wide(MO,YR,0,Total_Deferred_Debits,ACCT_TYPE) = &
                           TNDB_wide(MO,YR,0,Total_Deferred_Debits,ACCT_TYPE) &
                           + TNDB_wide(MO,YR,ASSET_CLASS,B_POS,ACCT_TYPE)
               ENDDO
            ENDDO
         ENDDO
       ENDDO
       
       MO = 0
       DO ASSET_CLASS = 0, NUM_OF_ASSET_CLASSES
         TCAMRT_wide(MO,1,ASSET_CLASS,ACCT_TYPE) = TAMRTE_wide(MO,1,ASSET_CLASS,ACCT_TYPE)
         DO YR = 1, FINANCIAL_SIMULATION_YEARS
            TCAMRT_wide(MO,YR,ASSET_CLASS,ACCT_TYPE) = &
                                   TCAMRT_wide(MO,YR-1,ASSET_CLASS,ACCT_TYPE) &
                                   + TAMRTE_wide(MO,YR,ASSET_CLASS,ACCT_TYPE)
            DO B_POS = 1,BAL_SHEET_OPTIONS 
               ATL_AMORT_SUB_ITEMS_wide(MO,YR,ASSET_CLASS,0,ACCT_TYPE) = &
                      ATL_AMORT_SUB_ITEMS_wide(MO,YR,ASSET_CLASS,0,ACCT_TYPE) &
                      + ATL_AMORT_SUB_ITEMS_wide(MO,YR,0,B_POS,ACCT_TYPE)
            ENDDO
         ENDDO
       ENDDO
      ENDDO

! REPORT TOTALS

      IF(REPORTS_ACTIVE .AND. TACCTS > 1) THEN

        INDEX_NAME = 'Total Debit Accounts'
        MO = 0
        DO ACCT_TYPE = 1, 2
         IF(ACCT_TYPE == 1 .AND. TACCTS_DR == 0) CYCLE
         IF(ACCT_TYPE == 2 .AND. TACCTS_CR == 0) CYCLE
         DEBIT_OPENING_NET_DD = TNDB_wide(12,0,0,Total_Deferred_Debits,ACCT_TYPE)        
         INTEREST_OPENING_NET_DD = 0. ! TNDB_wide(12,0,0,
         DO YR = 1, FINANCIAL_SIMULATION_YEARS
!
            DEBIT_CLOSING_NET_DD = TNDB_wide(12,YR,0,Total_Deferred_Debits,ACCT_TYPE)        
            INTEREST_CLOSING_NET_DD = 0. ! TNDB_wide(12,YR,0,
!
            WRITE(REPORTING_UNIT,REC=REPORTING_REC) PRT_ENDPOINT(), &
                           INDEX_NAME, &
                           FLOAT(YR+get_base_year()), &
                           DEBIT_OPENING_NET_DD, &
                           DEFERRED_CASH_ADDITIONS_wide(MO,YR,0,ACCT_TYPE), &
                           ATL_AMORTIZATION_wide(MO,YR,0,ACCT_TYPE), &
                           INTRST_AMORT_wide(MO,YR,0,ACCT_TYPE), &
                           BTL_AMORTIZATION_wide(MO,YR,0,ACCT_TYPE), &
                           DEBIT_CLOSING_NET_DD, &
                          (TDDRBB_wide(12,YR-1,0,ACCT_TYPE)&
                          +TDDRBB_wide(12,YR,0,ACCT_TYPE))/2., &
                          ATL_DEF_TAX_wide(MO,YR,0,ACCT_TYPE), &
                           BTL_DEF_TAX_wide(MO,YR,0,ACCT_TYPE), &
                           ATL_CURRENT_TAX_EXPENSE_wide(MO,YR,0,ACCT_TYPE), &
                           BTL_CURRENT_TAX_EXPENSE_wide(MO,YR,0,ACCT_TYPE), &
                           VARIABLE_OM_AMORT_wide(MO,YR,0,ACCT_TYPE), &
                           INTEREST_OPENING_NET_DD, &
                           INTEREST_CLOSING_NET_DD, &
                           OTHER_OM_AMORT_wide(MO,YR,0,ACCT_TYPE), &
                           PURCHASE_POWER_AMORT_wide(MO,YR,0,ACCT_TYPE), &
                          (ATL_AMORT_SUB_ITEMS_wide(MO,YR,0,B_POS,ACCT_TYPE), &
                          B_POS = 1, BAL_SHEET_OPTIONS)
            REPORTING_REC = REPORTING_REC + 1
!
            DEBIT_OPENING_NET_DD = DEBIT_CLOSING_NET_DD
            INTEREST_OPENING_NET_DD = INTEREST_CLOSING_NET_DD
         ENDDO
         INDEX_NAME = 'Total Credit Accounts'
        ENDDO
      ENDIF
      IF(SAVE_BASE_CASE) THEN
         CALL OPEN_DD_BASE_CASE_FILE(INT(10,2))
         WRITE(10) TDDB_wide,TDDRBB_wide,TAMRTE_wide,TNDB_wide, &
            TCAMRT_wide, &
            RB_DD_AMRTE_wide, &
                   INTRST_AMORT_wide, &
                   ATL_AMORTIZATION_wide,BTL_AMORTIZATION_wide, &
                   ATL_DEF_TAX_wide,BTL_DEF_TAX_wide, &
                   DEFERRED_CASH_ADDITIONS_wide, &
                   VARIABLE_OM_AMORT_wide, &
                   ATL_CURRENT_TAX_EXPENSE_wide, &
                   BTL_CURRENT_TAX_EXPENSE_wide, &
                   TAX_TIMING_DIFFERENCES_wide, &
                   OTHER_OM_AMORT_wide, &
                   PURCHASE_POWER_AMORT_wide, &
                   ATL_AMORT_SUB_ITEMS_wide, &
                   NET_CHANGE_IN_DB
         CLOSE(10)
      ENDIF
      
      RETURN

      ENTRY READ_DEBIT_BASE_CASE

         CALL RESET_DEBITOL
         CALL SET_UP_DD_ARRAYS
         CALL OPEN_DD_BASE_CASE_FILE(INT(10,2))
         READ(10) TDDB_wide,TDDRBB_wide,TAMRTE_wide,TNDB_wide,TCAMRT_wide, &
            RB_DD_AMRTE_wide, &
                  INTRST_AMORT_wide, &
                  ATL_AMORTIZATION_wide,BTL_AMORTIZATION_wide, &
                  ATL_DEF_TAX_wide,BTL_DEF_TAX_wide, &
                  DEFERRED_CASH_ADDITIONS_wide, &
                  VARIABLE_OM_AMORT_wide, &
                  ATL_CURRENT_TAX_EXPENSE_wide, &
                  BTL_CURRENT_TAX_EXPENSE_wide, &
                  TAX_TIMING_DIFFERENCES_wide, &
                  OTHER_OM_AMORT_wide, &
                  PURCHASE_POWER_AMORT_wide, &
                  ATL_AMORT_SUB_ITEMS_wide, &
                  NET_CHANGE_IN_DB
         CLOSE(10)
      RETURN

      ENTRY RETURN_TOTAL_DEFERRED_DEBITS(R_YR,R_TDDB,R_TDDRBB,R_TAMRTE, &
                                         R_TNDB,R_TCAMRT,R_RB_DD_AMRTE)

         MO = 0
         R_TDDB = TDDB_wide(MO,R_YR,0,Total_Deferred_Debits,DEBIT) 
         R_TDDRBB = TDDRBB_wide(MO,R_YR,0,DEBIT)
         R_TAMRTE = TAMRTE_wide(MO,R_YR,0,DEBIT)
         R_TNDB = TNDB_wide(MO,R_YR,0,Total_Deferred_Debits,DEBIT)
         R_RB_DD_AMRTE = RB_DD_AMRTE_wide(MO,R_YR,0,DEBIT)
         R_TCAMRT = TCAMRT_wide(MO,R_YR,0,DEBIT)
      RETURN

      ENTRY DEBIT_BY_INFO(R_CLASS,R_TNDB,R_TDDRBB, &
                          R_UNAMORTIZED_INTEREST_BALANCE, &
                          R_OTHER_DD_BALANCE, &
                          R_GOODWILL, &
                          R_CLASS_REGULATORY_ASSETS, &
                          R_FASB_109, &
                          R_FASB_133, &
                          UNAMORTIZED_ISSUE_EXP_BALANCE)

         R_TNDB = 0.
         R_TDDRBB = 0.
         R_OTHER_DD_BALANCE = 0.
         R_GOODWILL = 0.
         R_CLASS_REGULATORY_ASSETS = 0.
         R_FASB_109 = 0.
         R_FASB_133 = 0.
         R_UNAMORTIZED_INTEREST_BALANCE = 0.
         UNAMORTIZED_ISSUE_EXP_BALANCE = 0.
         MO = 12
         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN 
               R_TNDB=TNDB_wide(MO,0,ASSET_CLASS,Total_Deferred_Debits,DEBIT)
               R_OTHER_DD_BALANCE = TNDB_wide(MO,0,ASSET_CLASS, &
               Other_Deferred_Debits,DEBIT)
               R_GOODWILL = TNDB_wide(MO,0,ASSET_CLASS,Goodwill,DEBIT)
               R_CLASS_REGULATORY_ASSETS = TNDB_wide(MO,0,ASSET_CLASS, &
               	RegulatoryAssets,DEBIT)
               R_FASB_109 = TNDB_wide(MO,0,ASSET_CLASS,FASB109,DEBIT)
               R_FASB_133 = TNDB_wide(MO,0,ASSET_CLASS,FASB133,DEBIT)
               R_TDDRBB = TDDRBB_wide(MO,0,ASSET_CLASS,DEBIT)
               R_UNAMORTIZED_INTEREST_BALANCE = TNDB_wide(MO,0,ASSET_CLASS, &
               	Unamortized_InterestBalance,DEBIT)
               UNAMORTIZED_ISSUE_EXP_BALANCE = TNDB_wide(MO,0,ASSET_CLASS, &
               Unamortized_Issue_Expen_Balance,DEBIT)
!
            ENDIF
         ENDIF
      RETURN

      ENTRY DEFERRED_CREDITS_BY_INFO(R_CLASS, &
                                     R_DEF_CREDITS_IN_RB, &
                                     R_OTHER_DEF_CREDITS, &
                                     R_RegulatoryLiabilites, &
                                     R_GainOnReaquiredDebt, &
                                     R_AssetSalesDeferredGain)

         R_DEF_CREDITS_IN_RB = 0.
         R_OTHER_DEF_CREDITS = 0.
         R_RegulatoryLiabilites = 0.
         R_GainOnReaquiredDebt = 0.

         MO = 12
         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_CLASS == 0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN 
               R_OTHER_DEF_CREDITS = TNDB_wide(MO,0,ASSET_CLASS, &
               	Other_Deferred_Credit,CREDIT)
               R_RegulatoryLiabilites = TNDB_wide(MO,0,ASSET_CLASS, &
               	Regulatory_Liabilites,CREDIT)
               R_GainOnReaquiredDebt = TNDB_wide(MO,0,ASSET_CLASS, &
               	Gain_on_Reaquired_Debt,CREDIT)
               R_AssetSalesDeferredGain = &
                           R_AssetSalesDeferredGain &
                           + TNDB_wide(MO,0,ASSET_CLASS, &
                                       Asset_Sales_Deferred_Gain,CREDIT)
               R_DEF_CREDITS_IN_RB = TDDRBB_wide(MO,0,ASSET_CLASS,CREDIT)
            ENDIF
         ENDIF
      RETURN

      ENTRY DEBIT_INFO(R_YR,R_CLASS,R_CLASS_EXISTS, &
                       R_TDDB,R_TDDRBB,R_TAMRTE, &
                       R_TNDB,R_TCAMRT,R_RB_DD_AMRTE, &
                       R_INTEREST_AMORTIZATION, &
                       R_ATL_AMORTIZATION, &
                       R_BTL_AMORTIZATION, &
                       R_ATL_DEF_TAX, &
                       R_BTL_DEF_TAX,&
                       R_UNAMORTIZED_INTEREST_BALANCE, &
                       R_DEFERRED_CASH_ADDITIONS, &
                       R_VARIABLE_OM_AMORT, &
                       R_ATL_CURRENT_TAX_EXPENSE, &
                       R_BTL_CURRENT_TAX_EXPENSE, &
                       R_OTHER_OM_AMORT, &
                       R_PURCHASE_POWER_AMORT, &
                       R_OTHER_DD_BALANCE, &
                       R_GOODWILL, &
                       R_CLASS_REGULATORY_ASSETS, &
                       R_FASB_109, &
                       R_FASB_133, &
                       R_OTHER_DD_AMORT, &
                       R_GOODWILL_AMORT, &
                       R_CLASS_REGULATORY_ASSETS_AMORT, &
                       R_FASB_109_AMORT, &
                       R_FASB_133_AMORT, &
                       UNAMORTIZED_ISSUE_EXP_BALANCE, &
! DEFERRED CREDITS
                       R_DEF_CREDITS_IN_RB, &
                       R_OTHER_DEF_CREDITS, &
                       R_RegulatoryLiabilites, &
                       R_GainOnReaquiredDebt, &
                       R_AssetSalesDeferredGain)

!
         R_CLASS_EXISTS = .FALSE.
         R_TDDB = 0.
         R_RB_DD_AMRTE = 0.
         R_TCAMRT = 0.
         R_INTEREST_AMORTIZATION = 0.
         R_ATL_AMORTIZATION = 0.
         R_BTL_AMORTIZATION = 0.
         R_ATL_DEF_TAX = 0.
         R_BTL_DEF_TAX = 0.
         R_UNAMORTIZED_INTEREST_BALANCE = 0.
         UNAMORTIZED_ISSUE_EXP_BALANCE = 0.
         R_DEFERRED_CASH_ADDITIONS = 0.
         R_ATL_CURRENT_TAX_EXPENSE = 0.
         R_BTL_CURRENT_TAX_EXPENSE = 0.
         R_PURCHASE_POWER_AMORT = 0.
         R_GOODWILL = 0.
         R_CLASS_REGULATORY_ASSETS = 0.
         R_FASB_109 = 0.
         R_FASB_133 = 0.
         R_OTHER_DD_AMORT = 0.
         R_GOODWILL_AMORT = 0.
         R_CLASS_REGULATORY_ASSETS_AMORT = 0.
         R_FASB_109_AMORT = 0.
         R_FASB_133_AMORT = 0.
         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN 
               R_CLASS_EXISTS = .TRUE.
               MO = 0
               MO_BAL = 0 ! 12
               YR = R_YR - 1
               YR_BAL = R_YR
!
! BALANCE SHEET YEAR END IN THE 12 MONTH
!
               R_TDDB = TDDB_wide(MO_BAL,YR_BAL,ASSET_CLASS, &
               	Total_Deferred_Debits,DEBIT)
               R_TDDRBB = R_TDDRBB + TDDRBB_wide(MO_BAL,YR_BAL,ASSET_CLASS,DEBIT)
               R_TNDB = R_TNDB + TNDB_wide(MO_BAL,YR_BAL,ASSET_CLASS, &
               Total_Deferred_Debits,DEBIT)
               R_OTHER_DD_BALANCE = R_OTHER_DD_BALANCE + &
               TNDB_wide(MO_BAL,YR_BAL,ASSET_CLASS, &
               Other_Deferred_Debits,DEBIT)
               R_GOODWILL=TNDB_wide(MO_BAL,YR_BAL,ASSET_CLASS,Goodwill,DEBIT)
               R_CLASS_REGULATORY_ASSETS = TNDB_wide(MO_BAL,YR_BAL,ASSET_CLASS, &
               RegulatoryAssets,DEBIT)
               R_FASB_109=TNDB_wide(MO_BAL,YR_BAL,ASSET_CLASS,FASB109,DEBIT)
               R_FASB_133=TNDB_wide(MO_BAL,YR_BAL,ASSET_CLASS,FASB133,DEBIT)
!
! CREDIT BALANCES
!
               R_OTHER_DEF_CREDITS = TNDB_wide(MO,YR_BAL,ASSET_CLASS, &
               	Other_Deferred_Credit,CREDIT)
               R_RegulatoryLiabilites = TNDB_wide(MO,YR_BAL,ASSET_CLASS, &
               	Regulatory_Liabilites,CREDIT)
               R_GainOnReaquiredDebt = TNDB_wide(MO,YR_BAL,ASSET_CLASS, &
               	Gain_on_Reaquired_Debt,CREDIT)
               R_AssetSalesDeferredGain = &
                           R_AssetSalesDeferredGain &
                           + TNDB_wide(MO,YR_BAL,ASSET_CLASS, &
                                       Asset_Sales_Deferred_Gain,CREDIT)
               R_DEF_CREDITS_IN_RB=TDDRBB_wide(MO,YR_BAL,ASSET_CLASS,CREDIT)
!
! END BALANCE SHEET ITEMS
!
               R_OTHER_DD_AMORT = ATL_AMORT_SUB_ITEMS_wide(MO,YR,ASSET_CLASS, &
               	Other_Deferred_Debits,DEBIT)
               R_GOODWILL_AMORT = ATL_AMORT_SUB_ITEMS_wide(MO,YR,ASSET_CLASS, &
               Goodwill,DEBIT)
               R_CLASS_REGULATORY_ASSETS_AMORT = ATL_AMORT_SUB_ITEMS_wide(MO, &
               	YR,ASSET_CLASS,RegulatoryAssets,DEBIT)
               R_FASB_109_AMORT = ATL_AMORT_SUB_ITEMS_wide(MO,YR, &
               	ASSET_CLASS,FASB109,DEBIT)
               R_FASB_133_AMORT = ATL_AMORT_SUB_ITEMS_wide(MO,YR, &
               	ASSET_CLASS,FASB133,DEBIT)
               R_TCAMRT = TCAMRT_wide(MO_BAL,YR_BAL,ASSET_CLASS,DEBIT)
               R_UNAMORTIZED_INTEREST_BALANCE = TNDB_wide(MO_BAL,YR_BAL,ASSET_CLASS, &
               	Unamortized_InterestBalance,DEBIT)
               UNAMORTIZED_ISSUE_EXP_BALANCE = TNDB_wide(MO_BAL,YR_BAL,ASSET_CLASS, &
               	Unamortized_Issue_Expen_Balance,DEBIT)
!
!
! ANNUAL TOTAL DEBIT VALUES
!
               R_RB_DD_AMRTE = RB_DD_AMRTE_wide(MO,YR,ASSET_CLASS,DEBIT)
               R_TAMRTE = R_TAMRTE + TAMRTE_wide(MO,YR,ASSET_CLASS,DEBIT)
               R_INTEREST_AMORTIZATION = &
               	INTRST_AMORT_wide(MO,YR,ASSET_CLASS,DEBIT)
               R_ATL_AMORTIZATION = ATL_AMORTIZATION_wide(MO,YR,ASSET_CLASS,DEBIT)
               R_BTL_AMORTIZATION = BTL_AMORTIZATION_wide(MO,YR,ASSET_CLASS,DEBIT)
               R_ATL_DEF_TAX = ATL_DEF_TAX_wide(MO,YR,ASSET_CLASS,DEBIT)
               R_BTL_DEF_TAX = BTL_DEF_TAX_wide(MO,YR,ASSET_CLASS,DEBIT)
               R_DEFERRED_CASH_ADDITIONS = &
               	DEFERRED_CASH_ADDITIONS_wide(MO,YR,ASSET_CLASS,DEBIT)
               R_VARIABLE_OM_AMORT = R_VARIABLE_OM_AMORT + &
               	VARIABLE_OM_AMORT_wide(MO,YR,ASSET_CLASS,DEBIT)
               R_OTHER_OM_AMORT = R_OTHER_OM_AMORT + &
               	OTHER_OM_AMORT_wide(MO,YR,ASSET_CLASS,DEBIT)
               R_PURCHASE_POWER_AMORT = R_PURCHASE_POWER_AMORT + &
               	PURCHASE_POWER_AMORT_wide(MO,YR,ASSET_CLASS,DEBIT)
               R_ATL_CURRENT_TAX_EXPENSE = &
               	ATL_CURRENT_TAX_EXPENSE_wide(MO,YR,ASSET_CLASS,DEBIT)
               R_BTL_CURRENT_TAX_EXPENSE = &
               	BTL_CURRENT_TAX_EXPENSE_wide(MO,YR,ASSET_CLASS,DEBIT)
!
! ANNUAL TOTAL CREDIT VALUES
!
               R_RB_DD_AMRTE = R_RB_DD_AMRTE - RB_DD_AMRTE_wide(MO,YR,ASSET_CLASS,CREDIT)
               R_TAMRTE = R_TAMRTE - TAMRTE_wide(MO,YR,ASSET_CLASS,CREDIT)
               R_INTEREST_AMORTIZATION = R_INTEREST_AMORTIZATION - &
               	 INTRST_AMORT_wide(MO,YR,ASSET_CLASS,CREDIT)
               R_ATL_AMORTIZATION = R_ATL_AMORTIZATION - &
               	ATL_AMORTIZATION_wide(MO,YR,ASSET_CLASS,CREDIT)
               R_BTL_AMORTIZATION = R_BTL_AMORTIZATION - &
               	BTL_AMORTIZATION_wide(MO,YR,ASSET_CLASS,CREDIT)
               R_ATL_DEF_TAX = R_ATL_DEF_TAX - &
               	ATL_DEF_TAX_wide(MO,YR,ASSET_CLASS,CREDIT)
               R_BTL_DEF_TAX = R_BTL_DEF_TAX - &
               	BTL_DEF_TAX_wide(MO,YR,ASSET_CLASS,CREDIT)
               R_DEFERRED_CASH_ADDITIONS = R_DEFERRED_CASH_ADDITIONS &
               	 - DEFERRED_CASH_ADDITIONS_wide(MO,YR,ASSET_CLASS,CREDIT)
               R_VARIABLE_OM_AMORT = R_VARIABLE_OM_AMORT &
               	 - VARIABLE_OM_AMORT_wide(MO,YR,ASSET_CLASS,CREDIT)
               R_OTHER_OM_AMORT = R_OTHER_OM_AMORT &
               	 - OTHER_OM_AMORT_wide(MO,YR,ASSET_CLASS,CREDIT)
               R_PURCHASE_POWER_AMORT = R_PURCHASE_POWER_AMORT &
               	 - PURCHASE_POWER_AMORT_wide(MO,YR,ASSET_CLASS,CREDIT)
               R_ATL_CURRENT_TAX_EXPENSE = R_ATL_CURRENT_TAX_EXPENSE &
               	 - ATL_CURRENT_TAX_EXPENSE_wide(MO,YR,ASSET_CLASS,CREDIT)
               R_BTL_CURRENT_TAX_EXPENSE = R_BTL_CURRENT_TAX_EXPENSE &
               	 -  BTL_CURRENT_TAX_EXPENSE_wide(MO,YR,ASSET_CLASS,CREDIT)
            ENDIF
         ENDIF
      RETURN

      ENTRY RETURN_DEBIT_MONTHLY_VALUES(R_CLASS,R_YR, &
                                        INCOME_VARIABLES, &
                                        CASH_VARIABLES)

!
!
         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN 
               YR = R_YR - 1
               DO MO = 0, 12
                  INCOME_VARIABLES(MO, &
                  	mty_dbt_fl_intrst_amrt) = &
                             INTRST_AMORT_wide(MO,YR,ASSET_CLASS,Dr)
                  INCOME_VARIABLES(MO, &
                  	mty_dbt_fl_intrst_amrt_cr) = &
                             INTRST_AMORT_wide(MO,YR,ASSET_CLASS,Cr)
                  INCOME_VARIABLES(MO, &
                  	mt_dbt_fl_chg_intrst_exp_amrt) = &
                           NET_CHANGE_IN_DB(MO,YR,ASSET_CLASS, &
                           	Unamortized_InterestBalance,Dr)
                  INCOME_VARIABLES(MO, &
                  	mt_chg_dbt_fl_issu_exp_amrt) = &
                           NET_CHANGE_IN_DB(MO,YR,ASSET_CLASS, &
                           	Unamortized_Issue_Expen_Balance,Dr)
!
                  INCOME_VARIABLES(MO,monthly_variable_oandm) = &
                             INCOME_VARIABLES(MO,monthly_variable_oandm) &
                             + VARIABLE_OM_AMORT_wide(MO,YR,ASSET_CLASS,Dr)
                  INCOME_VARIABLES(MO,monthly_other_oandm) = &
                                INCOME_VARIABLES(MO,monthly_other_oandm) &
                                + OTHER_OM_AMORT_wide(MO,YR,ASSET_CLASS,Dr)
                  INCOME_VARIABLES(MO,monthly_purchased_power) = &
                            INCOME_VARIABLES(MO,monthly_purchased_power) &
                            + PURCHASE_POWER_AMORT_wide(MO,YR,ASSET_CLASS,Dr)
                  INCOME_VARIABLES(MO,mthly_debit_fl_Pchs_pwr_amort) = &
                              PURCHASE_POWER_AMORT_wide(MO,YR,ASSET_CLASS,Dr)
                  CASH_VARIABLES(MO,cash_deferred_expenses) = &
                           DEFERRED_CASH_ADDITIONS_wide(MO,YR,ASSET_CLASS,Dr)
                  INCOME_VARIABLES(MO,Monthly_Income_Tax_Deferrals_Cr) = &
                                 INCOME_VARIABLES(MO,Monthly_Income_Tax_Deferrals_Cr) &
                                 + ATL_DEF_TAX_wide(MO,YR,ASSET_CLASS,Dr)
                  INCOME_VARIABLES(MO,BTL_Mthly_Income_Tax_Deferrals) = &
                                 INCOME_VARIABLES(MO,BTL_Mthly_Income_Tax_Deferrals) &
                                 + BTL_DEF_TAX_wide(MO,YR,ASSET_CLASS,Dr)
                  INCOME_VARIABLES(MO,Monthly_Amortization) = &
                                INCOME_VARIABLES(MO,Monthly_Amortization) &
                                + ATL_AMORTIZATION_wide(MO,YR,ASSET_CLASS,Dr)
!
                  INCOME_VARIABLES(MO,monthly_other_btl_amort) = &
                                INCOME_VARIABLES(MO,monthly_other_btl_amort) &
                                + BTL_AMORTIZATION_wide(MO, &
                                YR,ASSET_CLASS,Dr)
!
                  INCOME_VARIABLES(MO,Monthly_Goodwill_Amort) = &
                                INCOME_VARIABLES(MO,Monthly_Goodwill_Amort) &
                                + ATL_AMORT_SUB_ITEMS_wide(MO,YR,ASSET_CLASS, &
                                	Goodwill,Dr)
                  INCOME_VARIABLES(MO,Monthly_Regulatory_Assets_Amort) = &
                                INCOME_VARIABLES(MO,Monthly_Regulatory_Assets_Amort) &
                                + ATL_AMORT_SUB_ITEMS_wide(MO,YR,ASSET_CLASS,RegulatoryAssets,Dr)
                  INCOME_VARIABLES(MO,Monthly_FASB_109_Amort) = &
                                INCOME_VARIABLES(MO,Monthly_FASB_109_Amort) &
                               + ATL_AMORT_SUB_ITEMS_wide(MO,YR,ASSET_CLASS,FASB109,Dr)
                  INCOME_VARIABLES(MO,Monthly_FASB_133_Amort) = &
                                INCOME_VARIABLES(MO,Monthly_FASB_133_Amort) &
                                + ATL_AMORT_SUB_ITEMS_wide(MO,YR,ASSET_CLASS,FASB133,Dr)
                  INCOME_VARIABLES(MO,monthly_other_atl_amort) = &
                            INCOME_VARIABLES(MO,monthly_other_atl_amort) &
                            + ATL_AMORT_SUB_ITEMS_wide(MO,YR,ASSET_CLASS,Other_Deferred_Debits,Dr)

! CREDIT AMORTIZATIONS
!
                  INCOME_VARIABLES(MO,monthly_other_atl_amort) = &
                            INCOME_VARIABLES(MO,monthly_other_atl_amort) &
                            - ATL_AMORT_SUB_ITEMS_wide(MO, &
                            	YR,ASSET_CLASS,Other_Deferred_Debits,Cr)

                  INCOME_VARIABLES(MO,Monthly_ATL_Gain_Amort) = &
                            INCOME_VARIABLES(MO,Monthly_ATL_Gain_Amort) &
                            + ATL_AMORTIZATION_wide(MO,YR,ASSET_CLASS,CREDIT)
                  INCOME_VARIABLES(MO,Monthly_BTL_Gain_Amort) = &
                            INCOME_VARIABLES(MO,Monthly_BTL_Gain_Amort) &
                            + BTL_AMORTIZATION_wide(MO,YR,ASSET_CLASS,CREDIT)
                  INCOME_VARIABLES(MO,Monthly_Regulatory_Assets_Amort) = &
                                INCOME_VARIABLES(MO,Monthly_Regulatory_Assets_Amort) &
                                - ATL_AMORT_SUB_ITEMS_wide(MO,YR,ASSET_CLASS,RegulatoryAssets,Cr)
                  INCOME_VARIABLES(MO,monthly_variable_oandm) = &
                             INCOME_VARIABLES(MO,monthly_variable_oandm) &
                             - VARIABLE_OM_AMORT_wide(MO,YR,ASSET_CLASS,Cr)
                  INCOME_VARIABLES(MO,monthly_other_oandm) = &
                                INCOME_VARIABLES(MO,monthly_other_oandm) &
                                - OTHER_OM_AMORT_wide(MO,YR,ASSET_CLASS,Cr)
                  INCOME_VARIABLES(MO,monthly_purchased_power) = &
                            INCOME_VARIABLES(MO,monthly_purchased_power) &
                            - PURCHASE_POWER_AMORT_wide(MO,YR,ASSET_CLASS,Cr)
               ENDDO
            ENDIF
         ENDIF
      RETURN

      ENTRY RETURN_DEBIT_MONTHLY_TAX_VALUES(R_CLASS,R_YR, &
                                            TAX_VARIABLES)

!
         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN 
               YR = R_YR - 1
               DO MO = 0, 12
                  TAX_VARIABLES(MO,monthly_atl_debit_tax_expense) = &
                         ATL_CURRENT_TAX_EXPENSE_wide(MO,YR,ASSET_CLASS,Dr) &
                         - ATL_CURRENT_TAX_EXPENSE_wide(MO,YR,ASSET_CLASS,Cr)
                  TAX_VARIABLES(MO,monthly_btl_debit_tax_expense) = &
                         BTL_CURRENT_TAX_EXPENSE_wide(MO,YR,ASSET_CLASS,Dr) &
                         - BTL_CURRENT_TAX_EXPENSE_wide(MO,YR,ASSET_CLASS,Cr)
               ENDDO
            ENDIF
         ENDIF
      RETURN

      ENTRY MONTHLY_DEFERRED_DEBIT_BS_INFO(R_YR,R_CLASS, &
                                           BALANCE_VARIABLES)

!
!
         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_CLASS == 0) THEN
               ASSET_CLASS = 0
            ELSEIF(R_CLASS == -1) THEN
               ASSET_CLASS = -1
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
               YR = R_YR ! - 1
               DO MO = 0, 12
                  BALANCE_VARIABLES(MO,monthly_net_goodwill) = &
                  	TNDB_wide(MO,YR,ASSET_CLASS,Goodwill,Dr)
                  BALANCE_VARIABLES(MO,monthly_net_regulatory_assets) = &
                  	TNDB_wide(MO,YR,ASSET_CLASS,RegulatoryAssets,Dr)
                  BALANCE_VARIABLES(MO,mty_net_fasb_109) = &
                  	TNDB_wide(MO,YR,ASSET_CLASS,FASB109,Dr)
                  BALANCE_VARIABLES(MO,mty_net_fasb_133) = &
                  	TNDB_wide(MO,YR,ASSET_CLASS,FASB133,Dr)
! DEFERRED CREDITS
                  BALANCE_VARIABLES(MO,monthly_gain_on_reaqd_debt_bal) = &
                       TNDB_wide(MO,YR,ASSET_CLASS,Gain_on_Reaquired_Debt,Cr)
                  BALANCE_VARIABLES(MO,mty_regty_liablties_bal) = &
                        TNDB_wide(MO,YR,ASSET_CLASS,Regulatory_Liabilites,Cr)
                  BALANCE_VARIABLES(MO,monthly_other_deferred_credits) = &
                        TNDB_wide(MO,YR,ASSET_CLASS,Other_Deferred_Credit,Cr)
                  BALANCE_VARIABLES(MO,monthly_deferred_gains) = &
                        TNDB_wide(MO,YR,ASSET_CLASS,Asset_Sales_Deferred_Gain,Cr)
               ENDDO
            ENDIF
         ENDIF
      RETURN

      ENTRY MNTHLY_DEF_DEBIT_BS_ELIM_INFO(R_YR,R_CLASS, &
                                          BALANCE_VARIABLES)

!
!
         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_CLASS == 0) THEN
               ASSET_CLASS = 0
            ELSEIF(R_CLASS == -1) THEN
               ASSET_CLASS = -1
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
               YR = R_YR ! - 1
               BALANCE_VARIABLES(:,monthly_net_goodwill) = &
                               BALANCE_VARIABLES(:,monthly_net_goodwill) &
                               - TNDB_wide(:,YR,ASSET_CLASS,Goodwill,Dr)
               BALANCE_VARIABLES(:,monthly_net_regulatory_assets) = &
                      BALANCE_VARIABLES(:,monthly_net_regulatory_assets) &
                      - TNDB_wide(:,YR,ASSET_CLASS,RegulatoryAssets,Dr) 
               BALANCE_VARIABLES(:,mty_net_fasb_109) = &
                               BALANCE_VARIABLES(:,mty_net_fasb_109) &
                               - TNDB_wide(:,YR,ASSET_CLASS,FASB109,Dr)
               BALANCE_VARIABLES(:,mty_net_fasb_133) = &
                               BALANCE_VARIABLES(:,mty_net_fasb_133) &
                               - TNDB_wide(:,YR,ASSET_CLASS,FASB133,Dr) 
               BALANCE_VARIABLES(:,monthly_net_dfrd_debits) = &
                       BALANCE_VARIABLES(:,monthly_net_dfrd_debits) &
                       - TNDB_wide(:,YR,ASSET_CLASS,Total_Deferred_Debits,Dr)

! DEFERRED CREDITS
               BALANCE_VARIABLES(:,monthly_gain_on_reaqd_debt_bal) = &
                     BALANCE_VARIABLES(:,monthly_gain_on_reaqd_debt_bal) &
                      - TNDB_wide(:,YR,ASSET_CLASS,Gain_on_Reaquired_Debt,Cr)
               BALANCE_VARIABLES(:,mty_regty_liablties_bal) = &
                       BALANCE_VARIABLES(:,mty_regty_liablties_bal) &
                       - TNDB_wide(:,YR,ASSET_CLASS,Regulatory_Liabilites,Cr)
               BALANCE_VARIABLES(:,monthly_other_deferred_credits) = &
                     BALANCE_VARIABLES(:,monthly_other_deferred_credits) &
                     - TNDB_wide(:,YR,ASSET_CLASS,Other_Deferred_Credit,Cr)
               BALANCE_VARIABLES(:,monthly_deferred_gains) = &
                     BALANCE_VARIABLES(:,monthly_deferred_gains) &
                  - TNDB_wide(:,YR,ASSET_CLASS,Asset_Sales_Deferred_Gain,Cr)
            ENDIF
         ENDIF
      RETURN

      ENTRY SET_UP_DD_ARRAYS

!
      CALL RETURN_NUM_OF_DEBIT_CLASSES(NUM_OF_ASSET_CLASSES, &
                                       MAX_ASSET_CLASS_NUM)
      IF(ALLOCATED(ASSET_CLASS_POINTER)) DEALLOCATE(ASSET_CLASS_POINTER)
      IF(MAX_ASSET_CLASS_NUM > 0) THEN
         ALLOCATE(ASSET_CLASS_POINTER(MAX_ASSET_CLASS_NUM))
         CALL RETURN_DEBIT_CLASS_POINTER(ASSET_CLASS_POINTER)
      ENDIF
      FINANCIAL_SIMULATION_YEARS =MAX(6,RUN_YEARS()+EXTENSION_YEARS()+1)
      IF(ALLOCATED(TDDB_wide)) &
                  DEALLOCATE(TDDB_wide,TDDRBB_wide,TAMRTE_wide,TNDB_wide,TCAMRT_wide,RB_DD_AMRTE_wide, &
                             INTRST_AMORT_wide,BTL_AMORTIZATION_wide, &
                             ATL_DEF_TAX_wide,BTL_DEF_TAX_wide, &
                             ATL_AMORTIZATION_wide, &
                             ATL_AMORT_SUB_ITEMS_wide, &
                             DEFERRED_CASH_ADDITIONS_wide, &
                             VARIABLE_OM_AMORT_wide, &
                             OTHER_OM_AMORT_wide, &
                             PURCHASE_POWER_AMORT_wide, &
                             ATL_CURRENT_TAX_EXPENSE_wide, &
                             BTL_CURRENT_TAX_EXPENSE_wide, &
                             TAX_TIMING_DIFFERENCES_wide, &
                             NET_CHANGE_IN_DB)
                             
      ALLOCATE(TDDB_wide(0:12,0:FINANCIAL_SIMULATION_YEARS, &
      	0:NUM_OF_ASSET_CLASSES,0:BAL_SHEET_OPTIONS,&
      	UNIQUE_ACCT_TYPE))
      allocate(TDDRBB_wide(0:12,0:FINANCIAL_SIMULATION_YEARS, &
               0:NUM_OF_ASSET_CLASSES, &
               UNIQUE_ACCT_TYPE), &
               TAMRTE_wide(0:12,0:FINANCIAL_SIMULATION_YEARS,&
               	0:NUM_OF_ASSET_CLASSES, &
               	UNIQUE_ACCT_TYPE), &
               TNDB_wide(0:12,0:FINANCIAL_SIMULATION_YEARS, &
               0:NUM_OF_ASSET_CLASSES,0:BAL_SHEET_OPTIONS, &
               UNIQUE_ACCT_TYPE), &
               NET_CHANGE_IN_DB(0:12,0:FINANCIAL_SIMULATION_YEARS, &
               0:NUM_OF_ASSET_CLASSES,0:BAL_SHEET_OPTIONS,UNIQUE_ACCT_TYPE), &
            TCAMRT_wide(0:12,0:FINANCIAL_SIMULATION_YEARS, &
            	0:NUM_OF_ASSET_CLASSES, &
               UNIQUE_ACCT_TYPE), &
               RB_DD_AMRTE_wide(0:12,0:FINANCIAL_SIMULATION_YEARS, &
               0:NUM_OF_ASSET_CLASSES,UNIQUE_ACCT_TYPE), &
               INTRST_AMORT_wide(0:12,0:FINANCIAL_SIMULATION_YEARS, &
                    0:NUM_OF_ASSET_CLASSES, &
                    UNIQUE_ACCT_TYPE), &
!
               BTL_AMORTIZATION_wide(0:12,0:FINANCIAL_SIMULATION_YEARS, &
               0:NUM_OF_ASSET_CLASSES, &
               UNIQUE_ACCT_TYPE), &
               ATL_AMORTIZATION_wide(0:12,0:FINANCIAL_SIMULATION_YEARS, &
               	0:NUM_OF_ASSET_CLASSES, &
               	UNIQUE_ACCT_TYPE), &
               ATL_AMORT_SUB_ITEMS_wide(0:12,0:FINANCIAL_SIMULATION_YEARS, &
               	0:NUM_OF_ASSET_CLASSES,0:BAL_SHEET_OPTIONS, &
               	UNIQUE_ACCT_TYPE), &
               ATL_DEF_TAX_wide(0:12,0:FINANCIAL_SIMULATION_YEARS, &
               0:NUM_OF_ASSET_CLASSES, &
               UNIQUE_ACCT_TYPE), &
               BTL_DEF_TAX_wide(0:12,0:FINANCIAL_SIMULATION_YEARS, &
               0:NUM_OF_ASSET_CLASSES,&
               UNIQUE_ACCT_TYPE), &
               DEFERRED_CASH_ADDITIONS_wide(0:12, &
               	0:FINANCIAL_SIMULATION_YEARS, &
               	0:NUM_OF_ASSET_CLASSES, &
               	UNIQUE_ACCT_TYPE), &
               ATL_CURRENT_TAX_EXPENSE_wide(0:12, &
               	0:FINANCIAL_SIMULATION_YEARS, &
               	0:NUM_OF_ASSET_CLASSES, &
               	UNIQUE_ACCT_TYPE), &
               BTL_CURRENT_TAX_EXPENSE_wide(0:12, &
               0:FINANCIAL_SIMULATION_YEARS, &
               	0:NUM_OF_ASSET_CLASSES, &
               	UNIQUE_ACCT_TYPE), &
               VARIABLE_OM_AMORT_wide(0:12,0:FINANCIAL_SIMULATION_YEARS, &
               0:NUM_OF_ASSET_CLASSES,UNIQUE_ACCT_TYPE), &
               OTHER_OM_AMORT_wide(0:12,0:FINANCIAL_SIMULATION_YEARS, &
               0:NUM_OF_ASSET_CLASSES,UNIQUE_ACCT_TYPE), &
               PURCHASE_POWER_AMORT_wide(0:12,0:FINANCIAL_SIMULATION_YEARS, &
               0:NUM_OF_ASSET_CLASSES,UNIQUE_ACCT_TYPE), &
               TAX_TIMING_DIFFERENCES_wide(0:FINANCIAL_SIMULATION_YEARS, &
               0:NUM_OF_ASSET_CLASSES,2,2,UNIQUE_ACCT_TYPE))
!
      TDDB_wide = 0.
      TNDB_wide = 0.
      NET_CHANGE_IN_DB = 0.
      ATL_AMORT_SUB_ITEMS_wide = 0.
      TDDRBB_wide = 0.
      TAMRTE_wide = 0.
      TCAMRT_wide = 0.
      RB_DD_AMRTE_wide = 0.
      INTRST_AMORT_wide = 0.
      BTL_AMORTIZATION_wide = 0.
      ATL_AMORTIZATION_wide = 0.
      ATL_DEF_TAX_wide = 0.
      BTL_DEF_TAX_wide = 0.
      DEFERRED_CASH_ADDITIONS_wide = 0.
      VARIABLE_OM_AMORT_wide = 0.
      OTHER_OM_AMORT_wide = 0.
      PURCHASE_POWER_AMORT_wide = 0.
      ATL_CURRENT_TAX_EXPENSE_wide = 0.
      BTL_CURRENT_TAX_EXPENSE_wide = 0.
      TAX_TIMING_DIFFERENCES_wide = 0.
      RETURN
      END
