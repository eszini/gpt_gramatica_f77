!     Last change: MSG 9/26/2011 4:37:40 PM
      SUBROUTINE DB_OBJECT
      use end_routine, only: end_program, er_message


      use spindriftlib
      use prod_arrays_dimensions
      use sizecom
      use msgmmdbt_decs

      INTEGER*2 NUMBER_OF_BC_DEBT_CLASSES/0/,
     +          MAX_BC_DEBT_CLASS_ID_NUM/0/
      INTEGER*2 NUMBER_OF_OL_DEBT_CLASSES/0/,
     +          MAX_OL_DEBT_CLASS_ID_NUM/0/
      INTEGER*2 R_NUM_OF_DEBT_CLASSES,R_MAX_DEBT_CLASS_NUM
      INTEGER*2 R_DEBT_CLASS_POINTERS(*)



      INTEGER*4 IOS,IOS_BASE
      INTEGER*2 UNIT_NO/0/,R_UNIT_NO
      CHARACTER*5 BASE_FILE_NAME,OVERLAY_FAMILY_NAME,DEBTFIL
      CHARACTER*256 FILE_NAME, fname
      CHARACTER*256 BASE_FILE_DIRECTORY,OUTPUT_DIRECTORY
      CHARACTER*256 DATA_DRIVE
      LOGICAL*4 FILE_EXISTS
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER*1024 RECLN


      CHARACTER*22 FILE_TYPE/'Debt/Investments/Notes'/

      INTEGER* 2 ASSET_CLASS_NUM,ASSET_ALLOCATION_VECTOR
      CHARACTER*2 DEBTOL/'BC'/,R_DEBTOL



      REAL*4 PREM_ISSUE_AMORT_PERIOD,
     +       PREM_DISC_AMORT_PERIOD   ! 41

      INTEGER*2 DEBT_BC_ASSET_CLASS_POINTER(:),
     +          DEBT_OL_ASSET_CLASS_POINTER(:),
     +          TEMP_ASSET_CLASS_POINTER(:)
      ALLOCATABLE :: DEBT_BC_ASSET_CLASS_POINTER,
     +               DEBT_OL_ASSET_CLASS_POINTER,
     +               TEMP_ASSET_CLASS_POINTER
      SAVE DEBT_BC_ASSET_CLASS_POINTER,
     +     DEBT_OL_ASSET_CLASS_POINTER



      INTEGER*2 PAYMENT_LAGS_ISSUE_MONTH,
     +          CAPITAL_RATES_VECTOR

      REAL ISSUE_PRICE_PER_SHARE,
     +     PURCHASE_PRICE_PER_SHARE
      LOGICAL*1 :: LAHEY_LF95
      CHARACTER*30 SCREEN_OUTPUT
      character(len=1) :: account_active



!

!
!          ROUTINE TO CONVERT METAFILE FILES TO DIRECT-ACESS BINARY
!          COPYRIGHT (C) 1983, 84, 85  M.S. GERBER & ASSOCIATES, INC.
!

!
! CONVERT THE DEBT-ACCOUNTS FILE

      ENTRY DB_MAKEBIN
      BASE_FILE_NAME = DEBTFIL()
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME = TRIM(BASE_FILE_DIRECTORY())//
     +  "DBB"//TRIM(BASE_FILE_NAME)//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = TRIM(FILE_TYPE)//'-'//BASE_FILE_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_LOCATE_WRITE(16,30,BASE_FILE_NAME,ALL_VERSIONS,0)
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         ENDIF
         ALLOCATE(TEMP_ASSET_CLASS_POINTER(1024))
         TEMP_ASSET_CLASS_POINTER = 0
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=TRIM(DATA_DRIVE)//"BCDEBT.BIN",ACCESS="DIRECT",
     +           STATUS="UNKNOWN",RECL=LRECL)
         IREC = 0
         ASSET_CLASS_NUM = 0
         ASSET_ALLOCATION_VECTOR = 0
         INTRA_CLASS_ID = 0
         READ(10,*) DELETE
         DO
            ACCRUED_INTEREST_PAYABLE = 0
            INTEREST_PAID_WHEN = 'A'
            PAYMENT_LAGS_ISSUE_MONTH= 0
            PREM_ISSUE_AMORT_PERIOD = 0.
            INVESTMENT_TYPE = 'N' !ot an Investment'
            SINKING_FUND_VECTOR = 0
            PREM_DISC_BALANCE = 0.
            ISSUE_EXPENSE_BALANCE = 0.
            ISSUE_PRICE_PER_SHARE = 0.
            PURCHASE_PRICE_PER_SHARE = 0.
            ACCOUNT_ACTIVE = 'A' !ctive
            EQUITY_MARKET = 'E'
            LHS_DISTRIBUTION = 'Not Active'
            CAPITAL_RATES_VECTOR = 0
            WVPA_PRIMARY_ACCOUNT = 0
            WVPA_DEPARTMENT_UNIT = 0 ! 38
            WVPA_REPORT_CATEGORY = 'Not Classified'
            WVPA_SUB_ACCOUNT_NUMBER = 0

            DO
               INTRA_COMPANY = 'N'
               PREM_ISSUE_AMORT_PERIOD = -99999.
               PREM_DISC_AMORT_PERIOD = -99999.
               READ(10,1000,IOSTAT=IOS) RECLN
               IF(IOS /= 0) EXIT
               IF(RECLN(1:1) == '7') EXIT
               RECLN = TRIM(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
              READ(RECLN,*,ERR=200)DELETE,TYPE,ISSMO,ISSDA,ISSYR,MATMO,
     +          MATDA,YRMAT,AMT,BYRBAL,ITYPE,IRATE,SFY1,SFA1,SFY2,SFA2,
     +           DESC,COMMENT,ASSET_CLASS_NUM,ASSET_ALLOCATION_VECTOR,
     +           INTRA_COMPANY,INTRA_CLASS_ID,
     +           SINKING_FUND_VECTOR,PREM_DISC_BALANCE,
     +           ISSUE_EXPENSE_BALANCE, ! 24
     +           COMMON_SHARES,
     +           ACCRUED_INTEREST_PAYABLE,
     +           INTEREST_PAID_WHEN,
     +           PAYMENT_LAGS_ISSUE_MONTH,
     +           PREM_ISSUE_AMORT_PERIOD, ! 29
     +           INVESTMENT_TYPE,
     +           ISSUE_PRICE_PER_SHARE,
     +           PURCHASE_PRICE_PER_SHARE,
     +           ACCOUNT_ACTIVE,
     +           EQUITY_MARKET,
     +           LHS_DISTRIBUTION,     ! 35
     +           WVPA_PRIMARY_ACCOUNT,
     +           CAPITAL_RATES_VECTOR,
     +           WVPA_DEPARTMENT_UNIT,  ! 38
     +           WVPA_REPORT_CATEGORY,   ! 39
     +           WVPA_SUB_ACCOUNT_NUMBER, ! 40
     +           PREM_DISC_AMORT_PERIOD   ! 41


               IF(PREM_ISSUE_AMORT_PERIOD == -99999.)
     +                                   PREM_ISSUE_AMORT_PERIOD = YRMAT


               IF(PREM_DISC_AMORT_PERIOD == -99999.)
     +                 PREM_DISC_AMORT_PERIOD = PREM_ISSUE_AMORT_PERIOD
               IF(.NOT. (DELETE >= 8 .OR. ACCOUNT_ACTIVE == 'N')) THEN
                CALL SET_ASSET_CLASSES(ASSET_CLASS_NUM,
     +                                   NUMBER_OF_BC_DEBT_CLASSES,
     +                                    MAX_BC_DEBT_CLASS_ID_NUM,
     +                                   TEMP_ASSET_CLASS_POINTER)
                  IF(INTRA_COMPANY == 'Y') THEN
                   CALL SET_ASSET_CLASSES(INTRA_CLASS_ID,
     +                                      NUMBER_OF_BC_DEBT_CLASSES,
     +                                       MAX_BC_DEBT_CLASS_ID_NUM,
     +                                      TEMP_ASSET_CLASS_POINTER)
                  ENDIF
               ENDIF

               IREC = IREC + 1
               WRITE(11,REC=IREC) DELETE,TYPE,ISSMO,ISSDA,ISSYR,MATMO,
     +                          MATDA,YRMAT,AMT,BYRBAL,ITYPE,
     +                          IRATE,SFY1,SFA1,SFY2,SFA2,
     +                         ASSET_CLASS_NUM,ASSET_ALLOCATION_VECTOR,
     +                          INTRA_COMPANY,INTRA_CLASS_ID,
     +                          SINKING_FUND_VECTOR,PREM_DISC_BALANCE,
     +                          ISSUE_EXPENSE_BALANCE,
     +                          PREM_ISSUE_AMORT_PERIOD,
     +                          COMMON_SHARES,
     +                          ACCRUED_INTEREST_PAYABLE,
     +                          INTEREST_PAID_WHEN,
     +                          PAYMENT_LAGS_ISSUE_MONTH,
     +                          INVESTMENT_TYPE,
     +                          ISSUE_PRICE_PER_SHARE,
     +                          PURCHASE_PRICE_PER_SHARE,
     +                          ACCOUNT_ACTIVE,
     +                          EQUITY_MARKET,
     +                          LHS_DISTRIBUTION,
     +                          CAPITAL_RATES_VECTOR,
     +                          DESC,
     +                          WVPA_PRIMARY_ACCOUNT,
     +                          WVPA_DEPARTMENT_UNIT,  ! 38
     +                          WVPA_REPORT_CATEGORY,
     +                          WVPA_SUB_ACCOUNT_NUMBER, ! 40
     +                          PREM_DISC_AMORT_PERIOD   ! 41
            ENDDO
            IF(IOS /= 0) EXIT
         ENDDO
         CLOSE(10)

         CLOSE(11)
         IF(MAX_BC_DEBT_CLASS_ID_NUM > 0) THEN
            ALLOCATE(DEBT_BC_ASSET_CLASS_POINTER
     +                                      (MAX_BC_DEBT_CLASS_ID_NUM))
            DEBT_BC_ASSET_CLASS_POINTER =
     +             TEMP_ASSET_CLASS_POINTER(1:MAX_BC_DEBT_CLASS_ID_NUM)
         ENDIF
         DEALLOCATE(TEMP_ASSET_CLASS_POINTER)
      ELSE IF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "msgmmdbt:1111")
      ENDIF
      RETURN



!
!          ROUTINE TO CREATE OVERLAY FILES
!          COPYRIGHT (C) 1984-88  M.S. GERBER & ASSOCIATES, INC.
!          COPYRIGHT (C) 1991-92  M.S. GERBER & ASSOCIATES, INC.
!

!
! OVERLAY THE DEBT-ACCOUNTS FILE


      ENTRY DB_MAKEOVL(OVERLAY_FAMILY_NAME)
      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = TRIM(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME=TRIM(DATA_DRIVE)//"DBO"//
     +                               TRIM(OVERLAY_FAMILY_NAME)//".DAT"
      OPEN(10,FILE=FILE_NAME)
      READ(10,*) DELETE
      INUNIT = 12
      IF(DEBTOL == 'BC') THEN
         OPEN(11,FILE=TRIM(DATA_DRIVE)//"BCDEBT.BIN",
     +                                      ACCESS="DIRECT",RECL=LRECL)
         INUNIT = 11
      ENDIF
      OPEN(12,FILE=TRIM(DATA_DRIVE)//"OLDEBT.BIN",ACCESS="DIRECT",
     +                                     STATUS="UNKNOWN",RECL=LRECL)
      ALLOCATE(TEMP_ASSET_CLASS_POINTER(1024))
      TEMP_ASSET_CLASS_POINTER = 0
      IREC = 0
      NUMBER_OF_OL_DEBT_CLASSES = 0
      MAX_OL_DEBT_CLASS_ID_NUM = 0
      DO
         DO
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(RECLN(1:1) == '7') EXIT
            IREC = IREC + 1
            READ(INUNIT,REC=IREC,IOSTAT=IOS_BASE) DELETE,TYPE,
     +           ISSMO,ISSDA,ISSYR,MATMO,MATDA,YRMAT,AMT,BYRBAL,ITYPE,
     +           IRATE,SFY1,SFA1,SFY2,SFA2,
     +           ASSET_CLASS_NUM,ASSET_ALLOCATION_VECTOR,
     +           INTRA_COMPANY,INTRA_CLASS_ID,
     +           SINKING_FUND_VECTOR,PREM_DISC_BALANCE,
     +           ISSUE_EXPENSE_BALANCE,
     +           PREM_ISSUE_AMORT_PERIOD,
     +           COMMON_SHARES,
     +           ACCRUED_INTEREST_PAYABLE,
     +           INTEREST_PAID_WHEN,
     +           PAYMENT_LAGS_ISSUE_MONTH,
     +           INVESTMENT_TYPE,
     +           ISSUE_PRICE_PER_SHARE,
     +           PURCHASE_PRICE_PER_SHARE,
     +           ACCOUNT_ACTIVE,
     +           EQUITY_MARKET,
     +           LHS_DISTRIBUTION,
     +           CAPITAL_RATES_VECTOR,
     +           DESC,
     +           WVPA_PRIMARY_ACCOUNT,
     +           WVPA_DEPARTMENT_UNIT,  ! 38
     +           WVPA_REPORT_CATEGORY,
     +           WVPA_SUB_ACCOUNT_NUMBER, ! 40
     +           PREM_DISC_AMORT_PERIOD   ! 41

            IF(IOS_BASE /= 0) EXIT
            IF(IOS == 0) THEN
               RECLN = TRIM(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,ERR=200) DELETE,TYPE,ISSMO,ISSDA,ISSYR,
     +            MATMO,MATDA,YRMAT,AMT,BYRBAL,ITYPE,IRATE,
     +            SFY1,SFA1,SFY2,SFA2,DESC,COMMENT,ASSET_CLASS_NUM,
     +            ASSET_ALLOCATION_VECTOR,
     +            INTRA_COMPANY,INTRA_CLASS_ID,
     +            SINKING_FUND_VECTOR,PREM_DISC_BALANCE,
     +            ISSUE_EXPENSE_BALANCE,
     +            COMMON_SHARES,
     +            ACCRUED_INTEREST_PAYABLE,
     +            INTEREST_PAID_WHEN,
     +            PAYMENT_LAGS_ISSUE_MONTH,
     +            PREM_ISSUE_AMORT_PERIOD,
     +            INVESTMENT_TYPE,
     +            ISSUE_PRICE_PER_SHARE,
     +            PURCHASE_PRICE_PER_SHARE,
     +            ACCOUNT_ACTIVE,
     +            EQUITY_MARKET,
     +            LHS_DISTRIBUTION,
     +            WVPA_PRIMARY_ACCOUNT,
     +            CAPITAL_RATES_VECTOR,
     +            WVPA_DEPARTMENT_UNIT,  ! 38
     +            WVPA_REPORT_CATEGORY,
     +            WVPA_SUB_ACCOUNT_NUMBER, ! 40
     +            PREM_DISC_AMORT_PERIOD   ! 41

            ENDIF
            IF(.NOT. (DELETE >= 8 .OR. ACCOUNT_ACTIVE == 'N')) THEN
                CALL SET_ASSET_CLASSES(ASSET_CLASS_NUM,
     +                                NUMBER_OF_OL_DEBT_CLASSES,
     +                                 MAX_OL_DEBT_CLASS_ID_NUM,
     +                                TEMP_ASSET_CLASS_POINTER)
               IF(INTRA_COMPANY == 'Y') THEN
                CALL SET_ASSET_CLASSES(INTRA_CLASS_ID,
     +                                NUMBER_OF_OL_DEBT_CLASSES,
     +                                 MAX_OL_DEBT_CLASS_ID_NUM,
     +                                TEMP_ASSET_CLASS_POINTER)
               ENDIF
            ENDIF

            WRITE(12,REC=IREC) DELETE,TYPE,ISSMO,ISSDA,ISSYR,MATMO,
     +         MATDA,YRMAT,AMT,BYRBAL,ITYPE,IRATE,SFY1,SFA1,SFY2,SFA2,
     +         ASSET_CLASS_NUM,ASSET_ALLOCATION_VECTOR,
     +         INTRA_COMPANY,INTRA_CLASS_ID,
     +         SINKING_FUND_VECTOR,PREM_DISC_BALANCE,
     +         ISSUE_EXPENSE_BALANCE,
     +         PREM_ISSUE_AMORT_PERIOD,
     +         COMMON_SHARES,
     +         ACCRUED_INTEREST_PAYABLE,
     +         INTEREST_PAID_WHEN,
     +         PAYMENT_LAGS_ISSUE_MONTH,
     +         INVESTMENT_TYPE,
     +         ISSUE_PRICE_PER_SHARE,
     +         PURCHASE_PRICE_PER_SHARE,
     +         ACCOUNT_ACTIVE,
     +         EQUITY_MARKET,
     +         LHS_DISTRIBUTION,
     +         CAPITAL_RATES_VECTOR,
     +         DESC,
     +         WVPA_PRIMARY_ACCOUNT,
     +         WVPA_DEPARTMENT_UNIT,  ! 38
     +         WVPA_REPORT_CATEGORY,
     +         WVPA_SUB_ACCOUNT_NUMBER, ! 40
     +         PREM_DISC_AMORT_PERIOD   ! 41

         ENDDO
         IF(IOS_BASE /= 0) EXIT
      ENDDO
      CLOSE(10)
      CLOSE(12)
      IF(DEBTOL == 'BC') CLOSE(11)
      IF(ALLOCATED(DEBT_OL_ASSET_CLASS_POINTER))
     +                          DEALLOCATE(DEBT_OL_ASSET_CLASS_POINTER)
      IF(MAX_OL_DEBT_CLASS_ID_NUM > 0) THEN
        ALLOCATE(DEBT_OL_ASSET_CLASS_POINTER(MAX_OL_DEBT_CLASS_ID_NUM))
         DEBT_OL_ASSET_CLASS_POINTER =
     +             TEMP_ASSET_CLASS_POINTER(1:MAX_OL_DEBT_CLASS_ID_NUM)
      ENDIF
      DEALLOCATE(TEMP_ASSET_CLASS_POINTER)
      DEBTOL = 'OL'

      RETURN

  200 CALL MG_LOCATE_WRITE(20,0,TRIM(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from msgmmdbt SIID202'
      call end_program(er_message)
!

      ENTRY RESET_DEBTOL

         DEBTOL = 'BC'
      RETURN
!

      ENTRY GET_DEBT_OL(R_DEBTOL)

         R_DEBTOL = DEBTOL
      RETURN
!

      ENTRY OPEN_DB_OUT_FILE(R_UNIT_NO)



         OPEN(R_UNIT_NO,FILE=TRIM(OUTPUT_DIRECTORY())//DEBTOL//
     +                            'DB_AST.BIN',ACCESS='DIRECT',
     +                           RECL=4*MAX_FINANCIAL_SIMULATION_YEARS)
      RETURN

      ENTRY OPEN_DEBT_FILE(R_UNIT_NO)



         OPEN(R_UNIT_NO,FILE=TRIM(OUTPUT_DIRECTORY())//DEBTOL//
     +          "DEBT.BIN",ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         UNIT_NO = R_UNIT_NO
      RETURN


      ENTRY OPEN_DB_BASE_CASE_FILE(R_UNIT_NO)


         OPEN(R_UNIT_NO,FILE=TRIM(OUTPUT_DIRECTORY())//
     +                                "BC_DEBT.BIN",FORM='UNFORMATTED')
      RETURN


      ENTRY CLOSE_DEBT_FILE

         CLOSE(UNIT_NO)
      RETURN

      ENTRY RETURN_NUM_OF_DEBT_CLASSES(R_NUM_OF_DEBT_CLASSES,
     +                                       R_MAX_DEBT_CLASS_NUM)


         IF(DEBTOL == 'OL') THEN
            R_NUM_OF_DEBT_CLASSES = NUMBER_OF_OL_DEBT_CLASSES
            R_MAX_DEBT_CLASS_NUM = MAX_OL_DEBT_CLASS_ID_NUM
         ELSE
            R_NUM_OF_DEBT_CLASSES = NUMBER_OF_BC_DEBT_CLASSES
            R_MAX_DEBT_CLASS_NUM = MAX_BC_DEBT_CLASS_ID_NUM
         ENDIF
      RETURN


      ENTRY RETURN_DEBT_CLASS_POINTER(R_DEBT_CLASS_POINTERS)

         IF(DEBTOL == 'OL') THEN
            R_DEBT_CLASS_POINTERS(1:MAX_OL_DEBT_CLASS_ID_NUM) =
     +          DEBT_OL_ASSET_CLASS_POINTER(1:MAX_OL_DEBT_CLASS_ID_NUM)
         ELSE
            R_DEBT_CLASS_POINTERS(1:MAX_BC_DEBT_CLASS_ID_NUM) =
     +          DEBT_BC_ASSET_CLASS_POINTER(1:MAX_BC_DEBT_CLASS_ID_NUM)
         ENDIF
      RETURN

 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END

!*                                                                     *
!*                             D E B T                                 *
!*                                                                     *
!*          COPYRIGHT (C) 1982-93 M.S. GERBER & ASSOCIATES, INC.       *
!*                                                                     *

!                                                                      *
!     PURPOSE:                                                         *
!        DEBT CALCULATES THE INTEREST, SINKING FUND PAYMENTS           *
!        AND RETIREMENTS OF LTD AND PERFERRED STOCK                    *
!                                                                      *

!
      RECURSIVE SUBROUTINE DEBT(IARRAY,save_base_cs)
!
      use drilling_rept_parameters
      use irec_endpoint_control
      use grx_planning_routines
      use sizecom
      use globecom
      use spindriftlib
      use prod_arrays_dimensions
      use mthnmcom
      use namescom
      use globecom
      use endpoint
      use namescom
      use mod_base_year
      use msgmmdbt_decs
      implicit none
      SAVE
      logical (kind=1) :: save_base_cs
      character (len=34) :: INDEX_NAME
      logical (kind=1) :: save_base_case_loc
      REAL(kind=4) :: TIME_FROM_FIRST_DAY ! ENTRY function


      INTEGER*2 PAYMENT_LAGS_ISSUE_MONTH,CURRENT_YR


      logical(kind=1) :: FirstEnergy
      INTEGER*2 J,YR,SFYA,SFYB,LAST_YR,NORMALIZE_YR,END_MO
      REAL*4 FRACTION,FRACTN,R_ANN_PS_DIV_LTD_INT,
     +     R_ANN_PS_DIV,R_ANN_LTD_INT,FRACTION_OF_MONTH,
     +     DAYS_IN_MONTH
      INTEGER*2 DAYS_IN_YEAR,REMAINING_DAYS_IN_MONTH
      REAL*4 R_PS_DIV,R_PS_RETIRE,R_LTD_RETIRE,R_LTD_INT
      INTEGER*2 RUN_YEAR,R_CLASS
      LOGICAL*1 R_CLASS_EXISTS
      REAL*4 R_PSISS,R_PSSINK,R_LTDISS,R_LTSINK
      character(len=1) :: DUMMY_TYPe

      REAL*4 IARRAY(MAX_FINANCIAL_SIMULATION_YEARS,*)

      REAL*4 ISSUE_EXP_AMORT_PERIOD,
     +       PREM_DISC_AMORT_PERIOD   ! 41
      LOGICAL*1 VOID_LOGICAL,ASSET_CLASS_LINKAGES,
     +          RETURN_ASSET_CLASS_LISTS

      INTEGER*2 I,NOACCT
      REAL*4 R_OTHER_INCOME,
     +       R_INVESTMENT_IN_AFILLIATES,
     +       R_INTEREST_INCOME,
     +       R_LOANS_TO_AFILLIATES,
     +       R_PS_PREM_BAL,
     +       R_PS_PREM_AMORT,
     +       R_LTD_PREM_BAL,
     +       R_LTD_PREM_AMORT,
     +       R_PS_ISSUE_EXP_BAL,
     +       R_PS_ISSUE_EXP_AMORT,
     +       R_LTD_ISSUE_EXP_BAL,
     +       R_LTD_ISSUE_EXP_AMORT,
     +       R_LTD_PS_ISSUE_TAX_EXPENSE,
     +       R_MIPS_INCOME_TAX_DEDUCTION,
     +       R_RETAINED_EARNINGS_ADJ,
     +       R_NOTES_PAYABLE_CASH_INTEREST,
     +       R_NOTES_RECEIVALBE_CASH_INTEREST
      REAL ISSUE_PRICE_PER_SHARE,
     +     PURCHASE_PRICE_PER_SHARE,
     +     R_ISSUE_PRICE_PER_SHARE,
     +     R_PURCHASE_PRICE_PER_SHARE,
     +     SINKING_FUND_PAYMENT


      INTEGER*2 WRITE_DRILLING_RPT,VOID_INT2,ACCOUNTS_REPORTED,
     +          CAPITAL_RATES_VECTOR
       CHARACTER*1 DRILLING_REPRT_LEVEL,FINANCIAL_DRILLING,
     +             ACCOUNT_ACTIVE

      CHARACTER(len=30) :: DRILLING_NAME
      CHARACTER*34 OUTPUT_OPTION_NAME,LEFT_JUSTIFY_I2_IN_STR*15
      LOGICAL*1 DEBT_ASSET_REPORT,REPORT_ALL_ACCOUNTS,DEBT_REPORT

      INTEGER*2 REPORTING_UNIT,DEBT_FILE_RPT_HEADER,
     +          WVPA_DEBT_FILE_RPT_HEADER,
     +          MTG_REPORTING_UNIT,
     +          WVPA_MORTGAGE_SUMMARY_RPT_HEADR
      INTEGER REPORTING_REC,MTG_REPORTING_REC
      REAL*4 LTD_INTEREST,
     +       PS_DIVIDENDS,
     +       MIPS_DIVIDENDS,
     +       CLOSING_BALANCE,
     +       OPENING_BALANCE,
     +       AMOUNT_ISSUED,
     +       P_INVESTMENT_INCOME,
     +       P_DIVIDEND_INCOME,
     +       P_TAX_FREE_70_DIVIDEND_INCOME,
     +       LTD_INTEREST_CASH,
     +       PS_DIVIDENDS_CASH,
     +       MIPS_DIVIDENDS_CASH,
     +       PS_PREMIUM_AMORT,
     +       PS_ISSUE_EXPENSE_AMORT,
     +       MIPS_PREMIUM_AMORT,
     +       MIPS_ISSUE_EXPENSE_AMORT,
     +       LTD_PREMIUM_AMORT,
     +       LTD_ISSUE_EXPENSE_AMORT
      REAL*4 MONTHLY_DAILY_RATE
      REAL*4 TAX_VARIABLES(0:12,TAX_VARS)
      REAL*4 R_LTD_INTEREST_CASH_PAYMENTS,
     +       R_PS_DIVIDEND_CASH_PAYMENTS,
     +       R_CASH_INVESTMENT_EARNINGS,
     +       R_CURRENT_LTD_RETIRE
      REAL*4 R_INVESTMENTS_MADE,
     +       R_INVESTMENTS_SOLD,
     +       R_INVESTMENT_INCOME,
     +       R_DIVIDEND_70_INCOME,
     +       R_NOTES_RECEIVABLE_MADE,
     +       R_NOTES_RECEIVABLE_CLEARED,
     +       R_NOTES_PAYABLE_MADE,
     +       R_NOTES_PAYABLE_CLEARED,
     +       R_NOTES_PAYABLE_INTEREST,
     +       R_INTRA_INVESTMENT_INCOME
      REAL*4 R_COMMON_STOCK_ISSUED_AMOUNT,
     +       R_COMMON_STOCK_ISSUED_SHARES,
     +       R_COMMON_STOCK_BUYBACK_AMOUNT,
     +       R_COMMON_STOCK_BUYBACK_SHARES,
     +       R_COMMON_STOCK_BALANCE,
     +       R_COMMON_SHARES_OUTSTANDING,
     +       R_COMMON_STOCK_ISSUED_2_EQUITY,
     +       R_COMMON_STOCK_BUY_FROM_EQUITY,
     +       R_CS_ISSUE_EXP_BAL,
     +       R_CS_ISSUE_EXP_AMORT,
     +       R_CS_ISSUE_EXP_THIS_PERIOD,
     +       R_NET_MONTHLY_SHARES(12)

      REAL*4 INCOME_VARIABLES(0:12,INCOME_VARS)
      REAL*4 CASH_VARIABLES(0:12,CASH_VARS)

      REAL*4 BALANCE_SHEET_VARIABLES(0:12,BAL_SHEET_VARS),
     +       R_CURRENT_PORTION_OF_LTD(0:12)

      REAL*4 R_LTD_BAL,LTD_BAL,R_LTD_RATE,MONTHLY_INTEREST


      INTEGER*2 R_ISSMO,START_MO,LAG_SINKING_FUND_YEARS
      REAL*4 R_LAG_SINKING_FUND_YEARS
      REAL*4 MONTHLY_LTD_INTEREST,
     +       R_ANNINT(*),
     +       ANN_LTD_INT(*)
      CHARACTER*1 PAID_WHEN
      REAL*4 R_PS_BAL,PS_BAL,R_PS_RATE,MONTHLY_DIVIDEND

      REAL*4 R_INVESTMENT_BALANCE,
     +       R_NOTES_RECEIVABLE,
     +       R_NOTES_PAYABLE
      REAL*4 R_STD_INTEREST,MGT_ACCURRED_INTEREST,R_STD_INTEREST_CASH
      REAL*4 PAYMENT_IN_CASH,INC_STATEMTENT_PAYMENT


      REAL*4 R_COMMON_SHARES,R_AMT,R_BYRBAL,

     +       R_RET(0:12,0:*),R_BAL(0:12,0:*),R_ISS(0:12,0:*),
     +       R_SHARES_ISSUED(0:12,0:*),
     +       R_SHARES_PURCHASED(0:12,0:*),
     +       R_SHARES_OUTSTANDING(0:12,0:*),
     +       R_ADJ_RETAINED_EARNINGS(0:12,0:*)

      INTEGER*2 ISSUE_MO,ISSUE_YR,MAT_MO,YR_MAT,
     +          R_ASSET_CLASS,R_ASSET_CLASS_VECTOR,
     +          R_INTRA_CLASS_ID

      CHARACTER*1 R_INTRA_COMPANY
      CHARACTER*30 DESCRIPTION
      REAL*4 ISSUE_AMOUNT,REPURCHASE_AMOUNT,PRINCIPAL_PAYMENT
      REAL*4 R_LTDRET(0:*),R_LTDINT(0:*)
      REAL*4 R_NOTES_PAYABLE_MONTHLY_MADE(12),
     +       R_NOTES_RECEIVABLE_MONTHLY_MADE(12),
     +       R_UNAMORTIZED_INTEREST_BALANCE(0:12)
      LOGICAL*1 INTEREST_INPUT_ACTIVE
      LOGICAL*1 SHARES_WHERE_ISSUED,STOCK_WAS_ISSUED,
     +          SHARES_WHERE_PURCHASES,STOCK_WAS_PURCHASED


      INTEGER*2 PAY_MONTH,R_YR,PARENT_CLASS_ID_NUM
      REAL*4 CARRY_OVER,MONTHLY_INTEREST_PAID(0:12,0:*)
      LOGICAL*1 LF95,LAHEY_LF95


      REAL*4 MTG_DEBT_RETIRE,PIBIEN,PAY_PERIODS_PER_YR,R_LTD_LIFE

      LOGICAL THIS_IS_A_PAY_MONTH
      LOGICAL*1 WVPA
      CHARACTER*6 SHORT_MONTH_NAMES
      INTEGER*2 SCENARIO_INDEX,GET_SCENARIO_INDEX
      REAL*4 GET_SCENARIO_BY_INDEX

      integer (kind=2) :: run_years, extension_years, val1, val2
      integer :: ios
      save_base_case_loc=save_base_cs


      LF95 = LAHEY_LF95()
      CALL PARENT_CLASS_ID(PARENT_CLASS_ID_NUM)
      ACCOUNTS_REPORTED = 0
      NOACCT = 0
      IREC = 0
      PS_BASEYR = 0.
      LTD_BASEYR = 0.
      INVESTMENT_BY_BALANCE = 0.
      COMMON_BASEYR = 0.

      FINANCIAL_SIMULATION_YEARS=MAX(int(5,2),int(RUN_YEARS()+
     + EXTENSION_YEARS()+1),2)
      LAST_YR = get_BASE_YEAR() + FINANCIAL_SIMULATION_YEARS - 1
      DEBT_ASSET_REPORT = DEBT_REPORT(REPORT_ALL_ACCOUNTS)
     +                    .AND. FINANCIAL_DRILLING() /= 'O'
     +                    .AND. .NOT. save_base_case_loc
      REPORT_ALL_ACCOUNTS = DEBT_ASSET_REPORT .AND. REPORT_ALL_ACCOUNTS
      DRILLING_REPRT_LEVEL = FINANCIAL_DRILLING()

      CALL SET_UP_DEBT_ARRAYS

! REPORTING HANDLES


      ALLOCATE(BAL(0:12,0:FINANCIAL_SIMULATION_YEARS),
     +         RET(0:12,0:FINANCIAL_SIMULATION_YEARS),
     +         INTEREST(0:12,0:FINANCIAL_SIMULATION_YEARS),
     +         INTEREST_IN_SPLIT_MONTH(0:12,
     +                                   0:FINANCIAL_SIMULATION_YEARS),
     +         ISS(0:12,0:FINANCIAL_SIMULATION_YEARS),
     +         SINKING_FUND(0:12,0:FINANCIAL_SIMULATION_YEARS),
     +         LTD_CURRENT_PORTION(0:12,0:FINANCIAL_SIMULATION_YEARS),
     +         CASH_PAYMENTS(0:12,0:FINANCIAL_SIMULATION_YEARS),
     +         ANNINT(0:FINANCIAL_SIMULATION_YEARS),
     +         RATE(0:12,0:FINANCIAL_SIMULATION_YEARS),
     +         INTEREST_EXPENSE_ADJ(0:12,0:FINANCIAL_SIMULATION_YEARS),
     +         ISSUE_TAX_EXPENSE(0:12,0:FINANCIAL_SIMULATION_YEARS),
     +         ISSUE_EXPENSE_AMORT(0:12,0:FINANCIAL_SIMULATION_YEARS),
     +         ISSUE_EXPENSE_BAL(0:12,0:FINANCIAL_SIMULATION_YEARS),
     +         SHARES_ISSUED(0:12,0:FINANCIAL_SIMULATION_YEARS),
     +         SHARES_PURCHASED(0:12,0:FINANCIAL_SIMULATION_YEARS),
     +         SHARES_OUTSTANDING(0:12,0:FINANCIAL_SIMULATION_YEARS),
     +        ADJ_RETAINED_EARNINGS(0:12,0:FINANCIAL_SIMULATION_YEARS),
     +         ASSET_CLASS_LIST(1024),
     +         ASSET_ALLOCATION_LIST(1024),
     +         PREMIUM_BAL(0:12,0:FINANCIAL_SIMULATION_YEARS),
     +         PREMIUM_AMORT(0:12,0:FINANCIAL_SIMULATION_YEARS),
     +         PREMIUM_DISCOUNT_EXP(0:12,0:FINANCIAL_SIMULATION_YEARS))

      IF(LF95) THEN
         WRITE(SCREEN_MESSAGES,"(A)") "Debt/Investment Accounts "
         CALL MG_LOCATE_WRITE(18,70,TRIM(SCREEN_MESSAGES),3,2)
      ENDIF
      CALL OPEN_DEBT_FILE(INT2(11))
      DO
         IREC = IREC + 1
         READ(11,REC=IREC,IOSTAT=IOS) DELETE,TYPE,ISSMO,ISSDA,ISSYR,
     +                            MATMO,MATDA,YRMAT,AMT,BYRBAL,ITYPE,
     +                            IRATE,SFY1,SFA1,SFY2,SFA2,
     +                            ASSET_CLASS,ASSET_CLASS_VECTOR,
     +                            INTRA_COMPANY,INTRA_CLASS_ID,
     +                            SINKING_FUND_VECTOR,
     +                            PREM_DISC_BALANCE,
     +                            ISSUE_EXPENSE_BALANCE,
     +                            ISSUE_EXP_AMORT_PERIOD,
     +                            COMMON_SHARES,
     +                            ACCRUED_INTEREST_PAYABLE,
     +                            INTEREST_PAID_WHEN,
     +                            PAYMENT_LAGS_ISSUE_MONTH,
     +                            INVESTMENT_TYPE,
     +                            ISSUE_PRICE_PER_SHARE,
     +                            PURCHASE_PRICE_PER_SHARE,
     +                            ACCOUNT_ACTIVE,
     +                            EQUITY_MARKET,
     +                            LHS_DISTRIBUTION,
     +                            CAPITAL_RATES_VECTOR,
     +                            DESC,
     +                            WVPA_PRIMARY_ACCOUNT,
     +                            WVPA_DEPARTMENT_UNIT,  ! 38
     +                            WVPA_REPORT_CATEGORY,
     +                            WVPA_SUB_ACCOUNT_NUMBER, ! 40
     +                            PREM_DISC_AMORT_PERIOD   ! 41

         IF(IOS /= 0) EXIT
         IF(DELETE > 7 .OR. ISSYR > LAST_YR .OR.
     +                          ACCOUNT_ACTIVE == 'N' .OR.

     +                                     INDEX(TYPE,'Not') /= 0) then
              CYCLE
          end if

         NOACCT = NOACCT + 1
         IF(LF95) THEN
            WRITE(SCREEN_MESSAGES,"(I4,A)") NOACCT,"-"//DESC
            ! WRITE TO RW STATUS
            CALL MG_LOCATE_WRITE(14,70,TRIM(SCREEN_MESSAGES),3,0)
         ELSE
            WRITE(SCREEN_MESSAGES,"(I4)") NOACCT
            CALL MG_LOCATE_WRITE(14,70,TRIM(SCREEN_MESSAGES),
     +                                                  ALL_VERSIONS,0)
         ENDIF

         IF(ISSYR > get_BASE_YeaR()) BYRBAL = 0.

         IF(TYPE == INVESTMENT) THEN
            INVESTMENT_BY_BALANCE = INVESTMENT_BY_BALANCE + BYRBAL
         ELSEIF(TYPE == PFS .OR. TYPE == MIPS) THEN
            PS_BASEYR = PS_BASEYR + BYRBAL
         ELSEIF(TYPE == LTD) THEN
            LTD_BASEYR = LTD_BASEYR + BYRBAL
         ENDIF
         IF(TYPE == MIPS) INTEREST_PAID_WHEN = 'M'

! ZERO MONTHLY ARRAYS

         RET = 0.
         INTEREST = 0.
         INTEREST_IN_SPLIT_MONTH = 0.
         BAL = 0.
         ISS = 0.
         SHARES_ISSUED = 0.
         SHARES_PURCHASED = 0.
         SHARES_OUTSTANDING = 0.
         ADJ_RETAINED_EARNINGS = 0.
         SINKING_FUND = 0.
         CASH_PAYMENTS = 0.
         ISSUE_TAX_EXPENSE = 0.
         INTEREST_EXPENSE_ADJ = 0.
         RATE = 0.
         PREMIUM_BAL = 0.
         PREMIUM_AMORT = 0.
         PREMIUM_DISCOUNT_EXP = 0.
         LTD_CURRENT_PORTION = 0.

! ZERO ANNUAL ARRAYS
         ANNINT = 0.

! ISSUE EXPENSE AMORTIZATION

         ISSUE_EXPENSE_BAL = 0.
         ISSUE_EXPENSE_AMORT = 0.
         IF(ISSUE_EXPENSE_BALANCE /= 0. .AND.
     +                                ISSUE_EXP_AMORT_PERIOD < 0.) THEN
           CALL GET_MONTHLY_ANNUAL_VALUES(INT2(ISSUE_EXP_AMORT_PERIOD),
     +                                     DATA_TYPE,
     +                                     VECTOR_TYPE,
     +                                     ANNUAL_VECTOR_VALUES,
     +                                     VECTOR_MONTHLY_DATA(1,1),
     +                                     MONTHLY_DATA_UNITS,
     +                                     MONTH_ENDING)
            STOP_AT_MATURITY = .FALSE.
            IF(ISSYR <= get_BASE_YEAR()) ISSUE_EXPENSE_BAL(12,0) =
     +                                          ISSUE_EXPENSE_BALANCE
            IF(ISSYR > get_BASE_YEAR()) THEN
               I = ISSYR - get_BASE_YEAR()
               ISSUE_TAX_EXPENSE(ISSMO,I) = ISSUE_EXPENSE_BALANCE
               ISSUE_TAX_EXPENSE(0,I) = ISSUE_EXPENSE_BALANCE
            ENDIF
            IF(MONTHLY_MIDAS_ACTIVE) THEN

               CALL RIPPLE_MONTHLY_DATA(ANNUAL_VECTOR_VALUES,
     +                                  VECTOR_MONTHLY_DATA)

               CALL MONTHLY_BOOK_VALUES_IN_DOLLARS(
     +                                          ANNUAL_VECTOR_VALUES,
     +                                          VECTOR_MONTHLY_DATA,
     +                                          MONTHLY_DATA_UNITS,
     +                                          MONTH_ENDING)

               AMORTIZATION_START_YR = MAX(1,ISSYR - get_BASE_YEAR())
               AMORT_YR = 0
               DO I=AMORTIZATION_START_YR, FINANCIAL_SIMULATION_YEARS
                  YR = get_BASE_YEAR() + I
                  IF(YR < ISSYR) CYCLE
                  ISSUE_EXPENSE_BAL(0,I) = ISSUE_EXPENSE_BAL(12,I-1)
                  AMORT_YR = MIN(AMORT_YR + 1,AVAIL_DATA_YEARS)

                  DO MO = 1, 12
                     IF(YR == ISSYR .AND. MO < ISSMO) CYCLE
                     IF(YR == ISSYR .AND. MO == ISSMO) THEN
                        ISSUE_EXPENSE_BAL(MO,I)=ISSUE_EXPENSE_BALANCE
                     ELSE
                        IF(I < AMORTIZATION_START_YR
     +                           + LAST_AVAILABLE_MONTHLY_YEAR) THEN
                           MONTH_AMORT = VECTOR_MONTHLY_DATA(MO,
     +                                       I+1-AMORTIZATION_START_YR)
                        ELSE
                          MONTH_AMORT = ANNUAL_VECTOR_VALUES(AMORT_YR)/
     +                                                              12.
                        ENDIF
                        IF(ABS(MONTH_AMORT) <
     +                           ABS(ISSUE_EXPENSE_BAL(MO-1,I))) THEN
                           ISSUE_EXPENSE_BAL(MO,I) =
     +                                      ISSUE_EXPENSE_BAL(MO-1,I)
     +                                      - MONTH_AMORT
                           ISSUE_EXPENSE_AMORT(MO,I) = MONTH_AMORT
                        ELSE
                           ISSUE_EXPENSE_AMORT(MO,I) =
     +                                      ISSUE_EXPENSE_BAL(MO-1,I)
                           ISSUE_EXPENSE_BAL(MO,I) = 0.
                           EXIT
                        ENDIF
                     ENDIF
                  ENDDO
                  ISSUE_EXPENSE_AMORT(0,I) =
     +                               SUM(ISSUE_EXPENSE_AMORT(1:12,I))

                  IF(ISSUE_EXPENSE_BAL(12,I) == 0.) EXIT
               ENDDO

            ELSE

               AMORTIZATION_START_YR = MAX(1,ISSYR - get_BASE_YeaR())


               AMORT_YR = 0
               DO I=AMORTIZATION_START_YR, FINANCIAL_SIMULATION_YEARS
                  YR = get_BASE_YEAR() + I
                  IF(YR < ISSYR) CYCLE
                  ISSUE_EXPENSE_BAL(0,I) = ISSUE_EXPENSE_BAL(12,I-1)
                  AMORT_YR = MIN(AMORT_YR + 1,AVAIL_DATA_YEARS)
                  MONTH_AMORT = ANNUAL_VECTOR_VALUES(AMORT_YR)/12.
                  IF(YR == ISSYR) THEN
                     MONTH_AMORT = ANNUAL_VECTOR_VALUES(AMORT_YR)/
     +                                  (13.-ISSMO)

                  ENDIF


                  DO MO = 1, 12
                     IF(YR == ISSYR .AND. MO < ISSMO) CYCLE
                     IF(YR == ISSYR .AND. MO == ISSMO) THEN
                        ISSUE_EXPENSE_BAL(MO,I)= ISSUE_EXPENSE_BALANCE
     +                                           - MONTH_AMORT
                        ISSUE_EXPENSE_AMORT(MO,I) = MONTH_AMORT
                        IF(STOP_AT_MATURITY .AND.
     +                            YR == YRMAT .AND. MO == MATMO) THEN
                           ISSUE_EXPENSE_AMORT(MO,I) =
     +                                          ISSUE_EXPENSE_BALANCE
                           ISSUE_EXPENSE_BAL(MO,I) = 0.
                           EXIT
                        ENDIF
                     ELSE
                        IF(STOP_AT_MATURITY .AND.
     +                            YR == YRMAT .AND. MO == MATMO) THEN
                           ISSUE_EXPENSE_AMORT(MO,I) =
     +                                      ISSUE_EXPENSE_BAL(MO-1,I)
                           ISSUE_EXPENSE_BAL(MO,I) = 0.
                           EXIT
                        ELSEIF(ABS(MONTH_AMORT) <
     +                           ABS(ISSUE_EXPENSE_BAL(MO-1,I))) THEN
                           ISSUE_EXPENSE_BAL(MO,I) =
     +                                      ISSUE_EXPENSE_BAL(MO-1,I)
     +                                      - MONTH_AMORT
                           ISSUE_EXPENSE_AMORT(MO,I) = MONTH_AMORT
                        ELSE
                           ISSUE_EXPENSE_AMORT(MO,I) =
     +                                      ISSUE_EXPENSE_BAL(MO-1,I)
                           ISSUE_EXPENSE_BAL(MO,I) = 0.
                           EXIT
                        ENDIF
                     ENDIF
                  ENDDO
                  ISSUE_EXPENSE_AMORT(0,I) =
     +                               SUM(ISSUE_EXPENSE_AMORT(1:12,I))

                  IF(ISSUE_EXPENSE_BAL(12,I) == 0.) EXIT
               ENDDO
            ENDIF
         ELSEIF(ISSUE_EXPENSE_BALANCE /= 0.) THEN ! VECTOR NOT USED

            IF(ISSYR > get_BASE_YeaR()) THEN
               I = ISSYR - get_BASE_YEAR()

               ISSUE_TAX_EXPENSE(ISSMO,I) = ISSUE_EXPENSE_BALANCE
               ISSUE_TAX_EXPENSE(0,I) = ISSUE_EXPENSE_BALANCE
               AMORT_PERIOD_THIS_YEAR = 1. - FRACTN(ISSMO,ISSDA,ISSYR)
               IF(ISSUE_EXP_AMORT_PERIOD >= ISSYR) THEN
                  YEARS_2_AMORTIZE = ISSUE_EXP_AMORT_PERIOD - ISSYR-1 +
     +                 FRACTN(MATMO,MATDA,INT2(ISSUE_EXP_AMORT_PERIOD))
                  YEARS_2_AMORTIZE = YEARS_2_AMORTIZE +
     +                                           AMORT_PERIOD_THIS_YEAR
                  STOP_AT_MATURITY = .TRUE.
               ELSE
                  YEARS_2_AMORTIZE = ISSUE_EXP_AMORT_PERIOD
                  STOP_AT_MATURITY = .FALSE.
               ENDIF
               AMORTIZATION_START_YR = I ! + 1
            ELSE
               AMORTIZATION_START_YR = 1
              ISSUE_EXPENSE_BAL(12,0) = ISSUE_EXPENSE_BALANCE
               IF(ISSUE_EXP_AMORT_PERIOD >= get_BASE_YeaR()) THEN
                  YEARS_2_AMORTIZE = ISSUE_EXP_AMORT_PERIOD-
     +             get_BASE_YEAR()-1+
     +             FRACTN(MATMO,MATDA,INT2(ISSUE_EXP_AMORT_PERIOD))
                  STOP_AT_MATURITY = .TRUE.
               ELSE
                  YEARS_2_AMORTIZE = ISSUE_EXP_AMORT_PERIOD
                  STOP_AT_MATURITY = .FALSE.
               ENDIF
            ENDIF
            MONTHS_2_AMORTIZE = 12. * YEARS_2_AMORTIZE
            IF(YEARS_2_AMORTIZE >= 99.) THEN
               MONTH_AMORT = 0.
            ELSEIF(YEARS_2_AMORTIZE > 0.) THEN
               MONTH_AMORT = ISSUE_EXPENSE_BALANCE/MONTHS_2_AMORTIZE
            ELSE
               MONTH_AMORT = ISSUE_EXPENSE_BALANCE
            ENDIF

            IF(ISSYR <= get_BASE_YeaR()) ISSUE_EXPENSE_BAL(12,0) =
     +                                             ISSUE_EXPENSE_BALANCE

            DO I = AMORTIZATION_START_YR, FINANCIAL_SIMULATION_YEARS
               YR = get_BASE_YEAR() + I
               IF(YR < ISSYR) CYCLE
               ISSUE_EXPENSE_BAL(0,I) = ISSUE_EXPENSE_BAL(12,I-1)

               DO MO = 1, 12
                  IF(YR == ISSYR .AND. MO < ISSMO) CYCLE
                  IF(YR == ISSYR .AND. MO == ISSMO) THEN
                     ISSUE_EXPENSE_BAL(MO,I) = ISSUE_EXPENSE_BALANCE
                     IF(STOP_AT_MATURITY .AND.
     +                              YR == YRMAT .AND. MO == MATMO) THEN
                        ISSUE_EXPENSE_AMORT(MO,I)=ISSUE_EXPENSE_BALANCE
                        ISSUE_EXPENSE_BAL(MO,I) = 0.
                        EXIT
                     ENDIF
                  ELSE
                     IF(STOP_AT_MATURITY .AND.
     +                              YR == YRMAT .AND. MO == MATMO) THEN
                        ISSUE_EXPENSE_AMORT(MO,I) =
     +                                        ISSUE_EXPENSE_BAL(MO-1,I)
                        ISSUE_EXPENSE_BAL(MO,I) = 0.
                        EXIT
                     ELSEIF(ABS(MONTH_AMORT) <
     +                             ABS(ISSUE_EXPENSE_BAL(MO-1,I))) THEN
                        ISSUE_EXPENSE_BAL(MO,I) =
     +                                        ISSUE_EXPENSE_BAL(MO-1,I)
     +                                         - MONTH_AMORT
                        ISSUE_EXPENSE_AMORT(MO,I) = MONTH_AMORT
                     ELSE
                        ISSUE_EXPENSE_AMORT(MO,I) =
     +                                        ISSUE_EXPENSE_BAL(MO-1,I)
                        ISSUE_EXPENSE_BAL(MO,I) = 0.
                        EXIT
                     ENDIF
                  ENDIF
               ENDDO

                  ISSUE_EXPENSE_AMORT(0,I) =
     +                                  SUM(ISSUE_EXPENSE_AMORT(1:12,I))

               IF(ISSUE_EXPENSE_BAL(12,I) == 0.) EXIT
            ENDDO
         ENDIF  ! PREMIUM NOT ZERO

      IF(TYPE == COMMON) THEN
            COMMON_BASEYR = COMMON_BASEYR + BYRBAL
            CALL COMMON_STOCK_ANALYSIS(DESC,AMT,BYRBAL,
     +                                 ISSMO,ISSYR,MATMO,YRMAT,
     +                                 COMMON_SHARES,
     +                                 ASSET_CLASS,ASSET_CLASS_VECTOR,
     +                                 INTRA_COMPANY,INTRA_CLASS_ID,
     +                                 RET,BAL,ISS,
     +                                 SHARES_ISSUED,
     +                                 SHARES_PURCHASED,
     +                                 SHARES_OUTSTANDING,
     +                                 ADJ_RETAINED_EARNINGS,
     +                                 ISSUE_PRICE_PER_SHARE,
     +                                 PURCHASE_PRICE_PER_SHARE)
      ELSE ! NOT COMMON
         IRATE = IRATE/100.
         INTEREST_INPUT_ACTIVE = .FALSE.
         IF(ABS(ITYPE) >= 1) THEN !VECTOR IN THE ASSET VECTOR FILE
            ITYPE = ABS(ITYPE)
            CALL GET_MONTHLY_ANNUAL_VALUES(ITYPE,
     +                                     DATA_TYPE,
     +                                     VECTOR_TYPE,
     +                                     ANNUAL_VECTOR_VALUES,
     +                                     VECTOR_MONTHLY_DATA(1,1),
     +                                     MONTHLY_DATA_UNITS,
     +                                     MONTH_ENDING)
            IF(MONTHLY_MIDAS_ACTIVE) THEN

               CALL RIPPLE_MONTHLY_DATA(ANNUAL_VECTOR_VALUES,
     +                                  VECTOR_MONTHLY_DATA)

               IF(INDEX(VECTOR_TYPE,'Expense') /= 0 .OR.
     +                           INDEX(VECTOR_TYPE,'Income') /= 0) THEN
                  INTEREST_INPUT_ACTIVE = .TRUE.
                  CALL MONTHLY_BOOK_VALUES_IN_DOLLARS(
     +                                            ANNUAL_VECTOR_VALUES,
     +                                             VECTOR_MONTHLY_DATA,
     +                                             MONTHLY_DATA_UNITS,
     +                                             MONTH_ENDING)
                  TREND_NORM = 0.
                  DO YR = 1, FINANCIAL_SIMULATION_YEARS
                     INTEREST_EXPENSE_ADJ(0,YR) =
     +                   ANNUAL_VECTOR_VALUES(MIN(YR,AVAIL_DATA_YEARS))
                     DO MO = 1, 12
                        IF(YR <= LAST_AVAILABLE_MONTHLY_YEAR) THEN
                           INTEREST_EXPENSE_ADJ(MO,YR) =
     +                                       VECTOR_MONTHLY_DATA(MO,YR)
                        ELSEIF(TREND_NORM /= 0.) THEN
                           INTEREST_EXPENSE_ADJ(MO,YR) =
     +                               INTEREST_EXPENSE_ADJ(0,YR) *
     +                                VECTOR_MONTHLY_DATA(MO,
     +                                    LAST_AVAILABLE_MONTHLY_YEAR)/
     +                                                       TREND_NORM
                        ELSE
                           INTEREST_EXPENSE_ADJ(MO,YR) =
     +                                   INTEREST_EXPENSE_ADJ(0,YR)/12.
                        ENDIF
                        IF(YR == LAST_AVAILABLE_MONTHLY_YEAR) THEN
                           TREND_NORM = TREND_NORM +
     +                                       VECTOR_MONTHLY_DATA(MO,YR)
                        ENDIF
                     ENDDO
                  ENDDO
               ELSE
                  CALL RETURN_BASE_YEAR_VECTOR_VALUES(RATE(12,0))
                  RATE(0,0) = RATE(12,0)
                  DO YR = 1, FINANCIAL_SIMULATION_YEARS
                     RATE(0,YR) = IRATE
                     DAYS_IN_CURRENT_YEAR =
     +                       FLOAT(DAYS_IN_YEAR(get_BASE_YEAR()+YR))
                     DAILY_RATE = IRATE/DAYS_IN_CURRENT_YEAR
                     IF(YR <= LAST_AVAILABLE_MONTHLY_YEAR) THEN
                        DO MO = 1, 12
                           MONTHLY_DAILY_RATE =
     +                                     VECTOR_MONTHLY_DATA(MO,YR)/
     +                                    (100. * DAYS_IN_CURRENT_YEAR)
                          RATE(MO,YR) = DAILY_RATE + MONTHLY_DAILY_RATE
                        ENDDO
                     ELSE
                        MONTHLY_DAILY_RATE =
     +                  ANNUAL_VECTOR_VALUES(MIN(YR,AVAIL_DATA_YEARS))/
     +                                    (100. * DAYS_IN_CURRENT_YEAR)
                        DO MO = 1, 12
                          RATE(MO,YR) = DAILY_RATE + MONTHLY_DAILY_RATE
                        ENDDO
                     ENDIF
                  ENDDO
               ENDIF
            ELSE ! INTEREST VECTOR AND NOT MONTHLY
               IF(INDEX(VECTOR_TYPE,'Expense') /= 0 .OR.
     +                           INDEX(VECTOR_TYPE,'Income') /= 0) THEN
                  INTEREST_INPUT_ACTIVE = .TRUE.
                  DO YR = 1, FINANCIAL_SIMULATION_YEARS
                     INTEREST_EXPENSE_ADJ(0,YR) =
     +                   ANNUAL_VECTOR_VALUES(MIN(YR,AVAIL_DATA_YEARS))
                     DO MO = 1, 12
                        INTEREST_EXPENSE_ADJ(MO,YR) =
     +                                   INTEREST_EXPENSE_ADJ(0,YR)/12.
                     ENDDO
                  ENDDO
               ELSE
                  CALL RETURN_BASE_YEAR_VECTOR_VALUES(RATE(12,0))
                  RATE(0,0) = RATE(12,0)
                  DO YR = 1, FINANCIAL_SIMULATION_YEARS
                     RATE(0,YR) = IRATE
                     DAYS_IN_CURRENT_YEAR =
     +                       FLOAT(DAYS_IN_YEAR(get_BASE_YEAR()+YR))
                     DAILY_RATE = IRATE/DAYS_IN_CURRENT_YEAR
                     MONTHLY_DAILY_RATE =
     +                  ANNUAL_VECTOR_VALUES(MIN(YR,AVAIL_DATA_YEARS))/
     +                                 (100. * DAYS_IN_CURRENT_YEAR)
                     DO MO = 1, 12
                        RATE(MO,YR) = DAILY_RATE + MONTHLY_DAILY_RATE
                     ENDDO
                  ENDDO
               ENDIF
            ENDIF
         ELSEIF(ABS(CAPITAL_RATES_VECTOR) >= 1) THEN
            ! USE CAPITAL RATES FILE
            CALL GET_CAP_RATES_MONTHLY_VALUES(VECTOR_FOUND,
     +                                        CAPITAL_RATES_VECTOR,
     +                                        RATE,
     +                                      FINANCIAL_SIMULATION_YEARS)
            DO YR = 1, FINANCIAL_SIMULATION_YEARS
          DAYS_IN_CURRENT_YEAR = FLOAT(DAYS_IN_YEAR(get_BASE_YEAR()+YR))
               DAILY_RATE = IRATE/DAYS_IN_CURRENT_YEAR
               DO MO = 1, 12
                  MONTHLY_DAILY_RATE = RATE(MO,YR)/
     +                                    (100. * DAYS_IN_CURRENT_YEAR)
                  RATE(MO,YR) = DAILY_RATE + MONTHLY_DAILY_RATE
               ENDDO
            ENDDO
         ELSE
            DO YR = 0, FINANCIAL_SIMULATION_YEARS
          DAYS_IN_CURRENT_YEAR = FLOAT(DAYS_IN_YEAR(get_BASE_YEAR()+YR))
               RATE(0,YR) = IRATE
               DAILY_RATE = IRATE/DAYS_IN_CURRENT_YEAR
               DO MO = 1, 12
                  RATE(MO,YR) = DAILY_RATE
               ENDDO
            ENDDO
         ENDIF

! MODIFY INTEREST RATE USING LATIN HYPERCUBE

         IF(INDEX(LHS_DISTRIBUTION,"Not Active") == 0) THEN
            SCENARIO_INDEX = GET_SCENARIO_INDEX(LHS_DISTRIBUTION)
            IF(SCENARIO_INDEX > 0) THEN
               DO YR = 1, FINANCIAL_SIMULATION_YEARS
                  RATE(0,YR) = 0.
                  DO MO = 1, 12
                     RATE(MO,YR) = RATE(MO,YR)
     +                    * GET_SCENARIO_BY_INDEX(YR,MO,SCENARIO_INDEX)
                     RATE(0,YR) = RATE(0,YR) + RATE(MO,YR)
                  ENDDO
                  RATE(0,YR) = RATE(0,YR)/12.
               ENDDO
            ENDIF
         ENDIF
! END LHC MODIFICATIONS
         ANNINT(0) = RATE(0,0) * BYRBAL
         BAL(12,0) = BYRBAL
         ISS(12,0) = BYRBAL

! SET-UP SINKING FUND ANNUAL AMOUNTS USING A VECTOR POINTER
! NOTE THAT FOR MORTGAGE DEBT THE SINKING AMOUNT IS THE ANNUAL
! PRINCIPLE PAYMENTS. ALL OTHER SINKING FUND INFO IS LOST

         IF(SINKING_FUND_VECTOR /= 0) THEN
            SINKING_FUND_VECTOR = ABS(SINKING_FUND_VECTOR)
            CALL GET_MONTHLY_ANNUAL_VALUES(SINKING_FUND_VECTOR,
     +                                     DATA_TYPE,
     +                                     VECTOR_TYPE,
     +                                     ANNUAL_VECTOR_VALUES,
     +                                     VECTOR_MONTHLY_DATA(1,1),
     +                                     MONTHLY_DATA_UNITS,
     +                                     MONTH_ENDING)
            IF(MONTHLY_MIDAS_ACTIVE) THEN

               CALL RIPPLE_MONTHLY_DATA(ANNUAL_VECTOR_VALUES,
     +                                  VECTOR_MONTHLY_DATA)

               CALL MONTHLY_BOOK_VALUES_IN_DOLLARS(ANNUAL_VECTOR_VALUES,

     +                                             VECTOR_MONTHLY_DATA,
     +                                             MONTHLY_DATA_UNITS,
     +                                             MONTH_ENDING)

               NORMALIZE_YR = 1
               DO YR = 1, FINANCIAL_SIMULATION_YEARS
                  globecom_year = YR + get_BASE_YEAR()
                  IF(globecom_year > YRMAT) then
                    EXIT
                  end if
                  IF(globecom_year < ISSYR) THEN
                     SINKING_FUND(:,YR) = 0.
                     CYCLE
                  ENDIF
                  IF(YR <= AVAIL_DATA_YEARS) THEN
                     SINKING_FUND(0,YR) = ANNUAL_VECTOR_VALUES(YR)
                  ELSE
                     SINKING_FUND(0,YR) =
     +                           ANNUAL_VECTOR_VALUES(AVAIL_DATA_YEARS)
                  ENDIF
                  DO MO = 1, 12
                     IF(globecom_year == YRMAT .AND. MO == MATMO) then
                        EXIT
                     end if
                     IF(globecom_year == ISSYR .AND. MO < ISSMO) THEN
                        SINKING_FUND(MO,YR) = 0.
                        CYCLE
                     ENDIF
                     IF(YR <= LAST_AVAILABLE_MONTHLY_YEAR) THEN
                       SINKING_FUND(MO,YR) = VECTOR_MONTHLY_DATA(MO,YR)
                     ELSE

                        IF(SINKING_FUND(0,NORMALIZE_YR) == 0. .OR.
     +                   (NORMALIZE_YR + get_BASE_YEAR() == ISSYR .AND.
     +                      MONTHLY_DATA_UNITS(NORMALIZE_YR)/="D")) THEN

                           SINKING_FUND(ISSMO,YR)=SINKING_FUND(0,YR)
                           EXIT
                        ELSE
                           SINKING_FUND(MO,YR) = SINKING_FUND(0,YR) *
     +                                SINKING_FUND(MO,NORMALIZE_YR)/
     +                                     SINKING_FUND(0,NORMALIZE_YR)
                        ENDIF
                     ENDIF
                     IF(YR <= LAST_AVAILABLE_MONTHLY_YEAR .AND.
     +                                   SINKING_FUND(0,YR) /= 0.) THEN
                        NORMALIZE_YR = YR
                     ENDIF
                  ENDDO
               ENDDO
            ELSE
               DO YR = 1, FINANCIAL_SIMULATION_YEARS
                  globecom_year = YR + get_BASE_YEAR()
                  IF(globecom_year > YRMAT) then
                    EXIT
                  end if
                  IF(globecom_year < ISSYR) THEN
                     CYCLE
                  ENDIF
                  IF(YR <= AVAIL_DATA_YEARS) THEN
                     SINKING_FUND(0,YR) = ANNUAL_VECTOR_VALUES(YR)
                  ELSE
                     SINKING_FUND(0,YR) =
     +                           ANNUAL_VECTOR_VALUES(AVAIL_DATA_YEARS)
                  ENDIF
                  IF(globecom_year == ISSYR .AND. MATMO < ISSMO) THEN
                     SINKING_FUND(ISSMO,YR) = SINKING_FUND(0,YR)
                  ELSE
                     SINKING_FUND(MATMO,YR) = SINKING_FUND(0,YR)
                  ENDIF
               ENDDO
            ENDIF
         ENDIF

         SFYA = MAX0(1,SFY1 - get_BASE_YEAR())
         SFYB = SFY2 - get_BASE_YEAR()
         SINKING_FUND_PAYMENT = SFA1
         DO YR = 1, FINANCIAL_SIMULATION_YEARS
            globecom_year = get_BASE_YEAR() + YR
            IF(globecom_year > YRMAT) then
                EXIT
            end if
            IF(globecom_year < ISSYR .OR. YR < SFYA) then
                CYCLE
            end if
            IF(YR >= SFYB .AND. SFYB > 0) SINKING_FUND_PAYMENT = SFA2
            IF(TYPE == MTG .AND. SINKING_FUND_VECTOR == 0) THEN
               CALL MGT_DEBT_PandI_PAYMENTS(YR,INTEREST_PAID_WHEN,
     +                                      SINKING_FUND,
     +                                      SINKING_FUND_PAYMENT,
     +                                      ISSMO,ISSYR,globecom_year,
     +                                      PAYMENT_LAGS_ISSUE_MONTH)
            ELSE
               IF(globecom_year == YRMAT .AND. ISSMO >= MATMO) then
                EXIT
               end if
               SINKING_FUND(ISSMO,YR) = SINKING_FUND(ISSMO,YR)
     +                                  + SINKING_FUND_PAYMENT
               SINKING_FUND(0,YR) = SINKING_FUND(0,YR)
     +                              + SINKING_FUND_PAYMENT
            ENDIF
         ENDDO

! PREMIUM/DISCOUNT AMORTIZATION

         IF(ISSYR <= get_BASE_YeaR()) then
            PREMIUM_BAL(12,0) = PREM_DISC_BALANCE
        end if

         IF(PREM_DISC_BALANCE /= 0. .AND.
     +                                PREM_DISC_AMORT_PERIOD < 0.) THEN
           CALL GET_MONTHLY_ANNUAL_VALUES(INT2(PREM_DISC_AMORT_PERIOD),
     +                                     DATA_TYPE,
     +                                     VECTOR_TYPE,
     +                                     ANNUAL_VECTOR_VALUES,
     +                                     VECTOR_MONTHLY_DATA(1,1),
     +                                     MONTHLY_DATA_UNITS,
     +                                     MONTH_ENDING)
            STOP_AT_MATURITY = .FALSE.
            IF(MONTHLY_MIDAS_ACTIVE) THEN

               CALL RIPPLE_MONTHLY_DATA(ANNUAL_VECTOR_VALUES,
     +                                  VECTOR_MONTHLY_DATA)

               CALL MONTHLY_BOOK_VALUES_IN_DOLLARS(
     +                                          ANNUAL_VECTOR_VALUES,
     +                                          VECTOR_MONTHLY_DATA,
     +                                          MONTHLY_DATA_UNITS,
     +                                          MONTH_ENDING)
               AMORTIZATION_START_YR = MAX(1,ISSYR - get_BASE_YEAR())
               AMORT_YR = 0
               DO I=AMORTIZATION_START_YR, FINANCIAL_SIMULATION_YEARS
                  YR = get_BASE_YEAR() + I
                  IF(YR < ISSYR) CYCLE
                  PREMIUM_BAL(0,I) = PREMIUM_BAL(12,I-1)
                  AMORT_YR = MIN(AMORT_YR + 1,AVAIL_DATA_YEARS)

                  DO MO = 1, 12

                     IF(YR == ISSYR .AND. MO < ISSMO) CYCLE
                     IF(YR == ISSYR .AND. MO == ISSMO) THEN
                        PREMIUM_BAL(ISSMO,I) = PREM_DISC_BALANCE
                        PREMIUM_DISCOUNT_EXP(ISSMO,I)=PREM_DISC_BALANCE
                        PREMIUM_DISCOUNT_EXP(0,I) = PREM_DISC_BALANCE
                     ELSE
                        IF(I < AMORTIZATION_START_YR
     +                           + LAST_AVAILABLE_MONTHLY_YEAR) THEN
                           MONTH_AMORT = VECTOR_MONTHLY_DATA(MO,
     +                                       I+1-AMORTIZATION_START_YR)
                        ELSE
                          MONTH_AMORT = ANNUAL_VECTOR_VALUES(AMORT_YR)/
     +                                                              12.
                        ENDIF
                        IF(ABS(MONTH_AMORT) <
     +                                   ABS(PREMIUM_BAL(MO-1,I))) THEN
                           PREMIUM_BAL(MO,I) = PREMIUM_BAL(MO-1,I)
     +                                         - MONTH_AMORT
                           PREMIUM_AMORT(MO,I) = MONTH_AMORT
                        ELSE
                           PREMIUM_AMORT(MO,I) = PREMIUM_BAL(MO-1,I)
                           PREMIUM_BAL(MO,I) = 0.
                           EXIT
                        ENDIF
                     ENDIF
                  ENDDO
                  PREMIUM_AMORT(0,I) = SUM(PREMIUM_AMORT(1:12,I))
                  IF(PREMIUM_BAL(12,I) == 0.) EXIT
               ENDDO
            ELSE
               AMORTIZATION_START_YR = MAX(1,ISSYR - get_BASE_YEAR())
               AMORT_YR = 0
               DO I=AMORTIZATION_START_YR, FINANCIAL_SIMULATION_YEARS
                  YR = get_BASE_YEAR() + I
                  IF(YR < ISSYR) CYCLE
                  PREMIUM_BAL(0,I) = PREMIUM_BAL(12,I-1)
                  AMORT_YR = MIN(AMORT_YR + 1,AVAIL_DATA_YEARS)
                  MONTH_AMORT = ANNUAL_VECTOR_VALUES(AMORT_YR)/12.

                  DO MO = 1, 12

                     IF(YR == ISSYR .AND. MO < ISSMO) CYCLE
                     IF(YR == ISSYR .AND. MO == ISSMO) THEN
                        PREMIUM_BAL(ISSMO,I) = PREM_DISC_BALANCE
                        PREMIUM_DISCOUNT_EXP(ISSMO,I)=PREM_DISC_BALANCE
                        PREMIUM_DISCOUNT_EXP(0,I) = PREM_DISC_BALANCE
                        IF(STOP_AT_MATURITY .AND.
     +                              YR == YRMAT .AND. MO == MATMO) THEN
                           PREMIUM_AMORT(MO,I) = PREM_DISC_BALANCE
                           PREMIUM_BAL(MO,I) = 0.
                           EXIT
                        ENDIF
                     ELSE
                        IF(STOP_AT_MATURITY .AND.
     +                              YR == YRMAT .AND. MO == MATMO) THEN
                           PREMIUM_AMORT(MO,I) = PREMIUM_BAL(MO-1,I)
                           PREMIUM_BAL(MO,I) = 0.
                           EXIT
                        ELSEIF(ABS(MONTH_AMORT) <
     +                                   ABS(PREMIUM_BAL(MO-1,I))) THEN
                           PREMIUM_BAL(MO,I) = PREMIUM_BAL(MO-1,I)
     +                                         - MONTH_AMORT
                           PREMIUM_AMORT(MO,I) = MONTH_AMORT
                        ELSE
                           PREMIUM_AMORT(MO,I) = PREMIUM_BAL(MO-1,I)
                           PREMIUM_BAL(MO,I) = 0.
                           EXIT
                        ENDIF
                     ENDIF
                  ENDDO
                  PREMIUM_AMORT(0,I) = SUM(PREMIUM_AMORT(1:12,I))
                  IF(PREMIUM_BAL(12,I) == 0.) EXIT
               ENDDO
            ENDIF
         ELSEIF(PREM_DISC_BALANCE /= 0.) THEN

            IF(ISSYR > get_BASE_YeaR()) THEN
               I = ISSYR - get_BASE_YEAR()

               AMORT_PERIOD_THIS_YEAR = 1. - FRACTN(ISSMO,ISSDA,ISSYR)
               IF(PREM_DISC_AMORT_PERIOD >= ISSYR) THEN
                  YEARS_2_AMORTIZE = PREM_DISC_AMORT_PERIOD - ISSYR-1+
     +                 FRACTN(MATMO,MATDA,INT2(PREM_DISC_AMORT_PERIOD))
                  YEARS_2_AMORTIZE = YEARS_2_AMORTIZE +
     +                                           AMORT_PERIOD_THIS_YEAR
                  STOP_AT_MATURITY = .TRUE.
               ELSE
                  YEARS_2_AMORTIZE = PREM_DISC_AMORT_PERIOD
                  STOP_AT_MATURITY = .FALSE.
               ENDIF
               AMORTIZATION_START_YR = I ! + 1
            ELSE
               AMORTIZATION_START_YR = 1
               PREMIUM_BAL(12,0) = PREM_DISC_BALANCE
               IF(PREM_DISC_AMORT_PERIOD > get_BASE_YEAR()) THEN
         YEARS_2_AMORTIZE = PREM_DISC_AMORT_PERIOD-get_BASE_YEAR()- 1+
     +                 FRACTN(MATMO,MATDA,INT2(PREM_DISC_AMORT_PERIOD))
                  STOP_AT_MATURITY = .TRUE.
               ELSE
                  YEARS_2_AMORTIZE = PREM_DISC_AMORT_PERIOD
                  STOP_AT_MATURITY = .FALSE.
               ENDIF
            ENDIF
            MONTHS_2_AMORTIZE = 12. * YEARS_2_AMORTIZE
            IF(YEARS_2_AMORTIZE >= 99.) THEN
               MONTH_AMORT = 0.
            ELSEIF(YEARS_2_AMORTIZE > 1.) THEN
               MONTH_AMORT = PREM_DISC_BALANCE/MONTHS_2_AMORTIZE
            ELSE
               MONTH_AMORT = PREM_DISC_BALANCE
            ENDIF

            DO I = AMORTIZATION_START_YR, FINANCIAL_SIMULATION_YEARS
               YR = get_BASE_YEAR() + I
               IF(YR < ISSYR) CYCLE
               PREMIUM_BAL(0,I) = PREMIUM_BAL(12,I-1)

               DO MO = 1, 12

                  IF(YR == ISSYR .AND. MO < ISSMO) CYCLE
                  IF(YR == ISSYR .AND. MO == ISSMO) THEN
                     PREMIUM_BAL(ISSMO,I) = PREM_DISC_BALANCE
                     PREMIUM_DISCOUNT_EXP(ISSMO,I) = PREM_DISC_BALANCE
                     PREMIUM_DISCOUNT_EXP(0,I) = PREM_DISC_BALANCE
                     IF(STOP_AT_MATURITY .AND.
     +                              YR == YRMAT .AND. MO == MATMO) THEN
                        PREMIUM_AMORT(MO,I) = PREM_DISC_BALANCE
                        PREMIUM_BAL(MO,I) = 0.
                        EXIT
                     ENDIF
                  ELSE
                     IF(STOP_AT_MATURITY .AND.
     +                              YR == YRMAT .AND. MO == MATMO) THEN
                        PREMIUM_AMORT(MO,I) = PREMIUM_BAL(MO-1,I)
                        PREMIUM_BAL(MO,I) = 0.
                        EXIT
                     ELSEIF(ABS(MONTH_AMORT) <
     +                                   ABS(PREMIUM_BAL(MO-1,I))) THEN
                        PREMIUM_BAL(MO,I) = PREMIUM_BAL(MO-1,I)
     +                                      - MONTH_AMORT
                        PREMIUM_AMORT(MO,I) = MONTH_AMORT
                     ELSE
                        PREMIUM_AMORT(MO,I) = PREMIUM_BAL(MO-1,I)
                        PREMIUM_BAL(MO,I) = 0.
                        EXIT
                     ENDIF
                  ENDIF
               ENDDO
               DO MO = 1, 12
                  PREMIUM_AMORT(0,I) = PREMIUM_AMORT(0,I)
     +                                 + PREMIUM_AMORT(MO,I)
               ENDDO
               IF(PREMIUM_BAL(12,I) == 0.) EXIT
            ENDDO
         ENDIF

! COMPUTE ANNUAL ADDITIONS, RETIREMENTS, AND INTEREST PAYMENTS

         BAL(12,0) = BYRBAL
         ISS(12,0) = BYRBAL
         ISS(0,0) = BYRBAL
         ISSUE_ACURRED = .FALSE.
         IF(TYPE == MTG) THEN
            IF(MONTHLY_MIDAS_ACTIVE) THEN
               MGT_ACCURRED_INTEREST = 0.
            ELSE
               IF(ISSYR > get_BASE_YEAR()) THEN
                  MGT_ACCURRED_INTEREST = 0.
               ELSE
                  MGT_ACCURRED_INTEREST = RATE(1,1) * BAL(12,0) *
     +  FLOAT(REMAINING_DAYS_IN_MONTH(INT2(12),ISSDA,get_BASE_YEAR()))
                  MGT_ACCURRED_INTEREST = 0.
               ENDIF
            ENDIF
            DO I = 1, FINANCIAL_SIMULATION_YEARS
               BAL(0,I) = BAL(12,I-1)
               YR = get_BASE_YEAR() + I
               SINKING_FUND(0,I) = 0.
               IF(YR < ISSYR) then
                CYCLE
               end if
               DO MO = 1, 12
                  IF (YR == ISSYR .AND. MO < ISSMO) THEN
                     SINKING_FUND(MO,I) = 0.
                     CYCLE
                  ENDIF
                  IF (YR == ISSYR .AND. MO == ISSMO) THEN
! DEBT OF QUANTITY AMT IS ISSUED IN Ith YEAR
                     ISSUE_ACURRED = .TRUE.
                     ISS(MO,I) = AMT
                     ISS(0,I) = ISS(0,I) + AMT
                     IF(YR == YRMAT .AND. MO == MATMO) THEN
                        BAL(MO,I) = 0.
                        RET(MO,I) = AMT
                        RET(0,I) = RET(0,I) + AMT
                        INTEREST(MO,I) = RATE(MO,I) * AMT *
     +                             FRACTION_OF_MONTH(MO,ISSDA,MATDA,YR)
     +                              + INTEREST_EXPENSE_ADJ(MO,I)
                        CALL ZERO_REMAINING_MONTHS(SINKING_FUND,I,MO)
                        CASH_PAYMENTS(MO,I) = INTEREST(MO,I)
                        EXIT
                     ELSE

                        BAL(MO,I) = AMT
                        IF(WVPA() .AND. INTEREST_PAID_WHEN == 'L') THEN
                           ! FIRST PAYMENT INTEREST ONLY
                           INTEREST(MO,I) = RATE(MO,I) * BAL(MO,I) *
     +                      FLOAT(REMAINING_DAYS_IN_MONTH(MO,ISSDA,YR))
     +                       + INTEREST_EXPENSE_ADJ(MO,I)
                           IF(ISSMO == 1) THEN
                              MGT_ACCURRED_INTEREST = INTEREST(MO,I)
                              SINKING_FUND(1,I) = MGT_ACCURRED_INTEREST
                           ELSEIF(ISSMO >= 11) THEN
                              MGT_ACCURRED_INTEREST = INTEREST(MO,I)
                              DO MO1 = MO+1, 12
                                 MGT_ACCURRED_INTEREST =
     +                                     MGT_ACCURRED_INTEREST
     +                                     + 30.*RATE(MO1,I)*BAL(MO,I)
     +                                    + INTEREST_EXPENSE_ADJ(MO1,I)
                              ENDDO
                              IF(I < FINANCIAL_SIMULATION_YEARS) THEN
                                 MGT_ACCURRED_INTEREST =
     +                                     MGT_ACCURRED_INTEREST
     +                                     + 30.*RATE(1,I+1)*BAL(MO,I)
     +                                    + INTEREST_EXPENSE_ADJ(1,I+1)
                                 SINKING_FUND(1,I+1) =
     +                                            MGT_ACCURRED_INTEREST
                              ENDIF
                           ELSE
                              IF(ISSMO <= 10) END_MO = 10 !9
                              IF(ISSMO <= 7) END_MO =  7 !6
                              IF(ISSMO <= 4) END_MO = 4 !3
                              MGT_ACCURRED_INTEREST = INTEREST(MO,I)
                              DO MO1 = MO+1, END_MO
                                 MGT_ACCURRED_INTEREST =
     +                                     MGT_ACCURRED_INTEREST
     +                                     + 30.*RATE(MO1,I)*BAL(MO,I)
     +                                    + INTEREST_EXPENSE_ADJ(MO1,I)
                              ENDDO
                              SINKING_FUND(END_MO,I) =
     +                                            MGT_ACCURRED_INTEREST
                           ENDIF
                           ACCRUED_INTEREST_PAYABLE = INTEREST(MO,I)
                           MGT_ACCURRED_INTEREST = 0.
                           IF(SINKING_FUND(MO,I) /= 0.) THEN
                              CASH_PAYMENTS(MO,I) = INTEREST(MO,I)

                              SINKING_FUND(MO,I) = 0.
                              ACCRUED_INTEREST_PAYABLE = 0.
                           ENDIF
                        ELSE
                           INTEREST(MO,I) = INTEREST_EXPENSE_ADJ(MO,I)
                          MGT_ACCURRED_INTEREST = RATE(MO,I)*BAL(MO,I)*
     +                      FLOAT(REMAINING_DAYS_IN_MONTH(MO,ISSDA,YR))
     +                              + INTEREST_EXPENSE_ADJ(MO,I)
                           ACCRUED_INTEREST_PAYABLE = INTEREST(MO,I)
                          IF(WVPA().AND.INTEREST_PAID_WHEN == 'M') THEN
                              ACCRUED_INTEREST_PAYABLE =
     +                                            MGT_ACCURRED_INTEREST
                              INTEREST(MO,I) = MGT_ACCURRED_INTEREST
                              IF(SINKING_FUND(MO,I) /= 0.) THEN
                                 INTEREST_IN_SPLIT_MONTH(MO,I) = 0.
                                 MGT_ACCURRED_INTEREST = 0.
                                 PRINCIPAL_PAYMENT = MIN(BAL(MO,I),
     +                                       SINKING_FUND(MO,I)
     +                                      - ACCRUED_INTEREST_PAYABLE)
                                 CASH_PAYMENTS(MO,I) =
     +                                         ACCRUED_INTEREST_PAYABLE
                                 BAL(MO,I) = BAL(MO,I)
     +                                       - PRINCIPAL_PAYMENT
                                 RET(MO,I) = RET(MO,I)
     +                                       + PRINCIPAL_PAYMENT
                                 SINKING_FUND(MO,I) = 0.
                                 ACCRUED_INTEREST_PAYABLE = 0.
                              ENDIF
                           ENDIF
                        ENDIF
                     ENDIF

                  ELSEIF(YR == YRMAT .AND. MO == MATMO) THEN
                     RET(MO,I) = RET(MO,I) + BAL(MO-1,I)
                     RET(0,I) = RET(0,I) + BAL(MO-1,I)
                     BAL(MO,I) = 0.
                     SINKING_FUND(MO,I) = 0.
                     INTEREST(MO,I) = RATE(MO,I)*BAL(MO-1,I)*(MATDA-1)
     +                                + MGT_ACCURRED_INTEREST
     +                                + INTEREST_EXPENSE_ADJ(MO,I)
                     CALL ZERO_REMAINING_MONTHS(SINKING_FUND,I,MO)
                     CASH_PAYMENTS(MO,I) = INTEREST(MO,I)
     +                                     + ACCRUED_INTEREST_PAYABLE
                     EXIT
                  ELSE
                     IF(SINKING_FUND(MO,I) /= 0.) THEN

                      IF(WVPA()) THEN ! CURRENT MONTH INTEREST INCLUDED
                           INTEREST(MO,I) = 30.*RATE(MO,I)*BAL(MO-1,I)
     +                                     + INTEREST_EXPENSE_ADJ(MO,I)
                           ACCRUED_INTEREST_PAYABLE = INTEREST(MO,I)
     +                                       + ACCRUED_INTEREST_PAYABLE
                           INTEREST_IN_SPLIT_MONTH(MO,I) = 0.
                           MGT_ACCURRED_INTEREST = 0.
                        ELSE
                           INTEREST(MO,I)=RATE(MO,I)*
     +                                            (ISSDA-1)*BAL(MO-1,I)
                           INTEREST_IN_SPLIT_MONTH(MO,I)=INTEREST(MO,I)
                           ACCRUED_INTEREST_PAYABLE = INTEREST(MO,I)
     +                                       + ACCRUED_INTEREST_PAYABLE
                        ENDIF
                        PRINCIPAL_PAYMENT = MIN(BAL(MO-1,I),
     +                                SINKING_FUND(MO,I)
     +                                - ACCRUED_INTEREST_PAYABLE)
                        CASH_PAYMENTS(MO,I) = ACCRUED_INTEREST_PAYABLE
                        BAL(MO,I) = BAL(MO-1,I)
     +                              - PRINCIPAL_PAYMENT
                        RET(MO,I) = RET(MO,I)
     +                              + PRINCIPAL_PAYMENT
                        SINKING_FUND(MO,I) = 0.
                        IF(WVPA()) THEN

                            ACCRUED_INTEREST_PAYABLE = 0.
                        ELSE
                          MGT_ACCURRED_INTEREST = RATE(MO,I)*BAL(MO,I)*
     +                      FLOAT(REMAINING_DAYS_IN_MONTH(MO,ISSDA,YR))
                           INTEREST(MO,I) = INTEREST(MO,I)
     +                                      + MGT_ACCURRED_INTEREST

     +                                      + INTEREST_EXPENSE_ADJ(MO,I)

                           ACCRUED_INTEREST_PAYABLE =
     +                                      MGT_ACCURRED_INTEREST
     +                                     + INTEREST_EXPENSE_ADJ(MO,I)
                        ENDIF

                     ELSE
                        BAL(MO,I) = BAL(MO-1,I)
                        IF(WVPA()) THEN
                           INTEREST(MO,I) = 30.*RATE(MO,I)*BAL(MO-1,I)
     +                                     + INTEREST_EXPENSE_ADJ(MO,I)
                           INTEREST_IN_SPLIT_MONTH(MO,I) = 0.
                        ELSE
                           INTEREST(MO,I) = RATE(MO,I) * BAL(MO,I) *
     +                                             DAYS_IN_MONTH(MO,YR)
     +                                   + INTEREST_EXPENSE_ADJ(MO,I)
                           INTEREST_IN_SPLIT_MONTH(MO,I) =
     +                                   RATE(MO,I)*(ISSDA-1)*BAL(MO,I)
                        ENDIF
                        ACCRUED_INTEREST_PAYABLE =
     +                                         ACCRUED_INTEREST_PAYABLE
     +                                          + INTEREST(MO,I)
                     ENDIF
                  ENDIF
                  IF(BAL(MO,I) == 0.) THEN
                     CALL ZERO_REMAINING_MONTHS(SINKING_FUND,I,MO)

                     EXIT
                  ENDIF
               ENDDO ! MO
               CASH_PAYMENTS(0,I) = SUM(CASH_PAYMENTS(1:,I))
               RET(0,I) = SUM(RET(1:,I))
               INTEREST(0,I) = SUM(INTEREST(1:,I))
               SINKING_FUND(0,I) = SUM(SINKING_FUND(1:,I)) !  2/23/03
               ANNINT(I) = RATE(0,I) * BAL(12,I)
            ENDDO ! YR
! ELSE USE THE AMORTIZATION SCHEDULE PREVIOUSLY COMPUTED
         ELSE ! end MGT
! DEBT ISSUE IS EITHER LTD OR PFS (TREATED ALIKE)
            DO I = 1, FINANCIAL_SIMULATION_YEARS
               BAL(0,I) = BAL(12,I-1)
               YR = get_BASE_YEAR() + I
               IF(YR < ISSYR) CYCLE
               DO MO = 1, 12
                  IF (YR == ISSYR .AND. MO < ISSMO) THEN
                     SINKING_FUND(MO,I) = 0.
                     CYCLE
                  ENDIF
                  IF (YR == ISSYR .AND. MO == ISSMO) THEN
! DEBT OF QUANTITY AMT IS ISSUED IN Ith YEAR
                     ISSUE_ACURRED = .TRUE.
                     ISS(MO,I) = AMT
                     ISS(0,I) = ISS(0,I) + AMT
                     IF(YR == YRMAT .AND. MO == MATMO) THEN
                        BAL(MO,I) = 0.
                        RET(MO,I) = AMT
                        INTEREST(MO,I) = RATE(MO,I) * AMT *
     +                             FRACTION_OF_MONTH(MO,ISSDA,MATDA,YR)
                        EXIT
                     ELSE
                        BAL(MO,I) = MAX(0.,AMT - SINKING_FUND(MO,I))
                       SINKING_FUND(MO,I) = MIN(AMT,SINKING_FUND(MO,I))
                        INTEREST(MO,I) = RATE(MO,I) * BAL(MO,I) *
     +                      FLOAT(REMAINING_DAYS_IN_MONTH(MO,ISSDA,YR))
                     ENDIF
! ELSE (YR < ISSYR) => NO COMPUTATIONS YET
                  ELSEIF(YR == YRMAT .AND. MO == MATMO) THEN
                     RET(MO,I) = RET(MO,I) + BAL(MO-1,I)
                     BAL(MO,I) = 0.
                     SINKING_FUND(MO,I) = 0.
                     INTEREST(MO,I) = RATE(MO,I)*BAL(MO-1,I)*(MATDA-1)

                  ELSE
                     IF(SINKING_FUND(MO,I) /= 0.) THEN
                        FRACTION = TIME_FROM_FIRST_DAY(MO,
     +                                                ISSDA-INT2(1),YR)
                        INTEREST(MO,I)=RATE(MO,I)*(ISSDA-1)*BAL(MO-1,I)
                        INTEREST_IN_SPLIT_MONTH(MO,I) = INTEREST(MO,I)
                        SINKING_FUND(MO,I) =
     +                              MIN(BAL(MO-1,I),SINKING_FUND(MO,I))
                        BAL(MO,I) = BAL(MO-1,I)-SINKING_FUND(MO,I)
                        INTEREST(MO,I) = INTEREST(MO,I) +
     +                       RATE(MO,I) * BAL(MO,I) *

     +                       FLOAT(REMAINING_DAYS_IN_MONTH(MO,ISSDA,YR))

                     ELSE
                        BAL(MO,I) = BAL(MO-1,I)
                        INTEREST(MO,I) = RATE(MO,I) * BAL(MO,I) *
     +                                             DAYS_IN_MONTH(MO,YR)
                        INTEREST_IN_SPLIT_MONTH(MO,I) =
     +                                   RATE(MO,I)*(ISSDA-1)*BAL(MO,I)
                     ENDIF
                  ENDIF
                  IF(BAL(MO,I) == 0.) THEN
                     SINKING_FUND(MO+1:,I) = 0.
                     EXIT
                  ENDIF
               ENDDO ! MO
               INTEREST(0,I) = SUM(INTEREST(1:,I))
               SINKING_FUND(0,I) = SUM(SINKING_FUND(1:,I))
               RET(0,I) = SUM(RET(1:,I))
               ANNINT(I) = RATE(0,I) * BAL(12,I)
               IF(BAL(MIN(MO,12),I) == 0.) THEN
                  DO YR = I+1, FINANCIAL_SIMULATION_YEARS
                     SINKING_FUND(:,YR) = 0.
                  ENDDO
                  EXIT
               ENDIF
            ENDDO ! YR

! ADDED TO HARDWIRE INTEREST 10/21/98 MSG

            IF(INTEREST_INPUT_ACTIVE) THEN
               DO YR = 1, FINANCIAL_SIMULATION_YEARS
                  DO MO = 0, 12
                     INTEREST(MO,YR) = INTEREST(MO,YR)
     +                              + INTEREST_EXPENSE_ADJ(MO,YR)
                  ENDDO
               ENDDO
            ENDIF
         ENDIF

! CURRENT PORTION OF LTD

         IF(TYPE == LTD .OR. TYPE == MTG) THEN
            DO YR = 1, FINANCIAL_SIMULATION_YEARS-1
               globecom_year = YR + get_BASE_YEAR()
               IF(globecom_year > YRMAT) then
                EXIT
               end if
               IF(globecom_year < ISSYR) then
                CYCLE
               end if
               DO MO = 1, 12
                  IF(globecom_year == YRMAT .AND. MO == MATMO) then
                    EXIT
                  end if
                  IF(globecom_year == ISSYR .AND. MO < ISSMO) then
                    CYCLE
                  end if
                  IF(YR == 1 .AND. MO == 1) THEN
                     LTD_CURRENT_PORTION(0,YR) =
     +                                       SUM(SINKING_FUND(1:12,YR))
     +                                        + SUM(RET(1:12,YR))
                  ENDIF
                  IF(MO == 12) THEN
                     LTD_CURRENT_PORTION(MO,YR) =
     +                                     SUM(SINKING_FUND(1:12,YR+1))
     +                                      + SUM(RET(1:12,YR+1))
                     LTD_CURRENT_PORTION(0,YR+1) =
     +                                       LTD_CURRENT_PORTION(MO,YR)
                  ELSE
                     LTD_CURRENT_PORTION(MO,YR) =
     +                                    SUM(SINKING_FUND(MO+1:12,YR))
     +                                    + SUM(RET(MO+1:12,YR))
     +                                   + SUM(SINKING_FUND(1:MO,YR+1))
     +                                    + SUM(RET(1:MO,YR+1))
                  ENDIF
               ENDDO
            ENDDO
         ENDIF ! CURRENT PORTION
      ENDIF ! NOT COMMON STOCK

! ALIGN THE BOOKED VALUES WITH THE CASH FLOWS

      IF(TYPE == COMMON) THEN
         CALL STOCK_BOOK_2_CASH(INTEREST,BAL,
     +                          ACCRUED_INTEREST_PAYABLE,
     +                          PAYMENT_LAGS_ISSUE_MONTH,
     +                          CASH_PAYMENTS,
     +                          FINANCIAL_SIMULATION_YEARS)
      ELSEIF(TYPE == MTG) THEN

      ELSE
         CALL LTD_BOOK_2_CASH(YRMAT-get_BASE_YEAR(),MATMO,
     +                        INTEREST,
     +                        INTEREST_IN_SPLIT_MONTH,
     +                        BAL,
     +                        ACCRUED_INTEREST_PAYABLE,
     +                        INTEREST_PAID_WHEN,
     +                        PAYMENT_LAGS_ISSUE_MONTH,ISSMO,
     +                        CASH_PAYMENTS,
     +                        FINANCIAL_SIMULATION_YEARS)
      ENDIF

!$ifdefined(fixthiscode)
! WRITE ASSET RESULTS

         IF((DEBT_ASSET_REPORT .AND. DELETE > 1) .OR.
     +                                        REPORT_ALL_ACCOUNTS) THEN
            ANNUAL_INFO_ACTIVE = DRILLING_REPRT_LEVEL == 'A'
            IF(DEBT_ASSET_REPORT .AND. .NOT. REPORT_HEADER_OPEN) THEN
               IF(WVPA()) THEN
                  REPORTING_UNIT =
     +                         WVPA_DEBT_FILE_RPT_HEADER(REPORTING_REC)
                  MTG_REPORTING_UNIT =
     +               WVPA_MORTGAGE_SUMMARY_RPT_HEADR(MTG_REPORTING_REC)
               ELSE
                  REPORT_HEADER_OPEN = .NOT. ANNUAL_INFO_ACTIVE
     +                                       .AND. MONTHLY_MIDAS_ACTIVE
                  REPORTING_UNIT = DEBT_FILE_RPT_HEADER(REPORTING_REC,
     +                                              REPORT_HEADER_OPEN)
               ENDIF
               REPORT_HEADER_OPEN = .TRUE.
            ENDIF
            INDEX_NAME = DESC//LEFT_JUSTIFY_I2_IN_STR(INT2(IREC))
            MO_START = 13
            IF(MONTHLY_MIDAS_ACTIVE) MO_START = 1
            IF(ANNUAL_INFO_ACTIVE) MO_START = 13
            OPENING_BALANCE = BAL(12,0)
     +                        + PREMIUM_BAL(12,0)
            DO YR = 1, FINANCIAL_SIMULATION_YEARS

               CURRENT_YR = get_BASE_YEAR() + YR

               DO MO1 = MO_START, 13
                  IF(MO1 == 13) THEN
                     MO= 0
                     OPENING_BALANCE = BAL(12,YR-1)
     +                                 + PREMIUM_BAL(12,YR-1)
                     IF(BAL(12,YR-1) + BAL(12,YR) /= 0.) THEN
                        RPT_RATE = 200.*INTEREST(0,YR)/
     +                                      (BAL(12,YR-1) + BAL(12,YR))
                     ELSE
                        RPT_RATE = 0.
                     ENDIF
                  ELSE
                     MO = MO1
                     RPT_RATE = 100.*RATE(MO,YR)*DAYS_IN_YEAR(YR)

                  ENDIF

                  LTD_INTEREST = 0.
                  PS_DIVIDENDS = 0.
                  MIPS_DIVIDENDS = 0.
                  P_INVESTMENT_INCOME = 0.
                  P_DIVIDEND_INCOME = 0.
                  P_TAX_FREE_70_DIVIDEND_INCOME= 0.
                  LTD_INTEREST_CASH = 0.
                  PS_DIVIDENDS_CASH = 0.
                  MIPS_DIVIDENDS_CASH = 0.
                  PS_PREMIUM_AMORT = 0.
                  PS_ISSUE_EXPENSE_AMORT = 0.
                  MIPS_PREMIUM_AMORT = 0.
                  MIPS_ISSUE_EXPENSE_AMORT = 0.
                  LTD_PREMIUM_AMORT = 0.
                  LTD_ISSUE_EXPENSE_AMORT = 0.
                  LTD_BOOK_INTEREST = 0.
                  PS_BOOK_DIVIDENDS = 0.
                  MIPS_BOOK_DIVIDENDS = 0.

                  IF(MONTHLY_MIDAS_ACTIVE) THEN
                     PAYMENT_IN_CASH = CASH_PAYMENTS(MO,YR)
                  ELSE
                     PAYMENT_IN_CASH = INTEREST(MO,YR)
                  ENDIF

                  INC_STATEMTENT_PAYMENT = INTEREST(MO,YR)
     +                                     + ISSUE_EXPENSE_AMORT(MO,YR)
     +                                     - PREMIUM_AMORT(MO,YR)

                  IF(TYPE == INVESTMENT) THEN
                     IF(INVESTMENT_TYPE == '7') THEN
                        P_TAX_FREE_70_DIVIDEND_INCOME = INTEREST(MO,YR)
                     ELSEIF(INVESTMENT_TYPE == 'D') THEN
                        P_DIVIDEND_INCOME = INTEREST(MO,YR)
                     ELSE
                        P_INVESTMENT_INCOME = INTEREST(MO,YR)
                     ENDIF
                  ELSE
                     IF(TYPE == PFS) THEN
                        PS_BOOK_DIVIDENDS = INTEREST(MO,YR)
                        PS_DIVIDENDS = INC_STATEMTENT_PAYMENT
                        PS_DIVIDENDS_CASH = PAYMENT_IN_CASH
                        PS_PREMIUM_AMORT = PREMIUM_AMORT(MO,YR)
                        PS_ISSUE_EXPENSE_AMORT =
     +                                       ISSUE_EXPENSE_AMORT(MO,YR)
                     ELSEIF(TYPE == MIPS) THEN
                        MIPS_BOOK_DIVIDENDS = INTEREST(MO,YR)
                        MIPS_DIVIDENDS = INC_STATEMTENT_PAYMENT
                        MIPS_DIVIDENDS_CASH = PAYMENT_IN_CASH
                        MIPS_PREMIUM_AMORT = PREMIUM_AMORT(MO,YR)
                        MIPS_ISSUE_EXPENSE_AMORT =
     +                                       ISSUE_EXPENSE_AMORT(MO,YR)
                     ELSE
                        LTD_BOOK_INTEREST = INTEREST(MO,YR)
                        LTD_INTEREST = INC_STATEMTENT_PAYMENT
                        LTD_INTEREST_CASH = PAYMENT_IN_CASH
                        LTD_PREMIUM_AMORT = PREMIUM_AMORT(MO,YR)
                        LTD_ISSUE_EXPENSE_AMORT =
     +                                       ISSUE_EXPENSE_AMORT(MO,YR)
                     ENDIF
                  ENDIF

                     CLOSING_BALANCE = MAX(.0000001,
     +                             ABS(OPENING_BALANCE
     +                                 - SINKING_FUND(MO,YR)
     +                                 - RET(MO,YR)
     +                                 + ISS(MO,YR)
     +                                 - PREMIUM_AMORT(MO,YR)
     +                                 + PREMIUM_DISCOUNT_EXP(MO,YR)))

                  IF(WVPA()) THEN
                     IF(TYPE == MTG .OR. TYPE == LTD) THEN
                        WRITE(REPORTING_UNIT,REC=REPORTING_REC)
     +                               PRT_ENDPOINT(),
     +                               FLOAT(YR+get_BASE_YEAR()),
     +                               INDEX_NAME,
     +                               SHORT_MONTH_NAMES(MO),
     +                               CLOSING_BALANCE,
     +                               RET(MO,YR)
     +                                + SINKING_FUND(MO,YR),
     +                               LTD_INTEREST,
     +                               LTD_INTEREST_CASH,
     +                               RET(MO,YR)
     +                                + LTD_INTEREST_CASH
     +                                + SINKING_FUND(MO,YR),
     +                               LTD_CURRENT_PORTION(MO,YR)
                        TOTAL_REPORT_VARS(MO,YR,Principal Balance) =
     +                       TOTAL_REPORT_VARS(MO,YR,Principal Balance)
     +                        + CLOSING_BALANCE
                        TOTAL_REPORT_VARS(MO,YR,Principal Payment) =
     +                       TOTAL_REPORT_VARS(MO,YR,Principal Payment)
     +                        + RET(MO,YR)
     +                        + SINKING_FUND(MO,YR)
                       TOTAL_REPORT_VARS(MO,YR,Book Interest Expense) =
     +                   TOTAL_REPORT_VARS(MO,YR,Book Interest Expense)
     +                    + LTD_INTEREST
                       TOTAL_REPORT_VARS(MO,YR,Cash Interest Expense) =
     +                   TOTAL_REPORT_VARS(MO,YR,Cash Interest Expense)
     +                    + LTD_INTEREST_CASH
                        TOTAL_REPORT_VARS(MO,YR,Cash PnI Payment) =
     +                        TOTAL_REPORT_VARS(MO,YR,Cash PnI Payment)
     +                         + RET(MO,YR)
     +                         + LTD_INTEREST_CASH
     +                         + SINKING_FUND(MO,YR)

                        REPORTING_REC = REPORTING_REC + 1

! MORTGAGE DEBT BY TYPE

                        IF(WVPA_REPORT_CATEGORY(1:1) /= 'N') THEN
                           WRITE(MTG_REPORTING_UNIT,
     +                              REC=MTG_REPORTING_REC)
     +     PRT_ENDPOINT(),
     +     FLOAT(YR+get_BASE_YEAR()),
     +     INDEX_NAME,
     +     WVPA_REPORT_CATEGORY,
     +     SHORT_MONTH_NAMES(MO),
     +     CLOSING_BALANCE,
     +     RET(MO,YR)
     +     + SINKING_FUND(MO,YR),
     +     LTD_INTEREST,
     +     LTD_INTEREST_CASH,
     +     RET(MO,YR)
     +     + LTD_INTEREST_CASH
     +     + SINKING_FUND(MO,YR)
                           MTG_REPORTING_REC = MTG_REPORTING_REC + 1
                        ENDIF
                     ENDIF
                  ELSE
                     IF(.NOT. ANNUAL_INFO_ACTIVE
     +                                 .AND. MONTHLY_MIDAS_ACTIVE) THEN
                        WRITE(REPORTING_UNIT,REC=REPORTING_REC)
     +                               PRT_ENDPOINT(),
     +                               FLOAT(YR+get_BASE_YEAR()),
     +                               INDEX_NAME,
     +                               SHORT_MONTH_NAMES(MO),
     +                               OPENING_BALANCE,
     +                               SINKING_FUND(MO,YR),
     +                               RET(MO,YR),
     +                               LTD_INTEREST,
     +                               PREMIUM_AMORT(MO,YR),
     +                               ISSUE_EXPENSE_AMORT(MO,YR),
     +                               ISS(MO,YR),
     +                               PS_DIVIDENDS,
     +                               MIPS_DIVIDENDS,
     +                               CLOSING_BALANCE,
     +                               P_INVESTMENT_INCOME,
     +                               P_DIVIDEND_INCOME,
     +                               P_TAX_FREE_70_DIVIDEND_INCOME,
     +                               LTD_INTEREST_CASH,
     +                               PS_DIVIDENDS_CASH,
     +                               MIPS_DIVIDENDS_CASH,
     +                               PS_PREMIUM_AMORT,
     +                               PS_ISSUE_EXPENSE_AMORT,
     +                               MIPS_PREMIUM_AMORT,
     +                               MIPS_ISSUE_EXPENSE_AMORT,
     +                               LTD_PREMIUM_AMORT,
     +                               LTD_ISSUE_EXPENSE_AMORT,
     +                               ISSUE_TAX_EXPENSE(MO,YR),
     +                               PREMIUM_DISCOUNT_EXP(MO,YR),
     +                               LTD_BOOK_INTEREST,
     +                               PS_BOOK_DIVIDENDS,
     +                               MIPS_BOOK_DIVIDENDS,
     +                               ISS(MO,YR)
     +                               + PREMIUM_DISCOUNT_EXP(MO,YR)
     +                               - ISSUE_TAX_EXPENSE(MO,YR),
     +                               PREMIUM_BAL(MO,YR),
     +                               LTD_CURRENT_PORTION(MO,YR),
     +                               RPT_RATE ! 30
                     ELSE
                        WRITE(REPORTING_UNIT,REC=REPORTING_REC)
     +                               PRT_ENDPOINT(),
     +                               INDEX_NAME,
     +                               FLOAT(YR+get_BASE_YEAR()),
     +                               OPENING_BALANCE,
     +                               SINKING_FUND(MO,YR),
     +                               RET(MO,YR),
     +                               LTD_INTEREST,
     +                               PREMIUM_AMORT(MO,YR),
     +                               ISSUE_EXPENSE_AMORT(MO,YR),
     +                               ISS(MO,YR),
     +                               PS_DIVIDENDS,
     +                               MIPS_DIVIDENDS,
     +                               CLOSING_BALANCE,
     +                               P_INVESTMENT_INCOME,
     +                               P_DIVIDEND_INCOME,
     +                               P_TAX_FREE_70_DIVIDEND_INCOME,
     +                               LTD_INTEREST_CASH,
     +                               PS_DIVIDENDS_CASH,
     +                               MIPS_DIVIDENDS_CASH,
     +                               PS_PREMIUM_AMORT,
     +                               PS_ISSUE_EXPENSE_AMORT,
     +                               MIPS_PREMIUM_AMORT,
     +                               MIPS_ISSUE_EXPENSE_AMORT,
     +                               LTD_PREMIUM_AMORT,
     +                               LTD_ISSUE_EXPENSE_AMORT,
     +                               ISSUE_TAX_EXPENSE(MO,YR),
     +                               PREMIUM_DISCOUNT_EXP(MO,YR),
     +                               LTD_BOOK_INTEREST,
     +                               PS_BOOK_DIVIDENDS,  ! 25
     +                               MIPS_BOOK_DIVIDENDS,
     +                               ISS(MO,YR)
     +                               + PREMIUM_DISCOUNT_EXP(MO,YR)
     +                               - ISSUE_TAX_EXPENSE(MO,YR),
     +                               PREMIUM_BAL(MO,YR),
     +                               LTD_CURRENT_PORTION(MO,YR),
     +                               RPT_RATE ! 30
                     ENDIF
                     REPORTING_REC = REPORTING_REC + 1
                  ENDIF
                  OPENING_BALANCE = CLOSING_BALANCE
               ENDDO
            ENDDO
         ENDIF

!$endif
      MO = 0

         IF(INTRA_COMPANY == 'Y') THEN
            ASSET_ALLOCATOR = 1.
            IF(TYPE == NOTE_RECEIVABLE .OR. TYPE == NOTE_PAYABLE
     +            .or. (TYPE == LTD .AND. .NOT. FirstEnergy())! 8/17/04
     +     .or. (TYPE == MTG .AND. .NOT. FirstEnergy())) THEN ! 1/12/05
               VOID_LOGICAL = ASSET_CLASS_LINKAGES(INTRA_CLASS_ID,
     +                                            ASSET_CLASS_LIST,
     +                                            INT2(0),
     +                                           ASSET_ALLOCATION_LIST)
               CLASS_POINTER = 1
               DO
                  INTRA_CLASS_ID = ASSET_CLASS_LIST(CLASS_POINTER)
                  CALL CHECK_IF_CLASS_DEFINED(INTRA_CLASS_ID)
                  INTRA_CLASS_ID = INTRA_CLASS_ID + 1
                  IF(INTRA_CLASS_ID > 0) INTRA_CLASS_ID =
     +                              ASSET_CLASS_POINTER(INTRA_CLASS_ID)
                  DO I = 0, FINANCIAL_SIMULATION_YEARS
                     DO MO = 0, 12
                        IF(TYPE == NOTE_RECEIVABLE) THEN
                          NOTES_PAYABLE(MO,I,INTRA_CLASS_ID,Additions)=
     +                               NOTES_PAYABLE(MO,I,INTRA_CLASS_ID,
     +                                                       Additions)
     +                                + ASSET_ALLOCATOR * ISS(MO,I)
                           NOTES_PAYABLE(MO,I,INTRA_CLASS_ID,
     +                                                    Reductions) =
     +                        NOTES_PAYABLE(MO,I,INTRA_CLASS_ID,
     +                                                      Reductions)
     +                        + ASSET_ALLOCATOR *
     +                                 (RET(MO,I) + SINKING_FUND(MO,I))
                           NOTES_PAYABLE(MO,I,INTRA_CLASS_ID,

     +                                              Interest_Payments) =
     +                                NOTES_PAYABLE(MO,I,INTRA_CLASS_ID,
     +                                                Interest_Payments)
     +                                + ASSET_ALLOCATOR * INTEREST(MO,I)
                           NOTES_PAYABLE(MO,I,INTRA_CLASS_ID,
     +                                         Cash_Interest_Payments) =
     +                           NOTES_PAYABLE(MO,I,INTRA_CLASS_ID,
     +                                           Cash_Interest_Payments)
     +                           + ASSET_ALLOCATOR * CASH_PAYMENTS(MO,I)

                        ELSEIF(TYPE == NOTE_PAYABLE .or.
     +                                         TYPE ==LTD .or.
     +                                       TYPE ==MTG) THEN ! 8/17/04
                           NOTES_RECEIVABLE(MO,I,INTRA_CLASS_ID,
     +                                                     Additions) =
     +                           NOTES_RECEIVABLE(MO,I,INTRA_CLASS_ID,
     +                                                       Additions)
     +                           + ASSET_ALLOCATOR * ISS(MO,I)
                           NOTES_RECEIVABLE(MO,I,INTRA_CLASS_ID,
     +                                                    Reductions) =
     +                        NOTES_RECEIVABLE(MO,I,INTRA_CLASS_ID,
     +                                                      Reductions)
     +                        + ASSET_ALLOCATOR *
     +                                 (RET(MO,I) + SINKING_FUND(MO,I))
                           NOTES_RECEIVABLE(MO,I,INTRA_CLASS_ID,
     +                                              Interest_Earnings) =
     +                             NOTES_RECEIVABLE(MO,I,INTRA_CLASS_ID,
     +                                                Interest_Earnings)
     +                             + ASSET_ALLOCATOR * INTEREST(MO,I)
                           NOTES_RECEIVABLE(MO,I,INTRA_CLASS_ID,
     +                                Intra_Company_Interest_Earnings) =
     +                             NOTES_RECEIVABLE(MO,I,INTRA_CLASS_ID,
     +                                  Intra_Company_Interest_Earnings)
     +                             + ASSET_ALLOCATOR * INTEREST(MO,I)
                           NOTES_RECEIVABLE(MO,I,INTRA_CLASS_ID,
     +                                         Cash_Interest_Earnings) =
     +                           NOTES_RECEIVABLE(MO,I,INTRA_CLASS_ID,
     +                                           Cash_Interest_Earnings)
     +                           + ASSET_ALLOCATOR * CASH_PAYMENTS(MO,I)

                        ENDIF
                        IF(CLASS_POINTER == 1) THEN
                           IF(TYPE == LTD .OR. TYPE == MTG) THEN
                              LTD_PREM_BAL(MO,I,-1) =
     +                                            LTD_PREM_BAL(MO,I,-1)
     +                                             + PREMIUM_BAL(MO,I)
                              LTD_PREM_AMORT(MO,I,-1) =
     +                                          LTD_PREM_AMORT(MO,I,-1)
     +                                           + PREMIUM_AMORT(MO,I)

! TRACK THE ELIMINATION

                              LTD_ISSUE_EXP_BAL(MO,I,-1) =
     +                                       LTD_ISSUE_EXP_BAL(MO,I,-1)
     +                                        + ISSUE_EXPENSE_BAL(MO,I)
     +                                        + ISSUE_TAX_EXPENSE(MO,I)
                              LTD_ISSUE_EXP_AMORT(MO,I,-1) =
     +                                     LTD_ISSUE_EXP_AMORT(MO,I,-1)
     +                                      + ISSUE_EXPENSE_AMORT(MO,I)

                              SFLTD(MO,I,-1) = SFLTD(MO,I,-1)
     +                                         + SINKING_FUND(MO,I)
                              RETLTD(MO,I,-1) = RETLTD(MO,I,-1)
     +                                          + RET(MO,I)
                              CURRENT_PORTION_OF_LTD(MO,I,-1) =
     +                                  CURRENT_PORTION_OF_LTD(MO,I,-1)
     +                                   + LTD_CURRENT_PORTION(MO,I)
                              INTLTD(MO,I,-1) = INTLTD(MO,I,-1)
     +                                          + INTEREST(MO,I)
                              ISSLTD(MO,I,-1) = ISSLTD(MO,I,-1)
     +                                          + ISS(MO,I)
                              LTD_PS_ISSUE_TAX_EXPENSE(MO,I,-1) =
     +                                LTD_PS_ISSUE_TAX_EXPENSE(MO,I,-1)
     +                                 + ISSUE_TAX_EXPENSE(MO,I)
                              LTD_CASH_INTEREST(MO,I,-1) =
     +                                       LTD_CASH_INTEREST(MO,I,-1)
     +                                        + CASH_PAYMENTS(MO,I)
                              LTD_NEW_ISSUE_PREM_DISC_AMOUNT(MO,I,-1) =
     +                          LTD_NEW_ISSUE_PREM_DISC_AMOUNT(MO,I,-1)
     +                           + PREMIUM_DISCOUNT_EXP(MO,I)
                           ELSE
                              NOTES_PAYABLE(MO,I,-1,Additions) =
     +                                 NOTES_PAYABLE(MO,I,-1,Additions)
     +                                  + ASSET_ALLOCATOR * ISS(MO,I)
                              NOTES_PAYABLE(MO,I,-1,Reductions) =
     +                                NOTES_PAYABLE(MO,I,-1,Reductions)
     +                                 + ASSET_ALLOCATOR *
     +                                  (RET(MO,I) + SINKING_FUND(MO,I))
                              NOTES_PAYABLE(MO,I,-1,Interest_Payments) =
     +                          NOTES_PAYABLE(MO,I,-1,Interest_Payments)
     +                          + ASSET_ALLOCATOR * INTEREST(MO,I)
                              NOTES_PAYABLE(MO,I,-1,
     +                                         Cash_Interest_Payments) =
     +                           NOTES_PAYABLE(MO,I,-1,
     +                                           Cash_Interest_Payments)
     +                           + ASSET_ALLOCATOR * CASH_PAYMENTS(MO,I)

                           ENDIF
                           NOTES_RECEIVABLE(MO,I,-1,Additions) =
     +                              NOTES_RECEIVABLE(MO,I,-1,Additions)
     +                               + ASSET_ALLOCATOR * ISS(MO,I)
                           NOTES_RECEIVABLE(MO,I,-1,Reductions) =
     +                             NOTES_RECEIVABLE(MO,I,-1,Reductions)
     +                              + ASSET_ALLOCATOR *
     +                                  (RET(MO,I) + SINKING_FUND(MO,I))
                           NOTES_RECEIVABLE(MO,I,-1,Interest_Earnings) =
     +                              NOTES_RECEIVABLE(MO,I,-1,
     +                                                Interest_Earnings)
     +                              + ASSET_ALLOCATOR * INTEREST(MO,I)
                           NOTES_RECEIVABLE(MO,I,-1,
     +                                Intra_Company_Interest_Earnings) =
     +                              NOTES_RECEIVABLE(MO,I,-1,
     +                                  Intra_Company_Interest_Earnings)
     +                              + ASSET_ALLOCATOR * INTEREST(MO,I)
                           NOTES_RECEIVABLE(MO,I,-1,
     +                                         Cash_Interest_Earnings) =
     +                           NOTES_RECEIVABLE(MO,I,-1,
     +                                           Cash_Interest_Earnings)
     +                           + ASSET_ALLOCATOR * CASH_PAYMENTS(MO,I)


                        ENDIF
                     ENDDO ! MONTH
                  ENDDO ! YEAR (globecom_year)
                  CLASS_POINTER = CLASS_POINTER + 1
                  IF(CLASS_POINTER > AVAIL_DATA_YEARS) EXIT
                  IF(ASSET_CLASS_LIST(CLASS_POINTER) == -99) EXIT
               ENDDO ! CLASS ID
            ELSE
               INTRA_CLASS_ID = INTRA_CLASS_ID + 1
               IF(INTRA_CLASS_ID > 0) INTRA_CLASS_ID =
     +                              ASSET_CLASS_POINTER(INTRA_CLASS_ID)
               DO I = 0, FINANCIAL_SIMULATION_YEARS
                  IF(TYPE == PFS .OR. TYPE == MIPS) THEN
                     AINTPS(I,-1) = AINTPS(I,-1) +  ANNINT(I)
                     MIPS_INCOME_TAX_DEDUCTION(I,-1) =
     +                                MIPS_INCOME_TAX_DEDUCTION(I,-1) +
     +                                         ISSUE_EXPENSE_AMORT(0,I)
                     DO MO = 0, 12

                        OTHER_INCOME(MO,I,INTRA_CLASS_ID)=INTEREST(MO,I)
     +                               + OTHER_INCOME(MO,I,INTRA_CLASS_ID)
                        INVESTMENT_IN_AFILLIATES(MO,I,INTRA_CLASS_ID) =
     +                     INVESTMENT_IN_AFILLIATES(MO,I,INTRA_CLASS_ID)
     +                     + ISS(MO,I) - RET(MO,I) - SINKING_FUND(MO,I)

! TRACK THE ELIMINATION

                        PS_ISSUE_EXP_BAL(MO,I,-1) =
     +                                        PS_ISSUE_EXP_BAL(MO,I,-1)
     +                                        + ISSUE_EXPENSE_BAL(MO,I)
     +                                        + ISSUE_TAX_EXPENSE(MO,I)
                        PS_ISSUE_EXP_AMORT(MO,I,-1) =
     +                                      PS_ISSUE_EXP_AMORT(MO,I,-1)
     +                                      + ISSUE_EXPENSE_AMORT(MO,I)
                        PS_PREM_BAL(MO,I,-1) = PS_PREM_BAL(MO,I,-1)
     +                                      + PREMIUM_BAL(MO,I)
                        PS_PREM_AMORT(MO,I,-1) = PS_PREM_AMORT(MO,I,-1)
     +                                        + PREMIUM_AMORT(MO,I)
                        OTHER_INCOME(MO,I,-1) = OTHER_INCOME(MO,I,-1) +
     +                                                   INTEREST(MO,I)
                        INVESTMENT_IN_AFILLIATES(MO,I,-1) =
     +                              INVESTMENT_IN_AFILLIATES(MO,I,-1) +
     +                               ISS(MO,I) - RET(MO,I) -
     +                               SINKING_FUND(MO,I)
                       SFPS(MO,I,-1) = SFPS(MO,I,-1)+SINKING_FUND(MO,I)
                        RETPS(MO,I,-1) = RETPS(MO,I,-1) + RET(MO,I)
                       INTPS(MO,I,-1) = INTPS(MO,I,-1) + INTEREST(MO,I)
                        ISSPS(MO,I,-1) = ISSPS(MO,I,-1) + ISS(MO,I)
                        LTD_PS_ISSUE_TAX_EXPENSE(MO,I,-1) =
     +                                LTD_PS_ISSUE_TAX_EXPENSE(MO,I,-1)
     +                                 + ISSUE_TAX_EXPENSE(MO,I)
                     ENDDO
                  ELSEIF(TYPE == COMMON) THEN
                  ELSE
                     AINTDB(I,-1) = AINTDB(I,-1) +  ANNINT(I)
                     DO MO = 0, 12
                        INTEREST_INCOME(MO,I,INTRA_CLASS_ID) =
     +                             INTEREST_INCOME(MO,I,INTRA_CLASS_ID)
     +                              + ASSET_ALLOCATOR * INTEREST(MO,I)
                        NOTES_RECEIVABLE(MO,I,INTRA_CLASS_ID,
     +                                Intra_Company_Interest_Earnings) =
     +                             NOTES_RECEIVABLE(MO,I,INTRA_CLASS_ID,
     +                                  Intra_Company_Interest_Earnings)

     +                             + ASSET_ALLOCATOR * INTEREST(MO,I)
                        LOANS_TO_AFILLIATES(MO,I,INTRA_CLASS_ID) =
     +                       LOANS_TO_AFILLIATES(MO,I,INTRA_CLASS_ID) +
     +                                          ISS(MO,I) - RET(MO,I) -
     +                                          SINKING_FUND(MO,I)
                        LTD_PREM_BAL(MO,I,-1) = LTD_PREM_BAL(MO,I,-1)
     +                                          + PREMIUM_BAL(MO,I)
                        LTD_PREM_AMORT(MO,I,-1)=LTD_PREM_AMORT(MO,I,-1)
     +                                          + PREMIUM_AMORT(MO,I)

! TRACK THE ELIMINATION

                        LTD_ISSUE_EXP_BAL(MO,I,-1) =
     +                                       LTD_ISSUE_EXP_BAL(MO,I,-1)
     +                                        + ISSUE_EXPENSE_BAL(MO,I)
     +                                        + ISSUE_TAX_EXPENSE(MO,I)
                        LTD_ISSUE_EXP_AMORT(MO,I,-1) =
     +                                     LTD_ISSUE_EXP_AMORT(MO,I,-1)
     +                                      + ISSUE_EXPENSE_AMORT(MO,I)
                        INTEREST_INCOME(MO,I,-1) =
     +                        INTEREST_INCOME(MO,I,-1) + INTEREST(MO,I)
                        LOANS_TO_AFILLIATES(MO,I,-1) =
     +                                   LOANS_TO_AFILLIATES(MO,I,-1) +
     +                                    ISS(MO,I) - RET(MO,I) -
     +                                       SINKING_FUND(MO,I)
                       SFLTD(MO,I,-1)=SFLTD(MO,I,-1)+SINKING_FUND(MO,I)
                        RETLTD(MO,I,-1) = RETLTD(MO,I,-1) + RET(MO,I)
                        CURRENT_PORTION_OF_LTD(MO,I,-1) =
     +                                  CURRENT_PORTION_OF_LTD(MO,I,-1)
     +                                   + LTD_CURRENT_PORTION(MO,I)
                       INTLTD(MO,I,-1)=INTLTD(MO,I,-1) + INTEREST(MO,I)
                        ISSLTD(MO,I,-1) = ISSLTD(MO,I,-1) + ISS(MO,I)
                        LTD_PS_ISSUE_TAX_EXPENSE(MO,I,-1) =
     +                                LTD_PS_ISSUE_TAX_EXPENSE(MO,I,-1)
     +                                 + ISSUE_TAX_EXPENSE(MO,I)
                        LTD_CASH_INTEREST(MO,I,-1) =
     +                                       LTD_CASH_INTEREST(MO,I,-1)
     +                                        + CASH_PAYMENTS(MO,I)
                        LTD_NEW_ISSUE_PREM_DISC_AMOUNT(MO,I,-1) =
     +                          LTD_NEW_ISSUE_PREM_DISC_AMOUNT(MO,I,-1)
     +                           + PREMIUM_DISCOUNT_EXP(MO,I)
                     ENDDO
                  ENDIF
               ENDDO
            ENDIF
         ENDIF ! INTRA COMPANY

! ALLOCATE TO TOTAL COMPANY AND ASSET CLASSES

         IF(TYPE == COMMON) THEN
            VOID_LOGICAL = RETURN_ASSET_CLASS_LISTS(ASSET_CLASS,
     +                                            ASSET_CLASS_LIST,
     +                                            ASSET_CLASS_VECTOR,
     +                                           ASSET_ALLOCATION_LIST)
         ELSE
            VOID_LOGICAL = ASSET_CLASS_LINKAGES(ASSET_CLASS,
     +                                          ASSET_CLASS_LIST,
     +                                          ASSET_CLASS_VECTOR,
     +                                          ASSET_ALLOCATION_LIST)
         ENDIF

         CLASS_POINTER = 1
         DO
            ASSET_CLASS = ASSET_CLASS_LIST(CLASS_POINTER)
            CALL CHECK_IF_CLASS_DEFINED(ASSET_CLASS)
            ASSET_CLASS = ASSET_CLASS + 1
            IF(ASSET_CLASS > 0) ASSET_CLASS =
     +                                 ASSET_CLASS_POINTER(ASSET_CLASS)
            ASSET_CLASS_VECTOR = ASSET_ALLOCATION_LIST(CLASS_POINTER)
            IF(ASSET_CLASS_VECTOR < 0.) THEN
               CALL GET_ASSET_VAR(ABS(ASSET_CLASS_VECTOR),DUMMY_TYPE,
     +                                     VECTOR_CLASS_ALLOCATIONS(1))
               CALL RETURN_BASE_YEAR_VECTOR_VALUES(
     +                                     VECTOR_CLASS_ALLOCATIONS(0))
            ENDIF
            ASSET_ALLOCATOR = ASSET_ALLOCATION_LIST(CLASS_POINTER)/100.
            DO I = 0, FINANCIAL_SIMULATION_YEARS
               IF(ASSET_CLASS_VECTOR < 0 .AND.
     +                                      I <= AVAIL_DATA_YEARS) THEN
                  ASSET_ALLOCATOR = VECTOR_CLASS_ALLOCATIONS(I)/100.
               ENDIF
               IF(TYPE == INVESTMENT) THEN
                  DO MO = 0, 12
                     IF(INVESTMENT_TYPE == '7') THEN
                        INVESTMENT_INCOME(MO,I,ASSET_CLASS,Dividend_70)=
     +                   INVESTMENT_INCOME(MO,I,ASSET_CLASS,Dividend_70)
     +                   + ASSET_ALLOCATOR * INTEREST(MO,I)
                        INVESTMENT_INCOME(MO,I,ASSET_CLASS,
     +                                         Cash_Dividend_Earnings) =
     +                      INVESTMENT_INCOME(MO,I,ASSET_CLASS,
     +                                           Cash_Dividend_Earnings)

     +                      + ASSET_ALLOCATOR * CASH_PAYMENTS(MO,I)
                     ELSEIF(INVESTMENT_TYPE == 'D') THEN
                        INVESTMENT_INCOME(MO,I,ASSET_CLASS,Dividend) =
     +                     INVESTMENT_INCOME(MO,I,ASSET_CLASS,Dividend)
     +                      + ASSET_ALLOCATOR * INTEREST(MO,I)
                        INVESTMENT_INCOME(MO,I,ASSET_CLASS,

     +                                         Cash_Dividend_Earnings) =
     +                      INVESTMENT_INCOME(MO,I,ASSET_CLASS,
     +                                           Cash_Dividend_Earnings)
     +                      + ASSET_ALLOCATOR * CASH_PAYMENTS(MO,I)
                     ELSE
                        INVESTMENT_INCOME(MO,I,ASSET_CLASS,
     +                                              Interest_Earnings) =
     +                               INVESTMENT_INCOME(MO,I,ASSET_CLASS,
     +                                                Interest_Earnings)
     +                               + ASSET_ALLOCATOR * INTEREST(MO,I)
                        INVESTMENT_INCOME(MO,I,ASSET_CLASS,
     +                                         Cash_Interest_Earnings) =
     +                           INVESTMENT_INCOME(MO,I,ASSET_CLASS,
     +                                           Cash_Interest_Earnings)
     +                           + ASSET_ALLOCATOR * CASH_PAYMENTS(MO,I)


                     ENDIF
                     INVESTMENT_INCOME(MO,I,ASSET_CLASS,Additions) =
     +                    INVESTMENT_INCOME(MO,I,ASSET_CLASS,Additions)
     +                     + ASSET_ALLOCATOR * ISS(MO,I)
                     INVESTMENT_INCOME(MO,I,ASSET_CLASS,Reductions) =
     +                   INVESTMENT_INCOME(MO,I,ASSET_CLASS,Reductions)
     +                    + ASSET_ALLOCATOR *
     +                                 (RET(MO,I) + SINKING_FUND(MO,I))
                  ENDDO
               ELSEIF(TYPE == NOTE_RECEIVABLE) THEN
                  DO MO = 0, 12
                     NOTES_RECEIVABLE(MO,I,ASSET_CLASS,Additions) =
     +                     NOTES_RECEIVABLE(MO,I,ASSET_CLASS,Additions)
     +                      + ASSET_ALLOCATOR * ISS(MO,I)
                     NOTES_RECEIVABLE(MO,I,ASSET_CLASS,Reductions) =
     +                    NOTES_RECEIVABLE(MO,I,ASSET_CLASS,Reductions)
     +                     + ASSET_ALLOCATOR *
     +                                 (RET(MO,I) + SINKING_FUND(MO,I))
                     NOTES_RECEIVABLE(MO,I,ASSET_CLASS,
     +                                              Interest_Earnings) =
     +                             NOTES_RECEIVABLE(MO,I,ASSET_CLASS,
     +                                                Interest_Earnings)
     +                             + ASSET_ALLOCATOR * INTEREST(MO,I)
                     NOTES_RECEIVABLE(MO,I,ASSET_CLASS,
     +                                         Cash_Interest_Earnings) =
     +                           NOTES_RECEIVABLE(MO,I,ASSET_CLASS,
     +                                           Cash_Interest_Earnings)
     +                           + ASSET_ALLOCATOR * CASH_PAYMENTS(MO,I)
                     IF(INTRA_COMPANY == 'Y') THEN
                        NOTES_RECEIVABLE(MO,I,ASSET_CLASS,
     +                                Intra_Company_Interest_Earnings) =
     +                             NOTES_RECEIVABLE(MO,I,ASSET_CLASS,
     +                                  Intra_Company_Interest_Earnings)

     +                             + ASSET_ALLOCATOR * INTEREST(MO,I)
                     ENDIF
                  ENDDO
               ELSEIF(TYPE == NOTE_PAYABLE) THEN
                  DO MO = 0, 12
                     NOTES_PAYABLE(MO,I,ASSET_CLASS,Additions) =
     +                        NOTES_PAYABLE(MO,I,ASSET_CLASS,Additions)
     +                         + ASSET_ALLOCATOR * ISS(MO,I)
                     NOTES_PAYABLE(MO,I,ASSET_CLASS,Reductions) =
     +                       NOTES_PAYABLE(MO,I,ASSET_CLASS,Reductions)
     +                        + ASSET_ALLOCATOR *
     +                                  (RET(MO,I) + SINKING_FUND(MO,I))
                     NOTES_PAYABLE(MO,I,ASSET_CLASS,Interest_Payments) =
     +                 NOTES_PAYABLE(MO,I,ASSET_CLASS,Interest_Payments)
     +                 + ASSET_ALLOCATOR * INTEREST(MO,I)

                     NOTES_PAYABLE(MO,I,ASSET_CLASS,
     +                                         Cash_Interest_Payments) =
     +                           NOTES_PAYABLE(MO,I,ASSET_CLASS,
     +                                           Cash_Interest_Payments)
     +                           + ASSET_ALLOCATOR * CASH_PAYMENTS(MO,I)

                  ENDDO
               ELSEIF(TYPE == PFS .OR. TYPE == MIPS) THEN
                  AINTPS(I,ASSET_CLASS) = AINTPS(I,ASSET_CLASS) +
     +                                      ASSET_ALLOCATOR * ANNINT(I)
                  IF(TYPE == MIPS)
     +                    MIPS_INCOME_TAX_DEDUCTION(I,ASSET_CLASS) =
     +                       MIPS_INCOME_TAX_DEDUCTION(I,ASSET_CLASS) +
     +                                  ASSET_ALLOCATOR * INTEREST(0,I)
                  DO MO = 0, 12
                     PS_ISSUE_EXP_BAL(MO,I,ASSET_CLASS) =
     +                         PS_ISSUE_EXP_BAL(MO,I,ASSET_CLASS) +
     +                        ASSET_ALLOCATOR * ISSUE_EXPENSE_BAL(MO,I)
                     PS_ISSUE_EXP_AMORT(MO,I,ASSET_CLASS) =
     +                       PS_ISSUE_EXP_AMORT(MO,I,ASSET_CLASS) +
     +                      ASSET_ALLOCATOR * ISSUE_EXPENSE_AMORT(MO,I)
                     PS_PREM_BAL(MO,I,ASSET_CLASS) =
     +                             PS_PREM_BAL(MO,I,ASSET_CLASS)
     +                            + ASSET_ALLOCATOR * PREMIUM_BAL(MO,I)
                     PS_PREM_AMORT(MO,I,ASSET_CLASS) =
     +                           PS_PREM_AMORT(MO,I,ASSET_CLASS)
     +                          + ASSET_ALLOCATOR * PREMIUM_AMORT(MO,I)
                     SFPS(MO,I,ASSET_CLASS) = SFPS(MO,I,ASSET_CLASS) +
     +                             ASSET_ALLOCATOR * SINKING_FUND(MO,I)
                    RETPS(MO,I,ASSET_CLASS) = RETPS(MO,I,ASSET_CLASS) +
     +                                      ASSET_ALLOCATOR * RET(MO,I)
                    INTPS(MO,I,ASSET_CLASS) = INTPS(MO,I,ASSET_CLASS) +
     +                                 ASSET_ALLOCATOR * INTEREST(MO,I)
                    ISSPS(MO,I,ASSET_CLASS) = ISSPS(MO,I,ASSET_CLASS) +
     +                                      ASSET_ALLOCATOR * ISS(MO,I)
                     PS_CASH_DIVIDEND(MO,I,ASSET_CLASS) =
     +                           PS_CASH_DIVIDEND(MO,I,ASSET_CLASS)
     +                          + ASSET_ALLOCATOR * CASH_PAYMENTS(MO,I)
                     LTD_PS_ISSUE_TAX_EXPENSE(MO,I,ASSET_CLASS) =
     +                       LTD_PS_ISSUE_TAX_EXPENSE(MO,I,ASSET_CLASS)
     +                      + ASSET_ALLOCATOR * ISSUE_TAX_EXPENSE(MO,I)
                     PS_NEW_ISSUE_PREM_DISC_AMOUNT(MO,I,ASSET_CLASS) =
     +                  PS_NEW_ISSUE_PREM_DISC_AMOUNT(MO,I,ASSET_CLASS)
     +                   + ASSET_ALLOCATOR * PREMIUM_DISCOUNT_EXP(MO,I)
                  ENDDO
               ELSEIF(TYPE == COMMON) THEN
                  DO MO = 0, 12
                    ! Todo: Remove code in IF block
                     IF(.false. .and. (EQUITY_MARKET == 'E' .OR.
     +                        ASSET_CLASS == PARENT_CLASS_ID_NUM)) THEN
                        COMMON_STOCK_ISSUED_AMOUNT(MO,I,
     +                                            ASSET_CLASS,Equity) =
     +                      COMMON_STOCK_ISSUED_AMOUNT(MO,I,
     +                                              ASSET_CLASS,Equity)
     +                      + ASSET_ALLOCATOR * ISS(MO,I)
                        COMMON_STOCK_ISSUED_SHARES(MO,I,
     +                                            ASSET_CLASS,Equity) =
     +                      COMMON_STOCK_ISSUED_SHARES(MO,I,
     +                                              ASSET_CLASS,Equity)
     +                      + ASSET_ALLOCATOR * SHARES_ISSUED(MO,I)

                        COMMON_STOCK_BUYBACK_AMOUNT(MO,I,
     +                                            ASSET_CLASS,Equity) =
     +                     COMMON_STOCK_BUYBACK_AMOUNT(MO,I,
     +                                              ASSET_CLASS,Equity)
     +                     + ASSET_ALLOCATOR * RET(MO,I)
                        COMMON_STOCK_BUYBACK_SHARES(MO,I,
     +                                            ASSET_CLASS,Equity) =
     +                     COMMON_STOCK_BUYBACK_SHARES(MO,I,
     +                                              ASSET_CLASS,Equity)
     +                     + ASSET_ALLOCATOR * SHARES_PURCHASED(MO,I)
                     ELSE
                        COMMON_STOCK_ISSUED_AMOUNT(MO,I,
     +                                            ASSET_CLASS,Parent) =
     +                      COMMON_STOCK_ISSUED_AMOUNT(MO,I,
     +                                              ASSET_CLASS,Parent)
     +                      + ASSET_ALLOCATOR * ISS(MO,I)
                        COMMON_STOCK_ISSUED_SHARES(MO,I,
     +                                            ASSET_CLASS,Parent) =
     +                      COMMON_STOCK_ISSUED_SHARES(MO,I,
     +                                              ASSET_CLASS,Parent)
     +                      + ASSET_ALLOCATOR * SHARES_ISSUED(MO,I)

                        COMMON_STOCK_BUYBACK_AMOUNT(MO,I,
     +                                            ASSET_CLASS,Parent) =
     +                     COMMON_STOCK_BUYBACK_AMOUNT(MO,I,
     +                                              ASSET_CLASS,Parent)
     +                     + ASSET_ALLOCATOR * RET(MO,I)
                        COMMON_STOCK_BUYBACK_SHARES(MO,I,
     +                                            ASSET_CLASS,Parent) =
     +                     COMMON_STOCK_BUYBACK_SHARES(MO,I,
     +                                              ASSET_CLASS,Parent)
     +                     + ASSET_ALLOCATOR * SHARES_PURCHASED(MO,I)

                     ENDIF

                     COMMON_STOCK_BALANCE(MO,I,ASSET_CLASS) =
     +                           COMMON_STOCK_BALANCE(MO,I,ASSET_CLASS)
     +                            + ASSET_ALLOCATOR * BAL(MO,I)

                     COMMON_STOCK_SHARES_OUTSTANDING(MO,I,ASSET_CLASS) =
     +                 COMMON_STOCK_SHARES_OUTSTANDING(MO,I,ASSET_CLASS)
     +                 + ASSET_ALLOCATOR * SHARES_OUTSTANDING(MO,I)
!
                     PURCHASED_SHARES_RE_ADJ(MO,I,ASSET_CLASS) =
     +                   PURCHASED_SHARES_RE_ADJ(MO,I,ASSET_CLASS)
     +                   + ASSET_ALLOCATOR * ADJ_RETAINED_EARNINGS(MO,I)

                     CS_ISSUE_TAX_EXPENSE(MO,I,ASSET_CLASS) =
     +                      CS_ISSUE_TAX_EXPENSE(MO,I,ASSET_CLASS) +
     +                         ASSET_ALLOCATOR * ISSUE_TAX_EXPENSE(MO,I)
!
                     CS_ISSUE_EXP_BAL(MO,I,ASSET_CLASS) =
     +                       CS_ISSUE_EXP_BAL(MO,I,ASSET_CLASS)
     +                       + ASSET_ALLOCATOR * ISSUE_EXPENSE_BAL(MO,I)
!

                     CS_ISSUE_EXP_AMORT(MO,I,ASSET_CLASS) =
     +                     CS_ISSUE_EXP_AMORT(MO,I,ASSET_CLASS)
     +                    + ASSET_ALLOCATOR * ISSUE_EXPENSE_AMORT(MO,I)
                  ENDDO
               ELSE
!        LUMP MTG AND LTD TOGETHER
                  AINTDB(I,ASSET_CLASS) = AINTDB(I,ASSET_CLASS) +

     +                                       ASSET_ALLOCATOR * ANNINT(I)

                  DO MO = 0, 12
                     CURRENT_PORTION_OF_LTD(MO,I,ASSET_CLASS) =
     +                     CURRENT_PORTION_OF_LTD(MO,I,ASSET_CLASS)
     +                    + ASSET_ALLOCATOR * LTD_CURRENT_PORTION(MO,I)
                     LTD_ISSUE_EXP_BAL(MO,I,ASSET_CLASS) =
     +                       LTD_ISSUE_EXP_BAL(MO,I,ASSET_CLASS)
     +                      + ASSET_ALLOCATOR * ISSUE_EXPENSE_BAL(MO,I)
                     LTD_ISSUE_EXP_AMORT(MO,I,ASSET_CLASS) =
     +                     LTD_ISSUE_EXP_AMORT(MO,I,ASSET_CLASS)
     +                    + ASSET_ALLOCATOR * ISSUE_EXPENSE_AMORT(MO,I)
                     LTD_PREM_BAL(MO,I,ASSET_CLASS) =
     +                             LTD_PREM_BAL(MO,I,ASSET_CLASS)
     +                            + ASSET_ALLOCATOR * PREMIUM_BAL(MO,I)
                     LTD_PREM_AMORT(MO,I,ASSET_CLASS) =
     +                           LTD_PREM_AMORT(MO,I,ASSET_CLASS)
     +                          + ASSET_ALLOCATOR * PREMIUM_AMORT(MO,I)
                     SFLTD(MO,I,ASSET_CLASS)=SFLTD(MO,I,ASSET_CLASS) +
     +                             ASSET_ALLOCATOR * SINKING_FUND(MO,I)
                    RETLTD(MO,I,ASSET_CLASS)=RETLTD(MO,I,ASSET_CLASS)+
     +                                      ASSET_ALLOCATOR * RET(MO,I)
                     INTLTD(MO,I,ASSET_CLASS)=INTLTD(MO,I,ASSET_CLASS)+
     +                                 ASSET_ALLOCATOR * INTEREST(MO,I)
                     ISSLTD(MO,I,ASSET_CLASS)=ISSLTD(MO,I,ASSET_CLASS)+
     +                                      ASSET_ALLOCATOR * ISS(MO,I)
                     LTD_CASH_INTEREST(MO,I,ASSET_CLASS) =
     +                            LTD_CASH_INTEREST(MO,I,ASSET_CLASS) +
     +                            ASSET_ALLOCATOR * CASH_PAYMENTS(MO,I)
                     LTD_PS_ISSUE_TAX_EXPENSE(MO,I,ASSET_CLASS) =
     +                     LTD_PS_ISSUE_TAX_EXPENSE(MO,I,ASSET_CLASS) +
     +                        ASSET_ALLOCATOR * ISSUE_TAX_EXPENSE(MO,I)
                     LTD_NEW_ISSUE_PREM_DISC_AMOUNT(MO,I,ASSET_CLASS) =
     +                 LTD_NEW_ISSUE_PREM_DISC_AMOUNT(MO,I,ASSET_CLASS)
     +                  + ASSET_ALLOCATOR * PREMIUM_DISCOUNT_EXP(MO,I)
                  ENDDO
               ENDIF
            ENDDO
            CLASS_POINTER = CLASS_POINTER + 1
            IF(CLASS_POINTER > AVAIL_DATA_YEARS) EXIT
            IF(ASSET_CLASS_LIST(CLASS_POINTER) == -99) EXIT
         ENDDO

! WRITE RESULTS

         IF((DEBT_ASSET_REPORT .AND. DELETE > 1) .OR.
     +                                        REPORT_ALL_ACCOUNTS) THEN
            ACCOUNTS_REPORTED = ACCOUNTS_REPORTED + 1
            IF(IREC < 10) THEN
               OUTPUT_OPTION_NAME = DESC//'   '//
     +                               LEFT_JUSTIFY_I2_IN_STR(INT2(IREC))
            ELSEIF(IREC < 100) THEN
               OUTPUT_OPTION_NAME = DESC//'  '//
     +                               LEFT_JUSTIFY_I2_IN_STR(INT2(IREC))
            ELSEIF(IREC < 1000) THEN
               OUTPUT_OPTION_NAME = DESC//' '//
     +                               LEFT_JUSTIFY_I2_IN_STR(INT2(IREC))
            ELSEIF(IREC < 10000) THEN
               OUTPUT_OPTION_NAME = DESC//
     +                               LEFT_JUSTIFY_I2_IN_STR(INT2(IREC))
            ENDIF

            IF(WVPA()) THEN
               DRILLING_ACCOUNT_NAME = WVPA_REPORTING_NAME(DESC,
     +                                          WVPA_PRIMARY_ACCOUNT,
     +                                          WVPA_DEPARTMENT_UNIT,
     +                                         WVPA_SUB_ACCOUNT_NUMBER)
            ELSE
               DRILLING_ACCOUNT_NAME = OUTPUT_OPTION_NAME
            ENDIF
            ANNUAL_INFO_ACTIVE = DRILLING_REPRT_LEVEL == 'A'
            IF(ISSUE_ACURRED) THEN
               IF(TYPE == PFS) THEN
                  DRILLING_NAME = 'Preferred Issued'
               ELSEIF(TYPE == MIPS) THEN
                  DRILLING_NAME = 'MIPS Issued'
               ELSEIF(TYPE == NOTE_RECEIVABLE) THEN
                  DRILLING_NAME = 'Receivable Issued'
               ELSEIF(TYPE == INVESTMENT) THEN
                  DRILLING_NAME = 'Investment Made'
               ELSEIF(TYPE == NOTE_PAYABLE) THEN
                  DRILLING_NAME = 'Payable Issued'
               ELSEIF(TYPE == COMMON) THEN
                  DRILLING_NAME = 'Common Issued'
               ELSE
                  DRILLING_NAME = 'LTD Issued'
               ENDIF
               VOID_INT2 = WRITE_DRILLING_RPT(DRILLING_NAME,
     +                                        DRILLING_ACCOUNT_NAME,
     +                                        ISS,
     +                                        CASH_REPORT_ITEM,
     +                                        ANNUAL_INFO_ACTIVE)
            ENDIF
            IF(TYPE == COMMON) THEN
               STOCK_WAS_ISSUED = .FALSE.
               DO YR = 1, FINANCIAL_SIMULATION_YEARS
                  IF(ISS(0,YR) /= 0.) THEN
                     DRILLING_NAME = 'Common Issued'
                     VOID_INT2 = WRITE_DRILLING_RPT(DRILLING_NAME,
     +                                           DRILLING_ACCOUNT_NAME,
     +                                            ISS,
     +                                            CASH_REPORT_ITEM,
     +                                            ANNUAL_INFO_ACTIVE)
                     STOCK_WAS_ISSUED = .TRUE.
                     EXIT
                  ENDIF
               ENDDO
               DO YR = 1, FINANCIAL_SIMULATION_YEARS
                  IF(ISSUE_TAX_EXPENSE(0,YR) /= 0.) THEN
                     DRILLING_NAME = 'Common Issue Expense'
                     VOID_INT2 = WRITE_DRILLING_RPT(DRILLING_NAME,
     +                                           DRILLING_ACCOUNT_NAME,
     +                                            ISSUE_TAX_EXPENSE,
     +                                            TAX_REPORT_ITEM,
     +                                            ANNUAL_INFO_ACTIVE)
                     EXIT
                  ENDIF
               ENDDO
               DO YR = 1, FINANCIAL_SIMULATION_YEARS
                  IF(ISSUE_EXPENSE_AMORT(0,YR) /= 0.) THEN
                     DRILLING_NAME = 'Amort Common Issue Expense'
                     VOID_INT2 = WRITE_DRILLING_RPT(DRILLING_NAME,
     +                                           DRILLING_ACCOUNT_NAME,
     +                                            ISSUE_EXPENSE_AMORT,
     +                                            BALANCE_SHEET_ITEM,
     +                                            ANNUAL_INFO_ACTIVE)
                     EXIT
                  ENDIF
               ENDDO
               SHARES_WHERE_ISSUED = .FALSE.
               DO YR = 1, FINANCIAL_SIMULATION_YEARS
                  IF(SHARES_ISSUED(0,YR) /= 0.) THEN
                     DRILLING_NAME = 'Issued Common Shares'
                     VOID_INT2 = WRITE_DRILLING_RPT(DRILLING_NAME,
     +                                           DRILLING_ACCOUNT_NAME,
     +                                            SHARES_ISSUED,
     +                                            CASH_REPORT_ITEM,
     +                                            ANNUAL_INFO_ACTIVE)
                     SHARES_WHERE_ISSUED = .TRUE.
                     EXIT
                  ENDIF
               ENDDO
               SHARES_WHERE_PURCHASES = .FALSE.
               DO YR = 1, FINANCIAL_SIMULATION_YEARS
                  IF(SHARES_PURCHASED(0,YR) /= 0.) THEN
                     DRILLING_NAME = 'Purchased Common Shares'
                     VOID_INT2 = WRITE_DRILLING_RPT(DRILLING_NAME,
     +                                           DRILLING_ACCOUNT_NAME,
     +                                            SHARES_PURCHASED,
     +                                            CASH_REPORT_ITEM,
     +                                            ANNUAL_INFO_ACTIVE)
                     SHARES_WHERE_PURCHASES = .TRUE.
                     EXIT
                  ENDIF
               ENDDO
               STOCK_WAS_PURCHASED = .FALSE.
               DO YR = 1, FINANCIAL_SIMULATION_YEARS
                  IF(RET(0,YR) /= 0.) THEN
                     DRILLING_NAME = 'Common Buyback'
                     VOID_INT2 = WRITE_DRILLING_RPT(DRILLING_NAME,
     +                                           DRILLING_ACCOUNT_NAME,
     +                                            RET,
     +                                            CASH_REPORT_ITEM,
     +                                            ANNUAL_INFO_ACTIVE)
                     STOCK_WAS_PURCHASED = .TRUE.
                     EXIT
                  ENDIF
               ENDDO
               IF(STOCK_WAS_PURCHASED .OR. STOCK_WAS_ISSUED
     +                                        .OR. BAL(0,1) /= 0.) THEN
                  DRILLING_NAME = 'Common Stock Balance'
                  VOID_INT2 = WRITE_DRILLING_RPT(DRILLING_NAME,
     +                                           DRILLING_ACCOUNT_NAME,
     +                                           BAL,
     +                                           BALANCE_SHEET_ITEM,
     +                                           ANNUAL_INFO_ACTIVE)
               ENDIF
               IF(SHARES_WHERE_PURCHASES .OR. SHARES_WHERE_ISSUED
     +                         .OR. SHARES_OUTSTANDING(0,1) /= 0.) THEN
                  DRILLING_NAME = 'Common Shares Outstanding'
                  VOID_INT2 = WRITE_DRILLING_RPT(DRILLING_NAME,
     +                                           DRILLING_ACCOUNT_NAME,
     +                                           SHARES_OUTSTANDING,
     +                                           CASH_REPORT_ITEM,
     +                                           ANNUAL_INFO_ACTIVE)
               ENDIF
            ELSE
               IF(TYPE == PFS) THEN
                  DRILLING_NAME = 'Preferred Dividends'
               ELSEIF(TYPE == MIPS) THEN
                  DRILLING_NAME = 'MIPS Dividends'
               ELSEIF(TYPE == NOTE_RECEIVABLE) THEN
                  DRILLING_NAME = 'Notes Receivable Income'
               ELSEIF(TYPE == INVESTMENT) THEN
                  IF(INVESTMENT_TYPE == '7' .OR.
     +                                     INVESTMENT_TYPE == 'D') THEN
                     DRILLING_NAME = 'Investment Dividend Earnings'
                  ELSE
                     DRILLING_NAME = 'Investment Interest_Earnings'
                  ENDIF
               ELSEIF(TYPE == NOTE_PAYABLE) THEN
                  DRILLING_NAME = 'Notes Payable Interest Paid'
               ELSE
                  DRILLING_NAME = 'LTD Interest'
               ENDIF
               VOID_INT2 = WRITE_DRILLING_RPT(DRILLING_NAME,
     +                                     DRILLING_ACCOUNT_NAME,
     +                                     INTEREST,
     +                                     INCOME_REPORT_ITEM,
     +                                     ANNUAL_INFO_ACTIVE)
               IF(MONTHLY_MIDAS_ACTIVE) THEN
                  VOID_INT2 = WRITE_DRILLING_RPT(DRILLING_NAME,
     +                                           DRILLING_ACCOUNT_NAME,
     +                                           CASH_PAYMENTS,
     +                                           CASH_REPORT_ITEM,
     +                                           ANNUAL_INFO_ACTIVE)
               ELSE
                  VOID_INT2 = WRITE_DRILLING_RPT(DRILLING_NAME,
     +                                           DRILLING_ACCOUNT_NAME,
     +                                           INTEREST,
     +                                           CASH_REPORT_ITEM,
     +                                           ANNUAL_INFO_ACTIVE)
               ENDIF
               IF(TYPE == PFS) THEN
                  DRILLING_NAME = 'Preferred Balance'
               ELSEIF(TYPE == MIPS) THEN
                  DRILLING_NAME = 'MIPS Balance'
               ELSEIF(TYPE == NOTE_RECEIVABLE) THEN
                  DRILLING_NAME = 'Notes Receivable Balance'
               ELSEIF(TYPE == INVESTMENT) THEN
                  DRILLING_NAME = 'Investment Balance'
               ELSEIF(TYPE == NOTE_PAYABLE) THEN
                  DRILLING_NAME = 'Notes Payable Balance'
               ELSE
                  DRILLING_NAME = 'LTD Balance'
               ENDIF
               VOID_INT2 = WRITE_DRILLING_RPT(DRILLING_NAME,
     +                                        DRILLING_ACCOUNT_NAME,
     +                                        BAL,
     +                                        BALANCE_SHEET_ITEM,
     +                                        ANNUAL_INFO_ACTIVE)
               RETIREMENTS_ACTIVE = .FALSE.
               DO YR = 1, FINANCIAL_SIMULATION_YEARS
                  DO MO = 0, 12
                     SINKING_FUND(MO,YR) = SINKING_FUND(MO,YR)
     +                                     + RET(MO,YR)
                  ENDDO
                  RETIREMENTS_ACTIVE = RETIREMENTS_ACTIVE .OR.
     +                                         SINKING_FUND(0,YR) /= 0.
               ENDDO
               IF(RETIREMENTS_ACTIVE) THEN
                  IF(TYPE == PFS) THEN
                     DRILLING_NAME = 'Preferred Retirements'
                  ELSEIF(TYPE == MIPS) THEN
                     DRILLING_NAME = 'MIPS Retirements'
                  ELSEIF(TYPE == NOTE_RECEIVABLE) THEN
                     DRILLING_NAME = 'Notes Receivable Retired'
                  ELSEIF(TYPE == INVESTMENT) THEN
                     DRILLING_NAME = 'Investments Sold'
                  ELSEIF(TYPE == NOTE_PAYABLE) THEN
                     DRILLING_NAME = 'Notes Payable Retired'
                  ELSE
                     DRILLING_NAME = 'LTD Retirements'
                  ENDIF
                  VOID_INT2 = WRITE_DRILLING_RPT(DRILLING_NAME,
     +                                           DRILLING_ACCOUNT_NAME,
     +                                           SINKING_FUND,
     +                                           CASH_REPORT_ITEM,
     +                                           ANNUAL_INFO_ACTIVE)
               ENDIF
               DO YR = 1, FINANCIAL_SIMULATION_YEARS
                  IF(ISSUE_EXPENSE_AMORT(0,YR) /= 0.) THEN
                     IF(TYPE == PFS) THEN
                       DRILLING_NAME = 'Amort Preferred Issue Expense '
                     ELSEIF(TYPE == MIPS) THEN
                        DRILLING_NAME = 'Amort MIPS Issue Expense'
                     ELSEIF(TYPE == NOTE_RECEIVABLE) THEN
                        DRILLING_NAME = 'Amort Notes Rec Issue Expense'
                     ELSEIF(TYPE == INVESTMENT) THEN
                       DRILLING_NAME = 'Amort Investments Sold Expense'
                     ELSEIF(TYPE == NOTE_PAYABLE) THEN
                        DRILLING_NAME = 'Amort Notes Pay Issue Expense'
                     ELSE
                        DRILLING_NAME = 'Amort LTD Issue Expense'
                     ENDIF
                     VOID_INT2 = WRITE_DRILLING_RPT(DRILLING_NAME,
     +                                           DRILLING_ACCOUNT_NAME,
     +                                            ISSUE_EXPENSE_AMORT,
     +                                            BALANCE_SHEET_ITEM,
     +                                            ANNUAL_INFO_ACTIVE)
                     EXIT
                  ENDIF
               ENDDO
               IF(INTRA_COMPANY == 'Y') THEN
                  IF(TYPE == NOTE_RECEIVABLE .OR.
     +                                       TYPE == NOTE_PAYABLE) THEN
                     IF(TYPE == NOTE_RECEIVABLE) THEN
                        DRILLING_NAME = 'Interest Paid'
                     ELSEIF(TYPE == NOTE_PAYABLE) THEN
                        DRILLING_NAME = 'Interest Income'
                     ENDIF
                     VOID_INT2 = WRITE_DRILLING_RPT(DRILLING_NAME,
     +                                           DRILLING_ACCOUNT_NAME,
     +                                            INTEREST,
     +                                            INCOME_REPORT_ITEM,
     +                                            ANNUAL_INFO_ACTIVE)
                     DRILLING_NAME = 'Notes with Afilliates'
                     VOID_INT2 = WRITE_DRILLING_RPT(DRILLING_NAME,
     +                                           DRILLING_ACCOUNT_NAME,
     +                                            ISS,
     +                                            CASH_REPORT_ITEM,
     +                                            ANNUAL_INFO_ACTIVE)
                  ELSE
                     DRILLING_NAME = 'Other Income'
                     VOID_INT2 = WRITE_DRILLING_RPT(DRILLING_NAME,
     +                                           DRILLING_ACCOUNT_NAME,
     +                                            INTEREST,
     +                                            INCOME_REPORT_ITEM,
     +                                            ANNUAL_INFO_ACTIVE)
                     DO YR = 1, FINANCIAL_SIMULATION_YEARS
                        DO MO = 0, 12
                           SINKING_FUND(MO,YR) = ISS(MO,YR)
     +                                           - SINKING_FUND(MO,YR)
                        ENDDO
                     ENDDO
                     DRILLING_NAME = 'Investment in Afilliates'
                     VOID_INT2 = WRITE_DRILLING_RPT(DRILLING_NAME,
     +                                           DRILLING_ACCOUNT_NAME,
     +                                            SINKING_FUND,
     +                                            CASH_REPORT_ITEM,
     +                                            ANNUAL_INFO_ACTIVE)
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDDO
      CALL CLOSE_DEBT_FILE
      IF(LF95 .AND. NOACCT > 0) THEN
         WRITE(SCREEN_MESSAGES,'(I4,A)') NOACCT,'-Debt accounts'
         CALL MG_LOCATE_WRITE(0,0,TRIM(SCREEN_MESSAGES),
     +                                                  ALL_VERSIONS,1)
      ENDIF
      IF(WVPA() .AND. REPORT_HEADER_OPEN) THEN
         INDEX_NAME = 'All Issues Totals'
         DO YR = 1, FINANCIAL_SIMULATION_YEARS
            DO MO1 = 1, 13
               IF(MO1 == 13) THEN
                  MO= 0
               ELSE
                  MO = MO1
               ENDIF
               WRITE(REPORTING_UNIT,REC=REPORTING_REC)
     +                        PRT_ENDPOINT(),
     +                        FLOAT(YR+get_BASE_YEAR()),
     +                        INDEX_NAME,
     +                        SHORT_MONTH_NAMES(MO),
     +                      (TOTAL_REPORT_VARS(MO,YR,VarNo), VarNo=0,4)
               REPORTING_REC = REPORTING_REC + 1
            ENDDO
         ENDDO
      ENDIF

      DEALLOCATE(ANNINT,SINKING_FUND,BAL,RET,INTEREST,RATE,ISS)
      DEALLOCATE(ASSET_CLASS_LIST,ASSET_ALLOCATION_LIST)
      DEALLOCATE(PREMIUM_BAL,PREMIUM_AMORT,PREMIUM_DISCOUNT_EXP)
      DEALLOCATE(ISSUE_EXPENSE_BAL,ISSUE_EXPENSE_AMORT,
     +           ISSUE_TAX_EXPENSE)
      DEALLOCATE(SHARES_ISSUED,
     +           SHARES_PURCHASED,
     +           SHARES_OUTSTANDING,
     +           INTEREST_EXPENSE_ADJ,
     +           CASH_PAYMENTS,
     +           ADJ_RETAINED_EARNINGS,
     +           INTEREST_IN_SPLIT_MONTH,
     +           LTD_CURRENT_PORTION)

! WRITE RESULTS TO MASTER FILE FOR TOTAL COMPANY

      ISSPS(12,0,0) = PS_BASEYR
      ISSLTD(12,0,0) = LTD_BASEYR
      MO = 0

      DO ASSET_CLASS = 1, NUM_OF_ASSET_CLASSES
         DO I = 0, FINANCIAL_SIMULATION_YEARS
               SFPS(MO,I,0) = SFPS(MO,I,0) + SFPS(MO,I,ASSET_CLASS)
               RETPS(MO,I,0) = RETPS(MO,I,0) + RETPS(MO,I,ASSET_CLASS)
               INTPS(MO,I,0) = INTPS(MO,I,0) + INTPS(MO,I,ASSET_CLASS)
               ISSPS(MO,I,0) = ISSPS(MO,I,0) + ISSPS(MO,I,ASSET_CLASS)
               AINTPS(I,0) = AINTPS(I,0)+AINTPS(I,ASSET_CLASS)
! LUMP MTG AND LTD TOGETHER
               SFLTD(MO,I,0) = SFLTD(MO,I,0)  + SFLTD(MO,I,ASSET_CLASS)
               RETLTD(MO,I,0) = RETLTD(MO,I,0)+RETLTD(MO,I,ASSET_CLASS)
               CURRENT_PORTION_OF_LTD(MO,I,0) =
     +                        CURRENT_PORTION_OF_LTD(MO,I,0)
     +                       + CURRENT_PORTION_OF_LTD(MO,I,ASSET_CLASS)
               INTLTD(MO,I,0) = INTLTD(MO,I,0)+INTLTD(MO,I,ASSET_CLASS)
               ISSLTD(MO,I,0) = ISSLTD(MO,I,0)+ISSLTD(MO,I,ASSET_CLASS)
               AINTDB(I,0) = AINTDB(I,0)+AINTDB(I,ASSET_CLASS)

! INTRA COMPANY ITEMS

            OTHER_INCOME(MO,I,0) = OTHER_INCOME(MO,I,0) +
     +                                   OTHER_INCOME(MO,I,ASSET_CLASS)
            INVESTMENT_IN_AFILLIATES(MO,I,0) =
     +                        INVESTMENT_IN_AFILLIATES(MO,I,0) +
     +                       INVESTMENT_IN_AFILLIATES(MO,I,ASSET_CLASS)
     +
            INTEREST_INCOME(MO,I,0) = INTEREST_INCOME(MO,I,0) +
     +                                INTEREST_INCOME(MO,I,ASSET_CLASS)
            LOANS_TO_AFILLIATES(MO,I,0) = LOANS_TO_AFILLIATES(MO,I,0) +
     +                            LOANS_TO_AFILLIATES(MO,I,ASSET_CLASS)
         ENDDO
      ENDDO
      IF(save_base_case_loc) THEN
         NUM_OF_BASE_CASE_ASSET_CLASSES = NUM_OF_ASSET_CLASSES
         MAX_BASE_CASE_ASSET_CLASS_NUM = MAX_ASSET_CLASS_NUM
         CALL OPEN_DB_BASE_CASE_FILE(INT2(10))
         WRITE(10) ISSPS,RETPS,ISSLTD,RETLTD,INTLTD,
     +             INTPS,SFPS,SFLTD,AINTPS,AINTDB,
     +             OTHER_INCOME,INVESTMENT_IN_AFILLIATES,
     +             INTEREST_INCOME,LOANS_TO_AFILLIATES,
     +             LTD_CASH_INTEREST,
     +             PS_CASH_DIVIDEND,
     +             PS_PREM_BAL,PS_PREM_AMORT,
     +             LTD_PREM_BAL,LTD_PREM_AMORT,
     +             PS_ISSUE_EXP_BAL,PS_ISSUE_EXP_AMORT,
     +             LTD_ISSUE_EXP_BAL,LTD_ISSUE_EXP_AMORT,
     +             LTD_PS_ISSUE_TAX_EXPENSE,
     +             MIPS_INCOME_TAX_DEDUCTION,
     +             COMMON_STOCK_ISSUED_AMOUNT,
     +             COMMON_STOCK_ISSUED_SHARES,
     +             COMMON_STOCK_BUYBACK_AMOUNT,
     +             COMMON_STOCK_BUYBACK_SHARES,
     +             COMMON_STOCK_BALANCE,
     +             COMMON_STOCK_SHARES_OUTSTANDING,
     +             PURCHASED_SHARES_RE_ADJ,
     +             CS_ISSUE_EXP_BAL,
     +             CS_ISSUE_EXP_AMORT,
     +             CS_ISSUE_TAX_EXPENSE,
     +             NOTES_PAYABLE,
     +             NOTES_RECEIVABLE,
     +             INVESTMENT_INCOME,
     +             LTD_NEW_ISSUE_PREM_DISC_AMOUNT,
     +             PS_NEW_ISSUE_PREM_DISC_AMOUNT,
     +             CURRENT_PORTION_OF_LTD
         CLOSE(10)
      ENDIF
      RETURN

!          CALCULATE INTEREST PAID ON LTD

      ENTRY CAL_LTD_INTEREST_PAID(PAY_MONTH,R_YR,CARRY_OVER,
     +                                 MONTHLY_INTEREST_PAID)

         YR = R_YR
         IF(PAY_MONTH < 1) THEN
            MONTHLY_INTEREST_PAID(PAY_MONTH+12,YR-1) = CARRY_OVER
         ELSEIF(PAY_MONTH > 12) THEN
            IF(YR < FINANCIAL_SIMULATION_YEARS)
     +            MONTHLY_INTEREST_PAID(PAY_MONTH-12,YR+1) = CARRY_OVER
         ELSE
            MONTHLY_INTEREST_PAID(PAY_MONTH,YR) = CARRY_OVER
         ENDIF
         CARRY_OVER = 0.
      RETURN

      ENTRY COMMON_STOCK_ANALYSIS(DESCRIPTION,R_AMT,R_BYRBAL,
     +                            ISSUE_MO,ISSUE_YR,MAT_MO,YR_MAT,
     +                            R_COMMON_SHARES,
     +                            R_ASSET_CLASS,R_ASSET_CLASS_VECTOR,
     +                            R_INTRA_COMPANY,R_INTRA_CLASS_ID,
     +                            R_RET,R_BAL,R_ISS,
     +                            R_SHARES_ISSUED,
     +                            R_SHARES_PURCHASED,
     +                            R_SHARES_OUTSTANDING,
     +                            R_ADJ_RETAINED_EARNINGS,
     +                            R_ISSUE_PRICE_PER_SHARE,
     +                            R_PURCHASE_PRICE_PER_SHARE)

         ISSUE_AMOUNT = R_AMT
         R_BAL(12,0) = R_BYRBAL
         IF(ISSUE_YR <= get_BASE_YEAR()) THEN
            R_SHARES_OUTSTANDING(12,0) = R_COMMON_SHARES
         ELSE
            R_SHARES_OUTSTANDING(12,0) = 0.
         ENDIF

         DO YR = 1, FINANCIAL_SIMULATION_YEARS
            R_BAL(0,YR) = R_BAL(12,YR-1)
            R_SHARES_OUTSTANDING(0,YR) = R_SHARES_OUTSTANDING(12,YR-1)
            globecom_year = get_BASE_YEAR() + YR
            IF(globecom_year < ISSUE_YR) then
                CYCLE
            end if
            IF(globecom_year == ISSUE_YR) THEN
               R_ISS(ISSUE_MO,YR) = ISSUE_AMOUNT
               R_ISS(0,YR) = ISSUE_AMOUNT
               R_SHARES_ISSUED(ISSUE_MO,YR) = R_COMMON_SHARES
               R_SHARES_ISSUED(0,YR) = R_COMMON_SHARES
            ENDIF
            IF(globecom_year == YR_MAT) THEN
               IF(R_PURCHASE_PRICE_PER_SHARE /= 0.) THEN
                  REPURCHASE_AMOUNT = R_PURCHASE_PRICE_PER_SHARE *
     +                                R_COMMON_SHARES
               ELSE
                  REPURCHASE_AMOUNT = ISSUE_AMOUNT
               ENDIF
               R_RET(MAT_MO,YR) = REPURCHASE_AMOUNT ! ISSUE_AMOUNT
               R_RET(0,YR) = REPURCHASE_AMOUNT ! ISSUE_AMOUNT
               R_SHARES_PURCHASED(MAT_MO,YR) = R_COMMON_SHARES
               R_SHARES_PURCHASED(0,YR) = R_COMMON_SHARES
               R_ADJ_RETAINED_EARNINGS(MAT_MO,YR) = ISSUE_AMOUNT
     +                                              - REPURCHASE_AMOUNT
               R_ADJ_RETAINED_EARNINGS(0,YR) = ISSUE_AMOUNT
     +                                         - REPURCHASE_AMOUNT
            ENDIF
            DO MO = 1, 12
              R_BAL(MO,YR)=R_BAL(MO-1,YR) + R_ISS(MO,YR) - R_RET(MO,YR)
              R_SHARES_OUTSTANDING(MO,YR)=R_SHARES_OUTSTANDING(MO-1,YR)
     +                                     + R_SHARES_ISSUED(MO,YR)
     +                                     - R_SHARES_PURCHASED(MO,YR)
            ENDDO
         ENDDO

      RETURN


      ENTRY SET_UP_DEBT_ARRAYS


         CALL RETURN_INITIALIZATION_CLASSES(NUM_OF_ASSET_CLASSES,
     +                                       MAX_ASSET_CLASS_NUM)
      ENTRY SET_UP_BASE_CASE_DEBT_ARRAYS
         IF(ALLOCATED(ASSET_CLASS_POINTER))
     +                                  DEALLOCATE(ASSET_CLASS_POINTER)
         IF(MAX_ASSET_CLASS_NUM > 0) THEN
            ALLOCATE(ASSET_CLASS_POINTER(MAX_ASSET_CLASS_NUM))
            CALL RETURN_INITIALIZATION_POINTER(ASSET_CLASS_POINTER)

         ENDIF

         IF(ALLOCATED(ISSPS)) DEALLOCATE(ISSPS,RETPS,ISSLTD,RETLTD,
     +                           INTLTD,INTPS,SFPS,SFLTD,AINTPS,AINTDB,
     +                           OTHER_INCOME,INVESTMENT_IN_AFILLIATES,
     +                            INTEREST_INCOME,LOANS_TO_AFILLIATES,
     +                            PS_PREM_BAL,PS_PREM_AMORT,
     +                            LTD_PREM_BAL,LTD_PREM_AMORT,
     +                            PS_ISSUE_EXP_BAL,PS_ISSUE_EXP_AMORT,
     +                           LTD_ISSUE_EXP_BAL,LTD_ISSUE_EXP_AMORT,
     +                            LTD_PS_ISSUE_TAX_EXPENSE,
     +                            MIPS_INCOME_TAX_DEDUCTION,
     +                            LTD_CASH_INTEREST,
     +                            PS_CASH_DIVIDEND,
     +                            COMMON_STOCK_ISSUED_AMOUNT,
     +                            COMMON_STOCK_ISSUED_SHARES,
     +                            COMMON_STOCK_BUYBACK_AMOUNT,
     +                            COMMON_STOCK_BUYBACK_SHARES,
     +                            COMMON_STOCK_BALANCE,
     +                            COMMON_STOCK_SHARES_OUTSTANDING,
     +                            PURCHASED_SHARES_RE_ADJ,
     +                            CS_ISSUE_EXP_BAL,
     +                            CS_ISSUE_TAX_EXPENSE,
     +                            CS_ISSUE_EXP_AMORT,
     +                            LTD_NEW_ISSUE_PREM_DISC_AMOUNT,
     +                            PS_NEW_ISSUE_PREM_DISC_AMOUNT,
     +                            NOTES_PAYABLE,
     +                            NOTES_RECEIVABLE,
     +                            INVESTMENT_INCOME,
     +                            CURRENT_PORTION_OF_LTD)
         ALLOCATE(ISSPS(0:12,0:FINANCIAL_SIMULATION_YEARS,
     +                                        -1:NUM_OF_ASSET_CLASSES),
     +         RETPS(0:12,0:FINANCIAL_SIMULATION_YEARS,
     +                                        -1:NUM_OF_ASSET_CLASSES),
     +         ISSLTD(0:12,0:FINANCIAL_SIMULATION_YEARS,
     +                                        -1:NUM_OF_ASSET_CLASSES),
     +         RETLTD(0:12,0:FINANCIAL_SIMULATION_YEARS,
     +                                        -1:NUM_OF_ASSET_CLASSES),
     +        CURRENT_PORTION_OF_LTD(0:12,0:FINANCIAL_SIMULATION_YEARS,
     +                                        -1:NUM_OF_ASSET_CLASSES),
     +         INTLTD(0:12,0:FINANCIAL_SIMULATION_YEARS,
     +                                        -1:NUM_OF_ASSET_CLASSES),
     +         INTPS(0:12,0:FINANCIAL_SIMULATION_YEARS,
     +                                        -1:NUM_OF_ASSET_CLASSES),
     +         SFPS(0:12,0:FINANCIAL_SIMULATION_YEARS,
     +                                        -1:NUM_OF_ASSET_CLASSES),
     +         SFLTD(0:12,0:FINANCIAL_SIMULATION_YEARS,
     +                                        -1:NUM_OF_ASSET_CLASSES),
     +         OTHER_INCOME(0:12,0:FINANCIAL_SIMULATION_YEARS,
     +                                        -1:NUM_OF_ASSET_CLASSES),
     +         INVESTMENT_IN_AFILLIATES(0:12,
     +                                    0:FINANCIAL_SIMULATION_YEARS,
     +                                        -1:NUM_OF_ASSET_CLASSES),
     +         INTEREST_INCOME(0:12,0:FINANCIAL_SIMULATION_YEARS,
     +                                        -1:NUM_OF_ASSET_CLASSES),
     +         LOANS_TO_AFILLIATES(0:12,0:FINANCIAL_SIMULATION_YEARS,
     +                                        -1:NUM_OF_ASSET_CLASSES),
     +         LTD_CASH_INTEREST(0:12,0:FINANCIAL_SIMULATION_YEARS,
     +                                        -1:NUM_OF_ASSET_CLASSES),
     +         PS_CASH_DIVIDEND(0:12,0:FINANCIAL_SIMULATION_YEARS,
     +                                        -1:NUM_OF_ASSET_CLASSES),
     +         LTD_PS_ISSUE_TAX_EXPENSE(0:12,
     +                                    0:FINANCIAL_SIMULATION_YEARS,
     +                                        -1:NUM_OF_ASSET_CLASSES),
     +         COMMON_STOCK_ISSUED_AMOUNT(0:12,
     +                                    0:FINANCIAL_SIMULATION_YEARS,
     +                                      -1:NUM_OF_ASSET_CLASSES,2),
     +         COMMON_STOCK_ISSUED_SHARES(0:12,
     +                                    0:FINANCIAL_SIMULATION_YEARS,
     +                                      -1:NUM_OF_ASSET_CLASSES,2),
     +         COMMON_STOCK_BUYBACK_AMOUNT(0:12,
     +                                    0:FINANCIAL_SIMULATION_YEARS,
     +                                      -1:NUM_OF_ASSET_CLASSES,2),
     +         COMMON_STOCK_BUYBACK_SHARES(0:12,
     +                                    0:FINANCIAL_SIMULATION_YEARS,
     +                                      -1:NUM_OF_ASSET_CLASSES,2),
     +         COMMON_STOCK_BALANCE(0:12,0:FINANCIAL_SIMULATION_YEARS,
     +                                        -1:NUM_OF_ASSET_CLASSES),
     +         COMMON_STOCK_SHARES_OUTSTANDING(0:12,
     +                                    0:FINANCIAL_SIMULATION_YEARS,
     +                                        -1:NUM_OF_ASSET_CLASSES),
     +         PURCHASED_SHARES_RE_ADJ(0:12,
     +                                    0:FINANCIAL_SIMULATION_YEARS,
     +                                        -1:NUM_OF_ASSET_CLASSES),
     +         CS_ISSUE_EXP_BAL(0:12,0:FINANCIAL_SIMULATION_YEARS,
     +                                        -1:NUM_OF_ASSET_CLASSES),
     +         CS_ISSUE_EXP_AMORT(0:12,0:FINANCIAL_SIMULATION_YEARS,
     +                                        -1:NUM_OF_ASSET_CLASSES),
     +         CS_ISSUE_TAX_EXPENSE(0:12,0:FINANCIAL_SIMULATION_YEARS,
     +                                        -1:NUM_OF_ASSET_CLASSES),
     +         LTD_NEW_ISSUE_PREM_DISC_AMOUNT(0:12,
     +                                    0:FINANCIAL_SIMULATION_YEARS,
     +                                        -1:NUM_OF_ASSET_CLASSES),
     +         PS_NEW_ISSUE_PREM_DISC_AMOUNT(0:12,
     +                                    0:FINANCIAL_SIMULATION_YEARS,
     +                                        -1:NUM_OF_ASSET_CLASSES),
     +         NOTES_PAYABLE(0:12,0:FINANCIAL_SIMULATION_YEARS,
     +                                         -1:NUM_OF_ASSET_CLASSES,
     +                                         1:NUM_OF_NOTE_ACCOUNTS),
     +         NOTES_RECEIVABLE(0:12,0:FINANCIAL_SIMULATION_YEARS,
     +                                        -1:NUM_OF_ASSET_CLASSES,
     +                                         1:NUM_OF_NOTE_ACCOUNTS),
     +         INVESTMENT_INCOME(0:12,0:FINANCIAL_SIMULATION_YEARS,
     +                                      -1:NUM_OF_ASSET_CLASSES,
     +                                      1:NUM_INVESTMENT_ACCOUNTS),
     +         AINTPS(0:FINANCIAL_SIMULATION_YEARS,
     +                                        -1:NUM_OF_ASSET_CLASSES),
     +         AINTDB(0:FINANCIAL_SIMULATION_YEARS,
     +                                        -1:NUM_OF_ASSET_CLASSES),
     +         PS_PREM_BAL(0:12,0:FINANCIAL_SIMULATION_YEARS,
     +                                        -1:NUM_OF_ASSET_CLASSES),
     +         PS_PREM_AMORT(0:12,0:FINANCIAL_SIMULATION_YEARS,
     +                                        -1:NUM_OF_ASSET_CLASSES),
     +         LTD_PREM_BAL(0:12,0:FINANCIAL_SIMULATION_YEARS,
     +                                        -1:NUM_OF_ASSET_CLASSES),
     +         LTD_PREM_AMORT(0:12,0:FINANCIAL_SIMULATION_YEARS,
     +                                        -1:NUM_OF_ASSET_CLASSES),
     +         PS_ISSUE_EXP_BAL(0:12,0:FINANCIAL_SIMULATION_YEARS,
     +                                        -1:NUM_OF_ASSET_CLASSES),
     +         PS_ISSUE_EXP_AMORT(0:12,0:FINANCIAL_SIMULATION_YEARS,
     +                                        -1:NUM_OF_ASSET_CLASSES),
     +         LTD_ISSUE_EXP_BAL(0:12,0:FINANCIAL_SIMULATION_YEARS,
     +                                        -1:NUM_OF_ASSET_CLASSES),
     +         LTD_ISSUE_EXP_AMORT(0:12,0:FINANCIAL_SIMULATION_YEARS,
     +                                        -1:NUM_OF_ASSET_CLASSES),
     +         MIPS_INCOME_TAX_DEDUCTION(0:FINANCIAL_SIMULATION_YEARS,
     +                                         -1:NUM_OF_ASSET_CLASSES))

         ISSPS = 0.
         RETPS = 0.
         ISSLTD = 0.
         RETLTD = 0.
         CURRENT_PORTION_OF_LTD = 0.
         INTLTD = 0.
         INTPS = 0.
         SFPS = 0.
         SFLTD = 0.
         OTHER_INCOME = 0.
         INVESTMENT_IN_AFILLIATES = 0.
         INTEREST_INCOME = 0.
         LOANS_TO_AFILLIATES = 0.
         LTD_CASH_INTEREST = 0.
         PS_CASH_DIVIDEND = 0.
         LTD_PS_ISSUE_TAX_EXPENSE = 0.
         COMMON_STOCK_ISSUED_AMOUNT = 0.
         COMMON_STOCK_ISSUED_SHARES = 0.
         COMMON_STOCK_BUYBACK_AMOUNT = 0.
         COMMON_STOCK_BUYBACK_SHARES = 0.
         COMMON_STOCK_BALANCE = 0.
         COMMON_STOCK_SHARES_OUTSTANDING = 0.
         PURCHASED_SHARES_RE_ADJ = 0.
         CS_ISSUE_EXP_BAL = 0.
         CS_ISSUE_EXP_AMORT = 0.
         CS_ISSUE_TAX_EXPENSE = 0.
         LTD_NEW_ISSUE_PREM_DISC_AMOUNT = 0.
         PS_NEW_ISSUE_PREM_DISC_AMOUNT = 0.
         LTD_ISSUE_EXP_BAL = 0.
         LTD_ISSUE_EXP_AMORT = 0.
         PS_ISSUE_EXP_BAL = 0.
         PS_ISSUE_EXP_AMORT = 0.
         PS_PREM_BAL = 0.
         PS_PREM_AMORT = 0.
         LTD_PREM_BAL = 0.
         LTD_PREM_AMORT = 0.

         INVESTMENT_INCOME = 0.
         NOTES_PAYABLE = 0.
         NOTES_RECEIVABLE = 0.

         AINTPS = 0.
         AINTDB = 0.
         MIPS_INCOME_TAX_DEDUCTION = 0.
      RETURN


      ENTRY READ_DEBT_BASE_CASE

         NUM_OF_ASSET_CLASSES = NUM_OF_BASE_CASE_ASSET_CLASSES
         MAX_ASSET_CLASS_NUM =  MAX_BASE_CASE_ASSET_CLASS_NUM
         CALL SET_UP_BASE_CASE_DEBT_ARRAYS
         CALL OPEN_DB_BASE_CASE_FILE(INT2(10))
         READ(10) ISSPS,RETPS,ISSLTD,RETLTD,INTLTD,
     +            INTPS,SFPS,SFLTD,AINTPS,AINTDB,
     +            OTHER_INCOME,INVESTMENT_IN_AFILLIATES,
     +            INTEREST_INCOME,LOANS_TO_AFILLIATES,
     +            LTD_CASH_INTEREST,
     +            PS_CASH_DIVIDEND,
     +            PS_PREM_BAL,PS_PREM_AMORT,
     +            LTD_PREM_BAL,LTD_PREM_AMORT,
     +            PS_ISSUE_EXP_BAL,
     +            PS_ISSUE_EXP_AMORT,
     +            LTD_ISSUE_EXP_BAL,
     +            LTD_ISSUE_EXP_AMORT,
     +            LTD_PS_ISSUE_TAX_EXPENSE,
     +            MIPS_INCOME_TAX_DEDUCTION,
     +            COMMON_STOCK_ISSUED_AMOUNT,
     +            COMMON_STOCK_ISSUED_SHARES,
     +            COMMON_STOCK_BUYBACK_AMOUNT,
     +            COMMON_STOCK_BUYBACK_SHARES,
     +            COMMON_STOCK_BALANCE,
     +            COMMON_STOCK_SHARES_OUTSTANDING,
     +            PURCHASED_SHARES_RE_ADJ,
     +            CS_ISSUE_EXP_BAL,
     +            CS_ISSUE_EXP_AMORT,
     +            CS_ISSUE_TAX_EXPENSE,
     +            NOTES_PAYABLE,
     +            NOTES_RECEIVABLE,
     +            INVESTMENT_INCOME,
     +            LTD_NEW_ISSUE_PREM_DISC_AMOUNT,
     +            PS_NEW_ISSUE_PREM_DISC_AMOUNT,
     +            CURRENT_PORTION_OF_LTD
         CLOSE(10)
      RETURN


      ENTRY RETURN_MUNI_DEBT_INFO(R_LTDRET,R_LTDINT)


         DO I = 0, FINANCIAL_SIMULATION_YEARS
            R_LTDRET(I) = RETLTD(0,I,0)
            R_LTDINT(I) = INTLTD(0,I,0)
         ENDDO
      RETURN


      ENTRY MUNI_BONDS_ISSUED(RUN_YEAR,R_LTDISS,R_LTSINK)

         YR = RUN_YEAR - 1
         R_LTDISS = ISSLTD(0,YR,0)
         R_LTSINK = SFLTD(0,YR,0)
      RETURN

      ENTRY DEBT_INFO(RUN_YEAR,R_CLASS,R_CLASS_EXISTS,
     +                R_PS_DIV,R_PS_RETIRE,R_PSISS,
     +                R_LTDISS,R_LTD_INT,R_LTD_RETIRE,
     +                R_ANN_PS_DIV_LTD_INT,
     +                R_ANN_PS_DIV,
     +                R_ANN_LTD_INT,
     +                R_OTHER_INCOME,
     +                R_INVESTMENT_IN_AFILLIATES,
     +                R_INTEREST_INCOME,
     +                R_LOANS_TO_AFILLIATES,
     +                R_PS_PREM_BAL,
     +                R_PS_PREM_AMORT,
     +                R_LTD_PREM_BAL,
     +                R_LTD_PREM_AMORT,
     +                R_PS_ISSUE_EXP_BAL,
     +                R_PS_ISSUE_EXP_AMORT,
     +                R_LTD_ISSUE_EXP_BAL,
     +                R_LTD_ISSUE_EXP_AMORT,
     +                R_LTD_PS_ISSUE_TAX_EXPENSE,
     +                R_MIPS_INCOME_TAX_DEDUCTION,
     +                R_LTD_INTEREST_CASH_PAYMENTS,
     +                R_PS_DIVIDEND_CASH_PAYMENTS,
     +                R_CURRENT_LTD_RETIRE)

         R_CLASS_EXISTS = .FALSE.
         R_PSISS  = 0.
         R_LTDISS = 0.
         R_PS_PREM_BAL = 0.
         R_PS_PREM_AMORT = 0.
         R_LTD_PREM_BAL = 0.
         R_LTD_PREM_AMORT = 0.
         R_PS_ISSUE_EXP_BAL = 0.
         R_PS_ISSUE_EXP_AMORT = 0.
         R_LTD_ISSUE_EXP_BAL = 0.
         R_LTD_ISSUE_EXP_AMORT = 0.
         R_LTD_PS_ISSUE_TAX_EXPENSE = 0.
         R_MIPS_INCOME_TAX_DEDUCTION = 0.
         R_OTHER_INCOME = 0.
         R_INVESTMENT_IN_AFILLIATES = 0.
         R_LOANS_TO_AFILLIATES = 0.
         R_LTD_INTEREST_CASH_PAYMENTS = 0.
         R_PS_DIVIDEND_CASH_PAYMENTS = 0.
         R_CURRENT_LTD_RETIRE = 0.
         R_PERIOD = 0

         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
               YR = RUN_YEAR - 1
               R_CLASS_EXISTS = .TRUE.
               RETLTD(R_PERIOD,YR,ASSET_CLASS) =
     +                                   SUM(RETLTD(1:,YR,ASSET_CLASS))
               SFLTD(R_PERIOD,YR,ASSET_CLASS) =
     +                                    SUM(SFLTD(1:,YR,ASSET_CLASS))
               R_PSISS  = ISSPS(R_PERIOD,YR,ASSET_CLASS)
     +                + PS_NEW_ISSUE_PREM_DISC_AMOUNT(0,YR,ASSET_CLASS)
               R_PS_RETIRE = R_PS_RETIRE
     +                       + SFPS(R_PERIOD,YR,ASSET_CLASS)
     +                       + RETPS(R_PERIOD,YR,ASSET_CLASS)
               R_PS_DIV = R_PS_DIV + INTPS(R_PERIOD,YR,ASSET_CLASS)
               R_LTDISS = ISSLTD(R_PERIOD,YR,ASSET_CLASS)
     +               + LTD_NEW_ISSUE_PREM_DISC_AMOUNT(0,YR,ASSET_CLASS)
               R_LTD_RETIRE = R_LTD_RETIRE
     +                        + SFLTD(R_PERIOD,YR,ASSET_CLASS)
     +                        + RETLTD(R_PERIOD,YR,ASSET_CLASS)
               R_CURRENT_LTD_RETIRE = R_CURRENT_LTD_RETIRE

     +               + CURRENT_PORTION_OF_LTD(R_PERIOD,YR+1,ASSET_CLASS)

               R_LTD_INT = R_LTD_INT + INTLTD(R_PERIOD,YR,ASSET_CLASS)
               R_LTD_PS_ISSUE_TAX_EXPENSE =
     +                LTD_PS_ISSUE_TAX_EXPENSE(R_PERIOD,YR,ASSET_CLASS)
               R_OTHER_INCOME = OTHER_INCOME(R_PERIOD,YR,ASSET_CLASS)
               R_INVESTMENT_IN_AFILLIATES =
     +                INVESTMENT_IN_AFILLIATES(R_PERIOD,YR,ASSET_CLASS)
               R_INTEREST_INCOME =
     +                         INTEREST_INCOME(R_PERIOD,YR,ASSET_CLASS)
               R_LOANS_TO_AFILLIATES =
     +                     LOANS_TO_AFILLIATES(R_PERIOD,YR,ASSET_CLASS)
               R_LTD_INTEREST_CASH_PAYMENTS =
     +                       LTD_CASH_INTEREST(R_PERIOD,YR,ASSET_CLASS)
               R_PS_DIVIDEND_CASH_PAYMENTS =
     +                        PS_CASH_DIVIDEND(R_PERIOD,YR,ASSET_CLASS)
               R_LTD_ISSUE_EXP_BAL =
     +                             LTD_ISSUE_EXP_BAL(12,YR,ASSET_CLASS)
               R_LTD_ISSUE_EXP_AMORT =
     +                     LTD_ISSUE_EXP_AMORT(R_PERIOD,YR,ASSET_CLASS)
               R_PS_ISSUE_EXP_BAL =
     +                         PS_ISSUE_EXP_BAL(12,YR,ASSET_CLASS)
               R_PS_ISSUE_EXP_AMORT =
     +                      PS_ISSUE_EXP_AMORT(R_PERIOD,YR,ASSET_CLASS)
               R_PS_PREM_BAL = PS_PREM_BAL(12,YR,ASSET_CLASS)
               R_PS_PREM_AMORT = PS_PREM_AMORT(R_PERIOD,YR,ASSET_CLASS)

               R_LTD_PREM_BAL =
     +                  LTD_NEW_ISSUE_PREM_DISC_AMOUNT(0,YR,ASSET_CLASS)
               R_LTD_PREM_AMORT=LTD_PREM_AMORT(R_PERIOD,YR,ASSET_CLASS)

               IF(R_PERIOD == 0) THEN
                  R_ANN_PS_DIV_LTD_INT = R_ANN_PS_DIV_LTD_INT +
     +                                   AINTPS(YR,ASSET_CLASS) +
     +                                   AINTDB(YR,ASSET_CLASS)
                  R_ANN_PS_DIV = R_ANN_PS_DIV +
     +                                      AINTPS(YR,ASSET_CLASS)
                  R_ANN_LTD_INT = R_ANN_LTD_INT +
     +                                      AINTDB(YR,ASSET_CLASS)
                  R_MIPS_INCOME_TAX_DEDUCTION =
     +                   MIPS_INCOME_TAX_DEDUCTION(YR,ASSET_CLASS)
               ELSE
                  R_ANN_PS_DIV_LTD_INT = R_ANN_PS_DIV_LTD_INT +
     +                                (AINTPS(YR,ASSET_CLASS) +
     +                                 AINTDB(YR,ASSET_CLASS))/12.
                  R_ANN_PS_DIV = R_ANN_PS_DIV +
     +                                      AINTPS(YR,ASSET_CLASS)/12.
                  R_ANN_LTD_INT = R_ANN_LTD_INT +
     +                                      AINTDB(YR,ASSET_CLASS)/12.
                  R_MIPS_INCOME_TAX_DEDUCTION =
     +                    MIPS_INCOME_TAX_DEDUCTION(YR,ASSET_CLASS)/12.
               ENDIF
            ENDIF
         ENDIF
      RETURN

      ENTRY DEBT_FILE_INVESTMENT_INFO(RUN_YEAR,R_CLASS,
     +                                R_INVESTMENTS_MADE,
     +                                R_INVESTMENTS_SOLD,
     +                                R_INVESTMENT_INCOME,
     +                                R_DIVIDEND_70_INCOME,
     +                                R_NOTES_RECEIVABLE_MADE,
     +                                R_NOTES_RECEIVABLE_CLEARED,
     +                                R_NOTES_PAYABLE_MADE,
     +                                R_NOTES_PAYABLE_CLEARED,
     +                                R_NOTES_PAYABLE_INTEREST,
     +                                R_INTRA_INVESTMENT_INCOME,
     +                                R_NOTES_PAYABLE_CASH_INTEREST,
     +                                R_NOTES_RECEIVALBE_CASH_INTEREST)

         R_INVESTMENTS_MADE = 0.
         R_INVESTMENTS_SOLD = 0.
         R_INVESTMENT_INCOME = 0.
         R_DIVIDEND_70_INCOME = 0.
         R_NOTES_RECEIVABLE_MADE = 0.
         R_NOTES_RECEIVABLE_CLEARED = 0.
         R_NOTES_PAYABLE_MADE = 0.
         R_NOTES_PAYABLE_CLEARED = 0.
         R_NOTES_PAYABLE_INTEREST = 0.
         R_INTRA_INVESTMENT_INCOME = 0.
         R_NOTES_PAYABLE_CASH_INTEREST = 0.
         R_NOTES_RECEIVALBE_CASH_INTEREST = 0.

         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
               YR = RUN_YEAR - 1
               R_INVESTMENTS_MADE =
     +                    INVESTMENT_INCOME(0,YR,ASSET_CLASS,Additions)
               R_INVESTMENTS_SOLD =
     +                   INVESTMENT_INCOME(0,YR,ASSET_CLASS,Reductions)
               R_INVESTMENT_INCOME =
     +                   INVESTMENT_INCOME(0,YR,ASSET_CLASS,Dividend_70)
     +                   + INVESTMENT_INCOME(0,YR,ASSET_CLASS,Dividend)
     +                   + INVESTMENT_INCOME(0,YR,ASSET_CLASS,
     +                                                Interest_Earnings)
     +                   + NOTES_RECEIVABLE(0,YR,ASSET_CLASS,
     +                                             Interest_Earnings)
               R_INTRA_INVESTMENT_INCOME =
     +                             NOTES_RECEIVABLE(0,YR,ASSET_CLASS,
     +                                  Intra_Company_Interest_Earnings)
               R_DIVIDEND_70_INCOME =
     +                   INVESTMENT_INCOME(0,YR,ASSET_CLASS,Dividend_70)

               R_NOTES_RECEIVABLE_MADE =
     +                     NOTES_RECEIVABLE(0,YR,ASSET_CLASS,Additions)
               R_NOTES_RECEIVABLE_CLEARED =
     +                    NOTES_RECEIVABLE(0,YR,ASSET_CLASS,Reductions)
               R_NOTES_RECEIVALBE_CASH_INTEREST =
     +                     NOTES_RECEIVABLE(0,YR,ASSET_CLASS,

     +                                           Cash_Interest_Earnings)

               R_NOTES_PAYABLE_MADE =
     +                        NOTES_PAYABLE(0,YR,ASSET_CLASS,Additions)
               R_NOTES_PAYABLE_CLEARED =
     +                       NOTES_PAYABLE(0,YR,ASSET_CLASS,Reductions)
               R_NOTES_PAYABLE_INTEREST =

     +                 NOTES_PAYABLE(0,YR,ASSET_CLASS,Interest_Payments)
               R_NOTES_PAYABLE_CASH_INTEREST =
     +            NOTES_PAYABLE(0,YR,ASSET_CLASS,Cash_Interest_Payments)
            ENDIF
         ENDIF

      RETURN


      ENTRY DEBT_COMMON_STOCK_INFO(RUN_YEAR,R_CLASS,
     +                             R_COMMON_STOCK_ISSUED_AMOUNT,
     +                             R_COMMON_STOCK_ISSUED_SHARES,
     +                             R_COMMON_STOCK_BUYBACK_AMOUNT,
     +                             R_COMMON_STOCK_BUYBACK_SHARES,
     +                             R_COMMON_STOCK_BALANCE,
     +                             R_COMMON_SHARES_OUTSTANDING,
     +                             R_CS_ISSUE_EXP_BAL,
     +                             R_CS_ISSUE_EXP_AMORT,
     +                             R_CS_ISSUE_EXP_THIS_PERIOD,
     +                             R_NET_MONTHLY_SHARES,
     +                             R_RETAINED_EARNINGS_ADJ,
     +                             R_COMMON_STOCK_ISSUED_2_EQUITY,
     +                             R_COMMON_STOCK_BUY_FROM_EQUITY)

         R_NET_MONTHLY_SHARES = 0.
         R_RETAINED_EARNINGS_ADJ = 0.
         MO = 0

         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
               YR = RUN_YEAR - 1

               R_COMMON_STOCK_ISSUED_AMOUNT =
     +             COMMON_STOCK_ISSUED_AMOUNT(MO,YR,ASSET_CLASS,Parent)
               R_COMMON_STOCK_ISSUED_SHARES =
     +            COMMON_STOCK_ISSUED_SHARES(MO,YR,ASSET_CLASS,Parent)
     +           + COMMON_STOCK_ISSUED_SHARES(MO,YR,ASSET_CLASS,Equity)
               R_COMMON_STOCK_BUYBACK_AMOUNT =
     +            COMMON_STOCK_BUYBACK_AMOUNT(MO,YR,ASSET_CLASS,Parent)
               R_COMMON_STOCK_BUYBACK_SHARES =
     +           COMMON_STOCK_BUYBACK_SHARES(MO,YR,ASSET_CLASS,Parent)
     +           + COMMON_STOCK_BUYBACK_SHARES(MO,YR,ASSET_CLASS,Equity)

               R_COMMON_STOCK_ISSUED_2_EQUITY =
     +              COMMON_STOCK_ISSUED_AMOUNT(MO,YR,ASSET_CLASS,Equity)
               R_COMMON_STOCK_BUY_FROM_EQUITY =
     +             COMMON_STOCK_BUYBACK_AMOUNT(MO,YR,ASSET_CLASS,Equity)

               R_RETAINED_EARNINGS_ADJ =
     +                       PURCHASED_SHARES_RE_ADJ(MO,YR,ASSET_CLASS)
               R_COMMON_STOCK_BALANCE =
     +                          COMMON_STOCK_BALANCE(12,YR,ASSET_CLASS)
               R_COMMON_SHARES_OUTSTANDING =
     +               COMMON_STOCK_SHARES_OUTSTANDING(12,YR,ASSET_CLASS)
               R_CS_ISSUE_EXP_BAL = CS_ISSUE_EXP_BAL(12,YR,ASSET_CLASS)
               R_CS_ISSUE_EXP_THIS_PERIOD =
     +                           CS_ISSUE_TAX_EXPENSE(0,YR,ASSET_CLASS)
               R_CS_ISSUE_EXP_AMORT =
     +                            CS_ISSUE_EXP_AMORT(MO,YR,ASSET_CLASS)
               IF(R_COMMON_STOCK_ISSUED_SHARES /= 0. .OR.
     +                        R_COMMON_STOCK_BUYBACK_SHARES /= 0.) THEN
                  DO MO = 1, 12
                     R_NET_MONTHLY_SHARES(MO) =
     +                     COMMON_STOCK_ISSUED_SHARES(MO,YR,
     +                                              ASSET_CLASS,Equity)
     +                     + COMMON_STOCK_ISSUED_SHARES(MO,YR,
     +                                              ASSET_CLASS,Parent)
     +                     - COMMON_STOCK_BUYBACK_SHARES(MO,YR,
     +                                              ASSET_CLASS,Equity)
     +                     - COMMON_STOCK_BUYBACK_SHARES(MO,YR,
     +                                              ASSET_CLASS,Parent)
                  ENDDO
               ENDIF
            ENDIF
         ENDIF
      RETURN

      ENTRY DEBT_COMMON_STOCK_BY_INFO(R_CLASS,
     +                                R_COMMON_STOCK_BALANCE,
     +                                R_COMMON_SHARES_OUTSTANDING,
     +                                R_CS_ISSUE_EXP_BAL)

         R_CS_ISSUE_EXP_BAL = 0.
         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
               R_COMMON_STOCK_BALANCE = R_COMMON_STOCK_BALANCE
     +                          + COMMON_STOCK_BALANCE(0,1,ASSET_CLASS)
              R_COMMON_SHARES_OUTSTANDING = R_COMMON_SHARES_OUTSTANDING
     +               + COMMON_STOCK_SHARES_OUTSTANDING(0,1,ASSET_CLASS)
               R_CS_ISSUE_EXP_BAL = CS_ISSUE_EXP_BAL(0,1,ASSET_CLASS)
            ENDIF
         ENDIF
      RETURN

      ENTRY MONTHLY_NOTES_MADE(RUN_YEAR,R_CLASS,
     +                         R_NOTES_PAYABLE_MONTHLY_MADE,
     +                         R_NOTES_RECEIVABLE_MONTHLY_MADE,
     +                         R_UNAMORTIZED_INTEREST_BALANCE)

         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_CLASS <= 0) THEN
               ASSET_CLASS = R_CLASS
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS >= -1) THEN
               YR = RUN_YEAR ! - 1
               R_NOTES_PAYABLE_MONTHLY_MADE(:) =
     +                      R_NOTES_PAYABLE_MONTHLY_MADE(:)
     +                     + NOTES_PAYABLE(1:,YR,ASSET_CLASS,Additions)
               R_NOTES_RECEIVABLE_MONTHLY_MADE(:) =
     +                   R_NOTES_RECEIVABLE_MONTHLY_MADE(:)
     +                  + NOTES_RECEIVABLE(1:,YR,ASSET_CLASS,Additions)
               R_UNAMORTIZED_INTEREST_BALANCE(:) =
     +                             R_UNAMORTIZED_INTEREST_BALANCE(:)
     +                            + LTD_ISSUE_EXP_BAL(:,YR,ASSET_CLASS)
     +                             + PS_ISSUE_EXP_BAL(:,YR,ASSET_CLASS)

            ENDIF
         ENDIF
      RETURN


      ENTRY DEBT_MONTHLY_TAX_INFO(RUN_YEAR,R_CLASS,TAX_VARIABLES)


         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
               YR = RUN_YEAR ! - 1
               DO MO = 0, 12

                  TAX_VARIABLES(MO,mty_ltd_ps_tax_issue_expense) =
     +                       LTD_PS_ISSUE_TAX_EXPENSE(MO,YR,ASSET_CLASS)

     +                       + CS_ISSUE_TAX_EXPENSE(MO,YR,ASSET_CLASS)
               ENDDO
            ENDIF
         ENDIF

      RETURN

      ENTRY CURRENT_PORTION_OF_LTD_INFO(RUN_YEAR,R_CLASS,
     +                                  R_CURRENT_PORTION_OF_LTD)


         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
               YR = RUN_YEAR ! - 1
               R_CURRENT_PORTION_OF_LTD(:) =
     +                         CURRENT_PORTION_OF_LTD(:,YR,ASSET_CLASS)
            ENDIF
         ENDIF
      RETURN

      ENTRY DEBT_MONTHLY_INCOME_INFO(RUN_YEAR,R_CLASS,
     +                               INCOME_VARIABLES,
     +                               CASH_VARIABLES)


         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_CLASS == 0 .OR. R_CLASS == -1) THEN
               ASSET_CLASS = R_CLASS
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS >= -1) THEN
               YR = RUN_YEAR ! - 1

               INCOME_VARIABLES(:,BTL_Monthly_Total_Other_Income) =
     +             INCOME_VARIABLES(:,BTL_Monthly_Total_Other_Income)
     +                + OTHER_INCOME(:,YR,ASSET_CLASS)
               INCOME_VARIABLES(:,BTL_Monthly_Interest_Income) =
     +                   INCOME_VARIABLES(:,BTL_Monthly_Interest_Income)
     +                   + INTEREST_INCOME(:,YR,ASSET_CLASS)
               INCOME_VARIABLES(:,Monthly_LTD_Total_Interest) =
     +                    INCOME_VARIABLES(:,Monthly_LTD_Total_Interest)
     +                    + INTLTD(:,YR,ASSET_CLASS)
               INCOME_VARIABLES(:,Monthly_LTD_Booked_Interest) =
                         ! used for elimination purposes
     +                    INCOME_VARIABLES(:,Monthly_LTD_Total_Interest)
               INCOME_VARIABLES(:,Monthly_Total_PS_Dividends) =
     +                    INCOME_VARIABLES(:,Monthly_Total_PS_Dividends)
     +                    + INTPS(:,YR,ASSET_CLASS)
! Monthly Debt File Interest Amort is now mt_dt_file_intrst_amrt
               INCOME_VARIABLES(:,mt_dt_file_intrst_amrt) =
     +                    INCOME_VARIABLES(:,
     +                                 mt_dt_file_intrst_amrt)
     +                    + LTD_ISSUE_EXP_AMORT(:,YR,ASSET_CLASS)
     +                    - LTD_PREM_AMORT(:,YR,ASSET_CLASS)
               INCOME_VARIABLES(:,Monthly_LTD_Premium_Amort) =
     +                     INCOME_VARIABLES(:,Monthly_LTD_Premium_Amort)
     +                     + LTD_PREM_AMORT(:,YR,ASSET_CLASS)
               INCOME_VARIABLES(:,Monthly_PS_Amort_Dividends) =
     +                    INCOME_VARIABLES(:,Monthly_PS_Amort_Dividends)
     +                    + PS_ISSUE_EXP_AMORT(:,YR,ASSET_CLASS)
     +                    - PS_PREM_AMORT(:,YR,ASSET_CLASS)
               INCOME_VARIABLES(:,Monthly_PS_Premium_Amort) =
     +                      INCOME_VARIABLES(:,Monthly_PS_Premium_Amort)
     +                      + PS_PREM_AMORT(:,YR,ASSET_CLASS)

               INCOME_VARIABLES(:,Monthly_Issue_Expense_Amorts) =
     +                  INCOME_VARIABLES(:,Monthly_Issue_Expense_Amorts)
     +                  + LTD_ISSUE_EXP_AMORT(:,YR,ASSET_CLASS)
     +                  + PS_ISSUE_EXP_AMORT(:,YR,ASSET_CLASS)
     +                  + CS_ISSUE_EXP_AMORT(:,YR,ASSET_CLASS)

! Merge2Issue left side used monthly_common_stock_issue_amort, right
! side used mthy_common_stock_issue_amort. Going with right
! side choice
               INCOME_VARIABLES(:,mthy_common_stock_issue_amort)=
     +              INCOME_VARIABLES(:,mthy_common_stock_issue_amort)
     +              + CS_ISSUE_EXP_AMORT(:,YR,ASSET_CLASS)
               INCOME_VARIABLES(:,Monthly_Investment_Earnings) =
     +                 INCOME_VARIABLES(:,Monthly_Investment_Earnings)
     +                 + INVESTMENT_INCOME(:,YR,ASSET_CLASS,Dividend_70)
     +                 + INVESTMENT_INCOME(:,YR,ASSET_CLASS,Dividend)
     +                 + INVESTMENT_INCOME(:,YR,ASSET_CLASS,
     +                                                Interest_Earnings)

               INCOME_VARIABLES(:,Monthly_Dividend_70_Earnings) =
     +                  INCOME_VARIABLES(:,Monthly_Dividend_70_Earnings)
     +                  +INVESTMENT_INCOME(:,YR,ASSET_CLASS,Dividend_70)
               INCOME_VARIABLES(:,mty_rglr_dvd_income) =
! was monthly regular dividend income
     +               INCOME_VARIABLES(:,mty_rglr_dvd_income)
     +               + INVESTMENT_INCOME(:,YR,ASSET_CLASS,Dividend)
! Was Monthly Interest on Notes Payable ->
               INCOME_VARIABLES(:,mty_intst_on_nts_pyble)=
     +             INCOME_VARIABLES(:,mty_intst_on_nts_pyble)
     +             + NOTES_PAYABLE(:,YR,ASSET_CLASS,Interest_Payments)
! was Monthly Debt File LInvestment Income ->
               INCOME_VARIABLES(:,mt_dbt_file_linvst_income)=
     +               INCOME_VARIABLES(:,
     +                             mt_dbt_file_linvst_income)
     +               + INVESTMENT_INCOME(:,YR,ASSET_CLASS,
     +                                                Interest_Earnings)


               INCOME_VARIABLES(:,monthly_notes_receivable_income) =
     +              INCOME_VARIABLES(:,monthly_notes_receivable_income)
     +               + NOTES_RECEIVABLE(:,YR,ASSET_CLASS,
     +                                                Interest_Earnings)

!
! CASH ITEMS
!
               CASH_VARIABLES(:,cash_ps_issued) =
     +                 CASH_VARIABLES(:,cash_ps_issued)
     +                 + ISSPS(:,YR,ASSET_CLASS)
     +                 + PS_NEW_ISSUE_PREM_DISC_AMOUNT(:,YR,ASSET_CLASS)
               CASH_VARIABLES(:,cash_ps_retirements) =
     +                             CASH_VARIABLES(:,cash_ps_retirements)

     +                             + SFPS(:,YR,ASSET_CLASS)
     +                             + RETPS(:,YR,ASSET_CLASS)
               CASH_VARIABLES(:,cash_ltd_issued) =
     +                CASH_VARIABLES(:,cash_ltd_issued)
     +                + ISSLTD(:,YR,ASSET_CLASS)
     +                + LTD_NEW_ISSUE_PREM_DISC_AMOUNT(:,YR,ASSET_CLASS)
               CASH_VARIABLES(:,cash_ltd_retirements) =
     +                            CASH_VARIABLES(:,cash_ltd_retirements)
     +                            + SFLTD(:,YR,ASSET_CLASS)
     +                            + RETLTD(:,YR,ASSET_CLASS)
               CASH_VARIABLES(:,cash_investment_affiliates) =
     +                     CASH_VARIABLES(:,cash_investment_affiliates)
     +                     + INVESTMENT_IN_AFILLIATES(:,YR,ASSET_CLASS)
               CASH_VARIABLES(:,cash_loads_2_affiliates) =
     +                        CASH_VARIABLES(:,cash_loads_2_affiliates)
     +                        + LOANS_TO_AFILLIATES(:,YR,ASSET_CLASS)
               CASH_VARIABLES(:,cash_ps_dividends) =
     +                              CASH_VARIABLES(:,cash_ps_dividends)
     +                              + PS_CASH_DIVIDEND(:,YR,ASSET_CLASS)
               CASH_VARIABLES(:,cash_ltd_interest) =
     +                             CASH_VARIABLES(:,cash_ltd_interest)
     +                             + LTD_CASH_INTEREST(:,YR,ASSET_CLASS)
               CASH_VARIABLES(:,cash_ltd_ps_issue_expense) =
     +                      CASH_VARIABLES(:,cash_ltd_ps_issue_expense)
     +                      + LTD_PS_ISSUE_TAX_EXPENSE(:,YR,ASSET_CLASS)
               CASH_VARIABLES(:,cash_common_issue_expense) =
     +                       CASH_VARIABLES(:,cash_common_issue_expense)
     +                       + CS_ISSUE_TAX_EXPENSE(:,YR,ASSET_CLASS)
               CASH_VARIABLES(:,cash_common_stock_issued) =
     +             CASH_VARIABLES(:,cash_common_stock_issued)
     +             + COMMON_STOCK_ISSUED_AMOUNT(:,YR,ASSET_CLASS,Parent)
               CASH_VARIABLES(:,cash_common_stock_buyback) =
     +            CASH_VARIABLES(:,cash_common_stock_buyback)
     +            + COMMON_STOCK_BUYBACK_AMOUNT(:,YR,ASSET_CLASS,Parent)
               CASH_VARIABLES(:,common_shares_issued) =
     +             CASH_VARIABLES(:,common_shares_issued)
     +             + COMMON_STOCK_ISSUED_SHARES(:,YR,ASSET_CLASS,Parent)
     +             - COMMON_STOCK_BUYBACK_SHARES(:,YR,
     +                                               ASSET_CLASS,Parent)
! Was Cash From Repayment Of Notes Issued
               CASH_VARIABLES(:,cash_fm_repaid_issued_notes) =
     +                   CASH_VARIABLES(:,
     +                              cash_fm_repaid_issued_notes)
     +                   + NOTES_RECEIVABLE(:,YR,ASSET_CLASS,Reductions)
               CASH_VARIABLES(:,cash_4_new_notes_issued) =
     +                   CASH_VARIABLES(:,cash_4_new_notes_issued)
     +                   +  NOTES_RECEIVABLE(:,YR,ASSET_CLASS,Additions)
               CASH_VARIABLES(:,csh_4_redeeming_notes_owed) =
     +                    CASH_VARIABLES(:,csh_4_redeeming_notes_owed)
     +                    + NOTES_PAYABLE(:,YR,ASSET_CLASS,Reductions)
               CASH_VARIABLES(:,cash_fm_notes_issd_by_othrs) =
     +                CASH_VARIABLES(:,cash_fm_notes_issd_by_othrs)
     +                + NOTES_PAYABLE(:,YR,ASSET_CLASS,Additions)

               CASH_VARIABLES(:,cash_investmt_divnd_earngs) =
     +               CASH_VARIABLES(:,cash_investmt_divnd_earngs)
     +               + INVESTMENT_INCOME(:,YR,ASSET_CLASS,
     +                                           Cash_Dividend_Earnings)
               CASH_VARIABLES(:,cash_invest_interest_ernings) =
     +               CASH_VARIABLES(:,cash_invest_interest_ernings)
     +               + INVESTMENT_INCOME(:,YR,ASSET_CLASS,
     +                                           Cash_Interest_Earnings)
     +               + NOTES_RECEIVABLE(:,YR,ASSET_CLASS,
     +                                           Cash_Interest_Earnings)
               CASH_VARIABLES(:,Cash_Interest_on_Notes_Payable) =
     +                  CASH_VARIABLES(:,Cash_Interest_on_Notes_Payable)
     +                  + NOTES_PAYABLE(:,YR,ASSET_CLASS,
     +                                           Cash_Interest_Payments)
               CASH_VARIABLES(:,cash_change_debt_investments) =
     +                  CASH_VARIABLES(:,cash_change_debt_investments)
     +                  + INVESTMENT_INCOME(:,YR,ASSET_CLASS,Additions)
     +                  - INVESTMENT_INCOME(:,YR,ASSET_CLASS,Reductions)
               CASH_VARIABLES(:,cash_net_investments) =
     +                      CASH_VARIABLES(:,cash_net_investments)
     +                      + LOANS_TO_AFILLIATES(:,YR,ASSET_CLASS)
     +                      + INVESTMENT_IN_AFILLIATES(:,YR,ASSET_CLASS)


            ENDIF
         ENDIF
      RETURN

      ENTRY MONTHLY_LTD_LAST_HALF(RUN_YEAR,R_CLASS,R_LTD_INT,R_LTDISS)

         LTD_CASH_CARRY_OVER = 0.
         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
               YR = RUN_YEAR ! - 1
               INTLTD(0,YR,ASSET_CLASS) = INTLTD(0,YR,ASSET_CLASS) +
     +                                                     R_LTD_INT/2.
               ISSLTD(0,YR,ASSET_CLASS) = ISSLTD(0,YR,ASSET_CLASS) +
     +                                                         R_LTDISS
               ISSLTD(7,YR,ASSET_CLASS) = ISSLTD(7,YR,ASSET_CLASS) +
     +                                                         R_LTDISS
               DO MO = 7, 12
                  IF(MO == 10) THEN
                     LTD_CASH_INTEREST(MO,YR,ASSET_CLASS) =
     +                           LTD_CASH_INTEREST(MO,YR,ASSET_CLASS) +
     +                            LTD_CASH_CARRY_OVER
                     LTD_CASH_INTEREST(0,YR,ASSET_CLASS) =
     +                            LTD_CASH_INTEREST(0,YR,ASSET_CLASS) +
     +                            LTD_CASH_CARRY_OVER
                     LTD_CASH_CARRY_OVER = 0.
                  ENDIF
                 INTLTD(MO,YR,ASSET_CLASS) = INTLTD(MO,YR,ASSET_CLASS)+
     +                                                    R_LTD_INT/12.
                  LTD_CASH_CARRY_OVER=LTD_CASH_CARRY_OVER+R_LTD_INT/12.

               ENDDO
            ENDIF
         ENDIF
      RETURN


      ENTRY MONTHLY_LTD_FIRST_HALF(RUN_YEAR,R_CLASS,R_LTD_INT,
     +                                                     R_LTD_RETIRE)


         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
               YR = RUN_YEAR ! - 1
               INTLTD(0,YR,ASSET_CLASS) = INTLTD(0,YR,ASSET_CLASS) +
     +                                                     R_LTD_INT/2.
               RETLTD(0,YR,ASSET_CLASS) = RETLTD(0,YR,ASSET_CLASS) +
     +                                                     R_LTD_RETIRE
               RETLTD(7,YR,ASSET_CLASS) = RETLTD(7,YR,ASSET_CLASS) +
     +                                                     R_LTD_RETIRE
               DO MO = 1, 6
                  IF(MO == 1 .OR. MO == 4) THEN
                     LTD_CASH_INTEREST(MO,YR,ASSET_CLASS) =
     +                           LTD_CASH_INTEREST(MO,YR,ASSET_CLASS) +
     +                            LTD_CASH_CARRY_OVER
                     LTD_CASH_INTEREST(0,YR,ASSET_CLASS) =
     +                            LTD_CASH_INTEREST(0,YR,ASSET_CLASS) +
     +                            LTD_CASH_CARRY_OVER
                     LTD_CASH_CARRY_OVER = 0.
                  ENDIF
                 INTLTD(MO,YR,ASSET_CLASS) = INTLTD(MO,YR,ASSET_CLASS)+
     +                                                    R_LTD_INT/12.
                  LTD_CASH_CARRY_OVER=LTD_CASH_CARRY_OVER+R_LTD_INT/12.
               ENDDO
            ENDIF
         ENDIF
      RETURN

      ENTRY MONTHLY_LTD_FULL_YEAR(RUN_YEAR,R_CLASS,R_LTD_BAL,R_LTD_RATE,
     +                                                     R_LTD_RETIRE)


         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
               YR = RUN_YEAR ! - 1
               LTD_BAL = R_LTD_BAL
               DO MO = 1, 12
                 IF(MO ==1 .OR. MO ==4 .OR. MO == 7 .OR. MO == 10) THEN
                     LTD_CASH_INTEREST(MO,YR,ASSET_CLASS) =
     +                           LTD_CASH_INTEREST(MO,YR,ASSET_CLASS) +
     +                            LTD_CASH_CARRY_OVER
                     LTD_CASH_INTEREST(0,YR,ASSET_CLASS) =
     +                           LTD_CASH_INTEREST(0,YR,ASSET_CLASS) +
     +                            LTD_CASH_CARRY_OVER
                     LTD_CASH_CARRY_OVER = 0.
                  ENDIF
                  IF(MO == 7) THEN
                     LTD_BAL = LTD_BAL - R_LTD_RETIRE
                    SFLTD(7,YR,ASSET_CLASS) = SFLTD(7,YR,ASSET_CLASS) +
     +                                                     R_LTD_RETIRE
                    SFLTD(0,YR,ASSET_CLASS) = SFLTD(0,YR,ASSET_CLASS) +
     +                                                     R_LTD_RETIRE
                  ENDIF
                  MONTHLY_INTEREST = LTD_BAL*R_LTD_RATE/12.
                 INTLTD(MO,YR,ASSET_CLASS) = INTLTD(MO,YR,ASSET_CLASS)+
     +                                                 MONTHLY_INTEREST
                 INTLTD(0,YR,ASSET_CLASS) = INTLTD(0,YR,ASSET_CLASS) +
     +                                                 MONTHLY_INTEREST
                  LTD_CASH_CARRY_OVER = LTD_CASH_CARRY_OVER +
     +                                                 MONTHLY_INTEREST
               ENDDO
            ENDIF
         ENDIF
      RETURN

      ENTRY MONTHLY_LTD_ADDITIONS_ALL_YEAR(R_ISSMO,RUN_YEAR,R_CLASS,
     +                                     R_LTD_BAL,
     +                                     R_LTD_RATE,
     +                                     R_LTD_RETIRE,
     +                                     R_LAG_SINKING_FUND_YEARS,
     +                                     R_ANNINT,
     +                                     ANN_LTD_INT,
     +                                     R_LTD_LIFE)

         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
               YR = RUN_YEAR ! - 1
               PAID_WHEN = 'S'
               LTD_BAL = R_LTD_BAL
               ISSMO = R_ISSMO
               LAG_SINKING_FUND_YEARS = R_LAG_SINKING_FUND_YEARS
               MONTHLY_INTEREST = R_LTD_RATE * LTD_BAL/12.
               INTLTD(0,YR,ASSET_CLASS) = INTLTD(0,YR,ASSET_CLASS)
     +                                    + MONTHLY_INTEREST
              INTLTD(ISSMO,YR,ASSET_CLASS)=INTLTD(ISSMO,YR,ASSET_CLASS)
     +                                      + MONTHLY_INTEREST
               ISSLTD(0,YR,ASSET_CLASS) = ISSLTD(0,YR,ASSET_CLASS) +
     +                                                          LTD_BAL
               ISSLTD(ISSMO,YR,ASSET_CLASS) =
     +                           ISSLTD(ISSMO,YR,ASSET_CLASS) + LTD_BAL
               START_MO = ISSMO + 1
               DO YR = RUN_YEAR, MIN(RUN_YEAR+R_LTD_LIFE,

     +                                       FINANCIAL_SIMULATION_YEARS)

                  DO MO = START_MO, 12
                     IF((PAID_WHEN == 'S') .AND.
     +                  ((MO == ISSMO) .OR.
     +                          MO == ISSMO+6 .OR. MO == ISSMO-6)) THEN
                        LTD_CASH_INTEREST(MO,YR,ASSET_CLASS) =
     +                           LTD_CASH_INTEREST(MO,YR,ASSET_CLASS) +
     +                            LTD_CASH_CARRY_OVER
                        LTD_CASH_INTEREST(0,YR,ASSET_CLASS) =
     +                            LTD_CASH_INTEREST(0,YR,ASSET_CLASS) +
     +                            LTD_CASH_CARRY_OVER
                        LTD_CASH_CARRY_OVER = 0.
                     ENDIF
                     IF(MO == ISSMO .AND.
     +                   YR >= RUN_YEAR + LAG_SINKING_FUND_YEARS) THEN
                        IF(YR == RUN_YEAR + R_LTD_LIFE) THEN
                           R_LTD_RETIRE = LTD_BAL
                           LTD_BAL = 0.
                        ELSE
                           LTD_BAL = LTD_BAL - R_LTD_RETIRE
                        ENDIF
                        SFLTD(ISSMO,YR,ASSET_CLASS) =
     +                                      SFLTD(ISSMO,YR,ASSET_CLASS)
     +                                       + R_LTD_RETIRE
                        SFLTD(0,YR,ASSET_CLASS) =
     +                                          SFLTD(0,YR,ASSET_CLASS)
     +                                           + R_LTD_RETIRE
                        IF(YR == RUN_YEAR + R_LTD_LIFE) EXIT
                     ENDIF
                     MONTHLY_INTEREST = LTD_BAL*R_LTD_RATE/12.
                    INTLTD(MO,YR,ASSET_CLASS)=INTLTD(MO,YR,ASSET_CLASS)
     +                                         + MONTHLY_INTEREST
                    INTLTD(0,YR,ASSET_CLASS) = INTLTD(0,YR,ASSET_CLASS)
     +                                          + MONTHLY_INTEREST
                     LTD_CASH_CARRY_OVER = LTD_CASH_CARRY_OVER +
     +                                                 MONTHLY_INTEREST
                  ENDDO
                  R_ANNINT(YR) = R_ANNINT(YR) + LTD_BAL * R_LTD_RATE
                  ANN_LTD_INT(YR) = ANN_LTD_INT(YR)
     +                              + LTD_BAL * R_LTD_RATE
                  START_MO = 1
                  IF(YR == RUN_YEAR + R_LTD_LIFE) EXIT
               ENDDO
            ENDIF
         ENDIF
      RETURN

      ENTRY MONTHLY_MTG_ADDITIONS_ALL_YEAR(R_ISSMO,RUN_YEAR,R_CLASS,
     +                                     R_LTD_BAL,
     +                                     R_LTD_RATE,
     +                                     R_ANNINT,
     +                                     ANN_LTD_INT,
     +                                     R_LTD_LIFE)

         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
               PAID_WHEN = 'Q'
               YR = RUN_YEAR ! - 1
               LTD_BAL = R_LTD_BAL
               ISSMO = R_ISSMO
               MTGLIF = MAX(1,INT(R_LTD_LIFE))
               IF(PAID_WHEN == 'M') PAY_PERIODS_PER_YR = 12.
               IF(PAID_WHEN == 'S') PAY_PERIODS_PER_YR = 2.
               IF(PAID_WHEN == 'Q') PAY_PERIODS_PER_YR = 4.
               IF(PAID_WHEN == 'A') PAY_PERIODS_PER_YR = 1.

               IF(R_LTD_RATE /= 0.) THEN
                  PIBIEN = (LTD_BAL * R_LTD_RATE/PAY_PERIODS_PER_YR)/
     +               (1. - (1./(1. + R_LTD_RATE/PAY_PERIODS_PER_YR)**
     +                               (INT(PAY_PERIODS_PER_YR)*MTGLIF)))
               ELSE
                  PIBIEN = LTD_BAL/MAX(1.,PAY_PERIODS_PER_YR*MTGLIF)
               ENDIF

               ISSLTD(0,YR,ASSET_CLASS) = ISSLTD(0,YR,ASSET_CLASS)
     +                                    + LTD_BAL
               ISSLTD(ISSMO,YR,ASSET_CLASS) =
     +                           ISSLTD(ISSMO,YR,ASSET_CLASS) + LTD_BAL
               MONTHLY_INTEREST = R_LTD_RATE * LTD_BAL/12.
              INTLTD(ISSMO,YR,ASSET_CLASS)=INTLTD(ISSMO,YR,ASSET_CLASS)
     +                                      + MONTHLY_INTEREST
               INTLTD(0,YR,ASSET_CLASS) = INTLTD(0,YR,ASSET_CLASS)
     +                                          + MONTHLY_INTEREST
               LTD_CASH_CARRY_OVER = MONTHLY_INTEREST
               START_MO = ISSMO + 1

               DO YR = RUN_YEAR, FINANCIAL_SIMULATION_YEARS

                  DO MO = START_MO, 12
                     IF(LTD_BAL == 0.) EXIT
                     IF(THIS_IS_A_PAY_MONTH(ISSMO,MO,PAID_WHEN)) THEN
                        LTD_CASH_INTEREST(MO,YR,ASSET_CLASS) =
     +                             LTD_CASH_INTEREST(MO,YR,ASSET_CLASS)
     +                              + LTD_CASH_CARRY_OVER
                        LTD_CASH_INTEREST(0,YR,ASSET_CLASS) =
     +                              LTD_CASH_INTEREST(0,YR,ASSET_CLASS)
     +                               + LTD_CASH_CARRY_OVER
                       IF(YR == RUN_YEAR+MTGLIF .AND. MO == ISSMO) THEN
                           MTG_DEBT_RETIRE = LTD_BAL
                           LTD_BAL = 0.
                        ELSE
                           MTG_DEBT_RETIRE = PIBIEN-LTD_CASH_CARRY_OVER
                           LTD_BAL = LTD_BAL - MTG_DEBT_RETIRE
                        ENDIF
                        SFLTD(MO,YR,ASSET_CLASS) =
     +                                      SFLTD(ISSMO,YR,ASSET_CLASS)
     +                                       + MTG_DEBT_RETIRE
                        SFLTD(0,YR,ASSET_CLASS) =
     +                                          SFLTD(0,YR,ASSET_CLASS)
     +                                           + MTG_DEBT_RETIRE
                        LTD_CASH_CARRY_OVER = 0.
                     ENDIF

                     MONTHLY_INTEREST = R_LTD_RATE * LTD_BAL/12.
                    INTLTD(MO,YR,ASSET_CLASS)=INTLTD(MO,YR,ASSET_CLASS)
     +                                         + MONTHLY_INTEREST
                    INTLTD(0,YR,ASSET_CLASS) = INTLTD(0,YR,ASSET_CLASS)
     +                                          + MONTHLY_INTEREST
                     LTD_CASH_CARRY_OVER = LTD_CASH_CARRY_OVER +
     +                                                 MONTHLY_INTEREST
                  ENDDO
                  IF(LTD_BAL == 0.) EXIT
                  R_ANNINT(YR) = R_ANNINT(YR) + LTD_BAL * R_LTD_RATE
                  ANN_LTD_INT(YR) = ANN_LTD_INT(YR)
     +                              + LTD_BAL * R_LTD_RATE
                  START_MO = 1
               ENDDO
            ENDIF
         ENDIF
      RETURN


      ENTRY MONTHLY_LTD_FULL_YEAR_NO_LAG(RUN_YEAR,R_CLASS,R_LTD_BAL,
     +                                   R_LTD_RATE,R_LTD_RETIRE)


         LTD_CASH_CARRY_OVER = 0.
         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
               YR = RUN_YEAR ! - 1
               LTD_BAL = R_LTD_BAL
               DO MO = 1, 12
                  IF(MO == 7) THEN
                     LTD_BAL = LTD_BAL - R_LTD_RETIRE
                    SFLTD(7,YR,ASSET_CLASS) = SFLTD(7,YR,ASSET_CLASS) +
     +                                                     R_LTD_RETIRE
                    SFLTD(0,YR,ASSET_CLASS) = SFLTD(0,YR,ASSET_CLASS) +
     +                                                     R_LTD_RETIRE
                  ENDIF
                  MONTHLY_INTEREST = LTD_BAL*R_LTD_RATE/12.
                 INTLTD(MO,YR,ASSET_CLASS) = INTLTD(MO,YR,ASSET_CLASS)+
     +                                                 MONTHLY_INTEREST
                 INTLTD(0,YR,ASSET_CLASS) = INTLTD(0,YR,ASSET_CLASS) +
     +                                                 MONTHLY_INTEREST
                  LTD_CASH_CARRY_OVER = LTD_CASH_CARRY_OVER +
     +                                                 MONTHLY_INTEREST
                 IF(MO ==3 .OR. MO ==6 .OR. MO == 9 .OR. MO == 12) THEN
                     LTD_CASH_INTEREST(MO,YR,ASSET_CLASS) =
     +                           LTD_CASH_INTEREST(MO,YR,ASSET_CLASS) +
     +                            LTD_CASH_CARRY_OVER
                     LTD_CASH_INTEREST(0,YR,ASSET_CLASS) =
     +                            LTD_CASH_INTEREST(0,YR,ASSET_CLASS) +
     +                            LTD_CASH_CARRY_OVER
                     LTD_CASH_CARRY_OVER = 0.
                  ENDIF
               ENDDO
            ENDIF
         ENDIF
      RETURN

      ENTRY MONTHLY_PS_LAST_HALF(RUN_YEAR,R_CLASS,R_PS_DIV,R_PSISS)


         PS_CASH_CARRY_OVER = 0.
         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
               YR = RUN_YEAR ! - 1
               INTPS(0,YR,ASSET_CLASS) = INTPS(0,YR,ASSET_CLASS) +
     +                                                      R_PS_DIV/2.
               ISSPS(0,YR,ASSET_CLASS) = ISSPS(0,YR,ASSET_CLASS) +
     +                                                          R_PSISS
               ISSPS(7,YR,ASSET_CLASS) = ISSPS(7,YR,ASSET_CLASS) +
     +                                                          R_PSISS
               DO MO = 7, 12
                  IF(MO == 10) THEN
                     PS_CASH_DIVIDEND(MO,YR,ASSET_CLASS) =
     +                            PS_CASH_DIVIDEND(MO,YR,ASSET_CLASS) +
     +                            PS_CASH_CARRY_OVER
                     PS_CASH_DIVIDEND(0,YR,ASSET_CLASS) =
     +                            PS_CASH_DIVIDEND(0,YR,ASSET_CLASS) +
     +                            PS_CASH_CARRY_OVER
                     PS_CASH_CARRY_OVER = 0.
                  ENDIF
                  INTPS(MO,YR,ASSET_CLASS) = INTPS(MO,YR,ASSET_CLASS)+
     +                                                     R_PS_DIV/12.
                  PS_CASH_CARRY_OVER=PS_CASH_CARRY_OVER+R_PS_DIV/12.

               ENDDO
            ENDIF
         ENDIF
      RETURN


      ENTRY MONTHLY_PS_FIRST_HALF(RUN_YEAR,R_CLASS,R_PS_DIV,R_PS_RETIRE)


         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
               YR = RUN_YEAR ! - 1
               INTPS(0,YR,ASSET_CLASS) = INTPS(0,YR,ASSET_CLASS) +
     +                                                      R_PS_DIV/2.
               RETPS(0,YR,ASSET_CLASS) = RETPS(0,YR,ASSET_CLASS) +
     +                                                      R_PS_RETIRE
               RETPS(7,YR,ASSET_CLASS) = RETPS(7,YR,ASSET_CLASS) +
     +                                                      R_PS_RETIRE
               DO MO = 1, 6
                  IF(MO == 1 .OR. MO == 4) THEN
                     PS_CASH_DIVIDEND(MO,YR,ASSET_CLASS) =
     +                            PS_CASH_DIVIDEND(MO,YR,ASSET_CLASS) +
     +                            PS_CASH_CARRY_OVER
                     PS_CASH_DIVIDEND(0,YR,ASSET_CLASS) =
     +                            PS_CASH_DIVIDEND(0,YR,ASSET_CLASS) +
     +                            PS_CASH_CARRY_OVER
                     PS_CASH_CARRY_OVER = 0.
                  ENDIF
                  INTPS(MO,YR,ASSET_CLASS) = INTPS(MO,YR,ASSET_CLASS)+
     +                                                     R_PS_DIV/12.
                  PS_CASH_CARRY_OVER=PS_CASH_CARRY_OVER + R_PS_DIV/12.
               ENDDO
            ENDIF
         ENDIF
      RETURN

      ENTRY MONTHLY_PS_FULL_YEAR(RUN_YEAR,R_CLASS,R_PS_BAL,R_PS_RATE,
     +                                                     R_PS_RETIRE)


         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
               YR = RUN_YEAR ! - 1
               PS_BAL = R_PS_BAL
               DO MO = 1, 12
                 IF(MO ==1 .OR. MO ==4 .OR. MO == 7 .OR. MO == 10) THEN
                     PS_CASH_DIVIDEND(MO,YR,ASSET_CLASS) =
     +                            PS_CASH_DIVIDEND(MO,YR,ASSET_CLASS) +
     +                            PS_CASH_CARRY_OVER
                     PS_CASH_DIVIDEND(0,YR,ASSET_CLASS) =
     +                            PS_CASH_DIVIDEND(0,YR,ASSET_CLASS) +
     +                            PS_CASH_CARRY_OVER
                     PS_CASH_CARRY_OVER = 0.
                  ENDIF
                  IF(MO == 7) THEN
                     PS_BAL = PS_BAL - R_PS_RETIRE
                     SFPS(7,YR,ASSET_CLASS) = SFPS(7,YR,ASSET_CLASS) +
     +                                                      R_PS_RETIRE
                     SFPS(0,YR,ASSET_CLASS) = SFPS(0,YR,ASSET_CLASS) +
     +                                                      R_PS_RETIRE
                  ENDIF
                  MONTHLY_DIVIDEND = PS_BAL*R_PS_RATE/12.
                  INTPS(MO,YR,ASSET_CLASS) = INTPS(MO,YR,ASSET_CLASS)+
     +                                                 MONTHLY_DIVIDEND
                  INTPS(0,YR,ASSET_CLASS) = INTPS(0,YR,ASSET_CLASS) +
     +                                                 MONTHLY_DIVIDEND
                  PS_CASH_CARRY_OVER = PS_CASH_CARRY_OVER +
     +                                                 MONTHLY_DIVIDEND
               ENDDO
            ENDIF
         ENDIF
      RETURN


      ENTRY MONTHLY_PS_FULL_YEAR_NO_LAG(RUN_YEAR,R_CLASS,R_PS_BAL,
     +                                  R_PS_RATE,R_PS_RETIRE)


         PS_CASH_CARRY_OVER = 0.
         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
               YR = RUN_YEAR ! - 1
               PS_BAL = R_PS_BAL
               DO MO = 1, 12
                  IF(MO == 7) THEN
                     PS_BAL = PS_BAL - R_PS_RETIRE
                     SFPS(7,YR,ASSET_CLASS) = SFPS(7,YR,ASSET_CLASS) +
     +                                                      R_PS_RETIRE
                     SFPS(0,YR,ASSET_CLASS) = SFPS(0,YR,ASSET_CLASS) +
     +                                                      R_PS_RETIRE
                  ENDIF
                  MONTHLY_DIVIDEND = PS_BAL*R_PS_RATE/12.
                  INTPS(MO,YR,ASSET_CLASS) = INTPS(MO,YR,ASSET_CLASS)+
     +                                                 MONTHLY_DIVIDEND
                  INTPS(0,YR,ASSET_CLASS) = INTPS(0,YR,ASSET_CLASS) +
     +                                                 MONTHLY_DIVIDEND
                  PS_CASH_CARRY_OVER = PS_CASH_CARRY_OVER +
     +                                                 MONTHLY_DIVIDEND
                 IF(MO ==3 .OR. MO ==6 .OR. MO == 9 .OR. MO == 12) THEN
                     PS_CASH_DIVIDEND(MO,YR,ASSET_CLASS) =
     +                            PS_CASH_DIVIDEND(MO,YR,ASSET_CLASS) +
     +                            PS_CASH_CARRY_OVER
                     PS_CASH_DIVIDEND(0,YR,ASSET_CLASS) =
     +                            PS_CASH_DIVIDEND(0,YR,ASSET_CLASS) +
     +                            PS_CASH_CARRY_OVER
                     PS_CASH_CARRY_OVER = 0.
                  ENDIF
               ENDDO
            ENDIF
         ENDIF
      RETURN

      ENTRY RESET_CARRY_OVERS()


         PS_CASH_CARRY_OVER = 0.
         LTD_CASH_CARRY_OVER = 0.
      RETURN


      ENTRY DEBT_BY_ISSUE_EXP_BAL(R_CLASS,
     +                            R_PS_ISSUE_EXP_BAL,
     +                            R_LTD_ISSUE_EXP_BAL,
     +                            R_INVESTMENT_BALANCE,
     +                            R_NOTES_RECEIVABLE,
     +                            R_NOTES_PAYABLE,
     +                            R_CURRENT_LTD_RETIRE)
         R_INVESTMENT_BALANCE = 0.
         R_PS_ISSUE_EXP_BAL = 0.
         R_LTD_ISSUE_EXP_BAL = 0.
         R_NOTES_RECEIVABLE = 0.
         R_NOTES_PAYABLE = 0.

         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
               R_PS_ISSUE_EXP_BAL = PS_ISSUE_EXP_BAL(12,0,ASSET_CLASS)
              R_LTD_ISSUE_EXP_BAL = LTD_ISSUE_EXP_BAL(12,0,ASSET_CLASS)
               R_INVESTMENT_BALANCE =
     +                    INVESTMENT_INCOME(12,0,ASSET_CLASS,Additions)
               R_NOTES_RECEIVABLE =
     +                     NOTES_RECEIVABLE(12,0,ASSET_CLASS,Additions)
              R_NOTES_PAYABLE=NOTES_PAYABLE(12,0,ASSET_CLASS,Additions)
               R_CURRENT_LTD_RETIRE = SFLTD(0,1,ASSET_CLASS)
     +                                + RETLTD(0,1,ASSET_CLASS)
               R_CURRENT_LTD_RETIRE =

     +                           CURRENT_PORTION_OF_LTD(0,1,ASSET_CLASS)
            ENDIF
         ENDIF
      RETURN

      ENTRY DEBT_ELIM_INFO(RUN_YEAR,R_CLASS,R_CLASS_EXISTS,
     +                     R_PS_DIV,R_PS_RETIRE,R_PSISS,
     +                     R_LTDISS,R_LTD_INT,R_LTD_RETIRE,
     +                     R_ANN_PS_DIV_LTD_INT,
     +                     R_ANN_PS_DIV,
     +                     R_ANN_LTD_INT,
     +                     R_OTHER_INCOME,
     +                     R_INVESTMENT_IN_AFILLIATES,
     +                     R_INTEREST_INCOME,
     +                     R_LOANS_TO_AFILLIATES,
     +                     R_PS_PREM_BAL,
     +                     R_PS_PREM_AMORT,
     +                     R_LTD_PREM_BAL,
     +                     R_LTD_PREM_AMORT,
     +                     R_PS_ISSUE_EXP_BAL,
     +                     R_PS_ISSUE_EXP_AMORT,
     +                     R_LTD_ISSUE_EXP_BAL,
     +                     R_LTD_ISSUE_EXP_AMORT,
     +                     R_LTD_PS_ISSUE_TAX_EXPENSE,
     +                     R_CS_ISSUE_EXP_THIS_PERIOD,
     +                     R_MIPS_INCOME_TAX_DEDUCTION,
     +                     R_STD_INTEREST,
     +                     R_INVESTMENT_BALANCE,
     +                     R_NOTES_RECEIVABLE,
     +                     R_NOTES_PAYABLE,
     +                     R_DIVIDEND_70_INCOME,
     +                     R_INVESTMENT_INCOME,
     +                     R_STD_INTEREST_CASH,
     +                     R_LTD_INTEREST_CASH_PAYMENTS,
     +                     R_PS_DIVIDEND_CASH_PAYMENTS,
     +                     R_CASH_INVESTMENT_EARNINGS,
     +                     R_CURRENT_LTD_RETIRE)

! SET THE AUTO ELIMINATIONS

         YR = RUN_YEAR - 1
         R_PERIOD = 0
         R_INVESTMENT_INCOME =
     +             INVESTMENT_INCOME(R_PERIOD,YR,-1,Dividend_70)
     +             + INVESTMENT_INCOME(R_PERIOD,YR,-1,Dividend)

     +             + INVESTMENT_INCOME(R_PERIOD,YR,-1,Interest_Earnings)
     +             + NOTES_RECEIVABLE(R_PERIOD,YR,-1,Interest_Earnings)
     +             + INTEREST_INCOME(R_PERIOD,YR,-1)
         R_DIVIDEND_70_INCOME =
     +                     INVESTMENT_INCOME(R_PERIOD,YR,-1,Dividend_70)
         R_STD_INTEREST=NOTES_PAYABLE(R_PERIOD,YR,-1,Interest_Payments)
         IF(MONTHLY_MIDAS_ACTIVE) THEN
            R_STD_INTEREST_CASH = NOTES_PAYABLE(R_PERIOD,YR,-1,
     +                                           Cash_Interest_Payments)
         ELSE
            R_STD_INTEREST_CASH = NOTES_PAYABLE(R_PERIOD,YR,-1,
     +                                                Interest_Payments)

         ENDIF
         R_INVESTMENT_BALANCE =
     +                      INVESTMENT_INCOME(R_PERIOD,YR,-1,Additions)
         R_NOTES_RECEIVABLE = R_NOTES_RECEIVABLE +
     +                     NOTES_RECEIVABLE(R_PERIOD,YR,-1,Additions)
     +                    - NOTES_RECEIVABLE(R_PERIOD,YR,-1,Reductions)
         R_NOTES_PAYABLE = R_NOTES_PAYABLE +
     +                     NOTES_PAYABLE(R_PERIOD,YR,-1,Additions)
     +                     - NOTES_PAYABLE(R_PERIOD,YR,-1,Reductions)

         R_PSISS = ISSPS(R_PERIOD,YR,-1)
         R_PS_RETIRE = R_PS_RETIRE + SFPS(R_PERIOD,YR,-1) +
     +                                            RETPS(R_PERIOD,YR,-1)
         R_PS_DIV = R_PS_DIV + INTPS(R_PERIOD,YR,-1)
         R_LTDISS = ISSLTD(R_PERIOD,YR,-1)
         RETLTD(R_PERIOD,YR,-1) = SUM(RETLTD(1:,YR,-1))
         SFLTD(R_PERIOD,YR,-1) = SUM(SFLTD(1:,YR,-1))
         R_LTD_RETIRE = R_LTD_RETIRE
     +                  + RETLTD(R_PERIOD,YR,-1)
     +                  + SFLTD(R_PERIOD,YR,-1)
         R_LTD_INT = R_LTD_INT + INTLTD(R_PERIOD,YR,-1)
         R_OTHER_INCOME = OTHER_INCOME(R_PERIOD,YR,-1)
         R_INVESTMENT_IN_AFILLIATES =
     +                         INVESTMENT_IN_AFILLIATES(R_PERIOD,YR,-1)
         R_INTEREST_INCOME = INTEREST_INCOME(R_PERIOD,YR,-1)

     +              + NOTES_RECEIVABLE(R_PERIOD,YR,-1,Interest_Earnings)

         R_LOANS_TO_AFILLIATES = LOANS_TO_AFILLIATES(R_PERIOD,YR,-1)
         R_LTD_PS_ISSUE_TAX_EXPENSE =
     +                         LTD_PS_ISSUE_TAX_EXPENSE(R_PERIOD,YR,-1)
         R_CS_ISSUE_EXP_THIS_PERIOD =

     +                              CS_ISSUE_TAX_EXPENSE(R_PERIOD,YR,-1)

         R_ANN_PS_DIV_LTD_INT = R_ANN_PS_DIV_LTD_INT +
     +                          AINTPS(YR,-1) +
     +                          AINTDB(YR,-1)
         R_ANN_PS_DIV = R_ANN_PS_DIV + AINTPS(YR,-1)
         R_ANN_LTD_INT = R_ANN_LTD_INT + AINTDB(YR,-1)
         R_PS_PREM_BAL = PS_PREM_BAL(R_PERIOD,YR,-1)
         R_PS_PREM_AMORT = PS_PREM_AMORT(R_PERIOD,YR,-1)
         R_LTD_PREM_BAL = LTD_PREM_BAL(R_PERIOD,YR,-1)
         R_LTD_PREM_AMORT = LTD_PREM_AMORT(R_PERIOD,YR,-1)
         R_PS_ISSUE_EXP_BAL = PS_ISSUE_EXP_BAL(R_PERIOD,YR,-1)
         R_PS_ISSUE_EXP_AMORT = PS_ISSUE_EXP_AMORT(R_PERIOD,YR,-1)
         R_LTD_ISSUE_EXP_BAL = LTD_ISSUE_EXP_BAL(R_PERIOD,YR,-1)
         R_LTD_ISSUE_EXP_AMORT = LTD_ISSUE_EXP_AMORT(R_PERIOD,YR,-1)
         R_MIPS_INCOME_TAX_DEDUCTION =
     +                            MIPS_INCOME_TAX_DEDUCTION(YR,-1)
         R_LTD_INTEREST_CASH_PAYMENTS =
     +                                LTD_CASH_INTEREST(R_PERIOD,YR,-1)
         R_PS_DIVIDEND_CASH_PAYMENTS =
     +                                 PS_CASH_DIVIDEND(R_PERIOD,YR,-1)
         R_CASH_INVESTMENT_EARNINGS =
     +               INVESTMENT_INCOME(R_PERIOD,YR,-1,
     +                                           Cash_Dividend_Earnings)
     +               + INVESTMENT_INCOME(R_PERIOD,YR,-1,
     +                                           Cash_Interest_Earnings)
     +               + NOTES_RECEIVABLE(R_PERIOD,YR,-1,
     +                                           Cash_Interest_Earnings)

         R_CURRENT_LTD_RETIRE = R_CURRENT_LTD_RETIRE
     +                        + CURRENT_PORTION_OF_LTD(R_PERIOD,YR+1,-1)

! ADDED USER SPECIFIED ELIMINATIONS

         R_CLASS_EXISTS = .FALSE.
         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
               R_CLASS_EXISTS = .TRUE.
               R_INVESTMENT_INCOME = R_INVESTMENT_INCOME
     +            + INVESTMENT_INCOME(R_PERIOD,YR,ASSET_CLASS,
     +                                                      Dividend_70)
     +            + INVESTMENT_INCOME(R_PERIOD,YR,ASSET_CLASS,Dividend)
     +            + INVESTMENT_INCOME(R_PERIOD,YR,ASSET_CLASS,
     +                                                Interest_Earnings)
     +            + NOTES_RECEIVABLE(R_PERIOD,YR,ASSET_CLASS,
     +                                                Interest_Earnings)
     +            + INTEREST_INCOME(R_PERIOD,YR,ASSET_CLASS)
               R_DIVIDEND_70_INCOME = R_DIVIDEND_70_INCOME
     +                + INVESTMENT_INCOME(R_PERIOD,YR,ASSET_CLASS,
     +                                                      Dividend_70)
               R_STD_INTEREST= R_STD_INTEREST +
     +          NOTES_PAYABLE(R_PERIOD,YR,ASSET_CLASS,Interest_Payments)
               R_STD_INTEREST_CASH = R_STD_INTEREST_CASH
     +               + NOTES_PAYABLE(R_PERIOD,YR,ASSET_CLASS,
     +                                           Cash_Interest_Payments)


               R_PSISS  = R_PSISS +ISSPS(R_PERIOD,YR,ASSET_CLASS)
               R_PS_RETIRE = R_PS_RETIRE+SFPS(R_PERIOD,YR,ASSET_CLASS)+
     +                                   RETPS(R_PERIOD,YR,ASSET_CLASS)
               R_PS_DIV = R_PS_DIV + INTPS(R_PERIOD,YR,ASSET_CLASS)
               R_LTDISS = R_LTDISS + ISSLTD(R_PERIOD,YR,ASSET_CLASS)
               RETLTD(R_PERIOD,YR,ASSET_CLASS) =
     +                                   SUM(RETLTD(1:,YR,ASSET_CLASS))
               SFLTD(R_PERIOD,YR,ASSET_CLASS) =
     +                                    SUM(SFLTD(1:,YR,ASSET_CLASS))
               R_LTD_RETIRE = R_LTD_RETIRE
     +                        + RETLTD(R_PERIOD,YR,ASSET_CLASS)
     +                        + SFLTD(R_PERIOD,YR,ASSET_CLASS)
               R_LTD_INT = R_LTD_INT + INTLTD(R_PERIOD,YR,ASSET_CLASS)
               R_LTD_PS_ISSUE_TAX_EXPENSE = R_LTD_PS_ISSUE_TAX_EXPENSE
     +              + LTD_PS_ISSUE_TAX_EXPENSE(R_PERIOD,YR,ASSET_CLASS)
               R_CS_ISSUE_EXP_THIS_PERIOD = R_CS_ISSUE_EXP_THIS_PERIOD
     +                  + CS_ISSUE_TAX_EXPENSE(R_PERIOD,YR,ASSET_CLASS)
               R_OTHER_INCOME = R_OTHER_INCOME +
     +                            OTHER_INCOME(R_PERIOD,YR,ASSET_CLASS)
              R_INVESTMENT_IN_AFILLIATES = R_INVESTMENT_IN_AFILLIATES +
     +                INVESTMENT_IN_AFILLIATES(R_PERIOD,YR,ASSET_CLASS)
               R_INTEREST_INCOME = R_INTEREST_INCOME
     +                + INTEREST_INCOME(R_PERIOD,YR,ASSET_CLASS)
     +                + NOTES_RECEIVABLE(R_PERIOD,YR,ASSET_CLASS,
     +                                                Interest_Earnings)
               R_LOANS_TO_AFILLIATES = R_LOANS_TO_AFILLIATES +
     +                      LOANS_TO_AFILLIATES(R_PERIOD,YR,ASSET_CLASS)

               R_ANN_PS_DIV_LTD_INT = R_ANN_PS_DIV_LTD_INT +
     +                                AINTPS(YR,ASSET_CLASS) +
     +                                AINTDB(YR,ASSET_CLASS)
               R_ANN_PS_DIV = R_ANN_PS_DIV + AINTPS(YR,ASSET_CLASS)
               R_ANN_LTD_INT = R_ANN_LTD_INT + AINTDB(YR,ASSET_CLASS)
               R_PS_PREM_BAL = R_PS_PREM_BAL
     +                         + PS_PREM_BAL(R_PERIOD,YR,ASSET_CLASS)
               R_LTD_PREM_BAL=R_LTD_PREM_BAL
     +                         + LTD_PREM_BAL(R_PERIOD,YR,ASSET_CLASS)
               R_PS_PREM_AMORT = R_PS_PREM_AMORT
     +                         + PS_PREM_AMORT(R_PERIOD,YR,ASSET_CLASS)
               R_LTD_PREM_AMORT = R_LTD_PREM_AMORT
     +                        + LTD_PREM_AMORT(R_PERIOD,YR,ASSET_CLASS)
               R_PS_ISSUE_EXP_BAL = R_PS_ISSUE_EXP_BAL +
     +                        PS_ISSUE_EXP_BAL(R_PERIOD,YR,ASSET_CLASS)
               R_PS_ISSUE_EXP_AMORT = R_PS_ISSUE_EXP_AMORT +
     +                      PS_ISSUE_EXP_AMORT(R_PERIOD,YR,ASSET_CLASS)
               R_LTD_ISSUE_EXP_BAL = R_LTD_ISSUE_EXP_BAL +
     +                       LTD_ISSUE_EXP_BAL(R_PERIOD,YR,ASSET_CLASS)
               R_LTD_ISSUE_EXP_AMORT = R_LTD_ISSUE_EXP_AMORT +
     +                     LTD_ISSUE_EXP_AMORT(R_PERIOD,YR,ASSET_CLASS)
              R_MIPS_INCOME_TAX_DEDUCTION=R_MIPS_INCOME_TAX_DEDUCTION +
     +                   MIPS_INCOME_TAX_DEDUCTION(YR,ASSET_CLASS)
               R_INVESTMENT_BALANCE = R_INVESTMENT_BALANCE +
     +             INVESTMENT_INCOME(R_PERIOD,YR,ASSET_CLASS,Additions)
               R_NOTES_RECEIVABLE = R_NOTES_RECEIVABLE +
     +            NOTES_RECEIVABLE(R_PERIOD,YR,ASSET_CLASS,Additions)
     +           - NOTES_RECEIVABLE(R_PERIOD,YR,ASSET_CLASS,Reductions)
               R_NOTES_PAYABLE = R_NOTES_PAYABLE +
     +               NOTES_PAYABLE(R_PERIOD,YR,ASSET_CLASS,Additions)
     +              - NOTES_PAYABLE(R_PERIOD,YR,ASSET_CLASS,Reductions)
               R_LTD_INTEREST_CASH_PAYMENTS =
     +                      R_LTD_INTEREST_CASH_PAYMENTS
     +                     + LTD_CASH_INTEREST(R_PERIOD,YR,ASSET_CLASS)
              R_PS_DIVIDEND_CASH_PAYMENTS = R_PS_DIVIDEND_CASH_PAYMENTS
     +                      + PS_CASH_DIVIDEND(R_PERIOD,YR,ASSET_CLASS)
               R_CASH_INVESTMENT_EARNINGS = R_CASH_INVESTMENT_EARNINGS
     +               + INVESTMENT_INCOME(R_PERIOD,YR,ASSET_CLASS,
     +                                           Cash_Dividend_Earnings)
     +               + INVESTMENT_INCOME(R_PERIOD,YR,ASSET_CLASS,
     +                                           Cash_Interest_Earnings)
     +               + NOTES_RECEIVABLE(R_PERIOD,YR,ASSET_CLASS,
     +                                           Cash_Interest_Earnings)

               R_CURRENT_LTD_RETIRE = R_CURRENT_LTD_RETIRE
     +                + CURRENT_PORTION_OF_LTD(R_PERIOD,YR,ASSET_CLASS)
            ENDIF
         ENDIF
      RETURN

      ENTRY MONTHLY_ELIM_DEBT(RUN_YEAR,R_CLASS,
     +                        INCOME_VARIABLES,
     +                        CASH_VARIABLES)


         YR = RUN_YEAR ! - 1
         ASSET_CLASS = -1
         DO I = 1, 2
! Was Monthly Interest on Notes Payable ->
            INCOME_VARIABLES(:,mty_intst_on_nts_pyble) =
     +             INCOME_VARIABLES(:,mty_intst_on_nts_pyble)
     +             +  NOTES_PAYABLE(:,YR,ASSET_CLASS,Interest_Payments)


            INCOME_VARIABLES(:,monthly_notes_receivable_income) =
     +              INCOME_VARIABLES(:,monthly_notes_receivable_income)
     +               + NOTES_RECEIVABLE(:,YR,ASSET_CLASS,
     +                                  Intra_Company_Interest_Earnings)
            INCOME_VARIABLES(:,Monthly_LTD_Total_Interest) =
     +                                         INTLTD(:,YR,ASSET_CLASS)

! CASH ITEMS

            CASH_VARIABLES(:,cash_fm_repaid_issued_notes) =
     +             CASH_VARIABLES(:,cash_fm_repaid_issued_notes)
     +             + NOTES_RECEIVABLE(:,YR,ASSET_CLASS,Reductions)
            CASH_VARIABLES(:,cash_4_new_notes_issued) =
     +                    CASH_VARIABLES(:,cash_4_new_notes_issued)
     +                    + NOTES_RECEIVABLE(:,YR,ASSET_CLASS,Additions)
            CASH_VARIABLES(:,csh_4_redeeming_notes_owed) =
     +                    CASH_VARIABLES(:,csh_4_redeeming_notes_owed)
     +                    + NOTES_PAYABLE(:,YR,ASSET_CLASS,Reductions)
            CASH_VARIABLES(:,cash_fm_notes_issd_by_othrs) =
     +                CASH_VARIABLES(:,cash_fm_notes_issd_by_othrs)
     +                + NOTES_PAYABLE(:,YR,ASSET_CLASS,Additions)
            CASH_VARIABLES(:,cash_investmt_divnd_earngs) =
     +               CASH_VARIABLES(:,cash_investmt_divnd_earngs)
     +               + INVESTMENT_INCOME(:,YR,ASSET_CLASS,
     +                                           Cash_Dividend_Earnings)
            CASH_VARIABLES(:,cash_invest_interest_ernings) =
     +               CASH_VARIABLES(:,cash_invest_interest_ernings)
     +               + INVESTMENT_INCOME(:,YR,ASSET_CLASS,
     +                                           Cash_Interest_Earnings)
     +               + NOTES_RECEIVABLE(:,YR,ASSET_CLASS,
     +                                           Cash_Interest_Earnings)
            CASH_VARIABLES(:,Cash_Interest_on_Notes_Payable) =
     +                  CASH_VARIABLES(:,Cash_Interest_on_Notes_Payable)
     +                  +  NOTES_PAYABLE(:,YR,ASSET_CLASS,
     +                                           Cash_Interest_Payments)
            CASH_VARIABLES(:,cash_change_debt_investments) =
     +                  CASH_VARIABLES(:,cash_change_debt_investments)
     +                  + INVESTMENT_INCOME(:,YR,ASSET_CLASS,Additions)
     +                  - INVESTMENT_INCOME(:,YR,ASSET_CLASS,Reductions)
            CASH_VARIABLES(:,cash_net_investments) =
     +                      CASH_VARIABLES(:,cash_net_investments)
     +                      + LOANS_TO_AFILLIATES(:,YR,ASSET_CLASS)
     +                      + INVESTMENT_IN_AFILLIATES(:,YR,ASSET_CLASS)
            CASH_VARIABLES(:,cash_ltd_issued) =
     +               CASH_VARIABLES(:,cash_ltd_issued)
     +               + ISSLTD(:,YR,ASSET_CLASS)
     +               + LTD_NEW_ISSUE_PREM_DISC_AMOUNT(:,YR,ASSET_CLASS)
            CASH_VARIABLES(:,cash_ltd_retirements) =
     +                            CASH_VARIABLES(:,cash_ltd_retirements)
     +                            + SFLTD(:,YR,ASSET_CLASS)
     +                            + RETLTD(:,YR,ASSET_CLASS)
            IF(I ==2) EXIT
            IF(R_CLASS > MAX_ASSET_CLASS_NUM) EXIT
            IF(R_CLASS == 0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS < 0) EXIT
         ENDDO
      RETURN

      ENTRY DEBT_BASE_YEAR_INFO(R_CLASS,R_PSISS,R_LTDISS,
     +                          R_INVESTMENT_IN_AFILLIATES,
     +                          R_LOANS_TO_AFILLIATES,
     +                          R_PS_PREM_BAL,
     +                          R_LTD_PREM_BAL,
     +                          R_PS_ISSUE_EXP_BAL,
     +                          R_LTD_ISSUE_EXP_BAL)
         R_PSISS  = 0.
         R_LTDISS = 0.
         R_INVESTMENT_IN_AFILLIATES = 0.
         R_LOANS_TO_AFILLIATES = 0.
         R_PS_PREM_BAL = 0.
         R_LTD_PREM_BAL = 0.
         R_PS_ISSUE_EXP_BAL = 0.
         R_LTD_ISSUE_EXP_BAL = 0.

         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
               R_PSISS  = ISSPS(12,0,ASSET_CLASS)
               R_LTDISS = ISSLTD(12,0,ASSET_CLASS)
               R_INVESTMENT_IN_AFILLIATES =
     +                       INVESTMENT_IN_AFILLIATES(12,0,ASSET_CLASS)
               R_LOANS_TO_AFILLIATES =
     +                            LOANS_TO_AFILLIATES(12,0,ASSET_CLASS)
               R_PS_PREM_BAL = PS_PREM_BAL(12,0,ASSET_CLASS)
               R_LTD_PREM_BAL = LTD_PREM_BAL(12,0,ASSET_CLASS)
               R_PS_ISSUE_EXP_BAL = PS_ISSUE_EXP_BAL(12,0,ASSET_CLASS)
              R_LTD_ISSUE_EXP_BAL = LTD_ISSUE_EXP_BAL(12,0,ASSET_CLASS)
            ENDIF
         ENDIF
      RETURN

      ENTRY DEBT_BASE_YEAR_ELIM(R_CLASS,R_PSISS,R_LTDISS,
     +                          R_INVESTMENT_IN_AFILLIATES,
     +                          R_LOANS_TO_AFILLIATES,
     +                          R_PS_PREM_BAL,
     +                          R_LTD_PREM_BAL,
     +                          R_PS_ISSUE_EXP_BAL,
     +                          R_LTD_ISSUE_EXP_BAL,
     +                          R_INVESTMENT_BALANCE,
     +                          R_NOTES_RECEIVABLE,
     +                          R_NOTES_PAYABLE)

         R_PSISS = ISSPS(12,0,-1)
         R_LTDISS = ISSLTD(12,0,-1)
         R_INVESTMENT_IN_AFILLIATES = INVESTMENT_IN_AFILLIATES(12,0,-1)
         R_LOANS_TO_AFILLIATES = LOANS_TO_AFILLIATES(12,0,-1)
         R_PS_PREM_BAL = PS_PREM_BAL(12,0,-1)
         R_LTD_PREM_BAL = LTD_PREM_BAL(12,0,-1)
         R_PS_ISSUE_EXP_BAL = PS_ISSUE_EXP_BAL(12,0,-1)
         R_LTD_ISSUE_EXP_BAL = LTD_ISSUE_EXP_BAL(12,0,-1)
         R_INVESTMENT_BALANCE = INVESTMENT_INCOME(12,0,-1,Additions)
         R_NOTES_RECEIVABLE = NOTES_RECEIVABLE(12,0,-1,Additions)
         R_NOTES_PAYABLE = NOTES_PAYABLE(12,0,-1,Additions)

         IF(R_CLASS <= MAX_ASSET_CLASS_NUM) THEN
            IF(R_CLASS ==0) THEN
               ASSET_CLASS = 0
            ELSE
               ASSET_CLASS = ASSET_CLASS_POINTER(R_CLASS)
            ENDIF
            IF(ASSET_CLASS > 0 .OR. R_CLASS == 0) THEN
               R_PSISS = R_PSISS + ISSPS(12,0,ASSET_CLASS)
               R_LTDISS = R_LTDISS + ISSLTD(12,0,ASSET_CLASS)
              R_INVESTMENT_IN_AFILLIATES = R_INVESTMENT_IN_AFILLIATES +
     +                       INVESTMENT_IN_AFILLIATES(12,0,ASSET_CLASS)
               R_LOANS_TO_AFILLIATES = R_LOANS_TO_AFILLIATES +
     +                            LOANS_TO_AFILLIATES(12,0,ASSET_CLASS)
               R_PS_PREM_BAL = R_PS_PREM_BAL
     +                         + PS_PREM_BAL(12,0,ASSET_CLASS)
               R_LTD_PREM_BAL = R_LTD_PREM_BAL
     +                          + LTD_PREM_BAL(12,0,ASSET_CLASS)
               R_PS_ISSUE_EXP_BAL = R_PS_ISSUE_EXP_BAL +
     +                               PS_ISSUE_EXP_BAL(12,0,ASSET_CLASS)
               R_LTD_ISSUE_EXP_BAL = R_LTD_ISSUE_EXP_BAL +
     +                              LTD_ISSUE_EXP_BAL(12,0,ASSET_CLASS)
               R_INVESTMENT_BALANCE = R_INVESTMENT_BALANCE
     +                  + INVESTMENT_INCOME(12,0,ASSET_CLASS,Additions)
               R_NOTES_RECEIVABLE = R_NOTES_RECEIVABLE
     +                   + NOTES_RECEIVABLE(12,0,ASSET_CLASS,Additions)
               R_NOTES_PAYABLE = R_NOTES_PAYABLE
     +                      + NOTES_PAYABLE(12,0,ASSET_CLASS,Additions)
            ENDIF
         ENDIF
      RETURN
      END

      REAL FUNCTION FRACTN(MO,DA,YEAR)

      INTEGER*2 YEAR
      INTEGER*2 I2YR
      INTEGER*2 YR,MO,DA,ILEAP,JULIAN
      INTEGER*2 DAYS_IN_EACH_MONTH
      INTEGER*2 DAYS_YEAR_TO_DATE,REMAINING_DAYS_IN_MONTH
      REAL*4 TIME_FROM_FIRST_DAY,DAYS_IN_MONTH,
     +       FRACTION_OF_MONTH,BALANCE_OF_MONTH
      INTEGER*2 FIRST_DAY,LAST_DAY,DAYS_IN_YEAR
      LOGICAL*1 WVPA

         IF(WVPA()) THEN
            FRACTN = FLOAT((MO-1)*30+MIN(DA,30)-1)/360.
         ELSE
            I2YR = YEAR
            IF(MOD(I2YR,4) == 0) THEN
               ILEAP=1
            ELSE
               ILEAP=0
            ENDIF
            JULIAN = DAYS_YEAR_TO_DATE(MO) + DA - 1
            IF(MO >= 3) JULIAN = JULIAN + ILEAP
            FRACTN = FLOAT(JULIAN)/FLOAT(365+ILEAP)
         ENDIF
      RETURN

      ENTRY TIME_FROM_FIRST_DAY(MO,DA,YR)


         IF(WVPA()) THEN
            TIME_FROM_FIRST_DAY = FLOAT(MIN(DA,30))/30.
         ELSE
            TIME_FROM_FIRST_DAY = FLOAT(DA)/DAYS_IN_MONTH(MO,YR)
         ENDIF
      RETURN

      ENTRY FRACTION_OF_MONTH(MO,FIRST_DAY,LAST_DAY,YR)


         IF(WVPA()) THEN
            FRACTION_OF_MONTH = FLOAT(30 - MIN(FIRST_DAY,30) + 1)
         ELSE
            FRACTION_OF_MONTH = FLOAT(LAST_DAY - FIRST_DAY + 1)
         ENDIF
      RETURN

      ENTRY BALANCE_OF_MONTH(MO,DA,YR)


         IF(WVPA()) THEN
            BALANCE_OF_MONTH = 1. - FLOAT(MIN(DA,30)-1)/30.
         ELSE
            BALANCE_OF_MONTH = 1. - FLOAT(DA-1)/DAYS_IN_MONTH(MO,YR)
         ENDIF
      RETURN
      ENTRY REMAINING_DAYS_IN_MONTH(MO,DA,YR)

         IF(WVPA()) THEN
            REMAINING_DAYS_IN_MONTH = 30 - MIN(DA,30) + 1
         ELSE
            REMAINING_DAYS_IN_MONTH = DAYS_IN_MONTH(MO,YR) - DA + 1
         ENDIF
      RETURN

      ENTRY DAYS_IN_YEAR(YR)

         IF(WVPA()) THEN
            DAYS_IN_YEAR = 360
         ELSE
            IF(MOD(YR,4) == 0) THEN
               ILEAP=1
            ELSE
               ILEAP=0
            ENDIF
            DAYS_IN_YEAR = FLOAT(365+ILEAP)
         ENDIF
      RETURN
      END


      FUNCTION DAYS_IN_MONTH(MO,YR)

      REAL*4 DAYS_IN_MONTH
      INTEGER*2 MO,YR,DAYS_IN_EACH_MONTH
      LOGICAL*1 WVPA

         IF(WVPA()) THEN
            DAYS_IN_MONTH = 30.
         ELSE
            IF(MO == 2 .AND. MOD(YR,4) == 0) THEN
               DAYS_IN_MONTH = 29.
            ELSE
               DAYS_IN_MONTH = DAYS_IN_EACH_MONTH(MO)
            ENDIF
         ENDIF
      RETURN
      END

!          CALCULATION SUBROUTINE

      SUBROUTINE REMORT(FINANCIAL_SIMULATION_YEARS,YR,I,OLDRAT,NEWRAT,
     +                 BAL,SINKING_FUND,INTEREST,ISS,AMT,ISSYR,
     +                 MATMO,MATDA,YRMAT)

        use mod_base_year
        use class_parameters




!   COMPUTES A NEW AMORTIZATION SCHEDULE BASED UPON THE BALANCE IN
!   EFFECT ON THE REVERSE ANNIVERSARY OF THE MATURITY DATE (HERE
!   WE USE THE REVERSE ANNIVERSARY ON ADVICE FROM TIM PRYOR THAT:
!   SOME USERS MAY FEEL THAT IN CASES OF THE ISSUE DATE BEING IN THE
!   HISTORIC PAST, THEY ARE NOT BOUND TO ACCURATELY INPUT THE ISSUE
!   DATE; BUT IN FACT THEY DO DESCRIBE THE MATURITY DATE ACCURATELY)

!         TREAT THIS DEBT ISSUE AS A VARIABLE-RATE MORTGAGE,
!         WITH PRINCIPAL AND INTEREST VARYING OVER ITS LIFE;
!         AT MOST 4 CASES MUST BE ACTIVELY CONSIDERED:
!         00) ISSYR IS NOT < (get_BASE_YEAR()+1) AND
!          YRMAT IS NOT > (get_BASE_YEAR()-1+FINANCIAL_SIMULATION_YEARS)
!         01) ISSYR IS NOT < (get_BASE_YEAR()+1) AND
!          YRMAT IS     > (get_BASE_YEAR()-1+FINANCIAL_SIMULATION_YEARS)
!         10) ISSYR IS     < (get_BASE_YEAR()+1) AND
!          YRMAT IS NOT > (get_BASE_YEAR()-1+FINANCIAL_SIMULATION_YEARS)
!         11) ISSYR IS     < (get_BASE_YEAR()+1) AND
!          YRMAT IS     > (get_BASE_YEAR()-1+FINANCIAL_SIMULATION_YEARS)
!         [ CASES OF YRMAT < (get_BASE_YEAR()+1)
!             OR ISSYR > (get_BASE_YEAR()-1+FINANCIAL_SIMULATION_YEARS)
!                                                     ARE DEAD ISSUES,
!           OUTSIDE THE RANGE OF YEARS MODELED;
!           CASES OF ISSYR >= YRMAT ARE IGNORED AS SPURIOUS DATA ]
!         CASES 00 AND 01 CALL REMORT TO SCHEDULE THE AMORTIZATION
!         OF THE ORIGINAL, NOMINAL BALANCE (AMT);
!         CASES 10 AND 11 CALL REMORT IN THE FIRST MODEL YEAR (I=2)
!    TO SCHEDULE THE AMORTIZATION OF THE REDUCED, END-OF-get_BASE_YEAR()
!         BALANCE (BYRBAL);
!         NOTE THAT BELOW, CASES 00 AND 01 ARE TREATED UNDER (2),
!         WHILE            CASES 10 AND 11 ARE TREATED UNDER (1);
!         THE DIFFERENCE IN TREATMENT OF YRMAT BETWEEN CASES 00 AND 10
!       ON THE ONE HAND, AND CASES 01 AND 11 ON THE OTHER, IS THAT THE
!         ARRAYS OF RESULTS ARE TRUNCATED WHEN YRMAT >
!                          get_BASE_YEAR()-1+FINANCIAL_SIMULATION_YEARS;
!         REGARDLESS OF WHETHER YRMAT FALLS BEYOND THE MODEL PURVIEW,
!THE INT, SINKING_FUND, AND BAL ARRAYS ARE OVER-WRITTEN WITHIN BOUNDS.

!         REAMORTIZE THE MORTGAGE WHEN THE CURRENT YEAR (YR)
!         IS NOT BEYOND THE MATURITY YEAR (YRMAT) AND EITHER
!         1)  THIS YR IS THE FIRST MODEL YEAR (WITH I=2)
!             AND THE ISSUE YEAR (ISSYR) PRECEDES YR, OR
!         2)  THIS YR IS THE ISSUE YEAR, OR
!         3)  THIS YR'S INTEREST RATE DIFFERS FROM THE PRIOR RATE

      INTEGER*2 I,J,YR,FINANCIAL_SIMULATION_YEARS,MO
      REAL*4 OLDRAT,NEWRAT,PR1231,IN1231,PI1231,FRACT1,FRACT2,FRACTN,
     +       BAL(*),SINKING_FUND(0:12,0:*),
     +       INTEREST(*),ISS(*),BYRBAL

      INTEGER*2 ISSYR,MATMO,MATDA,YRMAT
      REAL*4 AMT

      MO = 0
      FRACT1 = FRACTN(MATMO,MATDA,YR)
      FRACT2 = 1. - FRACT1
      BYRBAL = BAL(1)
!     AFTER INITIALIZATION OF THE AMORTIZATION SCHEDULE IN ISSYR OR I=2,
!     THIS ROUTINE IS CALLED ONLY WHEN NEWRAT DIFFERS FROM THE OLD RATE;
!     IN ANY CASE, IT IS CALLED ONLY WHEN (YR <= YRMAT)
      IF (YR < YRMAT) THEN
         IF ((I == 2) .AND. (ISSYR < YR)) THEN
! IF THE MORTGAGE SECURITY EXISTED IN THE get_base_year(); CONSIDER 
! BYRBAL TO
! BE EFFECTIVE ON THE ISSUE'S ANNIVERSARY DATE MATMO/MATDA/year;
! ACCOUNT FOR THE PARTIAL YEAR UNDER OLDRAT, POSSIBLY .NEQ. NEWRAT

            IF(OLDRAT /= 0.) THEN
               IN1231 = BYRBAL * OLDRAT
           PI1231 = IN1231/(1. - ((1.+OLDRAT)**(get_BASE_YEAR()-YRMAT)))
            ELSE
               IN1231 = 0.
               PI1231 = BYRBAL/(get_BASE_YEAR() - YRMAT)
            ENDIF
            PR1231 = PI1231 - IN1231
            INTEREST(I) = IN1231 * FRACT1
            BAL(I) = BYRBAL - PR1231
            SINKING_FUND(MO,I) = PR1231
         ELSE
            IF (ISSYR == YR) THEN
!              DEBT WAS ISSUED DURING THIS MODEL YEAR
               ISS(I) = AMT

               BAL(I) = AMT

            ELSE
!    THE RE-AMORTIZATION BEGINS WITHIN THE CURRENT YEAR;
!    RECOMPUTE THE YEAR'S POST-ANNIVERSARY INTEREST BY DEDUCTING
!    THE AMOUNT AT OLDRAT DURING POST-PAYMENT PART OF THE YEAR
               INTEREST(I) = INTEREST(I) - BAL(I) * OLDRAT * FRACT2

            ENDIF
         ENDIF
! NEXT COMPUTE THE ANNUAL 12/31/YR PAYMENT NEEDED TO AMORTIZE BAL(I) ...

         IF(NEWRAT /= 0.) THEN
            IN1231 = BAL(I) * NEWRAT
            PI1231 = IN1231/(1. - ((1. + NEWRAT)**(YR-YRMAT)))
         ELSE
            IN1231 = 0.
            PI1231 = BAL(I) / (YR - YRMAT)
         ENDIF
         PR1231 = PI1231 - IN1231

!     FOR DEVELOPMENT OF AMORTIZATION FACTOR ABOVE, SEE *FOOTNOTE BELOW
!  ... AND PRORATE INTEREST LINEARLY TO THE BALANCE OF THE CURRENT YEAR
         INTEREST(I) = INTEREST(I) + IN1231 * FRACT2

!   FILL OUT THE AMORTIZATION SCHEDULE, ASSUMING NEWRAT REMAINS CONSTANT
         DO J=I+1,MIN0(YRMAT-get_base_year(), 
     +      FINANCIAL_SIMULATION_YEARS)
            INTEREST(J) = IN1231 * FRACT1
            SINKING_FUND(MO,J) = PR1231
            BAL(J) = BAL(J-1) - PR1231
! VALUES ABOVE ARE VALID JUST AFTER PAYMENT ON THE ANNIVERSARY DATE

            IN1231 = BAL(J) * NEWRAT
            PR1231 = PI1231 - IN1231
            INTEREST(J) = INTEREST(J) + IN1231 * FRACT2
         ENDDO

!        ACCOUNT FOR THE FRACTIONAL OVERFLOW INTO THE YEAR OF MATURITY
         J = YRMAT - get_base_year() + 1
         IF (J <= FINANCIAL_SIMULATION_YEARS) THEN
            INTEREST(J) = IN1231 * FRACT1
            SINKING_FUND(MO,J) = BAL(J-1)

         ENDIF

      ELSEIF (I == 2) THEN
!      THE MORTGAGE SECURITY EXISTED IN THE get_base_year(), AND EXPIRES
!      THIS YEAR. BYRBAL EXISTS FOR ONLY A PORTION OF THE MATURITY
!      YEAR
       INTEREST(2) = BYRBAL * OLDRAT * FRACT1
       SINKING_FUND(MO,2) = BYRBAL
!      BAL(2) = 0. [has always been zero since initialization in DEBTC]
!      FOR (I /= 2), THE ARRAYS INT, SINKING_FUND, BAL ALREADY
!      CONTAIN PROPER VALUES FROM A PRIOR INVOCATION OF REMORT
      ENDIF
      RETURN
!
!  *FOOTNOTE ON AMORTIZATION FORMULA [ APPLICABLE ONLY FOR INTEGER M ]:
!     Using the notation that
!       r = annual interest rate
!       M = duration of loan in years
!       A = initial amount of loan
!       P = fixed annual payment of principal & interest
!       S(x,j,k) = the sum of powers x^i, where i ranges from j to k
!
!     One can show from the Present-Worth formula for payments made at
!     the end of years 1 through M
!       A = S(P * (1+r)^-i ,1,M)
!
!     and from the sum-of-powers formula
!       S(x      ,0,M-1) = (   x  ^ M - 1 ) / (   x      - 1 ) or
!       S(1/(1+r),0,M-1) = ( (1+r)^-M - 1 ) / ( (1+r)^-1 - 1 )
!
!     that
!       P = (A * r) / (1 - (1+r)^-M )
!
      END



      SUBROUTINE LTD_BOOK_2_CASH(YRMAT,MATMO,INTEREST,
     +                           INTEREST_IN_SPLIT_MONTH,
     +                           BALANCE,PAYABLE,
     +                           PAID_WHEN,PAYMENT_LAG,R_ISSMO,
     +                           CASH_PAYMENTS,
     +                           FINANCIAL_SIMULATION_YEARS)


      use class_master
      REAL*4 INTEREST(0:12,0:*),BALANCE(0:12,0:*),PAYABLE,
     +       CASH_PAYMENTS(0:12,0:*),CARRY_OVER,
     +       INTEREST_IN_SPLIT_MONTH(0:12,0:*)
      CHARACTER*1 PAID_WHEN
      INTEGER*2 PAYMENT_LAG,R_ISSMO,ISSMO,PAY_MONTH,
     +          FINANCIAL_SIMULATION_YEARS,YR,MO,YEAR,
     +          YRMAT,MATMO

      REAL*4 DIVIDENDS_DECLARED,UNPAID_DIVIDENDS
      INTEGER*2 PFS_DECLARATION_MO/3/,PS_LAG_VALUE

         ISSMO = R_ISSMO
         IF(PAID_WHEN == 'C') ISSMO = 3
         CARRY_OVER = PAYABLE ! 0.
         DO YR = 1, FINANCIAL_SIMULATION_YEARS
            IF(YR > YRMAT) EXIT
            IF(INTEREST(0,YR) == 0. .AND. CARRY_OVER == 0.) CYCLE
            DO MO = 1, 12
               IF(INTEREST(MO,YR) == 0. .AND. CARRY_OVER == 0.) CYCLE

               PAY_MONTH = MO + PAYMENT_LAG
               IF(YR == YRMAT .AND. MO == MATMO) THEN
                  CARRY_OVER = CARRY_OVER + INTEREST(MO,YR)
                  CALL CAL_LTD_INTEREST_PAID(MO,YR,CARRY_OVER,
     +                                       CASH_PAYMENTS)
                  EXIT
               ENDIF
               IF(PAID_WHEN == 'M') THEN
                 CARRY_OVER=CARRY_OVER + INTEREST_IN_SPLIT_MONTH(MO,YR)
                  CALL CAL_LTD_INTEREST_PAID(PAY_MONTH,YR,CARRY_OVER,
     +                                       CASH_PAYMENTS)
                  CARRY_OVER = INTEREST(MO,YR)
     +                         - INTEREST_IN_SPLIT_MONTH(MO,YR)
               ELSEIF(PAID_WHEN == 'J' .AND.
     +          (MO == 1 .OR. MO == 4 .OR. MO == 7 .OR. MO == 10)) THEN
                  CARRY_OVER = CARRY_OVER + INTEREST(MO,YR)
                  CALL CAL_LTD_INTEREST_PAID(PAY_MONTH,YR,CARRY_OVER,
     +                                       CASH_PAYMENTS)
                  CARRY_OVER = 0.
               ELSEIF((PAID_WHEN == 'Q' .OR. PAID_WHEN == 'C'
     +                                     .OR. PAID_WHEN == 'L') .AND.
     +             ((MO == ISSMO) .OR.
     +                 (MO == ISSMO+3 .OR. MO == ISSMO-3) .OR.
     +                     (MO == ISSMO+6 .OR. MO == ISSMO-6) .OR.

     +                        (MO == ISSMO+9 .OR. MO == ISSMO-9)) ) THEN

                  CARRY_OVER=CARRY_OVER + INTEREST_IN_SPLIT_MONTH(MO,YR)

                  CALL CAL_LTD_INTEREST_PAID(PAY_MONTH,YR,CARRY_OVER,
     +                                       CASH_PAYMENTS)
                  CARRY_OVER = INTEREST(MO,YR)
     +                         - INTEREST_IN_SPLIT_MONTH(MO,YR)
               ELSEIF((PAID_WHEN == 'S') .AND.
     +                 ((MO == ISSMO) .OR.
     +                          MO == ISSMO+6 .OR. MO == ISSMO-6)) THEN
                 CARRY_OVER=CARRY_OVER + INTEREST_IN_SPLIT_MONTH(MO,YR)
                  CALL CAL_LTD_INTEREST_PAID(PAY_MONTH,YR,CARRY_OVER,
     +                                       CASH_PAYMENTS)
                  CARRY_OVER = INTEREST(MO,YR)
     +                         - INTEREST_IN_SPLIT_MONTH(MO,YR)
               ELSEIF(PAID_WHEN == 'A' .AND. MO == ISSMO) THEN
                 CARRY_OVER=CARRY_OVER + INTEREST_IN_SPLIT_MONTH(MO,YR)
                  CALL CAL_LTD_INTEREST_PAID(PAY_MONTH,YR,CARRY_OVER,
     +                                       CASH_PAYMENTS)
                  CARRY_OVER = INTEREST(MO,YR)
     +                         - INTEREST_IN_SPLIT_MONTH(MO,YR)
               ELSEIF(INDEX('A,M,Q,S,C,L,J',PAID_WHEN) == 0) THEN

!                 FILE NOT UP DATED.  INTEREST PAID MONTHLY
                  CARRY_OVER=CARRY_OVER + INTEREST_IN_SPLIT_MONTH(MO,YR)

                  CALL CAL_LTD_INTEREST_PAID(PAY_MONTH,YR,CARRY_OVER,
     +                                       CASH_PAYMENTS)
                  CARRY_OVER = INTEREST(MO,YR)
     +                         - INTEREST_IN_SPLIT_MONTH(MO,YR)
               ELSE
                  CARRY_OVER = CARRY_OVER + INTEREST(MO,YR)
               ENDIF

            ENDDO
         ENDDO
         DO YEAR = 1, MIN(YR,FINANCIAL_SIMULATION_YEARS)
            DO MO = 1, 12
               CASH_PAYMENTS(0,YEAR) = CASH_PAYMENTS(0,YEAR) +
     +                                           CASH_PAYMENTS(MO,YEAR)
            ENDDO
         ENDDO
      RETURN

      ENTRY STOCK_BOOK_2_CASH(INTEREST,BALANCE,PAYABLE,
     +                        PAYMENT_LAG,
     +                        CASH_PAYMENTS,
     +                        FINANCIAL_SIMULATION_YEARS)

         DIVIDENDS_DECLARED = 0.

         UNPAID_DIVIDENDS = PAYABLE
         PS_LAG_VALUE = PAYMENT_LAG
         DO YR = 1, FINANCIAL_SIMULATION_YEARS
            IF(INTEREST(0,YR) == 0. .AND. UNPAID_DIVIDENDS == 0.) CYCLE
            DO MO = 1, 12
               IF(INTEREST(MO,YR) == 0. .AND.
     +                                     UNPAID_DIVIDENDS == 0.) CYCLE

               DIVIDENDS_DECLARED = DIVIDENDS_DECLARED + INTEREST(MO,YR)

               IF((MO == PFS_DECLARATION_MO) .OR.
     +                  (MO == PFS_DECLARATION_MO+3) .OR.
     +                       (MO == PFS_DECLARATION_MO+6) .OR.
     +                              (MO == PFS_DECLARATION_MO+9)) THEN

                  UNPAID_DIVIDENDS = UNPAID_DIVIDENDS +
     +                                               DIVIDENDS_DECLARED
                  DIVIDENDS_DECLARED = 0.
               ENDIF

               IF((MO == PFS_DECLARATION_MO-3+PS_LAG_VALUE) .OR.
     +              (MO == PFS_DECLARATION_MO+PS_LAG_VALUE) .OR.
     +               (MO == PFS_DECLARATION_MO+3+PS_LAG_VALUE) .OR.
     +                (MO == PFS_DECLARATION_MO+6+PS_LAG_VALUE) .OR.

     +                 (MO == PFS_DECLARATION_MO+9+PS_LAG_VALUE)) THEN
                  CASH_PAYMENTS(MO,YR) = UNPAID_DIVIDENDS
                  UNPAID_DIVIDENDS = 0.
               ENDIF
               IF(BALANCE(MO,YR) == 0.) THEN
                  CASH_PAYMENTS(MO,YR) =
     +                             CASH_PAYMENTS(MO,YR) +
     +                            UNPAID_DIVIDENDS + DIVIDENDS_DECLARED
                  UNPAID_DIVIDENDS = 0.
                  DIVIDENDS_DECLARED = 0.
                 EXIT
               ENDIF
            ENDDO
         ENDDO
         DO YEAR = 1, MIN(YR,FINANCIAL_SIMULATION_YEARS)
            DO MO = 1, 12
               CASH_PAYMENTS(0,YEAR) = CASH_PAYMENTS(0,YEAR) +
     +                                           CASH_PAYMENTS(MO,YEAR)
            ENDDO
         ENDDO
      RETURN
      END

!                                                                      C
!                   CLASS_INTEREST_PS_DIVIDENDS                        C
!        Copyright (c) 1995 M.S. Gerber & Associates, Inc.             C
!                      All Rights Reserved                             C
!                                                                      C

!                                                                      C
!     Purpose:  CLASS_INTEREST_PS_DIVIDENDS is used to calculate       C
!               The increments to LTD and PS of interest       ,       C
!               dividends, and retirements are computed.               C
!                                                                      C

      SUBROUTINE CLASS_INTEREST_PS_DIVIDENDS(RUN_YEAR,R_CLASS,
     +                                       MORTGAGE_DEBT,
     +                                       LTDINT,LTDRET,
     +                                       PSDIV,PSRED,ANNINT,
     +                                       ANN_PS_DIV,
     +                                       ANN_LTD_INT)
      use class_master
      use class_parameters
      use spindriftlib
      use prod_arrays_dimensions
      USE SIZECOM
      use class_assets_results_2
      use class_assets_results


      REAL*4 REMAIN,PSINC,CURRENT_SINKING_FUND_PAYMENT,
     +       LTD_CURRENT_INTEREST
      REAL*4 SINKING_FUND_PAYMENT,LTD_INTEREST,LTD_BAL,
     +     MTGBAL(0:MAX_FINANCIAL_SIMULATION_YEARS+1),
     +     MTGPRN(0:MAX_FINANCIAL_SIMULATION_YEARS+1),
     +     MTGINT(0:MAX_FINANCIAL_SIMULATION_YEARS+1),
     +     BEGINNING_LTD_ISSUED
      REAL*4 LTDINT(0:*),LTDRET(0:*),PSDIV(0:*),PSRED(0:*),ANNINT(0:*),
     +     ANN_PS_DIV(0:*),ANN_LTD_INT(0:*)
      REAL*4 LONG_TERM_DEBT_INC
      INTEGER*2 RUN_YEAR,TP1,I,gc_max_years,RETLTD,SINKING_YEAR,
     +          LAG_SINKING_FUND_YEARS,R_CLASS
      INTEGER*2 RUN_YEARS,EXTENSION_YEARS
      LOGICAL*1 MORTGAGE_DEBT,CPL_ACTIVE

! ADDED 5/24/94 FOR SRP M.S.G.

      LOGICAL*1 SALT_RIVER_PROJECT/.FALSE./
      REAL*4 SRP_SINKING_PATTERN(AVAIL_DATA_YEARS)
      INTEGER*2 MO
      REAL*4 LTD_ISSUED_DISTRIBUTION(0:12),ANNUAL_PS_DIV

!     COMPUTE THE INCREMENTAL CHANGES TO LTD ITEMS

      gc_max_years = RUN_YEARS() + EXTENSION_YEARS()
      TP1    = RUN_YEAR + 1
      RETLTD = RUN_YEAR + ns_class_parameters%LDLIFE
      LONG_TERM_DEBT_INC = LONG_TERM_DEBT_ISSUED -
     + ns_class_master%DEBT_FILE_LTD_ISSUED
      IF(SALT_RIVER_PROJECT)
     +                 CALL SRP_SINKING_FUNDS_INFO(SRP_SINKING_PATTERN)
      IF(LONG_TERM_DEBT_INC > 0.) THEN
         BEGINNING_LTD_ISSUED = LONG_TERM_DEBT_INC
         LTD_INTEREST = LONG_TERM_DEBT_INC * ns_class_parameters%LTDRTE
         IF(MORTGAGE_DEBT) THEN
            IF(MONTHLY_MIDAS_ACTIVE .AND. .FALSE.) THEN
               CALL RETURN_MONTNLY_LTD_ISSUES(LTD_ISSUED_DISTRIBUTION)
            ELSE
               DO MO = 1, 12
                  LTD_ISSUED_DISTRIBUTION(MO) = 0.
               ENDDO
               LTD_ISSUED_DISTRIBUTION(7) = LONG_TERM_DEBT_INC
            ENDIF
            DO MO = 1, 12
               IF(LTD_ISSUED_DISTRIBUTION(MO) == 0.) CYCLE
               LTD_BAL = LTD_ISSUED_DISTRIBUTION(MO)
             SINKING_FUND_PAYMENT = LTD_BAL * ns_class_parameters%LDSINK
               CALL MONTHLY_MTG_ADDITIONS_ALL_YEAR(MO,RUN_YEAR,
     +                                             R_CLASS,
     +                                             LTD_BAL,
     +                                       ns_class_parameters%LTDRTE,
     +                                             ANNINT,
     +                                             ANN_LTD_INT,
     +                                       ns_class_parameters%LDLIFE)
            ENDDO
         ELSE
            IF(CPL_ACTIVE()) THEN
               CALL RETURN_MONTNLY_LTD_ISSUES(LTD_ISSUED_DISTRIBUTION)
               DO MO = 1, 12
                  IF(LTD_ISSUED_DISTRIBUTION(MO) == 0.) CYCLE
                  LTD_BAL = LTD_ISSUED_DISTRIBUTION(MO)
             SINKING_FUND_PAYMENT = LTD_BAL * ns_class_parameters%LDSINK
                  CALL MONTHLY_LTD_ADDITIONS_ALL_YEAR(MO,RUN_YEAR,
     +                                             R_CLASS,
     +                                             LTD_BAL,
     +                                       ns_class_parameters%LTDRTE,
     +                                            SINKING_FUND_PAYMENT,
     +                         ns_class_parameters%LTD_SINKING_FUND_LAG,
     +                                             ANNINT,
     +                                             ANN_LTD_INT,
     +                                       ns_class_parameters%LDLIFE)
               ENDDO
            ELSE

               LTDINT(RUN_YEAR) = LTDINT(RUN_YEAR) + LTD_INTEREST/2.
               ANNINT(RUN_YEAR) = ANNINT(RUN_YEAR) + LTD_INTEREST
               ANN_LTD_INT(RUN_YEAR)=ANN_LTD_INT(RUN_YEAR)+LTD_INTEREST
               SINKING_FUND_PAYMENT = LONG_TERM_DEBT_INC *
     + ns_class_parameters%LDSINK
               REMAIN = SINKING_FUND_PAYMENT/2.
               LTD_BAL = LONG_TERM_DEBT_INC
       LAG_SINKING_FUND_YEARS = ns_class_parameters%LTD_SINKING_FUND_LAG
              CALL MONTHLY_LTD_LAST_HALF(RUN_YEAR,R_CLASS,LTD_INTEREST,
     +                                    LONG_TERM_DEBT_INC)
               IF(TP1 <= gc_max_years) THEN
                  DO I = TP1, gc_max_years
                     SINKING_YEAR = I - TP1 + 1
                     IF(SALT_RIVER_PROJECT) THEN
                        IF(SINKING_YEAR <= AVAIL_DATA_YEARS) THEN
                          SINKING_FUND_PAYMENT = BEGINNING_LTD_ISSUED *
     +                                SRP_SINKING_PATTERN(SINKING_YEAR)
                        ENDIF
                        REMAIN = SINKING_FUND_PAYMENT/2.
                        CURRENT_SINKING_FUND_PAYMENT =
     +                                             SINKING_FUND_PAYMENT
                     ELSEIF(SINKING_YEAR < LAG_SINKING_FUND_YEARS) THEN
                        REMAIN = 0.
                        CURRENT_SINKING_FUND_PAYMENT = 0.
                     ELSE
                        REMAIN = SINKING_FUND_PAYMENT/2.
                        CURRENT_SINKING_FUND_PAYMENT =
     +                                             SINKING_FUND_PAYMENT
                     ENDIF
                     IF(I < RETLTD .AND.
     +                     LTD_BAL > CURRENT_SINKING_FUND_PAYMENT) THEN
                        CALL MONTHLY_LTD_FULL_YEAR(I,R_CLASS,LTD_BAL,
     +          ns_class_parameters%LTDRTE,CURRENT_SINKING_FUND_PAYMENT)

                        LTD_BAL = LTD_BAL - CURRENT_SINKING_FUND_PAYMENT
            ANNINT(I) = ANNINT(I) + LTD_BAL * ns_class_parameters%LTDRTE
                        ANN_LTD_INT(I) = ANN_LTD_INT(I) +
     + LTD_BAL*ns_class_parameters%LTDRTE

                     ELSE
             LTD_CURRENT_INTEREST = LTD_BAL * ns_class_parameters%LTDRTE
                        CALL MONTHLY_LTD_FIRST_HALF(I,R_CLASS,
     +                                     LTD_CURRENT_INTEREST,LTD_BAL)

                        EXIT
                     ENDIF
                  ENDDO
               ENDIF
            ENDIF ! CP&L
         ENDIF
      ENDIF

!     COMPUTE THE PREFERRED STOCK ITEMS

      PSINC =  PREFERRED_STOCK_ISSUED -
     + ns_class_master%DEBT_FILE_PREFERRED_ISSUED
      IF(PSINC > 0.) THEN
         PSDIV(RUN_YEAR) = PSDIV(RUN_YEAR) +
     + ns_class_parameters%PSRATE * PSINC/2.
         SINKING_FUND_PAYMENT = PSINC * ns_class_parameters%PSSINK
         REMAIN = SINKING_FUND_PAYMENT/2.
         ANNINT(RUN_YEAR) = ANNINT(RUN_YEAR) + PSINC *
     + ns_class_parameters%PSRATE
         ANN_PS_DIV(RUN_YEAR) = ANN_PS_DIV(RUN_YEAR) + PSINC *
     + ns_class_parameters%PSRATE
        LAG_SINKING_FUND_YEARS = ns_class_parameters%PS_SINKING_FUND_LAG
         ANNUAL_PS_DIV = ns_class_parameters%PSRATE * PSINC
         CALL MONTHLY_PS_LAST_HALF(RUN_YEAR,R_CLASS,ANNUAL_PS_DIV,
     +                                 PSINC)
         IF(TP1 <= gc_max_years) THEN
            DO I = TP1, gc_max_years
               SINKING_YEAR = I - TP1 + 1
               IF(SINKING_YEAR < LAG_SINKING_FUND_YEARS) THEN
                  REMAIN = 0.
                  CURRENT_SINKING_FUND_PAYMENT = 0.
               ELSE
                  REMAIN = SINKING_FUND_PAYMENT/2.
                  CURRENT_SINKING_FUND_PAYMENT = SINKING_FUND_PAYMENT
               ENDIF
               IF(PSINC > CURRENT_SINKING_FUND_PAYMENT) THEN
                  CALL MONTHLY_PS_FULL_YEAR(I,R_CLASS,PSINC,
     +          ns_class_parameters%PSRATE,CURRENT_SINKING_FUND_PAYMENT)

                  PSINC    = PSINC - CURRENT_SINKING_FUND_PAYMENT
              ANNINT(I) = ANNINT(I) + PSINC * ns_class_parameters%PSRATE
      ANN_PS_DIV(I) = ANN_PS_DIV(I) + PSINC * ns_class_parameters%PSRATE
               ELSE

! ADDED 4/27/88 from FIN
                  ANNUAL_PS_DIV = ns_class_parameters%PSRATE * PSINC
                  CALL MONTHLY_PS_FIRST_HALF(I,R_CLASS,ANNUAL_PS_DIV,
     +                                                            PSINC)

                  EXIT
               ENDIF
            ENDDO
         ENDIF
      ENDIF

         CALL RESET_CARRY_OVERS()

      RETURN
      END

      SUBROUTINE MGT_DEBT_PandI_PAYMENTS(YR,PAID_WHEN,SINKING_FUND,
     +                                   SINKING_FUND_PAYMENT,
     +                                   R_ISSUE_MO,
     +                                   R_ISSUE_YR,
     +                                   CURRENT_YR,
     +                                   PAYMENT_LAGS_ISSUE_MONTH)

      REAL*4 SINKING_FUND(0:12,0:*),SINKING_FUND_PAYMENT
      CHARACTER*1 PAID_WHEN
      INTEGER*2 MO,YR,PAY_MO,START_PAYING_YR,CURRENT_YR,
     +          R_ISSUE_MO,
     +          R_ISSUE_YR,
     +          PAYMENT_LAGS_ISSUE_MONTH
      LOGICAL*1 PAY_BY_QRT,WVPA

         START_PAYING_YR = R_ISSUE_YR
         PAY_MO = R_ISSUE_MO
         IF(PAID_WHEN == 'C') THEN
            IF(R_ISSUE_MO <= 12) PAY_MO = 12
            IF(R_ISSUE_MO <= 9) PAY_MO = 9
            IF(R_ISSUE_MO <= 6) PAY_MO = 6
            IF(R_ISSUE_MO <= 3) PAY_MO = 3
         ENDIF
         IF(PAID_WHEN == 'L') THEN
            IF(R_ISSUE_MO <= 12) PAY_MO = 12
            IF(R_ISSUE_MO <= 9) PAY_MO = 9
            IF(R_ISSUE_MO <= 6) PAY_MO = 6
            IF(R_ISSUE_MO <= 3) PAY_MO = 3
            IF(WVPA()) THEN
               PAY_MO = PAY_MO + 1
            ELSE
               PAY_MO = PAY_MO + PAYMENT_LAGS_ISSUE_MONTH
            ENDIF
         ENDIF
         IF(PAID_WHEN == 'Q') PAY_MO = R_ISSUE_MO + 2
         IF(PAID_WHEN == 'S') PAY_MO = R_ISSUE_MO + 5
         IF(PAID_WHEN == 'A') PAY_MO = R_ISSUE_MO + 11
         IF(PAY_MO > 12) THEN
            PAY_MO = PAY_MO - 12
            START_PAYING_YR = START_PAYING_YR + 1
         ENDIF

         IF(CURRENT_YR < START_PAYING_YR) RETURN
        PAY_BY_QRT = PAID_WHEN=='Q'.OR.PAID_WHEN=='C'.OR.PAID_WHEN=='L'
         DO MO = 1, 12
            IF(MO < PAY_MO .AND. CURRENT_YR == START_PAYING_YR) CYCLE
            IF(PAID_WHEN == 'M') THEN
               SINKING_FUND(MO,YR) = SINKING_FUND_PAYMENT

            ELSEIF(PAY_BY_QRT .AND.
     +             ((MO == PAY_MO) .OR.
     +                 (MO == PAY_MO+3 .OR. MO == PAY_MO-3) .OR.
     +                     (MO == PAY_MO+6 .OR. MO == PAY_MO-6) .OR.
     +                     (MO == PAY_MO+9 .OR. MO == PAY_MO-9)) ) THEN
               SINKING_FUND(MO,YR) = SINKING_FUND_PAYMENT

            ELSEIF((PAID_WHEN == 'S') .AND.
     +                 ((MO == PAY_MO) .OR.
     +                         MO == PAY_MO+6 .OR. MO == PAY_MO-6)) THEN
               SINKING_FUND(MO,YR) = SINKING_FUND_PAYMENT

            ELSEIF(PAID_WHEN == 'A' .AND. MO == PAY_MO) THEN
                  SINKING_FUND(MO,YR) = SINKING_FUND_PAYMENT
            ENDIF
         ENDDO
         SINKING_FUND(0,YR) = SUM(SINKING_FUND(1:,YR))

      RETURN
      END

      SUBROUTINE ZERO_REMAINING_MONTHS(MONTHLY_ARRAY,I,MO)

      use class_master
      implicit none
      REAL*4 MONTHLY_ARRAY(0:12,*)
      INTEGER*2 I,MO,FINANCIAL_SIMULATION_YEARS,YR
      INTEGER*2 RUN_YEARS,EXTENSION_YEARS

         FINANCIAL_SIMULATION_YEARS =
     +                           MAX(5,RUN_YEARS()+EXTENSION_YEARS()+1)
         MONTHLY_ARRAY(MO:12,I) = 0.
         MONTHLY_ARRAY(0,I) = SUM(MONTHLY_ARRAY(1:,I))

         MONTHLY_ARRAY(:,I+1:FINANCIAL_SIMULATION_YEARS) = 0.
      RETURN
      END

!                                                                      C
!                   CLASS_INTEREST_PS_DIVIDENDS                        C
!        Copyright (c) 1995 M.S. Gerber & Associates, Inc.             C
!                      All Rights Reserved                             C
!                                                                      C

!                                                                      C
!     Purpose:  CLASS_INTEREST_PS_DIVIDENDS is used to calculate       C
!               The increments to LTD and PS of interest       ,       C
!               dividends, and retirements are computed.               C
!                                                                      C

      SUBROUTINE CLASS_FINANCE_DETAIL_RPT(RUN_YEAR,
     +                                    R_CLASS,
     +                                    ClassName,
     +                                    MORTGAGE_DEBT)

      use class_master
      use irec_endpoint_control
      use grx_planning_routines
      use globecom
      use spindriftlib
      use prod_arrays_dimensions
      use sizecom
      use globecom
      use class_assets_results
      use endpoint
      use class_parameters
      use mod_base_year
      use msgmmdbt_decs
      use globecom
      implicit none

      CHARACTER (LEN=*) :: ClassName
      REAL (KIND=4) :: REMAIN,CURRENT_SINKING_FUND_PAYMENT,
     +       LTD_CURRENT_INTEREST
      REAL (KIND=4) :: SINKING_FUND_PAYMENT,LTD_INTEREST,LTD_BAL,
     +       BEGINNING_LTD_ISSUED
      REAL (KIND=4) :: MONTHLY_INTEREST
      REAL (KIND=4) :: LONG_TERM_DEBT_INC,PSINC
      INTEGER (KIND=2) :: RUN_YEAR,TP1,I,SINKING_YEAR,
     +          LAG_SINKING_FUND_YEARS,R_CLASS

      INTEGER (KIND=2) :: RUN_YEARS,EXTENSION_YEARS
      LOGICAL (KIND=1) :: MORTGAGE_DEBT,CPL_ACTIVE

! ADDED 5/24/94 FOR SRP M.S.G.

      LOGICAL (KIND=1) :: SALT_RIVER_PROJECT/.FALSE./
      REAL (KIND=4) :: SRP_SINKING_PATTERN(AVAIL_DATA_YEARS)
      INTEGER (KIND=2) :: YR,START_MO
      REAL (KIND=4) :: LTD_ISSUED_DISTRIBUTION(0:12),ANNUAL_PS_DIV,
     +                 LTD_LIFE,LTD_RETIRE
      REAL (KIND=4), DIMENSION(0:12) :: Balance,
     +                                  IssueAmount,
     +                                  InterestExp,
     +                                  CashInterestPayments,
     +                                  SinkingFundPayments
      CHARACTER (LEN=1) :: PAID_WHEN

      INTEGER (KIND=2) OVERHEAD_LENGTH,
     +                 VARIABLE_NUMBER,
     +                 RECORD_LENGTH,
     +                 ESTABLISH_DETAIL_REPORT_FILE_WO,
     +                 DIMENSIONS
      INTEGER (KIND=2) :: RPT_UNIT,TEMP_REC
      INTEGER (KIND=4), SAVE :: NEXT_REC
      CHARACTER (LEN=256) :: FILE_NAME,GET_RESULTS_DIRECTORY
      CHARACTER (LEN=5) :: GET_SCENAME
      CHARACTER (LEN=6) :: SHORT_MONTH_NAMES
      CHARACTER (LEN=4) :: YEAR_STR
      CHARACTER (LEN=50) :: ISSUE_NAME

      REAL (KIND=4) :: PAY_PERIODS_PER_YR,PIBIEN
      LOGICAL (KIND=4) :: THIS_IS_A_PAY_MONTH

!     COMPUTE THE INCREMENTAL CHANGES TO LTD ITEMS

      TP1 = RUN_YEAR + 1

      LONG_TERM_DEBT_INC = LONG_TERM_DEBT_ISSUED -
     + ns_class_master%DEBT_FILE_LTD_ISSUED
      IF(LONG_TERM_DEBT_INC > 0.) THEN
         IF(.NOT. REPORT_HEADER_OPEN) THEN
            FINANCIAL_SIMULATION_YEARS = MAX(5,
     +        get_globecom_study_period()+get_EXTENSION_PERIOD()+1)
            FILE_NAME = TRIM(GET_RESULTS_DIRECTORY())//"MSG"//
     +                          TRIM(GET_SCENAME())//".BCD"
            OVERHEAD_LENGTH = 46
            VARIABLE_NUMBER = 40
            DIMENSIONS = 4
            RPT_UNIT = 7337
            RECORD_LENGTH = ESTABLISH_DETAIL_REPORT_FILE_WO(FILE_NAME,
     +                                                 OVERHEAD_LENGTH,
     +                                                 VARIABLE_NUMBER,
     +                                                  DIMENSIONS,
     +                                                  RPT_UNIT,
     +                                                  TEMP_REC)

            NEXT_REC = TEMP_REC
            WRITE(RPT_UNIT,REC=NEXT_REC)
     +                       'Endpoint            ','N',INT2(4),'V','D'
            WRITE(RPT_UNIT,REC=NEXT_REC+1)
     +                       'Year                ','N',INT2(4),'V','D'
            WRITE(RPT_UNIT,REC=NEXT_REC+2)
     +                      'Issue Name          ','C',INT2(50),'V','D'
            WRITE(RPT_UNIT,REC=NEXT_REC+3)
     +              'Period              ','C',INT2(6),'F','D'
            NEXT_REC = NEXT_REC + 4

            REPORT_HEADER_OPEN = .TRUE.
         ENDIF

         PAID_WHEN = 'Q'
         LTD_CASH_CARRY_OVER = 0.
         LTD_RETIRE = ns_class_parameters%LDSINK * LONG_TERM_DEBT_INC
         LTD_ISSUED_DISTRIBUTION = 0.
         LTD_ISSUED_DISTRIBUTION(7) = LONG_TERM_DEBT_INC
         IF(RUN_YEAR <= LAST_AVAILABLE_MONTHLY_YEAR .AND.
     +                                       MONTHLY_MIDAS_ACTIVE) THEN
            CALL RETURN_MONTNLY_LTD_ISSUES(LTD_ISSUED_DISTRIBUTION)
         ENDIF
         IF(SALT_RIVER_PROJECT)
     +                 CALL SRP_SINKING_FUNDS_INFO(SRP_SINKING_PATTERN)
         IF(MORTGAGE_DEBT) THEN
            PAID_WHEN = 'Q'
            MTGLIF = MAX(1,INT(ns_class_parameters%LDLIFE))
            IF(PAID_WHEN == 'M') PAY_PERIODS_PER_YR = 12.
            IF(PAID_WHEN == 'S') PAY_PERIODS_PER_YR = 2.
            IF(PAID_WHEN == 'Q') PAY_PERIODS_PER_YR = 4.
            IF(PAID_WHEN == 'A') PAY_PERIODS_PER_YR = 1.

            DO ISSMO = 1, 12
               IF(LTD_ISSUED_DISTRIBUTION(ISSMO) == 0.) CYCLE
               LTD_BAL = LTD_ISSUED_DISTRIBUTION(ISSMO)
           SINKING_FUND_PAYMENT = LTD_BAL * ns_class_parameters%LDSINK
               IF(ns_class_parameters%LTDRTE /= 0.) THEN
                  PIBIEN = (LTD_BAL *
     + ns_class_parameters%LTDRTE/PAY_PERIODS_PER_YR)/
     + (1. - (1./(1. + ns_class_parameters%LTDRTE/PAY_PERIODS_PER_YR)**
     +                             (INT(PAY_PERIODS_PER_YR)*MTGLIF)))
               ELSE
                  PIBIEN = LTD_BAL/MAX(1.,PAY_PERIODS_PER_YR*MTGLIF)
               ENDIF
               InterestExp = 0.
               CashInterestPayments = 0.
               SinkingFundPayments = 0.
               IssueAmount = 0.
               Balance = 0.
             MONTHLY_INTEREST = ns_class_parameters%LTDRTE * LTD_BAL/12.
               InterestExp(ISSMO) = MONTHLY_INTEREST
               LTD_CASH_CARRY_OVER = MONTHLY_INTEREST
               IssueAmount(0) = LTD_BAL
               IssueAmount(ISSMO) = LTD_BAL
               Balance(ISSMO) = LTD_BAL
               WRITE(YEAR_STR,'(I4)') RUN_YEAR+get_BASE_YEAR()
               ISSUE_NAME = TRIM(ClassName)//"-"//
     +                   TRIM(SHORT_MONTH_NAMES(ISSMO))//", "//YEAR_STR
               START_MO = ISSMO + 1
              DO YR = RUN_YEAR, MIN(RUN_YEAR+ns_class_parameters%LDLIFE,
     +                                 FINANCIAL_SIMULATION_YEARS)

                  DO MO = START_MO, 12
                     IF(LTD_BAL == 0.) EXIT
                     IF(THIS_IS_A_PAY_MONTH(ISSMO,MO,PAID_WHEN)) THEN
                        CashInterestPayments(MO) =
     +                                         LTD_CASH_CARRY_OVER
                       IF(YR == RUN_YEAR+MTGLIF .AND. MO == ISSMO) THEN
                           LTD_RETIRE = LTD_BAL
                           LTD_BAL = 0.
                        ELSE
                           LTD_RETIRE = PIBIEN-LTD_CASH_CARRY_OVER
                           LTD_BAL = LTD_BAL - LTD_RETIRE
                        ENDIF
                        SinkingFundPayments(MO) = LTD_RETIRE
                        LTD_CASH_CARRY_OVER = 0.
                     ENDIF

                     Balance(MO) = LTD_BAL
             MONTHLY_INTEREST = ns_class_parameters%LTDRTE * LTD_BAL/12.
                     InterestExp(MO) = MONTHLY_INTEREST
                     LTD_CASH_CARRY_OVER = LTD_CASH_CARRY_OVER +
     +                                               MONTHLY_INTEREST
                  ENDDO
                  InterestExp(0) = SUM(InterestExp(1:12))
                  CashInterestPayments(0) =
     +                              SUM(CashInterestPayments(1:12))
                  SinkingFundPayments(0) =
     +                              SUM(SinkingFundPayments(1:12))
                  WRITE(7337,REC=NEXT_REC) PRT_ENDPOINT(),
     +                                  FLOAT(YR+get_BASE_YEAR()),
     +                                  ISSUE_NAME,
     +                                  "Year  ",
     +                               Balance(0),      ! opening balance
     +                                  IssueAmount(0),
     +                                  SinkingFundPayments(0),
     +                              Balance(12),      ! opening balance
     +                                  InterestExp(0),
     +                                  CashInterestPayments(0),
     +                                  100.*ns_class_parameters%LTDRTE
                  NEXT_REC = NEXT_REC + 1
                  DO MO = 1, 12
                     WRITE(7337,REC=NEXT_REC) PRT_ENDPOINT(),
     +                                     FLOAT(YR+get_BASE_YEAR()),
     +                                     ISSUE_NAME,
     +                                     SHORT_MONTH_NAMES(MO),
     +                            Balance(MO-1),      ! opening balance
     +                                     IssueAmount(MO),
     +                                     SinkingFundPayments(MO),
     +                             Balance(MO),      ! closing balance
     +                                     InterestExp(MO),
     +                                     CashInterestPayments(MO),
     +                                   100.*ns_class_parameters%LTDRTE
                     NEXT_REC = NEXT_REC + 1
                  ENDDO
                  Balance(0) = Balance(12)
                  Balance(1:12) = 0.
                  InterestExp = 0.
                  CashInterestPayments = 0.
                  SinkingFundPayments = 0.
                  IssueAmount = 0.
                  START_MO = 1
                  IF(YR == RUN_YEAR + ns_class_parameters%LDLIFE) EXIT
                  IF(LTD_BAL == 0.) EXIT
               ENDDO
            ENDDO

! END OF MORTGAGE

         ELSE
            DO ISSMO = 1, 12
               IF(LTD_ISSUED_DISTRIBUTION(ISSMO) == 0.) CYCLE
               LTD_BAL = LTD_ISSUED_DISTRIBUTION(ISSMO)
             SINKING_FUND_PAYMENT = LTD_BAL * ns_class_parameters%LDSINK
               LAG_SINKING_FUND_YEARS = LAG_SINKING_FUND_YEARS
               InterestExp = 0.
               CashInterestPayments = 0.
               SinkingFundPayments = 0.
               IssueAmount = 0.
               Balance = 0.
             MONTHLY_INTEREST = ns_class_parameters%LTDRTE * LTD_BAL/12.
               InterestExp(ISSMO) = MONTHLY_INTEREST
               LTD_CASH_CARRY_OVER = MONTHLY_INTEREST
               IssueAmount(0) = LTD_BAL
               IssueAmount(ISSMO) = LTD_BAL
               Balance(ISSMO) = LTD_BAL
               WRITE(YEAR_STR,'(I4)') RUN_YEAR+get_BASE_YEAR()
               ISSUE_NAME = TRIM(ClassName)//"-"//
     +                  TRIM(SHORT_MONTH_NAMES(ISSMO))//", "//YEAR_STR
               START_MO = ISSMO + 1
              DO YR = RUN_YEAR, MIN(RUN_YEAR+ns_class_parameters%LDLIFE,
     +                                    FINANCIAL_SIMULATION_YEARS)

                 IF(MONTHLY_MIDAS_ACTIVE) THEN
                  DO MO = START_MO, 12
                     IF(MO == ISSMO .AND.
     +                    YR >= RUN_YEAR+LAG_SINKING_FUND_YEARS) THEN
                     IF(YR == RUN_YEAR + ns_class_parameters%LDLIFE .OR.
     +                                      LTD_RETIRE >= LTD_BAL) THEN
                           LTD_RETIRE = LTD_BAL
               MONTHLY_INTEREST = LTD_BAL*ns_class_parameters%LTDRTE/12.
                           InterestExp(MO) = MONTHLY_INTEREST
                           CashInterestPayments(MO) =
     +                                         CashInterestPayments(MO)
     +                                          + MONTHLY_INTEREST
                           LTD_BAL = 0.
                        ELSE
                           LTD_BAL = LTD_BAL - LTD_RETIRE
                        ENDIF
                        SinkingFundPayments(MO) = LTD_RETIRE
                        Balance(MO) = LTD_BAL
                        IF(YR == RUN_YEAR+ns_class_parameters%LDLIFE
     + .OR. LTD_BAL==0.) then
                            EXIT
                         endif
                     ENDIF
                     Balance(MO) = LTD_BAL
               MONTHLY_INTEREST = LTD_BAL*ns_class_parameters%LTDRTE/12.
                     InterestExp(MO) = MONTHLY_INTEREST
                     IF(THIS_IS_A_PAY_MONTH(ISSMO,MO,PAID_WHEN)) THEN
                        CashInterestPayments(MO) =
     +                                            LTD_CASH_CARRY_OVER
                        LTD_CASH_CARRY_OVER = 0.
                     ENDIF
                     LTD_CASH_CARRY_OVER = LTD_CASH_CARRY_OVER
     +                                     + MONTHLY_INTEREST
                  ENDDO
                 ELSE

                  DO MO = START_MO, 12
                     IF(MO == ISSMO .AND.
     +                    YR > RUN_YEAR+LAG_SINKING_FUND_YEARS) THEN
                    IF(YR == RUN_YEAR + ns_class_parameters%LDLIFE .OR.
     +                                      LTD_RETIRE >= LTD_BAL) THEN
                           LTD_RETIRE = LTD_BAL
               MONTHLY_INTEREST = LTD_BAL*ns_class_parameters%LTDRTE/12.
                           InterestExp(MO) = MONTHLY_INTEREST
                           CashInterestPayments(MO) =
     +                                         CashInterestPayments(MO)
     +                                          + MONTHLY_INTEREST
                           LTD_BAL = 0.
                        ELSE
                           LTD_BAL = LTD_BAL - LTD_RETIRE
                        ENDIF
                        SinkingFundPayments(MO) = LTD_RETIRE
                        Balance(MO) = LTD_BAL
                        IF(YR == RUN_YEAR+ns_class_parameters%LDLIFE
     + .OR. LTD_BAL==0.) then
                            EXIT
                         endif
                     ENDIF
                     Balance(MO) = LTD_BAL
               MONTHLY_INTEREST = LTD_BAL*ns_class_parameters%LTDRTE/12.
                     InterestExp(MO) = MONTHLY_INTEREST
                     LTD_CASH_CARRY_OVER = LTD_CASH_CARRY_OVER
     +                                     + MONTHLY_INTEREST
                     IF(MO == ISSMO - 1 .OR. MO == 12) THEN
                        CashInterestPayments(MO) =
     +                                            LTD_CASH_CARRY_OVER
                        LTD_CASH_CARRY_OVER = 0.
                     ENDIF
                  ENDDO
                 ENDIF
                  InterestExp(0) = SUM(InterestExp(1:12))
                  CashInterestPayments(0) =
     +                                 SUM(CashInterestPayments(1:12))
                  SinkingFundPayments(0) =
     +                                 SUM(SinkingFundPayments(1:12))
                  WRITE(7337,REC=NEXT_REC) PRT_ENDPOINT(),
     +                                     FLOAT(YR+get_BASE_YEAR()),
     +                                     ISSUE_NAME,
     +                                     "Year  ",
     +                         Balance(0),      ! opening balance
     +                                     IssueAmount(0),
     +                                     SinkingFundPayments(0),
     +                            Balance(12),      ! opening balance
     +                                     InterestExp(0),
     +                                     CashInterestPayments(0),
     +                                   100.*ns_class_parameters%LTDRTE
                  NEXT_REC = NEXT_REC + 1
                  DO MO = 1, 12
                     WRITE(7337,REC=NEXT_REC) PRT_ENDPOINT(),
     +                                        FLOAT(YR+get_BASE_YEAR()),
     +                                        ISSUE_NAME,
     +                                        SHORT_MONTH_NAMES(MO),
     +                           Balance(MO-1),      ! opening balance
     +                                        IssueAmount(MO),
     +                                        SinkingFundPayments(MO),
     +                              Balance(MO),      ! closing balance
     +                                        InterestExp(MO),
     +                                        CashInterestPayments(MO),
     +                                   100.*ns_class_parameters%LTDRTE
                     NEXT_REC = NEXT_REC + 1
                  ENDDO
                  Balance(0) = Balance(12)
                  Balance(1:12) = 0.
                  InterestExp = 0.
                  CashInterestPayments = 0.
                  SinkingFundPayments = 0.
                  IssueAmount = 0.
                  START_MO = 1
                  IF(YR == RUN_YEAR + ns_class_parameters%LDLIFE) EXIT
               ENDDO
            ENDDO
         ENDIF
      ENDIF
      return ! temp

!     COMPUTE THE PREFERRED STOCK ITEMS

      IF(PSINC > 0.) THEN
         PSINC =  PREFERRED_STOCK_ISSUED -
     + ns_class_master%DEBT_FILE_PREFERRED_ISSUED
         SINKING_FUND_PAYMENT = PSINC * ns_class_parameters%PSSINK
         REMAIN = SINKING_FUND_PAYMENT/2.
        LAG_SINKING_FUND_YEARS = ns_class_parameters%PS_SINKING_FUND_LAG
         ANNUAL_PS_DIV = ns_class_parameters%PSRATE * PSINC
         CALL MONTHLY_PS_LAST_HALF(RUN_YEAR,R_CLASS,ANNUAL_PS_DIV,
     +                                 PSINC)
         IF(TP1 <= gc_max_years) THEN
            DO I = TP1, gc_max_years
               SINKING_YEAR = I - TP1 + 1
               IF(SINKING_YEAR < LAG_SINKING_FUND_YEARS) THEN
                  REMAIN = 0.
                  CURRENT_SINKING_FUND_PAYMENT = 0.
               ELSE
                  REMAIN = SINKING_FUND_PAYMENT/2.
                  CURRENT_SINKING_FUND_PAYMENT = SINKING_FUND_PAYMENT
               ENDIF
               IF(PSINC > CURRENT_SINKING_FUND_PAYMENT) THEN
                  CALL MONTHLY_PS_FULL_YEAR(I,R_CLASS,PSINC,
     +      ns_class_parameters%PSRATE,CURRENT_SINKING_FUND_PAYMENT)
                  PSINC    = PSINC - CURRENT_SINKING_FUND_PAYMENT
               ELSE
                  ANNUAL_PS_DIV = ns_class_parameters%PSRATE * PSINC
                  CALL MONTHLY_PS_FIRST_HALF(I,R_CLASS,ANNUAL_PS_DIV,
     +                                                           PSINC)
                  EXIT
               ENDIF
            ENDDO
         ENDIF
      ENDIF
      RETURN
      END SUBROUTINE CLASS_FINANCE_DETAIL_RPT

