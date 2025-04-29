module msgmmdbt_decs
use prod_arrays_dimensions
use globecom
use mthnmcom
implicit none

    integer (kind=2) :: financial_simulation_years=0
    CHARACTER*1 INTRA_COMPANY
    real (kind=4), allocatable :: issps(:,:,:)
    real (kind=4), allocatable :: retps(:,:,:)
    real (kind=4), allocatable :: issltd(:,:,:)

      LOGICAL (kind=1) ::  SAVE_BASE_CASE, &
       STOP_AT_MATURITY,VECTOR_FOUND

      REAL*4 DAYS_IN_CURRENT_YEAR
      
      


      INTEGER*2 :: ASSET_CLASS,ASSET_CLASS_VECTOR

      INTEGER*2 MONTH
      REAL*4 BALANCE_OF_MONTH, &
             ANNUAL_SUM
      integer (kind=2) :: num_of_asset_classes
      REAL*4 ASSET_ALLOCATION_LIST(:),ASSET_ALLOCATOR
      ALLOCATABLE :: ASSET_ALLOCATION_LIST
      
      INTEGER*2 NUM_OF_BASE_CASE_ASSET_CLASSES, &
                MAX_BASE_CASE_ASSET_CLASS_NUM
      
      
      INTEGER*2 MAX_ASSET_CLASS_NUM/0/
      
      real (kind=4) :: PS_BASEYR,LTD_BASEYR,COMMON_BASEYR,INVESTMENT_BY_BALANCE
      INTEGER*4 VALUES_TO_ZERO
      REAL*4 VECTOR_CLASS_ALLOCATIONS(0:AVAIL_DATA_YEARS)
!
      INTEGER*2 ISSMO,ISSDA,ISSYR,MATMO,MATDA,YRMAT,ITYPE,SFY1,SFY2
      REAL*4 AMT,BYRBAL,IRATE,SFA1,SFA2
      INTEGER*2 R_PERIOD
!
      INTEGER*2 CLASS_POINTER
      INTEGER (KIND=2), ALLOCATABLE :: ASSET_CLASS_POINTER(:)
      REAL (KIND=4), ALLOCATABLE :: &
                    RETLTD(:,:,:),INTLTD(:,:,:), &
                    CURRENT_PORTION_OF_LTD(:,:,:), &
                    INTPS(:,:,:),SFPS(:,:,:),SFLTD(:,:,:), &
                   OTHER_INCOME(:,:,:),INVESTMENT_IN_AFILLIATES(:,:,:), &
                    INTEREST_INCOME(:,:,:),LOANS_TO_AFILLIATES(:,:,:), &
                    LTD_CASH_INTEREST(:,:,:), &
                    PS_CASH_DIVIDEND(:,:,:), &
                    AINTPS(:,:),AINTDB(:,:), &
                    PS_PREM_BAL(:,:,:),PS_PREM_AMORT(:,:,:), &
                    LTD_PREM_BAL(:,:,:),LTD_PREM_AMORT(:,:,:), &
                    PS_ISSUE_EXP_BAL(:,:,:),PS_ISSUE_EXP_AMORT(:,:,:), &
                   LTD_ISSUE_EXP_BAL(:,:,:),LTD_ISSUE_EXP_AMORT(:,:,:), &
                    LTD_PS_ISSUE_TAX_EXPENSE(:,:,:), &
                    MIPS_INCOME_TAX_DEDUCTION(:,:), &
                    COMMON_STOCK_ISSUED_AMOUNT(:,:,:,:), &
                    COMMON_STOCK_BUYBACK_AMOUNT(:,:,:,:), &
                    COMMON_STOCK_BUYBACK_SHARES(:,:,:,:), &
                    COMMON_STOCK_ISSUED_SHARES(:,:,:,:), &
                    COMMON_STOCK_BALANCE(:,:,:), &
                    COMMON_STOCK_SHARES_OUTSTANDING(:,:,:), &
                    PURCHASED_SHARES_RE_ADJ(:,:,:), &
                    CS_ISSUE_EXP_BAL(:,:,:), &
                    CS_ISSUE_EXP_AMORT(:,:,:), &
                    CS_ISSUE_TAX_EXPENSE(:,:,:), &
                    LTD_NEW_ISSUE_PREM_DISC_AMOUNT(:,:,:), &
                    PS_NEW_ISSUE_PREM_DISC_AMOUNT(:,:,:), &
                    ASSET_CLASS_LIST(:)
      REAL (KIND=4), ALLOCATABLE :: INVESTMENT_INCOME(:,:,:,:), &
                                    NOTES_RECEIVABLE(:,:,:,:), &
                                    NOTES_PAYABLE(:,:,:,:)
      REAL (KIND=4), ALLOCATABLE :: SINKING_FUND(:,:),RATE(:,:), &
                                    BAL(:,:),RET(:,:),INTEREST(:,:), &
                                    ISS(:,:),CASH_PAYMENTS(:,:), &
                                    SHARES_ISSUED(:,:), &
                                    SHARES_PURCHASED(:,:), &
                                    SHARES_OUTSTANDING(:,:), &
                                    ADJ_RETAINED_EARNINGS(:,:), &
                                    INTEREST_EXPENSE_ADJ(:,:), &
                                    INTEREST_IN_SPLIT_MONTH(:,:), &
                                    LTD_CURRENT_PORTION(:,:), &
                                    ANNINT(:)
      LOGICAL*1 RETIREMENTS_ACTIVE,ISSUE_ACURRED
      REAL (KIND=4) :: RPT_RATE
!
!  PREMIUM/DISCOUNT AMORTIZATION
!
      REAL (KIND=4), ALLOCATABLE :: PREMIUM_BAL(:,:), &
       PREMIUM_AMORT(:,:), &
                       ISSUE_EXPENSE_AMORT(:,:),ISSUE_EXPENSE_BAL(:,:), &
                        ISSUE_TAX_EXPENSE(:,:), &
                        PREMIUM_DISCOUNT_EXP(:,:)
      REAL*4 YEARS_2_AMORTIZE,AMORT_PERIOD_THIS_YEAR,ANNUAL_AMORT, &
             MONTH_AMORT,MONTHS_2_AMORTIZE, &
             DAILY_RATE
      INTEGER*2 AMORTIZATION_START_YR
      CHARACTER*3 PFS,LTD,MTG,COMMON
      CHARACTER*3 MIPS,INVESTMENT,NOTE_RECEIVABLE,NOTE_PAYABLE
      PARAMETER (PFS='PFS',MTG='MTG',MIPS='MIP',LTD='LTD',COMMON='COM', &
                 INVESTMENT='INV', &
                 NOTE_RECEIVABLE='REC',NOTE_PAYABLE='PAY')
      REAL*4 LTD_CASH_CARRY_OVER,PS_CASH_CARRY_OVER
!
      CHARACTER*1 DATA_TYPE,VECTOR_TYPE*20
      REAL*4 ANNUAL_VECTOR_VALUES(AVAIL_DATA_YEARS)
      REAL*4 VECTOR_MONTHLY_DATA(12,LAST_AVAILABLE_MONTHLY_YEAR)

      
      INTEGER*2 MONTH_ENDING(LAST_AVAILABLE_MONTHLY_YEAR),MO
      REAL*4 TREND_NORM
!


      REAL*4 TOTAL_REPORT_VARS(0:12,30,0:4)
      INTEGER*2 VarNo
      INTEGER*2 Principal Balance, &
                Principal Payment, &
                Book Interest Expense, &
                Cash Interest Expense, &
                Cash PnI Payment
      PARAMETER (Principal Balance=0, &
                 Principal Payment=1, &
                 Book Interest Expense=2, &
                 Cash Interest Expense=3, &
                 Cash PnI Payment=4)

      INTEGER*2 INTRA_CLASS_ID,SINKING_FUND_VECTOR
      LOGICAL*1 ANNUAL_INFO_ACTIVE
      CHARACTER (len=1) :: INTEREST_PAID_WHEN, &
                 INVESTMENT_TYPE, &
                 EQUITY_MARKET
      REAL (kind=4) :: COMMON_SHARES, &
            ACCRUED_INTEREST_PAYABLE
     REAL(kind=4) ::  PREM_DISC_BALANCE, &
            ISSUE_EXPENSE_BALANCE
     INTEGER (kind=2) :: DELETE,IREC,INUNIT,LRECL=256
!  OPEN DETAIL REPORT FILE
!
      LOGICAL(kind=1) :: REPORT_HEADER_OPEN=.FALSE.
      CHARACTER (len=3) :: TYPE
      character (len=30) :: DESC, TEMP_DESC, &
        WVPA_REPORT_CATEGORY
      character (len=50) :: COMMENT
      CHARACTER (len=20) :: LHS_DISTRIBUTION
      INTEGER*1, parameter :: Equity=1,Parent=2

      INTEGER*2 MTGLIF,MO_START,MO1,AMORT_YR
      


      CHARACTER(len=1) :: MONTHLY_DATA_UNITS(LAST_AVAILABLE_MONTHLY_YEAR)
      
     INTEGER WVPA_PRIMARY_ACCOUNT, &
             WVPA_DEPARTMENT_UNIT,  &
             WVPA_SUB_ACCOUNT_NUMBER
     
      REAL*4 LTD_BOOK_INTEREST, &
             PS_BOOK_DIVIDENDS, &
             MIPS_BOOK_DIVIDENDS
      
      
end module msgmmdbt_decs
