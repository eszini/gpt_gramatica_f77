C     Last change: MSG 1/9/2012 2:45:38 PM
C     Last change: MSG 1/9/2012 2:45:38 PM

C                                                                      C
C                            R P A R M                                 C
C        Copyright (c) 1982 M.S. Gerber & Associates, Inc.             C
C                      All Rights Reserved                             C
C                                                                      C

C                                                                      C
C     Purpose:  This routine reads the parameter file and           C
C               converts percent values to fractions.                  C
C                                                                      C

C
      SUBROUTINE INIT_READING_CLASS_PARAMETERS(MAX_CLASS_NUM)
C

      use spindriftlib
      use class_canadian
      use class_master
      use class_ctaxes
      use prod_arrays_dimensions
      use sizecom
      use class_parameters
      use class_fin_results
      implicit none
      INTEGER*2 CLASS,YEAR,MAX_CLASS_NUM
      INTEGER*4 IOS
      REAL*4 PARM_TABLE_FED_TAX_RATE
      REAL*4 D100,ADJ01,ROEQU1,TARGET_RATIO
      REAL*4 PASS_THROUGH_VALUES(10)/10*0./,
     +       R_PASS_THROUGH_VALUES(*)
      REAL*4 ATL_EMISSION_CREDIT_EXPENSE,ATL_EMISSION_CREDIT_REVENUE
      REAL*4 STORE_PARM_TABLE_FED_TAX_RATE,REAL_VOID,
     +       RATE_BASE_CASH_ADJ,ADJUSTMENT_2_DEFERRED_TAXES_BAL,
     +       ADJUSTMENT_2_DEFERRED_ITCS_BAL,
     +       NET_NUCLEAR_FUEL_ADJ,
     +       RETAINED_EARNINGS_ADJ,
     +       EXTRA_ORDINARY_EXPENSE,
     +       LT_LIAB_BAL_ADJ,
     +       CWIP_BALANCE_ADJ,
     +       CASH_ADDITIONS_2_FUEL_INVENTORY,
     +       CASH_ADDS_2_MATRIALS_INVENTORY,
     +       CASH_ADDS_2_GAS_INVENTORY,
     +       EXPENSING_FUEL_INVENTORY,
     +       EXPENSING_MATRIALS_INVENTORY,
     +       EXPENSING_GAS_INVENTORY,
     +       STORE_FEDERAL_TAX_RATE,
     +       OPREV_IN_ACCTS_RECEIVABLE,
     +       ADDEN_ACCOUNTS_RECEIVABLE,
     +       EXP_IN_ACCTS_PAYABLE,
     +       ADDEN_ACCOUNTS_PAYABLE,
     +       COI_EARNINGS_ADJ
      PARAMETER(D100=100.)
      INTEGER*2 DELETE,IREC,COVERAGE_RATIO
      LOGICAL*1 IOU_UTILITY,TABLE_NOT_FOUND

      INTEGER*2 PARM_UNIT,I
      PARAMETER(PARM_UNIT=41)
      CHARACTER*512 ZEROS,CLASS_TYPE*1
      INTEGER*2 MAX_PARM_FILE_CLASS_NUMBER,
     +          CLASS_TABLE_POINTER(:),CLASS_TABLE_NUM
      ALLOCATABLE :: CLASS_TABLE_POINTER
      SAVE CLASS_TABLE_POINTER
      REAL*4 QUARTERLY_COMMON_DIVIDENDS(4)
      REAL*4 RETIREMENT_MEDICAL_PAYMENTS,
     +       SALVAGE_TRANSACTIONS,
     +       ADDEDUM_TO_BOOK_GAIN_LOSS,
     +       CLASS_OTH_LIAB_SALE_ASSETS,
     +       NET_DEFERRED_DEBIT_ADJUSTMENT,
     +       DEFERRED_DEBIT_ADJUSTMENT,
     +       CASH_FROM_ASSETS_IN_CLASS
      REAL*4 TEMP_FED_TAX_RATE
      INTEGER*2 PARENT_TABLE_NUMBER,
     +          SUB_DEFAULT_TABLE_NUMBER,
     +          SBU_DEFAULT_TABLE_NUMBER,
     +          REG_DEFAULT_TABLE_NUMBER
      SAVE PARENT_TABLE_NUMBER,
     +     SUB_DEFAULT_TABLE_NUMBER,
     +     SBU_DEFAULT_TABLE_NUMBER,
     +     REG_DEFAULT_TABLE_NUMBER
      REAL*4 ADJ_DEFERRED_TAX_DR_BALANCE,
     +       PAID_IN_CAPITAL,
     +       SUBSIDIARY_INVESTMENT_ADJ,
     +       GOODWILL_ADJUSTMENT,
     +       REG_ASSESTS_ADJUSTMENT,
     +       FASB109_ADJUSTMENT,
     +       FASB133_ADJUSTMENT,
     +       UNAMORT_INTEREST_ADJUSTMENT,
     +       NUCLEAR_DECOM_FUND_BAL_ADJ,
     +       CAP_LEASES_BAL_ADJ,
     +       ASSETS_NEC_BAL_ADJ,
     +       PREFERRED_STOCK_BAL_ADJ,
     +       LTD_BAL_ADJ,
     +       STD_BAL_ADJ,
     +       NUC_DECOM_LIAB_BAL_ADJ,
     +       LIABS_NEC_BAL_ADJ,
     +       OTHER_INVESTMENT_BAL_ADJ,
     +       NOTES_RECEIVABLE_BAL_ADJ,
     +       POST_RETIRE_MEDICAL_BAL_ADJ,
     +       DEFERRED_REVENUES_BAL_ADJ,
     +       DEFERRED_FUEL_BAL_ADJ,
     +       DEFERRED_PURCH_GAS_BAL_ADJ,
     +       LT_INVEST_BAL_ADJ,
     +       ST_INVEST_BAL_ADJ,
     +       FUEL_INVENTORY_BAL_ADJ,
     +       GAS_IN_STORAGE_BAL_ADJ,
     +       MATRIAL_SUPPLY_BAL_ADJ,
     +       ACCOUNTS_RECV_BAL_ADJ,
     +       UNBILLED_REV_BAL_ADJ,
     +       TAXES_RECEIVABLE_BAL_ADJ,
     +       CURRENT_LT_DEBT_BAL_ADJ,
     +       NOTES_PAYABLE_BAL_ADJ,
     +       CUSTOMER_DEPOSIT_BAL_ADJ,
     +       CICA_BAL_ADJ,
     +       POST_RETIRE_PAYABLE_BAL_ADJ,
     +       ACCRUED_PENSION_BAL_ADJ,
     +       DEFERRED_GAINS_BAL_ADJ,
     +       STROM_RESERVE_BAL_ADJ,
     +       ACCOUNTS_PAYABLE_BAL_ADJ,
     +       ARO_NET_ASSETS_BAL_ADJ,
     +       ARO_LIABILITY_BAL_ADJ,
     +       DEFERRED_PURCHASE_POWER_ADJ
C
         CALL RETURN_PARM_FILE_CLASS_NUM(MAX_PARM_FILE_CLASS_NUMBER)
         IF(ALLOCATED(CLASS_TABLE_POINTER)) 
     +                                   DEALLOCATE(CLASS_TABLE_POINTER)
         MAX_PARM_FILE_CLASS_NUMBER = MAX(MAX_PARM_FILE_CLASS_NUMBER,
     +                                    MAX_CLASS_NUM)
         ALLOCATE(CLASS_TABLE_POINTER(0:MAX_PARM_FILE_CLASS_NUMBER))
         CLASS_TABLE_POINTER = -1
         CALL RETURN_PARM_FILE_CLASS_INFO(CLASS_TABLE_POINTER,
     +                                    PARENT_TABLE_NUMBER,
     +                                    SUB_DEFAULT_TABLE_NUMBER,
     +                                    SBU_DEFAULT_TABLE_NUMBER,
     +                                    REG_DEFAULT_TABLE_NUMBER)
C
C OPEN PARAMETER FILE
C
         CALL OPEN_FINANICAL_PARAMETER_FILE(PARM_UNIT)
      RETURN

      ENTRY READ_CLASS_PARAMETERS(CLASS,YEAR,CLASS_TYPE,
     +                            COVERAGE_RATIO,
     +                            RETIREMENT_MEDICAL_PAYMENTS,
     +                            SALVAGE_TRANSACTIONS,
     +                            CLASS_OTH_LIAB_SALE_ASSETS,
     +                            NET_DEFERRED_DEBIT_ADJUSTMENT,
     +                            RATE_BASE_CASH_ADJ,
     +                            ADJUSTMENT_2_DEFERRED_TAXES_BAL,
     +                            ADJUSTMENT_2_DEFERRED_ITCS_BAL,
     +                            NET_NUCLEAR_FUEL_ADJ,
     +                            RETAINED_EARNINGS_ADJ,
     +                            EXTRA_ORDINARY_EXPENSE,
     +                            LT_LIAB_BAL_ADJ,
     +                            CWIP_BALANCE_ADJ,
     +                            CASH_ADDITIONS_2_FUEL_INVENTORY,
     +                            QUARTERLY_COMMON_DIVIDENDS,
     +                            ADJ_DEFERRED_TAX_DR_BALANCE,
     +                            PAID_IN_CAPITAL,
     +                            SUBSIDIARY_INVESTMENT_ADJ,
     +                            GOODWILL_ADJUSTMENT,
     +                            REG_ASSESTS_ADJUSTMENT,
     +                            FASB109_ADJUSTMENT,
     +                            FASB133_ADJUSTMENT,
     +                            UNAMORT_INTEREST_ADJUSTMENT,
     +                                NUCLEAR_DECOM_FUND_BAL_ADJ,
     +                                CAP_LEASES_BAL_ADJ,
     +                                ASSETS_NEC_BAL_ADJ,
     +                                PREFERRED_STOCK_BAL_ADJ,
     +                                LTD_BAL_ADJ,
     +                                STD_BAL_ADJ,
     +                                NUC_DECOM_LIAB_BAL_ADJ,
     +                                LIABS_NEC_BAL_ADJ,
     +                                OTHER_INVESTMENT_BAL_ADJ,
     +                                NOTES_RECEIVABLE_BAL_ADJ,
     +                                POST_RETIRE_MEDICAL_BAL_ADJ,
     +                                DEFERRED_REVENUES_BAL_ADJ,
     +                                DEFERRED_FUEL_BAL_ADJ,
     +                                DEFERRED_PURCH_GAS_BAL_ADJ,
     +                                LT_INVEST_BAL_ADJ,
     +                                ST_INVEST_BAL_ADJ,
     +                                FUEL_INVENTORY_BAL_ADJ,
     +                                GAS_IN_STORAGE_BAL_ADJ,
     +                                MATRIAL_SUPPLY_BAL_ADJ,
     +                                ACCOUNTS_RECV_BAL_ADJ,
     +                                UNBILLED_REV_BAL_ADJ,
     +                                TAXES_RECEIVABLE_BAL_ADJ,
     +                                CURRENT_LT_DEBT_BAL_ADJ,
     +                                NOTES_PAYABLE_BAL_ADJ,
     +                                CUSTOMER_DEPOSIT_BAL_ADJ,
     +                                CICA_BAL_ADJ,
     +                                POST_RETIRE_PAYABLE_BAL_ADJ,
     +                                ACCRUED_PENSION_BAL_ADJ,
     +                                DEFERRED_GAINS_BAL_ADJ,
     +                                STROM_RESERVE_BAL_ADJ,
     +                                ACCOUNTS_PAYABLE_BAL_ADJ,
     +                                ARO_NET_ASSETS_BAL_ADJ,
     +                                ARO_LIABILITY_BAL_ADJ,
     +                                OPREV_IN_ACCTS_RECEIVABLE,
     +                                ADDEN_ACCOUNTS_RECEIVABLE,
     +                                EXP_IN_ACCTS_PAYABLE,
     +                                ADDEN_ACCOUNTS_PAYABLE,
     +                                DEFERRED_PURCHASE_POWER_ADJ,
     +                                CASH_ADDS_2_MATRIALS_INVENTORY,
     +                                CASH_ADDS_2_GAS_INVENTORY,
     +                                EXPENSING_FUEL_INVENTORY,
     +                                EXPENSING_MATRIALS_INVENTORY,
     +                                EXPENSING_GAS_INVENTORY,
     +                                COI_EARNINGS_ADJ)

C
      IOU_UTILITY = .TRUE.
C
C
C     READ PARAMETER FILE
C
      ns_class_parameters%RB_INCLUDED_DEF_TAXES_DR_RATE = 0.
      ns_class_parameters%
     + RB_EXCLUDED_PENSION_LIABILITY_RATE = 0.! 156
      ns_class_parameters%
     + RB_EXCLUDED_Deferred_Gain_RATE = 0. ! 157
      ns_class_parameters%
     + RB_EXCLUDED_Storm_Reserve_RATE = 0. ! 158
      ns_class_parameters%RB_EXCLUDED_Accrued_Vacation_Pay_RATE = 0.!159
      ns_class_parameters%RB_INCLUDED_DEFERRED_REVENUES_RATE = 0.  ! 160
      ns_class_fin_results%x_price_1 = 0.
      ns_class_fin_results%X_PRICE_2 = 0.
      ns_class_fin_results%X_PRICE_3 = 0.

      CLASS_TABLE_NUM = CLASS_TABLE_POINTER(CLASS)
      IF(CLASS_TABLE_NUM < 1) THEN
         IF(CLASS_TYPE == 'P') CLASS_TABLE_NUM = PARENT_TABLE_NUMBER
         IF(CLASS_TYPE == 'S') CLASS_TABLE_NUM =
     +                                          SUB_DEFAULT_TABLE_NUMBER
         IF(CLASS_TYPE == 'B') CLASS_TABLE_NUM =
     +                                          SBU_DEFAULT_TABLE_NUMBER
         IF(CLASS_TYPE == 'R') CLASS_TABLE_NUM =
     +                                          REG_DEFAULT_TABLE_NUMBER
         TABLE_NOT_FOUND = CLASS_TABLE_NUM < 1
      ELSE
         TABLE_NOT_FOUND = .FALSE.
      ENDIF
      CLASS_TABLE_NUM = MAX(CLASS_TABLE_NUM,1)
      IREC = (CLASS_TABLE_NUM - 1) * AVAIL_DATA_YEARS + MIN(YEAR,30)
C     IREC = YEAR
      READ(PARM_UNIT,REC=IREC,IOSTAT=IOS) DELETE,DELETE,ROEQU1,
     +   ns_class_parameters%RETURN_ON_NPV,
     +   ns_class_parameters%COMMON_PAYOUT_RATIO,
     + ns_class_parameters%DIVIDEND_PER_SHARE,
     + ns_class_parameters%PSRATE,
     +   ns_class_parameters%STD_INTEREST_RATE,
     +   ns_class_parameters%LTDRTE,
     +   REAL_VOID, ! ns_class_parameters%CLASS_SALES,
     +   REAL_VOID, ! ns_class_parameters%CLASS_BASE_REVENUE,  ! 10
     +   REAL_VOID, ! ns_class_parameters%CLASS_ADJ_CLAUSE_REVENUE, ! 11
     +   REAL_VOID, ! ns_class_parameters%CLASS_OTHER_REVENUE, ! 12
     +   ns_class_master%ADJ_RATE_REVENUE,   ! 13
     +   ns_class_parameters%MAXIMUM_RATE_CHANGE,
     +   ns_class_parameters%MIN_ST_INVESTMENTS, ! 15
     +   ns_class_parameters%RBITC,     ! 16
     +   ns_class_parameters%RBDFTX,    ! 17
     +   ns_class_parameters%RBANEC,    ! 18
     +   REAL_VOID, 
     +   REAL_VOID, ! ADJ01,  20
     +   ns_class_parameters%EQRATO,ns_class_parameters%PSRATO,
     +   ns_class_parameters%MARKET_2_BOOK,ns_class_parameters%CSUNIT,
     + ns_class_parameters%CSMIN,
     +   ns_class_parameters%CSMAX,ns_class_parameters%PSUNIT,
     + ns_class_parameters%PSMIN,ns_class_parameters%PSMAX,
     + ns_class_parameters%PSSINK, ! 30
     +   ns_class_parameters%STDMIN,
     + ns_class_parameters%STD_MAX,
     + ns_class_parameters%LDUNIT,ns_class_parameters%LDLIFE,
     +   ns_class_parameters%LTDMIN,ns_class_parameters%LTDMAX,
     + ns_class_parameters%LDSINK,TARGET_RATIO,
     + ns_class_parameters%TARGET_EQUITY_RATIO,
     +   ns_class_parameters%OPREV_IN_ASSETS_NEC, ! 40
     +   ns_class_parameters%NPV_IN_ASSETS_NEC,
     +   REAL_VOID, ! ns_class_parameters%ADDENUM_2_ASSETS_NEC, 42
     +   ns_class_parameters%OPREV_IN_LIBS_NEC,
     +   ns_class_parameters%NPV_IN_LIBS_NEC,
     +   REAL_VOID, ! ns_class_parameters%ADDENUM_2_LIBS_NEC, 45
     +   REAL_VOID, ! ns_class_parameters%SUBSIDIARY_DIVIDEND,
     +   REAL_VOID, ! ns_class_parameters%SUBSIDIARY_INCOME,
     +   REAL_VOID, ! ns_class_parameters%NEW_SUBSIDIARY_INVESTMENT,
     +   REAL_VOID, ! ns_class_parameters%CLASS_BTL_REVENUES,
     +   REAL_VOID, ! PARM_TABLE_FED_TAX_RATE  50
     +   REAL_VOID, ! ns_class_parameters%ITC_AMORTIZATION_RATE,
     +   REAL_VOID, ! ns_class_ctaxes%FED_INCOME_TAX_DEDUCTIONS,
     +   REAL_VOID, ! ns_class_parameters%STATE_TAX_RATE,
     +   REAL_VOID, ! ns_class_ctaxes%STATE_INCOME_TAX_DEDUCTIONS,
     +   REAL_VOID, ! ns_class_parameters%OPREV_TAX_RATE,
     +   REAL_VOID, ! ns_class_parameters%PROPERTY_TAX_RATE,
     +   REAL_VOID, ! ns_class_parameters%CLASS_OTHER_TAXES_RATE,
     +   REAL_VOID, ! ns_class_parameters%CLASS_ADDENDUM_2_OTHER_TAXES,
     +   REAL_VOID, ! ns_class_parameters%DEFERRED_TAX_ADJUSTMENT,
     +   REAL_VOID, ! PARM_AMORTIZATION_ADJUSTMENT, 60
     +   REAL_VOID, ! ns_class_parameters%DEFERRED_REVENUES,
     +   REAL_VOID, ! ns_class_parameters%AMORT_DEF_REVENUES,

     + ns_class_parameters%RETURN_ON_ST_INVEST,PASS_THROUGH_VALUES(1:3),
     +   REAL_VOID, ! SALVAGE_TRANSACTIONS,
     +   REAL_VOID, ! AMT_PREP_DEPRE_ADDEN,
     +   REAL_VOID, ! ADJTXNORM,  70 note 64 is the COMMENT
     +   REAL_VOID, ! CASH_OTH_ASSET,
     +   REAL_VOID, ! NUCL_DECOM_FUND,
     +   ns_class_parameters%NUCL_DECOM_FUND_RETURN,
     +   ns_class_parameters%NUC_DEOM_FUND_RATEBASE,
C ADDED 4/13/92 FOR WPK WORK 
     +   ns_class_parameters%OM_CAPITALIZATION_RATE, ! 75
C ADDED 9/29/92 FOR SRP
     +   REAL_VOID, ! CLASS_ADDENDUM_TO_PROPERTY_TAX,
     +   REAL_VOID, ! ADDENDUM_TO_ITC_AMORTIZATED,
C ADDED 10/28/92 FOR KEPCO
     +   REAL_VOID, ! ns_class_parameters%DEFERRED_EXPENSE_AMORT_PERIOD,
     +   REAL_VOID, ! DEFERRED_EXPENSE_CASH,
     +   ns_class_parameters%DEFERRED_EXPENSE_AMORT,  ! 80
C ADDED 3/26/93 FOR UPA AND OTHERS
     +   ns_class_parameters%MAX_EQUITY_IN_CAP_STRUCTURE,
     +   REAL_VOID, ! CLASS_BTL_EXPENSES,
     +   REAL_VOID, ! CASH_FROM_SALE_OF_ASSETS,
     +   REAL_VOID, ! GROSS_VALUE_OF_SOLD_ASSETS,
     +   REAL_VOID, ! CUMULATIVE_DEP_ON_SOLD_ASSETS,
     +   REAL_VOID, ! ADJUSTMENT_TO_DEFERRED_TAXES,
     +   REAL_VOID, ! ATL_AMORT_OF_BOOK,
     +   REAL_VOID, ! BTL_AMORT_OF_BOOK,
     +   ns_class_parameters%LEASE_PAYMENTS_IN_TIER,
     +   REAL_VOID, ! CAPITIALIZED_LEASE_ADDITIONS, 90
     +   REAL_VOID, ! ATL_LEASE_PAYMENTS,
     +   REAL_VOID, ! BTL_LEASE_PAYMENTS,
C ADDED 6/18/93 FOR WKP 
     +   REAL_VOID, ! ns_class_canadian%PROVINCIAL_CAPITAL_TAX_RATE,
     +   REAL_VOID, ! ns_class_canadian%FEDERAL_CAPITAL_TAX_RATE,
     +   REAL_VOID, ! ns_class_canadian%PROVINCIAL_CAP_TAX_DEDUCTION,
     +   REAL_VOID, ! ns_class_canadian%FEDERAL_CAPITAL_TAX_DEDUCTION,
C ADDED 5/5/93 FOR FP&L TC WORK     ITEMS 93-100
     +   REAL_VOID, ! CUSTOMER_DEPOSITS_PERCENT_REVS,
     +   REAL_VOID, ! ADDENDUM_TO_CUSTOMER_DEPOSITS,
     +   ns_class_parameters%INTEREST_ON_CUSTOMER_DEPOSITS,
     +   ns_class_parameters%STD_IN_WEIGHTED_COST_PERCENT,  ! 100
     +   ns_class_parameters%CUSTOMER_DEPOSITS_IN_WC_PERCENT,
     +   ns_class_parameters%DEF_ITC_IN_WC_PERCENT,
     +   ns_class_parameters%DEF_TAX_CR_IN_WC_PERCENT,
     +   ns_class_parameters%MAX_COMMON_BUY_BACK,
C ADDED 7/15/93 FOR WKP ITEMS 105-108
     +   REAL_VOID, ! CIAC_CASH,
     +   REAL_VOID, ! CIAC_AMORTIZATION_RATE,
     +   REAL_VOID, ! CIAC_AMORTIZATION_ADDENDUM,
     +   ns_class_parameters%CIAC_EXCLUDED_FROM_RATEBASE,
C ADDED 7 PASS THROUGHT VARIABLES FOR TVA 10/93
     +   PASS_THROUGH_VALUES(4:10),   ! 109-115
C ITEMS 116-119 9/6/94 FOR OHIO EDISON
     +   ns_class_parameters%BTL_EMISSION_CREDIT_EXPENSE,
     +   ns_class_parameters%BTL_EMISSION_CREDIT_REVENUE,
     +   ns_class_parameters%ADJ_CLAUSE_REVENUE_ALLOCATION,
     +   ns_class_parameters%ADJ_CLAUSE_EXPENSE_ALLOCATION,  ! 119
C ITEMS 120-122 3/9/95 ADDED FOR SHAREHOLDER VALUE
     +   REAL_VOID, ! COST_OF_CAPITAL_RISK_ADJUSTMENT, 120
     +   REAL_VOID, ! ADJUSTMENT_2_ECONOMIC_ASSETS,
     +   REAL_VOID, ! ADJUSTMENT_TO_OPERATING_PROFITS
     +   ns_class_parameters%LTD_SINKING_FUND_LAG,
     +   ns_class_parameters%PS_SINKING_FUND_LAG,
     + ! 127 skip from class name and number
     +   ns_class_parameters%CAP_LEASES_IN_RATEBASE, 
     +   REAL_VOID, 
     +   REAL_VOID, ! ACE_TAX_DEPRECIATION,
     +   ns_class_parameters%CIAC_BALANCE_IN_WC_PERCENT, ! 130
     +   REAL_VOID, ! CAPITAL_RECOVERY_NOT_IN_OPAT,
     +   REAL_VOID, ! BTL_MISC_DEDUCTIONS,
C ITEMS 133-140 10/16/96 ADDED FOR SRP AND GENERAL
     +   ns_class_parameters%MAX_SHORT_TERM_INVESTMENT,
     +   ns_class_parameters%MIN_LONG_TERM_INVESTMENT,
     +   ns_class_parameters%MAX_LONG_TERM_INVESTMENT,  ! 135
     +   REAL_VOID, ! CHANGE_IN_LONG_TERM_INVESTMENTS,
     +   ns_class_parameters%RETURN_ON_LONG_TERM_INVESTMENTS,
     +   REAL_VOID, ! RETIREMENT_MEDICAL_PAYMENTS,
     +   REAL_VOID, ! ns_class_parameters%RETIREE_MEDICAL_PAYMENTS,
C ITEMS 140-149
     +   ns_class_parameters%RETURN_RETIREMENT_MEDICAL_FUND,
     +   ns_class_parameters%MINIMUM_COMMON_STOCK_BALANCE,
     +   ns_class_parameters%DEPOSITS_EXCLUDED_FROM_RATEBASE,
     +   REAL_VOID, ! CHANGE_2_DECOMMISSIONING_LIAB, 143
     +   REAL_VOID, ! ns_class_parameters%ADJUSTMENT_2_OP_REV_TAX,
     +   REAL_VOID, ! OTHER_TAXES_PERCENT_OF_EXPENSES,
     +   ns_class_parameters%ADDENDUM_2_SHARES_ISSUED,
     +   REAL_VOID, ! STATE_DEDUC_PERCT_OF_TIB4DEDUC,
     +   REAL_VOID, ! FED_DEDUC_PERCT_OF_TIB4DEDUC,
     +   ns_class_parameters%REA_MARGIN_ADDENDUM,
     +   QUARTERLY_COMMON_DIVIDENDS, ! 4 VALUES 151-154

     +   ns_class_parameters%RB_INCLUDED_DEF_TAXES_DR_RATE, ! 155
     +   ns_class_parameters%RB_EXCLUDED_PENSION_LIABILITY_RATE, ! 156
     +   ns_class_parameters%RB_EXCLUDED_Deferred_Gain_RATE, ! 157
     +   ns_class_parameters%RB_EXCLUDED_Storm_Reserve_RATE, ! 158
     +   ns_class_parameters%RB_EXCLUDED_Accrued_Vacation_Pay_RATE, !159
     +   ns_class_parameters%RB_INCLUDED_DEFERRED_REVENUES_RATE,  ! 160
     +   ns_class_parameters%ODEC_MaxEquityRatio,
     +   ns_class_parameters%ODEC_MinPayoutRatio, ! 162
     +   ns_class_parameters%ODEC_MaxPayoutRatio, ! 163
     +   ns_class_parameters%ODEC_PercentREReduction, ! 164
     +   ns_class_parameters%ODEC_DollarREReduction  ! 165
C
C ATL AND BTL ALLOCATION OF EMISSIONS CREDIT EXPENSE AND GAINS
C
      ns_class_fin_results%x_price_1 = PASS_THROUGH_VALUES(1)
      ns_class_fin_results%X_PRICE_2 = PASS_THROUGH_VALUES(2)
      ns_class_fin_results%X_PRICE_3 = PASS_THROUGH_VALUES(3)
      ns_class_parameters%CLASS_SALES = 0.
      ns_class_parameters%CLASS_BASE_REVENUE = 0.
      ns_class_parameters%CLASS_ADJ_CLAUSE_REVENUE= 0.
      ns_class_parameters%CLASS_OTHER_REVENUE = 0.
    
    
    
    
      ns_class_parameters%ADDENDUM_2_RATE_BASE = 0.
      ADJ01 = 0.
      ns_class_parameters%ADDENUM_2_ASSETS_NEC = 0.
      ns_class_parameters%ADDENUM_2_LIBS_NEC = 0.
      ns_class_parameters%SUBSIDIARY_DIVIDEND = 0.
      ns_class_parameters%SUBSIDIARY_INCOME = 0.
      ns_class_parameters%NEW_SUBSIDIARY_INVESTMENT = 0.
      ns_class_parameters%CLASS_BTL_REVENUES = 0.
      PARM_TABLE_FED_TAX_RATE = 0.
      ns_class_parameters%ITC_AMORTIZATION_RATE = 0.
      ns_class_ctaxes%FED_INCOME_TAX_DEDUCTIONS = 0.
      ns_class_parameters%STATE_TAX_RATE = 0.
      ns_class_ctaxes%STATE_INCOME_TAX_DEDUCTIONS = 0.
      ns_class_parameters%OPREV_TAX_RATE = 0.
      ns_class_parameters%PROPERTY_TAX_RATE = 0.
      ns_class_parameters%CLASS_OTHER_TAXES_RATE = 0.
      ns_class_parameters%CLASS_ADDENDUM_2_OTHER_TAXES = 0.
      ns_class_parameters%DEFERRED_TAX_ADJUSTMENT = 0.
      ns_class_parameters%PARM_AMORTIZATION_ADJUSTMENT = 0.
      ns_class_parameters%DEFERRED_REVENUES = 0.
      ns_class_parameters%AMORT_DEF_REVENUES = 0.
      SALVAGE_TRANSACTIONS = 0.
      ns_class_parameters%AMT_PREP_DEPRE_ADDEN = 0.
      ns_class_ctaxes%ADJTXNORM = 0.
      ns_class_parameters%CASH_OTH_ASSET = 0.
      ns_class_parameters%NUCL_DECOM_FUND = 0.
      ns_class_parameters%CLASS_ADDENDUM_TO_PROPERTY_TAX = 0.
      ns_class_parameters%ADDENDUM_TO_ITC_AMORTIZATED = 0.
      ns_class_parameters%DEFERRED_EXPENSE_AMORT_PERIOD = 0.
      ns_class_parameters%DEFERRED_EXPENSE_CASH = 0.
      ns_class_parameters%CLASS_BTL_EXPENSES = 0.
      ns_class_parameters%CASH_FROM_SALE_OF_ASSETS = 0.
      ns_class_parameters%GROSS_VALUE_OF_SOLD_ASSETS = 0.
      ns_class_parameters%CUMULATIVE_DEP_ON_SOLD_ASSETS = 0.
      ns_class_parameters%ADJUSTMENT_TO_DEFERRED_TAXES = 0.
      ns_class_parameters%ATL_AMORT_OF_BOOK = 0.
      ns_class_parameters%BTL_AMORT_OF_BOOK = 0.
      ns_class_parameters%CAPITIALIZED_LEASE_ADDITIONS = 0.
      ns_class_parameters%ATL_LEASE_PAYMENTS = 0.
      ns_class_parameters%BTL_LEASE_PAYMENTS = 0.
      ns_class_canadian%PROVINCIAL_CAPITAL_TAX_RATE = 0.
      ns_class_canadian%FEDERAL_CAPITAL_TAX_RATE = 0.
      ns_class_canadian%PROVINCIAL_CAP_TAX_DEDUCTION = 0.
      ns_class_canadian%FEDERAL_CAPITAL_TAX_DEDUCTION = 0.
      ns_class_parameters%CUSTOMER_DEPOSITS_PERCENT_REVS = 0.
      ns_class_parameters%ADDENDUM_TO_CUSTOMER_DEPOSITS = 0.
      ns_class_parameters%CIAC_CASH = 0.
      ns_class_parameters%CIAC_AMORTIZATION_RATE = 0.
      ns_class_parameters%CIAC_AMORTIZATION_ADDENDUM = 0.
      ns_class_parameters%COST_OF_CAPITAL_RISK_ADJUSTMENT = 0.
      ns_class_parameters%ADJUSTMENT_2_ECONOMIC_ASSETS = 0.
      ns_class_parameters%ADJUSTMENT_TO_OPERATING_PROFITS = 0.
      ns_class_parameters%INCOME_TAX_DEPRECIATION = 0.
      ns_class_parameters%ACE_TAX_DEPRECIATION = 0.
      ns_class_parameters%CAPITAL_RECOVERY_NOT_IN_OPAT = 0.
      ns_class_parameters%BTL_MISC_DEDUCTIONS = 0.
      ns_class_parameters%CHANGE_IN_LONG_TERM_INVESTMENTS = 0.
      RETIREMENT_MEDICAL_PAYMENTS = 0.
      ns_class_parameters%RETIREE_MEDICAL_PAYMENTS = 0.
      ns_class_parameters%CHANGE_2_DECOMMISSIONING_LIAB = 0.
      ns_class_parameters%ADJUSTMENT_2_OP_REV_TAX 
     + = 0.
      ns_class_parameters%OTHER_TAXES_PERCENT_OF_EXPENSES = 0.
      ns_class_parameters%STATE_DEDUC_PERCT_OF_TIB4DEDUC = 0.
      ns_class_parameters%FED_DEDUC_PERCT_OF_TIB4DEDUC = 0.
    
    
      IF(ns_class_parameters%MIN_ST_INVESTMENTS > 
     + ns_class_parameters%MAX_SHORT_TERM_INVESTMENT)
     +ns_class_parameters%MIN_ST_INVESTMENTS = 
     + ns_class_parameters%MAX_SHORT_TERM_INVESTMENT
      IF(CLASS_TABLE_POINTER(CLASS) == -1 .AND. CLASS > 1 .AND.
     +                                             TABLE_NOT_FOUND) THEN
      ns_class_parameters%ADJUSTMENT_2_OP_REV_TAX
     +  = 0.
         ns_class_parameters%CLASS_SALES = 0.
         ns_class_parameters%CLASS_BASE_REVENUE = 0.
         ns_class_parameters%CLASS_ADJ_CLAUSE_REVENUE = 0.
         ns_class_parameters%CLASS_OTHER_REVENUE = 0.
         ns_class_parameters%CSMIN = 0.
         ns_class_parameters%PSMIN = 0.
         ns_class_parameters%STDMIN = 0.
         ns_class_parameters%LTDMIN = 0.
         ns_class_parameters%MIN_ST_INVESTMENTS = 0.
         ns_class_master%ADJ_RATE_REVENUE = 0.
         ns_class_parameters%ADDENDUM_2_RATE_BASE = 0.
         ADJ01 = 0.
         ns_class_parameters%ADDENUM_2_ASSETS_NEC = 0.
         ns_class_parameters%ADDENUM_2_LIBS_NEC = 0.
         ns_class_parameters%SUBSIDIARY_DIVIDEND = 0.
         ns_class_parameters%SUBSIDIARY_INCOME = 0.
         ns_class_parameters%NEW_SUBSIDIARY_INVESTMENT = 0.
         ns_class_parameters%CLASS_BTL_REVENUES = 0.
         ns_class_ctaxes%FED_INCOME_TAX_DEDUCTIONS = 0.
         ns_class_ctaxes%STATE_INCOME_TAX_DEDUCTIONS = 0.
         ns_class_parameters%CLASS_ADDENDUM_2_OTHER_TAXES = 0.
         ns_class_parameters%DEFERRED_TAX_ADJUSTMENT = 0.
         ns_class_parameters%PARM_AMORTIZATION_ADJUSTMENT = 0.
         ns_class_parameters%DEFERRED_REVENUES = 0.
         ns_class_parameters%AMORT_DEF_REVENUES = 0.
         SALVAGE_TRANSACTIONS = 0.
         ns_class_parameters%AMT_PREP_DEPRE_ADDEN = 0.
         ns_class_ctaxes%ADJTXNORM = 0.
         ns_class_parameters%CASH_OTH_ASSET = 0.
         ns_class_parameters%NUCL_DECOM_FUND = 0.
         ns_class_parameters%CLASS_ADDENDUM_TO_PROPERTY_TAX = 0.
         ns_class_parameters%ADDENDUM_TO_ITC_AMORTIZATED = 0.
         ns_class_parameters%DEFERRED_EXPENSE_CASH = 0.
         ns_class_parameters%DEFERRED_EXPENSE_AMORT = 0.
         ns_class_parameters%CLASS_BTL_EXPENSES = 0.
         ns_class_parameters%CASH_FROM_SALE_OF_ASSETS = 0.
         ns_class_parameters%GROSS_VALUE_OF_SOLD_ASSETS = 0.
         ns_class_parameters%CUMULATIVE_DEP_ON_SOLD_ASSETS = 0.
         ns_class_parameters%ADJUSTMENT_TO_DEFERRED_TAXES = 0.
         ns_class_parameters%ATL_AMORT_OF_BOOK = 0.
         ns_class_parameters%BTL_AMORT_OF_BOOK = 0.
         ns_class_parameters%CAPITIALIZED_LEASE_ADDITIONS = 0.
         ns_class_parameters%ATL_LEASE_PAYMENTS = 0.
         ns_class_parameters%BTL_LEASE_PAYMENTS = 0.
        ns_class_parameters%ADDENDUM_TO_CUSTOMER_DEPOSITS = 0.
         ns_class_parameters%CIAC_CASH = 0.
         ns_class_parameters%CIAC_AMORTIZATION_ADDENDUM = 0.
         ns_class_parameters%BTL_EMISSION_CREDIT_EXPENSE = 0.
         ns_class_parameters%BTL_EMISSION_CREDIT_REVENUE = 0.
         ns_class_parameters%COST_OF_CAPITAL_RISK_ADJUSTMENT = 0.
         ns_class_parameters%ADJUSTMENT_2_ECONOMIC_ASSETS = 0.
         ns_class_parameters%ADJUSTMENT_TO_OPERATING_PROFITS = 0.
         ns_class_parameters%INCOME_TAX_DEPRECIATION = 0.
         ns_class_parameters%ACE_TAX_DEPRECIATION = 0.
      ENDIF

C SECTION FOR READING ADDENDUMS FORM THE ADDENDUMS FILE
C
         ! not allowed in parameter file
         ns_class_parameters%DEFERRED_EXPENSE_CASH = 0. 
         CALL RETURN_PAYMENT_ADDENDUMS(YEAR,CLASS,
     +                              ns_class_parameters%NUCL_DECOM_FUND,
     +                ns_class_parameters%CHANGE_2_DECOMMISSIONING_LIAB,
     +                  RETIREMENT_MEDICAL_PAYMENTS,
     +                     ns_class_parameters%RETIREE_MEDICAL_PAYMENTS)
         CALL RETURN_INVESTMENT_ADDENDUMS(YEAR,CLASS,
     +                        ns_class_parameters%SUBSIDIARY_DIVIDEND,
     +                            ns_class_parameters%SUBSIDIARY_INCOME,
     +                   ns_class_parameters%NEW_SUBSIDIARY_INVESTMENT,
     +                               ns_class_parameters%CASH_OTH_ASSET,
     +                 ns_class_parameters%CAPITIALIZED_LEASE_ADDITIONS,
     +                                  CASH_FROM_ASSETS_IN_CLASS,
     +              ns_class_parameters%CHANGE_IN_LONG_TERM_INVESTMENTS)
         CALL RETURN_RATE_BASE_ADDENDUMS(YEAR,CLASS,
     +                                   ns_class_parameters%RBANEC,
     +                        ns_class_parameters%ADDENDUM_2_RATE_BASE)
         CALL RETURN_CD_CIAC_ADDENDUMS(YEAR,CLASS,
     +               ns_class_parameters%CUSTOMER_DEPOSITS_PERCENT_REVS,
     +   ns_class_parameters%ADDENDUM_TO_CUSTOMER_DEPOSITS,
     +                                 ns_class_parameters%CIAC_CASH,
     +                      ns_class_parameters%CIAC_AMORTIZATION_RATE,
     +                  ns_class_parameters%CIAC_AMORTIZATION_ADDENDUM)
         CALL RETURN_WORKING_CAP_ADDENDUMS(YEAR,CLASS,
     +                         ns_class_parameters%ADDENUM_2_ASSETS_NEC,
     +                         ns_class_parameters%ADDENUM_2_LIBS_NEC,
     +                                  OPREV_IN_ACCTS_RECEIVABLE,
     +                                  ADDEN_ACCOUNTS_RECEIVABLE,
     +                                  EXP_IN_ACCTS_PAYABLE,
     +                                  ADDEN_ACCOUNTS_PAYABLE,
     +                                  CASH_ADDITIONS_2_FUEL_INVENTORY,
     +                                  CASH_ADDS_2_MATRIALS_INVENTORY,
     +                                  CASH_ADDS_2_GAS_INVENTORY,
     +                                  EXPENSING_FUEL_INVENTORY,
     +                                  EXPENSING_MATRIALS_INVENTORY,
     +                                  EXPENSING_GAS_INVENTORY)
         CALL RETURN_SHAREHOLDER_ADDENDUMS(YEAR,CLASS,
     +              ns_class_parameters%COST_OF_CAPITAL_RISK_ADJUSTMENT,
     +                 ns_class_parameters%ADJUSTMENT_2_ECONOMIC_ASSETS,
     +              ns_class_parameters%ADJUSTMENT_TO_OPERATING_PROFITS,
     +                 ns_class_parameters%CAPITAL_RECOVERY_NOT_IN_OPAT)
     
         ! NEED TO FLIP THIS VALUE
         SALVAGE_TRANSACTIONS = -SALVAGE_TRANSACTIONS 
         CALL RETURN_SALE_REMOVAL_ADDENDUMS(YEAR,CLASS,
     +                                    SALVAGE_TRANSACTIONS,
     +                     ns_class_parameters%CASH_FROM_SALE_OF_ASSETS,
     +                   ns_class_parameters%GROSS_VALUE_OF_SOLD_ASSETS,
     +                ns_class_parameters%CUMULATIVE_DEP_ON_SOLD_ASSETS,
     +       ns_class_parameters%ADJUSTMENT_TO_DEFERRED_TAXES,
     +                           ns_class_parameters%ATL_AMORT_OF_BOOK,
     +                           ns_class_parameters%BTL_AMORT_OF_BOOK,
     +                                    ADDEDUM_TO_BOOK_GAIN_LOSS)
         CLASS_OTH_LIAB_SALE_ASSETS = 
     + ns_class_parameters%CASH_FROM_SALE_OF_ASSETS
     +                  - ns_class_parameters%GROSS_VALUE_OF_SOLD_ASSETS
     +               + ns_class_parameters%CUMULATIVE_DEP_ON_SOLD_ASSETS
     +                          - ns_class_parameters%ATL_AMORT_OF_BOOK
     +                           - ns_class_parameters%BTL_AMORT_OF_BOOK
     +                                + ADDEDUM_TO_BOOK_GAIN_LOSS
         ns_class_parameters%CASH_FROM_SALE_OF_ASSETS = 
     + ns_class_parameters%CASH_FROM_SALE_OF_ASSETS +
     +                                         CASH_FROM_ASSETS_IN_CLASS
C
         CALL RETURN_TRANSFER_ADDENDUMS(YEAR,CLASS,
     +                   ns_class_parameters%GROSS_VALUE_OF_SOLD_ASSETS,
     +                ns_class_parameters%CUMULATIVE_DEP_ON_SOLD_ASSETS,
     +                                ADJUSTMENT_2_DEFERRED_TAXES_BAL,
     +                                DEFERRED_DEBIT_ADJUSTMENT,
     +                                NET_DEFERRED_DEBIT_ADJUSTMENT,
     +                            ns_class_parameters%ATL_AMORT_OF_BOOK,
     +                            ns_class_parameters%BTL_AMORT_OF_BOOK,
     +                     ns_class_parameters%DEFERRED_TAX_ADJUSTMENT,
     +                                ADJUSTMENT_2_DEFERRED_ITCS_BAL,
     +                                NET_NUCLEAR_FUEL_ADJ,
     +                                RETAINED_EARNINGS_ADJ,
     +                                EXTRA_ORDINARY_EXPENSE,
     +                                LT_LIAB_BAL_ADJ,
     +                                CWIP_BALANCE_ADJ,
     +                                ADJ_DEFERRED_TAX_DR_BALANCE,
     +                                PAID_IN_CAPITAL,
     +                                SUBSIDIARY_INVESTMENT_ADJ,
     +                                GOODWILL_ADJUSTMENT,
     +                                REG_ASSESTS_ADJUSTMENT,
     +                                FASB109_ADJUSTMENT,
     +                                FASB133_ADJUSTMENT,
     +                                UNAMORT_INTEREST_ADJUSTMENT,
     +                                NUCLEAR_DECOM_FUND_BAL_ADJ,
     +                                CAP_LEASES_BAL_ADJ,
     +                                ASSETS_NEC_BAL_ADJ,
     +                                PREFERRED_STOCK_BAL_ADJ,
     +                                LTD_BAL_ADJ,
     +                                STD_BAL_ADJ,
     +                                NUC_DECOM_LIAB_BAL_ADJ,
     +                                LIABS_NEC_BAL_ADJ,
     +                                OTHER_INVESTMENT_BAL_ADJ,
     +                                NOTES_RECEIVABLE_BAL_ADJ,
     +                                POST_RETIRE_MEDICAL_BAL_ADJ,
     +                                DEFERRED_REVENUES_BAL_ADJ,
     +                                DEFERRED_FUEL_BAL_ADJ,
     +                                DEFERRED_PURCH_GAS_BAL_ADJ,
     +                                LT_INVEST_BAL_ADJ,
     +                                ST_INVEST_BAL_ADJ,
     +                                FUEL_INVENTORY_BAL_ADJ,
     +                                GAS_IN_STORAGE_BAL_ADJ,
     +                                MATRIAL_SUPPLY_BAL_ADJ,
     +                                ACCOUNTS_RECV_BAL_ADJ,
     +                                UNBILLED_REV_BAL_ADJ,
     +                                TAXES_RECEIVABLE_BAL_ADJ,
     +                                CURRENT_LT_DEBT_BAL_ADJ,
     +                                NOTES_PAYABLE_BAL_ADJ,
     +                                CUSTOMER_DEPOSIT_BAL_ADJ,
     +                                CICA_BAL_ADJ,
     +                                POST_RETIRE_PAYABLE_BAL_ADJ,
     +                                ACCRUED_PENSION_BAL_ADJ,
     +                                DEFERRED_GAINS_BAL_ADJ,
     +                                STROM_RESERVE_BAL_ADJ,
     +                                ACCOUNTS_PAYABLE_BAL_ADJ,
     +                                ARO_NET_ASSETS_BAL_ADJ,
     +                                ARO_LIABILITY_BAL_ADJ,
     +                                DEFERRED_PURCHASE_POWER_ADJ,
     +                                COI_EARNINGS_ADJ)
         IF(CLASS_TYPE == 'R') THEN
            RATE_BASE_CASH_ADJ = DEFERRED_DEBIT_ADJUSTMENT -
     + ns_class_parameters%RBDFTX * 
     + ADJUSTMENT_2_DEFERRED_TAXES_BAL/100. -
     + ns_class_parameters%RBITC * ADJUSTMENT_2_DEFERRED_ITCS_BAL/100.
         ELSE
            RATE_BASE_CASH_ADJ = 0.
         ENDIF
         TEMP_FED_TAX_RATE = -999999.
         CALL RETURN_TAX_RATES(YEAR,CLASS,
     +                         TEMP_FED_TAX_RATE,
     +                       ns_class_parameters%ITC_AMORTIZATION_RATE,
     +                         ns_class_parameters%STATE_TAX_RATE,
     +                         ns_class_parameters%OPREV_TAX_RATE,
     +                         ns_class_parameters%PROPERTY_TAX_RATE,
     +                       ns_class_parameters%CLASS_OTHER_TAXES_RATE,
     +              ns_class_parameters%OTHER_TAXES_PERCENT_OF_EXPENSES,
     +      ns_class_parameters%STATE_DEDUC_PERCT_OF_TIB4DEDUC,
     +                 ns_class_parameters%FED_DEDUC_PERCT_OF_TIB4DEDUC,
     +                         CLASS_TYPE)


         IF(TEMP_FED_TAX_RATE /= -999999.) THEN
            REAL_VOID = STORE_FEDERAL_TAX_RATE(TEMP_FED_TAX_RATE)
         ELSE
            REAL_VOID = STORE_FEDERAL_TAX_RATE(0.) 
         ENDIF

         CALL RETURN_CAPITAL_TAX_ITEMS(YEAR,CLASS,
     +                    ns_class_canadian%PROVINCIAL_CAPITAL_TAX_RATE,
     +                     ns_class_canadian%FEDERAL_CAPITAL_TAX_RATE,
     + ns_class_canadian%PROVINCIAL_CAP_TAX_DEDUCTION,
     +                  ns_class_canadian%FEDERAL_CAPITAL_TAX_DEDUCTION,
     +                    ns_class_canadian%PROVINCIAL_CAP_TAX_ADDENDUM,
     +                   ns_class_canadian%FEDERAL_CAPITAL_TAX_ADDENDUM,
     +                                 CLASS_TYPE)

      ns_class_parameters%STD_INTEREST_RATE = 
     + ns_class_parameters%STD_INTEREST_RATE/100.
      ns_class_parameters%RETURN_ON_ST_INVEST = 
     + ns_class_parameters%RETURN_ON_ST_INVEST/100.
      ns_class_parameters%INTEREST_ON_CUSTOMER_DEPOSITS = 
     + ns_class_parameters%INTEREST_ON_CUSTOMER_DEPOSITS/100.
      ns_class_parameters%RETURN_ON_LONG_TERM_INVESTMENTS =
     +          ns_class_parameters%RETURN_ON_LONG_TERM_INVESTMENTS/100.
      ns_class_parameters%RETURN_RETIREMENT_MEDICAL_FUND=
     + ns_class_parameters%RETURN_RETIREMENT_MEDICAL_FUND/100.
      ns_class_parameters%NUCL_DECOM_FUND_RETURN = 
     + ns_class_parameters%NUCL_DECOM_FUND_RETURN/100.
      ns_class_parameters%LTD_SINKING_FUND_LAG = 
     + MAX(1.,ns_class_parameters%LTD_SINKING_FUND_LAG)
      ns_class_parameters%PS_SINKING_FUND_LAG = 
     + MAX(1.,ns_class_parameters%PS_SINKING_FUND_LAG)
      ns_class_parameters%CLASS_BTL_EXPENSES = 
     + ns_class_parameters%CLASS_BTL_EXPENSES + 
     + ns_class_parameters%BTL_LEASE_PAYMENTS

      ns_class_parameters%ADDENDUM_2_RATE_BASE = 
     + ns_class_parameters%ADDENDUM_2_RATE_BASE + ADJ01
      ns_class_parameters%STATE_TAX_RATE  = 
     + ns_class_parameters%STATE_TAX_RATE/D100
      ns_class_parameters%COMMON_PAYOUT_RATIO  = 
     + ns_class_parameters%COMMON_PAYOUT_RATIO/D100
      ns_class_parameters%OPREV_TAX_RATE = 
     + ns_class_parameters%OPREV_TAX_RATE/D100
      IF(IOU_UTILITY) THEN
         ns_class_parameters%PSRATE = ns_class_parameters%PSRATE/D100
         ns_class_parameters%RBITC  = ns_class_parameters%RBITC/D100
         ns_class_parameters%RBDFTX = ns_class_parameters%RBDFTX/D100
         ns_class_parameters%EQRATO = ns_class_parameters%EQRATO/D100
         ns_class_parameters%PSRATO = ns_class_parameters%PSRATO/D100
         ns_class_parameters%PSSINK = ns_class_parameters%PSSINK/D100
         ns_class_parameters%RBANEC = ns_class_parameters%RBANEC/D100
         ns_class_parameters%RB_INCLUDED_DEF_TAXES_DR_RATE =
     +    ns_class_parameters%RB_INCLUDED_DEF_TAXES_DR_RATE/100.  ! 155
         ns_class_parameters%RB_EXCLUDED_PENSION_LIABILITY_RATE =
     + ns_class_parameters%RB_EXCLUDED_PENSION_LIABILITY_RATE/100.! 156
         ns_class_parameters%RB_EXCLUDED_Deferred_Gain_RATE =
     +     ns_class_parameters%RB_EXCLUDED_Deferred_Gain_RATE/100. ! 157
         ns_class_parameters%RB_EXCLUDED_Storm_Reserve_RATE =
     +     ns_class_parameters%RB_EXCLUDED_Storm_Reserve_RATE/100. ! 158
     
         !159
         ns_class_parameters%RB_EXCLUDED_Accrued_Vacation_Pay_RATE =
     +    ns_class_parameters%RB_EXCLUDED_Accrued_Vacation_Pay_RATE/100.
         ! 160
         ns_class_parameters%RB_INCLUDED_DEFERRED_REVENUES_RATE =
     +   ns_class_parameters%RB_INCLUDED_DEFERRED_REVENUES_RATE/100. 
     
      ns_class_parameters%ODEC_MaxEquityRatio = 
     + ns_class_parameters%ODEC_MaxEquityRatio/100
     
         ns_class_parameters%ODEC_MinPayoutRatio = 
     + ns_class_parameters%ODEC_MinPayoutRatio/ 
     + 100.
    
         ns_class_parameters%ODEC_MaxPayoutRatio = 
     + ns_class_parameters%ODEC_MaxPayoutRatio/100.
     
         ns_class_parameters%ODEC_PercentREReduction = 
     + ns_class_parameters%ODEC_PercentREReduction/100.

C
         ns_class_parameters%NUC_DEOM_FUND_RATEBASE = 
     + ns_class_parameters%NUC_DEOM_FUND_RATEBASE/D100
         ns_class_parameters%CIAC_EXCLUDED_FROM_RATEBASE = 
     + ns_class_parameters%CIAC_EXCLUDED_FROM_RATEBASE/D100
         ns_class_parameters%DEPOSITS_EXCLUDED_FROM_RATEBASE =
     +   ns_class_parameters%DEPOSITS_EXCLUDED_FROM_RATEBASE/D100
         ns_class_parameters%ITC_AMORTIZATION_RATE = 
     + ns_class_parameters%ITC_AMORTIZATION_RATE/D100
         ns_class_parameters%MAX_EQUITY_IN_CAP_STRUCTURE = 
     + ns_class_parameters%MAX_EQUITY_IN_CAP_STRUCTURE/D100
         ns_class_parameters%CAP_LEASES_IN_RATEBASE = 
     + ns_class_parameters%CAP_LEASES_IN_RATEBASE/D100
      ELSE
         ns_class_parameters%STATE_TAX_RATE  = 0.
         ns_class_parameters%MAX_EQUITY_IN_CAP_STRUCTURE = 0.
         ns_class_parameters%DEFERRED_TAX_ADJUSTMENT = 0.
         ns_class_master%DEF_TAX_RATEBASE = 0.
         ns_class_master%COMMON_DIVIDENDS = 
     + ns_class_parameters%DIVIDEND_PER_SHARE
         ns_class_parameters%CSMIN = 0.
         ns_class_parameters%CSMAX = 0.
         ns_class_parameters%PSMIN = 0.
         ns_class_parameters%PSMAX = 0.
         ns_class_parameters%PSRATE = 0.
         ns_class_parameters%RBITC  = 0.
         ns_class_parameters%RBDFTX = 0.
         ns_class_parameters%EQRATO = 0.
         ns_class_parameters%PSRATO = 0.
         ns_class_parameters%PSSINK = 0.
         ns_class_parameters%RBANEC = 0.
         ns_class_parameters%ITC_AMORTIZATION_RATE  = 0.
         ns_class_parameters%NUC_DEOM_FUND_RATEBASE = 0.
      ENDIF
C
C     ADJUST SELECTED PARAMETERS
C
      ns_class_parameters%MIN_COV_RATIO = TARGET_RATIO
      ns_class_parameters%ROEQU  = ROEQU1/D100
      ns_class_parameters%RETURN_ON_NPV  = 
     + ns_class_parameters%RETURN_ON_NPV/D100
      IF(COVERAGE_RATIO < 8)ns_class_parameters%TARGET_EQUITY_RATIO=
     + ns_class_parameters%TARGET_EQUITY_RATIO/D100
      ns_class_parameters%LTDRTE = ns_class_parameters%LTDRTE/D100
      ns_class_parameters%LDSINK = ns_class_parameters%LDSINK/D100
      ns_class_parameters%OPREV_IN_ASSETS_NEC = 
     + ns_class_parameters%OPREV_IN_ASSETS_NEC/D100
      ns_class_parameters%NPV_IN_ASSETS_NEC = 
     +ns_class_parameters%NPV_IN_ASSETS_NEC/D100
      ns_class_parameters%OPREV_IN_LIBS_NEC = 
     + ns_class_parameters%OPREV_IN_LIBS_NEC/D100
      ns_class_parameters%NPV_IN_LIBS_NEC = 
     + ns_class_parameters%NPV_IN_LIBS_NEC/D100
      ns_class_parameters%OM_CAPITALIZATION_RATE = 
     + ns_class_parameters%OM_CAPITALIZATION_RATE/D100

      RETURN

      ENTRY READ_ELIMINATIONS_PARM_FILE(RETIREMENT_MEDICAL_PAYMENTS,
     +                                  SALVAGE_TRANSACTIONS)

      ZEROS = '0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,'//
     +        '0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,'//
     +        '0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,'//
     +        '0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,'//
     +        '0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,'//
     +        '0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,'//
     +        '0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,'//
     +        '0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,'//
     +        '0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,'//
     +        '0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,'//
     +        '0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,'//
     +        '0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,'
      READ(ZEROS,*) ROEQU1,ns_class_parameters%RETURN_ON_NPV,
     +   ns_class_parameters%COMMON_PAYOUT_RATIO,
     + ns_class_parameters%DIVIDEND_PER_SHARE,
     + ns_class_parameters%PSRATE,
     +   ns_class_parameters%LTDRTE,ns_class_parameters%CLASS_SALES,
     +   ns_class_parameters%CLASS_BASE_REVENUE,
     + ns_class_parameters%CLASS_ADJ_CLAUSE_REVENUE,
     +   ns_class_parameters%CLASS_OTHER_REVENUE,
     +   ns_class_master%ADJ_RATE_REVENUE,
     + ns_class_parameters%MAXIMUM_RATE_CHANGE,
     + ns_class_parameters%MIN_ST_INVESTMENTS,ns_class_parameters%RBITC,
     +   ns_class_parameters%RBDFTX,ns_class_parameters%RBANEC,
     + ns_class_parameters%ADDENDUM_2_RATE_BASE,ADJ01,
     + ns_class_parameters%EQRATO,ns_class_parameters%PSRATO,
     +   ns_class_parameters%MARKET_2_BOOK,ns_class_parameters%CSUNIT,
     + ns_class_parameters%CSMIN,
     +   ns_class_parameters%CSMAX,ns_class_parameters%PSUNIT,
     + ns_class_parameters%PSMIN,ns_class_parameters%PSMAX,
     + ns_class_parameters%PSSINK,ns_class_parameters%STDMIN,
     + ns_class_parameters%STD_MAX,ns_class_parameters%LDUNIT,
     + ns_class_parameters%LDLIFE,
     +   ns_class_parameters%LTDMIN,ns_class_parameters%LTDMAX,
     + ns_class_parameters%LDSINK,TARGET_RATIO,
     + ns_class_parameters%TARGET_EQUITY_RATIO,
     +   ns_class_parameters%OPREV_IN_ASSETS_NEC,
     +   ns_class_parameters%NPV_IN_ASSETS_NEC,
     + ns_class_parameters%ADDENUM_2_ASSETS_NEC,
     + ns_class_parameters%OPREV_IN_LIBS_NEC,
     +   ns_class_parameters%NPV_IN_LIBS_NEC,
     + ns_class_parameters%ADDENUM_2_LIBS_NEC,
     +   ns_class_parameters%SUBSIDIARY_DIVIDEND,
     + ns_class_parameters%SUBSIDIARY_INCOME,
     +   ns_class_parameters%NEW_SUBSIDIARY_INVESTMENT,
     + ns_class_parameters%CLASS_BTL_REVENUES,
     +PARM_TABLE_FED_TAX_RATE,ns_class_parameters%ITC_AMORTIZATION_RATE,
     +   ns_class_ctaxes%FED_INCOME_TAX_DEDUCTIONS,
     +   ns_class_parameters%STATE_TAX_RATE,
     +   ns_class_ctaxes%STATE_INCOME_TAX_DEDUCTIONS,
     + ns_class_parameters%OPREV_TAX_RATE,
     +   ns_class_parameters%PROPERTY_TAX_RATE,
     + ns_class_parameters%CLASS_OTHER_TAXES_RATE,
     +   ns_class_parameters%CLASS_ADDENDUM_2_OTHER_TAXES,
     +   ns_class_parameters%DEFERRED_TAX_ADJUSTMENT,
     +   ns_class_parameters%PARM_AMORTIZATION_ADJUSTMENT,
     + ns_class_parameters%DEFERRED_REVENUES,
     +   ns_class_parameters%AMORT_DEF_REVENUES,
     +   ns_class_parameters%AMORT_DEF_REVENUES,
     +   ns_class_parameters%RETURN_ON_ST_INVEST, 
     +   SALVAGE_TRANSACTIONS,
     + ns_class_parameters%AMT_PREP_DEPRE_ADDEN,
     + ns_class_ctaxes%ADJTXNORM,
     +   ns_class_parameters%CASH_OTH_ASSET,
     + ns_class_parameters%NUCL_DECOM_FUND,
     + ns_class_parameters%NUCL_DECOM_FUND_RETURN,
     +   ns_class_parameters%NUC_DEOM_FUND_RATEBASE,
C ADDED 4/13/92 FOR WPK WORK 
     +   ns_class_parameters%OM_CAPITALIZATION_RATE,
C ADDED 9/29/92 FOR SRP
     +   ns_class_parameters%CLASS_ADDENDUM_TO_PROPERTY_TAX,
     +   ns_class_parameters%ADDENDUM_TO_ITC_AMORTIZATED,
C ADDED 10/28/92 FOR KEPCO
     +   ns_class_parameters%DEFERRED_EXPENSE_AMORT_PERIOD,
     +   ns_class_parameters%DEFERRED_EXPENSE_CASH,
     +   ns_class_parameters%DEFERRED_EXPENSE_AMORT,
C ADDED 3/26/93 FOR UPA AND OTHERS
     +   ns_class_parameters%MAX_EQUITY_IN_CAP_STRUCTURE,
     +   ns_class_parameters%CLASS_BTL_EXPENSES,
     +   ns_class_parameters%CASH_FROM_SALE_OF_ASSETS,
     +   ns_class_parameters%GROSS_VALUE_OF_SOLD_ASSETS,
     +   ns_class_parameters%CUMULATIVE_DEP_ON_SOLD_ASSETS,
     +   ns_class_parameters%ADJUSTMENT_TO_DEFERRED_TAXES,
     +   ns_class_parameters%ATL_AMORT_OF_BOOK,
     +   ns_class_parameters%BTL_AMORT_OF_BOOK,
     +   ns_class_parameters%LEASE_PAYMENTS_IN_TIER,
     +   ns_class_parameters%CAPITIALIZED_LEASE_ADDITIONS,
     +   ns_class_parameters%ATL_LEASE_PAYMENTS,
     +   ns_class_parameters%BTL_LEASE_PAYMENTS,
C ADDED 6/18/93 FOR WKP 
     +   ns_class_canadian%PROVINCIAL_CAPITAL_TAX_RATE,
     + ns_class_canadian%FEDERAL_CAPITAL_TAX_RATE,
     +   ns_class_canadian%PROVINCIAL_CAP_TAX_DEDUCTION,
     +   ns_class_canadian%FEDERAL_CAPITAL_TAX_DEDUCTION,
C ADDED 5/5/93 FOR FP&L TC WORK     ITEMS 93-100
     +   ns_class_parameters%CUSTOMER_DEPOSITS_PERCENT_REVS,
     +   ns_class_parameters%ADDENDUM_TO_CUSTOMER_DEPOSITS,
     +   ns_class_parameters%INTEREST_ON_CUSTOMER_DEPOSITS,
     +   ns_class_parameters%STD_IN_WEIGHTED_COST_PERCENT,
     +   ns_class_parameters%CUSTOMER_DEPOSITS_IN_WC_PERCENT,
     +   ns_class_parameters%DEF_ITC_IN_WC_PERCENT,
     +   ns_class_parameters%DEF_TAX_CR_IN_WC_PERCENT,
     +   ns_class_parameters%MAX_COMMON_BUY_BACK,
C ADDED 7/15/93 FOR WKP ITEMS 105-108
     +   ns_class_parameters%CIAC_CASH,
     +   ns_class_parameters%CIAC_AMORTIZATION_RATE,
     +   ns_class_parameters%CIAC_AMORTIZATION_ADDENDUM,
     +   ns_class_parameters%CIAC_EXCLUDED_FROM_RATEBASE,
C ADDED 7 PASS THROUGHT VARIABLES FOR TVA 10/93
c    +   PASS_THROUGH_VALUES(4:10),
C ITEMS 116-119 9/6/94 FOR OHIO EDISON
     +   ns_class_parameters%BTL_EMISSION_CREDIT_EXPENSE,
     +   ns_class_parameters%BTL_EMISSION_CREDIT_REVENUE,
     +   ns_class_parameters%ADJ_CLAUSE_REVENUE_ALLOCATION,
     +   ns_class_parameters%ADJ_CLAUSE_EXPENSE_ALLOCATION,
C ITEMS 120-122 3/9/95 ADDED FOR SHAREHOLDER VALUE
     +   ns_class_parameters%COST_OF_CAPITAL_RISK_ADJUSTMENT,
     +   ns_class_parameters%ADJUSTMENT_2_ECONOMIC_ASSETS,
     +   ns_class_parameters%ADJUSTMENT_TO_OPERATING_PROFITS,
     +   ns_class_parameters%LTD_SINKING_FUND_LAG,
     +   ns_class_parameters%PS_SINKING_FUND_LAG,
     +   ns_class_parameters%CAP_LEASES_IN_RATEBASE,
     +   ns_class_parameters%INCOME_TAX_DEPRECIATION,
     +   ns_class_parameters%ACE_TAX_DEPRECIATION,
     +   ns_class_parameters%CIAC_BALANCE_IN_WC_PERCENT,
     +   ns_class_parameters%CAPITAL_RECOVERY_NOT_IN_OPAT,
     +   ns_class_parameters%BTL_MISC_DEDUCTIONS,
C ITEMS 133-140 10/16/96 ADDED FOR SRP AND GENERAL
     +   ns_class_parameters%MAX_SHORT_TERM_INVESTMENT,
     +   ns_class_parameters%MIN_LONG_TERM_INVESTMENT,
     +   ns_class_parameters%MAX_LONG_TERM_INVESTMENT,
     +   ns_class_parameters%CHANGE_IN_LONG_TERM_INVESTMENTS,
     +   ns_class_parameters%RETURN_ON_LONG_TERM_INVESTMENTS,
     +   RETIREMENT_MEDICAL_PAYMENTS,
     +   ns_class_parameters%RETIREE_MEDICAL_PAYMENTS,
     +   ns_class_parameters%RETURN_RETIREMENT_MEDICAL_FUND,
     +   ns_class_parameters%MINIMUM_COMMON_STOCK_BALANCE,
     +   ns_class_parameters%DEPOSITS_EXCLUDED_FROM_RATEBASE,
     +   ns_class_parameters%CHANGE_2_DECOMMISSIONING_LIAB,
     +   ns_class_parameters%ADJUSTMENT_2_OP_REV_TAX,
     +   ns_class_parameters%OTHER_TAXES_PERCENT_OF_EXPENSES,
     +   ns_class_parameters%ADDENDUM_2_SHARES_ISSUED,
     +   ns_class_parameters%STATE_DEDUC_PERCT_OF_TIB4DEDUC,
     +   ns_class_parameters%FED_DEDUC_PERCT_OF_TIB4DEDUC
      RETURN

      ENTRY RETURN_CLASS_EXTERNALITY_VALUES(R_PASS_THROUGH_VALUES)

         R_PASS_THROUGH_VALUES(1:10) = PASS_THROUGH_VALUES(1:10)
      RETURN
      END

      FUNCTION FEDERAL_INCOME_TAX_RATE()

      REAL*4 FEDERAL_INCOME_TAX_RATE,FED_TAX_RATE
      REAL*4 STORE_FEDERAL_TAX_RATE,R_FED_TAX_RATE,
     +       PARM_TABLE_FED_TAX_RATE,
     +       STORE_PARM_TABLE_FED_TAX_RATE
      SAVE FED_TAX_RATE,PARM_TABLE_FED_TAX_RATE
      LOGICAL*1 R_PARAMETER_TABLE_FOUND,PARAMETER_TABLE_FOUND/.FALSE./
C
         IF(PARAMETER_TABLE_FOUND) THEN
            FEDERAL_INCOME_TAX_RATE = PARM_TABLE_FED_TAX_RATE
            FED_TAX_RATE = PARM_TABLE_FED_TAX_RATE
            PARAMETER_TABLE_FOUND = .FALSE.
         ELSE
            FEDERAL_INCOME_TAX_RATE = FED_TAX_RATE
         ENDIF
      RETURN

      ENTRY STORE_FEDERAL_TAX_RATE(R_FED_TAX_RATE)

         IF(R_FED_TAX_RATE <= 1.) THEN
            FED_TAX_RATE = 100. * R_FED_TAX_RATE
         ELSE
            FED_TAX_RATE = R_FED_TAX_RATE
         ENDIF
         PARM_TABLE_FED_TAX_RATE = R_FED_TAX_RATE
         STORE_FEDERAL_TAX_RATE = FED_TAX_RATE
      RETURN

      ENTRY STORE_PARM_TABLE_FED_TAX_RATE(R_FED_TAX_RATE)

         PARM_TABLE_FED_TAX_RATE = R_FED_TAX_RATE
c        PARM_TABLE_FED_TAX_RATE = 35. ! R_FED_TAX_RATE
         PARAMETER_TABLE_FOUND = .TRUE.
         STORE_PARM_TABLE_FED_TAX_RATE = PARM_TABLE_FED_TAX_RATE
      RETURN
      END

      SUBROUTINE CLASS_SHAREHOLDER_VALUE(VARIABLE_VALUE,
     +                                   R_OPERATING_ASSETS_YEAR_END,
     +                                   EMBEDDED_PARENT_LTD_COST,
     +                                   EMBEDDED_PARENT_PS_COST)

C

      use spindriftlib
      use prod_arrays_dimensions
      use sizecom
      use class_parameters
      use class_ctaxes


C
C SHAREHOLDER VALUE CALCUALTIONS 
C
      REAL*4 WORKING_CAP,R_OPERATING_ASSETS_YEAR_END
      REAL*4 VARIABLE_VALUE(0:*)
      REAL*4 CASH_INCOME_TAXES,
     +     CASH_REVENUE,
     +     CAPITAL_RECOVERY,
     +     OPERATING_PROFITS_AFTER_TAXES,
     +     DEDUCTIBLE_EXPENSES,
     +     RISK_ADJUSTED_COST_OF_CAPITAL,
     +     AFTER_TAX_COST_OF_CAPITAL,
     +     OPERATING_ASSETS_YEAR_END,
     +     OPERATING_ASSETS_YEAR_START,
     +     ECONOMIC_RETURN_OPENING_BALANCE,
     +     ECONOMIC_RETURN_AVERAGE_BALANCE,
     +     ECONOMIC_RETURN_CLOSING_BALANCE,
     +     COST_OF_SHAREHOLDER_CAPATIAL,
     +     SHAREHOLDER_VALUE_ADDED,
     +     VALUE_ADDED_WO_INTEREST_INCOME,
     +     EFFECTIVE_TAX_RATE,FEDERAL_INCOME_TAX_RATE
      REAL*4 PRODUCTION_EXPENSES,NON_PRODUCTION_EXPENSES,
     +       OPERATING_MARGIN,
     +       TAXES_NON_INCOME,
     +       TAXABLE_OPERATING_PROFITS,
     +       OPT_INCOME_B4_INCOME_TAXES,
     +       EMBEDDED_PARENT_LTD_COST,LTD_COST,
     +       EMBEDDED_PARENT_PS_COST,PS_COST
      LOGICAL*1 UTILITY_IS_IP
      REAL*4 NOT_AVAIL
      PARAMETER(NOT_AVAIL=-999999.)
C
         OPERATING_ASSETS_YEAR_START = R_OPERATING_ASSETS_YEAR_END
         CASH_REVENUE = VARIABLE_VALUE(4) +  ! OPERATING_REVENUES(2)
     +                  VARIABLE_VALUE(33) +  ! Other Income
     +                  VARIABLE_VALUE(34)   ! Interest Income
         TAXES_NON_INCOME = VARIABLE_VALUE(315)
         EFFECTIVE_TAX_RATE = FEDERAL_INCOME_TAX_RATE()/100. *
     + (1.-ns_class_parameters%STATE_TAX_RATE/100.) + 
     + ns_class_parameters%STATE_TAX_RATE/100.
         CAPITAL_RECOVERY = (1.
     + -ns_class_parameters%CAPITAL_RECOVERY_NOT_IN_OPAT/100.)*
     +                      (VARIABLE_VALUE(17) +   ! Book Depreciation
     +                    VARIABLE_VALUE(18) +   ! ATL+BTL Amortization
     +                       VARIABLE_VALUE(19) +   ! CIAC Amortization
     +                  VARIABLE_VALUE(229))   ! Nuclear Fuel Owned Burn
         IF(UTILITY_IS_IP()) THEN
            LTD_COST = EMBEDDED_PARENT_LTD_COST
            PS_COST = EMBEDDED_PARENT_PS_COST
            PRODUCTION_EXPENSES = VARIABLE_VALUE(5) +  ! Fossil Fuel
     +                  VARIABLE_VALUE(227) + ! Nuclear Fuel Tax Expense
     +                           VARIABLE_VALUE(6) +  ! Purchase Expense
     +                VARIABLE_VALUE(15) +  ! Emissions Credit Expense
     +                            VARIABLE_VALUE(7)    ! Variable O&M
            OPERATING_MARGIN =  VARIABLE_VALUE(2) +  ! Energy Sales
     +                          VARIABLE_VALUE(380) -  ! Capacity Sales
     +                          PRODUCTION_EXPENSES
           CAPITAL_RECOVERY = VARIABLE_VALUE(17) +   ! Book Depreciation
     +                    VARIABLE_VALUE(18) +   ! ATL+BTL Amortization
     +                         VARIABLE_VALUE(19)    ! CIAC Amortization
     
            ! Fixed O&M Production Expense
            DEDUCTIBLE_EXPENSES = VARIABLE_VALUE(8) +   
     +               VARIABLE_VALUE(9) +   ! Service Transaction Expense
     +                         VARIABLE_VALUE(10) +  ! Other O&M
     +                         VARIABLE_VALUE(11) +  ! Gas Purchases
     +                         VARIABLE_VALUE(12) +  ! Other 1 Expenses
     +                        VARIABLE_VALUE(13) +  ! DSM Program Expens
     +               VARIABLE_VALUE(14) +  ! DSM Program Rebate Expense
     +                         VARIABLE_VALUE(60) +  ! Lease Expenses
     +                VARIABLE_VALUE(290) + ! Retirement Medical Expense
     +                         VARIABLE_VALUE(304) + ! Other 2 Expenses
     +                         VARIABLE_VALUE(321) + ! Customer Accounts
     +                         VARIABLE_VALUE(322) + ! Customer Services
     +                         VARIABLE_VALUE(323) + ! Sales_expense
     +                         VARIABLE_VALUE(324) + ! A&G Operations
     +                         VARIABLE_VALUE(325) + ! A&G Maintenance
     +                    VARIABLE_VALUE(335) + ! Transmission Operation
     +                  VARIABLE_VALUE(336) + ! Transmission Maintenance
     +                    VARIABLE_VALUE(337) + ! Distribution Operation
     +                  VARIABLE_VALUE(338)   ! Distribution Maintenance
            TAXABLE_OPERATING_PROFITS = CASH_REVENUE - 
     +                                  DEDUCTIBLE_EXPENSES -
     +                                  PRODUCTION_EXPENSES -
     +                                  TAXES_NON_INCOME -
     +                                  CAPITAL_RECOVERY
            OPT_INCOME_B4_INCOME_TAXES = TAXABLE_OPERATING_PROFITS +
     +               ns_class_parameters%ADJUSTMENT_TO_OPERATING_PROFITS
            CASH_INCOME_TAXES = EFFECTIVE_TAX_RATE *
     +                                        OPT_INCOME_B4_INCOME_TAXES
            OPERATING_PROFITS_AFTER_TAXES = OPT_INCOME_B4_INCOME_TAXES -
     +                                      CASH_INCOME_TAXES
         ELSE   
            LTD_COST = ns_class_parameters%LTDRTE
            PS_COST = ns_class_parameters%PSRATE
            PRODUCTION_EXPENSES = VARIABLE_VALUE(5) +  ! Fossil Fuel
     +                  VARIABLE_VALUE(227) + ! Nuclear Fuel Tax Expense
     +                           VARIABLE_VALUE(6) +  ! Purchase Expense
     +                            VARIABLE_VALUE(7)    ! Variable O&M
            OPERATING_MARGIN =  VARIABLE_VALUE(2) +  ! Energy Sales
     +                          VARIABLE_VALUE(380) -  ! Capacity Sales
     +                          PRODUCTION_EXPENSES
     
            ! TOTAL DEDUCTIBLE TAX EXPENSES FROM BOOK
            DEDUCTIBLE_EXPENSES = VARIABLE_VALUE(210) + 
     +                            VARIABLE_VALUE(5) +  ! Fossil Fuel
     +                           VARIABLE_VALUE(6) +  ! Purchase Expense
     +                            VARIABLE_VALUE(7) +   ! Variable O&M
     +                            TAXES_NON_INCOME -
     +                            VARIABLE_VALUE(39)  - ! LTDINT
     +                            VARIABLE_VALUE(40)  - ! STDINT
     +                            VARIABLE_VALUE(227)   ! NF_TAX_EXPENSE
            ! Fixed O&M Production Expense
            NON_PRODUCTION_EXPENSES = VARIABLE_VALUE(8)      
     +            + VARIABLE_VALUE(9)      ! Service Transaction Expense
     +                            + VARIABLE_VALUE(10)    ! Other O&M
     +                           + VARIABLE_VALUE(11)    ! Gas Purchases
     +                     + VARIABLE_VALUE(12)    ! Other 1 Expenses
     +                   + VARIABLE_VALUE(13)    ! DSM Program Expens
     +             + VARIABLE_VALUE(14)    ! DSM Program Rebate Expense
     +               + VARIABLE_VALUE(15)    ! Emissions Credit Expense
     +                       + VARIABLE_VALUE(60)    ! Lease Expenses
     +           + VARIABLE_VALUE(290)  ! Retirement Medical Expense
     +                   + VARIABLE_VALUE(304)  ! Other 2 Expenses
     +                     + VARIABLE_VALUE(321)  ! Customer Accounts
     +                    + VARIABLE_VALUE(322)  ! Customer Services
     +                        + VARIABLE_VALUE(323)  ! Sales_expense
     +                       + VARIABLE_VALUE(324)  ! A&G Operations
     +                      + VARIABLE_VALUE(325)  ! A&G Maintenance
     +                   + VARIABLE_VALUE(335)  ! Transmission Operation
     +                 + VARIABLE_VALUE(336)  ! Transmission Maintenance
     +               + VARIABLE_VALUE(337)  ! Distribution Operation
     +                 + VARIABLE_VALUE(338)  ! Distribution Maintenance
            DEDUCTIBLE_EXPENSES = NON_PRODUCTION_EXPENSES
     +                            + PRODUCTION_EXPENSES
     
            ! TOTAL DEDUCTIBLE TAX EXPENSES FROM BOOK
            DEDUCTIBLE_EXPENSES = VARIABLE_VALUE(210) - 
     +                           VARIABLE_VALUE(39)  - ! LTDINT
     +                            VARIABLE_VALUE(40)  ! STDINT
            CASH_INCOME_TAXES = EFFECTIVE_TAX_RATE *
     +                         (CASH_REVENUE - DEDUCTIBLE_EXPENSES -
     +                         VARIABLE_VALUE(130) -  ! Tax Depreciation
     +                            VARIABLE_VALUE(39)  -  ! LTDINT
     +                            VARIABLE_VALUE(40))    ! STDINT
c    +                           VARIABLE_VALUE(227))   ! NF_TAX_EXPENSE
            ! TOTAL DEDUCTIBLE TAX EXPENSES FROM BOOK
            DEDUCTIBLE_EXPENSES = VARIABLE_VALUE(210) - 
     +                            VARIABLE_VALUE(39)  - ! LTDINT
     +                            VARIABLE_VALUE(40) + ! STDINT
     +                        VARIABLE_VALUE(17) +   ! Book Depreciation
     +                     VARIABLE_VALUE(18) +   ! ATL+BTL Amortization
     +                       VARIABLE_VALUE(19) +   ! CIAC Amortization
     +                  VARIABLE_VALUE(229)    ! Nuclear Fuel Owned Burn
            TAXABLE_OPERATING_PROFITS = CASH_REVENUE - 
     +                                  DEDUCTIBLE_EXPENSES
            OPERATING_PROFITS_AFTER_TAXES = TAXABLE_OPERATING_PROFITS - 
     +                                   VARIABLE_VALUE(39)  -  ! LTDINT
     +                                  VARIABLE_VALUE(40) -    ! STDINT
     +                                   CASH_INCOME_TAXES +
     +                                   CAPITAL_RECOVERY +
     +             ns_class_parameters%ADJUSTMENT_TO_OPERATING_PROFITS
         ENDIF
C
C OPERATING ASSETS
C
         WORKING_CAP = VARIABLE_VALUE(75)-VARIABLE_VALUE(89)
         OPERATING_ASSETS_YEAR_END = VARIABLE_VALUE(68) + ! NPV(2)
     +                               WORKING_CAP  -     ! WORKING CAP
     +                               VARIABLE_VALUE(87) - ! DEF TAX CR
     +                               VARIABLE_VALUE(88) + ! DEF TAX CR
     +                  ns_class_parameters%ADJUSTMENT_2_ECONOMIC_ASSETS
C ITEMS 120-121 3/9/95 ADDED FOR SHAREHOLDER VALUE
         RISK_ADJUSTED_COST_OF_CAPITAL = ns_class_parameters%EQRATO * 
     + (ns_class_parameters%ROEQU +
     +      ns_class_parameters%COST_OF_CAPITAL_RISK_ADJUSTMENT/100.) +
     +                           ns_class_parameters%PSRATO * PS_COST +
     + (1. - ns_class_parameters%EQRATO - ns_class_parameters%PSRATO) 
     + * LTD_COST
       AFTER_TAX_COST_OF_CAPITAL = ns_class_parameters%EQRATO * 
     + (ns_class_parameters%ROEQU +
     +    ns_class_parameters%COST_OF_CAPITAL_RISK_ADJUSTMENT/100.) +
     +                           ns_class_parameters%PSRATO * PS_COST +
     + (1. - ns_class_parameters%EQRATO - ns_class_parameters%PSRATO)
     + *  LTD_COST *
     +                                         (1. - EFFECTIVE_TAX_RATE)
         COST_OF_SHAREHOLDER_CAPATIAL = OPERATING_ASSETS_YEAR_END *
     +                                         AFTER_TAX_COST_OF_CAPITAL
         SHAREHOLDER_VALUE_ADDED = OPERATING_PROFITS_AFTER_TAXES -
     +                                      COST_OF_SHAREHOLDER_CAPATIAL
         VALUE_ADDED_WO_INTEREST_INCOME = SHAREHOLDER_VALUE_ADDED -
     +                           VARIABLE_VALUE(34) *
     +                                         (1. - EFFECTIVE_TAX_RATE)
         IF(ABS(OPERATING_ASSETS_YEAR_START) < .001)
     +                                  OPERATING_ASSETS_YEAR_START = 0.
         IF(ABS(OPERATING_ASSETS_YEAR_END) < .001)
     +                                    OPERATING_ASSETS_YEAR_END = 0.
         IF(OPERATING_ASSETS_YEAR_START /= 0.) THEN
            ECONOMIC_RETURN_OPENING_BALANCE =
     +                               100.*OPERATING_PROFITS_AFTER_TAXES/
     +                                     OPERATING_ASSETS_YEAR_START
         ELSE
            ECONOMIC_RETURN_OPENING_BALANCE = NOT_AVAIL
         ENDIF
         IF(OPERATING_ASSETS_YEAR_START +
     +                             OPERATING_ASSETS_YEAR_END /= 0.) THEN
            ECONOMIC_RETURN_AVERAGE_BALANCE =
     +                      200.*OPERATING_PROFITS_AFTER_TAXES/
     +         (OPERATING_ASSETS_YEAR_START + OPERATING_ASSETS_YEAR_END)
         ELSE
            ECONOMIC_RETURN_AVERAGE_BALANCE = NOT_AVAIL
         ENDIF
         IF(OPERATING_ASSETS_YEAR_END /= 0.) THEN
            ECONOMIC_RETURN_CLOSING_BALANCE =
     +                              100.*OPERATING_PROFITS_AFTER_TAXES/
     +                                     OPERATING_ASSETS_YEAR_END
         ELSE
            ECONOMIC_RETURN_CLOSING_BALANCE = NOT_AVAIL
         ENDIF
C

         VARIABLE_VALUE(316) = PRODUCTION_EXPENSES
         VARIABLE_VALUE(459) = NON_PRODUCTION_EXPENSES
         VARIABLE_VALUE(317) = OPERATING_MARGIN
         VARIABLE_VALUE(254) = CASH_REVENUE
         VARIABLE_VALUE(319) = CASH_REVENUE - PRODUCTION_EXPENSES
         VARIABLE_VALUE(255) = DEDUCTIBLE_EXPENSES
         VARIABLE_VALUE(256) = CAPITAL_RECOVERY
         VARIABLE_VALUE(257) = CASH_INCOME_TAXES
         VARIABLE_VALUE(258) = 
     + ns_class_parameters%ADJUSTMENT_TO_OPERATING_PROFITS
         VARIABLE_VALUE(259) = OPERATING_PROFITS_AFTER_TAXES
         VARIABLE_VALUE(260) = OPERATING_ASSETS_YEAR_START

         VARIABLE_VALUE(261) = WORKING_CAP ! WCCUM(2)

         VARIABLE_VALUE(318) = VARIABLE_VALUE(87) + ! DEF TAX CR
     +                         VARIABLE_VALUE(88)   ! DEF TAX ITC
         VARIABLE_VALUE(262) = 
     + ns_class_parameters%ADJUSTMENT_2_ECONOMIC_ASSETS
         VARIABLE_VALUE(263) = OPERATING_ASSETS_YEAR_END
         VARIABLE_VALUE(264) = (OPERATING_ASSETS_YEAR_START +
     +                                     OPERATING_ASSETS_YEAR_END)/2.
         VARIABLE_VALUE(265) = 100.*ns_class_parameters%ROEQU
         VARIABLE_VALUE(266) = 
     + ns_class_parameters%COST_OF_CAPITAL_RISK_ADJUSTMENT
         VARIABLE_VALUE(267) = 100.*ns_class_parameters%EQRATO
         VARIABLE_VALUE(268) = 
     + ns_class_parameters%EQRATO*(100.*ns_class_parameters%ROEQU +
     +            ns_class_parameters%COST_OF_CAPITAL_RISK_ADJUSTMENT)
         VARIABLE_VALUE(269) = 100. * PS_COST
         VARIABLE_VALUE(270) = 100. * ns_class_parameters%PSRATO
        VARIABLE_VALUE(271)= 100. * ns_class_parameters%PSRATO * PS_COST
         VARIABLE_VALUE(272) = 100. * LTD_COST
      VARIABLE_VALUE(627) = 100. * ns_class_parameters%STD_INTEREST_RATE
         VARIABLE_VALUE(273) = 100. * (1. - ns_class_parameters%EQRATO
     +  - ns_class_parameters%PSRATO)
         VARIABLE_VALUE(274) = 100. * (1. - ns_class_parameters%EQRATO
     +    - ns_class_parameters%PSRATO) * LTD_COST
         VARIABLE_VALUE(275) = 100. * EFFECTIVE_TAX_RATE
         VARIABLE_VALUE(276) = 100. * (1. - ns_class_parameters%EQRATO 
     + - ns_class_parameters%PSRATO) *
     +                                    LTD_COST *
     +                                         (1. - EFFECTIVE_TAX_RATE)
         VARIABLE_VALUE(277) = RISK_ADJUSTED_COST_OF_CAPITAL*100.
         VARIABLE_VALUE(278) = AFTER_TAX_COST_OF_CAPITAL*100.
         VARIABLE_VALUE(279) = COST_OF_SHAREHOLDER_CAPATIAL
         VARIABLE_VALUE(280) = SHAREHOLDER_VALUE_ADDED
         VARIABLE_VALUE(281) = VALUE_ADDED_WO_INTEREST_INCOME
         VARIABLE_VALUE(282) = OPERATING_ASSETS_YEAR_END -
     +                                       OPERATING_ASSETS_YEAR_START
         VARIABLE_VALUE(283) = ECONOMIC_RETURN_OPENING_BALANCE
         VARIABLE_VALUE(284) = ECONOMIC_RETURN_AVERAGE_BALANCE
         VARIABLE_VALUE(285) = ECONOMIC_RETURN_CLOSING_BALANCE
         R_OPERATING_ASSETS_YEAR_END = OPERATING_ASSETS_YEAR_END
      RETURN

      ENTRY CLASS_SHAREHOLDER_VALUE_NA(VARIABLE_VALUE)

C
c        VARIABLE_VALUE(254) = NOT_AVAIL
c        VARIABLE_VALUE(255) = NOT_AVAIL
c        VARIABLE_VALUE(256) = NOT_AVAIL
c        VARIABLE_VALUE(257) = NOT_AVAIL
c        VARIABLE_VALUE(258) = NOT_AVAIL
c        VARIABLE_VALUE(259) = NOT_AVAIL
c        VARIABLE_VALUE(260) = NOT_AVAIL
C        VARIABLE_VALUE(261) = NOT_AVAIL
C        VARIABLE_VALUE(262) = NOT_AVAIL
C        VARIABLE_VALUE(263) = NOT_AVAIL
c        VARIABLE_VALUE(264) = NOT_AVAIL
         VARIABLE_VALUE(265) = NOT_AVAIL
         VARIABLE_VALUE(266) = NOT_AVAIL
         VARIABLE_VALUE(267) = NOT_AVAIL
         VARIABLE_VALUE(268) = NOT_AVAIL
         VARIABLE_VALUE(269) = NOT_AVAIL
         VARIABLE_VALUE(270) = NOT_AVAIL
         VARIABLE_VALUE(271) = NOT_AVAIL
         VARIABLE_VALUE(272) = NOT_AVAIL
         VARIABLE_VALUE(273) = NOT_AVAIL
         VARIABLE_VALUE(274) = NOT_AVAIL
         VARIABLE_VALUE(275) = NOT_AVAIL
         VARIABLE_VALUE(276) = NOT_AVAIL
         VARIABLE_VALUE(277) = NOT_AVAIL
         VARIABLE_VALUE(278) = NOT_AVAIL
         VARIABLE_VALUE(279) = NOT_AVAIL
         VARIABLE_VALUE(280) = NOT_AVAIL
         VARIABLE_VALUE(281) = NOT_AVAIL
         VARIABLE_VALUE(282) = NOT_AVAIL
         VARIABLE_VALUE(283) = NOT_AVAIL
         VARIABLE_VALUE(284) = NOT_AVAIL
         VARIABLE_VALUE(285) = NOT_AVAIL
         VARIABLE_VALUE(627) = NOT_AVAIL
      RETURN
C
C ZERO OUT THE COST OF CAPITAL STUFF INTEGER*2 THE SUBSIDIARY TOTALS
C
      ENTRY CLASS_SUB_TOTALS_VALUE_NA(VARIABLE_VALUE)
C
         OPERATING_ASSETS_YEAR_END = VARIABLE_VALUE(263)
         OPERATING_PROFITS_AFTER_TAXES = VARIABLE_VALUE(259)
         IF(OPERATING_ASSETS_YEAR_END /= 0.) THEN
            COST_OF_SHAREHOLDER_CAPATIAL = VARIABLE_VALUE(279)
            AFTER_TAX_COST_OF_CAPITAL = COST_OF_SHAREHOLDER_CAPATIAL/
     +                                         OPERATING_ASSETS_YEAR_END
            VARIABLE_VALUE(278) = 100.* AFTER_TAX_COST_OF_CAPITAL
            VARIABLE_VALUE(283) = 100. * OPERATING_PROFITS_AFTER_TAXES/
     +                                         OPERATING_ASSETS_YEAR_END
         ELSE
            VARIABLE_VALUE(278) = NOT_AVAIL
         ENDIF
         IF(VARIABLE_VALUE(263) /= 0.) THEN
            VARIABLE_VALUE(284) = 100. * OPERATING_PROFITS_AFTER_TAXES/
     +                                               VARIABLE_VALUE(263)
         ELSE   
            VARIABLE_VALUE(284) = NOT_AVAIL
         ENDIF
         IF(VARIABLE_VALUE(264) /= 0.) THEN
            VARIABLE_VALUE(285) = 100. * OPERATING_PROFITS_AFTER_TAXES/
     +                                               VARIABLE_VALUE(264)
         ELSE   
            VARIABLE_VALUE(285) = NOT_AVAIL
         ENDIF
C
         VARIABLE_VALUE(265) = NOT_AVAIL
         VARIABLE_VALUE(266) = NOT_AVAIL
         VARIABLE_VALUE(267) = NOT_AVAIL
         VARIABLE_VALUE(268) = NOT_AVAIL
         VARIABLE_VALUE(269) = NOT_AVAIL
         VARIABLE_VALUE(270) = NOT_AVAIL
         VARIABLE_VALUE(271) = NOT_AVAIL
         VARIABLE_VALUE(272) = NOT_AVAIL
         VARIABLE_VALUE(273) = NOT_AVAIL
         VARIABLE_VALUE(274) = NOT_AVAIL
         VARIABLE_VALUE(275) = NOT_AVAIL
         VARIABLE_VALUE(276) = NOT_AVAIL
         VARIABLE_VALUE(277) = NOT_AVAIL
         RETURN
      END

      FUNCTION NET_DEFERRED_CASH_VALUE(YR,
     +                                   NET_ANNUAL_DEFERRED_CASH_VALUE,
     +                                   ANNUAL_DEFERRED_CASH_AMORT)

C

      use spindriftlib
      use prod_arrays_dimensions
      use kepcocom
      USE SIZECOM

      use class_parameters



      INTEGER*2 ACTIVE_YEARS
      PARAMETER(ACTIVE_YEARS=MAX_FINANCIAL_SIMULATION_YEARS-1)
      REAL*4 NET_DEFERRED_CASH_VALUE,
     +     ANNUAL_DEFERRED_CASH_AMORT(0:ACTIVE_YEARS),
     +     NET_ANNUAL_DEFERRED_CASH_VALUE(0:ACTIVE_YEARS)
      INTEGER*2 YR,I
      REAL*4 UNAMORT_AMOUNT,CURRENT_AMORT,ANNUAL_AMORT
C
C DEFERRED CASH EXPENSE CALCULATIONS
C
         IF(ns_class_parameters%DEFERRED_EXPENSE_AMORT_PERIOD <= .5) 
     + THEN
            ANNUAL_DEFERRED_CASH_AMORT(YR) = 
     + ns_class_parameters%DEFERRED_EXPENSE_CASH +
     +   ANNUAL_DEFERRED_CASH_AMORT(YR)
         ELSEIF(
     + ns_class_parameters%DEFERRED_EXPENSE_AMORT_PERIOD <= 1.) THEN
            ANNUAL_DEFERRED_CASH_AMORT(YR) = 
     + ns_class_parameters%DEFERRED_EXPENSE_CASH/2. +
     +               ANNUAL_DEFERRED_CASH_AMORT(YR)
            NET_ANNUAL_DEFERRED_CASH_VALUE(YR) =
     +                              NET_ANNUAL_DEFERRED_CASH_VALUE(YR) +
     +                      ns_class_parameters%DEFERRED_EXPENSE_CASH/2.
            ANNUAL_DEFERRED_CASH_AMORT(YR+1) = 
     + ns_class_parameters%DEFERRED_EXPENSE_CASH/2.+
     +                                  ANNUAL_DEFERRED_CASH_AMORT(YR+1)
         ELSEIF(
     + ns_class_parameters%DEFERRED_EXPENSE_AMORT_PERIOD < 98.) THEN
            ANNUAL_AMORT = ns_class_parameters%DEFERRED_EXPENSE_CASH/
     +               ns_class_parameters%DEFERRED_EXPENSE_AMORT_PERIOD
            UNAMORT_AMOUNT = 
     + ns_class_parameters%DEFERRED_EXPENSE_CASH - ANNUAL_AMORT/2.
            ANNUAL_DEFERRED_CASH_AMORT(YR) = ANNUAL_AMORT/2. +
     +                                  ANNUAL_DEFERRED_CASH_AMORT(YR)
            NET_ANNUAL_DEFERRED_CASH_VALUE(YR) = UNAMORT_AMOUNT +
     +                                NET_ANNUAL_DEFERRED_CASH_VALUE(YR)
            I = YR + 1
            DOWHILE(UNAMORT_AMOUNT > 0. .AND. I <= ACTIVE_YEARS)
               CURRENT_AMORT = MIN(UNAMORT_AMOUNT,ANNUAL_AMORT)
               UNAMORT_AMOUNT = UNAMORT_AMOUNT - CURRENT_AMORT
               ANNUAL_DEFERRED_CASH_AMORT(I) = CURRENT_AMORT +
     +                                     ANNUAL_DEFERRED_CASH_AMORT(I)
               NET_ANNUAL_DEFERRED_CASH_VALUE(I) = UNAMORT_AMOUNT +
     +                                 NET_ANNUAL_DEFERRED_CASH_VALUE(I)
     +                               
               I = I + 1
            ENDDO
         ELSE
            DO I = YR, ACTIVE_YEARS
               NET_ANNUAL_DEFERRED_CASH_VALUE(I)=
     + ns_class_parameters%DEFERRED_EXPENSE_CASH +
     +     NET_ANNUAL_DEFERRED_CASH_VALUE(I)
            ENDDO
         ENDIF
         IF(REALLY_KEPCO) THEN
C
C 12/20/92 THE FOLLOWING LOGIC IS FOR KEPCO IT ASSUMES THAT SOME KEPCO
C DEFERRED WOLF CREEK MAINTENANCE ENERGY EXPENSE FOR THE FIRST TWO YEARS
C IS FROM THE FINANCIAL PARAMETER FILE WHERE IT IS READ IN AS
C ns_class_parameters%DEFERRED_EXPENSE_AMORT
C
C6/10/97 NEED TO ADD A SPECIFIC ENTRY IN A DATA FILE FOR THE UNAMORTIZED
C WC MAINTENANCE-M.S.G.
C
            ns_class_parameters%DEFERRED_EXPENSE_CASH = 
     + ns_class_parameters%DEFERRED_EXPENSE_CASH +
     +                                      KEPCO_WC_DEF_MAINT_ENRG_COST
       KEPCO_WC_DEF_MAINT_ENRG_AMORT = 
     + ns_class_parameters%DEFERRED_EXPENSE_AMORT +
     +                                     KEPCO_WC_DEF_MAINT_ENRG_AMORT
            ns_class_parameters%DEFERRED_EXPENSE_AMORT = 
     + ANNUAL_DEFERRED_CASH_AMORT(YR) +
     +                                     KEPCO_WC_DEF_MAINT_ENRG_AMORT
            DO I = YR, ACTIVE_YEARS
               NET_ANNUAL_DEFERRED_CASH_VALUE(I) =
     +                               NET_ANNUAL_DEFERRED_CASH_VALUE(I) +
     +                               KEPCO_WC_DEF_MAINT_ENRG_COST
            ENDDO
         ELSE
            ns_class_parameters%DEFERRED_EXPENSE_AMORT = 
     + ns_class_parameters%DEFERRED_EXPENSE_AMORT +
     +                                    ANNUAL_DEFERRED_CASH_AMORT(YR)
         ENDIF
         NET_DEFERRED_CASH_VALUE = NET_ANNUAL_DEFERRED_CASH_VALUE(YR) 
      RETURN
      END
