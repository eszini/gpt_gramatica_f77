
!     finanbse.for
!     Copyright(c) 2000

!     Created: 8/13/2003 12:33:17 PM
!     Author : MARK S GERBER
!     Last change: MSG 8/26/2009 9:14:06 AM

!
!                        F I N / A N C E
!        Copyright (c) 1982, 1983 M.S. Gerber & Associates, Inc.
!                      All Rights Reserved
!

!
!     Purpose:  FIN/ANCE is the heart of the FIN system.  This
!               module computes the liabilities portion of the
!               balance sheet, and the tax and interest expenses
!               on the income statement.
!

!
      SUBROUTINE FINANCE(RUN_YEAR,WRITE_THE_RESULTS,YEARS_TO_RUN)
      use midasmod_decs
      use globecom
      use foshydi2com
      use class_tva_ai_financial
      use spindriftlib
      use prod_arrays_dimensions
      use financial_switches_common
      use kepcocom
      use sizecom
      use envircom
      use servcom
      use retslcom
      use tva_ai_financial
      use msgoutpt
      use enrglimit
      implicit none

      INTEGER (kind=2) ::  LAST_FIN_PARM_RECORD=AVAIL_DATA_YEARS
      INTEGER (kind=2) ::  CLASS_NUM
      INTEGER (kind=4) ::  VALUES_TO_ZERO
      INTEGER (kind=4) ::  ERROR_CODE
      LOGICAL (kind=4) ::  WRITE_THE_RESULTS
      INTEGER (kind=2) ::  LAST_VARIABLE,LAST_OUTPUT_VARIABLE,IOS
      REAL*4 var_values(:)
      ALLOCATABLE :: var_values


      LOGICAL (kind=1) ::  POOLING_TRANSACTIONS
      LOGICAL (kind=1) ::  COVERAGE_RATIO_4_TO_7_OR_10,
     +          RUN_THIS_CODE,USE_BUDGET_EXPENSES

      CHARACTER (len=1) ::  TYPETAXPAID
      INTEGER (kind=2) ::  YEARS_TO_RUN,PARM_YEAR
      INTEGER (kind=2) ::  I,ITER,RUN_YEARS,EXTENSION_YEARS,

     +          MXITER,SPMETH,TXMETH,RUN_YEAR,DELETE
      REAL (kind=4) ::  INTLTD
      REAL (kind=4) ::  LTDLAST,CUM_NF_EXPENSE,CUM_NF_RATEBASE_EXP,
     +     ROR,LTDISP,PSDIVP,PSISSP,PS_DIV,
     +     FORECAST_SALES,EXTERNAL_FINANCING_REQUIRED,
     +     EXTERNAL_FINANCING,
     +     GET_ENERGY_EXCHANGE_ADJUSTMENT,
     +     NOT_AVAIL,CUSTOMER_DEPOSITS_BAL(2),
     +     R_CUSTOMER_DEPOSITS_BAL1,
     +     R_CUSTOMER_DEPOSITS_BAL2,TEMP_TAX,
     +     RESERVE_CAPACITY_PURCHASES,
     +     RESERVE_CAPACITY_SALES,
     +     NUC_FUEL_TOTAL_BURN,
     +     NF_SL_DEF_TAX_DEP,
     +     NF_DEFERRED_TAX_BASIS
      LOGICAL (kind=1) ::  DUKE
      REAL (kind=4) ::  RATEBASE_GOPINC,TRB,R_TRB,R_RATEBASE_GOPINC
      REAL (kind=4) ::  CUM_SALVAGE_TRANSACTIONS,AFUDC_BORROWED_RATE,
     +     TAXABLE_BTL_INCOME

!
! DUKE CATAWBA STUFF
!
      REAL (kind=4) ::  CATAWBA_DEFERRED_TAXES
      REAL (kind=4) ::  CATAWBA_TOTAL_OTHER_NET_REVS,
     +       CATAWBA_TOTAL_LEVEL_CAP_PAYMTS
!
! CICA STUFF 7/15/93
!
      REAL (kind=4) ::  CIAC_BALANCE(2)
!
! TVA STUFF
!
      REAL (kind=4) ::  CUM_AI_DEP
      REAL (kind=4) ::  CUM_AI_GPV,AI_LAGGED_GPV,AI_CWIP_BALANCE
!
      REAL (kind=8) ::  GET_TOTAL_SALES_REVENUE
      REAL (kind=8) ::  GET_VAR_COST,GET_FUEL_COST
      LOGICAL (kind=1) ::  DONEIN
      LOGICAL (kind=1) ::  DONEDV,DONETX,FIRST_PASS,FUNDS_BALANCE,
     +          IOU_UTILITY,USE_EXTERNAL_PRODUCTION_COST,
     +          SALT_RIVER_PROJECT
      LOGICAL (kind=1) ::  NUC_FUEL_TAX_EXP_IS_BURN=.TRUE.
!
! FP&L STUFF ADDED 5/5/93 M.S.G.
!
      REAL (kind=4) ::  CUSTOMER_DEPOSITS_PERCENT_REVS,
     +     ADDENDUM_TO_CUSTOMER_DEPOSITS,
     +     INTEREST_ON_CUSTOMER_DEPOSITS
      REAL (kind=4) ::  EXP_INC_EMIS_EXPENSE
      LOGICAL (kind=1) ::  TEST_FINANCE_ELIM
!
      PARAMETER(NOT_AVAIL=-999999.)
      CHARACTER (LEN=1) :: UTILITY_TYPE

      SAVE SPMETH,CUM_NF_EXPENSE,CUM_NF_RATEBASE_EXP,IOU_UTILITY,
     +     CUM_AI_DEP,CUM_AI_GPV,CUM_SALVAGE_TRANSACTIONS,
     +     CUSTOMER_DEPOSITS_BAL,AI_LAGGED_GPV,AI_CWIP_BALANCE
      SAVE TRB,RATEBASE_GOPINC

      LOGICAL (kind=1) ::  WKP_ACTIVE,ASSET_CLASS_REPORTS
      SAVE WKP_ACTIVE

!     READ EXECUTION FILE AND OPEN ASSET FILES, OUTPUT FILES
!     IF THE BASE CASE AND FIRST YEAR

      MXITER = 20
    1 IF(RUN_YEAR == 1) THEN

! READ TAX LOSSES
!
         ns_tva_ai_financial%AI_GPV = 0.
         CALL ASSET_CLASS_ANALYSIS_BY(RUN_YEAR,WRITE_THE_RESULTS)


         IF(WRITE_THE_RESULTS) THEN
            LAST_OUTPUT_VARIABLE = get_last_variable()
            ALLOCATE(var_values(0:LAST_OUTPUT_VARIABLE))
            var_values = NOT_AVAIL
            CALL RETURN_ASSET_CLASS_VALUES(var_values,
     +                                   INT2(1), ! COVERAGE_RATIO_TYPE,
     +                                     'O', ! OPERATING_METHOD,
     +                                    0.,  ! LEASE_PAYMENTS_IN_TIER,
     +                                     0.)  ! SRP_RATIO)

            WKP_ACTIVE = WKP_OBJECT(var_values)

!  WRITE-BASE YEAR DATA HERE

            CALL OUTPUT_RESULTS(var_values,get_BASE_YEAR())
            DEALLOCATE(var_values,STAT=ERROR_CODE)
         ENDIF
      ENDIF
!
! TVA STUFF
!


      IF(UTILITY_TYPE() == 'T' .AND. USE_EXTERNAL_PRODUCTION_COST())THEN
         CALL EXTERNAL_AI_ANALYSIS
         CALL RESET_UNIT_COUNT_TO_PSYM
         CALL AI_ANALYSIS(RUN_YEAR,ns_tva_ai_financial%AI_CASH,
     + ns_tva_ai_financial%AI_DEPRECIATION,
     + ns_tva_ai_financial%AI_AFUDC1,ns_tva_ai_financial%AI_EXPENSE,
     +                    ns_tva_ai_financial%AI_TAX_DEPRECIATION,
     +                    ns_tva_ai_financial%AI_AMT_PREFERENCES,
     +                    ns_tva_ai_financial%AI_DEFERRED_TAX)
         CALL RESET_UNIT_COUNT_TO_MIDAS
      ELSE
         CALL AI_ANALYSIS(RUN_YEAR,ns_tva_ai_financial%AI_CASH,
     + ns_tva_ai_financial%AI_DEPRECIATION,
     +     ns_tva_ai_financial%AI_AFUDC1,ns_tva_ai_financial%AI_EXPENSE,
     +                       ns_tva_ai_financial%AI_TAX_DEPRECIATION,
     +                       ns_tva_ai_financial%AI_AMT_PREFERENCES,
     +                       ns_tva_ai_financial%AI_DEFERRED_TAX)
      ENDIF
      CALL ASSET_CLASS_ANALYSIS(RUN_YEAR,WRITE_THE_RESULTS)
      RETURN
      END

!
!             THE ADDITIONS & IMPROVMENTS ANALYSIS MODULE FOR TVA
!                             COPYRIGHT (C) 1989
!                        M.S. GERBER & ASSOCIATES, INC.
!                              ALL RIGHTS RESERVED
!

!
      SUBROUTINE AI_ANALYSIS(RUN_YEAR,AI_CASH_ord,
     +                       AI_DEPRECIATION_ord,
     +                       AI_AFUDC1_ord,
     +                       AI_EXPENSE_ord,
     +                       AI_TAX_DEPRECIATION_ord,
     +                       AI_AMT_PREFERENCES_ord,
     +                       AI_DEFERRED_TAX_ord)
!
      use spindriftlib
      use prod_arrays_dimensions
      use sizecom
      use globecom
      use foshydi2com
      use aitvafin
      use aitvaprod
      use mthnmcom
      use prod2com
      use tva_ai_financial
      use enrglimit
      use nunits_shared
      
      implicit none

      REAL (kind=4) ::  R_MONTHLY_CLASS_VALUES(0:12,*)
      INTEGER (kind=2) ::  MO
      REAL (kind=4) ::  AI_CASH_ord,
     + AI_DEPRECIATION_ord,
     + AI_AFUDC1_ord,
     +     AI_EXPENSE_ord,
     +     AI_TAX_DEPRECIATION_ord,
     +     AI_AMT_PREFERENCES_ord,
     +     AI_DEFERRED_TAX_ord
      INTEGER (kind=2) ::  DATE,RUN_YEAR,START_DEP_YEAR
      REAL (kind=4) ::  CL_LAST_YEAR_INVESTMENT(:)
      REAL (kind=4) ::  EL_LAST_YEAR_INVESTMENT(:),


     +     AI_ANNUAL_DEP(:),
     +     AI_ANNUAL_TAX_DEP(:),
     +     AI_ANNUAL_DEFERRED_TAX(:),
     +     AI_ANNUAL_AMT_PREFERENCE_ITEMS(:)
      ALLOCATABLE :: CL_LAST_YEAR_INVESTMENT,
     +               EL_LAST_YEAR_INVESTMENT,
     +               AI_ANNUAL_DEP,
     +               AI_ANNUAL_TAX_DEP,
     +               AI_ANNUAL_DEFERRED_TAX,
     +               AI_ANNUAL_AMT_PREFERENCE_ITEMS

! CLASS VARAIBLES

      INTEGER (kind=4) ::  VALUES_2_ZERO
      INTEGER (kind=2) ::  MAX_AI_CLASSES
      INTEGER (kind=2) ::  RETURN_MAX_CL_CLASS_NUM,VOID_INT2,
     +          GET_CL_ASSET_CLASS_INFO,R_CLASS,
     +          RETURN_MAX_EL_CLASS_NUM
      SAVE MAX_AI_CLASSES
      REAL (kind=4) ::  R_AI_ANNUAL_CLASS_DEP,
     +     R_AI_ANNUAL_CLASS_TAX_DEP,
     +     R_AI_ANNUAL_CLASS_DEFERRED_TAX,
     +     R_AI_ANNUAL_CLASS_PREF_ITEMS,
     +     R_AI_CLASS_CASH,
     +     R_AI_CLASS_EXPENSE,
     +     R_AI_CLASS_AFUDC1
      INTEGER (kind=2) ::  ASSET_CLASS_NUM(:),ASSET_VECTOR(:)
      ALLOCATABLE :: ASSET_CLASS_NUM,ASSET_VECTOR
      REAL (kind=4) ::  AI_ANNUAL_CLASS_DEP(:,:),
     +     AI_ANNUAL_CLASS_TAX_DEP(:,:),
     +     AI_ANNUAL_CLASS_DEFERRED_TAX(:,:),
     +     AI_ANNUAL_CLASS_AMT_PREF_ITEMS(:,:),
     +     AI_CLASS_CASH(:),
     +     AI_CLASS_EXPENSE(:),
     +     AI_CLASS_AFUDC1(:)
      REAL (kind=4) ::  R_AI_LAGGED_GPV,AI_LAGGED_GPV(:)
      ALLOCATABLE :: AI_ANNUAL_CLASS_DEP,
     +               AI_ANNUAL_CLASS_TAX_DEP,
     +               AI_ANNUAL_CLASS_DEFERRED_TAX,
     +               AI_ANNUAL_CLASS_AMT_PREF_ITEMS,
     +               AI_CLASS_CASH,
     +               AI_CLASS_EXPENSE,
     +               AI_CLASS_AFUDC1,AI_LAGGED_GPV

      SAVE AI_ANNUAL_CLASS_DEP,
     +     AI_ANNUAL_CLASS_TAX_DEP,
     +     AI_ANNUAL_CLASS_DEFERRED_TAX,
     +     AI_ANNUAL_CLASS_AMT_PREF_ITEMS,
     +     AI_CLASS_CASH,
     +     AI_CLASS_EXPENSE,
     +     AI_CLASS_AFUDC1,AI_LAGGED_GPV

      SAVE AI_ANNUAL_DEP,
     +     AI_ANNUAL_TAX_DEP,
     +     AI_ANNUAL_DEFERRED_TAX,
     +     AI_ANNUAL_AMT_PREFERENCE_ITEMS,
     +     CL_LAST_YEAR_INVESTMENT,
     +     EL_LAST_YEAR_INVESTMENT

      IF(RUN_YEAR == 1) THEN
         IF(ALLOCATED(AI_ANNUAL_DEP)) DEALLOCATE(AI_ANNUAL_DEP,
     +                                   AI_ANNUAL_TAX_DEP,
     +                                   AI_ANNUAL_DEFERRED_TAX,
     +                                   AI_ANNUAL_AMT_PREFERENCE_ITEMS,
     +                                   CL_LAST_YEAR_INVESTMENT,
     +                                   EL_LAST_YEAR_INVESTMENT)
         ALLOCATE(AI_ANNUAL_DEP(MAX_FINANCIAL_SIMULATION_YEARS),
     +           AI_ANNUAL_TAX_DEP(MAX_FINANCIAL_SIMULATION_YEARS),
     +           AI_ANNUAL_DEFERRED_TAX(MAX_FINANCIAL_SIMULATION_YEARS),
     +           AI_ANNUAL_AMT_PREFERENCE_ITEMS
     +                                 (MAX_FINANCIAL_SIMULATION_YEARS),
     +           CL_LAST_YEAR_INVESTMENT(MAX_CL_UNITS),
     +           EL_LAST_YEAR_INVESTMENT(MAX_EL_UNITS))
         AI_ANNUAL_DEP = 0.
         AI_ANNUAL_TAX_DEP = 0.
         AI_ANNUAL_DEFERRED_TAX = 0.
         AI_ANNUAL_AMT_PREFERENCE_ITEMS = 0.
         EL_LAST_YEAR_INVESTMENT = 0.
         CL_LAST_YEAR_INVESTMENT = 0.

! ASSET CLASS ADDITIONS

         IF(ALLOCATED(AI_ANNUAL_CLASS_DEP))
     +                  DEALLOCATE(AI_ANNUAL_CLASS_DEP,
     +                             AI_ANNUAL_CLASS_TAX_DEP,
     +                             AI_ANNUAL_CLASS_DEFERRED_TAX,
     +                             AI_ANNUAL_CLASS_AMT_PREF_ITEMS,
     +                             AI_CLASS_CASH,
     +                             AI_CLASS_EXPENSE,
     +                             AI_CLASS_AFUDC1,AI_LAGGED_GPV)
         MAX_AI_CLASSES = MAX(RETURN_MAX_CL_CLASS_NUM(),
     +                        RETURN_MAX_EL_CLASS_NUM())
         ALLOCATE(AI_ANNUAL_CLASS_DEP(MAX_FINANCIAL_SIMULATION_YEARS,
     +                                                0:MAX_AI_CLASSES),
     +           AI_ANNUAL_CLASS_TAX_DEP(MAX_FINANCIAL_SIMULATION_YEARS,
     +                                                0:MAX_AI_CLASSES),
     +           AI_ANNUAL_CLASS_DEFERRED_TAX(
     +                                   MAX_FINANCIAL_SIMULATION_YEARS,
     +                                                0:MAX_AI_CLASSES),
     +           AI_ANNUAL_CLASS_AMT_PREF_ITEMS(
     +                                   MAX_FINANCIAL_SIMULATION_YEARS,
     +                                                0:MAX_AI_CLASSES),
     +           AI_CLASS_CASH(0:MAX_AI_CLASSES),
     +           AI_CLASS_EXPENSE(0:MAX_AI_CLASSES),
     +           AI_CLASS_AFUDC1(0:MAX_AI_CLASSES),
     +           AI_LAGGED_GPV(0:MAX_AI_CLASSES))
         AI_ANNUAL_CLASS_DEP = 0.
         AI_ANNUAL_CLASS_TAX_DEP = 0.
         AI_ANNUAL_CLASS_DEFERRED_TAX = 0.
         AI_ANNUAL_CLASS_AMT_PREF_ITEMS = 0.
         AI_LAGGED_GPV = 0.
      ENDIF

      AI_CLASS_CASH = 0.
      AI_CLASS_EXPENSE = 0.
      AI_CLASS_AFUDC1 = 0.

      AI_CASH_ord = 0.
      AI_EXPENSE_ord = 0.
      AI_AFUDC1_ord = 0.
      DATE = get_BASE_YEAR() - 1900 + globecom_year
      START_DEP_YEAR = RUN_YEAR + 1
      IF(get_nunits() > 0) THEN
         ALLOCATE(
     +              ASSET_CLASS_NUM(get_nunits()),
     +              ASSET_VECTOR(get_nunits()))
         VOID_INT2=GET_CL_ASSET_CLASS_INFO(ASSET_CLASS_NUM,ASSET_VECTOR)
         CALL AI_DEP_AFUDC(START_DEP_YEAR,
     + AI_CASH_ord,
     + AI_AFUDC1_ord,
     +                     AI_EXPENSE_ord,
     +                     AI_ANNUAL_DEP,
     +                     AI_ANNUAL_TAX_DEP,
     +                     AI_ANNUAL_DEFERRED_TAX,
     +                     AI_ANNUAL_AMT_PREFERENCE_ITEMS,
     +                     CL_AI_INVESTMENT,AI_CL_REMAINING_LIFE,
     +                     AI_CL_TAX_LIFE,AI_CL_ADR_LIFE,
     +  get_nunits(),ONLINE,OFLINE,RUN_YEAR,

     +                     CL_LAST_YEAR_INVESTMENT,
! ASSET CLASS ADDITIONS

     +                     ASSET_CLASS_NUM,ASSET_VECTOR,MAX_AI_CLASSES,
     +                     AI_ANNUAL_CLASS_DEP,
     +                     AI_ANNUAL_CLASS_TAX_DEP,
     +                     AI_ANNUAL_CLASS_DEFERRED_TAX,
     +                     AI_ANNUAL_CLASS_AMT_PREF_ITEMS,
     +                     AI_CLASS_CASH,
     +                     AI_CLASS_EXPENSE,
     +                     AI_CLASS_AFUDC1)
         DEALLOCATE(ASSET_CLASS_NUM,ASSET_VECTOR)
      ENDIF
      IF(HYDRO_UNITS > 0) THEN
         ALLOCATE(ASSET_CLASS_NUM(HYDRO_UNITS),
     +            ASSET_VECTOR(HYDRO_UNITS))
         CALL GET_EL_ASSET_CLASS_INFO(ASSET_CLASS_NUM,ASSET_VECTOR)
         CALL AI_DEP_AFUDC(START_DEP_YEAR,
     + AI_CASH_ord,
     + AI_AFUDC1_ord,
     +                     AI_EXPENSE_ord,
     +                     AI_ANNUAL_DEP,
     +                     AI_ANNUAL_TAX_DEP,
     +                     AI_ANNUAL_DEFERRED_TAX,
     +                     AI_ANNUAL_AMT_PREFERENCE_ITEMS,
     +                     EL_AI_INVESTMENT,AI_EL_REMAINING_LIFE,
     +                     AI_EL_TAX_LIFE,AI_EL_ADR_LIFE,
     +                     HYDRO_UNITS,ON_LINE,OFF_LINE,RUN_YEAR,
     +                     EL_LAST_YEAR_INVESTMENT,
     
! ASSET CLASS ADDITIONS

     +                     ASSET_CLASS_NUM,ASSET_VECTOR,MAX_AI_CLASSES,
     +                     AI_ANNUAL_CLASS_DEP,
     +                     AI_ANNUAL_CLASS_TAX_DEP,
     +                     AI_ANNUAL_CLASS_DEFERRED_TAX,
     +                     AI_ANNUAL_CLASS_AMT_PREF_ITEMS,
     +                     AI_CLASS_CASH,
     +                     AI_CLASS_EXPENSE,
     +                     AI_CLASS_AFUDC1)
         DEALLOCATE(ASSET_CLASS_NUM,ASSET_VECTOR)
      ENDIF



      AI_DEPRECIATION_ord = AI_ANNUAL_DEP(RUN_YEAR)
      AI_TAX_DEPRECIATION_ord =
     + AI_ANNUAL_TAX_DEP(RUN_YEAR)
      AI_DEFERRED_TAX_ord =
     + AI_ANNUAL_DEFERRED_TAX(RUN_YEAR)
      AI_AMT_PREFERENCES_ord =
     + AI_ANNUAL_AMT_PREFERENCE_ITEMS(RUN_YEAR)
      RETURN


      ENTRY RETURN_AI_ASSET_CLASS_EXPENSES(R_CLASS,RUN_YEAR,
     +                                 R_AI_ANNUAL_CLASS_DEP,
     +                                 R_AI_ANNUAL_CLASS_TAX_DEP,
     +                                 R_AI_ANNUAL_CLASS_DEFERRED_TAX,
     +                                 R_AI_ANNUAL_CLASS_PREF_ITEMS,
     +                                 R_AI_CLASS_CASH,
     +                                 R_AI_CLASS_EXPENSE,
     +                                 R_AI_CLASS_AFUDC1,
     +                                 R_AI_LAGGED_GPV)

         IF(R_CLASS >= 0 .AND. R_CLASS <= MAX_AI_CLASSES) THEN
            R_AI_ANNUAL_CLASS_DEP=AI_ANNUAL_CLASS_DEP(RUN_YEAR,R_CLASS)
            R_AI_ANNUAL_CLASS_TAX_DEP =
     +                         AI_ANNUAL_CLASS_TAX_DEP(RUN_YEAR,R_CLASS)
            R_AI_ANNUAL_CLASS_DEFERRED_TAX =
     +                    AI_ANNUAL_CLASS_DEFERRED_TAX(RUN_YEAR,R_CLASS)
            R_AI_ANNUAL_CLASS_PREF_ITEMS =
     +                  AI_ANNUAL_CLASS_AMT_PREF_ITEMS(RUN_YEAR,R_CLASS)
            R_AI_CLASS_CASH = AI_CLASS_CASH(R_CLASS)
            R_AI_CLASS_EXPENSE = AI_CLASS_EXPENSE(R_CLASS)
            R_AI_CLASS_AFUDC1 = AI_CLASS_AFUDC1(R_CLASS)
            R_AI_LAGGED_GPV = AI_LAGGED_GPV(R_CLASS)

            AI_LAGGED_GPV(R_CLASS) = R_AI_CLASS_CASH + R_AI_CLASS_AFUDC1

         ELSE
            R_AI_ANNUAL_CLASS_DEP = 0.
            R_AI_ANNUAL_CLASS_TAX_DEP = 0.
            R_AI_ANNUAL_CLASS_DEFERRED_TAX = 0.
            R_AI_ANNUAL_CLASS_PREF_ITEMS = 0.
            R_AI_CLASS_CASH = 0.
            R_AI_CLASS_EXPENSE = 0.
            R_AI_CLASS_AFUDC1 = 0.
            R_AI_LAGGED_GPV = 0.
         ENDIF
      RETURN


      ENTRY RETURN_AI_MONTHLY_CLASS_VALUES(R_CLASS,
     +                                 RUN_YEAR, !NOTE: ADD 1 WHEN USED
     +                                     R_MONTHLY_CLASS_VALUES)


!
         IF(R_CLASS >= 0 .AND. R_CLASS <= MAX_AI_CLASSES) THEN
            R_MONTHLY_CLASS_VALUES(0,cash_ai_investment) =
     +                    R_MONTHLY_CLASS_VALUES(0,cash_ai_investment) +


     +                    AI_CLASS_CASH(R_CLASS)
            R_MONTHLY_CLASS_VALUES(0,ai_book_investment) =
     +                    R_MONTHLY_CLASS_VALUES(0,ai_book_investment) +
     +                    AI_CLASS_CASH(R_CLASS)
            DO MO = 1, 12

               R_MONTHLY_CLASS_VALUES(MO,cash_ai_investment) =
     +                   R_MONTHLY_CLASS_VALUES(MO,cash_ai_investment) +

     +                   AI_CLASS_CASH(R_CLASS)/12.
               R_MONTHLY_CLASS_VALUES(MO,ai_book_investment) =
     +                   R_MONTHLY_CLASS_VALUES(MO,ai_book_investment) +
     +                   AI_CLASS_CASH(R_CLASS)/12.
            ENDDO
         ENDIF
      RETURN

      END



!                   THE DEPRECIATION & AFUDC FOR TVA ON
!                          ADDITIONS & IMPROVMENTS
!                             COPYRIGHT (C) 1989
!                        M.S. GERBER & ASSOCIATES, INC.
!                              ALL RIGHTS RESERVED


!
      SUBROUTINE AI_DEP_AFUDC(START_DEP_YEAR,AI_CASH,AI_AFUDC1,
     +                        AI_EXPENSE,
     +                        AI_ANNUAL_DEP,
     +                        AI_ANNUAL_TAX_DEP,
     +                        AI_ANNUAL_DEFERRED_TAX,
     +                        AI_ANNUAL_AMT_PREFERENCE_ITEMS,
     +                        INVESTMENT,AI_REMAINING_LIFE,
     +                        AI_TAX_LIFE,AI_ADR_TAX_LIFE,
     +               nunits,ON_LINE,OFF_LINE,YEAR,
     +                        LAST_YEAR_INVESTMENT,
     +                        ASSET_CLASS_NUM,ASSET_CLASS_VECTOR,
     +                        MAX_AI_CLASSES,
     +                        AI_ANNUAL_CLASS_DEP,
     +                        AI_ANNUAL_CLASS_TAX_DEP,
     +                        AI_ANNUAL_CLASS_DEFERRED_TAX,
     +                        AI_ANNUAL_CLASS_AMT_PREF_ITEMS,
     +                        AI_CLASS_CASH,
     +                        AI_CLASS_EXPENSE,
     +                        AI_CLASS_AFUDC1)

      use end_routine, only: end_program, er_message
      use SpinDriftLib

      use prod_arrays_dimensions
      USE SIZECOM
      use mod_base_year
      implicit none
      integer (kind=2) :: nunits
      INTEGER (kind=2) ::  DATE,AI_REMAINING_LIFE(*),


     +          ON_LINE(*),OFF_LINE(*),YEAR,YR
      REAL (kind=4) ::   AI_ANNUAL_DEP(*),
     +      AI_ANNUAL_TAX_DEP(*),
     +      AI_ANNUAL_DEFERRED_TAX(*),
     +      AI_ANNUAL_AMT_PREFERENCE_ITEMS(*)

      INTEGER (kind=2) ::  I,START_DEP_YEAR,J,POINTER_VALUE
      REAL (kind=4) ::  AI_CASH,AI_AFUDC1,INVESTMENT(*),GET_VAR
      REAL (kind=4) ::  AI_TAX_LIFE(*),AI_ADR_TAX_LIFE(*)
      REAL (kind=4) ::  INTEREST_CAPITIALIZATION_RATE,INTEREST_CAP_RATE,
     +       AI_BOOK_VALUE
      REAL (kind=4) ::  LAST_YEAR_INVESTMENT(nunits),AI_TAX_VALUE
      REAL (kind=4) ::  REMAINING_LIFE
      REAL (kind=4) ::  AI_INVESTMENT,AFUDC_RATE,AI_EXPENSE,
     +     ANNUAL_DEP,ANNUAL_AFUDC,AI_AFUDC_RATE
      CHARACTER (len=1) ::  UTILITY_TYPE
      LOGICAL (kind=1) ::  UTILITY_IS_TVA,UTILITY_IS_IOU
!
      REAL (kind=4) ::  AI_INVESTMENT_CLASS,AI_EXPENSE_CLASS
      INTEGER (kind=2) ::  ASSET_CLASS_NUM(*)
      INTEGER (kind=2) ::  ASSET_CLASS_VECTOR(*),MAX_AI_CLASSES
      INTEGER (kind=2) ::  ASSET_CLASS
      INTEGER (kind=2) ::  ASSET_ALLOCATION_VECTOR,CLASS_POINTER
      CHARACTER (len=1) ::  DUMMY_TYPE
      REAL (kind=4) ::
     +   AI_ANNUAL_CLASS_DEP(MAX_FINANCIAL_SIMULATION_YEARS,0:*),

     +     AI_ANNUAL_CLASS_TAX_DEP(MAX_FINANCIAL_SIMULATION_YEARS,0:*),
     +     AI_ANNUAL_CLASS_DEFERRED_TAX(
     +                              MAX_FINANCIAL_SIMULATION_YEARS,0:*),
     +     AI_ANNUAL_CLASS_AMT_PREF_ITEMS(
     +                              MAX_FINANCIAL_SIMULATION_YEARS,0:*),
     +     AI_CLASS_CASH(0:*),
     +     AI_CLASS_EXPENSE(0:*),
     +     AI_CLASS_AFUDC1(0:*)

      REAL (kind=4) ::  ASSET_CLASS_LIST(AVAIL_DATA_YEARS)
      REAL (kind=4) ::  ASSET_ALLOCATION_LIST(AVAIL_DATA_YEARS)
      REAL (kind=4) ::  ASSET_ALLOCATOR
!
      REAL (kind=4) ::  AI_TAX_DEP(:),


     +     AI_BOOK_DEP(:),
     +     AI_TAX_PREFERRENCE(:),
     +     AI_DEFERRED_TAXES(:)
      ALLOCATABLE :: AI_TAX_DEP,
     +               AI_BOOK_DEP,
     +               AI_TAX_PREFERRENCE,
     +               AI_DEFERRED_TAXES

      ALLOCATE(AI_TAX_DEP(MAX_FINANCIAL_SIMULATION_YEARS),
     +         AI_TAX_PREFERRENCE(MAX_FINANCIAL_SIMULATION_YEARS),
     +         AI_BOOK_DEP(MAX_FINANCIAL_SIMULATION_YEARS),
     +         AI_DEFERRED_TAXES(MAX_FINANCIAL_SIMULATION_YEARS))

      AFUDC_RATE = AI_AFUDC_RATE()
      INTEREST_CAPITIALIZATION_RATE = INTEREST_CAP_RATE()
      UTILITY_IS_TVA = UTILITY_TYPE() == 'T'
      UTILITY_IS_IOU = UTILITY_TYPE() == 'I'
      DO I = 1, nunits
         AI_TAX_DEP = 0.
         AI_BOOK_DEP = 0.
         AI_TAX_PREFERRENCE = 0.
         AI_DEFERRED_TAXES = 0.
         ANNUAL_AFUDC = 0.
         AI_INVESTMENT_CLASS = 0.
         AI_EXPENSE_CLASS = 0.
         AI_INVESTMENT = INVESTMENT(I)/1000000.
         IF(AI_REMAINING_LIFE(I) > 0) THEN
            REMAINING_LIFE = FLOAT(AI_REMAINING_LIFE(I))
            IF(ON_LINE(I)/100 <= DATE .AND. OFF_LINE(I)/100 >= DATE)
     +           AI_REMAINING_LIFE(I) = MAX(AI_REMAINING_LIFE(I) - 1,2)
         ELSEIF(AI_REMAINING_LIFE(I) < 0) THEN
! ADDED FOR SCOTT JONES. GAT. 1/3/95.
            IF(ON_LINE(I)/100 <= DATE .AND. OFF_LINE(I)/100>=DATE) THEN
               POINTER_VALUE = MIN(DATE - MAX(ON_LINE(I)/100-1,
     +                      get_BASE_YEAR() - 1900),AVAIL_DATA_YEARS)
               REMAINING_LIFE = GET_VAR(FLOAT(AI_REMAINING_LIFE(I)),
     +                                         POINTER_VALUE,"A&I CALC")
               IF(REMAINING_LIFE <= 0. .OR. REMAINING_LIFE > 100.) THEN
                  WRITE(4,*) "Remaining Life for Unit ",I,
     +         " in year ",YEAR+get_BASE_YEAR()," is ",REMAINING_LIFE
                  WRITE(4,*) "This is outside of the valid range "//
     +                                              "for this variable."
                  WRITE(4,*) '*** line 601 FINANBSE.FOR ***'
                  er_message='See WARNING MESSAGES -finanbse.for-1'
                  call end_program(er_message)
               ENDIF
            ELSE
               REMAINING_LIFE = 0.
            ENDIF
         ELSE
            REMAINING_LIFE = 0.
         ENDIF
         IF(AI_INVESTMENT /= 0.) THEN
            IF(REMAINING_LIFE >= 1.) THEN
               AI_INVESTMENT_CLASS = AI_INVESTMENT
               AI_CASH =
     + AI_CASH + AI_INVESTMENT
               IF(UTILITY_IS_TVA) THEN

! TVA ASSUMES ALL A&I CASH IS PUT INTO CWIP AT MID-YEAR

                  IF(YEAR == 1)THEN
                     ANNUAL_AFUDC = AI_INVESTMENT * AFUDC_RATE
                  ELSE
                     ANNUAL_AFUDC = (AI_INVESTMENT +
     +                          LAST_YEAR_INVESTMENT(I)) * AFUDC_RATE/2.
                  ENDIF
               ELSE
                  ANNUAL_AFUDC = AI_INVESTMENT * AFUDC_RATE/2.
               ENDIF

               AI_AFUDC1 = AI_AFUDC1 + ANNUAL_AFUDC

               AI_BOOK_VALUE = AI_INVESTMENT + ANNUAL_AFUDC
               ANNUAL_DEP = MIN(AI_BOOK_VALUE/REMAINING_LIFE,
     +                                                    AI_BOOK_VALUE)
               DO J = START_DEP_YEAR, MAX_FINANCIAL_SIMULATION_YEARS
                  IF(AI_BOOK_VALUE <= ANNUAL_DEP) THEN
                     AI_BOOK_DEP(J) = AI_BOOK_DEP(J) + AI_BOOK_VALUE
                     EXIT
                  ENDIF
                  AI_BOOK_DEP(J) = AI_BOOK_DEP(J) + ANNUAL_DEP
                  AI_BOOK_VALUE = AI_BOOK_VALUE - ANNUAL_DEP
               ENDDO
            ELSE
               AI_EXPENSE =
     + AI_EXPENSE + AI_INVESTMENT
               AI_EXPENSE_CLASS = AI_INVESTMENT
            ENDIF

            IF(UTILITY_IS_IOU) THEN
               AI_TAX_VALUE = AI_INVESTMENT + ANNUAL_AFUDC *
     +                                     INTEREST_CAPITIALIZATION_RATE
               CALL AI_TAX_CALCULATIONS(START_DEP_YEAR,
     +                                  AI_TAX_VALUE,
     +                                  REMAINING_LIFE,
     +                                  AI_TAX_LIFE(I),
     +                                  AI_ADR_TAX_LIFE(I),
     +                                  AI_TAX_DEP,
     +                                  AI_TAX_PREFERRENCE,
     +                                  AI_DEFERRED_TAXES,
     +                                  MAX_FINANCIAL_SIMULATION_YEARS)
            ELSE
               AI_TAX_VALUE = 0.
            ENDIF

! ADD ANNUAL VALUES TO TOTAL VALUES

            DO YR = START_DEP_YEAR, MAX_FINANCIAL_SIMULATION_YEARS
               AI_ANNUAL_DEP(YR) = AI_ANNUAL_DEP(YR) + AI_BOOK_DEP(YR)
               AI_ANNUAL_TAX_DEP(YR) = AI_ANNUAL_TAX_DEP(YR) +
     +                                                    AI_TAX_DEP(YR)
               AI_ANNUAL_DEFERRED_TAX(YR) = AI_ANNUAL_DEFERRED_TAX(YR) +
     +                                      AI_DEFERRED_TAXES(YR)
               AI_ANNUAL_AMT_PREFERENCE_ITEMS(YR) =
     +                                AI_TAX_PREFERRENCE(YR) +
     +                                AI_ANNUAL_AMT_PREFERENCE_ITEMS(YR)
            ENDDO

! CLASS A&I VALUES

            ASSET_CLASS = ASSET_CLASS_NUM(I)
            ASSET_ALLOCATION_VECTOR = ASSET_CLASS_VECTOR(I)

            IF(ASSET_CLASS < 0) THEN
               CALL GET_ASSET_VAR(ABS(ASSET_CLASS),
     +                                 DUMMY_TYPE,ASSET_CLASS_LIST)
               CALL GET_ASSET_VAR(ABS(ASSET_ALLOCATION_VECTOR),
     +                                 DUMMY_TYPE,ASSET_ALLOCATION_LIST)
            ELSE
               ASSET_CLASS_LIST(1) = ASSET_CLASS
               ASSET_CLASS_LIST(2) = 0.
               ASSET_ALLOCATION_LIST(1) = 100.
               ASSET_ALLOCATION_LIST(2) = 0.
            ENDIF
            CLASS_POINTER = 1
            DO
               ASSET_CLASS = ASSET_CLASS_LIST(CLASS_POINTER)
               CALL CHECK_IF_CLASS_DEFINED(ASSET_CLASS)
               ASSET_CLASS = ASSET_CLASS + 1
               IF(ASSET_CLASS <= 0 .OR.
     +                               ASSET_CLASS > MAX_AI_CLASSES) CYCLE
               ASSET_ALLOCATOR=ASSET_ALLOCATION_LIST(CLASS_POINTER)/100.

               AI_CLASS_CASH(ASSET_CLASS) =
     +                           AI_INVESTMENT_CLASS * ASSET_ALLOCATOR +
     +                                        AI_CLASS_CASH(ASSET_CLASS)
               AI_CLASS_EXPENSE(ASSET_CLASS) =
     +                              AI_EXPENSE_CLASS * ASSET_ALLOCATOR +
     +                                     AI_CLASS_EXPENSE(ASSET_CLASS)
               AI_CLASS_AFUDC1(ASSET_CLASS) = ANNUAL_AFUDC *
     +                                                 ASSET_ALLOCATOR +
     +                                      AI_CLASS_AFUDC1(ASSET_CLASS)

               DO YR = START_DEP_YEAR, MAX_FINANCIAL_SIMULATION_YEARS
                  AI_ANNUAL_CLASS_DEP(YR,ASSET_CLASS) =
     +                             AI_ANNUAL_CLASS_DEP(YR,ASSET_CLASS) +
     +                                 AI_BOOK_DEP(YR) * ASSET_ALLOCATOR
                  AI_ANNUAL_CLASS_TAX_DEP(YR,ASSET_CLASS) =
     +                         AI_ANNUAL_CLASS_TAX_DEP(YR,ASSET_CLASS) +
     +                                  AI_TAX_DEP(YR) * ASSET_ALLOCATOR
                  AI_ANNUAL_CLASS_DEFERRED_TAX(YR,ASSET_CLASS) =
     +                    AI_ANNUAL_CLASS_DEFERRED_TAX(YR,ASSET_CLASS) +
     +                           AI_DEFERRED_TAXES(YR) * ASSET_ALLOCATOR
                  AI_ANNUAL_CLASS_AMT_PREF_ITEMS(YR,ASSET_CLASS) =
     +                  AI_ANNUAL_CLASS_AMT_PREF_ITEMS(YR,ASSET_CLASS) +
     +                          AI_TAX_PREFERRENCE(YR) * ASSET_ALLOCATOR
               ENDDO

               CLASS_POINTER = CLASS_POINTER + 1
               IF(CLASS_POINTER > AVAIL_DATA_YEARS) EXIT
               IF(ASSET_CLASS_LIST(CLASS_POINTER) == 0. .OR.
     +                      ASSET_CLASS_LIST(CLASS_POINTER) == -99.)EXIT
            ENDDO ! ASSET CLASSES

         ENDIF
         LAST_YEAR_INVESTMENT(I) = AI_INVESTMENT
      ENDDO
      DEALLOCATE(AI_TAX_DEP,
     +           AI_BOOK_DEP,
     +           AI_TAX_PREFERRENCE,
     +           AI_DEFERRED_TAXES)
      RETURN
      END subroutine 


!*                                                                     *
!*                          D E P T A X                                *
!*                                                                     *
!*          COPYRIGHT (C) 1995 M.S. GERBER & ASSOCIATES, INC.          *
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

      SUBROUTINE AI_TAX_CALCULATIONS(START_DEP_YEAR,
     +                               TAX_VALUE,
     +                               REMAINING_LIFE,
     +                               TAX_LIFE,
     +                               ADR_TAX_LIFE,
     +                               AI_TAX_DEP,
     +                               AI_TAX_PREFERRENCE,
     +                               AI_DEFERRED_TAXES,
     +                               FINANCIAL_SIMULATION_YEARS)

!     INTERNAL VARIABLES
      use SpinDriftLib
      use prod_arrays_dimensions
      implicit none
      
      LOGICAL (kind=1) ::  NO_SWITCH
      INTEGER (kind=2) ::  L,M,START_DEP_YEAR
      INTEGER (kind=2) ::  FINANCIAL_SIMULATION_YEARS
      REAL (kind=4) ::  TAX_LIFE
      REAL (kind=4) ::  DB_RATE,TOTAL_TAX_DEP,DEPDB,DEPSL,ADR_TAX_LIFE
      REAL (kind=4) ::  SL_OFFSET
      REAL (kind=4) ::  NPV_4_TAX,NPV_4_BOOK,REMAINING_LIFE
      INTEGER (kind=4) ::  VALUES_2_ZERO
!
!     EXTERNAL VARIABLES
!
      CHARACTER (len=4) ::  DEPMET
      REAL (kind=4) ::  AI_TAX_DEP(FINANCIAL_SIMULATION_YEARS),
     +     AI_TAX_PREFERRENCE(FINANCIAL_SIMULATION_YEARS),
     +     AI_DEFERRED_TAXES(FINANCIAL_SIMULATION_YEARS),
     +     TAX_VALUE,TAX_DEP_SL,TOTAL_BOOK_DEP

      AI_TAX_DEP = 0.
      AI_TAX_PREFERRENCE = 0.
      AI_DEFERRED_TAXES = 0.

!     IF THE TAX LIFE IS GREATER THAN 98 YEARS, IT IS ASSUMED
!     THAT NO TAX DEPRECIATION WILL OCCUR

      IF(TAX_VALUE <= 0.) RETURN

      SL_OFFSET = 0.
        DEPMET = 'MACR'
        IF(TAX_LIFE <= 1.0) THEN
         AI_TAX_DEP(START_DEP_YEAR) = TAX_VALUE
       ELSEIF(TAX_LIFE < 99.) THEN

! TAX DEPRECIATION


         DB_RATE = 1.5
         IF(TAX_LIFE <= 20.) DB_RATE = 2.
         SL_OFFSET = .5
         NO_SWITCH = INDEX(DEPMET,'DBNS') /= 0

         NPV_4_TAX = TAX_VALUE/2.
         TOTAL_TAX_DEP = 0.
         L = START_DEP_YEAR
         DOWHILE (L <= FINANCIAL_SIMULATION_YEARS .AND. NPV_4_TAX > 0.)
            DEPDB = DB_RATE * NPV_4_TAX/TAX_LIFE
            DEPSL=NPV_4_TAX/(TAX_LIFE-FLOAT(L-START_DEP_YEAR)+SL_OFFSET)
            IF(DEPDB > DEPSL .OR. NO_SWITCH) THEN
! THEN TAKE THE DB VALUE FOR THE EXPENSE
               AI_TAX_DEP(L) = AI_TAX_DEP(L) + DEPDB
               TOTAL_TAX_DEP =  TOTAL_TAX_DEP + DEPDB
               NPV_4_TAX = TAX_VALUE - TOTAL_TAX_DEP

! ELSE TAKE THE STRAIGHT LINE UNTIL NPV IS ZERO

            ELSE
               DO M = L, FINANCIAL_SIMULATION_YEARS
                  IF(NPV_4_TAX < DEPSL) THEN
                     AI_TAX_DEP(M) = AI_TAX_DEP(M) + NPV_4_TAX
                     NPV_4_TAX = 0.
                     EXIT
                  ENDIF
                  AI_TAX_DEP(M) = AI_TAX_DEP(M) + DEPSL
                  NPV_4_TAX = NPV_4_TAX - DEPSL
                  IF(NPV_4_TAX < .0001) NPV_4_TAX = 0.
               ENDDO
               EXIT
            ENDIF
           L = L + 1
         ENDDO
      ENDIF

! DEFERRED TAX CALCULATION

      IF(REMAINING_LIFE > 0. .AND. REMAINING_LIFE < 99.) THEN
         TAX_DEP_SL = MIN(TAX_VALUE/REMAINING_LIFE,TAX_VALUE)
         AI_DEFERRED_TAXES(START_DEP_YEAR) = AI_TAX_DEP(START_DEP_YEAR)-
     +                                                     TAX_DEP_SL/2.
         TOTAL_BOOK_DEP = TAX_DEP_SL/2.
         NPV_4_BOOK = TAX_VALUE - TOTAL_BOOK_DEP
         L = START_DEP_YEAR + 1

         DOWHILE (L <= FINANCIAL_SIMULATION_YEARS .AND. NPV_4_BOOK > 0.)
            TAX_DEP_SL = MIN(TAX_DEP_SL,NPV_4_BOOK)
            AI_DEFERRED_TAXES(L) = AI_TAX_DEP(L) - TAX_DEP_SL
            TOTAL_BOOK_DEP = TOTAL_BOOK_DEP + TAX_DEP_SL
            NPV_4_BOOK = MAX(TAX_VALUE - TOTAL_BOOK_DEP,0.)
            IF(NPV_4_BOOK < .0001) NPV_4_BOOK = 0.
            L = L + 1
         ENDDO
      ENDIF

! AMT PREFERENCE CALCULATION

      IF(ADR_TAX_LIFE <= 1.0 .OR. ADR_TAX_LIFE > 98.) THEN
         IF(ADR_TAX_LIFE <= 1.0) THEN
            AI_TAX_PREFERRENCE(START_DEP_YEAR) =
     +                            AI_TAX_DEP(START_DEP_YEAR) - TAX_VALUE
         ELSE
            AI_TAX_PREFERRENCE(START_DEP_YEAR) =
     +                                        AI_TAX_DEP(START_DEP_YEAR)
         ENDIF
         L = START_DEP_YEAR + 1
         DO L = START_DEP_YEAR+1, FINANCIAL_SIMULATION_YEARS
            AI_TAX_PREFERRENCE(L) = AI_TAX_DEP(L)
         ENDDO
      ELSE
         DB_RATE = 1.5
         IF(INDEX(DEPMET,'MACR') /= 0) SL_OFFSET = .5
         NO_SWITCH = INDEX(DEPMET,'DBNS') /= 0

         NPV_4_TAX = TAX_VALUE/2.
         TOTAL_TAX_DEP = 0.
         L = START_DEP_YEAR
         DOWHILE (L <= FINANCIAL_SIMULATION_YEARS .AND. NPV_4_TAX > 0.)
            DEPDB = DB_RATE * NPV_4_TAX/ADR_TAX_LIFE
            DEPSL = NPV_4_TAX/
     +                  (ADR_TAX_LIFE-FLOAT(L-START_DEP_YEAR)+SL_OFFSET)
            IF(DEPDB > DEPSL .OR. NO_SWITCH) THEN

! THEN TAKE THE DB VALUE FOR THE EXPENSE
               AI_TAX_PREFERRENCE(L) = AI_TAX_DEP(L) - DEPDB
               TOTAL_TAX_DEP =  TOTAL_TAX_DEP + DEPDB
               NPV_4_TAX = TAX_VALUE - TOTAL_TAX_DEP

! ELSE TAKE THE STRAIGHT LINE UNTIL NPV IS ZERO

            ELSE
               DO M = L, FINANCIAL_SIMULATION_YEARS
                  IF(NPV_4_TAX < DEPSL) THEN
                     AI_TAX_PREFERRENCE(M) = AI_TAX_DEP(M) - NPV_4_TAX
                     NPV_4_TAX = 0.
                     EXIT
                  ENDIF
                  AI_TAX_PREFERRENCE(M) = AI_TAX_DEP(M) - DEPSL
                  NPV_4_TAX = NPV_4_TAX - DEPSL
                  IF(NPV_4_TAX < .0001) NPV_4_TAX = 0.
               ENDDO
               EXIT
            ENDIF
            L = L + 1
         ENDDO
      ENDIF
      RETURN
      END

!
!             THE ADDITIONS & IMPROVMENTS ANALYSIS MODULE FOR TVA
!                         EXTERNAL PRODUCTION COSTS
!                             COPYRIGHT (C) 1994
!                        M.S. GERBER & ASSOCIATES, INC.
!                              ALL RIGHTS RESERVED
!

!

      SUBROUTINE EXTERNAL_AI
      use aitvafin
      use end_routine, only: end_program, er_message
      use foshydi2com
      use SpinDriftLib
      use prod_arrays_dimensions
      USE SIZECOM
      use prod2com
      use endpoint
      use globecom
      use mod_base_year
      use nunits_shared
      implicit none
      
      CHARACTER (len=1) ::  UTILITY_TYPE
      CHARACTER (len=4) ::  FILE_EXTENSION
      CHARACTER (len=5) ::  SCENARIO_NAME,GET_SCENAME_AND_EXT_AI_NUM
      CHARACTER (len=64) ::  FILE_NAME,BASE_FILE_DIRECTORY
      CHARACTER (len=1024) ::  RECLN
      INTEGER (kind=2) ::  EXTERNAL_AI_NUM
      INTEGER (kind=4) ::  IOS
      LOGICAL (kind=4) ::  FILE_EXISTS
!
      INTEGER (kind=2) ::  OTHER_YEAR
      INTEGER (kind=2) ::  OTHER_ID_NUMBER,MAX_CL_ID_NUM,MAX_EL_ID_NUM,


     +         TEMP_NUNITS,TEMP_HYDRO_UNITS
      CHARACTER (len=1) ::  OTHER_UNIT_TYPE
      REAL (kind=4) ::  OTHER_ANNUAL_GWH,
     +     OTHER_AI_ENERGY_RATE,
     +     OTHER_ANNUAL_MW_MONTHS,
     +     OTHER_AI_CAPACITY_RATE,
     +     OTHER_REMAINING_LIFE

      SAVE EXTERNAL_AI_NUM,
     +         MAX_CL_ID_NUM,
     +         MAX_EL_ID_NUM,
     +         TEMP_NUNITS,
     +         TEMP_HYDRO_UNITS,
     +         OTHER_YEAR,
     +         OTHER_UNIT_TYPE,
     +         OTHER_ID_NUMBER,
     +         OTHER_ANNUAL_GWH,
     +         OTHER_AI_ENERGY_RATE,
     +         OTHER_ANNUAL_MW_MONTHS,
     +         OTHER_AI_CAPACITY_RATE,
     +         OTHER_REMAINING_LIFE

! END OF DECLARATIONS
! GET THE FILE NAME
!
      ENTRY OPEN_EXTERNAL_AI_FILE
         IF(gc_end_point < 10 .AND. gc_end_point > 0) THEN
            FILE_EXTENSION = ".00"//CHAR(gc_end_point+48)
         ELSEIF(gc_end_point < 100) THEN
            FILE_EXTENSION = ".0"//CHAR(gc_end_point+48)
         ELSEIF(gc_end_point < 1000) THEN
            FILE_EXTENSION = "."//CHAR(gc_end_point+48)
         ELSE
            WRITE(4,*) "Endpoint Number Too High For External A&I"
            WRITE(4,*) "Production Data File.  Endpoint =",gc_end_point
            WRITE(4,*) '*** line 988NANBSE.FOR ***'
            er_message='See WARNING MESSAGES -finanbse.for-2'
            call end_program(er_message)
         ENDIF
         SCENARIO_NAME =
     +             GET_SCENAME_AND_EXT_AI_NUM(EXTERNAL_AI_NUM)
         FILE_NAME = trim(BASE_FILE_DIRECTORY())//
     +                      "A&I"//trim(SCENARIO_NAME)//FILE_EXTENSION
         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
         IF(FILE_EXISTS) THEN
            OPEN(EXTERNAL_AI_NUM,FILE=FILE_NAME)
         ELSEIF(UTILITY_TYPE() == 'T') THEN
            WRITE(4,*) "External A&I Production Data File",FILE_NAME
            WRITE(4,*) "Not In Current Project Directory"
            WRITE(4,*) '*** line 1000 FINANBSE.FOR ***'
            er_message='See WARNING MESSAGES -finanbse.for-3'
            call end_program(er_message)
         ENDIF
      RETURN

      ENTRY EXTERNAL_AI_ANALYSIS

         MAX_CL_ID_NUM = 0
         MAX_EL_ID_NUM = 0
         TEMP_NUNITS = get_nunits()
         TEMP_HYDRO_UNITS = HYDRO_UNITS

         IF(globecom_year==1) THEN

            AI_CL_REMAINING_LIFE = 0
            AI_EL_REMAINING_LIFE = 0

            READ(EXTERNAL_AI_NUM,1000,IOSTAT=IOS) RECLN
            IF(IOS /=0) THEN
               WRITE(4,*)"Error Reading External A&I Production File"
             WRITE(4,*) "At First Record In Year=",
     +     get_BASE_YEAR()+globecom_YEAR
               WRITE(4,*) '*** line 1021 FINANBSE.FOR ***'
               er_message='See WARNING MESSAGES -finanbse.for-4'
               call end_program(er_message)
            ENDIF
            RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
            READ(RECLN,*,ERR=200)
     +         OTHER_YEAR,
     +         OTHER_UNIT_TYPE,
     +         OTHER_ID_NUMBER,
     +         OTHER_ANNUAL_GWH,
     +         OTHER_AI_ENERGY_RATE,
     +         OTHER_ANNUAL_MW_MONTHS,
     +         OTHER_AI_CAPACITY_RATE,
     +         OTHER_REMAINING_LIFE
         ENDIF

         DO
            IF(OTHER_YEAR == get_BASE_YEAR()+globecom_YEAR) THEN
               IF(OTHER_UNIT_TYPE == 'C') THEN
                  CL_AI_INVESTMENT(OTHER_ID_NUMBER) =
     +                               CL_AI_INVESTMENT(OTHER_ID_NUMBER) +
     +                         OTHER_AI_ENERGY_RATE * OTHER_ANNUAL_GWH +
     +                         1000. * OTHER_ANNUAL_MW_MONTHS *
     +                                            OTHER_AI_CAPACITY_RATE
                  AI_CL_REMAINING_LIFE(OTHER_ID_NUMBER) =
     +                                        NINT(OTHER_REMAINING_LIFE)
                  MAX_CL_ID_NUM = MAX(MAX_CL_ID_NUM,OTHER_ID_NUMBER)
               ELSE
                  EL_AI_INVESTMENT(OTHER_ID_NUMBER) =
     +                               EL_AI_INVESTMENT(OTHER_ID_NUMBER) +
     +                         OTHER_AI_ENERGY_RATE * OTHER_ANNUAL_GWH +
     +                         1000. * OTHER_ANNUAL_MW_MONTHS *
     +                                            OTHER_AI_CAPACITY_RATE
                  AI_EL_REMAINING_LIFE(OTHER_ID_NUMBER) =
     +                                        NINT(OTHER_REMAINING_LIFE)
                  MAX_EL_ID_NUM = MAX(MAX_EL_ID_NUM,OTHER_ID_NUMBER)
               ENDIF
            ELSE
               EXIT
            ENDIF

            READ(EXTERNAL_AI_NUM,1000,IOSTAT=IOS) RECLN
            IF(IOS /=0) THEN
               IF(get_BASE_YEAR()+globecom_YEAR == gc_last_study_year
     +                                     .AND. MAX_CL_ID_NUM > 0) THEN
                  EXIT
               ELSE
          WRITE(4,*)"Error Reading External A&I Production File"
          WRITE(4,*) "Record In Year=",get_BASE_YEAR()+globecom_YEAR
          WRITE(4,*) '*** line 1070 FINANBSE.FOR ***'
          er_message='See WARNING MESSAGES -finanbse.for-5'
          call end_program(er_message)
               ENDIF
            ENDIF
            RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
            READ(RECLN,*,ERR=200)
     +         OTHER_YEAR,
     +         OTHER_UNIT_TYPE,
     +         OTHER_ID_NUMBER,
     +         OTHER_ANNUAL_GWH,
     +         OTHER_AI_ENERGY_RATE,
     +         OTHER_ANNUAL_MW_MONTHS,
     +         OTHER_AI_CAPACITY_RATE,
     +         OTHER_REMAINING_LIFE

         ENDDO
      RETURN
      ENTRY RESET_UNIT_COUNT_TO_PSYM
         call set_nunits(MAX_CL_ID_NUM)
         HYDRO_UNITS = MAX_EL_ID_NUM
      RETURN
      ENTRY RESET_UNIT_COUNT_TO_MIDAS
         call set_nunits(TEMP_NUNITS)
         HYDRO_UNITS = TEMP_HYDRO_UNITS
      RETURN

  200 WRITE(4,*) "Cannot Read External A&I Production File"
      WRITE(4,*) '*** line 1099 FINANBSE.FOR ***'
      er_message='See WARNING MESSAGES -finanbse.for-6'
      call end_program(er_message)
 1000 FORMAT(A)
      END

!                             DEFERRED EXPENSES
!                             COPYRIGHT (C) 1992
!                        M.S. GERBER & ASSOCIATES, INC.
!                              ALL RIGHTS RESERVED
!

!
      SUBROUTINE DEFERRED_EXPENSES(RUN_YEAR,DEFERRED_EXPENSE_AMORT,
     +                             DEFERRED_EXPENSE_CASH,
     +                             DEFERRED_EXPENSE_AMORT_PERIOD)
!
      use SpinDriftLib
      use prod_arrays_dimensions
      use kepcocom
      USE SIZECOM

      INTEGER (kind=2) ::  RUN_YEAR,I
      REAL (kind=4) ::  DEFERRED_EXPENSE_AMORT_PERIOD,


     +     DEFERRED_EXPENSE_CASH,
     +     DEFERRED_EXPENSE_AMORT,
     +     AMORT_PERIOD,
     +     CURRENT_AMORT
      REAL (kind=4) ::  ANNUAL_AMORT,UNAMORT_DEFERRED_EXPENSE
      REAL (kind=4) ::
     +   ANNUAL_AMORTIZATION(MAX_FINANCIAL_SIMULATION_YEARS)
      SAVE ANNUAL_AMORTIZATION

      IF(RUN_YEAR == 1) ANNUAL_AMORTIZATION = 0.
      AMORT_PERIOD = DEFERRED_EXPENSE_AMORT_PERIOD
      IF(AMORT_PERIOD <= .5) THEN
         ANNUAL_AMORTIZATION(RUN_YEAR) = ANNUAL_AMORTIZATION(RUN_YEAR) +
     +                                             DEFERRED_EXPENSE_CASH
      ELSE IF(AMORT_PERIOD <= 1.) THEN
         ANNUAL_AMORTIZATION(RUN_YEAR) = ANNUAL_AMORTIZATION(RUN_YEAR) +
     +                                          DEFERRED_EXPENSE_CASH/2.
         ANNUAL_AMORTIZATION(RUN_YEAR+1) = DEFERRED_EXPENSE_CASH/2. +
     +                                   ANNUAL_AMORTIZATION(RUN_YEAR+1)
      ELSE IF(AMORT_PERIOD < 98.) THEN
         ANNUAL_AMORT = DEFERRED_EXPENSE_CASH/AMORT_PERIOD
         UNAMORT_DEFERRED_EXPENSE=DEFERRED_EXPENSE_CASH-ANNUAL_AMORT/2.
         ANNUAL_AMORTIZATION(RUN_YEAR) = ANNUAL_AMORTIZATION(RUN_YEAR) +
     +                                                   ANNUAL_AMORT/2.
         I = RUN_YEAR + 1
         DOWHILE (UNAMORT_DEFERRED_EXPENSE > 0. .AND.
     +                              I <= MAX_FINANCIAL_SIMULATION_YEARS)
            CURRENT_AMORT = MIN(UNAMORT_DEFERRED_EXPENSE,ANNUAL_AMORT)
            UNAMORT_DEFERRED_EXPENSE = UNAMORT_DEFERRED_EXPENSE -
     +                                                     CURRENT_AMORT
            ANNUAL_AMORTIZATION(I) = ANNUAL_AMORTIZATION(I) +
     +                                                     CURRENT_AMORT
            I = I + 1
         ENDDO
      ENDIF
      DEFERRED_EXPENSE_CASH = DEFERRED_EXPENSE_CASH +
     +                                      KEPCO_WC_DEF_MAINT_ENRG_COST

!
! 12/20/92 THE FOLLOWING LOGIC IS FOR KEPCO IT ASSUMES THAT SOME KEPCO
! DEFERRED WOLF CREEK MAINTENANCE ENERGY EXPENSE FOR THE FIRST TWO YEARS
! IS FROM THE FINANCIAL PARAMETER FILE WHERE IT IS READ IN AS
! DEFERRED_EXPENSE_AMORT
!

      KEPCO_WC_DEF_MAINT_ENRG_AMORT = DEFERRED_EXPENSE_AMORT +
     +                                     KEPCO_WC_DEF_MAINT_ENRG_AMORT
      DEFERRED_EXPENSE_AMORT = ANNUAL_AMORTIZATION(RUN_YEAR) +
     +                                     KEPCO_WC_DEF_MAINT_ENRG_AMORT
      RETURN
      END


      SUBROUTINE EXTERNAL_PRODUCTION_RESULTS
      use mod_base_year
      use end_routine, only: end_program, er_message

      use SpinDriftLib
      use prod_arrays_dimensions
      USE SIZECOM
      use globecom
      implicit none
         REAL (kind=4) ::

     +         OTHER_YEAR,
     +         OTHER_FOSSIL_CAPACITY,
     +         OTHER_FOSSIL_GENERATION,
     +         OTHER_FOSSIL_INPUT,
     +         OTHER_FOSSIL_FUEL,
     +         OTHER_FOSSIL_VAROM,
     +         OTHER_FOSSIL_FIXOM,
     +         OTHER_NUC_CAPACITY,
     +         OTHER_NUC_GENERATION,
     +         OTHER_NUC_INPUT,
     +         OTHER_NUC_OWNED,
     +         OTHER_NUC_LEASED,
     +         OTHER_NUC_VAROM,
     +         OTHER_NUC_FIXOM,
     +         OTHER_HYDRO_CAPACITY,
     +         OTHER_HYDRO_GENERATION,
     +         OTHER_HYDRO_VAROM,
     +         OTHER_HYDRO_FIXOM,
     +         OTHER_STORAGE_PUMP,
     +         OTHER_STORAGE_GENERATION,
     +         OTHER_STORAGE_VAROM,
     +         OTHER_STORAGE_FIXOM,
     +         OTHER_PURCHASE_GENERATION,
     +         OTHER_PURCHASE_COST,
     +         OTHER_ECONOMY_PURCHASE,
     +         OTHER_ECONOMY_SALES,
     +         OTHER_ECONOMY_COST,
     +         OTHER_ECONOMY_REVENUE,
     +         OTHER_SECONDARY_SALES,
     +         OTHER_SECONDARY_SALES_REVENUES,
     +         OTHER_DSM_ENERGY_MODIFICATION,
     +         OTHER_DSM_PEAK_MODIFICATION,
     +         OTHER_DSM_EXPENSE_WO_REBATES,
     +         OTHER_DSM_REBATE_EXPENSE,
     +         OTHER_T_AND_D_EXPENSE,
     +         OTHER_EMISSIONS(TOTAL_EMISSION_GROUPS),
     +         OTHER_PEAK_BEFORE_DSM,
     +         OTHER_ANNUAL_ENERGY,

     +         MIDAS_FUEL_COST,
     +         MIDAS_VAROM,
     +         MIDAS_FIXOM,
     +         MIDAS_NUC_OWNED,
     +         MIDAS_NUC_LEASED,
     +         MIDAS_PURCHASE_COST,
     +         MIDAS_SALES_REVENUES,
     +         MIDAS_DSM_EXPENSE,
     +         MIDAS_DSM_REBATES,
     +         MIDAS_OTHER3_EXPENSE

         SAVE
     +         OTHER_YEAR,
     +         OTHER_FOSSIL_CAPACITY,
     +         OTHER_FOSSIL_GENERATION,
     +         OTHER_FOSSIL_INPUT,
     +         OTHER_FOSSIL_FUEL,
     +         OTHER_FOSSIL_VAROM,
     +         OTHER_FOSSIL_FIXOM,
     +         OTHER_NUC_CAPACITY,
     +         OTHER_NUC_GENERATION,
     +         OTHER_NUC_INPUT,
     +         OTHER_NUC_OWNED,
     +         OTHER_NUC_LEASED,
     +         OTHER_NUC_VAROM,
     +         OTHER_NUC_FIXOM,
     +         OTHER_HYDRO_CAPACITY,
     +         OTHER_HYDRO_GENERATION,
     +         OTHER_HYDRO_VAROM,
     +         OTHER_HYDRO_FIXOM,
     +         OTHER_STORAGE_PUMP,
     +         OTHER_STORAGE_GENERATION,
     +         OTHER_STORAGE_VAROM,
     +         OTHER_STORAGE_FIXOM,
     +         OTHER_PURCHASE_GENERATION,
     +         OTHER_PURCHASE_COST,
     +         OTHER_ECONOMY_PURCHASE,
     +         OTHER_ECONOMY_SALES,
     +         OTHER_ECONOMY_COST,
     +         OTHER_ECONOMY_REVENUE,
     +         OTHER_SECONDARY_SALES,
     +         OTHER_SECONDARY_SALES_REVENUES,
     +         OTHER_DSM_ENERGY_MODIFICATION,
     +         OTHER_DSM_PEAK_MODIFICATION,
     +         OTHER_DSM_EXPENSE_WO_REBATES,
     +         OTHER_DSM_REBATE_EXPENSE,
     +         OTHER_T_AND_D_EXPENSE,
     +         OTHER_EMISSIONS,
     +         OTHER_PEAK_BEFORE_DSM,
     +         OTHER_ANNUAL_ENERGY

      CHARACTER (len=4) ::  FILE_EXTENSION
      CHARACTER (len=5) ::  SCENARIO_NAME,GET_SCENAME_AND_EXT_PROD_NUM
      CHARACTER (len=64) ::  FILE_NAME,BASE_FILE_DIRECTORY
      CHARACTER (len=1024) ::  RECLN
      INTEGER (kind=2) ::  EXTERNAL_PRODUCTION_NUM,YR
      INTEGER (kind=4) ::  IOS
      LOGICAL (kind=4) ::  FILE_EXISTS
      SAVE EXTERNAL_PRODUCTION_NUM

! END OF DECLARATIONS
! GET THE FILE NAME

      ENTRY OPEN_EXTERNAL_PRODUCTION_FILE
         IF(gc_end_point < 10 .AND. gc_end_point > 0) THEN
            FILE_EXTENSION = ".00"//CHAR(gc_end_point+48)
         ELSEIF(gc_end_point < 100) THEN
            FILE_EXTENSION = ".0"//CHAR(gc_end_point+48)
         ELSEIF(gc_end_point < 1000) THEN
            FILE_EXTENSION = "."//CHAR(gc_end_point+48)
         ELSE
            WRITE(4,*) "Endpoint Number Too High For External"
            WRITE(4,*) "Production Data File.  Endpoint =",gc_end_point
            WRITE(4,*) '*** line 1286 FINANBSE.FOR ***'
            er_message='See WARNING MESSAGES -finanbse.for-7'
            call end_program(er_message)
         ENDIF
         SCENARIO_NAME =
     +             GET_SCENAME_AND_EXT_PROD_NUM(EXTERNAL_PRODUCTION_NUM)
         FILE_NAME = trim(BASE_FILE_DIRECTORY())//
     +                      "ANN"//trim(SCENARIO_NAME)//FILE_EXTENSION
         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
         IF(FILE_EXISTS) THEN
            OPEN(EXTERNAL_PRODUCTION_NUM,FILE=FILE_NAME)
         ELSE
            WRITE(4,*) "External Production Data File",FILE_NAME
            WRITE(4,*) "Not In Current Project Directory"
            WRITE(4,*) '*** line 1299 FINANBSE.FOR ***'
            er_message='See WARNING MESSAGES -finanbse.for-8'
            call end_program(er_message)
         ENDIF
      RETURN

      ENTRY UPDATE_EXTERNAL_PRODUCTION(YR,
     +                                 MIDAS_FUEL_COST,
     +                                 MIDAS_VAROM,
     +                                 MIDAS_FIXOM,
     +                                 MIDAS_NUC_OWNED,
     +                                 MIDAS_NUC_LEASED,
     +                                 MIDAS_PURCHASE_COST,
     +                                 MIDAS_SALES_REVENUES,
     +                                 MIDAS_DSM_EXPENSE,
     +                                 MIDAS_DSM_REBATES,
     +                                 MIDAS_OTHER3_EXPENSE)
         READ(EXTERNAL_PRODUCTION_NUM,1000,IOSTAT=IOS) RECLN
         IF(IOS /=0) THEN
            WRITE(4,*) "Error Reading External Production File"
            WRITE(4,*) "Record In Year=",YR
            WRITE(4,*) '*** line 1219 FINANBSE.FOR ***'
            er_message='See WARNING MESSAGES -finanbse.for-9'
            call end_program(er_message)
         ENDIF
         RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
         READ(RECLN,*,ERR=200)
     +         OTHER_YEAR,
     +         OTHER_FOSSIL_CAPACITY,
     +         OTHER_FOSSIL_GENERATION,
     +         OTHER_FOSSIL_INPUT,
     +         OTHER_FOSSIL_FUEL,
     +         OTHER_FOSSIL_VAROM,
     +         OTHER_FOSSIL_FIXOM,
     +         OTHER_NUC_CAPACITY,
     +         OTHER_NUC_GENERATION,
     +         OTHER_NUC_INPUT,
     +         OTHER_NUC_OWNED,
     +         OTHER_NUC_LEASED,
     +         OTHER_NUC_VAROM,
     +         OTHER_NUC_FIXOM,
     +         OTHER_HYDRO_CAPACITY,
     +         OTHER_HYDRO_GENERATION,
     +         OTHER_HYDRO_VAROM,
     +         OTHER_HYDRO_FIXOM,
     +         OTHER_STORAGE_PUMP,
     +         OTHER_STORAGE_GENERATION,
     +         OTHER_STORAGE_VAROM,
     +         OTHER_STORAGE_FIXOM,
     +         OTHER_PURCHASE_GENERATION,
     +         OTHER_PURCHASE_COST,
     +         OTHER_ECONOMY_PURCHASE,
     +         OTHER_ECONOMY_SALES,
     +         OTHER_ECONOMY_COST,
     +         OTHER_ECONOMY_REVENUE,
     +         OTHER_SECONDARY_SALES,
     +         OTHER_SECONDARY_SALES_REVENUES,
     +         OTHER_DSM_ENERGY_MODIFICATION,
     +         OTHER_DSM_PEAK_MODIFICATION,
     +         OTHER_DSM_EXPENSE_WO_REBATES,
     +         OTHER_DSM_REBATE_EXPENSE,
     +         OTHER_T_AND_D_EXPENSE,
     +         OTHER_EMISSIONS,
     +         OTHER_PEAK_BEFORE_DSM,
     +         OTHER_ANNUAL_ENERGY
         IF(INT(OTHER_YEAR) /= YR+get_BASE_YEAR()) THEN
            WRITE(4,*) "External Production File Year =",INT(OTHER_YEAR)
       WRITE(4,*) "Is Inconsistent With Model Year =",YR+get_BASE_YEAR()
            WRITE(4,*) '*** line 1365 FINANBSE.FOR ***'
            er_message='See WARNING MESSAGES -finanbse.for-10'
            call end_program(er_message)
         ENDIF
         MIDAS_FUEL_COST = MIDAS_FUEL_COST + OTHER_FOSSIL_FUEL
         MIDAS_VAROM = MIDAS_VAROM + OTHER_FOSSIL_VAROM +
     +                  OTHER_NUC_VAROM +
     +                           OTHER_HYDRO_VAROM + OTHER_STORAGE_VAROM
         MIDAS_FIXOM = MIDAS_FIXOM +
     +               OTHER_FOSSIL_FIXOM + OTHER_NUC_FIXOM +
     +                           OTHER_HYDRO_FIXOM + OTHER_STORAGE_FIXOM
         MIDAS_NUC_OWNED = MIDAS_NUC_OWNED + OTHER_NUC_OWNED
         MIDAS_NUC_LEASED = MIDAS_NUC_LEASED + OTHER_NUC_LEASED
         MIDAS_PURCHASE_COST = MIDAS_PURCHASE_COST +
     +                                             OTHER_PURCHASE_COST +
     +                                             OTHER_ECONOMY_COST -
     +                                             OTHER_ECONOMY_REVENUE
         MIDAS_SALES_REVENUES = MIDAS_SALES_REVENUES +
     +                                    OTHER_SECONDARY_SALES_REVENUES
         MIDAS_DSM_EXPENSE = MIDAS_DSM_EXPENSE +
     +                                      OTHER_DSM_EXPENSE_WO_REBATES
         MIDAS_DSM_REBATES = MIDAS_DSM_REBATES +
     +                                          OTHER_DSM_REBATE_EXPENSE
         MIDAS_OTHER3_EXPENSE = MIDAS_OTHER3_EXPENSE +
     +                                             OTHER_T_AND_D_EXPENSE
      RETURN

  200 WRITE(4,*) "Cannot Read External Production File"
      WRITE(4,*) '*** line 1393 FINANBSE.FOR ***'
      er_message='See WARNING MESSAGES -finanbse.for-11'
      call end_program(er_message)
 1000 FORMAT(A)
      END
