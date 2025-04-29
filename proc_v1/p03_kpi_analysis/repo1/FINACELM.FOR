
!     FINACELM.FOR
!     Copyright(c)  2000

!     Created: 9/15/2003 3:09:25 PM
!     Author : MARK S GERBER
!     Last change: MSG 8/23/2009 4:41:18 PM



      SUBROUTINE ELIMINATIONS_OBJECT()
      use end_routine, only: end_program, er_message
      use ac_elmeq
      use mod_base_year
      use spindriftlib
      use prod_arrays_dimensions
      USE SIZECOM
      use globecom
      implicit none

      LOGICAL*1 RETURN_A_VECTOR_FOR_ALL_YEARS,VOID_LOGICAL
      INTEGER IOS_BASE,IOS
      INTEGER*4 VALUES_2_ZERO
      INTEGER*2 INUNIT,IREC,DELETE,LRECL/384/,
     +          I,L,POINTER
      CHARACTER*5 BASE_FILE_NAME,OVERLAY_FAMILY_NAME,ELIM_FILE

      CHARACTER*256 FILE_NAME
      CHARACTER*256 BASE_FILE_DIRECTORY,OUTPUT_DIRECTORY
      CHARACTER*256 DATA_DRIVE
      CHARACTER*30 CLASSIFICATIONS(:)
      INTEGER*2 SINGLE_YEAR(:),MULTI_YEAR_ESCALAITON_VECTOR(:)
      REAL*4 SINGLE_AMOUNT(:)
      ALLOCATABLE :: CLASSIFICATIONS,SINGLE_YEAR,
     +               SINGLE_AMOUNT,MULTI_YEAR_ESCALAITON_VECTOR
      LOGICAL*4 FILE_EXISTS,ELIMINATION_FILE_EXISTS
      SAVE ELIMINATION_FILE_EXISTS
C DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER*2048 RECLN
      CHARACTER*20 FILE_TYPE/'Eliminations'/

      CHARACTER*2 ELIMINATION_OL/'BC'/

C CALCULATE CREDIT AND DEBIT INFORMATION

      INTEGER*2 NUM_OF_DEBITS_AND_CREDITS,MAX_ELIMINATION_NUM,
     +          R_MAX_ELIMINATION_NUM,NUM_OF_DEBITS
      PARAMETER (NUM_OF_DEBITS_AND_CREDITS=8,NUM_OF_DEBITS=4)
      SAVE MAX_ELIMINATION_NUM
      REAL ANNUAL_DATA(:),YEARLY_ELIMINATIONS_VALUES(:,:)
      ALLOCATABLE :: ANNUAL_DATA,YEARLY_ELIMINATIONS_VALUES
      SAVE YEARLY_ELIMINATIONS_VALUES
      INTEGER*2 R_YEAR
      REAL R_ELIMINATIONS_VALUES(0:*)
      REAL*4 ACCOUNTING_SIGN
      LOGICAL*1 IS_A_DEBIT,NON_CASH_ITEM
      LOGICAL*1 LAHEY_LF95
      CHARACTER*30 SCREEN_OUTPUT


      RETURN

      ENTRY EM_MAKEBIN


         BASE_FILE_NAME = ELIM_FILE()
         FILE_NAME = trim(BASE_FILE_DIRECTORY())//
     +                             "EMB"//trim(BASE_FILE_NAME)//".DAT"
         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
         IF(FILE_EXISTS) THEN
            IF(LAHEY_LF95()) THEN
               SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
               CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
            ELSE
               CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,
     +                                                   ALL_VERSIONS,0)
               CALL MG_LOCATE_WRITE(16,30,BASE_FILE_NAME,ALL_VERSIONS,0)
            ENDIF
            DATA_DRIVE = OUTPUT_DIRECTORY()
            OPEN(10,FILE=FILE_NAME)
            OPEN(11,FILE=trim(DATA_DRIVE)//"BCELIMS.BIN",
     +                      ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
            ALLOCATE(CLASSIFICATIONS(8),SINGLE_YEAR(8),
     +            SINGLE_AMOUNT(8),MULTI_YEAR_ESCALAITON_VECTOR(8))
            IREC = 0

            READ(10,*,IOSTAT=IOS) DELETE
            DO
               DO
                  READ(10,1000,IOSTAT=IOS) RECLN
                  IF(IOS /= 0) EXIT
                  IF(RECLN(1:1) == '7') EXIT
                  RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,'//
     +                                   ',,,,,,,,,,,,,,,,,,,,,,,,'
                  READ(RECLN,*,ERR=200) DELETE,
     +                                  CLASSIFICATIONS,
     +                                  SINGLE_YEAR,
     +                                  SINGLE_AMOUNT,
     +                                  MULTI_YEAR_ESCALAITON_VECTOR

C WRITE RECORD

                  IREC = IREC + 1
                  WRITE(11,REC=IREC) DELETE,
     +                               CLASSIFICATIONS,
     +                               SINGLE_YEAR,
     +                               SINGLE_AMOUNT,
     +                               MULTI_YEAR_ESCALAITON_VECTOR
               ENDDO
               IF(IOS /= 0) EXIT
            ENDDO
            CLOSE(10)
            CLOSE(11)
            DEALLOCATE(CLASSIFICATIONS,SINGLE_YEAR,
     +              SINGLE_AMOUNT,MULTI_YEAR_ESCALAITON_VECTOR)
         ELSE IF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
            CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "finacelm:1111")
         ENDIF
      RETURN

      ENTRY EM_MAKEOVL(OVERLAY_FAMILY_NAME) 


         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
            CALL LOCATE(10,51)
         ENDIF
         DATA_DRIVE = OUTPUT_DIRECTORY()
         FILE_NAME = trim(DATA_DRIVE)//"EMO"//
     +                               trim(OVERLAY_FAMILY_NAME)//".DAT"
         OPEN(10,FILE=FILE_NAME)
         READ(10,*) DELETE
         INUNIT = 12
         IF(ELIMINATION_OL == 'BC') THEN
            OPEN(11,FILE=trim(DATA_DRIVE)//"BCELIMS.BIN",
     +                                       ACCESS="DIRECT",RECL=LRECL)
            INUNIT = 11
         ENDIF
         OPEN(12,FILE=trim(DATA_DRIVE)//"OLELIMS.BIN",ACCESS="DIRECT",
     +                                      STATUS="UNKNOWN",RECL=LRECL)
         ALLOCATE(CLASSIFICATIONS(8),SINGLE_YEAR(8),
     +            SINGLE_AMOUNT(8),MULTI_YEAR_ESCALAITON_VECTOR(8))
         IREC = 0
         DO
            DO
               READ(10,1000,IOSTAT=IOS) RECLN
               IF(RECLN(1:1) == '7') EXIT
               IREC = IREC + 1
               READ(INUNIT,REC=IREC,IOSTAT=IOS_BASE) DELETE,
     +                                  CLASSIFICATIONS,
     +                                  SINGLE_YEAR,
     +                                  SINGLE_AMOUNT,
     +                                  MULTI_YEAR_ESCALAITON_VECTOR

               IF(IOS_BASE /= 0) EXIT
               IF(IOS == 0) THEN
                  RECLN = trim(RECLN)//
     +                        ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'//
     +                        ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
                  READ(RECLN,*,ERR=200) DELETE,
     +                                  CLASSIFICATIONS,
     +                                  SINGLE_YEAR,
     +                                  SINGLE_AMOUNT,
     +                                  MULTI_YEAR_ESCALAITON_VECTOR
               ENDIF

               WRITE(12,REC=IREC) DELETE,
     +                            CLASSIFICATIONS,
     +                            SINGLE_YEAR,
     +                            SINGLE_AMOUNT,
     +                            MULTI_YEAR_ESCALAITON_VECTOR
            ENDDO
            IF(IOS_BASE /= 0) EXIT
         ENDDO
         DEALLOCATE(CLASSIFICATIONS,SINGLE_YEAR,
     +              SINGLE_AMOUNT,MULTI_YEAR_ESCALAITON_VECTOR)
         CLOSE(10)
         CLOSE(12)

         IF(ELIMINATION_OL == 'BC') CLOSE(11)
         ELIMINATION_OL = 'OL'       
      RETURN

  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from FINACELM SIID116'
      call end_program(er_message)

      ENTRY RESET_ELIMINATION_OL

         ELIMINATION_OL = 'BC'
      RETURN

      ENTRY READ_ELIMINATIONS(R_MAX_ELIMINATION_NUM)

         DATA_DRIVE = OUTPUT_DIRECTORY()
         FILE_NAME = trim(DATA_DRIVE)//ELIMINATION_OL//"ELIMS.BIN"
         INQUIRE(FILE=FILE_NAME,EXIST=ELIMINATION_FILE_EXISTS)
         IF(ELIMINATION_FILE_EXISTS) THEN
            MAX_ELIMINATION_NUM = R_MAX_ELIMINATION_NUM
            OPEN(10,FILE=FILE_NAME,ACCESS="DIRECT",RECL=LRECL)
            ALLOCATE(CLASSIFICATIONS(8),SINGLE_YEAR(8),
     +               SINGLE_AMOUNT(8),
     +               MULTI_YEAR_ESCALAITON_VECTOR(8),
     +               ANNUAL_DATA(AVAIL_DATA_YEARS))
            IF(ALLOCATED(YEARLY_ELIMINATIONS_VALUES))
     +                            DEALLOCATE(YEARLY_ELIMINATIONS_VALUES)
            ALLOCATE(YEARLY_ELIMINATIONS_VALUES(0:MAX_ELIMINATION_NUM,
     +                                              0:AVAIL_DATA_YEARS))
            VALUES_2_ZERO = INT(AVAIL_DATA_YEARS+1) *
     +                                       INT(MAX_ELIMINATION_NUM+1)
            YEARLY_ELIMINATIONS_VALUES = 0.
            CALL SET_CLASSIFICATIONS_TYPES(R_MAX_ELIMINATION_NUM)
            DO
               READ(10,IOSTAT=IOS) DELETE,
     +                             CLASSIFICATIONS,
     +                             SINGLE_YEAR,
     +                             SINGLE_AMOUNT,
     +                             MULTI_YEAR_ESCALAITON_VECTOR
               IF(IOS /= 0) EXIT
               IF(DELETE > 7) CYCLE
               DO I = 1, NUM_OF_DEBITS_AND_CREDITS
                  CALL FIND_CLASSIFICATION(CLASSIFICATIONS(I),POINTER)
                  IF(POINTER < 0) CYCLE
                  IS_A_DEBIT = I <= NUM_OF_DEBITS
                  CALL FIND_OPERATOR(POINTER,IS_A_DEBIT,ACCOUNTING_SIGN)
                  
                  IF(MULTI_YEAR_ESCALAITON_VECTOR(I) /= 0) THEN
                     VOID_LOGICAL = 
     +                        RETURN_A_VECTOR_FOR_ALL_YEARS(ANNUAL_DATA,
     +                                     MULTI_YEAR_ESCALAITON_VECTOR)
                     DO L = 1, AVAIL_DATA_YEARS
                        YEARLY_ELIMINATIONS_VALUES(POINTER,L) =
     +                           YEARLY_ELIMINATIONS_VALUES(POINTER,L) +
     +                                  ACCOUNTING_SIGN * ANNUAL_DATA(L)
                        IF(POINTER == 39) THEN ! LTD INTEREST
                           YEARLY_ELIMINATIONS_VALUES(293,L) =
     +                           YEARLY_ELIMINATIONS_VALUES(293,L) +
     +                                  ACCOUNTING_SIGN * ANNUAL_DATA(L)
                        ENDIF
                     ENDDO
                     IF(NON_CASH_ITEM(POINTER)) THEN
                        DO L = 1, AVAIL_DATA_YEARS
                           YEARLY_ELIMINATIONS_VALUES(142,L) =
     +                           YEARLY_ELIMINATIONS_VALUES(142,L) +
     +                                  ACCOUNTING_SIGN * ANNUAL_DATA(L)
                        ENDDO
                     ENDIF
                  ENDIF
                  IF(SINGLE_YEAR(I) >= get_BASE_YEAR() .AND.
     +          SINGLE_YEAR(I) <= get_BASE_YEAR()+AVAIL_DATA_YEARS) THEN
                     L = SINGLE_YEAR(I) - get_BASE_YEAR()
                     YEARLY_ELIMINATIONS_VALUES(POINTER,L) =
     +                           YEARLY_ELIMINATIONS_VALUES(POINTER,L) +
     +                                ACCOUNTING_SIGN * SINGLE_AMOUNT(I)
                     IF(POINTER == 39) THEN ! LTD INTEREST
                        YEARLY_ELIMINATIONS_VALUES(293,L) =
     +                           YEARLY_ELIMINATIONS_VALUES(293,L) +
     +                                ACCOUNTING_SIGN * SINGLE_AMOUNT(I)
                     ENDIF
                     IF(NON_CASH_ITEM(POINTER)) THEN
                        YEARLY_ELIMINATIONS_VALUES(142,L) =
     +                           YEARLY_ELIMINATIONS_VALUES(142,L) +
     +                                ACCOUNTING_SIGN * SINGLE_AMOUNT(I)
                     ENDIF
                  ENDIF
               ENDDO
            ENDDO
            CLOSE(10)
C           IF(ALLOCATED(CLASSIFICATION_TYPES))
C    +                                  DEALLOCATE(CLASSIFICATION_TYPES)
            DEALLOCATE(CLASSIFICATIONS,SINGLE_YEAR,ANNUAL_DATA,
     +                 SINGLE_AMOUNT,MULTI_YEAR_ESCALAITON_VECTOR)
         ENDIF
C         
      RETURN

      ENTRY RETURN_ALL_ELIMINATIONS_FOR(R_YEAR,R_ELIMINATIONS_VALUES)

         IF(ELIMINATION_FILE_EXISTS) THEN
            L = R_YEAR
            IF(L <= AVAIL_DATA_YEARS) THEN
               CALL COMPLETE_STATEMENTS(R_YEAR,MAX_ELIMINATION_NUM,
     +                                  YEARLY_ELIMINATIONS_VALUES(0,L))
            ELSE 
               L = AVAIL_DATA_YEARS
            ENDIF
            DO I = 0, MAX_ELIMINATION_NUM
               R_ELIMINATIONS_VALUES(I) = R_ELIMINATIONS_VALUES(I) +
     +                                   YEARLY_ELIMINATIONS_VALUES(I,L)
            ENDDO
         ENDIF
      RETURN
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END

      SUBROUTINE FIND_CLASSIFICATION(R_CLASSIFICATIONS,R_CLASS_POSITION)

      INTEGER*2 R_CLASS_POSITION
      CHARACTER*(*) R_CLASSIFICATIONS
      INTEGER*2 R_VARIABLE,I
      REAL*4 FIND_OPERATION
      INTEGER*2 MAX_ELIMINATION_NUM,R_MAX_ELIMINATION_NUM
      SAVE MAX_ELIMINATION_NUM
      CHARACTER*30 CLASSIFICATION_TYPES(:)
      ALLOCATABLE :: CLASSIFICATION_TYPES
      SAVE CLASSIFICATION_TYPES
      LOGICAL*1 R_DEBIT

         R_CLASS_POSITION = -1
         IF(INDEX(R_CLASSIFICATIONS,'Not Used') == 0) THEN
            DO I = 0, MAX_ELIMINATION_NUM
               IF(trim(CLASSIFICATION_TYPES(I)) == ' ') CYCLE 
               IF(INDEX(R_CLASSIFICATIONS,
     +                               CLASSIFICATION_TYPES(I)) /=0 ) THEN
                  R_CLASS_POSITION = I
                  EXIT
               ENDIF   
            ENDDO
         ENDIF
      RETURN

      ENTRY FIND_OPERATOR(R_VARIABLE,R_DEBIT,FIND_OPERATION)

         FIND_OPERATION = -1
         SELECT CASE (R_VARIABLE)
         ! ADDITIONS ON THE INCOME STATEMENT
         CASE(0:3,32:34,37,41,43,307,329:333,326:328)  
            IF(R_DEBIT) THEN
               FIND_OPERATION = 1.
            ELSE
               FIND_OPERATION = -1.
            ENDIF   
         CASE(69,75)  ! BALANCE ASSET SHEET ITEMS
            IF(R_DEBIT) THEN
               FIND_OPERATION = 1.
            ELSE
               FIND_OPERATION = -1.
            ENDIF   
         CASE(89,104)  ! BALANCE LIABILIITES SHEET ITEMS
            IF(R_DEBIT) THEN
               FIND_OPERATION = -1.
            ELSE
               FIND_OPERATION = 1.
            ENDIF   
         CASE DEFAULT
            IF(R_DEBIT) THEN
               FIND_OPERATION = -1.
            ELSE
               FIND_OPERATION = 1.
            ENDIF   
         END SELECT
      RETURN

      ENTRY SET_CLASSIFICATIONS_TYPES(R_MAX_ELIMINATION_NUM)


         MAX_ELIMINATION_NUM = R_MAX_ELIMINATION_NUM
         IF(ALLOCATED(CLASSIFICATION_TYPES))
     +                                  DEALLOCATE(CLASSIFICATION_TYPES)
         ALLOCATE(CLASSIFICATION_TYPES(0:MAX_ELIMINATION_NUM))
         DO I = 0, MAX_ELIMINATION_NUM
            CLASSIFICATION_TYPES(I) = " "
         ENDDO

C ADDITIONS TO INCOME STATEMENT

         CLASSIFICATION_TYPES(0) = "Base Rates"
         CLASSIFICATION_TYPES(329) = "Residential"
         CLASSIFICATION_TYPES(330) = "Commercial"
         CLASSIFICATION_TYPES(331) = "Industrial"
         CLASSIFICATION_TYPES(332) = "Lighting"
         CLASSIFICATION_TYPES(307) = "Gas Revenues"
         CLASSIFICATION_TYPES(326) = "Unbilled Revenues"
         CLASSIFICATION_TYPES(327) = "ATL Deferred Revenues"
         CLASSIFICATION_TYPES(328) = "Relationship Revenues"
         CLASSIFICATION_TYPES(333) = "Bulk Power"
         CLASSIFICATION_TYPES(1) = "Adjustment Clause"
         CLASSIFICATION_TYPES(2) = "Secondary Sales"
         CLASSIFICATION_TYPES(3) = "Other Revenue"
         CLASSIFICATION_TYPES(4) = " "

C REDUCTIONS TO INCOME STATEMENT

         CLASSIFICATION_TYPES(5) = "Fossil Fuel"
         CLASSIFICATION_TYPES(6) = "Purchased Power"
         CLASSIFICATION_TYPES(7) = "Variable O&M"
         CLASSIFICATION_TYPES(8) = "Fixed O&M"
         CLASSIFICATION_TYPES(9) = "Service Transactions"
         CLASSIFICATION_TYPES(10) = "Other O&M"
         CLASSIFICATION_TYPES(11) = "Purchased Gas"
         CLASSIFICATION_TYPES(12) = "Other Expenses"
         CLASSIFICATION_TYPES(335) = "Transmission-Operation"
         CLASSIFICATION_TYPES(336) = "Transmission-Maintenance"
         CLASSIFICATION_TYPES(337) = "Distribution-Operation"
         CLASSIFICATION_TYPES(338) = "Distribution-Maintenance"
         CLASSIFICATION_TYPES(321) = "Customer Accounts"
         CLASSIFICATION_TYPES(322) = "Customer Services"
         CLASSIFICATION_TYPES(323) = "Sales Expense"
         CLASSIFICATION_TYPES(324) = "A&G Operations"
         CLASSIFICATION_TYPES(325) = "A&G Maintenance"
         CLASSIFICATION_TYPES(60) = "Lease Expense"
         CLASSIFICATION_TYPES(219) = "Owned Nuclear Fuel"
         CLASSIFICATION_TYPES(116) = "Leased Nuclear Fuel"
         CLASSIFICATION_TYPES(13) = "DSM Expense"
         CLASSIFICATION_TYPES(14) = "DSM Rebate"

         CLASSIFICATION_TYPES(15) = "Emissions Credit"
         CLASSIFICATION_TYPES(17) = "Depreciation Expense"
         CLASSIFICATION_TYPES(18) = "Amortization Expense"
         CLASSIFICATION_TYPES(19) = "CIAC Amortization"
         CLASSIFICATION_TYPES(20) = " " ! acl_TOTAL_EXPENSES_B4_TAXES
         CLASSIFICATION_TYPES(21) = "Operating Revenue Tax"
         CLASSIFICATION_TYPES(218) = "Other Taxes"
         CLASSIFICATION_TYPES(217) = "Property Taxes"
         CLASSIFICATION_TYPES(216) = "Environmental Tax"
         CLASSIFICATION_TYPES(23) = "Local Income Taxes"
         CLASSIFICATION_TYPES(24) = "State Income Taxes Paid"
         CLASSIFICATION_TYPES(25) = "State Tax on Capital"
         CLASSIFICATION_TYPES(26) = "Federal Income Taxes Paid"
         CLASSIFICATION_TYPES(27) = "Federal Tax on Capital"
         CLASSIFICATION_TYPES(28) = "Income Tax Deferrals Debit"
         CLASSIFICATION_TYPES(29) = "Income Tax Deferrals Credit"
         CLASSIFICATION_TYPES(50) = "Investment Tax Credit"
         CLASSIFICATION_TYPES(30) = " " ! acl_TOTAL_EXPENSE_AND_TAXES
         CLASSIFICATION_TYPES(31) = " " ! acl_OPERATING_INCOME
         CLASSIFICATION_TYPES(32) = "Deferred Revenues"
         CLASSIFICATION_TYPES(33) = "BTL Revenues"
         CLASSIFICATION_TYPES(34) = "Interest Income"
         CLASSIFICATION_TYPES(35) = "BTL Expenses"
         CLASSIFICATION_TYPES(36) = "BTL Income Taxes"
         CLASSIFICATION_TYPES(37) = "AFUDC Equity"
         CLASSIFICATION_TYPES(38) = " " !acl_INCOME_B4_INTEREST
         CLASSIFICATION_TYPES(39) = "LTD Interest"
         CLASSIFICATION_TYPES(40) = "STD Interest"
         CLASSIFICATION_TYPES(41) = "AFUDC Borrowed"
         CLASSIFICATION_TYPES(42) = " " ! acl_INCOME_AFTER_INTEREST
         CLASSIFICATION_TYPES(43) = "Subsidiary Income"
         CLASSIFICATION_TYPES(44) = "Extraordinary Items"
         CLASSIFICATION_TYPES(45) = " " ! acl_NET_PROFIT_OR_LOSS
         CLASSIFICATION_TYPES(46) = "Preferred Dividends"
       CLASSIFICATION_TYPES(47) = " " ! acl_EARNINGS_AVAILABLE_TO_COMMON
         CLASSIFICATION_TYPES(48) = "Common Dividends"
         CLASSIFICATION_TYPES(49) = " " ! acl_RETAINED_EARNINGS

C BALANCE SHEET ITEMS

         CLASSIFICATION_TYPES(69)  = "Other Investments"
         CLASSIFICATION_TYPES(75)  = "Assets NEC"
         CLASSIFICATION_TYPES(89)  = "Liabilities NEC"
         CLASSIFICATION_TYPES(104) = "STD Issued"
      RETURN
      END

      FUNCTION NON_CASH_ITEM(R_VARIABLE)


      LOGICAL*1 NON_CASH_ITEM
      INTEGER*2 R_VARIABLE

         SELECT CASE (R_VARIABLE)
         ! NON-CASH ITEMS ON THE INCOME STATEMENT
         CASE(17:19,28:29,32,37,41,43,44,326)  
            NON_CASH_ITEM = .TRUE.
         CASE DEFAULT
            NON_CASH_ITEM = .FALSE.
         END SELECT
      RETURN
      END

      SUBROUTINE COMPLETE_STATEMENTS(R_YEAR,MAX_ELIMINATION_NUM,
     +                               R_VARIABLE)


      use ac_elmeq
      INTEGER*2 MAX_ELIMINATION_NUM,R_YEAR,I
      REAL*4 R_VARIABLE(0:MAX_ELIMINATION_NUM)
      REAL*4 WORKING_CAPITIAL_LAST_YEAR
      REAL*4 TOTAL_DEPRECIATION,TOTAL_AMORTIZATION,
     +       TOTAL_CIAC_AMORTIZATION,TOTAL_DEFERRED_TAXES_CR,
     +       TOTAL_DEFERRED_TAXES_DR,TOTAL_RETAINED_EARNINGS,
     +       TOTAL_NUCLEAR_FUEL_BURNED,
     +       TOTAL_STD_ISSUED,
     +       TOTAL_CHANGE_IN_CASH,
     +       TOTAL_CHANGE_OTHER_ASSETS,
     +       TOTAL_CHANGE_ASSETS_NEC,
     +       TOTAL_CHANGE_LIABS_NEC,
     +       TOTAL_CHANGE_DEFERRED_REVENUES
      SAVE TOTAL_DEPRECIATION,TOTAL_AMORTIZATION,
     +     TOTAL_CIAC_AMORTIZATION,TOTAL_DEFERRED_TAXES_CR,
     +     TOTAL_DEFERRED_TAXES_DR,TOTAL_RETAINED_EARNINGS,
     +     TOTAL_NUCLEAR_FUEL_BURNED,
     +     TOTAL_STD_ISSUED,
     +     TOTAL_CHANGE_IN_CASH,
     +     TOTAL_CHANGE_OTHER_ASSETS,
     +     TOTAL_CHANGE_ASSETS_NEC,
     +     TOTAL_CHANGE_LIABS_NEC,
     +     TOTAL_CHANGE_DEFERRED_REVENUES
      SAVE WORKING_CAPITIAL_LAST_YEAR

C COPY ELIMINATION DATA INTO WORKING ARRAY

         acl_VARIABLE(0:MAX_ELIMINATION_NUM) =
     +                                 R_VARIABLE(0:MAX_ELIMINATION_NUM)
         IF(R_YEAR == 1) THEN
            WORKING_CAPITIAL_LAST_YEAR = 0.
            TOTAL_CHANGE_OTHER_ASSETS = 0.
            TOTAL_CHANGE_ASSETS_NEC = 0.
            TOTAL_CHANGE_LIABS_NEC = 0.
            TOTAL_DEPRECIATION = 0.
            TOTAL_AMORTIZATION = 0.
            TOTAL_CIAC_AMORTIZATION = 0.
            TOTAL_DEFERRED_TAXES_CR = 0.
            TOTAL_DEFERRED_TAXES_DR = 0.
            TOTAL_RETAINED_EARNINGS = 0.
            TOTAL_NUCLEAR_FUEL_BURNED = 0.
            TOTAL_STD_ISSUED = 0.
            TOTAL_CHANGE_IN_CASH = 0.
            TOTAL_CHANGE_DEFERRED_REVENUES = 0.
         ENDIF

C ADJUSTMENTS TO ITEMS THAT ARE MAINTAINED AS BALANCES BUT ARE ENTERED
C  AS CHANGES

C ASSETS NEC
         TOTAL_CHANGE_ASSETS_NEC = acl_ASSET_NEC + 
     + TOTAL_CHANGE_ASSETS_NEC
         acl_ASSET_NEC = TOTAL_CHANGE_ASSETS_NEC
C LIABILITIES NEC
         TOTAL_CHANGE_LIABS_NEC = acl_LIABILITIES_NEC +
     +          TOTAL_CHANGE_LIABS_NEC
         acl_LIABILITIES_NEC = TOTAL_CHANGE_LIABS_NEC
C OTHER ASSETS
         TOTAL_CHANGE_OTHER_ASSETS = TOTAL_CHANGE_OTHER_ASSETS  +
     +                                                 acl_ASSETS_OTHER
         acl_INVESTMENT_SALVAGE_OTHER = acl_INVESTMENT_SALVAGE_OTHER +
     +                                                 acl_ASSETS_OTHER
         acl_ASSETS_OTHER = TOTAL_CHANGE_OTHER_ASSETS 

C INCOME STATEMENT

         acl_REVENUES_OPERATING = acl_BASE_RATES_REVENUES +
     +                        acl_ADJUSTMENT_CLAUSE_REVENUES +
     +                        acl_SECONDARY_SALES_REVENUES +
     +                        acl_REVENUES_OTHER
         acl_CLASS_NUCLEAR_FUEL_EXPENSE = acl_LEASED_NF_BURN +
     +                                acl_OWNED_NF_BURN
         acl_TOTAL_EXPENSES_B4_TAXES = acl_FUEL_EXPENSE +
     +                             acl_PURCHASE_POWER_EXPENSE +
     +                             acl_VARIABLE_EXPENSE +
     +                             acl_FIXED_EXPENSE +
     +                             acl_SERVICE_TRANSACTION_EXPENSE +
     +                             acl_OTHER_EXPENSE_1 +
     +                             acl_OTHER_EXPENSE_2 +
     +                             acl_OTHER_EXPENSE_3 +
     +                             acl_ATL_LEASE_PAYMENTS +
     +                             acl_DSM_EXPENSE +
     +                             acl_DSM_REBATE +
     +                             acl_EMISSION_CREDIT_EXPENSE +
     +                             acl_CLASS_NUCLEAR_FUEL_EXPENSE +
     +                             acl_BOOK_DEPRECIATION +
     +                             acl_AMORTIZATION_EXPENSE +
     +                             acl_CIAC_AMORTIZATION

C TAX ITEMS

               acl_OTHER_AND_ENVIRONMENTAL_TAXES = acl_OTHER_TAXES +
     +                                         acl_ENVIRONMENTAL_TAX
            acl_TOTAL_EXPENSE_AND_TAXES = acl_TOTAL_EXPENSES_B4_TAXES +
     +                                   acl_OTHER_TAXES +
     +                                   acl_ENVIRONMENTAL_TAX +
     +                                   acl_CLASS_PROPERTY_TAXES +
     +                                   acl_OPERATING_REVENUE_TAX +
     +                                   acl_LOCAL_TAXES +
     +                                   acl_STATE_TAXES +
     +                                   acl_STATE_TAX_ON_CAPITAL +
     +                                   acl_FEDERAL_TAXES +
     +                                   acl_FEDERAL_TAX_ON_CAPITAL +
     +                                   acl_DEFERRED_TAXES_DR +
     +                                   acl_DEFERRED_TAXES_CR +
     +                                   acl_TAX_CREDITS

C OPERATING INCOME

               acl_OPERATING_INCOME = acl_REVENUES_OPERATING -
     +                            acl_TOTAL_EXPENSE_AND_TAXES

C INCOME B4 INTEREST

               acl_INCOME_B4_INTEREST = acl_OPERATING_INCOME
     +                              + acl_DEFERRED_REVENUES
     +                              + acl_CLASS_BTL_REVENUES
     +                              + acl_NET_DERIVATIVES_INCOME
     +                              + acl_INVESTMENT_INCOME
     +                              - acl_CLASS_BTL_EXPENSES
     +                              - acl_BTL_INCOME_TAXES
     +                              + acl_AFUDC_EQUITY

               acl_INCOME_AFTER_INTEREST = acl_INCOME_B4_INTEREST -
     +                              acl_INTEREST_ON_LONG_TERM_DEBT -
     +                                 acl_STD_INTEREST +
     +                                 acl_AFUDC_BORROWED

               acl_NET_PROFIT_OR_LOSS = acl_INCOME_AFTER_INTEREST +
     +                              acl_SUBSIDIARY_INCOME -
     +                              acl_NET_OF_TAX_EXEXP

             acl_EARNINGS_AVAILABLE_TO_COMMON = acl_NET_PROFIT_OR_LOSS -
     +                                        acl_PREFERRED_DIVIDENDS

              acl_RETAINED_EARNINGS = acl_EARNINGS_AVAILABLE_TO_COMMON -
     +                             acl_COMMON_DIVIDENDS

C CHANGE IN CASH 

         acl_INCOME_BEFORE_CAPITAL_SERVICE = acl_INCOME_B4_INTEREST +
     +                                                acl_AFUDC_BORROWED
        acl_FUNDS_FROM_OPERATIONS = acl_INCOME_BEFORE_CAPITAL_SERVICE + 
     +                                 acl_CLASS_NUCLEAR_FUEL_EXPENSE + 
     +                                 acl_BOOK_DEPRECIATION + 
     +                                 acl_AMORTIZATION_EXPENSE + 
     +                                 acl_CIAC_AMORTIZATION - 
     +                                 acl_DEFERRED_REVENUES - 
     +                                 acl_AFUDC_EQUITY - 
     +                                 acl_AFUDC_BORROWED + 
     +                                 acl_DEFERRED_TAXES_DR + 
     +                                 acl_DEFERRED_TAXES_CR + 
     +                                 acl_TAX_CREDITS - 
     +                            acl_INTEREST_ON_LONG_TERM_DEBT - 
     +                                 acl_STD_INTEREST - 
     +                                 acl_PREFERRED_DIVIDENDS - 
     +                                 acl_COMMON_DIVIDENDS + 
     +                                 acl_CASH_FROM_SALE_OF_ASSETS + 
     +                                 acl_DIVIDEND_FROM_SUBSIDIARY + 
     +                                 acl_TAXES_PAID_BY_PARENT + 
     +                            acl_TAX_PAYMENTS_2_PARENT_FROM_SUB - 
     +                                 acl_INCOME_TAXES_CONSOLIDATED + 
     +                                 acl_CIAC_CASH + 
     +                                 acl_CUSTOMER_DEPOSITS +
     +                                 acl_ELIMINATION_CASH_ADJUSTMENT

         acl_CONSTRUCTION_AND_NET_INVESTMENT = acl_FA_CASH  + 
     +                                     acl_NF_CASH  + 
     +                                  acl_INVESTMENT_SALVAGE_OTHER + 
     +                                  acl_NEW_SUBSIDIARY_INVESTMENT + 
     +                                  acl_CAPITIALIZED_LEASE_ADDITIONS
       acl_CHANGE_IN_WORKING_CAPITIAL = acl_ASSET_NEC - 
     + acl_LIABILITIES_NEC - 
     +                                WORKING_CAPITIAL_LAST_YEAR
        WORKING_CAPITIAL_LAST_YEAR = acl_ASSET_NEC - acl_LIABILITIES_NEC
         acl_CHANGE_IN_FUNDS_B4_FINANCING = acl_FUNDS_FROM_OPERATIONS - 
     +                             acl_CONSTRUCTION_AND_NET_INVESTMENT -
     +                                 acl_LEASED_NF_BURN - 
     +                                 acl_NUCL_DECOM_FUND - 
     +                                 acl_CHANGE_IN_WORKING_CAPITIAL - 
     +                                 acl_LONG_TERM_DEBT_RETIREMENTS - 
     +                                 acl_PREFERRED_STOCK_RETIREMENTS -
     +                                 acl_COMMON_STOCK_BOUGHT- 
     +                                 acl_DEFERRED_EXPENSE_CASH  
C         acl_LEASE_RECEIPTS = acl_CAPITIALIZED_LEASE_ADDITIONS +
C     +                    acl_LEASED_NF_BURN 
         acl_EXTERNAL_FINANCING_PERFORMED = acl_COMMON_STOCK_ISSUED + 
     +                                acl_LONG_TERM_DEBT_ISSUED + 
     +                                acl_PREFERRED_STOCK_ISSUED + 
     +                                acl_SHORT_TERM_DEBT_ISSUED + 
     +                                acl_LEASE_RECEIPTS   
         acl_CHANGE_IN_CASH = acl_EXTERNAL_FINANCING_PERFORMED +
     +                    acl_CHANGE_IN_FUNDS_B4_FINANCING 

C BALANCE SHEET ITEMS

         acl_GROSS_PLANT_VALUE_PLUS_CWIP = acl_GROSS_PLANT_VALUE 
     +    + acl_CWIP
C      
         TOTAL_DEPRECIATION = TOTAL_DEPRECIATION + acl_BOOK_DEPRECIATION
         acl_CUMULATIVE_DEPRECIATION = acl_CUMULATIVE_DEPRECIATION +
     +                                                TOTAL_DEPRECIATION
         TOTAL_NUCLEAR_FUEL_BURNED = TOTAL_NUCLEAR_FUEL_BURNED +
     +                                    acl_CLASS_NUCLEAR_FUEL_EXPENSE
         acl_NET_NF_VALUE = acl_NET_NF_VALUE - TOTAL_NUCLEAR_FUEL_BURNED

         acl_UTILITY_PLANT_NET = acl_GROSS_PLANT_VALUE_PLUS_CWIP -
     +                       acl_CUMULATIVE_DEPRECIATION +
     +                       acl_NET_NF_VALUE 

      TOTAL_AMORTIZATION = TOTAL_AMORTIZATION + acl_AMORTIZATION_EXPENSE
         acl_DD_BALANCE_NET = acl_DD_BALANCE_NET - TOTAL_AMORTIZATION 
         TOTAL_DEFERRED_TAXES_DR = TOTAL_DEFERRED_TAXES_DR +
     +                                             acl_DEFERRED_TAXES_DR
         acl_DEFERRED_TAXES_DR_BAL = acl_DEFERRED_TAXES_DR_BAL -
     +                           TOTAL_DEFERRED_TAXES_DR 

        TOTAL_CHANGE_IN_CASH = TOTAL_CHANGE_IN_CASH + acl_CHANGE_IN_CASH
         acl_cash_balance = acl_cash_balance + TOTAL_CHANGE_IN_CASH
         TOTAL_CHANGE_DEFERRED_REVENUES = acl_DEFERRED_REVENUES +
     +                                    TOTAL_CHANGE_DEFERRED_REVENUES
         acl_DEFERRED_REVENUES_CUM = acl_DEFERRED_REVENUES_CUM+ 
     + acl_DEFERRED_REVENUES

         acl_TOTAL_ASSETS = acl_UTILITY_PLANT_NET +
     +                  acl_ASSETS_OTHER +
     +                  acl_SUBSIDIARY_INVESTMENT +
     +                  acl_LEASES_CAPITIALZIED +
     +                  acl_DECOMMISIONING_FUND_BAL +
     +                  acl_DD_BALANCE_NET +
     +                  acl_DEFERRED_REVENUES_CUM +
     +                  acl_DEFERRED_TAXES_DR_BAL +
     +                  acl_cash_balance +
     +                  acl_ASSET_NEC

         TOTAL_CIAC_AMORTIZATION = TOTAL_CIAC_AMORTIZATION +
     +                                             acl_CIAC_AMORTIZATION
         acl_CIAC_BAL = acl_CIAC_BAL - TOTAL_CIAC_AMORTIZATION 
         TOTAL_DEFERRED_TAXES_CR = TOTAL_DEFERRED_TAXES_CR +
     +                                             acl_DEFERRED_TAXES_CR
       acl_DEF_TAX_CR_BAL = acl_DEF_TAX_CR_BAL + TOTAL_DEFERRED_TAXES_CR
         TOTAL_RETAINED_EARNINGS = TOTAL_RETAINED_EARNINGS +
     +                                       acl_RETAINED_EARNINGS
         acl_RETAINED_EARNINGS_BAL = acl_RETAINED_EARNINGS_BAL +
     +                           TOTAL_RETAINED_EARNINGS 
        TOTAL_STD_ISSUED = TOTAL_STD_ISSUED + acl_SHORT_TERM_DEBT_ISSUED
         acl_SHORT_TERM_DEBT_BAL = acl_SHORT_TERM_DEBT_BAL + 
     + TOTAL_STD_ISSUED 
         acl_COMMON_EQUITY_BAL = acl_COMMON_STOCK_BAL +
     +                       acl_RETAINED_EARNINGS_BAL
         acl_TOTAL_CAPITIAL_BAL = acl_COMMON_EQUITY_BAL +
     +                        acl_PREFERRED_STOCK_BAL +
     +                        acl_LONG_TERM_DEBT_BAL
         ACL_TOTAL_LIABILITIES = acl_TOTAL_CAPITIAL_BAL +
     +                       acl_OTHER_LT_LIABILITY_BAL +
     +                       acl_SHORT_TERM_DEBT_BAL +
     +                       acl_CUSTOMER_DEPOSITS_BALANCE +
     +                       acl_CIAC_BAL +
     +                       acl_DEF_TAX_CR_BAL +
     +                       acl_DEF_ITC_BAL +
     +                       acl_LIABILITIES_NEC

            R_VARIABLE(0:MAX_ELIMINATION_NUM) =
     +                               acl_VARIABLE(0:MAX_ELIMINATION_NUM)
      RETURN
      END
