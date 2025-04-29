!     ******************************************************************
!     msgmmpyb.for
!     Copyright(c) M.S. Gerber & Associates 2000
!
!     Created: 1/13/2003 4:07:27 PM
!     Author : MARK S GERBER
!     Last change: MSG 1/29/2007 3:58:36 PM
!     ******************************************************************

! ***********************************************************************
! *                                                                     *
! *                           PAYABLESS                                 *
! *                                                                     *
! *          COPYRIGHT (C) 1997,1994 M.S. GERBER & ASSOCIATES, INC      *
! *                         ALL RIGHTS RESERVED                         *
! *                                                                     *
! ***********************************************************************
      SUBROUTINE  MONTHLY_PAYABLE_OBJECT
      use end_routine, only: end_program, er_message
!
       use SpinDriftLib
       use prod_arrays_dimensions
       use sizecom

      INTEGER(kind=2) :: IREC,INUNIT,LRECL=512
      INTEGER(kind=4) :: IOS,IOS_BASE

      INTEGER(kind=2) :: UNIT_NUM=10,ASSET_CLASS_ID,MO

      INTEGER(kind=2) :: R_UNIT_NUM,YR,I
      CHARACTER(len=30) :: PAYABLES_CLASSIFICATION
      CHARACTER(len=5) :: BASE_FILE_NAME,OVERLAY_FAMILY_NAME, &
                  MONTHLY_PAYABLES_FILE
      CHARACTER(len=30) :: COMMENT*50,DESCRIPTION,TEMP_DESCRIPTION
      CHARACTER(len=256) :: FILE_NAME
      CHARACTER(len=256) :: BASE_FILE_DIRECTORY,OUTPUT_DIRECTORY
      CHARACTER(len=256) :: DATA_DRIVE
      LOGICAL(kind=4) :: FILE_EXISTS,R_FILE_EXISTS
      SAVE FILE_EXISTS
!  DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER(len=1024) :: RECLN
!  DECLARATION FOR /PAYABLES FILE/
      INTEGER(kind=2) :: DELETE
      REAL(kind=4) :: VALUES(12,5),VALUES_24(12,2),VALUES_36(12,3)
      EQUIVALENCE (VALUES(1,1),VALUES_24(1,1)), &
                  (VALUES(1,3),VALUES_36(1,1))
      CHARACTER(len=16) :: FILE_TYPE='Monthly Payables'
      CHARACTER(len=2) :: PAYABLES_OL='BC',R_PAYABLES_OL,CLASS_TYPE
      CHARACTER(len=1) :: DATA_TYPE(5),ACCOUNT_STATUS
      LOGICAL(kind=1) :: LAHEY_LF95
      CHARACTER(len=30) :: SCREEN_OUTPUT
      CHARACTER(len=4) :: LAST_MONTH(2)

! ***********************************************************************
!
!           ROUTINE TO CONVERT METAFILE FILES TO DIRECT-ACESS BINARY
!           COPYRIGHT (C) 1983, 84, 85  M.S. GERBER & ASSOCIATES, INC.
!
!  **********************************************************************
!
!  CONVERT THE PAYABLES FILE
! ***********************************************************************
      ENTRY MONTHLY_PAYABLES_MAKEBIN
! ***********************************************************************
      BASE_FILE_NAME = MONTHLY_PAYABLES_FILE()
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME = trim(BASE_FILE_DIRECTORY())// &
                                  "MPB"//trim(BASE_FILE_NAME)//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_LOCATE_WRITE(16,30,BASE_FILE_NAME,ALL_VERSIONS,0)
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         ENDIF
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCPAYABL.BIN", &
                 ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         IREC = 0
         READ(10,*) DELETE
         CLASS_TYPE = 'Cl'
         DATA_TYPE(1) = 'I'
         DATA_TYPE(2:5) = 'N'
         LAST_MONTH = 'Dec'
         DO
            ACCOUNT_STATUS = 'A'
!
            DO
               READ(10,1000,IOSTAT=IOS) RECLN
               IF(IOS /=0) EXIT
               IF(RECLN(1:1) == '7') EXIT
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,' &
                                    //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,' &
                                    //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,' &
                                    //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,' &
                                    //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,' &
                                    //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,' &
                                    //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               VALUES = -999999.
               PAYABLES_CLASSIFICATION = 'Not Active'
               READ(RECLN,*,ERR=200) DELETE, &
                                     DESCRIPTION, &
                                     ASSET_CLASS_ID, &
                                     COMMENT, &
                                     PAYABLES_CLASSIFICATION, &
                                     VALUES_24, &
                                     DATA_TYPE(1),DATA_TYPE(2), & 
                                     CLASS_TYPE, &
                                     ACCOUNT_STATUS, &
                                     LAST_MONTH, &
                                     VALUES_36

               DO YR = 1, 5
                  IF(VALUES(1,YR) == -999999.) VALUES(1,YR) = 0.
                  DO MO = 2, 12
                     IF(VALUES(MO,YR) == -999999.) then
                        VALUES(MO,YR) = VALUES(MO-1,YR)
                     end if
                  ENDDO
               ENDDO
               IREC = IREC + 1
               WRITE(11,REC=IREC) DELETE, &
                                  DESCRIPTION, &
                                  ASSET_CLASS_ID, &
                                  PAYABLES_CLASSIFICATION, &
                                  CLASS_TYPE, &
                                  ACCOUNT_STATUS, &
                                  LAST_MONTH, &
                                  DATA_TYPE, &
                                  VALUES

            ENDDO
            IF(IOS /= 0) EXIT
         ENDDO
         CLOSE(10)

         CLOSE(11)

      ELSE IF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "msgmmpyb:1111")
      ENDIF
      RETURN

! ***********************************************************************
!
!           ROUTINE TO CREATE OVERLAY FILES
!           COPYRIGHT (C) 1984-88  M.S. GERBER & ASSOCIATES, INC.
!           COPYRIGHT (C) 1991-92  M.S. GERBER & ASSOCIATES, INC.
!
! ***********************************************************************
!
!  OVERLAY THE PAYABLES FILE
! ***********************************************************************
      ENTRY MONTHLY_PAYABLES_MAKEOVL(OVERLAY_FAMILY_NAME)
! ***********************************************************************
      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME=trim(DATA_DRIVE)//"MPO"// &
                                     trim(OVERLAY_FAMILY_NAME)//".DAT"
      OPEN(10,FILE=FILE_NAME)
      READ(10,*) DELETE
      INUNIT = 12
      IF(PAYABLES_OL == 'BC') THEN
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCPAYABL.BIN", &
                                             ACCESS="DIRECT",RECL=LRECL)
         INUNIT = 11
      ENDIF
      OPEN(12,FILE=trim(DATA_DRIVE)//"OLPAYABL.BIN",ACCESS="DIRECT", &
              STATUS="UNKNOWN",RECL=LRECL)
!
      IREC = 0
      DO
         DO
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(RECLN(1:1) == '7') EXIT
            IREC = IREC + 1
            READ(INUNIT,REC=IREC,IOSTAT=IOS_BASE) DELETE, &
                                     DESCRIPTION, &
                                     ASSET_CLASS_ID, &
                                     PAYABLES_CLASSIFICATION, &
                                     CLASS_TYPE, &
                                     ACCOUNT_STATUS, &
                                     LAST_MONTH, &
                                     DATA_TYPE, &
                                     VALUES
            IF(IOS_BASE /= 0) EXIT
            IF(IOS == 0) THEN
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,' &
                                    //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,' &
                                    //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,' &
                                    //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,' &
                                    //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,' &
                                    //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,' &
                                    //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,ERR=200) DELETE, &
                                     TEMP_DESCRIPTION, &
                                     ASSET_CLASS_ID, &
                                     COMMENT, &
                                     PAYABLES_CLASSIFICATION, &
                                     VALUES_24, &
                                     DATA_TYPE(1),DATA_TYPE(2), & !  (I),I=1,2),
                                     CLASS_TYPE, &
                                     ACCOUNT_STATUS, &
                                     LAST_MONTH, &
                                     VALUES_36
            ENDIF

            WRITE(12,REC=IREC) DELETE, &
                               DESCRIPTION, &
                               ASSET_CLASS_ID, &
                               PAYABLES_CLASSIFICATION, &
                               CLASS_TYPE, &
                               ACCOUNT_STATUS, &
                               LAST_MONTH, &
                               DATA_TYPE, &
                               VALUES
         ENDDO
         IF(IOS_BASE /= 0) EXIT
      ENDDO
      CLOSE(10)
      CLOSE(12)
      IF(PAYABLES_OL == 'BC') CLOSE(11)
      PAYABLES_OL= 'OL'

!
      RETURN
!

  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from msgmmpyb SIID231'
      call end_program(er_message)
!
! ***********************************************************************
      ENTRY RESET_PAYABLES_OL
! ***********************************************************************
         PAYABLES_OL = 'BC'
      RETURN
!
! ***********************************************************************
      ENTRY GET_PAYABLES_OL(R_PAYABLES_OL,R_FILE_EXISTS)
! ***********************************************************************
         R_PAYABLES_OL = PAYABLES_OL
         R_FILE_EXISTS = FILE_EXISTS
      RETURN
!
! ***********************************************************************
      ENTRY OPEN_PAYABLES_FILE(R_UNIT_NUM)
! ***********************************************************************
         OPEN(R_UNIT_NUM,FILE=trim(OUTPUT_DIRECTORY())//PAYABLES_OL// &
               "PAYABL.BIN",ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         UNIT_NUM = R_UNIT_NUM
      RETURN
!
! ***********************************************************************
      ENTRY OPEN_PAYABLES_BASE_CASE_FILE(R_UNIT_NUM)
! ***********************************************************************
         OPEN(R_UNIT_NUM,FILE=trim(OUTPUT_DIRECTORY())// &
                                      "BC_PAYAB.BIN",FORM='UNFORMATTED')
      RETURN
! ***********************************************************************
      ENTRY CLOSE_PAYABLES_FILE
! ***********************************************************************
         CLOSE(UNIT_NUM)
      RETURN

 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END
! ***********************************************************************
      SUBROUTINE MONTHLY_PAYABLES(PAYABLE_MONTHLY_VALUES, &
                                  ASSET_CLASS_POINTER, &
                                  MAX_CLASS_ID, &
                                  R_NUM_ACTIVE_CLASS, &
                                  R_PARENT_CLASS_ID_NUM)
! ***********************************************************************
!

      use mthnmcom

      CHARACTER(len=30) :: PAYABLES_CLASSIFICATION,DESCRIPTION
      INTEGER(kind=2) :: DELETE,MAX_CLASS_ID,IREC, &
                R_NUM_ACTIVE_CLASS, &
                R_PARENT_CLASS_ID_NUM,YR, &
                NUM_ACTIVE_CLASS,ACT_MOS
      INTEGER (KIND=2), SAVE :: PARENT_CLASS_ID_NUM
      INTEGER(kind=4) :: IOS
      SAVE NUM_ACTIVE_CLASS
      INTEGER(kind=2) :: ASSET_CLASS_POINTER(0:MAX_CLASS_ID), &
                ASSET_CLASS_LIST(30)
      REAL(kind=4) :: VALUES(12,5)
      CHARACTER(len=1) :: DUMMY_TYPE,CLASS_TYPE*2,ACCOUNT_STATUS
      INTEGER(kind=2) :: ASSET_CLASS,ASSET_CLASS_ID,CLASS_POINTER, &
                   MO,PAYMENT_TYPE
      REAL(kind=4) :: PAYABLE_MONTHLY_VALUES(0:12,PAYMENT_VARS,-1:*), & 
             TEMP_PAYABLE_MONTHLY_VALUES(0:12,PAYMENT_VARS,-1:*)
      REAL(kind=4) :: FILE_PAYABLE_VALUES(:,:,:,:), &
                 ACTUAL_PAYABLE_VALUES(:,:,:,:)
      ALLOCATABLE :: FILE_PAYABLE_VALUES,ACTUAL_PAYABLE_VALUES
      SAVE FILE_PAYABLE_VALUES,ACTUAL_PAYABLE_VALUES
      LOGICAL (KIND=1),ALLOCATABLE,SAVE::ACTUAL_VALUE_SPECIFIED(:,:,:,:)
      INTEGER(kind=2) :: R_YR,GET_MONTH_NUMBER
      REAL(kind=4) :: ANNUAL_AMOUNT
      CHARACTER(len=1) :: DATA_TYPE(5)
      INTEGER(kind=4) :: VALUES_2_SET
      LOGICAL(kind=1) :: INCLUDE_IN_COSOLIDATE
      INTEGER(kind=2) :: CLASS_POS,VARIABLE
      REAL(kind=4) :: CASH_VALUES(0:12,1:*)
      INTEGER(kind=2) :: R_CLASS_POS
      REAL(kind=4) :: CONSOLIDATED_VALUES(0:12,80:*)
      CHARACTER(len=4) :: LAST_MONTH(2),LAST_MONTH_STR
      REAL (KIND=4), DIMENSION(0:12) :: R_OFF_SYSTEM_SALES_RECEIVABLE, &
                                        R_OFF_SYSTEM_SALES_PAYABLE, &
                                      R_INT_POWER_COSTS_PAYABLE_PAY
      REAL (KIND=4), DIMENSION(12,6) :: R_ACTUAL_CASH_VALUES
      LOGICAL(kind=1) :: WVPA_DATA
      INTEGER(kind=2), PARAMETER :: Fuel=1, &
                              NonFuel=2, &
                              FixedCost=3, &
                              MemberCostofPower=4, &
                              NonMemberSales=5, &
                              NonMemberCostofPower=6
!
!
         PARENT_CLASS_ID_NUM = R_PARENT_CLASS_ID_NUM - 1
         NUM_ACTIVE_CLASS = R_NUM_ACTIVE_CLASS
         IF(ALLOCATED(FILE_PAYABLE_VALUES)) &
                                         DEALLOCATE(FILE_PAYABLE_VALUES, &
                                                 ACTUAL_VALUE_SPECIFIED, &
                                                  ACTUAL_PAYABLE_VALUES)
         ALLOCATE(FILE_PAYABLE_VALUES(12,5,PAYMENT_VARS, &
                                                   -1:NUM_ACTIVE_CLASS), &
                  ACTUAL_VALUE_SPECIFIED(12,5,PAYMENT_VARS, &
                                                   -1:NUM_ACTIVE_CLASS), &
                  ACTUAL_PAYABLE_VALUES(12,5,PAYMENT_VARS, &
                                                   -1:NUM_ACTIVE_CLASS))
         FILE_PAYABLE_VALUES = 0.
         ACTUAL_PAYABLE_VALUES = 0.
         ACTUAL_VALUE_SPECIFIED = .FALSE.
!
         IREC = 0
         CALL OPEN_PAYABLES_FILE(INT(11,2))
!
         DO
            IREC = IREC + 1
            READ(11,REC=IREC,IOSTAT=IOS) DELETE, &
                                         DESCRIPTION, &
                                         ASSET_CLASS_ID, &
                                         PAYABLES_CLASSIFICATION, &
                                         CLASS_TYPE, &
                                         ACCOUNT_STATUS, &
                                         LAST_MONTH, &
                                         DATA_TYPE, &
                                         VALUES
            IF(IOS /= 0) EXIT
            IF(DELETE > 7 .OR. ACCOUNT_STATUS == 'N' .OR. &
                      INDEX(PAYABLES_CLASSIFICATION,'Not A') /= 0) CYCLE
!
!  FIND THE PAYMENT TYPE
!
            WVPA_DATA = .FALSE.
            SELECT CASE(trim(PAYABLES_CLASSIFICATION))
               CASE ('STD Interest')
                  PAYMENT_TYPE = payment_std_interest
               CASE ('Revenue Taxes')
                  PAYMENT_TYPE = payment_operating_revenue_tax
               CASE ('Property Taxes')
                  PAYMENT_TYPE = payment_property_taxes
               CASE ('State Income Taxes')
                  PAYMENT_TYPE = payment_state_income_taxes_paid
               CASE ('Federal Income Taxes')
                  PAYMENT_TYPE = payment_federal_inc_taxes_paid
               CASE ('State Tax on Capital')
                  PAYMENT_TYPE = payment_state_tax_on_capital
               CASE ('Federal Tax on Capital')
                  PAYMENT_TYPE = payment_federal_tax_on_capital
               CASE ('Other Taxes')
                  PAYMENT_TYPE = payment_other_taxes
               CASE('Revenues Receivable','Accounts Receivable')
                  PAYMENT_TYPE = payment_revenues_receivable
               CASE('Expenses Payable','Accounts Payable')
                  PAYMENT_TYPE = payment_expenses_payable
               CASE('Non-Member Revenues Receivable')
                  PAYMENT_TYPE = wvpa_nonmbr_revnues_rcbl
                  WVPA_DATA = .TRUE.
               CASE('Non-Member Power Cost')
                  PAYMENT_TYPE = wvpa_nonmember_cost_of_power
                  WVPA_DATA = .TRUE.
               CASE('Member Power Cost')
                  PAYMENT_TYPE = WVPA_Member_Cost_of_Power
                  WVPA_DATA = .TRUE.
               CASE('Fuel Expense')
                  PAYMENT_TYPE = wvpa_fuel_expense
                  WVPA_DATA = .TRUE.
               CASE('Non-fuel Expense')
                  PAYMENT_TYPE = wvpa_nonfuel_expense
                  WVPA_DATA = .TRUE.
               CASE('Fixed Expense')
                  PAYMENT_TYPE = wvpa_fixed_expense
                  WVPA_DATA = .TRUE.
               CASE DEFAULT
                  PAYMENT_TYPE = -99
            END SELECT
            IF(PAYMENT_TYPE == -99) CYCLE
!
!  ALLOCATE TO ASSET CLASSES
!
            IF(INDEX(CLASS_TYPE,'Co') /= 0) THEN
            ELSEIF(INDEX(CLASS_TYPE,'Pr') /= 0 .OR. WVPA_DATA) THEN
               ASSET_CLASS_LIST(1) = PARENT_CLASS_ID_NUM
               ASSET_CLASS_LIST(2) = 0
            ELSEIF(ASSET_CLASS_ID < 0.) THEN
               CALL GET_ASSET_VAR(ABS(ASSET_CLASS_ID), &
                                            DUMMY_TYPE,ASSET_CLASS_LIST)
            ELSE
               ASSET_CLASS_LIST(1) = ASSET_CLASS_ID
               ASSET_CLASS_LIST(2) = 0
            ENDIF
            CLASS_POINTER = 1

            DO
               INCLUDE_IN_COSOLIDATE = .FALSE.
               IF(INDEX(CLASS_TYPE,'Co') /= 0) THEN
                  ASSET_CLASS = -1
               ELSE
                  ASSET_CLASS = ASSET_CLASS_LIST(CLASS_POINTER)
                  INCLUDE_IN_COSOLIDATE = &
                                      ASSET_CLASS == PARENT_CLASS_ID_NUM
                  ASSET_CLASS_ID = ASSET_CLASS + 1
                  IF(ASSET_CLASS_ID > MAX_CLASS_ID) EXIT
                  IF(ASSET_CLASS >= 0) ASSET_CLASS = &
                                     ASSET_CLASS_POINTER(ASSET_CLASS_ID)
               ENDIF
!
               DO YR = 1, 5
                  IF(DATA_TYPE(YR) == 'N') CYCLE
                  IF(DATA_TYPE(YR) == 'A' .AND. YR < 3) THEN
                     LAST_MONTH_STR = LAST_MONTH(YR)
                     ACT_MOS = GET_MONTH_NUMBER(LAST_MONTH_STR)
                     ACTUAL_VALUE_SPECIFIED(1:ACT_MOS,YR, &
                                     PAYMENT_TYPE,ASSET_CLASS) = .TRUE.
!                      DO MO = 1, 12
                     ACTUAL_PAYABLE_VALUES(1:ACT_MOS,YR, &
                                             PAYMENT_TYPE,ASSET_CLASS) = &
                            ACTUAL_PAYABLE_VALUES(1:ACT_MOS,YR, &
                                               PAYMENT_TYPE,ASSET_CLASS) &
                            + VALUES(1:ACT_MOS,YR)
                     IF(ACT_MOS < 12) THEN
                        FILE_PAYABLE_VALUES(ACT_MOS+1:,YR, &
                                             PAYMENT_TYPE,ASSET_CLASS) = &
                            FILE_PAYABLE_VALUES(ACT_MOS+1:,YR, &
                                               PAYMENT_TYPE,ASSET_CLASS) &
                            + VALUES(ACT_MOS+1:,YR)
                     ENDIF

                  ELSE

                      FILE_PAYABLE_VALUES(1:,YR,PAYMENT_TYPE, &
                                                          ASSET_CLASS) = &
                            FILE_PAYABLE_VALUES(1:,YR,PAYMENT_TYPE, &
                                                            ASSET_CLASS) &
                            + VALUES(1:,YR)

                  ENDIF
               ENDDO
!
!  IF PARENT CLASS COPY TO CONSOLIDATED
!
               IF(INCLUDE_IN_COSOLIDATE) THEN
                  DO YR = 1, 5
                     IF(DATA_TYPE(YR) == 'N') CYCLE
                     IF(DATA_TYPE(YR) == 'A' .AND. YR < 3) THEN
                     LAST_MONTH_STR = LAST_MONTH(YR)
                     ACT_MOS = GET_MONTH_NUMBER(LAST_MONTH_STR)

                     ACTUAL_VALUE_SPECIFIED(1:ACT_MOS,YR, &
                                               PAYMENT_TYPE,-1) = .TRUE.
                     ACTUAL_PAYABLE_VALUES(1:ACT_MOS,YR, &
                                                      PAYMENT_TYPE,-1) = &
                            ACTUAL_PAYABLE_VALUES(1:ACT_MOS,YR, &
                                                        PAYMENT_TYPE,-1) &
                            + VALUES(1:ACT_MOS,YR)
                     IF(ACT_MOS < 12) THEN
                        FILE_PAYABLE_VALUES(ACT_MOS+1:,YR, &
                                                      PAYMENT_TYPE,-1) = &
                            FILE_PAYABLE_VALUES(ACT_MOS+1:,YR, &
                                                        PAYMENT_TYPE,-1) &
                            + VALUES(ACT_MOS+1:,YR)
                     ENDIF

                  ELSE

                      FILE_PAYABLE_VALUES(1:,YR,PAYMENT_TYPE,-1) = &
                              FILE_PAYABLE_VALUES(1:,YR,PAYMENT_TYPE,-1) &
                              + VALUES(1:,YR)

                  ENDIF
                  ENDDO
               ENDIF
!
               CLASS_POINTER = CLASS_POINTER + 1
               IF(CLASS_POINTER > 30) EXIT
               IF(ASSET_CLASS_LIST(CLASS_POINTER) == 0. .OR. &
                           ASSET_CLASS_LIST(CLASS_POINTER) == -99.) EXIT
            ENDDO
         ENDDO
!
         CALL CLOSE_PAYABLES_FILE
!
         YR = 1
         DO CLASS_POS = -1, NUM_ACTIVE_CLASS
            DO VARIABLE = 1, PAYMENT_VARS
               DO MO = 1, 12
                  IF(ACTUAL_VALUE_SPECIFIED(MO,1, &
                                               VARIABLE,CLASS_POS)) THEN
                     PAYABLE_MONTHLY_VALUES(MO,VARIABLE,CLASS_POS) = &
                          ACTUAL_PAYABLE_VALUES(MO,1,VARIABLE,CLASS_POS)
                  ELSE
                     PAYABLE_MONTHLY_VALUES(MO,VARIABLE,CLASS_POS) = &
                            FILE_PAYABLE_VALUES(MO,1,VARIABLE,CLASS_POS)
                  ENDIF
               ENDDO
               PAYABLE_MONTHLY_VALUES(0,VARIABLE,CLASS_POS) = &
                      SUM(PAYABLE_MONTHLY_VALUES(1:,VARIABLE,CLASS_POS))
            ENDDO
         ENDDO
      RETURN
! ***********************************************************************
      ENTRY GET_ACTUAL_MONTHLY_PAYABLES(R_YR,R_CLASS_POS,CASH_VALUES)
! ***********************************************************************
         CLASS_POS = R_CLASS_POS
         YR = R_YR
!
         DO VARIABLE = 1, PAYMENT_VARS
            IF(.NOT. ANY(ACTUAL_VALUE_SPECIFIED(:, &
                                        YR,VARIABLE,CLASS_POS),1)) CYCLE

            SELECT CASE(VARIABLE)
               CASE (payment_std_interest)
                  PAYMENT_TYPE = cash_std_interest
               CASE (payment_operating_revenue_tax)
                  PAYMENT_TYPE = cash_operating_revenue_tax
               CASE (payment_property_taxes)
                  PAYMENT_TYPE = cash_property_taxes
               CASE (payment_state_income_taxes_paid)
                  PAYMENT_TYPE = cash_st_income_taxes_pd
               CASE (payment_federal_inc_taxes_paid)
                  PAYMENT_TYPE = cash_fed_income_taxs_pd
               CASE (payment_state_tax_on_capital)
                  PAYMENT_TYPE = cash_state_on_capital
               CASE (payment_federal_tax_on_capital)
                  PAYMENT_TYPE = cash_fed_tax_on_capital
               CASE (payment_other_taxes)
                  PAYMENT_TYPE = cash_other_taxes
               CASE(payment_revenues_receivable)
                  PAYMENT_TYPE = cash_rcd_accts_rcvbl ! 158
               CASE(payment_expenses_payable)
                  PAYMENT_TYPE = Cash_Paid_on_Accounts_Payable     ! 157
               CASE DEFAULT
                  PAYMENT_TYPE = -99
            END SELECT
            IF(PAYMENT_TYPE == -99) CYCLE
!
            DO MO = 1, 12
               IF(ACTUAL_VALUE_SPECIFIED(MO,YR,VARIABLE,CLASS_POS))THEN
                  CASH_VALUES(MO,PAYMENT_TYPE) = &
                         ACTUAL_PAYABLE_VALUES(MO,YR,VARIABLE,CLASS_POS)
               ENDIF
            ENDDO
            CASH_VALUES(0,PAYMENT_TYPE) = &
                                       SUM(CASH_VALUES(1:,PAYMENT_TYPE))
         ENDDO
      RETURN
! ***********************************************************************
      ENTRY GET_WVPA_MONTHLY_PAYABLES(R_YR, &
                                     R_OFF_SYSTEM_SALES_RECEIVABLE, &
                                     R_OFF_SYSTEM_SALES_PAYABLE, &
                                     R_INT_POWER_COSTS_PAYABLE_PAY)
! ***********************************************************************
         IF(R_YR > 2) RETURN
!
         ASSET_CLASS = PARENT_CLASS_ID_NUM + 1

         R_OFF_SYSTEM_SALES_PAYABLE(1:12) = &
               R_OFF_SYSTEM_SALES_PAYABLE(1:12) &
               - ACTUAL_PAYABLE_VALUES(1:12,R_YR, &
                                          wvpa_member_cost_of_power, &
                                                         ASSET_CLASS) &
               - ACTUAL_PAYABLE_VALUES(1:12,R_YR, &
                                        wvpa_nonmember_cost_of_power, &
                                                         ASSET_CLASS)
         R_INT_POWER_COSTS_PAYABLE_PAY(1:12) = &
               R_INT_POWER_COSTS_PAYABLE_PAY(1:12) &
               - ACTUAL_PAYABLE_VALUES(1:12,R_YR,wvpa_fuel_expense, &
                                                    PARENT_CLASS_ID_NUM) &
               - ACTUAL_PAYABLE_VALUES(1:12,R_YR,wvpa_nonfuel_expense, &
                                                            ASSET_CLASS) &
               - ACTUAL_PAYABLE_VALUES(1:12,R_YR,wvpa_fixed_expense, &
                                                            ASSET_CLASS)
!
      RETURN
! ***********************************************************************
      ENTRY WVPA_CASH_PAYMENTS_RECEIPTS(R_YR,CASH_VALUES)
! ***********************************************************************
         IF(R_YR > 2) RETURN
!
         ASSET_CLASS = PARENT_CLASS_ID_NUM + 1
            CASH_VALUES(1:12,wvpa_cash_nonmbr_pwr_cost) = &
                 CASH_VALUES(1:12,wvpa_cash_nonmbr_pwr_cost) &
                 + ACTUAL_PAYABLE_VALUES(1:12,R_YR, &
                                           wvpa_nonmember_cost_of_power, &
                                                            ASSET_CLASS)
            CASH_VALUES(1:12,wvpa_cash_fm_nonmbr_pwr_sales) = &
                 CASH_VALUES(1:12,wvpa_cash_fm_nonmbr_pwr_sales) &
                 + ACTUAL_PAYABLE_VALUES(1:12,R_YR, &
                         wvpa_nonmbr_revnues_rcbl, &
                                                    ASSET_CLASS)
            CASH_VALUES(1:12,WVPA_Cash_Member_Cost_of_Power) = &
                   CASH_VALUES(1:12,WVPA_Cash_Member_Cost_of_Power) &
                   + ACTUAL_PAYABLE_VALUES(1:12,R_YR, &
                      wvpa_member_cost_of_power, &
                                                 ASSET_CLASS)


            CASH_VALUES(1:,Cash_Fossil_Fuel) = &
                 CASH_VALUES(1:,Cash_Fossil_Fuel) &
                 + ACTUAL_PAYABLE_VALUES(1:12,R_YR,wvpa_fuel_expense, &
                                                            ASSET_CLASS)
            CASH_VALUES(1:,Cash_Variable_OandM) = &
                 CASH_VALUES(1:,Cash_Variable_OandM) &
                 + ACTUAL_PAYABLE_VALUES(1:12,R_YR,wvpa_nonfuel_expense, &
                                                            ASSET_CLASS)
            CASH_VALUES(1:,Cash_Fixed_OandM) = &
                 CASH_VALUES(1:,Cash_Fixed_OandM) &
                 + ACTUAL_PAYABLE_VALUES(1:12,R_YR,wvpa_fixed_expense, &
                                                            ASSET_CLASS)
!
      RETURN
! ***********************************************************************
      ENTRY GET_ACTUAL_CONSOLID_PAYABLES(R_YR,CONSOLIDATED_VALUES)
! ***********************************************************************
         CLASS_POS = -1
         YR = R_YR
!
         DO VARIABLE = 1, PAYMENT_VARS

            SELECT CASE(VARIABLE)
               CASE (payment_std_interest)
                  PAYMENT_TYPE = cash_std_interest    ! 82
!                 CYCLE
               CASE (payment_operating_revenue_tax)
                  PAYMENT_TYPE = cash_operating_revenue_tax  ! 90
!                  CYCLE
               CASE (payment_property_taxes)
                  PAYMENT_TYPE = cash_property_taxes ! 92
!                  CYCLE
               CASE (payment_state_income_taxes_paid)
                  PAYMENT_TYPE = cash_st_income_taxes_pd ! 93
               CASE (payment_federal_inc_taxes_paid)
                  PAYMENT_TYPE = cash_fed_income_taxs_pd ! 95
               CASE (payment_state_tax_on_capital)
                  PAYMENT_TYPE = cash_state_on_capital ! 94
                  CYCLE
               CASE (payment_federal_tax_on_capital)
                  PAYMENT_TYPE = cash_fed_tax_on_capital ! 96
                  CYCLE
               CASE (payment_other_taxes)
                  PAYMENT_TYPE = cash_other_taxes ! 91
!                  CYCLE
               CASE(payment_revenues_receivable)
                  PAYMENT_TYPE = cash_rcd_accts_rcvbl ! 158
               CASE(payment_expenses_payable)
                  PAYMENT_TYPE = Cash_Paid_on_Accounts_Payable     ! 157
               CASE DEFAULT
                  PAYMENT_TYPE = -99
                  CYCLE
            END SELECT
!
            IF(ANY(ACTUAL_VALUE_SPECIFIED(:, &
                                         YR,VARIABLE,CLASS_POS),1)) THEN
               DO MO = 1, 12
                  IF(ACTUAL_VALUE_SPECIFIED(MO,YR, &
                                                VARIABLE,CLASS_POS))THEN
                     CONSOLIDATED_VALUES(MO,PAYMENT_TYPE) = &
                         ACTUAL_PAYABLE_VALUES(MO,YR,VARIABLE,CLASS_POS)
                  ELSE
                     CONSOLIDATED_VALUES(MO,PAYMENT_TYPE) = &
                         CONSOLIDATED_VALUES(MO,PAYMENT_TYPE) &
                         + FILE_PAYABLE_VALUES(MO,YR,VARIABLE,CLASS_POS)
                  ENDIF
               ENDDO
            ELSE
               CONSOLIDATED_VALUES(1:,PAYMENT_TYPE) = &
                         CONSOLIDATED_VALUES(1:,PAYMENT_TYPE) &
                         + FILE_PAYABLE_VALUES(1:,YR,VARIABLE,CLASS_POS)
            ENDIF
            CONSOLIDATED_VALUES(0,PAYMENT_TYPE) = &
                               SUM(CONSOLIDATED_VALUES(1:,PAYMENT_TYPE))
         ENDDO
      RETURN
! ***********************************************************************
      ENTRY MONTHLY_PAYABLES_4(R_YR,PAYABLE_MONTHLY_VALUES, &
                               TEMP_PAYABLE_MONTHLY_VALUES)
! ***********************************************************************
         YR = R_YR
         DO CLASS_POS = -1, NUM_ACTIVE_CLASS
            DO VARIABLE = 1, PAYMENT_VARS
               IF(ANY(ACTUAL_VALUE_SPECIFIED(:, &
                                         YR,VARIABLE,CLASS_POS),1)) THEN
                  DO MO = 1, 12
                     IF(ACTUAL_VALUE_SPECIFIED(MO,YR, &
                                                VARIABLE,CLASS_POS))THEN
                        PAYABLE_MONTHLY_VALUES(MO,VARIABLE,CLASS_POS) = &
                           ACTUAL_PAYABLE_VALUES(MO,YR,VARIABLE, &
                                                              CLASS_POS)
                        TEMP_PAYABLE_MONTHLY_VALUES(MO,VARIABLE, &
                                                            CLASS_POS) = &
                           ACTUAL_PAYABLE_VALUES(MO,YR,VARIABLE, &
                                                              CLASS_POS)
                     ELSE
                        TEMP_PAYABLE_MONTHLY_VALUES(MO,VARIABLE, &
                                                            CLASS_POS) = &
                               TEMP_PAYABLE_MONTHLY_VALUES(MO,VARIABLE, &
                                                              CLASS_POS) &
                               + FILE_PAYABLE_VALUES(MO,YR,VARIABLE, &
                                                              CLASS_POS)
                        PAYABLE_MONTHLY_VALUES(MO,VARIABLE,CLASS_POS) = &
                               TEMP_PAYABLE_MONTHLY_VALUES(MO,VARIABLE, &
                                                              CLASS_POS)
                     ENDIF
                  ENDDO
               ELSE
!                   DO MO = 1, 12
                  PAYABLE_MONTHLY_VALUES(1:,VARIABLE,CLASS_POS) = &
                           FILE_PAYABLE_VALUES(1:,YR,VARIABLE,CLASS_POS)
                  TEMP_PAYABLE_MONTHLY_VALUES(1:,VARIABLE,CLASS_POS)= &
                      TEMP_PAYABLE_MONTHLY_VALUES(1:,VARIABLE,CLASS_POS) &
                      + PAYABLE_MONTHLY_VALUES(1:,VARIABLE,CLASS_POS)
!                   ENDDO
               ENDIF
               PAYABLE_MONTHLY_VALUES(0,VARIABLE,CLASS_POS) = &
                      SUM(PAYABLE_MONTHLY_VALUES(1:,VARIABLE,CLASS_POS))
               TEMP_PAYABLE_MONTHLY_VALUES(0,VARIABLE,CLASS_POS) = &
                 SUM(TEMP_PAYABLE_MONTHLY_VALUES(1:,VARIABLE,CLASS_POS))
            ENDDO
         ENDDO
      RETURN
      END
! ***********************************************************************
      FUNCTION GET_ACCUMULATION_PERIOD_INFO(TYPE,DONT_LAG_DECEMBER, &
                                            ACC_PERIOD)
! ***********************************************************************
!
      use mthnmcom

      INTEGER(kind=2) :: GET_ACCUMULATION_PERIOD_INFO,TYPE,MO
      LOGICAL(kind=1) :: ACC_PERIOD(12),DONT_LAG_DECEMBER,EMPIRE, &
                UI,BANGOR,WVPA,IPALCO,GreatRiver
!
         DONT_LAG_DECEMBER = .FALSE.
         ACC_PERIOD(:) = .FALSE.
!
         SELECT CASE(TYPE)
            CASE (payment_std_interest)
               ACC_PERIOD(:) = .TRUE.
               GET_ACCUMULATION_PERIOD_INFO = 1
            CASE (payment_operating_revenue_tax)
               IF(GreatRiver()) THEN
                  GET_ACCUMULATION_PERIOD_INFO = 1
                  ACC_PERIOD(3) = .TRUE.
                  ACC_PERIOD(12) = .TRUE.
               ELSE
                  ACC_PERIOD(:) = .TRUE.
                  GET_ACCUMULATION_PERIOD_INFO = 1
               ENDIF
            CASE (payment_other_taxes)
               IF(GreatRiver()) THEN
                  GET_ACCUMULATION_PERIOD_INFO = 1
                  ACC_PERIOD(3) = .TRUE.
                  ACC_PERIOD(12) = .TRUE.
               ELSE
                  ACC_PERIOD(:) = .TRUE.
                  GET_ACCUMULATION_PERIOD_INFO = 1
               ENDIF
            CASE (payment_property_taxes)
               GET_ACCUMULATION_PERIOD_INFO = 7
               IF(WVPA() .OR. IPALCO()) THEN
                  DONT_LAG_DECEMBER = .FALSE.
                  GET_ACCUMULATION_PERIOD_INFO = 11
                  ACC_PERIOD(6) = .TRUE.
                  ACC_PERIOD(12) = .TRUE.
               ELSEIF(GreatRiver()) THEN
                  GET_ACCUMULATION_PERIOD_INFO = 1
                  ACC_PERIOD(3) = .TRUE.
                  ACC_PERIOD(12) = .TRUE.
                  DONT_LAG_DECEMBER = .FALSE.
               ELSE
                  ACC_PERIOD(6) = .TRUE.
                  ACC_PERIOD(12) = .TRUE.
               ENDIF

            CASE (payment_federal_inc_taxes_paid)
               ACC_PERIOD(3) = .TRUE.
               ACC_PERIOD(5) = .TRUE.
               ACC_PERIOD(8) = .TRUE.
               ACC_PERIOD(12) = .TRUE.
               GET_ACCUMULATION_PERIOD_INFO = 1
               IF(EMPIRE() .OR. BANGOR() .OR. IPALCO()) &
                                              DONT_LAG_DECEMBER = .TRUE.
            CASE (payment_state_income_taxes_paid)
               ACC_PERIOD(3) = .TRUE.
               ACC_PERIOD(6) = .TRUE.
               ACC_PERIOD(9) = .TRUE.
               ACC_PERIOD(12) = .TRUE.
               IF(IPALCO()) THEN
                  ACC_PERIOD(3) = .TRUE.
                  ACC_PERIOD(5) = .TRUE.
                  ACC_PERIOD(8) = .TRUE.
                  ACC_PERIOD(12) = .TRUE.
               ENDIF
               GET_ACCUMULATION_PERIOD_INFO = 1
               DONT_LAG_DECEMBER = .TRUE.
            CASE (payment_state_tax_on_capital, &
                  payment_federal_tax_on_capital)
               ACC_PERIOD(3) = .TRUE.
               ACC_PERIOD(6) = .TRUE.
               ACC_PERIOD(9) = .TRUE.
               ACC_PERIOD(12) = .TRUE.
               GET_ACCUMULATION_PERIOD_INFO = 1
            CASE DEFAULT
               ACC_PERIOD(:) = .TRUE.
               GET_ACCUMULATION_PERIOD_INFO = 1
         END SELECT
      RETURN
      END

