!     Last change: MSG 8/9/2011 12:16:38 PM

      SUBROUTINE CLASS_RUN_SWITCHES_OBJECT
      use spindriftlib
      use prod_arrays_dimensions
      USE SIZECOM

      LOGICAL(kind=1) :: FIRST_RECORD_OF_TABLE,INIT_CREATE_CLASS_LIST
      INTEGER(kind=2) :: CREATE_CLASS_LIST,TABLE_NUMBER
      LOGICAL(kind=1) :: R_SWITCH_TABLE_EXISTS,SWITCH_TABLE_EXISTS,   &
                         END_OF_TABLE
      INTEGER(kind=2) :: MAX_TABLE_NUMBER

      SAVE SWITCH_TABLE_EXISTS
      INTEGER(kind=2) :: IREC,DELETE,LRECL=1024,INUNIT,   &
                SAVE_RUN_SWITCH_UNIT=441,RUN_SWITCH_UNIT,I
      INTEGER :: IOS,IOS_BASE
      CHARACTER(len=5) :: BASE_FILE_NAME,OVERLAY_FAMILY_NAME,   &
                  ASSET_SWITCHES_BASE_FILE
      CHARACTER(len=50) :: COMMENT
      CHARACTER(len=1)  :: TABLE_TYPE
      CHARACTER(len=256) :: FILE_NAME
      CHARACTER(len=256) :: BASE_FILE_DIRECTORY,OUTPUT_DIRECTORY
      CHARACTER(len=256) :: DATA_DRIVE
      CHARACTER(len=32) :: CLASS_NAME
      CHARACTER(len=156) :: ERROR_MESSAGE
      INTEGER(kind=2) :: CLASS_NUM,CLASS_RUN_SPECT_VALUES
      PARAMETER (CLASS_RUN_SPECT_VALUES=23) ! DON'T CHANGE VALUE HERE, BUT TO CHANGE BELOW=LAST VAR NUM - 4
      CHARACTER(len=20) :: ALL_VALUES(CLASS_RUN_SPECT_VALUES),   &
                  PRE_NAME_VALUES(13),   &
                  POST_COMMENT_VALUES(CLASS_RUN_SPECT_VALUES-13)
      EQUIVALENCE (ALL_VALUES(1),PRE_NAME_VALUES(1))
      EQUIVALENCE (ALL_VALUES(14),POST_COMMENT_VALUES(1))
      CHARACTER(len=20) :: DEFERRED_FUEL_ACCOUNTING,   &
                   FASB_109_ACCOUNTING,CAPITIAL_TAX,   &
                   DEFERRED_PGA_ACCOUNTING,   &
                   DEFERRED_PURCH_POWER_ACCOUNTING,   &
                   RETAIN_POST_RETIREMENT_EARNINGS,   &
                   PROPERTY_TAX_BASIS_VALUATION
      LOGICAL(kind=4) :: FILE_EXISTS,FILE_OPENED
      INTEGER(kind=2) :: BC_TABLE_POSITION_FOR(:),   &
                PASS_TABLE_POINTER,INCREMENT_TABLE_NUMBER
      ALLOCATABLE :: BC_TABLE_POSITION_FOR
      INTEGER(kind=2) :: MAX_OL_CLASS_NUMBER,MAX_BC_CLASS_NUMBER,   &
                R_CLASS_NUMBER,R_TABLE_POINTR(0:*),MAX_CLASS_IN_TABLE
      SAVE BC_TABLE_POSITION_FOR,MAX_BC_CLASS_NUMBER
      INTEGER(kind=2) :: OL_TABLE_POSITION_FOR(:)
      ALLOCATABLE :: OL_TABLE_POSITION_FOR
      SAVE OL_TABLE_POSITION_FOR,MAX_OL_CLASS_NUMBER
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER(len=1024) :: RECLN
! DECLARATION FOR FINANCIAL PARAMETER FILE
      INTEGER(kind=2) :: YEAR,DATA_RECORDS_IN_TABLE
      CHARACTER(len=20) :: FILE_TYPE='Class Run Switches'
!
      CHARACTER(len=2) :: CLASS_RUN_SWITCH_OL='BC'
      INTEGER(kind=2) :: BC_PARENT_TABLE_NUMBER=-1,   &
                BC_SUB_DEFAULT_TABLE_NUMBER=-1,   &
                BC_SBU_DEFAULT_TABLE_NUMBER=-1,   &
                BC_REG_DEFAULT_TABLE_NUMBER=-1
      INTEGER(kind=2) :: OL_PARENT_TABLE_NUMBER=-1,   &
                OL_SUB_DEFAULT_TABLE_NUMBER=-1,   &
                OL_SBU_DEFAULT_TABLE_NUMBER=-1,   &
                OL_REG_DEFAULT_TABLE_NUMBER=-1
!
      INTEGER(kind=4) :: UNIT_NUMBER
      CHARACTER(len=20) :: USE_FEDERAL_TAX_BENEFITS,   &
                   USE_STATE_TAX_BENEFITS
!
      INTEGER(kind=2) :: R_PARENT_TABLE_NUMBER,   &
                R_SUB_DEFAULT_TABLE_NUMBER,   &
                R_SBU_DEFAULT_TABLE_NUMBER,   &
                R_REG_DEFAULT_TABLE_NUMBER
      LOGICAL(kind=1) :: PARENT_TABLE_SPECIFIED,   &
                SUB_TABLE_SPECIFIED,   &
                SBU_TABLE_SPECIFIED,   &
                REG_TABLE_SPECIFIED
      LOGICAL(kind=1) :: LAHEY_LF95
      CHARACTER(len=30) :: SCREEN_OUTPUT


!
!          ROUTINE TO CONVERT METAFILE FILES TO DIRECT-ACESS BINARY
!          COPYRIGHT (C) 1983, 84, 85  M.S. GERBER & ASSOCIATES, INC.
!

!
! CONVERT THE FINANCIAL-PARAMETERS FILE

      ENTRY CLASS_RUN_SWITCHES_MAKEBIN

      BASE_FILE_NAME = ASSET_SWITCHES_BASE_FILE()
      FILE_NAME = trim(BASE_FILE_DIRECTORY())//   &
                                   "ASB"//trim(BASE_FILE_NAME)//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      DATA_DRIVE = OUTPUT_DIRECTORY()
      MAX_BC_CLASS_NUMBER = 0
      SWITCH_TABLE_EXISTS = FILE_EXISTS
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
            CALL MG_LOCATE_WRITE(16,30,BASE_FILE_NAME,ALL_VERSIONS,0)
         ENDIF
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCCLSRS.BIN",ACCESS="DIRECT",   &
              STATUS="UNKNOWN",RECL=LRECL)
         IREC = 0
         CLASS_NAME = 'Parent'
         CLASS_NUM = 0
         FIRST_RECORD_OF_TABLE = INIT_CREATE_CLASS_LIST(INT(-1,2))
         PARENT_TABLE_SPECIFIED = .FALSE.
         SUB_TABLE_SPECIFIED = .FALSE.
         SBU_TABLE_SPECIFIED = .FALSE.
         REG_TABLE_SPECIFIED = .FALSE.
!
         READ(10,*,IOSTAT=IOS) DELETE
         DO
            ALL_VALUES(1:13) = ' '
            ALL_VALUES(6)  = 'Oper'
            ALL_VALUES(10)  = '9'
            ALL_VALUES(14) = 'Burn'
            ALL_VALUES(15) = 'Not A'!ctive
            ALL_VALUES(16) = 'Amort'!ize
            ALL_VALUES(17) = 'Paren' ! Revenue Tax
            ALL_VALUES(18) = 'Paren' ! Other Tax
            ALL_VALUES(19) = 'Paren' ! Property Tax
            ALL_VALUES(20) = 'Paren' ! State Income Tax
            ALL_VALUES(21) = 'False' ! Use Federal Tax Table
            ALL_VALUES(22) = 'Opera' ! Revenue Tax Basis
            ALL_VALUES(23) = 'No' ! Turn OFF Environmental Tax
            DEFERRED_FUEL_ACCOUNTING = 'N'!o
            FASB_109_ACCOUNTING = 'N'!o
            DEFERRED_PGA_ACCOUNTING = 'N'!o
            DEFERRED_PURCH_POWER_ACCOUNTING = 'N'!o
            RETAIN_POST_RETIREMENT_EARNINGS = 'Y'
            PROPERTY_TAX_BASIS_VALUATION = 'A'
            DATA_RECORDS_IN_TABLE = 0
            FIRST_RECORD_OF_TABLE = .TRUE.
            CAPITIAL_TAX = 'Paren'
            TABLE_TYPE = 'C'
            DO
               READ(10,1000,IOSTAT=IOS) RECLN
               IF(IOS /= 0) EXIT
               IF(RECLN(1:1) == '7') EXIT
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,'//   &
                                      ',,,,,,,,,,,,,,,,,,,,,,,'//   &
                                      ',,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*) DELETE,YEAR,PRE_NAME_VALUES,   &
                                     CLASS_NAME,CLASS_NUM,COMMENT,   &
                                     POST_COMMENT_VALUES,   &
                                     TABLE_TYPE,   &
                                     DEFERRED_FUEL_ACCOUNTING,   &
                                     FASB_109_ACCOUNTING,   &
                                     CAPITIAL_TAX,   &
                                     USE_FEDERAL_TAX_BENEFITS,   &
                                     USE_STATE_TAX_BENEFITS,   &
                                     DEFERRED_PGA_ACCOUNTING,   &
                                     DEFERRED_PURCH_POWER_ACCOUNTING,   &
                                     RETAIN_POST_RETIREMENT_EARNINGS,   &
                                     PROPERTY_TAX_BASIS_VALUATION
!
! WRITE RECORD
!
               IREC = IREC + 1
               WRITE(11,REC=IREC) DELETE,YEAR,CLASS_NAME,CLASS_NUM,   &
                                  TABLE_TYPE,   &
                                  ALL_VALUES,   &
                                  DEFERRED_FUEL_ACCOUNTING,   &
                                  FASB_109_ACCOUNTING,   &
                                  CAPITIAL_TAX,   &
                                  USE_FEDERAL_TAX_BENEFITS,   &
                                  USE_STATE_TAX_BENEFITS,   &
                                  DEFERRED_PGA_ACCOUNTING,   &
                                  DEFERRED_PURCH_POWER_ACCOUNTING,   &
                                  RETAIN_POST_RETIREMENT_EARNINGS,   &
                                  PROPERTY_TAX_BASIS_VALUATION
              IF(FIRST_RECORD_OF_TABLE) THEN
                  FIRST_RECORD_OF_TABLE = .FALSE.
                  IF(DELETE > 7) THEN
                     TABLE_NUMBER = INCREMENT_TABLE_NUMBER()
                  ELSEIF(INDEX('ASBRNP',TABLE_TYPE) == 0) THEN
                     TABLE_NUMBER = CREATE_CLASS_LIST(CLASS_NUM)
                  ELSE
                     TABLE_NUMBER = INCREMENT_TABLE_NUMBER()
                     IF((TABLE_TYPE == 'P' .OR. TABLE_TYPE == 'A') .AND.   &
                                      .NOT. PARENT_TABLE_SPECIFIED) THEN
                        IF(TABLE_TYPE=='P')PARENT_TABLE_SPECIFIED=.TRUE.
                        BC_PARENT_TABLE_NUMBER = TABLE_NUMBER
                     ENDIF
                     IF((TABLE_TYPE == 'S' .OR. TABLE_TYPE == 'A') .AND.   &
                                         .NOT. SUB_TABLE_SPECIFIED) THEN
                        IF(TABLE_TYPE=='S') SUB_TABLE_SPECIFIED = .TRUE.
                        BC_SUB_DEFAULT_TABLE_NUMBER = TABLE_NUMBER
                     ENDIF
                     IF((TABLE_TYPE == 'B' .OR. TABLE_TYPE == 'A') .AND.   &
                                         .NOT. SBU_TABLE_SPECIFIED) THEN
                        IF(TABLE_TYPE=='B') SBU_TABLE_SPECIFIED = .TRUE.
                        BC_SBU_DEFAULT_TABLE_NUMBER = TABLE_NUMBER
                     ENDIF
                     IF((TABLE_TYPE == 'R' .OR. TABLE_TYPE == 'A') .AND.   &
                                         .NOT. REG_TABLE_SPECIFIED) THEN
                        IF(TABLE_TYPE=='R') REG_TABLE_SPECIFIED = .TRUE.
                        BC_REG_DEFAULT_TABLE_NUMBER = TABLE_NUMBER
                     ENDIF
                  ENDIF
               ENDIF
               DATA_RECORDS_IN_TABLE = DATA_RECORDS_IN_TABLE + 1
            ENDDO
            IF(DATA_RECORDS_IN_TABLE < AVAIL_DATA_YEARS) THEN
               DOWHILE (DATA_RECORDS_IN_TABLE < AVAIL_DATA_YEARS)
                  IREC = IREC + 1
                  WRITE(11,REC=IREC) DELETE,YEAR,   &
                                     CLASS_NAME,CLASS_NUM,ALL_VALUES
                  DATA_RECORDS_IN_TABLE = DATA_RECORDS_IN_TABLE + 1
               ENDDO
            ENDIF
            IF(IOS /= 0) EXIT
         ENDDO
         MAX_BC_CLASS_NUMBER = MAX_CLASS_IN_TABLE()
         ALLOCATE(BC_TABLE_POSITION_FOR(0:MAX_BC_CLASS_NUMBER))
         MAX_TABLE_NUMBER = PASS_TABLE_POINTER(BC_TABLE_POSITION_FOR)
         CLOSE(10)
      ELSE IF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "cls_runs:0001")
      ELSE
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCCLSRS.BIN",ACCESS="DIRECT",   &
                                            STATUS="UNKNOWN",RECL=LRECL)
      ENDIF
      CLOSE(11)
      RETURN


      ENTRY CLASS_RUN_SWITCHES_MAKEOVL(OVERLAY_FAMILY_NAME)


      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      FILE_NAME = trim(OUTPUT_DIRECTORY())//   &
                              "ASO"//trim(OVERLAY_FAMILY_NAME)//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      OPEN(10,FILE=FILE_NAME)
      INUNIT = 12
      IF(CLASS_RUN_SWITCH_OL == 'BC') THEN
         INUNIT = 11
         FILE_NAME = trim(OUTPUT_DIRECTORY())//"BCCLSRS.BIN"
         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
         IF(FILE_EXISTS) THEN
            OPEN(11,FILE= FILE_NAME,ACCESS="DIRECT",RECL=LRECL)
         ELSE
            CALL STOP_NO_BASE_FILE(FILE_TYPE)
         ENDIF
      ENDIF
      INQUIRE(UNIT=12,OPENED=FILE_OPENED)
      IF(FILE_OPENED) CLOSE(12)
      INQUIRE(FILE=trim(OUTPUT_DIRECTORY())//"OLCLSRS.BIN",   &
                                  OPENED=FILE_OPENED,NUMBER=UNIT_NUMBER)
      IF(FILE_OPENED) CLOSE(UNIT_NUMBER)
      OPEN(12,FILE=trim(OUTPUT_DIRECTORY())//"OLCLSRS.BIN",   &
                            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         PARENT_TABLE_SPECIFIED = .FALSE.
         SUB_TABLE_SPECIFIED = .FALSE.
         SBU_TABLE_SPECIFIED = .FALSE.
         REG_TABLE_SPECIFIED = .FALSE.
         IREC = 0
         OL_PARENT_TABLE_NUMBER = -1
         OL_SUB_DEFAULT_TABLE_NUMBER = -1
         OL_SBU_DEFAULT_TABLE_NUMBER = -1
         OL_REG_DEFAULT_TABLE_NUMBER = -1
         FIRST_RECORD_OF_TABLE = INIT_CREATE_CLASS_LIST(INT(-1,2))
!
         READ(10,*,IOSTAT=IOS) DELETE
         DO
            DO I = 1, CLASS_RUN_SPECT_VALUES
               ALL_VALUES(I) = ' '
            ENDDO
            FIRST_RECORD_OF_TABLE = .TRUE.
            END_OF_TABLE = .FALSE.
            DO I = 1, AVAIL_DATA_YEARS
!
               IF(.NOT. END_OF_TABLE) THEN
                  READ(10,1000,IOSTAT=IOS) RECLN
                  IF(RECLN(1:1) == '7') THEN
                     IF(FIRST_RECORD_OF_TABLE) THEN
                        READ(10,1000,IOSTAT=IOS) RECLN
                     ELSE
                        END_OF_TABLE = .TRUE.
                     ENDIF
                  ENDIF
               ENDIF
               IREC = IREC + 1
               READ(INUNIT,REC=IREC,IOSTAT=IOS_BASE) DELETE,YEAR,   &
                                        CLASS_NAME,CLASS_NUM,   &
                                        TABLE_TYPE,   &
                                        ALL_VALUES,   &
                                        DEFERRED_FUEL_ACCOUNTING,   &
                                        FASB_109_ACCOUNTING,   &
                                        CAPITIAL_TAX,   &
                                        USE_FEDERAL_TAX_BENEFITS,   &
                                        USE_STATE_TAX_BENEFITS,   &
                                        DEFERRED_PGA_ACCOUNTING,   &
                                        DEFERRED_PURCH_POWER_ACCOUNTING,   &
                                        RETAIN_POST_RETIREMENT_EARNINGS,   &
                                        PROPERTY_TAX_BASIS_VALUATION
               IF(IOS_BASE /= 0) EXIT
               IF(IOS == 0 .AND. .NOT. END_OF_TABLE) THEN
                  RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,'//   &
                                              ',,,,,,,,,,,,,,,,,,,,,,,,'
                  READ(RECLN,*) DELETE,YEAR,PRE_NAME_VALUES,   &
                                     CLASS_NAME,CLASS_NUM,COMMENT,   &
                                     POST_COMMENT_VALUES,TABLE_TYPE,   &
                                     DEFERRED_FUEL_ACCOUNTING,   &
                                     FASB_109_ACCOUNTING,   &
                                     CAPITIAL_TAX,   &
                                     USE_FEDERAL_TAX_BENEFITS,   &
                                     USE_STATE_TAX_BENEFITS,   &
                                     DEFERRED_PGA_ACCOUNTING,   &
                                     DEFERRED_PURCH_POWER_ACCOUNTING,   &
                                     RETAIN_POST_RETIREMENT_EARNINGS,   &
                                     PROPERTY_TAX_BASIS_VALUATION
               ENDIF
!
! WRITE RECORD
!
               WRITE(12,REC=IREC) DELETE,YEAR,CLASS_NAME,CLASS_NUM,   &
                                  TABLE_TYPE,ALL_VALUES,   &
                                  DEFERRED_FUEL_ACCOUNTING,   &
                                  FASB_109_ACCOUNTING,   &
                                  CAPITIAL_TAX,   &
                                  USE_FEDERAL_TAX_BENEFITS,   &
                                  USE_STATE_TAX_BENEFITS,   &
                                  DEFERRED_PGA_ACCOUNTING,   &
                                  DEFERRED_PURCH_POWER_ACCOUNTING,   &
                                  RETAIN_POST_RETIREMENT_EARNINGS,   &
                                  PROPERTY_TAX_BASIS_VALUATION
               IF(FIRST_RECORD_OF_TABLE) THEN
                  FIRST_RECORD_OF_TABLE = .FALSE.
                  IF(DELETE > 7) THEN
                     TABLE_NUMBER = INCREMENT_TABLE_NUMBER()
                  ELSEIF(INDEX('ASBRNP',TABLE_TYPE) == 0) THEN
                     TABLE_NUMBER = CREATE_CLASS_LIST(CLASS_NUM)
                  ELSE
                     TABLE_NUMBER = INCREMENT_TABLE_NUMBER()
                     IF((TABLE_TYPE == 'P' .OR. TABLE_TYPE == 'A') .AND.   &
                                      .NOT. PARENT_TABLE_SPECIFIED) THEN
                        IF(TABLE_TYPE=='P')PARENT_TABLE_SPECIFIED=.TRUE.
                        OL_PARENT_TABLE_NUMBER = TABLE_NUMBER
                     ENDIF
                     IF((TABLE_TYPE == 'S' .OR. TABLE_TYPE == 'A') .AND.   &
                                         .NOT. SUB_TABLE_SPECIFIED) THEN
                        IF(TABLE_TYPE=='S') SUB_TABLE_SPECIFIED = .TRUE.
                        OL_SUB_DEFAULT_TABLE_NUMBER = TABLE_NUMBER
                     ENDIF
                     IF((TABLE_TYPE == 'B' .OR. TABLE_TYPE == 'A') .AND.   &
                                         .NOT. SBU_TABLE_SPECIFIED) THEN
                        IF(TABLE_TYPE=='B') SBU_TABLE_SPECIFIED = .TRUE.
                        OL_SBU_DEFAULT_TABLE_NUMBER = TABLE_NUMBER
                     ENDIF
                     IF((TABLE_TYPE == 'R' .OR. TABLE_TYPE == 'A') .AND.   &
                                         .NOT. REG_TABLE_SPECIFIED) THEN
                        IF(TABLE_TYPE=='R') REG_TABLE_SPECIFIED = .TRUE.
                        OL_REG_DEFAULT_TABLE_NUMBER = TABLE_NUMBER
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO
            IF(IOS_BASE /= 0) EXIT
         ENDDO
         CLOSE(10)
         CLOSE(12)
         IF(ALLOCATED(OL_TABLE_POSITION_FOR))   &
                                       DEALLOCATE(OL_TABLE_POSITION_FOR)
         MAX_OL_CLASS_NUMBER = MAX_CLASS_IN_TABLE()
         ALLOCATE(OL_TABLE_POSITION_FOR(0:MAX_OL_CLASS_NUMBER))
         MAX_TABLE_NUMBER = PASS_TABLE_POINTER(OL_TABLE_POSITION_FOR)
      IF(CLASS_RUN_SWITCH_OL == 'BC') CLOSE(11)
      CLASS_RUN_SWITCH_OL = 'OL'
      RETURN


      ENTRY RESET_CLASS_RUN_SWITCH_OL

         CLASS_RUN_SWITCH_OL = 'BC'
      RETURN



      ENTRY OPEN_CLASS_RUN_SWITCH_FILE(RUN_SWITCH_UNIT)

         SAVE_RUN_SWITCH_UNIT = RUN_SWITCH_UNIT
         CLOSE(RUN_SWITCH_UNIT,IOSTAT=IOS)
         OPEN(RUN_SWITCH_UNIT,FILE=trim(OUTPUT_DIRECTORY())//   &
            CLASS_RUN_SWITCH_OL//"CLSRS.BIN",ACCESS="DIRECT",RECL=LRECL)
      RETURN


      ENTRY CLOSE_CLASS_RUN_SWITCH_FILE

         CLOSE(SAVE_RUN_SWITCH_UNIT,IOSTAT=IOS)
      RETURN


      ENTRY RETURN_CLS_RUN_SWITCH_TABLE_NUM(R_CLASS_NUMBER,   &
                                            R_SWITCH_TABLE_EXISTS)


         R_SWITCH_TABLE_EXISTS = SWITCH_TABLE_EXISTS
         IF(CLASS_RUN_SWITCH_OL == 'OL') THEN
            R_CLASS_NUMBER = MAX_OL_CLASS_NUMBER
         ELSE
            R_CLASS_NUMBER = MAX_BC_CLASS_NUMBER
         ENDIF
      RETURN


      ENTRY RETURN_CLASS_RUN_SWITCH_INFO(R_TABLE_POINTR,   &
                                         R_PARENT_TABLE_NUMBER,   &
                                         R_SUB_DEFAULT_TABLE_NUMBER,   &
                                         R_SBU_DEFAULT_TABLE_NUMBER,   &
                                         R_REG_DEFAULT_TABLE_NUMBER)

!
         IF(CLASS_RUN_SWITCH_OL == 'OL') THEN
            R_PARENT_TABLE_NUMBER = OL_PARENT_TABLE_NUMBER
            R_SUB_DEFAULT_TABLE_NUMBER = OL_SUB_DEFAULT_TABLE_NUMBER
            R_SBU_DEFAULT_TABLE_NUMBER = OL_SBU_DEFAULT_TABLE_NUMBER
            R_REG_DEFAULT_TABLE_NUMBER = OL_REG_DEFAULT_TABLE_NUMBER
            DO I = 1, MAX_OL_CLASS_NUMBER
               R_TABLE_POINTR(I) = OL_TABLE_POSITION_FOR(I)
            ENDDO
         ELSE
            R_PARENT_TABLE_NUMBER = BC_PARENT_TABLE_NUMBER
            R_SUB_DEFAULT_TABLE_NUMBER = BC_SUB_DEFAULT_TABLE_NUMBER
            R_SBU_DEFAULT_TABLE_NUMBER = BC_SBU_DEFAULT_TABLE_NUMBER
            R_REG_DEFAULT_TABLE_NUMBER = BC_REG_DEFAULT_TABLE_NUMBER
            DO I = 1, MAX_BC_CLASS_NUMBER
               R_TABLE_POINTR(I) = BC_TABLE_POSITION_FOR(I)
            ENDDO
         ENDIF
      RETURN
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END


!
!                  INIT_CLASS_RUN_SWITCH_FILE
!        Copyright (c) 1995 M.S. Gerber & Associates, Inc.
!                      All Rights Reserved
!

!     Purpose:  This subroutine reads the asset class
!               run switch file for each class by year.
!

!
      SUBROUTINE INIT_CLASS_RUN_SWITCH_FILE(MAX_CLASS_NUM)

!
      use spindriftlib
      use prod_arrays_dimensions
      USE FINANCIAL_RUN_SWITCHES
      USE FINANCIAL_SWITCHES_COMMON
      USE SIZECOM
      use class_run_switchesc
      use class_run_switchesl1
      use class_run_switchesl4


      INTEGER(kind=2) :: CLASS_RUN_SPECT_VALUES


      PARAMETER (CLASS_RUN_SPECT_VALUES=32) ! CHANGE=LAST VAR NUM - 5
      CHARACTER(len=20) :: SWITCH_SETTINGS(CLASS_RUN_SPECT_VALUES)
      CHARACTER(len=32) :: CLASS_NAME
      CHARACTER(len=1)  :: CLASS_TYPE,TABLE_TYPE
      LOGICAL(kind=1) :: SWITCH_FILE_TABLES_ACTIVE,R_CALCULATE_AMT,   &
                R_USE_BUDGET_EXPENSES
      INTEGER(kind=2) :: YEAR,CLASS,MAX_CLASS_NUM,RUN_SWITCH_UNIT=441
      INTEGER(kind=2) :: MAX_SWITCH_FILE_CLASS_NUMBER,   &
                CLASS_TABLE_POINTER(:),CLASS_TABLE_NUM,   &
                CLASS_NUM,IREC,DELETE,R_COVERAGE_RATIO
      ALLOCATABLE :: CLASS_TABLE_POINTER
      SAVE CLASS_TABLE_POINTER,SWITCH_FILE_TABLES_ACTIVE
      LOGICAL(kind=1) :: R_PASS_SBU_REVENUE_TAX,   &
                R_PASS_SBU_OTHER_TAX,   &
                R_PASS_SBU_PROPERTY_TAX,   &
                R_PASS_SBU_STATE_INCOME_TAX,   &
                R_CALC_ENVIRONMENTAL_TAX,   &
                R_PASS_SBU_CAPITIAL_TAX,   &
                R_USE_RETAIN_POST_RETRMNT_ERNGS
      LOGICAL (KIND=1) , SAVE :: PASS_SBU_REVENUE_TAX,   &
                PASS_SBU_OTHER_TAX,   &
                PASS_SBU_PROPERTY_TAX,   &
                PASS_SBU_STATE_INCOME_TAX,   &
                CALC_ENVIRONMENTAL_TAX,   &
                PASS_SBU_CAPITIAL_TAX,   &
                USE_RETAIN_POST_RETRMNT_ERNGS
      CHARACTER(len=1) :: REVENUE_TAX_BASIS,R_REVENUE_TAX_BASIS
      INTEGER (KIND=2) , SAVE :: PARENT_TABLE_NUMBER,   &
               SUB_DEFAULT_TABLE_NUMBER,   &
               SBU_DEFAULT_TABLE_NUMBER,   &
               REG_DEFAULT_TABLE_NUMBER
      LOGICAL(kind=1) :: USE_APPRAISED_PROP_VALUES,   &
                         R_USE_APPRAISED_PROP_VALUES
      SAVE USE_APPRAISED_PROP_VALUES
!
         CALL RETURN_CLS_RUN_SWITCH_TABLE_NUM(   &
                                           MAX_SWITCH_FILE_CLASS_NUMBER,   &
                                           SWITCH_FILE_TABLES_ACTIVE)
         IF(SWITCH_FILE_TABLES_ACTIVE) THEN
            IF(ALLOCATED(CLASS_TABLE_POINTER))   &
                                         DEALLOCATE(CLASS_TABLE_POINTER)
            MAX_SWITCH_FILE_CLASS_NUMBER =   &
                         MAX(MAX_SWITCH_FILE_CLASS_NUMBER,MAX_CLASS_NUM)
            ALLOCATE(   &
                    CLASS_TABLE_POINTER(0:MAX_SWITCH_FILE_CLASS_NUMBER))
            CLASS_TABLE_POINTER = -1
            CALL RETURN_CLASS_RUN_SWITCH_INFO(CLASS_TABLE_POINTER,   &
                                              PARENT_TABLE_NUMBER,   &
                                              SUB_DEFAULT_TABLE_NUMBER,   &
                                              SBU_DEFAULT_TABLE_NUMBER,   &
                                              REG_DEFAULT_TABLE_NUMBER)
            CALL OPEN_CLASS_RUN_SWITCH_FILE(RUN_SWITCH_UNIT)
         ENDIF
      RETURN


      ENTRY READ_CLASS_RUN_SWITCHES(CLASS,YEAR,CLASS_TYPE)

!
! READ CLASS SWITCH FILE
!
         IF(SWITCH_FILE_TABLES_ACTIVE) THEN
            CLASS_TABLE_NUM = CLASS_TABLE_POINTER(CLASS)
            IF(CLASS_TABLE_POINTER(CLASS) < 1) THEN
               IF(CLASS_TYPE == 'P') CLASS_TABLE_NUM =   &
                                                     PARENT_TABLE_NUMBER
               IF(CLASS_TYPE == 'S') CLASS_TABLE_NUM =   &
                                                SUB_DEFAULT_TABLE_NUMBER
               IF(CLASS_TYPE == 'B') CLASS_TABLE_NUM =   &
                                                SBU_DEFAULT_TABLE_NUMBER
               IF(CLASS_TYPE == 'R') CLASS_TABLE_NUM =   &
                                                REG_DEFAULT_TABLE_NUMBER
               CLASS_TABLE_NUM = MAX(CLASS_TABLE_NUM,INT(1,2))
            ENDIF
            IREC = (CLASS_TABLE_NUM-1) * AVAIL_DATA_YEARS +   &
                                                  MIN(YEAR,INT(30,2))
!
            READ(RUN_SWITCH_UNIT,REC=IREC) DELETE,DELETE,   &
                                           CLASS_NAME,CLASS_NUM,   &
                                           TABLE_TYPE,   &
                                           SWITCH_SETTINGS
            AFUDC_RETURN_POLICY = SWITCH_SETTINGS(1)(1:1)
            AFUDC_RETURN_POLICY = "1"
            DIVIDEND_PAYMENT_METHOD = SWITCH_SETTINGS(2)(1:1)
            IF(INDEX(SWITCH_SETTINGS(2),'ash') /= 0 .AND.   &
                                    DIVIDEND_PAYMENT_METHOD == 'S') THEN
               DIVIDEND_PAYMENT_METHOD = 'H'
            ENDIF
            RETURN_ON_RATEBASE_SOURCE = SWITCH_SETTINGS(3)(1:1)
            NORMALIZE_FEDERAL_TAXES = SWITCH_SETTINGS(4)(1:1) == 'N'
            NORMALIZE_STATE_TAXES = SWITCH_SETTINGS(5)(1:1) == 'N'
            OPERATING_METHOD = SWITCH_SETTINGS(6)(1:1)
            PRICE_SOURCE_FOR_PRICE_DRIVER = SWITCH_SETTINGS(7)(1:1)
            RATEBASE_VALUATION = SWITCH_SETTINGS(8)(1:1)
            PROPERTY_TAX_METHOD = 1
            IF(INDEX(SWITCH_SETTINGS(9),'GPV') /= 0) THEN
               PROPERTY_TAX_METHOD = 2
               IF(INDEX(SWITCH_SETTINGS(9),'+')/=0)PROPERTY_TAX_METHOD=1
            ELSE
               PROPERTY_TAX_METHOD = 4
               IF(INDEX(SWITCH_SETTINGS(9),'+')/=0)PROPERTY_TAX_METHOD=3
            ENDIF
            USE_APPRAISED_PROP_VALUES = SWITCH_SETTINGS(9)(1:1) == 'A'
!
            IF(TRIM(SWITCH_SETTINGS(10)) == '') THEN
               COVERAGE_RATIO = 5
            ELSE
               READ(SWITCH_SETTINGS(10)(1:2),*) COVERAGE_RATIO
            ENDIF
!
            MORTGAGE_DEBT = SWITCH_SETTINGS(11)(1:1) == 'M'
            CALCULATE_BTL_INCOME_TAXES = SWITCH_SETTINGS(12)(1:1) == 'Y'
            CALCULATE_AMT  = SWITCH_SETTINGS(13)(1:1) == 'N'
            USE_BURN_4_NUC_FUEL_TAX_EXP = SWITCH_SETTINGS(14)(1:1)=='B'
            USE_NF_BURN_4_EXPENSE = SWITCH_SETTINGS(16)(1:1)=='B'
            EQUITY_DEFINITION = 'A'
            USE_BUDGET_VARIABLE_OM = SWITCH_SETTINGS(15)(1:1)=='R'& ! eplace
                                .OR. SWITCH_SETTINGS(15)(1:1)=='A'!ll Items
            USE_BUDGET_FUEL_PURCHASE = SWITCH_SETTINGS(15)(1:1)=='A'!ll Items
            USE_PRODUCTION_MODULE_EXPENSE=SWITCH_SETTINGS(15)(1:1)=='P'!roduction Expense Only
            PASS_SBU_REVENUE_TAX = SWITCH_SETTINGS(17)(1:1) == 'S'
            PASS_SBU_OTHER_TAX = SWITCH_SETTINGS(18)(1:1) == 'S'
            PASS_SBU_PROPERTY_TAX = SWITCH_SETTINGS(19)(1:1) == 'S'
            PASS_SBU_STATE_INCOME_TAX = SWITCH_SETTINGS(20)(1:1) == 'S'
            USE_FED_TAX_TABLE = SWITCH_SETTINGS(21)(1:1) == 'T'
            REVENUE_TAX_BASIS = SWITCH_SETTINGS(22)(1:1)
            CALC_ENVIRONMENTAL_TAX=.NOT.(SWITCH_SETTINGS(23)(1:1)=='Y')
            USE_DEFERRED_FUEL_ACCOUNTING = SWITCH_SETTINGS(24)(1:1)=='Y'
            USE_FASB_109_ACCOUNTING = SWITCH_SETTINGS(25)(1:1)=='Y'
            PASS_SBU_CAPITIAL_TAX = SWITCH_SETTINGS(26)(1:1) == 'S'
            USE_ALL_FED_TAX_BENEFITS = SWITCH_SETTINGS(27)(1:1) == 'Y'
            USE_STATE_TAX_BENEFITS_NOW = SWITCH_SETTINGS(28)(1:1) == 'Y'
            USE_PGA_ACCOUNTING = SWITCH_SETTINGS(29)(1:1) == 'Y'
            USE_PURCHASE_POWER_ACCOUNTING=SWITCH_SETTINGS(30)(1:1)=='Y'
            USE_RETAIN_POST_RETRMNT_ERNGS =   &
                                           SWITCH_SETTINGS(31)(1:1)=='Y'
            PROPERTY_TAX_BASIS_VALUATION = SWITCH_SETTINGS(32)(1:1)
         ELSEIF(YEAR ==1) THEN
            AFUDC_RETURN_POLICY = '1'
            DIVIDEND_PAYMENT_METHOD = 'R'
            RETURN_ON_RATEBASE_SOURCE = 'P'
            NORMALIZE_FEDERAL_TAXES = .TRUE.
            NORMALIZE_STATE_TAXES = .TRUE.
            USE_APPRAISED_PROP_VALUES = .FALSE.
            OPERATING_METHOD = 'O'
            PRICE_SOURCE_FOR_PRICE_DRIVER = 'C'
            RATEBASE_VALUATION = 'A'
            PROPERTY_TAX_METHOD = 1
            COVERAGE_RATIO = 1
            MORTGAGE_DEBT = .FALSE.
            CALCULATE_BTL_INCOME_TAXES = .TRUE.
            CALCULATE_AMT  = .TRUE.
            EQUITY_DEFINITION = 'A'
            USE_BURN_4_NUC_FUEL_TAX_EXP = .TRUE.
            USE_BUDGET_VARIABLE_OM = .FALSE.
            USE_BUDGET_FUEL_PURCHASE = .FALSE.
            USE_PRODUCTION_MODULE_EXPENSE = .FALSE.
            USE_NF_BURN_4_EXPENSE = .FALSE.
            PASS_SBU_REVENUE_TAX = .FALSE.
            PASS_SBU_CAPITIAL_TAX = .FALSE.
            PASS_SBU_OTHER_TAX = .FALSE.
            PASS_SBU_PROPERTY_TAX = .FALSE.
            PASS_SBU_STATE_INCOME_TAX = .FALSE.
            USE_FED_TAX_TABLE = .FALSE.
            REVENUE_TAX_BASIS = 'O' !perating
            CALC_ENVIRONMENTAL_TAX = .TRUE.
            USE_DEFERRED_FUEL_ACCOUNTING = .FALSE.
            USE_FASB_109_ACCOUNTING = .FALSE.
            USE_ALL_FED_TAX_BENEFITS = .FALSE.
            USE_STATE_TAX_BENEFITS_NOW = .FALSE.
            USE_PGA_ACCOUNTING = .FALSE.
            USE_PURCHASE_POWER_ACCOUNTING = .FALSE.
            USE_RETAIN_POST_RETRMNT_ERNGS = .TRUE.
            PROPERTY_TAX_BASIS_VALUATION = 'A'
         ENDIF
         IF(CLASS_TYPE == 'P') THEN
            OPMETH = OPERATING_METHOD
            PRICE_SOURCE = PRICE_SOURCE_FOR_PRICE_DRIVER
            CALCULATE_BTL_TAXES = CALCULATE_BTL_INCOME_TAXES
            DVMETH = DIVIDEND_PAYMENT_METHOD
            SRCNPV = RETURN_ON_RATEBASE_SOURCE
            ACMETH = AFUDC_RETURN_POLICY
            TXMETHF = SWITCH_SETTINGS(4)(1:1)
            TXMETHS = SWITCH_SETTINGS(5)(1:1)
            P_EQUITY_DEFINITION = 'A'
            DEBT_ISSUE_TYPE = SWITCH_SETTINGS(11)(1:1)
            RBMETH = 1
         ENDIF
      RETURN


      ENTRY RETURN_SBU_PASS_THROUGH_VALUES(R_PASS_SBU_REVENUE_TAX,   &
                                           R_PASS_SBU_OTHER_TAX,   &
                                           R_PASS_SBU_PROPERTY_TAX,   &
                                           R_PASS_SBU_STATE_INCOME_TAX,   &
                                           R_PASS_SBU_CAPITIAL_TAX)

!
            R_PASS_SBU_REVENUE_TAX = PASS_SBU_REVENUE_TAX
            R_PASS_SBU_OTHER_TAX = PASS_SBU_OTHER_TAX
            R_PASS_SBU_PROPERTY_TAX = PASS_SBU_PROPERTY_TAX
            R_PASS_SBU_STATE_INCOME_TAX = PASS_SBU_STATE_INCOME_TAX
            R_PASS_SBU_CAPITIAL_TAX = PASS_SBU_CAPITIAL_TAX
      RETURN


      ENTRY RETURN_CAL_AMT_SWITCH(R_CALCULATE_AMT)

!
         R_CALCULATE_AMT = CALCULATE_AMT
      RETURN


      ENTRY RETURN_CURRENT_COVERAGE_RATIO(R_COVERAGE_RATIO)

!
         R_COVERAGE_RATIO = COVERAGE_RATIO
      RETURN


      ENTRY RETURN_USE_BUDGET_EXPENSES(R_USE_BUDGET_EXPENSES)

!
         R_USE_BUDGET_EXPENSES = USE_BUDGET_VARIABLE_OM
      RETURN


      ENTRY RETURN_CALC_ENVIRONMENTAL_TAX(R_CALC_ENVIRONMENTAL_TAX)

!
         R_CALC_ENVIRONMENTAL_TAX = CALC_ENVIRONMENTAL_TAX
      RETURN


      ENTRY RETURN_REVENUE_TAX_BASIS(R_REVENUE_TAX_BASIS)

!
         R_REVENUE_TAX_BASIS = REVENUE_TAX_BASIS
      RETURN


      ENTRY RETURN_APPRAISED_PROP_SWITCH(R_USE_APPRAISED_PROP_VALUES)

!
         R_USE_APPRAISED_PROP_VALUES = USE_APPRAISED_PROP_VALUES
      RETURN


      ENTRY RTN_RETAIN_POST_RETRMNT_ERNGS(   &
                                  R_USE_RETAIN_POST_RETRMNT_ERNGS)

!
         R_USE_RETAIN_POST_RETRMNT_ERNGS = .true.
      RETURN
      END
!
!
