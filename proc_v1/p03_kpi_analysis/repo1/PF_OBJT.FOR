!     ******************************************************************
!     PF_OBJT.FOR
!     Copyright(c)  2000
!
!     Created: 8/5/2003 10:39:11 AM
!     Author : MARK S GERBER
!     Last change: MSG 1/9/2012 2:38:02 PM
!     ******************************************************************

      SUBROUTINE PF_OBJECT     
      use end_routine, only: end_program, er_message
C
       use SpinDriftLib
             use prod_arrays_dimensions
      use sizecom
      LOGICAL*1 INIT_CREATE_CLASS_LIST,
     +          FIRST_RECORD_OF_TABLE,END_OF_TABLE
      INTEGER*2 CREATE_CLASS_LIST,TABLE_NUMBER
      INTEGER*2 MAX_CLASS_IN_TABLE,PASS_TABLE_POINTER
      INTEGER*2 INUNIT,IREC,DELETE,
     +          SAVE_PARM_UNIT/41/,PARM_UNIT,I,
     +          LAST_PARM_RECORD/0/,R_LAST_PARM_RECORD
      INTEGER*4 IOS,IOS_BASE
      CHARACTER*5 BASE_FILE_NAME,OVERLAY_FAMILY_NAME,PARMFIN
      CHARACTER*50 COMMENT,PARM_COMMENT
      CHARACTER*256 FILE_NAME
      CHARACTER*256 BASE_FILE_DIRECTORY,OUTPUT_DIRECTORY
      CHARACTER*256 DATA_DRIVE
      CHARACTER*32 CLASS_NAME
      INTEGER*2 CLASS_NUM
C     REAL R_PASS_THOURGH_VALUES(*)
      INTEGER*2 LAST_VAR_NUM,LRECL
      REAL (KIND=4) :: RB_INCLUDED_DEF_TAXES_DR_RATE, ! 155
     +                 RB_EXCLUDED_PENSION_LIABILITY_RATE, ! 156
     +                 RB_EXCLUDED_Deferred_Gain_RATE, ! 157
     +                 RB_EXCLUDED_Storm_Reserve_RATE, ! 158
     +                 RB_EXCLUDED_Accrued_Vacation_Pay_RATE, !159
     +                 RB_INCLUDED_DEFERRED_REVENUES_RATE,  ! 160
     +                 ODEC_MaxEquityRatio, ! 161
     +                 ODEC_MinPayoutRatio, ! 162
     +                 ODEC_MaxPayoutRatio, ! 163
     +                 ODEC_PercentREReduction, ! 164
     +                 ODEC_DollarREReduction  ! 165
C
C TO ADD VARIABLES TO THE MAKEBIN AND MAKEOVL FUNCTIONS SET LAST_VAR_NUM
C TO THE LAST NUMBER USED IN THE FINANCIAL PARAMETERS FILE.
C
      PARAMETER (LAST_VAR_NUM=149,
     +           LRECL=50+4*(LAST_VAR_NUM-4)+16+24+20) ! for quarterly dividends & RATE BASE RATES
      REAL PARM_VALUES(LAST_VAR_NUM-4),
     +     PRE_COMMENT_PARM_VALUES(62),
     +     POST_COMMENT_PARM_VALUES(60),
     +     POST_CLASS_NUM_VALUES(23) ! (LAST_VAR_NUM-126) last_var_num was 149 
      EQUIVALENCE (PARM_VALUES(1),PRE_COMMENT_PARM_VALUES(1)),
     +            (PARM_VALUES(63),POST_COMMENT_PARM_VALUES(1)),
     +            (PARM_VALUES(123),POST_CLASS_NUM_VALUES(1))
      LOGICAL*4 FILE_EXISTS,FILE_OPEN
      INTEGER*2 BC_TABLE_POSITION_FOR(:),
     +          BC_TABLE_NUMBER,OL_TABLE_POSITION_FOR(:),
     +          OL_TABLE_NUMBER
      ALLOCATABLE :: BC_TABLE_POSITION_FOR,
     +               OL_TABLE_POSITION_FOR
      INTEGER*2 MAX_OL_CLASS_NUMBER,MAX_BC_CLASS_NUMBER,R_CLASS_NUMBER,
     +          R_TABLE_POINTR(0:*)
      SAVE BC_TABLE_POSITION_FOR,BC_TABLE_NUMBER,MAX_BC_CLASS_NUMBER
      SAVE OL_TABLE_POSITION_FOR,OL_TABLE_NUMBER,MAX_OL_CLASS_NUMBER
C DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER*2048 RECLN,TABLE_TYPE*1
C DECLARATION FOR FINANCIAL PARAMETER FILE
      INTEGER*2 YEAR,DATA_RECORDS_IN_TABLE,INCREMENT_TABLE_NUMBER
      REAL (KIND=4), SAVE :: SINKING_FUND_PATTERN(AVAIL_DATA_YEARS)
      REAL (KIND=4) :: R_SINKING_FUND_PATTERN(*)
      LOGICAL*1 SALT_RIVER_PROJECT,THIS_IS_THE_FIRST_TABLE,
     +          TABLE_IS_NOT_ACTIVE
      REAL*4 FEDERAL_TAX_RATE(AVAIL_DATA_YEARS),
     +       STATE_TAX_RATE(AVAIL_DATA_YEARS),
     +       R_FEDERAL_TAX_RATE,R_STATE_TAX_RATE
      INTEGER*2 R_YEAR
      SAVE FEDERAL_TAX_RATE,STATE_TAX_RATE
      CHARACTER*20 FILE_TYPE/'Financial Parameters'/
      LOGICAL*1 ORLANDO_UC,OUC_ACTIVE
      REAL*4 QUARTERLY_COMMON_DIVIDENDS(4)
C
C MUNICIPAL FINANCIAL PARAMETER FILE DATA
C
      INTEGER*2 MUNI_LRECL/320/
C
      REAL MUNI_PARM_VALUES(75)
      CHARACTER*20 MUNI_FILE_TYPE/'Municipal Parameters'/
C
      CHARACTER*2 PARMFNOL/'BC'/
      INTEGER*2 BC_PARENT_TABLE_NUMBER/-1/,
     +          BC_SUB_DEFAULT_TABLE_NUMBER/-1/,
     +          BC_SBU_DEFAULT_TABLE_NUMBER/-1/,
     +          BC_REG_DEFAULT_TABLE_NUMBER/-1/
      INTEGER*2 OL_PARENT_TABLE_NUMBER/-1/,
     +          OL_SUB_DEFAULT_TABLE_NUMBER/-1/,
     +          OL_SBU_DEFAULT_TABLE_NUMBER/-1/,
     +          OL_REG_DEFAULT_TABLE_NUMBER/-1/
      INTEGER*2 R_PARENT_TABLE_NUMBER,
     +          R_SUB_DEFAULT_TABLE_NUMBER,
     +          R_SBU_DEFAULT_TABLE_NUMBER,
     +          R_REG_DEFAULT_TABLE_NUMBER
      LOGICAL*1 LAHEY_LF95
      CHARACTER*30 SCREEN_OUTPUT
***********************************************************************
C
C          ROUTINE TO CONVERT METAFILE FILES TO DIRECT-ACESS BINARY
C          COPYRIGHT (C) 1983, 84, 85  M.S. GERBER & ASSOCIATES, INC.
C
C***********************************************************************
C
C CONVERT THE FINANCIAL-PARAMETERS FILE
C***********************************************************************
      ENTRY PF_MAKEBIN
C***********************************************************************
      BASE_FILE_NAME = PARMFIN()
      FILE_NAME = trim(BASE_FILE_DIRECTORY())//
     +                             "PFB"//trim(BASE_FILE_NAME)//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      DATA_DRIVE = OUTPUT_DIRECTORY()
      BC_TABLE_NUMBER = 0
      MAX_BC_CLASS_NUMBER = 0
      THIS_IS_THE_FIRST_TABLE = .TRUE.
      OUC_ACTIVE = ORLANDO_UC()
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
            CALL MG_LOCATE_WRITE(16,30,BASE_FILE_NAME,ALL_VERSIONS,0)
         ENDIF
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCFPARM.BIN",ACCESS="DIRECT",
     +                                      STATUS="UNKNOWN",RECL=LRECL)
         IREC = 0
         CLASS_NUM = 0
         SINKING_FUND_PATTERN = 0.
         FIRST_RECORD_OF_TABLE = INIT_CREATE_CLASS_LIST(INT2(-1))
C
         READ(10,*,IOSTAT=IOS) DELETE
         DO
            PARM_VALUES = 0.
            PARM_VALUES(98) = 100.     ! STD_IN_CAPITAL_STRUCTURE = 100.
            PARM_VALUES(129) = 999999.  ! MAX ST INVESTMENTS ITEM 133
            PARM_VALUES(131) = 999999.  ! MAX LT INVESTMENTS ITEM 135
            QUARTERLY_COMMON_DIVIDENDS(1) = -999.
            QUARTERLY_COMMON_DIVIDENDS(2) = -999.
            QUARTERLY_COMMON_DIVIDENDS(3) = -999.
            QUARTERLY_COMMON_DIVIDENDS(4) = -999.
            DATA_RECORDS_IN_TABLE = 0
            FIRST_RECORD_OF_TABLE = .TRUE.
            TABLE_IS_NOT_ACTIVE = .FALSE.
            TABLE_TYPE = 'C'
            MUNI_PARM_VALUES = 0.
            RB_INCLUDED_DEF_TAXES_DR_RATE = 0.
            RB_EXCLUDED_PENSION_LIABILITY_RATE = 0.! 156
            RB_EXCLUDED_Deferred_Gain_RATE = 0. ! 157
            RB_EXCLUDED_Storm_Reserve_RATE = 0. ! 158
            RB_EXCLUDED_Accrued_Vacation_Pay_RATE = 0. !159
            RB_INCLUDED_DEFERRED_REVENUES_RATE = 0.  ! 160
            ODEC_MaxEquityRatio = 100.
            ODEC_MinPayoutRatio = 0. ! 162
            ODEC_MaxPayoutRatio = 10000. ! 163
            ODEC_PercentREReduction = 0. ! 164
            ODEC_DollarREReduction = 0. ! 165

C
            DO
               READ(10,1000,IOSTAT=IOS) RECLN
               IF(IOS /= 0) EXIT
               IF(RECLN(1:1) == '7') EXIT
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,'//
     +                                ',,,,,,,,,,,,,,,,,,,,,,,,'//
     +                                ',,,,,,,,,,,,,,,,,,,,,,,,'//
     +                                ',,,,,,,,,,,,,,,,,,,,,,,,'//
     +                                ',,,,,,,,,,,,,,,,,,,,,,,,'//
     +                                ',,,,,,,,,,,,,,,,,,,,,,,,'
               IF(OUC_ACTIVE) THEN
                  READ(RECLN,*,ERR=200) DELETE,YEAR,
     +                            CLASS_NAME,
     +                            TABLE_TYPE,
     +                            CLASS_NUM,
     +                            COMMENT,  ! THIS AND NEXT TWO PLACE HOLDERS
     +                            COMMENT,
     +                            COMMENT,
     +                            COMMENT,
     +                            MUNI_PARM_VALUES
               ELSE
                  READ(RECLN,*,ERR=200) DELETE,YEAR,
     +                            PRE_COMMENT_PARM_VALUES,
     +                            COMMENT,
     +                            POST_COMMENT_PARM_VALUES,
     +                            CLASS_NAME,CLASS_NUM,
     +                            POST_CLASS_NUM_VALUES,
     +                            TABLE_TYPE,
     +                            QUARTERLY_COMMON_DIVIDENDS,
     +                            RB_INCLUDED_DEF_TAXES_DR_RATE, ! 155
     +                            RB_EXCLUDED_PENSION_LIABILITY_RATE, ! 156
     +                            RB_EXCLUDED_Deferred_Gain_RATE, ! 157
     +                            RB_EXCLUDED_Storm_Reserve_RATE, ! 158
     +                            RB_EXCLUDED_Accrued_Vacation_Pay_RATE, !159
     +                            RB_INCLUDED_DEFERRED_REVENUES_RATE,  ! 160
     +                            ODEC_MaxEquityRatio,
     +                            ODEC_MinPayoutRatio, ! 162
     +                            ODEC_MaxPayoutRatio, ! 163
     +                            ODEC_PercentREReduction, ! 164
     +                            ODEC_DollarREReduction  ! 165
               ENDIF
C
C WRITE RECORD
C
               IREC = IREC + 1
               IF(OUC_ACTIVE) THEN
                  WRITE(11,REC=IREC) DELETE,YEAR,
     +                               MUNI_PARM_VALUES,
     +                               CLASS_NAME,
     +                               TABLE_TYPE,
     +                               CLASS_NUM
               ELSE
                  WRITE(11,REC=IREC) DELETE,YEAR,PARM_VALUES,
     +                            QUARTERLY_COMMON_DIVIDENDS,
     +                            RB_INCLUDED_DEF_TAXES_DR_RATE, ! 155
     +                            RB_EXCLUDED_PENSION_LIABILITY_RATE, ! 156
     +                            RB_EXCLUDED_Deferred_Gain_RATE, ! 157
     +                            RB_EXCLUDED_Storm_Reserve_RATE, ! 158
     +                            RB_EXCLUDED_Accrued_Vacation_Pay_RATE, !159
     +                            RB_INCLUDED_DEFERRED_REVENUES_RATE,  ! 160
     +                            ODEC_MaxEquityRatio,
     +                            ODEC_MinPayoutRatio, ! 162
     +                            ODEC_MaxPayoutRatio, ! 163
     +                            ODEC_PercentREReduction, ! 164
     +                            ODEC_DollarREReduction, ! 165
     +                            CLASS_NAME,CLASS_NUM,TABLE_TYPE
               ENDIF
               DATA_RECORDS_IN_TABLE = DATA_RECORDS_IN_TABLE + 1
C
               IF(FIRST_RECORD_OF_TABLE .AND. DELETE >= 8) THEN
                  TABLE_IS_NOT_ACTIVE = .TRUE.
                  FIRST_RECORD_OF_TABLE = .FALSE.
               ENDIF
               IF(TABLE_IS_NOT_ACTIVE) CYCLE
               IF((THIS_IS_THE_FIRST_TABLE .OR. TABLE_TYPE == 'P') .AND.
     +                                    IREC <= AVAIL_DATA_YEARS) THEN
                  FEDERAL_TAX_RATE(IREC) =  PARM_VALUES(49)
                  STATE_TAX_RATE(IREC) =  PARM_VALUES(52)
                  SINKING_FUND_PATTERN(IREC) = PARM_VALUES(36)
               ENDIF
               IF(FIRST_RECORD_OF_TABLE) THEN
                  FIRST_RECORD_OF_TABLE = .FALSE.
                  IF(INDEX('ASBR',TABLE_TYPE) == 0) THEN
                     TABLE_NUMBER = CREATE_CLASS_LIST(CLASS_NUM)
                  ELSE
                     TABLE_NUMBER = INCREMENT_TABLE_NUMBER()
                     IF(TABLE_TYPE == 'P' .OR. TABLE_TYPE == 'A') 
     +                             BC_PARENT_TABLE_NUMBER = TABLE_NUMBER
                     IF(TABLE_TYPE == 'S' .OR. TABLE_TYPE == 'A')
     +                        BC_SUB_DEFAULT_TABLE_NUMBER = TABLE_NUMBER
                     IF(TABLE_TYPE == 'B' .OR. TABLE_TYPE == 'A')
     +                        BC_SBU_DEFAULT_TABLE_NUMBER = TABLE_NUMBER
                     IF(TABLE_TYPE == 'R' .OR. TABLE_TYPE == 'A')
     +                        BC_REG_DEFAULT_TABLE_NUMBER = TABLE_NUMBER
                  ENDIF
               ENDIF
            ENDDO
            IF(DATA_RECORDS_IN_TABLE < AVAIL_DATA_YEARS) THEN
               DOWHILE (DATA_RECORDS_IN_TABLE < AVAIL_DATA_YEARS)
                  IREC = IREC + 1
                  WRITE(11,REC=IREC) DELETE,YEAR,PARM_VALUES,
     +                               CLASS_NAME,CLASS_NUM,TABLE_TYPE           
                  DATA_RECORDS_IN_TABLE = DATA_RECORDS_IN_TABLE + 1
               ENDDO
            ENDIF
            IF(IOS /= 0) EXIT
            THIS_IS_THE_FIRST_TABLE = .FALSE.
         ENDDO
         MAX_BC_CLASS_NUMBER = MAX_CLASS_IN_TABLE()
         ALLOCATE(BC_TABLE_POSITION_FOR(0:MAX_BC_CLASS_NUMBER))
         BC_TABLE_NUMBER = PASS_TABLE_POINTER(BC_TABLE_POSITION_FOR)
         CLOSE(10)
      ELSEIF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,BASE_FILE_NAME, "pf_objt:1111")

      ENDIF

      CLOSE(11)
      LAST_PARM_RECORD = IREC
      RETURN
C
C***********************************************************************
C
C          ROUTINE TO CREATE OVERLAY FILES
C          COPYRIGHT (C) 1984-88  M.S. GERBER & ASSOCIATES, INC.
C          COPYRIGHT (C) 1991-92  M.S. GERBER & ASSOCIATES, INC.
C
C***********************************************************************
C
C OVERLAY THE FINANCIAL-PARAMETERS FILE
C***********************************************************************
      ENTRY PF_MAKEOVL(OVERLAY_FAMILY_NAME)
C***********************************************************************
C
      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME = trim(DATA_DRIVE)//"PFO"//
     +                               trim(OVERLAY_FAMILY_NAME)//".DAT"
      OPEN(10,FILE=FILE_NAME)
      READ(10,*) DELETE
      INUNIT = 12
      IF(PARMFNOL == 'BC') THEN
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCFPARM.BIN",ACCESS="DIRECT",
     +                                                       RECL=LRECL)
         INUNIT = 11
      ENDIF
      OPEN(12,FILE=trim(DATA_DRIVE)//"OLFPARM.BIN",ACCESS="DIRECT",
     +                                      STATUS="UNKNOWN",RECL=LRECL)
      OUC_ACTIVE = ORLANDO_UC()
      OL_TABLE_NUMBER = 0
      MAX_OL_CLASS_NUMBER = 0
      THIS_IS_THE_FIRST_TABLE = .TRUE.
      IREC = 0
      FIRST_RECORD_OF_TABLE = INIT_CREATE_CLASS_LIST(INT2(-1))
      OL_PARENT_TABLE_NUMBER = -1
      OL_SUB_DEFAULT_TABLE_NUMBER = -1
      OL_SBU_DEFAULT_TABLE_NUMBER = -1
      OL_REG_DEFAULT_TABLE_NUMBER = -1
      SINKING_FUND_PATTERN = 0.
C
      DO
         FIRST_RECORD_OF_TABLE = .TRUE.
         END_OF_TABLE = .FALSE.
         TABLE_IS_NOT_ACTIVE = .FALSE.
         I = 0
C
         DO ! I = 1, AVAIL_DATA_YEARS 
C
            IF(.NOT. END_OF_TABLE) THEN
               READ(10,1000,IOSTAT=IOS) RECLN
               IF(RECLN(1:1) == '7') END_OF_TABLE = .TRUE.
            ENDIF
            IF(END_OF_TABLE .AND. I >= AVAIL_DATA_YEARS) EXIT 
            IREC = IREC + 1
            IF(OUC_ACTIVE) THEN
               READ(INUNIT,REC=IREC,IOSTAT=IOS_BASE) DELETE,YEAR,
     +                               MUNI_PARM_VALUES,
     +                               CLASS_NAME,
     +                               TABLE_TYPE,
     +                               CLASS_NUM
            ELSE
               READ(INUNIT,REC=IREC,IOSTAT=IOS_BASE) DELETE,YEAR,
     +                            PARM_VALUES,
     +                            QUARTERLY_COMMON_DIVIDENDS,
     +                            RB_INCLUDED_DEF_TAXES_DR_RATE, ! 155
     +                            RB_EXCLUDED_PENSION_LIABILITY_RATE, ! 156
     +                            RB_EXCLUDED_Deferred_Gain_RATE, ! 157
     +                            RB_EXCLUDED_Storm_Reserve_RATE, ! 158
     +                            RB_EXCLUDED_Accrued_Vacation_Pay_RATE, !159
     +                            RB_INCLUDED_DEFERRED_REVENUES_RATE,  ! 160
     +                            ODEC_MaxEquityRatio,
     +                            ODEC_MinPayoutRatio, ! 162
     +                            ODEC_MaxPayoutRatio, ! 163
     +                            ODEC_PercentREReduction, ! 164
     +                            ODEC_DollarREReduction, ! 165
     +                            CLASS_NAME,CLASS_NUM,TABLE_TYPE
            ENDIF
C
            IF(IOS_BASE /= 0) EXIT
            IF(IOS == 0 .AND. .NOT. END_OF_TABLE) THEN
               RECLN = trim(RECLN)//
     +                        ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'//
     +                        ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               IF(OUC_ACTIVE) THEN
                  READ(RECLN,*,ERR=200) DELETE,YEAR,
     +                            CLASS_NAME,
     +                            TABLE_TYPE,
     +                            CLASS_NUM,
     +                            COMMENT,  ! THIS AND NEXT TWO PLACE HOLDERS
     +                            COMMENT,
     +                            COMMENT,
     +                            COMMENT,
     +                            MUNI_PARM_VALUES
               ELSE
                  READ(RECLN,*,ERR=200) DELETE,YEAR,
     +                            PRE_COMMENT_PARM_VALUES,COMMENT,
     +                            POST_COMMENT_PARM_VALUES,
     +                            CLASS_NAME,CLASS_NUM,
     +                            POST_CLASS_NUM_VALUES,
     +                            TABLE_TYPE,
     +                            QUARTERLY_COMMON_DIVIDENDS,
     +                            RB_INCLUDED_DEF_TAXES_DR_RATE, ! 155
     +                            RB_EXCLUDED_PENSION_LIABILITY_RATE, ! 156
     +                            RB_EXCLUDED_Deferred_Gain_RATE, ! 157
     +                            RB_EXCLUDED_Storm_Reserve_RATE, ! 158
     +                            RB_EXCLUDED_Accrued_Vacation_Pay_RATE, !159
     +                            RB_INCLUDED_DEFERRED_REVENUES_RATE,  ! 160
     +                            ODEC_MaxEquityRatio,
     +                            ODEC_MinPayoutRatio, ! 162
     +                            ODEC_MaxPayoutRatio, ! 163
     +                            ODEC_PercentREReduction, ! 164
     +                            ODEC_DollarREReduction  ! 165
               ENDIF
            ENDIF
C
            I = I + 1
            IF(OUC_ACTIVE) THEN
               WRITE(12,REC=IREC) DELETE,YEAR,
     +                               MUNI_PARM_VALUES,
     +                               CLASS_NAME,
     +                               TABLE_TYPE,
     +                               CLASS_NUM
            ELSE
               WRITE(12,REC=IREC) DELETE,YEAR,PARM_VALUES,
     +                            QUARTERLY_COMMON_DIVIDENDS,
     +                            RB_INCLUDED_DEF_TAXES_DR_RATE, ! 155
     +                            RB_EXCLUDED_PENSION_LIABILITY_RATE, ! 156
     +                            RB_EXCLUDED_Deferred_Gain_RATE, ! 157
     +                            RB_EXCLUDED_Storm_Reserve_RATE, ! 158
     +                            RB_EXCLUDED_Accrued_Vacation_Pay_RATE, !159
     +                            RB_INCLUDED_DEFERRED_REVENUES_RATE,  ! 160
     +                            ODEC_MaxEquityRatio,
     +                            ODEC_MinPayoutRatio,
     +                            ODEC_MaxPayoutRatio,
     +                            ODEC_PercentREReduction,
     +                            ODEC_DollarREReduction,
     +                            CLASS_NAME,CLASS_NUM,TABLE_TYPE
            ENDIF
C
            IF(FIRST_RECORD_OF_TABLE .AND. DELETE >= 8) THEN
               TABLE_IS_NOT_ACTIVE = .TRUE.
               FIRST_RECORD_OF_TABLE = .FALSE.
            ENDIF
            IF(TABLE_IS_NOT_ACTIVE) CYCLE
C
            IF((THIS_IS_THE_FIRST_TABLE .OR. TABLE_TYPE == 'P') .AND.
     +                                    IREC <= AVAIL_DATA_YEARS) THEN
               FEDERAL_TAX_RATE(IREC) =  PARM_VALUES(49)
               STATE_TAX_RATE(IREC) =  PARM_VALUES(52)
               SINKING_FUND_PATTERN(IREC) = PARM_VALUES(36)
            ENDIF
            IF(FIRST_RECORD_OF_TABLE) THEN
               FIRST_RECORD_OF_TABLE = .FALSE.
               IF(INDEX('ASBR',TABLE_TYPE) == 0) THEN
                  TABLE_NUMBER = CREATE_CLASS_LIST(CLASS_NUM)
               ELSE
                  TABLE_NUMBER = INCREMENT_TABLE_NUMBER()
                  IF(TABLE_TYPE == 'P' .OR. TABLE_TYPE == 'A')
     +                             OL_PARENT_TABLE_NUMBER = TABLE_NUMBER
                  IF(TABLE_TYPE == 'S' .OR. TABLE_TYPE == 'A')
     +                        OL_SUB_DEFAULT_TABLE_NUMBER = TABLE_NUMBER
                  IF(TABLE_TYPE == 'B' .OR. TABLE_TYPE == 'A')
     +                        OL_SBU_DEFAULT_TABLE_NUMBER = TABLE_NUMBER
                  IF(TABLE_TYPE == 'R' .OR. TABLE_TYPE == 'A')
     +                        OL_REG_DEFAULT_TABLE_NUMBER = TABLE_NUMBER
               ENDIF
            ENDIF
         ENDDO
         IF(IOS_BASE /= 0) EXIT
         THIS_IS_THE_FIRST_TABLE = .FALSE.
      ENDDO
      CLOSE(10)
      CLOSE(12)
C
      MAX_OL_CLASS_NUMBER = MAX_CLASS_IN_TABLE()
      IF(ALLOCATED(OL_TABLE_POSITION_FOR))
     +                                 DEALLOCATE(OL_TABLE_POSITION_FOR)
      ALLOCATE(OL_TABLE_POSITION_FOR(0:MAX_OL_CLASS_NUMBER))
      OL_TABLE_NUMBER = PASS_TABLE_POINTER(OL_TABLE_POSITION_FOR)
C
      LAST_PARM_RECORD = IREC - 1
      IF(PARMFNOL == 'BC') CLOSE(11)
      PARMFNOL = 'OL'
      RETURN
C
C MUNICIPAL FINANCIAL PARAMETERS FILE
C
C***********************************************************************
      ENTRY MUNI_PF_MAKEBIN
C***********************************************************************
      BASE_FILE_NAME = PARMFIN()
      FILE_NAME = trim(BASE_FILE_DIRECTORY())//
     +                             "PFB"//trim(BASE_FILE_NAME)//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      DATA_DRIVE = OUTPUT_DIRECTORY()
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(MUNI_FILE_TYPE)//'-'//BASE_FILE_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_LOCATE_WRITE(16,30,BASE_FILE_NAME,ALL_VERSIONS,0)
            CALL MG_CLEAR_LINE_WRITE(17,9,36,MUNI_FILE_TYPE,
     +                                                   ALL_VERSIONS,0)
         ENDIF
         MUNI_PARM_VALUES = 0.
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCFPARM.BIN",ACCESS="DIRECT",
     +                                 STATUS="UNKNOWN",RECL=MUNI_LRECL)

         IREC = 0
         READ(10,*) DELETE
         DO
            READ(10,'(A)',IOSTAT=IOS) RECLN
            IF(IOS /= 0) EXIT
            RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
            READ(RECLN,*,ERR=200) DELETE,YEAR,
     +                            CLASS_NAME,
     +                            TABLE_TYPE,
     +                            CLASS_NUM,
     +                            COMMENT,  ! THIS AND NEXT TWO PLACE HOLDERS
     +                            COMMENT,
     +                            COMMENT,
     +                            COMMENT,
     +                            MUNI_PARM_VALUES
C
            IREC = IREC + 1
            WRITE(11,REC=IREC) DELETE,YEAR,CLASS_NAME,
     +                            TABLE_TYPE,
     +                            CLASS_NUM,MUNI_PARM_VALUES
         ENDDO
         CLOSE(10)
      ELSE IF(INDEX(PARMFIN(),'NONE') == 0) THEN
         WRITE(4,*) '*** line 438 PF_OBJT.FOR ***'
         WRITE(4,*) 'Municipal Parameter File ',TRIM(FILE_NAME),
     +              ' does not exist.'
         er_message='See WARNING MESSAGES -PF_OBJT.FOR-1'
         call end_program(er_message)
      ELSE
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCFPARM.BIN",
     +                 ACCESS="DIRECT",STATUS="UNKNOWN",RECL=MUNI_LRECL)
c        ENDFILE(11)
         CLOSE(11)
      ENDIF
      LAST_PARM_RECORD = IREC - 1
      RETURN
C***********************************************************************
      ENTRY MUNI_PF_MAKEOVL(OVERLAY_FAMILY_NAME)
C***********************************************************************
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(MUNI_FILE_TYPE)//'-'//
     +                                               OVERLAY_FAMILY_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_CLEAR_LINE_WRITE(17,9,36,MUNI_FILE_TYPE,
     +                                                   ALL_VERSIONS,0)
            CALL LOCATE(10,51)
         ENDIF
         DATA_DRIVE = OUTPUT_DIRECTORY()
         FILE_NAME = trim(DATA_DRIVE)//"PFO"//
     +                               trim(OVERLAY_FAMILY_NAME)//".DAT"
         OPEN(10,FILE=FILE_NAME)
         READ(10,*) DELETE
         INUNIT = 12
         IF(PARMFNOL == 'BC') THEN
            OPEN(11,FILE=trim(DATA_DRIVE)//"BCFPARM.BIN",
     +                 ACCESS="DIRECT",STATUS="UNKNOWN",RECL=MUNI_LRECL)
            INUNIT = 11
         ENDIF
         OPEN(12,FILE=trim(DATA_DRIVE)//"OLFPARM.BIN",ACCESS="DIRECT",
     +                                 STATUS="UNKNOWN",RECL=MUNI_LRECL)
         IREC = 0
         DO
            IREC = IREC + 1
            READ(INUNIT,REC=IREC,IOSTAT=IOS) DELETE,YEAR,CLASS_NAME,
     +                            TABLE_TYPE,CLASS_NUM,MUNI_PARM_VALUES
            IF(IOS /= 0) EXIT
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(IOS == 0) THEN
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,ERR=200) DELETE,YEAR,CLASS_NAME,
     +                            TABLE_TYPE,CLASS_NUM,MUNI_PARM_VALUES
            ENDIF
            WRITE(12,REC=IREC) DELETE,YEAR,CLASS_NAME,
     +                            TABLE_TYPE,CLASS_NUM,MUNI_PARM_VALUES
          ENDDO
         CLOSE(10)
         CLOSE(12)
         LAST_PARM_RECORD = IREC - 1
         IF(PARMFNOL == 'BC') CLOSE(11)
         PARMFNOL = 'OL'
      RETURN

  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from PF_OBJT SIID253'
      call end_program(er_message)
C
C***********************************************************************
      ENTRY RESET_PARMFNOL
C***********************************************************************
         PARMFNOL = 'BC'
      RETURN
C
C***********************************************************************
      ENTRY OPEN_FINANICAL_PARAMETER_FILE(PARM_UNIT)
C***********************************************************************
         SAVE_PARM_UNIT = PARM_UNIT
         FILE_NAME = trim(OUTPUT_DIRECTORY())//PARMFNOL//"FPARM.BIN"
         INQUIRE(FILE=FILE_NAME,OPENED=FILE_OPEN)
         IF(.NOT. FILE_OPEN)
     +         OPEN(PARM_UNIT,FILE=FILE_NAME,ACCESS="DIRECT",RECL=LRECL)
      RETURN
C
C***********************************************************************
      ENTRY CLOSE_FINANCIAL_PARAMETER_FILE
C***********************************************************************
         CLOSE(SAVE_PARM_UNIT)
      RETURN


       ENTRY RETURN_FED_STATE_TAX_RATES(R_YEAR,R_FEDERAL_TAX_RATE, 
     +                                  R_STATE_TAX_RATE)
C***********************************************************************
          IF(R_YEAR <= AVAIL_DATA_YEARS) THEN
             R_FEDERAL_TAX_RATE = FEDERAL_TAX_RATE(R_YEAR)
             R_STATE_TAX_RATE = STATE_TAX_RATE(R_YEAR)
          ELSE   
             R_FEDERAL_TAX_RATE = FEDERAL_TAX_RATE(AVAIL_DATA_YEARS)
             R_STATE_TAX_RATE = STATE_TAX_RATE(AVAIL_DATA_YEARS)
          ENDIF
       RETURN
C***********************************************************************

       ENTRY RETURN_FED_TAX_RATES(R_YEAR,R_FEDERAL_TAX_RATE) 
C***********************************************************************
          IF(R_YEAR <= AVAIL_DATA_YEARS) THEN
             R_FEDERAL_TAX_RATE = FEDERAL_TAX_RATE(R_YEAR)
          ELSE   
             R_FEDERAL_TAX_RATE = FEDERAL_TAX_RATE(AVAIL_DATA_YEARS)
          ENDIF
       RETURN
C***********************************************************************
      ENTRY LAST_FIN_PARM_RECORD_IS(R_LAST_PARM_RECORD)
C***********************************************************************
         R_LAST_PARM_RECORD = AVAIL_DATA_YEARS
      RETURN
C***********************************************************************
      ENTRY RETURN_PARM_FILE_CLASS_NUM(R_CLASS_NUMBER)
C***********************************************************************
         IF(PARMFNOL == 'OL') THEN
            R_CLASS_NUMBER = MAX_OL_CLASS_NUMBER
         ELSE
            R_CLASS_NUMBER = MAX_BC_CLASS_NUMBER
         ENDIF
      RETURN
C***********************************************************************
      ENTRY RETURN_PARM_FILE_CLASS_INFO(R_TABLE_POINTR,
     +                                 R_PARENT_TABLE_NUMBER,
     +                                 R_SUB_DEFAULT_TABLE_NUMBER,
     +                                 R_SBU_DEFAULT_TABLE_NUMBER,
     +                                 R_REG_DEFAULT_TABLE_NUMBER)
C***********************************************************************
         IF(PARMFNOL == 'OL') THEN
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
C***********************************************************************
      ENTRY SRP_SINKING_FUNDS_INFO(R_SINKING_FUND_PATTERN)
C***********************************************************************
         R_SINKING_FUND_PATTERN(1:AVAIL_DATA_YEARS) =
     +                     SINKING_FUND_PATTERN(1:AVAIL_DATA_YEARS)/100.
      RETURN
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END