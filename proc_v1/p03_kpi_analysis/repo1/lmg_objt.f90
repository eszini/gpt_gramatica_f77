
!     LMG_OBJT.FOR
!     Copyright(c)  2000
!
!     Created: 1/29/2007 3:53:38 PM
!     Author : MARK S GERBER
!     Last change: MSG 1/29/2007 3:53:38 PM


      SUBROUTINE LG_OBJECT
      use end_routine, only: end_program, er_message
      use spindriftlib
      use prod_arrays_dimensions
      USE SIZECOM
!
      INTEGER (kind=2) ::  I,IREC,INUNIT,DELETE,LRECL=400 ,STORE_RECORDS
      INTEGER ::  IOS
      INTEGER (kind=2) ::  STORE_ACCEPTANCE
      INTEGER (kind=2) ::  STORE_BASE_MAX_PROGRAM_NUM
      INTEGER (kind=2) ::  STORE_MAX_PROGRAM_NUM
      INTEGER (kind=2) ::  RESPONSE_CURVE_FOR(3)
      INTEGER ::  UNIT_NO
      INTEGER (kind=2) ::  VOID_INT2,RESET_DSM_MAX_PROGRAM_NUM
      INTEGER (kind=1) ::  PEAK_DAY,WEEKDAY,WEEKEND
      PARAMETER(PEAK_DAY=3,WEEKDAY=1,WEEKEND=2)
      CHARACTER (len=5) ::  OVERLAY_FAMILY_NAME,LDMGT
      CHARACTER (len=30) ::  COMMENT
      CHARACTER (len=256) ::  FILE_NAME
      CHARACTER (len=256) ::  BASE_FILE_DIRECTORY,OUTPUT_DIRECTORY
      LOGICAL (kind=4) ::  FILE_EXISTS
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER (len=2048) ::  RECLN
! DECLARATION FOR LOAD MANAGEMENT APPLICATION VARIABLES
      INTEGER (kind=2) ::  DEVICE_NUM,CLASS_NUM,DSM_START_YEAR
      INTEGER (kind=2) ::  MAX_DSM_PROGRAM_NUM=0
      REAL ::  BUFFER(3),CUSTOMERS(AVAIL_DATA_YEARS), &
           DSM_CAP_PLAN_FACTOR,BASE_CUST, &
           FREE_RIDERS,FREE_DRIVERS,PROGRAM_LOSS_FACTOR, &
           NEW_CUSTOMERS(AVAIL_DATA_YEARS)
      CHARACTER (len=32) ::  CLASS_NAME,DEVICE_NAME
      CHARACTER (len=14) ::  FILE_TYPE='DSM-Acceptance'
      CHARACTER (len=1) ::  METHOD,ENERGY_CLASSIFICATION, &
                  PROGRAM_DSM_ALLOCATION_METHOD
      CHARACTER (len=2) ::  LDMGT_OL='BC'
      LOGICAL (kind=1) ::  LAHEY_LF95
      CHARACTER (len=30) ::  SCREEN_OUTPUT


!
!          ROUTINE TO CONVERT METAFILE FILES TO DIRECT-ACESS BINARY
!          COPYRIGHT (C) 1983, 84, 85  M.S. GERBER & ASSOCIATES, INC.
!

!
! CONVERT THE LOAD-MANAGEMENT APPLICATION FILE
      ENTRY LG_MAKEBIN
!
      FILE_NAME = trim(BASE_FILE_DIRECTORY())// &
                                          "LGB"//trim(LDMGT())//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//LDMGT()
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
            CALL MG_LOCATE_WRITE(16,30,LDMGT(),ALL_VERSIONS,0)
         ENDIF
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCLDMGT.BIN", &
                            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         IREC = 1
         READ(10,*)
         DO
            CUSTOMERS = 0.
            NEW_CUSTOMERS = 0.
            BUFFER(3) = -9999.
            METHOD = 'E'
            ENERGY_CLASSIFICATION = 'S'
            DSM_CAP_PLAN_FACTOR = 1.0
            RESPONSE_CURVE_FOR(WEEKDAY) = -9999
            RESPONSE_CURVE_FOR(WEEKEND) = -9999
            RESPONSE_CURVE_FOR(PEAK_DAY) = -9999
            FREE_RIDERS = 0.
            FREE_DRIVERS = 0.
            PROGRAM_LOSS_FACTOR = 0.
            PROGRAM_DSM_ALLOCATION_METHOD = 'L'
!
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(IOS /=0) EXIT
            RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
               ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
            READ(RECLN,*,ERR=200) &
               DELETE,DEVICE_NUM,DEVICE_NAME,CLASS_NUM, &
               CLASS_NAME,BUFFER(1),BUFFER(2),CUSTOMERS, &
               COMMENT,BUFFER(3),METHOD, &
               DSM_CAP_PLAN_FACTOR,DSM_START_YEAR, &
               BASE_CUST,ENERGY_CLASSIFICATION, &
               FREE_RIDERS,FREE_DRIVERS,PROGRAM_LOSS_FACTOR, &
               RESPONSE_CURVE_FOR, &
               NEW_CUSTOMERS, &
               PROGRAM_DSM_ALLOCATION_METHOD
            IF(BUFFER(3) == -9999.) BUFFER(3) = BUFFER(1)
            IF(RESPONSE_CURVE_FOR(WEEKDAY) == -9999) &
                               RESPONSE_CURVE_FOR(WEEKDAY) = DEVICE_NUM
            IF(RESPONSE_CURVE_FOR(WEEKEND) == -9999) &
                               RESPONSE_CURVE_FOR(WEEKEND) = DEVICE_NUM
            IF(RESPONSE_CURVE_FOR(PEAK_DAY) == -9999) &
                              RESPONSE_CURVE_FOR(PEAK_DAY) = DEVICE_NUM
            WRITE(11,REC=IREC) DELETE, &
                               DEVICE_NAME,DEVICE_NUM, &
                               RESPONSE_CURVE_FOR, &
                               CLASS_NUM,CLASS_NAME, &
                               BUFFER,METHOD,CUSTOMERS, &
                               DSM_CAP_PLAN_FACTOR,DSM_START_YEAR, &
                               BASE_CUST,ENERGY_CLASSIFICATION, &
                               FREE_RIDERS,FREE_DRIVERS, &
                               PROGRAM_LOSS_FACTOR, &
                               NEW_CUSTOMERS, &
                               PROGRAM_DSM_ALLOCATION_METHOD
            IREC = IREC + 1
            MAX_DSM_PROGRAM_NUM = MAX(MAX_DSM_PROGRAM_NUM,DEVICE_NUM)
         ENDDO
         CLOSE(10)
         CLOSE(11)
         STORE_RECORDS = IREC - 1
         IREC = STORE_ACCEPTANCE(STORE_RECORDS)
         IREC = STORE_BASE_MAX_PROGRAM_NUM(MAX_DSM_PROGRAM_NUM)
      ELSE IF(INDEX(LDMGT(),'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "lmg_objt:1113")
      ENDIF
      RETURN


!
!          ROUTINE TO CREATE OVERLAY FILES
!          COPYRIGHT (C) 1984-88  M.S. GERBER & ASSOCIATES, INC.
!          COPYRIGHT (C) 1991-92  M.S. GERBER & ASSOCIATES, INC.
!

!
! OVERLAY THE LOAD-MANAGEMENT APPLICATION FILE
      ENTRY LG_MAKEOVL(OVERLAY_FAMILY_NAME)
!
      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      FILE_NAME=trim(OUTPUT_DIRECTORY())//"LGO"// &
                                     trim(OVERLAY_FAMILY_NAME)//".DAT"
      OPEN(10,FILE=FILE_NAME)
      READ(10,*) DELETE
      INUNIT = 12
      IF(LDMGT_OL == 'BC') THEN
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCLDMGT.BIN", &
                                             ACCESS="DIRECT",RECL=LRECL)
         INUNIT = 11
      ENDIF
      OPEN(12,FILE=trim(OUTPUT_DIRECTORY())//"OLLDMGT.BIN", &
                            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      IREC = 0
      MAX_DSM_PROGRAM_NUM = 0
      DO
         IREC = IREC + 1
         READ(INUNIT,REC=IREC,IOSTAT=IOS) DELETE, &
                               DEVICE_NAME,DEVICE_NUM, &
                               RESPONSE_CURVE_FOR, &
                               CLASS_NUM,CLASS_NAME, &
                               BUFFER,METHOD,CUSTOMERS, &
                               DSM_CAP_PLAN_FACTOR,DSM_START_YEAR, &
                               BASE_CUST,ENERGY_CLASSIFICATION, &
                               FREE_RIDERS,FREE_DRIVERS, &
                               PROGRAM_LOSS_FACTOR, &
                               NEW_CUSTOMERS, &
                               PROGRAM_DSM_ALLOCATION_METHOD
         IF(IOS /= 0) EXIT
         READ(10,1000,IOSTAT=IOS) RECLN
         IF(IOS == 0) THEN
            RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
               ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
            READ(RECLN,*,ERR=200) DELETE,DEVICE_NUM, &
               DEVICE_NAME,CLASS_NUM, &
               CLASS_NAME,BUFFER(1),BUFFER(2),CUSTOMERS, &
               COMMENT,BUFFER(3),METHOD, &
               DSM_CAP_PLAN_FACTOR,DSM_START_YEAR, &
               BASE_CUST,ENERGY_CLASSIFICATION, &
               FREE_RIDERS,FREE_DRIVERS,PROGRAM_LOSS_FACTOR, &
               RESPONSE_CURVE_FOR, &
               NEW_CUSTOMERS, &
               PROGRAM_DSM_ALLOCATION_METHOD
         ENDIF
         WRITE(12,REC=IREC) DELETE, &
                            DEVICE_NAME,DEVICE_NUM, &
                            RESPONSE_CURVE_FOR, &
                            CLASS_NUM,CLASS_NAME, &
                            BUFFER,METHOD,CUSTOMERS, &
                            DSM_CAP_PLAN_FACTOR,DSM_START_YEAR, &
                            BASE_CUST,ENERGY_CLASSIFICATION, &
                            FREE_RIDERS,FREE_DRIVERS, &
                            PROGRAM_LOSS_FACTOR, &
                            NEW_CUSTOMERS, &
                            PROGRAM_DSM_ALLOCATION_METHOD
         MAX_DSM_PROGRAM_NUM = MAX(MAX_DSM_PROGRAM_NUM,DEVICE_NUM)
      ENDDO
      IREC = STORE_MAX_PROGRAM_NUM(MAX_DSM_PROGRAM_NUM)
      CLOSE(10)
      CLOSE(12)
      IF(LDMGT_OL == 'BC') CLOSE(11)
      LDMGT_OL = 'OL'
      RETURN


  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from LMG_OBJT SIIDL186'
      call end_program(er_message)
!
      ENTRY RESET_LDMGT_OL
         LDMGT_OL = 'BC'
         VOID_INT2 = RESET_DSM_MAX_PROGRAM_NUM()
      RETURN
!
      ENTRY OPEN_DSM_ACCEPTENCE_FILE(UNIT_NO)
         OPEN(UNIT_NO, &
            FILE=trim(OUTPUT_DIRECTORY())//LDMGT_OL//'LDMGT.BIN', &
                                             ACCESS='DIRECT',RECL=LRECL)
      RETURN
!
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END
!
! RESPONSE FILES
!
      SUBROUTINE LS_OBJECT
      use end_routine, only: end_program, er_message
      use spindriftlib
      use prod_arrays_dimensions
      USE SIZECOM

      INTEGER (kind=2) ::  I,IREC,INUNIT,DELETE,LRECL=256
      INTEGER ::  IOS
      INTEGER (kind=2) ::  STORE_MAX_RESPONSE_NUM
      CHARACTER (len=5) ::  OVERLAY_FAMILY_NAME,LMGLS
      CHARACTER (len=30) ::  COMMENT
      CHARACTER (len=32) ::  DEVICE_NAME
      CHARACTER (len=256) ::  FILE_NAME
      CHARACTER (len=256) ::  BASE_FILE_DIRECTORY,OUTPUT_DIRECTORY
      LOGICAL (kind=4) ::  FILE_EXISTS
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER (len=2048) ::  RECLN
! DECLARATION FOR LOAD MANAGEMENT DEVICE VARIABLES
      INTEGER (kind=2) ::  DEVICE_NUM,FIRST_MONTH,UNITS,LAST_MONTH
      INTEGER ::  UNIT_NO
      INTEGER (kind=2) ::  MAX_DSM_RESPONSE_NUM=0
      INTEGER (kind=2) ::  STORE_RESPONSE
      REAL ::  BUFFER(48),ADJUSTMENT_FACTOR(2)
      CHARACTER (len=12) ::  FILE_TYPE='DSM-Response'
      CHARACTER (len=2) ::  LMGLS_OL='BC'
      LOGICAL (kind=1) ::  LAHEY_LF95
      CHARACTER (len=30) ::  SCREEN_OUTPUT


!
!          ROUTINE TO CONVERT METAFILE FILES TO DIRECT-ACESS BINARY
!          COPYRIGHT (C) 1983, 84, 85  M.S. GERBER & ASSOCIATES, INC.
!

!
! CONVERT THE LOAD-MANAGEMENT DEVICE FILE
      ENTRY LS_MAKEBIN
!
      FILE_NAME = trim(BASE_FILE_DIRECTORY())// &
                                          "LSB"//trim(LMGLS())//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//LMGLS()
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
            CALL MG_LOCATE_WRITE(16,30,LMGLS(),ALL_VERSIONS,0)
         ENDIF
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCLMGLS.BIN", &
                            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         IREC = 1
         READ(10,*)
         DO
            ADJUSTMENT_FACTOR(1) = 1.
            ADJUSTMENT_FACTOR(2) = 1.
            LAST_MONTH = -9999
            DO I = 1, 48
               BUFFER(I) = 0.0
            ENDDO
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(IOS /=0) EXIT
            RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,'
            READ(RECLN,*,ERR=200) DELETE,DEVICE_NUM,DEVICE_NAME, &
                                  FIRST_MONTH,UNITS,BUFFER, &
                                  COMMENT,ADJUSTMENT_FACTOR,LAST_MONTH
            IF(LAST_MONTH == -9999) LAST_MONTH = FIRST_MONTH
            WRITE(11,REC=IREC)DELETE,DEVICE_NUM,DEVICE_NAME,FIRST_MONTH, &
                               UNITS,BUFFER,ADJUSTMENT_FACTOR,LAST_MONTH
            IREC = IREC + 1
            MAX_DSM_RESPONSE_NUM = MAX(MAX_DSM_RESPONSE_NUM,DEVICE_NUM)
         ENDDO
         CLOSE(10)
         CLOSE(11)
         IREC = IREC - 1
         IREC = STORE_RESPONSE(IREC)
         IREC = STORE_MAX_RESPONSE_NUM(MAX_DSM_RESPONSE_NUM)
      ELSE IF(INDEX(LMGLS(),'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "lmg_objt:1112")
      ENDIF
      RETURN



!          ROUTINE TO CREATE OVERLAY FILES
!          COPYRIGHT (C) 1984-88/92  M.S. GERBER & ASSOCIATES, INC.

! OVERLAY THE LOAD-MANAGEMENT DEVICE FILE
      ENTRY LS_MAKEOVL(OVERLAY_FAMILY_NAME)
!
      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      FILE_NAME=trim(OUTPUT_DIRECTORY())//"LSO"// &
                                     trim(OVERLAY_FAMILY_NAME)//".DAT"
      OPEN(10,FILE=FILE_NAME)
      READ(10,*) DELETE
      INUNIT = 12
      IF(LMGLS_OL == 'BC') THEN
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCLMGLS.BIN", &
                                             ACCESS="DIRECT",RECL=LRECL)
         INUNIT = 11
      ENDIF
      OPEN(12,FILE=trim(OUTPUT_DIRECTORY())//"OLLMGLS.BIN", &
                            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      MAX_DSM_RESPONSE_NUM = 0
      IREC = 0
      DO
         IREC = IREC + 1
         READ(INUNIT,REC=IREC,IOSTAT=IOS) DELETE,DEVICE_NUM,DEVICE_NAME, &
                   FIRST_MONTH,UNITS,BUFFER,ADJUSTMENT_FACTOR,LAST_MONTH
         IF(IOS /= 0) EXIT
         READ(10,1000,IOSTAT=IOS) RECLN
         IF(IOS == 0) THEN
            RECLN = trim(RECLN)//',,,,,,,,,,,,,'
            READ(RECLN,*,ERR=200) DELETE,DEVICE_NUM,DEVICE_NAME, &
               FIRST_MONTH,UNITS,BUFFER,COMMENT,ADJUSTMENT_FACTOR, &
               LAST_MONTH
         ENDIF
         WRITE(12,REC=IREC) DELETE,DEVICE_NUM,DEVICE_NAME,FIRST_MONTH, &
                            UNITS,BUFFER,ADJUSTMENT_FACTOR,LAST_MONTH
         MAX_DSM_RESPONSE_NUM = MAX(MAX_DSM_RESPONSE_NUM,DEVICE_NUM)
      ENDDO
      IREC = STORE_MAX_RESPONSE_NUM(MAX_DSM_RESPONSE_NUM)
      CLOSE(10)
      CLOSE(12)
      IF(LMGLS_OL == 'BC') CLOSE(11)
      LMGLS_OL = 'OL'
      RETURN

  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from LMG_OBJT SIID184'
      call end_program(er_message)
!
      ENTRY RESET_LMGLS_OL
         LMGLS_OL = 'BC'
      RETURN
!
      ENTRY OPEN_DSM_RESPONSE_FILE(UNIT_NO)
         OPEN(UNIT_NO, &
               FILE=trim(OUTPUT_DIRECTORY())//LMGLS_OL//'LMGLS.BIN', &
                                             ACCESS='DIRECT',RECL=LRECL)
      RETURN
!
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END

      SUBROUTINE LF_OBJECT
      use end_routine, only: end_program, er_message
      use spindriftlib
      use prod_arrays_dimensions
      USE SIZECOM

      INTEGER (kind=2) ::  IREC,INUNIT,DELETE,ACCTNO,LRECL=344
      INTEGER ::  IOS
      INTEGER (kind=2) ::  STORE_FINANCIAL
      INTEGER ::  UNIT_NO
      CHARACTER (len=5) ::  LD_FIN_FIL,OVERLAY_FAMILY_NAME
      CHARACTER (len=30) ::  DESC,COMMENT
      CHARACTER (len=256) ::  FILE_NAME
      CHARACTER (len=256) ::  BASE_FILE_DIRECTORY,OUTPUT_DIRECTORY
      LOGICAL (kind=4) ::  FILE_EXISTS
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER (len=2048) ::  RECLN
! DECLARATION FOR /LOAD MANAGEMENT FINANCIAL FILE/
      INTEGER (kind=2) ::  DEVICE_NUM
! Merge2Issue: (probably ok)  Left side used ONGO_CUST_EXP_ESCAL_LOC, and
! right side used ONGO_PROG_EXP_ESCAL (original name). 
! Going with left-side choice.
      INTEGER (kind=2) ::  ONGO_PROG_EXP_ESCAL_loc, &
      	ONGO_CUST_EXP_ESCAL_loc, &

               ONGO_KWH_EXP_ESCAL_loc,ONGO_KW_EXP_ESCAL_loc, &
               ONGO_PROG_CAP_ESCAL_loc,ONGO_CUST_CAP_ESCAL_loc, &
               ONGO_NEW_CUST_CAP_ESCAL_loc,ONGO_KWH_CAP_ESCAL_loc, &
               ONGO_KW_CAP_ESCAL_loc,ONGO_NEW_CUST_EXP_ESCAL_loc, &
               REBATE_EXP_ESCAL_loc,REBATE_CAP_ESCAL_loc, &
               NON_UTIL_COST_ESCAL,SERVICEMO_loc
      REAL ::  BASE_PROG_CAP,ONGO_PROG_CAP, &
           BASE_PROG_EXP,ONGO_PROG_EXP, &
           BASE_CUST_EXP,ONGO_CUST_EXP, &
           ONGO_NEW_CUST_EXP, &
           ONGO_KWH_EXP, &
           ONGO_KW_EXP, &
           BASE_CUST_CAP,ONGO_CUST_CAP, &
           ONGO_NEW_CUST_CAP, &
           ONGO_KWH_CAP, &
           ONGO_KW_CAP, &
           REBATE_CUST_EXP,REBATE_NEW_CUST_EXP,REBATE_CUST_CAP, &
           REBATE_NEW_CUST_CAP, &
           CONSTRUCTION_PERIOD,REGULATORY_ALLOCATOR, &
           BOKLF,TAXLF,ADRLIFE,DBRATE,TAXEXP
      REAL ::  PARTICIPANT_CUST_COST,PARTICIPANT_NEW_CUST_COST, &
           UTIL_NON_ELEC_CUST_COST,UTIL_NON_ELEC_NEW_CUST_COST, &
           THIRD_PARTY_CUST_COST,THIRD_PARTY_NEW_CUST_COST, &
           OTH_PARTICIPANT_CUST_COST,OTH_PARTICIPANT_NEW_CUST_COST
      INTEGER*2 PARTICIPANT_COST_ESCAL_loc, &
               UTIL_NON_ELEC_COST_ESCAL_loc, &
               THIRD_PARTY_COST_ESCAL_loc, &
               OTH_PARTICIPANT_COST_ESCAL_loc, &
               START_PROG_EXP_ESCAL_loc, &
               BASE_CUST_EXP_ESCAL_loc, &
               REBATE_NEW_EXP_ESCAL_loc, &
               START_PROG_CAP_ESCAL_loc, &
               BASE_CUST_CAP_ESCAL_loc, &
               REBATE_NEW_CAP_ESCAL_loc, &
               PARTICIPANT_NEW_COST_ESCAL_loc, &
               UTIL_NON_ELEC_NEW_COST_ESCAL_loc, &
               THIRD_PARTY_NEW_COST_ESCAL_loc, &
               OTH_PARTICIPANT_NEW_COST_ESCAL_loc, &
                ASSET_CLASS_NUM, &
                ASSET_CLASS_VECTOR
      CHARACTER (len=25) ::  FILE_TYPE='Load Management-Financial'
      CHARACTER (len=4) ::  DEPMET
      CHARACTER (len=1) ::  REG_TREAT,COLLECT
      CHARACTER (len=2) ::  LOAD_MAN_FIN_OL='BC'
      LOGICAL (kind=1) ::  LAHEY_LF95
      CHARACTER (len=30) ::  SCREEN_OUTPUT



!          ROUTINE TO CONVERT METAFILE FILES TO DIRECT-ACESS BINARY
!          COPYRIGHT (C) 1983, 84, 85  M.S. GERBER & ASSOCIATES, INC.


!
! CONVERT THE LOAD-MANAGEMENT FINANCIAL FILE
      ENTRY LF_MAKEBIN

      FILE_NAME = trim(BASE_FILE_DIRECTORY())// &
                                   "LFB"//trim(LD_FIN_FIL())//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//LD_FIN_FIL()
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
            CALL MG_LOCATE_WRITE(16,30,LD_FIN_FIL(),ALL_VERSIONS,0)
         ENDIF
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCLDFIN.BIN", &
                            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         IREC = 1
         READ(10,*) DELETE
         DO

            START_PROG_EXP_ESCAL_loc = -999
            BASE_CUST_EXP_ESCAL_loc = -999
            REBATE_NEW_EXP_ESCAL_loc = -999
            START_PROG_CAP_ESCAL_loc = -999
            BASE_CUST_CAP_ESCAL_loc = -999
            REBATE_NEW_CAP_ESCAL_loc = -999
            PARTICIPANT_NEW_COST_ESCAL_loc = -999
            UTIL_NON_ELEC_NEW_COST_ESCAL_loc = -999
            THIRD_PARTY_NEW_COST_ESCAL_loc = -999
            OTH_PARTICIPANT_NEW_COST_ESCAL_loc = -999
            ASSET_CLASS_NUM = 0
            ASSET_CLASS_VECTOR = 0

!
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(IOS /=0) EXIT
            RECLN = trim(RECLN)// &
                  ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
            READ(RECLN,*,ERR=200) DELETE,ACCTNO,DESC,DEVICE_NUM, &
               CONSTRUCTION_PERIOD,COLLECT, &
               BASE_PROG_EXP,ONGO_PROG_EXP,ONGO_PROG_EXP_ESCAL_loc, &
               BASE_CUST_EXP,ONGO_CUST_EXP,ONGO_CUST_EXP_ESCAL_loc, &
               ONGO_NEW_CUST_EXP,ONGO_NEW_CUST_EXP_ESCAL_loc, &
               ONGO_KWH_EXP,ONGO_KWH_EXP_ESCAL_loc, &
               ONGO_KW_EXP,ONGO_KW_EXP_ESCAL_loc, &
               BOKLF,TAXLF, &
               ADRLIFE,DEPMET,DBRATE, &
               SERVICEMO_loc,REG_TREAT, &
               TAXEXP,REGULATORY_ALLOCATOR, &
               BASE_PROG_CAP,ONGO_PROG_CAP,ONGO_PROG_CAP_ESCAL_loc, &
               BASE_CUST_CAP,ONGO_CUST_CAP,ONGO_CUST_CAP_ESCAL_loc, &
               ONGO_NEW_CUST_CAP,ONGO_NEW_CUST_CAP_ESCAL_loc, &
               ONGO_KWH_CAP,ONGO_KWH_CAP_ESCAL_loc, &
               ONGO_KW_CAP,ONGO_KW_CAP_ESCAL_loc, &
               REBATE_CUST_EXP,REBATE_NEW_CUST_EXP,REBATE_EXP_ESCAL_loc, &
               REBATE_CUST_CAP,REBATE_NEW_CUST_CAP,REBATE_CAP_ESCAL_loc, &
               COMMENT,PARTICIPANT_CUST_COST,PARTICIPANT_NEW_CUST_COST, &
               PARTICIPANT_COST_ESCAL_loc, &
               UTIL_NON_ELEC_CUST_COST,UTIL_NON_ELEC_NEW_CUST_COST, &
               UTIL_NON_ELEC_COST_ESCAL_loc, &
               THIRD_PARTY_CUST_COST,THIRD_PARTY_NEW_CUST_COST, &
               THIRD_PARTY_COST_ESCAL_loc, &
               OTH_PARTICIPANT_CUST_COST,OTH_PARTICIPANT_NEW_CUST_COST, &
              OTH_PARTICIPANT_COST_ESCAL_loc, &
              START_PROG_EXP_ESCAL_loc, &
              BASE_CUST_EXP_ESCAL_loc, &
              REBATE_NEW_EXP_ESCAL_loc, &
              START_PROG_CAP_ESCAL_loc, &
              BASE_CUST_CAP_ESCAL_loc, &
              REBATE_NEW_CAP_ESCAL_loc, &
              PARTICIPANT_NEW_COST_ESCAL_loc, &
              UTIL_NON_ELEC_NEW_COST_ESCAL_loc, &
              THIRD_PARTY_NEW_COST_ESCAL_loc, &
              OTH_PARTICIPANT_NEW_COST_ESCAL_loc, &
               ASSET_CLASS_NUM, &
               ASSET_CLASS_VECTOR

!
            IF(START_PROG_EXP_ESCAL_loc == -999) then
                     START_PROG_EXP_ESCAL_loc = ONGO_PROG_EXP_ESCAL_loc
            end if
     
            IF(BASE_CUST_EXP_ESCAL_loc == -999) then
                      BASE_CUST_EXP_ESCAL_loc = ONGO_CUST_EXP_ESCAL_loc
            end if
     
            IF(REBATE_NEW_EXP_ESCAL_loc == -999) then
                        REBATE_NEW_EXP_ESCAL_loc = REBATE_EXP_ESCAL_loc
            end if
     
            IF(START_PROG_CAP_ESCAL_loc == -999) then
                     START_PROG_CAP_ESCAL_loc = ONGO_PROG_CAP_ESCAL_loc
            end if
     
            IF(BASE_CUST_CAP_ESCAL_loc == -999) then
                      BASE_CUST_CAP_ESCAL_loc = ONGO_CUST_CAP_ESCAL_loc
            end if
     
            IF(REBATE_NEW_CAP_ESCAL_loc == -999) then
                        REBATE_NEW_CAP_ESCAL_loc = REBATE_CAP_ESCAL_loc
            end if
     
            IF(PARTICIPANT_NEW_COST_ESCAL_loc == -999) then
                 PARTICIPANT_NEW_COST_ESCAL_loc = PARTICIPANT_COST_ESCAL_loc
            end if
     
            IF(UTIL_NON_ELEC_NEW_COST_ESCAL_loc == -999) then
        UTIL_NON_ELEC_NEW_COST_ESCAL_loc = UTIL_NON_ELEC_COST_ESCAL_loc
            end if
     
            IF(THIRD_PARTY_NEW_COST_ESCAL_loc == -999) then
            	THIRD_PARTY_NEW_COST_ESCAL_loc = THIRD_PARTY_COST_ESCAL_loc
            end if
     
            IF(OTH_PARTICIPANT_NEW_COST_ESCAL_loc == -999) then
      			OTH_PARTICIPANT_NEW_COST_ESCAL_loc= &
      				OTH_PARTICIPANT_COST_ESCAL_loc
            end if
     
!
            WRITE(11,REC=IREC) DELETE,ACCTNO,DESC,DEVICE_NUM, &
               CONSTRUCTION_PERIOD,COLLECT, &
               BASE_PROG_EXP,ONGO_PROG_EXP,ONGO_PROG_EXP_ESCAL_loc, &
               BASE_CUST_EXP,ONGO_CUST_EXP,ONGO_CUST_EXP_ESCAL_loc, &
               ONGO_NEW_CUST_EXP,ONGO_NEW_CUST_EXP_ESCAL_loc, &
               ONGO_KWH_EXP,ONGO_KWH_EXP_ESCAL_loc, &
               ONGO_KW_EXP,ONGO_KW_EXP_ESCAL_loc, &
               BOKLF,TAXLF, &
               ADRLIFE,DEPMET,DBRATE, &
               SERVICEMO_loc,REG_TREAT, &
               TAXEXP,REGULATORY_ALLOCATOR, &
               BASE_PROG_CAP,ONGO_PROG_CAP,ONGO_PROG_CAP_ESCAL_loc, &
               BASE_CUST_CAP,ONGO_CUST_CAP,ONGO_CUST_CAP_ESCAL_loc, &
               ONGO_NEW_CUST_CAP,ONGO_NEW_CUST_CAP_ESCAL_loc, &
               ONGO_KWH_CAP,ONGO_KWH_CAP_ESCAL_loc, &
               ONGO_KW_CAP,ONGO_KW_CAP_ESCAL_loc, &
               REBATE_CUST_EXP,REBATE_NEW_CUST_EXP,REBATE_EXP_ESCAL_loc, &
               REBATE_CUST_CAP,REBATE_NEW_CUST_CAP,REBATE_CAP_ESCAL_loc, &
               PARTICIPANT_CUST_COST,PARTICIPANT_NEW_CUST_COST, &
               PARTICIPANT_COST_ESCAL_loc, &
               UTIL_NON_ELEC_CUST_COST,UTIL_NON_ELEC_NEW_CUST_COST, &
               UTIL_NON_ELEC_COST_ESCAL_loc, &
               THIRD_PARTY_CUST_COST,THIRD_PARTY_NEW_CUST_COST, &
               THIRD_PARTY_COST_ESCAL_loc, &
               OTH_PARTICIPANT_CUST_COST,OTH_PARTICIPANT_NEW_CUST_COST, &
               OTH_PARTICIPANT_COST_ESCAL_loc, &
               START_PROG_EXP_ESCAL_loc, &
               BASE_CUST_EXP_ESCAL_loc, &
               REBATE_NEW_EXP_ESCAL_loc, &
               START_PROG_CAP_ESCAL_loc, &
               BASE_CUST_CAP_ESCAL_loc, &
               REBATE_NEW_CAP_ESCAL_loc, &
               PARTICIPANT_NEW_COST_ESCAL_loc, &
               UTIL_NON_ELEC_NEW_COST_ESCAL_loc, &
               THIRD_PARTY_NEW_COST_ESCAL_loc, &
               OTH_PARTICIPANT_NEW_COST_ESCAL_loc, &
               ASSET_CLASS_NUM, &
               ASSET_CLASS_VECTOR
            IREC = IREC + 1
         ENDDO
         CLOSE(10)
         CLOSE(11)
         IREC = IREC - 1
         IREC =  STORE_FINANCIAL(IREC)
      ELSEIF(INDEX(LD_FIN_FIL(),'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "lmg_objt:1111")
      ENDIF
      RETURN



!          ROUTINE TO CREATE OVERLAY FILES
!          COPYRIGHT (C) 1984-88  M.S. GERBER & ASSOCIATES, INC.
!          COPYRIGHT (C) 1991-92  M.S. GERBER & ASSOCIATES, INC.



! OVERLAY THE LOAD-MANAGEMENT FINANCIAL FILE
      ENTRY LF_MAKEOVL(OVERLAY_FAMILY_NAME)

      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      FILE_NAME=trim(OUTPUT_DIRECTORY())//"LFO"// &
                                     trim(OVERLAY_FAMILY_NAME)//".DAT"
      OPEN(10,FILE=FILE_NAME)
      READ(10,*) DELETE
      INUNIT = 12
      IF(LOAD_MAN_FIN_OL == 'BC') THEN
         OPEN(11,FILE=trim(OUTPUT_DIRECTORY())//"BCLDFIN.BIN", &
                                             ACCESS="DIRECT",RECL=LRECL)
         INUNIT = 11
      ENDIF
      OPEN(12,FILE=trim(OUTPUT_DIRECTORY())//"OLLDFIN.BIN", &
                            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      IREC = 0
      DO
         IREC = IREC + 1
         READ(INUNIT,REC=IREC,IOSTAT=IOS) DELETE,ACCTNO,DESC, &
            DEVICE_NUM,CONSTRUCTION_PERIOD,COLLECT, &
            BASE_PROG_EXP,ONGO_PROG_EXP,ONGO_PROG_EXP_ESCAL_loc, &
            BASE_CUST_EXP,ONGO_CUST_EXP,ONGO_CUST_EXP_ESCAL_loc, &
            ONGO_NEW_CUST_EXP,ONGO_NEW_CUST_EXP_ESCAL_loc, &
            ONGO_KWH_EXP,ONGO_KWH_EXP_ESCAL_loc, &
            ONGO_KW_EXP,ONGO_KW_EXP_ESCAL_loc, &
            BOKLF,TAXLF, &
            ADRLIFE,DEPMET,DBRATE, &
            SERVICEMO_loc,REG_TREAT, &
            TAXEXP,REGULATORY_ALLOCATOR, &
            BASE_PROG_CAP,ONGO_PROG_CAP,ONGO_PROG_CAP_ESCAL_loc, &
            BASE_CUST_CAP,ONGO_CUST_CAP,ONGO_CUST_CAP_ESCAL_loc, &
            ONGO_NEW_CUST_CAP,ONGO_NEW_CUST_CAP_ESCAL_loc, &
            ONGO_KWH_CAP,ONGO_KWH_CAP_ESCAL_loc, &
            ONGO_KW_CAP,ONGO_KW_CAP_ESCAL_loc, &
            REBATE_CUST_EXP,REBATE_NEW_CUST_EXP,REBATE_EXP_ESCAL_loc, &
            REBATE_CUST_CAP,REBATE_NEW_CUST_CAP,REBATE_CAP_ESCAL_loc, &
            PARTICIPANT_CUST_COST,PARTICIPANT_NEW_CUST_COST, &
            PARTICIPANT_COST_ESCAL_loc, &
            UTIL_NON_ELEC_CUST_COST,UTIL_NON_ELEC_NEW_CUST_COST, &
            UTIL_NON_ELEC_COST_ESCAL_loc, &
            THIRD_PARTY_CUST_COST,THIRD_PARTY_NEW_CUST_COST, &
            THIRD_PARTY_COST_ESCAL_loc, &
            OTH_PARTICIPANT_CUST_COST,OTH_PARTICIPANT_NEW_CUST_COST, &
            OTH_PARTICIPANT_COST_ESCAL_loc, &
            START_PROG_EXP_ESCAL_loc, &
            BASE_CUST_EXP_ESCAL_loc, &
            REBATE_NEW_EXP_ESCAL_loc, &
            START_PROG_CAP_ESCAL_loc, &
            BASE_CUST_CAP_ESCAL_loc, &
            REBATE_NEW_CAP_ESCAL_loc, &
            PARTICIPANT_NEW_COST_ESCAL_loc, &
            UTIL_NON_ELEC_NEW_COST_ESCAL_loc, &
            THIRD_PARTY_NEW_COST_ESCAL_loc, &
            OTH_PARTICIPANT_NEW_COST_ESCAL_loc, &
            ASSET_CLASS_NUM, &
            ASSET_CLASS_VECTOR
         IF(IOS /= 0) EXIT
         READ(10,1000,IOSTAT=IOS) RECLN
         IF(IOS == 0) THEN
            RECLN = trim(RECLN)// &
                  ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
            READ(RECLN,*,ERR=200) DELETE,ACCTNO,DESC,DEVICE_NUM, &
               CONSTRUCTION_PERIOD,COLLECT, &
              BASE_PROG_EXP,ONGO_PROG_EXP,ONGO_PROG_EXP_ESCAL_loc, &
              BASE_CUST_EXP,ONGO_CUST_EXP,ONGO_CUST_EXP_ESCAL_loc, &
              ONGO_NEW_CUST_EXP,ONGO_NEW_CUST_EXP_ESCAL_loc, &
              ONGO_KWH_EXP,ONGO_KWH_EXP_ESCAL_loc, &
              ONGO_KW_EXP,ONGO_KW_EXP_ESCAL_loc, &
               BOKLF,TAXLF, &
               ADRLIFE,DEPMET,DBRATE, &
               SERVICEMO_loc,REG_TREAT, &
               TAXEXP,REGULATORY_ALLOCATOR, &
              BASE_PROG_CAP,ONGO_PROG_CAP,ONGO_PROG_CAP_ESCAL_loc, &
              BASE_CUST_CAP,ONGO_CUST_CAP,ONGO_CUST_CAP_ESCAL_loc, &
              ONGO_NEW_CUST_CAP,ONGO_NEW_CUST_CAP_ESCAL_loc, &
              ONGO_KWH_CAP,ONGO_KWH_CAP_ESCAL_loc, &
              ONGO_KW_CAP,ONGO_KW_CAP_ESCAL_loc, &
              REBATE_CUST_EXP,REBATE_NEW_CUST_EXP,REBATE_EXP_ESCAL_loc, &
              REBATE_CUST_CAP,REBATE_NEW_CUST_CAP,REBATE_CAP_ESCAL_loc, &
               COMMENT,PARTICIPANT_CUST_COST,PARTICIPANT_NEW_CUST_COST, &
              PARTICIPANT_COST_ESCAL_loc, &
               UTIL_NON_ELEC_CUST_COST,UTIL_NON_ELEC_NEW_CUST_COST, &
              UTIL_NON_ELEC_COST_ESCAL_loc, &
               THIRD_PARTY_CUST_COST,THIRD_PARTY_NEW_CUST_COST, &
              THIRD_PARTY_COST_ESCAL_loc, &
               OTH_PARTICIPANT_CUST_COST,OTH_PARTICIPANT_NEW_CUST_COST, &
              OTH_PARTICIPANT_COST_ESCAL_loc, &
              START_PROG_EXP_ESCAL_loc, &
              BASE_CUST_EXP_ESCAL_loc, &
              REBATE_NEW_EXP_ESCAL_loc, &
              START_PROG_CAP_ESCAL_loc, &
              BASE_CUST_CAP_ESCAL_loc, &
              REBATE_NEW_CAP_ESCAL_loc, &
              PARTICIPANT_NEW_COST_ESCAL_loc, &
              UTIL_NON_ELEC_NEW_COST_ESCAL_loc, &
              THIRD_PARTY_NEW_COST_ESCAL_loc, &
              OTH_PARTICIPANT_NEW_COST_ESCAL_loc, &
               ASSET_CLASS_NUM, &
               ASSET_CLASS_VECTOR
         ENDIF
         WRITE(12,REC=IREC) DELETE,ACCTNO,DESC,DEVICE_NUM, &
            CONSTRUCTION_PERIOD,COLLECT, &
           BASE_PROG_EXP,ONGO_PROG_EXP,ONGO_PROG_EXP_ESCAL_loc, &
           BASE_CUST_EXP,ONGO_CUST_EXP,ONGO_CUST_EXP_ESCAL_loc, &
           ONGO_NEW_CUST_EXP,ONGO_NEW_CUST_EXP_ESCAL_loc, &
           ONGO_KWH_EXP,ONGO_KWH_EXP_ESCAL_loc, &
           ONGO_KW_EXP,ONGO_KW_EXP_ESCAL_loc, &
            BOKLF,TAXLF,ADRLIFE,DEPMET,DBRATE, &
           SERVICEMO_loc,REG_TREAT, &
            TAXEXP,REGULATORY_ALLOCATOR, &
            BASE_PROG_CAP,ONGO_PROG_CAP,ONGO_PROG_CAP_ESCAL_loc, &
            BASE_CUST_CAP,ONGO_CUST_CAP,ONGO_CUST_CAP_ESCAL_loc, &
            ONGO_NEW_CUST_CAP,ONGO_NEW_CUST_CAP_ESCAL_loc, &
            ONGO_KWH_CAP,ONGO_KWH_CAP_ESCAL_loc, &
            ONGO_KW_CAP,ONGO_KW_CAP_ESCAL_loc, &
            REBATE_CUST_EXP,REBATE_NEW_CUST_EXP,REBATE_EXP_ESCAL_loc, &
            REBATE_CUST_CAP,REBATE_NEW_CUST_CAP,REBATE_CAP_ESCAL_loc, &
            PARTICIPANT_CUST_COST,PARTICIPANT_NEW_CUST_COST, &
           PARTICIPANT_COST_ESCAL_loc, &
            UTIL_NON_ELEC_CUST_COST,UTIL_NON_ELEC_NEW_CUST_COST, &
           UTIL_NON_ELEC_COST_ESCAL_loc, &
            THIRD_PARTY_CUST_COST,THIRD_PARTY_NEW_CUST_COST, &
           THIRD_PARTY_COST_ESCAL_loc, &
            OTH_PARTICIPANT_CUST_COST,OTH_PARTICIPANT_NEW_CUST_COST, &
           OTH_PARTICIPANT_COST_ESCAL_loc, &
           START_PROG_EXP_ESCAL_loc, &
           BASE_CUST_EXP_ESCAL_loc, &
           REBATE_NEW_EXP_ESCAL_loc, &
           START_PROG_CAP_ESCAL_loc, &
           BASE_CUST_CAP_ESCAL_loc, &
           REBATE_NEW_CAP_ESCAL_loc, &
           PARTICIPANT_NEW_COST_ESCAL_loc, &
           UTIL_NON_ELEC_NEW_COST_ESCAL_loc, &
           THIRD_PARTY_NEW_COST_ESCAL_loc, &
           OTH_PARTICIPANT_NEW_COST_ESCAL_loc, &
            ASSET_CLASS_NUM, &
            ASSET_CLASS_VECTOR
      ENDDO
      CLOSE(10)
      CLOSE(12)
      IF(LOAD_MAN_FIN_OL == 'BC') CLOSE(11)
      LOAD_MAN_FIN_OL = 'OL'
      RETURN


  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from LMG_OBJT SIID185'
      call end_program(er_message)

      ENTRY RESET_LOAD_MAN_FIN_OL
         LOAD_MAN_FIN_OL = 'BC'
      RETURN

      ENTRY OPEN_DSM_FINANCIAL_FILE(UNIT_NO)
         OPEN(UNIT_NO,ACCESS='DIRECT',RECL=LRECL, &
          FILE=trim(OUTPUT_DIRECTORY())//LOAD_MAN_FIN_OL//"LDFIN.BIN")
      RETURN
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END

! DSM FUNCTION TO FUNCTIONALIZE IMPORTANT DSM VARIABLES

      FUNCTION DSM_IMPORTANT_STUFF()

      INTEGER (kind=2) ::  MAX_DEVICE_NUM
      LOGICAL (kind=1) ::  DSM_IMPORTANT_STUFF
      INTEGER (kind=2) ::  MAX_DSM_PROGRAMS=0 ,STORE_MAX_PROGRAM_NUM, &
                STORE_BASE_MAX_PROGRAM_NUM,MAX_BASE_DSM_PROGRAMS=0 , &
                RESET_DSM_MAX_PROGRAM_NUM
      INTEGER (kind=2) ::  MAX_DSM_RESPONSES=0 ,STORE_MAX_RESPONSE_NUM
      LOGICAL (kind=1) ::  ACCEPTANCE_ACTIVE
      LOGICAL (kind=1) ::  RESPONSE_ACTIVE,LMF_FILE_EXISTS
      LOGICAL (kind=1) ::  DSM_ACCEPTANCE_ACTIVE=.FALSE. , &
                DSM_RESPONSE_ACTIVE=.FALSE. , &
                DSM_FINANCIAL_ACTIVE=.FALSE.
      INTEGER (kind=2) ::  STORE_ACCEPTANCE
      INTEGER (kind=2) ::  STORE_RESPONSE,STORE_FINANCIAL,RECORDS
      INTEGER (kind=2) ::  DSM_ACCEPTANCE_RECORDS=0
      INTEGER (kind=2) ::  DSM_RESPONSE_RECORDS=0 , &
                DSM_FINANCIAL_RECORDS=0
      DSM_IMPORTANT_STUFF = .TRUE.
      RETURN
      ENTRY STORE_ACCEPTANCE(RECORDS)
         DSM_ACCEPTANCE_RECORDS = RECORDS
         DSM_ACCEPTANCE_ACTIVE = DSM_ACCEPTANCE_RECORDS > 0
         STORE_ACCEPTANCE = DSM_ACCEPTANCE_RECORDS
      RETURN
      ENTRY STORE_BASE_MAX_PROGRAM_NUM(MAX_DEVICE_NUM)
         MAX_BASE_DSM_PROGRAMS = MAX_DEVICE_NUM
         MAX_DSM_PROGRAMS = MAX_DEVICE_NUM
         STORE_BASE_MAX_PROGRAM_NUM = MAX_BASE_DSM_PROGRAMS
      RETURN
      ENTRY STORE_MAX_PROGRAM_NUM(MAX_DEVICE_NUM)
         MAX_DSM_PROGRAMS = MAX_DEVICE_NUM
         STORE_MAX_PROGRAM_NUM = MAX_DSM_PROGRAMS
      RETURN
      ENTRY RESET_DSM_MAX_PROGRAM_NUM
         MAX_DSM_PROGRAMS = MAX_BASE_DSM_PROGRAMS
         RESET_DSM_MAX_PROGRAM_NUM = MAX_BASE_DSM_PROGRAMS
      RETURN
      ENTRY STORE_RESPONSE(RECORDS)
         DSM_RESPONSE_RECORDS = RECORDS
         DSM_RESPONSE_ACTIVE = DSM_RESPONSE_RECORDS > 0
         STORE_RESPONSE = DSM_RESPONSE_RECORDS
      RETURN
      ENTRY STORE_MAX_RESPONSE_NUM(MAX_DEVICE_NUM)
         MAX_DSM_RESPONSES = MAX_DEVICE_NUM
         STORE_MAX_RESPONSE_NUM = MAX_DSM_RESPONSES
      RETURN
      ENTRY STORE_FINANCIAL(RECORDS)
         DSM_FINANCIAL_RECORDS = RECORDS
         DSM_FINANCIAL_ACTIVE = DSM_FINANCIAL_RECORDS > 0
         STORE_FINANCIAL = DSM_FINANCIAL_RECORDS
      RETURN
      ENTRY ACCEPTANCE_ACTIVE(RECORDS,MAX_DEVICE_NUM)
         RECORDS = DSM_ACCEPTANCE_RECORDS
         MAX_DEVICE_NUM = MAX_DSM_PROGRAMS
         ACCEPTANCE_ACTIVE = DSM_ACCEPTANCE_ACTIVE
      RETURN
      ENTRY RESPONSE_ACTIVE(RECORDS,MAX_DEVICE_NUM)
         RECORDS = DSM_RESPONSE_RECORDS
         MAX_DEVICE_NUM = MAX_DSM_RESPONSES
         RESPONSE_ACTIVE = DSM_RESPONSE_ACTIVE
      RETURN
      ENTRY LMF_FILE_EXISTS(RECORDS)
         RECORDS = DSM_FINANCIAL_RECORDS
         LMF_FILE_EXISTS = DSM_FINANCIAL_ACTIVE
      RETURN
      END
