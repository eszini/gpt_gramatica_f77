!     ******************************************************************
!     FI_OBJT.FOR
!     Copyright(c) Global Energy Decisions 2000
!
!     Created: 11/7/2006 10:02:31 AM
!     Author : Tom Sweet
!     Last change: TS  11/7/2006 10:02:31 AM
!     ******************************************************************

      SUBROUTINE FI_OBJECT
      use end_routine, only: end_program, er_message
	  use filename_tracker
	  use sizecom
!
      INCLUDE 'SpinLib.MON'
      INTEGER(kind=2) :: DELETE,INUNIT,IREC,LRECL=384
      INTEGER(kind=2) :: UNIT_NUM=10
      INTEGER :: IOS,IOS_BASE
      INTEGER(kind=4) :: R_UNIT_NUM
      CHARACTER(len=5) :: BASE_FILE_NAME,OVERLAY_FAMILY_NAME,FUEL_INVENTORY_FILE
      CHARACTER(len=256) :: FILE_NAME
      CHARACTER(len=256) :: BASE_FILE_DIRECTORY
      CHARACTER(len=256) :: DATA_DRIVE,OUTPUT_DIRECTORY
      LOGICAL(kind=4) :: FILE_EXISTS=.FALSE.,R_FUEL_INVENTORY_ACTIVE
!  FUEL PRICE VARIABLES
      INTEGER(kind=2) :: START_MO,END_MO
!  DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER(len=1024) :: RECLN
!  DECLARATION FOR /FUEL INVENTORY FILE/
      INTEGER(kind=2) :: FUEL_ID,FUEL_GROUP
      INTEGER(kind=2) :: PENALTY_ESCALATION_VECTOR
      REAL :: TAKE_OR_PAY_PENALTY
      REAL(kind=8) :: ANNUAL_AMOUNTS(31),BTU_CONTENT
      CHARACTER(len=50) :: COMMENT
      CHARACTER(len=30) :: DESC
      CHARACTER(len=14) :: FILE_TYPE='Fuel Inventory'
      CHARACTER(len=1) :: SHIPPING_UNITS,FUEL_STRATEGY,INVENTORY_TREATMENT
      CHARACTER(len=2) :: FUEL_INVENTORY_OL='BC'
      LOGICAL(kind=1) :: LAHEY_LF95
      CHARACTER(len=30) :: SCREEN_OUTPUT

! ***********************************************************************
!
!    ROUTINE TO CONVERT PRODUCTION PARAMETER FILE DATA
!
!                              COPYRIGHT (C) 1992
!                         M.S. GERBER & ASSOCIATES, INC.
!                              ALL RIGHTS RESERVED
!
! ***********************************************************************
!
!  CONVERT THE FUEL-INVENTORY FILE
      ENTRY FI_MAKEBIN
      BASE_FILE_NAME = FUEL_INVENTORY_FILE()
      DATA_DRIVE = OUTPUT_DIRECTORY()


	  file_name=get_fib_filename(base_file_name)
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
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCFUELIN.BIN", &
                            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         IREC = 1
         READ(10,*) DELETE
         DO
            START_MO = 0
            END_MO = 0
            TAKE_OR_PAY_PENALTY = 0
            PENALTY_ESCALATION_VECTOR = 0
            INVENTORY_TREATMENT = 'R'
            DO
               READ(10,1000,IOSTAT=IOS) RECLN
               IF(IOS /= 0) EXIT
               IF(RECLN(1:1) == '7') EXIT
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,'
               READ(RECLN,*,ERR=300) DELETE,FUEL_ID,FUEL_GROUP, &
                                     SHIPPING_UNITS,ANNUAL_AMOUNTS, &
                                     DESC,COMMENT,BTU_CONTENT, &
                                     FUEL_STRATEGY, &
                                     START_MO,END_MO, &
                                     TAKE_OR_PAY_PENALTY, &
                                     PENALTY_ESCALATION_VECTOR, &
                                     INVENTORY_TREATMENT
               WRITE(11,REC=IREC) DELETE,DESC,FUEL_ID,FUEL_GROUP, &
                                  SHIPPING_UNITS,ANNUAL_AMOUNTS, &
                                  BTU_CONTENT,FUEL_STRATEGY, &
                                  START_MO,END_MO, &
                                  TAKE_OR_PAY_PENALTY, &
                                  PENALTY_ESCALATION_VECTOR, &
                                  INVENTORY_TREATMENT
               IREC = IREC + 1
            ENDDO
            IF(IOS /= 0) EXIT
         ENDDO
         CLOSE(10)
!          ENDFILE(11)
         CLOSE(11)
      ELSE IF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "fi_objt:1111")
      ENDIF
      RETURN
!
!  OVERLAY THE FUEL-INVENTORY FILE
      ENTRY FI_MAKEOVL(OVERLAY_FAMILY_NAME)
      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      DATA_DRIVE = OUTPUT_DIRECTORY()
	  file_name=get_makeovl_filename(trim(data_drive), "FIO", &
        trim(overlay_family_name))
      OPEN(10,FILE=FILE_NAME)
      READ(10,*) DELETE
      INUNIT = 12
      IF(FUEL_INVENTORY_OL == 'BC') THEN
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCFUELIN.BIN", &
                                             ACCESS="DIRECT",RECL=LRECL)
         INUNIT = 11
      ENDIF
      OPEN(12,FILE=trim(DATA_DRIVE)//"OLFUELIN.BIN",ACCESS="DIRECT", &
                                            STATUS="UNKNOWN",RECL=LRECL)
      IREC = 0
      DO
         DO
            READ(10,1000,IOSTAT=IOS) RECLN
!             IF(IOS /= 0) EXIT
            IF(RECLN(1:1) == '7') EXIT
            IREC = IREC + 1
            READ(INUNIT,REC=IREC,IOSTAT=IOS_BASE) DELETE, &
                            DESC,FUEL_ID,FUEL_GROUP, &
                            SHIPPING_UNITS,ANNUAL_AMOUNTS, &
                            BTU_CONTENT,FUEL_STRATEGY, &
                            START_MO,END_MO, &
                            TAKE_OR_PAY_PENALTY, &
                            PENALTY_ESCALATION_VECTOR, &
                            INVENTORY_TREATMENT
            IF(IOS_BASE /= 0) EXIT
            IF(IOS == 0) THEN
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,ERR=300) DELETE, &
                                     FUEL_ID,FUEL_GROUP, &
                                     SHIPPING_UNITS,ANNUAL_AMOUNTS, &
                                     DESC,COMMENT,BTU_CONTENT, &
                                     FUEL_STRATEGY, &
                                     START_MO,END_MO, &
                                     TAKE_OR_PAY_PENALTY, &
                                     PENALTY_ESCALATION_VECTOR, &
                                     INVENTORY_TREATMENT
            ENDIF
            WRITE(12,REC=IREC) DELETE, &
                               DESC,FUEL_ID,FUEL_GROUP, &
                               SHIPPING_UNITS,ANNUAL_AMOUNTS, &
                               BTU_CONTENT,FUEL_STRATEGY, &
                               START_MO,END_MO, &
                               TAKE_OR_PAY_PENALTY, &
                               PENALTY_ESCALATION_VECTOR, &
                               INVENTORY_TREATMENT
         ENDDO
         IF(IOS_BASE /= 0) EXIT
      ENDDO
      CLOSE(10)
      CLOSE(12)
      IF(FUEL_INVENTORY_OL == 'BC') CLOSE(11)
      FUEL_INVENTORY_OL = 'OL'
      RETURN
!

  300 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      CALL MG_LOCATE_WRITE(21,0, &
                       'Error reading Fuel Inventory record. Look for'// &
                                          ' a "," in a character name.', &
                           ALL_VERSIONS,1)
      er_message='stop requested from FI_OBJT SIID129'
      call end_program(er_message)
!
      ENTRY RESET_FUEL_INVENTORY_OL
         FUEL_INVENTORY_OL = 'BC'
      RETURN
!
      ENTRY OPEN_FUEL_INVENTORY_FILE(R_UNIT_NUM,R_FUEL_INVENTORY_ACTIVE)
         UNIT_NUM = R_UNIT_NUM
         R_FUEL_INVENTORY_ACTIVE = FILE_EXISTS
         IF(FILE_EXISTS) OPEN(UNIT_NUM,FILE=trim(OUTPUT_DIRECTORY())// &
                        FUEL_INVENTORY_OL//"FUELIN.BIN",ACCESS="DIRECT", &
                                            STATUS="UNKNOWN",RECL=LRECL)
      RETURN
!
      ENTRY CLOSE_FUEL_INVENTORY_FILE
         IF(FILE_EXISTS) CLOSE(UNIT_NUM)
      RETURN
!
 1000 FORMAT(A)
 1010 FORMAT('&',A)
      END

! Last commit as EPA-8977