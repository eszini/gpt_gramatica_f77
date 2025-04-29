!     ******************************************************************
!     FC_OBJT.FOR
!     Copyright(c) M.S. Gerber & Associates, Inc. 2000
!
!     Created: 8/5/2005 3:01:58 PM
!     Author : TOM SWEET
!     Last change: GT 9/12/2007 5:31:25 PM
!     ******************************************************************

      SUBROUTINE FC_OBJECT
      use end_routine, only: end_program, er_message
	  use filename_tracker
!
      INCLUDE 'SpinLib.MON'
      USE SIZECOM
      LOGICAL(kind=1) :: LOAD_FUEL_PRICES
      INTEGER(kind=2) :: DELETE,INUNIT,IREC,LRECL=69 ! init value => SAVE
      INTEGER(kind=2) :: UNIT_NUM=10
      INTEGER(kind=4) :: IOS,IOS_BASE
      INTEGER(kind=4) :: R_UNIT_NUM
      CHARACTER(len=1) :: RECORD_ACTIVE
      CHARACTER(len=5) :: BASE_FILE_NAME,OVERLAY_FAMILY_NAME, &
	              FUEL_PRICE_FILE, &
                  FUEL_PRICE_FILE_F1, &
                  FUEL_PRICE_FILE_F2, &
                  FUEL_PRICE_FILE_F3, &
                  FUEL_PRICE_FILE_F4, &
                  FUEL_PRICE_FILE_F5, &
                  FUEL_PRICE_FILE_F6, &
                  FUEL_PRICE_FILE_F7, &
                  FUEL_PRICE_FILE_F8, &
                  FUEL_PRICE_FILE_F9, &
                  FUEL_PRICE_FILE_F0
      CHARACTER(len=256) :: FILE_NAME
      CHARACTER(len=256) :: BASE_FILE_DIRECTORY
      CHARACTER(len=256) :: DATA_DRIVE,OUTPUT_DIRECTORY
      LOGICAL(kind=4) :: FILE_EXISTS
!  DECLARATION FOR FUEL PRICE VARIABLES
      INTEGER(kind=2) :: CHANGE_YEAR,START_MO,END_MO,I,FUEL_GROUP
      REAL :: DUMMY
      LOGICAL(kind=1) :: R_DUMMY_1,R_DUMMY_2
      CHARACTER(len=1) :: DATA_TYPE,PERCENT_OR_ESCALATION
      REAL :: FUEL_PRICE(:),WRITE_FUEL_PRICE(:)
      ALLOCATABLE :: FUEL_PRICE,WRITE_FUEL_PRICE
!  DECLARATION FOR /FUEL INVENTORY FILE/
      REAL(kind=8) :: BTU_CONTENT
      CHARACTER(len=1) :: SHIPPING_UNITS
      CHARACTER(len=30) :: DESC
      CHARACTER(len=50) :: COMMENT
      INTEGER(kind=2) :: FUEL_ID
!  DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER(len=1024) :: RECLN
!  DECLARATION FOR LOCALS
      CHARACTER(len=10) :: FILE_TYPE='Fuel Prices'
      CHARACTER(len=2) :: FUEL_PRICE_OL(0:10)='BC', &
                  LAST_FUEL_PRICE_OL_TYPE(0:10)='OL'
      INTEGER :: FILE_NUMBER,FILE_ID
      INTEGER :: MAX_FUEL_FILES=11 ! 1 MEANS ONE FILE THOUGHT THEY INDEX 0-9
      INTEGER :: R_MAX_FUEL_FILES
      CHARACTER(len=5) :: FUEL_FILE_BASE_NAME(0:10)
      CHARACTER(len=2) :: FUEL_FILE_CODE(0:10)=(/'FC','F1','F2','F3','F4', &
                                      'F5','F6','F7','F8','F9', &
                                      'F0'/), &
                  FILE_CODE
      CHARACTER(len=6) :: FUEL_FILE_BINARY_NAME(0:10)=(/'FUELFC', &
                                             'FUELF1', &
                                             'FUELF2', &
                                             'FUELF3', &
                                             'FUELF4', &
                                             'FUELF5', &
                                             'FUELF6', &
                                             'FUELF7', &
                                             'FUELF8', &
                                             'FUELF9', &
                                             'FUELF0'/), &
                  BINARY_FILE_NAME
      LOGICAL :: BASE_FUEL_FILE_ACTIVE(0:11)=.FALSE., &
              OVERLAY_FUEL_FILE_ACTIVE(0:10)=.FALSE.
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
!  CONVERT THE FUEL-PRICE FILE
      ENTRY FC_MAKEBIN
      FUEL_FILE_BASE_NAME(0) = FUEL_PRICE_FILE()
      FUEL_FILE_BASE_NAME(1) = FUEL_PRICE_FILE_F1()
      FUEL_FILE_BASE_NAME(2) = FUEL_PRICE_FILE_F2()
      FUEL_FILE_BASE_NAME(3) = FUEL_PRICE_FILE_F3()
      FUEL_FILE_BASE_NAME(4) = FUEL_PRICE_FILE_F4()
      FUEL_FILE_BASE_NAME(5) = FUEL_PRICE_FILE_F5()
      FUEL_FILE_BASE_NAME(6) = FUEL_PRICE_FILE_F6()
      FUEL_FILE_BASE_NAME(7) = FUEL_PRICE_FILE_F7()
      FUEL_FILE_BASE_NAME(8) = FUEL_PRICE_FILE_F8()
      FUEL_FILE_BASE_NAME(9) = FUEL_PRICE_FILE_F9()
      FUEL_FILE_BASE_NAME(10) = FUEL_PRICE_FILE_F0()
      LRECL = 65+4*(AVAIL_DATA_YEARS+1)
      IF(.NOT. LAHEY_LF95()) &
              CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
      DATA_DRIVE = OUTPUT_DIRECTORY()
!
!
      DO FILE_ID = 0, MAX_FUEL_FILES-1

         BASE_FUEL_FILE_ACTIVE(FILE_ID) = .FALSE.
         BASE_FILE_NAME = FUEL_FILE_BASE_NAME(FILE_ID)
         IF(INDEX(BASE_FILE_NAME,'NONE') /= 0) CYCLE
         FILE_CODE = FUEL_FILE_CODE(FILE_ID)
         BINARY_FILE_NAME = FUEL_FILE_BINARY_NAME(FILE_ID)
         FILE_NAME = get_b_filename(base_file_name, file_code)
         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
         IF(FILE_EXISTS) THEN
            IF(LAHEY_LF95()) THEN
               SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
               CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
            ELSE
               CALL MG_LOCATE_WRITE(16,30,BASE_FILE_NAME//'  ', &
                                                         ALL_VERSIONS,0)
            ENDIF
            BASE_FUEL_FILE_ACTIVE(FILE_ID) = .TRUE.
            BASE_FUEL_FILE_ACTIVE(11) = .TRUE.
            OPEN(10,FILE=FILE_NAME)
            OPEN(11,FILE=trim(DATA_DRIVE)// &
                           "BC"//BINARY_FILE_NAME//".BIN", &
                            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
            IREC = 1
            READ(10,*) DELETE
            ALLOCATE(FUEL_PRICE(0:AVAIL_DATA_YEARS))
            ALLOCATE(WRITE_FUEL_PRICE(0:AVAIL_DATA_YEARS))
            DO
               PERCENT_OR_ESCALATION = 'P'
               DO
                  READ(10,1000,IOSTAT=IOS) RECLN
                  IF(IOS /= 0) EXIT
                  IF(RECLN(1:1) == '7') EXIT
                  RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,'
                  RECORD_ACTIVE = 'T'
                  DO I = 0, AVAIL_DATA_YEARS
                     FUEL_PRICE(I) = -999999.
                  ENDDO
                  READ(RECLN,*,ERR=300) DELETE,DESC,FUEL_ID,FUEL_GROUP, &
                                     START_MO,END_MO, &
                                     SHIPPING_UNITS,BTU_CONTENT, &
                                     DATA_TYPE,CHANGE_YEAR, &
                                     PERCENT_OR_ESCALATION,DUMMY,DUMMY, &
                                     COMMENT, &
                                     FUEL_PRICE, &
                                     RECORD_ACTIVE
                  IF(FUEL_PRICE(0) == -999999.) FUEL_PRICE(0) = 0.
                  WRITE_FUEL_PRICE(0) = FUEL_PRICE(0)
                  DO I = 1, AVAIL_DATA_YEARS
                     IF(FUEL_PRICE(I) == -999999.) THEN
                        WRITE_FUEL_PRICE(I) = WRITE_FUEL_PRICE(I-1)
                     ELSE
                        WRITE_FUEL_PRICE(I) = FUEL_PRICE(I)
                     ENDIF
                  ENDDO
                  WRITE(11,REC=IREC)  DELETE,DESC,FUEL_ID,FUEL_GROUP, &
                                START_MO,END_MO, &
                                SHIPPING_UNITS,BTU_CONTENT, &
                                DATA_TYPE,CHANGE_YEAR, &
                                PERCENT_OR_ESCALATION, &
                                WRITE_FUEL_PRICE, &
                                RECORD_ACTIVE
                  IREC = IREC + 1
               ENDDO
               IF(IOS /= 0) EXIT
            ENDDO
            DEALLOCATE(FUEL_PRICE,WRITE_FUEL_PRICE)
            CLOSE(10)
            CLOSE(11)

         ENDIF
      ENDDO ! FILE_ID
      RETURN
!
!  OVERLAY THE FUEL-PRICE FILE
! ***********************************************************************
      ENTRY FC_MAKEOVL(OVERLAY_FAMILY_NAME,FILE_NUMBER)
!      ENTRY FC_MAKEOVL(OVERLAY_FAMILY_NAME,R_DUMMY_1,R_DUMMY_2)
! ***********************************************************************
      IF(.NOT. BASE_FUEL_FILE_ACTIVE(FILE_NUMBER)) RETURN
      FILE_CODE = FUEL_FILE_CODE(FILE_NUMBER)
      BINARY_FILE_NAME = FUEL_FILE_BINARY_NAME(FILE_NUMBER)
      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      DATA_DRIVE = OUTPUT_DIRECTORY()
      FILE_NAME = get_overlay_filename(data_drive, file_code, &
       overlay_family_name)
      OPEN(10,FILE=FILE_NAME)
      READ(10,*) DELETE
      INUNIT = 12
      DUMMY = 0.
      IF(FUEL_PRICE_OL(FILE_NUMBER) == 'BC') THEN
         OPEN(11,FILE=trim(DATA_DRIVE)//"BC"// &
                            BINARY_FILE_NAME//".BIN", &
                            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         INUNIT = 11
      ENDIF
      OPEN(12,FILE=trim(DATA_DRIVE)//"OL"// &
                            BINARY_FILE_NAME//".BIN", &
                            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
      ALLOCATE(FUEL_PRICE(0:AVAIL_DATA_YEARS))
      IREC = 0
      DO
         DO
            READ(10,'(A)',IOSTAT=IOS) RECLN
            IF(RECLN(1:1) == '7') EXIT
            IREC = IREC + 1
            READ(INUNIT,REC=IREC,IOSTAT=IOS_BASE)  DELETE,DESC,FUEL_ID, &
                                FUEL_GROUP,START_MO,END_MO, &
                                SHIPPING_UNITS,BTU_CONTENT, &
                                DATA_TYPE,CHANGE_YEAR, &
                                PERCENT_OR_ESCALATION, &
                                FUEL_PRICE, &
                                RECORD_ACTIVE
            IF(IOS_BASE /= 0) EXIT
!          READ(10,1010,IOSTAT=IOS) RECLN
            IF(IOS == 0) THEN
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,IOSTAT=IOS)  DELETE,DESC,FUEL_ID, &
                                 FUEL_GROUP,START_MO,END_MO, &
                                 SHIPPING_UNITS,BTU_CONTENT, &
                                 DATA_TYPE,CHANGE_YEAR, &
                                 PERCENT_OR_ESCALATION,DUMMY,DUMMY, &
                                 COMMENT, &
                                 FUEL_PRICE, &
                                 RECORD_ACTIVE
               IF(IOS /= 0) GOTO 300
            ENDIF
            WRITE(12,REC=IREC)  DELETE,DESC,FUEL_ID,FUEL_GROUP, &
                                START_MO,END_MO, &
                                SHIPPING_UNITS,BTU_CONTENT, &
                                DATA_TYPE,CHANGE_YEAR, &
                                PERCENT_OR_ESCALATION, &
                                FUEL_PRICE, &
                                RECORD_ACTIVE
         ENDDO
         IF(IOS_BASE /= 0) EXIT
      ENDDO
      DEALLOCATE(FUEL_PRICE)
      CLOSE(10)  ! OVERLAY DATA
      CLOSE(12)  ! OL FILE
      IF(FUEL_PRICE_OL(FILE_NUMBER) == 'BC') CLOSE(11)  ! BASE FILE IF OPEN
      FUEL_PRICE_OL(FILE_NUMBER) = 'OL'
      RETURN
!
!   300 CALL LOCATE(20,0)
!       WRITE(6,1010) trim(RECLN)
!       WRITE(6,1010) 'Error reading the above record.  Look for',
!      +              ' a "," in a character name.'
  300 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      CALL MG_LOCATE_WRITE(21,0, &
                           'Error reading Fuel Cost record. Look for'// &
                                          ' a "," in a character name.', &
                           ALL_VERSIONS,1)
      er_message='stop requested from FC_OBJT SIID113'
      call end_program(er_message)
!
      ENTRY RESET_FUEL_PRICE_OL
         DO FILE_ID = 0, MAX_FUEL_FILES-1
            FUEL_PRICE_OL(FILE_ID) = 'BC'
            LAST_FUEL_PRICE_OL_TYPE(FILE_ID) = 'OL'
         ENDDO
      RETURN
!
      ENTRY OPEN_FUEL_PRICE_FILE(R_UNIT_NUM,LOAD_FUEL_PRICES, &
                                                            FILE_NUMBER)
         LOAD_FUEL_PRICES = BASE_FUEL_FILE_ACTIVE(FILE_NUMBER)
         IF(.NOT. LOAD_FUEL_PRICES) RETURN
         UNIT_NUM = R_UNIT_NUM
!          IF(FUEL_PRICE_OL(FILE_NUMBER) == 'OL' .OR.
!      +               LAST_FUEL_PRICE_OL_TYPE(FILE_NUMBER) == 'OL')  THEN
            OPEN(UNIT_NUM,FILE=trim(OUTPUT_DIRECTORY())// &
                 FUEL_PRICE_OL(FILE_NUMBER)// &
                             FUEL_FILE_BINARY_NAME(FILE_NUMBER)//".BIN", &
                                             ACCESS="DIRECT",RECL=LRECL)
!             LOAD_FUEL_PRICES = .TRUE.
!          ELSE
!             LOAD_FUEL_PRICES = .FALSE.
!          ENDIF
!          LAST_FUEL_PRICE_OL_TYPE(FILE_NUMBER) =
!      +                                        FUEL_PRICE_OL(FILE_NUMBER)
      RETURN
!
      ENTRY CLOSE_FUEL_PRICE_FILE
         CLOSE(UNIT_NUM)
      RETURN
      ENTRY FUEL_PRICE_FILES_EXIST(LOAD_FUEL_PRICES,R_MAX_FUEL_FILES)
         R_MAX_FUEL_FILES = MAX_FUEL_FILES
         LOAD_FUEL_PRICES = BASE_FUEL_FILE_ACTIVE(11)
      RETURN
      ENTRY FUEL_PRICE_FILES_ACTIVE(LOAD_FUEL_PRICES)
         LOAD_FUEL_PRICES = BASE_FUEL_FILE_ACTIVE(11)
      RETURN
!
 1000 FORMAT(A)
 1010 FORMAT('&',A)

      END

