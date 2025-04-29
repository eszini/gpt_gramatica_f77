
!     msgmmfv.for
!     Created: 11/11/02 10:23:26 AM
!     Author : msg
!     Last change: MSG 4/2/2010 8:28:38 AM


      RECURSIVE SUBROUTINE FV_OBJECT
      use end_routine, only: end_program, er_message
!
      use SpinDriftLib
      USE PROD_ARRAYS_DIMENSIONS
      use sizecom
      use error_central
      use iostatmsg

      CHARACTER (LEN=1) :: R_DATA_TYPE,IOS_MESSAGE*64
      INTEGER (KIND=2) :: R_VECTOR,R_VECTOR_IN
      REAL (KIND=4) :: R_DATA(AVAIL_DATA_YEARS)
      REAL (KIND=4) ::  R_ANNUAL_DATA(AVAIL_DATA_YEARS)
      INTEGER (KIND=2) :: I,IREC,DELETE,LRECL=1024
      INTEGER (KIND=4) :: IOS
      INTEGER :: LAST_UNIT_NUMBER_OPENED=82,UNIT_NUM
      CHARACTER(LEN=1) :: DATA_TYPE,DUMMY
      CHARACTER (LEN=5) :: BASE_FILE_NAME,OVERLAY_FAMILY_NAME
      CHARACTER (LEN=5) :: ASSET_VECTOR_FILE
      CHARACTER (LEN=128) :: FILE_NAME
      CHARACTER (LEN=64) :: DESCRIPTION
      CHARACTER (LEN=256) :: BASE_FILE_DIRECTORY,OUTPUT_DIRECTORY
      CHARACTER (LEN=256) :: DATA_DRIVE
      CHARACTER (LEN=4) :: COMMENT
      REAL (KIND=4) :: VALUES_30(AVAIL_DATA_YEARS)
      LOGICAL (KIND=4) :: FILE_EXISTS
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER (LEN=2048) :: RECLN
! DECLARATION FOR LOCALS
      CHARACTER (LEN=13) :: FILE_TYPE='Asset Vectors'
      CHARACTER (LEN=2) :: INTRSTOL='BC',R_ASSET_VEC_OL
      INTEGER (KIND=2) :: VECTOR_NO,MAX_BASE_VECTOR_NUM=0, &
        MAX_OVERLAY_VECTOR_NUM=0,TEMP_VECTOR_NO
      INTEGER (KIND=2) :: TEMP_POINTER(:),BASE_VECTOR_POINTER(:), &
        OVLY_VECTOR_POINTER(:)
      ALLOCATABLE :: TEMP_POINTER,BASE_VECTOR_POINTER,OVLY_VECTOR_POINTER
      SAVE BASE_VECTOR_POINTER,OVLY_VECTOR_POINTER
      CHARACTER (LEN=20) :: VECTOR_TYPE,R_VECTOR_TYPE
      LOGICAL (KIND=1) :: GET_VECTOR_TYPE=.TRUE.,DUPLICATE_VECTOR_FOUND, &
        R_MONTHLY_VALUES_EXIST,MONTHLY_VALUES_EXIST
      SAVE MONTHLY_VALUES_EXIST
!
! MONTHLY MIDAS GOLD ADDITIONS 2/13/98
!
      REAL (KIND=4) :: MIDAS_MONTHLY_VALUES(60)
      CHARACTER (LEN=1) :: MIDAS_MONTHLY_DATA_UNITS(5)
      CHARACTER (LEN=4) :: MIDAS_LAST_MONTH(5)
      REAL (KIND=4) :: R_MIDAS_MONTHLY_VALUES(60),BASE_YEAR_VALUE, &
        BASE_YEAR_VECTOR_VALUE
      SAVE BASE_YEAR_VECTOR_VALUE
      CHARACTER (LEN=1) :: R_MIDAS_MONTHLY_DATA_UNITS(5)
      INTEGER (KIND=2) :: R_MIDAS_LAST_MONTH(5)
      LOGICAL (KIND=1) :: CPL_ACTIVE,CPL_IS_ACTIVE
      SAVE CPL_IS_ACTIVE
      REAL (KIND=4) :: R_BASE_YEAR_VECTOR_VALUE
      CHARACTER (KIND=1) VECTOR_ACTIVE
      LOGICAL :: ARRAY_AVAILABLE
      INTEGER (KIND=2) :: MAX_VECTOR_NUM,INUNIT
      INTEGER :: IOS_BASE
      LOGICAL (KIND=1) :: BANGOR
      LOGICAL (KIND=1) :: LAHEY_LF95
      CHARACTER (LEN=30) :: SCREEN_OUTPUT
!

!
!             CONVERSION ROUTINE FOR MIDAS 2.X ASSET PARAMETERS AND
!             INTEREST VECTOR FILES TO MIDAS GOLD ASSET RATE AND
!             ASSET VECTOR FILES.
!                       COPYRIGHT (C) 1991
!                 M.S. GERBER & ASSOCIATES, INC.
!                       ALL RIGHTS RESERVED
!

!
! CONVERT THE ASSET-VECTOR FILE
      ENTRY FV_MAKEBIN

      BASE_FILE_NAME = ASSET_VECTOR_FILE()
      CPL_IS_ACTIVE = CPL_ACTIVE()
      DUPLICATE_VECTOR_FOUND = .FALSE.
      FILE_NAME = trim(BASE_FILE_DIRECTORY())// &
        "FVB"//trim(BASE_FILE_NAME)//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_LOCATE_WRITE(16,30,BASE_FILE_NAME,ALL_VERSIONS,0)
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         ENDIF
         DATA_DRIVE = OUTPUT_DIRECTORY()
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCASTVEC.BIN",ACCESS="DIRECT", &
            STATUS="REPLACE",RECL=LRECL)
         ALLOCATE(TEMP_POINTER(0:10000))
         TEMP_POINTER = 0
         MIDAS_MONTHLY_DATA_UNITS = 'A'
         MIDAS_LAST_MONTH = ' ' 
         IREC = 0
         VECTOR_TYPE = ' '
         READ(10,*) DELETE
         DO
            BASE_YEAR_VALUE = -999999.
            VECTOR_ACTIVE = 'Y'
            DO
               READ(10,'(A)',IOSTAT=IOS) RECLN
               IF(IOS /= 0) EXIT
               IF(RECLN(1:1) == '7') EXIT
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
                ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,' &
                //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
                ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,' &
                //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
                    ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,' &
                //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
                    ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,' &
                //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               VALUES_30 = -999999.
               MIDAS_MONTHLY_VALUES = -999999.
               IF(BANGOR() .and. .false.) THEN
                  READ(RECLN,*,ERR=200)DELETE,DESCRIPTION,VECTOR_NO, &
                  DATA_TYPE,VALUES_30,COMMENT,VECTOR_TYPE,MIDAS_MONTHLY_VALUES, &
                  MIDAS_MONTHLY_DATA_UNITS,MIDAS_LAST_MONTH,BASE_YEAR_VALUE, &
                  VECTOR_ACTIVE
               ELSE
                  READ(RECLN,*,ERR=200)DELETE,DESCRIPTION,VECTOR_NO, &
                  DATA_TYPE,VALUES_30,COMMENT,VECTOR_TYPE,BASE_YEAR_VALUE, &
                  VECTOR_ACTIVE,DUMMY,DUMMY,DUMMY,MIDAS_MONTHLY_VALUES, &
                  MIDAS_MONTHLY_DATA_UNITS,MIDAS_LAST_MONTH
               ENDIF
               VECTOR_NO = ABS(VECTOR_NO)
               MAX_BASE_VECTOR_NUM = MAX(MAX_BASE_VECTOR_NUM,VECTOR_NO)
               IREC = IREC + 1
               IF(.NOT. (DELETE >= 8 .OR. VECTOR_ACTIVE == 'N')) THEN
                  IF(TEMP_POINTER(VECTOR_NO) /= 0) THEN
                     IF(.NOT. DUPLICATE_VECTOR_FOUND) THEN
                        WRITE(4,*) 'In base file ',TRIM(BASE_FILE_NAME), &
                            ' the following duplicate asset ', &
                              'vector numbers were found:'
                     ENDIF
                     WRITE(4,*) 'Duplicate Active Asset Vector found ', &
                     'Vector # ',VECTOR_NO,' Description ',DESCRIPTION, &
                                ' File position ',IREC
                     DUPLICATE_VECTOR_FOUND = .TRUE.
                  ELSE   
                     TEMP_POINTER(VECTOR_NO) = IREC
                  ENDIF
               ENDIF
               IF(VALUES_30(1) == -999999.) VALUES_30(1) = 0.
               DO I = 2, AVAIL_DATA_YEARS
                  IF(VALUES_30(I)==-999999.) VALUES_30(I)=VALUES_30(I-1)
               ENDDO
               WRITE(11,REC=IREC) DELETE,VECTOR_NO,DATA_TYPE,VECTOR_TYPE, &
               VECTOR_ACTIVE,BASE_YEAR_VALUE,VALUES_30, &
               MIDAS_MONTHLY_VALUES,MIDAS_MONTHLY_DATA_UNITS,MIDAS_LAST_MONTH
               VECTOR_NO = VECTOR_NO + 1
            ENDDO
            IF(IOS /= 0) EXIT
         ENDDO
         CLOSE(10)
         CLOSE(11)
         IF(MAX_BASE_VECTOR_NUM > 0) THEN
            ALLOCATE(BASE_VECTOR_POINTER(0:MAX_BASE_VECTOR_NUM))
            DO I = 0, MAX_BASE_VECTOR_NUM
               BASE_VECTOR_POINTER(I) = TEMP_POINTER(I)
            ENDDO
         ENDIF
         DEALLOCATE(TEMP_POINTER)
      ELSE IF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "msgmmfv:1113")
      ENDIF
      IF(DUPLICATE_VECTOR_FOUND) THEN
         WRITE(4,*)"DUPLICATE FINANCIAL VECTOR(S) FOUND."// &
            " CHECK WARNING MESSAGES"
         er_message='See WARNING MESSAGES -msgmmfv.for-1'
         call end_program(er_message)
      ENDIF
      RETURN
!
! OVERLAY THE ASSET-VECTOR FILE
!

      ENTRY FV_MAKEOVL(OVERLAY_FAMILY_NAME)

! 
      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      DATA_DRIVE = OUTPUT_DIRECTORY()
      DUPLICATE_VECTOR_FOUND = .FALSE.
      INUNIT= 12
      IF(INTRSTOL == 'BC') THEN
      ! TODO: Construct complete filename in one line of code, 
      ! assign to a variable and pass the path variable to the
      ! open statement.
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCASTVEC.BIN", &
                           ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         INUNIT= 11   
      ! TODO: Construct complete filename in one line of code, 
      ! assign to a variable and pass the path variable to the
      ! open statement.

         OPEN(12,FILE=trim(DATA_DRIVE)//"OLASTVEC.BIN", &
                           ACCESS="DIRECT",STATUS="REPLACE",RECL=LRECL)
      ELSE
      ! TODO: Construct complete filename in one line of code, 
      ! assign to a variable and pass the path variable to the
      ! open statement.
         OPEN(12,FILE=trim(DATA_DRIVE)//"OLASTVEC.BIN", &
                           ACCESS="DIRECT",STATUS="OLD",RECL=LRECL)
      ENDIF

      ! TODO: Construct complete filename in one line of code, 
      ! assign to a variable and pass the path variable to the
      ! open statement.
      OPEN(10,FILE=trim(DATA_DRIVE)//"FVO"// &
                                   trim(OVERLAY_FAMILY_NAME)//".DAT")
      ALLOCATE(TEMP_POINTER(0:10000))
      TEMP_POINTER = 0
      READ(10,*) DELETE
      MAX_OVERLAY_VECTOR_NUM = 0
      IREC = 0
      DO
         DO
            READ(10,"(A)",IOSTAT=IOS) RECLN
            IF(IOS /=0) EXIT
            IF(RECLN(1:1) == '7') EXIT
            IREC = IREC + 1
            READ(INUNIT,REC=IREC,IOSTAT=IOS_BASE) DELETE,TEMP_VECTOR_NO, &
                DATA_TYPE,VECTOR_TYPE,VECTOR_ACTIVE,BASE_YEAR_VALUE, &
                 VALUES_30,MIDAS_MONTHLY_VALUES,MIDAS_MONTHLY_DATA_UNITS, &
                 MIDAS_LAST_MONTH
            IF(IOS_BASE /= 0) then
                EXIT
            end if
            IF(IOS == 0) THEN
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
               ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,' &
                 //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
                 ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,' &
                 //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
                 ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,' &
                 //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
                 ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,ERR=200)DELETE,DESCRIPTION,VECTOR_NO, &
               DATA_TYPE,VALUES_30,COMMENT,VECTOR_TYPE,BASE_YEAR_VALUE, &
                  VECTOR_ACTIVE,DUMMY,DUMMY,DUMMY, &
                   MIDAS_MONTHLY_VALUES,MIDAS_MONTHLY_DATA_UNITS,MIDAS_LAST_MONTH
            ENDIF
            VECTOR_NO = ABS(VECTOR_NO)
            MAX_OVERLAY_VECTOR_NUM=MAX(MAX_OVERLAY_VECTOR_NUM,VECTOR_NO)
            WRITE(12,REC=IREC) DELETE,VECTOR_NO,DATA_TYPE,VECTOR_TYPE, &
            VECTOR_ACTIVE,BASE_YEAR_VALUE,VALUES_30, &
            MIDAS_MONTHLY_VALUES,MIDAS_MONTHLY_DATA_UNITS,MIDAS_LAST_MONTH
!
            IF(VECTOR_NO > 0 .AND. VECTOR_NO <= 10000) THEN
               IF(.NOT. (DELETE >= 8 .OR. VECTOR_ACTIVE == 'N')) THEN
!
! CHECK FOR DUPLICATE VECTORS
!
                  IF(TEMP_POINTER(VECTOR_NO) /= 0) THEN
                     IF(.NOT. DUPLICATE_VECTOR_FOUND) THEN
                        WRITE(4,*) 'In overlay file ',trim(OVERLAY_FAMILY_NAME), &
                            ' the following duplicate asset ', &
                                   'vector numbers were found:'
                     ENDIF
                     WRITE(4,*) 'Duplicate Asset Vector found ', &
                        'Vector # ',VECTOR_NO,' Description ',DESCRIPTION, &
                                ' File position ',IREC
                     DUPLICATE_VECTOR_FOUND = .TRUE.
                  ELSE   
                     TEMP_POINTER(VECTOR_NO) = IREC
                  ENDIF
               ENDIF
!
            ELSE
               WRITE(4,*) 'While overlaying the Asset Vector file ', &
               'the following problem was found:'
               WRITE(4,*) 'In overlay file ',OVERLAY_FAMILY_NAME, &
               ' in the vector with the description ',trim(DESCRIPTION)
               WRITE(4,*) 'the vector number',VECTOR_NO, &
               ' is not in the range 1 to 10000.'
               WRITE(4,*)
            ENDIF
         ENDDO
         IF(IOS /= 0) EXIT
      ENDDO
      CLOSE(10)
      CLOSE(12)
      IF(MAX_OVERLAY_VECTOR_NUM > 0) THEN
         IF(ALLOCATED(OVLY_VECTOR_POINTER)) then
            DEALLOCATE(OVLY_VECTOR_POINTER)
         end if
         ALLOCATE(OVLY_VECTOR_POINTER(0:MAX_OVERLAY_VECTOR_NUM))
         DO I = 0, MAX_OVERLAY_VECTOR_NUM
            OVLY_VECTOR_POINTER(I) = TEMP_POINTER(I)
         ENDDO
      ENDIF
      DEALLOCATE(TEMP_POINTER)
      INTRSTOL = 'OL'
      RETURN


      ENTRY RESET_INTRSTOL

         INTRSTOL = 'BC'
      RETURN


      ENTRY GET_ASSET_VEC_OL(R_ASSET_VEC_OL)

         R_ASSET_VEC_OL = INTRSTOL
      RETURN


      ENTRY OPEN_ASSET_VECTOR_FILE(UNIT_NUM)

         OPEN(UNIT_NUM,FILE=trim(OUTPUT_DIRECTORY())//INTRSTOL//"ASTVEC.BIN", &
            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         LAST_UNIT_NUMBER_OPENED = UNIT_NUM
      RETURN
!

      ENTRY CLOSE_ASSET_VECTOR_FILE

         CLOSE(LAST_UNIT_NUMBER_OPENED)
      RETURN
!

!
!             FUNCTION TO READ VARIABLE UNIT PARAMETERS FOR MIDAS     
!                   COPYRIGHT (C) 1986                             
!                        M.S. GERBER & ASSOCIATES, INC.
!                              ALL RIGHTS RESERVED
!

!
      ! TODO: Pick one of the following. I don't think both 
      ! can be cleanly migrated to standard Fortran.
      ENTRY GET_ASSET_VAR(R_VECTOR_IN,R_DATA_TYPE,R_DATA)
         GET_VECTOR_TYPE = .FALSE. ! TELLS WHICH ENTRY POINT IN USED
!
      ENTRY GET_ASSET_VAR_TYPE(R_VECTOR_IN,R_DATA_TYPE,R_DATA,R_VECTOR_TYPE)

!
      R_VECTOR = ABS(R_VECTOR_IN)
      IREC = 0
      IF(INTRSTOL == 'BC') THEN
         ARRAY_AVAILABLE = ALLOCATED(BASE_VECTOR_POINTER)
         IF(ARRAY_AVAILABLE) IREC = BASE_VECTOR_POINTER(R_VECTOR)
         MAX_VECTOR_NUM = MAX_BASE_VECTOR_NUM
      ELSE
         ARRAY_AVAILABLE = ALLOCATED(OVLY_VECTOR_POINTER)
         IF(ARRAY_AVAILABLE) IREC = OVLY_VECTOR_POINTER(R_VECTOR)
         MAX_VECTOR_NUM = MAX_OVERLAY_VECTOR_NUM
      ENDIF
      IF(ARRAY_AVAILABLE) THEN
         IF(R_VECTOR > MAX_VECTOR_NUM) THEN
            WRITE(SCREEN_MESSAGES,'(A,I4,A)')'Asset vector ',R_VECTOR, &
                ' was requested,'
            CALL MG_CLEAR_LINE_WRITE(20,0,79,trim(SCREEN_MESSAGES),ALL_VERSIONS,1)
            CALL MG_CLEAR_LINE_WRITE(21,0,79, &
                'but it is not in the Asset Vector File.',ALL_VERSIONS,1)
            call call_error('VECTOR ERROR')
            er_message='stop requested from msgmmfv SIID210'
            call end_program(er_message)
         ENDIF 
         IOS = 0
         INQUIRE(UNIT=LAST_UNIT_NUMBER_OPENED,OPENED=FILE_EXISTS,NAME=DATA_DRIVE)
         IF(.NOT. FILE_EXISTS) CALL OPEN_ASSET_VECTOR_FILE(82)        
         READ(LAST_UNIT_NUMBER_OPENED,REC=IREC,IOSTAT=IOS) DELETE,VECTOR_NO, &
            DATA_TYPE,VECTOR_TYPE,VECTOR_ACTIVE, &
                                               BASE_YEAR_VECTOR_VALUE,VALUES_30
         IF(IOS /= 0) THEN
            IF(IREC == 0) THEN
               WRITE(4,*) R_VECTOR,'-Asset Vector',R_VECTOR, &
                ' is NOT in the Asset Vector file.'
               R_DATA_TYPE = 'D'
               R_DATA = 0.
               IF(GET_VECTOR_TYPE) R_VECTOR_TYPE = 'Basis-Deferred Tax'
               GET_VECTOR_TYPE = .TRUE.
               RETURN
            ELSE
               CALL iostatmsg_unit(IOS,IOS_MESSAGE)
               WRITE(SCREEN_MESSAGES,'(A,I10)') &
                '   Error reading variable asset '// &
                    'vector file when requesting vector',R_VECTOR
           CALL MG_CLEAR_LINE_WRITE(20,2,79,trim(SCREEN_MESSAGES),ALL_VERSIONS,1)
               CALL MG_CLEAR_LINE_WRITE(21,2,79,trim(IOS_MESSAGE),ALL_VERSIONS,1)
               call call_error('trace back')
               er_message='stop requested from msgmmfv SIID211'
               call end_program(er_message)
            ENDIF
         ENDIF
         IF(R_VECTOR <= 20 .AND. CPL_IS_ACTIVE &
            .AND. INDEX(VECTOR_TYPE,'Dyn') /= 0) THEN
            CALL RETURN_CPL_ALLOCATION_VECTOR(R_VECTOR,VALUES_30)
         ENDIF
         IF(BASE_YEAR_VECTOR_VALUE == -999999.) THEN
            BASE_YEAR_VECTOR_VALUE = VALUES_30(1)
         ENDIF
         IF(VECTOR_NO /= R_VECTOR) THEN

            WRITE(SCREEN_MESSAGES,'(A,I10,A,I4,A)') &
                'Asset vector mismatch.  Vector ',R_VECTOR, &
                   'was requested.  Vector ',VECTOR_NO,' was found.'
            CALL MG_CLEAR_LINE_WRITE(20,1,79,trim(SCREEN_MESSAGES), &
                ALL_VERSIONS,1)
            er_message='stop requested from msgmmfv SIID212'
            call end_program(er_message)
         ENDIF
!
         R_DATA_TYPE = DATA_TYPE
         R_DATA = VALUES_30
!
         IF(GET_VECTOR_TYPE) R_VECTOR_TYPE = VECTOR_TYPE
         GET_VECTOR_TYPE = .TRUE.
      ELSE
         R_DATA = 0.

      ENDIF
      RETURN

      ENTRY GET_MONTHLY_ANNUAL_VALUES(R_VECTOR_IN,R_DATA_TYPE,R_VECTOR_TYPE, &
        R_DATA,R_MIDAS_MONTHLY_VALUES, &
           R_MIDAS_MONTHLY_DATA_UNITS,R_MIDAS_LAST_MONTH)
     

!
      R_VECTOR = ABS(R_VECTOR_IN)
      IREC = 0
      MONTHLY_VALUES_EXIST = .FALSE.
      IF(INTRSTOL == 'BC') THEN
         ARRAY_AVAILABLE = ALLOCATED(BASE_VECTOR_POINTER)
         IF(ARRAY_AVAILABLE) IREC = BASE_VECTOR_POINTER(R_VECTOR)
         MAX_VECTOR_NUM = MAX_BASE_VECTOR_NUM
      ELSE
         ARRAY_AVAILABLE = ALLOCATED(OVLY_VECTOR_POINTER)
         IF(ARRAY_AVAILABLE) IREC = OVLY_VECTOR_POINTER(R_VECTOR)
         MAX_VECTOR_NUM = MAX_OVERLAY_VECTOR_NUM
      ENDIF
      IF(ARRAY_AVAILABLE) THEN
         IF(R_VECTOR > MAX_VECTOR_NUM) THEN
            WRITE(SCREEN_MESSAGES,'(A,I4,A)') 'Asset vector ',R_VECTOR, &
                ' was requested,'
            CALL MG_CLEAR_LINE_WRITE(20,0,79,trim(SCREEN_MESSAGES),ALL_VERSIONS,1)
            CALL MG_CLEAR_LINE_WRITE(21,0,79, &
                'but it is not in the Asset Vector File.',ALL_VERSIONS,1)
            call call_error('VECTOR ERROR')
            er_message='stop requested from msgmmfv SIID213'
            call end_program(er_message)
         ENDIF 
         IOS = 0
         INQUIRE(UNIT=LAST_UNIT_NUMBER_OPENED,OPENED=FILE_EXISTS,NAME=DATA_DRIVE)
         IF(.NOT. FILE_EXISTS) CALL OPEN_ASSET_VECTOR_FILE(82)        
         READ(LAST_UNIT_NUMBER_OPENED,REC=IREC,IOSTAT=IOS) DELETE,VECTOR_NO, &
            DATA_TYPE,VECTOR_TYPE,VECTOR_ACTIVE, &
            BASE_YEAR_VECTOR_VALUE,VALUES_30,MIDAS_MONTHLY_VALUES, &
                MIDAS_MONTHLY_DATA_UNITS, &
                MIDAS_LAST_MONTH
         IF(IOS /= 0) THEN
            IF(IREC == 0) THEN
               WRITE(4,*) R_VECTOR,'-Asset Vector',R_VECTOR, &
                ' is NOT in the Asset Vector file.'
               R_DATA_TYPE = 'D'
               R_DATA = 0.
               R_VECTOR_TYPE = 'Basis-Deferred Tax'
               R_MIDAS_MONTHLY_DATA_UNITS = 'D'
               R_MIDAS_LAST_MONTH = 0
               R_MIDAS_MONTHLY_VALUES = 0.
               RETURN
            ELSE
               CALL iostatmsg_unit(IOS,IOS_MESSAGE)
               WRITE(SCREEN_MESSAGES,'(A,I4)') &
                '   Error reading variable asset '// &
                'vector file when requesting vector',R_VECTOR
               CALL MG_CLEAR_LINE_WRITE(20,2,79,trim(SCREEN_MESSAGES), &
                ALL_VERSIONS,1)
               CALL MG_CLEAR_LINE_WRITE(21,2,79,trim(IOS_MESSAGE),ALL_VERSIONS,1)
               call call_error('trace back')
               er_message='stop requested from msgmmfv SIID214'
               call end_program(er_message)
            ENDIF
         ENDIF
         IF(R_VECTOR <= 20 .AND. CPL_IS_ACTIVE .AND. &
            INDEX(VECTOR_TYPE,'Dyn') /= 0) THEN
            CALL RETURN_CPL_ALLOCATION_VECTOR(R_VECTOR,VALUES_30)
         ENDIF
         IF(BASE_YEAR_VECTOR_VALUE == -999999.) THEN
            BASE_YEAR_VECTOR_VALUE = VALUES_30(1)
         ENDIF
         IF(VECTOR_NO /= R_VECTOR) THEN
            WRITE(SCREEN_MESSAGES,'(A,I4,A,I4,A)') &
                'Asset vector mismatch.  Vector ',R_VECTOR, &
                       'was requested.  Vector ',VECTOR_NO,' was found.'
            CALL MG_CLEAR_LINE_WRITE(20,1,79,trim(SCREEN_MESSAGES),ALL_VERSIONS,1)
            er_message='stop requested from msgmmfv SIID215'
            call end_program(er_message)
         ENDIF
!
         R_DATA_TYPE = DATA_TYPE
         R_DATA = VALUES_30
!
         R_VECTOR_TYPE = VECTOR_TYPE
         CALL MAP_LAST_MONTH(MIDAS_LAST_MONTH,R_MIDAS_LAST_MONTH)
         R_MIDAS_MONTHLY_DATA_UNITS = MIDAS_MONTHLY_DATA_UNITS
         R_MIDAS_MONTHLY_VALUES = MIDAS_MONTHLY_VALUES
         MONTHLY_VALUES_EXIST = ANY(MIDAS_MONTHLY_VALUES /= -999999.) 
      ELSE
         WRITE(SCREEN_MESSAGES,'(A,I4,A)') &
            'Asset vector ',R_VECTOR,'was requested.'
         CALL MG_CLEAR_LINE_WRITE(20,0,79,trim(SCREEN_MESSAGES),ALL_VERSIONS,1)
         CALL MG_CLEAR_LINE_WRITE(21,0,79, &
            'Base Asset Vector File was named NONE.',ALL_VERSIONS,1)
         CALL call_error('Print Screen and e-mail to diane.crockett@hitachienergy.com')
         er_message='stop requested from msgmmfv SIID216'
         call end_program(er_message)
      ENDIF
      RETURN


      ENTRY IS_THERE_MONTHLY_INFORMATION(R_MONTHLY_VALUES_EXIST)


         R_MONTHLY_VALUES_EXIST = MONTHLY_VALUES_EXIST
      RETURN


      ENTRY RETURN_BASE_YEAR_VECTOR_VALUES(R_BASE_YEAR_VECTOR_VALUE)


         R_BASE_YEAR_VECTOR_VALUE = BASE_YEAR_VECTOR_VALUE
      RETURN

  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from msgmmfv SIID217'
      call end_program(er_message)

 1010 FORMAT('&',A)
 1020 FORMAT('&',4A,I4)
 1030 FORMAT('&',A,I4,A)
 1040 FORMAT('&',2(A,I3))
      END

      SUBROUTINE AR_OBJECT
      use end_routine, only: end_program, er_message
      use miscmod

!
      use SpinDriftLib
      USE PROD_ARRAYS_DIMENSIONS
      USE SIZECOM
      INTEGER (KIND=2) :: IREC,DELETE,YEAR,LRECL=100
      INTEGER (KIND=4) :: IOS
      INTEGER :: LAST_UNIT_NUMBER_OPENED=10,UNIT_NUM
      CHARACTER (LEN=5) :: BASE_FILE_NAME,OVERLAY_FAMILY_NAME
      CHARACTER (LEN=5) :: ASSET_RATE_FILE
      CHARACTER (LEN=256) :: FILE_NAME,FILE_NAME2
      CHARACTER (LEN=50) :: COMMENT
      CHARACTER (LEN=40) :: DESCRIPTION
      CHARACTER (LEN=256) :: BASE_FILE_DIRECTORY,OUTPUT_DIRECTORY
      CHARACTER (LEN=256) :: DATA_DRIVE
      REAL (KIND=4) :: VALUES_12(12),VALUES_6(6),VALUES(18)
      LOGICAL (KIND=4) :: FILE_EXISTS
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER (LEN=1024) :: RECLN
      LOGICAL (KIND=1) :: SET_VALUES_5_2_VALUES_12_1,SET_VALUES_6_2_VALUES_12_1
! DECLARATION FOR LOCALS
      CHARACTER (LEN=11) :: FILE_TYPE='Asset Rates'
      CHARACTER (LEN=2) :: PARMASOL='BC',R_PARMASOL
      EQUIVALENCE (VALUES(1),VALUES_12(1)),(VALUES(13),VALUES_6(1))
      LOGICAL (KIND=1) :: LAHEY_LF95
      CHARACTER (LEN=30) :: SCREEN_OUTPUT

!
!             CONVERSION ROUTINE FOR MIDAS 2.X ASSET PARAMETERS AND
!             INTEREST VECTOR FILES TO MIDAS GOLD ASSET RATE AND
!             ASSET VECTOR FILES.
!                       COPYRIGHT (C) 1991
!                 M.S. GERBER & ASSOCIATES, INC.
!                       ALL RIGHTS RESERVED
!

!
! CONVERT THE ASSET RATES FILE
      ENTRY AR_MAKEBIN
!
      BASE_FILE_NAME = ASSET_RATE_FILE()
      FILE_NAME = trim(BASE_FILE_DIRECTORY())//"ARB"//trim(BASE_FILE_NAME)//".DAT"
      DATA_DRIVE = OUTPUT_DIRECTORY()
      CALL ERASE(trim(DATA_DRIVE)//"BCASTPRM.BIN")
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
            CALL MG_LOCATE_WRITE(16,30,BASE_FILE_NAME,ALL_VERSIONS,0)
         ENDIF
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCASTPRM.BIN", &
            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         VALUES(1:16) = 0.
         VALUES_6(5) = -99999.
         VALUES_6(6) = -99999.
         IREC = 1
         SET_VALUES_5_2_VALUES_12_1 = .FALSE.
         SET_VALUES_6_2_VALUES_12_1 = .FALSE.
         READ(10,*) DELETE
         READ(10,"(A)",IOSTAT=IOS) RECLN
         RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
         READ(RECLN,*,IOSTAT=IOS) DELETE,YEAR,VALUES_12,COMMENT,VALUES_6
         IF(IOS /= 0) THEN

            CALL MG_CLEAR_LINE_WRITE(20,0,79,trim(RECLN),ALL_VERSIONS,1)
            er_message='stop requested from msgmmfv SIID218'
            call end_program(er_message)
         ENDIF
         IF(VALUES_6(5) == -99999.) SET_VALUES_5_2_VALUES_12_1 = .TRUE.
         IF(VALUES_6(6) == -99999.) SET_VALUES_6_2_VALUES_12_1 = .TRUE.
         DO
            IF(SET_VALUES_5_2_VALUES_12_1) VALUES_6(5) = VALUES_12(1)
            IF(SET_VALUES_6_2_VALUES_12_1) VALUES_6(6) = VALUES_12(1)
            WRITE(11,REC=IREC) DELETE,YEAR,VALUES
            IREC = IREC + 1
            READ(10,"(A)",IOSTAT=IOS) RECLN
            IF(IOS /= 0) EXIT
            RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
            READ(RECLN,*,IOSTAT=IOS) DELETE,YEAR,VALUES_12,COMMENT,VALUES_6
            IF(IOS /= 0) THEN

               CALL MG_CLEAR_LINE_WRITE(20,0,79,trim(RECLN),ALL_VERSIONS,1)
               er_message='stop requested from msgmmfv SIID219'
               call end_program(er_message)
            ENDIF
         ENDDO
         CLOSE(10)

         CLOSE(11)
      ELSE IF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "msgmmfv:1112")
      ENDIF
      RETURN
!
! OVERLAY THE ASSET RATES FILE
      ENTRY AR_MAKEOVL(OVERLAY_FAMILY_NAME)
!     NOTE: THE VARIABLE VALUE WAS ADDED 5/25/91 FOR INTEREST
!           CAPITALIZATION FOR THE MIDAS GOLD VERSION
      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      DATA_DRIVE = OUTPUT_DIRECTORY()
      IF(PARMASOL == 'BC') THEN
         FILE_NAME   = trim(DATA_DRIVE)//"BCASTPRM.BIN"
         FILE_NAME2 = trim(DATA_DRIVE)//"OLASTPRM.BIN"
         CALL COPY_FILE_2_FILE(FILE_NAME,FILE_NAME2)
      ENDIF
      OPEN(12,FILE=trim(DATA_DRIVE)//"OLASTPRM.BIN",ACCESS="DIRECT", &
        STATUS="UNKNOWN",RECL=LRECL)
      OPEN(10,FILE=trim(DATA_DRIVE)//"ARO"//trim(OVERLAY_FAMILY_NAME)//".DAT")
      READ(10,*) DELETE
      IREC = 0
      DO
         IREC = IREC + 1
         READ(12,REC=IREC,IOSTAT=IOS) DELETE,YEAR,VALUES
         IF(IOS /= 0) EXIT
         READ(10,"(A)",IOSTAT=IOS) RECLN
         IF(IOS /= 0) EXIT
         RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
         READ(RECLN,*,IOSTAT=IOS) DELETE,YEAR,VALUES_12,COMMENT,VALUES_6
         IF(IOS /= 0) THEN

            CALL MG_CLEAR_LINE_WRITE(20,0,79,trim(RECLN),ALL_VERSIONS,1)
            er_message='stop requested from msgmmfv SIID220'
            call end_program(er_message)
         ENDIF
         WRITE(12,REC=IREC)DELETE,YEAR,VALUES
      ENDDO
      CLOSE(10)
      CLOSE(12)
      PARMASOL = 'OL'
      RETURN
!
      ENTRY RESET_PARMASOL
         PARMASOL = 'BC'
      RETURN
!
      ENTRY GET_PARMAS_OL(R_PARMASOL)
         R_PARMASOL = PARMASOL
      RETURN
!
      ENTRY OPEN_ASSET_RATES_FILE(UNIT_NUM)
         OPEN(UNIT_NUM,FILE=trim(OUTPUT_DIRECTORY())//PARMASOL// &
            "ASTPRM.BIN",ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         LAST_UNIT_NUMBER_OPENED = UNIT_NUM
      RETURN
!
      ENTRY CLOSE_ASSET_RATES_FILE
         CLOSE(LAST_UNIT_NUMBER_OPENED)
      RETURN
!
 1010 FORMAT('&',A)
      END

      SUBROUTINE MR_OBJECT
      use end_routine, only: end_program, er_message

!
      use SpinDriftLib
      USE PROD_ARRAYS_DIMENSIONS
      USE SIZECOM
      use iostatmsg
!
      LOGICAL :: IS_OPEN
      CHARACTER (LEN=1) :: R_DATA_TYPE,IOS_MESSAGE*64
      INTEGER (KIND=2) :: R_VECTOR,R_VECTOR_IN,D_YR
      REAL (KIND=4) :: R_DATA(AVAIL_DATA_YEARS),ANNUAL_AVERAGE
      INTEGER (KIND=2) :: I,IREC,DELETE,LRECL=2048
      INTEGER (KIND=4) :: IOS
      INTEGER :: LAST_UNIT_NUMBER_OPENED=827,UNIT_NUM
      CHARACTER (LEN=1) :: DATA_TYPE,DUMMY
      CHARACTER (LEN=5) :: BASE_FILE_NAME,OVERLAY_FAMILY_NAME,CAPITAL_RATES_FILE
      CHARACTER (LEN=40) :: LHS_DISTRIBUTION
      CHARACTER (LEN=128) :: FILE_NAME
      CHARACTER (LEN=64) :: DESCRIPTION
      CHARACTER (LEN=256) :: BASE_FILE_DIRECTORY,OUTPUT_DIRECTORY
      CHARACTER (LEN=256) :: DATA_DRIVE,ERR_MESSAGE
      CHARACTER (LEN=4) :: COMMENT
      INTEGER (KIND=2) :: MONTH_ENDING,GET_MONTH_NUMBER
      REAL (KIND=4) :: VALUES_30(0:AVAIL_DATA_YEARS)
      LOGICAL (KIND=4) :: FILE_EXISTS
! DECLARATION FOR DBREAD COMMON BLOCK
      CHARACTER (LEN=2048) :: RECLN
! DECLARATION FOR LOCALS
      CHARACTER (LEN=13) :: FILE_TYPE='Capital Rates'
      CHARACTER (LEN=2) :: INTRSTOL='BC',R_ASSET_VEC_OL
      INTEGER (KIND=2) :: VECTOR_NO,MAX_BASE_VECTOR_NUM=0,MAX_OVERLAY_VECTOR_NUM=0
      INTEGER (KIND=2) :: TEMP_POINTER(:),BASE_VECTOR_POINTER(:)
      integer (kind=2) :: OVLY_VECTOR_POINTER(:)
      ALLOCATABLE :: TEMP_POINTER,BASE_VECTOR_POINTER,OVLY_VECTOR_POINTER
      SAVE BASE_VECTOR_POINTER,OVLY_VECTOR_POINTER
      CHARACTER (LEN=20) :: VECTOR_TYPE,R_VECTOR_TYPE
      LOGICAL (KIND=1) :: DUPLICATE_VECTOR_FOUND,CAPITAL_RATES_FILE_ACTIVE
      SAVE CAPITAL_RATES_FILE_ACTIVE
!
! MONTHLY MIDAS GOLD ADDITIONS 2/13/98
!
      REAL (KIND=4) :: MIDAS_MONTHLY_VALUES(120)
      real (kind=4) :: MONTHLY_VALUES_IN(12,AVAIL_DATA_YEARS)
      real (kind=4) :: MONTHLY_INPUT_BY(12,10)
      EQUIVALENCE (MIDAS_MONTHLY_VALUES(1),MONTHLY_INPUT_BY(1,1))
      CHARACTER (LEN=1) :: MIDAS_MONTHLY_DATA_UNITS(10)
      CHARACTER (LEN=4) :: MIDAS_LAST_MONTH(10)
      INTEGER (KIND=2) :: R_YEARS_TO_RETURN
      REAL (KIND=4) :: R_MIDAS_MONTHLY_VALUES(0:12,0:R_YEARS_TO_RETURN), &
        BASE_YEAR_VECTOR_VALUE
      SAVE BASE_YEAR_VECTOR_VALUE
      CHARACTER (LEN=1) :: R_MIDAS_MONTHLY_DATA_UNITS(10)
      INTEGER (KIND=2) :: R_MIDAS_LAST_MONTH(10)
      REAL (KIND=4) :: R_BASE_YEAR_VECTOR_VALUE
      CHARACTER (LEN=1) :: VECTOR_ACTIVE
      LOGICAL :: ARRAY_AVAILABLE
      INTEGER (KIND=2) :: MAX_VECTOR_NUM,INUNIT
      INTEGER :: IOS_BASE
      LOGICAL (KIND=1) :: LAHEY_LF95,LHS_ACTIVE
      CHARACTER (LEN=30) :: SCREEN_OUTPUT
      INTEGER (KIND=2) :: SCENARIO_INDEX,GET_SCENARIO_INDEX
      REAL (KIND=4) :: GET_SCENARIO_BY_INDEX
      INTEGER (KIND=2) :: YR,MO
      INTEGER (KIND=2) :: YRS_2_RETURN=1
      LOGICAL (KIND=1) :: R_VECTOR_FOUND
      REAL (KIND=4) :: R_MIDAS_MONTHLY_ANNUAL_VALUES(0:12)
      REAL (KIND=4) :: TEMP_MONTHLY_VALUES(12)

!
!     MONTHLY CAPITAL COST RATES FOR MIDAS GOLD
!                       COPYRIGHT (C) 2003
!                 M.S. GERBER & ASSOCIATES, INC.
!                       ALL RIGHTS RESERVED
!

!
! CONVERT THE ASSET-VECTOR FILE

      ENTRY MR_MAKEBIN

      BASE_FILE_NAME = CAPITAL_RATES_FILE()
      DUPLICATE_VECTOR_FOUND = .FALSE.
      FILE_NAME = trim(BASE_FILE_DIRECTORY())//"MRB"//trim(BASE_FILE_NAME)//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
      IF(FILE_EXISTS) THEN
         IF(LAHEY_LF95()) THEN
            SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//BASE_FILE_NAME
            CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
         ELSE
            CALL MG_LOCATE_WRITE(16,30,BASE_FILE_NAME,ALL_VERSIONS,0)
            CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         ENDIF
         DATA_DRIVE = OUTPUT_DIRECTORY()
         OPEN(10,FILE=FILE_NAME)
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCCAPRTE.BIN",ACCESS="DIRECT", &
            STATUS="REPLACE",RECL=LRECL)
         ALLOCATE(TEMP_POINTER(0:10000))
         TEMP_POINTER = 0
         MIDAS_MONTHLY_DATA_UNITS = 'A'
         IREC = 0
         VECTOR_TYPE = ' '
         READ(10,*) DELETE
         DO
            VECTOR_ACTIVE = 'Y'
            DO
               READ(10,'(A)',IOSTAT=IOS) RECLN
               IF(IOS /= 0) EXIT
               IF(RECLN(1:1) == '7') EXIT
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
                ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,' &
                  //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
                  ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,' &
                  //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
                    ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,' &
                  //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
                  ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,' &
                  //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               VALUES_30 = -999999.
               MIDAS_MONTHLY_VALUES = -999999.
               READ(RECLN,*,ERR=200) DELETE,DESCRIPTION,VECTOR_NO, &
               VECTOR_ACTIVE,VECTOR_TYPE,DATA_TYPE,LHS_DISTRIBUTION, &
                (DUMMY,I=1,6),COMMENT,VALUES_30,MIDAS_MONTHLY_VALUES, &
                MIDAS_MONTHLY_DATA_UNITS,MIDAS_LAST_MONTH
               VECTOR_NO = ABS(VECTOR_NO)
               MAX_BASE_VECTOR_NUM = MAX(MAX_BASE_VECTOR_NUM,VECTOR_NO)
               IREC = IREC + 1
               IF(.NOT. (DELETE >= 8 .OR. VECTOR_ACTIVE == 'N')) THEN
                  IF(TEMP_POINTER(VECTOR_NO) /= 0) THEN
                     IF(.NOT. DUPLICATE_VECTOR_FOUND) THEN
                        WRITE(4,*) 'In base file ',trim(BASE_FILE_NAME), &
                            ' the following duplicate asset ', &
                                   'vector numbers were found:'
                     ENDIF
                     WRITE(4,*) 'Duplicate Active Capital Rate Vector ', &
                     'found Vector # ',VECTOR_NO,' Description ',DESCRIPTION, &
                               ' File position ',IREC
                     DUPLICATE_VECTOR_FOUND = .TRUE.
                  ELSE   
                     TEMP_POINTER(VECTOR_NO) = IREC
                  ENDIF
               ENDIF
               IF(VALUES_30(0) == -999999.) VALUES_30(0) = 0.
               DO I = 1, AVAIL_DATA_YEARS
                  IF(VALUES_30(I)==-999999.) VALUES_30(I)=VALUES_30(I-1)
               ENDDO
               WRITE(11,REC=IREC) DELETE,VECTOR_NO,DATA_TYPE,VECTOR_TYPE, &
                VECTOR_ACTIVE,VALUES_30,MIDAS_MONTHLY_VALUES, &
                    MIDAS_MONTHLY_DATA_UNITS,MIDAS_LAST_MONTH, &
                    LHS_DISTRIBUTION,DESCRIPTION
               VECTOR_NO = VECTOR_NO + 1
            ENDDO
            IF(IOS /= 0) EXIT
         ENDDO
         CLOSE(10)
         CLOSE(11)
         IF(MAX_BASE_VECTOR_NUM > 0) THEN
            ALLOCATE(BASE_VECTOR_POINTER(0:MAX_BASE_VECTOR_NUM))
            DO I = 0, MAX_BASE_VECTOR_NUM
               BASE_VECTOR_POINTER(I) = TEMP_POINTER(I)
            ENDDO
         ENDIF
         DEALLOCATE(TEMP_POINTER)
      ELSE IF(INDEX(BASE_FILE_NAME,'NONE') == 0) THEN
         CALL STOP_NOFILE(FILE_TYPE,FILE_NAME, "msgmmfv:1111")
      ENDIF
      IF(DUPLICATE_VECTOR_FOUND) THEN
         WRITE(4,*)'DUPLICATE VECTOR(S) FOUND. CHECK WARNING MESSAGES'
         WRITE(4,*) '*** line 963 MSGMMFV.FOR ***'
         er_message='See WARNING MESSAGES -msgmmfv.for-2'
         call end_program(er_message)
      ENDIF
      RETURN
!
! OVERLAY THE ASSET-VECTOR FILE
!

      ENTRY MR_MAKEOVL(OVERLAY_FAMILY_NAME)

! 
      IF(LAHEY_LF95()) THEN
         SCREEN_OUTPUT = trim(FILE_TYPE)//'-'//OVERLAY_FAMILY_NAME
         CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,ALL_VERSIONS,0)
      ELSE
         CALL MG_CLEAR_LINE_WRITE(17,9,36,FILE_TYPE,ALL_VERSIONS,0)
         CALL LOCATE(10,51)
      ENDIF
      DATA_DRIVE = OUTPUT_DIRECTORY()
      DUPLICATE_VECTOR_FOUND = .FALSE.
      INUNIT= 12
      IF(INTRSTOL == 'BC') THEN
         OPEN(11,FILE=trim(DATA_DRIVE)//"BCCAPRTE.BIN",ACCESS="DIRECT", &
            STATUS="UNKNOWN",RECL=LRECL)
         INUNIT= 11   
         OPEN(12,FILE=trim(DATA_DRIVE)//"OLCAPRTE.BIN",ACCESS="DIRECT", &
            STATUS="REPLACE",RECL=LRECL)
      ELSE
         OPEN(12,FILE=trim(DATA_DRIVE)//"OLCAPRTE.BIN",ACCESS="DIRECT", &
            STATUS="OLD",RECL=LRECL)
      ENDIF
!
      OPEN(10,FILE=trim(DATA_DRIVE)//"MRO"//trim(OVERLAY_FAMILY_NAME)//".DAT")
      ALLOCATE(TEMP_POINTER(0:10000))
      TEMP_POINTER = 0
      READ(10,*) DELETE
      MAX_OVERLAY_VECTOR_NUM = 0
      IREC = 0
      DO
         DO
            READ(10,"(A)",IOSTAT=IOS) RECLN
            IF(IOS /=0) EXIT
            IF(RECLN(1:1) == '7') EXIT
            IREC = IREC + 1
            READ(INUNIT,REC=IREC,IOSTAT=IOS_BASE) DELETE,VECTOR_NO,DATA_TYPE, &
                VECTOR_TYPE,VECTOR_ACTIVE,VALUES_30, &
                MIDAS_MONTHLY_VALUES,MIDAS_MONTHLY_DATA_UNITS,MIDAS_LAST_MONTH, &
                LHS_DISTRIBUTION,DESCRIPTION
            IF(IOS_BASE /= 0) EXIT
            IF(IOS == 0) THEN
               RECLN = trim(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
               ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,' &
        //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
        ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,' &
        //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
        ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,' &
        //',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'// &
        ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,ERR=200) DELETE,DESCRIPTION,VECTOR_NO,VECTOR_ACTIVE, &
                   VECTOR_TYPE,DATA_TYPE,LHS_DISTRIBUTION, &
                   (DUMMY,I=1,6),COMMENT,MIDAS_MONTHLY_VALUES, &
                   MIDAS_MONTHLY_DATA_UNITS,MIDAS_LAST_MONTH
            ENDIF
            VECTOR_NO = ABS(VECTOR_NO)
            MAX_OVERLAY_VECTOR_NUM=MAX(MAX_OVERLAY_VECTOR_NUM,VECTOR_NO)
            WRITE(12,REC=IREC) DELETE,VECTOR_NO,DATA_TYPE,VECTOR_TYPE, &
            VECTOR_ACTIVE,VALUES_30,MIDAS_MONTHLY_VALUES, &
                               MIDAS_MONTHLY_DATA_UNITS,MIDAS_LAST_MONTH, &
                               LHS_DISTRIBUTION,DESCRIPTION
!
            IF(VECTOR_NO > 0 .AND. VECTOR_NO <= 10000) THEN
               IF(.NOT. (DELETE >= 8 .OR. VECTOR_ACTIVE == 'N')) THEN
!
! CHECK FOR DUPLICATE VECTORS
!
                  IF(TEMP_POINTER(VECTOR_NO) /= 0) THEN
                     IF(.NOT. DUPLICATE_VECTOR_FOUND) THEN
                        WRITE(4,*) 'In overlay file ',trim(OVERLAY_FAMILY_NAME), &
                        ' the following duplicate Capitial Rates ', &
                                   'vector numbers were found:'
                     ENDIF
                     WRITE(4,*) 'Duplicate Asset Vector found ','Vector # ', &
                     VECTOR_NO,' Description ',DESCRIPTION, &
                                ' File position ',IREC
                     DUPLICATE_VECTOR_FOUND = .TRUE.
                  ELSE   
                     TEMP_POINTER(VECTOR_NO) = IREC
                  ENDIF
               ENDIF
!
            ELSE
               WRITE(4,*) 'While overlaying the Asset Vector file ', &
               'the following problem was found:'
               WRITE(4,*) 'In overlay file ',OVERLAY_FAMILY_NAME, &
               ' in the vector with the description ',trim(DESCRIPTION)
               WRITE(4,*) 'the vector number',VECTOR_NO, &
               ' is not in the range 1 to 10000.'
               WRITE(4,*)
            ENDIF
         ENDDO
         IF(IOS /= 0) EXIT
      ENDDO
      CLOSE(10)
      CLOSE(12)
      IF(MAX_OVERLAY_VECTOR_NUM > 0) THEN
         IF(ALLOCATED(OVLY_VECTOR_POINTER)) DEALLOCATE(OVLY_VECTOR_POINTER)
         ALLOCATE(OVLY_VECTOR_POINTER(0:MAX_OVERLAY_VECTOR_NUM))
         DO I = 0, MAX_OVERLAY_VECTOR_NUM
            OVLY_VECTOR_POINTER(I) = TEMP_POINTER(I)
         ENDDO
      ENDIF
      DEALLOCATE(TEMP_POINTER)
      INTRSTOL = 'OL'
      RETURN


      ENTRY RESET_CAP_RATES_OL

         INTRSTOL = 'BC'
      RETURN


      ENTRY GET_CAP_RATES_VEC_OL(R_ASSET_VEC_OL)

         R_ASSET_VEC_OL = INTRSTOL
      RETURN


      ENTRY OPEN_CAPITIAL_RATES_VECTOR_FILE(UNIT_NUM)

         OPEN(UNIT_NUM,FILE=trim(OUTPUT_DIRECTORY())//INTRSTOL//"CAPRTE.BIN", &
            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)
         LAST_UNIT_NUMBER_OPENED = UNIT_NUM
      RETURN


      ENTRY CLOSE_CAPITAL_RATES_VECTOR_FILE

         CLOSE(31123,IOSTAT=IOS)
      RETURN

!
!            FUNCTION TO READ CAPITAL RATES AND ADJUST THEM
!                FOR LHS DISTRIBUTIONSF
!                         MIDAS GOLD     
!                     COPYRIGHT (C) 2003                             
!                M.S. GERBER & ASSOCIATES, INC.
!                     ALL RIGHTS RESERVED
!


      ENTRY PROCESS_CAPITAL_RATES_FILE

         CAPITAL_RATES_FILE_ACTIVE = .FALSE.       
         IF(.NOT. (ALLOCATED(BASE_VECTOR_POINTER) .OR. &
            ALLOCATED(OVLY_VECTOR_POINTER))) then 
                RETURN
            end if

         CAPITAL_RATES_FILE_ACTIVE = .TRUE.       
         OPEN(10,FILE=trim(OUTPUT_DIRECTORY())//INTRSTOL//"CAPRTE.BIN", &
            ACCESS="DIRECT",STATUS="UNKNOWN",RECL=LRECL)

         FILE_NAME = trim(OUTPUT_DIRECTORY())//"CAPRATES.BIN"
         INQUIRE(UNIT=31123,OPENED=IS_OPEN)
         OPEN(31123,FILE=FILE_NAME,ACCESS="DIRECT",STATUS="REPLACE",RECL=LRECL)
         IREC = 1
         DO
            READ(10,REC=IREC,IOSTAT=IOS) DELETE,VECTOR_NO,DATA_TYPE,VECTOR_TYPE, &
                VECTOR_ACTIVE,VALUES_30,MIDAS_MONTHLY_VALUES, &
                MIDAS_MONTHLY_DATA_UNITS,MIDAS_LAST_MONTH, &
                LHS_DISTRIBUTION,DESCRIPTION
            IF(IOS /= 0) EXIT

!
! PROCESS THE DATA 
!
            LHS_ACTIVE = .FALSE. 
            IF(INDEX(LHS_DISTRIBUTION,'Not Active') == 0) THEN
               SCENARIO_INDEX = GET_SCENARIO_INDEX(LHS_DISTRIBUTION)
               LHS_ACTIVE = SCENARIO_INDEX > 0 
            ENDIF
            DO YR = 1, 9 

                  IF(ANY(MONTHLY_INPUT_BY(1:12,YR)== -999999.,1)) THEN
                     IF(MONTHLY_INPUT_BY(1,YR) == -999999.) then
                        MONTHLY_INPUT_BY(1,YR) = VALUES_30(YR)
                    end if
                     DO MO = 2, 12
                        IF(MONTHLY_INPUT_BY(MO,YR) == -999999.) THEN
                           MONTHLY_INPUT_BY(MO,YR) = MONTHLY_INPUT_BY(MO-1,YR)
                        ENDIF
                     ENDDO
                  ENDIF
            ENDDO
            DO YR = 1, AVAIL_DATA_YEARS 
               D_YR = MIN(YR,int(10,2))
               IF(MIDAS_MONTHLY_DATA_UNITS(D_YR) == 'A') THEN
                  MONTHLY_VALUES_IN(:,YR) = VALUES_30(YR)
               ELSEIF(MIDAS_MONTHLY_DATA_UNITS(D_YR) == 'P') THEN
                  MONTHLY_VALUES_IN(:,YR) = MONTHLY_INPUT_BY(:,D_YR)
                  VALUES_30(YR) = SUM(MONTHLY_VALUES_IN(1:12,YR))/12. 
               ELSEIF(MIDAS_MONTHLY_DATA_UNITS(D_YR) == 'L') THEN
                  MONTHLY_VALUES_IN(:,YR) = MONTHLY_INPUT_BY(:,D_YR)
                  VALUES_30(YR) = SUM(MONTHLY_VALUES_IN(1:12,YR))/12. 

               ELSEIF(MIDAS_MONTHLY_DATA_UNITS(D_YR) == 'M') THEN 
                  MONTHLY_VALUES_IN(:,YR) = VALUES_30(YR) *  &
                    MONTHLY_INPUT_BY(:,D_YR)
                  VALUES_30(YR) = SUM(MONTHLY_VALUES_IN(1:12,YR))/12. 
               ELSEIF(MIDAS_MONTHLY_DATA_UNITS(D_YR) == 'T' .AND. YR > 1) THEN 
                  IF(ANNUAL_AVERAGE /= 0.) THEN
                     MONTHLY_VALUES_IN(:,YR) = VALUES_30(YR) * &
                        MONTHLY_VALUES_IN(:,YR-1)/ANNUAL_AVERAGE                
                  ELSE
                     MONTHLY_VALUES_IN(:,YR) = VALUES_30(YR) 
                  ENDIF
               ENDIF
               ANNUAL_AVERAGE = SUM(MONTHLY_VALUES_IN(1:12,YR))/12. 
            ENDDO

!
! MODIFY QUANTIES USING LATIN HYPERCUBE
!
            IF(LHS_ACTIVE) THEN
               DO YR = 1, AVAIL_DATA_YEARS
                  MONTH_ENDING = GET_MONTH_NUMBER(MIDAS_LAST_MONTH(YR))
                  DO MO = 1, 12
                     IF(YR < 10) THEN
                        IF(MIDAS_MONTHLY_DATA_UNITS(YR) == 'L' .AND. MO <= &
                                MONTH_ENDING) then
                            CYCLE
                        end if
                     ENDIF
                     MONTHLY_VALUES_IN(MO,YR) = MONTHLY_VALUES_IN(MO,YR) * &
                        GET_SCENARIO_BY_INDEX(YR,MO,SCENARIO_INDEX)
                  ENDDO 
               ENDDO
               VALUES_30(YR) = SUM(MONTHLY_VALUES_IN(1:12,YR))/12. 
            ENDIF
!
! WRITE RECORD
!
            WRITE(31123,REC=IREC,IOSTAT=IOS) DELETE,VECTOR_NO,DATA_TYPE, &
                VECTOR_TYPE,VECTOR_ACTIVE,VALUES_30,MONTHLY_VALUES_IN, &
                                             DESCRIPTION
            IREC = IREC + 1
         ENDDO
!     
         CLOSE(10,IOSTAT=IOS)
      RETURN
!

!
!             FUNCTION TO READ CAPITAL RATES FOR MIDAS GOLD     
!                     COPYRIGHT (C) 2003                             
!                M.S. GERBER & ASSOCIATES, INC.
!                     ALL RIGHTS RESERVED
!

!
!
      ENTRY GET_CAP_RATES_MONTHLY_VALUES(R_VECTOR_FOUND,R_VECTOR_IN, &
        R_MIDAS_MONTHLY_VALUES,R_YEARS_TO_RETURN)
      YRS_2_RETURN = MIN(R_YEARS_TO_RETURN,AVAIL_DATA_YEARS)
      R_VECTOR = ABS(R_VECTOR_IN)
      IREC = 0
      R_VECTOR_FOUND = .FALSE.
      IF(INTRSTOL == 'BC') THEN
         ARRAY_AVAILABLE = ALLOCATED(BASE_VECTOR_POINTER)
         IF(ARRAY_AVAILABLE) IREC = BASE_VECTOR_POINTER(R_VECTOR)
         MAX_VECTOR_NUM = MAX_BASE_VECTOR_NUM
      ELSE
         ARRAY_AVAILABLE = ALLOCATED(OVLY_VECTOR_POINTER)
         IF(ARRAY_AVAILABLE) IREC = OVLY_VECTOR_POINTER(R_VECTOR)
         MAX_VECTOR_NUM = MAX_OVERLAY_VECTOR_NUM
      ENDIF
      IF(ARRAY_AVAILABLE) THEN
         IF(R_VECTOR > MAX_VECTOR_NUM) THEN
            WRITE(4,'(A,I4,A)')' Capital Rates vector ',R_VECTOR, &
                ' was requested,'//'but it is not in the Capital Rates File.'
            RETURN 
         ENDIF 
         READ(31123,REC=IREC,IOSTAT=IOS) DELETE,VECTOR_NO,DATA_TYPE, &
         VECTOR_TYPE,VECTOR_ACTIVE,VALUES_30,MONTHLY_VALUES_IN, &
                                         DESCRIPTION
         IF(IOS /= 0) THEN
            IF(IREC == 0) THEN
               WRITE(4,*) R_VECTOR,'-Capital Rates Vector',R_VECTOR, &
                ' is NOT in the Capital Rates Vector file.'
               RETURN
            ELSE
               CALL iostatmsg_unit(IOS,IOS_MESSAGE)
               WRITE(SCREEN_MESSAGES,'(A,I4)')' Error reading Capital Rates '// &
                'file when requesting vector',R_VECTOR
               CALL MG_CLEAR_LINE_WRITE(20,2,79,trim(SCREEN_MESSAGES), &
               ALL_VERSIONS,1)
               CALL MG_CLEAR_LINE_WRITE(21,2,79,trim(IOS_MESSAGE),ALL_VERSIONS,1)
               er_message='stop requested from msgmmfv SIID223'
               call end_program(er_message)
            ENDIF
         ENDIF

         IF(VECTOR_NO /= R_VECTOR) THEN
            WRITE(4,'(A,I4,A,I4,A)')' Capital Rates vector mismatch.  Vector ', &
                R_VECTOR,'was requested.  Vector ',VECTOR_NO, &
                                    ' was found.'
            RETURN
         ENDIF
!
!  TRANSFER VALUES
!
         R_VECTOR_FOUND = .TRUE.
         R_MIDAS_MONTHLY_VALUES(12,0) = VALUES_30(0)
         DO YR = 1, YRS_2_RETURN
            R_MIDAS_MONTHLY_VALUES(0,YR) = VALUES_30(YR)
            R_MIDAS_MONTHLY_VALUES(1:12,YR) = MONTHLY_VALUES_IN(1:12,YR)
         ENDDO
      ELSE
         WRITE(4,'(A,I4,A)')' Capital Rates vector ',R_VECTOR,&
            ' was requested.'// &
            ' Capital Rates file was named NONE.'
         ERR_MESSAGE="Print Screen and e-mail to gturk@globalenergy.com"
         RETURN ! er_message='stop requested from msgmmfv SIID225'
         RETURN ! call end_program(er_message)
      ENDIF
      YRS_2_RETURN = 1
      RETURN

      ENTRY GET_CAP_RATES_4_CURRENT_YEAR(R_VECTOR_FOUND,R_VECTOR_IN, &
        R_MIDAS_MONTHLY_ANNUAL_VALUES,R_YEARS_TO_RETURN)

!
      YRS_2_RETURN = MIN(R_YEARS_TO_RETURN,AVAIL_DATA_YEARS)
      R_VECTOR = ABS(R_VECTOR_IN)
      IREC = 0
      R_VECTOR_FOUND = .FALSE.
      IF(INTRSTOL == 'BC') THEN
         ARRAY_AVAILABLE = ALLOCATED(BASE_VECTOR_POINTER)
         IF(ARRAY_AVAILABLE) IREC = BASE_VECTOR_POINTER(R_VECTOR)
         MAX_VECTOR_NUM = MAX_BASE_VECTOR_NUM
      ELSE
         ARRAY_AVAILABLE = ALLOCATED(OVLY_VECTOR_POINTER)
         IF(ARRAY_AVAILABLE) IREC = OVLY_VECTOR_POINTER(R_VECTOR)
         MAX_VECTOR_NUM = MAX_OVERLAY_VECTOR_NUM
      ENDIF
      IF(ARRAY_AVAILABLE) THEN
         IF(R_VECTOR > MAX_VECTOR_NUM) THEN
            WRITE(4,'(A,I4,A)')' Capital Rates vector ',R_VECTOR, &
                ' was requested,'//'but it is not in the Capital Rates File.'
            RETURN ! er_message='stop requested from msgmmfv SIID226'
            RETURN ! call end_program(er_message)
         ENDIF 
         READ(31123,REC=IREC,IOSTAT=IOS) DELETE,VECTOR_NO,DATA_TYPE, &
            VECTOR_TYPE,VECTOR_ACTIVE,VALUES_30,MONTHLY_VALUES_IN, &
                                         DESCRIPTION
         IF(IOS /= 0) THEN
            IF(IREC == 0) THEN
               WRITE(4,*) R_VECTOR,'-Capital Rates Vector',R_VECTOR, &
                ' is NOT in the Capital Rates Vector file.'
               RETURN
            ELSE
               CALL iostatmsg_unit(IOS,IOS_MESSAGE)
               WRITE(SCREEN_MESSAGES,'(A,I4)')' Error reading Capital Rates '// &
                'file when requesting vector',R_VECTOR
               CALL MG_CLEAR_LINE_WRITE(20,2,79,trim(SCREEN_MESSAGES), &
                ALL_VERSIONS,1)
               CALL MG_CLEAR_LINE_WRITE(21,2,79,trim(IOS_MESSAGE), &
                ALL_VERSIONS,1)
               er_message='stop requested from msgmmfv SIID227'
               call end_program(er_message)
            ENDIF
         ENDIF

         IF(VECTOR_NO /= R_VECTOR) THEN
            WRITE(4,'(A,I4,A,I4,A)')' Capital Rates vector mismatch.  Vector ', &
                R_VECTOR,'was requested.  Vector ',VECTOR_NO, & 
                                    ' was found.'
            RETURN ! er_message='stop requested from msgmmfv SIID228'
            RETURN ! call end_program(er_message)
         ENDIF
!
!  TRANSFER VALUES
!
         R_VECTOR_FOUND = .TRUE.
         R_MIDAS_MONTHLY_ANNUAL_VALUES(0) = VALUES_30(YRS_2_RETURN)
         R_MIDAS_MONTHLY_ANNUAL_VALUES(1:12) = &
            MONTHLY_VALUES_IN(1:12,YRS_2_RETURN)
      ELSE
         WRITE(4,'(A,I4,A)') ' Capital Rates vector ', &
            R_VECTOR,' was requested.'//' Capital Rates file was named NONE.'
         ERR_MESSAGE="Print Screen and e-mail to gturk@globalenergy.com"
         RETURN
      ENDIF
      YRS_2_RETURN = 1
      RETURN

  200 CALL MG_LOCATE_WRITE(20,0,trim(RECLN),ALL_VERSIONS,1)
      er_message='stop requested from msgmmfv SIID230'
      call end_program(er_message)

 1010 FORMAT('&',A)
 1020 FORMAT('&',4A,I4)
 1030 FORMAT('&',A,I4,A)
 1040 FORMAT('&',2(A,I3))
      END