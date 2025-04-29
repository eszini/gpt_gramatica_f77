!     ******************************************************************
!     COAL_VECTOR_OBJ.F90
!     Copyright(c)  2000
!
!     Created: 12/23/2010 1:42:47 PM
!     Author : MARK S GERBER
!     Last change: MSG 10/12/2011 4:36:42 PM
!     ******************************************************************

      MODULE COAL_ESCALATIONS_MAKEBIN_INPUTS
         CHARACTER (LEN=50) :: COMMENT
         REAL (KIND=4) :: VALUES_30(0:30)
         CHARACTER (LEN=40) :: DESCRIPTION
         INTEGER (KIND=2) :: START_MONTH,END_MONTH, &
                             GROWTH_RATE_YEAR
         INTEGER (KIND=4) :: VECTOR_NO
         CHARACTER (LEN=2) :: VECTOR_TYPE
         CHARACTER (LEN=2), PARAMETER :: DATA_FILE_CODE="CZ"
         CHARACTER (LEN=36), PARAMETER :: FILE_TYPE= &
                                  'Coal Model Demand & Escalation Rates'
! COAL_ESCALATIONS_READ_DATA
         INTEGER (KIND=2), PARAMETER :: COAL_ESCALATIONS_LRECL=512 ! OUT
         CHARACTER (LEN=40), PARAMETER :: &
               COAL_ESCA_BIN_FILE_NAME ="COAL_ESCALATIONS.BIN"
         LOGICAL (KIND=4), SAVE ::  COAL_ESCALATIONS_FILE_EXISTS=.FALSE.
         CHARACTER (LEN=2), SAVE :: &
                                PROCESS_COAL_ESCA_FILE_OL='BC'
         INTEGER (KIND=2), SAVE :: ESCALATIONS_VECTOR_BASEFILE=0, &
                                   ESCALATIONS_VECTOR_OVERLAY=0
         INTEGER (KIND=4), SAVE :: MAX_VECTOR_NO_BASEFILE=0, &
                                   MAX_VECTOR_NO_OVERLAY=0
      END MODULE COAL_ESCALATIONS_MAKEBIN_INPUTS
      MODULE COAL_ESCALATION_VECTORS_DATA
         INTEGER (KIND=4), ALLOCATABLE ,SAVE :: COAL_VECTOR_MAP(:)
         REAL (KIND=4), ALLOCATABLE, SAVE :: COAL_VECTOR_VALUES(:,:)
         LOGICAL (KIND=1), ALLOCATABLE, SAVE :: &
                                            COAL_VECTOR_TYPE_IS_VALUE(:)
         INTEGER (KIND=4) :: MaxVectorNum
      END MODULE
!***********************************************************************
!
!
      SUBROUTINE COAL_ESCALATIONS_MAKEBIN(BASE_FILE_NAME)  ! ENTERY NAME
!
!***********************************************************************
!
!
      USE COAL_ESCALATIONS_MAKEBIN_INPUTS
      use end_routine

!
! SIMULATION VARIABLES
!
         INTEGER (KIND=2) ::   UNIT_NUM=10,INUNIT,IREC,DELETE,I
         INTEGER (KIND=4) :: IOS
         CHARACTER (LEN=256) :: BASE_FILE_NAME,OVERLAY_FAMILY_NAME, &
                                FILE_NAME,OUTPUT_DIRECTORY, &
                                BASE_FILE_DIRECTORY,SCREEN_OUTPUT
!         LOGICAL (KIND=4) :: FILE_EXISTS
         CHARACTER (LEN=1) :: Active="Y"
! DECLARATION FOR DBREAD COMMON BLOCK
         CHARACTER (len=1024) :: RECLN
! **********************************************************************
!
      SCREEN_OUTPUT = TRIM(FILE_TYPE)//'-'//BASE_FILE_NAME
!      CALL MG_LOCATE_WRITE(16,30,SCREEN_OUTPUT,0,0)
      FILE_NAME = TRIM(BASE_FILE_DIRECTORY())// &
                      DATA_FILE_CODE//"B"//TRIM(BASE_FILE_NAME )//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=COAL_ESCALATIONS_FILE_EXISTS)
      MAX_VECTOR_NO_BASEFILE = 0
      IF(COAL_ESCALATIONS_FILE_EXISTS) THEN
!
         OPEN(10,FILE=FILE_NAME,STATUS="OLD")
         OPEN(11,FILE=TRIM(OUTPUT_DIRECTORY())//"BC"// &
                                TRIM(COAL_ESCA_BIN_FILE_NAME), &
                                ACCESS="DIRECT",STATUS="UNKNOWN", &
                                RECL=COAL_ESCALATIONS_LRECL)
!
!
         IREC = 0
         READ(10,*) DELETE
         DO ! TABLES
            DO ! TABLE RECORDS
               READ(10,'(A)',IOSTAT=IOS) RECLN
!
               IF(IOS /= 0) EXIT ! END OF FILE
!
               IF(RECLN(1:1) == '7') THEN ! END OF TABLE ! EXIT AT BOTTO
                  EXIT ! GO TO NEXT TABLE
               ENDIF
!
               VALUES_30 = -9999.
               VALUES_30(0) = 0.
               START_MONTH = 0
               END_MONTH = 0
               RECLN = TRIM(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
               READ(RECLN,*,IOSTAT=IOS) DELETE, &
                                        DESCRIPTION,VECTOR_NO, &
                                        VECTOR_TYPE,GROWTH_RATE_YEAR, &
                                        VALUES_30, &
                                        COMMENT,START_MONTH,END_MONTH
!
               IF(IOS /= 0) THEN
                  WRITE(4,*) TRIM(RECLN)
                  call end_program("Stop: coal_vector_obj CVO1")
               ENDIF
               IREC = IREC + 1
               DO I = 1, 30
                  IF(VALUES_30(I) == -9999.) THEN
                     VALUES_30(I) = VALUES_30(I-1)
                  ENDIF
               ENDDO
               IF(DELETE < 8) THEN
                  ESCALATIONS_VECTOR_BASEFILE = &
                                         ESCALATIONS_VECTOR_BASEFILE + 1
                  MAX_VECTOR_NO_BASEFILE = MAX(VECTOR_NO, &
                                                 MAX_VECTOR_NO_BASEFILE)
               ENDIF
               WRITE(11,REC=IREC) DELETE, &
                                  VECTOR_NO, &
                                  VECTOR_TYPE,GROWTH_RATE_YEAR, &
                                  VALUES_30, &
                                  START_MONTH,END_MONTH

            ENDDO ! TABLE RECORDS
            IF(IOS /= 0) EXIT
         ENDDO ! READ TABLES
         CLOSE(10)
         CLOSE(11)
      ENDIF
      RETURN
      END
! **********************************************************************
      LOGICAL FUNCTION READ_COAL_ESCALATION_FILE(BASE_YEAR)
         USE COAL_ESCALATIONS_MAKEBIN_INPUTS
         USE COAL_ESCALATION_VECTORS_DATA
         CHARACTER (LEN=256) :: OUTPUT_DIRECTORY
         INTEGER (KIND=4) :: IOS
         integer (kind=2) :: iArray,DELETE,IREC,nArray,Yr,BASE_YEAR

         READ_COAL_ESCALATION_FILE = COAL_ESCALATIONS_FILE_EXISTS
         IF(COAL_ESCALATIONS_FILE_EXISTS) THEN
            IF(PROCESS_COAL_ESCA_FILE_OL == 'OL') THEN
               nArray = ESCALATIONS_VECTOR_OVERLAY
               MaxVectorNum = MAX_VECTOR_NO_OVERLAY
            ELSE
               nArray = ESCALATIONS_VECTOR_BASEFILE
               MaxVectorNum = MAX_VECTOR_NO_BASEFILE
            ENDIF
! ALLOCATE ARRAYS
            IF(ALLOCATED(COAL_VECTOR_MAP)) THEN
               DEALLOCATE (COAL_VECTOR_MAP)
            ENDIF
            IF(.NOT. ALLOCATED(COAL_VECTOR_MAP)) THEN
               ALLOCATE (COAL_VECTOR_MAP(-1:MaxVectorNum))
            ENDIF
            IF(ALLOCATED(COAL_VECTOR_TYPE_IS_VALUE)) THEN
               DEALLOCATE (COAL_VECTOR_TYPE_IS_VALUE, &
                              COAL_VECTOR_VALUES)
            ENDIF
            IF(.NOT. ALLOCATED(COAL_VECTOR_TYPE_IS_VALUE)) THEN
               ALLOCATE (COAL_VECTOR_TYPE_IS_VALUE(-1:nArray), &
                         COAL_VECTOR_VALUES(31,-1:nArray))
            ENDIF
            COAL_VECTOR_TYPE_IS_VALUE = .FALSE.
            COAL_VECTOR_VALUES = 0.
            COAL_VECTOR_MAP = 0.
!
            OPEN(10,FILE=TRIM(OUTPUT_DIRECTORY())// &
              PROCESS_COAL_ESCA_FILE_OL// &
                                TRIM(COAL_ESCA_BIN_FILE_NAME), &
                                  ACCESS="DIRECT",STATUS="UNKNOWN", &
                                    RECL=COAL_ESCALATIONS_LRECL)
            IREC = 0
            iArray = 1
            DO
               IREC = IREC + 1
               READ(10,REC=IREC,IOSTAT=IOS) DELETE, &
                                            VECTOR_NO,VECTOR_TYPE, &
                                            GROWTH_RATE_YEAR, &
                                            VALUES_30, &
                                            START_MONTH,END_MONTH
               IF(IOS /= 0) EXIT
               IF(DELETE > 7) CYCLE
               COAL_VECTOR_MAP(VECTOR_NO) = iArray
               IF(INDEX(VECTOR_TYPE,'V') /= 0) THEN
                  COAL_VECTOR_TYPE_IS_VALUE(iArray) = .TRUE.
                  GROWTH_RATE_YEAR = GROWTH_RATE_YEAR - BASE_YEAR
                  IF(GROWTH_RATE_YEAR > 0 .AND. &
                                             GROWTH_RATE_YEAR < 31) THEN
                     COAL_VECTOR_VALUES(31,iArray) = VALUES_30(30)/100.
!
                     DO Yr = GROWTH_RATE_YEAR, 30
                        VALUES_30(Yr) = VALUES_30(Yr-1) * &
                                                 (1.+VALUES_30(Yr)/100.)
                     ENDDO
                  ELSE
                     IF(VALUES_30(30-1) /= 0.) THEN
                        COAL_VECTOR_VALUES(30+1,iArray) = -1. + &
                                           VALUES_30(30)/VALUES_30(30-1)
                     ELSE
                        COAL_VECTOR_VALUES(30+1,iArray) = 0.
                     ENDIF
                  ENDIF
                  COAL_VECTOR_VALUES(1:30,iArray) = VALUES_30(1:30)
               ELSE
                  COAL_VECTOR_VALUES(1:30,iArray) = VALUES_30(1:30)/100.
                  COAL_VECTOR_VALUES(30+1,iArray) = &
                                           COAL_VECTOR_VALUES(30,iArray)
               ENDIF
! AFTER ALL IS DONE
               iArray = iArray + 1
               IF(iArray > nArray) EXIT
            ENDDO

            CLOSE(10)
         ENDIF
      END FUNCTION
! ***********************************************************************
      LOGICAL FUNCTION RETURN_COAL_VECTOR(R_VECTOR_VALUES,R_VECTOR_NO)
! ***********************************************************************
         USE COAL_ESCALATION_VECTORS_DATA
         REAL (KIND=4) :: R_VECTOR_VALUES(*)
         INTEGER (KIND=4) :: R_VECTOR_NO,I4

! THE VALUES FOR A VECTOR ARE RETURN FOR THE AVAILABLE DATA YEARS
!
         IF(-1 <= R_VECTOR_NO .AND. R_VECTOR_NO <= MaxVectorNum) THEN
            I4 = COAL_VECTOR_MAP(R_VECTOR_NO)
            RETURN_COAL_VECTOR = COAL_VECTOR_TYPE_IS_VALUE(I4)
            IF(COAL_VECTOR_TYPE_IS_VALUE(I4)) THEN
               R_VECTOR_VALUES(1:30) = COAL_VECTOR_VALUES(1:30,I4)
            ELSE
               R_VECTOR_VALUES(1:30)=1.+COAL_VECTOR_VALUES(1:30,I4)
            ENDIF
         ELSE
            RETURN_COAL_VECTOR = .TRUE.
            R_VECTOR_VALUES(1:30) = 0.
            WRITE(4,*) "Coal Escalation vector",R_VECTOR_NO, &
                                                      ' does not exist.'
         ENDIF
      RETURN
      END
!***********************************************************************
      SUBROUTINE COAL_ESCALATIONS_MAKEOVL(OVERLAY_FILE_NAME)
!***********************************************************************
!
!
      USE COAL_ESCALATIONS_MAKEBIN_INPUTS
!
! SIMULATION VARIABLES
!
         INTEGER (KIND=2) ::   UNIT_NUM=10,INUNIT,IREC,DELETE
         INTEGER (KIND=4) :: IOS
         CHARACTER (LEN=*) :: OVERLAY_FILE_NAME
         CHARACTER (LEN=256) :: FILE_NAME,OUTPUT_DIRECTORY
         LOGICAL (KIND=4) :: DATA_FILE_EXISTS
! DECLARATION FOR DBREAD COMMON BLOCK
         CHARACTER (len=1024) :: RECLN
! **********************************************************************
!
      FILE_NAME=TRIM(OUTPUT_DIRECTORY())//DATA_FILE_CODE//"O"// &
                                        TRIM(OVERLAY_FILE_NAME)//".DAT"
      INQUIRE(FILE=FILE_NAME,EXIST=DATA_FILE_EXISTS)
      IF(DATA_FILE_EXISTS .AND. COAL_ESCALATIONS_FILE_EXISTS) THEN
         MAX_VECTOR_NO_OVERLAY = 0
         ESCALATIONS_VECTOR_OVERLAY = 0
         INUNIT = 12
         IF(PROCESS_COAL_ESCA_FILE_OL == 'BC') THEN
            OPEN(11,FILE=TRIM(OUTPUT_DIRECTORY())//"BC"// &
                                TRIM(COAL_ESCA_BIN_FILE_NAME), &
                                     ACCESS="DIRECT",STATUS="UNKNOWN", &
                                            RECL=COAL_ESCALATIONS_LRECL)
            INUNIT = 11
         ENDIF
         OPEN(12,FILE=TRIM(OUTPUT_DIRECTORY())//"OL"// &
                                TRIM(COAL_ESCA_BIN_FILE_NAME), &
                                      ACCESS="DIRECT",STATUS="UNKNOWN", &
                                            RECL=COAL_ESCALATIONS_LRECL)
!
         OPEN(10,FILE=FILE_NAME)
         READ(10,*) DELETE
!
         READ(10,'(A)',IOSTAT=IOS) RECLN
         DO WHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE
            READ(10,'(A)',IOSTAT=IOS) RECLN
         ENDDO
!
         IREC = 0
         DO
            IREC = IREC + 1
            READ(INUNIT,REC=IREC,IOSTAT=IOS)DELETE, &
                                  VECTOR_NO, &
                                  VECTOR_TYPE,GROWTH_RATE_YEAR, &
                                  VALUES_30, &
                                  START_MONTH,END_MONTH
            IF(IOS /= 0) EXIT
            RECLN = TRIM(RECLN)//',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
            READ(RECLN,*,IOSTAT=IOS) DELETE, &
                                        DESCRIPTION,VECTOR_NO, &
                                        VECTOR_TYPE,GROWTH_RATE_YEAR, &
                                        VALUES_30, &
                                        COMMENT,START_MONTH,END_MONTH
            READ(10,'(A)',IOSTAT=IOS) RECLN
            DO WHILE(RECLN(1:1) == '7')  ! END OF OVERLAY TABLE
               READ(10,'(A)',IOSTAT=IOS) RECLN
            ENDDO
            IF(DELETE < 8) THEN
               ESCALATIONS_VECTOR_OVERLAY = &
                                          ESCALATIONS_VECTOR_OVERLAY + 1
               MAX_VECTOR_NO_OVERLAY = MAX(VECTOR_NO, &
                                                  MAX_VECTOR_NO_OVERLAY)
            ENDIF
            WRITE(12,REC=IREC) DELETE, &
                               VECTOR_NO, &
                               VECTOR_TYPE,GROWTH_RATE_YEAR, &
                               VALUES_30, &
                               START_MONTH,END_MONTH
         ENDDO
         CLOSE(10)
         CLOSE(12)
         IF(PROCESS_COAL_ESCA_FILE_OL == 'BC') CLOSE(11)
         PROCESS_COAL_ESCA_FILE_OL = 'OL'
      ENDIF
      RETURN
      END
!
      SUBROUTINE RESET_VECTORS_OL()
      USE COAL_ESCALATIONS_MAKEBIN_INPUTS
         PROCESS_COAL_ESCA_FILE_OL = 'BC'
      RETURN
      END

