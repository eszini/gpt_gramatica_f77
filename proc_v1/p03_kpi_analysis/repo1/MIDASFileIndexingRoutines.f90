module file_indexing
implicit none
contains
!     ******************************************************************
!     MIDASFileIndexingRoutines.FOR
!     Copyright(c)  2000
!
!     Created: 9/30/2003 5:44:02 PM
!     Author : MARK S GERBER
!     Last change: MSG 4/8/2010 2:55:07 PM
!     ******************************************************************

!***********************************************************************
      LOGICAL FUNCTION INDEX_THIS_DETAILED_REPORT(StudyName,        &
                                                  ReportCode,       & 
                                                  ReportName,       &
                                                  STUDY_DIRECTORY)
!***********************************************************************
      USE realwin
      use logging

      IMPLICIT NONE

      CHARACTER (len=5) ::    StudyName
      CHARACTER (len=2) ::    ReportCode
      CHARACTER (len=256) ::  FILE_NAME,MESSAGE_TEXT
      CHARACTER (len=4) ::    NUM_INDEX_VALUE
      CHARACTER (len=3) ::    INDEX_EXT
      CHARACTER (len=30) ::   ReportName

      LOGICAL ::  FILE_EXISTS,FILE_OPEN

      INTEGER (kind=2) ::   RECORD_LENGTH,        &
                DIMENSIONS,                       &
                LAST_YEAR,                        &
                NUM_OF_END_POINTS

      CHARACTER (LEN=*) ::  STUDY_DIRECTORY
      CHARACTER (LEN=20), ALLOCATABLE, DIMENSION(:) :: DIMENSION_NAME
      INTEGER (kind=2), ALLOCATABLE, DIMENSION(:) :: DIMENSION_LENGHT
      CHARACTER (LEN=1), ALLOCATABLE, DIMENSION(:) :: DIMENSION_TYPE

      INTEGER, ALLOCATABLE, DIMENSION(:) :: DEM_1_PTR,    &
                                            DEM_2_PTR,    &
                                            DEM_3_PTR,    & 
                                            DEM_4_PTR,    &
                                            DEM_5_PTR,    &
                                            DEM_6_PTR,    &
                                            DEM_7_PTR

      CHARACTER(LEN=80),ALLOCATABLE,DIMENSION(:) :: DEM_1_NAME_STORAGE,     &
                                                    DEM_2_NAME_STORAGE,     &
                                                    DEM_3_NAME_STORAGE,     &
                                                    DEM_4_NAME_STORAGE,     &
                                                    DEM_5_NAME_STORAGE,     &
                                                    DEM_6_NAME_STORAGE,     &
                                                    DEM_7_NAME_STORAGE

      INTEGER :: DEM_1_UNIQUE_VALUES,      &
              DEM_2_UNIQUE_VALUES,         &
              DEM_3_UNIQUE_VALUES,         &
              DEM_4_UNIQUE_VALUES,         &
              DEM_5_UNIQUE_VALUES,         &
              DEM_6_UNIQUE_VALUES,         &
              DEM_7_UNIQUE_VALUES

      INTEGER :: DEM_1_MAX_VALUES,         &
              DEM_2_MAX_VALUES,            &
              DEM_3_MAX_VALUES,            &
              DEM_4_MAX_VALUES,            &
              DEM_5_MAX_VALUES,            &
              DEM_6_MAX_VALUES,            &
              DEM_7_MAX_VALUES

      INTEGER (kind=8) ::   MEMORY_NEEDED
      INTEGER, ALLOCATABLE, DIMENSION(:,:) :: DEM_1_REC_POS,       &
                                              DEM_2_REC_POS,       &
                                              DEM_3_REC_POS,       &
                                              DEM_4_REC_POS,       &
                                              DEM_5_REC_POS,       &
                                              DEM_6_REC_POS,       &
                                              DEM_7_REC_POS

      INTEGER :: I,J,RECORDS_IN_FILE,TOTAL_DIMENSION_LENGTH,       &
              MAX_UNIQUE_VALUES,FILE_UNIT

      CHARACTER (LEN=256) :: DIM_BALANCE
      REAL (kind=4) ::  REAL_TIME
      INTEGER :: IOS
      INTEGER :: START_TIME,CURRENT_TIME,SUB_DIMENSION,            &
              PROCESS_START_TIME,PROCESS_END_TIME

      REAL ::  REAL_VALUE
      CHARACTER (len=80) ::  CHAR_NAME
      CHARACTER (len=4) ::  CHAR_NUM
      INTEGER :: DIM_VALUE
      EQUIVALENCE(REAL_VALUE,CHAR_NUM)
      INTEGER (kind=8) ::   FILE_SIZE
!      INTEGER (kind=4) ::   BYTE_SIZE
!      EQUIVALENCE (FILE_SIZE, BYTE_SIZE)
      INTEGER (kind=4) ::   L_BYTE_POS
      INTEGER (kind=8) ::   BYTE_POS
      integer (kind=2) :: base_year_loc
      EQUIVALENCE (BYTE_POS,L_BYTE_POS)
!
         call write_log_entry("midasindex:0001", &
            "Study_directory=" // trim(STUDY_DIRECTORY))
            
         IF(trim(STUDY_DIRECTORY) == ' ') THEN
            FILE_NAME = 'MSG'//trim(StudyName)//'.'//ReportCode//'D'
         ELSE
            FILE_NAME = trim(STUDY_DIRECTORY)//                   &
                    'MSG'//trim(StudyName)//'.'//ReportCode//'D'
         ENDIF
         call write_log_entry("midasindex:0002", &
            "File_name=" // trim(file_name))
         call write_log_entry("midasindex:0003", &
            "Checking file open status.")
         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS,FLEN=FILE_SIZE, &
                     OPENED=FILE_OPEN,NUMBER=FILE_UNIT)
         call write_log_entry("midasindex:0004", &
            "Checked file open status.")
         IF(.NOT. FILE_EXISTS) THEN
           INDEX_THIS_DETAILED_REPORT = FILE_EXISTS
           call write_log_entry("midasindex:0005", "returning.")
           RETURN
         ENDIF
         call write_log_entry("midasindex:0006", "closing if open.")
         
         IF(FILE_OPEN) CLOSE(FILE_UNIT)
         OPEN(11,FILE=FILE_NAME,ACCESS='TRANSPARENT',STATUS='OLD')
         READ(11,REC=2,IOSTAT=IOS) RECORD_LENGTH,         &
                                   DIMENSIONS,            &
                                   base_year_loc,             &
                                   LAST_YEAR,             &
                                   NUM_OF_END_POINTS
         CLOSE(11)
         IF(IOS /= 0) RETURN
         IF(DIMENSIONS < 2) THEN
            WRITE(MESSAGE_TEXT,*) ReportCode," ",ReportName
            CALL PROCESS_ERROR_MESSAGES(MESSAGE_TEXT)
            CALL TWO_DIMENSION_REPORT()
            RETURN
         ENDIF
         OPEN(11,FILE=FILE_NAME,ACCESS='DIRECT',STATUS='OLD',       &
                                          RECL=RECORD_LENGTH)
         ALLOCATE(DIMENSION_NAME(DIMENSIONS),      &
                  DIMENSION_TYPE(DIMENSIONS),      &
                  DIMENSION_LENGHT(DIMENSIONS))
!
         TOTAL_DIMENSION_LENGTH = 0
         DO I = 1, DIMENSIONS
            READ(11,REC=3+I,IOSTAT=IOS) DIMENSION_NAME(I),     &
                             DIMENSION_TYPE(I),                &
                             DIMENSION_LENGHT(I)
            IF(IOS /= 0) THEN
               WRITE(MESSAGE_TEXT,*) "ERROR indexing ",        &
                                               ReportCode," ",ReportName
               CALL PROCESS_ERROR_MESSAGES(MESSAGE_TEXT)
               CALL PROCESS_ERROR_MESSAGES(FILE_NAME)
               WRITE(MESSAGE_TEXT,*) "Dimensions ",DIMENSIONS
               CALL PROCESS_ERROR_MESSAGES(MESSAGE_TEXT)
               WRITE(MESSAGE_TEXT,*) "Record Lenght ",RECORD_LENGTH
               CALL PROCESS_ERROR_MESSAGES(MESSAGE_TEXT)
               DEALLOCATE(DIMENSION_NAME,               &
                          DIMENSION_TYPE,               &
                          DIMENSION_LENGHT)
               RETURN
            ENDIF
            TOTAL_DIMENSION_LENGTH = TOTAL_DIMENSION_LENGTH            &
                                                + DIMENSION_LENGHT(I)
         ENDDO
         CALL RW_PROCESS_MESSAGES()
         RECORDS_IN_FILE = FILE_SIZE/RECORD_LENGTH
         MAX_UNIQUE_VALUES = MIN(50000,RECORDS_IN_FILE)
!
         DEM_1_UNIQUE_VALUES = 0
         DEM_2_UNIQUE_VALUES = 0
         DEM_3_UNIQUE_VALUES = 0
         DEM_4_UNIQUE_VALUES = 0
         DEM_5_UNIQUE_VALUES = 0
         DEM_6_UNIQUE_VALUES = 0
         DEM_7_UNIQUE_VALUES = 0
!
         ALLOCATE(DEM_1_PTR(MAX_UNIQUE_VALUES))
         DEM_1_PTR = 0
         ALLOCATE(DEM_1_NAME_STORAGE(MAX_UNIQUE_VALUES))
!
         ALLOCATE(DEM_2_PTR(MAX_UNIQUE_VALUES))
         DEM_2_PTR = 0
         ALLOCATE(DEM_2_NAME_STORAGE(MAX_UNIQUE_VALUES))
!
         IF(DIMENSIONS >= 3) THEN
            ALLOCATE(DEM_3_PTR(MAX_UNIQUE_VALUES))
            DEM_3_PTR = 0
            ALLOCATE(DEM_3_NAME_STORAGE(MAX_UNIQUE_VALUES))
         ENDIF
         IF(DIMENSIONS >= 4) THEN
            ALLOCATE(DEM_4_PTR(MAX_UNIQUE_VALUES))
            DEM_4_PTR = 0
            ALLOCATE(DEM_4_NAME_STORAGE(MAX_UNIQUE_VALUES))
         ENDIF
         IF(DIMENSIONS >= 5) THEN
            ALLOCATE(DEM_5_PTR(MAX_UNIQUE_VALUES))
            DEM_5_PTR = 0
            ALLOCATE(DEM_5_NAME_STORAGE(MAX_UNIQUE_VALUES))
         ENDIF
         IF(DIMENSIONS >= 6) THEN
            ALLOCATE(DEM_6_PTR(MAX_UNIQUE_VALUES))
            DEM_6_PTR = 0
            ALLOCATE(DEM_6_NAME_STORAGE(MAX_UNIQUE_VALUES))
         ENDIF
         IF(DIMENSIONS >= 7) THEN
            ALLOCATE(DEM_7_PTR(MAX_UNIQUE_VALUES))
            DEM_7_PTR = 0
            ALLOCATE(DEM_7_NAME_STORAGE(MAX_UNIQUE_VALUES))
         ENDIF

         I = DIMENSIONS+4
         CALL TIMER(START_TIME)
         PROCESS_START_TIME = START_TIME
         SUB_DIMENSION = 0
         MESSAGE_TEXT=' Indexing '//ReportCode//"-"//trim(ReportName)
         CALL RW_UPDATE_RUNTIME_MESSAGES(MESSAGE_TEXT)
         DO
            READ(11,REC=I,IOSTAT=IOS)                    &
                                   DIM_BALANCE(1:TOTAL_DIMENSION_LENGTH)
            CALL RW_PROCESS_MESSAGES()
            IF(IOS /= 0) EXIT
! First dimension
            CHAR_NAME =  DIM_BALANCE(1:DIMENSION_LENGHT(1))
            DO J = 1, DEM_1_UNIQUE_VALUES
               IF(DEM_1_NAME_STORAGE(J) ==               &
                                  CHAR_NAME(1:DIMENSION_LENGHT(1))) THEN
                  DEM_1_PTR(J) = DEM_1_PTR(J) + 1
                  EXIT
               ENDIF
            ENDDO
            IF(J > DEM_1_UNIQUE_VALUES) THEN
               DEM_1_UNIQUE_VALUES = J
               DEM_1_NAME_STORAGE(J) = CHAR_NAME
               DEM_1_PTR(J) = DEM_1_PTR(J) + 1
            ENDIF
            SUB_DIMENSION = DIMENSION_LENGHT(1)
! Second dimension
            CHAR_NAME =  DIM_BALANCE(SUB_DIMENSION+1:    &
                                      SUB_DIMENSION+DIMENSION_LENGHT(2))
            DO J = 1, DEM_2_UNIQUE_VALUES
               IF(DEM_2_NAME_STORAGE(J) ==               &
                               CHAR_NAME(1:DIMENSION_LENGHT(2))) THEN
                  DEM_2_PTR(J) = DEM_2_PTR(J) + 1
                  EXIT
               ENDIF
            ENDDO
            IF(J > DEM_2_UNIQUE_VALUES) THEN
               DEM_2_UNIQUE_VALUES = J
               DEM_2_NAME_STORAGE(J) = CHAR_NAME
               DEM_2_PTR(J) = DEM_2_PTR(J) + 1
            ENDIF
            SUB_DIMENSION = SUB_DIMENSION + DIMENSION_LENGHT(2)
! Third dimension
            IF(DIMENSIONS >= 3) THEN
               CHAR_NAME =  DIM_BALANCE(SUB_DIMENSION+1:  &
                                   SUB_DIMENSION+DIMENSION_LENGHT(3))
               DO J = 1, DEM_3_UNIQUE_VALUES
                  IF(DEM_3_NAME_STORAGE(J) ==             &
                                  CHAR_NAME(1:DIMENSION_LENGHT(3))) THEN
                     DEM_3_PTR(J) = DEM_3_PTR(J) + 1
                     EXIT
                  ENDIF
               ENDDO
               IF(J > DEM_3_UNIQUE_VALUES) THEN
                  DEM_3_UNIQUE_VALUES = J
                  DEM_3_NAME_STORAGE(J) = CHAR_NAME
                  DEM_3_PTR(J) = DEM_3_PTR(J) + 1
               ENDIF
               SUB_DIMENSION = SUB_DIMENSION + DIMENSION_LENGHT(3)
            ENDIF
! Forth dimension
            IF(DIMENSIONS >= 4) THEN
               CHAR_NAME =  DIM_BALANCE(SUB_DIMENSION+1:SUB_DIMENSION   &
                                               + DIMENSION_LENGHT(4))
               DO J = 1, DEM_4_UNIQUE_VALUES
                  IF(DEM_4_NAME_STORAGE(J) ==                           &
                                  CHAR_NAME(1:DIMENSION_LENGHT(4))) THEN
                     DEM_4_PTR(J) = DEM_4_PTR(J) + 1
                     EXIT
                  ENDIF
               ENDDO
               IF(J > DEM_4_UNIQUE_VALUES) THEN
                  DEM_4_UNIQUE_VALUES = J
                  DEM_4_NAME_STORAGE(J) = CHAR_NAME
                  DEM_4_PTR(J) = DEM_4_PTR(J) + 1
               ENDIF
               SUB_DIMENSION = SUB_DIMENSION + DIMENSION_LENGHT(4)
            ENDIF
! Fifth dimension
            IF(DIMENSIONS >= 5) THEN
               CHAR_NAME = DIM_BALANCE(SUB_DIMENSION+1:SUB_DIMENSION  &
                                               + DIMENSION_LENGHT(5))
               DO J = 1, DEM_5_UNIQUE_VALUES
                  IF(DEM_5_NAME_STORAGE(J) ==                         &
                               CHAR_NAME(1:DIMENSION_LENGHT(5))) THEN
                     DEM_5_PTR(J) = DEM_5_PTR(J) + 1
                     EXIT
                  ENDIF
               ENDDO
               IF(J > DEM_5_UNIQUE_VALUES) THEN
                  DEM_5_UNIQUE_VALUES = J
                  DEM_5_NAME_STORAGE(J) = CHAR_NAME
                  DEM_5_PTR(J) = DEM_5_PTR(J) + 1
               ENDIF
               SUB_DIMENSION = SUB_DIMENSION + DIMENSION_LENGHT(5)
            ENDIF
! Sixth dimension
            IF(DIMENSIONS >= 6) THEN
               CHAR_NAME = DIM_BALANCE(SUB_DIMENSION+1:SUB_DIMENSION  &
                                               + DIMENSION_LENGHT(6))
               DO J = 1, DEM_6_UNIQUE_VALUES
                  IF(DEM_6_NAME_STORAGE(J) ==                         &
                               CHAR_NAME(1:DIMENSION_LENGHT(6))) THEN
                     DEM_6_PTR(J) = DEM_6_PTR(J) + 1
                     EXIT
                  ENDIF
               ENDDO
               IF(J > DEM_6_UNIQUE_VALUES) THEN
                  DEM_6_UNIQUE_VALUES = J
                  DEM_6_NAME_STORAGE(J) = CHAR_NAME
                  DEM_6_PTR(J) = DEM_6_PTR(J) + 1
               ENDIF
               SUB_DIMENSION = SUB_DIMENSION + DIMENSION_LENGHT(6)
            ENDIF
! Seventh dimension
            IF(DIMENSIONS >= 7) THEN
               CHAR_NAME = DIM_BALANCE(SUB_DIMENSION+1:SUB_DIMENSION  &
                                               + DIMENSION_LENGHT(7))
               DO J = 1, DEM_7_UNIQUE_VALUES
                  IF(DEM_7_NAME_STORAGE(J) ==                         &
                               CHAR_NAME(1:DIMENSION_LENGHT(7))) THEN
                     DEM_7_PTR(J) = DEM_7_PTR(J) + 1
                     EXIT
                  ENDIF
               ENDDO
               IF(J > DEM_7_UNIQUE_VALUES) THEN
                  DEM_7_UNIQUE_VALUES = J
                  DEM_7_NAME_STORAGE(J) = CHAR_NAME
                  DEM_7_PTR(J) = DEM_7_PTR(J) + 1
               ENDIF
            ENDIF
!
            IF(MOD(I,MAX(1,INT(RECORDS_IN_FILE/20))) == 0) THEN
               CALL TIMER(CURRENT_TIME)
               REAL_TIME = FLOAT(CURRENT_TIME-START_TIME)/100
            ENDIF
            I = I + 1
         ENDDO
         RECORDS_IN_FILE = I-1
         CALL TIMER(CURRENT_TIME)
         REAL_TIME = FLOAT(CURRENT_TIME-START_TIME)/100
!
! Dimension arrays for record positions
!
         MEMORY_NEEDED = 0
         DEM_1_MAX_VALUES = MAXVAL(DEM_1_PTR(1:DEM_1_UNIQUE_VALUES))
         MEMORY_NEEDED = MEMORY_NEEDED                      &
                        + 4 * (DEM_1_MAX_VALUES+1) * DEM_1_UNIQUE_VALUES
         DEALLOCATE(DEM_1_PTR)
         ALLOCATE(DEM_1_PTR(DEM_1_UNIQUE_VALUES),           &
                    DEM_1_REC_POS(DEM_1_UNIQUE_VALUES,DEM_1_MAX_VALUES))
         DEM_1_PTR = 0
!
         DEM_2_MAX_VALUES = MAXVAL(DEM_2_PTR(1:DEM_2_UNIQUE_VALUES))
         MEMORY_NEEDED = MEMORY_NEEDED                      &
                        + 4 * (DEM_2_MAX_VALUES+1) * DEM_2_UNIQUE_VALUES
         DEALLOCATE(DEM_2_PTR)
         ALLOCATE(DEM_2_PTR(DEM_2_UNIQUE_VALUES),           & 
                    DEM_2_REC_POS(DEM_2_UNIQUE_VALUES,DEM_2_MAX_VALUES))
         DEM_2_PTR = 0
!
!
         IF(DIMENSIONS >= 3) THEN
            DEM_3_MAX_VALUES = MAXVAL(DEM_3_PTR(1:DEM_3_UNIQUE_VALUES))
            MEMORY_NEEDED = MEMORY_NEEDED                   &
                        + 4 * (DEM_3_MAX_VALUES+1) * DEM_3_UNIQUE_VALUES
            DEALLOCATE(DEM_3_PTR)
            ALLOCATE(DEM_3_PTR(DEM_3_UNIQUE_VALUES),        &
                    DEM_3_REC_POS(DEM_3_UNIQUE_VALUES,DEM_3_MAX_VALUES))
            DEM_3_PTR = 0
         ENDIF
         IF(DIMENSIONS >= 4) THEN
            DEM_4_MAX_VALUES = MAXVAL(DEM_4_PTR(1:DEM_4_UNIQUE_VALUES))
            MEMORY_NEEDED = MEMORY_NEEDED                   &
                        + 4 * (DEM_4_MAX_VALUES+1) * DEM_4_UNIQUE_VALUES
            DEALLOCATE(DEM_4_PTR)
            ALLOCATE(DEM_4_PTR(DEM_4_UNIQUE_VALUES),        &
                    DEM_4_REC_POS(DEM_4_UNIQUE_VALUES,DEM_4_MAX_VALUES))
            DEM_4_PTR = 0
         ENDIF
!
         IF(DIMENSIONS >= 5) THEN
            DEM_5_MAX_VALUES = MAXVAL(DEM_5_PTR(1:DEM_5_UNIQUE_VALUES))
            MEMORY_NEEDED = MEMORY_NEEDED                   &
                        + 4 * (DEM_5_MAX_VALUES+1) * DEM_5_UNIQUE_VALUES
            DEALLOCATE(DEM_5_PTR)
            ALLOCATE(DEM_5_PTR(DEM_5_UNIQUE_VALUES),        &
                    DEM_5_REC_POS(DEM_5_UNIQUE_VALUES,DEM_5_MAX_VALUES))
            DEM_5_PTR = 0
         ENDIF
         IF(DIMENSIONS >= 6) THEN
            DEM_6_MAX_VALUES = MAXVAL(DEM_6_PTR(1:DEM_6_UNIQUE_VALUES))
            MEMORY_NEEDED = MEMORY_NEEDED                   &
                        + 4 * (DEM_6_MAX_VALUES+1) * DEM_6_UNIQUE_VALUES
            DEALLOCATE(DEM_6_PTR)
            ALLOCATE(DEM_6_PTR(DEM_6_UNIQUE_VALUES),        &
                    DEM_6_REC_POS(DEM_6_UNIQUE_VALUES,DEM_6_MAX_VALUES))
            DEM_6_PTR = 0
         ENDIF
         IF(DIMENSIONS >= 7) THEN
            DEM_7_MAX_VALUES = MAXVAL(DEM_7_PTR(1:DEM_7_UNIQUE_VALUES))
            MEMORY_NEEDED = MEMORY_NEEDED                   &
                        + 4 * (DEM_7_MAX_VALUES+1) * DEM_7_UNIQUE_VALUES
            DEALLOCATE(DEM_7_PTR)
            ALLOCATE(DEM_7_PTR(DEM_7_UNIQUE_VALUES),        &
                    DEM_7_REC_POS(DEM_7_UNIQUE_VALUES,DEM_7_MAX_VALUES))
            DEM_7_PTR = 0
         ENDIF
!
         I = DIMENSIONS+4
         CALL TIMER(START_TIME)
         DO
            READ(11,REC=I,IOSTAT=IOS)                       &
                                   DIM_BALANCE(1:TOTAL_DIMENSION_LENGTH)
            CALL RW_PROCESS_MESSAGES()
            IF(IOS /= 0) EXIT
!
! First dimension
            CHAR_NAME = DIM_BALANCE(1:DIMENSION_LENGHT(1))
            DO J = 1, DEM_1_UNIQUE_VALUES
               IF(DEM_1_NAME_STORAGE(J) ==                  &
                                  CHAR_NAME(1:DIMENSION_LENGHT(1))) THEN
                  DEM_1_PTR(J) = DEM_1_PTR(J) + 1
                  DEM_1_REC_POS(J,DEM_1_PTR(J)) = I - 1 !LF95 IS 1 BASED RECORD
                  EXIT
               ENDIF
            ENDDO
            SUB_DIMENSION = DIMENSION_LENGHT(1)
! Second dimension
            CHAR_NAME = DIM_BALANCE(SUB_DIMENSION+1:        &
                                      SUB_DIMENSION+DIMENSION_LENGHT(2))
            DO J = 1, DEM_2_UNIQUE_VALUES                
               IF(DEM_2_NAME_STORAGE(J) ==                  &
                                  CHAR_NAME(1:DIMENSION_LENGHT(2))) THEN
                  DEM_2_PTR(J) = DEM_2_PTR(J) + 1
                  DEM_2_REC_POS(J,DEM_2_PTR(J)) = I - 1 !LF95 IS 1 BASED RECORD
                  EXIT
               ENDIF
            ENDDO
            SUB_DIMENSION = SUB_DIMENSION + DIMENSION_LENGHT(2)
!
! Third dimension
            IF(DIMENSIONS >= 3) THEN
                  CHAR_NAME =  DIM_BALANCE(SUB_DIMENSION+1:   &
                                      SUB_DIMENSION+DIMENSION_LENGHT(3))
                  DO J = 1, DEM_3_UNIQUE_VALUES
                     IF(DEM_3_NAME_STORAGE(J) ==              &
                                  CHAR_NAME(1:DIMENSION_LENGHT(3))) THEN
                        DEM_3_PTR(J) = DEM_3_PTR(J) + 1
                        DEM_3_REC_POS(J,DEM_3_PTR(J)) = I - 1 !LF95 IS 1 BASED RECORD
                        EXIT
                     ENDIF
                  ENDDO
               SUB_DIMENSION = SUB_DIMENSION + DIMENSION_LENGHT(3)
            ENDIF
!
! Forth dimension
            IF(DIMENSIONS >= 4) THEN
                  CHAR_NAME =  DIM_BALANCE(SUB_DIMENSION+1:   &
                                      SUB_DIMENSION+DIMENSION_LENGHT(4))
                  DO J = 1, DEM_4_UNIQUE_VALUES
                     IF(DEM_4_NAME_STORAGE(J) ==              &
                                  CHAR_NAME(1:DIMENSION_LENGHT(4))) THEN
                        DEM_4_PTR(J) = DEM_4_PTR(J) + 1
                        DEM_4_REC_POS(J,DEM_4_PTR(J)) = I - 1 !LF95 IS 1 BASED RECORD
                        EXIT
                     ENDIF
                  ENDDO
               SUB_DIMENSION = SUB_DIMENSION + DIMENSION_LENGHT(4)
            ENDIF
!
! Fifth dimension
            IF(DIMENSIONS >= 5) THEN
               CHAR_NAME =  DIM_BALANCE(SUB_DIMENSION+1:      &
                                   SUB_DIMENSION+DIMENSION_LENGHT(5))
               DO J = 1, DEM_5_UNIQUE_VALUES
                  IF(DEM_5_NAME_STORAGE(J) ==                 &
                                  CHAR_NAME(1:DIMENSION_LENGHT(5))) THEN
                     DEM_5_PTR(J) = DEM_5_PTR(J) + 1
                     DEM_5_REC_POS(J,DEM_5_PTR(J)) = I - 1 !LF95 IS 1 BASED RECORD
                     EXIT
                  ENDIF
               ENDDO
               SUB_DIMENSION = SUB_DIMENSION + DIMENSION_LENGHT(5)
            ENDIF
!
! Sixth dimension
            IF(DIMENSIONS >= 6) THEN
               CHAR_NAME =  DIM_BALANCE(SUB_DIMENSION+1:      &
                                   SUB_DIMENSION+DIMENSION_LENGHT(6))
               DO J = 1, DEM_6_UNIQUE_VALUES
                  IF(DEM_6_NAME_STORAGE(J) ==                 &
                                  CHAR_NAME(1:DIMENSION_LENGHT(6))) THEN
                     DEM_6_PTR(J) = DEM_6_PTR(J) + 1
                     DEM_6_REC_POS(J,DEM_6_PTR(J)) = I - 1 !LF95 IS 1 BASED RECORD
                     EXIT
                  ENDIF
               ENDDO
               SUB_DIMENSION = SUB_DIMENSION + DIMENSION_LENGHT(6)
            ENDIF
!
! Seventh dimension
            IF(DIMENSIONS >= 7) THEN
               CHAR_NAME =  DIM_BALANCE(SUB_DIMENSION+1:      &
                                   SUB_DIMENSION+DIMENSION_LENGHT(7))
               DO J = 1, DEM_7_UNIQUE_VALUES
                  IF(DEM_7_NAME_STORAGE(J) ==                 &
                                  CHAR_NAME(1:DIMENSION_LENGHT(7))) THEN 
                     DEM_7_PTR(J) = DEM_7_PTR(J) + 1
                     DEM_7_REC_POS(J,DEM_7_PTR(J)) = I - 1 !LF95 IS 1 BASED RECORD
                     EXIT
                  ENDIF
               ENDDO
               SUB_DIMENSION = SUB_DIMENSION + DIMENSION_LENGHT(7)
            ENDIF
!
            IF(MOD(I,MAX(1,INT(RECORDS_IN_FILE/20))) == 0) THEN
               CALL TIMER(CURRENT_TIME)
               REAL_TIME = FLOAT(CURRENT_TIME-START_TIME)/100
            ENDIF
            I = I + 1
         ENDDO
         CALL TIMER(CURRENT_TIME)
         REAL_TIME = FLOAT(CURRENT_TIME-START_TIME)/100
         CLOSE(11)
!
! PROCESS DIMENTION 1-ENDPOINTS
!
         START_TIME = CURRENT_TIME
         INDEX_EXT = ReportCode//'1'
         CALL INDEXING_FILE_HEADER_INFO(StudyName,                 &
                                        INDEX_EXT,                 &
                                        DEM_1_UNIQUE_VALUES,       &
                                        1,                         &
                                        DEM_1_UNIQUE_VALUES,       &
                                        DEM_1_PTR,                 &
                                        STUDY_DIRECTORY)

         DO I = 1, DEM_1_UNIQUE_VALUES
            IF(DEM_1_PTR(I) > 0) THEN
               CALL INT_INDEX_SORT(DEM_1_PTR(I),DEM_1_REC_POS,I,   &
                                                  1,DEM_1_UNIQUE_VALUES)
               IF(DIMENSION_TYPE(1) == 'N') THEN  ! AN NUMBER
                  CHAR_NUM = DEM_1_NAME_STORAGE(I)
                  DIM_VALUE = INT(REAL_VALUE)
                  WRITE(NUM_INDEX_VALUE,'(I4)') DIM_VALUE
                  WRITE(10) NUM_INDEX_VALUE
!     +            (RECORD_LENGTH*DEM_1_REC_POS(I,J),J=1,DEM_1_PTR(I))
               ELSE
                  WRITE(10)DEM_2_NAME_STORAGE(I)(1:DIMENSION_LENGHT(1))
!     +            (RECORD_LENGTH*DEM_1_REC_POS(I,J),J=1,DEM_1_PTR(I))
               ENDIF
               DO J = 1, DEM_1_PTR(I)
                  BYTE_POS = CAL_BYTE_POSITION(RECORD_LENGTH,      &
                                               DEM_1_REC_POS(I,J))
                  WRITE(10) L_BYTE_POS
                  CALL RW_PROCESS_MESSAGES()
               ENDDO
            ENDIF
         ENDDO
         CLOSE(10)
         DEALLOCATE(DEM_1_PTR,DEM_1_REC_POS,DEM_1_NAME_STORAGE)
         CALL TIMER(CURRENT_TIME)
         CALL RW_PROCESS_MESSAGES()
         REAL_TIME = FLOAT(CURRENT_TIME-START_TIME)/100
!
! PROCESS SECOND DIMENSION
!
         START_TIME = CURRENT_TIME
         INDEX_EXT = ReportCode//'2'
         CALL INDEXING_FILE_HEADER_INFO(StudyName,              &
                                        INDEX_EXT,              &
                                        DEM_2_UNIQUE_VALUES,    &
                                        1,                      &
                                        DEM_2_UNIQUE_VALUES,    &
                                        DEM_2_PTR,              &
                                        STUDY_DIRECTORY)
         DO I = 1, DEM_2_UNIQUE_VALUES
            IF(DEM_2_PTR(I) > 0) THEN
               CALL INT_INDEX_SORT(DEM_2_PTR(I),DEM_2_REC_POS,I,    &
                                           1,DEM_2_UNIQUE_VALUES)
               IF(DIMENSION_TYPE(2) == 'N') THEN  ! AN NUMBER
                  CHAR_NUM = DEM_2_NAME_STORAGE(I)
                  DIM_VALUE = INT(REAL_VALUE)
                  WRITE(NUM_INDEX_VALUE,'(I4)') DIM_VALUE
                  WRITE(10) NUM_INDEX_VALUE
!     +            (RECORD_LENGTH*DEM_2_REC_POS(I,J),J=1,DEM_2_PTR(I))
               ELSE
                  WRITE(10)DEM_2_NAME_STORAGE(I)(1:DIMENSION_LENGHT(2))
!    +            (RECORD_LENGTH*DEM_2_REC_POS(I,J),J=1,DEM_2_PTR(I))
               ENDIF
               DO J = 1, DEM_2_PTR(I)
                  BYTE_POS = CAL_BYTE_POSITION(RECORD_LENGTH,       &
                                               DEM_2_REC_POS(I,J))
                  WRITE(10) L_BYTE_POS
                  CALL RW_PROCESS_MESSAGES()
               ENDDO
            ENDIF
         ENDDO
         CLOSE(10)
         DEALLOCATE(DEM_2_PTR,DEM_2_REC_POS,DEM_2_NAME_STORAGE)
         CALL TIMER(CURRENT_TIME)
         REAL_TIME = FLOAT(CURRENT_TIME-START_TIME)/100
         CALL RW_PROCESS_MESSAGES()
!
! PROCESS THIRD DIMENSION
!
         IF(DIMENSIONS >= 3) THEN
            START_TIME = CURRENT_TIME
            INDEX_EXT = ReportCode//'3'
            CALL INDEXING_FILE_HEADER_INFO(StudyName,           &
                                        INDEX_EXT,              &
                                        DEM_3_UNIQUE_VALUES,    &
                                        1,                      &
                                        DEM_3_UNIQUE_VALUES,    &
                                        DEM_3_PTR,              &
                                        STUDY_DIRECTORY)
            DO I = 1, DEM_3_UNIQUE_VALUES
               IF(DEM_3_PTR(I) > 0) THEN
                  CALL INT_INDEX_SORT(DEM_3_PTR(I),DEM_3_REC_POS,I,   &
                                                  1,DEM_3_UNIQUE_VALUES)
                  IF(DIMENSION_TYPE(3) == 'N') THEN  ! AN NUMBER
                     CHAR_NUM = DEM_3_NAME_STORAGE(I)
                     DIM_VALUE = INT(REAL_VALUE)
                     WRITE(NUM_INDEX_VALUE,'(I4)') DIM_VALUE
                     WRITE(10) NUM_INDEX_VALUE
!     +            (RECORD_LENGTH*DEM_3_REC_POS(I,J),J=1,DEM_3_PTR(I))
                  ELSE
                     WRITE(10) DEM_3_NAME_STORAGE(I)                  &
                                                 (1:DIMENSION_LENGHT(3))
!     +                 (RECORD_LENGTH*DEM_3_REC_POS(I,J),
!     +                                              J=1,DEM_3_PTR(I))
                  ENDIF
                  DO J = 1, DEM_3_PTR(I)
                     BYTE_POS = CAL_BYTE_POSITION(RECORD_LENGTH,      &
                                                  DEM_3_REC_POS(I,J))
                     WRITE(10) L_BYTE_POS
                     CALL RW_PROCESS_MESSAGES()
                  ENDDO
               ENDIF
            ENDDO
            CLOSE(10)
            DEALLOCATE(DEM_3_PTR,DEM_3_REC_POS,DEM_3_NAME_STORAGE)
            CALL TIMER(CURRENT_TIME)
            REAL_TIME = FLOAT(CURRENT_TIME-START_TIME)/100
            CALL RW_PROCESS_MESSAGES()
         ENDIF
!
! PROCESS REMAINING DIMENTIONS
!
         IF(DIMENSIONS >= 4) THEN
            START_TIME = CURRENT_TIME
            INDEX_EXT = ReportCode//'4'
            CALL INDEXING_FILE_HEADER_INFO(StudyName,             &
                                           INDEX_EXT,             &
                                           DEM_4_UNIQUE_VALUES,   &
                                           1,                     &
                                           DEM_4_UNIQUE_VALUES,   &
                                           DEM_4_PTR,             & 
                                           STUDY_DIRECTORY)
            DO I = 1, DEM_4_UNIQUE_VALUES
               IF(DEM_4_PTR(I) > 0) THEN
                  CALL INT_INDEX_SORT(DEM_4_PTR(I),DEM_4_REC_POS,I,    &
                                                  1,DEM_4_UNIQUE_VALUES)
                  IF(DIMENSION_TYPE(4) == 'N') THEN  ! AN NUMBER
                     CHAR_NUM = DEM_4_NAME_STORAGE(I)
                     DIM_VALUE = INT(REAL_VALUE)
                     WRITE(NUM_INDEX_VALUE,'(I4)') DIM_VALUE
                     WRITE(10) NUM_INDEX_VALUE
!     +            (RECORD_LENGTH*DEM_4_REC_POS(I,J),J=1,DEM_4_PTR(I))
                  ELSE
                     WRITE(10) DEM_4_NAME_STORAGE(I)                   &
                                                 (1:DIMENSION_LENGHT(4))
!     +                 (RECORD_LENGTH*DEM_4_REC_POS(I,J),
!     +                                              J=1,DEM_4_PTR(I))
                  ENDIF
                  DO J = 1, DEM_4_PTR(I)
                     BYTE_POS = CAL_BYTE_POSITION(RECORD_LENGTH,       &
                                                  DEM_4_REC_POS(I,J))
                     WRITE(10) L_BYTE_POS
                     CALL RW_PROCESS_MESSAGES()
                  ENDDO
               ENDIF
            ENDDO
            CLOSE(10)
            CALL TIMER(CURRENT_TIME)
            REAL_TIME = FLOAT(CURRENT_TIME-START_TIME)/100

            DEALLOCATE(DEM_4_PTR,DEM_4_REC_POS,DEM_4_NAME_STORAGE)
            CALL RW_PROCESS_MESSAGES()
         ENDIF
!
         IF(DIMENSIONS >= 5) THEN
            START_TIME = CURRENT_TIME
            INDEX_EXT = ReportCode//'5'
            CALL INDEXING_FILE_HEADER_INFO(StudyName,             &
                                           INDEX_EXT,             &
                                           DEM_5_UNIQUE_VALUES,   &
                                           1,                     &
                                           DEM_5_UNIQUE_VALUES,   &
                                           DEM_5_PTR,             &
                                           STUDY_DIRECTORY)
            DO I = 1, DEM_5_UNIQUE_VALUES
               IF(DEM_5_PTR(I) > 0) THEN
                  CALL INT_INDEX_SORT(DEM_5_PTR(I),DEM_5_REC_POS,I,     &
                                                  1,DEM_5_UNIQUE_VALUES)
                  IF(DIMENSION_TYPE(5) == 'N') THEN  ! AN NUMBER
                     CHAR_NUM = DEM_5_NAME_STORAGE(I)
                     DIM_VALUE = INT(REAL_VALUE)
                     WRITE(NUM_INDEX_VALUE,'(I4)') DIM_VALUE
                     WRITE(10) NUM_INDEX_VALUE
!     +            (RECORD_LENGTH*DEM_5_REC_POS(I,J),J=1,DEM_5_PTR(I))
                  ELSE
                     WRITE(10) DEM_5_NAME_STORAGE(I)                    &
                                              (1:DIMENSION_LENGHT(5))
!     +                 (RECORD_LENGTH*DEM_5_REC_POS(I,J),
!     +                                              J=1,DEM_5_PTR(I))
                  ENDIF
                  DO J = 1, DEM_5_PTR(I)
                     BYTE_POS = CAL_BYTE_POSITION(RECORD_LENGTH,        &
                                                  DEM_5_REC_POS(I,J))
                     WRITE(10) L_BYTE_POS
                     CALL RW_PROCESS_MESSAGES()
                  ENDDO
               ENDIF
            ENDDO
            CLOSE(10)
            CALL TIMER(CURRENT_TIME)
            REAL_TIME = FLOAT(CURRENT_TIME-START_TIME)/100

            DEALLOCATE(DEM_5_PTR,DEM_5_REC_POS,DEM_5_NAME_STORAGE)
            CALL RW_PROCESS_MESSAGES()
         ENDIF
!
         IF(DIMENSIONS >= 6) THEN
            START_TIME = CURRENT_TIME
            INDEX_EXT = ReportCode//'6'
            CALL INDEXING_FILE_HEADER_INFO(StudyName,              &
                                           INDEX_EXT,              &
                                           DEM_6_UNIQUE_VALUES,    &
                                           1,                      &
                                           DEM_6_UNIQUE_VALUES,    & 
                                           DEM_6_PTR,              &
                                           STUDY_DIRECTORY)
            DO I = 1, DEM_6_UNIQUE_VALUES
               IF(DEM_6_PTR(I) > 0) THEN
                  CALL INT_INDEX_SORT(DEM_6_PTR(I),DEM_6_REC_POS,I,     &
                                                  1,DEM_6_UNIQUE_VALUES)
                  IF(DIMENSION_TYPE(6) == 'N') THEN  ! AN NUMBER
                     CHAR_NUM = DEM_6_NAME_STORAGE(I)
                     DIM_VALUE = INT(REAL_VALUE)
                     WRITE(NUM_INDEX_VALUE,'(I4)') DIM_VALUE
                     WRITE(10) NUM_INDEX_VALUE
                  ELSE
                     WRITE(10) DEM_6_NAME_STORAGE(I)                    &
                                                 (1:DIMENSION_LENGHT(6))

                  ENDIF
                  DO J = 1, DEM_6_PTR(I)
                     BYTE_POS = CAL_BYTE_POSITION(RECORD_LENGTH,        &
                                                  DEM_6_REC_POS(I,J))
                     WRITE(10) L_BYTE_POS
                     CALL RW_PROCESS_MESSAGES()
                  ENDDO
               ENDIF
            ENDDO
            CLOSE(10)
            CALL TIMER(CURRENT_TIME)
            REAL_TIME = FLOAT(CURRENT_TIME-START_TIME)/100

            DEALLOCATE(DEM_6_PTR,DEM_6_REC_POS,DEM_6_NAME_STORAGE)
            CALL RW_PROCESS_MESSAGES()
         ENDIF
!
         IF(DIMENSIONS >= 7) THEN
            START_TIME = CURRENT_TIME
            INDEX_EXT = ReportCode//'7'
            CALL INDEXING_FILE_HEADER_INFO(StudyName,               &
                                           INDEX_EXT,               &
                                           DEM_7_UNIQUE_VALUES,     &
                                           1,                       &
                                           DEM_7_UNIQUE_VALUES,     &
                                           DEM_7_PTR,               &
                                           STUDY_DIRECTORY)
            DO I = 1, DEM_7_UNIQUE_VALUES
               IF(DEM_7_PTR(I) > 0) THEN
                  CALL INT_INDEX_SORT(DEM_7_PTR(I),DEM_7_REC_POS,I, &
                                            1,DEM_7_UNIQUE_VALUES)
                  IF(DIMENSION_TYPE(7) == 'N') THEN  ! AN NUMBER
                     CHAR_NUM = DEM_7_NAME_STORAGE(I)
                     DIM_VALUE = INT(REAL_VALUE)
                     WRITE(NUM_INDEX_VALUE,'(I4)') DIM_VALUE
                     WRITE(10) NUM_INDEX_VALUE
!     +            (RECORD_LENGTH*DEM_7_REC_POS(I,J),J=1,DEM_7_PTR(I))
                  ELSE
                     WRITE(10)DEM_7_NAME_STORAGE(I)                 &
                                                 (1:DIMENSION_LENGHT(7))
!     +                 (RECORD_LENGTH*DEM_7_REC_POS(I,J),
!     +                                              J=1,DEM_7_PTR(I))
                  ENDIF
                  DO J = 1, DEM_7_PTR(I)
                     BYTE_POS = CAL_BYTE_POSITION(RECORD_LENGTH,    &
                                                  DEM_7_REC_POS(I,J))
                     WRITE(10) L_BYTE_POS
                     CALL RW_PROCESS_MESSAGES()
                  ENDDO
               ENDIF
            ENDDO
            CLOSE(10)
            CALL TIMER(CURRENT_TIME)
            REAL_TIME = FLOAT(CURRENT_TIME-START_TIME)/100

            DEALLOCATE(DEM_7_PTR,DEM_7_REC_POS,DEM_7_NAME_STORAGE)
            CALL RW_PROCESS_MESSAGES()
         ENDIF
!
         CALL TIMER(PROCESS_END_TIME)
         CALL RW_PROCESS_MESSAGES()
         REAL_TIME = FLOAT(PROCESS_END_TIME-PROCESS_START_TIME)/100.
         IF(REAL_TIME <= 60.) THEN
            IF(REAL_TIME < 10.) THEN
               WRITE(MESSAGE_TEXT,'(A,A,A,F6.3,A)')             &
                ' '//ReportCode,"-",trim(ReportName)//' took',  &
                REAL_TIME,' seconds to index.'
            ELSE
               WRITE(MESSAGE_TEXT,'(A,A,A,F7.3,A)')             &
                ' '//ReportCode,"-",trim(ReportName)//' took',  &
                REAL_TIME,' seconds to index.'
            ENDIF
         ELSE
            REAL_TIME = REAL_TIME/60.
            WRITE(MESSAGE_TEXT,'(A,A,A,F6.3,A)')                 &
                ' '//ReportCode,"-",trim(ReportName)//' took',   &
                                          REAL_TIME,' minutes to index.'
         ENDIF
         CALL RW_UPDATE_RUNTIME_MESSAGES(MESSAGE_TEXT)
         DEALLOCATE(DIMENSION_NAME,             &
                    DIMENSION_TYPE,             &
                    DIMENSION_LENGHT)
         INDEX_THIS_DETAILED_REPORT = .TRUE.
         CALL RW_PROCESS_MESSAGES()
      RETURN
      END function INDEX_THIS_DETAILED_REPORT
!***********************************************************************
      FUNCTION CAL_BYTE_POSITION(A,B)
!***********************************************************************
!
      INTEGER (kind=2) ::   A
      INTEGER :: B
      INTEGER (kind=8) ::   CAL_BYTE_POSITION,L_A,L_B
         L_A = A
         L_B = B
         CAL_BYTE_POSITION = L_A * L_B
      RETURN
      END function CAL_BYTE_POSITION
!***********************************************************************
      SUBROUTINE INDEXING_FILE_HEADER_INFO(StudyName,         &
                                           FILE_EXTENSION,    &
                                           R_UNIQUE_VALUES,   &
                                           START_LOOP,        &
                                           END_LOOP,          &
                                           NUM_OF_EACH_VALUE, &
                                           STUDY_DIRECTORY)
!***********************************************************************
!
      INTEGER :: START_LOOP,END_LOOP,I,R_UNIQUE_VALUES
      INTEGER (kind=2) ::   UNIQUE_VALUES,HEADER_LENGHT
      INTEGER :: NUM_OF_EACH_VALUE(START_LOOP:END_LOOP)
      CHARACTER (len=5) ::  StudyName
      CHARACTER (len=3) ::  FILE_EXTENSION
      CHARACTER (len=256) ::  FILE_IN
      CHARACTER (LEN=*) :: STUDY_DIRECTORY
!
         FILE_IN = 'MGX'//trim(StudyName)//'.'//FILE_EXTENSION
         IF(trim(STUDY_DIRECTORY) /= ' ') FILE_IN =           &
                                        trim(STUDY_DIRECTORY)//FILE_IN
         OPEN(10,FILE=FILE_IN,ACCESS="TRANSPARENT",STATUS="REPLACE")
         UNIQUE_VALUES = R_UNIQUE_VALUES
         HEADER_LENGHT = 4*(UNIQUE_VALUES + 1)
         WRITE(10) HEADER_LENGHT,UNIQUE_VALUES
!
         DO I = START_LOOP, END_LOOP
            IF(NUM_OF_EACH_VALUE(I) > 0) WRITE(10) NUM_OF_EACH_VALUE(I)
         ENDDO
      RETURN
      END  SUBROUTINE INDEXING_FILE_HEADER_INFO
!***********************************************************************
!
      SUBROUTINE INT_INDEX_SORT(ISUP,A,I_POS,SIZE1,SIZE2)
!
!***********************************************************************
! SORTS ISUP ITEMS INTO INCREASING ORDER BASED ON VALUES IN (1-BASED) ARRAY A;
! SHELL SORT ADAPTED FROM THE SOURCE ON PAGE 110 OF
! SOFTWARE TOOLS IN PASCAL, BY KERNIGHAN & PLAUGER, ADDISON-WESLEY 1981
      INTEGER :: SIZE1,SIZE2
      INTEGER :: ISUP,I,J,K,M,GAP,A(SIZE1:SIZE2,*)
      INTEGER :: I_POS

      GAP = ISUP/2
      DO WHILE(GAP > 0)
         DO I = GAP, ISUP
            J = I-GAP
            DO WHILE(J>0)
               K=J+GAP
               IF(A(I_POS,J) <= A(I_POS,K)) THEN
                  J = 0 ! BREAK THE WHILE LOOP (ASSIGN J=-1 FOR 0-BASED ARRAYS)
               ELSE ! INTERCHANGE THE ARRAY VALUES
                  M = A(I_POS,J)
                  A(I_POS,J) = A(I_POS,K)
                  A(I_POS,K) = M
               END IF
               J = J - GAP
            END DO
         END DO
         GAP = GAP/2
      END DO
      RETURN
      END SUBROUTINE INT_INDEX_SORT
end module file_indexing
