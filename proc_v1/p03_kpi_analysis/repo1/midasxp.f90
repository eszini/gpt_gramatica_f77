!     Last change: GT 5/29/2008 2:09:39 PM
!***********************************************************************
!
!        COPYRIGHT (C) 1982-2003 M.S. GERBER & ASSOCIATES, INC.
!                         ALL RIGHTS RESERVED
!***********************************************************************
!
      SUBROUTINE MIDAS_XP()
      use end_routine, only: end_program, er_message
      use logging
      USE realwin
      use midas_decs

      use startup_tasks
      USE SIZECOM
      CHARACTER (LEN=1024) :: CURRENT_DIRECTORY,FILE_SEARCHF

      character (len=256) :: othervar
      INTEGER :: IY,CHDIR
      logical (KIND=1) :: LAHEY_LF95,SET_LAHEY_LF95
      CHARACTER (LEN=255) :: path_part
      CHARACTER (LEN=25) :: BUILD_STR,BUILD_NUM
      LOGICAL (KIND=1) :: PAUSE_ACTIVE
      LOGICAL (KIND=4) :: STATUS_FILE_EXISTS,STATUS_FILE_OPEN
      SAVE PAUSE_ACTIVE


      integer :: idx, charlen
      character (len=256) :: file_search


      CALL MG_SCREEN_WRITE_FUNCTIONS()
      !CALL GETENV("DEBUGDIR",CURRENT_DIRECTORY)
      call GETCWD(CURRENT_DIRECTORY)

      call write_log_entry("midasxp:1", "CURRENT_DIRECTORY: '" // &
       trim(CURRENT_DIRECTORY) // "'.")
 
      if(len_trim(current_directory) <2 .or. &
       TRIM(CURRENT_DIRECTORY) == "") THEN
         call end_program("MXP:1 - " //  &
          "Unable to retrieve current directory.")

      ELSE
         call write_log_entry("midasxp:4", "CURRENT_DIRECTORY=" // &
       trim(CURRENT_DIRECTORY))
     
         CURRENT_DIRECTORY=trim(CURRENT_DIRECTORY)


         IY = CHDIR(CURRENT_DIRECTORY)
	 call curdir(" ", current_directory)
	 
         PAUSE_ACTIVE = .TRUE.
      ENDIF

      call clear_files_from_last_run()

      CALL MIDAS_ROOT_PROGRAM

      FILE_SEARCH = TRIM(CURRENT_DIRECTORY)//"\SIMSTART"
      CALL ERASE(FILE_SEARCH)

!
      FILE_SEARCH = TRIM(CURRENT_DIRECTORY)//"\SIMSTATUS.LOG"
      INQUIRE(FILE=FILE_SEARCH,EXIST=STATUS_FILE_EXISTS)
      IF(STATUS_FILE_EXISTS) THEN
         INQUIRE(UNIT=7101,OPENED=STATUS_FILE_OPEN)
         IF(STATUS_FILE_OPEN) CLOSE(7101)
         CALL ERASE(FILE_SEARCH)
      ENDIF


!      
      IF(PAUSE_ACTIVE) PAUSE 'The simulation has finished.'
      IF(LAHEY_LF95()) THEN
         CALL ERROR_MESSAGE
      ENDIF
      RETURN ! STOP
      END