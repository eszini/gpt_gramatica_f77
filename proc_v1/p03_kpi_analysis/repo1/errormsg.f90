!     Previous change: MSG 9/23/2009 2:02:46 PM
!     Last change: Name change to errormsg.f90. Moving to f95 standard
!     Author: Pablo Bilbao
!     Date: 2024-05-28
!***********************************************************************
!
!     SUBROUTINE TO PRINT ABNORMAL PROGRAM EXIT
!
!***********************************************************************
!
      SUBROUTINE ERROR_MESSAGE
      USE REALWIN
      use SpinDriftLib
	  use PROD_ARRAYS_DIMENSIONS
      use sizecom
	  use string
	  use end_routine


      CHARACTER (LEN=256) :: FILE_IN,FILE_SEARCH,CURRENT_DIRECTORY
      INTEGER (KIND=4) :: FILSIZ,IOS
      INTEGER (KIND=2) :: START_LINE,ISERROR
      INTEGER (KIND=2) :: ROW,COL
      INTEGER (KIND=4) :: CURRENT_SCREEN(1024)
      LOGICAL (KIND=1) :: MESSAGE,DEBUG_ON,NO_WARNING_WINDOW,WARNING_MESSAGES
      INTEGER (KIND=2) :: INVERSE_VIDEO,I
      LOGICAL (KIND=1) :: USER_TERMINATED
      COMMON /ERRORS/ INVERSE_VIDEO,MESSAGE,DEBUG_ON,USER_TERMINATED
      INTEGER (KIND=2) :: MAX_YEARS
      CHARACTER (LEN=2) :: DATA_DRIVE
      
      COMMON /DATA_DRIVE_LOCATION/ DATA_DRIVE
      INTEGER (KIND=2) :: RETURN_DRILLING_REPORT_STATUS,VOID_INT2
      LOGICAL (KIND=1) :: DATA_NOT_IN_CASH_DRILLING
      LOGICAL (KIND=1) :: DATA_NOT_IN_BALANCE_DRILLING
      LOGICAL (KIND=1) :: DATA_NOT_IN_INCOME_DRILLING
      LOGICAL (KIND=1) :: DATA_NOT_IN_TAX_DRILLING, SP_CAPEX_ACTIVE
      LOGICAL (KIND=4) :: OPEN_LOGICAL
      CHARACTER (LEN=5) :: GET_SCENAME
      INTEGER (KIND=4) :: SIMDONE_HANDLE,NUMBER_OF_BYTES
      CHARACTER (LEN=15) :: EXIT_CODE
      LOGICAL (KIND=4) :: SIMDONE_DELETED
      VOID_INT2 = RETURN_DRILLING_REPORT_STATUS(DATA_NOT_IN_CASH_DRILLING, & 
	  DATA_NOT_IN_BALANCE_DRILLING,DATA_NOT_IN_INCOME_DRILLING,DATA_NOT_IN_TAX_DRILLING)
      IF(DATA_NOT_IN_TAX_DRILLING) THEN
         CALL ERASE('MSG'//TRIM(GET_SCENAME())//'.XDD')
         CALL ERASE('MSGTXDRL.RPT')
      ENDIF
      IF(DATA_NOT_IN_CASH_DRILLING) THEN
         CALL ERASE('MSG'//TRIM(GET_SCENAME())//'.CDD')
         CALL ERASE('MSGCHDRL.RPT')
      ENDIF
      IF(DATA_NOT_IN_BALANCE_DRILLING) THEN
         CALL ERASE('MSG'//TRIM(GET_SCENAME())//'.BDD')
         CALL ERASE('MSGBLDRL.RPT')
      ENDIF
      IF(DATA_NOT_IN_INCOME_DRILLING) THEN
         CALL ERASE('MSG'//TRIM(GET_SCENAME())//'.DID')
         CALL ERASE('MSGINDRL.RPT')
      ENDIF

      IF(MESSAGE .AND. .NOT. USER_TERMINATED) THEN
      CALL CAPTSCN(CURRENT_SCREEN)
      CALL SETATTRW(int(4,2),INVERSE_VIDEO)

      ROW = 8
      CALL LOCATEW(4,ROW,14)
      CALL PRINTW(4,"ษอออออออออออออออออออออออออออออออออออออออออออออออป")
      CALL LOCATEW(4,ROW+1,14)
      CALL PRINTW(4,"บ           MIDAS GOLD ended abnormally!        บ")
      CALL LOCATEW(4,ROW+2,14)
      CALL PRINTW(4,"บ  Press any key.  Then use Print Screen to     บ")
      CALL LOCATEW(4,ROW+3,14)
      CALL PRINTW(4,"บ  record the problem.  Call M.S. Gerber &      บ")
      CALL LOCATEW(4,ROW+4,14)
      CALL PRINTW(4,"บ  Associates at (614) 486-6711 to report it.   บ")
      CALL LOCATEW(4,ROW+5,14)
      CALL PRINTW(4,"บ  Once recorded, press any key to continue.    บ")
      CALL LOCATEW(4,ROW+6,14)
      CALL PRINTW(4,"ศอออออออออออออออออออออออออออออออออออออออออออออออผ")
      CALL ANYKEY
      CALL DISPSCN(CURRENT_SCREEN)
      CALL ANYKEY
      CALL LOCATEW(4,ROW+7,0)
      ENDIF
      WARNING_MESSAGES = FILSIZ(DATA_DRIVE//'MIDAS.ERR') > 10
      IF(WARNING_MESSAGES .AND. .NOT. MESSAGE .AND. .NOT.USER_TERMINATED &
                                        .AND. NO_WARNING_WINDOW()) THEN
         START_LINE = 8
         COL = 14
         CALL SETATTRW(int(4,2),INVERSE_VIDEO)
         CALL LOCATEW(4,START_LINE,COL)
         CALL PRINTW(4,"ษอออออออออออออออออออออออออออออออออออออออออป")
         CALL LOCATEW(4,START_LINE+1,COL)
         IF(DEBUG_ON) THEN
            CALL PRINTW(4,"บ    Debug information was generated.     บ")
         ELSE
            CALL PRINTW(4,"บ    Warning messages were generated.     บ")
         ENDIF
         CALL LOCATEW(4,START_LINE+2,COL)
         CALL PRINTW(4,"บ    Select Warning Messages in the       บ")
         CALL LOCATEW(4,START_LINE+3,COL)
         IF(DEBUG_ON) THEN
            CALL PRINTW(4,"บ    Simulation sub-menu to review it.    บ")
         ELSE
            CALL PRINTW(4,"บ    Simulation sub-menu to review them.  บ")
         ENDIF
         CALL LOCATEW(4,START_LINE+4,COL)
         CALL PRINTW(4,"บ        Press any key to continue.       บ")
         CALL LOCATEW(4,START_LINE+5,COL)
         CALL PRINTW(4,"ศอออออออออออออออออออออออออออออออออออออออออผ")
         CALL ANYKEY
         CALL LOCATE(START_LINE+6,0)
      ENDIF
! 
! ERASE OVERLAY BINARY FILES
!
!
! IN LAHEY 3.x THE ARGUMENT LIST IS AS BELOW
      CALL CURDIR(DATA_DRIVE(1:1),CURRENT_DIRECTORY)
      IF(LEN(TRIM(CURRENT_DIRECTORY)) .GT. 3) CURRENT_DIRECTORY =  TRIM(CURRENT_DIRECTORY)//'\'
      FILE_SEARCH = TRIM(CURRENT_DIRECTORY)//"OL*.BIN"
      CALL ERASEWC(FILE_SEARCH)
! 
! ERASE BASE BINARY FILES
!
      FILE_SEARCH = TRIM(CURRENT_DIRECTORY)//"BC*.BIN"
      CALL ERASEWC(FILE_SEARCH)
! 
! ERASE BASE BINARY FILES
!
      CLOSE(8800,STATUS='DELETE',IOSTAT=IOS)
      FILE_SEARCH = TRIM(CURRENT_DIRECTORY)//"MSGFUAST.BIN"
      CALL ERASE(FILE_SEARCH)
      FILE_SEARCH = TRIM(CURRENT_DIRECTORY)//"SIMSTART"
      CALL ERASE(FILE_SEARCH)
      er_message = ' Stop requested by errormsg. Code 17 '
      IF(SP_CAPEX_ACTIVE()) then
        CALL end_program(er_message)
      end if
      er_message = ' '
      INQUIRE(UNIT=10,OPENED=OPEN_LOGICAL) 
      IF(OPEN_LOGICAL) CLOSE(10)
      FILE_SEARCH = TRIM(CURRENT_DIRECTORY)//"SIMDONE.LOG"
      CALL ERASE(FILE_SEARCH)
      OPEN(10,FILE=FILE_SEARCH,STATUS='NEW')
     

      SCREEN_MESSAGES = "SIMDONE DELETED"
      CALL MG_LOCATE_WRITE(18,70,TRIM(SCREEN_MESSAGES),3,2)
      CALL RW_PROCESS_MESSAGES()

      CALL MG_LOCATE_WRITE(18,70,TRIM(SCREEN_MESSAGES),3,2)
      CALL RW_PROCESS_MESSAGES()

      SCREEN_MESSAGES = "Writing Log File"
      CALL MG_LOCATE_WRITE(18,70,TRIM(SCREEN_MESSAGES),3,2)
      CALL RW_PROCESS_MESSAGES()

      
      IF(USER_TERMINATED) THEN
         WRITE(10,*) '14 Exit Code'
         CLOSE(10)
         EXIT_CODE = '14 Exit Code'
         er_message = ' Function errormsg.f90 requested stop. Code 14 - Program terminated by user'
         CALL end_program(er_message)
      ELSEIF(MESSAGE) THEN
         IF(WARNING_MESSAGES) THEN
           WRITE(10,*) '15 Exit Code'
           CLOSE(10)
            EXIT_CODE = "15 Exit Code"
            er_message = ' Function errormsg.f90 requested stop. Code 15 '
            CALL end_program(er_message)
         ELSE   
           WRITE(10,*) '16 Exit Code'
           CLOSE(10)
            EXIT_CODE = "16 Exit Code"
            er_message = ' Function errormsg.f90 requested stop. Code 16 '
            CALL end_program(er_message)
         ENDIF
      ELSE
         IF(WARNING_MESSAGES) THEN
           WRITE(10,*) '18 Exit Code'
           CLOSE(10)
            EXIT_CODE = "18 Exit Code"
            er_message = ' Function errormsg.f90 requested stop. Code 18 '
            CALL end_program(er_message)
         ELSE   
           WRITE(10,'(A)') '17 Exit Code'
           CLOSE(10)

            SCREEN_MESSAGES = "Log File is Written"
            CALL MG_LOCATE_WRITE(18,70,TRIM(SCREEN_MESSAGES),3,2)
            CALL RW_PROCESS_MESSAGES()
            er_message = ' Function errormsg.f90 requested stop. Code 17. ' // &
                "No error."
            CALL end_program(er_message)
         ENDIF
      ENDIF

      RETURN
      END
	  
	  
	  
	  
	  
      SUBROUTINE CRITICAL_ERROR_MSG(ERR_NUM)
      use end_routine, only: end_program, er_message
	  use logging
      use SpinDriftLib
      use PROD_ARRAYS_DIMENSIONS
      use sizecom
      use mod_base_year
	  character (len=1024) :: actual_path
      CHARACTER*(*) FILE_NAME, base_file_definition 
      character*(*) :: base_file_family
      CHARACTER*152 ISO_ERR_MESSAGE
      INTEGER (KIND=2) :: get_endyr,NBLANK
      INTEGER ERR_NUM
      INTEGER IOS
	  integer (kind=2), parameter :: ERR_NO_STUDY=1
	  
	  er_message=""
	  
      SELECT CASE(ERR_NUM)
      CASE (ERR_NO_STUDY)
			er_message='No study name was specified.'
            WRITE(4,1010) 'No study name was specified.'
      CASE (2)
	        er_message='File MSGPARM.VAL was not found.'
            WRITE(4,1010) 'File MSGPARM.VAL was not found.'
            WRITE(4,1010) 'Select View/Set Parameters'
            WRITE(4,1010) 'to create this file.'
      CASE (4)
	        er_message='The MSGPARM.VAL file is empty or bad.'
            WRITE(4,1010) 'The MSGPARM.VAL file is empty or bad.'
      CASE (5)
	        er_message="Last forecast year is less than or " //"equal to base year"
	 
            WRITE(4,'(A,I4,A,I4)') ' Last forecast year, ',get_endyr(), ' is less than or equal to base year, ',get_BASE_YEAR(),'.'
            WRITE(4,'(A)') ' Last forecast year '// 'MUST be greater than the base year.'
      CASE DEFAULT
	        er_message='Unidentified critical error.'
            WRITE(4,1010) 'Unidentified critical error.'
      END SELECT
	  
      er_message='See WARNING MESSAGES - Errormsg.for-1 ' // trim(er_message)
	 
      call end_program(er_message)
  
  
      ENTRY CRITICAL_ERROR_MSG6(base_file_definition,base_file_family,FILE_NAME)

       call getcwd(actual_path)

      er_message="errormsg-2: File " //TRIM(FILE_NAME) // " for " //"base_file_definition=" // trim(base_file_definition) // &
      " and base file family=" // trim(base_file_family) // " does not exist in path " // trim(actual_path)
       
	   call write_log_entry("errormsg-2a", er_message)
       WRITE(4,1010) trim(er_message)

      call end_program(er_message)
      ENTRY CRITICAL_ERROR_MSG7
         WRITE(4,1010) 'System forecast chosen.  '//'No forecast file specified.'
      er_message='See WARNING MESSAGES -Errormsg.for-3'
      call end_program(er_message)
      return
      ENTRY CRITICAL_ERROR_MSG8
         WRITE(4,1010) 'A Run Specs file for this version of MIDAS'//' Gold was not specified. Version'
         WRITE(4,1010) '4A2 and later has split the Run Specs and '//'Detail Reports file of earlier'
         WRITE(4,1010) 'versions into separate files and moved other'//' items to the Production'
         WRITE(4,1010) 'Parameters file. Creating these new files '//'takes 15-20 minutes.'
         WRITE(4,1010) 'Call for help when doing this. Thank you.'

      er_message='See WARNING MESSAGES -Errormsg.for-4'
      call end_program(er_message)
      return
      ENTRY CRITICAL_ERROR_FORECAST_FILE(FILE_NAME,IOS)
         CALL IOSTAT_MSG(IOS,ISO_ERR_MESSAGE)

         WRITE(4,1010) 'There is a problem reading file: '//TRIM(FILE_NAME)//'.'
         WRITE(4,'(A,I3,A)')' The run-time error code is ',MOD(IOS,246),'.  The run-time error message is:'
         IF(NBLANK(ISO_ERR_MESSAGE) <= 76) THEN
            WRITE(4,1010) TRIM(ISO_ERR_MESSAGE)
         ELSE
            WRITE(4,1010) ISO_ERR_MESSAGE(1:76)

            WRITE(4,1010) TRIM(ISO_ERR_MESSAGE(76:))
         ENDIF
         CALL LOCATE(3,50)

      er_message='See WARNING MESSAGES -Errormsg.for-5'
      call end_program(er_message)
 1010 FORMAT(1X,A,I8,A)
      END
      SUBROUTINE SEE_WARNING_MESSAGES()
         USE flusher
         CHARACTER (LEN=30) :: ERROR_MESSAGE
         CALL flush_unit(int(9,2))
         CALL flush_unit(int(4,2))
         ERROR_MESSAGE = "See Warning Messages"
         CALL ERROR(ERROR_MESSAGE)
      END SUBROUTINE

