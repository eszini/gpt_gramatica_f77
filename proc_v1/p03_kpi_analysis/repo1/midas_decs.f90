module midas_decs
implicit none
contains
	subroutine clear_files_from_last_run()
	character (len=256) :: current_directory
	character (len=256) :: file_search
	logical :: status_file_exists, status_file_open
	
      CALL CURDIR(" ",CURRENT_DIRECTORY)
      FILE_SEARCH = TRIM(CURRENT_DIRECTORY)//"\SIMSTART"
      OPEN(10,FILE=FILE_SEARCH,STATUS='REPLACE')
      CLOSE(10)              
      FILE_SEARCH = TRIM(CURRENT_DIRECTORY)//"\SIMDONE"
      CALL ERASE(FILE_SEARCH)
      FILE_SEARCH = trim(CURRENT_DIRECTORY)//"\mdsgold.log"
      INQUIRE(FILE=FILE_SEARCH,EXIST=STATUS_FILE_EXISTS)
      IF(STATUS_FILE_EXISTS) THEN
         INQUIRE(UNIT=7101,OPENED=STATUS_FILE_OPEN)
         IF(STATUS_FILE_OPEN) CLOSE(7101)
         CALL ERASE(FILE_SEARCH)
      ENDIF
      FILE_SEARCH = TRIM(CURRENT_DIRECTORY)//"\exit_message.txt"
      INQUIRE(FILE=FILE_SEARCH,EXIST=STATUS_FILE_EXISTS)
      IF(STATUS_FILE_EXISTS) THEN
         INQUIRE(UNIT=7101,OPENED=STATUS_FILE_OPEN)
         IF(STATUS_FILE_OPEN) CLOSE(7101)
         CALL ERASE(FILE_SEARCH)
      ENDIF	
	end subroutine clear_files_from_last_run
end module midas_decs