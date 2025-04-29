module dirpath
implicit none
      FUNCTION OUTPUT_DIRECTORY()
      CHARACTER (LEN=256) :: OUTPUT_DIRECTORY, CurPath
         call CurDir(" ",CurPath)
         OUTPUT_DIRECTORY  = trim(CurPath)//'\'
      END
      FUNCTION BASE_FILE_DIRECTORY()
      CHARACTER (LEN=256) :: BASE_FILE_DIRECTORY, CurPath
         call CurDir(" ",CurPath)
         BASE_FILE_DIRECTORY  = trim(CurPath)//'\'
      END
      subroutine CurDir(DriveCharIn,CurPath) ! returns without trailing '\'
        USE SERVICE_ROUTINES
        character*1 DriveCharIn
        character*(*) CurPath
        CHARACTER*1024 CURRENT_DIRECTORY
        integer IY
        IY = GETCWD(CURRENT_DIRECTORY)
        IF(IY == 0) THEN
           IY = LEN_TRIM(CURRENT_DIRECTORY)
           IF(CURRENT_DIRECTORY(IY:IY) == '\') THEN
              CURRENT_DIRECTORY(IY:IY) = ' '
           ENDIF
           CurPath = TRIM(CURRENT_DIRECTORY)
        ELSE
           CurPath = ' '
        ENDIF
      end ! subroutine CurDir
end module dirpath