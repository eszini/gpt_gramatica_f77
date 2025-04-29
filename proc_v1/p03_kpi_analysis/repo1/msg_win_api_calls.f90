      MODULE MSG_WIN_API_CALLS
         use windows_h
         dll_import CreateFileA
         dll_import SetFilePointer
         dll_import SetEndOfFile
         dll_import CloseHandle
         INTEGER (KIND=4) :: CreateFileA,File_handle
         INTEGER (KIND=8) :: SetFilePointer
         LOGICAL (KIND=4) :: SetEndOfFile, CloseHandle
      integer FILE_BEGIN             /z'00000000'/
      integer FILE_FLAG_RANDOM_ACCESS/z'10000000'/
      integer (KIND=8), parameter :: TwoTo32nd=z'100000000'
      END MODULE