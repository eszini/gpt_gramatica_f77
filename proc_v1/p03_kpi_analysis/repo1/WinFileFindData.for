      module WinFileFindData
         implicit none
         dll_import FindFirstFileA, FindNextFileA
         dll_import FindClose
         integer FindFirstFileA, FindNextFileA
         logical :: FindClose

         type filetime
            sequence
            integer :: LowDateTime
            integer :: HighDatTime
         end type filetime
  
         type WIN32_FIND_DATA
            sequence
            integer :: FileAttributes
            type(filetime) :: CreationTime
            type(filetime) :: LastAcessTime
            type(filetime) :: LastWriteTime
    
            integer :: FileSizeHigh
            integer :: FileSizeLow
            integer :: Reserved0
            integer :: Reserved1
            character*260 :: FileName
            character*14 :: AlternateFileName
         end type WIN32_FIND_DATA  
      endmodule WinFileFindData
