module mk_fileio
use end_routine
use string
use filemod
implicit none
    integer (kind=2) :: m_mk_unit_no_read, m_mk_unit_no_write
contains
        subroutine close_mk_file_read
            close(m_mk_unit_no_read)
            m_mk_unit_no_read=0
        end subroutine close_mk_file_read
        subroutine close_mk_file_write
            close(m_mk_unit_no_write)
            m_mk_unit_no_write=0
        end subroutine close_mk_file_write
        
        function open_mk_file_read(R_MKROUP_OL, lrecl)
        implicit none
        integer :: open_mk_file_read
        CHARACTER (LEN=2), intent(inout) :: R_MKROUP_OL
        character (len=256) :: output_directory ! External
        integer (kind=2), intent(in) :: lrecl

        character (len=1024) :: fname
        integer :: ios

        call RETURN_MKROUP_OL(R_MKROUP_OL)

         fname=trim(OUTPUT_DIRECTORY())//R_MKROUP_OL// &
               "MKROUP.BIN"


          m_mk_unit_no_read=get_new_unit()
          open_mk_file_read=m_mk_unit_no_read
          OPEN(m_mk_unit_no_read,FILE=fname,ACCESS="DIRECT", &
             STATUS="OLD", RECL=LRECL, IOSTAT=ios, &
             action="read", form="unformatted")

         if (ios/=0) then
            call end_program("wh_objt:0008 - error " // &
            trim(itos(int(ios))) // " was returned from open " // &
             "statement.")
         end if

      end function open_mk_file_read
      function open_mk_file_write(R_MKROUP_OL, lrecl)
        implicit none
        integer :: open_mk_file_write
        CHARACTER (LEN=2), intent(inout) :: R_MKROUP_OL
        character (len=256) :: output_directory ! External
        integer (kind=2), intent(in) :: lrecl

        character (len=1024) :: fname
        integer :: ios

        call RETURN_MKROUP_OL(R_MKROUP_OL)
        
         fname=trim(OUTPUT_DIRECTORY())//"BCMKROUP.BIN"


          m_mk_unit_no_write=get_new_unit()
          open_mk_file_write=m_mk_unit_no_write
          OPEN(m_mk_unit_no_write,FILE=fname,ACCESS="DIRECT", &
             STATUS="UNKNOWN", RECL=LRECL, IOSTAT=ios, &
             action="write", form="unformatted")

         if (ios/=0) then
            call end_program("wh_objt:0007 - error " // &
            trim(itos(int(ios))) // " was returned from open " // &
             "statement.")
         end if

      end function open_mk_file_write
end module mk_fileio
