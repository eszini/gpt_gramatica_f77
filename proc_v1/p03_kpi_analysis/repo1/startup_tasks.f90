module startup_tasks
use logging
use miscmod
use cl_data
implicit none
contains

function get_base_family_name(cl)
character (len=256) :: get_base_family_name, rslt, cl
integer :: pos

    pos=index(cl, " ")
    rslt=cl(pos+1:len_trim(cl))
    pos=index(rslt, " ")
    rslt=rslt(1: pos-1)
    
    get_base_family_name=trim(rslt)

end function get_base_family_name

subroutine startup_dsex(TEMP_BASE_FAMILY_NAME)
    character (len=256), intent(inout) :: TEMP_BASE_FAMILY_NAME
    character (len=1024) :: anotherstring
    integer :: length

        if(trim(ClData%command_line)=="" .or. &
       len_trim(ClData%command_line)<2) then
            call end_program("dsex:00018 - command line not set.")
        ELSE
            call write_log_entry("startup_dsex:1", &
            "Command line is " // trim(ClData%command_line) // ".")
        endif
        
        call write_log_entry("dsex:00017", "In start-up routine.")
         
         call write_log_entry("sdsex:0001", trim(log_message))

         temp_base_family_name=get_base_family_name(ClData%command_line)

         CALL UPC(TEMP_BASE_FAMILY_NAME,TEMP_BASE_FAMILY_NAME)
         call UPC(ClData%command_line, ClData%command_line)

         length = INDEX(ClData%command_line,' ')
         
         if(ClData%command_line(1:1)=='"') then
            ClData%scename=ClData%command_line(2:length-1)
         else
            ClData%scename=ClData%command_line(1:length)
         endif

         call write_log_entry( &
         "startup_tasks:scename", &
         "SCENAME=" // trim(ClData%scename))

         IF(trim(ClData%SCENAME) == ' ') then
            CALL CRITICAL_ERROR_MSG(1) 
        endif
end subroutine startup_dsex


end module startup_tasks
