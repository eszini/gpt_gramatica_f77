module end_routine
use params
implicit none
character(len=4096) :: er_message=""
character(len=512) :: params_wf=""
contains
    function get_end_message_filename()
    character(len=512) :: get_end_message_filename
        params_wf=trim(working_folder)
        get_end_message_filename=trim(params_wf) // "\exit_message.txt"
    end function get_end_message_filename
	
    subroutine end_program(message)
        character(len=*), intent(in) :: message
        character(len=256) :: filename=""
        integer :: file_unit, open_status
        character(len=10) :: stat
        logical :: error_exists
        integer :: byui, noer
        filename=get_end_message_filename()
        error_exists=.true.
        stat="REPLACE"
        
        file_unit=6669
        error_exists=.true.
        byui=index(message, "by user")
        noer=index(message, "No error")
        
        if(byui>0.or.noer>0) then
                error_exists=.false.
        end if
        if(error_exists) then

            open(unit=file_unit, file=trim(filename), action="write", &
                IOSTAT=open_status)
            if(open_status/=0)then
                stop "Unable to open exit_message file."
            else
                write(file_unit,*) trim(message)
                call  flush(file_unit)
                close(file_unit)
            endif
            stop "Error condition encountered. See exit_message.txt for details." 
        else
            stop
        end if
        
        
    end subroutine end_program
    
   
end module end_routine