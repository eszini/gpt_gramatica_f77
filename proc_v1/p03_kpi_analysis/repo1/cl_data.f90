module cl_data
use miscmod
use logging
use clgetter
implicit none
    real :: emiss_blending_rate_ord
    
    integer, parameter :: command_line_size=256
    real (kind=4), allocatable :: saved_cl_ann_cap(:,:,:)
    real (kind=4), allocatable :: saved_cl_tg_cap(:,:,:,:)
          
    type Tnscommand_line
        character (len=command_line_size) :: command_line
        character (len=5)  :: scename
    end type Tnscommand_line

    type(Tnscommand_line) :: ClData
    contains
    subroutine read_command_line(cl_in)
    character (len=*) :: cl_in
    character (len=1024) :: clget
    character (len=256) :: cltemp
    integer :: getCAResult
    integer :: charlen, ic, argindex
    character (len=1024) :: logtext
    logical :: fail_on_empty
    character (len=1024) :: section

          cl_in=realtrim(cl_in)
          
          ! cl_in comes from RealWin 
          call write_log_entry("cl_data:0018", "In " // &
          "read_command_line routine. cl_in=" //trim(cl_in)//".")
          
          if(trimlen(cl_in)==0) then
            cltemp=""
          else
            cltemp=trim(cl_in)
          endif

      
      fail_on_empty=.false.
    
    
      argindex=1
      charlen=len_trim(cltemp)
      if(charlen>1) then
        fail_on_empty=.true.
        section="cl_data:0007"
        call write_log_entry(section, &
            "cltemp already set: " // trim(cltemp))
      endif



          if(charlen<2) then
            log_message="Routine entered, " // &
            "and command-line not set. Loading from GETCL."
            call write_log_entry("cl_data:0008", log_message)
            ! Call doesn't return anything.
            call GETCL(cltemp)
            charlen=len_trim(cltemp)

            if(charlen==0) then
                logtext="GetCL returned empty string."
            else
                fail_on_empty=.true.
                logtext="GetCL returned " // trim(cltemp)
            endif
            log_section="cl_data:0001"
            call write_log_entry(log_section, logtext)
        else
            log_section="cl_data:0002"
            call write_log_entry(log_section, "Didn't call GetCL " // &
               "because it wasn't needed - cl_in was already set " // &
               "upon routine entry.")
            call write_log_entry("cl_data:0002a", "CL is " // &
                trim(cltemp))
        endif
        
          charlen=len_trim(cltemp)
          
          if(charlen<2) then
            cltemp=" "
            CALL getenv("DEBUGDIR",cltemp)
            charlen=len_trim(cltemp)
            if(charlen>0) fail_on_empty=.true.
          endif
          clget=achar(0)
          
          if(charlen<2) then
             call GETCL(clget)
             cltemp=clget
             charlen=len_trim(cltemp)
             if(charlen>0) fail_on_empty=.true.
             
             call write_log_entry("midasxp:3", "After GETCL, " // &
           "command line is " // trim(cltemp) // ".")
          endif
          
          ic=iachar(cltemp(1:1))
          if (ic==0) then
              cltemp=cltemp(2:len_trim(cltemp))
          endif
          charlen=len_trim(cltemp)
          
          if(fail_on_empty .and. charlen<2) then
        call end_program("cl_data:99999 - command line went from " // &
                "set to not set in cl_data.")
          endif
          
          ClData%command_line=cltemp


    end subroutine read_command_line

    subroutine read_cols_from_tempcl(cl, col1, col2, col3)
    character (len=5), intent(out) :: col1, col2, col3
    character(len=5) :: cols(3)
    character (len=256) :: longtempstr, tempstr
    character (len=256), intent(in) :: cl
    character (len=256) :: tempcl
    integer :: chindex, varindex, array_index
    character :: ch
    integer :: lt, ic

        tempcl=cl
        cols=""
        ch=tempcl(1:1)
        ic=iachar(ch)
        
        if(ic==0) then
           call end_program("cl_data:00017 - found null(0) in cl " // &
            "argument (read_cols_from_tempcl).")
        endif
        if(ch=='"'.or.ic==0) then
            tempcl=tempcl(2:len_trim(tempcl))
        endif
        array_index=1

        lt=trimlen(tempcl)
        
        do varindex = 1, 3
            do chindex=1, lt
                ch=tempcl(array_index:array_index)
                if(ch==" ") then
                    array_index=array_index+1
                    exit
                else
                    cols(varindex)=trim(cols(varindex)) // ch
                endif
                array_index=array_index+1
            enddo
        enddo
        
        col1=cols(1)
        col2=cols(2)
        col3=cols(3)


    end subroutine read_cols_from_tempcl

end module cl_data
