module data_integrity
use end_routine
use miscmod
use logging
implicit none
contains
subroutine check_string_corrupt(key, string_to_check, irec, &
    error_text)
    character (len=*) :: error_text
    character (len=*) :: string_to_check
    character (len=4096) :: original_value
    character (len=*) :: key
    integer (kind=2) :: irec
    integer :: stringsize
    integer ::  num_times_here=0
    
            num_times_here=num_times_here+1
                
            stringsize=len(string_to_check)
            original_value=string_to_check(1:stringsize)
            

           
           if(num_times_here==4930) then
            num_times_here=num_times_here ! Debugstop
           end if

           call remove_leading_nulls(string_to_check, stringsize)
           
           if(trim(original_value)/=trim(string_to_check) .and. &
            trim(original_value)/="") then
           
            er_message=trim(key) // " - " // trim(error_text) // " " &
                // trim(original_value) // " became " // &
               trim(string_to_check) // ". IREC=" // &
               trim(itos(int(irec))) // ". (This routine was " // &
               "called " // trim(itos(num_times_here)) // " times."
               
             call end_program(er_message)
           end if
end subroutine check_string_corrupt
subroutine remove_leading_nulls(the_string, numchars)
integer :: numchars
character (len=numchars) :: the_string, newstring
integer :: ord_values(numchars)
integer :: index, lastidx, charval
integer :: destindex
logical :: all_zeroes

      all_zeroes=.true.
      

      newstring=" "
      
      ord_values=0
      
      do index=1,numchars
        lastidx=index
        charval=iachar(the_string(index:index))
        ord_values(index)=charval

        if (charval>0) then
            all_zeroes=.false.
            ! Done iterating over nulls. Keeping the rest of the string.
            exit
        end if
      end do
      destindex=1 ! First element in replacement string

      if (numchars>0) then
     
          do index=lastidx, numchars
            charval=iachar(the_string(index:index))
            if(charval/=0) then
                newstring(destindex:destindex)=the_string(index:index)
                destindex=destindex+1
            else
                exit ! A null character is assumed to be c-style terminated.
            endif
            
          end do
      end if
      
      if(len_trim(newstring)==0 .and. all_zeroes) then
        the_string=the_string ! Keep the string. It's filled with zeros and assuming that's intended.
      else 
        the_string=newstring
      endif
      
      
end subroutine remove_leading_nulls
    

end module data_integrity