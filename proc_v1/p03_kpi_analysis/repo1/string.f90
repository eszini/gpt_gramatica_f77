module string
use end_routine
implicit none
contains
    function ucase(s)
      integer*2 n,i
      character (len=*), intent(in) :: s
      character (len=2048) :: result_string, ucase
      character*1 c
      
      result_string=s
      n=len(s)
      
      do i=1,n
        c=result_string(i:i) ! convert ith char to upper-case
        if(('a'<=c).and.(c<='z')) result_string(i:i)=char(ichar(c)-32)
      end do
      ucase=result_string
    end function ucase
      
    function bl_2str(the_bool)
    character(len=5):: bl_2str
    logical :: the_bool
    
        if(the_bool) then
            bl_2str="True"
        else
            bl_2str="False"
        endif
        
    end function bl_2str
    
    function r_ats(array_to_convert)
    real, dimension(:) :: array_to_convert
    character(len=16000) :: r_ats
    
        r_ats=real_array_to_string(array_to_convert)
        
    end function r_ats

    function rtos(the_real)
        real(kind=4) :: the_real
        character(len=256) ::  rtos
        rtos=" "
        
        write(rtos, *) the_real
    end function  rtos
    function real_array_to_string(array_to_convert)
    character(len=512) :: real_array_to_string
    real, dimension(:) :: array_to_convert
    integer :: i, lb, ub
    
        real_array_to_string=""
        lb=lbound(array_to_convert, 1)
        ub=ubound(array_to_convert, 1)
        do i=lb, ub
            real_array_to_string=trim(real_array_to_string) // " " //  &
            trim(rtos(array_to_convert(i))) // ", " 
        enddo
    
    
    end function real_array_to_string
    
  
    ! ftos converts REALs (float) to string
    function ftos(the_float)
    character(len=20) :: the_string, ftos
    real :: the_float
    
        the_string=""
        write(the_string, *) the_float
        ftos=the_string
        
    end function ftos
    ! itos calls int_to_string. Simpler for fixed line length code.
    function itos(the_int)
    integer, intent(in) :: the_int
    character(len=20) :: the_string, itos
    
        itos=int_to_string(the_int)

    end function itos
    function int_to_string(the_int)
    integer, intent(in) :: the_int
    character(len=20) :: the_string, int_to_string
    
        the_string=""
        write(the_string, *) the_int
        int_to_string=trim(the_string)
        
        
    end function int_to_string

    function is_ascii_value(ascii_value)
    implicit none
    logical :: is_ascii_value
    integer :: ascii_value

        if(ascii_value >31) then
            is_ascii_value=.true.
        else
            is_ascii_value=.false.
        endif
        
        
    end function is_ascii_value    

    
    function realtrim(the_string)
    character(len=*) :: the_string
    character (len=1024) :: result_value, realtrim
    integer :: index
       result_value=" "
    
        do index=1, len_trim(the_string)
            if(is_ascii_value(iachar(the_string(index:index)))) then
                result_value=trim(result_value)//the_string(index:index)
            else
                result_value(index:index)=" "
            end if
        enddo
        result_value=trim(result_value)
        realtrim=result_value
        
    end function realtrim

end module string