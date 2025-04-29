module miscmod
use end_routine
use string
use filemod
implicit none
character (len=2) :: newline=char(10)//char(13)
contains
function remove_nulls(str)
character (len=1024) :: result_string, remove_nulls
character (len=*) :: str
character :: ch 
integer :: index

    result_string=" "
    do index=1, trimlen(str)
        ch=str(index:index)
        if(iachar(ch)>0) then
            result_string=trim(result_string) // ch
        endif
    enddo
    
    remove_nulls=result_string
    
end function remove_nulls
function trimlen(strtocount)
! Return length of string if it were to be trimmed.  LAHEY
! intrinsics sometimes return the actual variable length or
! (variable_length+1) when a null character is embedded in the
! character array.
character(len=*), intent(in) :: strtocount
character(1024) :: sc
integer :: result, trimlen, index, ch
integer :: lt
    
    result=0
    lt=min(len(strtocount), len_trim(strtocount))
    sc=strtocount

    do index=1, lt
        ch=iachar(sc(index:index))
        if(ch==0 .or. ch<8) then
            sc(index:index)=" "
        endif

    enddo
    
    result=len_trim(sc)

    trimlen=result

end function trimlen
function remove_null_characters(string_to_fix)
character(*), intent(in) :: string_to_fix
character*1024 :: return_value, remove_null_characters
integer, parameter :: command_line_size=256
integer :: index, lastidx
character :: ch
    return_value=""
    lastidx=min(len_trim(string_to_fix), command_line_size)


    do index=1,command_line_size
        ch=string_to_fix(index:index)
        if(iachar(ch)/=0) then
            return_value=trim(return_value) // ch
        else
            exit
        endif
    enddo
    
    remove_null_characters=trim(return_value)
end function remove_null_characters
function are_strings_equal(string1, string2)
    logical :: are_strings_equal
    character(*), intent(in) :: string1, string2
    integer :: stcr
    are_strings_equal=.true. ! Good unless they're different
    stcr=strcmp(string1, string2)
    if(stcr/=0) then
        are_strings_equal=.true.
    endif

end function are_strings_equal
function isgoodchar(ich)
integer :: ich
logical :: isgoodchar
    isgoodchar=.false.
    
    if(ich>31) then
        isgoodchar=.true.
    endif
end function isgoodchar




function strcmp(string1, string2)
character(*), intent(in) :: string1, string2
integer :: index, maxlen, ch1, ch2, result, strcmp
    result=0
    maxlen=len(string1)
    if(maxlen<len(string2)) then
        maxlen=len(string2)
    endif
    
    do index=1, maxlen
        if(string1(index:index)/=string2(index:index)) then
            ch1=iachar(string1(index:index))
            ch2=iachar(string2(index:index))
            if(ch2/=ch1) then
                if(ch2<ch1)then
                    result=-1
                elseif (ch2>ch1) then
                    result=1
                endif
                exit
            endif
            
        endif
        
        strcmp=result
        
    enddo
    
    

    

    
end function strcmp


    subroutine array_error(caller, array_name, upper_bound, requested_index)
    character*256, intent(in) :: array_name, caller
    integer :: upper_bound, requested_index
    
        er_message=trim(caller) // " - An attempt was made to access element " // &
        trim(itos(requested_index)) // " from the " // trim(array_name) // " array , " // &
        "which only has " //trim(itos(upper_bound)) // " elements."
        
        call end_program(er_message)
    
    end subroutine array_error
    
    
    function get_filename_from_unit(unit_no)
        integer :: unit_no
        logical :: file_opened
        character*512 :: filename, get_filename_from_unit
        integer :: ios

        filename=""
        get_filename_from_unit=" "
        inquire(unit=unit_no, NAME=filename, iostat=ios)
        if(ios/=0) then
            call end_program("miscmod:0020 - Unit " // &
            trim(itos(int(unit_no))) // " is not open.")
        end if
        
        get_filename_from_unit=filename
        if(len_trim(filename)==0) then
            call end_program("miscmod:0019 - unable to retrieve filename.")
        end if
        
    end function get_filename_from_unit
    
    subroutine check_capacity_array_bounds(name_of_array, array, &
        idx1st, idx2nd) 
        
    real, allocatable, intent(in) ::  array(:,:)
    integer, intent(in) :: idx1st, idx2nd
    integer :: ub1st, ub2nd
    character (len=*), intent(in) :: name_of_array
    character(len=255) :: error_reason
    character (len=256) :: array_name    
    logical :: bounds_exceeded
    array_name=name_of_array
    bounds_exceeded=.false.
    
    ub1st=ubound(array, 1)
    ub2nd=ubound(array, 2)
    
    if(idx1st>ub1st) then
        bounds_exceeded=.true.
        error_reason="first element exceeds bounds"
    else if (idx2nd>ub2nd) then
        bounds_exceeded=.true.
        error_reason="second element exceeds bounds"
    endif
    
    if (bounds_exceeded) then
        er_message="miscmod:0001 -  Bounds exceeded for the " // &
        trim(array_name) // " array: " // trim(error_reason) // "."
        call end_program(er_message)
    endif
    
    
    end subroutine check_capacity_array_bounds

function just_the_filename(filename)
character (len=*) :: filename
character (len=512) :: return_filename
character (len=512) :: just_the_filename
integer :: index, lastidx, foundidx

    return_filename=" "
    lastidx=len_trim(filename)
    foundidx=-1
    
    do index=lastidx,1, -1
        if (filename(index:index)=="\") then
            foundidx=index
            exit
        endif
    enddo
    
    if(foundidx<1) then
        er_message="miscmod:0002 - Unable to find path terminator " // &
        " in " // trim(filename)
        call end_program(er_message)
    else
        if(foundidx>1) then
            return_filename=filename(foundidx+1:len_trim(filename))
        else
            call end_program("miscmod:0003 - Could not parse " // &
            "return filename.")
        endif
    endif

    just_the_filename=return_filename
end function just_the_filename
!
      subroutine copy_file_2_file(FROM_FILE,TO_FILE)
	  use logging
      use string
	  implicit none

      CHARACTER (LEN=*)    :: FROM_FILE
      character(len=1024) :: uc_from_file
      CHARACTER (LEN=*)    :: TO_FILE
      CHARACTER (LEN=4096) :: BUFFER
      INTEGER (KIND=4) :: FILE_SIZE
      INTEGER (KIND=4) :: FILSIZ
      INTEGER (KIND=4) :: BYTES_WRITTEN
      INTEGER (KIND=4) :: BUFFER_SIZE
      INTEGER (KIND=4) :: BYTES_LEFT
      INTEGER (KIND=4) :: UNIT_USED

      uc_from_file=trim(ucase(FROM_FILE))

      if (index(uc_from_file, "OLFOSIL")>0) then
        uc_from_file=uc_from_file ! Debugstop
      endif
      
      if(index(from_file, "OLFOSIL")>0) then
        from_file=from_file ! Debugstop
      end if
      


      BUFFER_SIZE = 4096
      FILE_SIZE = FILSIZ(FROM_FILE)
      if(FILE_SIZE .GT. 0) THEN
         INQUIRE(FILE=FROM_FILE,NUMBER=UNIT_USED)
         if(UNIT_USED > 0) CLOSE(UNIT_USED)
         INQUIRE(FILE=TO_FILE,NUMBER=UNIT_USED)
         if(UNIT_USED > 0) CLOSE(UNIT_USED)
         OPEN(99,FILE=FROM_FILE,ACCESS='TRANSPARENT')
         OPEN(98,FILE=TO_FILE,ACCESS='TRANSPARENT')
         BYTES_WRITTEN = 0
         DO WHILE (BYTES_WRITTEN+BUFFER_SIZE < FILE_SIZE)
            READ(99) BUFFER
            WRITE(98) BUFFER
            BYTES_WRITTEN = BYTES_WRITTEN + BUFFER_SIZE
         enddo
         BYTES_LEFT = FILE_SIZE - BYTES_WRITTEN
         READ(99) BUFFER(1:BYTES_LEFT)
         WRITE(98) BUFFER(1:BYTES_LEFT)
         CLOSE(99)
         CLOSE(98)
      endif
      return
      end subroutine copy_file_2_file
      subroutine check_alloc(location_key, array_name, alloc_status, & 
        extra_data)
        character (len=*), intent(in) :: location_key, array_name
        character (len=*), intent(in), optional :: extra_data
        integer :: alloc_status

        if(alloc_status/=0) then
            er_message=trim(location_key) // " - " // &
            "allocation action failed for array "//trim(array_name)
            if(alloc_status==1001) then
                er_message=trim(er_message) // &
                ". STAT=1001 (array was already allocated)."
            else
                er_message=trim(er_message) // &
                    ". STAT="// trim(itos(alloc_status))
            endif
            if (present(extra_data)) then
                er_message=trim(er_message) // &
                " Extra_data=" // trim(extra_data) // "."
            end if
            
            call end_program(er_message)
        endif
        alloc_status=0

      end subroutine check_alloc

    
    
    function get_free_mem(key)
    real :: result_value, get_free_mem
    real :: fract
    character (len=*) :: key
    character (len=1024) :: line1,line2
    character (len=80) :: chs
    integer :: unitno=669, ios
    character (len=80) :: outstr
    real, save :: last_freemem_2=0, last_freemem_1=0
    logical :: freemems_same

    
    ! If a difference > this percentage, fail
    real :: max_percent_alloc=5 

    
        result_value=0
        outstr="wmic OS get FreePhysicalMemory > fpm.txt"
        call system(outstr, dosbox=.false.) ! Nonportable
        
       
        open(unit=unitno, file="fpm.txt", iostat=ios, &
             status="old", access="sequential", &
             action="read", recl=136)
            
        
        if(ios==0) then
            line1=" "
            line2=" "
            ! Toss the first line
            read(unit=unitno, "(A)") line1
            line1=trim(remove_nulls(line1))
            
            ! Second line contains answer - read it.
            read(unit=unitno, "(A)") line2
            line2=trim(remove_nulls(line2))

            ! Convert from string to real
            read(unit=line2, *) result_value
            last_freemem_1=last_freemem_2
            last_freemem_2=result_value
            freemems_same=(last_freemem_1==last_freemem_2)
            if(.not. freemems_same) then
                freemems_same=freemems_same ! Debugstop
            end if
            ! Watch for large memory requests.
            if(last_freemem_1/=0. .and. last_freemem_2/=0. .and. &
            .not. freemems_same) then
                ! It's only a valid check if the last allocation
                ! was an increase
                if(last_freemem_2<last_freemem_1) then
                    fract=last_freemem_1/last_freemem_2*100
                    
                    if(fract>max_percent_alloc) then 
                        call end_program("miscmod:0017 - Excessive " // &
                      "memory recently allocated, associated with the " // &
                        trim(key) // " key.")
                        
                    endif
                end if
                
            end if

          close(unitno)
         else
            call end_program("miscmod:0018 - Unable to read bounds " &
            // "file fpm.txt. IO error: " // &
            trim(itos(ios)) //".")
            
         endif
         
      get_free_mem=result_value
    end function get_free_mem
    
    subroutine check_unit(unitid, key)
        integer (kind=2) :: unitid
        character (len=*) :: key
        if (unitid==0) then
            er_message=trim(key) // " UNITID passed to check_unit " // &
            "is invalid ("  // trim(itos(int(unitid))) // ")."
            call end_program(er_message)
        end if
        if(.not. is_unit_in_use(int(unitid))) then
            er_message=trim(key) // " - unit " // &
            trim(itos(int(unitid))) // " isn't open."
            call end_program(er_message)
        end if

        
    end subroutine check_unit
end module miscmod
