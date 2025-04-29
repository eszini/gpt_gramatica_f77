module filemod
use end_routine
use string
implicit none

contains
subroutine alt_delete_file(filename)
    character (len=*) :: filename
    call system("del " // trim(filename))
    
end subroutine alt_delete_file
function is_unit_open(unitno)
    implicit none
    logical :: is_unit_open
    integer :: unitno
    logical :: reslt
    
    INQUIRE(UNIT=unitno,OPENED=reslt)
    is_unit_open=reslt
    
    
end function is_unit_open
subroutine delete_file(filename)
    character (len=*) :: filename
    character (len=1024) :: foldername
    integer :: ierr
    integer :: file_unit
	logical :: exists 
    call GETCWD(foldername)
    ! filename=trim(foldername) // "\" // trim(filename)
    INQUIRE(FILE=FILENAME,EXIST=exists)
	
    if(exists) then
        file_unit=1965
        open (unit=file_unit,FILE=filename,iostat=ierr)
        if (ierr == 0) then
            close (file_unit,STATUS="DELETE")
        else
            call end_program("filemod:0002 - Cannot delete file.")
        endif
		INQUIRE(FILE=FILENAME,EXIST=exists)
        if (exists) then
            call alt_delete_file(filename)
        endif
    endif
    
end subroutine delete_file
integer function get_new_unit()
      integer :: unit
      integer, parameter :: LUN_MIN=2500, LUN_MAX=10000
      integer :: lun
      get_new_unit=-1
      do lun=LUN_MIN,LUN_MAX
          if(.not. is_unit_in_use(lun)) then
              get_new_unit=lun
              exit
            end if
      end do

end function get_new_unit
    
logical function is_unit_in_use(unit_to_check)
        logical :: op
        integer :: ios
        integer :: unit_to_check
        inquire(UNIT=unit_to_check,opened=op, iostat=ios)
        if(ios/=0) then
            stop "IOS"
        end if
        
        if (.not. op) then
            is_unit_in_use=.false.
        else
            is_unit_in_use=.true.
        endif
        
end function is_unit_in_use
subroutine check_ios(ios, key, filename)
    integer :: ios
    character (len=*) :: key, filename
    if(ios/=0) then
        er_message=trim(key) // " - File action  for " // &
        trim(filename) // " has failed. EOS=" // &
            trim(itos(ios))
        call end_program(er_message)
    end if
end subroutine check_ios
subroutine close_unit(unit_num)
    INTEGER (KIND=2), PARAMETER ::  TRANS_LAS_UNIT = 1936
    integer (kind=2), intent(in) :: unit_num
    integer :: ios
    

    close(unit_num, iostat=ios)
    if(ios/=0) then
        er_message="filemod:0001 - close of unit " // &
        trim(itos(int(unit_num))) // " failed. IOS=" // itos(int(ios))
        call end_program(er_message)
    end if
    
end subroutine close_unit
    function get_file_size(filename)
        integer (kind=4) :: get_file_size, fs
        character (len=*) :: filename
        
        ! TODO:  To support standard fortran, FLEN must be replaced
        ! by SIZE. But Lahey doesn't support it.
        
        inquire(file=filename,FLEN=fs)
        get_file_size=fs
    end function get_file_size



    function does_file_exist(filename)
        logical :: does_file_exist, fe
        character(len=*) :: filename
        character(256) :: line, cmd
        character(1024) :: file_text
        integer(kind=4) :: ios, fu

        file_text=""
        
        inquire(FILE=filename, EXIST=fe)
      
        does_file_exist=fe
      
    end function does_file_exist

subroutine lalt_delete_file(filename)
    character*512 :: filename
    call system("del " // trim(filename))
    
end subroutine lalt_delete_file

subroutine ldelete_file(filename)
    character*512 :: filename
    character*512 :: foldername
    integer :: ierr
    integer :: file_unit
    call GETCWD(foldername)
    ! filename=trim(foldername) // "\" // trim(filename)
    
    if(does_file_exist(filename)) then
        file_unit=1965
        open (unit=file_unit,FILE=filename,iostat=ierr)
        if (ierr == 0) then
            close (file_unit,STATUS="DELETE")
        else
            call end_program("file_existence:0001 - Cannot delete file.")
        endif
        if (does_file_exist(filename)) then
            call lalt_delete_file(filename)
        endif
    endif
    
end subroutine ldelete_file
function open_new_text_file(filename)
    character(len=*) :: filename
    integer :: open_new_text_file, file_unit
    
    
        !inquire(unit=rdi_out_unit, NAME=txt_file_name)
        file_unit=get_new_unit()
        if(does_file_exist(filename)) then
            call delete_file(filename)
        end if
        
        if(.not. is_unit_in_use(file_unit)) then
            open (unit=file_unit, &
              file=filename,action="write", status="NEW")
        else
            call end_program("filemod:0003 - unit in use.")
        end if
        open_new_text_file=file_unit


end function open_new_text_file
end module filemod