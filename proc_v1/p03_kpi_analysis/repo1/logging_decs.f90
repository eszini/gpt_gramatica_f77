module logging_decs
implicit none
integer :: log_file_unit=-1
logical :: log_file_open=.false.
character (len=512) :: log_filename
end module logging_decs