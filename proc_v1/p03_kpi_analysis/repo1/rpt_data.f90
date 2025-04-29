module rpt_data
! Module solves circular reference issue between RptRecControl and irec_endpoint_control.
implicit none
      integer (kind=4) :: irec_saved
      logical (kind=4) :: horizons_active=.true. 
      integer (kind=1) :: cr_hex = z"0D"
      integer (kind=1) :: lf_hex = z"0A"
      character (len=1) :: crlf_temp(2)
      character (len=2) :: crlf
      equivalence (crlf_temp(1),cr_hex),(crlf_temp(2),lf_hex)
      equivalence (crlf,crlf_temp)
      integer (kind=1) :: endfile_mark_hex=z"1a"
      character (len=1) :: endfile_mark
      equivalence (endfile_mark,endfile_mark_hex)
end module rpt_data