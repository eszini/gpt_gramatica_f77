module hesi
use hesi_gendecs
use debugtrace
implicit none
integer (kind=8), save :: hesi_unit_id_nums(max_cl_units)
contains

      function get_hesi_unit_id_num(r_nunits, r_hesi_second_unit_id_num)
         integer :: file_trace_huidn=0
         real (kind=4) :: get_hesi_unit_id_num
         real (kind=4) :: r_hesi_second_unit_id_num
         integer (kind=2) :: r_nunits
         integer (kind=8) :: temp_i8
         if(r_nunits==407) then
            r_nunits=r_nunits ! Debugstop
         end if
         if(file_trace_huidn==0) then
            file_trace_huidn=open_trace("hesi_uid_num.trace", rq_huidn)
         end if
         call write_trace_message(file_trace_huidn, "Arguments:")
         call write_trace_int2(file_trace_huidn, "r_nunits", r_nunits)
         call write_trace_real4(file_trace_huidn, "second ID", &
            r_hesi_second_unit_id_num)
         call write_trace_int8(file_trace_huidn, &
            "hesi_unit_id_num(r_nunits)", hesi_unit_id_nums(r_nunits))
            
         if(hesi_unit_id_nums(r_nunits) <= 999999) then
            ! TODO: This could benefit greatly by using
            ! collections.               
            get_hesi_unit_id_num = real(hesi_unit_id_nums(r_nunits))
            r_hesi_second_unit_id_num = 0.
         else

            temp_i8 = hesi_unit_id_nums(r_nunits)/1000000
            call write_trace_int8(file_trace_huidn, "temp_i8 became", &
                temp_i8)
                
            r_hesi_second_unit_id_num = real(temp_i8)
            call write_trace_real4(file_trace_huidn, &
                "r_hesi_second_unit_id_num", r_hesi_second_unit_id_num)
                
            get_hesi_unit_id_num = &
               real(hesi_unit_id_nums(r_nunits)-1000000*temp_i8)
         endif
         call write_trace_real4(file_trace_huidn, "returning", &
            get_hesi_unit_id_num)
            
      end function get_hesi_unit_id_num

end module hesi